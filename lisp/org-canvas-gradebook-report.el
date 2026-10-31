;;; org-canvas-gradebook-report.el --- One student's standing and the what-if  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The two read-only reports `org-canvas-gradebook' grew past the
;; course-wide tables: one student's standing (`org-canvas-gradebook-student',
;; issue #542) and the what-if over assignment-groups.org
;; (`org-canvas-gradebook-what-if', #452).  Both print to a buffer and send
;; nothing; the student report also prints to stdout under
;; `noninteractive', so `scripts/org-canvas student NAME' answers "can I
;; still pass?" without a REST script.
;;
;; Neither writes anything: the pull, the grades and the scores file they
;; read stay in `org-canvas-gradebook.el', and the arithmetic they share is
;; in `org-canvas-gradebook-scores.el'.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-gradebook)
(require 'org-canvas-gradebook-scores)
(require 'cl-lib)

;;;; One Student's Standing
;;
;; The gradebook answers the course, a grading file one column;
;; `org-canvas-gradebook-student' (issue #542) answers one student.  It
;; prints every column with its score, points, state, late and missing
;; flags, due and lock dates and whether it is still open, grouped by
;; assignment group with its weight and drop rules; the columns past due
;; but not locked, the only work the student can still turn in; and the
;; best possible total if every still-open column scored full marks,
;; drop rules kept — the what-if (#452) applied to one student, read
;; live rather than from the scores file.  Nothing is sent and nothing
;; written: the report goes to a buffer, or to stdout under
;; `noninteractive', so `scripts/org-canvas student NAME' answers "can I
;; still pass?" without a REST script.
;;
;; The ceiling keeps every graded score, scores every still-open
;; ungraded column at its full points and every closed ungraded one at
;; zero, then runs the drop rules over those items: work the student can
;; no longer submit counts zero, as Canvas's final grade counts it.  A
;; column is open while its lock is in the future or absent; the dates
;; are the column's own, not the student's override.

(defun org-canvas--gradebook-student-params (user-id)
  "Return the query parameters of USER-ID's submissions read.
Each submission carries its assignment, for the name, dates, points
and group (issue #542)."
  `(("student_ids[]" . ,(format "%s" user-id))
    ("include[]" . "assignment")))

(defun org-canvas--gradebook-student-submissions (user-id)
  "Return USER-ID's submissions, each with its assignment, by due date.
Undated assignments come last, then by name."
  (sort (append (org-canvas-api-request-all-pages
                 'GET (org-canvas-api-course-endpoint "students/submissions")
                 (org-canvas--gradebook-student-params user-id))
                nil)
        #'org-canvas--gradebook-self-before-p))

(defun org-canvas--gradebook-student-id-p (name)
  "Return the user id NAME spells, or nil when it spells none.
Both 5001 and #5001 spell an id."
  (when (string-match "\\`#?\\([0-9]+\\)\\'" name)
    (string-to-number (match-string 1 name))))

(defun org-canvas--gradebook-student-matches (enrollment id name)
  "Return non-nil when ENROLLMENT is the student ID or NAME names.
ID is a number, or nil when NAME is a name; a name matches the user's
sortable name, as the Students table of gradebook.org shows it, or the
plain one."
  (let ((user (alist-get 'user enrollment)))
    (or (and id (eql (alist-get 'user_id enrollment) id))
        (and (null id)
             (or (equal (alist-get 'sortable_name user) name)
                 (equal (alist-get 'name user) name))))))

(defun org-canvas--gradebook-student-pick (enrollments name)
  "Return the enrollments of the student NAME names, or signal an error.
ENROLLMENTS are the course's student enrollments; NAME is a Canvas
user id (5001 or #5001), a sortable name (\"Last, First\", what
gradebook.org's Students table shows) or a plain one (\"First Last\").
A name two students share is an error naming both: the user id tells
them apart."
  (let* ((id (org-canvas--gradebook-student-id-p name))
         (matching (cl-remove-if-not
                    (lambda (e) (org-canvas--gradebook-student-matches e id name))
                    enrollments))
         (uids (delete-dups (mapcar (lambda (e) (alist-get 'user_id e)) matching))))
    (cond ((null uids)
           (user-error "No student %s in this course; gradebook.org's Students table shows the names, or name the user id"
                       name))
          ((and (null id) (cdr uids))
           (user-error "Two students in this course are named %s; name the user id: %s"
                       name (mapconcat #'number-to-string uids ", ")))
          (t matching))))

(defun org-canvas--gradebook-student-prompt ()
  "Read a student for `org-canvas-gradebook-student', with completion.
The candidates are the roster's sortable names, as gradebook.org shows
them; a user id answers too.  The roster is read once for the
completion and again by the command itself."
  (let* ((enrollments (org-canvas--gradebook-fetch-enrollments))
         (names (delete-dups
                 (delq nil
                       (mapcar (lambda (e)
                                 (let ((user (alist-get 'user e)))
                                   (or (alist-get 'sortable_name user)
                                       (alist-get 'name user))))
                               enrollments)))))
    (completing-read "Student (name or user id): " names nil t)))

(defun org-canvas--gradebook-student-row (submission assignments now)
  "Return SUBMISSION as one row of the student report.
ASSIGNMENTS is the map from `org-canvas--gradebook-assignment-map';
NOW a time value the lock dates are read at.  The row plist holds
:id :name :group-id :points :score :state :late :missing :excused :due
:lock (time values, or nil without a date) :counted :open and :item,
the counted graded item or nil."
  (let* ((assignment (alist-get 'assignment submission))
         (id (alist-get 'assignment_id submission))
         (lock (org-canvas--parse-iso8601-time (alist-get 'lock_at assignment))))
    (list :id id
          :name (or (alist-get 'name assignment) (format "Assignment %s" id))
          :group-id (alist-get 'assignment_group_id assignment)
          :points (or (org-canvas--gradebook-stat assignment 'points_possible) 0)
          :score (org-canvas--gradebook-stat submission 'score)
          :state (or (alist-get 'workflow_state submission) "-")
          :late (alist-get 'late submission)
          :missing (alist-get 'missing submission)
          :excused (eq (alist-get 'excused submission) t)
          :due (org-canvas--parse-iso8601-time (alist-get 'due_at assignment))
          :lock lock
          :counted (and (alist-get id assignments) t)
          :open (and (or (null lock) (time-less-p now lock)) t)
          :item (org-canvas--gradebook-submission-item
                 submission assignments org-canvas-gradebook-unposted))))

(defun org-canvas--gradebook-student-rows (submissions assignments now)
  "Return one row for each of SUBMISSIONS, in the order given.
Each row is what `org-canvas--gradebook-student-row' builds from the
submission, ASSIGNMENTS and NOW."
  (mapcar (lambda (s) (org-canvas--gradebook-student-row s assignments now))
          submissions))

(defun org-canvas--gradebook-student-graded-items (rows)
  "Return ROWS' counted graded items: the student's scores as they stand."
  (delq nil (mapcar (lambda (row) (plist-get row :item)) rows)))

(defun org-canvas--gradebook-student-ceiling-item (row)
  "Return ROW's item for the ceiling, the best it can still become.
A graded score stands; an excused or uncounted column counts nothing; a
still-open ungraded column scores its full points; a closed one zero."
  (cond ((plist-get row :item))
        ((or (plist-get row :excused) (not (plist-get row :counted))) nil)
        ((plist-get row :open)
         (list (plist-get row :id) (plist-get row :points) (plist-get row :points)))
        (t (list (plist-get row :id) 0 (plist-get row :points)))))

(defun org-canvas--gradebook-student-ceiling-items (rows)
  "Return the ceiling item of each of ROWS, in row order.
Those are the items Canvas's final grade would count were every
still-open column scored full marks."
  (delq nil (mapcar #'org-canvas--gradebook-student-ceiling-item rows)))

(defun org-canvas--gradebook-student-open-rows (rows now)
  "Return ROWS past due at NOW, still open, counted and ungraded.
Those are the columns the student can still turn in."
  (cl-remove-if-not
   (lambda (row)
     (and (plist-get row :open)
          (plist-get row :counted)
          (null (plist-get row :item))
          (plist-get row :due)
          (not (time-less-p now (plist-get row :due)))))
   rows))

(defun org-canvas--gradebook-student-time (time)
  "Return TIME as an Org timestamp in the course zone, or a dash."
  (if time (format-time-string "<%Y-%m-%d %a %H:%M>" time (org-canvas--time-zone)) "-"))

(defun org-canvas--gradebook-student-percent-text (percent)
  "Return PERCENT as text with its percent sign, or a dash."
  (if (numberp percent)
      (format "%s%%" (org-canvas--gradebook-number percent))
    "-"))

(defun org-canvas--gradebook-student-drops-text (group)
  "Return GROUP's drop rules and never-drop ids as text, empty without any."
  (let ((parts nil))
    (when (and (plist-get group :drop-lowest) (> (plist-get group :drop-lowest) 0))
      (push (format "drop lowest %d" (plist-get group :drop-lowest)) parts))
    (when (and (plist-get group :drop-highest) (> (plist-get group :drop-highest) 0))
      (push (format "drop highest %d" (plist-get group :drop-highest)) parts))
    (when (plist-get group :never-drop)
      (push (format "never drops %s"
                    (mapconcat #'number-to-string (plist-get group :never-drop) ", "))
            parts))
    (mapconcat #'identity (nreverse parts) ", ")))

(defun org-canvas--gradebook-student-group-text (group weighted)
  "Return GROUP's weight and drop rules as the report's header text.
WEIGHTED non-nil shows the group's weight, nil the word unweighted."
  (let ((weight (if weighted
                    (format "weight %s%%"
                            (org-canvas--gradebook-number (plist-get group :weight)))
                  "unweighted"))
        (drops (org-canvas--gradebook-student-drops-text group)))
    (if (string-empty-p drops) weight (concat weight ", " drops))))

(defun org-canvas--gradebook-student-name-cell (row)
  "Return ROW's Column cell: a link to its heading carrying the id.
A column that does not count toward the grade is named as such."
  (org-canvas--gradebook-assignment-cell
   `((title . ,(if (plist-get row :counted)
                    (plist-get row :name)
                  (format "%s (not counted)" (plist-get row :name))))
     (assignment_id . ,(plist-get row :id)))))

(defun org-canvas--gradebook-insert-student-row (row)
  "Insert the table line for ROW at point."
  (insert (format "| %s | %s | %s | %s | %s | %s | %s | %s | %s |\n"
                  (org-canvas--gradebook-student-name-cell row)
                  (org-canvas--gradebook-student-time (plist-get row :due))
                  (org-canvas--gradebook-student-time (plist-get row :lock))
                  (org-canvas--gradebook-number (plist-get row :score))
                  (org-canvas--gradebook-number (plist-get row :points))
                  (if (plist-get row :excused) "excused" (plist-get row :state))
                  (org-canvas--gradebook-flag (plist-get row :late))
                  (org-canvas--gradebook-flag (plist-get row :missing))
                  (org-canvas--gradebook-flag (plist-get row :open)))))

(defun org-canvas--gradebook-student-hurdle-text (required now best)
  "Return the hurdle line for a group that requires REQUIRED percent.
NOW and BEST are its percentages after the drop rules, nil without
points possible."
  (format "Hurdle %s%%: %s now, %s at best — %s."
          (org-canvas--gradebook-number required)
          (org-canvas--gradebook-student-percent-text now)
          (org-canvas--gradebook-student-percent-text best)
          (cond ((and now (>= now required)) "meets it now")
                ((and best (>= best required)) "within reach at best")
                (t "out of reach even at best"))))

(defun org-canvas--gradebook-insert-student-group (group rows context)
  "Insert one assignment GROUP's section for ROWS at point.
CONTEXT is the report's plist: the group's standing and best sums are
read from its :now-pg and :best-pg, its weight from :weighted, its
hurdle from :hurdles.  Nil ROWS write nothing."
  (when rows
    (let* ((now (org-canvas--gradebook-percent
                 (cdr (assq group (plist-get context :now-pg)))))
           (best (org-canvas--gradebook-percent
                  (cdr (assq group (plist-get context :best-pg)))))
           (required (cdr (assoc (plist-get group :name)
                                 (plist-get context :hurdles) #'equal))))
      (insert (format "* %s\n" (org-canvas--gradebook-cell-text
                                (plist-get group :name))))
      (insert (format "%s — %s now, %s at best\n"
                      (org-canvas--gradebook-student-group-text
                       group (plist-get context :weighted))
                      (org-canvas--gradebook-student-percent-text now)
                      (org-canvas--gradebook-student-percent-text best)))
      (when required
        (insert (format "%s\n" (org-canvas--gradebook-student-hurdle-text
                                 required now best))))
      (insert "| Column | Due | Lock | Score | Points | State | Late | Missing | Open |\n"
              "|---+---+---+---+---+---+---+---+---|\n")
      (mapc #'org-canvas--gradebook-insert-student-row rows)
      (insert "\n"))))

(defun org-canvas--gradebook-insert-student-groups (context)
  "Insert one section per assignment group of CONTEXT at point.
A row whose assignment group the groups read lacks goes under Without a
group; no rows at all write a line saying so."
  (let ((rows (plist-get context :rows))
        (used nil))
    (if (null rows)
        (insert "No assignments.\n")
      (dolist (group (plist-get context :groups))
        (let ((group-rows (cl-remove-if-not
                           (lambda (r) (eql (plist-get r :group-id)
                                            (plist-get group :id)))
                           rows)))
          (setq used (append used group-rows))
          (org-canvas--gradebook-insert-student-group group group-rows context)))
      (org-canvas--gradebook-insert-student-group
       (list :id nil :name "Without a group" :weight 0)
       (cl-remove-if (lambda (r) (memq r used)) rows)
       context))))

(defun org-canvas--gradebook-student-total-cell (row grades key)
  "Return ROW's Canvas KEY total as text, with its letter when it has one.
KEY is `:current' or `:final'; the letter comes from GRADES, honouring
`org-canvas-gradebook-unposted'."
  (let* ((prefix (if org-canvas-gradebook-unposted "unposted_" ""))
         (name (substring (symbol-name key) 1))
         (letter (org-canvas--alist-get-non-null
                  (intern (format "%s%s_grade" prefix name)) grades)))
    (concat (org-canvas--gradebook-number (plist-get row key))
            (if (stringp letter) (format " (%s)" letter) ""))))

(defun org-canvas--gradebook-student-unmatched-hurdles (context)
  "Return the hurdle pairs of CONTEXT naming no assignment group."
  (cl-remove-if (lambda (pair)
                  (cl-find-if (lambda (g) (equal (plist-get g :name) (car pair)))
                              (plist-get context :groups)))
                (plist-get context :hurdles)))

(defun org-canvas--gradebook-insert-student-head (context)
  "Insert the report's header lines for CONTEXT at point."
  (let ((row (plist-get context :row))
        (sections (plist-get context :sections)))
    (insert "#+TITLE: Student standing: "
            (org-canvas--gradebook-cell-text (plist-get row :name)) "\n\n")
    (insert (format "Read %s (%s scores); nothing was sent, nothing written.\n\n"
                    (format-time-string "%Y-%m-%d %H:%M" (plist-get context :now))
                    (if (plist-get context :unposted) "unposted" "posted")))
    (insert (format "%s (user %s)%s\n"
                    (org-canvas--gradebook-cell-text (plist-get row :name))
                    (plist-get row :user-id)
                    (if (string-empty-p sections) "" (format ", %s" sections))))
    (insert (format "Canvas: current %s, final %s\n\n"
                    (org-canvas--gradebook-student-total-cell
                     row (plist-get context :grades) :current)
                    (org-canvas--gradebook-student-total-cell
                     row (plist-get context :grades) :final)))))

(defun org-canvas--gradebook-insert-student-summary (context)
  "Insert the still-open list and the ceiling for CONTEXT at point."
  (let* ((open (org-canvas--gradebook-student-open-rows
                (plist-get context :rows) (plist-get context :now)))
         (best (org-canvas--gradebook-total
                (plist-get context :best-pg) (plist-get context :weighted)))
         (letter (org-canvas--gradebook-letter best (plist-get context :scheme))))
    (if (null open)
        (insert "No column past due is still open.\n")
      (insert (format "Still open and past due: %s\n"
                      (mapconcat (lambda (r) (plist-get r :name)) open ", "))))
    (insert (format "Best possible: %s%s, every still-open column at full marks.\n"
                   (org-canvas--gradebook-student-percent-text best)
                   (if letter (format " (%s)" letter) "")))
    (dolist (pair (org-canvas--gradebook-student-unmatched-hurdles context))
      (insert (format "No group named %s for its hurdle.\n" (car pair))))))

(defun org-canvas--gradebook-insert-student-report (context)
  "Insert the whole student report for CONTEXT at point."
  (org-canvas--gradebook-insert-student-head context)
  (org-canvas--gradebook-insert-student-groups context)
  (org-canvas--gradebook-insert-student-summary context)
  (delay-mode-hooks (org-mode))
  (org-table-map-tables #'org-table-align t))

(defun org-canvas--gradebook-student-context (name-or-id)
  "Return the report context for the student NAME-OR-ID names.
Reads the roster, the course, the assignment groups with their
assignments, the student's submissions with each assignment, the
section names and the grading scheme: everything the report shows.
Nothing is written."
  (let* ((enrollments (org-canvas--gradebook-fetch-enrollments))
         (mine (org-canvas--gradebook-student-pick enrollments name-or-id))
         (row (car (org-canvas--gradebook-rows mine nil)))
         (course (org-canvas-api-request
                  'GET (org-canvas-api-course-endpoint "")))
         (raw-groups (append (org-canvas-api-request-all-pages
                              'GET (org-canvas-api-course-endpoint "assignment_groups")
                              '(("include[]" . "assignments")))
                             nil))
         (assignments (org-canvas--gradebook-assignment-map raw-groups))
         (groups (org-canvas--gradebook-canvas-groups raw-groups))
         (now (current-time))
         (rows (org-canvas--gradebook-student-rows
                (org-canvas--gradebook-student-submissions (plist-get row :user-id))
                assignments now))
         (items (org-canvas--gradebook-student-graded-items rows))
         (ceiling (org-canvas--gradebook-student-ceiling-items rows)))
    (list :row row
          :grades (alist-get 'grades (car mine))
          :sections (org-canvas--gradebook-sections-cell
                     row (org-canvas--gradebook-fetch-section-names))
          :weighted (eq (alist-get 'apply_assignment_group_weights course) t)
          :scheme (org-canvas--gradebook-fetch-scheme course enrollments)
          :hurdles org-canvas-gradebook-hurdles
          :groups groups
          :rows rows
          :now now
          :unposted org-canvas-gradebook-unposted
          :now-pg (org-canvas--gradebook-student-groups items groups assignments)
          :best-pg (org-canvas--gradebook-student-groups ceiling groups assignments))))

;;;###autoload
(defun org-canvas-gradebook-student (&optional name-or-id)
  "Report one student's standing: every column, what is open, the ceiling.
NAME-OR-ID is a Canvas user id (5001 or #5001), a sortable
name (\"Last, First\", what gradebook.org's Students table shows) or a
plain one; interactively it is completed from the roster.  The report prints
every column with its score, points, state, late and missing flags, due
and lock dates and whether it is still open, grouped by assignment
group with its weight and drop rules; the columns past due but still
open, the only work the student can still turn in; and the best
possible total if every still-open column scored full marks, drop
rules kept, closed ungraded columns counting zero.  Read-only: nothing
is sent and nothing written (issue #542)."
  (interactive (list (org-canvas--gradebook-student-prompt)))
  (org-canvas--report-display
   "*canvas-student*"
   (lambda ()
     (org-canvas--gradebook-insert-student-report
      (org-canvas--gradebook-student-context name-or-id)))
   #'org-mode))

;;;; What-If
;;
;; The what-if answers "what would every total be under this group
;; table?" before the table is pushed, since pushing and reading back
;; is something students see.  It reads assignment-groups.org as it
;; stands (its buffer when visited, unsaved edits and all), pairs each
;; group with Canvas's by CANVAS_ID or, without one, by name, and
;; recomputes every total from the scores the last gradebook pull kept.
;; Weighting comes from APPLY_WEIGHTS in settings.org, else from the
;; course as pulled; letters from the course's grading scheme as
;; pulled.  Nothing is sent.

(defun org-canvas--gradebook-prop-number (prop)
  "Return the number in the property PROP of the entry at point, or nil."
  (let ((value (org-entry-get nil prop)))
    (and value (not (string-empty-p (string-trim value))) (string-to-number value))))

(defun org-canvas--gradebook-local-group ()
  "Return the assignment group heading at point as a group plist."
  (list :id (org-canvas--gradebook-prop-number "CANVAS_ID")
        :name (org-get-heading t t t t)
        :weight (or (org-canvas--gradebook-prop-number "WEIGHT") 0)
        :drop-lowest (org-canvas--gradebook-prop-number "DROP_LOWEST")
        :drop-highest (org-canvas--gradebook-prop-number "DROP_HIGHEST")
        :never-drop (mapcar #'string-to-number
                            (split-string (or (org-entry-get nil "NEVER_DROP") "") "," t " "))))

(defun org-canvas--gradebook-with-org-file (file fn)
  "Return what FN answers in the Org FILE as it stands.
The buffer visiting FILE, unsaved edits and all, widened; else the
file's text in a temporary buffer."
  (let ((buffer (find-buffer-visiting file)))
    (if buffer
        (with-current-buffer buffer
          (org-with-wide-buffer (goto-char (point-min)) (funcall fn)))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (funcall fn)))))

(defun org-canvas--gradebook-local-groups (file)
  "Return the assignment groups of FILE as group plists, in file order."
  (org-canvas--gradebook-with-org-file
   file (lambda () (org-map-entries #'org-canvas--gradebook-local-group "LEVEL=2+WEIGHT={.}"))))

(defun org-canvas--gradebook-local-weighted (fallback)
  "Return non-nil when the group weights are on in settings.org.
FALLBACK, the course as pulled, when the file does not say."
  (let* ((file (and (boundp 'org-canvas-settings-file) org-canvas-settings-file))
         (value (and file (file-exists-p file)
                     (car (org-canvas--gradebook-with-org-file
                           file (lambda () (org-map-entries
                                            (lambda () (org-entry-get nil "APPLY_WEIGHTS"))
                                            "APPLY_WEIGHTS={.}")))))))
    (if value
        (org-canvas--interpret-boolean (downcase (string-trim value)))
      fallback)))

(defun org-canvas--gradebook-match-local (group locals)
  "Return the group of LOCALS that stands for Canvas's GROUP, or nil.
By CANVAS_ID; a local group without one matches by name."
  (or (cl-find-if (lambda (l) (eql (plist-get l :id) (plist-get group :id))) locals)
      (cl-find-if (lambda (l) (and (null (plist-get l :id))
                                   (equal (plist-get l :name) (plist-get group :name))))
                  locals)))

(defun org-canvas--gradebook-what-if-groups (canvas-groups locals)
  "Return (GROUPS . NOTES): CANVAS-GROUPS under the weights and rules of LOCALS.
A Canvas group no local group stands for keeps Canvas's weight and
rules; a local group standing for no Canvas group holds no scores.
Each case is a line of NOTES."
  (let* ((notes nil)
         (groups (mapcar (lambda (g)
                           (let ((local (org-canvas--gradebook-match-local g locals)))
                             (if local
                                 (plist-put (copy-sequence local) :id (plist-get g :id))
                               (push (format "%s is not in assignment-groups.org: Canvas's weight and rules are kept"
                                             (plist-get g :name))
                                     notes)
                               g)))
                         canvas-groups)))
    (dolist (l locals)
      (unless (cl-find-if (lambda (g) (eq (org-canvas--gradebook-match-local g (list l)) l))
                          canvas-groups)
        (push (format "%s is not on Canvas as pulled: it holds no scores yet" (plist-get l :name))
              notes)))
    (cons groups (nreverse notes))))

(defun org-canvas--gradebook-what-if-rows (scores groups weighted)
  "Return (NAME CANVAS RECOMPUTED WHAT-IF) for each student of SCORES.
CANVAS is Canvas's total as pulled, RECOMPUTED the total under
Canvas's own table, and WHAT-IF the total under GROUPS and WEIGHTED."
  (let ((assignments (plist-get scores :assignments)))
    (mapcar (lambda (s)
              (list (plist-get s :name)
                    (plist-get s :canvas)
                    (org-canvas--gradebook-student-total
                     s (plist-get scores :groups) assignments (plist-get scores :weighted))
                    (org-canvas--gradebook-student-total s groups assignments weighted)))
            (plist-get scores :students))))

(defun org-canvas--gradebook-insert-distribution (series scheme)
  "Insert one line per (LABEL . TOTALS) of SERIES: mean, median, letters.
The letter columns are SCHEME's, best first; none without a scheme."
  (let ((letters (mapcar #'car (sort (copy-sequence scheme) (lambda (a b) (> (cdr a) (cdr b)))))))
    (insert (format "| Table | Mean | Median |%s\n"
                    (mapconcat (lambda (l) (format " %s |" l)) letters "")))
    (insert (format "|---+---+---|%s\n" (mapconcat (lambda (_) "---|") letters "")))
    (dolist (s series)
      (let ((counts (org-canvas--gradebook-letter-counts (cdr s) scheme)))
        (insert (format "| %s | %s | %s |%s\n" (car s)
                        (org-canvas--gradebook-number (org-canvas--gradebook-mean (cdr s)))
                        (org-canvas--gradebook-number (org-canvas--gradebook-median (cdr s)))
                        (mapconcat (lambda (l) (format " %d |" (or (cdr (assoc l counts)) 0)))
                                   letters "")))))))

(defun org-canvas--gradebook-group-change (canvas group)
  "Return the table line comparing GROUP with CANVAS when they disagree.
Nil when weight, drop rules and never-drop ids agree."
  (let ((cells (mapcar (lambda (key)
                         (cons (org-canvas--gradebook-number (or (plist-get canvas key) 0))
                               (org-canvas--gradebook-number (or (plist-get group key) 0))))
                       '(:weight :drop-lowest :drop-highest))))
    (unless (and (cl-every (lambda (c) (equal (car c) (cdr c))) cells)
                 (equal (sort (copy-sequence (plist-get canvas :never-drop)) #'<)
                        (sort (copy-sequence (plist-get group :never-drop)) #'<)))
      (format "| %s |%s\n" (org-canvas--gradebook-cell-text (plist-get canvas :name))
              (mapconcat (lambda (c) (format " %s -> %s |" (car c) (cdr c))) cells "")))))

(defun org-canvas--gradebook-insert-group-changes (canvas-groups groups)
  "Insert a line per group of GROUPS whose table differs from CANVAS-GROUPS'."
  (let ((lines (delq nil (cl-mapcar #'org-canvas--gradebook-group-change canvas-groups groups))))
    (if (null lines)
        (insert "No group's weight or drop rules differ from Canvas's.\n")
      (insert "| Group | Weight | Drop lowest | Drop highest |\n|---+---+---+---|\n")
      (mapc #'insert lines))))

(defun org-canvas--gradebook-insert-letter-changes (rows scheme)
  "Insert the students of ROWS whose letter under SCHEME would move.
ROWS are from `org-canvas--gradebook-what-if-rows'; the letter now is
SCHEME's for Canvas's total, so a change is the table's doing."
  (let ((moved (cl-remove-if (lambda (r) (equal (org-canvas--gradebook-letter (nth 1 r) scheme)
                                                (org-canvas--gradebook-letter (nth 3 r) scheme)))
                             rows)))
    (cond
     ((null scheme) (insert "The course has no grading scheme as pulled, so no letters.\n"))
     ((null moved) (insert "No student's letter changes.\n"))
     (t
      (insert "| Student | Canvas now | What-if | Letter now | What-if letter |\n|---+---+---+---+---|\n")
      (dolist (r moved)
        (insert (format "| %s | %s | %s | %s | %s |\n"
                        (org-canvas--gradebook-cell-text (nth 0 r))
                        (org-canvas--gradebook-number (nth 1 r))
                        (org-canvas--gradebook-number (nth 3 r))
                        (or (org-canvas--gradebook-letter (nth 1 r) scheme) "-")
                        (or (org-canvas--gradebook-letter (nth 3 r) scheme) "-"))))))))

(defun org-canvas--gradebook-render-what-if (scores groups weighted notes)
  "Insert the what-if report for SCORES under GROUPS and WEIGHTED, with NOTES."
  (let ((rows (org-canvas--gradebook-what-if-rows scores groups weighted))
        (scheme (plist-get scores :scheme)))
    (insert "#+TITLE: Gradebook what-if\n\n"
            (format "Scores pulled %s (%s); group table from assignment-groups.org as it stands, weights %s.  Nothing was sent to Canvas.\n"
                    (plist-get scores :pulled-at)
                    (if (plist-get scores :unposted) "unposted" "posted")
                    (if weighted "on" "off")))
    (dolist (note notes) (insert (format "- %s\n" note)))
    (insert "\n* Distribution\n\n")
    (org-canvas--gradebook-insert-distribution
     (list (cons "Canvas now" (mapcar (lambda (r) (nth 1 r)) rows))
           (cons "Canvas's table, recomputed" (mapcar (lambda (r) (nth 2 r)) rows))
           (cons "assignment-groups.org" (mapcar (lambda (r) (nth 3 r)) rows)))
     scheme)
    (insert "\n* Group changes\n\n")
    (org-canvas--gradebook-insert-group-changes (plist-get scores :groups) groups)
    (insert "\n* Letter changes\n\n")
    (org-canvas--gradebook-insert-letter-changes rows scheme)
    (delay-mode-hooks (org-mode))
    (org-table-map-tables #'org-table-align t)))

;;;###autoload
(defun org-canvas-gradebook-what-if ()
  "Report every course total under assignment-groups.org as it stands.
Weights and drop rules are read from the file, pushed or not, and
every total is recomputed from the scores the last gradebook pull
kept (pull with `org-canvas-gradebook-groups' on).  The report shows
the mean, the median and the letter counts under Canvas's totals now
and under the file's table, the groups whose table differs, and the
students whose letter changes.  Read-only: nothing is sent."
  (interactive)
  (let ((scores (org-canvas--gradebook-read-scores))
        (file (and (boundp 'org-canvas-assignment-groups-file)
                   (expand-file-name org-canvas-assignment-groups-file))))
    (unless scores
      (user-error "No scores pulled yet: turn on `org-canvas-gradebook-groups' and run `org-canvas-pull-gradebook'"))
    (unless (and file (file-exists-p file))
      (user-error "No assignment-groups.org to read the group table from"))
    (let ((merged (org-canvas--gradebook-what-if-groups
                   (plist-get scores :groups) (org-canvas--gradebook-local-groups file)))
          (weighted (org-canvas--gradebook-local-weighted (plist-get scores :weighted))))
      (org-canvas--report-display
       "*canvas-gradebook-what-if*"
       (lambda () (org-canvas--gradebook-render-what-if scores (car merged) weighted (cdr merged)))
       #'org-mode))))

(provide 'org-canvas-gradebook-report)
;;; org-canvas-gradebook-report.el ends here
