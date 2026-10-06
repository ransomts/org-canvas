;;; org-canvas-gradebook.el --- Pull a course-wide grade overview from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module pulls a course-wide overview of grades into
;; gradebook.org: each student's current and final score, how many
;; submissions are missing or late, a per-section summary, and the
;; score spread of every assignment.  It answers "who is falling
;; behind?" and "how did the class do on Homework 3?" without opening
;; the Canvas gradebook or pulling one grading file per assignment.  It
;; is pull-only, and its
;; tables are derived: every pull rewrites them, so nothing written by
;; hand under its headings survives.
;;
;; FILE STRUCTURE
;; ==============
;; In gradebook.org:
;;   * Students   one table, sorted by name: Student, Sections, Current,
;;                Final, Missing, Late, Last activity, then one column
;;                per custom gradebook column the course has
;;   * Sections   one table: Section, Students, Mean current, Mean final,
;;                Missing
;;   * Assignments one table, in Canvas order: Assignment, Due, Points,
;;                Min, Q1, Median, Q3, Max, Missing, Late
;;   * Groups     with `org-canvas-gradebook-groups' only: Student, one
;;                column per assignment group (its percentage after the
;;                drop rules), Computed, Canvas, and a line naming the
;;                students whose two totals differ by more than 0.1
;;
;; A student is one row however many sections they sit in.  The Student
;; cell links to the roster heading in people.org when that file holds
;; the user id.  Current is the score over graded work; Final counts
;; every ungraded assignment as zero, work not yet due included, so it
;; sits low early in the term.  `org-canvas-gradebook-unposted' (default t)
;; takes the instructor's unposted scores, hidden grades included; nil
;; takes what students see.  No per-student, per-assignment cells: that
;; is what grading files are for; the Assignments table is the class's
;; spread on each assignment, linked to its heading in assignments.org
;; when that file holds the id.  Custom gradebook columns (the Notes column,
;; anything added in the gradebook) follow Last activity in Canvas's
;; order, titled as in Canvas; a course without any gets the seven
;; columns and nothing more.  Hidden columns stay out unless
;; `org-canvas-gradebook-include-hidden-columns' is set.
;;
;; PERSONAL DATA
;; =============
;; Scores are more sensitive than names.  Treat gradebook.org as the
;; submissions directory is treated: keep it out of a course repository
;; (add it to .gitignore) and out of anything shared.  Only the name,
;; the numbers and the custom column text are written, never an
;; identifier; a Notes column is free text about a student, so it is
;; the most sensitive cell in the file.  The module writes
;; `org-canvas-gradebook-file' and, with `org-canvas-gradebook-groups',
;; `org-canvas-gradebook-scores-file': every student's score on every
;; assignment with their user ids, kept for
;; `org-canvas-gradebook-what-if'.  Keep both out of a repository; when
;; the course directory is in a git work tree the scores file is added
;; to the .gitignore beside it, unless git ignores it already, and a
;; tracked one is warned about rather than touched.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/enrollments?type[]=StudentEnrollment
;;       each enrollment carries a `grades' object (current_score,
;;       final_score, their unposted_* twins, letter grades when the
;;       course has a scheme).  Paginated by bookmark.
;;   GET /courses/:id/analytics/student_summaries
;;       per student: tardiness_breakdown (missing, late, on_time,
;;       floating, total), page views, participations.  Permission
;;       gated: a 403 leaves the Missing and Late columns blank with a
;;       note under the table, and the pull goes on.
;;   GET /courses/:id/sections           section names
;;   GET /courses/:id/custom_gradebook_columns[?include_hidden=true]
;;       id, title, position, hidden, teacher_notes, read_only; the
;;       hidden ones only with the parameter.
;;   GET /courses/:id/custom_gradebook_columns/:id/data
;;       user_id and content per student who has an entry.  Paginated.
;;       A refusal of either request logs one warning and leaves the
;;       custom columns out; the pull goes on.
;;   GET /courses/:id/analytics/assignments
;;       per assignment: points_possible, due_at, min_score, max_score,
;;       first_quartile, median, third_quartile (null until enough work
;;       is graded) and its own tardiness_breakdown.  Permission gated
;;       like the summaries: a 403 leaves the Assignments heading with
;;       a note, and the pull goes on.
;;   GET /courses/:id                    apply_assignment_group_weights,
;;       grading_standard_id (Groups only, as are the next three)
;;   GET /courses/:id/assignment_groups?include[]=assignments
;;       weights, rules (drop_lowest, drop_highest, never_drop), and
;;       each assignment's points, published and omit_from_final_grade
;;   GET /courses/:id/students/submissions?student_ids[]=all
;;       score, excused and posted_at of every submission.  Paginated.
;;       A refusal of any of these leaves the Groups heading with a note.
;;   GET /courses/:id/grading_standards/:id
;;       the letter cutoffs, as fractions; a refusal leaves the letters
;;       out of the what-if.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(defvar org-canvas-assignment-groups-file)
(defvar org-canvas-settings-file)

;;;; Configuration

(defcustom org-canvas-gradebook-file (org-canvas--path "gradebook.org")
  "Path to the gradebook.org file.
It holds every student's scores once pulled; keep it out of a course
repository."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-gradebook-file "gradebook.org")

(defcustom org-canvas-gradebook-unposted t
  "When non-nil, show the instructor's unposted scores, hidden grades included.
When nil, show the scores students see.  On a course that posts grades
automatically the two never differ."
  :type 'boolean
  :group 'org-canvas)

(defcustom org-canvas-gradebook-include-hidden-columns nil
  "When non-nil, pull custom gradebook columns hidden in the gradebook too.
By default only the columns shown in Canvas's gradebook reach the
Students table."
  :type 'boolean
  :group 'org-canvas)

(defcustom org-canvas-gradebook-groups nil
  "When non-nil, pull every score and write a per-group table.
The Groups heading of gradebook.org then shows each student's
percentage in each assignment group after the drop rules, recomputed
from every submission, and checks the recomputed total against
Canvas's.  The scores are kept in `org-canvas-gradebook-scores-file'
for `org-canvas-gradebook-what-if'.  Off by default: it costs one
request per hundred submissions."
  :type 'boolean
  :group 'org-canvas)

(defcustom org-canvas-gradebook-scores-file (org-canvas--path "gradebook-scores.eld")
  "Path to the scores the last gradebook pull kept for the what-if.
Written when `org-canvas-gradebook-groups' is on.  It holds every
student's score on every assignment; keep it out of a course
repository."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-gradebook-scores-file "gradebook-scores.eld")

(org-canvas-register-properties "gradebook"
  :label "Gradebook"
  :file-var 'org-canvas-gradebook-file
  :query "LEVEL=1"
  :properties nil)

;;;; Fetching

(defun org-canvas--gradebook-fetch-enrollments ()
  "Return the course's student enrollments, active, inactive and completed."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint "enrollments")
           '(("type[]" . "StudentEnrollment")
             ("state[]" . "active") ("state[]" . "inactive") ("state[]" . "completed")))
          nil))

(defun org-canvas--gradebook-fetch-section-names ()
  "Return an alist of section id to name for the course."
  (mapcar (lambda (s) (cons (alist-get 'id s) (alist-get 'name s)))
          (append (org-canvas-api-request-all-pages
                   'GET (org-canvas-api-course-endpoint "sections"))
                  nil)))

(defun org-canvas--gradebook-fetch-summaries ()
  "Return an alist of user id to tardiness breakdown, or the symbol `refused'.
The analytics endpoint is permission gated; a refusal is logged and
answered with `refused' so the tables are written without the Missing
and Late columns rather than not at all (the #171 rule: one refused
request costs its column, not the pull)."
  (condition-case err
      (mapcar (lambda (s) (cons (alist-get 'id s) (alist-get 'tardiness_breakdown s)))
              (append (org-canvas-api-request-all-pages
                       'GET (org-canvas-api-course-endpoint "analytics/student_summaries"))
                      nil))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Gradebook] Could not read the student summaries (%s); Missing and Late are left blank"
       (error-message-string err))
     'refused)))

(defun org-canvas--gradebook-fetch-column-data (column)
  "Return an alist of user id to content for the custom gradebook COLUMN.
COLUMN is Canvas's column object; a student without an entry is absent."
  (mapcar (lambda (d) (cons (alist-get 'user_id d) (alist-get 'content d)))
          (append (org-canvas-api-request-all-pages
                   'GET (org-canvas-api-course-endpoint
                         "custom_gradebook_columns/%s/data" (alist-get 'id column)))
                  nil)))

(defun org-canvas--gradebook-fetch-columns ()
  "Return the course's custom gradebook columns with their data, in position order.
Each is a plist with :title and :data, the alist from
`org-canvas--gradebook-fetch-column-data'.  Hidden columns come only
with `org-canvas-gradebook-include-hidden-columns'.  A refusal is
logged and answered with nil, so the Students table is written without
the custom columns rather than not at all (the #171 rule)."
  (condition-case err
      (let ((columns (append (org-canvas-api-request-all-pages
                              'GET (org-canvas-api-course-endpoint "custom_gradebook_columns")
                              (when org-canvas-gradebook-include-hidden-columns
                                '(("include_hidden" . "true"))))
                             nil)))
        (mapcar (lambda (c) (list :title (alist-get 'title c)
                                  :data (org-canvas--gradebook-fetch-column-data c)))
                (sort columns (lambda (a b)
                                (< (or (alist-get 'position a) 0)
                                   (or (alist-get 'position b) 0))))))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Gradebook] Could not read the custom gradebook columns (%s); they are left out"
       (error-message-string err))
     nil)))

(defun org-canvas--gradebook-fetch-assignments ()
  "Return the course's assignment analytics rows, or the symbol `refused'.
One alist per assignment in Canvas order, from the analytics
endpoint, which is permission gated: a refusal is logged and answered
with `refused' so the Assignments heading carries a note rather than
costing the pull (the #171 rule)."
  (condition-case err
      (append (org-canvas-api-request-all-pages
               'GET (org-canvas-api-course-endpoint "analytics/assignments"))
              nil)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Gradebook] Could not read the assignment analytics (%s); the Assignments table is left out"
       (error-message-string err))
     'refused)))

;;;; Folding Enrollments Into Rows

(defun org-canvas--gradebook-score (grades key)
  "Return the KEY score from GRADES, honouring `org-canvas-gradebook-unposted'.
KEY is `current' or `final'; nil when Canvas has no number."
  (let* ((name (format "%s%s_score" (if org-canvas-gradebook-unposted "unposted_" "") key))
         (value (alist-get (intern name) grades)))
    (and (numberp value) value)))

(defun org-canvas--gradebook-rows (enrollments summaries)
  "Fold ENROLLMENTS into one plist per student, sorted by name.
Each plist has :user-id, :name, :section-ids, :current, :final,
:missing, :late and :last-activity.  SUMMARIES is the alist from
`org-canvas--gradebook-fetch-summaries', or `refused', in which case
:missing and :late are nil."
  (let ((rows (make-hash-table :test 'eql)))
    (dolist (e enrollments)
      (let* ((uid (alist-get 'user_id e))
             (user (alist-get 'user e))
             (grades (alist-get 'grades e))
             (row (or (gethash uid rows)
                      (puthash uid (list :user-id uid
                                         :name (or (alist-get 'sortable_name user)
                                                   (alist-get 'name user)
                                                   (format "User %s" uid))
                                         :section-ids nil
                                         :current nil :final nil
                                         :missing nil :late nil
                                         :last-activity nil)
                               rows))))
        (let ((sid (alist-get 'course_section_id e)))
          (when (and sid (not (memq sid (plist-get row :section-ids))))
            (plist-put row :section-ids (append (plist-get row :section-ids) (list sid)))))
        (unless (plist-get row :current)
          (plist-put row :current (org-canvas--gradebook-score grades 'current)))
        (unless (plist-get row :final)
          (plist-put row :final (org-canvas--gradebook-score grades 'final)))
        (let ((seen (org-canvas--alist-get-non-null 'last_activity_at e)))
          (when (and (stringp seen)
                     (or (null (plist-get row :last-activity))
                         (string> seen (plist-get row :last-activity))))
            (plist-put row :last-activity seen)))
        (unless (eq summaries 'refused)
          (let ((t-b (alist-get uid summaries)))
            (when (listp t-b)
              (plist-put row :missing (alist-get 'missing t-b))
              (plist-put row :late (alist-get 'late t-b)))))))
    (let (result)
      (maphash (lambda (_uid row) (push row result)) rows)
      (sort result (lambda (a b) (string< (plist-get a :name) (plist-get b :name)))))))

;;;; Rendering

(defun org-canvas--gradebook-number (value)
  "Render VALUE for a table cell: one decimal for a score, - when absent."
  (cond ((null value) "-")
        ((integerp value) (format "%d" value))
        ((numberp value) (format "%.1f" value))
        (t (format "%s" value))))

(defun org-canvas--gradebook-roster-heading (user-id)
  "Return the people.org heading carrying USER-ID, or nil.
Nil as well when `org-canvas-people-file' is unset or missing."
  (org-canvas--heading-title-by-property
   (and (boundp 'org-canvas-people-file) org-canvas-people-file)
   "USER_ID" user-id "USER_ID={.}"))

(defun org-canvas--gradebook-heading-link (file heading text)
  "Return an Org link to HEADING in the course FILE, shown as TEXT.
Brackets Org escaped in HEADING are unescaped, so the link target is
the heading as written."
  (org-link-make-string
   (format "file:%s::*%s" (file-name-nondirectory file)
           (replace-regexp-in-string "\\\\\\([][]\\)" "\\1" heading))
   text))

(defun org-canvas--gradebook-student-cell (row)
  "Return ROW's Student cell: a link to the roster heading when known, else the name."
  (let ((heading (org-canvas--gradebook-roster-heading (plist-get row :user-id)))
        (name (plist-get row :name)))
    (if heading
        (org-canvas--gradebook-heading-link org-canvas-people-file heading name)
      name)))

(defun org-canvas--gradebook-sections-cell (row names)
  "Return ROW's Sections cell from the id-to-name alist NAMES."
  (mapconcat (lambda (sid) (or (alist-get sid names) (format "%s" sid)))
             (plist-get row :section-ids) ", "))

(defun org-canvas--gradebook-cell-text (text)
  "Return TEXT fit for one table cell, or an empty string.
Pipes and line breaks would split the cell, so each run of them, with
the blanks around it, becomes one space."
  (if (stringp text)
      (string-trim (replace-regexp-in-string "[ \t]*[|\n\r]+[ \t]*" " " text))
    ""))

(defun org-canvas--gradebook-custom-cells (row columns)
  "Return ROW's cells for the custom COLUMNS as one string of table cells.
Empty when there are no columns; a student without an entry gets a
blank cell."
  (mapconcat (lambda (column)
               (format " %s |" (org-canvas--gradebook-cell-text
                                (alist-get (plist-get row :user-id) (plist-get column :data)))))
             columns ""))

(defun org-canvas--gradebook-insert-students (rows names summaries &optional columns)
  "Insert the Students table for ROWS at point.
NAMES maps section ids to names; SUMMARIES is `refused' when the
Missing and Late columns are unavailable; COLUMNS are the custom
gradebook columns from `org-canvas--gradebook-fetch-columns', one
table column each after Last activity."
  (insert (format "| Student | Sections | Current | Final | Missing | Late | Last activity |%s\n"
                  (mapconcat (lambda (column)
                               (format " %s |" (org-canvas--gradebook-cell-text
                                                (plist-get column :title))))
                             columns "")))
  (insert (format "|---+---+---+---+---+---+---|%s\n"
                  (mapconcat (lambda (_) "---|") columns "")))
  (dolist (row rows)
    (insert (format "| %s | %s | %s | %s | %s | %s | %s |%s\n"
                    (org-canvas--gradebook-student-cell row)
                    (org-canvas--gradebook-sections-cell row names)
                    (org-canvas--gradebook-number (plist-get row :current))
                    (org-canvas--gradebook-number (plist-get row :final))
                    (if (eq summaries 'refused) "" (org-canvas--gradebook-number (plist-get row :missing)))
                    (if (eq summaries 'refused) "" (org-canvas--gradebook-number (plist-get row :late)))
                    (or (org-canvas--iso8601-to-org-timestamp (plist-get row :last-activity)) "-")
                    (org-canvas--gradebook-custom-cells row columns))))
  (when (eq summaries 'refused)
    (insert "\nMissing and Late are blank: Canvas refused the student summaries for this token.\n")))

(defun org-canvas--gradebook-mean (values)
  "Return the mean of the numbers in VALUES, or nil when there are none."
  (let ((numbers (cl-remove-if-not #'numberp values)))
    (and numbers (/ (apply #'+ numbers) (float (length numbers))))))

(defun org-canvas--gradebook-insert-sections (rows names summaries)
  "Insert the Sections table for ROWS at point, one line per section in NAMES.
SUMMARIES is `refused' when the Missing column is unavailable."
  (insert "| Section | Students | Mean current | Mean final | Missing |\n")
  (insert "|---+---+---+---+---|\n")
  (dolist (section names)
    (let* ((sid (car section))
           (members (cl-remove-if-not (lambda (r) (memq sid (plist-get r :section-ids))) rows)))
      (insert (format "| %s | %d | %s | %s | %s |\n"
                      (cdr section) (length members)
                      (org-canvas--gradebook-number
                       (org-canvas--gradebook-mean (mapcar (lambda (r) (plist-get r :current)) members)))
                      (org-canvas--gradebook-number
                       (org-canvas--gradebook-mean (mapcar (lambda (r) (plist-get r :final)) members)))
                      (if (eq summaries 'refused) ""
                        (org-canvas--gradebook-number
                         (apply #'+ (mapcar (lambda (r) (or (plist-get r :missing) 0)) members)))))))))

(defun org-canvas--gradebook-stat (row key)
  "Return the number under KEY in the analytics ROW, or nil.
Canvas answers null for a quartile until enough work is graded, and
null decodes as nil or `:null' depending on the reader."
  (let ((value (alist-get key row)))
    (and (numberp value) value)))

(defun org-canvas--gradebook-assignment-cell (row)
  "Return ROW's Assignment cell: a link to its heading, else the title.
The link goes to the assignments.org heading carrying the assignment id
as CANVAS_ID, when `org-canvas-assignments-file' is set and holds it."
  (let* ((title (or (alist-get 'title row)
                    (format "Assignment %s" (alist-get 'assignment_id row))))
         (file (and (boundp 'org-canvas-assignments-file) org-canvas-assignments-file))
         (heading (org-canvas--heading-title-by-property
                   file "CANVAS_ID" (alist-get 'assignment_id row))))
    (if heading
        (org-canvas--gradebook-heading-link file heading title)
      title)))

(defun org-canvas--gradebook-insert-assignment-row (row)
  "Insert the Assignments table line for the analytics ROW at point."
  (let ((tardiness (alist-get 'tardiness_breakdown row)))
    (insert (format "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n"
                    (org-canvas--gradebook-assignment-cell row)
                    (or (org-canvas--iso8601-to-org-timestamp (alist-get 'due_at row)) "-")
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat row 'points_possible))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat row 'min_score))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat row 'first_quartile))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat row 'median))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat row 'third_quartile))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat row 'max_score))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat tardiness 'missing))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat tardiness 'late))))))

(defun org-canvas--gradebook-insert-assignments (assignments)
  "Insert the Assignments table for the analytics rows ASSIGNMENTS at point.
`refused' writes a note instead; an empty list a `No assignments' line."
  (cond
   ((eq assignments 'refused)
    (insert "Canvas refused the assignment analytics for this token.\n"))
   ((null assignments)
    (insert "No assignments\n"))
   (t
    (insert "| Assignment | Due | Points | Min | Q1 | Median | Q3 | Max | Missing | Late |\n")
    (insert "|---+---+---+---+---+---+---+---+---+---|\n")
    (dolist (row assignments)
      (org-canvas--gradebook-insert-assignment-row row)))))

(defun org-canvas--gradebook-rewrite-body (title insert-fn)
  "Replace the body of the level-1 heading TITLE with INSERT-FN's output.
The heading is created when absent.  The body is derived from Canvas
on every pull, so nothing under the heading is kept."
  (let ((pos (org-canvas--pull-heading-by-title title)))
    (goto-char pos)
    (let ((body-start (save-excursion (org-end-of-meta-data t) (point)))
          (body-end (save-excursion (org-end-of-subtree t) (point))))
      (delete-region body-start body-end)
      (goto-char body-start)
      (insert "\n")
      (funcall insert-fn)
      (insert "\n")
      ;; Align the table just written.
      (save-excursion
        (goto-char body-start)
        (forward-line 1)
        (when (org-at-table-p) (org-table-align))))))

;;;; Group Scores: Computation
;;
;; Canvas's enrollments carry a course total and nothing per group, so
;; the Groups table and the what-if recompute each group the way
;; Canvas's grade calculator does, from every counted score:
;;
;;   - a score counts when its assignment is published and not omitted
;;     from the final grade, it is graded, and it is not excused; with
;;     `org-canvas-gradebook-unposted' nil, only when it is posted too
;;   - a group's drop rules drop the scores whose removal maximises the
;;     group's percentage (drop_lowest), then those whose removal
;;     minimises it (drop_highest), never one of never_drop, and always
;;     keep at least one droppable score.  The lowest score is not
;;     always the one to drop: 5/20 costs more than 0/1
;;   - weighted, the total is each group's percentage times its weight
;;     over the groups with points possible, scaled up when their
;;     weights sum under 100; unweighted, all points over all possible
;;
;; A score is an item, (ASSIGNMENT-ID SCORE POINTS); a group is a
;; plist, :id :name :weight :drop-lowest :drop-highest :never-drop.
;; Everything in this section is pure.

(defconst org-canvas--gradebook-default-scheme
  '(("A" . 94.0) ("A-" . 90.0) ("B+" . 87.0) ("B" . 84.0) ("B-" . 80.0)
    ("C+" . 77.0) ("C" . 74.0) ("C-" . 70.0) ("D+" . 67.0) ("D" . 64.0)
    ("D-" . 61.0) ("F" . 0.0))
  "Canvas's default grading scheme, as (LETTER . CUTOFF-PERCENT).")

(defun org-canvas--gradebook-sum (items index)
  "Return the sum of the INDEXth element of every item in ITEMS."
  (apply #'+ 0 (mapcar (lambda (item) (nth index item)) items)))

(defun org-canvas--gradebook-ratio (items)
  "Return ITEMS' summed score over their summed points, or nil without points."
  (let ((points (org-canvas--gradebook-sum items 2)))
    (and (> points 0) (/ (float (org-canvas--gradebook-sum items 1)) points))))

(defun org-canvas--gradebook-rank (items q keep lowest)
  "Return the KEEP items of ITEMS with the largest score less Q times points.
With LOWEST, the KEEP with the smallest."
  (let ((rated (mapcar (lambda (item) (cons (- (nth 1 item) (* q (nth 2 item))) item))
                       items)))
    (mapcar #'cdr (seq-take (sort rated (lambda (a b) (if lowest (< (car a) (car b))
                                                        (> (car a) (car b)))))
                            keep))))

(defun org-canvas--gradebook-keep (items keep fixed lowest)
  "Return the KEEP of ITEMS whose ratio, with the FIXED items, is highest.
With LOWEST, the KEEP whose ratio is lowest.  KEEP is at least one.
Dinkelbach's iteration: rank by score less Q times points, where Q is
the ratio of the set kept so far, until Q stops improving.  Each round
improves Q strictly, so it ends, and where it ends no set does better:
the exact optimum Canvas's bisection approximates."
  (let ((keep (max 1 keep)))
    (if (<= (length items) keep)
        items
      (let* ((kept (seq-take items keep))
             (q (org-canvas--gradebook-ratio (append kept fixed)))
             (done nil))
        (while (not done)
          (let* ((next (org-canvas--gradebook-rank items (or q 0.0) keep lowest))
                 (ratio (org-canvas--gradebook-ratio (append next fixed))))
            (if (and ratio (or (null q)
                               (if lowest (< ratio (- q 1e-12)) (> ratio (+ q 1e-12)))))
                (setq kept next q ratio)
              (setq done t))))
        kept))))

(defun org-canvas--gradebook-drop-from (items fixed lowest highest)
  "Return the droppable ITEMS that survive dropping LOWEST and HIGHEST.
FIXED are the never-drop items, which count toward the ratio but are
not returned.  At least one item is kept, and drop_highest gives way
when the two rules together would drop everything, as in Canvas.
Without points anywhere the rules drop by raw score."
  (let* ((n (length items))
         (lowest (min lowest (1- n)))
         (highest (if (>= (+ lowest highest) n) 0 highest))
         (keep-high (- n lowest))
         (keep-low (- keep-high highest)))
    (if (cl-some (lambda (item) (> (nth 2 item) 0)) items)
        (org-canvas--gradebook-keep
         (org-canvas--gradebook-keep items keep-high fixed nil) keep-low fixed t)
      (let ((sorted (sort (copy-sequence items) (lambda (a b) (< (nth 1 a) (nth 1 b))))))
        (seq-take (last sorted keep-high) keep-low)))))

(defun org-canvas--gradebook-drop (items group)
  "Return ITEMS less those GROUP's drop rules drop, as Canvas drops them."
  (let ((lowest (or (plist-get group :drop-lowest) 0))
        (highest (or (plist-get group :drop-highest) 0))
        (never (plist-get group :never-drop)))
    (if (and (<= lowest 0) (<= highest 0))
        items
      (let ((fixed (cl-remove-if-not (lambda (item) (member (car item) never)) items))
            (droppable (cl-remove-if (lambda (item) (member (car item) never)) items)))
        (if (null droppable)
            items
          (append (org-canvas--gradebook-drop-from droppable fixed lowest highest)
                  fixed))))))

(defun org-canvas--gradebook-group-sums (items group)
  "Return (SCORE . POSSIBLE) for ITEMS in GROUP after its drop rules.
Nil when no item counts."
  (let ((kept (org-canvas--gradebook-drop items group)))
    (and kept (cons (org-canvas--gradebook-sum kept 1)
                    (org-canvas--gradebook-sum kept 2)))))

(defun org-canvas--gradebook-percent (sums)
  "Return SUMS, a (SCORE . POSSIBLE), as a percentage, or nil."
  (and sums (> (cdr sums) 0) (* 100.0 (/ (float (car sums)) (cdr sums)))))

(defun org-canvas--gradebook-student-groups (items groups assignments)
  "Return a (GROUP . SUMS) per group of GROUPS for one student's ITEMS.
ASSIGNMENTS maps an assignment id to (GROUP-ID . POINTS); SUMS is
the group's (SCORE . POSSIBLE), nil when nothing in it counts."
  (mapcar (lambda (group)
            (cons group
                  (org-canvas--gradebook-group-sums
                   (cl-remove-if-not
                    (lambda (item) (eql (car (alist-get (car item) assignments))
                                        (plist-get group :id)))
                    items)
                   group)))
          groups))

(defun org-canvas--gradebook-weighted-total (per-group)
  "Return the weighted total of PER-GROUP, (GROUP . SUMS) pairs, or nil.
Only the groups with points possible count; when their weights sum
under 100 the total is scaled up to them, as Canvas does."
  (let* ((relevant (cl-remove-if-not (lambda (pg) (org-canvas--gradebook-percent (cdr pg)))
                                     per-group))
         (full (apply #'+ 0 (mapcar (lambda (pg) (or (plist-get (car pg) :weight) 0)) relevant))))
    (when (> full 0)
      (let ((grade (apply #'+ (mapcar (lambda (pg)
                                        (* (or (plist-get (car pg) :weight) 0)
                                           (/ (org-canvas--gradebook-percent (cdr pg)) 100.0)))
                                      relevant))))
        (if (< full 100) (/ (* grade 100.0) full) grade)))))

(defun org-canvas--gradebook-total (per-group weighted)
  "Return the course total from PER-GROUP, (GROUP . SUMS) pairs, or nil.
WEIGHTED applies the group weights; otherwise every kept point
counts over every kept point possible."
  (if weighted
      (org-canvas--gradebook-weighted-total per-group)
    (let ((sums (delq nil (mapcar #'cdr per-group))))
      (org-canvas--gradebook-percent
       (cons (apply #'+ 0 (mapcar #'car sums)) (apply #'+ 0 (mapcar #'cdr sums)))))))

(defun org-canvas--gradebook-letter (score scheme)
  "Return the letter SCHEME gives SCORE, or nil without either.
SCHEME is (LETTER . CUTOFF-PERCENT) pairs in any order; SCORE is
rounded to two decimals first, as Canvas rounds it."
  (when (and (numberp score) scheme)
    (let ((rounded (/ (fround (* score 100.0)) 100.0))
          (sorted (sort (copy-sequence scheme) (lambda (a b) (> (cdr a) (cdr b))))))
      (car (or (cl-find-if (lambda (entry) (>= rounded (cdr entry))) sorted)
               (car (last sorted)))))))

(defun org-canvas--gradebook-median (values)
  "Return the median of the numbers in VALUES, or nil when there are none."
  (let* ((numbers (sort (cl-remove-if-not #'numberp values) #'<))
         (n (length numbers)))
    (cond ((= n 0) nil)
          ((cl-oddp n) (float (nth (/ n 2) numbers)))
          (t (/ (+ (nth (1- (/ n 2)) numbers) (nth (/ n 2) numbers)) 2.0)))))

(defun org-canvas--gradebook-letter-counts (scores scheme)
  "Return (LETTER . COUNT) for the SCORES under SCHEME, best letter first.
Letters nobody gets are left out; so is a score that is nil."
  (let ((letters (mapcar (lambda (s) (org-canvas--gradebook-letter s scheme)) scores))
        (counts nil))
    (dolist (entry (sort (copy-sequence scheme) (lambda (a b) (> (cdr a) (cdr b)))))
      (let ((n (cl-count (car entry) letters :test #'equal)))
        (when (> n 0) (push (cons (car entry) n) counts))))
    (nreverse counts)))

;;;; Group Scores: From Canvas

(defun org-canvas--gradebook-rule-number (rules key)
  "Return the number under KEY in the Canvas RULES object, or nil."
  (let ((value (and (listp rules) (alist-get key rules))))
    (and (numberp value) value)))

(defun org-canvas--gradebook-canvas-group (group)
  "Return the Canvas assignment GROUP as a group plist."
  (let* ((rules (alist-get 'rules group))
         (never (and (listp rules) (alist-get 'never_drop rules))))
    (list :id (alist-get 'id group)
          :name (alist-get 'name group)
          :weight (let ((w (alist-get 'group_weight group))) (if (numberp w) w 0))
          :drop-lowest (org-canvas--gradebook-rule-number rules 'drop_lowest)
          :drop-highest (org-canvas--gradebook-rule-number rules 'drop_highest)
          :never-drop (and (sequencep never) (append never nil)))))

(defun org-canvas--gradebook-canvas-groups (raw-groups)
  "Return RAW-GROUPS, Canvas's assignment groups, as plists in position order."
  (mapcar #'org-canvas--gradebook-canvas-group
          (sort (copy-sequence raw-groups)
                (lambda (a b) (< (or (alist-get 'position a) 0)
                                 (or (alist-get 'position b) 0))))))

(defun org-canvas--gradebook-assignment-map (raw-groups)
  "Return an alist of assignment id to (GROUP-ID . POINTS) from RAW-GROUPS.
Only the assignments that count toward the grade: published and not
omitted from the final grade."
  (let (map)
    (dolist (group raw-groups)
      (dolist (a (append (alist-get 'assignments group) nil))
        (when (and (eq (alist-get 'published a) t)
                   (not (eq (alist-get 'omit_from_final_grade a) t)))
          (let ((points (alist-get 'points_possible a)))
            (push (cons (alist-get 'id a)
                        (cons (alist-get 'id group) (if (numberp points) points 0)))
                  map)))))
    (nreverse map)))

(defun org-canvas--gradebook-submission-item (submission assignments unposted)
  "Return SUBMISSION as an item when it counts toward the grade, else nil.
ASSIGNMENTS is the map from `org-canvas--gradebook-assignment-map'.
A score counts when it is graded and not excused, and, unless UNPOSTED,
when it is posted."
  (let* ((id (alist-get 'assignment_id submission))
         (entry (alist-get id assignments))
         (score (alist-get 'score submission)))
    (and entry (numberp score)
         (not (eq (alist-get 'excused submission) t))
         (or unposted (stringp (alist-get 'posted_at submission)))
         (list id score (cdr entry)))))

(defun org-canvas--gradebook-items-by-student (submissions assignments unposted)
  "Return a hash of user id to the counted items among SUBMISSIONS.
ASSIGNMENTS and UNPOSTED are as for `org-canvas--gradebook-submission-item'."
  (let ((by (make-hash-table :test 'eql)))
    (dolist (s submissions)
      (let ((item (org-canvas--gradebook-submission-item s assignments unposted)))
        (when item
          (push item (gethash (alist-get 'user_id s) by)))))
    by))

(defun org-canvas--gradebook-canvas-scheme (standard)
  "Return the Canvas grading STANDARD's entries as (LETTER . CUTOFF-PERCENT).
Canvas answers the cutoffs as fractions."
  (mapcar (lambda (e) (cons (alist-get 'name e) (* 100.0 (alist-get 'value e))))
          (append (alist-get 'grading_scheme standard) nil)))

(defun org-canvas--gradebook-letters-p (enrollments)
  "Return non-nil when any of ENROLLMENTS carries a letter grade."
  (cl-some (lambda (e) (let ((grades (alist-get 'grades e)))
                         (or (stringp (alist-get 'current_grade grades))
                             (stringp (alist-get 'unposted_current_grade grades)))))
           enrollments))

(defun org-canvas--gradebook-fetch-scheme (course enrollments)
  "Return the grading scheme of COURSE as (LETTER . CUTOFF-PERCENT), or nil.
A course naming a standard gets that standard; one naming none whose
ENROLLMENTS carry letters gets Canvas's default scheme; one without
letters gets nil.  A refused standard is logged and answered with
nil: the letters are left out, the pull goes on."
  (let ((id (alist-get 'grading_standard_id course)))
    (cond
     ((and (numberp id) (> id 0))
      (condition-case err
          (org-canvas--gradebook-canvas-scheme
           (org-canvas-api-request
            'GET (org-canvas-api-course-endpoint "grading_standards/%s" id)))
        (org-canvas-api-error
         (org-canvas--log-warning org-canvas--logger
           "[Gradebook] Could not read grading standard %s (%s); letters are left out"
           id (error-message-string err))
         nil)))
     ((org-canvas--gradebook-letters-p enrollments) org-canvas--gradebook-default-scheme))))

(defun org-canvas--gradebook-fetch-scores (rows enrollments)
  "Return every counted score of the students in ROWS, or the symbol `refused'.
The result is the plist the Groups table and the what-if read:
:pulled-at, :unposted, :weighted, :scheme, :groups, :assignments
and :students, one plist per row with :user-id, :name, :canvas (the
row's current score) and :items.  ENROLLMENTS tell whether the course
shows letters.  A refusal is logged and answered with `refused' (the
#171 rule)."
  (condition-case err
      (let* ((course (org-canvas-api-request 'GET (org-canvas-api-course-endpoint "")))
             (raw-groups (append (org-canvas-api-request-all-pages
                                  'GET (org-canvas-api-course-endpoint "assignment_groups")
                                  '(("include[]" . "assignments")))
                                 nil))
             (assignments (org-canvas--gradebook-assignment-map raw-groups))
             (by (org-canvas--gradebook-items-by-student
                  (append (org-canvas-api-request-all-pages
                           'GET (org-canvas-api-course-endpoint "students/submissions")
                           '(("student_ids[]" . "all")))
                          nil)
                  assignments org-canvas-gradebook-unposted)))
        (list :pulled-at (format-time-string "%Y-%m-%d %H:%M")
              :unposted org-canvas-gradebook-unposted
              :weighted (eq (alist-get 'apply_assignment_group_weights course) t)
              :scheme (org-canvas--gradebook-fetch-scheme course enrollments)
              :groups (org-canvas--gradebook-canvas-groups raw-groups)
              :assignments assignments
              :students (mapcar (lambda (row)
                                  (list :user-id (plist-get row :user-id)
                                        :name (plist-get row :name)
                                        :canvas (plist-get row :current)
                                        :items (gethash (plist-get row :user-id) by)))
                                rows)))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Gradebook] Could not read the scores (%s); the Groups table is left out"
       (error-message-string err))
     'refused)))

(defun org-canvas--gradebook-git (directory &rest args)
  "Return the exit status of git run with ARGS in DIRECTORY.
Nil when git cannot be run at all (not installed, say); its output
is discarded."
  (condition-case nil
      (let ((default-directory (file-name-as-directory directory)))
        (apply #'process-file "git" nil nil nil args))
    (error nil)))

(defun org-canvas--gradebook-append-ignore (directory name)
  "Add NAME as a line of the .gitignore in DIRECTORY, creating the file."
  (let ((ignore (expand-file-name ".gitignore" directory)))
    (with-temp-buffer
      (when (file-exists-p ignore)
        (insert-file-contents ignore))
      (goto-char (point-max))
      (unless (or (bobp) (bolp)) (insert "\n"))
      (insert name "\n")
      (write-region nil nil ignore nil 'silent))))

(defun org-canvas--gradebook-ignore-scores (file)
  "Keep the scores FILE out of the git work tree its directory is in.
Answer what was done: nil outside a work tree (or without git),
`ignored' when git already ignores FILE, `tracked' when git already
tracks it (logged as a warning, nothing touched: removing it from the
index is the user's call), and `added' when FILE's name was added to
the .gitignore beside it, which is created when missing."
  (let ((directory (file-name-directory file))
        (name (file-name-nondirectory file)))
    (cond
     ((not (eql 0 (org-canvas--gradebook-git directory "rev-parse" "--is-inside-work-tree")))
      nil)
     ((eql 0 (org-canvas--gradebook-git directory "ls-files" "--error-unmatch" "--" name))
      (org-canvas--log-warning org-canvas--logger
        "[Gradebook] %s is tracked by git: every student's scores travel with the repository.  Run git rm --cached on it and ignore it"
        file)
      'tracked)
     ((eql 0 (org-canvas--gradebook-git directory "check-ignore" "-q" "--" name))
      'ignored)
     (t
      (org-canvas--gradebook-append-ignore directory name)
      (org-canvas--log-info org-canvas--logger
        "[Gradebook] Added %s to %s" name (expand-file-name ".gitignore" directory))
      'added))))

(defun org-canvas--gradebook-write-scores (scores)
  "Write SCORES to `org-canvas-gradebook-scores-file' for the what-if.
The file is then kept out of git by `org-canvas--gradebook-ignore-scores'."
  (let ((coding-system-for-write 'utf-8-unix)
        (print-length nil)
        (print-level nil)
        (file (expand-file-name org-canvas-gradebook-scores-file)))
    (with-temp-file file
      (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n"
              ";; org-canvas gradebook scores: every student's score on every assignment.\n"
              ";; Written by `org-canvas-pull-gradebook'; keep it out of a course repository.\n")
      (prin1 scores (current-buffer))
      (insert "\n"))
    (org-canvas--gradebook-ignore-scores file)))

(defun org-canvas--gradebook-read-scores ()
  "Return the scores the last gradebook pull kept, or nil when there are none."
  (let ((file (expand-file-name org-canvas-gradebook-scores-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (goto-char (point-min))
        (read (current-buffer))))))

;;;; Group Scores: The Groups Table

(defun org-canvas--gradebook-student-total (student groups assignments weighted)
  "Return STUDENT's recomputed total under GROUPS, ASSIGNMENTS and WEIGHTED."
  (org-canvas--gradebook-total
   (org-canvas--gradebook-student-groups (plist-get student :items) groups assignments)
   weighted))

(defun org-canvas--gradebook-differs-p (computed canvas)
  "Return non-nil when COMPUTED and CANVAS's total differ by more than 0.1.
One number without the other differs too; two nils do not."
  (if (and (numberp computed) (numberp canvas))
      (> (abs (- computed canvas)) 0.1)
    (not (eq (null computed) (null canvas)))))

(defun org-canvas--gradebook-insert-group-row (student scores)
  "Insert the Groups table line for STUDENT from SCORES at point.
Return non-nil when the recomputed total differs from Canvas's."
  (let* ((groups (plist-get scores :groups))
         (per-group (org-canvas--gradebook-student-groups
                     (plist-get student :items) groups (plist-get scores :assignments)))
         (computed (org-canvas--gradebook-total per-group (plist-get scores :weighted)))
         (canvas (plist-get student :canvas)))
    (insert (format "| %s |%s %s | %s |\n"
                    (org-canvas--gradebook-cell-text (plist-get student :name))
                    (mapconcat (lambda (pg)
                                 (format " %s |" (org-canvas--gradebook-number
                                                  (org-canvas--gradebook-percent (cdr pg)))))
                               per-group "")
                    (org-canvas--gradebook-number computed)
                    (org-canvas--gradebook-number canvas)))
    (org-canvas--gradebook-differs-p computed canvas)))

(defun org-canvas--gradebook-insert-groups (scores)
  "Insert the Groups table for SCORES at point, and the check under it.
SCORES is the plist from `org-canvas--gradebook-fetch-scores', or
`refused', which writes a note instead.  Returns the names of the
students whose recomputed total differs from Canvas's."
  (if (eq scores 'refused)
      (progn (insert "Canvas refused the scores for this token.\n") nil)
    (let ((groups (plist-get scores :groups))
          (differ nil))
      (insert (format "| Student |%s Computed | Canvas |\n"
                      (mapconcat (lambda (g) (format " %s |" (org-canvas--gradebook-cell-text
                                                             (plist-get g :name))))
                                 groups "")))
      (insert (format "|---|%s---+---|\n" (mapconcat (lambda (_) "---+") groups "")))
      (dolist (student (plist-get scores :students))
        (when (org-canvas--gradebook-insert-group-row student scores)
          (push (plist-get student :name) differ)))
      (insert (if differ
                  (format "\n%d student(s) whose computed total differs from Canvas's by more than 0.1: %s\n"
                          (length differ) (string-join (nreverse differ) "; "))
                "\nEvery computed total is within 0.1 of Canvas's.\n"))
      differ)))

(defun org-canvas--gradebook-remove-heading (title)
  "Delete the level-1 heading TITLE and its body, when present."
  (let ((pos (org-find-exact-headline-in-buffer title nil t)))
    (when pos
      (goto-char pos)
      (delete-region pos (save-excursion (org-end-of-subtree t t) (point))))))

;;;; Pull

(defun org-canvas--gradebook-update-groups (scores)
  "Rewrite the Groups heading of the current buffer from SCORES.
Nil SCORES, `org-canvas-gradebook-groups' off, removes the heading,
so a table nobody refreshes does not sit there going stale."
  (if (null scores)
      (org-canvas--gradebook-remove-heading "Groups")
    (let (differ)
      (org-canvas--gradebook-rewrite-body
       "Groups" (lambda () (setq differ (org-canvas--gradebook-insert-groups scores))))
      (when differ
        (org-canvas--log-warning org-canvas--logger
          "[Gradebook] %d computed total(s) differ from Canvas's by more than 0.1: %s"
          (length differ) (string-join differ "; "))))))


;;;###autoload
(defun org-canvas-pull-gradebook ()
  "Pull a course-wide grade overview into gradebook.org.
One table of students with their current and final scores, missing
and late counts, last activity and the course's custom gradebook
columns; one table of sections with their means; and one table of
assignments with the class's score spread on each; with
`org-canvas-gradebook-groups', one table of each student's percentage
per assignment group, checked against Canvas's total.  Read-only, and
the tables are derived: every pull rewrites them.  The file holds
every student's scores afterwards; keep it out of a course
repository."
  (interactive)
  (org-canvas--start-operation "PULLING GRADEBOOK")
  (let* ((file (expand-file-name org-canvas-gradebook-file))
         (enrollments (org-canvas--gradebook-fetch-enrollments))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "gradebook")
    (if (null enrollments)
        (org-canvas--pull-emit-empty-file file (org-canvas--pull-label-for "gradebook"))
      (let* ((summaries (org-canvas--gradebook-fetch-summaries))
             (names (org-canvas--gradebook-fetch-section-names))
             (columns (org-canvas--gradebook-fetch-columns))
             (assignments (org-canvas--gradebook-fetch-assignments))
             (rows (org-canvas--gradebook-rows enrollments summaries))
             (scores (and org-canvas-gradebook-groups
                          (org-canvas--gradebook-fetch-scores rows enrollments))))
        (when columns
          (org-canvas--log-info org-canvas--logger
            "[Gradebook] %d custom column(s): %s" (length columns)
            (mapconcat (lambda (c) (plist-get c :title)) columns ", ")))
        (unless (file-exists-p file)
          (with-temp-file file (insert "")))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (org-canvas--gradebook-rewrite-body
           "Students" (lambda () (org-canvas--gradebook-insert-students rows names summaries columns)))
          (org-canvas--gradebook-rewrite-body
           "Sections" (lambda () (org-canvas--gradebook-insert-sections rows names summaries)))
          (org-canvas--gradebook-rewrite-body
           "Assignments" (lambda () (org-canvas--gradebook-insert-assignments assignments)))
          (org-canvas--gradebook-update-groups scores)
          (org-canvas--pull-write-file-header)
          (org-canvas--save-buffer))
        (org-canvas--pull-kill-fresh-buffer file was-fresh)
        (when (consp scores)
          (org-canvas--gradebook-write-scores scores))
        (let ((missing (cl-count-if (lambda (r) (and (numberp (plist-get r :missing))
                                                     (> (plist-get r :missing) 0)))
                                    rows)))
          (org-canvas--log-info org-canvas--logger
            "Gradebook pull complete: %d students, %d sections, %d assignments, %d with missing work"
            (length rows) (length names) (if (listp assignments) (length assignments) 0) missing)
          (message "Gradebook pull complete: %d students, %d sections, %d with missing work."
                   (length rows) (length names) missing))))))

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
  "Return whether settings.org applies the group weights.
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
  "Return a Group changes table line when GROUP's table differs from CANVAS's.
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
  "Insert the students of ROWS whose letter under SCHEME changes.
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

(provide 'org-canvas-gradebook)
;;; org-canvas-gradebook.el ends here
