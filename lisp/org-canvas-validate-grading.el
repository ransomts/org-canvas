;;; org-canvas-validate-grading.el --- Grading checks for org-canvas  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Checks on how a course grades: grading periods and overrides,
;; scheduled posting, grading schemes, the course late window and
;; its lock/unlock dates, and assignment weighting.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-validate-issue)
(require 'org-table)
(require 'cl-lib)

;;;; 1. Grading Periods and Assignment Overrides

(defvar org-canvas-grading-periods-file
  "Declared here so the grading-period date check compiles; the variable
is defined in org-canvas-grading-periods.el, which validate must not
require (issue #245).")

(defun org-canvas--validate-override-students (text row-loc)
  "Check TEXT, the names in a `Students:' override row; return an issue or nil.
A name people.org does not hold is reported first; otherwise a name
whose heading says the student left the course, since Canvas refuses
or ignores an override for someone no longer enrolled.  Both
push-only.  ROW-LOC locates the row."
  (let* ((names (split-string text ";" t "[ \t]+"))
         (missing (cl-remove-if
                   (lambda (name)
                     (or (string-match-p "\\`#[0-9]+\\'" name)
                         (org-canvas--validate-override-lookup
                          'org-canvas-people-file name "USER_ID")))
                   names))
         (departed (and (null missing) (org-canvas--validate-departed-people names))))
    (cond
     (missing
      (org-canvas--validate-push-only
       (org-canvas--validate-make-issue
        'warning row-loc nil
        (format "Override student(s) %s not in people.org (pull people first, or write #id)"
                (org-canvas--validate-name-list missing)))))
     (departed
      (org-canvas--validate-push-only
       (org-canvas--validate-make-issue
        'warning row-loc nil
        (format "Override student(s) %s no longer enrolled according to people.org"
                (org-canvas--validate-name-list departed))))))))

(defun org-canvas--validate-override-target (target row-loc)
  "Check TARGET, the first cell of an override row, and return an issue or nil.
A section link is checked elsewhere; `Group: <title>' must name a
group heading in groups.org and `Students: <name>; <name>' person
headings in people.org, unless written as a literal #id (issue #224).
Push-only: a row that resolves to nothing is skipped by the sync and
touches nothing on Canvas.  ROW-LOC locates the row."
  (cond
   ((string-match "\\`Group:[ \t]*\\(.+?\\)[ \t]*\\'" target)
    (let ((title (match-string 1 target)))
      (unless (or (string-match-p "\\`#[0-9]+\\'" title)
                  (org-canvas--validate-override-lookup
                   'org-canvas-groups-file title "CANVAS_ID"))
        (org-canvas--validate-push-only
         (org-canvas--validate-make-issue
          'warning row-loc nil
          (format "Override group '%s' is not in groups.org (pull groups first, or write #id)"
                  title))))))
   ((string-match "\\`Students?:[ \t]*\\(.+?\\)[ \t]*\\'" target)
    (org-canvas--validate-override-students (match-string 1 target) row-loc))
   ((not (string-match "\\[\\[file:" target))
    (org-canvas--validate-make-issue
     'warning row-loc nil
     (format "Override row '%s' is not a section link, Group: or Students:" target)))))

(defun org-canvas--validate-override-rows (file heading)
  "Check each data row in an override table for who it is for.
The first cell must be a section link, `Group: <title>' or
`Students: <name>; <name>' (see `org-canvas--validate-override-target').
FILE and HEADING identify the location.  Point must be at the first data row."
  (let ((issues nil))
    (while (looking-at "^|\\([^-]\\)")
      (let* ((row-line (line-number-at-pos))
             (row-loc (list :file file :line row-line :heading heading))
             (line-text (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
             (fields (split-string line-text "|" t "[ \t]+"))
             (target (and fields (car fields) (string-trim (car fields)))))
        (when (and target (not (string-empty-p target)))
          (when-let* ((issue (org-canvas--validate-override-target target row-loc)))
            (push issue issues))))
      (forward-line 1))
    (nreverse issues)))

(defun org-canvas--validate-external-tool (loc)
  "Check that an LTI-backed assignment at point names a tool, and vice versa.
An `external_tool' submission type with no EXTERNAL_TOOL_URL and no
EXTERNAL_TOOL_ID creates a Canvas assignment that launches nothing —
it looks fine in the assignment list and fails only when a student
opens it.  The mirror case (a tool declared but never selected as the
submission type) is the more common typo: Canvas silently ignores the
attributes and takes submissions itself.  LOC is a
\(:file :line :heading) plist."
  (let* ((submission (or (org-entry-get (point) "SUBMISSION") ""))
         (external-p (member "external_tool" (split-string submission "," t "[ \t]+")))
         (url (org-entry-get (point) "EXTERNAL_TOOL_URL"))
         (id (org-entry-get (point) "EXTERNAL_TOOL_ID"))
         (named (or url id)))
    (cond
     ((and external-p (not named))
      (list (org-canvas--validate-make-issue
             'warning loc "EXTERNAL_TOOL_URL"
             (concat "SUBMISSION: external_tool without EXTERNAL_TOOL_URL or "
                     "EXTERNAL_TOOL_ID — Canvas will create an assignment that "
                     "launches nothing.  M-x org-canvas-list-external-tools "
                     "shows the launch URL of every tool installed in the course"))))
     ((and named (not external-p))
      (list (org-canvas--validate-make-issue
             'warning loc "EXTERNAL_TOOL_URL"
             (concat "EXTERNAL_TOOL_URL/EXTERNAL_TOOL_ID is ignored unless "
                     "SUBMISSION includes external_tool")))))))

(defun org-canvas--validate-grading-period-bounds ()
  "Return the (START . END) ISO pairs of the pulled grading periods, or nil.
Read from `org-canvas-grading-periods-file' when the variable is bound
and the file exists with content; a period whose dates do not parse is
left out."
  (let ((file (and (boundp 'org-canvas-grading-periods-file)
                   org-canvas-grading-periods-file)))
    (when (and file (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 0))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (delq nil
              (org-map-entries
               (lambda ()
                 (let ((start (org-canvas--validate-safe-parse-timestamp
                               (org-entry-get (point) "START_DATE")))
                       (end (org-canvas--validate-safe-parse-timestamp
                             (org-entry-get (point) "END_DATE"))))
                   (and start end (cons start end))))
               "LEVEL=1" 'file))))))

(defun org-canvas--validate-due-in-grading-period (loc)
  "Warn when the DUE_AT at point falls outside every pulled grading period.
LOC is a (:file :line :heading) plist.  Says nothing when the heading
has no DUE_AT, when no grading periods have been pulled, or when the
date sits inside a period, bounds included."
  (let* ((raw (org-entry-get (point) "DUE_AT"))
         (due (org-canvas--validate-safe-parse-timestamp raw))
         (periods (and due (org-canvas--validate-grading-period-bounds))))
    (when (and periods
               (not (cl-some (lambda (period)
                               (and (not (string< due (car period)))
                                    (not (string< (cdr period) due))))
                             periods)))
      (list (org-canvas--validate-make-issue
             'warning loc "DUE_AT"
             (format "DUE_AT %s falls outside every grading period in %s"
                     raw (file-name-nondirectory org-canvas-grading-periods-file)))))))

;;;; 2. Scheduled Posting (issue #352)

(defun org-canvas--validate-post-schedule-problem (grades comments policy)
  "Return what is wrong with a posting schedule, or nil.
GRADES and COMMENTS are the raw POST_GRADES_AT and POST_COMMENTS_AT,
POLICY the raw POST_POLICY.  Canvas keeps a schedule only with both
times, grades no earlier than comments, under manual posting."
  (let ((grades-iso (org-canvas--validate-safe-parse-timestamp grades))
        (comments-iso (org-canvas--validate-safe-parse-timestamp comments)))
    (cond
     ((not (and grades comments))
      (format "%s without %s: Canvas schedules a posting only with both times"
              (if grades "POST_GRADES_AT" "POST_COMMENTS_AT")
              (if grades "POST_COMMENTS_AT" "POST_GRADES_AT")))
     ((and grades-iso comments-iso
           (time-less-p (date-to-time grades-iso) (date-to-time comments-iso)))
      (format "POST_GRADES_AT %s is before POST_COMMENTS_AT %s: Canvas posts grades no earlier than comments"
              grades comments))
     ((equal policy "automatic")
      "a posting schedule needs manual posting, and POST_POLICY is automatic"))))

(defun org-canvas--validate-post-schedule (loc)
  "Check the posting schedule of the assignment heading at point.
LOC is a (:file :line :heading) plist.  An error when Canvas would
refuse or drop the schedule; nothing when the heading has none.  A
time that does not parse is reported by the timestamp check."
  (let ((grades (org-entry-get (point) "POST_GRADES_AT"))
        (comments (org-entry-get (point) "POST_COMMENTS_AT")))
    (when-let* (((or grades comments))
                (problem (org-canvas--validate-post-schedule-problem
                          grades comments
                          (org-entry-get (point) "POST_POLICY"))))
      (list (org-canvas--validate-make-issue
             'error loc (if grades "POST_GRADES_AT" "POST_COMMENTS_AT")
             problem)))))

;;;; 3. Grading Schemes
;;
;; The table helpers live in the grading-schemes module, which is always
;; loaded with org-canvas; validate names them rather than requiring a
;; feature module (the dependency rule in
;; documentation/architecture-guide.md).

(declare-function org-canvas--grading-scheme-find-table "org-canvas-grading-schemes" (end))

(declare-function org-canvas--grading-scheme-table-rows "org-canvas-grading-schemes" (table))

(declare-function org-canvas--grading-scheme-cutoff-number "org-canvas-grading-schemes" (cell))

(defun org-canvas--validate-grading-scheme-structure (loc)
  "Check the scheme table under the heading at point.
LOC is a (:file :line :heading) plist.  The table must exist, every
cutoff must be a number no higher than 100, the rows must descend, and
the last row must be 0, or Canvas has no grade for the lowest scores."
  (let* ((end (save-excursion (org-end-of-subtree t) (point)))
         (table (org-canvas--grading-scheme-find-table end))
         (rows (and table (org-canvas--grading-scheme-table-rows table)))
         (issue (lambda (message)
                  (list (org-canvas--validate-make-issue 'error loc "scheme" message)))))
    (cond
     ((null table)
      (funcall issue "Grading scheme has no #+NAME: scheme table (| Grade | Cutoff |)"))
     ((null rows)
      (funcall issue "Grading scheme table has no grade rows"))
     (t
      (let ((cutoffs (mapcar (lambda (row)
                               (cons (car row)
                                     (org-canvas--grading-scheme-cutoff-number (cdr row))))
                             rows))
            (issues nil))
        (dolist (row cutoffs)
          (cond
           ((null (cdr row))
            (push (format "Cutoff for '%s' is not a number" (car row)) issues))
           ((> (cdr row) 100)
            (push (format "Cutoff for '%s' is above 100" (car row)) issues))))
        (unless issues
          (cl-loop for (a b) on cutoffs
                   while b
                   unless (> (cdr a) (cdr b))
                   do (push (format "Cutoffs must descend: '%s' (%s) is not above '%s' (%s)"
                                    (car a) (cdr a) (car b) (cdr b))
                            issues)
                   and return nil)
          (unless (zerop (cdr (car (last cutoffs))))
            (push (format "The last row, '%s', should have a cutoff of 0 so every score has a grade"
                          (car (car (last cutoffs))))
                  issues)))
        (mapcan (lambda (message) (funcall issue message)) (nreverse issues)))))))

(defun org-canvas--validate-grading-standard-ids (schemes-file)
  "Warn where settings.org or assignments.org name a scheme SCHEMES-FILE lacks.
A GRADING_STANDARD_ID that matches no CANVAS_ID in SCHEMES-FILE is
either stale or never pulled; the warning sits on the heading that
names it.  An id of 0 names no scheme and is passed over.  A scheme
Canvas deleted is not known, and a heading Canvas deleted is not
checked (issue #402)."
  (let ((known (with-current-buffer (org-canvas--find-file-noselect schemes-file)
                 (save-excursion
                   (delq nil (org-map-entries
                              (lambda ()
                                (unless (org-canvas--sync-deleted-stamp)
                                  (org-entry-get (point) "CANVAS_ID")))
                              "LEVEL=1" 'file)))))
        (issues nil))
    (dolist (var '(org-canvas-settings-file org-canvas-assignments-file))
      (let ((file (and (boundp var) (symbol-value var))))
        (when (and file (file-exists-p file))
          (with-current-buffer (org-canvas--find-file-noselect file)
            (save-excursion
              (org-map-entries
               (lambda ()
                 (let ((id (org-entry-get (point) "GRADING_STANDARD_ID")))
                   (when (and id (not (member id known))
                              (not (org-canvas--sync-deleted-stamp))
                              (not (member id '("0" ""))))
                     (push (org-canvas--validate-make-issue
                            'warning
                            (list :file file :line (line-number-at-pos)
                                  :heading (org-get-heading t t t t))
                            "GRADING_STANDARD_ID"
                            (format "GRADING_STANDARD_ID %s matches no CANVAS_ID in %s; pull the schemes, or check the id"
                                    id (file-name-nondirectory schemes-file)))
                           issues))))
               "LEVEL=1" 'file))))))
    (nreverse issues)))

(defun org-canvas--validate-assignment-structure (loc)
  "Check the LTI tool, grading period, posting schedule and override table.
LOC is a (:file :line :heading) plist."
  (let ((late-window (org-canvas--validate-late-window-defaults)))
    (nconc (org-canvas--validate-external-tool loc)
           (org-canvas--validate-due-in-grading-period loc)
           (org-canvas--validate-post-schedule loc)
           (org-canvas--validate-heading-late-window loc late-window)
           (org-canvas--validate-override-table loc late-window))))

(cl-defun org-canvas--validate-override-table (loc &optional late-window)
  "Check the `#+NAME: overrides' table under the assignment at point.
Each row is checked for who it is for, for a section link's id, for
the order of its dates and, given LATE-WINDOW (the assignment's dates
when the course's late-window rule applies), against that rule.  LOC
is a (:file :line :heading) plist.  Nil without a table."
  (let ((end (save-excursion (org-end-of-subtree t) (point)))
        (dates (org-canvas--validate-heading-dates)))
    (save-excursion
      (unless (re-search-forward "^#\\+NAME: overrides" end t)
        (cl-return-from org-canvas--validate-override-table nil))
      (forward-line 1)
      (unless (looking-at "^|")
        (cl-return-from org-canvas--validate-override-table nil))
      (let ((header-pos (point))
            (file (plist-get loc :file))
            (heading (plist-get loc :heading)))
        (forward-line 1)
        (when (looking-at "^|-")
          (forward-line 1))
        (let* ((rows-pos (point))
               (row-issues (org-canvas--validate-override-rows file heading))
               (id-issues (progn (goto-char rows-pos)
                                 (org-canvas--validate-override-section-ids
                                  file heading))))
          (goto-char header-pos)
          (nconc row-issues id-issues
                 (org-canvas--validate-override-row-dates
                  loc late-window dates)))))))

(defun org-canvas--validate-heading-dates ()
  "Return the assignment's (:due :unlock :lock) at point, raw Org timestamps."
  (list :due (org-entry-get (point) "DUE_AT")
        :unlock (org-entry-get (point) "UNLOCK_AT")
        :lock (org-entry-get (point) "LOCK_AT")))

(declare-function org-canvas--override-date-columns "org-canvas-sections" (header))

(declare-function org-canvas--override-parse-timestamp-cell "org-canvas-sections" (cell))

(declare-function org-canvas--override-accept-column "org-canvas-sections" (header))

(defun org-canvas--validate-override-cell (row column)
  "Return (VALUE . ISO) for the date cell at COLUMN of override ROW.
VALUE is the trimmed text, nil when the cell is empty or absent; ISO
is what the override push reads it as
\(`org-canvas--override-parse-timestamp-cell', in the course zone), nil
when it reads nothing."
  (let ((text (string-trim (or (and column (nth column row)) ""))))
    (unless (string-empty-p text)
      (cons text (ignore-errors
                   (org-canvas--override-parse-timestamp-cell text))))))

(defun org-canvas--validate-override-row-label (row)
  "Return the first cell of override ROW, a link shown as its description."
  (org-link-display-format (string-trim (or (nth 0 row) ""))))

(defun org-canvas--validate-override-row-date (row column inherited)
  "Return the (NAME VALUE ISO INHERITED) date of override ROW at COLUMN.
The row's own date when the push reads one there, else INHERITED, the
assignment's raw timestamp, marked as inherited: Canvas reads an
override that omits a date as not overriding it, so the assignment's
applies (issue #538)."
  (let ((cell (org-canvas--validate-override-cell row column)))
    (if (cdr cell)
        (list (car cell) (cdr cell) nil)
      (list (and inherited (format "%s (the assignment's)" inherited))
            (org-canvas--validate-safe-parse-timestamp inherited)
            t))))

(defun org-canvas--validate-override-row-date-order (row columns row-loc
                                                         &optional dates)
  "Check that ROW's Unlock At, Due At and Lock At run in that order.
COLUMNS are the due, unlock and lock indexes
\(`org-canvas--override-date-columns').  A date the row leaves empty,
or the push cannot read, is the assignment's, from DATES
\(:due :unlock :lock); a pair is reported only when the row sets at
least one of its two dates, since the assignment's own order is its
heading's to report (issues #534, #538).  ROW-LOC locates the row."
  (org-canvas--validate-date-sequence
   (cl-mapcar (lambda (name column key)
                (cons name (org-canvas--validate-override-row-date
                            row column (plist-get dates key))))
              '("Unlock At" "Due At" "Lock At")
              (list (nth 1 columns) (nth 0 columns) (nth 2 columns))
              '(:unlock :due :lock))
   row-loc
   (format "Override row '%s': "
           (org-canvas--validate-override-row-label row))))

(defun org-canvas--validate-override-row-late-window (row columns row-loc defaults)
  "Apply the course's late-window rule to override ROW, or return nil.
COLUMNS are the due, unlock and lock indexes.  DEFAULTS is the
assignment's own (:due :unlock :lock), from
`org-canvas--validate-late-window-defaults', nil when the rule does
not apply.  A row is checked only when it sets a due or a lock date of
its own; a date it leaves empty, or one the push cannot read, is the
assignment's, as on Canvas, where an override not setting a date
keeps the assignment's (issue #536).  ROW-LOC locates the row."
  (when defaults
    (let* ((dates (mapcar (lambda (column)
                            (let ((cell (org-canvas--validate-override-cell row column)))
                              (and (cdr cell) (car cell))))
                          columns))
           (due (nth 0 dates))
           (lock (nth 2 dates)))
      (when (or due lock)
        (org-canvas--validate-late-window
         (or due (plist-get defaults :due))
         (or lock (plist-get defaults :lock))
         (or (nth 1 dates) (plist-get defaults :unlock))
         row-loc
         (format "Override row '%s': "
                 (org-canvas--validate-override-row-label row)))))))

(defun org-canvas--validate-override-row-accepts (issues row column row-loc)
  "Apply the Accept cell of override ROW, at COLUMN, to ISSUES.
The cell holds accepts separated by `;', each a check id and the
reason (issue #538).  ROW-LOC locates the row."
  (let ((accepts (and column
                      (split-string (or (nth column row) "") ";" t "[ \t]+"))))
    (if accepts
        (org-canvas--validate-apply-accepts
         issues accepts row-loc
         (cons nil (format "Override row '%s': "
                           (org-canvas--validate-override-row-label row))))
      issues)))

(defun org-canvas--validate-override-row-dates (loc &optional defaults dates)
  "Check the dates of each row of the override table at point.
Point is on the table's first line, its header, which names the date
columns as the push reads them.  LOC is the assignment's
\(:file :line :heading) plist; each issue carries its row's line.
DEFAULTS, when the course's late-window rule applies to the
assignment, are its own dates (see
`org-canvas--validate-override-row-late-window'); DATES are its dates
whether or not it does, which a row's date order inherits.  A row's
Accept cell applies to its findings (issue #538)."
  (let* ((start (line-number-at-pos))
         (table (org-table-to-lisp))
         (columns (org-canvas--override-date-columns (car table)))
         (accept (org-canvas--override-accept-column (car table))))
    (cl-loop for row in (cdr table)
             for line from (1+ start)
             for row-loc = (list :file (plist-get loc :file) :line line
                                 :heading (plist-get loc :heading))
             unless (eq row 'hline)
             nconc (org-canvas--validate-override-row-accepts
                    (nconc (org-canvas--validate-override-row-date-order
                            row columns row-loc dates)
                           (org-canvas--validate-override-row-late-window
                            row columns row-loc defaults))
                    row accept row-loc))))

;;;; 4. A Course's Late Window (issue #536)

(defcustom org-canvas-validate-lock-after-due-hours nil
  "Hours after its due date an online column must lock, or nil for no rule.
A number, or a list of the offsets allowed, 0 meaning a lock at the
due date: \\='(0 48) accepts either.  `org-canvas-validate' warns
about a dated online column (SUBMISSION names an online type) with no
lock date, or one locking at another offset, on the assignment heading
and on each override row.  Hours are counted by the clock, as the
dates are written.  A course rule: set it in the course's
`org-canvas-course-config-file', beside its syllabus late policy."
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Hours")
                 (repeat :tag "Any of these hours" number))
  :group 'org-canvas)

(defcustom org-canvas-validate-require-unlock nil
  "Non-nil when every dated online column must carry an unlock date.
`org-canvas-validate' warns about an assignment heading or override
row with a due date and no unlock date.  A course rule: set it in the
course's `org-canvas-course-config-file'."
  :type 'boolean
  :group 'org-canvas)

(defconst org-canvas--validate-online-submission-types
  '("online_upload" "online_url" "online_text_entry" "media_recording")
  "Submission types Canvas groups as Online, which the late window covers.")

(defun org-canvas--validate-late-window-defaults ()
  "Return the assignment's dates for the late-window rule, or nil.
Nil unless a rule is set and the assignment at point takes an online
submission; otherwise its (:due :unlock :lock), raw Org timestamps."
  (when (and (or org-canvas-validate-lock-after-due-hours
                 org-canvas-validate-require-unlock)
             (cl-intersection
              (split-string (or (org-entry-get (point) "SUBMISSION") "")
                            "," t "[ \t]+")
              org-canvas--validate-online-submission-types
              :test #'string=))
    (org-canvas--validate-heading-dates)))

(defun org-canvas--validate-wall-seconds (timestamp)
  "Return Org TIMESTAMP as seconds, read as written in no zone."
  (let ((decoded (org-parse-time-string timestamp)))
    (setf (nth 8 decoded) 0)
    (float-time (encode-time decoded))))

(defun org-canvas--validate-wall-hours (from to)
  "Return the hours from Org timestamp FROM to TO by the clock, or nil.
Both are read as written, in no zone, so a lock two days after a due
date is 48 hours across a daylight-saving change too.  Nil when either
does not parse."
  (ignore-errors
    (/ (- (org-canvas--validate-wall-seconds to)
          (org-canvas--validate-wall-seconds from))
       3600.0)))

(defun org-canvas--validate-format-hours (hours)
  "Return HOURS, a list of numbers, as text for a message.
Each is rounded to a tenth of an hour."
  (concat (mapconcat (lambda (h)
                       (org-canvas--validate-format-weight
                        (/ (round (* 10 h)) 10.0)))
                     hours " or ")
          " hour(s)"))

(defun org-canvas--validate-lock-offset-text (hours)
  "Return where a lock HOURS after the due date falls, for a message."
  (if (< hours 0)
      (format "locks %s before the due date"
              (org-canvas--validate-format-hours (list (- hours))))
    (format "locks %s after the due date"
            (org-canvas--validate-format-hours (list hours)))))

(defun org-canvas--validate-lock-window (due lock loc prefix)
  "Check LOCK against DUE for `org-canvas-validate-lock-after-due-hours'.
DUE and LOCK are raw Org timestamps, LOCK nil when the column has
none.  LOC locates the column and PREFIX starts the message.  Returns
an issue or nil, check `lock-window'; a lock date that does not parse
is the timestamp check's to report."
  (when-let* ((allowed (if (numberp org-canvas-validate-lock-after-due-hours)
                           (list org-canvas-validate-lock-after-due-hours)
                         org-canvas-validate-lock-after-due-hours)))
    (let ((hours (and lock (org-canvas--validate-wall-hours due lock))))
      (org-canvas--validate-checked
       'lock-window
       (cond
        ((null lock)
         (org-canvas--validate-make-issue
          'warning loc "LOCK_AT"
          (format "%sLock window missing: due %s with no lock date; the course locks online columns %s after the due date"
                  prefix due (org-canvas--validate-format-hours allowed))))
        ((and hours
              (not (cl-some (lambda (h) (= (round (* 60 h)) (round (* 60 hours))))
                            allowed)))
         (org-canvas--validate-make-issue
          'warning loc "LOCK_AT"
          (format "%sLock window mismatch: %s (due %s, lock %s); the course allows %s"
                  prefix (org-canvas--validate-lock-offset-text hours)
                  due lock (org-canvas--validate-format-hours allowed)))))))))

(defun org-canvas--validate-late-window (due lock unlock loc &optional prefix)
  "Check one online column against the course's late-window rule.
DUE, LOCK and UNLOCK are its raw Org timestamps, an absent one nil.
A column with no due date that parses has no window and is passed
over.  The findings are warnings (`org-canvas-validate-lock-after-due-hours',
`org-canvas-validate-require-unlock'), the lock window's push-only
once the column is past due, since a column that has closed is a
record and not a push to come; a closed column is not asked for an
unlock date at all, since it opened as it ran.  LOC locates the
column and PREFIX starts each message."
  (when-let* ((due-iso (org-canvas--validate-safe-parse-timestamp due)))
    (let* ((prefix (or prefix ""))
           (past (string< due-iso (org-canvas-current-iso8601-timestamp)))
           (issues (delq nil
                         (list
                          (org-canvas--validate-lock-window due lock loc prefix)
                          (when (and org-canvas-validate-require-unlock (null unlock)
                                     (not past))
                            (org-canvas--validate-checked
                             'require-unlock
                             (org-canvas--validate-make-issue
                              'warning loc "UNLOCK_AT"
                              (format "%sUnlock date missing: the course requires one on every online column"
                                      prefix))))))))
      (if past
          (mapcar #'org-canvas--validate-push-only issues)
        issues))))

(defun org-canvas--validate-heading-late-window (loc defaults)
  "Check the assignment heading at LOC against the course's late window.
DEFAULTS are its dates from `org-canvas--validate-late-window-defaults',
nil when the rule does not apply."
  (when defaults
    (org-canvas--validate-late-window
     (plist-get defaults :due) (plist-get defaults :lock)
     (plist-get defaults :unlock) loc)))

;;;; 5. Assignment Weighting

(defun org-canvas--count-assignments-in-group (group-name assignments-file)
  "Count assignments in GROUP-NAME by scanning ASSIGNMENTS-FILE.
A heading Canvas deleted is not counted (issue #402)."
  (if (and assignments-file (file-exists-p assignments-file))
      (let ((count 0))
        (with-current-buffer (org-canvas--find-file-noselect assignments-file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (let ((group-prop (org-entry-get (point) "GROUP")))
                 (when (and group-prop
                            (not (org-canvas--sync-deleted-stamp))
                            (string-match (regexp-quote group-name) group-prop))
                   (setq count (1+ count)))))
             "LEVEL=1" 'file)))
        count)
    0))

(defun org-canvas--validate-drop-rules (loc)
  "Check that drop rules don't exceed assignment count for this group.
LOC is a (:file :line :heading) plist."
  (let ((drop-lowest (org-entry-get (point) "DROP_LOWEST"))
        (drop-highest (org-entry-get (point) "DROP_HIGHEST")))
    (when (or drop-lowest drop-highest)
      (let* ((group-name (plist-get loc :heading))
             (drop-low (if drop-lowest (string-to-number drop-lowest) 0))
             (drop-high (if drop-highest (string-to-number drop-highest) 0))
             (total-drops (+ drop-low drop-high))
             (assignments-file (and (boundp 'org-canvas-assignments-file)
                                    (expand-file-name
                                     (symbol-value 'org-canvas-assignments-file))))
             (assignment-count (org-canvas--count-assignments-in-group
                                group-name assignments-file)))
        (when (and (> total-drops 0) (>= total-drops assignment-count))
          (list (org-canvas--validate-make-issue
                 'warning loc "DROP_LOWEST"
                 (format "Drop rules (%d) >= assignment count (%d) for group '%s'"
                         total-drops assignment-count group-name))))))))

(defconst org-canvas--validate-weight-sum-tolerance 0.01
  "Slack allowed when checking that group weights sum to 100.
Weights are read as floats, so an exact comparison would reject
values like 33.33 + 33.33 + 33.34 that Canvas itself accepts.")

(defun org-canvas--validate-weight-sum (file)
  "Warn when the WEIGHT properties in FILE do not sum to 100.
Only meaningful when the course applies group weights, but the check is
offline by design (see issue #37): weights that sum to less than 100
silently inflate every grade, and more than 100 deflates them, with no
symptom until final grades come out.  Returns a list of issues.

Groups with no WEIGHT are not counted, nor are groups Canvas deleted
\(issue #402); a file with no weighted groups
at all produces no issue, since an unweighted course is a normal
configuration rather than a mistake."
  (let ((total 0)
        (count 0)
        (first-line nil))
    ;; Read FILE rather than whatever buffer happens to be current: the
    ;; hook is handed a path, so it should not depend on its caller
    ;; having visited it.
    (with-current-buffer (org-canvas--find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let ((weight (org-entry-get (point) "WEIGHT")))
             (when (and weight (not (org-canvas--sync-deleted-stamp)))
               (unless first-line (setq first-line (line-number-at-pos)))
               (setq count (1+ count))
               (setq total (+ total (string-to-number weight))))))
         "LEVEL=2+WEIGHT={.}" 'file)))
    (when (and (> count 0)
               (> (abs (- total 100)) org-canvas--validate-weight-sum-tolerance))
      (list (org-canvas--validate-make-issue
             'warning
             (list :file file :line (or first-line 1) :heading "Assignment Groups")
             "WEIGHT"
             (format "Group weights sum to %s, not 100 — every grade is %s if the course applies group weights"
                     (org-canvas--validate-format-weight total)
                     (if (< total 100) "inflated" "deflated")))))))

(defun org-canvas--validate-format-weight (weight)
  "Format WEIGHT without a trailing .0 for whole numbers."
  (if (= weight (floor weight))
      (format "%d" (floor weight))
    (format "%s" weight)))

(defun org-canvas--validate-override-section-ids (file heading)
  "Check that override table section links have CANVAS_IDs.
FILE and HEADING identify the location.  Point must be at the first data row."
  (let ((issues nil))
    (while (looking-at "^|\\([^-]\\)")
      (let* ((row-line (line-number-at-pos))
             (row-loc (list :file file :line row-line :heading heading))
             (line-text (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
             (fields (split-string line-text "|" t "[ \t]+")))
        (when (and fields (car fields))
          (let ((section-ref (string-trim (car fields))))
            (when (string-match "\\[\\[file:" section-ref)
              (let ((resolved (org-canvas--resolve-link-property
                               section-ref "CANVAS_ID" file)))
                (unless resolved
                  (push (org-canvas--validate-make-issue
                         'warning row-loc nil
                         (format "Override section link target has no CANVAS_ID (pull sections first)")
                         t)
                        issues)))))))
      (forward-line 1))
    (nreverse issues)))

(provide 'org-canvas-validate-grading)
;;; org-canvas-validate-grading.el ends here
