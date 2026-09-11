;;; org-canvas-gradebook.el --- Pull a course-wide grade overview from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module pulls a course-wide overview of grades into
;; gradebook.org: each student's current and final score, how many
;; submissions are missing or late, and a per-section summary.  It
;; answers "who is falling behind?" without opening the Canvas gradebook
;; or pulling one grading file per assignment.  It is pull-only, and its
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
;;
;; A student is one row however many sections they sit in.  The Student
;; cell links to the roster heading in people.org when that file holds
;; the user id.  Current is the score over graded work; Final counts
;; every ungraded assignment as zero, work not yet due included, so it
;; sits low early in the term.  `org-canvas-gradebook-unposted' (default t)
;; takes the instructor's unposted scores, hidden grades included; nil
;; takes what students see.  No per-assignment columns: that is what
;; grading files are for.  Custom gradebook columns (the Notes column,
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
;; the most sensitive cell in the file.  The module writes only
;; `org-canvas-gradebook-file' itself.
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

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

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
  (let ((file (and (boundp 'org-canvas-people-file) org-canvas-people-file)))
    (when (and file (file-exists-p file))
      (let ((target (format "%s" user-id)) (heading nil))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (when (and (not heading)
                          (equal (org-entry-get (point) "USER_ID") target))
                 (setq heading (org-get-heading t t t t))))
             "USER_ID={.}" 'file)))
        heading))))

(defun org-canvas--gradebook-student-cell (row)
  "Return ROW's Student cell: a link to the roster heading when known, else the name."
  (let ((heading (org-canvas--gradebook-roster-heading (plist-get row :user-id)))
        (name (plist-get row :name)))
    (if heading
        (org-link-make-string
         (format "file:%s::*%s" (file-name-nondirectory org-canvas-people-file)
                 (replace-regexp-in-string "\\\\\\([][]\\)" "\\1" heading))
         name)
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

;;;; Pull

;;;###autoload
(defun org-canvas-pull-gradebook ()
  "Pull a course-wide grade overview into gradebook.org.
One table of students with their current and final scores, missing
and late counts, last activity and the course's custom gradebook
columns, and one table of sections with their means.  Read-only, and
the tables are derived: every pull
rewrites them.  The file holds every student's scores afterwards;
keep it out of a course repository."
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
             (rows (org-canvas--gradebook-rows enrollments summaries)))
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
          (org-canvas--pull-write-file-header)
          (org-canvas--save-buffer))
        (org-canvas--pull-kill-fresh-buffer file was-fresh)
        (let ((missing (cl-count-if (lambda (r) (and (numberp (plist-get r :missing))
                                                     (> (plist-get r :missing) 0)))
                                    rows)))
          (org-canvas--log-info org-canvas--logger
            "Gradebook pull complete: %d students, %d sections, %d with missing work"
            (length rows) (length names) missing)
          (message "Gradebook pull complete: %d students, %d sections, %d with missing work."
                   (length rows) (length names) missing))))))

(provide 'org-canvas-gradebook)
;;; org-canvas-gradebook.el ends here
