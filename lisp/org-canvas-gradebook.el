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
;; The code is in three files: this one holds the pull and what it writes;
;; `org-canvas-gradebook-scores.el' recomputes the per-group scores and
;; keeps the file the what-if reads; `org-canvas-gradebook-report.el'
;; prints one student's standing (#542) and the what-if (#452).
;;
;; FILE STRUCTURE
;; ==============
;; In gradebook.org:
;;   * Students   one table, sorted by name: Student, Sections, Current,
;;                Final, Override, Missing, Late, Last activity, then one
;;                column per custom gradebook column the course has
;;   * Sections   one table: Section, Students, Mean current, Mean final,
;;                Missing
;;   * Assignments one table, in Canvas order: Assignment, Due, Points,
;;                Min, Q1, Median, Q3, Max, Missing, Late
;;   * Final grade overrides  one table, one row per student Canvas holds
;;                an override for: Student, Override.  Written by the pull,
;;                edited by hand, pushed by
;;                `org-canvas-sync-final-grade-overrides' (issue #556);
;;                the table is the whole set, as the accommodations table
;;                is.  "No final grade overrides." when none
;;   * Groups     with `org-canvas-gradebook-groups' only: Student, one
;;                column per assignment group (its percentage after the
;;                drop rules), Computed, Canvas, and a line naming the
;;                students whose two totals differ by more than 0.1
;;   * My grades  instead of all the above, when the caller's enrolment in
;;                the course is a student's (issue #488): one table,
;;                by due date: Assignment, Due, Points, Score, Grade,
;;                Status, Late, Missing; then the enrolment's current
;;                and final totals, then the submission comments
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
;; `org-canvas-gradebook-student' (issue #542) reports one student
;; without writing anything: every column grouped by assignment group
;; with its weight and drop rules, the columns past due but still open,
;; and the best possible total if every still-open column scored full
;; marks, drop rules kept.  It reads Canvas live rather than
;; gradebook.org, so `scripts/org-canvas student NAME' answers "can I
;; still pass?" without a REST script.
;;
;; PERSONAL DATA
;; =============
;; Scores are more sensitive than names.  Keep gradebook.org out of
;; anything shared.  Only the name, the numbers and the custom column
;; text are written, never an identifier; a Notes column is free text
;; about a student, so it is the most sensitive cell in the file.  In a
;; git work tree the pull keeps it out of a course repository itself
;; (issue #549): it adds the file's name to the .gitignore beside it
;; unless git ignores the file already, and warns when git tracks it,
;; since a .gitignore does not untrack.  The module writes
;; `org-canvas-gradebook-file' and, with `org-canvas-gradebook-groups',
;; `org-canvas-gradebook-scores-file': every student's score on every
;; assignment with their user ids, kept for
;; `org-canvas-gradebook-what-if'.  Both get the same guard through
;; `org-canvas--student-data-ignore'.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/enrollments?type[]=StudentEnrollment
;;       each enrollment carries a `grades' object (current_score,
;;       final_score, their unposted_* twins, letter grades when the
;;       course has a scheme) and, when the caller may manage grades and
;;       the student has one, `override_score' — the Final Grade
;;       Override column, the percentage the registrar receives in place
;;       of the computed total.  Paginated by bookmark.  The same read
;;       answers the Final grade overrides table, so the override side
;;       costs no extra request (issue #556).
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
;;
;;   The override push (issue #556) sends nothing over REST: Canvas
;;   publishes no API for writing the Final Grade Override column, and
;;   the bulk route in canvas-lms
;;   (PUT /courses/:id/update_final_grade_overrides) is undocumented,
;;   gated on the Import Override Scores flag and absent from the
;;   vendored spec, so the push rides the gradebook's own GraphQL
;;   mutation instead:
;;   POST /api/graphql  setOverrideScore(input: {enrollmentId,
;;       overrideScore}) — one enrollment id per student (Canvas updates
;;       every active enrollment the student holds in the course), the
;;       score a percentage, null clearing the override.  Requires
;;       manage grades, and the course's Final Grade Override setting
;;       on; a grading-period override is not written, only the course
;;       total the registrar receives.  Checked against the Canvas
;;       schema by the GraphQL contract test (issue #269).
;;
;;   A CSV export of the gradebook has no API route at all
;;   (coverage.org): the web UI's export is a browser action, and the
;;   pull is org-canvas's answer — gradebook.org holds every student's
;;   scores.  No code path here downloads one.

;;   For one student's standing (issue #542), in place of all the above:
;;   GET /courses/:id/enrollments?type[]=StudentEnrollment
;;       the names to resolve NAME against, and the student's own totals
;;   GET /courses/:id                    weights, the grading standard id
;;   GET /courses/:id/assignment_groups?include[]=assignments
;;       weights, drop rules and points, as the Groups read takes them
;;   GET /courses/:id/students/submissions?student_ids[]=UID&include[]=assignment
;;       every column's score, state, flags and dates.  The dates are
;;       the column's own, not the student's override.
;;   GET /courses/:id/sections           the header's section names

;;   For a student (issue #488), two reads in place of all the above:
;;   GET /courses/:id?include[]=total_scores
;;       read first on every pull: enrollments[] names the caller's own
;;       roles (type student, teacher, ta, designer, observer); a
;;       student one carries computed_{current,final}_{score,grade}.
;;       A caller holding a student enrolment and no teacher, TA or
;;       designer one gets My grades.  Groups reuses this object.
;;   GET /courses/:id/students/submissions
;;       ?include[]=assignment&include[]=submission_comments
;;       without student_ids, the caller's own submissions.  Paginated.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(defvar org-canvas-assignment-groups-file)
(defvar org-canvas-settings-file)

;; The scores half requires this file back; requiring it here would make
;; a loop of it.  `org-canvas.el' loads both.
(declare-function org-canvas--gradebook-fetch-scores "org-canvas-gradebook-scores"
                  (rows enrollments &optional course))
(declare-function org-canvas--gradebook-write-scores "org-canvas-gradebook-scores"
                  (scores))
(declare-function org-canvas--gradebook-insert-groups "org-canvas-gradebook-scores"
                  (scores))
(declare-function org-canvas--gradebook-median "org-canvas-gradebook-scores"
                  (values))

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

(defcustom org-canvas-gradebook-hurdles nil
  "Alist of assignment group name to the percent that group requires.
Each pair is (GROUP-NAME . PERCENT): the group's percentage after its
drop rules must reach PERCENT for the student to pass — a recitation
hurdle.  `org-canvas-gradebook-student' prints the group's percentage
standing and at best beside it, and names a group the course lacks."
  :type '(alist :key-type (string :tag "Assignment group")
                :value-type (number :tag "Percent"))
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

(defconst org-canvas--gradebook-enrollment-params
  '(("type[]" . "StudentEnrollment")
    ("state[]" . "active") ("state[]" . "inactive") ("state[]" . "completed"))
  "Query parameters of the enrollments read.
The students in any state the roster keeps, so the tables and the
override column see the class the gradebook does.  Pinned by the
read-parameter contract test (issue #273).")

(defun org-canvas--gradebook-fetch-enrollments ()
  "Return the course's student enrollments, active, inactive and completed."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint "enrollments")
           org-canvas--gradebook-enrollment-params)
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

(defun org-canvas--gradebook-user-name (user user-id)
  "Return USER's roster name: sortable, plain, or a placeholder for USER-ID."
  (or (alist-get 'sortable_name user)
      (alist-get 'name user)
      (format "User %s" user-id)))

(defun org-canvas--gradebook-override (enrollment)
  "Return ENROLLMENT's final grade override, a percentage, or nil.
Nil when Canvas holds none for the student, when the course has the
Final Grade Override column off, or when the caller cannot see grades."
  (org-canvas--gradebook-stat (alist-get 'grades enrollment) 'override_score))

(defun org-canvas--gradebook-fold-scores (row grades enrollment)
  "Fold into ROW the scores GRADES and ENROLLMENT carry.
Current, final and the override each take the first of the student's
enrollments to carry one, so a student in two sections is one row."
  (unless (plist-get row :current)
    (plist-put row :current (org-canvas--gradebook-score grades 'current)))
  (unless (plist-get row :final)
    (plist-put row :final (org-canvas--gradebook-score grades 'final)))
  (unless (plist-get row :override)
    (plist-put row :override (org-canvas--gradebook-override enrollment))))

(defun org-canvas--gradebook-rows (enrollments summaries)
  "Fold ENROLLMENTS into one plist per student, sorted by name.
Each plist has :user-id, :name, :section-ids, :current, :final,
:override, :missing, :late and :last-activity.  SUMMARIES is the alist
from `org-canvas--gradebook-fetch-summaries', or `refused', in which
case :missing and :late are nil."
  (let ((rows (make-hash-table :test 'eql)))
    (dolist (e enrollments)
      (let* ((uid (alist-get 'user_id e))
             (user (alist-get 'user e))
             (grades (alist-get 'grades e))
             (row (or (gethash uid rows)
                      (puthash uid (list :user-id uid
                                         :name (org-canvas--gradebook-user-name user uid)
                                         :section-ids nil
                                         :current nil :final nil :override nil
                                         :missing nil :late nil
                                         :last-activity nil)
                               rows))))
        (let ((sid (alist-get 'course_section_id e)))
          (when (and sid (not (memq sid (plist-get row :section-ids))))
            (plist-put row :section-ids (append (plist-get row :section-ids) (list sid)))))
        (org-canvas--gradebook-fold-scores row grades e)
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
  (org-canvas--heading-link file heading text))

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
  (insert (format "| Student | Sections | Current | Final | Override | Missing | Late | Last activity |%s\n"
                  (mapconcat (lambda (column)
                               (format " %s |" (org-canvas--gradebook-cell-text
                                                (plist-get column :title))))
                             columns "")))
  (insert (format "|---+---+---+---+---+---+---+---|%s\n"
                  (mapconcat (lambda (_) "---|") columns "")))
  (dolist (row rows)
    (insert (format "| %s | %s | %s | %s | %s | %s | %s | %s |%s\n"
                    (org-canvas--gradebook-student-cell row)
                    (org-canvas--gradebook-sections-cell row names)
                    (org-canvas--gradebook-number (plist-get row :current))
                    (org-canvas--gradebook-number (plist-get row :final))
                    (org-canvas--gradebook-number (plist-get row :override))
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

(defun org-canvas--gradebook-remove-heading (title)
  "Delete the level-1 heading TITLE and its body, when present."
  (let ((pos (org-find-exact-headline-in-buffer title nil t)))
    (when pos
      (goto-char pos)
      (delete-region pos (save-excursion (org-end-of-subtree t t) (point))))))

;;;; My Grades: A Student's Enrolment
;;
;; A student token reads the roster with every classmate's name and no
;; scores, and is refused the analytics, so the teacher's tables would
;; be empty or notes (issue #488).  What a student wants is their own
;; standing, which Canvas serves: their submissions, each with its
;; assignment and comments, and the totals on their enrolment.

(defconst org-canvas--gradebook-course-params '(("include[]" . "total_scores"))
  "Query parameters of the course read that tells the caller's role.
With total_scores a student enrolment carries the computed scores.")

(defconst org-canvas--gradebook-self-params
  '(("include[]" . "assignment") ("include[]" . "submission_comments"))
  "Query parameters of a student's read of their own submissions.
Without student_ids Canvas answers the calling user's submissions.")

(defun org-canvas--gradebook-fetch-course ()
  "Return the course with the caller's enrolments and their totals, or nil.
A refusal is logged and answered with nil, which the pull reads as a
teacher's course: the tables it wrote before issue #488."
  (condition-case err
      (org-canvas-api-request 'GET (org-canvas-api-course-endpoint "")
                              :params org-canvas--gradebook-course-params)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Gradebook] Could not read the course (%s); writing the teacher's tables"
       (error-message-string err))
     nil)))

(defun org-canvas--gradebook-student-enrollment (course)
  "Return the caller's student enrolment in COURSE, or nil.
Nil when the caller also teaches, assists or designs in COURSE: they
get the teacher's tables."
  (let* ((raw (alist-get 'enrollments course))
         (enrollments (and (sequencep raw) (append raw nil))))
    (unless (cl-some (lambda (e) (member (alist-get 'type e) '("teacher" "ta" "designer")))
                     enrollments)
      (cl-find-if (lambda (e) (equal (alist-get 'type e) "student")) enrollments))))

(defun org-canvas--gradebook-fetch-self ()
  "Return the caller's submissions, with assignments, sorted by due date.
Undated assignments come last, then by name."
  (let ((submissions (append (org-canvas-api-request-all-pages
                              'GET (org-canvas-api-course-endpoint "students/submissions")
                              org-canvas--gradebook-self-params)
                             nil)))
    (sort submissions #'org-canvas--gradebook-self-before-p)))

(defun org-canvas--gradebook-self-key (submission)
  "Return SUBMISSION's sort key: (DUE . NAME), DUE nil when undated."
  (let ((a (alist-get 'assignment submission)))
    (cons (org-canvas--alist-get-non-null 'due_at a)
          (or (alist-get 'name a) ""))))

(defun org-canvas--gradebook-self-before-p (a b)
  "Return non-nil to put submission A before B."
  (let ((ka (org-canvas--gradebook-self-key a))
        (kb (org-canvas--gradebook-self-key b)))
    (cond ((equal (car ka) (car kb)) (string< (cdr ka) (cdr kb)))
          ((null (car kb)) t)
          ((null (car ka)) nil)
          (t (string< (car ka) (car kb))))))

(defun org-canvas--gradebook-flag (value)
  "Render the Canvas boolean VALUE for a cell: yes, or no."
  (if (eq value t) "yes" "no"))

(defun org-canvas--gradebook-self-title (submission)
  "Return the assignment cell of SUBMISSION, a link when assignments.org has it."
  (org-canvas--gradebook-assignment-cell
   `((title . ,(alist-get 'name (alist-get 'assignment submission)))
     (assignment_id . ,(alist-get 'assignment_id submission)))))

(defun org-canvas--gradebook-insert-self-row (submission)
  "Insert the My grades table line for SUBMISSION at point."
  (let ((a (alist-get 'assignment submission))
        (grade (org-canvas--alist-get-non-null 'grade submission)))
    (insert (format "| %s | %s | %s | %s | %s | %s | %s | %s |\n"
                    (org-canvas--gradebook-self-title submission)
                    (or (org-canvas--iso8601-to-org-timestamp (alist-get 'due_at a)) "-")
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat a 'points_possible))
                    (org-canvas--gradebook-number (org-canvas--gradebook-stat submission 'score))
                    (if grade (org-canvas--gradebook-cell-text (format "%s" grade)) "-")
                    (if (eq (alist-get 'excused submission) t) "excused"
                      (or (alist-get 'workflow_state submission) "-"))
                    (org-canvas--gradebook-flag (alist-get 'late submission))
                    (org-canvas--gradebook-flag (alist-get 'missing submission))))))

(defun org-canvas--gradebook-total-text (enrollment key)
  "Return ENROLLMENT's computed KEY total as text: score, then its letter.
KEY is `current' or `final'; - when Canvas hides the total."
  (let ((score (org-canvas--gradebook-stat
                enrollment (intern (format "computed_%s_score" key))))
        (letter (org-canvas--alist-get-non-null
                 (intern (format "computed_%s_grade" key)) enrollment)))
    (concat (org-canvas--gradebook-number score)
            (if (stringp letter) (format " (%s)" letter) ""))))

(defun org-canvas--gradebook-insert-self-comments (submissions)
  "Insert one list item per comment on SUBMISSIONS at point, under a line.
Nothing when no submission has a comment."
  (let ((items nil))
    (dolist (s submissions)
      (dolist (c (append (alist-get 'submission_comments s) nil))
        (push (format "- %s, %s %s: %s\n"
                      (org-canvas--gradebook-cell-text
                       (alist-get 'name (alist-get 'assignment s)))
                      (or (alist-get 'author_name c) "Unknown")
                      (or (org-canvas--iso8601-to-org-timestamp (alist-get 'created_at c)) "")
                      (org-canvas--gradebook-cell-text (alist-get 'comment c)))
              items)))
    (when items
      (insert "\nComments:\n")
      (apply #'insert (nreverse items)))))

(defun org-canvas--gradebook-insert-self (submissions enrollment)
  "Insert the My grades table for SUBMISSIONS at point.
The table, then ENROLLMENT's current and final totals, then the
submission comments."
  (if (null submissions)
      (insert "No assignments\n")
    (insert "| Assignment | Due | Points | Score | Grade | Status | Late | Missing |\n")
    (insert "|---+---+---+---+---+---+---+---|\n")
    (mapc #'org-canvas--gradebook-insert-self-row submissions))
  (insert (format "\nCurrent: %s; final: %s\n"
                  (org-canvas--gradebook-total-text enrollment 'current)
                  (org-canvas--gradebook-total-text enrollment 'final)))
  (org-canvas--gradebook-insert-self-comments submissions))

(defun org-canvas--gradebook-pull-self (file enrollment)
  "Write the student ENROLLMENT's own grades to FILE; return a one-row list.
The row has :user-id, :current, :final, :assignments, :missing and
:self t.  The teacher's headings are removed: for a student they would
hold an unscored roster and refusal notes (issue #488)."
  (let* ((was-fresh (org-canvas--pull-was-fresh-p file))
         (submissions (org-canvas--gradebook-fetch-self))
         (missing (cl-count-if (lambda (s) (eq (alist-get 'missing s) t)) submissions)))
    (org-canvas--pull-confirm-unsaved file "gradebook")
    ;; The caller's own grades are student data too (issue #549).
    (org-canvas--student-data-ignore file)
    (org-canvas--create-file file)
    (with-current-buffer (org-canvas--find-file-noselect file)
      (dolist (title '("Students" "Sections" "Assignments"
                       "Final grade overrides" "Groups"))
        (org-canvas--gradebook-remove-heading title))
      (org-canvas--gradebook-rewrite-body
       "My grades" (lambda () (org-canvas--gradebook-insert-self submissions enrollment)))
      (org-canvas--pull-write-file-header)
      (org-canvas--save-buffer))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger
      "Gradebook pull complete: my grades, %d assignments, %d missing"
      (length submissions) missing)
    (message "Gradebook pull complete: my grades, %d assignments, %d missing."
             (length submissions) missing)
    (list (list :user-id (alist-get 'user_id enrollment)
                :current (org-canvas--gradebook-stat enrollment 'computed_current_score)
                :final (org-canvas--gradebook-stat enrollment 'computed_final_score)
                :assignments (length submissions) :missing missing :self t))))

;;;; Final Grade Overrides
;;
;; The grade the registrar receives is not the computed total: when the
;; course has the Final Grade Override column on, a percentage there
;; supersedes both.  The enrollments read the pull already makes
;; carries it (`grades.override_score', only when set and the caller
;; may manage grades), so the Students table shows it beside Current
;; and Final and the pull writes it under `* Final grade overrides' as
;; a table the instructor edits and `org-canvas-sync-final-grade-
;; overrides' pushes.  The table is the whole set, as the quiz
;; accommodations table is: a row whose percentage differs is sent, a
;; blank cell clears the student's override, and a student with an
;; override on Canvas and no row is cleared too.  A missing table —
;; no `#+NAME: final-grade-overrides' under the heading — sends
;; nothing, but an emptied one clears everyone, so delete the table
;; rather than emptying it when Canvas should be left alone.  The
;; table is rewritten by every pull, so hand edits do not survive one:
;; edit, sync, then pull to see what stuck.
;;
;; The push rides Canvas's own gradebook mutation, setOverrideScore
;; (see API NOTES above), one enrollment per student; Canvas updates
;; every active enrollment the student holds in the course, and it
;; records a grade change in the audit log the way the web UI does.
;; Writing final grades is grading, so the write allowlist names
;; `submissions' for it (issue #550), and a refused enrollments read
;; aborts the sync before anything is sent: with the class unread
;; there is no telling what a push would clear.

(defconst org-canvas--gradebook-overrides-table-regexp
  "^#\\+NAME:[ \t]+final-grade-overrides[ \t]*$"
  "Match the `#+NAME:' line that opens a final grade overrides table.")

(defun org-canvas--gradebook-first-enrollments (enrollments)
  "Return an alist of user id to one of ENROLLMENTS, per student.
The first the read lists for each: its id addresses the override
mutation, which updates every enrollment the student holds in the
course, and its grades carry the override Canvas holds."
  (let ((map nil))
    (dolist (e enrollments)
      (let ((uid (alist-get 'user_id e)))
        (unless (assq uid map)
          (push (cons uid e) map))))
    (nreverse map)))

(defun org-canvas--gradebook-override-rows (enrollments)
  "Return one plist per student ENROLLMENTS hold an override for.
Each plist has :user-id, :name and :override, the percentage; a
student without one is left out, so the table holds only what the
next push would reconcile."
  (delq nil
        (mapcar (lambda (pair)
                  (let ((e (cdr pair))
                        (o (org-canvas--gradebook-override (cdr pair))))
                    (when o
                      (list :user-id (car pair)
                            :name (org-canvas--gradebook-user-name
                                   (alist-get 'user e) (car pair))
                            :override o))))
                (org-canvas--gradebook-first-enrollments enrollments))))

(defun org-canvas--gradebook-override-student-cell (row)
  "Return ROW's Student cell: the person's heading title, or #id."
  (or (org-canvas--heading-title-by-property
       (and (boundp 'org-canvas-people-file) org-canvas-people-file)
       "USER_ID" (plist-get row :user-id) "LEVEL=2")
      (format "#%s" (plist-get row :user-id))))

(defun org-canvas--gradebook-insert-overrides (rows)
  "Insert the final grade overrides table for ROWS at point.
A line saying there are none when ROWS is empty; the table aligns
itself once written."
  (if (null rows)
      (insert "No final grade overrides.\n")
    (let ((start (point)))
      (insert "#+NAME: final-grade-overrides\n| Student | Override |\n|---+---|\n")
      (dolist (row rows)
        (insert (format "| %s | %s |\n"
                        (org-canvas--gradebook-override-student-cell row)
                        (org-canvas--gradebook-number (plist-get row :override)))))
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (when (org-at-table-p) (org-table-align))))))

(defun org-canvas--gradebook-update-overrides (rows)
  "Rewrite the Final grade overrides heading of the current buffer from ROWS."
  (org-canvas--gradebook-rewrite-body
   "Final grade overrides"
   (lambda () (org-canvas--gradebook-insert-overrides rows))))

;;;; Final Grade Overrides: Reading the Table

(defun org-canvas--gradebook-overrides-find-table ()
  "Return the overrides table under the Final grade overrides heading.
What `org-table-to-lisp' gives, or nil when the current buffer's
heading or its table is missing."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Final grade overrides[ \t]*$" nil t)
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (when (re-search-forward org-canvas--gradebook-overrides-table-regexp end t)
          (forward-line 1)
          (when (looking-at-p org-table-line-regexp)
            (org-table-to-lisp)))))))

(defun org-canvas--gradebook-overrides-column (header)
  "Return the index of the Override column in HEADER.
A header naming it by its title, case aside, reads by title; one
naming none falls back to position 1.  The Student column is always
first, as the pull writes it."
  (let ((titles (mapcar (lambda (cell) (downcase (string-trim cell))) header)))
    (or (cl-position "override" titles :test #'string=)
        1)))

(defun org-canvas--gradebook-overrides-resolve-student (text)
  "Resolve TEXT, a person's heading title or #id, to a USER_ID string."
  (if (string-match "\\`#\\([0-9]+\\)\\'" text)
      (match-string 1 text)
    (org-canvas--heading-property-by-title
     (and (boundp 'org-canvas-people-file) org-canvas-people-file)
     text "USER_ID" "LEVEL=2")))

(defun org-canvas--gradebook-override-cell (cell)
  "Return CELL as an override percentage, or nil when it is blank.
A cell that is not a number signals `org-canvas-config-error', since
a misread cell would set the wrong grade."
  (let ((text (string-trim (or cell ""))))
    (cond
     ((string-empty-p text) nil)
     ((string-match-p "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\'" text) (string-to-number text))
     (t (org-canvas--signal 'org-canvas-config-error
          "Override cell '%s' is not a percentage" text)))))

(defun org-canvas--gradebook-overrides-parse-row (row column)
  "Parse one table ROW with its Override COLUMN into (USER-ID . OVERRIDE).
USER-ID is a string; OVERRIDE a percentage or nil.  Nil, with one
warning, when the student resolves to nothing."
  (let* ((student (string-trim (or (nth 0 row) "")))
         (user-id (and (not (string-empty-p student))
                       (org-canvas--gradebook-overrides-resolve-student student))))
    (if (not user-id)
        (progn
          (org-canvas--log-warning org-canvas--logger
            "[Gradebook] Could not resolve student '%s' (pull people first, or write #id)"
            student)
          nil)
      (cons user-id (org-canvas--gradebook-override-cell (nth column row))))))

(defun org-canvas--gradebook-overrides-parse-table (table)
  "Parse the overrides TABLE (from `org-table-to-lisp') into rows.
Each row is (USER-ID . OVERRIDE); a row whose student resolves to
nothing is skipped with one warning."
  (let ((column (org-canvas--gradebook-overrides-column (car table)))
        (rows nil))
    (dolist (row (cdr table))
      (unless (eq row 'hline)
        (when-let* ((parsed (org-canvas--gradebook-overrides-parse-row row column)))
          (push parsed rows))))
    (nreverse rows)))

;;;; Final Grade Overrides: Pushing

(defconst org-canvas--gradebook-override-mutation
  "mutation ($enrollmentId: ID!, $overrideScore: Float) { setOverrideScore(input: {enrollmentId: $enrollmentId, overrideScore: $overrideScore}) { grades { overrideScore } errors { attribute message } } }"
  "The GraphQL mutation that sets one student's final grade override.
The score is a percentage; null clears it, so the student's grade goes
back to the computed total.  Checked against the Canvas schema by the
GraphQL contract test (issue #269), which names it by this symbol.")

(defun org-canvas--gradebook-overrides-fetch ()
  "Return the course's student enrollments for the override sync.
A refusal aborts the sync before anything is sent: with the class
unread there is no telling what a push would clear."
  (condition-case err
      (org-canvas--gradebook-fetch-enrollments)
    (org-canvas-api-error
     (org-canvas--signal 'org-canvas-api-error
       "Could not read the enrollments, so no override was sent: %s"
       (error-message-string err)))))

(defun org-canvas--gradebook-override-same-p (row current)
  "Return non-nil when ROW's override equals CURRENT, Canvas's.
Both nil agree; one number without the other differs."
  (if (numberp row)
      (and (numberp current) (= row current))
    (null current)))

(defun org-canvas--gradebook-overrides-send (enrollment override)
  "Send ENROLLMENT's student the OVERRIDE, or preview it under a dry run.
OVERRIDE nil clears the override.  Return non-nil when the request
was sent (or previewed), nil when it failed, with one error logged
and the other students still sent."
  (let* ((name (org-canvas--gradebook-user-name
                (alist-get 'user enrollment) (alist-get 'user_id enrollment)))
         (label (format "%s %s's final grade override to %s"
                        (if override "set" "clear") name
                        (or (org-canvas--gradebook-number override) "none"))))
    (condition-case err
        (let ((data (org-canvas--graphql-mutate
                     label org-canvas--gradebook-override-mutation
                     (list (cons 'enrollmentId
                                 (format "%s" (alist-get 'id enrollment)))
                           (cons 'overrideScore override)))))
          (if (org-canvas--dry-run-response-p data)
              t
            (org-canvas--gradebook-override-stored data)))
      (error
       (org-canvas--log-error org-canvas--logger
         "[Gradebook] Could not %s: %s" label (error-message-string err))
       nil))))

(defun org-canvas--gradebook-override-stored (data)
  "Return what Canvas stored, from DATA, the override mutation's reply.
The percentage, nil when Canvas holds none; errors the payload
carries signal `org-canvas-api-error' naming them."
  (let* ((payload (alist-get 'setOverrideScore data))
         (errors (org-canvas--alist-get-non-null 'errors payload))
         (grades (org-canvas--alist-get-non-null 'grades payload)))
    (when (and errors (> (length errors) 0))
      (org-canvas--signal 'org-canvas-api-error
        "setOverrideScore: %s" (org-canvas--graphql-errors-message errors)))
    (and (consp grades) (org-canvas--gradebook-stat grades 'overrideScore))))

(defun org-canvas--gradebook-overrides-sync-rows (rows by-user)
  "Send each of ROWS that differs from what BY-USER's enrollments hold.
BY-USER maps user id to enrollment, one per student.  Return
\(SET . FAILED): the rows sent and the rows that failed, a row whose
student has no enrollment or whose request failed.  A row matching
Canvas is sent nothing."
  (let ((set 0) (failed 0))
    (dolist (row rows)
      (let ((enrollment (alist-get (string-to-number (car row)) by-user)))
        (cond
         ((null enrollment)
          (org-canvas--log-error org-canvas--logger
            "[Gradebook] Student %s has no enrollment in this course; their override was not set"
            (car row))
          (cl-incf failed))
         ((org-canvas--gradebook-override-same-p
           (cdr row) (org-canvas--gradebook-override enrollment))
          nil)
         ((org-canvas--gradebook-overrides-send enrollment (cdr row)) (cl-incf set))
         (t (cl-incf failed)))))
    (cons set failed)))

(defun org-canvas--gradebook-overrides-clear (rows by-user)
  "Clear every override in BY-USER for a student with no row in ROWS.
Return (CLEARED . FAILED).  The table is the whole set: a student
with an override on Canvas and no row goes back to the computed total."
  (let ((cleared 0) (failed 0))
    (dolist (pair by-user)
      (when (and (org-canvas--gradebook-override (cdr pair))
                 (not (assoc (number-to-string (car pair)) rows)))
        (if (org-canvas--gradebook-overrides-send (cdr pair) nil)
            (cl-incf cleared)
          (cl-incf failed))))
    (cons cleared failed)))

(defun org-canvas--gradebook-overrides-report (set cleared failed)
  "Log and message the override sync's totals, and record the stats.
SET, CLEARED and FAILED are the counts: the rows sent, the overrides
cleared and the requests that failed."
  (org-canvas--log-info org-canvas--logger "========================================")
  (org-canvas--log-info org-canvas--logger ">>> FINAL GRADE OVERRIDE SYNC COMPLETE")
  (org-canvas--log-info org-canvas--logger "Set: %d | Cleared: %d | Failed: %d"
                        set cleared failed)
  (org-canvas--log-info org-canvas--logger "========================================")
  (org-canvas--sync-record-feature-stats "Final Grade Overrides"
    (if org-canvas--dry-run
        (list :dry-run (+ set cleared) :fail failed)
      (list :success (+ set cleared) :fail failed)))
  (message "Final grade override sync: %d set, %d cleared, %d failed.%s"
           set cleared failed
           (if org-canvas--dry-run " (dry run — nothing sent)" "")))

;;;###autoload
(defun org-canvas-sync-final-grade-overrides (&optional dry-run)
  "Sync the final grade overrides of gradebook.org with Canvas.
Reads the `#+NAME: final-grade-overrides' table under the Final grade
overrides heading, as the file stands unsaved edits included, and
reconciles it with the overrides Canvas holds: the table is the whole
set — a row whose percentage differs is sent, a blank cell clears the
student's override, and a student with an override on Canvas and no
row is cleared too.  Nothing is sent without a table, under a dry run
nothing at all, and a refused enrollments read aborts before anything
goes.  With a prefix argument, DRY-RUN: every change is logged and
nothing is sent.  The override is the course total, the grade the
registrar receives when the course has Final Grade Override on; the
push rides Canvas's own setOverrideScore mutation (issue #556)."
  ;; `(interactive "p")' rather than a sexp: edebug bails on the sexp
  ;; form and takes undercover with it (see `org-canvas-version').
  (interactive "p")
  (let ((org-canvas--dry-run (or org-canvas--dry-run dry-run)))
    (org-canvas--gradebook-overrides-command)))

(defun org-canvas--gradebook-overrides-command ()
  "Read the overrides table and reconcile it; the entry point's body.
Split so the dry run can be bound around the whole run, the way
`org-canvas-sync-dry-run' binds it around `org-canvas-sync'."
  (org-canvas--check-write-feature 'submissions "pushing final grade overrides")
  (let ((file (expand-file-name org-canvas-gradebook-file)))
    (unless (file-exists-p file)
      (org-canvas--signal 'org-canvas-config-error
        "No %s: pull the gradebook first (org-canvas-pull-gradebook)"
        (file-name-nondirectory file)))
    (org-canvas-clear-log)
    (display-buffer (get-buffer-create org-canvas--log-buffer-name))
    (let ((table (with-current-buffer (org-canvas--find-file-noselect file)
                   (org-canvas--gradebook-overrides-find-table))))
      (if (null table)
          (message "No final grade overrides table in %s; nothing to sync"
                   (file-name-nondirectory file))
        (let* ((rows (org-canvas--gradebook-overrides-parse-table table))
               (by-user (org-canvas--gradebook-first-enrollments
                         (org-canvas--gradebook-overrides-fetch)))
               (set-failed (org-canvas--gradebook-overrides-sync-rows rows by-user))
               (clear-failed (org-canvas--gradebook-overrides-clear rows by-user)))
          (org-canvas--gradebook-overrides-report
           (car set-failed) (car clear-failed)
           (+ (cdr set-failed) (cdr clear-failed))))))))

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
One table of students with their current and final scores, the final
grade override where one is set, missing and late counts, last
activity and the course's custom gradebook columns; one table of
sections with their means; one table of assignments with the class's
score spread on each; and the final grade overrides table, written
from the same read, for `org-canvas-sync-final-grade-overrides'; with
`org-canvas-gradebook-groups', one table of each student's percentage
per assignment group, checked against Canvas's total.  Read-only, and
the tables are derived: every pull rewrites them.  The file holds
every student's scores afterwards; the pull keeps it out of a course
repository (issue #549).
When the caller's enrolment is a student's, the file holds their own
grades instead, under My grades (see `org-canvas--gradebook-pull-self').
Return the student rows (see `org-canvas--gradebook-rows'), nil for
a course with no students, or the student's one row."
  (interactive)
  (org-canvas--start-operation "PULLING GRADEBOOK")
  (let* ((file (expand-file-name org-canvas-gradebook-file))
         (course (org-canvas--gradebook-fetch-course))
         (student (org-canvas--gradebook-student-enrollment course)))
    (if student
        (org-canvas--gradebook-pull-self file student)
      (org-canvas--gradebook-pull-class file course))))

(defun org-canvas--gradebook-pull-class (file course)
  "Write the teacher's tables to FILE and return the student rows.
COURSE is the course object already read, or nil; the Groups read
reuses it.  A My grades heading left by a student pull is removed."
  (let* ((enrollments (org-canvas--gradebook-fetch-enrollments))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "gradebook")
    ;; Scores are more sensitive than names: the pull keeps the file
    ;; out of git itself rather than by a comment (issue #549).
    (org-canvas--student-data-ignore file)
    (if (null enrollments)
        (progn
          (org-canvas--pull-emit-empty-file file (org-canvas--pull-label-for "gradebook"))
          nil)
      (let* ((summaries (org-canvas--gradebook-fetch-summaries))
             (names (org-canvas--gradebook-fetch-section-names))
             (columns (org-canvas--gradebook-fetch-columns))
             (assignments (org-canvas--gradebook-fetch-assignments))
             (rows (org-canvas--gradebook-rows enrollments summaries))
             (scores (and org-canvas-gradebook-groups
                          (org-canvas--gradebook-fetch-scores rows enrollments course))))
        (when columns
          (org-canvas--log-info org-canvas--logger
            "[Gradebook] %d custom column(s): %s" (length columns)
            (mapconcat (lambda (c) (plist-get c :title)) columns ", ")))
        (org-canvas--create-file file)
        (with-current-buffer (org-canvas--find-file-noselect file)
          (org-canvas--gradebook-remove-heading "My grades")
          (org-canvas--gradebook-rewrite-body
           "Students" (lambda () (org-canvas--gradebook-insert-students rows names summaries columns)))
          (org-canvas--gradebook-rewrite-body
           "Sections" (lambda () (org-canvas--gradebook-insert-sections rows names summaries)))
          (org-canvas--gradebook-rewrite-body
           "Assignments" (lambda () (org-canvas--gradebook-insert-assignments assignments)))
          (org-canvas--gradebook-update-overrides
           (org-canvas--gradebook-override-rows enrollments))
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
                   (length rows) (length names) missing))
        rows))))

;;;; Summary

(defun org-canvas--gradebook-sections-table (&optional title)
  "Return the Sections table of `org-canvas-gradebook-file' as text, or nil.
With TITLE, the body of that level-1 heading instead.  Nil when the
file or the heading is missing."
  (let ((file (expand-file-name org-canvas-gradebook-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward
               (format "^\\* +%s[ \t]*$" (regexp-quote (or title "Sections"))) nil t)
          (let ((start (point))
                (end (if (re-search-forward "^\\* " nil t)
                         (match-beginning 0)
                       (point-max))))
            (string-trim (buffer-substring-no-properties start end))))))))

(defun org-canvas--gradebook-spread (rows key)
  "Return \"mean M, median D\" of the scores under KEY in ROWS."
  (let ((scores (mapcar (lambda (r) (plist-get r key)) rows)))
    (format "mean %s, median %s"
            (org-canvas--gradebook-number (org-canvas--gradebook-mean scores))
            (org-canvas--gradebook-number (org-canvas--gradebook-median scores)))))

(defun org-canvas-gradebook-summary-text (rows)
  "Return the class standing after a gradebook pull, as text to print.
The Sections table of `org-canvas-gradebook-file', then one line with
the course's mean and median current and final scores over ROWS, the
student rows `org-canvas-pull-gradebook' returns.  This is what the
shell's gradebook command prints (issue #451).  For a student's one
row, the My grades heading and a line of their own totals (issue #488)."
  (if (plist-get (car rows) :self)
      (org-canvas--gradebook-self-summary-text (car rows))
    (let ((table (org-canvas--gradebook-sections-table)))
      (concat (if table (concat table "\n\n") "")
              (format "Course: %d student%s; current %s; final %s\n"
                      (length rows) (if (= (length rows) 1) "" "s")
                      (org-canvas--gradebook-spread rows :current)
                      (org-canvas--gradebook-spread rows :final))))))

(defun org-canvas--gradebook-self-summary-text (row)
  "Return a student's standing after a gradebook pull, from their ROW."
  (let ((body (org-canvas--gradebook-sections-table "My grades"))
        (n (plist-get row :assignments)))
    (concat (if body (concat body "\n\n") "")
            (format "Me: %d assignment%s, %d missing; current %s; final %s\n"
                    n (if (= n 1) "" "s") (plist-get row :missing)
                    (org-canvas--gradebook-number (plist-get row :current))
                    (org-canvas--gradebook-number (plist-get row :final))))))

(provide 'org-canvas-gradebook)
;;; org-canvas-gradebook.el ends here
