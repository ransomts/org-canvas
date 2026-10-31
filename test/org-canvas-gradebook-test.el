;;; org-canvas-gradebook-test.el --- Buttercup tests for the gradebook overview -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-gradebook': student enrollments and the course
;; analytics folded into the three tables of gradebook.org.  Every request
;; is answered by a fake keyed on the URL; nothing here reaches the
;; network (Hard Rule 2), and the log is never read from the shared
;; buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: the tier list lives in org-canvas.el and the
;; roster link needs `org-canvas-people-file' to be special.
(require 'org-canvas)

(defvar test-gradebook--enrollments nil
  "Student enrollments the fake API lists for the course.")
(defvar test-gradebook--sections nil
  "Sections the fake API lists for the course.")
(defvar test-gradebook--summaries nil
  "Student summaries the fake API lists, or `refuse' to answer with a 403.")
(defvar test-gradebook--columns nil
  "Custom gradebook columns the fake API lists, or `refuse' to answer with a 403.
Each column may carry a `data' entry: the rows its data endpoint answers.")
(defvar test-gradebook--columns-params nil
  "The parameters the last request for the custom columns carried.")
(defvar test-gradebook--assignments nil
  "Assignment analytics rows the fake API lists, or `refuse' to answer with a 403.")

(defvar test-gradebook--groups nil
  "Assignment groups, with their assignments, the fake API lists.")
(defvar test-gradebook--submissions nil
  "Submissions the fake API lists for every student, or `refuse'.")
(defvar test-gradebook--course nil
  "The course object the fake single request answers.")
(defvar test-gradebook--standard nil
  "The grading standard the fake single request answers, or `refuse'.")

(defvar test-gradebook--course-reads nil
  "The parameters of each course read, latest first.")
(defvar test-gradebook--submissions-params nil
  "The parameters the last request for the submissions carried.")

(defun test-gradebook--request (_method url &rest args)
  "Answer a single request for URL: the course or its grading standard.
ARGS are the keyword arguments; a course read's :params are recorded."
  (cond
   ((string-match "/grading_standards/" url)
    (if (eq test-gradebook--standard 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--standard))
   ((string-match "/courses/[0-9]+/\\'" url)
    (push (plist-get args :params) test-gradebook--course-reads)
    (if (eq test-gradebook--course 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--course))
   (t (error "Unexpected request: %s" url))))

(defun test-gradebook--api (_method url &optional _params)
  "Answer URL from the fake tables, as the paginated helper would."
  (cond
   ((string-match "/enrollments" url)
    (if (eq test-gradebook--enrollments 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--enrollments))
   ((string-match "/sections\\'" url) test-gradebook--sections)
   ((string-match "/analytics/student_summaries" url)
    (if (eq test-gradebook--summaries 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--summaries))
   ((string-match "/analytics/assignments" url)
    (if (eq test-gradebook--assignments 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--assignments))
   ((string-match "/custom_gradebook_columns/\\([0-9]+\\)/data" url)
    (let ((id (string-to-number (match-string 1 url))))
      (alist-get 'data (cl-find-if (lambda (c) (eql (alist-get 'id c) id))
                                   test-gradebook--columns))))
   ((string-match "/assignment_groups" url) test-gradebook--groups)
   ((string-match "/students/submissions" url)
    (setq test-gradebook--submissions-params _params)
    (if (eq test-gradebook--submissions 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--submissions))
   ((string-match "/custom_gradebook_columns\\'" url)
    (setq test-gradebook--columns-params _params)
    (if (eq test-gradebook--columns 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--columns))
   (t (error "Unexpected request: %s" url))))

(defun test-gradebook--enrollment (uid name section current final &rest extra)
  "A student enrollment for UID named NAME in SECTION scoring CURRENT and FINAL.
EXTRA pairs come first, so an explicit value shadows the default.
Positional is right: fifty-odd call sites read as the row they build —
student, section, two scores — and a bundle would add a field name per
argument at every one without changing what is passed (issue #599)."
  (append extra
          `((id . ,(+ 1000 uid (* 10 section)))
            (user_id . ,uid) (type . "StudentEnrollment")
            (course_section_id . ,section) (enrollment_state . "active")
            (user . ((id . ,uid) (name . ,name) (sortable_name . ,name)))
            (grades . ((current_score . ,current) (final_score . ,final)
                       (unposted_current_score . ,current)
                       (unposted_final_score . ,final))))))

(defun test-gradebook--column (id title position &rest data)
  "A custom gradebook column ID titled TITLE at POSITION.
DATA are (UID . CONTENT) pairs its data endpoint answers."
  `((id . ,id) (title . ,title) (position . ,position) (hidden . :json-false)
    (teacher_notes . :json-false) (read_only . :json-false)
    (data . ,(mapcar (lambda (d) `((user_id . ,(car d)) (content . ,(cdr d)))) data))))

(defun test-gradebook--assignment (id title due points &rest stats)
  "An assignment analytics row ID titled TITLE, due DUE, out of POINTS.
STATS are (KEY . VALUE) pairs for the score fields and the tardiness
counts; a score field not given is null, a count not given is 0."
  `((assignment_id . ,id) (title . ,title) (due_at . ,due) (muted . :json-false)
    (points_possible . ,points) (non_digital_submission . :json-false)
    (max_score . ,(alist-get 'max_score stats :null))
    (min_score . ,(alist-get 'min_score stats :null))
    (first_quartile . ,(alist-get 'first_quartile stats :null))
    (median . ,(alist-get 'median stats :null))
    (third_quartile . ,(alist-get 'third_quartile stats :null))
    (tardiness_breakdown . ((missing . ,(alist-get 'missing stats 0))
                            (late . ,(alist-get 'late stats 0))
                            (on_time . 5) (floating . 0) (total . 5)))))

(defun test-gradebook--summary (uid missing late)
  "A student summary for UID with MISSING and LATE submissions."
  `((id . ,uid) (page_views . 3)
    (tardiness_breakdown . ((missing . ,missing) (late . ,late)
                            (on_time . 5) (floating . 0) (total . ,(+ missing late 5))))))

(defun test-gradebook--fold-summaries (summaries)
  "Fold SUMMARIES as the fetch does: an alist of user id to tardiness."
  (mapcar (lambda (s) (cons (alist-get 'id s) (alist-get 'tardiness_breakdown s)))
          summaries))

(defun test-gradebook--with-override (enrollment override)
  "Return ENROLLMENT with OVERRIDE, a percentage, in its grades."
  (let ((copy (copy-alist enrollment)))
    (setcdr (assq 'grades copy)
            (cons (cons 'override_score override) (cdr (assq 'grades copy))))
    copy))

(defmacro test-gradebook--with-course (enrollments sections summaries &rest body)
  "Run BODY with the fake API serving ENROLLMENTS, SECTIONS and SUMMARIES.
The gradebook and roster files live in a temp directory."
  (declare (indent 3))
  `(let* ((dir (make-temp-file "gradebook-" t))
          (org-canvas-gradebook-file (expand-file-name "gradebook.org" dir))
          (org-canvas-people-file (expand-file-name "people.org" dir))
          (org-canvas-assignments-file (expand-file-name "assignments.org" dir))
          (test-gradebook--enrollments ,enrollments)
          (test-gradebook--sections ,sections)
          (test-gradebook--summaries ,summaries)
          (test-gradebook--columns nil)
          (test-gradebook--columns-params nil)
          (test-gradebook--assignments nil)
          (test-gradebook--course nil)
          (test-gradebook--course-reads nil)
          (test-gradebook--submissions-params nil))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-gradebook--api)
                     ((symbol-function 'org-canvas-api-request) #'test-gradebook--request)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-gradebook-file org-canvas-people-file
                        org-canvas-assignments-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-gradebook--file ()
  "Return gradebook.org's text."
  (with-temp-buffer (insert-file-contents org-canvas-gradebook-file) (buffer-string)))

(defun test-gradebook--row (name)
  "Return the table row of the student NAME, cells trimmed, or nil."
  (let ((text (test-gradebook--file)))
    (when (string-match (format "^| *\\(?:\\[\\[[^]]+\\]\\[\\)?%s\\(?:\\]\\]\\)? *|\\(.*\\)$"
                                (regexp-quote name))
                        text)
      (mapcar #'string-trim (split-string (match-string 1 text) "|" t)))))

(describe "org-canvas--gradebook-rows"
  (it "folds a student's section enrollments into one row with the first score"
    (let ((rows (org-canvas--gradebook-rows
                 (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
                       (test-gradebook--enrollment 1 "Adams, Alice" 20 91.5 88.0)
                       (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.25))
                 (test-gradebook--fold-summaries
                  (list (test-gradebook--summary 1 0 2) (test-gradebook--summary 2 3 1))))))
      (expect (length rows) :to-equal 2)
      (expect (plist-get (car rows) :section-ids) :to-equal '(10 20))
      (expect (plist-get (car rows) :current) :to-equal 91.5)
      (expect (plist-get (car rows) :final) :to-equal 88.0)
      (expect (plist-get (car rows) :missing) :to-equal 0)
      (expect (plist-get (car rows) :late) :to-equal 2)
      (expect (plist-get (cadr rows) :missing) :to-equal 3)))

  (it "takes the posted scores when unposted is off and nil when Canvas has none"
    (let* ((e (test-gradebook--enrollment 1 "A" 10 80.0 75.0
                                          '(grades . ((current_score . 80.0) (final_score . 75.0)
                                                      (unposted_current_score . 85.0)
                                                      (unposted_final_score . :null)))))
           (posted (let ((org-canvas-gradebook-unposted nil))
                     (car (org-canvas--gradebook-rows (list e) nil))))
           (unposted (let ((org-canvas-gradebook-unposted t))
                       (car (org-canvas--gradebook-rows (list e) nil)))))
      (expect (plist-get posted :current) :to-equal 80.0)
      (expect (plist-get posted :final) :to-equal 75.0)
      (expect (plist-get unposted :current) :to-equal 85.0)
      (expect (plist-get unposted :final) :to-be nil)))

  (it "keeps the latest activity, leaves the counts nil when refused, and sorts by name"
    (let ((rows (org-canvas--gradebook-rows
                 (list (test-gradebook--enrollment 2 "Zed, Zoe" 10 50.0 50.0
                                                   '(last_activity_at . "2026-09-01T10:00:00Z"))
                       (test-gradebook--enrollment 2 "Zed, Zoe" 20 50.0 50.0
                                                   '(last_activity_at . "2026-09-05T10:00:00Z"))
                       `((user_id . 7) (course_section_id . 10) (user . ((id . 7)))
                         (grades . ((current_score . :null) (final_score . :null)))))
                 'refused)))
      (expect (mapcar (lambda (r) (plist-get r :name)) rows) :to-equal '("User 7" "Zed, Zoe"))
      (expect (plist-get (cadr rows) :last-activity) :to-equal "2026-09-05T10:00:00Z")
      (expect (plist-get (cadr rows) :missing) :to-be nil)
      (expect (plist-get (car rows) :current) :to-be nil))))

(describe "org-canvas--gradebook-number"
  (it "renders a score with one decimal, a count as an integer, and a dash for nothing"
    (expect (org-canvas--gradebook-number 91.25) :to-equal "91.2")
    (expect (org-canvas--gradebook-number 3) :to-equal "3")
    (expect (org-canvas--gradebook-number nil) :to-equal "-")
    (expect (org-canvas--gradebook-number "A") :to-equal "A")))

(describe "org-canvas--gradebook-cell-text"
  (it "collapses pipes and line breaks with the blanks around them, and blanks a non-string"
    (expect (org-canvas--gradebook-cell-text "Sees me | Tue\nafternoons\r\n  late ") :to-equal "Sees me Tue afternoons late")
    (expect (org-canvas--gradebook-cell-text "plain") :to-equal "plain")
    (expect (org-canvas--gradebook-cell-text nil) :to-equal "")
    (expect (org-canvas--gradebook-cell-text :null) :to-equal "")))

(describe "org-canvas--gradebook-stat"
  (it "answers a number and nothing for a null however it was decoded"
    (expect (org-canvas--gradebook-stat '((median . 7.5)) 'median) :to-equal 7.5)
    (expect (org-canvas--gradebook-stat '((median . :null)) 'median) :to-be nil)
    (expect (org-canvas--gradebook-stat '((median . nil)) 'median) :to-be nil)
    (expect (org-canvas--gradebook-stat nil 'median) :to-be nil)))

(describe "org-canvas--gradebook-assignment-cell"
  (it "falls back to the id when the analytics row has no title and no file to link"
    (let ((org-canvas-assignments-file nil))
      (expect (org-canvas--gradebook-assignment-cell '((assignment_id . 77) (title . nil)))
              :to-equal "Assignment 77")
      (expect (org-canvas--gradebook-assignment-cell '((assignment_id . 78) (title . "Essay")))
              :to-equal "Essay"))))

(describe "org-canvas--gradebook-mean"
  (it "averages the numbers and ignores the blanks"
    (expect (org-canvas--gradebook-mean '(90.0 nil 70.0)) :to-equal 80.0)
    (expect (org-canvas--gradebook-mean '(nil)) :to-be nil)))

(describe "org-canvas--gradebook-median"
  (it "takes the middle number, the mean of the middle two, and nothing of blanks"
    (expect (org-canvas--gradebook-median '(90 nil 70 80)) :to-equal 80.0)
    (expect (org-canvas--gradebook-median '(90 70 80.5 60)) :to-equal 75.25)
    (expect (org-canvas--gradebook-median '(nil :null)) :to-be nil)))

(describe "org-canvas-pull-gradebook"
  (it "writes the students table sorted by name and the sections table with means"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.25
                                          '(last_activity_at . "2026-09-05T10:00:00Z"))
              (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 1 "Adams, Alice" 20 91.5 88.0)
              (test-gradebook--enrollment 3 "Cruz, Cal" 20 :null :null))
        '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Recitation")))
        (list (test-gradebook--summary 1 0 2) (test-gradebook--summary 2 3 1))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Students\n")
        (expect text :to-match "^\\* Sections\n")
        (expect (string-match "\\* Students" text) :to-be-less-than (string-match "\\* Sections" text))
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +|$")
        (expect (string-match "Adams, Alice" text) :to-be-less-than (string-match "Beta, Bob" text))
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture, Recitation" "91.5" "88.0" "-" "0" "2" "-"))
        (expect (nthcdr 1 (test-gradebook--row "Beta, Bob"))
                :to-equal '("70.0" "65.2" "-" "3" "1" "<2026-09-05 Sat 10:00>"))
        (expect (test-gradebook--row "Cruz, Cal") :to-equal '("Recitation" "-" "-" "-" "-" "-" "-"))
        (expect text :to-match "^| Section +| Students +| Mean current +| Mean final +| Missing +|$")
        (expect (test-gradebook--row "Lecture") :to-equal '("2" "80.8" "76.6" "3"))
        (expect (test-gradebook--row "Recitation") :to-equal '("2" "91.5" "88.0" "0"))
        (expect text :not :to-match "user_id\\|:PROPERTIES:")
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

  (it "keeps gradebook.org out of git when the course directory is a work tree (issue #549)"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      ;; rev-parse answers 0 (a work tree); ls-files and check-ignore
      ;; answer 1 (not tracked, not ignored), so the pull adds the name.
      (cl-letf (((symbol-function 'process-file)
                 (lambda (_program _in _out _display &rest args)
                   (if (equal (car args) "rev-parse") 0 1))))
        (org-canvas-pull-gradebook))
      (expect (with-temp-buffer
                (insert-file-contents (expand-file-name ".gitignore" dir))
                (buffer-string))
              :to-equal "gradebook.org\n")))

  (it "links a student to the roster heading when people.org holds the user id"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (with-temp-file org-canvas-people-file
        (insert "* Students\n** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"))
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file)
              :to-match "^| \\[\\[file:people.org::\\*Adams, Alice\\]\\[Adams, Alice\\]\\] +|")))

  (it "leaves Missing and Late blank with a note when the analytics are refused"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        'refuse
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (car warned) :to-match "Could not read the student summaries"))
      (let ((text (test-gradebook--file)))
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "" "" "-"))
        (expect (test-gradebook--row "Lecture") :to-equal '("1" "91.5" "88.0" ""))
        (expect text :to-match "^Missing and Late are blank: Canvas refused"))))

  (it "rewrites the tables on a re-pull and keeps nothing written by hand"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (with-temp-buffer
        (insert-file-contents org-canvas-gradebook-file)
        (goto-char (point-max))
        (insert "A note I wrote under Sections\n")
        (write-region (point-min) (point-max) org-canvas-gradebook-file))
      (setq test-gradebook--enrollments
            (list (test-gradebook--enrollment 1 "Adams, Alice" 10 95.0 95.0)))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "95.0" "95.0" "-" "-" "-" "-"))
        (expect text :not :to-match "Beta, Bob")
        (expect text :not :to-match "A note I wrote")
        (expect (length (split-string text "^\\* Students" t)) :to-equal 2))))

  (it "adds one column per custom column after Last activity, in position order"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--columns
            (list (test-gradebook--column 5 "Notes" 2 '(1 . "Sees me | Tue\nafternoons"))
                  (test-gradebook--column 7 "Advisor" 1 '(1 . "Dr. Chen") '(2 . ""))))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +| Advisor +| Notes +|$")
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-" "Dr. Chen" "Sees me Tue afternoons"))
        (expect (test-gradebook--row "Beta, Bob")
                :to-equal '("Lecture" "70.0" "65.0" "-" "-" "-" "-" "" ""))
        ;; The separator row has one more cell per custom column.
        (expect text :to-match "^|-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+|$")
        (expect (test-gradebook--row "Lecture") :to-equal '("2" "80.8" "76.5" "0"))
        (expect test-gradebook--columns-params :to-be nil))))

  (it "asks for the hidden columns only when told to"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (let ((org-canvas-gradebook-include-hidden-columns t))
        (org-canvas-pull-gradebook))
      (expect test-gradebook--columns-params :to-equal '(("include_hidden" . "true")))))

  (it "leaves the custom columns out with one warning when Canvas refuses them"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--columns 'refuse)
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (length warned) :to-equal 1)
        (expect (car warned) :to-match "Could not read the custom gradebook columns"))
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +|$")
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-"))
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

  (it "writes the assignments table in Canvas order with the spread, links and dashes"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (with-temp-file org-canvas-assignments-file
        (insert "* Homework 1\n:PROPERTIES:\n:CANVAS_ID: 501\n:END:\n"))
      (setq test-gradebook--assignments
            (list (test-gradebook--assignment 502 "Homework 2" "2026-09-20T23:59:00Z" 10.0)
                  (test-gradebook--assignment 501 "Homework 1" "2026-09-06T23:59:00Z" 20.0
                                              '(min_score . 4.0) '(first_quartile . 12.5)
                                              '(median . 16.0) '(third_quartile . 18.0)
                                              '(max_score . 20.0) '(missing . 3) '(late . 2))))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Assignments\n")
        (expect (string-match "\\* Sections" text) :to-be-less-than (string-match "\\* Assignments" text))
        (expect text :to-match "^| Assignment +| Due +| Points +| Min +| Q1 +| Median +| Q3 +| Max +| Missing +| Late +|$")
        (expect (string-match "Homework 2" text) :to-be-less-than (string-match "Homework 1" text))
        (expect text :to-match "^| \\[\\[file:assignments.org::\\*Homework 1\\]\\[Homework 1\\]\\] +|")
        (expect (test-gradebook--row "Homework 1")
                :to-equal '("<2026-09-06 Sun 23:59>" "20.0" "4.0" "12.5" "16.0" "18.0" "20.0" "3" "2"))
        (expect (test-gradebook--row "Homework 2")
                :to-equal '("<2026-09-20 Sun 23:59>" "10.0" "-" "-" "-" "-" "-" "0" "0"))
        (expect text :not :to-match "Homework 2\\]\\]"))))

  (it "leaves a note under Assignments with one warning when Canvas refuses the analytics"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--assignments 'refuse)
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (length warned) :to-equal 1)
        (expect (car warned) :to-match "Could not read the assignment analytics"))
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Assignments\n+Canvas refused the assignment analytics")
        (expect text :not :to-match "^| Assignment ")
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-"))
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

  (it "writes a No assignments line when the course has none"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :to-match "^\\* Assignments\n+No assignments\n")))

  (it "writes the empty-file note for a course with no students"
    (test-gradebook--with-course nil nil nil
      (expect (org-canvas-pull-gradebook) :to-be nil)
      (expect (test-gradebook--file) :to-match "Canvas returned 0 items")
      (expect (org-canvas-gradebook-summary-text nil) :to-equal
              "Course: 0 students; current mean -, median -; final mean -, median -\n")))

  (it "returns the student rows, summed up with the Sections table (issue #451)"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0)
              (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 3 "Cruz, Cal" 20 80.0 :null))
        '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Recitation")))
        nil
      (let* ((rows (org-canvas-pull-gradebook))
             (text (org-canvas-gradebook-summary-text rows)))
        (expect (mapcar (lambda (r) (plist-get r :name)) rows)
                :to-equal '("Adams, Alice" "Beta, Bob" "Cruz, Cal"))
        (expect text :to-match "\\`| Section +| Students +| Mean current")
        (expect text :to-match "^| Lecture +| +2 | +80.8 |")
        (expect text :to-match "^| Recitation +| +1 | +80.0 | -")
        (expect text :not :to-match "Student +| Sections\\|Assignment")
        (expect text :to-match
                "|\n\nCourse: 3 students; current mean 80.5, median 80.0; final mean 76.5, median 76.5\n\\'"))))

  (it "sums up without a table when gradebook.org has no Sections heading"
    (test-gradebook--with-course nil nil nil
      (with-temp-file org-canvas-gradebook-file (insert "* Students\n"))
      (expect (org-canvas-gradebook-summary-text (list (list :current 90 :final 85)))
              :to-equal
              "Course: 1 student; current mean 90.0, median 90.0; final mean 85.0, median 85.0\n")
      (with-temp-file org-canvas-gradebook-file (insert "* Students\n* Sections\n| Lecture | 1 |\n"))
      (expect (org-canvas--gradebook-sections-table) :to-equal "| Lecture | 1 |")))

  (it "is in the pull tiers after people"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-people names)
              :to-be-less-than (cl-position 'org-canvas-pull-gradebook names)))))

;;;; Final grade overrides (issue #556)

(defun test-gradebook--write-people (dir)
  "Write people.org with Adams, Beta and Doe in DIR; return its path."
  (let ((file (expand-file-name "people.org" dir)))
    (with-temp-file file
      (insert "* Students\n"
              "** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"
              "** Beta, Bob\n:PROPERTIES:\n:USER_ID: 2\n:END:\n"
              "** Doe, Dee\n:PROPERTIES:\n:USER_ID: 4\n:END:\n"))
    file))

(defun test-gradebook--overrides-table ()
  "Return the final grade overrides table's text, or nil without one."
  (let ((text (test-gradebook--file)))
    (when (string-match
           "^#\\+NAME: final-grade-overrides\n\\(\\(?:.*\n\\)*?\\)\n" text)
      (match-string 1 text))))

(defun test-gradebook--write-overrides-table (rows)
  "Write gradebook.org's overrides table with ROWS, (NAME . CELL) pairs."
  (with-temp-file org-canvas-gradebook-file
    (insert "* Final grade overrides\n"
            "#+NAME: final-grade-overrides\n"
            "| Student | Override |\n"
            "|---+---|\n")
    (dolist (row rows)
      (insert (format "| %s | %s |\n" (car row) (cdr row))))))

(describe "the Students table's override column"
  (it "shows the override the enrollment carries beside Final, one per student"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.25)
              (test-gradebook--enrollment 1 "Adams, Alice" 20 91.5 88.0)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Recitation")))
        nil
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text
                :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +|$")
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture, Recitation" "91.5" "88.0" "93.2" "-" "-" "-"))
        (expect (test-gradebook--row "Beta, Bob")
                :to-equal '("Lecture" "70.0" "65.0" "-" "-" "-" "-"))))))

(describe "the final grade overrides table"
  (it "writes one row per student Canvas holds an override for"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file))
            (table (test-gradebook--overrides-table)))
        (expect text :to-match "^\\* Final grade overrides\n")
        (expect table :to-match "^| Student +| Override +|$")
        (expect table :to-match "^| Adams, Alice +| +93.5 |$")
        (expect table :not :to-match "Beta"))))

  (it "falls back to the user id when people.org lacks the student"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 5 "Nobody, Noe" 10 50.0 50.0) 70.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--overrides-table) :to-match "^| #5 +| +70.0 |$")))

  (it "says none when no student has one, replacing a stale table"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (setq test-gradebook--enrollments
            (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Final grade overrides\n")
        (expect text :to-match "No final grade overrides")
        (expect text :not :to-match "#\\+NAME: final-grade-overrides")
        (expect text :not :to-match "Adams, Alice +| 93"))))

  (it "a student's pull removes the heading a teacher pull left"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--course
            (test-gradebook--self-course (test-gradebook--student-enrollment)))
      (setq test-gradebook--submissions (test-gradebook--self-subs))
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :not :to-match "Final grade overrides"))))

(describe "org-canvas-sync-final-grade-overrides"
  (it "sends one mutation per row that differs and one clear per missing row"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0)
              (test-gradebook--with-override
               (test-gradebook--enrollment 3 "Cruz, Cal" 10 88.0 88.0) 88.0)
              (test-gradebook--enrollment 4 "Doe, Dee" 10 60.0 60.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table
       '(("Adams, Alice" . "93.5") ("Beta, Bob" . "") ("#4" . "70")))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (what doc vars)
                     (push (list what doc vars) sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        ;; A row matching Canvas, and one whose blank says no override,
        ;; are sent nothing; Cruz on Canvas with no row is cleared.
        (expect (length sent) :to-equal 2)
        (let* ((clear (cl-find-if (lambda (call)
                                    (null (alist-get 'overrideScore (nth 2 call))))
                                  sent))
               (set (cl-find-if (lambda (call)
                                   (equal (alist-get 'overrideScore (nth 2 call)) 70))
                                 sent)))
          (expect (alist-get 'enrollmentId (nth 2 clear)) :to-equal "1103")
          (expect (car clear) :to-match "clear Cruz, Cal")
          (expect (alist-get 'enrollmentId (nth 2 set)) :to-equal "1104")
          (expect (car set) :to-match "set Doe, Dee")
          (expect (nth 1 set) :to-match "setOverrideScore")))))

  (it "runs the mutation for real, reading back what Canvas stored"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--with-override
               (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0) 70.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((said nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-send)
                   (lambda (&rest _)
                     '((setOverrideScore
                        . ((grades . ((overrideScore . 93.5))))))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-sync-final-grade-overrides))
        ;; Adams's row is sent, Beta's Canvas override is cleared, and
        ;; both reads come back through the reply: two successes.
        (expect (car said) :to-match "1 set, 1 cleared, 0 failed"))))

  (it "counts a payload that answers errors as a failed request"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((said nil) (errored nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-send)
                   (lambda (&rest _)
                     '((setOverrideScore
                        . ((errors
                            . (((attribute . "enrollment")
                                (message . "has no score")))))))))
                   ((symbol-function 'org-canvas--log-error)
                   (lambda (_ fmt &rest args) (push (apply #'format fmt args) errored)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-sync-final-grade-overrides))
        (expect (car said) :to-match "0 set, 0 cleared, 1 failed")
        (expect (car errored) :to-match "setOverrideScore: has no score"))))

  (it "reads the Override column by its title, falling back to position 1"
    (expect (org-canvas--gradebook-overrides-column '("Student" "Override")) :to-equal 1)
    (expect (org-canvas--gradebook-overrides-column '("Student" "Grade")) :to-equal 1)
    (expect (org-canvas--gradebook-overrides-column '("Override" "Student")) :to-equal 0))

  (it "sends nothing when the table says what Canvas already holds"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        (expect sent :to-be nil))))

  (it "skips a row that names nobody, and counts roster gaps and failed requests"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table
       '(("Adams, Alice" . "93.5") ("Ghost, Gus" . "70") ("#9" . "80")))
      (let ((said nil) (warned nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (signal 'org-canvas-api-error (list "403"))))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_ fmt &rest args) (push (apply #'format fmt args) warned)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-sync-final-grade-overrides))
        (expect (car said) :to-match "0 set, 0 cleared, 2 failed")
        (expect (car warned) :to-match "Could not resolve student 'Ghost, Gus'"))))

  (it "refuses an override cell that is not a number, before anything is sent"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "high")))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (expect (org-canvas-sync-final-grade-overrides)
                  :to-throw 'org-canvas-config-error))
        (expect sent :to-be nil))))

  (it "sends nothing under the dry run, and says what it would"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((sent nil) (lines nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-send)
                   (lambda (&rest _) (push 'sent t) nil))
                  ((symbol-function 'org-canvas--log-info)
                   (lambda (_ fmt &rest args) (push (apply #'format fmt args) lines))))
          (let ((org-canvas--dry-run t))
            (org-canvas-sync-final-grade-overrides)))
        (expect sent :to-be nil)
        (expect (cl-find-if (lambda (l) (string-match-p "\\[DRY-RUN\\].*Adams, Alice" l))
                            lines)
                :to-be-truthy))))

  (it "refuses a token whose allowlist does not name submissions, and allows one that does"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (expect (org-canvas-sync-final-grade-overrides)
                :to-throw 'org-canvas-write-scope-error))
      (let ((sent nil) (org-canvas-write-allowlist '("submissions")))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        (expect sent :to-be-truthy))))

  (it "aborts without sending when the enrollments read fails"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (setq test-gradebook--enrollments 'refuse)
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (expect (org-canvas-sync-final-grade-overrides) :to-throw))
        (expect sent :to-be nil))))

  (it "does nothing without a table, and refuses without the file"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (expect (org-canvas-sync-final-grade-overrides)
              :to-throw 'org-canvas-config-error)
      (with-temp-file org-canvas-gradebook-file
        (insert "* Final grade overrides\nNo final grade overrides.\n"))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        (expect sent :to-be nil)))))

;;;; Per-group scores and the what-if (#452)

;;;; My grades: a student's enrolment (issue #488)

(defun test-gradebook--self-course (&rest enrollments)
  "A course whose caller holds ENROLLMENTS, as Canvas answers a vector."
  `((id . 1) (apply_assignment_group_weights . :json-false)
    (enrollments . ,(vconcat enrollments))))

(defun test-gradebook--student-enrollment ()
  "The caller's student enrolment, as total_scores adds its totals."
  '((type . "student") (role . "StudentEnrollment") (user_id . 7)
    (enrollment_state . "active")
    (computed_current_score . 87.5) (computed_current_grade . "B+")
    (computed_final_score . 60.25) (computed_final_grade . :null)))

(defun test-gradebook--self-sub (aid name due points &rest extra)
  "The caller's submission to assignment AID, NAME, due DUE, out of POINTS.
EXTRA pairs come first, so an explicit value shadows the default."
  (append extra
          `((id . ,(+ 500 aid)) (assignment_id . ,aid) (user_id . 7)
            (score . :null) (grade . :null) (workflow_state . "unsubmitted")
            (late . :json-false) (missing . :json-false) (excused . :json-false)
            (submission_comments . [])
            (assignment . ((id . ,aid) (name . ,name) (due_at . ,due)
                           (points_possible . ,points))))))

(defun test-gradebook--self-subs ()
  "Three submissions, out of due order, one undated, one with comments."
  (vector
   (test-gradebook--self-sub 3 "Reading log" :null 5)
   (test-gradebook--self-sub 2 "Homework 2" "2026-09-20T03:59:00Z" 10
                             '(missing . t) '(late . t))
   (test-gradebook--self-sub
    1 "Homework 1" "2026-09-06T03:59:00Z" 10
    '(score . 8.5) '(grade . "8.5") '(workflow_state . "graded")
    '(submission_comments
      . [((id . 1) (author_name . "Pat Teacher") (created_at . "2026-09-08T14:00:00Z")
          (comment . "Good work.\nShow the units | next time."))]))))

(defun test-gradebook--self-row (name)
  "Return the My grades table row of the assignment NAME, cells trimmed."
  (let ((text (test-gradebook--file)))
    (when (string-match (format "^| *%s *|\\(.*\\)$" (regexp-quote name)) text)
      (mapcar #'string-trim (split-string (match-string 1 text) "|" t)))))

(describe "org-canvas--gradebook-student-enrollment"
  (it "finds the caller's student enrolment"
    (let ((e (test-gradebook--student-enrollment)))
      (expect (org-canvas--gradebook-student-enrollment (test-gradebook--self-course e))
              :to-equal e)))

  (it "answers nil for a teacher, a TA, a designer or a teacher who is also a student"
    (dolist (type '("teacher" "ta" "designer"))
      (expect (org-canvas--gradebook-student-enrollment
               (test-gradebook--self-course `((type . ,type))))
              :to-be nil))
    (expect (org-canvas--gradebook-student-enrollment
             (test-gradebook--self-course (test-gradebook--student-enrollment)
                                          '((type . "teacher"))))
            :to-be nil))

  (it "answers nil without a course, or with enrolments null"
    (expect (org-canvas--gradebook-student-enrollment nil) :to-be nil)
    (expect (org-canvas--gradebook-student-enrollment '((enrollments . :null))) :to-be nil)
    (expect (org-canvas--gradebook-student-enrollment
             (test-gradebook--self-course '((type . "observer"))))
            :to-be nil)))

(describe "org-canvas-pull-gradebook for a student (issue #488)"
  (it "writes My grades by due date, the totals and the comments, and no roster"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        'refuse
      (with-temp-file org-canvas-gradebook-file
        (insert "* Students\n| Beta, Bob |\n* Sections\n* Assignments\n"
                "Canvas refused the assignment analytics for this token.\n"))
      (let* ((test-gradebook--course (test-gradebook--self-course
                                      (test-gradebook--student-enrollment)))
             (test-gradebook--submissions (test-gradebook--self-subs))
             (urls nil)
             (api (symbol-function 'org-canvas-api-request-all-pages))
             (rows (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                              (lambda (method url &optional params)
                                (push url urls)
                                (funcall api method url params))))
                     (org-canvas-pull-gradebook)))
             (text (test-gradebook--file))
             (row (car rows)))
        (expect (length urls) :to-equal 1)
        (expect (car urls) :to-match "/students/submissions\\'")
        (expect test-gradebook--submissions-params :to-equal
                '(("include[]" . "assignment") ("include[]" . "submission_comments")))
        (expect test-gradebook--course-reads :to-equal '((("include[]" . "total_scores"))))
        (expect text :not :to-match "^\\* \\(Students\\|Sections\\|Assignments\\|Groups\\)")
        (expect text :not :to-match "Beta, Bob\\|refused")
        (expect text :to-match "^\\* My grades\n")
        (expect text :to-match
                "^| Assignment +| Due +| Points +| Score +| Grade +| Status +| Late +| Missing +|$")
        (expect (< (string-match "Homework 1" text) (string-match "Homework 2" text)) :to-be t)
        (expect (< (string-match "| Homework 2" text) (string-match "| Reading log" text)) :to-be t)
        (expect (test-gradebook--self-row "Homework 1")
                :to-equal (list (org-canvas--iso8601-to-org-timestamp "2026-09-06T03:59:00Z")
                                "10" "8.5" "8.5" "graded" "no" "no"))
        (expect (nthcdr 2 (test-gradebook--self-row "Homework 2"))
                :to-equal '("-" "-" "unsubmitted" "yes" "yes"))
        (expect (test-gradebook--self-row "Reading log")
                :to-equal '("-" "5" "-" "-" "unsubmitted" "no" "no"))
        (expect text :to-match "^Current: 87.5 (B\\+); final: 60.2$")
        (expect text :to-match
                "^Comments:\n- Homework 1, Pat Teacher <2026-09-0[89] [A-Za-z]+ [0-9:]+>: Good work. Show the units next time.$")
        (expect text :to-match "^#\\+LAST_SYNCED:")
        (expect (plist-get row :self) :to-be t)
        (expect (plist-get row :user-id) :to-equal 7)
        (expect (plist-get row :current) :to-equal 87.5)
        (expect (plist-get row :final) :to-equal 60.25)
        (expect (plist-get row :assignments) :to-equal 3)
        (expect (plist-get row :missing) :to-equal 1)
        (let ((summary (org-canvas-gradebook-summary-text rows)))
          (expect summary :to-match "\\`| Assignment +| Due")
          (expect summary :to-match "^Current: 87.5 (B\\+); final: 60.2\n")
          (expect summary :to-match
                  "\n\nMe: 3 assignments, 1 missing; current 87.5; final 60.2\n\\'")))))

  (it "keeps gradebook.org out of git for a student's own grades too (issue #549)"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        'refuse
      (let ((test-gradebook--course (test-gradebook--self-course
                                      (test-gradebook--student-enrollment)))
            (test-gradebook--submissions (test-gradebook--self-subs)))
        ;; rev-parse answers 0 (a work tree); ls-files and check-ignore
        ;; answer 1 (not tracked, not ignored), so the pull adds the name.
        (cl-letf (((symbol-function 'process-file)
                   (lambda (_program _in _out _display &rest args)
                     (if (equal (car args) "rev-parse") 0 1))))
          (org-canvas-pull-gradebook)))
      (expect (with-temp-buffer
                (insert-file-contents (expand-file-name ".gitignore" dir))
                (buffer-string))
              :to-equal "gradebook.org\n")))

  (it "marks an excused submission and writes No assignments for none"
    (test-gradebook--with-course nil nil nil
      (let ((test-gradebook--course (test-gradebook--self-course
                                     '((type . "student") (user_id . 7))))
            (test-gradebook--submissions
             (vector (test-gradebook--self-sub 2 "Survey" :null 0)
                     (test-gradebook--self-sub 1 "Quiz" :null 1 '(excused . t)))))
        (org-canvas-pull-gradebook)
        ;; Both undated: by name.
        (expect (< (string-match "| Quiz" (test-gradebook--file))
                   (string-match "| Survey" (test-gradebook--file)))
                :to-be t)
        (expect (nthcdr 4 (test-gradebook--self-row "Quiz")) :to-equal '("excused" "no" "no"))
        (expect (test-gradebook--file) :to-match "^Current: -; final: -$")
        (expect (test-gradebook--file) :not :to-match "Comments:"))
      (let* ((test-gradebook--course (test-gradebook--self-course
                                      '((type . "student") (user_id . 7))))
             (test-gradebook--submissions [])
             (rows (org-canvas-pull-gradebook)))
        (expect (test-gradebook--file) :to-match "^\\* My grades\n+No assignments\n")
        (expect (org-canvas-gradebook-summary-text rows) :to-match
                "Me: 0 assignments, 0 missing; current -; final -\n\\'"))))

  (it "prints the student's standing from the shell's gradebook command"
    (require 'org-canvas-batch)
    (test-gradebook--with-course nil nil nil
      (let ((test-gradebook--course (test-gradebook--self-course
                                     (test-gradebook--student-enrollment)))
            (test-gradebook--submissions (test-gradebook--self-subs))
            (status nil))
        (let ((out (with-output-to-string
                     (setq status (org-canvas-batch--cmd-gradebook nil)))))
          (expect status :to-equal 0)
          (expect out :to-match "| Homework 1 ")
          (expect out :to-match "Me: 3 assignments, 1 missing; current 87.5; final 60.2\n\\'")))))

  (it "writes the teacher's tables, without My grades, when the course read is refused"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (with-temp-file org-canvas-gradebook-file
        (insert "* My grades\n| Homework 1 |\n"))
      (let ((test-gradebook--course 'refuse)
            (warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (cl-some (lambda (w) (string-match-p
                                      "Could not read the course (.*403 Forbidden.*); writing the teacher's tables"
                                      w))
                         warned)
                :to-be-truthy)
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-"))
        (expect (test-gradebook--file) :not :to-match "My grades\\|Homework 1")))))

(defun test-gradebook--ids (items)
  "Return the assignment ids of ITEMS, sorted."
  (sort (mapcar #'car items) #'<))

(defun test-gradebook--subsets (items k)
  "Return every K-element subset of ITEMS."
  (cond ((= k 0) (list nil))
        ((null items) nil)
        (t (append (mapcar (lambda (s) (cons (car items) s))
                           (test-gradebook--subsets (cdr items) (1- k)))
                   (test-gradebook--subsets (cdr items) k)))))

(defun test-gradebook--best-ratio (items k)
  "Return the highest ratio any K of ITEMS reach: the brute-force answer."
  (apply #'max (mapcar #'org-canvas--gradebook-ratio (test-gradebook--subsets items k))))

(describe "org-canvas--gradebook-drop"
  (it "returns the items untouched without drop rules"
    (let ((items '((1 5 10) (2 0 10))))
      (expect (org-canvas--gradebook-drop items '(:drop-lowest 0)) :to-equal items)
      (expect (org-canvas--gradebook-drop items nil) :to-equal items)))

  (it "drops the score whose removal maximises the percentage, not the lowest one"
    ;; Dropping 0/1 leaves 15/30 = 50%; dropping 5/20 leaves 10/11.
    (let ((items '((1 5 20) (2 0 1) (3 10 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 1)))
              :to-equal '(2 3))))

  (it "drops plain lowest scores when the points are equal"
    (let ((items '((1 7 10) (2 3 10) (3 9 10) (4 5 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 2)))
              :to-equal '(1 3))))

  (it "finds the best set that brute force finds"
    (let ((items '((1 3 4) (2 18 25) (3 0 2) (4 9 10) (5 40 60) (6 1 1) (7 12 30))))
      (dolist (drop '(1 2 3 4))
        (expect (org-canvas--gradebook-ratio
                 (org-canvas--gradebook-drop items (list :drop-lowest drop)))
                :to-be-close-to (test-gradebook--best-ratio items (- 7 drop)) 9))))

  (it "drops the score whose removal minimises the percentage for drop_highest"
    ;; Dropping 10/10 leaves 5/21; dropping 5/20 leaves 10/11.
    (let ((items '((1 5 20) (2 0 1) (3 10 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-highest 1)))
              :to-equal '(1 2))))

  (it "applies drop_lowest before drop_highest"
    (let ((items '((1 2 10) (2 4 10) (3 6 10) (4 8 10))))
      (expect (test-gradebook--ids
               (org-canvas--gradebook-drop items '(:drop-lowest 1 :drop-highest 1)))
              :to-equal '(2 3))))

  (it "always keeps one score, and lets drop_highest give way when the two cover all"
    (let ((items '((1 2 10) (2 4 10) (3 6 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 5)))
              :to-equal '(3))
      (expect (test-gradebook--ids
               (org-canvas--gradebook-drop items '(:drop-lowest 2 :drop-highest 1)))
              :to-equal '(3))))

  (it "never drops a never_drop score, which still counts toward the ratio"
    (let ((items '((1 0 10) (2 4 10) (3 6 10))))
      (expect (test-gradebook--ids
               (org-canvas--gradebook-drop items '(:drop-lowest 1 :never-drop (1))))
              :to-equal '(1 3))
      (expect (org-canvas--gradebook-drop '((1 0 10)) '(:drop-lowest 1 :never-drop (1)))
              :to-equal '((1 0 10)))))

  (it "drops by raw score when no droppable score has points"
    (let ((items '((1 3 0) (2 1 0) (3 2 0))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 1)))
              :to-equal '(1 3))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-highest 1)))
              :to-equal '(2 3))))

  (it "ranks from zero when the first guess has no points"
    (let ((items '((1 1 0) (2 1 0) (3 5 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 1)))
              :to-equal '(1 3)))))

(describe "org-canvas--gradebook-total"
  (let ((quizzes '(:id 1 :weight 40))
        (essays '(:id 2 :weight 60)))
    (it "weights each group's percentage"
      (expect (org-canvas--gradebook-total
               (list (cons quizzes '(8 . 10)) (cons essays '(50 . 100))) t)
              :to-be-close-to 62.0 6))

    (it "scales up to the weights of the groups with points possible"
      (expect (org-canvas--gradebook-total
               (list (cons quizzes '(8 . 10)) (cons essays nil)) t)
              :to-be-close-to 80.0 6))

    (it "answers nil when no weighted group has points"
      (expect (org-canvas--gradebook-total (list (cons quizzes nil)) t) :to-be nil)
      (expect (org-canvas--gradebook-total (list (cons '(:id 3 :weight 0) '(5 . 10))) t)
              :to-be nil))

    (it "counts every kept point unweighted"
      (expect (org-canvas--gradebook-total
               (list (cons quizzes '(8 . 10)) (cons essays '(50 . 100))) nil)
              :to-be-close-to (/ 5800.0 110) 6)
      (expect (org-canvas--gradebook-total (list (cons quizzes nil)) nil) :to-be nil))))

(describe "org-canvas--gradebook-letter and the statistics"
  (let ((scheme '(("B" . 80.0) ("A" . 90.0) ("F" . 0.0))))
    (it "rounds to two decimals before reading the cutoff"
      (expect (org-canvas--gradebook-letter 89.996 scheme) :to-equal "A")
      (expect (org-canvas--gradebook-letter 89.994 scheme) :to-equal "B")
      (expect (org-canvas--gradebook-letter -3 scheme) :to-equal "F")
      (expect (org-canvas--gradebook-letter nil scheme) :to-be nil)
      (expect (org-canvas--gradebook-letter 95 nil) :to-be nil))

    (it "counts letters best first and leaves out letters nobody gets"
      (expect (org-canvas--gradebook-letter-counts '(95 85 82 nil) scheme)
              :to-equal '(("A" . 1) ("B" . 2)))))

  (it "takes the median of odd, even and empty lists"
    (expect (org-canvas--gradebook-median '(3 1 2)) :to-equal 2.0)
    (expect (org-canvas--gradebook-median '(4 1 nil 2 3)) :to-equal 2.5)
    (expect (org-canvas--gradebook-median '(nil)) :to-be nil)))

(describe "org-canvas--gradebook-assignment-map and the counted scores"
  (it "keeps published assignments that count toward the grade"
    (let ((groups `(((id . 1) (assignments . [((id . 11) (published . t) (points_possible . 10))
                                              ((id . 12) (published . :json-false) (points_possible . 5))
                                              ((id . 13) (published . t) (points_possible . :null))
                                              ((id . 14) (published . t) (omit_from_final_grade . t)
                                               (points_possible . 5))])))))
      (expect (org-canvas--gradebook-assignment-map groups)
              :to-equal '((11 1 . 10) (13 1 . 0)))))

  (it "counts graded scores that are not excused, and only posted ones when told"
    (let ((map '((11 1 . 10))))
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 7) (posted_at . :null)) map t)
              :to-equal '(11 7 10))
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 7) (posted_at . :null)) map nil)
              :to-be nil)
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 7) (posted_at . "2026-09-01T00:00:00Z")) map nil)
              :to-equal '(11 7 10))
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 0) (excused . t)) map t)
              :to-be nil)
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . :null)) map t)
              :to-be nil)
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 99) (score . 3)) map t)
              :to-be nil)))

  (it "reads Canvas's rules, null or nested, into a group plist"
    (expect (org-canvas--gradebook-canvas-group
             '((id . 1) (name . "Q") (group_weight . 15)
               (rules . ((drop_lowest . 1) (never_drop . [11 12])))))
            :to-equal '(:id 1 :name "Q" :weight 15 :drop-lowest 1 :drop-highest nil
                            :never-drop (11 12)))
    (expect (org-canvas--gradebook-canvas-group
             '((id . 2) (name . "E") (group_weight . :null) (rules . :null)))
            :to-equal '(:id 2 :name "E" :weight 0 :drop-lowest nil :drop-highest nil
                            :never-drop nil))))

(defun test-gradebook--sub (uid aid score &rest extra)
  "A submission of UID on AID scoring SCORE, posted, with EXTRA pairs first."
  (append extra `((user_id . ,uid) (assignment_id . ,aid) (score . ,score)
                  (posted_at . "2026-09-01T00:00:00Z") (excused . :json-false))))

(defun test-gradebook--fixture-groups ()
  "Quizzes (50%, drop lowest 1) and Essays (50%), with assignments."
  `(((id . 2) (name . "Essays") (position . 2) (group_weight . 50) (rules . nil)
     (assignments . [((id . 21) (published . t) (points_possible . 100))
                     ((id . 22) (published . t) (points_possible . 100)
                      (omit_from_final_grade . t))]))
    ((id . 1) (name . "Quizzes") (position . 1) (group_weight . 50)
     (rules . ((drop_lowest . 1)))
     (assignments . [((id . 11) (published . t) (points_possible . 10))
                     ((id . 12) (published . t) (points_possible . 10))
                     ((id . 13) (published . t) (points_possible . 20))
                     ((id . 14) (published . :json-false) (points_possible . 10))]))))

(defun test-gradebook--fixture-submissions ()
  "Adams: Quizzes 93.3 after the drop, Essays 80.  Beta: Quizzes 50, no essay."
  (list (test-gradebook--sub 1 11 10) (test-gradebook--sub 1 12 2)
        (test-gradebook--sub 1 13 18) (test-gradebook--sub 1 21 80)
        (test-gradebook--sub 1 22 100) (test-gradebook--sub 1 14 5)
        (test-gradebook--sub 2 11 0) (test-gradebook--sub 2 12 0 '(excused . t))
        (test-gradebook--sub 2 13 10) (test-gradebook--sub 2 21 :null)))

(defmacro test-gradebook--with-scores (canvas-beta &rest body)
  "Run BODY in a course of Adams (86.67) and Beta (CANVAS-BETA) with groups on."
  (declare (indent 1))
  `(test-gradebook--with-course
       (list (test-gradebook--enrollment 1 "Adams, Alice" 10 86.67 80.0)
             (test-gradebook--enrollment 2 "Beta, Bob" 10 ,canvas-beta 20.0))
       '(((id . 10) (name . "Lecture")))
       nil
     (let ((org-canvas-gradebook-groups t)
           (org-canvas-gradebook-scores-file (expand-file-name "gradebook-scores.eld" dir))
           (org-canvas-assignment-groups-file (expand-file-name "assignment-groups.org" dir))
           (org-canvas-settings-file (expand-file-name "settings.org" dir))
           (test-gradebook--groups (test-gradebook--fixture-groups))
           (test-gradebook--submissions (test-gradebook--fixture-submissions))
           (test-gradebook--course '((id . 1) (apply_assignment_group_weights . t)
                                     (grading_standard_id . 5)))
           (test-gradebook--standard '((id . 5) (grading_scheme . [((name . "A") (value . 0.85))
                                                                   ((name . "B") (value . 0.75))
                                                                   ((name . "F") (value . 0))]))))
       (cl-letf (((symbol-function 'org-canvas-api-request) #'test-gradebook--request)
                 ;; Not a git work tree: the scores file is left alone.
                 ((symbol-function 'process-file) (lambda (&rest _) 128)))
         (unwind-protect (progn ,@body)
           (dolist (f (list org-canvas-assignment-groups-file org-canvas-settings-file))
             (let ((buf (find-buffer-visiting f)))
               (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))))))))

(defun test-gradebook--groups-row (name)
  "Return NAME's row of the Groups table, cells trimmed, or nil."
  (let ((text (test-gradebook--file)))
    (when (string-match "^\\* Groups\n" text)
      (let ((groups (substring text (match-end 0))))
        (when (string-match (format "^| *%s *|\\(.*\\)$" (regexp-quote name)) groups)
          (mapcar #'string-trim (split-string (match-string 1 groups) "|" t)))))))

(describe "org-canvas-pull-gradebook with org-canvas-gradebook-groups"
  (it "reads the course once, for the role and the group weights (issue #488)"
    (test-gradebook--with-scores 40.0
      (org-canvas-pull-gradebook)
      (expect test-gradebook--course-reads :to-equal '((("include[]" . "total_scores"))))
      (expect (test-gradebook--groups-row "Adams, Alice") :to-be-truthy)))

  (it "keeps gradebook.org and the scores file out of git in a work tree (issue #549)"
    (test-gradebook--with-scores 50.0
      ;; rev-parse answers 0 (a work tree); ls-files and check-ignore
      ;; answer 1 (not tracked, not ignored), so the pull adds both
      ;; names, the file's before the scores file's.
      (cl-letf (((symbol-function 'process-file)
                 (lambda (_program _in _out _display &rest args)
                   (if (equal (car args) "rev-parse") 0 1))))
        (org-canvas-pull-gradebook))
      (expect (with-temp-buffer
                (insert-file-contents (expand-file-name ".gitignore" dir))
                (buffer-string))
              :to-equal "gradebook.org\ngradebook-scores.eld\n")))

  (it "reads the course itself when it was not handed one"
    (test-gradebook--with-scores 40.0
      (let ((scores (org-canvas--gradebook-fetch-scores
                     (list (list :user-id 1 :name "Adams, Alice" :current 86.67)) nil)))
        (expect (plist-get scores :weighted) :to-be t)
        (expect (length test-gradebook--course-reads) :to-equal 1))))

  (it "writes each group's percentage after the drop rules and checks Canvas's total"
    (test-gradebook--with-scores 40.0
      (spy-on 'org-canvas--log-warning)
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^| Student +| Quizzes +| Essays +| Computed +| Canvas +|$")
        (expect (test-gradebook--groups-row "Adams, Alice")
                :to-equal '("93.3" "80.0" "86.7" "86.7"))
        (expect (test-gradebook--groups-row "Beta, Bob")
                :to-equal '("50.0" "-" "50.0" "40.0"))
        (expect text :to-match "1 student(s) whose computed total differs from Canvas's by more than 0.1: Beta, Bob")
        (expect (spy-calls-count 'org-canvas--log-warning) :to-equal 1))
      (let ((scores (org-canvas--gradebook-read-scores)))
        (expect (plist-get scores :weighted) :to-be t)
        (expect (plist-get scores :scheme) :to-equal '(("A" . 85.0) ("B" . 75.0) ("F" . 0.0)))
        (expect (mapcar (lambda (g) (plist-get g :name)) (plist-get scores :groups))
                :to-equal '("Quizzes" "Essays")))))

  (it "says every total matches when they do, and counts only posted scores when told"
    (test-gradebook--with-scores 50.0
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :to-match "Every computed total is within 0.1 of Canvas's")
      (let ((org-canvas-gradebook-unposted nil)
            (test-gradebook--submissions
             (cons (test-gradebook--sub 2 21 100 '(posted_at . :null))
                   (test-gradebook--fixture-submissions))))
        (org-canvas-pull-gradebook)
        (expect (test-gradebook--groups-row "Beta, Bob") :to-equal '("50.0" "-" "50.0" "50.0")))))

  (it "removes the Groups heading when the option is turned off"
    (test-gradebook--with-scores 50.0
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :to-match "^\\* Groups")
      (let ((org-canvas-gradebook-groups nil))
        (org-canvas-pull-gradebook))
      (expect (test-gradebook--file) :not :to-match "Groups")
      (expect (test-gradebook--file) :to-match "^\\* Assignments")))

  (it "leaves a note and keeps no scores when Canvas refuses them"
    (test-gradebook--with-scores 50.0
      (let ((test-gradebook--submissions 'refuse))
        (spy-on 'org-canvas--log-warning)
        (org-canvas-pull-gradebook)
        (expect (test-gradebook--file) :to-match "Canvas refused the scores for this token")
        (expect (spy-calls-count 'org-canvas--log-warning) :to-equal 1)
        (expect (file-exists-p org-canvas-gradebook-scores-file) :to-be nil)))))

(describe "org-canvas--gradebook-fetch-scheme"
  (it "takes Canvas's default scheme when the course names none but shows letters"
    (expect (org-canvas--gradebook-fetch-scheme
             '((grading_standard_id . :null))
             '(((grades . ((current_grade . "B+"))))))
            :to-be org-canvas--gradebook-default-scheme)
    (expect (org-canvas--gradebook-fetch-scheme '((grading_standard_id . 0))
                                                '(((grades . ((current_score . 80))))))
            :to-be nil))

  (it "leaves the letters out with a warning when the standard is refused"
    (with-org-canvas-test-config
      (let ((test-gradebook--standard 'refuse))
        (cl-letf (((symbol-function 'org-canvas-api-request) #'test-gradebook--request))
          (spy-on 'org-canvas--log-warning)
          (expect (org-canvas--gradebook-fetch-scheme '((grading_standard_id . 5)) nil)
                  :to-be nil)
          (expect (spy-calls-count 'org-canvas--log-warning) :to-equal 1))))))

(defconst test-gradebook--local-groups
  "* Assignment Groups
** Quizzes
:PROPERTIES:
:CANVAS_ID: 1
:WEIGHT: 50
:END:
** Essays
:PROPERTIES:
:WEIGHT: 50
:END:
** Labs
:PROPERTIES:
:WEIGHT: 0
:END:
"
  "assignment-groups.org with the Quizzes drop rule removed and an unpushed Labs.")

(defun test-gradebook--what-if ()
  "Run the what-if and return its report, runs of blanks collapsed."
  (cl-letf (((symbol-function 'princ) #'ignore))
    (replace-regexp-in-string " +" " " (org-canvas-gradebook-what-if))))

(describe "org-canvas-gradebook-what-if"
  (it "recomputes every total under the local table and reports the letter moves"
    (test-gradebook--with-scores 40.0
      (org-canvas-pull-gradebook)
      (with-temp-file org-canvas-assignment-groups-file (insert test-gradebook--local-groups))
      (let ((report (test-gradebook--what-if)))
        (expect report :to-match "weights on. Nothing was sent to Canvas")
        (expect report :to-match "- Labs is not on Canvas as pulled")
        (expect report :to-match "^| Table | Mean | Median | A | B | F |$")
        (expect report :to-match "^| Canvas now | 63.3 | 63.3 | 1 | 0 | 1 |$")
        (expect report :to-match "^| Canvas's table, recomputed | 68.3 | 68.3 | 1 | 0 | 1 |$")
        (expect report :to-match "^| assignment-groups.org | 55.4 | 55.4 | 0 | 1 | 1 |$")
        (expect report :to-match "^| Quizzes | 50 -> 50 | 1 -> 0 | 0 -> 0 |$")
        (expect report :not :to-match "^| Essays |")
        (expect report :to-match "^| Adams, Alice | 86.7 | 77.5 | A | B |$")
        (expect report :not :to-match "^| Beta, Bob |"))))

  (it "reads unsaved edits and the weighting from settings.org"
    (test-gradebook--with-scores 40.0
      (org-canvas-pull-gradebook)
      (with-temp-file org-canvas-assignment-groups-file (insert test-gradebook--local-groups))
      (with-temp-file org-canvas-settings-file
        (insert "* Settings\n:PROPERTIES:\n:APPLY_WEIGHTS: false\n:END:\n"))
      (with-current-buffer (find-file-noselect org-canvas-assignment-groups-file)
        (goto-char (point-min))
        (re-search-forward ":CANVAS_ID: 1\n")
        (insert ":DROP_LOWEST: 1\n"))
      (let ((report (test-gradebook--what-if)))
        ;; Unweighted, Adams keeps 28/30 and 80/100: 83.1, a B.
        (expect report :to-match "weights off")
        (expect report :to-match "No group's weight or drop rules differ from Canvas's")
        (expect report :to-match "^| Adams, Alice | 86.7 | 83.1 | A | B |$"))))

  (it "keeps Canvas's table for a group the file lacks and says when there are no letters"
    (let* ((group '(:id 1 :name "Quizzes" :weight 50 :drop-lowest 1))
           (merged (org-canvas--gradebook-what-if-groups (list group) nil)))
      (expect (car merged) :to-equal (list group))
      (expect (cadr merged) :to-match "Quizzes is not in assignment-groups.org"))
    (expect (with-temp-buffer
              (org-canvas--gradebook-insert-letter-changes '(("A" 80 80 70)) nil)
              (buffer-string))
            :to-match "no grading scheme"))

  (it "says so when no letter moves, and counts one missing total as a difference"
    (expect (with-temp-buffer
              (org-canvas--gradebook-insert-letter-changes '(("A" 80 80 81)) '(("A" . 50.0)))
              (buffer-string))
            :to-match "No student's letter changes")
    (expect (org-canvas--gradebook-differs-p nil 40.0) :to-be-truthy)
    (expect (org-canvas--gradebook-differs-p nil nil) :not :to-be-truthy)
    (expect (org-canvas--gradebook-differs-p 40.05 40.0) :not :to-be-truthy))

  (it "refuses without pulled scores or without assignment-groups.org"
    (test-gradebook--with-scores 40.0
      (expect (org-canvas-gradebook-what-if) :to-throw 'user-error)
      (org-canvas-pull-gradebook)
      (expect (org-canvas-gradebook-what-if) :to-throw 'user-error))))

;;;; One student's standing (#542)

(defun test-gradebook--standing-groups ()
  "Quizzes (50%, drop lowest 1) and Essays (50%), for the standing fixtures.
Q3 sits in Quizzes, E2 is omitted from the final grade."
  `(((id . 2) (name . "Essays") (position . 2) (group_weight . 50) (rules . nil)
     (assignments . [((id . 21) (published . t) (points_possible . 100))
                     ((id . 22) (published . t) (omit_from_final_grade . t)
                      (points_possible . 100))]))
    ((id . 1) (name . "Quizzes") (position . 1) (group_weight . 50)
     (rules . ((drop_lowest . 1)))
     (assignments . [((id . 11) (published . t) (points_possible . 10))
                     ((id . 12) (published . t) (points_possible . 10))
                     ((id . 13) (published . t) (points_possible . 20))]))))

(defun test-gradebook--standing-sub (aid name group due lock points &rest extra)
  "Adams's submission to assignment AID, NAME, in group GROUP, out of POINTS.
DUE and LOCK are ISO8601 strings, or :null for no date; EXTRA pairs
come first, so an explicit value shadows the default.
Positional is right: the five are the fields a case varies and each
call reads as the submission it describes; a bundle would rename them
and nothing else (issue #599)."
  (append extra
          `((user_id . 1) (assignment_id . ,aid) (score . :null)
            (grade . :null) (workflow_state . "unsubmitted")
            (late . :json-false) (missing . :json-false) (excused . :json-false)
            (posted_at . :null)
            (assignment . ((id . ,aid) (name . ,name)
                           (assignment_group_id . ,group)
                           (published . t) (points_possible . ,points)
                           (due_at . ,due) (lock_at . ,lock))))))

(defun test-gradebook--standing-subs ()
  "Adams's submissions, by due date: Q1 graded 4/10 and closed, Q2 open
past its due date, Q3 closed and ungraded, E1 graded 80/100 with no
lock, and E2, which the final grade omits."
  (vector
   (test-gradebook--standing-sub 11 "Q1" 1 "2026-09-01T03:59:00Z" "2026-09-08T03:59:00Z" 10
                                 '(score . 4.0) '(workflow_state . "graded")
                                 '(posted_at . "2026-09-02T00:00:00Z"))
   (test-gradebook--standing-sub 12 "Q2" 1 "2026-09-15T03:59:00Z" "2030-01-01T00:00:00Z" 10)
   (test-gradebook--standing-sub 13 "Q3" 1 "2026-09-20T03:59:00Z" "2026-09-27T03:59:00Z" 20
                                 '(missing . t))
   (test-gradebook--standing-sub 21 "E1" 2 "2026-10-01T03:59:00Z" :null 100
                                 '(score . 80.0) '(workflow_state . "graded")
                                 '(posted_at . "2026-10-02T00:00:00Z"))
   (test-gradebook--standing-sub 22 "E2" 2 "2026-10-05T03:59:00Z" :null 100)))

(defmacro test-gradebook--with-standing (&rest body)
  "Run BODY with the fake API serving Adams's standing (issue #542).
Quizzes (50%, drop lowest 1) holds Q1 (4/10, graded, closed), Q2 (10
points, still open past its due date) and Q3 (20 points, closed,
ungraded); Essays (50%) holds E1 (80/100, graded) and E2 (omitted from
the final grade).  Adams's Canvas totals are current 60.0 (D), final
55.0 (F); the scheme runs A 90, B 80, C 70, D 60, F 0."
  (declare (indent 0))
  `(test-gradebook--with-course
       (list (test-gradebook--enrollment
              1 "Adams, Alice" 10 60.0 55.0
              '(grades . ((current_score . 60.0) (final_score . 55.0)
                          (unposted_current_score . 60.0)
                          (unposted_final_score . 55.0)
                          (current_grade . "D") (final_grade . "F")
                          (unposted_current_grade . "D")
                          (unposted_final_grade . "F")))))
       '(((id . 10) (name . "Lecture")))
       nil
     (let ((test-gradebook--groups (test-gradebook--standing-groups))
           (test-gradebook--submissions (test-gradebook--standing-subs))
           (test-gradebook--course '((id . 1) (apply_assignment_group_weights . t)
                                     (grading_standard_id . 5)))
           (test-gradebook--standard
            '((id . 5) (grading_scheme . [((name . "A") (value . 0.9))
                                          ((name . "B") (value . 0.8))
                                          ((name . "C") (value . 0.7))
                                          ((name . "D") (value . 0.6))
                                          ((name . "F") (value . 0))]))))
       ,@body)))

(describe "org-canvas--gradebook-student-pick (issue #542)"
  (it "resolves a sortable name, a plain name, an id and a #id"
    (let ((enrollments
           (list (test-gradebook--enrollment
                  1 "Adams, Alice" 10 60.0 55.0
                  '(user . ((id . 1) (name . "Alice Adams")
                            (sortable_name . "Adams, Alice")))))))
      (dolist (arg '("Adams, Alice" "Alice Adams" "1" "#1"))
        (expect (alist-get 'user_id
                           (car (org-canvas--gradebook-student-pick enrollments arg)))
                :to-be 1))))

  (it "says when no student matches, and when the name is ambiguous"
    (let ((enrollments (list (test-gradebook--enrollment 1 "Adams, Alice" 10 60.0 55.0)
                             (test-gradebook--enrollment 2 "Adams, Alice" 20 70.0 65.0))))
      (expect (org-canvas--gradebook-student-pick enrollments "Nobody")
              :to-throw 'user-error)
      (expect (org-canvas--gradebook-student-pick enrollments "Adams, Alice")
              :to-throw 'user-error)
      (expect (org-canvas--gradebook-student-pick enrollments "2")
              :to-be-truthy))))

(describe "org-canvas--gradebook-student-row and the ceiling (issue #542)"
  (let ((now (date-to-time "2026-10-15T12:00:00Z")))
    (it "marks a column open until its lock, closed after it, and open without one"
      (let* ((assignments (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups)))
             (rows (org-canvas--gradebook-student-rows
                    (append (test-gradebook--standing-subs) nil) assignments now))
             (by-name (lambda (name)
                        (cl-find-if (lambda (r) (equal (plist-get r :name) name))
                                    rows))))
        (expect (plist-get (funcall by-name "Q1") :open) :to-be nil)
        (expect (plist-get (funcall by-name "Q2") :open) :to-be t)
        (expect (plist-get (funcall by-name "Q3") :open) :to-be nil)
        (expect (plist-get (funcall by-name "E1") :open) :to-be t)
        (expect (plist-get (funcall by-name "Q1") :item) :to-equal '(11 4.0 10))
        (expect (plist-get (funcall by-name "Q2") :item) :to-be nil)
        (expect (plist-get (funcall by-name "Q1") :counted) :to-be t)
        (expect (plist-get (funcall by-name "E2") :counted) :to-be nil)
        (expect (plist-get (funcall by-name "E1") :excused) :to-be nil)))

    (it "builds the ceiling: graded scores stand, open columns full, closed ungraded zero"
      (let* ((assignments (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups)))
             (rows (org-canvas--gradebook-student-rows
                    (append (test-gradebook--standing-subs) nil) assignments now)))
        (expect (org-canvas--gradebook-student-ceiling-items rows)
                :to-equal '((11 4.0 10) (12 10 10) (13 0 20) (21 80.0 100)))))

    (it "names the past-due columns still open, counted and ungraded"
      (let* ((assignments (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups)))
             (rows (org-canvas--gradebook-student-rows
                    (append (test-gradebook--standing-subs) nil) assignments now)))
        (expect (mapcar (lambda (r) (plist-get r :name))
                        (org-canvas--gradebook-student-open-rows rows now))
                :to-equal '("Q2"))))

    (it "names an assignment Canvas left nameless by its id"
      (expect (plist-get (org-canvas--gradebook-student-row
                          '((assignment_id . 13) (score . :null)
                            (workflow_state . "unsubmitted")
                            (assignment . ((id . 13) (assignment_group_id . 1)
                                           (published . t) (points_possible . 20)
                                           (due_at . "2026-09-20T03:59:00Z")
                                           (lock_at . "2026-09-27T03:59:00Z"))))
                          (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups))
                          now)
                         :name)
              :to-equal "Assignment 13"))))

(describe "org-canvas-gradebook-student (issue #542)"
  (it "prints every column grouped, with the still-open list and the ceiling"
    (test-gradebook--with-standing
      (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                     (replace-regexp-in-string
                      " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
        (expect report :to-match "Student standing: Adams, Alice")
        (expect report :to-match "Adams, Alice [(]user 1[)], Lecture$")
        (expect report :to-match "Canvas: current 60.0 (D), final 55.0 (F)$")
        (expect report :to-match "nothing was sent, nothing written")
        (expect report :to-match "^\\* Quizzes$")
        (expect report :to-match "^weight 50%, drop lowest 1 — 40.0% now, 70.0% at best$")
        (expect report :to-match "^\\* Essays$")
        (expect report :to-match "^weight 50% — 80.0% now, 80.0% at best$")
        (expect report :to-match
                 "^| Column | Due | Lock | Score | Points | State | Late | Missing | Open |$")
        (expect report :to-match "| Q1 | <2026-09-01")
        (expect report :to-match
                 "| Q2 |.*| - | 10 | unsubmitted | no | no | yes |$")
        (expect report :to-match "| E2 (not counted) |")
        (expect report :to-match "^Still open and past due: Q2$")
        (expect report :to-match
                 "^Best possible: 75.0% (C), every still-open column at full marks\\.$"))))

  (it "reads one student's submissions with their assignments, by due date"
    (test-gradebook--with-standing
      (expect (org-canvas--gradebook-student-params 1)
              :to-equal '(("student_ids[]" . "1") ("include[]" . "assignment")))
      (org-canvas--gradebook-student-submissions 1)
      (expect test-gradebook--submissions-params
              :to-equal '(("student_ids[]" . "1") ("include[]" . "assignment")))
      (expect (mapcar (lambda (s) (alist-get 'name (alist-get 'assignment s)))
                      (org-canvas--gradebook-student-submissions 1))
              :to-equal '("Q1" "Q2" "Q3" "E1" "E2"))))

  (it "says so when nothing is past due and still open"
    (test-gradebook--with-standing
      (let* ((test-gradebook--submissions
              (vector (test-gradebook--standing-sub
                       12 "Q2" 1 "2026-09-15T03:59:00Z" "2026-09-22T03:59:00Z" 10)))
             (report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "1")))))
        (expect report :to-match "^No column past due is still open\\.$"))))

  (it "says so when the course holds no assignments at all"
    (test-gradebook--with-standing
      (let* ((test-gradebook--submissions (vector))
             (report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "#1")))))
        (expect report :to-match "^No assignments\\.$")
        (expect report :to-match "^No column past due is still open\\.$"))))

  (it "prints a group's hurdle against its percentage after the drop"
    (test-gradebook--with-standing
      (let ((org-canvas-gradebook-hurdles '(("Quizzes" . 30))))
        (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
          (expect report :to-match
                  "^Hurdle 30%: 40.0% now, 70.0% at best — meets it now\\.$")))
      (let ((org-canvas-gradebook-hurdles '(("Quizzes" . 60))))
        (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
          (expect report :to-match
                  "^Hurdle 60%: 40.0% now, 70.0% at best — within reach at best\\.$")))
      (let ((org-canvas-gradebook-hurdles '(("Quizzes" . 90) ("No such group" . 50))))
        (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
          (expect report :to-match
                  "^Hurdle 90%: 40.0% now, 70.0% at best — out of reach even at best\\.$")
          (expect report :to-match "^No group named No such group for its hurdle\\.$")))))

  (it "prompts with completion over the roster's sortable names"
    (test-gradebook--with-standing
      (let* ((test-gradebook--enrollments
              (append (org-canvas--gradebook-fetch-enrollments)
                      (list '((user_id . 3)
                              (user . ((id . 3) (name . "No Sortable")))))))
             (seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (setq seen (append collection nil))
                     "Adams, Alice")))
          (expect (org-canvas--gradebook-student-prompt) :to-equal "Adams, Alice")
          (expect seen :to-contain "Adams, Alice")
          (expect seen :to-contain "No Sortable")))))

  (it "reads a group's drop rules and never-drop ids as text"
    (expect (org-canvas--gradebook-student-drops-text
             '(:drop-lowest 1 :drop-highest 2 :never-drop (11 12)))
            :to-equal "drop lowest 1, drop highest 2, never drops 11, 12")
    (expect (org-canvas--gradebook-student-drops-text '(:weight 50))
            :to-equal ""))

  (it "answers interactively, the name completed from the roster"
    (test-gradebook--with-standing
      (let ((report (cl-letf (((symbol-function 'completing-read)
                               (lambda (_prompt _collection &rest _)
                                 "Adams, Alice"))
                              ((symbol-function 'princ) #'ignore))
                     (replace-regexp-in-string
                      " +" " " (call-interactively #'org-canvas-gradebook-student)))))
        (expect report :to-match "Student standing: Adams, Alice")
        (expect report :to-match "^Best possible: 75.0% (C),")))))

(provide 'org-canvas-gradebook-test)
;;; org-canvas-gradebook-test.el ends here
