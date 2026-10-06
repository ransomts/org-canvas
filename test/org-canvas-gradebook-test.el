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

(defun test-gradebook--request (_method url &rest _args)
  "Answer a single request for URL: the course or its grading standard."
  (cond
   ((string-match "/grading_standards/" url)
    (if (eq test-gradebook--standard 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--standard))
   ((string-match "/courses/[0-9]+/\\'" url) test-gradebook--course)
   (t (error "Unexpected request: %s" url))))

(defun test-gradebook--api (_method url &optional _params)
  "Answer URL from the fake tables, as the paginated helper would."
  (cond
   ((string-match "/enrollments" url) test-gradebook--enrollments)
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
EXTRA pairs come first, so an explicit value shadows the default."
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
          (test-gradebook--assignments nil))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-gradebook--api)
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
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Missing +| Late +| Last activity +|$")
        (expect (string-match "Adams, Alice" text) :to-be-less-than (string-match "Beta, Bob" text))
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture, Recitation" "91.5" "88.0" "0" "2" "-"))
        (expect (nthcdr 1 (test-gradebook--row "Beta, Bob"))
                :to-equal '("70.0" "65.2" "3" "1" "<2026-09-05 Sat 10:00>"))
        (expect (test-gradebook--row "Cruz, Cal") :to-equal '("Recitation" "-" "-" "-" "-" "-"))
        (expect text :to-match "^| Section +| Students +| Mean current +| Mean final +| Missing +|$")
        (expect (test-gradebook--row "Lecture") :to-equal '("2" "80.8" "76.6" "3"))
        (expect (test-gradebook--row "Recitation") :to-equal '("2" "91.5" "88.0" "0"))
        (expect text :not :to-match "user_id\\|:PROPERTIES:")
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

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
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "" "" "-"))
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
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "95.0" "95.0" "-" "-" "-"))
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
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Missing +| Late +| Last activity +| Advisor +| Notes +|$")
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "Dr. Chen" "Sees me Tue afternoons"))
        (expect (test-gradebook--row "Beta, Bob")
                :to-equal '("Lecture" "70.0" "65.0" "-" "-" "-" "" ""))
        ;; The separator row has one more cell per custom column.
        (expect text :to-match "^|-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+|$")
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
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Missing +| Late +| Last activity +|$")
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-"))
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
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-"))
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
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :to-match "Canvas returned 0 items")))

  (it "is in the pull tiers after people"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-people names)
              :to-be-less-than (cl-position 'org-canvas-pull-gradebook names)))))

;;;; Per-group scores and the what-if (#452)

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
       (cl-letf (((symbol-function 'org-canvas-api-request) #'test-gradebook--request))
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

(provide 'org-canvas-gradebook-test)
;;; org-canvas-gradebook-test.el ends here
