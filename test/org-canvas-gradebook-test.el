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

(provide 'org-canvas-gradebook-test)
;;; org-canvas-gradebook-test.el ends here
