;;; org-canvas-gradebook-test.el --- Buttercup tests for the gradebook overview -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-gradebook': student enrollments and the course
;; analytics folded into the two tables of gradebook.org.  Every request
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

(defun test-gradebook--api (_method url &optional _params)
  "Answer URL from the fake tables, as the paginated helper would."
  (cond
   ((string-match "/enrollments" url) test-gradebook--enrollments)
   ((string-match "/sections\\'" url) test-gradebook--sections)
   ((string-match "/analytics/student_summaries" url)
    (if (eq test-gradebook--summaries 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--summaries))
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
          (test-gradebook--enrollments ,enrollments)
          (test-gradebook--sections ,sections)
          (test-gradebook--summaries ,summaries))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-gradebook--api)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-gradebook-file org-canvas-people-file))
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
