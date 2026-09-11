;;; org-canvas-people-test.el --- Buttercup tests for the roster pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-people': enrollments folded into one heading
;; per person under a role heading in people.org.  Every request is
;; answered by a fake keyed on the URL; nothing here reaches the network
;; (Hard Rule 2), and the log is never read from the shared buffer
;; (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: `org-canvas-sections-file' must be special for
;; `let' to bind it, and the tier list lives in org-canvas.el.
(require 'org-canvas)

(defvar test-people--enrollments nil
  "Enrollments the fake API lists for the course.")
(defvar test-people--sections nil
  "Sections the fake API lists for the course.")

(defun test-people--api (_method url &optional _params)
  "Answer URL from the fake tables, as the paginated helper would."
  (cond
   ((string-match "/enrollments" url) test-people--enrollments)
   ((string-match "/sections\\'" url) test-people--sections)
   (t (error "Unexpected request: %s" url))))

(defun test-people--enrollment (uid name type section &rest extra)
  "An enrollment row for user UID named NAME of TYPE in SECTION, plus EXTRA pairs."
  ;; EXTRA first, so an explicit state or activity shadows the default.
  (append extra
          `((id . ,(+ 1000 uid (* 10 section)))
            (user_id . ,uid) (type . ,type) (role . ,type)
            (course_section_id . ,section) (enrollment_state . "active")
            (user . ((id . ,uid) (name . ,name) (sortable_name . ,name)
                     (sis_user_id . ,(format "sis-%d" uid))
                     (login_id . ,(format "login%d" uid)))))))

(defun test-people--count (regexp text)
  "Return how many times REGEXP matches in TEXT."
  (let ((n 0) (start 0))
    (while (string-match regexp text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defmacro test-people--with-course (enrollments sections &rest body)
  "Run BODY with the fake API serving ENROLLMENTS and SECTIONS into a temp people.org."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "people-" t))
          (org-canvas-people-file (expand-file-name "people.org" dir))
          (org-canvas-sections-file (expand-file-name "sections.org" dir))
          (test-people--enrollments ,enrollments)
          (test-people--sections ,sections))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-people--api)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-people-file org-canvas-sections-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-people--file ()
  "Return people.org's text."
  (with-temp-buffer (insert-file-contents org-canvas-people-file) (buffer-string)))

(describe "org-canvas--people-group"
  (it "folds a person's section enrollments into one plist with every section"
    (let ((people (org-canvas--people-group
                   (list (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 10)
                         (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 20)
                         (test-people--enrollment 2 "Beta, Bob" "StudentEnrollment" 10)))))
      (expect (length people) :to-equal 2)
      (expect (plist-get (car people) :section-ids) :to-equal '(10 20))
      (expect (plist-get (car people) :role) :to-equal "student")
      (expect (plist-get (car people) :heading) :to-equal "Students")))

  (it "takes the higher role when a person holds two, and skips the test student"
    (let ((people (org-canvas--people-group
                   (list (test-people--enrollment 1 "Cruz, Cal" "StudentEnrollment" 10)
                         (test-people--enrollment 1 "Cruz, Cal" "TaEnrollment" 10)
                         (test-people--enrollment 9 "Test Student" "StudentViewEnrollment" 10)
                         (test-people--enrollment 3 "Doe, Dee" "TeacherEnrollment" 10)))))
      ;; Heading order: Teachers before TAs
      (expect (mapcar (lambda (p) (plist-get p :name)) people) :to-equal '("Doe, Dee" "Cruz, Cal"))
      (expect (plist-get (cadr people) :role) :to-equal "ta")
      (expect (plist-get (car people) :heading) :to-equal "Teachers")))

  (it "keeps every distinct state and the latest activity"
    (let ((people (org-canvas--people-group
                   (list (test-people--enrollment 1 "A" "StudentEnrollment" 10
                                                  '(enrollment_state . "active")
                                                  '(last_activity_at . "2026-09-01T10:00:00Z"))
                         (test-people--enrollment 1 "A" "StudentEnrollment" 20
                                                  '(enrollment_state . "inactive")
                                                  '(last_activity_at . "2026-09-05T10:00:00Z"))))))
      (expect (plist-get (car people) :states) :to-equal '("active" "inactive"))
      (expect (plist-get (car people) :last-activity) :to-equal "2026-09-05T10:00:00Z")))

  (it "sorts by name and falls back to a placeholder when the user has none"
    (let ((people (org-canvas--people-group
                   (list (test-people--enrollment 2 "Zed, Zoe" "StudentEnrollment" 10)
                         `((user_id . 7) (type . "StudentEnrollment") (course_section_id . 10)
                           (enrollment_state . "active") (user . ((id . 7))))))))
      (expect (mapcar (lambda (p) (plist-get p :name)) people) :to-equal '("User 7" "Zed, Zoe")))))

(describe "org-canvas-pull-people"
  (it "writes role headings in order with one heading per person and its properties"
    (test-people--with-course
        (list (test-people--enrollment 2 "Beta, Bob" "StudentEnrollment" 10
                                       '(last_activity_at . "2026-09-05T10:00:00Z"))
              (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 10)
              (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 20)
              (test-people--enrollment 3 "Prof, Pat" "TeacherEnrollment" 10))
        '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Recitation")))
      (org-canvas-pull-people)
      (let ((text (test-people--file)))
        (expect text :to-match "^\\* Students\n")
        (expect text :to-match "^\\* Teachers\n")
        (expect (string-match "\\* Students" text) :to-be-less-than (string-match "\\* Teachers" text))
        (expect text :to-match "^\\*\\* Adams, Alice\n")
        (expect (string-match "Adams, Alice" text) :to-be-less-than (string-match "Beta, Bob" text))
        (expect text :to-match ":USER_ID: +1\n")
        (expect text :to-match ":ROLE: +student\n")
        (expect text :to-match ":SECTIONS: +Lecture, Recitation\n")
        (expect text :to-match ":ENROLLMENT_STATE: +active\n")
        (expect text :to-match ":LAST_ACTIVITY: +<2026-09-05")
        (expect text :to-match "^\\*\\* Prof, Pat\n")
        (expect text :to-match ":ROLE: +teacher\n")
        (expect text :not :to-match "SIS_USER_ID")
        (expect text :not :to-match "LOGIN_ID")
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

  (it "links a section to its sections.org heading when that file holds the id"
    (test-people--with-course
        (list (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 10))
        '(((id . 10) (name . "Lecture")))
      (with-temp-file org-canvas-sections-file
        (insert "* Lecture 001\n:PROPERTIES:\n:CANVAS_ID: 10\n:END:\n"))
      (org-canvas-pull-people)
      (expect (test-people--file)
              :to-match ":SECTIONS: +\\[\\[file:sections.org::\\*Lecture 001\\]\\[Lecture\\]\\]")))

  (it "writes the identifiers only when asked"
    (test-people--with-course
        (list (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 10))
        '(((id . 10) (name . "Lecture")))
      (let ((org-canvas-people-include-identifiers t))
        (org-canvas-pull-people))
      (let ((text (test-people--file)))
        (expect text :to-match ":SIS_USER_ID: +sis-1\n")
        (expect text :to-match ":LOGIN_ID: +login1\n"))))

  (it "upserts by USER_ID on a re-pull, keeps a moved person in place, and deletes nothing"
    (test-people--with-course
        (list (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 10)
              (test-people--enrollment 2 "Beta, Bob" "StudentEnrollment" 10))
        '(((id . 10) (name . "Lecture")))
      (org-canvas-pull-people)
      ;; Bob drops, Alice becomes a TA and renames.
      (setq test-people--enrollments
            (list (test-people--enrollment 1 "Adams, Alice B." "TaEnrollment" 10)))
      (org-canvas-pull-people)
      (let ((text (test-people--file)))
        (expect (test-people--count ":USER_ID: +1\n" text) :to-equal 1)
        (expect text :to-match "^\\*\\* Adams, Alice B\\.\n")
        (expect text :to-match ":ROLE: +ta\n")
        (expect text :not :to-match "^\\* TAs\n")
        (expect text :to-match "^\\*\\* Beta, Bob\n"))))

  (it "writes the empty-file note for a course with no one enrolled"
    (test-people--with-course nil nil
      (org-canvas-pull-people)
      (expect (test-people--file) :to-match "Canvas returned 0 items")))

  (it "is in the pull tiers after sections"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-sections names)
              :to-be-less-than (cl-position 'org-canvas-pull-people names)))))

(provide 'org-canvas-people-test)
;;; org-canvas-people-test.el ends here
