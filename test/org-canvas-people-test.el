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

(defvar test-people--history nil
  "Enrollments only a read asking for every state sees: deleted, rejected.")
(defvar test-people--user-reads nil
  "User ids the fake answered a departure read for, most recent first.")

(defun test-people--user-read (uid)
  "Answer the departure read for UID as Canvas does: that user's rows only.
A function in `test-people--history' answers instead, for a failing read."
  (push uid test-people--user-reads)
  (if (functionp test-people--history)
      (funcall test-people--history uid)
    (cl-remove-if-not (lambda (e) (equal (format "%s" (alist-get 'user_id e)) uid))
                      (append test-people--enrollments test-people--history))))

(defun test-people--api (_method url &optional params)
  "Answer URL with PARAMS from the fake tables, as the paginated helper would."
  (cond
   ((and (string-match "/enrollments" url) (assoc "user_id" params))
    (test-people--user-read (cdr (assoc "user_id" params))))
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
          (test-people--sections ,sections)
          (test-people--history nil)
          (test-people--user-reads nil))
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

  (it "falls back to the section id when the course names no such section"
    (test-people--with-course
        (list (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 77))
        '(((id . 10) (name . "Lecture")))
      (org-canvas-pull-people)
      (expect (test-people--file) :to-match ":SECTIONS: +77\n")))

  (it "reports progress every twenty-five people"
    (let ((said nil))
      (test-people--with-course
          (cl-loop for i from 1 to 26
                   collect (test-people--enrollment i (format "Student %02d" i) "StudentEnrollment" 10))
          '(((id . 10) (name . "Lecture")))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-pull-people))
        (expect (cl-some (lambda (m) (string-match-p "People \\[25/26\\]" m)) said) :to-be-truthy)
        (expect (test-people--count "^\\*\\* Student" (test-people--file)) :to-equal 26))))

  (it "writes the empty-file note for a course with no one enrolled"
    (test-people--with-course nil nil
      (org-canvas-pull-people)
      (expect (test-people--file) :to-match "Canvas returned 0 items")))

  (it "is in the pull tiers after sections"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-sections names)
              :to-be-less-than (cl-position 'org-canvas-pull-people names)))))

(defun test-people--said (thunk)
  "Call THUNK and return every message it showed, oldest first."
  (let ((said nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
      (funcall thunk))
    (nreverse said)))

(defun test-people--entry (uid property)
  "Return PROPERTY of the people.org heading carrying USER_ID UID."
  (with-current-buffer (org-canvas--find-file-noselect org-canvas-people-file)
    (let ((pos (org-canvas--people-find-person uid)))
      (and pos (org-entry-get pos property)))))

(defconst test-people--two
  (list (test-people--enrollment 1 "Adams, Alice" "StudentEnrollment" 10)
        (test-people--enrollment 2 "Beta, Bob" "StudentEnrollment" 10))
  "Two students in one section.")

(describe "org-canvas-pull-people departures (issue #290)"
  (it "marks a heading Canvas no longer lists with the state Canvas gives and when"
    (test-people--with-course test-people--two '(((id . 10) (name . "Lecture")))
      (org-canvas-pull-people)
      (setq test-people--enrollments (list (car test-people--two))
            test-people--history
            (list (test-people--enrollment 2 "Beta, Bob" "StudentEnrollment" 10
                                           '(enrollment_state . "deleted")
                                           '(updated_at . "2026-09-14T19:19:00Z"))
                  (test-people--enrollment 2 "Beta, Bob" "StudentEnrollment" 20
                                           '(enrollment_state . "deleted")
                                           '(updated_at . "2026-09-14T19:18:00Z"))))
      (let ((said (test-people--said #'org-canvas-pull-people)))
        (expect (test-people--entry 2 "ENROLLMENT_STATE") :to-equal "deleted")
        (expect (test-people--entry 2 "DEPARTED") :to-match "\\`<2026-09-14")
        (expect (test-people--entry 1 "ENROLLMENT_STATE") :to-equal "active")
        (expect (test-people--entry 1 "DEPARTED") :to-be nil)
        ;; Only the missing heading is looked up.
        (expect test-people--user-reads :to-equal '("2"))
        (expect (car (last said))
                :to-match "1 people (1 students); 1 heading no longer on Canvas: Beta, Bob (deleted 2026-09-14)\\.\\'")
        (expect (test-people--file) :to-match "^\\*\\* Beta, Bob\n"))))

  (it "asks the departure read for the user in every state Canvas has"
    (let ((seen nil))
      (test-people--with-course test-people--two nil
        (org-canvas-pull-people)
        (setq test-people--enrollments (list (car test-people--two)))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (method url &optional params)
                     (when (assoc "user_id" params) (setq seen params))
                     (test-people--api method url params))))
          (org-canvas-pull-people))
        (expect seen :to-contain '("user_id" . "2"))
        (dolist (state '("deleted" "rejected" "completed" "inactive" "active" "invited"))
          (expect seen :to-contain (cons "state[]" state))))))

  (it "marks a heading absent when Canvas has no enrollment at all, and keeps the first date"
    (test-people--with-course test-people--two nil
      (org-canvas-pull-people)
      (setq test-people--enrollments (list (car test-people--two)))
      (let ((said (test-people--said #'org-canvas-pull-people)))
        (expect (test-people--entry 2 "ENROLLMENT_STATE") :to-equal "absent")
        (expect (test-people--entry 2 "DEPARTED")
                :to-match (concat "\\`" (substring (org-canvas--iso8601-to-org-timestamp
                                                     (format-time-string "%FT%TZ" nil t))
                                                    0 11)))
        (expect (car (last said)) :to-match "no longer on Canvas: Beta, Bob (absent [0-9-]+)"))
      ;; A later pull keeps the date the absence was first seen.
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-people-file)
        (org-entry-put (org-canvas--people-find-person 2) "DEPARTED" "<2026-09-01 Tue 08:00>")
        (save-buffer))
      (org-canvas-pull-people)
      (expect (test-people--entry 2 "DEPARTED") :to-match "\\`<2026-09-01")))

  (it "leaves a heading alone when the departure read still finds it enrolled"
    (test-people--with-course test-people--two nil
      (org-canvas-pull-people)
      ;; The roster read misses Bob; his own read says he is active.
      (setq test-people--history (lambda (_uid) (list (cadr test-people--two)))
            test-people--enrollments (list (car test-people--two)))
      (let ((said (test-people--said #'org-canvas-pull-people)))
        (expect (test-people--entry 2 "ENROLLMENT_STATE") :to-equal "active")
        (expect (test-people--entry 2 "DEPARTED") :to-be nil)
        (expect (car (last said)) :to-match "1 heading not in the roster left unchanged, still enrolled or unreadable: Beta, Bob\\.\\'")
        (expect (car (last said)) :not :to-match "no longer on Canvas"))))

  (it "leaves a heading alone when its departure read fails"
    (let ((warned nil))
      (test-people--with-course test-people--two nil
        (org-canvas-pull-people)
        (setq test-people--history (lambda (_uid) (signal 'org-canvas-api-error '("HTTP 500")))
              test-people--enrollments (list (car test-people--two)))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-people))
        (expect (test-people--entry 2 "ENROLLMENT_STATE") :to-equal "active")
        (expect (cl-some (lambda (w) (string-match-p "Could not read the enrollments of user 2" w)) warned)
                :to-be-truthy))))

  (it "clears the departure when the person is listed again"
    (test-people--with-course test-people--two nil
      (org-canvas-pull-people)
      (setq test-people--enrollments (list (car test-people--two)))
      (org-canvas-pull-people)
      (expect (test-people--entry 2 "DEPARTED") :to-be-truthy)
      (setq test-people--enrollments test-people--two)
      (let ((said (test-people--said #'org-canvas-pull-people)))
        (expect (test-people--entry 2 "ENROLLMENT_STATE") :to-equal "active")
        (expect (test-people--entry 2 "DEPARTED") :to-be nil)
        (expect (car (last said)) :to-match "2 people (2 students)\\.\\'"))))

  (it "leaves an existing file alone when the roster read comes back empty"
    (test-people--with-course test-people--two nil
      (org-canvas-pull-people)
      (let ((before (test-people--file)))
        (setq test-people--enrollments [])
        (let ((said (test-people--said #'org-canvas-pull-people)))
          (expect (test-people--file) :to-equal before)
          (expect test-people--user-reads :to-be nil)
          (expect (car (last said)) :to-match "Canvas listed no one; people.org left as it was"))))))

(describe "org-canvas--people-departure"
  (it "counts only the user's own rows in a role the roster lists"
    (let ((verdict (org-canvas--people-departure
                    2 (list (test-people--enrollment 3 "Other" "StudentEnrollment" 10)
                            (test-people--enrollment 2 "Beta, Bob" "StudentViewEnrollment" 10)))))
      (expect (plist-get verdict :status) :to-be 'absent)))

  (it "gives no time when Canvas gave none, and the summary shows the state alone"
    (let ((verdict (org-canvas--people-departure
                    "2" (list (test-people--enrollment 2 "B" "TaEnrollment" 10
                                                       '(enrollment_state . "rejected")
                                                       '(updated_at . nil))))))
      (expect (plist-get verdict :states) :to-equal '("rejected"))
      (expect (plist-get verdict :at) :to-be nil)
      (expect (org-canvas--people-departure-summary
               (list :departed '(("B" "rejected" nil) ("C" "deleted" "2026-09-14"))))
              :to-equal "; 2 headings no longer on Canvas: B (rejected); C (deleted 2026-09-14)")))

  (it "is unverified when a row carries no state to go on"
    (let ((verdict (org-canvas--people-departure
                    2 (list `((user_id . 2) (type . "StudentEnrollment"))))))
      (expect (plist-get verdict :status) :to-be 'unverified)))

  (it "is unverified when an enrollment is pending account creation"
    (let ((verdict (org-canvas--people-departure
                    2 (list (test-people--enrollment 2 "B" "StudentEnrollment" 10
                                                     '(enrollment_state . "creation_pending"))
                            (test-people--enrollment 2 "B" "StudentEnrollment" 11
                                                     '(enrollment_state . "deleted"))))))
      (expect (plist-get verdict :status) :to-be 'unverified)
      (expect (org-canvas--people-departed-state-p "creation_pending") :to-be nil)))

  (it "says nothing when there were no departures"
    (expect (org-canvas--people-departure-summary nil) :to-equal "")))

(describe "org-canvas--people-departed-state-p"
  (it "calls a state departed only when none of its parts is current"
    (expect (org-canvas--people-departed-state-p "deleted") :to-be-truthy)
    (expect (org-canvas--people-departed-state-p "absent") :to-be-truthy)
    (expect (org-canvas--people-departed-state-p "completed, inactive") :to-be-truthy)
    (expect (org-canvas--people-departed-state-p "active") :to-be nil)
    (expect (org-canvas--people-departed-state-p "inactive, active") :to-be nil)
    (expect (org-canvas--people-departed-state-p "") :to-be nil)
    (expect (org-canvas--people-departed-state-p nil) :to-be nil)))

(provide 'org-canvas-people-test)
;;; org-canvas-people-test.el ends here
