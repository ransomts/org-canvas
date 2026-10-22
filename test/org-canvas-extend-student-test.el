;;; org-canvas-extend-student-test.el --- Tests for one student's new deadline  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-extend-student' and `org-canvas-extend-student-named'
;; (issue #526): write one `Students:' row into an assignment's
;; overrides table and send that override alone.  Every request goes
;; through a stand-in for `org-canvas-api-request'.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-sections)

;; Defined by modules this file does not require; declare them special.
(defvar org-canvas-people-file)
(defvar org-canvas-assignments-file)

(defconst test-extend--people
  (concat "* Students\n"
          "** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:ROLE: student\n"
          ":SECTIONS: [[file:sections.org::*Section A][Section A]]\n:END:\n"
          "** Beta, Bob\n:PROPERTIES:\n:USER_ID: 2\n:ROLE: student\n:END:\n"
          "* Teachers\n"
          "** Ransom, Tim\n:PROPERTIES:\n:USER_ID: 9\n:ROLE: teacher\n:END:\n")
  "A roster: two students, Alice in Section A, and a teacher.")

(defconst test-extend--table
  (concat "#+NAME: overrides\n"
          "| Section | Due At | Unlock At | Lock At |\n"
          "|---------+--------+-----------+---------|\n"
          "| [[file:sections.org::*Section A][Section A]] | <2026-10-20 Tue 16:00> "
          "| <2026-10-16 Fri 09:05> | <2026-10-22 Thu 16:00> |\n")
  "An overrides table holding Section A's row.")

(defun test-extend--assignment (&optional table extra)
  "Return assignments.org text: Lab 1 (CANVAS_ID 456), then TABLE.
EXTRA is spliced into the drawer."
  (concat "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 456\n"
          ":CANVAS_UPDATED_AT: 2026-10-20T10:00:00Z\n:PAYLOAD_HASH: abc\n"
          (or extra "") ":END:\n\nDo the lab.\n\n" (or table "")))

(defun test-extend--time (day hour)
  "Return 2026-10-DAY at HOUR:00 local time as a Lisp time value."
  (encode-time (list 0 0 hour day 10 2026 nil -1 nil)))

(defmacro test-extend--with-course (assignment &rest body)
  "Run BODY in assignments.org holding ASSIGNMENT, point at its start.
people.org holds `test-extend--people' and sections.org Section A
\(CANVAS_ID 100).  Everything is deleted afterwards."
  (declare (indent 1))
  `(let* ((dir (file-truename (make-temp-file "extend-" t)))
          (org-canvas-people-file (expand-file-name "people.org" dir))
          (org-canvas-sections-file (expand-file-name "sections.org" dir))
          (org-canvas-assignments-file (expand-file-name "assignments.org" dir)))
     (with-temp-file org-canvas-people-file (insert test-extend--people))
     (with-temp-file org-canvas-sections-file
       (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
     (with-temp-file org-canvas-assignments-file (insert ,assignment))
     (unwind-protect
         (with-org-canvas-test-config
           (with-sync-test-env
             (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
               (with-current-buffer (org-canvas--find-file-noselect
                                     org-canvas-assignments-file)
                 (goto-char (point-min))
                 ,@body))))
       (dolist (f (list org-canvas-people-file org-canvas-sections-file
                        org-canvas-assignments-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory dir t))))

(defvar test-extend--calls nil
  "The (METHOD URL DATA) requests the stand-in API saw, latest first.")

(defun test-extend--api (overrides &optional before fail)
  "Return a stand-in for `org-canvas-api-request'.
The overrides endpoint answers OVERRIDES; the assignment answers
`updated_at' BEFORE (default the fixture's stamp) until a write and
2026-10-22T12:00:00Z after.  FAIL non-nil makes every write signal."
  (let ((written nil))
    (lambda (method url &rest args)
      (push (list method url (plist-get args :data)) test-extend--calls)
      (cond ((and fail (not (eq method 'GET))) (error "HTTP 400"))
            ((not (eq method 'GET)) (setq written t) '((id . 77)))
            ((string-match-p "/overrides" url) overrides)
            (t `((id . 456)
                 (updated_at . ,(if written "2026-10-22T12:00:00Z"
                                  (or before "2026-10-20T10:00:00Z")))))))))

(defmacro test-extend--with-api (api &rest body)
  "Run BODY with API standing in for `org-canvas-api-request'."
  (declare (indent 1))
  `(let ((test-extend--calls nil))
     (cl-letf (((symbol-function 'org-canvas-api-request) ,api))
       ,@body)))

(defun test-extend--writes ()
  "Return the non-GET requests among `test-extend--calls', oldest first."
  (reverse (cl-remove-if (lambda (c) (eq (car c) 'GET)) test-extend--calls)))

(defun test-extend--alice ()
  "Return Alice's entry of the roster."
  (org-canvas--extend-find-student (org-canvas--extend-students) "Adams, Alice"))

(defun test-extend--run (&optional lock)
  "Extend Alice on the heading at point: due Oct 23 16:00, LOCK or Oct 25."
  (org-canvas--extend-student-here
   (test-extend--alice) (test-extend--time 23 16)
   (or lock (test-extend--time 25 16))))

;;;; The roster

(describe "org-canvas--extend-students (issue #526)"
  (it "lists people.org's students, with their ids and sections"
    (test-extend--with-course (test-extend--assignment)
      (let ((students (org-canvas--extend-students)))
        (expect (mapcar #'car students) :to-equal '("Adams, Alice" "Beta, Bob"))
        (expect (nth 1 (car students)) :to-equal "1")
        (expect (nth 2 (car students)) :to-match "Section A"))))

  (it "says to pull the roster when people.org is missing or names no student"
    (let ((org-canvas-people-file "/nonexistent/people-xyzzy.org"))
      (expect (org-canvas--extend-students) :to-throw 'user-error))
    (test-extend--with-course (test-extend--assignment)
      (with-temp-file org-canvas-people-file (insert "* Teachers\n"))
      (expect (org-canvas--extend-students) :to-throw 'user-error)))

  (it "finds a student by title or #USER_ID, and refuses anyone else"
    (test-extend--with-course (test-extend--assignment)
      (let ((students (org-canvas--extend-students)))
        (expect (car (org-canvas--extend-find-student students "#2"))
                :to-equal "Beta, Bob")
        (expect (nth 1 (org-canvas--extend-find-student students "Adams, Alice"))
                :to-equal "1")
        (expect (org-canvas--extend-find-student students "Ransom, Tim")
                :to-throw 'user-error)))))

;;;; Writing the row and sending the override

(describe "org-canvas--extend-student-here (issue #526)"
  (it "writes the row, POSTs the nested payload alone, and restamps"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [])
        (expect (test-extend--run) :to-equal 'created)
        (let* ((writes (test-extend--writes))
               (post (car writes))
               (body (alist-get 'assignment_override (nth 2 post))))
          (expect (length writes) :to-equal 1)
          (expect (car post) :to-equal 'POST)
          (expect (nth 1 post) :to-match "assignments/456/overrides\\'")
          (expect (alist-get 'student_ids body) :to-equal [1])
          (expect (alist-get 'due_at body)
                  :to-equal (org-canvas-org-parse-timestamp "<2026-10-23 Fri 16:00>"))
          ;; The unlock is Section A's, the student's section.
          (expect (alist-get 'unlock_at body)
                  :to-equal (org-canvas-org-parse-timestamp "<2026-10-16 Fri 09:05>"))
          (expect (alist-get 'lock_at body)
                  :to-equal (org-canvas-org-parse-timestamp "<2026-10-25 Sun 16:00>")))
        (let ((text (with-temp-buffer
                      (insert-file-contents org-canvas-assignments-file)
                      (buffer-string))))
          (expect text :to-match
                  "| Students: Adams, Alice +| <2026-10-23 Fri 16:00> | <2026-10-16 Fri 09:05> | <2026-10-25 Sun 16:00> |")
          (expect text :to-match "| \\[\\[file:sections.org::\\*Section A")
          (expect text :to-match ":CANVAS_UPDATED_AT: 2026-10-22T12:00:00Z")
          (expect text :to-match ":PAYLOAD_HASH: abc")))))

  (it "parses back to the override it sent"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [])
        (test-extend--run)
        (let* ((rows (org-canvas--override-parse-table
                      (save-excursion
                        (org-canvas--override-find-table (point-max)))
                      (file-name-directory org-canvas-assignments-file)))
               (alice (cl-find-if (lambda (o) (plist-get o :student-ids)) rows)))
          (expect (length rows) :to-equal 2)
          (expect (plist-get alice :student-ids) :to-equal '("1"))))))

  (it "updates the override Canvas holds for that student, replacing their row"
    (test-extend--with-course
        (test-extend--assignment
         (concat test-extend--table
                 "| Students: Adams, Alice | <2026-10-21 Wed 16:00> "
                 "| <2026-10-17 Sat 08:00> | |\n"))
      (test-extend--with-api (test-extend--api [((id . 31) (student_ids . [1])
                                                 (due_at . "2026-10-21T20:00:00Z"))
                                                ((id . 32) (student_ids . [2])
                                                 (due_at . "2026-10-21T20:00:00Z"))])
        (expect (test-extend--run) :to-equal 'updated)
        (let ((writes (test-extend--writes)))
          (expect (mapcar #'car writes) :to-equal '(PUT))
          (expect (nth 1 (car writes)) :to-match "overrides/31\\'")
          ;; The student's own row's unlock is kept.
          (expect (alist-get 'unlock_at
                             (alist-get 'assignment_override (nth 2 (car writes))))
                  :to-equal (org-canvas-org-parse-timestamp "<2026-10-17 Sat 08:00>")))
        (let ((text (buffer-string)))
          (expect (with-temp-buffer
                    (insert text)
                    (how-many "Students: Adams, Alice" (point-min) (point-max)))
                  :to-equal 1)
          (expect text :to-match "<2026-10-23 Fri 16:00>")
          (expect text :not :to-match "<2026-10-21 Wed 16:00>")))))

  (it "sends nothing when Canvas already holds those dates"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (let ((due (org-canvas-org-parse-timestamp "<2026-10-23 Fri 16:00>"))
            (unlock (org-canvas-org-parse-timestamp "<2026-10-16 Fri 09:05>"))
            (lock (org-canvas-org-parse-timestamp "<2026-10-25 Sun 16:00>")))
        (test-extend--with-api (test-extend--api
                                (vector `((id . 31) (student_ids . [1]) (due_at . ,due)
                                          (unlock_at . ,unlock) (lock_at . ,lock))))
          (expect (test-extend--run) :to-equal 'unchanged)
          (expect (test-extend--writes) :to-equal nil)
          (expect (buffer-string) :to-match ":CANVAS_UPDATED_AT: 2026-10-20T10:00:00Z")))))

  (it "refuses a student Canvas holds in an override with others"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [((id . 40) (student_ids . [1 2]))])
        (expect (test-extend--run) :to-throw 'user-error)
        (expect (test-extend--writes) :to-equal nil))))

  (it "refuses a student whose table row names others, before writing anything"
    (let ((text (test-extend--assignment
                 (concat test-extend--table
                         "| Students: Adams, Alice; Beta, Bob | <2026-10-21 Wed 16:00> | | |\n"))))
      (test-extend--with-course text
        (test-extend--with-api (test-extend--api [])
          (expect (test-extend--run) :to-throw 'user-error)
          (expect test-extend--calls :to-equal nil)
          (expect (buffer-modified-p) :to-be nil)))))

  (it "starts a table under a heading with none, the unlock its UNLOCK_AT"
    (test-extend--with-course
        (concat (test-extend--assignment nil ":UNLOCK_AT: <2026-10-10 Sat 08:00>\n")
                "** Notes\nA child heading.\n")
      (test-extend--with-api (test-extend--api [])
        (test-extend--run)
        (let ((text (buffer-string)))
          (expect text :to-match
                  "Do the lab\\.\n\n#\\+NAME: overrides\n| Section +| Due At +| Unlock At +| Lock At +|\n|-")
          (expect text :to-match "<2026-10-10 Sat 08:00> | <2026-10-25 Sun 16:00> |\n\\*\\* Notes")))))

  (it "puts a blank line before a new table at the end of the file"
    (test-extend--with-course "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 456\n:END:\nText"
      (test-extend--with-api (test-extend--api [])
        (test-extend--run)
        (expect (buffer-string) :to-match "Text\n\n#\\+NAME: overrides\n"))))

  (it "adds the date columns a pulled table dropped"
    (test-extend--with-course
        (test-extend--assignment
         (concat "#+NAME: overrides\n| Section | Due At |\n|---+---|\n"
                 "| [[file:sections.org::*Section A][Section A]] | <2026-10-20 Tue 16:00> |\n"))
      (test-extend--with-api (test-extend--api [])
        (test-extend--run)
        (let* ((table (save-excursion (org-canvas--override-find-table (point-max))))
               (header (mapcar #'string-trim (car table))))
          (expect header :to-equal '("Section" "Due At" "Unlock At" "Lock At"))
          (expect (string-trim (nth 3 (car (last table))))
                  :to-equal "<2026-10-25 Sun 16:00>")
          (expect (string-trim (nth 2 (nth 2 table))) :to-equal "")))))

  (it "leaves the lock empty when none is given"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [])
        (org-canvas--extend-student-here (test-extend--alice)
                                         (test-extend--time 23 16) nil)
        (expect (alist-get 'lock_at
                           (alist-get 'assignment_override
                                      (nth 2 (car (test-extend--writes)))))
                :to-be nil))))

  (it "keeps the stamp of a heading that had drifted from Canvas"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [] "2026-10-21T09:00:00Z")
        (expect (test-extend--run) :to-equal 'created)
        (expect (buffer-string) :to-match ":CANVAS_UPDATED_AT: 2026-10-20T10:00:00Z"))))

  (it "keeps the row and says so when the write fails"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [] nil t)
        (cl-letf (((symbol-function 'org-canvas--log-error) #'ignore))
          (expect (test-extend--run) :to-be nil))
        (expect (buffer-string) :to-match "Students: Adams, Alice")
        (expect (buffer-string) :to-match ":CANVAS_UPDATED_AT: 2026-10-20T10:00:00Z"))))

  (it "signals rather than create a twin when the overrides cannot be read"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (lambda (method url &rest _)
                               (push (list method url) test-extend--calls)
                               (if (string-match-p "/overrides" url)
                                   (error "HTTP 500")
                                 '((id . 456) (updated_at . "2026-10-20T10:00:00Z"))))
        (expect (test-extend--run) :to-throw 'error)
        (expect (test-extend--writes) :to-equal nil))))

  (it "previews under a dry run, writing no row and sending nothing"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (let ((before (buffer-string))
            (logged nil))
        (test-extend--with-api (test-extend--api [])
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged))))
            (let ((org-canvas--dry-run t))
              (expect (test-extend--run) :to-equal 'dry-run)))
          (expect (test-extend--writes) :to-equal nil)
          (expect (buffer-string) :to-equal before)
          (expect (cl-some (lambda (l) (string-match-p "Would CREATE override" l)) logged)
                  :to-be-truthy)))))

  (it "refuses on a read-only course before writing the row"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (let ((before (buffer-string)))
        (test-extend--with-api (test-extend--api [])
          (let ((org-canvas-read-only t))
            (expect (test-extend--run) :to-throw 'org-canvas-read-only-error))
          (expect test-extend--calls :to-equal nil)
          (expect (buffer-string) :to-equal before)))))

  (it "climbs from a child heading to its assignment"
    (test-extend--with-course
        (concat (test-extend--assignment) "** Notes\nA child heading.\n")
      (test-extend--with-api (test-extend--api [])
        (goto-char (point-max))
        (expect (org-canvas--extend-heading) :to-equal '("456" . "Lab 1")))))

  (it "refuses a heading with no CANVAS_ID, and a buffer not in Org"
    (test-extend--with-course "* Draft\nNo id yet.\n"
      (expect (org-canvas--extend-heading) :to-throw 'user-error))
    (with-temp-buffer
      (expect (org-canvas--extend-heading) :to-throw 'user-error))))

(describe "org-canvas--extend-report (issue #526)"
  (it "names each outcome"
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) said)))
                ((symbol-function 'org-canvas--log-info) #'ignore))
        (dolist (outcome '(created updated unchanged nil))
          (org-canvas--extend-report '("Adams, Alice" "1") "Lab 1"
                                     '("<due>" nil nil) outcome)))
      (expect (reverse said)
              :to-equal
              '("Adams, Alice on 'Lab 1': due <due>, lock none; override created."
                "Adams, Alice on 'Lab 1': due <due>, lock none; override updated."
                "Adams, Alice on 'Lab 1': due <due>, lock none; Canvas already held these dates, nothing sent."
                "Adams, Alice on 'Lab 1': due <due>, lock none; row written but the override was not sent, see the log.")))))

;;;; The two entry points

(describe "org-canvas-extend-student (issue #526)"
  (it "asks for a student and two times, the lock offered late-hours after the due"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (let ((asked nil) (got nil)
            (org-canvas-extend-student-late-hours 24))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (push collection asked) "Beta, Bob"))
                  ((symbol-function 'org-read-date)
                   (lambda (&rest args)
                     (push args asked)
                     (if (nth 5 args) (test-extend--time 24 9) (test-extend--time 23 16))))
                  ((symbol-function 'org-canvas--extend-student-here)
                   (lambda (&rest args) (setq got args) 'created)))
          (expect (org-canvas-extend-student) :to-equal 'created))
        (expect (car (nth 0 got)) :to-equal "Beta, Bob")
        (expect (nth 2 got) :to-equal (test-extend--time 24 9))
        ;; The lock prompt's default input is the due time plus 24 hours.
        (expect (nth 5 (car asked)) :to-equal "2026-10-24 16:00")
        (expect (car (last asked)) :to-equal '("Adams, Alice" "Beta, Bob"))))))

(describe "org-canvas-extend-student-named (issue #526)"
  (it "finds the assignment by title and the student by name, locking late-hours on"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [])
        (expect (org-canvas-extend-student-named "Lab 1" "Adams, Alice"
                                                 "2026-10-23 16:00")
                :to-equal 'created)
        (let ((body (alist-get 'assignment_override
                               (nth 2 (car (test-extend--writes))))))
          (expect (alist-get 'lock_at body)
                  :to-equal (org-canvas-org-parse-timestamp "<2026-10-25 Sun 16:00>"))))))

  (it "takes #CANVAS_ID, #USER_ID and a lock"
    (test-extend--with-course (test-extend--assignment test-extend--table)
      (test-extend--with-api (test-extend--api [])
        (expect (org-canvas-extend-student-named "#456" "#2" "2026-10-23 16:00"
                                                 "2026-10-23 18:00")
                :to-equal 'created)
        (let ((body (alist-get 'assignment_override
                               (nth 2 (car (test-extend--writes))))))
          (expect (alist-get 'student_ids body) :to-equal [2])
          (expect (alist-get 'lock_at body)
                  :to-equal (org-canvas-org-parse-timestamp "<2026-10-23 Fri 18:00>"))))))

  (it "refuses a course with no assignments.org, and an unknown assignment"
    (let ((org-canvas-assignments-file "/nonexistent/assignments-xyzzy.org"))
      (expect (org-canvas-extend-student-named "Lab 1" "#1" "2026-10-23 16:00")
              :to-throw 'user-error))
    (test-extend--with-course (test-extend--assignment)
      (expect (org-canvas-extend-student-named "Lab 9" "#1" "2026-10-23 16:00")
              :to-throw 'user-error))))

(provide 'org-canvas-extend-student-test)
;;; org-canvas-extend-student-test.el ends here
