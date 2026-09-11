;;; org-canvas-messages-test.el --- Buttercup tests for sending messages -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-messages': TO targets resolved through the
;; pulled files, the conversations payload, the stamp after a send, the
;; never-resend rule, the confirmation, the dry run and the read-only
;; refusal.  Every request is answered by a fake keyed on the URL;
;; nothing here reaches the network (Hard Rule 2), and the log is never
;; read from the shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: the file variables of the pulled files must be
;; special for `let' to bind them, and validate names the resolver.
(require 'org-canvas)

(defvar test-messages--posts nil
  "The (URL . DATA) pairs the fake API received as POSTs.")
(defvar test-messages--reply nil
  "What the fake API answers a POST with.")
(defvar test-messages--self-calls 0
  "How many times the fake API was asked for /users/self.")

(defun test-messages--api (method url &rest args)
  "Answer URL for METHOD from the fake tables; ARGS carry :data."
  (cond
   ((string-match "/users/self\\'" url)
    (cl-incf test-messages--self-calls)
    '((id . 77) (name . "Me")))
   ((and (eq method 'POST) (string-match "/conversations\\'" url))
    (push (cons url (plist-get args :data)) test-messages--posts)
    test-messages--reply)
   (t (error "Unexpected request: %s %s" method url))))

(defconst test-messages--people
  "* Students
** Adams, Alice
:PROPERTIES:
:USER_ID: 11
:END:
** Beta, Bob
:PROPERTIES:
:USER_ID: 22
:END:
"
  "A roster with two students.")

(defconst test-messages--sections
  "* Lecture 001
:PROPERTIES:
:CANVAS_ID: 501
:END:
"
  "One section.")

(defconst test-messages--groups
  "* Teams
** Team A
:PROPERTIES:
:CANVAS_ID: 901
:END:
"
  "One group category with one group.")

(defmacro test-messages--with-course (messages &rest body)
  "Run BODY with MESSAGES as messages.org beside a roster, a section and a group."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "messages-" t))
          (org-canvas-messages-file (expand-file-name "messages.org" dir))
          (org-canvas-people-file (expand-file-name "people.org" dir))
          (org-canvas-sections-file (expand-file-name "sections.org" dir))
          (org-canvas-groups-file (expand-file-name "groups.org" dir))
          (test-messages--posts nil)
          (test-messages--reply (vector '((id . 5001)) '((id . 5002))))
          (test-messages--self-calls 0))
     (with-temp-file org-canvas-messages-file (insert ,messages))
     (with-temp-file org-canvas-people-file (insert test-messages--people))
     (with-temp-file org-canvas-sections-file (insert test-messages--sections))
     (with-temp-file org-canvas-groups-file (insert test-messages--groups))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request) #'test-messages--api)
                     ((symbol-function 'message) #'ignore)
                     ((symbol-function 'display-buffer) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-messages-file org-canvas-people-file
                        org-canvas-sections-file org-canvas-groups-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-messages--file ()
  "Return messages.org's text."
  (with-temp-buffer (insert-file-contents org-canvas-messages-file) (buffer-string)))

(defun test-messages--heading (n)
  "Return the properties and body of the Nth message heading as one string."
  (nth n (split-string (test-messages--file) "^\\* " t)))

(defconst test-messages--unsent
  "* Reminder
:PROPERTIES:
:TO: Students: Adams, Alice; Beta, Bob
:END:
Proposals are due Friday.
"
  "One unsent message to two students.")

(describe "org-canvas--message-resolve-target"
  (it "resolves each kind of TO through the pulled files"
    (test-messages--with-course ""
      (expect (org-canvas--message-resolve-target "Students: Adams, Alice; Beta, Bob")
              :to-equal '(:recipients ("11" "22") :label "2 students"))
      (expect (org-canvas--message-resolve-target "Student: #33")
              :to-equal '(:recipients ("33") :label "1 student"))
      (expect (org-canvas--message-resolve-target "Section: Lecture 001")
              :to-equal '(:recipients ("section_501") :label "section 'Lecture 001'"))
      (expect (org-canvas--message-resolve-target "Group: Team A")
              :to-equal '(:recipients ("group_901") :label "group 'Team A'"))
      (expect (org-canvas--message-resolve-target "Group: #7")
              :to-equal '(:recipients ("group_7") :label "group '#7'"))
      (expect (org-canvas--message-resolve-target "Course")
              :to-equal (list :recipients (list (format "course_%s_students" org-canvas-course-id))
                              :label "every student in the course"))))

  (it "asks Canvas for Self once per run, and not at all offline"
    (test-messages--with-course ""
      (let ((org-canvas--message-self-id nil))
        (expect (org-canvas--message-resolve-target "Self")
                :to-equal '(:recipients ("77") :label "yourself"))
        (org-canvas--message-resolve-target "Self")
        (expect test-messages--self-calls :to-equal 1))
      (expect (org-canvas--message-resolve-target "Self" t)
              :to-equal '(:recipients ("self") :label "yourself"))
      (expect test-messages--self-calls :to-equal 1)))

  (it "names what did not resolve, all or nothing for students"
    (test-messages--with-course ""
      (expect (org-canvas--message-resolve-target "Students: Adams, Alice; Nobody, Ned")
              :to-equal '(:unresolved ("Nobody, Ned") :kind students))
      (expect (org-canvas--message-resolve-target "Students:")
              :to-equal '(:unresolved ("nobody") :kind students))
      (expect (org-canvas--message-resolve-target "Section: Lab 9")
              :to-equal '(:unresolved ("Lab 9") :kind section))
      (expect (org-canvas--message-resolve-target "Group: Team Z")
              :to-equal '(:unresolved ("Team Z") :kind group))
      (expect (org-canvas--message-resolve-target "everyone") :to-equal '(:invalid t))
      (expect (org-canvas--message-resolve-target nil) :to-equal '(:invalid t))))

  (it "resolves to nothing when the pulled files are absent"
    (with-nonexistent-canvas-files
      (expect (org-canvas--message-resolve-target "Students: Adams, Alice")
              :to-equal '(:unresolved ("Adams, Alice") :kind students))
      (expect (org-canvas--message-unresolved-advice 'students) :to-match "pull people")
      (expect (org-canvas--message-unresolved-advice 'section) :to-match "pull sections")
      (expect (org-canvas--message-unresolved-advice 'group) :to-match "pull groups")
      (expect (org-canvas--message-unresolved-advice 'self) :to-match "token"))))

(describe "org-canvas--message-build-payload"
  (it "sends one private conversation per recipient by default, or one group thread"
    (with-org-canvas-test-config
      (let* ((entry (list :subject "Hi" :body "Text" :bulk t))
             (target (list :recipients '("11" "22") :label "2 students"))
             (payload (org-canvas--message-build-payload entry target))
             (json (json-encode payload)))
        (expect (alist-get 'recipients payload) :to-equal '("11" "22"))
        (expect json :to-match "\"recipients\":\\[\"11\",\"22\"\\]")
        (expect (alist-get 'bulk_message payload) :to-be t)
        (expect (alist-get 'group_conversation payload) :to-be :json-false)
        (expect (alist-get 'force_new payload) :to-be t)
        (expect (alist-get 'context_code payload)
                :to-equal (format "course_%s" org-canvas-course-id))
        (expect (alist-get 'subject payload) :to-equal "Hi")
        (expect (alist-get 'body payload) :to-equal "Text"))
      (let ((payload (org-canvas--message-build-payload
                      (list :subject "Hi" :body "Text" :bulk nil)
                      (list :recipients '("section_5")))))
        (expect (alist-get 'bulk_message payload) :to-be :json-false)
        (expect (alist-get 'group_conversation payload) :to-be t)))))

(describe "org-canvas-send-messages"
  (it "sends the unsent heading, stamps it and saves the file"
    (test-messages--with-course test-messages--unsent
      (org-canvas-send-messages)
      (expect (length test-messages--posts) :to-equal 1)
      (let ((data (cdar test-messages--posts)))
        (expect (alist-get 'recipients data) :to-equal '("11" "22"))
        (expect (alist-get 'subject data) :to-equal "Reminder")
        (expect (alist-get 'body data) :to-equal "Proposals are due Friday."))
      (let ((text (test-messages--file)))
        (expect text :to-match ":SENT_AT: +<20")
        (expect text :to-match ":RECIPIENTS: +11, 22\n")
        (expect text :to-match ":CONVERSATION_IDS: +5001, 5002\n")
        (expect text :to-match (concat ":PAYLOAD_HASH: +" (md5 "Proposals are due Friday.") "\n"))
        (expect text :to-match "Proposals are due Friday\\.\n"))))

  (it "never sends a heading that carries SENT_AT, and says so"
    (test-messages--with-course
        "* Old news
:PROPERTIES:
:TO: Course
:SENT_AT: <2026-09-01 Tue 09:00>
:END:
Already went out.
"
      (let ((lines nil))
        (cl-letf (((symbol-function 'org-canvas--log-info)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) lines))))
          (org-canvas-send-messages))
        (expect test-messages--posts :to-be nil)
        (expect (cl-some (lambda (l) (string-match-p "already sent" l)) lines) :to-be-truthy)
        (expect (test-messages--file) :not :to-match "RECIPIENTS"))))

  (it "leaves Canvas untouched under the dry run and logs who would get it"
    (test-messages--with-course test-messages--unsent
      (let ((lines nil) (before (test-messages--file)))
        (cl-letf (((symbol-function 'org-canvas--log-info)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) lines))))
          (org-canvas-send-messages t))
        (expect test-messages--posts :to-be nil)
        (expect (cl-some (lambda (l) (string-match-p "\\[DRY-RUN\\] Would send 'Reminder' to 2 students (11, 22)" l)) lines)
                :to-be-truthy)
        (expect (test-messages--file) :to-equal before))))

  (it "asks once interactively, naming every message, and sends nothing on no"
    (test-messages--with-course test-messages--unsent
      (let ((prompts nil) (noninteractive nil) (org-canvas-assume-yes nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (push prompt prompts) nil)))
          (expect (org-canvas-send-messages) :to-throw 'user-error))
        (expect (length prompts) :to-equal 1)
        (expect (car prompts) :to-match "Send 1 message: 'Reminder' to 2 students\\?")
        (expect test-messages--posts :to-be nil))
      (let ((noninteractive nil) (org-canvas-assume-yes nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
          (org-canvas-send-messages))
        (expect (length test-messages--posts) :to-equal 1))))

  (it "skips a heading it cannot send and reports the rest"
    (test-messages--with-course
        "* No body
:PROPERTIES:
:TO: Course
:END:
* Bad target
:PROPERTIES:
:TO: everyone
:END:
Text.
* Unknown student
:PROPERTIES:
:TO: Students: Nobody, Ned
:END:
Text.
* Fine
:PROPERTIES:
:TO: Group: Team A
:END:
Text.
"
      (let ((errors nil) (warnings nil))
        (cl-letf (((symbol-function 'org-canvas--log-error)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) errors)))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
          (org-canvas-send-messages))
        (expect (length test-messages--posts) :to-equal 1)
        (expect (alist-get 'recipients (cdar test-messages--posts)) :to-equal '("group_901"))
        (expect (cl-some (lambda (l) (string-match-p "'No body' has no body" l)) errors) :to-be-truthy)
        (expect (cl-some (lambda (l) (string-match-p "'Bad target': TO must be" l)) errors) :to-be-truthy)
        (expect (cl-some (lambda (l) (string-match-p "'Nobody, Ned' (pull people first" l)) warnings)
                :to-be-truthy))))

  (it "counts a failed send and goes on"
    (test-messages--with-course
        (concat test-messages--unsent "* Second\n:PROPERTIES:\n:TO: Self\n:END:\nAgain.\n")
      (let ((errors nil) (said nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (if (and (eq method 'POST) (equal (alist-get 'subject (plist-get args :data)) "Reminder"))
                         (signal 'org-canvas-api-error '("Canvas said no"))
                       (apply #'test-messages--api method url args))))
                  ((symbol-function 'org-canvas--log-error)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) errors)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-send-messages))
        (expect (cl-some (lambda (l) (string-match-p "Failed to send 'Reminder'" l)) errors) :to-be-truthy)
        (expect (car said) :to-equal "Messages: 1 sent, 0 previewed, 1 failed")
        (expect (test-messages--heading 1) :to-match ":RECIPIENTS: +77\n")
        (expect (test-messages--heading 0) :not :to-match "SENT_AT"))))

  (it "stamps no conversation ids when Canvas answers with none"
    (test-messages--with-course test-messages--unsent
      (setq test-messages--reply [])
      (org-canvas-send-messages)
      (let ((text (test-messages--file)))
        (expect text :to-match ":SENT_AT:")
        (expect text :not :to-match "CONVERSATION_IDS"))))

  (it "refuses before resolving anything on a read-only course"
    (test-messages--with-course test-messages--unsent
      (let ((org-canvas-read-only t))
        (expect (org-canvas-send-messages) :to-throw 'org-canvas-read-only-error))
      (expect test-messages--posts :to-be nil)
      (expect test-messages--self-calls :to-equal 0)))

  (it "says there is nothing to send from when the file is missing"
    (test-messages--with-course ""
      (delete-file org-canvas-messages-file)
      (expect (org-canvas-send-messages) :to-throw 'user-error)))

  (it "is not part of any sync, pull or delete tier"
    (dolist (tiers (list org-canvas--sync-tiers org-canvas--pull-tiers org-canvas--delete-tiers))
      (let ((names (mapcar #'car (apply #'append tiers))))
        (expect (memq 'org-canvas-send-messages names) :to-be nil)
        (expect (memq 'org-canvas-send-message-at-point names) :to-be nil)))))

(describe "org-canvas-send-message-at-point"
  (it "sends the heading at point and only that one"
    (test-messages--with-course
        (concat test-messages--unsent "* Second\n:PROPERTIES:\n:TO: Self\n:END:\nAgain.\n")
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-messages-file)
        (goto-char (point-min))
        (re-search-forward "^\\* Second")
        (org-canvas-send-message-at-point))
      (expect (length test-messages--posts) :to-equal 1)
      (expect (alist-get 'subject (cdar test-messages--posts)) :to-equal "Second")
      (expect (test-messages--heading 0) :not :to-match "SENT_AT")
      (expect (test-messages--heading 1) :to-match ":SENT_AT:")))

  (it "refuses outside messages.org and off a message heading"
    (test-messages--with-course test-messages--unsent
      (with-temp-buffer
        (expect (org-canvas-send-message-at-point) :to-throw 'user-error))
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-messages-file)
        (goto-char (point-max))
        (insert "** Not a message\nText.\n")
        (set-buffer-modified-p nil)
        (expect (org-canvas-send-message-at-point) :to-throw 'user-error))
      (expect test-messages--posts :to-be nil))))

(describe "message validation (issue #232)"
  (defun test-messages--issues (text)
    "Validate TEXT as messages.org and return the issue messages."
    (test-messages--with-course text
      (mapcar (lambda (i) (plist-get i :message))
              (cl-remove-if-not
               (lambda (i) (equal (file-name-nondirectory (plist-get i :file)) "messages.org"))
               (plist-get (org-canvas--validate-run-all-specs) :issues)))))

  (it "requires TO and a body, and names what a TO cannot reach"
    (expect (test-messages--issues "* A\nText.\n")
            :to-equal '("TO is required: Self, Course, Section: name, Group: name or Students: names"))
    (expect (test-messages--issues "* A\n:PROPERTIES:\n:TO: everyone\n:END:\nText.\n")
            :to-equal '("TO: 'everyone' is none of Self, Course, Section: name, Group: name or Students: names"))
    (expect (test-messages--issues "* A\n:PROPERTIES:\n:TO: Section: Lab 9\n:END:\nText.\n")
            :to-equal '("TO: could not resolve 'Lab 9' (pull sections first, or write #id)"))
    (expect (test-messages--issues "* A\n:PROPERTIES:\n:TO: Self\n:END:\n")
            :to-equal '("The message has no body; nothing to send")))

  (it "accepts a resolvable heading and a stamped one whose text still matches"
    (expect (test-messages--issues "* A\n:PROPERTIES:\n:TO: Students: Adams, Alice\n:END:\nText.\n")
            :to-equal nil)
    (expect (test-messages--issues
             (format "* A\n:PROPERTIES:\n:TO: Self\n:SENT_AT: <2026-09-01 Tue 09:00>\n:PAYLOAD_HASH: %s\n:END:\nText.\n"
                     (md5 "Text.")))
            :to-equal nil))

  (it "warns when a sent message was edited afterwards"
    (expect (test-messages--issues
             "* A\n:PROPERTIES:\n:TO: Self\n:SENT_AT: <2026-09-01 Tue 09:00>\n:PAYLOAD_HASH: 0000\n:END:\nChanged.\n")
            :to-equal '("Already sent <2026-09-01 Tue 09:00>; the edited text will not go out (add a new heading to send again)"))))

(provide 'org-canvas-messages-test)
;;; org-canvas-messages-test.el ends here
