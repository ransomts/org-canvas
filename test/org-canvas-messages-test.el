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
(defvar test-messages--inbox nil
  "The conversations the fake API lists for the Inbox, as a vector.")
(defvar test-messages--details nil
  "Alist of conversation id to the detail the fake API answers with.")
(defvar test-messages--conversation-calls nil
  "The (METHOD URL ARGS) triples the fake API saw on /conversations.")

(defun test-messages--api (method url &rest args)
  "Answer URL for METHOD from the fake tables; ARGS carry :data."
  (cond
   ((string-match "/users/self\\'" url)
    (cl-incf test-messages--self-calls)
    '((id . 77) (name . "Me")))
   ((and (eq method 'POST) (string-match "/conversations\\'" url))
    (push (cons url (plist-get args :data)) test-messages--posts)
    test-messages--reply)
   ((string-match "/conversations/\\([0-9]+\\)\\'" url)
    (push (list method url args) test-messages--conversation-calls)
    (or (cdr (assq (string-to-number (match-string 1 url)) test-messages--details))
        (signal 'org-canvas-api-error (list "No such conversation"))))
   ((string-match "/conversations\\'" url)
    (push (list method url args) test-messages--conversation-calls)
    test-messages--inbox)
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
          (test-messages--self-calls 0)
          (test-messages--inbox nil)
          (test-messages--details nil)
          (test-messages--conversation-calls nil)
          ;; Pin the zone every stamp and heading is written in, so the
          ;; dates the specs pin hold wherever the suite runs.
          (org-canvas--pull-tz-cache "UTC0"))
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

(defconst test-messages--sent
  (format "* Reminder
:PROPERTIES:
:TO: Students: Adams, Alice; Beta, Bob
:SENT_AT: <2026-10-01 Thu 09:00>
:RECIPIENTS: 11, 22
:CONVERSATION_IDS: 5001, 5002
:PAYLOAD_HASH: %s
:END:
Proposals are due Friday.
"
          (md5 "Proposals are due Friday."))
  "One sent message to two students, stamped with both conversations.")

(defun test-messages--detail (id &rest messages)
  "Build the detail of conversation ID holding MESSAGES, as Canvas would.
MESSAGES, each an alist with id, author_id, created_at and body, go in
as given: the pull reads the earliest by created_at as the one the
send wrote and keeps the rest.  Participants are derived from the
messages' authors, author 77 being the token's owner."
  (let ((authors '((77 . "Me") (11 . "Adams, Alice") (22 . "Beta, Bob"))))
    `((id . ,id)
      (messages . ,(vconcat messages))
      (participants . ,(vconcat
                        (mapcar (lambda (m)
                                  `((id . ,(alist-get 'author_id m))
                                    (name . ,(cdr (assq (alist-get 'author_id m) authors)))))
                                messages))))))

(defun test-messages--sent-message (id created-at)
  "Build the message the send wrote: id ID, author 77, posted CREATED-AT."
  `((id . ,id) (author_id . 77) (created_at . ,created-at)
    (body . "Proposals are due Friday.")))

(defun test-messages--reply-message (id created-at body)
  "Build a reply: id ID, author 11, posted CREATED-AT, carrying BODY."
  `((id . ,id) (author_id . 11) (created_at . ,created-at) (body . ,body)))

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

(describe "org-canvas--message-check-file"
  (it "refuses a missing file"
    (test-messages--with-course test-messages--unsent
      (expect (org-canvas--message-check-file (expand-file-name "absent.org" dir))
              :to-throw 'user-error)))

  (it "saves an unsaved messages.org when the user agrees, and aborts when they decline"
    (test-messages--with-course test-messages--unsent
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-messages-file)
        (goto-char (point-max))
        (insert "Edited but not saved.\n"))
      (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_prompt) nil)))
        (expect (org-canvas--message-check-file org-canvas-messages-file)
                :to-throw 'user-error))
      (expect (test-messages--file) :not :to-match "Edited but not saved")
      (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_prompt) t)))
        (org-canvas--message-check-file org-canvas-messages-file))
      (expect (test-messages--file) :to-match "Edited but not saved")
      (expect (buffer-modified-p (org-canvas--find-file-noselect org-canvas-messages-file))
              :to-be nil))))

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

  (it "warns about a student people.org marks as gone (issue #290)"
    (let ((test-messages--people
           (concat test-messages--people
                   "** Gone, Gil\n:PROPERTIES:\n:USER_ID: 99\n:ENROLLMENT_STATE: deleted\n:END:\n")))
      (expect (test-messages--issues "* A\n:PROPERTIES:\n:TO: Students: Adams, Alice; Gone, Gil\n:END:\nText.\n")
              :to-equal '("TO: 'Gone, Gil' no longer enrolled according to people.org"))
      (expect (test-messages--issues "* A\n:PROPERTIES:\n:TO: Course\n:END:\nText.\n")
              :to-equal nil)))

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

(describe "org-canvas--message-body"
  (it "stops at the first child heading, so replies beneath a sent message are not its text"
    (with-temp-org-buffer
        "* Sent
:PROPERTIES:
:TO: Self
:SENT_AT: <2026-10-01 Thu 09:00>
:END:
The text that went out.
** Reply by Adams, Alice (2026-10-02 10:00)
:PROPERTIES:
:MESSAGE_ID: 601
:END:
What came back.
"
      (expect (org-canvas--message-body) :to-equal "The text that went out."))))

(describe "org-canvas-pull-message-replies"
  (it "writes every conversation's replies beneath the sent heading, matched by CONVERSATION_IDS"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox
            (vector '((id . 5001) (message_count . 2))
                    '((id . 5002) (message_count . 1)))
            test-messages--details
            (list (cons 5001 (test-messages--detail
                              5001
                              ;; Out of posting order on purpose: the pull
                              ;; drops the earliest as the one it sent.
                              (test-messages--reply-message
                               601 "2026-10-02T10:00:00Z" "When you say Friday, do you mean end of day?")
                              (test-messages--sent-message 600 "2026-10-01T09:00:00Z")))
                  (cons 5002 (test-messages--detail
                              5002
                              (test-messages--sent-message 610 "2026-10-01T09:00:00Z")))))
      (org-canvas-pull-message-replies)
      (let ((text (test-messages--file)))
        (expect text :to-match "^\\*\\* Reply by Adams, Alice (2026-10-02 10:00)\n")
        (expect text :to-match ":CONVERSATION_ID: +5001\n")
        (expect text :to-match ":MESSAGE_ID: +601\n")
        (expect text :to-match ":AUTHOR: +Adams, Alice\n")
        (expect text :to-match ":AUTHOR_ID: +11\n")
        (expect text :to-match ":POSTED_AT: +<2026-10-02")
        (expect text :to-match "When you say Friday, do you mean end of day?")
        ;; The heading's own text is untouched, and the reply sits after it.
        (expect text :to-match "Proposals are due Friday\\.\n")
        (expect (string-match "Proposals are due Friday" text)
                :to-be-less-than (string-match "Reply by Adams" text)))
      ;; Only the conversation that may hold replies is read in detail.
      (expect (cl-count-if (lambda (c) (string-match-p "/conversations/5001\\'" (nth 1 c)))
                           test-messages--conversation-calls)
              :to-equal 1)
      (expect (cl-count-if (lambda (c) (string-match-p "/conversations/5002\\'" (nth 1 c)))
                           test-messages--conversation-calls)
              :to-equal 0)))

  (it "scopes the Inbox list to the course and marks nothing read on the way"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox (vector '((id . 5001) (message_count . 2)))
            test-messages--details
            (list (cons 5001 (test-messages--detail
                              5001
                              (test-messages--sent-message 600 "2026-10-01T09:00:00Z")
                              (test-messages--reply-message
                               601 "2026-10-02T10:00:00Z" "Done, thanks.")))))
      (org-canvas-pull-message-replies)
      (let* ((list-call (cl-find-if
                         (lambda (c) (string-match-p "/conversations\\'" (nth 1 c)))
                         test-messages--conversation-calls))
             (detail-call (cl-find-if
                           (lambda (c) (string-match-p "/conversations/5001\\'" (nth 1 c)))
                           test-messages--conversation-calls)))
        (expect (car list-call) :to-be 'GET)
        (expect (assoc "filter" (plist-get (nth 2 list-call) :params))
                :to-equal `("filter" . ,(format "course_%s" org-canvas-course-id)))
        (expect (car detail-call) :to-be 'GET)
        (expect (assoc "auto_mark_as_read" (plist-get (nth 2 detail-call) :params))
                :to-equal '("auto_mark_as_read" . "false")))))

  (it "writes nothing beneath a heading whose conversations hold no replies"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox
            (vector '((id . 5001) (message_count . 1))
                    '((id . 5002) (message_count . 1))))
      (let ((said nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-pull-message-replies))
        (expect (test-messages--file) :not :to-match "^\\*\\*")
        (expect (cl-count-if (lambda (c) (string-match-p "/conversations/[0-9]+\\'" (nth 1 c)))
                             test-messages--conversation-calls)
                :to-equal 0)
        (expect (car said)
                :to-equal "Message replies pull: 1 messages, 0 replies."))))

  (it "updates a reply in place on a re-pull instead of duplicating it"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox (vector '((id . 5001) (message_count . 2)))
            test-messages--details
            (list (cons 5001 (test-messages--detail
                              5001
                              (test-messages--sent-message 600 "2026-10-01T09:00:00Z")
                              (test-messages--reply-message
                               601 "2026-10-02T10:00:00Z" "first")))))
      (org-canvas-pull-message-replies)
      (setq test-messages--details
            (list (cons 5001 (test-messages--detail
                              5001
                              (test-messages--sent-message 600 "2026-10-01T09:00:00Z")
                              (test-messages--reply-message
                               601 "2026-10-02T10:00:00Z" "edited")))))
      (org-canvas-pull-message-replies)
      (let ((text (test-messages--file)))
        (expect (cl-count "Reply by Adams, Alice" (split-string text "\n")
                          :test #'string-match-p)
                :to-equal 1)
        (expect text :to-match "edited")
        (expect text :not :to-match "first"))))

  (it "skips a conversation whose read fails and reports it in the summary"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox
            (vector '((id . 5001) (message_count . 2))
                    '((id . 5002) (message_count . 2)))
            test-messages--details
            (list (cons 5002 (test-messages--detail
                              5002
                              (test-messages--sent-message 610 "2026-10-01T09:00:00Z")
                              (test-messages--reply-message
                               621 "2026-10-03T08:00:00Z" "Also done.")))))
      (let ((warnings nil) (said nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (if (string-match-p "/conversations/5001\\'" url)
                         (signal 'org-canvas-api-error '("Canvas said no"))
                       (apply #'test-messages--api method url args))))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-pull-message-replies)
          (expect (cl-some (lambda (l) (string-match-p "conversation 5001" l)) warnings)
                  :to-be-truthy)
          (expect (car said)
                  :to-equal "Message replies pull: 1 messages, 1 replies, 1 conversations skipped.")
          (let ((records (org-canvas--pull-summary-records)))
            (expect (cl-some (lambda (r) (and (eq (plist-get r :kind) 'error)
                                              (string-match-p "5001" (plist-get r :item))))
                             records)
                    :to-be-truthy)))
        ;; The heading's other conversation still wrote its reply.
        (expect (test-messages--file) :to-match "Also done\\."))))

  (it "reads nothing when no heading is stamped with conversation ids"
    (test-messages--with-course test-messages--unsent
      (let ((said nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-pull-message-replies))
        (expect test-messages--conversation-calls :to-be nil)
        (expect (car said)
                :to-equal "Message replies pull: 0 messages, 0 replies."))))

  (it "runs on a read-only course, since it only reads"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox (vector '((id . 5001) (message_count . 2)))
            test-messages--details
            (list (cons 5001 (test-messages--detail
                              5001
                              (test-messages--sent-message 600 "2026-10-01T09:00:00Z")
                              (test-messages--reply-message
                               601 "2026-10-02T10:00:00Z" "Read-only course, read back anyway.")))))
      (let ((org-canvas-read-only t))
        (expect (org-canvas-pull-message-replies) :not :to-throw))
      (expect (test-messages--file) :to-match "Read-only course, read back anyway\\.")))

  (it "is not part of any sync, pull or delete tier"
    (dolist (tiers (list org-canvas--sync-tiers org-canvas--pull-tiers org-canvas--delete-tiers))
      (expect (memq 'org-canvas-pull-message-replies
                    (mapcar #'car (apply #'append tiers)))
              :to-be nil)))

  (it "says there is nothing to read when messages.org is missing"
    (test-messages--with-course ""
      (delete-file org-canvas-messages-file)
      (expect (org-canvas-pull-message-replies) :to-throw 'user-error)))

  (it "keeps a sent heading with replies beneath it validating clean"
    (expect (test-messages--issues
             (concat test-messages--sent
                     "** Reply by Adams, Alice (2026-10-02 10:00)
:PROPERTIES:
:CONVERSATION_ID: 5001
:MESSAGE_ID: 601
:AUTHOR: Adams, Alice
:AUTHOR_ID: 11
:POSTED_AT: <2026-10-02 Fri 10:00>
:END:
When you say Friday, do you mean end of day?
"))
            :to-equal nil))

  (it "falls back to a participant's full name when they picked no short one"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox (vector '((id . 5001) (message_count . 2)))
            test-messages--details
            (list (cons 5001 `((id . 5001)
                              (messages . ,(vector
                                            (test-messages--sent-message 600 "2026-10-01T09:00:00Z")
                                            '((id . 601) (author_id . 33)
                                              (created_at . "2026-10-02T10:00:00Z")
                                              (body . "Here."))))
                              (participants . ,(vector '((id . 77) (name . "Me"))
                                                       '((id . 33) (full_name . "Chen, Charlie"))))))))
      (org-canvas-pull-message-replies)
      (expect (test-messages--file) :to-match "^\\*\\* Reply by Chen, Charlie (2026-10-02 10:00)\n")
      (expect (test-messages--file) :to-match "Here\\.")))

  (it "renders a reply Canvas sent no time for, and keeps the dated send out of it"
    (test-messages--with-course test-messages--sent
      (setq test-messages--inbox (vector '((id . 5001) (message_count . 2)))
            test-messages--details
            (list (cons 5001 (test-messages--detail
                              5001
                              (test-messages--sent-message 600 "2026-10-01T09:00:00Z")
                              '((id . 601) (author_id . 11) (created_at)
                                (body . "Undated but mine."))))))
      (org-canvas-pull-message-replies)
      (let ((text (test-messages--file)))
        (expect text :to-match "^\\*\\* Reply by Adams, Alice (undated)\n")
        (expect text :to-match "Undated but mine\\.")
        ;; The dated send stays the heading's own text, not a reply.
        (expect (cl-count "Proposals are due Friday" (split-string text "\n")
                          :test #'string-match-p)
                :to-equal 1))))

  (it "inserts the reply on its own line when the message ends the file without one"
    (test-messages--with-course
        (concat "* Sent
:PROPERTIES:
:TO: Self
:SENT_AT: <2026-10-01 Thu 09:00>
:CONVERSATION_IDS: 5001
:END:
Text with no newline after it")
      (setq test-messages--inbox (vector '((id . 5001) (message_count . 2)))
            test-messages--details
            (list (cons 5001 (test-messages--detail
                              5001
                              (test-messages--sent-message 600 "2026-10-01T09:00:00Z")
                              (test-messages--reply-message
                               601 "2026-10-02T10:00:00Z" "On its own line.")))))
      (org-canvas-pull-message-replies)
      (expect (test-messages--file) :to-match "no newline after it\n\\*\\* Reply by Adams, Alice"))))

(describe "org-canvas--message-posted-before-p"
  (it "orders by posted time, a message with no time last"
    (expect (org-canvas--message-posted-before-p
             '((created_at . "2026-10-01T09:00:00Z"))
             '((created_at . "2026-10-02T10:00:00Z")))
            :to-be-truthy)
    (expect (org-canvas--message-posted-before-p
             '((created_at . "2026-10-02T10:00:00Z"))
             '((created_at . "2026-10-01T09:00:00Z")))
            :to-be nil)
    (expect (org-canvas--message-posted-before-p
             '((created_at . "2026-10-02T10:00:00Z")) '((id . 7)))
            :to-be-truthy)
    (expect (org-canvas--message-posted-before-p
             '((id . 7)) '((created_at . "2026-10-02T10:00:00Z")))
            :to-be nil)))

(provide 'org-canvas-messages-test)
;;; org-canvas-messages-test.el ends here
