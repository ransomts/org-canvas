;;; org-canvas-messages.el --- Send conversations from messages.org, read replies back -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The moment a message to students is wanted is the moment
;; gradebook.org shows who has missing work.  This module sends Canvas
;; conversations (the Inbox) from messages.org: one heading per
;; message, the heading is the subject, the body is the text, and TO
;; names who gets it.  A sent heading is stamped and never sent again,
;; so the file is also the record of what went out and when.
;;
;; FILE STRUCTURE
;; ==============
;;   * Subject line
;;   :PROPERTIES:
;;   :TO: Students: Adams, Alice; Beta, Bob
;;   :END:
;;   The message, as plain text.
;;   ** Reply by Beta, Bob (2026-09-02 09:30)
;;   What a student wrote back, written by the reply pull.
;;
;; TO is one of:
;;   Self                      the token's owner: rehearse a message on
;;                             yourself before sending it to students
;;   Course                    every student in the course
;;   Section: <name>           a section named in sections.org
;;   Group: <name>             a group named in groups.org
;;   Students: <name>; <name>  people named in people.org, `;' between
;;                             them since a sortable name carries a comma
;; A section, group or student may also be a literal #id.  BULK
;; (default true) sends one private conversation per recipient; false
;; sends one group conversation everyone can see.
;;
;; After a send the heading carries SENT_AT, RECIPIENTS (the recipient
;; ids or context Canvas was given), CONVERSATION_IDS (when Canvas
;; answers with them) and PAYLOAD_HASH (the body's md5, so validate
;; can tell an edited message from the one that went out).
;;
;; READING REPLIES BACK
;; =====================
;; `org-canvas-pull-message-replies' reads the CONVERSATION_IDS back:
;; each conversation's messages after the one the send wrote become
;; level-2 headings beneath the message that started them, matched by
;; MESSAGE_ID and updated in place, so the file holds both halves of
;; the conversation.  Nothing local is deleted by the pull.
;;
;; SAFEGUARDS
;; ==========
;; - Sending is never part of `org-canvas-sync': the commands here are
;;   the only path, and they are deliberately absent from every tier.
;; - The reply pull is not either: it is the send's other half, read on
;;   command, never in `org-canvas-pull-all' or any sync or pull tier.
;; - A heading with SENT_AT is skipped, always.  To send again, add a
;;   new heading.
;; - Interactive use confirms once, naming every message and its
;;   target; batch and `org-canvas-assume-yes' proceed.
;; - The dry run (`org-canvas--dry-run', or a prefix argument) resolves
;;   the recipients, logs them and sends nothing (Hard Rule 1).
;; - A course marked `org-canvas-read-only' is refused before anything
;;   is resolved, and again at the transport as for every write.  The
;;   reply pull is all GETs, so the same course allows it.
;;
;; API NOTES
;; =========
;;   POST /api/v1/conversations   not a course endpoint; `context_code'
;;       names the course.  `recipients' takes user ids or the context
;;       strings course_<id>_students, section_<id>, group_<id>.
;;       Bodies are plain text: Canvas does not render HTML here.
;;       `bulk_message' makes one private conversation per recipient;
;;       `group_conversation' one thread for all.  The reply lists the
;;       conversations created, sometimes none when Canvas queues a
;;       large batch.
;;   GET  /api/v1/users/self      the token owner, for TO: Self.
;;   GET  /api/v1/conversations  the Inbox list, `filter' scoped to
;;       this course, walked page by page as every list is; only the
;;       conversations whose ids a heading carries are read further.
;;   GET  /api/v1/conversations/:id  one conversation's messages and
;;       participants, `auto_mark_as_read' false so reading a reply
;;       back leaves the Inbox's unread state alone.
;;
;; PERSONAL DATA
;; =============
;; A name in TO travels with the course repository, as does the text of
;; what was said to whom.  Write `#id' instead of a name when the
;; repository is shared, or keep messages.org out of it.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-messages-file (org-canvas--path "messages.org")
  "Path to the messages.org file.
One heading per message; a sent heading is stamped and kept as the
record."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-messages-file "messages.org")

(org-canvas-register-properties "messages"
  :label "Messages"
  :file-var 'org-canvas-messages-file
  :query "LEVEL=1"
  :structural-fn #'org-canvas--validate-message-structure
  :properties
  `((:org-prop "TO" :data-key :to :type string
     :doc "Who gets it: Self, Course, Section: name, Group: name, or Students: name; name (a #id in place of any name)")
    (:org-prop "BULK" :data-key :bulk :type boolean :default t
     :doc "One private conversation per recipient; false sends one group conversation")
    (:org-prop "SENT_AT" :data-key :sent_at :type timestamp :pull-only t
     :doc "When the message was sent; a heading that carries it is never sent again")
    (:org-prop "RECIPIENTS" :data-key :recipients :type string :pull-only t
     :doc "The user ids or Canvas context the message was sent to")
    (:org-prop "CONVERSATION_IDS" :data-key :conversation_ids :type string :pull-only t
     :doc "The conversations Canvas created, when it answered with them")))

(org-canvas-register-properties "message-replies"
  :label "Message Replies"
  :file-var 'org-canvas-messages-file
  :query "LEVEL=2"
  :properties
  '((:org-prop "CONVERSATION_ID" :data-key :conversation-id :type string
     :doc "Conversation the reply belongs to, one of the heading's CONVERSATION_IDS (set by the reply pull)")
    (:org-prop "MESSAGE_ID" :data-key :message-id :type string
     :doc "Canvas id of the reply (set by the reply pull)")
    (:org-prop "AUTHOR" :data-key :author :type string :pull-only t
     :doc "Display name of the reply's author (set by the reply pull)")
    (:org-prop "AUTHOR_ID" :data-key :author-id :type number
     :doc "Canvas user id of the reply's author (set by the reply pull)")
    (:org-prop "POSTED_AT" :data-key :posted-at :type timestamp :pull-only t
     :doc "When the reply was posted (set by the reply pull)")))

;;;; Targets

(defconst org-canvas--message-target-regexp
  "\\`\\(Self\\|Course\\|Sections?:\\|Groups?:\\|Students?:\\)[ \t]*\\(.*?\\)[ \t]*\\'"
  "Match a TO value; group 1 is the kind, group 2 what follows the colon.")

(defvar org-canvas--message-self-id nil
  "The token owner's user id, fetched once per send run for TO: Self.
Bound by the commands; nil outside a run, so validation never asks
Canvas.")

(defun org-canvas--message-lookup-file (var)
  "Return the file VAR names when VAR is bound and the file exists, else nil.
The people, sections and groups files belong to other feature modules,
which this one must not require; it reads them by name when they exist."
  (let ((file (and (boundp var) (symbol-value var))))
    (and file (file-exists-p file) file)))

(defun org-canvas--message-literal-id (text)
  "Return the digits of TEXT when it is a literal #123, else nil."
  (and (string-match "\\`#\\([0-9]+\\)\\'" text) (match-string 1 text)))

(defun org-canvas--message-resolve-id (text var property match)
  "Resolve TEXT, a heading title or #id, to PROPERTY of that heading.
The heading is searched in the file VAR names, under MATCH."
  (or (org-canvas--message-literal-id text)
      (org-canvas--heading-property-by-title
       (org-canvas--message-lookup-file var) text property match)))

(defun org-canvas--message-conversations-url ()
  "Return the conversations collection URL under the instance.
Conversations belong to the user, not the course: `context_code' names
the course on the way in, and nothing on the way out.  A trailing
slash on `org-canvas-base-url' is trimmed."
  (format "%s/api/v1/conversations"
          (replace-regexp-in-string "/+\\'" "" org-canvas-base-url)))

(defun org-canvas--message-self-id ()
  "Return the token owner's user id as a string, fetching it once per run."
  (or org-canvas--message-self-id
      (let ((me (org-canvas-api-request 'GET (org-canvas--api-users-self-url))))
        (setq org-canvas--message-self-id
              (and (alist-get 'id me) (format "%s" (alist-get 'id me)))))))

(defun org-canvas--message-resolve-students (text)
  "Resolve TEXT, names separated by `;', to a list of user id strings.
Returns (:recipients IDS :label LABEL), or (:unresolved NAMES) naming
what did not resolve: a message to half the people named would be a
surprise, so it is all or nothing."
  (let* ((names (split-string text ";" t "[ \t]+"))
         (ids (mapcar (lambda (n)
                        (org-canvas--message-resolve-id
                         n 'org-canvas-people-file "USER_ID" "LEVEL=2"))
                      names))
         (unresolved (cl-loop for n in names for id in ids unless id collect n)))
    (cond
     ((null names) (list :unresolved '("nobody")))
     (unresolved (list :unresolved unresolved))
     (t (list :recipients ids
              :label (format "%d student%s" (length ids) (if (= (length ids) 1) "" "s")))))))

(defun org-canvas--message-resolve-context (kind text)
  "Resolve TEXT, a section or group title or #id, for KIND (`section' or `group').
Returns (:recipients (CONTEXT) :label LABEL) or (:unresolved (TEXT))."
  (let ((id (if (eq kind 'section)
                (org-canvas--message-resolve-id
                 text 'org-canvas-sections-file "CANVAS_ID" "LEVEL=1")
              (org-canvas--message-resolve-id
               text 'org-canvas-groups-file "CANVAS_ID" "LEVEL=2"))))
    (if id
        (list :recipients (list (format "%s_%s" kind id))
              :label (format "%s '%s'" kind text))
      (list :unresolved (list text)))))

(defun org-canvas--message-target-kind (to)
  "Return (KIND . REST) for the TO text, or nil when it is none of the kinds.
KIND is one of `self', `course', `section', `group' and `students'."
  (when (and to (string-match org-canvas--message-target-regexp to))
    (let ((word (downcase (match-string 1 to)))
          (rest (match-string 2 to)))
      (cons (cond ((string-prefix-p "self" word) 'self)
                  ((string-prefix-p "course" word) 'course)
                  ((string-prefix-p "section" word) 'section)
                  ((string-prefix-p "group" word) 'group)
                  (t 'students))
            rest))))

(defun org-canvas--message-resolve-target (to &optional offline)
  "Resolve the TO text to who gets the message.
Returns a plist: (:recipients LIST :label LABEL) when resolved, where
LIST holds user ids or one Canvas context string; (:unresolved NAMES
:kind KIND) when a name matches nothing pulled; or (:invalid t) when
TO is none of the kinds.  OFFLINE, for validation, resolves Self
without asking Canvas."
  (pcase (org-canvas--message-target-kind to)
    ('nil (list :invalid t))
    (`(self . ,_)
     (let ((id (if offline "self" (org-canvas--message-self-id))))
       (if id
           (list :recipients (list id) :label "yourself")
         (list :unresolved '("Self") :kind 'self))))
    (`(course . ,_)
     (list :recipients (list (format "course_%s_students" org-canvas-course-id))
           :label "every student in the course"))
    (`(students . ,rest)
     (let ((r (org-canvas--message-resolve-students rest)))
       (if (plist-get r :unresolved) (plist-put r :kind 'students) r)))
    (`(,kind . ,rest)
     (let ((r (org-canvas--message-resolve-context kind rest)))
       (if (plist-get r :unresolved) (plist-put r :kind kind) r)))))

(defun org-canvas--message-unresolved-advice (kind)
  "Return what to pull so a KIND target resolves, for a warning."
  (pcase kind
    ('section "pull sections first, or write #id")
    ('group "pull groups first, or write #id")
    ('students "pull people first, or write #id")
    (_ "check the token")))

;;;; Reading a Heading

(defun org-canvas--message-body ()
  "Return the plain-text body of the heading at point, trimmed.
The property drawer and planning lines are skipped, and the body ends
at the next heading of any level, so the replies the pull writes
beneath a sent message are not part of its text; the text is sent as
it stands, since Canvas renders no markup in a conversation."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (org-end-of-meta-data t)
      (string-trim (buffer-substring-no-properties (min (point) end) end)))))

(defun org-canvas--message-parse-entry ()
  "Parse the message heading at point into a plist.
Keys: :subject :body :to :bulk :sent-at :marker."
  (save-excursion
    (org-back-to-heading t)
    (list :subject (org-get-heading t t t t)
          :body (org-canvas--message-body)
          :to (org-entry-get (point) "TO")
          :bulk (org-canvas--interpret-boolean (org-entry-get (point) "BULK") t)
          :sent-at (org-entry-get (point) "SENT_AT")
          :marker (point-marker))))

;;;; Sending

(defun org-canvas--message-build-payload (entry target)
  "Build the conversations payload for ENTRY going to TARGET."
  (let ((bulk (plist-get entry :bulk)))
    `((recipients . ,(plist-get target :recipients))
      (subject . ,(plist-get entry :subject))
      (body . ,(plist-get entry :body))
      (context_code . ,(format "course_%s" org-canvas-course-id))
      (bulk_message . ,(if bulk t :json-false))
      (group_conversation . ,(if bulk :json-false t))
      (force_new . t))))

(defun org-canvas--message-stamp (entry target reply)
  "Stamp ENTRY's heading after a send to TARGET answered with REPLY."
  (let ((pom (plist-get entry :marker))
        (ids (mapconcat (lambda (c) (format "%s" (alist-get 'id c)))
                        (cl-remove-if-not (lambda (c) (and (listp c) (alist-get 'id c)))
                                          (append reply nil))
                        ", ")))
    (org-canvas-org-set-property
     pom "SENT_AT"
     (org-canvas--iso8601-to-org-timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
    (org-canvas-org-set-property
     pom "RECIPIENTS" (mapconcat #'identity (plist-get target :recipients) ", "))
    (unless (string-empty-p ids)
      (org-canvas-org-set-property pom "CONVERSATION_IDS" ids))
    (org-canvas-org-set-property pom "PAYLOAD_HASH" (md5 (plist-get entry :body)))))

(defun org-canvas--message-send-one (entry target)
  "Send ENTRY to TARGET, or log what would be sent under the dry run.
Returns `dry-run' or `sent'."
  (let ((subject (plist-get entry :subject))
        (recipients (plist-get target :recipients)))
    (if org-canvas--dry-run
        (progn
          (org-canvas--log-info org-canvas--logger
            "[DRY-RUN] Would send '%s' to %s (%s)"
            subject (plist-get target :label) (mapconcat #'identity recipients ", "))
          'dry-run)
      (let ((reply (org-canvas-api-request
                    'POST (org-canvas--message-conversations-url)
                    :data (org-canvas--message-build-payload entry target))))
        (org-canvas--message-stamp entry target reply)
        (org-canvas--log-info org-canvas--logger
          "[Messages] Sent '%s' to %s" subject (plist-get target :label))
        'sent))))

(defun org-canvas--message-prepare (entry)
  "Resolve ENTRY's target, or log why it cannot be sent and return nil.
Returns (ENTRY . TARGET)."
  (let ((subject (plist-get entry :subject))
        (target (org-canvas--message-resolve-target (plist-get entry :to))))
    (cond
     ((plist-get entry :sent-at)
      (org-canvas--log-info org-canvas--logger
        "[Messages] Skipping '%s': already sent %s (add a new heading to send again)"
        subject (plist-get entry :sent-at))
      nil)
     ((string-empty-p (plist-get entry :body))
      (org-canvas--log-error org-canvas--logger
        "[Messages] '%s' has no body; nothing to send" subject)
      nil)
     ((plist-get target :invalid)
      (org-canvas--log-error org-canvas--logger
        "[Messages] '%s': TO must be Self, Course, Section: name, Group: name or Students: names (got %S)"
        subject (plist-get entry :to))
      nil)
     ((plist-get target :unresolved)
      (org-canvas--log-warning org-canvas--logger
        "[Messages] '%s': could not resolve %s (%s)"
        subject (mapconcat (lambda (n) (format "'%s'" n)) (plist-get target :unresolved) ", ")
        (org-canvas--message-unresolved-advice (plist-get target :kind)))
      nil)
     (t (cons entry target)))))

(defun org-canvas--message-entries (file at-point)
  "Return the message entries of FILE, or only the one at point when AT-POINT.
Each is the plist `org-canvas--message-parse-entry' returns."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (if at-point
        (progn
          (unless (org-at-heading-p) (org-back-to-heading t))
          (unless (= (org-current-level) 1)
            (user-error "Point must be on a message heading (level 1)"))
          (list (org-canvas--message-parse-entry)))
      (save-excursion
        (goto-char (point-min))
        (org-map-entries #'org-canvas--message-parse-entry "LEVEL=1" 'file)))))

(defun org-canvas--message-confirm (prepared)
  "Ask once before sending PREPARED, a list of (ENTRY . TARGET).
Silent under the dry run; `org-canvas--confirm' answers yes in batch."
  (or org-canvas--dry-run
      (org-canvas--confirm
       (format "Send %d message%s: %s? "
               (length prepared) (if (= (length prepared) 1) "" "s")
               (mapconcat (lambda (p)
                            (format "'%s' to %s" (plist-get (car p) :subject)
                                    (plist-get (cdr p) :label)))
                          prepared "; ")))
      (user-error "Cancelled; nothing was sent")))

(defun org-canvas--message-check-file (file)
  "Refuse when FILE is missing; offer to save it when its buffer is modified."
  (unless (file-exists-p file)
    (user-error "No %s to send from" (file-name-nondirectory file)))
  (let ((buf (org-canvas--find-file-noselect file)))
    (when (buffer-modified-p buf)
      (if (org-canvas--confirm
           (format "%s has unsaved changes.  Save before sending? " (file-name-nondirectory file)))
          (with-current-buffer buf (org-canvas--save-buffer))
        (user-error "Aborted: unsaved changes in %s" file)))))

(defun org-canvas--message-deliver (prepared)
  "Send each (ENTRY . TARGET) of PREPARED, one failure never stopping the rest.
Returns (SENT PREVIEWED FAILED)."
  (let ((sent 0) (previewed 0) (failed 0))
    (dolist (p prepared)
      (condition-case err
          (if (eq (org-canvas--message-send-one (car p) (cdr p)) 'sent)
              (cl-incf sent)
            (cl-incf previewed))
        (error
         (cl-incf failed)
         (org-canvas--log-error org-canvas--logger
           "[Messages] Failed to send '%s': %s"
           (plist-get (car p) :subject) (error-message-string err)))))
    (list sent previewed failed)))

(defun org-canvas--message-send-all (file at-point)
  "Send the unsent messages of FILE, or the one at point when AT-POINT.
The shared body of the two commands."
  (org-canvas--check-writable 'POST "sending messages")
  (org-canvas--start-operation (if org-canvas--dry-run "PREVIEWING MESSAGES" "SENDING MESSAGES"))
  (org-canvas--message-check-file file)
  (let* ((org-canvas--message-self-id nil)
         (prepared (delq nil (mapcar #'org-canvas--message-prepare
                                     (org-canvas--message-entries file at-point))))
         (counts (list 0 0 0)))
    (when prepared
      (org-canvas--message-confirm prepared)
      (setq counts (org-canvas--message-deliver prepared))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (org-canvas--save-buffer)))
    (apply #'org-canvas--log-info org-canvas--logger
           "[Messages] Done: %d sent, %d previewed, %d failed" counts)
    (apply #'message "Messages: %d sent, %d previewed, %d failed" counts)))

;;;###autoload
(defun org-canvas-send-messages (&optional preview)
  "Send every message in messages.org that has not been sent.
A heading carrying SENT_AT is skipped.  With PREVIEW (a prefix
argument) resolve the recipients and log what would go out, sending
nothing.  Never part of `org-canvas-sync'."
  (interactive "P")
  (let ((org-canvas--dry-run (or org-canvas--dry-run preview)))
    (org-canvas--message-send-all (expand-file-name org-canvas-messages-file) nil)))

;;;###autoload
(defun org-canvas-send-message-at-point (&optional preview)
  "Send the message heading at point, unless it was already sent.
With PREVIEW (a prefix argument) resolve the recipients and log what
would go out, sending nothing."
  (interactive "P")
  (let ((file (expand-file-name org-canvas-messages-file)))
    (unless (and (buffer-file-name)
                 (equal (file-truename (buffer-file-name)) (file-truename file)))
      (user-error "Not in %s" (file-name-nondirectory file)))
    (let ((org-canvas--dry-run (or org-canvas--dry-run preview)))
      (org-canvas--message-send-all file t))))

;;;; Reading Replies Back

(defun org-canvas--message-split-ids (text)
  "Return the conversation ids TEXT lists, comma-separated, as strings."
  (split-string text "," t "[ \t\n]+"))

(defun org-canvas--message-sent-entries (file)
  "Return FILE's sent entries as a list of (MARKER IDS).
MARKER is the heading's position, IDS its CONVERSATION_IDS, one per
recipient a bulk send reached.  The markers are collected before any
writing (Hard Rule 9), so the pull can insert beneath one heading
while the ones after it keep their positions."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (delq nil
            (org-map-entries
             (lambda ()
               (let ((ids (org-canvas--message-split-ids
                           (org-entry-get (point) "CONVERSATION_IDS"))))
                 (when ids (list (point-marker) ids))))
             "LEVEL=1&CONVERSATION_IDS={.}" 'file)))))

(defun org-canvas--message-entry-subject (pom)
  "Return the subject of the message heading at POM: its heading text."
  (org-with-point-at pom (org-get-heading t t t t)))

(defun org-canvas--message-wanted-ids (entries)
  "Return a hash of every conversation id the ENTRIES name."
  (let ((wanted (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (dolist (id (cadr entry))
        (puthash id t wanted)))
    wanted))

(defun org-canvas--message-entry-subjects (entries)
  "Return a hash of conversation id to the subject of a message stamping it.
ENTRIES is what `org-canvas--message-sent-entries' returned."
  (let ((subjects (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (dolist (id (cadr entry))
        (unless (gethash id subjects)
          (puthash id (org-canvas--message-entry-subject (car entry))
                   subjects))))
    subjects))

(defun org-canvas--message-fetch-inbox ()
  "Return this course's conversations from the Inbox, as a list.
The list is scoped to the course by `filter', since conversations
belong to the user and cross every course the token teaches, and
walked page by page as every Canvas list is."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas--message-conversations-url)
           `(("filter" . ,(format "course_%s" org-canvas-course-id))))
          nil))

(defun org-canvas--message-matching-conversations (conversations wanted)
  "Return the elements of CONVERSATIONS whose id is in WANTED, a hash."
  (cl-remove-if-not
   (lambda (conversation)
     (gethash (format "%s" (alist-get 'id conversation)) wanted))
   conversations))

(defun org-canvas--message-may-have-replies-p (conversation)
  "Return non-nil when CONVERSATION may hold messages past the one sent.
`message_count' decides: one message is the send itself, so reading it
in detail would repeat what the heading already holds.  A count Canvas
did not send reads as maybe."
  (let ((count (alist-get 'message_count conversation)))
    (not (and (numberp count) (<= count 1)))))

(defun org-canvas--message-record-conversation-failure (id subject err)
  "Log and record the failed read of conversation ID, signalled as ERR.
SUBJECT names the message heading whose conversation it is.  A
refusal `org-canvas--api-skip-error-p' recognises is a skip, as every
pull counts one; anything else is an error.  One unreadable
conversation costs its own replies and not the pull — the rule issue
#171 set for every per-item failure."
  (let* ((skip (org-canvas--api-skip-error-p err))
         (msg (error-message-string err)))
    (org-canvas--log-warning org-canvas--logger
      "[Messages] '%s': conversation %s %s: %s"
      subject id (if skip "skipped" "not read") msg)
    (org-canvas--pull-summary-record
     :kind (if skip 'skip 'error)
     :file "messages.org"
     :item (format "conversation %s of '%s'" id subject)
     :error msg
     :log-line (org-canvas--pull-summary-current-log-line))))

(defun org-canvas--message-fetch-detail (conversation subject)
  "Return CONVERSATION's detail, or nil when the read fails.
SUBJECT names the message heading, for the failure's log line.  The
read asks Canvas not to mark the conversation read
\(`auto_mark_as_read' is true by default): reading a reply back has no
business with the Inbox's unread state."
  (condition-case err
      (org-canvas-api-request
       'GET (format "%s/%s" (org-canvas--message-conversations-url)
                    (alist-get 'id conversation))
       :params '(("auto_mark_as_read" . "false")))
    (org-canvas-api-error
     (org-canvas--message-record-conversation-failure
      (alist-get 'id conversation) subject err)
     nil)))

(defun org-canvas--message-fetch-details (conversations subjects)
  "Return (DETAILS . SKIPPED) for the CONVERSATIONS to read.
SUBJECTS maps conversation id to the subject of a message stamping
it.  Only a conversation that may hold messages past the one sent is
read in detail.  DETAILS maps conversation id to its detail, or nil
where the read failed; SKIPPED counts the failures, each logged and
recorded in the pull summary."
  (let ((details (make-hash-table :test 'equal))
        (skipped 0))
    (dolist (conversation conversations)
      (when (org-canvas--message-may-have-replies-p conversation)
        (let* ((id (format "%s" (alist-get 'id conversation)))
               (detail (org-canvas--message-fetch-detail
                        conversation (gethash id subjects))))
          (unless detail (cl-incf skipped))
          (puthash id detail details))))
    (cons details skipped)))

(defun org-canvas--message-read-details (entries)
  "Return (DETAILS . SKIPPED) for the conversations the ENTRIES name.
The Inbox's list is walked once, scoped to this course, and each
conversation on it that an entry stamps and that may hold messages
past the one sent is read in detail.  DETAILS maps conversation id to
its detail, or nil where the read failed; SKIPPED counts the
failures.  Nothing is read at all when no entry stamps an id."
  (if (null entries)
      (cons (make-hash-table :test 'equal) 0)
    (let* ((wanted (org-canvas--message-wanted-ids entries))
           (matched (org-canvas--message-matching-conversations
                     (org-canvas--message-fetch-inbox) wanted))
           (subjects (org-canvas--message-entry-subjects entries)))
      (org-canvas--message-fetch-details matched subjects))))

(defun org-canvas--message-posted-before-p (a b)
  "Return non-nil when message A was posted no later than message B.
A message Canvas sent no time for sorts after every dated one, as a
nil key does everywhere else a pull sorts."
  (let ((ta (alist-get 'created_at a))
        (tb (alist-get 'created_at b)))
    (cond
     ((and ta tb) (string< ta tb))
     ((null ta) nil)
     (t t))))

(defun org-canvas--message-replies-of (conversation)
  "Return CONVERSATION's messages after the one the send wrote.
The send created the conversation (`force_new'), so the earliest
message is the text messages.org already holds as the heading's body;
everything after it is what came back, in posting order."
  (cdr (sort (copy-sequence (append (alist-get 'messages conversation) nil))
             #'org-canvas--message-posted-before-p)))

(defun org-canvas--message-participant-name (conversation user-id)
  "Return the name CONVERSATION's participants give USER-ID, or nil.
The short name a user picked for conversations wins; the full name is
the fallback."
  (let ((participant
         (cl-find-if (lambda (p) (equal (alist-get 'id p) user-id))
                     (append (alist-get 'participants conversation) nil))))
    (and participant (or (alist-get 'name participant)
                         (alist-get 'full_name participant)))))

(defun org-canvas--message-reply-title (conversation message)
  "Return `Reply by NAME (YYYY-MM-DD HH:MM)' for MESSAGE of CONVERSATION.
The date and time are those of the Org timestamp the message's
POSTED_AT gets, so the heading and the property agree on the course's
zone."
  (let* ((name (or (org-canvas--message-participant-name
                    conversation (alist-get 'author_id message))
                   "Unknown"))
         (ts (org-canvas--iso8601-to-org-timestamp (alist-get 'created_at message)))
         (when-text (if (and ts (string-match "\\([0-9-]+\\) [A-Za-z]+ \\([0-9:]+\\)" ts))
                        (format "%s %s" (match-string 1 ts) (match-string 2 ts))
                      "undated")))
    (format "Reply by %s (%s)" name when-text)))

(defun org-canvas--message-find-reply (pom conversation-id message-id)
  "Return the position of the reply MESSAGE-ID of CONVERSATION-ID, or nil.
Only the subtree of the heading at POM is searched, so a reply stays
beneath the message it answers even when the same conversation is
stamped on another heading."
  (let ((found nil)
        (cid (format "%s" conversation-id))
        (mid (format "%s" message-id)))
    (save-excursion
      (goto-char pom)
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (while (and (not found) (outline-next-heading) (< (point) end))
          (when (and (equal (org-entry-get (point) "CONVERSATION_ID") cid)
                     (equal (org-entry-get (point) "MESSAGE_ID") mid))
            (setq found (point))))))
    found))

(defun org-canvas--message-insert-reply (pom title)
  "Insert a reply heading TITLE under the message at POM; return its position."
  (goto-char pom)
  (let ((stars (org-canvas--pull-child-stars)))
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (let ((start (point)))
      (insert stars " " title "\n")
      (goto-char start)
      (org-back-to-heading t)
      (point))))

(defun org-canvas--message-reply-set-body (pos text)
  "Replace the body of the reply heading at POS with TEXT, as plain text.
Conversations carry no markup — the send writes prose and Canvas
answers prose — so the text passes through the headline chokepoint
\(`org-canvas--org-body-neutralize-headlines', Hard Rule 22) and goes
in as it stands.  The body ends at the next heading of any level, so a
reply never swallows what follows it."
  (save-excursion
    (goto-char pos)
    (let* ((meta-end (save-excursion (org-end-of-meta-data t) (point)))
           (next (save-excursion (outline-next-heading) (point)))
           (body-start (save-excursion
                         (goto-char (min meta-end next))
                         (skip-chars-backward " \t\n")
                         (point)))
           (clean (and text (not (string-empty-p text))
                       (string-trim (org-canvas--org-body-neutralize-headlines text)))))
      (delete-region body-start next)
      (goto-char body-start)
      (insert "\n")
      (when (and clean (not (string-empty-p clean)))
        (insert clean "\n")))))

(defun org-canvas--message-write-reply (pom conversation message)
  "Write MESSAGE of CONVERSATION as a reply beneath the heading at POM.
A reply already written is found by its CONVERSATION_ID and MESSAGE_ID
and updated in place, so a re-pull rewrites rather than duplicates."
  (let* ((cid (format "%s" (alist-get 'id conversation)))
         (mid (format "%s" (alist-get 'id message)))
         (pos (or (org-canvas--message-find-reply pom cid mid)
                  (org-canvas--message-insert-reply
                   pom (org-canvas--message-reply-title conversation message)))))
    (org-canvas-org-set-property pos "CONVERSATION_ID" cid)
    (org-canvas-org-set-property pos "MESSAGE_ID" mid)
    (let ((author (org-canvas--message-participant-name
                   conversation (alist-get 'author_id message))))
      (when author (org-canvas-org-set-property pos "AUTHOR" author)))
    (when (alist-get 'author_id message)
      (org-canvas-org-set-property pos "AUTHOR_ID"
                                   (format "%s" (alist-get 'author_id message))))
    (org-canvas--pull-set-timestamp-property pos "POSTED_AT"
                                             (alist-get 'created_at message))
    (org-canvas--message-reply-set-body pos (alist-get 'body message))))

(defun org-canvas--message-write-conversation (pom conversation)
  "Write CONVERSATION's replies beneath the message heading at POM.
Returns how many were written.  Nil for a conversation whose read
failed, that holds nothing past the send, or that the Inbox list did
not return: nothing is written for it."
  (let ((replies (and conversation (org-canvas--message-replies-of conversation))))
    (dolist (message replies)
      (org-canvas--message-write-reply pom conversation message))
    (length replies)))

(defun org-canvas--message-write-replies (entry details)
  "Write the replies of ENTRY's conversations beneath its heading.
ENTRY is (MARKER IDS); DETAILS maps conversation id to its detail, or
nil.  Returns how many replies were written: one conversation's
replies in posting order after the previous one's, keeping each
student's exchange together."
  (let ((written 0))
    (dolist (id (cadr entry))
      (cl-incf written (org-canvas--message-write-conversation
                        (car entry) (gethash id details))))
    written))

;;;###autoload
(defun org-canvas-pull-message-replies ()
  "Read what came back to the sent messages, into messages.org.
Every heading stamped CONVERSATION_IDS is read back: the Inbox's list
is walked for the course, each of those conversations is fetched, and
its messages after the one the send wrote become level-2 headings
beneath the message that started them, headed by who wrote them and
when.  A re-pull matches replies by MESSAGE_ID and updates them in
place; nothing local is deleted, and a conversation whose read fails
is skipped and named, never aborting the rest.

All GETs: nothing is sent, and a course marked `org-canvas-read-only'
allows it.  Never part of `org-canvas-sync', `org-canvas-pull-all' or
any tier, the way sending is — the half of the conversation that
lives in Org stays there."
  (interactive)
  (org-canvas--start-operation "PULLING MESSAGE REPLIES")
  (let ((file (expand-file-name org-canvas-messages-file)))
    (unless (file-exists-p file)
      (user-error "No %s to read replies from" (file-name-nondirectory file)))
    (org-canvas--pull-confirm-unsaved file "message replies"))
  (with-current-buffer (org-canvas--find-file-noselect
                        (expand-file-name org-canvas-messages-file))
    (let* ((entries (org-canvas--message-sent-entries
                     (expand-file-name org-canvas-messages-file)))
           (results (org-canvas--message-read-details entries))
           (details (car results))
           (skipped (cdr results))
           (replies 0))
      (org-canvas--log-info org-canvas--logger
        "[Messages] %d sent message(s) to read back" (length entries))
      (dolist (entry entries)
        (let ((written (org-canvas--message-write-replies entry details)))
          (cl-incf replies written)
          (org-canvas--log-info org-canvas--logger
            "[Pull] '%s': %d replies"
            (org-canvas--message-entry-subject (car entry)) written)))
      (org-canvas--save-buffer)
      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger ">>> MESSAGE REPLIES PULL COMPLETE")
      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger
        "Messages read: %d | Replies written: %d | Conversations skipped: %d"
        (length entries) replies skipped)
      (message "Message replies pull: %d messages, %d replies%s."
               (length entries) replies
               (if (> skipped 0)
                   (format ", %d conversations skipped" skipped)
                 "")))))

(provide 'org-canvas-messages)
;;; org-canvas-messages.el ends here
