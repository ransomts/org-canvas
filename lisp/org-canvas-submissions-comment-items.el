;;; org-canvas-submissions-comment-items.el --- The comment bank and a grading file's saved comments -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Comments are written once and reused: the comment bank holds the
;; instructor's standing comments on a column (issue #352), read from
;; and written to Canvas with the GraphQL API, and the Sent Comments
;; section of a grading file records every comment the column already
;; carries, so a comment is never posted twice and an edit to one can
;; be pushed (issue #419).
;;
;; Both are local data with a Canvas half: the bank is pulled and
;; pushed on command, the sent comments are read at pull and posted
;; with the grades.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; The grading file, its buffer-local state and the minor mode it
;; is read in: this module is above that file, never required by it.
(require 'org-canvas-submissions)

;; Defined below this file (a sibling split off the same module), so
;; declared rather than required: requiring would be a cycle.
(declare-function org-canvas--submissions-clear-score-p "org-canvas-submissions-push"
                  (text))
(declare-function org-canvas--submissions-comment-line "org-canvas-submissions-render"
                  (label id text &optional delete))
(declare-function org-canvas--submissions-comment-org-text "org-canvas-submissions-render"
                  (comment))
(declare-function org-canvas--submissions-format-number "org-canvas-submissions-render"
                  (n))
(declare-function org-canvas--submissions-left-p "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas--submissions-parse-score "org-canvas-submissions-push"
                  (score-string))
(declare-function org-canvas--submissions-section-region "org-canvas-submissions-render"
                  (heading))
(declare-function org-canvas--submissions-user-id "org-canvas-submissions-render"
                  (submission))
;; Variables another module of this split defines; values come with it.
(defvar org-canvas--submissions-clear-words)
(defvar org-canvas--submissions-comment-cache)
(defvar org-canvas--submissions-comments-heading)

;;;; Comment Bank (issue #352)

;; SpeedGrader's comment library is a list of saved comments a grader
;; picks from instead of typing.  Canvas keeps it per user and course,
;; readable as `User.commentBankItemsConnection' and written by the
;; create, update and delete CommentBankItem mutations.  A grading file
;; keeps it under a level-1 `* Comment Bank' heading, one list item per
;; saved comment: `- 4821 :: text' for one on Canvas, labelled with its
;; id, and `- text' for one to create.  The heading's
;; CANVAS_COMMENT_BANK property holds each labelled item's digest as
;; last read or written, the baseline that tells an item edited here
;; from one edited in SpeedGrader.  The section is the grader's: a
;; refresh carries it over as it stands, S sends what is new or edited
;; after reading the bank (an item whose text the bank already holds
;; is labelled, not created twice), B reads the bank in, and nothing
;; is ever deleted on Canvas but by x on the item.

(defconst org-canvas--submissions-bank-heading "* Comment Bank"
  "Heading the grading file's saved comments live under.
A level-1 heading with no USER_ID, which is no student.")

(defconst org-canvas--submissions-bank-query
  "query ($userId: ID!, $courseId: ID!, $cursor: String) { user(id: $userId) { commentBankItemsConnection(courseId: $courseId, first: 100, after: $cursor) { pageInfo { hasNextPage endCursor } nodes { _id comment } } } }"
  "The GraphQL query that reads the grader's saved comments in the course.
One page per request.  Checked against the Canvas schema by the
GraphQL contract test (issue #269), which names it by this symbol.")

(defconst org-canvas--submissions-bank-create-mutation
  "mutation ($courseId: ID!, $assignmentId: ID, $comment: String!) { createCommentBankItem(input: {courseId: $courseId, assignmentId: $assignmentId, comment: $comment}) { commentBankItem { _id comment } errors { attribute message } } }"
  "The GraphQL mutation that saves a comment in the grader's library.
Checked by the GraphQL contract test, which names it by this symbol.")

(defconst org-canvas--submissions-bank-update-mutation
  "mutation ($id: ID!, $comment: String!) { updateCommentBankItem(input: {id: $id, comment: $comment}) { commentBankItem { _id comment } errors { attribute message } } }"
  "The GraphQL mutation that rewrites a saved comment.
Checked by the GraphQL contract test, which names it by this symbol.")

(defconst org-canvas--submissions-bank-delete-mutation
  "mutation ($id: ID!) { deleteCommentBankItem(input: {id: $id}) { commentBankItemId errors { attribute message } } }"
  "The GraphQL mutation that removes a saved comment from the library.
Sent only by `org-canvas-submissions-delete-comment-bank-item'.
Checked by the GraphQL contract test, which names it by this symbol.")

(defconst org-canvas--submissions-bank-item-regexp
  "^-[ \t]+\\(?:\\([0-9]+\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?\\(.*\\)$"
  "Match the first line of a Comment Bank item.
Group 1 is the saved comment's id when the item is labelled with one,
group 2 the first line of its text.  Its other lines follow, indented,
up to the next item.")

(defun org-canvas--submissions-bank-region ()
  "Return (START . END) of the Comment Bank section, heading included, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^" (regexp-quote org-canvas--submissions-bank-heading) "[ \t]*$")
             nil t)
        (let ((start (line-beginning-position)))
          (cons start (if (re-search-forward "^\\* " nil t)
                          (line-beginning-position)
                        (point-max))))))))

(defun org-canvas--submissions-bank-digest (text)
  "Return the short digest of a saved comment's TEXT."
  (substring (sha1 (or text "")) 0 12))

(defun org-canvas--submissions-bank-item-text (start end)
  "Return the text of the Comment Bank item between START and END.
START is where its first line's text begins.  Org comment lines are
dropped and the rest normalized as a Rubric comment is
\(`org-canvas--submissions-comment-text')."
  (org-canvas--submissions-comment-text
   (mapconcat #'identity
              (seq-remove (lambda (l) (string-match-p "\\`[ \t]*#\\(?: \\|\\'\\)" l))
                          (split-string (buffer-substring-no-properties start end) "\n"))
              "\n")))

(defun org-canvas--submissions-bank-items ()
  "Return the Comment Bank items as plists, in order, or nil without any.
Each has :id (a string, nil for an item to create), :text, :start (the
item's first character) and :end (the end of its last non-blank line).
An item with no text is left out."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (save-excursion
      (goto-char (car region))
      (forward-line 1)
      (let ((items nil))
        (while (re-search-forward org-canvas--submissions-bank-item-regexp (cdr region) t)
          (let* ((id (match-string-no-properties 1))
                 (start (match-beginning 0))
                 (text-start (match-beginning 2))
                 (next (save-excursion
                         (forward-line 1)
                         (if (re-search-forward "^-[ \t]" (cdr region) t)
                             (match-beginning 0)
                           (cdr region))))
                 (end (save-excursion (goto-char next)
                                      (skip-chars-backward " \t\n" start)
                                      (point)))
                 (text (org-canvas--submissions-bank-item-text text-start (max text-start end))))
            (when text
              (push (list :id id :text text :start start :end (max text-start end)) items))
            (goto-char next)))
        (nreverse items)))))

(defun org-canvas--submissions-bank-baseline ()
  "Return the CANVAS_COMMENT_BANK baseline as an alist of (ID . DIGEST)."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (org-canvas--submissions-parse-digests
     (save-excursion (goto-char (car region))
                     (org-entry-get (point) "CANVAS_COMMENT_BANK")))))

(defun org-canvas--submissions-bank-set-baseline (baseline)
  "Write BASELINE, an alist of (ID . DIGEST), as CANVAS_COMMENT_BANK.
Only the ids an item still carries are kept; none leaves no property."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (let* ((ids (delq nil (mapcar (lambda (i) (plist-get i :id))
                                  (org-canvas--submissions-bank-items))))
           (kept (seq-filter (lambda (pair) (member (car pair) ids)) baseline))
           (value (mapconcat (lambda (pair) (format "%s=%s" (car pair) (cdr pair)))
                             (sort (copy-sequence kept)
                                   (lambda (a b) (< (string-to-number (car a))
                                                    (string-to-number (car b)))))
                             " ")))
      (save-excursion
        (goto-char (car region))
        (if (string-empty-p value)
            (org-entry-delete (point) "CANVAS_COMMENT_BANK")
          (org-entry-put (point) "CANVAS_COMMENT_BANK" value))))))

(defun org-canvas--submissions-bank-pending-p (item baseline)
  "Return non-nil when ITEM is still to send, against BASELINE.
An item without an id is to create; a labelled one whose text no
longer digests to its BASELINE entry, or has none, is to compare."
  (or (null (plist-get item :id))
      (not (equal (cdr (assoc (plist-get item :id) baseline))
                  (org-canvas--submissions-bank-digest (plist-get item :text))))))

(defun org-canvas--submissions-bank-pending ()
  "Return the Comment Bank items a push has to send, in order."
  (let ((baseline (org-canvas--submissions-bank-baseline)))
    (seq-filter (lambda (item) (org-canvas--submissions-bank-pending-p item baseline))
                (org-canvas--submissions-bank-items))))

(defun org-canvas--submissions-self-id ()
  "Return the token owner's Canvas user id as a string.
The comment bank is theirs: Canvas keeps it per user."
  (let ((me (org-canvas-api-request 'GET (org-canvas--api-users-self-url))))
    (or (and (alist-get 'id me) (format "%s" (alist-get 'id me)))
        (org-canvas--signal 'org-canvas-api-error "users/self answered no id"))))

(defun org-canvas--submissions-fetch-bank ()
  "Return the grader's saved comments in the course, or nil when unreadable.
The value is a list of (ID . TEXT), in the order Canvas answers, the
text normalized as an item's is.  A failed read is one warning and
nil; an empty bank is `empty', so the two are never confused."
  (condition-case err
      (let* ((user-id (org-canvas--submissions-self-id))
             (bank (org-canvas--graphql-walk-pages
                    org-canvas--submissions-bank-query
                    (list (cons 'userId user-id)
                          (cons 'courseId (format "%s" org-canvas-course-id)))
                    '(user commentBankItemsConnection)
                    (lambda (node map)
                      (let ((id (org-canvas--alist-get-non-null '_id node))
                            (text (org-canvas--submissions-comment-text
                                   (org-canvas--alist-get-non-null 'comment node))))
                        (when (and id text)
                          (puthash (format "%s" id) text map))))))
             (items nil))
        (maphash (lambda (id text) (push (cons id text) items)) bank)
        (or (nreverse items) 'empty))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Could not read the comment bank (%s); nothing sent to it"
       (error-message-string err))
     nil)))

(defun org-canvas--submissions-bank-reply-item (data field)
  "Return the saved comment FIELD of DATA carries, as (ID . TEXT).
DATA is a create or update mutation's reply.  Errors the payload
carries signal `org-canvas-api-error' naming them; a reply with no
item is an error too, since there is no id to label the item with."
  (let* ((payload (alist-get field data))
         (errors (org-canvas--alist-get-non-null 'errors payload))
         (item (org-canvas--alist-get-non-null 'commentBankItem payload)))
    (when (and errors (> (length errors) 0))
      (org-canvas--signal 'org-canvas-api-error
        "%s: %s" field (org-canvas--graphql-errors-message errors)))
    (unless (and (consp item) (org-canvas--alist-get-non-null '_id item))
      (org-canvas--signal 'org-canvas-api-error "%s answered no saved comment" field))
    (cons (format "%s" (alist-get '_id item))
          (org-canvas--submissions-comment-text (alist-get 'comment item)))))

(defun org-canvas--submissions-bank-create (assignment-id text)
  "Save TEXT in the bank for ASSIGNMENT-ID; return (ID . TEXT) or `dry-run'."
  (let ((data (org-canvas--graphql-mutate
               (format "save a comment in the comment bank: %s" (truncate-string-to-width text 40))
               org-canvas--submissions-bank-create-mutation
               (list (cons 'courseId (format "%s" org-canvas-course-id))
                     (cons 'assignmentId (format "%s" assignment-id))
                     (cons 'comment text)))))
    (if (org-canvas--dry-run-response-p data)
        'dry-run
      (org-canvas--submissions-bank-reply-item data 'createCommentBankItem))))

(defun org-canvas--submissions-bank-update (id text)
  "Rewrite the saved comment ID as TEXT; return (ID . TEXT) or `dry-run'."
  (let ((data (org-canvas--graphql-mutate
               (format "rewrite saved comment %s" id)
               org-canvas--submissions-bank-update-mutation
               (list (cons 'id id) (cons 'comment text)))))
    (if (org-canvas--dry-run-response-p data)
        'dry-run
      (org-canvas--submissions-bank-reply-item data 'updateCommentBankItem))))

(defun org-canvas--submissions-bank-label (item id)
  "Label ITEM, an item without an id, with ID in the buffer."
  (save-excursion
    (goto-char (plist-get item :start))
    (when (looking-at "-[ \t]+")
      (replace-match (format "- %s :: " id) t t))))

(defun org-canvas--submissions-bank-send-new (item live assignment-id)
  "Send ITEM, which has no id, unless its text is in LIVE already.
LIVE is the bank as `org-canvas--submissions-fetch-bank' read it;
ASSIGNMENT-ID names the column a created comment is saved for.
Return (KIND ID . TEXT), KIND `adopted' or `created', or nil under a
dry run."
  (let ((twin (rassoc (plist-get item :text) live)))
    (if twin
        (cons 'adopted twin)
      (let ((made (org-canvas--submissions-bank-create assignment-id (plist-get item :text))))
        (unless (eq made 'dry-run)
          (cons 'created made))))))

(defun org-canvas--submissions-bank-send-edit (item live baseline)
  "Send ITEM, a labelled item, when it was edited here and not on Canvas.
LIVE and BASELINE are the bank as read and the file's baseline.
Return (KIND ID . TEXT): `same' when Canvas holds the text already,
`updated' when it was rewritten, `gone' when the bank no longer has
the id, `conflict' when Canvas's text moved off the baseline too; nil
under a dry run."
  (let* ((id (plist-get item :id))
         (remote (cdr (assoc id live)))
         (base (cdr (assoc id baseline))))
    (cond ((null remote) (cons 'gone (cons id nil)))
          ((equal remote (plist-get item :text)) (cons 'same (cons id remote)))
          ((and base (not (equal base (org-canvas--submissions-bank-digest remote))))
           (cons 'conflict (cons id remote)))
          (t (let ((made (org-canvas--submissions-bank-update id (plist-get item :text))))
               (unless (eq made 'dry-run)
                 (cons 'updated made)))))))

(defun org-canvas--submissions-bank-send-item (item live baseline assignment-id)
  "Send one pending ITEM; return its outcome, or (failed nil . TEXT).
LIVE, BASELINE and ASSIGNMENT-ID as the two senders take them.  A
failed request is one warning, and the item stays pending."
  (condition-case err
      (if (plist-get item :id)
          (org-canvas--submissions-bank-send-edit item live baseline)
        (org-canvas--submissions-bank-send-new item live assignment-id))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Saved comment not sent (%s): %s"
       (truncate-string-to-width (plist-get item :text) 40)
       (error-message-string err))
     (cons 'failed (cons nil (plist-get item :text))))))

(defun org-canvas--submissions-bank-warn (outcome)
  "Log the warning for OUTCOME of a labelled item, if it needs one."
  (pcase (car outcome)
    ('gone (org-canvas--log-warning org-canvas--logger
             "[Submissions] Saved comment %s is no longer in the bank; remove its label to create it again"
             (cadr outcome)))
    ('conflict (org-canvas--log-warning org-canvas--logger
                 "[Submissions] Saved comment %s was edited here and in SpeedGrader; B reads Canvas's text in"
                 (cadr outcome)))))

(defconst org-canvas--submissions-bank-count-keys
  '((created . :created) (adopted . :adopted) (updated . :updated)
    (gone . :skipped) (conflict . :skipped) (failed . :failed))
  "The count each outcome of a Comment Bank item adds to.
An item Canvas already held as typed (`same') counts nowhere.")

(defun org-canvas--submissions-bank-record-one (item outcome baseline counts)
  "Record OUTCOME of ITEM in the buffer, BASELINE and COUNTS.
Return BASELINE, which may have gained an entry; COUNTS is changed in
place.  Under `org-canvas--dry-run' the buffer is left alone (issue
#442)."
  (let ((kind (car outcome))
        (key (cdr (assq (car outcome) org-canvas--submissions-bank-count-keys))))
    (org-canvas--submissions-bank-warn outcome)
    (when key
      (plist-put counts key (1+ (plist-get counts key))))
    (when (and (memq kind '(created adopted)) (not org-canvas--dry-run))
      (org-canvas--submissions-bank-label item (cadr outcome)))
    (when (memq kind '(created adopted updated same))
      (setf (alist-get (cadr outcome) baseline nil nil #'equal)
            (org-canvas--submissions-bank-digest (plist-get item :text))))
    baseline))

(defun org-canvas--submissions-bank-record (pending outcomes)
  "Record OUTCOMES of PENDING items in the buffer; return the counts.
Items are labelled and the baseline rewritten, last item first so
the positions of the others hold; an outcome of nil (a dry run)
records nothing.  The counts are a plist of :created, :adopted,
:updated, :skipped (gone from Canvas or edited there too) and
:failed."
  (let ((baseline (org-canvas--submissions-bank-baseline))
        (counts (list :created 0 :adopted 0 :updated 0 :skipped 0 :failed 0)))
    (cl-mapc (lambda (item outcome)
               (when outcome
                 (setq baseline (org-canvas--submissions-bank-record-one
                                 item outcome baseline counts))))
             (reverse pending) (reverse outcomes))
    (unless org-canvas--dry-run
      (org-canvas--submissions-bank-set-baseline baseline))
    counts))

(defun org-canvas--submissions-push-bank (assignment-id)
  "Send the Comment Bank's new and edited items for ASSIGNMENT-ID.
The bank is read first, so an item whose text it already holds is
labelled with that id rather than created twice (the duplicate
guard's rule, Hard Rule 20), and an item edited in SpeedGrader since
it was last read is not overwritten.  Nothing is deleted.  Return the
counts of `org-canvas--submissions-bank-record', or nil when nothing
was pending or the bank could not be read."
  (when-let* ((pending (org-canvas--submissions-bank-pending))
              (live (org-canvas--submissions-fetch-bank)))
    (let* ((live (if (eq live 'empty) nil live))
           (baseline (org-canvas--submissions-bank-baseline))
           (outcomes (mapcar (lambda (item)
                               (org-canvas--submissions-bank-send-item
                                item live baseline assignment-id))
                             pending)))
      (org-canvas--submissions-bank-record pending outcomes))))

(defun org-canvas--submissions-describe-bank (counts &optional pending)
  "Return the push message note for the bank COUNTS, or \"\".
With PENDING items and no COUNTS the bank could not be read, and the
note says so."
  (if (null counts)
      (if pending "; comment bank not read (see the log)" "")
    (let ((parts (delq nil (list (org-canvas--submissions-count-part counts :created "saved")
                                 (org-canvas--submissions-count-part counts :adopted "already in the bank")
                                 (org-canvas--submissions-count-part counts :updated "rewritten")
                                 (org-canvas--submissions-count-part counts :skipped "skipped")
                                 (org-canvas--submissions-count-part counts :failed "failed")))))
      (if parts
          (format "; comment bank: %s" (string-join parts ", "))
        ""))))

(defun org-canvas--submissions-count-part (counts key label)
  "Return \"N LABEL\" for KEY of COUNTS, or nil when it is 0."
  (let ((n (plist-get counts key)))
    (and n (> n 0) (format "%d %s" n label))))

(defun org-canvas--submissions-render-bank-heading ()
  "Insert the Comment Bank heading with its template, when enabled."
  (when org-canvas-submissions-comment-bank-template
    (insert "\n" org-canvas--submissions-bank-heading "\n"
            org-canvas-submissions-comment-bank-template "\n")))

(defun org-canvas--submissions-bank-carryover ()
  "Return the Comment Bank section as it stands, heading included, or nil."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (let ((text (string-trim-right
                 (buffer-substring-no-properties (car region) (cdr region)))))
      (concat text "\n"))))

(defun org-canvas--submissions-restore-bank (text)
  "Put TEXT, a carried Comment Bank section, back in the buffer.
It replaces the section a render wrote, or goes before the first
student when there is none."
  (save-excursion
    (let ((region (org-canvas--submissions-bank-region)))
      (if region
          (progn (delete-region (car region) (cdr region))
                 (goto-char (car region)))
        (goto-char (point-min))
        (if (re-search-forward "^[ \t]*:USER_ID:" nil t)
            (org-back-to-heading t)
          (goto-char (point-max))))
      (insert text)
      (when (looking-at "^\\*") (insert "\n")))))

(defun org-canvas--submissions-bank-merge (live)
  "Bring LIVE, the bank as read, into the Comment Bank section.
A labelled item unedited here takes Canvas's text; an item Canvas
holds that the section lacks is added, labelled, at the end.  Items
edited here, and those Canvas no longer holds, are left as they are.
Return (:added N :rewritten N)."
  (let* ((baseline (org-canvas--submissions-bank-baseline))
         (items (org-canvas--submissions-bank-items))
         (added 0) (rewritten 0))
    (dolist (item (reverse items))
      (let ((remote (cdr (assoc (plist-get item :id) live))))
        (when (and remote
                   (not (equal remote (plist-get item :text)))
                   (not (org-canvas--submissions-bank-pending-p item baseline)))
          (save-excursion
            (delete-region (plist-get item :start) (plist-get item :end))
            (goto-char (plist-get item :start))
            (insert (org-canvas--submissions-item (plist-get item :id) remote)))
          (cl-incf rewritten))))
    (let ((known (mapcar (lambda (i) (plist-get i :id)) items))
          (region (org-canvas--submissions-bank-region)))
      (save-excursion
        (goto-char (cdr region))
        (skip-chars-backward " \t\n" (car region))
        (dolist (pair live)
          (unless (member (car pair) known)
            (insert "\n" (org-canvas--submissions-item (car pair) (cdr pair)))
            (cl-incf added)))))
    (dolist (pair live)
      (let ((item (cl-find (car pair) (org-canvas--submissions-bank-items)
                           :key (lambda (i) (plist-get i :id)) :test #'equal)))
        (when (and item (equal (plist-get item :text) (cdr pair)))
          (setf (alist-get (car pair) baseline nil nil #'equal)
                (org-canvas--submissions-bank-digest (cdr pair))))))
    (org-canvas--submissions-bank-set-baseline baseline)
    (list :added added :rewritten rewritten)))

(defun org-canvas--submissions-ensure-bank-section ()
  "Return the Comment Bank region, writing an empty section when missing."
  (or (org-canvas--submissions-bank-region)
      (progn
        (org-canvas--submissions-restore-bank
         (concat org-canvas--submissions-bank-heading "\n"
                 (if org-canvas-submissions-comment-bank-template
                     (concat org-canvas-submissions-comment-bank-template "\n")
                   "")))
        (org-canvas--submissions-bank-region))))

;;;###autoload
(defun org-canvas-submissions-pull-comment-bank ()
  "Read the grader's comment bank into this grading file's Comment Bank.
Every saved comment of the course's library comes in as an item
labelled with its id; one already there takes Canvas's text unless it
was edited here since it was last read.  Nothing is sent and nothing
is removed from the section."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (let ((live (org-canvas--submissions-fetch-bank)))
    (unless live
      (user-error "Could not read the comment bank; see the log"))
    (let ((inhibit-read-only t))
      (org-canvas--submissions-ensure-bank-section)
      (let ((counts (org-canvas--submissions-bank-merge (if (eq live 'empty) nil live))))
        (when buffer-file-name (save-buffer))
        (message "Comment bank: %d added, %d rewritten from Canvas"
                 (plist-get counts :added) (plist-get counts :rewritten))))))

(defun org-canvas--submissions-bank-item-at-point ()
  "Return the Comment Bank item point is on, or nil."
  (let ((pos (point)))
    (cl-find-if (lambda (item)
                  (and (<= (save-excursion (goto-char (plist-get item :start))
                                           (line-beginning-position))
                           pos)
                       (<= pos (save-excursion (goto-char (plist-get item :end))
                                               (line-end-position)))))
                (org-canvas--submissions-bank-items))))

(defun org-canvas--submissions-bank-delete (id)
  "Delete the saved comment ID from the bank; return non-nil when sent."
  (let* ((data (org-canvas--graphql-mutate
                (format "delete saved comment %s" id)
                org-canvas--submissions-bank-delete-mutation
                (list (cons 'id id))))
         (payload (and (not (org-canvas--dry-run-response-p data))
                       (alist-get 'deleteCommentBankItem data)))
         (errors (org-canvas--alist-get-non-null 'errors payload)))
    (when (and errors (> (length errors) 0))
      (org-canvas--signal 'org-canvas-api-error
        "deleteCommentBankItem: %s" (org-canvas--graphql-errors-message errors)))
    (and payload t)))

;;;###autoload
(defun org-canvas-submissions-delete-comment-bank-item ()
  "Delete the saved comment at point from the comment bank, after asking.
The item must carry the id of a comment on Canvas; it is removed from
the Comment Bank section once Canvas has deleted it.  The only way
org-canvas deletes a saved comment: removing an item from the section
never does."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (let ((item (org-canvas--submissions-bank-item-at-point)))
    (unless (and item (plist-get item :id))
      (user-error "No saved comment with an id at point"))
    ;; Deleting a saved comment is a submissions write, refused before
    ;; the question (issue #550).
    (org-canvas--check-write-feature 'submissions "deleting a saved comment")
    (when (y-or-n-p (format "Delete saved comment %s from the comment bank? "
                            (plist-get item :id)))
      (if (not (org-canvas--submissions-bank-delete (plist-get item :id)))
          (message "Dry run: saved comment %s left in place" (plist-get item :id))
        (let ((inhibit-read-only t)
              (baseline (org-canvas--submissions-bank-baseline)))
          (delete-region (plist-get item :start)
                         (save-excursion (goto-char (plist-get item :end))
                                         (min (point-max) (1+ (line-end-position)))))
          (org-canvas--submissions-bank-set-baseline baseline))
        (when buffer-file-name (save-buffer))
        (message "Saved comment %s deleted" (plist-get item :id))))))

;;;; Sent Comments (issue #419)

;; A comment already sent sits under the student's `** Comments'
;; heading as `- *Author* <time> [ID] :: text'.  The heading's
;; CANVAS_COMMENTS property holds each id's text digest as last pulled
;; or pushed, so an item whose text no longer digests to its entry was
;; edited here, and `- DELETE *Author* ...' marks one to delete; a line
;; removed from the section is never a deletion, since a missing line
;; is as likely a tidy-up as an intent.  S sends the grader's own
;; edited comments (PUT) and deletions (DELETE) after re-reading them:
;; one by someone else, one edited on Canvas since the pull, or one
;; gone from Canvas is refused and named, not sent.  A refresh keeps an
;; edit or a mark that was not sent.  An item without an id (a file
;; pulled before #419, or a comment posted from a draft) is read-only
;; until a refresh gives it one.

(defconst org-canvas--submissions-comment-item-regexp
  (concat "^-[ \t]+\\(DELETE[ \t]+\\)?"
          "\\(\\(?:[^:\n]\\|:[^:\n]\\)*?\\)[ \t]+\\[\\([0-9]+\\)\\][ \t]*::"
          "\\(?:[ \t]+\\(.*\\)\\)?$")
  "Match the first line of a Comments item that carries a comment id.
Group 1 is the DELETE mark, group 2 the label (the author and the
time), group 3 the id and group 4 the first line of the text, absent
when the text starts under the item.  The label holds no `::', so a
text that does cannot be mistaken for one.")

(defun org-canvas--submissions-comment-digest (text)
  "Return the digest of a sent comment's TEXT, normalized as an item's is."
  (org-canvas--submissions-bank-digest (org-canvas--submissions-comment-text text)))

(defun org-canvas--submissions-parse-digests (value)
  "Return VALUE, a string of ID=DIGEST pairs, as an alist of (ID . DIGEST)."
  (when value
    (delq nil (mapcar (lambda (pair)
                        (when (string-match "\\`\\([0-9]+\\)=\\([0-9a-f]+\\)\\'" pair)
                          (cons (match-string 1 pair) (match-string 2 pair))))
                      (split-string value)))))

(defun org-canvas--submissions-comment-baseline-value (comments)
  "Return the CANVAS_COMMENTS value for COMMENTS, or nil when none has an id."
  (let ((pairs (delq nil
                     (mapcar (lambda (c)
                               (when-let* ((id (org-canvas--alist-get-non-null 'id c)))
                                 (format "%s=%s" id
                                         (org-canvas--submissions-comment-digest
                                          (org-canvas--submissions-comment-org-text c)))))
                             (append comments nil)))))
    (and pairs (string-join pairs " "))))

(defun org-canvas--submissions-insert-comment-baseline (comments)
  "Insert the CANVAS_COMMENTS property line for COMMENTS, when any has an id."
  (when-let* ((value (org-canvas--submissions-comment-baseline-value comments)))
    (insert (format ":CANVAS_COMMENTS: %s\n" value))))

(defun org-canvas--submissions-comment-baseline ()
  "Return the CANVAS_COMMENTS baseline of the entry at point, (ID . DIGEST)."
  (org-canvas--submissions-parse-digests (org-entry-get (point) "CANVAS_COMMENTS")))

(defun org-canvas--submissions-set-comment-digest (id digest)
  "Set comment ID's CANVAS_COMMENTS entry at point to DIGEST; nil drops it."
  (let* ((old (org-canvas--submissions-comment-baseline))
         (new (if (assoc id old)
                  (delq nil (mapcar (lambda (pair)
                                      (if (equal (car pair) id) (and digest (cons id digest)) pair))
                                    old))
                (append old (and digest (list (cons id digest)))))))
    (if new
        (org-entry-put (point) "CANVAS_COMMENTS"
                       (mapconcat (lambda (p) (format "%s=%s" (car p) (cdr p))) new " "))
      (org-entry-delete (point) "CANVAS_COMMENTS"))))

(defun org-canvas--submissions-comment-item-end (start bound)
  "Return the end of the item starting at START, BOUND the section's end.
It is the end of the item's last non-blank line before the next item."
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (goto-char (if (and (<= (point) bound) (re-search-forward "^-[ \t]" bound t))
                   (match-beginning 0)
                 bound))
    (skip-chars-backward " \t\n" start)
    (point)))

(defun org-canvas--submissions-sent-comments ()
  "Return the Comments items of the entry at point that carry an id.
Each is a plist: :id (a string), :label, :delete (non-nil when marked
DELETE), :text (normalized as a Rubric comment is, nil when empty),
:start and :end, the end of its last non-blank line.  An item without
an id is left out, since nothing can address it."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-comments-heading)))
    (save-excursion
      (goto-char (car region))
      (let ((bound (save-excursion (goto-char (cdr region))
                                   (if (bolp) (point) (line-end-position))))
            (items nil))
        (while (re-search-forward org-canvas--submissions-comment-item-regexp bound t)
          (let* ((start (match-beginning 0))
                 (text-start (or (match-beginning 4) (match-end 0)))
                 (item (list :id (match-string-no-properties 3)
                             :label (match-string-no-properties 2)
                             :delete (and (match-beginning 1) t)))
                 (end (max text-start (org-canvas--submissions-comment-item-end start bound))))
            (push (append item
                          (list :start start :end end
                                :text (org-canvas--submissions-comment-text
                                       (buffer-substring-no-properties text-start end))))
                  items)
            (goto-char (max end (line-end-position)))))
        (nreverse items)))))

(defun org-canvas--submissions-find-sent-comment (id)
  "Return the Comments item of the entry at point whose id is ID, or nil."
  (seq-find (lambda (item) (equal (plist-get item :id) id))
            (org-canvas--submissions-sent-comments)))

(defun org-canvas--submissions-comment-pending-p (item baseline)
  "Return non-nil when ITEM, a sent comment, has a change to send.
BASELINE is the entry's CANVAS_COMMENTS alist.  An item marked DELETE
has; so does one whose text no longer digests to its entry.  An item
the baseline does not name cannot be told edited, and has not."
  (or (plist-get item :delete)
      (when-let* ((digest (cdr (assoc (plist-get item :id) baseline))))
        (not (equal digest (org-canvas--submissions-comment-digest
                            (plist-get item :text)))))))

(defun org-canvas--submissions-comment-edits-at-point (user-id)
  "Return the pending sent comments of USER-ID's heading at point.
Each is a `org-canvas--submissions-sent-comments' plist with :user-id
\(a number), :name and :baseline, the digest it was edited against."
  (let ((baseline (org-canvas--submissions-comment-baseline))
        (name (org-get-heading t t t t)))
    (mapcar (lambda (item)
              (append (list :user-id (string-to-number user-id) :name name
                            :baseline (cdr (assoc (plist-get item :id) baseline)))
                      item))
            (seq-filter (lambda (item) (org-canvas--submissions-comment-pending-p item baseline))
                        (org-canvas--submissions-sent-comments)))))

(defun org-canvas--submissions-comment-edits-by-student (&optional skip-left)
  "Return (USER-ID . EDITS) for every student whose sent comments changed here.
With SKIP-LEFT a student who left the course is passed over."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (when-let* ((user-id (org-entry-get (point) "USER_ID"))
                    ((not (and skip-left (org-canvas--submissions-left-p))))
                    (edits (org-canvas--submissions-comment-edits-at-point user-id)))
          (push (cons (string-to-number user-id) edits) found))
        (forward-line 1)))
    (nreverse found)))

(defun org-canvas--submissions-collect-comment-edits ()
  "Return every sent comment edited or marked DELETE here, in file order.
A student who left the course is skipped, as their draft is."
  (apply #'append (mapcar #'cdr (org-canvas--submissions-comment-edits-by-student t))))

(defun org-canvas--submissions-pending-comment-edits (assignment-id &optional only)
  "Return the sent comments changed here as (SENDABLE . REFUSED), or nil.
The changes are those `org-canvas--submissions-collect-comment-edits'
finds, checked against ASSIGNMENT-ID on Canvas by
`org-canvas--submissions-check-comment-edits'.  The full push and the
comment-only push both start here (issue #425).  ONLY, a list of user
ids, keeps those students' changes alone (issue #441)."
  (org-canvas--submissions-check-comment-edits
   assignment-id (org-canvas--submissions-only-rows
                  only (org-canvas--submissions-collect-comment-edits))))

(defun org-canvas--submissions-only-rows (only rows)
  "Return ROWS, plists with a :user-id, restricted to the user ids ONLY lists.
Nil ONLY keeps every row (issue #441)."
  (if only
      (seq-filter (lambda (r) (memql (plist-get r :user-id) only)) rows)
    rows))

(defun org-canvas--submissions-live-comments (assignment-id user-ids)
  "Return a hash from comment id to (AUTHOR-ID . DIGEST) on ASSIGNMENT-ID.
Only the submissions of USER-IDS are read into it, the digest taken
of each comment's text as its item would show it."
  (let ((live (make-hash-table :test 'equal))
        (org-canvas--submissions-comment-cache (make-hash-table :test 'equal)))
    (dolist (sub (append (org-canvas--submissions-fetch-for-assignment assignment-id) nil))
      (when (memql (org-canvas--submissions-user-id sub) user-ids)
        (dolist (c (append (alist-get 'submission_comments sub) nil))
          (puthash (format "%s" (alist-get 'id c))
                   (cons (alist-get 'author_id c)
                         (org-canvas--submissions-comment-digest
                          (org-canvas--submissions-comment-org-text c)))
                   live))))
    live))

(defun org-canvas--submissions-read-live-comments (assignment-id edits)
  "Return (SELF . LIVE) for EDITS on ASSIGNMENT-ID, or `unread'.
SELF is the token owner's user id and LIVE the hash of
`org-canvas--submissions-live-comments'.  A failed read is one warning."
  (condition-case err
      (cons (org-canvas--submissions-self-id)
            (org-canvas--submissions-live-comments
             assignment-id (delete-dups (mapcar (lambda (e) (plist-get e :user-id)) edits))))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Could not read the sent comments (%s); none edited or deleted"
       (error-message-string err))
     'unread)))

(defun org-canvas--submissions-comment-refusal (edit live self)
  "Return why EDIT may not be sent, or nil when it may.
LIVE is the comment's (AUTHOR-ID . DIGEST) on Canvas, nil when Canvas
no longer holds it; SELF is the token owner's user id, a string.
Canvas lets only a comment's author rewrite it, and the push holds
deletions to the same rule, so another grader's comment stays theirs."
  (cond ((and (not (plist-get edit :delete)) (null (plist-get edit :text)))
         "emptied; mark it DELETE to delete it")
        ((null live) "no longer on Canvas")
        ((not (equal (format "%s" (car live)) self))
         "not yours; only its author may change it")
        ((and (plist-get edit :baseline)
              (not (equal (cdr live) (plist-get edit :baseline))))
         "edited on Canvas since the pull")))

(defun org-canvas--submissions-check-comment-edits (assignment-id edits)
  "Split EDITS into (SENDABLE . REFUSED) against ASSIGNMENT-ID on Canvas.
The grader's user id and the students' comments are read once; a
refused edit carries its :refused reason and is logged as a warning.
When Canvas cannot be read every edit is refused, since neither the
author nor the baseline could be checked.  Nil without EDITS."
  (when edits
    (let ((read (org-canvas--submissions-read-live-comments assignment-id edits))
          (ok nil)
          (bad nil))
      (dolist (edit edits)
        (let ((why (if (eq read 'unread)
                       "Canvas could not be read"
                     (org-canvas--submissions-comment-refusal
                      edit (gethash (plist-get edit :id) (cdr read)) (car read)))))
          (if (not why)
              (push edit ok)
            (org-canvas--log-warning org-canvas--logger
              "[Submissions] Comment %s on %s not changed: %s"
              (plist-get edit :id) (plist-get edit :name) why)
            (push (append (list :refused why) edit) bad))))
      (cons (nreverse ok) (nreverse bad)))))

(defun org-canvas--submissions-comment-url (assignment-id edit)
  "Return the URL of EDIT's sent comment on ASSIGNMENT-ID.
Canvas addresses the submission by the student's user id (#125)."
  (org-canvas-api-course-endpoint "assignments/%s/submissions/%s/comments/%s"
                                  assignment-id (plist-get edit :user-id) (plist-get edit :id)))

(defun org-canvas--submissions-comment-edit-payload (text)
  "Return the body of the request that rewrites a sent comment as TEXT.
Canvas's `Edit a submission comment' takes the text as a top-level
`comment', not wrapped as a new comment's `text_comment' is."
  `((comment . ,text)))

(defun org-canvas--submissions-send-comment-edit (assignment-id edit)
  "Send EDIT, a sent comment of ASSIGNMENT-ID to rewrite or delete.
Return `dry-run' under `org-canvas--dry-run', when nothing is sent
\(Hard Rule 1); `deleted' after a deletion; else Canvas's reply, the
comment as it stored it."
  (let ((url (org-canvas--submissions-comment-url assignment-id edit))
        (verb (if (plist-get edit :delete) "delete" "edit")))
    (cond (org-canvas--dry-run
           (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s comment %s on %s"
             verb (plist-get edit :id) (plist-get edit :name))
           'dry-run)
          ((plist-get edit :delete)
           (org-canvas-api-request 'DELETE url)
           'deleted)
          (t (org-canvas-api-request
              'PUT url :data (org-canvas--submissions-comment-edit-payload
                              (plist-get edit :text)))))))

(defun org-canvas--submissions-rewrite-sent-comment (item text &optional delete)
  "Replace ITEM of the entry at point by its label, id and TEXT.
With DELETE the item is marked for deletion."
  (save-excursion
    (goto-char (plist-get item :start))
    (delete-region (plist-get item :start) (plist-get item :end))
    (insert (org-canvas--submissions-comment-line
             (plist-get item :label) (plist-get item :id) (or text "") delete))))

(defun org-canvas--submissions-record-comment-edit (edit reply)
  "Record under EDIT's student what Canvas answered, REPLY.
A deleted comment's item and baseline entry go; a rewritten one shows
the text Canvas stored, which becomes its baseline."
  (save-excursion
    (when-let* (((org-canvas--submissions-goto-user (plist-get edit :user-id)))
                (item (org-canvas--submissions-find-sent-comment (plist-get edit :id))))
      (if (eq reply 'deleted)
          (progn
            (delete-region (plist-get item :start)
                           (min (point-max) (1+ (plist-get item :end))))
            (org-canvas--submissions-set-comment-digest (plist-get edit :id) nil))
        (let ((text (if (stringp (org-canvas--alist-get-non-null 'comment reply))
                        (org-canvas--submissions-comment-org-text reply)
                      (plist-get edit :text))))
          (org-canvas--submissions-rewrite-sent-comment item text)
          (org-canvas--submissions-set-comment-digest
           (plist-get edit :id) (org-canvas--submissions-comment-digest text)))))))

(defun org-canvas--submissions-apply-comment-edit (assignment-id edit)
  "Send EDIT to ASSIGNMENT-ID and record it; return the count it falls under.
The count is :edited, :deleted, :dry-run, :failed for a request
Canvas refused or did not answer, or :errored for an error in Emacs,
which is not Canvas's doing (`org-canvas--api-failure-p', issue #443).
Either is one warning and leaves the item pending."
  (condition-case err
      (let ((reply (org-canvas--submissions-send-comment-edit assignment-id edit)))
        (if (eq reply 'dry-run)
            :dry-run
          (org-canvas--submissions-record-comment-edit edit reply)
          (if (eq reply 'deleted) :deleted :edited)))
    (error
     (let ((canvas (org-canvas--api-failure-p err)))
       (org-canvas--log-warning org-canvas--logger
         "[Submissions] Could not %s comment %s on %s%s: %s"
         (if (plist-get edit :delete) "delete" "edit")
         (plist-get edit :id) (plist-get edit :name)
         (if canvas "" " (an error in Emacs, not from Canvas)")
         (error-message-string err))
       (if canvas :failed :errored)))))

(defun org-canvas--submissions-apply-comment-edits (assignment-id edits)
  "Send each of EDITS to ASSIGNMENT-ID; return the counts, or nil without any.
The plist has :edited, :deleted, :dry-run, :failed and :errored."
  (when edits
    (let ((counts (list :edited 0 :deleted 0 :dry-run 0 :failed 0 :errored 0)))
      (dolist (edit edits)
        (let ((key (org-canvas--submissions-apply-comment-edit assignment-id edit)))
          (plist-put counts key (1+ (plist-get counts key)))))
      counts)))

(defun org-canvas--submissions-describe-comment-edits (edits)
  "Return the confirmation's words for EDITS, sent comments to send, or nil."
  (let* ((deletes (cl-count-if (lambda (e) (plist-get e :delete)) edits))
         (rewrites (- (length edits) deletes)))
    (when edits
      (string-join (delq nil (list (when (> rewrites 0) (format "%d comment edit(s)" rewrites))
                                   (when (> deletes 0) (format "%d comment deletion(s)" deletes))))
                   " and "))))

(defun org-canvas--submissions-list-comment-edits (comments)
  "Return COMMENTS, (SENDABLE . REFUSED) edits, one line each for the prompt."
  (mapconcat (lambda (e)
               (format "  %s: %s comment %s%s"
                       (plist-get e :name)
                       (cond ((plist-get e :refused) "not sending")
                             ((plist-get e :delete) "delete")
                             (t "edit"))
                       (plist-get e :id)
                       (if (plist-get e :refused) (format " (%s)" (plist-get e :refused)) "")))
             (append (car comments) (cdr comments)) "\n"))

(defun org-canvas--submissions-refused-note (refused)
  "Return the note a push message ends with for REFUSED comment edits."
  (if refused
      (format "; %d comment change(s) not sent (see the log)" (length refused))
    ""))

(defun org-canvas--submissions-count-note (counts key words)
  "Return \", N WORDS (see the log)\" for KEY's count N in COUNTS, or \"\"."
  (let ((n (or (plist-get counts key) 0)))
    (if (> n 0) (format ", %d %s (see the log)" n words) "")))

(defun org-canvas--submissions-comment-edits-note (counts refused)
  "Return the push message's note on sent comments, from COUNTS and REFUSED.
COUNTS is what `org-canvas--submissions-apply-comment-edits' returned."
  (concat
   (cond ((null counts) "")
         ((> (plist-get counts :dry-run) 0)
          (format "; dry run: would change %d sent comment(s)" (plist-get counts :dry-run)))
         (t (format "; %d sent comment(s) edited, %d deleted%s%s"
                    (plist-get counts :edited) (plist-get counts :deleted)
                    (org-canvas--submissions-count-note
                     counts :failed "failed at Canvas")
                    (org-canvas--submissions-count-note
                     counts :errored "failed in Emacs, not at Canvas"))))
   (org-canvas--submissions-refused-note refused)))

(defun org-canvas--submissions-comment-edit-result (counts comments)
  "Return the counts a push reports for sent comments, as a plist.
COUNTS is what `org-canvas--submissions-apply-comment-edits' returned
and COMMENTS the (SENDABLE . REFUSED) it was given.  The plist has
:edited, :deleted, :refused (not sent, see
`org-canvas--submissions-comment-refusal'), :failed (refused by
Canvas, or not answered), :errored (an error in Emacs, issue #443) and
:dry-run."
  (list :edited (or (plist-get counts :edited) 0)
        :deleted (or (plist-get counts :deleted) 0)
        :refused (length (cdr comments))
        :failed (or (plist-get counts :failed) 0)
        :dry-run (or (plist-get counts :dry-run) 0)
        :errored (or (plist-get counts :errored) 0)))

(defun org-canvas--submissions-restore-comment (edit)
  "Put EDIT, a sent comment changed here, back into the fresh entry at point.
Nothing is written when Canvas now holds the edited text (it was
pushed meanwhile).  Otherwise the item takes the grader's text or mark
again, with the baseline it was edited against, so a comment edited on
Canvas since is refused by the next push rather than overwritten.  A
comment gone from Canvas is one warning, which carries the text."
  (let ((item (org-canvas--submissions-find-sent-comment (plist-get edit :id)))
        (fresh (cdr (assoc (plist-get edit :id) (org-canvas--submissions-comment-baseline)))))
    (cond ((null item)
           (org-canvas--log-warning org-canvas--logger
             "[Refresh] Comment %s on %s is no longer on Canvas; the change made here was dropped: %s"
             (plist-get edit :id) (plist-get edit :name)
             (if (plist-get edit :delete) "DELETE" (plist-get edit :text))))
          ((and (not (plist-get edit :delete))
                (equal fresh (org-canvas--submissions-comment-digest (plist-get edit :text)))))
          (t (org-canvas--submissions-reapply-comment item edit fresh)))))

(defun org-canvas--submissions-reapply-comment (item edit fresh)
  "Write EDIT over ITEM, freshly rendered with the digest FRESH, at point.
The baseline goes back to the one EDIT was made against; when Canvas
moved from it, the log says the change will not be sent as it stands."
  (let ((baseline (plist-get edit :baseline)))
    (org-canvas--submissions-rewrite-sent-comment
     item (plist-get edit :text) (plist-get edit :delete))
    (org-canvas--submissions-set-comment-digest (plist-get edit :id) (or baseline fresh))
    (when (and baseline (not (equal baseline fresh)))
      (org-canvas--log-warning org-canvas--logger
        "[Refresh] Comment %s on %s was edited on Canvas since; the change made here is kept and a push will not send it"
        (plist-get edit :id) (plist-get edit :name)))))

(defun org-canvas--submissions-restore-comments (carry)
  "Write CARRY, from `org-canvas--submissions-comment-edits-by-student', back."
  (save-excursion
    (dolist (entry carry)
      (when (org-canvas--submissions-goto-user (car entry))
        (dolist (edit (cdr entry))
          (org-canvas--submissions-restore-comment edit))))))

(defconst org-canvas--submissions-rubric-heading "** Rubric"
  "Heading under which a student's rubric table lives.")

(defun org-canvas--submissions-rubric-criteria (assignment)
  "Return ASSIGNMENT's rubric criteria as a list, or nil without a rubric.
Each criterion's description is decoded to Org text here, once, since
Canvas escapes it (`session&#39;s') and every student's Rubric table
repeats it (issue #265)."
  (mapcar (lambda (c)
            (cons (cons 'description
                        (org-canvas--submissions-inline-text (alist-get 'description c)))
                  c))
          (append (alist-get 'rubric assignment) nil)))

(defun org-canvas--submissions-rubric-for-grading (assignment)
  "Return non-nil when the grade of ASSIGNMENT comes from its rubric.
The Assignment object carries the flag at its top level as
`use_rubric_for_grading' — the field the drift report compares for
RUBRIC_USE_FOR_GRADING — while its `rubric_settings' hold only the
rubric's id, title, points and display flags; reading
`use_for_grading' there answered false for every assignment (issue
#253).  The settings key is still honoured for an object that carries
it, as a rubric association does."
  (let ((top (alist-get 'use_rubric_for_grading assignment))
        (nested (alist-get 'use_for_grading (alist-get 'rubric_settings assignment))))
    (or (eq top t) (and (null top) (eq nested t)))))

(defun org-canvas--submissions-rubric-for-grading-p ()
  "Return non-nil when the grade of this grading file comes from its rubric.
Reads the CANVAS_RUBRIC_USE_FOR_GRADING keyword the pull wrote from the
assignment's rubric settings; a file without it answers nil."
  (equal (org-canvas--submissions-file-property "CANVAS_RUBRIC_USE_FOR_GRADING")
         "true"))

(defun org-canvas--submissions-assessment (submission)
  "Return SUBMISSION's rubric assessment, or nil when it has none.
Canvas maps each assessed criterion's id to its points, rating and
comments; an unassessed submission carries null or nothing."
  (let ((ra (org-canvas--alist-get-non-null 'rubric_assessment submission)))
    (and (consp ra) ra)))

(defun org-canvas--submissions-assessment-entry (assessment criterion-id)
  "Return (SCORE . COMMENT) from ASSESSMENT for CRITERION-ID.
Both are strings spelled as the Rubric section shows them, or nil when
the criterion is unscored or uncommented."
  (let ((entry (alist-get (intern criterion-id) assessment)))
    (when (consp entry)
      (let ((points (org-canvas--alist-get-non-null 'points entry))
            (comment (org-canvas--alist-get-non-null 'comments entry)))
        (cons (and (numberp points) (org-canvas--submissions-format-number points))
              (org-canvas--submissions-comment-text comment))))))

(defun org-canvas--submissions-assessment-triples (assessment)
  "Return ASSESSMENT as (ID SCORE COMMENT) triples, one per criterion in it."
  (mapcar (lambda (pair)
            (let* ((id (symbol-name (car pair)))
                   (entry (org-canvas--submissions-assessment-entry assessment id)))
              (list id (car entry) (cdr entry))))
          assessment))

(defun org-canvas--submissions-rubric-digest (triples)
  "Return a short digest of TRIPLES, or nil when none carries a score or a comment.
TRIPLES are (ID SCORE COMMENT) lists.  Order does not matter, so the
digest of a Rubric table as typed and of the assessment as Canvas
returns it agree whenever their content does; it is the CANVAS_RUBRIC
baseline a push compares the table against."
  (let ((kept (sort (seq-filter (lambda (r) (or (nth 1 r) (nth 2 r))) triples)
                    (lambda (a b) (string< (car a) (car b))))))
    (when kept
      (substring (sha1 (mapconcat (lambda (r)
                                    (format "%s=%s|%s" (nth 0 r) (or (nth 1 r) "") (or (nth 2 r) "")))
                                  kept "\n"))
                 0 12))))

(defun org-canvas--submissions-submission-rubric-digest (submission)
  "Return the CANVAS_RUBRIC digest of SUBMISSION's assessment, or nil."
  (org-canvas--submissions-rubric-digest
   (org-canvas--submissions-assessment-triples
    (org-canvas--submissions-assessment submission))))

(defun org-canvas--submissions-rubric-rows-from-canvas (criteria assessment)
  "Return the Rubric rows for CRITERIA as scored by ASSESSMENT.
Each row is (ID CRITERION MAX SCORE COMMENT), strings or nil.  Without
CRITERIA the rows come from the assessment alone, one per criterion
scored, so an ephemeral buffer rendered without the assignment still
shows what Canvas holds."
  (if criteria
      (mapcar (lambda (c)
                (let* ((id (format "%s" (alist-get 'id c)))
                       (entry (org-canvas--submissions-assessment-entry assessment id))
                       (max (alist-get 'points c)))
                  (list id
                        (org-canvas--submissions-table-cell (alist-get 'description c))
                        (and (numberp max) (org-canvas--submissions-format-number max))
                        (car entry) (cdr entry))))
              criteria)
    (mapcar (lambda (triple) (list (nth 0 triple) nil nil (nth 1 triple) (nth 2 triple)))
            (org-canvas--submissions-assessment-triples assessment))))

(defconst org-canvas--submissions-rubric-item-regexp
  "^-[ \t]+\\([^ \t\n]+\\)[ \t]+::\\(?:[ \t]+\\(.*\\)\\)?$"
  "Match the first line of a Rubric comment item, `- ID :: text'.
Group 1 is the criterion id, group 2 the first line of the comment,
absent when the item is empty.  The comment's other lines follow,
indented, up to the next item.")

(defun org-canvas--submissions-item (label text)
  "Return the description item `- LABEL :: TEXT', TEXT's later lines indented.
The first line of TEXT follows the label and the rest are indented
two spaces under it, a blank line kept as a paragraph break, so the
item wraps like any paragraph and keeps its line breaks.  Nil or an
empty TEXT gives a bare `- LABEL ::'.  No newline ends the result."
  (let ((lines (split-string (or text "") "\n")))
    (concat (format "- %s ::" label)
            (if (string-empty-p (car lines)) "" (concat " " (car lines)))
            (mapconcat (lambda (line) (if (string-empty-p line) "\n" (concat "\n  " line)))
                       (cdr lines) ""))))

(defun org-canvas--submissions-render-rubric (criteria assessment)
  "Insert the Rubric heading, table and items for CRITERIA scored by ASSESSMENT.
One table row per criterion — its Canvas id, description and points
possible, then the Score cell the grader fills — and under the table
one `- ID :: comment' item per criterion, where the comment is
written; an assessment Canvas already holds pre-fills both.  Nothing
is inserted without a rubric."
  (let ((rows (org-canvas--submissions-rubric-rows-from-canvas criteria assessment)))
    (when rows
      (insert "\n" org-canvas--submissions-rubric-heading "\n")
      (insert "| Id | Criterion | Max | Score |\n|---+---+---+---|\n")
      (dolist (row rows)
        (insert (format "| %s | %s | %s | %s |\n"
                        (nth 0 row) (or (nth 1 row) "") (or (nth 2 row) "") (or (nth 3 row) ""))))
      (save-excursion (forward-line -1) (org-table-align))
      (dolist (row rows)
        (insert (org-canvas--submissions-item (nth 0 row) (nth 4 row)) "\n")))))

(defun org-canvas--submissions-table-row-cells (line)
  "Return the cells of table row LINE, trimmed, or nil for a rule or a non-row.
A row a grader is still typing may lack its closing bar."
  (let ((trimmed (string-trim line)))
    (when (and (string-prefix-p "|" trimmed) (not (string-prefix-p "|-" trimmed)))
      (mapcar #'string-trim
              (split-string (substring trimmed 1 (and (string-suffix-p "|" trimmed) -1))
                            "|")))))

(defun org-canvas--submissions-rubric-items (region)
  "Return the comment items of the Rubric section REGION as (ID START TEXT END).
REGION is its (START . END).  An item is a `- ID :: text' line and its
continuation lines, everything up to the next item; START begins the
item, TEXT its comment (after the `::'), and END is the end of its
last non-blank line, so the blank lines before the next item or the
section's end are nobody's.  In order of appearance."
  (save-excursion
    (goto-char (car region))
    ;; The section's END is the next heading's line start or the end of
    ;; the subtree, which sits before the last line's trailing whitespace
    ;; — and an item line ending in spaces must match through them.
    (let ((bound (save-excursion
                   (goto-char (cdr region))
                   (if (bolp) (point) (line-end-position))))
          (items nil))
      (while (re-search-forward org-canvas--submissions-rubric-item-regexp bound t)
        (let* ((id (match-string-no-properties 1))
               (start (match-beginning 0))
               (text (or (match-beginning 2) (match-end 0)))
               (next (save-excursion
                       (if (re-search-forward org-canvas--submissions-rubric-item-regexp bound t)
                           (match-beginning 0)
                         bound)))
               (end (save-excursion
                      (goto-char next)
                      (skip-chars-backward " \t\n" start)
                      (point))))
          (push (list id start text (max text end)) items)
          (goto-char next)))
      (nreverse items))))

(defun org-canvas--submissions-rubric-comments (region)
  "Return the comment items in the Rubric section REGION as (ID . TEXT) pairs.
TEXT is the comment normalized, nil for an item left empty."
  (mapcar (lambda (item)
            (cons (car item)
                  (org-canvas--submissions-comment-text
                   (buffer-substring-no-properties (nth 2 item) (nth 3 item)))))
          (org-canvas--submissions-rubric-items region)))

(defun org-canvas--submissions-rubric-row (cells comments)
  "Return CELLS, a Rubric table row's, as (ID CRITERION MAX SCORE COMMENT).
Empty cells are nil; nil without an id.  The comment is the row's item
among COMMENTS, the section's (ID . TEXT) pairs, or, when the row has
no item, a fifth cell: the shape a grading file had before issue #263
moved comments out of the table, accepted so a file graded then still
pushes until its next pull rewrites it."
  (let* ((row (mapcar (lambda (c) (and (not (string-empty-p c)) c))
                      (cl-subseq (append cells (make-list 5 "")) 0 5)))
         (item (assoc (car row) comments)))
    (when (car row)
      (list (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row)
            (if item (cdr item) (org-canvas--submissions-comment-text (nth 4 row)))))))

(defun org-canvas--submissions-rubric-rows ()
  "Return the Rubric rows of the entry at point, or nil without a table.
Each row is (ID CRITERION MAX SCORE COMMENT): the first four as the
table spells them, cells trimmed and empty cells nil, the header row
and rows without an id dropped; the comment from the row's `- ID ::'
item under the table (`org-canvas--submissions-rubric-row')."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-rubric-heading)))
    (let ((comments (org-canvas--submissions-rubric-comments region))
          (rows nil) (header t))
      (dolist (line (split-string (buffer-substring-no-properties (car region) (cdr region))
                                  "\n"))
        (when-let* ((cells (org-canvas--submissions-table-row-cells line)))
          (if header
              (setq header nil)
            (when-let* ((row (org-canvas--submissions-rubric-row cells comments)))
              (push row rows)))))
      (nreverse rows))))

(defun org-canvas--submissions-rubric-cell-score (text)
  "Return TEXT, a Score cell, spelled as Canvas would return it, or as typed.
A number is normalized (2.0 becomes 2) so a typed score and the same
score pulled back digest alike; anything else is returned unchanged
for the push's check to name."
  (let ((parsed (and text (org-canvas--submissions-parse-score text))))
    (if (and parsed (not (equal parsed "EX")))
        (org-canvas--submissions-format-number (string-to-number parsed))
      text)))

(defun org-canvas--submissions-rubric-check-row (row name)
  "Return ROW as an (ID SCORE COMMENT) triple with the score normalized.
NAME is the student's, for the message.  A Score cell holding a clear
word (`org-canvas--submissions-clear-words') gives (ID nil COMMENT t),
a row whose points the push takes away (issue #449).  Signal a
`user-error' when the Score cell is anything else but a number from 0
to the criterion's Max, so nothing is sent for anyone until the cell
is fixed."
  (let* ((id (nth 0 row))
         (label (or (nth 1 row) id))
         (max (nth 2 row))
         (score (nth 3 row))
         (normalized (org-canvas--submissions-rubric-cell-score score))
         (limit (and max (string-match-p "\\`[0-9.]+\\'" max) (string-to-number max))))
    (when (org-canvas--submissions-clear-score-p score)
      (setq normalized nil))
    (when (and normalized (not (string-match-p "\\`[0-9]+\\.?[0-9]*\\'" normalized)))
      (user-error "Rubric score %S for %s (%s) is not a number" score name label))
    (when (and normalized limit (> (string-to-number normalized) limit))
      (user-error "Rubric score %s for %s (%s) is more than its %s points"
                  normalized name label max))
    (if (org-canvas--submissions-clear-score-p score)
        (list id nil (nth 4 row) t)
      (list id normalized (nth 4 row)))))

(defun org-canvas--submissions-rubric-total (triples)
  "Return the sum of the scores in TRIPLES as a score string, or nil when none."
  (let ((scores (delq nil (mapcar #'cadr triples))))
    (when scores
      (org-canvas--submissions-format-number
       (apply #'+ (mapcar #'string-to-number scores))))))

(defun org-canvas--submissions-rubric-change-at-point (name)
  "Return the Rubric table edits of the entry at point as a plist, or nil.
NAME is the student's, for messages.  Nil when the entry has no table
or its rows digest to the CANVAS_RUBRIC baseline.  A row whose Score
is a clear word takes its points away, and its comment unless one is
written (issue #449), so rows all cleared and uncommented take
Canvas's assessment away.  A table emptied by
hand while Canvas holds an assessment is a `user-error' that names the
clear word: blank rows are left out of the push, as a blank SCORE is,
so Canvas would keep the assessment the file no longer shows.  The
plist carries :triples (every row, checked), :old-rubric and
:new-rubric (the digests), :total (the scored rows' sum), :filled and
:of (how many rows carry a score, out of how many) and :cleared (how
many rows are cleared)."
  (when-let* ((rows (org-canvas--submissions-rubric-rows)))
    (let* ((triples (mapcar (lambda (r) (org-canvas--submissions-rubric-check-row r name))
                            rows))
           (new (org-canvas--submissions-rubric-digest triples))
           (old (org-entry-get (point) "CANVAS_RUBRIC"))
           (cleared (cl-count-if (lambda (triple) (nth 3 triple)) triples)))
      (when (and old (null new) (zerop cleared))
        (user-error "%s: the Rubric rows are empty but Canvas holds an assessment; write none in their Score cells to clear it"
                    name))
      (when (and (or new (> cleared 0)) (not (equal new old)))
        (list :triples triples :old-rubric old :new-rubric new
              :total (org-canvas--submissions-rubric-total triples)
              :filled (cl-count-if #'cadr triples)
              :of (length triples)
              :cleared cleared)))))

(defun org-canvas--submissions-rubric-derived-score (rubric old-score new-score name)
  "Return the score plist due to a changed RUBRIC for NAME, or nil.
Only when the rubric is used for grading: Canvas then derives the grade
from the assessment, so the rows' total becomes the score unless
NEW-SCORE was itself edited away from OLD-SCORE.  Rows that carry no
points have no total and derive nothing: the score stays as typed and
follows the assessment in a request of its own
\(`org-canvas--submissions-grade-after', issue #444).  An edited score
that disagrees with the total is a `user-error', since both cannot be
right, and so is a cleared one (NEW-SCORE nil) beside rows that carry
points, since Canvas would derive a grade from the rows the clear
takes away (issue #417); beside rows without points, cleared ones
included (issue #449), it stands.  One that agrees, or an excusal, is
left as typed."
  (when (and rubric (org-canvas--submissions-rubric-for-grading-p))
    (let ((total (plist-get rubric :total))
          (edited (not (equal new-score old-score))))
      (cond ((not edited)
             (and total (list :new-score total :score-derived t)))
            ((and (null new-score) total)
             (user-error "%s: SCORE clears the grade but the Rubric rows changed; undo one of them"
                         name))
            ((and new-score total (not (equal new-score "EX"))
                  (/= (string-to-number new-score) (string-to-number total)))
             (user-error "%s: SCORE %s disagrees with the rubric total %s; clear one of them"
                         name new-score total))))))

(defun org-canvas--submissions-grade-after (rubric score)
  "Return (:grade-after t) when SCORE must follow RUBRIC in a second request.
RUBRIC is the change of `org-canvas--submissions-rubric-change-at-point'
and SCORE the grade the change leaves the student with.  On a rubric
used for grading, an assessment none of whose rows carries points
leaves Canvas with no grade, whatever grade the same request sends
\(see `org-canvas--submissions-rubric-payload'); the grade then goes
after it, alone.  Nil without such an assessment, or without a grade
to keep."
  (and rubric score
       (null (plist-get rubric :total))
       (org-canvas--submissions-rubric-for-grading-p)
       (list :grade-after t)))

(defun org-canvas--submissions-rubric-set-score (id score region)
  "Write SCORE into the Rubric table row keyed by ID within REGION.
Nil empties the cell; the id, criterion and max cells stay, and a
fifth cell — the comment's place before issue #263 — is emptied, since
the comment now lives in the row's item.  Return non-nil when the row
exists."
  (save-excursion
    (goto-char (car region))
    (catch 'done
      (while (< (point) (cdr region))
        (let ((cells (org-canvas--submissions-table-row-cells
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position)))))
          (when (equal (car cells) id)
            (let ((kept (cl-subseq (append cells (make-list 4 "")) 0 4)))
              (delete-region (line-beginning-position) (line-end-position))
              (insert (format "| %s | %s | %s | %s |%s"
                              id (nth 1 kept) (nth 2 kept) (or score "")
                              (if (> (length cells) 4) " |" "")))
              (org-table-align)
              (throw 'done t))))
        (forward-line 1))
      nil)))

(defun org-canvas--submissions-rubric-table-end (region)
  "Return the end of the last table line in REGION, or its start without one."
  (save-excursion
    (goto-char (car region))
    (let ((end (car region)))
      (while (re-search-forward "^[ \t]*|" (cdr region) t)
        (setq end (line-end-position)))
      end)))

(defun org-canvas--submissions-rubric-set-comment (id comment region)
  "Write COMMENT into the `- ID ::' item of the Rubric section REGION.
An item the section holds is rewritten in place, continuation lines
included, and emptied when COMMENT is nil; one it lacks is added after
the last item, or after the table when there is none, and only for a
comment."
  (let* ((items (org-canvas--submissions-rubric-items region))
         (item (assoc id items))
         (text (org-canvas--submissions-item id comment)))
    (save-excursion
      (cond (item
             (delete-region (nth 1 item) (nth 3 item))
             (goto-char (nth 1 item))
             (insert text))
            (comment
             (goto-char (if items
                            (nth 3 (car (last items)))
                          (org-canvas--submissions-rubric-table-end region)))
             (insert "\n" text))))))

(defun org-canvas--submissions-rubric-set-row (id score comment)
  "Write SCORE and COMMENT for the Rubric row keyed by ID in the entry at point.
The score goes into the table row and the comment into the `- ID ::'
item under the table; either may be nil to empty its place.  Return
non-nil when the row exists; nothing is written when it does not."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-rubric-heading)))
    (when (org-canvas--submissions-rubric-set-score id score region)
      (org-canvas--submissions-rubric-set-comment
       id comment (org-canvas--submissions-section-region
                   org-canvas--submissions-rubric-heading))
      t)))

(defun org-canvas--submissions-rubric-fill-full ()
  "Give every Rubric row of the entry at point its Max as the Score.
Comments stay.  Return how many rows were filled, 0 without a table."
  (let ((n 0))
    (dolist (row (org-canvas--submissions-rubric-rows))
      (when (nth 2 row)
        (org-canvas--submissions-rubric-set-row (nth 0 row) (nth 2 row) (nth 4 row))
        (cl-incf n)))
    n))

(defun org-canvas--submissions-rubric-carryover ()
  "Return the unpushed Rubric rows of the entry at point, or nil.
The value is (:rows TRIPLES :baseline DIGEST): the rows as typed and
the CANVAS_RUBRIC they were typed against, so a re-render can put them
back and tell whether Canvas moved meanwhile.  Rows emptied by hand
against an assessment are carried too, as are cleared ones, so a
refresh does not fill back what the grader took out (issue #449).
Nil when the table matches its baseline, or is empty against no
assessment."
  (when-let* ((rows (org-canvas--submissions-rubric-rows)))
    (let* ((triples (mapcar (lambda (r)
                              (list (nth 0 r)
                                    (org-canvas--submissions-rubric-cell-score (nth 3 r))
                                    (nth 4 r)))
                            rows))
           (digest (org-canvas--submissions-rubric-digest triples))
           (baseline (org-entry-get (point) "CANVAS_RUBRIC")))
      (when (and (or digest baseline) (not (equal digest baseline)))
        (list :rows triples :baseline baseline)))))

(defun org-canvas--submissions-restore-rubric (carry)
  "Put CARRY, a `org-canvas--submissions-rubric-carryover' value, back at point.
The rows are written over the freshly pulled table.  When Canvas's
assessment moved since they were typed — the new CANVAS_RUBRIC is not
the baseline they were typed against — the heading is marked
CONFLICT, since the rows shown are the grader's and not what Canvas
holds.  Nothing is written when Canvas now holds the typed rows, as
when a push stored them but its job was never confirmed: the fresh
CANVAS_RUBRIC is their baseline then, as a typed score Canvas agrees
with is (issue #496)."
  (unless (equal (org-entry-get (point) "CANVAS_RUBRIC")
                 (org-canvas--submissions-rubric-digest (plist-get carry :rows)))
    (dolist (row (plist-get carry :rows))
      (org-canvas--submissions-rubric-set-row (nth 0 row) (nth 1 row) (nth 2 row)))
    (unless (equal (org-entry-get (point) "CANVAS_RUBRIC") (plist-get carry :baseline))
      (org-entry-put (point) "CONFLICT" "rubric: assessed on Canvas since these rows were typed"))))

(provide 'org-canvas-submissions-comment-items)
;;; org-canvas-submissions-comment-items.el ends here
