;;; org-canvas-discussion-replies.el --- Pull discussion replies from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A pull-only archive of what students and staff wrote under a
;; course's discussion topics.  Topics themselves are content and live
;; in discussions.org (see `org-canvas-discussions'); this module never
;; writes to Canvas and never requires another feature module: it
;; reads the discussions API itself.
;;
;; FILE STRUCTURE
;; ==============
;; In discussion-replies.org:
;;   - Level 1 headings = discussion topics that have at least one entry
;;     (title links the topic's heading in discussions.org when that file
;;     holds the topic's CANVAS_ID; otherwise the plain title)
;;   - Level 2 headings = top-level entries, in posting order
;;   - Level 3 headings = replies to an entry, in posting order
;;
;; Entry and reply headings read `Reply by NAME (YYYY-MM-DD HH:MM)';
;; the body is the message converted to Org.
;;
;; PROPERTIES (set by pull)
;; ========================
;; CANVAS_ID          - topic id, on the level-1 heading
;; CANVAS_ENTRY_ID    - entry id, on level-2 and level-3 headings
;; AUTHOR             - the author's display name
;; AUTHOR_ID          - the author's Canvas user id
;; POSTED_AT          - when the entry was posted (Org timestamp)
;; PARENT_ENTRY_ID    - the entry a reply answers, on level-3 headings
;;
;; A re-pull matches entries by CANVAS_ENTRY_ID and updates them in
;; place; an entry Canvas no longer returns is left alone, since this
;; module never deletes anything local.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/discussion_topics                          - list topics
;;   GET /courses/:id/discussion_topics/:tid/entries              - top-level entries
;;   GET /courses/:id/discussion_topics/:tid/entries/:eid/replies - an entry's replies

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-discussion-replies-file (org-canvas--path "discussion-replies.org")
  "Path to the discussion-replies.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-discussion-replies-file "discussion-replies.org")
(org-canvas-register-properties "discussion-replies"
  :label "Discussion Replies"
  :file-var 'org-canvas-discussion-replies-file
  :query "LEVEL>1"
  :properties
  '((:org-prop "CANVAS_ENTRY_ID" :data-key :canvas-entry-id :type string
     :doc "Canvas id of the entry or reply (set by pull)")
    (:org-prop "AUTHOR" :data-key :author :type string :pull-only t
     :doc "Display name of the entry's author (set by pull)")
    (:org-prop "AUTHOR_ID" :data-key :author-id :type number
     :doc "Canvas user id of the entry's author (set by pull)")
    (:org-prop "POSTED_AT" :data-key :posted-at :type timestamp
     :doc "When the entry was posted (set by pull)")
    (:org-prop "PARENT_ENTRY_ID" :data-key :parent-entry-id :type number
     :doc "Entry this reply answers, on replies only (set by pull)")))

;;;; Reading Canvas

(defun org-canvas--reply-fetch-topics ()
  "Return the course's discussion topics, announcements excluded, as a list."
  (cl-remove-if
   (lambda (topic) (eq (alist-get 'is_announcement topic) t))
   (append (org-canvas-api-request-all-pages
            'GET (org-canvas-api-course-endpoint "discussion_topics"))
           nil)))

(defun org-canvas--reply-fetch-entries (topic-id)
  "Return the top-level entries of topic TOPIC-ID as a list."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint
                 "discussion_topics/%s/entries" topic-id))
          nil))

(defun org-canvas--reply-fetch-replies (topic-id entry)
  "Return the replies to ENTRY of topic TOPIC-ID as a list, or nil.
Canvas inlines a few recent replies on the entry and flags the rest
with `has_more_replies'; one request fetches them all whenever either
says there are any."
  (when (or (eq (alist-get 'has_more_replies entry) t)
            (> (length (alist-get 'recent_replies entry)) 0))
    (append (org-canvas-api-request-all-pages
             'GET (org-canvas-api-course-endpoint
                   "discussion_topics/%s/entries/%s/replies"
                   topic-id (alist-get 'id entry)))
            nil)))

(defun org-canvas--reply-live-p (entry)
  "Return non-nil for a live ENTRY, nil for a deletion stub Canvas left behind."
  (not (eq (alist-get 'deleted entry) t)))

;;;; Writing Org

(defun org-canvas--reply-topic-title (topic)
  "Return the heading text for TOPIC: a link into discussions.org, else its title.
The link is used when `org-canvas-discussions-file' exists and holds a
level-1 heading stamped with the topic's id, as the modules pull links
its items; the discussions module is never loaded for it."
  (let* ((title (or (alist-get 'title topic) "Untitled"))
         (id (format "%s" (alist-get 'id topic)))
         (file (and (boundp 'org-canvas-discussions-file)
                    org-canvas-discussions-file
                    (expand-file-name org-canvas-discussions-file)))
         (heading nil))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (when (and (null heading)
                        (equal (org-entry-get (point) "CANVAS_ID") id))
               (setq heading (org-get-heading t t t t))))
           "LEVEL=1" 'file))))
    (if heading
        (org-link-make-string
         (format "file:%s::*%s" (file-name-nondirectory file) heading)
         title)
      title)))

(defun org-canvas--reply-heading-text (entry)
  "Return the heading text for ENTRY: `Reply by NAME (YYYY-MM-DD HH:MM)'.
The date and time are those of the Org timestamp the entry's POSTED_AT
gets, so the heading and the property agree on the course's zone."
  (let* ((name (or (alist-get 'user_name entry) "Unknown"))
         (ts (org-canvas--iso8601-to-org-timestamp (alist-get 'created_at entry)))
         (when-text (if (and ts (string-match "\\([0-9-]+\\) [A-Za-z]+ \\([0-9:]+\\)" ts))
                        (format "%s %s" (match-string 1 ts) (match-string 2 ts))
                      "undated")))
    (format "Reply by %s (%s)" name when-text)))

(defun org-canvas--reply-find-entry (entry-id)
  "Return the position of the heading carrying CANVAS_ENTRY_ID ENTRY-ID, or nil.
Searches the current buffer at every level."
  (let ((found nil)
        (target (format "%s" entry-id)))
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when (and (null found)
                    (equal (org-entry-get (point) "CANVAS_ENTRY_ID") target))
           (setq found (point))))
       "CANVAS_ENTRY_ID={.}" 'file))
    found))

(defun org-canvas--reply-set-properties (pos entry parent-id)
  "Write ENTRY's properties on the heading at POS.
PARENT-ID is the id of the entry ENTRY answers, or nil for a
top-level entry."
  (org-canvas-org-set-property pos "CANVAS_ENTRY_ID" (format "%s" (alist-get 'id entry)))
  (when (alist-get 'user_name entry)
    (org-canvas-org-set-property pos "AUTHOR" (alist-get 'user_name entry)))
  (when (alist-get 'user_id entry)
    (org-canvas-org-set-property pos "AUTHOR_ID" (format "%s" (alist-get 'user_id entry))))
  (org-canvas--pull-set-timestamp-property pos "POSTED_AT" (alist-get 'created_at entry))
  (when parent-id
    (org-canvas-org-set-property pos "PARENT_ENTRY_ID" (format "%s" parent-id))))

(defun org-canvas--reply-replace-body (message-html)
  "Replace the body of the heading at point with MESSAGE-HTML converted to Org.
The body ends at the next heading of any level, so an entry's replies,
which sit under it as headings, are not part of it.  A nil or empty
message leaves an empty body.  The conversion goes through
`org-canvas--html-to-org', the chokepoint that keeps a body from
introducing a headline (Hard Rule 22)."
  (let* ((meta-end (save-excursion (org-end-of-meta-data t) (point)))
         (next (save-excursion (outline-next-heading) (point)))
         (body-start (save-excursion
                       (goto-char (min meta-end next))
                       (skip-chars-backward " \t\n")
                       (point)))
         (text (and message-html (not (string-empty-p message-html))
                    (string-trim (org-canvas--html-to-org message-html)))))
    (delete-region body-start next)
    (goto-char body-start)
    (insert "\n")
    (when (and text (not (string-empty-p text)))
      (insert text "\n"))))

(defun org-canvas--reply-insert-under (parent-pos level text)
  "Insert a new LEVEL heading TEXT at the end of the subtree at PARENT-POS.
Returns the new heading's position."
  (goto-char parent-pos)
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n"))
  (let ((start (point)))
    (insert (make-string level ?*) " " text "\n")
    (goto-char start)
    (org-back-to-heading t)
    (point)))

(defun org-canvas--reply-upsert-entry (parent-pos level entry parent-id)
  "Write ENTRY as a LEVEL heading under PARENT-POS, or update it in place.
PARENT-ID is the id of the entry it answers, or nil.  Returns a cons
of the entry's heading position and the symbol `created' or `updated'."
  (let ((pos (org-canvas--reply-find-entry (alist-get 'id entry))))
    (if pos
        (progn
          (goto-char pos)
          (org-canvas--reply-set-properties pos entry parent-id)
          (org-canvas--reply-replace-body (alist-get 'message entry))
          (cons pos 'updated))
      (let ((new (org-canvas--reply-insert-under
                  parent-pos level (org-canvas--reply-heading-text entry))))
        (org-canvas--reply-set-properties new entry parent-id)
        (goto-char new)
        (org-canvas--reply-replace-body (alist-get 'message entry))
        (cons new 'created)))))

(defun org-canvas--reply-write-topic (file topic entries fetch-replies)
  "Write TOPIC and its ENTRIES into FILE; return (ENTRIES . REPLIES) written.
FETCH-REPLIES is called with an entry and returns its replies.  Point
ends up in FILE's buffer."
  (let* ((topic-id (alist-get 'id topic))
         (topic-pos (org-canvas--pull-upsert-heading
                     file topic-id (org-canvas--reply-topic-title topic)))
         (entry-count 0)
         (reply-count 0))
    (with-current-buffer (org-canvas--find-file-noselect (expand-file-name file))
      (org-canvas-org-set-property topic-pos "CANVAS_ID" (format "%s" topic-id))
      (dolist (entry entries)
        (when (org-canvas--reply-live-p entry)
          (let ((entry-pos (car (org-canvas--reply-upsert-entry topic-pos 2 entry nil))))
            (cl-incf entry-count)
            (dolist (reply (funcall fetch-replies entry))
              (when (org-canvas--reply-live-p reply)
                (org-canvas--reply-upsert-entry
                 entry-pos 3 reply (or (alist-get 'parent_id reply) (alist-get 'id entry)))
                (cl-incf reply-count)))))))
    (cons entry-count reply-count)))

;;;; Command

(defun org-canvas--reply-pull-topic (file topic)
  "Pull TOPIC's entries into FILE; return (ENTRIES . REPLIES), or nil on failure.
A request that fails for this one topic — a locked or restricted
discussion answers 403 — is logged, recorded in the pull summary and
skipped, so one such topic costs its own replies and not the pull
\(the rule issue #171 set for every per-item failure)."
  (let ((title (or (alist-get 'title topic) "Untitled"))
        (topic-id (alist-get 'id topic)))
    (condition-case err
        (let ((entries (org-canvas--reply-fetch-entries topic-id)))
          (if (null entries)
              (progn
                (org-canvas--log-debug org-canvas--logger
                  "[Pull] '%s' has no entries; nothing written" title)
                (cons 0 0))
            (let ((counts (org-canvas--reply-write-topic
                           file topic entries
                           (lambda (entry) (org-canvas--reply-fetch-replies topic-id entry)))))
              (org-canvas--log-info org-canvas--logger
                "[Pull] '%s': %d entries, %d replies" title (car counts) (cdr counts))
              counts)))
      (org-canvas-api-error
       (org-canvas--log-warning org-canvas--logger
         "[Pull] Skipping replies of '%s' (id %s): %s" title topic-id
         (error-message-string err))
       (org-canvas--pull-summary-record
        :file (file-name-nondirectory file)
        :item title
        :error (error-message-string err)
        :log-line (org-canvas--pull-summary-current-log-line))
       nil))))

;;;###autoload
(defun org-canvas-pull-discussion-replies ()
  "Pull every discussion topic's entries and replies into discussion-replies.org.
Topics come from discussions.org's Canvas objects; each topic with at
least one entry becomes a level-1 heading, its entries level-2 and
their replies level-3 headings, in posting order.  Entries are matched
by CANVAS_ENTRY_ID on a re-pull and updated in place; nothing local is
deleted.  Read-only: this never writes to Canvas."
  (interactive)
  (org-canvas--start-operation "PULLING DISCUSSION REPLIES")
  (let* ((file (expand-file-name org-canvas-discussion-replies-file))
         (was-fresh (org-canvas--pull-was-fresh-p file))
         (topics (org-canvas--reply-fetch-topics))
         (topic-count 0) (entry-count 0) (reply-count 0) (skipped 0))
    (org-canvas--pull-confirm-unsaved file "discussion replies")
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (org-canvas--log-info org-canvas--logger "[Pull] %d topics on Canvas" (length topics))
    (dolist (topic topics)
      (let ((counts (org-canvas--reply-pull-topic file topic)))
        (cond
         ((null counts) (cl-incf skipped))
         ((> (car counts) 0)
          (cl-incf topic-count)
          (cl-incf entry-count (car counts))
          (cl-incf reply-count (cdr counts))))))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-canvas--pull-write-file-header)
      (org-canvas--save-buffer))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> DISCUSSION REPLIES PULL COMPLETE")
    (org-canvas--log-info org-canvas--logger
      "Topics with entries: %d | Entries: %d | Replies: %d | Topics skipped: %d"
      topic-count entry-count reply-count skipped)
    (org-canvas--log-info org-canvas--logger "========================================")
    (message "Discussion replies pull: %d topics, %d entries, %d replies%s."
             topic-count entry-count reply-count
             (if (> skipped 0) (format ", %d topics skipped" skipped) ""))))

(provide 'org-canvas-discussion-replies)
;;; org-canvas-discussion-replies.el ends here
