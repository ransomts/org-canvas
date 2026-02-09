;;; org-canvas-announcements.el --- Pipeline-based Announcement Sync -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the sync pipeline for Canvas Announcements.
;;
;; FILE STRUCTURE
;; ==============
;; In announcements.org:
;;   - Level 1 headings = Announcements
;;   - Heading body = Announcement content (exported to HTML)
;;
;; PROPERTIES
;; ==========
;; POST_AT        - Scheduled post time (Org timestamp)
;;                  If set to a future time, announcement is delayed
;; ALLOW_COMMENTS - Allow student replies ("true"/"false")
;; PUBLISHED      - Visibility (defaults to true)
;;
;; API NOTES
;; =========
;; Announcements use the discussion_topics API with is_announcement=true.
;; They appear in the course's Announcements section, not Discussions.
;;
;; DELAYED POSTING
;; ===============
;; Use POST_AT with a future timestamp to schedule an announcement:
;;   :POST_AT: <2025-02-15 Sat 09:00>
;;
;; The announcement will be hidden until the scheduled time.

;;; Code:
(require 'org-canvas-core)
(require 'ox-html)
(require 'elog)

;;;; Configuration

(defcustom org-canvas-announcements-file (org-canvas--path "announcements.org")
  "Path to the announcements.org file."
  :type 'file
  :group 'org-canvas)

;;;; 1. Stage: Extraction

(defun org-canvas--announcement-parse-entry ()
  "Extract data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (published (org-canvas-org-get-boolean-property pom "PUBLISHED" t))
         (post-at-raw (org-canvas-org-get-property pom "POST_AT"))
         (post-at (org-canvas-org-parse-timestamp post-at-raw))
         (allow-comments (org-canvas-org-get-boolean-property pom "ALLOW_COMMENTS")))

    (when (or (null title) (string-empty-p title))
      (error "Announcement title cannot be empty at point %d" pom))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Announcement: '%s' (ID: %s)" title (or canvas-id "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: published=%s, post-at=%s, allow-comments=%s"
                published (or post-at "immediate") allow-comments)

    ;; Extract Body content (resolves cross-file links to Canvas URLs)
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((content (org-canvas--export-subtree-body-to-html)))
      (elog-info org-canvas--logger "[Stage 1: Parse] Body size: %d chars" (length content))

      (list :title title
            :message content
            :canvas-id canvas-id
            :published published
            :delayed_post_at post-at
            :discussion_type "side_comment"
            :is_announcement t
            :allow_rating nil
            :allow_discussion_comments (if allow-comments t :json-false)
            :pom pom))))

;;;; 2. Stage: Transformation

(defun org-canvas--announcement-build-payload (data)
  "Convert the extracted DATA plist into a Canvas API-compatible alist."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((base `((title . ,title)
                  (message . ,(plist-get data :message))
                  (published . ,(plist-get data :published))
                  (is_announcement . t)
                  (discussion_type . "side_comment"))))

      (when (plist-get data :delayed_post_at)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Adding delayed_post_at: %s" (plist-get data :delayed_post_at))
        (push `(delayed_post_at . ,(plist-get data :delayed_post_at)) base))

      (when (plist-get data :allow_discussion_comments)
        (push `(lock_at . ,(if (eq (plist-get data :allow_discussion_comments) :json-false)
                               (org-canvas-current-iso8601-timestamp)
                             nil)) base))

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      base)))

;;;; 3. Stage: Execution

(defun org-canvas--announcement-push-to-api (data payload)
  "Send PAYLOAD derived from DATA to Canvas API."
  (org-canvas--push-to-api data payload :endpoint "discussion_topics"))

;;;; 4. Stage: Finalization

(defun org-canvas--announcement-finalize (data response)
  "Update local Org file using DATA and metadata from API RESPONSE."
  (org-canvas--finalize-item data response))

;;;; Main Sync Function

;; Generate org-canvas-sync-announcements using the pipeline macro
(org-canvas-define-sync announcements
  :file org-canvas-announcements-file
  :parse #'org-canvas--announcement-parse-entry
  :build #'org-canvas--announcement-build-payload
  :push #'org-canvas--announcement-push-to-api
  :finalize #'org-canvas--announcement-finalize)

(org-canvas-define-push-at-point announcement
  :parse #'org-canvas--announcement-parse-entry
  :build #'org-canvas--announcement-build-payload
  :push #'org-canvas--announcement-push-to-api
  :finalize #'org-canvas--announcement-finalize)

;; Generate org-canvas-delete-all-announcements using the delete macro
(org-canvas-define-delete-all announcements
  :endpoint "discussion_topics"
  :file org-canvas-announcements-file
  :list-params '(("only_announcements" . "true")))

;;;; Pull

;;;###autoload
(defun org-canvas-pull-announcements ()
  "Pull announcements from Canvas into announcements.org."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> PULLING ANNOUNCEMENTS")
  (elog-info org-canvas--logger "========================================")
  (let* ((file (expand-file-name org-canvas-announcements-file))
         (endpoint (org-canvas-api-course-endpoint "discussion_topics"))
         (remote (org-canvas-api-request-all-pages
                  'GET endpoint '(("only_announcements" . "true"))))
         (count 0))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (find-file-noselect file)
      (dolist (item remote)
        (let* ((id (alist-get 'id item))
               (title (alist-get 'title item))
               (message-html (alist-get 'message item))
               (delayed-post (alist-get 'delayed_post_at item))
               (pos (org-canvas--pull-upsert-heading file id title)))
          (goto-char pos)
          (when title (org-edit-headline title))
          (org-canvas-org-save-sync-state pos id)
          (when delayed-post
            (let ((ts (org-canvas--iso8601-to-org-timestamp delayed-post)))
              (when ts (org-canvas-org-set-property pos "DELAYED_POST_AT" ts))))
          ;; Insert body
          (when (and message-html (not (string-empty-p message-html)))
            (let ((body-start (save-excursion
                                (org-end-of-meta-data t) (point)))
                  (body-end (save-excursion
                              (org-end-of-subtree t) (point))))
              (delete-region body-start body-end)
              (goto-char body-start)
              (insert "\n" (org-canvas--html-to-org message-html) "\n")))
          (cl-incf count)))
      (save-buffer))
    (elog-info org-canvas--logger "Announcements pull complete: %d items" count)
    (message "Announcements pull complete: %d items." count)))

(provide 'org-canvas-announcements)
;;; org-canvas-announcements.el ends here
