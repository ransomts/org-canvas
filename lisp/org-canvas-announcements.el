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

(defun org-canvas--announcement-read-props (pom)
  "Read raw property strings from the announcement heading at POM."
  (list :title-raw (org-get-heading t t t t)
        :canvas-id (org-entry-get pom "CANVAS_ID")
        :published-raw (org-entry-get pom "PUBLISHED")
        :post-at-raw (org-entry-get pom "POST_AT")
        :allow-comments-raw (org-entry-get pom "ALLOW_COMMENTS")
        :specific-sections (org-entry-get pom "SPECIFIC_SECTIONS")))

(defun org-canvas--announcement-transform-props (raw)
  "Transform raw property strings RAW into typed announcement data.
Pure function — no buffer access."
  (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
        :canvas-id (plist-get raw :canvas-id)
        :published (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
        :delayed_post_at (org-canvas-org-parse-timestamp (plist-get raw :post-at-raw))
        :allow_discussion_comments (org-canvas--interpret-boolean
                                    (plist-get raw :allow-comments-raw))
        :specific_sections (plist-get raw :specific-sections)))

(defun org-canvas--announcement-parse-entry ()
  "Extract data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--announcement-read-props pom))
         (data (org-canvas--announcement-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Announcement")

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Announcement: '%s' (ID: %s)"
              (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: published=%s, post-at=%s, allow-comments=%s"
                (plist-get data :published)
                (or (plist-get data :delayed_post_at) "immediate")
                (plist-get data :allow_discussion_comments))

    ;; Extract Body content (resolves cross-file links to Canvas URLs)
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((content (org-canvas--export-subtree-body-to-html)))
      (elog-info org-canvas--logger "[Stage 1: Parse] Body size: %d chars" (length content))

      (plist-put data :message content)
      (plist-put data :pom pom)
      data)))

;;;; 2. Stage: Transformation

(defun org-canvas--announcement-build-payload (data)
  "Convert the extracted DATA plist into a Canvas API-compatible alist."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((base `((title . ,title)
                  (message . ,(plist-get data :message))
                  (published . ,(org-canvas--to-json-boolean (plist-get data :published)))
                  (is_announcement . t)
                  (discussion_type . "side_comment"))))

      (when (plist-get data :delayed_post_at)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Adding delayed_post_at: %s" (plist-get data :delayed_post_at))
        (push `(delayed_post_at . ,(plist-get data :delayed_post_at)) base))

      (unless (plist-get data :allow_discussion_comments)
        (push `(lock_at . ,(org-canvas-current-iso8601-timestamp)) base))

      (when (plist-get data :specific_sections)
        (let ((resolved (org-canvas--resolve-section-names-to-ids
                         (plist-get data :specific_sections))))
          (when resolved
            (push `(specific_sections . ,resolved) base))))

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      base)))

;;;; Main Sync Function

;; Generate org-canvas-sync-announcements using the pipeline macro
(org-canvas-define-sync announcements
  :file org-canvas-announcements-file
  :parse #'org-canvas--announcement-parse-entry
  :build #'org-canvas--announcement-build-payload
  :endpoint "discussion_topics"
  :pull-item-fn #'org-canvas--announcement-pull-item)

(org-canvas-define-push-at-point announcement
  :parse #'org-canvas--announcement-parse-entry
  :build #'org-canvas--announcement-build-payload
  :endpoint "discussion_topics"
  :pull-item-fn #'org-canvas--announcement-pull-item)

;; Generate org-canvas-delete-all-announcements using the delete macro
(org-canvas-define-delete-all announcements
  :endpoint "discussion_topics"
  :file org-canvas-announcements-file
  :list-params '(("only_announcements" . "true")))

;;;; Pull

(defun org-canvas--announcement-pull-item (item pos)
  "Set per-item properties for a pulled announcement.
ITEM is the API response alist, POS is the heading position."
  (let ((delayed-post (alist-get 'delayed_post_at item)))
    (when delayed-post
      (let ((ts (org-canvas--iso8601-to-org-timestamp delayed-post)))
        (when ts (org-canvas-org-set-property pos "DELAYED_POST_AT" ts)))))
  (org-canvas--pull-insert-body (alist-get 'message item)))

(org-canvas-define-pull announcements
  :file org-canvas-announcements-file
  :endpoint "discussion_topics"
  :params '(("only_announcements" . "true"))
  :item-fn #'org-canvas--announcement-pull-item)

(provide 'org-canvas-announcements)
;;; org-canvas-announcements.el ends here
