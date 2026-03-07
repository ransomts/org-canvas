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

(org-canvas-define-parse announcement
  :body :message
  :properties
  (("PUBLISHED"        :published                 :type boolean :default t)
   ("POST_AT"          :delayed_post_at           :type timestamp)
   ("ALLOW_COMMENTS"   :allow_discussion_comments :type boolean)
   ("SPECIFIC_SECTIONS" :specific_sections        :type string)))

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

;; Generate org-canvas-delete-all-announcements using the delete macro
(org-canvas-define-delete-all announcements
  :endpoint "discussion_topics"
  :file org-canvas-announcements-file
  :list-params '(("only_announcements" . "true")))

(org-canvas-define-delete-at-point announcement
  :endpoint "discussion_topics/%s")

;;;; Pull

(org-canvas-define-pull-item announcement
  :body-field message
  :properties
  ((delayed_post_at "DELAYED_POST_AT" :type timestamp)))

(org-canvas-define-pull announcements
  :file org-canvas-announcements-file
  :endpoint "discussion_topics"
  :params '(("only_announcements" . "true"))
  :item-fn #'org-canvas--announcement-pull-item)

(provide 'org-canvas-announcements)
;;; org-canvas-announcements.el ends here
