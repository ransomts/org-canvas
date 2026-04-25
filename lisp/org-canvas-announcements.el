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

;;;; Configuration

(defcustom org-canvas-announcements-file (org-canvas--path "announcements.org")
  "Path to the announcements.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-announcements-file "announcements.org")
(org-canvas-register-feature
 :name "Announcements" :endpoint "discussion_topics"
 :file-var 'org-canvas-announcements-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title
 :list-params '(("only_announcements" . "true")))
(org-canvas-register-properties "announcements"
  :label "Announcements"
  :file-var 'org-canvas-announcements-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :api-key "published" :boolean-json t)
    (:org-prop "POST_AT" :data-key :delayed_post_at :type timestamp
     :api-key "delayed_post_at")
    (:org-prop "ALLOW_COMMENTS" :data-key :allow_discussion_comments :type boolean)))

;;;; 1. Stage: Extraction

(org-canvas-define-parse announcement
  :body :message
  :properties
  (("PUBLISHED"        :published                 :type boolean :default t)
   ("POST_AT"          :delayed_post_at           :type timestamp)
   ("ALLOW_COMMENTS"   :allow_discussion_comments :type boolean)
   ("SPECIFIC_SECTIONS" :specific_sections        :type string)))

;;;; 2. Stage: Transformation

(defun org-canvas--announcement-post-build (data payload)
  "Apply announcement-specific payload transformations.
DATA is the parsed plist, PAYLOAD is the alist so far."
  ;; Lock comments when ALLOW_COMMENTS is false/nil
  (unless (plist-get data :allow_discussion_comments)
    (push `(lock_at . ,(org-canvas-current-iso8601-timestamp)) payload))
  ;; Resolve section names to Canvas section IDs
  (when (plist-get data :specific_sections)
    (let ((resolved (org-canvas--resolve-section-names-to-ids
                     (plist-get data :specific_sections))))
      (when resolved
        (push `(specific_sections . ,resolved) payload))))
  payload)

(org-canvas-define-payload announcement
  :registry-key "announcements"
  :format alist
  :title-key :title
  :title-api-key title
  :body-key :message
  :body-api-key message
  :static-fields ((is_announcement . t)
                  (discussion_type . "side_comment"))
  :post-build-fn #'org-canvas--announcement-post-build)

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
  :pull-item-fn #'org-canvas--announcement-pull-item)

(provide 'org-canvas-announcements)
;;; org-canvas-announcements.el ends here
