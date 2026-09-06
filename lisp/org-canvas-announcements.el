;;; org-canvas-announcements.el --- Pipeline-based Announcement Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; DELAYED_POST_AT - Scheduled post time (Org timestamp)
;;                   If set to a future time, announcement is delayed
;; POSTED_AT       - Read-only: when announcement was actually posted
;; AUTHOR          - Read-only: display name of the announcement author
;; ALLOW_COMMENTS  - Allow student replies ("true"/"false")
;; PUBLISHED       - Visibility (defaults to true)
;;
;; API NOTES
;; =========
;; Announcements use the discussion_topics API with is_announcement=true.
;; They appear in the course's Announcements section, not Discussions.
;;
;; DELAYED POSTING
;; ===============
;; Use DELAYED_POST_AT with a future timestamp to schedule an announcement:
;;   :DELAYED_POST_AT: <2025-02-15 Sat 09:00>
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
;; The two properties Canvas does not hold under a flat key of their own.
;; A `:remote-fn' is what the drift report and the registry-driven pull
;; both read (issue #135); without one the pull could not write AUTHOR
;; at all and the report called every announcement's ALLOW_COMMENTS drifted.

(defun org-canvas--announcement-remote-author (item)
  "Return the display name of the user who posted ITEM, or nil.
Canvas nests it under `user'."
  (let ((user (alist-get 'user item)))
    (and (listp user) (alist-get 'display_name user))))

(defun org-canvas--announcement-remote-allow-comments (item)
  "Return non-nil when ITEM still accepts replies.
Disabling comments is pushed as a `lock_at' in the past (see the
payload builder), which Canvas reports back as `locked'."
  (not (eq (alist-get 'locked item) t)))

(org-canvas-register-properties "announcements"
  :label "Announcements"
  :file-var 'org-canvas-announcements-file
  :query "LEVEL=1"
  :body-api-key "message"
  :properties
  '((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :api-key "published" :boolean-json t
     :doc "Whether the announcement is visible to students (default: true)")
    (:org-prop "POSTED_AT" :data-key :posted_at :type timestamp
     :doc "When the announcement was posted (read-only, set by Canvas)")
    (:org-prop "DELAYED_POST_AT" :data-key :delayed_post_at :type timestamp
     :api-key "delayed_post_at"
     :doc "Schedule the announcement to publish at this time")
    (:org-prop "AUTHOR" :data-key :author :type string
     :remote-fn org-canvas--announcement-remote-author
     :doc "Announcement author display name (read-only)")
    (:org-prop "ALLOW_COMMENTS" :data-key :allow_discussion_comments :type boolean
     :remote-fn org-canvas--announcement-remote-allow-comments
     :doc "Allow student replies")))

;;;; 1. Stage: Extraction

(org-canvas-define-parse announcement
  :body :message
  :properties
  (("PUBLISHED"        :published                 :type boolean :default t)
   ("POSTED_AT"        :posted_at                 :type timestamp)
   ("DELAYED_POST_AT"  :delayed_post_at           :type timestamp)
   ("AUTHOR"           :author                    :type string)
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
  :registry-key "announcements"
  :body-field message)

(org-canvas-define-pull announcements
  :file org-canvas-announcements-file
  :endpoint "discussion_topics"
  :params '(("only_announcements" . "true"))
  :pull-item-fn #'org-canvas--announcement-pull-item)

(provide 'org-canvas-announcements)
;;; org-canvas-announcements.el ends here
