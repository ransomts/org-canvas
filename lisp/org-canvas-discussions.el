;;; org-canvas-discussions.el --- Pipeline-based Discussion Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas Discussions.
;;
;; FILE STRUCTURE
;; ==============
;; In discussions.org:
;;   - Level 1 headings = Discussion Topics
;;   - Heading body = Initial post content (exported to HTML)
;;
;; PROPERTIES
;; ==========
;; DISCUSSION_TYPE - "side_comment" (flat) or "threaded"
;; PINNED          - Pin to top of discussions list
;; POST_FIRST      - Require students to post before seeing replies
;;
;; GRADED DISCUSSIONS
;; ==================
;; Set these properties to make a discussion graded:
;;   GRADING_TYPE - "points", "percent", "pass_fail"
;;   POINTS       - Points possible
;;   DUE_AT       - Due date (Org timestamp)
;;   GROUP        - Link to assignment-groups.org heading
;;
;; DISCUSSION TYPES
;; ================
;; side_comment - Flat discussion (all replies at same level)
;; threaded     - Threaded replies (can reply to specific posts)
;;
;; API NOTES
;; =========
;; Discussions and announcements share the discussion_topics API.
;; The is_announcement flag distinguishes them.

;;; Code:

(require 'org-canvas-core)
(require 'ox-html)

;;;; Configuration

(defcustom org-canvas-discussions-file (org-canvas--path "discussions.org")
  "Path to the discussions.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-discussions-file "discussions.org")
(org-canvas-register-feature
 :name "Discussions" :endpoint "discussion_topics"
 :file-var 'org-canvas-discussions-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title
 :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t)))
(org-canvas-register-properties "discussions"
  :label "Discussions"
  :file-var 'org-canvas-discussions-file
  :query "LEVEL=1"
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean)
    (:org-prop "DISCUSSION_TYPE" :data-key :discussion_type :type enum
     :values ,org-canvas--valid-discussion-types)
    (:org-prop "GRADING_TYPE" :data-key :grading_type :type enum
     :values ,org-canvas--valid-grading-types)
    (:org-prop "POINTS" :data-key :points_possible :type number)
    (:org-prop "POST_FIRST" :data-key :require_initial_post :type boolean)
    (:org-prop "PINNED" :data-key :pinned :type boolean)
    (:org-prop "AVAILABLE_FROM" :data-key :delayed_post_at :type timestamp)
    (:org-prop "DUE_AT" :data-key :due_at :type timestamp)
    (:org-prop "LOCK_AT" :data-key :lock_at :type timestamp)
    (:org-prop "ALLOW_RATING" :data-key :allow_rating :type boolean)
    (:org-prop "ONLY_GRADERS_CAN_RATE" :data-key :only_graders_can_rate :type boolean)
    (:org-prop "SORT_BY_RATING" :data-key :sort_by_rating :type boolean)
    (:org-prop "GROUP_CATEGORY" :data-key :group_category_id :type number)
    (:org-prop "GROUP" :data-key :assignment_group_id :type link
     :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID")
    (:org-prop "RUBRIC_LINK" :data-key :rubric_id :type link
     :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID"))
  :date-order '(("AVAILABLE_FROM" "DUE_AT" "LOCK_AT")))

;;;; 1. Stage: Extraction

(defun org-canvas--discussion-read-props (pom)
  "Read raw property strings from the discussion heading at POM.
Link properties (GROUP, GROUP_CATEGORY, RUBRIC_LINK) are resolved
here since they require file I/O."
  (let* ((group-link (org-entry-get pom "GROUP"))
         (rubric-link (org-entry-get pom "RUBRIC_LINK"))
         (group-category-raw (org-canvas--resolve-link-or-raw
                              pom "GROUP_CATEGORY" "CANVAS_ID"
                              org-canvas-discussions-file)))
    (list :title-raw (org-get-heading t t t t)
          :canvas-id (org-entry-get pom "CANVAS_ID")
          :published-raw (org-entry-get pom "PUBLISHED")
          :discussion-type-raw (org-entry-get pom "DISCUSSION_TYPE")
          :post-first-raw (org-entry-get pom "POST_FIRST")
          :pinned-raw (org-entry-get pom "PINNED")
          :available-from-raw (org-entry-get pom "AVAILABLE_FROM")
          :allow-rating-raw (org-entry-get pom "ALLOW_RATING")
          :only-graders-can-rate-raw (org-entry-get pom "ONLY_GRADERS_CAN_RATE")
          :sort-by-rating-raw (org-entry-get pom "SORT_BY_RATING")
          :specific-sections (org-entry-get pom "SPECIFIC_SECTIONS")
          ;; Grading props
          :grading-type-raw (org-entry-get pom "GRADING_TYPE")
          :points-raw (org-entry-get pom "POINTS")
          :due-at-raw (org-entry-get pom "DUE_AT")
          :lock-at-raw (org-entry-get pom "LOCK_AT")
          ;; Resolved links (I/O)
          :assignment-group-id-raw (when group-link
                                     (org-canvas--resolve-link-property
                                      group-link "CANVAS_ID"
                                      org-canvas-discussions-file))
          :group-category-id-raw group-category-raw
          :rubric-id (when rubric-link
                       (org-canvas--resolve-link-property
                        rubric-link "CANVAS_ID"
                        org-canvas-discussions-file)))))

(defun org-canvas--discussion-transform-props (raw)
  "Transform raw property strings RAW into typed discussion data.
Pure function — no buffer access."
  (let ((points (plist-get raw :points-raw))
        (agid (plist-get raw :assignment-group-id-raw))
        (gcid (plist-get raw :group-category-id-raw)))
    (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
          :canvas-id (plist-get raw :canvas-id)
          :published (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
          :discussion_type (org-canvas--validate-property
                            (plist-get raw :discussion-type-raw)
                            org-canvas--valid-discussion-types
                            "DISCUSSION_TYPE" "side_comment")
          :grading_type (plist-get raw :grading-type-raw)
          :points_possible (when points
                             (org-canvas--safe-string-to-number points "POINTS"))
          :require_initial_post (org-canvas--interpret-boolean
                                 (plist-get raw :post-first-raw))
          :pinned (org-canvas--interpret-boolean (plist-get raw :pinned-raw))
          :delayed_post_at (org-canvas-org-parse-timestamp
                            (plist-get raw :available-from-raw))
          :due_at (org-canvas-org-parse-timestamp (plist-get raw :due-at-raw))
          :lock_at (org-canvas-org-parse-timestamp (plist-get raw :lock-at-raw))
          :assignment_group_id (when agid (string-to-number agid))
          :allow_rating (org-canvas--interpret-boolean (plist-get raw :allow-rating-raw))
          :only_graders_can_rate (org-canvas--interpret-boolean
                                  (plist-get raw :only-graders-can-rate-raw))
          :sort_by_rating (org-canvas--interpret-boolean
                           (plist-get raw :sort-by-rating-raw))
          :group_category_id (when gcid
                               (org-canvas--safe-string-to-number gcid "GROUP_CATEGORY"))
          :specific_sections (plist-get raw :specific-sections)
          :rubric-id (plist-get raw :rubric-id))))

(defun org-canvas--discussion-parse-entry ()
  "Extract discussion data from the Org heading at point."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--discussion-read-props pom))
         (data (org-canvas--discussion-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Discussion")

    (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Processing Discussion: '%s' (ID: %s)"
              (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Properties: type=%s, graded=%s, points=%s, post-first=%s, pinned=%s"
      (plist-get data :discussion_type)
      (if (plist-get data :grading_type) "yes" "no")
      (or (plist-get data :points_possible) "N/A")
      (plist-get data :require_initial_post)
      (plist-get data :pinned))

    ;; Extract Body content (resolves cross-file links to Canvas URLs)
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((content (org-canvas--export-subtree-body-to-html)))
      (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Body size: %d chars" (length content))

      (plist-put data :message content)
      (plist-put data :pom pom)
      data)))

;;;; 2. Stage: Transformation

(defun org-canvas--discussion-build-graded-assignment (data)
  "Build the graded assignment sub-payload from DATA.
Returns an alist for the `assignment' key."
  (let ((assignment-data `((points_possible . ,(plist-get data :points_possible))
                           (grading_type . ,(or (plist-get data :grading_type) "points")))))
    ;; delayed_post_at maps to unlock_at in the assignment sub-payload
    (when (plist-get data :delayed_post_at)
      (push `(unlock_at . ,(plist-get data :delayed_post_at)) assignment-data))
    (org-canvas--push-non-nil-fields data
      '((:due_at . due_at)
        (:lock_at . lock_at)
        (:assignment_group_id . assignment_group_id))
      assignment-data)))

(defun org-canvas--discussion-build-payload (data)
  "Convert DATA to Canvas payload."
  (let ((title (plist-get data :title)))
    (org-canvas--log-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((base `((title . ,title)
                  (message . ,(plist-get data :message))
                  (published . ,(org-canvas--to-json-boolean (plist-get data :published)))
                  (discussion_type . ,(plist-get data :discussion_type))
                  (require_initial_post . ,(org-canvas--to-json-boolean (plist-get data :require_initial_post))))))

      ;; Simple non-nil field pushes (booleans are t when truthy)
      (setq base (org-canvas--push-non-nil-fields data
                   '((:pinned . pinned)
                     (:delayed_post_at . delayed_post_at)
                     (:allow_rating . allow_rating)
                     (:only_graders_can_rate . only_graders_can_rate)
                     (:sort_by_rating . sort_by_rating)
                     (:group_category_id . group_category_id))
                   base))
      ;; lock_at only on non-graded discussions (graded uses assignment payload)
      (when (and (plist-get data :lock_at) (not (plist-get data :points_possible)))
        (push `(lock_at . ,(plist-get data :lock_at)) base))
      (when (plist-get data :specific_sections)
        (let ((resolved (org-canvas--resolve-section-names-to-ids
                         (plist-get data :specific_sections))))
          (when resolved
            (push `(specific_sections . ,resolved) base))))

      (when (plist-get data :points_possible)
        (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Adding graded assignment: %s pts"
          (plist-get data :points_possible))
        (push `(assignment . ,(org-canvas--discussion-build-graded-assignment data)) base)
        (org-canvas--validate-date-ordering data))

      (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      base)))

;;;; Post-Finalize: Rubric Association

(defun org-canvas--discussion-post-finalize (data response)
  "Associate rubric with discussion after finalize.
DATA is the parsed discussion plist, RESPONSE is the Canvas API response."
  (let ((rubric-id (plist-get data :rubric-id))
        (discussion-id (alist-get 'id response)))
    (when rubric-id
      (org-canvas--associate-rubric discussion-id rubric-id "Discussion"))))

;;;; Main Sync Function

;; Generate org-canvas-sync-discussions using the pipeline macro
(org-canvas-define-sync discussions
  :file org-canvas-discussions-file
  :parse #'org-canvas--discussion-parse-entry
  :build #'org-canvas--discussion-build-payload
  :endpoint "discussion_topics"
  :post-fn #'org-canvas--discussion-post-finalize
  :pull-item-fn #'org-canvas--discussion-pull-item)

;; Generate org-canvas-delete-all-discussions using the delete macro
;; Skip announcements (is_announcement = t) since those have their own delete
(org-canvas-define-delete-all discussions
  :endpoint "discussion_topics"
  :file org-canvas-discussions-file
  :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t)))

(org-canvas-define-delete-at-point discussion
  :endpoint "discussion_topics/%s")

;;;; Pull

(org-canvas-define-pull-item discussion
  :body-field message
  :properties
  ((discussion_type       "DISCUSSION_TYPE"       :type string)
   (delayed_post_at       "DELAYED_POST_AT"       :type timestamp)
   (allow_rating          "ALLOW_RATING"          :type boolean)
   (only_graders_can_rate "ONLY_GRADERS_CAN_RATE" :type boolean)
   (sort_by_rating        "SORT_BY_RATING"        :type boolean)))

(org-canvas-define-pull discussions
  :file org-canvas-discussions-file
  :endpoint "discussion_topics"
  :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t))
  :pull-item-fn #'org-canvas--discussion-pull-item)

(provide 'org-canvas-discussions)
;;; org-canvas-discussions.el ends here
