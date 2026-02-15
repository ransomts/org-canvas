;;; org-canvas-discussions.el --- Pipeline-based Discussion Sync -*- lexical-binding: t; -*-

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
(require 'elog)

;;;; Configuration

(defcustom org-canvas-discussions-file (org-canvas--path "discussions.org")
  "Path to the discussions.org file."
  :type 'file
  :group 'org-canvas)

;;;; 1. Stage: Extraction

(defun org-canvas--discussion-parse-grading-props (pom)
  "Extract grading-related properties from the heading at POM.
Returns a plist with :grading-type, :points, :due-at, :lock-at,
:assignment-group-id keys."
  (let* ((grading-type (org-canvas-org-get-property pom "GRADING_TYPE"))
         (points (org-canvas-org-get-property pom "POINTS"))
         (due-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "DUE_AT")))
         (lock-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "LOCK_AT")))
         (group-link (org-canvas-org-get-property pom "GROUP"))
         (assignment-group-id (when group-link
                                (org-canvas--resolve-link-property group-link "CANVAS_ID"
                                                                   org-canvas-discussions-file))))
    (list :grading-type grading-type :points points :due-at due-at
          :lock-at lock-at :assignment-group-id assignment-group-id)))

(defun org-canvas--discussion-parse-entry ()
  "Extract discussion data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (published (org-canvas-org-get-boolean-property pom "PUBLISHED" t))
         (discussion-type (org-canvas--validate-property
                          (org-canvas-org-get-property pom "DISCUSSION_TYPE")
                          '("side_comment" "threaded")
                          "DISCUSSION_TYPE" "side_comment"))
         (post-first (org-canvas-org-get-boolean-property pom "POST_FIRST"))
         (pinned (org-canvas-org-get-boolean-property pom "PINNED"))
         (available-from (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "AVAILABLE_FROM")))
         (allow-rating (org-canvas-org-get-boolean-property pom "ALLOW_RATING"))
         (only-graders-can-rate (org-canvas-org-get-boolean-property pom "ONLY_GRADERS_CAN_RATE"))
         (sort-by-rating (org-canvas-org-get-boolean-property pom "SORT_BY_RATING"))
         (group-category-raw (org-canvas-org-get-property pom "GROUP_CATEGORY"))
         (group-category-id (if (and group-category-raw
                                     (string-prefix-p "[[" group-category-raw))
                                (org-canvas--resolve-link-property
                                 group-category-raw "CANVAS_ID"
                                 org-canvas-discussions-file)
                              group-category-raw))
         (specific-sections (org-canvas-org-get-property pom "SPECIFIC_SECTIONS"))
         (grading (org-canvas--discussion-parse-grading-props pom)))

    (when (or (null title) (string-empty-p title))
      (error "Discussion title cannot be empty at point %d" pom))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Discussion: '%s' (ID: %s)" title (or canvas-id "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: type=%s, graded=%s, points=%s, post-first=%s, pinned=%s"
      discussion-type (if (plist-get grading :grading-type) "yes" "no")
      (or (plist-get grading :points) "N/A") post-first pinned)

    ;; Extract Body content (resolves cross-file links to Canvas URLs)
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((content (org-canvas--export-subtree-body-to-html))
          (points (plist-get grading :points))
          (agid (plist-get grading :assignment-group-id)))
      (elog-info org-canvas--logger "[Stage 1: Parse] Body size: %d chars" (length content))

      (list :title title
            :message content
            :canvas-id canvas-id
            :published published
            :discussion_type discussion-type
            :grading_type (plist-get grading :grading-type)
            :points_possible (when points (org-canvas--safe-string-to-number points "POINTS"))
            :require_initial_post post-first
            :pinned pinned
            :delayed_post_at available-from
            :due_at (plist-get grading :due-at)
            :lock_at (plist-get grading :lock-at)
            :assignment_group_id (when agid (string-to-number agid))
            :allow_rating allow-rating
            :only_graders_can_rate only-graders-can-rate
            :sort_by_rating sort-by-rating
            :group_category_id (when group-category-id (org-canvas--safe-string-to-number group-category-id "GROUP_CATEGORY"))
            :specific_sections specific-sections
            :pom pom))))

;;;; 2. Stage: Transformation

(defun org-canvas--discussion-build-graded-assignment (data)
  "Build the graded assignment sub-payload from DATA.
Returns an alist for the `assignment' key."
  (let ((assignment-data `((points_possible . ,(plist-get data :points_possible))
                           (grading_type . ,(or (plist-get data :grading_type) "points")))))
    (when (plist-get data :due_at)
      (push `(due_at . ,(plist-get data :due_at)) assignment-data))
    (when (plist-get data :lock_at)
      (push `(lock_at . ,(plist-get data :lock_at)) assignment-data))
    (when (plist-get data :delayed_post_at)
      (push `(unlock_at . ,(plist-get data :delayed_post_at)) assignment-data))
    (when (plist-get data :assignment_group_id)
      (push `(assignment_group_id . ,(plist-get data :assignment_group_id)) assignment-data))
    assignment-data))

(defun org-canvas--discussion-build-payload (data)
  "Convert DATA to Canvas payload."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((base `((title . ,title)
                  (message . ,(plist-get data :message))
                  (published . ,(org-canvas--to-json-boolean (plist-get data :published)))
                  (discussion_type . ,(plist-get data :discussion_type))
                  (require_initial_post . ,(org-canvas--to-json-boolean (plist-get data :require_initial_post))))))

      (when (plist-get data :pinned)
        (push '(pinned . t) base))
      (when (plist-get data :delayed_post_at)
        (push `(delayed_post_at . ,(plist-get data :delayed_post_at)) base))
      (when (and (plist-get data :lock_at) (not (plist-get data :points_possible)))
        (push `(lock_at . ,(plist-get data :lock_at)) base))

      (when (plist-get data :allow_rating)
        (push '(allow_rating . t) base))
      (when (plist-get data :only_graders_can_rate)
        (push '(only_graders_can_rate . t) base))
      (when (plist-get data :sort_by_rating)
        (push '(sort_by_rating . t) base))
      (when (plist-get data :group_category_id)
        (push `(group_category_id . ,(plist-get data :group_category_id)) base))
      (when (plist-get data :specific_sections)
        (let ((resolved (org-canvas--resolve-section-names-to-ids
                         (plist-get data :specific_sections))))
          (when resolved
            (push `(specific_sections . ,resolved) base))))

      (when (plist-get data :points_possible)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Adding graded assignment: %s pts"
          (plist-get data :points_possible))
        (push `(assignment . ,(org-canvas--discussion-build-graded-assignment data)) base)
        (org-canvas--validate-date-ordering data))

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      base)))

;;;; 3. Stage: Execution

(defun org-canvas--discussion-push-to-api (data payload)
  "Send PAYLOAD based on DATA to Canvas API.
Handles 404 on PUT by retrying as POST (stale CANVAS_ID recovery)."
  (org-canvas--push-to-api data payload :endpoint "discussion_topics"))

;;;; 4. Stage: Finalization

(defun org-canvas--discussion-finalize (data response)
  "Update local Org file based on the RESPONSE and DATA."
  (org-canvas--finalize-item data response))

;;;; Main Sync Function

;; Generate org-canvas-sync-discussions using the pipeline macro
(org-canvas-define-sync discussions
  :file org-canvas-discussions-file
  :parse #'org-canvas--discussion-parse-entry
  :build #'org-canvas--discussion-build-payload
  :push #'org-canvas--discussion-push-to-api
  :finalize #'org-canvas--discussion-finalize
  :pull-item-fn #'org-canvas--discussion-pull-item)

(org-canvas-define-push-at-point discussion
  :parse #'org-canvas--discussion-parse-entry
  :build #'org-canvas--discussion-build-payload
  :push #'org-canvas--discussion-push-to-api
  :finalize #'org-canvas--discussion-finalize)

;; Generate org-canvas-delete-all-discussions using the delete macro
;; Skip announcements (is_announcement = t) since those have their own delete
(org-canvas-define-delete-all discussions
  :endpoint "discussion_topics"
  :file org-canvas-discussions-file
  :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t)))

;;;; Pull

(defun org-canvas--discussion-pull-item (item pos)
  "Set per-item properties for a pulled discussion.
ITEM is the API response alist, POS is the heading position."
  (let ((discussion-type (alist-get 'discussion_type item))
        (delayed-post (alist-get 'delayed_post_at item)))
    (when discussion-type
      (org-canvas-org-set-property pos "DISCUSSION_TYPE" discussion-type))
    (when delayed-post
      (let ((ts (org-canvas--iso8601-to-org-timestamp delayed-post)))
        (when ts (org-canvas-org-set-property pos "DELAYED_POST_AT" ts)))))
  (org-canvas--pull-set-boolean-property pos "ALLOW_RATING" (alist-get 'allow_rating item))
  (org-canvas--pull-set-boolean-property pos "ONLY_GRADERS_CAN_RATE" (alist-get 'only_graders_can_rate item))
  (org-canvas--pull-set-boolean-property pos "SORT_BY_RATING" (alist-get 'sort_by_rating item))
  (org-canvas--pull-insert-body (alist-get 'message item)))

(org-canvas-define-pull discussions
  :file org-canvas-discussions-file
  :endpoint "discussion_topics"
  :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t))
  :item-fn #'org-canvas--discussion-pull-item)

(provide 'org-canvas-discussions)
;;; org-canvas-discussions.el ends here
