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
;; CHECKPOINTS
;; ===========
;; A checkpointed discussion (issue #216) grades the first post and the
;; replies to peers separately, each with its own points and due date:
;;   CHECKPOINT_TOPIC_POINTS      - Points for the reply-to-topic checkpoint
;;   CHECKPOINT_TOPIC_DUE_AT      - Its due date (Org timestamp)
;;   CHECKPOINT_REPLY_POINTS      - Points for the reply-to-entry checkpoint
;;   CHECKPOINT_REPLY_DUE_AT      - Its due date
;;   CHECKPOINT_REPLIES_REQUIRED  - Replies to peers required (default 1)
;; Any of them makes the discussion checkpointed; POINTS may be left
;; out (Canvas grades the sum) and DUE_AT does not apply.  Checkpoints
;; are not in the REST API: the topic is created or updated over REST
;; as before, then `updateDiscussionTopic' sets them over GraphQL from
;; finalize, the way an assignment's post policy is set (issue #202).
;; The pull and the drift report read every topic's checkpoints in one
;; course-wide GraphQL query per command, cached until the next command
;; starts (`org-canvas--operation-start-hook').  The feature is an
;; account-level flag in Canvas; where it is off, Canvas refuses the
;; mutation and the entry fails with that message.
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
 :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t))
 :skip-reason "announcement, pulled by the announcements module")
(org-canvas-register-properties "discussions"
  :duplicate-titles t
  :label "Discussions"
  :file-var 'org-canvas-discussions-file
  :query "LEVEL=1"
  :body-api-key "message"
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :doc "Whether the discussion is visible to students (default: true)")
    (:org-prop "DISCUSSION_TYPE" :data-key :discussion_type :type enum
     :values ,org-canvas--valid-discussion-types
     :doc "side_comment, threaded")
    (:org-prop "GRADING_TYPE" :data-key :grading_type :type enum
     :values ,org-canvas--valid-grading-types
     :doc "points, percent (for graded)")
    (:org-prop "POINTS" :data-key :points_possible :type number
     :doc "Points possible (for graded)")
    (:org-prop "POST_FIRST" :data-key :require_initial_post :type boolean
     :doc "Require post before viewing others")
    (:org-prop "PINNED" :data-key :pinned :type boolean
     :doc "Pin to top of discussions list")
    (:org-prop "AVAILABLE_FROM" :data-key :delayed_post_at :type timestamp
     :doc "Hide discussion until this date")
    (:org-prop "DUE_AT" :data-key :due_at :type timestamp
     :doc "Due date (for graded)")
    (:org-prop "LOCK_AT" :data-key :lock_at :type timestamp
     :doc "Prevent replies after this date")
    (:org-prop "ALLOW_RATING" :data-key :allow_rating :type boolean
     :doc "Allow post rating")
    (:org-prop "ONLY_GRADERS_CAN_RATE" :data-key :only_graders_can_rate :type boolean
     :doc "Restrict rating to graders")
    (:org-prop "SORT_BY_RATING" :data-key :sort_by_rating :type boolean
     :doc "Sort posts by rating")
    (:org-prop "GROUP_CATEGORY" :data-key :group_category_id :type number
     :doc "Group set ID or link to group-categories.org")
    (:org-prop "GROUP" :data-key :assignment_group_id :type link
     :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID"
     :doc "Assignment group (for graded)")
    (:org-prop "RUBRIC_LINK" :data-key :rubric_id :type link
     :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID"
     :doc "Link to a rubric heading in rubrics.org")
    (:org-prop "CHECKPOINT_TOPIC_POINTS" :data-key :checkpoint_topic_points :type number
     :remote-fn org-canvas--discussion-remote-checkpoint-topic-points
     :remote-known-p org-canvas--discussion-checkpoints-known-p
     :compare-p org-canvas--discussion-checkpoints-comparable-p
     :doc "Points for the reply-to-topic checkpoint; any CHECKPOINT_ property makes the discussion checkpointed")
    (:org-prop "CHECKPOINT_TOPIC_DUE_AT" :data-key :checkpoint_topic_due_at :type timestamp
     :remote-fn org-canvas--discussion-remote-checkpoint-topic-due-at
     :remote-known-p org-canvas--discussion-checkpoints-known-p
     :compare-p org-canvas--discussion-checkpoints-comparable-p
     :doc "Due date of the reply-to-topic checkpoint")
    (:org-prop "CHECKPOINT_REPLY_POINTS" :data-key :checkpoint_reply_points :type number
     :remote-fn org-canvas--discussion-remote-checkpoint-reply-points
     :remote-known-p org-canvas--discussion-checkpoints-known-p
     :compare-p org-canvas--discussion-checkpoints-comparable-p
     :doc "Points for the reply-to-entry checkpoint")
    (:org-prop "CHECKPOINT_REPLY_DUE_AT" :data-key :checkpoint_reply_due_at :type timestamp
     :remote-fn org-canvas--discussion-remote-checkpoint-reply-due-at
     :remote-known-p org-canvas--discussion-checkpoints-known-p
     :compare-p org-canvas--discussion-checkpoints-comparable-p
     :doc "Due date of the reply-to-entry checkpoint")
    (:org-prop "CHECKPOINT_REPLIES_REQUIRED" :data-key :checkpoint_replies_required :type number
     :default 1
     :remote-fn org-canvas--discussion-remote-checkpoint-replies-required
     :remote-known-p org-canvas--discussion-checkpoints-known-p
     :compare-p org-canvas--discussion-checkpoints-comparable-p
     :doc "Replies to peers the reply-to-entry checkpoint requires (default 1)"))
  :structural-fn #'org-canvas--validate-discussion-checkpoints
  :date-order '(("AVAILABLE_FROM" "DUE_AT" "LOCK_AT")
                ("AVAILABLE_FROM" "CHECKPOINT_TOPIC_DUE_AT" "CHECKPOINT_REPLY_DUE_AT")
                ("AVAILABLE_FROM" "CHECKPOINT_REPLY_DUE_AT" "LOCK_AT")))

;;;; Checkpoints: the course-wide GraphQL read

(defvar org-canvas--discussion-checkpoints-cache nil
  "Cons of (COURSE-ID . MAP) from the last course-wide checkpoint read.
MAP is a hash of topic id (a string) to a checkpoint plist
\=(:topic-points :topic-due-at :reply-points :reply-due-at
:replies-required), holding only the topics that have checkpoints; or
the symbol `refused' when Canvas would not answer, so the properties
are left alone rather than deleted.  Forgotten when a command starts
\=(`org-canvas--operation-start-hook') and after a checkpoint push, so
one command reads it once and the next reads afresh.")

(defconst org-canvas--discussion-checkpoints-query
  "query ($courseId: ID!, $cursor: String) { course(id: $courseId) { discussionsConnection(first: 100, after: $cursor) { pageInfo { hasNextPage endCursor } nodes { _id replyToEntryRequiredCount checkpoints { tag pointsPossible dueAt } } } } }"
  "The GraphQL query listing every discussion's checkpoints, one page at a time.")

(defun org-canvas--discussion-checkpoints-forget ()
  "Drop the cached checkpoint map, so the next read asks Canvas."
  (setq org-canvas--discussion-checkpoints-cache nil))

(add-hook 'org-canvas--operation-start-hook #'org-canvas--discussion-checkpoints-forget)

(defun org-canvas--discussion-checkpoints-from-node (node)
  "Return the checkpoints of the GraphQL discussion NODE as a plist, or nil.
Nil for a topic with no checkpoints, which is every topic on an
instance where the feature is off."
  (let ((topic nil) (reply nil))
    (dolist (checkpoint (append (alist-get 'checkpoints node) nil))
      (pcase (alist-get 'tag checkpoint)
        ("reply_to_topic" (setq topic checkpoint))
        ("reply_to_entry" (setq reply checkpoint))))
    (when (or topic reply)
      (list :topic-points (alist-get 'pointsPossible topic)
            :topic-due-at (org-canvas--alist-get-non-null 'dueAt topic)
            :reply-points (alist-get 'pointsPossible reply)
            :reply-due-at (org-canvas--alist-get-non-null 'dueAt reply)
            :replies-required (alist-get 'replyToEntryRequiredCount node)))))

(defun org-canvas--discussion-checkpoints-fetch ()
  "Read every discussion's checkpoints from Canvas into a hash by topic id.
One course-wide GraphQL query, followed page by page.  Returns the
symbol `refused' when the request fails, after one warning: the
discussions still pull, with their checkpoint properties untouched
\=(the #171 rule)."
  (condition-case err
      (let ((map (make-hash-table :test 'equal)) (cursor nil) (more t))
        (while more
          (let* ((data (org-canvas--graphql-query
                        org-canvas--discussion-checkpoints-query
                        (append (list (cons 'courseId (format "%s" org-canvas-course-id)))
                                (when cursor (list (cons 'cursor cursor))))))
                 (connection (alist-get 'discussionsConnection (alist-get 'course data)))
                 (info (alist-get 'pageInfo connection)))
            (dolist (node (append (alist-get 'nodes connection) nil))
              (when-let* ((checkpoints (org-canvas--discussion-checkpoints-from-node node)))
                (puthash (format "%s" (alist-get '_id node)) checkpoints map)))
            (setq cursor (org-canvas--alist-get-non-null 'endCursor info)
                  more (and (eq (alist-get 'hasNextPage info) t) cursor))))
        map)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Checkpoints] Could not read the discussion checkpoints (%s); the CHECKPOINT_ properties are left as they are"
       (error-message-string err))
     'refused)))

(defun org-canvas--discussion-checkpoints-map ()
  "Return the course's checkpoint map, reading it once per command."
  (unless (equal (car org-canvas--discussion-checkpoints-cache) org-canvas-course-id)
    (setq org-canvas--discussion-checkpoints-cache
          (cons org-canvas-course-id (org-canvas--discussion-checkpoints-fetch))))
  (cdr org-canvas--discussion-checkpoints-cache))

(defun org-canvas--discussion-checkpoints-known-p (item)
  "Return non-nil when the checkpoints of the Canvas topic ITEM can be answered.
Only a graded topic (one carrying an `assignment_id') can have
checkpoints, so an ungraded one is answered without asking Canvas,
and nothing is asked in a run that never meets a graded topic.  Nil
when the course-wide read was refused."
  (and (org-canvas--alist-get-non-null 'assignment_id item)
       (not (eq (org-canvas--discussion-checkpoints-map) 'refused))))

(defun org-canvas--discussion-checkpoints-comparable-p (_pom item)
  "Return non-nil when ITEM's checkpoints are known, for the drift report.
The `:compare-p' of the CHECKPOINT_ properties: a refused read is no
one's opinion to compare."
  (org-canvas--discussion-checkpoints-known-p item))

(defun org-canvas--discussion-remote-checkpoints (item)
  "Return the checkpoint plist of the Canvas topic ITEM, or nil."
  (when (org-canvas--discussion-checkpoints-known-p item)
    (gethash (format "%s" (alist-get 'id item))
             (org-canvas--discussion-checkpoints-map))))

(defun org-canvas--discussion-remote-checkpoint-topic-points (item)
  "Return the reply-to-topic points of the Canvas topic ITEM, or nil."
  (plist-get (org-canvas--discussion-remote-checkpoints item) :topic-points))

(defun org-canvas--discussion-remote-checkpoint-topic-due-at (item)
  "Return the reply-to-topic due date of the Canvas topic ITEM, or nil."
  (plist-get (org-canvas--discussion-remote-checkpoints item) :topic-due-at))

(defun org-canvas--discussion-remote-checkpoint-reply-points (item)
  "Return the reply-to-entry points of the Canvas topic ITEM, or nil."
  (plist-get (org-canvas--discussion-remote-checkpoints item) :reply-points))

(defun org-canvas--discussion-remote-checkpoint-reply-due-at (item)
  "Return the reply-to-entry due date of the Canvas topic ITEM, or nil."
  (plist-get (org-canvas--discussion-remote-checkpoints item) :reply-due-at))

(defun org-canvas--discussion-remote-checkpoint-replies-required (item)
  "Return the replies the Canvas topic ITEM requires, or nil."
  (plist-get (org-canvas--discussion-remote-checkpoints item) :replies-required))

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
          ;; Checkpoints (issue #216)
          :checkpoint-topic-points-raw (org-entry-get pom "CHECKPOINT_TOPIC_POINTS")
          :checkpoint-topic-due-at-raw (org-entry-get pom "CHECKPOINT_TOPIC_DUE_AT")
          :checkpoint-reply-points-raw (org-entry-get pom "CHECKPOINT_REPLY_POINTS")
          :checkpoint-reply-due-at-raw (org-entry-get pom "CHECKPOINT_REPLY_DUE_AT")
          :checkpoint-replies-required-raw (org-entry-get pom "CHECKPOINT_REPLIES_REQUIRED")
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

(defun org-canvas--discussion-transform-checkpoints (raw)
  "Return RAW's CHECKPOINT_ properties as a plist, or nil when it names none.
Any one of the five makes the discussion checkpointed; the replies
required default to 1, as they do on Canvas.  Points are numbers and
the due dates ISO8601 strings, read through the same converter the
REST payload's dates go through."
  (let ((topic-points (plist-get raw :checkpoint-topic-points-raw))
        (topic-due-at (plist-get raw :checkpoint-topic-due-at-raw))
        (reply-points (plist-get raw :checkpoint-reply-points-raw))
        (reply-due-at (plist-get raw :checkpoint-reply-due-at-raw))
        (required (plist-get raw :checkpoint-replies-required-raw)))
    (when (or topic-points topic-due-at reply-points reply-due-at required)
      (list :topic-points (and topic-points
                               (org-canvas--safe-string-to-number
                                topic-points "CHECKPOINT_TOPIC_POINTS"))
            :topic-due-at (org-canvas-org-parse-timestamp topic-due-at)
            :reply-points (and reply-points
                               (org-canvas--safe-string-to-number
                                reply-points "CHECKPOINT_REPLY_POINTS"))
            :reply-due-at (org-canvas-org-parse-timestamp reply-due-at)
            :replies-required (if required
                                  (org-canvas--safe-string-to-number
                                   required "CHECKPOINT_REPLIES_REQUIRED")
                                1)))))

(defun org-canvas--discussion-checkpoints-total (checkpoints)
  "Return the points of CHECKPOINTS added up, what Canvas grades the topic out of."
  (+ (or (plist-get checkpoints :topic-points) 0)
     (or (plist-get checkpoints :reply-points) 0)))

(defun org-canvas--discussion-transform-props (raw)
  "Transform raw property strings RAW into typed discussion data.
Pure function — no buffer access.  A checkpointed discussion without
a POINTS of its own is graded out of the checkpoints' sum, so the REST
payload still carries the assignment Canvas hangs the checkpoints on."
  (let ((points (plist-get raw :points-raw))
        (agid (plist-get raw :assignment-group-id-raw))
        (gcid (plist-get raw :group-category-id-raw))
        (checkpoints (org-canvas--discussion-transform-checkpoints raw)))
    (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
          :canvas-id (plist-get raw :canvas-id)
          :published (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
          :discussion_type (org-canvas--validate-property
                            (plist-get raw :discussion-type-raw)
                            org-canvas--valid-discussion-types
                            "DISCUSSION_TYPE" "side_comment")
          :grading_type (plist-get raw :grading-type-raw)
          :points_possible (cond (points (org-canvas--safe-string-to-number points "POINTS"))
                                 (checkpoints (org-canvas--discussion-checkpoints-total checkpoints)))
          :checkpoints checkpoints
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

;;;; Post-Finalize: Rubric Association and Checkpoints

(defconst org-canvas--discussion-checkpoints-mutation
  "mutation ($topicId: ID!, $checkpoints: [DiscussionCheckpoints!]) { updateDiscussionTopic(input: {discussionTopicId: $topicId, setCheckpoints: true, checkpoints: $checkpoints}) { discussionTopic { _id } errors { message } } }"
  "The GraphQL mutation that sets a discussion's checkpoints.")

(defun org-canvas--discussion-checkpoint-input (label points due-at &optional replies-required)
  "Return one DiscussionCheckpoints input for the checkpoint LABEL.
POINTS is its points possible, DUE-AT its ISO8601 due date or nil, and
REPLIES-REQUIRED, for the reply-to-entry checkpoint, how many replies
it takes.  The date applies to everyone; per-section checkpoint dates
are not managed."
  (append `((checkpointLabel . ,label)
            (pointsPossible . ,(or points 0))
            (dates . ,(vector (append '((type . "everyone"))
                                      (when due-at `((dueAt . ,due-at)))))))
          (when replies-required `((repliesRequired . ,replies-required)))))

(defun org-canvas--discussion-checkpoints-inputs (checkpoints)
  "Return the two DiscussionCheckpoints inputs for the plist CHECKPOINTS."
  (vector (org-canvas--discussion-checkpoint-input
           "reply_to_topic" (plist-get checkpoints :topic-points)
           (plist-get checkpoints :topic-due-at))
          (org-canvas--discussion-checkpoint-input
           "reply_to_entry" (plist-get checkpoints :reply-points)
           (plist-get checkpoints :reply-due-at)
           (or (plist-get checkpoints :replies-required) 1))))

(defun org-canvas--discussion-push-checkpoints (data topic-id)
  "Set TOPIC-ID's checkpoints from DATA's :checkpoints, when given.
The `updateDiscussionTopic' GraphQL mutation (issue #216), after the
REST write has created or updated the topic.  A mutation that answers
with validation errors — the feature is off for the account, say —
signals `org-canvas-api-error', so the entry fails with Canvas's
words and is pushed again next time.  Returns non-nil when a write
went out, so the caller can note it on the run."
  (let ((checkpoints (plist-get data :checkpoints)))
    (when (and checkpoints topic-id)
      (let ((reply (org-canvas--graphql-mutate
                    (format "set the checkpoints of '%s'" (plist-get data :title))
                    org-canvas--discussion-checkpoints-mutation
                    (list (cons 'topicId (format "%s" topic-id))
                          (cons 'checkpoints
                                (org-canvas--discussion-checkpoints-inputs checkpoints))))))
        (org-canvas--discussion-checkpoints-forget)
        (let ((errors (and (listp reply)
                           (alist-get 'errors (alist-get 'updateDiscussionTopic reply)))))
          (when (and errors (not (eq errors :null)) (> (length errors) 0))
            (org-canvas--signal 'org-canvas-api-error
              "Checkpoints of '%s': %s" (plist-get data :title)
              (org-canvas--graphql-errors-message errors))))
        t))))

(defun org-canvas--discussion-post-finalize (data response &optional ctx)
  "Associate rubric and set checkpoints after finalize.
DATA is the parsed discussion plist, RESPONSE is the Canvas API response,
CTX the run context a remote write is declared on."
  (let ((rubric-id (plist-get data :rubric-id))
        (discussion-id (alist-get 'id response)))
    (when (and rubric-id
               (org-canvas--associate-rubric discussion-id rubric-id "Discussion"))
      ;; The association bumps the topic's updated_at past the stamp
      ;; finalize just wrote; have it re-read (issue #124).
      (org-canvas--finalize-note-remote-write ctx))
    ;; Checkpoints are GraphQL, not part of the PUT; they touch the
    ;; topic too, so they are declared the same way (issue #216).
    (when (org-canvas--discussion-push-checkpoints data discussion-id)
      (org-canvas--finalize-note-remote-write ctx))))

(defun org-canvas--discussion-hash-extra (data)
  "Return DATA's checkpoint material for change detection, or \"\".
The checkpoints travel by GraphQL, outside the REST payload, so
without this a changed checkpoint date never dirtied the entry and
was never sent.  Empty when the heading has none, so other headings
keep their hash."
  (let ((checkpoints (plist-get data :checkpoints)))
    (if checkpoints
        (format "checkpoints:%s:%s:%s:%s:%s"
                (plist-get checkpoints :topic-points)
                (plist-get checkpoints :topic-due-at)
                (plist-get checkpoints :reply-points)
                (plist-get checkpoints :reply-due-at)
                (plist-get checkpoints :replies-required))
      "")))

(defun org-canvas--discussion-sync-prepare (_ctx)
  "Forget the checkpoint map before a sync run, so it is read afresh.
The `:prepare' of the discussions sync; its result is unused."
  (org-canvas--discussion-checkpoints-forget)
  nil)

;;;; Main Sync Function

;; Generate org-canvas-sync-discussions using the pipeline macro
(org-canvas-define-sync discussions
  :file org-canvas-discussions-file
  :parse #'org-canvas--discussion-parse-entry
  :build #'org-canvas--discussion-build-payload
  :endpoint "discussion_topics"
  :find-fn (lambda (title) (org-canvas--search-item "discussion_topics" title))
  :post-fn #'org-canvas--discussion-post-finalize
  :hash-extra #'org-canvas--discussion-hash-extra
  :prepare #'org-canvas--discussion-sync-prepare
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
  :registry-key "discussions"
  :body-field message)

(org-canvas-define-pull discussions
  :file org-canvas-discussions-file
  :endpoint "discussion_topics"
  :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t))
  :skip-reason "announcement, pulled by the announcements module"
  :pull-item-fn #'org-canvas--discussion-pull-item)

(provide 'org-canvas-discussions)
;;; org-canvas-discussions.el ends here
