;;; org-canvas-assignments.el --- Pipeline-based Assignment Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas Assignments.
;;
;; FILE STRUCTURE
;; ==============
;; In assignments.org:
;;   - Level 1 headings = Assignments
;;   - Heading body = Description (exported to HTML)
;;
;; KEY PROPERTIES
;; ==============
;;   POINTS           - Points possible
;;   DUE_AT           - Due date (Org timestamp)
;;   SUBMISSION       - Submission type(s): online_upload, online_text_entry, etc.
;;   GROUP            - Link to assignment-groups.org heading
;;   RUBRIC_LINK      - Link to rubrics.org heading
;;   RUBRIC_USE_FOR_GRADING - true: the rubric total becomes the grade
;;   RUBRIC_HIDE_SCORE_TOTAL - true: students do not see the rubric total
;;
;; LINK RESOLUTION
;; ===============
;; The GROUP and RUBRIC_LINK properties can contain Org links like:
;;   [[file:rubrics.org::*Standard Rubric][Standard Rubric]]
;;
;; During sync, these links are followed to read the CANVAS_ID property
;; from the linked heading.  This requires syncing rubrics and assignment
;; groups before assignments (see org-canvas-sync for proper ordering).
;;
;; RUBRIC ASSOCIATION
;; ==================
;; After creating/updating an assignment, if RUBRIC_LINK is specified,
;; the rubric is associated via a separate API call to rubric_associations.

;;; Code:

(require 'org-canvas-core)
(require 'ox-html)

;;;; Forward Declarations

(defvar org-canvas-assignment-groups-file)
(defvar org-canvas-sections-file)
(declare-function org-canvas--override-fetch "org-canvas-sections" (assignment-id))
(declare-function org-canvas--override-emit-table "org-canvas-sections"
                  (overrides &optional parent-due parent-unlock parent-lock))

;;;; Configuration

(defcustom org-canvas-assignments-file (org-canvas--path "assignments.org")
  "Path to the assignments.org file."
  :type 'file
  :group 'org-canvas)

(defcustom org-canvas-assignment-sort 'position
  "Sort order for assignments within their assignment group during pull.

- `position' (default): preserve Canvas's UI ordering.  This matches
  the order shown in the Canvas web UI but does not necessarily
  reflect chronology.
- `due-at': sort by due date ascending within each assignment group.
  Assignments without a due date sort after those that have one.

The chosen mode is consulted at pull time, so changing this defcustom
takes effect on the next `org-canvas-pull-assignments' invocation."
  :type '(choice (const :tag "Canvas position (UI order)" position)
                 (const :tag "Due date ascending" due-at))
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-assignments-file "assignments.org")
(org-canvas-register-feature
 :name "Assignments" :endpoint "assignments"
 :file-var 'org-canvas-assignments-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'name
 ;; A classic quiz drags a shadow assignment behind it; the quiz is the
 ;; thing the Org files manage (issue #98).
 :skip-fn (lambda (item)
            (let ((quiz-id (alist-get 'quiz_id item)))
              (and quiz-id (not (eq quiz-id :null)))))
 :skip-reason "a classic quiz's shadow assignment, managed via quizzes.org")
(org-canvas-register-properties "assignments"
  :label "Assignments"
  :file-var 'org-canvas-assignments-file
  :query "LEVEL=1"
  :body-api-key "description"
  :properties
  `((:org-prop "POINTS" :data-key :points_possible :type number
     :doc "Points possible")
    (:org-prop "GRADING_TYPE" :data-key :grading_type :type enum
     :values ,org-canvas--valid-grading-types
     :doc "How the assignment is graded")
    (:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :doc "Whether item is visible (default: true)")
    (:org-prop "SUBMISSION" :data-key :submission_types :type csv-enum
     :values ,org-canvas--valid-submission-types
     :doc "Submission type (see below)")
    (:org-prop "ALLOWED_EXTENSIONS" :data-key :allowed_extensions :type csv-enum
     :doc "File extensions accepted for online uploads (comma separated)")
    (:org-prop "MAX_ATTEMPTS" :data-key :allowed_attempts :type number
     :doc "Max submission attempts (-1 = unlimited)")
    (:org-prop "DUE_AT" :data-key :due_at :type timestamp
     :doc "Due date")
    (:org-prop "UNLOCK_AT" :data-key :unlock_at :type timestamp
     :doc "Available from date")
    (:org-prop "LOCK_AT" :data-key :lock_at :type timestamp
     :doc "Available until date")
    (:org-prop "PEER_REVIEWS" :data-key :peer_reviews :type boolean
     :doc "Enable peer reviews")
    (:org-prop "PEER_REVIEW_COUNT" :data-key :peer_review_count :type number
     :doc "Reviews per student")
    (:org-prop "PEER_REVIEW_DUE_AT" :data-key :peer_reviews_due_at :type timestamp
     :doc "Peer review due date")
    (:org-prop "GROUP" :data-key :assignment_group_id :type link
     :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID"
     :doc "Link to assignment-groups.org heading")
    (:org-prop "RUBRIC_LINK" :data-key :rubric_id :type link
     :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID"
     :remote-fn org-canvas--assignment-remote-rubric-id
     :doc "Link to rubrics.org heading")
    (:org-prop "RUBRIC_USE_FOR_GRADING" :data-key :use_rubric_for_grading :type boolean
     :doc "Rubric total becomes the grade; sent with the rubric association (needs RUBRIC_LINK)")
    (:org-prop "RUBRIC_HIDE_SCORE_TOTAL" :data-key :rubric_hide_score_total :type boolean
     :remote-fn ,#'org-canvas--assignment-remote-hide-score-total
     :doc "Hide the rubric's score total from students; sent with the rubric association")
    (:org-prop "OMIT_FROM_GRADES" :data-key :omit_from_final_grade :type boolean
     :doc "Exclude from grade calculations")
    (:org-prop "ANONYMOUS_GRADING" :data-key :anonymous_grading :type boolean
     :doc "Hide student names during grading")
    (:org-prop "NOTIFY_OF_UPDATE" :data-key :notify_of_update :type boolean
     :doc "Notify students of changes (write-only)")
    (:org-prop "AUTOMATIC_PEER_REVIEWS" :data-key :automatic_peer_reviews :type boolean
     :doc "Auto-assign peer reviews (default: true when PEER_REVIEWS set)")
    (:org-prop "GRADE_INDIVIDUALLY" :data-key :grade_group_students_individually :type boolean
     :doc "Grade group members individually")
    (:org-prop "ONLY_VISIBLE_TO_OVERRIDES" :data-key :only_visible_to_overrides :type boolean
     :doc "Only visible to students with overrides")
    (:org-prop "MODERATED_GRADING" :data-key :moderated_grading :type boolean
     :doc "Enable moderated grading")
    (:org-prop "GRADER_COUNT" :data-key :grader_count :type number
     :doc "Number of graders (required when MODERATED_GRADING is true)")
    (:org-prop "MUTED" :data-key :muted :type boolean
     :doc "Mute assignment (hide grades from students)")
    (:org-prop "GRADING_STANDARD_ID" :data-key :grading_standard_id :type number
     :doc "Canvas grading standard ID")
    (:org-prop "EXTERNAL_TOOL_URL" :data-key :external_tool_url :type string
     :remote-fn org-canvas--assignment-remote-tool-url
     :doc "LTI launch URL (requires SUBMISSION: external_tool)")
    (:org-prop "EXTERNAL_TOOL_ID" :data-key :external_tool_id :type number
     :remote-fn org-canvas--assignment-remote-tool-id
     :doc "Installed LTI tool id, sent as content_id")
    (:org-prop "EXTERNAL_TOOL_NEW_TAB" :data-key :external_tool_new_tab :type boolean
     :remote-fn org-canvas--assignment-remote-tool-new-tab
     :doc "Launch the tool in a new tab")
    (:org-prop "GROUP_CATEGORY_ID" :data-key :group_category_id :type number
     :doc "Group set ID or link to group-categories.org")
    (:org-prop "POSITION" :data-key :position :type number
     :doc "Position in assignment list"))
  :date-order '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
  :structural-fn #'org-canvas--validate-assignment-structure)

;;;; Helper Functions

(defun org-canvas--assignment-resolve-link-id (link-string id-property)
  "Resolve LINK-STRING to a CANVAS_ID by following the org link.
ID-PROPERTY specifies which property to look for (e.g., CANVAS_ID)."
  (org-canvas--resolve-link-property link-string id-property org-canvas-assignments-file))

(defun org-canvas--assignment-parse-extensions (ext-string)
  "Parse EXT-STRING into a list of allowed extensions.
Accepts comma or space separated values like \"py, txt\" or \"py txt\"."
  (when ext-string
    (split-string ext-string "[, \t]+" t)))

(defun org-canvas--assignment-parse-submission-types (type-string)
  "Convert TYPE-STRING to Canvas submission_types array.
Accepts: online_upload, online_url, online_text_entry, media_recording,
         on_paper, external_tool, none, or comma-separated combinations."
  (if type-string
      (let ((types (mapcar #'string-trim (split-string type-string "[, \t]+" t))))
        (dolist (t-val types)
          (unless (member t-val org-canvas--valid-submission-types)
            (when (boundp 'org-canvas--logger)
              (org-canvas--log-warning org-canvas--logger
                "[Validate] SUBMISSION: '%s' is not valid (expected: %s)"
                t-val (string-join org-canvas--valid-submission-types ", ")))
            (message "Warning: SUBMISSION '%s' is not valid" t-val)))
        types)
    '("none")))

;;;; 1. Stage: Extraction

(defun org-canvas--assignment-read-props (pom)
  "Read raw property strings from the assignment heading at POM.
Link properties (GROUP, RUBRIC_LINK, GROUP_CATEGORY_ID) are resolved
here since they require file I/O."
  (let ((group-link (org-entry-get pom "GROUP"))
        (rubric-link (org-entry-get pom "RUBRIC_LINK")))
    (list :title-raw (org-get-heading t t t t)
          :canvas-id (org-entry-get pom "CANVAS_ID")
          :points-raw (org-entry-get pom "POINTS")
          :grading-type-raw (org-entry-get pom "GRADING_TYPE")
          :published-raw (org-entry-get pom "PUBLISHED")
          :due-at-raw (org-entry-get pom "DUE_AT")
          :unlock-at-raw (org-entry-get pom "UNLOCK_AT")
          :lock-at-raw (org-entry-get pom "LOCK_AT")
          :submission-raw (org-entry-get pom "SUBMISSION")
          :allowed-extensions-raw (org-entry-get pom "ALLOWED_EXTENSIONS")
          :max-attempts-raw (org-entry-get pom "MAX_ATTEMPTS")
          :peer-reviews-raw (org-entry-get pom "PEER_REVIEWS")
          :peer-review-count-raw (org-entry-get pom "PEER_REVIEW_COUNT")
          :peer-review-due-at-raw (org-entry-get pom "PEER_REVIEW_DUE_AT")
          :automatic-peer-reviews-raw (org-entry-get pom "AUTOMATIC_PEER_REVIEWS")
          :omit-from-grades-raw (org-entry-get pom "OMIT_FROM_GRADES")
          :anonymous-grading-raw (org-entry-get pom "ANONYMOUS_GRADING")
          :notify-of-update-raw (org-entry-get pom "NOTIFY_OF_UPDATE")
          :grade-individually-raw (org-entry-get pom "GRADE_INDIVIDUALLY")
          :only-visible-to-overrides-raw (org-entry-get pom "ONLY_VISIBLE_TO_OVERRIDES")
          :moderated-grading-raw (org-entry-get pom "MODERATED_GRADING")
          :grader-count-raw (org-entry-get pom "GRADER_COUNT")
          :muted-raw (org-entry-get pom "MUTED")
          :rubric-use-for-grading-raw (org-entry-get pom "RUBRIC_USE_FOR_GRADING")
          :rubric-hide-score-total-raw (org-entry-get pom "RUBRIC_HIDE_SCORE_TOTAL")
          :grading-standard-id-raw (org-entry-get pom "GRADING_STANDARD_ID")
          :position-raw (org-entry-get pom "POSITION")
          :external-tool-url-raw (org-entry-get pom "EXTERNAL_TOOL_URL")
          :external-tool-id-raw (org-entry-get pom "EXTERNAL_TOOL_ID")
          :external-tool-new-tab-raw (org-entry-get pom "EXTERNAL_TOOL_NEW_TAB")
          ;; Resolved links (I/O)
          :assignment-group-id-raw (org-canvas--assignment-resolve-link-id group-link "CANVAS_ID")
          :rubric-id (org-canvas--assignment-resolve-link-id rubric-link "CANVAS_ID")
          :group-category-id-raw (org-canvas--resolve-link-or-raw
                                  pom "GROUP_CATEGORY_ID" "CANVAS_ID"
                                  org-canvas-assignments-file))))

(defun org-canvas--assignment-tristate (raw)
  "Return t for \"true\", `:json-false' for \"false\", nil when RAW is unset.
Association flags use this rather than a plain boolean so that a
property left out of the drawer is not sent as false."
  (cond ((null raw) nil)
        ((string-equal "true" raw) t)
        ((string-equal "false" raw) :json-false)))

(defun org-canvas--assignment-remote-hide-score-total (item)
  "Return the hide_score_total flag of ITEM's rubric settings.
Canvas nests it under `rubric_settings', so the drift report cannot
read it as a flat field."
  (alist-get 'hide_score_total (alist-get 'rubric_settings item)))

(defun org-canvas--assignment-remote-rubric-id (item)
  "Return the id of the rubric attached to ITEM, or nil.
An assignment carries no `rubric_id'; the association lives under
`rubric_settings'."
  (alist-get 'id (alist-get 'rubric_settings item)))

(defun org-canvas--assignment-transform-props (raw)
  "Transform raw property strings RAW into typed assignment data.
Pure function — no buffer access."
  (let ((points (plist-get raw :points-raw))
        (max-attempts (plist-get raw :max-attempts-raw))
        (agid (plist-get raw :assignment-group-id-raw))
        (peer-count (plist-get raw :peer-review-count-raw))
        (gcid (plist-get raw :group-category-id-raw))
        (grader-count (plist-get raw :grader-count-raw))
        (gsid (plist-get raw :grading-standard-id-raw))
        (position (plist-get raw :position-raw))
        (tool-id (plist-get raw :external-tool-id-raw)))
    (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
          :canvas-id (plist-get raw :canvas-id)
          :points_possible (when points (org-canvas--safe-string-to-number points "POINTS"))
          :grading_type (org-canvas--validate-property
                         (plist-get raw :grading-type-raw)
                         org-canvas--valid-grading-types
                         "GRADING_TYPE" "points")
          :published (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
          :due_at (org-canvas-org-parse-timestamp (plist-get raw :due-at-raw))
          :unlock_at (org-canvas-org-parse-timestamp (plist-get raw :unlock-at-raw))
          :lock_at (org-canvas-org-parse-timestamp (plist-get raw :lock-at-raw))
          :submission_types (org-canvas--assignment-parse-submission-types
                             (plist-get raw :submission-raw))
          :allowed_extensions (org-canvas--assignment-parse-extensions
                               (plist-get raw :allowed-extensions-raw))
          :allowed_attempts (when max-attempts
                              (org-canvas--safe-string-to-number max-attempts "MAX_ATTEMPTS"))
          :assignment_group_id (when agid (string-to-number agid))
          :rubric-id (plist-get raw :rubric-id)
          :rubric-use-for-grading (org-canvas--assignment-tristate
                                   (plist-get raw :rubric-use-for-grading-raw))
          :rubric-hide-score-total (org-canvas--assignment-tristate
                                    (plist-get raw :rubric-hide-score-total-raw))
          :peer_reviews (org-canvas--interpret-boolean (plist-get raw :peer-reviews-raw))
          :peer_review_count (when peer-count
                               (org-canvas--safe-string-to-number peer-count "PEER_REVIEW_COUNT"))
          :peer_reviews_due_at (org-canvas-org-parse-timestamp
                                (plist-get raw :peer-review-due-at-raw))
          :automatic_peer_reviews (org-canvas--interpret-boolean
                                   (plist-get raw :automatic-peer-reviews-raw))
          :omit_from_final_grade (org-canvas--interpret-boolean
                                  (plist-get raw :omit-from-grades-raw))
          :anonymous_grading (org-canvas--interpret-boolean
                              (plist-get raw :anonymous-grading-raw))
          :notify_of_update (org-canvas--interpret-boolean
                             (plist-get raw :notify-of-update-raw))
          :group_category_id (when gcid
                               (org-canvas--safe-string-to-number gcid "GROUP_CATEGORY_ID"))
          :grade_group_students_individually (org-canvas--interpret-boolean
                                              (plist-get raw :grade-individually-raw))
          :only_visible_to_overrides (org-canvas--interpret-boolean
                                      (plist-get raw :only-visible-to-overrides-raw))
          :moderated_grading (org-canvas--interpret-boolean
                              (plist-get raw :moderated-grading-raw))
          :grader_count (when grader-count
                          (org-canvas--safe-string-to-number grader-count "GRADER_COUNT"))
          :muted (org-canvas--interpret-boolean (plist-get raw :muted-raw))
          :grading_standard_id (when gsid
                                 (org-canvas--safe-string-to-number gsid "GRADING_STANDARD_ID"))
          :position (when position
                      (org-canvas--safe-string-to-number position "POSITION"))
          :external_tool_url (plist-get raw :external-tool-url-raw)
          :external_tool_id (when tool-id
                              (org-canvas--safe-string-to-number
                               tool-id "EXTERNAL_TOOL_ID"))
          :external_tool_new_tab (org-canvas--interpret-boolean
                                  (plist-get raw :external-tool-new-tab-raw)))))

(defun org-canvas--assignment-parse-entry ()
  "Extract assignment data from the Org heading at point."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--assignment-read-props pom))
         (data (org-canvas--assignment-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Assignment")

    (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Processing Assignment: '%s' (ID: %s)"
              (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Points: %s, Due: %s, Submission: %s"
                (or (plist-get data :points_possible) "0")
                (or (plist-get data :due_at) "none")
                (plist-get data :submission_types))
    (when (plist-get data :assignment_group_id)
      (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Assignment Group ID: %s"
                  (plist-get data :assignment_group_id)))
   (unless (plist-get data :rubric-id)
      (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Rubric ID: %s"
                  (plist-get data :rubric-id)))

    ;; Extract description (resolves cross-file links to Canvas URLs)
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((description (org-canvas--export-subtree-body-to-html)))
      (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Description size: %d chars" (length description))

      (plist-put data :description description)
      (plist-put data :pom pom)
      data)))

;;;; 2. Stage: Transformation

(defun org-canvas--assignment-add-peer-reviews (data assignment)
  "Add peer review fields from DATA to ASSIGNMENT hash-table when enabled."
  (when (plist-get data :peer_reviews)
    (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Peer reviews enabled")
    (puthash "peer_reviews" t assignment)
    ;; Default automatic_peer_reviews to t when PEER_REVIEWS is set,
    ;; but allow explicit override via AUTOMATIC_PEER_REVIEWS property
    (let ((auto (plist-get data :automatic_peer_reviews)))
      (puthash "automatic_peer_reviews"
               (if (eq auto nil) t (org-canvas--to-json-boolean auto))
               assignment))
    (org-canvas--puthash-when assignment data :peer_review_count "peer_review_count")
    (org-canvas--puthash-when assignment data :peer_reviews_due_at "peer_reviews_due_at")))

(defconst org-canvas--assignment-optional-field-specs
  '(;; (:data-key "hash-key" type)  type: bool = always t, value = use raw value
    (:omit_from_final_grade "omit_from_final_grade" bool)
    (:anonymous_grading "anonymous_grading" bool)
    (:notify_of_update "notify_of_update" bool)
    (:group_category_id "group_category_id" value)
    (:grade_group_students_individually "grade_group_students_individually" bool)
    (:only_visible_to_overrides "only_visible_to_overrides" bool)
    (:moderated_grading "moderated_grading" bool)
    (:muted "muted" bool)
    (:grading_standard_id "grading_standard_id" value)
    (:position "position" value))
  "Specs for optional assignment fields: (data-key hash-key type).")

(defun org-canvas--assignment-add-external-tool (data assignment)
  "Attach `external_tool_tag_attributes' to ASSIGNMENT from DATA.
Canvas takes an LTI-backed assignment (Gradescope and friends) as a
nested object beside `submission_types': the tool is named by launch
URL, or by the installed tool id as CONTENT_ID with the content type
Canvas requires.  Emitted whenever either is declared — an
`external_tool' submission type with neither points at nothing, which
`org-canvas--validate-external-tool' warns about before a push.

NEW_TAB rides along only when the block exists, since an explicit
false is meaningful to Canvas."
  (let ((url (plist-get data :external_tool_url))
        (id (plist-get data :external_tool_id)))
    (when (or url id)
      (let ((tag (make-hash-table :test 'equal)))
        (when url (puthash "url" url tag))
        (when id
          (puthash "content_id" id tag)
          (puthash "content_type" "context_external_tool" tag))
        (puthash "new_tab"
                 (if (plist-get data :external_tool_new_tab) t :json-false)
                 tag)
        (puthash "external_tool_tag_attributes" tag assignment)))))

(defun org-canvas--assignment-remote-tool-attrs (item)
  "Return the nested `external_tool_tag_attributes' alist of ITEM, or nil."
  (alist-get 'external_tool_tag_attributes item))

(defun org-canvas--assignment-remote-tool-url (item)
  "Return the launch URL Canvas has recorded for ITEM, or nil.
A `:remote-fn' because the field is nested — read flat, every
external-tool assignment would report drift on every run."
  (alist-get 'url (org-canvas--assignment-remote-tool-attrs item)))

(defun org-canvas--assignment-remote-tool-id (item)
  "Return the installed tool id Canvas has recorded for ITEM, or nil."
  (alist-get 'content_id (org-canvas--assignment-remote-tool-attrs item)))

(defun org-canvas--assignment-remote-tool-new-tab (item)
  "Return the remote new-tab flag of ITEM, or nil."
  (alist-get 'new_tab (org-canvas--assignment-remote-tool-attrs item)))

(defun org-canvas--assignment-add-optional-fields (data assignment)
  "Add optional fields from DATA to ASSIGNMENT hash-table."
  (dolist (spec org-canvas--assignment-optional-field-specs)
    (when-let* ((val (plist-get data (nth 0 spec))))
      (puthash (nth 1 spec)
               (if (eq (nth 2 spec) 'bool) t val)
               assignment)))
  ;; grader_count only applies when moderated_grading is enabled
  (when (and (plist-get data :moderated_grading) (plist-get data :grader_count))
    (puthash "grader_count" (plist-get data :grader_count) assignment))
  (org-canvas--assignment-add-external-tool data assignment))

(defun org-canvas--assignment-build-payload (data)
  "Convert DATA to Canvas assignment payload."
  (org-canvas--validate-date-ordering data)
  (let ((title (plist-get data :title)))
    (org-canvas--log-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((assignment (make-hash-table :test 'equal)))
      ;; Required fields
      (puthash "name" title assignment)
      (puthash "description" (plist-get data :description) assignment)
      (puthash "published" (org-canvas--to-json-boolean (plist-get data :published)) assignment)
      (puthash "submission_types" (plist-get data :submission_types) assignment)

      ;; Points and grading
      (org-canvas--puthash-when assignment data :points_possible "points_possible")
      (puthash "grading_type" (plist-get data :grading_type) assignment)

      ;; Dates
      (when (plist-get data :due_at)
        (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Due at: %s" (plist-get data :due_at))
        (puthash "due_at" (plist-get data :due_at) assignment))
      (org-canvas--puthash-when assignment data :unlock_at "unlock_at")
      (org-canvas--puthash-when assignment data :lock_at "lock_at")

      ;; Submission settings
      (org-canvas--puthash-when assignment data :allowed_extensions "allowed_extensions")
      (org-canvas--puthash-when assignment data :allowed_attempts "allowed_attempts")

      ;; Assignment group
      (when (plist-get data :assignment_group_id)
        (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Assignment group: %d"
                    (plist-get data :assignment_group_id))
        (puthash "assignment_group_id" (plist-get data :assignment_group_id) assignment))

      ;; Peer reviews
      (org-canvas--assignment-add-peer-reviews data assignment)

      ;; Additional properties
      (org-canvas--assignment-add-optional-fields data assignment)

      (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Payload complete")

      ;; Wrap in "assignment" key as required by Canvas API
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "assignment" assignment payload)
        payload))))

;;;; 3. Stage: Execution Helper

(defun org-canvas--assignment-associate-rubric (assignment-id rubric-id &optional flags)
  "Associate RUBRIC-ID with ASSIGNMENT-ID on Canvas.
FLAGS is the association plist `org-canvas--associate-rubric' takes."
  (org-canvas--associate-rubric assignment-id rubric-id "Assignment" flags))

(defun org-canvas--assignment-post-finalize (data response)
  "Verify assignment properties and associate rubric.
DATA is the parsed assignment plist, RESPONSE is the Canvas API response."
  ;; Verify assignment_group_id
  (let ((expected-group (plist-get data :assignment_group_id))
        (actual-group (alist-get 'assignment_group_id response)))
    (when (and expected-group actual-group
               (not (equal expected-group actual-group)))
      (org-canvas--log-warning org-canvas--logger
        "[Verify] '%s': assignment_group_id mismatch! Expected %s, got %s"
        (plist-get data :title) expected-group actual-group)))
  ;; Associate rubric, carrying the grading flags so a push keeps them.
  ;; Canvas touches the assignment for the association, so the write is
  ;; reported and finalize re-reads updated_at from it (issue #124).
  (let ((rubric-id (plist-get data :rubric-id))
        (assignment-id (alist-get 'id response)))
    (when (and rubric-id
               (org-canvas--assignment-associate-rubric
                assignment-id rubric-id
                (list :use-for-grading (plist-get data :rubric-use-for-grading)
                      :hide-score-total (plist-get data :rubric-hide-score-total))))
      (org-canvas--finalize-note-remote-write))))

;;;; Main Sync Function

;; Generate org-canvas-sync-assignments using the pipeline macro
(defun org-canvas--assignment-rubric-hash-extra (data)
  "Return DATA's rubric material for change detection, or \"\".
The rubric id and the association flags travel outside the assignment
payload, so without this an added or changed RUBRIC_LINK never dirtied
the entry and its association was never made (issue #120).  Empty when
the heading carries none of them, so other headings keep their hash."
  (let ((rubric-id (plist-get data :rubric-id))
        (use-for-grading (plist-get data :rubric-use-for-grading))
        (hide-score-total (plist-get data :rubric-hide-score-total)))
    (if (or rubric-id use-for-grading hide-score-total)
        (format "rubric:%s:%s:%s" rubric-id use-for-grading hide-score-total)
      "")))

(org-canvas-define-sync assignments
  :file org-canvas-assignments-file
  :parse #'org-canvas--assignment-parse-entry
  :build #'org-canvas--assignment-build-payload
  :endpoint "assignments"
  :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name))
  :post-fn #'org-canvas--assignment-post-finalize
  :hash-extra #'org-canvas--assignment-rubric-hash-extra
  :pull-item-fn #'org-canvas--assignment-pull-item)

;;;; Delete Functions

;; Generate org-canvas-delete-all-assignments using the delete macro
;; Note: assignments use 'name instead of 'title for the title field
(org-canvas-define-delete-all assignments
  :endpoint "assignments"
  :file org-canvas-assignments-file
  :title-field 'name)

;; Generate org-canvas-delete-assignment-at-point using the delete macro
(org-canvas-define-delete-at-point assignment
  :endpoint "assignments/%s")

;;;; Pull

(defun org-canvas--assignment-resolve-group-link (group-id)
  "Resolve assignment GROUP-ID to an Org link into assignment-groups.org.
Returns a string like \"[[file:assignment-groups.org::*Name][Name]]\"
or nil.  The registry's GROUP spec resolves the same way during a pull;
this is the one spelling kept for callers that hold a bare id."
  (org-canvas--pull-resolve-link
   (list :type 'link :target-file 'org-canvas-assignment-groups-file
         :link-id-property "CANVAS_ID")
   group-id))

(defun org-canvas--assignment-pull-after (item pos)
  "Write what the registry cannot express for the assignment pulled at POS.
ITEM is the API response alist: its description becomes the heading's
body, and its section overrides, fetched separately, its table.  Every
property comes from the registry (issue #135), so a field the push
side reads is a field the pull writes — PUBLISHED, SUBMISSION, the LTI
tool attributes — and nothing outside the schema lands in the drawer."
  (org-with-point-at pos
    (org-canvas--pull-insert-body (alist-get 'description item)))
  (let ((overrides (org-canvas--override-fetch (alist-get 'id item))))
    (when overrides
      (save-excursion
        (goto-char pos)
        (org-back-to-heading t)
        (org-end-of-meta-data t)
        (org-canvas--override-emit-table
         overrides
         (alist-get 'due_at item)
         (alist-get 'unlock_at item)
         (alist-get 'lock_at item))))))

(org-canvas-define-pull-item assignment
  :registry-key "assignments"
  :after-pull #'org-canvas--assignment-pull-after)

(org-canvas-define-pull assignments
  :file org-canvas-assignments-file
  :endpoint "assignments"
  :title-field 'name
  :secondary-sort-key 'assignment_group_id
  :tertiary-sort-key (when (eq org-canvas-assignment-sort 'due-at) 'due_at)
  :pull-item-fn #'org-canvas--assignment-pull-item)

;;;; External Tools (LTI)

(defconst org-canvas--external-tools-buffer "*canvas-external-tools*"
  "Buffer listing the LTI tools installed in the course.")

(defun org-canvas--external-tools-fetch ()
  "Return the LTI tools available in the course, account-installed included.
Without `include_parents' Canvas lists only tools installed on the
course itself, which on most institutional accounts is nothing at all
— the tools instructors actually use (Gradescope, Turnitin, a
publisher platform) are installed account-wide."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint "external_tools")
           '(("include_parents" . "true")))
          nil))

(defun org-canvas--external-tools-insert (tools)
  "Insert one line per entry of TOOLS at point."
  (dolist (tool tools)
    (insert (format "%-40s %s\n"
                    (or (alist-get 'name tool) "(unnamed)")
                    (or (alist-get 'url tool)
                        (alist-get 'domain tool)
                        "(no launch URL)")))))

;;;###autoload
(defun org-canvas-list-external-tools ()
  "List the LTI tools installed in this course with their launch URLs.
An `external_tool' assignment is pointed at a tool by URL
\(EXTERNAL_TOOL_URL) or by installed-tool id (EXTERNAL_TOOL_ID), and
neither is guessable — this is where you read them off.  Read-only."
  (interactive)
  (org-canvas--ensure-credentials)
  (let ((tools (org-canvas--external-tools-fetch)))
    (with-current-buffer (get-buffer-create org-canvas--external-tools-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "LTI tools available in course %s\n"
                        org-canvas-course-id))
        (insert (make-string 60 ?=) "\n\n")
        (if (null tools)
            (insert "No tools are installed in this course or its account.\n")
          (org-canvas--external-tools-insert tools))
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)))
    (message "%d external tool(s); see %s"
             (length tools) org-canvas--external-tools-buffer)))

(provide 'org-canvas-assignments)
;;; org-canvas-assignments.el ends here
