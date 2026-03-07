;;; org-canvas-assignments.el --- Pipeline-based Assignment Sync -*- lexical-binding: t; -*-

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
(require 'elog)

;;;; Configuration

(defcustom org-canvas-assignments-file (org-canvas--path "assignments.org")
  "Path to the assignments.org file."
  :type 'file
  :group 'org-canvas)

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
              (elog-warning org-canvas--logger
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
          :turnitin-enabled-raw (org-entry-get pom "TURNITIN_ENABLED")
          :grading-standard-id-raw (org-entry-get pom "GRADING_STANDARD_ID")
          :position-raw (org-entry-get pom "POSITION")
          ;; Resolved links (I/O)
          :assignment-group-id-raw (org-canvas--assignment-resolve-link-id group-link "CANVAS_ID")
          :rubric-id (org-canvas--assignment-resolve-link-id rubric-link "CANVAS_ID")
          :group-category-id-raw (org-canvas--resolve-link-or-raw
                                  pom "GROUP_CATEGORY_ID" "CANVAS_ID"
                                  org-canvas-assignments-file))))

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
        (position (plist-get raw :position-raw)))
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
          :turnitin_enabled (org-canvas--interpret-boolean
                             (plist-get raw :turnitin-enabled-raw))
          :grading_standard_id (when gsid
                                 (org-canvas--safe-string-to-number gsid "GRADING_STANDARD_ID"))
          :position (when position
                      (org-canvas--safe-string-to-number position "POSITION")))))

(defun org-canvas--assignment-parse-entry ()
  "Extract assignment data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--assignment-read-props pom))
         (data (org-canvas--assignment-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Assignment")

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Assignment: '%s' (ID: %s)"
              (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Points: %s, Due: %s, Submission: %s"
                (or (plist-get data :points_possible) "0")
                (or (plist-get data :due_at) "none")
                (plist-get data :submission_types))
    (when (plist-get data :assignment_group_id)
      (elog-debug org-canvas--logger "[Stage 1: Parse] Assignment Group ID: %s"
                  (plist-get data :assignment_group_id)))
    (when (plist-get data :rubric-id)
      (elog-debug org-canvas--logger "[Stage 1: Parse] Rubric ID: %s"
                  (plist-get data :rubric-id)))

    ;; Extract description (resolves cross-file links to Canvas URLs)
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((description (org-canvas--export-subtree-body-to-html)))
      (elog-info org-canvas--logger "[Stage 1: Parse] Description size: %d chars" (length description))

      (plist-put data :description description)
      (plist-put data :pom pom)
      data)))

;;;; 2. Stage: Transformation

(defun org-canvas--assignment-add-peer-reviews (data assignment)
  "Add peer review fields from DATA to ASSIGNMENT hash-table when enabled."
  (when (plist-get data :peer_reviews)
    (elog-debug org-canvas--logger "[Stage 2: Transform] Peer reviews enabled")
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
    (:turnitin_enabled "turnitin_enabled" bool)
    (:grading_standard_id "grading_standard_id" value)
    (:position "position" value))
  "Specs for optional assignment fields: (data-key hash-key type).")

(defun org-canvas--assignment-add-optional-fields (data assignment)
  "Add optional fields from DATA to ASSIGNMENT hash-table."
  (dolist (spec org-canvas--assignment-optional-field-specs)
    (when-let* ((val (plist-get data (nth 0 spec))))
      (puthash (nth 1 spec)
               (if (eq (nth 2 spec) 'bool) t val)
               assignment)))
  ;; grader_count only applies when moderated_grading is enabled
  (when (and (plist-get data :moderated_grading) (plist-get data :grader_count))
    (puthash "grader_count" (plist-get data :grader_count) assignment)))

(defun org-canvas--assignment-build-payload (data)
  "Convert DATA to Canvas assignment payload."
  (org-canvas--validate-date-ordering data)
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

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
        (elog-debug org-canvas--logger "[Stage 2: Transform] Due at: %s" (plist-get data :due_at))
        (puthash "due_at" (plist-get data :due_at) assignment))
      (org-canvas--puthash-when assignment data :unlock_at "unlock_at")
      (org-canvas--puthash-when assignment data :lock_at "lock_at")

      ;; Submission settings
      (org-canvas--puthash-when assignment data :allowed_extensions "allowed_extensions")
      (org-canvas--puthash-when assignment data :allowed_attempts "allowed_attempts")

      ;; Assignment group
      (when (plist-get data :assignment_group_id)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Assignment group: %d"
                    (plist-get data :assignment_group_id))
        (puthash "assignment_group_id" (plist-get data :assignment_group_id) assignment))

      ;; Peer reviews
      (org-canvas--assignment-add-peer-reviews data assignment)

      ;; Additional properties
      (org-canvas--assignment-add-optional-fields data assignment)

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")

      ;; Wrap in "assignment" key as required by Canvas API
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "assignment" assignment payload)
        payload))))

;;;; 3. Stage: Execution Helper

(defun org-canvas--assignment-associate-rubric (assignment-id rubric-id)
  "Associate RUBRIC-ID with ASSIGNMENT-ID on Canvas."
  (org-canvas--associate-rubric assignment-id rubric-id "Assignment"))

(defun org-canvas--assignment-post-finalize (data response)
  "Verify assignment properties and associate rubric.
DATA is the parsed assignment plist, RESPONSE is the Canvas API response."
  ;; Verify assignment_group_id
  (let ((expected-group (plist-get data :assignment_group_id))
        (actual-group (alist-get 'assignment_group_id response)))
    (when (and expected-group actual-group
               (not (equal expected-group actual-group)))
      (elog-warning org-canvas--logger
        "[Verify] '%s': assignment_group_id mismatch! Expected %s, got %s"
        (plist-get data :title) expected-group actual-group)))
  ;; Associate rubric
  (let ((rubric-id (plist-get data :rubric-id))
        (assignment-id (alist-get 'id response)))
    (when rubric-id
      (org-canvas--assignment-associate-rubric assignment-id rubric-id))))

;;;; Main Sync Function

;; Generate org-canvas-sync-assignments using the pipeline macro
(org-canvas-define-sync assignments
  :file org-canvas-assignments-file
  :parse #'org-canvas--assignment-parse-entry
  :build #'org-canvas--assignment-build-payload
  :endpoint "assignments"
  :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name))
  :post-fn #'org-canvas--assignment-post-finalize
  :pull-item-fn #'org-canvas--assignment-pull-item)

;;;; Push-at-Point

(org-canvas-define-push-at-point assignment
  :parse #'org-canvas--assignment-parse-entry
  :build #'org-canvas--assignment-build-payload
  :endpoint "assignments"
  :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name))
  :post-fn #'org-canvas--assignment-post-finalize
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
  "Resolve assignment GROUP-ID to an Org link to assignment-groups.org.
Returns a string like \"[[file:assignment-groups.org::*Name][Name]]\" or nil."
  (when group-id
    (let* ((groups-file (expand-file-name org-canvas-assignment-groups-file))
           (group-name nil))
      (when (file-exists-p groups-file)
        (with-current-buffer (find-file-noselect groups-file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (when (equal (org-entry-get (point) "CANVAS_ID")
                            (format "%s" group-id))
                 (setq group-name (org-get-heading t t t t))))
             "LEVEL=1" 'file))))
      (when group-name
        (format "[[file:assignment-groups.org::*%s][%s]]"
                group-name group-name)))))

(defun org-canvas--assignment-pull-item (item pos)
  "Set per-item properties for a pulled assignment.
ITEM is the API response alist, POS is the heading position."
  (let ((points (alist-get 'points_possible item))
        (submission-types (alist-get 'submission_types item))
        (peer-reviews (alist-get 'peer_reviews item))
        (group-id (alist-get 'assignment_group_id item)))
    (when points
      (org-canvas-org-set-property pos "POINTS" (format "%s" points)))
    (org-canvas--pull-set-timestamp-property pos "DUE_AT" (alist-get 'due_at item))
    (org-canvas--pull-set-timestamp-property pos "UNLOCK_AT" (alist-get 'unlock_at item))
    (org-canvas--pull-set-timestamp-property pos "LOCK_AT" (alist-get 'lock_at item))
    (when submission-types
      (org-canvas-org-set-property
       pos "SUBMISSION_TYPES"
       (mapconcat #'identity (append submission-types nil) ",")))
    (when peer-reviews
      (org-canvas--pull-set-boolean-property pos "PEER_REVIEWS" peer-reviews))
    (org-canvas--pull-set-boolean-property pos "OMIT_FROM_GRADES" (alist-get 'omit_from_final_grade item))
    (org-canvas--pull-set-boolean-property pos "ANONYMOUS_GRADING" (alist-get 'anonymous_grading item))
    (org-canvas--pull-set-boolean-property pos "ONLY_VISIBLE_TO_OVERRIDES" (alist-get 'only_visible_to_overrides item))
    (org-canvas--pull-set-boolean-property pos "MODERATED_GRADING" (alist-get 'moderated_grading item))
    (let ((gc (alist-get 'grader_count item)))
      (when (and gc (> gc 0))
        (org-canvas-org-set-property pos "GRADER_COUNT" (format "%s" gc))))
    (org-canvas--pull-set-boolean-property pos "GRADE_INDIVIDUALLY" (alist-get 'grade_group_students_individually item))
    (let ((gcat-id (alist-get 'group_category_id item)))
      (when gcat-id
        (org-canvas-org-set-property pos "GROUP_CATEGORY_ID" (format "%s" gcat-id))))
    (let ((apos (alist-get 'position item)))
      (when apos
        (org-canvas-org-set-property pos "POSITION" (format "%s" apos))))
    (org-canvas--pull-set-boolean-property pos "MUTED" (alist-get 'muted item))
    (org-canvas--pull-set-boolean-property pos "TURNITIN_ENABLED" (alist-get 'turnitin_enabled item))
    (let ((gs-id (alist-get 'grading_standard_id item)))
      (when gs-id
        (org-canvas-org-set-property pos "GRADING_STANDARD_ID" (format "%s" gs-id))))
    (let ((group-link (org-canvas--assignment-resolve-group-link group-id)))
      (when group-link
        (org-canvas-org-set-property pos "GROUP" group-link))))
  (org-canvas--pull-insert-body (alist-get 'description item)))

(org-canvas-define-pull assignments
  :file org-canvas-assignments-file
  :endpoint "assignments"
  :title-field 'name
  :item-fn #'org-canvas--assignment-pull-item)

(provide 'org-canvas-assignments)
;;; org-canvas-assignments.el ends here
