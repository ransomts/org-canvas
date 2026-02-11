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

(defconst org-canvas--valid-submission-types
  '("online_upload" "online_url" "online_text_entry" "media_recording"
    "on_paper" "external_tool" "none")
  "Valid Canvas submission types.")

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

(defun org-canvas--assignment-parse-entry ()
  "Extract assignment data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         ;; Core properties
         (points (org-canvas-org-get-property pom "POINTS"))
         (grading-type (org-canvas--validate-property
                        (org-canvas-org-get-property pom "GRADING_TYPE")
                        '("points" "percent" "letter_grade" "gpa_scale" "pass_fail" "not_graded")
                        "GRADING_TYPE" "points"))
         (published (org-canvas-org-get-boolean-property pom "PUBLISHED" t))
         ;; Dates
         (due-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "DUE_AT")))
         (unlock-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "UNLOCK_AT")))
         (lock-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "LOCK_AT")))
         ;; Submission settings
         (submission-types (org-canvas--assignment-parse-submission-types
                            (org-canvas-org-get-property pom "SUBMISSION")))
         (allowed-extensions (org-canvas--assignment-parse-extensions
                              (org-canvas-org-get-property pom "ALLOWED_EXTENSIONS")))
         (max-attempts (org-canvas-org-get-property pom "MAX_ATTEMPTS"))
         ;; Links to other entities
         (group-link (org-canvas-org-get-property pom "GROUP"))
         (assignment-group-id (org-canvas--assignment-resolve-link-id group-link "CANVAS_ID"))
         (rubric-link (org-canvas-org-get-property pom "RUBRIC_LINK"))
         (rubric-id (org-canvas--assignment-resolve-link-id rubric-link "CANVAS_ID"))
         ;; Peer reviews
         (peer-reviews (org-canvas-org-get-boolean-property pom "PEER_REVIEWS"))
         (peer-review-count (org-canvas-org-get-property pom "PEER_REVIEW_COUNT"))
         (peer-review-due-at (org-canvas-org-parse-timestamp
                              (org-canvas-org-get-property pom "PEER_REVIEW_DUE_AT"))))

    (when (or (null title) (string-empty-p title))
      (error "Assignment title cannot be empty at point %d" pom))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Assignment: '%s' (ID: %s)" title (or canvas-id "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Points: %s, Due: %s, Submission: %s"
                (or points "0") (or due-at "none") submission-types)
    (when assignment-group-id
      (elog-debug org-canvas--logger "[Stage 1: Parse] Assignment Group ID: %s" assignment-group-id))
    (when rubric-id
      (elog-debug org-canvas--logger "[Stage 1: Parse] Rubric ID: %s" rubric-id))

    ;; Extract description (resolves cross-file links to Canvas URLs)
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((description (org-canvas--export-subtree-body-to-html)))
      (elog-info org-canvas--logger "[Stage 1: Parse] Description size: %d chars" (length description))

      (list :title title
            :description description
            :canvas-id canvas-id
            :points_possible (when points (org-canvas--safe-string-to-number points "POINTS"))
            :grading_type grading-type
            :published published
            :due_at due-at
            :unlock_at unlock-at
            :lock_at lock-at
            :submission_types submission-types
            :allowed_extensions allowed-extensions
            :allowed_attempts (when max-attempts (org-canvas--safe-string-to-number max-attempts "MAX_ATTEMPTS"))
            :assignment_group_id (when assignment-group-id (string-to-number assignment-group-id))
            :rubric-id rubric-id
            :peer_reviews peer-reviews
            :peer_review_count (when peer-review-count (org-canvas--safe-string-to-number peer-review-count "PEER_REVIEW_COUNT"))
            :peer_reviews_due_at peer-review-due-at
            :pom pom))))

;;;; 2. Stage: Transformation

(defun org-canvas--assignment-add-peer-reviews (data assignment)
  "Add peer review fields from DATA to ASSIGNMENT hash-table when enabled."
  (when (plist-get data :peer_reviews)
    (elog-debug org-canvas--logger "[Stage 2: Transform] Peer reviews enabled")
    (puthash "peer_reviews" t assignment)
    (puthash "automatic_peer_reviews" t assignment)
    (when (plist-get data :peer_review_count)
      (puthash "peer_review_count" (plist-get data :peer_review_count) assignment))
    (when (plist-get data :peer_reviews_due_at)
      (puthash "peer_reviews_due_at" (plist-get data :peer_reviews_due_at) assignment))))

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
      (when (plist-get data :points_possible)
        (puthash "points_possible" (plist-get data :points_possible) assignment))
      (puthash "grading_type" (plist-get data :grading_type) assignment)

      ;; Dates
      (when (plist-get data :due_at)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Due at: %s" (plist-get data :due_at))
        (puthash "due_at" (plist-get data :due_at) assignment))
      (when (plist-get data :unlock_at)
        (puthash "unlock_at" (plist-get data :unlock_at) assignment))
      (when (plist-get data :lock_at)
        (puthash "lock_at" (plist-get data :lock_at) assignment))

      ;; Submission settings
      (when (plist-get data :allowed_extensions)
        (puthash "allowed_extensions" (plist-get data :allowed_extensions) assignment))
      (when (plist-get data :allowed_attempts)
        (puthash "allowed_attempts" (plist-get data :allowed_attempts) assignment))

      ;; Assignment group
      (when (plist-get data :assignment_group_id)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Assignment group: %d"
                    (plist-get data :assignment_group_id))
        (puthash "assignment_group_id" (plist-get data :assignment_group_id) assignment))

      ;; Peer reviews
      (org-canvas--assignment-add-peer-reviews data assignment)

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")

      ;; Wrap in "assignment" key as required by Canvas API
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "assignment" assignment payload)
        payload))))

;;;; 3. Stage: Execution

(defun org-canvas--assignment-search-by-name (name)
  "Search for an assignment with NAME on Canvas.  Return nil on error."
  (org-canvas--search-item "assignments" name :match-field 'name))

(defun org-canvas--assignment-push-to-api (data payload)
  "Send PAYLOAD using DATA to Canvas API.
Handles 404 on PUT by retrying as POST (stale CANVAS_ID recovery).
Handles timeout by searching for the assignment."
  (org-canvas--push-to-api data payload
    :endpoint "assignments"
    :find-fn #'org-canvas--assignment-search-by-name))

(defun org-canvas--assignment-associate-rubric (assignment-id rubric-id)
  "Associate RUBRIC-ID with ASSIGNMENT-ID on Canvas."
  (elog-info org-canvas--logger "[Rubric] Associating rubric %s with assignment %s"
             rubric-id assignment-id)
  (let* ((endpoint (org-canvas-api-course-endpoint "rubric_associations"))
         (payload (make-hash-table :test 'equal))
         (assoc (make-hash-table :test 'equal)))
    (puthash "rubric_id" (string-to-number rubric-id) assoc)
    (puthash "association_id" assignment-id assoc)
    (puthash "association_type" "Assignment" assoc)
    (puthash "purpose" "grading" assoc)
    (puthash "rubric_association" assoc payload)
    (condition-case err
        (progn
          (org-canvas-api-request 'POST endpoint :data payload)
          (elog-info org-canvas--logger "[Rubric] Association created"))
      (error
       (elog-warning org-canvas--logger "[Rubric] Association failed: %s" (cadr err))))))

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

;;;; 4. Stage: Finalization

(defun org-canvas--assignment-finalize (data response)
  "Update local Org file with CANVAS_ID using DATA and RESPONSE."
  (org-canvas--finalize-item data response
    :post-fn #'org-canvas--assignment-post-finalize))

;;;; Main Sync Function

;; Generate org-canvas-sync-assignments using the pipeline macro
(org-canvas-define-sync assignments
  :file org-canvas-assignments-file
  :parse #'org-canvas--assignment-parse-entry
  :build #'org-canvas--assignment-build-payload
  :push #'org-canvas--assignment-push-to-api
  :finalize #'org-canvas--assignment-finalize)

;;;; Push-at-Point

(org-canvas-define-push-at-point assignment
  :parse #'org-canvas--assignment-parse-entry
  :build #'org-canvas--assignment-build-payload
  :push #'org-canvas--assignment-push-to-api
  :finalize #'org-canvas--assignment-finalize)

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
