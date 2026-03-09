;;; org-canvas-rubrics.el --- Rubric Sync Pipeline for Canvas LMS -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the sync pipeline for Canvas Rubrics.
;;
;; FILE STRUCTURE
;; ==============
;; In rubrics.org:
;;   - Level 1 headings = Rubrics
;;   - Org tables under headings = Criteria
;;
;; TABLE FORMAT
;; ============
;; | Criterion Description | Points | Long Description (optional) | Outcome (optional)                                              |
;; |-----------------------+--------+-----------------------------+-----------------------------------------------------------------|
;; | Code Quality          |     10 | Well-formatted, readable    | [[file:outcomes.org::*Python Proficiency][Python Proficiency]] |
;; | Correctness           |     15 | Passes all test cases       |                                                                 |
;;
;; Column 1: Short criterion name (shown in rubric)
;; Column 2: Maximum points for this criterion
;; Column 3: Optional longer description
;; Column 4: Optional link to an outcome in outcomes.org (for mastery tracking)
;;
;; PROPERTIES
;; ==========
;; FREE_FORM_CRITERION_COMMENTS - Allow freeform feedback (t or nil)
;; HIDE_SCORE_TOTAL            - Hide total from students
;;
;; RUBRIC ASSOCIATION
;; ==================
;; Rubrics are associated with assignments via the RUBRIC_LINK property
;; in assignments.org.  The association happens during assignment sync,
;; not during rubric sync.
;;
;; CONFLICT HANDLING
;; =================
;; If a rubric with the same title already exists, it is deleted first
;; before creating the new one.  This avoids duplicate rubrics.

;;; Code:

(require 'org-canvas-core)
(require 'org-table)
(require 'cl-lib)
(require 'elog)

;;;; Configuration

(defcustom org-canvas-rubrics-file (org-canvas--path "rubrics.org")
  "Path to the rubrics.org file."
  :type 'file
  :group 'org-canvas)

;;;; 1. Stage: Extraction

(defun org-canvas--rubric-read-props (pom)
  "Read raw property strings and table data from the rubric heading at POM.
Table extraction requires buffer access (org-table-to-lisp)."
  (let ((table-data
         (save-excursion
           (save-restriction
             (org-narrow-to-subtree)
             (goto-char (point-min))
             (when (re-search-forward org-table-line-regexp nil t)
               (beginning-of-line)
               (org-table-to-lisp))))))
    (list :title-raw (org-get-heading t t t t)
          :canvas-id (org-entry-get pom "CANVAS_ID")
          :free-form-raw (org-entry-get pom "FREE_FORM_CRITERION_COMMENTS")
          :criteria table-data)))

(defun org-canvas--rubric-transform-props (raw)
  "Transform raw property strings RAW into typed rubric data.
Pure function — no buffer access."
  (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
        :canvas-id (plist-get raw :canvas-id)
        :free-form (org-canvas--interpret-boolean (plist-get raw :free-form-raw))
        :criteria (plist-get raw :criteria)))

(defun org-canvas--rubric-parse-entry ()
  "Extract rubric data and the associated table from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--rubric-read-props pom))
         (data (org-canvas--rubric-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Rubric")

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Rubric: '%s' (ID: %s)"
              (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: free-form=%s"
               (plist-get data :free-form))

    (unless (plist-get data :criteria)
      (elog-error org-canvas--logger "[Stage 1: Parse] No table found for rubric '%s'"
                  (plist-get data :title))
      (error "No table found for rubric '%s'" (plist-get data :title)))

    (let ((row-count (length (cl-remove-if (lambda (r) (eq r 'hline))
                                           (plist-get data :criteria)))))
      (elog-info org-canvas--logger "[Stage 1: Parse] Found %d criteria rows in table" row-count))

    (plist-put data :pom pom)
    data))

;;;; 2. Stage: Transformation

(defun org-canvas--rubric-rating-row-p (row)
  "Return non-nil if ROW is a rating row (first cell start with \"> \")."
  (and (listp row)
       (stringp (nth 0 row))
       (string-match-p "\\`> " (string-trim-left (nth 0 row)))))

(defun org-canvas--rubric-build-criterion (row counter &optional rating-rows outcome-id)
  "Build a hash-table for a single criterion from table ROW at COUNTER.
If RATING-ROWS is non-nil, build ratings from those rows instead of
the default 2-level (Full Marks / No Marks).
When OUTCOME-ID is non-nil, set learning_outcome_id on the criterion.
Returns a plist (:id KEY :obj HASH :points NUM)."
  (let* ((desc (nth 0 row))
         (points (org-canvas--safe-string-to-number (or (nth 1 row) "0") "POINTS"))
         (long-desc (or (nth 2 row) ""))
         (crit-obj (make-hash-table :test 'equal))
         (ratings (make-hash-table :test 'equal)))
    (elog-debug org-canvas--logger "[Stage 2: Transform] Criterion %d: '%s' (%d pts)" counter desc points)
    (puthash "description" desc crit-obj)
    (puthash "points" points crit-obj)
    (puthash "long_description" long-desc crit-obj)
    (when outcome-id
      (puthash "learning_outcome_id" outcome-id crit-obj))
    (if rating-rows
        ;; Custom multi-level ratings from > rows
        (let ((idx 0))
          (dolist (rrow rating-rows)
            (let ((r (make-hash-table :test 'equal))
                  (rdesc (replace-regexp-in-string "\\`>[ \t]*" "" (string-trim-left (nth 0 rrow))))
                  (rpts (org-canvas--safe-string-to-number (or (nth 1 rrow) "0") "RATING_POINTS")))
              (puthash "description" rdesc r)
              (puthash "points" rpts r)
              (puthash (format "%d" idx) r ratings)
              (setq idx (1+ idx)))))
      ;; Default 2-level ratings
      (let ((r1 (make-hash-table :test 'equal))
            (r2 (make-hash-table :test 'equal)))
        (puthash "description" "Full Marks" r1)
        (puthash "points" points r1)
        (puthash "description" "No Marks" r2)
        (puthash "points" 0 r2)
        (puthash "0" r1 ratings)
        (puthash "1" r2 ratings)))
    (puthash "ratings" ratings crit-obj)
    (list :id (format "%d" counter) :obj crit-obj :points points)))

(defun org-canvas--rubric-build-association ()
  "Build the rubric_association hash-table for a course-level rubric."
  (let ((ra (make-hash-table :test 'equal)))
    (puthash "association_type" "Course" ra)
    (puthash "association_id" org-canvas-course-id ra)
    ra))

(defun org-canvas--rubric-flush-pending-criterion (criterion ratings counter criteria-hash
                                                  &optional outcome-id)
  "Flush CRITERION with RATINGS at COUNTER into CRITERIA-HASH.
OUTCOME-ID, when non-nil, is passed through to the criterion builder.
Returns the points for this criterion."
  (let ((crit (org-canvas--rubric-build-criterion
               criterion counter (nreverse ratings) outcome-id)))
    (puthash (plist-get crit :id) (plist-get crit :obj) criteria-hash)
    (plist-get crit :points)))

(defun org-canvas--rubric-resolve-outcome-id (row)
  "Resolve outcome CANVAS_ID from the 4th column of criterion ROW.
Returns the outcome ID string, or nil."
  (let ((outcome-cell (nth 3 row)))
    (when (and outcome-cell
               (not (string-empty-p (string-trim outcome-cell)))
               (string-match "\\[\\[file:" outcome-cell))
      (org-canvas--resolve-link-property
       outcome-cell "CANVAS_ID" org-canvas-rubrics-file))))

(defun org-canvas--rubric-build-criteria (criteria-rows criteria-hash)
  "Process CRITERIA-ROWS into CRITERIA-HASH, grouping ratings with criteria.
Returns total points across all criteria."
  (let ((pending-criterion nil)
        (pending-ratings nil)
        (pending-outcome-id nil)
        (counter 0)
        (total-points 0))
    (dolist (row criteria-rows)
      (unless (eq row 'hline)
        (if (org-canvas--rubric-rating-row-p row)
            (push row pending-ratings)
          (when pending-criterion
            (setq total-points
                  (+ total-points
                     (org-canvas--rubric-flush-pending-criterion
                      pending-criterion pending-ratings counter criteria-hash
                      pending-outcome-id)))
            (setq counter (1+ counter)))
          (setq pending-criterion row)
          (setq pending-ratings nil)
          (setq pending-outcome-id (org-canvas--rubric-resolve-outcome-id row)))))
    (when pending-criterion
      (setq total-points
            (+ total-points
               (org-canvas--rubric-flush-pending-criterion
                pending-criterion pending-ratings counter criteria-hash
                pending-outcome-id)))
      (setq counter (1+ counter)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Built %d criteria, total points: %d"
      counter total-points)
    total-points))

(defun org-canvas--rubric-build-payload (data)
  "Convert DATA to Canvas rubric payload using Hash Tables."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)
    (let ((rubric-obj (make-hash-table :test 'equal))
          (criteria-hash (make-hash-table :test 'equal)))
      (puthash "title" title rubric-obj)
      (puthash "free_form_criterion_comments"
               (org-canvas--to-json-boolean (plist-get data :free-form)) rubric-obj)
      (org-canvas--rubric-build-criteria (plist-get data :criteria) criteria-hash)
      (puthash "criteria" criteria-hash rubric-obj)
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "rubric" rubric-obj payload)
        (puthash "rubric_association" (org-canvas--rubric-build-association) payload)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
        payload))))

;;;; 3. Stage: Execution

(defun org-canvas--rubric-delete-by-id (id)
  "Delete rubric with ID from Canvas."
  (elog-info org-canvas--logger "[Stage 3: Delete] Deleting rubric ID: %s" id)
  (let ((endpoint (org-canvas-api-course-endpoint "rubrics/%s" id)))
    (org-canvas-api-request 'DELETE endpoint)
    (elog-info org-canvas--logger "[Stage 3: Delete] Successfully deleted rubric ID: %s" id)))

(defun org-canvas--rubric-push-to-api (data payload)
  "Send PAYLOAD (using DATA title) to Canvas API.
Checks for existing rubric with same title and deletes
it before creating new one."
  (let* ((title (plist-get data :title))
         (endpoint (org-canvas-api-course-endpoint "rubrics")))

    (elog-info org-canvas--logger "[Stage 3: Execute] Starting push for rubric '%s'" title)

    ;; Check for existing rubric with same title
    (let ((existing (org-canvas--search-item "rubrics" title :params nil)))
      (when existing
        (let ((existing-id (alist-get 'id existing)))
          (elog-warning org-canvas--logger "[Stage 3: Conflict] Rubric '%s' already exists (ID: %s). Deleting..." title existing-id)
          (condition-case err
              (org-canvas--rubric-delete-by-id existing-id)
            (error
             (elog-error org-canvas--logger "[Stage 3: Conflict] Failed to delete existing rubric (may be in use): %s" (error-message-string err)))))))

    (elog-info org-canvas--logger "[Stage 3: Execute] POST Rubric '%s' to %s" title endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request 'POST endpoint :data payload)))
          (elog-info org-canvas--logger "[Stage 3: Execute] POST successful for '%s'" title)
          response)
      (error
       (elog-error org-canvas--logger "[Stage 3: Execute] POST failed: %s" (error-message-string err))
       (if (org-canvas--timeout-error-p err)
           (org-canvas--handle-timeout-recovery
            (lambda (t_) (org-canvas--search-item "rubrics" t_ :params nil)) title err)
         (signal (car err) (cdr err)))))))

;;;; 4. Stage: Finalization

(defun org-canvas--rubric-finalize (data response)
  "Update local Org file with CANVAS_ID using DATA and RESPONSE."
  (elog-debug org-canvas--logger "[Stage 4: Finalize] Processing response...")

  (let* ((rubric-data (or (alist-get 'rubric response) response))
         (id (alist-get 'id rubric-data))
         (pom (plist-get data :pom))
         (title (plist-get data :title)))

    (if id
        (progn
          (elog-info org-canvas--logger "[Stage 4: Finalize] Saving CANVAS_ID=%s for '%s'" id title)
          (org-canvas-org-save-sync-state pom id "CANVAS_ID")
          (elog-info org-canvas--logger "[Stage 4: Finalize] Complete for '%s'" title))
      (elog-warning org-canvas--logger "[Stage 4: Finalize] No ID in response for '%s'! Keys: %S" title (mapcar #'car response)))))

;;;; Main Sync Functions

;; Generate org-canvas-sync-rubrics using the pipeline macro
(org-canvas-define-sync rubrics
  :file org-canvas-rubrics-file
  :parse #'org-canvas--rubric-parse-entry
  :build #'org-canvas--rubric-build-payload
  :push #'org-canvas--rubric-push-to-api
  :finalize #'org-canvas--rubric-finalize
  :pull-item-fn #'org-canvas--rubric-pull-item)

;;;; Rubric Dissociation

(defun org-canvas--rubric-delete-association (assoc-id assignment-name)
  "Delete rubric association ASSOC-ID for ASSIGNMENT-NAME.
Returns t on success, nil on failure."
  (elog-info org-canvas--logger
             "Dissociating rubric from assignment '%s' (association: %s)"
             assignment-name assoc-id)
  (condition-case err
      (progn
        (org-canvas-api-request 'DELETE
          (org-canvas-api-course-endpoint "rubric_associations/%s" assoc-id))
        (elog-info org-canvas--logger "  -> Dissociated successfully")
        t)
    (error
     (elog-warning org-canvas--logger
                   "  -> Dissociation failed: %s" (error-message-string err))
     nil)))

(defun org-canvas--rubric-dissociate-all ()
  "Remove all rubric associations from assignments in the course.
Canvas returns 500 when deleting a rubric still associated with
an assignment, so this must run before rubric deletion."
  (elog-info org-canvas--logger "Dissociating rubrics from assignments...")
  (let* ((endpoint (org-canvas-api-course-endpoint "assignments"))
         (assignments (append (org-canvas-api-request 'GET endpoint :params org-canvas--api-max-per-page) nil))
         (dissociated 0))
    (dolist (assignment assignments)
      (let ((rubric-settings (alist-get 'rubric_settings assignment)))
        (when rubric-settings
          (let ((assoc-id (alist-get 'id rubric-settings))
                (title (alist-get 'name assignment)))
            (when (and assoc-id
                       (org-canvas--rubric-delete-association assoc-id title))
              (setq dissociated (1+ dissociated)))))))
    (elog-info org-canvas--logger "Dissociated %d rubric associations" dissociated)
    dissociated))

;;;; Rubric Diagnostics

(defun org-canvas--rubric-log-detail (rubric-id)
  "Fetch and log rubric detail with assessments for RUBRIC-ID."
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "rubrics/%s" rubric-id))
             (detail (org-canvas-api-request 'GET endpoint
                       :params '(("include[]" . "assessments")
                                 ("style" . "full")))))
        (elog-warning org-canvas--logger "  [Detail] Full rubric response keys: %S"
                      (mapcar #'car detail))
        (let ((assessments (alist-get 'assessments detail))
              (context-type (alist-get 'context_type detail))
              (context-id (alist-get 'context_id detail))
              (reusable (alist-get 'reusable detail))
              (read-only (alist-get 'read_only detail)))
          (elog-warning org-canvas--logger "  [Detail] context: %s (ID: %s)"
                        context-type context-id)
          (elog-warning org-canvas--logger "  [Detail] reusable: %S | read_only: %S"
                        reusable read-only)
          (if assessments
              (progn
                (elog-warning org-canvas--logger
                              "  [Detail] assessments: %d found (rubric has been used for grading)"
                              (length assessments))
                (dolist (assessment (append assessments nil))
                  (elog-warning org-canvas--logger
                                "    assessment: id=%s type=%s artifact=%s/%s score=%s"
                                (alist-get 'id assessment)
                                (alist-get 'assessment_type assessment)
                                (alist-get 'artifact_type assessment)
                                (alist-get 'artifact_id assessment)
                                (alist-get 'score assessment))))
            (elog-warning org-canvas--logger "  [Detail] assessments: none"))))
    (error
     (elog-warning org-canvas--logger
                   "  [Detail] Failed to fetch rubric detail: %s" (error-message-string err)))))

(defun org-canvas--rubric-find-linked-assignments (rubric-id)
  "Return list of assignments that reference RUBRIC-ID."
  (let* ((endpoint (org-canvas-api-course-endpoint "assignments"))
         (assignments (append (org-canvas-api-request 'GET endpoint
                                :params org-canvas--api-max-per-page) nil))
         (numeric-id (if (stringp rubric-id)
                         (string-to-number rubric-id)
                       rubric-id)))
    (cl-remove-if-not
     (lambda (a) (equal (alist-get 'rubric_id a) numeric-id))
     assignments)))

(defun org-canvas--rubric-log-linked-assignments (rubric-id)
  "Find and log assignments that reference RUBRIC-ID."
  (condition-case err
      (let ((linked (org-canvas--rubric-find-linked-assignments rubric-id)))
        (if linked
            (progn
              (elog-warning org-canvas--logger
                            "  [Assignments] %d assignment(s) reference this rubric:"
                            (length linked))
              (dolist (a linked)
                (elog-warning org-canvas--logger
                              "    assignment: id=%s name='%s' rubric_id=%s has_rubric_settings=%s"
                              (alist-get 'id a)
                              (alist-get 'name a)
                              (alist-get 'rubric_id a)
                              (if (alist-get 'rubric_settings a) "yes" "no"))))
          (elog-warning org-canvas--logger
                        "  [Assignments] No assignments reference this rubric")))
    (error
     (elog-warning org-canvas--logger
                   "  [Assignments] Failed to check assignments: %s" (error-message-string err)))))

(defun org-canvas--rubric-log-diagnostics (rubric-id list-data)
  "Fetch and log detailed info about rubric RUBRIC-ID after a failed deletion.
LIST-DATA is the rubric alist from the list response."
  (elog-warning org-canvas--logger "--- Diagnostic info for rubric %s ---" rubric-id)
  (dolist (pair list-data)
    (elog-warning org-canvas--logger "  %s: %S" (car pair) (cdr pair)))
  (org-canvas--rubric-log-detail rubric-id)
  (org-canvas--rubric-log-linked-assignments rubric-id)
  (elog-warning org-canvas--logger "--- End diagnostic info ---"))

;;;; Delete All Rubrics (custom, with dissociation and diagnostics)

(defun org-canvas--rubric-verify-deleted (rubric-id item)
  "Check whether RUBRIC-ID was actually deleted despite an API error.
ITEM is the rubric alist for diagnostics.
Returns t if confirmed deleted, nil if still present."
  (elog-info org-canvas--logger "  -> Verifying whether rubric was actually deleted...")
  (condition-case _verify-err
      (progn
        (org-canvas-api-request 'GET
          (org-canvas-api-course-endpoint "rubrics/%s" rubric-id))
        (elog-error org-canvas--logger "  -> Rubric still exists. Deletion truly failed.")
        (org-canvas--rubric-log-diagnostics rubric-id item)
        nil)
    (error
     (elog-info org-canvas--logger "  -> Rubric no longer exists (confirmed deleted)")
     t)))

(defun org-canvas--rubric-delete-with-verify (item)
  "Delete a single rubric ITEM, verifying on error.
Returns the string ID if deleted, nil otherwise."
  (let ((item-id (alist-get 'id item)))
    (condition-case err
        (progn
          (org-canvas-api-request 'DELETE
            (org-canvas-api-course-endpoint "rubrics/%s" item-id))
          (elog-info org-canvas--logger "  -> Deleted successfully")
          (org-canvas--normalize-id item-id))
      (error
       (elog-warning org-canvas--logger "  -> Delete returned error: %s" (error-message-string err))
       (when (org-canvas--rubric-verify-deleted item-id item)
         (org-canvas--normalize-id item-id))))))

;;;###autoload
(defun org-canvas-delete-all-rubrics ()
  "Delete ALL rubrics in the configured course.
First dissociates rubrics from assignments to avoid Canvas 500 errors.
On failure, fetches detailed rubric info for diagnostics."
  (interactive)
  (unless org-canvas--inhibit-log-clear
    (unless (y-or-n-p "Delete ALL rubrics in this course? ")
      (user-error "Aborted")))

  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (elog-warning org-canvas--logger "========================================")
  (elog-warning org-canvas--logger ">>> STARTING MASS DELETION OF RUBRICS")
  (elog-warning org-canvas--logger "========================================")

  ;; Step 1: Dissociate rubrics from assignments
  (org-canvas--rubric-dissociate-all)

  ;; Step 2: Fetch all rubrics and delete with diagnostics on failure
  (let* ((full-endpoint (org-canvas-api-course-endpoint "rubrics"))
         (remote-items (append (org-canvas-api-request 'GET full-endpoint :params org-canvas--api-max-per-page) nil))
         (deleted-count 0)
         (deleted-ids nil))

    (elog-info org-canvas--logger "Found %d rubrics on Canvas" (length remote-items))

    (dolist (item remote-items)
      (let* ((item-id (alist-get 'id item))
             (item-title (alist-get 'title item))
             (result (org-canvas--rubric-delete-with-verify item)))
        (elog-info org-canvas--logger "Deleting: '%s' (ID: %s)" item-title item-id)
        (when result
          (push result deleted-ids)
          (setq deleted-count (1+ deleted-count)))))

    ;; Cleanup local properties
    (org-canvas--clean-local-sync-properties org-canvas-rubrics-file)

    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d removed" deleted-count)
    (elog-info org-canvas--logger "========================================")
    (message "Rubrics deletion complete. %d removed." deleted-count)))

;;;; Pull

(defun org-canvas--rubric-has-custom-ratings (ratings)
  "Return non-nil if RATINGS list has custom levels (not default 2-level).
Custom means more than 2 ratings, or names other than Full Marks/No Marks."
  (when ratings
    (let ((rlist (append ratings nil)))
      (or (> (length rlist) 2)
          (not (and (= (length rlist) 2)
                    (member (alist-get 'description (nth 0 rlist))
                            '("Full Marks" "No Marks"))
                    (member (alist-get 'description (nth 1 rlist))
                            '("Full Marks" "No Marks"))))))))

(defun org-canvas--rubric-sort-ratings (ratings)
  "Sort RATINGS list by points descending."
  (sort (copy-sequence (append ratings nil))
        (lambda (a b)
          (> (or (alist-get 'points a) 0)
             (or (alist-get 'points b) 0)))))

(defun org-canvas--rubric-outcome-title (outcome-id)
  "Look up the title of an outcome with OUTCOME-ID in outcomes.org.
Returns the heading title string, or nil if not found."
  (when (and outcome-id
             (boundp 'org-canvas-outcomes-file)
             (file-exists-p org-canvas-outcomes-file))
    (with-current-buffer (find-file-noselect org-canvas-outcomes-file)
      (let ((id-str (if (numberp outcome-id)
                        (number-to-string outcome-id)
                      (format "%s" outcome-id))))
        (save-excursion
          (goto-char (point-min))
          (catch 'found
            (org-map-entries
             (lambda ()
               (when (equal (org-entry-get (point) "CANVAS_ID") id-str)
                 (throw 'found (org-get-heading t t t t))))
             "LEVEL=2" 'file)
            nil))))))

(defun org-canvas--rubric-pull-has-outcomes (criteria)
  "Return non-nil if any criterion in CRITERIA has a learning_outcome_id."
  (cl-some (lambda (c) (alist-get 'learning_outcome_id c))
           (append criteria nil)))

(defun org-canvas--rubric-pull-outcome-col (outcome-id)
  "Build the outcome column text for a pulled criterion.
Returns an Org file link when OUTCOME-ID resolves, the raw ID as fallback,
or empty string when OUTCOME-ID is nil."
  (if outcome-id
      (let ((title (org-canvas--rubric-outcome-title outcome-id)))
        (if title
            (format "[[file:outcomes.org::*%s][%s]]" title title)
          (format "%s" outcome-id)))
    ""))

(defun org-canvas--rubric-pull-insert-criterion (c has-outcomes)
  "Insert a single criterion C into the current buffer.
HAS-OUTCOMES controls whether a 4th outcome column is emitted."
  (let ((desc (replace-regexp-in-string
               "|" "/"
               (org-canvas--html-to-org-inline (or (alist-get 'description c) ""))))
        (pts (or (alist-get 'points c) 0))
        (long-desc (replace-regexp-in-string
                    "|" "/"
                    (org-canvas--html-to-org-inline (or (alist-get 'long_description c) ""))))
        (ratings (alist-get 'ratings c))
        (outcome-id (alist-get 'learning_outcome_id c)))
    (if has-outcomes
        (insert (format "| %s | %s | %s | %s |\n"
                        desc pts long-desc
                        (org-canvas--rubric-pull-outcome-col outcome-id)))
      (insert (format "| %s | %s | %s |\n" desc pts long-desc)))
    ;; Insert custom rating rows if present
    (when (org-canvas--rubric-has-custom-ratings ratings)
      (dolist (r (org-canvas--rubric-sort-ratings ratings))
        (let ((rdesc (replace-regexp-in-string
                      "|" "/"
                      (org-canvas--html-to-org-inline (or (alist-get 'description r) ""))))
              (rpts (or (alist-get 'points r) 0)))
          (if has-outcomes
              (insert (format "| > %s | %s | | |\n" rdesc rpts))
            (insert (format "| > %s | %s | |\n" rdesc rpts))))))))

(defun org-canvas--rubric-pull-item (item _pos)
  "Set per-item properties for a pulled rubric.
ITEM is the API response alist, POS is the heading position."
  (let ((criteria (alist-get 'data item)))
    (when criteria
      (let ((body-start (save-excursion
                          (org-end-of-meta-data t) (point)))
            (body-end (save-excursion
                        (org-end-of-subtree t) (point)))
            (has-outcomes (org-canvas--rubric-pull-has-outcomes criteria)))
        (delete-region body-start body-end)
        (goto-char body-start)
        (if has-outcomes
            (progn
              (insert "\n| Criterion | Points | Description | Outcome |\n")
              (insert "|---|---|---|---|\n"))
          (insert "\n| Criterion | Points | Description |\n")
          (insert "|---|---|---|\n"))
        (dolist (c (append criteria nil))
          (org-canvas--rubric-pull-insert-criterion c has-outcomes))
        (insert "\n")))))

(org-canvas-define-pull rubrics
  :file org-canvas-rubrics-file
  :endpoint "rubrics"
  :item-fn #'org-canvas--rubric-pull-item)

(org-canvas-define-delete-at-point rubric
  :endpoint "rubrics/%s")

(provide 'org-canvas-rubrics)
;;; org-canvas-rubrics.el ends here
