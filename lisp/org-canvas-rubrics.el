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
;; | Criterion Description | Points | Long Description (optional) |
;; |-----------------------+--------+-----------------------------|
;; | Code Quality          |     10 | Well-formatted, readable    |
;; | Correctness           |     15 | Passes all test cases       |
;;
;; Column 1: Short criterion name (shown in rubric)
;; Column 2: Maximum points for this criterion
;; Column 3: Optional longer description
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
(require 'elog)

;;;; Configuration

(defcustom org-canvas-rubrics-file (org-canvas--path "rubrics.org")
  "Path to the rubrics.org file."
  :type 'file
  :group 'org-canvas)

;;;; 1. Stage: Extraction

(defun org-canvas--rubric-parse-entry ()
  "Extract rubric data and the associated table from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (free-form (org-canvas-org-get-boolean-property pom "FREE_FORM_CRITERION_COMMENTS"))
         (table-data
          (save-excursion
            (save-restriction
              (org-narrow-to-subtree)
              (goto-char (point-min))
              (when (re-search-forward org-table-line-regexp nil t)
                (beginning-of-line)
                (org-table-to-lisp))))))

    (when (or (null title) (string-empty-p title))
      (error "Rubric title cannot be empty at point %d" pom))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Rubric: '%s' (ID: %s)" title (or canvas-id "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: free-form=%s" free-form)

    (unless table-data
      (elog-error org-canvas--logger "[Stage 1: Parse] No table found for rubric '%s'" title)
      (error "No table found for rubric '%s'" title))

    (let ((row-count (length (cl-remove-if (lambda (r) (eq r 'hline)) table-data))))
      (elog-info org-canvas--logger "[Stage 1: Parse] Found %d criteria rows in table" row-count))

    (list :title title
          :canvas-id canvas-id
          :free-form free-form
          :criteria table-data
          :pom pom)))

;;;; 2. Stage: Transformation

(defun org-canvas--rubric-build-payload (data)
  "Convert DATA to Canvas rubric payload using Hash Tables."
  (let ((title (plist-get data :title))
        (criteria-rows (plist-get data :criteria))
        (free-form (plist-get data :free-form)))

    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((rubric-obj (make-hash-table :test 'equal))
          (criteria-hash (make-hash-table :test 'equal))
          (counter 0)
          (total-points 0))

      (puthash "title" title rubric-obj)
      (puthash "free_form_criterion_comments" (org-canvas--to-json-boolean free-form) rubric-obj)

      ;; Process table rows (skip header and hlines)
      (dolist (row criteria-rows)
        (unless (eq row 'hline)
          (let* ((desc (nth 0 row))
                 (points (string-to-number (or (nth 1 row) "0")))
                 (long-desc (or (nth 2 row) ""))
                 (crit-id (format "%d" counter))
                 (crit-obj (make-hash-table :test 'equal)))

            (elog-debug org-canvas--logger "[Stage 2: Transform] Criterion %d: '%s' (%d pts)" counter desc points)

            (puthash "description" desc crit-obj)
            (puthash "points" points crit-obj)
            (puthash "long_description" long-desc crit-obj)

            ;; Add ratings (Full Marks vs No Marks)
            (let ((ratings (make-hash-table :test 'equal)))
              (let ((r1 (make-hash-table :test 'equal))
                    (r2 (make-hash-table :test 'equal)))
                (puthash "description" "Full Marks" r1)
                (puthash "points" points r1)
                (puthash "description" "No Marks" r2)
                (puthash "points" 0 r2)
                (puthash "0" r1 ratings)
                (puthash "1" r2 ratings))
              (puthash "ratings" ratings crit-obj))

            (puthash crit-id crit-obj criteria-hash)
            (setq total-points (+ total-points points))
            (setq counter (1+ counter)))))

      (puthash "criteria" criteria-hash rubric-obj)

      (elog-info org-canvas--logger "[Stage 2: Transform] Built %d criteria, total points: %d" counter total-points)

      ;; Wrapper
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "rubric" rubric-obj payload)
        (puthash "rubric_association"
                 (let ((ra (make-hash-table :test 'equal)))
                   (puthash "association_type" "Course" ra)
                   (puthash "association_id" org-canvas-course-id ra)
                   ra)
                 payload)

        (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
        payload))))

;;;; 3. Stage: Execution

(defun org-canvas--rubric-search-by-title (title)
  "Search for a rubric with TITLE on Canvas.  Return nil on error."
  ;; Rubrics API doesn't support search_term, so we fetch all and filter
  (org-canvas--search-item "rubrics" title :params nil))

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
    (let ((existing (org-canvas--rubric-search-by-title title)))
      (when existing
        (let ((existing-id (alist-get 'id existing)))
          (elog-warning org-canvas--logger "[Stage 3: Conflict] Rubric '%s' already exists (ID: %s). Deleting..." title existing-id)
          (condition-case err
              (org-canvas--rubric-delete-by-id existing-id)
            (error
             (elog-error org-canvas--logger "[Stage 3: Conflict] Failed to delete existing rubric (may be in use): %s" (cadr err)))))))

    (elog-info org-canvas--logger "[Stage 3: Execute] POST Rubric '%s' to %s" title endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request 'POST endpoint :data payload)))
          (elog-info org-canvas--logger "[Stage 3: Execute] POST successful for '%s'" title)
          response)
      (error
       (elog-error org-canvas--logger "[Stage 3: Execute] POST failed: %s" (cadr err))
       (if (org-canvas--timeout-error-p err)
           (org-canvas--handle-timeout-recovery
            #'org-canvas--rubric-search-by-title title err)
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
  :finalize #'org-canvas--rubric-finalize)

(org-canvas-define-push-at-point rubric
  :parse #'org-canvas--rubric-parse-entry
  :build #'org-canvas--rubric-build-payload
  :push #'org-canvas--rubric-push-to-api
  :finalize #'org-canvas--rubric-finalize)

;;;; Rubric Dissociation

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
            (when assoc-id
              (elog-info org-canvas--logger
                         "Dissociating rubric from assignment '%s' (association: %s)"
                         title assoc-id)
              (condition-case err
                  (progn
                    (org-canvas-api-request 'DELETE
                      (org-canvas-api-course-endpoint "rubric_associations/%s" assoc-id))
                    (setq dissociated (1+ dissociated))
                    (elog-info org-canvas--logger "  -> Dissociated successfully"))
                (error
                 (elog-warning org-canvas--logger
                              "  -> Dissociation failed: %s" (cadr err)))))))))
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
                   "  [Detail] Failed to fetch rubric detail: %s" (cadr err)))))

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
                   "  [Assignments] Failed to check assignments: %s" (cadr err)))))

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

(defun org-canvas--rubric-delete-with-verify (item)
  "Delete a single rubric ITEM, verifying on error.
Returns the string ID if deleted, nil otherwise."
  (let ((item-id (alist-get 'id item)))
    (condition-case err
        (progn
          (org-canvas-api-request 'DELETE
            (org-canvas-api-course-endpoint "rubrics/%s" item-id))
          (elog-info org-canvas--logger "  -> Deleted successfully")
          (if (numberp item-id) (number-to-string item-id) item-id))
      (error
       ;; Canvas sometimes returns 500 but actually deletes the rubric.
       (elog-warning org-canvas--logger "  -> Delete returned error: %s" (cadr err))
       (elog-info org-canvas--logger "  -> Verifying whether rubric was actually deleted...")
       (condition-case _verify-err
           (progn
             (org-canvas-api-request 'GET
               (org-canvas-api-course-endpoint "rubrics/%s" item-id))
             (elog-error org-canvas--logger "  -> Rubric still exists. Deletion truly failed.")
             (org-canvas--rubric-log-diagnostics item-id item)
             nil)
         (error
          (elog-info org-canvas--logger "  -> Rubric no longer exists (confirmed deleted)")
          (if (numberp item-id) (number-to-string item-id) item-id)))))))

(defun org-canvas-delete-all-rubrics ()
  "Delete ALL rubrics in the configured course.
First dissociates rubrics from assignments to avoid Canvas 500 errors.
On failure, fetches detailed rubric info for diagnostics."
  (interactive)
  (unless org-canvas--inhibit-log-clear
    (unless (y-or-n-p "Delete ALL rubrics in this course? ")
      (user-error "Aborted")))

  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))
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
    (when (and org-canvas-rubrics-file
               (file-exists-p org-canvas-rubrics-file))
      (elog-info org-canvas--logger "Cleaning local properties...")
      (with-current-buffer (find-file-noselect org-canvas-rubrics-file)
        (org-map-entries
         (lambda ()
           (elog-debug org-canvas--logger "Removing properties for: %s"
                       (org-entry-get (point) "CANVAS_ID"))
           (org-canvas-clear-sync-properties (point)))
         "CANVAS_ID={.}" 'file)
        (save-buffer)
        (elog-info org-canvas--logger "Saved %s" org-canvas-rubrics-file)))

    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d removed" deleted-count)
    (elog-info org-canvas--logger "========================================")
    (message "Rubrics deletion complete. %d removed." deleted-count)))

;;;; Pull

(defun org-canvas--rubric-pull-item (item pos)
  "Set per-item properties for a pulled rubric.
ITEM is the API response alist, POS is the heading position."
  (let ((criteria (alist-get 'data item)))
    (when criteria
      (let ((body-start (save-excursion
                          (org-end-of-meta-data t) (point)))
            (body-end (save-excursion
                        (org-end-of-subtree t) (point))))
        (delete-region body-start body-end)
        (goto-char body-start)
        (insert "\n| Criterion | Points | Description |\n")
        (insert "|---|---|---|\n")
        (dolist (c (append criteria nil))
          (let ((desc (or (alist-get 'description c) ""))
                (pts (or (alist-get 'points c) 0))
                (long-desc (or (alist-get 'long_description c) "")))
            (insert (format "| %s | %s | %s |\n"
                            (replace-regexp-in-string "|" "/" desc)
                            pts
                            (replace-regexp-in-string "|" "/" long-desc)))))
        (insert "\n")))))

(org-canvas-define-pull rubrics
  :file org-canvas-rubrics-file
  :endpoint "rubrics"
  :item-fn #'org-canvas--rubric-pull-item)

(provide 'org-canvas-rubrics)
;;; org-canvas-rubrics.el ends here
