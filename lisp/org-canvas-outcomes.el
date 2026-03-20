;;; org-canvas-outcomes.el --- Pipeline-based Outcome Sync -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the sync pipeline for Canvas Learning Outcomes.
;;
;; FILE STRUCTURE
;; ==============
;; In outcomes.org:
;;   - Level 1 headings = Outcome Groups (organizational containers)
;;   - Level 2 headings = Outcomes (measurable learning objectives)
;;   - List items under outcomes = Rating levels
;;
;; RATING FORMAT
;; =============
;; Ratings are parsed from list items:
;;
;;   - [5] Exceeds Expectations
;;   - [3] Meets Expectations
;;   - [1] Approaching
;;   - [0] Does Not Meet
;;
;; The number in brackets is the point value for that rating level.
;;
;; CALCULATION METHODS
;; ===================
;; Use CALCULATION_METHOD property:
;;   highest          - Use highest score
;;   latest           - Use most recent score
;;   decaying_average - Weighted average (use CALCULATION_INT for weight)
;;   n_mastery        - Score when N items reach mastery (CALCULATION_INT = N)
;;
;; MASTERY
;; =======
;; MASTERY_POINTS sets the threshold for "mastery" status.
;;
;; API NOTES
;; =========
;; Outcomes have a two-level hierarchy in Canvas.  Groups are synced first,
;; then outcomes are created within their parent groups.

;;; Code:

(require 'org-canvas-core)
(require 'elog)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-outcomes-file (org-canvas--path "outcomes.org")
  "Path to the outcomes.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-outcomes-file "outcomes.org")
(org-canvas-register-feature
 :name "Outcomes" :endpoint "outcome_groups"
 :file-var 'org-canvas-outcomes-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title)

;;;; Helper Functions

(defun org-canvas--outcome-get-root-group-id ()
  "Get the root outcome group ID for the course.
Canvas creates a root group automatically for each course."
  (let* ((endpoint (org-canvas-api-course-endpoint "root_outcome_group"))
         (response (org-canvas-api-request 'GET endpoint)))
    (alist-get 'id response)))

(defun org-canvas--outcome-parse-ratings ()
  "Parse ratings from list items under the current heading.
Expects format: - [N] Description (e.g., \"- [5] Exceeds Expectations\").
Returns a list of alists, each with \\='points (integer) and
\\='description (string) keys, in document order."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let ((ratings nil))
        (while (re-search-forward "^[ \t]*-[ \t]+\\[\\([0-9]+\\)\\][ \t]+\\(.+\\)$" nil t)
          (let ((points (string-to-number (match-string 1)))
                (description (string-trim (match-string 2))))
            (push `((points . ,points)
                    (description . ,description))
                  ratings)))
        (nreverse ratings)))))

(defun org-canvas--outcome-get-description ()
  "Get description text from under the current heading.
Collects body text between the property drawer and the first
rating list item (- [N] ...).  Skips the :PROPERTIES: drawer."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      ;; Skip heading line and property drawer
      (org-end-of-meta-data t)
      ;; Collect non-list text until we hit a list or end
      (let ((start (point))
            (end (point)))
        (while (and (not (eobp))
                    (not (looking-at "^[ \t]*-[ \t]+\\[")))
          (forward-line 1)
          (setq end (point)))
        (string-trim (buffer-substring-no-properties start end))))))

;;;; 1. Stage: Extraction

(defun org-canvas--outcome-group-read-props ()
  "Read raw outcome group properties from the Org heading at point.
Returns a plist with :title-raw and :canvas-id."
  (let ((pom (point)))
    (list :title-raw (org-get-heading t t t t)
          :canvas-id (org-entry-get pom "CANVAS_ID"))))

(defun org-canvas--outcome-group-transform-props (raw)
  "Transform RAW outcome group properties into final form.
Strips statistics cookies from :title-raw to produce :title."
  (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
        :canvas-id (plist-get raw :canvas-id)))

(defun org-canvas--outcome-group-parse-entry ()
  "Extract outcome group data from a level-1 Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Parsing outcome group at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--outcome-group-read-props))
         (data (org-canvas--outcome-group-transform-props raw))
         (title (plist-get data :title)))

    (org-canvas--require-title title pom "Outcome group")

    (elog-info org-canvas--logger "[Stage 1: Parse] Outcome Group: '%s' (ID: %s)"
      title (or (plist-get data :canvas-id) "NEW"))

    (list :title title
          :canvas-id (plist-get data :canvas-id)
          :pom pom)))

(defun org-canvas--outcome-read-props ()
  "Read raw outcome properties from the Org heading at point.
Returns a plist with raw string values from the buffer, including
:title-raw, :canvas-id, :calculation-method, :calculation-int-raw,
:mastery-points-raw, :description, :ratings, and :parent-group-id-raw."
  (let ((pom (point)))
    (list :title-raw (org-get-heading t t t t)
          :canvas-id (org-entry-get pom "CANVAS_ID")
          :calculation-method (or (org-entry-get pom "CALCULATION_METHOD") "highest")
          :calculation-int-raw (org-entry-get pom "CALCULATION_INT")
          :mastery-points-raw (org-entry-get pom "MASTERY_POINTS")
          :description (org-canvas--outcome-get-description)
          :ratings (org-canvas--outcome-parse-ratings)
          :parent-group-id-raw (save-excursion
                                 (org-up-heading-safe)
                                 (org-entry-get (point) "CANVAS_ID")))))

(defun org-canvas--outcome-transform-props (raw)
  "Transform RAW outcome properties into final form.
Strips statistics cookies from title, converts numeric strings
via `org-canvas--safe-string-to-number', and converts parent group
ID via `string-to-number'.  Pass-through keys: :canvas-id,
:calculation-method (as :calculation_method), :description, :ratings."
  (let ((calc-int-raw (plist-get raw :calculation-int-raw))
        (mastery-raw (plist-get raw :mastery-points-raw))
        (parent-raw (plist-get raw :parent-group-id-raw)))
    (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
          :description (plist-get raw :description)
          :canvas-id (plist-get raw :canvas-id)
          :calculation_method (plist-get raw :calculation-method)
          :calculation_int (when calc-int-raw
                             (org-canvas--safe-string-to-number calc-int-raw "CALCULATION_INT"))
          :mastery_points (when mastery-raw
                            (org-canvas--safe-string-to-number mastery-raw "MASTERY_POINTS"))
          :ratings (plist-get raw :ratings)
          :parent-group-id (when parent-raw (string-to-number parent-raw)))))

(defun org-canvas--outcome-parse-entry ()
  "Extract outcome data from a level-2 Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Parsing outcome at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--outcome-read-props))
         (data (org-canvas--outcome-transform-props raw))
         (title (plist-get data :title)))

    (org-canvas--require-title title pom "Outcome")

    (elog-info org-canvas--logger "[Stage 1: Parse] Outcome: '%s' (ID: %s)"
      title (or (plist-get data :canvas-id) "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Calculation: %s, Mastery: %s pts"
      (plist-get data :calculation_method) (or (plist-get data :mastery_points) "auto"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Found %d ratings"
      (length (plist-get data :ratings)))

    (list :title title
          :description (plist-get data :description)
          :canvas-id (plist-get data :canvas-id)
          :calculation_method (plist-get data :calculation_method)
          :calculation_int (plist-get data :calculation_int)
          :mastery_points (plist-get data :mastery_points)
          :ratings (plist-get data :ratings)
          :parent-group-id (plist-get data :parent-group-id)
          :pom pom)))

;;;; 2. Stage: Transformation

(defun org-canvas--outcome-group-build-payload (data)
  "Convert outcome group DATA to Canvas payload."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for group '%s'" title)
    `((title . ,title))))

(defun org-canvas--outcome-build-payload (data)
  "Convert outcome DATA to Canvas payload."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for outcome '%s'" title)

    (let ((payload `((title . ,title)
                     (description . ,(or (plist-get data :description) ""))
                     (calculation_method . ,(plist-get data :calculation_method)))))

      ;; Add calculation_int if method requires it
      (when (and (plist-get data :calculation_int)
                 (member (plist-get data :calculation_method)
                         '("decaying_average" "n_mastery" "highest" "latest")))
        (push `(calculation_int . ,(plist-get data :calculation_int)) payload))

      ;; Add mastery_points
      (when (plist-get data :mastery_points)
        (push `(mastery_points . ,(plist-get data :mastery_points)) payload))

      ;; Add ratings if present
      (when (plist-get data :ratings)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Adding %d ratings"
          (length (plist-get data :ratings)))
        (push `(ratings . ,(plist-get data :ratings)) payload))

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      payload)))

;;;; 3. Stage: Execution

(defun org-canvas--outcome-group-search-by-title (title parent-id)
  "Search for an outcome group with TITLE under PARENT-ID."
  (elog-debug org-canvas--logger "[Stage 3: Search] Looking for group '%s'" title)
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "outcome_groups/%s/subgroups" parent-id))
             (results (append (org-canvas-api-request 'GET endpoint) nil)))
        (cl-loop for item in results
                 when (string-equal (alist-get 'title item) title)
                 return item))
    (error
     (elog-warning org-canvas--logger "[Stage 3: Search] Failed: %s" (error-message-string err))
     nil)))

(defun org-canvas--outcome-group-push-to-api (data root-group-id)
  "Send outcome group DATA to Canvas API under ROOT-GROUP-ID."
  (let* ((id (plist-get data :canvas-id))
         (title (plist-get data :title))
         (payload (org-canvas--outcome-group-build-payload data)))

    (if id
        ;; Update existing group
        (let ((endpoint (org-canvas-api-course-endpoint "outcome_groups/%s" id)))
          (elog-info org-canvas--logger "[Stage 3: Execute] PUT Group '%s'" title)
          (condition-case err
              (org-canvas-api-request 'PUT endpoint :data payload)
            (error
             ;; 404 -> Create new
             (if (org-canvas--404-error-p err)
                 (progn
                   (elog-warning org-canvas--logger "[Stage 3: Recovery] Group not found, creating...")
                   (org-canvas--outcome-group-create data root-group-id))
               (signal (car err) (cdr err))))))
      ;; Create new group
      (org-canvas--outcome-group-create data root-group-id))))

(defun org-canvas--outcome-group-create (data parent-id)
  "Create a new outcome group from DATA under PARENT-ID."
  (let* ((title (plist-get data :title))
         (payload (org-canvas--outcome-group-build-payload data))
         (endpoint (org-canvas-api-course-endpoint "outcome_groups/%s/subgroups" parent-id)))

    (elog-info org-canvas--logger "[Stage 3: Execute] POST Group '%s' under %s" title parent-id)

    (condition-case err
        (let ((response (org-canvas-api-request 'POST endpoint :data payload)))
          (elog-info org-canvas--logger "[Stage 3: Execute] Group created successfully")
          response)
      (error
       (elog-error org-canvas--logger "[Stage 3: Execute] Failed: %s" (error-message-string err))
       ;; Try to find if it was created despite error
       (or (org-canvas--outcome-group-search-by-title title parent-id)
           (signal (car err) (cdr err)))))))

(defun org-canvas--outcome-search-by-title (title group-id)
  "Search for an outcome with TITLE in GROUP-ID."
  (elog-debug org-canvas--logger "[Stage 3: Search] Looking for outcome '%s' in group %s" title group-id)
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "outcome_groups/%s/outcomes" group-id))
             (params '(("outcome_style" . "full")))
             (results (append (org-canvas-api-request 'GET endpoint :params params) nil)))
        (cl-loop for link in results
                 for outcome = (alist-get 'outcome link)
                 when (and outcome (string-equal (alist-get 'title outcome) title))
                 return outcome))
    (error
     (elog-warning org-canvas--logger "[Stage 3: Search] Failed: %s" (error-message-string err))
     nil)))

(defun org-canvas--outcome-push-to-api (data)
  "Send outcome DATA to Canvas API."
  (let* ((id (plist-get data :canvas-id))
         (title (plist-get data :title))
         (parent-group-id (plist-get data :parent-group-id))
         (payload (org-canvas--outcome-build-payload data)))

    (unless parent-group-id
      (error "Outcome '%s' has no parent group.  Sync the group first" title))

    (if id
        ;; Update existing outcome
        (let ((endpoint (format "%s/api/v1/outcomes/%s" org-canvas-base-url id)))
          (elog-info org-canvas--logger "[Stage 3: Execute] PUT Outcome '%s'" title)
          (condition-case err
              (org-canvas-api-request 'PUT endpoint :data payload)
            (error
             (if (org-canvas--404-error-p err)
                 (progn
                   (elog-warning org-canvas--logger "[Stage 3: Recovery] Outcome not found, creating...")
                   (org-canvas--outcome-create data parent-group-id))
               (signal (car err) (cdr err))))))
      ;; Create new outcome
      (org-canvas--outcome-create data parent-group-id))))

(defun org-canvas--outcome-create (data group-id)
  "Create a new outcome from DATA in GROUP-ID."
  (let* ((title (plist-get data :title))
         (payload (org-canvas--outcome-build-payload data))
         (endpoint (org-canvas-api-course-endpoint "outcome_groups/%s/outcomes" group-id)))

    (elog-info org-canvas--logger "[Stage 3: Execute] POST Outcome '%s' to group %s" title group-id)

    (condition-case err
        (let ((response (org-canvas-api-request 'POST endpoint :data payload)))
          (elog-info org-canvas--logger "[Stage 3: Execute] Outcome created successfully")
          ;; Response contains outcome_link, extract the outcome
          (or (alist-get 'outcome response) response))
      (error
       (elog-error org-canvas--logger "[Stage 3: Execute] Failed: %s" (error-message-string err))
       (or (org-canvas--outcome-search-by-title title group-id)
           (signal (car err) (cdr err)))))))

;;;; 4. Stage: Finalization

(defun org-canvas--outcome-group-finalize (data response)
  "Update local Org file with group CANVAS_ID using DATA and RESPONSE."
  (org-canvas--finalize-item data response))

(defun org-canvas--outcome-finalize (data response)
  "Update local Org file with outcome CANVAS_ID using DATA and RESPONSE."
  (org-canvas--finalize-item data response))

;;;; Main Sync Function

(defun org-canvas--outcome-sync-preflight ()
  "Validate outcomes file and fetch root outcome group.
Returns the root group ID, or signals an error."
  (unless (and org-canvas-outcomes-file (file-exists-p org-canvas-outcomes-file))
    (error "Outcomes file not found: %s" org-canvas-outcomes-file))
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> STARTING OUTCOME SYNC")
  (elog-info org-canvas--logger "File: %s" org-canvas-outcomes-file)
  (elog-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger "[Pre-flight] Fetching root outcome group...")
  (let ((root-group-id (org-canvas--outcome-get-root-group-id)))
    (unless root-group-id
      (error "Could not get root outcome group for course"))
    (elog-info org-canvas--logger "[Pre-flight] Root group ID: %s" root-group-id)
    root-group-id))

;;;###autoload
(defun org-canvas-sync-outcomes ()
  "Synchronize all outcomes to Canvas.
First syncs outcome groups (level-1 headings), then outcomes (level-2 headings)."
  (interactive)
  (org-canvas-clear-log)
  (let ((org-canvas--inhibit-log-clear t)
        (root-group-id (org-canvas--outcome-sync-preflight)))
    ;; Phase 1: Sync outcome groups (level-1 headings)
    (org-canvas--sync-run-pipeline
     "outcome-groups" (expand-file-name org-canvas-outcomes-file) "LEVEL=1"
     #'org-canvas--outcome-group-parse-entry
     #'org-canvas--outcome-group-build-payload
     (lambda (data _payload)
       (org-canvas--outcome-group-push-to-api data root-group-id))
     #'org-canvas--outcome-group-finalize)
    ;; Phase 2: Sync outcomes (level-2 headings)
    (org-canvas--sync-run-pipeline
     "outcomes" (expand-file-name org-canvas-outcomes-file) "LEVEL=2"
     #'org-canvas--outcome-parse-entry
     #'org-canvas--outcome-build-payload
     (lambda (data _payload)
       (org-canvas--outcome-push-to-api data))
     #'org-canvas--outcome-finalize)))

;;;; Delete Functions

;;;###autoload
(defun org-canvas-delete-all-outcomes ()
  "Delete ALL outcomes and outcome groups in the configured course.
Warning: This will remove all learning outcomes from the course."
  (interactive)
  (unless org-canvas--inhibit-log-clear
    (unless (y-or-n-p "Delete ALL outcomes and outcome groups in this course? ")
      (user-error "Aborted")))

  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (elog-warning org-canvas--logger "========================================")
  (elog-warning org-canvas--logger ">>> STARTING MASS DELETION OF OUTCOMES")
  (elog-warning org-canvas--logger "========================================")

  ;; Get root group
  (let* ((root-group-id (org-canvas--outcome-get-root-group-id))
         (endpoint (org-canvas-api-course-endpoint "outcome_groups/%s/subgroups" root-group-id))
         (subgroups (append (org-canvas-api-request 'GET endpoint) nil))
         (result (progn
                   (elog-info org-canvas--logger "Found %d outcome groups (excluding root)" (length subgroups))
                   (org-canvas--delete-items-queued
                    subgroups
                    (lambda (item-id)
                      (org-canvas-api-course-endpoint "outcome_groups/%s" item-id))
                    'id 'title)))
         (deleted-groups (car result)))

    ;; Cleanup local properties
    (org-canvas--clean-local-sync-properties org-canvas-outcomes-file)

    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d groups removed" deleted-groups)
    (elog-info org-canvas--logger "========================================")
    (message "Outcome deletion complete. %d groups removed." deleted-groups)))

;;;; Pull

(defun org-canvas--outcome-pull-find-or-create-l2 (oid otitle)
  "Find or create an L2 heading with CANVAS_ID OID under current heading.
OTITLE is used when creating a new heading.  Point must be at the
parent L1 heading.  Returns the position of the L2 heading."
  (let ((opos nil))
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get (point) "CANVAS_ID")
                      (format "%s" oid))
           (setq opos (point))))
       "LEVEL=2" 'tree)
      (widen))
    (unless opos
      (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
        (goto-char subtree-end)
        (unless (bolp) (insert "\n"))
        (insert (format "** %s\n" otitle))
        (org-back-to-heading t)
        (setq opos (point))))
    opos))

(defun org-canvas--outcome-pull-insert-ratings (desc ratings)
  "Insert RATINGS list after the body, if DESC is non-empty.
RATINGS is a vector/list of alists with \\='description and \\='points keys."
  (when (and desc (not (string-empty-p desc)) ratings)
    (insert "\nRatings:\n")
    (dolist (r (append ratings nil))
      (insert (format "- %s (%s pts)\n"
                      (or (alist-get 'description r) "")
                      (or (alist-get 'points r) 0))))))

(defun org-canvas--outcome-pull-process-group (gid)
  "Fetch and insert outcomes for group GID as L2 headings.
Point must be at the parent group heading."
  (condition-case nil
      (let* ((outcomes-url (org-canvas-api-course-endpoint
                            "outcome_groups/%s/outcomes" gid))
             (outcomes (org-canvas-api-request-all-pages 'GET outcomes-url))
             (count 0))
        (dolist (link outcomes)
          (let* ((outcome (alist-get 'outcome link))
                 (oid (alist-get 'id outcome))
                 (otitle (or (alist-get 'title outcome) "Untitled"))
                 (desc (alist-get 'description outcome))
                 (ratings (alist-get 'ratings outcome))
                 (opos (org-canvas--outcome-pull-find-or-create-l2 oid otitle)))
            (goto-char opos)
            (when otitle (org-edit-headline otitle))
            (org-canvas-org-save-sync-state opos oid)
            (org-canvas--pull-insert-body desc)
            (org-canvas--outcome-pull-insert-ratings desc ratings)
            (cl-incf count)))
        count)
    (error 0)))

;;;###autoload
(defun org-canvas-pull-outcomes ()
  "Pull outcome groups and outcomes from Canvas into outcomes.org."
  (interactive)
  (org-canvas--start-operation "PULLING OUTCOMES")
  (let* ((file (expand-file-name org-canvas-outcomes-file))
         (groups-endpoint (org-canvas-api-course-endpoint "outcome_groups"))
         (remote-groups (org-canvas-api-request-all-pages 'GET groups-endpoint))
         (total-groups (length remote-groups))
         (group-count 0) (outcome-count 0))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (find-file-noselect file)
      (dolist (group remote-groups)
        (cl-incf group-count)
        (let* ((gid (alist-get 'id group))
               (gtitle (or (alist-get 'title group) "Untitled Group"))
               (pos (org-canvas--pull-upsert-heading file gid gtitle)))
          (message "Outcomes [%d/%d] Pulling group '%s'..." group-count total-groups gtitle)
          (goto-char pos)
          (when gtitle (org-edit-headline gtitle))
          (org-canvas-org-save-sync-state pos gid)
          (cl-incf outcome-count
                   (org-canvas--outcome-pull-process-group gid))))
      (save-buffer))
    (elog-info org-canvas--logger
      "Outcomes pull complete: %d groups, %d outcomes" group-count outcome-count)
    (message "Outcomes pull complete: %d groups, %d outcomes."
             group-count outcome-count)))

(provide 'org-canvas-outcomes)
;;; org-canvas-outcomes.el ends here
