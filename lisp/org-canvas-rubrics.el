;;; org-canvas-rubrics.el --- Rubric Sync Pipeline for Canvas LMS -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas Rubrics.
;;
;; FILE STRUCTURE
;; ==============
;; In rubrics.org:
;;   - Level 1 headings = Rubrics
;;   - Level 2 headings = Criteria (one heading per criterion)
;;   - Sub-tables under each criterion = Ratings for that criterion
;;
;; CRITERION FORMAT
;; ================
;; ** Criterion Name :5pt:
;;    :PROPERTIES:
;;    :OUTCOME: [[file:outcomes.org::*Python Proficiency][Python Proficiency]]
;;    :END:
;;    Long description (optional body text before the ratings table).
;;    | Rating     | Points | Description |
;;    |------------+--------+-------------|
;;    | Full Marks |    5.0 |             |
;;    | Partial    |    3.0 |             |
;;    | No Marks   |    0.0 |             |
;;
;; The trailing :Npt: tag holds the criterion's max points (fractional points
;; encoded with `_` instead of `.`, e.g. :3_5pt: for 3.5 points -- Org tags
;; cannot contain dots).  The :OUTCOME: property is optional and links to
;; outcomes.org for mastery tracking.
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

(declare-function org-canvas--validate-rubric-structure "org-canvas-validate")

;;;; Configuration

(defcustom org-canvas-rubrics-file (org-canvas--path "rubrics.org")
  "Path to the rubrics.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-rubrics-file "rubrics.org")
(org-canvas-register-feature
 :name "Rubrics" :endpoint "rubrics"
 :file-var 'org-canvas-rubrics-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title)
(defun org-canvas--rubric-remote-free-form (item)
  "Return ITEM's `free_form_criterion_comments' flag.
The Org property is named after it; the payload key is the shorter
`:free-form', which is no Canvas field at all."
  (alist-get 'free_form_criterion_comments item))

(org-canvas-register-properties "rubrics"
  :label "Rubrics"
  :file-var 'org-canvas-rubrics-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "FREE_FORM_CRITERION_COMMENTS" :data-key :free-form :type boolean
     :remote-fn org-canvas--rubric-remote-free-form
     :doc "Allow freeform comments"))
  :structural-fn #'org-canvas--validate-rubric-structure)

;;;; 1. Stage: Extraction

(defun org-canvas--rubric-format-points (n)
  "Format N as a number string, dropping .0 for whole numbers.
Used to emit human-readable points in headings, tags, and tables."
  (cond
   ((null n) "0")
   ((not (numberp n)) (format "%s" n))
   ((= n (truncate n)) (format "%d" (truncate n)))
   (t (number-to-string n))))

(defun org-canvas--rubric-points-tag (points)
  "Encode POINTS as an Org tag string (e.g. \"5pt\" or \"3_5pt\").
Org tags cannot contain dots, so fractional points use `_' as a separator."
  (let ((formatted (org-canvas--rubric-format-points points)))
    (concat (replace-regexp-in-string "\\." "_" formatted) "pt")))

(defun org-canvas--rubric-decode-points-tag (tag)
  "Decode Org TAG of the form \"5pt\" or \"3_5pt\" back to a number.
Returns nil if TAG does not match the expected pattern."
  (when (and tag (string-match "\\`\\([0-9]+\\)\\(?:_\\([0-9]+\\)\\)?pt\\'" tag))
    (let ((whole (match-string 1 tag))
          (frac (match-string 2 tag)))
      (if frac
          (string-to-number (concat whole "." frac))
        (string-to-number whole)))))

(defun org-canvas--rubric-parse-rating-row ()
  "Parse the table row at point as (rating-desc rating-points rating-long-desc).
Returns nil for separator rows or empty rows."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (unless (string-match-p "\\`[ \t]*|[-+|]+\\(?:|.*\\)?\\'" line)
      (let ((cells (mapcar #'string-trim
                           (split-string line "|" t))))
        (when (and (>= (length cells) 2)
                   (not (string-empty-p (car cells))))
          (list (car cells)
                (string-to-number (or (nth 1 cells) "0"))
                (or (nth 2 cells) "")))))))

(defun org-canvas--rubric-parse-criterion-at-point ()
  "Parse the level-2 criterion heading at point + its sub-table.
Returns a plist (:description :points :long-description :outcome-link :ratings)
where :ratings is a list of (description points long-description) tuples."
  (org-back-to-heading t)
  (let* ((heading (org-get-heading t t t t))
         (tags (org-get-tags))
         (points-tag (cl-find-if
                      (lambda (tg)
                        (string-match-p "\\`[0-9]+\\(?:_[0-9]+\\)?pt\\'" tg))
                      tags))
         (points (or (org-canvas--rubric-decode-points-tag points-tag) 0))
         (outcome-prop (org-entry-get (point) "OUTCOME"))
         (subtree-end (save-excursion (org-end-of-subtree t t) (point)))
         (long-desc "")
         (ratings nil))
    ;; Body before the table (between meta-data and first table line) is the
    ;; long description.
    (save-excursion
      (org-end-of-meta-data t)
      (let ((body-start (point))
            (table-start (save-excursion
                           (when (re-search-forward "^[ \t]*|" subtree-end t)
                             (line-beginning-position)))))
        (when (and table-start (> table-start body-start))
          (setq long-desc (string-trim
                           (buffer-substring-no-properties body-start table-start))))))
    ;; Walk the ratings sub-table.
    (save-excursion
      (org-end-of-meta-data t)
      (when (re-search-forward "^[ \t]*|" subtree-end t)
        (beginning-of-line)
        ;; Skip header row (first non-separator).
        (forward-line 1)
        ;; Skip separator(s).
        (while (and (< (point) subtree-end)
                    (looking-at "^[ \t]*|[-+|]+[ \t]*$"))
          (forward-line 1))
        ;; Parse remaining rows until we leave the table or subtree.
        (while (and (< (point) subtree-end)
                    (looking-at "^[ \t]*|"))
          (let ((row (org-canvas--rubric-parse-rating-row)))
            (when row (push row ratings)))
          (forward-line 1))))
    (list :description heading
          :points points
          :long-description long-desc
          :outcome-link outcome-prop
          :ratings (nreverse ratings))))

(defun org-canvas--rubric-collect-criteria (pom)
  "Walk level-2 child headings under the rubric at POM.
Returns a list of criterion plists (one per child heading)."
  (let ((criteria nil)
        (subtree-end (save-excursion
                       (goto-char pom)
                       (org-end-of-subtree t t)
                       (point))))
    (save-excursion
      (goto-char pom)
      (forward-line 1)
      (while (re-search-forward "^\\*\\* " subtree-end t)
        (push (org-canvas--rubric-parse-criterion-at-point) criteria)
        (org-end-of-subtree t t)))
    (nreverse criteria)))

(defun org-canvas--rubric-read-props (pom)
  "Read raw property strings and child criterion headings from the rubric at POM."
  (list :title-raw (org-get-heading t t t t)
        :canvas-id (org-entry-get pom "CANVAS_ID")
        :free-form-raw (org-entry-get pom "FREE_FORM_CRITERION_COMMENTS")
        :criteria (org-canvas--rubric-collect-criteria pom)))

(defun org-canvas--rubric-transform-props (raw)
  "Transform raw property strings RAW into typed rubric data.
Pure function — no buffer access."
  (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
        :canvas-id (plist-get raw :canvas-id)
        :free-form (org-canvas--interpret-boolean (plist-get raw :free-form-raw))
        :criteria (plist-get raw :criteria)))

(defun org-canvas--rubric-parse-entry ()
  "Extract rubric data from the Org heading at point.
Walks level-2 child headings (one per criterion) and parses each."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--rubric-read-props pom))
         (data (org-canvas--rubric-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Rubric")

    (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Processing Rubric: '%s' (ID: %s)"
              (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Properties: free-form=%s"
               (plist-get data :free-form))

    (unless (plist-get data :criteria)
      (org-canvas--log-error org-canvas--logger "[Stage 1: Parse] No criteria found for rubric '%s'"
                  (plist-get data :title))
      (org-canvas--signal 'org-canvas-validation-error
        "No criteria found for rubric '%s' (expected level-2 child headings)"
        (plist-get data :title)))

    (org-canvas--log-info org-canvas--logger
      "[Stage 1: Parse] Found %d criteria"
      (length (plist-get data :criteria)))

    (plist-put data :pom pom)
    data))

;;;; 2. Stage: Transformation

(defun org-canvas--rubric-build-ratings-from-plist (rating-list)
  "Build a ratings hash-table from RATING-LIST.
RATING-LIST is a list of (description points long-description) tuples.
Returns the populated ratings hash-table."
  (let ((ratings (make-hash-table :test 'equal))
        (idx 0))
    (dolist (r rating-list)
      (let ((rh (make-hash-table :test 'equal))
            (rdesc (nth 0 r))
            (rpts (or (nth 1 r) 0))
            (rlong (or (nth 2 r) "")))
        (puthash "description" rdesc rh)
        (puthash "points" rpts rh)
        (unless (string-empty-p rlong)
          (puthash "long_description" rlong rh))
        (puthash (format "%d" idx) rh ratings)
        (setq idx (1+ idx))))
    ratings))

(defun org-canvas--rubric-build-default-ratings (points)
  "Build a default two-level ratings hash-table.
The two levels are \"Full Marks\" (worth POINTS) and \"No Marks\" (worth 0)."
  (let ((ratings (make-hash-table :test 'equal))
        (r1 (make-hash-table :test 'equal))
        (r2 (make-hash-table :test 'equal)))
    (puthash "description" "Full Marks" r1)
    (puthash "points" points r1)
    (puthash "description" "No Marks" r2)
    (puthash "points" 0 r2)
    (puthash "0" r1 ratings)
    (puthash "1" r2 ratings)
    ratings))

(defun org-canvas--rubric-build-criterion (criterion counter)
  "Build a hash-table for CRITERION (a plist) at COUNTER.
CRITERION is a plist with :description :points :long-description
:outcome-link :ratings.  Returns a plist (:id KEY :obj HASH :points NUM)."
  (let* ((desc (plist-get criterion :description))
         (points (or (plist-get criterion :points) 0))
         (long-desc (or (plist-get criterion :long-description) ""))
         (outcome-link (plist-get criterion :outcome-link))
         (rating-list (plist-get criterion :ratings))
         (outcome-id (when (and outcome-link
                                (stringp outcome-link)
                                (not (string-empty-p (string-trim outcome-link)))
                                (string-match-p "\\[\\[file:" outcome-link))
                       (org-canvas--resolve-link-property
                        outcome-link "CANVAS_ID" org-canvas-rubrics-file)))
         (crit-obj (make-hash-table :test 'equal))
         (ratings (if rating-list
                      (org-canvas--rubric-build-ratings-from-plist rating-list)
                    (org-canvas--rubric-build-default-ratings points))))
    (org-canvas--log-debug org-canvas--logger
      "[Stage 2: Transform] Criterion %d: '%s' (%s pts)" counter desc points)
    (puthash "description" desc crit-obj)
    (puthash "points" points crit-obj)
    (puthash "long_description" long-desc crit-obj)
    (when outcome-id
      (puthash "learning_outcome_id" outcome-id crit-obj))
    (puthash "ratings" ratings crit-obj)
    (list :id (format "%d" counter) :obj crit-obj :points points)))

(defun org-canvas--rubric-build-association ()
  "Build the rubric_association hash-table for a course-level rubric."
  (let ((ra (make-hash-table :test 'equal)))
    (puthash "association_type" "Course" ra)
    (puthash "association_id" org-canvas-course-id ra)
    ra))

(defun org-canvas--rubric-build-criteria (criteria-plists criteria-hash)
  "Process CRITERIA-PLISTS into CRITERIA-HASH.
Each plist describes one criterion (see `org-canvas--rubric-build-criterion').
Returns total points across all criteria."
  (let ((counter 0)
        (total-points 0))
    (dolist (cp criteria-plists)
      (let ((crit (org-canvas--rubric-build-criterion cp counter)))
        (puthash (plist-get crit :id) (plist-get crit :obj) criteria-hash)
        (setq total-points (+ total-points (or (plist-get crit :points) 0)))
        (setq counter (1+ counter))))
    (org-canvas--log-info org-canvas--logger
      "[Stage 2: Transform] Built %d criteria, total points: %s"
      counter total-points)
    total-points))

(defun org-canvas--rubric-build-payload (data)
  "Convert DATA to Canvas rubric payload using Hash Tables."
  (let ((title (plist-get data :title)))
    (org-canvas--log-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)
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
        (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
        payload))))

;;;; 3. Stage: Execution

(defun org-canvas--rubric-find-by-title (title)
  "Return the Canvas rubric titled TITLE, or nil."
  (org-canvas--search-item "rubrics" title))

(defun org-canvas--rubric-push-to-api (data payload)
  "Send PAYLOAD (using DATA title) to Canvas, updating in place when known.

A rubric used to be pushed by deleting whatever carried its title and
POSTing a fresh one.  That silently dropped every assignment
association — the associations belong to the old rubric id and go down
with it, taking the rubric assessments students had already been
graded with (issue #123).  A heading that carries a CANVAS_ID is now
PUT to `rubrics/ID', which keeps the id, its associations and its
assessments; only a heading without one creates, and a title Canvas
already holds is offered for adoption by the duplicate-title guard
rather than deleted (issue #85).  A stale id 404s and retries as a
POST, as everywhere else."
  (org-canvas--push-to-api data payload
    :endpoint "rubrics"
    :find-fn #'org-canvas--rubric-find-by-title))

;;;; 4. Stage: Finalization

(defun org-canvas--rubric-warn-recreated (data id)
  "Warn when Canvas answered a rubric update with a different id than ID.
DATA is the parsed rubric plist.  An update keeps the id; a new one
means the old rubric is gone, and the assignment associations that
pointed at it went with it, so the assignments linking this rubric
have to be pushed again to re-associate (issue #123)."
  (let ((previous (plist-get data :canvas-id)))
    (when (and previous id (not (equal (format "%s" id) (format "%s" previous))))
      (org-canvas--log-warning org-canvas--logger
        "[Stage 4: Finalize] Rubric '%s' came back as id %s, not %s: any assignment associated with %s lost it — re-push the assignments whose RUBRIC_LINK names this rubric"
        (plist-get data :title) id previous previous))))

(defun org-canvas--rubric-finalize (data response)
  "Update local Org file with CANVAS_ID using DATA and RESPONSE.
Canvas answers a rubric write with the rubric under a `rubric' key,
so RESPONSE is unwrapped before the shared finalize sees it."
  (org-canvas--log-debug org-canvas--logger "[Stage 4: Finalize] Processing response...")
  (let ((rubric-data (or (alist-get 'rubric response) response)))
    (org-canvas--rubric-warn-recreated data (alist-get 'id rubric-data))
    (org-canvas--finalize-item data rubric-data)))

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
  (org-canvas--log-info org-canvas--logger
             "Dissociating rubric from assignment '%s' (association: %s)"
             assignment-name assoc-id)
  (condition-case err
      (progn
        (org-canvas-api-request 'DELETE
          (org-canvas-api-course-endpoint "rubric_associations/%s" assoc-id))
        (org-canvas--log-info org-canvas--logger "  -> Dissociated successfully")
        t)
    (error
     (org-canvas--log-warning org-canvas--logger
                   "  -> Dissociation failed: %s" (error-message-string err))
     nil)))

(defun org-canvas--rubric-dissociate-all ()
  "Remove all rubric associations from assignments in the course.
Canvas returns 500 when deleting a rubric still associated with
an assignment, so this must run before rubric deletion."
  (org-canvas--log-info org-canvas--logger "Dissociating rubrics from assignments...")
  (let* ((endpoint (org-canvas-api-course-endpoint "assignments"))
         (assignments (org-canvas-api-request-all-pages 'GET endpoint))
         (dissociated 0))
    (dolist (assignment assignments)
      (let ((rubric-settings (alist-get 'rubric_settings assignment)))
        (when rubric-settings
          (let ((assoc-id (alist-get 'id rubric-settings))
                (title (alist-get 'name assignment)))
            (when (and assoc-id
                       (org-canvas--rubric-delete-association assoc-id title))
              (setq dissociated (1+ dissociated)))))))
    (org-canvas--log-info org-canvas--logger "Dissociated %d rubric associations" dissociated)
    dissociated))

;;;; Rubric Diagnostics

(defun org-canvas--rubric-log-detail (rubric-id)
  "Fetch and log rubric detail with assessments for RUBRIC-ID."
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "rubrics/%s" rubric-id))
             (detail (org-canvas-api-request 'GET endpoint
                       :params '(("include[]" . "assessments")
                                 ("style" . "full")))))
        (org-canvas--log-warning org-canvas--logger "  [Detail] Full rubric response keys: %S"
                      (mapcar #'car detail))
        (let ((assessments (alist-get 'assessments detail))
              (context-type (alist-get 'context_type detail))
              (context-id (alist-get 'context_id detail))
              (reusable (alist-get 'reusable detail))
              (read-only (alist-get 'read_only detail)))
          (org-canvas--log-warning org-canvas--logger "  [Detail] context: %s (ID: %s)"
                        context-type context-id)
          (org-canvas--log-warning org-canvas--logger "  [Detail] reusable: %S | read_only: %S"
                        reusable read-only)
          (if assessments
              (progn
                (org-canvas--log-warning org-canvas--logger
                              "  [Detail] assessments: %d found (rubric has been used for grading)"
                              (length assessments))
                (dolist (assessment (append assessments nil))
                  (org-canvas--log-warning org-canvas--logger
                                "    assessment: id=%s type=%s artifact=%s/%s score=%s"
                                (alist-get 'id assessment)
                                (alist-get 'assessment_type assessment)
                                (alist-get 'artifact_type assessment)
                                (alist-get 'artifact_id assessment)
                                (alist-get 'score assessment))))
            (org-canvas--log-warning org-canvas--logger "  [Detail] assessments: none"))))
    (error
     (org-canvas--log-warning org-canvas--logger
                   "  [Detail] Failed to fetch rubric detail: %s" (error-message-string err)))))

(defun org-canvas--rubric-find-linked-assignments (rubric-id)
  "Return list of assignments that reference RUBRIC-ID."
  (let* ((endpoint (org-canvas-api-course-endpoint "assignments"))
         (assignments (org-canvas-api-request-all-pages 'GET endpoint))
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
              (org-canvas--log-warning org-canvas--logger
                            "  [Assignments] %d assignment(s) reference this rubric:"
                            (length linked))
              (dolist (a linked)
                (org-canvas--log-warning org-canvas--logger
                              "    assignment: id=%s name='%s' rubric_id=%s has_rubric_settings=%s"
                              (alist-get 'id a)
                              (alist-get 'name a)
                              (alist-get 'rubric_id a)
                              (if (alist-get 'rubric_settings a) "yes" "no"))))
          (org-canvas--log-warning org-canvas--logger
                        "  [Assignments] No assignments reference this rubric")))
    (error
     (org-canvas--log-warning org-canvas--logger
                   "  [Assignments] Failed to check assignments: %s" (error-message-string err)))))

(defun org-canvas--rubric-log-diagnostics (rubric-id list-data)
  "Fetch and log detailed info about rubric RUBRIC-ID after a failed deletion.
LIST-DATA is the rubric alist from the list response."
  (org-canvas--log-warning org-canvas--logger "--- Diagnostic info for rubric %s ---" rubric-id)
  (dolist (pair list-data)
    (org-canvas--log-warning org-canvas--logger "  %s: %S" (car pair) (cdr pair)))
  (org-canvas--rubric-log-detail rubric-id)
  (org-canvas--rubric-log-linked-assignments rubric-id)
  (org-canvas--log-warning org-canvas--logger "--- End diagnostic info ---"))

;;;; Delete All Rubrics (custom, with dissociation and diagnostics)

(defun org-canvas--rubric-verify-deleted (rubric-id item)
  "Check whether RUBRIC-ID was actually deleted despite an API error.
ITEM is the rubric alist for diagnostics.
Returns t if confirmed deleted, nil if still present."
  (org-canvas--log-info org-canvas--logger "  -> Verifying whether rubric was actually deleted...")
  (condition-case _verify-err
      (progn
        (org-canvas-api-request 'GET
          (org-canvas-api-course-endpoint "rubrics/%s" rubric-id))
        (org-canvas--log-error org-canvas--logger "  -> Rubric still exists. Deletion truly failed.")
        (org-canvas--rubric-log-diagnostics rubric-id item)
        nil)
    (error
     (org-canvas--log-info org-canvas--logger "  -> Rubric no longer exists (confirmed deleted)")
     t)))

(defun org-canvas--rubric-delete-with-verify (item)
  "Delete a single rubric ITEM, verifying on error.
Returns the string ID if deleted, nil otherwise."
  (let ((item-id (alist-get 'id item)))
    (condition-case err
        (progn
          (org-canvas-api-request 'DELETE
            (org-canvas-api-course-endpoint "rubrics/%s" item-id))
          (org-canvas--log-info org-canvas--logger "  -> Deleted successfully")
          (org-canvas--normalize-id item-id))
      (error
       (org-canvas--log-warning org-canvas--logger "  -> Delete returned error: %s" (error-message-string err))
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
  (org-canvas--log-warning org-canvas--logger "========================================")
  (org-canvas--log-warning org-canvas--logger ">>> STARTING MASS DELETION OF RUBRICS")
  (org-canvas--log-warning org-canvas--logger "========================================")

  ;; Step 1: Dissociate rubrics from assignments
  (org-canvas--rubric-dissociate-all)

  ;; Step 2: Fetch all rubrics and delete with diagnostics on failure
  (let* ((full-endpoint (org-canvas-api-course-endpoint "rubrics"))
         (remote-items (org-canvas-api-request-all-pages 'GET full-endpoint))
         (deleted-count 0)
         (deleted-ids nil))

    (org-canvas--log-info org-canvas--logger "Found %d rubrics on Canvas" (length remote-items))

    (dolist (item remote-items)
      (let* ((item-id (alist-get 'id item))
             (item-title (alist-get 'title item))
             (result (org-canvas--rubric-delete-with-verify item)))
        (org-canvas--log-info org-canvas--logger "Deleting: '%s' (ID: %s)" item-title item-id)
        (when result
          (push result deleted-ids)
          (setq deleted-count (1+ deleted-count)))))

    ;; Cleanup local properties
    (org-canvas--clean-local-sync-properties org-canvas-rubrics-file)

    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d removed" deleted-count)
    (org-canvas--log-info org-canvas--logger "========================================")
    (message "Rubrics deletion complete. %d removed." deleted-count)))

;;;; Pull

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
    (with-current-buffer (org-canvas--find-file-noselect org-canvas-outcomes-file)
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

(defun org-canvas--rubric-pull-outcome-link (outcome-id)
  "Build the OUTCOME property link string for a pulled criterion.
Returns an Org file link when OUTCOME-ID resolves, the raw ID as fallback,
or nil when OUTCOME-ID is nil."
  (when outcome-id
    (let ((title (org-canvas--rubric-outcome-title outcome-id)))
      (if title
          (format "[[file:outcomes.org::*%s][%s]]" title title)
        (format "%s" outcome-id)))))

(defun org-canvas--rubric-pull-emit-rating-rows (ratings)
  "Insert sorted RATINGS as table rows under the current criterion."
  (dolist (r (org-canvas--rubric-sort-ratings ratings))
    (let ((rdesc (replace-regexp-in-string
                  "|" "/"
                  (org-canvas--html-to-org-inline
                   (or (alist-get 'description r) ""))))
          (rpts (or (alist-get 'points r) 0))
          (rlong (replace-regexp-in-string
                  "|" "/"
                  (org-canvas--html-to-org-inline
                   (or (alist-get 'long_description r) "")))))
      (insert (format "| %s | %s | %s |\n"
                      rdesc
                      (org-canvas--rubric-format-points rpts)
                      rlong)))))

(defun org-canvas--rubric-pull-emit-criterion (c)
  "Emit a single criterion C as a level-2 heading with sub-table.
C is the API alist for one criterion."
  (let* ((desc (replace-regexp-in-string
                "|" "/"
                (org-canvas--html-to-org-inline
                 (or (alist-get 'description c) ""))))
         (pts (or (alist-get 'points c) 0))
         (long-desc (string-trim
                     (org-canvas--html-to-org-inline
                      (or (alist-get 'long_description c) ""))))
         (ratings (alist-get 'ratings c))
         (outcome-id (alist-get 'learning_outcome_id c))
         (outcome-link (org-canvas--rubric-pull-outcome-link outcome-id)))
    (insert (format "** %s :%s:\n" desc (org-canvas--rubric-points-tag pts)))
    (when outcome-link
      (insert ":PROPERTIES:\n")
      (insert (format ":OUTCOME: %s\n" outcome-link))
      (insert ":END:\n"))
    (unless (string-empty-p long-desc)
      (insert long-desc "\n"))
    (insert "| Rating | Points | Description |\n")
    (insert "|--------+--------+-------------|\n")
    (when ratings
      (org-canvas--rubric-pull-emit-rating-rows ratings))
    (insert "\n")))

(defun org-canvas--rubric-pull-item (item pos)
  "Set per-item content for a pulled rubric.
ITEM is the API response alist, POS is the heading position.
Writes the registry's properties, then replaces the rubric body with
one level-2 heading per criterion."
  (org-canvas--pull-item-from-registry "rubrics" item pos)
  (let ((criteria (alist-get 'data item)))
    (when criteria
      ;; Anchor deletion at the end of the drawer's last non-blank line so
      ;; the whole criterion body is replaced and re-pull stays idempotent
      ;; (otherwise each pull prepends another blank line).  See
      ;; `org-canvas--pull-insert-body' for the same pattern and rationale.
      (let* ((meta-end (save-excursion (org-end-of-meta-data t) (point)))
             (body-end (save-excursion (org-end-of-subtree t t) (point)))
             (body-start (save-excursion
                           (goto-char (min meta-end body-end))
                           (skip-chars-backward " \t\n")
                           (point))))
        (delete-region body-start body-end)
        (goto-char body-start)
        (insert "\n")
        (dolist (c (append criteria nil))
          (org-canvas--rubric-pull-emit-criterion c))))))

(org-canvas-define-pull rubrics
  :file org-canvas-rubrics-file
  :endpoint "rubrics"
  :pull-item-fn #'org-canvas--rubric-pull-item)

(org-canvas-define-delete-at-point rubric
  :endpoint "rubrics/%s")

(provide 'org-canvas-rubrics)
;;; org-canvas-rubrics.el ends here
