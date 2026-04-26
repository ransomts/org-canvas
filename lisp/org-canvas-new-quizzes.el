;;; org-canvas-new-quizzes.el --- New Quizzes Sync Pipeline for Canvas LMS -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas New Quizzes.
;;
;; New Quizzes is a newer quiz engine in Canvas LMS that uses a different
;; API from Classic Quizzes.  Key differences:
;;
;;   - API path:  /api/quiz/v1/courses/:id/ (not /api/v1/)
;;   - Update method: PATCH (not PUT)
;;   - ID field: assignment_id (New Quizzes are assignment-backed)
;;   - Questions: "items" with interaction_type_slug and interaction_data
;;
;; FILE STRUCTURE
;; ==============
;; In new-quizzes.org:
;;   - Level 1 headings = Quizzes (with TIME_LIMIT, SHUFFLE_ANSWERS, etc.)
;;   - Level 2 headings = Items/Questions (with TYPE, POINTS)
;;   - List items under questions = Answer choices
;;
;; QUESTION TYPES (interaction_type_slug)
;; ======================================
;;   choice           - [X] marks correct, [ ] marks wrong
;;   true_false       - [X] True or [X] False
;;   multi_answer     - Multiple [X] allowed
;;   short_answer     - [X] marks each acceptable answer
;;   essay            - No answers needed
;;   file_upload      - No answers needed
;;   numerical        - [X] 42 or [X] [10, 20] for range
;;   matching         - left = right pairs
;;   ordering         - Numbered list: 1. item
;;   categorization   - Category: item1, item2
;;   fill_in_the_blank - [X] for each blank answer
;;   hot_spot         - Coordinate data (advanced)
;;
;; API NOTES
;; =========
;; New Quizzes and items are separate API resources:
;;   POST/PATCH /api/quiz/v1/courses/:course_id/quizzes
;;   POST/PATCH /api/quiz/v1/courses/:course_id/quizzes/:quiz_id/items

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-new-quiz-items)
(require 'ox-html)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-new-quizzes-file (org-canvas--path "new-quizzes.org")
  "Path to the new-quizzes.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-new-quizzes-file "new-quizzes.org")
(org-canvas-register-properties "new-quizzes"
  :label "New Quizzes"
  :file-var 'org-canvas-new-quizzes-file
  :query "LEVEL=1"
  :properties
  `((:org-prop "TIME_LIMIT" :data-key :time_limit :type number)
    (:org-prop "SHUFFLE_ANSWERS" :data-key :shuffle_answers :type boolean)
    (:org-prop "ONE_AT_A_TIME" :data-key :one_at_a_time :type boolean)
    (:org-prop "ALLOWED_ATTEMPTS" :data-key :allowed_attempts :type number)
    (:org-prop "SCORING_POLICY" :data-key :scoring_policy :type enum
     :values ,org-canvas--valid-new-quiz-scoring-policies)
    (:org-prop "GROUP" :data-key :assignment_group_id :type link
     :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID")
    (:org-prop "RUBRIC_LINK" :data-key :rubric_id :type link
     :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID")))
(org-canvas-register-properties "new-quiz-items"
  :label "New Quiz Items"
  :file-var 'org-canvas-new-quizzes-file
  :query "LEVEL=2"
  :properties
  `((:org-prop "POINTS" :data-key :points :type number)
    (:org-prop "TYPE" :data-key :type :type enum
     :values ,org-canvas--valid-new-quiz-types)
    (:org-prop "OUTCOME" :data-key :outcome :type link
     :target-file org-canvas-outcomes-file :link-id-property "CANVAS_ID")))

(defvar org-canvas--new-quiz-debug-types nil
  "When non-nil, only sync item types in this list during debugging.
Set to a list of type strings like (\"choice\" \"true-false\") to restrict.
Set to nil to sync all types (normal operation).")




;;;; Quiz Parsing (Level 1)

(defun org-canvas--new-quiz-read-props ()
  "Read raw property strings from the Org heading at point.
Returns a plist of raw values with no transformations applied."
  (let* ((pom (point-marker))
         (title-raw (org-get-heading t t t t))
         (canvas-id (org-entry-get pom "CANVAS_ASSIGNMENT_ID"))
         (time-limit-raw (org-entry-get pom "TIME_LIMIT"))
         (shuffle-raw (org-entry-get pom "SHUFFLE_ANSWERS"))
         (one-at-a-time-raw (org-entry-get pom "ONE_AT_A_TIME"))
         (attempts-raw (org-entry-get pom "ALLOWED_ATTEMPTS"))
         (scoring-policy-raw (org-entry-get pom "SCORING_POLICY"))
         (group-link (org-entry-get pom "GROUP"))
         (assignment-group-id (when group-link
                                (org-canvas--resolve-link-property
                                 group-link "CANVAS_ID"
                                 org-canvas-new-quizzes-file)))
         (rubric-link (org-entry-get pom "RUBRIC_LINK"))
         (rubric-id (when rubric-link
                      (org-canvas--resolve-link-property
                       rubric-link "CANVAS_ID"
                       org-canvas-new-quizzes-file)))
         (body-text (org-canvas--new-quiz-parse-body-text)))
    (list :title-raw title-raw
          :canvas-id canvas-id
          :time-limit-raw time-limit-raw
          :shuffle-raw shuffle-raw
          :one-at-a-time-raw one-at-a-time-raw
          :attempts-raw attempts-raw
          :scoring-policy-raw scoring-policy-raw
          :assignment-group-id assignment-group-id
          :rubric-id rubric-id
          :body-text body-text
          :pom pom)))

(defun org-canvas--new-quiz-transform-props (props)
  "Apply pure transformations to raw PROPS plist.
No buffer access — only string/number/boolean conversions."
  (let* ((title (org-canvas--strip-statistics-cookie
                 (plist-get props :title-raw)))
         (time-limit-raw (plist-get props :time-limit-raw))
         (shuffle (org-canvas--interpret-boolean
                   (plist-get props :shuffle-raw)))
         (one-at-a-time (org-canvas--interpret-boolean
                         (plist-get props :one-at-a-time-raw)))
         (attempts-raw (plist-get props :attempts-raw))
         (scoring-policy (org-canvas--validate-property
                          (plist-get props :scoring-policy-raw)
                          org-canvas--valid-new-quiz-scoring-policies
                          "SCORING_POLICY" nil))
         (assignment-group-id (plist-get props :assignment-group-id)))
    (list :title title
          :canvas-id (plist-get props :canvas-id)
          :time_limit (when time-limit-raw
                        (org-canvas--safe-string-to-number
                         time-limit-raw "TIME_LIMIT"))
          :shuffle_answers shuffle
          :one_at_a_time one-at-a-time
          :allowed_attempts (when attempts-raw
                              (org-canvas--safe-string-to-number
                               attempts-raw "ALLOWED_ATTEMPTS"))
          :scoring_policy scoring-policy
          :assignment_group_id (when assignment-group-id
                                 (string-to-number assignment-group-id))
          :rubric-id (plist-get props :rubric-id)
          :body-text (plist-get props :body-text)
          :pom (plist-get props :pom))))

(defun org-canvas--new-quiz-parse-entry ()
  "Extract New Quiz data from the Org heading at point.
Reads raw properties, transforms them, and exports description to HTML."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[New Quiz Parse] Starting at point %d" (point))

  (let* ((raw (org-canvas--new-quiz-read-props))
         (data (org-canvas--new-quiz-transform-props raw))
         (title (plist-get data :title))
         (canvas-id (plist-get data :canvas-id))
         (body-text (plist-get data :body-text))
         (pom (plist-get data :pom)))

    (org-canvas--require-title title pom "New Quiz")

    (org-canvas--log-info org-canvas--logger "[New Quiz Parse] Quiz: '%s' (ID: %s)"
      title (or canvas-id "NEW"))

    ;; Replace :body-text with HTML :description in final result
    (plist-put data :description
               (when (and body-text (> (length body-text) 0))
                 (let ((org-export-with-sub-superscripts nil))
                   (org-export-string-as body-text 'html t))))
    (plist-put data :body-text nil)
    data))

;;;; Quiz Build Payload

(defun org-canvas--new-quiz-build-payload (data)
  "Convert New Quiz DATA to Canvas API payload."
  (let ((payload (make-hash-table :test 'equal)))
    (puthash "title" (plist-get data :title) payload)

    (when-let ((desc (plist-get data :description)))
      (puthash "instructions" desc payload))

    (when-let ((limit (plist-get data :time_limit)))
      (puthash "time_limit" limit payload))

    (when (plist-get data :shuffle_answers)
      (puthash "shuffle_answers" t payload))

    (when (plist-get data :one_at_a_time)
      (puthash "one_at_a_time" t payload))

    (when-let ((attempts (plist-get data :allowed_attempts)))
      (puthash "allowed_attempts" attempts payload))

    (when-let ((scoring (plist-get data :scoring_policy)))
      (puthash "scoring_policy" scoring payload))

    (when-let ((group-id (plist-get data :assignment_group_id)))
      (puthash "assignment_group_id" group-id payload))

    payload))

;;;; Quiz Push to API

(cl-defun org-canvas--new-quiz-push-to-api (data payload)
  "Send New Quiz PAYLOAD (from DATA) to Canvas API.
Uses POST for new quizzes and PATCH for existing ones.
PAYLOAD is the inner quiz data; it is wrapped under a \"quiz\" key
as required by the New Quizzes API.
Returns response with assignment_id."
  (let* ((id (plist-get data :canvas-id))
         (title (plist-get data :title))
         (method (if id 'PATCH 'POST))
         (endpoint (if id
                       (org-canvas--new-quiz-api-endpoint "quizzes/%s" id)
                     (org-canvas--new-quiz-api-endpoint "quizzes")))
         (wrapped (let ((ht (make-hash-table :test 'equal)))
                    (puthash "quiz" payload ht)
                    ht)))

    (when org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s New Quiz '%s' to %s"
        method title endpoint)
      (cl-return-from org-canvas--new-quiz-push-to-api
        '((assignment_id . "dry-run"))))

    (org-canvas--log-info org-canvas--logger "[New Quiz API] %s '%s'" method title)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data wrapped)))
          (org-canvas--log-info org-canvas--logger "[New Quiz API] %s successful for '%s'"
            method title)
          response)
      (error
       (org-canvas--log-error org-canvas--logger "[New Quiz API] Failed: %s"
         (error-message-string err))
       (cond
        ;; 404 on PATCH -> retry as POST (stale ID)
        ((and (eq method 'PATCH)
              (org-canvas--404-error-p err))
         (org-canvas--log-warning org-canvas--logger
           "[Recovery] Item not found (404). Retrying as POST...")
         (condition-case post-err
             (let ((response (org-canvas-api-request
                              'POST
                              (org-canvas--new-quiz-api-endpoint "quizzes")
                              :data wrapped)))
               (org-canvas--log-info org-canvas--logger "[Recovery] POST successful")
               response)
           (error
            (signal (car post-err) (cdr post-err)))))
        (t (signal (car err) (cdr err))))))))

;;;; Quiz Finalize

(defun org-canvas--new-quiz-sync-children (data response)
  "Sync items and associate rubric for the new quiz in DATA/RESPONSE."
  (let ((quiz-id (or (alist-get 'assignment_id response)
                     (alist-get 'id response)
                     (plist-get data :canvas-id)))
        (marker (point-marker)))
    (when quiz-id
      (org-canvas--sync-new-quiz-items marker quiz-id))
    ;; Associate rubric if RUBRIC_LINK is set
    (let ((rubric-id (plist-get data :rubric-id))
          (assignment-id (alist-get 'assignment_id response)))
      (when rubric-id
        (org-canvas--associate-rubric assignment-id rubric-id "Assignment")))))

(defun org-canvas--new-quiz-finalize (data response)
  "Finalize new quiz DATA with RESPONSE and sync child items.
Falls back to \\='id when \\='assignment_id is absent in RESPONSE."
  (let ((effective-response
         (if (alist-get 'assignment_id response)
             response
           ;; Fallback: copy assignment_id from id
           (cons (cons 'assignment_id (alist-get 'id response)) response))))
    (org-canvas--finalize-item data effective-response
      :id-field 'assignment_id
      :id-property "CANVAS_ASSIGNMENT_ID"
      :post-fn #'org-canvas--new-quiz-sync-children)))


;;;; Sync Item Loop

(defun org-canvas--sync-new-quiz-items (quiz-marker quiz-assignment-id)
  "Sync all items under the New Quiz at QUIZ-MARKER.
QUIZ-ASSIGNMENT-ID is the assignment ID of the parent quiz."
  (let ((item-markers nil)
        (item-success 0)
        (item-skipped 0))
    ;; Collect all item markers (level-2 headings under this quiz)
    (with-current-buffer (marker-buffer quiz-marker)
      (save-excursion
        (goto-char (marker-position quiz-marker))
        (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
          (while (and (outline-next-heading)
                      (< (point) subtree-end))
            (when (= (org-outline-level) 2)
              (push (point-marker) item-markers)))))
      (setq item-markers (nreverse item-markers)))

    ;; Sync each item using stable markers
    (dolist (m item-markers)
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char (marker-position m))
          (condition-case err
              (let* ((data (org-canvas--new-quiz-item-parse-entry quiz-assignment-id))
                     (q-type (plist-get data :type)))
                (if (and org-canvas--new-quiz-debug-types
                         (not (member q-type org-canvas--new-quiz-debug-types)))
                    (progn
                      (org-canvas--log-info org-canvas--logger
                        "[DEBUG SKIP] Skipping type '%s' for '%s'"
                        q-type (plist-get data :title))
                      (setq item-skipped (1+ item-skipped)))
                  (let* ((payload (org-canvas--new-quiz-item-build-payload data))
                         (response (org-canvas--new-quiz-item-push-to-api data payload)))
                    (org-canvas--new-quiz-item-finalize data response)
                    (setq item-success (1+ item-success)))))
            (error
             (org-canvas--log-error org-canvas--logger "[New Quiz Item] Failed: %s"
               (error-message-string err)))))))

    ;; Release markers to avoid memory leaks
    (dolist (m item-markers) (set-marker m nil))

    (when (> item-skipped 0)
      (org-canvas--log-info org-canvas--logger "[New Quiz Items] %d skipped (debug filter)"
        item-skipped))
    (org-canvas--log-info org-canvas--logger "[New Quiz Items] %d/%d synced"
      item-success (- (length item-markers) item-skipped))
    (cons item-success (- (length item-markers) item-success item-skipped))))

;;;; Main Sync Function

(org-canvas-define-sync new-quizzes
  :file org-canvas-new-quizzes-file
  :parse #'org-canvas--new-quiz-parse-entry
  :build #'org-canvas--new-quiz-build-payload
  :push #'org-canvas--new-quiz-push-to-api
  :finalize #'org-canvas--new-quiz-finalize
  :no-at-point t)

;;;; Sync at Point

;;;###autoload
(defun org-canvas-sync-new-quiz-at-point ()
  "Sync the New Quiz at point (level-1 heading only)."
  (interactive)
  (org-back-to-heading t)
  (unless (= (org-outline-level) 1)
    (user-error "Point must be on a level-1 quiz heading"))
  (let* ((data (org-canvas--new-quiz-parse-entry))
         (payload (org-canvas--new-quiz-build-payload data))
         (response (org-canvas--new-quiz-push-to-api data payload)))
    (org-canvas--new-quiz-finalize data response)
    (let ((quiz-id (or (alist-get 'assignment_id response)
                       (alist-get 'id response)
                       (plist-get data :canvas-id))))
      (when quiz-id
        (org-canvas--sync-new-quiz-items (point-marker) quiz-id)))
    ;; Associate rubric if RUBRIC_LINK is set
    (let ((rubric-id (plist-get data :rubric-id))
          (assignment-id (alist-get 'assignment_id response)))
      (when rubric-id
        (org-canvas--associate-rubric assignment-id rubric-id "Assignment")))
    (org-canvas--save-buffer)
    (message "New Quiz '%s' synced." (plist-get data :title))))

;;;; Delete Functions

;;;###autoload
(defun org-canvas-delete-all-new-quizzes ()
  "Delete ALL New Quizzes in the configured course."
  (interactive)
  (unless org-canvas--inhibit-log-clear
    (unless (y-or-n-p "Delete ALL new-quizzes in this course? ")
      (user-error "Aborted")))
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (org-canvas--log-warning org-canvas--logger "========================================")
  (org-canvas--log-warning org-canvas--logger ">>> STARTING MASS DELETION OF NEW-QUIZZES")
  (org-canvas--log-warning org-canvas--logger "========================================")
  (let* ((endpoint (org-canvas--new-quiz-api-endpoint "quizzes"))
         (remote-items (org-canvas-api-request-all-pages 'GET endpoint))
         (deleted 0))
    (org-canvas--log-info org-canvas--logger "Found %d new-quizzes on Canvas"
      (length remote-items))
    (dolist (item remote-items)
      (let* ((id (or (alist-get 'assignment_id item)
                     (alist-get 'id item)))
             (title (alist-get 'title item))
             (del-url (org-canvas--new-quiz-api-endpoint "quizzes/%s" id)))
        (condition-case err
            (progn
              (org-canvas-api-request 'DELETE del-url)
              (org-canvas--log-info org-canvas--logger "[Deleted] '%s' (ID: %s)" title id)
              (setq deleted (1+ deleted)))
          (error
           (org-canvas--log-warning org-canvas--logger "[Delete Failed] '%s': %s"
             title (error-message-string err))))))
    ;; Clean local properties
    (org-canvas--clean-local-sync-properties
     org-canvas-new-quizzes-file "CANVAS_ASSIGNMENT_ID")
    (message "New-quizzes deletion complete. %d removed." deleted)))

;;;; Pull

(defun org-canvas--new-quiz-pull-set-properties (pos quiz)
  "Set all properties on heading at POS from New Quiz API response QUIZ."
  (let ((assignment-id (or (alist-get 'assignment_id quiz)
                          (alist-get 'id quiz)))
        (time-limit (alist-get 'time_limit quiz))
        (shuffle (alist-get 'shuffle_answers quiz))
        (one-at-a-time (alist-get 'one_at_a_time quiz))
        (attempts (alist-get 'allowed_attempts quiz))
        (scoring (alist-get 'scoring_policy quiz)))
    (org-canvas-org-save-sync-state pos assignment-id "CANVAS_ASSIGNMENT_ID")
    (when time-limit
      (org-canvas-org-set-property pos "TIME_LIMIT" (format "%s" time-limit)))
    (when shuffle
      (org-canvas--pull-set-boolean-property pos "SHUFFLE_ANSWERS" shuffle))
    (when one-at-a-time
      (org-canvas--pull-set-boolean-property pos "ONE_AT_A_TIME" one-at-a-time))
    (when attempts
      (org-canvas-org-set-property pos "ALLOWED_ATTEMPTS" (format "%s" attempts)))
    (when scoring
      (org-canvas-org-set-property pos "SCORING_POLICY" scoring))))


(defun org-canvas--new-quiz-pull-items (quiz-assignment-id)
  "Fetch and insert items for QUIZ-ASSIGNMENT-ID as L2 headings."
  (condition-case nil
      (let* ((url (org-canvas--new-quiz-api-endpoint
                   "quizzes/%s/items" quiz-assignment-id))
             (items (org-canvas-api-request-all-pages 'GET url)))
        (dolist (item (org-canvas--pull-sort-items items))
          (org-canvas--new-quiz-pull-insert-item item)))
    (error nil)))

;;;###autoload
(defun org-canvas-pull-new-quizzes ()
  "Pull New Quizzes from Canvas into new-quizzes.org."
  (interactive)
  (org-canvas--start-operation "PULLING NEW QUIZZES")
  (let* ((file (expand-file-name org-canvas-new-quizzes-file))
         (endpoint (org-canvas--new-quiz-api-endpoint "quizzes"))
         (remote (org-canvas-api-request-all-pages 'GET endpoint))
         (count 0)
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "new quizzes")
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (find-file-noselect file)
      (dolist (quiz (org-canvas--pull-sort-items remote))
        (let* ((assignment-id (or (alist-get 'assignment_id quiz)
                                  (alist-get 'id quiz)))
               (title (alist-get 'title quiz))
               (pos (org-canvas--pull-upsert-heading
                     file assignment-id title "CANVAS_ASSIGNMENT_ID")))
          (goto-char pos)
          (when title (org-edit-headline title))
          (org-canvas--new-quiz-pull-set-properties pos quiz)
          (org-canvas--new-quiz-pull-items assignment-id)
          (cl-incf count)))
      (org-canvas--pull-write-file-header)
      (org-canvas--save-buffer))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger "New Quizzes pull complete: %d quizzes" count)
    (message "New Quizzes pull complete: %d quizzes." count)))

(provide 'org-canvas-new-quizzes)
;;; org-canvas-new-quizzes.el ends here
