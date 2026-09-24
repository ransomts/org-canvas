;;; org-canvas-new-quizzes.el --- New Quizzes Sync Pipeline for Canvas LMS -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas New Quizzes.
;;
;; It requires `org-canvas-new-quiz-items', its own sub-module holding the
;; question/item pipeline.  That is the one feature-to-feature require in
;; the package, sanctioned because the items file is not a feature: it
;; registers nothing, defines no command, and requires only core.  Nothing
;; else may require either file (see CLAUDE.md, "Dependency Rules").
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
;;   fill_in_the_blank - Not supported; use short_answer (issue #337)
;;   hot_spot         - Pulled only; a push refuses it (issue #340)
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
;; A New Quiz is an assignment to the web interface: its page launches
;; the quiz, its edit page the settings.  An item opens its quiz (#292).
(org-canvas-register-web-pages
 "New Quizzes" 'org-canvas-new-quizzes-file
 '(:level 1 :id-property "CANVAS_ASSIGNMENT_ID" :path "assignments/%s"
   :edit "assignments/%s/edit"))
;; New Quizzes stay out of the feature registry (see the pull entry
;; below), so a delete learns their id stamp here (#331).
(org-canvas-register-id-property "CANVAS_ASSIGNMENT_ID")
(org-canvas-register-properties "new-quizzes"
  :duplicate-titles t
  :label "New Quizzes"
  :file-var 'org-canvas-new-quizzes-file
  :query "LEVEL=1"
  :properties
  `((:org-prop "TIME_LIMIT" :data-key :time_limit :type number
     :doc "Time limit in minutes")
    (:org-prop "SHUFFLE_ANSWERS" :data-key :shuffle_answers :type boolean
     :doc "Randomize answer order")
    (:org-prop "ONE_AT_A_TIME" :data-key :one_at_a_time :type boolean
     :doc "Show one question per page")
    (:org-prop "ALLOWED_ATTEMPTS" :data-key :allowed_attempts :type number
     :doc "Max attempts")
    (:org-prop "SCORING_POLICY" :data-key :scoring_policy :type enum
     :values ,org-canvas--valid-new-quiz-scoring-policies
     :doc "Which attempt's score to keep across multiple attempts")
    (:org-prop "GROUP" :data-key :assignment_group_id :type link
     :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID"
     :doc "Link to assignment group in assignment-groups.org")
    (:org-prop "RUBRIC_LINK" :data-key :rubric_id :type link
     :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID"
     :doc "Link to rubric in rubrics.org (uses backing assignment)")))
(org-canvas-register-properties "new-quiz-items"
  :label "New Quiz Items"
  :file-var 'org-canvas-new-quizzes-file
  :query "LEVEL=2"
  :structural-fn #'org-canvas--validate-new-quiz-item-type
  :properties
  `((:org-prop "POINTS" :data-key :points :type number
     :doc "Points for this question")
    (:org-prop "TYPE" :data-key :type :type enum
     :values ,org-canvas--valid-new-quiz-types
     :read-only-values ,org-canvas--new-quiz-pull-only-types
     :doc "Question type (see below)")
    (:org-prop "OUTCOME" :data-key :outcome :type link
     :target-file org-canvas-outcomes-file :link-id-property "CANVAS_ID"
     :doc "Link to outcome in outcomes.org (local-only)")))

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
                 (org-canvas--org-to-html-string body-text)))
    (plist-put data :body-text nil)
    data))

;;;; Quiz Build Payload

(defun org-canvas--new-quiz-build-payload (data)
  "Convert New Quiz DATA to Canvas API payload."
  (let ((payload (make-hash-table :test 'equal)))
    (puthash "title" (plist-get data :title) payload)

    (when-let* ((desc (plist-get data :description)))
      (puthash "instructions" desc payload))

    (when-let* ((limit (plist-get data :time_limit)))
      (puthash "time_limit" limit payload))

    (when (plist-get data :shuffle_answers)
      (puthash "shuffle_answers" t payload))

    (when (plist-get data :one_at_a_time)
      (puthash "one_at_a_time" t payload))

    (when-let* ((attempts (plist-get data :allowed_attempts)))
      (puthash "allowed_attempts" attempts payload))

    (when-let* ((scoring (plist-get data :scoring_policy)))
      (puthash "scoring_policy" scoring payload))

    (when-let* ((group-id (plist-get data :assignment_group_id)))
      (puthash "assignment_group_id" group-id payload))

    payload))

;;;; Quiz Push to API

(defun org-canvas--new-quiz-find-by-title (title)
  "Return the New Quiz on Canvas under TITLE, or nil.
One list request; a failed read is logged and counts as no match, so a
create goes ahead as it did before the lookup existed."
  (condition-case err
      (cl-find-if (lambda (quiz)
                    (and (consp quiz) (consp (car quiz))
                         (equal (alist-get 'title quiz) title)))
                  (append (org-canvas-api-request-all-pages
                           'GET (org-canvas--new-quiz-api-endpoint "quizzes"))
                          nil))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[New Quiz API] Could not list the course's New Quizzes (%s); '%s' is created without the duplicate check"
       (error-message-string err) title)
     nil)))

(defun org-canvas--new-quiz-remote-id (quiz)
  "Return the id org-canvas stamps for remote New Quiz QUIZ, as a string."
  (format "%s" (or (alist-get 'assignment_id quiz) (alist-get 'id quiz))))

(defun org-canvas--new-quiz-guard-duplicate (data title ctx)
  "Before creating TITLE, ask whether it is already on Canvas.
The counterpart of `org-canvas--push-guard-duplicate' for the one
top-level push that does not go through `org-canvas--push-to-api'
\(issue #179): New Quizzes list from their own API, so the sync's
snapshot never carries them.  Returns nil to create, `skip' to leave
the heading alone, or the adopted id — written into DATA so the push
becomes a PATCH.  Not consulted when DATA carries an id, during a dry
run, or when `org-canvas-duplicate-title-strategy' is `create'.  CTX
is the run context, whose capital answer applies."
  (unless (or (plist-get data :canvas-id)
              org-canvas--dry-run
              (eq org-canvas-duplicate-title-strategy 'create))
    (let ((twin (org-canvas--new-quiz-find-by-title title)))
      (when twin
        (let* ((id (org-canvas--new-quiz-remote-id twin))
               (action (org-canvas--resolve-duplicate title (list id) ctx)))
          (pcase action
            ('adopt
             (org-canvas--log-info org-canvas--logger
               "[Duplicate] Adopted Canvas id %s for New Quiz '%s' — updating it instead of creating a second"
               id title)
             (plist-put data :canvas-id id)
             id)
            ('skip
             (org-canvas--log-warning org-canvas--logger
               "[Duplicate] Skipping New Quiz '%s' — Canvas already holds it as id %s; stamp CANVAS_ASSIGNMENT_ID, or rename the heading"
               title id)
             'skip)
            (_
             (org-canvas--log-warning org-canvas--logger
               "[Duplicate] Creating New Quiz '%s' although Canvas already holds it as id %s"
               title id)
             nil)))))))

(defun org-canvas--new-quiz-recover-404 (title wrapped)
  "Recover a New Quiz PATCH that 404ed: update TITLE's twin, or POST WRAPPED.
The stamped id is gone; when Canvas still holds the title under
another id that one is updated, so the recovery cannot make a second
copy (issue #179)."
  (let ((twin (unless (eq org-canvas-duplicate-title-strategy 'create)
                (org-canvas--new-quiz-find-by-title title))))
    (if twin
        (let ((id (org-canvas--new-quiz-remote-id twin)))
          (org-canvas--log-warning org-canvas--logger
            "[Recovery] New Quiz '%s' is gone under its stamped id, but Canvas holds the title as id %s — updating that one instead of creating a second copy"
            title id)
          (org-canvas-api-request
           'PATCH (org-canvas--new-quiz-api-endpoint "quizzes/%s" id) :data wrapped))
      (org-canvas--log-warning org-canvas--logger
        "[Recovery] Item not found (404). Retrying as POST...")
      (let ((response (org-canvas-api-request
                       'POST (org-canvas--new-quiz-api-endpoint "quizzes")
                       :data wrapped)))
        (org-canvas--log-info org-canvas--logger "[Recovery] POST successful")
        response))))

(cl-defun org-canvas--new-quiz-push-to-api (data payload &optional ctx)
  "Send New Quiz PAYLOAD (from DATA) to Canvas API.
Uses POST for new quizzes and PATCH for existing ones; before a POST,
`org-canvas--new-quiz-guard-duplicate' asks whether Canvas already
holds the title, and a `skip' answer returns the symbol `duplicate'
the way `org-canvas--push-to-api' does.  CTX is the run context.
PAYLOAD is the inner quiz data; it is wrapped under a \"quiz\" key
as required by the New Quizzes API.
Returns response with assignment_id."
  (let* ((title (plist-get data :title))
         (guard (org-canvas--new-quiz-guard-duplicate data title ctx))
         (id (plist-get data :canvas-id))
         (method (if id 'PATCH 'POST))
         (endpoint (if id
                       (org-canvas--new-quiz-api-endpoint "quizzes/%s" id)
                     (org-canvas--new-quiz-api-endpoint "quizzes")))
         (wrapped (let ((ht (make-hash-table :test 'equal)))
                    (puthash "quiz" payload ht)
                    ht)))

    (when (eq guard 'skip)
      (cl-return-from org-canvas--new-quiz-push-to-api 'duplicate))

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
        ;; 404 on PATCH -> update the title's twin, else retry as POST
        ((and (eq method 'PATCH)
              (org-canvas--404-error-p err))
         (org-canvas--new-quiz-recover-404 title wrapped))
        (t (signal (car err) (cdr err))))))))

;;;; Quiz Finalize

(defun org-canvas--new-quiz-sync-children (data response &optional _ctx)
  "Sync items and associate rubric for the new quiz in DATA/RESPONSE.
CTX, the run context, is accepted for the post-fn contract and unused."
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

(defun org-canvas--new-quiz-finalize (data response &optional ctx)
  "Finalize new quiz DATA with RESPONSE and sync child items.
CTX is the run context.  Falls back to \\='id when \\='assignment_id
is absent in RESPONSE."
  (let ((effective-response
         (if (alist-get 'assignment_id response)
             response
           ;; Fallback: copy assignment_id from id
           (cons (cons 'assignment_id (alist-get 'id response)) response))))
    (org-canvas--finalize-item data effective-response
      :ctx ctx
      :id-field 'assignment_id
      :id-property "CANVAS_ASSIGNMENT_ID"
      :post-fn #'org-canvas--new-quiz-sync-children)))


;;;; Sync Item Loop

(defun org-canvas--sync-new-quiz-items (quiz-marker quiz-assignment-id)
  "Sync all items under the New Quiz at QUIZ-MARKER.
QUIZ-ASSIGNMENT-ID is the assignment ID of the parent quiz.  An item
heading without a CANVAS_ITEM_ID adopts the item of its title the quiz
already holds, when one is unclaimed, instead of creating a second
\(issue #179)."
  (let ((item-markers nil)
        (item-success 0)
        (item-skipped 0)
        remote claimed)
    ;; Collect all item markers (level-2 headings under this quiz)
    (with-current-buffer (marker-buffer quiz-marker)
      (save-excursion
        (goto-char (marker-position quiz-marker))
        (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
          (while (and (outline-next-heading)
                      (< (point) subtree-end))
            (when (= (org-outline-level) 2)
              (push (point-marker) item-markers)))))
      (setq item-markers (nreverse item-markers)
            claimed (delq nil (mapcar (lambda (m) (org-entry-get m "CANVAS_ITEM_ID"))
                                      item-markers))
            remote (org-canvas--new-quiz-remote-items quiz-assignment-id item-markers)))

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
                  (let* ((adopted (org-canvas--adopt-child-twin
                                   data remote
                                   (lambda (item) (org-canvas--new-quiz-item-twin-p data item))
                                   claimed "[New Quiz Item]"))
                         (payload (org-canvas--new-quiz-item-build-payload data))
                         (response (org-canvas--new-quiz-item-push-to-api data payload)))
                    (when adopted (push adopted claimed))
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

(defconst org-canvas--new-quiz-ordering-digest-salt "|ordering-prompt=335"
  "Suffix for the items digest of a quiz holding an ordering item.
Before issue #335 an ordering item's answers, in their correct order,
were pushed inside its prompt.  The fix changes the payload of the
item and not the Org text, so without this salt a quiz pushed before
it would be skipped as unchanged and keep showing the answers; with
it, such a quiz's stored hash no longer matches, once, and the next
sync pushes it again.  A quiz with no ordering item keeps its hash.")

(defun org-canvas--new-quiz-has-ordering-item-p (pom)
  "Return non-nil when an item of the quiz at POM is of TYPE ordering."
  (save-excursion
    (goto-char pom)
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (case-fold-search nil))
      (outline-next-heading)
      (and (< (point) end)
           (re-search-forward "^[ \t]*:TYPE:[ \t]+ordering[ \t]*$" end t)))))

(defconst org-canvas--new-quiz-listed-prompt-digest-salt "|listed-prompt=337"
  "Suffix for the items digest of a quiz whose prompt-only item has a list.
Before issue #337 the prompt of an essay, file-upload or hot-spot item
was cut at its first bulleted line.  As with
`org-canvas--new-quiz-ordering-digest-salt', the fix changes the
payload and not the Org text, so this salt makes such a quiz's stored
hash stop matching, once, and the next sync pushes the whole prompt.")

(defun org-canvas--new-quiz-has-listed-prompt-p (pom)
  "Return non-nil when a prompt-only item of the quiz at POM has a list.
A prompt-only item is one whose TYPE is in
`org-canvas--new-quiz-prompt-only-types'."
  (save-excursion
    (goto-char pom)
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (found nil))
      (while (and (not found) (outline-next-heading) (< (point) end))
        (setq found
              (and (member (org-entry-get (point) "TYPE")
                           org-canvas--new-quiz-prompt-only-types)
                   (string-match-p org-canvas--new-quiz-prompt-end-regexp
                                   (org-canvas--new-quiz-parse-body-text)))))
      found)))

(defun org-canvas--new-quiz-items-digest (data)
  "Digest the item subtrees and rubric link of the new quiz in DATA.
Folded into the quiz payload hash via `:hash-extra': items sync inside
finalize, which the unchanged-skip bypasses, so without this an item
edit would never reach Canvas once the quiz's own attributes stopped
changing (same bug class as issue #26).  The rubric id is included
because rubric association also happens in finalize and is not part
of the quiz payload.  A quiz holding an ordering item adds
`org-canvas--new-quiz-ordering-digest-salt' (issue #335), and one
whose prompt-only item has a list adds
`org-canvas--new-quiz-listed-prompt-digest-salt' (issue #337)."
  (let ((pom (or (plist-get data :pom) (point))))
    (concat (org-canvas--org-children-digest pom)
            (format "|rubric=%s" (plist-get data :rubric-id))
            (when (org-canvas--new-quiz-has-ordering-item-p pom)
              org-canvas--new-quiz-ordering-digest-salt)
            (when (org-canvas--new-quiz-has-listed-prompt-p pom)
              org-canvas--new-quiz-listed-prompt-digest-salt))))

(org-canvas-define-sync new-quizzes
  :file org-canvas-new-quizzes-file
  :parse #'org-canvas--new-quiz-parse-entry
  :build #'org-canvas--new-quiz-build-payload
  :push #'org-canvas--new-quiz-push-to-api
  :finalize #'org-canvas--new-quiz-finalize
  :hash-extra #'org-canvas--new-quiz-items-digest
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
    (when (eq response 'duplicate)
      (user-error "New Quiz '%s' is already on Canvas; stamp CANVAS_ASSIGNMENT_ID or rename the heading"
                  (plist-get data :title)))
    (org-canvas--new-quiz-finalize data response)
    (org-canvas--sync-advance-header-from-entry)
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

;;;###autoload
(defun org-canvas-sync-new-quiz (&optional target by)
  "Sync the New Quiz heading TARGET names to Canvas (issue #287).
TARGET is the heading's exact title in the file
`org-canvas-new-quizzes-file' names, or, with BY `canvas-id', its
CANVAS_ASSIGNMENT_ID; an error names a target that matches no heading
or more than one.  Nil asks for a title (never under `noninteractive').
The at-point command does the work, so the value is not a run context
but a plist whose :outcome is `synced' — the command signals for
anything else."
  (interactive)
  (let* ((file (expand-file-name org-canvas-new-quizzes-file))
         (target (or target (org-canvas--sync-heading-ask "new-quiz" file "LEVEL=1")))
         (marker (org-canvas--sync-find-heading
                  file "LEVEL=1" target by "CANVAS_ASSIGNMENT_ID" "new-quiz")))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-canvas-sync-new-quiz-at-point)))
    (list :outcome 'synced)))

(org-canvas--sync-register-heading-fn "new-quiz" #'org-canvas-sync-new-quiz)
;;;###autoload
(defun org-canvas-pull-new-quiz (&optional target by)
  "Replace the New Quiz heading TARGET names with Canvas's version.
The pull twin of `org-canvas-sync-new-quiz' (issue #346), written by
hand since the sync is.  TARGET is the heading's exact title in the
file `org-canvas-new-quizzes-file' names, or, with BY `canvas-id', its
CANVAS_ASSIGNMENT_ID.  Never asks to confirm; saves the file and
returns a plist whose :outcome is `pulled' or `dry-run'."
  (interactive)
  (org-canvas--pull-heading-runtime
   "new-quiz" (expand-file-name org-canvas-new-quizzes-file) "LEVEL=1"
   "CANVAS_ASSIGNMENT_ID" target by))

(org-canvas--pull-register-heading-fn "new-quiz" #'org-canvas-pull-new-quiz)

;;;; Delete Functions

(defun org-canvas--new-quiz-delete-id (item)
  "Return the id a DELETE of the New Quiz ITEM names."
  (or (alist-get 'assignment_id item) (alist-get 'id item)))

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
         (deleted 0)
         (deleted-ids nil))
    (org-canvas--log-info org-canvas--logger "Found %d new-quizzes on Canvas"
      (length remote-items))
    (dolist (item remote-items)
      (let* ((id (org-canvas--new-quiz-delete-id item))
             (title (alist-get 'title item))
             (del-url (org-canvas--new-quiz-api-endpoint "quizzes/%s" id)))
        (condition-case err
            (progn
              (org-canvas-api-request 'DELETE del-url)
              (org-canvas--log-info org-canvas--logger "[Deleted] '%s' (ID: %s)" title id)
              (push (org-canvas--normalize-id id) deleted-ids)
              (setq deleted (1+ deleted)))
          (error
           (org-canvas--log-warning org-canvas--logger "[Delete Failed] '%s': %s"
             title (error-message-string err))))))
    ;; Keep the stamps of the quizzes still on Canvas (#324)
    (org-canvas--clean-local-sync-properties
     org-canvas-new-quizzes-file
     (org-canvas--delete-kept-ids remote-items
                                  #'org-canvas--new-quiz-delete-id deleted-ids)
     "CANVAS_ASSIGNMENT_ID")
    (message "New-quizzes deletion complete. %d removed." deleted)))

;;;; Pull

(defconst org-canvas--new-quiz-pulled-properties
  '("TIME_LIMIT" "SHUFFLE_ANSWERS" "ONE_AT_A_TIME"
    "ALLOWED_ATTEMPTS" "SCORING_POLICY")
  "Registered New Quiz properties a pull writes from the quiz itself.")

(defun org-canvas--new-quiz-pull-set-properties (pos quiz)
  "Set all properties on heading at POS from New Quiz API response QUIZ.
Each setting is written through `org-canvas--pull-apply-spec', so a
value Canvas cleared or turned off is deleted from a heading a
re-pull found rather than left standing (issue #320), while a field
QUIZ does not carry leaves the property alone."
  (let ((assignment-id (or (alist-get 'assignment_id quiz)
                           (alist-get 'id quiz)))
        (specs (plist-get (gethash "new-quizzes"
                                   org-canvas--property-registry)
                          :properties)))
    (org-canvas-org-save-sync-state pos assignment-id "CANVAS_ASSIGNMENT_ID")
    (dolist (spec specs)
      (when (member (plist-get spec :org-prop)
                    org-canvas--new-quiz-pulled-properties)
        (org-canvas--pull-apply-spec spec quiz pos)))))


(defun org-canvas--new-quiz-pull-items (quiz-assignment-id)
  "Fetch and insert items for QUIZ-ASSIGNMENT-ID as L2 headings."
  (condition-case err
      (let* ((url (org-canvas--new-quiz-api-endpoint
                   "quizzes/%s/items" quiz-assignment-id))
             (items (org-canvas-api-request-all-pages 'GET url)))
        (let ((quiz-pos (point)))
          (dolist (item (org-canvas--pull-sort-items items))
            (org-canvas--new-quiz-pull-insert-item item)
            (goto-char quiz-pos))))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[New Quizzes] Failed to fetch items for quiz %s (%s); items omitted from pull"
       quiz-assignment-id (error-message-string err))
     nil)))

(defun org-canvas--new-quiz-pull-instructions (quiz)
  "Write QUIZ's instructions as the text of the quiz heading at point.
The push sends the text between the heading's drawer and its first
item as `instructions' (`org-canvas--new-quiz-parse-body-text'), so
that text is what a pull replaces: never an item, never a second copy
beside it (issue #309).  The HTML goes through
`org-canvas--html-to-org-with-rewrite', so a heading in it becomes a
block, never a headline (Hard Rule 22).  Empty instructions leave the
text empty; a reply without the field leaves it as it is."
  (when-let* ((cell (assq 'instructions quiz)))
    (let* ((html (cdr cell))
           (text (and (stringp html)
                      (string-trim (org-canvas--html-to-org-with-rewrite html))))
           (bounds (org-canvas--pull-entry-text-bounds)))
      (save-excursion
        (delete-region (car bounds) (cdr bounds))
        (goto-char (car bounds))
        (insert "\n")
        ;; The blank line after the text stands even at the end of the
        ;; buffer, so items appended below it later sit where a re-pull
        ;; would put them.
        (unless (or (null text) (string-empty-p text))
          (insert "\n" text "\n\n"))))))

(defun org-canvas--new-quiz-pull-entry (quiz pos)
  "Write QUIZ, one New Quiz's API alist, into the heading at POS.
The properties, the items, fetched here, and the instructions: what
both pull paths write for one quiz.  The instructions go last, above
the items, so a first pull lays the quiz out as a re-pull would; an
item appended to a quiz with no items yet would otherwise follow its
text with no blank line.  Point is left at the heading."
  (goto-char pos)
  (org-back-to-heading t)
  (org-canvas--new-quiz-pull-set-properties (point) quiz)
  (org-canvas--new-quiz-pull-items (org-canvas--new-quiz-remote-id quiz))
  (org-canvas--new-quiz-pull-instructions quiz))

(defun org-canvas--new-quiz-pull-item (quiz pos)
  "Write QUIZ, one New Quiz's API alist, over the heading at POS.
The single-item pull of a New Quiz (issue #297): the properties, the
instructions (issue #309) and the items, through
`org-canvas--new-quiz-pull-entry', the writer the whole-file
`org-canvas-pull-new-quizzes' uses, so one quiz is written exactly as
a full pull would write it and nothing else in the file moves.  The
caller, `org-canvas--conflict-pull-local', renames the heading, stamps
CANVAS_UPDATED_AT and drops PAYLOAD_HASH."
  (save-excursion
    (org-canvas--new-quiz-pull-entry quiz pos)))

;; New Quizzes stay out of the feature registry: the drift report, the
;; orphan scan and prune would list them at the course API's endpoint.
;; This entry is read only by pull-at-point and adopt-at-point (#297).
(org-canvas-register-pull-feature
 :name "New Quizzes"
 :file-var 'org-canvas-new-quizzes-file
 ;; Adoption stamps `assignment_id' when the reply carries it, and
 ;; `id', which the quiz service makes the assignment id, otherwise:
 ;; the order `org-canvas--new-quiz-remote-id' reads them in (#309).
 :id-field '(assignment_id id) :id-property "CANVAS_ASSIGNMENT_ID"
 :title-field 'title
 :list-url-fn (lambda () (org-canvas--new-quiz-api-endpoint "quizzes"))
 :item-url-fn (lambda (id) (org-canvas--new-quiz-api-endpoint "quizzes/%s" id))
 :pull-item-fn #'org-canvas--new-quiz-pull-item
 :pull-whole-entry t)

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
    (if (zerop (length remote))
        (org-canvas--pull-emit-empty-file
         file (org-canvas--pull-label-for "new-quizzes"))
      (unless (file-exists-p file)
        (with-temp-file file (insert "")))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (let ((idless-before (org-canvas--pull-idless-entry-count
                              "CANVAS_ASSIGNMENT_ID")))
          (dolist (quiz (org-canvas--pull-sort-items remote))
            (let* ((assignment-id (or (alist-get 'assignment_id quiz)
                                      (alist-get 'id quiz)))
                   (title (alist-get 'title quiz))
                   (pos (org-canvas--pull-upsert-heading
                         file assignment-id title "CANVAS_ASSIGNMENT_ID")))
              (goto-char pos)
              (when title (org-edit-headline title))
              (org-canvas--new-quiz-pull-entry quiz pos)
              (cl-incf count)))
          (org-canvas--pull-check-entry-count
           "new quizzes" file "CANVAS_ASSIGNMENT_ID" idless-before count))
        (org-canvas--pull-write-file-header)
        (org-canvas--save-buffer)))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger "New Quizzes pull complete: %d quizzes" count)
    (message "New Quizzes pull complete: %d quizzes." count)))

(provide 'org-canvas-new-quizzes)
;;; org-canvas-new-quizzes.el ends here
