;;; org-canvas-new-quiz-items.el --- New Quizzes item/question pipeline -*- lexical-binding: t; -*-

;;; Commentary:

;; The question/item layer of the New Quizzes sync pipeline.
;;
;; A Canvas "New Quiz" is a container identified by an assignment_id.
;; Each quiz contains items (questions) that are synced via a separate
;; endpoint (`quizzes/:quiz_id/items'), with their own parse/build/push/
;; finalize pipeline and a rich set of interaction types (multiple choice,
;; matching, ordering, categorization, numerical, fill-in-the-blank, etc.).
;;
;; This module provides the item-level primitives.  Orchestration lives in
;; `org-canvas-new-quizzes'.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)

;;;; API Helper

(defun org-canvas--new-quiz-api-endpoint (suffix &rest args)
  "Construct a New Quizzes API endpoint URL.
SUFFIX is the path after /courses/:id/.  ARGS are format arguments.
New Quizzes use /api/quiz/v1/ instead of /api/v1/."
  (format "%s/api/quiz/v1/courses/%s/%s"
          org-canvas-base-url
          org-canvas-course-id
          (apply #'format suffix args)))

;;;; Type Slug Mapping

(defconst org-canvas--new-quiz-type-slugs
  '(("choice"             . "choice")
    ("true-false"          . "true-false")
    ("multi-answer"        . "multi-answer")
    ("essay"               . "essay")
    ("short-answer"        . "rich-fill-blank")
    ("file-upload"         . "file-upload")
    ("numerical"           . "numeric")
    ("matching"            . "matching")
    ("ordering"            . "ordering")
    ("categorization"      . "categorization")
    ("hot-spot"            . "hot-spot"))
  "Map from Org TYPE property values to New Quizzes interaction_type_slug.")


(defconst org-canvas--new-quiz-scoring-algorithms
  '(("choice"             . "Equivalence")
    ("true-false"          . "Equivalence")
    ("multi-answer"        . "PartialScore")
    ("matching"            . "PartialDeep")
    ("ordering"            . "DeepEquals")
    ("categorization"      . "Categorization")
    ("numerical"           . "Numeric")
    ("short-answer"        . "MultipleMethods")
    ("essay"               . "None")
    ("file-upload"         . "None")
    ("hot-spot"            . "None"))
  "Map from Org TYPE property values to New Quizzes scoring_algorithm.")

(defun org-canvas--new-quiz-item-scoring-algorithm (q-type)
  "Return the scoring_algorithm string for Q-TYPE."
  (or (cdr (assoc q-type org-canvas--new-quiz-scoring-algorithms))
      "Equivalence"))

;;;; Helper Functions

(defun org-canvas--new-quiz-uuid ()
  "Generate a random hex ID string for New Quiz item IDs.
Uses plain hex without dashes because Canvas normalizes JSON object
keys by replacing dashes with underscores, which breaks key lookups
when IDs are used as both hash-table keys and values."
  (format "%08x%04x%04x%04x%012x"
          (random (expt 16 8))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 12))))

(defun org-canvas--new-quiz-numeric-id ()
  "Generate a random short numeric ID string for matching questions.
Canvas uses short numeric strings like \"87146\" for matching question IDs."
  (format "%d" (+ 10000 (random 90000))))

(defun org-canvas--new-quiz-parse-body-text ()
  "Get the body text of current heading, excluding subheadings.
Returns the text between the current heading and the first subheading."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (save-excursion
                   (org-end-of-meta-data t)
                   (point)))
          (end (save-excursion
                 (outline-next-heading)
                 (point))))
      (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
        (when (> end subtree-end)
          (setq end subtree-end)))
      (string-trim (buffer-substring-no-properties start end)))))

(defun org-canvas--new-quiz-parse-question-text ()
  "Get the question prompt text, excluding answer lists.
Returns only the text before the first list item (- or *)."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (save-excursion
                   (org-end-of-meta-data t)
                   (point)))
          (end (save-excursion
                 (outline-next-heading)
                 (point))))
      (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
        (when (> end subtree-end)
          (setq end subtree-end)))
      (if (>= start end)
          ""
        (goto-char start)
        (when (re-search-forward "^[ \t]*[-*+] " end t)
          (setq end (match-beginning 0)))
        (string-trim (buffer-substring-no-properties start end))))))

(defun org-canvas--new-quiz-parse-checkbox-list ()
  "Parse a checkbox list under point, returning answer data.
Returns a list of (text . is-correct) pairs."
  (save-excursion
    (let ((answers nil)
          (bound (save-excursion (org-end-of-subtree t) (point))))
      (while (re-search-forward "^[ \t]*- \\(\\[[ X]\\]\\) \\(.+\\)$" bound t)
        (let ((checkbox (match-string 1))
              (text (string-trim (match-string 2))))
          (push (cons text (string= checkbox "[X]")) answers)))
      (nreverse answers))))

(defun org-canvas--new-quiz-parse-matching-list ()
  "Parse matching question format: left = right.
Returns list of (left . right) pairs."
  (save-excursion
    (let ((matches nil)
          (bound (save-excursion (org-end-of-subtree t) (point))))
      (while (re-search-forward "^- \\(.+?\\) = \\(.+\\)$" bound t)
        (push (cons (string-trim (match-string 1))
                    (string-trim (match-string 2)))
              matches))
      (nreverse matches))))

(defun org-canvas--new-quiz-parse-ordering-list ()
  "Parse ordering question format: numbered list items.
Returns list of items in order."
  (save-excursion
    (let ((items nil)
          (bound (save-excursion (org-end-of-subtree t) (point))))
      (while (re-search-forward "^[ \t]*[0-9]+\\.[ \t]+\\(.+\\)$" bound t)
        (push (string-trim (match-string 1)) items))
      (nreverse items))))

(defun org-canvas--new-quiz-parse-categorization-list ()
  "Parse categorization question format: Category: item1, item2.
Returns alist of (category . (item1 item2 ...))."
  (save-excursion
    (let ((categories nil)
          (bound (save-excursion (org-end-of-subtree t) (point))))
      (while (re-search-forward "^- \\(.+?\\): \\(.+\\)$" bound t)
        (let ((category (string-trim (match-string 1)))
              (items (split-string (match-string 2) "," t "[ \t]+")))
          (push (cons category items) categories)))
      (nreverse categories))))

(defun org-canvas--new-quiz-parse-numerical-answer ()
  "Parse numerical answer format.
Supports: exact value, or [min, max] range."
  (save-excursion
    (let ((bound (save-excursion (org-end-of-subtree t) (point))))
      (when (re-search-forward "^- \\[X\\] \\(.+\\)$" bound t)
        (let ((text (string-trim (match-string 1))))
          (if (string-match "\\[\\([0-9.-]+\\),[ ]*\\([0-9.-]+\\)\\]" text)
              (list :type 'range
                    :start (string-to-number (match-string 1 text))
                    :end (string-to-number (match-string 2 text)))
            (list :type 'exact
                  :value (string-to-number text))))))))

;;;; Item/Question Parsing (Level 2)

(defun org-canvas--new-quiz-item-read-props (quiz-assignment-id)
  "Read raw property strings from the quiz item heading at point.
QUIZ-ASSIGNMENT-ID is the assignment ID of the parent quiz.
Returns a plist of raw values with no transformations applied."
  (let* ((pom (point-marker))
         (title-raw (org-get-heading t t t t))
         (canvas-id (org-entry-get pom "CANVAS_ITEM_ID"))
         (type-raw (org-entry-get pom "TYPE"))
         (points-raw (org-entry-get pom "POINTS"))
         (outcome (org-entry-get pom "OUTCOME"))
         (body-text (org-canvas--new-quiz-parse-question-text)))
    (list :title-raw title-raw
          :canvas-id canvas-id
          :quiz-assignment-id quiz-assignment-id
          :type-raw type-raw
          :points-raw points-raw
          :outcome outcome
          :text body-text
          :pom pom)))

(defun org-canvas--new-quiz-item-transform-props (props)
  "Apply pure transformations to raw item PROPS plist.
No buffer access — only string/number/boolean conversions."
  (let* ((title (org-canvas--strip-statistics-cookie
                 (plist-get props :title-raw)))
         (q-type (org-canvas--validate-property
                  (plist-get props :type-raw)
                  org-canvas--valid-new-quiz-types
                  "TYPE" "choice"))
         (points-raw (plist-get props :points-raw))
         (points (if (and points-raw (not (string-empty-p points-raw)))
                     (org-canvas--safe-string-to-number points-raw "POINTS")
                   1)))
    (list :title title
          :text (plist-get props :text)
          :canvas-id (plist-get props :canvas-id)
          :quiz-assignment-id (plist-get props :quiz-assignment-id)
          :type q-type
          :points points
          :outcome (plist-get props :outcome)
          :pom (plist-get props :pom))))

(defun org-canvas--new-quiz-item-parse-entry (quiz-assignment-id)
  "Extract item data from Org heading at point.
QUIZ-ASSIGNMENT-ID is the assignment ID of the parent quiz.
Reads raw properties and transforms them."
  (org-back-to-heading t)

  (let* ((raw (org-canvas--new-quiz-item-read-props quiz-assignment-id))
         (data (org-canvas--new-quiz-item-transform-props raw))
         (title (plist-get data :title))
         (q-type (plist-get data :type))
         (question-text (let ((body (or (plist-get data :text) "")))
                          (if (string-empty-p body)
                              title
                            (concat title "\n\n" body))))
         (text-html (let ((org-export-with-sub-superscripts nil))
                      (org-export-string-as question-text 'html t)))
         (interaction-data (org-canvas--new-quiz-item-build-interaction-data q-type)))

    (org-canvas--log-debug org-canvas--logger "[New Quiz Item Parse] '%s' type=%s" title q-type)

    (plist-put data :text-html text-html)
    (plist-put data :interaction-data interaction-data)
    data))

;;;; Item Build Interaction Data

(defun org-canvas--new-quiz-item-build-choice-data (answers)
  "Build interaction_data for choice question from ANSWERS.
Returns alist with `choices' array and `_correct_ids' list for scoring.
Canvas stores choices as an array of objects with UUID IDs."
  (let* ((correct-ids nil)
         (choices (cl-loop for (text . correct) in answers
                           for i from 0
                           for id = (org-canvas--new-quiz-uuid)
                           when correct do (push id correct-ids)
                           collect `((id . ,id)
                                     (position . ,(1+ i))
                                     (item_body . ,(format "<p>%s</p>" text))))))
    `((choices . ,(vconcat choices))
      (_correct_ids . ,(nreverse correct-ids)))))

(defun org-canvas--new-quiz-item-build-true-false-data (answers)
  "Build interaction_data for true/false question from ANSWERS.
Canvas expects true_choice/false_choice as plain strings and
scoring_data value as boolean."
  (let ((true-correct nil))
    (dolist (ans answers)
      (when (cdr ans)
        (setq true-correct (string= (downcase (car ans)) "true"))))
    `((true_choice . "True")
      (false_choice . "False")
      (scoring_data . ((value . ,(if true-correct t :json-false)))))))

(defun org-canvas--new-quiz-item-build-matching-data (pairs)
  "Build interaction_data for matching question from PAIRS.
Canvas expects `questions' (array of objects with short numeric IDs)
and `answers' (flat string array).
Scoring maps question_id → answer text."
  (let ((questions nil)
        (answers nil))
    (cl-loop for (left . right) in pairs
             do (progn
                  (push `((id . ,(org-canvas--new-quiz-numeric-id))
                          (item_body . ,left))
                        questions)
                  (push right answers)))
    `((questions . ,(vconcat (nreverse questions)))
      (answers . ,(vconcat (nreverse answers))))))

(defun org-canvas--new-quiz-item-build-ordering-data (items)
  "Build interaction_data for ordering question from ITEMS.
Canvas stores ordering choices as a keyed object (hash-table),
not an array.  Each key is a UUID, value is an alist with id + item_body."
  (let ((choices-ht (make-hash-table :test 'equal))
        (ordered-ids nil))
    (dolist (item items)
      (let ((id (org-canvas--new-quiz-uuid)))
        (push id ordered-ids)
        (puthash id `((id . ,id) (item_body . ,item)) choices-ht)))
    (setq ordered-ids (nreverse ordered-ids))
    `((choices . ,choices-ht)
      (scoring_data . ((value . ,(vconcat ordered-ids)))))))

(defun org-canvas--new-quiz-item-build-categorization-data (categories)
  "Build interaction_data for categorization question from CATEGORIES.
Canvas expects categories and distractors as keyed objects (hash-tables),
not arrays.  category_order is an array of category IDs."
  (let ((cats-ht (make-hash-table :test 'equal))
        (distractors-ht (make-hash-table :test 'equal))
        (cat-order nil)
        (all-items nil))
    (cl-loop for (cat . items) in categories
             do (let ((cat-id (org-canvas--new-quiz-uuid)))
                  (push cat-id cat-order)
                  (puthash cat-id `((id . ,cat-id) (item_body . ,cat)) cats-ht)
                  (cl-loop for item in items
                           do (let ((item-id (org-canvas--new-quiz-uuid)))
                                (puthash item-id
                                         `((id . ,item-id) (item_body . ,item))
                                         distractors-ht)
                                (push `((id . ,item-id)
                                        (item_body . ,item)
                                        (scoring_data . ((value . ,cat-id))))
                                      all-items)))))
    (setq cat-order (nreverse cat-order))
    ;; Return alist with hash-table categories/distractors + array cat-order
    ;; Also include _flat-distractors for scoring_data builder
    `((categories . ,cats-ht)
      (distractors . ,distractors-ht)
      (category_order . ,(vconcat cat-order))
      (_flat_distractors . ,(vconcat (nreverse all-items))))))

(defun org-canvas--new-quiz-item-build-numerical-data (num)
  "Build interaction_data for numerical question from NUM plist.
Canvas numeric scoring_data.value is an array of answer objects:
  exactResponse: {id, type, value}
  withinARange:  {id, type, start, end}"
  (if (eq (plist-get num :type) 'range)
      `((scoring_data
         . ((value . ,(vector `((id . "1")
                                (type . "withinARange")
                                (start . ,(number-to-string (plist-get num :start)))
                                (end . ,(number-to-string (plist-get num :end)))))))))
    `((scoring_data
       . ((value . ,(vector `((id . "1")
                               (type . "exactResponse")
                               (value . ,(number-to-string
                                          (plist-get num :value)))))))))))

(defun org-canvas--new-quiz-item-build-fill-blank-data (answers)
  "Build interaction_data for short-answer from ANSWERS.
One scoring entry per correct answer, each with id, nested
scoring_data (string value), and scoring_algorithm Equivalence."
  (let* ((blank-id (org-canvas--new-quiz-uuid))
         (entries (cl-loop for (text . is-correct) in answers
                           when is-correct
                           collect `((id . ,blank-id)
                                     (scoring_data . ((value . ,text)))
                                     (scoring_algorithm . "Equivalence")))))
    `((blanks . ,(vector `((id . ,blank-id))))
      (scoring_data
       . ((value . ,(vconcat entries)))))))

(defconst org-canvas--new-quiz-interaction-dispatch
  `(("choice"         ,#'org-canvas--new-quiz-parse-checkbox-list       . ,#'org-canvas--new-quiz-item-build-choice-data)
    ("true-false"     ,#'org-canvas--new-quiz-parse-checkbox-list       . ,#'org-canvas--new-quiz-item-build-true-false-data)
    ("multi-answer"   ,#'org-canvas--new-quiz-parse-checkbox-list       . ,#'org-canvas--new-quiz-item-build-choice-data)
    ("short-answer"   ,#'org-canvas--new-quiz-parse-checkbox-list       . ,#'org-canvas--new-quiz-item-build-fill-blank-data)
    ("matching"       ,#'org-canvas--new-quiz-parse-matching-list       . ,#'org-canvas--new-quiz-item-build-matching-data)
    ("ordering"       ,#'org-canvas--new-quiz-parse-ordering-list       . ,#'org-canvas--new-quiz-item-build-ordering-data)
    ("categorization" ,#'org-canvas--new-quiz-parse-categorization-list . ,#'org-canvas--new-quiz-item-build-categorization-data)
    ("numerical"      ,#'org-canvas--new-quiz-parse-numerical-answer    . ,#'org-canvas--new-quiz-item-build-numerical-data))
  "Dispatch table for new quiz interaction data: (TYPE PARSER . BUILDER).")

(defun org-canvas--new-quiz-item-build-interaction-data (q-type)
  "Build interaction_data for Q-TYPE from current heading content.
Returns an alist that will be JSON-encoded."
  (when-let* ((entry (assoc q-type org-canvas--new-quiz-interaction-dispatch #'equal)))
    (let* ((parser (cadr entry))
           (builder (cddr entry))
           (parsed (funcall parser)))
      (when parsed
        (funcall builder parsed)))))

;;;; Item Scoring Data

(defun org-canvas--new-quiz-item-build-scoring-data (q-type interaction-data)
  "Build top-level scoring_data for Q-TYPE from INTERACTION-DATA.
Returns (SCORING-DATA . CLEANED-INTERACTION-DATA) where
CLEANED-INTERACTION-DATA has embedded scoring_data removed to
avoid duplication in the API payload."
  (pcase q-type
    ;; Types with top-level scoring_data in interaction-data: extract and remove
    ((or "true-false" "ordering" "numerical" "short-answer")
     (let ((sd (alist-get 'scoring_data interaction-data)))
       (cons sd (assq-delete-all 'scoring_data interaction-data))))

    ;; Choice: extract single correct ID from _correct_ids, then remove it
    ("choice"
     (let ((correct-id (car (alist-get '_correct_ids interaction-data))))
       (setq interaction-data
             (cl-remove-if (lambda (pair) (eq (car pair) '_correct_ids))
                           interaction-data))
       (cons `((value . ,correct-id)) interaction-data)))

    ;; Multi-answer: extract all correct IDs from _correct_ids, then remove it
    ("multi-answer"
     (let ((ids (alist-get '_correct_ids interaction-data)))
       (setq interaction-data
             (cl-remove-if (lambda (pair) (eq (car pair) '_correct_ids))
                           interaction-data))
       (cons `((value . ,(vconcat ids))) interaction-data)))

    ;; Matching: build question_id → answer_text map
    ("matching"
     (let ((value (make-hash-table :test 'equal))
           (questions (append (alist-get 'questions interaction-data) nil))
           (answers (append (alist-get 'answers interaction-data) nil)))
       (cl-loop for q in questions
                for ans in answers
                do (puthash (alist-get 'id q) ans value))
       (cons `((value . ,value)) interaction-data)))

    ;; Categorization: build per-category scoring from _flat_distractors
    ("categorization"
     (let ((cat-items (make-hash-table :test 'equal))
           cat-scoring)
       ;; Collect which distractor IDs belong to which category
       (dolist (item (append (alist-get '_flat_distractors interaction-data) nil))
         (let ((cat-id (alist-get 'value (alist-get 'scoring_data item))))
           (push (alist-get 'id item) (gethash cat-id cat-items))))
       ;; Build per-category scoring entries in category_order
       (dolist (cid (append (alist-get 'category_order interaction-data) nil))
         (push `((id . ,cid)
                 (scoring_data
                  . ((value . ,(vconcat (nreverse (gethash cid cat-items))))))
                 (scoring_algorithm . "AllOrNothing"))
               cat-scoring))
       ;; Remove _flat_distractors from interaction-data (internal only)
       (setq interaction-data
             (cl-remove-if (lambda (pair) (eq (car pair) '_flat_distractors))
                           interaction-data))
       (cons `((value . ,(vconcat (nreverse cat-scoring)))) interaction-data)))

    ;; Essay, file-upload, hot-spot, unknown
    (_
     (cons `((value . "")) interaction-data))))

;;;; Item Build Payload

(defun org-canvas--new-quiz-item-build-payload (data)
  "Build Canvas API payload from New Quiz item DATA (pure, no buffer access)."
  (let* ((q-type (plist-get data :type))
         (slug (or (cdr (assoc q-type org-canvas--new-quiz-type-slugs)) q-type))
         (text-html (plist-get data :text-html))
         (interaction-data (plist-get data :interaction-data))
         (scoring-result (org-canvas--new-quiz-item-build-scoring-data
                          q-type interaction-data))
         (scoring-data (car scoring-result))
         (interaction-data (cdr scoring-result))
         (scoring-algorithm (org-canvas--new-quiz-item-scoring-algorithm q-type))
         (payload (make-hash-table :test 'equal)))

    (puthash "item_body" text-html payload)
    (puthash "interaction_type_slug" slug payload)
    (puthash "points_possible" (plist-get data :points) payload)
    (puthash "entry_type" "Item" payload)
    (puthash "scoring_data" scoring-data payload)
    (puthash "scoring_algorithm" scoring-algorithm payload)

    (when interaction-data
      (puthash "interaction_data" interaction-data payload))

    payload))

;;;; Item Push to API

(defun org-canvas--new-quiz-item-wrap-payload (payload)
  "Restructure flat PAYLOAD into nested {item: {entry: ...}} format."
  (let ((entry (make-hash-table :test 'equal))
        (item (make-hash-table :test 'equal))
        (wrapped (make-hash-table :test 'equal)))
    ;; Entry-level fields (inside item.entry)
    (puthash "item_body" (gethash "item_body" payload) entry)
    (puthash "interaction_type_slug" (gethash "interaction_type_slug" payload) entry)
    (when (gethash "interaction_data" payload)
      (puthash "interaction_data" (gethash "interaction_data" payload) entry))
    (when (gethash "scoring_data" payload)
      (puthash "scoring_data" (gethash "scoring_data" payload) entry))
    (when (gethash "scoring_algorithm" payload)
      (puthash "scoring_algorithm" (gethash "scoring_algorithm" payload) entry))
    ;; Item-level fields
    (puthash "entry_type" (gethash "entry_type" payload) item)
    (puthash "points_possible" (gethash "points_possible" payload) item)
    (puthash "entry" entry item)
    (puthash "item" item wrapped)
    wrapped))

(cl-defun org-canvas--new-quiz-item-push-to-api (data payload)
  "Send New Quiz item PAYLOAD (from DATA) to Canvas API.
PAYLOAD is the flat item data from `build-payload'.  It is restructured
into the nested format required by the New Quizzes Items API:
  {\"item\": {\"entry_type\": ..., \"points_possible\": ...,
              \"entry\": {\"item_body\": ..., \"interaction_type_slug\": ..., ...}}}"
  (let* ((quiz-id (plist-get data :quiz-assignment-id))
         (item-id (plist-get data :canvas-id))
         (title (plist-get data :title))
         (method (if item-id 'PATCH 'POST))
         (endpoint (if item-id
                       (org-canvas--new-quiz-api-endpoint
                        "quizzes/%s/items/%s" quiz-id item-id)
                     (org-canvas--new-quiz-api-endpoint
                      "quizzes/%s/items" quiz-id)))
         (wrapped (org-canvas--new-quiz-item-wrap-payload payload)))

    (org-canvas--log-info org-canvas--logger "[New Quiz Item API] %s '%s'" method title)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data wrapped)))
          (org-canvas--log-info org-canvas--logger "[New Quiz Item API] %s successful for '%s'"
            method title)
          response)
      (error
       (org-canvas--log-error org-canvas--logger "[New Quiz Item API] Failed: %s"
         (error-message-string err))
       (cond
        ;; 404 on PATCH -> retry as POST (stale CANVAS_ITEM_ID)
        ((and (eq method 'PATCH)
              (org-canvas--404-error-p err))
         (org-canvas--log-warning org-canvas--logger
           "[Recovery] Item not found (404). Retrying as POST...")
         (condition-case post-err
             (let ((response (org-canvas-api-request
                              'POST
                              (org-canvas--new-quiz-api-endpoint
                               "quizzes/%s/items" quiz-id)
                              :data wrapped)))
               (org-canvas--log-info org-canvas--logger "[Recovery] POST successful")
               response)
           (error
            (signal (car post-err) (cdr post-err)))))
        (t (signal (car err) (cdr err))))))))

;;;; Item Finalize

(defun org-canvas--new-quiz-item-finalize (data response)
  "Save New Quiz item from DATA with CANVAS_ITEM_ID from RESPONSE."
  (let* ((id (alist-get 'id response))
         (pom (plist-get data :pom))
         (title (plist-get data :title)))
    (if id
        (progn
          (org-canvas-org-save-sync-state pom id "CANVAS_ITEM_ID")
          (org-canvas--log-info org-canvas--logger
            "[Finalize] Saved CANVAS_ITEM_ID=%s for '%s'" id title))
      (org-canvas--log-warning org-canvas--logger
        "[Finalize] No id in response for item '%s'" title))))

(defun org-canvas--new-quiz-slug-to-type (slug)
  "Convert an interaction_type_slug SLUG to the Org TYPE value."
  (or (car (cl-rassoc slug org-canvas--new-quiz-type-slugs :test #'string=))
      slug))

(defun org-canvas--new-quiz-pull-insert-item (item)
  "Insert a single New Quiz ITEM as an L2 heading under the current quiz."
  (let ((title (or (alist-get 'item_body item) "Question"))
        (slug (alist-get 'interaction_type_slug item))
        (points (alist-get 'points_possible item))
        (item-id (alist-get 'id item)))
    (setq title (org-canvas--html-to-org-inline title))
    (when (string-empty-p title)
      (setq title "Question"))
    (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
      (goto-char subtree-end)
      (unless (bolp) (insert "\n"))
      (insert (format "** %s\n" title))
      (org-back-to-heading t)
      (let ((qpos (point)))
        (when item-id
          (org-canvas-org-save-sync-state qpos (format "%s" item-id) "CANVAS_ITEM_ID"))
        (when slug
          (org-canvas-org-set-property
           qpos "TYPE" (org-canvas--new-quiz-slug-to-type slug)))
        (when points
          (org-canvas-org-set-property
           qpos "POINTS" (format "%s" points)))))))


(provide 'org-canvas-new-quiz-items)
;;; org-canvas-new-quiz-items.el ends here
