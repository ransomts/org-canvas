;;; org-canvas-new-quizzes.el --- New Quizzes Sync Pipeline for Canvas LMS -*- lexical-binding: t; -*-

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
(require 'ox-html)
(require 'cl-lib)
(require 'elog)

;;;; Configuration

(defcustom org-canvas-new-quizzes-file (org-canvas--path "new-quizzes.org")
  "Path to the new-quizzes.org file."
  :type 'file
  :group 'org-canvas)

(defvar org-canvas--new-quiz-debug-types nil
  "When non-nil, only sync item types in this list during debugging.
Set to a list of type strings like (\"choice\" \"true-false\") to restrict.
Set to nil to sync all types (normal operation).")

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
  (elog-debug org-canvas--logger "[New Quiz Parse] Starting at point %d" (point))

  (let* ((raw (org-canvas--new-quiz-read-props))
         (data (org-canvas--new-quiz-transform-props raw))
         (title (plist-get data :title))
         (canvas-id (plist-get data :canvas-id))
         (body-text (plist-get data :body-text))
         (pom (plist-get data :pom)))

    (org-canvas--require-title title pom "New Quiz")

    (elog-info org-canvas--logger "[New Quiz Parse] Quiz: '%s' (ID: %s)"
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
      (elog-info org-canvas--logger "[DRY-RUN] Would %s New Quiz '%s' to %s"
        method title endpoint)
      (cl-return-from org-canvas--new-quiz-push-to-api
        '((assignment_id . "dry-run"))))

    (elog-info org-canvas--logger "[New Quiz API] %s '%s'" method title)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data wrapped)))
          (elog-info org-canvas--logger "[New Quiz API] %s successful for '%s'"
            method title)
          response)
      (error
       (elog-error org-canvas--logger "[New Quiz API] Failed: %s"
         (error-message-string err))
       (cond
        ;; 404 on PATCH -> retry as POST (stale ID)
        ((and (eq method 'PATCH)
              (org-canvas--404-error-p err))
         (elog-warning org-canvas--logger
           "[Recovery] Item not found (404). Retrying as POST...")
         (condition-case post-err
             (let ((response (org-canvas-api-request
                              'POST
                              (org-canvas--new-quiz-api-endpoint "quizzes")
                              :data wrapped)))
               (elog-info org-canvas--logger "[Recovery] POST successful")
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

    (elog-debug org-canvas--logger "[New Quiz Item Parse] '%s' type=%s" title q-type)

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

    (elog-info org-canvas--logger "[New Quiz Item API] %s '%s'" method title)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data wrapped)))
          (elog-info org-canvas--logger "[New Quiz Item API] %s successful for '%s'"
            method title)
          response)
      (error
       (elog-error org-canvas--logger "[New Quiz Item API] Failed: %s"
         (error-message-string err))
       (cond
        ;; 404 on PATCH -> retry as POST (stale CANVAS_ITEM_ID)
        ((and (eq method 'PATCH)
              (org-canvas--404-error-p err))
         (elog-warning org-canvas--logger
           "[Recovery] Item not found (404). Retrying as POST...")
         (condition-case post-err
             (let ((response (org-canvas-api-request
                              'POST
                              (org-canvas--new-quiz-api-endpoint
                               "quizzes/%s/items" quiz-id)
                              :data wrapped)))
               (elog-info org-canvas--logger "[Recovery] POST successful")
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
          (elog-info org-canvas--logger
            "[Finalize] Saved CANVAS_ITEM_ID=%s for '%s'" id title))
      (elog-warning org-canvas--logger
        "[Finalize] No id in response for item '%s'" title))))

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
                      (elog-info org-canvas--logger
                        "[DEBUG SKIP] Skipping type '%s' for '%s'"
                        q-type (plist-get data :title))
                      (setq item-skipped (1+ item-skipped)))
                  (let* ((payload (org-canvas--new-quiz-item-build-payload data))
                         (response (org-canvas--new-quiz-item-push-to-api data payload)))
                    (org-canvas--new-quiz-item-finalize data response)
                    (setq item-success (1+ item-success)))))
            (error
             (elog-error org-canvas--logger "[New Quiz Item] Failed: %s"
               (error-message-string err)))))))

    ;; Release markers to avoid memory leaks
    (dolist (m item-markers) (set-marker m nil))

    (when (> item-skipped 0)
      (elog-info org-canvas--logger "[New Quiz Items] %d skipped (debug filter)"
        item-skipped))
    (elog-info org-canvas--logger "[New Quiz Items] %d/%d synced"
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
    (save-buffer)
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
  (elog-warning org-canvas--logger "========================================")
  (elog-warning org-canvas--logger ">>> STARTING MASS DELETION OF NEW-QUIZZES")
  (elog-warning org-canvas--logger "========================================")
  (let* ((endpoint (org-canvas--new-quiz-api-endpoint "quizzes"))
         (remote-items (org-canvas-api-request-all-pages 'GET endpoint))
         (deleted 0))
    (elog-info org-canvas--logger "Found %d new-quizzes on Canvas"
      (length remote-items))
    (dolist (item remote-items)
      (let* ((id (or (alist-get 'assignment_id item)
                     (alist-get 'id item)))
             (title (alist-get 'title item))
             (del-url (org-canvas--new-quiz-api-endpoint "quizzes/%s" id)))
        (condition-case err
            (progn
              (org-canvas-api-request 'DELETE del-url)
              (elog-info org-canvas--logger "[Deleted] '%s' (ID: %s)" title id)
              (setq deleted (1+ deleted)))
          (error
           (elog-warning org-canvas--logger "[Delete Failed] '%s': %s"
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

(defun org-canvas--new-quiz-pull-items (quiz-assignment-id)
  "Fetch and insert items for QUIZ-ASSIGNMENT-ID as L2 headings."
  (condition-case nil
      (let* ((url (org-canvas--new-quiz-api-endpoint
                   "quizzes/%s/items" quiz-assignment-id))
             (items (org-canvas-api-request-all-pages 'GET url)))
        (dolist (item items)
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
         (count 0))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (find-file-noselect file)
      (dolist (quiz remote)
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
      (save-buffer))
    (elog-info org-canvas--logger "New Quizzes pull complete: %d quizzes" count)
    (message "New Quizzes pull complete: %d quizzes." count)))

(provide 'org-canvas-new-quizzes)
;;; org-canvas-new-quizzes.el ends here
