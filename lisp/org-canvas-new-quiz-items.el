;;; org-canvas-new-quiz-items.el --- New Quizzes item/question pipeline -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The question/item layer of the New Quizzes sync pipeline.
;;
;; A Canvas "New Quiz" is a container identified by an assignment_id.
;; Each quiz contains items (questions) that are synced via a separate
;; endpoint (`quizzes/:quiz_id/items'), with their own parse/build/push/
;; finalize pipeline and a rich set of interaction types (multiple choice,
;; matching, ordering, categorization, numerical, short answer, etc.).
;;
;; This file provides the item-level primitives.  Orchestration lives in
;; `org-canvas-new-quizzes', which requires this file — the one sanctioned
;; feature-to-feature require in the package.  This is a sub-module, not
;; a feature: it registers nothing, defines no sync command, and requires
;; only `org-canvas-core', so it can never form a cycle.  It stays out of
;; core because everything in it is New Quizzes vocabulary (interaction
;; types, scoring data, the /api/quiz/v1/ endpoints) that no other
;; feature reads; it is a separate file only to keep the quiz container
;; and its questions each under a readable length.

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

;; A deleted New Quiz's items go with it: a delete clears the ids (#331).
(org-canvas-register-id-property "CANVAS_ITEM_ID")

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
    ("hot-spot"            . "HotSpot"))
  "Map from Org TYPE property values to New Quizzes scoring_algorithm.
Hot-spot is Canvas's \"HotSpot\", as a live item reads (issue #365),
though a push still refuses the type (issue #340).")

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

(defconst org-canvas--new-quiz-prompt-end-regexp "^[ \t]*[-*+] "
  "Regexp for the first answer line of an item, ending its prompt.")

(defconst org-canvas--new-quiz-ordering-prompt-end-regexp
  "^[ \t]*\\(?:[-*+] \\|[0-9]+\\.[ \t]\\)"
  "Regexp for the first answer line of an ordering item.
An ordering item's answers are a numbered list, in the correct order
\(`org-canvas--new-quiz-parse-ordering-list'), so a numbered line ends
its prompt as well as a bullet does (issue #335).  Other types keep a
numbered list in their prompt.")

(defconst org-canvas--new-quiz-prompt-only-types
  '("essay" "file-upload")
  "TYPE values whose items have no answer list after the prompt.
Such an item's prompt is all the text under its heading, bulleted
lists included (issue #337).  Hot-spot is not here: a push refuses
it (issue #340), so how its prompt ends is decided with its regions.")

(defconst org-canvas--new-quiz-unsupported-types
  '(("fill-in-the-blank"
     . "is not supported for New Quiz items; use short-answer")
    ("hot-spot"
     . "is not pushed: its image regions cannot be built from Org; \
edit the item in Canvas"))
  "TYPE values refused at the push, each with the reason given.
Canvas's editor needs a `working_item_body' with backtick-delimited
blanks that the API cannot set reliably (see the manual's New Quizzes
section), so fill-in-the-blank has no mapping (issue #337).  A
hot-spot item's image lives in the quiz service's media store, whose
upload was never probed, so the push cannot build one, and pushing its
prompt alone would strip the regions Canvas holds (issue #340).  A
pull writes the regions read-only (issue #365).")

(defun org-canvas--new-quiz-prompt-end-regexp-for (q-type)
  "Return the regexp of the line an item of Q-TYPE ends its prompt at.
Nil for a type in `org-canvas--new-quiz-prompt-only-types', whose
prompt is all its text (issue #337); an ordering item's numbered
answers end it as well as a bullet (issue #335)."
  (cond ((member q-type org-canvas--new-quiz-prompt-only-types) nil)
        ((equal q-type "ordering")
         org-canvas--new-quiz-ordering-prompt-end-regexp)
        (t org-canvas--new-quiz-prompt-end-regexp)))

(defun org-canvas--new-quiz-item-prompt-bounds (&optional q-type)
  "Return (START . END) of the prompt of the item heading at point.
START is the end of the heading's drawer, as
`org-canvas--pull-entry-text-bounds' finds it; END is the first line
of the answer list, as Q-TYPE ends it
\(`org-canvas--new-quiz-prompt-end-regexp-for'), else the first child
heading or the end of the subtree.  The push reads this region as the
prompt and the pull writes it (issue #333)."
  (let* ((bounds (org-canvas--pull-entry-text-bounds))
         (start (car bounds))
         (end (cdr bounds))
         (re (org-canvas--new-quiz-prompt-end-regexp-for q-type)))
    (save-excursion
      (goto-char start)
      (cons start
            (if (and re (re-search-forward re end t))
                (match-beginning 0)
              end)))))

(defun org-canvas--new-quiz-parse-question-text (&optional q-type)
  "Get the question prompt text, excluding answer lists.
Return only the text before the first list item (- or *), or, when
Q-TYPE is \"ordering\", before the first numbered item as well.  A
Q-TYPE in `org-canvas--new-quiz-prompt-only-types' has no answer
list, so its prompt is all the text before the first child heading."
  (let ((bounds (org-canvas--new-quiz-item-prompt-bounds q-type)))
    (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))))

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

(defun org-canvas--new-quiz-item-question-text (title body)
  "Return the Org text of an item's pushed body: TITLE, then BODY.
The heading is the body's first paragraph, which is what an unstamped
heading is paired on (`org-canvas--new-quiz-item-twin-p'); BODY, the
prompt above the answer list, follows when there is one."
  (let ((body (or body "")))
    (if (string-empty-p body)
        title
      (concat title "\n\n" body))))

(defun org-canvas--new-quiz-item-body-html ()
  "Return the HTML the item heading at point would push as its body.
The drift report's body extractor, named by the item registration's
`:body-fn' (issue #322): the heading and the prompt, exported as
`org-canvas--new-quiz-item-parse-entry' exports them, with no answer
list and nothing uploaded.  The prompt ends where the item's TYPE says
its answers begin, as the parse reads it: an ordering item's at a
numbered line (issue #335), an essay's nowhere (issue #337).  The TYPE
is read raw, not checked, so an item a push would refuse (issue #340)
is still compared."
  (org-back-to-heading t)
  (org-canvas--org-to-html-string
   (org-canvas--new-quiz-item-question-text
    (org-canvas--strip-statistics-cookie (org-get-heading t t t t))
    (org-canvas--new-quiz-parse-question-text (org-entry-get nil "TYPE")))))

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
         (body-text (org-canvas--new-quiz-parse-question-text type-raw)))
    (list :title-raw title-raw
          :canvas-id canvas-id
          :quiz-assignment-id quiz-assignment-id
          :type-raw type-raw
          :points-raw points-raw
          :outcome outcome
          :text body-text
          :pom pom)))

(defun org-canvas--new-quiz-item-check-supported (type-raw)
  "Return the item type TYPE-RAW names, or signal when it names none.
Nil TYPE-RAW is the default type, choice.  A type in
`org-canvas--new-quiz-unsupported-types' signals with its reason, and
any other value outside `org-canvas--valid-new-quiz-types' (a typo
such as mutliple-choice) signals naming the valid ones: falling back
to choice would push an item with no answers (issue #340).  The
signal is an `org-canvas-validation-error', so the item fails and the
rest of the quiz's items still sync."
  (let ((reason (cdr (assoc type-raw
                            org-canvas--new-quiz-unsupported-types))))
    (cond
     ((null type-raw) "choice")
     (reason
      (org-canvas--signal 'org-canvas-validation-error
        "TYPE %s %s" type-raw reason))
     ((member type-raw org-canvas--valid-new-quiz-types) type-raw)
     (t
      (org-canvas--signal 'org-canvas-validation-error
        "TYPE %s is not a New Quiz item type (expected: %s)"
        type-raw (string-join org-canvas--valid-new-quiz-types ", "))))))

(defun org-canvas--new-quiz-item-transform-props (props)
  "Apply pure transformations to raw item PROPS plist.
No buffer access — only string/number/boolean conversions."
  (let* ((title (org-canvas--strip-statistics-cookie
                 (plist-get props :title-raw)))
         (q-type (org-canvas--new-quiz-item-check-supported
                  (plist-get props :type-raw)))
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
         (text-html (org-canvas--org-to-html-string
                     (org-canvas--new-quiz-item-question-text
                      title (plist-get data :text))))
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

    ;; Essay, file-upload
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

(defun org-canvas--new-quiz-remote-items (quiz-id markers)
  "Return QUIZ-ID's items when a heading at MARKERS has no CANVAS_ITEM_ID.
The list serves the twin adoption of `org-canvas--adopt-child-twin'
\(issue #179); nothing is fetched while every item is stamped, and a
list that could not be read is `unknown', which adopts nothing."
  (when (cl-some (lambda (m) (not (org-entry-get m "CANVAS_ITEM_ID"))) markers)
    (condition-case err
        (org-canvas-api-request-all-pages
         'GET (org-canvas--new-quiz-api-endpoint "quizzes/%s/items" quiz-id))
      (error
       (org-canvas--log-warning org-canvas--logger
         "[New Quiz Item] Could not list quiz %s's items (%s); an unstamped item is created rather than adopted"
         quiz-id (error-message-string err))
       'unknown))))

(defun org-canvas--new-quiz-item-remote-body (item)
  "Return remote ITEM's body HTML, or nil for a reply without one.
The body sits under `entry' in the Items API and at the top level in
older replies; both are read.  The drift report's `:body-remote-fn'
for items (issue #322)."
  (let ((entry (alist-get 'entry item)))
    (or (and (listp entry) (alist-get 'item_body entry))
        (alist-get 'item_body item))))

(defun org-canvas--new-quiz-item-remote-type (item)
  "Return remote ITEM's question type in the Org TYPE spelling, or nil.
The Items API nests `interaction_type_slug' under `entry'; older
replies carry it at the top level (issue #322)."
  (let* ((entry (alist-get 'entry item))
         (slug (or (and (listp entry) (alist-get 'interaction_type_slug entry))
                   (alist-get 'interaction_type_slug item))))
    (and (stringp slug) (org-canvas--new-quiz-slug-to-type slug))))

;;;; Hot-Spot Regions (issue #365)
;;
;; A hot-spot item's answer is a list of regions of an image, held in
;; the item's `scoring_data' ({"value": [{"id": 1, "type": "square",
;; "coordinates": [{"x": 0.3156, "y": 0.1956}, ...]}]}), with the
;; image and the region count in `interaction_data'.  The pull writes
;; the regions as HOTSPOTS and the count as HOTSPOTS_COUNT, both
;; Canvas-owned: the push never reads them, and refuses the type.  The
;; image URL is not written: it points into the quiz service's media
;; store and may be signed.  Only `square' (two corners) was observed
;; on a live item; `oval' and `polygon' are written by the same rule,
;; one x,y pair per coordinate Canvas sends, unverified.

(defconst org-canvas--new-quiz-hotspot-decimals 4
  "Decimal places a hot-spot coordinate is written and compared at.
Coordinates are fractions of the image, 0 to 1; four places is what
the web editor stored on the live item (issue #365), and rounding the
reply to them keeps float noise out of the drift report.")

(defun org-canvas--new-quiz-item-entry-field (item field)
  "Return FIELD of remote ITEM, read under `entry' or at the top level."
  (let ((entry (alist-get 'entry item)))
    (or (and (listp entry) (alist-get field entry))
        (alist-get field item))))

(defun org-canvas--new-quiz-item-hot-spot-p (item)
  "Return non-nil when remote ITEM is a hot-spot item."
  (equal (org-canvas--new-quiz-item-remote-type item) "hot-spot"))

(defun org-canvas--new-quiz-hotspot-number (n)
  "Return coordinate N as HOTSPOTS writes it.
N is rounded to `org-canvas--new-quiz-hotspot-decimals' places and
written without trailing zeros; a string is read as a number first."
  (let* ((n (if (stringp n) (string-to-number n) (or n 0)))
         (text (replace-regexp-in-string
                "\\.?0+\\'" ""
                (format (format "%%.%df" org-canvas--new-quiz-hotspot-decimals)
                        n))))
    (if (member text '("" "-0" "-")) "0" text)))

(defun org-canvas--new-quiz-hotspot-format-region (region)
  "Return hot-spot REGION as HOTSPOTS writes it: SHAPE X,Y X,Y ...
REGION is one element of a hot-spot item's `scoring_data' value."
  (string-join
   (cons (format "%s" (or (alist-get 'type region) "?"))
         (mapcar (lambda (point)
                   (format "%s,%s"
                           (org-canvas--new-quiz-hotspot-number
                            (alist-get 'x point))
                           (org-canvas--new-quiz-hotspot-number
                            (alist-get 'y point))))
                 (append (alist-get 'coordinates region) nil)))
   " "))

(defun org-canvas--new-quiz-hotspot-format (regions)
  "Return REGIONS as the HOTSPOTS value, or nil when there are none.
REGIONS is a hot-spot item's `scoring_data' value, a vector or list of
region objects; each is written by
`org-canvas--new-quiz-hotspot-format-region', in Canvas's order,
separated by \"; \"."
  (let ((regions (and (or (vectorp regions) (consp regions))
                      (cl-remove-if-not #'consp (append regions nil)))))
    (when regions
      (mapconcat #'org-canvas--new-quiz-hotspot-format-region regions "; "))))

(defun org-canvas--new-quiz-item-remote-hotspots (item)
  "Return remote hot-spot ITEM's regions as HOTSPOTS spells them, or nil.
The `:remote-fn' of HOTSPOTS: the regions sit in `scoring_data' under
`entry' (Hard Rule 18).  Nil for any other type of item."
  (when (org-canvas--new-quiz-item-hot-spot-p item)
    (let ((scoring (org-canvas--new-quiz-item-entry-field item 'scoring_data)))
      (and (consp scoring)
           (org-canvas--new-quiz-hotspot-format (alist-get 'value scoring))))))

(defun org-canvas--new-quiz-item-remote-hotspots-count (item)
  "Return remote hot-spot ITEM's `hotspots_count', or nil.
The `:remote-fn' of HOTSPOTS_COUNT; nil for any other type of item."
  (when (org-canvas--new-quiz-item-hot-spot-p item)
    (let ((data (org-canvas--new-quiz-item-entry-field
                 item 'interaction_data)))
      (and (consp data) (alist-get 'hotspots_count data)))))

(defun org-canvas--new-quiz-item-carries (field)
  "Return a `:compare-p' predicate true of a hot-spot item reply with FIELD.
FIELD is `scoring_data' or `interaction_data'.  A reply that lacks it
says nothing about the regions, and must not read as their removal
\(Hard Rule 18); any other type of item has none to compare."
  (lambda (_pom item)
    (and (org-canvas--new-quiz-item-hot-spot-p item)
         (consp (org-canvas--new-quiz-item-entry-field item field)))))

(defun org-canvas--new-quiz-pull-hotspots (pom item)
  "Write hot-spot ITEM's region count and regions at POM.
A value the reply does not carry leaves the property as it is: an
unread field is unknown, not empty."
  (dolist (spec '(("HOTSPOTS_COUNT"
                   . org-canvas--new-quiz-item-remote-hotspots-count)
                  ("HOTSPOTS" . org-canvas--new-quiz-item-remote-hotspots)))
    (let ((value (funcall (cdr spec) item)))
      (when value
        (org-canvas-org-set-property pom (car spec) (format "%s" value))))))

(defun org-canvas--new-quiz-item-split-body (html)
  "Return (FIRST . REST) of item body HTML, split after its first paragraph.
FIRST is the text inside the <p> the body opens with and REST what
follows it, trimmed.  A body that opens with anything else is all
FIRST, with REST \"\".  A push sends the heading as the body's first
paragraph, so FIRST is what a heading pairs with (issue #333)."
  (let ((html (or html "")))
    (if (string-match
         "\\`[ \t\n\r]*<p\\(?:[ \t\n][^>]*\\)?>\\(\\(?:.\\|\n\\)*?\\)</p>"
         html)
        (cons (match-string 1 html) (string-trim (substring html (match-end 0))))
      (cons html ""))))

(defun org-canvas--new-quiz-item-remote-title (item)
  "Return the text of remote ITEM's first paragraph, tags stripped.
A push sends the heading as the item body's first paragraph, so this
is what a heading compares against.  A body that opens with no
paragraph is taken whole, as the pull titles it."
  (let ((first (car (org-canvas--new-quiz-item-split-body
                     (org-canvas--new-quiz-item-remote-body item)))))
    (string-trim
     (replace-regexp-in-string
      "[ \t\n\r]+" " "
      (replace-regexp-in-string "<[^>]+>" "" first)))))

(defun org-canvas--new-quiz-item-twin-p (data item)
  "Return non-nil when remote ITEM's first paragraph is DATA's title."
  (let ((title (string-trim (or (plist-get data :title) ""))))
    (and (not (string-empty-p title))
         (string= (org-canvas--new-quiz-item-remote-title item) title))))

(defun org-canvas--new-quiz-item-recover-404 (data wrapped)
  "Recover an item PATCH that 404ed: update the title's twin, or POST WRAPPED.
DATA names the quiz and the item.  The stamped id is gone; when the
quiz still holds an item of the title under another id, that one is
updated, so the recovery cannot make a second copy (issue #179)."
  (let* ((quiz-id (plist-get data :quiz-assignment-id))
         (title (plist-get data :title))
         (remote (unless (eq org-canvas-duplicate-title-strategy 'create)
                   (condition-case nil
                       (org-canvas-api-request-all-pages
                        'GET (org-canvas--new-quiz-api-endpoint "quizzes/%s/items" quiz-id))
                     (error nil))))
         (twin (car (org-canvas--child-twins
                     remote (lambda (item) (org-canvas--new-quiz-item-twin-p data item))
                     nil))))
    (if twin
        (let ((id (format "%s" (alist-get 'id twin))))
          (org-canvas--log-warning org-canvas--logger
            "[Recovery] Item '%s' is gone under its stamped id, but quiz %s holds it as item %s — updating that one instead of creating a second copy"
            title quiz-id id)
          (org-canvas-api-request
           'PATCH (org-canvas--new-quiz-api-endpoint "quizzes/%s/items/%s" quiz-id id)
           :data wrapped))
      (org-canvas--log-warning org-canvas--logger
        "[Recovery] Item not found (404). Retrying as POST...")
      (let ((response (org-canvas-api-request
                       'POST (org-canvas--new-quiz-api-endpoint "quizzes/%s/items" quiz-id)
                       :data wrapped)))
        (org-canvas--log-info org-canvas--logger "[Recovery] POST successful")
        response))))

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
        ;; 404 on PATCH -> update the title's twin, else retry as POST
        ((and (eq method 'PATCH)
              (org-canvas--404-error-p err))
         (org-canvas--new-quiz-item-recover-404 data wrapped))
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

(defun org-canvas--new-quiz-item-pull-prompt (html &optional q-type)
  "Return the Org text a pull writes as an item's prompt, from HTML.
HTML is what follows the body's first paragraph.  Text that would
hold a line the parse ends a Q-TYPE item's prompt at, a list above
all, goes in an HTML export block instead, which the push sends as it
stands, so the round trip keeps the whole body (issue #333).  An
essay's prompt ends at no line (issue #337); an ordering item's at a
numbered one too (issue #335)."
  (let ((text (string-trim (org-canvas--html-to-org-with-rewrite html)))
        (re (org-canvas--new-quiz-prompt-end-regexp-for q-type)))
    (if (and re (string-match-p re text))
        (format "#+begin_export html\n%s\n#+end_export" html)
      text)))

(defun org-canvas--new-quiz-item-pull-layout (html &optional q-type)
  "Return (TITLE . PROMPT), the Org layout a pull writes item body HTML as.
TITLE is the body's first paragraph on one line; PROMPT, the rest,
goes under the heading, where the parse of a Q-TYPE item reads it
back (`org-canvas--new-quiz-item-pull-prompt').  A body
whose first paragraph is empty, or that opens with none, is all
title, as every body was before issue #333."
  (let* ((split (org-canvas--new-quiz-item-split-body html))
         (title (org-canvas--html-to-org-inline (car split))))
    (if (and (not (string-empty-p title))
             (not (string-empty-p (cdr split))))
        (cons title (org-canvas--new-quiz-item-pull-prompt (cdr split) q-type))
      (let ((whole (if (string-empty-p (cdr split))
                       title
                     (org-canvas--html-to-org-inline html))))
        (cons (if (string-empty-p whole) "Question" whole) "")))))

(defun org-canvas--new-quiz-pull-item-heading (item-id title)
  "Return the item heading for ITEM-ID or TITLE under the quiz at point.
A heading the quiz holds for the item, by CANVAS_ITEM_ID or, unstamped,
by TITLE, is renamed to TITLE in place, so what it holds besides its
prompt, the answer list above all, stays (issue #239); otherwise a
heading is appended at the end of the quiz."
  (let ((existing (org-canvas--pull-find-child "CANVAS_ITEM_ID" item-id title)))
    (if existing
        (progn
          (goto-char existing)
          (unless (equal (org-get-heading t t t t) title)
            (org-edit-headline title)))
      (goto-char (save-excursion (org-end-of-subtree t) (point)))
      (unless (bolp) (insert "\n"))
      (insert (format "** %s\n" title))
      (forward-line -1))
    (org-back-to-heading t)
    (point)))

(defun org-canvas--new-quiz-pull-write-prompt (text &optional q-type)
  "Write TEXT as the prompt of the item heading at point, a Q-TYPE item.
The region the parse reads as the prompt is replaced, and nothing
else: the answer list below it stays.  Empty TEXT empties a region
holding text and leaves a blank one alone, so a one-paragraph item
pulls as it always did and a re-pull changes nothing."
  (let* ((bounds (org-canvas--new-quiz-item-prompt-bounds q-type))
         (old (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (unless (and (string-empty-p text) (string-blank-p old))
      (save-excursion
        (delete-region (car bounds) (cdr bounds))
        (goto-char (car bounds))
        (insert "\n")
        (unless (string-empty-p text)
          (insert "\n" text "\n\n"))))))

(defun org-canvas--new-quiz-pull-insert-item (item)
  "Write New Quiz ITEM as an L2 heading under the quiz at point.
Point must be at the parent quiz heading and is left there.  The
body's first paragraph is the heading and the rest the prompt under
it (issue #333), the layout the item parse reads back.  A heading the
quiz already holds for the item, by CANVAS_ITEM_ID or, unstamped, by
that title, is rewritten in place; otherwise the item is appended
\(issue #239).  The Items API nests the body and the type under
`entry'; older replies carry them at the top level, so both are read."
  (let* ((quiz-pos (point))
         (entry (let ((e (alist-get 'entry item))) (and (listp e) e)))
         (slug (or (alist-get 'interaction_type_slug entry)
                   (alist-get 'interaction_type_slug item)))
         (q-type (and slug (org-canvas--new-quiz-slug-to-type slug)))
         (layout (org-canvas--new-quiz-item-pull-layout
                  (or (alist-get 'item_body entry) (alist-get 'item_body item))
                  q-type))
         (points (alist-get 'points_possible item))
         (item-id (alist-get 'id item))
         (qpos (org-canvas--new-quiz-pull-item-heading item-id (car layout))))
    (when item-id
      (org-canvas-org-save-sync-state qpos (format "%s" item-id) "CANVAS_ITEM_ID"))
    (when q-type
      (org-canvas-org-set-property qpos "TYPE" q-type))
    (when points
      (org-canvas-org-set-property
       qpos "POINTS" (format "%s" points)))
    (when (equal q-type "hot-spot")
      (org-canvas--new-quiz-pull-hotspots qpos item))
    (goto-char qpos)
    (org-canvas--new-quiz-pull-write-prompt
     (cdr layout) (or q-type (org-entry-get qpos "TYPE")))
    (goto-char quiz-pos)))

(provide 'org-canvas-new-quiz-items)
;;; org-canvas-new-quiz-items.el ends here
