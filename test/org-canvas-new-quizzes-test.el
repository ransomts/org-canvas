;;; org-canvas-new-quizzes-test.el --- Buttercup tests for New Quizzes  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-new-quizzes)

;;;; Helper Functions

(describe "org-canvas--new-quiz-api-endpoint"
  (it "constructs quiz API URL with correct prefix"
    (with-org-canvas-test-config
      (expect (org-canvas--new-quiz-api-endpoint "quizzes")
              :to-equal "https://test.canvas.example.com/api/quiz/v1/courses/99999/quizzes")))

  (it "formats suffix with arguments"
    (with-org-canvas-test-config
      (expect (org-canvas--new-quiz-api-endpoint "quizzes/%s/items" "42")
              :to-equal "https://test.canvas.example.com/api/quiz/v1/courses/99999/quizzes/42/items")))

  (it "handles multiple format arguments"
    (with-org-canvas-test-config
      (expect (org-canvas--new-quiz-api-endpoint "quizzes/%s/items/%s" "42" "abc")
              :to-equal "https://test.canvas.example.com/api/quiz/v1/courses/99999/quizzes/42/items/abc"))))

(describe "org-canvas--new-quiz-parse-body-text"
  (it "extracts body text from heading"
    (with-temp-org-buffer
     "* Quiz Title
:PROPERTIES:
:END:

This is the quiz description.
"
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-parse-body-text)
             :to-equal "This is the quiz description.")))

  (it "excludes subheadings from body"
    (with-temp-org-buffer
     "* Quiz Title
:PROPERTIES:
:END:

Quiz intro text.

** Question 1
Question content.
"
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-parse-body-text)
             :to-equal "Quiz intro text.")))

  (it "returns empty string when no body"
    (with-temp-org-buffer
     "* Quiz Title
:PROPERTIES:
:END:
** Question
"
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-parse-body-text) :to-equal ""))))

(describe "org-canvas--new-quiz-parse-question-text"
  (it "extracts question text before list"
    (with-temp-org-buffer
     "* Quiz
** What is 2+2?
:PROPERTIES:
:END:

- [X] 4
- [ ] 5
"
     (search-forward "What is 2+2")
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-parse-question-text) :to-equal "")))

  (it "extracts multi-line question text"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

Consider the following scenario.
What would happen?

- [X] Answer A
- [ ] Answer B
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((text (org-canvas--new-quiz-parse-question-text)))
       (expect text :to-match "Consider the following scenario")))))

(describe "org-canvas--new-quiz-parse-checkbox-list"
  (it "parses correct and incorrect answers"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

- [X] Correct answer
- [ ] Wrong answer
- [ ] Also wrong
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((answers (org-canvas--new-quiz-parse-checkbox-list)))
       (expect (length answers) :to-equal 3)
       (expect (cdr (nth 0 answers)) :to-be t)
       (expect (cdr (nth 1 answers)) :to-be nil)
       (expect (car (nth 0 answers)) :to-equal "Correct answer"))))

  (it "handles multiple correct answers"
    (with-temp-org-buffer
     "* Quiz
** Question
- [X] First correct
- [X] Second correct
- [ ] Wrong
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((answers (org-canvas--new-quiz-parse-checkbox-list)))
       (expect (cl-count t answers :key #'cdr) :to-equal 2)))))

(describe "org-canvas--new-quiz-parse-matching-list"
  (it "parses matching pairs"
    (with-temp-org-buffer
     "* Quiz
** Match the terms
- DNA = Deoxyribonucleic acid
- RNA = Ribonucleic acid
- ATP = Adenosine triphosphate
"
     (search-forward "Match the terms")
     (org-back-to-heading)
     (let ((matches (org-canvas--new-quiz-parse-matching-list)))
       (expect (length matches) :to-equal 3)
       (expect (car (nth 0 matches)) :to-equal "DNA")
       (expect (cdr (nth 0 matches)) :to-equal "Deoxyribonucleic acid")))))

(describe "org-canvas--new-quiz-parse-ordering-list"
  (it "parses numbered list items"
    (with-temp-org-buffer
     "* Quiz
** Order these events
1. First event
2. Second event
3. Third event
"
     (search-forward "Order these events")
     (org-back-to-heading)
     (let ((items (org-canvas--new-quiz-parse-ordering-list)))
       (expect (length items) :to-equal 3)
       (expect (nth 0 items) :to-equal "First event")
       (expect (nth 2 items) :to-equal "Third event")))))

(describe "org-canvas--new-quiz-parse-categorization-list"
  (it "parses category: items format"
    (with-temp-org-buffer
     "* Quiz
** Categorize these
- Fruit: apple, banana
- Vegetable: carrot, peas
"
     (search-forward "Categorize these")
     (org-back-to-heading)
     (let ((categories (org-canvas--new-quiz-parse-categorization-list)))
       (expect (length categories) :to-equal 2)
       (expect (car (nth 0 categories)) :to-equal "Fruit")
       (expect (cdr (nth 0 categories)) :to-equal '("apple" "banana"))))))

(describe "org-canvas--new-quiz-parse-numerical-answer"
  (it "parses exact numerical answer"
    (with-temp-org-buffer
     "* Quiz
** What is pi?
- [X] 3.14
"
     (search-forward "What is pi")
     (org-back-to-heading)
     (unless test-org-canvas-emacs-30-p
       (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
     (let ((num (org-canvas--new-quiz-parse-numerical-answer)))
       (expect (plist-get num :type) :to-equal 'exact)
       (expect (plist-get num :value) :to-equal 3.14))))

  (it "parses range numerical answer"
    (with-temp-org-buffer
     "* Quiz
** Estimate the value
- [X] [10, 20]
"
     (search-forward "Estimate the value")
     (org-back-to-heading)
     (unless test-org-canvas-emacs-30-p
       (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
     (let ((num (org-canvas--new-quiz-parse-numerical-answer)))
       (expect (plist-get num :type) :to-equal 'range)
       (expect (plist-get num :start) :to-equal 10)
       (expect (plist-get num :end) :to-equal 20)))))

;;;; Quiz Parsing (Level 1)

(describe "org-canvas--new-quiz-parse-entry"
  (it "parses quiz heading with all properties"
    (with-temp-org-buffer
     "* Midterm Exam
:PROPERTIES:
:CANVAS_ASSIGNMENT_ID: 98765
:TIME_LIMIT: 60
:SHUFFLE_ANSWERS: true
:ONE_AT_A_TIME: true
:ALLOWED_ATTEMPTS: 2
:SCORING_POLICY: keep_highest
:END:

This is the midterm.
"
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-parse-entry)))
       (expect (plist-get data :title) :to-equal "Midterm Exam")
       (expect (plist-get data :canvas-id) :to-equal "98765")
       (expect (plist-get data :time_limit) :to-equal 60)
       (expect (plist-get data :shuffle_answers) :to-be t)
       (expect (plist-get data :one_at_a_time) :to-be t)
       (expect (plist-get data :allowed_attempts) :to-equal 2)
       (expect (plist-get data :scoring_policy) :to-equal "keep_highest")
       (expect (plist-get data :description) :to-be-truthy))))

  (it "parses new quiz without CANVAS_ASSIGNMENT_ID"
    (with-temp-org-buffer
     "* New Quiz
:PROPERTIES:
:TIME_LIMIT: 30
:END:

Quiz description.
"
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-parse-entry)))
       (expect (plist-get data :canvas-id) :to-be nil)
       (expect (plist-get data :title) :to-equal "New Quiz"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     "* \n:PROPERTIES:\n:END:\n"
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-parse-entry) :to-throw 'error)))

  (it "validates scoring policy"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SCORING_POLICY: invalid_policy
:END:
"
     (org-back-to-heading)
     (spy-on 'elog-warning)
     (let ((data (org-canvas--new-quiz-parse-entry)))
       (expect (plist-get data :scoring_policy) :to-equal "keep_highest"))))

  (it "parses quiz with no optional properties"
    (with-temp-org-buffer
     "* Simple Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-parse-entry)))
       (expect (plist-get data :title) :to-equal "Simple Quiz")
       (expect (plist-get data :time_limit) :to-be nil)
       (expect (plist-get data :shuffle_answers) :to-be nil)
       (expect (plist-get data :allowed_attempts) :to-be nil))))

  (it "strips statistics cookie from title"
    (with-temp-org-buffer
     "* Midterm [2/5]
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-parse-entry)))
       (expect (plist-get data :title) :to-equal "Midterm")))))

;;;; Quiz Build Payload

(describe "org-canvas--new-quiz-build-payload"
  (it "builds payload with all fields"
    (let* ((data (list :title "Final Exam"
                       :description "<p>Instructions</p>"
                       :time_limit 120
                       :shuffle_answers t
                       :one_at_a_time t
                       :allowed_attempts 3
                       :scoring_policy "keep_highest"))
           (payload (org-canvas--new-quiz-build-payload data)))
      (expect (gethash "title" payload) :to-equal "Final Exam")
      (expect (gethash "instructions" payload) :to-equal "<p>Instructions</p>")
      (expect (gethash "time_limit" payload) :to-equal 120)
      (expect (gethash "shuffle_answers" payload) :to-be t)
      (expect (gethash "one_at_a_time" payload) :to-be t)
      (expect (gethash "allowed_attempts" payload) :to-equal 3)
      (expect (gethash "scoring_policy" payload) :to-equal "keep_highest")))

  (it "omits nil optional fields"
    (let* ((data (list :title "Simple Quiz"
                       :description nil
                       :time_limit nil
                       :shuffle_answers nil
                       :one_at_a_time nil
                       :allowed_attempts nil
                       :scoring_policy nil))
           (payload (org-canvas--new-quiz-build-payload data)))
      (expect (gethash "title" payload) :to-equal "Simple Quiz")
      (expect (gethash "instructions" payload nil) :to-be nil)
      (expect (gethash "time_limit" payload nil) :to-be nil)
      (expect (gethash "shuffle_answers" payload nil) :to-be nil))))

;;;; Quiz Push to API

(describe "org-canvas--new-quiz-push-to-api"
  (it "uses POST for new quizzes"
    (with-org-canvas-test-config
      (with-mock-api
        (let* ((data (list :title "New Quiz" :canvas-id nil :pom (point-marker)))
               (payload (make-hash-table :test 'equal)))
          (puthash "title" "New Quiz" payload)
          (org-canvas--new-quiz-push-to-api data payload)
          (expect-api-called 'POST "quizzes")))))

  (it "wraps payload under quiz key"
    (with-org-canvas-test-config
      (let (sent-data)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq sent-data (plist-get args :data))
                     '((assignment_id . 1)))))
          (let* ((data (list :title "Wrap Test" :canvas-id nil :pom (point-marker)))
                 (payload (make-hash-table :test 'equal)))
            (puthash "title" "Wrap Test" payload)
            (org-canvas--new-quiz-push-to-api data payload)
            (expect (hash-table-p sent-data) :to-be t)
            (expect (gethash "quiz" sent-data) :to-equal payload))))))

  (it "uses PATCH for existing quizzes"
    (with-org-canvas-test-config
      (with-mock-api
        (let* ((data (list :title "Existing Quiz" :canvas-id "42" :pom (point-marker)))
               (payload (make-hash-table :test 'equal)))
          (puthash "title" "Existing Quiz" payload)
          (org-canvas--new-quiz-push-to-api data payload)
          (expect-api-called 'PATCH "quizzes/42")))))

  (it "returns dry-run response when dry-run active"
    (let ((org-canvas--dry-run t))
      (with-org-canvas-test-config
        (let* ((data (list :title "Test" :canvas-id nil :pom (point-marker)))
               (payload (make-hash-table :test 'equal))
               (response (org-canvas--new-quiz-push-to-api data payload)))
          (expect (alist-get 'assignment_id response) :to-equal "dry-run")))))

  (it "retries as POST on 404 for PATCH"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (and (eq method 'PATCH) (= call-count 1))
                         (error "HTTP 404 Not Found")
                       '((assignment_id . 99))))))
          (let* ((data (list :title "Stale Quiz" :canvas-id "old-id" :pom (point-marker)))
                 (payload (make-hash-table :test 'equal))
                 (response (org-canvas--new-quiz-push-to-api data payload)))
            (expect call-count :to-equal 2)
            (expect (alist-get 'assignment_id response) :to-equal 99))))))

  (it "signals error when POST retry also fails on 404"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (error "HTTP 404 Not Found"))))
        (let* ((data (list :title "Bad" :canvas-id "old" :pom (point-marker)))
               (payload (make-hash-table :test 'equal)))
          (expect (org-canvas--new-quiz-push-to-api data payload)
                  :to-throw 'error)))))

  (it "signals error for non-404 failures"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (error "HTTP 500 Internal Server Error"))))
        (let* ((data (list :title "Bad Quiz" :canvas-id nil :pom (point-marker)))
               (payload (make-hash-table :test 'equal)))
          (expect (org-canvas--new-quiz-push-to-api data payload)
                  :to-throw 'error))))))

;;;; Quiz Finalize

(describe "org-canvas--new-quiz-finalize"
  (it "saves CANVAS_ASSIGNMENT_ID from response"
    (with-temp-org-buffer
     "* Finalize Test Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Finalize Test Quiz" :pom pom))
            (response '((assignment_id . 98765))))
       (org-canvas--new-quiz-finalize data response)
       (expect (org-entry-get pom "CANVAS_ASSIGNMENT_ID") :to-equal "98765"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Timestamp Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Timestamp Test" :pom pom))
            (response '((assignment_id . 123))))
       (org-canvas--new-quiz-finalize data response)
       (expect-synced-timestamp pom))))

  (it "saves CANVAS_UPDATED_AT when present"
    (with-temp-org-buffer
     "* Updated At Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Updated At Test" :pom pom))
            (response '((assignment_id . 456)
                        (updated_at . "2026-02-14T12:00:00Z"))))
       (org-canvas--new-quiz-finalize data response)
       (expect (org-entry-get pom "CANVAS_UPDATED_AT")
               :to-equal "2026-02-14T12:00:00Z"))))

  (it "falls back to id when assignment_id missing"
    (with-temp-org-buffer
     "* Fallback Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Fallback Test" :pom pom)))
       (org-canvas--new-quiz-finalize data '((id . 42)))
       (expect (org-entry-get pom "CANVAS_ASSIGNMENT_ID") :to-equal "42"))))

  (it "errors when neither assignment_id nor id in response"
    (with-temp-org-buffer
     "* Error Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Error Test" :pom pom)))
       (expect (org-canvas--new-quiz-finalize data '((title . "foo")))
               :to-throw 'error))))

  (it "errors when pom is missing"
    (let ((data (list :title "No POM" :pom nil)))
      (expect (org-canvas--new-quiz-finalize data '((assignment_id . 1)))
              :to-throw 'error))))

;;;; Item Parsing (Level 2)

(describe "org-canvas--new-quiz-item-parse-entry"
  (it "parses item with all properties"
    (with-temp-org-buffer
     "* Quiz
** What is 2+2?
:PROPERTIES:
:CANVAS_ITEM_ID: item-uuid-1234
:TYPE: choice
:POINTS: 5
:END:

- [X] 4
- [ ] 3
"
     (search-forward "What is 2+2")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-parse-entry "quiz-42")))
       (expect (plist-get data :title) :to-equal "What is 2+2?")
       (expect (plist-get data :canvas-id) :to-equal "item-uuid-1234")
       (expect (plist-get data :quiz-assignment-id) :to-equal "quiz-42")
       (let* ((result-type (plist-get data :type)))
         (expect result-type :to-equal "choice"))
       (expect (plist-get data :points) :to-equal 5))))

  (it "defaults TYPE to choice when absent"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:
"
     (search-forward "Question")
     (org-back-to-heading)
     (let* ((data (org-canvas--new-quiz-item-parse-entry "42"))
            (result-type (plist-get data :type)))
       (expect result-type :to-equal "choice"))))

  (it "defaults POINTS to 1 when absent"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-parse-entry "42")))
       (expect (plist-get data :points) :to-equal 1))))

  (it "validates TYPE property"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:TYPE: invalid_type
:END:
"
     (search-forward "Question")
     (org-back-to-heading)
     (spy-on 'elog-warning)
     (let* ((data (org-canvas--new-quiz-item-parse-entry "42"))
            (result-type (plist-get data :type)))
       ;; Should fall back to default "choice"
       (expect result-type :to-equal "choice")))))

;;;; Interaction Data Building

(describe "org-canvas--new-quiz-item-build-choice-data"
  (it "builds choice data with correct/incorrect answers"
    (let ((data (org-canvas--new-quiz-item-build-choice-data
                 '(("Option A" . t) ("Option B" . nil) ("Option C" . nil)))))
      (expect (alist-get 'choices data) :to-be-truthy)
      (let* ((choices (append (alist-get 'choices data) nil))
             (first (nth 0 choices)))
        (expect (length choices) :to-equal 3)
        (expect (alist-get 'item_body first) :to-equal "Option A")
        (expect (alist-get 'value (alist-get 'scoring_data first)) :to-equal 1))))

  (it "assigns sequential IDs and positions"
    (let* ((data (org-canvas--new-quiz-item-build-choice-data
                  '(("A" . t) ("B" . nil))))
           (choices (append (alist-get 'choices data) nil)))
      (expect (alist-get 'id (nth 0 choices)) :to-equal "choice_0")
      (expect (alist-get 'id (nth 1 choices)) :to-equal "choice_1")
      (expect (alist-get 'position (nth 0 choices)) :to-equal 1)
      (expect (alist-get 'position (nth 1 choices)) :to-equal 2))))

(describe "org-canvas--new-quiz-item-build-true-false-data"
  (it "sets correct answer to true"
    (let ((data (org-canvas--new-quiz-item-build-true-false-data
                 '(("True" . t) ("False" . nil)))))
      (expect (alist-get 'value (alist-get 'scoring_data data))
              :to-equal "true")))

  (it "sets correct answer to false"
    (let ((data (org-canvas--new-quiz-item-build-true-false-data
                 '(("True" . nil) ("False" . t)))))
      (expect (alist-get 'value (alist-get 'scoring_data data))
              :to-equal "false")))

  (it "handles case-insensitive True"
    (let ((data (org-canvas--new-quiz-item-build-true-false-data
                 '(("true" . t) ("false" . nil)))))
      (expect (alist-get 'value (alist-get 'scoring_data data))
              :to-equal "true"))))

(describe "org-canvas--new-quiz-item-build-short-answer-data"
  (it "includes only correct answers"
    (let ((data (org-canvas--new-quiz-item-build-short-answer-data
                 '(("Paris" . t) ("paris" . t) ("London" . nil)))))
      (let ((values (append (alist-get 'value (alist-get 'scoring_data data)) nil)))
        (expect (length values) :to-equal 2)
        (expect (member "Paris" values) :to-be-truthy)
        (expect (member "paris" values) :to-be-truthy)))))

(describe "org-canvas--new-quiz-item-build-matching-data"
  (it "builds stems and choices from pairs"
    (let ((data (org-canvas--new-quiz-item-build-matching-data
                 '(("DNA" . "Deoxyribonucleic acid")
                   ("RNA" . "Ribonucleic acid")))))
      (let ((stems (append (alist-get 'stems data) nil))
            (choices (append (alist-get 'choices data) nil)))
        (expect (length stems) :to-equal 2)
        (expect (length choices) :to-equal 2)
        (expect (alist-get 'item_body (nth 0 stems)) :to-equal "DNA")
        (expect (alist-get 'item_body (nth 0 choices)) :to-equal "Deoxyribonucleic acid")
        ;; Verify scoring_data links stem to choice
        (expect (alist-get 'value (alist-get 'scoring_data (nth 0 stems)))
                :to-equal "choice_0")))))

(describe "org-canvas--new-quiz-item-build-ordering-data"
  (it "builds ordered choices"
    (let ((data (org-canvas--new-quiz-item-build-ordering-data
                 '("First" "Second" "Third"))))
      (let ((choices (append (alist-get 'choices data) nil)))
        (expect (length choices) :to-equal 3)
        (expect (alist-get 'item_body (nth 0 choices)) :to-equal "First"))
      ;; scoring_data value should contain ordered IDs
      (let ((value (append (alist-get 'value (alist-get 'scoring_data data)) nil)))
        (expect (length value) :to-equal 3)
        (expect (nth 0 value) :to-equal "item_0")))))

(describe "org-canvas--new-quiz-item-build-categorization-data"
  (it "builds categories and distractors as keyed objects"
    (let ((data (org-canvas--new-quiz-item-build-categorization-data
                 '(("Fruit" . ("apple" "banana"))
                   ("Vegetable" . ("carrot"))))))
      (let ((cats-ht (alist-get 'categories data))
            (dist-ht (alist-get 'distractors data))
            (cat-order (append (alist-get 'category_order data) nil))
            (flat (append (alist-get '_flat_distractors data) nil)))
        ;; Categories is a hash-table keyed by ID
        (expect (hash-table-p cats-ht) :to-be t)
        (expect (hash-table-count cats-ht) :to-equal 2)
        (expect (alist-get 'item_body (gethash "cat_0" cats-ht)) :to-equal "Fruit")
        ;; Distractors is a hash-table keyed by ID
        (expect (hash-table-p dist-ht) :to-be t)
        (expect (hash-table-count dist-ht) :to-equal 3)
        (expect (alist-get 'item_body (gethash "item_0_0" dist-ht)) :to-equal "apple")
        ;; _flat_distractors has scoring_data for scoring builder
        (expect (length flat) :to-equal 3)
        (expect (alist-get 'value (alist-get 'scoring_data (nth 0 flat)))
                :to-equal "cat_0")
        ;; category_order lists category IDs in order
        (expect cat-order :to-equal '("cat_0" "cat_1"))))))

(describe "org-canvas--new-quiz-item-build-numerical-data"
  (it "builds exact numerical data as answer object array"
    (let* ((data (org-canvas--new-quiz-item-build-numerical-data
                  '(:type exact :value 42)))
           (sd (alist-get 'scoring_data data))
           (answers (append (alist-get 'value sd) nil))
           (ans (nth 0 answers)))
      (expect (length answers) :to-equal 1)
      (let ((ans-type (alist-get 'type ans)))
        (expect ans-type :to-equal "exactResponse"))
      (expect (alist-get 'value ans) :to-equal "42")
      (expect (alist-get 'id ans) :to-equal "1")))

  (it "builds range numerical data as answer object array"
    (let* ((data (org-canvas--new-quiz-item-build-numerical-data
                  '(:type range :start 10 :end 20)))
           (sd (alist-get 'scoring_data data))
           (answers (append (alist-get 'value sd) nil))
           (ans (nth 0 answers)))
      (expect (length answers) :to-equal 1)
      (let ((ans-type (alist-get 'type ans)))
        (expect ans-type :to-equal "withinARange"))
      (expect (alist-get 'start ans) :to-equal "10")
      (expect (alist-get 'end ans) :to-equal "20"))))

(describe "org-canvas--new-quiz-item-build-fill-blank-data"
  (it "includes only correct answers"
    (let ((data (org-canvas--new-quiz-item-build-fill-blank-data
                 '(("correct1" . t) ("wrong" . nil) ("correct2" . t)))))
      (let ((values (append (alist-get 'value (alist-get 'scoring_data data)) nil)))
        (expect (length values) :to-equal 2)
        (expect (member "correct1" values) :to-be-truthy)))))

(describe "org-canvas--new-quiz-item-build-interaction-data"
  (it "dispatches to choice builder"
    (with-temp-org-buffer
     "* Quiz
** Q
:PROPERTIES:
:END:

- [X] Right
- [ ] Wrong
"
     (search-forward "Q")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "choice")))
       (expect (alist-get 'choices data) :to-be-truthy))))

  (it "dispatches to true-false builder"
    (with-temp-org-buffer
     "* Quiz
** TF
:PROPERTIES:
:END:

- [ ] True
- [X] False
"
     (search-forward "TF")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "true-false")))
       (expect data :to-be-truthy))))

  (it "dispatches to multi-answer builder"
    (with-temp-org-buffer
     "* Quiz
** MA
:PROPERTIES:
:END:

- [X] A
- [X] B
- [ ] C
"
     (search-forward "MA")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "multi-answer")))
       (expect (alist-get 'choices data) :to-be-truthy))))

  (it "dispatches to fill-in-the-blank builder"
    (with-temp-org-buffer
     "* Quiz
** FIB
:PROPERTIES:
:END:

- [X] answer1
- [X] answer2
"
     (search-forward "FIB")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "fill-in-the-blank")))
       (expect (alist-get 'scoring_data data) :to-be-truthy))))

  (it "returns nil for short-answer type (mapped to essay)"
    (with-temp-org-buffer
     "* Quiz
** SA
:PROPERTIES:
:END:
"
     (search-forward "SA")
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-item-build-interaction-data "short-answer")
             :to-be nil)))

  (it "dispatches to matching builder"
    (with-temp-org-buffer
     "* Quiz
** Match
:PROPERTIES:
:END:

- A = 1
- B = 2
"
     (search-forward "Match")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "matching")))
       (expect data :to-be-truthy))))

  (it "dispatches to ordering builder"
    (with-temp-org-buffer
     "* Quiz
** Order
:PROPERTIES:
:END:

1. First
2. Second
"
     (search-forward "Order")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "ordering")))
       (expect data :to-be-truthy))))

  (it "dispatches to categorization builder"
    (with-temp-org-buffer
     "* Quiz
** Cat
:PROPERTIES:
:END:

- Animals: cat, dog
- Plants: rose, lily
"
     (search-forward "Cat")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "categorization")))
       (expect data :to-be-truthy))))

  (it "dispatches to numerical builder"
    (with-temp-org-buffer
     "* Quiz
** Num
:PROPERTIES:
:END:

- [X] 42
"
     (search-forward "Num")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "numerical")))
       (expect data :to-be-truthy))))

  (it "returns nil for essay type"
    (with-temp-org-buffer
     "* Quiz
** Essay
:PROPERTIES:
:END:
"
     (search-forward "Essay")
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-item-build-interaction-data "essay")
             :to-be nil)))

  (it "returns nil for file-upload type"
    (with-temp-org-buffer
     "* Quiz
** Upload
:PROPERTIES:
:END:
"
     (search-forward "Upload")
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-item-build-interaction-data "file-upload")
             :to-be nil)))

  (it "returns nil for unknown type"
    (with-temp-org-buffer
     "* Quiz
** Unknown
:PROPERTIES:
:END:
"
     (search-forward "Unknown")
     (org-back-to-heading)
     (expect (org-canvas--new-quiz-item-build-interaction-data "nonexistent")
             :to-be nil))))

;;;; Item Scoring Data

(describe "org-canvas--new-quiz-item-build-scoring-data"
  (it "extracts correct choice ID for choice type"
    (let* ((idata `((choices . ,(vconcat
                                 '(((id . "choice_0") (scoring_data . ((value . 1))))
                                   ((id . "choice_1") (scoring_data . ((value . 0)))))))))
           (result (org-canvas--new-quiz-item-build-scoring-data "choice" idata))
           (sd (car result)))
      (expect (alist-get 'value sd) :to-equal "choice_0")))

  (it "collects all correct IDs for multi-answer type"
    (let* ((idata `((choices . ,(vconcat
                                 '(((id . "choice_0") (scoring_data . ((value . 1))))
                                   ((id . "choice_1") (scoring_data . ((value . 0))))
                                   ((id . "choice_2") (scoring_data . ((value . 1)))))))))
           (result (org-canvas--new-quiz-item-build-scoring-data "multi-answer" idata))
           (sd (car result)))
      (expect (append (alist-get 'value sd) nil)
              :to-equal '("choice_0" "choice_2"))))

  (it "extracts and removes scoring_data from true-false"
    (let* ((idata `((true_choice . ((id . "true") (position . 1)))
                    (false_choice . ((id . "false") (position . 2)))
                    (scoring_data . ((value . "true")))))
           (result (org-canvas--new-quiz-item-build-scoring-data "true-false" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (alist-get 'value sd) :to-equal "true")
      (expect (alist-get 'scoring_data cleaned) :to-be nil)))

  (it "returns empty value for short-answer (mapped to essay)"
    (let* ((result (org-canvas--new-quiz-item-build-scoring-data "short-answer" nil))
           (sd (car result)))
      (expect (alist-get 'value sd) :to-equal "")))

  (it "extracts scoring_data from ordering and removes it"
    (let* ((idata `((choices . ,(vconcat '(((id . "item_0") (position . 1) (item_body . "First"))
                                           ((id . "item_1") (position . 2) (item_body . "Second")))))
                    (scoring_data . ((value . ,(vector "item_0" "item_1"))))))
           (result (org-canvas--new-quiz-item-build-scoring-data "ordering" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (alist-get 'value sd) :to-be-truthy)
      (expect (alist-get 'scoring_data cleaned) :to-be nil)
      (expect (alist-get 'choices cleaned) :to-be-truthy)))

  (it "builds stem-to-choice map for matching"
    (let* ((idata `((stems . ,(vconcat
                               '(((id . "stem_0") (item_body . "DNA")
                                  (scoring_data . ((value . "choice_0"))))
                                 ((id . "stem_1") (item_body . "RNA")
                                  (scoring_data . ((value . "choice_1")))))))
                    (choices . ,(vconcat
                                '(((id . "choice_0") (item_body . "Deoxy"))
                                  ((id . "choice_1") (item_body . "Ribo")))))))
           (result (org-canvas--new-quiz-item-build-scoring-data "matching" idata))
           (sd (car result))
           (cleaned (cdr result)))
      ;; scoring_data value is a hash-table mapping stem IDs to choice IDs
      (expect (hash-table-p (alist-get 'value sd)) :to-be t)
      (expect (gethash "stem_0" (alist-get 'value sd)) :to-equal "choice_0")
      (expect (gethash "stem_1" (alist-get 'value sd)) :to-equal "choice_1")
      ;; stems should have scoring_data stripped
      (let ((stems (append (alist-get 'stems cleaned) nil)))
        (expect (alist-get 'scoring_data (nth 0 stems)) :to-be nil))))

  (it "builds per-category scoring for categorization"
    (let* ((cats-ht (make-hash-table :test 'equal))
           (dist-ht (make-hash-table :test 'equal)))
      (puthash "cat_0" '((id . "cat_0") (item_body . "Fruit")) cats-ht)
      (puthash "cat_1" '((id . "cat_1") (item_body . "Veg")) cats-ht)
      (puthash "item_0_0" '((id . "item_0_0") (item_body . "apple")) dist-ht)
      (puthash "item_1_0" '((id . "item_1_0") (item_body . "carrot")) dist-ht)
      (let* ((idata `((categories . ,cats-ht)
                      (distractors . ,dist-ht)
                      (category_order . ,(vector "cat_0" "cat_1"))
                      (_flat_distractors
                       . ,(vconcat
                           '(((id . "item_0_0") (item_body . "apple")
                              (scoring_data . ((value . "cat_0"))))
                             ((id . "item_1_0") (item_body . "carrot")
                              (scoring_data . ((value . "cat_1")))))))))
             (result (org-canvas--new-quiz-item-build-scoring-data "categorization" idata))
             (sd (car result))
             (cleaned (cdr result)))
        ;; scoring_data value is a vector of per-category entries
        (let ((cats (append (alist-get 'value sd) nil)))
          (expect (length cats) :to-equal 2)
          (expect (alist-get 'id (nth 0 cats)) :to-equal "cat_0")
          (expect (alist-get 'scoring_algorithm (nth 0 cats)) :to-equal "AllOrNothing"))
        ;; _flat_distractors should be removed from cleaned data
        (expect (alist-get '_flat_distractors cleaned) :to-be nil)
        ;; categories hash-table should still be present
        (expect (hash-table-p (alist-get 'categories cleaned)) :to-be t))))

  (it "returns empty value for essay"
    (let* ((result (org-canvas--new-quiz-item-build-scoring-data "essay" nil))
           (sd (car result)))
      (expect (alist-get 'value sd) :to-equal "")))

  (it "returns empty value for file-upload"
    (let* ((result (org-canvas--new-quiz-item-build-scoring-data "file-upload" nil))
           (sd (car result)))
      (expect (alist-get 'value sd) :to-equal "")))

  (it "extracts scoring_data from numerical exact"
    (let* ((answer-obj `((id . "1") (type . "exactResponse") (value . "42")))
           (idata `((scoring_data . ((value . ,(vector answer-obj))))))
           (result (org-canvas--new-quiz-item-build-scoring-data "numerical" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (alist-get 'value sd) :to-be-truthy)
      (expect (alist-get 'scoring_data cleaned) :to-be nil)))

  (it "extracts scoring_data from fill-in-the-blank"
    (let* ((idata `((scoring_data . ((value . ,(vector "answer1" "answer2"))))))
           (result (org-canvas--new-quiz-item-build-scoring-data "fill-in-the-blank" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (alist-get 'value sd) :to-be-truthy)
      (expect (alist-get 'scoring_data cleaned) :to-be nil))))

;;;; Item Build Payload

(describe "org-canvas--new-quiz-item-build-payload"
  (it "builds complete item payload"
    (with-temp-org-buffer
     "* Quiz
** What is 2+2?
:PROPERTIES:
:TYPE: choice
:POINTS: 5
:END:

- [X] 4
- [ ] 5
"
     (search-forward "What is 2+2")
     (org-back-to-heading)
     (let* ((data (list :title "What is 2+2?"
                        :text ""
                        :type "choice"
                        :points 5
                        :pom (point-marker)))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       (expect (gethash "interaction_type_slug" payload) :to-equal "choice")
       (expect (gethash "points_possible" payload) :to-equal 5)
       (expect (gethash "entry_type" payload) :to-equal "Item")
       ;; item_body uses title as question stem when text is empty
       (expect (gethash "item_body" payload) :to-match "What is 2\\+2")
       (expect (gethash "interaction_data" payload) :to-be-truthy)
       ;; scoring fields
       (expect (gethash "scoring_data" payload) :to-be-truthy)
       (expect (gethash "scoring_algorithm" payload) :to-equal "Equivalence"))))

  (it "maps type to correct API slug"
    (with-temp-org-buffer
     "* Quiz
** TF Question
:PROPERTIES:
:TYPE: true-false
:END:

- [X] True
- [ ] False
"
     (search-forward "TF Question")
     (org-back-to-heading)
     (let* ((data (list :title "TF Question"
                        :text ""
                        :type "true-false"
                        :points 1
                        :pom (point-marker)))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       (expect (gethash "interaction_type_slug" payload) :to-equal "true-false")
       (expect (gethash "scoring_algorithm" payload) :to-equal "Equivalence")
       (expect (gethash "scoring_data" payload) :to-be-truthy))))

  (it "omits interaction_data for essay"
    (with-temp-org-buffer
     "* Quiz
** Write an essay
:PROPERTIES:
:TYPE: essay
:END:
"
     (search-forward "Write an essay")
     (org-back-to-heading)
     (let* ((data (list :title "Write an essay"
                        :text ""
                        :type "essay"
                        :points 10
                        :pom (point-marker)))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       (expect (gethash "interaction_data" payload nil) :to-be nil)
       (expect (gethash "scoring_algorithm" payload) :to-equal "None")
       (expect (alist-get 'value (gethash "scoring_data" payload)) :to-equal ""))))

  (it "uses title as item_body when text is empty"
    (with-temp-org-buffer
     "* Quiz
** Capital of France?
:PROPERTIES:
:TYPE: short-answer
:END:

- [X] Paris
"
     (search-forward "Capital of France")
     (org-back-to-heading)
     (let* ((data (list :title "Capital of France?"
                        :text ""
                        :type "short-answer"
                        :points 1
                        :pom (point-marker)))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       (expect (gethash "item_body" payload) :to-match "Capital of France"))))

  (it "combines title and body text when both present"
    (with-temp-org-buffer
     "* Quiz
** Geography
:PROPERTIES:
:TYPE: short-answer
:END:

- [X] Paris
"
     (search-forward "Geography")
     (org-back-to-heading)
     (let* ((data (list :title "Geography"
                        :text "What is the capital of France?"
                        :type "short-answer"
                        :points 1
                        :pom (point-marker)))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       (expect (gethash "item_body" payload) :to-match "Geography")
       (expect (gethash "item_body" payload) :to-match "capital of France")))))

;;;; Item Push to API

(describe "org-canvas--new-quiz-item-push-to-api"
  (it "uses POST for new items"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data (list :title "New Item" :canvas-id nil
                          :quiz-assignment-id "42" :pom (point-marker))))
          (org-canvas--new-quiz-item-push-to-api
           data (make-hash-table :test 'equal))
          (expect-api-called 'POST "quizzes/42/items")))))

  (it "uses PATCH for existing items"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data (list :title "Existing Item" :canvas-id "item-99"
                          :quiz-assignment-id "42" :pom (point-marker))))
          (org-canvas--new-quiz-item-push-to-api
           data (make-hash-table :test 'equal))
          (expect-api-called 'PATCH "quizzes/42/items/item-99")))))

  (it "retries as POST on 404 for PATCH"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (and (eq method 'PATCH) (= call-count 1))
                         (error "HTTP 404 Not Found")
                       '((id . "new-item-id"))))))
          (let* ((data (list :title "Stale Item" :canvas-id "old-id"
                             :quiz-assignment-id "42" :pom (point-marker)))
                 (response (org-canvas--new-quiz-item-push-to-api
                            data (make-hash-table :test 'equal))))
            (expect call-count :to-equal 2)
            (expect (alist-get 'id response) :to-equal "new-item-id"))))))

  (it "signals error when POST retry also fails on 404"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (error "HTTP 404 Not Found"))))
        (let ((data (list :title "Bad" :canvas-id "old"
                          :quiz-assignment-id "42" :pom (point-marker))))
          (expect (org-canvas--new-quiz-item-push-to-api
                   data (make-hash-table :test 'equal))
                  :to-throw 'error)))))

  (it "signals error on non-404 API failure"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_m _u &rest _) (error "HTTP 500"))))
        (let ((data (list :title "Bad Item" :canvas-id nil
                          :quiz-assignment-id "42" :pom (point-marker))))
          (expect (org-canvas--new-quiz-item-push-to-api
                   data (make-hash-table :test 'equal))
                  :to-throw 'error)))))

  (it "wraps payload in nested item > entry structure"
    (with-org-canvas-test-config
      (let (sent-data)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq sent-data (plist-get args :data))
                     '((id . "item-1")))))
          (let* ((data (list :title "Wrap Item" :canvas-id nil
                             :quiz-assignment-id "42" :pom (point-marker)))
                 (payload (make-hash-table :test 'equal)))
            (puthash "item_body" "<p>Question</p>" payload)
            (puthash "interaction_type_slug" "choice" payload)
            (puthash "points_possible" 5 payload)
            (puthash "entry_type" "Item" payload)
            (puthash "interaction_data" '((some . data)) payload)
            (puthash "scoring_data" '((value . "choice_0")) payload)
            (puthash "scoring_algorithm" "Equivalence" payload)
            (org-canvas--new-quiz-item-push-to-api data payload)
            ;; Verify outer wrapping
            (expect (hash-table-p sent-data) :to-be t)
            (let* ((item (gethash "item" sent-data))
                   (entry (gethash "entry" item)))
              ;; Item-level fields
              (expect (gethash "entry_type" item) :to-equal "Item")
              (expect (gethash "points_possible" item) :to-equal 5)
              ;; Entry-level fields
              (expect (gethash "item_body" entry) :to-equal "<p>Question</p>")
              (expect (gethash "interaction_type_slug" entry) :to-equal "choice")
              (expect (gethash "interaction_data" entry) :to-equal '((some . data)))
              ;; Scoring fields in entry
              (expect (gethash "scoring_data" entry) :to-equal '((value . "choice_0")))
              (expect (gethash "scoring_algorithm" entry) :to-equal "Equivalence"))))))))

;;;; Item Finalize

(describe "org-canvas--new-quiz-item-finalize"
  (it "saves CANVAS_ITEM_ID from response"
    (with-temp-org-buffer
     "* Quiz
** Item to Finalize
:PROPERTIES:
:END:
"
     (search-forward "Item to Finalize")
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Item to Finalize" :pom pom))
            (response '((id . "item-uuid-abc"))))
       (org-canvas--new-quiz-item-finalize data response)
       (expect (org-entry-get pom "CANVAS_ITEM_ID")
               :to-equal "item-uuid-abc"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Quiz
** Item TS
:PROPERTIES:
:END:
"
     (search-forward "Item TS")
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Item TS" :pom pom))
            (response '((id . "abc"))))
       (org-canvas--new-quiz-item-finalize data response)
       (expect-synced-timestamp pom))))

  (it "handles missing id in response gracefully"
    (with-temp-org-buffer
     "* Quiz
** No ID Item
:PROPERTIES:
:END:
"
     (search-forward "No ID Item")
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "No ID Item" :pom pom))
            (response '((status . "ok"))))
       ;; Should not throw, but warn
       (spy-on 'elog-warning)
       (org-canvas--new-quiz-item-finalize data response)
       (expect 'elog-warning :to-have-been-called)))))

;;;; Sync Item Loop

(describe "org-canvas--sync-new-quiz-items"
  (it "syncs all level-2 items under a quiz"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ASSIGNMENT_ID: 42
:END:

** Q1
:PROPERTIES:
:TYPE: choice
:END:

- [X] Yes
- [ ] No

** Q2
:PROPERTIES:
:TYPE: essay
:END:
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_m _u &rest _)
                    '((id . "item-1")))))
         (let ((results (org-canvas--sync-new-quiz-items (point-marker) "42")))
           (expect (car results) :to-equal 2)
           (expect (cdr results) :to-equal 0))))))

  (it "handles item sync failures gracefully"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ASSIGNMENT_ID: 42
:END:

** Q1
:PROPERTIES:
:TYPE: choice
:END:

- [X] Yes
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_m _u &rest _) (error "HTTP 500"))))
         (let ((results (org-canvas--sync-new-quiz-items (point-marker) "42")))
           (expect (car results) :to-equal 0)
           (expect (cdr results) :to-equal 1))))))

  (it "returns zero counts when no items"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ASSIGNMENT_ID: 42
:END:
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (with-mock-api
         (let ((results (org-canvas--sync-new-quiz-items (point-marker) "42")))
           (expect (car results) :to-equal 0)
           (expect (cdr results) :to-equal 0)))))))

;;;; Main Sync Function

(describe "org-canvas-sync-new-quizzes"
  (it "syncs quizzes and items from file"
    (let* ((temp-dir (make-temp-file "nq-sync-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file))
            (with-temp-file test-file
              (insert "* Test Quiz
:PROPERTIES:
:END:

** Q1
:PROPERTIES:
:TYPE: choice
:END:

- [X] Yes
- [ ] No
"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_m _u &rest _)
                           '((assignment_id . 42) (id . "item-1")))))
                (with-sync-test-env
                  (org-canvas-sync-new-quizzes)
                  (let ((content (with-temp-buffer
                                   (insert-file-contents test-file)
                                   (buffer-string))))
                    (expect content :to-match "CANVAS_ASSIGNMENT_ID"))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "errors when file not found"
    (let ((org-canvas-new-quizzes-file "/tmp/nonexistent/new-quizzes.org"))
      (with-sync-test-env
        (expect (org-canvas-sync-new-quizzes) :to-throw 'error))))

  (it "handles quiz sync failures gracefully"
    (let* ((temp-dir (make-temp-file "nq-fail-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file))
            (with-temp-file test-file
              (insert "* Quiz A
:PROPERTIES:
:END:

* Quiz B
:PROPERTIES:
:END:
"))
            (with-org-canvas-test-config
              (let ((fail-first t))
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_m _u &rest _)
                             (if fail-first
                                 (progn (setq fail-first nil)
                                        (error "API Error"))
                               '((assignment_id . 999))))))
                  (with-sync-test-env
                    ;; Should not throw; first quiz fails, second succeeds
                    (org-canvas-sync-new-quizzes))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Sync at Point

(describe "org-canvas-sync-new-quiz-at-point"
  (it "syncs single quiz at point"
    (with-temp-org-buffer
     "* My Quiz
:PROPERTIES:
:END:

** Q1
:PROPERTIES:
:TYPE: choice
:END:

- [X] Right
- [ ] Wrong
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_m _u &rest _)
                    '((assignment_id . 42) (id . "item-1")))))
         (org-canvas-sync-new-quiz-at-point)
         (expect (org-entry-get (point) "CANVAS_ASSIGNMENT_ID") :to-be-truthy)))))

  (it "errors when not on level-1 heading"
    (with-temp-org-buffer
     "* Quiz
** Item
:PROPERTIES:
:END:
"
     (search-forward "Item")
     (org-back-to-heading)
     (expect (org-canvas-sync-new-quiz-at-point) :to-throw 'user-error))))

;;;; Delete Functions

(describe "org-canvas-delete-all-new-quizzes"
  (it "deletes all new quizzes from Canvas"
    (let* ((temp-dir (make-temp-file "nq-delete-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file)
                (deleted-urls nil))
            (with-temp-file test-file
              (insert "* Quiz A
:PROPERTIES:
:CANVAS_ASSIGNMENT_ID: 100
:END:

* Quiz B
:PROPERTIES:
:CANVAS_ASSIGNMENT_ID: 200
:END:
"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_m _u &optional _p)
                           '(((assignment_id . 100) (title . "Quiz A"))
                             ((assignment_id . 200) (title . "Quiz B")))))
                        ((symbol-function 'org-canvas-api-request)
                         (lambda (method url &rest _)
                           (when (eq method 'DELETE)
                             (push url deleted-urls))
                           nil))
                        ((symbol-function 'y-or-n-p) (lambda (_) t)))
                (with-sync-test-env
                  (org-canvas-delete-all-new-quizzes)
                  (expect (length deleted-urls) :to-equal 2)))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "aborts when user declines"
    (let ((org-canvas-new-quizzes-file "/tmp/test.org"))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (expect (org-canvas-delete-all-new-quizzes)
                :to-throw 'user-error))))

  (it "handles delete failures gracefully"
    (let* ((temp-dir (make-temp-file "nq-delete-fail" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file))
            (with-temp-file test-file
              (insert "* Quiz\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 100\n:END:\n"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_m _u &optional _p)
                           '(((assignment_id . 100) (title . "Quiz")))))
                        ((symbol-function 'org-canvas-api-request)
                         (lambda (_m _u &rest _)
                           (error "DELETE failed")))
                        ((symbol-function 'y-or-n-p) (lambda (_) t)))
                (with-sync-test-env
                  ;; Should not throw
                  (org-canvas-delete-all-new-quizzes)))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Type Slug Mapping

(describe "org-canvas--new-quiz-type-slugs"
  (it "maps all Org types to API slugs"
    (expect (cdr (assoc "choice" org-canvas--new-quiz-type-slugs))
            :to-equal "choice")
    (expect (cdr (assoc "true-false" org-canvas--new-quiz-type-slugs))
            :to-equal "true-false")
    (expect (cdr (assoc "multi-answer" org-canvas--new-quiz-type-slugs))
            :to-equal "multi-answer")
    (expect (cdr (assoc "short-answer" org-canvas--new-quiz-type-slugs))
            :to-equal "essay")
    (expect (cdr (assoc "essay" org-canvas--new-quiz-type-slugs))
            :to-equal "essay")
    (expect (cdr (assoc "file-upload" org-canvas--new-quiz-type-slugs))
            :to-equal "file-upload")
    (expect (cdr (assoc "numerical" org-canvas--new-quiz-type-slugs))
            :to-equal "numeric")
    (expect (cdr (assoc "matching" org-canvas--new-quiz-type-slugs))
            :to-equal "matching")
    (expect (cdr (assoc "ordering" org-canvas--new-quiz-type-slugs))
            :to-equal "ordering")
    (expect (cdr (assoc "categorization" org-canvas--new-quiz-type-slugs))
            :to-equal "categorization")
    (expect (cdr (assoc "fill-in-the-blank" org-canvas--new-quiz-type-slugs))
            :to-equal "rich-fill-blank")
    (expect (cdr (assoc "hot-spot" org-canvas--new-quiz-type-slugs))
            :to-equal "hot-spot")))

(describe "org-canvas--new-quiz-scoring-algorithms"
  (it "maps all types to scoring algorithms"
    (expect (cdr (assoc "choice" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "Equivalence")
    (expect (cdr (assoc "multi-answer" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "AllOrNothing")
    (expect (cdr (assoc "matching" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "DeepEquals")
    (expect (cdr (assoc "categorization" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "Categorization")
    (expect (cdr (assoc "numerical" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "Numeric")
    (expect (cdr (assoc "essay" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "None")))

(describe "org-canvas--new-quiz-item-scoring-algorithm"
  (it "returns Equivalence for choice"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "choice")
            :to-equal "Equivalence"))

  (it "returns AllOrNothing for multi-answer"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "multi-answer")
            :to-equal "AllOrNothing"))

  (it "returns DeepEquals for matching"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "matching")
            :to-equal "DeepEquals"))

  (it "returns Categorization for categorization"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "categorization")
            :to-equal "Categorization"))

  (it "returns Numeric for numerical"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "numerical")
            :to-equal "Numeric"))

  (it "returns None for essay"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "essay")
            :to-equal "None"))

  (it "returns None for short-answer (mapped to essay)"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "short-answer")
            :to-equal "None"))

  (it "returns None for file-upload"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "file-upload")
            :to-equal "None"))

  (it "returns Equivalence for unknown type"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "unknown")
            :to-equal "Equivalence")))

(describe "org-canvas--new-quiz-slug-to-type"
  (it "reverses slug to org type"
    (expect (org-canvas--new-quiz-slug-to-type "true-false")
            :to-equal "true-false")
    (expect (org-canvas--new-quiz-slug-to-type "multi-answer")
            :to-equal "multi-answer")
    (expect (org-canvas--new-quiz-slug-to-type "numeric")
            :to-equal "numerical")
    (expect (org-canvas--new-quiz-slug-to-type "rich-fill-blank")
            :to-equal "fill-in-the-blank")
    (expect (org-canvas--new-quiz-slug-to-type "choice")
            :to-equal "choice")
    (expect (org-canvas--new-quiz-slug-to-type "essay")
            :to-equal "essay"))

  (it "returns slug unchanged if not found"
    (expect (org-canvas--new-quiz-slug-to-type "unknown_type")
            :to-equal "unknown_type")))

;;;; Pull Functions

(describe "org-canvas--new-quiz-pull-set-properties"
  (it "sets all properties from API response"
    (with-temp-org-buffer
     "* Pull Test Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((pos (point)))
       (org-canvas--new-quiz-pull-set-properties
        pos '((assignment_id . 42)
              (time_limit . 60)
              (shuffle_answers . t)
              (one_at_a_time . t)
              (allowed_attempts . 3)
              (scoring_policy . "keep_highest")))
       (expect (org-entry-get pos "CANVAS_ASSIGNMENT_ID") :to-equal "42")
       (expect (org-entry-get pos "TIME_LIMIT") :to-equal "60")
       (expect (org-entry-get pos "SHUFFLE_ANSWERS") :to-equal "true")
       (expect (org-entry-get pos "ONE_AT_A_TIME") :to-equal "true")
       (expect (org-entry-get pos "ALLOWED_ATTEMPTS") :to-equal "3")
       (expect (org-entry-get pos "SCORING_POLICY") :to-equal "keep_highest")))))

(describe "org-canvas--new-quiz-pull-insert-item"
  (it "inserts item as L2 heading with properties"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ASSIGNMENT_ID: 42
:END:
"
     (org-back-to-heading)
     (org-canvas--new-quiz-pull-insert-item
      '((id . "item-abc")
        (item_body . "What is 2+2?")
        (interaction_type_slug . "choice")
        (points_possible . 5)))
     (let ((content (buffer-string)))
       (expect content :to-match "What is 2\\+2\\?")
       (expect content :to-match "CANVAS_ITEM_ID")
       (expect content :to-match "choice")
       (expect content :to-match "POINTS:.*5"))))

  (it "strips HTML tags from title"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--new-quiz-pull-insert-item
      '((id . "item-1")
        (item_body . "<p>What is <b>bold</b>?</p>")
        (interaction_type_slug . "choice")
        (points_possible . 1)))
     (let ((content (buffer-string)))
       (expect content :to-match "What is bold\\?"))))

  (it "uses default title when body is empty"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--new-quiz-pull-insert-item
      '((id . "item-2")
        (item_body . "")
        (interaction_type_slug . "essay")
        (points_possible . 10)))
     (expect (buffer-string) :to-match "Question"))))

(describe "org-canvas-pull-new-quizzes"
  (it "creates quiz headings from API response"
    (let* ((temp-dir (make-temp-file "nq-pull-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method url &optional _params)
                           (if (string-match-p "items" url)
                               nil
                             '(((assignment_id . 42) (title . "Midterm")
                                (time_limit . 60) (shuffle_answers . t))))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-new-quizzes)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (expect content :to-match "Midterm")
                  (expect content :to-match "CANVAS_ASSIGNMENT_ID: 42")
                  (expect content :to-match "TIME_LIMIT: 60")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "pulls quiz items as L2 headings"
    (let* ((temp-dir (make-temp-file "nq-pull-items-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method url &optional _params)
                           (if (string-match-p "items" url)
                               '(((id . "item-1")
                                  (item_body . "What is 2+2?")
                                  (interaction_type_slug . "choice")
                                  (points_possible . 5)))
                             '(((assignment_id . 42) (title . "Quiz"))))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-new-quizzes)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (expect content :to-match "What is 2\\+2\\?")
                  (expect content :to-match "CANVAS_ITEM_ID")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Validation Integration

(describe "org-canvas-validate with new-quizzes"
  (it "validates new quiz properties"
    (let* ((temp-dir (make-temp-file "nq-validate-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file)
                (org-canvas-assignments-file "/tmp/nonexistent/a.org")
                (org-canvas-pages-file "/tmp/nonexistent/p.org")
                (org-canvas-quizzes-file "/tmp/nonexistent/q.org")
                (org-canvas-modules-file "/tmp/nonexistent/m.org")
                (org-canvas-files-file "/tmp/nonexistent/f.org")
                (org-canvas-outcomes-file "/tmp/nonexistent/o.org")
                (org-canvas-rubrics-file "/tmp/nonexistent/r.org")
                (org-canvas-discussions-file "/tmp/nonexistent/d.org")
                (org-canvas-announcements-file "/tmp/nonexistent/ann.org")
                (org-canvas-assignment-groups-file "/tmp/nonexistent/ag.org")
                (org-canvas-sections-file "/tmp/nonexistent/s.org")
                (org-canvas-settings-file "/tmp/nonexistent/set.org"))
            (with-temp-file test-file
              (insert "* Good Quiz
:PROPERTIES:
:TIME_LIMIT: 60
:SHUFFLE_ANSWERS: true
:SCORING_POLICY: keep_highest
:END:

** Good Item
:PROPERTIES:
:TYPE: choice
:POINTS: 5
:END:

* Bad Quiz
:PROPERTIES:
:TIME_LIMIT: not-a-number
:SCORING_POLICY: invalid_policy
:END:

** Bad Item
:PROPERTIES:
:TYPE: invalid_type
:POINTS: abc
:END:
"))
            (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
              (let* ((result (org-canvas--validate-run-all-specs))
                     (issues (plist-get result :issues)))
                ;; Should find errors for invalid TIME_LIMIT, SCORING_POLICY, TYPE, POINTS
                (expect (length issues) :to-be-greater-than 0)
                (let ((error-issues (cl-remove-if-not
                                     (lambda (i) (eq (plist-get i :severity) 'error))
                                     issues)))
                  (expect (length error-issues) :to-be-greater-than 0)))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "passes validation for valid new quiz file"
    (let* ((temp-dir (make-temp-file "nq-validate-good-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file)
                (org-canvas-assignments-file "/tmp/nonexistent/a.org")
                (org-canvas-pages-file "/tmp/nonexistent/p.org")
                (org-canvas-quizzes-file "/tmp/nonexistent/q.org")
                (org-canvas-modules-file "/tmp/nonexistent/m.org")
                (org-canvas-files-file "/tmp/nonexistent/f.org")
                (org-canvas-outcomes-file "/tmp/nonexistent/o.org")
                (org-canvas-rubrics-file "/tmp/nonexistent/r.org")
                (org-canvas-discussions-file "/tmp/nonexistent/d.org")
                (org-canvas-announcements-file "/tmp/nonexistent/ann.org")
                (org-canvas-assignment-groups-file "/tmp/nonexistent/ag.org")
                (org-canvas-sections-file "/tmp/nonexistent/s.org")
                (org-canvas-settings-file "/tmp/nonexistent/set.org"))
            (with-temp-file test-file
              (insert "* Valid Quiz
:PROPERTIES:
:TIME_LIMIT: 60
:SHUFFLE_ANSWERS: true
:ONE_AT_A_TIME: false
:ALLOWED_ATTEMPTS: 3
:SCORING_POLICY: keep_average
:END:

** Valid Item
:PROPERTIES:
:TYPE: essay
:POINTS: 10
:END:
"))
            (let* ((result (org-canvas--validate-run-all-specs))
                   (issues (plist-get result :issues)))
              (expect issues :to-be nil)))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Constants

(describe "org-canvas--new-quiz-valid-types"
  (it "contains all expected types"
    (expect (length org-canvas--new-quiz-valid-types) :to-equal 12)
    (expect (member "choice" org-canvas--new-quiz-valid-types) :to-be-truthy)
    (expect (member "true-false" org-canvas--new-quiz-valid-types) :to-be-truthy)
    (expect (member "essay" org-canvas--new-quiz-valid-types) :to-be-truthy)
    (expect (member "hot-spot" org-canvas--new-quiz-valid-types) :to-be-truthy)))

(describe "org-canvas--new-quiz-valid-scoring-policies"
  (it "contains valid scoring policies"
    (expect (length org-canvas--new-quiz-valid-scoring-policies) :to-equal 3)
    (expect (member "keep_highest" org-canvas--new-quiz-valid-scoring-policies)
            :to-be-truthy)
    (expect (member "keep_average" org-canvas--new-quiz-valid-scoring-policies)
            :to-be-truthy)))

;;;; Configuration

(describe "org-canvas-new-quizzes-file"
  (it "defaults to new-quizzes.org in org-canvas-directory"
    (let ((org-canvas-directory "/tmp/test-course/"))
      (expect (org-canvas--path "new-quizzes.org")
              :to-equal "/tmp/test-course/new-quizzes.org"))))

;;; org-canvas-new-quizzes-test.el ends here
