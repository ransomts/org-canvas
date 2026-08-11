;;; org-canvas-new-quizzes-test.el --- Buttercup tests for New Quizzes  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;;; Transform Props (Pure)

(describe "org-canvas--new-quiz-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Midterm [2/5]"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :title) :to-equal "Midterm")))

  (it "interprets boolean shuffle as true"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw "true" :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :shuffle_answers) :to-be t)))

  (it "interprets boolean shuffle as nil when absent"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :shuffle_answers) :to-be nil)))

  (it "interprets one-at-a-time boolean"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw "true"
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :one_at_a_time) :to-be t)))

  (it "converts time-limit to number"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw "60"
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :time_limit) :to-equal 60)))

  (it "returns nil time-limit when absent"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :time_limit) :to-be nil)))

  (it "converts allowed-attempts to number"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw "3" :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :allowed_attempts) :to-equal 3)))

  (it "validates scoring policy with fallback"
    (spy-on 'org-canvas--log-warning)
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw "bad_policy"
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :scoring_policy) :to-equal "keep_highest")))

  (it "passes through valid scoring policy"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw "keep_latest"
                     :assignment-group-id nil :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :scoring_policy) :to-equal "keep_latest")))

  (it "converts assignment-group-id to number"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id "42" :rubric-id nil
                     :body-text "" :pom nil))))
      (expect (plist-get result :assignment_group_id) :to-equal 42)))

  (it "passes through canvas-id and rubric-id"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id "98765" :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id "rubric-1"
                     :body-text "" :pom nil))))
      (expect (plist-get result :canvas-id) :to-equal "98765")
      (expect (plist-get result :rubric-id) :to-equal "rubric-1")))

  (it "passes through body-text unchanged"
    (let ((result (org-canvas--new-quiz-transform-props
                   '(:title-raw "Quiz"
                     :canvas-id nil :time-limit-raw nil
                     :shuffle-raw nil :one-at-a-time-raw nil
                     :attempts-raw nil :scoring-policy-raw nil
                     :assignment-group-id nil :rubric-id nil
                     :body-text "Some description" :pom nil))))
      (expect (plist-get result :body-text) :to-equal "Some description"))))

(describe "org-canvas--new-quiz-item-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--new-quiz-item-transform-props
                   '(:title-raw "Question [1/3]"
                     :canvas-id nil :quiz-assignment-id "42"
                     :type-raw "choice" :points-raw "5"
                     :outcome nil :text "" :pom nil))))
      (expect (plist-get result :title) :to-equal "Question")))

  (it "validates type with fallback to choice"
    (spy-on 'org-canvas--log-warning)
    (let* ((result (org-canvas--new-quiz-item-transform-props
                    '(:title-raw "Q" :canvas-id nil
                      :quiz-assignment-id "42" :type-raw "invalid_type"
                      :points-raw nil :outcome nil :text "" :pom nil)))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "choice")))

  (it "defaults type to choice when absent"
    (let* ((result (org-canvas--new-quiz-item-transform-props
                    '(:title-raw "Q" :canvas-id nil
                      :quiz-assignment-id "42" :type-raw nil
                      :points-raw nil :outcome nil :text "" :pom nil)))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "choice")))

  (it "converts points to number"
    (let ((result (org-canvas--new-quiz-item-transform-props
                   '(:title-raw "Q" :canvas-id nil
                     :quiz-assignment-id "42" :type-raw "essay"
                     :points-raw "10" :outcome nil :text "" :pom nil))))
      (expect (plist-get result :points) :to-equal 10)))

  (it "defaults points to 1 when absent"
    (let ((result (org-canvas--new-quiz-item-transform-props
                   '(:title-raw "Q" :canvas-id nil
                     :quiz-assignment-id "42" :type-raw "essay"
                     :points-raw nil :outcome nil :text "" :pom nil))))
      (expect (plist-get result :points) :to-equal 1)))

  (it "defaults points to 1 when empty string"
    (let ((result (org-canvas--new-quiz-item-transform-props
                   '(:title-raw "Q" :canvas-id nil
                     :quiz-assignment-id "42" :type-raw "essay"
                     :points-raw "" :outcome nil :text "" :pom nil))))
      (expect (plist-get result :points) :to-equal 1)))

  (it "passes through quiz-assignment-id"
    (let ((result (org-canvas--new-quiz-item-transform-props
                   '(:title-raw "Q" :canvas-id nil
                     :quiz-assignment-id "quiz-99" :type-raw "choice"
                     :points-raw "5" :outcome nil :text "" :pom nil))))
      (expect (plist-get result :quiz-assignment-id) :to-equal "quiz-99")))

  (it "passes through canvas-id and outcome"
    (let ((result (org-canvas--new-quiz-item-transform-props
                   '(:title-raw "Q" :canvas-id "item-uuid"
                     :quiz-assignment-id "42" :type-raw "choice"
                     :points-raw "5"
                     :outcome "[[file:outcomes.org::*Skill][Skill]]"
                     :text "" :pom nil))))
      (expect (plist-get result :canvas-id) :to-equal "item-uuid")
      (expect (plist-get result :outcome)
              :to-equal "[[file:outcomes.org::*Skill][Skill]]")))

  (it "passes through body text"
    (let ((result (org-canvas--new-quiz-item-transform-props
                   '(:title-raw "Q" :canvas-id nil
                     :quiz-assignment-id "42" :type-raw "choice"
                     :points-raw "5" :outcome nil
                     :text "Consider this." :pom nil))))
      (expect (plist-get result :text) :to-equal "Consider this."))))

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
     (spy-on 'org-canvas--log-warning)
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

  (it "parses OUTCOME property when present"
    (with-temp-org-buffer
     "* Quiz
** Question with Outcome
:PROPERTIES:
:TYPE: choice
:POINTS: 5
:OUTCOME: [[file:outcomes.org::*Python Proficiency][Python Proficiency]]
:END:

- [X] Yes
- [ ] No
"
     (search-forward "Question with Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-parse-entry "42")))
       (expect (plist-get data :outcome)
               :to-equal "[[file:outcomes.org::*Python Proficiency][Python Proficiency]]"))))

  (it "returns nil for OUTCOME when absent"
    (with-temp-org-buffer
     "* Quiz
** Question without Outcome
:PROPERTIES:
:TYPE: essay
:POINTS: 10
:END:
"
     (search-forward "Question without Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-parse-entry "42")))
       (expect (plist-get data :outcome) :to-be nil))))

  (it "includes body text in question-text when present"
    (with-temp-org-buffer
     "* Quiz
** What is 2+2?
:PROPERTIES:
:TYPE: choice
:END:

Consider the following expression.

- [X] 4
- [ ] 3
"
     (search-forward "What is 2+2")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-parse-entry "42")))
       ;; text-html should contain both the title and the body text
       (expect (plist-get data :text-html) :to-match "What is 2\\+2")
       (expect (plist-get data :text-html) :to-match "Consider the following"))))

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
     (spy-on 'org-canvas--log-warning)
     (let* ((data (org-canvas--new-quiz-item-parse-entry "42"))
            (result-type (plist-get data :type)))
       ;; Should fall back to default "choice"
       (expect result-type :to-equal "choice")))))

;;;; Interaction Data Building

(describe "org-canvas--new-quiz-item-build-choice-data"
  (it "builds choices as array with item_body wrapped in <p> tags"
    (let ((data (org-canvas--new-quiz-item-build-choice-data
                 '(("Option A" . t) ("Option B" . nil) ("Option C" . nil)))))
      (let* ((choices (append (alist-get 'choices data) nil))
             (first (nth 0 choices)))
        (expect (length choices) :to-equal 3)
        (expect (alist-get 'item_body first) :to-equal "<p>Option A</p>")
        ;; No per-choice scoring_data
        (expect (alist-get 'scoring_data first) :to-be nil))))

  (it "assigns UUID IDs and sequential positions"
    (let* ((data (org-canvas--new-quiz-item-build-choice-data
                  '(("A" . t) ("B" . nil))))
           (choices (append (alist-get 'choices data) nil)))
      ;; IDs are UUID-format strings
      (expect (alist-get 'id (nth 0 choices)) :to-match "^[0-9a-f]\\{20,\\}$")
      (expect (alist-get 'id (nth 1 choices)) :to-match "^[0-9a-f]\\{20,\\}$")
      ;; Different IDs
      (expect (alist-get 'id (nth 0 choices))
              :not :to-equal (alist-get 'id (nth 1 choices)))
      (expect (alist-get 'position (nth 0 choices)) :to-equal 1)
      (expect (alist-get 'position (nth 1 choices)) :to-equal 2)))

  (it "tracks correct answer IDs in _correct_ids"
    (let* ((data (org-canvas--new-quiz-item-build-choice-data
                  '(("A" . nil) ("B" . t) ("C" . t))))
           (choices (append (alist-get 'choices data) nil))
           (correct-ids (alist-get '_correct_ids data)))
      (expect (length correct-ids) :to-equal 2)
      ;; correct IDs should match the IDs of choices at positions 2 and 3
      (expect (nth 0 correct-ids) :to-equal (alist-get 'id (nth 1 choices)))
      (expect (nth 1 correct-ids) :to-equal (alist-get 'id (nth 2 choices))))))

(describe "org-canvas--new-quiz-item-build-true-false-data"
  (it "uses plain string choices"
    (let ((data (org-canvas--new-quiz-item-build-true-false-data
                 '(("True" . t) ("False" . nil)))))
      (expect (alist-get 'true_choice data) :to-equal "True")
      (expect (alist-get 'false_choice data) :to-equal "False")))

  (it "sets scoring_data value to boolean true when True is correct"
    (let ((data (org-canvas--new-quiz-item-build-true-false-data
                 '(("True" . t) ("False" . nil)))))
      (expect (alist-get 'value (alist-get 'scoring_data data))
              :to-equal t)))

  (it "sets scoring_data value to :json-false when False is correct"
    (let ((data (org-canvas--new-quiz-item-build-true-false-data
                 '(("True" . nil) ("False" . t)))))
      (expect (alist-get 'value (alist-get 'scoring_data data))
              :to-equal :json-false)))

  (it "handles case-insensitive True"
    (let ((data (org-canvas--new-quiz-item-build-true-false-data
                 '(("true" . t) ("false" . nil)))))
      (expect (alist-get 'value (alist-get 'scoring_data data))
              :to-equal t))))

(describe "org-canvas--new-quiz-item-build-fill-blank-data (used by short-answer)"
  (it "builds blanks array and one scoring entry per correct answer"
    (let ((data (org-canvas--new-quiz-item-build-fill-blank-data
                 '(("Paris" . t) ("paris" . t) ("London" . nil)))))
      ;; Has blanks array with one blank
      (let ((blanks (append (alist-get 'blanks data) nil)))
        (expect (length blanks) :to-equal 1)
        (expect (alist-get 'id (nth 0 blanks)) :to-match "^[0-9a-f]\\{20,\\}$"))
      ;; scoring_data has one entry per correct answer, each with string value
      (let* ((sd (alist-get 'scoring_data data))
             (entries (append (alist-get 'value sd) nil))
             (blank-id (alist-get 'id (nth 0 (append (alist-get 'blanks data) nil)))))
        (expect (length entries) :to-equal 2)
        ;; Each entry references the same blank
        (expect (alist-get 'id (nth 0 entries)) :to-equal blank-id)
        (expect (alist-get 'id (nth 1 entries)) :to-equal blank-id)
        ;; Each has Equivalence algorithm
        (expect (alist-get 'scoring_algorithm (nth 0 entries)) :to-equal "Equivalence")
        (expect (alist-get 'scoring_algorithm (nth 1 entries)) :to-equal "Equivalence")
        ;; Each nested scoring_data.value is a string
        (expect (alist-get 'value (alist-get 'scoring_data (nth 0 entries)))
                :to-equal "Paris")
        (expect (alist-get 'value (alist-get 'scoring_data (nth 1 entries)))
                :to-equal "paris")))))

(describe "org-canvas--new-quiz-item-build-matching-data"
  (it "builds questions with short numeric IDs and flat string answers"
    (let ((data (org-canvas--new-quiz-item-build-matching-data
                 '(("DNA" . "Deoxyribonucleic acid")
                   ("RNA" . "Ribonucleic acid")))))
      (let ((questions (append (alist-get 'questions data) nil))
            (answers (append (alist-get 'answers data) nil)))
        (expect (length questions) :to-equal 2)
        (expect (length answers) :to-equal 2)
        (expect (alist-get 'item_body (nth 0 questions)) :to-equal "DNA")
        ;; IDs are short numeric strings (no dashes)
        (expect (alist-get 'id (nth 0 questions)) :to-match "^[0-9]+$")
        ;; answers is a flat string array
        (expect (nth 0 answers) :to-equal "Deoxyribonucleic acid")
        (expect (nth 1 answers) :to-equal "Ribonucleic acid")
        ;; No per-question scoring_data
        (expect (alist-get 'scoring_data (nth 0 questions)) :to-be nil)))))

(describe "org-canvas--new-quiz-item-build-ordering-data"
  (it "builds choices as keyed hash-table with UUID IDs"
    (let ((data (org-canvas--new-quiz-item-build-ordering-data
                 '("First" "Second" "Third"))))
      (let ((choices-ht (alist-get 'choices data)))
        ;; choices is a hash-table keyed by UUID
        (expect (hash-table-p choices-ht) :to-be t)
        (expect (hash-table-count choices-ht) :to-equal 3)
        ;; Each value has id + item_body
        (let* ((keys (hash-table-keys choices-ht))
               (first-key (nth 0 keys))
               (first-val (gethash first-key choices-ht)))
          (expect first-key :to-match "^[0-9a-f]\\{20,\\}$")
          (expect (alist-get 'id first-val) :to-equal first-key)
          (expect (alist-get 'item_body first-val) :to-be-truthy)))
      ;; scoring_data value contains ordered IDs
      (let ((value (append (alist-get 'value (alist-get 'scoring_data data)) nil)))
        (expect (length value) :to-equal 3)
        ;; Each ID should be a UUID
        (expect (nth 0 value) :to-match "^[0-9a-f]\\{20,\\}$")
        ;; IDs should exist in the hash-table
        (expect (gethash (nth 0 value) (alist-get 'choices data)) :to-be-truthy)))))

(describe "org-canvas--new-quiz-item-build-categorization-data"
  (it "builds categories and distractors as keyed objects with UUID IDs"
    (let ((data (org-canvas--new-quiz-item-build-categorization-data
                 '(("Fruit" . ("apple" "banana"))
                   ("Vegetable" . ("carrot"))))))
      (let ((cats-ht (alist-get 'categories data))
            (dist-ht (alist-get 'distractors data))
            (cat-order (append (alist-get 'category_order data) nil))
            (flat (append (alist-get '_flat_distractors data) nil)))
        ;; Categories is a hash-table keyed by UUID
        (expect (hash-table-p cats-ht) :to-be t)
        (expect (hash-table-count cats-ht) :to-equal 2)
        ;; First category in order should be Fruit
        (let ((first-cat-id (nth 0 cat-order)))
          (expect first-cat-id :to-match "^[0-9a-f]\\{20,\\}$")
          (expect (alist-get 'item_body (gethash first-cat-id cats-ht))
                  :to-equal "Fruit"))
        ;; Distractors is a hash-table keyed by UUID
        (expect (hash-table-p dist-ht) :to-be t)
        (expect (hash-table-count dist-ht) :to-equal 3)
        ;; _flat_distractors has scoring_data linking to category UUIDs
        (expect (length flat) :to-equal 3)
        (let ((first-cat-id (nth 0 cat-order)))
          (expect (alist-get 'value (alist-get 'scoring_data (nth 0 flat)))
                  :to-equal first-cat-id))
        ;; category_order has 2 UUID entries
        (expect (length cat-order) :to-equal 2)
        (expect (nth 0 cat-order) :to-match "^[0-9a-f]\\{20,\\}$")))))

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
  (it "builds one entry per correct answer with string values"
    (let ((data (org-canvas--new-quiz-item-build-fill-blank-data
                 '(("correct1" . t) ("wrong" . nil) ("correct2" . t)))))
      (let* ((sd (alist-get 'scoring_data data))
             (entries (append (alist-get 'value sd) nil)))
        ;; One entry per correct answer (not per blank)
        (expect (length entries) :to-equal 2)
        ;; Each has Equivalence algorithm and string value
        (expect (alist-get 'scoring_algorithm (nth 0 entries)) :to-equal "Equivalence")
        (expect (alist-get 'value (alist-get 'scoring_data (nth 0 entries)))
                :to-equal "correct1")
        (expect (alist-get 'value (alist-get 'scoring_data (nth 1 entries)))
                :to-equal "correct2")
        ;; All entries share the same blank ID
        (expect (alist-get 'id (nth 0 entries))
                :to-equal (alist-get 'id (nth 1 entries)))))))

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

  (it "dispatches to fill-blank builder for short-answer"
    (with-temp-org-buffer
     "* Quiz
** SA
:PROPERTIES:
:END:

- [X] Answer1
- [X] Answer2
"
     (search-forward "SA")
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-item-build-interaction-data "short-answer")))
       (expect (alist-get 'blanks data) :to-be-truthy)
       (expect (alist-get 'scoring_data data) :to-be-truthy))))

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
  (it "extracts correct choice ID from _correct_ids for choice type"
    (let* ((idata `((choices . ,(vconcat
                                 '(((id . "abc-1") (position . 1) (item_body . "<p>A</p>"))
                                   ((id . "abc-2") (position . 2) (item_body . "<p>B</p>")))))
                    (_correct_ids . ("abc-1"))))
           (result (org-canvas--new-quiz-item-build-scoring-data "choice" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (alist-get 'value sd) :to-equal "abc-1")
      ;; _correct_ids should be removed from cleaned data
      (expect (alist-get '_correct_ids cleaned) :to-be nil)
      (expect (alist-get 'choices cleaned) :to-be-truthy)))

  (it "collects all correct IDs from _correct_ids for multi-answer type"
    (let* ((idata `((choices . ,(vconcat
                                 '(((id . "abc-1") (position . 1) (item_body . "<p>A</p>"))
                                   ((id . "abc-2") (position . 2) (item_body . "<p>B</p>"))
                                   ((id . "abc-3") (position . 3) (item_body . "<p>C</p>")))))
                    (_correct_ids . ("abc-1" "abc-3"))))
           (result (org-canvas--new-quiz-item-build-scoring-data "multi-answer" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (append (alist-get 'value sd) nil)
              :to-equal '("abc-1" "abc-3"))
      ;; _correct_ids should be removed
      (expect (alist-get '_correct_ids cleaned) :to-be nil)))

  (it "extracts and removes scoring_data from true-false"
    (let* ((idata `((true_choice . "True")
                    (false_choice . "False")
                    (scoring_data . ((value . t)))))
           (result (org-canvas--new-quiz-item-build-scoring-data "true-false" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (alist-get 'value sd) :to-equal t)
      (expect (alist-get 'scoring_data cleaned) :to-be nil)))

  (it "extracts scoring_data from short-answer and removes it"
    (let* ((idata `((blanks . ,(vector '((id . "blank1"))))
                    (scoring_data . ((value . ,(vector '((id . "blank1") (value . "Paris"))))))))
           (result (org-canvas--new-quiz-item-build-scoring-data "short-answer" idata))
           (sd (car result))
           (cleaned (cdr result)))
      (expect (alist-get 'value sd) :to-be-truthy)
      (expect (alist-get 'scoring_data cleaned) :to-be nil)
      (expect (alist-get 'blanks cleaned) :to-be-truthy)))

  (it "extracts scoring_data from ordering and removes it"
    (let* ((choices-ht (make-hash-table :test 'equal)))
      (puthash "abc-1" '((id . "abc-1") (item_body . "First")) choices-ht)
      (puthash "abc-2" '((id . "abc-2") (item_body . "Second")) choices-ht)
      (let* ((idata `((choices . ,choices-ht)
                      (scoring_data . ((value . ,(vector "abc-1" "abc-2"))))))
             (result (org-canvas--new-quiz-item-build-scoring-data "ordering" idata))
             (sd (car result))
             (cleaned (cdr result)))
        (expect (alist-get 'value sd) :to-be-truthy)
        (expect (alist-get 'scoring_data cleaned) :to-be nil)
        (expect (hash-table-p (alist-get 'choices cleaned)) :to-be t))))

  (it "builds question-to-answer-text map for matching"
    (let* ((idata `((questions . ,(vconcat
                                   '(((id . "12345") (item_body . "DNA"))
                                     ((id . "67890") (item_body . "RNA")))))
                    (answers . ,(vconcat '("Deoxyribonucleic acid" "Ribonucleic acid")))))
           (result (org-canvas--new-quiz-item-build-scoring-data "matching" idata))
           (sd (car result))
           (cleaned (cdr result)))
      ;; scoring_data value is a hash-table mapping question IDs to answer text
      (expect (hash-table-p (alist-get 'value sd)) :to-be t)
      (expect (gethash "12345" (alist-get 'value sd)) :to-equal "Deoxyribonucleic acid")
      (expect (gethash "67890" (alist-get 'value sd)) :to-equal "Ribonucleic acid")
      ;; questions and answers should still be present in cleaned data
      (expect (alist-get 'questions cleaned) :to-be-truthy)
      (expect (alist-get 'answers cleaned) :to-be-truthy)))

  (it "builds per-category scoring for categorization"
    (let* ((cats-ht (make-hash-table :test 'equal))
           (dist-ht (make-hash-table :test 'equal)))
      (puthash "abc-cat-1" '((id . "abc-cat-1") (item_body . "Fruit")) cats-ht)
      (puthash "abc-cat-2" '((id . "abc-cat-2") (item_body . "Veg")) cats-ht)
      (puthash "abc-item-1" '((id . "abc-item-1") (item_body . "apple")) dist-ht)
      (puthash "abc-item-2" '((id . "abc-item-2") (item_body . "carrot")) dist-ht)
      (let* ((idata `((categories . ,cats-ht)
                      (distractors . ,dist-ht)
                      (category_order . ,(vector "abc-cat-1" "abc-cat-2"))
                      (_flat_distractors
                       . ,(vconcat
                           '(((id . "abc-item-1") (item_body . "apple")
                              (scoring_data . ((value . "abc-cat-1"))))
                             ((id . "abc-item-2") (item_body . "carrot")
                              (scoring_data . ((value . "abc-cat-2")))))))))
             (result (org-canvas--new-quiz-item-build-scoring-data "categorization" idata))
             (sd (car result))
             (cleaned (cdr result)))
        ;; scoring_data value is a vector of per-category entries
        (let ((cats (append (alist-get 'value sd) nil)))
          (expect (length cats) :to-equal 2)
          (expect (alist-get 'id (nth 0 cats)) :to-equal "abc-cat-1")
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

)

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
     (let* ((interaction-data (org-canvas--new-quiz-item-build-interaction-data "choice"))
            (data (list :title "What is 2+2?"
                        :text ""
                        :type "choice"
                        :points 5
                        :text-html "<p>What is 2+2?</p>"
                        :interaction-data interaction-data))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       (expect (gethash "interaction_type_slug" payload) :to-equal "choice")
       (expect (gethash "points_possible" payload) :to-equal 5)
       (expect (gethash "entry_type" payload) :to-equal "Item")
       ;; item_body uses pre-computed HTML
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
     (let* ((interaction-data (org-canvas--new-quiz-item-build-interaction-data "true-false"))
            (data (list :title "TF Question"
                        :text ""
                        :type "true-false"
                        :points 1
                        :text-html "<p>TF Question</p>"
                        :interaction-data interaction-data))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       (expect (gethash "interaction_type_slug" payload) :to-equal "true-false")
       (expect (gethash "scoring_algorithm" payload) :to-equal "Equivalence")
       (expect (gethash "scoring_data" payload) :to-be-truthy))))

  (it "omits interaction_data for essay"
    ;; build-payload is now pure — no buffer needed
    (let* ((data (list :title "Write an essay"
                       :text ""
                       :type "essay"
                       :points 10
                       :text-html "<p>Write an essay</p>"
                       :interaction-data nil))
           (payload (org-canvas--new-quiz-item-build-payload data)))
      (expect (gethash "interaction_data" payload nil) :to-be nil)
      (expect (gethash "scoring_algorithm" payload) :to-equal "None")
      (expect (alist-get 'value (gethash "scoring_data" payload)) :to-equal "")))

  (it "falls back to raw type when not in type-slugs"
    ;; build-payload is now pure — no buffer needed
    (let* ((data (list :title "Unknown Q"
                       :text ""
                       :type "custom-widget"
                       :points 1
                       :text-html "<p>Unknown Q</p>"
                       :interaction-data nil))
           (payload (org-canvas--new-quiz-item-build-payload data)))
      ;; "custom-widget" is not in type-slugs, so slug = q-type itself
      (expect (gethash "interaction_type_slug" payload) :to-equal "custom-widget")))

  (it "uses pre-computed text-html as item_body"
    ;; build-payload is now pure — no buffer needed
    (let* ((data (list :title "Capital of France?"
                       :text ""
                       :type "short-answer"
                       :points 1
                       :text-html "<p>Capital of France?</p>"
                       :interaction-data nil))
           (payload (org-canvas--new-quiz-item-build-payload data)))
      (expect (gethash "item_body" payload) :to-match "Capital of France")))

  (it "uses pre-computed text-html with combined title and body"
    ;; build-payload is now pure — no buffer needed
    (let* ((data (list :title "Geography"
                       :text "What is the capital of France?"
                       :type "short-answer"
                       :points 1
                       :text-html "<p>Geography\n\nWhat is the capital of France?</p>"
                       :interaction-data nil))
           (payload (org-canvas--new-quiz-item-build-payload data)))
      (expect (gethash "item_body" payload) :to-match "Geography")
      (expect (gethash "item_body" payload) :to-match "capital of France")))

  (it "sends no interaction_data for numerical type"
    (with-temp-org-buffer
     "* Quiz
** What is pi?
:PROPERTIES:
:TYPE: numerical
:END:

- [X] 3.14
"
     (search-forward "What is pi")
     (org-back-to-heading)
     (unless test-org-canvas-emacs-30-p
       (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
     (let* ((interaction-data (org-canvas--new-quiz-item-build-interaction-data "numerical"))
            (data (list :title "What is pi?"
                        :text ""
                        :type "numerical"
                        :points 1
                        :text-html "<p>What is pi?</p>"
                        :interaction-data interaction-data))
            (payload (org-canvas--new-quiz-item-build-payload data)))
       ;; numerical interaction_data should be nil (Canvas stores null)
       (expect (gethash "interaction_data" payload nil) :to-be nil)
       ;; but scoring_data should still be present
       (expect (gethash "scoring_data" payload) :to-be-truthy)))))

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
       (spy-on 'org-canvas--log-warning)
       (org-canvas--new-quiz-item-finalize data response)
       (expect 'org-canvas--log-warning :to-have-been-called)))))

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
           (expect (cdr results) :to-equal 0))))))

  (it "skips items not in debug-types list"
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
       (let ((org-canvas--new-quiz-debug-types '("choice")))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (_m _u &rest _)
                      '((id . "item-1")))))
           (let ((results (org-canvas--sync-new-quiz-items (point-marker) "42")))
             ;; Only choice item should be synced, essay skipped
             (expect (car results) :to-equal 1)
             (expect (cdr results) :to-equal 0))))))))

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

  (it "uses id fallback when assignment_id absent in sync response"
    (let* ((temp-dir (make-temp-file "nq-id-fb-test" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file))
            (with-temp-file test-file
              (insert "* ID Fallback Quiz\n:PROPERTIES:\n:END:\n\n** Q1\n:PROPERTIES:\n:TYPE: choice\n:END:\n\n- [X] Yes\n- [ ] No\n"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_m _u &rest _)
                           ;; Response has id but no assignment_id
                           '((id . "fallback-77")))))
                (with-sync-test-env
                  (org-canvas-sync-new-quizzes)
                  (let ((content (with-temp-buffer
                                   (insert-file-contents test-file)
                                   (buffer-string))))
                    (expect content :to-match "CANVAS_ASSIGNMENT_ID"))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "sends a create payload whose wrapped body reflects the org input"
    (let ((temp-dir (make-temp-file "nq-body-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "new-quizzes.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Cellular Respiration Quiz
:PROPERTIES:
:TIME_LIMIT: 45
:SHUFFLE_ANSWERS: true
:ALLOWED_ATTEMPTS: 2
:SCORING_POLICY: keep_latest
:END:

** Q1
:PROPERTIES:
:TYPE: choice
:END:

- [X] Yes
- [ ] No
"))
            (let ((org-canvas-new-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  ;; Quiz-create POST must return assignment_id so the
                  ;; pipeline can finalize and proceed to items.
                  (setq test-org-canvas-api-responses
                        '(("/quizzes" . ((assignment_id . 555) (id . "item-1")))))
                  (org-canvas-sync-new-quizzes)
                  ;; "/quizzes$" matches only the create call, not the
                  ;; ".../quizzes/555/items" item call.
                  (let* ((body (test-org-canvas-api-call-data 'POST "/quizzes$"))
                         (quiz (and (hash-table-p body) (gethash "quiz" body))))
                    (expect (hash-table-p body) :to-be t)
                    (expect (hash-table-p quiz) :to-be t)
                    (expect (gethash "title" quiz)
                            :to-equal "Cellular Respiration Quiz")
                    (expect (gethash "time_limit" quiz) :to-equal 45)
                    (expect (gethash "shuffle_answers" quiz) :to-be t)
                    (expect (gethash "allowed_attempts" quiz) :to-equal 2)
                    (expect (gethash "scoring_policy" quiz)
                            :to-equal "keep_latest"))))))
        (let ((buf (find-buffer-visiting
                    (expand-file-name "new-quizzes.org" temp-dir))))
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

  (it "uses id fallback for quiz-id when assignment_id absent at point"
    (with-temp-org-buffer
     "* ID Fallback Quiz
:PROPERTIES:
:END:

** Q1
:PROPERTIES:
:TYPE: choice
:END:

- [X] Yes
- [ ] No
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_m _u &rest _)
                    '((id . "only-id-99")))))
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
        (delete-directory temp-dir t))))

  (it "uses id fallback when assignment_id absent in delete-all"
    (let* ((temp-dir (make-temp-file "nq-delete-id-fb" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file)
                (deleted-urls nil))
            (with-temp-file test-file
              (insert "* Quiz\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 50\n:END:\n"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_m _u &optional _p)
                           ;; Response has id but no assignment_id
                           '(((id . 50) (title . "Quiz No AssignID")))))
                        ((symbol-function 'org-canvas-api-request)
                         (lambda (method url &rest _)
                           (when (eq method 'DELETE)
                             (push url deleted-urls))
                           nil))
                        ((symbol-function 'y-or-n-p) (lambda (_) t)))
                (with-sync-test-env
                  (org-canvas-delete-all-new-quizzes)
                  ;; Should have extracted id=50 and formed the delete URL
                  (expect (length deleted-urls) :to-equal 1)
                  (expect (car deleted-urls) :to-match "50")))))
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
            :to-equal "rich-fill-blank")
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
    (expect (cdr (assoc "hot-spot" org-canvas--new-quiz-type-slugs))
            :to-equal "hot-spot")))

(describe "org-canvas--new-quiz-scoring-algorithms"
  (it "maps all types to scoring algorithms"
    (expect (cdr (assoc "choice" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "Equivalence")
    (expect (cdr (assoc "multi-answer" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "PartialScore")
    (expect (cdr (assoc "matching" org-canvas--new-quiz-scoring-algorithms))
            :to-equal "PartialDeep")
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

  (it "returns PartialScore for multi-answer"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "multi-answer")
            :to-equal "PartialScore"))

  (it "returns PartialDeep for matching"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "matching")
            :to-equal "PartialDeep"))

  (it "returns Categorization for categorization"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "categorization")
            :to-equal "Categorization"))

  (it "returns Numeric for numerical"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "numerical")
            :to-equal "Numeric"))

  (it "returns None for essay"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "essay")
            :to-equal "None"))

  (it "returns MultipleMethods for short-answer"
    (expect (org-canvas--new-quiz-item-scoring-algorithm "short-answer")
            :to-equal "MultipleMethods"))

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
            :to-equal "short-answer")
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

(describe "org-canvas--new-quiz-pull-set-properties id fallback"
  (it "uses id when assignment_id is absent"
    (with-temp-org-buffer
     "* Pull Fallback Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((pos (point)))
       (org-canvas--new-quiz-pull-set-properties
        pos '((id . 77)
              (time_limit . 30)
              (shuffle_answers . nil)
              (one_at_a_time . nil)
              (allowed_attempts . 1)
              (scoring_policy . "keep_latest")))
       (expect (org-entry-get pos "CANVAS_ASSIGNMENT_ID") :to-equal "77")
       (expect (org-entry-get pos "TIME_LIMIT") :to-equal "30")))))

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

  (it "converts HTML title via html-to-org-inline"
    (cl-letf (((symbol-function 'org-canvas--html-to-org-inline)
               (lambda (html) (if (string-empty-p html) "" "Converted Title"))))
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
         (expect content :to-match "Converted Title")))))

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
        (delete-directory temp-dir t))))

  (it "uses id fallback when assignment_id absent in pull response"
    (let* ((temp-dir (make-temp-file "nq-pull-id-fb" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-new-quizzes-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method url &optional _params)
                           (if (string-match-p "items" url)
                               nil
                             ;; Response has id but no assignment_id
                             '(((id . 88) (title . "ID-Only Quiz")
                                (time_limit . 45))))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-new-quizzes)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (expect content :to-match "ID-Only Quiz")
                  (expect content :to-match "CANVAS_ASSIGNMENT_ID: 88")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "writes self-doc header when no new quizzes exist"
    (let* ((temp-dir (make-temp-file "nq-pull-empty" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "stale content\n* Old\n"))
            (let ((org-canvas-new-quizzes-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil)))
                  (org-canvas-pull-new-quizzes)
                  (with-temp-buffer
                    (insert-file-contents test-file)
                    (let ((s (buffer-string)))
                      (expect s :not :to-match "stale content")
                      (expect s :to-match "^#\\+TITLE: New Quizzes$")
                      (expect s :to-match "^# Canvas returned 0 items")))))))
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

(describe "org-canvas--valid-new-quiz-types"
  (it "contains all expected types"
    (expect (length org-canvas--valid-new-quiz-types) :to-equal 12)
    (expect (member "choice" org-canvas--valid-new-quiz-types) :to-be-truthy)
    (expect (member "true-false" org-canvas--valid-new-quiz-types) :to-be-truthy)
    (expect (member "essay" org-canvas--valid-new-quiz-types) :to-be-truthy)
    (expect (member "hot-spot" org-canvas--valid-new-quiz-types) :to-be-truthy)))

(describe "org-canvas--valid-new-quiz-scoring-policies"
  (it "contains valid scoring policies"
    (expect (length org-canvas--valid-new-quiz-scoring-policies) :to-equal 3)
    (expect (member "keep_highest" org-canvas--valid-new-quiz-scoring-policies)
            :to-be-truthy)
    (expect (member "keep_average" org-canvas--valid-new-quiz-scoring-policies)
            :to-be-truthy)))

;;;; Configuration

(describe "org-canvas-new-quizzes-file"
  (it "defaults to new-quizzes.org in org-canvas-directory"
    (let ((org-canvas-directory "/tmp/test-course/"))
      (expect (org-canvas--path "new-quizzes.org")
              :to-equal "/tmp/test-course/new-quizzes.org"))))

;;;; GROUP Link Resolution

(describe "org-canvas--new-quiz-parse-entry GROUP link"
  (it "resolves GROUP link to assignment_group_id"
    (let ((groups-file (make-temp-file "test-groups" nil ".org"))
          (nq-file (make-temp-file "test-nq" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Exams\n:PROPERTIES:\n:CANVAS_ID: 777\n:END:\n"))
            (with-temp-file nq-file
              (insert (format "* Midterm\n:PROPERTIES:\n:GROUP: [[file:%s::*Exams][Exams]]\n:END:\n\nQuiz description.\n"
                              groups-file)))
            (let ((org-canvas-new-quizzes-file nq-file))
              (with-current-buffer (find-file-noselect nq-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--new-quiz-parse-entry)))
                  (expect (plist-get data :assignment_group_id) :to-equal 777)))))
        (let ((buf (find-buffer-visiting groups-file)))
          (when buf (kill-buffer buf)))
        (let ((buf (find-buffer-visiting nq-file)))
          (when buf (kill-buffer buf)))
        (delete-file groups-file)
        (delete-file nq-file))))

  (it "returns nil assignment_group_id when no GROUP"
    (with-temp-org-buffer
     "* Simple Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-parse-entry)))
       (expect (plist-get data :assignment_group_id) :to-be nil)))))

(describe "org-canvas--new-quiz-build-payload GROUP"
  (it "includes assignment_group_id when present"
    (let* ((data (list :title "Exam"
                       :assignment_group_id 42))
           (payload (org-canvas--new-quiz-build-payload data)))
      (expect (gethash "assignment_group_id" payload) :to-equal 42)))

  (it "omits assignment_group_id when nil"
    (let* ((data (list :title "Quiz"
                       :assignment_group_id nil))
           (payload (org-canvas--new-quiz-build-payload data)))
      (expect (gethash "assignment_group_id" payload nil) :to-be nil))))

;;;; RUBRIC_LINK Tests

(describe "org-canvas-sync-new-quizzes RUBRIC_LINK integration"
  (it "calls associate-rubric during bulk sync when RUBRIC_LINK is set"
    (let* ((temp-dir (make-temp-file "nq-rubric-sync" t))
           (test-file (expand-file-name "new-quizzes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir))
           (assoc-calls nil))
      (unwind-protect
          (progn
            (with-temp-file rubrics-file
              (insert "* Lab Rubric\n:PROPERTIES:\n:CANVAS_ID: 777\n:END:\n\n| C | P |\n|---+---|\n| X | 5 |\n"))
            (with-temp-file test-file
              (insert (format "* Quiz With Rubric
:PROPERTIES:
:RUBRIC_LINK: [[file:%s::*Lab Rubric][Lab Rubric]]
:END:

Body text
" rubrics-file)))
            (let ((org-canvas-new-quizzes-file test-file)
                  (org-canvas-rubrics-file rubrics-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_m _u &rest _)
                             '((assignment_id . 8001) (id . "nq-1"))))
                          ((symbol-function 'org-canvas--associate-rubric)
                           (lambda (item-id rubric-id assoc-type)
                             (push (list item-id rubric-id assoc-type) assoc-calls))))
                  (with-sync-test-env
                    (org-canvas-sync-new-quizzes)
                    (expect (length assoc-calls) :to-equal 1)
                    (expect (nth 0 (car assoc-calls)) :to-equal 8001)
                    (expect (nth 1 (car assoc-calls)) :to-equal "777")
                    (expect (nth 2 (car assoc-calls)) :to-equal "Assignment"))))))
        (dolist (f (list test-file rubrics-file))
          (let ((buf (find-buffer-visiting f)))
            (when buf (kill-buffer buf))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--new-quiz-parse-entry RUBRIC_LINK"
  (it "resolves RUBRIC_LINK to rubric-id"
    (let ((rubrics-file (make-temp-file "test-rubrics" nil ".org"))
          (nq-file (make-temp-file "test-nq" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file rubrics-file
              (insert "* Lab Rubric\n:PROPERTIES:\n:CANVAS_ID: 123587\n:END:\n\n| C | P |\n|---+---|\n| X | 5 |\n"))
            (with-temp-file nq-file
              (insert (format "* Quiz A\n:PROPERTIES:\n:RUBRIC_LINK: [[file:%s::*Lab Rubric][Lab Rubric]]\n:END:\n\nBody text\n"
                              rubrics-file)))
            (let ((org-canvas-new-quizzes-file nq-file)
                  (org-canvas-rubrics-file rubrics-file))
              (with-current-buffer (find-file-noselect nq-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--new-quiz-parse-entry)))
                  (expect (plist-get data :rubric-id) :to-equal "123587")))))
        (let ((buf (find-buffer-visiting rubrics-file)))
          (when buf (kill-buffer buf)))
        (let ((buf (find-buffer-visiting nq-file)))
          (when buf (kill-buffer buf)))
        (delete-file rubrics-file)
        (delete-file nq-file))))

  (it "returns nil rubric-id when RUBRIC_LINK absent"
    (with-temp-org-buffer
     "* Quiz B
:PROPERTIES:
:END:

Body text
"
     (org-back-to-heading)
     (let ((data (org-canvas--new-quiz-parse-entry)))
       (expect (plist-get data :rubric-id) :to-be nil)))))

(describe "org-canvas-sync-new-quiz-at-point RUBRIC_LINK integration"
  (it "calls associate-rubric when RUBRIC_LINK is set"
    (with-org-canvas-test-config
      (let ((assoc-calls nil))
        (cl-letf (((symbol-function 'org-canvas--new-quiz-push-to-api)
                   (lambda (_data _payload)
                     '((id . "q1") (assignment_id . 5001))))
                  ((symbol-function 'org-canvas--new-quiz-finalize)
                   (lambda (_data _response) nil))
                  ((symbol-function 'org-canvas--sync-new-quiz-items)
                   (lambda (_marker _quiz-id) (cons 0 0)))
                  ((symbol-function 'org-canvas--associate-rubric)
                   (lambda (item-id rubric-id assoc-type)
                     (push (list item-id rubric-id assoc-type) assoc-calls)))
                  ((symbol-function 'save-buffer)
                   (lambda (&rest _) nil)))
          (with-temp-org-buffer
           "* Quiz With Rubric
:PROPERTIES:
:END:

Body text
"
           (org-back-to-heading)
           ;; Manually set rubric-id in parse result
           (cl-letf (((symbol-function 'org-canvas--new-quiz-parse-entry)
                      (lambda ()
                        (list :title "Quiz With Rubric"
                              :canvas-id nil
                              :rubric-id "999"
                              :pom (point-marker)))))
             (org-canvas-sync-new-quiz-at-point)
             (expect (length assoc-calls) :to-equal 1)
             (expect (nth 0 (car assoc-calls)) :to-equal 5001)
             (expect (nth 1 (car assoc-calls)) :to-equal "999")
             (expect (nth 2 (car assoc-calls)) :to-equal "Assignment")))))))

  (it "does not call associate-rubric when RUBRIC_LINK is absent"
    (with-org-canvas-test-config
      (let ((assoc-calls nil))
        (cl-letf (((symbol-function 'org-canvas--new-quiz-push-to-api)
                   (lambda (_data _payload)
                     '((id . "q2") (assignment_id . 5002))))
                  ((symbol-function 'org-canvas--new-quiz-finalize)
                   (lambda (_data _response) nil))
                  ((symbol-function 'org-canvas--sync-new-quiz-items)
                   (lambda (_marker _quiz-id) (cons 0 0)))
                  ((symbol-function 'org-canvas--associate-rubric)
                   (lambda (item-id rubric-id assoc-type)
                     (push (list item-id rubric-id assoc-type) assoc-calls)))
                  ((symbol-function 'save-buffer)
                   (lambda (&rest _) nil)))
          (with-temp-org-buffer
           "* Quiz Without Rubric
:PROPERTIES:
:END:

Body text
"
           (org-back-to-heading)
           (cl-letf (((symbol-function 'org-canvas--new-quiz-parse-entry)
                      (lambda ()
                        (list :title "Quiz Without Rubric"
                              :canvas-id nil
                              :rubric-id nil
                              :pom (point-marker)))))
             (org-canvas-sync-new-quiz-at-point)
             (expect assoc-calls :to-be nil))))))))

;;;; Coverage: quiz-id falls back to canvas-id when response lacks id fields (Lines 849, 901)

(describe "new quiz sync quiz-id fallback to canvas-id"
  (it "uses canvas-id when response has no assignment_id or id (at-point)"
    (with-org-canvas-test-config
      (let ((items-synced nil))
        (with-temp-org-buffer
         "* Quiz Fallback
:PROPERTIES:
:CANVAS_ID: 42
:END:

Body text
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'org-canvas--new-quiz-parse-entry)
                    (lambda ()
                      (list :title "Quiz Fallback"
                            :canvas-id 42
                            :rubric-id nil
                            :pom (point-marker))))
                   ((symbol-function 'org-canvas--new-quiz-build-payload)
                    (lambda (_data) '((title . "Quiz Fallback"))))
                   ((symbol-function 'org-canvas--new-quiz-push-to-api)
                    (lambda (_data _payload) '((status . "ok"))))
                   ((symbol-function 'org-canvas--new-quiz-finalize)
                    (lambda (_data _response) nil))
                   ((symbol-function 'org-canvas--sync-new-quiz-items)
                    (lambda (_marker quiz-id)
                      (setq items-synced quiz-id)
                      (cons 0 0)))
                   ((symbol-function 'save-buffer)
                    (lambda (&rest _) nil)))
           (org-canvas-sync-new-quiz-at-point)
           (expect items-synced :to-equal 42))))))

  (it "uses canvas-id when response has no assignment_id or id (batch sync)"
    (let* ((temp-dir (make-temp-file "nq-batch-" t))
           (nq-file (expand-file-name "new-quizzes.org" temp-dir))
           (items-synced nil))
      (unwind-protect
          (progn
            (with-temp-file nq-file
              (insert "* Quiz Batch\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 77\n:END:\n\nBody\n"))
            (let ((org-canvas-new-quizzes-file nq-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                          ((symbol-function 'display-buffer) #'ignore)
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_m _u &rest _)
                             '((assignment_id . 77))))
                          ((symbol-function 'org-canvas--sync-new-quiz-items)
                           (lambda (_marker quiz-id)
                             (setq items-synced quiz-id)
                             (cons 0 0))))
                  (org-canvas-sync-new-quizzes)
                  (expect items-synced :to-equal 77)))))
        (let ((buf (find-buffer-visiting nq-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Coverage: sync-children id fallback chain

(describe "org-canvas--new-quiz-sync-children"
  (it "uses id when assignment_id is absent"
    (with-org-canvas-test-config
      (let ((synced-quiz-id nil))
        (cl-letf (((symbol-function 'org-canvas--sync-new-quiz-items)
                   (lambda (_marker quiz-id)
                     (setq synced-quiz-id quiz-id))))
          (with-temp-org-buffer
           "* Quiz
:PROPERTIES:
:CANVAS_ID: 99
:END:
"
           (org-back-to-heading t)
           (org-canvas--new-quiz-sync-children
            '(:canvas-id "99" :rubric-id nil)
            '((id . 77)))
           (expect synced-quiz-id :to-equal 77))))))

  (it "falls back to canvas-id when response has no id fields"
    (with-org-canvas-test-config
      (let ((synced-quiz-id nil))
        (cl-letf (((symbol-function 'org-canvas--sync-new-quiz-items)
                   (lambda (_marker quiz-id)
                     (setq synced-quiz-id quiz-id))))
          (with-temp-org-buffer
           "* Quiz
:PROPERTIES:
:CANVAS_ID: 55
:END:
"
           (org-back-to-heading t)
           (org-canvas--new-quiz-sync-children
            '(:canvas-id "55" :rubric-id nil)
            '((other_field . "x")))
           (expect synced-quiz-id :to-equal "55")))))))

(describe "org-canvas--new-quiz-pull-items error handling"
  (it "warns (items omitted) when the item fetch fails"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (signal 'error '("HTTP 500")))))
        (org-canvas--new-quiz-pull-items "55")
        (let ((warned nil))
          (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
            (when (and (>= (length call) 2) (stringp (nth 1 call))
                       (string-match-p "items omitted from pull" (nth 1 call)))
              (setq warned t)))
          (expect warned :to-be t))))))

;;;; Items digest (hash-extra — issue #26 bug class)

(describe "org-canvas--new-quiz-items-digest"
  (it "changes when an item subtree changes"
    (let ((d1 (with-temp-org-buffer
               "* Quiz\n** Item 1\n:PROPERTIES:\n:TYPE: choice\n:END:\n- [X] A\n"
               (org-back-to-heading)
               (org-canvas--new-quiz-items-digest (list :pom (point)))))
          (d2 (with-temp-org-buffer
               "* Quiz\n** Item 1\n:PROPERTIES:\n:TYPE: choice\n:END:\n- [X] B\n"
               (org-back-to-heading)
               (org-canvas--new-quiz-items-digest (list :pom (point))))))
      (expect d1 :not :to-equal d2)))

  (it "changes when the rubric link changes"
    ;; Rubric association happens in finalize and is not part of the
    ;; quiz payload, so it must be folded into the digest.
    (with-temp-org-buffer
     "* Quiz\n** Item 1\n- [X] A\n"
     (org-back-to-heading)
     (let ((pom (point)))
       (expect (org-canvas--new-quiz-items-digest
                (list :pom pom :rubric-id "7"))
               :not :to-equal
               (org-canvas--new-quiz-items-digest
                (list :pom pom :rubric-id "8"))))))

  (it "does not change when an item gains a CANVAS_ITEM_ID"
    (with-temp-org-buffer
     "* Quiz\n** Item 1\n:PROPERTIES:\n:TYPE: choice\n:END:\n- [X] A\n"
     (org-back-to-heading)
     (let* ((data (list :pom (point)))
            (before (org-canvas--new-quiz-items-digest data)))
       (save-excursion
         (search-forward "** Item 1")
         (org-entry-put (point) "CANVAS_ITEM_ID" "300"))
       (expect (org-canvas--new-quiz-items-digest data)
               :to-equal before)))))

;;; org-canvas-new-quizzes-test.el ends here
