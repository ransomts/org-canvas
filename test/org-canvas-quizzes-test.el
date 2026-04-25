;;; org-canvas-quizzes-test.el --- Buttercup tests for quizzes  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-quizzes)

;;;; Helper Functions

(describe "org-canvas--quiz-parse-body-text"
  (it "extracts body text from heading"
    (with-temp-org-buffer
     "* Quiz Title
:PROPERTIES:
:END:

This is the quiz description.
"
     (org-back-to-heading)
     (expect (org-canvas--quiz-parse-body-text)
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
     (expect (org-canvas--quiz-parse-body-text)
             :to-equal "Quiz intro text."))))

(describe "org-canvas--quiz-parse-question-text"
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
     (expect (org-canvas--quiz-parse-question-text) :to-equal "")))

  (it "extracts multi-line question text"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

Consider the following scenario.
What would happen if X occurred?

- [X] Answer A
- [ ] Answer B
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((text (org-canvas--quiz-parse-question-text)))
       (expect text :to-match "Consider the following scenario")))))

(describe "org-canvas--quiz-parse-checkbox-list"
  (it "parses correct and incorrect answers"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

- [X] Correct answer
- [ ] Wrong answer 1
- [ ] Wrong answer 2
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((answers (org-canvas--quiz-parse-checkbox-list)))
       (expect (length answers) :to-equal 3)
       (expect (cdr (nth 0 answers)) :to-be t)  ; First is correct
       (expect (cdr (nth 1 answers)) :to-be nil)  ; Second is wrong
       (expect (car (nth 0 answers)) :to-equal "Correct answer"))))

  (it "handles multiple correct answers"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

- [X] Answer 1
- [X] Answer 2
- [ ] Answer 3
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((answers (org-canvas--quiz-parse-checkbox-list)))
       (expect (cdr (nth 0 answers)) :to-be t)
       (expect (cdr (nth 1 answers)) :to-be t)
       (expect (cdr (nth 2 answers)) :to-be nil)))))

;;;; Transform Props

(describe "org-canvas--quiz-transform-props"
  (it "strips statistics cookie from title"
    (let* ((props '(:title-raw "Midterm [2/5]"
                    :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :title) :to-equal "Midterm")))

  (it "interprets published default-true"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :published) :to-be t)))

  (it "interprets published false"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw "false"
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :published) :to-be nil)))

  (it "interprets shuffle_answers boolean"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw "true" :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :shuffle_answers) :to-be t)))

  (it "validates quiz_type with default"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :quiz_type) :to-equal "assignment")))

  (it "validates quiz_type with valid value"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil
                    :quiz-type-raw "practice_quiz"
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :quiz_type) :to-equal "practice_quiz")))

  (it "converts time_limit to number"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw "30" :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :time_limit) :to-equal 30)))

  (it "leaves time_limit nil when not set"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :time_limit) :to-be nil)))

  (it "converts allowed_attempts to number"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw "3"
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :allowed_attempts) :to-equal 3)))

  (it "parses due_at timestamp"
    (let* ((old-tz (getenv "TZ")))
      (unwind-protect
          (progn
            (set-time-zone-rule "UTC")
            (let* ((props '(:title-raw "Quiz" :canvas-id nil
                            :quiz-type-raw nil :time-limit-raw nil
                            :published-raw nil :shuffle-raw nil
                            :allowed-attempts-raw nil
                            :due-at-raw "<2026-06-15 Sun 23:59>"
                            :assignment-group-id-raw nil
                            :unlock-at-raw nil :lock-at-raw nil
                            :access_code nil :show-correct-raw nil
                            :show-correct-at-raw nil
                            :hide-correct-at-raw nil
                            :hide-results-raw nil
                            :scoring-policy-raw nil
                            :one-question-raw nil
                            :cant-go-back-raw nil :ip_filter nil
                            :show-correct-last-raw nil
                            :one-time-results-raw nil
                            :only-visible-raw nil))
                   (result (org-canvas--quiz-transform-props props)))
              (expect (plist-get result :due_at)
                      :to-equal "2026-06-15T23:59:00Z")))
        (set-time-zone-rule old-tz))))

  (it "validates hide_results property"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw "always"
                    :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :hide_results) :to-equal "always")))

  (it "validates scoring_policy property"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil
                    :scoring-policy-raw "keep_latest"
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :scoring_policy) :to-equal "keep_latest")))

  (it "converts assignment_group_id string to number"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw "42"
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :assignment_group_id) :to-equal 42)))

  (it "passes through string fields unchanged"
    (let* ((props '(:title-raw "Quiz" :canvas-id "999"
                    :quiz-type-raw nil :time-limit-raw nil
                    :published-raw nil :shuffle-raw nil
                    :allowed-attempts-raw nil :due-at-raw nil
                    :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code "secret123" :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw nil :cant-go-back-raw nil
                    :ip_filter "192.168.1.0/24"
                    :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :canvas-id) :to-equal "999")
      (expect (plist-get result :access_code) :to-equal "secret123")
      (expect (plist-get result :ip_filter) :to-equal "192.168.1.0/24")))

  (it "interprets one_question_at_a_time boolean"
    (let* ((props '(:title-raw "Quiz" :canvas-id nil :quiz-type-raw nil
                    :time-limit-raw nil :published-raw nil
                    :shuffle-raw nil :allowed-attempts-raw nil
                    :due-at-raw nil :assignment-group-id-raw nil
                    :unlock-at-raw nil :lock-at-raw nil
                    :access_code nil :show-correct-raw nil
                    :show-correct-at-raw nil :hide-correct-at-raw nil
                    :hide-results-raw nil :scoring-policy-raw nil
                    :one-question-raw "true" :cant-go-back-raw nil
                    :ip_filter nil :show-correct-last-raw nil
                    :one-time-results-raw nil :only-visible-raw nil))
           (result (org-canvas--quiz-transform-props props)))
      (expect (plist-get result :one_question_at_a_time) :to-be t))))

(describe "org-canvas--question-transform-props"
  (it "strips statistics cookie from title"
    (let* ((props '(:title-raw "Question [1/3]" :canvas-id nil
                    :type-raw nil :points-raw nil
                    :quiz-canvas-id "100" :text "prompt"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :name) :to-equal "Question")))

  (it "defaults question_type to multiple_choice_question"
    (let* ((props '(:title-raw "Q1" :canvas-id nil
                    :type-raw nil :points-raw nil
                    :quiz-canvas-id "100" :text "prompt"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :question_type)
              :to-equal "multiple_choice_question")))

  (it "validates custom question_type"
    (let* ((props '(:title-raw "Q1" :canvas-id nil
                    :type-raw "essay_question" :points-raw nil
                    :quiz-canvas-id "100" :text "prompt"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :question_type)
              :to-equal "essay_question")))

  (it "converts points to number with default 1"
    (let* ((props '(:title-raw "Q1" :canvas-id nil
                    :type-raw nil :points-raw nil
                    :quiz-canvas-id "100" :text "prompt"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :points_possible) :to-equal 1)))

  (it "converts points string to number"
    (let* ((props '(:title-raw "Q1" :canvas-id nil
                    :type-raw nil :points-raw "10"
                    :quiz-canvas-id "100" :text "prompt"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :points_possible) :to-equal 10)))

  (it "passes through quiz-canvas-id"
    (let* ((props '(:title-raw "Q1" :canvas-id nil
                    :type-raw nil :points-raw nil
                    :quiz-canvas-id "99999" :text "prompt"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :quiz-canvas-id) :to-equal "99999")))

  (it "passes through canvas-id"
    (let* ((props '(:title-raw "Q1" :canvas-id "555"
                    :type-raw nil :points-raw nil
                    :quiz-canvas-id "100" :text "prompt"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :canvas-id) :to-equal "555")))

  (it "passes through text"
    (let* ((props '(:title-raw "Q1" :canvas-id nil
                    :type-raw nil :points-raw nil
                    :quiz-canvas-id "100" :text "What is 2+2?"))
           (result (org-canvas--question-transform-props props)))
      (expect (plist-get result :text) :to-equal "What is 2+2?"))))

;;;; Stage 1: Parse Quiz Entry

(describe "org-canvas--quiz-parse-entry"
  (it "extracts quiz title from heading"
    (with-temp-org-buffer
     "* Midterm Quiz
:PROPERTIES:
:END:

Quiz description.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :title) :to-equal "Midterm Quiz"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:END:\n\nBody.\n")
     (org-back-to-heading)
     (expect (org-canvas--quiz-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ID: 44444
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "44444"))))

  (it "parses quiz_type (default assignment)"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :quiz_type) :to-equal "assignment"))))

  (it "parses custom quiz_type"
    (with-temp-org-buffer
     "* Practice Quiz
:PROPERTIES:
:QUIZ_TYPE: practice_quiz
:END:

Practice.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :quiz_type) :to-equal "practice_quiz"))))

  (it "parses time_limit"
    (with-temp-org-buffer
     "* Timed Quiz
:PROPERTIES:
:TIME_LIMIT: 30
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :time_limit) :to-equal 30))))

  (it "parses published (default true)"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :published) :to-be t))))

  (it "parses published false"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:PUBLISHED: false
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :published) :to-be nil))))

  (it "parses shuffle_answers"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SHUFFLE_ANSWERS: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :shuffle_answers) :to-be t))))

  (it "parses allowed_attempts"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:ALLOWED_ATTEMPTS: 3
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :allowed_attempts) :to-equal 3))))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:

Body.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy))))

  (it "parses GROUP link to assignment_group_id"
    (let* ((dir (make-temp-file "quiz-group-test-" t))
           (groups-file (expand-file-name "assignment-groups.org" dir))
           (quiz-file (expand-file-name "quizzes.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Exams\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (with-temp-file quiz-file
              (insert "* Midterm\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Exams][Exams]]\n:END:\n\nContent.\n"))
            (let ((org-canvas-quizzes-file quiz-file))
              (with-current-buffer (find-file-noselect quiz-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--quiz-parse-entry)))
                  (expect (plist-get data :assignment_group_id) :to-equal 42))
                (kill-buffer))))
        (delete-directory dir t))))

  (it "returns nil assignment_group_id when no GROUP property"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :assignment_group_id) :to-be nil)))))

;;;; Stage 1: Parse Question Entry

(describe "org-canvas--question-parse-entry"
  (it "extracts name from heading"
    (with-temp-org-buffer
     "* Quiz
** What is the capital of France?
:PROPERTIES:
:TYPE: multiple_choice_question
:POINTS: 5
:END:

- [X] Paris
- [ ] London
"
     (search-forward "What is the capital")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "12345")))
       (expect (plist-get data :name) :to-equal "What is the capital of France?"))))

  (it "parses question_type (default multiple_choice)"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

Content.
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "12345")))
       (expect (plist-get data :question_type) :to-equal "multiple_choice_question"))))

  (it "parses custom question_type"
    (with-temp-org-buffer
     "* Quiz
** True or False
:PROPERTIES:
:TYPE: true_false_question
:END:

- [X] True
- [ ] False
"
     (search-forward "True or False")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "12345")))
       (expect (plist-get data :question_type) :to-equal "true_false_question"))))

  (it "parses points_possible (default 1)"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

Content.
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "12345")))
       (expect (plist-get data :points_possible) :to-equal 1))))

  (it "parses custom points"
    (with-temp-org-buffer
     "* Quiz
** Hard Question
:PROPERTIES:
:POINTS: 10
:END:

Content.
"
     (search-forward "Hard Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "12345")))
       (expect (plist-get data :points_possible) :to-equal 10))))

  (it "includes quiz-canvas-id"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

Content.
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "99999")))
       (expect (plist-get data :quiz-canvas-id) :to-equal "99999")))))

;;;; Stage 2: Build Quiz Payload

(describe "org-canvas--quiz-build-payload"
  (it "wraps in quiz key"
    (let* ((data '(:title "Test Quiz" :quiz_type "assignment"))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'quiz payload) :to-be-truthy)))

  (it "includes title"
    (let* ((data '(:title "Final Exam" :quiz_type "assignment"))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'title (alist-get 'quiz payload)) :to-equal "Final Exam")))

  (it "includes quiz_type"
    (let* ((data '(:title "Test" :quiz_type "practice_quiz"))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'quiz_type (alist-get 'quiz payload)) :to-equal "practice_quiz")))

  (it "includes time_limit when specified"
    (let* ((data '(:title "Test" :quiz_type "assignment" :time_limit 60))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'time_limit (alist-get 'quiz payload)) :to-equal 60)))

  (it "includes published true"
    (let* ((data '(:title "Test" :quiz_type "assignment" :published t))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'published (alist-get 'quiz payload)) :to-be t)))

  (it "includes published json-false when nil"
    (let* ((data '(:title "Test" :quiz_type "assignment" :published nil))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'published (alist-get 'quiz payload)) :to-equal :json-false)))

  (it "includes shuffle_answers"
    (let* ((data '(:title "Test" :quiz_type "assignment" :shuffle_answers t))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'shuffle_answers (alist-get 'quiz payload)) :to-be t)))

  (it "includes allowed_attempts"
    (let* ((data '(:title "Test" :quiz_type "assignment" :allowed_attempts 2))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'allowed_attempts (alist-get 'quiz payload)) :to-equal 2)))

  (it "includes assignment_group_id when present"
    (let* ((data '(:title "Test" :quiz_type "assignment" :assignment_group_id 42))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'assignment_group_id (alist-get 'quiz payload)) :to-equal 42)))

  (it "excludes assignment_group_id when nil"
    (let* ((data '(:title "Test" :quiz_type "assignment" :assignment_group_id nil))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (assq 'assignment_group_id (alist-get 'quiz payload)) :to-be nil))))

;;;; Stage 2: Build Question Answers

(describe "org-canvas--question-build-answers"
  (it "builds multiple choice answers"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

- [X] Correct
- [ ] Wrong
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "multiple_choice_question")))
       (expect (length answers) :to-equal 2)
       ;; First answer should have weight 100 (correct)
       (expect (alist-get 'answer_weight (nth 0 answers)) :to-equal 100)
       ;; Second should have weight 0 (wrong)
       (expect (alist-get 'answer_weight (nth 1 answers)) :to-equal 0))))

  (it "returns nil for essay questions"
    (with-temp-org-buffer
     "* Quiz
** Essay Question
:PROPERTIES:
:TYPE: essay_question
:END:

Write about...
"
     (search-forward "Essay Question")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "essay_question")))
       (expect answers :to-be nil)))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--quiz-push-to-api (mocked)"
  (it "uses POST for new quizzes"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-id nil))
              (payload '((quiz . ((title . "New"))))))
          (org-canvas--quiz-push-to-api data payload)
          (expect-api-called 'POST "quizzes$")))))

  (it "uses PUT for existing quizzes"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "777"))
              (payload '((quiz . ((title . "Existing"))))))
          (org-canvas--quiz-push-to-api data payload)
          (expect-api-called 'PUT "quizzes/777"))))))

(describe "org-canvas--question-push-to-api (mocked)"
  (it "uses POST for new questions"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "Q1" :canvas-id nil :quiz-canvas-id "100"))
              (payload '((question . ((question_name . "Q1"))))))
          (org-canvas--question-push-to-api data payload)
          (expect-api-called 'POST "quizzes/100/questions$")))))

  (it "uses PUT for existing questions"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "Q1" :canvas-id "999" :quiz-canvas-id "100"))
              (payload '((question . ((question_name . "Q1"))))))
          (org-canvas--question-push-to-api data payload)
          (expect-api-called 'PUT "quizzes/100/questions/999"))))))

;;;; Stage 4: Finalize

(describe "org-canvas--quiz-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:END:

Description.
"
     (org-back-to-heading)
     (let ((data (list :title "Test Quiz" :pom (point-marker)))
           (response '((id . 55555) (title . "Test Quiz"))))
       (org-canvas--quiz-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "55555")))))

(describe "org-canvas--question-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Quiz
** Question 1
:PROPERTIES:
:END:

- [X] Answer
"
     (search-forward "Question 1")
     (org-back-to-heading)
     (let ((data (list :name "Question 1" :pom (point-marker)))
           (response '((id . 66666))))
       (org-canvas--question-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "66666")))))

;;;; Special Quiz Parsing Functions

(describe "org-canvas--quiz-parse-matching-list"
  (it "parses matching question pairs"
    (with-temp-org-buffer
     "* Quiz
** Matching Question
- Left 1 = Right 1
- Left 2 = Right 2
- Distractor
"
     (search-forward "Matching Question")
     (org-back-to-heading)
     (let ((pairs (org-canvas--quiz-parse-matching-list)))
       (expect (length pairs) :to-be-truthy))))

  (it "returns left-right pairs"
    (with-temp-org-buffer
     "* Quiz
** Match
- Cat = Feline
- Dog = Canine
"
     (search-forward "Match")
     (org-back-to-heading)
     (let ((pairs (org-canvas--quiz-parse-matching-list)))
       (expect (car (nth 0 pairs)) :to-equal "Cat")
       (expect (cdr (nth 0 pairs)) :to-equal "Feline"))))

  (it "handles distractors with nil right side"
    (with-temp-org-buffer
     "* Quiz
** Match
- Cat = Feline
- Elephant
"
     (search-forward "Match")
     (org-back-to-heading)
     (let ((pairs (org-canvas--quiz-parse-matching-list)))
       ;; Find the distractor (nil right side)
       (expect (cl-find-if (lambda (p) (null (cdr p))) pairs) :to-be-truthy)))))

(describe "org-canvas--quiz-parse-nested-blanks"
  (it "parses blank IDs with nested answers"
    (with-temp-org-buffer
     "* Quiz
** Fill in the blanks
- color
  - [X] blue
  - [ ] red
- size
  - [X] large
  - [ ] small
"
     (search-forward "Fill in the blanks")
     (org-back-to-heading)
     (let ((blanks (org-canvas--quiz-parse-nested-blanks)))
       (expect (length blanks) :to-equal 2)
       (expect (car (nth 0 blanks)) :to-equal "color")
       (expect (car (nth 1 blanks)) :to-equal "size"))))

  (it "extracts correct and incorrect answers"
    (with-temp-org-buffer
     "* Quiz
** Blanks
- word1
  - [X] correct
  - [ ] wrong
"
     (search-forward "Blanks")
     (org-back-to-heading)
     (let* ((blanks (org-canvas--quiz-parse-nested-blanks))
            (answers (cdr (nth 0 blanks))))
       (expect (cdr (nth 0 answers)) :to-be t)  ; correct is t
       (expect (cdr (nth 1 answers)) :to-be nil)))))  ; wrong is nil

(describe "org-canvas--quiz-parse-numerical-answer"
  ;; These tests require Emacs 30+ due to org-mode heading recognition differences
  (it "parses exact numerical value"
    (unless test-org-canvas-emacs-30-p
      (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
    (with-temp-org-buffer
     "* Quiz
** What is 2+2?
- [X] 4
"
     (search-forward "What is 2+2")
     (org-back-to-heading)
     (let ((num (org-canvas--quiz-parse-numerical-answer)))
       (expect (plist-get num :type) :to-equal 'exact)
       (expect (plist-get num :value) :to-equal 4))))

  (it "parses range answer"
    (unless test-org-canvas-emacs-30-p
      (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
    (with-temp-org-buffer
     "* Quiz
** Estimate pi
- [X] [3.1, 3.2]
"
     (search-forward "Estimate pi")
     (org-back-to-heading)
     (let ((num (org-canvas--quiz-parse-numerical-answer)))
       (expect (plist-get num :type) :to-equal 'range)
       (expect (plist-get num :start) :to-equal 3.1)
       (expect (plist-get num :end) :to-equal 3.2))))

  (it "returns nil when no checked answer"
    (with-temp-org-buffer
     "* Quiz
** No answer
- [ ] 5
"
     (search-forward "No answer")
     (org-back-to-heading)
     (expect (org-canvas--quiz-parse-numerical-answer) :to-be nil))))

;;;; Additional Quiz Parse Tests

(describe "org-canvas--quiz-parse-entry"
  :var ((original-export nil))

  (before-each
    (setq original-export (symbol-function 'org-export-string-as))
    (fset 'org-export-string-as (lambda (s _type _body-only) (format "<p>%s</p>" s))))

  (after-each
    (fset 'org-export-string-as original-export))

  (it "returns nil canvas-id for new quizzes"
    (with-temp-org-buffer
     "* New Quiz
:PROPERTIES:
:END:

Description.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :canvas-id) :to-be nil))))

  (it "extracts description from body text"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

This is the quiz description.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :description) :to-match "This is the quiz description"))))

  (it "parses due_at timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:DUE_AT: <2025-06-15 Sun 23:59>
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :due_at) :to-be-truthy))))

  (it "defaults shuffle_answers to nil"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :shuffle_answers) :to-be nil)))))

;;;; Additional Question Parse Tests

(describe "org-canvas--question-parse-entry"
  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:CANVAS_ID: 12345
:END:

- [X] Answer
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "999")))
       (expect (plist-get data :canvas-id) :to-equal "12345"))))

  (it "returns nil canvas-id for new questions"
    (with-temp-org-buffer
     "* Quiz
** New Question
:PROPERTIES:
:END:

- [X] Answer
"
     (search-forward "New Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "999")))
       (expect (plist-get data :canvas-id) :to-be nil))))

  (it "extracts text from body"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

Consider this scenario carefully.

- [X] Answer
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "999")))
       (expect (plist-get data :text) :to-match "Consider this scenario"))))

  (it "includes pom marker"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

- [X] Answer
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (org-canvas--question-parse-entry "999")))
       (expect (plist-get data :pom) :to-be-truthy)))))

;;;; Additional Quiz Build Payload Tests

(describe "org-canvas--quiz-build-payload"
  (it "includes description when present"
    (let* ((data '(:title "Test" :quiz_type "assignment" :description "<p>Desc</p>"))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'description (alist-get 'quiz payload)) :to-equal "<p>Desc</p>")))

  (it "excludes description when nil"
    (let* ((data '(:title "Test" :quiz_type "assignment" :description nil))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (assq 'description (alist-get 'quiz payload)) :to-be nil)))

  (it "includes due_at when present"
    (let* ((data '(:title "Test" :quiz_type "assignment" :due_at "2025-06-15T23:59:00Z"))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'due_at (alist-get 'quiz payload)) :to-equal "2025-06-15T23:59:00Z")))

  (it "does not include shuffle_answers when nil"
    (let* ((data '(:title "Test" :quiz_type "assignment" :shuffle_answers nil))
           (payload (org-canvas--quiz-build-payload data)))
      ;; shuffle_answers should not be present in payload when nil
      (expect (assq 'shuffle_answers (alist-get 'quiz payload)) :to-be nil))))

;;;; Additional Question Build Answers Tests

(describe "org-canvas--question-build-answers"
  (it "builds short_answer answers (only correct)"
    (with-temp-org-buffer
     "* Quiz
** Short Answer
:PROPERTIES:
:TYPE: short_answer_question
:END:

- [X] answer1
- [X] answer2
- [ ] wrong
"
     (search-forward "Short Answer")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "short_answer_question")))
       ;; Only checked answers are included
       (expect (length answers) :to-equal 2)
       (expect (alist-get 'answer_weight (nth 0 answers)) :to-equal 100))))

  (it "builds multiple_answers answers"
    (with-temp-org-buffer
     "* Quiz
** Multiple Answers
:PROPERTIES:
:END:

- [X] Correct 1
- [X] Correct 2
- [ ] Wrong
"
     (search-forward "Multiple Answers")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "multiple_answers_question")))
       (expect (length answers) :to-equal 3)
       (expect (alist-get 'answer_weight (nth 0 answers)) :to-equal 100)
       (expect (alist-get 'answer_weight (nth 1 answers)) :to-equal 100)
       (expect (alist-get 'answer_weight (nth 2 answers)) :to-equal 0))))

  (it "builds fill_in_multiple_blanks answers"
    (with-temp-org-buffer
     "* Quiz
** Fill In
:PROPERTIES:
:END:

- blank1
  - [X] correct
  - [ ] wrong
"
     (search-forward "Fill In")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "fill_in_multiple_blanks_question")))
       ;; Only correct answers for fill in blanks
       (expect (length answers) :to-equal 1)
       (expect (alist-get 'blank_id (nth 0 answers)) :to-equal "blank1")
       (expect (alist-get 'answer_weight (nth 0 answers)) :to-equal 100))))

  (it "builds multiple_dropdowns answers"
    (with-temp-org-buffer
     "* Quiz
** Dropdown
:PROPERTIES:
:END:

- dd1
  - [X] correct
  - [ ] wrong
"
     (search-forward "Dropdown")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "multiple_dropdowns_question")))
       ;; All answers included for dropdowns
       (expect (length answers) :to-equal 2)
       (expect (alist-get 'blank_id (nth 0 answers)) :to-equal "dd1"))))

  (it "builds matching answers"
    (with-temp-org-buffer
     "* Quiz
** Match
:PROPERTIES:
:END:

- Cat = Feline
- Dog = Canine
"
     (search-forward "Match")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "matching_question")))
       (expect (length answers) :to-equal 2)
       (expect (alist-get 'answer_match_left (nth 0 answers)) :to-equal "Cat")
       (expect (alist-get 'answer_match_right (nth 0 answers)) :to-equal "Feline"))))

  (it "builds numerical exact answer"
    (with-temp-org-buffer
     "* Quiz
** Numerical
:PROPERTIES:
:END:

- [X] 42
"
     (search-forward "Numerical")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "numerical_question")))
       (expect (length answers) :to-equal 1)
       (expect (alist-get 'numerical_answer_type (nth 0 answers)) :to-equal "exact_answer")
       (expect (alist-get 'answer_exact (nth 0 answers)) :to-equal 42))))

  (it "builds numerical range answer"
    (with-temp-org-buffer
     "* Quiz
** Range
:PROPERTIES:
:END:

- [X] [10, 20]
"
     (search-forward "Range")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "numerical_question")))
       (expect (length answers) :to-equal 1)
       (expect (alist-get 'numerical_answer_type (nth 0 answers)) :to-equal "range_answer")
       (expect (alist-get 'answer_range_start (nth 0 answers)) :to-equal 10)
       (expect (alist-get 'answer_range_end (nth 0 answers)) :to-equal 20))))

  (it "returns nil for file_upload questions"
    (with-temp-org-buffer
     "* Quiz
** Upload
:PROPERTIES:
:TYPE: file_upload_question
:END:

Upload your file.
"
     (search-forward "Upload")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "file_upload_question")))
       (expect answers :to-be nil))))

  (it "returns nil for text_only questions"
    (with-temp-org-buffer
     "* Quiz
** Text Only
:PROPERTIES:
:TYPE: text_only_question
:END:

Read this carefully.
"
     (search-forward "Text Only")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "text_only_question")))
       (expect answers :to-be nil)))))

;;;; Question Build Payload Tests

(describe "org-canvas--question-build-payload"
  (it "includes answers array when present"
    ;; build-payload is now pure — supply pre-computed data
    (let* ((data (list :name "Question"
                       :text-html "<p>Some text</p>"
                       :question_type "multiple_choice_question"
                       :points_possible 1
                       :answers '(((answer_text . "Correct")
                                   (answer_weight . 100))
                                  ((answer_text . "Wrong")
                                   (answer_weight . 0)))))
           (payload (org-canvas--question-build-payload data))
           (question (alist-get 'question payload)))
      (expect (alist-get 'answers question) :to-be-truthy)))

  (it "uses pre-computed HTML for question_text"
    (let* ((data (list :name "Question"
                       :text-html "<p>Some question text.</p>"
                       :question_type "multiple_choice_question"
                       :points_possible 1
                       :answers '(((answer_text . "Answer")
                                   (answer_weight . 100)))))
           (payload (org-canvas--question-build-payload data))
           (question (alist-get 'question payload)))
      (expect (alist-get 'question_text question) :to-match "<p>")))

  (it "omits answers for essay questions"
    (let* ((data (list :name "Essay"
                       :text-html "<p>Write about...</p>"
                       :question_type "essay_question"
                       :points_possible 1
                       :answers nil))
           (payload (org-canvas--question-build-payload data))
           (question (alist-get 'question payload)))
      (expect (assq 'answers question) :to-be nil))))

;;;; Additional Push to API Tests

(describe "org-canvas--quiz-push-to-api (mocked)"
  (it "re-signals errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API error")))))
        (let ((data '(:title "Test" :canvas-id nil))
              (payload '((quiz . ((title . "Test"))))))
          (expect (org-canvas--quiz-push-to-api data payload) :to-throw 'error))))))

(describe "org-canvas--question-push-to-api (mocked)"
  (it "re-signals errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API error")))))
        (let ((data '(:name "Q1" :canvas-id nil :quiz-canvas-id "100"))
              (payload '((question . ((question_name . "Q1"))))))
          (expect (org-canvas--question-push-to-api data payload) :to-throw 'error))))))

;;;; Additional Finalize Tests

(describe "org-canvas--quiz-finalize"
  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Description.
"
     (org-back-to-heading)
     (let ((data (list :title "Quiz" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (expect (org-canvas--quiz-finalize data response) :to-throw 'error))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Description.
"
     (org-back-to-heading)
     (let ((data (list :title "Quiz" :pom (point-marker)))
           (response '((id . 99999))))
       (org-canvas--quiz-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-match "^\\[20[0-9][0-9]-")))))

(describe "org-canvas--question-finalize"
  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

- [X] Answer
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (list :name "Question" :pom (point-marker)))
           (response '((error . "failed"))))
       (expect (org-canvas--question-finalize data response) :to-throw 'error))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:END:

- [X] Answer
"
     (search-forward "Question")
     (org-back-to-heading)
     (let ((data (list :name "Question" :pom (point-marker)))
           (response '((id . 88888))))
       (org-canvas--question-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-match "^\\[20[0-9][0-9]-")))))

;;;; Sync Quiz Questions Tests

(describe "org-canvas--sync-quiz-questions (mocked)"
  (it "collects and syncs level-2 questions"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:

Quiz description.

** Question 1
:PROPERTIES:
:END:

- [X] Answer 1

** Question 2
:PROPERTIES:
:END:

- [X] Answer 2
"
         (org-back-to-heading)
         (let ((marker (point-marker)))
           (org-canvas--sync-quiz-questions marker "100")
           ;; Both questions should have been POSTed
           (expect-api-called 'POST "quizzes/100/questions"))))))

  (it "continues after question failure"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         (signal 'error '("First question failed"))
                       '((id . 222))))))
          (with-temp-org-buffer
           "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:

** Question 1
:PROPERTIES:
:END:

- [X] A

** Question 2
:PROPERTIES:
:END:

- [X] B
"
           (org-back-to-heading)
           (let ((marker (point-marker)))
             (let ((results (org-canvas--sync-quiz-questions marker "100")))
               ;; Should have 1 success, 1 failure
               (expect (car results) :to-equal 1)
               (expect (cdr results) :to-equal 1))))))))

  (it "returns correct counts"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:

** Q1
- [X] A

** Q2
- [X] B

** Q3
- [X] C
"
         (org-back-to-heading)
         (let* ((marker (point-marker))
                (results (org-canvas--sync-quiz-questions marker "100")))
           (expect (car results) :to-equal 3)
           (expect (cdr results) :to-equal 0)))))))

;;;; Sync Quizzes Pipeline

(describe "org-canvas-sync-quizzes (mocked)"
  (it "errors when quizzes file not found"
    (let ((org-canvas-quizzes-file "/nonexistent/quizzes.org"))
      (expect (org-canvas-sync-quizzes) :to-throw 'error)))

  (it "syncs quizzes and their questions"
    (let ((temp-dir (make-temp-file "quiz-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (quiz-synced nil)
                 (questions-synced 0))
            (with-temp-file org-file
              (insert "* Test Quiz
:PROPERTIES:
:END:

Quiz description.

** Question 1
:PROPERTIES:
:END:

- [X] Answer A
- [ ] Answer B
"))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ;; Quiz POST
                              ((and (eq method 'POST) (string-match "quizzes$" url))
                               (setq quiz-synced t)
                               '((id . 100) (title . "Test Quiz")))
                              ;; Question POST
                              ((and (eq method 'POST) (string-match "questions$" url))
                               (setq questions-synced (1+ questions-synced))
                               '((id . 200)))
                              (t nil)))))
                  (org-canvas-sync-quizzes)
                  (expect quiz-synced :to-be t)
                  (expect questions-synced :to-equal 1)
                  ;; Check CANVAS_ID was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100"))))))
        (delete-directory temp-dir t))))

  (it "continues after quiz failure"
    (let ((temp-dir (make-temp-file "quiz-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (quiz-attempts 0))
            (with-temp-file org-file
              (insert "* Quiz 1
:PROPERTIES:
:END:

** Q1
- [X] A

* Quiz 2
:PROPERTIES:
:END:

** Q2
- [X] B
"))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((and (eq method 'POST) (string-match "quizzes$" url))
                               (setq quiz-attempts (1+ quiz-attempts))
                               (if (= quiz-attempts 1)
                                   (signal 'error '("First quiz failed"))
                                 '((id . 200))))
                              ((and (eq method 'POST) (string-match "questions$" url))
                               '((id . 300)))
                              (t nil)))))
                  (org-canvas-sync-quizzes)
                  ;; Both quizzes should be attempted
                  (expect quiz-attempts :to-equal 2)))))
        (delete-directory temp-dir t))))

  (it "uses existing canvas-id for question sync"
    (let ((temp-dir (make-temp-file "quiz-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (question-quiz-id nil))
            (with-temp-file org-file
              (insert "* Quiz
:PROPERTIES:
:CANVAS_ID: 555
:END:

** Q1
- [X] A
"))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((and (eq method 'PUT) (string-match "quizzes/555$" url))
                               '((id . 555)))
                              ((and (eq method 'POST) (string-match "quizzes/\\([0-9]+\\)/questions" url))
                               (setq question-quiz-id (match-string 1 url))
                               '((id . 700)))
                              (t nil)))))
                  (org-canvas-sync-quizzes)
                  ;; Question should use the existing quiz ID
                  (expect question-quiz-id :to-equal "555")))))
        (delete-directory temp-dir t)))))

;;;; Delete All Quizzes

(describe "org-canvas-delete-all-quizzes (mocked)"
  (it "aborts when user declines"
    (let ((delete-called nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
        (expect (org-canvas-delete-all-quizzes) :to-throw 'user-error))))

  (it "deletes all quizzes when confirmed"
    (let ((temp-dir (make-temp-file "quiz-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (deleted-quizzes nil))
            (with-temp-file org-file
              (insert "* Quiz
:PROPERTIES:
:CANVAS_ID: 111
:END:
"))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             [((id . 111) (title . "Quiz 1"))
                              ((id . 222) (title . "Quiz 2"))]))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                             (dolist (item items)
                               (push (number-to-string (alist-get 'id item)) deleted-quizzes))
                             (cons (length items) deleted-quizzes))))
                  (org-canvas-delete-all-quizzes)
                  (expect (length deleted-quizzes) :to-equal 2)))))
        (delete-directory temp-dir t))))

  (it "clears local properties for deleted quizzes"
    (let ((temp-dir (make-temp-file "quiz-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (cleared-count 0))
            (with-temp-file org-file
              (insert "* Quiz 1
:PROPERTIES:
:CANVAS_ID: 111
:LAST_SYNCED: [2024-01-01]
:END:

** Q1
:PROPERTIES:
:CANVAS_ID: 999
:END:

- [X] A
"))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((and (eq method 'GET) (string-match "/quizzes$" url))
                               [((id . 111) (title . "Quiz 1"))])
                              ((eq method 'DELETE) nil)
                              (t nil))))
                          ((symbol-function 'org-canvas-clear-sync-properties)
                           (lambda (_pom)
                             (setq cleared-count (1+ cleared-count)))))
                  (org-canvas-delete-all-quizzes)
                  ;; Both quiz and question should have properties cleared
                  (expect cleared-count :to-equal 2)))))
        (delete-directory temp-dir t))))

  (it "continues on individual delete errors"
    (let ((temp-dir (make-temp-file "quiz-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (delete-attempts 0))
            (with-temp-file org-file (insert ""))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             [((id . 1) (title . "Q1"))
                              ((id . 2) (title . "Q2"))]))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                             (setq delete-attempts (length items))
                             (cons (length items) nil))))
                  (org-canvas-delete-all-quizzes)
                  ;; Both should be attempted
                  (expect delete-attempts :to-equal 2)))))
        (delete-directory temp-dir t)))))

;;;; True/False Question Type

(describe "org-canvas--question-build-answers for true_false"
  (it "builds true/false answers correctly"
    (with-temp-org-buffer
     "* Quiz
** True or False
:PROPERTIES:
:END:

- [X] True
- [ ] False
"
     (search-forward "True or False")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "true_false_question")))
       (expect (length answers) :to-equal 2)
       (expect (alist-get 'answer_weight (nth 0 answers)) :to-equal 100)
       (expect (alist-get 'answer_weight (nth 1 answers)) :to-equal 0)))))

;;;; Matching Question Distractors

(describe "org-canvas--question-build-answers for matching with distractors"
  (it "handles distractor items without right side"
    (with-temp-org-buffer
     "* Quiz
** Match
:PROPERTIES:
:END:

- Cat = Feline
- Dog = Canine
- Elephant
"
     (search-forward "Match")
     (org-back-to-heading)
     (let ((answers (org-canvas--question-build-answers "matching_question")))
       (expect (length answers) :to-equal 3)
       ;; Find the distractor (should only have left side)
       (let ((distractor (cl-find-if (lambda (a)
                                       (and (alist-get 'answer_match_left a)
                                            (not (assq 'answer_match_right a))))
                                     answers)))
         (expect distractor :to-be-truthy)
         (expect (alist-get 'answer_match_left distractor) :to-equal "Elephant"))))))

;;;; Empty Quiz Body

(describe "org-canvas--quiz-parse-body-text edge cases"
  (it "returns empty string for quiz with no body"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
** Question
- [X] A
"
     (org-back-to-heading)
     (expect (org-canvas--quiz-parse-body-text) :to-equal "")))

  (it "handles quiz with only whitespace body"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:



** Question
- [X] A
"
     (org-back-to-heading)
     (expect (org-canvas--quiz-parse-body-text) :to-equal ""))))

;;;; Quiz Parse Entry Edge Cases

(describe "org-canvas--quiz-parse-entry edge cases"
  :var ((original-export nil))

  (before-each
    (setq original-export (symbol-function 'org-export-string-as))
    (fset 'org-export-string-as (lambda (s _type _body-only) (format "<p>%s</p>" s))))

  (after-each
    (fset 'org-export-string-as original-export))

  (it "returns nil description when body is empty"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
** Q
- [X] A
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :description) :to-be nil))))

  (it "parses due_at timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:DUE_AT: <2025-12-01 Mon 23:59>
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       ;; Just verify due_at is populated (timestamp parsing depends on org-mode version)
       (expect (plist-get data :due_at) :to-be-truthy)))))

;;;; Coverage Gap Tests

(describe "org-canvas--quiz-sync-children quiz-id fallback"
  (it "uses canvas-id from data when response has no id"
    (let ((sync-questions-quiz-id nil))
      (cl-letf (((symbol-function 'org-canvas--sync-quiz-groups)
                 (lambda (_marker _quiz-id) (cons 0 0)))
                ((symbol-function 'org-canvas--sync-quiz-questions)
                 (lambda (_marker quiz-id)
                   (setq sync-questions-quiz-id quiz-id)
                   (cons 0 0))))
        (with-temp-org-buffer
         "* My Quiz
:PROPERTIES:
:CANVAS_ID: 888
:END:

Quiz description.
"
         (org-back-to-heading)
         (let ((data '(:title "My Quiz" :canvas-id "888" :pom nil))
               (response '((title . "My Quiz"))))
           (org-canvas--quiz-sync-children data response)
           ;; Should use the canvas-id "888" from data since response has no id
           (expect sync-questions-quiz-id :to-equal "888"))))))

  (it "prefers response id over data canvas-id"
    (let ((sync-questions-quiz-id nil))
      (cl-letf (((symbol-function 'org-canvas--sync-quiz-groups)
                 (lambda (_marker _quiz-id) (cons 0 0)))
                ((symbol-function 'org-canvas--sync-quiz-questions)
                 (lambda (_marker quiz-id)
                   (setq sync-questions-quiz-id quiz-id)
                   (cons 0 0))))
        (with-temp-org-buffer
         "* My Quiz
:PROPERTIES:
:CANVAS_ID: 888
:END:
"
         (org-back-to-heading)
         (let ((data '(:title "My Quiz" :canvas-id "888" :pom nil))
               (response '((id . 999) (title . "My Quiz"))))
           (org-canvas--quiz-sync-children data response)
           (expect sync-questions-quiz-id :to-equal 999)))))))

;;;; Validation Tests

(describe "QUIZ_TYPE validation"
  (it "falls back to default for invalid quiz type"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:QUIZ_TYPE: invalid_type
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :quiz_type) :to-equal "assignment"))))

  (it "accepts valid quiz types"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:QUIZ_TYPE: graded_survey
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :quiz_type) :to-equal "graded_survey")))))

;;; Question Finalize

(describe "org-canvas--question-finalize"
  (it "logs the actual question name, not nil"
    (with-temp-org-buffer
     "* Question 1
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :name "What is 2+2?" :canvas-id nil :pom pom))
            (response '((id . 42)))
            (logged-messages nil))
       (cl-letf (((symbol-function 'org-canvas-org-save-sync-state)
                  (lambda (&rest _) nil))
                 ((symbol-function 'org-canvas--log-info)
                  (lambda (_logger fmt &rest args)
                    (push (apply #'format fmt args) logged-messages)))
                 ((symbol-function 'org-canvas--log-debug)
                  (lambda (_logger fmt &rest args)
                    (push (apply #'format fmt args) logged-messages))))
         (org-canvas--question-finalize data response)
         (let ((finalize-msg (cl-find-if
                              (lambda (m) (string-match-p "Saving.*CANVAS_ID" m))
                              logged-messages)))
           (expect finalize-msg :to-match "What is 2+2?")))))))

;;;; Quiz Verification

(describe "org-canvas--quiz-verify-response"
  (it "warns when assignment_group_id mismatches"
    (spy-on 'org-canvas--log-warning)
    (let ((data (list :title "Midterm" :assignment_group_id 200))
          (response '((id . 555) (assignment_group_id . 999))))
      (org-canvas--quiz-verify-response data response)
      (expect 'org-canvas--log-warning :to-have-been-called)))

  (it "does not warn when assignment_group_id matches"
    (spy-on 'org-canvas--log-warning)
    (let ((data (list :title "Midterm" :assignment_group_id 200))
          (response '((id . 555) (assignment_group_id . 200))))
      (org-canvas--quiz-verify-response data response)
      (expect 'org-canvas--log-warning :not :to-have-been-called)))

  (it "does not warn when no expected group"
    (spy-on 'org-canvas--log-warning)
    (let ((data (list :title "Midterm" :assignment_group_id nil))
          (response '((id . 555) (assignment_group_id . 200))))
      (org-canvas--quiz-verify-response data response)
      (expect 'org-canvas--log-warning :not :to-have-been-called))))

;;;; Pull Function Tests

(describe "org-canvas--quiz-pull-set-properties"
  (it "sets QUIZ_TYPE property"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (quiz_type . "graded_survey"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "QUIZ_TYPE") :to-equal "graded_survey"))))

  (it "sets TIME_LIMIT property"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (time_limit . 60))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "TIME_LIMIT") :to-equal "60"))))

  (it "sets SHUFFLE_ANSWERS boolean"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (shuffle_answers . t))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "SHUFFLE_ANSWERS") :to-equal "true"))))

  (it "sets timestamp properties"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (due_at . "2026-06-15T23:59:00Z")
                   (unlock_at . "2026-06-01T00:00:00Z")
                   (lock_at . "2026-06-30T23:59:00Z"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "DUE_AT") :to-match "<2026-06-15")
       (expect (org-entry-get (point) "UNLOCK_AT") :to-match "<2026-06-01")
       (expect (org-entry-get (point) "LOCK_AT") :to-match "<2026-06-30"))))

  (it "saves CANVAS_ID via sync state"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))

  (it "sets GROUP link when group-id resolves"
    (let ((groups-file (make-temp-file "test-groups" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Homework
:PROPERTIES:
:CANVAS_ID: 777
:END:
"))
            (let ((org-canvas-assignment-groups-file groups-file))
              (with-temp-org-buffer
               "* Test Quiz
:PROPERTIES:
:END:
"
               (org-back-to-heading)
               (let ((quiz '((id . 42) (assignment_group_id . 777))))
                 (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
                 (expect (org-entry-get (point) "GROUP") :to-match "Homework")))))
        (delete-file groups-file)))))

(describe "org-canvas--quiz-insert-question-body"
  (it "inserts converted question text"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (goto-char (point-max))
     (with-html-to-org-identity
       (org-canvas--quiz-insert-question-body "What is 2+2?")
       (expect (buffer-string) :to-match "What is 2+2?"))))

  (it "does nothing for nil text"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (let ((before (buffer-string)))
       (goto-char (point-max))
       (org-canvas--quiz-insert-question-body nil)
       (expect (buffer-string) :to-equal before))))

  (it "does nothing for empty text"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (let ((before (buffer-string)))
       (goto-char (point-max))
       (org-canvas--quiz-insert-question-body "")
       (expect (buffer-string) :to-equal before)))))

(describe "org-canvas--quiz-insert-answers"
  (it "inserts checked and unchecked answers"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (goto-char (point-max))
     (org-canvas--quiz-insert-answers
      [((text . "Correct") (weight . 100))
       ((text . "Wrong") (weight . 0))])
     (expect (buffer-string) :to-match "\\- \\[X\\] Correct")
     (expect (buffer-string) :to-match "\\- \\[ \\] Wrong")))

  (it "does nothing for nil answers"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (let ((before (buffer-string)))
       (goto-char (point-max))
       (org-canvas--quiz-insert-answers nil)
       (expect (buffer-string) :to-equal before))))

  (it "uses html field when text is absent"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (goto-char (point-max))
     (org-canvas--quiz-insert-answers
      [((html . "HTML Answer") (weight . 100))])
     (expect (buffer-string) :to-match "HTML Answer")))

  (it "converts HTML answer text via html-to-org-inline"
    (cl-letf (((symbol-function 'org-canvas--html-to-org-inline)
               (lambda (html) (concat "CONV:" html))))
      (with-temp-org-buffer
       "* Quiz
:PROPERTIES:
:END:
"
       (goto-char (point-max))
       (org-canvas--quiz-insert-answers
        [((text . "<em>Italic</em>") (weight . 100))])
       (expect (buffer-string) :to-match "CONV:<em>Italic</em>")))))

(describe "org-canvas--quiz-pull-insert-question"
  (it "creates L2 heading with properties"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (with-html-to-org-identity
       (org-canvas--quiz-pull-insert-question
        '((question_name . "Q1")
          (question_text . "What is 2+2?")
          (question_type . "multiple_choice_question")
          (points_possible . 5)
          (answers . [((text . "4") (weight . 100))
                      ((text . "5") (weight . 0))])))
       (expect (buffer-string) :to-match "\\*\\* Q1")
       (expect (buffer-string) :to-match "What is 2+2?")
       (expect (buffer-string) :to-match "\\[X\\] 4")
       (expect (buffer-string) :to-match "\\[ \\] 5")))))

(describe "org-canvas--quiz-pull-insert-questions"
  (it "fetches and inserts questions"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                (lambda (_method _url &optional _params)
                  '(((question_name . "Q1") (question_text . "Body")
                     (question_type . "short_answer_question")
                     (points_possible . 10) (answers . [])))))
               ((symbol-function 'org-canvas--html-to-org)
                (lambda (html) html)))
       (org-canvas--quiz-pull-insert-questions 100)
       (expect (buffer-string) :to-match "\\*\\* Q1"))))

  (it "handles API error gracefully"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                (lambda (_method _url &optional _params)
                  (signal 'error '("API error")))))
       ;; Should not throw
       (org-canvas--quiz-pull-insert-questions 100)
       (expect (buffer-string) :not :to-match "\\*\\*")))))

(describe "org-canvas-pull-quizzes"
  (it "creates headings from remote quizzes"
    (let* ((temp-dir (make-temp-file "pull-quiz-test" t))
           (quiz-file (expand-file-name "quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-quizzes-file quiz-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method url &optional _params)
                             (if (string-match "questions" url)
                                 '()
                               '(((id . 1) (title . "Midterm")
                                  (quiz_type . "assignment")
                                  (description . "<p>Instructions</p>"))))))
                          ((symbol-function 'org-canvas--html-to-org)
                           (lambda (html) (replace-regexp-in-string "<[^>]+>" "" html))))
                  (org-canvas-pull-quizzes)
                  (with-current-buffer (find-file-noselect quiz-file)
                    (goto-char (point-min))
                    (expect (buffer-string) :to-match "Midterm")
                    (expect (buffer-string) :to-match "CANVAS_ID.*1"))))))
        (let ((buf (find-buffer-visiting quiz-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "updates existing headings on re-pull"
    (let* ((temp-dir (make-temp-file "pull-quiz-test" t))
           (quiz-file (expand-file-name "quizzes.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file quiz-file
              (insert "* Old Title
:PROPERTIES:
:CANVAS_ID: 1
:END:
"))
            (let ((org-canvas-quizzes-file quiz-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                             (lambda (_method url &optional _params)
                               (if (string-match "questions" url)
                                   '()
                                 '(((id . 1) (title . "Updated Midterm")
                                    (quiz_type . "assignment"))))))
                            ((symbol-function 'org-canvas--html-to-org)
                             (lambda (html) html)))
                    (org-canvas-pull-quizzes)
                    (with-current-buffer (find-file-noselect quiz-file)
                      (goto-char (point-min))
                      (expect (buffer-string) :to-match "Updated Midterm")))))))
        (let ((buf (find-buffer-visiting quiz-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates file when it does not exist"
    (let* ((temp-dir (make-temp-file "pull-quiz-test" t))
           (quiz-file (expand-file-name "quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-quizzes-file quiz-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'org-canvas--html-to-org)
                           (lambda (html) html)))
                  (org-canvas-pull-quizzes)
                  (expect (file-exists-p quiz-file) :to-be-truthy)))))
        (let ((buf (find-buffer-visiting quiz-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; New Quiz Properties Tests

(describe "org-canvas--quiz-parse-entry new properties"
  (it "parses UNLOCK_AT timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:UNLOCK_AT: <2026-06-01 Mon 00:00>
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :unlock_at) :to-be-truthy))))

  (it "parses LOCK_AT timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:LOCK_AT: <2026-06-30 Tue 23:59>
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :lock_at) :to-be-truthy))))

  (it "parses ACCESS_CODE"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:ACCESS_CODE: exam2026
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :access_code) :to-equal "exam2026"))))

  (it "defaults SHOW_CORRECT_ANSWERS to true"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :show_correct_answers) :to-be t))))

  (it "parses SHOW_CORRECT_ANSWERS false"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SHOW_CORRECT_ANSWERS: false
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :show_correct_answers) :to-be nil))))

  (it "parses SHOW_CORRECT_ANSWERS_AT timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SHOW_CORRECT_ANSWERS_AT: <2026-07-01 Wed 00:00>
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :show_correct_answers_at) :to-be-truthy))))

  (it "parses HIDE_CORRECT_ANSWERS_AT timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:HIDE_CORRECT_ANSWERS_AT: <2026-08-01 Sat 00:00>
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :hide_correct_answers_at) :to-be-truthy))))

  (it "parses HIDE_RESULTS enum"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:HIDE_RESULTS: always
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :hide_results) :to-equal "always"))))

  (it "parses SCORING_POLICY enum"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SCORING_POLICY: keep_latest
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :scoring_policy) :to-equal "keep_latest"))))

  (it "parses ONE_QUESTION_AT_A_TIME"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:ONE_QUESTION_AT_A_TIME: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :one_question_at_a_time) :to-be t))))

  (it "parses CANT_GO_BACK"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:CANT_GO_BACK: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :cant_go_back) :to-be t))))

  (it "parses IP_FILTER"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:IP_FILTER: 192.168.1.0/24
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :ip_filter) :to-equal "192.168.1.0/24")))))

(describe "org-canvas--quiz-build-payload new properties"
  (describe "date properties"
    (it "includes unlock_at when present"
      (let* ((data '(:title "Test" :quiz_type "assignment" :unlock_at "2026-06-01T00:00:00Z"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'unlock_at (alist-get 'quiz payload))
                :to-equal "2026-06-01T00:00:00Z")))

    (it "includes lock_at when present"
      (let* ((data '(:title "Test" :quiz_type "assignment" :lock_at "2026-06-30T23:59:00Z"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'lock_at (alist-get 'quiz payload))
                :to-equal "2026-06-30T23:59:00Z")))

    (it "includes show_correct_answers_at when present"
      (let* ((data '(:title "Test" :quiz_type "assignment"
                     :show_correct_answers_at "2026-07-01T00:00:00Z"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'show_correct_answers_at (alist-get 'quiz payload))
                :to-equal "2026-07-01T00:00:00Z")))

    (it "includes hide_correct_answers_at when present"
      (let* ((data '(:title "Test" :quiz_type "assignment"
                     :hide_correct_answers_at "2026-08-01T00:00:00Z"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'hide_correct_answers_at (alist-get 'quiz payload))
                :to-equal "2026-08-01T00:00:00Z"))))

  (describe "boolean properties"
    (it "includes show_correct_answers true"
      (let* ((data '(:title "Test" :quiz_type "assignment" :show_correct_answers t))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'show_correct_answers (alist-get 'quiz payload)) :to-be t)))

    (it "includes show_correct_answers json-false when nil"
      (let* ((data '(:title "Test" :quiz_type "assignment" :show_correct_answers nil))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'show_correct_answers (alist-get 'quiz payload))
                :to-equal :json-false)))

    (it "includes one_question_at_a_time when true"
      (let* ((data '(:title "Test" :quiz_type "assignment" :one_question_at_a_time t))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'one_question_at_a_time (alist-get 'quiz payload)) :to-be t)))

    (it "excludes one_question_at_a_time when nil"
      (let* ((data '(:title "Test" :quiz_type "assignment" :one_question_at_a_time nil))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (assq 'one_question_at_a_time (alist-get 'quiz payload)) :to-be nil)))

    (it "includes cant_go_back when true"
      (let* ((data '(:title "Test" :quiz_type "assignment" :cant_go_back t))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'cant_go_back (alist-get 'quiz payload)) :to-be t))))

  (describe "access code and filters"
    (it "includes access_code when present"
      (let* ((data '(:title "Test" :quiz_type "assignment" :access_code "secret123"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'access_code (alist-get 'quiz payload))
                :to-equal "secret123")))

    (it "excludes access_code when nil"
      (let* ((data '(:title "Test" :quiz_type "assignment" :access_code nil))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (assq 'access_code (alist-get 'quiz payload)) :to-be nil)))

    (it "includes ip_filter when present"
      (let* ((data '(:title "Test" :quiz_type "assignment" :ip_filter "10.0.0.0/8"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'ip_filter (alist-get 'quiz payload))
                :to-equal "10.0.0.0/8")))

    (it "includes hide_results when present"
      (let* ((data '(:title "Test" :quiz_type "assignment" :hide_results "always"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'hide_results (alist-get 'quiz payload))
                :to-equal "always")))

    (it "excludes hide_results when nil"
      (let* ((data '(:title "Test" :quiz_type "assignment" :hide_results nil))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (assq 'hide_results (alist-get 'quiz payload)) :to-be nil)))

    (it "includes scoring_policy when present"
      (let* ((data '(:title "Test" :quiz_type "assignment" :scoring_policy "keep_latest"))
             (payload (org-canvas--quiz-build-payload data)))
        (expect (alist-get 'scoring_policy (alist-get 'quiz payload))
                :to-equal "keep_latest")))))

(describe "org-canvas--quiz-pull-set-properties new properties"
  (it "sets ACCESS_CODE property"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (access_code . "secret"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "ACCESS_CODE") :to-equal "secret"))))

  (it "sets SHOW_CORRECT_ANSWERS boolean"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (show_correct_answers . t))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "SHOW_CORRECT_ANSWERS") :to-equal "true"))))

  (it "sets HIDE_RESULTS property"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (hide_results . "always"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "HIDE_RESULTS") :to-equal "always"))))

  (it "sets SCORING_POLICY property"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (scoring_policy . "keep_highest"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "SCORING_POLICY") :to-equal "keep_highest"))))

  (it "sets ONE_QUESTION_AT_A_TIME boolean"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (one_question_at_a_time . t))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "ONE_QUESTION_AT_A_TIME") :to-equal "true"))))

  (it "sets CANT_GO_BACK boolean"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (cant_go_back . t))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "CANT_GO_BACK") :to-equal "true"))))

  (it "sets IP_FILTER property"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (ip_filter . "192.168.0.0/16"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "IP_FILTER") :to-equal "192.168.0.0/16"))))

  (it "sets SHOW_CORRECT_ANSWERS_AT timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (show_correct_answers_at . "2026-07-01T00:00:00Z"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "SHOW_CORRECT_ANSWERS_AT") :to-match "<2026-07-01"))))

  (it "sets HIDE_CORRECT_ANSWERS_AT timestamp"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (hide_correct_answers_at . "2026-08-01T00:00:00Z"))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "HIDE_CORRECT_ANSWERS_AT") :to-match "<2026-08-01")))))

;;;; Validation Tests for New Properties

(describe "quiz property validation"
  (it "falls back to first valid for invalid HIDE_RESULTS"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:HIDE_RESULTS: invalid_value
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       ;; validate-property falls back to (car allowed) when default is nil
       (expect (plist-get data :hide_results) :to-equal "always"))))

  (it "falls back to first valid for invalid SCORING_POLICY"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SCORING_POLICY: keep_average
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :scoring_policy) :to-equal "keep_highest"))))

  (it "returns nil for absent HIDE_RESULTS"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :hide_results) :to-be nil))))

  (it "returns nil for absent SCORING_POLICY"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :scoring_policy) :to-be nil))))

  (it "accepts valid HIDE_RESULTS until_after_last_attempt"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:HIDE_RESULTS: until_after_last_attempt
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :hide_results) :to-equal "until_after_last_attempt"))))

  (it "accepts valid SCORING_POLICY keep_highest"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SCORING_POLICY: keep_highest
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :scoring_policy) :to-equal "keep_highest")))))

;;;; SHOW_CORRECT_ANSWERS_LAST_ATTEMPT, ONE_TIME_RESULTS, ONLY_VISIBLE_TO_OVERRIDES

(describe "org-canvas--quiz-parse-entry boolean properties"
  (it "parses SHOW_CORRECT_ANSWERS_LAST_ATTEMPT true"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:SHOW_CORRECT_ANSWERS_LAST_ATTEMPT: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :show_correct_answers_last_attempt) :to-be t))))

  (it "defaults SHOW_CORRECT_ANSWERS_LAST_ATTEMPT to nil"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :show_correct_answers_last_attempt) :to-be nil))))

  (it "parses ONE_TIME_RESULTS true"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:ONE_TIME_RESULTS: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :one_time_results) :to-be t))))

  (it "defaults ONE_TIME_RESULTS to nil"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :one_time_results) :to-be nil))))

  (it "parses ONLY_VISIBLE_TO_OVERRIDES true"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:ONLY_VISIBLE_TO_OVERRIDES: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :only_visible_to_overrides) :to-be t))))

  (it "defaults ONLY_VISIBLE_TO_OVERRIDES to nil"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--quiz-parse-entry)))
       (expect (plist-get data :only_visible_to_overrides) :to-be nil)))))

(describe "org-canvas--quiz-build-payload boolean properties"
  (it "includes show_correct_answers_last_attempt when true"
    (let* ((data '(:title "Test" :quiz_type "assignment"
                   :show_correct_answers_last_attempt t))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'show_correct_answers_last_attempt (alist-get 'quiz payload))
              :to-be t)))

  (it "excludes show_correct_answers_last_attempt when nil"
    (let* ((data '(:title "Test" :quiz_type "assignment"
                   :show_correct_answers_last_attempt nil))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (assq 'show_correct_answers_last_attempt (alist-get 'quiz payload))
              :to-be nil)))

  (it "includes one_time_results when true"
    (let* ((data '(:title "Test" :quiz_type "assignment"
                   :one_time_results t))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'one_time_results (alist-get 'quiz payload))
              :to-be t)))

  (it "excludes one_time_results when nil"
    (let* ((data '(:title "Test" :quiz_type "assignment"
                   :one_time_results nil))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (assq 'one_time_results (alist-get 'quiz payload))
              :to-be nil)))

  (it "includes only_visible_to_overrides when true"
    (let* ((data '(:title "Test" :quiz_type "assignment"
                   :only_visible_to_overrides t))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (alist-get 'only_visible_to_overrides (alist-get 'quiz payload))
              :to-be t)))

  (it "excludes only_visible_to_overrides when nil"
    (let* ((data '(:title "Test" :quiz_type "assignment"
                   :only_visible_to_overrides nil))
           (payload (org-canvas--quiz-build-payload data)))
      (expect (assq 'only_visible_to_overrides (alist-get 'quiz payload))
              :to-be nil))))

(describe "org-canvas--quiz-pull-set-properties boolean properties"
  (it "sets SHOW_CORRECT_ANSWERS_LAST_ATTEMPT true"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (show_correct_answers_last_attempt . t))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT")
               :to-equal "true"))))

  (it "sets SHOW_CORRECT_ANSWERS_LAST_ATTEMPT false"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (show_correct_answers_last_attempt . :json-false))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT")
               :to-equal "false"))))

  (it "sets ONE_TIME_RESULTS true"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (one_time_results . t))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "ONE_TIME_RESULTS")
               :to-equal "true"))))

  (it "sets ONE_TIME_RESULTS false"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (one_time_results . :json-false))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "ONE_TIME_RESULTS")
               :to-equal "false"))))

  (it "sets ONLY_VISIBLE_TO_OVERRIDES true"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (only_visible_to_overrides . t))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "ONLY_VISIBLE_TO_OVERRIDES")
               :to-equal "true"))))

  (it "sets ONLY_VISIBLE_TO_OVERRIDES false"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((quiz '((id . 42) (only_visible_to_overrides . :json-false))))
       (org-canvas--quiz-pull-set-properties (point) quiz "/tmp/quizzes.org")
       (expect (org-entry-get (point) "ONLY_VISIBLE_TO_OVERRIDES")
               :to-equal "false")))))

;;;; Question Group Tests

(describe "org-canvas--question-group-parse-entry"
  (it "extracts name, pick_count, question_points, and bank_id"
    (with-temp-org-buffer
     "* Quiz
** Vocabulary Pool
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 10
:QUESTION_POINTS: 2
:QUESTION_BANK_ID: 42
:END:
"
     (search-forward "Vocabulary Pool")
     (org-back-to-heading)
     (let ((data (org-canvas--question-group-parse-entry "100")))
       (expect (plist-get data :name) :to-equal "Vocabulary Pool")
       (expect (plist-get data :quiz-canvas-id) :to-equal "100")
       (expect (plist-get data :pick-count) :to-equal 10)
       (expect (plist-get data :question-points) :to-equal 2)
       (expect (plist-get data :bank-id) :to-equal 42))))

  (it "returns nil bank-id when absent"
    (with-temp-org-buffer
     "* Quiz
** Group Without Bank
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 5
:QUESTION_POINTS: 3
:END:
"
     (search-forward "Group Without Bank")
     (org-back-to-heading)
     (let ((data (org-canvas--question-group-parse-entry "100")))
       (expect (plist-get data :bank-id) :to-be nil))))

  (it "uses default values for pick_count and question_points"
    (with-temp-org-buffer
     "* Quiz
** Minimal Group
:PROPERTIES:
:TYPE: group
:END:
"
     (search-forward "Minimal Group")
     (org-back-to-heading)
     (let ((data (org-canvas--question-group-parse-entry "100")))
       (expect (plist-get data :pick-count) :to-equal 1)
       (expect (plist-get data :question-points) :to-equal 1))))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Quiz
** Existing Group
:PROPERTIES:
:TYPE: group
:CANVAS_ID: 555
:PICK_COUNT: 5
:QUESTION_POINTS: 2
:END:
"
     (search-forward "Existing Group")
     (org-back-to-heading)
     (let ((data (org-canvas--question-group-parse-entry "100")))
       (expect (plist-get data :canvas-id) :to-equal "555"))))

  (it "includes pom marker"
    (with-temp-org-buffer
     "* Quiz
** Group
:PROPERTIES:
:TYPE: group
:END:
"
     (search-forward "Group")
     (org-back-to-heading)
     (let ((data (org-canvas--question-group-parse-entry "100")))
       (expect (plist-get data :pom) :to-be-truthy)))))

(describe "org-canvas--question-group-build-payload"
  (it "wraps in quiz_groups key"
    (let* ((data '(:name "Pool" :pick-count 5 :question-points 2 :canvas-id nil :bank-id nil))
           (payload (org-canvas--question-group-build-payload data)))
      (expect (assq 'quiz_groups payload) :to-be-truthy)
      (let ((groups (alist-get 'quiz_groups payload)))
        (expect (length groups) :to-equal 1)
        (expect (alist-get 'name (car groups)) :to-equal "Pool")
        (expect (alist-get 'pick_count (car groups)) :to-equal 5)
        (expect (alist-get 'question_points (car groups)) :to-equal 2))))

  (it "includes bank ID on POST (no canvas-id)"
    (let* ((data '(:name "Pool" :pick-count 5 :question-points 2 :canvas-id nil :bank-id 42))
           (payload (org-canvas--question-group-build-payload data))
           (group-obj (car (alist-get 'quiz_groups payload))))
      (expect (alist-get 'assessment_question_bank_id group-obj) :to-equal 42)))

  (it "excludes bank ID on PUT (has canvas-id)"
    (let* ((data '(:name "Pool" :pick-count 5 :question-points 2 :canvas-id "99" :bank-id 42))
           (payload (org-canvas--question-group-build-payload data))
           (group-obj (car (alist-get 'quiz_groups payload))))
      (expect (assq 'assessment_question_bank_id group-obj) :to-be nil)))

  (it "handles nil bank-id on POST"
    (let* ((data '(:name "Pool" :pick-count 3 :question-points 1 :canvas-id nil :bank-id nil))
           (payload (org-canvas--question-group-build-payload data))
           (group-obj (car (alist-get 'quiz_groups payload))))
      (expect (assq 'assessment_question_bank_id group-obj) :to-be nil))))

(describe "org-canvas--question-group-push-to-api (mocked)"
  (it "POSTs for new groups and unwraps response"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest _args)
                   (expect method :to-equal 'POST)
                   (expect url :to-match "quizzes/100/groups$")
                   '((quiz_groups . [((id . 55) (name . "Pool"))])))))
        (let* ((data '(:name "Pool" :canvas-id nil :quiz-canvas-id "100"))
               (payload '((quiz_groups . (((name . "Pool"))))))
               (response (org-canvas--question-group-push-to-api data payload)))
          (expect (alist-get 'id response) :to-equal 55)))))

  (it "PUTs for existing groups"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest _args)
                   (expect method :to-equal 'PUT)
                   (expect url :to-match "quizzes/100/groups/55$")
                   '((quiz_groups . [((id . 55) (name . "Updated Pool"))])))))
        (let* ((data '(:name "Updated Pool" :canvas-id "55" :quiz-canvas-id "100"))
               (payload '((quiz_groups . (((name . "Updated Pool"))))))
               (response (org-canvas--question-group-push-to-api data payload)))
          (expect (alist-get 'id response) :to-equal 55)))))

  (it "re-signals errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API error")))))
        (let ((data '(:name "Pool" :canvas-id nil :quiz-canvas-id "100"))
              (payload '((quiz_groups . (((name . "Pool")))))))
          (expect (org-canvas--question-group-push-to-api data payload)
                  :to-throw 'error))))))

(describe "org-canvas--question-group-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Quiz
** Group
:PROPERTIES:
:TYPE: group
:END:
"
     (search-forward "Group")
     (org-back-to-heading)
     (let ((data (list :name "Group" :pom (point-marker)))
           (response '((id . 77))))
       (org-canvas--question-group-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "77"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Quiz
** Group
:PROPERTIES:
:TYPE: group
:END:
"
     (search-forward "Group")
     (org-back-to-heading)
     (let ((data (list :name "Group" :pom (point-marker)))
           (response '((id . 77))))
       (org-canvas--question-group-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-match "^\\[20[0-9][0-9]-")))))

(describe "org-canvas--sync-quiz-groups (mocked)"
  (it "collects only TYPE=group headings"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:

** Question 1
:PROPERTIES:
:TYPE: multiple_choice_question
:END:

- [X] A

** Group 1
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 5
:END:

** Question 2
:PROPERTIES:
:TYPE: essay_question
:END:
"
         (org-back-to-heading)
         (let ((marker (point-marker)))
           (let ((results (org-canvas--sync-quiz-groups marker "100")))
             (expect (car results) :to-equal 1)
             (expect (cdr results) :to-equal 0)))))))

  (it "returns (0 . 0) when no groups present"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:

** Question 1
:PROPERTIES:
:TYPE: multiple_choice_question
:END:

- [X] A
"
         (org-back-to-heading)
         (let ((marker (point-marker)))
           (let ((results (org-canvas--sync-quiz-groups marker "100")))
             (expect (car results) :to-equal 0)
             (expect (cdr results) :to-equal 0)))))))

  (it "continues after group failure"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         (signal 'error '("First group failed"))
                       '((quiz_groups . [((id . 88))]))))))
          (with-temp-org-buffer
           "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:

** Group A
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 3
:END:

** Group B
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 5
:END:
"
           (org-back-to-heading)
           (let ((marker (point-marker)))
             (let ((results (org-canvas--sync-quiz-groups marker "100")))
               (expect (car results) :to-equal 1)
               (expect (cdr results) :to-equal 1)))))))))

(describe "org-canvas--sync-quiz-questions skips groups"
  (it "skips TYPE=group headings during question sync"
    (with-org-canvas-test-config
      (let ((question-posts 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (when (and (eq method 'POST) (string-match "questions$" url))
                       (setq question-posts (1+ question-posts)))
                     '((id . 999)))))
          (with-temp-org-buffer
           "* Quiz
:PROPERTIES:
:CANVAS_ID: 100
:END:

** Question 1
:PROPERTIES:
:TYPE: multiple_choice_question
:END:

- [X] A

** Group 1
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 5
:END:

** Question 2
:PROPERTIES:
:TYPE: essay_question
:END:

Write an essay.
"
           (org-back-to-heading)
           (let ((marker (point-marker)))
             (org-canvas--sync-quiz-questions marker "100")
             ;; Only 2 questions should be synced, not the group
             (expect question-posts :to-equal 2))))))))

(describe "org-canvas-sync-quizzes with groups (mocked)"
  (it "syncs groups and questions together"
    (let ((temp-dir (make-temp-file "quiz-group-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (group-posts 0)
                 (question-posts 0))
            (with-temp-file org-file
              (insert "* Test Quiz
:PROPERTIES:
:END:

** Question 1
:PROPERTIES:
:END:

- [X] Answer A
- [ ] Answer B

** Vocab Bank
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 5
:QUESTION_POINTS: 2
:QUESTION_BANK_ID: 42
:END:
"))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ;; Quiz POST
                              ((and (eq method 'POST) (string-match "quizzes$" url))
                               '((id . 100) (title . "Test Quiz")))
                              ;; Group POST
                              ((and (eq method 'POST) (string-match "groups$" url))
                               (setq group-posts (1+ group-posts))
                               '((quiz_groups . [((id . 55) (name . "Vocab Bank"))])))
                              ;; Question POST
                              ((and (eq method 'POST) (string-match "questions$" url))
                               (setq question-posts (1+ question-posts))
                               '((id . 200)))
                              (t nil)))))
                  (org-canvas-sync-quizzes)
                  (expect group-posts :to-equal 1)
                  (expect question-posts :to-equal 1)))))
        (delete-directory temp-dir t))))

  (it "reports quiz sync summary via pipeline"
    (let ((temp-dir (make-temp-file "quiz-group-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "quizzes.org" temp-dir))
                 (final-message nil))
            (with-temp-file org-file
              (insert "* Test Quiz
:PROPERTIES:
:END:

** Group 1
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 3
:END:
"))
            (let ((org-canvas-quizzes-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((and (eq method 'POST) (string-match "quizzes$" url))
                               '((id . 100)))
                              ((and (eq method 'POST) (string-match "groups$" url))
                               '((quiz_groups . [((id . 55))])))
                              (t nil))))
                          ((symbol-function 'message)
                           (lambda (fmt &rest args)
                             (setq final-message (apply #'format fmt args)))))
                  (org-canvas-sync-quizzes)
                  ;; Pipeline reports standard summary with success/skipped/failed
                  (expect final-message :to-match "success")
                  (expect final-message :to-match "Quizzes sync")))))
        (delete-directory temp-dir t)))))

;;; org-canvas-quizzes-test.el ends here
