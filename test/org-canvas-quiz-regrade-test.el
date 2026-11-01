;;; org-canvas-quiz-regrade-test.el --- Buttercup tests for the quiz regrade command -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-quiz-regrade' and its `quiz-regrade' batch
;; subcommand (issue #540): one question gets a score on every attempt
;; that drew it, including one drawn from a question bank, which
;; Canvas's own regrade skips.  Every request is answered by a fake
;; keyed on the URL and method; nothing here reaches the network (Hard
;; Rule 2), and the log is never read from the shared buffer (Hard
;; Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-batch)

(defvar test-quiz-regrade--quiz nil
  "The quiz record the fake API answers for GET /quizzes/:id.")
(defvar test-quiz-regrade--quizzes nil
  "Classic quizzes the list endpoint returns, for a regrade by title.")
(defvar test-quiz-regrade--questions nil
  "question_statistics rows the statistics endpoint returns.")
(defvar test-quiz-regrade--rows nil
  "Assignment submission rows, each an attempt history, the fake serves.")
(defvar test-quiz-regrade--submission-ids nil
  "Quiz submission rows: one id per student.")
(defvar test-quiz-regrade--calls nil
  "Requests the fake API was asked for, newest first.")
(defvar test-quiz-regrade--puts nil
  "PUT payloads the fake API was sent, newest first.")
(defvar test-quiz-regrade--fail nil
  "Quiz submission ids whose PUT raises, as strings.")
(defvar test-quiz-regrade--apply-p t
  "Whether a PUT updates the fake's history, as Canvas would.")
(defvar test-quiz-regrade--report nil
  "The report text the last run rendered.")

(defun test-quiz-regrade--question (id text &optional points)
  "A question statistic for question ID whose text is TEXT.
POINTS is its points_possible, nil to leave it out."
  (append `((id . ,id) (question_text . ,(format "<p>%s</p>" text)))
          (when points `((points_possible . ,points)))))

(defun test-quiz-regrade--answer (qid points)
  "One submission_data row: question QID scored POINTS."
  `((question_id . ,qid) (points . ,points) (correct . nil)))

(defun test-quiz-regrade--entry (attempt &rest answers)
  "One submission_history entry: ATTEMPT holding ANSWERS."
  `((id . ,attempt) (attempt . ,attempt)
    (submission_data . ,(vconcat answers))))

(defun test-quiz-regrade--row (uid name &rest entries)
  "An assignment submission row of user UID named NAME with ENTRIES."
  `((user_id . ,uid)
    (user . ((id . ,uid) (name . ,name) (sortable_name . ,name)))
    (submission_history . ,(vconcat entries))))

(defun test-quiz-regrade--submission (id uid)
  "A quiz submission row: ID belongs to user UID."
  `((id . ,id) (user_id . ,uid) (attempt . 1)))

(defun test-quiz-regrade--submission-for (submission-id)
  "Return the user id the fake quiz submission SUBMISSION-ID belongs to."
  (alist-get 'user_id
             (cl-find-if (lambda (s)
                           (equal (number-to-string (alist-get 'id s))
                                  submission-id))
                         test-quiz-regrade--submission-ids)))

(defun test-quiz-regrade--apply (submission-id attempt question-id score)
  "Write SCORE into the fake history the way Canvas would.
SUBMISSION-ID, ATTEMPT and QUESTION-ID say where."
  (let* ((uid (test-quiz-regrade--submission-for submission-id))
         (row (cl-find-if (lambda (r) (equal (alist-get 'user_id r) uid))
                          test-quiz-regrade--rows))
         (entry (cl-find-if (lambda (e) (equal (alist-get 'attempt e) attempt))
                            (append (alist-get 'submission_history row) nil)))
         (answer (cl-find-if (lambda (a)
                               (equal (alist-get 'question_id a) question-id))
                             (append (alist-get 'submission_data entry) nil))))
    (when answer (setf (alist-get 'points answer) score))))

(defun test-quiz-regrade--api (method url &rest args)
  "Answer METHOD URL from the fakes, recording ARGS.
A PUT records its payload, raises for an id in
`test-quiz-regrade--fail', and updates the history unless
`test-quiz-regrade--apply-p' is nil."
  (push (list :method method :url url :args args) test-quiz-regrade--calls)
  (cond
   ((eq method 'PUT)
    (let* ((data (plist-get args :data))
           (row (aref (alist-get 'quiz_submissions data) 0))
           (attempt (alist-get 'attempt row))
           (questions (alist-get 'questions row))
           (question-id (string-to-number (car (car questions))))
           (score (alist-get 'score (cdr (car questions))))
           (submission-id (and (string-match "\\([0-9]+\\)\\'" url)
                               (match-string 1 url))))
      (push (list :url url :attempt attempt :question-id question-id
                  :score score)
            test-quiz-regrade--puts)
      (when (member submission-id test-quiz-regrade--fail)
        (error "Canvas said no to submission %s" submission-id))
      (when test-quiz-regrade--apply-p
        (test-quiz-regrade--apply submission-id attempt question-id score))
      nil))
   ((and (eq method 'GET) (string-match-p "/quizzes/[0-9]+\\'" url))
    test-quiz-regrade--quiz)
   ((and (eq method 'GET) (string-match-p "/statistics\\'" url))
    `((quiz_statistics
       . ,(vector `((question_statistics
                     . ,(vconcat test-quiz-regrade--questions)))))))
   (t (error "Unexpected request: %s %s" method url))))

(defun test-quiz-regrade--all-pages (method url &optional params)
  "Answer the paginated METHOD URL from the fakes, recording PARAMS."
  (push (list :method method :url url :params params) test-quiz-regrade--calls)
  (cond
   ((string-match-p "/assignments/[0-9]+/submissions\\'" url)
    (vconcat test-quiz-regrade--rows))
   ((string-match-p "/quizzes/[0-9]+/submissions\\'" url)
    (vconcat test-quiz-regrade--submission-ids))
   ((string-match-p "/quizzes\\'" url)
    (vconcat test-quiz-regrade--quizzes))
   (t (error "Unexpected paged request: %s" url))))

(defmacro test-quiz-regrade--with-course (&rest body)
  "Run BODY against the fake API, with the report captured.
Every fake and every record starts empty; a spec sets what it needs.
The preflight check is stubbed, since no credentials are read here."
  (declare (indent 0))
  `(let ((test-quiz-regrade--quiz nil)
         (test-quiz-regrade--quizzes nil)
         (test-quiz-regrade--questions nil)
         (test-quiz-regrade--rows nil)
         (test-quiz-regrade--submission-ids nil)
         (test-quiz-regrade--calls nil)
         (test-quiz-regrade--puts nil)
         (test-quiz-regrade--fail nil)
         (test-quiz-regrade--apply-p t)
         (test-quiz-regrade--report nil)
         (org-canvas--dry-run nil)
         (org-canvas-write-allowlist nil))
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  #'test-quiz-regrade--api)
                 ((symbol-function 'org-canvas-api-request-all-pages)
                  #'test-quiz-regrade--all-pages)
                 ((symbol-function 'org-canvas--preflight-check) #'ignore)
                 ((symbol-function 'org-canvas--report-display)
                  (lambda (_buffer render-fn &optional _mode)
                    (with-temp-buffer
                      (funcall render-fn)
                      (setq test-quiz-regrade--report (buffer-string)))))
                 ((symbol-function 'message) #'ignore))
         ,@body))))

(defun test-quiz-regrade--called (url-regexp)
  "Return the requests the fake served whose URL matches URL-REGEXP."
  (cl-remove-if-not (lambda (call)
                      (string-match-p url-regexp (plist-get call :url)))
                    test-quiz-regrade--calls))

(defun test-quiz-regrade--record (id title &optional assignment)
  "A quiz record ID titled TITLE with shadow assignment ASSIGNMENT."
  (append `((id . ,id) (title . ,title) (quiz_type . "assignment")
            (points_possible . 10.0))
          (when assignment `((assignment_id . ,assignment)))))

(defun test-quiz-regrade--refused (fn)
  "Return the text of the `user-error' FN raises, or nil when it does not.
The message is compared by regexp, since `error' renders its quotes in
whatever style `text-quoting-style' asks for."
  (condition-case err
      (progn (funcall fn) nil)
    (user-error (error-message-string err))))

(defun test-quiz-regrade--history ()
  "Set the fakes for one quiz with one question and three attempts.
Question 7, worth 5, is drawn by Adams (attempt 1 scored 0), by Bell
(attempt 1 scored 5) and by Clark (attempt 2 scored 2 and attempt 1
scored 0).  Adams's quiz submission is 501, Bell's 502, Clark's 503."
  (setq test-quiz-regrade--quiz (test-quiz-regrade--record 42 "Quiz One" 900)
        ;; The course list holds the same record, so a title finds it.
        test-quiz-regrade--quizzes (list (test-quiz-regrade--record
                                          42 "Quiz One" 900))
        test-quiz-regrade--questions
        (list (test-quiz-regrade--question 7 "Which term is flawed?" 5.0)
              (test-quiz-regrade--question 8 "Another question?" 5.0))
        test-quiz-regrade--rows
        (list (test-quiz-regrade--row
               1 "Adams, Alice"
               (test-quiz-regrade--entry 1 (test-quiz-regrade--answer 7 0.0)
                                         (test-quiz-regrade--answer 8 5.0)))
              (test-quiz-regrade--row
               2 "Bell, Bob"
               (test-quiz-regrade--entry 1 (test-quiz-regrade--answer 7 5.0)))
              (test-quiz-regrade--row
               3 "Clark, Carol"
               (test-quiz-regrade--entry 1 (test-quiz-regrade--answer 7 0.0))
               (test-quiz-regrade--entry 2 (test-quiz-regrade--answer 7 2.0))))
        test-quiz-regrade--submission-ids
        (list (test-quiz-regrade--submission 501 1)
              (test-quiz-regrade--submission 502 2)
              (test-quiz-regrade--submission 503 3))))

;;;; Naming the quiz and the question

(describe "org-canvas--quiz-regrade-quiz-id"
  (it "reads an id, with or without a leading hash"
    (expect (org-canvas--quiz-regrade-quiz-id "42") :to-equal 42)
    (expect (org-canvas--quiz-regrade-quiz-id "#42") :to-equal 42)
    (expect (org-canvas--quiz-regrade-quiz-id " 42 ") :to-equal 42))

  (it "reads a title as no id at all"
    (expect (org-canvas--quiz-regrade-quiz-id "Quiz One") :to-be nil)
    (expect (org-canvas--quiz-regrade-quiz-id nil) :to-be nil))

  (it "does not read a mixed word as an id"
    (expect (org-canvas--quiz-regrade-quiz-id "42a") :to-be nil)))

(describe "org-canvas--quiz-regrade-quiz-named"
  (it "finds the quiz whose title matches, whatever its case"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quizzes
            (list (test-quiz-regrade--record 42 "Quiz One" 900)
                  (test-quiz-regrade--record 43 "Quiz Two" 901)))
      (expect (alist-get 'id (org-canvas--quiz-regrade-quiz-named "quiz one"))
              :to-equal 42)))

  (it "names the quizzes when the title is not theirs"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quizzes
            (list (test-quiz-regrade--record 42 "Quiz One" 900)))
      (expect (test-quiz-regrade--refused
               (lambda () (org-canvas--quiz-regrade-quiz-named "Quiz Three")))
              :to-match "No classic quiz is titled \"Quiz Three\"")))

  (it "refuses a title that names more than one quiz"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quizzes
            (list (test-quiz-regrade--record 42 "Quiz One" 900)
                  (test-quiz-regrade--record 43 "Quiz One" 901)))
      (expect (test-quiz-regrade--refused
               (lambda () (org-canvas--quiz-regrade-quiz-named "Quiz One")))
              :to-match "names 2 quizzes (42, 43)")))

  (it "reads a quiz without a title as the empty title"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quizzes (list '((id . 42))))
      (expect (alist-get 'id (org-canvas--quiz-regrade-quiz-named ""))
              :to-equal 42))))

(describe "org-canvas--quiz-regrade-quiz"
  (it "asks for the quiz by id when given one"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quiz (test-quiz-regrade--record 42 "Quiz One" 900))
      (let ((record (org-canvas--quiz-regrade-quiz "#42")))
        (expect (alist-get 'id record) :to-equal 42)
        (expect (plist-get (car (test-quiz-regrade--called "/quizzes/42\\'"))
                           :method)
                :to-equal 'GET))))

  (it "looks a title up in the course's quiz list"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quizzes
            (list (test-quiz-regrade--record 42 "Quiz One" 900)))
      ;; A list row is a whole record for this purpose.
      (expect (alist-get 'assignment_id (org-canvas--quiz-regrade-quiz "Quiz One"))
              :to-equal 900)
      (expect (length (test-quiz-regrade--called "/quizzes\\'")) :to-equal 1)))

  (it "refuses a quiz with no shadow assignment, as a practice quiz has none"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quiz (test-quiz-regrade--record 42 "Practice" nil))
      (expect (test-quiz-regrade--refused
               (lambda () (org-canvas--quiz-regrade-quiz "42")))
              :to-match "has no shadow assignment")))

(describe "org-canvas--quiz-regrade-statistics"
  (it "asks for every version and returns the questions"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--questions
            (list (test-quiz-regrade--question 7 "Flawed?" 5.0)
                  (test-quiz-regrade--question 8 "Fine?" 5.0)))
      (expect (length (org-canvas--quiz-regrade-statistics 42)) :to-equal 2)
      (let ((call (car (test-quiz-regrade--called "/statistics\\'"))))
        (expect (equal (plist-get call :args)
                       '(:params (("all_versions" . "true"))))
                :to-be-truthy))))

  (it "reads a reply with no statistics as no questions"
    (test-quiz-regrade--with-course
      (expect (org-canvas--quiz-regrade-statistics 42) :to-be nil))))

(describe "org-canvas--quiz-regrade-question"
  (it "finds the one question holding the fragment, whatever its case"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((question (org-canvas--quiz-regrade-question
                       test-quiz-regrade--quiz "WHICH TERM")))
        (expect (plist-get question :id) :to-equal 7)
        (expect (plist-get question :points) :to-equal 5.0)
        ;; The HTML is gone: the report names the question as text.
        (expect (plist-get question :text) :to-equal "Which term is flawed?"))))

  (it "takes the quiz's points when the statistics leave them out"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quiz (test-quiz-regrade--record 42 "Quiz One" 900)
            test-quiz-regrade--questions
            (list (test-quiz-regrade--question 7 "Which term is flawed?" nil)))
      (expect (plist-get (org-canvas--quiz-regrade-question
                          test-quiz-regrade--quiz "flawed")
                         :points)
              :to-equal 10.0)))

  (it "names the question count when nothing holds the fragment"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (expect (test-quiz-regrade--refused
               (lambda ()
                 (org-canvas--quiz-regrade-question
                  test-quiz-regrade--quiz "not there")))
              :to-match "its statistics list 2"))))

  (it "refuses a fragment that names more than one question"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (expect (test-quiz-regrade--refused
               (lambda ()
                 (org-canvas--quiz-regrade-question
                  test-quiz-regrade--quiz "?")))
              :to-match "matches 2 questions"))))

(describe "org-canvas--quiz-regrade-score"
  (it "reads full, or nothing, as the question's points"
    (expect (org-canvas--quiz-regrade-score "full" 5.0) :to-equal 5.0)
    (expect (org-canvas--quiz-regrade-score nil 5.0) :to-equal 5.0)
    (expect (org-canvas--quiz-regrade-score 'full 5.0) :to-equal 5.0))

  (it "reads a number, as a number or as text"
    (expect (org-canvas--quiz-regrade-score 3 5.0) :to-equal 3)
    (expect (org-canvas--quiz-regrade-score "2.5" 5.0) :to-equal 2.5))

  (it "refuses what is neither full nor a number"
    (expect (test-quiz-regrade--refused
             (lambda () (org-canvas--quiz-regrade-score "half" 5.0)))
            :to-match "Score \"half\" is neither")
    (expect (test-quiz-regrade--refused
             (lambda () (org-canvas--quiz-regrade-score "-1" 5.0)))
            :to-match "Score \"-1\" is neither"))

  (it "refuses full when Canvas does not say what the question is worth"
    (expect (test-quiz-regrade--refused
             (lambda () (org-canvas--quiz-regrade-score "full" nil)))
            :to-match "does not say what the question is worth"))

  (it "refuses a negative score"
    (expect (test-quiz-regrade--refused
             (lambda () (org-canvas--quiz-regrade-score -1 5.0)))
            :to-match "cannot be negative"))

  (it "refuses a score above the question's points"
    (expect (test-quiz-regrade--refused
             (lambda () (org-canvas--quiz-regrade-score 6 5.0)))
            :to-match "above the question")))

;;;; Reading the attempts

(describe "org-canvas--quiz-regrade-attempts"
  (it "reads every attempt that drew the question, each with its student"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((attempts (org-canvas--quiz-regrade-attempts
                       test-quiz-regrade--quiz 7)))
        (expect (length attempts) :to-equal 4)
        (expect (plist-get (nth 0 attempts) :name) :to-equal "Adams, Alice")
        (expect (plist-get (nth 0 attempts) :attempt) :to-equal 1)
        (expect (plist-get (nth 0 attempts) :points) :to-equal 0.0)
        (expect (plist-get (nth 3 attempts) :attempt) :to-equal 2)
        (expect (plist-get (nth 3 attempts) :points) :to-equal 2.0))))

  (it "asks for the history and the student with the submissions"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (org-canvas--quiz-regrade-attempts test-quiz-regrade--quiz 7)
      (let ((call (car (test-quiz-regrade--called
                        "/assignments/900/submissions\\'"))))
        (expect (equal (plist-get call :params)
                       '(("include[]" . "submission_history")
                         ("include[]" . "user")))
                :to-be-truthy))))

  (it "leaves out an attempt that drew another question"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quiz (test-quiz-regrade--record 42 "Quiz" 900)
            test-quiz-regrade--rows
            (list (test-quiz-regrade--row
                   1 "Adams, Alice"
                   (test-quiz-regrade--entry 1 (test-quiz-regrade--answer 8 5.0)))))
      (expect (org-canvas--quiz-regrade-attempts test-quiz-regrade--quiz 7)
              :to-be nil)))

  (it "leaves out an answer whose question id is text, not a number"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quiz (test-quiz-regrade--record 42 "Quiz" 900)
            test-quiz-regrade--rows
            (list (test-quiz-regrade--row
                   1 "Adams, Alice"
                   (test-quiz-regrade--entry
                    1 (test-quiz-regrade--answer "7" 0.0)))))
      (expect (org-canvas--quiz-regrade-attempts test-quiz-regrade--quiz 7)
              :to-be nil)))

  (it "names a student the reply does not name, and falls back to the name"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--quiz (test-quiz-regrade--record 42 "Quiz" 900)
            test-quiz-regrade--rows
            (list `((user_id . 1)
                    (user . ((id . 1) (name . "Adams, Alice")))
                    (submission_history
                     . ,(vector (test-quiz-regrade--entry
                                 1 (test-quiz-regrade--answer 7 0.0)))))
                  `((user_id . 2)
                    (submission_history
                     . ,(vector (test-quiz-regrade--entry
                                 1 (test-quiz-regrade--answer 7 0.0)))))))
      (expect (mapcar (lambda (a) (plist-get a :name))
                      (org-canvas--quiz-regrade-attempts
                       test-quiz-regrade--quiz 7))
              :to-equal '("Adams, Alice" "User 2")))))

(describe "org-canvas--quiz-regrade-submission-ids"
  (it "pairs each student with their quiz submission id"
    (test-quiz-regrade--with-course
      (setq test-quiz-regrade--submission-ids
            (list (test-quiz-regrade--submission 501 1)
                  (test-quiz-regrade--submission 502 2)))
      (expect (org-canvas--quiz-regrade-submission-ids 42)
              :to-equal '((1 . 501) (2 . 502))))))

(describe "org-canvas--quiz-regrade-plan"
  (it "changes every attempt under the score that has a submission"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let* ((attempts (org-canvas--quiz-regrade-attempts
                        test-quiz-regrade--quiz 7))
             (plan (org-canvas--quiz-regrade-plan
                    attempts '((1 . 501) (2 . 502) (3 . 503)) 5.0)))
        ;; Adams's and Clark's first attempt, and Clark's second: Bell's
        ;; attempt is already at the score and is neither changed nor kept.
        (expect (length (plist-get plan :changes)) :to-equal 3)
        (expect (plist-get (car (plist-get plan :changes)) :submission-id)
                :to-equal 501)
        (expect (plist-get plan :skipped) :to-be nil))))

  (it "keeps back an attempt with no score for the question"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((plan (org-canvas--quiz-regrade-plan
                   (list (list :user-id 1 :name "Adams" :attempt 1
                               :points nil))
                   '((1 . 501)) 5.0)))
        (expect (plist-get plan :changes) :to-be nil)
        (expect (plist-get (car (plist-get plan :skipped)) :reason)
                :to-equal "no score for the question"))))

  (it "keeps back an attempt whose student has no quiz submission"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((plan (org-canvas--quiz-regrade-plan
                   (list (list :user-id 1 :name "Adams" :attempt 1
                               :points 0.0))
                   nil 5.0)))
        (expect (plist-get plan :changes) :to-be nil)
        (expect (plist-get (car (plist-get plan :skipped)) :reason)
                :to-equal "no quiz submission on Canvas")))))

;;;; Sending and reading back

(describe "org-canvas--quiz-regrade-send"
  (it "sends the attempt and the question's score"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (expect (org-canvas--quiz-regrade-send
               test-quiz-regrade--quiz 7 5.0
               (list :submission-id 501 :attempt 1 :name "Adams"))
              :to-equal 'sent)
      (let ((put (car test-quiz-regrade--puts)))
        (expect (plist-get put :score) :to-equal 5.0)
        (expect (plist-get put :attempt) :to-equal 1)
        (expect (plist-get put :question-id) :to-equal 7))))

  (it "returns the error's text when Canvas refuses"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (setq test-quiz-regrade--fail '("501"))
      (expect (org-canvas--quiz-regrade-send
               test-quiz-regrade--quiz 7 5.0
               (list :submission-id 501 :attempt 1 :name "Adams"))
              :to-equal "Canvas said no to submission 501"))))

(describe "org-canvas--quiz-regrade-verify"
  (it "reads each attempt back and says whether it holds the score"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (setq test-quiz-regrade--rows
            (list (test-quiz-regrade--row
                   1 "Adams, Alice"
                   (test-quiz-regrade--entry
                    1 (test-quiz-regrade--answer 7 5.0)))
                  (test-quiz-regrade--row
                   2 "Bell, Bob"
                   (test-quiz-regrade--entry
                    1 (test-quiz-regrade--answer 7 0.0)))))
      (let ((verified (org-canvas--quiz-regrade-verify
                       test-quiz-regrade--quiz 7 5.0
                       (list (list :user-id 1 :name "Adams" :attempt 1)
                             (list :user-id 2 :name "Bell" :attempt 1)
                             (list :user-id 3 :name "Clark" :attempt 1)))))
        (expect (mapcar (lambda (v) (plist-get v :ok)) verified)
                :to-equal '(t nil nil))
        (expect (plist-get (nth 2 verified) :points) :to-be nil)))))

;;;; The command

(describe "org-canvas-quiz-regrade"
  (it "names the attempts under the score and sends nothing on a dry run"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((result (org-canvas-quiz-regrade "Quiz One" "flawed" "full" t)))
        (expect (plist-get result :dry-run) :to-be-truthy)
        (expect (length (plist-get (plist-get result :plan) :changes))
                :to-equal 3)
        (expect test-quiz-regrade--puts :to-be nil)
        (expect test-quiz-regrade--report :to-match "Adams, Alice")
        (expect test-quiz-regrade--report :to-match
                "Dry run: nothing sent"))))

  (it "honours the global dry run"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((org-canvas--dry-run t))
        (org-canvas-quiz-regrade "Quiz One" "flawed")
        (expect test-quiz-regrade--puts :to-be nil))))

  (it "sends the score to each attempt and reads them all back"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((result (org-canvas-quiz-regrade "Quiz One" "flawed")))
        (expect (length test-quiz-regrade--puts) :to-equal 3)
        (expect (plist-get result :sent) :to-equal 3)
        (expect (plist-get result :failed) :to-be nil)
        (expect (cl-every (lambda (v) (plist-get v :ok))
                          (plist-get result :verified))
                :to-be-truthy)
        (expect test-quiz-regrade--report :to-match "read back 3 at 5.0"))))

  (it "reports a failed send and leaves the rest alone"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (setq test-quiz-regrade--fail '("501"))
      (let ((result (org-canvas-quiz-regrade "Quiz One" "flawed")))
        (expect (length (plist-get result :failed)) :to-equal 1)
        (expect (car (car (plist-get result :failed))) :to-equal "Adams, Alice")
        (expect (plist-get result :sent) :to-equal 2)
        (expect test-quiz-regrade--report :to-match "1 failed"))))

  (it "names an attempt that did not read back at the score"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (setq test-quiz-regrade--apply-p nil)
      (let ((result (org-canvas-quiz-regrade "Quiz One" "flawed")))
        (expect (plist-get result :sent) :to-equal 3)
        (expect (cl-remove-if (lambda (v) (plist-get v :ok))
                              (plist-get result :verified))
                :not :to-be nil)
        (expect test-quiz-regrade--report :to-match
                "3 at another score"))))

  (it "says there was nothing to set when every attempt holds the score"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((result (org-canvas-quiz-regrade "Quiz One" "flawed" 0)))
        (expect (plist-get (plist-get result :plan) :changes) :to-be nil)
        (expect test-quiz-regrade--report :to-match "Nothing to set"))))

  (it "says there was nothing to set on a dry run with no changes"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (org-canvas-quiz-regrade "Quiz One" "flawed" 0 t)
      (expect test-quiz-regrade--report :to-match "Dry run: nothing to set")))

  (it "names the attempt it kept back, and why"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      ;; Clark's quiz submission is gone, so his two attempts cannot go.
      (setq test-quiz-regrade--submission-ids
            (list (test-quiz-regrade--submission 501 1)
                  (test-quiz-regrade--submission 502 2)))
      (org-canvas-quiz-regrade "Quiz One" "flawed")
      (expect test-quiz-regrade--report
              :to-match "kept back, no quiz submission on Canvas")
      (expect (length test-quiz-regrade--puts) :to-equal 1)))

  (it "refuses to send when the allowlist does not name submissions"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((org-canvas-write-allowlist '("pages.org")))
        (expect (org-canvas-quiz-regrade "Quiz One" "flawed")
                :to-throw 'org-canvas-write-scope-error))
      (expect test-quiz-regrade--puts :to-be nil)))

  (it "sends when the allowlist names submissions"
    (test-quiz-regrade--with-course
      (test-quiz-regrade--history)
      (let ((org-canvas-write-allowlist '("submissions")))
        (org-canvas-quiz-regrade "Quiz One" "flawed")
        (expect (length test-quiz-regrade--puts) :to-equal 3)))))

;;;; The batch subcommand

(describe "org-canvas-batch--quiz-regrade-args"
  (it "reads the two words and defaults the score to full"
    (expect (org-canvas-batch--quiz-regrade-args '("42" "flawed"))
            :to-equal '("42" "flawed" "full" nil)))

  (it "reads --score with a number, or joined to it"
    (expect (org-canvas-batch--quiz-regrade-args '("42" "flawed" "--score" "3"))
            :to-equal '("42" "flawed" "3" nil))
    (expect (org-canvas-batch--quiz-regrade-args '("42" "flawed" "--score=2.5"))
            :to-equal '("42" "flawed" "2.5" nil)))

  (it "reads a dry run spelled either way, anywhere"
    (expect (org-canvas-batch--quiz-regrade-args
             '("--dry-run" "42" "flawed"))
            :to-equal '("42" "flawed" "full" t))
    (expect (org-canvas-batch--quiz-regrade-args
             '("42" "-n" "flawed"))
            :to-equal '("42" "flawed" "full" t)))

  (it "refuses an option it does not know"
    (expect (org-canvas-batch--quiz-regrade-args '("42" "flawed" "--all"))
            :to-throw 'org-canvas-batch-usage-error))

  (it "refuses --score with nothing after it"
    (expect (org-canvas-batch--quiz-regrade-args '("42" "flawed" "--score"))
            :to-throw 'org-canvas-batch-usage-error))

  (it "refuses too few or too many words"
    (expect (org-canvas-batch--quiz-regrade-args '("flawed"))
            :to-throw 'org-canvas-batch-usage-error)
    (expect (org-canvas-batch--quiz-regrade-args '("42" "flawed" "extra"))
            :to-throw 'org-canvas-batch-usage-error)))

(describe "org-canvas-batch--cmd-quiz-regrade"
  (it "passes the quiz, the text, the score and the dry run through"
    (let (seen)
      (spy-on 'org-canvas-quiz-regrade
              :and-call-fake
              (lambda (quiz fragment score)
                (setq seen (list quiz fragment score org-canvas--dry-run))
                '(:failed nil :verified nil)))
      (expect (org-canvas-batch--cmd-quiz-regrade
               '(:args ("42" "flawed" "--score" "3" "--dry-run")
                 :dry-run nil))
              :to-equal 0)
      (expect seen :to-equal '("42" "flawed" "3" t))))

  (it "returns 1 when an attempt failed to send"
    (spy-on 'org-canvas-quiz-regrade
            :and-call-fake
            (lambda (&rest _) (list :failed '(("Adams" . "boom"))
                                    :verified nil)))
    (expect (org-canvas-batch--cmd-quiz-regrade
             '(:args ("42" "flawed") :dry-run nil))
            :to-equal 1))

  (it "returns 1 when an attempt did not read back at the score"
    (spy-on 'org-canvas-quiz-regrade
            :and-call-fake
            (lambda (&rest _)
              (list :failed nil
                    :verified (list (list :name "Adams" :attempt 1
                                          :points 0.0 :ok nil)))))
    (expect (org-canvas-batch--cmd-quiz-regrade
             '(:args ("42" "flawed") :dry-run nil))
            :to-equal 1)))
