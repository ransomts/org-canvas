;;; org-canvas-quiz-regrade.el --- Credit one quiz question across attempts -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A classic quiz question drawn from a question bank had a flawed
;; premise, and every attempt that drew it was owed full credit on that
;; one question (issue #540).  Canvas's own regrade skips a question in
;; a bank-linked group, and nothing here changed a question's score on
;; an existing attempt, so the repair was a scratch script.
;;
;; `org-canvas-quiz-regrade' (`scripts/org-canvas quiz-regrade QUIZ
;; "question text" [--score full|N] [--dry-run]') gives every attempt
;; that drew one question a score: it finds the question by a fragment
;; of its text, reads every attempt through the shadow assignment's
;; submission history, names the attempts scoring under the score, and,
;; unless it is a dry run, sends the score to each and reads it back.
;; Every attempt is raised, not only the one Canvas kept, so the kept
;; score is right whichever attempt wins under
;; `SCORING_POLICY keep_highest'.
;;
;; Nothing is written to Org.  A quiz's table (`quiz-results.org' from
;; `org-canvas-pull-quiz-submissions') and its grading file are pull
;; products, and the next pull of either shows the scores this sent.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/quizzes/:quiz_id
;;       the title, points_possible, and the shadow assignment's id.
;;   GET /courses/:id/quizzes/:quiz_id/statistics?all_versions=true
;;       question_statistics carries each question's id and its text as
;;       HTML, for every version an attempt could have drawn.  A
;;       bank-drawn question is listed here and not in
;;       /quizzes/:quiz_id/questions, which is why this is the endpoint
;;       that names it.
;;   GET /courses/:id/assignments/:assignment_id/submissions
;;       ?include[]=submission_history&include[]=user
;;       one row per student, its history one entry per attempt, whose
;;       submission_data names every question the attempt drew and the
;;       points it scored.  The quiz submission endpoint answers the
;;       latest attempt alone.
;;   GET /courses/:id/quizzes/:quiz_id/submissions
;;       the quiz submission ids, wrapped under quiz_submissions: the
;;       id identifies a student's submission, the attempt number picks
;;       the attempt to score.
;;   PUT /courses/:id/quizzes/:quiz_id/submissions/:id
;;       {"quiz_submissions": [{"attempt": N,
;;        "questions": {"QID": {"score": S}}}]}

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)
;; The course's classic quizzes and its quiz submission ids, which
;; pulling a quiz's attempts already reads.
(require 'org-canvas-quiz-submissions)
;; Question text arrives as HTML, and one normalizer is enough.
(require 'org-canvas-diff-fields)

(defconst org-canvas--quiz-regrade-buffer-name "*canvas-quiz-regrade*"
  "Buffer the regrade report is rendered into.")

;;;; Naming the Quiz and the Question

(defun org-canvas--quiz-regrade-quiz-id (quiz)
  "Return QUIZ as a quiz id, or nil when QUIZ is a title.
QUIZ is an id, #ID, or a title."
  (let ((trimmed (and (stringp quiz) (string-trim quiz))))
    (when (and trimmed (string-match "\\`#?\\([0-9]+\\)\\'" trimmed))
      (string-to-number (match-string 1 trimmed)))))

(defun org-canvas--quiz-regrade-quiz-named (title)
  "Return the course's classic quiz titled TITLE.
TITLE must name exactly one quiz: the error names the ids when it
names none or more than one."
  (let* ((quizzes (org-canvas--quiz-submissions-fetch-quizzes))
         (wanted (downcase (string-trim title)))
         (matches (cl-remove-if-not
                   (lambda (q)
                     (string= wanted
                              (downcase (or (alist-get 'title q) ""))))
                   quizzes)))
    (pcase (length matches)
      (1 (car matches))
      (0 (user-error "No classic quiz is titled %S" title))
      (_ (user-error "%S names %d quizzes (%s); give one quiz's id"
                     title (length matches)
                     (mapconcat (lambda (q) (format "%s" (alist-get 'id q)))
                                matches ", "))))))

(defun org-canvas--quiz-regrade-quiz (quiz)
  "Return QUIZ's Canvas record, from a title, an id or #ID.
Signals when the quiz has no shadow assignment, which is what a
practice quiz is: its attempts have no submission history to read."
  (let* ((id (org-canvas--quiz-regrade-quiz-id quiz))
         (record (if id
                     (org-canvas-api-request
                      'GET (org-canvas-api-course-endpoint "quizzes/%s" id))
                   (org-canvas--quiz-regrade-quiz-named quiz))))
    (unless (org-canvas--alist-get-non-null 'assignment_id record)
      (user-error "Quiz %S has no shadow assignment (a practice or survey quiz), so there are no attempts to regrade"
                  (org-canvas--alist-get-non-null 'title record)))
    record))

(defun org-canvas--quiz-regrade-question-text (question)
  "Return QUESTION's text, from the statistics reply, as plain text."
  (org-canvas--diff-html-to-text (or (alist-get 'question_text question) "")))

(defun org-canvas--quiz-regrade-statistics (quiz-id)
  "Return QUIZ-ID's question statistics, every version, as a list."
  (let* ((stats (org-canvas-api-request
                 'GET (org-canvas-api-course-endpoint
                       "quizzes/%s/statistics" quiz-id)
                 :params '(("all_versions" . "true"))))
         (quiz-stats (car (append (alist-get 'quiz_statistics stats) nil))))
    (append (alist-get 'question_statistics quiz-stats) nil)))

(defun org-canvas--quiz-regrade-question (record fragment)
  "Return the one question of RECORD with FRAGMENT in its text.
The result is a plist (:id :text :points), the question's own points
unless the statistics leave them out, when the quiz's stand in.
Signals unless exactly one question matches, naming those that do."
  (let* ((questions (org-canvas--quiz-regrade-statistics
                     (org-canvas--alist-get-non-null 'id record)))
         (wanted (downcase fragment))
         (matches (cl-remove-if-not
                   (lambda (q)
                     (string-match-p
                      (regexp-quote wanted)
                      (downcase (org-canvas--quiz-regrade-question-text q))))
                   questions)))
    (unless matches
      (user-error "No question in %S holds %S; its statistics list %d"
                  (org-canvas--alist-get-non-null 'title record)
                  fragment (length questions)))
    (when (> (length matches) 1)
      (user-error "%S matches %d questions (%s); give more of the text"
                  fragment (length matches)
                  (mapconcat (lambda (q)
                               (format "%s: %s" (alist-get 'id q)
                                       (org-canvas--quiz-regrade-question-text q)))
                             matches "; ")))
    (let ((question (car matches)))
      (list :id (org-canvas--alist-get-non-null 'id question)
            :text (org-canvas--quiz-regrade-question-text question)
            :points (or (org-canvas--alist-get-non-null 'points_possible question)
                        (org-canvas--alist-get-non-null 'points_possible record))))))

(defun org-canvas--quiz-regrade-score (score points)
  "Return SCORE as a number, with `full' or nil meaning POINTS.
Signals when SCORE is neither, when it is negative, when it is above
POINTS, or when POINTS is unknown and SCORE asks for them."
  (let ((value (cond
                ((or (null score) (equal score "full") (eq score 'full))
                 points)
                ((numberp score) score)
                ((and (stringp score)
                      (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" score))
                 (string-to-number score))
                (t (user-error "Score %S is neither `full' nor a number" score)))))
    (unless (numberp value)
      (user-error "Canvas does not say what the question is worth; give the score as a number"))
    (when (< value 0)
      (user-error "A score cannot be negative"))
    (when (and (numberp points) (> value points))
      (user-error "%s is above the question's %s points"
                  (format "%s" value) (format "%s" points)))
    value))

;;;; Reading the Attempts

(defun org-canvas--quiz-regrade-attempts (record question-id)
  "Return every attempt on RECORD's assignment that drew QUESTION-ID.
Each is a plist (:user-id :name :attempt :points), POINTS nil when the
attempt holds no score for the question."
  (let ((rows (org-canvas-api-request-all-pages
               'GET (org-canvas-api-course-endpoint
                     "assignments/%s/submissions"
                     (org-canvas--alist-get-non-null 'assignment_id record))
               '(("include[]" . "submission_history")
                 ("include[]" . "user")))))
    (cl-loop for row in (append rows nil)
             for user = (alist-get 'user row)
             for user-id = (org-canvas--alist-get-non-null 'user_id row)
             for name = (or (alist-get 'sortable_name user)
                            (alist-get 'name user)
                            (format "User %s" user-id))
             append (cl-loop
                     for entry in (append
                                   (org-canvas--alist-get-non-null
                                    'submission_history row)
                                   nil)
                     for attempt = (org-canvas--alist-get-non-null 'attempt entry)
                     for data = (append (org-canvas--alist-get-non-null
                                         'submission_data entry)
                                        nil)
                     for answer = (cl-find-if
                                   (lambda (d)
                                     (let ((qid (org-canvas--alist-get-non-null
                                                 'question_id d)))
                                       (and (numberp qid)
                                            (numberp question-id)
                                            (= qid question-id))))
                                   data)
                     when answer
                     collect (list :user-id user-id
                                   :name name
                                   :attempt attempt
                                   :points (org-canvas--alist-get-non-null
                                            'points answer))))))

(defun org-canvas--quiz-regrade-submission-ids (quiz-id)
  "Return an alist of user id to quiz submission id for QUIZ-ID."
  (let ((org-canvas--api-unwrap-key 'quiz_submissions))
    (cl-loop for s in (append
                       (org-canvas-api-request-all-pages
                        'GET (org-canvas-api-course-endpoint
                              "quizzes/%s/submissions" quiz-id))
                       nil)
             collect (cons (org-canvas--alist-get-non-null 'user_id s)
                           (org-canvas--alist-get-non-null 'id s)))))

(defun org-canvas--quiz-regrade-plan (attempts ids score)
  "Return what raising SCORE needs, from ATTEMPTS and IDS.
The result is a plist (:changes :skipped).  Each change is an attempt
scoring under SCORE with its :submission-id; each skip is one that
cannot be changed, its :reason saying why: no score for the question,
or no quiz submission on Canvas."
  (let (changes skipped)
    (dolist (attempt (append attempts nil))
      (let ((points (plist-get attempt :points))
            (submission (alist-get (plist-get attempt :user-id) ids)))
        (cond
         ((not (numberp points))
          (push (append attempt (list :reason "no score for the question"))
                skipped))
         ((>= points score))
         ((not submission)
          (push (append attempt
                        (list :reason "no quiz submission on Canvas"))
                skipped))
         (t (push (append attempt (list :submission-id submission)) changes)))))
    (list :changes (nreverse changes) :skipped (nreverse skipped))))

;;;; Sending and Reading Back

(defun org-canvas--quiz-regrade-send (record question-id score change)
  "Send SCORE for CHANGE's attempt on QUESTION-ID in RECORD.
Return `sent', or the error's text when the send failed."
  (condition-case err
      (progn
        (org-canvas-api-request
         'PUT (org-canvas-api-course-endpoint
               "quizzes/%s/submissions/%s"
               (org-canvas--alist-get-non-null 'id record)
               (plist-get change :submission-id))
         :data (list (cons 'quiz_submissions
                           (vector
                            (list (cons 'attempt (plist-get change :attempt))
                                  (cons 'questions
                                        (list (cons (number-to-string question-id)
                                                    (list (cons 'score score))))))))))
        'sent)
    (error (error-message-string err))))

(defun org-canvas--quiz-regrade-verify (record question-id score changes)
  "Read each attempt's SCORE back from RECORD.
CHANGES are the attempts just sent for QUESTION-ID.  Each result is a
plist (:name :attempt :points :ok), :points what Canvas held when read
back."
  (let ((after (org-canvas--quiz-regrade-attempts record question-id)))
    (mapcar
     (lambda (change)
       (let* ((same (cl-find-if
                     (lambda (a)
                       (and (equal (plist-get a :user-id)
                                   (plist-get change :user-id))
                            (equal (plist-get a :attempt)
                                   (plist-get change :attempt))))
                     after))
              (points (plist-get same :points)))
         (list :name (plist-get change :name)
               :attempt (plist-get change :attempt)
               :points points
               :ok (and (numberp points) (= points score)))))
     (append changes nil))))

(defun org-canvas--quiz-regrade-message (result)
  "Return RESULT's report, one line per attempt."
  (let* ((question (plist-get result :question))
         (plan (plist-get result :plan))
         (changes (plist-get plan :changes))
         (skipped (plist-get plan :skipped))
         (verified (plist-get result :verified))
         (failed (plist-get result :failed)))
    (concat
     (format "org-canvas quiz regrade: %s\n\n" (plist-get result :title))
     (format "Question %s, worth %s: %s\n"
             (plist-get question :id)
             (format "%s" (plist-get question :points))
             (plist-get question :text))
     (format "%d attempt(s) drew it; %d score(s) under %s\n"
             (length (plist-get result :attempts))
             (length changes)
             (format "%s" (plist-get result :score)))
     (mapconcat (lambda (c)
                  (format "  %-28s attempt %s: %s -> %s\n"
                          (plist-get c :name) (plist-get c :attempt)
                          (format "%s" (plist-get c :points))
                          (format "%s" (plist-get result :score))))
                changes "")
     (mapconcat (lambda (s)
                  (format "  %-28s attempt %s: kept back, %s\n"
                          (plist-get s :name) (plist-get s :attempt)
                          (plist-get s :reason)))
                skipped "")
     (cond
      ((plist-get result :dry-run)
       (if changes
           "\nDry run: nothing sent.  Run without --dry-run to set these.\n"
         "\nDry run: nothing to set.\n"))
      (failed
       (format "\nSent %d attempt(s); %d failed: %s\n"
               (- (length changes) (length failed)) (length failed)
               (mapconcat (lambda (f) (format "%s: %s" (car f) (cdr f)))
                          failed "; ")))
      ((null changes) "\nNothing to set.\n")
      (t (format "\nSent %d attempt(s); read back %s.\n"
                 (length changes)
                 (let ((bad (cl-remove-if (lambda (v) (plist-get v :ok)) verified)))
                   (if bad
                       (format "%d at another score (%s)"
                               (length bad)
                               (mapconcat
                                (lambda (v)
                                  (format "%s attempt %s: %s"
                                          (plist-get v :name) (plist-get v :attempt)
                                          (format "%s" (plist-get v :points))))
                                bad "; "))
                     (format "%d at %s" (length verified)
                             (format "%s" (plist-get result :score)))))))))))

;;;; The Command

;;;###autoload
(defun org-canvas-quiz-regrade (quiz fragment &optional score dry-run)
  "Give every attempt that drew one quiz question SCORE.
QUIZ is a classic quiz's exact title, its id as text, or #ID; FRAGMENT
is a piece of the question's text, which must name exactly one
question.  SCORE is `full' (or nil) for the question's points, or a
number no greater than them.  DRY-RUN, or `org-canvas--dry-run', only
reads: it names the attempts it would change and sends nothing.

Every attempt is read through the quiz's shadow assignment, so a
question drawn from a question bank -- which Canvas's own regrade
skips -- is found the way a typed-in one is.  Each attempt scoring
under SCORE is sent that score and read back, and the report names
every attempt, every one kept back and every one that did not read
back at the score.  Nothing is written to Org.

Return a plist: :title, :question (:id :text :points), :attempts (one
plist per attempt that drew it), :score, :plan (:changes :skipped),
:sent the count, :failed (NAME . ERROR) per failed send, :verified (one
plist per attempt read back), :dry-run."
  (org-canvas--preflight-check)
  (let* ((dry-run (or dry-run org-canvas--dry-run))
         (record (org-canvas--quiz-regrade-quiz quiz))
         (question (org-canvas--quiz-regrade-question record fragment))
         (question-id (plist-get question :id))
         (score (org-canvas--quiz-regrade-score score (plist-get question :points)))
         (attempts (org-canvas--quiz-regrade-attempts record question-id))
         (ids (org-canvas--quiz-regrade-submission-ids
               (org-canvas--alist-get-non-null 'id record)))
         (plan (org-canvas--quiz-regrade-plan attempts ids score))
         (changes (plist-get plan :changes))
         (failed nil)
         sent verified)
    (unless (or dry-run (null changes))
      (org-canvas--check-write-feature 'submissions "a quiz regrade")
      (dolist (change (append changes nil))
        (let ((outcome (org-canvas--quiz-regrade-send record question-id score change)))
          (if (eq outcome 'sent)
              (setq sent (1+ (or sent 0)))
            (push (cons (plist-get change :name) outcome) failed)))))
    (when (and sent (null failed))
      (setq verified (org-canvas--quiz-regrade-verify
                      record question-id score changes)))
    (let ((result (list :title (org-canvas--alist-get-non-null 'title record)
                        :question question
                        :attempts attempts
                        :score score
                        :plan plan
                        :sent sent
                        :failed (nreverse failed)
                        :verified verified
                        :dry-run dry-run)))
      (org-canvas--report-display org-canvas--quiz-regrade-buffer-name
                                  (lambda ()
                                    (insert (org-canvas--quiz-regrade-message result))))
      (when (or failed
                (cl-remove-if (lambda (v) (plist-get v :ok)) verified))
        (message "Org-canvas: %d attempt(s) did not read back at %s"
                 (+ (length failed)
                    (length (cl-remove-if (lambda (v) (plist-get v :ok)) verified)))
                 (format "%s" score)))
      result)))

(provide 'org-canvas-quiz-regrade)
;;; org-canvas-quiz-regrade.el ends here
