;;; org-canvas-quiz-results-test.el --- Buttercup tests for the quiz results pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-quiz-results': each classic quiz's statistics
;; folded into one table per quiz in quiz-results.org.  Every request
;; is answered by a fake keyed on the URL; nothing here reaches the
;; network (Hard Rule 2), and the log is never read from the shared
;; buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: the tier list lives in org-canvas.el and the quiz
;; link needs `org-canvas-quizzes-file' to be special.
(require 'org-canvas)

(defvar test-quiz-results--quizzes nil
  "Quizzes the fake API lists for the course.")
(defvar test-quiz-results--statistics nil
  "Alist of quiz id to the statistics object the fake API answers.
A value of `refuse' answers a 403 instead.")

(defun test-quiz-results--list (_method url &optional _params)
  "Answer the paginated URL from the fake tables."
  (cond
   ((string-match "/quizzes\\'" url) test-quiz-results--quizzes)
   (t (error "Unexpected paginated request: %s" url))))

(defun test-quiz-results--one (_method url &rest _)
  "Answer the single-object URL from the fake tables."
  (cond
   ((string-match "/quizzes/\\([0-9]+\\)/statistics" url)
    (let ((stats (alist-get (string-to-number (match-string 1 url)) test-quiz-results--statistics)))
      (if (eq stats 'refuse)
          (signal 'org-canvas-permission-error (list "403 Forbidden"))
        `((quiz_statistics . ,(if stats (vector stats) []))))))
   (t (error "Unexpected request: %s" url))))

(defun test-quiz-results--quiz (id title &rest extra)
  "A published classic quiz ID titled TITLE with an assignment, plus EXTRA pairs."
  (append extra `((id . ,id) (title . ,title) (published . t)
                  (assignment_id . ,(+ 700 id)) (quiz_type . "assignment"))))

(defun test-quiz-results--answer (id responses correct &optional names)
  "An answer ID picked RESPONSES times, CORRECT or not, naming NAMES.
Canvas lists the students behind every answer; NAMES stands in for
that and must never reach the file."
  `((id . ,id) (text . ,(format "Answer %s" id)) (correct . ,(if correct t :json-false))
    (responses . ,responses)
    (user_ids . ,(vconcat (mapcar (lambda (_) 987654) names)))
    (user_names . ,(vconcat names))))

(defun test-quiz-results--question (position type text answers &rest extra)
  "A question at POSITION of TYPE with HTML TEXT and ANSWERS, plus EXTRA pairs."
  (append extra
          `((id . ,(+ 100 position)) (position . ,position) (question_type . ,type)
            (question_text . ,text) (answers . ,(vconcat answers))
            (responses . ,(apply #'+ (mapcar (lambda (a) (or (alist-get 'responses a) 0)) answers))))))

(defun test-quiz-results--stats (students questions &rest summary)
  "A statistics object over STUDENTS with QUESTIONS and SUMMARY pairs."
  `((id . 1) (generated_at . "2026-09-10T12:00:00Z") (multiple_attempts_exist . t)
    (question_statistics . ,(vconcat questions))
    (submission_statistics . ,(append summary `((unique_count . ,students))))))

(defmacro test-quiz-results--with-course (quizzes statistics &rest body)
  "Run BODY with the fake API serving QUIZZES and STATISTICS.
The results and quizzes files live in a temp directory."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "quiz-results-" t))
          (org-canvas-quiz-results-file (expand-file-name "quiz-results.org" dir))
          (org-canvas-quizzes-file (expand-file-name "quizzes.org" dir))
          (test-quiz-results--quizzes ,quizzes)
          (test-quiz-results--statistics ,statistics))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-quiz-results--list)
                     ((symbol-function 'org-canvas-api-request) #'test-quiz-results--one)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-quiz-results-file org-canvas-quizzes-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-quiz-results--file ()
  "Return quiz-results.org's text."
  (with-temp-buffer (insert-file-contents org-canvas-quiz-results-file) (buffer-string)))

(defun test-quiz-results--row (position)
  "Return the table row of question POSITION, cells trimmed, or nil."
  (let ((text (test-quiz-results--file)))
    (when (string-match (format "^| %d *|\\(.*\\)$" position) text)
      (mapcar #'string-trim (split-string (match-string 1 text) "|" t)))))

(defconst test-quiz-results--q1
  (test-quiz-results--question
   1 "multiple_choice_question" "<p>What is <b>ethics</b>?&nbsp;&amp; why</p>"
   (list (test-quiz-results--answer 11 12 nil '("Ada Lovelace"))
         (test-quiz-results--answer 12 40 t '("Grace Hopper" "Alan Turing"))
         (test-quiz-results--answer 13 3 nil))
   '(answered_student_count . 55) '(correct_student_ratio . 0.7272)
   '(difficulty_index . 0.7272)
   `(point_biserials . ,(vector '((answer_id . 11) (point_biserial . -0.2) (correct . :json-false))
                                '((answer_id . 12) (point_biserial . 0.41) (correct . t)))))
  "A multiple-choice question with three answers.")

(defconst test-quiz-results--q2
  (test-quiz-results--question
   2 "true_false_question" "<p>Ethics is easy.</p>"
   (list (test-quiz-results--answer 21 5 nil) (test-quiz-results--answer 22 50 t))
   '(answered_student_count . 55) '(correct_student_ratio . 0.909)
   '(difficulty_index . 0.909)
   `(point_biserials . ,(vector '((answer_id . 21) (point_biserial . 0.1) (correct . :json-false)))))
  "A true/false question whose correct answer has no point biserial.")

(defconst test-quiz-results--q3
  (test-quiz-results--question
   3 "essay_question"
   (concat "<p>" (make-string 80 ?x) "</p>") nil
   '(answered_student_count . 50) '(correct_student_ratio . :null)
   '(difficulty_index . :null))
  "An essay question: no answers, no ratio, a long text.")

(defun test-quiz-results--survey (id title type &rest extra)
  "A published survey ID titled TITLE of quiz TYPE, plus EXTRA pairs.
No assignment unless EXTRA gives one, as for an ungraded survey."
  (append extra `((id . ,id) (title . ,title) (published . t)
                  (assignment_id . :null) (quiz_type . ,type))))

(defun test-quiz-results--survey-answer (id text responses &optional correct)
  "A survey answer ID reading TEXT picked RESPONSES times.
CORRECT marks it right, as Canvas marks a survey's first option."
  `((id . ,id) (text . ,text) (correct . ,(if correct t :json-false))
    (responses . ,responses) (user_ids . []) (user_names . [])))

(defconst test-quiz-results--survey-q1
  (test-quiz-results--question
   1 "multiple_choice_question" "<p>Ethics classes are important.</p>"
   (list (test-quiz-results--survey-answer 51 "Strongly agree" 33 t)
         (test-quiz-results--survey-answer 52 "<p>Agree</p>" 27)
         (test-quiz-results--survey-answer 53 "Neutral, unsure" 17)
         (test-quiz-results--survey-answer 54 :null 5)
         (test-quiz-results--survey-answer 55 "Disagree" 1))
   '(answered_student_count . 83) '(correct_student_ratio . 0.397)
   '(difficulty_index . 0.397)
   `(point_biserials . ,(vector '((answer_id . 51) (point_biserial . 0.3) (correct . t)))))
  "A five-point agreement item; one answer has no text.")

(defconst test-quiz-results--survey-q2
  (test-quiz-results--question
   2 "essay_question" "<p>Comments?</p>" nil
   '(answered_student_count . 40))
  "A survey's free-text question: no answers listed.")

(describe "org-canvas--quiz-results-skip-reason"
  (it "keeps a published survey of either kind without an assignment"
    (expect (org-canvas--quiz-results-skip-reason
             (test-quiz-results--survey 1 "S" "survey"))
            :to-be nil)
    (expect (org-canvas--quiz-results-skip-reason
             (test-quiz-results--survey 2 "G" "graded_survey"))
            :to-be nil))
  (it "names why an unpublished quiz or a practice quiz is left out"
    (expect (org-canvas--quiz-results-skip-reason
             (test-quiz-results--survey 1 "S" "survey" '(published . :json-false)))
            :to-be 'unpublished)
    (expect (org-canvas--quiz-results-skip-reason
             (test-quiz-results--quiz 3 "P" '(assignment_id . :null)
                                      '(quiz_type . "practice_quiz")))
            :to-be 'no-assignment)
    (expect (org-canvas--quiz-results-skip-reason (test-quiz-results--quiz 4 "Q"))
            :to-be nil)))

(describe "org-canvas--quiz-results-plain-text"
  (it "strips tags, decodes entities, collapses whitespace and cuts to the width"
    (expect (org-canvas--quiz-results-plain-text "<p>What is <b>ethics</b>?&nbsp;&amp; why</p>")
            :to-equal "What is ethics ? & why")
    (expect (org-canvas--quiz-results-plain-text "a |\n b\t&lt;c&gt; &quot;d&quot; &#39;e&#39;")
            :to-equal "a b <c> \"d\" 'e'")
    (expect (org-canvas--quiz-results-plain-text nil) :to-equal "")
    (let ((cut (org-canvas--quiz-results-plain-text (make-string 80 ?x))))
      (expect (length cut) :to-equal 60)
      (expect cut :to-match "\\.\\.\\.\\'"))))

(describe "org-canvas--quiz-results-type-label"
  (it "shortens the known types and derives a label for an unknown one"
    (expect (org-canvas--quiz-results-type-label "multiple_choice_question") :to-equal "multiple choice")
    (expect (org-canvas--quiz-results-type-label "true_false_question") :to-equal "true/false")
    (expect (org-canvas--quiz-results-type-label "hot_spot_question") :to-equal "hot spot")
    (expect (org-canvas--quiz-results-type-label nil) :to-equal "-")))

(describe "org-canvas--quiz-results-question-row"
  (it "reads the counts, the ratio as a percent, the correct answer's point biserial and the responses"
    ;; :type is pre-bound: inside `expect' Emacs 29's oclosure shadows it.
    (let* ((row (org-canvas--quiz-results-question-row test-quiz-results--q1))
           (row-type (plist-get row :type)))
      (expect (plist-get row :position) :to-equal 1)
      (expect (plist-get row :text) :to-equal "What is ethics ? & why")
      (expect row-type :to-equal "multiple choice")
      (expect (plist-get row :answered) :to-equal 55)
      (expect (plist-get row :correct) :to-be-close-to 72.72 1)
      (expect (plist-get row :difficulty) :to-equal 0.7272)
      (expect (plist-get row :discrimination) :to-equal 0.41)
      (expect (plist-get row :responses) :to-equal "12, 40*, 3")))

  (it "falls back to the first point biserial and leaves an essay's blanks nil"
    (expect (plist-get (org-canvas--quiz-results-question-row test-quiz-results--q2) :discrimination)
            :to-equal 0.1)
    (let* ((row (org-canvas--quiz-results-question-row test-quiz-results--q3))
           (row-type (plist-get row :type)))
      (expect (plist-get row :correct) :to-be nil)
      (expect (plist-get row :difficulty) :to-be nil)
      (expect (plist-get row :discrimination) :to-be nil)
      (expect (plist-get row :responses) :to-equal "-")
      (expect row-type :to-equal "essay")))

  (it "leaves a survey question's scoring out and labels its counts by answer text"
    (let ((row (org-canvas--quiz-results-question-row test-quiz-results--survey-q1 t)))
      (expect (plist-get row :answered) :to-equal 83)
      (expect (plist-get row :correct) :to-be nil)
      (expect (plist-get row :difficulty) :to-be nil)
      (expect (plist-get row :discrimination) :to-be nil)
      (expect (plist-get row :responses)
              :to-equal "Strongly agree 33; Agree 27; Neutral, unsure 17; #4 5; Disagree 1"))
    (expect (plist-get (org-canvas--quiz-results-question-row test-quiz-results--survey-q2 t)
                       :responses)
            :to-equal "-")
    (expect (org-canvas--quiz-results-labelled-responses
             `((answers . ,(vector '((id . 1) (text . 42))))))
            :to-equal "42 0"))

  (it "counts an answer with no responses field as zero"
    (let ((row (org-canvas--quiz-results-question-row
                (test-quiz-results--question 4 "true_false_question" "t?"
                                             (list '((id . 41) (correct . t)))
                                             '(answered_student_count . :null)))))
      (expect (plist-get row :responses) :to-equal "0*")
      (expect (plist-get row :answered) :to-equal 0))))

(describe "org-canvas--quiz-results-entry"
  (it "folds the summary and sorts the questions by position"
    (let ((entry (org-canvas--quiz-results-entry
                  (test-quiz-results--quiz 5 "Quiz 5")
                  (test-quiz-results--stats 55 (list test-quiz-results--q2 test-quiz-results--q1)
                                            '(score_average . 24.5) '(score_high . 27.0)
                                            '(score_low . 9.5) '(score_stdev . 3.25)
                                            '(duration_average . 754)))))
      (expect (plist-get entry :name) :to-equal "Quiz 5")
      (expect (plist-get entry :students) :to-equal 55)
      (expect (plist-get entry :mean) :to-equal 24.5)
      (expect (plist-get entry :duration) :to-equal "0:13")
      (expect (plist-get entry :generated-at) :to-equal "2026-09-10T12:00:00Z")
      (expect (mapcar (lambda (r) (plist-get r :position)) (plist-get entry :rows)) :to-equal '(1 2))))

  (it "names a quiz by its id and has no rows when Canvas has no report"
    (let ((entry (org-canvas--quiz-results-entry '((id . 9) (published . t)) nil)))
      (expect (plist-get entry :name) :to-equal "Quiz 9")
      (expect (plist-get entry :students) :to-equal 0)
      (expect (plist-get entry :rows) :to-be nil)
      (expect (plist-get entry :duration) :to-be nil))))

(describe "org-canvas--quiz-results-duration"
  (it "renders seconds as h:mm and nothing for no number"
    (expect (org-canvas--quiz-results-duration 754) :to-equal "0:13")
    (expect (org-canvas--quiz-results-duration 3661.4) :to-equal "1:01")
    (expect (org-canvas--quiz-results-duration nil) :to-be nil)))

(describe "org-canvas--quiz-results-number"
  (it "renders with the asked decimals, an integer as is, and a dash for nothing"
    (expect (org-canvas--quiz-results-number 24.55) :to-equal "24.6")
    (expect (org-canvas--quiz-results-number 0.7272 2) :to-equal "0.73")
    (expect (org-canvas--quiz-results-number 27) :to-equal "27")
    (expect (org-canvas--quiz-results-number nil) :to-equal "-")
    (expect (org-canvas--quiz-results-number "A") :to-equal "A")))

(describe "org-canvas-pull-quiz-results"
  (it "writes one heading per published quiz, in order, with its properties and table"
    (test-quiz-results--with-course
        (list (test-quiz-results--quiz 1 "Syllabus Quiz")
              (test-quiz-results--quiz 2 "Draft Quiz" '(published . :json-false))
              (test-quiz-results--quiz 3 "Practice" '(assignment_id . :null))
              (test-quiz-results--quiz 4 "Midterm"))
        (list (cons 1 (test-quiz-results--stats 55 (list test-quiz-results--q1 test-quiz-results--q2
                                                         test-quiz-results--q3)
                                                '(score_average . 24.5) '(score_high . 27.0)
                                                '(score_low . 9.5) '(score_stdev . 3.25)
                                                '(duration_average . 754)))
              (cons 4 (test-quiz-results--stats 0 (list test-quiz-results--q1))))
      (org-canvas-pull-quiz-results)
      (let ((text (test-quiz-results--file)))
        (expect text :to-match "^#\\+TITLE: Quiz Results\n")
        (expect text :to-match "^#\\+LAST_SYNCED:")
        (expect text :to-match "^\\* Syllabus Quiz\n")
        (expect text :to-match "^\\* Midterm\n")
        (expect text :not :to-match "Draft Quiz\\|Practice")
        (expect (string-match "\\* Syllabus Quiz" text) :to-be-less-than (string-match "\\* Midterm" text))
        (expect text :to-match ":QUIZ_ID: +1\n")
        (expect text :to-match ":STUDENTS: +55\n")
        (expect text :to-match ":MEAN: +24\\.5\n")
        (expect text :to-match ":HIGH: +27\\.0\n")
        (expect text :to-match ":LOW: +9\\.5\n")
        (expect text :to-match ":STDEV: +3\\.2\n")
        (expect text :to-match ":DURATION: +0:13\n")
        (expect text :to-match ":GENERATED_AT: +<2026-09-10")
        (expect text :to-match "^| # +| Question +| Type +| Answered +| Correct +| Difficulty +| Discrimination +| Responses +|$")
        (expect (test-quiz-results--row 1)
                :to-equal '("What is ethics ? & why" "multiple choice" "55" "73%" "0.73" "0.41" "12, 40*, 3"))
        (expect (test-quiz-results--row 2)
                :to-equal '("Ethics is easy." "true/false" "55" "91%" "0.91" "0.10" "5, 50*"))
        (expect (nthcdr 1 (test-quiz-results--row 3)) :to-equal '("essay" "50" "-" "-" "-" "-"))
        (expect text :to-match ":STUDENTS: +0\n")
        (expect text :to-match "^No attempts yet\\.$"))))

  (it "never writes the students Canvas names behind an answer"
    (test-quiz-results--with-course
        (list (test-quiz-results--quiz 1 "Syllabus Quiz"))
        (list (cons 1 (test-quiz-results--stats 55 (list test-quiz-results--q1 test-quiz-results--q2))))
      (org-canvas-pull-quiz-results)
      (let ((text (test-quiz-results--file)))
        (expect text :not :to-match "Ada Lovelace\\|Grace Hopper\\|Alan Turing")
        (expect text :not :to-match "987654\\|user_names\\|user_ids"))))

  (it "links a heading to its quizzes.org heading when that file holds the id"
    (test-quiz-results--with-course
        (list (test-quiz-results--quiz 1 "Syllabus Quiz"))
        (list (cons 1 (test-quiz-results--stats 55 (list test-quiz-results--q1))))
      (with-temp-file org-canvas-quizzes-file
        (insert "* Syllabus Quiz (v2)\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** Q1\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
      (org-canvas-pull-quiz-results)
      (expect (test-quiz-results--file)
              :to-match "^\\* \\[\\[file:quizzes.org::\\*Syllabus Quiz (v2)\\]\\[Syllabus Quiz\\]\\]\n")))

  (it "skips a quiz whose statistics are refused, with one warning, and keeps the rest"
    (test-quiz-results--with-course
        (list (test-quiz-results--quiz 1 "Syllabus Quiz") (test-quiz-results--quiz 4 "Midterm"))
        (list (cons 1 'refuse)
              (cons 4 (test-quiz-results--stats 10 (list test-quiz-results--q2))))
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-quiz-results))
        (expect (length warned) :to-equal 1)
        (expect (car warned) :to-match "statistics of quiz 1"))
      (let ((text (test-quiz-results--file)))
        (expect text :not :to-match "Syllabus Quiz")
        (expect text :to-match "^\\* Midterm\n")
        (expect (test-quiz-results--row 2) :to-equal '("Ethics is easy." "true/false" "55" "91%" "0.91" "0.10" "5, 50*")))))

  (it "rewrites the whole file on a re-pull and keeps nothing written by hand"
    (test-quiz-results--with-course
        (list (test-quiz-results--quiz 1 "Syllabus Quiz") (test-quiz-results--quiz 4 "Midterm"))
        (list (cons 1 (test-quiz-results--stats 55 (list test-quiz-results--q1)))
              (cons 4 nil))
      (org-canvas-pull-quiz-results)
      (with-temp-buffer
        (insert-file-contents org-canvas-quiz-results-file)
        (goto-char (point-max))
        (insert "A note I wrote\n")
        (write-region (point-min) (point-max) org-canvas-quiz-results-file))
      ;; The midterm is unpublished; the syllabus quiz gains a question.
      (setq test-quiz-results--quizzes
            (list (test-quiz-results--quiz 1 "Syllabus Quiz")
                  (test-quiz-results--quiz 4 "Midterm" '(published . :json-false))))
      (setq test-quiz-results--statistics
            (list (cons 1 (test-quiz-results--stats 60 (list test-quiz-results--q1 test-quiz-results--q2)))))
      (org-canvas-pull-quiz-results)
      (let ((text (test-quiz-results--file)))
        (expect text :not :to-match "Midterm")
        (expect text :not :to-match "A note I wrote")
        (expect text :to-match ":STUDENTS: +60\n")
        (expect (test-quiz-results--row 2) :not :to-be nil)
        (expect (length (split-string text "^\\* " t)) :to-equal 2))))

  (it "writes the empty-file note when the course has no published quiz"
    (test-quiz-results--with-course
        (list (test-quiz-results--quiz 2 "Draft Quiz" '(published . :json-false)))
        nil
      (org-canvas-pull-quiz-results)
      (expect (test-quiz-results--file) :to-match "Canvas returned 0 items")))

  (it "writes an ungraded survey with no assignment, its answers by text and no scoring"
    (test-quiz-results--with-course
        (list (test-quiz-results--quiz 1 "Syllabus Quiz")
              (test-quiz-results--survey 5 "Pulse Check" "survey"))
        (list (cons 1 (test-quiz-results--stats 55 (list test-quiz-results--q3)
                                                '(score_average . 24.5)))
              (cons 5 (test-quiz-results--stats 83 (list test-quiz-results--survey-q1
                                                         test-quiz-results--survey-q2)
                                                '(score_average . 1.0) '(score_high . 1.0)
                                                '(score_low . 1.0) '(score_stdev . 0.0)
                                                '(duration_average . 120))))
      (org-canvas-pull-quiz-results)
      (let* ((text (test-quiz-results--file))
             (survey (substring text (string-match "^\\* Pulse Check" text))))
        (expect text :to-match "^\\* Syllabus Quiz\n")
        (expect survey :to-match ":QUIZ_ID: +5\n")
        (expect survey :to-match ":STUDENTS: +83\n")
        (expect survey :to-match ":DURATION: +0:02\n")
        (expect survey :not :to-match ":MEAN:\\|:HIGH:\\|:LOW:\\|:STDEV:")
        (expect survey :to-match "^| # +| Question +| Type +| Answered +| Responses +|$")
        (expect survey :not :to-match "Correct\\|Difficulty\\|Discrimination")
        (expect (test-quiz-results--row 1)
                :to-equal '("Ethics classes are important." "multiple choice" "83"
                            "Strongly agree 33; Agree 27; Neutral, unsure 17; #4 5; Disagree 1"))
        (expect (test-quiz-results--row 2) :to-equal '("Comments?" "essay" "40" "-"))
        ;; The graded quiz above keeps its scoring.
        (expect text :to-match ":MEAN: +24\\.5\n")
        (expect text :to-match "| Correct +| Difficulty +| Discrimination +|"))))

  (it "treats a graded survey as a survey: its assignment keeps it, the scoring goes"
    (test-quiz-results--with-course
        (list (test-quiz-results--survey 6 "Graded Pulse" "graded_survey"
                                         '(assignment_id . 706)))
        (list (cons 6 (test-quiz-results--stats 83 (list test-quiz-results--survey-q1)
                                                '(score_average . 2.0) '(score_high . 2.0))))
      (org-canvas-pull-quiz-results)
      (let ((text (test-quiz-results--file)))
        (expect text :to-match "^\\* Graded Pulse\n")
        (expect text :not :to-match ":MEAN:\\|:HIGH:")
        (expect (car (last (test-quiz-results--row 1))) :to-match "\\`Strongly agree 33; "))))

  (it "says in the closing line how many quizzes it left out and why"
    (let ((messages nil))
      (test-quiz-results--with-course
          (list (test-quiz-results--quiz 1 "Syllabus Quiz")
                (test-quiz-results--quiz 2 "Draft Quiz" '(published . :json-false))
                (test-quiz-results--survey 3 "Draft Survey" "survey"
                                           '(published . :json-false))
                (test-quiz-results--quiz 4 "Practice" '(assignment_id . :null)
                                         '(quiz_type . "practice_quiz"))
                (test-quiz-results--quiz 7 "Locked"))
          (list (cons 1 (test-quiz-results--stats 55 (list test-quiz-results--q1)))
                (cons 7 'refuse))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                  ((symbol-function 'org-canvas--log-warning) #'ignore))
          (org-canvas-pull-quiz-results)))
      (expect (car messages)
              :to-equal (concat "Quiz results pull complete: 1 quizzes, 1 with attempts;"
                                " skipped 4 (2 unpublished, 1 without an assignment,"
                                " 1 statistics refused)."))))

  (it "names the skipped quizzes even when none is left to write"
    (let ((messages nil))
      (test-quiz-results--with-course
          (list (test-quiz-results--quiz 2 "Draft Quiz" '(published . :json-false)))
          nil
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (org-canvas-pull-quiz-results)))
      (expect (car messages)
              :to-equal (concat "Quiz results pull complete: 0 quizzes, 0 with attempts;"
                                " skipped 1 (1 unpublished)."))))

  (it "keeps the closing line plain when nothing was left out"
    (let ((messages nil))
      (test-quiz-results--with-course
          (list (test-quiz-results--quiz 1 "Syllabus Quiz"))
          (list (cons 1 nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (org-canvas-pull-quiz-results)))
      (expect (car messages)
              :to-equal "Quiz results pull complete: 1 quizzes, 0 with attempts.")))

  (it "is in the pull tiers after the rubric results and bound in the pull menu"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-rubric-results names)
              :to-be-less-than (cl-position 'org-canvas-pull-quiz-results names)))
    (let ((source (with-temp-buffer
                    (insert-file-contents (locate-library "org-canvas-transient.el"))
                    (buffer-string))))
      (expect source :to-match "(\"t\" \"Quiz results (statistics)\" org-canvas-pull-quiz-results)"))))

(provide 'org-canvas-quiz-results-test)
;;; org-canvas-quiz-results-test.el ends here
