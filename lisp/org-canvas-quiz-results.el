;;; org-canvas-quiz-results.el --- Pull a classic quiz's item analysis from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file pulls the item analysis of every classic quiz into
;; quiz-results.org: the score spread, and question by question how many
;; answered, how many were right, how hard the question was, how well it
;; told strong students from weak ones, and how the distractors were
;; picked.  It answers "which question is broken?" without opening the
;; web UI's statistics page.  It is pull-only and the file is derived:
;; every pull rewrites it whole, so nothing written by hand survives.
;;
;; FILE STRUCTURE
;; ==============
;; In quiz-results.org, one level-1 heading per published classic quiz
;; that has an assignment, and per published survey with or without
;; one, in Canvas's order, titled by the quiz title and linked to its
;; heading in quizzes.org when that file holds the quiz's CANVAS_ID:
;;   :QUIZ_ID:  :STUDENTS:  :MEAN:  :HIGH:  :LOW:  :STDEV:  :DURATION:
;;   :GENERATED_AT:
;;   | # | Question | Type | Answered | Correct | Difficulty
;;     | Discrimination | Responses |
;; Correct is the share of students who got the question right,
;; Difficulty is Canvas's difficulty index, Discrimination the point
;; biserial of the correct answer, and Responses the response count per
;; answer in answer order with the correct answer starred ("12, 40*,
;; 3"), so a distractor nobody picks is visible.  A quiz nobody has
;; taken gets the heading and a "No attempts yet" line instead.
;;
;; A survey (quiz_type survey or graded_survey) has no right answers:
;; Canvas marks one anyway, and a graded survey gives full marks for
;; taking it.  So a survey's heading carries no MEAN, HIGH, LOW or
;; STDEV, its table drops Correct, Difficulty and Discrimination, and
;; Responses names each answer by its text ("Agree 27; Disagree 5").
;;
;; A quiz left out is counted in the closing message by reason:
;; unpublished, a practice quiz with no assignment, or statistics
;; Canvas refused.
;;
;; PERSONAL DATA
;; =============
;; The statistics reply names the students behind every answer
;; (`user_ids', `user_names'); neither is ever read, let alone written.
;; Only aggregates reach the file, but with a small class a question
;; everyone else got right can still identify someone, so treat
;; quiz-results.org as rubric-results.org is treated: keep it out of a
;; course repository and out of anything shared.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/quizzes
;;       classic quizzes only; a New Quiz is an assignment and has no
;;       statistics endpoint in the public API.  Paginated.
;;   GET /courses/:id/quizzes/:id/statistics
;;       one object: `submission_statistics' (score_average, score_high,
;;       score_low, score_stdev, duration_average, unique_count) and
;;       `question_statistics', one per question with its answers.  Not
;;       paginated; one request per quiz.  Without all_versions the
;;       report covers each student's latest attempt.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-quiz-results-file (org-canvas--path "quiz-results.org")
  "Path to the quiz-results.org file.
It holds the item analysis of every classic quiz; keep it out of a
course repository."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-quiz-results-file "quiz-results.org")

(org-canvas-register-properties "quiz-results"
  :label "Quiz Results"
  :file-var 'org-canvas-quiz-results-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "QUIZ_ID" :data-key :quiz_id :type number :pull-only t
     :doc "Canvas id of the classic quiz")
    (:org-prop "STUDENTS" :data-key :students :type number :pull-only t
     :doc "Students whose latest attempt the statistics cover")
    (:org-prop "MEAN" :data-key :mean :type number :pull-only t
     :doc "Mean score")
    (:org-prop "HIGH" :data-key :high :type number :pull-only t
     :doc "Highest score")
    (:org-prop "LOW" :data-key :low :type number :pull-only t
     :doc "Lowest score")
    (:org-prop "STDEV" :data-key :stdev :type number :pull-only t
     :doc "Standard deviation of the scores")
    (:org-prop "DURATION" :data-key :duration :type string :pull-only t
     :doc "Mean time spent, as h:mm")
    (:org-prop "GENERATED_AT" :data-key :generated_at :type timestamp :pull-only t
     :doc "When Canvas generated the statistics")))

(defconst org-canvas--quiz-results-type-labels
  '(("multiple_choice_question" . "multiple choice")
    ("true_false_question" . "true/false")
    ("multiple_answers_question" . "multiple answers")
    ("short_answer_question" . "short answer")
    ("fill_in_multiple_blanks_question" . "fill in blanks")
    ("multiple_dropdowns_question" . "dropdowns")
    ("matching_question" . "matching")
    ("numerical_question" . "numerical")
    ("calculated_question" . "formula")
    ("essay_question" . "essay")
    ("file_upload_question" . "file upload")
    ("text_only_question" . "text"))
  "Short labels for Canvas's classic quiz question types.")

(defconst org-canvas--quiz-results-text-width 60
  "How many characters of a question's text the table shows.")

(defconst org-canvas--quiz-results-survey-types '("survey" "graded_survey")
  "The quiz types whose answers are opinions, not right or wrong.")

(defconst org-canvas--quiz-results-skip-labels
  '((unpublished . "unpublished")
    (no-assignment . "without an assignment")
    (refused . "statistics refused"))
  "How the closing message names each reason a quiz was left out.")

;;;; Fetching

(defun org-canvas--quiz-results-fetch-quizzes ()
  "Return every classic quiz of the course, in Canvas's order.
The quizzes endpoint lists classic quizzes only; a New Quiz is an
assignment with no statistics to pull.  Which of them get a heading
is `org-canvas--quiz-results-partition's to say."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint "quizzes"))
          nil))

(defun org-canvas--quiz-results-survey-p (quiz)
  "Return non-nil when QUIZ is a survey, graded or not."
  (member (alist-get 'quiz_type quiz) org-canvas--quiz-results-survey-types))

(defun org-canvas--quiz-results-skip-reason (quiz)
  "Return why QUIZ gets no heading, or nil when it gets one.
`unpublished' for a quiz students cannot take, `no-assignment' for a
practice quiz, which has no assignment and so no grade to analyse.
A survey is kept without an assignment: an ungraded survey has none,
and its answers are the whole reason to run it."
  (cond ((not (eq (alist-get 'published quiz) t)) 'unpublished)
        ((org-canvas--quiz-results-survey-p quiz) nil)
        ((not (org-canvas--alist-get-non-null 'assignment_id quiz))
         'no-assignment)))

(defun org-canvas--quiz-results-count-skip (reason skipped)
  "Return the SKIPPED alist of reason to count with REASON counted once more."
  (let ((cell (assq reason skipped)))
    (if cell
        (progn (setcdr cell (1+ (cdr cell))) skipped)
      (append skipped (list (cons reason 1))))))

(defun org-canvas--quiz-results-partition (quizzes)
  "Split QUIZZES into those that get a heading and a count of the rest.
Return (KEPT . SKIPPED): KEPT in Canvas's order, SKIPPED an alist of
reason (see `org-canvas--quiz-results-skip-reason') to count."
  (let (kept skipped)
    (dolist (quiz quizzes)
      (let ((reason (org-canvas--quiz-results-skip-reason quiz)))
        (if reason
            (setq skipped (org-canvas--quiz-results-count-skip reason skipped))
          (push quiz kept))))
    (cons (nreverse kept) skipped)))

(defun org-canvas--quiz-results-fetch-statistics (quiz-id)
  "Return QUIZ-ID's statistics alist, nil when Canvas has none, or `refused'.
Canvas wraps the one report in a `quiz_statistics' array.  A refusal is
logged and answered with `refused', so one quiz Canvas will not report
on costs its heading and not the pull (the #171 rule)."
  (condition-case err
      (let ((reply (org-canvas-api-request
                    'GET (org-canvas-api-course-endpoint "quizzes/%s/statistics" quiz-id))))
        (car (append (alist-get 'quiz_statistics reply) nil)))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Quiz Results] Could not read the statistics of quiz %s (%s); skipped"
       quiz-id (error-message-string err))
     'refused)))

;;;; Folding Question Statistics Into Rows

(defun org-canvas--quiz-results-plain-text (html)
  "Return HTML as one line of plain text, cut to the table's width.
Tags go, the common entities are decoded, and runs of whitespace
collapse to one space."
  (let* ((text (replace-regexp-in-string "<[^>]*>" " " (or html "")))
         (text (replace-regexp-in-string "&nbsp;" " " text))
         (text (replace-regexp-in-string "&lt;" "<" text))
         (text (replace-regexp-in-string "&gt;" ">" text))
         (text (replace-regexp-in-string "&quot;" "\"" text))
         (text (replace-regexp-in-string "&#39;" "'" text))
         (text (replace-regexp-in-string "&amp;" "&" text))
         (text (string-trim (replace-regexp-in-string "[ \t\n\r|]+" " " text))))
    (if (> (length text) org-canvas--quiz-results-text-width)
        (concat (string-trim (substring text 0 (- org-canvas--quiz-results-text-width 3))) "...")
      text)))

(defun org-canvas--quiz-results-type-label (type)
  "Return the short label for the question TYPE Canvas names."
  (or (cdr (assoc type org-canvas--quiz-results-type-labels))
      (and type (replace-regexp-in-string
                 "_" " " (replace-regexp-in-string "_question\\'" "" type)))
      "-"))

(defun org-canvas--quiz-results-discrimination (question)
  "Return the correct answer's point biserial from QUESTION, or nil.
Canvas lists one point biserial per answer; the correct answer's is
the discrimination index.  The first one stands in when none is
marked correct."
  (let* ((all (append (alist-get 'point_biserials question) nil))
         (chosen (or (cl-find-if (lambda (pb) (eq (alist-get 'correct pb) t)) all)
                     (car all)))
         (value (and chosen (org-canvas--alist-get-non-null 'point_biserial chosen))))
    (and (numberp value) value)))

(defun org-canvas--quiz-results-responses (question)
  "Describe QUESTION's response count per answer, the correct one starred.
In answer order, as \"12, 40*, 3\"; - for a question without listed
answers.  Only the counts and the correct flag are read: the answers
also name the students who chose them, and those names go nowhere."
  (let ((answers (append (alist-get 'answers question) nil)))
    (if (null answers)
        "-"
      (mapconcat (lambda (answer)
                   (format "%s%s"
                           (or (org-canvas--alist-get-non-null 'responses answer) 0)
                           (if (eq (alist-get 'correct answer) t) "*" "")))
                 answers ", "))))

(defun org-canvas--quiz-results-answer-label (answer position)
  "Return ANSWER's text as one plain line, or #POSITION when it has none."
  (let* ((raw (org-canvas--alist-get-non-null 'text answer))
         (text (org-canvas--quiz-results-plain-text
                (and raw (format "%s" raw)))))
    (if (string-empty-p text) (format "#%d" position) text)))

(defun org-canvas--quiz-results-labelled-responses (question)
  "Describe QUESTION's response count per answer, each after its text.
In answer order, as \"Strongly agree 33; Agree 27\"; - for a question
without listed answers.  For a survey, where no answer is right and
the position alone says nothing.  Semicolons part the answers, since
an answer's text may hold a comma."
  (let ((answers (append (alist-get 'answers question) nil))
        (position 0))
    (if (null answers)
        "-"
      (mapconcat
       (lambda (answer)
         (setq position (1+ position))
         (format "%s %s"
                 (org-canvas--quiz-results-answer-label answer position)
                 (or (org-canvas--alist-get-non-null 'responses answer) 0)))
       answers "; "))))

(defun org-canvas--quiz-results-question-row (question &optional survey)
  "Return the table row plist for QUESTION's statistics.
Keys: :position, :text, :type, :answered, :correct (a percent, nil
when Canvas gives no ratio), :difficulty, :discrimination and
:responses.  With SURVEY non-nil the three scoring keys are nil and
:responses labels each count with its answer's text."
  (let ((ratio (org-canvas--alist-get-non-null 'correct_student_ratio question))
        (difficulty (org-canvas--alist-get-non-null 'difficulty_index question)))
    (list :position (alist-get 'position question)
          :text (org-canvas--quiz-results-plain-text (alist-get 'question_text question))
          :type (org-canvas--quiz-results-type-label (alist-get 'question_type question))
          :answered (or (org-canvas--alist-get-non-null 'answered_student_count question) 0)
          :correct (and (not survey) (numberp ratio) (* 100 ratio))
          :difficulty (and (not survey) (numberp difficulty) difficulty)
          :discrimination
          (and (not survey) (org-canvas--quiz-results-discrimination question))
          :responses (if survey
                         (org-canvas--quiz-results-labelled-responses question)
                       (org-canvas--quiz-results-responses question)))))

(defun org-canvas--quiz-results-duration (seconds)
  "Render SECONDS as h:mm, or nil when there is no number."
  (when (numberp seconds)
    (let ((minutes (round (/ seconds 60.0))))
      (format "%d:%02d" (/ minutes 60) (% minutes 60)))))

(defun org-canvas--quiz-results-entry (quiz stats)
  "Fold QUIZ and its STATS into one entry plist.
Keys: :id, :name, :survey, :students, :mean, :high, :low, :stdev,
:duration, :generated-at and :rows, one
`org-canvas--quiz-results-question-row' per question sorted by
position.  STATS nil means Canvas has no report; :students is then 0
and :rows nil.  :survey is non-nil for a survey, graded or not."
  (let* ((id (alist-get 'id quiz))
         (survey (and (org-canvas--quiz-results-survey-p quiz) t))
         (summary (alist-get 'submission_statistics stats))
         (students (or (org-canvas--alist-get-non-null 'unique_count summary) 0))
         (rows (mapcar (lambda (q)
                         (org-canvas--quiz-results-question-row q survey))
                       (append (alist-get 'question_statistics stats) nil))))
    (list :id id
          :name (or (alist-get 'title quiz) (format "Quiz %s" id))
          :survey survey
          :students students
          :mean (org-canvas--alist-get-non-null 'score_average summary)
          :high (org-canvas--alist-get-non-null 'score_high summary)
          :low (org-canvas--alist-get-non-null 'score_low summary)
          :stdev (org-canvas--alist-get-non-null 'score_stdev summary)
          :duration (org-canvas--quiz-results-duration
                     (org-canvas--alist-get-non-null 'duration_average summary))
          :generated-at (org-canvas--alist-get-non-null 'generated_at stats)
          :rows (if (zerop students)
                    nil
                  (sort rows (lambda (a b) (< (or (plist-get a :position) 0)
                                              (or (plist-get b :position) 0))))))))

;;;; Rendering

(defun org-canvas--quiz-results-number (value &optional decimals)
  "Render VALUE for a table cell with DECIMALS places, - when absent.
DECIMALS defaults to one; an integer keeps none."
  (cond ((null value) "-")
        ((integerp value) (format "%d" value))
        ((numberp value) (format (format "%%.%df" (or decimals 1)) value))
        (t (format "%s" value))))

(defun org-canvas--quiz-results-quiz-heading (quiz-id)
  "Return the quizzes.org heading carrying QUIZ-ID, or nil.
Nil as well when `org-canvas-quizzes-file' is unset or missing."
  (let ((file (and (boundp 'org-canvas-quizzes-file) org-canvas-quizzes-file)))
    (when (and file (file-exists-p file))
      (let ((target (format "%s" quiz-id)) (heading nil))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (when (and (not heading)
                          (equal (org-entry-get (point) "CANVAS_ID") target))
                 (setq heading (org-get-heading t t t t))))
             "LEVEL=1+CANVAS_ID={.}" 'file)))
        heading))))

(defun org-canvas--quiz-results-title (entry)
  "Return ENTRY's heading title: a link to its quizzes.org heading, else the name."
  (let ((heading (org-canvas--quiz-results-quiz-heading (plist-get entry :id)))
        (name (plist-get entry :name)))
    (if heading
        (org-link-make-string
         (format "file:%s::*%s" (file-name-nondirectory org-canvas-quizzes-file)
                 (replace-regexp-in-string "\\\\\\([][]\\)" "\\1" heading))
         name)
      name)))

(defun org-canvas--quiz-results-row-cells (row survey)
  "Return ROW's table cells as strings, without the scoring ones for a SURVEY."
  (append
   (list (org-canvas--quiz-results-number (plist-get row :position))
         (plist-get row :text)
         (plist-get row :type)
         (format "%d" (plist-get row :answered)))
   (unless survey
     (list (let ((correct (plist-get row :correct)))
             (if correct (format "%d%%" (round correct)) "-"))
           (org-canvas--quiz-results-number (plist-get row :difficulty) 2)
           (org-canvas--quiz-results-number (plist-get row :discrimination) 2)))
   (list (plist-get row :responses))))

(defun org-canvas--quiz-results-insert-table (rows &optional survey)
  "Insert the question table for ROWS at point and align it.
With SURVEY non-nil the Correct, Difficulty and Discrimination
columns are left out: a survey has no right answer to measure."
  (let ((start (point))
        (header (append '("#" "Question" "Type" "Answered")
                        (unless survey
                          '("Correct" "Difficulty" "Discrimination"))
                        '("Responses"))))
    (insert "| " (mapconcat #'identity header " | ") " |\n|-\n")
    (dolist (row rows)
      (insert "| " (mapconcat #'identity
                              (org-canvas--quiz-results-row-cells row survey)
                              " | ")
              " |\n"))
    (save-excursion
      (goto-char start)
      (org-table-align))))

(defun org-canvas--quiz-results-properties (entry)
  "Return ENTRY's drawer as (PROPERTY . VALUE) pairs, in order.
A survey's has no score properties: an ungraded survey has no score
and a graded one gives every taker full marks."
  (let ((score (unless (plist-get entry :survey)
                 (mapcar (lambda (prop)
                           (cons (car prop)
                                 (org-canvas--quiz-results-number
                                  (plist-get entry (cdr prop)))))
                         '(("MEAN" . :mean) ("HIGH" . :high)
                           ("LOW" . :low) ("STDEV" . :stdev))))))
    (append `(("QUIZ_ID" . ,(plist-get entry :id))
              ("STUDENTS" . ,(plist-get entry :students)))
            score
            `(("DURATION" . ,(plist-get entry :duration))
              ("GENERATED_AT" . ,(org-canvas--iso8601-to-org-timestamp
                                  (plist-get entry :generated-at)))))))

(defun org-canvas--quiz-results-insert-entry (entry)
  "Insert ENTRY as a level-1 heading with its properties and table at point."
  (insert (format "* %s\n" (org-canvas--quiz-results-title entry)))
  (insert ":PROPERTIES:\n")
  (dolist (prop (org-canvas--quiz-results-properties entry))
    (when (and (cdr prop) (not (equal (cdr prop) "-")))
      (insert (format ":%s: %s\n" (car prop) (cdr prop)))))
  (insert ":END:\n\n")
  (if (null (plist-get entry :rows))
      (insert "No attempts yet.\n\n")
    (org-canvas--quiz-results-insert-table (plist-get entry :rows)
                                           (plist-get entry :survey))
    (insert "\n")))

;;;; Pull

(defun org-canvas--quiz-results-entries (quizzes)
  "Fetch the statistics of QUIZZES and fold each into an entry plist.
A quiz whose statistics Canvas refuses is left out."
  (let (entries)
    (dolist (quiz quizzes)
      (let ((stats (org-canvas--quiz-results-fetch-statistics (alist-get 'id quiz))))
        (unless (eq stats 'refused)
          (push (org-canvas--quiz-results-entry quiz stats) entries))))
    (nreverse entries)))

;;;###autoload
(defun org-canvas-pull-quiz-results ()
  "Pull the item analysis of every classic quiz into quiz-results.org.
One heading per published quiz with the score spread in its
properties and a table of its questions: answered, correct, difficulty,
discrimination and the responses per answer.  A survey gets its
response counts by answer text and no scoring.  The closing message
counts the quizzes left out and why.  Read-only, and the file is
derived: every pull rewrites it whole.  Only aggregates are written,
but keep the file out of a course repository all the same."
  (interactive)
  (org-canvas--start-operation "PULLING QUIZ RESULTS")
  (let* ((file (expand-file-name org-canvas-quiz-results-file))
         (split (org-canvas--quiz-results-partition
                 (org-canvas--quiz-results-fetch-quizzes)))
         (quizzes (car split))
         (skipped (cdr split))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "quiz-results")
    (if (null quizzes)
        (progn
          (org-canvas--pull-emit-empty-file
           file (org-canvas--pull-label-for "quiz-results"))
          (org-canvas--quiz-results-report nil skipped))
      (let ((entries (org-canvas--quiz-results-entries quizzes)))
        (org-canvas--quiz-results-write file entries)
        (org-canvas--pull-kill-fresh-buffer file was-fresh)
        (dotimes (_ (- (length quizzes) (length entries)))
          (setq skipped (org-canvas--quiz-results-count-skip 'refused skipped)))
        (org-canvas--quiz-results-report entries skipped)))))

(defun org-canvas--quiz-results-write (file entries)
  "Rewrite FILE whole with one heading per entry of ENTRIES."
  (unless (file-exists-p file)
    (with-temp-file file (insert "")))
  (with-current-buffer (org-canvas--find-file-noselect file)
    (erase-buffer)
    (insert (format "#+TITLE: %s\n\n"
                    (org-canvas--pull-label-for "quiz-results")))
    (dolist (entry entries)
      (org-canvas--quiz-results-insert-entry entry))
    (org-canvas--pull-write-file-header)
    (org-canvas--save-buffer)))

(defun org-canvas--quiz-results-skip-note (skipped)
  "Describe SKIPPED, an alist of reason to count, for the closing line.
Empty when nothing was skipped, else as \"; skipped 3 (2 unpublished,
1 without an assignment)\"."
  (if (null skipped)
      ""
    (format "; skipped %d (%s)"
            (apply #'+ (mapcar #'cdr skipped))
            (mapconcat
             (lambda (cell)
               (let ((label (alist-get (car cell)
                                       org-canvas--quiz-results-skip-labels)))
                 (format "%d %s" (cdr cell) label)))
             skipped ", "))))

(defun org-canvas--quiz-results-report (entries skipped)
  "Log and show the closing line for ENTRIES written and SKIPPED left out."
  (let ((line (format
               "Quiz results pull complete: %d quizzes, %d with attempts%s"
                      (length entries)
                      (cl-count-if (lambda (e) (plist-get e :rows)) entries)
                      (org-canvas--quiz-results-skip-note skipped))))
    (org-canvas--log-info org-canvas--logger "%s" line)
    (message "%s." line)))

(provide 'org-canvas-quiz-results)
;;; org-canvas-quiz-results.el ends here
