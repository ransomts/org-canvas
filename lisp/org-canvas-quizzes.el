;;; org-canvas-quizzes.el --- Quiz Sync Pipeline for Canvas LMS -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the sync pipeline for Canvas Quizzes.
;;
;; FILE STRUCTURE
;; ==============
;; In quizzes.org:
;;   - Level 1 headings = Quizzes (with QUIZ_TYPE, TIME_LIMIT, etc.)
;;   - Level 2 headings = Questions (with TYPE, POINTS)
;;   - List items under questions = Answer choices
;;
;; QUESTION TYPES
;; ==============
;; Each question type has a specific answer format:
;;
;;   multiple_choice_question    - [X] marks correct, [ ] marks wrong
;;   true_false_question         - [X] True or [X] False
;;   short_answer_question       - [X] marks each acceptable answer
;;   fill_in_multiple_blanks     - Nested lists: - blank_id / - [X] answer
;;   multiple_dropdowns          - Same as fill_in_multiple_blanks
;;   matching_question           - left = right (no = means distractor)
;;   numerical_question          - [X] 42 or [X] [10, 20] for range
;;   essay_question              - No answers needed
;;   file_upload_question        - No answers needed
;;
;; API NOTES
;; =========
;; Quizzes and questions are separate API resources.  The quiz description
;; is just introductory text.  Actual questions are synced via:
;;   POST /api/v1/courses/:course_id/quizzes/:quiz_id/questions
;;
;; The sync process:
;;   1. Create/update the quiz
;;   2. For each level-2 heading under the quiz, sync the question

;;; Code:

(require 'org-canvas-core)
(require 'ox-html)
(require 'elog)

;;;; Configuration

(defcustom org-canvas-quizzes-file (org-canvas--path "quizzes.org")
  "Path to the quizzes.org file."
  :type 'file
  :group 'org-canvas)

;;;; Helper Functions

(defun org-canvas--quiz-parse-body-text ()
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
      ;; Check if there's a subheading before end of subtree
      (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
	(when (> end subtree-end)
	  (setq end subtree-end)))
      (string-trim (buffer-substring-no-properties start end)))))

(defun org-canvas--quiz-parse-question-text ()
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
      ;; Check if there's a subheading before end of subtree
      (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
	(when (> end subtree-end)
	  (setq end subtree-end)))
      ;; Find the first list item and stop there
      (goto-char start)
      (when (re-search-forward "^[ \t]*[-*+] " end t)
	(setq end (match-beginning 0)))
      (string-trim (buffer-substring-no-properties start end)))))

(defun org-canvas--quiz-parse-checkbox-list ()
  "Parse a checkbox list under point, returning answer data.
Returns a list of (text . is-correct) pairs."
  (save-excursion
    (let ((answers nil)
	  (bound (save-excursion (org-end-of-subtree t) (point))))
      ;; Find all checkbox items
      (while (re-search-forward "^[ \t]*- \\(\\[[ X]\\]\\) \\(.+\\)$" bound t)
	(let ((checkbox (match-string 1))
	      (text (string-trim (match-string 2))))
	  (push (cons text (string= checkbox "[X]")) answers)))
      (nreverse answers))))

(defun org-canvas--quiz-parse-nested-blanks ()
  "Parse nested blank answers for fill_in_multiple_blanks or multiple_dropdowns.
Format:
- blank_id
  - [X] correct answer
  - [ ] wrong answer
Returns alist of (blank_id . ((text . is-correct) ...))."
  (save-excursion
    (let ((blanks nil)
	  (bound (save-excursion (org-end-of-subtree t) (point))))
      ;; Find top-level list items (blank IDs)
      (while (re-search-forward "^- \\([a-zA-Z0-9_]+\\)$" bound t)
	(let ((blank-id (match-string 1))
	      (answers nil)
	      (blank-bound (save-excursion
			     (if (re-search-forward "^- [a-zA-Z0-9_]+$" bound t)
				 (match-beginning 0)
			       bound))))
	  ;; Find checkbox items under this blank
	  (while (re-search-forward "^  - \\(\\[[ X]\\]\\) \\(.+\\)$" blank-bound t)
	    (let ((checkbox (match-string 1))
		  (text (string-trim (match-string 2))))
	      (push (cons text (string= checkbox "[X]")) answers)))
	  (push (cons blank-id (nreverse answers)) blanks)))
      (nreverse blanks))))

(defun org-canvas--quiz-parse-matching-list ()
  "Parse matching question format: left = right.
Returns list of (left . right) pairs.  Last item with no match is distractor."
  (save-excursion
    (let ((matches nil)
	  (bound (save-excursion (org-end-of-subtree t) (point))))
      (while (re-search-forward "^- \\(.+?\\) = \\(.+\\)$" bound t)
	(push (cons (string-trim (match-string 1))
		    (string-trim (match-string 2)))
	      matches))
      ;; Also check for distractors (items without =)
      (goto-char (point))
      (while (re-search-forward "^- \\([^=\n]+\\)$" bound t)
	(let ((text (string-trim (match-string 1))))
	  (unless (string-match-p "\\[[ X]\\]" text)  ; Skip checkboxes
	    (push (cons text nil) matches))))
      (nreverse matches))))

(defun org-canvas--quiz-parse-numerical-answer ()
  "Parse numerical answer format.
Supports: exact value, or [min, max] range."
  (save-excursion
    (let ((bound (save-excursion (org-end-of-subtree t) (point))))
      (when (re-search-forward "^- \\[X\\] \\(.+\\)$" bound t)
	(let ((text (string-trim (match-string 1))))
	  (if (string-match "\\[\\([0-9.-]+\\),[ ]*\\([0-9.-]+\\)\\]" text)
	      ;; Range answer
	      (list :type 'range
		    :start (string-to-number (match-string 1 text))
		    :end (string-to-number (match-string 2 text)))
	    ;; Exact answer
	    (list :type 'exact
		  :value (string-to-number text))))))))

;;;; Quiz Parsing (Level 1)

(defun org-canvas--quiz-parse-entry ()
  "Extract quiz data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Quiz Parse] Starting at point %d" (point))

  (let* ((pom (point-marker))
	 (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
	 (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
	 (quiz-type (org-canvas--validate-property
		    (org-canvas-org-get-property pom "QUIZ_TYPE")
		    '("assignment" "practice_quiz" "graded_survey" "survey")
		    "QUIZ_TYPE" "assignment"))
	 (time-limit (org-canvas-org-get-property pom "TIME_LIMIT"))
	 (published (org-canvas-org-get-boolean-property pom "PUBLISHED" t))
	 (shuffle (org-canvas-org-get-boolean-property pom "SHUFFLE_ANSWERS"))
	 (attempts (org-canvas-org-get-property pom "ALLOWED_ATTEMPTS"))
	 (due-at (org-canvas-org-get-property pom "DUE_AT"))
	 (group-link (org-canvas-org-get-property pom "GROUP"))
	 (assignment-group-id (org-canvas--resolve-link-property
			       group-link "CANVAS_ID" org-canvas-quizzes-file))
	 (unlock-at (org-canvas-org-get-property pom "UNLOCK_AT"))
	 (lock-at (org-canvas-org-get-property pom "LOCK_AT"))
	 (access-code (org-canvas-org-get-property pom "ACCESS_CODE"))
	 (show-correct (org-canvas-org-get-boolean-property pom "SHOW_CORRECT_ANSWERS" t))
	 (show-correct-at (org-canvas-org-get-property pom "SHOW_CORRECT_ANSWERS_AT"))
	 (hide-correct-at (org-canvas-org-get-property pom "HIDE_CORRECT_ANSWERS_AT"))
	 (hide-results (org-canvas--validate-property
			(org-canvas-org-get-property pom "HIDE_RESULTS")
			'("always" "until_after_last_attempt")
			"HIDE_RESULTS" nil))
	 (scoring-policy (org-canvas--validate-property
			  (org-canvas-org-get-property pom "SCORING_POLICY")
			  '("keep_highest" "keep_latest")
			  "SCORING_POLICY" nil))
	 (one-question (org-canvas-org-get-boolean-property pom "ONE_QUESTION_AT_A_TIME"))
	 (cant-go-back (org-canvas-org-get-boolean-property pom "CANT_GO_BACK"))
	 (ip-filter (org-canvas-org-get-property pom "IP_FILTER"))
	 (body-text (org-canvas--quiz-parse-body-text)))

    (when (or (null title) (string-empty-p title))
      (error "Quiz title cannot be empty at point %d" (marker-position pom)))

    (elog-info org-canvas--logger "[Quiz Parse] Quiz: '%s' (ID: %s)" title (or canvas-id "NEW"))
    (when assignment-group-id
      (elog-debug org-canvas--logger "[Quiz Parse] Assignment Group ID: %s" assignment-group-id))

    (list :title title
	  :description (when (> (length body-text) 0)
			 (let ((org-export-with-sub-superscripts nil))
			   (org-export-string-as body-text 'html t)))
	  :canvas-id canvas-id
	  :published published
	  :quiz_type quiz-type
	  :time_limit (when time-limit (org-canvas--safe-string-to-number time-limit "TIME_LIMIT"))
	  :shuffle_answers shuffle
	  :allowed_attempts (when attempts (org-canvas--safe-string-to-number attempts "ALLOWED_ATTEMPTS"))
	  :due_at (when due-at (org-canvas-org-parse-timestamp due-at))
	  :unlock_at (when unlock-at (org-canvas-org-parse-timestamp unlock-at))
	  :lock_at (when lock-at (org-canvas-org-parse-timestamp lock-at))
	  :access_code access-code
	  :show_correct_answers show-correct
	  :show_correct_answers_at (when show-correct-at (org-canvas-org-parse-timestamp show-correct-at))
	  :hide_correct_answers_at (when hide-correct-at (org-canvas-org-parse-timestamp hide-correct-at))
	  :hide_results hide-results
	  :scoring_policy scoring-policy
	  :one_question_at_a_time one-question
	  :cant_go_back cant-go-back
	  :ip_filter ip-filter
	  :assignment_group_id (when assignment-group-id (string-to-number assignment-group-id))
	  :pom pom)))

(defun org-canvas--quiz-build-payload (data)
  "Convert quiz DATA to Canvas payload."
  (let ((quiz-obj `((title . ,(plist-get data :title))
		    (quiz_type . ,(plist-get data :quiz_type))
		    (published . ,(org-canvas--to-json-boolean (plist-get data :published))))))

    (when-let ((desc (plist-get data :description)))
      (push `(description . ,desc) quiz-obj))

    (when-let ((limit (plist-get data :time_limit)))
      (push `(time_limit . ,limit) quiz-obj))

    (when (plist-get data :shuffle_answers)
      (push `(shuffle_answers . t) quiz-obj))

    (when-let ((attempts (plist-get data :allowed_attempts)))
      (push `(allowed_attempts . ,attempts) quiz-obj))

    (when-let ((due (plist-get data :due_at)))
      (push `(due_at . ,due) quiz-obj))

    (when-let ((group-id (plist-get data :assignment_group_id)))
      (push `(assignment_group_id . ,group-id) quiz-obj))

    (when-let ((unlock (plist-get data :unlock_at)))
      (push `(unlock_at . ,unlock) quiz-obj))

    (when-let ((lock (plist-get data :lock_at)))
      (push `(lock_at . ,lock) quiz-obj))

    (when-let ((code (plist-get data :access_code)))
      (push `(access_code . ,code) quiz-obj))

    ;; Always send show_correct_answers (meaningful default of true)
    (push `(show_correct_answers . ,(org-canvas--to-json-boolean
				     (plist-get data :show_correct_answers)))
	  quiz-obj)

    (when-let ((show-at (plist-get data :show_correct_answers_at)))
      (push `(show_correct_answers_at . ,show-at) quiz-obj))

    (when-let ((hide-at (plist-get data :hide_correct_answers_at)))
      (push `(hide_correct_answers_at . ,hide-at) quiz-obj))

    (when-let ((hide (plist-get data :hide_results)))
      (push `(hide_results . ,hide) quiz-obj))

    (when-let ((scoring (plist-get data :scoring_policy)))
      (push `(scoring_policy . ,scoring) quiz-obj))

    (when (plist-get data :one_question_at_a_time)
      (push `(one_question_at_a_time . t) quiz-obj))

    (when (plist-get data :cant_go_back)
      (push `(cant_go_back . t) quiz-obj))

    (when-let ((ip (plist-get data :ip_filter)))
      (push `(ip_filter . ,ip) quiz-obj))

    `((quiz . ,quiz-obj))))

(defun org-canvas--quiz-push-to-api (data payload)
  "Send quiz PAYLOAD (from DATA) to Canvas API.  Return response with quiz ID."
  (org-canvas--push-to-api data payload :endpoint "quizzes"))

(defun org-canvas--quiz-verify-response (data response)
  "Verify quiz properties in RESPONSE match DATA."
  (let ((expected-group (plist-get data :assignment_group_id))
        (actual-group (alist-get 'assignment_group_id response)))
    (when (and expected-group actual-group
               (not (equal expected-group actual-group)))
      (elog-warning org-canvas--logger
        "[Verify] '%s': assignment_group_id mismatch! Expected %s, got %s"
        (plist-get data :title) expected-group actual-group))))

(defun org-canvas--quiz-finalize (data response)
  "Save quiz pointed to in DATA with CANVAS_ID from RESPONSE to org heading."
  (org-canvas--finalize-item data response
    :post-fn #'org-canvas--quiz-verify-response))

;;;; Question Parsing (Level 2)

(defun org-canvas--question-parse-entry (quiz-canvas-id)
  "Extract question data from Org heading at point.
QUIZ-CANVAS-ID is the Canvas ID of the parent quiz."
  (org-back-to-heading t)

  (let* ((pom (point-marker))
	 (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
	 (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
	 (q-type (org-canvas--validate-property
		  (org-canvas-org-get-property pom "TYPE")
		  '("multiple_choice_question" "true_false_question"
		    "short_answer_question" "fill_in_multiple_blanks_question"
		    "multiple_dropdowns_question" "multiple_answers_question"
		    "matching_question" "numerical_question" "essay_question"
		    "file_upload_question" "text_only_question")
		  "TYPE" "multiple_choice_question"))
	 (points (org-canvas-org-get-number-property pom "POINTS" 1))
	 (body-text (org-canvas--quiz-parse-question-text)))

    (elog-debug org-canvas--logger "[Question Parse] '%s' type=%s" title q-type)

    (list :name title
	  :text body-text
	  :canvas-id canvas-id
	  :quiz-canvas-id quiz-canvas-id
	  :question_type q-type
	  :points_possible points
	  :pom pom)))

(defun org-canvas--question-build-checkbox-answers ()
  "Build answers from checkbox list where all items get weights."
  (let ((answers (org-canvas--quiz-parse-checkbox-list)))
    (cl-loop for (text . correct) in answers
             collect `((answer_text . ,text)
                       (answer_weight . ,(if correct
                                             org-canvas--answer-weight-correct
                                           org-canvas--answer-weight-incorrect))))))

(defun org-canvas--question-build-short-answers ()
  "Build answers from checkbox list, keeping only correct items."
  (let ((answers (org-canvas--quiz-parse-checkbox-list)))
    (cl-loop for (text . correct) in answers
             when correct
             collect `((answer_text . ,text)
                       (answer_weight . ,org-canvas--answer-weight-correct)))))

(defun org-canvas--question-build-blank-answers (correct-only)
  "Build answers from nested blanks.
When CORRECT-ONLY is non-nil, only include correct answers."
  (let ((blanks (org-canvas--quiz-parse-nested-blanks)))
    (cl-loop for (blank-id . answers) in blanks
             append (cl-loop for (text . correct) in answers
                             when (or (not correct-only) correct)
                             collect `((blank_id . ,blank-id)
                                       (answer_text . ,text)
                                       (answer_weight . ,(if correct
                                                             org-canvas--answer-weight-correct
                                                           org-canvas--answer-weight-incorrect)))))))

(defun org-canvas--question-build-matching-answers ()
  "Build answers from matching pairs list."
  (let ((matches (org-canvas--quiz-parse-matching-list)))
    (cl-loop for (left . right) in matches
             collect (if right
                        `((answer_match_left . ,left)
                          (answer_match_right . ,right))
                      `((answer_match_left . ,left))))))

(defun org-canvas--question-build-numerical-answer ()
  "Build answer from numerical value or range."
  (let ((num (org-canvas--quiz-parse-numerical-answer)))
    (when num
      (if (eq (plist-get num :type) 'range)
          `(((numerical_answer_type . "range_answer")
             (answer_range_start . ,(plist-get num :start))
             (answer_range_end . ,(plist-get num :end))
             (answer_weight . ,org-canvas--answer-weight-correct)))
        `(((numerical_answer_type . "exact_answer")
           (answer_exact . ,(plist-get num :value))
           (answer_weight . ,org-canvas--answer-weight-correct)))))))

(defun org-canvas--question-build-answers (q-type)
  "Build answers array based on Q-TYPE from current heading content."
  (pcase q-type
    ((or "multiple_choice_question" "true_false_question" "multiple_answers_question")
     (org-canvas--question-build-checkbox-answers))
    ("short_answer_question"
     (org-canvas--question-build-short-answers))
    ("fill_in_multiple_blanks_question"
     (org-canvas--question-build-blank-answers t))
    ("multiple_dropdowns_question"
     (org-canvas--question-build-blank-answers nil))
    ("matching_question"
     (org-canvas--question-build-matching-answers))
    ("numerical_question"
     (org-canvas--question-build-numerical-answer))
    ((or "essay_question" "file_upload_question" "text_only_question")
     nil)
    (_ nil)))

(defun org-canvas--question-build-payload (data)
  "Build Canvas API payload from question DATA."
  (let* ((q-type (plist-get data :question_type))
	 (text-html (let ((org-export-with-sub-superscripts nil))
		     (org-export-string-as (plist-get data :text) 'html t)))
	 (answers (save-excursion
		    (goto-char (marker-position (plist-get data :pom)))
		    (org-canvas--question-build-answers q-type)))
	 (question-obj `((question_name . ,(plist-get data :name))
			 (question_text . ,text-html)
			 (question_type . ,q-type)
			 (points_possible . ,(plist-get data :points_possible)))))

    (when answers
      (push `(answers . ,(vconcat answers)) question-obj))

    `((question . ,question-obj))))

(defun org-canvas--question-push-to-api (data payload)
  "Send question PAYLOAD (from DATA) to Canvas API."
  (let* ((quiz-id (plist-get data :quiz-canvas-id))
	 (q-id (plist-get data :canvas-id))
	 (name (plist-get data :name))
	 (method (if q-id 'PUT 'POST))
	 (endpoint (if q-id
		       (org-canvas-api-course-endpoint "quizzes/%s/questions/%s" quiz-id q-id)
		     (org-canvas-api-course-endpoint "quizzes/%s/questions" quiz-id))))

    (elog-info org-canvas--logger "[Question API] %s '%s'" method name)

    (condition-case err
	(org-canvas-api-request method endpoint :data payload)
      (error
       (elog-error org-canvas--logger "[Question API] Failed: %s" (error-message-string err))
       (signal (car err) (cdr err))))))

(defun org-canvas--question-finalize (data response)
  "Save question from DATA with CANVAS_ID from RESPONSE."
  (org-canvas--finalize-item data response :title-key :name))

;;;; Main Sync Function

(defun org-canvas--sync-quiz-questions (quiz-marker quiz-canvas-id)
  "Sync all questions under the quiz at QUIZ-MARKER.
QUIZ-CANVAS-ID is the Canvas ID of the quiz."
  (let ((question-markers nil)
	(question-success 0))
    ;; First, collect all question markers (level-2 headings under this quiz)
    (with-current-buffer (marker-buffer quiz-marker)
      (save-excursion
	(goto-char (marker-position quiz-marker))
	(let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
	  (while (and (outline-next-heading)
		      (< (point) subtree-end))
	    (when (= (org-outline-level) 2)
	      (push (point-marker) question-markers)))))
      (setq question-markers (nreverse question-markers)))

    ;; Now sync each question using the stable markers
    (dolist (q-marker question-markers)
      (with-current-buffer (marker-buffer q-marker)
	(save-excursion
	  (goto-char (marker-position q-marker))
	  (condition-case err
	      (let* ((data (org-canvas--question-parse-entry quiz-canvas-id))
		     (payload (org-canvas--question-build-payload data))
		     (response (org-canvas--question-push-to-api data payload)))
		(org-canvas--question-finalize data response)
		(setq question-success (1+ question-success)))
	    (error
	     (elog-error org-canvas--logger "[Question] Failed: %s"
	       (error-message-string err)))))))

    (elog-info org-canvas--logger "[Questions] %d/%d synced"
      question-success (length question-markers))
    (cons question-success (- (length question-markers) question-success))))

;;;###autoload
(defun org-canvas-sync-quizzes ()
  "Synchronize quizzes and their questions."
  (interactive)
  (org-canvas-clear-log)
  (unless (and org-canvas-quizzes-file (file-exists-p org-canvas-quizzes-file))
    (error "Quizzes file not found: %s" org-canvas-quizzes-file))

  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> STARTING QUIZ SYNC")
  (elog-info org-canvas--logger "File: %s" org-canvas-quizzes-file)
  (elog-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
  (elog-info org-canvas--logger "========================================")

  (let ((quiz-targets nil)
	(quiz-success 0)
	(quiz-fail 0)
	(question-success 0)
	(question-fail 0))

    ;; Collect all level-1 headings (quizzes)
    (with-current-buffer (find-file-noselect org-canvas-quizzes-file)
      (setq quiz-targets (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))

    (elog-info org-canvas--logger "Found %d quizzes to sync" (length quiz-targets))

    ;; Sync each quiz and its questions
    (dolist (marker quiz-targets)
      (elog-info org-canvas--logger "----------------------------------------")
      (with-current-buffer (marker-buffer marker)
	(save-excursion
	  (goto-char (marker-position marker))
	  (condition-case err
	      (let* ((data (org-canvas--quiz-parse-entry))
		     (payload (org-canvas--quiz-build-payload data))
		     (response (org-canvas--quiz-push-to-api data payload)))
		(org-canvas--quiz-finalize data response)
		(setq quiz-success (1+ quiz-success))
		(message "Quizzes [%d/%d] Synced '%s'"
		  (+ quiz-success quiz-fail) (length quiz-targets)
		  (plist-get data :title))

		;; Now sync questions under this quiz
		(let ((quiz-id (or (alist-get 'id response)
				   (plist-get data :canvas-id))))
		  (when quiz-id
		    (let ((q-results (org-canvas--sync-quiz-questions marker quiz-id)))
		      (setq question-success (+ question-success (car q-results)))
		      (setq question-fail (+ question-fail (cdr q-results)))))))
	    (error
	     (setq quiz-fail (1+ quiz-fail))
	     (elog-error org-canvas--logger "[FAILED] Quiz at point %d: %s"
	       (marker-position marker) (error-message-string err))
	     (message "Quizzes [%d/%d] FAILED: %s"
	       (+ quiz-success quiz-fail) (length quiz-targets)
	       (error-message-string err)))))))

    ;; Save the org file
    (with-current-buffer (find-file-noselect org-canvas-quizzes-file)
      (save-buffer)
      (elog-info org-canvas--logger "Saved %s" org-canvas-quizzes-file))

    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> QUIZ SYNC COMPLETE")
    (elog-info org-canvas--logger "Quizzes: %d success, %d failed" quiz-success quiz-fail)
    (elog-info org-canvas--logger "Questions: %d success, %d failed" question-success question-fail)
    (elog-info org-canvas--logger "========================================")
    (message "Quiz sync: %d/%d quizzes, %d/%d questions."
	     quiz-success (+ quiz-success quiz-fail)
	     question-success (+ question-success question-fail))))

;;;; Delete Functions

(org-canvas-define-delete-all quizzes
  :endpoint "quizzes"
  :file org-canvas-quizzes-file)

;;;; Pull

(defun org-canvas--quiz-pull-set-properties (pos quiz file)
  "Set all properties on heading at POS from QUIZ API response.
FILE is the quizzes.org path, used for group link resolution."
  (let ((quiz-type (alist-get 'quiz_type quiz))
        (time-limit (alist-get 'time_limit quiz))
        (shuffle (alist-get 'shuffle_answers quiz))
        (group-id (alist-get 'assignment_group_id quiz)))
    (org-canvas-org-save-sync-state pos (alist-get 'id quiz))
    (when quiz-type
      (org-canvas-org-set-property pos "QUIZ_TYPE" quiz-type))
    (when time-limit
      (org-canvas-org-set-property pos "TIME_LIMIT" (format "%s" time-limit)))
    (when shuffle
      (org-canvas--pull-set-boolean-property pos "SHUFFLE_ANSWERS" shuffle))
    (org-canvas--pull-set-timestamp-property pos "DUE_AT" (alist-get 'due_at quiz))
    (org-canvas--pull-set-timestamp-property pos "UNLOCK_AT" (alist-get 'unlock_at quiz))
    (org-canvas--pull-set-timestamp-property pos "LOCK_AT" (alist-get 'lock_at quiz))
    (let ((access-code (org-canvas--alist-get-non-null 'access_code quiz))
          (show-correct (alist-get 'show_correct_answers quiz))
          (hide-results (org-canvas--alist-get-non-null 'hide_results quiz))
          (scoring (org-canvas--alist-get-non-null 'scoring_policy quiz))
          (one-q (alist-get 'one_question_at_a_time quiz))
          (cant-back (alist-get 'cant_go_back quiz))
          (ip (org-canvas--alist-get-non-null 'ip_filter quiz)))
      (when access-code
        (org-canvas-org-set-property pos "ACCESS_CODE" access-code))
      (when (not (eq show-correct :null))
        (org-canvas--pull-set-boolean-property pos "SHOW_CORRECT_ANSWERS" show-correct))
      (org-canvas--pull-set-timestamp-property pos "SHOW_CORRECT_ANSWERS_AT"
                                               (alist-get 'show_correct_answers_at quiz))
      (org-canvas--pull-set-timestamp-property pos "HIDE_CORRECT_ANSWERS_AT"
                                               (alist-get 'hide_correct_answers_at quiz))
      (when hide-results
        (org-canvas-org-set-property pos "HIDE_RESULTS" hide-results))
      (when scoring
        (org-canvas-org-set-property pos "SCORING_POLICY" scoring))
      (when one-q
        (org-canvas--pull-set-boolean-property pos "ONE_QUESTION_AT_A_TIME" one-q))
      (when cant-back
        (org-canvas--pull-set-boolean-property pos "CANT_GO_BACK" cant-back))
      (when ip
        (org-canvas-org-set-property pos "IP_FILTER" ip)))
    (when (and group-id (fboundp 'org-canvas--assignment-resolve-group-link))
      (let ((group-link (org-canvas--assignment-resolve-group-link group-id)))
        (when group-link
          (org-canvas-org-set-property pos "GROUP" group-link))))))

(defun org-canvas--quiz-insert-question-body (q-text)
  "Insert question body text Q-TEXT (HTML) at point as Org markup."
  (when (and q-text (not (string-empty-p q-text)))
    (insert "\n" (org-canvas--html-to-org q-text) "\n")))

(defun org-canvas--quiz-insert-answers (answers)
  "Insert ANSWERS list at point as Org checklist items.
Each answer's weight determines checked ([X]) vs unchecked ([ ])."
  (when answers
    (dolist (a (append answers nil))
      (let ((text (or (alist-get 'text a) (alist-get 'html a) ""))
            (weight (alist-get 'weight a)))
        (insert (format "- [%s] %s\n"
                        (if (and weight (> weight 0)) "X" " ")
                        text))))))

(defun org-canvas--quiz-pull-insert-question (q)
  "Insert a single question Q as an L2 heading under the current quiz.
Point must be at the parent quiz heading."
  (let ((q-name (or (alist-get 'question_name q) "Question"))
        (q-text (alist-get 'question_text q))
        (q-type (alist-get 'question_type q))
        (q-points (alist-get 'points_possible q))
        (answers (alist-get 'answers q)))
    (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
      (goto-char subtree-end)
      (unless (bolp) (insert "\n"))
      (insert (format "** %s\n" q-name))
      (org-back-to-heading t)
      (let ((qpos (point)))
        (when q-type
          (org-canvas-org-set-property qpos "QUESTION_TYPE" q-type))
        (when q-points
          (org-canvas-org-set-property qpos "POINTS" (format "%s" q-points)))
        (goto-char (save-excursion (org-end-of-meta-data t) (point)))
        (org-canvas--quiz-insert-question-body q-text)
        (org-canvas--quiz-insert-answers answers)))))

(defun org-canvas--quiz-pull-insert-questions (quiz-id)
  "Fetch and insert questions for QUIZ-ID as L2 headings.
Point must be at the parent quiz heading."
  (condition-case nil
      (let* ((q-url (org-canvas-api-course-endpoint
                     "quizzes/%s/questions" quiz-id))
             (questions (org-canvas-api-request-all-pages 'GET q-url)))
        (dolist (q questions)
          (org-canvas--quiz-pull-insert-question q)))
    (error nil)))

;;;###autoload
(defun org-canvas-pull-quizzes ()
  "Pull quizzes from Canvas into quizzes.org."
  (interactive)
  (org-canvas--start-operation "PULLING QUIZZES")
  (let* ((file (expand-file-name org-canvas-quizzes-file))
         (endpoint (org-canvas-api-course-endpoint "quizzes"))
         (remote (org-canvas-api-request-all-pages 'GET endpoint))
         (count 0))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (find-file-noselect file)
      (dolist (quiz remote)
        (let* ((id (alist-get 'id quiz))
               (title (alist-get 'title quiz))
               (pos (org-canvas--pull-upsert-heading file id title)))
          (goto-char pos)
          (when title (org-edit-headline title))
          (org-canvas--quiz-pull-set-properties pos quiz file)
          (org-canvas--pull-insert-body (alist-get 'description quiz))
          (org-canvas--quiz-pull-insert-questions id)
          (cl-incf count)))
      (save-buffer))
    (elog-info org-canvas--logger "Quizzes pull complete: %d quizzes" count)
    (message "Quizzes pull complete: %d quizzes." count)))

(provide 'org-canvas-quizzes)
;;; org-canvas-quizzes.el ends here
