;;; org-canvas-quizzes.el --- Quiz Sync Pipeline for Canvas LMS -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas Quizzes.
;;
;; FILE STRUCTURE
;; ==============
;; In quizzes.org:
;;   - Level 1 headings = Quizzes (with QUIZ_TYPE, TIME_LIMIT, etc.)
;;   - Level 2 headings = Questions (with TYPE, POINTS)
;;   - Level 2 headings with TYPE=group = Question groups (question banks)
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
;; QUESTION GROUPS (QUESTION BANKS)
;; ================================
;; Level 2 headings with TYPE=group create question groups that pull
;; random questions from external question banks.  Properties:
;;   PICK_COUNT         - Number of questions to randomly select
;;   QUESTION_POINTS    - Points per selected question
;;   QUESTION_BANK_ID   - Canvas assessment question bank ID (create-only)
;;
;; The bank must be created externally (e.g., via text2qti QTI import).
;; The bank ID is obtained from the Canvas UI URL after import.
;; Note: assessment_question_bank_id is accepted on POST but ignored on PUT.
;;
;; API NOTES
;; =========
;; Quizzes, questions, and question groups are separate API resources.
;; The quiz description is just introductory text.  Actual questions are
;; synced via:
;;   POST /api/v1/courses/:course_id/quizzes/:quiz_id/questions
;; Question groups are synced via:
;;   POST /api/v1/courses/:course_id/quizzes/:quiz_id/groups
;;
;; The sync process:
;;   1. Create/update the quiz
;;   2. Sync question groups (TYPE=group headings)
;;   3. Sync questions (all other level-2 headings)

;;; Code:

(require 'org-canvas-core)
(require 'ox-html)
(require 'cl-lib)

(declare-function org-canvas--validate-quiz-point-total "org-canvas-validate")

;;;; Configuration

(defcustom org-canvas-quizzes-file (org-canvas--path "quizzes.org")
  "Path to the quizzes.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-quizzes-file "quizzes.org")
(org-canvas-register-feature
 :name "Quizzes" :endpoint "quizzes"
 :file-var 'org-canvas-quizzes-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title)
(org-canvas-register-properties "quizzes"
  :label "Quizzes"
  :file-var 'org-canvas-quizzes-file
  :query "LEVEL=1"
  :body-api-key "description"
  :body-fn 'org-canvas--quiz-body-html
  :properties
  `((:org-prop "QUIZ_TYPE" :data-key :quiz_type :type enum
     :values ,org-canvas--valid-quiz-types
     :doc "The kind of quiz")
    (:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :doc "Whether item is visible (default: true)")
    (:org-prop "SHUFFLE_ANSWERS" :data-key :shuffle_answers :type boolean
     :doc "Randomize answer order")
    (:org-prop "TIME_LIMIT" :data-key :time_limit :type number
     :doc "Time limit in minutes")
    (:org-prop "ALLOWED_ATTEMPTS" :data-key :allowed_attempts :type number
     :doc "Max attempts (-1 = unlimited)")
    (:org-prop "DUE_AT" :data-key :due_at :type timestamp
     :doc "Due date")
    (:org-prop "UNLOCK_AT" :data-key :unlock_at :type timestamp
     :doc "When the quiz becomes available")
    (:org-prop "LOCK_AT" :data-key :lock_at :type timestamp
     :doc "When the quiz closes")
    (:org-prop "SHOW_CORRECT_ANSWERS" :data-key :show_correct_answers :type boolean
     :doc "Show correct answers after submission (default: true)")
    (:org-prop "SHOW_CORRECT_ANSWERS_AT" :data-key :show_correct_answers_at :type timestamp
     :doc "When to start showing correct answers")
    (:org-prop "HIDE_CORRECT_ANSWERS_AT" :data-key :hide_correct_answers_at :type timestamp
     :doc "When to stop showing correct answers")
    (:org-prop "HIDE_RESULTS" :data-key :hide_results :type enum
     :values ,org-canvas--valid-hide-results
     :doc "When to hide quiz results from students")
    (:org-prop "SCORING_POLICY" :data-key :scoring_policy :type enum
     :values ,org-canvas--valid-scoring-policies
     :doc "Which attempt's score to keep across multiple attempts")
    (:org-prop "ONE_QUESTION_AT_A_TIME" :data-key :one_question_at_a_time :type boolean
     :doc "Show one question per page")
    (:org-prop "CANT_GO_BACK" :data-key :cant_go_back :type boolean
     :doc "Prevent backtracking (requires =ONE_QUESTION_AT_A_TIME=)")
    (:org-prop "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT" :data-key :show_correct_answers_last_attempt :type boolean
     :doc "Only show correct answers on last attempt")
    (:org-prop "ONE_TIME_RESULTS" :data-key :one_time_results :type boolean
     :doc "Students can only view results once")
    (:org-prop "ONLY_VISIBLE_TO_OVERRIDES" :data-key :only_visible_to_overrides :type boolean
     :doc "Only visible to students with overrides")
    (:org-prop "GROUP" :data-key :assignment_group_id :type link
     :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID"
     :doc "Link to assignment-groups.org heading"))
  :date-order '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
  :structural-fn #'org-canvas--validate-quiz-point-total)
(org-canvas-register-properties "quiz-questions"
  :label "Quiz Questions"
  :file-var 'org-canvas-quizzes-file
  :query "LEVEL=2"
  :properties
  `((:org-prop "TYPE" :data-key :type :type enum
     :values ,org-canvas--valid-question-types
     :doc "Question type (see below)")
    (:org-prop "POINTS" :data-key :points :type number
     :doc "Points for this question")
    (:org-prop "PICK_COUNT" :data-key :pick_count :type number
     :doc "Number of questions to randomly select (default 1)")
    (:org-prop "QUESTION_POINTS" :data-key :question_points :type number
     :doc "Points per selected question (default 1)")
    (:org-prop "QUESTION_BANK_ID" :data-key :question_bank_id :type number
     :doc "Canvas assessment question bank ID (optional)")))

;;;; Helper Functions

(defun org-canvas--quiz-parse-description-subheading ()
  "Return the body of a `** Description' subheading under the quiz at point.
Point must be at the parent quiz heading.  Returns nil if no
`** Description' subheading exists."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point))))
      (when (re-search-forward "^\\*\\* Description[ \t]*$" subtree-end t)
        (let* ((desc-heading-pos (match-beginning 0))
               (desc-body-start (save-excursion
                                  (goto-char desc-heading-pos)
                                  (org-end-of-meta-data t)
                                  (point)))
               (desc-body-end (save-excursion
                                (goto-char desc-heading-pos)
                                (org-end-of-subtree t t)
                                (point))))
          (string-trim
           (buffer-substring-no-properties desc-body-start desc-body-end)))))))

(defun org-canvas--quiz-parse-body-text ()
  "Get the body text of current heading, excluding subheadings.
If a `** Description' subheading is present, return its body.
Otherwise return the inline text between the current heading and
the first subheading."
  (or (org-canvas--quiz-parse-description-subheading)
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
          (string-trim (buffer-substring-no-properties start end))))))

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

(defun org-canvas--quiz-read-props (pom)
  "Read raw quiz properties from org buffer at POM.
Returns a plist of raw string values."
  (list :title-raw (org-get-heading t t t t)
	:canvas-id (org-entry-get pom "CANVAS_ID")
	:quiz-type-raw (org-entry-get pom "QUIZ_TYPE")
	:time-limit-raw (org-entry-get pom "TIME_LIMIT")
	:published-raw (org-entry-get pom "PUBLISHED")
	:shuffle-raw (org-entry-get pom "SHUFFLE_ANSWERS")
	:allowed-attempts-raw (org-entry-get pom "ALLOWED_ATTEMPTS")
	:due-at-raw (org-entry-get pom "DUE_AT")
	:group-link (org-entry-get pom "GROUP")
	:assignment-group-id-raw (org-canvas--resolve-link-property
				  (org-entry-get pom "GROUP")
				  "CANVAS_ID" org-canvas-quizzes-file)
	:unlock-at-raw (org-entry-get pom "UNLOCK_AT")
	:lock-at-raw (org-entry-get pom "LOCK_AT")
	:access_code (org-entry-get pom "ACCESS_CODE")
	:show-correct-raw (org-entry-get pom "SHOW_CORRECT_ANSWERS")
	:show-correct-at-raw (org-entry-get pom "SHOW_CORRECT_ANSWERS_AT")
	:hide-correct-at-raw (org-entry-get pom "HIDE_CORRECT_ANSWERS_AT")
	:hide-results-raw (org-entry-get pom "HIDE_RESULTS")
	:scoring-policy-raw (org-entry-get pom "SCORING_POLICY")
	:one-question-raw (org-entry-get pom "ONE_QUESTION_AT_A_TIME")
	:cant-go-back-raw (org-entry-get pom "CANT_GO_BACK")
	:ip_filter (org-entry-get pom "IP_FILTER")
	:show-correct-last-raw (org-entry-get pom "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT")
	:one-time-results-raw (org-entry-get pom "ONE_TIME_RESULTS")
	:only-visible-raw (org-entry-get pom "ONLY_VISIBLE_TO_OVERRIDES")
	:body-text (org-canvas--quiz-parse-body-text)))

(defun org-canvas--quiz-transform-props (props)
  "Transform raw PROPS plist into final quiz data (pure, no buffer access)."
  (let ((title (org-canvas--strip-statistics-cookie (plist-get props :title-raw)))
	(time-limit-raw (plist-get props :time-limit-raw))
	(attempts-raw (plist-get props :allowed-attempts-raw))
	(due-at-raw (plist-get props :due-at-raw))
	(unlock-at-raw (plist-get props :unlock-at-raw))
	(lock-at-raw (plist-get props :lock-at-raw))
	(show-correct-at-raw (plist-get props :show-correct-at-raw))
	(hide-correct-at-raw (plist-get props :hide-correct-at-raw))
	(group-id-raw (plist-get props :assignment-group-id-raw)))
    (list :title title
	  :canvas-id (plist-get props :canvas-id)
	  :quiz_type (org-canvas--validate-property
		      (plist-get props :quiz-type-raw)
		      '("assignment" "practice_quiz" "graded_survey" "survey")
		      "QUIZ_TYPE" "assignment")
	  :time_limit (when time-limit-raw
			(org-canvas--safe-string-to-number time-limit-raw "TIME_LIMIT"))
	  :published (org-canvas--interpret-boolean (plist-get props :published-raw) t)
	  :shuffle_answers (org-canvas--interpret-boolean (plist-get props :shuffle-raw))
	  :allowed_attempts (when attempts-raw
			      (org-canvas--safe-string-to-number attempts-raw "ALLOWED_ATTEMPTS"))
	  :due_at (when due-at-raw (org-canvas-org-parse-timestamp due-at-raw))
	  :unlock_at (when unlock-at-raw (org-canvas-org-parse-timestamp unlock-at-raw))
	  :lock_at (when lock-at-raw (org-canvas-org-parse-timestamp lock-at-raw))
	  :access_code (plist-get props :access_code)
	  :show_correct_answers (org-canvas--interpret-boolean (plist-get props :show-correct-raw) t)
	  :show_correct_answers_at (when show-correct-at-raw
				     (org-canvas-org-parse-timestamp show-correct-at-raw))
	  :hide_correct_answers_at (when hide-correct-at-raw
				     (org-canvas-org-parse-timestamp hide-correct-at-raw))
	  :hide_results (org-canvas--validate-property
			 (plist-get props :hide-results-raw)
			 '("always" "until_after_last_attempt")
			 "HIDE_RESULTS" nil)
	  :scoring_policy (org-canvas--validate-property
			   (plist-get props :scoring-policy-raw)
			   '("keep_highest" "keep_latest")
			   "SCORING_POLICY" nil)
	  :one_question_at_a_time (org-canvas--interpret-boolean
				   (plist-get props :one-question-raw))
	  :cant_go_back (org-canvas--interpret-boolean (plist-get props :cant-go-back-raw))
	  :ip_filter (plist-get props :ip_filter)
	  :show_correct_answers_last_attempt (org-canvas--interpret-boolean
					      (plist-get props :show-correct-last-raw))
	  :one_time_results (org-canvas--interpret-boolean
			     (plist-get props :one-time-results-raw))
	  :only_visible_to_overrides (org-canvas--interpret-boolean
				      (plist-get props :only-visible-raw))
	  :assignment_group_id (when group-id-raw (string-to-number group-id-raw)))))

(defun org-canvas--quiz-body-text-to-html (text)
  "Return TEXT, a quiz's Org description, as HTML, or nil when empty."
  (when (> (length text) 0)
    (let ((org-export-with-sub-superscripts nil))
      (org-export-string-as text 'html t))))

(defun org-canvas--quiz-body-html ()
  "Return the HTML the quiz at point would push as its description.
The drift report's body extractor for quizzes, named by the registry's
`:body-fn': only the text before the first question heading is the
description (see `org-canvas--quiz-parse-body-text'), so the shared
subtree export, which would take the questions too, does not apply.
Returns \"\" for a quiz with none."
  (or (org-canvas--quiz-body-text-to-html (org-canvas--quiz-parse-body-text)) ""))

(defun org-canvas--quiz-parse-entry ()
  "Extract quiz data from the Org heading at point."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[Quiz Parse] Starting at point %d" (point))

  (let* ((pom (point-marker))
	 (raw (org-canvas--quiz-read-props pom))
	 (data (org-canvas--quiz-transform-props raw))
	 (title (plist-get data :title))
	 (body-text (plist-get raw :body-text)))

    (org-canvas--require-title title pom "Quiz")

    (org-canvas--log-info org-canvas--logger "[Quiz Parse] Quiz: '%s' (ID: %s)"
      title (or (plist-get data :canvas-id) "NEW"))
    (when (plist-get data :assignment_group_id)
      (org-canvas--log-debug org-canvas--logger "[Quiz Parse] Assignment Group ID: %s"
	(plist-get data :assignment_group_id)))

    (plist-put data :description (org-canvas--quiz-body-text-to-html body-text))
    (plist-put data :pom pom)
    data))

(defun org-canvas--quiz-build-payload (data)
  "Convert quiz DATA to Canvas payload."
  ;; No `published' here: it is applied after the questions exist, by
  ;; `org-canvas--quiz-settle-publish-state' (issue #59).  Omitting the
  ;; key leaves an existing quiz's state untouched, and Canvas creates a
  ;; new quiz unpublished, which is what the deferred publish needs.
  (let ((quiz-obj `((title . ,(plist-get data :title))
		    (quiz_type . ,(plist-get data :quiz_type)))))

    (when-let* ((desc (plist-get data :description)))
      (push `(description . ,desc) quiz-obj))

    (when-let* ((limit (plist-get data :time_limit)))
      (push `(time_limit . ,limit) quiz-obj))

    (when (plist-get data :shuffle_answers)
      (push `(shuffle_answers . t) quiz-obj))

    (when-let* ((attempts (plist-get data :allowed_attempts)))
      (push `(allowed_attempts . ,attempts) quiz-obj))

    (when-let* ((due (plist-get data :due_at)))
      (push `(due_at . ,due) quiz-obj))

    (when-let* ((group-id (plist-get data :assignment_group_id)))
      (push `(assignment_group_id . ,group-id) quiz-obj))

    (when-let* ((unlock (plist-get data :unlock_at)))
      (push `(unlock_at . ,unlock) quiz-obj))

    (when-let* ((lock (plist-get data :lock_at)))
      (push `(lock_at . ,lock) quiz-obj))

    (when-let* ((code (plist-get data :access_code)))
      (push `(access_code . ,code) quiz-obj))

    ;; Always send show_correct_answers (meaningful default of true)
    (push `(show_correct_answers . ,(org-canvas--to-json-boolean
				     (plist-get data :show_correct_answers)))
	  quiz-obj)

    (when-let* ((show-at (plist-get data :show_correct_answers_at)))
      (push `(show_correct_answers_at . ,show-at) quiz-obj))

    (when-let* ((hide-at (plist-get data :hide_correct_answers_at)))
      (push `(hide_correct_answers_at . ,hide-at) quiz-obj))

    (when-let* ((hide (plist-get data :hide_results)))
      (push `(hide_results . ,hide) quiz-obj))

    (when-let* ((scoring (plist-get data :scoring_policy)))
      (push `(scoring_policy . ,scoring) quiz-obj))

    (when (plist-get data :one_question_at_a_time)
      (push `(one_question_at_a_time . t) quiz-obj))

    (when (plist-get data :cant_go_back)
      (push `(cant_go_back . t) quiz-obj))

    (when-let* ((ip (plist-get data :ip_filter)))
      (push `(ip_filter . ,ip) quiz-obj))

    (when (plist-get data :show_correct_answers_last_attempt)
      (push '(show_correct_answers_last_attempt . t) quiz-obj))

    (when (plist-get data :one_time_results)
      (push '(one_time_results . t) quiz-obj))

    (when (plist-get data :only_visible_to_overrides)
      (push '(only_visible_to_overrides . t) quiz-obj))

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
      (org-canvas--log-warning org-canvas--logger
        "[Verify] '%s': assignment_group_id mismatch! Expected %s, got %s"
        (plist-get data :title) expected-group actual-group))))

(defun org-canvas--quiz-sync-children (data response)
  "Sync question groups and questions for the quiz in DATA/RESPONSE.
Returns a plist (:groups N :questions N :failed N) describing what was
written, which `org-canvas--quiz-settle-publish-state\' uses to decide
whether Canvas\'s cached totals still reflect the quiz."
  (let ((quiz-id (or (alist-get 'id response)
                     (plist-get data :canvas-id)))
        (marker (point-marker)))
    (when quiz-id
      (let ((groups (org-canvas--sync-quiz-groups marker quiz-id))
            (questions (org-canvas--sync-quiz-questions marker quiz-id)))
        (list :groups (car groups)
              :questions (car questions)
              :failed (+ (cdr groups) (cdr questions)))))))

;;;; Publish Sequencing
;;
;; Canvas computes a classic quiz's points_possible and question_count —
;; and the points on its backing assignment — when the quiz is published,
;; not when a question is inserted.  Questions sync inside finalize, after
;; the quiz record already exists, so a quiz created with PUBLISHED: true
;; published while it was still empty and froze its totals at zero: 27
;; one-point questions and a gradebook column out of 0, with every
;; question showing its point value in the editor (issue #59).
;;
;; `published' is therefore kept out of the quiz payload and applied here,
;; once the questions are in place, where it is a real transition.

(defun org-canvas--quiz-set-published (quiz-id published)
  "Set the PUBLISHED state of QUIZ-ID on Canvas.
Returns `org-canvas--dry-run-response' without contacting Canvas when
previewing."
  (if org-canvas--dry-run
      (progn
        (org-canvas--log-info org-canvas--logger
          "[DRY-RUN] Would %s quiz %s"
          (if published "publish" "unpublish") quiz-id)
        org-canvas--dry-run-response)
    (org-canvas--log-info org-canvas--logger
      "[Publish] %s quiz %s" (if published "Publishing" "Unpublishing") quiz-id)
    (org-canvas-api-request
     'PUT (org-canvas-api-course-endpoint "quizzes/%s" quiz-id)
     :data `((quiz . ((published . ,(org-canvas--to-json-boolean published))))))))

(defun org-canvas--quiz-totals-stale-p (quiz written)
  "Return non-nil when QUIZ's cached totals do not reflect WRITTEN.
WRITTEN is the plist from `org-canvas--quiz-sync-children'.

Only a remote count *lower* than what we wrote counts as stale: that is
the signature of questions Canvas has not folded into its quiz data.  A
higher count means the course holds questions this file does not
describe, which is a different problem and must not trigger a republish.
The check is skipped entirely when a child failed to sync (the counts
would not line up anyway) and narrowed to a flat zero when the quiz uses
question groups, where the quiz draws a subset and the numbers are not
comparable."
  (let ((questions (plist-get written :questions))
        (groups (plist-get written :groups))
        (failed (plist-get written :failed))
        (remote-count (alist-get 'question_count quiz))
        (points (alist-get 'points_possible quiz)))
    (cond
     ((or (null questions) (zerop questions)) nil)
     ((and failed (> failed 0)) nil)
     ((and groups (> groups 0))
      (or (and (numberp remote-count) (zerop remote-count))
          (and (numberp points) (zerop points))))
     (t (and (numberp remote-count) (< remote-count questions))))))

(defun org-canvas--quiz-refresh-totals (quiz-id title written)
  "Regenerate the cached totals of the already published QUIZ-ID if stale.
TITLE is for logging and WRITTEN is what `org-canvas--quiz-sync-children'
reported.  Canvas only recomputes on a publish transition, so a quiz that
gained questions while published needs one; an unpublish is only safe
while nothing has been submitted, which `unpublishable' reports."
  (let ((quiz (org-canvas-api-request
               'GET (org-canvas-api-course-endpoint "quizzes/%s" quiz-id))))
    (when (org-canvas--quiz-totals-stale-p quiz written)
      (if (eq (alist-get 'unpublishable quiz) t)
          (progn
            (org-canvas--log-info org-canvas--logger
              "[Publish] '%s' has questions Canvas has not counted (%s of %s); republishing to regenerate its totals"
              title (alist-get 'question_count quiz) (plist-get written :questions))
            (org-canvas--quiz-set-published quiz-id nil)
            (org-canvas--quiz-set-published quiz-id t))
        (org-canvas--log-warning org-canvas--logger
          "[Publish] '%s' reads %s question(s) and %s point(s) on Canvas but has %s question(s); it has submissions, so it cannot be republished automatically — open it in Canvas and save to regenerate"
          title (alist-get 'question_count quiz) (alist-get 'points_possible quiz)
          (plist-get written :questions))))))

(defun org-canvas--quiz-settle-publish-state (data response written)
  "Apply DATA's PUBLISHED to the quiz in RESPONSE, now that its questions exist.
WRITTEN is the plist from `org-canvas--quiz-sync-children', or nil when
no children were synced.

RESPONSE describes the quiz as it was *before* the questions were
written, which is what makes it usable here: its `published' field says
whether this run needs a publish transition or whether the quiz was
already published and may now be carrying stale totals."
  (let* ((quiz-id (or (alist-get 'id response) (plist-get data :canvas-id)))
         (desired (plist-get data :published))
         (was-published (eq (alist-get 'published response) t))
         (title (plist-get data :title)))
    (when quiz-id
      (cond
       ((and desired (not was-published))
        (org-canvas--quiz-set-published quiz-id t))
       ((and (not desired) was-published)
        (org-canvas--quiz-set-published quiz-id nil))
       ((and desired was-published written (not org-canvas--dry-run))
        (org-canvas--quiz-refresh-totals quiz-id title written))))))

(defun org-canvas--quiz-finalize (data response)
  "Save quiz pointed to in DATA with CANVAS_ID from RESPONSE to org heading."
  (org-canvas--finalize-item data response
    :post-fn (lambda (data response)
               (org-canvas--quiz-verify-response data response)
               (let ((written (org-canvas--quiz-sync-children data response)))
                 (org-canvas--quiz-settle-publish-state data response written)))))

;;;; Question Parsing (Level 2)

(defun org-canvas--question-read-props (pom quiz-canvas-id)
  "Read raw question properties from org buffer at POM.
QUIZ-CANVAS-ID is the Canvas ID of the parent quiz.
Returns a plist of raw string values."
  (list :title-raw (org-get-heading t t t t)
        :canvas-id (org-entry-get pom "CANVAS_ID")
        :type-raw (org-entry-get pom "TYPE")
        :points-raw (org-entry-get pom "POINTS")
        :quiz-canvas-id quiz-canvas-id
        :text (org-canvas--quiz-parse-question-text)))

(defun org-canvas--question-transform-props (props)
  "Transform raw PROPS plist into final question data (pure, no buffer access)."
  (list :name (org-canvas--strip-statistics-cookie (plist-get props :title-raw))
        :text (plist-get props :text)
        :canvas-id (plist-get props :canvas-id)
        :quiz-canvas-id (plist-get props :quiz-canvas-id)
        :question_type (org-canvas--validate-property
                        (plist-get props :type-raw)
                        '("multiple_choice_question" "true_false_question"
                          "short_answer_question"
                          "fill_in_multiple_blanks_question"
                          "multiple_dropdowns_question"
                          "multiple_answers_question"
                          "matching_question" "numerical_question"
                          "essay_question" "file_upload_question"
                          "text_only_question")
                        "TYPE" "multiple_choice_question")
        :points_possible (org-canvas--interpret-number
                          (plist-get props :points-raw) 1)))

(defun org-canvas--question-parse-entry (quiz-canvas-id)
  "Extract question data from Org heading at point.
QUIZ-CANVAS-ID is the Canvas ID of the parent quiz."
  (org-back-to-heading t)

  (let* ((pom (point-marker))
         (raw (org-canvas--question-read-props pom quiz-canvas-id))
         (data (org-canvas--question-transform-props raw))
         (q-type (plist-get data :question_type))
         (text-html (let ((org-export-with-sub-superscripts nil))
                      (org-export-string-as (plist-get data :text) 'html t)))
         (answers (org-canvas--question-build-answers q-type)))

    (org-canvas--log-debug org-canvas--logger "[Question Parse] '%s' type=%s"
      (plist-get data :name) q-type)

    (plist-put data :pom pom)
    (plist-put data :text-html text-html)
    (plist-put data :answers answers)
    data))

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

(defconst org-canvas--question-answer-builders
  `(("multiple_choice_question"         . ,#'org-canvas--question-build-checkbox-answers)
    ("true_false_question"              . ,#'org-canvas--question-build-checkbox-answers)
    ("multiple_answers_question"        . ,#'org-canvas--question-build-checkbox-answers)
    ("short_answer_question"            . ,#'org-canvas--question-build-short-answers)
    ("fill_in_multiple_blanks_question" . ,(lambda () (org-canvas--question-build-blank-answers t)))
    ("multiple_dropdowns_question"      . ,(lambda () (org-canvas--question-build-blank-answers nil)))
    ("matching_question"                . ,#'org-canvas--question-build-matching-answers)
    ("numerical_question"               . ,#'org-canvas--question-build-numerical-answer))
  "Alist mapping question types to answer builder functions.")

(defun org-canvas--question-build-answers (q-type)
  "Build answers array based on Q-TYPE from current heading content."
  (when-let* ((builder (alist-get q-type org-canvas--question-answer-builders nil nil #'equal)))
    (funcall builder)))

(defun org-canvas--question-build-payload (data)
  "Build Canvas API payload from question DATA (pure, no buffer access)."
  (let* ((text-html (plist-get data :text-html))
	 (answers (plist-get data :answers))
	 (question-obj `((question_name . ,(plist-get data :name))
			 (question_text . ,text-html)
			 (question_type . ,(plist-get data :question_type))
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

    (org-canvas--log-info org-canvas--logger "[Question API] %s '%s'" method name)

    (condition-case err
	(org-canvas-api-request method endpoint :data payload)
      (error
       (org-canvas--log-error org-canvas--logger "[Question API] Failed: %s" (error-message-string err))
       (signal (car err) (cdr err))))))

(defun org-canvas--question-finalize (data response)
  "Save question from DATA with CANVAS_ID from RESPONSE."
  (org-canvas--finalize-item data response :title-key :name))

;;;; Question Groups (Question Banks)

(defun org-canvas--question-group-parse-entry (quiz-canvas-id)
  "Extract question group data from Org heading at point.
QUIZ-CANVAS-ID is the Canvas ID of the parent quiz."
  (org-back-to-heading t)

  (let* ((pom (point-marker))
	 (name (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
	 (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
	 (pick-count (org-canvas-org-get-number-property pom "PICK_COUNT" 1))
	 (question-points (org-canvas-org-get-number-property pom "QUESTION_POINTS" 1))
	 (bank-id-str (org-canvas-org-get-property pom "QUESTION_BANK_ID"))
	 (bank-id (when bank-id-str
		    (org-canvas--safe-string-to-number bank-id-str "QUESTION_BANK_ID"))))

    (org-canvas--log-debug org-canvas--logger "[Group Parse] '%s' pick=%s pts=%s bank=%s"
      name pick-count question-points (or bank-id "none"))

    (list :name name
	  :canvas-id canvas-id
	  :quiz-canvas-id quiz-canvas-id
	  :pick-count pick-count
	  :question-points question-points
	  :bank-id bank-id
	  :pom pom)))

(defun org-canvas--question-group-build-payload (data)
  "Build Canvas API payload from question group DATA.
Wraps in `quiz_groups' key as required by the API.
Only includes `assessment_question_bank_id' on create (POST),
since the API ignores it on update (PUT)."
  (let ((group-obj `((name . ,(plist-get data :name))
		     (pick_count . ,(plist-get data :pick-count))
		     (question_points . ,(plist-get data :question-points)))))
    ;; Bank ID is create-only: only include when no CANVAS_ID (POST)
    (when (and (null (plist-get data :canvas-id))
	       (plist-get data :bank-id))
      (push `(assessment_question_bank_id . ,(plist-get data :bank-id)) group-obj))
    `((quiz_groups . (,group-obj)))))

(defun org-canvas--question-group-push-to-api (data payload)
  "Send question group PAYLOAD (from DATA) to Canvas API.
Unwraps the `quiz_groups' response wrapper."
  (let* ((quiz-id (plist-get data :quiz-canvas-id))
	 (g-id (plist-get data :canvas-id))
	 (name (plist-get data :name))
	 (method (if g-id 'PUT 'POST))
	 (endpoint (if g-id
		       (org-canvas-api-course-endpoint "quizzes/%s/groups/%s" quiz-id g-id)
		     (org-canvas-api-course-endpoint "quizzes/%s/groups" quiz-id))))

    (org-canvas--log-info org-canvas--logger "[Group API] %s '%s'" method name)

    (condition-case err
	(let ((response (org-canvas-api-request method endpoint :data payload)))
	  ;; Response wraps in {"quiz_groups": [QuizGroup]} — unwrap
	  (let ((groups (alist-get 'quiz_groups response)))
	    (if (and groups (> (length groups) 0))
		(aref groups 0)
	      response)))
      (error
       (org-canvas--log-error org-canvas--logger "[Group API] Failed: %s" (error-message-string err))
       (signal (car err) (cdr err))))))

(defun org-canvas--question-group-finalize (data response)
  "Save question group from DATA with CANVAS_ID from RESPONSE."
  (org-canvas--finalize-item data response :title-key :name))

(defun org-canvas--sync-quiz-groups (quiz-marker quiz-canvas-id)
  "Sync all question groups under the quiz at QUIZ-MARKER.
QUIZ-CANVAS-ID is the Canvas ID of the quiz.
Returns a cons (SUCCESS . FAIL) count."
  (let ((group-markers nil)
	(group-success 0))
    ;; Collect all group markers (level-2 headings with TYPE=group)
    (with-current-buffer (marker-buffer quiz-marker)
      (save-excursion
	(goto-char (marker-position quiz-marker))
	(let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
	  (while (and (outline-next-heading)
		      (< (point) subtree-end))
	    (when (and (= (org-outline-level) 2)
		       (equal (org-entry-get (point) "TYPE") "group"))
	      (push (point-marker) group-markers)))))
      (setq group-markers (nreverse group-markers)))

    ;; Sync each group
    (dolist (g-marker group-markers)
      (with-current-buffer (marker-buffer g-marker)
	(save-excursion
	  (goto-char (marker-position g-marker))
	  (condition-case err
	      (let* ((data (org-canvas--question-group-parse-entry quiz-canvas-id))
		     (payload (org-canvas--question-group-build-payload data))
		     (response (org-canvas--question-group-push-to-api data payload)))
		(org-canvas--question-group-finalize data response)
		(setq group-success (1+ group-success)))
	    (error
	     (org-canvas--log-error org-canvas--logger "[Group] Failed: %s"
	       (error-message-string err)))))))

    ;; Release markers to avoid memory leaks
    (dolist (m group-markers) (set-marker m nil))

    (when (> (length group-markers) 0)
      (org-canvas--log-info org-canvas--logger "[Groups] %d/%d synced"
	group-success (length group-markers)))
    (cons group-success (- (length group-markers) group-success))))

;;;; Main Sync Function

(defun org-canvas--quiz-description-heading-p ()
  "Return non-nil if the level-2 heading at point is a `** Description' wrapper."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading (org-get-heading t t t t)))
      (and heading (string= (string-trim heading) "Description")))))

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
	    (when (and (= (org-outline-level) 2)
		       (not (equal (org-entry-get (point) "TYPE") "group"))
		       (not (org-canvas--quiz-description-heading-p)))
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
	     (org-canvas--log-error org-canvas--logger "[Question] Failed: %s"
	       (error-message-string err)))))))

    ;; Release markers to avoid memory leaks
    (dolist (m question-markers) (set-marker m nil))

    (org-canvas--log-info org-canvas--logger "[Questions] %d/%d synced"
      question-success (length question-markers))
    (cons question-success (- (length question-markers) question-success))))

(defun org-canvas--quiz-questions-digest (data)
  "Digest the question and question-group subtrees of the quiz in DATA.
Folded into the quiz payload hash via `:hash-extra': questions sync
inside finalize, which the unchanged-skip bypasses, so without this a
question edit (text, answers, points, type) would never reach Canvas
once the quiz's own attributes stopped changing — the same bug class
as issue #26."
  (org-canvas--org-children-digest (or (plist-get data :pom) (point))))

(org-canvas-define-sync quizzes
  :file org-canvas-quizzes-file
  :parse #'org-canvas--quiz-parse-entry
  :build #'org-canvas--quiz-build-payload
  :push #'org-canvas--quiz-push-to-api
  :finalize #'org-canvas--quiz-finalize
  :hash-extra #'org-canvas--quiz-questions-digest)

;;;; Delete Functions

(org-canvas-define-delete-all quizzes
  :endpoint "quizzes"
  :file org-canvas-quizzes-file)

;;;; Pull

(defconst org-canvas--quiz-pull-property-specs
  '(;; (api-key property-name type)
    ;; Types: string = set if non-nil, format = format as string,
    ;;        boolean = set-boolean, boolean-nonnull = set-boolean if not :null,
    ;;        timestamp = set-timestamp, string-nonnull = set if non-null
    (quiz_type "QUIZ_TYPE" string)
    (time_limit "TIME_LIMIT" format)
    (shuffle_answers "SHUFFLE_ANSWERS" boolean)
    (due_at "DUE_AT" timestamp)
    (unlock_at "UNLOCK_AT" timestamp)
    (lock_at "LOCK_AT" timestamp)
    (access_code "ACCESS_CODE" string-nonnull)
    (show_correct_answers "SHOW_CORRECT_ANSWERS" boolean-nonnull)
    (show_correct_answers_at "SHOW_CORRECT_ANSWERS_AT" timestamp)
    (hide_correct_answers_at "HIDE_CORRECT_ANSWERS_AT" timestamp)
    (hide_results "HIDE_RESULTS" string-nonnull)
    (scoring_policy "SCORING_POLICY" string-nonnull)
    (one_question_at_a_time "ONE_QUESTION_AT_A_TIME" boolean)
    (cant_go_back "CANT_GO_BACK" boolean)
    (ip_filter "IP_FILTER" string-nonnull)
    (show_correct_answers_last_attempt "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT" boolean)
    (one_time_results "ONE_TIME_RESULTS" boolean)
    (only_visible_to_overrides "ONLY_VISIBLE_TO_OVERRIDES" boolean))
  "Specs for pulling quiz properties: (api-key property-name type).")

(defun org-canvas--quiz-pull-set-single-property (pos prop-name val type)
  "Set a single quiz property PROP-NAME at POS from VAL using TYPE."
  (pcase type
    ('string (when val (org-canvas-org-set-property pos prop-name val)))
    ('format (when val (org-canvas-org-set-property pos prop-name (format "%s" val))))
    ('boolean (when val (org-canvas--pull-set-boolean-property pos prop-name val)))
    ('boolean-nonnull
     (when (not (eq val :null))
       (org-canvas--pull-set-boolean-property pos prop-name val)))
    ('timestamp (org-canvas--pull-set-timestamp-property pos prop-name val))
    ('string-nonnull (when val (org-canvas-org-set-property pos prop-name val)))))

(defun org-canvas--quiz-pull-set-properties (pos quiz _file)
  "Set all properties on heading at POS from QUIZ API response.
FILE is the quizzes.org path, used for group link resolution."
  (org-canvas-org-save-sync-state pos (alist-get 'id quiz))
  (dolist (spec org-canvas--quiz-pull-property-specs)
    (let ((val (if (memq (nth 2 spec) '(string-nonnull))
                   (org-canvas--alist-get-non-null (nth 0 spec) quiz)
                 (alist-get (nth 0 spec) quiz))))
      (org-canvas--quiz-pull-set-single-property pos (nth 1 spec) val (nth 2 spec))))
  ;; Resolve assignment group link (special case)
  (let ((group-id (alist-get 'assignment_group_id quiz)))
    (when (and group-id (fboundp 'org-canvas--assignment-resolve-group-link))
      (let ((group-link (org-canvas--assignment-resolve-group-link group-id)))
        (when group-link
          (org-canvas-org-set-property pos "GROUP" group-link))))))

(defun org-canvas--quiz-insert-question-body (q-text)
  "Insert question body text Q-TEXT (HTML) at point as Org markup.
Canvas file URLs embedded in the question (e.g. inline images) are
rewritten to local `[[file:...]]' links via the shared file-id cache."
  (when (and q-text (not (string-empty-p q-text)))
    (insert "\n" (org-canvas--html-to-org-with-rewrite q-text) "\n")))

(defun org-canvas--quiz-insert-answers (answers)
  "Insert ANSWERS list at point as Org checklist items.
Each answer's weight determines checked ([X]) vs unchecked ([ ]).
Canvas file URLs in answer text/html are rewritten to local file links."
  (when answers
    (dolist (a (append answers nil))
      (let ((text (org-canvas--html-to-org-inline-with-rewrite
                   (or (alist-get 'text a) (alist-get 'html a) "")))
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

(defun org-canvas--quiz-pull-fetch-questions (quiz-id)
  "Fetch question list for QUIZ-ID, returning a list of alists.
Returns nil on API error."
  (condition-case err
      (let ((q-url (org-canvas-api-course-endpoint
                    "quizzes/%s/questions" quiz-id)))
        (org-canvas-api-request-all-pages 'GET q-url))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Quizzes] Failed to fetch questions for quiz %s (%s); questions omitted from pull"
       quiz-id (error-message-string err))
     nil)))

(defun org-canvas--quiz-pull-insert-questions (quiz-id)
  "Fetch and insert questions for QUIZ-ID as L2 headings.
Point must be at the parent quiz heading."
  (let ((questions (org-canvas--quiz-pull-fetch-questions quiz-id)))
    (dolist (q questions)
      (org-canvas--quiz-pull-insert-question q))))

(defun org-canvas--quiz-pull-insert-description-wrapped (description)
  "Insert DESCRIPTION HTML inside a `** Description' subheading.
Point must be at the parent quiz heading.  Leaves point at the
parent quiz heading after insertion."
  (let ((quiz-pos (point))
        (subtree-end (save-excursion (org-end-of-subtree t) (point))))
    (goto-char subtree-end)
    (unless (bolp) (insert "\n"))
    (insert "** Description\n")
    (org-back-to-heading t)
    (when description
      (org-canvas--pull-insert-body description))
    (goto-char quiz-pos)))

(defun org-canvas--quiz-pull-emit-body (quiz-pos description questions)
  "Emit DESCRIPTION and QUESTIONS under the quiz at QUIZ-POS.
DESCRIPTION (when non-empty) is always wrapped in a `** Description'
subheading regardless of whether QUESTIONS follow.  This keeps the
schema consistent across quizzes — every quiz with text gets a
predictable Description subtree, so downstream tooling and
human readers don't have to guess whether to find prose under the
parent quiz or under a dedicated subheading."
  (goto-char quiz-pos)
  (when (and description (not (string-empty-p description)))
    (org-canvas--quiz-pull-insert-description-wrapped description)
    (goto-char quiz-pos))
  (when questions
    (dolist (q questions)
      (org-canvas--quiz-pull-insert-question q))))

;;;###autoload
(defun org-canvas-pull-quizzes ()
  "Pull quizzes from Canvas into quizzes.org."
  (interactive)
  (org-canvas--start-operation "PULLING QUIZZES")
  (let* ((file (expand-file-name org-canvas-quizzes-file))
         (endpoint (org-canvas-api-course-endpoint "quizzes"))
         (remote (org-canvas-api-request-all-pages 'GET endpoint))
         (count 0)
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "quizzes")
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (dolist (quiz (org-canvas--pull-sort-items remote))
        (let* ((id (alist-get 'id quiz))
               (title (alist-get 'title quiz))
               (description (alist-get 'description quiz))
               (questions (org-canvas--quiz-pull-fetch-questions id))
               (pos (org-canvas--pull-upsert-heading file id title)))
          (goto-char pos)
          (when title (org-edit-headline title))
          (org-canvas--quiz-pull-set-properties pos quiz file)
          (org-canvas--quiz-pull-emit-body pos description questions)
          (cl-incf count)))
      (org-canvas--pull-write-file-header)
      (org-canvas--save-buffer))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger "Quizzes pull complete: %d quizzes" count)
    (message "Quizzes pull complete: %d quizzes." count)))

(provide 'org-canvas-quizzes)
;;; org-canvas-quizzes.el ends here
