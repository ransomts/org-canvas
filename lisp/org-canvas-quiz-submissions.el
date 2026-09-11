;;; org-canvas-quiz-submissions.el --- Pull a classic quiz's attempts from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A grading file (org-canvas-submissions.el) shows an assignment's
;; submissions, and a classic quiz has one through its shadow
;; assignment: the score is there, the story is not.  How many attempts
;; a student took, which score Canvas kept against the latest one, how
;; long they spent and whether an attempt is still open live on the
;; quiz submission, which only the quiz's own endpoint returns.
;;
;; `org-canvas-pull-quiz-submissions' picks a classic quiz and writes
;; one read-only table for it under the submissions directory, as
;; <quiz> (quiz).org: one row per student, sorted by name, with
;; Attempts, Kept, Latest, Out of, Time, Finished and State, and a
;; summary line above.  Every pull rewrites the file.  Nothing is
;; pushed: a quiz score is changed through the quiz's grading file
;; (`org-canvas-pull-submissions' on the shadow assignment), where the
;; push and the conflict check already live.
;;
;; New Quizzes have no such endpoint; their scores arrive through the
;; shadow assignment only, so the prompt lists classic quizzes alone.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/quizzes                 the classic quizzes
;;   GET /courses/:id/quizzes/:quiz_id/submissions?include[]=user
;;       answers {quiz_submissions: [...], users: [...]}, a wrapped
;;       object rather than a bare array, paginated by the Link header.
;;       One quiz submission per student, the latest attempt: `attempt',
;;       `kept_score', `score', `quiz_points_possible', `time_spent'
;;       (seconds), `finished_at', `workflow_state' (untaken while an
;;       attempt is open, complete, pending_review).
;;
;; PRIVACY
;; =======
;; The file is a table of scores.  It lives in the submissions
;; directory, which gets a .gitignore when created
;; (`org-canvas-submissions-write-gitignore'), so it never travels with
;; a course repository.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules, extending the grading
;; workflow: it reuses the submissions directory and its .gitignore
;; (as adopt requires diff).
(require 'org-canvas-submissions)

;;;; Fetching

(defun org-canvas--quiz-submissions-fetch-quizzes ()
  "Return the course's classic quizzes as a list, sorted by title.
The quizzes endpoint lists classic quizzes only; a New Quiz is an
assignment and has no quiz submissions to pull."
  (sort (append (org-canvas-api-request-all-pages
                 'GET (org-canvas-api-course-endpoint "quizzes"))
                nil)
        (lambda (a b) (string< (or (alist-get 'title a) "")
                               (or (alist-get 'title b) "")))))

(defun org-canvas--quiz-submissions-fetch (quiz-id)
  "Return QUIZ-ID's submissions and users as a plist (:submissions :users).
The endpoint wraps its rows in an object, so the paginated helper
cannot walk it; the Link header is followed here, and a reply without
headers (a stubbed request) is taken as the only page."
  (let ((url (org-canvas-api-course-endpoint "quizzes/%s/submissions" quiz-id))
        (params '(("include[]" . "user") ("per_page" . "100")))
        (submissions nil) (users nil) (next-url nil) (page 1) (done nil))
    (while (not done)
      (when (> page 1)
        (message "Fetching quiz submissions, page %d (%d so far)..."
                 page (length submissions)))
      (let* ((reply (if next-url
                        (org-canvas-api-request 'GET next-url :as 'response)
                      (org-canvas-api-request 'GET url :params params :as 'response)))
             (with-headers (plz-response-p reply))
             (body (if with-headers (org-canvas--api-decode-response reply) reply)))
        (setq submissions (append submissions (append (alist-get 'quiz_submissions body) nil))
              users (append users (append (alist-get 'users body) nil))
              next-url (and with-headers (org-canvas--api-next-page-url reply)))
        (if next-url (setq page (1+ page)) (setq done t))))
    (list :submissions submissions :users users)))

;;;; Rows

(defun org-canvas--quiz-submissions-name (user-id users)
  "Return the sortable name of USER-ID from the USERS sidecar.
Fall back to \"User <id>\" so rows stay distinct when the include is
missing, as the grading file does."
  (let ((user (cl-find-if (lambda (u) (equal (alist-get 'id u) user-id)) users)))
    (or (alist-get 'sortable_name user)
        (alist-get 'name user)
        (format "User %s" user-id))))

(defun org-canvas--quiz-submissions-rows (submissions users)
  "Fold SUBMISSIONS into one plist per student, sorted by name.
USERS is the sidecar the include supplies.  Each plist has :name,
:attempt, :kept, :latest, :possible, :time (seconds), :finished
and :state; a number Canvas does not have is nil."
  (let ((rows
         (mapcar
          (lambda (s)
            (list :name (org-canvas--quiz-submissions-name (alist-get 'user_id s) users)
                  :attempt (org-canvas--alist-get-non-null 'attempt s)
                  :kept (org-canvas--alist-get-non-null 'kept_score s)
                  :latest (org-canvas--alist-get-non-null 'score s)
                  :possible (org-canvas--alist-get-non-null 'quiz_points_possible s)
                  :time (org-canvas--alist-get-non-null 'time_spent s)
                  :finished (org-canvas--alist-get-non-null 'finished_at s)
                  :state (or (org-canvas--alist-get-non-null 'workflow_state s) "")))
          submissions)))
    (sort rows (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

;;;; Rendering

(defun org-canvas--quiz-submissions-number (value)
  "Render VALUE for a table cell: one decimal for a score, - when absent."
  (cond ((null value) "-")
        ((integerp value) (format "%d" value))
        ((numberp value) (format "%.1f" value))
        (t (format "%s" value))))

(defun org-canvas--quiz-submissions-duration (seconds)
  "Render SECONDS as h:mm, or - when there is no number."
  (if (numberp seconds)
      (let ((s (truncate seconds)))
        (format "%d:%02d" (/ s 3600) (/ (% s 3600) 60)))
    "-"))

(defun org-canvas--quiz-submissions-summary (rows)
  "Return the summary line for ROWS."
  (let* ((open (cl-count-if (lambda (r) (equal (plist-get r :state) "untaken")) rows))
         (retaken (cl-count-if (lambda (r) (and (numberp (plist-get r :attempt))
                                                (> (plist-get r :attempt) 1)))
                               rows))
         (kept (cl-remove-if-not #'numberp (mapcar (lambda (r) (plist-get r :kept)) rows)))
         (possible (cl-find-if #'numberp (mapcar (lambda (r) (plist-get r :possible)) rows)))
         (line (format "%d submissions | %d in progress | %d with more than one attempt"
                       (length rows) open retaken)))
    (if kept
        (concat line (format " | Mean kept score: %.1f"
                             (/ (apply #'+ kept) (float (length kept))))
                (if possible (format " / %s" (org-canvas--quiz-submissions-number possible)) ""))
      line)))

(defun org-canvas--quiz-submissions-insert (quiz rows)
  "Insert the file for QUIZ from ROWS at point, header, summary and table."
  (insert (format "#+TITLE: Quiz submissions: %s\n" (alist-get 'title quiz)))
  (insert (format "#+PROPERTY: QUIZ_ID %s\n" (alist-get 'id quiz)))
  (insert (format "#+PROPERTY: QUIZ_NAME %s\n" (alist-get 'title quiz)))
  (insert (format "#+PROPERTY: PULLED_AT %s\n" (format-time-string "<%Y-%m-%d %a %H:%M>")))
  (when (stringp (alist-get 'html_url quiz))
    (insert (format "\nQuiz: [[%s][Open in Canvas]]\n" (alist-get 'html_url quiz))))
  (insert "\nRead-only: every pull rewrites this file.  A score is changed in the\n"
          "quiz's grading file (org-canvas-pull-submissions on its assignment).\n\n")
  (insert (org-canvas--quiz-submissions-summary rows) "\n\n")
  (insert "| Student | Attempts | Kept | Latest | Out of | Time | Finished | State |\n")
  (insert "|---+---+---+---+---+---+---+---|\n")
  (dolist (row rows)
    (insert (format "| %s | %s | %s | %s | %s | %s | %s | %s |\n"
                    (plist-get row :name)
                    (org-canvas--quiz-submissions-number (plist-get row :attempt))
                    (org-canvas--quiz-submissions-number (plist-get row :kept))
                    (org-canvas--quiz-submissions-number (plist-get row :latest))
                    (org-canvas--quiz-submissions-number (plist-get row :possible))
                    (org-canvas--quiz-submissions-duration (plist-get row :time))
                    (or (org-canvas--iso8601-to-org-timestamp (plist-get row :finished)) "-")
                    (plist-get row :state))))
  (forward-line -1)
  (when (org-at-table-p) (org-table-align)))

(defun org-canvas--quiz-submissions-file-path (quiz)
  "Return the file path for QUIZ under the submissions directory.
The name is the quiz's title sanitised as grading files are, with
\" (quiz)\" appended so it never collides with the shadow assignment's
grading file."
  (expand-file-name
   (format "%s (quiz).org" (org-canvas--submissions-sanitize-filename
                            (or (alist-get 'title quiz) "quiz")))
   (org-canvas--submissions-ensure-directory)))

(defun org-canvas--quiz-submissions-write (quiz rows)
  "Write ROWS for QUIZ to its file and return the buffer.
The buffer is left read-only: the table is derived from Canvas and a
pull rewrites it."
  (let ((file (org-canvas--quiz-submissions-file-path quiz)))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-canvas--quiz-submissions-insert quiz rows)
        (org-canvas--save-buffer))
      (setq buffer-read-only t)
      (current-buffer))))

;;;; Entry Point

(defun org-canvas--quiz-submissions-choose (quizzes)
  "Ask which of QUIZZES to pull and return it."
  (unless quizzes
    (user-error "This course has no classic quizzes"))
  (let* ((names (mapcar (lambda (q) (alist-get 'title q)) quizzes))
         (chosen (completing-read "Quiz: " names nil t)))
    (cl-find-if (lambda (q) (equal (alist-get 'title q) chosen)) quizzes)))

(defun org-canvas--quiz-submissions-pull (quiz)
  "Pull QUIZ's submissions into its file and return the buffer."
  (let* ((fetched (org-canvas--quiz-submissions-fetch (alist-get 'id quiz)))
         (rows (org-canvas--quiz-submissions-rows (plist-get fetched :submissions)
                                                  (plist-get fetched :users)))
         (buffer (org-canvas--quiz-submissions-write quiz rows)))
    (org-canvas--log-info org-canvas--logger
      "Quiz submissions pulled for '%s': %d students" (alist-get 'title quiz) (length rows))
    (message "Quiz submissions for %s: %s" (alist-get 'title quiz)
             (org-canvas--quiz-submissions-summary rows))
    buffer))

;;;###autoload
(defun org-canvas-pull-quiz-submissions ()
  "Select a classic quiz and pull its attempts into a read-only table.
The file is written under the submissions directory as <quiz> (quiz).org,
one row per student with attempts, kept and latest score, time spent,
when the attempt finished and its state.  Nothing is pushed from it:
a quiz score is changed through the quiz's grading file."
  (interactive)
  (let* ((quiz (org-canvas--quiz-submissions-choose
                (org-canvas--quiz-submissions-fetch-quizzes)))
         (buffer (org-canvas--quiz-submissions-pull quiz)))
    (unless noninteractive
      (pop-to-buffer buffer))))

(provide 'org-canvas-quiz-submissions)
;;; org-canvas-quiz-submissions.el ends here
