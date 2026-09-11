;;; org-canvas-quiz-submissions-test.el --- Buttercup tests for the quiz attempts pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-quiz-submissions': a classic quiz's submissions
;; folded into one read-only table under the submissions directory.
;; Every request is answered by a fake keyed on the URL; nothing here
;; reaches the network (Hard Rule 2), and the log is never read from the
;; shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defvar test-quiz-subs--quizzes nil
  "Quizzes the fake API lists for the course.")
(defvar test-quiz-subs--pages nil
  "Replies to the submissions endpoint, one per page, consumed in order.
Each is a decoded alist or a `plz-response' carrying one as JSON.")
(defvar test-quiz-subs--urls nil
  "URLs the fake API was asked for, newest first.")

(defun test-quiz-subs--quiz (id title)
  "A quiz ID titled TITLE, as the quizzes endpoint lists it."
  `((id . ,id) (title . ,title) (quiz_type . "assignment")
    (html_url . ,(format "https://canvas.example.com/courses/1/quizzes/%d" id))))

(defun test-quiz-subs--submission (uid attempt kept latest &rest extra)
  "A quiz submission of user UID at ATTEMPT scoring KEPT and LATEST.
EXTRA pairs come first, so an explicit value shadows the default."
  (append extra
          `((id . ,(+ 500 uid)) (user_id . ,uid) (quiz_id . 42)
            (attempt . ,attempt) (kept_score . ,kept) (score . ,latest)
            (quiz_points_possible . 10.0) (time_spent . 754)
            (finished_at . "2026-09-05T14:30:00Z") (workflow_state . "complete"))))

(defun test-quiz-subs--user (uid name)
  "A user UID named NAME, as the include sidecar lists them."
  `((id . ,uid) (name . ,name) (sortable_name . ,name)))

(defun test-quiz-subs--page (submissions users)
  "A decoded reply carrying SUBMISSIONS and USERS as vectors."
  `((quiz_submissions . ,(vconcat submissions)) (users . ,(vconcat users))))

(defun test-quiz-subs--response (page next-url)
  "PAGE as a `plz-response' whose Link header names NEXT-URL, when given."
  (make-plz-response
   :status 200
   :headers (append '((content-type . "application/json"))
                    (when next-url
                      (list (cons 'link (format "<%s>; rel=\"next\"" next-url)))))
   :body (json-encode page)))

(defun test-quiz-subs--api (_method url &rest _args)
  "Answer URL with the next fake page."
  (push url test-quiz-subs--urls)
  (unless (string-match-p "/submissions" url)
    (error "Unexpected request: %s" url))
  (pop test-quiz-subs--pages))

(defun test-quiz-subs--all-pages (_method url &optional _params)
  "Answer the quiz list for URL."
  (push url test-quiz-subs--urls)
  (unless (string-match-p "/quizzes\\'" url)
    (error "Unexpected paged request: %s" url))
  (vconcat test-quiz-subs--quizzes))

(defmacro test-quiz-subs--with-course (quizzes pages &rest body)
  "Run BODY with the fake API serving QUIZZES and the submission PAGES.
The submissions directory is a temp directory."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "quiz-subs-" t))
          (org-canvas-submissions-directory dir)
          (org-canvas-submissions-write-gitignore t)
          (test-quiz-subs--quizzes ,quizzes)
          (test-quiz-subs--pages ,pages)
          (test-quiz-subs--urls nil))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request) #'test-quiz-subs--api)
                     ((symbol-function 'org-canvas-api-request-all-pages)
                      #'test-quiz-subs--all-pages)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p dir (buffer-file-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun test-quiz-subs--file (name)
  "Return the text of NAME under the submissions directory."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name org-canvas-submissions-directory))
    (buffer-string)))

(defun test-quiz-subs--row (name text)
  "Return the table row of student NAME in TEXT, cells trimmed, or nil."
  (when (string-match (format "^| *%s *|\\(.*\\)$" (regexp-quote name)) text)
    (mapcar #'string-trim (split-string (match-string 1 text) "|" t))))

(describe "org-canvas--quiz-submissions-fetch"
  (it "unwraps a headerless reply as the only page"
    (test-quiz-subs--with-course nil
        (list (test-quiz-subs--page (list (test-quiz-subs--submission 1 1 8.0 8.0))
                                    (list (test-quiz-subs--user 1 "Adams, Alice"))))
      (let ((fetched (org-canvas--quiz-submissions-fetch 42)))
        (expect (length (plist-get fetched :submissions)) :to-equal 1)
        (expect (alist-get 'name (car (plist-get fetched :users))) :to-equal "Adams, Alice")
        (expect (car test-quiz-subs--urls) :to-match "/quizzes/42/submissions\\'"))))

  (it "follows the Link header and joins the pages' submissions and users"
    (test-quiz-subs--with-course nil
        (list (test-quiz-subs--response
               (test-quiz-subs--page (list (test-quiz-subs--submission 1 1 8.0 8.0))
                                     (list (test-quiz-subs--user 1 "A")))
               "https://x/api/v1/courses/1/quizzes/42/submissions?page=bookmark2")
              (test-quiz-subs--response
               (test-quiz-subs--page (list (test-quiz-subs--submission 2 1 9.0 9.0))
                                     (list (test-quiz-subs--user 2 "B")))
               nil))
      (let ((fetched (org-canvas--quiz-submissions-fetch 42)))
        (expect (length (plist-get fetched :submissions)) :to-equal 2)
        (expect (length (plist-get fetched :users)) :to-equal 2)
        (expect (car test-quiz-subs--urls) :to-match "page=bookmark2")
        (expect (length test-quiz-subs--urls) :to-equal 2)))))

(describe "org-canvas--quiz-submissions-rows"
  (it "names each row from the sidecar, falls back to the id, and sorts by name"
    (let ((rows (org-canvas--quiz-submissions-rows
                 (list (test-quiz-subs--submission 2 3 9.0 7.5)
                       (test-quiz-subs--submission 1 1 8.0 8.0)
                       (test-quiz-subs--submission 7 1 :null :null
                                                   '(workflow_state . "untaken")
                                                   '(finished_at . :null)
                                                   '(time_spent . :null)))
                 (list (test-quiz-subs--user 1 "Adams, Alice")
                       (test-quiz-subs--user 2 "Beta, Bob")))))
      (expect (mapcar (lambda (r) (plist-get r :name)) rows)
              :to-equal '("Adams, Alice" "Beta, Bob" "User 7"))
      (expect (plist-get (cadr rows) :attempt) :to-equal 3)
      (expect (plist-get (cadr rows) :kept) :to-equal 9.0)
      (expect (plist-get (cadr rows) :latest) :to-equal 7.5)
      (expect (plist-get (cadr rows) :possible) :to-equal 10.0)
      (expect (plist-get (cadr rows) :time) :to-equal 754)
      (expect (plist-get (nth 2 rows) :kept) :to-be nil)
      (expect (plist-get (nth 2 rows) :finished) :to-be nil)
      (expect (plist-get (nth 2 rows) :state) :to-equal "untaken"))))

(describe "org-canvas--quiz-submissions-duration"
  (it "renders seconds as h:mm and a dash for nothing"
    (expect (org-canvas--quiz-submissions-duration 754) :to-equal "0:12")
    (expect (org-canvas--quiz-submissions-duration 3600) :to-equal "1:00")
    (expect (org-canvas--quiz-submissions-duration 5432.7) :to-equal "1:30")
    (expect (org-canvas--quiz-submissions-duration nil) :to-equal "-")))

(describe "org-canvas--quiz-submissions-number"
  (it "renders a score with one decimal, a count as an integer, and a dash"
    (expect (org-canvas--quiz-submissions-number 7.25) :to-equal "7.2")
    (expect (org-canvas--quiz-submissions-number 3) :to-equal "3")
    (expect (org-canvas--quiz-submissions-number nil) :to-equal "-")
    (expect (org-canvas--quiz-submissions-number "x") :to-equal "x")))

(describe "org-canvas--quiz-submissions-summary"
  (it "counts open attempts and retakes and averages the kept scores"
    (let ((rows (org-canvas--quiz-submissions-rows
                 (list (test-quiz-subs--submission 1 3 9.0 7.5)
                       (test-quiz-subs--submission 2 1 7.0 7.0)
                       (test-quiz-subs--submission 3 1 :null :null
                                                   '(workflow_state . "untaken")))
                 nil)))
      (expect (org-canvas--quiz-submissions-summary rows)
              :to-equal "3 submissions | 1 in progress | 1 with more than one attempt | Mean kept score: 8.0 / 10.0")))

  (it "leaves the mean out when no score is kept"
    (expect (org-canvas--quiz-submissions-summary nil)
            :to-equal "0 submissions | 0 in progress | 0 with more than one attempt")))

(describe "org-canvas-pull-quiz-submissions"
  (it "prompts for a quiz and writes its read-only table under the submissions directory"
    (test-quiz-subs--with-course
        (list (test-quiz-subs--quiz 42 "Week 1 Quiz") (test-quiz-subs--quiz 43 "Week 2 Quiz"))
        (list (test-quiz-subs--page
               (list (test-quiz-subs--submission 2 3 9.0 7.5)
                     (test-quiz-subs--submission 1 1 8.0 8.0
                                                 '(time_spent . 3660)
                                                 '(finished_at . :null)
                                                 '(workflow_state . "untaken")))
               (list (test-quiz-subs--user 1 "Adams, Alice")
                     (test-quiz-subs--user 2 "Beta, Bob"))))
      (let ((asked nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt names &rest _)
                     (setq asked (cons prompt names))
                     "Week 1 Quiz")))
          (org-canvas-pull-quiz-submissions))
        (expect (car asked) :to-equal "Quiz: ")
        (expect (cdr asked) :to-equal '("Week 1 Quiz" "Week 2 Quiz")))
      (expect (car test-quiz-subs--urls) :to-match "/quizzes/42/submissions\\'")
      (let* ((file "Week_1_Quiz (quiz).org")
             (text (test-quiz-subs--file file)))
        (expect text :to-match "^#\\+TITLE: Quiz submissions: Week 1 Quiz\n")
        (expect text :to-match "^#\\+PROPERTY: QUIZ_ID 42\n")
        (expect text :to-match "^#\\+PROPERTY: PULLED_AT <")
        (expect text :to-match "^Quiz: \\[\\[https://canvas.example.com/courses/1/quizzes/42\\]\\[Open in Canvas\\]\\]")
        (expect text :to-match "^2 submissions | 1 in progress | 1 with more than one attempt | Mean kept score: 8.5 / 10.0$")
        (expect text :to-match "^| Student +| Attempts +| Kept +| Latest +| Out of +| Time +| Finished +| State +|$")
        (expect (string-match "Adams, Alice" text) :to-be-less-than (string-match "Beta, Bob" text))
        (expect (test-quiz-subs--row "Adams, Alice" text)
                :to-equal '("1" "8.0" "8.0" "10.0" "1:01" "-" "untaken"))
        (expect (test-quiz-subs--row "Beta, Bob" text)
                :to-equal '("3" "9.0" "7.5" "10.0" "0:12" "<2026-09-05 Sat 14:30>" "complete"))
        (expect (file-exists-p (expand-file-name ".gitignore" org-canvas-submissions-directory))
                :to-be-truthy)
        (let ((buf (find-buffer-visiting (expand-file-name file org-canvas-submissions-directory))))
          (expect buf :to-be-truthy)
          (expect (buffer-local-value 'buffer-read-only buf) :to-be-truthy)))))

  (it "rewrites the file on a re-pull"
    (test-quiz-subs--with-course
        (list (test-quiz-subs--quiz 42 "Week 1 Quiz"))
        (list (test-quiz-subs--page (list (test-quiz-subs--submission 1 1 8.0 8.0)
                                          (test-quiz-subs--submission 2 1 6.0 6.0))
                                    (list (test-quiz-subs--user 1 "Adams, Alice")
                                          (test-quiz-subs--user 2 "Beta, Bob")))
              (test-quiz-subs--page (list (test-quiz-subs--submission 1 2 9.5 9.5))
                                    (list (test-quiz-subs--user 1 "Adams, Alice"))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Week 1 Quiz")))
        (org-canvas-pull-quiz-submissions)
        (org-canvas-pull-quiz-submissions))
      (let ((text (test-quiz-subs--file "Week_1_Quiz (quiz).org")))
        (expect (test-quiz-subs--row "Adams, Alice" text)
                :to-equal '("2" "9.5" "9.5" "10.0" "0:12" "<2026-09-05 Sat 14:30>" "complete"))
        (expect text :not :to-match "Beta, Bob")
        (expect (length (split-string text "^#\\+TITLE:" t)) :to-equal 1))))

  (it "writes the header and an empty table for a quiz nobody has taken"
    (test-quiz-subs--with-course
        (list (test-quiz-subs--quiz 42 "Week 1 Quiz"))
        (list (test-quiz-subs--page nil nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Week 1 Quiz")))
        (org-canvas-pull-quiz-submissions))
      (let ((text (test-quiz-subs--file "Week_1_Quiz (quiz).org")))
        (expect text :to-match "^0 submissions | 0 in progress | 0 with more than one attempt$")
        (expect text :to-match "^| Student +| Attempts"))))

  (it "refuses a course with no classic quizzes"
    (test-quiz-subs--with-course nil nil
      (expect (org-canvas-pull-quiz-submissions) :to-throw 'user-error)))

  (it "lists the quizzes sorted by title"
    (test-quiz-subs--with-course
        (list (test-quiz-subs--quiz 43 "Week 2 Quiz") (test-quiz-subs--quiz 42 "Week 1 Quiz"))
        nil
      (expect (mapcar (lambda (q) (alist-get 'title q))
                      (org-canvas--quiz-submissions-fetch-quizzes))
              :to-equal '("Week 1 Quiz" "Week 2 Quiz"))))

  (it "is not in the pull tiers"
    (expect (memq 'org-canvas-pull-quiz-submissions
                  (mapcar #'car (apply #'append org-canvas--pull-tiers)))
            :to-be nil))

  (it "is bound in the transient's pull and submissions groups"
    (let ((source (with-temp-buffer
                    (insert-file-contents (locate-library "org-canvas-transient.el"))
                    (buffer-string))))
      (expect (let ((n 0) (start 0))
                (while (string-match "org-canvas-pull-quiz-submissions" source start)
                  (setq n (1+ n) start (match-end 0)))
                n)
              :to-equal 2))))

(provide 'org-canvas-quiz-submissions-test)
;;; org-canvas-quiz-submissions-test.el ends here
