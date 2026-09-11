;;; org-canvas-peer-reviews-test.el --- Buttercup tests for the peer reviews pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-peer-reviews': an assignment's peer reviews
;; folded into two read-only tables under the submissions directory.
;; Every request is answered by a fake keyed on the URL; nothing here
;; reaches the network (Hard Rule 2), and the log is never read from the
;; shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defvar test-peer--assignments nil
  "Assignments the fake API lists for the course.")
(defvar test-peer--reviews nil
  "Peer reviews the fake API lists for any assignment.")
(defvar test-peer--enrollments nil
  "Student enrollments the fake API lists for the course.")
(defvar test-peer--calls nil
  "Requests the fake API answered, newest first, as (URL . PARAMS).")

(defun test-peer--assignment (id name &optional no-peer-reviews)
  "An assignment ID named NAME with peer reviews on unless NO-PEER-REVIEWS."
  `((id . ,id) (name . ,name)
    (peer_reviews . ,(if no-peer-reviews :json-false t))
    (html_url . ,(format "https://canvas.example.com/courses/1/assignments/%d" id))))

(defun test-peer--review (reviewer student state &rest extra)
  "A peer review by REVIEWER of STUDENT in STATE, plus EXTRA pairs.
EXTRA comes first, so an explicit `user', `assessor' or comment list
shadows the default."
  (append extra
          `((id . ,(+ 700 (* 10 reviewer) student))
            (user_id . ,student) (assessor_id . ,reviewer)
            (asset_id . ,(+ 900 student)) (asset_type . "Submission")
            (workflow_state . ,state)
            (submission_comments . []))))

(defun test-peer--comment (author at)
  "A submission comment by AUTHOR created AT."
  `((id . ,(random 10000)) (author_id . ,author) (created_at . ,at) (comment . "x")))

(defun test-peer--enrollment (uid name)
  "An active student enrollment of user UID named NAME."
  `((id . ,(+ 1000 uid)) (user_id . ,uid) (type . "StudentEnrollment")
    (enrollment_state . "active")
    (user . ((id . ,uid) (name . ,name) (sortable_name . ,name)))))

(defun test-peer--api (_method url &optional params)
  "Answer URL from the fake tables, recording PARAMS."
  (push (cons url params) test-peer--calls)
  (cond
   ((string-match-p "/peer_reviews\\'" url) (vconcat test-peer--reviews))
   ((string-match-p "/assignments\\'" url) (vconcat test-peer--assignments))
   ((string-match-p "/enrollments\\'" url) (vconcat test-peer--enrollments))
   (t (error "Unexpected request: %s" url))))

(defmacro test-peer--with-course (assignments reviews enrollments &rest body)
  "Run BODY with the fake API serving ASSIGNMENTS, REVIEWS and ENROLLMENTS.
The submissions directory is a temp directory."
  (declare (indent 3))
  `(let* ((dir (make-temp-file "peer-reviews-" t))
          (org-canvas-submissions-directory dir)
          (org-canvas-submissions-write-gitignore t)
          (test-peer--assignments ,assignments)
          (test-peer--reviews ,reviews)
          (test-peer--enrollments ,enrollments)
          (test-peer--calls nil))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-peer--api)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p dir (buffer-file-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun test-peer--file (name)
  "Return the text of NAME under the submissions directory."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name org-canvas-submissions-directory))
    (buffer-string)))

(defun test-peer--rows (first-cell text)
  "Return every table row of TEXT whose first cell is FIRST-CELL, cells trimmed.
The match's end and the cells are read before `split-string' and
`string-trim' run, since both clobber the match data: read afterwards,
`match-end' answered the trimmed cell's and the loop re-matched the
same row forever.  The loop is also bounded by the number of lines."
  (let ((rows nil) (start 0)
        (limit (length (split-string text "\n")))
        (regexp (format "^| *%s *|\\(.*\\)$" (regexp-quote first-cell))))
    (while (and (> limit 0) (string-match regexp text start))
      (let ((end (match-end 0))
            (cells (match-string 1 text)))
        (push (mapcar #'string-trim (split-string cells "|" t)) rows)
        (setq start end limit (1- limit))))
    (nreverse rows)))

(defconst test-peer--roster
  (list (test-peer--enrollment 1 "Adams, Alice")
        (test-peer--enrollment 2 "Beta, Bob")
        (test-peer--enrollment 3 "Cruz, Cal"))
  "Three active students.")

(describe "test-peer--rows"
  (it "collects every row of the name once and stops, match data clobbered or not"
    ;; The first version read `match-end' after `split-string' had run
    ;; and re-matched the same row until the process was killed.
    (let ((text "| Adams, Alice | x | 1 |\n| Beta, Bob | y | 2 |\n| Adams, Alice | 3 | 4 |\n"))
      (expect (test-peer--rows "Adams, Alice" text)
              :to-equal '(("x" "1") ("3" "4")))
      (expect (test-peer--rows "Cruz, Cal" text) :to-be nil))))

(describe "org-canvas--peer-reviews-fetch-assignments"
  (it "keeps the assignments with peer reviews on, sorted by name"
    (test-peer--with-course
        (list (test-peer--assignment 12 "Essay 2")
              (test-peer--assignment 11 "Essay 1")
              (test-peer--assignment 13 "Quiz" t))
        nil nil
      (expect (mapcar (lambda (a) (alist-get 'name a))
                      (org-canvas--peer-reviews-fetch-assignments))
              :to-equal '("Essay 1" "Essay 2")))))

(describe "org-canvas--peer-reviews-fetch"
  (it "asks for the users and the submission comments"
    (test-peer--with-course nil (list (test-peer--review 1 2 "assigned")) nil
      (expect (length (org-canvas--peer-reviews-fetch 11)) :to-equal 1)
      (let ((call (car test-peer--calls)))
        (expect (car call) :to-match "/assignments/11/peer_reviews\\'")
        (expect (cdr call) :to-equal '(("include[]" . "user")
                                       ("include[]" . "submission_comments")))))))

(describe "org-canvas--peer-reviews-fetch-students"
  (it "lists each active student once by sortable name"
    (test-peer--with-course nil nil
        (append test-peer--roster (list (test-peer--enrollment 1 "Adams, Alice")))
      (expect (org-canvas--peer-reviews-fetch-students)
              :to-equal '((1 . "Adams, Alice") (2 . "Beta, Bob") (3 . "Cruz, Cal")))
      (let ((call (car test-peer--calls)))
        (expect (car call) :to-match "/enrollments\\'")
        (expect (cdr call) :to-equal '(("type[]" . "StudentEnrollment")
                                       ("state[]" . "active")))))))

(describe "org-canvas--peer-reviews-completed-at"
  (it "dates the completion by the reviewer's newest comment and ignores others"
    (let ((review (test-peer--review
                   1 2 "completed"
                   `(submission_comments . ,(vector (test-peer--comment 1 "2026-09-03T10:00:00Z")
                                                    (test-peer--comment 9 "2026-09-09T10:00:00Z")
                                                    (test-peer--comment 1 "2026-09-05T10:00:00Z"))))))
      (expect (org-canvas--peer-reviews-completed-at review) :to-equal "2026-09-05T10:00:00Z")))

  (it "answers nil without a comment by the reviewer"
    (expect (org-canvas--peer-reviews-completed-at (test-peer--review 1 2 "assigned")) :to-be nil)))

(describe "org-canvas--peer-reviews-rows"
  (it "names both sides from the roster, then the include, then the id, sorted by reviewer"
    (let ((rows (org-canvas--peer-reviews-rows
                 (list (test-peer--review 2 1 "assigned")
                       (test-peer--review 1 2 "completed")
                       (test-peer--review 1 7 "assigned" '(user . ((id . 7) (display_name . "Zed, Zoe"))))
                       (test-peer--review 8 1 "assigned"))
                 '((1 . "Adams, Alice") (2 . "Beta, Bob")))))
      (expect (mapcar (lambda (r) (list (plist-get r :reviewer) (plist-get r :student))) rows)
              :to-equal '(("Adams, Alice" "Beta, Bob") ("Adams, Alice" "Zed, Zoe")
                          ("Beta, Bob" "Adams, Alice") ("User 8" "Adams, Alice")))
      (expect (plist-get (car rows) :completed) :to-be t)
      (expect (plist-get (nth 2 rows) :completed) :to-be nil))))

(describe "org-canvas--peer-reviews-student-rows"
  (it "gives every roster student a row and counts assigned and received reviews"
    (let* ((rows (org-canvas--peer-reviews-rows
                  (list (test-peer--review 1 2 "completed")
                        (test-peer--review 3 2 "assigned")
                        (test-peer--review 2 9 "assigned"))
                  '((1 . "Adams, Alice") (2 . "Beta, Bob") (3 . "Cruz, Cal"))))
           (students (org-canvas--peer-reviews-student-rows
                      rows '((1 . "Adams, Alice") (2 . "Beta, Bob") (3 . "Cruz, Cal")))))
      (expect (mapcar (lambda (s) (list (plist-get s :name) (plist-get s :assigned) (plist-get s :received)))
                      students)
              :to-equal '(("Adams, Alice" 0 0) ("Beta, Bob" 2 1) ("Cruz, Cal" 0 0) ("User 9" 1 0))))))

(describe "org-canvas--peer-reviews-summary"
  (it "counts reviews, completions, unreviewed students and idle reviewers"
    (let* ((roster '((1 . "Adams, Alice") (2 . "Beta, Bob") (3 . "Cruz, Cal")))
           (rows (org-canvas--peer-reviews-rows
                  (list (test-peer--review 1 2 "completed")
                        (test-peer--review 2 1 "assigned")
                        (test-peer--review 3 1 "assigned"))
                  roster))
           (students (org-canvas--peer-reviews-student-rows rows roster)))
      (expect (org-canvas--peer-reviews-summary rows students)
              :to-equal "3 reviews assigned | 1 completed | 1 students with no reviewer | 2 students who reviewed nobody"))))

(describe "org-canvas-pull-peer-reviews"
  (it "prompts for an assignment and writes both read-only tables under the submissions directory"
    (test-peer--with-course
        (list (test-peer--assignment 11 "Essay 1") (test-peer--assignment 12 "Essay 2"))
        (list (test-peer--review 2 1 "completed"
                                 `(submission_comments . ,(vector (test-peer--comment 2 "2026-09-05T14:30:00Z"))))
              (test-peer--review 1 2 "assigned"))
        test-peer--roster
      (let ((asked nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt names &rest _)
                     (setq asked (cons prompt names))
                     "Essay 1")))
          (org-canvas-pull-peer-reviews))
        (expect (car asked) :to-equal "Assignment: ")
        (expect (cdr asked) :to-equal '("Essay 1" "Essay 2")))
      (expect (cl-some (lambda (c) (string-match-p "/assignments/11/peer_reviews\\'" (car c)))
                       test-peer--calls)
              :to-be-truthy)
      (let* ((file "Essay_1 (peer reviews).org")
             (text (test-peer--file file)))
        (expect text :to-match "^#\\+TITLE: Peer reviews: Essay 1\n")
        (expect text :to-match "^#\\+PROPERTY: ASSIGNMENT_ID 11\n")
        (expect text :to-match "^#\\+PROPERTY: PULLED_AT <")
        (expect text :to-match "^Assignment: \\[\\[https://canvas.example.com/courses/1/assignments/11\\]\\[Open in Canvas\\]\\]")
        (expect text :to-match "^2 reviews assigned | 1 completed | 1 students with no reviewer | 2 students who reviewed nobody$")
        (expect text :to-match "^| Reviewer +| Reviews +| State +| Completed at +|$")
        (expect (test-peer--rows "Adams, Alice" text)
                :to-equal '(("Beta, Bob" "assigned" "-")
                            ("1" "1")))
        (expect (test-peer--rows "Beta, Bob" text)
                :to-equal '(("Adams, Alice" "completed" "<2026-09-05 Sat 14:30>")
                            ("1" "0")))
        (expect text :to-match "^| Student +| Reviewers assigned +| Reviews received +|$")
        (expect (test-peer--rows "Cruz, Cal" text) :to-equal '(("0" "0")))
        (expect (file-exists-p (expand-file-name ".gitignore" org-canvas-submissions-directory))
                :to-be-truthy)
        (let ((buf (find-buffer-visiting (expand-file-name file org-canvas-submissions-directory))))
          (expect buf :to-be-truthy)
          (expect (buffer-local-value 'buffer-read-only buf) :to-be-truthy)))))

  (it "rewrites the file on a re-pull"
    (test-peer--with-course
        (list (test-peer--assignment 11 "Essay 1"))
        (list (test-peer--review 1 2 "assigned"))
        test-peer--roster
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Essay 1")))
        (org-canvas-pull-peer-reviews)
        (setq test-peer--reviews (list (test-peer--review 1 2 "completed")))
        (org-canvas-pull-peer-reviews))
      (let ((text (test-peer--file "Essay_1 (peer reviews).org")))
        (expect (car (test-peer--rows "Adams, Alice" text)) :to-equal '("Beta, Bob" "completed" "-"))
        (expect text :to-match "^1 reviews assigned | 1 completed")
        (expect (length (split-string text "^#\\+TITLE:" t)) :to-equal 1))))

  (it "writes the header, the summary and the roster for an assignment with no review yet"
    (test-peer--with-course
        (list (test-peer--assignment 11 "Essay 1"))
        nil
        test-peer--roster
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Essay 1")))
        (org-canvas-pull-peer-reviews))
      (let ((text (test-peer--file "Essay_1 (peer reviews).org")))
        (expect text :to-match "^0 reviews assigned | 0 completed | 3 students with no reviewer | 3 students who reviewed nobody$")
        (expect text :to-match "^| Reviewer +| Reviews")
        (expect (test-peer--rows "Cruz, Cal" text) :to-equal '(("0" "0"))))))

  (it "shows the file in an interactive session and not in batch"
    (test-peer--with-course
        (list (test-peer--assignment 11 "Essay 1"))
        nil nil
      (let ((shown nil))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Essay 1"))
                  ((symbol-function 'pop-to-buffer) (lambda (buf &rest _) (push buf shown))))
          (let ((noninteractive t))
            (org-canvas-pull-peer-reviews))
          (expect shown :to-be nil)
          (let ((noninteractive nil))
            (org-canvas-pull-peer-reviews))
          (expect (length shown) :to-equal 1)
          (expect (buffer-file-name (car shown)) :to-match "Essay_1 (peer reviews).org\\'")))))

  (it "refuses a course where no assignment has peer reviews"
    (test-peer--with-course (list (test-peer--assignment 13 "Quiz" t)) nil nil
      (expect (org-canvas-pull-peer-reviews) :to-throw 'user-error)))

  (it "is not in the pull tiers"
    (expect (memq 'org-canvas-pull-peer-reviews
                  (mapcar #'car (apply #'append org-canvas--pull-tiers)))
            :to-be nil))

  (it "is bound in the transient's pull and submissions groups"
    (let ((source (with-temp-buffer
                    (insert-file-contents (locate-library "org-canvas-transient.el"))
                    (buffer-string))))
      (expect (let ((n 0) (start 0))
                (while (string-match "org-canvas-pull-peer-reviews" source start)
                  (setq n (1+ n) start (match-end 0)))
                n)
              :to-equal 2))))

(provide 'org-canvas-peer-reviews-test)
;;; org-canvas-peer-reviews-test.el ends here
