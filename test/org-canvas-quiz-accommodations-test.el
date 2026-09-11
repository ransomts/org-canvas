;;; org-canvas-quiz-accommodations-test.el --- Buttercup tests for quiz accommodations -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-quiz-accommodations': the accommodations table
;; under a classic quiz, parsed, reconciled against the quiz's
;; submissions and written back on pull.  Every request is answered by
;; a fake keyed on the URL; nothing here reaches the network (Hard Rule
;; 2), and the log is never read from the shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-quiz-accommodations)
;; The pull hook lives in quizzes.el; the whole package loads it.
(require 'org-canvas)

(defvar test-accommodations--submissions nil
  "Submission rows the fake API answers for any quiz.")
(defvar test-accommodations--posts nil
  "Payloads the fake API received on POST, newest first.")

(defun test-accommodations--submission (uid attempts time unlocked)
  "A quiz submission row for user UID with ATTEMPTS, TIME and UNLOCKED."
  `((id . ,(+ 500 uid)) (user_id . ,uid) (quiz_id . 7) (attempt . 1)
    (extra_attempts . ,attempts) (extra_time . ,time)
    (manually_unlocked . ,(if unlocked t :json-false))
    (workflow_state . "complete")))

(defun test-accommodations--api (method url &rest args)
  "Answer URL from the fake tables; record a POST's payload."
  (cond
   ((and (eq method 'GET) (string-match "/quizzes/[0-9]+/submissions" url))
    `((quiz_submissions . ,(vconcat test-accommodations--submissions))))
   ((and (eq method 'POST) (string-match "/quizzes/\\([0-9]+\\)/extensions" url))
    (push (plist-get args :data) test-accommodations--posts)
    '((quiz_extensions . [])))
   (t (error "Unexpected request: %s %s" method url))))

(defmacro test-accommodations--with-course (submissions &rest body)
  "Run BODY with the fake API serving SUBMISSIONS and temp quiz and people files."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "accommodations-" t))
          (org-canvas-quizzes-file (expand-file-name "quizzes.org" dir))
          (org-canvas-people-file (expand-file-name "people.org" dir))
          (test-accommodations--submissions ,submissions)
          (test-accommodations--posts nil))
     (with-temp-file org-canvas-people-file
       (insert "* Students\n** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"
               "** Beta, Bob\n:PROPERTIES:\n:USER_ID: 2\n:END:\n"))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request) #'test-accommodations--api)
                     ((symbol-function 'message) #'ignore)
                     ((symbol-function 'display-buffer) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-quizzes-file org-canvas-people-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf))))
       (delete-directory dir t))))

(defconst test-accommodations--quiz
  "* Syllabus Quiz
:PROPERTIES:
:CANVAS_ID: 7
:END:

#+NAME: accommodations
| Student      | Extra attempts | Extra time | Unlocked |
|--------------+----------------+------------+----------|
| Adams, Alice |              1 |         30 |          |
| #9           |                |         15 | yes      |

** Description
Read the syllabus first.
"
  "One quiz with a two-row accommodations table.")

(defun test-accommodations--posted (uid)
  "Return the extension posted for user UID, as an alist, or nil."
  (cl-some (lambda (payload)
             (let ((ext (aref (alist-get 'quiz_extensions payload) 0)))
               (and (= (alist-get 'user_id ext) uid) ext)))
           test-accommodations--posts))

(describe "org-canvas--accommodation-parse-table"
  (it "resolves a name through people.org and a literal #id, with blanks as zeros"
    (test-accommodations--with-course nil
      (let ((rows (org-canvas--accommodation-parse-table
                   '(("Student" "Extra attempts" "Extra time" "Unlocked") hline
                     ("Adams, Alice" "1" "30" "")
                     ("#9" "" "15" "yes")))))
        (expect rows :to-equal
                '((:user-id "1" :extra-attempts 1 :extra-time 30 :unlocked nil)
                  (:user-id "9" :extra-attempts 0 :extra-time 15 :unlocked t))))))

  (it "finds the columns by header and skips an unresolved student with a warning"
    (test-accommodations--with-course nil
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) warned))))
          (expect (org-canvas--accommodation-parse-table
                   '(("Student" "Unlocked" "Extra time") hline
                     ("Beta, Bob" "x" "20")
                     ("Nobody, Nell" "" "5")))
                  :to-equal '((:user-id "2" :extra-attempts 0 :extra-time 20 :unlocked t))))
        (expect (car warned) :to-match "Nobody, Nell"))))

  (it "refuses a cell that is not a whole number"
    (test-accommodations--with-course nil
      (expect (org-canvas--accommodation-parse-table
               '(("Student" "Extra attempts" "Extra time" "Unlocked") hline
                 ("#9" "one" "" "")))
              :to-throw 'org-canvas-config-error))))

(describe "org-canvas--accommodation-payload"
  (it "sends the three values as absolutes, the unlock as a JSON boolean"
    (let ((payload (org-canvas--accommodation-payload
                    '(:user-id "9" :extra-attempts 0 :extra-time 15 :unlocked nil))))
      (expect (json-encode payload)
              :to-equal "{\"quiz_extensions\":[{\"user_id\":9,\"extra_attempts\":0,\"extra_time\":15,\"manually_unlocked\":false}]}"))))

(describe "org-canvas--accommodation-sync-for-quiz"
  (it "sends a row that differs, skips one that matches, and clears a student without a row"
    (test-accommodations--with-course
        (list (test-accommodations--submission 1 1 30 nil)   ; matches the table
              (test-accommodations--submission 3 0 45 nil)   ; extension, no row
              (test-accommodations--submission 4 0 0 nil))   ; nothing, no row
      (let ((counts (org-canvas--accommodation-sync-for-quiz
                     "7" (org-canvas--accommodation-parse-table
                          '(("Student" "Extra attempts" "Extra time" "Unlocked") hline
                            ("Adams, Alice" "1" "30" "")
                            ("#9" "" "15" "yes"))))))
        (expect counts :to-equal '(1 1))
        (expect (length test-accommodations--posts) :to-equal 2)
        (expect (alist-get 'extra_time (test-accommodations--posted 9)) :to-equal 15)
        (expect (alist-get 'manually_unlocked (test-accommodations--posted 9)) :to-be t)
        (expect (test-accommodations--posted 3)
                :to-equal '((user_id . 3) (extra_attempts . 0) (extra_time . 0)
                            (manually_unlocked . :json-false)))
        (expect (test-accommodations--posted 1) :to-be nil)
        (expect (test-accommodations--posted 4) :to-be nil))))

  (it "sends nothing under the dry run and still counts what it would do"
    (test-accommodations--with-course
        (list (test-accommodations--submission 3 0 45 nil))
      (let ((org-canvas--dry-run t) (logged nil))
        (cl-letf (((symbol-function 'org-canvas--log-info)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
          (expect (org-canvas--accommodation-sync-for-quiz
                   "7" '((:user-id "9" :extra-attempts 0 :extra-time 15 :unlocked nil)))
                  :to-equal '(1 1)))
        (expect test-accommodations--posts :to-be nil)
        (expect (cl-count-if (lambda (l) (string-match-p "\\[DRY-RUN\\] Would set" l)) logged)
                :to-equal 1)
        (expect (cl-count-if (lambda (l) (string-match-p "\\[DRY-RUN\\] Would clear" l)) logged)
                :to-equal 1))))

  (it "logs a failed request and goes on with the next row"
    (test-accommodations--with-course nil
      (let ((errors nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _)
                     (if (eq method 'POST) (error "Boom") (test-accommodations--api method url))))
                  ((symbol-function 'org-canvas--log-error)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) errors))))
          (expect (org-canvas--accommodation-sync-for-quiz
                   "7" '((:user-id "9" :extra-attempts 1 :extra-time 0 :unlocked nil)
                         (:user-id "8" :extra-attempts 2 :extra-time 0 :unlocked nil)))
                  :to-equal '(0 0)))
        (expect (length errors) :to-equal 2)))))

(describe "org-canvas-sync-quiz-accommodations"
  (it "walks every quiz heading with a CANVAS_ID and a table"
    (test-accommodations--with-course nil
      (with-temp-file org-canvas-quizzes-file
        (insert test-accommodations--quiz
                "* Draft Quiz\n\n#+NAME: accommodations\n| Student | Extra time |\n|---+---|\n| #9 | 5 |\n"))
      (let ((stats nil))
        (cl-letf (((symbol-function 'org-canvas--sync-record-feature-stats)
                   (lambda (label counters) (setq stats (cons label counters)))))
          (org-canvas-sync-quiz-accommodations))
        (expect (length test-accommodations--posts) :to-equal 2)
        (expect stats :to-equal '("Quiz Accommodations" :success 2)))))

  (it "syncs the quiz at point and refuses a heading without a table"
    (test-accommodations--with-course nil
      (with-temp-file org-canvas-quizzes-file
        (insert test-accommodations--quiz "* Bare Quiz\n:PROPERTIES:\n:CANVAS_ID: 8\n:END:\n"))
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-quizzes-file)
        (goto-char (point-min))
        (search-forward "Read the syllabus")
        (org-canvas-sync-quiz-accommodations-at-point)
        (expect (length test-accommodations--posts) :to-equal 2)
        (search-forward "Bare Quiz")
        (expect (org-canvas-sync-quiz-accommodations-at-point) :to-throw 'user-error)))))

(describe "the quiz pull writes the accommodations table"
  (it "emits the students who carry an extension, names through people.org, drops empty columns"
    (test-accommodations--with-course
        (list (test-accommodations--submission 1 0 30 nil)
              (test-accommodations--submission 2 0 0 nil)
              (test-accommodations--submission 9 2 0 t))
      (with-temp-file org-canvas-quizzes-file
        (insert "* Syllabus Quiz\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n\n** Description\nIntro.\n"))
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-quizzes-file)
        (goto-char (point-min))
        (org-canvas--accommodation-write-table 7)
        (let ((text (buffer-string)))
          (expect text :to-match "^#\\+NAME: accommodations\n| Student +| Extra attempts +| Extra time +| Unlocked +|\n")
          (expect text :to-match "^| Adams, Alice +| +| 30 +| +|$")
          (expect text :to-match "^| #9 +| 2 +| +| yes +|$")
          (expect text :not :to-match "Beta, Bob")
          (expect (string-match "accommodations" text)
                  :to-be-less-than (string-match "\\*\\* Description" text))))))

  (it "replaces a stale table, and removes it when nobody has an extension"
    (test-accommodations--with-course nil
      (with-temp-file org-canvas-quizzes-file (insert test-accommodations--quiz))
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-quizzes-file)
        (goto-char (point-min))
        (org-canvas--accommodation-write-table 7)
        (expect (buffer-string) :not :to-match "accommodations")
        (expect (buffer-string) :to-match "\\*\\* Description\nRead the syllabus first")
        (setq test-accommodations--submissions
              (list (test-accommodations--submission 2 0 10 nil)))
        (org-canvas--accommodation-write-table 7)
        (org-canvas--accommodation-write-table 7)
        (expect (cl-count-if (lambda (l) (string-prefix-p "#+NAME: accommodations" l))
                             (split-string (buffer-string) "\n"))
                :to-equal 1)
        (expect (buffer-string) :to-match "^| Beta, Bob +| 10 +|$"))))

  (it "runs from org-canvas-pull-quizzes and keeps the table out of the description"
    (test-accommodations--with-course
        (list (test-accommodations--submission 9 0 20 nil))
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method url &optional _params)
                   ;; A stub of the paginated helper answers the rows
                   ;; themselves; the unwrap happens inside the real one.
                   (cond ((string-match "/submissions" url) test-accommodations--submissions)
                         ((string-match "/questions" url) nil)
                         (t '(((id . 7) (title . "Syllabus Quiz") (quiz_type . "assignment")))))))
                ((symbol-function 'org-canvas--html-to-org) #'identity))
        (org-canvas-pull-quizzes)
        (with-current-buffer (org-canvas--find-file-noselect org-canvas-quizzes-file)
          (expect (buffer-string) :to-match "^| #9 +| 20 +|$")
          (goto-char (point-min))
          (re-search-forward "^\\* Syllabus Quiz")
          (expect (org-canvas--quiz-parse-body-text) :to-equal ""))))))

(describe "org-canvas--strip-named-tables"
  (it "removes the named tables and leaves the rest"
    (expect (org-canvas--strip-named-tables
             "Intro\n\n#+NAME: accommodations\n| a | b |\n|---+---|\n| 1 | 2 |\n\n#+NAME: other\n| x |\nAfter\n"
             '("accommodations" "overrides"))
            :to-equal "Intro\n\n\n#+NAME: other\n| x |\nAfter\n")))

(describe "wiring"
  (it "is in the sync tiers right after quizzes' tier, and in the dry-run command list"
    (let ((names (mapcar #'car (apply #'append org-canvas--sync-tiers))))
      (expect (cl-position 'org-canvas-sync-quizzes names)
              :to-be-less-than (cl-position 'org-canvas-sync-quiz-accommodations names)))
    (expect (commandp 'org-canvas-sync-quiz-accommodations) :to-be-truthy)
    (expect (commandp 'org-canvas-sync-quiz-accommodations-at-point) :to-be-truthy)))

(provide 'org-canvas-quiz-accommodations-test)
;;; org-canvas-quiz-accommodations-test.el ends here
