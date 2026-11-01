;;; org-canvas-submissions-conflict-test.el --- Tests for org-canvas-submissions-conflict -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for resolving grade conflicts.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-conflict)

(describe "org-canvas-submissions-push-grades error path"
  (it "errors when not in submissions mode"
    (with-temp-buffer
      (expect (org-canvas-submissions-push-grades) :to-throw 'user-error))))

;;;; Score Parsing

(describe "org-canvas-submissions-push-grades"
  (it "messages when no changes"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:END:\n")
      (setq-local org-canvas-submissions--current-view 'detail)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--assignment-id "1001")
      (setq-local org-canvas-submissions--data nil)
      (org-canvas-submissions-mode 1)
      (spy-on 'message)
      (org-canvas-submissions-push-grades)
      ;; An unchanged file pushes nothing and says so
      (expect (cl-some (lambda (call)
                         (and (stringp (car call))
                              (string-match-p "Nothing to push" (car call))))
                       (spy-calls-all-args 'message))
              :to-be-truthy)))

  (it "dispatches single change to PUT"
    (with-test-api
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n")
        (setq-local org-canvas-submissions--current-view 'detail)
        (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
        (setq-local org-canvas-submissions--assignment-id "1001")
        (setq-local org-canvas-submissions--data nil)
        (org-canvas-submissions-mode 1)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect-api-called 'PUT "assignments/1001/submissions/5001"))))

  (it "dispatches multiple changes to bulk POST"
    (with-test-api
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:END:\n")
        (setq-local org-canvas-submissions--current-view 'detail)
        (setq-local org-canvas-submissions--original-scores
                    '((5001 . "92") (5002 . "75")))
        (setq-local org-canvas-submissions--assignment-id "1001")
        (setq-local org-canvas-submissions--data nil)
        (org-canvas-submissions-mode 1)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect-api-called 'POST "assignments/1001/submissions/update_grades"))))

  (it "respects confirmation prompt denial"
    (with-test-api
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n")
        (setq-local org-canvas-submissions--current-view 'detail)
        (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
        (setq-local org-canvas-submissions--assignment-id "1001")
        (setq-local org-canvas-submissions--data nil)
        (org-canvas-submissions-mode 1)
        (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil)))
          (org-canvas-submissions-push-grades))
        (expect (test-org-canvas-api-call-count) :to-equal 0))))

  (it "updates original-scores after push"
    (with-test-api
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n")
        (setq-local org-canvas-submissions--current-view 'detail)
        (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
        (setq-local org-canvas-submissions--assignment-id "1001")
        (setq-local org-canvas-submissions--data nil)
        (org-canvas-submissions-mode 1)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect (alist-get 5001 org-canvas-submissions--original-scores)
                :to-equal "95"))))

  (it "handles API error gracefully"
    (with-org-canvas-test-config
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n")
        (setq-local org-canvas-submissions--current-view 'detail)
        (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
        (setq-local org-canvas-submissions--assignment-id "1001")
        (setq-local org-canvas-submissions--data nil)
        (org-canvas-submissions-mode 1)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) (error "Connection failed"))))
          ;; Should not signal — error is caught
          (org-canvas-submissions-push-grades))))))

;;;; Keybinding

(describe "detail edit → push integration"
  (it "full flow: render, edit score, push"
    (with-test-api
      (let ((subs (list (test-org-canvas-make-submission '((score . 92))))))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail "HW" "1001" subs)
          (setq-local org-canvas-submissions--assignment-name "HW")
          (setq-local org-canvas-submissions--assignment-id "1001")
          (setq-local org-canvas-submissions--data subs)
          (setq-local org-canvas-submissions--current-view 'detail)
          (setq-local org-canvas-submissions--original-scores
                      (org-canvas--submissions-snapshot-scores subs))
          (org-canvas-submissions-mode 1)
          ;; Edit the score
          (goto-char (point-min))
          (search-forward ":SCORE: 92")
          (replace-match ":SCORE: 98")
          ;; Push
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades))
          (expect-api-called 'PUT "assignments/1001/submissions/5001")
          ;; Verify snapshot updated
          (expect (alist-get 5001 org-canvas-submissions--original-scores)
                  :to-equal "98"))))))

;;;; Integration — Refresh Resets Scores

(describe "pushing from a reopened grading file"
  (it "recovers the assignment id, pushes, updates CANVAS_SCORE, and saves"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (let ((org-canvas-submissions-check-conflicts nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades)))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
        (expect (buffer-string) :to-match ":CANVAS_SCORE: 95")
        (expect (buffer-modified-p) :to-be nil)
        (expect (with-temp-buffer (insert-file-contents file) (buffer-string))
                :to-match ":CANVAS_SCORE: 95"))))
  (it "pushes nothing when the file needs no push"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n")
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect (test-org-canvas-api-call-count) :to-equal 0)))))

(describe "conflict handling on push"
  (it "skips and marks a heading Canvas has since regraded"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                   (lambda (_id) '((5001 . ("93" 1 nil)))))
                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect (test-org-canvas-api-call-count) :to-equal 0)
        (goto-char (point-min))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 93")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92"))))
  (it "skips a student who resubmitted since the pull"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:END:\n")
        (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                   (lambda (_id) '((5001 . ("92" 2 nil)))))
                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect (test-org-canvas-api-call-count) :to-equal 0)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "attempt: Canvas has 2"))))
  (it "holds back a heading already marked CONFLICT and pushes the clean ones (issue #440)"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:CONFLICT: stale\n:END:\n\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:CANVAS_SCORE: 75\n:END:\n")
        (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                   (lambda (_id) '((5001 . ("92" nil nil)) (5002 . ("75" nil nil)))))
                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect-api-called 'PUT "assignments/1001/submissions/5002")
        (expect (test-org-canvas-api-call-count) :to-equal 1)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "stale")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92")
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "80"))))
  (it "does not consult Canvas for an ephemeral buffer"
    (with-test-api
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (setq-local org-canvas-submissions--current-view 'detail)
        (setq-local org-canvas-submissions--assignment-id "1001")
        (org-canvas-submissions-mode 1)
        (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                   (lambda (_id) (error "should not be called")))
                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")))))

(describe "pushing drafted comments"
  (it "posts each draft by user id, records it under Comments, and resets the draft"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n\n** Comment to post\n"
                                 org-canvas-submissions-comment-template "\nStrong opening.\n")
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
        (let ((data (nth 2 (test-org-canvas-last-api-call))))
          (expect (alist-get 'text_comment (alist-get 'comment data)) :to-equal "Strong opening."))
        (expect (buffer-string) :to-match "^\\*\\* Comments\n- \\*You\\* <[^>]+> :: Strong opening\\.\n")
        (expect (string-match "\\*\\* Comments" (buffer-string))
                :to-be-less-than (string-match "\\*\\* Comment to post" (buffer-string)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-canvas--submissions-comment-draft) :to-be nil)
        (expect (buffer-modified-p) :to-be nil))))
  (it "posts grades and comments in one confirmation"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n\n** Comment to post\nNice.\n")
        (let ((org-canvas-submissions-check-conflicts nil) (prompt nil))
          (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t)))
            (org-canvas-submissions-push-grades))
          (expect prompt :to-match "1 grade change(s) and 1 comment(s)")
          (expect (test-org-canvas-api-call-count) :to-equal 2)
          (expect (buffer-string) :to-match ":CANVAS_SCORE: 95")
          (expect (buffer-string) :to-match ":: Nice\\.")))))
  (it "carries a draft across a refresh instead of asking about it"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n\n** Comment to post\nA draft.\n")
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                   (lambda (_id) (list (test-org-canvas-make-submission))))
                  ((symbol-function 'org-canvas--submissions-fetch-assignment) (lambda (_id) nil))
                  ((symbol-function 'switch-to-buffer) (lambda (b) b))
                  ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask"))))
          (org-canvas-submissions-refresh))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-canvas--submissions-comment-draft) :to-equal "A draft.")))))

;;;; Excused, DAYS_LATE, Notes, Completion Rule

(describe "org-canvas-submissions-push-grades guards"
  (it "refuses when the buffer carries no assignment id"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n")
      (setq-local org-canvas-submissions--current-view 'detail)
      (setq-local org-canvas-submissions--assignment-id nil)
      (org-canvas-submissions-mode 1)
      (expect (org-canvas-submissions-push-grades) :to-throw 'user-error)))

  (it "names the skipped conflicts in the confirmation prompt"
    (with-test-api
      (with-grading-file
          (concat test-grading-file-header
                  "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n\n"
                  "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:CANVAS_SCORE: 75\n:END:\n")
        (let ((org-canvas-submissions-check-conflicts t)
              (prompt nil))
          (setq-local org-canvas-submissions--current-view 'detail)
          (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                     (lambda (_id) '((5001 . ("92" 1 nil)) (5002 . ("70" 1 nil)))))
                    ((symbol-function 'org-canvas--confirm)
                     (lambda (p) (setq prompt p) nil)))
            (org-canvas-submissions-push-grades))
          (expect prompt :to-match "skipping 1 conflict")
          (expect (test-org-canvas-api-call-count) :to-equal 0)
          ;; The way out lives in the message, not in the property (issue #264).
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CONFLICT") :to-be nil)
          (org-canvas--submissions-goto-user 5002)
          (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 70")))))
  (it "says how to clear a marked conflict when there is nothing else to push"
    (with-test-api
      (with-grading-file
          (concat test-grading-file-header
                  "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (let ((org-canvas-submissions-check-conflicts t) (messages nil))
          (setq-local org-canvas-submissions--current-view 'detail)
          (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                     (lambda (_id) '((5001 . ("93" 1 nil)))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages) nil)))
            (org-canvas-submissions-push-grades))
          (expect (car messages)
                  :to-equal "Nothing to push; 1 conflict(s) not sent (Adams, Alice): resolve each with t (take Canvas's) or k (keep mine)"))))))

(describe "pushing rubric assessments with S"
  (it "sends the rows with the derived grade in one PUT and records the baselines"
    (with-test-api
      (with-rubric-file (concat test-rubric-file-header
                                 (test-rubric-entry "Adams, Alice" 5001 ""
                                                    '(("_7104" "Thesis" 2 2 "Sharp.\n\nWhere to look: deck 02.")
                                                      ("_7105" "Evidence" 3 1.5 nil)
                                                      ("_7106" "Style" 1 nil nil))))
        (let ((org-canvas-submissions-check-conflicts nil) (prompt nil))
          (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t)))
            (org-canvas-submissions-push-grades))
          (expect prompt :to-match "1 grade change(s) (1 with rubric, 1 partly scored)"))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
        (let ((data (nth 2 (test-org-canvas-last-api-call))))
          (expect (alist-get 'posted_grade (alist-get 'submission data)) :to-equal "3.5")
          (expect (alist-get 'rubric_assessment data)
                  :to-equal '((_7104 . ((points . 2) (comments . "Sharp.\n\nWhere to look: deck 02.")))
                              (_7105 . ((points . 1.5))))))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "3.5")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "3.5")
        (expect (org-entry-get (point) "CANVAS_RUBRIC")
                :to-equal (org-canvas--submissions-rubric-digest
                           '(("_7104" "2" "Sharp.\n\nWhere to look: deck 02.") ("_7105" "1.5" nil))))
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil))))
  (it "sends several students through the bulk endpoint, each with its own fields"
    (with-test-api
      (with-rubric-file (concat test-rubric-file-header
                                 (test-rubric-entry "Adams, Alice" 5001 ""
                                                    '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil)
                                                      ("_7106" "Style" 1 1 nil)))
                                 "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 4\n:CANVAS_SCORE: 2\n:END:\n")
        (let ((org-canvas-submissions-check-conflicts nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades)))
        (expect-api-called 'POST "assignments/1001/submissions/update_grades")
        (let* ((data (nth 2 (test-org-canvas-last-api-call)))
               (grade-data (alist-get 'grade_data data))
               (alice (alist-get "5001" grade-data nil nil #'equal))
               (bob (alist-get "5002" grade-data nil nil #'equal)))
          (expect (alist-get 'posted_grade alice) :to-equal "6")
          (expect (length (alist-get 'rubric_assessment alice)) :to-equal 3)
          (expect (alist-get 'posted_grade bob) :to-equal "4")
          (expect (assq 'rubric_assessment bob) :to-be nil)))))
  (it "skips and marks a student whose rubric was assessed on Canvas since the pull"
    (with-test-api
      (with-rubric-file (concat test-rubric-file-header
                                 (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 2\n:CANVAS_SCORE: 2\n"
                                                    '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil))))
        (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                   (lambda (_id) '((5001 . ("2" 1 "canvas-digest")))))
                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect (test-org-canvas-api-call-count) :to-equal 0)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "rubric: assessed on Canvas since the pull"))))
  (it "does not hold a score-only change against a rubric that moved"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 5\n:CANVAS_SCORE: 2\n:END:\n")
        (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                   (lambda (_id) '((5001 . ("2" 1 "canvas-digest")))))
                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
          (org-canvas-submissions-push-grades))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")))))

(defun test-dry-push--file ()
  "Return a grading file holding every kind of change, and a conflicted row."
  (concat (test-sent-only--everything-file)
          "\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 8\n:CANVAS_SCORE: 7\n:END:\n\n"
          "** Comment to post\nBob's note.\n"))

(defun test-dry-push--canvas ()
  "Return the submissions Canvas holds for `test-dry-push--file'.
Alice's score is her baseline; Bob was regraded in SpeedGrader."
  (list (test-org-canvas-make-submission
         (list (cons 'score 3)
               (cons 'submission_comments
                     (vector (test-sent--comment 22 77 "Mine.")))))
        (test-org-canvas-make-submission
         (list (cons 'id 50002) (cons 'user_id 5002) (cons 'score 9)
               (cons 'user (test-org-canvas-make-user
                            '((id . 5002) (sortable_name . "Beta, Bob"))))))))

(defun test-dry-push--run (fn)
  "Call FN, a push of HW.org, under a dry run with Canvas as above.
Return a plist: :result, :calls (every request), :before and :after
\(the file on disk) and :modified (the buffer's flag afterwards)."
  (let (out)
    (with-org-canvas-test-config
      (test-batch-push--with-dir (test-dry-push--file)
        (let ((org-canvas--dry-run t)
              (noninteractive t)
              (calls nil)
              (before (with-temp-buffer
                        (insert-file-contents-literally file)
                        (buffer-string))))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _) (push (list method url) calls) nil))
                    ((symbol-function 'org-canvas--submissions-self-id) (lambda () "77"))
                    ((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                     (lambda (_id) (test-dry-push--canvas)))
                    ((symbol-function 'org-canvas--submissions-fetch-bank)
                     (lambda () '(("4821" . "Show your units."))))
                    ((symbol-function 'org-canvas--graphql-send)
                     (lambda (&rest _) (error "Sent a GraphQL request")))
                    ((symbol-function 'org-canvas--confirm) (lambda (_) t))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Prompted")))
                    ((symbol-function 'org-canvas--user-message) #'ignore)
                    ((symbol-function 'org-canvas--log-info) #'ignore)
                    ((symbol-function 'org-canvas--log-warning) #'ignore))
            (let ((result (funcall fn)))
              (setq out (list :result result :calls calls :before before
                              :after (with-temp-buffer
                                       (insert-file-contents-literally file)
                                       (buffer-string))
                              :modified (buffer-modified-p (find-buffer-visiting file)))))))))
    out))

(describe "a dry run of the grade push (issue #442)"
  (it "sends nothing and leaves the grading file byte for byte as it was"
    (let* ((r (test-dry-push--run
               (lambda () (org-canvas-push-submission-grades "1001" t))))
           (result (plist-get r :result)))
      (expect (plist-get r :calls) :to-be nil)
      (expect (plist-get r :modified) :to-be nil)
      (expect (plist-get r :after) :to-equal (plist-get r :before))
      (expect (plist-get result :state) :to-be 'dry-run)
      (expect (plist-get result :pushed) :to-equal 0)
      (expect (plist-get result :comments) :to-equal 2)
      (expect (plist-get result :conflicts) :to-equal 1)
      (expect (plist-get result :dry-run) :to-equal 1)
      (expect (plist-get result :posted) :to-be nil)))

  (it "keeps every draft, baseline and conflict in the buffer of S"
    (let ((r (test-dry-push--run
              (lambda ()
                (with-current-buffer (org-canvas--submissions-visit-grading-file
                                      (expand-file-name "HW.org" org-canvas-submissions-directory))
                  (setq org-canvas-submissions--current-view 'detail)
                  (org-canvas-submissions-push-grades)
                  (list :drafts (length (org-canvas--submissions-collect-comment-drafts))
                        :bank (length (org-canvas--submissions-bank-pending))
                        :text (buffer-string)))))))
      (expect (plist-get r :modified) :to-be nil)
      (expect (plist-get (plist-get r :result) :drafts) :to-equal 2)
      (expect (plist-get (plist-get r :result) :bank) :to-equal 1)
      (expect (plist-get (plist-get r :result) :text) :not :to-match "CONFLICT")
      (expect (plist-get (plist-get r :result) :text) :not :to-match "\\*You\\*"))))

;;;; A Heading Marked CONFLICT Is Not Pushed (issue #440)

(defun test-conflict--file ()
  "Return a grading file whose Alice is marked CONFLICT over typed rows and a draft."
  (concat test-rubric-file-header
          (test-rubric-entry "Adams, Alice" 5001
                             ":SCORE: 4\n:CANVAS_SCORE: 0\n:SUBMISSION_ID: 9001\n:ATTEMPT: 1\n:LATE_STATUS: late\n:CONFLICT: score: Canvas has 0\n"
                             '(("_7104" "Thesis" 2 2 "Sharp") ("_7105" "Evidence" 3 2 nil)
                               ("_7106" "Style" 1 nil nil)))
          "** Notes\nRead twice.\n\n** Comment to post\nSee me.\n"))

(defun test-conflict--canvas ()
  "Return Alice's submission as Canvas holds it after a SpeedGrader zero."
  (test-org-canvas-make-submission
   '((score . 0) (entered_score . 0) (attempt . 2) (late_policy_status . "missing")
     (rubric_assessment . ((_7104 . ((points . 0) (comments . "Authorship"))))))))

(defun test-conflict--resolve (fn)
  "Call FN, a resolving command, with Canvas answering `test-conflict--canvas'.
Return the requests made, as (METHOD URL)."
  (let ((calls nil))
    (cl-letf (((symbol-function 'org-canvas-api-request)
               (lambda (method url &rest _) (push (list method url) calls)
                 (test-conflict--canvas)))
              ((symbol-function 'message) #'ignore))
      (funcall fn))
    calls))

(describe "a heading marked CONFLICT (issue #440)"
  (it "is not pushed after a refresh puts a stale typed score over a SpeedGrader grade"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student
                                  "Beta, Bob" 5002
                                  ":STATUS: submitted\n:SCORE: 10\n:CANVAS_SCORE: 8\n:ATTEMPT: 1\n"
                                  ":SUBMITTED_AT: <2026-02-15 Sun 23:45>\n")
                                 "\n** Comment to post\nLate, minus two.\n")
        (test-refresh--run (list (test-refresh--bob '((score . 0) (attempt . 1)))))
        (expect (test-refresh--summary) :to-equal
                (concat "Refreshed HW: 1 scored on Canvas since the pull"
                        "; CONFLICT on 1: Beta, Bob (score: Canvas has 0)"))
        (with-mock-api
          (let ((result (cl-letf (((symbol-function 'message) #'ignore))
                          (org-canvas--submissions-push-current nil))))
            (expect (test-org-canvas-api-call-count) :to-equal 0)
            (expect (plist-get result :pushed) :to-equal 0)
            (expect (plist-get result :comments) :to-equal 0)
            (expect (plist-get result :conflicts) :to-equal 1)
            (expect (plist-get result :conflict-names) :to-equal '("Beta, Bob"))))
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "SCORE") :to-equal "10")
        (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 0")
        (expect (org-canvas--submissions-comment-draft) :to-equal "Late, minus two."))))

  (it "names the held heading beside those the push's own check finds"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Adams, Alice" 5001
                                                        ":SCORE: 95\n:CANVAS_SCORE: 92\n:CONFLICT: stale\n")
                                 (test-refresh--student "Beta, Bob" 5002
                                                        ":SCORE: 80\n:CANVAS_SCORE: 75\n"))
        (let ((said nil))
          (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                     (lambda (_id) '((5002 . ("77" nil nil)))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
            (let ((result (org-canvas--submissions-push-current nil)))
              (expect (plist-get result :conflicts) :to-equal 2)
              (expect (plist-get result :conflict-names) :to-equal '("Adams, Alice" "Beta, Bob"))))
          (expect said :to-equal
                  "Nothing to push; 2 conflict(s) not sent (Adams, Alice; Beta, Bob): resolve each with t (take Canvas's) or k (keep mine)")
          (expect (test-org-canvas-api-call-count) :to-equal 0)
          (org-canvas--submissions-goto-user 5002)
          (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 77")))))

  (it "takes Canvas's grade with t, keeping what was typed under Notes"
    (with-org-canvas-test-config
      (with-rubric-file (test-conflict--file)
        (org-canvas--submissions-goto-user 5001)
        (forward-line 3)
        (let ((calls (test-conflict--resolve #'org-canvas-submissions-take-canvas)))
          (expect calls :to-equal
                  (list (list 'GET (org-canvas-api-course-endpoint
                                    "assignments/%s/submissions/%s" "1001" "5001")))))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (org-entry-get (point) "SCORE") :to-equal "0")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "0")
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "missing")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
        (expect (org-entry-get (point) "ATTEMPT") :to-equal "2")
        (expect (mapcar (lambda (r) (list (nth 0 r) (nth 3 r) (nth 4 r)))
                        (org-canvas--submissions-rubric-rows))
                :to-equal '(("_7104" "0" "Authorship") ("_7105" nil nil) ("_7106" nil nil)))
        (expect (org-canvas--submissions-section-text org-canvas--submissions-notes-heading)
                :to-match "\\`Read twice\\.\n- Typed before taking Canvas's grade <[^>]+>: SCORE 4; LATE_STATUS late; Rubric _7104 2 (Sharp), _7105 2\\'")
        (expect (org-canvas--submissions-comment-draft) :to-equal "See me.")
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
        (expect (buffer-modified-p) :to-be nil))))

  (it "takes Canvas's absence of a grade too, from anywhere in the heading"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student
                                  "Adams, Alice" 5001
                                  ":SCORE: 7\n:CANVAS_SCORE: 5\n:LATE_STATUS: late\n:CONFLICT: score: Canvas has no grade\n")
                                 "** Comment to post\nSee me.")
        (re-search-forward "^\\*\\* Comment to post")
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) (test-org-canvas-make-submission
                                      '((score . nil) (entered_score . nil)))))
                  ((symbol-function 'message) #'ignore))
          (expect (org-canvas-submissions-take-canvas) :to-equal "Adams, Alice"))
        (org-canvas--submissions-goto-user 5001)
        (expect (mapcar (lambda (p) (org-entry-get (point) p))
                        '("SCORE" "CANVAS_SCORE" "LATE_STATUS" "CANVAS_LATE_STATUS" "CONFLICT"))
                :to-equal '(nil nil nil nil nil))
        (expect (buffer-string) :to-match
                "\\*\\* Notes\n- Typed before taking Canvas's grade <[^>]+>: SCORE 7; LATE_STATUS late\n\\'"))))

  (it "keeps the typed grade with k, against Canvas's as its new baseline"
    (with-org-canvas-test-config
      (with-rubric-file (test-conflict--file)
        (org-canvas--submissions-goto-user 5001)
        (test-conflict--resolve #'org-canvas-submissions-keep-mine)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (org-entry-get (point) "SCORE") :to-equal "4")
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "late")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
        (expect (org-entry-get (point) "CANVAS_RUBRIC") :to-equal
                (org-canvas--submissions-submission-rubric-digest (test-conflict--canvas)))
        (expect (org-canvas--submissions-section-text org-canvas--submissions-notes-heading)
                :to-equal "Read twice.")
        (let* ((change (car (org-canvas--submissions-collect-grade-changes)))
               (old (plist-get change :old-score))
               (new (plist-get change :new-score)))
          (expect (list old new) :to-equal '("0" "4")))
        ;; Canvas still holds what k read, so the push's check passes it.
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                   (lambda (_id) (list (test-conflict--canvas)))))
          (expect (cdr (org-canvas--submissions-partition-conflicts
                        "1001" (org-canvas--submissions-collect-grade-changes)))
                  :to-be nil)))))

  (it "refuses a heading without CONFLICT, the summary view, and other buffers"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Adams, Alice" 5001 ":SCORE: 4\n"))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-canvas-submissions-keep-mine) :to-throw 'user-error)
        (setq-local org-canvas-submissions--current-view 'summary)
        (cl-letf (((symbol-function 'org-canvas--submissions-ensure-context) #'ignore))
          (expect (org-canvas-submissions-take-canvas) :to-throw 'user-error))))
    (with-temp-buffer
      (expect (org-canvas-submissions-take-canvas) :to-throw 'user-error)))

  (it "binds t and k in the grading mode"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "t"))
            :to-be #'org-canvas-submissions-take-canvas)
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "k"))
            :to-be #'org-canvas-submissions-keep-mine)))

;;;; Pushing Some Rows (issue #441)

(defconst test-only--file
  (concat test-grading-file-header
          "* Comment Bank\n- Show your units.\n\n"
          "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n\n"
          "** Comment to post\nSee me.\n\n"
          "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:CANVAS_SCORE: 75\n:END:\n\n"
          "** Comment to post\nBob's note.\n\n"
          "* Cole, Cy\n:PROPERTIES:\n:USER_ID: 5003\n:SCORE: 70\n:CANVAS_SCORE: 70\n:END:\n")
  "A grading file where Alice and Bob have a score and a draft, Cy nothing.")

(defun test-only--push (fn)
  "Call FN against HW.org holding `test-only--file', unvisited.
Return a plist: :result, :calls (METHOD URL), :mutations, :said (the
closing message), :asked (the question) and :saved (the file after)."
  (let (out)
    (with-org-canvas-test-config
      (test-batch-push--with-dir test-only--file
        (let ((org-canvas-submissions-check-conflicts nil)
              (noninteractive t)
              (mutations nil) (said nil) (asked nil))
          (test-progress--with-replies nil
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Prompted")))
                      ((symbol-function 'org-canvas--confirm) (lambda (q) (setq asked q) t))
                      ((symbol-function 'org-canvas--graphql-mutate)
                       (lambda (&rest args) (push args mutations) nil))
                      ((symbol-function 'org-canvas--log-info) #'ignore)
                      ((symbol-function 'message) #'ignore)
                      ((symbol-function 'org-canvas--user-message)
                       (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
              (let ((result (funcall fn file)))
                (setq out (list :result result :calls (reverse calls) :mutations mutations
                                :said said :asked asked
                                :saved (with-temp-buffer
                                         (insert-file-contents file)
                                         (buffer-string))))))))))
    out))

(describe "pushing some rows of a grading file (issue #441)"
  (it "pushes the heading at point with s: its score and draft, nothing else"
    (let* ((r (test-only--push
               (lambda (file)
                 (with-current-buffer (org-canvas--submissions-visit-grading-file file)
                   (setq org-canvas-submissions--current-view 'detail)
                   (org-canvas--submissions-goto-user 5002)
                   (re-search-forward "^\\*\\* Comment to post")
                   (org-canvas-submissions-push-at-point)))))
           (result (plist-get r :result))
           (saved (plist-get r :saved)))
      (expect (plist-get r :calls) :to-equal
              (list (list 'PUT (org-canvas-api-course-endpoint
                                "assignments/%s/submissions/%s" "1001" 5002))
                    (list 'PUT (org-canvas-api-course-endpoint
                                "assignments/%s/submissions/%s" "1001" 5002))))
      (expect (plist-get r :mutations) :to-be nil)
      (expect (plist-get r :asked) :to-equal "Push 1 grade change(s) and 1 comment(s)? ")
      (expect (plist-get result :pushed) :to-equal 1)
      (expect (plist-get result :comments) :to-equal 1)
      (expect saved :to-match ":CANVAS_SCORE: 80")
      (expect saved :to-match ":CANVAS_SCORE: 92")
      (expect saved :to-match "^\\*\\* Comment to post\nSee me\\.$")
      (expect saved :not :to-match "Bob's note\\.\n\n\\* Cole")
      (expect saved :to-match "^- Show your units\\.$")))

  (it "pushes the rows a script names, and names the ids it sent nothing for"
    (let* ((r (test-only--push
               (lambda (_file)
                 (org-canvas-push-submission-grades "1001" nil '("5001" 9999 5003)))))
           (result (plist-get r :result)))
      (expect (mapcar #'car (plist-get r :calls)) :to-equal '(PUT PUT))
      (expect (cl-every (lambda (c) (string-suffix-p "/submissions/5001" (cadr c)))
                        (plist-get r :calls))
              :to-be t)
      (expect (plist-get result :missing) :to-equal '(9999))
      (expect (plist-get result :unchanged) :to-equal '(5003))
      (expect (plist-get r :said) :to-match
              "Pushed 1 grade(s) and 1 comment(s); no row for 9999; nothing to send for 5003\\'")
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 95")
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 75")))

  (it "says per row what a dry run would send, and sends and writes nothing"
    (let* ((before test-only--file)
           (r (let ((org-canvas--dry-run t))
                (test-only--push
                 (lambda (_file) (org-canvas-push-submission-grades 1001 nil '(5001 5002))))))
           (result (plist-get r :result)))
      (expect (plist-get r :calls) :to-be nil)
      (expect (plist-get r :saved) :to-equal before)
      (expect (mapcar (lambda (row) (plist-get row :line)) (plist-get result :would-send))
              :to-equal '("Adams, Alice: {\"posted_grade\":\"95\"}; comment \"See me.\""
                          "Beta, Bob: {\"posted_grade\":\"80\"}; comment \"Bob's note.\""))))

  (it "names a restricted push with nothing to send"
    (let ((said nil))
      (with-org-canvas-test-config
        (with-grading-file test-only--file
          (with-mock-api
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
              (let ((result (org-canvas--submissions-push-current nil '(5003 42))))
                (expect (plist-get result :unchanged) :to-equal '(5003))
                (expect (plist-get result :missing) :to-equal '(42))))
            (expect (test-org-canvas-api-call-count) :to-equal 0))))
      (expect said :to-equal "Nothing to push; no row for 42; nothing to send for 5003")))

  (it "names the late status and the rubric rows a dry run would send"
    (let ((line (org-canvas--submissions-would-send-line
                 "Adams, Alice"
                 (list :old-score "3" :new-score "4" :late-status "late"
                       :triples '(("_7104" "2" "Sharp")))
                 nil)))
      (expect line :to-equal
              "Adams, Alice: {\"posted_grade\":\"4\",\"rubric_assessment\":{\"_7104\":{\"points\":2,\"comments\":\"Sharp\"}}}; late status late")))

  (it "refuses POST with ONLY, and an id that is not one"
    (expect (org-canvas-push-submission-grades "1001" t '(5001)) :to-throw 'user-error)
    (expect (org-canvas--submissions-user-ids '("12x")) :to-throw 'user-error)
    (expect (org-canvas--submissions-user-ids '(" 12" 7)) :to-equal '(12 7)))

  (it "refuses s outside a student's heading, and reports a failed push"
    (with-org-canvas-test-config
      (with-grading-file test-only--file
        (goto-char (point-min))
        (expect (org-canvas-submissions-push-at-point) :to-throw 'user-error)
        (org-canvas--submissions-goto-user 5001)
        (let ((said nil))
          (cl-letf (((symbol-function 'org-canvas--submissions-push-current)
                     (lambda (&rest _) (error "Boom")))
                    ((symbol-function 'org-canvas--user-message)
                     (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
            (expect (org-canvas-submissions-push-at-point) :to-be nil))
          (expect said :to-equal "Error pushing Adams, Alice: Boom"))
        (cl-letf (((symbol-function 'org-canvas--submissions-push-current)
                   (lambda (&rest _) (user-error "Typed badly"))))
          (expect (org-canvas-submissions-push-at-point) :to-throw 'user-error))
        (setq-local org-canvas-submissions--assignment-id nil)
        (cl-letf (((symbol-function 'org-canvas--submissions-ensure-context) #'ignore))
          (expect (org-canvas-submissions-push-at-point) :to-throw 'user-error)))))

  (it "binds s in the grading mode"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "s"))
            :to-be #'org-canvas-submissions-push-at-point)))

;;;; An Unconfirmed Push (issue #496)

(describe "org-canvas-submissions-resolve-conflicts (issue #496)"
  (it "takes or keeps the named students' CONFLICTs and names the rest"
    (with-org-canvas-test-config
      (test-batch-push--with-dir
          (concat test-grading-file-header
                  (test-refresh--student "Adams, Alice" 5001
                                         ":SCORE: 8\n:CANVAS_SCORE: 5\n:CONFLICT: score: Canvas has 8\n")
                  (test-refresh--student "Beta, Bob" 5002 ":SCORE: 4\n:CANVAS_SCORE: 4\n"))
        (let ((calls nil))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _)
                       (push (list method url) calls)
                       (test-org-canvas-make-submission '((score . 8) (attempt . 1)))))
                    ((symbol-function 'message) #'ignore))
            (let ((result (org-canvas-submissions-resolve-conflicts "1001" '("5001" 5002 9) nil)))
              (expect (plist-get result :resolved) :to-equal '("Adams, Alice"))
              (expect (plist-get result :unmarked) :to-equal '(5002))
              (expect (plist-get result :missing) :to-equal '(9))))
          (expect calls :to-equal
                  (list (list 'GET (org-canvas-api-course-endpoint
                                    "assignments/%s/submissions/%s" "1001" "5001"))))
          (with-temp-buffer
            (insert-file-contents file)
            (expect (buffer-string) :not :to-match ":CONFLICT:")
            (expect (buffer-string) :to-match ":CANVAS_SCORE: 8")
            (expect (buffer-string) :to-match ":SCORE: +8\n"))))))

  (it "takes Canvas's grade when asked to"
    (with-org-canvas-test-config
      (test-batch-push--with-dir
          (concat test-grading-file-header
                  (test-refresh--student "Adams, Alice" 5001
                                         ":SCORE: 9\n:CANVAS_SCORE: 5\n:CONFLICT: score: Canvas has 8\n")
                  "** Notes\n")
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) (test-org-canvas-make-submission '((score . 8)))))
                  ((symbol-function 'message) #'ignore))
          (expect (plist-get (org-canvas-submissions-resolve-conflicts 1001 '(5001) t)
                             :resolved)
                  :to-equal '("Adams, Alice")))
        (with-temp-buffer
          (insert-file-contents file)
          (expect (buffer-string) :to-match ":SCORE: +8\n")
          (expect (buffer-string) :to-match "Typed before taking Canvas's grade")))))

  (it "refuses an id that is not a user id before reading anything"
    (with-org-canvas-test-config
      (test-batch-push--with-dir test-grading-file-header
        (expect (org-canvas-submissions-resolve-conflicts "1001" '("abc") t)
                :to-throw 'user-error)))))

(provide 'org-canvas-submissions-conflict-test)
;;; org-canvas-submissions-conflict-test.el ends here
