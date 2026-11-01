;;; org-canvas-submissions-entry-test.el --- Tests for org-canvas-submissions-entry -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for grading-file entry, refresh and carry-over.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-entry)

(describe "org-canvas-submissions-add-comment"
  (it "posts comment via interactive flow"
    (with-test-api
      (with-temp-org-buffer
       "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SUBMISSION_ID: 50001\n:END:\n"
       (org-back-to-heading)
       (setq-local org-canvas-submissions--assignment-id "1001")
       (setq-local org-canvas-submissions--current-view 'detail)
       (org-canvas-submissions-mode 1)
       (cl-letf (((symbol-function 'read-string) (lambda (_) "Nice!"))
                 ((symbol-function 'y-or-n-p) (lambda (_) t)))
         (org-canvas-submissions-add-comment))
       ;; Canvas addresses the submission by the student's user id (#125)
       (expect-api-called 'PUT "assignments/1001/submissions/5001")
       (expect (buffer-string) :to-match "Nice!"))))

  (it "errors when not in detail view"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (setq-local org-canvas-submissions--current-view 'summary)
      (expect (org-canvas-submissions-add-comment) :to-throw 'user-error)))

  (it "errors when not in submissions mode"
    (with-temp-buffer
      (expect (org-canvas-submissions-add-comment) :to-throw 'user-error)))

  (it "errors on empty comment"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (org-canvas-submissions-mode 1)
     (cl-letf (((symbol-function 'read-string) (lambda (_) "")))
       (expect (org-canvas-submissions-add-comment) :to-throw 'user-error))))

  (it "errors when no USER_ID"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (org-canvas-submissions-mode 1)
     (expect (org-canvas-submissions-add-comment) :to-throw 'user-error))))

(describe "org-canvas--submissions-post-comment"
  (it "sends PUT request with comment data, addressed by user id"
    (with-test-api
      (org-canvas--submissions-post-comment "1001" "5001" "Great job!")
      (expect-api-called 'PUT "assignments/1001/submissions/5001")
      (let* ((call (test-org-canvas-last-api-call))
             (data (nth 2 call)))
        (expect (alist-get 'text_comment (alist-get 'comment data))
                :to-equal "Great job!")))))

(describe "org-canvas--submissions-append-comment-to-buffer"
  (it "appends comment after existing comments"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n\n** Comments\n- *Prof* <2026-01-01 Thu 00:00> :: Old comment\n"
     (org-back-to-heading)
     (let ((inhibit-read-only t))
       (org-canvas--submissions-append-comment-to-buffer "Adams, Alice" "New comment"))
     (expect (buffer-string) :to-match "New comment")
     (expect (buffer-string) :to-match "\\*You\\*")))

  (it "appends after multiple existing comments"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n\n** Comments\n- *Prof* <2026-01-01 Thu 00:00> :: First\n- *TA* <2026-01-02 Fri 00:00> :: Second\n"
     (org-back-to-heading)
     (let ((inhibit-read-only t))
       (org-canvas--submissions-append-comment-to-buffer "Adams, Alice" "Third"))
     (let ((content (buffer-string)))
       (expect content :to-match "Third")
       (expect (string-match "Second" content)
               :to-be-less-than
               (string-match "Third" content)))))

  (it "creates Comments section when missing"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n"
     (org-back-to-heading)
     (let ((inhibit-read-only t))
       (org-canvas--submissions-append-comment-to-buffer "Adams, Alice" "First comment"))
     (expect (buffer-string) :to-match "\\*\\* Comments")
     (expect (buffer-string) :to-match "First comment"))))

;;;; Fetch Parameters

(describe "org-canvas-pull-submissions"
  (it "fetches assignments and submissions"
    (with-org-canvas-test-config
      (let ((assignments-fetched nil)
            (submissions-fetched nil)
            (org-canvas-submissions-default-view 'summary))
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-reports) #'ignore)
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method url &optional _params)
                     (cond
                      ((string-match-p "assignments$" url)
                       (setq assignments-fetched t)
                       (list '((id . 1001) (name . "Homework 1"))))
                      ((string-match-p "submissions" url)
                       (setq submissions-fetched t)
                       (list (test-org-canvas-make-submission))))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _coll &rest _args) "Homework 1"))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf) buf)))
          (org-canvas-pull-submissions)
          (expect assignments-fetched :to-be-truthy)
          (expect submissions-fetched :to-be-truthy))))))

;;;; Refresh

(describe "org-canvas-submissions-refresh"
  (it "re-fetches and re-renders"
    (with-org-canvas-test-config
      (let ((fetch-count 0))
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-reports) #'ignore)
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     (cl-incf fetch-count)
                     (list (test-org-canvas-make-submission))))
                  ((symbol-function 'org-canvas--submissions-fetch-assignment)
                   (lambda (_id) nil))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf) buf)))
          (with-temp-buffer
            (org-mode)
            (setq-local org-canvas-submissions--assignment-name "HW")
            (setq-local org-canvas-submissions--assignment-id "1001")
            (setq-local org-canvas-submissions--current-view 'summary)
            (setq-local org-canvas-submissions--data nil)
            (org-canvas-submissions-mode 1)
            (org-canvas-submissions-refresh)
            (expect fetch-count :to-equal 1)))))))

;;;; Minor Mode

(describe "org-canvas-submissions-download-attachments"
  (it "errors when not in submissions mode"
    (with-temp-buffer
      (expect (org-canvas-submissions-download-attachments) :to-throw 'user-error)))

  (it "errors when not in detail view"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (setq-local org-canvas-submissions--current-view 'summary)
      (expect (org-canvas-submissions-download-attachments) :to-throw 'user-error)))

  (it "errors when no attachments found"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--assignment-name "HW")
     (org-canvas-submissions-mode 1)
     (expect (org-canvas-submissions-download-attachments) :to-throw 'user-error)))

  (it "downloads attachment files"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n\n** Attachments\n- [[https://example.com/files/1/download][hw.pdf]]\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--assignment-name "HW")
     (org-canvas-submissions-mode 1)
     (let ((downloaded nil))
       (cl-letf (((symbol-function 'org-canvas--submissions-download-file)
                  (lambda (url _dir filename)
                    (push (cons url filename) downloaded)))
                 ((symbol-function 'make-directory) (lambda (_dir &rest _) nil)))
         (org-canvas-submissions-download-attachments))
       (expect (length downloaded) :to-equal 1)
       (expect (cdar downloaded) :to-equal "hw.pdf"))))

  (it "falls back to submissions-directory when org-canvas-directory is nil"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n\n** Attachments\n- [[https://example.com/files/1/download][hw.pdf]]\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--assignment-name "HW")
     (org-canvas-submissions-mode 1)
     (let ((org-canvas-directory nil)
           (downloaded-dir nil))
       (cl-letf (((symbol-function 'org-canvas--submissions-download-file)
                  (lambda (_url dir _filename)
                    (setq downloaded-dir dir)))
                 ((symbol-function 'make-directory) (lambda (_dir &rest _) nil)))
         (org-canvas-submissions-download-attachments))
       (expect downloaded-dir :to-be-truthy))))

  (it "stops collecting at next sub-heading after Attachments"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n\n** Attachments\n- [[https://example.com/files/1/download][hw.pdf]]\n** Comments\n- *Prof* <2026-01-01 Thu 00:00> :: Comment with [[https://example.com/not-an-attachment][link]]\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--assignment-name "HW")
     (org-canvas-submissions-mode 1)
     (let ((downloaded nil))
       (cl-letf (((symbol-function 'org-canvas--submissions-download-file)
                  (lambda (url _dir filename)
                    (push (cons url filename) downloaded)))
                 ((symbol-function 'make-directory) (lambda (_dir &rest _) nil)))
         (org-canvas-submissions-download-attachments))
       ;; Only the attachment link, not the link in Comments
       (expect (length downloaded) :to-equal 1)
       (expect (cdar downloaded) :to-equal "hw.pdf")))))

(describe "org-canvas--submissions-download-file"
  (it "calls url-copy-file with auth token"
    (let ((org-canvas-api-token "test-token")
          (copied-url nil))
      (cl-letf (((symbol-function 'url-copy-file)
                 (lambda (url _path &rest _) (setq copied-url url))))
        (org-canvas--submissions-download-file
         "https://example.com/download" "/tmp" "test.pdf"))
      (expect copied-url :to-match "access_token=test-token")
      (expect copied-url :to-match "\\?access_token")))

  (it "uses & separator when URL already has query params"
    (let ((org-canvas-api-token "test-token")
          (copied-url nil))
      (cl-letf (((symbol-function 'url-copy-file)
                 (lambda (url _path &rest _) (setq copied-url url))))
        (org-canvas--submissions-download-file
         "https://example.com/download?foo=bar" "/tmp" "test.pdf"))
      (expect copied-url :to-match "&access_token=test-token")))

  (it "re-signals a failure without the token (issue #435)"
    ;; url-copy-file puts the whole URL, token included, into the error
    ;; data; a redaction-shaped leak straight into the echo area.
    (let ((org-canvas-api-token "secret-token-value"))
      (cl-letf (((symbol-function 'url-copy-file)
                 (lambda (&rest _)
                   ;; A non-string datum rides along untouched
                   (signal 'file-missing
                           (list "Opening URL" 404
                                 "https://example.com/download?access_token=secret-token-value")))))
        (let ((err (condition-case e
                       (progn (org-canvas--submissions-download-file
                               "https://example.com/download" "/tmp" "test.pdf")
                              nil)
                     (error e))))
          (expect (cdr err) :to-equal
                  '("Opening URL" 404
                    "https://example.com/download?access_token=***MASKED***")))))))

;;;; Refresh Error Paths

(describe "org-canvas-submissions-refresh error paths"
  (it "errors when not in submissions mode"
    (with-temp-buffer
      (expect (org-canvas-submissions-refresh) :to-throw 'user-error)))

  (it "errors when no assignment ID"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (setq-local org-canvas-submissions--assignment-id nil)
      (expect (org-canvas-submissions-refresh) :to-throw 'user-error))))

;;;; Push-grades Error Path

(describe "refresh resets original-scores"
  (it "repopulates snapshot after refresh"
    (with-org-canvas-test-config
      (let ((target-buf nil)
            (org-canvas-submissions-directory (make-temp-file "org-canvas-subs-" t)))
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-reports) #'ignore)
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     (list (test-org-canvas-make-submission '((score . 99))))))
                  ((symbol-function 'org-canvas--submissions-fetch-assignment)
                   (lambda (_id) nil))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf) (setq target-buf buf) buf)))
          (with-temp-buffer
            (org-mode)
            (setq-local org-canvas-submissions--assignment-name "HW")
            (setq-local org-canvas-submissions--assignment-id "1001")
            (setq-local org-canvas-submissions--current-view 'detail)
            (setq-local org-canvas-submissions--data nil)
            (setq-local org-canvas-submissions--original-scores '((5001 . "50")))
            (org-canvas-submissions-mode 1)
            (org-canvas-submissions-refresh)
            ;; Display creates a *submissions: HW* buffer; check scores there
            (with-current-buffer target-buf
              (expect (alist-get 5001 org-canvas-submissions--original-scores)
                      :to-equal "99"))
            (when (buffer-live-p target-buf)
              (kill-buffer target-buf))))))))

;;;; Integration — Nil to Score Transition

(describe "org-canvas--submissions-display as a grading file"
  (it "writes and visits the detail view, with a .gitignore beside it"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (subs (list (test-org-canvas-make-submission)))
           (shown nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) (setq shown b) b)))
              (org-canvas--submissions-display "HW 1" "1001" subs 'detail))
            (expect (file-exists-p (expand-file-name "HW_1.org" dir)) :to-be-truthy)
            (expect (file-exists-p (expand-file-name ".gitignore" dir)) :to-be-truthy)
            (with-current-buffer shown
              (expect buffer-file-name :to-match "HW_1\\.org$")
              (expect (buffer-modified-p) :to-be nil)
              (expect org-canvas-submissions-mode :to-be-truthy)
              (expect (buffer-string) :to-match ":CANVAS_SCORE: 92")))
        (when (buffer-live-p shown) (kill-buffer shown))
        (delete-directory dir t))))
  (it "keeps the summary view ephemeral"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (shown nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) (setq shown b) b)))
              (org-canvas--submissions-display "HW 1" "1001" (list (test-org-canvas-make-submission)) 'summary))
            (expect (directory-files dir nil "\\.org\\'") :to-equal nil)
            (with-current-buffer shown
              (expect buffer-file-name :to-be nil)))
        (when (buffer-live-p shown) (kill-buffer shown))
        (delete-directory dir t))))
  (it "does not rewrite an existing .gitignore and honors the option"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (gi (expand-file-name ".gitignore" dir)))
      (unwind-protect
          (progn
            (with-temp-file gi (insert "custom\n"))
            (org-canvas--submissions-ensure-directory)
            (expect (with-temp-buffer (insert-file-contents gi) (buffer-string)) :to-equal "custom\n")
            (delete-file gi)
            (let ((org-canvas-submissions-write-gitignore nil))
              (org-canvas--submissions-ensure-directory))
            (expect (file-exists-p gi) :to-be nil))
        (delete-directory dir t))))

  (it "warns when git tracks files under the submissions directory (issue #549)"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (warned nil))
      (unwind-protect
          (progn
            ;; A work tree with a tracked file under the directory:
            ;; ls-files answers 0, and the .gitignore does not untrack.
            (cl-letf (((symbol-function 'process-file)
                       (lambda (_program _in _out _display &rest args)
                         (if (equal (car args) "ls-files") 0 1)))
                      ((symbol-function 'org-canvas--log-warning)
                       (lambda (_logger fmt &rest args)
                         (push (apply #'format fmt args) warned))))
              (org-canvas--submissions-ensure-directory))
            (expect (car warned) :to-match "git tracks"))
            ;; Nothing tracked (ls-files answers 1): no warning.
            (let ((org-canvas-submissions-write-gitignore nil))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (_program _in _out _display &rest args)
                           (if (equal (car args) "ls-files") 1 0)))
                        ((symbol-function 'org-canvas--log-warning)
                         (lambda (&rest _) (error "no warning expected"))))
                (org-canvas--submissions-ensure-directory)))
            ;; Without git: no warning, never an error.
            (let ((org-canvas-submissions-write-gitignore nil))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (&rest _)
                           (signal 'file-missing '("Searching for program" "git")))))
                (org-canvas--submissions-ensure-directory))))
        (delete-directory dir t))))

(describe "a re-pull keeps typed scores (issue #281)"
  (it "keeps the typed SCORE, refreshes CANVAS_SCORE, and asks nothing"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (test-refresh--from-canvas nil)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "95")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (buffer-modified-p) :to-be nil))))
  (it "keeps a 0 typed on a missing row that never had a CANVAS_SCORE"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:STATUS: missing\n:SCORE: 0\n:END:\n")
        (test-refresh--from-canvas '((score . nil) (submitted_at . nil) (missing . t)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "0")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-be nil)
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 1))))
  (it "marks the heading when Canvas graded it since the score was typed"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (test-refresh--from-canvas '((score . 93)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "95")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "93")
        (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 93"))))
  (it "names a grade Canvas cleared meanwhile"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (test-refresh--from-canvas '((score . nil)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "95")
        (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has no grade"))))
  (it "writes nothing when Canvas now holds the typed score"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (test-refresh--from-canvas '((score . 95)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "95")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "95")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil))))
  (it "does not carry a score that matches its baseline"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n"
                               "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:END:\n")
      (expect (org-canvas--submissions-collect-carryover) :to-be nil))))

(describe "the summary table still guards unpushed edits"
  (defun test-summary--buffer-with-edit ()
    "Insert a summary table whose one score was edited, with its context."
    (insert "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n\n")
    (insert "| Student | Status | Submitted At | Score |\n")
    (insert "|---------+--------+--------------+-------|\n")
    (insert "| Adams, Alice | submitted | <2026-02-15> | 95 |\n")
    (org-table-align)
    (setq-local org-canvas-submissions--assignment-name "HW")
    (setq-local org-canvas-submissions--assignment-id "1001")
    (setq-local org-canvas-submissions--current-view 'summary)
    (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
    (setq-local org-canvas-submissions--data (list (test-org-canvas-make-submission)))
    (org-canvas-submissions-mode 1))
  (it "refuses when the grader declines to lose edits"
    (with-org-canvas-test-config
      (let ((fetched nil)
            (noninteractive nil))
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                   (lambda (_id) (setq fetched t) nil))
                  ((symbol-function 'y-or-n-p) (lambda (_) nil)))
          (with-temp-buffer
            (org-mode)
            (test-summary--buffer-with-edit)
            (expect (org-canvas-submissions-refresh) :to-throw 'user-error)))
        (expect fetched :to-be nil))))
  (it "goes ahead under noninteractive, saying what is lost"
    (with-org-canvas-test-config
      (let ((fetched nil)
            (messages nil)
            (noninteractive t))
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-reports) #'ignore)
                  ((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                   (lambda (_id) (setq fetched t) (list (test-org-canvas-make-submission))))
                  ((symbol-function 'org-canvas--submissions-fetch-assignment) (lambda (_id) nil))
                  ((symbol-function 'switch-to-buffer) (lambda (b) b))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                  ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask"))))
          (with-temp-buffer
            (org-mode)
            (test-summary--buffer-with-edit)
            (org-canvas-submissions-refresh)))
        (expect fetched :to-be-truthy)
        (expect messages :to-contain "1 unpushed score change(s) lost by Refresh")))))

;;;; What a refresh changed (issue #282)

(describe "a refresh says what changed (issue #282)"
  (it "counts new work, posted rows and scores moved on Canvas, and logs each student"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Adams, Alice" 5001
                                                        ":STATUS: unsubmitted\n")
                                 (test-refresh--student "Beta, Bob" 5002
                                                        ":STATUS: submitted\n:SCORE: 80\n:CANVAS_SCORE: 80\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"))
        (test-refresh--run
         (list (test-org-canvas-make-submission '((score . nil)))
               (test-refresh--bob '((score . 85) (posted_at . "2026-02-20T10:00:00Z")))))
        (expect (test-refresh--summary)
                :to-equal "Refreshed HW: 1 new, 1 scored on Canvas since the pull")
        (expect test-refresh--log :to-contain "[Refresh] Adams, Alice: new")
        (expect test-refresh--log :to-contain "[Refresh] Beta, Bob: scored on Canvas since the pull")
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (org-entry-get (point) "POSTED_AT") :to-be-truthy))))
  (it "counts a row that was posted, on its own"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Beta, Bob" 5002
                                                        ":STATUS: graded\n:SCORE: 92\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"))
        (test-refresh--run (list (test-refresh--bob '((posted_at . "2026-02-20T10:00:00Z")))))
        (expect (test-refresh--summary) :to-equal "Refreshed HW: 1 posted"))))
  (it "marks a resubmission on a graded row CONFLICT and a later attempt on an ungraded row new"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Adams, Alice" 5001
                                                        ":STATUS: submitted\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n")
                                 (test-refresh--student "Beta, Bob" 5002
                                                        ":STATUS: graded\n:SCORE: 3\n:CANVAS_SCORE: 3\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"))
        (test-refresh--run
         (list (test-org-canvas-make-submission '((score . nil) (attempt . 2)))
               (test-refresh--bob '((score . 3) (attempt . 2) (late . t)))))
        (expect (test-refresh--summary)
                :to-equal (concat "Refreshed HW: 1 new, 1 resubmitted after grading"
                                  "; CONFLICT on 1: Beta, Bob (attempt: 2 submitted after grading)"))
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "attempt: 2 submitted after grading")
        (expect (org-entry-get (point) "ATTEMPT") :to-equal "2")
        (expect (org-entry-get (point) "SCORE") :to-equal "3")
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-be nil))))
  (it "says when nothing changed, naming the last pull"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "#+PROPERTY: PULLED_AT <2026-02-16 Mon 09:00>\n"
                                 (test-refresh--student "Adams, Alice" 5001
                                                        ":STATUS: submitted\n:SCORE: 92\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"))
        (test-refresh--run (list (test-org-canvas-make-submission '((attempt . 1)))))
        (expect (test-refresh--summary)
                :to-equal "Refreshed HW: no changes since <2026-02-16 Mon 09:00>")
        (expect test-refresh--log :not :to-contain "[Refresh] Adams, Alice: new"))))
  (it "says nothing after the first pull of a file"
    (with-org-canvas-test-config
      (with-grading-file test-grading-file-header
        ;; A file with no student heading yet reads as a summary; this is
        ;; the grading file being written for the first time.
        (setq-local org-canvas-submissions--current-view 'detail)
        (test-refresh--run (list (test-org-canvas-make-submission)))
        (expect (test-refresh--summary) :to-be nil)
        (expect (buffer-string) :to-match "^\\* Adams, Alice"))))
  (it "keeps the Refreshed line in the buffer, and nil after a first pull (issue #415)"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Beta, Bob" 5002
                                                        ":STATUS: graded\n:SCORE: 92\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"))
        (test-refresh--run (list (test-refresh--bob '((posted_at . "2026-02-20T10:00:00Z")))))
        (expect org-canvas-submissions--last-refresh :to-equal "Refreshed HW: 1 posted")))
    (with-org-canvas-test-config
      (with-grading-file test-grading-file-header
        (setq-local org-canvas-submissions--current-view 'detail)
        (setq-local org-canvas-submissions--last-refresh "stale")
        (test-refresh--run (list (test-org-canvas-make-submission)))
        (expect org-canvas-submissions--last-refresh :to-be nil))))
  (it "drops a departed student with nothing under their heading, and says so"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Adams, Alice" 5001
                                                        ":STATUS: submitted\n:SCORE: 92\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n")
                                 (test-refresh--student "Beta, Bob" 5002 ":STATUS: missing\n"))
        (test-refresh--run (list (test-org-canvas-make-submission '((attempt . 1)))))
        (expect (test-refresh--summary) :to-equal "Refreshed HW: 1 left the course (0 kept)")
        (expect test-refresh--log :to-contain "[Refresh] Beta, Bob: left the course")
        (expect (org-canvas--submissions-goto-user 5002) :to-be nil))))
  (it "keeps a departed student's heading for the work under it, marked left, and never pushes it"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Adams, Alice" 5001
                                                        ":STATUS: submitted\n:SCORE: 92\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n")
                                 (test-refresh--student "Beta, Bob" 5002
                                                        ":STATUS: submitted\n:SCORE: 0\n:CANVAS_SCORE: 80\n")
                                 "\n** Notes\nExtension granted to Friday.\n\n** Comment to post\nSee me.\n")
        (test-refresh--run (list (test-org-canvas-make-submission '((attempt . 1)))))
        (expect (test-refresh--summary) :to-equal "Refreshed HW: 1 left the course (1 kept)")
        (expect test-refresh--log :to-contain "[Refresh] Beta, Bob: left the course (heading kept)")
        (expect (org-canvas--submissions-goto-user 5002) :to-be-truthy)
        (expect (org-entry-get (point) "STATUS") :to-equal "left")
        (expect (org-entry-get (point) "SCORE") :to-equal "0")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "80")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (org-canvas--submissions-section-text org-canvas--submissions-notes-heading)
                :to-equal "Extension granted to Friday.")
        (expect (org-canvas--submissions-comment-draft) :to-equal "See me.")
        ;; Alphabetical order is kept: Adams before Beta.
        (expect (buffer-string) :to-match "\\* Adams, Alice\\(.\\|\n\\)*\\* Beta, Bob")
        ;; Nothing is ever pushed for them.
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
        (expect (org-canvas--submissions-collect-comment-drafts) :to-be nil)
        (org-canvas-submissions-apply-completion-rule 10)
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "SCORE") :to-equal "0")
        ;; A second refresh keeps them without reporting them leaving again.
        (test-refresh--run (list (test-org-canvas-make-submission '((attempt . 1)))))
        (expect (test-refresh--summary) :to-match "\\`Refreshed HW: no changes since <")
        (expect (org-canvas--submissions-goto-user 5002) :to-be-truthy)
        (expect (org-entry-get (point) "STATUS") :to-equal "left"))))
  (it "keeps a departed student's excusal and rubric rows too"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-refresh--student "Beta, Bob" 5002
                                                        ":STATUS: submitted\n:SCORE: EX\n:CANVAS_SCORE: EX\n")
                                 "\n** Notes\nLeft mid-term.\n")
        (test-refresh--run nil)
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "SCORE") :to-equal "EX")
        (expect (org-entry-get (point) "STATUS") :to-equal "left")))))

(describe "refresh over a clean grading file"
  (it "refreshes when nothing is pending"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n")
        (let ((fetched nil))
          (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                     (lambda (_id) (setq fetched t) (list (test-org-canvas-make-submission))))
                    ((symbol-function 'org-canvas--submissions-fetch-assignment)
                     (lambda (_id) '((id . 1001) (rubric_settings . ((id . 133477) (title . "Essay Rubric"))))))
                    ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) nil))
                    ((symbol-function 'switch-to-buffer) (lambda (b) b))
                    ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask"))))
            (org-canvas-submissions-refresh))
          (expect fetched :to-be-truthy)
          (expect (buffer-string) :to-match "^#\\+PROPERTY: CANVAS_RUBRIC_ID 133477")
          (expect (buffer-string) :to-match "^Rubric: \\[\\[https://.*/rubrics/133477\\]\\[on Canvas\\]\\]"))))))

(describe "org-canvas-open-submissions"
  (it "visits a chosen grading file with the mode and context set"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (file (expand-file-name "HW.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file (insert test-grading-file-header "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt files &rest _) (car files))))
              (org-canvas-open-submissions))
            (expect buffer-file-name :to-equal file)
            (expect org-canvas-submissions-mode :to-be-truthy)
            (expect org-canvas-submissions--assignment-id :to-equal "1001")
            (expect org-canvas-submissions--current-view :to-equal 'detail))
        (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
        (delete-directory dir t))))
  (it "explains when there is nothing to open"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir))
      (unwind-protect
          (expect (org-canvas-open-submissions) :to-throw 'user-error)
        (delete-directory dir t)))))

;;;; Non-interactive entry points (issue #280)

(describe "org-canvas-pull-submissions with an argument"
  (it "takes an id, reads that one assignment and returns the grading buffer"
    (with-org-canvas-test-config
      (with-submissions-dir
        (setq test-entry--calls nil)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-entry--canvas '((id . 1001) (name . "Homework 1")))))
          (let ((buf (org-canvas-pull-submissions "1001")))
            (expect (buffer-live-p buf) :to-be-truthy)
            (with-current-buffer buf
              (expect buffer-file-name :to-match "Homework_1\\.org$")
              (expect org-canvas-submissions-mode :to-be-truthy)
              (expect org-canvas-submissions--assignment-id :to-equal "1001")
              (expect org-canvas-submissions--current-view :to-equal 'detail)
              (expect (buffer-string) :to-match "^\\* Adams, Alice")))
          (expect (cl-count-if (lambda (c) (string-match-p "/assignments$" (cdr c))) test-entry--calls)
                  :to-equal 0)
          (expect (cl-count-if (lambda (c) (string-match-p "/assignments/1001$" (cdr c))) test-entry--calls)
                  :to-equal 1)))))
  (it "takes an integer id too"
    (with-org-canvas-test-config
      (with-submissions-dir
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-entry--canvas '((id . 1001) (name . "Homework 1")))))
          (with-current-buffer (org-canvas-pull-submissions 1001)
            (expect org-canvas-submissions--assignment-name :to-equal "Homework 1"))))))
  (it "takes an exact name, resolved against the course's list"
    (with-org-canvas-test-config
      (with-submissions-dir
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-entry--canvas nil '(((id . 1001) (name . "Homework 1"))
                                             ((id . 1002) (name . "Homework 2"))))))
          (with-current-buffer (org-canvas-pull-submissions "Homework 2")
            (expect org-canvas-submissions--assignment-id :to-equal "1002"))))))
  (it "names an id or a name that matches nothing"
    (with-org-canvas-test-config
      (with-submissions-dir
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-entry--canvas nil '(((id . 1001) (name . "Homework 1"))))))
          (expect (org-canvas-pull-submissions "9999")
                  :to-throw 'user-error '("No assignment with id 9999 in this course"))
          (expect (org-canvas-pull-submissions "Homework 7")
                  :to-throw 'user-error '("No assignment named Homework 7 in this course"))))))
  (it "downloads every attachment into the grading file when asked, whatever the default view"
    (with-org-canvas-test-config
      (with-submissions-dir
        (let ((org-canvas-submissions-default-view 'summary)
              (downloaded nil))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _)
                       (push (cons method url) test-entry--calls)
                       (cond ((string-match-p "/submissions$" url)
                              (vector (test-org-canvas-make-submission-with-attachment)))
                             (t '((id . 1001) (name . "Homework 1"))))))
                    ((symbol-function 'org-canvas--submissions-download-file)
                     (lambda (_url _dir filename) (push filename downloaded))))
            (with-current-buffer (org-canvas-pull-submissions "1001" t)
              (expect org-canvas-submissions--current-view :to-equal 'detail)
              (expect buffer-file-name :to-be-truthy)))
          (expect downloaded :to-equal '("homework1.pdf")))))))

(describe "org-canvas-open-submissions with an argument"
  (defun test-entry--write-grading-file (dir)
    "Write HW's grading file into DIR and return its path."
    (let ((file (expand-file-name "HW.org" dir)))
      (with-temp-file file
        (insert test-grading-file-header "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"))
      file))
  (it "opens an absolute path and returns the buffer with its context"
    (with-submissions-dir
      (let* ((file (test-entry--write-grading-file dir))
             (buf (org-canvas-open-submissions file)))
        (expect (buffer-file-name buf) :to-equal file)
        (with-current-buffer buf
          (expect org-canvas-submissions-mode :to-be-truthy)
          (expect org-canvas-submissions--assignment-id :to-equal "1001")
          (expect org-canvas-submissions--current-view :to-equal 'detail)))))
  (it "opens a bare name, with or without .org, and an assignment's name"
    (with-submissions-dir
      (let ((file (test-entry--write-grading-file dir)))
        (expect (buffer-file-name (org-canvas-open-submissions "HW")) :to-equal file)
        (expect (buffer-file-name (org-canvas-open-submissions "HW.org")) :to-equal file)
        (with-temp-file (expand-file-name "Journal_02.org" dir)
          (insert test-grading-file-header))
        (expect (buffer-file-name (org-canvas-open-submissions "Journal 02"))
                :to-equal (expand-file-name "Journal_02.org" dir)))))
  (it "names a file that is not there"
    (with-submissions-dir
      (test-entry--write-grading-file dir)
      (expect (org-canvas-open-submissions "Quiz 3") :to-throw 'user-error)
      (expect (org-canvas-open-submissions (expand-file-name "gone.org" dir))
              :to-throw 'user-error)))
  (it "turns on org-mode when the visited buffer is not in it"
    (with-submissions-dir
      (let ((file (test-entry--write-grading-file dir))
            (plain (generate-new-buffer " *grading-plain*")))
        (unwind-protect
            (cl-letf (((symbol-function 'org-canvas--find-file-noselect) (lambda (&rest _) plain)))
              (with-current-buffer plain
                (insert-file-contents file))
              (expect (org-canvas-open-submissions file) :to-be plain)
              (expect (buffer-local-value 'major-mode plain) :to-be 'org-mode)
              (expect (buffer-local-value 'org-canvas-submissions--assignment-id plain)
                      :to-equal "1001"))
          (kill-buffer plain))))))

(defun test-byid--write (dir base id name &optional body)
  "Write grading file BASE.org into DIR for assignment ID named NAME.
A nil ID writes a header with no CANVAS_ASSIGNMENT_ID.  BODY follows
the header.  Return the path."
  (let ((file (expand-file-name (concat base ".org") dir)))
    (with-temp-file file
      (insert (format "#+TITLE: Submissions: %s\n" name)
              (if id (format "#+PROPERTY: CANVAS_ASSIGNMENT_ID %s\n" id) "")
              (format "#+PROPERTY: CANVAS_ASSIGNMENT_NAME %s\n\n" name)
              (or body "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 1\n:END:\n")))
    file))

(defun test-byid--org-files (dir)
  "Return the names of the .org files in DIR."
  (directory-files dir nil "\\.org\\'"))

(defmacro test-byid--with-info-log (&rest body)
  "Run BODY with `test-byid--infos' collecting the info lines logged."
  (declare (indent 0))
  `(let ((test-byid--infos nil))
     (cl-letf (((symbol-function 'org-canvas--log-info)
                (lambda (_logger fmt &rest args)
                  (push (apply #'format fmt args) test-byid--infos))))
       ,@body)))

(defvar test-byid--infos nil "Info lines `test-byid--with-info-log' collected.")

(describe "A column's grading file is found by its id (issue #431)"
  (it "renames the file of a renamed column, its attachments and its header"
    (with-submissions-dir
      (let* ((old (test-byid--write
                   dir "Attendance_03" 77 "Attendance 03"
                   (concat "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n"
                           ":SCORE: 1\n:END:\n"
                           "- [[file:files/Attendance_03/Adams__Alice/a.pdf][a.pdf]]\n")))
             (files (expand-file-name "files/Attendance_03/Adams__Alice/" dir))
             (target (expand-file-name "Attendance_03__Wed_Sep_30.org" dir)))
        (make-directory files t)
        (with-temp-file (expand-file-name "a.pdf" files) (insert "pdf"))
        (test-byid--with-info-log
          (expect (org-canvas--submissions-grading-file-for
                   "Attendance 03: Wed Sep 30" "77")
                  :to-equal target)
          (expect (cl-count-if (lambda (line) (string-match-p "Renamed" line))
                               test-byid--infos)
                  :to-equal 1)
          (expect (cl-find-if (lambda (line) (string-match-p "Renamed" line))
                              test-byid--infos)
                  :to-match "Renamed grading file Attendance_03\\.org to Attendance_03__Wed_Sep_30\\.org"))
        (expect (file-exists-p old) :to-be nil)
        (expect (test-byid--org-files dir)
                :to-equal '("Attendance_03__Wed_Sep_30.org"))
        (expect (file-exists-p
                 (expand-file-name
                  "files/Attendance_03__Wed_Sep_30/Adams__Alice/a.pdf" dir))
                :to-be-truthy)
        (expect (file-exists-p (expand-file-name "files/Attendance_03" dir))
                :to-be nil)
        (let ((text (with-temp-buffer (insert-file-contents target) (buffer-string))))
          (expect text :to-match "^#\\+TITLE: Submissions: Attendance 03: Wed Sep 30$")
          (expect text :to-match
                  "^#\\+PROPERTY: CANVAS_ASSIGNMENT_NAME Attendance 03: Wed Sep 30$")
          (expect text :to-match "^#\\+PROPERTY: CANVAS_ASSIGNMENT_ID 77$")
          (expect text :to-match "^:SCORE: 1$")
          (expect text :to-match
                  "\\[\\[file:files/Attendance_03__Wed_Sep_30/Adams__Alice/a\\.pdf\\]")))))

  (it "pulls a renamed column into its old file, scores kept, and writes no second file"
    (with-org-canvas-test-config
      (with-submissions-dir
        (test-byid--write
         dir "Homework_1" 1001 "Homework 1"
         "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-entry--canvas '((id . 1001) (name . "Homework 1 (revised)")))))
          (with-current-buffer (org-canvas-pull-submissions "1001")
            (expect buffer-file-name :to-match "Homework_1__revised_\\.org\\'")
            (expect org-canvas-submissions--assignment-name
                    :to-equal "Homework 1 (revised)")
            (org-canvas--submissions-goto-user 5001)
            (expect (org-entry-get (point) "SCORE") :to-equal "95")))
        (expect (test-byid--org-files dir)
                :to-equal '("Homework_1__revised_.org")))))

  (it "moves a buffer visiting the old file along with it"
    (with-submissions-dir
      (let* ((old (test-byid--write dir "HW" 1001 "HW"))
             (buf (org-canvas--find-file-noselect old)))
        (with-current-buffer buf
          (setq-local org-canvas-submissions--assignment-name "HW"))
        (org-canvas--submissions-grading-file-for "HW 2" 1001)
        (expect (buffer-live-p buf) :to-be-truthy)
        (expect (buffer-file-name buf)
                :to-equal (expand-file-name "HW_2.org" dir))
        (expect (buffer-modified-p buf) :to-be nil)
        (expect (buffer-local-value 'org-canvas-submissions--assignment-name buf)
                :to-equal "HW 2"))))

  (it "refuses to rename a file whose buffer holds unsaved changes"
    (with-submissions-dir
      (let* ((old (test-byid--write dir "HW" 1001 "HW"))
             (buf (org-canvas--find-file-noselect old)))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "typed\n"))
        (expect (org-canvas--submissions-grading-file-for "HW 2" 1001)
                :to-throw 'user-error)
        (expect (file-exists-p old) :to-be-truthy)
        (expect (test-byid--org-files dir) :to-equal '("HW.org"))
        (with-current-buffer buf (set-buffer-modified-p nil)))))

  (it "refuses to rename over a file that already has the new name"
    (with-submissions-dir
      (test-byid--write dir "HW" 1001 "HW")
      (test-byid--write dir "HW_2" nil "HW 2")
      (expect (org-canvas--submissions-grading-file-for "HW 2" 1001)
              :to-throw 'user-error)
      (expect (test-byid--org-files dir) :to-equal '("HW.org" "HW_2.org"))))

  (it "leaves an attachment directory where it is when the new one exists"
    (with-submissions-dir
      (test-byid--write dir "HW" 1001 "HW")
      (make-directory (expand-file-name "files/HW/A/" dir) t)
      (make-directory (expand-file-name "files/HW_2/" dir) t)
      (let ((warnings nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) warnings))))
          (org-canvas--submissions-grading-file-for "HW 2" 1001))
        (expect (length warnings) :to-equal 1))
      (expect (file-directory-p (expand-file-name "files/HW/A/" dir)) :to-be-truthy)
      (expect (test-byid--org-files dir) :to-equal '("HW_2.org"))))

  (it "names both files when two claim one id, and writes nothing"
    (with-org-canvas-test-config
      (with-submissions-dir
        (test-byid--write dir "Attendance_01" 1001 "Attendance 01")
        (test-byid--write dir "Attendance_01__Fri_Sep_18" 1001 "Attendance 01: Fri Sep 18")
        (let ((message nil))
          (condition-case err
              (org-canvas--submissions-grading-file-for "Attendance 01: Fri" 1001)
            (user-error (setq message (error-message-string err))))
          (expect message :to-match "Attendance_01\\.org and Attendance_01__Fri_Sep_18\\.org")
          (expect message :to-match "assignment 1001"))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-entry--canvas '((id . 1001) (name . "Attendance 01: Fri")))))
          (expect (org-canvas-pull-submissions "1001") :to-throw 'user-error))
        (expect (org-canvas-open-submissions "1001") :to-throw 'user-error)
        (expect (test-byid--org-files dir)
                :to-equal '("Attendance_01.org" "Attendance_01__Fri_Sep_18.org")))))

  (it "still finds a file without an id header by the column's name"
    (with-submissions-dir
      (let ((legacy (test-byid--write dir "HW_1" nil "HW 1")))
        (expect (org-canvas--submissions-grading-file-for "HW 1" 1001)
                :to-equal legacy)
        (expect (org-canvas--submissions-grading-file-path "HW 1") :to-equal legacy)
        (expect (test-byid--org-files dir) :to-equal '("HW_1.org")))))

  (it "opens a file by the id in its header, whatever it is called"
    (with-submissions-dir
      (let ((file (test-byid--write dir "Old_name" 1001 "Old name")))
        (expect (org-canvas--submissions-grading-file-path "1001") :to-equal file)
        (expect (org-canvas--submissions-grading-file-path 1001) :to-equal file)
        (expect (buffer-file-name (org-canvas-open-submissions 1001)) :to-equal file)
        (expect (org-canvas--submissions-grading-file-path 1002) :to-throw 'user-error))))

  (it "passes over lock files when reading the headers"
    (with-submissions-dir
      (let ((file (test-byid--write dir "HW" 1001 "HW")))
        (make-symbolic-link "user@host.1234:1" (expand-file-name ".#HW.org" dir))
        (expect (org-canvas--submissions-files-for-id 1001) :to-equal (list file)))))

  (it "passes over a .org it cannot read"
    (with-submissions-dir
      (let ((file (test-byid--write dir "HW" 1001 "HW")))
        (make-directory (expand-file-name "archive.org" dir))
        (expect (org-canvas--submissions-files-for-id 1001) :to-equal (list file))))))

(describe "org-canvas-submissions-refresh-file"
  (it "visits, re-pulls and returns a buffer the grading commands can run in"
    (with-org-canvas-test-config
      (with-submissions-dir
        (let ((file (expand-file-name "HW.org" dir))
              (downloaded nil))
          (with-temp-file file
            (insert test-grading-file-header
                    "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n"))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (lambda (_method url &rest _)
                       (cond ((string-match-p "/submissions$" url)
                              (vector (test-org-canvas-make-submission-with-attachment)))
                             (t '((id . 1001) (name . "HW"))))))
                    ((symbol-function 'org-canvas--submissions-download-file)
                     (lambda (_url _dir filename) (push filename downloaded))))
            (let ((buf (org-canvas-submissions-refresh-file "HW")))
              (expect (buffer-file-name buf) :to-equal file)
              (with-current-buffer buf
                (expect org-canvas-submissions-mode :to-be-truthy)
                (expect (buffer-modified-p) :to-be nil)
                (expect (buffer-string) :to-match "homework1\\.pdf")
                (org-canvas--submissions-goto-user 5001)
                (expect (org-entry-get (point) "SCORE") :to-equal "95")
                (org-canvas-submissions-download-all-attachments)
                (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 1))))
          (expect downloaded :to-equal '("homework1.pdf"))))))
  (it "asks for the file only when none is given"
    (with-submissions-dir
      (with-temp-file (expand-file-name "HW.org" dir)
        (insert test-grading-file-header))
      (expect (org-canvas-submissions-refresh-file) :to-throw 'error '("must not prompt"))))
  (it "re-pulls a list of files and returns their buffers (issue #450)"
    (with-submissions-dir
      (let ((file (expand-file-name "HW.org" dir))
            (refreshed 0))
        (with-temp-file file (insert test-grading-file-header))
        (cl-letf (((symbol-function 'org-canvas--submissions-refresh-buffer)
                   (lambda () (cl-incf refreshed) (current-buffer))))
          (let ((bufs (org-canvas-submissions-refresh-file '("HW" "1001"))))
            (expect refreshed :to-equal 2)
            (expect (mapcar #'buffer-file-name bufs) :to-equal (list file file))))))))

(describe "org-canvas-submissions-download-all-attachments"
  (it "downloads for every student with attachments and skips the rest"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Attachments\n- [[https://example.com/files/1/download][a.pdf]]\n\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:END:\n\n* Cruz, Cal\n:PROPERTIES:\n:USER_ID: 5003\n:END:\n\n** Attachments\n- [[https://example.com/files/2/download][c.pdf]]\n")
      (let ((downloaded nil))
        (cl-letf (((symbol-function 'org-canvas--submissions-download-file)
                   (lambda (_url _dir filename) (push filename downloaded)))
                  ((symbol-function 'make-directory) (lambda (_dir &rest _) nil)))
          (org-canvas-submissions-download-all-attachments))
        (expect (sort downloaded #'string<) :to-equal '("a.pdf" "c.pdf"))))))

(describe "download rewrites attachment entries"
  (it "links the local copy after downloading and skips it next time"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (calls 0))
      (unwind-protect
          (with-temp-org-buffer
           "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Attachments\n- [[https://example.com/files/1/download][hw.pdf]]\n\n** Comments\n- *Prof* :: fine\n"
           (org-back-to-heading)
           (setq-local org-canvas-submissions--current-view 'detail)
           (setq-local org-canvas-submissions--assignment-name "HW")
           (org-canvas-submissions-mode 1)
           (cl-letf (((symbol-function 'org-canvas--submissions-download-file)
                      (lambda (_url d filename)
                        (cl-incf calls)
                        (with-temp-file (expand-file-name filename d) (insert "pdf")))))
             (org-canvas-submissions-download-attachments)
             (expect calls :to-equal 1)
             (expect (buffer-string)
                     :to-match "- \\[\\[file:files/HW/Adams__Alice/hw\\.pdf\\]\\[hw\\.pdf\\]\\] (\\[\\[https://example\\.com/files/1/download\\]\\[Canvas\\]\\])\n\n\\*\\* Comments")
             (expect (buffer-modified-p) :to-be nil)
             (org-canvas-submissions-download-attachments)
             (expect calls :to-equal 1)))
        (delete-directory dir t)))))

(describe "org-canvas-submissions-apply-completion-rule"
  (it "scores the ungraded by status and lateness and leaves graded or excused rows alone"
    (with-grading-file (concat test-grading-file-header
                               "#+PROPERTY: POINTS_POSSIBLE 5\n"
                               "* A\n:PROPERTIES:\n:USER_ID: 1\n:STATUS: submitted\n:END:\n"
                               "* B\n:PROPERTIES:\n:USER_ID: 2\n:STATUS: late\n:DAYS_LATE: 1\n:END:\n"
                               "* C\n:PROPERTIES:\n:USER_ID: 3\n:STATUS: late\n:DAYS_LATE: 4\n:END:\n"
                               "* D\n:PROPERTIES:\n:USER_ID: 4\n:STATUS: missing\n:END:\n"
                               "* E\n:PROPERTIES:\n:USER_ID: 5\n:STATUS: graded\n:SCORE: 3\n:CANVAS_SCORE: 3\n:END:\n"
                               "* F\n:PROPERTIES:\n:USER_ID: 6\n:STATUS: graded\n:SCORE: EX\n:CANVAS_SCORE: EX\n:END:\n")
      (expect (org-canvas--submissions-default-points) :to-equal 5)
      (org-canvas-submissions-apply-completion-rule 5)
      (let ((scores (mapcar (lambda (uid) (org-canvas--submissions-goto-user uid) (org-entry-get (point) "SCORE"))
                            '(1 2 3 4 5 6))))
        (expect scores :to-equal '("5" "5" "0" "0" "3" "EX")))
      (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 4)))
  (it "overwrites hand grades only with the prefix argument"
    (with-grading-file (concat test-grading-file-header
                               "* E\n:PROPERTIES:\n:USER_ID: 5\n:STATUS: submitted\n:SCORE: 3\n:CANVAS_SCORE: 3\n:END:\n")
      (org-canvas-submissions-apply-completion-rule 10)
      (org-canvas--submissions-goto-user 5)
      (expect (org-entry-get (point) "SCORE") :to-equal "3")
      (org-canvas-submissions-apply-completion-rule 10 t)
      (org-canvas--submissions-goto-user 5)
      (expect (org-entry-get (point) "SCORE") :to-equal "10")))
  (it "honors the late window setting"
    (let ((org-canvas-submissions-late-window-days 5))
      (with-grading-file (concat test-grading-file-header
                                 "* C\n:PROPERTIES:\n:USER_ID: 3\n:STATUS: late\n:DAYS_LATE: 4\n:END:\n")
        (org-canvas-submissions-apply-completion-rule 5)
        (org-canvas--submissions-goto-user 3)
        (expect (org-entry-get (point) "SCORE") :to-equal "5"))))
  (it "reads the points from the minibuffer, after the buffer checks, when none are given"
    (with-grading-file (concat test-grading-file-header
                               "#+PROPERTY: POINTS_POSSIBLE 5\n"
                               "* A\n:PROPERTIES:\n:USER_ID: 1\n:STATUS: submitted\n:END:\n"
                               "* E\n:PROPERTIES:\n:USER_ID: 5\n:STATUS: submitted\n:SCORE: 3\n:CANVAS_SCORE: 3\n:END:\n")
      (let ((default nil))
        (cl-letf (((symbol-function 'read-number)
                   (lambda (_prompt &optional d) (setq default d) 7)))
          (let ((current-prefix-arg '(4)))
            (org-canvas-submissions-apply-completion-rule)))
        (expect default :to-equal 5)
        (org-canvas--submissions-goto-user 1)
        (expect (org-entry-get (point) "SCORE") :to-equal "7")
        (org-canvas--submissions-goto-user 5)
        (expect (org-entry-get (point) "SCORE") :to-equal "7"))))
  (it "checks the buffer before prompting"
    (with-temp-buffer
      (let ((prompted nil))
        (cl-letf (((symbol-function 'read-number)
                   (lambda (&rest _) (setq prompted t) 1)))
          (expect (org-canvas-submissions-apply-completion-rule) :to-throw 'user-error))
        (expect prompted :to-be nil))))
  (it "refuses in the summary view"
    (with-grading-file (concat test-grading-file-header
                               "* A\n:PROPERTIES:\n:USER_ID: 1\n:STATUS: submitted\n:END:\n")
      (setq-local org-canvas-submissions--current-view 'summary)
      (expect (org-canvas-submissions-apply-completion-rule 5) :to-throw 'user-error))))

;;;; Helpers and guards only a live grading session reached

(describe "org-canvas-submissions-download-all-attachments guards"
  (it "refuses outside a submissions buffer"
    (with-temp-buffer
      (expect (org-canvas-submissions-download-all-attachments)
              :to-throw 'user-error)))

  (it "refuses in the summary view"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (setq-local org-canvas-submissions--current-view 'summary)
      (expect (org-canvas-submissions-download-all-attachments)
              :to-throw 'user-error))))

(describe "org-canvas--submissions-grading-buffer"
  (it "returns the visited buffer, and the display turns on org-mode before reading it"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (scratch (generate-new-buffer " *grading-plain*")))
      (unwind-protect
          (progn
            (with-current-buffer scratch
              (insert test-grading-file-header
                      "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--find-file-noselect)
                       (lambda (&rest _) scratch))
                      ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
                      ((symbol-function 'switch-to-buffer) (lambda (b) b)))
              (expect (org-canvas--submissions-grading-buffer "HW") :to-be scratch)
              (expect (buffer-local-value 'major-mode scratch) :to-be 'fundamental-mode)
              (org-canvas--submissions-display "HW" "1001" (list (test-org-canvas-make-submission)) 'detail)
              (expect (buffer-local-value 'major-mode scratch) :to-be 'org-mode)
              (with-current-buffer scratch
                (org-canvas--submissions-goto-user 5001)
                (expect (org-entry-get (point) "SCORE") :to-equal "95"))))
        (kill-buffer scratch)
        (delete-directory dir t)))))

(describe "completion rule on a rubric assignment"
  (it "fills every row at its Max for full credit and leaves them empty for a 0"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":STATUS: submitted\n" test-rubric-blank-rows)
                               (test-rubric-entry "Beta, Bob" 5002 ":STATUS: missing\n" test-rubric-blank-rows)
                               (test-rubric-entry "Gamma, Gus" 5003 ":STATUS: graded\n:SCORE: 4\n:CANVAS_SCORE: 4\n"
                                                  test-rubric-blank-rows))
      (org-canvas-submissions-apply-completion-rule 6)
      (org-canvas--submissions-goto-user 5001)
      (expect (org-entry-get (point) "SCORE") :to-equal "6")
      (expect (mapcar (lambda (r) (nth 3 r)) (org-canvas--submissions-rubric-rows))
              :to-equal '("2" "3" "1"))
      (org-canvas--submissions-goto-user 5002)
      (expect (org-entry-get (point) "SCORE") :to-equal "0")
      (expect (mapcar (lambda (r) (nth 3 r)) (org-canvas--submissions-rubric-rows))
              :to-equal '(nil nil nil))
      (org-canvas--submissions-goto-user 5003)
      (expect (mapcar (lambda (r) (nth 3 r)) (org-canvas--submissions-rubric-rows))
              :to-equal '(nil nil nil))
      ;; The filled rows total the score, so the push derives nothing new.
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :user-id) :to-equal 5001)
        (expect (plist-get change :new-score) :to-equal "6")
        (expect (plist-get change :filled) :to-equal 3)))))

(describe "rubric rows carry over a re-pull"
  (it "keeps typed rows, and marks the heading when Canvas assessed it meanwhile"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (shown (list nil)))
      (unwind-protect
          (progn
            (test-rubric--display (list (test-org-canvas-make-submission '((score . nil)))) shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (org-canvas--submissions-rubric-set-row "_7104" "2" "Sharp")
              (org-canvas--submissions-rubric-set-row "_7106" "1" nil)
              (save-buffer))
            ;; Nothing moved on Canvas: the rows come back, no conflict.
            (test-rubric--display (list (test-org-canvas-make-submission '((score . nil)))) shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (expect (mapcar (lambda (r) (list (nth 3 r) (nth 4 r))) (org-canvas--submissions-rubric-rows))
                      :to-equal '(("2" "Sharp") (nil nil) ("1" nil)))
              (expect (org-entry-get (point) "CONFLICT") :to-be nil)
              (expect (count-matches "^\\*\\* Rubric$" (point-min) (point-max)) :to-equal 1))
            ;; Someone assessed in SpeedGrader: the rows still come back, marked.
            (test-rubric--display
             (list (test-org-canvas-make-submission
                    '((score . 3) (rubric_assessment . ((_7105 . ((points . 3))))))))
             shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (expect (nth 3 (car (org-canvas--submissions-rubric-rows))) :to-equal "2")
              (expect (org-entry-get (point) "CONFLICT") :to-equal "rubric: assessed on Canvas since these rows were typed")
              (expect (org-entry-get (point) "CANVAS_RUBRIC")
                      :to-equal (org-canvas--submissions-rubric-digest '(("_7105" "3" nil))))))
        (when (buffer-live-p (car shown)) (kill-buffer (car shown)))
        (delete-directory dir t))))
  (it "does not carry rows that match their baseline, so a fresh pull shows Canvas"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (shown (list nil))
           (assessed (test-org-canvas-make-submission
                      '((score . 2) (rubric_assessment . ((_7104 . ((points . 2)))))))))
      (unwind-protect
          (progn
            (test-rubric--display (list assessed) shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (expect (org-canvas--submissions-rubric-carryover) :to-be nil))
            (test-rubric--display
             (list (test-org-canvas-make-submission
                    '((score . 3) (rubric_assessment . ((_7104 . ((points . 1))) (_7105 . ((points . 2))))))))
             shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (expect (mapcar (lambda (r) (nth 3 r)) (org-canvas--submissions-rubric-rows))
                      :to-equal '("1" "2" nil))
              (expect (org-entry-get (point) "CONFLICT") :to-be nil)))
        (when (buffer-live-p (car shown)) (kill-buffer (car shown)))
        (delete-directory dir t)))))

(provide 'org-canvas-submissions-entry-test)
;;; org-canvas-submissions-entry-test.el ends here
