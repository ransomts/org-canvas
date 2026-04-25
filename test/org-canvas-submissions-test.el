;;; org-canvas-submissions-test.el --- Tests for org-canvas-submissions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for the submissions viewer module.

;;; Code:

(require 'test-helper)
(require 'org-canvas-submissions)

;;;; Mock Data Builders

(defun test-org-canvas-make-user (&optional overrides)
  "Build a mock Canvas user alist."
  (test-org-canvas-make-response
   '((id . 5001)
     (name . "Alice Adams")
     (sortable_name . "Adams, Alice"))
   overrides))

(defun test-org-canvas-make-submission (&optional overrides)
  "Build a mock Canvas submission alist."
  (test-org-canvas-make-response
   `((id . 50001)
     (user_id . 5001)
     (assignment_id . 1001)
     (workflow_state . "submitted")
     (submitted_at . "2026-02-15T23:45:00Z")
     (late . :json-false)
     (missing . :json-false)
     (score . 92)
     (body . "Here is my solution.")
     (user . ,(test-org-canvas-make-user))
     (assignment . ((points_possible . 100)))
     (submission_comments . [])
     (rubric_assessment . nil)
     (attachments . []))
   overrides))

(defun test-org-canvas-make-submission-with-comment (&optional overrides)
  "Build a mock submission with a comment."
  (test-org-canvas-make-submission
   (append
    `((submission_comments
       . [((author_name . "Prof. Smith")
           (comment . "Good work!")
           (created_at . "2026-02-16T10:00:00Z"))]))
    overrides)))

(defun test-org-canvas-make-submission-with-attachment (&optional overrides)
  "Build a mock submission with an attachment."
  (test-org-canvas-make-submission
   (append
    `((attachments
       . [((display_name . "homework1.pdf")
           (url . "https://canvas.example.com/files/999/download"))]))
    overrides)))

(defun test-org-canvas-make-submission-with-rubric (&optional overrides)
  "Build a mock submission with rubric assessment."
  (test-org-canvas-make-submission
   (append
    `((rubric_assessment
       . ((crit_1 . ((points . 18)
                     (rating_description . "Excellent")))
          (crit_2 . ((points . 15)
                     (rating_description . "Good"))))))
    overrides)))

;;;; Status Normalization

(describe "org-canvas--submissions-normalize-status"
  (it "returns submitted for normal submissions"
    (expect (org-canvas--submissions-normalize-status
             (test-org-canvas-make-submission))
            :to-equal 'submitted))

  (it "returns late when late flag is set"
    (expect (org-canvas--submissions-normalize-status
             (test-org-canvas-make-submission '((late . t))))
            :to-equal 'late))

  (it "returns missing when missing flag is set"
    (expect (org-canvas--submissions-normalize-status
             (test-org-canvas-make-submission '((missing . t))))
            :to-equal 'missing))

  (it "returns graded for graded submissions"
    (expect (org-canvas--submissions-normalize-status
             (test-org-canvas-make-submission '((workflow_state . "graded"))))
            :to-equal 'graded))

  (it "returns unsubmitted for unsubmitted state"
    (expect (org-canvas--submissions-normalize-status
             (test-org-canvas-make-submission '((workflow_state . "unsubmitted")
                                               (score . nil))))
            :to-equal 'unsubmitted))

  (it "returns pending_review for pending state"
    (expect (org-canvas--submissions-normalize-status
             (test-org-canvas-make-submission '((workflow_state . "pending_review"))))
            :to-equal 'pending_review))

  (it "missing takes priority over late"
    (expect (org-canvas--submissions-normalize-status
             (test-org-canvas-make-submission '((missing . t) (late . t))))
            :to-equal 'missing)))

;;;; Statistics

(describe "org-canvas--submissions-compute-stats"
  (it "counts statuses correctly"
    (let* ((subs (list
                  (test-org-canvas-make-submission)
                  (test-org-canvas-make-submission '((late . t) (score . 85)))
                  (test-org-canvas-make-submission '((missing . t) (score . nil)))))
           (stats (org-canvas--submissions-compute-stats subs)))
      (expect (plist-get stats :submitted) :to-equal 1)
      (expect (plist-get stats :late) :to-equal 1)
      (expect (plist-get stats :missing) :to-equal 1)
      (expect (plist-get stats :total) :to-equal 3)))

  (it "computes average from scored submissions"
    (let* ((subs (list
                  (test-org-canvas-make-submission '((score . 90)))
                  (test-org-canvas-make-submission '((score . 80)))))
           (stats (org-canvas--submissions-compute-stats subs)))
      (expect (plist-get stats :average) :to-equal 85.0)))

  (it "returns nil average when no scores"
    (let* ((subs (list
                  (test-org-canvas-make-submission '((score . nil)))))
           (stats (org-canvas--submissions-compute-stats subs)))
      (expect (plist-get stats :average) :to-equal nil)))

  (it "counts graded status"
    (let* ((subs (list
                  (test-org-canvas-make-submission
                   '((workflow_state . "graded") (score . 95)))))
           (stats (org-canvas--submissions-compute-stats subs)))
      (expect (plist-get stats :graded) :to-equal 1))))

;;;; Format Helpers

(describe "org-canvas--submissions-format-stats"
  (it "formats a stats line"
    (let ((stats '(:submitted 30 :missing 2 :late 1 :graded 0
                   :total 33 :average 85.3 :points 100)))
      (expect (org-canvas--submissions-format-stats stats)
              :to-match "30 submitted")
      (expect (org-canvas--submissions-format-stats stats)
              :to-match "2 missing")
      (expect (org-canvas--submissions-format-stats stats)
              :to-match "1 late")
      (expect (org-canvas--submissions-format-stats stats)
              :to-match "Average: 85.3/100")))

  (it "includes graded count when present"
    (let ((stats '(:submitted 5 :missing 0 :late 0 :graded 3
                   :total 8 :average nil :points nil)))
      (expect (org-canvas--submissions-format-stats stats)
              :to-match "3 graded"))))

(describe "org-canvas--submissions-format-number"
  (it "formats integers without decimals"
    (expect (org-canvas--submissions-format-number 92.0) :to-equal "92"))

  (it "formats non-integers with one decimal"
    (expect (org-canvas--submissions-format-number 85.5) :to-equal "85.5")))

(describe "org-canvas--submissions-format-score"
  (it "formats score/points"
    (let ((sub (test-org-canvas-make-submission '((score . 92)))))
      (expect (org-canvas--submissions-format-score sub)
              :to-equal "92/100")))

  (it "returns empty string when no score"
    (let ((sub (test-org-canvas-make-submission '((score . nil)))))
      (expect (org-canvas--submissions-format-score sub)
              :to-equal "")))

  (it "formats score without points when points_possible is nil"
    (let ((sub (test-org-canvas-make-submission
                '((score . 88) (assignment . nil)))))
      (expect (org-canvas--submissions-format-score sub)
              :to-equal "88"))))

(describe "org-canvas--submissions-user-sortable-name"
  (it "returns sortable_name from user"
    (let ((sub (test-org-canvas-make-submission)))
      (expect (org-canvas--submissions-user-sortable-name sub)
              :to-equal "Adams, Alice")))

  (it "falls back to name when sortable_name is nil"
    (let ((sub (test-org-canvas-make-submission
                '((user . ((id . 5001) (name . "Alice") (sortable_name . nil)))))))
      (expect (org-canvas--submissions-user-sortable-name sub)
              :to-equal "Alice")))

  (it "returns Unknown when no user"
    (let ((sub (test-org-canvas-make-submission '((user . nil)))))
      (expect (org-canvas--submissions-user-sortable-name sub)
              :to-equal "Unknown"))))

(describe "org-canvas--submissions-sanitize-filename"
  (it "replaces spaces and special chars"
    (expect (org-canvas--submissions-sanitize-filename "Homework 1: Test")
            :to-equal "Homework_1__Test"))

  (it "preserves alphanumeric and dots"
    (expect (org-canvas--submissions-sanitize-filename "file.txt")
            :to-equal "file.txt")))

;;;; Summary View Rendering

(describe "org-canvas--submissions-render-summary"
  (it "renders a valid org table"
    (let ((subs (list (test-org-canvas-make-submission))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-summary "Homework 1" "1001" subs)
        (expect (buffer-string) :to-match "\\+TITLE: Submissions: Homework 1")
        (expect (buffer-string) :to-match "CANVAS_ASSIGNMENT_ID 1001")
        (expect (buffer-string) :to-match "Adams, Alice")
        (expect (buffer-string) :to-match "submitted")
        (expect (buffer-string) :to-match "92/100"))))

  (it "sorts students alphabetically"
    (let ((subs (list
                 (test-org-canvas-make-submission
                  '((user . ((id . 5002)
                             (sortable_name . "Zeta, Zara")
                             (name . "Zara Zeta")))))
                 (test-org-canvas-make-submission))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-summary "HW" "1" subs)
        (let ((content (buffer-string)))
          (expect (string-match "Adams" content) :to-be-truthy)
          (expect (string-match "Zeta" content) :to-be-truthy)
          (expect (string-match "Adams" content)
                  :to-be-less-than
                  (string-match "Zeta" content))))))

  (it "includes stats line"
    (let ((subs (list
                 (test-org-canvas-make-submission)
                 (test-org-canvas-make-submission '((missing . t) (score . nil))))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-summary "HW" "1" subs)
        (expect (buffer-string) :to-match "1 submitted")
        (expect (buffer-string) :to-match "1 missing")))))

;;;; Detail View Rendering

(describe "org-canvas--submissions-render-detail"
  (it "renders student headings with properties"
    (let ((subs (list (test-org-canvas-make-submission))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1001" subs)
        (expect (buffer-string) :to-match "^\\* Adams, Alice")
        (expect (buffer-string) :to-match ":USER_ID: 5001")
        (expect (buffer-string) :to-match ":SUBMISSION_ID: 50001")
        (expect (buffer-string) :to-match ":STATUS: submitted")
        (expect (buffer-string) :to-match ":SCORE: 92"))))

  (it "renders submission body"
    (let ((subs (list (test-org-canvas-make-submission
                       '((body . "My answer is 42."))))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1" subs)
        (expect (buffer-string) :to-match "My answer is 42."))))

  (it "converts HTML body via html-to-org"
    (cl-letf (((symbol-function 'org-canvas--html-to-org)
               (lambda (html) (concat "CONVERTED:" html))))
      (let ((subs (list (test-org-canvas-make-submission
                         '((body . "<p>Essay</p>"))))))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail "HW" "1" subs)
          (expect (buffer-string) :to-match "CONVERTED:<p>Essay</p>")))))

  (it "sorts students alphabetically"
    (let ((subs (list
                 (test-org-canvas-make-submission
                  '((user . ((id . 5002)
                             (sortable_name . "Zeta, Zara")
                             (name . "Zara Zeta")))))
                 (test-org-canvas-make-submission))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1" subs)
        (let ((content (buffer-string)))
          (expect (string-match "Adams" content)
                  :to-be-less-than
                  (string-match "Zeta" content))))))

  (it "skips body when nil"
    (let ((subs (list (test-org-canvas-make-submission '((body . nil))))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1" subs)
        (expect (buffer-string) :not :to-match "nil")))))

(describe "org-canvas--submissions-render-attachments"
  (it "renders attachment links"
    (with-temp-buffer
      (org-canvas--submissions-render-attachments
       [((display_name . "hw.pdf")
         (url . "https://example.com/files/1/download"))])
      (expect (buffer-string) :to-match "\\*\\* Attachments")
      (expect (buffer-string) :to-match "hw.pdf")
      (expect (buffer-string) :to-match "https://example.com/files/1/download")))

  (it "skips when no attachments"
    (with-temp-buffer
      (org-canvas--submissions-render-attachments [])
      (expect (buffer-string) :to-equal "")))

  (it "skips when nil"
    (with-temp-buffer
      (org-canvas--submissions-render-attachments nil)
      (expect (buffer-string) :to-equal ""))))

(describe "org-canvas--submissions-render-comments"
  (it "renders comments with author and timestamp"
    (with-temp-buffer
      (org-canvas--submissions-render-comments
       [((author_name . "Prof. Smith")
         (comment . "Nice work!")
         (created_at . "2026-02-16T10:00:00Z"))])
      (expect (buffer-string) :to-match "\\*\\* Comments")
      (expect (buffer-string) :to-match "Prof. Smith")
      (expect (buffer-string) :to-match "Nice work!")
      (expect (buffer-string) :to-match "<2026-02-16")))

  (it "handles multiple comments"
    (with-temp-buffer
      (org-canvas--submissions-render-comments
       [((author_name . "A") (comment . "First") (created_at . "2026-01-01T00:00:00Z"))
        ((author_name . "B") (comment . "Second") (created_at . "2026-01-02T00:00:00Z"))])
      (expect (buffer-string) :to-match "First")
      (expect (buffer-string) :to-match "Second")))

  (it "skips when empty"
    (with-temp-buffer
      (org-canvas--submissions-render-comments [])
      (expect (buffer-string) :to-equal "")))

  (it "converts HTML comment text via html-to-org-inline"
    (cl-letf (((symbol-function 'org-canvas--html-to-org-inline)
               (lambda (html) (concat "INLINE:" html))))
      (with-temp-buffer
        (org-canvas--submissions-render-comments
         [((author_name . "Prof")
           (comment . "<b>Good</b>")
           (created_at . "2026-03-01T00:00:00Z"))])
        (expect (buffer-string) :to-match "INLINE:<b>Good</b>")))))

(describe "org-canvas--submissions-render-rubric"
  (it "renders rubric as org table"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-rubric
       '((crit_1 . ((points . 18) (rating_description . "Excellent")))
         (crit_2 . ((points . 15) (rating_description . "Good")))))
      (expect (buffer-string) :to-match "\\*\\* Rubric Assessment")
      (expect (buffer-string) :to-match "Excellent")
      (expect (buffer-string) :to-match "18")))

  (it "skips when nil"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric nil)
      (expect (buffer-string) :to-equal "")))

  (it "handles hash-table rubric"
    (let ((ht (make-hash-table :test 'equal)))
      (puthash 'crit_1 '((points . 20) (rating_description . "Great")) ht)
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-rubric ht)
        (expect (buffer-string) :to-match "Great")
        (expect (buffer-string) :to-match "20"))))

  (it "falls back to rating_id when description is nil"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-rubric
       '((crit_1 . ((points . 10) (rating_id . "rat_42")))))
      (expect (buffer-string) :to-match "rat_42"))))

;;;; View Toggle

(describe "org-canvas-submissions-toggle-view"
  (it "switches from summary to detail"
    (let ((subs (list (test-org-canvas-make-submission))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-summary "HW" "1" subs)
        (setq-local org-canvas-submissions--assignment-name "HW")
        (setq-local org-canvas-submissions--assignment-id "1")
        (setq-local org-canvas-submissions--data subs)
        (setq-local org-canvas-submissions--current-view 'summary)
        (org-canvas-submissions-mode 1)
        (org-canvas-submissions-toggle-view)
        (expect org-canvas-submissions--current-view :to-equal 'detail)
        (expect (buffer-string) :to-match "^\\* Adams, Alice"))))

  (it "switches from detail to summary"
    (let ((subs (list (test-org-canvas-make-submission))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1" subs)
        (setq-local org-canvas-submissions--assignment-name "HW")
        (setq-local org-canvas-submissions--assignment-id "1")
        (setq-local org-canvas-submissions--data subs)
        (setq-local org-canvas-submissions--current-view 'detail)
        (org-canvas-submissions-mode 1)
        (org-canvas-submissions-toggle-view)
        (expect org-canvas-submissions--current-view :to-equal 'summary)
        (expect (buffer-string) :to-match "| Adams, Alice")))))

;;;; View Toggle Error Paths

(describe "org-canvas-submissions-toggle-view error paths"
  (it "errors when not in submissions mode"
    (with-temp-buffer
      (expect (org-canvas-submissions-toggle-view) :to-throw 'user-error)))

  (it "errors when no cached data"
    (with-temp-buffer
      (org-mode)
      (org-canvas-submissions-mode 1)
      (setq-local org-canvas-submissions--data nil)
      (setq-local org-canvas-submissions--current-view 'summary)
      (expect (org-canvas-submissions-toggle-view) :to-throw 'user-error))))

;;;; Comment Writing

(describe "org-canvas-submissions-add-comment"
  (it "posts comment via interactive flow"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n"
         (org-back-to-heading)
         (setq-local org-canvas-submissions--assignment-id "1001")
         (setq-local org-canvas-submissions--current-view 'detail)
         (org-canvas-submissions-mode 1)
         (cl-letf (((symbol-function 'read-string) (lambda (_) "Nice!"))
                   ((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas-submissions-add-comment))
         (expect-api-called 'PUT "assignments/1001/submissions/50001")
         (expect (buffer-string) :to-match "Nice!")))))

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
     "* Adams, Alice\n:PROPERTIES:\n:SUBMISSION_ID: 50001\n:END:\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (org-canvas-submissions-mode 1)
     (cl-letf (((symbol-function 'read-string) (lambda (_) "")))
       (expect (org-canvas-submissions-add-comment) :to-throw 'user-error))))

  (it "errors when no SUBMISSION_ID"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:END:\n"
     (org-back-to-heading)
     (setq-local org-canvas-submissions--current-view 'detail)
     (org-canvas-submissions-mode 1)
     (expect (org-canvas-submissions-add-comment) :to-throw 'user-error))))

(describe "org-canvas--submissions-post-comment"
  (it "sends PUT request with comment data"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--submissions-post-comment "1001" "50001" "Great job!")
        (expect-api-called 'PUT "assignments/1001/submissions/50001")
        (let* ((call (test-org-canvas-last-api-call))
               (data (nth 2 call)))
          (expect (alist-get 'text_comment (alist-get 'comment data))
                  :to-equal "Great job!"))))))

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

;;;; Entry Point (org-canvas-pull-submissions)

(describe "org-canvas-pull-submissions"
  (it "fetches assignments and submissions"
    (with-org-canvas-test-config
      (let ((assignments-fetched nil)
            (submissions-fetched nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
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
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     (cl-incf fetch-count)
                     (list (test-org-canvas-make-submission))))
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

(describe "org-canvas-submissions-mode"
  (it "does not set buffer-read-only (writable for grade editing)"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (expect buffer-read-only :to-be nil)))

  (it "defines expected keybindings"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "g"))
              :to-equal #'org-canvas-submissions-refresh)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "v"))
              :to-equal #'org-canvas-submissions-toggle-view)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "d"))
              :to-equal #'org-canvas-submissions-download-attachments)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "c"))
              :to-equal #'org-canvas-submissions-add-comment))))

;;;; Full Detail Rendering Integration

(describe "detail view integration"
  (it "renders complete detail with comments, attachments, and rubric"
    (let ((sub (test-org-canvas-make-submission
                `((submission_comments
                   . [((author_name . "Prof. Smith")
                       (comment . "Good work!")
                       (created_at . "2026-02-16T10:00:00Z"))])
                  (attachments
                   . [((display_name . "homework1.pdf")
                       (url . "https://canvas.example.com/files/999/download"))])
                  (rubric_assessment
                   . ((crit_1 . ((points . 18)
                                 (rating_description . "Excellent")))))))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW 1" "1001" (list sub))
        (let ((content (buffer-string)))
          ;; Header
          (expect content :to-match "^\\* Adams, Alice")
          (expect content :to-match ":SCORE: 92")
          ;; Body
          (expect content :to-match "Here is my solution")
          ;; Attachments
          (expect content :to-match "homework1.pdf")
          ;; Comments
          (expect content :to-match "Prof. Smith")
          (expect content :to-match "Good work!")
          ;; Rubric
          (expect content :to-match "Excellent")
          (expect content :to-match "18"))))))

;;;; Edge Cases

(describe "edge cases"
  (it "handles empty submissions list in summary"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-summary "HW" "1" nil)
      (expect (buffer-string) :to-match "\\+TITLE")))

  (it "handles submission with no user"
    (let ((sub (test-org-canvas-make-submission '((user . nil)))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1" (list sub))
        (expect (buffer-string) :to-match "Unknown"))))

  (it "handles submission with no score"
    (let ((sub (test-org-canvas-make-submission '((score . nil)))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-summary "HW" "1" (list sub))
        ;; Should not crash; score column should be empty
        (expect (buffer-string) :to-match "Adams, Alice"))))

  (it "handles json-false for late/missing"
    (let ((sub (test-org-canvas-make-submission
                '((late . :json-false) (missing . :json-false)))))
      (expect (org-canvas--submissions-normalize-status sub)
              :to-equal 'submitted))))

;;;; File Download

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
      (expect copied-url :to-match "&access_token=test-token"))))

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

(describe "org-canvas-submissions-push-grades error path"
  (it "errors when not in submissions mode"
    (with-temp-buffer
      (expect (org-canvas-submissions-push-grades) :to-throw 'user-error))))

;;;; Score Parsing

(describe "org-canvas--submissions-parse-score"
  (it "parses integer string"
    (expect (org-canvas--submissions-parse-score "92") :to-equal "92"))

  (it "parses decimal string"
    (expect (org-canvas--submissions-parse-score "85.5") :to-equal "85.5"))

  (it "extracts numerator from score/points format"
    (expect (org-canvas--submissions-parse-score "95/100") :to-equal "95"))

  (it "trims whitespace"
    (expect (org-canvas--submissions-parse-score " 92 ") :to-equal "92"))

  (it "returns nil for non-numeric input"
    (expect (org-canvas--submissions-parse-score "abc") :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--submissions-parse-score "") :to-be nil))

  (it "returns nil for nil"
    (expect (org-canvas--submissions-parse-score nil) :to-be nil)))

;;;; Snapshot Scores

(describe "org-canvas--submissions-snapshot-scores"
  (it "builds alist from submissions"
    (let* ((subs (list (test-org-canvas-make-submission '((score . 92)))
                       (test-org-canvas-make-submission
                        '((score . nil)
                          (user . ((id . 5002) (sortable_name . "Beta, Bob")))))))
           (snapshot (org-canvas--submissions-snapshot-scores subs)))
      (expect (alist-get 5001 snapshot) :to-equal "92")
      (expect (alist-get 5002 snapshot) :to-be nil)))

  (it "formats decimal scores"
    (let* ((subs (list (test-org-canvas-make-submission '((score . 85.5)))))
           (snapshot (org-canvas--submissions-snapshot-scores subs)))
      (expect (alist-get 5001 snapshot) :to-equal "85.5"))))

;;;; Change Detection — Detail View

(describe "org-canvas--submissions-collect-detail-changes"
  (it "detects changed score"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (setq-local org-canvas-submissions--data nil)
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :user-id) :to-equal 5001)
       (expect (plist-get (car changes) :new-score) :to-equal "95")
       (expect (plist-get (car changes) :old-score) :to-equal "92"))))

  (it "ignores unchanged scores"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 0))))

  (it "detects new score where none existed"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 88\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . nil)))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :new-score) :to-equal "88"))))

  (it "detects multiple changes"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores
                 '((5001 . "92") (5002 . "75")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 2))))

  (it "handles score/points format in SCORE property"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95/100\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (plist-get (car changes) :new-score) :to-equal "95")))))

;;;; Change Detection — Dispatcher

(describe "org-canvas--submissions-collect-grade-changes"
  (it "dispatches to detail collector in detail view"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (let ((changes (org-canvas--submissions-collect-grade-changes)))
       (expect (length changes) :to-equal 1))))

  (it "dispatches to summary collector in summary view"
    (with-temp-buffer
      (org-mode)
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 99 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-grade-changes)))
        (expect (length changes) :to-equal 1)))))

;;;; Change Detection — Summary View

(describe "org-canvas--submissions-collect-summary-changes"
  (it "detects changed score in table"
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Submissions: HW\n\n")
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 95/100 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-summary-changes)))
        (expect (length changes) :to-equal 1)
        (expect (plist-get (car changes) :new-score) :to-equal "95"))))

  (it "ignores unchanged scores in table"
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Submissions: HW\n\n")
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 92/100 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-summary-changes)))
        (expect (length changes) :to-equal 0))))

  (it "maps student name to user ID via cached data"
    (with-temp-buffer
      (org-mode)
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 99 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-summary-changes)))
        (expect (plist-get (car changes) :user-id) :to-equal 5001)))))

;;;; Push Functions

(describe "org-canvas--submissions-push-single-grade"
  (it "sends PUT with correct payload"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--submissions-push-single-grade "1001" 5001 "95")
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
        (let* ((call (test-org-canvas-last-api-call))
               (data (nth 2 call)))
          (expect (alist-get 'posted_grade (alist-get 'submission data))
                  :to-equal "95"))))))

(describe "org-canvas--submissions-push-bulk-grades"
  (it "sends POST with grade_data payload"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((changes (list (list :user-id 5001 :name "A" :old-score "90" :new-score "95")
                             (list :user-id 5002 :name "B" :old-score "80" :new-score "85"))))
          (org-canvas--submissions-push-bulk-grades "1001" changes)
          (expect-api-called 'POST "assignments/1001/submissions/update_grades")
          (let* ((call (test-org-canvas-last-api-call))
                 (data (nth 2 call))
                 (grade-data (alist-get 'grade_data data)))
            (expect (alist-get 'posted_grade (alist-get "5001" grade-data nil nil #'equal))
                    :to-equal "95")
            (expect (alist-get 'posted_grade (alist-get "5002" grade-data nil nil #'equal))
                    :to-equal "85")))))))

;;;; Push Command

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
      (org-canvas-submissions-push-grades)
      ;; No error = success; function returns after "No grade changes" message
      ))

  (it "dispatches single change to PUT"
    (with-org-canvas-test-config
      (with-mock-api
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
          (expect-api-called 'PUT "assignments/1001/submissions/5001")))))

  (it "dispatches multiple changes to bulk POST"
    (with-org-canvas-test-config
      (with-mock-api
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
          (expect-api-called 'POST "assignments/1001/submissions/update_grades")))))

  (it "respects confirmation prompt denial"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-buffer
          (org-mode)
          (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n")
          (setq-local org-canvas-submissions--current-view 'detail)
          (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
          (setq-local org-canvas-submissions--assignment-id "1001")
          (setq-local org-canvas-submissions--data nil)
          (org-canvas-submissions-mode 1)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
            (org-canvas-submissions-push-grades))
          (expect (test-org-canvas-api-call-count) :to-equal 0)))))

  (it "updates original-scores after push"
    (with-org-canvas-test-config
      (with-mock-api
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
                  :to-equal "95")))))

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

(describe "submissions grade keybinding"
  (it "binds S to push-grades"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "S"))
            :to-equal #'org-canvas-submissions-push-grades)))

;;;; Minor Mode — Writable Buffer

(describe "submissions buffer writability"
  (it "does not set buffer-read-only"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (expect buffer-read-only :to-be nil))))

;;;; Integration — Detail Edit → Push Flow

(describe "detail edit → push integration"
  (it "full flow: render, edit score, push"
    (with-org-canvas-test-config
      (with-mock-api
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
                    :to-equal "98")))))))

;;;; Integration — Refresh Resets Scores

(describe "refresh resets original-scores"
  (it "repopulates snapshot after refresh"
    (with-org-canvas-test-config
      (let ((target-buf nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     (list (test-org-canvas-make-submission '((score . 99))))))
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

(describe "unsubmitted student grading"
  (it "detects nil→score transition"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 75\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . nil)))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :old-score) :to-be nil)
       (expect (plist-get (car changes) :new-score) :to-equal "75")))))

(provide 'org-canvas-submissions-test)
;;; org-canvas-submissions-test.el ends here
