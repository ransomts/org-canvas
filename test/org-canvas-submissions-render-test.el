;;; org-canvas-submissions-render-test.el --- Tests for org-canvas-submissions-render -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for the submissions viewer's rendering.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-render)

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

  (it "falls back to User <id> from the submission's user_id when the user object is missing"
    (let ((sub (test-org-canvas-make-submission '((user . nil)))))
      (expect (org-canvas--submissions-user-sortable-name sub)
              :to-equal "User 5001")))

  (it "returns Unknown only when there is no user and no user_id"
    (let ((sub (test-org-canvas-make-submission '((user . nil) (user_id . nil)))))
      (expect (org-canvas--submissions-user-sortable-name sub)
              :to-equal "Unknown"))))

(describe "org-canvas--submissions-user-id"
  (it "reads the id from the included user object"
    (expect (org-canvas--submissions-user-id (test-org-canvas-make-submission))
            :to-equal 5001))

  (it "falls back to the submission's top-level user_id"
    (let ((sub (test-org-canvas-make-submission '((user . nil) (user_id . 7007)))))
      (expect (org-canvas--submissions-user-id sub) :to-equal 7007)))

  (it "is nil when neither is present"
    (let ((sub (test-org-canvas-make-submission '((user . nil) (user_id . nil)))))
      (expect (org-canvas--submissions-user-id sub) :to-be nil))))

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

  (it "keeps a comment's paragraphs under the item, and a one-liner on its line (issue #264)"
    (cl-letf (((symbol-function 'org-canvas--html-to-org)
               (lambda (html)
                 (pcase html
                   ("<b>Good</b>" "*Good*")
                   (_ "This upload is your R1 responses.\\\\\n\n#+begin_h2\nNext\n#+end_h2\n\nUpload the R2 arguments and I will regrade it.")))))
      (with-temp-buffer
        (org-canvas--submissions-render-comments
         [((author_name . "Prof") (comment . "<b>Good</b>") (created_at . "2026-03-01T00:00:00Z"))
          ((author_name . "Prof") (comment . "<p>two</p><h2>Next</h2><p>paragraphs</p>")
           (created_at . "2026-03-02T00:00:00Z"))])
        (expect (buffer-string)
                :to-equal (concat "\n** Comments\n"
                                  "- *Prof* <2026-03-01 Sun 00:00> :: *Good*\n"
                                  "- *Prof* <2026-03-02 Mon 00:00> ::\n"
                                  "  This upload is your R1 responses.\n\n"
                                  "  Next\n\n"
                                  "  Upload the R2 arguments and I will regrade it.\n")))))
  (it "renders an empty item for a comment without text"
    (with-temp-buffer
      (org-canvas--submissions-render-comments
       [((author_name . "Prof") (comment . :null) (created_at . "2026-03-01T00:00:00Z"))])
      (expect (buffer-string) :to-match "^- \\*Prof\\* <2026-03-01 Sun 00:00> ::$"))))

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
          ;; Rubric: the assessment alone, since no assignment was given
          (expect content :to-match "^\\*\\* Rubric$")
          (expect content :to-match "| crit_1 *| *| *| *18 *|$")
          (expect content :to-match "^- crit_1 ::$"))))))

(describe "org-canvas--submissions-body-text"
  (it "drops pandoc's hard-break markup and collapses the blank lines it leaves (issue #266)"
    (cl-letf (((symbol-function 'org-canvas--html-to-org)
               (lambda (_html)
                 "I would lean toward pragmatism. \\\\\nI think the hardest is determinism.\\\\\n\\\\\n  \\\\\nLast line.")))
      (expect (org-canvas--submissions-body-text "<p>x</p>")
              :to-equal "I would lean toward pragmatism.\nI think the hardest is determinism.\n\nLast line.")))
  (it "is nil for an empty or blank body"
    (expect (org-canvas--submissions-body-text nil) :to-be nil)
    (expect (org-canvas--submissions-body-text "") :to-be nil)
    (cl-letf (((symbol-function 'org-canvas--html-to-org) (lambda (_html) "\\\\\n")))
      (expect (org-canvas--submissions-body-text "<br>") :to-be nil)))
  (it "leaves a backslash pair inside a line alone"
    (with-html-to-org-identity
      (expect (org-canvas--submissions-body-text "a \\\\ b") :to-equal "a \\\\ b")))
  (it "is what the submission body is rendered through"
    (cl-letf (((symbol-function 'org-canvas--html-to-org)
               (lambda (_html) "First line.\\\\\nSecond line.")))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry
         (test-org-canvas-make-submission '((body . "<p>First line.<br>Second line.</p>"))))
        (expect (buffer-string) :to-match "\nFirst line\\.\nSecond line\\.\n")
        (expect (buffer-string) :not :to-match "\\\\\\\\")))))

;;;; Edge Cases

(describe "edge cases"
  (it "handles empty submissions list in summary"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-summary "HW" "1" nil)
      (expect (buffer-string) :to-match "\\+TITLE")))

  (it "handles submission with no user by naming the row after its user_id"
    (let ((sub (test-org-canvas-make-submission '((user . nil)))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1" (list sub))
        (expect (buffer-string) :to-match "User 5001")
        (expect (buffer-string) :not :to-match "Unknown"))))

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

(describe "grading file header and baseline properties"
  (it "writes the assignment name, pull time, and per-student baselines"
    (let ((sub (test-org-canvas-make-submission
                '((entered_score . 5.0) (score . 4.0) (attempt . 2)))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1001" (list sub))
        (let ((content (buffer-string)))
          (expect content :to-match "#\\+PROPERTY: CANVAS_ASSIGNMENT_NAME HW")
          (expect content :to-match "#\\+PROPERTY: PULLED_AT <")
          (expect content :to-match ":SCORE: 5\n")
          (expect content :to-match ":CANVAS_SCORE: 5\n")
          (expect content :to-match ":FINAL_SCORE: 4\n")
          (expect content :to-match ":ATTEMPT: 2\n")))))
  (it "omits FINAL_SCORE when nothing was deducted and the baseline when ungraded"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail
       "HW" "1001" (list (test-org-canvas-make-submission '((score . 92)))
                         (test-org-canvas-make-submission
                          '((score . nil) (user . ((id . 5002) (sortable_name . "Beta, Bob")))))))
      (let ((content (buffer-string)))
        (expect content :to-match ":CANVAS_SCORE: 92")
        (expect content :not :to-match "FINAL_SCORE")
        (expect (with-temp-buffer (insert content) (count-matches "CANVAS_SCORE" (point-min) (point-max)))
                :to-equal 1)))))

(describe "summary of a grading file"
  (it "opens a read-only table built from the headings and returns with v"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:STATUS: submitted\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
      (let ((summary nil))
        (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) (setq summary b) b)))
          (org-canvas-submissions-toggle-view))
        (with-current-buffer summary
          (expect buffer-read-only :to-be-truthy)
          (expect (buffer-string) :to-match "| Adams, Alice *| submitted *| *| *95 *|")
          (expect org-canvas-submissions--source-file :to-equal file)
          (let ((returned nil))
            (cl-letf (((symbol-function 'find-file) (lambda (f) (setq returned f))))
              (org-canvas-submissions-toggle-view))
            (expect returned :to-equal file)))
        (kill-buffer summary)
        (expect (buffer-string) :to-match ":SCORE: 95")))))

(describe "rubric header in the grading file"
  (it "writes the rubric properties, the Rubric line, and the rubric block"
    (let ((org-canvas-base-url "https://canvas.example.edu")
          (org-canvas-course-id "42")
          (org-canvas-submissions-directory "/course/submissions/"))
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
                ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) "Essay Rubric"))
                ((symbol-function 'org-canvas--submissions-rubrics-file) (lambda () "/course/rubrics.org")))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail "Essay" "1001" nil test-rubric-assignment)
          (let ((content (buffer-string)))
            (expect content :to-match "^#\\+PROPERTY: CANVAS_RUBRIC_ID 133477$")
            (expect content :to-match "^#\\+PROPERTY: CANVAS_RUBRIC_TITLE Essay Rubric$")
            (expect content :to-match "^#\\+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING true$")
            (expect content :to-match "^Rubric: \\[\\[file:\\.\\./rubrics\\.org::\\*Essay Rubric\\]\\[in Org\\]\\], \\[\\[https://canvas\\.example\\.edu/courses/42/rubrics/133477\\]\\[on Canvas\\]\\]$")
            (expect content :to-match "^\\* Rubric\n\n\\*\\* Thesis (20)\n| Rating *| Points *| Description *|\n|[-+]+|\n| Excellent *| *20 *| *|\n| Weak vague *| *5 *| *|\n\n\\*\\* Evidence (30)\n")
            ;; the Rubric block precedes the students and follows the Assignment line
            (expect (string-match "^Assignment: " content)
                    :to-be-less-than (string-match "^Rubric: " content)))))))
  (it "reads use_rubric_for_grading from the assignment's top level (issue #253)"
    (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
              ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) nil)))
      ;; A real Assignment object: the flag beside rubric_settings, not in it.
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail
         "Essay" "1001" nil
         '((id . 1001) (use_rubric_for_grading . :json-false)
           (rubric_settings . ((id . 133477) (title . "Essay Rubric") (points_possible . 100)))))
        (expect (buffer-string) :to-match "^#\\+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING false$"))
      ;; Absent at the top level, the settings key still counts.
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail
         "Essay" "1001" nil
         '((id . 1001) (rubric_settings . ((id . 133477) (title . "Essay Rubric") (use_for_grading . t)))))
        (expect (buffer-string) :to-match "^#\\+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING true$"))
      ;; And the top level wins over it.
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail
         "Essay" "1001" nil
         '((id . 1001) (use_rubric_for_grading . :json-false)
           (rubric_settings . ((id . 133477) (title . "Essay Rubric") (use_for_grading . t)))))
        (expect (buffer-string) :to-match "^#\\+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING false$"))))
  (it "omits the criteria when the option is off"
    (let ((org-canvas-submissions-include-rubric-criteria nil))
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
                ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) nil)))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail "Essay" "1001" nil test-rubric-assignment)
          (expect (buffer-string) :to-match "^Rubric: \\[\\[https://")
          (expect (buffer-string) :not :to-match "Criterion\\|Rating\\|^\\* Rubric")))))
  (it "writes nothing rubric-related for an assignment without one"
    (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil)))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1001" nil '((id . 1001) (name . "HW")))
        (expect (buffer-string) :not :to-match "Rubric")
        (expect (buffer-string) :not :to-match "CANVAS_RUBRIC")))))

(defun test-rubric--decode (html)
  "Stand in for pandoc: decode the apostrophe and split paragraphs."
  (replace-regexp-in-string
   "&#39;" "'"
   (string-trim (replace-regexp-in-string "</?p>" "" (replace-regexp-in-string "</p><p>" "\n\n" html)))))

(describe "the rubric block in the grading file (issue #265)"
  (it "writes the rubric in the rubrics file's shape: a heading per criterion, its prose, its ratings"
    (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
              ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) nil))
              ((symbol-function 'org-canvas--html-to-org) #'test-rubric--decode))
      (with-temp-buffer
        (org-mode)
        ;; The Comment Bank heading (#352) would sit between the rubric
        ;; and the first student; its own specs place it.
        (let ((org-canvas-submissions-comment-bank-template nil))
          (org-canvas--submissions-render-detail
           "Closer" "1001" (list (test-org-canvas-make-submission)) test-rubric-assignment-described))
        (let ((content (buffer-string)))
          (expect content :to-match "^Rubric: \\[\\[https://.*/rubrics/138462\\]\\[on Canvas\\]\\]\n\n\\* Rubric\n\n\\*\\* The Day's Idea Does the Work (2)\nThe answer is built from the session's material\\.\n\nFeed-forward: journal row 2\\.\n| Rating *| Points *| Description *|\n|-+\\+-+\\+-+|\n| Working *| *2 *| The day's idea carries the answer *|\n| Named *| *1 *| Named, but the answer reads the same without it *|\n| Absent *| *0 *| *|\n\n\\*\\* Stakes | Named (4)\n| Rating *| Points *| Description *|\n|-+\\+-+\\+-+|\n| Yes *| *4 *| *|\n\n\\* Adams, Alice\n")
          (expect content :not :to-match "| Criterion *| Points *| Ratings *|")
          ;; The student's own table repeats the criterion, decoded once for all of them.
          (expect content :to-match "| _1 *| The Day's Idea Does the Work *| *2 *| *|")
          (expect content :not :to-match "&#39;")))))
  (it "writes the one-line table under `summary', decoded the same way"
    (let ((org-canvas-submissions-include-rubric-criteria 'summary))
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
                ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) nil))
                ((symbol-function 'org-canvas--html-to-org) #'test-rubric--decode))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail "Closer" "1001" nil test-rubric-assignment-described)
          (let ((content (buffer-string)))
            (expect content :to-match "| Criterion *| Points *| Ratings *|")
            (expect content :to-match "| The Day's Idea Does the Work *| *2 *| Working (2), Named (1), Absent (0) *|")
            (expect content :to-match "| Stakes Named *| *4 *| Yes (4) *|")
            (expect content :not :to-match "^\\* Rubric$"))))))
  (it "is no student: the summary, the completion rule and the pushes leave it alone"
    (with-rubric-file (concat test-rubric-file-header
                               "* Rubric\n\n** Thesis (2)\n| Rating | Points | Description |\n|---+---+---|\n| Full | 2 |  |\n\n"
                               (test-rubric-entry "Adams, Alice" 5001 ":STATUS: submitted\n" test-rubric-blank-rows))
      (expect (mapcar #'car (org-canvas--submissions-heading-rows)) :to-equal '("Adams, Alice"))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages) nil)))
          (org-canvas-submissions-apply-completion-rule 6))
        (expect (car messages) :to-match "1 at 6, 0 at 0 .*, 0 left as they were"))
      (goto-char (point-min))
      (re-search-forward "^\\* Rubric$")
      (expect (org-entry-get (point) "SCORE") :to-be nil)
      (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 1)
      (expect (org-canvas--submissions-collect-comment-drafts) :to-be nil))))

(describe "comment draft heading"
  (it "is rendered under each student with the template as comment lines"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry (test-org-canvas-make-submission))
      (expect (buffer-string) :to-match "^\\*\\* Comment to post\n# Write your comment")
      (expect (buffer-string) :to-match "Lines starting with # are never sent")))
  (it "is omitted when the template is nil"
    (let ((org-canvas-submissions-comment-template nil))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry (test-org-canvas-make-submission))
        (expect (buffer-string) :not :to-match "Comment to post")))))

(describe "org-canvas--submissions-section-text"
  (it "keeps a paragraph break, collapses a run of blank lines, drops # lines and the ends (issue #264)"
    (with-temp-org-buffer
     (concat "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Notes\n"
             "# template\n\n\nFirst paragraph.\n  \n\n\nSecond paragraph,\nsame one.\n# a note to self\n\n\n"
             "** Comment to post\n")
     (org-back-to-heading)
     (expect (org-canvas--submissions-section-text org-canvas--submissions-notes-heading)
             :to-equal "First paragraph.\n\nSecond paragraph,\nsame one.")))
  (it "is nil when only template and blank lines are there"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Notes\n# template\n\n  \n"
     (org-back-to-heading)
     (expect (org-canvas--submissions-section-text org-canvas--submissions-notes-heading) :to-be nil))))

(describe "org-canvas--submissions-comment-draft"
  (it "is nil while only the template is there"
    (with-temp-org-buffer
     (concat "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comment to post\n"
             org-canvas-submissions-comment-template "\n\n")
     (org-back-to-heading)
     (expect (org-canvas--submissions-comment-draft) :to-be nil)))
  (it "returns the written text with the template lines dropped and its paragraph break kept"
    (with-temp-org-buffer
     (concat "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comment to post\n"
             org-canvas-submissions-comment-template "\nStrong opening.\n\nName the stakeholders next time.\n")
     (org-back-to-heading)
     (expect (org-canvas--submissions-comment-draft)
             :to-equal "Strong opening.\n\nName the stakeholders next time.")))
  (it "stops at the next heading"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comment to post\nGood.\n\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:END:\n\n** Comment to post\n# only the template\n"
     (org-back-to-heading)
     (expect (org-canvas--submissions-comment-draft) :to-equal "Good.")
     (let ((drafts (org-canvas--submissions-collect-comment-drafts)))
       (expect (length drafts) :to-equal 1)
       (expect (plist-get (car drafts) :user-id) :to-equal 5001)
       (expect (plist-get (car drafts) :text) :to-equal "Good.")))))

(describe "notes heading and carry-over"
  (it "renders the Notes heading with its template ahead of the draft heading"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry (test-org-canvas-make-submission))
      (expect (buffer-string) :to-match "^\\*\\* Notes\n# Your notes for this student")
      (expect (string-match "\\*\\* Notes" (buffer-string))
              :to-be-less-than (string-match "\\*\\* Comment to post" (buffer-string)))))
  (it "keeps notes and drafts when the file is re-rendered from Canvas"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (subs (list (test-org-canvas-make-submission)))
           (shown nil))
      (unwind-protect
          (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) (setq shown b) b))
                    ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil)))
            (org-canvas--submissions-display "HW" "1001" subs 'detail)
            (with-current-buffer shown
              (org-canvas--submissions-goto-user 5001)
              (org-canvas--submissions-set-section org-canvas--submissions-notes-heading
                                                   org-canvas-submissions-notes-template
                                                   "Struggled with the maxim.")
              (org-canvas--submissions-set-section org-canvas--submissions-draft-heading
                                                   org-canvas-submissions-comment-template
                                                   "Say the maxim first.")
              (save-buffer))
            (org-canvas--submissions-display "HW" "1001" subs 'detail)
            (with-current-buffer shown
              (org-canvas--submissions-goto-user 5001)
              (expect (org-canvas--submissions-section-text org-canvas--submissions-notes-heading)
                      :to-equal "Struggled with the maxim.")
              (expect (org-canvas--submissions-comment-draft) :to-equal "Say the maxim first.")
              (expect (count-matches "^\\*\\* Notes" (point-min) (point-max)) :to-equal 1)
              (expect (buffer-string) :to-match ":CANVAS_SCORE: 92")))
        (when (buffer-live-p shown) (kill-buffer shown))
        (delete-directory dir t)))))

(describe "org-canvas--submissions-draft-region"
  (it "returns the body region under the comment draft heading"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n"
              "** Comment to post\nGood work.\n\n* Beta, Bob\n")
      (goto-char (point-min))
      (let ((region (org-canvas--submissions-draft-region)))
        (expect region :not :to-be nil)
        (expect (buffer-substring-no-properties (car region) (cdr region))
                :to-match "Good work\\."))))

  (it "is nil for a student without a draft heading"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-draft-region) :to-be nil))))

(defun test-reports--node (type progress &optional result)
  "Return a GraphQL report node of TYPE at PROGRESS with RESULT."
  `((reportType . ,type) (processingProgress . ,progress)
    (result . ,(or result :null))))

(defun test-reports--reply (rows &optional cursor)
  "Return a reports query reply for ROWS, a list of (USER-ID . NODES).
With CURSOR the reply says another page follows after it."
  `((assignment
     . ((submissionsConnection
         . ((pageInfo . ((hasNextPage . ,(if cursor t :json-false))
                         (endCursor . ,(or cursor :null))))
            (nodes . ,(vconcat
                       (mapcar (lambda (row)
                                 `((userId . ,(car row))
                                   (ltiAssetReportsConnection
                                    . ((nodes . ,(vconcat (cdr row)))))))
                               rows)))))))))

(defun test-reports--graphql (rows)
  "Return a `org-canvas--graphql-query' stub answering ROWS in one page."
  (lambda (_document &optional _variables) (test-reports--reply rows)))

(defconst test-reports--alice
  (list "5001"
        (test-reports--node "originality" "Processed" "33%")
        (test-reports--node "turnitin_aiwriting" "Processed" "0%"))
  "Alice's two processed reports.")

(describe "org-canvas--submissions-report-property"
  (it "names Turnitin's two report types and derives a name for any other"
    (expect (org-canvas--submissions-report-property "originality") :to-equal "SIMILARITY")
    (expect (org-canvas--submissions-report-property "turnitin_aiwriting") :to-equal "AI_WRITING")
    (expect (org-canvas--submissions-report-property "code-similarity v2")
            :to-equal "REPORT_CODE_SIMILARITY_V2")
    (expect (org-canvas--submissions-report-property "_grammar_") :to-equal "REPORT_GRAMMAR"))
  (it "answers nil for a missing or blank type"
    (expect (org-canvas--submissions-report-property nil) :to-be nil)
    (expect (org-canvas--submissions-report-property :null) :to-be nil)
    (expect (org-canvas--submissions-report-property " - ") :to-be nil)))

(describe "org-canvas--submissions-report-value"
  (it "writes a processed report's result, or processed when it has none"
    (expect (org-canvas--submissions-report-value
             (test-reports--node "originality" "Processed" "33%"))
            :to-equal "33%")
    (expect (org-canvas--submissions-report-value
             (test-reports--node "originality" "Processed"))
            :to-equal "processed")
    (expect (org-canvas--submissions-report-value
             (test-reports--node "originality" "Processed" ""))
            :to-equal "processed"))
  (it "writes failed, not processed and pending for the rest"
    (expect (org-canvas--submissions-report-value
             (test-reports--node "originality" "Failed" "0%"))
            :to-equal "failed")
    (expect (org-canvas--submissions-report-value
             (test-reports--node "originality" "NotProcessed"))
            :to-equal "not processed")
    (dolist (progress '("Pending" "Processing" "PendingManual" "NotReady" "SomethingNew"))
      (expect (org-canvas--submissions-report-value
               (test-reports--node "originality" progress))
              :to-equal "pending"))))

(describe "org-canvas--submissions-report-alist"
  (it "keeps both values of a type reported twice and drops an untyped report"
    (expect (org-canvas--submissions-report-alist
             (vector (test-reports--node "originality" "Processed" "33%")
                     (test-reports--node nil "Processed" "1%")
                     (test-reports--node "turnitin_aiwriting" "Pending")
                     (test-reports--node "originality" "Failed")))
            :to-equal '(("SIMILARITY" "33%" "failed") ("AI_WRITING" "pending")))))

(describe "org-canvas--submissions-report-counts"
  (it "counts each row once, failed before pending before processed"
    (expect (org-canvas--submissions-report-counts
             '(("33%" "0%") ("failed" "0%") ("not processed") ("pending" "5%") nil))
            :to-equal '(:processed 1 :unscored 0 :failed 2 :pending 1 :none 0)))
  (it "counts a failure with its error code as failed (issue #436)"
    (expect (org-canvas--submissions-report-counts
             '(("failed (EULA_NOT_ACCEPTED)") ("not processed (X)") ("failedish")))
            :to-equal '(:processed 1 :unscored 0 :failed 2 :pending 0 :none 0)))
  (it "counts none and --% apart, after failed and pending (issue #436)"
    (expect (org-canvas--submissions-report-counts
             '(("none") ("12%" "--%") ("*%" "5%") ("--%" "pending") ("none" "failed")))
            :to-equal '(:processed 1 :unscored 1 :failed 1 :pending 1 :none 1))
    (expect (org-canvas--submissions-format-report-counts
             '(:processed 1 :unscored 1 :failed 1 :pending 1 :none 1))
            :to-equal
            "Reports: 1 processed, 1 failed, 1 pending, 1 unscored, 1 without a report"))
  (it "is nil when no row has a report"
    (expect (org-canvas--submissions-report-counts '(nil nil)) :to-be nil)
    (expect (org-canvas--submissions-format-report-counts nil) :to-be nil)))

(describe "org-canvas--submissions-fetch-reports"
  (it "follows the pages and keys each submission's reports by user id"
    (with-org-canvas-test-config
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (lambda (_document &optional variables)
                     (push variables sent)
                     (if (alist-get 'cursor variables)
                         (test-reports--reply
                          (list (list "5002" (test-reports--node "originality" "Failed"))))
                       (test-reports--reply
                        (list test-reports--alice (list "5003")) "Mg")))))
          (let ((map (org-canvas--submissions-fetch-reports "1001")))
            (expect (hash-table-count map) :to-equal 2)
            (expect (gethash "5001" map)
                    :to-equal '(("SIMILARITY" "33%") ("AI_WRITING" "0%")))
            (expect (gethash "5002" map) :to-equal '(("SIMILARITY" "failed")))
            (expect (gethash "5003" map) :to-be nil)
            (expect (length sent) :to-equal 2)
            (expect (alist-get 'cursor (car sent)) :to-equal "Mg"))))))
  (it "answers nil after one warning when the query fails"
    (with-org-canvas-test-config
      (let ((warnings nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (lambda (&rest _) (signal 'org-canvas-api-error (list "GraphQL: nope"))))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
          (expect (org-canvas--submissions-fetch-reports "1001") :to-be nil)
          (expect (length warnings) :to-equal 1)
          (expect (car warnings) :to-match "document processor reports of assignment 1001"))))))

;;;; Current attempt, error codes and missing reports (issue #436)

(defun test-reports--asset-node (type progress result asset &optional code)
  "Return a report node of TYPE at PROGRESS with RESULT on ASSET and CODE."
  (append (test-reports--node type progress result)
          `((errorCode . ,(or code :null)) (asset . ,asset))))

(defun test-reports--submission (uid attempt files reports &optional submitted)
  "Return a submission node for UID at ATTEMPT carrying FILES and REPORTS.
SUBMITTED non-nil marks it handed in; REPORTS `null' answers a null
connection."
  `((userId . ,uid) (attempt . ,attempt)
    (submittedAt . ,(if submitted "2026-09-20T12:00:00Z" :null))
    (attachments . ,(vconcat (mapcar (lambda (id) `((_id . ,id))) files)))
    (ltiAssetReportsConnection
     . ,(if (eq reports 'null) :null `((nodes . ,(vconcat reports)))))))

(defun test-reports--page (nodes)
  "Return a one-page reports query reply holding the submission NODES."
  `((assignment
     . ((submissionsConnection
         . ((pageInfo . ((hasNextPage . :json-false) (endCursor . :null)))
            (nodes . ,(vconcat nodes))))))))

(defun test-reports--graphql-with-processor (nodes processor &optional asked)
  "Return a GraphQL stub answering NODES, and PROCESSOR for the processor query.
ASKED, a cons, has its car incremented on each processor query."
  (lambda (document &optional _variables)
    (if (eq document org-canvas--submissions-processors-query)
        (progn (when asked (setcar asked (1+ (car asked))))
               `((assignment . ((ltiAssetProcessorsConnection
                                 . ((nodes . ,(if processor [((_id . "9"))] []))))))))
      (test-reports--page nodes))))

(describe "org-canvas--submissions-report-value with an error code (issue #436)"
  (it "writes the error code beside failed and not processed"
    (expect (org-canvas--submissions-report-value
             (test-reports--asset-node "originality" "Failed" nil nil "EULA_NOT_ACCEPTED"))
            :to-equal "failed (EULA_NOT_ACCEPTED)")
    (expect (org-canvas--submissions-report-value
             (test-reports--asset-node "originality" "NotProcessed" nil nil "TOO_SMALL"))
            :to-equal "not processed (TOO_SMALL)"))
  (it "keeps a comma or parenthesis out of the value and ignores a blank code"
    (expect (org-canvas--submissions-report-value
             (test-reports--asset-node "originality" "Failed" nil nil "A, (B)"))
            :to-equal "failed (A B)")
    (expect (org-canvas--submissions-report-value
             (test-reports--asset-node "originality" "Failed" nil nil " "))
            :to-equal "failed"))
  (it "writes --% and *% verbatim"
    (expect (org-canvas--submissions-report-value
             (test-reports--node "turnitin_aiwriting" "Processed" "--%"))
            :to-equal "--%")
    (expect (org-canvas--submissions-report-value
             (test-reports--node "turnitin_aiwriting" "Processed" "*%"))
            :to-equal "*%")))

(describe "org-canvas--submissions-node-reports (issue #436)"
  (it "keeps the reports on the files the submission carries now"
    (let* ((node (test-reports--submission
                  "5001" 2 '("11")
                  (list (test-reports--asset-node "originality" "Processed" "31%"
                                                  '((attachmentId . "10")
                                                    (submissionAttempt . :null)))
                        (test-reports--asset-node "originality" "Processed" "12%"
                                                  '((attachmentId . "11")
                                                    (submissionAttempt . :null))))
                  t))
           (split (org-canvas--submissions-node-reports node)))
      (expect (mapcar (lambda (r) (alist-get 'result r)) (car split)) :to-equal '("12%"))
      (expect (mapcar (lambda (r) (alist-get 'result r)) (cdr split)) :to-equal '("31%"))))
  (it "goes by the asset's attempt when it names one"
    (let ((node (test-reports--submission
                 "5001" 2 nil
                 (list (test-reports--asset-node "originality" "Processed" "1%"
                                                 '((attachmentId . "10")
                                                   (submissionAttempt . 2)))
                       (test-reports--asset-node "originality" "Processed" "2%"
                                                 '((submissionAttempt . 1)))
                       (test-reports--asset-node "originality" "Processed" "3%"
                                                 '((submissionAttempt . :null))))
                 t)))
      (expect (mapcar (lambda (r) (alist-get 'result r))
                      (car (org-canvas--submissions-node-reports node)))
              :to-equal '("1%" "3%"))))
  (it "keeps a discussion entry's report, which has no file"
    (let ((node (test-reports--submission
                 "5001" 3 nil
                 (list (test-reports--asset-node "originality" "Processed" "4%"
                                                 '((attachmentId . :null)
                                                   (submissionAttempt . 1)
                                                   (discussionEntryVersion . ((_id . "77"))))))
                 t)))
      (expect (length (car (org-canvas--submissions-node-reports node))) :to-equal 1)))
  (it "answers nil for a null connection"
    (expect (org-canvas--submissions-node-reports
             (test-reports--submission "5001" 1 nil 'null t))
            :to-be nil)))

(describe "org-canvas--submissions-fetch-reports and missing reports (issue #436)"
  (it "writes none on a handed-in row without a report when the column has a processor"
    (with-org-canvas-test-config
      (let ((asked (list 0)))
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (test-reports--graphql-with-processor
                    (list (test-reports--submission "5001" 1 '("11") nil t)
                          (test-reports--submission "5002" 1 nil nil t)
                          (test-reports--submission "5003" 1 nil nil nil)
                          (test-reports--submission "5004" 1 nil 'null t))
                    t asked)))
          (let ((map (org-canvas--submissions-fetch-reports "1001")))
            (expect (gethash "5001" map) :to-equal '(("SIMILARITY" "none")))
            (expect (gethash "5002" map) :to-equal '(("SIMILARITY" "none")))
            (expect (gethash "5003" map) :to-be nil)
            (expect (gethash "5004" map) :to-be nil)
            (expect (car asked) :to-equal 1))))))
  (it "stays silent on a column without a processor"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--graphql-query)
                 (test-reports--graphql-with-processor
                  (list (test-reports--submission "5001" 1 '("11") nil t)) nil)))
        (expect (hash-table-count (org-canvas--submissions-fetch-reports "1001"))
                :to-equal 0))))
  (it "does not ask about the processor when told, nor when every row has a report"
    (with-org-canvas-test-config
      (let ((asked (list 0)))
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (test-reports--graphql-with-processor
                    (list (test-reports--submission "5001" 1 nil nil t)) nil asked)))
          (expect (gethash "5001" (org-canvas--submissions-fetch-reports "1001" t))
                  :to-equal '(("SIMILARITY" "none"))))
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (test-reports--graphql-with-processor
                    (list (test-reports--submission
                           "5001" 1 nil
                           (list (test-reports--node "originality" "Processed" "5%"))
                           t))
                    t asked)))
          (org-canvas--submissions-fetch-reports "1001"))
        (expect (car asked) :to-equal 0))))
  (it "leaves the rows blank after one warning when the processor read fails"
    (with-org-canvas-test-config
      (let ((warnings nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (lambda (document &optional _variables)
                     (if (eq document org-canvas--submissions-processors-query)
                         (signal 'org-canvas-api-error (list "GraphQL: nope"))
                       (test-reports--page
                        (list (test-reports--submission "5001" 1 nil nil t))))))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
          (expect (hash-table-count (org-canvas--submissions-fetch-reports "1001"))
                  :to-equal 0)
          (expect (length warnings) :to-equal 1)
          (expect (car warnings) :to-match "rows without a report left blank"))))))

(describe "a grading file names refusals, replaced attempts and missing reports (issue #436)"
  (it "writes the current attempt's values, the error code and none"
    (with-org-canvas-test-config
      (with-grading-file test-grading-file-header
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                   (lambda (_id) (list (test-org-canvas-make-submission)
                                       (test-refresh--bob))))
                  ((symbol-function 'org-canvas--submissions-fetch-assignment)
                   (lambda (_id) '((id . 1001))))
                  ((symbol-function 'org-canvas--submissions-heading-for-assignment)
                   (lambda (_id) nil))
                  ((symbol-function 'org-canvas--graphql-query)
                   (test-reports--graphql-with-processor
                    (list (test-reports--submission
                           "5001" 2 '("11")
                           (list (test-reports--asset-node
                                  "originality" "Processed" "31%" '((attachmentId . "10")))
                                 (test-reports--asset-node
                                  "originality" "Failed" nil '((attachmentId . "11"))
                                  "EULA_NOT_ACCEPTED"))
                           t)
                          (test-reports--submission "5002" 1 '("12") nil t))
                    t))
                  ((symbol-function 'switch-to-buffer) (lambda (b) b))
                  ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask")))
                  ((symbol-function 'message) #'ignore))
          (setq-local org-canvas-submissions--current-view 'detail)
          (org-canvas-submissions-refresh))
        (expect (buffer-string)
                :to-match "^Reports: 0 processed, 1 failed, 0 pending, 1 without a report$")
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SIMILARITY") :to-equal "failed (EULA_NOT_ACCEPTED)")
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "SIMILARITY") :to-equal "none")))))

(describe "org-canvas--submissions-with-reports"
  (it "does not ask when nothing was handed in"
    (let ((asked nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-query)
                 (lambda (&rest _) (setq asked t) nil)))
        (let ((subs (list (test-org-canvas-make-submission '((submitted_at . nil))))))
          (expect (org-canvas--submissions-with-reports "1001" subs) :to-be subs)
          (expect asked :to-be nil)))))
  (it "returns the rows untouched when the column has no report"
    (cl-letf (((symbol-function 'org-canvas--graphql-query) (test-reports--graphql nil)))
      (let ((subs (list (test-org-canvas-make-submission))))
        (expect (org-canvas--submissions-with-reports "1001" subs) :to-be subs))))
  (it "attaches the reports to the row whose user they are and logs the count"
    (let ((logged nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-query)
                 (test-reports--graphql (list test-reports--alice)))
                ((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args) (push (apply #'format fmt args) logged))))
        (let ((subs (org-canvas--submissions-with-reports
                     "1001" (list (test-org-canvas-make-submission) (test-refresh--bob)))))
          (expect (alist-get 'org-canvas-reports (car subs))
                  :to-equal '(("SIMILARITY" "33%") ("AI_WRITING" "0%")))
          (expect (assq 'org-canvas-reports (cadr subs)) :to-be nil)
          (expect logged :to-equal
                  '("[Submissions] Reports: 1 processed, 0 failed, 0 pending")))))))

(defun test-reports--run (rows subs)
  "Refresh the current grading file as if Canvas held SUBS and report ROWS.
ROWS is what the reports query answers; nil answers no report."
  (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment) (lambda (_id) subs))
            ((symbol-function 'org-canvas--submissions-fetch-assignment) (lambda (_id) '((id . 1001))))
            ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
            ((symbol-function 'org-canvas--graphql-query) (test-reports--graphql rows))
            ((symbol-function 'switch-to-buffer) (lambda (b) b))
            ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask")))
            ((symbol-function 'message) #'ignore))
    ;; A file with no student heading yet reads as a summary; this is
    ;; the grading file, whatever it holds.
    (setq-local org-canvas-submissions--current-view 'detail)
    (org-canvas-submissions-refresh)))

(describe "a grading file carries the document processor reports (issue #351)"
  (it "writes SIMILARITY and AI_WRITING on the row and the counts in the header"
    (with-org-canvas-test-config
      (with-grading-file test-grading-file-header
        (test-reports--run
         (list test-reports--alice
               (list "5002" (test-reports--node "originality" "Failed")))
         (list (test-org-canvas-make-submission) (test-refresh--bob)))
        (expect (buffer-string)
                :to-match "^Reports: 1 processed, 1 failed, 0 pending$")
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SIMILARITY") :to-equal "33%")
        (expect (org-entry-get (point) "AI_WRITING") :to-equal "0%")
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "SIMILARITY") :to-equal "failed")
        (expect (org-entry-get (point) "AI_WRITING") :to-be nil))))
  (it "writes nothing when the column has no report"
    (with-org-canvas-test-config
      (with-grading-file test-grading-file-header
        (test-reports--run nil (list (test-org-canvas-make-submission)))
        (expect (buffer-string) :not :to-match "^Reports:")
        (expect (buffer-string) :not :to-match "SIMILARITY\\|AI_WRITING"))))
  (it "joins two reports of one type and names an unknown type"
    (with-org-canvas-test-config
      (with-grading-file test-grading-file-header
        (test-reports--run
         (list (list "5001"
                     (test-reports--node "originality" "Processed" "33%")
                     (test-reports--node "originality" "Processing")
                     (test-reports--node "grammar_check" "Processed" "12 issues")))
         (list (test-org-canvas-make-submission)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SIMILARITY") :to-equal "33%, pending")
        (expect (org-entry-get (point) "REPORT_GRAMMAR_CHECK") :to-equal "12 issues")
        (expect (buffer-string) :to-match "^Reports: 0 processed, 0 failed, 1 pending$"))))
  (it "refreshes the reports and keeps the typed score, notes and drafted comment (#281)"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "Reports: 0 processed, 0 failed, 1 pending\n\n"
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n"
                                 ":SCORE: 95\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n"
                                 ":SIMILARITY: pending\n:REPORT_OLD: 5%\n:END:\n"
                                 "\n** Notes\nCheck the sources.\n\n** Comment to post\nSee me.\n")
        (test-reports--run (list test-reports--alice)
                           (list (test-org-canvas-make-submission '((attempt . 1)))))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SIMILARITY") :to-equal "33%")
        (expect (org-entry-get (point) "AI_WRITING") :to-equal "0%")
        (expect (org-entry-get (point) "REPORT_OLD") :to-be nil)
        (expect (org-entry-get (point) "SCORE") :to-equal "95")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92")
        (expect (buffer-string) :to-match "Check the sources\\.")
        (expect (buffer-string) :to-match "never sent\\.  c posts a one-off comment now\\.\nSee me\\.")
        (expect (buffer-string) :to-match "^Reports: 1 processed, 0 failed, 0 pending$")
        (expect (buffer-string) :not :to-match "1 pending"))))
  (it "pulls without the reports when the query fails, after one warning"
    (with-org-canvas-test-config
      (with-submissions-dir
        (let ((warnings nil))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (test-entry--canvas '((id . 1001) (name . "Homework 1"))))
                    ((symbol-function 'org-canvas--graphql-query)
                     (lambda (&rest _) (signal 'org-canvas-api-error (list "GraphQL: timeout"))))
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
            (with-current-buffer (org-canvas-pull-submissions "1001")
              (expect (buffer-string) :to-match "^\\* Adams, Alice")
              (expect (buffer-string) :not :to-match "SIMILARITY\\|^Reports:")))
          (expect (length warnings) :to-equal 1)
          (expect (car warnings) :to-match "pulled without them")))))
  (it "puts the counts under the summary table's statistics"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-summary
       "HW" "1001"
       (list (cons '(org-canvas-reports ("SIMILARITY" "failed"))
                   (test-org-canvas-make-submission))))
      (expect (buffer-string) :to-match "submitted.*\nReports: 0 processed, 1 failed, 0 pending\n")))
  (it "adds the headings' reports up in the summary of a grading file"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n"
                                 ":SIMILARITY: 33%, failed\n:AI_WRITING: 0%\n:END:\n"
                                 "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n"
                                 ":REPORT_GRAMMAR: pending\n:END:\n"
                                 "* Gamma, Gil\n:PROPERTIES:\n:USER_ID: 5003\n:END:\n")
        (org-canvas--submissions-ensure-context)
        (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) b)))
          (with-current-buffer (org-canvas--submissions-show-summary-of-file)
            (unwind-protect
                (expect (buffer-string)
                        :to-match "^Reports: 0 processed, 1 failed, 1 pending$")
              (kill-buffer)))))))
  (it "leaves the summary of a grading file without reports alone"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
        (org-canvas--submissions-ensure-context)
        (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) b)))
          (with-current-buffer (org-canvas--submissions-show-summary-of-file)
            (unwind-protect
                (expect (buffer-string) :not :to-match "^Reports:")
              (kill-buffer))))))))

;;;; Late status (issue #352)

(defun test-history--node (attempt &rest fields)
  "Return a submission history node for ATTEMPT with FIELDS, an alist."
  (append `((attempt . ,attempt)) (car fields)))

(defconst test-history--bob
  (list (test-history--node 1 '((submittedAt . "2026-02-15T23:45:00Z") (gradedAt . "2026-02-17T10:00:00Z")
                                (enteredScore . 3.0) (gradeMatchesCurrentSubmission . t)
                                (wordCount . 412.0) (attachments . [((displayName . "essay.pdf"))])))
        (test-history--node 2 '((submittedAt . :null) (gradedAt . "2026-02-17T10:00:00Z")
                                (enteredScore . 3.0) (gradeMatchesCurrentSubmission . :json-false)
                                (wordCount . :null)
                                (attachments . [((displayName . "essay-v2.pdf")) ((displayName . "notes.txt"))]))))
  "Bob's attempts: the first graded 3, the second carrying the copied score.")

(defun test-history--graded-bob-file ()
  "Return a grading file where Bob's first attempt is graded 3."
  (concat test-grading-file-header
          (test-refresh--student "Beta, Bob" 5002
                                 ":STATUS: graded\n:SCORE: 3\n:CANVAS_SCORE: 3\n:ATTEMPT: 1\n:SUBMITTED_AT: <2026-02-15 Sun 23:45>\n")))

(describe "org-canvas--submissions-graded-attempt"
  (it "is the attempt the grade was given to, not the one Canvas copied the score onto"
    (expect (alist-get 'attempt (org-canvas--submissions-graded-attempt test-history--bob))
            :to-equal 1))
  (it "is nil when no attempt was graded as it stands"
    (expect (org-canvas--submissions-graded-attempt
             (list (test-history--node 1 '((enteredScore . :null) (gradeMatchesCurrentSubmission . t)))
                   (test-history--node 2 '((enteredScore . 3.0) (gradeMatchesCurrentSubmission . :json-false)))))
            :to-be nil)))

(describe "org-canvas--submissions-describe-attempts"
  (it "sets the latest attempt beside the graded one, with what each held"
    (let ((found (org-canvas--submissions-describe-attempts test-history--bob)))
      (expect (car found) :to-equal 1)
      (expect (cdr found)
              :to-match "\\`attempt 2 (essay-v2\\.pdf, notes\\.txt) after the score 3 given on attempt 1 (submitted <2026-02-1[56] [A-Z][a-z][a-z][^>]*>, essay\\.pdf, 412 words)\\'")))
  (it "says so when no attempt carries the score, and is nil without attempts"
    (expect (org-canvas--submissions-describe-attempts
             (list (test-history--node 1 '((gradeMatchesCurrentSubmission . :json-false)))))
            :to-equal '(nil . "attempt 1; no attempt carries the score"))
    (expect (org-canvas--submissions-describe-attempts nil) :to-be nil)))

(describe "org-canvas--submissions-fetch-history"
  (it "reads the column's one submission for the student, attempts in order"
    (let ((sent nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-query)
                 (lambda (document variables)
                   (setq sent (cons document variables))
                   `((submission . ((submissionHistoriesConnection
                                     . ((nodes . ,(vector (nth 1 test-history--bob) '((attempt . :null))
                                                          (nth 0 test-history--bob)))))))))))
        (expect (mapcar (lambda (n) (alist-get 'attempt n))
                        (org-canvas--submissions-fetch-history 1001 5002))
                :to-equal '(1 2))
        (expect (cdr sent) :to-equal '((assignmentId . "1001") (userId . "5002"))))))
  (it "answers nil for a submission Canvas does not have"
    (cl-letf (((symbol-function 'org-canvas--graphql-query)
               (lambda (&rest _) '((submission . :null)))))
      (expect (org-canvas--submissions-fetch-history 1001 5002) :to-be nil))))

(describe "a refresh names the attempt a resubmitted row was graded on"
  (it "logs both attempts and writes GRADED_ATTEMPT beside the CONFLICT"
    (with-org-canvas-test-config
      (with-grading-file (test-history--graded-bob-file)
        (let ((asked nil))
          (cl-letf (((symbol-function 'org-canvas--graphql-query)
                     (lambda (document &optional variables)
                       (when (eq document org-canvas--submissions-history-query)
                         (push (alist-get 'userId variables) asked)
                         `((submission . ((submissionHistoriesConnection
                                           . ((nodes . ,(vconcat test-history--bob)))))))))))
            (test-refresh--run (list (test-refresh--bob '((score . 3) (attempt . 2))))))
          (expect asked :to-equal '("5002")))
        (expect (test-refresh--summary)
                :to-equal (concat "Refreshed HW: 1 resubmitted after grading"
                                  "; CONFLICT on 1: Beta, Bob (attempt: 2 submitted after grading)"))
        (expect (cl-find-if (lambda (l) (string-prefix-p "[Refresh] Beta, Bob: attempt 2 (" l))
                            test-refresh--log)
                :to-match "after the score 3 given on attempt 1 ")
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "attempt: 2 submitted after grading")
        (expect (org-entry-get (point) "GRADED_ATTEMPT") :to-equal "1"))))
  (it "asks nothing when no graded row was resubmitted"
    (with-org-canvas-test-config
      (with-grading-file (test-history--graded-bob-file)
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (lambda (document &rest _)
                     (when (eq document org-canvas--submissions-history-query)
                       (error "must not ask")))))
          (test-refresh--run (list (test-refresh--bob '((score . 3) (attempt . 1))))))
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "GRADED_ATTEMPT") :to-be nil))))
  (it "reports the resubmission without its history when the read fails, warning once"
    (with-org-canvas-test-config
      (with-grading-file (test-history--graded-bob-file)
        (let ((warnings nil))
          (cl-letf (((symbol-function 'org-canvas--graphql-query)
                     (lambda (document &rest _)
                       (when (eq document org-canvas--submissions-history-query)
                         (error "HTTP 500"))))
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
            (test-refresh--run (list (test-refresh--bob '((score . 3) (attempt . 2))))))
          (expect warnings
                  :to-equal '("[Refresh] Could not read the attempt history of assignment 1001 (HTTP 500); resubmissions reported without it")))
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "attempt: 2 submitted after grading")
        (expect (org-entry-get (point) "GRADED_ATTEMPT") :to-be nil)))))

;;;; Comment bank (issue #352)

(provide 'org-canvas-submissions-render-test)
;;; org-canvas-submissions-render-test.el ends here
