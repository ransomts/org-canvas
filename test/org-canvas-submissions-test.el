;;; org-canvas-submissions-test.el --- Tests for org-canvas-submissions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for the submissions viewer module.

;;; Code:

(require 'buttercup)
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

(defconst test-rubric-criteria
  '(((id . "_7104") (description . "Thesis") (points . 2.0)
     (ratings . [((id . "r1") (description . "Excellent") (points . 2.0))
                 ((id . "r2") (description . "Weak") (points . 1.0))]))
    ((id . "_7105") (description . "Evidence | support") (points . 3))
    ((id . "_7106") (description . "Style") (points . 1)))
  "Three rubric criteria as the assignment object lists them.")

(describe "org-canvas--submissions-render-rubric"
  (it "renders one row and one comment item per criterion, pre-filled from the assessment"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-rubric
       test-rubric-criteria
       '((_7104 . ((points . 2.0) (rating_id . "r1") (comments . "Sharp")))
         (_7105 . ((points . 1.5) (comments . :null)))))
      (let ((content (buffer-string)))
        (expect content :to-match "^\\*\\* Rubric$")
        (expect content :to-match "| Id *| Criterion *| Max *| Score *|$")
        (expect content :to-match "| _7104 *| Thesis *| *2 *| *2 *|$")
        (expect content :to-match "| _7105 *| Evidence support *| *3 *| *1.5 *|$")
        (expect content :to-match "| _7106 *| Style *| *1 *| *|$")
        (expect content :not :to-match "Comment")
        ;; The items follow the table directly, one per row, in its order.
        (expect content :to-match "| *|\n- _7104 :: Sharp\n- _7105 ::\n- _7106 ::\n"))))
  (it "keeps a comment's line breaks as the item's continuation lines (issue #263)"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n")
      (org-canvas--submissions-render-rubric
       test-rubric-criteria
       '((_7104 . ((points . 2) (comments . "State the maxim first.\r\n\r\n\r\nWhere to look: deck 02, 'Universalizability'; Quinn 2.6.2.  ")))))
      (expect (buffer-string)
              :to-match "- _7104 :: State the maxim first\\.\n\n  Where to look: deck 02, 'Universalizability'; Quinn 2\\.6\\.2\\.\n- _7105 ::\n")
      (goto-char (point-min))
      (expect (nth 4 (car (org-canvas--submissions-rubric-rows)))
              :to-equal "State the maxim first.\n\nWhere to look: deck 02, 'Universalizability'; Quinn 2.6.2.")))
  (it "renders empty cells and items without an assessment"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-rubric test-rubric-criteria nil)
      (expect (buffer-string) :to-match "| _7104 *| Thesis *| *2 *| *|$")
      (expect (buffer-string) :to-match "^- _7104 ::$")))
  (it "renders the assessment alone when the criteria are unknown"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-rubric
       nil '((crit_1 . ((points . 18) (rating_description . "Excellent") (comments . "Fine")))))
      (expect (buffer-string) :to-match "| crit_1 *| *| *| *18 *|$")
      (expect (buffer-string) :to-match "^- crit_1 :: Fine$")))
  (it "inserts nothing without a rubric"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric nil nil)
      (expect (buffer-string) :to-equal ""))))

(describe "org-canvas--submissions-comment-text"
  (it "normalizes line ends, surrounding whitespace and blank runs, and is nil when blank"
    (expect (org-canvas--submissions-comment-text nil) :to-be nil)
    (expect (org-canvas--submissions-comment-text :null) :to-be nil)
    (expect (org-canvas--submissions-comment-text "  \n \t\n") :to-be nil)
    (expect (org-canvas--submissions-comment-text " Sharp ") :to-equal "Sharp")
    (expect (org-canvas--submissions-comment-text "a | b") :to-equal "a | b")
    (expect (org-canvas--submissions-comment-text "\n\na\r\n  b  \n\n\n\nc\n\n")
            :to-equal "a\nb\n\nc"))
  (it "agrees between a comment as Canvas returns it and as its item reads back"
    (let ((canvas "First point.\r\n\r\nSecond point, indented on Canvas.\r\n  - a sub-point"))
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n")
        (org-canvas--submissions-render-rubric
         test-rubric-criteria `((_7104 . ((points . 2) (comments . ,canvas)))))
        (goto-char (point-min))
        (expect (nth 4 (car (org-canvas--submissions-rubric-rows)))
                :to-equal (org-canvas--submissions-comment-text canvas))))))

(describe "org-canvas--submissions-rubric-digest"
  (it "is nil for an empty table and stable across row order"
    (expect (org-canvas--submissions-rubric-digest nil) :to-be nil)
    (expect (org-canvas--submissions-rubric-digest '(("_7104" nil nil) ("_7105" nil nil)))
            :to-be nil)
    (expect (org-canvas--submissions-rubric-digest '(("_7104" "2" "Sharp") ("_7105" "1.5" nil)))
            :to-equal (org-canvas--submissions-rubric-digest
                       '(("_7105" "1.5" nil) ("_7104" "2" "Sharp"))))
    (expect (org-canvas--submissions-rubric-digest '(("_7104" "2" "Sharp")))
            :not :to-equal (org-canvas--submissions-rubric-digest '(("_7104" "2" "Sharper")))))
  (it "agrees between Canvas's assessment and the section as typed, line breaks included"
    (let ((sub (test-org-canvas-make-submission
                '((rubric_assessment . ((_7104 . ((points . 2.0) (comments . "Sharp.\n\nWhere to look: deck 02.")))
                                        (_7105 . ((points . 1.5)))))))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry sub "HW" "1001" test-rubric-criteria)
        (goto-char (point-min))
        (expect (org-entry-get (point) "CANVAS_RUBRIC")
                :to-equal (org-canvas--submissions-submission-rubric-digest sub))
        (expect (org-canvas--submissions-rubric-carryover) :to-be nil))))
  (it "is absent from a heading whose student has no assessment"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission) "HW" "1001" test-rubric-criteria)
      (expect (buffer-string) :not :to-match "CANVAS_RUBRIC")
      (expect (buffer-string) :to-match "^\\*\\* Rubric$"))))

(describe "org-canvas--submissions-rubric-rows"
  (it "reads the table rows and their comment items, dropping the header and idless rows"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric\n"
              "| Id | Criterion | Max | Score |\n|---+---+---+---|\n"
              "| _7104 | Thesis | 2 | 2 |\n"
              "| _7105 | Evidence | 3 |  |\n"
              "|  | stray | | 1 |\n"
              "| _7106 | Style | 1 | 0.5\n"
              "- _7104 :: Sharp\n"
              "- _7105 ::\n"
              "- _7106 :: Thin, and the second sentence\n  runs on to a second line.\n\n"
              "  A second paragraph.  \n\n\n"
              "- _9999 :: no such row\n"
              "\n** Notes\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-rubric-rows)
              :to-equal '(("_7104" "Thesis" "2" "2" "Sharp")
                          ("_7105" "Evidence" "3" nil nil)
                          ("_7106" "Style" "1" "0.5"
                           "Thin, and the second sentence\nruns on to a second line.\n\nA second paragraph.")))))
  (it "reads an item's text from the line after the id, and none from an item left empty"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric\n"
              "| Id | Criterion | Max | Score |\n|---+---+---+---|\n"
              "| _7104 | Thesis | 2 | 2 |\n| _7105 | Evidence | 3 | 1 |\n"
              "- _7104 :: \n  Typed under the id.\n"
              "- _7105 ::   \n\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-rubric-rows)
              :to-equal '(("_7104" "Thesis" "2" "2" "Typed under the id.")
                          ("_7105" "Evidence" "3" "1" nil)))))
  (it "takes a fifth cell as the comment of a row without an item, the shape before issue #263"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric\n"
              "| Id | Criterion | Max | Score | Comment |\n|---+---+---+---+---|\n"
              "| _7104 | Thesis | 2 | 2 | Sharp |\n"
              "| _7105 | Evidence | 3 |  |  |\n"
              "| _7106 | Style | 1 | 0.5 | In the cell |\n"
              "- _7106 :: In the item\n"
              "\n** Notes\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-rubric-rows)
              :to-equal '(("_7104" "Thesis" "2" "2" "Sharp")
                          ("_7105" "Evidence" "3" nil nil)
                          ("_7106" "Style" "1" "0.5" "In the item")))))
  (it "is nil without a Rubric heading, the old read-only one included"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric Assessment\n| crit_1 | Excellent | 18 |\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-rubric-rows) :to-be nil))))

(describe "org-canvas--submissions-rubric-set-row"
  (it "writes the score into the row and the comment into its item, keeping the rest"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric\n"
              "| Id | Criterion | Max | Score |\n|---+---+---+---|\n"
              "| _7104 | Thesis | 2 |  |\n| _7105 | Evidence | 3 |  |\n"
              "- _7104 :: Old\n  comment\n- _7105 ::\n\n** Notes\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-rubric-set-row "_7105" "1.5" "Thin.\nWhere to look: deck 02.")
              :to-be-truthy)
      (expect (org-canvas--submissions-rubric-set-row "_7104" "2" nil) :to-be-truthy)
      (expect (org-canvas--submissions-rubric-set-row "_9999" "1" "stray") :to-be nil)
      (expect (org-canvas--submissions-rubric-rows)
              :to-equal '(("_7104" "Thesis" "2" "2" nil)
                          ("_7105" "Evidence" "3" "1.5" "Thin.\nWhere to look: deck 02.")))
      (expect (buffer-string)
              :to-match "|\n- _7104 ::\n- _7105 :: Thin\\.\n  Where to look: deck 02\\.\n\n\\*\\* Notes\n")
      (expect (buffer-string) :not :to-match "Old\\|stray")))
  (it "adds the item of a row that lacks one, after the last item or after the table"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric\n"
              "| Id | Criterion | Max | Score |\n|---+---+---+---|\n"
              "| _7104 | Thesis | 2 |  |\n| _7105 | Evidence | 3 |  |\n\n** Notes\n")
      (goto-char (point-min))
      ;; Nil adds nothing: a missing item already reads as no comment.
      (org-canvas--submissions-rubric-set-row "_7104" "2" nil)
      (expect (buffer-string) :not :to-match "::")
      (org-canvas--submissions-rubric-set-row "_7105" "3" "Second")
      (org-canvas--submissions-rubric-set-row "_7104" "2" "First")
      (expect (buffer-string)
              :to-match "|\n- _7105 :: Second\n- _7104 :: First\n\n\\*\\* Notes\n")
      (expect (org-canvas--submissions-rubric-rows)
              :to-equal '(("_7104" "Thesis" "2" "2" "First")
                          ("_7105" "Evidence" "3" "3" "Second")))))
  (it "moves a comment out of a fifth cell into an item, the shape before issue #263"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric\n"
              "| Id | Criterion | Max | Score | Comment |\n|---+---+---+---+---|\n"
              "| _7104 | Thesis | 2 |  | Sharp |\n| _7105 | Evidence | 3 |  |  |\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-rubric-set-row "_7104" "2" "Sharper") :to-be-truthy)
      (expect (buffer-string) :to-match "| _7104 *| Thesis *| *2 *| *2 *| *|\n")
      (expect (buffer-string) :to-match "^- _7104 :: Sharper$")
      (expect (org-canvas--submissions-rubric-rows)
              :to-equal '(("_7104" "Thesis" "2" "2" "Sharper")
                          ("_7105" "Evidence" "3" nil nil)))))
  (it "writes the last entry of the file, whose section has no heading after it"
    (with-temp-buffer
      (org-mode)
      (insert "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Rubric\n"
              "| Id | Criterion | Max | Score |\n|---+---+---+---|\n"
              "| _7104 | Thesis | 2 | 2 |\n- _7104 :: Sharp\n  more")
      (goto-char (point-min))
      (org-canvas--submissions-rubric-set-row "_7104" "1" "Less")
      (expect (buffer-string) :to-match "| *1 *|\n- _7104 :: Less\\'")
      (expect (org-canvas--submissions-rubric-rows)
              :to-equal '(("_7104" "Thesis" "2" "1" "Less"))))))

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
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--submissions-post-comment "1001" "5001" "Great job!")
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
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

;;;; Fetch Parameters

(describe "org-canvas--submissions-fetch-for-assignment"
  (it "sends each include as its own include[] key, never comma-joined (#112)"
    (with-org-canvas-test-config
      (let ((captured nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional params)
                     (setq captured params)
                     nil)))
          (org-canvas--submissions-fetch-for-assignment "1001")
          (let ((includes (mapcar #'cdr
                                  (cl-remove-if-not
                                   (lambda (p) (equal (car p) "include[]"))
                                   captured))))
            (expect includes :to-have-same-items-as
                    '("submission_comments" "rubric_assessment" "user"))
            (expect (cl-some (lambda (v) (string-match-p "," v)) includes)
                    :to-be nil)))))))

;;;; Entry Point (org-canvas-pull-submissions)

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
              :to-equal #'org-canvas-submissions-add-comment)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "D"))
              :to-equal #'org-canvas-submissions-download-all-attachments))))

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
        (org-canvas--submissions-push-single-grade
         "1001" (list :user-id 5001 :name "A" :old-score "90" :new-score "95"))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
        (let* ((call (test-org-canvas-last-api-call))
               (data (nth 2 call)))
          (expect (alist-get 'posted_grade (alist-get 'submission data))
                  :to-equal "95")
          (expect (assq 'rubric_assessment data) :to-be nil)))))
  (it "sends the rubric assessment beside the grade, and alone when only it moved"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--submissions-push-single-grade
         "1001" (list :user-id 5001 :name "A" :old-score "90" :new-score "4"
                      :score-derived t
                      :triples '(("_7104" "2" "Sharp") ("_7105" "2" nil) ("_7106" nil nil))))
        (let ((data (nth 2 (test-org-canvas-last-api-call))))
          (expect (alist-get 'posted_grade (alist-get 'submission data)) :to-equal "4")
          (expect (alist-get 'rubric_assessment data)
                  :to-equal '((_7104 . ((points . 2) (comments . "Sharp")))
                              (_7105 . ((points . 2))))))
        (org-canvas--submissions-push-single-grade
         "1001" (list :user-id 5001 :name "A" :old-score "90" :new-score "90"
                      :triples '(("_7104" nil "Late but fine"))))
        (let ((data (nth 2 (test-org-canvas-last-api-call))))
          (expect (assq 'submission data) :to-be nil)
          (expect (alist-get 'rubric_assessment data)
                  :to-equal '((_7104 . ((comments . "Late but fine"))))))))))

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
          (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil)))
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

;;;; Grading Files (file-backed round trip)

(defmacro with-grading-file (content &rest body)
  "Visit a scratch grading file holding CONTENT, in submissions mode, run BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-canvas-subs-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (setq buf (find-file-noselect file))
           (with-current-buffer buf
             (org-mode)
             (org-canvas-submissions-mode 1)
             ;; A refresh reads the column's reports over GraphQL
             ;; (#351); here the column has none.
             (cl-letf (((symbol-function 'org-canvas--graphql-query) #'ignore))
               ,@body)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

(defconst test-grading-file-header
  "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n\n")

(describe "org-canvas--submissions-entered-score"
  (it "prefers entered_score over the late-adjusted score"
    (expect (org-canvas--submissions-entered-score
             '((entered_score . 5.0) (score . 4.0)))
            :to-equal 5.0))
  (it "falls back to score"
    (expect (org-canvas--submissions-entered-score '((score . 92))) :to-equal 92))
  (it "is nil when ungraded"
    (expect (org-canvas--submissions-entered-score '((score . nil))) :to-be nil)))

(describe "org-canvas--submissions-dir"
  (it "resolves submissions/ under org-canvas-directory when unset"
    (let ((org-canvas-submissions-directory nil)
          (org-canvas-directory "/tmp/course/"))
      (expect (org-canvas--submissions-dir) :to-match "/tmp/course/submissions/?$")))
  (it "honors an explicit directory"
    (let ((org-canvas-submissions-directory "/tmp/elsewhere/"))
      (expect (org-canvas--submissions-dir) :to-equal "/tmp/elsewhere/"))))

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

(describe "org-canvas--submissions-file-property"
  (it "reads a #+PROPERTY keyword"
    (with-temp-buffer
      (insert test-grading-file-header "* Adams, Alice\n")
      (expect (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID") :to-equal "1001")
      (expect (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_NAME") :to-equal "HW")))
  (it "is nil when absent"
    (with-temp-buffer
      (insert "* Adams, Alice\n")
      (expect (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID") :to-be nil))))

(describe "org-canvas--submissions-ensure-context"
  (it "recovers id, name, and view from a reopened grading file"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n")
      (org-canvas--submissions-ensure-context)
      (expect org-canvas-submissions--assignment-id :to-equal "1001")
      (expect org-canvas-submissions--assignment-name :to-equal "HW")
      (expect org-canvas-submissions--current-view :to-equal 'detail)))
  (it "leaves values a pull already set alone"
    (with-temp-buffer
      (org-mode)
      (setq-local org-canvas-submissions--assignment-id "7")
      (setq-local org-canvas-submissions--current-view 'summary)
      (org-canvas--submissions-ensure-context)
      (expect org-canvas-submissions--assignment-id :to-equal "7")
      (expect org-canvas-submissions--current-view :to-equal 'summary))))

(describe "CANVAS_SCORE baseline in change detection"
  (it "compares SCORE against CANVAS_SCORE, ignoring a stale snapshot"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "95")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :old-score) :to-equal "92")
       (expect (plist-get (car changes) :new-score) :to-equal "95")
       (expect (plist-get (car changes) :attempt) :to-equal 1))))
  (it "sees no change when SCORE equals CANVAS_SCORE"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores nil)
     (expect (org-canvas--submissions-collect-detail-changes) :to-equal nil)))
  (it "treats a first grade on an ungraded heading as a change"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 5\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores nil)
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (plist-get (car changes) :old-score) :to-be nil)
       (expect (plist-get (car changes) :new-score) :to-equal "5")))))

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
        (delete-directory dir t)))))

(describe "pushing from a reopened grading file"
  (it "recovers the assignment id, pushes, updates CANVAS_SCORE, and saves"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
          (let ((org-canvas-submissions-check-conflicts nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (org-canvas-submissions-push-grades)))
          (expect-api-called 'PUT "assignments/1001/submissions/5001")
          (expect (buffer-string) :to-match ":CANVAS_SCORE: 95")
          (expect (buffer-modified-p) :to-be nil)
          (expect (with-temp-buffer (insert-file-contents file) (buffer-string))
                  :to-match ":CANVAS_SCORE: 95")))))
  (it "pushes nothing when the file needs no push"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades))
          (expect (test-org-canvas-api-call-count) :to-equal 0))))))

(describe "conflict handling on push"
  (it "skips and marks a heading Canvas has since regraded"
    (with-org-canvas-test-config
      (with-mock-api
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
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92")))))
  (it "skips a student who resubmitted since the pull"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:END:\n")
          (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                     (lambda (_id) '((5001 . ("92" 2 nil)))))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades))
          (expect (test-org-canvas-api-call-count) :to-equal 0)
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CONFLICT") :to-equal "attempt: Canvas has 2")))))
  (it "pushes the clean changes and clears CONFLICT once resolved"
    (with-org-canvas-test-config
      (with-mock-api
        ;; The bulk endpoint answers a Progress object; its job has
        ;; finished here, so nothing is polled (issue #382).
        (setq test-org-canvas-api-responses
              '(("update_grades" . ((id . 77) (workflow_state . "completed")))))
        (with-grading-file (concat test-grading-file-header
                                   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:CONFLICT: stale\n:END:\n\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:CANVAS_SCORE: 75\n:END:\n")
          (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                     (lambda (_id) '((5001 . ("92" nil nil)) (5002 . ("75" nil nil)))))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades))
          (expect-api-called 'POST "assignments/1001/submissions/update_grades")
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CONFLICT") :to-be nil)
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "95")))))
  (it "does not consult Canvas for an ephemeral buffer"
    (with-org-canvas-test-config
      (with-mock-api
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
          (expect-api-called 'PUT "assignments/1001/submissions/5001"))))))

(defun test-refresh--from-canvas (overrides)
  "Refresh the current grading file as if Canvas held one submission.
OVERRIDES adjust `test-org-canvas-make-submission'; a prompt fails."
  (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
             (lambda (_id) (list (test-org-canvas-make-submission overrides))))
            ((symbol-function 'org-canvas--submissions-fetch-assignment)
             (lambda (_id) '((id . 1001))))
            ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
            ((symbol-function 'switch-to-buffer) (lambda (b) b))
            ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask"))))
    (org-canvas-submissions-refresh)))

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

(defvar test-refresh--messages nil "What the last `test-refresh--run' said.")
(defvar test-refresh--log nil "What the last `test-refresh--run' logged at INFO.")

(defun test-refresh--run (subs)
  "Refresh the current grading file as if Canvas held SUBS.
Messages and INFO log lines are collected in `test-refresh--messages'
and `test-refresh--log'; a prompt fails."
  (setq test-refresh--messages nil
        test-refresh--log nil)
  (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment) (lambda (_id) subs))
            ((symbol-function 'org-canvas--submissions-fetch-assignment) (lambda (_id) '((id . 1001))))
            ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
            ((symbol-function 'switch-to-buffer) (lambda (b) b))
            ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask")))
            ((symbol-function 'message)
             (lambda (fmt &rest args) (push (apply #'format fmt args) test-refresh--messages)))
            ((symbol-function 'org-canvas--log-info)
             (lambda (_logger fmt &rest args) (push (apply #'format fmt args) test-refresh--log))))
    (org-canvas-submissions-refresh)))

(defun test-refresh--summary ()
  "Return the Refreshed line of the last `test-refresh--run'."
  (cl-find-if (lambda (m) (string-prefix-p "Refreshed " m)) test-refresh--messages))

(defun test-refresh--student (name id &rest props)
  "Return a grading-file heading for NAME with USER_ID ID and PROPS."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n" name id)
          (apply #'concat props)
          ":END:\n"))

(defun test-refresh--bob (&optional overrides)
  "Return a submission for Beta, Bob (5002), adjusted by OVERRIDES."
  (test-org-canvas-make-submission
   (append overrides
           '((id . 50002) (user_id . 5002)
             (user . ((id . 5002) (name . "Bob Beta") (sortable_name . "Beta, Bob")))))))

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
                :to-equal "Refreshed HW: 1 new, 1 resubmitted after grading")
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

(defmacro with-submissions-dir (&rest body)
  "Run BODY with `org-canvas-submissions-directory' bound to a fresh DIR.
Every buffer visiting a file under it is killed afterwards, and no
prompt may be reached: `completing-read' and `y-or-n-p' both signal."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "org-canvas-subs-" t))
          (org-canvas-submissions-directory dir)
          (noninteractive t))
     (unwind-protect
         (cl-letf (((symbol-function 'completing-read)
                    (lambda (&rest _) (error "must not prompt")))
                   ((symbol-function 'y-or-n-p)
                    (lambda (&rest _) (error "must not ask")))
                   ((symbol-function 'switch-to-buffer) (lambda (b) b))
                   ((symbol-function 'org-canvas--submissions-heading-for-assignment)
                    (lambda (_id) nil))
                   ((symbol-function 'org-canvas--graphql-query) #'ignore))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p (file-truename dir) (file-truename (buffer-file-name b))))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun test-entry--canvas (assignment-object &optional listed)
  "Return an `org-canvas-api-request' stub answering for ASSIGNMENT-OBJECT.
A single-assignment read answers ASSIGNMENT-OBJECT, or signals when it
is nil; the list read answers LISTED; a submissions read answers one
submission.  Every call is pushed onto `test-entry--calls'."
  (lambda (method url &rest _)
    (push (cons method url) test-entry--calls)
    (cond ((string-match-p "/submissions$" url)
           (vector (test-org-canvas-make-submission)))
          ((string-match-p "/assignments/[0-9]+$" url)
           (or assignment-object (signal 'org-canvas-api-error (list "404"))))
          ((string-match-p "/assignments$" url)
           (vconcat listed))
          (t (error "Unexpected request %s" url)))))

(defvar test-entry--calls nil "The requests a stub of `test-entry--canvas' saw.")

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
      (expect (org-canvas-submissions-refresh-file) :to-throw 'error '("must not prompt")))))

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

(describe "org-canvas--submissions-live-baselines"
  (it "maps each student to the entered score string and attempt"
    (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
               (lambda (_id)
                 (list (test-org-canvas-make-submission
                        '((entered_score . 5.0) (score . 4.0) (attempt . 2)))
                       (test-org-canvas-make-submission
                        '((score . nil) (attempt . nil)
                          (user . ((id . 5002) (sortable_name . "Beta, Bob")))))))))
      (let ((live (org-canvas--submissions-live-baselines "1001")))
        (expect (alist-get 5001 live) :to-equal '("5" 2 nil nil))
        (expect (alist-get 5002 live) :to-equal '(nil nil nil nil)))))
  (it "carries the digest of each student's assessment"
    (let ((sub (test-org-canvas-make-submission
                '((rubric_assessment . ((_7104 . ((points . 2)))))))))
      (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                 (lambda (_id) (list sub))))
        (expect (nth 2 (alist-get 5001 (org-canvas--submissions-live-baselines "1001")))
                :to-equal (org-canvas--submissions-rubric-digest '(("_7104" "2" nil))))))))

(describe "clearing a grade (issue #417)"
  (defun test-clear--entry (name user-id props)
    "Return a student heading for NAME and USER-ID with PROPS lines."
    (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n%s:END:\n" name user-id props))

  (it "reads a typed SCORE: blank keeps the baseline, a clear word is no grade"
    (expect (org-canvas--submissions-typed-score nil "92" "A") :to-equal "92")
    (expect (org-canvas--submissions-typed-score "  " "92" "A") :to-equal "92")
    (expect (org-canvas--submissions-typed-score "none" "92" "A") :to-be nil)
    (expect (org-canvas--submissions-typed-score " None " "92" "A") :to-be nil)
    (expect (org-canvas--submissions-typed-score "-" "92" "A") :to-be nil)
    (expect (org-canvas--submissions-typed-score "95" "92" "A") :to-equal "95")
    (expect (org-canvas--submissions-typed-score "excused" nil "A") :to-equal "EX")
    (expect (org-canvas--submissions-typed-score "9O" "92" "A") :to-throw 'user-error))

  (it "pushes the empty grade and takes SCORE and CANVAS_SCORE away"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
          (let ((org-canvas-submissions-check-conflicts nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (org-canvas-submissions-push-grades)))
          (expect-api-called 'PUT "assignments/1001/submissions/5001")
          (let ((data (nth 2 (test-org-canvas-last-api-call))))
            (expect (alist-get 'submission data) :to-equal '((posted_grade . ""))))
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-be nil)
          (expect (org-entry-get (point) "SCORE") :to-be nil)
          (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)))))

  (it "leaves a grade alone when SCORE is absent or blank"
    (with-grading-file (concat test-grading-file-header
                               (test-clear--entry "Adams, Alice" 5001 ":CANVAS_SCORE: 92\n")
                               (test-clear--entry "Beta, Bob" 5002 ":SCORE:\n:CANVAS_SCORE: 80\n"))
      (org-canvas--submissions-ensure-context)
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
      (expect (org-canvas--submissions-collect-carryover) :to-be nil)))

  (it "sees no change in a clear where Canvas holds no grade"
    (with-grading-file (concat test-grading-file-header
                               (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n"))
      (org-canvas--submissions-ensure-context)
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
      (expect (org-canvas--submissions-collect-carryover) :to-be nil)))

  (it "refuses a SCORE it cannot read before anything is sent"
    (with-grading-file (concat test-grading-file-header
                               (test-clear--entry "Adams, Alice" 5001 ":SCORE: 9O\n:CANVAS_SCORE: 92\n"))
      (org-canvas--submissions-ensure-context)
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))

  (it "sends a clear through update_grades beside a score, and names it"
    (let ((asked nil))
      (with-org-canvas-test-config
        (with-mock-api
          (setq test-org-canvas-api-responses
                '(("update_grades" . ((id . 77) (workflow_state . "completed")))))
          (with-grading-file (concat test-grading-file-header
                                     (test-clear--entry "Adams, Alice" 5001 ":SCORE: -\n:CANVAS_SCORE: 92\n")
                                     (test-clear--entry "Beta, Bob" 5002 ":SCORE: 85\n:CANVAS_SCORE: 80\n"))
            (let ((org-canvas-submissions-check-conflicts nil)
                  (org-canvas-assume-yes nil)
                  (noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (q) (if asked (error "Asked twice: %s" q) (setq asked q)) t))
                        ((symbol-function 'yes-or-no-p)
                         (lambda (q) (if asked (error "Asked twice: %s" q) (setq asked q)) t)))
                (org-canvas--submissions-push-current t)))
            (let* ((call (test-org-canvas-find-api-call 'POST "update_grades"))
                   (grade-data (alist-get 'grade_data (nth 2 call))))
              (expect (alist-get "5001" grade-data nil nil #'equal)
                      :to-equal '((posted_grade . "")))
              (expect (alist-get "5002" grade-data nil nil #'equal)
                      :to-equal '((posted_grade . "85"))))
            (org-canvas--submissions-goto-user 5001)
            (expect (org-entry-get (point) "SCORE") :to-be nil)
            (expect (org-entry-get (point) "CANVAS_SCORE") :to-be nil)
            (org-canvas--submissions-goto-user 5002)
            (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "85"))))
      (expect asked :to-match "2 grade change(s) (1 clearing a grade)")))

  (it "shows a clear as clear in the list of changes"
    (expect (org-canvas--submissions-describe-changes
             (list (list :name "Adams, Alice" :old-score "92" :new-score nil :clear t)))
            :to-equal "  Adams, Alice: 92 → clear"))

  (it "sends nothing under a dry run and records nothing"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
          (let ((org-canvas-submissions-check-conflicts nil)
                (org-canvas--dry-run t))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (org-canvas-submissions-push-grades)))
          (expect (test-org-canvas-api-call-count) :to-equal 0)
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "SCORE") :to-equal "none")
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92")))))

  (it "keeps a typed clear over a re-pull until Canvas holds no grade"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
        (test-refresh--from-canvas '((score . 92) (entered_score . 92)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "none")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (plist-get (car (org-canvas--submissions-collect-grade-changes)) :clear)
                :to-be t)
        (test-refresh--from-canvas '((score . nil) (entered_score . nil)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-be nil)
        (expect (org-entry-get (point) "CONFLICT") :to-be nil))))

  (it "marks a typed clear when Canvas's grade moved under it"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
        (test-refresh--from-canvas '((score . 93) (entered_score . 93)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "none")
        (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 93"))))

  (it "reads a clear word and a blank cell in the summary table"
    (with-temp-buffer
      (org-mode)
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | graded | <2026-02-15> | none |\n")
      (insert "| Beta, Bob | graded | <2026-02-15> |  |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92") (5002 . "80")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)
                        (test-org-canvas-make-submission
                         '((user . ((id . 5002) (sortable_name . "Beta, Bob")))))))
      (let ((changes (org-canvas--submissions-collect-grade-changes)))
        (expect (length changes) :to-equal 1)
        (expect (plist-get (car changes) :user-id) :to-equal 5001)
        (expect (plist-get (car changes) :clear) :to-be t)
        (expect (org-canvas--submissions-grade-fields (car changes))
                :to-equal '((posted_grade . ""))))))

  (it "refuses a clear beside changed Rubric rows that set the grade"
    (with-rubric-file (concat test-rubric-file-header
                              (test-rubric-entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 3\n"
                                                 '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 nil nil)
                                                   ("_7106" "Style" 1 nil nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))

  (it "clears the grade alone when the Rubric rows did not change"
    (with-rubric-file (concat test-rubric-file-header
                              (test-rubric-entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 3\n"
                                                 test-rubric-blank-rows))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :clear) :to-be t)
        (expect (plist-get change :triples) :to-be nil)
        (expect (org-canvas--submissions-grade-fields change)
                :to-equal '((posted_grade . "")))))))

;;;; Links and Local Attachments

(describe "submission link helpers"
  (it "build course URLs without a doubled slash"
    (let ((org-canvas-base-url "https://canvas.example.edu/")
          (org-canvas-course-id "42"))
      (expect (org-canvas--submissions-assignment-url "7")
              :to-equal "https://canvas.example.edu/courses/42/assignments/7")
      (expect (org-canvas--submissions-speedgrader-url "7")
              :to-equal "https://canvas.example.edu/courses/42/gradebook/speed_grader?assignment_id=7")
      (expect (org-canvas--submissions-speedgrader-url "7" 5001)
              :to-equal "https://canvas.example.edu/courses/42/gradebook/speed_grader?assignment_id=7&student_id=5001"))))

(describe "org-canvas--submissions-heading-for-assignment"
  (it "finds the assignments heading by CANVAS_ID"
    (let ((file (make-temp-file "org-canvas-assignments-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Other\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n* Closer: First Stakes\n:PROPERTIES:\n:CANVAS_ID: 1001\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--submissions-assignments-file) (lambda () file)))
              (expect (org-canvas--submissions-heading-for-assignment "1001")
                      :to-equal "Closer: First Stakes")
              (expect (org-canvas--submissions-heading-for-assignment "999") :to-be nil)))
        (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
        (delete-file file))))
  (it "is nil when the assignments file does not exist"
    (cl-letf (((symbol-function 'org-canvas--submissions-assignments-file)
               (lambda () "/nonexistent/assignments.org")))
      (expect (org-canvas--submissions-heading-for-assignment "1001") :to-be nil))))

(describe "grading file links line"
  (it "links the Org heading, the Canvas page, and SpeedGrader"
    (let ((org-canvas-base-url "https://canvas.example.edu")
          (org-canvas-course-id "42")
          (org-canvas-submissions-directory "/course/submissions/"))
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment)
                 (lambda (_id) "Closer: First Stakes"))
                ((symbol-function 'org-canvas--submissions-assignments-file)
                 (lambda () "/course/assignments.org")))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail "HW" "1001" nil)
          (let ((content (buffer-string)))
            (expect content :to-match "Assignment: \\[\\[file:\\.\\./assignments\\.org::\\*Closer: First Stakes\\]\\[in Org\\]\\]")
            (expect content :to-match "\\[\\[https://canvas\\.example\\.edu/courses/42/assignments/1001\\]\\[on Canvas\\]\\]")
            (expect content :to-match "speed_grader\\?assignment_id=1001\\]\\[SpeedGrader\\]\\]"))))))
  (it "omits the Org link when no heading carries the id"
    (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil)))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1001" nil)
        (expect (buffer-string) :to-match "^Assignment: \\[\\[https://")
        (expect (buffer-string) :not :to-match "in Org")))))

(describe "per-student SpeedGrader link"
  (it "follows the property drawer when the assignment id is known"
    (let ((org-canvas-base-url "https://canvas.example.edu")
          (org-canvas-course-id "42"))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry (test-org-canvas-make-submission) "HW" "1001")
        (expect (buffer-string)
                :to-match ":END:\n\\[\\[https://canvas\\.example\\.edu/courses/42/gradebook/speed_grader\\?assignment_id=1001&student_id=5001\\]\\[Open in SpeedGrader\\]\\]"))))
  (it "is absent without an assignment id"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry (test-org-canvas-make-submission))
      (expect (buffer-string) :not :to-match "SpeedGrader"))))

(describe "attachment entries"
  (it "parses remote and local forms"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Attachments\n- [[https://example.com/files/1/download][hw.pdf]]\n- [[file:files/HW/Adams__Alice/notes.pdf][notes.pdf]] ([[https://example.com/files/2/download][Canvas]])\n\n** Comments\n- [[https://example.com/x][not an attachment]]\n"
     (org-back-to-heading)
     (let ((entries (org-canvas--submissions-attachment-entries)))
       (expect (length entries) :to-equal 2)
       (expect (plist-get (nth 0 entries) :name) :to-equal "hw.pdf")
       (expect (plist-get (nth 0 entries) :local) :to-be nil)
       (expect (plist-get (nth 1 entries) :name) :to-equal "notes.pdf")
       (expect (plist-get (nth 1 entries) :url) :to-equal "https://example.com/files/2/download")
       (expect (plist-get (nth 1 entries) :local) :to-equal "files/HW/Adams__Alice/notes.pdf"))))
  (it "renders a downloaded attachment with the local link first"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (local-dir (org-canvas--submissions-attachment-dir "HW" "Adams, Alice")))
      (unwind-protect
          (progn
            (make-directory local-dir t)
            (with-temp-file (expand-file-name "hw.pdf" local-dir) (insert "pdf"))
            (with-temp-buffer
              (org-canvas--submissions-render-attachments
               [((display_name . "hw.pdf") (url . "https://example.com/files/1/download"))
                ((display_name . "late.pdf") (url . "https://example.com/files/2/download"))]
               "HW" "Adams, Alice")
              (expect (buffer-string)
                      :to-match "- \\[\\[file:files/HW/Adams__Alice/hw\\.pdf\\]\\[hw\\.pdf\\]\\] (\\[\\[https://example\\.com/files/1/download\\]\\[Canvas\\]\\])")
              (expect (buffer-string)
                      :to-match "- \\[\\[https://example\\.com/files/2/download\\]\\[late\\.pdf\\]\\]")))
        (delete-directory dir t)))))

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

(describe "org-canvas--submissions-refresh-links"
  (it "rewrites the Assignment line when the heading was renamed, once"
    (with-grading-file (concat test-grading-file-header
                               "Assignment: [[file:../assignments.org::*Old Title][in Org]], [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/courses/1/gradebook/speed_grader?assignment_id=1001][SpeedGrader]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment)
                 (lambda (_id) "New Title"))
                ((symbol-function 'org-canvas--submissions-assignments-file)
                 (lambda () (expand-file-name "../assignments.org" dir))))
        (expect (org-canvas--submissions-refresh-links) :to-be-truthy)
        (expect (buffer-string) :to-match "^Assignment: \\[\\[file:\\.\\./assignments\\.org::\\*New Title\\]\\[in Org\\]\\]")
        (expect (buffer-string) :not :to-match "Old Title")
        (expect (count-matches "^Assignment: " (point-min) (point-max)) :to-equal 1)
        (expect (org-canvas--submissions-refresh-links) :to-be nil))))
  (it "leaves a file without an Assignment line alone"
    (with-grading-file (concat test-grading-file-header "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (expect (org-canvas--submissions-refresh-links) :to-be nil)
      (expect (buffer-string) :not :to-match "Assignment:")))
  (it "runs when a grading file is opened, and saves the result"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (file (expand-file-name "HW.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert test-grading-file-header
                      "Assignment: [[file:../assignments.org::*Old Title][in Org]], [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/courses/1/gradebook/speed_grader?assignment_id=1001][SpeedGrader]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"))
            (cl-letf (((symbol-function 'completing-read) (lambda (_p files &rest _) (car files)))
                      ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) "Fresh Title"))
                      ((symbol-function 'org-canvas--submissions-assignments-file)
                       (lambda () (expand-file-name "../assignments.org" dir))))
              (org-canvas-open-submissions))
            (expect (buffer-string) :to-match "::\\*Fresh Title\\]")
            (expect (buffer-modified-p) :to-be nil)
            (expect (with-temp-buffer (insert-file-contents file) (buffer-string)) :to-match "Fresh Title"))
        (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
        (delete-directory dir t))))
  (it "runs after a successful push"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "Assignment: [[file:../assignments.org::*Old Title][in Org]], [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/courses/1/gradebook/speed_grader?assignment_id=1001][SpeedGrader]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
          (let ((org-canvas-submissions-check-conflicts nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                      ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) "Renamed"))
                      ((symbol-function 'org-canvas--submissions-assignments-file)
                       (lambda () (expand-file-name "../assignments.org" dir))))
              (org-canvas-submissions-push-grades)))
          (expect-api-called 'PUT "assignments/1001/submissions/5001")
          (expect (buffer-string) :to-match "::\\*Renamed\\]")
          (expect (buffer-string) :to-match ":CANVAS_SCORE: 95")
          (expect (buffer-modified-p) :to-be nil))))))

;;;; Rubric Link and Criteria

(defconst test-rubric-assignment
  '((id . 1001)
    (name . "Global Challenge Essay")
    (use_rubric_for_grading . t)
    (rubric_settings . ((id . 133477) (title . "Essay Rubric") (points_possible . 100)
                        (free_form_criterion_comments . :json-false)
                        (hide_score_total . :json-false) (hide_points . :json-false)))
    (rubric . [((id . "_1") (description . "Thesis") (points . 20.0)
                (ratings . [((description . "Excellent") (points . 20.0))
                            ((description . "Weak | vague") (points . 5.0))]))
               ((id . "_2") (description . "Evidence") (points . 30)
                (ratings . [((description . "Strong") (points . 30))]))])))

(describe "org-canvas--submissions-rubric-settings"
  (it "returns the rubric id and title as strings"
    (expect (org-canvas--submissions-rubric-settings test-rubric-assignment)
            :to-equal '("133477" . "Essay Rubric")))
  (it "is nil without an attached rubric"
    (expect (org-canvas--submissions-rubric-settings '((id . 1))) :to-be nil)
    (expect (org-canvas--submissions-rubric-settings nil) :to-be nil)))

(describe "org-canvas--submissions-heading-for-rubric"
  (it "looks the rubric up in the registered rubrics file"
    (let ((file (make-temp-file "org-canvas-rubrics-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Essay Rubric\n:PROPERTIES:\n:CANVAS_ID: 133477\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--submissions-rubrics-file) (lambda () file)))
              (expect (org-canvas--submissions-heading-for-rubric "133477") :to-equal "Essay Rubric")
              (expect (org-canvas--submissions-heading-for-rubric "1") :to-be nil)))
        (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
        (delete-file file)))))

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

(defconst test-rubric-assignment-described
  '((id . 1001)
    (name . "Closer")
    (use_rubric_for_grading . t)
    (rubric_settings . ((id . 138462) (title . "Closer Rubric") (points_possible . 6)))
    (rubric . [((id . "_1") (description . "The Day&#39;s Idea Does the Work") (points . 2)
                (long_description . "<p>The answer is built from the session&#39;s material.</p><p>Feed-forward: journal row 2.</p>")
                (ratings . [((description . "Working") (points . 2)
                             (long_description . "The day&#39;s idea carries the answer"))
                            ((description . "Named") (points . 1)
                             (long_description . "Named, but the answer reads the same without it"))
                            ((description . "Absent") (points . 0) (long_description . :null))]))
               ((id . "_2") (description . "Stakes | Named") (points . 4) (long_description . :null)
                (ratings . [((description . "Yes") (points . 4))]))]))
  "An assignment whose rubric carries the descriptions Canvas escapes (issue #265).")

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

(describe "refresh-links also rebuilds the Rubric line"
  (it "recomputes it from the CANVAS_RUBRIC_ID property"
    (with-grading-file (concat test-grading-file-header
                               "#+PROPERTY: CANVAS_RUBRIC_ID 133477\n#+PROPERTY: CANVAS_RUBRIC_TITLE Old Rubric\nAssignment: [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/speed][SpeedGrader]]\nRubric: [[file:../rubrics.org::*Old Rubric][in Org]], [[https://x/courses/1/rubrics/133477][on Canvas]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
                ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) "Renamed Rubric"))
                ((symbol-function 'org-canvas--submissions-rubrics-file)
                 (lambda () (expand-file-name "../rubrics.org" dir))))
        (expect (org-canvas--submissions-refresh-links) :to-be-truthy)
        (expect (buffer-string) :to-match "^Rubric: \\[\\[file:\\.\\./rubrics\\.org::\\*Renamed Rubric\\]\\[in Org\\]\\]")
        (expect (buffer-string) :not :to-match "Old Rubric\\]")
        (expect (count-matches "^Rubric: " (point-min) (point-max)) :to-equal 1)))))

(describe "org-canvas--submissions-fetch-assignment"
  (it "GETs the assignment"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--submissions-fetch-assignment "1001")
        (expect-api-called 'GET "assignments/1001"))))
  (it "is nil when the request fails"
    (cl-letf (((symbol-function 'org-canvas-api-request) (lambda (&rest _) (error "down"))))
      (expect (org-canvas--submissions-fetch-assignment "1001") :to-be nil))))

;;;; Drafted Comments

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

(describe "pushing drafted comments"
  (it "posts each draft by user id, records it under Comments, and resets the draft"
    (with-org-canvas-test-config
      (with-mock-api
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
          (expect (buffer-modified-p) :to-be nil)))))
  (it "posts grades and comments in one confirmation"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n\n** Comment to post\nNice.\n")
          (let ((org-canvas-submissions-check-conflicts nil) (prompt nil))
            (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t)))
              (org-canvas-submissions-push-grades))
            (expect prompt :to-match "1 grade change(s) and 1 comment(s)")
            (expect (test-org-canvas-api-call-count) :to-equal 2)
            (expect (buffer-string) :to-match ":CANVAS_SCORE: 95")
            (expect (buffer-string) :to-match ":: Nice\\."))))))
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

(describe "excused submissions"
  (it "parse EX in any spelling"
    (expect (org-canvas--submissions-parse-score "EX") :to-equal "EX")
    (expect (org-canvas--submissions-parse-score " excused ") :to-equal "EX")
    (expect (org-canvas--submissions-parse-score "extra") :to-be nil))
  (it "render as SCORE EX with no FINAL_SCORE and snapshot as EX"
    (let ((sub (test-org-canvas-make-submission '((excused . t) (score . nil)))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry sub)
        (expect (buffer-string) :to-match ":SCORE: EX\n:CANVAS_SCORE: EX\n")
        (expect (buffer-string) :not :to-match "FINAL_SCORE"))
      (expect (alist-get 5001 (org-canvas--submissions-snapshot-scores (list sub))) :to-equal "EX")))
  (it "push EX as the posted grade"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: ex\n:CANVAS_SCORE: 92\n:END:\n")
          (let ((org-canvas-submissions-check-conflicts nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (org-canvas-submissions-push-grades)))
          (let ((data (nth 2 (test-org-canvas-last-api-call))))
            (expect (alist-get 'posted_grade (alist-get 'submission data)) :to-equal "EX"))
          (expect (buffer-string) :to-match ":CANVAS_SCORE: EX"))))))

(describe "DAYS_LATE"
  (it "is the lateness rounded up to whole days, only when late"
    (expect (org-canvas--submissions-days-late '((submitted_at . "2026-02-15T23:45:00Z") (seconds_late . 133200))) :to-equal 2)
    (expect (org-canvas--submissions-days-late '((submitted_at . "2026-02-15T23:45:00Z") (seconds_late . 86400))) :to-equal 1)
    (expect (org-canvas--submissions-days-late '((submitted_at . "2026-02-15T23:45:00Z") (seconds_late . 0))) :to-be nil)
    ;; a missing submission carries seconds_late as time since due; that is not lateness
    (expect (org-canvas--submissions-days-late '((submitted_at . nil) (seconds_late . 1000000))) :to-be nil)
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((late . t) (seconds_late . 400000))))
      (expect (buffer-string) :to-match ":DAYS_LATE: 5\n"))))

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

(defvar test-org-canvas-subs--rubrics-file "/tmp/org-canvas-test-rubrics.org"
  "File variable a fake rubrics feature registration points at.")

(describe "org-canvas--submissions-rubrics-file"
  (it "returns the file the rubrics feature registers"
    (let ((org-canvas--feature-registry
           (list (list :name "Rubrics" :endpoint "rubrics"
                       :file-var 'test-org-canvas-subs--rubrics-file))))
      (expect (org-canvas--submissions-rubrics-file)
              :to-equal test-org-canvas-subs--rubrics-file)))

  (it "is nil when no rubrics feature is registered"
    (let ((org-canvas--feature-registry nil))
      (expect (org-canvas--submissions-rubrics-file) :to-be nil))))

(describe "org-canvas--submissions-render-rubric-properties"
  (it "writes the points and no rubric keywords when none is attached"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties '((points_possible . 10)))
      (expect (buffer-string) :to-equal "#+PROPERTY: POINTS_POSSIBLE 10\n")))

  (it "writes the rubric id and title beside the points"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties
       '((points_possible . 12.5)
         (rubric_settings . ((id . 7) (title . "Essay")))))
      (expect (buffer-string) :to-match "POINTS_POSSIBLE 12\\.5\n")
      (expect (buffer-string) :to-match "CANVAS_RUBRIC_ID 7\n")
      (expect (buffer-string) :to-match "CANVAS_RUBRIC_TITLE Essay\n"))))

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
    (with-org-canvas-test-config
      (with-mock-api
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
            (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 70"))))))
  (it "says how to clear a marked conflict when there is nothing else to push"
    (with-org-canvas-test-config
      (with-mock-api
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
                    :to-equal "Nothing to push; 1 conflict(s) marked CONFLICT: pull again, or set CANVAS_SCORE to Canvas's value to override")))))))

(describe "posting grades (issue #202)"
  (it "records the assignment's effective policy in the file header"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties '((points_possible . 10) (post_manually . t)))
      (expect (buffer-string) :to-match "#\\+PROPERTY: POST_POLICY manual\n"))
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties '((points_possible . 10)))
      (expect (buffer-string) :not :to-match "POST_POLICY")))

  (it "renders POSTED_AT for a posted submission and nothing for an unposted one"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((score . 5) (posted_at . "2026-09-10T15:00:00Z"))) "1001")
      (expect (buffer-string) :to-match ":POSTED_AT: <2026-09-10"))
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((score . 5) (posted_at . :null))) "1001")
      (expect (buffer-string) :not :to-match "POSTED_AT"))
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((score . 5) (posted_at . "not a date"))) "HW" "1001")
      (expect (buffer-string) :to-match ":POSTED_AT: not a date")))

  (it "reads the policy from the file, so no request is needed at push time"
    (with-grading-file (concat test-grading-file-header "#+PROPERTY: POST_POLICY manual\n* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (expect (org-canvas--submissions-post-manually-p) :to-be t))
    (with-grading-file (concat test-grading-file-header "* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (expect (org-canvas--submissions-post-manually-p) :to-be nil)))

  (it "offers to post after a push only under a manual policy, and posts on yes"
    (with-grading-file (concat test-grading-file-header "#+PROPERTY: POST_POLICY manual\n* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((asked nil) (posted nil) (noninteractive nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) t))
                  ((symbol-function 'org-canvas-submissions-post-grades) (lambda () (setq posted t))))
          (org-canvas--submissions-offer-to-post 1)
          (expect asked :to-be t)
          (expect posted :to-be t)
          (setq asked nil posted nil)
          (org-canvas--submissions-offer-to-post 0)
          (expect asked :to-be nil))))
    (with-grading-file (concat test-grading-file-header "* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((asked nil) (noninteractive nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) t)))
          (org-canvas--submissions-offer-to-post 1))
        (expect asked :to-be nil))))

  (it "never posts from a batch Emacs, even with every prompt assumed yes (issue #381)"
    (with-grading-file (concat test-grading-file-header "#+PROPERTY: POST_POLICY manual\n* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((asked nil) (posted nil) (said nil)
            (noninteractive t) (org-canvas-assume-yes t))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) t))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args))))
                  ((symbol-function 'org-canvas-submissions-post-grades) (lambda () (setq posted t))))
          (org-canvas--submissions-offer-to-post 3))
        (expect asked :to-be nil)
        (expect posted :to-be nil)
        (expect said :to-match "Grades held under the manual post policy"))))

  (it "posts the assignment's grades through the mutation"
    (with-grading-file (concat test-grading-file-header "* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((seen nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (what doc vars) (setq seen (list what doc vars)) nil)))
          (org-canvas-submissions-post-grades))
        (expect (nth 1 seen) :to-match "postAssignmentGrades")
        (expect (alist-get 'assignmentId (nth 2 seen)) :to-equal "1001"))))

  (it "refuses outside a grading buffer and without an assignment id"
    (with-temp-buffer
      (expect (org-canvas-submissions-post-grades) :to-throw 'user-error))
    (with-temp-buffer
      (org-mode)
      (insert "* A\n")
      (setq-local org-canvas-submissions--assignment-id nil)
      (org-canvas-submissions-mode 1)
      (expect (org-canvas-submissions-post-grades) :to-throw 'user-error)))

  (it "binds P in the grading file and lists it in the menu"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "P")) :to-be #'org-canvas-submissions-post-grades)))


;;;; Rubric Assessments From the Grading File (issue #250)

(defconst test-rubric-file-header
  (concat test-grading-file-header
          "#+PROPERTY: POINTS_POSSIBLE 6\n"
          "#+PROPERTY: CANVAS_RUBRIC_ID 133477\n"
          "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING true\n\n")
  "Grading-file header of an assignment whose rubric sets the grade.")

(defun test-rubric-entry (name user-id props rows)
  "Return a student entry for NAME and USER-ID with PROPS and rubric ROWS.
PROPS is a string of property lines; ROWS a list of (ID CRITERION MAX
SCORE COMMENT), written as the table and the comment items under it."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n%s:END:\n\n** Rubric\n" name user-id props)
          "| Id | Criterion | Max | Score |\n|---+---+---+---|\n"
          (mapconcat (lambda (r) (apply #'format "| %s | %s | %s | %s |\n"
                                        (mapcar (lambda (c) (or c "")) (cl-subseq r 0 4))))
                     rows "")
          (mapconcat (lambda (r)
                       (format "- %s ::%s\n" (nth 0 r)
                               (if (nth 4 r)
                                   (concat " " (replace-regexp-in-string "\n" "\n  " (nth 4 r)))
                                 "")))
                     rows "")
          "\n"))

(defmacro with-rubric-file (content &rest body)
  "Visit CONTENT as a grading file with its context recovered, then run BODY."
  (declare (indent 1))
  `(with-grading-file ,content
     (org-canvas--submissions-ensure-context)
     ,@body))

(defconst test-rubric-blank-rows
  '(("_7104" "Thesis" 2 nil nil) ("_7105" "Evidence" 3 nil nil) ("_7106" "Style" 1 nil nil))
  "An unassessed rubric table's rows.")

(describe "rubric change detection"
  (it "sees no change while the rows match the baseline"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001
                                                  (format ":SCORE: 4\n:CANVAS_SCORE: 4\n:CANVAS_RUBRIC: %s\n"
                                                          (org-canvas--submissions-rubric-digest
                                                           '(("_7104" "2" "Sharp") ("_7105" "2" nil))))
                                                  '(("_7104" "Thesis" 2 "2.0" "Sharp") ("_7105" "Evidence" 3 2 nil)
                                                    ("_7106" "Style" 1 nil nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
      (expect (org-canvas--submissions-pending-count) :to-equal 0)))
  (it "reports edited rows with their digests, total, and how many are scored"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" "Thesis" 2 2 "Sharp") ("_7105" "Evidence" 3 1.5 nil)
                                                    ("_7106" "Style" 1 nil nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :user-id) :to-equal 5001)
        (expect (plist-get change :triples)
                :to-equal '(("_7104" "2" "Sharp") ("_7105" "1.5" nil) ("_7106" nil nil)))
        (expect (plist-get change :old-rubric) :to-be nil)
        (expect (plist-get change :new-rubric)
                :to-equal (org-canvas--submissions-rubric-digest '(("_7104" "2" "Sharp") ("_7105" "1.5" nil))))
        (expect (plist-get change :total) :to-equal "3.5")
        (expect (plist-get change :filled) :to-equal 2)
        (expect (plist-get change :of) :to-equal 3))))
  (it "derives the score from the rows when the rubric is used for grading"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 3\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil)
                                                    ("_7106" "Style" 1 1 nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :new-score) :to-equal "6")
        (expect (plist-get change :old-score) :to-equal "3")
        (expect (plist-get change :score-derived) :to-be t)
        (expect (org-canvas--submissions-describe-changes (list change))
                :to-equal "  Adams, Alice: 3 → 6 (rubric 3/3)"))))
  (it "keeps a score edited to the rows' total, and refuses one that disagrees"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 5.0\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil)
                                                    ("_7106" "Style" 1 nil nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :new-score) :to-equal "5.0")
        (expect (plist-get change :score-derived) :to-be nil))
      (org-canvas--submissions-goto-user 5001)
      (org-entry-put (point) "SCORE" "4")
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))
  (it "leaves the score alone when the rubric is not used for grading"
    (with-rubric-file (concat test-grading-file-header
                               "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING false\n\n"
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 3\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :new-score) :to-equal "3")
        (expect (plist-get change :score-derived) :to-be nil)
        (expect (org-canvas--submissions-change-sends-grade-p change) :to-be nil)
        (expect (plist-get change :triples) :to-be-truthy))))
  (it "refuses a score that is not a number or exceeds the criterion's points"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" nil 2 "two" nil))))
      (expect (org-canvas--submissions-collect-grade-changes)
              :to-throw 'user-error '("Rubric score \"two\" for Adams, Alice (_7104) is not a number")))
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" "Thesis" 2 3 nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error))
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" "Thesis" 2 "EX" nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))
  (it "does not count rubric edits, or the score they derive, as edits a re-pull loses"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 3\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil)))
                               "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 4\n:CANVAS_SCORE: 2\n:END:\n")
      (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 2)
      (expect (org-canvas--submissions-pending-count) :to-equal 1)))
  (it "does not treat a table emptied by hand as a change"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001
                                                  ":SCORE: 2\n:CANVAS_SCORE: 2\n:CANVAS_RUBRIC: abcdef012345\n"
                                                  test-rubric-blank-rows))
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil))))

(describe "pushing rubric assessments with S"
  (it "sends the rows with the derived grade in one PUT and records the baselines"
    (with-org-canvas-test-config
      (with-mock-api
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
          (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)))))
  (it "sends several students through the bulk endpoint, each with its own fields"
    (with-org-canvas-test-config
      (with-mock-api
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
            (expect (assq 'rubric_assessment bob) :to-be nil))))))
  (it "skips and marks a student whose rubric was assessed on Canvas since the pull"
    (with-org-canvas-test-config
      (with-mock-api
        (with-rubric-file (concat test-rubric-file-header
                                   (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 2\n:CANVAS_SCORE: 2\n"
                                                      '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil))))
          (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                     (lambda (_id) '((5001 . ("2" 1 "canvas-digest")))))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades))
          (expect (test-org-canvas-api-call-count) :to-equal 0)
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CONFLICT") :to-equal "rubric: assessed on Canvas since the pull")))))
  (it "does not hold a score-only change against a rubric that moved"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 5\n:CANVAS_SCORE: 2\n:END:\n")
          (cl-letf (((symbol-function 'org-canvas--submissions-live-baselines)
                     (lambda (_id) '((5001 . ("2" 1 "canvas-digest")))))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades))
          (expect-api-called 'PUT "assignments/1001/submissions/5001"))))))

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
  (defun test-rubric--display (subs shown-cell)
    "Render SUBS as HW's grading file, storing the buffer in SHOWN-CELL's car."
    (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) (setcar shown-cell b) b))
              ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
              ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) nil)))
      (org-canvas--submissions-display
       "HW" "1001" subs 'detail
       `((id . 1001) (use_rubric_for_grading . t) (rubric_settings . ((id . 7) (title . "R")))
         (rubric . ,(vconcat test-rubric-criteria))))))
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


;;;; Document processor reports (issue #351)

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

(defvar test-late--mutations nil "What `test-late--push' sent through the mutation.")
(defvar test-late--warnings nil "What `test-late--push' logged at WARNING.")

(defun test-late--heading (&rest props)
  "Return Alice's grading-file heading with PROPS and a SUBMISSION_ID."
  (concat test-grading-file-header
          "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SUBMISSION_ID: 50001\n"
          (apply #'concat props) ":END:\n"))

(defun test-late--push (reply &optional live)
  "Push the current grading file; the mutation answers REPLY.
REPLY is a function of the variables, or a value.  LIVE is the
student's live baseline for the conflict check, nil to skip it."
  (setq test-late--mutations nil
        test-late--warnings nil)
  (let ((org-canvas-submissions-check-conflicts (and live t)))
    (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
               (lambda (_what document variables)
                 (push (cons document variables) test-late--mutations)
                 (if (functionp reply) (funcall reply variables) reply)))
              ((symbol-function 'org-canvas--submissions-live-baselines)
               (lambda (_id) (list (cons 5001 live))))
              ((symbol-function 'org-canvas--log-warning)
               (lambda (_logger fmt &rest args)
                 (push (apply #'format fmt args) test-late--warnings)))
              ((symbol-function 'y-or-n-p) (lambda (_) t)))
      (org-canvas-submissions-push-grades))))

(defun test-late--stored (status &optional seconds)
  "Return a mutation reply saying Canvas stored STATUS, SECONDS late."
  `((updateSubmissionGradeStatus
     . ((submission . ((_id . "50001") (latePolicyStatus . ,(or status :null))
                       (secondsLate . ,(or seconds 0))))
        (errors . :null)))))

(describe "the late status in a grading file (issue #352)"
  (it "is written with its baseline when Canvas holds one, and not otherwise"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((late_policy_status . "extended"))))
      (expect (buffer-string) :to-match ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: extended\n"))
    (dolist (status '(nil :null ""))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry
         (test-org-canvas-make-submission `((late_policy_status . ,status))))
        (expect (buffer-string) :not :to-match "LATE_STATUS"))))
  (it "writes a status Canvas returns that the push does not know, as it comes"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((late_policy_status . "excused_by_policy"))))
      (expect (buffer-string) :to-match ":LATE_STATUS: excused_by_policy\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-late-status-change-at-point "Adams") :to-be nil))))

(describe "org-canvas--submissions-late-status-change-at-point"
  (it "is a change when the typed status differs from its baseline, in any case"
    (with-grading-file (test-late--heading ":LATE_STATUS:  Extended \n:CANVAS_LATE_STATUS: late\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (org-canvas--submissions-late-status-change-at-point "Adams")
              :to-equal '(:late-status "extended" :old-late-status "late"
                          :submission-id "50001"))))
  (it "is a change on a row with no status yet"
    (with-grading-file (test-late--heading ":LATE_STATUS: missing\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (plist-get (org-canvas--submissions-late-status-change-at-point "Adams")
                         :old-late-status)
              :to-be nil)))
  (it "is no change when it matches, is blank, or is absent"
    (dolist (props '(":LATE_STATUS: late\n:CANVAS_LATE_STATUS: late\n"
                     ":LATE_STATUS:\n:CANVAS_LATE_STATUS: late\n"
                     ":CANVAS_LATE_STATUS: late\n"
                     ""))
      (with-grading-file (test-late--heading props)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-canvas--submissions-late-status-change-at-point "Adams") :to-be nil))))
  (it "refuses a value Canvas does not take, naming the ones it does"
    (with-grading-file (test-late--heading ":LATE_STATUS: tardy\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (org-canvas--submissions-late-status-change-at-point "Adams")
              :to-throw 'user-error
              '("Adams: LATE_STATUS tardy is not one of late, missing, extended, none"))))
  (it "refuses a heading with no submission id to address"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:LATE_STATUS: late\n:END:\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (org-canvas--submissions-late-status-change-at-point "Adams")
              :to-throw 'user-error))))

(describe "pushing a late status with S"
  (it "sends a status-only change through the mutation and nothing over REST"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-late--heading ":SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"
                                               ":DAYS_LATE: 3\n:SCORE: 92\n:CANVAS_SCORE: 92\n"
                                               ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
          (test-late--push (test-late--stored "extended" 0))
          (expect (test-org-canvas-api-call-count) :to-equal 0)
          (expect (length test-late--mutations) :to-equal 1)
          (expect (caar test-late--mutations) :to-be org-canvas--submissions-late-status-mutation)
          (expect (cdar test-late--mutations)
                  :to-equal '((submissionId . "50001") (status . "extended")))
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "extended")
          (expect (org-entry-get (point) "DAYS_LATE") :to-be nil)
          (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
          (expect (buffer-modified-p) :to-be nil)))))
  (it "sends a score and a status together, the grade over REST"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-late--heading ":SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"
                                               ":SCORE: 80\n:CANVAS_SCORE: 92\n:LATE_STATUS: late\n")
          (let ((prompt nil) (messages nil))
            (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                      ((symbol-function 'org-canvas--graphql-mutate)
                       (lambda (&rest _) (test-late--stored "late" 90000))))
              (let ((org-canvas-submissions-check-conflicts nil))
                (org-canvas-submissions-push-grades)))
            (expect prompt :to-match "1 grade change(s) (1 setting a late status)")
            (expect (cl-some (lambda (m) (string-match-p "92 → 80 (late status: no status → late)" m))
                             messages)
                    :to-be-truthy)
            (expect (car messages) :to-match "Pushed 1 grade(s), 1 late status(es) and 0 comment(s)"))
          (expect-api-called 'PUT "assignments/1001/submissions/5001")
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "80")
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "late")
          (expect (org-entry-get (point) "DAYS_LATE") :to-equal "2")))))
  (it "records what Canvas stored when it is not what was sent, and says so"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-late--heading ":LATE_STATUS: none\n:CANVAS_LATE_STATUS: missing\n")
          (test-late--push (test-late--stored nil))
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "LATE_STATUS") :to-be nil)
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-be nil)
          (expect test-late--warnings
                  :to-contain "[Submissions] Adams, Alice: Canvas stored late status no status, not the none sent")))))
  (it "takes the status sent as stored when the reply names no submission"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-late--heading ":DAYS_LATE: 3\n:LATE_STATUS: missing\n")
          (test-late--push '((updateSubmissionGradeStatus . ((submission . :null) (errors . :null)))))
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
          (expect (org-entry-get (point) "DAYS_LATE") :to-equal "3")
          (expect test-late--warnings :to-be nil)))))
  (it "leaves a refused status a change, warns once, and sends the others"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat (test-late--heading ":LATE_STATUS: late\n")
                                   "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SUBMISSION_ID: 50002\n"
                                   ":LATE_STATUS: missing\n:END:\n")
          (let ((messages nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (test-late--push
               (lambda (variables)
                 (if (equal (alist-get 'submissionId variables) "50001")
                     '((updateSubmissionGradeStatus
                        . ((submission . :null)
                           (errors . [((attribute . "late_policy_status")
                                       (message . "is not allowed"))]))))
                   (test-late--stored "missing")))))
            (expect (car messages)
                    :to-match "Pushed 0 grade(s), 1 late status(es) and 0 comment(s); late status not set for Adams, Alice (see the log)"))
          (expect (length test-late--warnings) :to-equal 1)
          (expect (car test-late--warnings) :to-match "Adams, Alice not set: .*is not allowed")
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-be nil)
          (org-canvas--submissions-goto-user 5002)
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
          (let ((left (org-canvas--submissions-collect-grade-changes)))
            (expect (length left) :to-equal 1)
            (expect (plist-get (car left) :user-id) :to-equal 5001))))))
  (it "records nothing under a dry run"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-late--heading ":LATE_STATUS: late\n")
          (test-late--push org-canvas--dry-run-response)
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-be nil)
          (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 1)))))
  (it "sends nothing for a value Canvas does not take"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-late--heading ":LATE_STATUS: tardy\n")
          (expect (test-late--push (test-late--stored "late")) :to-throw 'user-error)
          (expect test-late--mutations :to-be nil)
          (expect (test-org-canvas-api-call-count) :to-equal 0)))))
  (it "skips and marks a row whose status Canvas changed since the pull"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-late--heading ":SCORE: 92\n:CANVAS_SCORE: 92\n"
                                               ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
          (test-late--push (test-late--stored "extended") '("92" nil nil "missing"))
          (expect test-late--mutations :to-be nil)
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CONFLICT") :to-equal "late status: Canvas has missing")
          (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "late")))))
  (it "names a status Canvas cleared as no status"
    (expect (org-canvas--submissions-conflict-p
             '(:old-score "92" :late-status "missing" :old-late-status "late")
             '("92" nil nil nil))
            :to-equal "late status: Canvas has no status")
    (expect (org-canvas--submissions-conflict-p
             '(:old-score "92" :new-score "95")
             '("92" nil nil "late"))
            :to-be nil))
  (it "reads the live status alongside the score"
    (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
               (lambda (_id) (list (test-org-canvas-make-submission
                                    '((late_policy_status . "late")))))))
      (expect (nth 3 (alist-get 5001 (org-canvas--submissions-live-baselines "1001")))
              :to-equal "late"))))

(describe "a re-pull keeps a typed late status"
  (it "keeps it against an unmoved baseline, and asks nothing"
    (with-org-canvas-test-config
      (with-grading-file (test-late--heading ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
        (test-refresh--from-canvas '((late_policy_status . "late")))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "extended")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "late")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil))))
  (it "marks the heading when Canvas set another status since"
    (with-org-canvas-test-config
      (with-grading-file (test-late--heading ":LATE_STATUS: extended\n")
        (test-refresh--from-canvas '((late_policy_status . "missing")))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "extended")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
        (expect (org-entry-get (point) "CONFLICT") :to-equal "late status: Canvas has missing"))))
  (it "writes nothing when Canvas now holds the typed status"
    (with-org-canvas-test-config
      (with-grading-file (test-late--heading ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
        (test-refresh--from-canvas '((late_policy_status . "extended")))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "extended")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)))))

(describe "org-canvas--submissions-record-days-late"
  (it "rewrites DAYS_LATE on a submitted row only"
    (with-grading-file (test-late--heading ":SUBMITTED_AT: <2026-02-15 Sun 23:45>\n:DAYS_LATE: 1\n")
      (org-canvas--submissions-goto-user 5001)
      (org-canvas--submissions-record-days-late 200000)
      (expect (org-entry-get (point) "DAYS_LATE") :to-equal "3")
      (org-canvas--submissions-record-days-late nil)
      (expect (org-entry-get (point) "DAYS_LATE") :to-equal "3"))
    (with-grading-file (test-late--heading ":DAYS_LATE: 1\n")
      (org-canvas--submissions-goto-user 5001)
      (org-canvas--submissions-record-days-late 0)
      (expect (org-entry-get (point) "DAYS_LATE") :to-equal "1"))))

;;;; Attempt history (issue #352)

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
        (expect (test-refresh--summary) :to-equal "Refreshed HW: 1 resubmitted after grading")
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

(defvar test-bank--live nil "The bank `test-bank--run' reads, as (ID . TEXT) pairs.")
(defvar test-bank--sent nil "What `test-bank--run' sent, as (DOCUMENT . VARIABLES).")
(defvar test-bank--warnings nil "What `test-bank--run' logged at WARNING.")
(defvar test-bank--messages nil "What `test-bank--run' said.")

(defun test-bank--file (&rest lines)
  "Return a grading file with a Comment Bank section of LINES and one student."
  (concat test-grading-file-header
          "* Comment Bank\n" (apply #'concat lines)
          "\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n"))

(defun test-bank--reply (kind id text)
  "Return a KIND mutation reply for the saved comment ID holding TEXT."
  `((,kind . ((commentBankItem . ((_id . ,id) (comment . ,text))) (errors . :null)))))

(defun test-bank--run (fn &optional mutate)
  "Call FN with the bank reading `test-bank--live' and MUTATE answering mutations.
MUTATE is a function of the document and variables; by default a
create answers id 9001 and an update its own id."
  (setq test-bank--sent nil test-bank--warnings nil test-bank--messages nil)
  (cl-letf (((symbol-function 'org-canvas--submissions-self-id) (lambda () "77"))
            ((symbol-function 'org-canvas--graphql-query)
             (lambda (_document &optional _variables)
               `((user . ((commentBankItemsConnection
                           . ((pageInfo . ((hasNextPage . :json-false) (endCursor . :null)))
                              (nodes . ,(vconcat (mapcar (lambda (p) `((_id . ,(car p)) (comment . ,(cdr p))))
                                                         test-bank--live))))))))))
            ((symbol-function 'org-canvas--graphql-mutate)
             (lambda (_what document variables)
               (push (cons document variables) test-bank--sent)
               (if mutate
                   (funcall mutate document variables)
                 (if (eq document org-canvas--submissions-bank-create-mutation)
                     (test-bank--reply 'createCommentBankItem "9001" (alist-get 'comment variables))
                   (test-bank--reply 'updateCommentBankItem (alist-get 'id variables)
                                     (alist-get 'comment variables))))))
            ((symbol-function 'org-canvas--log-warning)
             (lambda (_logger fmt &rest args) (push (apply #'format fmt args) test-bank--warnings)))
            ((symbol-function 'message)
             (lambda (fmt &rest args) (push (apply #'format fmt args) test-bank--messages)))
            ((symbol-function 'y-or-n-p) (lambda (_) t)))
    (funcall fn)))

(defun test-bank--section ()
  "Return the Comment Bank section's text."
  (let ((region (org-canvas--submissions-bank-region)))
    (buffer-substring-no-properties (car region) (cdr region))))

(describe "the Comment Bank heading of a grading file (issue #352)"
  (it "sits after the rubric and before the first student, with its template"
    (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil)))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1001" (list (test-org-canvas-make-submission)))
        (expect (buffer-string)
                :to-match "^\\* Comment Bank\n# Saved comments for SpeedGrader's comment library[^*]*\n\\* Adams, Alice\n")
        (expect (org-canvas--submissions-bank-items) :to-be nil)
        (expect (mapcar #'car (org-canvas--submissions-heading-rows)) :to-equal '("Adams, Alice")))))
  (it "is left out when the template is nil"
    (let ((org-canvas-submissions-comment-bank-template nil))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1001" nil)
        (expect (buffer-string) :not :to-match "Comment Bank")))))

(describe "org-canvas--submissions-bank-items"
  (it "reads labelled and new items, several lines each, and skips the template"
    (with-grading-file (test-bank--file "# a template line\n"
                                        "- 4821 :: Cite the source.\n"
                                        "- Show your units,\n  every time.\n\n  Really.\n"
                                        "- 4822 ::\n"
                                        "-  \n")
      (let ((items (org-canvas--submissions-bank-items)))
        (expect (mapcar (lambda (i) (list (plist-get i :id) (plist-get i :text))) items)
                :to-equal '(("4821" "Cite the source.")
                            (nil "Show your units,\nevery time.\n\nReally."))))))
  (it "answers nil without a section"
    (with-grading-file test-grading-file-header
      (expect (org-canvas--submissions-bank-items) :to-be nil)
      (expect (org-canvas--submissions-bank-pending) :to-be nil))))

(describe "org-canvas--submissions-bank-pending"
  (it "is the new items and the labelled ones edited since their baseline"
    (with-grading-file (test-bank--file
                        (format ":PROPERTIES:\n:CANVAS_COMMENT_BANK: 4821=%s 4822=%s\n:END:\n"
                                (org-canvas--submissions-bank-digest "Cite the source.")
                                (org-canvas--submissions-bank-digest "Old text."))
                        "- 4821 :: Cite the source.\n- 4822 :: New text.\n- 4823 :: Hand labelled.\n- Fresh.\n")
      (expect (mapcar (lambda (i) (plist-get i :text)) (org-canvas--submissions-bank-pending))
              :to-equal '("New text." "Hand labelled." "Fresh.")))))

(describe "pushing the comment bank with S"
  (it "creates a new item after reading the bank, labels it and records its baseline"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-bank--file "- Show your units.\n")
          (setq test-bank--live '(("4821" . "Cite the source.")))
          (test-bank--run #'org-canvas-submissions-push-grades)
          (expect (length test-bank--sent) :to-equal 1)
          (expect (caar test-bank--sent) :to-be org-canvas--submissions-bank-create-mutation)
          (expect (cdar test-bank--sent)
                  :to-equal '((courseId . "99999") (assignmentId . "1001") (comment . "Show your units.")))
          (expect (test-bank--section) :to-match "^- 9001 :: Show your units\\.$")
          (expect (test-bank--section) :not :to-match "Cite the source")
          (expect (org-canvas--submissions-bank-baseline)
                  :to-equal (list (cons "9001" (org-canvas--submissions-bank-digest "Show your units."))))
          (expect (car test-bank--messages) :to-match "; comment bank: 1 saved")
          (expect (buffer-modified-p) :to-be nil)
          ;; A second S has nothing to send.
          (test-bank--run #'org-canvas-submissions-push-grades)
          (expect test-bank--sent :to-be nil)
          (expect (car test-bank--messages) :to-equal "Nothing to push")))))
  (it "labels an item whose text the bank already holds instead of creating it"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-bank--file "- Cite the  source.\n")
          (setq test-bank--live '(("4821" . "Cite the  source.")))
          (test-bank--run #'org-canvas-submissions-push-grades)
          (expect test-bank--sent :to-be nil)
          (expect (test-bank--section) :to-match "^- 4821 :: Cite the  source\\.$")
          (expect (car test-bank--messages) :to-match "; comment bank: 1 already in the bank")))))
  (it "rewrites an item edited here, and leaves one edited in SpeedGrader too"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-bank--file
                            (format ":PROPERTIES:\n:CANVAS_COMMENT_BANK: 4821=%s 4822=%s\n:END:\n"
                                    (org-canvas--submissions-bank-digest "Old one.")
                                    (org-canvas--submissions-bank-digest "Old two."))
                            "- 4821 :: New one.\n- 4822 :: New two.\n- 4823 :: Gone.\n")
          (setq test-bank--live '(("4821" . "Old one.") ("4822" . "Two, as SpeedGrader has it.")))
          (test-bank--run #'org-canvas-submissions-push-grades)
          (expect (length test-bank--sent) :to-equal 1)
          (expect (cdar test-bank--sent) :to-equal '((id . "4821") (comment . "New one.")))
          (expect (org-canvas--submissions-bank-baseline)
                  :to-equal (list (cons "4821" (org-canvas--submissions-bank-digest "New one."))
                                  (cons "4822" (org-canvas--submissions-bank-digest "Old two."))))
          (expect test-bank--warnings
                  :to-contain "[Submissions] Saved comment 4822 was edited here and in SpeedGrader; B reads Canvas's text in")
          (expect test-bank--warnings
                  :to-contain "[Submissions] Saved comment 4823 is no longer in the bank; remove its label to create it again")
          (expect (car test-bank--messages) :to-match "; comment bank: 1 rewritten, 2 skipped")))))
  (it "records a labelled item Canvas already holds as typed, sending nothing"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-bank--file "- 4821 :: Same.\n")
          (setq test-bank--live '(("4821" . "Same.")))
          (test-bank--run #'org-canvas-submissions-push-grades)
          (expect test-bank--sent :to-be nil)
          (expect (org-canvas--submissions-bank-baseline)
                  :to-equal (list (cons "4821" (org-canvas--submissions-bank-digest "Same."))))))))
  (it "leaves a refused item new, warns once, and sends the others"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-bank--file "- Refused.\n- Kept.\n")
          (setq test-bank--live nil)
          (test-bank--run #'org-canvas-submissions-push-grades
                          (lambda (_document variables)
                            (if (equal (alist-get 'comment variables) "Refused.")
                                '((createCommentBankItem
                                   . ((commentBankItem . :null)
                                      (errors . [((attribute . "comment") (message . "is too long"))]))))
                              (test-bank--reply 'createCommentBankItem "9002" "Kept."))))
          (expect (test-bank--section) :to-match "^- Refused\\.$")
          (expect (test-bank--section) :to-match "^- 9002 :: Kept\\.$")
          (expect (length test-bank--warnings) :to-equal 1)
          (expect (car test-bank--warnings) :to-match "Saved comment not sent (Refused\\.): .*is too long")
          (expect (car test-bank--messages) :to-match "; comment bank: 1 saved, 1 failed")))))
  (it "calls a reply with no saved comment a failure"
    (expect (org-canvas--submissions-bank-reply-item
             '((createCommentBankItem . ((commentBankItem . :null) (errors . :null))))
             'createCommentBankItem)
            :to-throw 'org-canvas-api-error))
  (it "sends nothing when the bank cannot be read, and says so"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-bank--file "- New.\n")
          (test-bank--run (lambda ()
                            (cl-letf (((symbol-function 'org-canvas--submissions-self-id)
                                       (lambda () (error "HTTP 401"))))
                              (org-canvas-submissions-push-grades))))
          (expect test-bank--sent :to-be nil)
          (expect (test-bank--section) :to-match "^- New\\.$")
          (expect (car test-bank--warnings) :to-match "Could not read the comment bank (HTTP 401)")
          (expect (car test-bank--messages) :to-match "; comment bank not read (see the log)")))))
  (it "labels nothing under a dry run"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-bank--file "- New.\n- 4821 :: Edited.\n")
          (setq test-bank--live '(("4821" . "Before.")))
          (test-bank--run #'org-canvas-submissions-push-grades
                          (lambda (&rest _) org-canvas--dry-run-response))
          (expect (length test-bank--sent) :to-equal 2)
          (expect (test-bank--section) :to-match "^- New\\.$")
          (expect (org-canvas--submissions-bank-baseline) :to-be nil)))))
  (it "names the saved comments in the confirmation"
    (expect (org-canvas--submissions-describe-push nil '(x) '(a b))
            :to-equal "1 comment(s) and 2 saved comment(s)")
    (expect (org-canvas--submissions-describe-bank nil) :to-equal "")
    (expect (org-canvas--submissions-describe-bank '(:created 0 :adopted 0 :updated 0 :skipped 0 :failed 0))
            :to-equal "")))

(describe "the Comment Bank across a refresh"
  (it "comes back as it stood, items and baseline, with no second heading"
    (with-org-canvas-test-config
      (with-grading-file (test-bank--file ":PROPERTIES:\n:CANVAS_COMMENT_BANK: 4821=abc\n:END:\n"
                                          "- 4821 :: Cite the source.\n- Show your units.\n")
        (test-refresh--from-canvas nil)
        (expect (how-many "^\\* Comment Bank$" (point-min) (point-max)) :to-equal 1)
        (expect (test-bank--section)
                :to-equal "* Comment Bank\n:PROPERTIES:\n:CANVAS_COMMENT_BANK: 4821=abc\n:END:\n- 4821 :: Cite the source.\n- Show your units.\n\n")
        (expect (buffer-string) :to-match "- Show your units\\.\n\n\\* Adams, Alice"))))
  (it "is kept when the template is off, before the first student"
    (with-org-canvas-test-config
      (let ((org-canvas-submissions-comment-bank-template nil))
        (with-grading-file (test-bank--file "- Show your units.\n")
          (test-refresh--from-canvas nil)
          (expect (buffer-string) :to-match "^\\* Comment Bank\n- Show your units\\.\n\n\\* Adams, Alice"))))))

(describe "org-canvas-submissions-pull-comment-bank"
  (it "adds what Canvas holds, takes Canvas's text for unedited items, keeps edited ones"
    (with-org-canvas-test-config
      (with-grading-file (test-bank--file
                          (format ":PROPERTIES:\n:CANVAS_COMMENT_BANK: 4821=%s 4822=%s\n:END:\n"
                                  (org-canvas--submissions-bank-digest "Old one.")
                                  (org-canvas--submissions-bank-digest "Old two."))
                          "- 4821 :: Old one.\n- 4822 :: Mine now.\n- Unsent.\n")
        (org-canvas--submissions-ensure-context)
        (setq test-bank--live '(("4821" . "One from SpeedGrader.") ("4822" . "Two from SpeedGrader.")
                                ("4830" . "Brand new,\nover two lines.")))
        (test-bank--run #'org-canvas-submissions-pull-comment-bank)
        (expect test-bank--sent :to-be nil)
        (expect (test-bank--section)
                :to-match "- 4821 :: One from SpeedGrader\\.\n- 4822 :: Mine now\\.\n- Unsent\\.\n- 4830 :: Brand new,\n  over two lines\\.\n")
        (expect (car test-bank--messages) :to-equal "Comment bank: 1 added, 1 rewritten from Canvas")
        (expect (mapcar #'car (org-canvas--submissions-bank-baseline)) :to-equal '("4821" "4822" "4830"))
        (expect (cdr (assoc "4822" (org-canvas--submissions-bank-baseline)))
                :to-equal (org-canvas--submissions-bank-digest "Old two."))
        (expect (buffer-modified-p) :to-be nil))))
  (it "writes the section when the file has none, and reads an empty bank"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
        (org-canvas--submissions-ensure-context)
        (setq test-bank--live nil)
        (let ((org-canvas-submissions-comment-bank-template nil))
          (test-bank--run #'org-canvas-submissions-pull-comment-bank))
        (expect (buffer-string) :to-match "^\\* Comment Bank\n\n\\* Adams, Alice")
        (expect (car test-bank--messages) :to-equal "Comment bank: 0 added, 0 rewritten from Canvas"))))
  (it "writes the section with its template at the end of a file with no student"
    (with-org-canvas-test-config
      (with-grading-file test-grading-file-header
        (org-canvas--submissions-ensure-context)
        (setq test-bank--live '(("4821" . "Cite the source.")))
        (test-bank--run #'org-canvas-submissions-pull-comment-bank)
        (expect (buffer-string)
                :to-match "^\\* Comment Bank\n:PROPERTIES:\n:CANVAS_COMMENT_BANK: 4821=[0-9a-f]+\n:END:\n# Saved comments[^*]*\n- 4821 :: Cite the source\\.\n\\'")
        (expect (mapcar (lambda (i) (plist-get i :id)) (org-canvas--submissions-bank-items))
                :to-equal '("4821")))))
  (it "refuses when the bank cannot be read"
    (with-org-canvas-test-config
      (with-grading-file (test-bank--file "- New.\n")
        (org-canvas--submissions-ensure-context)
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-bank) #'ignore))
          (expect (org-canvas-submissions-pull-comment-bank) :to-throw 'user-error)))))
  (it "refuses outside a submissions buffer"
    (with-temp-buffer
      (expect (org-canvas-submissions-pull-comment-bank) :to-throw 'user-error))))

(describe "org-canvas-submissions-delete-comment-bank-item"
  (it "deletes the item at point on Canvas, then from the section and the baseline"
    (with-org-canvas-test-config
      (with-grading-file (test-bank--file ":PROPERTIES:\n:CANVAS_COMMENT_BANK: 4821=abc 4822=def\n:END:\n"
                                          "- 4821 :: Cite the source.\n- 4822 :: Show your units,\n  always.\n")
        (re-search-forward "always")
        (test-bank--run #'org-canvas-submissions-delete-comment-bank-item
                        (lambda (&rest _)
                          '((deleteCommentBankItem . ((commentBankItemId . "4822") (errors . :null))))))
        (expect (cdar test-bank--sent) :to-equal '((id . "4822")))
        (expect (caar test-bank--sent) :to-be org-canvas--submissions-bank-delete-mutation)
        (expect (test-bank--section) :not :to-match "4822\\|always")
        (expect (test-bank--section) :to-match "- 4821 :: Cite the source\\.")
        (expect (org-canvas--submissions-bank-baseline) :to-equal '(("4821" . "abc")))
        (expect (car test-bank--messages) :to-equal "Saved comment 4822 deleted"))))
  (it "keeps the item when Canvas refuses, and under a dry run"
    (with-org-canvas-test-config
      (with-grading-file (test-bank--file "- 4821 :: Cite the source.\n")
        (re-search-forward "Cite")
        (expect (test-bank--run #'org-canvas-submissions-delete-comment-bank-item
                                (lambda (&rest _)
                                  '((deleteCommentBankItem
                                     . ((commentBankItemId . "4821")
                                        (errors . [((message . "not yours"))]))))))
                :to-throw 'org-canvas-api-error)
        (test-bank--run #'org-canvas-submissions-delete-comment-bank-item
                        (lambda (&rest _) org-canvas--dry-run-response))
        (expect (test-bank--section) :to-match "- 4821 :: Cite the source\\.")
        (expect (car test-bank--messages) :to-equal "Dry run: saved comment 4821 left in place"))))
  (it "asks first, and does nothing on no"
    (with-org-canvas-test-config
      (with-grading-file (test-bank--file "- 4821 :: Cite the source.\n")
        (re-search-forward "Cite")
        (let ((sent nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                    ((symbol-function 'org-canvas--graphql-mutate) (lambda (&rest _) (setq sent t))))
            (org-canvas-submissions-delete-comment-bank-item))
          (expect sent :to-be nil)))))
  (it "refuses off a labelled item, and outside a submissions buffer"
    (with-grading-file (test-bank--file "- New.\n")
      (re-search-forward "New")
      (expect (org-canvas-submissions-delete-comment-bank-item) :to-throw 'user-error)
      (goto-char (point-max))
      (expect (org-canvas-submissions-delete-comment-bank-item) :to-throw 'user-error))
    (with-temp-buffer
      (expect (org-canvas-submissions-delete-comment-bank-item) :to-throw 'user-error))))

(describe "org-canvas--submissions-self-id"
  (it "reads the token owner's id, and refuses an answer without one"
    (with-org-canvas-test-config
      (with-mock-api
        (expect (org-canvas--submissions-self-id) :to-equal "12345")
        (expect-api-called 'GET "/api/v1/users/self"))
      (cl-letf (((symbol-function 'org-canvas-api-request) (lambda (&rest _) '((name . "x")))))
        (expect (org-canvas--submissions-self-id) :to-throw 'org-canvas-api-error)))))

;;;; Bulk Grade Progress (issue #382)

(defmacro test-progress--with-replies (replies &rest body)
  "Run BODY with the API answering REPLIES in turn, waits recorded, not slept.
Each call is pushed onto `calls' as (METHOD URL); each wait onto
`waits'.  A reply that is the symbol `error' signals instead."
  (declare (indent 1))
  `(let ((queue ,replies) (calls nil) (waits nil))
     (cl-letf (((symbol-function 'org-canvas-api-request)
                (lambda (method url &rest _)
                  (push (list method url) calls)
                  (let ((reply (pop queue)))
                    (if (eq reply 'error)
                        (signal 'org-canvas-api-error '("Connection failed"))
                      reply))))
               ((symbol-function 'org-canvas--wait)
                (lambda (seconds &rest _) (push seconds waits)))
               ((symbol-function 'org-canvas--log-warning) #'ignore))
       ,@body)))

(defconst test-progress--two-students
  (concat test-grading-file-header
          "#+PROPERTY: POST_POLICY manual\n"
          "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n\n"
          "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:CANVAS_SCORE: 75\n:END:\n")
  "A grading file under a manual post policy with two score changes.")

(defun test-progress--scores ()
  "Return the CANVAS_SCORE of students 5001 and 5002 in this buffer."
  (mapcar (lambda (uid)
            (org-canvas--submissions-goto-user uid)
            (org-entry-get (point) "CANVAS_SCORE"))
          '(5001 5002)))

(defun test-progress--push (replies)
  "Push `test-progress--two-students' with the API answering REPLIES.
Return a plist: :calls, :waits, :asked (the post offer was made),
:said (the closing message) and :scores (each CANVAS_SCORE after)."
  (let (result)
    (with-org-canvas-test-config
      (with-grading-file test-progress--two-students
        (let ((org-canvas-submissions-check-conflicts nil)
              (org-canvas-submissions-progress-timeout 10)
              (org-canvas-submissions-progress-interval 2)
              (noninteractive nil)
              (asked nil) (said nil))
          (test-progress--with-replies replies
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt)
                         (if (string-match-p "post them" prompt) (progn (setq asked t) nil) t)))
                      ((symbol-function 'org-canvas--user-message)
                       (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
              (org-canvas-submissions-push-grades))
            (setq result (list :calls (reverse calls) :waits waits :asked asked
                               :said said :scores (test-progress--scores)))))))
    result))

(describe "org-canvas--submissions-await-progress (issue #382)"
  (it "reads the job at the configured interval until it completes"
    (with-org-canvas-test-config
      (let ((org-canvas-submissions-progress-interval 3)
            (org-canvas-submissions-progress-timeout 60))
        (test-progress--with-replies
            (list '((id . 9) (workflow_state . "running"))
                  '((id . 9) (workflow_state . "completed")))
          (let ((outcome (org-canvas--submissions-await-progress
                          '((id . 9) (workflow_state . "queued")
                            (url . "https://elsewhere.example/api/v1/progress/9"))
                          "2 grade(s)")))
            (expect (plist-get outcome :state) :to-be 'completed)
            (expect waits :to-equal '(3 3))
            (expect (length calls) :to-equal 2)
            (expect (car (car calls)) :to-be 'GET)
            ;; The address is built on the configured instance, never
            ;; taken from the reply's url.
            (expect (cadr (car calls)) :to-equal
                    (org-canvas--submissions-progress-url 9))
            (expect (cadr (car calls)) :not :to-match "elsewhere"))))))

  (it "asks nothing more of a job that has already finished"
    (test-progress--with-replies nil
      (let ((outcome (org-canvas--submissions-await-progress
                      '((id . 9) (workflow_state . "completed")) "x")))
        (expect (plist-get outcome :state) :to-be 'completed)
        (expect calls :to-be nil)
        (expect waits :to-be nil))))

  (it "reports a failed job with Canvas's message, or says Canvas gave none"
    (with-org-canvas-test-config
      (test-progress--with-replies
          (list '((id . 9) (workflow_state . "failed") (message . "grade too high")))
        (let ((outcome (org-canvas--submissions-await-progress
                        '((id . 9) (workflow_state . "queued")) "x")))
          (expect (plist-get outcome :state) :to-be 'failed)
          (expect (plist-get outcome :message) :to-equal "grade too high")))
      (test-progress--with-replies nil
        (let ((outcome (org-canvas--submissions-await-progress
                        '((id . 9) (workflow_state . "failed") (message)) "x")))
          (expect (plist-get outcome :message) :to-equal "Canvas gave no reason")))))

  (it "gives up after the timeout and calls the grades unconfirmed"
    (with-org-canvas-test-config
      (let ((org-canvas-submissions-progress-interval 2)
            (org-canvas-submissions-progress-timeout 5))
        (test-progress--with-replies
            (make-list 10 '((id . 9) (workflow_state . "running")))
          (let ((outcome (org-canvas--submissions-await-progress
                          '((id . 9) (workflow_state . "queued")) "x")))
            (expect (plist-get outcome :state) :to-be 'unconfirmed)
            (expect (plist-get outcome :message) :to-equal "still running after 6s")
            (expect (length waits) :to-equal 3))))))

  (it "waits a second at a time when the interval is not a positive number"
    (with-org-canvas-test-config
      (let ((org-canvas-submissions-progress-interval 0)
            (org-canvas-submissions-progress-timeout 2))
        (test-progress--with-replies
            (make-list 5 '((id . 9) (workflow_state . "running")))
          (org-canvas--submissions-await-progress '((id . 9) (workflow_state . "queued")) "x")
          (expect waits :to-equal '(1 1))))))

  (it "calls the grades unconfirmed when the progress cannot be read"
    (with-org-canvas-test-config
      (test-progress--with-replies (list 'error)
        (let ((outcome (org-canvas--submissions-await-progress
                        '((id . 9) (workflow_state . "running")) "x")))
          (expect (plist-get outcome :state) :to-be 'unconfirmed)
          (expect (plist-get outcome :message) :to-equal "the progress could not be read")))))

  (it "calls the grades unconfirmed when Canvas answered no Progress at all"
    (test-progress--with-replies nil
      (let ((outcome (org-canvas--submissions-await-progress '((name . "Mock")) "x")))
        (expect (plist-get outcome :state) :to-be 'unconfirmed)
        (expect (plist-get outcome :message) :to-match "no progress")
        (expect calls :to-be nil)))))

(describe "org-canvas--submissions-progress-url"
  (it "addresses the progress under the configured instance, trailing slash trimmed"
    (let ((org-canvas-base-url "https://canvas.example.edu/"))
      (expect (org-canvas--submissions-progress-url 42)
              :to-equal "https://canvas.example.edu/api/v1/progress/42"))))

(describe "a bulk grade push waits for Canvas's job (issue #382)"
  (it "records the baselines and offers to post once the job completes"
    (let ((r (test-progress--push
              (list '((id . 9) (workflow_state . "queued"))
                    '((id . 9) (workflow_state . "completed"))))))
      (expect (mapcar #'car (plist-get r :calls)) :to-equal '(POST GET))
      (expect (plist-get r :scores) :to-equal '("95" "80"))
      (expect (plist-get r :asked) :to-be t)
      (expect (plist-get r :said) :to-match "^Pushed 2 grade(s)")))

  (it "records nothing and offers no posting when the job fails"
    (let ((r (test-progress--push
              (list '((id . 9) (workflow_state . "queued"))
                    '((id . 9) (workflow_state . "failed") (message . "grade too high"))))))
      (expect (plist-get r :scores) :to-equal '("92" "75"))
      (expect (plist-get r :asked) :to-be nil)
      (expect (plist-get r :said)
              :to-match "Canvas did not apply 2 grade(s) (grade too high); nothing recorded")))

  (it "records nothing and offers no posting when the wait runs out"
    (let ((r (test-progress--push
              (cons '((id . 9) (workflow_state . "queued"))
                    (make-list 10 '((id . 9) (workflow_state . "running")))))))
      (expect (plist-get r :scores) :to-equal '("92" "75"))
      (expect (plist-get r :asked) :to-be nil)
      (expect (length (plist-get r :waits)) :to-equal 5)
      (expect (plist-get r :said)
              :to-match "2 grade(s) sent but not confirmed (still running after 10s)")))

  (it "sends no grade under a dry run and says the bulk push would be a background job"
    (let ((logged nil) (said nil) (scores nil) (sent nil))
      (with-org-canvas-test-config
        (with-grading-file test-progress--two-students
          (let ((org-canvas-submissions-check-conflicts nil)
                (org-canvas--dry-run t))
            (test-progress--with-replies nil
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                        ((symbol-function 'org-canvas--log-info)
                         (lambda (_logger fmt &rest args)
                           (push (apply #'format fmt args) logged)))
                        ((symbol-function 'org-canvas--user-message)
                         (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
                (org-canvas-submissions-push-grades))
              (setq sent calls
                    scores (test-progress--scores))))))
      (expect sent :to-be nil)
      (expect scores :to-equal '("92" "75"))
      (expect (car logged) :to-match
              "\\[DRY-RUN\\] Would send 2 grade(s) for assignment 1001 through update_grades, a Canvas background job")
      (expect said :to-match "^Dry run: would push 2 grade(s)")))

  (it "says nothing of a background job for a single dry-run grade"
    (let ((logged nil) (state nil))
      (with-org-canvas-test-config
        (let ((org-canvas--dry-run t))
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args) (push (apply #'format fmt args) logged))))
            (setq state (plist-get (org-canvas--submissions-send-grades
                                    "1001" (list (list :user-id 1 :old-score "1" :new-score "2")))
                                   :state)))))
      (expect state :to-be 'dry-run)
      (expect (car logged) :not :to-match "background")))

  (it "still records a late status Canvas stored when the grades did not land"
    (with-grading-file test-progress--two-students
      (let ((recorded nil))
        (cl-letf (((symbol-function 'org-canvas--submissions-record-late-status)
                   (lambda (change stored)
                     (push (list (plist-get change :user-id) stored) recorded))))
          (org-canvas--submissions-record-late-only
           (list (list :user-id 5001) (list :user-id 5002) (list :user-id 5003))
           '((5001 . (:status "late")) (5002 . dry-run) (5003 . (:status "none")))))
        ;; 5002 was a dry run and 5003 has no heading.
        (expect recorded :to-equal '((5001 (:status "late"))))))))

;;;; Pushing From a Script (issue #381)

(defmacro test-batch-push--with-dir (content &rest body)
  "Run BODY with a submissions directory holding HW.org, CONTENT, unvisited.
`file' is bound to its path; any buffer visiting it is killed after."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-canvas-batch-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           ;; The grading file reads the column's reports over GraphQL
           ;; on a refresh only; a push asks nothing of it.
           ,@body)
       (let ((buf (find-buffer-visiting file)))
         (when (buffer-live-p buf)
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun test-batch-push--run (assignment post &optional replies)
  "Push ASSIGNMENT from `test-progress--two-students' as a script would.
POST is passed through; REPLIES answer the API in turn (a completed
Progress by default).  No prompt may be asked.  Return a plist:
:result, :calls, :mutations and :saved (the file's text afterwards)."
  (let (out)
    (with-org-canvas-test-config
      (test-batch-push--with-dir test-progress--two-students
        (let ((org-canvas-submissions-check-conflicts nil)
              (noninteractive t)
              (mutations nil))
          (test-progress--with-replies
              (or replies (list '((id . 9) (workflow_state . "completed"))))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _) (error "Prompted")))
                      ((symbol-function 'org-canvas--confirm)
                       (lambda (&rest _) (error "Confirmed")))
                      ((symbol-function 'org-canvas--graphql-mutate)
                       (lambda (_what doc vars) (push (list doc vars) mutations) nil))
                      ((symbol-function 'org-canvas--user-message) #'ignore))
              (let ((result (org-canvas-push-submission-grades assignment post)))
                (setq out (list :result result :calls (reverse calls)
                                :mutations mutations
                                :saved (with-temp-buffer
                                         (insert-file-contents file)
                                         (buffer-string))))))))))
    out))

(describe "org-canvas-push-submission-grades (issue #381)"
  (it "pushes a grading file named by id, with no prompt, mode or view set by hand"
    (let* ((r (test-batch-push--run "1001" nil))
           (result (plist-get r :result)))
      (expect (mapcar #'car (plist-get r :calls)) :to-equal '(POST))
      (expect (cadr (car (plist-get r :calls))) :to-match "assignments/1001/submissions/update_grades")
      (expect (plist-get result :pushed) :to-equal 2)
      (expect (plist-get result :state) :to-be 'completed)
      (expect (plist-get result :posted) :to-be nil)
      (expect (plist-get result :conflicts) :to-equal 0)
      (expect (plist-get r :mutations) :to-be nil)
      ;; The baselines were recorded and the file saved.
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 95")
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 80")))

  (it "takes an integer id and the file's name as well"
    (expect (plist-get (plist-get (test-batch-push--run 1001 nil) :result) :pushed)
            :to-equal 2)
    (expect (plist-get (plist-get (test-batch-push--run "HW" nil) :result) :pushed)
            :to-equal 2))

  (it "posts only when asked, and only once Canvas stored the grades"
    (let* ((r (test-batch-push--run "1001" t))
           (mutation (car (plist-get r :mutations))))
      (expect (plist-get (plist-get r :result) :posted) :to-be t)
      (expect (car mutation) :to-be org-canvas--submissions-post-grades-mutation)
      (expect (alist-get 'assignmentId (cadr mutation)) :to-equal "1001")))

  (it "does not post grades whose job failed, and records none of them"
    (let* ((r (test-batch-push--run
               "1001" t
               (list '((id . 9) (workflow_state . "failed") (message . "no")))))
           (result (plist-get r :result)))
      (expect (plist-get result :pushed) :to-equal 0)
      (expect (plist-get result :state) :to-be 'failed)
      (expect (plist-get result :message) :to-equal "no")
      (expect (plist-get result :posted) :to-be nil)
      (expect (plist-get r :mutations) :to-be nil)
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 92")))

  (it "posts a column with nothing left to push when asked to"
    (with-org-canvas-test-config
      (test-batch-push--with-dir (concat test-grading-file-header
                                         "* A\n:PROPERTIES:\n:USER_ID: 1\n:SCORE: 5\n:CANVAS_SCORE: 5\n:END:\n")
        (let ((posted nil) (noninteractive t))
          (cl-letf (((symbol-function 'org-canvas--submissions-post-assignment)
                     (lambda (id) (setq posted id) t)))
            (let ((result (org-canvas-push-submission-grades "1001" t)))
              (expect (plist-get result :pushed) :to-equal 0)
              (expect (plist-get result :state) :to-be nil)
              (expect (plist-get result :posted) :to-be t)
              (expect posted :to-equal "1001")))))))

  (it "refuses an id no grading file names"
    (with-org-canvas-test-config
      (test-batch-push--with-dir test-progress--two-students
        (expect (org-canvas-push-submission-grades 9999) :to-throw 'user-error)
        (expect (org-canvas-push-submission-grades "9999") :to-throw 'user-error))))

  (it "refuses an id when there is no submissions directory"
    (let ((org-canvas-submissions-directory
           (expand-file-name "no-such-dir" temporary-file-directory)))
      (expect (org-canvas--submissions-grading-file-for-id 1001) :to-be nil))))

(describe "org-canvas--submissions-post-assignment"
  (it "posts, or under a dry run sends nothing and answers nil"
    (let ((org-canvas--dry-run nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-mutate) (lambda (&rest _) nil))
                ((symbol-function 'message) #'ignore))
        (expect (org-canvas--submissions-post-assignment "1001") :to-be t)))
    (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
               (lambda (&rest _) org-canvas--dry-run-response)))
      (expect (org-canvas--submissions-post-assignment "1001") :to-be nil))))

(describe "org-canvas--submissions-ensure-context (issue #381)"
  (it "reads a saved grading file as the detail view before it holds a student"
    (with-grading-file test-grading-file-header
      (setq-local org-canvas-submissions--current-view nil)
      (org-canvas--submissions-ensure-context)
      (expect org-canvas-submissions--current-view :to-be 'detail))))

(describe "org-canvas-submissions-push-grades (issue #381)"
  (it "lets a user-error through rather than calling it a push failure"
    (with-grading-file test-progress--two-students
      (cl-letf (((symbol-function 'org-canvas--submissions-push-current)
                 (lambda (_) (user-error "SCORE disagrees with the rubric"))))
        (expect (org-canvas-submissions-push-grades) :to-throw 'user-error))))

  (it "asks nothing more when the push is declined"
    (with-grading-file test-progress--two-students
      (let ((offered nil) (org-canvas-submissions-check-conflicts nil))
        (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil))
                  ((symbol-function 'org-canvas--submissions-offer-to-post)
                   (lambda (_) (setq offered t))))
          (org-canvas-submissions-push-grades))
        (expect offered :to-be nil)))))

;;;; Sent Comments (issue #419)

(defun test-sent--file (baseline items &optional more)
  "Return a grading file whose student carries BASELINE and ITEMS under Comments.
BASELINE is a list of (ID . TEXT), written as CANVAS_COMMENTS digests;
ITEMS is the text of the Comments section.  MORE follows the student."
  (concat test-grading-file-header
          "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n"
          (if baseline
              (format ":CANVAS_COMMENTS: %s\n"
                      (mapconcat (lambda (p)
                                   (format "%s=%s" (car p)
                                           (org-canvas--submissions-comment-digest (cdr p))))
                                 baseline " "))
            "")
          ":END:\n\n** Comments\n" items "\n** Notes\nMine.\n" (or more "")))

(defun test-sent--comment (id author text)
  "Return a Canvas submission comment ID by the user AUTHOR holding TEXT."
  `((id . ,id) (author_id . ,author) (author_name . "Prof") (comment . ,text)
    (created_at . "2026-09-28T14:02:00Z")))

(defmacro test-sent--with-canvas (comments &rest body)
  "Run BODY with Canvas holding COMMENTS on Alice's submission, the grader 77."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-canvas--submissions-self-id) (lambda () "77"))
             ((symbol-function 'org-canvas--submissions-fetch-for-assignment)
              (lambda (_id)
                (list (test-org-canvas-make-submission
                       (list (cons 'submission_comments (vconcat ,comments)))))))
             ((symbol-function 'org-canvas--submissions-fetch-assignment) #'ignore)
             ((symbol-function 'switch-to-buffer) #'identity))
     (with-html-to-org-identity ,@body)))

(defun test-sent--push ()
  "Push the grading buffer at hand, confirming; return the prompt asked."
  (let ((prompt nil))
    (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t))
              ((symbol-function 'org-canvas--submissions-offer-to-post) #'ignore))
      (org-canvas-submissions-push-grades))
    prompt))

(defmacro test-sent--collecting-warnings (&rest body)
  "Run BODY and return the warnings it logged, in order."
  `(let ((warnings nil))
     (cl-letf (((symbol-function 'org-canvas--log-warning)
                (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
       ,@body)
     (nreverse warnings)))

(describe "rendering sent comments with their ids (issue #419)"
  (it "labels each comment with its id and records its digest as CANVAS_COMMENTS"
    (with-html-to-org-identity
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry
         (test-org-canvas-make-submission
          `((submission_comments
             . [,(test-sent--comment 48213 77 "Good work.")
                ,(test-sent--comment 48214 90 "First line.\nSecond line.")]))))
        (let ((content (buffer-string)))
          (expect content :to-match "^- \\*Prof\\* <[^>]+> \\[48213\\] :: Good work\\.$")
          (expect content :to-match "^- \\*Prof\\* <[^>]+> \\[48214\\] ::\n  First line\\.\n  Second line\\.$")
          (expect content :to-match
                  (format ":CANVAS_COMMENTS: 48213=%s 48214=%s\n"
                          (org-canvas--submissions-comment-digest "Good work.")
                          (org-canvas--submissions-comment-digest "First line.\nSecond line.")))))))

  (it "writes no baseline and no id for a comment Canvas gave none"
    (with-html-to-org-identity
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry
         (test-org-canvas-make-submission-with-comment))
        (expect (buffer-string) :not :to-match "CANVAS_COMMENTS")
        (expect (buffer-string) :to-match "^- \\*Prof\\. Smith\\* <[^>]+> :: Good work!$"))))

  (it "converts each comment once for its item and its digest"
    (let ((calls 0))
      (cl-letf (((symbol-function 'org-canvas--html-to-org)
                 (lambda (html) (cl-incf calls) html)))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail
           "HW" "1001"
           (list (test-org-canvas-make-submission
                  `((body . nil)
                    (submission_comments . [,(test-sent--comment 1 77 "Once.")])))))))
      (expect calls :to-equal 1))))

(describe "reading sent comments (issue #419)"
  (it "reads the items that carry an id, their marks and their paragraphs"
    (with-temp-org-buffer
        (concat "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comments\n"
                "- *Prof* <2026-09-28 Mon 14:02> [11] :: One line.\n"
                "- *You* <2026-09-29 Tue 09:00> :: No id, see [5] :: here.\n"
                "- DELETE *TA* <2026-09-28 Mon 15:00> [12] :: Gone soon.\n"
                "- *Prof* <2026-09-28 Mon 16:00> [13] ::\n  First.\n\n  Second.\n\n"
                "** Notes\n")
      (org-back-to-heading t)
      (let ((items (org-canvas--submissions-sent-comments)))
        (expect (mapcar (lambda (i) (plist-get i :id)) items) :to-equal '("11" "12" "13"))
        (expect (mapcar (lambda (i) (plist-get i :delete)) items) :to-equal '(nil t nil))
        (expect (plist-get (nth 1 items) :label) :to-equal "*TA* <2026-09-28 Mon 15:00>")
        (expect (plist-get (nth 0 items) :text) :to-equal "One line.")
        (expect (plist-get (nth 2 items) :text) :to-equal "First.\n\nSecond."))))

  (it "reads an item whose line ends the file"
    (with-temp-org-buffer
        "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comments\n- *Prof* [11] :: Last.  "
      (org-back-to-heading t)
      (expect (plist-get (car (org-canvas--submissions-sent-comments)) :text) :to-equal "Last.")))

  (it "reads a file from before #419 as holding nothing to send"
    (with-temp-org-buffer
        (concat "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comments\n"
                "- *Prof* <2026-01-01 Thu 00:00> :: Old comment\n")
      (expect (org-canvas--submissions-sent-comments) :to-be nil)
      (expect (org-canvas--submissions-collect-comment-edits) :to-be nil)))

  (it "collects an edited or marked comment, never an untouched or unknown one"
    (with-grading-file
        (test-sent--file '(("11" . "Same.") ("12" . "Before.") ("13" . "Kept."))
                         (concat "- *Prof* [11] :: Same.\n- *Prof* [12] :: After.\n"
                                 "- DELETE *Prof* [13] :: Kept.\n- *Prof* [14] :: No baseline.\n"))
      (let ((edits (org-canvas--submissions-collect-comment-edits)))
        (expect (mapcar (lambda (e) (plist-get e :id)) edits) :to-equal '("12" "13"))
        (expect (plist-get (car edits) :user-id) :to-equal 5001)
        (expect (plist-get (car edits) :name) :to-equal "Adams, Alice")
        (expect (plist-get (car edits) :baseline)
                :to-equal (org-canvas--submissions-comment-digest "Before.")))))

  (it "adds, replaces and drops a baseline entry"
    (with-temp-org-buffer "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"
      (org-back-to-heading t)
      (org-canvas--submissions-set-comment-digest "11" "aaa")
      (org-canvas--submissions-set-comment-digest "12" "bbb")
      (expect (org-entry-get (point) "CANVAS_COMMENTS") :to-equal "11=aaa 12=bbb")
      (org-canvas--submissions-set-comment-digest "11" "ccc")
      (expect (org-entry-get (point) "CANVAS_COMMENTS") :to-equal "11=ccc 12=bbb")
      (org-canvas--submissions-set-comment-digest "11" nil)
      (org-canvas--submissions-set-comment-digest "13" nil)
      (expect (org-entry-get (point) "CANVAS_COMMENTS") :to-equal "12=bbb")))

  (it "passes over a student who left the course"
    (with-grading-file
        (concat test-grading-file-header
                "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:STATUS: left\n:CANVAS_COMMENTS: 11=abc\n:END:\n\n"
                "** Comments\n- DELETE *Prof* [11] :: Bye.\n")
      (expect (org-canvas--submissions-collect-comment-edits) :to-be nil)
      (expect (length (org-canvas--submissions-comment-edits-by-student)) :to-equal 1))))

(describe "pushing sent comments (issue #419)"
  (it "rewrites the grader's own edited comment and records Canvas's text as the baseline"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              `(("comments/48213" . ,(test-sent--comment 48213 77 "Sharper now."))))
        (with-grading-file (test-sent--file '(("48213" . "Good."))
                                            "- *Prof* <2026-09-28 Mon 14:02> [48213] :: Sharper now.\n")
          (test-sent--with-canvas (list (test-sent--comment 48213 77 "Good."))
            (let ((prompt (test-sent--push)))
              (expect prompt :to-match "1 comment edit(s)"))
            (let ((call (test-org-canvas-find-api-call 'PUT "comments")))
              (expect (nth 1 call) :to-match "assignments/1001/submissions/5001/comments/48213\\'")
              (expect (nth 2 call) :to-equal '((comment . "Sharper now."))))
            (expect (buffer-string) :to-match "^- \\*Prof\\* <2026-09-28 Mon 14:02> \\[48213\\] :: Sharper now\\.$")
            (org-canvas--submissions-goto-user 5001)
            (expect (org-entry-get (point) "CANVAS_COMMENTS")
                    :to-equal (format "48213=%s" (org-canvas--submissions-comment-digest "Sharper now.")))
            (expect (org-canvas--submissions-collect-comment-edits) :to-be nil)
            (expect (buffer-modified-p) :to-be nil))))))

  (it "sends a paragraph break as typed"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("48213" . "Good."))
                                            "- *Prof* [48213] ::\n  First.\n\n  Second.\n")
          (test-sent--with-canvas (list (test-sent--comment 48213 77 "Good."))
            (test-sent--push)
            (expect (nth 2 (test-org-canvas-find-api-call 'PUT "comments"))
                    :to-equal '((comment . "First.\n\nSecond."))))))))

  (it "keeps the item's text when Canvas answers without one"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("48213" . "Good.")) "- *Prof* [48213] :: Better.\n")
          (test-sent--with-canvas (list (test-sent--comment 48213 77 "Good."))
            (test-sent--push)
            (expect (buffer-string) :to-match "^- \\*Prof\\* \\[48213\\] :: Better\\.$")
            (expect (org-canvas--submissions-collect-comment-edits) :to-be nil))))))

  (it "deletes a comment marked DELETE and takes its item and baseline away"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("11" . "Keep.") ("12" . "Drop."))
                                            "- *Prof* [11] :: Keep.\n- DELETE *Prof* [12] :: Drop.\n")
          (test-sent--with-canvas (list (test-sent--comment 11 77 "Keep.")
                                        (test-sent--comment 12 77 "Drop."))
            (expect (test-sent--push) :to-match "1 comment deletion(s)")
            (expect (nth 1 (test-org-canvas-find-api-call 'DELETE "comments"))
                    :to-match "submissions/5001/comments/12\\'")
            (expect (nth 2 (test-org-canvas-find-api-call 'DELETE "comments")) :to-be nil)
            (expect (buffer-string) :to-match "\\*\\* Comments\n- \\*Prof\\* \\[11\\] :: Keep\\.\n\n\\*\\* Notes")
            (org-canvas--submissions-goto-user 5001)
            (expect (org-entry-get (point) "CANVAS_COMMENTS")
                    :to-equal (format "11=%s" (org-canvas--submissions-comment-digest "Keep."))))))))

  (it "drops the baseline property with the last comment"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("12" . "Drop.")) "- DELETE *Prof* [12] :: Drop.\n")
          (test-sent--with-canvas (list (test-sent--comment 12 77 "Drop."))
            (test-sent--push)
            (org-canvas--submissions-goto-user 5001)
            (expect (org-entry-get (point) "CANVAS_COMMENTS") :to-be nil))))))

  (it "never deletes a comment whose line was removed"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("11" . "Keep.") ("12" . "Gone from the file."))
                                            "- *Prof* [11] :: Keep.\n")
          (test-sent--with-canvas (list (test-sent--comment 11 77 "Keep.")
                                        (test-sent--comment 12 77 "Gone from the file."))
            (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) (error "must not ask"))))
              (org-canvas-submissions-push-grades))
            (expect (test-org-canvas-api-call-count) :to-equal 0))))))

  (it "refuses, names and never sends a change it may not make"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file
            (test-sent--file '(("21" . "Theirs.") ("22" . "Mine.") ("23" . "Vanished.") ("24" . "Was."))
                             (concat "- *TA* [21] :: Theirs, rewritten.\n"
                                     "- *Prof* [22] :: Mine, rewritten.\n"
                                     "- DELETE *Prof* [23] :: Vanished.\n"
                                     "- *Prof* [24] ::\n"))
          (test-sent--with-canvas (list (test-sent--comment 21 90 "Theirs.")
                                        (test-sent--comment 22 77 "Mine, changed in SpeedGrader.")
                                        (test-sent--comment 24 77 "Was."))
            (let* ((warnings (test-sent--collecting-warnings
                              (cl-letf (((symbol-function 'org-canvas--confirm)
                                         (lambda (_) (error "must not ask"))))
                                (org-canvas-submissions-push-grades)))))
              (expect warnings
                      :to-equal '("[Submissions] Comment 21 on Adams, Alice not changed: not yours; only its author may change it"
                                  "[Submissions] Comment 22 on Adams, Alice not changed: edited on Canvas since the pull"
                                  "[Submissions] Comment 23 on Adams, Alice not changed: no longer on Canvas"
                                  "[Submissions] Comment 24 on Adams, Alice not changed: emptied; mark it DELETE to delete it")))
            (expect (test-org-canvas-api-call-count) :to-equal 0)
            (expect (buffer-string) :to-match "\\[21\\] :: Theirs, rewritten\\.")
            (expect (buffer-string) :to-match "DELETE \\*Prof\\* \\[23\\]"))))))

  (it "lists what it sends and what it leaves in the confirmation"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("21" . "Theirs.") ("22" . "Mine.") ("23" . "Drop."))
                                            (concat "- *TA* [21] :: Theirs, rewritten.\n"
                                                    "- *Prof* [22] :: Mine, rewritten.\n"
                                                    "- DELETE *Prof* [23] :: Drop.\n"))
          (test-sent--with-canvas (list (test-sent--comment 21 90 "Theirs.")
                                        (test-sent--comment 22 77 "Mine.")
                                        (test-sent--comment 23 77 "Drop."))
            (let ((shown nil) (prompt nil))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args) (push (apply #'format fmt args) shown)))
                        ((symbol-function 'org-canvas--log-warning) #'ignore))
                (setq prompt (test-sent--push)))
              (expect prompt :to-equal
                      "Push 1 comment edit(s) and 1 comment deletion(s), leaving 1 comment change(s) unsent? ")
              (expect (seq-find (lambda (m) (string-prefix-p "Sent comments:" m)) shown)
                      :to-equal
                      (concat "Sent comments:\n"
                              "  Adams, Alice: edit comment 22\n"
                              "  Adams, Alice: delete comment 23\n"
                              "  Adams, Alice: not sending comment 21 (not yours; only its author may change it)"))
              (expect (seq-find (lambda (m) (string-match-p "sent comment(s) edited" m)) shown)
                      :to-match "; 1 sent comment(s) edited, 1 deleted; 1 comment change(s) not sent (see the log)")))))))

  (it "sends nothing when the comments cannot be read"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
          (cl-letf (((symbol-function 'org-canvas--submissions-self-id)
                     (lambda () (signal 'org-canvas-api-error '("HTTP 500")))))
            (let ((warnings (test-sent--collecting-warnings
                             (cl-letf (((symbol-function 'org-canvas--confirm)
                                        (lambda (_) (error "must not ask"))))
                               (org-canvas-submissions-push-grades)))))
              (expect (car warnings) :to-match "Could not read the sent comments")
              (expect (cadr warnings) :to-match "not changed: Canvas could not be read")))
          (expect (test-org-canvas-api-call-count) :to-equal 0)))))

  (it "sends nothing and changes nothing under a dry run"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                            "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine.")
                                        (test-sent--comment 23 77 "Drop."))
            (let ((org-canvas--dry-run t)
                  (before (buffer-string)))
              (expect (plist-get (org-canvas--submissions-push-current nil) :edited) :to-equal 0)
              (expect (test-org-canvas-api-call-count) :to-equal 0)
              (expect (buffer-string) :to-equal before)
              (expect (length (org-canvas--submissions-collect-comment-edits)) :to-equal 2)))))))

  (it "leaves an item pending when Canvas refuses the request"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (lambda (&rest _) (signal 'org-canvas-api-error '("HTTP 403 Forbidden")))))
            (let ((warnings (test-sent--collecting-warnings (test-sent--push))))
              (expect (length warnings) :to-equal 1)
              (expect (car warnings) :to-match
                      "\\`\\[Submissions\\] Could not edit comment 22 on Adams, Alice: .*HTTP 403 Forbidden")))
          (expect (length (org-canvas--submissions-collect-comment-edits)) :to-equal 1)))))

  (it "reports the counts to a script"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                            "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine.")
                                        (test-sent--comment 23 77 "Drop."))
            (let ((result (org-canvas-push-submission-grades 1001)))
              (expect (plist-get result :edited) :to-equal 1)
              (expect (plist-get result :deleted) :to-equal 1))))))))

;;;; Pushing Only the Sent Comments (issue #425)

(defun test-sent-only--everything-file ()
  "Return a grading file where a sent comment, scores, rows, drafts and the bank differ."
  (concat test-rubric-file-header
          "* Comment Bank\n- Show your units.\n\n"
          (test-rubric-entry "Adams, Alice" 5001
                             (format ":SCORE: 4\n:CANVAS_SCORE: 3\n:SUBMISSION_ID: 9001\n:LATE_STATUS: late\n:CANVAS_COMMENTS: 22=%s\n"
                                     (org-canvas--submissions-comment-digest "Mine."))
                             '(("_7104" "Thesis" 2 2 "Sharp") ("_7105" "Evidence" 3 2 nil)
                               ("_7106" "Style" 1 nil nil)))
          "** Comments\n- *Prof* [22] :: Mine, rewritten.\n\n"
          "** Comment to post\nSee me.\n"))

(defun test-sent-only--push (&optional assignment)
  "Push only the sent comments of ASSIGNMENT, confirming; return (RESULT . PROMPT)."
  (let ((prompt nil))
    (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t)))
      (cons (org-canvas-push-submission-comment-edits assignment) prompt))))

(describe "pushing only the sent comments (issue #425)"
  (it "sends the comment edit and nothing else the file holds"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent-only--everything-file)
          (org-canvas--submissions-ensure-context)
          (expect (org-canvas--submissions-collect-grade-changes) :not :to-be nil)
          (expect (org-canvas--submissions-collect-comment-drafts) :not :to-be nil)
          (expect (org-canvas--submissions-bank-pending) :not :to-be nil)
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
            (let* ((mutations nil)
                   (pushed (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                                      (lambda (&rest args) (push args mutations) nil)))
                             (test-sent-only--push))))
              (expect (cdr pushed) :to-equal "Push 1 comment edit(s)? ")
              (expect (car pushed) :to-equal
                      '(:edited 1 :deleted 0 :refused 0 :failed 0 :dry-run 0))
              (expect mutations :to-be nil))
            (expect (mapcar (lambda (c) (list (car c) (cadr c))) test-org-canvas-api-calls)
                    :to-equal
                    (list (list 'PUT (org-canvas-api-course-endpoint
                                      "assignments/%s/submissions/%s/comments/%s" 1001 5001 "22"))))
            (org-canvas--submissions-goto-user 5001)
            (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "3")
            (expect (org-entry-get (point) "SCORE") :to-equal "4")
            (expect (buffer-string) :to-match "^\\*\\* Comment to post\nSee me\\.$")
            (expect (buffer-string) :to-match "^- Show your units\\.$")
            (expect (org-canvas--submissions-collect-comment-edits) :to-be nil)
            (expect (org-canvas--submissions-collect-comment-drafts) :not :to-be nil))))))

  (it "refuses what the full push refuses and counts it"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("21" . "Theirs.") ("22" . "Mine."))
                                            (concat "- *TA* [21] :: Theirs, rewritten.\n"
                                                    "- DELETE *Prof* [22] :: Mine.\n"))
          (test-sent--with-canvas (list (test-sent--comment 21 90 "Theirs.")
                                        (test-sent--comment 22 77 "Mine."))
            (let* ((pushed nil)
                   (warnings (test-sent--collecting-warnings
                              (setq pushed (test-sent-only--push)))))
              (expect warnings :to-equal
                      '("[Submissions] Comment 21 on Adams, Alice not changed: not yours; only its author may change it"))
              (expect (cdr pushed) :to-equal
                      "Push 1 comment deletion(s), leaving 1 comment change(s) unsent? ")
              (expect (car pushed) :to-equal
                      '(:edited 0 :deleted 1 :refused 1 :failed 0 :dry-run 0)))
            (expect (test-org-canvas-api-call-count) :to-equal 1)
            (expect (buffer-string) :to-match "\\[21\\] :: Theirs, rewritten\\."))))))

  (it "sends nothing and changes nothing under a dry run, even on a read-only course"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                            "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine.")
                                        (test-sent--comment 23 77 "Drop."))
            (let ((org-canvas--dry-run t)
                  (org-canvas-read-only t)
                  (before (buffer-string)))
              (expect (car (test-sent-only--push)) :to-equal
                      '(:edited 0 :deleted 0 :refused 0 :failed 0 :dry-run 2))
              (expect (test-org-canvas-api-call-count) :to-equal 0)
              (expect (buffer-string) :to-equal before)
              (expect (length (org-canvas--submissions-collect-comment-edits)) :to-equal 2)))))))

  (it "refuses a read-only course before anything is sent"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
            (let ((org-canvas-read-only t))
              (expect (test-sent-only--push) :to-throw 'org-canvas-read-only-error))
            (expect (test-org-canvas-api-call-count) :to-equal 0))))))

  (it "sends nothing when the question is declined"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
            (let ((noninteractive nil)
                  (org-canvas-assume-yes nil)
                  (asked nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq asked p) nil)))
                (expect (org-canvas-push-submission-comment-edits) :to-be nil))
              (expect asked :to-equal "Push 1 comment edit(s)? "))
            (expect (test-org-canvas-api-call-count) :to-equal 0))))))

  (it "says when there is nothing to send"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine.\n")
          (let ((shown nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) shown)))
                      ((symbol-function 'org-canvas--confirm) (lambda (_) (error "must not ask"))))
              (expect (org-canvas-push-submission-comment-edits) :to-equal
                      '(:edited 0 :deleted 0 :refused 0 :failed 0 :dry-run 0)))
            (expect (car shown) :to-equal "No sent comment to change")
            (expect (test-org-canvas-api-call-count) :to-equal 0))))))

  (it "saves the new baseline through the package's save path"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              `(("comments/22" . ,(test-sent--comment 22 77 "Mine, rewritten."))))
        (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
            (let ((saves 0)
                  (save (symbol-function 'org-canvas--save-buffer)))
              (cl-letf (((symbol-function 'org-canvas--save-buffer)
                         (lambda () (cl-incf saves) (funcall save))))
                (test-sent-only--push))
              (expect saves :to-equal 1))
            (expect (buffer-modified-p) :to-be nil)
            (expect (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))
                    :to-match (format ":CANVAS_COMMENTS: 22=%s"
                                      (org-canvas--submissions-comment-digest
                                       "Mine, rewritten."))))))))

  (it "pushes a grading file named from a script, without a prompt"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                            "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
          (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine.")
                                        (test-sent--comment 23 77 "Drop."))
            (let ((noninteractive t)
                  (org-canvas-assume-yes nil)
                  (grading (current-buffer)))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "must not ask")))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _) (error "must not prompt"))))
                (with-temp-buffer
                  (expect (org-canvas-push-submission-comment-edits "HW") :to-equal
                          '(:edited 1 :deleted 1 :refused 0 :failed 0 :dry-run 0))))
              (expect (buffer-local-value 'org-canvas-submissions--current-view grading)
                      :to-equal 'detail)
              (expect (test-org-canvas-api-call-count) :to-equal 2)
              (expect (buffer-modified-p grading) :to-be nil)))))))

  (it "finds a grading file by its assignment id"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("23" . "Drop.")) "- DELETE *Prof* [23] :: Drop.\n")
          (test-sent--with-canvas (list (test-sent--comment 23 77 "Drop."))
            (with-temp-buffer
              (expect (plist-get (car (test-sent-only--push 1001)) :deleted) :to-equal 1)))))))

  (it "reports the refused count from the full push as well"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (test-sent--file '(("21" . "Theirs.")) "- *TA* [21] :: Theirs, rewritten.\n")
          (test-sent--with-canvas (list (test-sent--comment 21 90 "Theirs."))
            (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
              (expect (plist-get (org-canvas-push-submission-grades 1001) :refused)
                      :to-equal 1)))))))

  (it "refuses a buffer that names no assignment"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"
          (expect (org-canvas-push-submission-comment-edits) :to-throw 'user-error)
          (expect (test-org-canvas-api-call-count) :to-equal 0)))))

  (it "is bound to C in a grading file and in the dispatch menu"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "C"))
            :to-equal #'org-canvas-push-submission-comment-edits)
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-push-submission-comment-edits)
            :to-be-truthy)))

(describe "refreshing a file with sent comments changed (issue #419)"
  (it "keeps an unsent edit and a DELETE mark over the re-render"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                          "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine.")
                                      (test-sent--comment 23 77 "Drop."))
          (org-canvas-submissions-refresh)
          (expect (buffer-string) :to-match "^- \\*Prof\\* <[^>]+> \\[22\\] :: Mine, rewritten\\.$")
          (expect (buffer-string) :to-match "^- DELETE \\*Prof\\* <[^>]+> \\[23\\] :: Drop\\.$")
          (let ((edits (org-canvas--submissions-collect-comment-edits)))
            (expect (mapcar (lambda (e) (plist-get e :id)) edits) :to-equal '("22" "23"))
            (expect (plist-get (car edits) :baseline)
                    :to-equal (org-canvas--submissions-comment-digest "Mine.")))))))

  (it "gives ids to a file pulled before them"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file nil "- *Prof* <2026-01-01 Thu 00:00> :: Mine.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
          (org-canvas-submissions-refresh)
          (expect (buffer-string) :to-match "\\[22\\] :: Mine\\.$")
          (expect (org-canvas--submissions-collect-comment-edits) :to-be nil)))))

  (it "writes nothing over a comment Canvas now holds as edited"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine, rewritten."))
          (org-canvas-submissions-refresh)
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_COMMENTS")
                  :to-equal (format "22=%s" (org-canvas--submissions-comment-digest "Mine, rewritten.")))
          (expect (org-canvas--submissions-collect-comment-edits) :to-be nil)))))

  (it "keeps an edit against its old baseline when Canvas moved, and says so"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Changed in SpeedGrader."))
          (let ((warnings (test-sent--collecting-warnings (org-canvas-submissions-refresh))))
            (expect warnings :to-contain
                    "[Refresh] Comment 22 on Adams, Alice was edited on Canvas since; the change made here is kept and a push will not send it"))
          (expect (buffer-string) :to-match "\\[22\\] :: Mine, rewritten\\.$")
          (expect (plist-get (car (org-canvas--submissions-collect-comment-edits)) :baseline)
                  :to-equal (org-canvas--submissions-comment-digest "Mine."))))))

  (it "keeps a DELETE mark on a comment the old baseline did not name"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file nil "- DELETE *Prof* [23] :: Drop.\n")
        (test-sent--with-canvas (list (test-sent--comment 23 77 "Drop."))
          (org-canvas-submissions-refresh)
          (expect (buffer-string) :to-match "^- DELETE \\*Prof\\* <[^>]+> \\[23\\] :: Drop\\.$")
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_COMMENTS")
                  :to-equal (format "23=%s" (org-canvas--submissions-comment-digest "Drop.")))))))

  (it "names a changed comment Canvas no longer holds, with its text"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                          "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
        (test-sent--with-canvas nil
          (let ((warnings (test-sent--collecting-warnings (org-canvas-submissions-refresh))))
            (expect warnings :to-contain
                    "[Refresh] Comment 22 on Adams, Alice is no longer on Canvas; the change made here was dropped: Mine, rewritten.")
            (expect warnings :to-contain
                    "[Refresh] Comment 23 on Adams, Alice is no longer on Canvas; the change made here was dropped: DELETE")))))))

(describe "recording a posted draft after multi-line comments (issue #419)"
  (it "goes after the last item's paragraphs, never inside them"
    (with-temp-org-buffer
        (concat "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comments\n"
                "- *Prof* [13] ::\n  First.\n\n  Second.\n\n** Notes\n")
      (org-back-to-heading t)
      (org-canvas--submissions-append-comment-to-buffer "Adams, Alice" "New.")
      (expect (buffer-string) :to-match
              "  Second\\.\n- \\*You\\* <[^>]+> :: New\\.\n\n\\*\\* Notes")
      (org-back-to-heading t)
      (expect (plist-get (car (org-canvas--submissions-sent-comments)) :text)
              :to-equal "First.\n\nSecond.")))

  (it "fills an empty Comments section"
    (with-temp-org-buffer
        "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Comments\n** Notes\n"
      (org-back-to-heading t)
      (org-canvas--submissions-append-comment-to-buffer "Adams, Alice" "New.")
      (expect (buffer-string) :to-match "\\*\\* Comments\n- \\*You\\* <[^>]+> :: New\\.\n\\*\\* Notes"))))

(provide 'org-canvas-submissions-test)
;;; org-canvas-submissions-test.el ends here
