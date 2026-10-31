;;; org-canvas-submissions-comment-items-test.el --- Tests for org-canvas-submissions-comment-items -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for the comment bank and sent comments.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-comment-items)

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

(defun test-sent--push ()
  "Push the grading buffer at hand, confirming; return the prompt asked."
  (let ((prompt nil))
    (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t))
              ((symbol-function 'org-canvas--submissions-offer-to-post) #'ignore))
      (org-canvas-submissions-push-grades))
    prompt))

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

  (it "counts an error in Emacs apart from a Canvas refusal (#443)"
    (with-org-canvas-test-config
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
          (let ((said nil) (result nil))
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (&rest _)
                         (signal 'file-error
                                 '("org-canvas.log" "Cannot resolve lock conflict in batch mode"))))
                      ((symbol-function 'org-canvas--user-message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
              (let ((warnings (test-sent--collecting-warnings
                               (setq result (org-canvas-push-submission-comment-edits)))))
                (expect (car warnings) :to-match "an error in Emacs, not from Canvas")))
            (expect (plist-get result :failed) :to-equal 0)
            (expect (plist-get result :errored) :to-equal 1)
            (expect (car said) :to-match "1 failed in Emacs, not at Canvas")
            (expect (car said) :not :to-match "refused\\|failed at Canvas"))
          (expect (length (org-canvas--submissions-collect-comment-edits)) :to-equal 1)))))

  (it "words a Canvas failure as one in the closing note (#443)"
    (expect (org-canvas--submissions-comment-edits-note
             '(:edited 0 :deleted 0 :dry-run 0 :failed 2 :errored 0) nil)
            :to-equal "; 0 sent comment(s) edited, 0 deleted, 2 failed at Canvas (see the log)"))

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

;;;; A Dry Run of the Grade Push Changes Nothing (issue #442)

(provide 'org-canvas-submissions-comment-items-test)
;;; org-canvas-submissions-comment-items-test.el ends here
