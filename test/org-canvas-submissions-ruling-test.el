;;; org-canvas-submissions-ruling-test.el --- Tests for applying a grading ruling -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-apply-ruling' and its JSON and
;; command-line forms (issue #448).  Everything is local: no spec
;; reaches Canvas.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-ruling)
(require 'org-canvas-batch)

(defun test-rul--student (name uid props &rest sections)
  "Return a grading-file heading NAME for UID with PROPS and SECTIONS text."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n" name uid)
          (mapconcat (lambda (p) (format ":%s: %s\n" (car p) (cdr p))) props "")
          ":END:\n" (apply #'concat sections)))

(defun test-rul--rubric (score1 comment1 score2 comment2)
  "Return a Rubric section scoring _1 SCORE1 and _2 SCORE2, with COMMENT1, COMMENT2."
  (concat "\n** Rubric\n| Id | Criterion | Max | Score |\n|----+-----------+-----+-------|\n"
          (format "| _1 | Thesis    | 5   | %-5s |\n| _2 | Structure | 5   | %-5s |\n"
                  score1 score2)
          (org-canvas--submissions-item "_1" comment1) "\n"
          (org-canvas--submissions-item "_2" comment2) "\n"))

(defun test-rul--file (&optional header)
  "Return the grading file the specs read, HEADER lines added to its header."
  (concat
   "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
   "#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n" (or header "") "\n"
   (test-rul--student
    "Adams, Alice" 5001 '(("STATUS" . "graded") ("SCORE" . "8"))
    (test-rul--rubric "3" "Thesis unclear." "5" nil)
    "\n** Notes\n# Your notes.\nLate by a day.\n"
    "\n** Comment to post\n# Write a comment.\nDraft for Alice.\n\n")
   (test-rul--student
    "Baker, Bob" 5002 '(("STATUS" . "submitted"))
    (test-rul--rubric "4" nil "5" "Good order.")
    "\n** Notes\n# Your notes.\n")
   (test-rul--student "Chen, Cara" 5003 '(("STATUS" . "submitted")))
   (test-rul--student
    "Davis, Dan" 5004 '(("STATUS" . "left"))
    (test-rul--rubric "0" nil "" nil))))

(defmacro with-rul-file (header &rest body)
  "Run BODY in a scratch grading file holding `test-rul--file' with HEADER.
`dir' is the submissions directory and `file' the grading file."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-canvas-rul-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (org-canvas--dry-run nil)
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert (test-rul--file ,header)))
           (setq buf (org-canvas--submissions-visit-grading-file file))
           (with-current-buffer buf
             (cl-letf (((symbol-function 'org-canvas--log-info) #'ignore)
                       ((symbol-function 'org-canvas--log-warning) #'ignore)
                       ((symbol-function 'message) #'ignore)
                       ((symbol-function 'org-canvas--user-message) #'ignore))
               ,@body)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

(defun test-rul--at (uid fn)
  "Return FN's value at the heading of USER_ID UID."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^:USER_ID: %s$" uid))
    (org-back-to-heading t)
    (funcall fn)))

(defun test-rul--rows (uid)
  "Return the Rubric rows of UID as (ID SCORE COMMENT) lists."
  (test-rul--at uid (lambda ()
                      (mapcar (lambda (r) (list (nth 0 r) (nth 3 r) (nth 4 r)))
                              (org-canvas--submissions-rubric-rows)))))

(defun test-rul--prop (uid name)
  "Return property NAME of UID's heading."
  (test-rul--at uid (lambda () (org-entry-get (point) name))))

(defun test-rul--section (uid heading)
  "Return the text under HEADING in UID's entry."
  (test-rul--at uid (lambda () (org-canvas--submissions-section-text heading))))

(describe "org-canvas-submissions-apply-ruling (issue #448)"
  (it "zeroes the rows with one comment, sets SCORE, and keeps the old in Notes"
    (with-rul-file nil
      (let* ((result (org-canvas-submissions-apply-ruling
                      nil '(5001 "5002")
                      :rows 0 :row-comment "No link, so it scores zero."
                      :score 0 :note "Open-link rule."))
             (alice (car (plist-get result :changed)))
             (notes (test-rul--section "5001" org-canvas--submissions-notes-heading)))
        (expect (mapcar (lambda (p) (plist-get p :user-id)) (plist-get result :changed))
                :to-equal '("5001" "5002"))
        (expect (plist-get result :unmatched) :to-be nil)
        (expect (plist-get result :skipped) :to-be nil)
        (expect (plist-get result :dry-run) :to-be nil)
        (expect (plist-get alice :old-score) :to-equal "8")
        (expect (plist-get alice :score) :to-equal "0")
        (expect (plist-get alice :old-rows)
                :to-equal '(("_1" "3" "Thesis unclear.") ("_2" "5" nil)))
        (expect (test-rul--rows "5001")
                :to-equal '(("_1" "0" "No link, so it scores zero.")
                            ("_2" "0" "No link, so it scores zero.")))
        (expect (test-rul--prop "5001" "SCORE") :to-equal "0")
        (expect (test-rul--prop "5002" "SCORE") :to-equal "0")
        (expect notes :to-match "\\`Late by a day\\.\n- Ruling <[^>]+>: Open-link rule\\.\n")
        (expect notes :to-match "Before: SCORE 8; Rubric _1 3 (Thesis unclear\\.), _2 5$")
        (expect (test-rul--section "5001" org-canvas--submissions-draft-heading)
                :to-equal "Draft for Alice.")
        (expect (buffer-modified-p) :to-be nil)
        (with-temp-buffer
          (insert-file-contents file)
          (expect (buffer-string) :to-match "^| _1 | Thesis +| +5 +| +0 +|$")))))

  (it "derives SCORE from a function of the total and the column's points"
    (with-rul-file "#+PROPERTY: POINTS_POSSIBLE 20\n"
      (let* ((result (org-canvas-submissions-apply-ruling
                      nil '(5002) :row-comment 'empty
                      :score (lambda (total max) (- total (* 0.2 max)))
                      :comment "Late, so 20% of the column comes off."))
             (bob (car (plist-get result :changed))))
        (expect (plist-get bob :score) :to-equal "5")
        (expect (test-rul--rows "5002") :to-equal '(("_1" "4" nil) ("_2" "5" nil)))
        (expect (test-rul--section "5002" org-canvas--submissions-draft-heading)
                :to-equal "Late, so 20% of the column comes off.")
        (expect (test-rul--section "5002" org-canvas--submissions-notes-heading)
                :to-match "\\`- Ruling <[^>]+>\n  Before: Rubric _1 4, _2 5 (Good order\\.)\\'"))))

  (it "sums the rows' Max without POINTS_POSSIBLE, and never goes below 0"
    (with-rul-file nil
      (let ((seen nil))
        (org-canvas-submissions-apply-ruling
         nil '(5001) :rows 'empty
         :score (lambda (total max) (setq seen (list total max)) -3))
        (expect seen :to-equal '(0 10))
        (expect (test-rul--prop "5001" "SCORE") :to-equal "0")
        (expect (test-rul--rows "5001")
                :to-equal '(("_1" nil "Thesis unclear.") ("_2" nil nil))))))

  (it "records a replaced draft, and adds the Comment to post heading when missing"
    (with-rul-file nil
      (org-canvas-submissions-apply-ruling nil '(5001 5003) :comment "New draft.\nLine two."
                                           :note "Two\nlines")
      (expect (test-rul--section "5001" org-canvas--submissions-draft-heading)
              :to-equal "New draft.\nLine two.")
      (expect (test-rul--section "5001" org-canvas--submissions-notes-heading)
              :to-match ": Two\n  lines\n  Before: SCORE 8; .*; draft (Draft for Alice\\.)$")
      (expect (test-rul--section "5003" org-canvas--submissions-draft-heading)
              :to-equal "New draft.\nLine two.")
      (expect (test-rul--section "5003" org-canvas--submissions-notes-heading)
              :to-match "Before: nothing typed$")))

  (it "names the ids that match nothing and keeps back what it cannot rule on"
    (with-rul-file nil
      (let ((result (org-canvas-submissions-apply-ruling
                     nil '(9999 5003 5004 5004) :rows 0)))
        (expect (plist-get result :changed) :to-be nil)
        (expect (plist-get result :unmatched) :to-equal '("9999"))
        (expect (plist-get result :skipped)
                :to-equal '(("5003" . "no Rubric table to rule on")
                            ("5004" . "left the course")))
        (expect (test-rul--prop "5003" "SCORE") :to-be nil))))

  (it "keeps back a SCORE the rubric total contradicts when the rubric grades"
    (with-rul-file "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING true\n"
      (let ((result (org-canvas-submissions-apply-ruling nil '(5001) :rows 1 :score 0)))
        (expect (cdar (plist-get result :skipped)) :to-match "disagrees with the rubric total 2")
        (expect (test-rul--prop "5001" "SCORE") :to-equal "8"))
      (expect (plist-get (org-canvas-submissions-apply-ruling nil '(5001) :rows 0 :score 0)
                         :changed)
              :not :to-be nil)))

  (it "writes nothing under a dry run and reports the same"
    (with-rul-file nil
      (let* ((before (buffer-string))
             (org-canvas--dry-run t)
             (result (org-canvas-submissions-apply-ruling nil '(5001) :rows 0 :score 0)))
        (expect (buffer-string) :to-equal before)
        (expect (plist-get result :dry-run) :to-be t)
        (expect (plist-get (car (plist-get result :changed)) :score) :to-equal "0"))))

  (it "describes each change for the echo area"
    (expect (org-canvas--submissions-ruling-describe
             '(:user-id "1" :name "A" :old-score nil :score "0"
               :old-rows (("_1" "3" nil)) :rows (("_1" "0" nil)) :draft "x"))
            :to-equal "user 1 (A): SCORE - -> 0, 1 row(s), draft")
    (expect (org-canvas--submissions-ruling-describe
             '(:user-id "1" :name "A" :old-rows nil :rows nil))
            :to-equal "user 1 (A): SCORE left")
    (let ((lines nil))
      (cl-letf (((symbol-function 'org-canvas--user-message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) lines))))
        (org-canvas--submissions-ruling-report
         '(:changed ((:user-id "1" :name "A" :score "0")) :unmatched ("2")
           :skipped (("3" . "left the course")) :dry-run nil)))
      (expect (car lines)
              :to-equal "Ruling: 1 student(s) changed, 1 matched nothing, 1 kept back; press S to push")
      (expect lines :to-contain "Ruling: user 2 matched no student")
      (expect lines :to-contain "Ruling: kept back user 3: left the course")))

  (it "refuses a ruling it cannot apply before reading any file"
    (dolist (args '((:rows "zero") (:rows -1) (:row-comment 3) (:score "0")
                    (:comment 4) (:comment "Fine.\n# hidden")))
      (expect (apply #'org-canvas-submissions-apply-ruling "HW" '(1) args)
              :to-throw 'user-error))))

;;;; JSON and the command line

(defun test-rul--batch (args)
  "Return the exit status of the batch command line ARGS."
  (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
            ((symbol-function 'org-canvas--report-display) #'ignore))
    (let ((status nil))
      (with-output-to-string (setq status (org-canvas-batch-main args)))
      status)))

(describe "the JSON rulings and apply-ruling on the command line (issue #448)"
  (it "applies each ruling of a file, a percentage deduction among them"
    (with-rul-file "#+PROPERTY: POINTS_POSSIBLE 10\n"
      (let ((in (expand-file-name "r.json" dir)))
        (with-temp-file in
          (insert "[{\"user_ids\": [5001], \"rows\": 0, \"row_comment\": \"No link.\","
                  " \"score\": 0, \"note\": \"Rule.\"},"
                  " {\"user_ids\": [\"5002\"], \"rows\": \"empty\","
                  " \"score\": {\"deduct_percent\": 20}}]"))
        (let ((results (org-canvas-submissions-apply-ruling-json "HW" in)))
          (expect (length results) :to-equal 2)
          (expect (test-rul--rows "5001")
                  :to-equal '(("_1" "0" "No link.") ("_2" "0" "No link.")))
          (expect (test-rul--prop "5002" "SCORE") :to-equal "0")
          (expect (test-rul--rows "5002") :to-equal '(("_1" nil nil) ("_2" nil "Good order."))))
        (with-temp-file in (insert "{\"user_ids\": [5001], \"score\": {\"deduct_percent\": \"x\"}}"))
        (expect (org-canvas-submissions-apply-ruling-json "HW" in) :to-throw 'user-error))))

  (it "runs from the command line, a dry run on -n, exiting 1 on a miss"
    (with-rul-file nil
      (let ((in (expand-file-name "r.json" dir))
            (before (buffer-string)))
        (with-temp-file in (insert "{\"user_ids\": [5001], \"score\": 0}"))
        (expect (test-rul--batch (list "-n" "apply-ruling" "HW" in)) :to-equal 0)
        (expect (buffer-string) :to-equal before)
        (expect (test-rul--batch (list "apply-ruling" "HW" in)) :to-equal 0)
        (expect (test-rul--prop "5001" "SCORE") :to-equal "0")
        (with-temp-file in (insert "{\"user_ids\": [5001, 42], \"score\": 1}"))
        (expect (test-rul--batch (list "apply-ruling" "HW" in)) :to-equal 1)
        (expect (test-rul--batch (list "apply-ruling" "HW")) :to-equal 2)))))

(provide 'org-canvas-submissions-ruling-test)
;;; org-canvas-submissions-ruling-test.el ends here
