;;; org-canvas-submissions-comments-test.el --- Tests for comment export, import and check -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-export-comments',
;; `org-canvas-submissions-import-comments' and
;; `org-canvas-submissions-check-comments' (issue #438).  Everything is
;; local: no spec reaches Canvas.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-comments)
(require 'org-canvas-batch)

(defun test-scom--student (name uid props &rest sections)
  "Return a grading-file heading NAME for UID with PROPS and SECTIONS text."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n" name uid)
          (mapconcat (lambda (p) (format ":%s: %s\n" (car p) (cdr p))) props "")
          ":END:\n" (apply #'concat sections)))

(defun test-scom--rubric (score1 comment1 score2 comment2)
  "Return a Rubric section scoring _1 SCORE1 and _2 SCORE2, with COMMENT1, COMMENT2."
  (concat "\n** Rubric\n| Id | Criterion | Max | Score |\n|----+-----------+-----+-------|\n"
          (format "| _1 | Thesis    | 5   | %-5s |\n| _2 | Structure | 5   | %-5s |\n"
                  score1 score2)
          (org-canvas--submissions-item "_1" comment1) "\n"
          (org-canvas--submissions-item "_2" comment2) "\n"))

(defun test-scom--draft (&optional text)
  "Return a Comment to post section holding TEXT under its template."
  (concat "\n** Comment to post\n# Write a comment.\n" (if text (concat text "\n") "") "\n"))

(defun test-scom--file ()
  "Return the grading file the specs read: four students and two blocks."
  (concat
   "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
   "#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n\n"
   "* Rubric\nThe criteria.\n\n"
   (test-scom--student
    "Adams, Alice" 5001
    `(("STATUS" . "graded") ("SCORE" . "8")
      ("CANVAS_COMMENTS" . ,(format "11=%s 12=%s"
                                    (org-canvas--submissions-comment-digest "One line.")
                                    (org-canvas--submissions-comment-digest
                                     "First.\n\nSecond."))))
    "\n** Comments\n- *Prof* <2026-09-28 Mon 14:02> [11] :: One line.\n"
    "- *TA* <2026-09-28 Mon 15:00> [12] ::\n  First.\n\n  Second.\n"
    (test-scom--rubric "3" "Thesis unclear. The claim needs a reason given early." "5" nil)
    "\n** Notes\n# Your notes.\nLate by a day.\n"
    (test-scom--draft "Draft for Alice."))
   (test-scom--student
    "Baker, Bob" 5002 '(("STATUS" . "submitted"))
    (test-scom--rubric "2" nil "5" "The claim needs a reason given early. Good order.")
    (test-scom--draft))
   (test-scom--student
    "Chen, Cara" 5003 '(("STATUS" . "graded") ("POSTED_AT" . "<2026-09-29 Tue 10:00>"))
    (test-scom--rubric "1" nil "5" "The claim needs a reason given early.")
    (test-scom--draft))
   (test-scom--student
    "Davis, Dan" 5004 '(("STATUS" . "left"))
    (test-scom--rubric "0" nil "" "The claim needs a reason given early.")
    (test-scom--draft))
   "* Comment Bank\n"))

(defmacro with-scom-file (&rest body)
  "Run BODY in a scratch grading file holding `test-scom--file'.
`dir' is the submissions directory and `file' the grading file."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "org-canvas-scom-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (org-canvas-submissions-comment-flag-regexps nil)
          (org-canvas-submissions-comment-shared-threshold 3)
          (org-canvas-submissions-comment-sentence-min-length 25)
          (org-canvas--dry-run nil)
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert (test-scom--file)))
           (setq buf (org-canvas--submissions-visit-grading-file file))
           (with-current-buffer buf
             (cl-letf (((symbol-function 'org-canvas--log-info) #'ignore)
                       ((symbol-function 'org-canvas--log-warning) #'ignore))
               ,@body)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

(defmacro test-scom--quietly (&rest body)
  "Run BODY with `message' and `org-canvas--user-message' silenced."
  `(cl-letf (((symbol-function 'message) #'ignore)
             ((symbol-function 'org-canvas--user-message) #'ignore))
     ,@body))

(defun test-scom--student-of (students uid)
  "Return the exported student UID among STUDENTS."
  (seq-find (lambda (s) (equal (plist-get s :user-id) uid)) students))

(defun test-scom--set (plist key value)
  "Return a copy of PLIST with KEY set to VALUE."
  (plist-put (copy-sequence plist) key value))

;;;; Export

(describe "org-canvas-submissions-export-comments (issue #438)"
  (it "returns every student's rows, draft, sent comments and notes"
    (with-scom-file
      (let* ((students (org-canvas-submissions-export-comments))
             (alice (test-scom--student-of students "5001"))
             (row (car (plist-get alice :rows)))
             (sent (plist-get alice :sent)))
        (expect (mapcar (lambda (s) (plist-get s :user-id)) students)
                :to-equal '("5001" "5002" "5003" "5004"))
        (expect (plist-get alice :name) :to-equal "Adams, Alice")
        (expect (plist-get alice :status) :to-equal "graded")
        (expect (plist-get alice :score) :to-equal "8")
        (expect (plist-get alice :posted) :to-be nil)
        (expect row :to-equal '(:criterion-id "_1" :criterion "Thesis" :max "5" :score "3"
                                :comment "Thesis unclear. The claim needs a reason given early."))
        (expect (plist-get (nth 1 (plist-get alice :rows)) :comment) :to-be nil)
        (expect (plist-get alice :draft) :to-equal "Draft for Alice.")
        (expect (plist-get alice :notes) :to-equal "Late by a day.")
        (expect (car sent) :to-equal '(:id "11" :author "Prof" :time "<2026-09-28 Mon 14:02>"
                                       :delete nil :text "One line."))
        (expect (plist-get (nth 1 sent) :text) :to-equal "First.\n\nSecond.")
        (expect (plist-get (test-scom--student-of students "5002") :draft) :to-be nil)
        (expect (plist-get (test-scom--student-of students "5003") :posted)
                :to-equal "<2026-09-29 Tue 10:00>"))))

  (it "writes JSON in snake_case with the assignment beside the students"
    (with-scom-file
      (let ((out (expand-file-name "out.json" dir)))
        (test-scom--quietly (org-canvas-submissions-export-comments file out))
        (let* ((json-object-type 'alist) (json-array-type 'list)
               (data (json-read-file out))
               (alice (car (alist-get 'students data))))
          (expect (alist-get 'assignment_id data) :to-equal "1001")
          (expect (alist-get 'assignment data) :to-equal "HW")
          (expect (alist-get 'user_id alice) :to-equal "5001")
          (expect (alist-get 'criterion_id (car (alist-get 'rows alice))) :to-equal "_1")
          (expect (alist-get 'delete (car (alist-get 'sent alice))) :to-be :json-false)
          (expect (alist-get 'rows (nth 3 (alist-get 'students data))) :not :to-be nil)))))

  (it "prints the JSON when the output is -"
    (with-scom-file
      (let ((printed (with-output-to-string
                       (org-canvas-submissions-export-comments nil "-"))))
        (expect printed :to-match "\"user_id\": \"5001\""))))

  (it "asks where to write when called interactively"
    (with-scom-file
      (let ((out (expand-file-name "asked.json" dir)))
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) out)))
          (test-scom--quietly (call-interactively #'org-canvas-submissions-export-comments)))
        (expect (file-exists-p out) :to-be-truthy))))

  (it "refuses the summary view"
    (with-scom-file
      (setq org-canvas-submissions--current-view 'summary)
      (expect (org-canvas-submissions-export-comments) :to-throw 'user-error))))

;;;; Import

(defun test-scom--lines-moved (before after)
  "Return (REMOVED ADDED), the lines of BEFORE and AFTER the other lacks."
  (let ((old (split-string before "\n")) (new (split-string after "\n")))
    (list (seq-difference old new) (seq-difference new old))))

(defun test-scom--edited (students)
  "Return STUDENTS with one row comment, one draft and one sent text changed."
  (mapcar
   (lambda (s)
     (pcase (plist-get s :user-id)
       ("5001"
        (let* ((rows (plist-get s :rows))
               (sent (plist-get s :sent)))
          (test-scom--set
           (test-scom--set s :rows (cons (test-scom--set (car rows) :comment "New thesis note.")
                                         (cdr rows)))
           :sent (cons (test-scom--set (car sent) :text "Rewritten.") (cdr sent)))))
       ("5002" (test-scom--set s :draft "Draft for Bob."))
       (_ s)))
   students))

(describe "org-canvas-submissions-import-comments (issue #438)"
  (it "leaves the file byte-identical when the unchanged export is imported"
    (with-scom-file
      (let* ((before (buffer-string))
             (out (expand-file-name "out.json" dir)))
        (test-scom--quietly (org-canvas-submissions-export-comments nil out))
        (let ((result (test-scom--quietly (org-canvas-submissions-import-comments nil out))))
          (expect (buffer-string) :to-equal before)
          (expect (buffer-modified-p) :to-be nil)
          (expect (list (plist-get result :rows) (plist-get result :drafts)
                        (plist-get result :sent))
                  :to-equal '(0 0 0))
          (expect (plist-get result :unmatched) :to-be nil)
          (expect (plist-get result :skipped) :to-be nil)))))

  (it "moves only the lines of the row comment, draft and sent text changed"
    (with-scom-file
      (let* ((before (buffer-string))
             (edited (test-scom--edited (org-canvas-submissions-export-comments)))
             (result (test-scom--quietly (org-canvas-submissions-import-comments nil edited))))
        (expect (list (plist-get result :rows) (plist-get result :drafts)
                      (plist-get result :sent))
                :to-equal '(1 1 1))
        (expect (test-scom--lines-moved before (buffer-string))
                :to-equal
                '(("- *Prof* <2026-09-28 Mon 14:02> [11] :: One line."
                   "- _1 :: Thesis unclear. The claim needs a reason given early.")
                  ("- *Prof* <2026-09-28 Mon 14:02> [11] :: Rewritten."
                   "- _1 :: New thesis note."
                   "Draft for Bob.")))
        (expect (length (split-string (buffer-string) "\n"))
                :to-equal (1+ (length (split-string before "\n"))))
        (expect (buffer-modified-p) :to-be nil)
        (expect (with-temp-buffer (insert-file-contents file) (buffer-string))
                :to-equal (buffer-string))
        ;; The state a hand edit leaves: the comment push sends it.
        (let ((edits (org-canvas--submissions-collect-comment-edits)))
          (expect (mapcar (lambda (e) (plist-get e :id)) edits) :to-equal '("11"))
          (expect (plist-get (car edits) :text) :to-equal "Rewritten."))
        ;; And the grade push reads the new draft and row.
        (org-canvas--submissions-goto-user "5002")
        (expect (org-canvas--submissions-comment-draft) :to-equal "Draft for Bob.")
        (org-canvas--submissions-goto-user "5001")
        (expect (nth 4 (car (org-canvas--submissions-rubric-rows)))
                :to-equal "New thesis note."))))

  (it "writes nothing under a dry run and reports the same counts"
    (with-scom-file
      (let* ((before (buffer-string))
             (edited (test-scom--edited (org-canvas-submissions-export-comments)))
             (result (let ((org-canvas--dry-run t))
                       (test-scom--quietly
                        (org-canvas-submissions-import-comments nil edited)))))
        (expect (buffer-string) :to-equal before)
        (expect (plist-get result :dry-run) :to-be t)
        (expect (list (plist-get result :rows) (plist-get result :drafts)
                      (plist-get result :sent))
                :to-equal '(1 1 1)))))

  (it "names the keys that matched nothing and changes nothing for them"
    (with-scom-file
      (let* ((before (buffer-string))
             (result (test-scom--quietly
                      (org-canvas-submissions-import-comments
                       nil '((:user-id 9999 :draft "x")
                             (:user-id 5001 :rows ((:criterion-id "_9" :comment "x"))
                              :sent ((:id 99 :text "x"))))))))
        (expect (buffer-string) :to-equal before)
        (expect (plist-get result :unmatched)
                :to-equal '("user 9999" "user 5001 criterion _9" "user 5001 comment 99")))))

  (it "names a draft with no Comment to post heading as unmatched"
    (with-scom-file
      (org-canvas--submissions-goto-user "5002")
      (let ((region (org-canvas--submissions-section-region
                     org-canvas--submissions-draft-heading)))
        (delete-region (- (car region) (length "** Comment to post\n")) (cdr region)))
      (let ((result (test-scom--quietly
                     (org-canvas-submissions-import-comments
                      nil '((:user-id "5002" :draft "x"))))))
        (expect (plist-get result :unmatched)
                :to-equal '("user 5002 draft (no Comment to post heading)")))))

  (it "keeps back a posted student's changes unless told otherwise"
    (with-scom-file
      (let* ((data '((:user-id "5003" :rows ((:criterion-id "_1" :comment "Posted note.")
                                             (:criterion-id "_9" :comment "x")))))
             (before (buffer-string))
             (kept (test-scom--quietly (org-canvas-submissions-import-comments nil data))))
        (expect (buffer-string) :to-equal before)
        (expect (plist-get kept :rows) :to-equal 0)
        (expect (plist-get kept :skipped)
                :to-equal '("user 5003 criterion _1: grade already posted"))
        (expect (plist-get kept :unmatched) :to-equal '("user 5003 criterion _9"))
        (let ((let-through (test-scom--quietly
                            (org-canvas-submissions-import-comments nil data t))))
          (expect (plist-get let-through :rows) :to-equal 1)
          (expect (buffer-string) :to-match "^- _1 :: Posted note\\.$")))))

  (it "refuses an emptied sent comment and a draft line Org reads as a comment"
    (with-scom-file
      (let* ((before (buffer-string))
             (result (test-scom--quietly
                      (org-canvas-submissions-import-comments
                       nil '((:user-id "5001" :sent ((:id "11" :text ""))
                              :draft "Fine.\n# not a comment"))))))
        (expect (buffer-string) :to-equal before)
        (expect (plist-get result :skipped)
                :to-equal '("user 5001 draft: a line starts with #, which Org reads as a comment"
                            "user 5001 comment 11: emptied; mark it DELETE in the file to delete it")))))

  (it "writes a comment of several paragraphs, empties one, and takes a number"
    (with-scom-file
      (test-scom--quietly
       (org-canvas-submissions-import-comments
        nil '((:user-id "5002" :rows ((:criterion-id "_1" :comment "One.\n\nTwo.")
                                      (:criterion-id "_2" :comment nil)))
              (:user-id "5001" :sent ((:id "12" :text 42))))))
      (org-canvas--submissions-goto-user "5002")
      (expect (mapcar (lambda (r) (nth 4 r)) (org-canvas--submissions-rubric-rows))
              :to-equal '("One.\n\nTwo." nil))
      (org-canvas--submissions-goto-user "5001")
      (expect (plist-get (org-canvas--submissions-find-sent-comment "12") :text)
              :to-equal "42")))

  (it "keeps the template above a draft it writes and replaces an old one"
    (with-scom-file
      (test-scom--quietly
       (org-canvas-submissions-import-comments
        nil '((:user-id "5001" :draft "New.\n\n\nSecond paragraph."))))
      (expect (buffer-string)
              :to-match "# Write a comment\\.\nNew\\.\n\nSecond paragraph\\.\n\n\\* Baker")
      (expect (buffer-string) :not :to-match "Draft for Alice")))

  (it "writes a draft into a section with nothing under it, ending the student"
    (with-scom-file
      (goto-char (point-min))
      (re-search-forward "^\\* Chen")
      (let ((heading (line-beginning-position)))
        (re-search-backward "^# Write a comment\\.\n")
        (delete-region (match-beginning 0) heading))
      (org-canvas--submissions-goto-user "5002")
      (expect (org-canvas--submissions-comment-draft) :to-be nil)
      (test-scom--quietly
       (org-canvas-submissions-import-comments nil '((:user-id "5002" :draft "Tight."))))
      (expect (buffer-string) :to-match "^\\*\\* Comment to post\nTight\\.\n\\* Chen")))

  (it "writes a draft into a section with nothing under it, before a sibling"
    (with-scom-file
      (goto-char (point-min))
      (re-search-forward "^\\* Chen")
      (let ((heading (line-beginning-position)))
        (re-search-backward "^# Write a comment\\.\n")
        (delete-region (match-beginning 0) heading)
        (goto-char (match-beginning 0))
        (insert "** Notes\n"))
      (test-scom--quietly
       (org-canvas-submissions-import-comments nil '((:user-id "5002" :draft "Tight."))))
      (expect (buffer-string)
              :to-match "^\\*\\* Comment to post\nTight\\.\n\\*\\* Notes\n\\* Chen")))

  (it "reads a bare JSON array of students"
    (with-scom-file
      (let ((in (expand-file-name "in.json" dir)))
        (with-temp-file in
          (insert "[{\"user_id\": 5002, \"draft\": \"From JSON.\"}]"))
        (let ((result (test-scom--quietly (org-canvas-submissions-import-comments file in))))
          (expect (plist-get result :drafts) :to-equal 1)))))

  (it "asks for the JSON file interactively, and refuses to guess in batch"
    (with-scom-file
      (let ((in (expand-file-name "in.json" dir)))
        (with-temp-file in (insert "{\"students\": [{\"user_id\": \"5002\", \"draft\": \"Asked.\"}]}"))
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) in)))
            (test-scom--quietly (call-interactively #'org-canvas-submissions-import-comments))))
        (expect (buffer-string) :to-match "^Asked\\.$")
        (let ((noninteractive t))
          (expect (org-canvas-submissions-import-comments) :to-throw 'user-error)))))

  (it "says what it did, the misses and the kept-back changes named"
    (with-scom-file
      (let ((said nil))
        (cl-letf (((symbol-function 'org-canvas--user-message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (let ((org-canvas--dry-run t))
            (org-canvas-submissions-import-comments
             nil '((:user-id 9999 :draft "x")
                   (:user-id "5003" :draft "Posted draft.")))))
        (expect (reverse said)
                :to-equal
                '("Comments: user 9999 matched nothing"
                  "Comments: kept back user 5003 draft: grade already posted"
                  "[DRY-RUN] Comments: 0 row comment(s), 0 draft(s), 0 sent comment(s) would change; 1 key(s) matched nothing, 1 kept back"))))))

;;;; Check

(defun test-scom--check ()
  "Run the check on the grading buffer at hand; return (FINDINGS REPORT)."
  (let ((report nil))
    (cl-letf (((symbol-function 'org-canvas--report-display)
               (lambda (_name render &optional _mode)
                 (setq report (with-temp-buffer (funcall render) (buffer-string)))))
              ((symbol-function 'message) #'ignore))
      (list (org-canvas-submissions-check-comments) report))))

(describe "org-canvas-submissions-check-comments (issue #438)"
  (it "names each deducted row without a comment, posted and left students skipped"
    (with-scom-file
      (pcase-let ((`(,findings ,report) (test-scom--check)))
        (expect (length findings) :to-equal 1)
        (let* ((f (car findings)) (kind (plist-get f :kind)) (sev (plist-get f :severity)))
          (expect kind :to-be 'missing)
          (expect sev :to-be 'error)
          (expect (plist-get f :user-id) :to-equal "5002")
          (expect (plist-get f :where) :to-equal "Thesis"))
        (expect (org-canvas-submissions-comment-check-errors findings) :to-equal 1)
        (expect report :to-match "^1 error(s), 0 warning(s); 2 student(s) checked, 2 skipped")
        (expect report :to-match "^- Baker, Bob (5002) :: Thesis: scored 2 of 5, no comment$"))))

  (it "flags text a course regexp matches, in rows, drafts and sent comments"
    (with-scom-file
      (let ((org-canvas-submissions-comment-flag-regexps '("Draft" "One line\\|Second")))
        (pcase-let ((`(,findings ,report) (test-scom--check)))
          (let ((flagged (seq-filter (lambda (f) (eq (plist-get f :kind) 'flagged)) findings)))
            (expect (mapcar (lambda (f) (plist-get f :where)) flagged)
                    :to-equal '("draft" "comment 11" "comment 12")))
          (expect report :to-match "^- Adams, Alice (5001), comment 12 :: matches =One line\\\\|Second=: First\\. Second\\.$")))))

  (it "warns of a sentence given to more students than the threshold"
    (with-scom-file
      (let ((org-canvas-submissions-comment-shared-threshold 1))
        (pcase-let ((`(,findings ,report) (test-scom--check)))
          (let ((shared (seq-filter (lambda (f) (eq (plist-get f :kind) 'shared)) findings)))
            (expect (length shared) :to-equal 1)
            (expect (plist-get (car shared) :text)
                    :to-equal "The claim needs a reason given early.")
            (expect (plist-get (car shared) :user-ids) :to-equal '("5001" "5002"))
            (expect (org-canvas-submissions-comment-check-errors findings) :to-equal 1))
          (expect report :to-match "^\\* Sentences given to more than 1 students$")
          (expect report :to-match "^- 2 students :: The claim needs a reason given early\\. (5001, 5002)$")))))

  (it "orders shared sentences by how many students share them"
    (let ((org-canvas-submissions-comment-shared-threshold 1)
          (org-canvas-submissions-comment-sentence-min-length 5))
      (let ((found (org-canvas--submissions-comments-shared
                    '((:user-id "1" :draft "Bravo bravo. Alpha alpha. Zulu zulu.")
                      (:user-id "2" :draft "Bravo bravo. Alpha alpha. Zulu zulu.")
                      (:user-id "3" :draft "Zulu zulu.")))))
        (expect (mapcar (lambda (f) (plist-get f :text)) found)
                :to-equal '("Zulu zulu." "Alpha alpha." "Bravo bravo.")))))

  (it "names a row without a criterion name by its id"
    (let* ((student '(:user-id "1" :name "X"
                      :rows ((:criterion-id "_7" :max "5" :score "1" :comment nil)
                             (:criterion-id "_8" :max "5" :score "5" :comment "Fine."))))
           (missing (org-canvas--submissions-comments-missing student)))
      (expect (mapcar (lambda (f) (plist-get f :where)) missing) :to-equal '("_7"))
      (expect (org-canvas--submissions-comments-texts student) :to-equal '(("_8" . "Fine.")))))

  (it "counts no shared sentence when the threshold is off"
    (let ((org-canvas-submissions-comment-shared-threshold nil))
      (expect (org-canvas--submissions-comments-shared
               '((:user-id "1" :draft "Same sentence here, long enough.")
                 (:user-id "2" :draft "Same sentence here, long enough.")))
              :to-be nil))))

;;;; Shared reader

(describe "org-canvas--submissions-section-normalize (issue #438)"
  (it "drops comment lines and end blanks, and folds blank runs"
    (expect (org-canvas--submissions-section-normalize "# t\n\nA\n\n\n\nB\n\n")
            :to-equal "A\n\nB")
    (expect (org-canvas--submissions-section-normalize "# only\n\n") :to-be nil)))

;;;; Batch

(defun test-scom--batch (args)
  "Return the exit status and printed text of the batch command line ARGS."
  (let ((status nil))
    (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
              ((symbol-function 'org-canvas--report-display) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'org-canvas--user-message) #'ignore))
      (let ((out (with-output-to-string (setq status (org-canvas-batch-main args)))))
        (list status out)))))

(describe "the comment subcommands of the command line (issue #438)"
  (it "exports to standard output or a file"
    (with-scom-file
      (pcase-let ((`(,status ,out) (test-scom--batch '("export-comments" "HW"))))
        (expect status :to-equal 0)
        (expect out :to-match "\"criterion_id\": \"_1\""))
      (let ((out (expand-file-name "b.json" dir)))
        (expect (car (test-scom--batch (list "export-comments" "HW" out))) :to-equal 0)
        (expect (file-exists-p out) :to-be-truthy))))

  (it "imports, a dry run on -n, exiting 1 on a miss or a kept-back change"
    (with-scom-file
      (let ((in (expand-file-name "in.json" dir))
            (before (buffer-string)))
        (with-temp-file in (insert "[{\"user_id\": \"5002\", \"draft\": \"Batch.\"}]"))
        (expect (car (test-scom--batch (list "-n" "import-comments" "HW" in))) :to-equal 0)
        (expect (buffer-string) :to-equal before)
        (expect org-canvas--dry-run :to-be nil)
        (expect (car (test-scom--batch (list "import-comments" "HW" in))) :to-equal 0)
        (expect (buffer-string) :to-match "^Batch\\.$")
        (with-temp-file in (insert "[{\"user_id\": \"5003\", \"draft\": \"Late.\"}]"))
        (expect (car (test-scom--batch (list "import-comments" "HW" in))) :to-equal 1)
        (expect (car (test-scom--batch (list "import-comments" "--posted" "HW" in)))
                :to-equal 0)
        (expect (car (test-scom--batch (list "import-comments" "--posted" "HW")))
                :to-equal 2))))

  (it "checks each file, exiting 1 on an error"
    (with-scom-file
      (expect (car (test-scom--batch '("check-comments" "HW"))) :to-equal 1)
      (cl-letf (((symbol-function 'org-canvas-submissions-check-comments)
                 (lambda (_f) (list (list :severity 'warning)))))
        (expect (car (test-scom--batch '("check-comments" "HW"))) :to-equal 0)))))

(provide 'org-canvas-submissions-comments-test)
;;; org-canvas-submissions-comments-test.el ends here
