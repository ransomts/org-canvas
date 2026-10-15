;;; org-canvas-submissions-grades-test.el --- Tests for excusing and importing grades -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-excuse' (issue #501), the EX score
;; of a ruling, and `org-canvas-submissions-import-grades' (issue #483),
;; with their command-line forms.  Everything is local: no spec reaches
;; Canvas.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-grades)
(require 'org-canvas-batch)

(defun test-sgr--student (name uid props &rest sections)
  "Return a grading-file heading NAME for UID with PROPS and SECTIONS text."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n" name uid)
          (mapconcat (lambda (p) (format ":%s: %s\n" (car p) (cdr p))) props "")
          ":END:\n" (apply #'concat sections)))

(defun test-sgr--rubric (&rest rows)
  "Return a Rubric section of ROWS, each (ID CRITERION MAX SCORE COMMENT)."
  (with-temp-buffer
    (org-mode)
    (insert "| Id | Criterion | Max | Score |\n|---+---+---+---|\n")
    (dolist (r rows)
      (insert (format "| %s | %s | %s | %s |\n" (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r))))
    (goto-char (point-min))
    (org-table-align)
    (concat "\n** Rubric\n" (buffer-string)
            (mapconcat (lambda (r) (concat (org-canvas--submissions-item (nth 0 r) (nth 4 r)) "\n"))
                       rows ""))))

(defconst test-sgr--notes "\n** Notes\n# Your notes for this student.\n"
  "An empty Notes section, as a pull writes it.")

(defun test-sgr--file (&optional header)
  "Return the grading file the specs read, HEADER lines added to its header."
  (concat
   "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
   "#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n" (or header "") "\n"
   (test-sgr--student
    "Adams, Alice" 5001 '(("STATUS" . "graded") ("SCORE" . "8") ("CANVAS_SCORE" . "8")
                          ("SUBMITTED_AT" . "<2026-10-01 Thu 10:00>"))
    (test-sgr--rubric '("_409" "Thesis" "5" "3" "Thesis unclear.")
                      '("_5390" "Structure" "5" "5" nil))
    "\n** Notes\n# Your notes for this student.\nLate by a day.\n")
   (test-sgr--student
    "Baker, Bob" 5002 '(("STATUS" . "submitted") ("SUBMITTED_AT" . "<2026-10-01 Thu 11:00>"))
    (test-sgr--rubric '("_409" "Thesis" "5" "" nil) '("_5390" "Structure" "5" "" nil))
    test-sgr--notes)
   (test-sgr--student "Chen, Cara" 5003 '(("STATUS" . "missing")) test-sgr--notes)
   (test-sgr--student "Davis, Dan" 5004 '(("STATUS" . "left")))
   (test-sgr--student "Evans, Eve" 5005 '(("STATUS" . "missing") ("SCORE" . "0")))
   (test-sgr--student "Fox, Fay" 5006 '(("STATUS" . "missing") ("SCORE" . "EX")))
   (test-sgr--student
    "Gray, Gil" 5007 '(("STATUS" . "missing") ("POSTED_AT" . "<2026-10-02 Fri 09:00>"))
    (test-sgr--rubric '("_409" "Thesis" "5" "" nil) '("_5390" "Structure" "5" "" nil)))))

(defmacro with-sgr-file (header &rest body)
  "Run BODY in a scratch grading file holding `test-sgr--file' with HEADER.
`dir' is the submissions directory and `file' the grading file."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-canvas-sgr-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (org-canvas--dry-run nil)
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert (test-sgr--file ,header)))
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

(defun test-sgr--goto (uid)
  "Move point to the heading of USER_ID UID."
  (goto-char (point-min))
  (re-search-forward (format "^:USER_ID: %s$" uid))
  (org-back-to-heading t))

(defun test-sgr--prop (uid name)
  "Return property NAME of UID's heading."
  (save-excursion (test-sgr--goto uid) (org-entry-get (point) name)))

(defun test-sgr--rows (uid)
  "Return the Rubric rows of UID as (ID SCORE COMMENT) lists."
  (save-excursion
    (test-sgr--goto uid)
    (mapcar (lambda (r) (list (nth 0 r) (nth 3 r) (nth 4 r)))
            (org-canvas--submissions-rubric-rows))))

(defun test-sgr--notes (uid)
  "Return the text of UID's Notes."
  (save-excursion
    (test-sgr--goto uid)
    (org-canvas--submissions-section-text org-canvas--submissions-notes-heading)))

(defun test-sgr--ids (plans)
  "Return the :user-id of each of PLANS."
  (mapcar (lambda (p) (plist-get p :user-id)) plans))

(defun test-sgr--batch (args)
  "Return the exit status of the batch command line ARGS."
  (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
            ((symbol-function 'org-canvas--report-display) #'ignore))
    (let ((status nil))
      (with-output-to-string (setq status (org-canvas-batch-main args)))
      status)))

;;;; Excusing

(describe "org-canvas-submissions-excuse (issue #501)"
  (it "excuses every student with no submission, leaving the graded and the left"
    (with-sgr-file nil
      (let ((result (org-canvas-submissions-excuse)))
        (expect (test-sgr--ids (plist-get result :excused)) :to-equal '("5003" "5007"))
        (expect (plist-get result :already) :to-equal '("5006"))
        (expect (plist-get result :skipped)
                :to-equal '(("5004" . "left the course") ("5005" . "graded 0")))
        (expect (plist-get result :dry-run) :to-be nil)
        (expect (test-sgr--prop "5003" "SCORE") :to-equal "EX")
        (expect (test-sgr--prop "5007" "SCORE") :to-equal "EX")
        (expect (test-sgr--prop "5002" "SCORE") :to-be nil)
        (expect (test-sgr--prop "5005" "SCORE") :to-equal "0")
        (expect (buffer-modified-p) :to-be nil)
        (with-temp-buffer
          (insert-file-contents file)
          (expect (buffer-string) :to-match "^:SCORE: +EX$")))))

  (it "excuses every student with all, and the graded ones too with overwrite"
    (with-sgr-file nil
      (let ((result (org-canvas-submissions-excuse nil 'all)))
        (expect (test-sgr--ids (plist-get result :excused)) :to-equal '("5002" "5003" "5007"))
        (expect (mapcar #'car (plist-get result :skipped)) :to-equal '("5001" "5004" "5005")))
      (let ((result (org-canvas-submissions-excuse nil 'all t)))
        (expect (test-sgr--ids (plist-get result :excused)) :to-equal '("5001" "5005"))
        (expect (plist-get (car (plist-get result :excused)) :old-score) :to-equal "8")
        (expect (plist-get result :skipped) :to-equal '(("5004" . "left the course")))
        (expect (test-sgr--prop "5001" "SCORE") :to-equal "EX"))))

  (it "takes the student at point, or the students in the region"
    (with-sgr-file nil
      (test-sgr--goto "5002")
      (re-search-forward "Structure")
      (let ((result (org-canvas-submissions-excuse nil 'point)))
        (expect (test-sgr--ids (plist-get result :excused)) :to-equal '("5002")))
      (transient-mark-mode 1)
      (test-sgr--goto "5001")
      (forward-line 2)
      (push-mark (point) t t)
      (test-sgr--goto "5003")
      (forward-line 1)
      (let ((result (org-canvas-submissions-excuse nil 'point)))
        (expect (test-sgr--ids (plist-get result :excused)) :to-equal '("5003"))
        (expect (plist-get result :already) :to-equal '("5002"))
        (expect (plist-get result :skipped) :to-equal '(("5001" . "graded 8"))))
      (deactivate-mark)
      (goto-char (point-min))
      (expect (org-canvas-submissions-excuse nil 'point) :to-throw 'user-error)
      (push-mark (point) t t)
      (forward-line 1)
      (expect (org-canvas-submissions-excuse nil 'point) :to-throw 'user-error)
      (deactivate-mark)))

  (it "writes nothing under a dry run, and refuses a scope it does not know"
    (with-sgr-file nil
      (let* ((before (buffer-string))
             (org-canvas--dry-run t)
             (result (org-canvas-submissions-excuse)))
        (expect (buffer-string) :to-equal before)
        (expect (plist-get result :dry-run) :to-be t)
        (expect (length (plist-get result :excused)) :to-equal 2))
      (expect (org-canvas-submissions-excuse nil 'everyone) :to-throw 'user-error)))

  (it "asks for the scope interactively, the region meaning point"
    (with-sgr-file nil
      (let ((noninteractive nil) (asked nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest args) (setq asked args) "all")))
          (call-interactively #'org-canvas-submissions-excuse)
          (expect (car asked) :to-equal "Excuse which students? ")
          (expect (test-sgr--prop "5002" "SCORE") :to-equal "EX")
          (expect (test-sgr--prop "5001" "SCORE") :to-equal "8")
          (let ((current-prefix-arg '(4)))
            (call-interactively #'org-canvas-submissions-excuse))
          (expect (test-sgr--prop "5001" "SCORE") :to-equal "EX")))
      (cl-letf (((symbol-function 'use-region-p) (lambda () t)))
        (expect (org-canvas--submissions-excuse-read-scope) :to-be 'point))))

  (it "names each student it excused and kept back, then sums up"
    (let ((lines nil))
      (cl-letf (((symbol-function 'org-canvas--user-message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) lines))))
        (org-canvas--submissions-excuse-report
         '(:excused ((:user-id "1" :name "A" :old-score nil)) :already ("2")
           :skipped (("3" . "graded 4")) :dry-run nil))
        (expect (car lines)
                :to-equal "Excuse: 1 student(s) excused, 1 already excused, 1 kept back; press S to push")
        (expect lines :to-contain "Excuse: user 1 (A): SCORE - -> EX")
        (expect lines :to-contain "Excuse: kept back user 3: graded 4")
        (org-canvas--submissions-excuse-report '(:excused nil :dry-run t))
        (expect (car lines)
                :to-equal "[DRY-RUN] Excuse: 0 student(s) would be excused, 0 already excused, 0 kept back"))))

  (it "runs from the command line, --all and -n among its options"
    (with-sgr-file nil
      (let ((before (buffer-string)))
        (expect (test-sgr--batch '("-n" "excuse" "HW")) :to-equal 0)
        (expect (buffer-string) :to-equal before)
        (expect (test-sgr--batch '("excuse" "HW")) :to-equal 0)
        (expect (test-sgr--prop "5003" "SCORE") :to-equal "EX")
        (expect (test-sgr--prop "5002" "SCORE") :to-be nil)
        (expect (test-sgr--batch '("excuse" "--all" "HW")) :to-equal 0)
        (expect (test-sgr--prop "5002" "SCORE") :to-equal "EX")
        (expect (test-sgr--batch '("excuse" "--overwrite" "--all" "HW")) :to-equal 0)
        (expect (test-sgr--prop "5005" "SCORE") :to-equal "EX")
        (expect (test-sgr--batch '("excuse" "--every" "HW")) :to-equal 2)
        (expect (test-sgr--batch '("excuse" "HW" "HW2")) :to-equal 2)))))

(describe "an EX score in a ruling (issue #501)"
  (it "excuses the listed students, beside rows a grading rubric totals"
    (with-sgr-file "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING true\n"
      (let ((result (org-canvas-submissions-apply-ruling nil '(5001 5003) :score "ex"
                                                         :note "Called off.")))
        (expect (test-sgr--ids (plist-get result :changed)) :to-equal '("5001" "5003"))
        (expect (plist-get result :skipped) :to-be nil)
        (expect (test-sgr--prop "5001" "SCORE") :to-equal "EX")
        (expect (test-sgr--notes "5001") :to-match "Before: SCORE 8"))))

  (it "takes \"score\": \"EX\" from the JSON, and still refuses other text"
    (with-sgr-file nil
      (let ((in (expand-file-name "r.json" dir)))
        (with-temp-file in (insert "{\"user_ids\": [5002], \"score\": \"EX\"}"))
        (org-canvas-submissions-apply-ruling-json "HW" in)
        (expect (test-sgr--prop "5002" "SCORE") :to-equal "EX")
        (expect (org-canvas-submissions-apply-ruling "HW" '(1) :score "zero")
                :to-throw 'user-error)))))

;;;; Importing grades

(defun test-sgr--student-data (uid &rest props)
  "Return an imported student UID with PROPS."
  (append (list :user-id uid) props))

(describe "org-canvas-submissions-import-grades (issue #483)"
  (it "writes row scores whatever the ids' padding, sums SCORE, and adds the notes"
    (with-sgr-file nil
      (let ((result (org-canvas-submissions-import-grades
                     nil (list (test-sgr--student-data
                                "5002" :rows '((:criterion-id "_409" :score 3)
                                               (:criterion-id "_5390" :score "4.5"))
                                :notes "Drafted by the TA.\nSecond line.")))))
        (expect (test-sgr--rows "5002")
                :to-equal '(("_409" "3" nil) ("_5390" "4.5" nil)))
        (expect (test-sgr--prop "5002" "SCORE") :to-equal "7.5")
        (expect (test-sgr--notes "5002") :to-equal "Drafted by the TA.\nSecond line.")
        (expect (plist-get result :scores) :to-equal 2)
        (expect (plist-get result :score) :to-equal 1)
        (expect (plist-get result :notes) :to-equal 1)
        (expect (plist-get result :unmatched) :to-be nil)
        (expect (plist-get result :skipped) :to-be nil)
        (expect (buffer-modified-p) :to-be nil)
        (with-temp-buffer
          (insert-file-contents file)
          (expect (buffer-string) :to-match "^| _409 +| Thesis +| +5 | +3 |$")))))

  (it "sets an explicit SCORE or EX, and leaves SCORE when rows are only partly scored"
    (with-sgr-file nil
      (org-canvas-submissions-import-grades
       nil (list (test-sgr--student-data "5003" :score "EX")
                 (test-sgr--student-data "5002" :rows '((:criterion-id "_409" :score 2)))
                 (test-sgr--student-data "5005" :score 4.25)))
      (expect (test-sgr--prop "5003" "SCORE") :to-equal "EX")
      (expect (test-sgr--prop "5002" "SCORE") :to-be nil)
      (expect (test-sgr--rows "5002") :to-equal '(("_409" "2" nil) ("_5390" nil nil)))
      (expect (test-sgr--prop "5005" "SCORE") :to-equal "4.25")))

  (it "appends to the Notes, and adds nothing the Notes already hold"
    (with-sgr-file nil
      (let ((result (org-canvas-submissions-import-grades
                     nil (list (test-sgr--student-data "5001" :notes "Regraded.")))))
        (expect (test-sgr--notes "5001") :to-equal "Late by a day.\nRegraded.")
        (expect (plist-get result :notes) :to-equal 1))
      (let ((result (org-canvas-submissions-import-grades
                     nil (list (test-sgr--student-data "5001" :notes "Late by a day.")))))
        (expect (plist-get result :notes) :to-equal 0))
      (org-canvas-submissions-import-grades
       nil (list (test-sgr--student-data "5004" :notes "Gone.")))
      (expect (test-sgr--notes "5004") :to-be nil)))

  (it "ends the added notes with a newline at the end of the buffer"
    (with-temp-buffer
      (org-mode)
      (insert "* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n** Notes\nFirst.")
      (goto-char (point-min))
      (org-canvas--submissions-grades-append-notes "Second.")
      (expect (buffer-string) :to-match "\nFirst\\.\nSecond\\.\n\\'")))

  (it "keeps a student back whole when a value is refused, naming every miss"
    (with-sgr-file nil
      (let* ((result (org-canvas-submissions-import-grades
                      nil (list (test-sgr--student-data
                                 "5002" :rows '((:criterion-id "_409" :score 3)
                                                (:criterion-id "_5390" :score 9)
                                                (:criterion-id "_77" :score 1)))
                                (test-sgr--student-data "5003" :score "lots")
                                (test-sgr--student-data "5005" :notes "* Heading")
                                (test-sgr--student-data "9999" :score 1))))
             (skipped (plist-get result :skipped)))
        (expect (test-sgr--rows "5002") :to-equal '(("_409" nil nil) ("_5390" nil nil)))
        (expect (test-sgr--prop "5003" "SCORE") :to-be nil)
        (expect (plist-get result :unmatched)
                :to-equal '("user 5002 criterion _77" "user 9999"))
        (expect skipped :to-contain
                "user 5002 criterion _5390: score 9 is not a number from 0 to its Max 5")
        (expect skipped :to-contain
                "user 5002 criterion _409: another value for this student was refused")
        (expect skipped :to-contain "user 5003 SCORE lots is not a number or EX")
        (expect (seq-find (lambda (s) (string-prefix-p "user 5005 notes: a line starts" s))
                          skipped)
                :not :to-be nil)
        (expect (plist-get result :scores) :to-equal 0))))

  (it "refuses a SCORE the rows contradict when the rubric grades"
    (with-sgr-file "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING true\n"
      (let ((result (org-canvas-submissions-import-grades
                     nil (list (test-sgr--student-data
                                "5002" :score 10
                                :rows '((:criterion-id "_409" :score 3)
                                        (:criterion-id "_5390" :score 4)))))))
        (expect (plist-get result :skipped)
                :to-contain "user 5002 SCORE 10 disagrees with the rubric total 7, and the rubric is used for grading")
        (expect (test-sgr--prop "5002" "SCORE") :to-be nil))))

  (it "keeps back a posted student unless told, and holds one with an empty score"
    (with-sgr-file nil
      (let* ((data (list (test-sgr--student-data
                          "5007" :rows '((:criterion-id "_409" :score 5)))
                         (test-sgr--student-data
                          "5002" :score "" :notes "Waiting on the extension ruling."
                          :rows '((:criterion-id "_409" :score 5)))))
             (result (org-canvas-submissions-import-grades nil data)))
        (expect (plist-get result :skipped)
                :to-equal '("user 5007 criterion _409: grade already posted"))
        (expect (plist-get result :held) :to-equal '("user 5002 held: notes only, no score"))
        (expect (test-sgr--rows "5007") :to-equal '(("_409" nil nil) ("_5390" nil nil)))
        (expect (test-sgr--rows "5002") :to-equal '(("_409" nil nil) ("_5390" nil nil)))
        (expect (test-sgr--notes "5002") :to-equal "Waiting on the extension ruling.")
        (setq result (org-canvas-submissions-import-grades nil data t))
        (expect (plist-get result :skipped) :to-be nil)
        (expect (test-sgr--rows "5007") :to-equal '(("_409" "5" nil) ("_5390" nil nil))))))

  (it "imports the comments in the same pass"
    (with-sgr-file nil
      (let ((result (org-canvas-submissions-import-grades
                     nil (list (test-sgr--student-data
                                "5002" :rows '((:criterion-id "_409" :score 4
                                                :comment "Say what the claim is."))
                                :draft "Good start.")))))
        (expect (test-sgr--rows "5002")
                :to-equal '(("_409" "4" "Say what the claim is.") ("_5390" nil nil)))
        (expect (plist-get result :scores) :to-equal 1)
        (expect (plist-get result :rows) :to-equal 1)
        (expect (plist-get result :drafts) :to-equal 0)
        (expect (plist-get result :unmatched) :to-equal '("user 5002 draft (no Comment to post heading)")))))

  (it "changes nothing when an unchanged export comes back, nor under a dry run"
    (with-sgr-file nil
      (let* ((before (buffer-string))
             (students (org-canvas-submissions-export-comments))
             (result (org-canvas-submissions-import-grades nil students)))
        (expect (buffer-string) :to-equal before)
        (expect (+ (plist-get result :scores) (plist-get result :score)
                   (plist-get result :notes) (plist-get result :rows))
                :to-equal 0)
        (let* ((org-canvas--dry-run t)
               (result (org-canvas-submissions-import-grades
                        nil (list (test-sgr--student-data "5003" :score 2 :notes "x")))))
          (expect (buffer-string) :to-equal before)
          (expect (plist-get result :score) :to-equal 1)
          (expect (plist-get result :dry-run) :to-be t)))))

  (it "reads a JSON file, asks for one interactively, and refuses to guess in batch"
    (with-sgr-file nil
      (let ((in (expand-file-name "in.json" dir)))
        (with-temp-file in
          (insert "{\"students\": [{\"user_id\": 5002, \"rows\": [{\"criterion_id\": \"_409\", \"score\": 1}, {\"criterion_id\": \"_5390\", \"score\": 2}]}]}"))
        (org-canvas-submissions-import-grades file in)
        (expect (test-sgr--prop "5002" "SCORE") :to-equal "3")
        (with-temp-file in (insert "[{\"user_id\": \"5003\", \"score\": 1}]"))
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) in)))
            (call-interactively #'org-canvas-submissions-import-grades)))
        (expect (test-sgr--prop "5003" "SCORE") :to-equal "1")
        (let ((noninteractive t))
          (expect (org-canvas-submissions-import-grades) :to-throw 'user-error)))))

  (it "says what it did, the misses, the kept back and the held named"
    (let ((lines nil))
      (cl-letf (((symbol-function 'org-canvas--user-message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) lines))))
        (org-canvas--submissions-grades-report
         '(:scores 2 :score 1 :notes 1 :held ("user 3 held: notes only, no score")
           :rows 0 :drafts 0 :sent 0 :unmatched ("user 9") :skipped ("user 4 SCORE x")
           :dry-run t)))
      (expect (car lines)
              :to-equal "[DRY-RUN] Grades: 2 row score(s), 1 SCORE(s), 1 note(s), 0 row comment(s), 0 draft(s), 0 sent comment(s) would change; 1 key(s) matched nothing, 1 kept back, 1 held")
      (expect lines :to-contain "Grades: user 9 matched nothing")
      (expect lines :to-contain "Grades: kept back user 4 SCORE x")
      (expect lines :to-contain "Grades: user 3 held: notes only, no score")))

  (it "runs from the command line, exiting 1 on a miss"
    (with-sgr-file nil
      (let ((in (expand-file-name "in.json" dir))
            (before (buffer-string)))
        (with-temp-file in (insert "[{\"user_id\": 5003, \"score\": 2}]"))
        (expect (test-sgr--batch (list "-n" "import-grades" "HW" in)) :to-equal 0)
        (expect (buffer-string) :to-equal before)
        (expect (test-sgr--batch (list "import-grades" "HW" in)) :to-equal 0)
        (expect (test-sgr--prop "5003" "SCORE") :to-equal "2")
        (with-temp-file in (insert "[{\"user_id\": 5007, \"rows\": [{\"criterion_id\": \"_409\", \"score\": 2}]}]"))
        (expect (test-sgr--batch (list "import-grades" "HW" in)) :to-equal 1)
        (expect (test-sgr--batch (list "import-grades" "--posted" "HW" in)) :to-equal 0)
        (expect (test-sgr--batch (list "import-grades" "--posted" "HW")) :to-equal 2)))))

(provide 'org-canvas-submissions-grades-test)
;;; org-canvas-submissions-grades-test.el ends here
