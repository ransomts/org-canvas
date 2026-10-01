;;; org-canvas-submissions-reports-test.el --- Buttercup tests for the cross-column reports -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-reports' (issue #437): every
;; column with a document processor, its current-attempt reports read
;; through the #436 reader, rendered one row per student.  Every
;; GraphQL request is answered by a fake keyed on the document; REST
;; must not be reached at all, and nothing here reaches the network
;; (Hard Rule 2) or reads the shared log buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-batch)

(defvar test-preports--columns nil
  "The assignments the fake course lists: (ID NAME DUE PROCESSOR-P).")

(defvar test-preports--submissions nil
  "Each column's submission nodes, an alist from the assignment id string.
The symbol `refused' makes that column's read fail.")

(defun test-preports--column-node (column)
  "Return the assignments-query node of COLUMN, (ID NAME DUE PROCESSOR-P)."
  `((_id . ,(nth 0 column)) (name . ,(nth 1 column))
    (dueAt . ,(or (nth 2 column) :null))
    (ltiAssetProcessorsConnection
     . ((nodes . ,(if (nth 3 column) [((_id . "9"))] []))))))

(defun test-preports--report (type progress &optional result attachment code resubmit)
  "Return a report node of TYPE at PROGRESS with RESULT on ATTACHMENT.
CODE is its error code, RESUBMIT non-nil its resubmitAvailable."
  `((reportType . ,type) (processingProgress . ,progress)
    (result . ,(or result :null)) (errorCode . ,(or code :null))
    (resubmitAvailable . ,(if resubmit t :json-false))
    (asset . ((attachmentId . ,(or attachment :null))
              (submissionAttempt . :null)
              (discussionEntryVersion . :null)))))

(defun test-preports--submission (uid name sections files reports &optional attempt)
  "Return a submission node for UID NAME in SECTIONS with FILES and REPORTS.
FILES are (ID . DISPLAY-NAME) pairs; nil FILES and nil REPORTS with a
nil ATTEMPT is a row nothing was handed in on."
  `((userId . ,uid) (attempt . ,(or attempt 1))
    (submittedAt . ,(if (or files reports) "2026-09-20T16:00:00Z" :null))
    (attachments . ,(vconcat (mapcar (lambda (f) `((_id . ,(car f))
                                                    (displayName . ,(cdr f))))
                                     files)))
    (user . ((sortableName . ,name)))
    (enrollmentsConnection
     . ((nodes . ,(vconcat (mapcar (lambda (s) `((section . ((name . ,s)))))
                                   sections)))))
    (ltiAssetReportsConnection . ((nodes . ,(vconcat reports))))))

(defun test-preports--page (connection-parent connection nodes)
  "Return a one-page reply under CONNECTION-PARENT and CONNECTION of NODES."
  `((,connection-parent
     . ((,connection
         . ((pageInfo . ((hasNextPage . :json-false) (endCursor . :null)))
            (nodes . ,(vconcat nodes))))))))

(defun test-preports--graphql (document &optional variables)
  "Answer DOCUMENT sent VARIABLES from the fake course."
  (cond
   ((eq document org-canvas--processor-reports-columns-query)
    (test-preports--page 'course 'assignmentsConnection
                         (mapcar #'test-preports--column-node test-preports--columns)))
   ((eq document org-canvas--processor-reports-query)
    (let ((nodes (cdr (assoc (alist-get 'assignmentId variables)
                             test-preports--submissions))))
      (if (eq nodes 'refused)
          (signal 'org-canvas-api-error (list "GraphQL: refused"))
        (test-preports--page 'assignment 'submissionsConnection nodes))))
   (t (error "Unexpected document %S" document))))

(defmacro test-preports--with-course (&rest body)
  "Run BODY against the fake course, the report shown nowhere."
  (declare (indent 0))
  `(with-org-canvas-test-config
     (let ((printed nil))
       ;; Under batch the report is printed; it is caught here instead,
       ;; and the buffer it was rendered in is what the specs read.
       (cl-letf (((symbol-function 'org-canvas--graphql-query) #'test-preports--graphql)
                 ((symbol-function 'org-canvas-api-request)
                  (lambda (&rest _) (error "No REST request is expected")))
                 ((symbol-function 'princ)
                  (lambda (object &rest _) (push object printed)))
                 ((symbol-function 'display-buffer) #'ignore)
                 ((symbol-function 'message) #'ignore))
         ,@body
         (ignore printed)))))

(defun test-preports--text ()
  "Return the rendered report's text."
  (with-current-buffer org-canvas--processor-reports-buffer-name (buffer-string)))

(defun test-preports--flat-text ()
  "Return the rendered report with each run of spaces made one."
  (replace-regexp-in-string " +" " " (test-preports--text)))

(defconst test-preports--course
  (list
   (list "101" "R5" "2026-09-10T03:59:00Z" t)
   (list "102" "R6" "2026-09-17T03:59:00Z" t)
   (list "103" "Quiz" nil nil))
  "Two columns with a processor and one without.")

(defun test-preports--course-submissions ()
  "Return the fake course's submissions, column by column."
  (list
   (cons "101"
         (list
          ;; Alice: a replaced attempt's file and the current one.
          (test-preports--submission
           "5001" "Adams, Alice" '("Lab 002" "Lecture 001") '(("11" . "r5.pdf"))
           (list (test-preports--report "originality" "Processed" "31%" "10")
                 (test-preports--report "originality" "Processed" "12%" "11")
                 (test-preports--report "turnitin_aiwriting" "Processed" "--%" "11"))
           2)
          ;; Bob: refused.
          (test-preports--submission
           "5002" "Beta, Bob" '("Lab 003") '(("12" . "bob.pdf"))
           (list (test-preports--report "originality" "Failed" nil "12"
                                        "EULA_NOT_ACCEPTED" t)))
          ;; Cy: nothing handed in.
          (test-preports--submission "5003" "Cyan, Cy" '("Lab 003") nil nil)))
   (cons "102"
         (list
          ;; Alice: scored.
          (test-preports--submission
           "5001" "Adams, Alice" '("Lab 002" "Lecture 001") '(("21" . "r6.pdf"))
           (list (test-preports--report "originality" "Processed" "5%" "21")
                 (test-preports--report "turnitin_aiwriting" "Processed" "*%" "21")))
          ;; Bob: handed in, no report.
          (test-preports--submission
           "5002" "Beta, Bob" '("Lab 003") '(("22" . "late.docx")) nil)))))

(describe "org-canvas-submissions-reports (issue #437)"
  (it "reads every column with a processor and returns the rows per student"
    (test-preports--with-course
      (let* ((test-preports--columns test-preports--course)
             (test-preports--submissions (test-preports--course-submissions))
             (result (org-canvas-submissions-reports))
             (columns (plist-get result :columns))
             (students (plist-get result :students))
             (alice (car students))
             (r5 (car columns)))
        (expect (mapcar (lambda (c) (plist-get c :name)) columns) :to-equal '("R5" "R6"))
        (expect (mapcar (lambda (s) (plist-get s :name)) students)
                :to-equal '("Adams, Alice" "Beta, Bob"))
        (expect (plist-get alice :sections) :to-equal "Lab 002, Lecture 001")
        (expect (plist-get (org-canvas--processor-reports-cell-of alice r5) :sim)
                :to-equal "12%")
        (expect (plist-get (org-canvas--processor-reports-cell-of alice r5) :ai)
                :to-equal "--%")
        (expect (plist-get r5 :summary)
                :to-equal '(:handed-in 2
                            :counts (:processed 0 :unscored 1 :failed 1 :pending 0 :none 0)
                            :replaced 1))
        (expect (plist-get (plist-get (cadr columns) :summary) :counts)
                :to-equal '(:processed 1 :unscored 0 :failed 0 :pending 0 :none 1))
        (expect (plist-get result :file) :to-be nil))))

  (it "renders the summary, every student, the refused files and the missing reports"
    (test-preports--with-course
      (let ((test-preports--columns test-preports--course)
            (test-preports--submissions (test-preports--course-submissions)))
        (org-canvas-submissions-reports)
        (let* ((text (test-preports--flat-text))
               (missing (cl-remove-if
                         (lambda (re) (string-match-p re text))
                         (list
                          (regexp-quote "| R5 | 2026-09-10 | 2 | 0 | 1 | 1 | 0 | 0 | 1 |")
                          (regexp-quote "| R6 | 2026-09-17 | 2 | 1 | 0 | 0 | 0 | 1 | 0 |")
                          (regexp-quote "R5 Similarity | R5 AI Writing | R6 Similarity | R6 AI Writing")
                          (regexp-quote "| Adams, Alice | Lab 002, Lecture 001 | 12% | --% | 5% | *% |")
                          (regexp-quote "| Beta, Bob | Lab 003 | failed (EULA_NOT_ACCEPTED) | | none | |")
                          "^\\* Refused files"
                          (regexp-quote "| Beta, Bob | Lab 003 | R5 | failed (EULA_NOT_ACCEPTED) | yes |")
                          "^\\* Handed in, no report"
                          "| Beta, Bob | Lab 003 | R6 | <2026-09-2[01] [A-Za-z]+ [0-9:]+> | late\\.docx |"))))
          (expect missing :to-equal nil)
          (expect (string-match-p "Quiz\\|31%\\|Cyan" text) :to-be nil)))))

  (it "says None under a list with nothing in it, and keeps a bar out of a cell"
    (test-preports--with-course
      (let ((test-preports--columns (list (list "101" "A|B" nil t)))
            (test-preports--submissions
             (list (cons "101"
                         (list (test-preports--submission
                                "5001" "Adams, Alice" nil '(("11" . "a.pdf"))
                                (list (test-preports--report
                                       "originality" "Processed" "3%" "11"))))))))
        (org-canvas-submissions-reports)
        (let ((text (test-preports--text)))
          (expect text :to-match "\\* Refused files\n\n.*\n\nNone\\.")
          (expect (test-preports--flat-text) :to-match (regexp-quote "| A/B | | 1 |"))))))

  (it "marks a column it cannot read with ? and reads the rest, after one warning"
    (test-preports--with-course
      (let ((test-preports--columns test-preports--course)
            (test-preports--submissions
             (cons (cons "101" 'refused) (cdr (test-preports--course-submissions))))
            (warnings nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
          (let ((result (org-canvas-submissions-reports)))
            (expect (plist-get (car (plist-get result :columns)) :summary) :to-be nil)
            (expect (length (plist-get result :students)) :to-equal 2)))
        (expect (length warnings) :to-equal 1)
        (expect (car warnings) :to-match "reports of 'R5'")
        (expect (test-preports--flat-text) :to-match
                (regexp-quote "| R5 | 2026-09-10 | ? | ? |")))))

  (it "answers nil when the processors cannot be read"
    (test-preports--with-course
      (let ((said nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-query)
                   (lambda (&rest _) (signal 'org-canvas-api-error (list "GraphQL: nope"))))
                  ((symbol-function 'org-canvas--log-warning) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (expect (org-canvas-submissions-reports) :to-be nil)
          (expect said :to-match "Could not read which assignments")))))

  (it "says so when no assignment has a processor"
    (test-preports--with-course
      (let ((test-preports--columns (list (list "103" "Quiz" nil nil)))
            (said nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (expect (org-canvas-submissions-reports)
                  :to-equal '(:columns nil :students nil :file nil))
          (expect said :to-equal "No assignment has a document processor")))))

  (it "saves the report under the submissions directory with SAVE"
    (test-preports--with-course
      (let* ((dir (make-temp-file "org-canvas-preports-" t))
             (org-canvas-submissions-directory (expand-file-name "subs" dir))
             (test-preports--columns test-preports--course)
             (test-preports--submissions (test-preports--course-submissions)))
        (unwind-protect
            (let ((file (plist-get (org-canvas-submissions-reports t) :file)))
              (expect (file-name-directory file)
                      :to-equal (file-name-as-directory
                                 (expand-file-name "subs" dir)))
              (expect (file-exists-p (expand-file-name ".gitignore" (file-name-directory file)))
                      :to-be-truthy)
              (expect (file-name-nondirectory file)
                      :to-match "\\`document-processor-reports-[0-9-]+\\.org\\'")
              (expect (with-temp-buffer (insert-file-contents file) (buffer-string))
                      :to-equal (test-preports--text)))
          (delete-directory dir t))))))

(describe "org-canvas--processor-reports-column<"
  (it "sorts by due date, a column without one last, then by name"
    (expect (mapcar (lambda (c) (plist-get c :name))
                    (sort (list (list :name "B" :due nil)
                                (list :name "A" :due nil)
                                (list :name "Late" :due "2026-10-01")
                                (list :name "Early" :due "2026-09-01")
                                (list :name "Same2" :due "2026-09-15")
                                (list :name "Same1" :due "2026-09-15"))
                          #'org-canvas--processor-reports-column<))
            :to-equal '("Early" "Same1" "Same2" "Late" "A" "B"))))

(describe "org-canvas--processor-reports-cell"
  (it "is nil for a submission without a report connection or anything handed in"
    (expect (org-canvas--processor-reports-cell
             '((userId . "1") (ltiAssetReportsConnection . :null)))
            :to-be nil)
    (expect (org-canvas--processor-reports-cell
             (test-preports--submission "1" "X" nil nil nil))
            :to-be nil))
  (it "names a student without a sortable name by user id"
    (let ((students (make-hash-table :test 'equal)))
      (expect (plist-get (org-canvas--processor-reports-student
                          students '((userId . 77) (user . :null)))
                         :name)
              :to-equal "User 77"))))

(describe "the reports batch subcommand (issue #437)"
  (it "runs the report, saving it with --save"
    (let ((calls nil))
      (cl-letf (((symbol-function 'org-canvas-submissions-reports)
                 (lambda (&optional save) (push save calls) nil)))
        (expect (org-canvas-batch--cmd-reports '(:args nil)) :to-equal 0)
        (expect (org-canvas-batch--cmd-reports '(:args ("--save"))) :to-equal 0))
      (expect calls :to-equal '(t nil))))
  (it "refuses any other argument"
    (expect (org-canvas-batch--cmd-reports '(:args ("--all")))
            :to-throw 'org-canvas-batch-usage-error))
  (it "is listed among the subcommands"
    (expect (org-canvas-batch-parse-args '("reports" "--save"))
            :to-be-truthy)))

;;; org-canvas-submissions-reports-test.el ends here
