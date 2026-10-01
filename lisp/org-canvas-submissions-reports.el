;;; org-canvas-submissions-reports.el --- Similarity and AI Writing across columns -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A document processor (Turnitin as an LTI asset processor) files a
;; Similarity and an AI Writing report on each file handed in, and a
;; grading file carries them one column at a time (issue #351).  The
;; question an instructor asks is across columns: one 40% on one
;; recitation is a different conversation from the same student at 80%
;; on three, and a student refused on every column (the end-user
;; agreement not accepted) shows in no single grading file.  It was
;; answered by a GraphQL script and a Python pass (issue #437).
;;
;; `org-canvas-submissions-reports' is that pair as a command.  It
;; finds every assignment with a document processor, reads each one's
;; reports on the current attempt (the reader of issue #436, shared
;; with the grading file), and renders one Org report:
;;
;;   - a summary per column: handed in, then the rows processed,
;;     unscored (--%), failed, pending and without a report, counted
;;     per row as a grading file's Reports: line counts them, and the
;;     rows that carry reports on a replaced attempt;
;;   - every student, with their sections, and a Similarity and an AI
;;     Writing cell per column;
;;   - the refused files (a failed report, with its error code and
;;     whether Canvas lets the file be sent again);
;;   - the files handed in with no report.
;;
;; It is read-only in every sense: nothing is sent to Canvas, no
;; grading file is read or written, and none need exist.  The names
;; and sections come from the same GraphQL read (the submission's user
;; and enrollments), so neither people.org nor a pull is needed.  With
;; SAVE the report is also written under the submissions directory,
;; dated, beside the grading files that hold the same student data.
;; Under `noninteractive' the report is printed to standard output.
;;
;; API NOTES
;; =========
;;   POST /api/graphql (a read, `org-canvas--graphql-query')
;;       every assignment's name, due date and document processors, a
;;       page of a hundred per request (the #350 read, with the name).
;;   POST /api/graphql (a read)
;;       per column with a processor, its submissions fifty a page:
;;       the user's sortable name, the sections of their enrollments,
;;       the attempt, the current files and every report with its
;;       asset, error code and `resubmitAvailable'.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules: it shares the reports
;; reader the submissions module uses for a grading file.
(require 'org-canvas-submissions)

;; assignments.el owns the course-wide GraphQL read of #350; it is a
;; feature module, so it is declared and never required.
(declare-function org-canvas--assignment-graphql-read "org-canvas-assignments"
                  (query node-value what fallback))

(defconst org-canvas--processor-reports-buffer-name
  "*canvas-document-processor-reports*"
  "Name of the buffer the cross-column report is rendered in.")

(defconst org-canvas--processor-reports-columns-query
  "query ($courseId: ID!, $cursor: String) { course(id: $courseId) { assignmentsConnection(first: 100, after: $cursor) { pageInfo { hasNextPage endCursor } nodes { _id name dueAt ltiAssetProcessorsConnection { nodes { _id } } } } } }"
  "The GraphQL query listing every assignment with its document processors.
Checked against the Canvas schema by the GraphQL contract test.")

(defconst org-canvas--processor-reports-query
  "query ($assignmentId: ID!, $cursor: String) { assignment(id: $assignmentId) { submissionsConnection(first: 50, after: $cursor) { pageInfo { hasNextPage endCursor } nodes { userId attempt submittedAt attachments { _id displayName } user { sortableName } enrollmentsConnection { nodes { section { name } } } ltiAssetReportsConnection { nodes { reportType processingProgress result errorCode resubmitAvailable asset { attachmentId submissionAttempt discussionEntryVersion { _id } } } } } } } }"
  "The GraphQL query reading one column's reports with the students' names.
The grading file's query (`org-canvas--submissions-reports-query')
plus each student's sortable name and sections, the file names and
whether a failed file can be sent again.  Checked against the Canvas
schema by the GraphQL contract test.")

;;;; Reading

(defun org-canvas--processor-reports-column-node (node)
  "Return the column plist of the assignment NODE, or nil without a processor."
  (when (> (length (org-canvas--alist-get-non-null
                    'nodes (org-canvas--alist-get-non-null
                            'ltiAssetProcessorsConnection node)))
           0)
    (list :id (format "%s" (alist-get '_id node))
          :name (or (org-canvas--alist-get-non-null 'name node) "")
          :due (org-canvas--alist-get-non-null 'dueAt node))))

(defun org-canvas--processor-reports-column< (a b)
  "Return non-nil when column A is earlier than B, by due date, then name.
A column without a due date sorts last."
  (let ((da (plist-get a :due)) (db (plist-get b :due)))
    (cond ((and da db (not (equal da db))) (string< da db))
          ((and da (not db)) t)
          ((and db (not da)) nil)
          (t (string< (plist-get a :name) (plist-get b :name))))))

(defun org-canvas--processor-reports-columns ()
  "Return the columns with a document processor, sorted, or `refused'."
  (let ((map (org-canvas--assignment-graphql-read
              org-canvas--processor-reports-columns-query
              #'org-canvas--processor-reports-column-node
              "the document processors" "no report was read")))
    (if (not (hash-table-p map))
        'refused
      (let ((columns nil))
        (maphash (lambda (_id column) (when column (push column columns))) map)
        (sort columns #'org-canvas--processor-reports-column<)))))

(defun org-canvas--processor-reports-sections (node)
  "Return the section names of submission NODE's student, joined."
  (let ((names nil))
    (dolist (enrollment (append (org-canvas--alist-get-non-null
                                 'nodes (org-canvas--alist-get-non-null
                                         'enrollmentsConnection node))
                                nil))
      (when-let* ((name (org-canvas--alist-get-non-null
                         'name (org-canvas--alist-get-non-null
                                'section enrollment))))
        (cl-pushnew name names :test #'equal)))
    (string-join (sort names #'string<) ", ")))

(defun org-canvas--processor-reports-files (node)
  "Return the names of the files submission NODE carries now, joined."
  (string-join
   (delq nil (mapcar (lambda (file)
                       (org-canvas--alist-get-non-null 'displayName file))
                     (append (org-canvas--alist-get-non-null 'attachments node)
                             nil)))
   ", "))

(defun org-canvas--processor-reports-value (alist property)
  "Return the values of PROPERTY in ALIST, joined, or the empty string."
  (string-join (cdr (assoc property alist)) ", "))

(defun org-canvas--processor-reports-cell (node)
  "Return the cell plist of submission NODE, or nil when it has nothing.
Keys: :sim and :ai (the joined values), :values (all of them), :replaced
\(how many reports are on a replaced attempt), :resubmit (a failed
report Canvas lets the student send again), :submitted and :files.
A handed-in row with no current report reads none, the column having
a processor; a node without a report connection is nil."
  (when-let* ((split (org-canvas--submissions-node-reports node)))
    (let ((alist (org-canvas--submissions-report-alist (car split)))
          (submitted (org-canvas--alist-get-non-null 'submittedAt node)))
      (when (or alist submitted)
        (setq alist (or alist (list (list "SIMILARITY"
                                          org-canvas--submissions-report-none))))
        (list :sim (org-canvas--processor-reports-value alist "SIMILARITY")
              :ai (org-canvas--processor-reports-value alist "AI_WRITING")
              :values (apply #'append (mapcar #'cdr alist))
              :replaced (length (cdr split))
              :resubmit (cl-some (lambda (r) (eq (alist-get 'resubmitAvailable r) t))
                                 (car split))
              :submitted submitted
              :files (org-canvas--processor-reports-files node))))))

(defun org-canvas--processor-reports-student (students node)
  "Return the student plist of submission NODE in STUDENTS, adding it."
  (let ((uid (format "%s" (alist-get 'userId node))))
    (or (gethash uid students)
        (puthash uid
                 (list :id uid
                       :name (or (org-canvas--alist-get-non-null
                                  'sortableName
                                  (org-canvas--alist-get-non-null 'user node))
                                 (format "User %s" uid))
                       :sections (org-canvas--processor-reports-sections node)
                       :cells nil)
                 students))))

(defun org-canvas--processor-reports-read-column (column students)
  "Read COLUMN's reports into STUDENTS; return its cells, or `unreadable'.
Each student with a cell gains (COLUMN-ID . CELL) under :cells.  A
failed read is one warning; the other columns are still read."
  (condition-case err
      (let ((cells nil))
        (org-canvas--graphql-walk-pages
         org-canvas--processor-reports-query
         (list (cons 'assignmentId (plist-get column :id)))
         '(assignment submissionsConnection)
         (lambda (node _map)
           (when-let* (((org-canvas--alist-get-non-null 'userId node))
                       (cell (org-canvas--processor-reports-cell node))
                       (student (org-canvas--processor-reports-student
                                 students node)))
             (push cell cells)
             (plist-put student :cells
                        (cons (cons (plist-get column :id) cell)
                              (plist-get student :cells))))))
        cells)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Reports] Could not read the reports of '%s' (%s)"
       (plist-get column :name) (error-message-string err))
     'unreadable)))

(defun org-canvas--processor-reports-summary (cells)
  "Return the summary plist of a column's CELLS, or nil when `unreadable'.
Keys: :handed-in, :counts (`org-canvas--submissions-report-counts')
and :replaced, the rows carrying a report on a replaced attempt."
  (unless (eq cells 'unreadable)
    (list :handed-in (cl-count-if (lambda (c) (plist-get c :submitted)) cells)
          :counts (org-canvas--submissions-report-counts
                   (mapcar (lambda (c) (plist-get c :values)) cells))
          :replaced (cl-count-if (lambda (c) (> (plist-get c :replaced) 0))
                                 cells))))

(defun org-canvas--processor-reports-read (columns)
  "Read the reports of COLUMNS; return (COLUMNS . STUDENTS).
COLUMNS come back with :summary added; STUDENTS is a list of student
plists sorted by name."
  (let ((students (make-hash-table :test 'equal))
        (read nil)
        (rows nil))
    (dolist (column columns)
      (message "Reading the reports of %s..." (plist-get column :name))
      (push (append column
                    (list :summary (org-canvas--processor-reports-summary
                                    (org-canvas--processor-reports-read-column
                                     column students))))
            read))
    (maphash (lambda (_uid student) (push student rows)) students)
    (cons (nreverse read)
          (sort rows (lambda (a b) (string< (downcase (plist-get a :name))
                                            (downcase (plist-get b :name))))))))

;;;; Rendering

(defun org-canvas--processor-reports-text (value)
  "Return VALUE as table cell text: a string with no vertical bar."
  (replace-regexp-in-string "|" "/" (format "%s" (or value ""))))

(defun org-canvas--processor-reports-row (cells)
  "Return the Org table row of CELLS, a list of values."
  (concat "| " (mapconcat #'org-canvas--processor-reports-text cells " | ")
          " |\n"))

(defun org-canvas--processor-reports-table (header rows)
  "Insert an Org table of HEADER and ROWS, lists of values, and align it."
  (let ((start (point)))
    (insert (org-canvas--processor-reports-row header))
    (insert "|" (mapconcat (lambda (_) "---") header "+") "|\n")
    (dolist (row rows)
      (insert (org-canvas--processor-reports-row row)))
    (save-excursion
      (goto-char start)
      (org-table-align))))

(defun org-canvas--processor-reports-summary-row (column)
  "Return the summary table row of COLUMN."
  (let* ((summary (plist-get column :summary))
         (counts (plist-get summary :counts)))
    (append (list (plist-get column :name)
                  (if (plist-get column :due)
                      (substring (plist-get column :due) 0 10)
                    ""))
            (if (not summary)
                (make-list 7 "?")
              (cons (plist-get summary :handed-in)
                    (append (mapcar (lambda (key) (or (plist-get counts key) 0))
                                    org-canvas--submissions-report-count-keys)
                            (list (plist-get summary :replaced))))))))

(defun org-canvas--processor-reports-cell-of (student column)
  "Return STUDENT's cell plist on COLUMN, or nil."
  (cdr (assoc (plist-get column :id) (plist-get student :cells))))

(defun org-canvas--processor-reports-student-row (student columns)
  "Return STUDENT's row of the every-student table over COLUMNS."
  (append (list (plist-get student :name) (plist-get student :sections))
          (mapcan (lambda (column)
                    (let ((cell (org-canvas--processor-reports-cell-of
                                 student column)))
                      (list (plist-get cell :sim) (plist-get cell :ai))))
                  columns)))

(defun org-canvas--processor-reports-listed (columns students pred row-fn)
  "Return a row per student and column whose cell satisfies PRED.
ROW-FN is called with the student, the column and the cell, and
returns the row.  STUDENTS are in name order, COLUMNS in theirs."
  (let ((rows nil))
    (dolist (student students)
      (dolist (column columns)
        (let ((cell (org-canvas--processor-reports-cell-of student column)))
          (when (and cell (funcall pred cell))
            (push (funcall row-fn student column cell) rows)))))
    (nreverse rows)))

(defun org-canvas--processor-reports-refused-p (cell)
  "Return non-nil when a report of CELL failed or was not processed."
  (cl-some #'org-canvas--submissions-report-failed-p (plist-get cell :values)))

(defun org-canvas--processor-reports-unreported-p (cell)
  "Return non-nil when CELL was handed in with no report."
  (member org-canvas--submissions-report-none (plist-get cell :values)))

(defun org-canvas--processor-reports-refused-row (student column cell)
  "Return the refused-files row of STUDENT's CELL on COLUMN."
  (list (plist-get student :name) (plist-get student :sections)
        (plist-get column :name) (plist-get cell :sim)
        (if (plist-get cell :resubmit) "yes" "no")))

(defun org-canvas--processor-reports-unreported-row (student column cell)
  "Return the no-report row of STUDENT's CELL on COLUMN."
  (list (plist-get student :name) (plist-get student :sections)
        (plist-get column :name)
        (or (org-canvas--iso8601-to-org-timestamp (plist-get cell :submitted)) "")
        (plist-get cell :files)))

(defun org-canvas--processor-reports-insert-list (title note header rows)
  "Insert the section TITLE with NOTE, and a table of HEADER and ROWS.
With no ROWS the section says so instead of showing an empty table."
  (insert "\n* " title "\n\n" note "\n\n")
  (if rows
      (org-canvas--processor-reports-table header rows)
    (insert "None.\n")))

(defconst org-canvas--processor-reports-legend
  "- =37%= :: the tool's own number.  Similarity is the share of the text matching other sources; AI Writing the share of qualifying prose its detector attributes to AI.
- =*%= :: AI Writing between 1 and 19 percent, a band Turnitin does not print.
- =--%= :: AI Writing could not score the file (too little qualifying prose); counted as unscored.
- =failed (CODE)= :: the file was refused; =EULA_NOT_ACCEPTED= means the student has not accepted Turnitin's agreement.
- =none= :: a file was handed in and no report was filed.
- =pending= :: the report has not finished.  A blank cell: nothing handed in."
  "How to read a cell, printed under the report's title.")

(defun org-canvas--processor-reports-render (columns students)
  "Render the report of COLUMNS and STUDENTS into the current buffer."
  (org-mode)
  (insert (format "#+TITLE: Document processor reports, course %s, read %s\n\n"
                  org-canvas-course-id
                  (format-time-string "<%Y-%m-%d %a %H:%M>")))
  (insert "Student data.  Each cell is the report on the student's current"
          " attempt; reports on a replaced attempt are left out and counted"
          " in the summary.  Read-only: nothing was written to Canvas or to"
          " a grading file.\n\n"
          org-canvas--processor-reports-legend "\n\n* Summary by column\n\n")
  (org-canvas--processor-reports-table
   '("Column" "Due" "Handed in" "Processed" "Unscored" "Failed" "Pending"
     "No report" "Replaced attempts")
   (mapcar #'org-canvas--processor-reports-summary-row columns))
  (insert "\nA student counts once per column, in the first of failed,"
          " pending, no report, unscored and processed that applies;"
          " ? = the column could not be read.\n\n* Every student\n\n")
  (org-canvas--processor-reports-table
   (append '("Student" "Section")
           (mapcan (lambda (c) (list (concat (plist-get c :name) " Similarity")
                                     (concat (plist-get c :name) " AI Writing")))
                   columns))
   (mapcar (lambda (s) (org-canvas--processor-reports-student-row s columns))
           students))
  (org-canvas--processor-reports-insert-list
   "Refused files"
   "A failed report, with the code Canvas gives.  Resubmit: whether Canvas lets the file be sent again."
   '("Student" "Section" "Column" "Similarity" "Resubmit")
   (org-canvas--processor-reports-listed
    columns students #'org-canvas--processor-reports-refused-p
    #'org-canvas--processor-reports-refused-row))
  (org-canvas--processor-reports-insert-list
   "Handed in, no report"
   "A submission with no report on its current attempt."
   '("Student" "Section" "Column" "Submitted" "Files")
   (org-canvas--processor-reports-listed
    columns students #'org-canvas--processor-reports-unreported-p
    #'org-canvas--processor-reports-unreported-row)))

(defun org-canvas--processor-reports-save (text)
  "Write TEXT to a dated report file under the submissions directory.
The directory is made as a pull makes it, its .gitignore included,
since the report names students.  Return the path."
  (let ((file (expand-file-name
               (format-time-string "document-processor-reports-%Y-%m-%d.org")
               (org-canvas--submissions-ensure-directory))))
    (with-temp-file file (insert text))
    file))

;;;; Entry Point

;;;###autoload
(defun org-canvas-submissions-reports (&optional save)
  "Show every column's Similarity and AI Writing reports, one row per student.
Every assignment with a document processor is a column: a summary of
each column's counts, then each student with their sections and a
Similarity and an AI Writing cell per column, then the refused files
and the files handed in with no report.  Only the reports on each
submission's current attempt are shown.  Reads only: nothing is sent
to Canvas and no grading file is read or written.  With SAVE (a prefix
argument) the report is also written, dated, under the submissions
directory.  Under `noninteractive' it is printed to standard output.
Return a plist of :columns (with their :summary), :students and :file
\(the saved path, or nil); nil when the processors could not be read."
  (interactive "P")
  (let ((columns (org-canvas--processor-reports-columns)))
    (cond
     ((eq columns 'refused)
      (message "Could not read which assignments have a document processor; see the log")
      nil)
     ((null columns)
      (message "No assignment has a document processor")
      (list :columns nil :students nil :file nil))
     (t (org-canvas--processor-reports-show
         (org-canvas--processor-reports-read columns) save)))))

(defun org-canvas--processor-reports-show (read save)
  "Render READ, (COLUMNS . STUDENTS), saving it when SAVE; return the plist."
  (let* ((columns (car read))
         (students (cdr read))
         (text (org-canvas--report-display
                org-canvas--processor-reports-buffer-name
                (lambda () (org-canvas--processor-reports-render
                            columns students))))
         (file (and save (org-canvas--processor-reports-save text))))
    (message "Document processor reports: %d column(s), %d student(s)%s"
             (length columns) (length students)
             (if file (format "; saved to %s" file) ""))
    (list :columns columns :students students :file file)))

(provide 'org-canvas-submissions-reports)
;;; org-canvas-submissions-reports.el ends here
