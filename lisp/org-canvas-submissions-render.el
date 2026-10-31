;;; org-canvas-submissions-render.el --- Render a grading file: summary table, detail view, attempts -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A grading file is data: headings, properties, tables and items.
;; This module turns it into the two views the minor mode switches
;; between (issue #142's split of the submissions module):
;;
;; Summary: a read-only table, one row per student, derived from the
;;          headings so it shows unpushed edits; `v' returns.
;; Detail:  the grading file itself, one heading per student, written
;;          by `org-canvas--submissions-render-detail' from the
;;          submissions and the data already in the file.
;;
;; It also carries what the summary and the detail rows report about
;; a column: the document processor reports of issue #351, the
;; statistics the summary prints, every user's name in the course's
;; sortable order, scores as the instructor reads them, and the
;; attempt history of one student (issue #352).

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; The grading file, its buffer-local state and the minor mode it
;; is read in: this module is above that file, never required by it.
(require 'org-canvas-submissions)

;; Defined below this file (a sibling split off the same module), so
;; declared rather than required: requiring would be a cycle.
(declare-function org-canvas--submissions-assessment "org-canvas-submissions-comment-items"
                  (submission))
(declare-function org-canvas--submissions-assessment-triples "org-canvas-submissions-comment-items"
                  (assessment))
(declare-function org-canvas--submissions-insert-comment-baseline "org-canvas-submissions-comment-items"
                  (comments))
(declare-function org-canvas--submissions-insert-late-status "org-canvas-submissions-push"
                  (submission))
(declare-function org-canvas--submissions-item "org-canvas-submissions-comment-items"
                  (label text))
(declare-function org-canvas--submissions-left-p "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas--submissions-render-bank-heading "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas--submissions-render-rubric "org-canvas-submissions-comment-items"
                  (criteria assessment))
(declare-function org-canvas--submissions-rubric-criteria "org-canvas-submissions-comment-items"
                  (assignment))
(declare-function org-canvas--submissions-rubric-digest "org-canvas-submissions-comment-items"
                  (triples))

;;;; Document Processor Reports (issue #351)

;; A document processor (Turnitin as an LTI 1.3 asset processor, #184)
;; files a Similarity and an AI Writing report on each upload.  REST
;; does not carry them; GraphQL does, as each submission's
;; `ltiAssetReportsConnection'.  Two look-alikes answer something else:
;; `Assignment.hasPlagiarismTool' is the legacy plagiarism framework
;; (false on a column with a processor attached) and
;; `Submission.turnitinData' the legacy plugin's.  One query per column
;; reads them all; each row gets a property per report type, and the
;; header a count line.  A column without a processor answers no
;; reports, and then nothing is written.
;;
;; The connection answers every attempt's reports, so only those on the
;; current attempt are kept (issue #436): a report on a file the
;; submission no longer carries belongs to a replaced attempt.  A
;; failed report carries its `errorCode', and a handed-in row with no
;; report on a column that has a processor reads none.

(defconst org-canvas--submissions-reports-query
  "query ($assignmentId: ID!, $cursor: String) { assignment(id: $assignmentId) { submissionsConnection(first: 100, after: $cursor) { pageInfo { hasNextPage endCursor } nodes { userId attempt submittedAt attachments { _id } ltiAssetReportsConnection { nodes { reportType processingProgress result errorCode asset { attachmentId submissionAttempt discussionEntryVersion { _id } } } } } } } }"
  "The GraphQL query that reads a column's document processor reports.
One page of submissions per request, each with its user id, attempt,
hand-in time, current attachments and every report with the asset it
is about.  Checked against the Canvas schema by the GraphQL contract
test (issue #269), which names it by this symbol.")

(defconst org-canvas--submissions-processors-query
  "query ($assignmentId: ID!) { assignment(id: $assignmentId) { ltiAssetProcessorsConnection { nodes { _id } } } }"
  "The GraphQL query asking whether one assignment has a document processor.
Asked only when a handed-in row has no report, to tell a missing
report from a column that never files one (issue #436).  Checked by
the GraphQL contract test.")

(defconst org-canvas--submissions-report-none "none"
  "The SIMILARITY value of a handed-in row with no report (issue #436).")

(defconst org-canvas--submissions-report-unscored "--%"
  "The result Turnitin gives a file it could not score (issue #436).")

(defconst org-canvas--submissions-report-count-keys
  '(:processed :unscored :failed :pending :none)
  "The buckets a row is counted in, in the order the Reports: line names them.")

(defconst org-canvas--submissions-report-properties
  '(("originality" . "SIMILARITY")
    ("turnitin_aiwriting" . "AI_WRITING"))
  "The property each known `reportType' is written to on a row.
Any other type is written to REPORT_ and its name, upcased.")

(defconst org-canvas--submissions-report-progress
  '(("Processed" . processed) ("Failed" . failed)
    ("NotProcessed" . not-processed)
    ("Pending" . pending) ("Processing" . pending)
    ("PendingManual" . pending) ("NotReady" . pending))
  "The LTI Asset Processor `processingProgress' values, by what they mean.
A value not listed here reads as pending: the report exists and has
nothing final to say.")

(defun org-canvas--submissions-report-property (type)
  "Return the row property the report type TYPE is written to, or nil.
Nil for a missing or blank type.  An unknown type becomes REPORT_ and
its name upcased, every run of other characters one underscore, so a
second processor's report still lands somewhere a grader can see."
  (when (and (stringp type) (string-match-p "[[:alnum:]]" type))
    (or (cdr (assoc type org-canvas--submissions-report-properties))
        (concat "REPORT_"
                (upcase (string-trim
                         (replace-regexp-in-string "[^[:alnum:]]+" "_" type)
                         "_+" "_+"))))))

(defun org-canvas--submissions-report-error-suffix (report)
  "Return \" (CODE)\" for REPORT's `errorCode', or the empty string.
A comma or parenthesis in the code becomes a space, so the value still
splits at its commas when a grading file is read back."
  (let ((code (org-canvas--alist-get-non-null 'errorCode report)))
    (if (and (stringp code) (string-match-p "[[:alnum:]]" code))
        (format " (%s)" (string-trim (replace-regexp-in-string
                                      "[,()[:space:]]+" " " code)))
      "")))

(defun org-canvas--submissions-report-value (report)
  "Return the value one REPORT, a GraphQL report node, is written as.
A processed report gives its result (\"33%\"), or processed when it has
none; a failed one failed; one the tool declined, not processed; any
other, pending.  Failed and not processed carry the report's error
code in parentheses when Canvas sends one (issue #436)."
  (let ((result (org-canvas--alist-get-non-null 'result report)))
    (pcase (cdr (assoc (alist-get 'processingProgress report)
                       org-canvas--submissions-report-progress))
      ('processed (if (and (stringp result) (not (string-empty-p result)))
                      result
                    "processed"))
      ('failed (concat "failed"
                       (org-canvas--submissions-report-error-suffix report)))
      ('not-processed (concat "not processed"
                              (org-canvas--submissions-report-error-suffix
                               report)))
      (_ "pending"))))

(defun org-canvas--submissions-report-alist (reports)
  "Return REPORTS as ((PROPERTY . VALUES) ...), in the order they came.
REPORTS are one submission's report nodes; a type reported twice (a
report per uploaded file of the attempt) keeps both values, oldest
first."
  (let ((alist nil))
    (dolist (report (append reports nil))
      (when-let* ((property (org-canvas--submissions-report-property
                             (alist-get 'reportType report))))
        (let ((cell (assoc property alist))
              (value (org-canvas--submissions-report-value report)))
          (if cell
              (setcdr cell (append (cdr cell) (list value)))
            (push (list property value) alist)))))
    (nreverse alist)))

(defun org-canvas--submissions-node-attachment-ids (node)
  "Return the ids of the files submission NODE carries now, as strings."
  (mapcar (lambda (file) (format "%s" (alist-get '_id file)))
          (append (org-canvas--alist-get-non-null 'attachments node) nil)))

(defun org-canvas--submissions-report-current-p (report attempt attachment-ids)
  "Return non-nil when REPORT is about the submission's current attempt.
ATTEMPT is the submission's attempt number and ATTACHMENT-IDS the ids
of the files it carries now.  The rule of canvas-lms's own `latest'
filter: a discussion entry's report is current (Canvas sends only the
latest version's), a file's report is current when the file is still
on the submission or its asset names this attempt, and any other
asset is current unless it names another attempt."
  (let* ((asset (org-canvas--alist-get-non-null 'asset report))
         (attachment (org-canvas--alist-get-non-null 'attachmentId asset))
         (asset-attempt (org-canvas--alist-get-non-null
                         'submissionAttempt asset)))
    (cond ((org-canvas--alist-get-non-null 'discussionEntryVersion asset) t)
          ((and asset-attempt (equal asset-attempt attempt)) t)
          (attachment (and (member (format "%s" attachment) attachment-ids) t))
          (t (not (and asset-attempt attempt))))))

(defun org-canvas--submissions-node-reports (node)
  "Return submission NODE's reports as (CURRENT . REPLACED), or nil.
CURRENT are the reports on the current attempt and REPLACED the rest,
two lists in the order Canvas sent them.  Nil when the node carries
no report connection at all, which is how Canvas answers a submission
it runs no processor on (a discussion without the account flag)."
  (when-let* ((connection (org-canvas--alist-get-non-null
                           'ltiAssetReportsConnection node)))
    (let ((attempt (org-canvas--alist-get-non-null 'attempt node))
          (ids (org-canvas--submissions-node-attachment-ids node))
          (current nil)
          (replaced nil))
      (dolist (report (append (org-canvas--alist-get-non-null 'nodes connection)
                              nil))
        (if (org-canvas--submissions-report-current-p report attempt ids)
            (push report current)
          (push report replaced)))
      (cons (nreverse current) (nreverse replaced)))))

(defun org-canvas--submissions-node-report-alist (node)
  "Return submission NODE's current-attempt report alist, or a marker.
The alist of `org-canvas--submissions-report-alist'; the symbol
`unreported' when NODE was handed in and has no current report, which
`org-canvas--submissions-settle-unreported' turns into none or
nothing; nil otherwise."
  (when-let* ((split (org-canvas--submissions-node-reports node)))
    (or (org-canvas--submissions-report-alist (car split))
        (and (org-canvas--alist-get-non-null 'submittedAt node)
             'unreported))))

(defun org-canvas--submissions-reports-node (node map)
  "Store submission NODE's report alist in MAP under its user id."
  (let ((user-id (org-canvas--alist-get-non-null 'userId node))
        (reports (org-canvas--submissions-node-report-alist node)))
    (when (and user-id reports)
      (puthash (format "%s" user-id) reports map))))

(defun org-canvas--submissions-column-has-processor-p (assignment-id)
  "Return non-nil when ASSIGNMENT-ID has a document processor attached.
A failed read is one warning and nil: the rows without a report are
then left without the property, as before issue #436."
  (condition-case err
      (let* ((data (org-canvas--graphql-query
                    org-canvas--submissions-processors-query
                    (list (cons 'assignmentId (format "%s" assignment-id)))))
             (connection (org-canvas--alist-get-non-null
                          'ltiAssetProcessorsConnection
                          (org-canvas--alist-get-non-null 'assignment data))))
        (> (length (org-canvas--alist-get-non-null 'nodes connection)) 0))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       (concat "[Submissions] Could not tell whether assignment %s has a"
               " document processor (%s); rows without a report left blank")
       assignment-id (error-message-string err))
     nil)))

(defun org-canvas--submissions-settle-unreported (assignment-id processor map)
  "Resolve MAP's `unreported' rows for ASSIGNMENT-ID, and return MAP.
On a column with a processor each becomes ((\"SIMILARITY\" \"none\"));
on one without, it is dropped, so the row stays silent.  PROCESSOR
non-nil says the column has one; nil asks Canvas, once, and only when
some row is unreported."
  (let ((unreported nil))
    (maphash (lambda (uid reports)
               (when (eq reports 'unreported) (push uid unreported)))
             map)
    (when unreported
      (let ((none (or processor
                      (org-canvas--submissions-column-has-processor-p
                       assignment-id))))
        (dolist (uid unreported)
          (if none
              (puthash uid (list (list "SIMILARITY"
                                       org-canvas--submissions-report-none))
                       map)
            (remhash uid map)))))
    map))

(defun org-canvas--submissions-fetch-reports (assignment-id &optional processor)
  "Return ASSIGNMENT-ID's document processor reports by user id, or nil.
The value is a hash from user id (a string) to the report alist of
`org-canvas--submissions-report-alist', current attempt only, followed
page by page; a handed-in row with no report reads none when the
column has a processor (PROCESSOR non-nil says it has; nil asks).  A
failed read is one warning and nil, the same as a column with no
processor: the pull goes on without the reports."
  (condition-case err
      (org-canvas--submissions-settle-unreported
       assignment-id processor
       (org-canvas--graphql-walk-pages
        (org-canvas--graphql-request
         :query org-canvas--submissions-reports-query
         :variables (list (cons 'assignmentId (format "%s" assignment-id)))
         :connection-path '(assignment submissionsConnection))
        #'org-canvas--submissions-reports-node))
    (error
     (org-canvas--log-warning org-canvas--logger
       (concat "[Submissions] Could not read the document processor reports"
               " of assignment %s (%s); pulled without them")
       assignment-id (error-message-string err))
     nil)))

(defun org-canvas--submissions-submitted-any-p (submissions)
  "Return non-nil when any of SUBMISSIONS was handed in."
  (cl-some (lambda (s)
             (stringp (org-canvas--alist-get-non-null 'submitted_at s)))
           submissions))

(defun org-canvas--submissions-with-reports (assignment-id submissions)
  "Return SUBMISSIONS with ASSIGNMENT-ID's reports attached to their rows.
Each row with a report gains an `org-canvas-reports' entry, the alist
of `org-canvas--submissions-report-alist'.  A column where nothing
was handed in has no report to read, and is returned without a
request."
  (let ((map (and (org-canvas--submissions-submitted-any-p submissions)
                  (org-canvas--submissions-fetch-reports assignment-id))))
    (if (not (and map (> (hash-table-count map) 0)))
        submissions
      (org-canvas--log-info org-canvas--logger "[Submissions] %s"
        (org-canvas--submissions-map-reports-line map))
      (mapcar (lambda (sub)
                (let* ((uid (org-canvas--submissions-user-id sub))
                       (reports (and uid (gethash (format "%s" uid) map))))
                  (if reports
                      (cons (cons 'org-canvas-reports reports) sub)
                    sub)))
              submissions))))

(defun org-canvas--submissions-fetch-with-reports (assignment-id)
  "Fetch ASSIGNMENT-ID's submissions with their reports attached.
The pull's and the refresh's one read of the column: the REST
submissions, then the GraphQL reports keyed onto them."
  (org-canvas--submissions-with-reports
   assignment-id (org-canvas--submissions-fetch-for-assignment assignment-id)))

(defun org-canvas--submissions-insert-report-properties (submission)
  "Insert one property line per report type SUBMISSION carries.
Several reports of one type are joined with a comma."
  (dolist (cell (alist-get 'org-canvas-reports submission))
    (insert (format ":%s: %s\n" (car cell) (string-join (cdr cell) ", ")))))

(defun org-canvas--submissions-report-failed-p (value)
  "Return non-nil when the report VALUE is a failure.
Failed or not processed, with or without its error code."
  (string-match-p "\\`\\(?:failed\\|not processed\\)\\(?: (\\|\\'\\)" value))

(defun org-canvas--submissions-report-bucket (values)
  "Return the count a row with report VALUES falls in, or nil.
Failed when any report failed or was not processed, else pending when
any is pending, else none when the row has no report on a column with
a processor, else unscored when a report answered --% (the tool could
not score the file), else processed; nil when VALUES is empty."
  (cond ((null values) nil)
        ((cl-some #'org-canvas--submissions-report-failed-p values) :failed)
        ((member "pending" values) :pending)
        ((member org-canvas--submissions-report-none values) :none)
        ((member org-canvas--submissions-report-unscored values) :unscored)
        (t :processed)))

(defun org-canvas--submissions-report-counts (rows)
  "Return the report counts of ROWS, a plist, or nil.
The keys are `org-canvas--submissions-report-count-keys'.  ROWS holds
each row's report values, a list of strings; a row counts once, in
the bucket of `org-canvas--submissions-report-bucket'.  Nil when no
row has a report, so a column without a processor says nothing."
  (let ((counts (mapcan (lambda (key) (list key 0))
                        org-canvas--submissions-report-count-keys))
        (any nil))
    (dolist (values rows)
      (when-let* ((bucket (org-canvas--submissions-report-bucket values)))
        (setq any t)
        (plist-put counts bucket (1+ (plist-get counts bucket)))))
    (and any counts)))

(defun org-canvas--submissions-format-report-counts (counts)
  "Return the Reports: line for COUNTS, without its newline, or nil.
Unscored rows and rows without a report are named only when there
are some, so a column with neither reads as it did before issue #436."
  (when counts
    (let ((unscored (or (plist-get counts :unscored) 0))
          (none (or (plist-get counts :none) 0)))
      (concat
       (format "Reports: %d processed, %d failed, %d pending"
               (plist-get counts :processed) (plist-get counts :failed)
               (plist-get counts :pending))
       (if (> unscored 0) (format ", %d unscored" unscored) "")
       (if (> none 0) (format ", %d without a report" none) "")))))

(defun org-canvas--submissions-map-report-counts (map)
  "Return the report counts of MAP, the hash of the reports fetch, or nil."
  (let ((rows nil))
    (when map
      (maphash (lambda (_uid reports)
                 (push (apply #'append (mapcar #'cdr reports)) rows))
               map))
    (org-canvas--submissions-report-counts rows)))

(defun org-canvas--submissions-map-reports-line (map)
  "Return the Reports: line for MAP, the hash of the reports fetch, or nil."
  (org-canvas--submissions-format-report-counts
   (org-canvas--submissions-map-report-counts map)))

(defun org-canvas--submissions-report-values (submission)
  "Return every report value SUBMISSION carries, as one list."
  (apply #'append (mapcar #'cdr (alist-get 'org-canvas-reports submission))))

(defun org-canvas--submissions-reports-line (submissions)
  "Return the Reports: line for SUBMISSIONS, or nil when none has one."
  (org-canvas--submissions-format-report-counts
   (org-canvas--submissions-report-counts
    (mapcar #'org-canvas--submissions-report-values submissions))))

(defun org-canvas--submissions-report-values-at-point ()
  "Return the report values the heading at point records, as one list.
Read from SIMILARITY, AI_WRITING and every REPORT_ property; a value
holding several reports is split at its commas."
  (let ((values nil))
    (dolist (prop (org-entry-properties (point) 'standard))
      (when (or (rassoc (car prop) org-canvas--submissions-report-properties)
                (string-prefix-p "REPORT_" (car prop)))
        (setq values (append values (split-string (cdr prop) ", *" t)))))
    values))

;;;; Status Normalization

(defun org-canvas--submissions-normalize-status (submission)
  "Derive a status symbol from SUBMISSION alist.
Returns one of: submitted, late, missing, graded, pending_review,
unsubmitted, or left for the stand-in a departed student's kept
heading is rendered from (`org-canvas--submissions-departed-entries')."
  (let ((state (alist-get 'workflow_state submission))
        (late (alist-get 'late submission))
        (missing (alist-get 'missing submission)))
    (cond
     ((alist-get 'org-canvas-left submission) 'left)
     ((and missing (not (eq missing :json-false))) 'missing)
     ((and late (not (eq late :json-false))) 'late)
     ((equal state "graded") 'graded)
     ((equal state "submitted") 'submitted)
     ((equal state "pending_review") 'pending_review)
     (t 'unsubmitted))))

;;;; Statistics

(defun org-canvas--submissions-compute-stats (submissions)
  "Compute summary statistics from SUBMISSIONS list.
Returns a plist with :submitted :missing :late :graded :average :total :points."
  (let ((submitted 0) (missing 0) (late 0) (graded 0)
        (scores nil) (total (length submissions))
        (points nil))
    (dolist (sub submissions)
      (pcase (org-canvas--submissions-normalize-status sub)
        ('submitted (cl-incf submitted))
        ('missing (cl-incf missing))
        ('late (cl-incf late))
        ('graded (cl-incf graded)))
      (let ((score (alist-get 'score sub)))
        (when (and score (numberp score))
          (push score scores)))
      (unless points
        (let ((pts (alist-get 'points_possible (alist-get 'assignment sub))))
          (when (and pts (numberp pts))
            (setq points pts)))))
    (list :submitted submitted :missing missing :late late :graded graded
          :total total :points points
          :average (when scores
                     (/ (apply #'+ scores) (float (length scores)))))))

(defun org-canvas--submissions-format-stats (stats)
  "Format STATS plist as a summary line string."
  (let ((parts nil))
    (when (> (plist-get stats :graded) 0)
      (push (format "%d graded" (plist-get stats :graded)) parts))
    (when (> (plist-get stats :late) 0)
      (push (format "%d late" (plist-get stats :late)) parts))
    (when (> (plist-get stats :missing) 0)
      (push (format "%d missing" (plist-get stats :missing)) parts))
    (push (format "%d submitted" (plist-get stats :submitted)) parts)
    (let ((line (string-join (nreverse parts) " | ")))
      (when (plist-get stats :average)
        (setq line (concat line
                           (format " | Average: %.1f" (plist-get stats :average))
                           (if (plist-get stats :points)
                               (format "/%s" (plist-get stats :points))
                             ""))))
      line)))

;;;; User Name Formatting

(defun org-canvas--submissions-user-sortable-name (submission)
  "Extract sortable name from SUBMISSION's user data.
Falls back to `name', then to \"User <id>\" built from the submission's
own `user_id' so rows stay distinct when the user include is missing,
and to \"Unknown\" only when there is no id at all."
  (let ((user (alist-get 'user submission))
        (uid (org-canvas--submissions-user-id submission)))
    (or (alist-get 'sortable_name user)
        (alist-get 'name user)
        (and uid (format "User %s" uid))
        "Unknown")))

(defun org-canvas--submissions-user-id (submission)
  "Extract user ID from SUBMISSION.
Prefers the included user object and falls back to the submission's
top-level `user_id', which Canvas returns without any include."
  (or (alist-get 'id (alist-get 'user submission))
      (alist-get 'user_id submission)))

;;;; Score Formatting

(defun org-canvas--submissions-format-score (submission)
  "Format score from SUBMISSION as \"score/points\" or empty string."
  (let ((score (alist-get 'score submission))
        (points (alist-get 'points_possible
                           (alist-get 'assignment submission))))
    (cond
     ((and score (numberp score) points (numberp points))
      (format "%s/%s" (org-canvas--submissions-format-number score)
              (org-canvas--submissions-format-number points)))
     ((and score (numberp score))
      (org-canvas--submissions-format-number score))
     (t ""))))

(defun org-canvas--submissions-format-number (n)
  "Format number N, dropping .0 for integers."
  (if (= n (truncate n))
      (format "%d" (truncate n))
    (format "%.1f" n)))

;;;; Summary View Rendering

(defun org-canvas--submissions-render-summary (assignment-name assignment-id submissions)
  "Render summary table view into current buffer.
ASSIGNMENT-NAME and ASSIGNMENT-ID identify the assignment.
SUBMISSIONS is the list of submission alists."
  (let* ((stats (org-canvas--submissions-compute-stats submissions))
         (sorted (sort (copy-sequence submissions)
                       (lambda (a b)
                         (string< (org-canvas--submissions-user-sortable-name a)
                                  (org-canvas--submissions-user-sortable-name b))))))
    (erase-buffer)
    (insert (format "#+TITLE: Submissions: %s\n" assignment-name))
    (insert (format "#+PROPERTY: CANVAS_ASSIGNMENT_ID %s\n\n" assignment-id))
    (insert (org-canvas--submissions-format-stats stats))
    (when-let* ((reports (org-canvas--submissions-reports-line submissions)))
      (insert "\n" reports))
    (insert "\n\n")
    (insert "| Student | Status | Submitted At | Score |\n")
    (insert "|---------+--------+--------------+-------|\n")
    (dolist (sub sorted)
      (let ((name (org-canvas--submissions-user-sortable-name sub))
            (status (symbol-name (org-canvas--submissions-normalize-status sub)))
            (timestamp (org-canvas--submissions-format-submitted-at sub))
            (score (org-canvas--submissions-format-score sub)))
        (insert (format "| %s | %s | %s | %s |\n" name status timestamp score))))
    (org-table-align)))

(defun org-canvas--submissions-format-submitted-at (submission)
  "Format submitted_at timestamp from SUBMISSION."
  (let ((ts (alist-get 'submitted_at submission)))
    (or (org-canvas--iso8601-to-org-timestamp ts) "")))

;;;; Detail View Rendering

(defvar org-canvas--submissions-comment-cache nil
  "Hash from a comment's text as Canvas holds it to its grading-file text.
Bound around a pull's render and a push's read of the sent comments,
where each comment is converted for its item and again for its
CANVAS_COMMENTS digest; nil converts every time.")

(defconst org-canvas--submissions-comments-heading "** Comments"
  "Heading under which a student's sent comments are listed.")

(defun org-canvas--submissions-render-detail (assignment-name assignment-id submissions &optional assignment)
  "Render detail view with per-student headings into current buffer.
ASSIGNMENT-NAME and ASSIGNMENT-ID identify the assignment.
SUBMISSIONS is the list of submission alists.  ASSIGNMENT, the Canvas
assignment object when at hand, supplies the rubric header."
  (let ((sorted (sort (copy-sequence submissions)
                      (lambda (a b)
                        (string< (org-canvas--submissions-user-sortable-name a)
                                 (org-canvas--submissions-user-sortable-name b))))))
    (erase-buffer)
    (insert (format "#+TITLE: Submissions: %s\n" assignment-name))
    (insert (format "#+PROPERTY: CANVAS_ASSIGNMENT_ID %s\n" assignment-id))
    (insert (format "#+PROPERTY: CANVAS_ASSIGNMENT_NAME %s\n" assignment-name))
    (insert (format "#+PROPERTY: PULLED_AT %s\n"
                    (format-time-string "<%Y-%m-%d %a %H:%M>")))
    (org-canvas--submissions-render-rubric-properties assignment)
    (org-canvas--submissions-render-links assignment-id)
    (when-let* ((reports (org-canvas--submissions-reports-line submissions)))
      (insert reports "\n"))
    (org-canvas--submissions-render-rubric-header assignment)
    (org-canvas--submissions-render-bank-heading)
    (insert "\n")
    (let ((criteria (org-canvas--submissions-rubric-criteria assignment))
          (org-canvas--submissions-comment-cache (make-hash-table :test 'equal)))
      (dolist (sub sorted)
        (org-canvas--submissions-render-detail-entry
         sub assignment-name assignment-id criteria)))))

(defun org-canvas--submissions-body-text (html)
  "Return HTML as Org text for a grading file, or nil when it is blank.
`org-canvas--html-to-org' with the hard-break markup dropped: pandoc
renders a `<br>' as a line ending in two backslashes, which only means
something on export, and a grading file is never exported, so the
marker is noise after a student's sentence and a line of nothing but
it looks like a blank answer with a stray token (issue #266).  A run
of the blank lines that leaves collapses to one."
  (when (and (stringp html) (not (string-empty-p html)))
    (let* ((org (org-canvas--html-to-org html))
           (org (replace-regexp-in-string
                 (concat "[ \t]*" (regexp-quote "\\\\") "[ \t]*$") "" org))
           (org (string-trim (replace-regexp-in-string
                              "\n[ \t]*\n\\(?:[ \t]*\n\\)+" "\n\n" org))))
      (and (not (string-empty-p org)) org))))

(defun org-canvas--submissions-render-detail-entry (submission &optional assignment-name assignment-id criteria)
  "Render a single SUBMISSION as an Org heading with properties.
SCORE is the editable grade (a number, or EX for excused); CANVAS_SCORE
is the same value as pulled, the baseline a later push compares against.
CANVAS_RUBRIC is the same baseline for the Rubric table, a digest of
the assessment as pulled, present only when Canvas holds one.
FINAL_SCORE appears only when Canvas's late policy made the recorded
score differ from the one entered.  DAYS_LATE, rounded up, appears on
a late submission, and DUE_AT, the due time it missed, beside it.
LATE_STATUS and its baseline CANVAS_LATE_STATUS appear when Canvas
holds a late policy status for the submission (see
`org-canvas--submissions-insert-late-status').  ATTEMPT lets a push
notice a resubmission.  CANVAS_COMMENTS holds the text digest
of each sent comment by id, the baseline an edit to one is told by
\(issue #419).  With
ASSIGNMENT-ID the heading gets its SpeedGrader link, and with
ASSIGNMENT-NAME attachments already downloaded link to the local copy.
CRITERIA, the assignment's rubric criteria, shape the Rubric table."
  (let ((name (org-canvas--submissions-user-sortable-name submission))
        (user-id (org-canvas--submissions-user-id submission))
        (sub-id (alist-get 'id submission))
        (status (org-canvas--submissions-normalize-status submission))
        (entered (org-canvas--submissions-entered-score submission))
        (shown (org-canvas--submissions-shown-score submission))
        (days-late (org-canvas--submissions-days-late submission))
        (final (alist-get 'score submission))
        (attempt (alist-get 'attempt submission))
        (submitted-at (org-canvas--submissions-format-submitted-at submission))
        (body (alist-get 'body submission))
        (attachments (alist-get 'attachments submission))
        (comments (alist-get 'submission_comments submission))
        (rubric (org-canvas--submissions-assessment submission)))
    (insert (format "* %s\n" name))
    (insert ":PROPERTIES:\n")
    (when user-id
      (insert (format ":USER_ID: %s\n" user-id)))
    (when sub-id
      (insert (format ":SUBMISSION_ID: %s\n" sub-id)))
    (insert (format ":STATUS: %s\n" status))
    (when days-late
      (insert (format ":DAYS_LATE: %s\n" days-late)))
    (when-let* ((due (org-canvas--submissions-due-at submission)))
      (insert (format ":DUE_AT: %s\n" due)))
    (org-canvas--submissions-insert-late-status submission)
    (when shown
      (insert (format ":SCORE: %s\n" shown))
      (insert (format ":CANVAS_SCORE: %s\n" shown)))
    (when-let* ((digest (org-canvas--submissions-rubric-digest
                         (org-canvas--submissions-assessment-triples rubric))))
      (insert (format ":CANVAS_RUBRIC: %s\n" digest)))
    (when (and entered (not (equal shown "EX")) (numberp final) (/= final entered))
      (insert (format ":FINAL_SCORE: %s\n"
                      (org-canvas--submissions-format-number final))))
    (when (numberp attempt)
      (insert (format ":ATTEMPT: %s\n" attempt)))
    (when (not (string-empty-p submitted-at))
      (insert (format ":SUBMITTED_AT: %s\n" submitted-at)))
    (let ((posted-at (org-canvas--alist-get-non-null 'posted_at submission)))
      (when (stringp posted-at)
        (insert (format ":POSTED_AT: %s\n"
                        (or (org-canvas--iso8601-to-org-timestamp posted-at) posted-at)))))
    (org-canvas--submissions-insert-report-properties submission)
    (org-canvas--submissions-insert-comment-baseline comments)
    (insert ":END:\n")
    (when (and user-id assignment-id)
      (insert (format "[[%s][Open in SpeedGrader]]\n"
                      (org-canvas--submissions-speedgrader-url assignment-id user-id))))
    (when-let* ((text (org-canvas--submissions-body-text body)))
      (insert "\n" text "\n"))
    (org-canvas--submissions-render-attachments attachments assignment-name name)
    (org-canvas--submissions-render-comments comments)
    (org-canvas--submissions-render-rubric criteria rubric)
    (org-canvas--submissions-render-notes)
    (org-canvas--submissions-render-comment-draft)
    (insert "\n")))

(defconst org-canvas--submissions-draft-heading "** Comment to post"
  "Heading under which a student's drafted comment is written.")

(defconst org-canvas--submissions-notes-heading "** Notes"
  "Heading under which a grader's own notes on a student live.")

(defun org-canvas--submissions-render-notes ()
  "Insert the Notes heading with its template, when enabled."
  (when org-canvas-submissions-notes-template
    (insert "\n" org-canvas--submissions-notes-heading "\n"
            org-canvas-submissions-notes-template "\n")))

(defun org-canvas--submissions-render-comment-draft ()
  "Insert the Comment to post heading with the template, when enabled."
  (when org-canvas-submissions-comment-template
    (insert "\n" org-canvas--submissions-draft-heading "\n"
            org-canvas-submissions-comment-template "\n")))

(defun org-canvas--submissions-section-region (heading)
  "Return (START . END) of the body under HEADING within the entry at point.
START is the line after HEADING; END the next heading or the end of the
student's subtree.  Nil when the entry has no such heading.  A
HEADING that ends the subtree with nothing under it gives an empty
region at the end of its own line (issue #438)."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point))))
      (when (re-search-forward (concat "^" (regexp-quote heading) "$") end t)
        (forward-line 1)
        (goto-char (min (point) end))
        (cons (point)
              (save-excursion
                (if (re-search-forward "^\\*" end t) (line-beginning-position) end)))))))

(defun org-canvas--submissions-section-text (heading)
  "Return what is written under HEADING in the entry at point, or nil.
Org comment lines (the templates) are dropped, and so are the blank
lines at either end; a blank line between paragraphs is kept, a run
of them as one, so a drafted comment reaches Canvas with its
paragraph breaks (issue #264)."
  (let ((region (org-canvas--submissions-section-region heading)))
    (when region
      (org-canvas--submissions-section-normalize
       (buffer-substring-no-properties (car region) (cdr region))))))

(defun org-canvas--submissions-section-normalize (raw)
  "Return RAW, a section's body, normalized as a drafted comment is read.
This is how `org-canvas--submissions-section-text' reads one: Org
comment lines go, blank lines at either end go and a run of them
between paragraphs becomes one; nil when nothing is left.  The comment
import normalizes a new draft through it too (issue #438)."
  (let* ((kept (seq-remove (lambda (l) (string-match-p "\\`[ \t]*#" l))
                           (split-string raw "\n")))
         (text (string-trim
                (replace-regexp-in-string "\n[ \t]*\n\\(?:[ \t]*\n\\)+" "\n\n"
                                          (mapconcat #'identity kept "\n")))))
    (and (not (string-empty-p text)) text)))

(defun org-canvas--submissions-set-section (heading template text)
  "Replace the body under HEADING in the entry at point with TEMPLATE and TEXT.
Either may be nil.  Does nothing when the entry has no such heading."
  (let ((region (org-canvas--submissions-section-region heading)))
    (when region
      (save-excursion
        (goto-char (car region))
        (delete-region (car region) (cdr region))
        (when template (insert template "\n"))
        (when text (insert text "\n"))
        (when (looking-at "^\\*") (insert "\n"))))))

(defun org-canvas--submissions-notes-end ()
  "Return the position for a new line at the end of the entry's Notes.
The Notes heading is added at the end of the entry when it has none."
  (let ((region (org-canvas--submissions-section-region
                 org-canvas--submissions-notes-heading)))
    (if region
        (save-excursion
          (goto-char (cdr region))
          (skip-chars-backward " \t\n" (car region))
          (unless (= (point) (car region)) (insert "\n"))
          (point))
      (save-excursion
        (org-end-of-subtree t t)
        (insert (if (bolp) "" "\n") org-canvas--submissions-notes-heading "\n")
        (point)))))

(defun org-canvas--submissions-draft-region ()
  "Return (START . END) of the draft body under the heading at point, or nil."
  (org-canvas--submissions-section-region org-canvas--submissions-draft-heading))

(defun org-canvas--submissions-comment-draft ()
  "Return the drafted comment of the heading at point, or nil when empty."
  (org-canvas--submissions-section-text org-canvas--submissions-draft-heading))

(defun org-canvas--submissions-reset-draft ()
  "Put the template back under the Comment to post heading at point."
  (org-canvas--submissions-set-section org-canvas--submissions-draft-heading
                                       org-canvas-submissions-comment-template nil))

;;;; Attempt History (issue #352)

;; A refresh marks a graded row that gained an attempt (#282), and
;; nothing in the REST submission says which attempt the score was
;; given on or how the new one differs.  GraphQL's
;; `Submission.submissionHistoriesConnection' lists every attempt, so
;; for each such row — only those, a request each — the refresh reads
;; them, logs the attempt the score belongs to beside the new one, and
;; writes GRADED_ATTEMPT on the heading next to its CONFLICT.  Read
;; only: nothing is pushed, and the property is Canvas's, rewritten by
;; the next render like the CONFLICT it explains.

(defconst org-canvas--submissions-history-query
  "query ($assignmentId: ID!, $userId: ID!) { submission(assignmentId: $assignmentId, userId: $userId) { submissionHistoriesConnection(first: 100, orderBy: {field: attempt, direction: ascending}) { nodes { attempt submittedAt gradedAt enteredScore gradeMatchesCurrentSubmission wordCount attachments { displayName } } } } }"
  "The GraphQL query that reads one student's attempts on a column.
Checked against the Canvas schema by the GraphQL contract test (issue
#269), which names it by this symbol.")

(defun org-canvas--submissions-fetch-history (assignment-id user-id)
  "Return USER-ID's attempts on ASSIGNMENT-ID as history nodes, in order."
  (let* ((data (org-canvas--graphql-query
                org-canvas--submissions-history-query
                (list (cons 'assignmentId (format "%s" assignment-id))
                      (cons 'userId (format "%s" user-id)))))
         (connection (alist-get 'submissionHistoriesConnection
                                (org-canvas--alist-get-non-null 'submission data))))
    (sort (seq-filter (lambda (node) (numberp (alist-get 'attempt node)))
                      (append (org-canvas--alist-get-non-null 'nodes connection) nil))
          (lambda (a b) (< (alist-get 'attempt a) (alist-get 'attempt b))))))

(defun org-canvas--submissions-graded-attempt (nodes)
  "Return the node of NODES whose attempt the current score was given on.
Canvas copies the score onto a later attempt's record, so a score on a
node proves nothing; the node's `gradeMatchesCurrentSubmission' says
whether the grade was given to that attempt.  The latest such node
with a score wins; nil when none has one."
  (car (last (seq-filter (lambda (node)
                           (and (eq (alist-get 'gradeMatchesCurrentSubmission node) t)
                                (numberp (org-canvas--alist-get-non-null 'enteredScore node))))
                         nodes))))

(defun org-canvas--submissions-describe-attempt (node)
  "Return NODE, one attempt, as `attempt N (submitted TS, FILES, W words)'.
The parts Canvas leaves out are left out."
  (let* ((submitted (org-canvas--iso8601-to-org-timestamp
                     (org-canvas--alist-get-non-null 'submittedAt node)))
         (files (mapcar (lambda (f) (org-canvas--alist-get-non-null 'displayName f))
                        (append (org-canvas--alist-get-non-null 'attachments node) nil)))
         (words (org-canvas--alist-get-non-null 'wordCount node))
         (parts (delq nil (list (and submitted (format "submitted %s" submitted))
                                (and files (string-join (delq nil files) ", "))
                                (and (numberp words) (> words 0)
                                     (format "%d words" (round words)))))))
    (format "attempt %s%s" (alist-get 'attempt node)
            (if parts (format " (%s)" (string-join parts ", ")) ""))))

(defun org-canvas--submissions-describe-attempts (nodes)
  "Return (GRADED . LINE) for a resubmitted row's attempt history NODES.
GRADED is the attempt number the score was given on, or nil when no
attempt carries it; LINE sets the latest attempt beside that one.
Nil when NODES is empty."
  (when nodes
    (let ((latest (car (last nodes)))
          (graded (org-canvas--submissions-graded-attempt nodes)))
      (cons (and graded (alist-get 'attempt graded))
            (if graded
                (format "%s after the score %s given on %s"
                        (org-canvas--submissions-describe-attempt latest)
                        (org-canvas--submissions-format-number (alist-get 'enteredScore graded))
                        (org-canvas--submissions-describe-attempt graded))
              (format "%s; no attempt carries the score"
                      (org-canvas--submissions-describe-attempt latest)))))))

(defun org-canvas--submissions-record-attempts (pair found)
  "Log FOUND for PAIR, a (user-id . name), and mark its heading.
FOUND is what `org-canvas--submissions-describe-attempts' returned."
  (org-canvas--log-info org-canvas--logger "[Refresh] %s: %s" (cdr pair) (cdr found))
  (when (car found)
    (save-excursion
      (when (org-canvas--submissions-goto-user (car pair))
        (org-entry-put (point) "GRADED_ATTEMPT" (format "%s" (car found)))))))

(defun org-canvas--submissions-report-attempts (assignment-id resubmitted)
  "Name the attempt each of RESUBMITTED was graded on, from its history.
RESUBMITTED are (user-id . name) pairs on ASSIGNMENT-ID.  A failed
read is one warning, and the rest are reported without their history,
as the refresh reported them before."
  (condition-case err
      (dolist (pair resubmitted)
        (when-let* ((found (org-canvas--submissions-describe-attempts
                            (org-canvas--submissions-fetch-history assignment-id (car pair)))))
          (org-canvas--submissions-record-attempts pair found)))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Refresh] Could not read the attempt history of assignment %s (%s); resubmissions reported without it"
       assignment-id (error-message-string err)))))

(defun org-canvas--submissions-collect-comment-drafts ()
  "Return (:user-id :name :text) for every heading with a drafted comment.
A student who left the course is skipped: their draft stays under the
kept heading, since Canvas has nowhere to post it (issue #282)."
  (let ((drafts nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let ((text (org-canvas--submissions-comment-draft))
              (user-id (org-entry-get (point) "USER_ID")))
          (when (and text user-id (not (org-canvas--submissions-left-p)))
            (push (list :user-id (string-to-number user-id)
                        :name (org-get-heading t t t t)
                        :text text)
                  drafts)))
        (forward-line 1)))
    (nreverse drafts)))

(defun org-canvas--submissions-render-attachments (attachments &optional assignment-name student-name)
  "Render ATTACHMENTS as a sub-heading with links.
With ASSIGNMENT-NAME and STUDENT-NAME, an attachment already downloaded
links to the local copy first, with the Canvas link beside it.  An
earlier attempt's copy under the same name is not that download, and
the entry links Canvas alone until D fetches it (issue #457)."
  (when (and attachments (> (length attachments) 0))
    (insert "\n** Attachments\n")
    (dolist (att (append attachments nil))
      (let ((url (alist-get 'url att))
            (filename (alist-get 'display_name att)))
        (when (and url filename)
          (insert (org-canvas--submissions-attachment-line
                   filename url
                   (org-canvas--submissions-local-attachment
                    assignment-name student-name filename att))))))))

(defun org-canvas--submissions-comment-org-text (comment)
  "Return COMMENT's text for its Comments item, or an empty string.
COMMENT is a submission comment alist.  The text is the full
conversion (`org-canvas--submissions-body-text'), with the heading
block markers dropped as `org-canvas--html-to-org-inline' drops them
\(issue #264)."
  (let* ((raw (alist-get 'comment comment))
         (convert (lambda ()
                    (string-trim
                     (org-canvas--strip-heading-block-markers
                      (or (org-canvas--submissions-body-text raw) ""))))))
    (if (and org-canvas--submissions-comment-cache (stringp raw))
        (with-memoization (gethash raw org-canvas--submissions-comment-cache)
          (funcall convert))
      (funcall convert))))

(defun org-canvas--submissions-comment-line (label id text &optional delete)
  "Return the Comments item LABEL [ID] :: TEXT, marked DELETE when DELETE.
ID is nil for a comment the item cannot name, which is then read-only.
A one-line TEXT follows the label, and one with more lines starts
under it, indented, so its paragraphs survive (issue #264)."
  (org-canvas--submissions-item
   (concat (if delete "DELETE " "") label (if id (format " [%s]" id) ""))
   (if (string-match-p "\n" text) (concat "\n" text) text)))

(defun org-canvas--submissions-comment-item (comment)
  "Return the Comments item for COMMENT, a submission comment alist.
The label is the author in bold and the timestamp, then the comment's
id in brackets, which is what lets a push edit or delete it (issue
#419); a one-line comment follows it on the same line, and one with
more lines starts under it, indented, so its paragraphs survive where
the inline conversion flattened them to one line (issue #264)."
  (org-canvas--submissions-comment-line
   (string-trim-right
    (format "*%s* %s"
            (or (alist-get 'author_name comment) "Unknown")
            (or (org-canvas--iso8601-to-org-timestamp (alist-get 'created_at comment)) "")))
   (org-canvas--alist-get-non-null 'id comment)
   (org-canvas--submissions-comment-org-text comment)))

(defun org-canvas--submissions-render-comments (comments)
  "Render submission COMMENTS as a sub-heading, one item each."
  (when (and comments (> (length comments) 0))
    (insert "\n" org-canvas--submissions-comments-heading "\n")
    (dolist (comment (append comments nil))
      (insert (org-canvas--submissions-comment-item comment) "\n"))))

;;;; View Toggle

;;;###autoload
(defun org-canvas-submissions-toggle-view ()
  "Toggle between the summary table and the detail headings.
In a saved grading file the summary opens as a separate read-only
buffer derived from the headings, so it reflects unpushed edits, and
`v' there returns to the file.  An ephemeral buffer re-renders in place."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (cond
   (org-canvas-submissions--source-file
    (find-file org-canvas-submissions--source-file))
   (buffer-file-name
    (org-canvas--submissions-show-summary-of-file))
   (t
    (org-canvas--submissions-toggle-in-place))))

(defun org-canvas--submissions-toggle-in-place ()
  "Re-render the current ephemeral buffer in the other view."
  (let ((current org-canvas-submissions--current-view)
        (name org-canvas-submissions--assignment-name)
        (id org-canvas-submissions--assignment-id)
        (subs org-canvas-submissions--data))
    (unless subs
      (user-error "No cached submission data; use refresh"))
    (let ((new-view (if (eq current 'summary) 'detail 'summary))
          (inhibit-read-only t))
      (if (eq new-view 'summary)
          (org-canvas--submissions-render-summary name id subs)
        (org-canvas--submissions-render-detail name id subs))
      (setq org-canvas-submissions--current-view new-view)
      (goto-char (point-min))
      (message "Switched to %s view" new-view))))

(defun org-canvas--submissions-heading-rows ()
  "Return (name status submitted-at score) for each student heading in the file.
A level-1 heading without a USER_ID — the rubric block — is no student."
  (delq nil
        (org-map-entries
         (lambda ()
           (when (org-entry-get (point) "USER_ID")
             (list (org-get-heading t t t t)
                   (or (org-entry-get (point) "STATUS") "")
                   (or (org-entry-get (point) "SUBMITTED_AT") "")
                   (or (org-entry-get (point) "SCORE") ""))))
         "LEVEL=1")))

(defun org-canvas--submissions-heading-reports-line ()
  "Return the Reports: line the file's student headings add up to, or nil."
  (org-canvas--submissions-format-report-counts
   (org-canvas--submissions-report-counts
    (org-map-entries
     (lambda ()
       (and (org-entry-get (point) "USER_ID")
            (org-canvas--submissions-report-values-at-point)))
     "LEVEL=1"))))

(defun org-canvas--submissions-show-summary-of-file ()
  "Show a read-only summary table of the current grading file."
  (let* ((name org-canvas-submissions--assignment-name)
         (id org-canvas-submissions--assignment-id)
         (file buffer-file-name)
         (rows (org-canvas--submissions-heading-rows))
         (reports (org-canvas--submissions-heading-reports-line))
         (buf (get-buffer-create (format "*submissions summary: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: Submissions: %s\n" name))
        (insert (format "#+PROPERTY: CANVAS_ASSIGNMENT_ID %s\n\n" id))
        (insert "Read-only overview of the grading file; edit scores there (press v).\n\n")
        (when reports
          (insert reports "\n\n"))
        (insert "| Student | Status | Submitted At | Score |\n")
        (insert "|---------+--------+--------------+-------|\n")
        (dolist (row rows)
          (insert (apply #'format "| %s | %s | %s | %s |\n" row)))
        (org-table-align)
        (goto-char (point-min)))
      (setq org-canvas-submissions--assignment-name name
            org-canvas-submissions--assignment-id id
            org-canvas-submissions--current-view 'summary
            org-canvas-submissions--source-file file)
      (org-canvas-submissions-mode 1)
      (setq buffer-read-only t))
    (switch-to-buffer buf)))

(provide 'org-canvas-submissions-render)
;;; org-canvas-submissions-render.el ends here
