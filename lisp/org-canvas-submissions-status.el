;;; org-canvas-submissions-status.el --- Which columns need pulling, grading or posting -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Every graded column in a course goes through pull, grade, push and
;; post, and where each one stands lives in three places: Canvas, the
;; column's grading file and the instructor's memory.  gradebook.org
;; answers how the class did and who is falling behind; a grading file
;; answers one column.  Neither answers the weekly question, "which
;; columns need work right now?", so it was answered by a hand script
;; reading every column's submissions (issue #283).
;;
;; `org-canvas-submissions-status' is that script as a command.  It
;; reads every published, graded assignment's submissions and renders
;; one table, a row per column:
;;
;;   Assignment | Due | Rows | Submitted | Graded | Posted | Late |
;;   Missing | Mean | Median | Pulled | New | Next
;;
;; Mean and Median are Canvas's score statistics for the column (issue
;; #352), read for the whole course at once; they sit beside the row
;; counts and never stand in for them.
;;
;; Pulled is the PULLED_AT of the column's grading file, when one is
;; on disk, and New counts the submissions handed in since then.  Next
;; names the one thing to do: pull (work is in and no grading file
;; exists), refresh (work arrived after the pull), grade (submitted
;; and not scored), post (scored and not posted), or nothing.  The
;; columns with something to do come first, in that order.
;;
;; The report is read-only in every sense: nothing is written to disk,
;; nothing is pushed, and a course marked read-only reads it the same.
;; Under `noninteractive' it is printed to standard output, which is
;; where the hand script used to print.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/assignments?order_by=position
;;       &override_assignment_dates=false
;;       the columns, in gradebook order, with their own due dates
;;       (issue #273: with overrides applied a teacher reads the most
;;       lenient extension as the due date).
;;   GET /courses/:id/assignments/:id/submissions
;;       one row per enrolled student, submitted or not, with
;;       `submitted_at', `score', `excused', `posted_at', `late' and
;;       `missing'.  The counts come from these rows rather than the
;;       assignment's `needs_grading_count', which says nothing about
;;       posting or about what arrived since a pull.
;;   POST /api/graphql (a read, `org-canvas--graphql-query')
;;       the document processor reports of a column someone submitted
;;       to, one query per column (issue #351), summed into a
;;       Reports: line that names the columns where a report failed.
;;   POST /api/graphql (a read, `org-canvas--graphql-query')
;;       every column's `scoreStatistic' mean and median, a page of a
;;       hundred assignments per request (issue #352); null when
;;       nothing in the column is graded.
;;
;; PRIVACY
;; =======
;; The report holds counts and column averages only: no name, no
;; student's score and no user id is written.  The Reports: line names
;; columns, never students.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules: it reads the grading
;; files the submissions module writes, by their paths and header.
(require 'org-canvas-submissions)

(defconst org-canvas--submissions-status-buffer-name "*canvas-submissions-status*"
  "Name of the buffer the grading queue is rendered in.")

(defconst org-canvas--submissions-status-next-order
  '("pull" "refresh" "grade" "post" "-" "?")
  "Next actions, in the order the report puts them.
A column that could not be read answers ?, and comes last.")

;;;; Fetching

(defun org-canvas--submissions-status-fetch-assignments ()
  "Return the course's published, graded assignments in gradebook order.
An unpublished assignment has no submissions to speak of and a
`not_graded' one nothing to grade or post, so both stay out."
  (cl-remove-if-not
   (lambda (a)
     (and (eq (alist-get 'published a) t)
          (not (equal (alist-get 'grading_type a) "not_graded"))))
   (append (org-canvas-api-request-all-pages
            'GET (org-canvas-api-course-endpoint "assignments")
            '(("order_by" . "position")
              ("override_assignment_dates" . "false")))
           nil)))

(defun org-canvas--submissions-status-fetch-submissions (assignment-id)
  "Return ASSIGNMENT-ID's submissions as a list, one per student."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint
                 "assignments/%s/submissions" assignment-id))
          nil))

;;;; Score Statistics (issue #352)

;; Canvas keeps a mean and a median for every column in GraphQL, as
;; `Assignment.scoreStatistic', and a course's assignments come a page
;; at a time from `assignmentsConnection', so the whole course is read
;; in one request per hundred columns rather than one per column.  The
;; statistic is filled whenever a column has a graded submission,
;; posted or not, and is null when nothing is graded.  It is a column
;; fact shown beside the row counts, never one of them: Canvas's own
;; `count' can exceed the graded rows a teacher reads (departed
;; students' scores stay in it), so it is not asked for at all.

(defconst org-canvas--submissions-status-statistics-query
  "query ($courseId: ID!, $filter: AssignmentFilter, $cursor: String) { course(id: $courseId) { assignmentsConnection(first: 100, after: $cursor, filter: $filter) { pageInfo { hasNextPage endCursor } nodes { _id scoreStatistic { mean median } } } } }"
  "The GraphQL query reading every column's mean and median score.
One page of the course's assignments per request.  The filter is
sent with a null grading period, since without one Canvas answers
the current grading period's assignments only.  Checked against the
Canvas schema by the GraphQL contract test (issue #269), which names
it by this symbol.")

(defun org-canvas--submissions-status-statistics-variables (cursor)
  "Return the statistics query's variables for the page after CURSOR.
The filter's grading period is nil, which encodes as JSON null and
asks for every grading period's assignments."
  (append (list (cons 'courseId (format "%s" org-canvas-course-id))
                (list 'filter (cons 'gradingPeriodId nil)))
          (when cursor (list (cons 'cursor cursor)))))

(defun org-canvas--submissions-status-statistics-page (cursor map)
  "Read one page of score statistics after CURSOR into MAP.
MAP is a hash from assignment id (a string) to the node's
`scoreStatistic' alist, or to the symbol `none' when Canvas answers
null, as it does for a column with nothing graded.  Return the next
page's cursor, or nil after the last."
  (let* ((data (org-canvas--graphql-query
                org-canvas--submissions-status-statistics-query
                (org-canvas--submissions-status-statistics-variables cursor)))
         (connection (alist-get 'assignmentsConnection
                                (alist-get 'course data)))
         (info (alist-get 'pageInfo connection)))
    (dolist (node (append (alist-get 'nodes connection) nil))
      (when-let* ((id (org-canvas--alist-get-non-null '_id node)))
        (puthash (format "%s" id)
                 (or (org-canvas--alist-get-non-null 'scoreStatistic node)
                     'none)
                 map)))
    (and (eq (alist-get 'hasNextPage info) t)
         (org-canvas--alist-get-non-null 'endCursor info))))

(defun org-canvas--submissions-status-fetch-statistics ()
  "Return every column's score statistic by assignment id, or nil.
The value is the hash of `org-canvas--submissions-status-statistics-page'.
A failed read is one warning and nil, and the queue is rendered
without the statistics: they add to the counts and are never needed
to read them."
  (condition-case err
      (let ((map (make-hash-table :test 'equal))
            (cursor nil))
        (while (setq cursor (org-canvas--submissions-status-statistics-page
                             cursor map)))
        map)
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions status] Could not read the score statistics (%s); %s"
       (error-message-string err) "the queue is shown without them")
     nil)))

(defun org-canvas--submissions-status-statistic (assignment statistics)
  "Return ASSIGNMENT's entry in STATISTICS, or nil when there are none.
STATISTICS is the hash of `org-canvas--submissions-status-fetch-statistics',
nil when the read failed.  A column the read did not name counts as
one with nothing graded, `none'."
  (when statistics
    (gethash (format "%s" (alist-get 'id assignment)) statistics 'none)))

;;;; The Grading File

(defun org-canvas--submissions-status-pulled-at (assignment-name)
  "Return the PULLED_AT of ASSIGNMENT-NAME's grading file, or nil.
Nil when no grading file is on disk.  The header is read from the
file itself, never through a visited buffer: the property is written
and saved by the pull that wrote the file, so the disk is right, and a
report should neither visit a hundred files nor touch one the grader
has open."
  (let ((file (org-canvas--submissions-file-path assignment-name)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file nil 0 4096)
        (org-canvas--submissions-file-property "PULLED_AT")))))

(defun org-canvas--submissions-status-parse-pulled-at (pulled-at)
  "Return PULLED-AT, a grading file's Org timestamp, as a time value.
Nil when it does not parse.  The grading file writes it in the local
zone with no seconds, so the value is the start of that minute."
  (condition-case nil
      (and (stringp pulled-at)
           (encode-time (org-parse-time-string pulled-at)))
    (error nil)))

(defun org-canvas--submissions-status-after-p (iso8601 time)
  "Return non-nil when ISO8601, a Canvas timestamp, is later than TIME.
The date prefix is checked first, as every ISO parse in the package
does: `date-to-time' answers garbage with a 1970 date on some Emacs
versions, which would read as \"not new\" without a word."
  (condition-case nil
      (and (org-canvas--iso8601-date-p iso8601) time
           (time-less-p time (date-to-time iso8601)))
    (error nil)))

;;;; Counting

(defun org-canvas--submissions-status-submitted-p (submission)
  "Return non-nil when SUBMISSION was handed in."
  (stringp (org-canvas--alist-get-non-null 'submitted_at submission)))

(defun org-canvas--submissions-status-graded-p (submission)
  "Return non-nil when SUBMISSION carries a score or is excused."
  (or (numberp (alist-get 'score submission))
      (eq (alist-get 'excused submission) t)))

(defun org-canvas--submissions-status-posted-p (submission)
  "Return non-nil when SUBMISSION's grade has been posted."
  (stringp (org-canvas--alist-get-non-null 'posted_at submission)))

(defun org-canvas--submissions-status-counts (submissions pulled-at)
  "Fold SUBMISSIONS into a plist of counts for one column.
PULLED-AT is the grading file's timestamp as a time value, or nil
when the column was never pulled; :new counts the submissions handed
in after it.  :ungraded and :unposted are what Next is derived from:
submitted rows without a score, and scored rows not yet posted."
  (list :rows (length submissions)
        :submitted (cl-count-if #'org-canvas--submissions-status-submitted-p
                                submissions)
        :graded (cl-count-if #'org-canvas--submissions-status-graded-p
                             submissions)
        :posted (cl-count-if #'org-canvas--submissions-status-posted-p
                             submissions)
        :late (cl-count-if (lambda (s) (eq (alist-get 'late s) t)) submissions)
        :missing (cl-count-if (lambda (s) (eq (alist-get 'missing s) t))
                              submissions)
        :new (if pulled-at
                 (cl-count-if
                  (lambda (s)
                    (org-canvas--submissions-status-after-p
                     (alist-get 'submitted_at s) pulled-at))
                  submissions)
               0)
        :ungraded (cl-count-if
                   (lambda (s)
                     (and (org-canvas--submissions-status-submitted-p s)
                          (not (org-canvas--submissions-status-graded-p s))))
                   submissions)
        :unposted (cl-count-if
                   (lambda (s)
                     (and (org-canvas--submissions-status-graded-p s)
                          (not (org-canvas--submissions-status-posted-p s))))
                   submissions)))

(defun org-canvas--submissions-status-next (counts pulled)
  "Return the next action for a column with COUNTS, as (VERB COUNT).
PULLED is non-nil when the column has a grading file.  The verb is the
first that applies of pull, refresh, grade and post; COUNT is how many
rows it concerns, and is absent for pull.  A column with nothing to do
answers (\"-\")."
  (cond
   ((and (not pulled) (> (plist-get counts :submitted) 0)) (list "pull"))
   ((> (plist-get counts :new) 0) (list "refresh" (plist-get counts :new)))
   ((> (plist-get counts :ungraded) 0) (list "grade" (plist-get counts :ungraded)))
   ((> (plist-get counts :unposted) 0) (list "post" (plist-get counts :unposted)))
   (t (list "-"))))

;;;; Columns

(defun org-canvas--submissions-status-read-column (assignment)
  "Return ASSIGNMENT's submissions, or the symbol `unreadable'.
A column the token cannot read (a 403 or any other API error) is
logged once and reported as such; the rest of the report goes on."
  (condition-case err
      (org-canvas--submissions-status-fetch-submissions (alist-get 'id assignment))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions status] Could not read the submissions of '%s': %s"
       (alist-get 'name assignment) (error-message-string err))
     'unreadable)))

(defun org-canvas--submissions-status-reports (assignment counts)
  "Return ASSIGNMENT's document processor report counts, or nil.
COUNTS are the column's; a column nobody submitted to, or one that
could not be read, is not asked (issue #351).  The value is the plist
of `org-canvas--submissions-report-counts', nil when the column has
no report or the read failed."
  (when (and counts (> (plist-get counts :submitted) 0))
    (org-canvas--submissions-map-report-counts
     (org-canvas--submissions-fetch-reports (alist-get 'id assignment)))))

(defun org-canvas--submissions-status-column (assignment &optional statistics)
  "Return the report row for ASSIGNMENT as a plist.
The keys are :name, :due (an ISO timestamp or nil), :pulled-at (the
grading file's PULLED_AT string or nil), :counts (see
`org-canvas--submissions-status-counts', nil when unreadable),
:next (see `org-canvas--submissions-status-next'; (\"?\") when
unreadable), :reports (see `org-canvas--submissions-status-reports')
and :statistic, the column's entry in STATISTICS (see
`org-canvas--submissions-status-statistic')."
  (let* ((name (or (alist-get 'name assignment) ""))
         (pulled-at (org-canvas--submissions-status-pulled-at name))
         (submissions (org-canvas--submissions-status-read-column assignment))
         (counts (unless (eq submissions 'unreadable)
                   (org-canvas--submissions-status-counts
                    submissions
                    (org-canvas--submissions-status-parse-pulled-at pulled-at)))))
    (list :name name
          :due (org-canvas--alist-get-non-null 'due_at assignment)
          :pulled-at pulled-at
          :counts counts
          :reports (org-canvas--submissions-status-reports assignment counts)
          :statistic (org-canvas--submissions-status-statistic
                      assignment statistics)
          :next (if counts
                    (org-canvas--submissions-status-next counts pulled-at)
                  (list "?")))))

(defun org-canvas--submissions-status-rank (column)
  "Return COLUMN's place in the sort: the index of its next action."
  (cl-position (car (plist-get column :next))
               org-canvas--submissions-status-next-order
               :test #'equal))

(defun org-canvas--submissions-status-columns (assignments &optional statistics)
  "Return one report row per assignment in ASSIGNMENTS, work first.
STATISTICS, the hash of `org-canvas--submissions-status-fetch-statistics'
or nil, gives each row its score statistic.  The rows are sorted by
their next action in the order of
`org-canvas--submissions-status-next-order', and within one action
they keep Canvas's order, since `sort' is stable."
  (sort (mapcar (lambda (assignment)
                  (org-canvas--submissions-status-column assignment statistics))
                assignments)
        (lambda (a b)
          (< (org-canvas--submissions-status-rank a)
             (org-canvas--submissions-status-rank b)))))

;;;; Rendering

(defun org-canvas--submissions-status-count-cell (counts key)
  "Return the cell for KEY of COUNTS, or ? when the column is unreadable."
  (if counts
      (number-to-string (plist-get counts key))
    "?"))

(defun org-canvas--submissions-status-next-cell (next)
  "Return the Next cell for NEXT, the (VERB COUNT) a column answers."
  (if (cadr next)
      (format "%s (%d)" (car next) (cadr next))
    (car next)))

(defun org-canvas--submissions-status-score-cell (statistic key)
  "Return the cell for KEY (`mean' or `median') of STATISTIC.
STATISTIC is a column's :statistic; `none', or a value Canvas left
null, is the dash of a column with nothing graded.  A score is shown
to two decimals at most, with trailing zeros dropped."
  (let ((value (and (consp statistic)
                    (org-canvas--alist-get-non-null key statistic))))
    (if (numberp value)
        (replace-regexp-in-string "\\.?0+\\'" "" (format "%.2f" value))
      "-")))

(defun org-canvas--submissions-status-cells (column &optional statistics)
  "Return the table cells of COLUMN, a report row plist.
With STATISTICS non-nil, the column's mean and median follow its
counts."
  (let ((counts (plist-get column :counts))
        (statistic (plist-get column :statistic)))
    (append
     (list (org-canvas--submissions-table-cell (plist-get column :name))
           (or (org-canvas--iso8601-to-org-timestamp (plist-get column :due))
               "-"))
     (mapcar (lambda (key)
               (org-canvas--submissions-status-count-cell counts key))
             '(:rows :submitted :graded :posted :late :missing))
     (when statistics
       (list (org-canvas--submissions-status-score-cell statistic 'mean)
             (org-canvas--submissions-status-score-cell statistic 'median)))
     (list (or (plist-get column :pulled-at) "-")
           (if (plist-get column :pulled-at)
               (org-canvas--submissions-status-count-cell counts :new)
             "-")
           (org-canvas--submissions-status-next-cell
            (plist-get column :next))))))

(defun org-canvas--submissions-status-statistics-p (columns)
  "Return non-nil when COLUMNS carry score statistics to show.
Every column has a :statistic once the read succeeded, and none has
one when it failed, so the table either shows the two columns for
all or leaves them out."
  (cl-some (lambda (c) (plist-get c :statistic)) columns))

(defun org-canvas--submissions-status-summary (columns)
  "Return the one-line summary of COLUMNS: how many columns need what."
  (let ((verbs (mapcar (lambda (c) (car (plist-get c :next))) columns)))
    (format "%d columns | %d to pull | %d to refresh | %d to grade | %d to post | %d unreadable"
            (length columns)
            (cl-count "pull" verbs :test #'equal)
            (cl-count "refresh" verbs :test #'equal)
            (cl-count "grade" verbs :test #'equal)
            (cl-count "post" verbs :test #'equal)
            (cl-count-if (lambda (c) (null (plist-get c :counts))) columns))))

(defun org-canvas--submissions-status-reports-line (columns)
  "Return the Reports: line summed over COLUMNS, or nil when none has one.
Rows are counted once each, as in a grading file; the columns where a
report failed are named with their count, since that is the one a
grader acts on before grading."
  (let ((total (list :processed 0 :failed 0 :pending 0))
        (failing nil))
    (dolist (column columns)
      (when-let* ((reports (plist-get column :reports)))
        (dolist (key '(:processed :failed :pending))
          (plist-put total key
                     (+ (plist-get total key) (plist-get reports key))))
        (when (> (plist-get reports :failed) 0)
          (push (format "%s (%d)" (plist-get column :name)
                        (plist-get reports :failed))
                failing))))
    (when (cl-some (lambda (c) (plist-get c :reports)) columns)
      (concat (org-canvas--submissions-format-report-counts total)
              (if failing
                  (concat "; failed in " (string-join (nreverse failing) ", "))
                "")))))

(defun org-canvas--submissions-status-insert-table (columns)
  "Insert the report table for COLUMNS at point and align it."
  (let* ((statistics (org-canvas--submissions-status-statistics-p columns))
         (header (append '("Assignment" "Due" "Rows" "Submitted" "Graded"
                           "Posted" "Late" "Missing")
                         (when statistics '("Mean" "Median"))
                         '("Pulled" "New" "Next")))
         (start (point)))
    (insert "| " (mapconcat #'identity header " | ") " |\n")
    (insert "|" (mapconcat (lambda (_) "---") header "+") "|\n")
    (dolist (column columns)
      (insert "| "
              (mapconcat #'identity
                         (org-canvas--submissions-status-cells
                          column statistics)
                         " | ")
              " |\n"))
    (save-excursion
      (goto-char start)
      (org-table-align))))

(defun org-canvas--submissions-status-render (columns)
  "Render the grading queue for COLUMNS into the current buffer."
  (org-mode)
  (insert (format "Grading queue for course %s, read %s\n\n" org-canvas-course-id
                  (format-time-string "<%Y-%m-%d %a %H:%M>")))
  (insert (org-canvas--submissions-status-summary columns) "\n")
  (when-let* ((reports (org-canvas--submissions-status-reports-line columns)))
    (insert reports "\n"))
  (insert "\n")
  (org-canvas--submissions-status-insert-table columns)
  (insert "\nNext: pull = work is in and no grading file exists;"
          " refresh = submitted since the grading file's PULLED_AT;"
          " grade = submitted and not scored; post = scored and not posted.\n")
  (when (org-canvas--submissions-status-statistics-p columns)
    (insert "Mean, Median: Canvas's statistics over the scores it holds,"
            " posted or not (- = none graded); a departed student's score"
            " counts there and not in Graded.\n"))
  (insert "Read-only: nothing here is written to disk or to Canvas.\n"))

;;;; Entry Point

;;;###autoload
(defun org-canvas-submissions-status ()
  "Report which columns need pulling, grading or posting, one table.
Every published, graded assignment is a row with how many students
have submitted, been scored and been posted, the late and missing
counts, Canvas's mean and median score, when its grading file was
pulled and how many submissions arrived since, and the next thing to
do; the columns with work to do come first.  Reads only.  Under
`noninteractive' the table is printed to standard output."
  (interactive)
  (let* ((assignments (org-canvas--submissions-status-fetch-assignments))
         (columns (org-canvas--submissions-status-columns
                   assignments
                   (and assignments
                        (org-canvas--submissions-status-fetch-statistics))))
         (summary (org-canvas--submissions-status-summary columns)))
    (org-canvas--report-display
     org-canvas--submissions-status-buffer-name
     (lambda () (org-canvas--submissions-status-render columns)))
    (org-canvas--log-info org-canvas--logger "[Submissions status] %s" summary)
    (when-let* ((reports (org-canvas--submissions-status-reports-line columns)))
      (org-canvas--log-info org-canvas--logger
        "[Submissions status] %s" reports))
    (message "Grading queue: %s" summary)
    columns))

(provide 'org-canvas-submissions-status)
;;; org-canvas-submissions-status.el ends here
