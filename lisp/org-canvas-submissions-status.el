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
;;   Missing | Pulled | New | Next
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
;;
;; PRIVACY
;; =======
;; The report holds counts only: no name, score or user id is
;; written.

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

(defun org-canvas--submissions-status-column (assignment)
  "Return the report row for ASSIGNMENT as a plist.
The keys are :name, :due (an ISO timestamp or nil), :pulled-at (the
grading file's PULLED_AT string or nil), :counts (see
`org-canvas--submissions-status-counts', nil when unreadable) and
:next (see `org-canvas--submissions-status-next'; (\"?\") when
unreadable)."
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
          :next (if counts
                    (org-canvas--submissions-status-next counts pulled-at)
                  (list "?")))))

(defun org-canvas--submissions-status-rank (column)
  "Return COLUMN's place in the sort: the index of its next action."
  (cl-position (car (plist-get column :next))
               org-canvas--submissions-status-next-order
               :test #'equal))

(defun org-canvas--submissions-status-columns (assignments)
  "Return one report row per assignment in ASSIGNMENTS, work first.
The rows are sorted by their next action in the order of
`org-canvas--submissions-status-next-order', and within one action
they keep Canvas's order, since `sort' is stable."
  (sort (mapcar #'org-canvas--submissions-status-column assignments)
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

(defun org-canvas--submissions-status-cells (column)
  "Return the table cells of COLUMN, a report row plist."
  (let ((counts (plist-get column :counts)))
    (list (org-canvas--submissions-table-cell (plist-get column :name))
          (or (org-canvas--iso8601-to-org-timestamp (plist-get column :due)) "-")
          (org-canvas--submissions-status-count-cell counts :rows)
          (org-canvas--submissions-status-count-cell counts :submitted)
          (org-canvas--submissions-status-count-cell counts :graded)
          (org-canvas--submissions-status-count-cell counts :posted)
          (org-canvas--submissions-status-count-cell counts :late)
          (org-canvas--submissions-status-count-cell counts :missing)
          (or (plist-get column :pulled-at) "-")
          (if (plist-get column :pulled-at)
              (org-canvas--submissions-status-count-cell counts :new)
            "-")
          (org-canvas--submissions-status-next-cell (plist-get column :next)))))

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

(defun org-canvas--submissions-status-insert-table (columns)
  "Insert the report table for COLUMNS at point and align it."
  (let ((header '("Assignment" "Due" "Rows" "Submitted" "Graded" "Posted"
                  "Late" "Missing" "Pulled" "New" "Next"))
        (start (point)))
    (insert "| " (mapconcat #'identity header " | ") " |\n")
    (insert "|" (mapconcat (lambda (_) "---") header "+") "|\n")
    (dolist (column columns)
      (insert "| "
              (mapconcat #'identity (org-canvas--submissions-status-cells column)
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
  (insert (org-canvas--submissions-status-summary columns) "\n\n")
  (org-canvas--submissions-status-insert-table columns)
  (insert "\nNext: pull = work is in and no grading file exists;"
          " refresh = submitted since the grading file's PULLED_AT;"
          " grade = submitted and not scored; post = scored and not posted.\n"
          "Read-only: nothing here is written to disk or to Canvas.\n"))

;;;; Entry Point

;;;###autoload
(defun org-canvas-submissions-status ()
  "Report which columns need pulling, grading or posting, one table.
Every published, graded assignment is a row with how many students
have submitted, been scored and been posted, the late and missing
counts, when its grading file was pulled and how many submissions
arrived since, and the next thing to do; the columns with work to do
come first.  Reads only.  Under `noninteractive' the table is printed
to standard output."
  (interactive)
  (let* ((columns (org-canvas--submissions-status-columns
                   (org-canvas--submissions-status-fetch-assignments)))
         (summary (org-canvas--submissions-status-summary columns)))
    (org-canvas--report-display
     org-canvas--submissions-status-buffer-name
     (lambda () (org-canvas--submissions-status-render columns)))
    (org-canvas--log-info org-canvas--logger "[Submissions status] %s" summary)
    (message "Grading queue: %s" summary)
    columns))

(provide 'org-canvas-submissions-status)
;;; org-canvas-submissions-status.el ends here
