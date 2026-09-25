;;; org-canvas-submissions-status-test.el --- Buttercup tests for the grading queue -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-status': every graded column's
;; submitted, graded, posted and newer-than-last-pull counts in one
;; table, with the next thing to do (issue #283).  Every request is
;; answered by a fake keyed on the URL; nothing here reaches the network
;; (Hard Rule 2), and the log is never read from the shared buffer
;; (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defvar test-sstatus--assignments nil
  "Assignments the fake API lists for the course.")
(defvar test-sstatus--submissions nil
  "Submissions the fake API lists, an alist of assignment id to rows.
A value of `forbidden' answers the read with a 403.")
(defvar test-sstatus--calls nil
  "Requests the fake API answered, newest first, as (URL . PARAMS).")
(defvar test-sstatus--graphql-calls nil
  "Assignment ids the fake GraphQL was asked about, newest first.")
(defvar test-sstatus--statistics nil
  "Score statistics the fake GraphQL answers, an alist of id to statistic.
A statistic is an alist of `mean' and `median', or nil for Canvas's
null; the symbol `refused' makes the read signal.")
(defvar test-sstatus--statistics-calls nil
  "Variables the fake GraphQL's statistics query was sent, newest first.")
(defvar test-sstatus--reports nil
  "Report nodes the fake GraphQL answers, an alist of assignment id to rows.
Each row is (USER-ID . REPORT-NODES); a value of `refused' signals.")

(defun test-sstatus--assignment (id name &rest extra)
  "A published, points-graded assignment ID named NAME, plus EXTRA pairs.
EXTRA comes first, so an explicit `published', `grading_type' or
`due_at' shadows the default."
  (append extra
          `((id . ,id) (name . ,name) (published . t)
            (grading_type . "points") (due_at . "2026-09-10T03:59:00Z"))))

(defun test-sstatus--submission (&rest fields)
  "A submission row carrying FIELDS, an unsubmitted, unscored one by default."
  (append fields
          '((submitted_at . nil) (score . nil) (excused . :json-false)
            (posted_at . nil) (late . :json-false) (missing . :json-false))))

(defun test-sstatus--api (_method url &optional params)
  "Answer URL from the fake tables, recording PARAMS."
  (push (cons url params) test-sstatus--calls)
  (cond
   ((string-match "/assignments/\\([0-9]+\\)/submissions\\'" url)
    (let ((rows (alist-get (string-to-number (match-string 1 url))
                           test-sstatus--submissions)))
      (if (eq rows 'forbidden)
          (signal 'org-canvas-permission-error
                  (list "403 Forbidden: user not authorized"))
        (vconcat rows))))
   ((string-match-p "/assignments\\'" url) (vconcat test-sstatus--assignments))
   (t (error "Unexpected request: %s" url))))

(defun test-sstatus--statistics-reply (variables)
  "Answer the statistics query sent VARIABLES from the fake table, in one page."
  (push variables test-sstatus--statistics-calls)
  (if (eq test-sstatus--statistics 'refused)
      (signal 'org-canvas-api-error (list "GraphQL: refused"))
    `((course
       . ((assignmentsConnection
           . ((pageInfo . ((hasNextPage . :json-false) (endCursor . :null)))
              (nodes . ,(vconcat
                         (mapcar (lambda (row)
                                   `((_id . ,(format "%s" (car row)))
                                     (scoreStatistic . ,(or (cdr row) :null))))
                                 test-sstatus--statistics))))))))))

(defun test-sstatus--graphql (document &optional variables)
  "Answer DOCUMENT sent VARIABLES: the statistics or the reports query."
  (if (eq document org-canvas--submissions-status-statistics-query)
      (test-sstatus--statistics-reply variables)
    (test-sstatus--reports-reply variables)))

(defun test-sstatus--reports-reply (variables)
  "Answer the reports query for the assignment VARIABLES name, in one page."
  (let* ((id (string-to-number (alist-get 'assignmentId variables)))
         (rows (alist-get id test-sstatus--reports)))
    (push id test-sstatus--graphql-calls)
    (if (eq rows 'refused)
        (signal 'org-canvas-api-error (list "GraphQL: refused"))
      `((assignment
         . ((submissionsConnection
             . ((pageInfo . ((hasNextPage . :json-false) (endCursor . :null)))
                (nodes . ,(vconcat
                           (mapcar (lambda (row)
                                     `((userId . ,(format "%s" (car row)))
                                       (ltiAssetReportsConnection
                                        . ((nodes . ,(vconcat (cdr row)))))))
                                   rows)))))))))))

(defmacro test-sstatus--with-course (assignments submissions &rest body)
  "Run BODY with the fake API serving ASSIGNMENTS and SUBMISSIONS.
The submissions directory is a temp directory, so a grading file
written there is the only one the report can find."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "submissions-status-" t))
          (org-canvas-submissions-directory dir)
          (org-canvas-time-zone "UTC")
          (test-sstatus--assignments ,assignments)
          (test-sstatus--submissions ,submissions)
          (test-sstatus--calls nil)
          (test-sstatus--graphql-calls nil)
          (test-sstatus--statistics nil)
          (test-sstatus--statistics-calls nil))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                      #'test-sstatus--api)
                     ((symbol-function 'org-canvas--graphql-query)
                      #'test-sstatus--graphql)
                     ((symbol-function 'message) #'ignore)
                     ((symbol-function 'princ) #'ignore))
             ,@body))
       (delete-directory dir t))))

(defun test-sstatus--write-grading-file (name pulled-at)
  "Write a grading file for assignment NAME whose PULLED_AT is PULLED-AT."
  (with-temp-file (org-canvas--submissions-file-path name)
    (insert (format "#+TITLE: Submissions: %s\n" name)
            "#+PROPERTY: CANVAS_ASSIGNMENT_ID 1\n"
            (format "#+PROPERTY: CANVAS_ASSIGNMENT_NAME %s\n" name)
            (format "#+PROPERTY: PULLED_AT %s\n\n" pulled-at)
            "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")))

(defun test-sstatus--rows (text)
  "Return the body rows of the report table in TEXT as cell lists.
Bounded by the number of lines; the match's end is read before
`split-string' runs, since it clobbers the match data."
  (let ((rows nil) (start 0)
        (limit (length (split-string text "\n"))))
    (while (and (> limit 0) (string-match "^| \\([^|\n]*\\)|\\(.*\\)$" text start))
      (let ((end (match-end 0))
            (first (string-trim (match-string 1 text)))
            (rest (match-string 2 text)))
        (unless (equal first "Assignment")
          (push (cons first (mapcar #'string-trim (split-string rest "|" t))) rows))
        (setq start end limit (1- limit))))
    (nreverse rows)))

;;;; Fetching

(describe "org-canvas--submissions-status-fetch-assignments"
  (it "keeps published, graded assignments in Canvas order and asks for their own dates"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 2 "Second")
              (test-sstatus--assignment 3 "Draft" '(published . :json-false))
              (test-sstatus--assignment 4 "Attendance" '(grading_type . "not_graded"))
              (test-sstatus--assignment 1 "First"))
        nil
      (let ((names (mapcar (lambda (a) (alist-get 'name a))
                           (org-canvas--submissions-status-fetch-assignments)))
            (params (cdr (car test-sstatus--calls))))
        (expect names :to-equal '("Second" "First"))
        (expect (assoc "order_by" params) :to-equal '("order_by" . "position"))
        (expect (assoc "override_assignment_dates" params)
                :to-equal '("override_assignment_dates" . "false"))))))

(describe "org-canvas--submissions-status-fetch-submissions"
  (it "returns the rows of the assignment as a list"
    (test-sstatus--with-course
        nil
        (list (cons 7 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z")))))
      (let ((rows (org-canvas--submissions-status-fetch-submissions 7)))
        (expect (listp rows) :to-be-truthy)
        (expect (length rows) :to-equal 1)
        (expect (car (car test-sstatus--calls))
                :to-match "/assignments/7/submissions\\'")))))

;;;; The Grading File

(describe "org-canvas--submissions-status-pulled-at"
  (it "reads PULLED_AT from the grading file on disk"
    (test-sstatus--with-course nil nil
      (test-sstatus--write-grading-file "Journal 02" "<2026-09-12 Sat 10:30>")
      (expect (org-canvas--submissions-status-pulled-at "Journal 02")
              :to-equal "<2026-09-12 Sat 10:30>")))

  (it "is nil when the column was never pulled"
    (test-sstatus--with-course nil nil
      (expect (org-canvas--submissions-status-pulled-at "Journal 03") :to-be nil)))

  (it "does not visit the file"
    (test-sstatus--with-course nil nil
      (test-sstatus--write-grading-file "Journal 02" "<2026-09-12 Sat 10:30>")
      (org-canvas--submissions-status-pulled-at "Journal 02")
      (expect (find-buffer-visiting (org-canvas--submissions-file-path "Journal 02"))
              :to-be nil))))

(describe "org-canvas--submissions-status-parse-pulled-at"
  (it "turns the Org timestamp into a time value"
    (let ((time (org-canvas--submissions-status-parse-pulled-at "<2026-09-12 Sat 10:30>")))
      (expect time :to-be-truthy)
      (expect (format-time-string "%Y-%m-%d %H:%M" time) :to-equal "2026-09-12 10:30")))

  (it "is nil for nothing or for text that is not a timestamp"
    (expect (org-canvas--submissions-status-parse-pulled-at nil) :to-be nil)
    (expect (org-canvas--submissions-status-parse-pulled-at "yesterday") :to-be nil)))

(describe "org-canvas--submissions-status-after-p"
  (let ((pulled (org-canvas--submissions-status-parse-pulled-at "<2026-09-12 Sat 10:30>")))
    (it "is true for a submission after the pull and false for one before"
      (expect (org-canvas--submissions-status-after-p "2026-09-16T14:16:00Z" pulled)
              :to-be-truthy)
      (expect (org-canvas--submissions-status-after-p "2026-09-01T14:16:00Z" pulled)
              :to-be nil))

    (it "is nil without a timestamp on either side"
      (expect (org-canvas--submissions-status-after-p nil pulled) :to-be nil)
      (expect (org-canvas--submissions-status-after-p "2026-09-16T14:16:00Z" nil) :to-be nil))

    (it "is nil when the Canvas timestamp does not parse"
      (cl-letf (((symbol-function 'date-to-time)
                 (lambda (_) (error "Invalid date"))))
        (expect (org-canvas--submissions-status-after-p "2026-09-16Tgarbage" pulled)
                :to-be nil)))

    (it "never parses text without a date prefix"
      (let ((parsed nil))
        (cl-letf (((symbol-function 'date-to-time)
                   (lambda (s) (setq parsed s) (current-time))))
          (expect (org-canvas--submissions-status-after-p "garbage" pulled) :to-be nil)
          (expect parsed :to-be nil))))))

;;;; Counting

(describe "org-canvas--submissions-status-counts"
  (let ((pulled (org-canvas--submissions-status-parse-pulled-at "<2026-09-12 Sat 10:30>"))
        (rows (list
               ;; Submitted before the pull, scored, posted.
               (test-sstatus--submission '(submitted_at . "2026-09-09T20:00:00Z")
                                         '(score . 3) '(posted_at . "2026-09-11T00:00:00Z"))
               ;; Submitted after the pull, late, unscored.
               (test-sstatus--submission '(submitted_at . "2026-09-16T14:16:00Z")
                                         '(late . t))
               ;; Missing, scored 0, not posted.
               (test-sstatus--submission '(score . 0) '(missing . t))
               ;; Excused counts as graded; never submitted.
               (test-sstatus--submission '(excused . t))
               ;; Nothing at all.
               (test-sstatus--submission))))
    (it "counts rows, submitted, graded, posted, late and missing"
      (let ((counts (org-canvas--submissions-status-counts rows pulled)))
        (expect (plist-get counts :rows) :to-equal 5)
        (expect (plist-get counts :submitted) :to-equal 2)
        (expect (plist-get counts :graded) :to-equal 3)
        (expect (plist-get counts :posted) :to-equal 1)
        (expect (plist-get counts :late) :to-equal 1)
        (expect (plist-get counts :missing) :to-equal 1)))

    (it "counts what arrived since the pull, and what is left to grade and to post"
      (let ((counts (org-canvas--submissions-status-counts rows pulled)))
        (expect (plist-get counts :new) :to-equal 1)
        (expect (plist-get counts :ungraded) :to-equal 1)
        (expect (plist-get counts :unposted) :to-equal 2)))

    (it "counts nothing as new for a column never pulled"
      (expect (plist-get (org-canvas--submissions-status-counts rows nil) :new)
              :to-equal 0))))

(defun test-sstatus--counts (&rest fields)
  "Counts with every key 0 except FIELDS, alternating keys and values."
  (let ((counts (list :rows 0 :submitted 0 :graded 0 :posted 0 :late 0
                      :missing 0 :new 0 :ungraded 0 :unposted 0)))
    (while fields
      (plist-put counts (pop fields) (pop fields)))
    counts))

(describe "org-canvas--submissions-status-next"
  (it "says pull when work is in and there is no grading file"
    (expect (org-canvas--submissions-status-next
             (test-sstatus--counts :submitted 3 :ungraded 3) nil)
            :to-equal '("pull")))

  (it "says nothing for a column nobody has submitted to and nobody pulled"
    (expect (org-canvas--submissions-status-next (test-sstatus--counts) nil)
            :to-equal '("-")))

  (it "puts refresh before grade, grade before post, and post before nothing"
    (expect (org-canvas--submissions-status-next
             (test-sstatus--counts :new 1 :ungraded 4 :unposted 2) "<x>")
            :to-equal '("refresh" 1))
    (expect (org-canvas--submissions-status-next
             (test-sstatus--counts :ungraded 4 :unposted 2) "<x>")
            :to-equal '("grade" 4))
    (expect (org-canvas--submissions-status-next
             (test-sstatus--counts :unposted 2) "<x>")
            :to-equal '("post" 2))
    (expect (org-canvas--submissions-status-next (test-sstatus--counts) "<x>")
            :to-equal '("-")))

  (it "does not ask for a pull of a column already pulled"
    (expect (org-canvas--submissions-status-next
             (test-sstatus--counts :submitted 3) "<x>")
            :to-equal '("-"))))

;;;; Columns

(describe "org-canvas--submissions-status-columns"
  (it "reports one row per assignment, the ones with work to do first"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 1 "Done")
              (test-sstatus--assignment 2 "To post")
              (test-sstatus--assignment 3 "To grade")
              (test-sstatus--assignment 4 "To refresh")
              (test-sstatus--assignment 5 "To pull")
              (test-sstatus--assignment 6 "Quiet"))
        (list (cons 1 (list (test-sstatus--submission
                             '(submitted_at . "2026-09-01T00:00:00Z") '(score . 1)
                             '(posted_at . "2026-09-02T00:00:00Z"))))
              (cons 2 (list (test-sstatus--submission
                             '(submitted_at . "2026-09-01T00:00:00Z") '(score . 1))))
              (cons 3 (list (test-sstatus--submission
                             '(submitted_at . "2026-09-01T00:00:00Z"))))
              (cons 4 (list (test-sstatus--submission
                             '(submitted_at . "2026-09-16T00:00:00Z"))))
              (cons 5 (list (test-sstatus--submission
                             '(submitted_at . "2026-09-01T00:00:00Z"))))
              (cons 6 (list (test-sstatus--submission))))
      (dolist (name '("Done" "To post" "To grade" "To refresh"))
        (test-sstatus--write-grading-file name "<2026-09-12 Sat 10:30>"))
      (let* ((columns (org-canvas--submissions-status-columns
                       (org-canvas--submissions-status-fetch-assignments)))
             (names (mapcar (lambda (c) (plist-get c :name)) columns))
             (nexts (mapcar (lambda (c) (car (plist-get c :next))) columns)))
        (expect names :to-equal '("To pull" "To refresh" "To grade" "To post" "Done" "Quiet"))
        (expect nexts :to-equal '("pull" "refresh" "grade" "post" "-" "-"))
        (expect (plist-get (nth 1 columns) :next) :to-equal '("refresh" 1))
        (expect (plist-get (nth 1 columns) :pulled-at) :to-equal "<2026-09-12 Sat 10:30>")
        (expect (plist-get (nth 0 columns) :pulled-at) :to-be nil))))

  (it "keeps Canvas's order among columns with the same thing to do"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 3 "C")
              (test-sstatus--assignment 1 "A")
              (test-sstatus--assignment 2 "B"))
        (list (cons 1 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z"))))
              (cons 2 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z"))))
              (cons 3 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z")))))
      (expect (mapcar (lambda (c) (plist-get c :name))
                      (org-canvas--submissions-status-columns
                       (org-canvas--submissions-status-fetch-assignments)))
              :to-equal '("C" "A" "B"))))

  (it "logs a column the token cannot read once, marks it, and goes on"
    (let ((warnings nil))
      (test-sstatus--with-course
          (list (test-sstatus--assignment 1 "Open")
                (test-sstatus--assignment 2 "Locked"))
          (list (cons 1 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z"))))
                (cons 2 'forbidden))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
          (let ((columns (org-canvas--submissions-status-columns
                          (org-canvas--submissions-status-fetch-assignments))))
            (expect (mapcar (lambda (c) (plist-get c :name)) columns)
                    :to-equal '("Open" "Locked"))
            (expect (plist-get (nth 1 columns) :counts) :to-be nil)
            (expect (plist-get (nth 1 columns) :next) :to-equal '("?"))
            (expect (plist-get (nth 0 columns) :next) :to-equal '("pull"))
            (expect (length warnings) :to-equal 1)
            (expect (car warnings) :to-match "Locked")))))))

;;;; Rendering

(describe "org-canvas--submissions-status-cells"
  (it "spells a readable, pulled column"
    (let ((org-canvas-time-zone "UTC")
          (column (list :name "Journal | 02" :due "2026-09-10T03:59:00Z"
                        :pulled-at "<2026-09-12 Sat 10:30>"
                        :counts (list :rows 5 :submitted 4 :graded 3 :posted 2
                                      :late 1 :missing 0 :new 1 :ungraded 1 :unposted 1)
                        :next '("refresh" 1))))
      (let ((cells (org-canvas--submissions-status-cells column)))
        (expect (nth 0 cells) :to-equal "Journal 02")
        (expect (nth 1 cells) :to-equal "<2026-09-10 Thu 03:59>")
        (expect (cl-subseq cells 2 8) :to-equal '("5" "4" "3" "2" "1" "0"))
        (expect (nth 8 cells) :to-equal "<2026-09-12 Sat 10:30>")
        (expect (nth 9 cells) :to-equal "1")
        (expect (nth 10 cells) :to-equal "refresh (1)"))))

  (it "spells an unreadable column with no due date and no grading file"
    (let ((column (list :name "Locked" :due nil :pulled-at nil :counts nil :next '("?"))))
      (expect (org-canvas--submissions-status-cells column)
              :to-equal '("Locked" "-" "?" "?" "?" "?" "?" "?" "-" "-" "?"))))

  (it "leaves New blank for a column never pulled"
    (let ((column (list :name "Fresh" :due nil :pulled-at nil
                        :counts (list :rows 1 :submitted 1 :graded 0 :posted 0
                                      :late 0 :missing 0 :new 0 :ungraded 1 :unposted 0)
                        :next '("pull"))))
      (expect (nthcdr 8 (org-canvas--submissions-status-cells column))
              :to-equal '("-" "-" "pull")))))

(describe "org-canvas--submissions-status-summary"
  (it "counts the columns by what they need"
    (let ((columns (list (list :counts t :next '("pull"))
                         (list :counts t :next '("pull"))
                         (list :counts t :next '("refresh" 1))
                         (list :counts t :next '("grade" 2))
                         (list :counts t :next '("post" 3))
                         (list :counts t :next '("-"))
                         (list :counts nil :next '("?")))))
      (expect (org-canvas--submissions-status-summary columns)
              :to-equal "7 columns | 2 to pull | 1 to refresh | 1 to grade | 1 to post | 1 unreadable"))))

;;;; Entry Point

(describe "org-canvas-submissions-status"
  (it "is an interactive command in the Submissions group of the menu"
    (expect (commandp 'org-canvas-submissions-status) :to-be-truthy)
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-submissions-status)
            :to-be-truthy))

  (it "renders the table, prints it in batch, and returns the columns"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 1 "Journal 02")
              (test-sstatus--assignment 2 "R4: Agency" '(due_at . nil)))
        (list (cons 1 (list (test-sstatus--submission
                             '(submitted_at . "2026-09-16T14:16:00Z"))
                            (test-sstatus--submission '(score . 0) '(missing . t))))
              (cons 2 (list (test-sstatus--submission))))
      (test-sstatus--write-grading-file "Journal 02" "<2026-09-12 Sat 10:30>")
      (let* ((printed nil)
             (columns (cl-letf (((symbol-function 'princ)
                                 (lambda (text &optional _) (push text printed))))
                        (org-canvas-submissions-status)))
             (text (with-current-buffer org-canvas--submissions-status-buffer-name
                     (buffer-string)))
             (rows (test-sstatus--rows text)))
        (expect (length columns) :to-equal 2)
        (expect (car (plist-get (car columns) :next)) :to-equal "refresh")
        (expect printed :to-equal (list text))
        (expect text :to-match "2 columns | 0 to pull | 1 to refresh | 0 to grade | 0 to post | 0 unreadable")
        (expect (car rows)
                :to-equal '("Journal 02" "<2026-09-10 Thu 03:59>" "2" "1" "1" "0" "0" "1"
                            "-" "-" "<2026-09-12 Sat 10:30>" "1" "refresh (1)"))
        (expect (cadr rows)
                :to-equal '("R4: Agency" "-" "1" "0" "0" "0" "0" "0" "-" "-" "-" "-" "-"))
        (expect (with-current-buffer org-canvas--submissions-status-buffer-name
                  (derived-mode-p 'org-mode))
                :to-be-truthy))))

  (it "writes nothing under the submissions directory"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 1 "Journal 02"))
        (list (cons 1 (list (test-sstatus--submission))))
      (org-canvas-submissions-status)
      (expect (directory-files org-canvas-submissions-directory nil "\\.org\\'")
              :to-equal nil))))

;;;; Document processor reports (issue #351)

(defun test-sstatus--report (type progress &optional result)
  "Return a GraphQL report node of TYPE at PROGRESS with RESULT."
  `((reportType . ,type) (processingProgress . ,progress)
    (result . ,(or result :null))))

(describe "the grading queue's Reports line (issue #351)"
  (it "sums the rows' reports over the columns and names where one failed"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 1 "R6")
              (test-sstatus--assignment 2 "R7")
              (test-sstatus--assignment 3 "Quiz")
              (test-sstatus--assignment 4 "Nobody"))
        (list (cons 1 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z"))))
              (cons 2 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z"))))
              (cons 3 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z"))))
              (cons 4 (list (test-sstatus--submission))))
      (let* ((test-sstatus--reports
              `((1 . ((101 ,(test-sstatus--report "originality" "Processed" "33%")
                           ,(test-sstatus--report "turnitin_aiwriting" "Processed" "0%"))
                      (102 ,(test-sstatus--report "originality" "Failed"))
                      (103 ,(test-sstatus--report "originality" "Pending"))))
                (2 . ((201 ,(test-sstatus--report "originality" "Failed"))))))
             (logged nil)
             (columns (cl-letf (((symbol-function 'org-canvas--log-info)
                                 (lambda (_logger fmt &rest args)
                                   (push (apply #'format fmt args) logged))))
                        (org-canvas-submissions-status)))
             (text (with-current-buffer org-canvas--submissions-status-buffer-name
                     (buffer-string))))
        (expect (plist-get (cl-find "R6" columns :key (lambda (c) (plist-get c :name))
                                    :test #'equal)
                           :reports)
                :to-equal '(:processed 1 :failed 1 :pending 1))
        (expect (plist-get (cl-find "Quiz" columns :key (lambda (c) (plist-get c :name))
                                    :test #'equal)
                           :reports)
                :to-be nil)
        (expect text :to-match
                "unreadable\nReports: 1 processed, 2 failed, 1 pending; failed in R6 (1), R7 (1)\n\n|")
        (expect logged :to-contain
                "[Submissions status] Reports: 1 processed, 2 failed, 1 pending; failed in R6 (1), R7 (1)")
        ;; Nothing was handed in on Nobody, so it was not asked.
        (expect (sort (copy-sequence test-sstatus--graphql-calls) #'<) :to-equal '(1 2 3)))))

  (it "has no Reports line when no column has a report"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 1 "Journal"))
        (list (cons 1 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z")))))
      (org-canvas-submissions-status)
      (expect (with-current-buffer org-canvas--submissions-status-buffer-name (buffer-string))
              :not :to-match "Reports:")))

  (it "leaves out a column whose reports cannot be read, after one warning"
    (let ((warnings nil))
      (test-sstatus--with-course
          (list (test-sstatus--assignment 1 "Open") (test-sstatus--assignment 2 "Refused"))
          (list (cons 1 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z"))))
                (cons 2 (list (test-sstatus--submission '(submitted_at . "2026-09-01T00:00:00Z")))))
        (let ((test-sstatus--reports
               `((1 . ((101 ,(test-sstatus--report "originality" "Processed" "5%"))))
                 (2 . refused))))
          (cl-letf (((symbol-function 'org-canvas--log-warning)
                     (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
            (let ((columns (org-canvas--submissions-status-columns
                            (org-canvas--submissions-status-fetch-assignments))))
              (expect (org-canvas--submissions-status-reports-line columns)
                      :to-equal "Reports: 1 processed, 0 failed, 0 pending")
              (expect (length warnings) :to-equal 1)
              (expect (car warnings) :to-match "assignment 2")))))))

  (it "does not ask about a column it could not read at all"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 2 "Locked"))
        (list (cons 2 'forbidden))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--submissions-status-columns
         (org-canvas--submissions-status-fetch-assignments)))
      (expect test-sstatus--graphql-calls :to-be nil))))

;;;; Score statistics (issue #352)

(describe "the grading queue's score statistics (issue #352)"
  (it "reads every column in one course-level query and keys it by id"
    (test-sstatus--with-course nil nil
      (let* ((test-sstatus--statistics
              '((1 . ((mean . 8.43) (median . 9.0)))
                (2 . nil)))
             (map (org-canvas--submissions-status-fetch-statistics)))
        (expect (length test-sstatus--statistics-calls) :to-equal 1)
        (expect (alist-get 'courseId (car test-sstatus--statistics-calls))
                :to-equal test-org-canvas-course-id)
        (expect (gethash "1" map) :to-equal '((mean . 8.43) (median . 9.0)))
        (expect (gethash "2" map) :to-be 'none))))

  (it "shows a filled statistic beside the counts, and a dash for none graded"
    (test-sstatus--with-course
        (list (test-sstatus--assignment 1 "R4")
              (test-sstatus--assignment 2 "R6")
              (test-sstatus--assignment 3 "Unlisted"))
        (list (cons 1 (list (test-sstatus--submission
                             '(submitted_at . "2026-09-01T00:00:00Z") '(score . 8)
                             '(posted_at . "2026-09-02T00:00:00Z"))))
              (cons 2 (list (test-sstatus--submission)))
              (cons 3 (list (test-sstatus--submission))))
      (let* ((test-sstatus--statistics
              '((1 . ((mean . 8.425) (median . 8.5))) (2 . nil)))
             (columns (org-canvas-submissions-status))
             (text (with-current-buffer org-canvas--submissions-status-buffer-name
                     (buffer-string)))
             (rows (test-sstatus--rows text)))
        (expect (length test-sstatus--statistics-calls) :to-equal 1)
        (expect (plist-get (car columns) :statistic)
                :to-equal '((mean . 8.425) (median . 8.5)))
        (expect (plist-get (nth 1 columns) :statistic) :to-be 'none)
        ;; A column the read did not name counts as nothing graded.
        (expect (plist-get (nth 2 columns) :statistic) :to-be 'none)
        (expect text :to-match "| Missing | Mean | Median | Pulled |")
        (expect (cl-subseq (assoc "R4" rows) 7 11)
                :to-equal '("0" "8.43" "8.5" "-"))
        (expect (cl-subseq (assoc "R6" rows) 7 11)
                :to-equal '("0" "-" "-" "-"))
        (expect (cl-subseq (assoc "Unlisted" rows) 8 10) :to-equal '("-" "-"))
        ;; The counts are the rows' own, whatever Canvas counted.
        (expect (nth 4 (assoc "R4" rows)) :to-equal "1")
        (expect text :to-match "departed student"))))

  (it "renders the queue without the statistics after one warning when the read fails"
    (let ((warnings nil))
      (test-sstatus--with-course
          (list (test-sstatus--assignment 1 "R4"))
          (list (cons 1 (list (test-sstatus--submission))))
        (let ((test-sstatus--statistics 'refused))
          (cl-letf (((symbol-function 'org-canvas--log-warning)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) warnings))))
            (let* ((columns (org-canvas-submissions-status))
                   (text (with-current-buffer
                             org-canvas--submissions-status-buffer-name
                           (buffer-string))))
              (expect (plist-get (car columns) :statistic) :to-be nil)
              (expect (length warnings) :to-equal 1)
              (expect (car warnings) :to-match "score statistics")
              (expect text :not :to-match "Mean")
              (expect (assoc "R4" (test-sstatus--rows text))
                      :to-equal '("R4" "<2026-09-10 Thu 03:59>" "1" "0" "0" "0"
                                  "0" "0" "-" "-" "-"))))))))

  (it "does not ask for statistics when there is no column"
    (test-sstatus--with-course nil nil
      (org-canvas-submissions-status)
      (expect test-sstatus--statistics-calls :to-be nil)))

  (it "spells a score to two decimals at most and a null mean as a dash"
    (expect (org-canvas--submissions-status-score-cell '((mean . 10.0)) 'mean)
            :to-equal "10")
    (expect (org-canvas--submissions-status-score-cell '((mean . 0.0)) 'mean)
            :to-equal "0")
    (expect (org-canvas--submissions-status-score-cell '((mean . 7.5)) 'mean)
            :to-equal "7.5")
    (expect (org-canvas--submissions-status-score-cell '((mean . :null)) 'mean)
            :to-equal "-")
    (expect (org-canvas--submissions-status-score-cell 'none 'median)
            :to-equal "-")))

(provide 'org-canvas-submissions-status-test)
;;; org-canvas-submissions-status-test.el ends here
