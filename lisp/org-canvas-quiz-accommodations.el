;;; org-canvas-quiz-accommodations.el --- Per-student extensions on a classic quiz -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module syncs quiz accommodations: the extra attempts, extra
;; time and manual unlock Canvas grants one student on one classic
;; quiz (Canvas calls them quiz extensions).  They are the quiz
;; counterpart of the student override rows on an assignment.
;;
;; FILE STRUCTURE
;; ==============
;; Under a classic quiz heading in quizzes.org, a named table:
;;
;;   #+NAME: accommodations
;;   | Student          | Extra attempts | Extra time | Unlocked |
;;   |------------------+----------------+------------+----------|
;;   | Lovelace, Ada    |              1 |         30 |          |
;;   | #123456          |                |         15 | yes      |
;;
;; Student is a person's heading title in people.org (resolved to
;; USER_ID) or a literal #id.  Extra time is in minutes.  Unlocked is
;; `yes' for a manual unlock, blank otherwise.  Columns are found by
;; their headers; a pulled table drops a column no row fills.
;;
;; SYNC
;; ====
;; `org-canvas-sync-quiz-accommodations' reads, for every quiz heading
;; that has a CANVAS_ID and a table, the quiz's submissions (which
;; carry each student's current extension) and sends one extension per
;; row whose values differ.  The values are absolute: Canvas replaces
;; what it holds, so sending zeros clears an extension.  A student who
;; has an extension on Canvas and no row is sent zeros: the table is
;; the whole set, as the overrides table is.  Dry run logs what it
;; would send and sends nothing.
;;
;; PULL
;; ====
;; The quiz pull emits the table from the submissions that carry any
;; extension, naming students through people.org (#id when unknown),
;; and removes a stale table when none do.  quizzes.el reaches the two
;; functions through `declare-function'; it never requires this file.
;;
;; PERSONAL DATA
;; =============
;; A student's name in quizzes.org travels with the course repository.
;; Write #id instead when the repository is shared.
;;
;; API
;; ===
;;   GET  /courses/:id/quizzes/:quiz_id/submissions   (rows wrapped
;;        under quiz_submissions; extra_attempts, extra_time,
;;        manually_unlocked per row)
;;   POST /courses/:id/quizzes/:quiz_id/extensions
;;        quiz_extensions: [{user_id, extra_attempts, extra_time,
;;        manually_unlocked}]

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(defvar org-canvas-quizzes-file)
(defvar org-canvas-people-file)

;;;; Table

(defconst org-canvas--accommodation-table-regexp
  "^#\\+NAME:[ \t]+accommodations[ \t]*$"
  "Match the `#+NAME:' line that opens an accommodations table.")

(defun org-canvas--accommodation-find-table (end)
  "Return the accommodations table between point and END, parsed, or nil.
The result is what `org-table-to-lisp' gives; nil without a table."
  (save-excursion
    (when (re-search-forward org-canvas--accommodation-table-regexp end t)
      (forward-line 1)
      (when (looking-at-p org-table-line-regexp)
        (org-table-to-lisp)))))

(defun org-canvas--accommodation-people-file ()
  "Return people.org when the roster module is loaded and the file exists."
  (let ((file (and (boundp 'org-canvas-people-file) org-canvas-people-file)))
    (and file (file-exists-p file) file)))

(defun org-canvas--accommodation-resolve-student (text)
  "Resolve TEXT, a person's heading title or #id, to a USER_ID string, or nil."
  (if (string-match "\\`#\\([0-9]+\\)\\'" text)
      (match-string 1 text)
    (org-canvas--heading-property-by-title
     (org-canvas--accommodation-people-file) text "USER_ID" "LEVEL=2")))

(defun org-canvas--accommodation-student-cell (user-id)
  "Render USER-ID as the person's heading title in people.org, or #id."
  (or (org-canvas--heading-title-by-property
       (org-canvas--accommodation-people-file) "USER_ID" user-id "LEVEL=2")
      (format "#%s" user-id)))

(defun org-canvas--accommodation-columns (header)
  "Return the column indexes of the attempts, time and unlocked cells in HEADER.
Each column is found by its title (Extra attempts, Extra time,
Unlocked, case aside); an unnamed one is absent (nil).  A header naming
none falls back to positions 1, 2 and 3."
  (let* ((titles (mapcar (lambda (cell) (downcase (string-trim cell))) header))
         (specs '(("extra attempts" . 1) ("extra time" . 2) ("unlocked" . 3)))
         (named (cl-some (lambda (spec) (member (car spec) titles)) specs)))
    (mapcar (lambda (spec)
              (if named
                  (cl-position (car spec) titles :test #'string=)
                (cdr spec)))
            specs)))

(defun org-canvas--accommodation-number-cell (cell)
  "Return CELL as a non-negative integer; blank is 0.
A cell that is not a number signals `org-canvas-config-error', since a
silently misread accommodation would clear or grant the wrong thing."
  (let ((text (string-trim (or cell ""))))
    (cond
     ((string-empty-p text) 0)
     ((string-match-p "\\`[0-9]+\\'" text) (string-to-number text))
     (t (org-canvas--signal 'org-canvas-config-error
          "Accommodation cell '%s' is not a whole number" text)))))

(defun org-canvas--accommodation-unlocked-cell (cell)
  "Return non-nil for a CELL that means a manual unlock (yes, true or x)."
  (and cell (member (downcase (string-trim cell)) '("yes" "true" "x")) t))

(defun org-canvas--accommodation-parse-row (row columns)
  "Parse one table ROW with COLUMNS into an accommodation plist, or nil.
Nil, with one warning, when the student resolves to nothing."
  (let* ((cell (lambda (col) (and col (nth col row))))
         (student (string-trim (or (nth 0 row) "")))
         (user-id (and (not (string-empty-p student))
                       (org-canvas--accommodation-resolve-student student))))
    (if (not user-id)
        (progn
          (org-canvas--log-warning org-canvas--logger
            "[Accommodations] Could not resolve student '%s' (pull people first, or write #id)"
            student)
          nil)
      (list :user-id user-id
            :extra-attempts (org-canvas--accommodation-number-cell (funcall cell (nth 0 columns)))
            :extra-time (org-canvas--accommodation-number-cell (funcall cell (nth 1 columns)))
            :unlocked (org-canvas--accommodation-unlocked-cell (funcall cell (nth 2 columns)))))))

(defun org-canvas--accommodation-parse-table (table)
  "Parse an accommodations TABLE (from `org-table-to-lisp') into plists.
Each plist carries :user-id (a string), :extra-attempts, :extra-time
\(integers, 0 when blank) and :unlocked (a boolean).  A row whose
student resolves to nothing is skipped with a warning."
  (let ((columns (org-canvas--accommodation-columns (car table)))
        (rows nil))
    (dolist (row (cdr table))
      (unless (eq row 'hline)
        (when-let* ((parsed (org-canvas--accommodation-parse-row row columns)))
          (push parsed rows))))
    (nreverse rows)))

;;;; Fetching

(defun org-canvas--accommodation-fetch-submissions (quiz-id)
  "Return QUIZ-ID's submissions as a list.
The endpoint wraps its rows under `quiz_submissions'; the paginated
helper unwraps each page."
  (let ((org-canvas--api-unwrap-key 'quiz_submissions))
    (org-canvas-api-request-all-pages
     'GET (org-canvas-api-course-endpoint "quizzes/%s/submissions" quiz-id))))

(defun org-canvas--accommodation-from-submission (submission)
  "Return SUBMISSION's extension as a plist, or nil when it carries none.
The plist has the shape `org-canvas--accommodation-parse-table' gives."
  (let ((attempts (alist-get 'extra_attempts submission))
        (time (alist-get 'extra_time submission))
        (unlocked (eq (alist-get 'manually_unlocked submission) t)))
    (when (or (and (numberp attempts) (> attempts 0))
              (and (numberp time) (> time 0))
              unlocked)
      (list :user-id (format "%s" (alist-get 'user_id submission))
            :extra-attempts (if (numberp attempts) attempts 0)
            :extra-time (if (numberp time) time 0)
            :unlocked unlocked))))

(defun org-canvas--accommodation-fetch (quiz-id)
  "Return the extensions on Canvas for QUIZ-ID, one plist per student.
Students without an extension are left out.  An API error is recorded
on the pull summary and answered with nil, so a quiz whose submissions
cannot be read still pulls."
  (condition-case err
      (delq nil (mapcar #'org-canvas--accommodation-from-submission
                        (org-canvas--accommodation-fetch-submissions quiz-id)))
    (org-canvas-error
     (org-canvas--pull-summary-record
      :file (and (boundp 'org-canvas-quizzes-file)
                 (file-name-nondirectory org-canvas-quizzes-file))
      :item (format "quiz %s accommodations" quiz-id)
      :error (error-message-string err))
     nil)))

;;;; Emitting

(defun org-canvas--accommodation-delete-table ()
  "Delete the accommodations table in the body of the entry at point, if any.
Only the entry's own body is searched, up to its first child heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (when (re-search-forward org-canvas--accommodation-table-regexp end t)
        (let ((start (line-beginning-position)))
          (forward-line 1)
          (while (and (< (point) end) (looking-at "^|"))
            (forward-line 1))
          (while (and (< (point) end) (looking-at "^[ \t]*$"))
            (forward-line 1))
          (delete-region start (point)))))))

(defun org-canvas--accommodation-build-row (row)
  "Render the accommodation plist ROW as four table cells."
  (list (org-canvas--accommodation-student-cell (plist-get row :user-id))
        (if (> (plist-get row :extra-attempts) 0)
            (number-to-string (plist-get row :extra-attempts)) "")
        (if (> (plist-get row :extra-time) 0)
            (number-to-string (plist-get row :extra-time)) "")
        (if (plist-get row :unlocked) "yes" "")))

(defun org-canvas--accommodation-emit-table (rows)
  "Insert a `#+NAME: accommodations' table for ROWS at point.
ROWS are accommodation plists; nothing is inserted for none.  A column
no row fills is dropped, as the overrides table does."
  (when rows
    (let* ((cells (mapcar #'org-canvas--accommodation-build-row rows))
           (titles '("Student" "Extra attempts" "Extra time" "Unlocked"))
           (keep (cl-loop for i from 0 below 4
                          collect (or (= i 0)
                                      (cl-some (lambda (c) (not (string-empty-p (nth i c))))
                                               cells))))
           (pick (lambda (list) (cl-loop for x in list for k in keep when k collect x))))
      (insert "#+NAME: accommodations\n")
      (insert "| " (mapconcat #'identity (funcall pick titles) " | ") " |\n")
      (insert "|" (mapconcat (lambda (_) "---") (funcall pick titles) "+") "|\n")
      (dolist (c cells)
        (insert "| " (mapconcat #'identity (funcall pick c) " | ") " |\n"))
      (insert "\n"))))

(defun org-canvas--accommodation-write-table (quiz-id)
  "Replace the accommodations table under the quiz at point with Canvas's.
QUIZ-ID names the quiz.  The old table goes whether or not a new one
follows, so a cleared extension disappears on pull."
  (let ((rows (org-canvas--accommodation-fetch quiz-id)))
    (save-excursion
      (org-back-to-heading t)
      (org-canvas--accommodation-delete-table)
      (when rows
        (org-end-of-meta-data t)
        (org-canvas--accommodation-emit-table rows)
        (save-excursion
          (forward-line -1)
          (when (org-at-table-p) (org-table-align)))))))

;;;; Pushing

(defun org-canvas--accommodation-payload (row)
  "Build the extensions payload for the accommodation plist ROW."
  `((quiz_extensions
     . [((user_id . ,(string-to-number (plist-get row :user-id)))
         (extra_attempts . ,(plist-get row :extra-attempts))
         (extra_time . ,(plist-get row :extra-time))
         (manually_unlocked . ,(if (plist-get row :unlocked) t :json-false)))])))

(defun org-canvas--accommodation-same-p (row existing)
  "Return non-nil when ROW and EXISTING grant the same extension.
EXISTING is nil for a student Canvas holds nothing for, which is the
same as all zeros."
  (let ((e (or existing (list :extra-attempts 0 :extra-time 0 :unlocked nil))))
    (and (= (plist-get row :extra-attempts) (plist-get e :extra-attempts))
         (= (plist-get row :extra-time) (plist-get e :extra-time))
         (eq (and (plist-get row :unlocked) t) (and (plist-get e :unlocked) t)))))

(defun org-canvas--accommodation-label (row)
  "Describe ROW for the log: the student and the three values."
  (format "student %s (attempts +%d, time +%d min%s)"
          (plist-get row :user-id) (plist-get row :extra-attempts)
          (plist-get row :extra-time)
          (if (plist-get row :unlocked) ", unlocked" "")))

(defun org-canvas--accommodation-cleared (existing)
  "Return the all-zero accommodation that clears EXISTING."
  (list :user-id (plist-get existing :user-id)
        :extra-attempts 0 :extra-time 0 :unlocked nil))

(defun org-canvas--accommodation-push-one (endpoint row verb)
  "POST ROW's extension to ENDPOINT; VERB names the change in the log.
Returns t when sent (or previewed under `org-canvas--dry-run'), nil
when the request failed."
  (let ((label (org-canvas--accommodation-label row)))
    (condition-case err
        (progn
          (if org-canvas--dry-run
              (org-canvas--log-info org-canvas--logger
                "[DRY-RUN] Would %s accommodation for %s" verb label)
            (org-canvas--log-info org-canvas--logger
              "[Accommodations] %s for %s" (capitalize verb) label)
            (org-canvas-api-request 'POST endpoint
                                    :data (org-canvas--accommodation-payload row)))
          t)
      (error
       (org-canvas--log-error org-canvas--logger
         "[Accommodations] Failed for %s: %s" label (error-message-string err))
       nil))))

(defun org-canvas--accommodation-sync-for-quiz (quiz-id rows)
  "Reconcile the accommodation ROWS of QUIZ-ID with the extensions on Canvas.
A row whose values differ from Canvas's is sent; a student with an
extension on Canvas and no row is sent zeros.  Returns (SET CLEARED)
as counts of requests sent, or previewed under the dry run."
  (let* ((endpoint (org-canvas-api-course-endpoint "quizzes/%s/extensions" quiz-id))
         (existing (org-canvas--accommodation-fetch quiz-id))
         (find (lambda (id) (cl-find id existing :key (lambda (e) (plist-get e :user-id))
                                     :test #'equal)))
         (set 0) (cleared 0))
    (dolist (row rows)
      (unless (org-canvas--accommodation-same-p row (funcall find (plist-get row :user-id)))
        (when (org-canvas--accommodation-push-one endpoint row "set")
          (cl-incf set))))
    (dolist (e existing)
      (unless (cl-find (plist-get e :user-id) rows
                       :key (lambda (r) (plist-get r :user-id)) :test #'equal)
        (when (org-canvas--accommodation-push-one
               endpoint (org-canvas--accommodation-cleared e) "clear")
          (cl-incf cleared))))
    (list set cleared)))

;;;; Commands

(defun org-canvas--accommodation-sync-entry ()
  "Sync the accommodations table of the quiz heading at point.
Returns (SET CLEARED), or nil when the heading has no CANVAS_ID or no
table."
  (let* ((canvas-id (org-canvas-org-get-property (point) "CANVAS_ID"))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (end (save-excursion (org-end-of-subtree t) (point)))
         (table (and canvas-id (org-canvas--accommodation-find-table end))))
    (when table
      (org-canvas--log-info org-canvas--logger
        "[Accommodations] Processing '%s' (ID: %s)" title canvas-id)
      (org-canvas--accommodation-sync-for-quiz
       canvas-id (org-canvas--accommodation-parse-table table)))))

(defun org-canvas--accommodation-report (quizzes set cleared)
  "Log and message the totals: QUIZZES processed, SET and CLEARED requests."
  (org-canvas--log-info org-canvas--logger "========================================")
  (org-canvas--log-info org-canvas--logger ">>> ACCOMMODATION SYNC COMPLETE")
  (org-canvas--log-info org-canvas--logger "Quizzes: %d | Set: %d | Cleared: %d"
                        quizzes set cleared)
  (org-canvas--log-info org-canvas--logger "========================================")
  (org-canvas--sync-record-feature-stats "Quiz Accommodations"
    (if org-canvas--dry-run
        (list :dry-run (+ set cleared))
      (list :success (+ set cleared))))
  (message "Accommodation sync: %d quizzes, %d set, %d cleared.%s"
           quizzes set cleared
           (if org-canvas--dry-run " (dry run — nothing sent)" "")))

;;;###autoload
(defun org-canvas-sync-quiz-accommodations ()
  "Sync the per-student accommodations of every classic quiz.
Reads the `#+NAME: accommodations' table under each quiz heading in
quizzes.org that carries a CANVAS_ID and reconciles it with the
extensions Canvas holds: the table is the whole set."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((file (expand-file-name org-canvas-quizzes-file))
        (quizzes 0) (set 0) (cleared 0))
    (unless (file-exists-p file)
      (org-canvas--signal 'org-canvas-config-error "Quizzes file not found: %s" file))
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> STARTING ACCOMMODATION SYNC")
    (org-canvas--log-info org-canvas--logger "File: %s" file)
    (org-canvas--log-info org-canvas--logger "========================================")
    (with-current-buffer (org-canvas--find-file-noselect file)
      (let ((markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))
        (dolist (marker markers)
          (goto-char (marker-position marker))
          (when-let* ((counts (org-canvas--accommodation-sync-entry)))
            (cl-incf quizzes)
            (cl-incf set (nth 0 counts))
            (cl-incf cleared (nth 1 counts))))
        (dolist (m markers) (set-marker m nil))))
    (org-canvas--accommodation-report quizzes set cleared)))

;;;###autoload
(defun org-canvas-sync-quiz-accommodations-at-point ()
  "Sync the accommodations table of the quiz heading at point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (org-canvas-clear-log)
  (save-excursion
    (org-back-to-heading t)
    (while (> (org-current-level) 1) (org-up-heading-safe))
    (let ((counts (org-canvas--accommodation-sync-entry)))
      (unless counts
        (user-error "This heading has no CANVAS_ID or no accommodations table"))
      (org-canvas--accommodation-report 1 (nth 0 counts) (nth 1 counts)))))

(provide 'org-canvas-quiz-accommodations)
;;; org-canvas-quiz-accommodations.el ends here
