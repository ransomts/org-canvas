;;; org-canvas-submissions.el --- View student submissions from Canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; Browse student submissions for Canvas assignments without leaving Emacs.
;; Provides two views (summary table and per-student detail) with toggle,
;; comment posting, and on-demand file download.
;;
;; VIEWS
;; =====
;; Summary: Org table with student name, status, timestamp, score
;; Detail:  Per-student headings with body, attachments, comments, rubric
;;
;; COMMANDS
;; ========
;; org-canvas-pull-submissions          - Select assignment, fetch, render
;; org-canvas-submissions-refresh       - Re-fetch current buffer
;; org-canvas-submissions-toggle-view   - Switch summary <-> detail
;; org-canvas-submissions-add-comment   - Post comment on student submission
;; org-canvas-submissions-download-attachments - Download files locally
;;
;; MINOR MODE KEYS
;; ===============
;; g - refresh
;; v - toggle view
;; d - download attachments
;; c - add comment
;;
;; S - push grade changes to Canvas
;;
;; GRADE WRITING
;; =============
;; Edit :SCORE: properties (detail view) or Score column cells (summary
;; view) in-place, then press S to push all changes.  The buffer is
;; ephemeral so direct editing is safe.  A single change uses PUT;
;; multiple changes use the bulk update_grades endpoint.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-submissions-directory
  (org-canvas--path "submissions/")
  "Directory for submission view files."
  :type 'directory
  :group 'org-canvas)

(defcustom org-canvas-submissions-default-view 'summary
  "Default view when pulling submissions.
Either `summary' (table) or `detail' (per-student headings)."
  :type '(choice (const :tag "Summary table" summary)
                 (const :tag "Detail headings" detail))
  :group 'org-canvas)

;;;; Data Fetching

(defun org-canvas--submissions-fetch-assignments ()
  "Fetch assignment list for interactive selection.
Returns a list of alists with `id' and `name' keys."
  (org-canvas-api-request-all-pages
   'GET (org-canvas-api-course-endpoint "assignments")
   '(("order_by" . "name"))))

(defun org-canvas--submissions-fetch-for-assignment (assignment-id)
  "Fetch all submissions for ASSIGNMENT-ID with comments, rubric, and user info.
Returns a list of submission alists."
  (org-canvas-api-request-all-pages
   'GET (org-canvas-api-course-endpoint "assignments/%s/submissions" assignment-id)
   '(("include[]" . "submission_comments,rubric_assessment,user"))))

;;;; Status Normalization

(defun org-canvas--submissions-normalize-status (submission)
  "Derive a status symbol from SUBMISSION alist.
Returns one of: submitted, late, missing, graded, pending_review, unsubmitted."
  (let ((state (alist-get 'workflow_state submission))
        (late (alist-get 'late submission))
        (missing (alist-get 'missing submission)))
    (cond
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
      (let ((status (org-canvas--submissions-normalize-status sub))
            (score (alist-get 'score sub)))
        (pcase status
          ('submitted (cl-incf submitted))
          ('missing (cl-incf missing))
          ('late (cl-incf late))
          ('graded (cl-incf graded)))
        (when (and score (numberp score))
          (push score scores))
        (unless points
          (let ((pts (alist-get 'points_possible
                                (alist-get 'assignment sub))))
            (when (and pts (numberp pts))
              (setq points pts))))))
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
Falls back to `name' or \"Unknown\"."
  (let ((user (alist-get 'user submission)))
    (or (alist-get 'sortable_name user)
        (alist-get 'name user)
        "Unknown")))

(defun org-canvas--submissions-user-id (submission)
  "Extract user ID from SUBMISSION."
  (let ((user (alist-get 'user submission)))
    (alist-get 'id user)))

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

(defun org-canvas--submissions-render-detail (assignment-name assignment-id submissions)
  "Render detail view with per-student headings into current buffer.
ASSIGNMENT-NAME and ASSIGNMENT-ID identify the assignment.
SUBMISSIONS is the list of submission alists."
  (let ((sorted (sort (copy-sequence submissions)
                      (lambda (a b)
                        (string< (org-canvas--submissions-user-sortable-name a)
                                 (org-canvas--submissions-user-sortable-name b))))))
    (erase-buffer)
    (insert (format "#+TITLE: Submissions: %s\n" assignment-name))
    (insert (format "#+PROPERTY: CANVAS_ASSIGNMENT_ID %s\n\n" assignment-id))
    (dolist (sub sorted)
      (org-canvas--submissions-render-detail-entry sub))))

(defun org-canvas--submissions-render-detail-entry (submission)
  "Render a single SUBMISSION as an Org heading with properties."
  (let ((name (org-canvas--submissions-user-sortable-name submission))
        (user-id (org-canvas--submissions-user-id submission))
        (sub-id (alist-get 'id submission))
        (status (org-canvas--submissions-normalize-status submission))
        (score (alist-get 'score submission))
        (submitted-at (org-canvas--submissions-format-submitted-at submission))
        (body (alist-get 'body submission))
        (attachments (alist-get 'attachments submission))
        (comments (alist-get 'submission_comments submission))
        (rubric (alist-get 'rubric_assessment submission)))
    (insert (format "* %s\n" name))
    (insert ":PROPERTIES:\n")
    (when user-id
      (insert (format ":USER_ID: %s\n" user-id)))
    (when sub-id
      (insert (format ":SUBMISSION_ID: %s\n" sub-id)))
    (insert (format ":STATUS: %s\n" status))
    (when (and score (numberp score))
      (insert (format ":SCORE: %s\n" (org-canvas--submissions-format-number score))))
    (when (not (string-empty-p submitted-at))
      (insert (format ":SUBMITTED_AT: %s\n" submitted-at)))
    (insert ":END:\n")
    (when (and body (stringp body) (not (string-empty-p body)))
      (insert (format "\n%s\n" body)))
    (org-canvas--submissions-render-attachments attachments)
    (org-canvas--submissions-render-comments comments)
    (org-canvas--submissions-render-rubric rubric)
    (insert "\n")))

(defun org-canvas--submissions-render-attachments (attachments)
  "Render ATTACHMENTS list as a sub-heading with links."
  (when (and attachments (> (length attachments) 0))
    (insert "\n** Attachments\n")
    (let ((att-list (append attachments nil)))
      (dolist (att att-list)
        (let ((url (alist-get 'url att))
              (filename (alist-get 'display_name att)))
          (when (and url filename)
            (insert (format "- [[%s][%s]]\n" url filename))))))))

(defun org-canvas--submissions-render-comments (comments)
  "Render submission COMMENTS as a sub-heading."
  (when (and comments (> (length comments) 0))
    (insert "\n** Comments\n")
    (let ((comment-list (append comments nil)))
      (dolist (comment comment-list)
        (let ((author (alist-get 'author_name comment))
              (text (alist-get 'comment comment))
              (created (alist-get 'created_at comment)))
          (insert (format "- *%s* %s :: %s\n"
                          (or author "Unknown")
                          (or (org-canvas--iso8601-to-org-timestamp created) "")
                          (or text ""))))))))

(defun org-canvas--submissions-render-rubric (rubric)
  "Render RUBRIC assessment as a sub-heading with table."
  (when rubric
    (insert "\n** Rubric Assessment\n")
    (insert "| Criterion | Rating | Score |\n")
    (insert "|-----------+--------+-------|\n")
    (let ((criteria (if (hash-table-p rubric)
                        (let (pairs)
                          (maphash (lambda (k v) (push (cons k v) pairs)) rubric)
                          pairs)
                      rubric)))
      (dolist (entry criteria)
        (let* ((data (cdr entry))
               (rating (or (alist-get 'rating_description data)
                           (alist-get 'rating_id data) ""))
               (points (alist-get 'points data)))
          (insert (format "| %s | %s | %s |\n"
                          (car entry)
                          rating
                          (if (numberp points)
                              (org-canvas--submissions-format-number points)
                            ""))))))
    (org-table-align)))

;;;; View Toggle

(defun org-canvas-submissions-toggle-view ()
  "Toggle between summary and detail views using cached data."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
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

;;;; Comment Writing

(defun org-canvas-submissions-add-comment ()
  "Post a text comment on the submission at point.
Must be on or inside a student heading in detail view."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (save-excursion
    (org-back-to-heading t)
    (let ((sub-id (org-entry-get (point) "SUBMISSION_ID"))
          (user-name (org-get-heading t t t t)))
      (unless sub-id
        (user-error "No SUBMISSION_ID at point"))
      (let ((text (read-string (format "Comment for %s: " user-name))))
        (when (string-empty-p text)
          (user-error "Empty comment"))
        (when (y-or-n-p (format "Post comment to %s? " user-name))
          (org-canvas--submissions-post-comment
           org-canvas-submissions--assignment-id sub-id text)
          (org-canvas--submissions-append-comment-to-buffer user-name text)
          (message "Comment posted for %s" user-name))))))

(defun org-canvas--submissions-post-comment (assignment-id submission-id text)
  "POST TEXT as a comment on SUBMISSION-ID for ASSIGNMENT-ID."
  (let ((url (org-canvas-api-course-endpoint
              "assignments/%s/submissions/%s" assignment-id submission-id)))
    (org-canvas-api-request 'PUT url
      :data `((comment . ((text_comment . ,text)))))))

(defun org-canvas--submissions-append-comment-to-buffer (user-name text)
  "Append posted comment TEXT to the Comments section for USER-NAME.
Searches from point for the ** Comments sub-heading under the current heading."
  (let ((inhibit-read-only t))
    (save-excursion
      (org-back-to-heading t)
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (if (re-search-forward "^\\*\\* Comments$" end t)
            (progn
              (forward-line 1)
              ;; Find end of comments section
              (while (and (< (point) end)
                          (looking-at "^- "))
                (forward-line 1))
              (insert (format "- *You* %s :: %s\n"
                              (format-time-string "<%Y-%m-%d %a %H:%M>")
                              text)))
          ;; No Comments section yet — create one
          (goto-char end)
          (insert "\n** Comments\n")
          (insert (format "- *You* %s :: %s\n"
                          (format-time-string "<%Y-%m-%d %a %H:%M>")
                          text)))))))

;;;; File Download

(defun org-canvas-submissions-download-attachments ()
  "Download attachments for the submission at point.
Files are saved to submissions/files/<assignment>/<student>/."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((user-name (org-get-heading t t t t))
           (end (save-excursion (org-end-of-subtree t) (point)))
           (assignment-name org-canvas-submissions--assignment-name)
           (download-dir (expand-file-name
                          (format "submissions/files/%s/%s/"
                                  (org-canvas--submissions-sanitize-filename assignment-name)
                                  (org-canvas--submissions-sanitize-filename user-name))
                          (or org-canvas-directory
                              (file-name-directory
                               (directory-file-name org-canvas-submissions-directory)))))
           (urls nil))
      ;; Collect attachment URLs from the Attachments sub-heading
      (save-excursion
        (when (re-search-forward "^\\*\\* Attachments$" end t)
          (let ((att-end (save-excursion
                           (if (re-search-forward "^\\*\\*" end t)
                               (line-beginning-position)
                             end))))
            (while (re-search-forward "\\[\\[\\(https?://[^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" att-end t)
              (push (cons (match-string 1) (match-string 2)) urls)))))
      (unless urls
        (user-error "No attachments found for %s" user-name))
      (make-directory download-dir t)
      (dolist (url-pair (nreverse urls))
        (let ((url (car url-pair))
              (filename (cdr url-pair)))
          (message "Downloading %s..." filename)
          (org-canvas--submissions-download-file url download-dir filename)))
      (message "Downloaded %d file(s) to %s" (length urls) download-dir))))

(defun org-canvas--submissions-download-file (url directory filename)
  "Download URL to DIRECTORY as FILENAME using Bearer auth."
  (let ((output-path (expand-file-name filename directory)))
    (url-copy-file
     (concat url
             (if (string-match-p "\\?" url) "&" "?")
             "access_token=" org-canvas-api-token)
     output-path t)))

(defun org-canvas--submissions-sanitize-filename (name)
  "Sanitize NAME for use as a directory/filename.
Replaces problematic characters with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" name))

;;;; Entry Points

;;;###autoload
(defun org-canvas-pull-submissions ()
  "Select an assignment and view its submissions.
Prompts for assignment selection, fetches submissions, and renders
to a buffer in `org-canvas-submissions-directory'."
  (interactive)
  (let* ((assignments (org-canvas--submissions-fetch-assignments))
         (names (mapcar (lambda (a) (alist-get 'name a)) assignments))
         (selected-name (completing-read "Assignment: " names nil t))
         (selected (cl-find-if (lambda (a)
                                 (equal (alist-get 'name a) selected-name))
                               assignments))
         (assignment-id (number-to-string (alist-get 'id selected)))
         (submissions (org-canvas--submissions-fetch-for-assignment assignment-id)))
    (org-canvas--submissions-display
     selected-name assignment-id submissions
     org-canvas-submissions-default-view)))

(defun org-canvas-submissions-refresh ()
  "Re-fetch and re-render submissions for the current buffer."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (let ((id org-canvas-submissions--assignment-id)
        (name org-canvas-submissions--assignment-name)
        (view org-canvas-submissions--current-view))
    (unless id
      (user-error "No assignment ID in this buffer"))
    (message "Refreshing submissions for %s..." name)
    (let ((submissions (org-canvas--submissions-fetch-for-assignment id)))
      (org-canvas--submissions-display name id submissions view))))

(defun org-canvas--submissions-display (assignment-name assignment-id submissions view)
  "Display SUBMISSIONS in a buffer.
ASSIGNMENT-NAME and ASSIGNMENT-ID identify the assignment.
VIEW is `summary' or `detail'."
  (let* ((dir (expand-file-name org-canvas-submissions-directory))
         (filename (format "%s.org"
                           (org-canvas--submissions-sanitize-filename assignment-name)))
         (filepath (expand-file-name filename dir))
         (buf (get-buffer-create (format "*submissions: %s*" assignment-name))))
    (make-directory dir t)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (org-mode)
        (if (eq view 'summary)
            (org-canvas--submissions-render-summary
             assignment-name assignment-id submissions)
          (org-canvas--submissions-render-detail
           assignment-name assignment-id submissions))
        (goto-char (point-min))
        (setq-local org-canvas-submissions--assignment-name assignment-name)
        (setq-local org-canvas-submissions--assignment-id assignment-id)
        (setq-local org-canvas-submissions--data submissions)
        (setq-local org-canvas-submissions--current-view view)
        (setq-local org-canvas-submissions--original-scores
                    (org-canvas--submissions-snapshot-scores submissions))
        (org-canvas-submissions-mode 1)))
    (switch-to-buffer buf)))

;;;; Minor Mode

(defvar-local org-canvas-submissions--assignment-name nil
  "Name of the assignment shown in this buffer.")

(defvar-local org-canvas-submissions--assignment-id nil
  "Canvas ID of the assignment shown in this buffer.")

(defvar-local org-canvas-submissions--data nil
  "Cached submission data for toggle/refresh.")

(defvar-local org-canvas-submissions--current-view nil
  "Current view: `summary' or `detail'.")

(defvar-local org-canvas-submissions--original-scores nil
  "Alist of (user-id . score-string) captured at render time for change detection.")

(defvar org-canvas-submissions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-canvas-submissions-refresh)
    (define-key map (kbd "v") #'org-canvas-submissions-toggle-view)
    (define-key map (kbd "d") #'org-canvas-submissions-download-attachments)
    (define-key map (kbd "c") #'org-canvas-submissions-add-comment)
    (define-key map (kbd "S") #'org-canvas-submissions-push-grades)
    map)
  "Keymap for `org-canvas-submissions-mode'.")

(define-minor-mode org-canvas-submissions-mode
  "Minor mode for viewing Canvas submissions.
\\{org-canvas-submissions-mode-map}"
  :lighter " Submissions"
  :keymap org-canvas-submissions-mode-map)

;;;; Grade Writing

(defun org-canvas--submissions-snapshot-scores (submissions)
  "Build an alist of (user-id . score-string) from SUBMISSIONS.
Score-string is the formatted number or nil when no score."
  (mapcar (lambda (sub)
            (let ((uid (org-canvas--submissions-user-id sub))
                  (score (alist-get 'score sub)))
              (cons uid (when (and score (numberp score))
                          (org-canvas--submissions-format-number score)))))
          submissions))

(defun org-canvas--submissions-parse-score (score-string)
  "Parse SCORE-STRING into a numeric string suitable for Canvas posted_grade.
Handles \"92\", \"85.5\", \"95/100\" (extracts numerator), trims whitespace.
Returns nil for non-numeric or empty input."
  (when (and score-string (stringp score-string))
    (let ((trimmed (string-trim score-string)))
      (cond
       ((string-empty-p trimmed) nil)
       ((string-match "^\\([0-9]+\\.?[0-9]*\\)/[0-9]" trimmed)
        (match-string 1 trimmed))
       ((string-match "^[0-9]+\\.?[0-9]*$" trimmed)
        trimmed)
       (t nil)))))

(defun org-canvas--submissions-collect-grade-changes ()
  "Return a list of grade diffs from the current buffer.
Each element is a plist (:user-id ID :name NAME :old-score OLD :new-score NEW)."
  (if (eq org-canvas-submissions--current-view 'detail)
      (org-canvas--submissions-collect-detail-changes)
    (org-canvas--submissions-collect-summary-changes)))

(defun org-canvas--submissions-collect-detail-changes ()
  "Return grade diffs from detail view by reading :SCORE: and :USER_ID: properties."
  (let ((changes nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let* ((user-id-str (org-entry-get (point) "USER_ID"))
               (user-id (when user-id-str (string-to-number user-id-str)))
               (name (org-get-heading t t t t))
               (score-raw (org-entry-get (point) "SCORE"))
               (new-score (org-canvas--submissions-parse-score score-raw))
               (old-score (alist-get user-id org-canvas-submissions--original-scores)))
          (when (and user-id (not (equal new-score old-score)))
            (push (list :user-id user-id :name name
                        :old-score old-score :new-score new-score)
                  changes)))
        (forward-line 1)))
    (nreverse changes)))

(defun org-canvas--submissions-collect-summary-changes ()
  "Return grade diffs from summary view by parsing org-table rows."
  (let ((changes nil)
        (name-to-uid (org-canvas--submissions-build-name-uid-map)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^|[^-+]" nil t)
        (beginning-of-line)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match
                 "^| *\\([^|]+?\\) *| [^|]* | [^|]* | *\\([^|]*?\\) *|$"
                 line)
            (let* ((name (string-trim (match-string 1 line)))
                   (score-cell (string-trim (match-string 2 line)))
                   (new-score (org-canvas--submissions-parse-score score-cell))
                   (user-id (cdr (assoc name name-to-uid)))
                   (old-score (when user-id
                                (alist-get user-id
                                           org-canvas-submissions--original-scores))))
              (when (and user-id
                         (not (equal name "Student"))
                         (not (equal new-score old-score)))
                (push (list :user-id user-id :name name
                            :old-score old-score :new-score new-score)
                      changes)))))
        (forward-line 1)))
    (nreverse changes)))

(defun org-canvas--submissions-build-name-uid-map ()
  "Build an alist of (sortable-name . user-id) from cached submission data."
  (mapcar (lambda (sub)
            (cons (org-canvas--submissions-user-sortable-name sub)
                  (org-canvas--submissions-user-id sub)))
          org-canvas-submissions--data))

(defun org-canvas--submissions-push-single-grade (assignment-id user-id score)
  "Push SCORE for USER-ID on ASSIGNMENT-ID via PUT."
  (let ((url (org-canvas-api-course-endpoint
              "assignments/%s/submissions/%s" assignment-id user-id)))
    (org-canvas-api-request 'PUT url
      :data `((submission . ((posted_grade . ,score)))))))

(defun org-canvas--submissions-push-bulk-grades (assignment-id diffs)
  "Push grade DIFFS for ASSIGNMENT-ID via the bulk update_grades endpoint.
DIFFS is a list of plists with :user-id and :new-score."
  (let* ((grade-data
          (mapcar (lambda (ch)
                    (cons (number-to-string (plist-get ch :user-id))
                          `((posted_grade . ,(plist-get ch :new-score)))))
                  diffs))
         (url (org-canvas-api-course-endpoint
               "assignments/%s/submissions/update_grades" assignment-id)))
    (org-canvas-api-request 'POST url
      :data `((grade_data . ,grade-data)))))

(cl-defun org-canvas-submissions-push-grades ()
  "Push all modified grades from the current buffer to Canvas.
Compare current scores against the snapshot taken at render time."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (let ((changes (org-canvas--submissions-collect-grade-changes))
        (assignment-id org-canvas-submissions--assignment-id))
    (unless changes
      (message "No grade changes to push")
      (cl-return-from org-canvas-submissions-push-grades))
    (message "Grade changes:\n%s"
             (mapconcat (lambda (ch)
                          (format "  %s: %s → %s"
                                  (plist-get ch :name)
                                  (or (plist-get ch :old-score) "nil")
                                  (or (plist-get ch :new-score) "nil")))
                        changes "\n"))
    (when (y-or-n-p (format "Push %d grade change(s)? " (length changes)))
      (condition-case err
          (progn
            (if (= (length changes) 1)
                (let ((ch (car changes)))
                  (org-canvas--submissions-push-single-grade
                   assignment-id
                   (plist-get ch :user-id)
                   (plist-get ch :new-score)))
              (org-canvas--submissions-push-bulk-grades assignment-id changes))
            ;; Update snapshot so re-pressing S won't re-push
            (dolist (ch changes)
              (setf (alist-get (plist-get ch :user-id)
                               org-canvas-submissions--original-scores)
                    (plist-get ch :new-score)))
            (message "Pushed %d grade(s) successfully" (length changes)))
        (error (message "Error pushing grades: %s" (error-message-string err)))))))

(provide 'org-canvas-submissions)
;;; org-canvas-submissions.el ends here
