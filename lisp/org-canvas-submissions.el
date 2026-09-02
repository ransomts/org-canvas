;;; org-canvas-submissions.el --- View student submissions from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pull, read, grade, and comment on student submissions without leaving
;; Emacs.  A pulled assignment becomes a GRADING FILE, saved under
;; `org-canvas-submissions-directory' as <assignment>.org: one heading
;; per student with the submitted text, attachments, comments, rubric,
;; and a property drawer that carries the grade.  Grading can therefore
;; span sessions: close Emacs, reopen the file later, push then.
;;
;; WORKFLOW
;; ========
;; 1. `org-canvas-pull-submissions'  select an assignment; the grading
;;    file is written and visited (or the summary table shown, see
;;    `org-canvas-submissions-default-view').
;; 2. Read.  `d' downloads the attachments of the student at point, `D'
;;    everyone's, into files/<assignment>/<student>/ beside the file.
;; 3. Grade.  Edit :SCORE: on each heading; `c' posts a comment.
;; 4. `S' pushes every SCORE that differs from its CANVAS_SCORE, the
;;    score as last pulled or pushed.  Before pushing from a saved file
;;    Canvas is re-read: a student whose grade changed there since the
;;    pull, or who resubmitted, is skipped and marked :CONFLICT: rather
;;    than overwritten (`org-canvas-submissions-check-conflicts').
;;    Pushed scores become the new CANVAS_SCORE and the file is saved.
;; 5. `org-canvas-open-submissions' reopens a saved grading file.
;;
;; VIEWS
;; =====
;; Detail:  the grading file, per-student headings (editable)
;; Summary: a read-only table, derived from the headings so it shows
;;          unpushed edits; `v' switches between the two.
;;
;; KEYS (org-canvas-submissions-mode)
;; ===============================
;; g  refresh from Canvas (asks first if edits are unpushed)
;; v  summary <-> detail
;; d  download attachments for the student at point
;; D  download attachments for every student
;; c  post a comment on the student at point
;; S  push grade changes
;;
;; PRIVACY
;; =======
;; Grading files are student work.  The directory gets a .gitignore
;; when created (`org-canvas-submissions-write-gitignore') so they never
;; travel with a course repository.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-submissions-directory nil
  "Directory for grading files, or nil for submissions/ under the course.
Each pulled assignment is saved here as <assignment>.org and its
downloaded attachments under files/<assignment>/<student>/.  Nil means
`org-canvas-directory'/submissions/, resolved when used rather than
when the package loads, so a course activated later still gets its own
tree.  Everything in it is student work; see
`org-canvas-submissions-write-gitignore'."
  :type '(choice (const :tag "submissions/ under org-canvas-directory" nil)
                 directory)
  :group 'org-canvas)

(defcustom org-canvas-submissions-default-view 'detail
  "View shown after a pull.
`detail' visits the saved grading file, one heading per student;
`summary' shows the read-only overview table instead.  Grades are
edited in the detail file either way."
  :type '(choice (const :tag "Detail headings (grading file)" detail)
                 (const :tag "Summary table" summary))
  :group 'org-canvas)

(defcustom org-canvas-submissions-check-conflicts t
  "Non-nil means re-read Canvas before pushing grades from a saved file.
A heading whose CANVAS_SCORE no longer matches what Canvas holds, or
whose student resubmitted since the pull, is skipped and marked with a
CONFLICT property instead of being overwritten."
  :type 'boolean
  :group 'org-canvas)

(defcustom org-canvas-submissions-include-rubric-criteria t
  "Non-nil means a grading file lists the assignment's rubric criteria.
The table (criterion, points, ratings) follows the Rubric: line, so the
work can be read against the rubric offline.  Nil keeps just the line."
  :type 'boolean
  :group 'org-canvas)

(defcustom org-canvas-submissions-write-gitignore t
  "Non-nil means write a .gitignore into the submissions directory.
It is written once, when the directory is created, and excludes
everything in it: grading files hold student work and must not travel
with a course repository."
  :type 'boolean
  :group 'org-canvas)

;;;; Buffer-Local State

(defvar-local org-canvas-submissions--assignment-name nil
  "Name of the assignment shown in this buffer.")

(defvar-local org-canvas-submissions--assignment-id nil
  "Canvas ID of the assignment shown in this buffer.")

(defvar-local org-canvas-submissions--data nil
  "Cached submission data for toggle/refresh.")

(defvar-local org-canvas-submissions--current-view nil
  "Current view: `summary' or `detail'.")

(defvar-local org-canvas-submissions--original-scores nil
  "Alist of (user-id . score-string) captured at render time.
The in-memory baseline for ephemeral buffers; a grading file carries
its baseline in each heading's CANVAS_SCORE property instead.")

(defvar-local org-canvas-submissions--source-file nil
  "Grading file a read-only summary buffer was derived from.")

;;;; Minor Mode

(defvar org-canvas-submissions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-canvas-submissions-refresh)
    (define-key map (kbd "v") #'org-canvas-submissions-toggle-view)
    (define-key map (kbd "d") #'org-canvas-submissions-download-attachments)
    (define-key map (kbd "c") #'org-canvas-submissions-add-comment)
    (define-key map (kbd "S") #'org-canvas-submissions-push-grades)
    (define-key map (kbd "D") #'org-canvas-submissions-download-all-attachments)
    map)
  "Keymap for `org-canvas-submissions-mode'.")

(define-minor-mode org-canvas-submissions-mode
  "Minor mode for viewing Canvas submissions.
\\{org-canvas-submissions-mode-map}"
  :lighter " Submissions"
  :keymap org-canvas-submissions-mode-map)

;;;; Grading Files

(defun org-canvas--submissions-dir ()
  "Return the submissions directory as an absolute path.
`org-canvas-submissions-directory' when set, else submissions/ under
`org-canvas-directory', resolved now (see the defcustom)."
  (expand-file-name (or org-canvas-submissions-directory
                        (org-canvas--path "submissions/"))))

(defun org-canvas--submissions-file-path (assignment-name)
  "Return the grading file path for ASSIGNMENT-NAME."
  (expand-file-name
   (format "%s.org" (org-canvas--submissions-sanitize-filename assignment-name))
   (org-canvas--submissions-dir)))

(defun org-canvas--submissions-ensure-directory ()
  "Create the submissions directory and return it.
Write a .gitignore there once, when
`org-canvas-submissions-write-gitignore' is non-nil, so the student
work the directory holds never travels with a course repository."
  (let ((dir (org-canvas--submissions-dir)))
    (make-directory dir t)
    (when org-canvas-submissions-write-gitignore
      (let ((gitignore (expand-file-name ".gitignore" dir)))
        (unless (file-exists-p gitignore)
          (with-temp-file gitignore
            (insert "# Student work pulled by org-canvas.  Never commit it.\n"
                    "*\n!.gitignore\n")))))
    dir))

(defun org-canvas--submissions-file-property (name)
  "Return the value of the #+PROPERTY: NAME keyword in this buffer, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^#\\+PROPERTY: %s[ \t]+\\(.+\\)$" (regexp-quote name))
             nil t)
        (string-trim (match-string 1))))))

(defun org-canvas--submissions-ensure-context ()
  "Recover the assignment id, name, and view of a reopened grading file.
A buffer created by a pull already carries them; a file visited later
gets them back from its #+PROPERTY: header and its headings."
  (unless org-canvas-submissions--assignment-id
    (setq org-canvas-submissions--assignment-id
          (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID")))
  (unless org-canvas-submissions--assignment-name
    (setq org-canvas-submissions--assignment-name
          (or (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_NAME")
              (and buffer-file-name (file-name-base buffer-file-name)))))
  (unless org-canvas-submissions--current-view
    (setq org-canvas-submissions--current-view
          (if (save-excursion
                (goto-char (point-min))
                (re-search-forward "^[ \t]*:USER_ID:" nil t))
              'detail
            'summary))))

(defun org-canvas--submissions-goto-user (user-id)
  "Move point to the heading whose USER_ID property is USER-ID.
Return non-nil when found."
  (goto-char (point-min))
  (when (re-search-forward
         (format "^[ \t]*:USER_ID:[ \t]+%s[ \t]*$" user-id) nil t)
    (org-back-to-heading t)
    t))

(defun org-canvas--submissions-guard-unpushed (verb)
  "Ask before VERB (a capitalized verb) discards unpushed score edits."
  (let ((pending (length (org-canvas--submissions-collect-grade-changes))))
    (when (and (> pending 0)
               (not (y-or-n-p
                     (format "%d unpushed score change(s) will be lost; %s anyway? "
                             pending verb))))
      (user-error "%s cancelled" verb))))

(defun org-canvas--submissions-entered-score (submission)
  "Return the grader-entered score of SUBMISSION, or nil.
Canvas reports `entered_score' (what the grader typed) and `score'
\(after any late-policy deduction).  Grading works with what was typed."
  (let ((entered (alist-get 'entered_score submission))
        (score (alist-get 'score submission)))
    (cond ((numberp entered) entered)
          ((numberp score) score))))

;;;; Links

(defun org-canvas--submissions-course-url (path)
  "Return the web URL of PATH under the course."
  (format "%s/courses/%s/%s"
          (replace-regexp-in-string "/+\\'" "" org-canvas-base-url)
          org-canvas-course-id path))

(defun org-canvas--submissions-assignment-url (assignment-id)
  "Return the Canvas page URL of ASSIGNMENT-ID."
  (org-canvas--submissions-course-url (format "assignments/%s" assignment-id)))

(defun org-canvas--submissions-speedgrader-url (assignment-id &optional user-id)
  "Return the SpeedGrader URL for ASSIGNMENT-ID, at USER-ID's work when given."
  (concat (org-canvas--submissions-course-url
           (format "gradebook/speed_grader?assignment_id=%s" assignment-id))
          (if user-id (format "&student_id=%s" user-id) "")))

(defun org-canvas--submissions-feature-file (endpoint)
  "Return the Org file registered for the feature whose endpoint is ENDPOINT.
Read through the feature registry so this module depends on no feature
module.  Nil when the feature is not loaded."
  (let* ((feature (cl-find endpoint org-canvas--feature-registry
                           :key (lambda (f) (plist-get f :endpoint))
                           :test #'equal))
         (var (and feature (plist-get feature :file-var))))
    (and var (boundp var) (symbol-value var))))

(defun org-canvas--submissions-assignments-file ()
  "Return the assignments file registered with org-canvas, or nil."
  (org-canvas--submissions-feature-file "assignments"))

(defun org-canvas--submissions-rubrics-file ()
  "Return the rubrics file registered with org-canvas, or nil."
  (org-canvas--submissions-feature-file "rubrics"))

(defun org-canvas--submissions-heading-for-id (file id)
  "Return the title of the heading in FILE whose CANVAS_ID is ID, or nil.
Nil as well when FILE is nil or missing."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (catch 'found
          (org-map-entries
           (lambda ()
             (when (equal (org-entry-get (point) "CANVAS_ID") id)
               (throw 'found (org-get-heading t t t t))))
           nil 'file)
          nil)))))

(defun org-canvas--submissions-heading-for-assignment (assignment-id)
  "Return the title of the assignments heading whose CANVAS_ID is ASSIGNMENT-ID.
Nil when there is none, as for a classic quiz's shadow assignment or
one authored in the web UI."
  (org-canvas--submissions-heading-for-id
   (org-canvas--submissions-assignments-file) assignment-id))

(defun org-canvas--submissions-heading-for-rubric (rubric-id)
  "Return the title of the rubrics heading whose CANVAS_ID is RUBRIC-ID, or nil."
  (org-canvas--submissions-heading-for-id
   (org-canvas--submissions-rubrics-file) rubric-id))

(defun org-canvas--submissions-links-line (assignment-id)
  "Return the Assignment: line for ASSIGNMENT-ID, without its newline.
It links the assignments heading that carries the id (omitted when
none does), the Canvas assignment page, and SpeedGrader."
  (let* ((title (org-canvas--submissions-heading-for-assignment assignment-id))
         (file (and title (org-canvas--submissions-assignments-file)))
         (org-link (and file
                        (format "[[file:%s::*%s][in Org]], "
                                (file-relative-name file (org-canvas--submissions-dir))
                                title))))
    (format "Assignment: %s[[%s][on Canvas]], [[%s][SpeedGrader]]"
            (or org-link "")
            (org-canvas--submissions-assignment-url assignment-id)
            (org-canvas--submissions-speedgrader-url assignment-id))))

(defun org-canvas--submissions-render-links (assignment-id)
  "Insert the Assignment: links line for ASSIGNMENT-ID."
  (insert (org-canvas--submissions-links-line assignment-id) "\n"))

(defun org-canvas--submissions-rubric-url (rubric-id)
  "Return the Canvas page URL of RUBRIC-ID."
  (org-canvas--submissions-course-url (format "rubrics/%s" rubric-id)))

(defun org-canvas--submissions-rubric-settings (assignment)
  "Return (ID . TITLE) of ASSIGNMENT's attached rubric, or nil."
  (let ((settings (alist-get 'rubric_settings assignment)))
    (when (and settings (alist-get 'id settings))
      (cons (format "%s" (alist-get 'id settings))
            (or (alist-get 'title settings) "")))))

(defun org-canvas--submissions-rubric-line (rubric-id)
  "Return the Rubric: line for RUBRIC-ID, without its newline.
It links the rubrics heading that carries the id (omitted when none
does) and the rubric's Canvas page."
  (let* ((title (org-canvas--submissions-heading-for-rubric rubric-id))
         (file (and title (org-canvas--submissions-rubrics-file)))
         (org-link (and file
                        (format "[[file:%s::*%s][in Org]], "
                                (file-relative-name file (org-canvas--submissions-dir))
                                title))))
    (format "Rubric: %s[[%s][on Canvas]]"
            (or org-link "")
            (org-canvas--submissions-rubric-url rubric-id))))

(defun org-canvas--submissions-render-rubric-properties (assignment)
  "Insert the CANVAS_RUBRIC_ID and CANVAS_RUBRIC_TITLE keywords for ASSIGNMENT.
Nothing is inserted when no rubric is attached.  A reopened file uses
them to rebuild its Rubric: line without a request."
  (let ((rubric (org-canvas--submissions-rubric-settings assignment)))
    (when rubric
      (insert (format "#+PROPERTY: CANVAS_RUBRIC_ID %s\n" (car rubric)))
      (insert (format "#+PROPERTY: CANVAS_RUBRIC_TITLE %s\n" (cdr rubric))))))

(defun org-canvas--submissions-table-cell (text)
  "Return TEXT safe for an Org table cell.
Pipes and newlines, with the whitespace around them, become one space."
  (string-trim (replace-regexp-in-string "[ \t]*[|\n]+[ \t]*" " " (or text ""))))

(defun org-canvas--submissions-render-criteria (criteria)
  "Insert a table of rubric CRITERIA: criterion, points, and ratings."
  (when (and criteria (> (length criteria) 0))
    (insert "\n| Criterion | Points | Ratings |\n|---+---+---|\n")
    (dolist (c (append criteria nil))
      (insert (format "| %s | %s | %s |\n"
                      (org-canvas--submissions-table-cell (alist-get 'description c))
                      (org-canvas--submissions-format-number (or (alist-get 'points c) 0))
                      (mapconcat
                       (lambda (r)
                         (format "%s (%s)"
                                 (org-canvas--submissions-table-cell (alist-get 'description r))
                                 (org-canvas--submissions-format-number (or (alist-get 'points r) 0))))
                       (append (alist-get 'ratings c) nil) ", "))))
    (org-table-align)))

(defun org-canvas--submissions-render-rubric-header (assignment)
  "Insert ASSIGNMENT's Rubric: line and, when enabled, its criteria table.
Nothing is inserted when no rubric is attached; the table follows
`org-canvas-submissions-include-rubric-criteria'."
  (let ((rubric (org-canvas--submissions-rubric-settings assignment)))
    (when rubric
      (insert (org-canvas--submissions-rubric-line (car rubric)) "\n")
      (when org-canvas-submissions-include-rubric-criteria
        (org-canvas--submissions-render-criteria (alist-get 'rubric assignment))))))

(defun org-canvas--submissions-replace-header-line (regexp fresh)
  "Replace the header line matching REGEXP with FRESH.
Only the region before the first heading is searched.  Return non-nil
when the line changed."
  (save-excursion
    (goto-char (point-min))
    (let ((limit (save-excursion (or (re-search-forward "^\\* " nil t) (point-max)))))
      (when (re-search-forward regexp limit t)
        (unless (equal (match-string 0) fresh)
          (let ((inhibit-read-only t))
            (replace-match fresh t t))
          t)))))

(defun org-canvas--submissions-refresh-links ()
  "Rebuild this grading file's Assignment: and Rubric: lines from current sources.
Headings may have been renamed since the pull; both lines are recomputed
by CANVAS_ID with no Canvas request.  Return non-nil when either changed."
  (org-canvas--submissions-ensure-context)
  (when org-canvas-submissions--assignment-id
    (let ((rubric-id (org-canvas--submissions-file-property "CANVAS_RUBRIC_ID"))
          (changed nil))
      (when (org-canvas--submissions-replace-header-line
             "^Assignment: .*$"
             (org-canvas--submissions-links-line org-canvas-submissions--assignment-id))
        (setq changed t))
      (when (and rubric-id
                 (org-canvas--submissions-replace-header-line
                  "^Rubric: .*$" (org-canvas--submissions-rubric-line rubric-id)))
        (setq changed t))
      changed)))

;;;; Attachments on Disk

(defun org-canvas--submissions-attachment-dir (assignment-name student-name)
  "Return the download directory for STUDENT-NAME's work on ASSIGNMENT-NAME."
  (expand-file-name
   (format "files/%s/%s/"
           (org-canvas--submissions-sanitize-filename assignment-name)
           (org-canvas--submissions-sanitize-filename student-name))
   (org-canvas--submissions-dir)))

(defun org-canvas--submissions-local-attachment (assignment-name student-name filename)
  "Return FILENAME's link path, relative to the grading file, if downloaded.
Nil otherwise.  ASSIGNMENT-NAME and STUDENT-NAME locate the download
directory."
  (when (and assignment-name student-name filename)
    (let ((path (expand-file-name
                 filename (org-canvas--submissions-attachment-dir assignment-name student-name))))
      (when (file-exists-p path)
        (file-relative-name path (org-canvas--submissions-dir))))))

(defun org-canvas--submissions-attachment-line (name url local)
  "Return the Attachments list line for NAME at URL.
With LOCAL, the downloaded copy's link path, the local file comes first
and the Canvas link stays beside it."
  (if local
      (format "- [[file:%s][%s]] ([[%s][Canvas]])\n" local name url)
    (format "- [[%s][%s]]\n" url name)))

(defconst org-canvas--submissions-attachment-line-re
  (concat "^- \\(?:"
          "\\[\\[file:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\] (\\[\\[\\(https?://[^]]+\\)\\]\\[Canvas\\]\\])"
          "\\|"
          "\\[\\[\\(https?://[^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
          "\\)[ \t]*$")
  "An Attachments entry, in either form.
Local: groups 1 path, 2 name, 3 url.  Remote: groups 4 url, 5 name.")

(defun org-canvas--submissions-attachments-region ()
  "Return (START . END) of the Attachments list under the heading at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point))))
      (when (re-search-forward "^\\*\\* Attachments$" end t)
        (forward-line 1)
        (cons (point)
              (save-excursion
                (if (re-search-forward "^\\*\\*" end t) (line-beginning-position) end)))))))

(defun org-canvas--submissions-attachment-entries ()
  "Return the Attachments entries of the heading at point as plists.
Each has :name, :url, and :local (the downloaded copy's link path or nil)."
  (let ((region (org-canvas--submissions-attachments-region))
        (entries nil))
    (when region
      (save-excursion
        (goto-char (car region))
        (while (re-search-forward org-canvas--submissions-attachment-line-re (cdr region) t)
          (push (if (match-beginning 4)
                    (list :name (match-string 5) :url (match-string 4) :local nil)
                  (list :name (match-string 2) :url (match-string 3) :local (match-string 1)))
                entries))))
    (nreverse entries)))

(defun org-canvas--submissions-rewrite-attachments (entries)
  "Replace the Attachments list of the heading at point with ENTRIES."
  (let ((region (org-canvas--submissions-attachments-region)))
    (when region
      (save-excursion
        (goto-char (car region))
        (delete-region (car region) (cdr region))
        (dolist (e entries)
          (insert (org-canvas--submissions-attachment-line
                   (plist-get e :name) (plist-get e :url) (plist-get e :local))))
        (when (looking-at "^\\*")
          (insert "\n"))))))

;;;; Data Fetching

(defun org-canvas--submissions-fetch-assignments ()
  "Fetch assignment list for interactive selection.
Returns a list of alists with `id' and `name' keys."
  (org-canvas-api-request-all-pages
   'GET (org-canvas-api-course-endpoint "assignments")
   '(("order_by" . "name"))))

(defun org-canvas--submissions-fetch-assignment (assignment-id)
  "Fetch ASSIGNMENT-ID's assignment object, or nil when the request fails.
The object carries `rubric_settings' and `rubric' for the grading file
header; a refresh needs it because only the pull had the object."
  (condition-case nil
      (org-canvas-api-request
       'GET (org-canvas-api-course-endpoint "assignments/%s" assignment-id))
    (error nil)))

(defun org-canvas--submissions-fetch-for-assignment (assignment-id)
  "Fetch all submissions for ASSIGNMENT-ID with comments, rubric, and user info.
Returns a list of submission alists.  Each include travels as its own
include[] key: Canvas ignores a comma-joined list and returns the bare
submission, which is how every student rendered as Unknown (#112)."
  (org-canvas-api-request-all-pages
   'GET (org-canvas-api-course-endpoint "assignments/%s/submissions" assignment-id)
   '(("include[]" . "submission_comments")
     ("include[]" . "rubric_assessment")
     ("include[]" . "user"))))

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
    (org-canvas--submissions-render-rubric-header assignment)
    (insert "\n")
    (dolist (sub sorted)
      (org-canvas--submissions-render-detail-entry sub assignment-name assignment-id))))

(defun org-canvas--submissions-render-detail-entry (submission &optional assignment-name assignment-id)
  "Render a single SUBMISSION as an Org heading with properties.
SCORE is the editable grade; CANVAS_SCORE is the same value as pulled,
the baseline a later push compares against.  FINAL_SCORE appears only
when Canvas's late policy made the recorded score differ from the one
entered.  ATTEMPT lets a push notice a resubmission.  With
ASSIGNMENT-ID the heading gets its SpeedGrader link, and with
ASSIGNMENT-NAME attachments already downloaded link to the local copy."
  (let ((name (org-canvas--submissions-user-sortable-name submission))
        (user-id (org-canvas--submissions-user-id submission))
        (sub-id (alist-get 'id submission))
        (status (org-canvas--submissions-normalize-status submission))
        (entered (org-canvas--submissions-entered-score submission))
        (final (alist-get 'score submission))
        (attempt (alist-get 'attempt submission))
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
    (when entered
      (let ((shown (org-canvas--submissions-format-number entered)))
        (insert (format ":SCORE: %s\n" shown))
        (insert (format ":CANVAS_SCORE: %s\n" shown))))
    (when (and entered (numberp final) (/= final entered))
      (insert (format ":FINAL_SCORE: %s\n"
                      (org-canvas--submissions-format-number final))))
    (when (numberp attempt)
      (insert (format ":ATTEMPT: %s\n" attempt)))
    (when (not (string-empty-p submitted-at))
      (insert (format ":SUBMITTED_AT: %s\n" submitted-at)))
    (insert ":END:\n")
    (when (and user-id assignment-id)
      (insert (format "[[%s][Open in SpeedGrader]]\n"
                      (org-canvas--submissions-speedgrader-url assignment-id user-id))))
    (when (and body (stringp body) (not (string-empty-p body)))
      (insert "\n" (org-canvas--html-to-org body) "\n"))
    (org-canvas--submissions-render-attachments attachments assignment-name name)
    (org-canvas--submissions-render-comments comments)
    (org-canvas--submissions-render-rubric rubric)
    (insert "\n")))

(defun org-canvas--submissions-render-attachments (attachments &optional assignment-name student-name)
  "Render ATTACHMENTS as a sub-heading with links.
With ASSIGNMENT-NAME and STUDENT-NAME, an attachment already downloaded
links to the local copy first, with the Canvas link beside it."
  (when (and attachments (> (length attachments) 0))
    (insert "\n** Attachments\n")
    (dolist (att (append attachments nil))
      (let ((url (alist-get 'url att))
            (filename (alist-get 'display_name att)))
        (when (and url filename)
          (insert (org-canvas--submissions-attachment-line
                   filename url
                   (org-canvas--submissions-local-attachment
                    assignment-name student-name filename))))))))

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
                          (org-canvas--html-to-org-inline (or text "")))))))))

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
  "Return (name status submitted-at score) for each heading of a grading file."
  (org-map-entries
   (lambda ()
     (list (org-get-heading t t t t)
           (or (org-entry-get (point) "STATUS") "")
           (or (org-entry-get (point) "SUBMITTED_AT") "")
           (or (org-entry-get (point) "SCORE") "")))
   "LEVEL=1"))

(defun org-canvas--submissions-show-summary-of-file ()
  "Show a read-only summary table of the current grading file."
  (let* ((name org-canvas-submissions--assignment-name)
         (id org-canvas-submissions--assignment-id)
         (file buffer-file-name)
         (rows (org-canvas--submissions-heading-rows))
         (buf (get-buffer-create (format "*submissions summary: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: Submissions: %s\n" name))
        (insert (format "#+PROPERTY: CANVAS_ASSIGNMENT_ID %s\n\n" id))
        (insert "Read-only overview of the grading file; edit scores there (press v).\n\n")
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

;;;; Comment Writing

;;;###autoload
(defun org-canvas-submissions-add-comment ()
  "Post a text comment on the submission at point.
Must be on or inside a student heading in detail view."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
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

(defun org-canvas--submissions-append-comment-to-buffer (_user-name text)
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

;;;###autoload
(defun org-canvas-submissions-download-attachments ()
  "Download the attachments of the submission at point.
Files land in files/<assignment>/<student>/ under the submissions
directory, beside the grading file, and each Attachments entry is then
rewritten to link the local copy with the Canvas link kept beside it.
Entries already downloaded are skipped."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((user-name (org-get-heading t t t t))
           (assignment-name org-canvas-submissions--assignment-name)
           (dir (org-canvas--submissions-attachment-dir assignment-name user-name))
           (entries (org-canvas--submissions-attachment-entries))
           (fetched 0))
      (unless entries
        (user-error "No attachments found for %s" user-name))
      (make-directory dir t)
      (dolist (e entries)
        (unless (plist-get e :local)
          (message "Downloading %s..." (plist-get e :name))
          (org-canvas--submissions-download-file (plist-get e :url) dir (plist-get e :name))
          (cl-incf fetched)
          (plist-put e :local (org-canvas--submissions-local-attachment
                               assignment-name user-name (plist-get e :name)))))
      (when (> fetched 0)
        (let ((inhibit-read-only t))
          (org-canvas--submissions-rewrite-attachments entries))
        (when buffer-file-name
          (save-buffer)))
      (message "Downloaded %d file(s) to %s" fetched dir))))

;;;###autoload
(defun org-canvas-submissions-download-all-attachments ()
  "Download every student's attachments in this grading file.
Students without attachments are skipped; the count is reported."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (let ((students 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (when (condition-case nil
                  (progn (org-canvas-submissions-download-attachments) t)
                (user-error nil))
          (cl-incf students))
        (org-end-of-subtree t)))
    (message "Downloaded attachments for %d student(s)" students)))

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
  "Select an assignment and pull its submissions.
The detail view is saved as a grading file under
`org-canvas-submissions-directory' and visited; the summary view is
an ephemeral table.  Which one opens first is
`org-canvas-submissions-default-view'."
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
     org-canvas-submissions-default-view selected)))

;;;###autoload
(defun org-canvas-open-submissions ()
  "Visit a saved grading file and turn on `org-canvas-submissions-mode'.
Grading files are the detail views `org-canvas-pull-submissions' saves
under `org-canvas-submissions-directory'."
  (interactive)
  (let* ((dir (org-canvas--submissions-dir))
         (files (and (file-directory-p dir)
                     (directory-files dir nil "\\.org\\'"))))
    (unless files
      (user-error "No grading files in %s; pull an assignment first" dir))
    (find-file (expand-file-name (completing-read "Grading file: " files nil t) dir))
    (org-canvas-submissions-mode 1)
    (org-canvas--submissions-ensure-context)
    (when (org-canvas--submissions-refresh-links)
      (save-buffer))))

;;;###autoload
(defun org-canvas-submissions-refresh ()
  "Re-fetch and re-render the submissions for the current buffer.
Ask first when the buffer holds score edits that were never pushed."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (let ((id org-canvas-submissions--assignment-id)
        (name org-canvas-submissions--assignment-name)
        (view org-canvas-submissions--current-view))
    (unless id
      (user-error "No assignment ID in this buffer"))
    (org-canvas--submissions-guard-unpushed "Refresh")
    (message "Refreshing submissions for %s..." name)
    (let ((submissions (org-canvas--submissions-fetch-for-assignment id)))
      (org-canvas--submissions-display
       name id submissions view (org-canvas--submissions-fetch-assignment id)))))

(defun org-canvas--submissions-display (assignment-name assignment-id submissions view &optional assignment)
  "Show SUBMISSIONS for ASSIGNMENT-NAME (ASSIGNMENT-ID) in VIEW.
The detail view is the grading file under the submissions directory,
rendered and saved; the summary view is an ephemeral buffer.
ASSIGNMENT, the Canvas assignment object when at hand, supplies the
rubric header of the detail view."
  (let ((buf (if (eq view 'detail)
                 (org-canvas--submissions-grading-buffer assignment-name)
               (get-buffer-create (format "*submissions: %s*" assignment-name)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (if (eq view 'summary)
            (org-canvas--submissions-render-summary
             assignment-name assignment-id submissions)
          (org-canvas--submissions-render-detail
           assignment-name assignment-id submissions assignment))
        (goto-char (point-min))
        (setq-local org-canvas-submissions--assignment-name assignment-name)
        (setq-local org-canvas-submissions--assignment-id assignment-id)
        (setq-local org-canvas-submissions--data submissions)
        (setq-local org-canvas-submissions--current-view view)
        (setq-local org-canvas-submissions--original-scores
                    (org-canvas--submissions-snapshot-scores submissions))
        (org-canvas-submissions-mode 1)
        (when buffer-file-name
          (save-buffer))))
    (switch-to-buffer buf)))

(defun org-canvas--submissions-grading-buffer (assignment-name)
  "Return the buffer visiting ASSIGNMENT-NAME's grading file.
Create the submissions directory as needed, and ask before an existing
file's unpushed score edits are overwritten."
  (org-canvas--submissions-ensure-directory)
  (let ((buf (find-file-noselect (org-canvas--submissions-file-path assignment-name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (org-canvas--submissions-ensure-context)
      (org-canvas--submissions-guard-unpushed "Re-pull"))
    buf))


;;;; Grade Writing

(defun org-canvas--submissions-snapshot-scores (submissions)
  "Build an alist of (user-id . score-string) from SUBMISSIONS.
The score is the entered one, formatted as the buffer shows it."
  (mapcar (lambda (sub)
            (let ((score (org-canvas--submissions-entered-score sub)))
              (cons (org-canvas--submissions-user-id sub)
                    (when score (org-canvas--submissions-format-number score)))))
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
  "Return grade diffs from the detail view.
Each heading's SCORE is compared with its CANVAS_SCORE property, the
score as last pulled or pushed; a heading without that property falls
back to the in-memory snapshot taken at render time."
  (let ((changes nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let ((change (org-canvas--submissions-detail-change-at-point)))
          (when change
            (push change changes)))
        (forward-line 1)))
    (nreverse changes)))

(defun org-canvas--submissions-detail-change-at-point ()
  "Return the grade change plist for the heading at point, or nil."
  (let* ((user-id-str (org-entry-get (point) "USER_ID"))
         (user-id (when user-id-str (string-to-number user-id-str)))
         (baseline (org-entry-get (point) "CANVAS_SCORE"))
         (old-score (if baseline
                        (org-canvas--submissions-parse-score baseline)
                      (alist-get user-id org-canvas-submissions--original-scores)))
         (new-score (org-canvas--submissions-parse-score
                     (org-entry-get (point) "SCORE")))
         (attempt (org-entry-get (point) "ATTEMPT")))
    (when (and user-id (not (equal new-score old-score)))
      (list :user-id user-id
            :name (org-get-heading t t t t)
            :old-score old-score
            :new-score new-score
            :attempt (and attempt (string-to-number attempt))))))

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

(defun org-canvas--submissions-live-baselines (assignment-id)
  "Fetch ASSIGNMENT-ID's submissions as (user-id . (score . attempt)).
The score is the entered score as a string, or nil when ungraded."
  (mapcar (lambda (sub)
            (let ((score (org-canvas--submissions-entered-score sub)))
              (cons (org-canvas--submissions-user-id sub)
                    (cons (and score (org-canvas--submissions-format-number score))
                          (alist-get 'attempt sub)))))
          (org-canvas--submissions-fetch-for-assignment assignment-id)))

(defun org-canvas--submissions-conflict-p (change live)
  "Return why CHANGE conflicts with LIVE Canvas state, or nil.
LIVE is (score . attempt) for the same student, or nil if gone."
  (let ((live-score (car live))
        (live-attempt (cdr live))
        (attempt (plist-get change :attempt)))
    (cond ((not (equal live-score (plist-get change :old-score)))
           (format "Canvas now has %s" (or live-score "no grade")))
          ((and attempt (numberp live-attempt) (/= attempt live-attempt))
           (format "resubmitted (attempt %s)" live-attempt)))))

(defun org-canvas--submissions-partition-conflicts (assignment-id diffs)
  "Split DIFFS into (pushable . conflicting) against live Canvas state.
ASSIGNMENT-ID names the assignment whose submissions are re-read.
Only a buffer visiting a saved grading file is checked, and only when
`org-canvas-submissions-check-conflicts' is non-nil; otherwise every
diff is pushable.  A conflicting diff carries a :conflict reason."
  (if (not (and diffs buffer-file-name org-canvas-submissions-check-conflicts))
      (cons diffs nil)
    (let ((live (org-canvas--submissions-live-baselines assignment-id))
          (ok nil)
          (bad nil))
      (dolist (change diffs)
        (let ((reason (org-canvas--submissions-conflict-p
                       change (alist-get (plist-get change :user-id) live))))
          (if reason
              (push (plist-put (copy-sequence change) :conflict reason) bad)
            (push change ok))))
      (cons (nreverse ok) (nreverse bad)))))

(defun org-canvas--submissions-mark-conflicts (conflicts)
  "Write a CONFLICT property on the heading of each of CONFLICTS."
  (save-excursion
    (dolist (c conflicts)
      (when (org-canvas--submissions-goto-user (plist-get c :user-id))
        (org-entry-put (point) "CONFLICT"
                       (format "%s; pull again, or set CANVAS_SCORE to Canvas's value to override"
                               (plist-get c :conflict)))))))

(defun org-canvas--submissions-describe-changes (diffs)
  "Return DIFFS as one line per student: name, old score, new score."
  (mapconcat (lambda (ch)
               (format "  %s: %s → %s"
                       (plist-get ch :name)
                       (or (plist-get ch :old-score) "nil")
                       (or (plist-get ch :new-score) "nil")))
             diffs "\n"))

(defun org-canvas--submissions-send-grades (assignment-id diffs)
  "Send DIFFS for ASSIGNMENT-ID: one PUT, or the bulk endpoint for several."
  (if (= (length diffs) 1)
      (let ((ch (car diffs)))
        (org-canvas--submissions-push-single-grade
         assignment-id (plist-get ch :user-id) (plist-get ch :new-score)))
    (org-canvas--submissions-push-bulk-grades assignment-id diffs)))

(defun org-canvas--submissions-record-pushed (diffs)
  "Make DIFFS the new baseline: snapshot, CANVAS_SCORE, and the file."
  (dolist (ch diffs)
    (setf (alist-get (plist-get ch :user-id) org-canvas-submissions--original-scores)
          (plist-get ch :new-score)))
  (when (eq org-canvas-submissions--current-view 'detail)
    (save-excursion
      (dolist (ch diffs)
        (when (org-canvas--submissions-goto-user (plist-get ch :user-id))
          (if (plist-get ch :new-score)
              (org-entry-put (point) "CANVAS_SCORE" (plist-get ch :new-score))
            (org-entry-delete (point) "CANVAS_SCORE"))
          (org-entry-delete (point) "CONFLICT"))))
    (org-canvas--submissions-refresh-links))
  (when buffer-file-name
    (save-buffer)))

;;;###autoload
(cl-defun org-canvas-submissions-push-grades ()
  "Push every changed score in the current buffer to Canvas.
In a grading file a change is a SCORE that differs from its
CANVAS_SCORE.  Changes that conflict with what Canvas holds now are
skipped and marked (see `org-canvas-submissions-check-conflicts').
After a successful push the baselines and the file are updated."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (let ((assignment-id org-canvas-submissions--assignment-id))
    (unless assignment-id
      (user-error "No CANVAS_ASSIGNMENT_ID in this buffer"))
    (pcase-let ((`(,changes . ,conflicts)
                 (org-canvas--submissions-partition-conflicts
                  assignment-id (org-canvas--submissions-collect-grade-changes))))
      (org-canvas--submissions-mark-conflicts conflicts)
      (unless changes
        (message "No grade changes to push%s"
                 (if conflicts (format " (%d conflict(s) marked)" (length conflicts)) ""))
        (cl-return-from org-canvas-submissions-push-grades))
      (message "Grade changes:\n%s" (org-canvas--submissions-describe-changes changes))
      (when (y-or-n-p (format "Push %d grade change(s)%s? " (length changes)
                              (if conflicts
                                  (format ", skipping %d conflict(s)" (length conflicts))
                                "")))
        (condition-case err
            (progn
              (org-canvas--submissions-send-grades assignment-id changes)
              (org-canvas--submissions-record-pushed changes)
              (message "Pushed %d grade(s) successfully" (length changes)))
          (error (message "Error pushing grades: %s" (error-message-string err))))))))

(provide 'org-canvas-submissions)
;;; org-canvas-submissions.el ends here
