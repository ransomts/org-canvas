;;; org-canvas-submissions.el --- View student submissions from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pull, read, grade, and comment on student submissions without leaving
;; Emacs.  A pulled assignment becomes a GRADING FILE, saved under
;; `org-canvas-submissions-directory' as <assignment>.org: one heading
;; per student with the submitted text, attachments, comments, a rubric
;; table when the assignment has one, and a property drawer that carries
;; the grade.  Grading can therefore span sessions: close Emacs, reopen
;; the file later, push then.
;;
;; WORKFLOW
;; ========
;; 1. `org-canvas-pull-submissions'  select an assignment; the grading
;;    file is written and visited (or the summary table shown, see
;;    `org-canvas-submissions-default-view').
;; 2. Read.  `d' downloads the attachments of the student at point, `D'
;;    everyone's, into files/<assignment>/<student>/ beside the file.
;; 3. Grade.  Edit :SCORE: on each heading, or fill the Score and
;;    Comment cells of the student's Rubric table; `c' posts a comment.
;; 4. `S' pushes every SCORE that differs from its CANVAS_SCORE, the
;;    score as last pulled or pushed, and every Rubric table whose rows
;;    differ from its CANVAS_RUBRIC, the assessment as last pulled or
;;    pushed.  When the rubric is used for grading the row total sets
;;    the score.  Before pushing from a saved file Canvas is re-read: a
;;    student whose grade or assessment changed there since the pull,
;;    or who resubmitted, is skipped and marked :CONFLICT: rather than
;;    overwritten (`org-canvas-submissions-check-conflicts').  Pushed
;;    scores and assessments become the new baselines and the file is
;;    saved.
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

(defcustom org-canvas-submissions-comment-template
  "# Write your comment for this student below these lines.  S posts every
# drafted comment along with the grade changes; nothing is sent until then.
# Lines starting with # are never sent.  c posts a one-off comment now."
  "Text placed under each student's \"Comment to post\" heading at pull time.
Written as Org comment lines so it is never sent.  Nil omits the heading
and turns drafted comments off."
  :type '(choice (const :tag "No draft heading" nil) string)
  :group 'org-canvas)

(defcustom org-canvas-submissions-notes-template
  "# Your notes for this student.  Never sent to Canvas, and kept across pulls."
  "Text placed under each student's \"Notes\" heading at pull time.
Whatever is written under that heading survives a re-pull.  Nil omits
the heading."
  :type '(choice (const :tag "No notes heading" nil) string)
  :group 'org-canvas)

(defcustom org-canvas-submissions-late-window-days 2
  "Days of lateness the completion rule still gives full credit for.
`org-canvas-submissions-apply-completion-rule' scores a submission later
than this as 0.  Canvas's own late policy, if any, still deducts inside
the window."
  :type 'integer
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
    (define-key map (kbd "P") #'org-canvas-submissions-post-grades)
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

(defun org-canvas--submissions-pending-count ()
  "Return how many typed score edits have not been pushed.
Drafted comments, notes and Rubric rows are not counted: a re-pull
carries them over, and a score the rows derive comes back with them."
  (cl-count-if (lambda (ch)
                 (and (not (plist-get ch :score-derived))
                      (not (equal (plist-get ch :new-score) (plist-get ch :old-score)))))
               (org-canvas--submissions-collect-grade-changes)))

(defun org-canvas--submissions-guard-unpushed (verb)
  "Ask before VERB (a capitalized verb) discards unpushed edits."
  (let ((pending (org-canvas--submissions-pending-count)))
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

(defun org-canvas--submissions-excused-p (submission)
  "Return non-nil when SUBMISSION is excused."
  (let ((v (alist-get 'excused submission)))
    (and v (not (eq v :json-false)))))

(defun org-canvas--submissions-shown-score (submission)
  "Return SUBMISSION's score as the grading file spells it, or nil.
\"EX\" for an excused submission, otherwise the entered score formatted
as a number."
  (cond ((org-canvas--submissions-excused-p submission) "EX")
        ((org-canvas--submissions-entered-score submission)
         (org-canvas--submissions-format-number
          (org-canvas--submissions-entered-score submission)))))

(defun org-canvas--submissions-days-late (submission)
  "Return how many days late SUBMISSION arrived, rounded up, or nil.
Nil when it was on time and also when nothing was submitted: Canvas
reports `seconds_late' for a missing submission as the time since the
due date, which is not lateness of a submission."
  (let ((secs (alist-get 'seconds_late submission)))
    (when (and (alist-get 'submitted_at submission)
               (numberp secs) (> secs 0))
      (ceiling secs 86400))))

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
    (with-current-buffer (org-canvas--find-file-noselect file)
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
  "Insert the ASSIGNMENT keywords a reopened file needs: points, policy, rubric.
POINTS_POSSIBLE feeds the completion rule; POST_POLICY (manual or
automatic, the assignment's effective grade post policy) tells the push
whether to offer posting the grades; CANVAS_RUBRIC_ID and
CANVAS_RUBRIC_TITLE rebuild the Rubric: line without a request, and
CANVAS_RUBRIC_USE_FOR_GRADING tells the push whether a rubric table's
total sets the score.  Nothing rubric-related is inserted when no
rubric is attached."
  (let ((points (alist-get 'points_possible assignment))
        (policy (org-canvas--post-manually-to-policy
                 (alist-get 'post_manually assignment)))
        (rubric (org-canvas--submissions-rubric-settings assignment))
        (grading (alist-get 'use_for_grading (alist-get 'rubric_settings assignment))))
    (when (numberp points)
      (insert (format "#+PROPERTY: POINTS_POSSIBLE %s\n"
                      (org-canvas--submissions-format-number points))))
    (when policy
      (insert (format "#+PROPERTY: POST_POLICY %s\n" policy)))
    (when rubric
      (insert (format "#+PROPERTY: CANVAS_RUBRIC_ID %s\n" (car rubric)))
      (insert (format "#+PROPERTY: CANVAS_RUBRIC_TITLE %s\n" (cdr rubric)))
      (insert (format "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING %s\n"
                      (if (eq grading t) "true" "false"))))))

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
    (let ((criteria (org-canvas--submissions-rubric-criteria assignment)))
      (dolist (sub sorted)
        (org-canvas--submissions-render-detail-entry
         sub assignment-name assignment-id criteria)))))

(defun org-canvas--submissions-render-detail-entry (submission &optional assignment-name assignment-id criteria)
  "Render a single SUBMISSION as an Org heading with properties.
SCORE is the editable grade (a number, or EX for excused); CANVAS_SCORE
is the same value as pulled, the baseline a later push compares against.
CANVAS_RUBRIC is the same baseline for the Rubric table, a digest of
the assessment as pulled, present only when Canvas holds one.
FINAL_SCORE appears only when Canvas's late policy made the recorded
score differ from the one entered.  DAYS_LATE, rounded up, appears on a
late submission.  ATTEMPT lets a push notice a resubmission.  With
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
    (insert ":END:\n")
    (when (and user-id assignment-id)
      (insert (format "[[%s][Open in SpeedGrader]]\n"
                      (org-canvas--submissions-speedgrader-url assignment-id user-id))))
    (when (and body (stringp body) (not (string-empty-p body)))
      (insert "\n" (org-canvas--html-to-org body) "\n"))
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
student's subtree.  Nil when the entry has no such heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point))))
      (when (re-search-forward (concat "^" (regexp-quote heading) "$") end t)
        (forward-line 1)
        (cons (point)
              (save-excursion
                (if (re-search-forward "^\\*" end t) (line-beginning-position) end)))))))

(defun org-canvas--submissions-section-text (heading)
  "Return what is written under HEADING in the entry at point, or nil.
Org comment lines (the templates) and blank lines are dropped."
  (let ((region (org-canvas--submissions-section-region heading)))
    (when region
      (let* ((raw (buffer-substring-no-properties (car region) (cdr region)))
             (kept (seq-remove (lambda (l) (string-match-p "\\`[ \t]*\\(#\\|\\'\\)" l))
                               (split-string raw "\n"))))
        (when kept
          (string-trim (mapconcat #'identity kept "\n")))))))

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

;;;; Carry-over Across Pulls

(defun org-canvas--submissions-collect-carryover ()
  "Return (user-id . (:notes TEXT :draft TEXT :rubric ROWS)) for entries with any.
Read from the current buffer before a re-render replaces it.  ROWS are
the unpushed Rubric rows, see `org-canvas--submissions-rubric-carryover'."
  (let ((carry nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let ((user-id (org-entry-get (point) "USER_ID"))
              (notes (org-canvas--submissions-section-text org-canvas--submissions-notes-heading))
              (draft (org-canvas--submissions-section-text org-canvas--submissions-draft-heading))
              (rubric (org-canvas--submissions-rubric-carryover)))
          (when (and user-id (or notes draft rubric))
            (push (cons (string-to-number user-id)
                        (list :notes notes :draft draft :rubric rubric))
                  carry)))
        (forward-line 1)))
    carry))

(defun org-canvas--submissions-restore-carryover (carry)
  "Write CARRY back under the students it names.
CARRY is the alist `org-canvas--submissions-collect-carryover' produced
before the buffer was re-rendered."
  (save-excursion
    (dolist (entry carry)
      (when (org-canvas--submissions-goto-user (car entry))
        (let ((notes (plist-get (cdr entry) :notes))
              (draft (plist-get (cdr entry) :draft))
              (rubric (plist-get (cdr entry) :rubric)))
          (when notes
            (org-canvas--submissions-set-section org-canvas--submissions-notes-heading
                                                 org-canvas-submissions-notes-template notes))
          (when draft
            (org-canvas--submissions-set-section org-canvas--submissions-draft-heading
                                                 org-canvas-submissions-comment-template draft))
          (when rubric
            (org-canvas--submissions-restore-rubric rubric)))))))

(defun org-canvas--submissions-collect-comment-drafts ()
  "Return (:user-id :name :text) for every heading with a drafted comment."
  (let ((drafts nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let ((text (org-canvas--submissions-comment-draft))
              (user-id (org-entry-get (point) "USER_ID")))
          (when (and text user-id)
            (push (list :user-id (string-to-number user-id)
                        :name (org-get-heading t t t t)
                        :text text)
                  drafts)))
        (forward-line 1)))
    (nreverse drafts)))

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

(defconst org-canvas--submissions-rubric-heading "** Rubric"
  "Heading under which a student's rubric table lives.")

(defun org-canvas--submissions-rubric-criteria (assignment)
  "Return ASSIGNMENT's rubric criteria as a list, or nil without a rubric."
  (append (alist-get 'rubric assignment) nil))

(defun org-canvas--submissions-rubric-for-grading-p ()
  "Return non-nil when the grade of this grading file comes from its rubric.
Reads the CANVAS_RUBRIC_USE_FOR_GRADING keyword the pull wrote from the
assignment's rubric settings; a file without it answers nil."
  (equal (org-canvas--submissions-file-property "CANVAS_RUBRIC_USE_FOR_GRADING")
         "true"))

(defun org-canvas--submissions-assessment (submission)
  "Return SUBMISSION's rubric assessment, or nil when it has none.
Canvas maps each assessed criterion's id to its points, rating and
comments; an unassessed submission carries null or nothing."
  (let ((ra (org-canvas--alist-get-non-null 'rubric_assessment submission)))
    (and (consp ra) ra)))

(defun org-canvas--submissions-assessment-entry (assessment criterion-id)
  "Return (SCORE . COMMENT) from ASSESSMENT for CRITERION-ID.
Both are strings spelled as the Rubric table shows them, or nil when
the criterion is unscored or uncommented."
  (let ((entry (alist-get (intern criterion-id) assessment)))
    (when (consp entry)
      (let ((points (org-canvas--alist-get-non-null 'points entry))
            (comment (org-canvas--alist-get-non-null 'comments entry)))
        (cons (and (numberp points) (org-canvas--submissions-format-number points))
              (and (stringp comment) (not (string-blank-p comment))
                   (org-canvas--submissions-table-cell comment)))))))

(defun org-canvas--submissions-assessment-triples (assessment)
  "Return ASSESSMENT as (ID SCORE COMMENT) triples, one per criterion in it."
  (mapcar (lambda (pair)
            (let* ((id (symbol-name (car pair)))
                   (entry (org-canvas--submissions-assessment-entry assessment id)))
              (list id (car entry) (cdr entry))))
          assessment))

(defun org-canvas--submissions-rubric-digest (triples)
  "Return a short digest of TRIPLES, or nil when none carries a score or a comment.
TRIPLES are (ID SCORE COMMENT) lists.  Order does not matter, so the
digest of a Rubric table as typed and of the assessment as Canvas
returns it agree whenever their content does; it is the CANVAS_RUBRIC
baseline a push compares the table against."
  (let ((kept (sort (seq-filter (lambda (r) (or (nth 1 r) (nth 2 r))) triples)
                    (lambda (a b) (string< (car a) (car b))))))
    (when kept
      (substring (sha1 (mapconcat (lambda (r)
                                    (format "%s=%s|%s" (nth 0 r) (or (nth 1 r) "") (or (nth 2 r) "")))
                                  kept "\n"))
                 0 12))))

(defun org-canvas--submissions-submission-rubric-digest (submission)
  "Return the CANVAS_RUBRIC digest of SUBMISSION's assessment, or nil."
  (org-canvas--submissions-rubric-digest
   (org-canvas--submissions-assessment-triples
    (org-canvas--submissions-assessment submission))))

(defun org-canvas--submissions-rubric-rows-from-canvas (criteria assessment)
  "Return the Rubric table rows for CRITERIA as scored by ASSESSMENT.
Each row is (ID CRITERION MAX SCORE COMMENT), strings or nil.  Without
CRITERIA the rows come from the assessment alone, one per criterion
scored, so an ephemeral buffer rendered without the assignment still
shows what Canvas holds."
  (if criteria
      (mapcar (lambda (c)
                (let* ((id (format "%s" (alist-get 'id c)))
                       (entry (org-canvas--submissions-assessment-entry assessment id))
                       (max (alist-get 'points c)))
                  (list id
                        (org-canvas--submissions-table-cell (alist-get 'description c))
                        (and (numberp max) (org-canvas--submissions-format-number max))
                        (car entry) (cdr entry))))
              criteria)
    (mapcar (lambda (triple) (list (nth 0 triple) nil nil (nth 1 triple) (nth 2 triple)))
            (org-canvas--submissions-assessment-triples assessment))))

(defun org-canvas--submissions-render-rubric (criteria assessment)
  "Insert the Rubric heading and table for CRITERIA, scored by ASSESSMENT.
One row per criterion: its Canvas id, description and points possible,
then the Score and Comment cells the grader fills; an assessment Canvas
already holds pre-fills them.  Nothing is inserted without a rubric."
  (let ((rows (org-canvas--submissions-rubric-rows-from-canvas criteria assessment)))
    (when rows
      (insert "\n" org-canvas--submissions-rubric-heading "\n")
      (insert "| Id | Criterion | Max | Score | Comment |\n|---+---+---+---+---|\n")
      (dolist (row rows)
        (insert (format "| %s | %s | %s | %s | %s |\n"
                        (nth 0 row) (or (nth 1 row) "") (or (nth 2 row) "")
                        (or (nth 3 row) "") (or (nth 4 row) ""))))
      (save-excursion (forward-line -1) (org-table-align)))))

(defun org-canvas--submissions-table-row-cells (line)
  "Return the cells of table row LINE, trimmed, or nil for a rule or a non-row.
A row a grader is still typing may lack its closing bar."
  (let ((trimmed (string-trim line)))
    (when (and (string-prefix-p "|" trimmed) (not (string-prefix-p "|-" trimmed)))
      (mapcar #'string-trim
              (split-string (substring trimmed 1 (and (string-suffix-p "|" trimmed) -1))
                            "|")))))

(defun org-canvas--submissions-rubric-rows ()
  "Return the Rubric table rows of the entry at point, or nil without a table.
Each row is (ID CRITERION MAX SCORE COMMENT) as the table spells them,
cells trimmed and empty cells nil; the header row and rows without an
id are dropped."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-rubric-heading)))
    (let ((rows nil) (header t))
      (dolist (line (split-string (buffer-substring-no-properties (car region) (cdr region))
                                  "\n"))
        (when-let* ((cells (org-canvas--submissions-table-row-cells line)))
          (if header
              (setq header nil)
            (let ((row (mapcar (lambda (c) (and (not (string-empty-p c)) c))
                               (cl-subseq (append cells (make-list 5 "")) 0 5))))
              (when (car row) (push row rows))))))
      (nreverse rows))))

(defun org-canvas--submissions-rubric-cell-score (text)
  "Return TEXT, a Score cell, spelled as Canvas would return it, or as typed.
A number is normalized (2.0 becomes 2) so a typed score and the same
score pulled back digest alike; anything else is returned unchanged
for the push's check to name."
  (let ((parsed (and text (org-canvas--submissions-parse-score text))))
    (if (and parsed (not (equal parsed "EX")))
        (org-canvas--submissions-format-number (string-to-number parsed))
      text)))

(defun org-canvas--submissions-rubric-check-row (row name)
  "Return ROW as an (ID SCORE COMMENT) triple with the score normalized.
NAME is the student's, for the message.  Signal a `user-error' when the
Score cell is not a number or lies outside 0 to the criterion's Max, so
nothing is sent for anyone until the cell is fixed."
  (let* ((id (nth 0 row))
         (label (or (nth 1 row) id))
         (max (nth 2 row))
         (score (nth 3 row))
         (normalized (org-canvas--submissions-rubric-cell-score score))
         (limit (and max (string-match-p "\\`[0-9.]+\\'" max) (string-to-number max))))
    (when (and normalized (not (string-match-p "\\`[0-9]+\\.?[0-9]*\\'" normalized)))
      (user-error "Rubric score %S for %s (%s) is not a number" score name label))
    (when (and normalized limit (> (string-to-number normalized) limit))
      (user-error "Rubric score %s for %s (%s) is more than its %s points"
                  normalized name label max))
    (list id normalized (nth 4 row))))

(defun org-canvas--submissions-rubric-total (triples)
  "Return the sum of the scores in TRIPLES as a score string, or nil when none."
  (let ((scores (delq nil (mapcar #'cadr triples))))
    (when scores
      (org-canvas--submissions-format-number
       (apply #'+ (mapcar #'string-to-number scores))))))

(defun org-canvas--submissions-rubric-change-at-point (name)
  "Return the Rubric table edits of the entry at point as a plist, or nil.
NAME is the student's, for messages.  Nil when the entry has no table
or its rows digest to the CANVAS_RUBRIC baseline; a table emptied by
hand is not a change either, since Canvas clears an assessment only
from SpeedGrader.  The plist carries :triples (every row, checked),
:old-rubric and :new-rubric (the digests), :total (the scored rows'
sum), :filled and :of (how many rows carry a score, out of how many)."
  (when-let* ((rows (org-canvas--submissions-rubric-rows)))
    (let* ((triples (mapcar (lambda (r) (org-canvas--submissions-rubric-check-row r name))
                            rows))
           (new (org-canvas--submissions-rubric-digest triples))
           (old (org-entry-get (point) "CANVAS_RUBRIC")))
      (when (and new (not (equal new old)))
        (list :triples triples :old-rubric old :new-rubric new
              :total (org-canvas--submissions-rubric-total triples)
              :filled (cl-count-if #'cadr triples)
              :of (length triples))))))

(defun org-canvas--submissions-rubric-derived-score (rubric old-score new-score name)
  "Return the score plist due to a changed RUBRIC for NAME, or nil.
Only when the rubric is used for grading: Canvas then derives the grade
from the assessment, so the rows' total becomes the score unless
NEW-SCORE was itself edited away from OLD-SCORE.  An edited score
that disagrees with the total is a `user-error', since both cannot be
right; one that agrees, or an excusal, is left as typed."
  (when (and rubric (org-canvas--submissions-rubric-for-grading-p))
    (let ((total (plist-get rubric :total))
          (edited (not (equal new-score old-score))))
      (cond ((not edited)
             (list :new-score total :score-derived t))
            ((and new-score total (not (equal new-score "EX"))
                  (/= (string-to-number new-score) (string-to-number total)))
             (user-error "%s: SCORE %s disagrees with the rubric total %s; clear one of them"
                         name new-score total))))))

(defun org-canvas--submissions-rubric-set-row (id score comment)
  "Write SCORE and COMMENT into the Rubric row keyed by ID in the entry at point.
Either may be nil for an empty cell; the id, criterion and max cells
stay.  Return non-nil when the row exists."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-rubric-heading)))
    (save-excursion
      (goto-char (car region))
      (catch 'done
        (while (< (point) (cdr region))
          (let ((cells (org-canvas--submissions-table-row-cells
                        (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position)))))
            (when (equal (car cells) id)
              (let ((kept (cl-subseq (append cells (make-list 5 "")) 0 5)))
                (delete-region (line-beginning-position) (line-end-position))
                (insert (format "| %s | %s | %s | %s | %s |"
                                id (nth 1 kept) (nth 2 kept) (or score "") (or comment "")))
                (org-table-align)
                (throw 'done t))))
          (forward-line 1))
        nil))))

(defun org-canvas--submissions-rubric-fill-full ()
  "Give every Rubric row of the entry at point its Max as the Score.
Comments stay.  Return how many rows were filled, 0 without a table."
  (let ((n 0))
    (dolist (row (org-canvas--submissions-rubric-rows))
      (when (nth 2 row)
        (org-canvas--submissions-rubric-set-row (nth 0 row) (nth 2 row) (nth 4 row))
        (cl-incf n)))
    n))

(defun org-canvas--submissions-rubric-carryover ()
  "Return the unpushed Rubric rows of the entry at point, or nil.
The value is (:rows TRIPLES :baseline DIGEST): the rows as typed and
the CANVAS_RUBRIC they were typed against, so a re-render can put them
back and tell whether Canvas moved meanwhile.  Nil when the table
matches its baseline or is empty."
  (when-let* ((rows (org-canvas--submissions-rubric-rows)))
    (let* ((triples (mapcar (lambda (r)
                              (list (nth 0 r)
                                    (org-canvas--submissions-rubric-cell-score (nth 3 r))
                                    (nth 4 r)))
                            rows))
           (digest (org-canvas--submissions-rubric-digest triples))
           (baseline (org-entry-get (point) "CANVAS_RUBRIC")))
      (when (and digest (not (equal digest baseline)))
        (list :rows triples :baseline baseline)))))

(defun org-canvas--submissions-restore-rubric (carry)
  "Put CARRY, a `org-canvas--submissions-rubric-carryover' value, back at point.
The rows are written over the freshly pulled table.  When Canvas's
assessment moved since they were typed — the new CANVAS_RUBRIC is not
the baseline they were typed against — the heading is marked
CONFLICT, since the rows shown are the grader's and not what Canvas
holds."
  (dolist (row (plist-get carry :rows))
    (org-canvas--submissions-rubric-set-row (nth 0 row) (nth 1 row) (nth 2 row)))
  (unless (equal (org-entry-get (point) "CANVAS_RUBRIC") (plist-get carry :baseline))
    (org-entry-put (point) "CONFLICT"
                   "rubric assessed on Canvas since these rows were typed; the rows shown are yours, so check SpeedGrader before pushing")))

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
  "Post a text comment on the submission at point, right now.
Prompts for the text; for comments written while grading, use the
student's Comment to post heading and `S' instead."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (save-excursion
    (org-back-to-heading t)
    (let ((user-id (org-entry-get (point) "USER_ID"))
          (user-name (org-get-heading t t t t)))
      (unless user-id
        (user-error "No USER_ID at point"))
      (let ((text (read-string (format "Comment for %s: " user-name))))
        (when (string-empty-p text)
          (user-error "Empty comment"))
        (when (y-or-n-p (format "Post comment to %s? " user-name))
          (org-canvas--submissions-post-comment
           org-canvas-submissions--assignment-id user-id text)
          (org-canvas--submissions-append-comment-to-buffer user-name text)
          (when buffer-file-name (save-buffer))
          (message "Comment posted for %s" user-name))))))

(defun org-canvas--submissions-post-comment (assignment-id user-id text)
  "Post TEXT as a comment on USER-ID's submission to ASSIGNMENT-ID.
Canvas addresses a submission by the student's user id, not by the
submission's own id; the latter is a 404 (#125)."
  (let ((url (org-canvas-api-course-endpoint
              "assignments/%s/submissions/%s" assignment-id user-id)))
    (org-canvas-api-request 'PUT url
      :data `((comment . ((text_comment . ,text)))))))

(defun org-canvas--submissions-append-comment-to-buffer (_user-name text)
  "Record posted comment TEXT under the Comments heading of the entry at point.
A Comments heading is created when missing, ahead of the Comment to post
heading so the record reads in order.  Newlines in TEXT become spaces in
the one-line record; Canvas keeps the original."
  (let ((inhibit-read-only t)
        (line (format "- *You* %s :: %s\n"
                      (format-time-string "<%Y-%m-%d %a %H:%M>")
                      (replace-regexp-in-string "\n+" " " text))))
    (save-excursion
      (org-back-to-heading t)
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (if (re-search-forward "^\\*\\* Comments$" end t)
            (progn
              (forward-line 1)
              (while (and (< (point) end) (looking-at "^- "))
                (forward-line 1))
              (insert line))
          ;; Org 9.6 returns t from `org-back-to-heading', later versions the
          ;; position; never use its value.
          (org-back-to-heading t)
          (if (re-search-forward (concat "^" (regexp-quote org-canvas--submissions-draft-heading) "$") end t)
              (progn (beginning-of-line)
                     (insert "** Comments\n" line "\n"))
            (goto-char end)
            (insert "\n** Comments\n" line)))))))

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

;;;; Completion Rule

(defun org-canvas--submissions-completion-score (points window)
  "Return the score the completion rule gives the entry at point.
POINTS for a submission within WINDOW days of lateness, 0 for a later
one or a missing one, nil when there is nothing to grade."
  (let ((status (org-entry-get (point) "STATUS"))
        (days (org-entry-get (point) "DAYS_LATE")))
    (cond ((member status '("missing" "unsubmitted")) "0")
          ((and days (> (string-to-number days) window)) "0")
          ((member status '("submitted" "late" "graded" "pending_review"))
           (org-canvas--submissions-format-number points))
          (t nil))))

(defun org-canvas--submissions-default-points ()
  "Return the assignment's points from the file header, or nil."
  (let ((p (org-canvas--submissions-file-property "POINTS_POSSIBLE")))
    (and p (string-to-number p))))

;;;###autoload
(defun org-canvas-submissions-apply-completion-rule (&optional points overwrite)
  "Score every ungraded student in this grading file by completion.
A submission within `org-canvas-submissions-late-window-days' of the
due date gets POINTS, a later or missing one gets 0.  Only headings with
an empty SCORE are touched, so hand grading is never overwritten, unless
OVERWRITE (the prefix argument) is given; excused rows are always left
alone.  On an assignment with a rubric, full credit also fills every
row of the student's Rubric table with its Max, so the rubric cells the
student sees are populated too; a 0 leaves the rows empty.  Nothing is
pushed: review the scores, then press S.  When POINTS is nil it is read
from the minibuffer, after the buffer checks, with the file's
POINTS_POSSIBLE as the default."
  ;; Bare `(interactive)' rather than `(interactive (list ...))': a sexp
  ;; argument to `interactive' makes edebug skip the defun, which blanks
  ;; undercover's line counts for the whole body (see CLAUDE.md).
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (unless points
    (setq points (read-number "Full credit points: "
                              (or (org-canvas--submissions-default-points) 0))
          overwrite current-prefix-arg))
  (let ((window org-canvas-submissions-late-window-days)
        (full 0) (zero 0) (skipped 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let* ((current (org-entry-get (point) "SCORE"))
               (has (and current (not (string-empty-p (string-trim current)))))
               (score (org-canvas--submissions-completion-score points window)))
          (cond ((or (equal (and has (org-canvas--submissions-parse-score current)) "EX")
                     (and has (not overwrite))
                     (null score))
                 (cl-incf skipped))
                (t (org-entry-put (point) "SCORE" score)
                   (if (equal score "0")
                       (cl-incf zero)
                     (org-canvas--submissions-rubric-fill-full)
                     (cl-incf full)))))
        (forward-line 1)))
    (message "Completion rule: %d at %s, %d at 0 (missing or more than %d day(s) late), %d left as they were; press S to push"
             full (org-canvas--submissions-format-number points) zero window skipped)))

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
             "access_token=" (org-canvas--api-token))
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
rubric header of the detail view.  Notes and drafted comments already
in the file are carried over to the new render."
  (let ((buf (if (eq view 'detail)
                 (org-canvas--submissions-grading-buffer assignment-name)
               (get-buffer-create (format "*submissions: %s*" assignment-name)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (carry (and (eq view 'detail) (org-canvas--submissions-collect-carryover))))
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (if (eq view 'summary)
            (org-canvas--submissions-render-summary
             assignment-name assignment-id submissions)
          (org-canvas--submissions-render-detail
           assignment-name assignment-id submissions assignment)
          (org-canvas--submissions-restore-carryover carry))
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
  (let ((buf (org-canvas--find-file-noselect (org-canvas--submissions-file-path assignment-name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (org-canvas--submissions-ensure-context)
      (org-canvas--submissions-guard-unpushed "Re-pull"))
    buf))


;;;; Grade Writing

(defun org-canvas--submissions-snapshot-scores (submissions)
  "Build an alist of (user-id . score-string) from SUBMISSIONS.
The score is spelled as the buffer shows it: a number, or EX."
  (mapcar (lambda (sub)
            (cons (org-canvas--submissions-user-id sub)
                  (org-canvas--submissions-shown-score sub)))
          submissions))

(defun org-canvas--submissions-parse-score (score-string)
  "Parse SCORE-STRING into a string suitable for Canvas posted_grade.
Handles \"92\", \"85.5\", \"95/100\" (extracts numerator), and \"EX\" or
\"excused\" in any case, which becomes \"EX\".  Trims whitespace.
Returns nil for anything else, including empty input."
  (when (and score-string (stringp score-string))
    (let ((trimmed (string-trim score-string)))
      (cond
       ((string-empty-p trimmed) nil)
       ((string-match-p "\\`\\(?:ex\\|excused\\)\\'" (downcase trimmed)) "EX")
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
back to the in-memory snapshot taken at render time.  Each heading's
Rubric table is compared with its CANVAS_RUBRIC the same way."
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
  "Return the grade change plist for the heading at point, or nil.
A change is a SCORE that differs from its baseline, a Rubric table
that differs from its baseline, or both; the rubric keys are those of
`org-canvas--submissions-rubric-change-at-point', and :score-derived
marks a score the rubric's total set
\(`org-canvas--submissions-rubric-derived-score')."
  (let* ((user-id-str (org-entry-get (point) "USER_ID"))
         (user-id (when user-id-str (string-to-number user-id-str)))
         (name (org-get-heading t t t t))
         (baseline (org-entry-get (point) "CANVAS_SCORE"))
         (old-score (if baseline
                        (org-canvas--submissions-parse-score baseline)
                      (alist-get user-id org-canvas-submissions--original-scores)))
         (new-score (org-canvas--submissions-parse-score
                     (org-entry-get (point) "SCORE")))
         (attempt (org-entry-get (point) "ATTEMPT"))
         (rubric (and user-id (org-canvas--submissions-rubric-change-at-point name)))
         (derived (org-canvas--submissions-rubric-derived-score
                   rubric old-score new-score name)))
    (when (and user-id (or rubric (not (equal new-score old-score))))
      (append (list :user-id user-id
                    :name name
                    :old-score old-score
                    :new-score (if derived (plist-get derived :new-score) new-score)
                    :attempt (and attempt (string-to-number attempt)))
              derived
              rubric))))

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

(defun org-canvas--submissions-change-sends-grade-p (change)
  "Return non-nil when CHANGE carries a grade to send.
A score that moved, a cleared one included, or one the rubric derived."
  (or (plist-get change :score-derived)
      (not (equal (plist-get change :new-score) (plist-get change :old-score)))))

(defun org-canvas--submissions-rubric-payload (triples)
  "Return TRIPLES as the rubric_assessment object Canvas accepts.
Each scored or commented criterion maps its id to points and comments;
Canvas picks the rating from the points.  Unscored rows are left out."
  (delq nil
        (mapcar (lambda (triple)
                  (pcase-let ((`(,id ,score ,comment) triple))
                    (when (or score comment)
                      (cons (intern id)
                            (append (and score `((points . ,(string-to-number score))))
                                    (and comment `((comments . ,comment))))))))
                triples)))

(defun org-canvas--submissions-grade-fields (change)
  "Return the fields CHANGE sends for its student, as one grade_data entry.
`posted_grade' when the score moves, `rubric_assessment' when the
Rubric rows did; the bulk endpoint takes the entry as is and the
single PUT nests the grade under `submission'."
  (append (and (org-canvas--submissions-change-sends-grade-p change)
               `((posted_grade . ,(plist-get change :new-score))))
          (and (plist-get change :triples)
               `((rubric_assessment
                  . ,(org-canvas--submissions-rubric-payload (plist-get change :triples)))))))

(defun org-canvas--submissions-push-single-grade (assignment-id change)
  "Push CHANGE for its student on ASSIGNMENT-ID via PUT."
  (let* ((url (org-canvas-api-course-endpoint
               "assignments/%s/submissions/%s" assignment-id (plist-get change :user-id)))
         (fields (org-canvas--submissions-grade-fields change))
         (grade (assq 'posted_grade fields))
         (rubric (assq 'rubric_assessment fields)))
    (org-canvas-api-request 'PUT url
      :data (append (and grade `((submission . (,grade))))
                    (and rubric (list rubric))))))

(defun org-canvas--submissions-push-bulk-grades (assignment-id diffs)
  "Push grade DIFFS for ASSIGNMENT-ID via the bulk update_grades endpoint.
DIFFS is a list of change plists; each student's entry carries the
grade, the rubric assessment, or both."
  (let* ((grade-data
          (mapcar (lambda (ch)
                    (cons (number-to-string (plist-get ch :user-id))
                          (org-canvas--submissions-grade-fields ch)))
                  diffs))
         (url (org-canvas-api-course-endpoint
               "assignments/%s/submissions/update_grades" assignment-id)))
    (org-canvas-api-request 'POST url
      :data `((grade_data . ,grade-data)))))

(defun org-canvas--submissions-live-baselines (assignment-id)
  "Fetch ASSIGNMENT-ID's submissions as (user-id . (score attempt rubric)).
The score is spelled as the buffer shows it (a number or EX), or nil
when ungraded; the rubric is the CANVAS_RUBRIC digest of the
assessment Canvas holds, or nil."
  (mapcar (lambda (sub)
            (cons (org-canvas--submissions-user-id sub)
                  (list (org-canvas--submissions-shown-score sub)
                        (alist-get 'attempt sub)
                        (org-canvas--submissions-submission-rubric-digest sub))))
          (org-canvas--submissions-fetch-for-assignment assignment-id)))

(defun org-canvas--submissions-conflict-p (change live)
  "Return why CHANGE conflicts with LIVE Canvas state, or nil.
LIVE is (score attempt rubric) for the same student, or nil if gone.
The rubric is compared only when CHANGE sends one."
  (let ((live-score (nth 0 live))
        (live-attempt (nth 1 live))
        (live-rubric (nth 2 live))
        (attempt (plist-get change :attempt)))
    (cond ((not (equal live-score (plist-get change :old-score)))
           (format "Canvas now has %s" (or live-score "no grade")))
          ((and attempt (numberp live-attempt) (/= attempt live-attempt))
           (format "resubmitted (attempt %s)" live-attempt))
          ((and (plist-get change :triples)
                (not (equal live-rubric (plist-get change :old-rubric))))
           "rubric assessed on Canvas since the pull"))))

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

(defun org-canvas--submissions-describe-rubric (change)
  "Return the note CHANGE's rubric rows add to its line, or an empty string."
  (if (plist-get change :triples)
      (format " (rubric %d/%d)" (plist-get change :filled) (plist-get change :of))
    ""))

(defun org-canvas--submissions-describe-changes (diffs)
  "Return DIFFS as one line per student: name, old score, new score, rubric."
  (mapconcat (lambda (ch)
               (format "  %s: %s → %s%s"
                       (plist-get ch :name)
                       (or (plist-get ch :old-score) "nil")
                       (or (plist-get ch :new-score) "nil")
                       (org-canvas--submissions-describe-rubric ch)))
             diffs "\n"))

(defun org-canvas--submissions-send-grades (assignment-id diffs)
  "Send DIFFS for ASSIGNMENT-ID: one PUT, or the bulk endpoint for several."
  (if (= (length diffs) 1)
      (org-canvas--submissions-push-single-grade assignment-id (car diffs))
    (org-canvas--submissions-push-bulk-grades assignment-id diffs)))

(defun org-canvas--submissions-record-pushed-at-point (change)
  "Make CHANGE the baseline of the heading at point.
CANVAS_SCORE follows the score, SCORE too when the rubric derived it,
CANVAS_RUBRIC follows the rows sent, and CONFLICT is cleared."
  (let ((score (plist-get change :new-score)))
    (if score
        (org-entry-put (point) "CANVAS_SCORE" score)
      (org-entry-delete (point) "CANVAS_SCORE"))
    (when (plist-get change :score-derived)
      (org-entry-put (point) "SCORE" score))
    (when (plist-get change :new-rubric)
      (org-entry-put (point) "CANVAS_RUBRIC" (plist-get change :new-rubric)))
    (org-entry-delete (point) "CONFLICT")))

(defun org-canvas--submissions-record-pushed (diffs)
  "Make DIFFS the new baseline: snapshot, the heading properties, and the file."
  (dolist (ch diffs)
    (setf (alist-get (plist-get ch :user-id) org-canvas-submissions--original-scores)
          (plist-get ch :new-score)))
  (when (eq org-canvas-submissions--current-view 'detail)
    (save-excursion
      (dolist (ch diffs)
        (when (org-canvas--submissions-goto-user (plist-get ch :user-id))
          (org-canvas--submissions-record-pushed-at-point ch))))
    (org-canvas--submissions-refresh-links))
  (when buffer-file-name
    (save-buffer)))

(defun org-canvas--submissions-post-drafts (assignment-id drafts)
  "Post each of DRAFTS to ASSIGNMENT-ID, recording it under Comments as it lands.
The draft is reset to the template after its comment is posted, so a
failure midway leaves the file accurate: posted comments are recorded,
unposted ones still drafted.  Return the number posted."
  (let ((posted 0))
    (dolist (d drafts)
      (org-canvas--submissions-post-comment assignment-id (plist-get d :user-id) (plist-get d :text))
      (save-excursion
        (when (org-canvas--submissions-goto-user (plist-get d :user-id))
          (org-canvas--submissions-append-comment-to-buffer (plist-get d :name) (plist-get d :text))
          (org-canvas--submissions-reset-draft)))
      (cl-incf posted))
    posted))

(defun org-canvas--submissions-describe-push (diffs drafts)
  "Return a one-line summary of DIFFS and DRAFTS for the confirmation.
Rubric assessments among DIFFS are counted, and those scoring only some
of their criteria named, since Canvas accepts a partial assessment."
  (let ((rubrics (cl-count-if (lambda (ch) (plist-get ch :triples)) diffs))
        (partial (cl-count-if (lambda (ch)
                                (and (plist-get ch :triples)
                                     (< (plist-get ch :filled) (plist-get ch :of))))
                              diffs)))
    (concat (when diffs (format "%d grade change(s)" (length diffs)))
            (when (> rubrics 0)
              (format " (%d with rubric%s)" rubrics
                      (if (> partial 0) (format ", %d partly scored" partial) "")))
            (when (and diffs drafts) " and ")
            (when drafts (format "%d comment(s)" (length drafts))))))

;;;###autoload
(cl-defun org-canvas-submissions-push-grades ()
  "Push every changed score, rubric table, and drafted comment in this buffer.
In a grading file a change is a SCORE that differs from its
CANVAS_SCORE or a Rubric table whose rows differ from its
CANVAS_RUBRIC; a drafted comment is text under a student's Comment to
post heading.  A rubric used for grading sets the score from its rows'
total.  Changes that conflict with what Canvas holds now are skipped
and marked (see `org-canvas-submissions-check-conflicts').  After a
successful push the baselines, the comment records, and the file are
updated."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (let ((assignment-id org-canvas-submissions--assignment-id)
        (drafts (org-canvas--submissions-collect-comment-drafts)))
    (unless assignment-id
      (user-error "No CANVAS_ASSIGNMENT_ID in this buffer"))
    (pcase-let ((`(,changes . ,conflicts)
                 (org-canvas--submissions-partition-conflicts
                  assignment-id (org-canvas--submissions-collect-grade-changes))))
      (org-canvas--submissions-mark-conflicts conflicts)
      (unless (or changes drafts)
        (message "Nothing to push%s"
                 (if conflicts (format " (%d conflict(s) marked)" (length conflicts)) ""))
        (cl-return-from org-canvas-submissions-push-grades))
      (when changes
        (message "Grade changes:\n%s" (org-canvas--submissions-describe-changes changes)))
      (when (y-or-n-p (format "Push %s%s? "
                              (org-canvas--submissions-describe-push changes drafts)
                              (if conflicts
                                  (format ", skipping %d conflict(s)" (length conflicts))
                                "")))
        (condition-case err
            (let ((posted 0))
              (when changes
                (org-canvas--submissions-send-grades assignment-id changes))
              (setq posted (org-canvas--submissions-post-drafts assignment-id drafts))
              (org-canvas--submissions-record-pushed changes)
              (message "Pushed %d grade(s) and %d comment(s)" (length changes) posted)
              (org-canvas--submissions-offer-to-post changes))
          (error (org-canvas--user-message "Error pushing: %s" (error-message-string err))))))))

;;;; Posting Grades

(defun org-canvas--submissions-post-manually-p ()
  "Return non-nil when this grading file's grades are held until posted.
Reads the POST_POLICY keyword the pull wrote from the assignment's
effective policy; a file without it, pulled before the keyword
existed, answers nil and nothing is offered."
  (equal (org-canvas--submissions-file-property "POST_POLICY") "manual"))

(defun org-canvas--submissions-offer-to-post (diffs)
  "Offer to post the grades just pushed as DIFFS, under a manual policy.
Under a manual post policy a pushed grade stays hidden from the
student until it is posted (issue #202)."
  (when (and diffs
             (org-canvas--submissions-post-manually-p)
             (y-or-n-p "This assignment posts grades manually; post them now? "))
    (org-canvas-submissions-post-grades)))

;;;###autoload
(defun org-canvas-submissions-post-grades ()
  "Post this grading file's assignment grades, so students can see them.
The gradebook's Post Grades for the assignment, as the GraphQL
mutation `postAssignmentGrades' (issue #202).  Needed only under a
manual post policy; harmless otherwise.  Press g afterwards to see
each student's POSTED_AT."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (let ((assignment-id org-canvas-submissions--assignment-id))
    (unless assignment-id
      (user-error "No CANVAS_ASSIGNMENT_ID in this buffer"))
    (org-canvas--graphql-mutate
     (format "post the grades of assignment %s" assignment-id)
     "mutation ($assignmentId: ID!) { postAssignmentGrades(input: {assignmentId: $assignmentId}) { progress { _id state } } }"
     (list (cons 'assignmentId (format "%s" assignment-id))))
    (message "Grades posted for assignment %s." assignment-id)))

(provide 'org-canvas-submissions)
;;; org-canvas-submissions.el ends here
