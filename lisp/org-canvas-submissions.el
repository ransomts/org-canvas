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
;; 3. Grade.  Edit :SCORE: on each heading, or fill the Score cells of
;;    the student's Rubric table and the `- ID :: comment' items under
;;    it; `c' posts a comment.  :LATE_STATUS: marks a student late,
;;    missing or extended, or none.  :SCORE: none (or -) takes away a
;;    grade Canvas holds; an absent SCORE leaves it alone.
;; 4. `S' pushes every SCORE that differs from its CANVAS_SCORE, the
;;    score as last pulled or pushed, and every Rubric section whose
;;    rows differ from its CANVAS_RUBRIC, the assessment as last pulled
;;    or pushed.  When the rubric is used for grading the row total sets
;;    the score.  Before pushing from a saved file Canvas is re-read: a
;;    student whose grade or assessment changed there since the pull,
;;    or who resubmitted, is skipped and marked :CONFLICT: rather than
;;    overwritten (`org-canvas-submissions-check-conflicts').  A
;;    heading marked :CONFLICT: is never pushed until `t' (take
;;    Canvas's) or `k' (keep mine) resolves it.  Pushed scores and
;;    assessments become the new baselines and the file is saved.
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
;; u  the text of every downloaded upload, pairs grouped (SAME_AS)
;; c  post a comment on the student at point
;; S  push grade changes (and new saved comments)
;; B  read the comment bank into the Comment Bank heading
;; x  delete the saved comment at point from the bank
;;
;; PRIVACY
;; =======
;; Grading files are student work.  The directory gets a .gitignore
;; when created (`org-canvas-submissions-write-gitignore') so they never
;; travel with a course repository; a file git tracked before that
;; warns, since a .gitignore does not untrack (issue #549).

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;; Defined below this file (a sibling split off the same module), so
;; declared rather than required: requiring would be a cycle.
(declare-function org-canvas--submissions-body-text "org-canvas-submissions-render"
                  (html))
(declare-function org-canvas--submissions-collect-grade-changes "org-canvas-submissions-push"
                  ())
(declare-function org-canvas--submissions-format-number "org-canvas-submissions-render"
                  (n))
(declare-function org-canvas--submissions-rubric-for-grading "org-canvas-submissions-comment-items"
                  (assignment))
(declare-function org-canvas-push-submission-comment-edits "org-canvas-submissions-push"
                  (&optional assignment))
(declare-function org-canvas-submissions-add-comment "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas-submissions-delete-comment-bank-item "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas-submissions-download-all-attachments "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas-submissions-download-attachments "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas-submissions-keep-mine "org-canvas-submissions-conflict"
                  ())
(declare-function org-canvas-submissions-post-grades "org-canvas-submissions-push"
                  ())
(declare-function org-canvas-submissions-pull-comment-bank "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas-submissions-push-at-point "org-canvas-submissions-conflict"
                  ())
(declare-function org-canvas-submissions-push-grades "org-canvas-submissions-conflict"
                  ())
(declare-function org-canvas-submissions-refresh "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas-submissions-take-canvas "org-canvas-submissions-conflict"
                  ())
(declare-function org-canvas-submissions-toggle-view "org-canvas-submissions-render"
                  ())

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
CONFLICT property instead of being overwritten.  A heading already
marked CONFLICT is held back whatever this says (issue #440)."
  :type 'boolean
  :group 'org-canvas)

(defcustom org-canvas-submissions-include-rubric-criteria t
  "Form of the assignment's rubric under a grading file's Rubric: line.
t writes the rubric in the shape of the rubrics file — a `* Rubric'
heading with one sub-heading per criterion: its description, points
and long description, then a `| Rating | Points | Description |'
table — so the work can be read against the rubric offline, the text
that separates one rating from the next included (issue #265).
`summary' writes one `| Criterion | Points | Ratings |' table, a line
per criterion; nil keeps just the line."
  :type '(choice (const :tag "The rubric in full" t)
                 (const :tag "One line per criterion" summary)
                 (const :tag "Just the Rubric: line" nil))
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

(defcustom org-canvas-submissions-comment-bank-template
  "# Saved comments for SpeedGrader's comment library, one list item each.
# S creates the new ones and B reads the library in; an item labelled
# with a number is on Canvas already.  Removing an item here never
# deletes it there: x on the item does, after asking."
  "Text placed under a grading file's Comment Bank heading at pull time.
Written as Org comment lines, so it is never sent.  Nil leaves the
heading out of a new grading file; one a file already has is kept."
  :type '(choice (const :tag "No Comment Bank heading" nil) string)
  :group 'org-canvas)

(defcustom org-canvas-submissions-late-window-days 2
  "Days of lateness the completion rule still gives full credit for.
`org-canvas-submissions-apply-completion-rule' scores a submission later
than this as 0.  Canvas's own late policy, if any, still deducts inside
the window."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-submissions-progress-timeout 180
  "Seconds a bulk grade push waits for Canvas to apply the grades.
Canvas applies several grades at once in a background job and answers
the push with that job's progress; the push reads the progress again
every `org-canvas-submissions-progress-interval' seconds until the job
has completed or failed.  When this many seconds pass first, the push
reports the grades as unconfirmed, records no baseline for them and
offers no posting, so the next push sends them again (issue #382)."
  :type 'number
  :group 'org-canvas)

(defcustom org-canvas-submissions-progress-interval 2
  "Seconds between two reads of a bulk grade push's progress.
See `org-canvas-submissions-progress-timeout'."
  :type 'number
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

(defvar-local org-canvas-submissions--last-refresh nil
  "The line saying what the last render of this grading file changed.
Nil after a first pull, which has nothing to compare, and in a summary
buffer.  Kept so a caller that pulled several columns can report each
one's changes after the echo area has moved on (issue #415).")

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
    (define-key map (kbd "B") #'org-canvas-submissions-pull-comment-bank)
    (define-key map (kbd "x") #'org-canvas-submissions-delete-comment-bank-item)
    (define-key map (kbd "C") #'org-canvas-push-submission-comment-edits)
    (define-key map (kbd "t") #'org-canvas-submissions-take-canvas)
    (define-key map (kbd "k") #'org-canvas-submissions-keep-mine)
    (define-key map (kbd "s") #'org-canvas-submissions-push-at-point)
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

(defun org-canvas--submissions-sanitize-filename (name)
  "Sanitize NAME for use as a directory/filename.
Replaces problematic characters with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" name))

(defun org-canvas--submissions-file-path (assignment-name)
  "Return the grading file path for ASSIGNMENT-NAME."
  (expand-file-name
   (format "%s.org" (org-canvas--submissions-sanitize-filename assignment-name))
   (org-canvas--submissions-dir)))

;;;; Finding a Column's Grading File (issue #431)

;; A grading file is named after its column, but the column is the id
;; in its header: a column renamed on Canvas keeps its id, and the file
;; under the old name keeps every score typed in it.  So a column's file
;; is looked up by CANVAS_ASSIGNMENT_ID first and by name second, and
;; the pull renames a file it found under an old name.  Two files that
;; claim one id are named and neither is picked.

(defvar org-canvas--submissions-id-index nil
  "Hash of assignment id to the grading files claiming it, or nil.
Bound by a read-only command that looks up many columns at once (the
grading queue), so the directory is read once; nil reads it afresh at
each lookup, which a command that renames files needs.")

(defun org-canvas--submissions-header-files ()
  "Return the .org files under the submissions directory, sorted.
Emacs lock files (.#NAME.org, dangling links) and other dot files are
left out."
  (let ((dir (org-canvas--submissions-dir)))
    (when (file-directory-p dir)
      (directory-files dir t "\\`[^.#].*\\.org\\'"))))

(defun org-canvas--submissions-grading-files-by-id ()
  "Return a hash of assignment id to the grading files whose header names it.
Only the first part of each file is read; no request is made."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (file (org-canvas--submissions-header-files))
      (when-let* ((id (condition-case nil
                          (org-canvas--submissions-file-assignment-id file)
                        (file-error nil))))
        (puthash id (append (gethash id index) (list file)) index)))
    index))

(defun org-canvas--submissions-files-for-id (id)
  "Return the grading files whose CANVAS_ASSIGNMENT_ID is ID, sorted."
  (gethash (format "%s" id)
           (or org-canvas--submissions-id-index
               (org-canvas--submissions-grading-files-by-id))))

(defun org-canvas--submissions-id-p (assignment)
  "Return non-nil when ASSIGNMENT is an id: an integer or a string of digits."
  (or (integerp assignment)
      (and (stringp assignment) (string-match-p "\\`[0-9]+\\'" assignment))))

(defun org-canvas--submissions-locate-file (assignment-name assignment-id)
  "Return the grading file of ASSIGNMENT-NAME (ASSIGNMENT-ID), or a list.
The file whose header names ASSIGNMENT-ID wins, whatever its name;
without one the path is the name's, existing or not.  When two or more
files claim the id the value is their list, for the caller to report."
  (let ((files (and assignment-id
                    (org-canvas--submissions-files-for-id assignment-id))))
    (cond ((cdr files) files)
          (files (car files))
          (t (org-canvas--submissions-file-path assignment-name)))))

(defun org-canvas--submissions-duplicate-error (assignment-id files)
  "Signal a `user-error' naming FILES, which all claim ASSIGNMENT-ID."
  (user-error "Grading files %s all claim assignment %s; keep one, merge \
what was typed in the others into it, delete them and try again"
              (mapconcat #'file-name-nondirectory files " and ")
              assignment-id))

(defun org-canvas--submissions-claimed-file (assignment-id)
  "Return the grading file claiming ASSIGNMENT-ID, or nil when none does.
Two files claiming it are a `user-error' naming both."
  (let ((files (org-canvas--submissions-files-for-id assignment-id)))
    (when (cdr files)
      (org-canvas--submissions-duplicate-error assignment-id files))
    (car files)))

(defun org-canvas--submissions-same-file-p (a b)
  "Return non-nil when paths A and B name one file, compared as truenames."
  (equal (file-truename a) (file-truename b)))

(defun org-canvas--submissions-attachment-root (file)
  "Return the files/<assignment>/ directory of the grading FILE."
  (expand-file-name (format "files/%s/" (file-name-base file))
                    (file-name-directory file)))

(defun org-canvas--submissions-move-attachments (old new)
  "Move the attachment directory of grading file OLD to that of NEW.
Return non-nil when it moved.  A directory already under the new name
is left alone, with a warning, rather than merged."
  (let ((from (org-canvas--submissions-attachment-root old))
        (to (org-canvas--submissions-attachment-root new)))
    (cond ((not (file-directory-p from)) nil)
          ((file-exists-p to)
           (org-canvas--log-warning org-canvas--logger
             "[Submissions] Left %s where it is: %s already exists"
             from to)
           nil)
          (t (rename-file (directory-file-name from) (directory-file-name to))
             t))))

(defun org-canvas--submissions-retitle (assignment-name old new moved)
  "Rewrite this grading file's header for ASSIGNMENT-NAME.
The title and CANVAS_ASSIGNMENT_NAME name the column again, and when
MOVED the attachment links follow the directory from that of the
grading file OLD to that of NEW."
  (org-canvas--submissions-replace-header-line
   "^#\\+TITLE: .*$" (format "#+TITLE: Submissions: %s" assignment-name))
  (org-canvas--submissions-replace-header-line
   "^#\\+PROPERTY: CANVAS_ASSIGNMENT_NAME .*$"
   (format "#+PROPERTY: CANVAS_ASSIGNMENT_NAME %s" assignment-name))
  (when moved
    (save-excursion
      (goto-char (point-min))
      (let ((from (format "[[file:files/%s/" (file-name-base old)))
            (to (format "[[file:files/%s/" (file-name-base new)))
            (inhibit-read-only t))
        (while (search-forward from nil t)
          (replace-match to t t)))))
  (when (local-variable-p 'org-canvas-submissions--assignment-name)
    (setq-local org-canvas-submissions--assignment-name assignment-name)))

(defun org-canvas--submissions-rename-grading-file (old new assignment-name)
  "Rename the grading file OLD to NEW, the file of ASSIGNMENT-NAME; return NEW.
Its attachment directory moves with it and its header is rewritten.  A
buffer visiting OLD follows the rename; one holding unsaved changes is
a `user-error', as is a NEW that already exists, so nothing typed is
lost and no file is written over."
  (let ((buf (find-buffer-visiting old)))
    (when (file-exists-p new)
      (user-error "Grading file %s is for a column now named %s, but %s \
already exists; merge the two by hand"
                  (file-name-nondirectory old) assignment-name
                  (file-name-nondirectory new)))
    (when (and buf (buffer-modified-p buf))
      (user-error "Grading file %s has unsaved changes and its column is \
now named %s; save it, then try again"
                  (file-name-nondirectory old) assignment-name))
    (rename-file old new)
    (when buf
      (with-current-buffer buf (set-visited-file-name new t t)))
    (let ((moved (org-canvas--submissions-move-attachments old new)))
      (with-current-buffer (or buf (org-canvas--find-file-noselect new))
        (org-canvas--submissions-retitle assignment-name old new moved)
        (org-canvas--save-buffer)))
    (org-canvas--log-info org-canvas--logger
      "[Submissions] Renamed grading file %s to %s: its column is now named %s"
      (file-name-nondirectory old) (file-name-nondirectory new) assignment-name)
    new))

(defun org-canvas--submissions-grading-file-for (assignment-name assignment-id)
  "Return the grading file path of ASSIGNMENT-NAME (ASSIGNMENT-ID) for a pull.
The file claiming ASSIGNMENT-ID is renamed to ASSIGNMENT-NAME's when
it carries an old name; two files claiming it are a `user-error'
naming both, and nothing is written."
  (let ((found (org-canvas--submissions-locate-file
                assignment-name assignment-id))
        (target (org-canvas--submissions-file-path assignment-name)))
    (cond ((consp found)
           (org-canvas--submissions-duplicate-error assignment-id found))
          ((org-canvas--submissions-same-file-p found target) target)
          (t (org-canvas--submissions-rename-grading-file
              found target assignment-name)))))

(defun org-canvas--submissions-ensure-directory ()
  "Create the submissions directory and return it.
Write a .gitignore there once, when
`org-canvas-submissions-write-gitignore' is non-nil, so the student
work the directory holds never travels with a course repository.
Warn when git already tracks files under it — a .gitignore does not
untrack (issue #549)."
  (let ((dir (org-canvas--submissions-dir)))
    (make-directory dir t)
    (when org-canvas-submissions-write-gitignore
      (let ((gitignore (expand-file-name ".gitignore" dir)))
        (unless (file-exists-p gitignore)
          (with-temp-file gitignore
            (insert "# Student work pulled by org-canvas.  Never commit it.\n"
                    "*\n!.gitignore\n")))))
    ;; Grading files, quiz attempts and peer reviews are student data
    ;; (issue #549); a tracked one warns, since the .gitignore above
    ;; does not untrack what was committed before it existed.
    (when (eql 0 (org-canvas--git dir "ls-files" "--error-unmatch" "--" "."))
      (org-canvas--log-warning org-canvas--logger
        "[Submissions] git tracks files under %s: student work travels with the repository.  Run git rm --cached on them"
        dir))
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
gets them back from its #+PROPERTY: header and its headings.  A file
with that header is a grading file, whose view is `detail' even before
it holds a student; the summary table is never saved (issue #381)."
  (unless org-canvas-submissions--assignment-id
    (setq org-canvas-submissions--assignment-id
          (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID")))
  (unless org-canvas-submissions--assignment-name
    (setq org-canvas-submissions--assignment-name
          (or (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_NAME")
              (and buffer-file-name (file-name-base buffer-file-name)))))
  (unless org-canvas-submissions--current-view
    (setq org-canvas-submissions--current-view
          (if (or (and buffer-file-name
                       (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID"))
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "^[ \t]*:USER_ID:" nil t)))
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
Only the summary table asks: a re-pull of the grading file carries
typed scores, drafted comments, notes and Rubric rows over, and a
score the rows derive comes back with them (issue #281)."
  (cl-count-if (lambda (ch)
                 (and (not (plist-get ch :score-derived))
                      (not (equal (plist-get ch :new-score) (plist-get ch :old-score)))))
               (org-canvas--submissions-collect-grade-changes)))

(defun org-canvas--submissions-guard-unpushed (verb)
  "Ask before VERB (a capitalized verb) discards unpushed edits.
Under `noninteractive' there is nobody to ask: the edits are named in
a message and VERB goes ahead, since a prompt a batch Emacs cannot
answer would hang it (issue #281)."
  (let ((pending (org-canvas--submissions-pending-count)))
    (when (> pending 0)
      (cond (noninteractive
             (message "%d unpushed score change(s) lost by %s" pending verb))
            ((not (y-or-n-p
                   (format "%d unpushed score change(s) will be lost; %s anyway? "
                           pending verb)))
             (user-error "%s cancelled" verb))))))

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

(defun org-canvas--submissions-due-at (submission)
  "Return the due time SUBMISSION was late against, as an Org timestamp.
Nil unless it arrived late (`org-canvas--submissions-days-late').  The
time is Canvas's `cached_due_date', the student's own due date with
any override applied; without it, `submitted_at' less `seconds_late'.
The late-note draft names it (issue #484)."
  (when (org-canvas--submissions-days-late submission)
    (let ((cached (org-canvas--alist-get-non-null 'cached_due_date submission)))
      (if (stringp cached)
          (org-canvas--iso8601-to-org-timestamp cached)
        (when-let* ((at (org-canvas--alist-get-non-null 'submitted_at submission))
                    ((org-canvas--iso8601-date-p at)))
          (format-time-string
           "<%Y-%m-%d %a %H:%M>"
           (time-subtract (date-to-time at) (alist-get 'seconds_late submission))
           (org-canvas--time-zone)))))))

(defun org-canvas--submissions-file-assignment-id (file)
  "Return the CANVAS_ASSIGNMENT_ID in the header of the grading FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID")))

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
  (let* ((feature (cl-find endpoint (org-canvas--feature-entries)
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
        (grading (org-canvas--submissions-rubric-for-grading assignment)))
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

(defun org-canvas--submissions-comment-text (text)
  "Return TEXT spelled as it is in a Rubric comment item, or nil when blank.
Line ends become LF, every line loses the whitespace around it, a run
of blank lines collapses to one and the ends are dropped, so a comment
as Canvas returns it and the same comment read back from its item
digest alike (issue #263)."
  (when (stringp text)
    (let ((lines nil) (blank nil))
      (dolist (line (split-string text "\r?\n"))
        (let ((line (string-trim line)))
          (if (string-empty-p line)
              (setq blank t)
            (when (and blank lines) (push "" lines))
            (setq blank nil)
            (push line lines))))
      (when lines
        (mapconcat #'identity (nreverse lines) "\n")))))

(defun org-canvas--submissions-inline-text (html)
  "Return HTML, a rubric field, as one line of Org text; empty unless text.
Canvas escapes the field (`session&#39;s'), so it goes through the
converter as the rubrics pull's does, not straight into a cell."
  (org-canvas--html-to-org-inline (and (stringp html) html)))

(defun org-canvas--submissions-render-criteria (criteria)
  "Insert a table of rubric CRITERIA, a line each: criterion, points, ratings.
The compact form, `summary' in
`org-canvas-submissions-include-rubric-criteria'."
  (when (and criteria (> (length criteria) 0))
    (insert "\n| Criterion | Points | Ratings |\n|---+---+---|\n")
    (dolist (c (append criteria nil))
      (insert (format "| %s | %s | %s |\n"
                      (org-canvas--submissions-table-cell
                       (org-canvas--submissions-inline-text (alist-get 'description c)))
                      (org-canvas--submissions-format-number (or (alist-get 'points c) 0))
                      (mapconcat
                       (lambda (r)
                         (format "%s (%s)"
                                 (org-canvas--submissions-table-cell
                                  (org-canvas--submissions-inline-text (alist-get 'description r)))
                                 (org-canvas--submissions-format-number (or (alist-get 'points r) 0))))
                       (append (alist-get 'ratings c) nil) ", "))))
    (org-table-align)))

(defconst org-canvas--submissions-rubric-block-heading "* Rubric"
  "Heading the grading file's rubric is written under, before the students.
A level-1 heading without a USER_ID is no student: every loop over
the students checks the property, and the summary leaves it out.")

(defun org-canvas--submissions-render-criterion (criterion)
  "Insert CRITERION as a sub-heading in the shape of the rubrics file.
The title is its description and points, the body its long
description as prose, then a `| Rating | Points | Description |'
table, so the text that separates one rating from the next is in the
file (issue #265)."
  (insert (format "\n** %s (%s)\n"
                  (org-canvas--submissions-inline-text (alist-get 'description criterion))
                  (org-canvas--submissions-format-number (or (alist-get 'points criterion) 0))))
  (when-let* ((long (org-canvas--submissions-body-text
                     (org-canvas--alist-get-non-null 'long_description criterion))))
    (insert long "\n"))
  (insert "| Rating | Points | Description |\n|---+---+---|\n")
  (dolist (r (append (alist-get 'ratings criterion) nil))
    (insert (format "| %s | %s | %s |\n"
                    (org-canvas--submissions-table-cell
                     (org-canvas--submissions-inline-text (alist-get 'description r)))
                    (org-canvas--submissions-format-number (or (alist-get 'points r) 0))
                    (org-canvas--submissions-table-cell
                     (org-canvas--submissions-inline-text (alist-get 'long_description r))))))
  (save-excursion (forward-line -1) (org-table-align)))

(defun org-canvas--submissions-render-rubric-block (criteria)
  "Insert the rubric of CRITERIA in full: a heading with one sub-heading each."
  (when (and criteria (> (length criteria) 0))
    (insert "\n" org-canvas--submissions-rubric-block-heading "\n")
    (dolist (c (append criteria nil))
      (org-canvas--submissions-render-criterion c))))

(defun org-canvas--submissions-render-rubric-header (assignment)
  "Insert ASSIGNMENT's Rubric: line and its criteria, in the form configured.
Nothing is inserted when no rubric is attached; the criteria follow
`org-canvas-submissions-include-rubric-criteria'."
  (let ((rubric (org-canvas--submissions-rubric-settings assignment)))
    (when rubric
      (insert (org-canvas--submissions-rubric-line (car rubric)) "\n")
      (pcase org-canvas-submissions-include-rubric-criteria
        ('nil nil)
        ('summary (org-canvas--submissions-render-criteria (alist-get 'rubric assignment)))
        (_ (org-canvas--submissions-render-rubric-block (alist-get 'rubric assignment)))))))

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

(defun org-canvas--submissions-local-attachment (assignment-name student-name filename
                                                                 &optional attachment)
  "Return FILENAME's link path, relative to the grading file, if downloaded.
Nil otherwise.  ASSIGNMENT-NAME and STUDENT-NAME locate the download
directory.  With ATTACHMENT, the Canvas attachment an entry lists, a
copy that is not its download is nil too
\(`org-canvas--submissions-stale-copy-p')."
  (when (and assignment-name student-name filename)
    (let ((path (expand-file-name
                 filename (org-canvas--submissions-attachment-dir assignment-name student-name))))
      (when (and (file-exists-p path)
                 (not (org-canvas--submissions-stale-copy-p path attachment)))
        (file-relative-name path (org-canvas--submissions-dir))))))

(defun org-canvas--submissions-stale-copy-p (path attachment)
  "Return non-nil when the file at PATH is not the download of ATTACHMENT.
That is when Canvas gives ATTACHMENT a size and the file's differs,
or the file was written before Canvas received ATTACHMENT: a
resubmission under the same name, whose earlier copy would otherwise
be linked and graded as the new one (issue #457)."
  (let ((size (alist-get 'size attachment))
        (created (org-canvas--alist-get-non-null 'created_at attachment))
        (attributes (file-attributes path)))
    (or (and (numberp size) (/= size (file-attribute-size attributes)))
        (and (stringp created)
             (time-less-p (file-attribute-modification-time attributes)
                          (date-to-time created))))))

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
header; a refresh needs it because only the pull had the object.  A
failed read says so with one warning — a swallow here rendered the
grading file without its rubric header and left nothing to debug
\(#435)."
  (condition-case err
      (org-canvas-api-request
       'GET (org-canvas-api-course-endpoint "assignments/%s" assignment-id))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Could not read assignment %s (%s); the grading file renders without its rubric header"
       assignment-id (error-message-string err))
     nil)))

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

(provide 'org-canvas-submissions)
;;; org-canvas-submissions.el ends here
