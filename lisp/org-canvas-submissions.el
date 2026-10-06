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
;; c  post a comment on the student at point
;; S  push grade changes (and new saved comments)
;; B  read the comment bank into the Comment Bank heading
;; x  delete the saved comment at point from the bank
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
        org-canvas--submissions-reports-query
        (list (cons 'assignmentId (format "%s" assignment-id)))
        '(assignment submissionsConnection)
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
score differ from the one entered.  DAYS_LATE, rounded up, appears on a
late submission.  LATE_STATUS and its baseline CANVAS_LATE_STATUS
appear when Canvas holds a late policy status for the submission (see
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

;;;; Late Status (issue #352)

;; A submission's late policy status — late, missing, extended or none,
;; or nothing when Canvas decides from the due date — is what SpeedGrader
;; sets when a grader marks one student late, missing or extended.  The
;; pull writes it as LATE_STATUS with a CANVAS_LATE_STATUS baseline, the
;; way SCORE has CANVAS_SCORE; the push sends a LATE_STATUS that differs
;; from its baseline through GraphQL's `updateSubmissionGradeStatus'
;; (REST reads the status as `late_policy_status', so no read is added),
;; and records what Canvas answers it stored.

(defconst org-canvas--submissions-late-statuses
  '("late" "missing" "extended" "none")
  "The LATE_STATUS values a push sends, Canvas's `LatePolicyStatusType'.
A value Canvas returns that is not listed is written as it comes and
compared as it is, so it never stops a pull; only a typed one is
refused, before anything is sent.")

(defconst org-canvas--submissions-late-status-mutation
  "mutation ($submissionId: ID!, $status: String!) { updateSubmissionGradeStatus(input: {submissionId: $submissionId, latePolicyStatus: $status}) { submission { _id latePolicyStatus secondsLate } errors { attribute message } } }"
  "The GraphQL mutation that sets one submission's late policy status.
The reply carries the status Canvas stored and the lateness it now
counts, which the push records.  Checked against the Canvas schema by
the GraphQL contract test (issue #269), which names it by this
symbol.")

(defun org-canvas--submissions-late-status (submission)
  "Return SUBMISSION's late policy status as a string, or nil when unset."
  (let ((status (org-canvas--alist-get-non-null 'late_policy_status submission)))
    (and (stringp status) (not (string-empty-p status)) status)))

(defun org-canvas--submissions-insert-late-status (submission)
  "Insert SUBMISSION's LATE_STATUS and CANVAS_LATE_STATUS lines, when set.
Nothing is inserted for a submission whose status Canvas derives from
the due date alone; a grader who wants to set one adds LATE_STATUS."
  (when-let* ((status (org-canvas--submissions-late-status submission)))
    (insert (format ":LATE_STATUS: %s\n:CANVAS_LATE_STATUS: %s\n" status status))))

(defun org-canvas--submissions-typed-late-status ()
  "Return the LATE_STATUS of the entry at point, trimmed and downcased, or nil.
Nil too when the property is absent or blank."
  (let ((typed (org-entry-get (point) "LATE_STATUS")))
    (when typed
      (let ((status (downcase (string-trim typed))))
        (and (not (string-empty-p status)) status)))))

(defun org-canvas--submissions-late-status-change-at-point (name)
  "Return the late status edit of the entry at point as a plist, or nil.
NAME is the student's, for messages.  Nil when LATE_STATUS is blank or
matches CANVAS_LATE_STATUS; an emptied LATE_STATUS is not a change,
since the way back to no status is to type none.  The plist carries
:late-status, :old-late-status and :submission-id.  A value Canvas
does not take, or a heading with no SUBMISSION_ID to address, is a
`user-error', so nothing is sent for anyone until it is fixed."
  (let ((new (org-canvas--submissions-typed-late-status))
        (old (org-entry-get (point) "CANVAS_LATE_STATUS"))
        (submission-id (org-entry-get (point) "SUBMISSION_ID")))
    (when (and new (not (equal new old)))
      (unless (member new org-canvas--submissions-late-statuses)
        (user-error "%s: LATE_STATUS %s is not one of %s" name new
                    (string-join org-canvas--submissions-late-statuses ", ")))
      (unless submission-id
        (user-error "%s: no SUBMISSION_ID to set the late status on; pull again" name))
      (list :late-status new :old-late-status old :submission-id submission-id))))

(defun org-canvas--submissions-late-status-stored (data)
  "Return what Canvas stored, from DATA, the late status mutation's reply.
The value is (:status STATUS :seconds SECONDS), STATUS nil when Canvas
holds none, or nil when the reply names no submission.  Errors the
payload carries signal `org-canvas-api-error' naming them."
  (let* ((payload (alist-get 'updateSubmissionGradeStatus data))
         (errors (org-canvas--alist-get-non-null 'errors payload))
         (submission (org-canvas--alist-get-non-null 'submission payload)))
    (when (and errors (> (length errors) 0))
      (org-canvas--signal 'org-canvas-api-error
        "updateSubmissionGradeStatus: %s" (org-canvas--graphql-errors-message errors)))
    (when (consp submission)
      (list :status (org-canvas--alist-get-non-null 'latePolicyStatus submission)
            :seconds (org-canvas--alist-get-non-null 'secondsLate submission)))))

(defun org-canvas--submissions-push-late-status (change)
  "Send CHANGE's late status; return what Canvas stored, or `dry-run'.
The stored value is that of `org-canvas--submissions-late-status-stored'."
  (let ((data (org-canvas--graphql-mutate
               (format "set the late status of %s to %s"
                       (plist-get change :name) (plist-get change :late-status))
               org-canvas--submissions-late-status-mutation
               (list (cons 'submissionId (format "%s" (plist-get change :submission-id)))
                     (cons 'status (plist-get change :late-status))))))
    (if (org-canvas--dry-run-response-p data)
        'dry-run
      (org-canvas--submissions-late-status-stored data))))

(defun org-canvas--submissions-send-late-statuses (diffs)
  "Send the late status of each of DIFFS that changed one.
Return (SENT . FAILED): SENT an alist of (USER-ID . STORED), STORED
what Canvas answered it stored (see
`org-canvas--submissions-push-late-status'); FAILED the names of the
students whose request failed, each one warning in the log, so one
refusal leaves the others sent and its heading still a change."
  (let ((sent nil) (failed nil))
    (dolist (change diffs)
      (when (plist-get change :late-status)
        (condition-case err
            (push (cons (plist-get change :user-id)
                        (org-canvas--submissions-push-late-status change))
                  sent)
          (error
           (org-canvas--log-warning org-canvas--logger
             "[Submissions] Late status for %s not set: %s"
             (plist-get change :name) (error-message-string err))
           (push (plist-get change :name) failed)))))
    (cons (nreverse sent) (nreverse failed))))

(defun org-canvas--submissions-record-days-late (seconds)
  "Rewrite DAYS_LATE of the entry at point from SECONDS, Canvas's lateness.
Only a submitted row records lateness, as the pull does
\(`org-canvas--submissions-days-late'); a non-number leaves it alone."
  (when (and (numberp seconds) (org-entry-get (point) "SUBMITTED_AT"))
    (if (> seconds 0)
        (org-entry-put (point) "DAYS_LATE" (format "%d" (ceiling seconds 86400)))
      (org-entry-delete (point) "DAYS_LATE"))))

(defun org-canvas--submissions-record-late-status (change stored)
  "Make the late status of CHANGE, as Canvas STORED it, the entry's baseline.
STORED is what `org-canvas--submissions-push-late-status' returned;
without it the status sent is taken as stored.  When Canvas stored
something else than was sent, LATE_STATUS follows it too and the log
says so, since the file is to show what Canvas holds (issue #349's
rule)."
  (let* ((sent (plist-get change :late-status))
         (status (if stored (plist-get stored :status) sent)))
    (if status
        (progn (org-entry-put (point) "LATE_STATUS" status)
               (org-entry-put (point) "CANVAS_LATE_STATUS" status))
      (org-entry-delete (point) "LATE_STATUS")
      (org-entry-delete (point) "CANVAS_LATE_STATUS"))
    (unless (equal status sent)
      (org-canvas--log-warning org-canvas--logger
        "[Submissions] %s: Canvas stored late status %s, not the %s sent"
        (plist-get change :name) (or status "no status") sent))
    (org-canvas--submissions-record-days-late (plist-get stored :seconds))))

(defun org-canvas--submissions-late-carryover ()
  "Return the typed LATE_STATUS of the entry at point with its baseline, or nil.
The value is (:typed STATUS :baseline CANVAS-LATE-STATUS), when the
two differ; nil when LATE_STATUS is blank or what Canvas holds."
  (let ((typed (org-canvas--submissions-typed-late-status))
        (baseline (org-entry-get (point) "CANVAS_LATE_STATUS")))
    (when (and typed (not (equal typed baseline)))
      (list :typed typed :baseline baseline))))

(defun org-canvas--submissions-restore-late-status (carry)
  "Put CARRY, a `org-canvas--submissions-late-carryover' value, back at point.
Nothing is written when Canvas now holds the typed status.  When
Canvas's status is no longer the baseline it was typed against, the
heading is marked CONFLICT, as a typed score is (issue #281)."
  (let ((fresh (org-entry-get (point) "CANVAS_LATE_STATUS"))
        (typed (plist-get carry :typed)))
    (unless (equal fresh typed)
      (org-entry-put (point) "LATE_STATUS" typed)
      (unless (equal fresh (plist-get carry :baseline))
        (org-entry-put (point) "CONFLICT"
                       (format "late status: Canvas has %s" (or fresh "no status")))))))

(defun org-canvas--submissions-describe-late (change)
  "Return the late status note for the line of CHANGE, or an empty string."
  (if (plist-get change :late-status)
      (format " (late status: %s → %s)"
              (or (plist-get change :old-late-status) "no status")
              (plist-get change :late-status))
    ""))

;;;; Comment Bank (issue #352)

;; SpeedGrader's comment library is a list of saved comments a grader
;; picks from instead of typing.  Canvas keeps it per user and course,
;; readable as `User.commentBankItemsConnection' and written by the
;; create, update and delete CommentBankItem mutations.  A grading file
;; keeps it under a level-1 `* Comment Bank' heading, one list item per
;; saved comment: `- 4821 :: text' for one on Canvas, labelled with its
;; id, and `- text' for one to create.  The heading's
;; CANVAS_COMMENT_BANK property holds each labelled item's digest as
;; last read or written, the baseline that tells an item edited here
;; from one edited in SpeedGrader.  The section is the grader's: a
;; refresh carries it over as it stands, S sends what is new or edited
;; after reading the bank (an item whose text the bank already holds
;; is labelled, not created twice), B reads the bank in, and nothing
;; is ever deleted on Canvas but by x on the item.

(defconst org-canvas--submissions-bank-heading "* Comment Bank"
  "Heading the grading file's saved comments live under.
A level-1 heading with no USER_ID, which is no student.")

(defconst org-canvas--submissions-bank-query
  "query ($userId: ID!, $courseId: ID!, $cursor: String) { user(id: $userId) { commentBankItemsConnection(courseId: $courseId, first: 100, after: $cursor) { pageInfo { hasNextPage endCursor } nodes { _id comment } } } }"
  "The GraphQL query that reads the grader's saved comments in the course.
One page per request.  Checked against the Canvas schema by the
GraphQL contract test (issue #269), which names it by this symbol.")

(defconst org-canvas--submissions-bank-create-mutation
  "mutation ($courseId: ID!, $assignmentId: ID, $comment: String!) { createCommentBankItem(input: {courseId: $courseId, assignmentId: $assignmentId, comment: $comment}) { commentBankItem { _id comment } errors { attribute message } } }"
  "The GraphQL mutation that saves a comment in the grader's library.
Checked by the GraphQL contract test, which names it by this symbol.")

(defconst org-canvas--submissions-bank-update-mutation
  "mutation ($id: ID!, $comment: String!) { updateCommentBankItem(input: {id: $id, comment: $comment}) { commentBankItem { _id comment } errors { attribute message } } }"
  "The GraphQL mutation that rewrites a saved comment.
Checked by the GraphQL contract test, which names it by this symbol.")

(defconst org-canvas--submissions-bank-delete-mutation
  "mutation ($id: ID!) { deleteCommentBankItem(input: {id: $id}) { commentBankItemId errors { attribute message } } }"
  "The GraphQL mutation that removes a saved comment from the library.
Sent only by `org-canvas-submissions-delete-comment-bank-item'.
Checked by the GraphQL contract test, which names it by this symbol.")

(defconst org-canvas--submissions-bank-item-regexp
  "^-[ \t]+\\(?:\\([0-9]+\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?\\(.*\\)$"
  "Match the first line of a Comment Bank item.
Group 1 is the saved comment's id when the item is labelled with one,
group 2 the first line of its text.  Its other lines follow, indented,
up to the next item.")

(defun org-canvas--submissions-bank-region ()
  "Return (START . END) of the Comment Bank section, heading included, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^" (regexp-quote org-canvas--submissions-bank-heading) "[ \t]*$")
             nil t)
        (let ((start (line-beginning-position)))
          (cons start (if (re-search-forward "^\\* " nil t)
                          (line-beginning-position)
                        (point-max))))))))

(defun org-canvas--submissions-bank-digest (text)
  "Return the short digest of a saved comment's TEXT."
  (substring (sha1 (or text "")) 0 12))

(defun org-canvas--submissions-bank-item-text (start end)
  "Return the text of the Comment Bank item between START and END.
START is where its first line's text begins.  Org comment lines are
dropped and the rest normalized as a Rubric comment is
\(`org-canvas--submissions-comment-text')."
  (org-canvas--submissions-comment-text
   (mapconcat #'identity
              (seq-remove (lambda (l) (string-match-p "\\`[ \t]*#\\(?: \\|\\'\\)" l))
                          (split-string (buffer-substring-no-properties start end) "\n"))
              "\n")))

(defun org-canvas--submissions-bank-items ()
  "Return the Comment Bank items as plists, in order, or nil without any.
Each has :id (a string, nil for an item to create), :text, :start (the
item's first character) and :end (the end of its last non-blank line).
An item with no text is left out."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (save-excursion
      (goto-char (car region))
      (forward-line 1)
      (let ((items nil))
        (while (re-search-forward org-canvas--submissions-bank-item-regexp (cdr region) t)
          (let* ((id (match-string-no-properties 1))
                 (start (match-beginning 0))
                 (text-start (match-beginning 2))
                 (next (save-excursion
                         (forward-line 1)
                         (if (re-search-forward "^-[ \t]" (cdr region) t)
                             (match-beginning 0)
                           (cdr region))))
                 (end (save-excursion (goto-char next)
                                      (skip-chars-backward " \t\n" start)
                                      (point)))
                 (text (org-canvas--submissions-bank-item-text text-start (max text-start end))))
            (when text
              (push (list :id id :text text :start start :end (max text-start end)) items))
            (goto-char next)))
        (nreverse items)))))

(defun org-canvas--submissions-bank-baseline ()
  "Return the CANVAS_COMMENT_BANK baseline as an alist of (ID . DIGEST)."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (org-canvas--submissions-parse-digests
     (save-excursion (goto-char (car region))
                     (org-entry-get (point) "CANVAS_COMMENT_BANK")))))

(defun org-canvas--submissions-bank-set-baseline (baseline)
  "Write BASELINE, an alist of (ID . DIGEST), as CANVAS_COMMENT_BANK.
Only the ids an item still carries are kept; none leaves no property."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (let* ((ids (delq nil (mapcar (lambda (i) (plist-get i :id))
                                  (org-canvas--submissions-bank-items))))
           (kept (seq-filter (lambda (pair) (member (car pair) ids)) baseline))
           (value (mapconcat (lambda (pair) (format "%s=%s" (car pair) (cdr pair)))
                             (sort (copy-sequence kept)
                                   (lambda (a b) (< (string-to-number (car a))
                                                    (string-to-number (car b)))))
                             " ")))
      (save-excursion
        (goto-char (car region))
        (if (string-empty-p value)
            (org-entry-delete (point) "CANVAS_COMMENT_BANK")
          (org-entry-put (point) "CANVAS_COMMENT_BANK" value))))))

(defun org-canvas--submissions-bank-pending-p (item baseline)
  "Return non-nil when ITEM is still to send, against BASELINE.
An item without an id is to create; a labelled one whose text no
longer digests to its BASELINE entry, or has none, is to compare."
  (or (null (plist-get item :id))
      (not (equal (cdr (assoc (plist-get item :id) baseline))
                  (org-canvas--submissions-bank-digest (plist-get item :text))))))

(defun org-canvas--submissions-bank-pending ()
  "Return the Comment Bank items a push has to send, in order."
  (let ((baseline (org-canvas--submissions-bank-baseline)))
    (seq-filter (lambda (item) (org-canvas--submissions-bank-pending-p item baseline))
                (org-canvas--submissions-bank-items))))

(defun org-canvas--submissions-self-id ()
  "Return the token owner's Canvas user id as a string.
The comment bank is theirs: Canvas keeps it per user."
  (let ((me (org-canvas-api-request
             'GET (format "%s/api/v1/users/self"
                          (replace-regexp-in-string "/+\\'" "" org-canvas-base-url)))))
    (or (and (alist-get 'id me) (format "%s" (alist-get 'id me)))
        (org-canvas--signal 'org-canvas-api-error "users/self answered no id"))))

(defun org-canvas--submissions-fetch-bank ()
  "Return the grader's saved comments in the course, or nil when unreadable.
The value is a list of (ID . TEXT), in the order Canvas answers, the
text normalized as an item's is.  A failed read is one warning and
nil; an empty bank is `empty', so the two are never confused."
  (condition-case err
      (let* ((user-id (org-canvas--submissions-self-id))
             (bank (org-canvas--graphql-walk-pages
                    org-canvas--submissions-bank-query
                    (list (cons 'userId user-id)
                          (cons 'courseId (format "%s" org-canvas-course-id)))
                    '(user commentBankItemsConnection)
                    (lambda (node map)
                      (let ((id (org-canvas--alist-get-non-null '_id node))
                            (text (org-canvas--submissions-comment-text
                                   (org-canvas--alist-get-non-null 'comment node))))
                        (when (and id text)
                          (puthash (format "%s" id) text map))))))
             (items nil))
        (maphash (lambda (id text) (push (cons id text) items)) bank)
        (or (nreverse items) 'empty))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Could not read the comment bank (%s); nothing sent to it"
       (error-message-string err))
     nil)))

(defun org-canvas--submissions-bank-reply-item (data field)
  "Return the saved comment FIELD of DATA carries, as (ID . TEXT).
DATA is a create or update mutation's reply.  Errors the payload
carries signal `org-canvas-api-error' naming them; a reply with no
item is an error too, since there is no id to label the item with."
  (let* ((payload (alist-get field data))
         (errors (org-canvas--alist-get-non-null 'errors payload))
         (item (org-canvas--alist-get-non-null 'commentBankItem payload)))
    (when (and errors (> (length errors) 0))
      (org-canvas--signal 'org-canvas-api-error
        "%s: %s" field (org-canvas--graphql-errors-message errors)))
    (unless (and (consp item) (org-canvas--alist-get-non-null '_id item))
      (org-canvas--signal 'org-canvas-api-error "%s answered no saved comment" field))
    (cons (format "%s" (alist-get '_id item))
          (org-canvas--submissions-comment-text (alist-get 'comment item)))))

(defun org-canvas--submissions-bank-create (assignment-id text)
  "Save TEXT in the bank for ASSIGNMENT-ID; return (ID . TEXT) or `dry-run'."
  (let ((data (org-canvas--graphql-mutate
               (format "save a comment in the comment bank: %s" (truncate-string-to-width text 40))
               org-canvas--submissions-bank-create-mutation
               (list (cons 'courseId (format "%s" org-canvas-course-id))
                     (cons 'assignmentId (format "%s" assignment-id))
                     (cons 'comment text)))))
    (if (org-canvas--dry-run-response-p data)
        'dry-run
      (org-canvas--submissions-bank-reply-item data 'createCommentBankItem))))

(defun org-canvas--submissions-bank-update (id text)
  "Rewrite the saved comment ID as TEXT; return (ID . TEXT) or `dry-run'."
  (let ((data (org-canvas--graphql-mutate
               (format "rewrite saved comment %s" id)
               org-canvas--submissions-bank-update-mutation
               (list (cons 'id id) (cons 'comment text)))))
    (if (org-canvas--dry-run-response-p data)
        'dry-run
      (org-canvas--submissions-bank-reply-item data 'updateCommentBankItem))))

(defun org-canvas--submissions-bank-label (item id)
  "Label ITEM, an item without an id, with ID in the buffer."
  (save-excursion
    (goto-char (plist-get item :start))
    (when (looking-at "-[ \t]+")
      (replace-match (format "- %s :: " id) t t))))

(defun org-canvas--submissions-bank-send-new (item live assignment-id)
  "Send ITEM, which has no id, unless its text is in LIVE already.
LIVE is the bank as `org-canvas--submissions-fetch-bank' read it;
ASSIGNMENT-ID names the column a created comment is saved for.
Return (KIND ID . TEXT), KIND `adopted' or `created', or nil under a
dry run."
  (let ((twin (rassoc (plist-get item :text) live)))
    (if twin
        (cons 'adopted twin)
      (let ((made (org-canvas--submissions-bank-create assignment-id (plist-get item :text))))
        (unless (eq made 'dry-run)
          (cons 'created made))))))

(defun org-canvas--submissions-bank-send-edit (item live baseline)
  "Send ITEM, a labelled item, when it was edited here and not on Canvas.
LIVE and BASELINE are the bank as read and the file's baseline.
Return (KIND ID . TEXT): `same' when Canvas holds the text already,
`updated' when it was rewritten, `gone' when the bank no longer has
the id, `conflict' when Canvas's text moved off the baseline too; nil
under a dry run."
  (let* ((id (plist-get item :id))
         (remote (cdr (assoc id live)))
         (base (cdr (assoc id baseline))))
    (cond ((null remote) (cons 'gone (cons id nil)))
          ((equal remote (plist-get item :text)) (cons 'same (cons id remote)))
          ((and base (not (equal base (org-canvas--submissions-bank-digest remote))))
           (cons 'conflict (cons id remote)))
          (t (let ((made (org-canvas--submissions-bank-update id (plist-get item :text))))
               (unless (eq made 'dry-run)
                 (cons 'updated made)))))))

(defun org-canvas--submissions-bank-send-item (item live baseline assignment-id)
  "Send one pending ITEM; return its outcome, or (failed nil . TEXT).
LIVE, BASELINE and ASSIGNMENT-ID as the two senders take them.  A
failed request is one warning, and the item stays pending."
  (condition-case err
      (if (plist-get item :id)
          (org-canvas--submissions-bank-send-edit item live baseline)
        (org-canvas--submissions-bank-send-new item live assignment-id))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Saved comment not sent (%s): %s"
       (truncate-string-to-width (plist-get item :text) 40)
       (error-message-string err))
     (cons 'failed (cons nil (plist-get item :text))))))

(defun org-canvas--submissions-bank-warn (outcome)
  "Log the warning for OUTCOME of a labelled item, if it needs one."
  (pcase (car outcome)
    ('gone (org-canvas--log-warning org-canvas--logger
             "[Submissions] Saved comment %s is no longer in the bank; remove its label to create it again"
             (cadr outcome)))
    ('conflict (org-canvas--log-warning org-canvas--logger
                 "[Submissions] Saved comment %s was edited here and in SpeedGrader; B reads Canvas's text in"
                 (cadr outcome)))))

(defconst org-canvas--submissions-bank-count-keys
  '((created . :created) (adopted . :adopted) (updated . :updated)
    (gone . :skipped) (conflict . :skipped) (failed . :failed))
  "The count each outcome of a Comment Bank item adds to.
An item Canvas already held as typed (`same') counts nowhere.")

(defun org-canvas--submissions-bank-record-one (item outcome baseline counts)
  "Record OUTCOME of ITEM in the buffer, BASELINE and COUNTS.
Return BASELINE, which may have gained an entry; COUNTS is changed in
place.  Under `org-canvas--dry-run' the buffer is left alone (issue
#442)."
  (let ((kind (car outcome))
        (key (cdr (assq (car outcome) org-canvas--submissions-bank-count-keys))))
    (org-canvas--submissions-bank-warn outcome)
    (when key
      (plist-put counts key (1+ (plist-get counts key))))
    (when (and (memq kind '(created adopted)) (not org-canvas--dry-run))
      (org-canvas--submissions-bank-label item (cadr outcome)))
    (when (memq kind '(created adopted updated same))
      (setf (alist-get (cadr outcome) baseline nil nil #'equal)
            (org-canvas--submissions-bank-digest (plist-get item :text))))
    baseline))

(defun org-canvas--submissions-bank-record (pending outcomes)
  "Record OUTCOMES of PENDING items in the buffer; return the counts.
Items are labelled and the baseline rewritten, last item first so
the positions of the others hold; an outcome of nil (a dry run)
records nothing.  The counts are a plist of :created, :adopted,
:updated, :skipped (gone from Canvas or edited there too) and
:failed."
  (let ((baseline (org-canvas--submissions-bank-baseline))
        (counts (list :created 0 :adopted 0 :updated 0 :skipped 0 :failed 0)))
    (cl-mapc (lambda (item outcome)
               (when outcome
                 (setq baseline (org-canvas--submissions-bank-record-one
                                 item outcome baseline counts))))
             (reverse pending) (reverse outcomes))
    (unless org-canvas--dry-run
      (org-canvas--submissions-bank-set-baseline baseline))
    counts))

(defun org-canvas--submissions-push-bank (assignment-id)
  "Send the Comment Bank's new and edited items for ASSIGNMENT-ID.
The bank is read first, so an item whose text it already holds is
labelled with that id rather than created twice (the duplicate
guard's rule, Hard Rule 20), and an item edited in SpeedGrader since
it was last read is not overwritten.  Nothing is deleted.  Return the
counts of `org-canvas--submissions-bank-record', or nil when nothing
was pending or the bank could not be read."
  (when-let* ((pending (org-canvas--submissions-bank-pending))
              (live (org-canvas--submissions-fetch-bank)))
    (let* ((live (if (eq live 'empty) nil live))
           (baseline (org-canvas--submissions-bank-baseline))
           (outcomes (mapcar (lambda (item)
                               (org-canvas--submissions-bank-send-item
                                item live baseline assignment-id))
                             pending)))
      (org-canvas--submissions-bank-record pending outcomes))))

(defun org-canvas--submissions-describe-bank (counts &optional pending)
  "Return the push message note for the bank COUNTS, or \"\".
With PENDING items and no COUNTS the bank could not be read, and the
note says so."
  (if (null counts)
      (if pending "; comment bank not read (see the log)" "")
    (let ((parts (delq nil (list (org-canvas--submissions-count-part counts :created "saved")
                                 (org-canvas--submissions-count-part counts :adopted "already in the bank")
                                 (org-canvas--submissions-count-part counts :updated "rewritten")
                                 (org-canvas--submissions-count-part counts :skipped "skipped")
                                 (org-canvas--submissions-count-part counts :failed "failed")))))
      (if parts
          (format "; comment bank: %s" (string-join parts ", "))
        ""))))

(defun org-canvas--submissions-count-part (counts key label)
  "Return \"N LABEL\" for KEY of COUNTS, or nil when it is 0."
  (let ((n (plist-get counts key)))
    (and n (> n 0) (format "%d %s" n label))))

(defun org-canvas--submissions-render-bank-heading ()
  "Insert the Comment Bank heading with its template, when enabled."
  (when org-canvas-submissions-comment-bank-template
    (insert "\n" org-canvas--submissions-bank-heading "\n"
            org-canvas-submissions-comment-bank-template "\n")))

(defun org-canvas--submissions-bank-carryover ()
  "Return the Comment Bank section as it stands, heading included, or nil."
  (when-let* ((region (org-canvas--submissions-bank-region)))
    (let ((text (string-trim-right
                 (buffer-substring-no-properties (car region) (cdr region)))))
      (concat text "\n"))))

(defun org-canvas--submissions-restore-bank (text)
  "Put TEXT, a carried Comment Bank section, back in the buffer.
It replaces the section a render wrote, or goes before the first
student when there is none."
  (save-excursion
    (let ((region (org-canvas--submissions-bank-region)))
      (if region
          (progn (delete-region (car region) (cdr region))
                 (goto-char (car region)))
        (goto-char (point-min))
        (if (re-search-forward "^[ \t]*:USER_ID:" nil t)
            (org-back-to-heading t)
          (goto-char (point-max))))
      (insert text)
      (when (looking-at "^\\*") (insert "\n")))))

(defun org-canvas--submissions-bank-merge (live)
  "Bring LIVE, the bank as read, into the Comment Bank section.
A labelled item unedited here takes Canvas's text; an item Canvas
holds that the section lacks is added, labelled, at the end.  Items
edited here, and those Canvas no longer holds, are left as they are.
Return (:added N :rewritten N)."
  (let* ((baseline (org-canvas--submissions-bank-baseline))
         (items (org-canvas--submissions-bank-items))
         (added 0) (rewritten 0))
    (dolist (item (reverse items))
      (let ((remote (cdr (assoc (plist-get item :id) live))))
        (when (and remote
                   (not (equal remote (plist-get item :text)))
                   (not (org-canvas--submissions-bank-pending-p item baseline)))
          (save-excursion
            (delete-region (plist-get item :start) (plist-get item :end))
            (goto-char (plist-get item :start))
            (insert (org-canvas--submissions-item (plist-get item :id) remote)))
          (cl-incf rewritten))))
    (let ((known (mapcar (lambda (i) (plist-get i :id)) items))
          (region (org-canvas--submissions-bank-region)))
      (save-excursion
        (goto-char (cdr region))
        (skip-chars-backward " \t\n" (car region))
        (dolist (pair live)
          (unless (member (car pair) known)
            (insert "\n" (org-canvas--submissions-item (car pair) (cdr pair)))
            (cl-incf added)))))
    (dolist (pair live)
      (let ((item (cl-find (car pair) (org-canvas--submissions-bank-items)
                           :key (lambda (i) (plist-get i :id)) :test #'equal)))
        (when (and item (equal (plist-get item :text) (cdr pair)))
          (setf (alist-get (car pair) baseline nil nil #'equal)
                (org-canvas--submissions-bank-digest (cdr pair))))))
    (org-canvas--submissions-bank-set-baseline baseline)
    (list :added added :rewritten rewritten)))

(defun org-canvas--submissions-ensure-bank-section ()
  "Return the Comment Bank region, writing an empty section when missing."
  (or (org-canvas--submissions-bank-region)
      (progn
        (org-canvas--submissions-restore-bank
         (concat org-canvas--submissions-bank-heading "\n"
                 (if org-canvas-submissions-comment-bank-template
                     (concat org-canvas-submissions-comment-bank-template "\n")
                   "")))
        (org-canvas--submissions-bank-region))))

;;;###autoload
(defun org-canvas-submissions-pull-comment-bank ()
  "Read the grader's comment bank into this grading file's Comment Bank.
Every saved comment of the course's library comes in as an item
labelled with its id; one already there takes Canvas's text unless it
was edited here since it was last read.  Nothing is sent and nothing
is removed from the section."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (let ((live (org-canvas--submissions-fetch-bank)))
    (unless live
      (user-error "Could not read the comment bank; see the log"))
    (let ((inhibit-read-only t))
      (org-canvas--submissions-ensure-bank-section)
      (let ((counts (org-canvas--submissions-bank-merge (if (eq live 'empty) nil live))))
        (when buffer-file-name (save-buffer))
        (message "Comment bank: %d added, %d rewritten from Canvas"
                 (plist-get counts :added) (plist-get counts :rewritten))))))

(defun org-canvas--submissions-bank-item-at-point ()
  "Return the Comment Bank item point is on, or nil."
  (let ((pos (point)))
    (cl-find-if (lambda (item)
                  (and (<= (save-excursion (goto-char (plist-get item :start))
                                           (line-beginning-position))
                           pos)
                       (<= pos (save-excursion (goto-char (plist-get item :end))
                                               (line-end-position)))))
                (org-canvas--submissions-bank-items))))

(defun org-canvas--submissions-bank-delete (id)
  "Delete the saved comment ID from the bank; return non-nil when sent."
  (let* ((data (org-canvas--graphql-mutate
                (format "delete saved comment %s" id)
                org-canvas--submissions-bank-delete-mutation
                (list (cons 'id id))))
         (payload (and (not (org-canvas--dry-run-response-p data))
                       (alist-get 'deleteCommentBankItem data)))
         (errors (org-canvas--alist-get-non-null 'errors payload)))
    (when (and errors (> (length errors) 0))
      (org-canvas--signal 'org-canvas-api-error
        "deleteCommentBankItem: %s" (org-canvas--graphql-errors-message errors)))
    (and payload t)))

;;;###autoload
(defun org-canvas-submissions-delete-comment-bank-item ()
  "Delete the saved comment at point from the comment bank, after asking.
The item must carry the id of a comment on Canvas; it is removed from
the Comment Bank section once Canvas has deleted it.  The only way
org-canvas deletes a saved comment: removing an item from the section
never does."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (let ((item (org-canvas--submissions-bank-item-at-point)))
    (unless (and item (plist-get item :id))
      (user-error "No saved comment with an id at point"))
    (when (y-or-n-p (format "Delete saved comment %s from the comment bank? "
                            (plist-get item :id)))
      (if (not (org-canvas--submissions-bank-delete (plist-get item :id)))
          (message "Dry run: saved comment %s left in place" (plist-get item :id))
        (let ((inhibit-read-only t)
              (baseline (org-canvas--submissions-bank-baseline)))
          (delete-region (plist-get item :start)
                         (save-excursion (goto-char (plist-get item :end))
                                         (min (point-max) (1+ (line-end-position)))))
          (org-canvas--submissions-bank-set-baseline baseline))
        (when buffer-file-name (save-buffer))
        (message "Saved comment %s deleted" (plist-get item :id))))))

;;;; Carry-over Across Pulls

(defun org-canvas--submissions-score-carryover ()
  "Return the typed SCORE of the entry at point with its baseline, or nil.
The value is (:typed SCORE :baseline CANVAS-SCORE): the score as typed
and the CANVAS_SCORE it was typed against, when the two differ; a 0
typed on a missing row that never had a CANVAS_SCORE counts, and so
does a clear word typed against a grade (issue #417), which is kept
until Canvas holds no grade.  Nil when the score is what Canvas holds,
or no SCORE is present at all, or a blank one, which leaves the grade
alone."
  (let ((typed (org-entry-get (point) "SCORE"))
        (baseline (org-entry-get (point) "CANVAS_SCORE")))
    (when (and typed
               (not (string-empty-p (string-trim typed)))
               (not (equal (org-canvas--submissions-parse-score typed)
                           (org-canvas--submissions-parse-score baseline))))
      (list :typed typed :baseline baseline))))

(defun org-canvas--submissions-restore-score (carry)
  "Put CARRY, a `org-canvas--submissions-score-carryover' value, back at point.
Nothing is written when Canvas now holds the typed score (the same
grade was pushed meanwhile).  When Canvas's score is no longer the
baseline the score was typed against — someone graded in SpeedGrader
since — the heading is marked CONFLICT the way a push does, since the
SCORE shown is the grader's and not what Canvas holds (issue #281)."
  (let ((fresh (org-entry-get (point) "CANVAS_SCORE"))
        (typed (plist-get carry :typed)))
    (unless (equal (org-canvas--submissions-parse-score fresh)
                   (org-canvas--submissions-parse-score typed))
      (org-entry-put (point) "SCORE" typed)
      (unless (equal fresh (plist-get carry :baseline))
        (org-entry-put (point) "CONFLICT"
                       (format "score: Canvas has %s" (or fresh "no grade")))))))

(defun org-canvas--submissions-collect-carryover ()
  "Return (user-id . (:notes TEXT :draft TEXT :rubric ROWS :score SCORE :late L)).
Read from the current buffer before a re-render replaces it, one
entry per student heading that carries any of them.  ROWS are the
unpushed Rubric rows, see
`org-canvas--submissions-rubric-carryover'; SCORE is the typed score
with its baseline, see `org-canvas--submissions-score-carryover'; L
the typed late status, see `org-canvas--submissions-late-carryover'."
  (let ((carry nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let ((user-id (org-entry-get (point) "USER_ID"))
              (notes (org-canvas--submissions-section-text org-canvas--submissions-notes-heading))
              (draft (org-canvas--submissions-section-text org-canvas--submissions-draft-heading))
              (rubric (org-canvas--submissions-rubric-carryover))
              (score (org-canvas--submissions-score-carryover))
              (late (org-canvas--submissions-late-carryover)))
          (when (and user-id (or notes draft rubric score late))
            (push (cons (string-to-number user-id)
                        (list :notes notes :draft draft :rubric rubric :score score
                              :late late))
                  carry)))
        (forward-line 1)))
    carry))

(defun org-canvas--submissions-restore-entry (carried)
  "Write CARRIED, one student's carry-over plist, back at point.
The score goes last, so a heading whose score and rubric both moved on
Canvas is marked for the score, the way the push's conflict check
orders them; the late status goes before the rubric, as the check
compares it after them."
  (let ((notes (plist-get carried :notes))
        (draft (plist-get carried :draft))
        (rubric (plist-get carried :rubric))
        (score (plist-get carried :score))
        (late (plist-get carried :late)))
    (when notes
      (org-canvas--submissions-set-section org-canvas--submissions-notes-heading
                                           org-canvas-submissions-notes-template notes))
    (when draft
      (org-canvas--submissions-set-section org-canvas--submissions-draft-heading
                                           org-canvas-submissions-comment-template draft))
    (when late
      (org-canvas--submissions-restore-late-status late))
    (when rubric
      (org-canvas--submissions-restore-rubric rubric))
    (when score
      (org-canvas--submissions-restore-score score))))

(defun org-canvas--submissions-restore-carryover (carry)
  "Write CARRY back under the students it names.
CARRY is the alist `org-canvas--submissions-collect-carryover' produced
before the buffer was re-rendered."
  (save-excursion
    (dolist (entry carry)
      (when (org-canvas--submissions-goto-user (car entry))
        (org-canvas--submissions-restore-entry (cdr entry))))))

;;;; What a Refresh Changed (issue #282)

;; A refresh is the grader's "what happened since I last looked".  The
;; state each heading recorded is read before the re-render, compared
;; with the submissions just fetched, and the differences are named:
;; in one message line as counts, in the log per student, and on the
;; heading where the score no longer describes the file.

(defun org-canvas--submissions-left-p ()
  "Return non-nil when the heading at point is a student who left the course."
  (equal (org-entry-get (point) "STATUS") "left"))

(defun org-canvas--submissions-state-at-point ()
  "Return what the heading at point recorded of its student, as a plist.
The keys are :name, :attempt (a number), :submitted-at, :score (the
CANVAS_SCORE baseline), :posted-at and :status, the property values
as strings, nil where the property is absent."
  (let ((attempt (org-entry-get (point) "ATTEMPT")))
    (list :name (org-get-heading t t t t)
          :attempt (and attempt (string-to-number attempt))
          :submitted-at (org-entry-get (point) "SUBMITTED_AT")
          :score (org-entry-get (point) "CANVAS_SCORE")
          :posted-at (org-entry-get (point) "POSTED_AT")
          :status (org-entry-get (point) "STATUS"))))

(defun org-canvas--submissions-collect-previous ()
  "Return what the grading file recorded before a re-render, or nil.
The value is (:pulled-at TIMESTAMP :students ALIST), ALIST mapping
each USER_ID to `org-canvas--submissions-state-at-point'; nil when the
buffer has no student heading yet, the first pull of a file."
  (let ((students nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (when-let* ((user-id (org-entry-get (point) "USER_ID")))
          (push (cons (string-to-number user-id) (org-canvas--submissions-state-at-point))
                students))
        (forward-line 1)))
    (when students
      (list :pulled-at (org-canvas--submissions-file-property "PULLED_AT")
            :students (nreverse students)))))

(defun org-canvas--submissions-state (submission)
  "Return SUBMISSION's state as the grading file would record it.
The keys are those of `org-canvas--submissions-state-at-point'."
  (list :name (org-canvas--submissions-user-sortable-name submission)
        :attempt (alist-get 'attempt submission)
        :submitted-at (org-canvas--alist-get-non-null 'submitted_at submission)
        :score (org-canvas--submissions-shown-score submission)
        :posted-at (org-canvas--alist-get-non-null 'posted_at submission)))

(defun org-canvas--submissions-resubmitted-p (old new)
  "Return non-nil when NEW's attempt is later than OLD's.
OLD and NEW are state plists; a student with no earlier attempt has
not resubmitted."
  (let ((before (plist-get old :attempt))
        (now (plist-get new :attempt)))
    (and (numberp before) (numberp now) (> now before))))

(defun org-canvas--submissions-classify-change (old new)
  "Return the kind of change from OLD to NEW for one student, or nil.
OLD is the heading's state before the refresh, nil for a student the
file did not have; NEW is the fetched submission's.  One kind per
student, the one the grader acts on first: `resubmitted' when a new
attempt arrived on a row that carried a grade, `new' when work
arrived where there was none (a first submission, a later attempt on
an ungraded row, a student the file did not have), `regraded' when
Canvas's score moved, `posted' when the grade became visible."
  (cond ((and old (org-canvas--submissions-resubmitted-p old new))
         (if (plist-get old :score) 'resubmitted 'new))
        ((and (plist-get new :submitted-at)
              (not (plist-get old :submitted-at)))
         'new)
        ((and old (not (equal (plist-get old :score) (plist-get new :score))))
         'regraded)
        ((and old (plist-get new :posted-at) (not (plist-get old :posted-at)))
         'posted)))

(defun org-canvas--submissions-changes-since (previous submissions carry)
  "Compare PREVIOUS, the file's recorded state, with the fetched SUBMISSIONS.
Return a plist of lists of (user-id . name): :new, :resubmitted,
:regraded and :posted as `org-canvas--submissions-classify-change'
sorts them, :left for the students no longer in the pull, and :kept
for those of them whose heading stays because CARRY holds work under
it.  A student already marked left is not reported leaving again."
  (let ((students (plist-get previous :students))
        (changes nil)
        (seen nil))
    (dolist (sub submissions)
      (let* ((user-id (org-canvas--submissions-user-id sub))
             (new (org-canvas--submissions-state sub))
             (kind (org-canvas--submissions-classify-change
                    (alist-get user-id students) new)))
        (push user-id seen)
        (when kind
          (push (cons user-id (plist-get new :name))
                (plist-get changes (intern (format ":%s" kind)))))))
    (dolist (entry students)
      (unless (or (memql (car entry) seen)
                  (equal (plist-get (cdr entry) :status) "left"))
        (push (cons (car entry) (plist-get (cdr entry) :name)) (plist-get changes :left))
        (when (assoc (car entry) carry)
          (push (cons (car entry) (plist-get (cdr entry) :name)) (plist-get changes :kept)))))
    changes))

(defun org-canvas--submissions-departed-entries (previous carry submissions)
  "Return stand-in submissions for the students to keep although they left.
A student in PREVIOUS but not in SUBMISSIONS is kept when CARRY holds
work under their heading — notes, a draft, Rubric rows, a typed
score — since dropping the heading would drop that too.  Each stand-in
renders as a heading with STATUS left and the CANVAS_SCORE it had, so
the carried score comes back against its own baseline; nothing is
ever pushed for it (`org-canvas--submissions-left-p')."
  (let ((present (mapcar #'org-canvas--submissions-user-id submissions))
        (entries nil))
    (dolist (entry (plist-get previous :students))
      (let ((user-id (car entry))
            (state (cdr entry)))
        (when (and (not (memql user-id present)) (assoc user-id carry))
          (push `((user_id . ,user-id)
                  (user . ((sortable_name . ,(plist-get state :name))))
                  (org-canvas-left . t)
                  ,@(let ((score (plist-get state :score)))
                      (cond ((equal score "EX") '((excused . t)))
                            (score `((score . ,(string-to-number score)))))))
                entries))))
    (nreverse entries)))

(defun org-canvas--submissions-mark-resubmitted (resubmitted submissions)
  "Mark the headings of RESUBMITTED, (user-id . name) pairs, CONFLICT.
The value names the attempt SUBMISSIONS carry, in the push's short
style — `attempt: 2 submitted after grading' — so the grader sees the
score no longer describes the file; a successful push clears it."
  (save-excursion
    (dolist (pair resubmitted)
      (when (org-canvas--submissions-goto-user (car pair))
        (let ((sub (cl-find-if (lambda (s) (eql (org-canvas--submissions-user-id s) (car pair)))
                               submissions)))
          (org-entry-put (point) "CONFLICT"
                         (format "attempt: %s submitted after grading"
                                 (alist-get 'attempt sub))))))))

(defconst org-canvas--submissions-change-labels
  '((:new . "new") (:resubmitted . "resubmitted after grading")
    (:regraded . "scored on Canvas since the pull") (:posted . "posted")
    (:left . "left the course"))
  "The name of each kind of change a refresh can report, in report order.")

(defun org-canvas--submissions-describe-refresh (name previous found)
  "Return the one-line summary of what a refresh of NAME FOUND.
FOUND is the plist of `org-canvas--submissions-changes-since';
PREVIOUS supplies the PULLED_AT the no-change line names.  Each kind
present is counted; the students who left say how many headings were
kept for the work under them.  FOUND's :conflicted, the headings left
marked CONFLICT, are named (issue #440)."
  (let ((parts nil))
    (dolist (label org-canvas--submissions-change-labels)
      (when-let* ((pairs (plist-get found (car label))))
        (push (format "%d %s%s" (length pairs) (cdr label)
                      (if (eq (car label) :left)
                          (format " (%d kept)" (length (plist-get found :kept)))
                        ""))
              parts)))
    (concat
     (if parts
         (format "Refreshed %s: %s" name (string-join (nreverse parts) ", "))
       (format "Refreshed %s: no changes since %s" name
               (or (plist-get previous :pulled-at) "the last pull")))
     (org-canvas--submissions-describe-conflicted (plist-get found :conflicted)))))

(defun org-canvas--submissions-describe-conflicted (conflicted)
  "Return the refresh line's note naming CONFLICTED students, or \"\".
CONFLICTED is a list of (USER-ID NAME REASON), as
`org-canvas--submissions-conflicted-headings' returns it (issue #440)."
  (if conflicted
      (format "; CONFLICT on %d: %s" (length conflicted)
              (mapconcat (lambda (c) (format "%s (%s)" (nth 1 c) (nth 2 c)))
                         conflicted "; "))
    ""))

(defun org-canvas--submissions-log-changes (found)
  "Log one line per student a refresh FOUND something about.
FOUND is the plist of `org-canvas--submissions-changes-since'; each
line carries the label of its kind."
  (dolist (label org-canvas--submissions-change-labels)
    (dolist (pair (plist-get found (car label)))
      (org-canvas--log-info org-canvas--logger "[Refresh] %s: %s%s"
        (cdr pair) (cdr label)
        (if (and (eq (car label) :left) (assoc (car pair) (plist-get found :kept)))
            " (heading kept)"
          "")))))

(defun org-canvas--submissions-report-changes (name previous submissions carry
                                                     &optional assignment-id)
  "Say what a refresh of NAME changed, once it is rendered.
PREVIOUS is what the file recorded before (nil on a first pull, which
reports nothing), SUBMISSIONS the fetched ones and CARRY the work
carried over.  Resubmissions on graded rows are marked on their
headings, every change is logged per student, and the counts go to
the echo area.  With ASSIGNMENT-ID each resubmission also names the
attempt its score was given on (`org-canvas--submissions-report-attempts').
Return the summary line, or nil on a first pull."
  (when previous
    (let ((changes (org-canvas--submissions-changes-since previous submissions carry)))
      (org-canvas--submissions-mark-resubmitted (plist-get changes :resubmitted) submissions)
      (setq changes (plist-put changes :conflicted
                               (org-canvas--submissions-conflicted-headings)))
      (org-canvas--submissions-log-changes changes)
      (when assignment-id
        (org-canvas--submissions-report-attempts
         assignment-id (plist-get changes :resubmitted)))
      (let ((line (org-canvas--submissions-describe-refresh name previous changes)))
        (message "%s" line)
        line))))

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

;;;; Sent Comments (issue #419)

;; A comment already sent sits under the student's `** Comments'
;; heading as `- *Author* <time> [ID] :: text'.  The heading's
;; CANVAS_COMMENTS property holds each id's text digest as last pulled
;; or pushed, so an item whose text no longer digests to its entry was
;; edited here, and `- DELETE *Author* ...' marks one to delete; a line
;; removed from the section is never a deletion, since a missing line
;; is as likely a tidy-up as an intent.  S sends the grader's own
;; edited comments (PUT) and deletions (DELETE) after re-reading them:
;; one by someone else, one edited on Canvas since the pull, or one
;; gone from Canvas is refused and named, not sent.  A refresh keeps an
;; edit or a mark that was not sent.  An item without an id (a file
;; pulled before #419, or a comment posted from a draft) is read-only
;; until a refresh gives it one.

(defconst org-canvas--submissions-comment-item-regexp
  (concat "^-[ \t]+\\(DELETE[ \t]+\\)?"
          "\\(\\(?:[^:\n]\\|:[^:\n]\\)*?\\)[ \t]+\\[\\([0-9]+\\)\\][ \t]*::"
          "\\(?:[ \t]+\\(.*\\)\\)?$")
  "Match the first line of a Comments item that carries a comment id.
Group 1 is the DELETE mark, group 2 the label (the author and the
time), group 3 the id and group 4 the first line of the text, absent
when the text starts under the item.  The label holds no `::', so a
text that does cannot be mistaken for one.")

(defun org-canvas--submissions-comment-digest (text)
  "Return the digest of a sent comment's TEXT, normalized as an item's is."
  (org-canvas--submissions-bank-digest (org-canvas--submissions-comment-text text)))

(defun org-canvas--submissions-parse-digests (value)
  "Return VALUE, a string of ID=DIGEST pairs, as an alist of (ID . DIGEST)."
  (when value
    (delq nil (mapcar (lambda (pair)
                        (when (string-match "\\`\\([0-9]+\\)=\\([0-9a-f]+\\)\\'" pair)
                          (cons (match-string 1 pair) (match-string 2 pair))))
                      (split-string value)))))

(defun org-canvas--submissions-comment-baseline-value (comments)
  "Return the CANVAS_COMMENTS value for COMMENTS, or nil when none has an id."
  (let ((pairs (delq nil
                     (mapcar (lambda (c)
                               (when-let* ((id (org-canvas--alist-get-non-null 'id c)))
                                 (format "%s=%s" id
                                         (org-canvas--submissions-comment-digest
                                          (org-canvas--submissions-comment-org-text c)))))
                             (append comments nil)))))
    (and pairs (string-join pairs " "))))

(defun org-canvas--submissions-insert-comment-baseline (comments)
  "Insert the CANVAS_COMMENTS property line for COMMENTS, when any has an id."
  (when-let* ((value (org-canvas--submissions-comment-baseline-value comments)))
    (insert (format ":CANVAS_COMMENTS: %s\n" value))))

(defun org-canvas--submissions-comment-baseline ()
  "Return the CANVAS_COMMENTS baseline of the entry at point, (ID . DIGEST)."
  (org-canvas--submissions-parse-digests (org-entry-get (point) "CANVAS_COMMENTS")))

(defun org-canvas--submissions-set-comment-digest (id digest)
  "Set comment ID's CANVAS_COMMENTS entry at point to DIGEST; nil drops it."
  (let* ((old (org-canvas--submissions-comment-baseline))
         (new (if (assoc id old)
                  (delq nil (mapcar (lambda (pair)
                                      (if (equal (car pair) id) (and digest (cons id digest)) pair))
                                    old))
                (append old (and digest (list (cons id digest)))))))
    (if new
        (org-entry-put (point) "CANVAS_COMMENTS"
                       (mapconcat (lambda (p) (format "%s=%s" (car p) (cdr p))) new " "))
      (org-entry-delete (point) "CANVAS_COMMENTS"))))

(defun org-canvas--submissions-comment-item-end (start bound)
  "Return the end of the item starting at START, BOUND the section's end.
It is the end of the item's last non-blank line before the next item."
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (goto-char (if (and (<= (point) bound) (re-search-forward "^-[ \t]" bound t))
                   (match-beginning 0)
                 bound))
    (skip-chars-backward " \t\n" start)
    (point)))

(defun org-canvas--submissions-sent-comments ()
  "Return the Comments items of the entry at point that carry an id.
Each is a plist: :id (a string), :label, :delete (non-nil when marked
DELETE), :text (normalized as a Rubric comment is, nil when empty),
:start and :end, the end of its last non-blank line.  An item without
an id is left out, since nothing can address it."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-comments-heading)))
    (save-excursion
      (goto-char (car region))
      (let ((bound (save-excursion (goto-char (cdr region))
                                   (if (bolp) (point) (line-end-position))))
            (items nil))
        (while (re-search-forward org-canvas--submissions-comment-item-regexp bound t)
          (let* ((start (match-beginning 0))
                 (text-start (or (match-beginning 4) (match-end 0)))
                 (item (list :id (match-string-no-properties 3)
                             :label (match-string-no-properties 2)
                             :delete (and (match-beginning 1) t)))
                 (end (max text-start (org-canvas--submissions-comment-item-end start bound))))
            (push (append item
                          (list :start start :end end
                                :text (org-canvas--submissions-comment-text
                                       (buffer-substring-no-properties text-start end))))
                  items)
            (goto-char (max end (line-end-position)))))
        (nreverse items)))))

(defun org-canvas--submissions-find-sent-comment (id)
  "Return the Comments item of the entry at point whose id is ID, or nil."
  (seq-find (lambda (item) (equal (plist-get item :id) id))
            (org-canvas--submissions-sent-comments)))

(defun org-canvas--submissions-comment-pending-p (item baseline)
  "Return non-nil when ITEM, a sent comment, has a change to send.
BASELINE is the entry's CANVAS_COMMENTS alist.  An item marked DELETE
has; so does one whose text no longer digests to its entry.  An item
the baseline does not name cannot be told edited, and has not."
  (or (plist-get item :delete)
      (when-let* ((digest (cdr (assoc (plist-get item :id) baseline))))
        (not (equal digest (org-canvas--submissions-comment-digest
                            (plist-get item :text)))))))

(defun org-canvas--submissions-comment-edits-at-point (user-id)
  "Return the pending sent comments of USER-ID's heading at point.
Each is a `org-canvas--submissions-sent-comments' plist with :user-id
\(a number), :name and :baseline, the digest it was edited against."
  (let ((baseline (org-canvas--submissions-comment-baseline))
        (name (org-get-heading t t t t)))
    (mapcar (lambda (item)
              (append (list :user-id (string-to-number user-id) :name name
                            :baseline (cdr (assoc (plist-get item :id) baseline)))
                      item))
            (seq-filter (lambda (item) (org-canvas--submissions-comment-pending-p item baseline))
                        (org-canvas--submissions-sent-comments)))))

(defun org-canvas--submissions-comment-edits-by-student (&optional skip-left)
  "Return (USER-ID . EDITS) for every student whose sent comments changed here.
With SKIP-LEFT a student who left the course is passed over."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (when-let* ((user-id (org-entry-get (point) "USER_ID"))
                    ((not (and skip-left (org-canvas--submissions-left-p))))
                    (edits (org-canvas--submissions-comment-edits-at-point user-id)))
          (push (cons (string-to-number user-id) edits) found))
        (forward-line 1)))
    (nreverse found)))

(defun org-canvas--submissions-collect-comment-edits ()
  "Return every sent comment edited or marked DELETE here, in file order.
A student who left the course is skipped, as their draft is."
  (apply #'append (mapcar #'cdr (org-canvas--submissions-comment-edits-by-student t))))

(defun org-canvas--submissions-pending-comment-edits (assignment-id &optional only)
  "Return the sent comments changed here as (SENDABLE . REFUSED), or nil.
The changes are those `org-canvas--submissions-collect-comment-edits'
finds, checked against ASSIGNMENT-ID on Canvas by
`org-canvas--submissions-check-comment-edits'.  The full push and the
comment-only push both start here (issue #425).  ONLY, a list of user
ids, keeps those students' changes alone (issue #441)."
  (org-canvas--submissions-check-comment-edits
   assignment-id (org-canvas--submissions-only-rows
                  only (org-canvas--submissions-collect-comment-edits))))

(defun org-canvas--submissions-only-rows (only rows)
  "Return ROWS, plists with a :user-id, restricted to the user ids ONLY lists.
Nil ONLY keeps every row (issue #441)."
  (if only
      (seq-filter (lambda (r) (memql (plist-get r :user-id) only)) rows)
    rows))

(defun org-canvas--submissions-live-comments (assignment-id user-ids)
  "Return a hash from comment id to (AUTHOR-ID . DIGEST) on ASSIGNMENT-ID.
Only the submissions of USER-IDS are read into it, the digest taken
of each comment's text as its item would show it."
  (let ((live (make-hash-table :test 'equal))
        (org-canvas--submissions-comment-cache (make-hash-table :test 'equal)))
    (dolist (sub (append (org-canvas--submissions-fetch-for-assignment assignment-id) nil))
      (when (memql (org-canvas--submissions-user-id sub) user-ids)
        (dolist (c (append (alist-get 'submission_comments sub) nil))
          (puthash (format "%s" (alist-get 'id c))
                   (cons (alist-get 'author_id c)
                         (org-canvas--submissions-comment-digest
                          (org-canvas--submissions-comment-org-text c)))
                   live))))
    live))

(defun org-canvas--submissions-read-live-comments (assignment-id edits)
  "Return (SELF . LIVE) for EDITS on ASSIGNMENT-ID, or `unread'.
SELF is the token owner's user id and LIVE the hash of
`org-canvas--submissions-live-comments'.  A failed read is one warning."
  (condition-case err
      (cons (org-canvas--submissions-self-id)
            (org-canvas--submissions-live-comments
             assignment-id (delete-dups (mapcar (lambda (e) (plist-get e :user-id)) edits))))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Could not read the sent comments (%s); none edited or deleted"
       (error-message-string err))
     'unread)))

(defun org-canvas--submissions-comment-refusal (edit live self)
  "Return why EDIT may not be sent, or nil when it may.
LIVE is the comment's (AUTHOR-ID . DIGEST) on Canvas, nil when Canvas
no longer holds it; SELF is the token owner's user id, a string.
Canvas lets only a comment's author rewrite it, and the push holds
deletions to the same rule, so another grader's comment stays theirs."
  (cond ((and (not (plist-get edit :delete)) (null (plist-get edit :text)))
         "emptied; mark it DELETE to delete it")
        ((null live) "no longer on Canvas")
        ((not (equal (format "%s" (car live)) self))
         "not yours; only its author may change it")
        ((and (plist-get edit :baseline)
              (not (equal (cdr live) (plist-get edit :baseline))))
         "edited on Canvas since the pull")))

(defun org-canvas--submissions-check-comment-edits (assignment-id edits)
  "Split EDITS into (SENDABLE . REFUSED) against ASSIGNMENT-ID on Canvas.
The grader's user id and the students' comments are read once; a
refused edit carries its :refused reason and is logged as a warning.
When Canvas cannot be read every edit is refused, since neither the
author nor the baseline could be checked.  Nil without EDITS."
  (when edits
    (let ((read (org-canvas--submissions-read-live-comments assignment-id edits))
          (ok nil)
          (bad nil))
      (dolist (edit edits)
        (let ((why (if (eq read 'unread)
                       "Canvas could not be read"
                     (org-canvas--submissions-comment-refusal
                      edit (gethash (plist-get edit :id) (cdr read)) (car read)))))
          (if (not why)
              (push edit ok)
            (org-canvas--log-warning org-canvas--logger
              "[Submissions] Comment %s on %s not changed: %s"
              (plist-get edit :id) (plist-get edit :name) why)
            (push (append (list :refused why) edit) bad))))
      (cons (nreverse ok) (nreverse bad)))))

(defun org-canvas--submissions-comment-url (assignment-id edit)
  "Return the URL of EDIT's sent comment on ASSIGNMENT-ID.
Canvas addresses the submission by the student's user id (#125)."
  (org-canvas-api-course-endpoint "assignments/%s/submissions/%s/comments/%s"
                                  assignment-id (plist-get edit :user-id) (plist-get edit :id)))

(defun org-canvas--submissions-comment-edit-payload (text)
  "Return the body of the request that rewrites a sent comment as TEXT.
Canvas's `Edit a submission comment' takes the text as a top-level
`comment', not wrapped as a new comment's `text_comment' is."
  `((comment . ,text)))

(defun org-canvas--submissions-send-comment-edit (assignment-id edit)
  "Send EDIT, a sent comment of ASSIGNMENT-ID to rewrite or delete.
Return `dry-run' under `org-canvas--dry-run', when nothing is sent
\(Hard Rule 1); `deleted' after a deletion; else Canvas's reply, the
comment as it stored it."
  (let ((url (org-canvas--submissions-comment-url assignment-id edit))
        (verb (if (plist-get edit :delete) "delete" "edit")))
    (cond (org-canvas--dry-run
           (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s comment %s on %s"
             verb (plist-get edit :id) (plist-get edit :name))
           'dry-run)
          ((plist-get edit :delete)
           (org-canvas-api-request 'DELETE url)
           'deleted)
          (t (org-canvas-api-request
              'PUT url :data (org-canvas--submissions-comment-edit-payload
                              (plist-get edit :text)))))))

(defun org-canvas--submissions-rewrite-sent-comment (item text &optional delete)
  "Replace ITEM of the entry at point by its label, id and TEXT.
With DELETE the item is marked for deletion."
  (save-excursion
    (goto-char (plist-get item :start))
    (delete-region (plist-get item :start) (plist-get item :end))
    (insert (org-canvas--submissions-comment-line
             (plist-get item :label) (plist-get item :id) (or text "") delete))))

(defun org-canvas--submissions-record-comment-edit (edit reply)
  "Record under EDIT's student what Canvas answered, REPLY.
A deleted comment's item and baseline entry go; a rewritten one shows
the text Canvas stored, which becomes its baseline."
  (save-excursion
    (when-let* (((org-canvas--submissions-goto-user (plist-get edit :user-id)))
                (item (org-canvas--submissions-find-sent-comment (plist-get edit :id))))
      (if (eq reply 'deleted)
          (progn
            (delete-region (plist-get item :start)
                           (min (point-max) (1+ (plist-get item :end))))
            (org-canvas--submissions-set-comment-digest (plist-get edit :id) nil))
        (let ((text (if (stringp (org-canvas--alist-get-non-null 'comment reply))
                        (org-canvas--submissions-comment-org-text reply)
                      (plist-get edit :text))))
          (org-canvas--submissions-rewrite-sent-comment item text)
          (org-canvas--submissions-set-comment-digest
           (plist-get edit :id) (org-canvas--submissions-comment-digest text)))))))

(defun org-canvas--submissions-apply-comment-edit (assignment-id edit)
  "Send EDIT to ASSIGNMENT-ID and record it; return the count it falls under.
The count is :edited, :deleted, :dry-run, :failed for a request
Canvas refused or did not answer, or :errored for an error in Emacs,
which is not Canvas's doing (`org-canvas--api-failure-p', issue #443).
Either is one warning and leaves the item pending."
  (condition-case err
      (let ((reply (org-canvas--submissions-send-comment-edit assignment-id edit)))
        (if (eq reply 'dry-run)
            :dry-run
          (org-canvas--submissions-record-comment-edit edit reply)
          (if (eq reply 'deleted) :deleted :edited)))
    (error
     (let ((canvas (org-canvas--api-failure-p err)))
       (org-canvas--log-warning org-canvas--logger
         "[Submissions] Could not %s comment %s on %s%s: %s"
         (if (plist-get edit :delete) "delete" "edit")
         (plist-get edit :id) (plist-get edit :name)
         (if canvas "" " (an error in Emacs, not from Canvas)")
         (error-message-string err))
       (if canvas :failed :errored)))))

(defun org-canvas--submissions-apply-comment-edits (assignment-id edits)
  "Send each of EDITS to ASSIGNMENT-ID; return the counts, or nil without any.
The plist has :edited, :deleted, :dry-run, :failed and :errored."
  (when edits
    (let ((counts (list :edited 0 :deleted 0 :dry-run 0 :failed 0 :errored 0)))
      (dolist (edit edits)
        (let ((key (org-canvas--submissions-apply-comment-edit assignment-id edit)))
          (plist-put counts key (1+ (plist-get counts key)))))
      counts)))

(defun org-canvas--submissions-describe-comment-edits (edits)
  "Return the confirmation's words for EDITS, sent comments to send, or nil."
  (let* ((deletes (cl-count-if (lambda (e) (plist-get e :delete)) edits))
         (rewrites (- (length edits) deletes)))
    (when edits
      (string-join (delq nil (list (when (> rewrites 0) (format "%d comment edit(s)" rewrites))
                                   (when (> deletes 0) (format "%d comment deletion(s)" deletes))))
                   " and "))))

(defun org-canvas--submissions-list-comment-edits (comments)
  "Return COMMENTS, (SENDABLE . REFUSED) edits, one line each for the prompt."
  (mapconcat (lambda (e)
               (format "  %s: %s comment %s%s"
                       (plist-get e :name)
                       (cond ((plist-get e :refused) "not sending")
                             ((plist-get e :delete) "delete")
                             (t "edit"))
                       (plist-get e :id)
                       (if (plist-get e :refused) (format " (%s)" (plist-get e :refused)) "")))
             (append (car comments) (cdr comments)) "\n"))

(defun org-canvas--submissions-refused-note (refused)
  "Return the note a push message ends with for REFUSED comment edits."
  (if refused
      (format "; %d comment change(s) not sent (see the log)" (length refused))
    ""))

(defun org-canvas--submissions-count-note (counts key words)
  "Return \", N WORDS (see the log)\" for KEY's count N in COUNTS, or \"\"."
  (let ((n (or (plist-get counts key) 0)))
    (if (> n 0) (format ", %d %s (see the log)" n words) "")))

(defun org-canvas--submissions-comment-edits-note (counts refused)
  "Return the push message's note on sent comments, from COUNTS and REFUSED.
COUNTS is what `org-canvas--submissions-apply-comment-edits' returned."
  (concat
   (cond ((null counts) "")
         ((> (plist-get counts :dry-run) 0)
          (format "; dry run: would change %d sent comment(s)" (plist-get counts :dry-run)))
         (t (format "; %d sent comment(s) edited, %d deleted%s%s"
                    (plist-get counts :edited) (plist-get counts :deleted)
                    (org-canvas--submissions-count-note
                     counts :failed "failed at Canvas")
                    (org-canvas--submissions-count-note
                     counts :errored "failed in Emacs, not at Canvas"))))
   (org-canvas--submissions-refused-note refused)))

(defun org-canvas--submissions-comment-edit-result (counts comments)
  "Return the counts a push reports for sent comments, as a plist.
COUNTS is what `org-canvas--submissions-apply-comment-edits' returned
and COMMENTS the (SENDABLE . REFUSED) it was given.  The plist has
:edited, :deleted, :refused (not sent, see
`org-canvas--submissions-comment-refusal'), :failed (refused by
Canvas, or not answered), :errored (an error in Emacs, issue #443) and
:dry-run."
  (list :edited (or (plist-get counts :edited) 0)
        :deleted (or (plist-get counts :deleted) 0)
        :refused (length (cdr comments))
        :failed (or (plist-get counts :failed) 0)
        :dry-run (or (plist-get counts :dry-run) 0)
        :errored (or (plist-get counts :errored) 0)))

(defun org-canvas--submissions-restore-comment (edit)
  "Put EDIT, a sent comment changed here, back into the fresh entry at point.
Nothing is written when Canvas now holds the edited text (it was
pushed meanwhile).  Otherwise the item takes the grader's text or mark
again, with the baseline it was edited against, so a comment edited on
Canvas since is refused by the next push rather than overwritten.  A
comment gone from Canvas is one warning, which carries the text."
  (let ((item (org-canvas--submissions-find-sent-comment (plist-get edit :id)))
        (fresh (cdr (assoc (plist-get edit :id) (org-canvas--submissions-comment-baseline)))))
    (cond ((null item)
           (org-canvas--log-warning org-canvas--logger
             "[Refresh] Comment %s on %s is no longer on Canvas; the change made here was dropped: %s"
             (plist-get edit :id) (plist-get edit :name)
             (if (plist-get edit :delete) "DELETE" (plist-get edit :text))))
          ((and (not (plist-get edit :delete))
                (equal fresh (org-canvas--submissions-comment-digest (plist-get edit :text)))))
          (t (org-canvas--submissions-reapply-comment item edit fresh)))))

(defun org-canvas--submissions-reapply-comment (item edit fresh)
  "Write EDIT over ITEM, freshly rendered with the digest FRESH, at point.
The baseline goes back to the one EDIT was made against; when Canvas
moved from it, the log says the change will not be sent as it stands."
  (let ((baseline (plist-get edit :baseline)))
    (org-canvas--submissions-rewrite-sent-comment
     item (plist-get edit :text) (plist-get edit :delete))
    (org-canvas--submissions-set-comment-digest (plist-get edit :id) (or baseline fresh))
    (when (and baseline (not (equal baseline fresh)))
      (org-canvas--log-warning org-canvas--logger
        "[Refresh] Comment %s on %s was edited on Canvas since; the change made here is kept and a push will not send it"
        (plist-get edit :id) (plist-get edit :name)))))

(defun org-canvas--submissions-restore-comments (carry)
  "Write CARRY, from `org-canvas--submissions-comment-edits-by-student', back."
  (save-excursion
    (dolist (entry carry)
      (when (org-canvas--submissions-goto-user (car entry))
        (dolist (edit (cdr entry))
          (org-canvas--submissions-restore-comment edit))))))

(defconst org-canvas--submissions-rubric-heading "** Rubric"
  "Heading under which a student's rubric table lives.")

(defun org-canvas--submissions-rubric-criteria (assignment)
  "Return ASSIGNMENT's rubric criteria as a list, or nil without a rubric.
Each criterion's description is decoded to Org text here, once, since
Canvas escapes it (`session&#39;s') and every student's Rubric table
repeats it (issue #265)."
  (mapcar (lambda (c)
            (cons (cons 'description
                        (org-canvas--submissions-inline-text (alist-get 'description c)))
                  c))
          (append (alist-get 'rubric assignment) nil)))

(defun org-canvas--submissions-rubric-for-grading (assignment)
  "Return non-nil when the grade of ASSIGNMENT comes from its rubric.
The Assignment object carries the flag at its top level as
`use_rubric_for_grading' — the field the drift report compares for
RUBRIC_USE_FOR_GRADING — while its `rubric_settings' hold only the
rubric's id, title, points and display flags; reading
`use_for_grading' there answered false for every assignment (issue
#253).  The settings key is still honoured for an object that carries
it, as a rubric association does."
  (let ((top (alist-get 'use_rubric_for_grading assignment))
        (nested (alist-get 'use_for_grading (alist-get 'rubric_settings assignment))))
    (or (eq top t) (and (null top) (eq nested t)))))

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
Both are strings spelled as the Rubric section shows them, or nil when
the criterion is unscored or uncommented."
  (let ((entry (alist-get (intern criterion-id) assessment)))
    (when (consp entry)
      (let ((points (org-canvas--alist-get-non-null 'points entry))
            (comment (org-canvas--alist-get-non-null 'comments entry)))
        (cons (and (numberp points) (org-canvas--submissions-format-number points))
              (org-canvas--submissions-comment-text comment))))))

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
  "Return the Rubric rows for CRITERIA as scored by ASSESSMENT.
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

(defconst org-canvas--submissions-rubric-item-regexp
  "^-[ \t]+\\([^ \t\n]+\\)[ \t]+::\\(?:[ \t]+\\(.*\\)\\)?$"
  "Match the first line of a Rubric comment item, `- ID :: text'.
Group 1 is the criterion id, group 2 the first line of the comment,
absent when the item is empty.  The comment's other lines follow,
indented, up to the next item.")

(defun org-canvas--submissions-item (label text)
  "Return the description item `- LABEL :: TEXT', TEXT's later lines indented.
The first line of TEXT follows the label and the rest are indented
two spaces under it, a blank line kept as a paragraph break, so the
item wraps like any paragraph and keeps its line breaks.  Nil or an
empty TEXT gives a bare `- LABEL ::'.  No newline ends the result."
  (let ((lines (split-string (or text "") "\n")))
    (concat (format "- %s ::" label)
            (if (string-empty-p (car lines)) "" (concat " " (car lines)))
            (mapconcat (lambda (line) (if (string-empty-p line) "\n" (concat "\n  " line)))
                       (cdr lines) ""))))

(defun org-canvas--submissions-render-rubric (criteria assessment)
  "Insert the Rubric heading, table and items for CRITERIA scored by ASSESSMENT.
One table row per criterion — its Canvas id, description and points
possible, then the Score cell the grader fills — and under the table
one `- ID :: comment' item per criterion, where the comment is
written; an assessment Canvas already holds pre-fills both.  Nothing
is inserted without a rubric."
  (let ((rows (org-canvas--submissions-rubric-rows-from-canvas criteria assessment)))
    (when rows
      (insert "\n" org-canvas--submissions-rubric-heading "\n")
      (insert "| Id | Criterion | Max | Score |\n|---+---+---+---|\n")
      (dolist (row rows)
        (insert (format "| %s | %s | %s | %s |\n"
                        (nth 0 row) (or (nth 1 row) "") (or (nth 2 row) "") (or (nth 3 row) ""))))
      (save-excursion (forward-line -1) (org-table-align))
      (dolist (row rows)
        (insert (org-canvas--submissions-item (nth 0 row) (nth 4 row)) "\n")))))

(defun org-canvas--submissions-table-row-cells (line)
  "Return the cells of table row LINE, trimmed, or nil for a rule or a non-row.
A row a grader is still typing may lack its closing bar."
  (let ((trimmed (string-trim line)))
    (when (and (string-prefix-p "|" trimmed) (not (string-prefix-p "|-" trimmed)))
      (mapcar #'string-trim
              (split-string (substring trimmed 1 (and (string-suffix-p "|" trimmed) -1))
                            "|")))))

(defun org-canvas--submissions-rubric-items (region)
  "Return the comment items of the Rubric section REGION as (ID START TEXT END).
REGION is its (START . END).  An item is a `- ID :: text' line and its
continuation lines, everything up to the next item; START begins the
item, TEXT its comment (after the `::'), and END is the end of its
last non-blank line, so the blank lines before the next item or the
section's end are nobody's.  In order of appearance."
  (save-excursion
    (goto-char (car region))
    ;; The section's END is the next heading's line start or the end of
    ;; the subtree, which sits before the last line's trailing whitespace
    ;; — and an item line ending in spaces must match through them.
    (let ((bound (save-excursion
                   (goto-char (cdr region))
                   (if (bolp) (point) (line-end-position))))
          (items nil))
      (while (re-search-forward org-canvas--submissions-rubric-item-regexp bound t)
        (let* ((id (match-string-no-properties 1))
               (start (match-beginning 0))
               (text (or (match-beginning 2) (match-end 0)))
               (next (save-excursion
                       (if (re-search-forward org-canvas--submissions-rubric-item-regexp bound t)
                           (match-beginning 0)
                         bound)))
               (end (save-excursion
                      (goto-char next)
                      (skip-chars-backward " \t\n" start)
                      (point))))
          (push (list id start text (max text end)) items)
          (goto-char next)))
      (nreverse items))))

(defun org-canvas--submissions-rubric-comments (region)
  "Return the comment items in the Rubric section REGION as (ID . TEXT) pairs.
TEXT is the comment normalized, nil for an item left empty."
  (mapcar (lambda (item)
            (cons (car item)
                  (org-canvas--submissions-comment-text
                   (buffer-substring-no-properties (nth 2 item) (nth 3 item)))))
          (org-canvas--submissions-rubric-items region)))

(defun org-canvas--submissions-rubric-row (cells comments)
  "Return CELLS, a Rubric table row's, as (ID CRITERION MAX SCORE COMMENT).
Empty cells are nil; nil without an id.  The comment is the row's item
among COMMENTS, the section's (ID . TEXT) pairs, or, when the row has
no item, a fifth cell: the shape a grading file had before issue #263
moved comments out of the table, accepted so a file graded then still
pushes until its next pull rewrites it."
  (let* ((row (mapcar (lambda (c) (and (not (string-empty-p c)) c))
                      (cl-subseq (append cells (make-list 5 "")) 0 5)))
         (item (assoc (car row) comments)))
    (when (car row)
      (list (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row)
            (if item (cdr item) (org-canvas--submissions-comment-text (nth 4 row)))))))

(defun org-canvas--submissions-rubric-rows ()
  "Return the Rubric rows of the entry at point, or nil without a table.
Each row is (ID CRITERION MAX SCORE COMMENT): the first four as the
table spells them, cells trimmed and empty cells nil, the header row
and rows without an id dropped; the comment from the row's `- ID ::'
item under the table (`org-canvas--submissions-rubric-row')."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-rubric-heading)))
    (let ((comments (org-canvas--submissions-rubric-comments region))
          (rows nil) (header t))
      (dolist (line (split-string (buffer-substring-no-properties (car region) (cdr region))
                                  "\n"))
        (when-let* ((cells (org-canvas--submissions-table-row-cells line)))
          (if header
              (setq header nil)
            (when-let* ((row (org-canvas--submissions-rubric-row cells comments)))
              (push row rows)))))
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
NEW-SCORE was itself edited away from OLD-SCORE.  Rows that carry no
points have no total and derive nothing: the score stays as typed and
follows the assessment in a request of its own
\(`org-canvas--submissions-grade-after', issue #444).  An edited score
that disagrees with the total is a `user-error', since both cannot be
right, and so is a cleared one (NEW-SCORE nil), since Canvas would
derive a grade from the rows the clear takes away (issue #417); one
that agrees, or an excusal, is left as typed."
  (when (and rubric (org-canvas--submissions-rubric-for-grading-p))
    (let ((total (plist-get rubric :total))
          (edited (not (equal new-score old-score))))
      (cond ((not edited)
             (and total (list :new-score total :score-derived t)))
            ((null new-score)
             (user-error "%s: SCORE clears the grade but the Rubric rows changed; undo one of them"
                         name))
            ((and new-score total (not (equal new-score "EX"))
                  (/= (string-to-number new-score) (string-to-number total)))
             (user-error "%s: SCORE %s disagrees with the rubric total %s; clear one of them"
                         name new-score total))))))

(defun org-canvas--submissions-grade-after (rubric score)
  "Return (:grade-after t) when SCORE must follow RUBRIC in a second request.
RUBRIC is the change of `org-canvas--submissions-rubric-change-at-point'
and SCORE the grade the change leaves the student with.  On a rubric
used for grading, an assessment none of whose rows carries points
leaves Canvas with no grade, whatever grade the same request sends
\(see `org-canvas--submissions-rubric-payload'); the grade then goes
after it, alone.  Nil without such an assessment, or without a grade
to keep."
  (and rubric score
       (null (plist-get rubric :total))
       (org-canvas--submissions-rubric-for-grading-p)
       (list :grade-after t)))

(defun org-canvas--submissions-rubric-set-score (id score region)
  "Write SCORE into the Rubric table row keyed by ID within REGION.
Nil empties the cell; the id, criterion and max cells stay, and a
fifth cell — the comment's place before issue #263 — is emptied, since
the comment now lives in the row's item.  Return non-nil when the row
exists."
  (save-excursion
    (goto-char (car region))
    (catch 'done
      (while (< (point) (cdr region))
        (let ((cells (org-canvas--submissions-table-row-cells
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position)))))
          (when (equal (car cells) id)
            (let ((kept (cl-subseq (append cells (make-list 4 "")) 0 4)))
              (delete-region (line-beginning-position) (line-end-position))
              (insert (format "| %s | %s | %s | %s |%s"
                              id (nth 1 kept) (nth 2 kept) (or score "")
                              (if (> (length cells) 4) " |" "")))
              (org-table-align)
              (throw 'done t))))
        (forward-line 1))
      nil)))

(defun org-canvas--submissions-rubric-table-end (region)
  "Return the end of the last table line in REGION, or its start without one."
  (save-excursion
    (goto-char (car region))
    (let ((end (car region)))
      (while (re-search-forward "^[ \t]*|" (cdr region) t)
        (setq end (line-end-position)))
      end)))

(defun org-canvas--submissions-rubric-set-comment (id comment region)
  "Write COMMENT into the `- ID ::' item of the Rubric section REGION.
An item the section holds is rewritten in place, continuation lines
included, and emptied when COMMENT is nil; one it lacks is added after
the last item, or after the table when there is none, and only for a
comment."
  (let* ((items (org-canvas--submissions-rubric-items region))
         (item (assoc id items))
         (text (org-canvas--submissions-item id comment)))
    (save-excursion
      (cond (item
             (delete-region (nth 1 item) (nth 3 item))
             (goto-char (nth 1 item))
             (insert text))
            (comment
             (goto-char (if items
                            (nth 3 (car (last items)))
                          (org-canvas--submissions-rubric-table-end region)))
             (insert "\n" text))))))

(defun org-canvas--submissions-rubric-set-row (id score comment)
  "Write SCORE and COMMENT for the Rubric row keyed by ID in the entry at point.
The score goes into the table row and the comment into the `- ID ::'
item under the table; either may be nil to empty its place.  Return
non-nil when the row exists; nothing is written when it does not."
  (when-let* ((region (org-canvas--submissions-section-region
                       org-canvas--submissions-rubric-heading)))
    (when (org-canvas--submissions-rubric-set-score id score region)
      (org-canvas--submissions-rubric-set-comment
       id comment (org-canvas--submissions-section-region
                   org-canvas--submissions-rubric-heading))
      t)))

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
    (org-entry-put (point) "CONFLICT" "rubric: assessed on Canvas since these rows were typed")))

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
the one-line record; Canvas keeps the original.  The record goes after
the last item, a comment's indented paragraphs included, and carries no
id, so it is read-only until a refresh renders it with one (#419)."
  (let ((inhibit-read-only t)
        (line (format "- *You* %s :: %s\n"
                      (format-time-string "<%Y-%m-%d %a %H:%M>")
                      (replace-regexp-in-string "\n+" " " text))))
    (save-excursion
      (org-back-to-heading t)
      (let ((end (save-excursion (org-end-of-subtree t) (point)))
            (region (org-canvas--submissions-section-region
                     org-canvas--submissions-comments-heading)))
        (if region
            (progn
              (goto-char (cdr region))
              (skip-chars-backward " \t\n" (car region))
              (if (> (point) (car region))
                  (insert "\n" (string-remove-suffix "\n" line))
                (insert line)))
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
alone, and so is a student who left the course (STATUS left), since
there is nothing to push for them.  On an assignment with a rubric,
full credit also fills every
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
          (cond ((or (not (org-entry-get (point) "USER_ID"))
                     (org-canvas--submissions-left-p))
                 nil)
                ((or (equal (and has (org-canvas--submissions-parse-score current)) "EX")
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
  "Download URL to DIRECTORY as FILENAME using the token query parameter.
The access_token parameter, not an Authorization header, is deliberate:
Canvas's download route 302-redirects to a presigned S3 or InstFS URL
whose own signature lives in the query string, and a request carrying
both that signature and an Authorization header is rejected (S3 allows
one auth mechanism only).  The headerless fetch `url-copy-file' makes
is the one that survives the redirect chain (#435)."
  (let ((output-path (expand-file-name filename directory)))
    (condition-case err
        (url-copy-file
         (concat url
                 (if (string-match-p "\\?" url) "&" "?")
                 "access_token=" (org-canvas--api-token))
         output-path t)
      ;; url-copy-file puts the whole URL, token included, into the
      ;; error data; redact it before the signal reaches the user
      ;; (issue #154's rule applied to a URL rather than a body).
      (error (signal (car err)
                     (mapcar (lambda (datum)
                               (if (stringp datum)
                                   (org-canvas--log-redact datum)
                                 datum))
                             (cdr err)))))))

(defun org-canvas--submissions-sanitize-filename (name)
  "Sanitize NAME for use as a directory/filename.
Replaces problematic characters with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" name))

;;;; Entry Points

;; Each command takes its target as an optional argument and prompts only
;; when it is nil, so a script, a keyboard macro over several columns or
;; a tool built on the package can pull, open and refresh without the
;; minibuffer (issue #280).  The prompts live in the resolvers below
;; rather than in a sexp `interactive' spec, which blanks undercover's
;; line counts for the whole body (see CLAUDE.md).

(defun org-canvas--submissions-resolve-assignment (assignment)
  "Return the Canvas assignment object ASSIGNMENT names.
An integer, or a string of digits, is an id and costs one request; any
other string is an exact name, found in the course's assignment list;
nil asks for the name in the minibuffer.  Signal a `user-error' when
nothing matches, so a script is told its mistake instead of pulling
the wrong column."
  (if (or (integerp assignment)
          (and (stringp assignment) (string-match-p "\\`[0-9]+\\'" assignment)))
      (or (org-canvas--submissions-fetch-assignment (format "%s" assignment))
          (user-error "No assignment with id %s in this course" assignment))
    (let* ((assignments (org-canvas--submissions-fetch-assignments))
           (names (mapcar (lambda (a) (alist-get 'name a)) assignments))
           (name (or assignment (completing-read "Assignment: " names nil t))))
      (or (cl-find-if (lambda (a) (equal (alist-get 'name a) name)) assignments)
          (user-error "No assignment named %s in this course" name)))))

(defun org-canvas--submissions-named-grading-file (file)
  "Return the grading file the name or path FILE spells, or signal.
FILE is an absolute path, or a name under the submissions directory
with or without its .org: the file's own name, or the assignment's,
which the pull spelled into a file name."
  (let* ((dir (org-canvas--submissions-dir))
         (given (if (string-suffix-p ".org" file) file (concat file ".org")))
         (candidates (list (expand-file-name given dir)
                           (org-canvas--submissions-file-path file))))
    (or (cl-find-if #'file-exists-p candidates)
        (user-error "No grading file %s in %s" file dir))))

(defun org-canvas--submissions-choose-grading-file ()
  "Ask for one of the submissions directory's files and return its path."
  (let* ((dir (org-canvas--submissions-dir))
         (files (and (file-directory-p dir)
                     (directory-files dir nil "\\.org\\'"))))
    (unless files
      (user-error "No grading files in %s; pull an assignment first" dir))
    (expand-file-name (completing-read "Grading file: " files nil t) dir)))

(defun org-canvas--submissions-grading-file-path (file)
  "Return the path of the grading file FILE names, or ask for one.
FILE is a Canvas assignment id (an integer, or a string of digits),
matched against each grading file's CANVAS_ASSIGNMENT_ID whatever the
file is called (issue #431); or an absolute path, or a name under
`org-canvas-submissions-directory' with or without its .org: the
file's own name, or the assignment's, which the pull spelled into a
file name.  A string of digits no header names is tried as a name.
Nil offers the directory's files in the minibuffer.  A FILE that
matches no file, or an id two files claim, is a `user-error'."
  (cond ((null file) (org-canvas--submissions-choose-grading-file))
        ((and (org-canvas--submissions-id-p file)
              (org-canvas--submissions-claimed-file file)))
        ((integerp file)
         (user-error "No grading file for assignment %s in %s; pull it first"
                     file (org-canvas--submissions-dir)))
        (t (org-canvas--submissions-named-grading-file file))))

(defun org-canvas--submissions-visit-grading-file (path)
  "Return the buffer visiting the grading file PATH, set up for grading.
Org mode and `org-canvas-submissions-mode' are on and the assignment's
id, name and view are recovered from the file's header, so any grading
command can run in the buffer it returns."
  (let ((buf (org-canvas--find-file-noselect path)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (org-canvas-submissions-mode 1)
      (org-canvas--submissions-ensure-context))
    buf))

;;;###autoload
(defun org-canvas-pull-submissions (&optional assignment download)
  "Pull ASSIGNMENT's submissions and return the buffer showing them.
ASSIGNMENT is a Canvas assignment id (an integer, or a string of
digits) or an assignment's exact name; interactively, and when it is
nil, the name is read in the minibuffer.  The detail view is saved as
a grading file under `org-canvas-submissions-directory' and visited;
the summary view is an ephemeral table.  Which one opens first is
`org-canvas-submissions-default-view'.  With DOWNLOAD non-nil (from
Lisp; there is no prefix argument for it) the grading file is the
view whatever the default, and every student's attachments are
downloaded beside it the way D does.  A script that knows the id
calls (org-canvas-pull-submissions \"2497503\" t) and needs nothing
interactive (issue #280)."
  (interactive)
  (let* ((selected (org-canvas--submissions-resolve-assignment assignment))
         (name (alist-get 'name selected))
         (assignment-id (number-to-string (alist-get 'id selected)))
         (submissions
          (org-canvas--submissions-fetch-with-reports assignment-id))
         (buf (org-canvas--submissions-display
               name assignment-id submissions
               (if download 'detail org-canvas-submissions-default-view)
               selected)))
    (when download
      (with-current-buffer buf
        (org-canvas-submissions-download-all-attachments)))
    buf))

;;;###autoload
(defun org-canvas-open-submissions (&optional file)
  "Visit the grading file FILE with `org-canvas-submissions-mode' on.
Grading files are the detail views `org-canvas-pull-submissions' saves
under `org-canvas-submissions-directory'.  FILE is a Canvas
assignment id, found in the files' headers whatever they are called
\(issue #431), an absolute path, or a name under that directory with
or without its .org — the file's name or the assignment's;
interactively, and when it is nil, one is chosen in the minibuffer.
Return the buffer (issue #280)."
  (interactive)
  (let ((buf (org-canvas--submissions-visit-grading-file
              (org-canvas--submissions-grading-file-path file))))
    (with-current-buffer buf
      (when (org-canvas--submissions-refresh-links)
        (save-buffer)))
    (switch-to-buffer buf)
    buf))

(defun org-canvas--submissions-refresh-buffer ()
  "Re-fetch and re-render the current buffer's submissions; return the buffer.
The body of `org-canvas-submissions-refresh', shared with
`org-canvas-submissions-refresh-file'."
  (org-canvas--submissions-ensure-context)
  (let ((id org-canvas-submissions--assignment-id)
        (name org-canvas-submissions--assignment-name)
        (view org-canvas-submissions--current-view))
    (unless id
      (user-error "No assignment ID in this buffer"))
    (when (eq view 'summary)
      (org-canvas--submissions-guard-unpushed "Refresh"))
    (message "Refreshing submissions for %s..." name)
    (let ((submissions (org-canvas--submissions-fetch-with-reports id)))
      (org-canvas--submissions-display
       name id submissions view (org-canvas--submissions-fetch-assignment id)))))

;;;###autoload
(defun org-canvas-submissions-refresh ()
  "Re-fetch and re-render the submissions for the current buffer.
A grading file keeps what was typed in it — scores, Rubric rows, notes
and drafted comments — across the re-render.  The summary table does
not, so there the command asks first when it holds score edits that
were never pushed."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-refresh-buffer))

;;;###autoload
(defun org-canvas-submissions-refresh-file (&optional file)
  "Re-pull the grading file FILE from Canvas and return its buffer.
FILE is what `org-canvas-open-submissions' accepts; nil asks for one.
The file is visited with the mode and its context set, re-rendered as
`org-canvas-submissions-refresh' does — what was typed in it carried
over — and saved, so a script can go on in the buffer:

  (with-current-buffer (org-canvas-submissions-refresh-file \"Journal_02\")
    (org-canvas-submissions-download-all-attachments))

\(issue #280)."
  (interactive)
  (with-current-buffer (org-canvas--submissions-visit-grading-file
                        (org-canvas--submissions-grading-file-path file))
    (org-canvas--submissions-refresh-buffer)))

(defun org-canvas--submissions-display (assignment-name assignment-id submissions view &optional assignment)
  "Show SUBMISSIONS for ASSIGNMENT-NAME (ASSIGNMENT-ID) in VIEW.
The detail view is the grading file under the submissions directory,
rendered and saved; the summary view is an ephemeral buffer.
ASSIGNMENT, the Canvas assignment object when at hand, supplies the
rubric header of the detail view.  The Comment Bank section, notes,
drafted comments, Rubric rows, typed scores and sent comments changed
but not pushed already in the file are carried over to the new
render, a departed student's heading stays when it holds any of them,
and what changed since the last render is reported
\(`org-canvas--submissions-report-changes') and kept in
`org-canvas-submissions--last-refresh'.  Return the buffer."
  (let ((buf (if (eq view 'detail)
                 (org-canvas--submissions-grading-buffer
                  assignment-name assignment-id)
               (get-buffer-create (format "*submissions: %s*" assignment-name)))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (let* ((inhibit-read-only t)
             (detail (eq view 'detail))
             (previous (and detail (org-canvas--submissions-collect-previous)))
             (carry (and detail (org-canvas--submissions-collect-carryover)))
             (comments (and detail (org-canvas--submissions-comment-edits-by-student)))
             (bank (and detail (org-canvas--submissions-bank-carryover)))
             (changed nil))
        (if (not detail)
            (org-canvas--submissions-render-summary
             assignment-name assignment-id submissions)
          (org-canvas--submissions-render-detail
           assignment-name assignment-id
           (append submissions
                   (org-canvas--submissions-departed-entries previous carry submissions))
           assignment)
          (when bank
            (org-canvas--submissions-restore-bank bank))
          (org-canvas--submissions-restore-carryover carry)
          (org-canvas--submissions-restore-comments comments)
          (setq changed (org-canvas--submissions-report-changes
                         assignment-name previous submissions carry
                         assignment-id)))
        (goto-char (point-min))
        (setq-local org-canvas-submissions--assignment-name assignment-name)
        (setq-local org-canvas-submissions--assignment-id assignment-id)
        (setq-local org-canvas-submissions--data submissions)
        (setq-local org-canvas-submissions--current-view view)
        (setq-local org-canvas-submissions--last-refresh changed)
        (setq-local org-canvas-submissions--original-scores
                    (org-canvas--submissions-snapshot-scores submissions))
        (org-canvas-submissions-mode 1)
        (when buffer-file-name
          (save-buffer))))
    (switch-to-buffer buf)
    buf))

(defun org-canvas--submissions-grading-buffer (assignment-name
                                               &optional assignment-id)
  "Return the buffer visiting ASSIGNMENT-NAME's grading file.
The file is the one whose header names ASSIGNMENT-ID, renamed when the
column was (`org-canvas--submissions-grading-file-for', issue #431),
else the name's.  Create the submissions directory as needed.  An
existing file is not guarded: whatever was typed in it is carried over
the re-render (`org-canvas--submissions-collect-carryover')."
  (org-canvas--submissions-ensure-directory)
  (org-canvas--find-file-noselect
   (org-canvas--submissions-grading-file-for assignment-name assignment-id)))


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

(defconst org-canvas--submissions-clear-words '("none" "-")
  "SCORE values that ask the push to take away the grade Canvas holds.
Compared in lower case after trimming.  An absent or blank SCORE is
not one: it leaves the grade alone (issue #417).")

(defun org-canvas--submissions-clear-score-p (text)
  "Return non-nil when TEXT, a typed SCORE, asks to clear the grade."
  (and (stringp text)
       (member (downcase (string-trim text)) org-canvas--submissions-clear-words)
       t))

(defun org-canvas--submissions-typed-score (text old-score name)
  "Return the score the typed SCORE TEXT asks Canvas to hold for NAME.
An absent or blank TEXT is OLD-SCORE, the baseline, so no SCORE leaves
the grade alone; a clear word (see
`org-canvas--submissions-clear-words') is nil, no grade.  Anything else
is read by `org-canvas--submissions-parse-score', and a value it cannot
read is a `user-error' naming NAME, since sending it would send no
grade at all (issue #417)."
  (cond ((or (null text) (string-empty-p (string-trim text))) old-score)
        ((org-canvas--submissions-clear-score-p text) nil)
        ((org-canvas--submissions-parse-score text))
        (t (user-error "%s: SCORE %S is not a number, EX, or none to clear the grade"
                       name text))))

(defun org-canvas--submissions-clear-change (old-score new-score)
  "Return (:clear t) when NEW-SCORE takes away OLD-SCORE, else nil.
A clear where Canvas holds no grade is no change at all."
  (and old-score (null new-score) (list :clear t)))

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
Rubric table is compared with its CANVAS_RUBRIC the same way.  A
student who left the course is never a diff: Canvas has no submission
of theirs to grade any more (issue #282)."
  (let ((changes nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let ((change (and (not (org-canvas--submissions-left-p))
                           (org-canvas--submissions-detail-change-at-point))))
          (when change
            (push change changes)))
        (forward-line 1)))
    (nreverse changes)))

(defun org-canvas--submissions-detail-change-at-point ()
  "Return the grade change plist for the heading at point, or nil.
A change is a SCORE that differs from its baseline, a Rubric table
that differs from its baseline, a LATE_STATUS that differs from its
baseline, or any of them together.  An absent SCORE is no change and
a clear word is one, marked :clear (see
`org-canvas--submissions-typed-score'); the rubric keys are those of
`org-canvas--submissions-rubric-change-at-point', the late status keys
those of `org-canvas--submissions-late-status-change-at-point',
:score-derived marks a score the rubric's total set
\(`org-canvas--submissions-rubric-derived-score'), and :grade-after
one sent after the assessment (`org-canvas--submissions-grade-after')."
  (let* ((user-id-str (org-entry-get (point) "USER_ID"))
         (user-id (when user-id-str (string-to-number user-id-str)))
         (name (org-get-heading t t t t))
         (baseline (org-entry-get (point) "CANVAS_SCORE"))
         (old-score (if baseline
                        (org-canvas--submissions-parse-score baseline)
                      (alist-get user-id org-canvas-submissions--original-scores)))
         (new-score (org-canvas--submissions-typed-score
                     (org-entry-get (point) "SCORE") old-score name))
         (attempt (org-entry-get (point) "ATTEMPT"))
         (rubric (and user-id (org-canvas--submissions-rubric-change-at-point name)))
         (late (and user-id (org-canvas--submissions-late-status-change-at-point name)))
         (derived (org-canvas--submissions-rubric-derived-score
                   rubric old-score new-score name))
         (score (if derived (plist-get derived :new-score) new-score)))
    (when (and user-id (or rubric late (not (equal new-score old-score))))
      (append (list :user-id user-id
                    :name name
                    :old-score old-score
                    :new-score score
                    :attempt (and attempt (string-to-number attempt)))
              (org-canvas--submissions-clear-change old-score new-score)
              derived
              (org-canvas--submissions-grade-after rubric score)
              rubric
              late))))

(defun org-canvas--submissions-collect-summary-changes ()
  "Return grade diffs from summary view by parsing org-table rows.
A blank Score cell leaves the grade alone and a clear word takes it
away, as SCORE does in the grading file."
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
                   (user-id (and (not (equal name "Student"))
                                 (cdr (assoc name name-to-uid))))
                   (old-score (when user-id
                                (alist-get user-id
                                           org-canvas-submissions--original-scores)))
                   (new-score (and user-id
                                   (org-canvas--submissions-typed-score
                                    score-cell old-score name))))
              (when (and user-id (not (equal new-score old-score)))
                (push (append (list :user-id user-id :name name
                                    :old-score old-score :new-score new-score)
                              (org-canvas--submissions-clear-change old-score new-score))
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

;; What Canvas does with a rubric assessment (issue #444, read back from
;; a Test Student on 2026-10-01).  On an assignment whose rubric is
;; used for grading, Canvas sets the grade from the assessment, and an
;; assessment none of whose rows carries points sets it to no grade:
;; comment-only rows leave an ungraded submission ungraded, clear a
;; score already there, and discard a `posted_grade' sent in the same
;; grade_data entry or PUT, while the bulk job still reports
;; `completed'.  A `posted_grade' sent alone afterwards sticks and keeps
;; the comments.  So a change carrying such an assessment and a grade
;; to keep -- a typed one, or the one Canvas already holds -- is sent
;; in two requests, the assessment first and the grade after it
;; (`org-canvas--submissions-grade-after', `:grade-after').  The
;; existing score is resent rather than warned about: the file shows it
;; unchanged, so keeping it is what the grader asked for, and a
;; baseline stamped over a score Canvas cleared would hide the loss
;; until the next refresh.  An assessment with points is unaffected:
;; Canvas derives the grade from them, and the push sends the rows'
;; total with them.

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
`posted_grade' when the score moves, the empty string Canvas reads as
no grade when CHANGE clears it (issue #417), `rubric_assessment' when
the Rubric rows did; the bulk endpoint takes the entry as is and the
single PUT nests the grade under `submission'.  A CHANGE marked
:grade-after sends its grade later, in
`org-canvas--submissions-trailing-grade-fields' (issue #444)."
  (append (and (org-canvas--submissions-change-sends-grade-p change)
               (not (plist-get change :grade-after))
               `((posted_grade . ,(if (plist-get change :clear)
                                      ""
                                    (plist-get change :new-score)))))
          (and (plist-get change :triples)
               `((rubric_assessment
                  . ,(org-canvas--submissions-rubric-payload (plist-get change :triples)))))))

(defun org-canvas--submissions-trailing-grade-fields (change)
  "Return the grade CHANGE sends after its rubric assessment, or nil.
Only a CHANGE marked :grade-after has one: its score, sent alone once
the assessment that would have discarded it is stored (issue #444)."
  (and (plist-get change :grade-after)
       `((posted_grade . ,(plist-get change :new-score)))))

(defun org-canvas--submissions-push-single-grade (assignment-id change &optional fields-fn)
  "Push CHANGE for its student on ASSIGNMENT-ID via PUT.
FIELDS-FN picks the fields from CHANGE, by default
`org-canvas--submissions-grade-fields'."
  (let* ((url (org-canvas-api-course-endpoint
               "assignments/%s/submissions/%s" assignment-id (plist-get change :user-id)))
         (fields (funcall (or fields-fn #'org-canvas--submissions-grade-fields) change))
         (grade (assq 'posted_grade fields))
         (rubric (assq 'rubric_assessment fields)))
    (org-canvas-api-request 'PUT url
      :data (append (and grade `((submission . (,grade))))
                    (and rubric (list rubric))))))

(defun org-canvas--submissions-push-bulk-grades (assignment-id diffs &optional fields-fn)
  "Push grade DIFFS for ASSIGNMENT-ID via the bulk update_grades endpoint.
DIFFS is a list of change plists; each student's entry carries the
grade, the rubric assessment, or both, as FIELDS-FN picks them, by
default `org-canvas--submissions-grade-fields'."
  (let* ((fields-fn (or fields-fn #'org-canvas--submissions-grade-fields))
         (grade-data
          (mapcar (lambda (ch)
                    (cons (number-to-string (plist-get ch :user-id))
                          (funcall fields-fn ch)))
                  diffs))
         (url (org-canvas-api-course-endpoint
               "assignments/%s/submissions/update_grades" assignment-id)))
    (org-canvas-api-request 'POST url
      :data `((grade_data . ,grade-data)))))

;;;; Bulk Grade Progress (issue #382)
;;
;; `update_grades' answers with a Progress object and applies the grades
;; in a background job.  The push waits for that job before it records
;; a baseline or offers to post: a baseline stamped for a grade Canvas
;; never stored is one the next push no longer sends.

(defun org-canvas--submissions-progress-url (id)
  "Return the address of the Canvas Progress object ID.
Built on `org-canvas-base-url', never taken from the reply's `url', so
the token only ever travels to the configured instance."
  (format "%s/api/v1/progress/%s"
          (replace-regexp-in-string "/+\\'" "" org-canvas-base-url) id))

(defun org-canvas--submissions-progress-state (progress)
  "Return the state of PROGRESS: `completed', `failed' or `pending'.
Nil when PROGRESS is no Progress object: an id and a workflow_state."
  (let ((state (and (consp progress) (alist-get 'id progress)
                    (alist-get 'workflow_state progress))))
    (cond ((not (stringp state)) nil)
          ((equal state "completed") 'completed)
          ((equal state "failed") 'failed)
          (t 'pending))))

(defun org-canvas--submissions-progress-read (id)
  "Read the Progress object ID again; return it, or nil when the read fails.
A failed read is logged; the caller reports the job unconfirmed."
  (condition-case err
      (org-canvas-api-request 'GET (org-canvas--submissions-progress-url id))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Submissions] Could not read progress %s: %s" id (error-message-string err))
     nil)))

(defun org-canvas--submissions-progress-outcome (state progress waited)
  "Return the outcome plist for a job in STATE after WAITED seconds.
PROGRESS is the last Progress object read, whose `message' a failed
job's outcome carries."
  (pcase state
    ('completed (list :state 'completed))
    ('failed (list :state 'failed
                   :message (or (org-canvas--alist-get-non-null 'message progress)
                                "Canvas gave no reason")))
    ('pending (list :state 'unconfirmed
                    :message (format "still running after %ds" (round waited))))
    (_ (list :state 'unconfirmed
             :message (if (zerop waited)
                          "Canvas answered with no progress to follow"
                        "the progress could not be read")))))

(defun org-canvas--submissions-await-progress (progress what)
  "Wait for the Canvas job PROGRESS describes; return how it ended.
PROGRESS is the Progress object a bulk request answered, and WHAT names
the job in the echo area.  The job is read again every
`org-canvas-submissions-progress-interval' seconds (through
`org-canvas--wait' and the request pacing) until it has completed or
failed, or `org-canvas-submissions-progress-timeout' seconds have been
waited.  Return a plist (:state STATE :message WHY): STATE is
`completed', `failed' (WHY is Canvas's message) or `unconfirmed' (WHY
says whether the wait ran out or the progress could not be read)."
  (let ((id (and (consp progress) (alist-get 'id progress)))
        (state (org-canvas--submissions-progress-state progress))
        (step (let ((i org-canvas-submissions-progress-interval))
                (if (and (numberp i) (> i 0)) i 1)))
        (waited 0))
    (while (and (eq state 'pending)
                (< waited org-canvas-submissions-progress-timeout))
      (message "Waiting for Canvas to apply %s (%ds)..." what (round waited))
      (org-canvas--wait step)
      (setq waited (+ waited step)
            progress (org-canvas--submissions-progress-read id)
            state (org-canvas--submissions-progress-state progress)))
    (org-canvas--submissions-progress-outcome state progress waited)))

(defun org-canvas--submissions-live-baselines (assignment-id)
  "Fetch ASSIGNMENT-ID's submissions as (user-id . (score attempt rubric late)).
The score is spelled as the buffer shows it (a number or EX), or nil
when ungraded; the rubric is the CANVAS_RUBRIC digest of the
assessment Canvas holds, or nil; late is its late policy status, or
nil."
  (mapcar (lambda (sub)
            (cons (org-canvas--submissions-user-id sub)
                  (list (org-canvas--submissions-shown-score sub)
                        (alist-get 'attempt sub)
                        (org-canvas--submissions-submission-rubric-digest sub)
                        (org-canvas--submissions-late-status sub))))
          (org-canvas--submissions-fetch-for-assignment assignment-id)))

(defun org-canvas--submissions-conflict-p (change live)
  "Return why CHANGE conflicts with LIVE Canvas state, or nil.
LIVE is (score attempt rubric late) for the same student, or nil if
gone.  The rubric is compared only when CHANGE sends one, and the late
status only when CHANGE sets one.  The reason names
what moved and what Canvas holds — `score: Canvas has 93' — and is
the CONFLICT value; a property is a slot for a short value, and the
way out (`org-canvas-submissions-take-canvas' or
`org-canvas-submissions-keep-mine', issue #440) is the push's message
and the manual's (issue #264)."
  (let ((live-score (nth 0 live))
        (live-attempt (nth 1 live))
        (live-rubric (nth 2 live))
        (attempt (plist-get change :attempt)))
    (cond ((not (equal live-score (plist-get change :old-score)))
           (format "score: Canvas has %s" (or live-score "no grade")))
          ((and attempt (numberp live-attempt) (/= attempt live-attempt))
           (format "attempt: Canvas has %s" live-attempt))
          ((and (plist-get change :triples)
                (not (equal live-rubric (plist-get change :old-rubric))))
           "rubric: assessed on Canvas since the pull")
          ((and (plist-get change :late-status)
                (not (equal (nth 3 live) (plist-get change :old-late-status))))
           (format "late status: Canvas has %s" (or (nth 3 live) "no status"))))))

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
  "Write a CONFLICT property on the heading of each of CONFLICTS.
Under `org-canvas--dry-run' nothing is written: a dry run changes
nothing in the file (issue #442)."
  (save-excursion
    (dolist (c (unless org-canvas--dry-run conflicts))
      (when (org-canvas--submissions-goto-user (plist-get c :user-id))
        (org-entry-put (point) "CONFLICT" (plist-get c :conflict))))))

(defun org-canvas--submissions-conflict-names (conflicts)
  "Return the student names of CONFLICTS, joined for a message."
  (mapconcat (lambda (c) (plist-get c :name)) conflicts "; "))

(defun org-canvas--submissions-conflicts-note (conflicts)
  "Return the note a push message ends with for CONFLICTS, or an empty string.
It names the students held back and carries the way out of a
CONFLICT, which the property value does not (issue #264): the two
commands of issue #440."
  (if conflicts
      (format "; %d conflict(s) not sent (%s): resolve each with t (take Canvas's) or k (keep mine)"
              (length conflicts) (org-canvas--submissions-conflict-names conflicts))
    ""))

(defun org-canvas--submissions-conflict-result (conflicts)
  "Return the plist a push reports CONFLICTS under.
:conflicts counts the headings held back, those already marked
CONFLICT and those the push's own check found; :conflict-names lists
their students' names."
  (list :conflicts (length conflicts)
        :conflict-names (mapcar (lambda (c) (plist-get c :name)) conflicts)))

;;;; Resolving a Conflict (issue #440)

;; A heading marked CONFLICT shows the grader's typed values against a
;; Canvas that moved since they were typed: a refresh found a newer
;; SpeedGrader grade, assessment or late status under them, or a
;; resubmission on a graded row, or the push's own check did.  Nothing
;; of such a heading is pushed — score, Rubric rows, late status or
;; drafted comment — until the grader says which side wins: take
;; Canvas's, or keep mine.  Both read the student's submission first,
;; so the baselines they write are Canvas's now and not the file's.

(defun org-canvas--submissions-conflicted-headings ()
  "Return (USER-ID NAME REASON) for every student heading marked CONFLICT.
A student who left the course is passed over; nothing is pushed for
them anyway."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:CONFLICT:" nil t)
        (org-back-to-heading t)
        (let ((user-id (org-entry-get (point) "USER_ID"))
              (reason (org-entry-get (point) "CONFLICT")))
          (when (and user-id reason (= (org-current-level) 1)
                     (not (org-canvas--submissions-left-p)))
            (push (list (string-to-number user-id) (org-get-heading t t t t) reason)
                  found)))
        (org-end-of-meta-data)))
    (nreverse found)))

(defun org-canvas--submissions-hold-conflicted (changes drafts)
  "Hold back what the headings marked CONFLICT would send.
CHANGES are the grade changes and DRAFTS the drafted comments.
Return (CHANGES DRAFTS HELD): the grade changes and drafted comments
still to send, and one plist (:user-id :name :conflict :held t) per
heading that had something to send and was held."
  (let* ((marked (org-canvas--submissions-conflicted-headings))
         (held-p (lambda (x) (assoc (plist-get x :user-id) marked)))
         (ids (delete-dups (mapcar (lambda (x) (plist-get x :user-id))
                                   (seq-filter held-p (append changes drafts))))))
    (list (seq-remove held-p changes)
          (seq-remove held-p drafts)
          (mapcar (lambda (id)
                    (let ((m (assoc id marked)))
                      (list :user-id id :name (nth 1 m) :conflict (nth 2 m) :held t)))
                  ids))))

(defun org-canvas--submissions-fetch-student (assignment-id user-id)
  "Return USER-ID's submission to ASSIGNMENT-ID, read from Canvas now."
  (org-canvas-api-request
   'GET (org-canvas-api-course-endpoint "assignments/%s/submissions/%s"
                                        assignment-id user-id)
   :params '(("include[]" . "rubric_assessment"))))

(defun org-canvas--submissions-put-or-delete (property value)
  "Set PROPERTY of the entry at point to VALUE, or delete it when VALUE is nil."
  (if value
      (org-entry-put (point) property value)
    (org-entry-delete (point) property)))

(defun org-canvas--submissions-rebaseline-at-point (submission)
  "Make SUBMISSION, just read from Canvas, the baselines of the entry at point.
CANVAS_SCORE, CANVAS_RUBRIC, CANVAS_LATE_STATUS and ATTEMPT follow it
and CONFLICT goes; the typed SCORE, rows and LATE_STATUS stay."
  (let ((attempt (alist-get 'attempt submission)))
    (org-canvas--submissions-put-or-delete
     "CANVAS_SCORE" (org-canvas--submissions-shown-score submission))
    (org-canvas--submissions-put-or-delete
     "CANVAS_RUBRIC" (org-canvas--submissions-submission-rubric-digest submission))
    (org-canvas--submissions-put-or-delete
     "CANVAS_LATE_STATUS" (org-canvas--submissions-late-status submission))
    (when (numberp attempt)
      (org-entry-put (point) "ATTEMPT" (format "%d" attempt)))
    (org-entry-delete (point) "CONFLICT")))

(defun org-canvas--submissions-typed-record ()
  "Return the grader's typed grade of the entry at point as one line, or nil.
SCORE, LATE_STATUS and every Rubric row with a score or a comment, as
typed; nil when none of them is present."
  (let* ((score (org-entry-get (point) "SCORE"))
         (late (org-entry-get (point) "LATE_STATUS"))
         (rows (seq-filter (lambda (r) (or (nth 3 r) (nth 4 r)))
                           (org-canvas--submissions-rubric-rows)))
         (parts (delq nil
                      (list (and score (format "SCORE %s" score))
                            (and late (format "LATE_STATUS %s" late))
                            (and rows
                                 (concat "Rubric "
                                         (mapconcat #'org-canvas--submissions-describe-row
                                                    rows ", ")))))))
    (and parts (string-join parts "; "))))

(defun org-canvas--submissions-describe-row (row)
  "Return ROW, a Rubric row, as `ID SCORE (COMMENT)' on one line."
  (concat (nth 0 row) " " (or (nth 3 row) "-")
          (if (nth 4 row)
              (format " (%s)" (replace-regexp-in-string "\n+" " " (nth 4 row)))
            "")))

(defun org-canvas--submissions-take-canvas-at-point (submission)
  "Make the entry at point show SUBMISSION's grade, as just read from Canvas.
SCORE, LATE_STATUS and the Rubric rows follow Canvas, the baselines
too (`org-canvas--submissions-rebaseline-at-point'), and what was typed
is kept as one line under Notes, so nothing the grader wrote is lost."
  (let ((typed (org-canvas--submissions-typed-record))
        (assessment (org-canvas--submissions-assessment submission)))
    (org-canvas--submissions-rebaseline-at-point submission)
    (org-canvas--submissions-put-or-delete
     "SCORE" (org-canvas--submissions-shown-score submission))
    (org-canvas--submissions-put-or-delete
     "LATE_STATUS" (org-canvas--submissions-late-status submission))
    (dolist (row (org-canvas--submissions-rubric-rows))
      (let ((entry (org-canvas--submissions-assessment-entry assessment (nth 0 row))))
        (org-canvas--submissions-rubric-set-row (nth 0 row) (car entry) (cdr entry))))
    (when typed
      (save-excursion
        (goto-char (org-canvas--submissions-notes-end))
        (insert (format "- Typed before taking Canvas's grade %s: %s"
                        (format-time-string "<%Y-%m-%d %a %H:%M>") typed))
        (unless (looking-at-p "\n") (insert "\n"))))))

(defun org-canvas--submissions-student-at-point ()
  "Return (USER-ID . NAME) of the student heading point is in.
Point may be anywhere in the student's subtree.  Outside a grading
file's detail view, or outside a student heading, signal a
`user-error'.  USER-ID is a number."
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (save-excursion
    (unless (org-before-first-heading-p)
      (org-back-to-heading t)
      (while (> (org-current-level) 1) (outline-up-heading 1 t)))
    (let ((user-id (and (org-at-heading-p) (org-entry-get (point) "USER_ID"))))
      (unless user-id
        (user-error "Not on a student's heading"))
      (cons (string-to-number user-id) (org-get-heading t t t t)))))

(defun org-canvas--submissions-resolve-conflict (take)
  "Resolve the CONFLICT of the student heading at point; TAKE picks the side.
With TAKE non-nil Canvas's grade replaces the typed one
\(`org-canvas--submissions-take-canvas-at-point'); otherwise the typed
one stays and Canvas's becomes its baseline, so the next push sends
it.  The student's submission is read first; the file is saved.
Return the student's name."
  (save-excursion
    (let* ((student (org-canvas--submissions-student-at-point))
           (user-id (car student))
           (name (cdr student))
           (inhibit-read-only t))
      (org-canvas--submissions-goto-user user-id)
      (unless (org-entry-get (point) "CONFLICT")
        (user-error "No CONFLICT on the heading at point"))
      (let ((submission (org-canvas--submissions-fetch-student
                         org-canvas-submissions--assignment-id user-id)))
        (if take
            (org-canvas--submissions-take-canvas-at-point submission)
          (org-canvas--submissions-rebaseline-at-point submission)))
      (when buffer-file-name
        (org-canvas--save-buffer))
      (message (if take "%s now shows Canvas's grade; what was typed is under Notes"
                 "%s keeps the typed grade; the next push sends it")
               name)
      name)))

;;;###autoload
(defun org-canvas-submissions-take-canvas ()
  "Resolve the CONFLICT at point by taking Canvas's grade.
The student's submission is read; SCORE, LATE_STATUS and the Rubric
rows become Canvas's, as do their baselines, CONFLICT is cleared, and
the values that were typed are kept as one line under Notes (issue
#440).  A drafted comment stays drafted.  Return the student's name."
  (interactive)
  (org-canvas--submissions-resolve-conflict t))

;;;###autoload
(defun org-canvas-submissions-keep-mine ()
  "Resolve the CONFLICT at point by keeping the typed grade.
The student's submission is read and becomes the heading's baseline
\(CANVAS_SCORE, CANVAS_RUBRIC, CANVAS_LATE_STATUS, ATTEMPT), the typed
SCORE, Rubric rows and LATE_STATUS stay, and CONFLICT is cleared, so
the next push sends them over Canvas's grade (issue #440).  Return the
student's name."
  (interactive)
  (org-canvas--submissions-resolve-conflict nil))

(defun org-canvas--submissions-describe-rubric (change)
  "Return the note CHANGE's rubric rows add to its line, or an empty string."
  (if (plist-get change :triples)
      (format " (rubric %d/%d)" (plist-get change :filled) (plist-get change :of))
    ""))

(defun org-canvas--submissions-describe-new-score (change)
  "Return the new score of CHANGE, spelled for its line.
A cleared grade reads clear, never nil (issue #417)."
  (cond ((plist-get change :clear) "clear")
        ((plist-get change :new-score))
        (t "nil")))

(defun org-canvas--submissions-describe-changes (diffs)
  "Return DIFFS as one line per student: old and new score, rubric, lateness."
  (mapconcat (lambda (ch)
               (format "  %s: %s → %s%s%s"
                       (plist-get ch :name)
                       (or (plist-get ch :old-score) "nil")
                       (org-canvas--submissions-describe-new-score ch)
                       (org-canvas--submissions-describe-rubric ch)
                       (org-canvas--submissions-describe-late ch)))
             diffs "\n"))

(defun org-canvas--submissions-send-batch (assignment-id diffs fields-fn what)
  "Send the FIELDS-FN fields of DIFFS for ASSIGNMENT-ID; return the outcome.
One diff goes as a PUT, which is `completed' once it returns; several
go through the bulk endpoint, whose background job is waited for
\(`org-canvas--submissions-await-progress', issue #382).  WHAT names
the diffs in the echo area while the job runs."
  (if (cdr diffs)
      (org-canvas--submissions-await-progress
       (org-canvas--submissions-push-bulk-grades assignment-id diffs fields-fn)
       (format "%d %s to assignment %s" (length diffs) what assignment-id))
    (org-canvas--submissions-push-single-grade assignment-id (car diffs) fields-fn)
    (list :state 'completed)))

(defun org-canvas--submissions-send-grades (assignment-id diffs)
  "Send DIFFS for ASSIGNMENT-ID: one PUT, or the bulk endpoint for several.
A diff that only sets a late status has no grade field, and is left
to `org-canvas--submissions-send-late-statuses'.  The grades of diffs
marked :grade-after follow in a second send, once the first has
landed, since Canvas discards a grade sent beside a rubric assessment
without points (issue #444).  Return nil when no diff carries a grade,
else the outcome plist (:state STATE :message WHY) of
`org-canvas--submissions-send-batch', the second send's when there is
one.  Under `org-canvas--dry-run' nothing is sent and STATE is
`dry-run'."
  (let* ((grading (seq-filter #'org-canvas--submissions-grade-fields diffs))
         (after (seq-filter #'org-canvas--submissions-trailing-grade-fields grading)))
    (cond ((null grading) nil)
          (org-canvas--dry-run
           (org-canvas--log-info org-canvas--logger
             "[DRY-RUN] Would send %d grade(s) for assignment %s%s%s"
             (length grading) assignment-id
             (if (cdr grading)
                 " through update_grades, a Canvas background job the push would wait for"
               "")
             (if after
                 (format ", then %d grade(s) after their rubric assessments" (length after))
               ""))
           (list :state 'dry-run))
          (t
           (let ((outcome (org-canvas--submissions-send-batch
                           assignment-id grading nil "grade(s)")))
             (if (and after (org-canvas--submissions-grades-applied-p outcome))
                 (org-canvas--submissions-send-batch
                  assignment-id after #'org-canvas--submissions-trailing-grade-fields
                  "grade(s) after their rubric assessments")
               outcome))))))

(defun org-canvas--submissions-grades-applied-p (outcome)
  "Return non-nil if the grades behind OUTCOME, from the grade send, landed.
Nil OUTCOME means no grade was sent, which leaves nothing unconfirmed."
  (memq (plist-get outcome :state) '(nil completed)))

(defun org-canvas--submissions-grades-note (count outcome)
  "Return the push message's opening for COUNT grades sent with OUTCOME."
  (pcase (plist-get outcome :state)
    ('dry-run (format "Dry run: would push %d grade(s)" count))
    ('failed (format "Canvas did not apply %d grade(s) (%s); nothing recorded"
                     count (plist-get outcome :message)))
    ('unconfirmed (format "%d grade(s) sent but not confirmed (%s); nothing recorded, pull to check"
                          count (plist-get outcome :message)))
    (_ (format "Pushed %d grade(s)" count))))

(defun org-canvas--submissions-record-late-only (diffs late)
  "Record the late statuses LATE of DIFFS whose grades did not land.
LATE is the alist of `org-canvas--submissions-send-late-statuses';
each status Canvas stored becomes its heading's baseline, while the
score and rubric baselines stay as they were."
  (save-excursion
    (dolist (ch diffs)
      (let ((entry (assoc (plist-get ch :user-id) late)))
        (when (and entry (not (eq (cdr entry) 'dry-run))
                   (org-canvas--submissions-goto-user (plist-get ch :user-id)))
          (org-canvas--submissions-record-late-status ch (cdr entry))))))
  (when buffer-file-name
    (org-canvas--save-buffer)))

(defun org-canvas--submissions-record-pushed-at-point (change &optional late)
  "Make CHANGE the baseline of the heading at point.
CANVAS_SCORE follows the score, SCORE too when the rubric derived it;
a cleared grade takes both away, as a pull of an ungraded row shows
it (issue #417).  CANVAS_RUBRIC follows the rows sent, and CONFLICT
is cleared.  LATE is the entry
`org-canvas--submissions-send-late-statuses' made for the student,
\(USER-ID . STORED), when CHANGE set a late status and the request
went through; CANVAS_LATE_STATUS then follows what Canvas
stored.  Without it — the request failed, or it was a dry run — the
late status stays a change for the next push."
  (let ((score (plist-get change :new-score)))
    (when (and late (not (eq (cdr late) 'dry-run)))
      (org-canvas--submissions-record-late-status change (cdr late)))
    (if score
        (org-entry-put (point) "CANVAS_SCORE" score)
      (org-entry-delete (point) "CANVAS_SCORE"))
    (when (plist-get change :clear)
      (org-entry-delete (point) "SCORE"))
    (when (plist-get change :score-derived)
      (org-entry-put (point) "SCORE" score))
    (when (plist-get change :new-rubric)
      (org-entry-put (point) "CANVAS_RUBRIC" (plist-get change :new-rubric)))
    (org-entry-delete (point) "CONFLICT")))

(defun org-canvas--submissions-record-pushed (diffs &optional late)
  "Make DIFFS the new baseline: snapshot, the heading properties, and the file.
LATE is the alist of late statuses sent, (USER-ID . STORED) each; see
`org-canvas--submissions-record-pushed-at-point'."
  (dolist (ch diffs)
    (setf (alist-get (plist-get ch :user-id) org-canvas-submissions--original-scores)
          (plist-get ch :new-score)))
  (when (eq org-canvas-submissions--current-view 'detail)
    (save-excursion
      (dolist (ch diffs)
        (when (org-canvas--submissions-goto-user (plist-get ch :user-id))
          (org-canvas--submissions-record-pushed-at-point
           ch (assoc (plist-get ch :user-id) late)))))
    (org-canvas--submissions-refresh-links))
  (when buffer-file-name
    (save-buffer)))

(defun org-canvas--submissions-post-draft (assignment-id draft)
  "Post DRAFT to ASSIGNMENT-ID and record it under its student's Comments.
The draft is reset to the template once its comment is posted.  Under
`org-canvas--dry-run' nothing is sent and nothing written: the draft
stays where it is, for the real push to send (issue #442)."
  (if org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger
        "[DRY-RUN] Would post a comment for %s" (plist-get draft :name))
    (org-canvas--submissions-post-comment
     assignment-id (plist-get draft :user-id) (plist-get draft :text))
    (save-excursion
      (when (org-canvas--submissions-goto-user (plist-get draft :user-id))
        (org-canvas--submissions-append-comment-to-buffer
         (plist-get draft :name) (plist-get draft :text))
        (org-canvas--submissions-reset-draft)))))

(defun org-canvas--submissions-post-drafts (assignment-id drafts)
  "Post each of DRAFTS to ASSIGNMENT-ID, recording it under Comments as it lands.
The draft is reset to the template after its comment is posted, so a
failure midway leaves the file accurate: posted comments are recorded,
unposted ones still drafted.  Return the number posted, or under
`org-canvas--dry-run' the number a push would post."
  (dolist (d drafts)
    (org-canvas--submissions-post-draft assignment-id d))
  (length drafts))

(defun org-canvas--submissions-describe-grade-changes (diffs)
  "Return the confirmation's words for the grade DIFFS, or nil without any.
Rubric assessments are counted, and those scoring only some of their
criteria named, since Canvas accepts a partial assessment; so are the
late statuses set and the grades cleared (issue #417)."
  (when diffs
    (let ((rubrics (cl-count-if (lambda (ch) (plist-get ch :triples)) diffs))
          (lates (cl-count-if (lambda (ch) (plist-get ch :late-status)) diffs))
          (clears (cl-count-if (lambda (ch) (plist-get ch :clear)) diffs))
          (partial (cl-count-if (lambda (ch)
                                  (and (plist-get ch :triples)
                                       (< (plist-get ch :filled) (plist-get ch :of))))
                                diffs)))
      (concat (format "%d grade change(s)" (length diffs))
              (when (> rubrics 0)
                (format " (%d with rubric%s)" rubrics
                        (if (> partial 0) (format ", %d partly scored" partial) "")))
              (when (> lates 0) (format " (%d setting a late status)" lates))
              (when (> clears 0) (format " (%d clearing a grade)" clears))))))

(defun org-canvas--submissions-describe-push (diffs drafts &optional bank comments)
  "Return a one-line summary of DIFFS, DRAFTS and BANK for the confirmation.
The grade DIFFS are described by
`org-canvas--submissions-describe-grade-changes'.  BANK are the Comment
Bank items to send, and COMMENTS the sent comments to edit or delete
\(issue #419)."
  (string-join
   (delq nil
         (list (org-canvas--submissions-describe-grade-changes diffs)
               (when drafts (format "%d comment(s)" (length drafts)))
               (org-canvas--submissions-describe-comment-edits comments)
               (when bank (format "%d saved comment(s)" (length bank)))))
   " and "))

;;;###autoload
(defun org-canvas-submissions-push-grades ()
  "Push every changed score, rubric table, and drafted comment in this buffer.
In a grading file a change is a SCORE that differs from its
CANVAS_SCORE or a Rubric table whose rows differ from its
CANVAS_RUBRIC; a drafted comment is text under a student's Comment to
post heading.  A SCORE of none (or -) clears the grade Canvas holds,
while an absent or blank SCORE leaves it alone (issue #417).  A
rubric used for grading sets the score from its rows' total.  Changes
that conflict with what Canvas holds now are skipped and marked (see
`org-canvas-submissions-check-conflicts'), and a heading already
marked CONFLICT sends nothing until `org-canvas-submissions-take-canvas'
or `org-canvas-submissions-keep-mine' resolves it (issue #440); both
are named in the closing message.  After a successful push
the baselines, the comment records, and the file are updated.  A
LATE_STATUS that differs from its CANVAS_LATE_STATUS is sent too, one
GraphQL request per student, and so is every new or edited item of
the Comment Bank section (issue #352), and every sent comment of the
grader's own edited or marked DELETE under a student's Comments
heading (issue #419).  The push is confirmed through
`org-canvas--confirm'; under a manual post policy posting is offered
afterwards, interactively only.  A script calls
`org-canvas-push-submission-grades' instead (issue #381)."
  (interactive)
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (unless org-canvas-submissions--assignment-id
    (user-error "No CANVAS_ASSIGNMENT_ID in this buffer"))
  (condition-case err
      (let ((result (org-canvas--submissions-push-current t)))
        (when (org-canvas--submissions-push-landed-p result)
          (org-canvas--submissions-offer-to-post (plist-get result :pushed))))
    (user-error (signal (car err) (cdr err)))
    (error (org-canvas--user-message "Error pushing: %s" (error-message-string err)))))

;;;###autoload
(defun org-canvas-submissions-push-at-point ()
  "Push the student heading at point alone, after confirming.
Its score, Rubric rows, late status, drafted comment and sent-comment
edits go up through the push `S' runs, restricted to this student
\(`org-canvas--submissions-push-current' with ONLY, issue #441): the
same conflict check, the same question through `org-canvas--confirm',
the same baselines recorded and the file saved.  No Comment Bank item
is sent and posting is never offered, since posting would show the
whole column.  A heading marked CONFLICT sends nothing (issue #440).
Return the push's plist, nil when declined."
  (interactive)
  (let ((student (org-canvas--submissions-student-at-point)))
    (unless org-canvas-submissions--assignment-id
      (user-error "No CANVAS_ASSIGNMENT_ID in this buffer"))
    (condition-case err
        (org-canvas--submissions-push-current t (list (car student)))
      (user-error (signal (car err) (cdr err)))
      (error (org-canvas--user-message "Error pushing %s: %s"
                                       (cdr student) (error-message-string err))
             nil))))

(defun org-canvas--submissions-confirm-push (diffs drafts bank conflicts &optional comments)
  "Show the grade DIFFS and ask whether to push them with DRAFTS and BANK.
CONFLICTS are counted in the question.  COMMENTS, the sent comments
as (SENDABLE . REFUSED), are listed one per line, what will not be
sent among them, and the sendable counted in the question.  The
question goes through `org-canvas--confirm', so
`org-canvas-assume-yes' and a batch Emacs answer it yes."
  (let ((lines (delq nil (list (when diffs
                                 (concat "Grade changes:\n"
                                         (org-canvas--submissions-describe-changes diffs)))
                               (when (or (car comments) (cdr comments))
                                 (concat "Sent comments:\n"
                                         (org-canvas--submissions-list-comment-edits comments)))))))
    (when lines
      (message "%s" (string-join lines "\n"))))
  (org-canvas--confirm
   (format "Push %s%s%s? "
           (org-canvas--submissions-describe-push diffs drafts bank (car comments))
           (if conflicts
               (format ", skipping %d conflict(s)" (length conflicts))
             "")
           (if (cdr comments)
               (format ", leaving %d comment change(s) unsent" (length (cdr comments)))
             ""))))

(defun org-canvas--submissions-only-leftovers (only touched)
  "Return the ids of ONLY a push restricted to them sends nothing for.
TOUCHED are the plists, each with a :user-id, of what the push will
send, hold back or refuse.  The value is (:missing IDS :unchanged
IDS): the ids with no student heading in the buffer, and those whose
heading has nothing to send (issue #441)."
  (let ((ids (mapcar (lambda (x) (plist-get x :user-id)) touched))
        (missing nil)
        (unchanged nil))
    (dolist (id only)
      (cond ((not (save-excursion (org-canvas--submissions-goto-user id)))
             (push id missing))
            ((not (memql id ids))
             (push id unchanged))))
    (list :missing (nreverse missing) :unchanged (nreverse unchanged))))

(defun org-canvas--submissions-push-plan (assignment-id only)
  "Return what a push of the grading buffer at hand would send, as a plist.
ASSIGNMENT-ID is the column's.  ONLY nil plans the whole file; a list
of user ids plans those students' rows alone — their grade changes,
drafts and sent-comment edits, and no Comment Bank item, which belongs
to no row (issue #441).  The keys are :changes, :drafts, :bank,
:comments (SENDABLE . REFUSED), :conflicts (the headings held back as
marked CONFLICT, issue #440, and those the push's own check finds,
which are marked here) and the :missing and :unchanged of
`org-canvas--submissions-only-leftovers'."
  (pcase-let* ((`(,pending ,drafts ,held)
                (org-canvas--submissions-hold-conflicted
                 (org-canvas--submissions-only-rows
                  only (org-canvas--submissions-collect-grade-changes))
                 (org-canvas--submissions-only-rows
                  only (org-canvas--submissions-collect-comment-drafts))))
               (`(,changes . ,found)
                (org-canvas--submissions-partition-conflicts assignment-id pending))
               (conflicts (append held found))
               (comments (org-canvas--submissions-pending-comment-edits assignment-id only)))
    (org-canvas--submissions-mark-conflicts found)
    (append (list :changes changes :drafts drafts :comments comments :conflicts conflicts
                  :bank (unless only (org-canvas--submissions-bank-pending)))
            (org-canvas--submissions-only-leftovers
             only (append changes drafts (car comments) (cdr comments) conflicts)))))

(defun org-canvas--submissions-plan-sends-p (plan)
  "Return non-nil when PLAN, a push plan, has anything to send."
  (or (plist-get plan :changes) (plist-get plan :drafts) (plist-get plan :bank)
      (car (plist-get plan :comments))))

(defun org-canvas--submissions-only-note (plan)
  "Return the push message's note on PLAN's ids that sent nothing, or \"\"."
  (concat
   (if-let* ((ids (plist-get plan :missing)))
       (format "; no row for %s" (mapconcat #'number-to-string ids ", "))
     "")
   (if-let* ((ids (plist-get plan :unchanged)))
       (format "; nothing to send for %s" (mapconcat #'number-to-string ids ", "))
     "")))

(defun org-canvas--submissions-plan-result (plan)
  "Return the keys a push reports for PLAN's conflicts and restriction."
  (append (org-canvas--submissions-conflict-result (plist-get plan :conflicts))
          (list :missing (plist-get plan :missing)
                :unchanged (plist-get plan :unchanged))))

(defun org-canvas--submissions-push-current (ask &optional only)
  "Push the grading buffer at hand to Canvas; return what was done.
With ASK non-nil the push is confirmed first
\(`org-canvas--submissions-confirm-push'); nil pushes without a
question.  ONLY, a list of user ids, pushes those students' rows alone
\(`org-canvas--submissions-push-plan', issue #441).  Return nil when
the push was declined, else the plist of
`org-canvas--submissions-push-all', which with nothing to push has
:pushed 0 and :state nil.  Posting is never part of it.

A heading marked CONFLICT sends nothing — grade, Rubric rows, late
status or draft — until it is resolved (issue #440); it is counted
and named with the conflicts the push's own check finds, which are
marked."
  (org-canvas--submissions-ensure-context)
  (let ((assignment-id org-canvas-submissions--assignment-id))
    (unless assignment-id
      (user-error "No CANVAS_ASSIGNMENT_ID in this buffer"))
    (let* ((plan (org-canvas--submissions-push-plan assignment-id only))
           (comments (plist-get plan :comments)))
      (cond ((not (org-canvas--submissions-plan-sends-p plan))
             (message "Nothing to push%s%s%s"
                      (org-canvas--submissions-conflicts-note (plist-get plan :conflicts))
                      (org-canvas--submissions-refused-note (cdr comments))
                      (org-canvas--submissions-only-note plan))
             (append (list :pushed 0 :state nil :late 0 :comments 0)
                     (org-canvas--submissions-plan-result plan)
                     (org-canvas--submissions-comment-edit-result nil comments)))
            ((or (not ask)
                 (org-canvas--submissions-confirm-push
                  (plist-get plan :changes) (plist-get plan :drafts) (plist-get plan :bank)
                  (plist-get plan :conflicts) comments))
             (org-canvas--submissions-push-all assignment-id plan))))))

(defun org-canvas--submissions-push-landed-p (result)
  "Return non-nil when the push RESULT sent grades and Canvas stored them.
RESULT is the plist of `org-canvas--submissions-push-all'."
  (and (> (or (plist-get result :pushed) 0) 0)
       (eq (plist-get result :state) 'completed)))

;;;; Pushing From a Script (issue #381)

(defun org-canvas--submissions-file-assignment-id (file)
  "Return the CANVAS_ASSIGNMENT_ID in the header of the grading FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID")))

(defun org-canvas--submissions-push-target (assignment)
  "Return the path of the grading file ASSIGNMENT names.
ASSIGNMENT is whatever `org-canvas--submissions-grading-file-path'
takes: a Canvas assignment id, matched against each grading file's
CANVAS_ASSIGNMENT_ID, a path, or the file's or the assignment's name.
Nil asks for the file.  An id no file names is a `user-error'."
  (org-canvas--submissions-grading-file-path assignment))

(defun org-canvas--submissions-user-ids (only)
  "Return ONLY, user ids as integers or strings of digits, as integers.
Anything else is a `user-error' naming it, before anything is read."
  (mapcar (lambda (id)
            (cond ((natnump id) id)
                  ((and (stringp id) (string-match-p "\\`[0-9]+\\'" (string-trim id)))
                   (string-to-number id))
                  (t (user-error "%S is not a Canvas user id" id))))
          only))

;;;###autoload
(defun org-canvas-push-submission-grades (&optional assignment post only)
  "Push ASSIGNMENT's grading file to Canvas without a question; return a plist.
ASSIGNMENT is a Canvas assignment id (an integer or a string of
digits, found in the grading files' headers), or a path or a name as
`org-canvas-open-submissions' takes it; nil asks for the file.  The
file is visited with `org-canvas-submissions-mode' on and read as the
grading file it is, and everything `org-canvas-submissions-push-grades'
would send is sent, without confirming.  Grades are posted only when
POST is non-nil, and then only once Canvas has stored every grade the
push sent (issue #382): posting is what students see, so it is a
choice of its own and never follows from the push.  The file is saved,
except under `org-canvas--dry-run', which sends nothing and leaves the
file as it was: no draft consumed, no baseline or CONFLICT written
\(issue #442).

Return a plist: :pushed, the grades Canvas stored; :state, how the
grade send ended (`completed', `failed', `unconfirmed', `dry-run', or
nil when no grade was sent) with :message its reason; :late,
:comments and :conflicts, the late statuses set, the comments posted
and the headings skipped as conflicts (held as marked CONFLICT, or
found by the push's check), with :conflict-names their students'
names (issue #440); :edited, :deleted, :refused, :failed, :errored
and :dry-run, the sent comments rewritten, deleted, not sent, refused
by Canvas, failed in Emacs (issue #443) and only shown (issue #419);
:posted, non-nil when
the grades were posted.  A script that pushes a column by id and posts
it calls

  (org-canvas-push-submission-grades \"2573836\" t)

and one that only pushes leaves POST out (issue #381).

ONLY, a list of Canvas user ids (integers or strings of digits),
pushes those students' rows alone: their scores, Rubric rows, late
statuses, drafted comments and sent-comment edits, and no Comment Bank
item (issue #441).  The plist then names under :missing the ids with
no row in the file and under :unchanged those whose row had nothing
to send.  POST cannot go with ONLY, since posting shows the whole
column; that is a `user-error'.  Under `org-canvas--dry-run' the plist
carries :would-send, one (:user-id :name :line) per student, LINE the
fields that would be sent.  So a script releases two decided rows,
hidden, with

  (org-canvas-push-submission-grades \"2573836\" nil \\='(5001 5002))"
  (when (and post only)
    (user-error "Posting shows every grade of the column; push the rows with ONLY, then post separately"))
  (let ((ids (org-canvas--submissions-user-ids only))
        (buf (org-canvas--submissions-visit-grading-file
              (org-canvas--submissions-push-target assignment))))
    (with-current-buffer buf
      (setq org-canvas-submissions--current-view 'detail)
      (let* ((result (org-canvas--submissions-push-current nil ids))
             (posted (and post
                          (memq (plist-get result :state) '(nil completed))
                          (org-canvas--submissions-post-assignment
                           org-canvas-submissions--assignment-id))))
        (unless org-canvas--dry-run
          (org-canvas--save-buffer))
        (plist-put result :posted posted)))))

;;;; Pushing Only the Sent Comments (issue #425)

(defun org-canvas--submissions-send-comment-edits (assignment-id comments)
  "Send COMMENTS, (SENDABLE . REFUSED), to ASSIGNMENT-ID and save the file.
Nothing else in the file is sent.  Return the plist of
`org-canvas--submissions-comment-edit-result'."
  (let ((counts (org-canvas--submissions-apply-comment-edits assignment-id (car comments))))
    (when buffer-file-name
      (org-canvas--save-buffer))
    (org-canvas--user-message
     "Comment edits: %s"
     (string-remove-prefix
      "; " (org-canvas--submissions-comment-edits-note counts (cdr comments))))
    (org-canvas--submissions-comment-edit-result counts comments)))

(defun org-canvas--submissions-push-comment-edits-current ()
  "Push only the sent comments changed in the grading buffer at hand.
They are confirmed first, with the full push's question and listing,
through `org-canvas--confirm'.  A course marked `org-canvas-read-only' refuses
before anything is sent, unless under `org-canvas--dry-run'.  Return
nil when declined, else the plist of
`org-canvas--submissions-comment-edit-result'."
  (org-canvas--submissions-ensure-context)
  (let ((assignment-id org-canvas-submissions--assignment-id))
    (unless assignment-id
      (user-error "No CANVAS_ASSIGNMENT_ID in this buffer"))
    (let ((comments (org-canvas--submissions-pending-comment-edits assignment-id)))
      (cond ((not (car comments))
             (message "No sent comment to change%s"
                      (org-canvas--submissions-refused-note (cdr comments)))
             (org-canvas--submissions-comment-edit-result nil comments))
            ((progn (unless org-canvas--dry-run
                      (org-canvas--check-writable 'PUT "editing sent comments"))
                    (org-canvas--submissions-confirm-push nil nil nil nil comments))
             (org-canvas--submissions-send-comment-edits assignment-id comments))))))

(defun org-canvas--submissions-comment-edits-buffer (assignment)
  "Return the grading buffer for ASSIGNMENT, as the comment-only push takes it.
Nil in a grading buffer is that buffer; otherwise ASSIGNMENT is
resolved by `org-canvas--submissions-push-target' and its file visited
in the detail view."
  (if (and (null assignment) org-canvas-submissions-mode)
      (current-buffer)
    (with-current-buffer (org-canvas--submissions-visit-grading-file
                          (org-canvas--submissions-push-target assignment))
      (setq org-canvas-submissions--current-view 'detail)
      (current-buffer))))

;;;###autoload
(defun org-canvas-push-submission-comment-edits (&optional assignment)
  "Push only the sent comments rewritten or marked DELETE in a grading file.
Nothing else is sent: no score, rubric row, late status, drafted
comment or Comment Bank item, and nothing is posted (issue #425).
Each change is checked as `org-canvas-submissions-push-grades' checks
it (only your own comments, none edited on Canvas since the pull,
none gone, none emptied), listed, and confirmed through
`org-canvas--confirm', which `org-canvas-assume-yes' and a batch Emacs
answer yes.  A dry run sends nothing and changes nothing.  The new
baselines are written and the file saved.

ASSIGNMENT is nil in a grading buffer, for that buffer; else a Canvas
assignment id or a grading file's path or name, as
`org-canvas-push-submission-grades' takes it, and nil elsewhere asks
for the file.  Return nil when declined, else a plist: :edited and
:deleted, the comments rewritten and deleted; :refused, the changes
not sent; :failed, those Canvas refused; :errored, those an error in
Emacs stopped (issue #443); :dry-run, those a dry run
only showed.  A script calls

  (org-canvas-push-submission-comment-edits \"2573836\")"
  (interactive)
  (with-current-buffer (org-canvas--submissions-comment-edits-buffer assignment)
    (org-canvas--submissions-push-comment-edits-current)))

(defun org-canvas--submissions-late-note (failed)
  "Return the push message note for FAILED late statuses, or \"\"."
  (if failed
      (format "; late status not set for %s (see the log)" (string-join failed ", "))
    ""))

(defun org-canvas--submissions-would-send (changes drafts)
  "Return, and log, what a push would send per student.
CHANGES are the grade changes and DRAFTS the drafted comments.  Each
element is (:user-id ID :name NAME :line LINE), LINE naming the grade
fields as JSON (`org-canvas--submissions-grade-fields'), the late
status and the drafted comment, in the order the students come; each
LINE is logged as a [DRY-RUN] line (issue #441)."
  (let ((rows nil))
    (dolist (x (append changes drafts))
      (let ((id (plist-get x :user-id)))
        (unless (assoc id rows)
          (push (cons id (plist-get x :name)) rows))))
    (mapcar (lambda (row)
              (let* ((change (cl-find (car row) changes :key (lambda (c) (plist-get c :user-id))))
                     (draft (cl-find (car row) drafts :key (lambda (d) (plist-get d :user-id))))
                     (line (org-canvas--submissions-would-send-line (cdr row) change draft)))
                (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would send %s" line)
                (list :user-id (car row) :name (cdr row) :line line)))
            (nreverse rows))))

(defun org-canvas--submissions-would-send-line (name change draft)
  "Return one line naming what CHANGE and DRAFT would send for NAME."
  (let ((fields (and change (org-canvas--submissions-grade-fields change)))
        (after (and change (org-canvas--submissions-trailing-grade-fields change)))
        (late (plist-get change :late-status))
        (text (plist-get draft :text)))
    (concat name ": "
            (string-join
             (delq nil (list (and fields (json-encode fields))
                             (and after (concat "then " (json-encode after)))
                             (and late (format "late status %s" late))
                             (and text (format "comment %S"
                                               (replace-regexp-in-string "\n+" " " text)))))
             "; "))))

(defun org-canvas--submissions-push-all (assignment-id plan)
  "Push the contents of PLAN to ASSIGNMENT-ID, then record them.
PLAN is `org-canvas--submissions-push-plan''s: its :changes, the
grade diffs, go first, grades and rubric assessments, then their late
statuses, then the sent comments of :comments, (SENDABLE . REFUSED),
to edit or delete (issue #419), then the :drafts, then the :bank
items; the :conflicts, already marked, the refused comment changes
and the ids a restricted push sent nothing for are only named in the
closing message.  A dry run records nothing at all (issue #442), and
says per student what would be sent (issue #441).  Every baseline is
recorded only once the grades have landed: a bulk push waits for
Canvas's background job, and one that failed or ran out of time
records no score or rubric baseline, so the next push sends them
again (issue #382).

Return a plist: :pushed, the grades Canvas stored (0 when they did not
land); :state, the grade send's (`completed', `failed', `unconfirmed',
`dry-run', or nil when no grade was sent) and :message its reason;
:late and :comments, the late statuses set and the comments posted;
the keys of `org-canvas--submissions-plan-result' (:conflicts,
:conflict-names, :missing, :unchanged) and of
`org-canvas--submissions-comment-edit-result'; and under a dry run
:would-send, one (:user-id :name :line) per student.  Posting is left
to the caller."
  (let* ((changes (plist-get plan :changes))
         (drafts (plist-get plan :drafts))
         (comments (plist-get plan :comments))
         (bank (plist-get plan :bank))
         (rows (and org-canvas--dry-run
                    (org-canvas--submissions-would-send changes drafts)))
         (grading (seq-filter #'org-canvas--submissions-grade-fields changes))
         (outcome (org-canvas--submissions-send-grades assignment-id changes))
         (late (org-canvas--submissions-send-late-statuses changes))
         (edited (org-canvas--submissions-apply-comment-edits assignment-id (car comments)))
         (posted (org-canvas--submissions-post-drafts assignment-id drafts))
         (saved (and bank (org-canvas--submissions-push-bank assignment-id)))
         (applied (org-canvas--submissions-grades-applied-p outcome)))
    (cond (org-canvas--dry-run nil)
          (applied (org-canvas--submissions-record-pushed changes (car late)))
          (t (org-canvas--submissions-record-late-only changes (car late))))
    (org-canvas--user-message
     "%s%s and %d comment(s)%s%s%s%s%s"
     (org-canvas--submissions-grades-note (length grading) outcome)
     (if (car late) (format ", %d late status(es)" (length (car late))) "")
     posted
     (org-canvas--submissions-comment-edits-note edited (cdr comments))
     (org-canvas--submissions-late-note (cdr late))
     (org-canvas--submissions-describe-bank saved bank)
     (org-canvas--submissions-conflicts-note (plist-get plan :conflicts))
     (org-canvas--submissions-only-note plan))
    (append (list :pushed (if applied (length grading) 0)
                  :state (plist-get outcome :state)
                  :message (plist-get outcome :message)
                  :late (length (car late))
                  :comments posted)
            (org-canvas--submissions-plan-result plan)
            (org-canvas--submissions-comment-edit-result edited comments)
            (and rows (list :would-send rows)))))

;;;; Posting Grades

(defun org-canvas--submissions-post-manually-p ()
  "Return non-nil when this grading file's grades are held until posted.
Reads the POST_POLICY keyword the pull wrote from the assignment's
effective policy; a file without it, pulled before the keyword
existed, answers nil and nothing is offered."
  (equal (org-canvas--submissions-file-property "POST_POLICY") "manual"))

(defun org-canvas--submissions-offer-to-post (count)
  "Offer to post the COUNT grades just pushed, under a manual policy.
Under a manual post policy a pushed grade stays hidden from the
student until it is posted (issue #202).  Posting is what students
see, so it is never assumed: a batch Emacs, which cannot be asked,
leaves the grades held and says how to post them, and
`org-canvas-assume-yes' does not answer the question (issue #381)."
  (when (and (> count 0) (org-canvas--submissions-post-manually-p))
    (cond (noninteractive
           (message "Grades held under the manual post policy; post them with (org-canvas-push-submission-grades ASSIGNMENT t) or P"))
          ((y-or-n-p "This assignment posts grades manually; post them now? ")
           (org-canvas-submissions-post-grades)))))

(defconst org-canvas--submissions-post-grades-mutation
  "mutation ($assignmentId: ID!) { postAssignmentGrades(input: {assignmentId: $assignmentId}) { progress { _id state } } }"
  "The GraphQL mutation that posts an assignment's grades to its students.
Checked against the Canvas schema by the GraphQL contract test
\(issue #269), which names it by this symbol.")

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
    (org-canvas--submissions-post-assignment assignment-id)))

(defun org-canvas--submissions-post-assignment (assignment-id)
  "Post ASSIGNMENT-ID's grades; return non-nil when the request was sent.
Under `org-canvas--dry-run' nothing is sent and nil comes back."
  (let ((reply (org-canvas--graphql-mutate
                (format "post the grades of assignment %s" assignment-id)
                org-canvas--submissions-post-grades-mutation
                (list (cons 'assignmentId (format "%s" assignment-id))))))
    (unless (org-canvas--dry-run-response-p reply)
      (message "Grades posted for assignment %s." assignment-id)
      t)))

(provide 'org-canvas-submissions)
;;; org-canvas-submissions.el ends here
