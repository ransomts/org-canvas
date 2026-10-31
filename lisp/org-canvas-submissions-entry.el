;;; org-canvas-submissions-entry.el --- Pull, refresh and download into a grading file -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pulling a column writes its grading file; refreshing it re-reads
;; Canvas and rewrites what is there.  This module is that path
;; (issue #142's split of the submissions module): fetching an
;; assignment and its submissions, carrying over what a pull must not
;; lose (the scores typed since the last pull, the comment bank, the
;; sent comments, the rubric rows), pairing uploads that are the same
;; document (issue #457), and saying what a refresh changed (issue
;; #282).
;;
;; It also holds the commands that act on one heading: post a comment,
;; download that student's attachments or everyone's (issue #154), and
;; apply the completion rule that marks a student complete once their
;; work is graded and posted.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; The grading file, its buffer-local state and the minor mode it
;; is read in: this module is above that file, never required by it.
(require 'org-canvas-submissions)
;; The paired-upload reader the refresh and `u' share.
(require 'org-canvas-submissions-uploads)

;; Defined below this file (a sibling split off the same module), so
;; declared rather than required: requiring would be a cycle.
(declare-function org-canvas--submissions-bank-carryover "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas--submissions-comment-edits-by-student "org-canvas-submissions-comment-items"
                  (&optional skip-left))
(declare-function org-canvas--submissions-conflicted-headings "org-canvas-submissions-conflict"
                  ())
(declare-function org-canvas--submissions-fetch-with-reports "org-canvas-submissions-render"
                  (assignment-id))
(declare-function org-canvas--submissions-format-number "org-canvas-submissions-render"
                  (n))
(declare-function org-canvas--submissions-late-carryover "org-canvas-submissions-push"
                  ())
(declare-function org-canvas--submissions-parse-score "org-canvas-submissions-push"
                  (score-string))
(declare-function org-canvas--submissions-render-detail "org-canvas-submissions-render"
                  (assignment-name assignment-id submissions &optional assignment))
(declare-function org-canvas--submissions-render-summary "org-canvas-submissions-render"
                  (assignment-name assignment-id submissions))
(declare-function org-canvas--submissions-report-attempts "org-canvas-submissions-render"
                  (assignment-id resubmitted))
(declare-function org-canvas--submissions-restore-bank "org-canvas-submissions-comment-items"
                  (text))
(declare-function org-canvas--submissions-restore-comments "org-canvas-submissions-comment-items"
                  (carry))
(declare-function org-canvas--submissions-restore-late-status "org-canvas-submissions-push"
                  (carry))
(declare-function org-canvas--submissions-restore-rubric "org-canvas-submissions-comment-items"
                  (carry))
(declare-function org-canvas--submissions-rubric-carryover "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas--submissions-rubric-fill-full "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas--submissions-section-region "org-canvas-submissions-render"
                  (heading))
(declare-function org-canvas--submissions-section-text "org-canvas-submissions-render"
                  (heading))
(declare-function org-canvas--submissions-set-section "org-canvas-submissions-render"
                  (heading template text))
(declare-function org-canvas--submissions-snapshot-scores "org-canvas-submissions-push"
                  (submissions))
(declare-function org-canvas--submissions-user-id "org-canvas-submissions-render"
                  (submission))
(declare-function org-canvas--submissions-user-sortable-name "org-canvas-submissions-render"
                  (submission))
;; Variables another module of this split defines; values come with it.
(defvar org-canvas--submissions-comments-heading)
(defvar org-canvas--submissions-draft-heading)
(defvar org-canvas--submissions-notes-heading)

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

;;;; Shared Uploads Across a Refresh (issue #457)

;; SAME_AS names the students who uploaded the same document as the
;; one at point; `org-canvas-submissions-upload-text' writes it from
;; the downloaded files.  A refresh re-renders every heading, so the
;; value is carried over, for a student whose attempt is still the one
;; it was worked out from.  A new attempt drops it: the pairing has to
;; be worked out again from the new file.

(defun org-canvas--submissions-same-as-carryover ()
  "Return (USER-ID ATTEMPT SAME-AS) for every heading carrying SAME_AS."
  (let ((carry nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:SAME_AS:" nil t)
        (org-back-to-heading t)
        (when-let* ((user-id (org-entry-get (point) "USER_ID")))
          (push (list (string-to-number user-id)
                      (org-entry-get (point) "ATTEMPT")
                      (org-entry-get (point) "SAME_AS"))
                carry))
        (org-end-of-meta-data)))
    carry))

(defun org-canvas--submissions-restore-same-as (carry)
  "Write CARRY's SAME_AS back on the headings whose attempt is unchanged.
CARRY is what `org-canvas--submissions-same-as-carryover' read before
the re-render."
  (save-excursion
    (dolist (entry carry)
      (when (and (org-canvas--submissions-goto-user (nth 0 entry))
                 (equal (org-entry-get (point) "ATTEMPT") (nth 1 entry)))
        (org-entry-put (point) "SAME_AS" (nth 2 entry))))))

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
marked CONFLICT, are named (issue #440), and so are its :undownloaded,
the attachments listed and not on disk (issue #457)."
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
     (org-canvas--submissions-describe-conflicted (plist-get found :conflicted))
     (org-canvas--submissions-describe-undownloaded (plist-get found :undownloaded)))))

(defun org-canvas--submissions-describe-conflicted (conflicted)
  "Return the refresh line's note naming CONFLICTED students, or \"\".
CONFLICTED is a list of (USER-ID NAME REASON), as
`org-canvas--submissions-conflicted-headings' returns it (issue #440)."
  (if conflicted
      (format "; CONFLICT on %d: %s" (length conflicted)
              (mapconcat (lambda (c) (format "%s (%s)" (nth 1 c) (nth 2 c)))
                         conflicted "; "))
    ""))

(defun org-canvas--submissions-undownloaded-at-point (assignment-name)
  "Return the attachments of the student at point not on disk.
Each is (STUDENT FILENAME OLDER), OLDER non-nil when a copy under that
name is on disk but is not the attachment listed, an earlier attempt's
\(`org-canvas--submissions-stale-copy-p').  ASSIGNMENT-NAME locates the
download folder."
  (let ((name (org-get-heading t t t t)))
    (mapcar (lambda (entry)
              (list name (plist-get entry :name)
                    (file-exists-p
                     (expand-file-name
                      (plist-get entry :name)
                      (org-canvas--submissions-attachment-dir assignment-name name)))))
            (cl-remove-if (lambda (entry) (plist-get entry :local))
                          (org-canvas--submissions-attachment-entries)))))

(defun org-canvas--submissions-undownloaded (assignment-name)
  "Return the attachments this grading file lists and its folder lacks.
A list of `org-canvas--submissions-undownloaded-at-point' values over
the students still enrolled.  Nil unless ASSIGNMENT-NAME's attachment
folder exists: a column nobody downloaded is not missing anything, and
every refresh would say otherwise (issue #457)."
  (when (file-directory-p
         (expand-file-name
          (format "files/%s/" (org-canvas--submissions-sanitize-filename assignment-name))
          (org-canvas--submissions-dir)))
    (let ((missing nil))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* " nil t)
          (org-back-to-heading t)
          (when (and (org-entry-get (point) "USER_ID")
                     (not (org-canvas--submissions-left-p)))
            (setq missing (append missing (org-canvas--submissions-undownloaded-at-point
                                           assignment-name))))
          (forward-line 1)))
      missing)))

(defun org-canvas--submissions-describe-undownloaded (missing)
  "Return the refresh line's note naming MISSING attachments, or \"\".
MISSING is a list of (STUDENT FILENAME OLDER), as
`org-canvas--submissions-undownloaded' returns it (issue #457)."
  (if missing
      (format "; %d attachment(s) not on disk, D downloads them: %s"
              (length missing)
              (mapconcat (lambda (m)
                           (format "%s (%s%s)" (nth 0 m) (nth 1 m)
                                   (if (nth 2 m) ", an older copy is there" "")))
                         missing "; "))
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
      (setq changes (plist-put changes :undownloaded
                               (org-canvas--submissions-undownloaded name)))
      (org-canvas--submissions-log-changes changes)
      (when assignment-id
        (org-canvas--submissions-report-attempts
         assignment-id (plist-get changes :resubmitted)))
      (let ((line (org-canvas--submissions-describe-refresh name previous changes)))
        (message "%s" line)
        line))))

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
  ;; Before the text is read from the grader: posting a comment is a
  ;; submissions write (issue #550).
  (org-canvas--check-write-feature 'submissions "posting a comment")
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
  ;; undercover's line counts for the whole body (see
  ;; documentation/testing-guide.md).
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

;;;; Entry Points

;; Each command takes its target as an optional argument and prompts only
;; when it is nil, so a script, a keyboard macro over several columns or
;; a tool built on the package can pull, open and refresh without the
;; minibuffer (issue #280).  The prompts live in the resolvers below
;; rather than in a sexp `interactive' spec, which blanks undercover's
;; line counts for the whole body (see documentation/testing-guide.md).

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
over — and saved, so a script can go on in the buffer (issue #280):

  (with-current-buffer (org-canvas-submissions-refresh-file \"Journal_02\")
    (org-canvas-submissions-download-all-attachments))

FILE may also be a list of them: each is re-pulled in turn and the
list of their buffers returned (issue #450)."
  (interactive)
  (if (consp file)
      (mapcar #'org-canvas-submissions-refresh-file file)
    (with-current-buffer (org-canvas--submissions-visit-grading-file
                          (org-canvas--submissions-grading-file-path file))
      (org-canvas--submissions-refresh-buffer))))

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
             (same-as (and detail (org-canvas--submissions-same-as-carryover)))
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
          (org-canvas--submissions-restore-same-as same-as)
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

(provide 'org-canvas-submissions-entry)
;;; org-canvas-submissions-entry.el ends here
