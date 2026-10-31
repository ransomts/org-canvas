;;; org-canvas-submissions-conflict.el --- Find and resolve what changed on Canvas since a pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A grading file is edited offline, so Canvas may have moved on
;; before the push: a grade changed there, a rubric rewritten, a
;; student who submitted again.  Pushing over that would lose the
;; instructor's or the grader's work, so before a push the file is
;; re-read and each student whose work changed since the pull is
;; marked :CONFLICT: and skipped (issue #431's rule, issue #440's
;; resolution).
;;
;; `t' takes Canvas's version of the heading, `k' keeps the one in the
;; file; both rewrite the heading and its baseline and clear the mark,
;; so the next push goes through.  Nothing here sends anything to
;; Canvas.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; The grading file, its buffer-local state and the minor mode it
;; is read in: this module is above that file, never required by it.
(require 'org-canvas-submissions)

;; Defined below this file (a sibling split off the same module), so
;; declared rather than required: requiring would be a cycle.
(declare-function org-canvas--submissions-append-comment-to-buffer "org-canvas-submissions-entry"
                  (_user-name text))
(declare-function org-canvas--submissions-assessment "org-canvas-submissions-comment-items"
                  (submission))
(declare-function org-canvas--submissions-assessment-entry "org-canvas-submissions-comment-items"
                  (assessment criterion-id))
(declare-function org-canvas--submissions-await-progress "org-canvas-submissions-push"
                  (progress what))
(declare-function org-canvas--submissions-bank-pending "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas--submissions-collect-comment-drafts "org-canvas-submissions-render"
                  ())
(declare-function org-canvas--submissions-collect-grade-changes "org-canvas-submissions-push"
                  ())
(declare-function org-canvas--submissions-comment-edit-result "org-canvas-submissions-comment-items"
                  (counts comments))
(declare-function org-canvas--submissions-conflict-result "org-canvas-submissions-push"
                  (conflicts))
(declare-function org-canvas--submissions-conflicts-note "org-canvas-submissions-push"
                  (conflicts))
(declare-function org-canvas--submissions-describe-comment-edits "org-canvas-submissions-comment-items"
                  (edits))
(declare-function org-canvas--submissions-describe-late "org-canvas-submissions-push"
                  (change))
(declare-function org-canvas--submissions-grade-fields "org-canvas-submissions-push"
                  (change))
(declare-function org-canvas--submissions-grading-file-path "org-canvas-submissions-entry"
                  (file))
(declare-function org-canvas--submissions-late-status "org-canvas-submissions-push"
                  (submission))
(declare-function org-canvas--submissions-left-p "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas--submissions-list-comment-edits "org-canvas-submissions-comment-items"
                  (comments))
(declare-function org-canvas--submissions-mark-conflicts "org-canvas-submissions-push"
                  (conflicts))
(declare-function org-canvas--submissions-notes-end "org-canvas-submissions-render"
                  ())
(declare-function org-canvas--submissions-offer-to-post "org-canvas-submissions-push"
                  (count))
(declare-function org-canvas--submissions-only-rows "org-canvas-submissions-comment-items"
                  (only rows))
(declare-function org-canvas--submissions-partition-conflicts "org-canvas-submissions-push"
                  (assignment-id diffs))
(declare-function org-canvas--submissions-pending-comment-edits "org-canvas-submissions-comment-items"
                  (assignment-id &optional only))
(declare-function org-canvas--submissions-post-comment "org-canvas-submissions-entry"
                  (assignment-id user-id text))
(declare-function org-canvas--submissions-push-all "org-canvas-submissions-push"
                  (assignment-id plan))
(declare-function org-canvas--submissions-push-bulk-grades "org-canvas-submissions-push"
                  (assignment-id diffs &optional fields-fn))
(declare-function org-canvas--submissions-push-single-grade "org-canvas-submissions-push"
                  (assignment-id change &optional fields-fn))
(declare-function org-canvas--submissions-record-late-status "org-canvas-submissions-push"
                  (change stored))
(declare-function org-canvas--submissions-refused-note "org-canvas-submissions-comment-items"
                  (refused))
(declare-function org-canvas--submissions-reset-draft "org-canvas-submissions-render"
                  ())
(declare-function org-canvas--submissions-rubric-rows "org-canvas-submissions-comment-items"
                  ())
(declare-function org-canvas--submissions-rubric-set-row "org-canvas-submissions-comment-items"
                  (id score comment))
(declare-function org-canvas--submissions-submission-rubric-digest "org-canvas-submissions-comment-items"
                  (submission))
(declare-function org-canvas--submissions-trailing-grade-fields "org-canvas-submissions-push"
                  (change))
(declare-function org-canvas--submissions-user-ids "org-canvas-submissions-push"
                  (only))
(declare-function org-canvas--submissions-visit-grading-file "org-canvas-submissions-entry"
                  (path))

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

;; t and k work on the heading at point; a script resolving a column's
;; conflicts after a push names the students instead (issue #496).

(defun org-canvas--submissions-resolve-one (user-id take)
  "Resolve USER-ID's CONFLICT in the current grading file; TAKE picks the side.
Return the student's name, `missing' when no heading carries USER-ID,
or `unmarked' when its heading has no CONFLICT."
  (cond ((not (org-canvas--submissions-goto-user user-id)) 'missing)
        ((not (org-entry-get (point) "CONFLICT")) 'unmarked)
        (t (org-canvas--submissions-resolve-conflict take))))

;;;###autoload
(defun org-canvas-submissions-resolve-conflicts (file user-ids take)
  "Resolve the CONFLICT of each of USER-IDS in the grading FILE.
FILE is what `org-canvas-push-submission-grades' takes: a Canvas
assignment id, a path or a name.  USER-IDS are Canvas user ids,
integers or strings of digits.  TAKE non-nil takes Canvas's grade, as
`org-canvas-submissions-take-canvas' does at point; nil keeps the
typed one, as `org-canvas-submissions-keep-mine' does (issue #496).
Each student's submission is read and the file is saved.  Return a
plist: :resolved, the names resolved; :missing, the ids with no
heading in the file; :unmarked, the ids whose heading carries no
CONFLICT."
  (let ((ids (org-canvas--submissions-user-ids user-ids))
        (buf (org-canvas--submissions-visit-grading-file
              (org-canvas--submissions-grading-file-path file)))
        (resolved nil) (missing nil) (unmarked nil))
    (with-current-buffer buf
      (setq org-canvas-submissions--current-view 'detail)
      (save-excursion
        (dolist (id ids)
          (let ((outcome (org-canvas--submissions-resolve-one id take)))
            (pcase outcome
              ('missing (push id missing))
              ('unmarked (push id unmarked))
              (_ (push outcome resolved)))))))
    (list :resolved (nreverse resolved)
          :missing (nreverse missing)
          :unmarked (nreverse unmarked))))

(defun org-canvas--submissions-describe-rubric (change)
  "Return the note CHANGE's rubric rows add to its line, or an empty string."
  (if (plist-get change :triples)
      (format " (rubric %d/%d%s)" (plist-get change :filled) (plist-get change :of)
              (if (> (or (plist-get change :cleared) 0) 0)
                  (format ", %d cleared" (plist-get change :cleared))
                ""))
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
  "Return the push message's opening for COUNT grades sent with OUTCOME.
A dry run reads as one even when no grade was in it and OUTCOME is
nil, so a comment-only dry run never says Pushed (issue #529)."
  (pcase (if org-canvas--dry-run 'dry-run (plist-get outcome :state))
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
it (issue #417).  CANVAS_RUBRIC follows the rows sent, gone when they
took the assessment away, the cleared rows' Score cells are emptied
\(issue #449), and CONFLICT is cleared.  LATE is the entry
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
    (when (plist-get change :triples)
      (org-canvas--submissions-put-or-delete "CANVAS_RUBRIC" (plist-get change :new-rubric))
      (org-canvas--submissions-empty-cleared-rows (plist-get change :triples)))
    (org-entry-delete (point) "CONFLICT")))

(defun org-canvas--submissions-empty-cleared-rows (triples)
  "Empty the Score cell of each cleared row among TRIPLES at point.
A cleared row, (ID nil COMMENT t), once pushed reads as a pull of it
would: no score, the comment as typed."
  (dolist (triple triples)
    (when (nth 3 triple)
      (org-canvas--submissions-rubric-set-row (nth 0 triple) nil (nth 2 triple)))))

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
    ;; Grading is the \"submissions\" word of the allowlist, its own
    ;; axis beside the files: checked before the plan and the question,
    ;; so a scoped identity is refused before any read or send (#550).
    (org-canvas--check-write-feature 'submissions "a grade push")
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

(provide 'org-canvas-submissions-conflict)
;;; org-canvas-submissions-conflict.el ends here
