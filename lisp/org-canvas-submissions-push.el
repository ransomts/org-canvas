;;; org-canvas-submissions-push.el --- Push grades, late statuses and saved comments to Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The grading file is the source of truth for a column's grades: what
;; differs from CANVAS_SCORE is pushed, what the rubric table says
;; becomes the assessment, and a late, missing or extended student is
;; sent their status (issue #352).  This module is that path (issue
;; #142's split of the submissions module), including the progress
;; reporting of a bulk push (issue #382), the script entry points that
;; push one column by id or name (issue #381), pushing only the saved
;; comment edits (issue #425), and posting an assignment's grades so
;; students can see them (issue #202).
;;
;; Nothing here decides whether a push is safe: the conflict check runs
;; first, in `org-canvas-submissions-conflict'.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; The grading file, its buffer-local state and the minor mode it
;; is read in: this module is above that file, never required by it.
(require 'org-canvas-submissions)
;; Reading a pushed column back (verify.el), which shares the comment
;; reader of submissions-comments.el.
(require 'org-canvas-submissions-verify)

;; Defined below this file (a sibling split off the same module), so
;; declared rather than required: requiring would be a cycle.
(declare-function org-canvas--submissions-apply-comment-edits "org-canvas-submissions-comment-items"
                  (assignment-id edits))
(declare-function org-canvas--submissions-comment-edit-result "org-canvas-submissions-comment-items"
                  (counts comments))
(declare-function org-canvas--submissions-comment-edits-note "org-canvas-submissions-comment-items"
                  (counts refused))
(declare-function org-canvas--submissions-confirm-push "org-canvas-submissions-conflict"
                  (diffs drafts bank conflicts &optional comments))
(declare-function org-canvas--submissions-describe-bank "org-canvas-submissions-comment-items"
                  (counts &optional pending))
(declare-function org-canvas--submissions-grade-after "org-canvas-submissions-comment-items"
                  (rubric score))
(declare-function org-canvas--submissions-grades-applied-p "org-canvas-submissions-conflict"
                  (outcome))
(declare-function org-canvas--submissions-grades-note "org-canvas-submissions-conflict"
                  (count outcome))
(declare-function org-canvas--submissions-grading-file-path "org-canvas-submissions-entry"
                  (file))
(declare-function org-canvas--submissions-left-p "org-canvas-submissions-entry"
                  ())
(declare-function org-canvas--submissions-only-note "org-canvas-submissions-conflict"
                  (plan))
(declare-function org-canvas--submissions-pending-comment-edits "org-canvas-submissions-comment-items"
                  (assignment-id &optional only))
(declare-function org-canvas--submissions-plan-result "org-canvas-submissions-conflict"
                  (plan))
(declare-function org-canvas--submissions-post-drafts "org-canvas-submissions-conflict"
                  (assignment-id drafts))
(declare-function org-canvas--submissions-push-bank "org-canvas-submissions-comment-items"
                  (assignment-id))
(declare-function org-canvas--submissions-push-current "org-canvas-submissions-conflict"
                  (ask &optional only))
(declare-function org-canvas--submissions-record-late-only "org-canvas-submissions-conflict"
                  (diffs late))
(declare-function org-canvas--submissions-record-pushed "org-canvas-submissions-conflict"
                  (diffs &optional late))
(declare-function org-canvas--submissions-refused-note "org-canvas-submissions-comment-items"
                  (refused))
(declare-function org-canvas--submissions-rubric-change-at-point "org-canvas-submissions-comment-items"
                  (name))
(declare-function org-canvas--submissions-rubric-derived-score "org-canvas-submissions-comment-items"
                  (rubric old-score new-score name))
(declare-function org-canvas--submissions-send-grades "org-canvas-submissions-conflict"
                  (assignment-id diffs))
(declare-function org-canvas--submissions-submission-rubric-digest "org-canvas-submissions-comment-items"
                  (submission))
(declare-function org-canvas--submissions-user-id "org-canvas-submissions-render"
                  (submission))
(declare-function org-canvas--submissions-user-sortable-name "org-canvas-submissions-render"
                  (submission))
(declare-function org-canvas--submissions-visit-grading-file "org-canvas-submissions-entry"
                  (path))

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
;;
;; A row whose Score is `none' (issue #449) is sent with empty points
;; and comments, so the criterion Canvas holds is taken away; rows all
;; cleared take the assessment away, and the same ordering keeps a
;; score sent in the same entry.

(defun org-canvas--submissions-rubric-payload (triples)
  "Return TRIPLES as the rubric_assessment object Canvas accepts.
Each scored or commented criterion maps its id to points and comments;
Canvas picks the rating from the points.  Unscored rows are left out,
and a cleared one, (ID nil COMMENT t), goes with empty points and its
comment, empty without one (issue #449)."
  (delq nil
        (mapcar (lambda (triple)
                  (pcase-let ((`(,id ,score ,comment ,cleared) triple))
                    (cond (cleared
                           (cons (intern id) `((points . "") (comments . ,(or comment "")))))
                          ((or score comment)
                           (cons (intern id)
                                 (append (and score `((points . ,(string-to-number score))))
                                         (and comment `((comments . ,comment)))))))))
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

(defun org-canvas--submissions-live-holds-change-p (change live)
  "Return non-nil when CHANGE would send nothing new to LIVE.
LIVE is (score attempt rubric late) for the student, as
`org-canvas--submissions-live-baselines' reads it.  The score, the
attempt, and the rubric and late status when CHANGE sends them, all
agree: a push whose job was never confirmed stored them, and only its
baselines were left behind (issue #496).  Such a change is sent again
rather than held as a conflict, and its baselines are then recorded."
  (and live
       (equal (org-canvas--submissions-parse-score (nth 0 live))
              (org-canvas--submissions-parse-score (plist-get change :new-score)))
       (let ((attempt (plist-get change :attempt)))
         (or (null attempt) (not (numberp (nth 1 live))) (= attempt (nth 1 live))))
       (or (null (plist-get change :triples))
           (equal (nth 2 live) (plist-get change :new-rubric)))
       (or (null (plist-get change :late-status))
           (equal (nth 3 live) (plist-get change :late-status)))))

(defun org-canvas--submissions-conflict-p (change live)
  "Return why CHANGE conflicts with LIVE Canvas state, or nil.
LIVE is (score attempt rubric late) for the same student, or nil if
gone.  The rubric is compared only when CHANGE sends one, and the late
status only when CHANGE sets one.  The reason names
what moved and what Canvas holds — `score: Canvas has 93' — and is
the CONFLICT value; a property is a slot for a short value, and the
way out (`org-canvas-submissions-take-canvas' or
`org-canvas-submissions-keep-mine', issue #440) is the push's message
and the manual's (issue #264).  A CHANGE whose values LIVE already
holds is no conflict (`org-canvas--submissions-live-holds-change-p')."
  (let ((live-score (nth 0 live))
        (live-attempt (nth 1 live))
        (live-rubric (nth 2 live))
        (attempt (plist-get change :attempt)))
    (cond ((org-canvas--submissions-live-holds-change-p change live) nil)
          ((not (equal live-score (plist-get change :old-score)))
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

;;;; Pushing From a Script (issue #381)

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

Return a plist (issue #450 spells out the types):

  :pushed          integer, the grades Canvas stored (a count, not a
                   list of them);
  :state           symbol, how the grade send ended: `completed',
                   `failed', `unconfirmed', `dry-run', or nil when no
                   grade was sent;
  :message         string or nil, the reason for that state;
  :late            integer, the late statuses set;
  :comments        integer, the drafted comments posted;
  :conflicts       integer, the headings skipped as conflicts (held
                   as marked CONFLICT, or found by the push's check);
  :conflict-names  list of strings, their students' names (issue
                   #440);
  :edited, :deleted, :refused, :failed, :errored, :dry-run
                   integers, the sent comments rewritten, deleted,
                   not sent, refused by Canvas, failed in Emacs (issue
                   #443) and only shown (issue #419);
  :missing, :unchanged
                   lists of integer user ids, for ONLY (below);
  :posted          non-nil when the grades were posted.

A script that pushes a column by id and posts it calls

  (org-canvas-push-submission-grades \"2573836\" t)

and one that only pushes leaves POST out (issue #381).  ASSIGNMENT may
also be a list of columns: each is pushed in turn, with the same POST
and ONLY, and the value is a list of plists, one per column, in
ASSIGNMENT's order (issue #450).  After a push,
`org-canvas-submissions-verify' reads the column back.

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
  (if (consp assignment)
      (mapcar (lambda (one) (org-canvas-push-submission-grades one post only))
              assignment)
    (org-canvas--submissions-push-file assignment post only)))

(defun org-canvas--submissions-push-file (assignment post only)
  "Push the one grading file ASSIGNMENT names; return the push's plist.
POST and ONLY are as `org-canvas-push-submission-grades' takes them."
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
    (org-canvas--check-write-feature 'submissions "a comment-edit push")
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
  ;; The one post path every poster reaches: P, the post commands of
  ;; the status table, and a script's POST argument (issue #550).
  (org-canvas--check-write-feature 'submissions "posting grades")
  (let ((reply (org-canvas--graphql-mutate
                (format "post the grades of assignment %s" assignment-id)
                org-canvas--submissions-post-grades-mutation
                (list (cons 'assignmentId (format "%s" assignment-id))))))
    (unless (org-canvas--dry-run-response-p reply)
      (message "Grades posted for assignment %s." assignment-id)
      t)))
;;; org-canvas-submissions.el ends here

(provide 'org-canvas-submissions-push)
;;; org-canvas-submissions-push.el ends here
