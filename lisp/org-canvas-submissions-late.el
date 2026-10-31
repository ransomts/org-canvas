;;; org-canvas-submissions-late.el --- Draft the late-policy comment of late rows -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A course's feedback rules may ask for a comment on every late upload:
;; when it came in, the due time it missed, how late Canvas counts it,
;; the percentage the late policy takes, and what the score shows as
;; ("your 8 shows as 6 of 10").  The grading file holds every number
;; that needs, and the arithmetic was done by a scratch script per
;; column (issue #484).
;;
;; `org-canvas-submissions-draft-late-notes' is that script as a command
;; on a grading file.  For each student with STATUS late and a score it
;; fills `org-canvas-submissions-late-note-format' from:
;;
;;   - SUBMITTED_AT, and DUE_AT, the student's own due time, which the
;;     pull writes beside DAYS_LATE;
;;   - DAYS_LATE, or for an hourly policy the hours between the two;
;;   - the course late policy, the LATE_SUBMISSION_* properties the
;;     settings pull writes on settings.org's heading;
;;   - SCORE and the file's POINTS_POSSIBLE.
;;
;; and adds the note to the student's `** Comment to post'.  A student
;; whose draft or sent comments already carry the same note is left
;; alone; one carrying a note of the same shape with other numbers (a
;; regrade since) is named, not rewritten.  Students to hold, such as
;; one with an extension ruling pending, are named and skipped.
;;
;; Everything is local: nothing is read from Canvas and nothing is
;; sent.  The drafts go out with the grade push (S), as typed ones do.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
(require 'format-spec)
;; A command file above the feature modules: it writes the grading
;; files the submissions module writes, through the comment file's
;; draft writer, and reads the late policy the settings pull stores.
(require 'org-canvas-submissions)
(require 'org-canvas-submissions-comments)
(require 'org-canvas-settings)

;; The rest of the submissions library, split by concern (issue #570).
(require 'org-canvas-submissions-render)
(require 'org-canvas-submissions-comment-items)

(defcustom org-canvas-submissions-late-note-format
  "This came in %s, after the due time of %d: %l late.  The course's late policy takes %p%% for that, so your %r shows as %f of %m."
  "The late-policy comment `org-canvas-submissions-draft-late-notes' drafts.
A `format-spec' string: %s the submitted time, %d the due time it
missed (the student's own, an override's when there is one), %l the
lateness in the policy's unit (\"2 days\"), %n the same as a bare
number, %p the percent the policy takes, %r the score as entered, %f
the score after the deduction, %m the points possible, and %% a
percent sign.  The times are written with
`org-canvas-submissions-late-note-time-format'."
  :type 'string
  :group 'org-canvas)

(defcustom org-canvas-submissions-late-note-time-format "%a %b %-d at %-H:%M"
  "The `format-time-string' format of the times in a late-policy comment."
  :type 'string
  :group 'org-canvas)

;;;; The policy

(defun org-canvas--submissions-late-policy-value (name)
  "Return property NAME of the heading at point as a number, or nil."
  (let ((v (org-entry-get (point) name)))
    (and v (string-match-p "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\'" v)
         (string-to-number v))))

(defun org-canvas--submissions-late-policy ()
  "Return the course late policy, as settings.org records it, as a plist.
The plist has :deduction (percent per interval), :interval (\"day\" or
\"hour\") and :minimum (the floor's percent, nil when the floor is
off).  A course whose settings hold no enabled deduction is a
`user-error', since a note with nothing taken says nothing."
  (let ((file (expand-file-name org-canvas-settings-file)))
    (unless (file-exists-p file)
      (user-error "No %s: pull the settings first (org-canvas-pull-settings)" file))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (unless (re-search-forward org-outline-regexp-bol nil t)
         (user-error "No heading in %s" file))
       (let ((deduction (org-canvas--submissions-late-policy-value
                         "LATE_SUBMISSION_DEDUCTION")))
         (unless (and deduction
                      (equal (org-entry-get (point) "LATE_SUBMISSION_DEDUCTION_ENABLED")
                             "true"))
           (user-error "The late policy in %s takes nothing (LATE_SUBMISSION_DEDUCTION_ENABLED)"
                       file))
         (list :deduction deduction
               :interval (or (org-entry-get (point) "LATE_SUBMISSION_INTERVAL") "day")
               :minimum (and (equal (org-entry-get
                                     (point) "LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED")
                                    "true")
                             (org-canvas--submissions-late-policy-value
                              "LATE_SUBMISSION_MINIMUM_PERCENT"))))))))

(defun org-canvas--submissions-late-final (raw possible percent minimum)
  "Return RAW, out of POSSIBLE, after the policy takes PERCENT of POSSIBLE.
MINIMUM, the floor's percent or nil, is the least the deduction leaves,
and it never raises a score below it: the arithmetic of Canvas's own
late policy, the deduction rounded to two places."
  (let* ((raw-pct (/ (* raw 100.0) possible))
         (final-pct (min raw-pct (max (- raw-pct percent) (or minimum 0))))
         (deducted (/ (fround (* (- raw-pct final-pct) possible)) 100.0)))
    (- raw deducted)))

(defun org-canvas--submissions-late-number (n)
  "Return N as the note writes it: no trailing zeros, at most two places."
  (let ((text (format "%.2f" n)))
    (replace-regexp-in-string "\\.?0+\\'" "" text)))

;;;; One student

(defun org-canvas--submissions-late-time (timestamp)
  "Return TIMESTAMP, an Org timestamp, as a time read in UTC, or nil.
Both the submitted and the due time were written in one zone, so
reading them as UTC keeps their difference and their wall clock."
  (when (and timestamp (string-match org-ts-regexp0 timestamp))
    (let ((decoded (org-parse-time-string (match-string 0 timestamp))))
      (setf (nth 8 decoded) 0)
      (encode-time decoded))))

(defun org-canvas--submissions-late-count (policy days submitted due)
  "Return how many of POLICY's intervals late the entry is, or nil.
A daily policy counts DAYS, the entry's DAYS_LATE; an hourly one the
hours from DUE to SUBMITTED, both times, rounded up."
  (if (equal (plist-get policy :interval) "hour")
      (and submitted due
           (ceiling (float-time (time-subtract submitted due)) 3600))
    days))

(defun org-canvas--submissions-late-facts (policy possible)
  "Return the late note's facts of the entry at point, or a string.
POLICY is the course late policy and POSSIBLE the column's points.
The facts are a plist for `org-canvas--submissions-late-text'; the
string, when something it needs is missing, says what."
  (let* ((score (org-entry-get (point) "SCORE"))
         (days (org-canvas--submissions-late-policy-value "DAYS_LATE"))
         (submitted (org-canvas--submissions-late-time (org-entry-get (point) "SUBMITTED_AT")))
         (due (org-canvas--submissions-late-time (org-entry-get (point) "DUE_AT")))
         (count (org-canvas--submissions-late-count policy days submitted due)))
    (cond ((not (org-canvas--submissions-comments-number-p score)) "no score")
          ((null submitted) "no SUBMITTED_AT")
          ((null due) "no DUE_AT (re-pull the column)")
          ((null count) "no DAYS_LATE")
          (t (let* ((raw (string-to-number score))
                    (percent (* count (plist-get policy :deduction))))
               (list :submitted submitted :due due :count count
                     :unit (plist-get policy :interval) :percent percent
                     :raw raw :possible possible
                     :final (org-canvas--submissions-late-final
                             raw possible percent (plist-get policy :minimum))))))))

(defun org-canvas--submissions-late-text (facts)
  "Return the late note FACTS give, by `org-canvas-submissions-late-note-format'."
  (let ((time (lambda (key) (format-time-string
                             org-canvas-submissions-late-note-time-format
                             (plist-get facts key) t)))
        (count (plist-get facts :count)))
    (format-spec org-canvas-submissions-late-note-format
                 `((?s . ,(funcall time :submitted))
                   (?d . ,(funcall time :due))
                   (?l . ,(format "%d %s%s" count (plist-get facts :unit)
                                  (if (= count 1) "" "s")))
                   (?n . ,(format "%d" count))
                   (?p . ,(org-canvas--submissions-late-number (plist-get facts :percent)))
                   (?r . ,(org-canvas--submissions-late-number (plist-get facts :raw)))
                   (?f . ,(org-canvas--submissions-late-number (plist-get facts :final)))
                   (?m . ,(org-canvas--submissions-late-number (plist-get facts :possible)))))))

(defun org-canvas--submissions-late-squeeze (text)
  "Return TEXT with each run of whitespace one space, trimmed."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))

(defun org-canvas--submissions-late-pattern ()
  "Return a regexp matching any note the late-note format writes.
Each %-spec matches any text; the rest is literal, its whitespace
squeezed as `org-canvas--submissions-late-squeeze' squeezes a text."
  (let ((fmt (org-canvas--submissions-late-squeeze
              org-canvas-submissions-late-note-format))
        (start 0) (parts nil))
    (while (string-match "%\\(.\\)" fmt start)
      (push (regexp-quote (substring fmt start (match-beginning 0))) parts)
      (push (if (equal (match-string 1 fmt) "%") "%" ".+?") parts)
      (setq start (match-end 0)))
    (push (regexp-quote (substring fmt start)) parts)
    (apply #'concat (nreverse parts))))

(defun org-canvas--submissions-late-texts ()
  "Return the draft and sent comment texts of the entry at point, squeezed."
  (mapcar #'org-canvas--submissions-late-squeeze
          (delq nil (cons (org-canvas--submissions-comment-draft)
                          (mapcar (lambda (c) (plist-get c :text))
                                  (org-canvas--submissions-sent-comments))))))

(defun org-canvas--submissions-late-ensure-draft-heading ()
  "Add a Comment to post heading at the end of the entry at point if absent."
  (unless (org-canvas--submissions-draft-region)
    (save-excursion
      (org-end-of-subtree t t)
      (insert (if (bolp) "" "\n") org-canvas--submissions-draft-heading "\n\n"))))

(defun org-canvas--submissions-late-write (note)
  "Add NOTE to the draft of the entry at point, after any text there."
  (org-canvas--submissions-late-ensure-draft-heading)
  (let ((draft (org-canvas--submissions-comment-draft)))
    (org-canvas--submissions-comments-set-draft
     (if draft (concat draft "\n\n" note) note))))

(defun org-canvas--submissions-late-outcome (note write)
  "Return the outcome of NOTE for the entry at point, written when WRITE.
`same' when its draft or a sent comment already carries NOTE,
`changed' when one carries a note of the same shape and other
numbers, else `drafted'."
  (let ((texts (org-canvas--submissions-late-texts))
        (squeezed (org-canvas--submissions-late-squeeze note))
        (pattern (org-canvas--submissions-late-pattern)))
    (cond ((cl-some (lambda (text) (string-search squeezed text)) texts) 'same)
          ((cl-some (lambda (text) (string-match-p pattern text)) texts) 'changed)
          (t (when write (org-canvas--submissions-late-write note))
             'drafted))))

(defun org-canvas--submissions-late-student (policy possible hold write)
  "Return the late-note outcome of the student heading at point, or nil.
Nil unless STATUS is late.  Else (KIND NAME USER-ID DETAIL): `held'
when HOLD names the user id or the heading, `skipped' with DETAIL
saying what is missing, or the outcome of
`org-canvas--submissions-late-outcome' with DETAIL the note.  POLICY
and POSSIBLE are as `org-canvas--submissions-late-facts' takes them;
WRITE writes a drafted note."
  (when (equal (org-entry-get (point) "STATUS") "late")
    (let* ((name (org-get-heading t t t t))
           (user-id (org-entry-get (point) "USER_ID"))
           (facts (and (not (or (member user-id hold) (member name hold)))
                       (org-canvas--submissions-late-facts policy possible))))
      (cond ((null facts) (list 'held name user-id nil))
            ((stringp facts) (list 'skipped name user-id facts))
            (t (let ((note (org-canvas--submissions-late-text facts)))
                 (list (org-canvas--submissions-late-outcome note write)
                       name user-id note)))))))

;;;; The command

(defun org-canvas--submissions-late-possible ()
  "Return the grading file's POINTS_POSSIBLE as a positive number.
Signal a `user-error' when the file has none."
  (let ((p (org-canvas--submissions-file-property "POINTS_POSSIBLE")))
    (unless (and (org-canvas--submissions-comments-number-p p)
                 (> (string-to-number p) 0))
      (user-error "This grading file has no POINTS_POSSIBLE; re-pull it"))
    (string-to-number p)))

(defun org-canvas--submissions-late-run (hold write)
  "Return the outcomes of every late student in this grading buffer.
HOLD and WRITE are as `org-canvas--submissions-late-student' takes them."
  (let ((policy (org-canvas--submissions-late-policy))
        (possible (org-canvas--submissions-late-possible))
        (markers (org-canvas--submissions-comments-markers)))
    (prog1 (save-excursion
             (delq nil (mapcar (lambda (entry)
                                 (goto-char (cdr entry))
                                 (org-canvas--submissions-late-student
                                  policy possible hold write))
                               markers)))
      (dolist (m markers) (set-marker (cdr m) nil)))))

(defun org-canvas--submissions-late-tally (outcomes dry-run)
  "Return the command's result plist for OUTCOMES; DRY-RUN as it ran.
:drafted is a list of (NAME USER-ID NOTE); :same, :changed and :held
list (NAME USER-ID); :skipped lists (NAME USER-ID WHY)."
  (let ((of (lambda (kind keep)
              (mapcar (lambda (o) (seq-take (cdr o) keep))
                      (seq-filter (lambda (o) (eq (car o) kind)) outcomes)))))
    (list :drafted (funcall of 'drafted 3)
          :same (funcall of 'same 2)
          :changed (funcall of 'changed 2)
          :held (funcall of 'held 2)
          :skipped (funcall of 'skipped 3)
          :dry-run (and dry-run t))))

(defun org-canvas--submissions-late-report (result)
  "Name RESULT's held, changed and skipped students, then sum it up.
Under a dry run each draft is shown with its note."
  (let ((dry-run (plist-get result :dry-run)))
    (when dry-run
      (dolist (d (plist-get result :drafted))
        (org-canvas--user-message "[DRY-RUN] Late note for %s (%s): %s"
                                  (nth 0 d) (nth 1 d) (nth 2 d))))
    (dolist (h (plist-get result :held))
      (org-canvas--user-message "Late notes: held %s (%s)" (nth 0 h) (nth 1 h)))
    (dolist (c (plist-get result :changed))
      (org-canvas--user-message
       "Late notes: %s (%s) has a late note with other numbers; check it after the regrade"
       (nth 0 c) (nth 1 c)))
    (dolist (s (plist-get result :skipped))
      (org-canvas--user-message "Late notes: skipped %s (%s): %s"
                                (nth 0 s) (nth 1 s) (nth 2 s)))
    (org-canvas--user-message
     "%sLate notes: %d %s, %d already there, %d changed, %d held, %d skipped"
     (if dry-run "[DRY-RUN] " "")
     (length (plist-get result :drafted)) (if dry-run "would be drafted" "drafted")
     (length (plist-get result :same)) (length (plist-get result :changed))
     (length (plist-get result :held)) (length (plist-get result :skipped)))))

(defun org-canvas--submissions-late-read-hold ()
  "Ask for the students to hold, user ids or names separated by commas."
  (split-string (read-string "Hold (user ids or names, comma-separated): ")
                "," t "[ \t]+"))

;;;###autoload
(defun org-canvas-submissions-draft-late-notes (&optional file hold dry-run)
  "Draft the late-policy comment of every late student in a grading file.
FILE is taken as `org-canvas-submissions-export-comments' takes it.
Each student with STATUS late and a score gets, under Comment to post,
the note `org-canvas-submissions-late-note-format' makes of their
SUBMITTED_AT, DUE_AT, DAYS_LATE (hours, for an hourly policy), SCORE,
the file's POINTS_POSSIBLE and the course late policy settings.org
holds: the percent it takes and the score after it, floored at the
policy's minimum.  The note follows any draft already there.

A student whose draft or sent comments already carry the same note is
left alone; one whose carry a note of the same shape with other
numbers, as a regrade leaves, is named and not rewritten.  HOLD lists
students to skip, by user id or heading name, such as one awaiting an
extension ruling; they are named in the report.  Interactively a
prefix argument asks for them.  A student without a DUE_AT (a file
pulled before it was written) is skipped and named.

Under DRY-RUN or `org-canvas--dry-run' nothing is written and each
note is shown.  Nothing is sent: the drafts go out with the grade
push.  Return a plist: :drafted, a list of (NAME USER-ID NOTE); :same,
:changed and :held, lists of (NAME USER-ID); :skipped, a list of (NAME
USER-ID WHY); :dry-run."
  ;; Bare `(interactive)': a sexp spec blanks undercover's line counts.
  (interactive)
  (when (and (null hold) current-prefix-arg (called-interactively-p 'any))
    (setq hold (org-canvas--submissions-late-read-hold)))
  (with-current-buffer (org-canvas--submissions-comments-buffer file)
    (let* ((dry-run (or dry-run org-canvas--dry-run))
           (result (org-canvas--submissions-late-tally
                    (org-canvas--submissions-late-run hold (not dry-run)) dry-run)))
      (when (and buffer-file-name (not dry-run) (plist-get result :drafted))
        (org-canvas--save-buffer))
      (org-canvas--submissions-late-report result)
      result)))

(provide 'org-canvas-submissions-late)
;;; org-canvas-submissions-late.el ends here
