;;; org-canvas-submissions-window.el --- Score by section window -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An in-class check — attendance, an in-room closer — is graded by
;; when it arrived: full credit when the student submitted during their
;; own section's meeting, 0 otherwise.  The meeting windows were typed
;; in the assignment's overrides table and then again in a script that
;; joined people.org's sections to the grading file's SUBMITTED_AT, and
;; nothing in the grading file said which rule had set a 0 (issue #383).
;;
;; `org-canvas-submissions-score-by-window' is that script as a command
;; on a grading file.  For each student:
;;
;;   - the sections come from people.org's SECTIONS (by USER_ID);
;;   - a section's window is its row of the assignment's overrides
;;     table in assignments.org, Unlock At to Lock At (Due At when the
;;     row has no Lock At), or else the section's MEETS property in
;;     sections.org on the day the work was submitted;
;;   - the submission is inside when SUBMITTED_AT falls in any of the
;;     student's windows widened by GRACE minutes either side.
;;
;; Inside earns the assignment's points (the file's POINTS_POSSIBLE),
;; outside and unsubmitted earn 0, and a student with no window (no
;; section, or no section with a window) is left unscored.  A SCORE
;; already there, an excused row and a student who left are left
;; alone; OVERWRITE replaces a typed score.  A report listing the
;; outside and windowless students with their times is shown before
;; anything is written, and each scored student's Notes get a line
;; saying which rule gave the score.
;;
;; Everything is local: nothing is read from Canvas and nothing is
;; sent.  The scores go out with the grade push (S), as typed ones do.
;;
;; TIME
;; ====
;; SUBMITTED_AT and the table's cells are Org timestamps the pull wrote
;; in one zone (`org-canvas--time-zone'), and MEETS is a wall clock in
;; the course's zone.  All three are compared as wall-clock minutes, so
;; the result does not depend on the zone Emacs runs in.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules: it scores the grading
;; files the submissions module writes.
(require 'org-canvas-submissions)

;; sections.el owns MEETS and the overrides table; it is a feature
;; module, so it is declared and never required.
(declare-function org-canvas--section-keys "org-canvas-sections" (text))
(declare-function org-canvas--section-meets-parse "org-canvas-sections" (text))
(declare-function org-canvas--override-section-windows "org-canvas-sections"
                  (assignment-id))

(defcustom org-canvas-submissions-window-grace 0
  "Minutes either side of a section window that still count as inside.
The default for `org-canvas-submissions-score-by-window' when it is
called from Lisp without a grace, and the prompt's default otherwise."
  :type 'integer
  :group 'org-canvas)

(defconst org-canvas--submissions-window-report-buffer "*canvas-window-scores*"
  "Buffer the section-window report is rendered into.")

(defconst org-canvas--submissions-window-note-prefix "- Window rule: "
  "Start of the Notes line recording the rule a window score came from.")

;;;; Wall-Clock Minutes

(defun org-canvas--submissions-window-minutes (timestamp)
  "Return Org TIMESTAMP as wall-clock minutes since the epoch, or nil.
The zone is ignored on purpose: every time compared is a wall clock in
the course's zone, so reading all of them as UTC keeps their order and
their differences whatever zone Emacs runs in."
  (when (and (stringp timestamp)
             (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" timestamp))
    (let ((d (org-parse-time-string timestamp)))
      (/ (truncate (float-time
                    (encode-time (list 0 (nth 1 d) (nth 2 d) (nth 3 d)
                                       (nth 4 d) (nth 5 d) nil nil t))))
         60))))

(defun org-canvas--submissions-window-clock (minutes)
  "Return wall-clock MINUTES as HH:MM."
  (format-time-string "%H:%M" (* 60 minutes) t))

(defun org-canvas--submissions-window-day (minutes)
  "Return wall-clock MINUTES as a day and a time, \"Fri 09:07\"."
  (format-time-string "%a %H:%M" (* 60 minutes) t))

;;;; The Course's Sections and Windows

(defun org-canvas--submissions-window-file (var)
  "Return the file VAR names when VAR is bound and the file exists, else nil."
  (let ((file (and (boundp var) (symbol-value var))))
    (and file (file-exists-p file) file)))

(defun org-canvas--submissions-window-people-sections ()
  "Return a hash table of USER_ID to the section keys people.org lists."
  (let ((table (make-hash-table :test 'equal))
        (file (org-canvas--submissions-window-file 'org-canvas-people-file)))
    (when file
      (with-current-buffer (org-canvas--find-file-noselect file)
        (org-map-entries
         (lambda ()
           (puthash (org-entry-get (point) "USER_ID")
                    (org-canvas--section-keys
                     (or (org-entry-get (point) "SECTIONS") ""))
                    table))
         "USER_ID={.}" 'file)))
    table))

(defun org-canvas--submissions-window-meets ()
  "Return a hash table of sections.org heading title to its parsed MEETS.
A MEETS that does not parse is left out with a warning in the log;
validation names it too."
  (let ((table (make-hash-table :test 'equal))
        (file (org-canvas--submissions-window-file 'org-canvas-sections-file)))
    (when file
      (with-current-buffer (org-canvas--find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let* ((title (org-get-heading t t t t))
                  (text (org-entry-get (point) "MEETS"))
                  (patterns (org-canvas--section-meets-parse text)))
             (if patterns
                 (puthash title patterns table)
               (org-canvas--log-warning org-canvas--logger
                 "[Window] Section '%s': MEETS '%s' does not parse; ignored"
                 title text))))
         "MEETS={.}" 'file)))
    table))

(defun org-canvas--submissions-window-overrides (assignment-id)
  "Return ASSIGNMENT-ID's overrides windows as (KEY OPENS CLOSES) minutes."
  (delq nil
        (mapcar (lambda (w)
                  (let ((opens (org-canvas--submissions-window-minutes (nth 1 w)))
                        (closes (org-canvas--submissions-window-minutes (nth 2 w))))
                    (and opens closes (list (nth 0 w) opens closes))))
                (org-canvas--override-section-windows assignment-id))))

;;;; Judging One Submission

(defun org-canvas--submissions-window-meets-windows (key patterns at)
  "Return section KEY's windows from MEETS PATTERNS on the day of AT.
AT is wall-clock minutes; a window is (KEY OPENS CLOSES) in the same
minutes.  Nil when the section does not meet that day."
  (let ((weekday (decoded-time-weekday (decode-time (* 60 at) t)))
        (midnight (- at (mod at 1440))))
    (delq nil (mapcar (lambda (p)
                        (and (memq weekday (nth 0 p))
                             (list key (+ midnight (nth 1 p)) (+ midnight (nth 2 p)))))
                      patterns))))

(defun org-canvas--submissions-window-section (key at context)
  "Return section KEY's windows around AT, `off-day', or nil for no rule.
CONTEXT carries :overrides, the table's windows, and :meets, the
MEETS table.  A table row is the section's window whatever the day;
MEETS answers `off-day' when the section does not meet on AT's day."
  (let ((row (assoc key (plist-get context :overrides)))
        (patterns (gethash key (plist-get context :meets))))
    (cond (row (list row))
          (patterns (or (org-canvas--submissions-window-meets-windows key patterns at)
                        'off-day)))))

(defun org-canvas--submissions-window-inside-p (window at grace)
  "Return non-nil when AT falls in WINDOW widened by GRACE minutes."
  (<= (- (nth 1 window) grace) at (+ (nth 2 window) grace)))

(defun org-canvas--submissions-window-judge (at keys context)
  "Judge a submission at AT for a student in sections KEYS.
Return (VERDICT . WINDOW): `inside' with the window it fell in,
`outside' with the student's first window (nil when none of their
sections meets that day), or `no-window' when none of the sections
has a window at all.  CONTEXT is as in `org-canvas--submissions-window-section',
plus :grace in minutes."
  (let (windows ruled)
    (dolist (key keys)
      (let ((found (org-canvas--submissions-window-section key at context)))
        (when found (setq ruled t))
        (when (consp found) (setq windows (append windows found)))))
    (let ((hit (cl-find-if (lambda (w) (org-canvas--submissions-window-inside-p
                                        w at (plist-get context :grace)))
                           windows)))
      (cond (hit (cons 'inside hit))
            (ruled (cons 'outside (car windows)))
            (t (cons 'no-window nil))))))

;;;; Rows

(defun org-canvas--submissions-window-typed-p ()
  "Return the SCORE of the entry at point when one is typed, else nil."
  (let ((score (org-entry-get (point) "SCORE")))
    (and score (not (string-empty-p (string-trim score))) score)))

(defun org-canvas--submissions-window-verdict (at keys context)
  "Return (VERDICT . WINDOW) for a submission at AT in sections KEYS.
`unsubmitted' when AT is nil; otherwise what
`org-canvas--submissions-window-judge' makes of it under CONTEXT."
  (if at
      (org-canvas--submissions-window-judge at keys context)
    (cons 'unsubmitted nil)))

(defun org-canvas--submissions-window-action (verdict typed context)
  "Return what to do with a row judged VERDICT whose typed SCORE is TYPED.
`excused' and `kept' leave the row alone, `unscored' too (no window);
`write' sets the score.  CONTEXT's :overwrite replaces a typed score."
  (cond ((equal (and typed (org-canvas--submissions-parse-score typed)) "EX")
         'excused)
        ((and typed (not (plist-get context :overwrite))) 'kept)
        ((eq verdict 'no-window) 'unscored)
        (t 'write)))

(defun org-canvas--submissions-window-row (context)
  "Return the plan for the student heading at point, as a plist.
Keys: :marker, :name, :at (wall-clock minutes or nil), :keys (the
sections), :verdict, :window, :action and :score.  CONTEXT is the
run's plist (see `org-canvas-submissions-score-by-window')."
  (let* ((at (org-canvas--submissions-window-minutes (org-entry-get (point) "SUBMITTED_AT")))
         (keys (gethash (org-entry-get (point) "USER_ID") (plist-get context :people)))
         (judged (org-canvas--submissions-window-verdict at keys context))
         (action (org-canvas--submissions-window-action
                  (car judged) (org-canvas--submissions-window-typed-p) context)))
    ;; Advancing: a Notes heading added to the entry above lands at
    ;; this heading's start and must stay above the marker.
    (list :marker (copy-marker (point) t) :name (org-get-heading t t t t)
          :at at :keys keys :verdict (car judged) :window (cdr judged)
          :action action
          :score (if (eq (car judged) 'inside) (plist-get context :points-text) "0"))))

(defun org-canvas--submissions-window-plan (context)
  "Return the plan for every student heading in this grading file.
A heading without USER_ID (the Comment Bank) and a student who left
the course are not part of it.  CONTEXT is the run's plist."
  (let (rows)
    (org-map-entries
     (lambda ()
       (unless (or (not (org-entry-get (point) "USER_ID"))
                   (org-canvas--submissions-left-p))
         (push (org-canvas--submissions-window-row context) rows)))
     "LEVEL=1" 'file)
    (nreverse rows)))

;;;; Report

(defun org-canvas--submissions-window-describe (window)
  "Return WINDOW as \"KEY HH:MM-HH:MM\", or a dash when nil."
  (if window
      (format "%s %s-%s" (nth 0 window)
              (org-canvas--submissions-window-clock (nth 1 window))
              (org-canvas--submissions-window-clock (nth 2 window)))
    "-"))

(defun org-canvas--submissions-window-sections-text (keys)
  "Return section KEYS for a table cell, or a dash when there are none."
  (if keys (mapconcat #'identity keys ", ") "-"))

(defun org-canvas--submissions-window-when (row)
  "Return when ROW's student submitted, for a report or a note."
  (if (plist-get row :at) (org-canvas--submissions-window-day (plist-get row :at)) "-"))

(defun org-canvas--submissions-window-count (rows verdict &optional action)
  "Count ROWS judged VERDICT, and taking ACTION when given."
  (cl-count-if (lambda (r) (and (eq (plist-get r :verdict) verdict)
                                (or (null action) (eq (plist-get r :action) action))))
               rows))

(defun org-canvas--submissions-window-insert-table (rows)
  "Insert an Org table of ROWS: student, sections, submitted, window, action."
  (insert "| Student | Sections | Submitted | Window | Score |\n|-\n")
  (dolist (r rows)
    (insert (format "| %s | %s | %s | %s | %s |\n"
                    (plist-get r :name)
                    (org-canvas--submissions-window-sections-text (plist-get r :keys))
                    (org-canvas--submissions-window-when r)
                    (org-canvas--submissions-window-describe (plist-get r :window))
                    (if (eq (plist-get r :action) 'write)
                        (plist-get r :score)
                      (format "left as %s" (plist-get r :action))))))
  (forward-line -1)
  (org-table-align)
  (goto-char (point-max)))

(defun org-canvas--submissions-window-summary (rows context)
  "Return the one-line count of ROWS for the report and the echo area.
CONTEXT supplies :grace and :points-text."
  (format (concat "%d inside, %d outside, %d unsubmitted, %d without a window; "
                 "%d to write (grace %d min, full credit %s)")
          (org-canvas--submissions-window-count rows 'inside)
          (org-canvas--submissions-window-count rows 'outside)
          (org-canvas--submissions-window-count rows 'unsubmitted)
          (org-canvas--submissions-window-count rows 'no-window)
          (cl-count 'write rows :key (lambda (r) (plist-get r :action)))
          (plist-get context :grace) (plist-get context :points-text)))

(defun org-canvas--submissions-window-render (rows context)
  "Insert the report on ROWS for CONTEXT's assignment into this buffer."
  (insert (format "#+TITLE: Section windows: %s\n\n" (plist-get context :name)))
  (insert (org-canvas--submissions-window-summary rows context) "\n")
  (dolist (group `(("Outside the window (0)" outside)
                   ("No window (left unscored)" no-window)
                   ("Unsubmitted (0)" unsubmitted)))
    (let ((these (cl-remove-if-not (lambda (r) (eq (plist-get r :verdict) (nth 1 group)))
                                   rows)))
      (when these
        (insert "\n* " (nth 0 group) "\n\n")
        (org-canvas--submissions-window-insert-table these)))))

(defun org-canvas--submissions-window-report (rows context)
  "Show the report on ROWS for CONTEXT; see `org-canvas--report-display'."
  (org-canvas--report-display
   org-canvas--submissions-window-report-buffer
   (lambda () (org-canvas--submissions-window-render rows context))
   #'org-mode))

;;;; Writing

(defun org-canvas--submissions-window-note (row context)
  "Return the Notes line recording how ROW was scored under CONTEXT."
  (format "%ssubmitted %s, sections %s, window %s, grace %d min: %s, score %s"
          org-canvas--submissions-window-note-prefix
          (org-canvas--submissions-window-when row)
          (org-canvas--submissions-window-sections-text (plist-get row :keys))
          (org-canvas--submissions-window-describe (plist-get row :window))
          (plist-get context :grace)
          (plist-get row :verdict) (plist-get row :score)))

(defun org-canvas--submissions-window-notes-end ()
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
        (unless (bolp) (insert "\n"))
        (insert org-canvas--submissions-notes-heading "\n")
        (point)))))

(defun org-canvas--submissions-window-set-note (line)
  "Record LINE in the Notes of the entry at point, replacing an older one."
  (let ((region (org-canvas--submissions-section-region
                 org-canvas--submissions-notes-heading)))
    (when region
      (save-excursion
        (goto-char (car region))
        (when (re-search-forward
               (concat "^" (regexp-quote org-canvas--submissions-window-note-prefix) ".*\n?")
               (cdr region) t)
          (replace-match "")))))
  (save-excursion
    (goto-char (org-canvas--submissions-window-notes-end))
    (insert line)
    (unless (looking-at-p "\n") (insert "\n"))))

(defun org-canvas--submissions-window-write (row context)
  "Write ROW's score, its Rubric rows on full credit, and its note.
CONTEXT is the run's plist, which the note describes."
  (goto-char (plist-get row :marker))
  (org-entry-put (point) "SCORE" (plist-get row :score))
  (when (eq (plist-get row :verdict) 'inside)
    (org-canvas--submissions-rubric-fill-full))
  (org-canvas--submissions-window-set-note (org-canvas--submissions-window-note row context)))

(defun org-canvas--submissions-window-confirm (count)
  "Return non-nil when COUNT scores may be written.
Under `noninteractive' there is nobody to ask, and the report has
already been printed, so the answer is yes (issue #281's rule)."
  (or noninteractive
      (y-or-n-p (format "Write %d score(s) to this grading file? " count))))

;;;; Command

(defun org-canvas--submissions-window-target (file)
  "Return the grading buffer FILE names, or the current one when FILE is nil.
The current buffer when it is a grading file in submissions mode;
otherwise FILE is resolved, or asked for, as by
`org-canvas-open-submissions'."
  (if (and (null file) (bound-and-true-p org-canvas-submissions-mode))
      (current-buffer)
    (org-canvas--submissions-visit-grading-file
     (org-canvas--submissions-grading-file-path file))))

(defun org-canvas--submissions-window-points (points)
  "Return POINTS, or the file's POINTS_POSSIBLE, or ask for them."
  (or points
      (org-canvas--submissions-default-points)
      (if noninteractive
          (user-error "No POINTS_POSSIBLE in this grading file; pass POINTS")
        (read-number "Full credit points: "))))

(defun org-canvas--submissions-window-context (grace overwrite points)
  "Return the run's plist for this grading buffer.
GRACE, OVERWRITE and POINTS are as the command received them."
  (org-canvas--submissions-ensure-context)
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (list :name org-canvas-submissions--assignment-name
        :grace grace :overwrite overwrite
        :points-text (org-canvas--submissions-format-number
                      (org-canvas--submissions-window-points points))
        :people (org-canvas--submissions-window-people-sections)
        :meets (org-canvas--submissions-window-meets)
        :overrides (org-canvas--submissions-window-overrides
                    org-canvas-submissions--assignment-id)))

(defun org-canvas--submissions-window-apply (rows context)
  "Write the scores ROWS plan, after confirmation; return how many.
CONTEXT is the run's plist."
  (let ((writes (cl-remove-if-not (lambda (r) (eq (plist-get r :action) 'write)) rows)))
    (when (and writes (org-canvas--submissions-window-confirm (length writes)))
      (save-excursion
        (dolist (r writes) (org-canvas--submissions-window-write r context)))
      (when buffer-file-name (save-buffer))
      (length writes))))

;;;###autoload
(defun org-canvas-submissions-score-by-window (&optional file grace overwrite points)
  "Score a grading file by when each student submitted, against their section.
FILE is the grading file, as `org-canvas-open-submissions' takes it;
nil means the grading buffer at hand, or one chosen in the
minibuffer.  A student whose SUBMITTED_AT falls in one of their
sections' windows, widened by GRACE minutes either side, gets POINTS
\(nil: the file's POINTS_POSSIBLE); outside, or not submitted, gets 0.
A section's window is its row of the assignment's overrides table,
Unlock At to Lock At, or else its MEETS property in sections.org;
the sections are people.org's.  A student without a window is left
unscored, and so is a SCORE already there unless OVERWRITE.

The students outside the window, without one, and unsubmitted are
listed before anything is written; interactively the write is then
confirmed.  Each score written gets a Notes line naming its rule, and
the file is saved.  Nothing is sent: push with S.  GRACE nil is
`org-canvas-submissions-window-grace', asked for when interactive
\(where the prefix argument is OVERWRITE).  Return the buffer."
  ;; Bare `(interactive)': a sexp spec blanks undercover's line counts
  ;; for the whole body (see CLAUDE.md, and decisions.org, issue #280).
  (interactive)
  (let ((buf (org-canvas--submissions-window-target file)))
    (unless (or grace noninteractive)
      (setq grace (read-number "Grace, minutes either side: "
                               org-canvas-submissions-window-grace)
            overwrite (or overwrite current-prefix-arg)))
    (with-current-buffer buf
      (let* ((context (org-canvas--submissions-window-context
                       (or grace org-canvas-submissions-window-grace)
                       overwrite points))
             (rows (org-canvas--submissions-window-plan context)))
        (org-canvas--submissions-window-report rows context)
        (let ((written (org-canvas--submissions-window-apply rows context)))
          (dolist (r rows) (set-marker (plist-get r :marker) nil))
          (message "Section windows: %s; %s"
                   (org-canvas--submissions-window-summary rows context)
                   (if written
                       (format "%d written, press S to push" written)
                     "nothing written")))))
    buf))

(provide 'org-canvas-submissions-window)
;;; org-canvas-submissions-window.el ends here
