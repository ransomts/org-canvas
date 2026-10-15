;;; org-canvas-submissions-grades.el --- Excuse students and import scores in a grading file -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Two jobs on a grading file that each ended in a scratch script over
;; the Org text.
;;
;; Excusing a column's students.  A column called off after it opened
;; (a cancelled session, a closer the clock ran out on) leaves every
;; student who did not submit marked Missing on Canvas, even once the
;; column no longer counts; the fix is to excuse them, which the web UI
;; does from the gradebook header and the grading file did one SCORE: EX
;; at a time (issue #501).  `org-canvas-submissions-excuse' writes EX
;; into every row with no submission, every row, or the rows at point or
;; in the region, leaving graded rows alone unless told.
;;
;; Importing a drafted grade set.  `org-canvas-submissions-import-comments'
;; (#438) writes back comment text only, so a column graded outside the
;; buffer (a second grader, a drafting pass writing JSON per student)
;; still had its Rubric cells, SCORE and Notes regex-edited in (issue
;; #483).  `org-canvas-submissions-import-grades' reads the same JSON,
;; writes each row's score, SCORE and a Notes line through the writers
;; the buffer uses, then imports the comments as the comment import does.
;;
;; Everything is local: nothing is read from Canvas and nothing is sent.
;; The changes go out with the grade push (S), as typed ones do.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules: it edits the grading files
;; the submissions module writes, through the comment import's and the
;; ruling's readers.
(require 'org-canvas-submissions)
(require 'org-canvas-submissions-comments)
(require 'org-canvas-submissions-ruling)

;;;; Excusing (issue #501)

(defconst org-canvas--submissions-excuse-scopes '(unsubmitted all point)
  "The scopes `org-canvas-submissions-excuse' takes.")

(defun org-canvas--submissions-excuse-verdict (scope overwrite)
  "Return what an excusal under SCOPE does to the entry at point.
`excuse' to write EX, `already' when the student is excused,
`submitted' for a submission the scope `unsubmitted' passes over, or
a string saying why the student is kept back.  A grade is SCORE, or
CANVAS_SCORE behind an empty SCORE; only OVERWRITE replaces one."
  (let ((grade (or (org-canvas--submissions-comments-prop "SCORE")
                   (org-canvas--submissions-comments-prop "CANVAS_SCORE"))))
    (cond ((org-canvas--submissions-left-p) "left the course")
          ((equal (org-canvas--submissions-parse-score grade) "EX") 'already)
          ((and (eq scope 'unsubmitted)
                (org-canvas--submissions-comments-prop "SUBMITTED_AT"))
           'submitted)
          ((and grade (not overwrite)) (format "graded %s" grade))
          (t 'excuse))))

(defun org-canvas--submissions-excuse-student (user-id scope overwrite write)
  "Return what an excusal does to USER-ID's entry at point, written when WRITE.
SCOPE and OVERWRITE are as `org-canvas--submissions-excuse-verdict'
takes them.  A plist: (:user-id :name :old-score) for a student
excused, (:user-id :already t) for one excused before, (:user-id
:skipped WHY) for one kept back; nil for one out of SCOPE."
  (pcase (org-canvas--submissions-excuse-verdict scope overwrite)
    ('submitted nil)
    ('already (list :user-id user-id :already t))
    ('excuse
     (let ((old (org-canvas--submissions-comments-prop "SCORE")))
       (when write (org-entry-put (point) "SCORE" "EX"))
       (list :user-id user-id :name (org-get-heading t t t t) :old-score old)))
    (why (list :user-id user-id :skipped why))))

(defun org-canvas--submissions-excuse-range ()
  "Return (START . END) of the student headings the region or point is in.
START is the student heading the region, or point, begins in; END the
region's end, or without a region the end of that student's subtree."
  (let* ((region (use-region-p))
         (start (if region (region-beginning) (point)))
         (end (and region (region-end))))
    (save-excursion
      (goto-char start)
      (if (org-before-first-heading-p)
          (unless region (user-error "Not on a student's heading"))
        (org-back-to-heading t)
        (while (> (org-current-level) 1) (outline-up-heading 1 t)))
      (cons (point) (or end (save-excursion (org-end-of-subtree t t) (point)))))))

(defun org-canvas--submissions-excuse-targets (scope markers)
  "Return the (USER-ID . MARKER) of MARKERS that SCOPE names.
Every one, except under `point', which takes the students whose
heading lies in `org-canvas--submissions-excuse-range'."
  (if (not (eq scope 'point))
      markers
    (let* ((range (org-canvas--submissions-excuse-range))
           (found (seq-filter (lambda (m) (and (>= (cdr m) (car range))
                                               (< (cdr m) (cdr range))))
                              markers)))
      (or found (user-error "No student heading at point or in the region")))))

(defun org-canvas--submissions-excuse-result (plans dry-run)
  "Return the excusal's result plist for PLANS; DRY-RUN as it ran."
  (let ((plans (delq nil plans)))
    (list :excused (seq-filter (lambda (p) (plist-get p :name)) plans)
          :already (mapcar (lambda (p) (plist-get p :user-id))
                           (seq-filter (lambda (p) (plist-get p :already)) plans))
          :skipped (mapcar (lambda (p) (cons (plist-get p :user-id) (plist-get p :skipped)))
                           (seq-filter (lambda (p) (plist-get p :skipped)) plans))
          :dry-run (and dry-run t))))

(defun org-canvas--submissions-excuse-report (result)
  "Name the students RESULT excused and kept back, then its summary."
  (let ((prefix (if (plist-get result :dry-run) "[DRY-RUN] " ""))
        (excused (plist-get result :excused)))
    (dolist (plan excused)
      (org-canvas--user-message "%sExcuse: user %s (%s): SCORE %s -> EX" prefix
                                (plist-get plan :user-id) (plist-get plan :name)
                                (or (plist-get plan :old-score) "-")))
    (dolist (skip (plist-get result :skipped))
      (org-canvas--user-message "Excuse: kept back user %s: %s" (car skip) (cdr skip)))
    (org-canvas--user-message
     "%sExcuse: %d student(s) %s, %d already excused, %d kept back%s"
     prefix (length excused)
     (if (plist-get result :dry-run) "would be excused" "excused")
     (length (plist-get result :already)) (length (plist-get result :skipped))
     (if (or (plist-get result :dry-run) (null excused)) "" "; press S to push"))))

(defun org-canvas--submissions-excuse-read-scope ()
  "Return the scope an interactive excusal takes: the region, else asked."
  (if (use-region-p)
      'point
    (intern (completing-read "Excuse which students? "
                             (mapcar #'symbol-name org-canvas--submissions-excuse-scopes)
                             nil t nil nil "unsubmitted"))))

;;;###autoload
(defun org-canvas-submissions-excuse (&optional file scope overwrite)
  "Excuse students of a grading file: write EX as their SCORE.
FILE is taken as `org-canvas-submissions-export-comments' takes it:
nil for the grading buffer at hand, else an assignment id or a
grading file's path or name.  SCOPE names the students:

  `unsubmitted' (or nil)  every student with no submission, no
                          SUBMITTED_AT: the students Canvas marks
                          Missing on a column called off;
  `all'                   every student;
  `point'                 the student at point, or every student
                          heading in the active region.

A student already graded, by a SCORE or a CANVAS_SCORE behind an
empty one, is kept back unless OVERWRITE; one who left the course
\(STATUS left) is always kept back, since the push sends nothing for
a student who left; one already excused is counted and left.
Interactively an active region is `point', else SCOPE is asked for,
and a prefix argument is OVERWRITE.

Only the grading file is written: the grade push (S) sends EX as
`excused', as it does a hand-typed one.  Under `org-canvas--dry-run'
nothing is written and the result is the same.  The file is saved
when anything changed.  Return a plist: :excused, one plist per
student excused (:user-id, :name, :old-score); :already, the ids
excused before; :skipped, (USER-ID . WHY) for each student kept back;
:dry-run."
  ;; Bare `(interactive)': a sexp spec blanks undercover's line counts
  ;; (see documentation/testing-guide.md).
  (interactive)
  (when (called-interactively-p 'any)
    (setq scope (org-canvas--submissions-excuse-read-scope)
          overwrite (and current-prefix-arg t)))
  (let ((scope (or scope 'unsubmitted)))
    (unless (memq scope org-canvas--submissions-excuse-scopes)
      (user-error "Excuse scope %S; give unsubmitted, all or point" scope))
    (with-current-buffer (org-canvas--submissions-comments-buffer file)
      (let* ((dry-run org-canvas--dry-run)
             (markers (org-canvas--submissions-comments-markers))
             (plans (unwind-protect
                        (save-excursion
                          (mapcar (lambda (m)
                                    (goto-char (cdr m))
                                    (org-canvas--submissions-excuse-student
                                     (car m) scope overwrite (not dry-run)))
                                  (org-canvas--submissions-excuse-targets scope markers)))
                      (dolist (m markers) (set-marker (cdr m) nil))))
             (result (org-canvas--submissions-excuse-result plans dry-run)))
        (when (and buffer-file-name (not dry-run) (plist-get result :excused))
          (org-canvas--save-buffer))
        (org-canvas--submissions-excuse-report result)
        result))))

;;;; Importing Grades (issue #483)

(defun org-canvas--submissions-grades-blank-p (value)
  "Return non-nil when VALUE, an imported value, is empty: nil or blank text."
  (or (null value) (and (stringp value) (string-empty-p (string-trim value)))))

(defun org-canvas--submissions-grades-number (value)
  "Return VALUE, an imported score or a cell's text, as a number, or nil.
A string is read as SCORE is (\"95/100\" is 95); EX is no number."
  (cond ((numberp value) value)
        ((stringp value)
         (let ((parsed (org-canvas--submissions-parse-score value)))
           (and parsed (not (equal parsed "EX")) (string-to-number parsed))))))

(defun org-canvas--submissions-grades-format (n)
  "Return the number N as a Score cell spells it, 3 for 3.0 and 2.25 unrounded."
  (if (= n (truncate n)) (format "%d" (truncate n)) (number-to-string n)))

(defun org-canvas--submissions-grades-raw (value)
  "Return VALUE, an imported score, as text to compare with a cell."
  (if (numberp value)
      (org-canvas--submissions-grades-format value)
    (string-trim (format "%s" value))))

(defun org-canvas--submissions-grades-same-p (new old)
  "Return non-nil when score texts NEW and OLD are the same score."
  (let ((a (org-canvas--submissions-grades-number new))
        (b (org-canvas--submissions-grades-number old)))
    (if (and a b)
        (= a b)
      (let ((parsed (org-canvas--submissions-parse-score new)))
        (and parsed (equal parsed (org-canvas--submissions-parse-score old)))))))

(defun org-canvas--submissions-grades-row (row current)
  "Return the outcome of ROW's imported score against CURRENT, or nil.
CURRENT is the entry's Rubric rows.  (:scores WHAT WRITE CELL) for a
cell to write, WRITE writing CELL; `unmatched' for a row the entry
lacks; `refused' for a score that is no number from 0 to the row's
Max.  Nil when the cell already holds the score."
  (let* ((id (format "%s" (plist-get row :criterion-id)))
         (old (assoc id current))
         (value (plist-get row :score))
         (n (org-canvas--submissions-grades-number value))
         (max (and old (org-canvas--submissions-grades-number (nth 2 old))))
         (what (format "criterion %s" id)))
    (cond ((null old) (list 'unmatched what))
          ((equal (org-canvas--submissions-grades-raw value) (nth 3 old)) nil)
          ((or (null n) (< n 0) (and max (> n max)))
           (list 'refused (format "%s: score %s is not a number from 0 to its Max %s"
                                  what (org-canvas--submissions-grades-raw value)
                                  (or (nth 2 old) "-"))))
          ((org-canvas--submissions-grades-same-p (nth 3 old) value) nil)
          (t (let ((cell (org-canvas--submissions-grades-format n)))
               (list :scores what
                     (lambda ()
                       (org-canvas--submissions-rubric-set-score
                        id cell (org-canvas--submissions-section-region
                                 org-canvas--submissions-rubric-heading)))
                     cell))))))

(defun org-canvas--submissions-grades-rows (student current)
  "Return the outcomes of STUDENT's imported row scores against CURRENT.
A row with no score, or an empty one, is left alone."
  (delq nil (mapcar (lambda (row)
                      (unless (org-canvas--submissions-grades-blank-p (plist-get row :score))
                        (org-canvas--submissions-grades-row row current)))
                    (plist-get student :rows))))

(defun org-canvas--submissions-grades-scored-p (student)
  "Return non-nil when STUDENT's import carries a score for any row."
  (cl-some (lambda (row) (not (org-canvas--submissions-grades-blank-p (plist-get row :score))))
           (plist-get student :rows)))

(defun org-canvas--submissions-grades-after (current outcomes)
  "Return CURRENT, the entry's Rubric rows, with OUTCOMES' cells written in."
  (mapcar (lambda (r)
            (let ((o (seq-find (lambda (o) (and (eq (car o) :scores)
                                                (equal (cadr o) (format "criterion %s" (car r)))))
                               outcomes)))
              (if o
                  (list (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 o) (nth 4 r))
                r)))
          current))

(defun org-canvas--submissions-grades-sum (rows)
  "Return the sum of ROWS' scores as text, or nil unless every row is scored."
  (let ((scores (mapcar (lambda (r) (org-canvas--submissions-grades-number (nth 3 r))) rows)))
    (and scores (not (memq nil scores))
         (org-canvas--submissions-grades-format (apply #'+ scores)))))

(defun org-canvas--submissions-grades-new-score (student rows)
  "Return the SCORE STUDENT asks for as text, nil for none, or `bad'.
ROWS are the entry's Rubric rows as the import leaves them.  Without a
score key it is ROWS' sum, when every row is scored and the import
scored any; an empty score asks for none."
  (let* ((value (plist-get student :score))
         (n (org-canvas--submissions-grades-number value)))
    (cond ((not (plist-member student :score))
           (and (org-canvas--submissions-grades-scored-p student)
                (org-canvas--submissions-grades-sum rows)))
          ((org-canvas--submissions-grades-blank-p value) nil)
          ((and (stringp value) (equal (org-canvas--submissions-parse-score value) "EX"))
           "EX")
          ((and n (>= n 0)) (org-canvas--submissions-grades-format n))
          (t 'bad))))

(defun org-canvas--submissions-grades-score (student rows)
  "Return the outcome of STUDENT's SCORE at point beside ROWS, or nil.
ROWS are the Rubric rows as the import leaves them.  A SCORE that is
no number or EX is refused, and so is one the rows' total contradicts
on a column graded by its rubric, which the push would refuse."
  (let* ((new (org-canvas--submissions-grades-new-score student rows))
         (old (org-canvas--submissions-comments-prop "SCORE"))
         (why (and (stringp new)
                   (org-canvas--submissions-ruling-disagrees
                    new (mapcar (lambda (r) (list (car r) (nth 3 r))) rows)))))
    (cond ((null new) nil)
          ((eq new 'bad)
           (list 'refused (format "SCORE %s is not a number or EX"
                                  (org-canvas--submissions-grades-raw (plist-get student :score)))))
          ((and old (org-canvas--submissions-grades-same-p new old)) nil)
          (why (list 'refused why))
          (t (list :score (format "SCORE %s" new)
                   (lambda () (org-entry-put (point) "SCORE" new)))))))

(defun org-canvas--submissions-grades-append-notes (text)
  "Write TEXT on new lines at the end of the Notes of the entry at point."
  (save-excursion
    (goto-char (org-canvas--submissions-notes-end))
    (insert text)
    (unless (looking-at-p "\n") (insert "\n"))))

(defun org-canvas--submissions-grades-notes (student)
  "Return the outcome of STUDENT's imported notes at point, or nil.
The text is added at the end of the Notes, never over them; a text
the Notes already hold is no change, so an export imported back
changes nothing.  A line starting with * or # is refused, since Org
reads it as a heading or a comment."
  (let ((text (org-canvas--submissions-comments-text (plist-get student :notes))))
    (unless (org-canvas--submissions-grades-blank-p text)
      (let ((new (string-trim text))
            (have (org-canvas--submissions-section-text
                   org-canvas--submissions-notes-heading)))
        (cond ((string-match-p "^\\(?:\\*+[ \t]\\|[ \t]*#\\)" new)
               (list 'refused "notes: a line starts with * or #, which Org reads as a heading or a comment"))
              ((and have (string-search new have)) nil)
              (t (list :notes "notes"
                       (lambda () (org-canvas--submissions-grades-append-notes new)))))))))

(defun org-canvas--submissions-grades-held-p (student)
  "Return non-nil when STUDENT is held: an empty score beside notes."
  (and (plist-member student :score)
       (org-canvas--submissions-grades-blank-p (plist-get student :score))
       (not (org-canvas--submissions-grades-blank-p (plist-get student :notes)))))

(defun org-canvas--submissions-grades-at-point (student)
  "Return the outcomes of STUDENT at its heading, without writing.
Row scores first, then SCORE, then notes, the order they are written
in.  A held student gets the notes alone and a `held' outcome."
  (let ((notes (org-canvas--submissions-grades-notes student)))
    (if (org-canvas--submissions-grades-held-p student)
        (cons (list 'held "held: notes only, no score") (and notes (list notes)))
      (let* ((current (org-canvas--submissions-rubric-rows))
             (rows (org-canvas--submissions-grades-rows student current))
             (score (org-canvas--submissions-grades-score
                     student (org-canvas--submissions-grades-after current rows))))
        (append rows (and score (list score)) (and notes (list notes)))))))

(defun org-canvas--submissions-grades-hold-back (outcomes why)
  "Return OUTCOMES with every change kept back as `posted' for WHY."
  (mapcar (lambda (o)
            (if (org-canvas--submissions-comments-change-p o)
                (list 'posted (concat (cadr o) ": " why))
              o))
          outcomes))

(defun org-canvas--submissions-grades-plan (student allow-posted)
  "Return the outcomes of STUDENT at its heading, held back when due.
A student who left keeps every change back, so does one whose grade
is posted (POSTED_AT) unless ALLOW-POSTED, and so does one with a
refused value, since half a grade is worse than none."
  (let* ((plan (org-canvas--submissions-grades-at-point student))
         (why (cond ((not (cl-some #'org-canvas--submissions-comments-change-p plan)) nil)
                    ((org-canvas--submissions-left-p) "left the course")
                    ((and (org-canvas--submissions-comments-prop "POSTED_AT")
                          (not allow-posted))
                     "grade already posted")
                    ((assq 'refused plan) "another value for this student was refused"))))
    (if why (org-canvas--submissions-grades-hold-back plan why) plan)))

(defun org-canvas--submissions-grades-student (student markers allow-posted write)
  "Return the outcomes of STUDENT, each (KIND WHAT) with WHAT naming the user.
MARKERS are the file's (USER-ID . MARKER); a posted student's changes
go through only with ALLOW-POSTED, and they are written when WRITE."
  (let* ((user-id (format "%s" (plist-get student :user-id)))
         (marker (cdr (assoc user-id markers))))
    (if (null marker)
        (list (list 'unmatched (format "user %s" user-id)))
      (goto-char marker)
      (let ((plan (org-canvas--submissions-grades-plan student allow-posted)))
        (when write
          (dolist (o plan)
            (when (org-canvas--submissions-comments-change-p o) (funcall (nth 2 o)))))
        (mapcar (lambda (o) (list (car o) (format "user %s %s" user-id (cadr o))))
                plan)))))

(defun org-canvas--submissions-grades-tally (outcomes dry-run)
  "Return the grade import's result plist for OUTCOMES; DRY-RUN as it ran."
  (let ((of (lambda (kinds)
              (mapcar #'cadr (seq-filter (lambda (o) (memq (car o) kinds)) outcomes)))))
    (list :scores (cl-count :scores outcomes :key #'car)
          :score (cl-count :score outcomes :key #'car)
          :notes (cl-count :notes outcomes :key #'car)
          :held (funcall of '(held))
          :unmatched (funcall of '(unmatched))
          :skipped (funcall of '(posted refused))
          :dry-run (and dry-run t))))

(defun org-canvas--submissions-grades-import (students allow-posted)
  "Write STUDENTS' scores and notes into this grading buffer; return the tally.
A posted student's changes are kept back unless ALLOW-POSTED.  Under
`org-canvas--dry-run' nothing is written and the tally is the same."
  (let* ((dry-run org-canvas--dry-run)
         (markers (org-canvas--submissions-comments-markers))
         (outcomes (unwind-protect
                       (save-excursion
                         (apply #'append
                                (mapcar (lambda (s)
                                          (org-canvas--submissions-grades-student
                                           s markers allow-posted (not dry-run)))
                                        students)))
                     (dolist (m markers) (set-marker (cdr m) nil)))))
    (when (and buffer-file-name (not dry-run))
      (org-canvas--save-buffer))
    (org-canvas--submissions-grades-tally outcomes dry-run)))

(defun org-canvas--submissions-grades-merge (grades comments)
  "Return the result of an import whose grade half is GRADES and comments COMMENTS."
  (list :scores (plist-get grades :scores) :score (plist-get grades :score)
        :notes (plist-get grades :notes) :held (plist-get grades :held)
        :rows (plist-get comments :rows) :drafts (plist-get comments :drafts)
        :sent (plist-get comments :sent)
        :unmatched (delete-dups (append (plist-get grades :unmatched)
                                        (plist-get comments :unmatched)))
        :skipped (append (plist-get grades :skipped) (plist-get comments :skipped))
        :dry-run (plist-get grades :dry-run)))

(defun org-canvas--submissions-grades-input (data)
  "Return the students DATA gives: a list as is, a file name read as JSON.
Nil asks for the file, or is a `user-error' under `noninteractive'."
  (cond ((consp data) data)
        ((stringp data) (org-canvas--submissions-comments-read-json data))
        (noninteractive (user-error "No grades to import; pass a JSON file"))
        (t (org-canvas--submissions-comments-read-json
            (read-file-name "Import grades from JSON: " nil nil t)))))

(defun org-canvas--submissions-grades-summary (result)
  "Return the one-line summary of RESULT, a grade import's."
  (format "%sGrades: %d row score(s), %d SCORE(s), %d note(s), %d row comment(s), %d draft(s), %d sent comment(s) %s; %d key(s) matched nothing, %d kept back, %d held"
          (if (plist-get result :dry-run) "[DRY-RUN] " "")
          (plist-get result :scores) (plist-get result :score) (plist-get result :notes)
          (plist-get result :rows) (plist-get result :drafts) (plist-get result :sent)
          (if (plist-get result :dry-run) "would change" "changed")
          (length (plist-get result :unmatched)) (length (plist-get result :skipped))
          (length (plist-get result :held))))

(defun org-canvas--submissions-grades-report (result)
  "Name RESULT's unmatched, kept-back and held keys, then its summary."
  (dolist (what (plist-get result :unmatched))
    (org-canvas--user-message "Grades: %s matched nothing" what))
  (dolist (what (plist-get result :skipped))
    (org-canvas--user-message "Grades: kept back %s" what))
  (dolist (what (plist-get result :held))
    (org-canvas--user-message "Grades: %s" what))
  (org-canvas--user-message "%s" (org-canvas--submissions-grades-summary result)))

;;;###autoload
(defun org-canvas-submissions-import-grades (&optional file data allow-posted)
  "Write a drafted grade set into a grading file: scores, notes and comments.
FILE is taken as `org-canvas-submissions-export-comments' takes it.
DATA is a list of student plists shaped as that export returns them,
or the name of a JSON file shaped as it writes them; nil asks for the
file.  Each student is found by :user-id, and only the keys present
are read:

  a row's :score   the Rubric cell of its :criterion-id, a number
                   from 0 to the row's Max; an empty one is left;
  :score           SCORE, a number or EX.  Without the key, and with
                   every row scored and some scored by the import,
                   SCORE is the rows' sum, which is what the push
                   sends for a rubric used for grading.  An empty
                   score beside :notes holds the student: the notes go
                   in and no score, row or SCORE;
  :notes           a text added at the end of the Notes, never over
                   them; a text they already hold is no change.

Then the comment keys (a row's :comment, :draft, a sent comment's
:text) are imported as `org-canvas-submissions-import-comments'
imports them, so one file does the whole round trip, and importing an
unchanged export changes nothing.

A student kept back keeps every grade change back: one who left the
course, one whose grade is posted (POSTED_AT) unless ALLOW-POSTED,
and one with a value refused (a score off its row's range, a SCORE
the rows' total contradicts on a column graded by its rubric, notes
with a line starting with * or #).  Nothing is sent: the changes go
out with the grade push (S).  Under `org-canvas--dry-run' nothing is
written.  The file is saved when anything changed.

Return a plist: :scores, :score and :notes, the row cells, SCORE
values and notes changed (or under a dry run, that would be); :held,
the students held; :rows, :drafts and :sent, as the comment import
counts them; :unmatched, the keys that matched nothing (\"user 5001
criterion _812\"); :skipped, the changes kept back with why; :dry-run."
  ;; Bare `(interactive)': a sexp spec blanks undercover's line counts.
  (interactive)
  (with-current-buffer (org-canvas--submissions-comments-buffer file)
    (let* ((students (org-canvas--submissions-grades-input data))
           (grades (org-canvas--submissions-grades-import students allow-posted))
           (comments (org-canvas--submissions-comments-import students allow-posted))
           (result (org-canvas--submissions-grades-merge grades comments)))
      (org-canvas--submissions-grades-report result)
      result)))

(provide 'org-canvas-submissions-grades)
;;; org-canvas-submissions-grades.el ends here
