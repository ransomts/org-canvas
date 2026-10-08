;;; org-canvas-submissions-ruling.el --- Apply a grading ruling to listed students -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A ruling is an instructor's decision after grading, made for a list
;; of students rather than judged row by row: the submissions whose
;; Google Doc link did not open score zero, every row carrying one
;; fixed sentence; three late extra-credit submissions lose 20% of the
;; column off their rubric total.  Both were applied to grading files
;; with a Python pass over the Org text, which had to find SCORE when
;; it was absent, match the Rubric table whatever its padding, locate
;; Notes and Comment to post by heading, and keep a backup copy as
;; the only way back (issue #448).
;;
;; `org-canvas-submissions-apply-ruling' is that pass as a function a
;; batch run calls.  For each listed USER_ID it replaces the Rubric
;; rows' scores, their comments, the SCORE and the drafted comment as
;; told, and appends to the student's Notes a dated line with the
;; ruling's note and everything the ruling replaced, so it can be
;; undone by hand.  It returns what it changed, for the caller to check
;; against the list it passed.
;;
;; Everything is local: nothing is read from Canvas and nothing is
;; sent.  The changes go out with the grade push (S), as typed ones do.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
(require 'json)
;; A command file above the feature modules: it edits the grading files
;; the submissions module writes, through the comment import's writers.
(require 'org-canvas-submissions)
(require 'org-canvas-submissions-comments)

;;;; The Ruling

(defun org-canvas--submissions-ruling-empty-p (value)
  "Return non-nil when VALUE asks for an emptied place: `empty' or \"empty\"."
  (member value '(empty "empty")))

(defun org-canvas--submissions-ruling-check (ruling)
  "Signal a `user-error' unless RULING's values are ones it can apply.
RULING is the plist `org-canvas-submissions-apply-ruling' takes."
  (let ((rows (plist-get ruling :rows))
        (row-comment (plist-get ruling :row-comment))
        (score (plist-get ruling :score))
        (comment (plist-get ruling :comment)))
    (unless (or (null rows) (and (numberp rows) (>= rows 0))
                (org-canvas--submissions-ruling-empty-p rows))
      (user-error "Ruling :rows is %S; give a number, `empty' or nil" rows))
    (unless (or (null row-comment) (stringp row-comment)
                (org-canvas--submissions-ruling-empty-p row-comment))
      (user-error "Ruling :row-comment is %S; give a string, `empty' or nil" row-comment))
    (unless (or (null score) (numberp score) (functionp score))
      (user-error "Ruling :score is %S; give a number, a function or nil" score))
    (unless (or (null comment) (stringp comment))
      (user-error "Ruling :comment is %S; give a string or nil" comment))
    (when (and comment (string-match-p "^[ \t]*#" comment))
      (user-error "Ruling :comment has a line starting with #, which Org reads as a comment"))))

(defun org-canvas--submissions-ruling-row (ruling row)
  "Return ROW, a Rubric row, as an (ID SCORE COMMENT) triple after RULING."
  (let ((rows (plist-get ruling :rows))
        (row-comment (plist-get ruling :row-comment)))
    (list (nth 0 row)
          (cond ((null rows) (nth 3 row))
                ((numberp rows) (org-canvas--submissions-format-number rows)))
          (cond ((null row-comment) (nth 4 row))
                ((org-canvas--submissions-ruling-empty-p row-comment) nil)
                (t row-comment)))))

(defun org-canvas--submissions-ruling-number (text)
  "Return TEXT, a score cell or nil, as a number, or 0 for an empty cell."
  (let ((parsed (org-canvas--submissions-parse-score text)))
    (if (and parsed (not (equal parsed "EX"))) (string-to-number parsed) 0)))

(defun org-canvas--submissions-ruling-max (rows)
  "Return the column's points: POINTS_POSSIBLE, else the sum of ROWS' Max."
  (or (org-canvas--submissions-default-points)
      (apply #'+ (mapcar (lambda (r) (org-canvas--submissions-ruling-number (nth 2 r)))
                         rows))))

(defun org-canvas--submissions-ruling-score (ruling rows triples)
  "Return the SCORE set by RULING, as a string, or nil to leave SCORE alone.
ROWS are the entry's Rubric rows before the ruling and TRIPLES after
it.  A function is called with the rows' total after the ruling and
the column's points; what it returns below 0 is 0."
  (let ((score (plist-get ruling :score)))
    (cond ((null score) nil)
          ((numberp score) (org-canvas--submissions-format-number score))
          (t (let ((total (apply #'+ (mapcar (lambda (tr)
                                               (org-canvas--submissions-ruling-number
                                                (nth 1 tr)))
                                             triples))))
               (org-canvas--submissions-format-number
                (max 0 (funcall score total (org-canvas--submissions-ruling-max rows)))))))))

;;;; Planning

(defun org-canvas--submissions-ruling-disagrees (score triples)
  "Return why SCORE cannot stand beside TRIPLES, or nil when it can.
Only on a column whose grade comes from its rubric, where the push
refuses a SCORE that is not the rows' total."
  (let ((total (org-canvas--submissions-rubric-total triples)))
    (when (and score total (org-canvas--submissions-rubric-for-grading-p)
               (/= (string-to-number score) (string-to-number total)))
      (format "SCORE %s disagrees with the rubric total %s, and the rubric is used for grading"
              score total))))

(defun org-canvas--submissions-ruling-touches-rows-p (ruling)
  "Return non-nil when RULING is to replace Rubric scores or comments."
  (or (plist-get ruling :rows) (plist-get ruling :row-comment)))

(defun org-canvas--submissions-ruling-plan (ruling user-id)
  "Return what RULING does to USER-ID's entry at point, without writing.
A plist: :user-id, :name, :old-score and :score (the SCORE before and
after, :score nil when left), :old-rows and :rows ((ID SCORE COMMENT)
before and after), :draft (the new draft, or nil) and :old-draft.  A
refusal is (:user-id USER-ID :skipped WHY) instead."
  (let* ((rows (org-canvas--submissions-rubric-rows))
         (old (mapcar (lambda (r) (list (nth 0 r) (nth 3 r) (nth 4 r))) rows))
         (new (if (org-canvas--submissions-ruling-touches-rows-p ruling)
                  (mapcar (lambda (r) (org-canvas--submissions-ruling-row ruling r)) rows)
                old))
         (score (org-canvas--submissions-ruling-score ruling rows new))
         (why (cond ((org-canvas--submissions-left-p) "left the course")
                    ((and (org-canvas--submissions-ruling-touches-rows-p ruling) (null rows))
                     "no Rubric table to rule on")
                    ((org-canvas--submissions-ruling-disagrees score new)))))
    (if why
        (list :user-id user-id :skipped why)
      (list :user-id user-id :name (org-get-heading t t t t)
            :old-score (org-canvas--submissions-comments-prop "SCORE") :score score
            :old-rows old :rows new
            :old-draft (org-canvas--submissions-comment-draft)
            :draft (plist-get ruling :comment)))))

;;;; Writing

(defun org-canvas--submissions-ruling-before (plan)
  "Return what PLAN's ruling will replace, as one line for the Notes."
  (let ((record (org-canvas--submissions-typed-record))
        (draft (plist-get plan :old-draft)))
    (concat "Before: " (or record "nothing typed")
            (if (and draft (plist-get plan :draft))
                (format "; draft (%s)" (replace-regexp-in-string "\n+" " " draft))
              ""))))

(defun org-canvas--submissions-ruling-note (note before)
  "Return the Notes item for a ruling with NOTE, recording BEFORE.
NOTE's later lines, and BEFORE, are indented under the item."
  (concat (format "- Ruling %s" (format-time-string "<%Y-%m-%d %a %H:%M>"))
          (if note
              (concat ": " (replace-regexp-in-string "\n" "\n  " (string-trim note)))
            "")
          "\n  " before))

(defun org-canvas--submissions-ruling-ensure-draft ()
  "Add a Comment to post heading to the entry at point when it has none."
  (unless (org-canvas--submissions-draft-region)
    (save-excursion
      (org-end-of-subtree t t)
      (insert (if (bolp) "" "\n") "\n" org-canvas--submissions-draft-heading "\n"))))

(defun org-canvas--submissions-ruling-write (plan note)
  "Write PLAN into the entry at point, and its Notes item with NOTE."
  (let ((before (org-canvas--submissions-ruling-before plan)))
    (unless (equal (plist-get plan :rows) (plist-get plan :old-rows))
      (dolist (row (plist-get plan :rows))
        (org-canvas--submissions-rubric-set-row (nth 0 row) (nth 1 row) (nth 2 row))))
    (when (plist-get plan :score)
      (org-entry-put (point) "SCORE" (plist-get plan :score)))
    (when (plist-get plan :draft)
      (org-canvas--submissions-ruling-ensure-draft)
      (org-canvas--submissions-comments-set-draft (plist-get plan :draft)))
    (save-excursion
      (goto-char (org-canvas--submissions-notes-end))
      (insert (org-canvas--submissions-ruling-note note before))
      (unless (looking-at-p "\n") (insert "\n")))))

(defun org-canvas--submissions-ruling-student (ruling user-id markers write)
  "Return RULING's plan for USER-ID, found among MARKERS, written when WRITE.
An id no student heading carries gives (:user-id USER-ID :unmatched t)."
  (let ((marker (cdr (assoc user-id markers))))
    (if (null marker)
        (list :user-id user-id :unmatched t)
      (goto-char marker)
      (let ((plan (org-canvas--submissions-ruling-plan ruling user-id)))
        (when (and write (not (plist-get plan :skipped)))
          (org-canvas--submissions-ruling-write plan (plist-get ruling :note)))
        plan))))

(defun org-canvas--submissions-ruling-result (plans dry-run)
  "Return the ruling's result plist for PLANS; DRY-RUN as it ran."
  (list :changed (seq-filter (lambda (p) (plist-get p :name)) plans)
        :unmatched (mapcar (lambda (p) (plist-get p :user-id))
                           (seq-filter (lambda (p) (plist-get p :unmatched)) plans))
        :skipped (mapcar (lambda (p) (cons (plist-get p :user-id) (plist-get p :skipped)))
                         (seq-filter (lambda (p) (plist-get p :skipped)) plans))
        :dry-run (and dry-run t)))

(defun org-canvas--submissions-ruling-describe (plan)
  "Return one line saying what PLAN will change."
  (format "user %s (%s): %s%s%s"
          (plist-get plan :user-id) (plist-get plan :name)
          (if (plist-get plan :score)
              (format "SCORE %s -> %s" (or (plist-get plan :old-score) "-")
                      (plist-get plan :score))
            "SCORE left")
          (if (equal (plist-get plan :rows) (plist-get plan :old-rows))
              ""
            (format ", %d row(s)" (length (plist-get plan :rows))))
          (if (plist-get plan :draft) ", draft" "")))

(defun org-canvas--submissions-ruling-report (result)
  "Name the edits, misses and refusals in RESULT, then its summary."
  (let ((prefix (if (plist-get result :dry-run) "[DRY-RUN] " "")))
    (dolist (plan (plist-get result :changed))
      (org-canvas--user-message "%sRuling: %s" prefix
                                (org-canvas--submissions-ruling-describe plan)))
    (dolist (id (plist-get result :unmatched))
      (org-canvas--user-message "Ruling: user %s matched no student" id))
    (dolist (skip (plist-get result :skipped))
      (org-canvas--user-message "Ruling: kept back user %s: %s" (car skip) (cdr skip)))
    (org-canvas--user-message
     "%sRuling: %d student(s) %s, %d matched nothing, %d kept back%s"
     prefix (length (plist-get result :changed))
     (if (plist-get result :dry-run) "would change" "changed")
     (length (plist-get result :unmatched)) (length (plist-get result :skipped))
     (if (or (plist-get result :dry-run) (null (plist-get result :changed)))
         "" "; press S to push"))))

;;;; Command

;;;###autoload
(cl-defun org-canvas-submissions-apply-ruling (file user-ids &key rows row-comment
                                                    score comment note)
  "Apply a grading ruling to the students USER-IDS in grading file FILE.
FILE is taken as `org-canvas-submissions-export-comments' takes it:
nil for the grading buffer at hand, else an assignment id or a
grading file's path or name.  USER-IDS are USER_IDs, numbers or
strings.  For each listed student:

  ROWS         every Rubric row's Score: a number, `empty' to empty
               the cells, or nil to leave them;
  ROW-COMMENT  every Rubric row's comment: a string, `empty' to
               empty them, or nil to leave them;
  SCORE        the SCORE: a number, or a function called with the
               rows' total after the ruling and the column's points
               \(POINTS_POSSIBLE, else the rows' Max summed), as in
               (lambda (total max) (- total (* 0.2 max))), whose
               result below 0 is 0; nil leaves SCORE;
  COMMENT      the draft under Comment to post, replacing one there
               \(the heading is added when missing); nil leaves it;
  NOTE         the reason, written in the Notes item.

Every student changed gets a Notes item: the time, NOTE, and what
the ruling replaced (SCORE, the Rubric rows with their comments, and
a replaced draft), so it can be undone by hand.  A student who left
the course is kept back, and so is one without a Rubric table when
ROWS or ROW-COMMENT is given, and one whose new SCORE is not the new
rows' total on a column graded by its rubric, since the push would
refuse the whole file for it.  Whether emptied cells clear an
assessment on Canvas is the push's rule, not this function's.

Nothing is sent: the changes go out with the grade push (S).  Under
`org-canvas--dry-run' nothing is written and the result is the same.
The file is saved when anything changed.  Return a plist: :changed,
one plist per student changed (:user-id, :name, :old-score, :score,
:old-rows and :rows as (ID SCORE COMMENT) lists, :old-draft, :draft);
:unmatched, the ids no student carries; :skipped, (USER-ID . WHY)
for each student kept back; :dry-run."
  (let ((ruling (list :rows rows :row-comment row-comment :score score
                      :comment comment :note note)))
    (org-canvas--submissions-ruling-check ruling)
    (with-current-buffer (org-canvas--submissions-comments-buffer file)
      (let* ((dry-run org-canvas--dry-run)
             (markers (org-canvas--submissions-comments-markers))
             (plans (save-excursion
                      (mapcar (lambda (id)
                                (org-canvas--submissions-ruling-student
                                 ruling id markers (not dry-run)))
                              (delete-dups (mapcar (lambda (id) (format "%s" id))
                                                   user-ids)))))
             (result (org-canvas--submissions-ruling-result plans dry-run)))
        (dolist (m markers) (set-marker (cdr m) nil))
        (when (and buffer-file-name (not dry-run) (plist-get result :changed))
          (org-canvas--save-buffer))
        (org-canvas--submissions-ruling-report result)
        result))))

;;;; JSON

(defun org-canvas--submissions-ruling-json-score (value)
  "Return VALUE, a JSON ruling's score, as the ruling function takes it.
A number is itself; (:deduct-percent P) is the rows' total less P% of
the column's points."
  (if (and (consp value) (plist-member value :deduct-percent))
      (let ((percent (plist-get value :deduct-percent)))
        (unless (numberp percent)
          (user-error "Ruling score deduct_percent is %S; give a number" percent))
        (lambda (total max) (- total (* (/ percent 100.0) max))))
    value))

(defun org-canvas--submissions-ruling-from-json (path)
  "Return the rulings of the JSON file PATH, as plists.
The file holds one ruling object or an array of them."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false nil)
         (data (org-canvas--submissions-comments-from-json (json-read-file path))))
    (if (keywordp (car data)) (list data) data)))

(defun org-canvas-submissions-apply-ruling-json (file path)
  "Apply each ruling of the JSON file PATH to grading file FILE.
Each ruling is an object with user_ids (an array) and any of rows (a
number or \"empty\"), row_comment, score (a number, or
{\"deduct_percent\": P} for the rows' total less P% of the column's
points), comment and note, as `org-canvas-submissions-apply-ruling'
takes them.  Return the list of results, one per ruling."
  (mapcar (lambda (r)
            (org-canvas-submissions-apply-ruling
             file (plist-get r :user-ids)
             :rows (plist-get r :rows) :row-comment (plist-get r :row-comment)
             :score (org-canvas--submissions-ruling-json-score (plist-get r :score))
             :comment (plist-get r :comment) :note (plist-get r :note)))
          (org-canvas--submissions-ruling-from-json path)))

(provide 'org-canvas-submissions-ruling)
;;; org-canvas-submissions-ruling.el ends here
