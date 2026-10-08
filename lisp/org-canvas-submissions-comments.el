;;; org-canvas-submissions-comments.el --- Export, import and check a grading file's comments -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A grading file holds three kinds of comment text: each Rubric row's
;; comment (the `- ID :: text' items under the table, #263), the draft
;; under `** Comment to post', and the comments already sent, under
;; `** Comments' with their ids (#419).  They could be read and changed
;; one student at a time in the buffer, so a course-wide rewrite of
;; 1,617 row comments ran on two scratch scripts that parsed the Org
;; text with regular expressions of their own (issue #438).
;;
;; This file is those scripts as commands, built on the readers and
;; writers the push itself uses:
;;
;;   - `org-canvas-submissions-export-comments' returns every student's
;;     comments as data, and writes them as JSON for a script;
;;   - `org-canvas-submissions-import-comments' writes new text back by
;;     (user id, criterion id), user id, and (user id, comment id), and
;;     nothing else, reporting what changed and what matched nothing;
;;   - `org-canvas-submissions-check-comments' lists every deducted row
;;     without a comment, every text a course's own regexps flag, and
;;     every sentence many students were given word for word.
;;
;; Everything is local: nothing is read from Canvas and nothing is sent.
;; An imported sent comment is an edit the comment push (C) sends, as a
;; hand edit is, and the rest goes out with the grade push (S).

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
(require 'json)
;; A command file above the feature modules: it reads and writes the
;; grading files the submissions module writes.
(require 'org-canvas-submissions)

(defcustom org-canvas-submissions-comment-flag-regexps nil
  "Regexps a comment must not match, for `org-canvas-submissions-check-comments'.
Each is matched against every row comment, draft and sent comment of
a student not yet posted; a match is an error, which fails the batch
check.  Matching follows `case-fold-search', so it ignores case unless
that is nil.  A course's feedback guide goes here: a dash it bans, a
word it does not want a student to read."
  :type '(repeat regexp)
  :group 'org-canvas)

(defcustom org-canvas-submissions-comment-shared-threshold 3
  "Most students one sentence may be given before the check names it.
A sentence of `org-canvas-submissions-comment-sentence-min-length' or
more characters found word for word in the comments of more students
than this, in one grading file, is listed as a warning by
`org-canvas-submissions-check-comments'.  Nil turns the count off."
  :type '(choice (integer :tag "Students") (const :tag "Off" nil))
  :group 'org-canvas)

(defcustom org-canvas-submissions-comment-sentence-min-length 25
  "Shortest sentence, in characters, the shared-sentence count considers.
Shorter ones (\"Nice work.\") are left out."
  :type 'integer
  :group 'org-canvas)

(defconst org-canvas--submissions-comments-check-buffer "*canvas-comment-check*"
  "Buffer the comment check's report is rendered into.")

(defconst org-canvas--submissions-comments-json-lists '(:rows :sent :students)
  "Keys whose value is a list of plists, written as a JSON array.")

(defconst org-canvas--submissions-comments-json-booleans '(:delete)
  "Keys whose value is a flag, written as a JSON boolean.")

;;;; Target

(defun org-canvas--submissions-comments-buffer (file)
  "Return the grading buffer FILE names, in the detail view.
FILE is nil for the grading buffer at hand, else an assignment id or
a grading file's path or name, as `org-canvas-push-submission-grades'
takes it; nil outside a grading buffer asks for the file."
  (let ((buf (org-canvas--submissions-comment-edits-buffer file)))
    (with-current-buffer buf
      (org-canvas--submissions-ensure-context)
      (unless (eq org-canvas-submissions--current-view 'detail)
        (user-error "Switch to detail view first (press v)")))
    buf))

;;;; Reading

(defun org-canvas--submissions-comments-markers ()
  "Return (USER-ID . MARKER) for each student heading of this grading file.
The markers are taken before anything is written (Hard Rule 9); the
caller releases them."
  (let ((found nil))
    (org-map-entries
     (lambda ()
       (when-let* ((user-id (org-entry-get (point) "USER_ID")))
         (push (cons user-id (point-marker)) found)))
     "LEVEL=1")
    (nreverse found)))

(defun org-canvas--submissions-comments-prop (name)
  "Return property NAME of the heading at point, nil when absent or empty."
  (let ((value (org-entry-get (point) name)))
    (and value (not (string-empty-p value)) value)))

(defun org-canvas--submissions-comments-row (row)
  "Return ROW, a Rubric row (ID CRITERION MAX SCORE COMMENT), as a plist."
  (list :criterion-id (nth 0 row) :criterion (nth 1 row) :max (nth 2 row)
        :score (nth 3 row) :comment (nth 4 row)))

(defun org-canvas--submissions-comments-sent (item)
  "Return ITEM, a sent comment of `org-canvas--submissions-sent-comments'.
The plist has :id, :author and :time from the item's label, :delete
when it is marked for deletion, and :text."
  (let* ((label (plist-get item :label))
         (author (and (string-match "\\`\\*\\(.+?\\)\\*" label)
                      (match-string 1 label)))
         (time (and (string-match "<[^>]+>" label) (match-string 0 label))))
    (list :id (plist-get item :id) :author author :time time
          :delete (plist-get item :delete) :text (plist-get item :text))))

(defun org-canvas--submissions-comments-at-point (user-id)
  "Return the comments of USER-ID's heading at point as a plist.
See `org-canvas-submissions-export-comments' for its keys."
  (list :user-id user-id
        :name (org-get-heading t t t t)
        :status (org-canvas--submissions-comments-prop "STATUS")
        :score (org-canvas--submissions-comments-prop "SCORE")
        :posted (org-canvas--submissions-comments-prop "POSTED_AT")
        :rows (mapcar #'org-canvas--submissions-comments-row
                      (org-canvas--submissions-rubric-rows))
        :draft (org-canvas--submissions-comment-draft)
        :sent (mapcar #'org-canvas--submissions-comments-sent
                      (org-canvas--submissions-sent-comments))
        :notes (org-canvas--submissions-section-text
                org-canvas--submissions-notes-heading)))

(defun org-canvas--submissions-comments-collect ()
  "Return the comments of every student heading here, in file order."
  (save-excursion
    (mapcar (lambda (entry)
              (goto-char (cdr entry))
              (prog1 (org-canvas--submissions-comments-at-point (car entry))
                (set-marker (cdr entry) nil)))
            (org-canvas--submissions-comments-markers))))

;;;; JSON

(defun org-canvas--submissions-comments-json-key (keyword)
  "Return KEYWORD as a JSON key symbol, its dashes as underscores."
  (intern (replace-regexp-in-string "-" "_" (substring (symbol-name keyword) 1))))

(defun org-canvas--submissions-comments-json-value (key value)
  "Return VALUE, KEY's in a plist, as `json-encode' should see it."
  (cond ((memq key org-canvas--submissions-comments-json-lists)
         (vconcat (mapcar #'org-canvas--submissions-comments-to-json value)))
        ((memq key org-canvas--submissions-comments-json-booleans)
         (if value t :json-false))
        (t value)))

(defun org-canvas--submissions-comments-to-json (plist)
  "Return PLIST as an alist for `json-encode', its keys in snake_case."
  (let ((alist nil))
    (while plist
      (let ((key (pop plist)) (value (pop plist)))
        (push (cons (org-canvas--submissions-comments-json-key key)
                    (org-canvas--submissions-comments-json-value key value))
              alist)))
    (nreverse alist)))

(defun org-canvas--submissions-comments-from-json (value)
  "Return VALUE, as `json-read' gave it, with each key's underscores as dashes."
  (cond ((and (consp value) (keywordp (car value)))
         (let ((out nil))
           (while value
             (push (intern (replace-regexp-in-string
                            "_" "-" (symbol-name (pop value))))
                   out)
             (push (org-canvas--submissions-comments-from-json (pop value)) out))
           (nreverse out)))
        ((consp value) (mapcar #'org-canvas--submissions-comments-from-json value))
        (t value)))

(defun org-canvas--submissions-comments-json (students)
  "Return STUDENTS, exported comments, as the JSON text of this grading file.
The object carries the file's assignment_id and assignment beside them."
  (let ((json-encoding-pretty-print t))
    (json-encode
     (org-canvas--submissions-comments-to-json
      (list :assignment-id org-canvas-submissions--assignment-id
            :assignment org-canvas-submissions--assignment-name
            :students students)))))

(defun org-canvas--submissions-comments-write-json (text output)
  "Write TEXT to OUTPUT, a file name, or to standard output when it is \"-\"."
  (if (equal output "-")
      (princ (concat text "\n"))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (concat text "\n") nil output nil 'silent))))

(defun org-canvas--submissions-comments-read-json (path)
  "Return the students of the JSON file PATH, as exported plists.
The file is an export's object, whose students are read, or a bare
array of students; either may carry only the keys to change."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false nil)
         (data (org-canvas--submissions-comments-from-json (json-read-file path))))
    (if (keywordp (car data)) (plist-get data :students) data)))

;;;###autoload
(defun org-canvas-submissions-export-comments (&optional file output)
  "Return a grading file's comments, one plist per student, in file order.
FILE is nil for the grading buffer at hand, else an assignment id or
a grading file's path or name, as `org-canvas-push-submission-grades'
takes it; nil elsewhere asks for the file.  With OUTPUT, a file name,
the comments are also written there as JSON, or printed when OUTPUT
is \"-\"; interactively OUTPUT is asked for.

Each plist has :user-id (the USER_ID, a string), :name, :status,
:score, :posted (POSTED_AT, nil until the grade is posted), :rows,
:draft (the Comment to post text, or nil), :sent and :notes.  A row is
\(:criterion-id :criterion :max :score :comment), strings as the
Rubric table spells them, nil when empty; a sent comment is (:id
:author :time :delete :text).  The JSON is an object with
assignment_id, assignment and students, each key in snake_case
\(user_id, criterion_id).  Every student is exported, posted ones
too: :posted says which, and `org-canvas-submissions-import-comments'
leaves them alone unless told otherwise.  Nothing is changed."
  ;; Bare `(interactive)': a sexp spec blanks undercover's line counts
  ;; for the whole body (see documentation/testing-guide.md, and
  ;; decisions.org, issue #280).
  (interactive)
  (let ((buf (org-canvas--submissions-comments-buffer file)))
    (when (and (null output) (called-interactively-p 'any))
      (setq output (read-file-name "Write the comments as JSON to: ")))
    (with-current-buffer buf
      (let ((students (org-canvas--submissions-comments-collect)))
        (when output
          (org-canvas--submissions-comments-write-json
           (org-canvas--submissions-comments-json students) output)
          (unless (equal output "-")
            (message "Comments of %d student(s) written to %s"
                     (length students) output)))
        students))))

;;;; Importing

(defun org-canvas--submissions-comments-text (value)
  "Return VALUE, an imported text, as a string, or nil for none."
  (cond ((null value) nil)
        ((stringp value) value)
        (t (format "%s" value))))

(defun org-canvas--submissions-comments-set-row (id text)
  "Write TEXT as the comment of Rubric row ID of the entry at point."
  (org-canvas--submissions-rubric-set-comment
   id text (org-canvas--submissions-section-region
            org-canvas--submissions-rubric-heading)))

(defun org-canvas--submissions-comments-import-row (row current write)
  "Return the outcome of ROW, an imported row, against CURRENT, or nil.
CURRENT is the entry's Rubric rows.  The outcome is (KIND WHAT):
:rows when the comment changes, written when WRITE; `unmatched' when
the entry has no such row.  Nil when the comment is unchanged."
  (let* ((id (format "%s" (plist-get row :criterion-id)))
         (old (assoc id current))
         (new (org-canvas--submissions-comment-text
               (org-canvas--submissions-comments-text (plist-get row :comment))))
         (what (format "criterion %s" id)))
    (cond ((null old) (list 'unmatched what))
          ((equal new (nth 4 old)) nil)
          (t (when write (org-canvas--submissions-comments-set-row id new))
             (list :rows what)))))

(defun org-canvas--submissions-comments-import-rows (student write)
  "Return the outcomes of STUDENT's imported row comments at point.
Only a row carrying :comment is compared; WRITE writes the changes."
  (let ((current (org-canvas--submissions-rubric-rows)))
    (delq nil (mapcar (lambda (row)
                        (when (plist-member row :comment)
                          (org-canvas--submissions-comments-import-row row current write)))
                      (plist-get student :rows)))))

(defun org-canvas--submissions-comments-set-draft (text)
  "Write TEXT under the entry's Comment to post, keeping its comment lines.
The template, and any other Org comment line there, stays above it.
Only the section's text is replaced: the blank lines after it, before
the next heading, are left as they were, so the file moves by the
draft's own lines and no more."
  (let* ((region (org-canvas--submissions-draft-region))
         (end (save-excursion (goto-char (cdr region))
                              (skip-chars-backward " \t\n" (car region))
                              (point)))
         (kept (seq-filter (lambda (l) (string-match-p "\\`[ \t]*#" l))
                           (split-string (buffer-substring-no-properties (car region) end)
                                         "\n"))))
    (save-excursion
      (goto-char (car region))
      (delete-region (car region) end)
      (unless (bolp) (insert "\n"))
      (insert (string-join (append kept (list text)) "\n"))
      (unless (eq (char-after) ?\n) (insert "\n")))))

(defun org-canvas--submissions-comments-import-draft (student write)
  "Return the outcome of STUDENT's imported draft at point, as a list.
Nothing without :draft; a change is written when WRITE.  A draft with
a line starting with # is refused, since Org reads that line as a
comment and the push would leave it out."
  (when (plist-member student :draft)
    (let* ((raw (or (org-canvas--submissions-comments-text (plist-get student :draft)) ""))
           (new (org-canvas--submissions-section-normalize raw)))
      (cond ((null (org-canvas--submissions-draft-region))
             (list (list 'unmatched "draft (no Comment to post heading)")))
            ((string-match-p "^[ \t]*#" raw)
             (list (list 'refused "draft: a line starts with #, which Org reads as a comment")))
            ((equal new (org-canvas--submissions-comment-draft)) nil)
            (t (when write (org-canvas--submissions-comments-set-draft new))
               (list (list :drafts "draft")))))))

(defun org-canvas--submissions-comments-import-one-sent (comment items write)
  "Return the outcome of COMMENT, an imported sent comment, or nil.
ITEMS are the entry's sent comments as read before any write; a
change is written when WRITE.  An emptied text is refused: a sent
comment is deleted by its DELETE mark, never by emptying it (issue
#419)."
  (let* ((id (format "%s" (plist-get comment :id)))
         (item (seq-find (lambda (i) (equal (plist-get i :id) id)) items))
         (new (org-canvas--submissions-comment-text
               (org-canvas--submissions-comments-text (plist-get comment :text))))
         (what (format "comment %s" id)))
    (cond ((null item) (list 'unmatched what))
          ((equal new (plist-get item :text)) nil)
          ((null new)
           (list 'refused (concat what ": emptied; mark it DELETE in the file to delete it")))
          (t (when write
               (org-canvas--submissions-rewrite-sent-comment
                (org-canvas--submissions-find-sent-comment id) new (plist-get item :delete)))
             (list :sent what)))))

(defun org-canvas--submissions-comments-import-sent (student write)
  "Return the outcomes of STUDENT's imported sent comments at point.
Only a comment carrying :text is compared; WRITE writes the changes."
  (let ((items (org-canvas--submissions-sent-comments)))
    (delq nil (mapcar (lambda (c)
                        (when (plist-member c :text)
                          (org-canvas--submissions-comments-import-one-sent c items write)))
                      (plist-get student :sent)))))

(defun org-canvas--submissions-comments-import-at-point (student write)
  "Return the outcomes of STUDENT at its heading, writing them when WRITE."
  (append (org-canvas--submissions-comments-import-rows student write)
          (org-canvas--submissions-comments-import-draft student write)
          (org-canvas--submissions-comments-import-sent student write)))

(defun org-canvas--submissions-comments-change-p (outcome)
  "Return non-nil when OUTCOME is a change, not a refusal or a miss."
  (keywordp (car outcome)))

(defun org-canvas--submissions-comments-plan (student allow-posted write)
  "Return the outcomes of STUDENT at its heading, written when WRITE.
The changes are worked out first without writing.  A student whose
grade is posted keeps every change back, each one `posted', unless
ALLOW-POSTED."
  (let ((plan (org-canvas--submissions-comments-import-at-point student nil)))
    (cond ((not (cl-some #'org-canvas--submissions-comments-change-p plan)) plan)
          ((and (org-canvas--submissions-comments-prop "POSTED_AT") (not allow-posted))
           (mapcar (lambda (o)
                     (if (org-canvas--submissions-comments-change-p o)
                         (list 'posted (concat (cadr o) ": grade already posted"))
                       o))
                   plan))
          (write (org-canvas--submissions-comments-import-at-point student t))
          (t plan))))

(defun org-canvas--submissions-comments-import-student (student markers allow-posted write)
  "Return the outcomes of STUDENT, each (KIND WHAT) with WHAT naming the user.
MARKERS are the file's (USER-ID . MARKER); ALLOW-POSTED and WRITE are
as `org-canvas--submissions-comments-plan' takes them."
  (let* ((user-id (format "%s" (plist-get student :user-id)))
         (marker (cdr (assoc user-id markers))))
    (if (null marker)
        (list (list 'unmatched (format "user %s" user-id)))
      (goto-char marker)
      (mapcar (lambda (o) (list (car o) (format "user %s %s" user-id (cadr o))))
              (org-canvas--submissions-comments-plan student allow-posted write)))))

(defun org-canvas--submissions-comments-tally (outcomes dry-run)
  "Return the import's result plist for OUTCOMES; DRY-RUN as it ran.
:rows, :drafts and :sent count the changes; :unmatched names the keys
that matched nothing; :skipped names the changes kept back, with why."
  (let ((of (lambda (kinds)
              (mapcar #'cadr (seq-filter (lambda (o) (memq (car o) kinds)) outcomes)))))
    (list :rows (cl-count :rows outcomes :key #'car)
          :drafts (cl-count :drafts outcomes :key #'car)
          :sent (cl-count :sent outcomes :key #'car)
          :unmatched (funcall of '(unmatched))
          :skipped (funcall of '(posted refused))
          :dry-run (and dry-run t))))

(defun org-canvas--submissions-comments-import (students allow-posted)
  "Write STUDENTS' comment text into this grading buffer; return the tally.
A posted student's changes are kept back unless ALLOW-POSTED.  Under
`org-canvas--dry-run' nothing is written and the tally is the same.
The file is saved when anything changed."
  (let* ((dry-run org-canvas--dry-run)
         (markers (org-canvas--submissions-comments-markers))
         (outcomes (save-excursion
                     (apply #'append
                            (mapcar (lambda (s)
                                      (org-canvas--submissions-comments-import-student
                                       s markers allow-posted (not dry-run)))
                                    students)))))
    (dolist (m markers) (set-marker (cdr m) nil))
    (when (and buffer-file-name (not dry-run))
      (org-canvas--save-buffer))
    (org-canvas--submissions-comments-tally outcomes dry-run)))

(defun org-canvas--submissions-comments-input (data)
  "Return the students DATA gives: a list as is, a file name read as JSON.
Nil asks for the file, or is a `user-error' under `noninteractive'."
  (cond ((consp data) data)
        ((stringp data) (org-canvas--submissions-comments-read-json data))
        (noninteractive (user-error "No comments to import; pass a JSON file"))
        (t (org-canvas--submissions-comments-read-json
            (read-file-name "Import comments from JSON: " nil nil t)))))

(defun org-canvas--submissions-comments-summary (result)
  "Return the one-line summary of RESULT, an import's tally."
  (format "%sComments: %d row comment(s), %d draft(s), %d sent comment(s) %s; %d key(s) matched nothing, %d kept back"
          (if (plist-get result :dry-run) "[DRY-RUN] " "")
          (plist-get result :rows) (plist-get result :drafts) (plist-get result :sent)
          (if (plist-get result :dry-run) "would change" "changed")
          (length (plist-get result :unmatched)) (length (plist-get result :skipped))))

(defun org-canvas--submissions-comments-report-import (result)
  "Name RESULT's unmatched and kept-back keys, then its summary."
  (dolist (what (plist-get result :unmatched))
    (org-canvas--user-message "Comments: %s matched nothing" what))
  (dolist (what (plist-get result :skipped))
    (org-canvas--user-message "Comments: kept back %s" what))
  (org-canvas--user-message "%s" (org-canvas--submissions-comments-summary result)))

;;;###autoload
(defun org-canvas-submissions-import-comments (&optional file data allow-posted)
  "Write new comment text into a grading file and touch nothing else.
FILE is taken as `org-canvas-submissions-export-comments' takes it.
DATA is a list of student plists shaped as that export returns them,
or the name of a JSON file shaped as it writes them; nil asks for the
file.  Each student is found by :user-id, and only the keys present
are read: a row's :comment by its :criterion-id, :draft, and a sent
comment's :text by its :id.  A text equal to the file's is no change,
so importing an unchanged export changes nothing.

A row comment and a draft are written as typed ones are, for the grade
push (S).  A sent comment's new text is written over its item and its
CANVAS_COMMENTS baseline left as it was, which is the state a hand
edit leaves, so the comment push (C) sends it, checked as any edit is.
An emptied sent comment is refused (mark it DELETE in the file to
delete it), and so is a draft with a line starting with #.  A student
whose grade is posted (POSTED_AT) keeps every change back unless
ALLOW-POSTED, since a student who read the comment would find it
rewritten without a word.

Under `org-canvas--dry-run' nothing is written.  The file is saved
when anything changed.  Return a plist: :rows, :drafts and :sent, the
changes (made, or under a dry run that would be made); :unmatched,
the keys that matched nothing (\"user 5001 criterion _812\");
:skipped, the changes kept back with why; :dry-run."
  ;; Bare `(interactive)': a sexp spec blanks undercover's line counts.
  (interactive)
  (with-current-buffer (org-canvas--submissions-comments-buffer file)
    (let ((result (org-canvas--submissions-comments-import
                   (org-canvas--submissions-comments-input data) allow-posted)))
      (org-canvas--submissions-comments-report-import result)
      result)))

;;;; Checking

(defun org-canvas--submissions-comments-number-p (text)
  "Return non-nil when TEXT is a plain non-negative number."
  (and text (string-match-p "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\'" text)))

(defun org-canvas--submissions-comments-deducted-p (row)
  "Return non-nil when ROW, an exported row, is scored below its max."
  (let ((score (plist-get row :score)) (max (plist-get row :max)))
    (and (org-canvas--submissions-comments-number-p score)
         (org-canvas--submissions-comments-number-p max)
         (< (string-to-number score) (string-to-number max)))))

(defun org-canvas--submissions-comments-finding (student &rest props)
  "Return a finding on STUDENT, an exported plist, with PROPS added."
  (append props (list :user-id (plist-get student :user-id)
                      :name (plist-get student :name))))

(defun org-canvas--submissions-comments-missing (student)
  "Return an error for each of STUDENT's deducted rows without a comment."
  (mapcar (lambda (row)
            (org-canvas--submissions-comments-finding
             student :severity 'error :kind 'missing
             :where (or (plist-get row :criterion) (plist-get row :criterion-id))
             :text (format "%s of %s" (plist-get row :score) (plist-get row :max))))
          (seq-filter (lambda (row) (and (org-canvas--submissions-comments-deducted-p row)
                                         (null (plist-get row :comment))))
                      (plist-get student :rows))))

(defun org-canvas--submissions-comments-texts (student)
  "Return (WHERE . TEXT) for each of STUDENT's comment texts."
  (seq-filter
   #'cdr
   (append (mapcar (lambda (row)
                     (cons (or (plist-get row :criterion) (plist-get row :criterion-id))
                           (plist-get row :comment)))
                   (plist-get student :rows))
           (list (cons "draft" (plist-get student :draft)))
           (mapcar (lambda (c) (cons (format "comment %s" (plist-get c :id))
                                     (plist-get c :text)))
                   (plist-get student :sent)))))

(defun org-canvas--submissions-comments-flagged (student)
  "Return an error for each of STUDENT's texts a flag regexp matches."
  (let ((found nil))
    (dolist (text (org-canvas--submissions-comments-texts student))
      (dolist (re org-canvas-submissions-comment-flag-regexps)
        (when (string-match-p re (cdr text))
          (push (org-canvas--submissions-comments-finding
                 student :severity 'error :kind 'flagged
                 :where (car text) :regexp re :text (cdr text))
                found))))
    (nreverse found)))

(defun org-canvas--submissions-comments-sentences (text)
  "Return TEXT's sentences long enough for the shared count, trimmed."
  (seq-filter
   (lambda (s) (>= (length s) org-canvas-submissions-comment-sentence-min-length))
   (mapcar #'string-trim
           (split-string
            (replace-regexp-in-string
             "\\([.?!][\"')]*\\) " "\\1\n"
             (replace-regexp-in-string "[ \t\n]+" " " text))
            "\n" t))))

(defun org-canvas--submissions-comments-sentence-users (students)
  "Return a hash from each sentence of STUDENTS' texts to their user ids."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (student students)
      (dolist (text (org-canvas--submissions-comments-texts student))
        (dolist (sentence (org-canvas--submissions-comments-sentences (cdr text)))
          (cl-pushnew (plist-get student :user-id) (gethash sentence seen)
                      :test #'equal))))
    seen))

(defun org-canvas--submissions-comments-shared (students)
  "Return a warning for each sentence more STUDENTS share than the threshold.
Most shared first; none when `org-canvas-submissions-comment-shared-threshold'
is nil."
  (when org-canvas-submissions-comment-shared-threshold
    (let ((found nil))
      (maphash (lambda (sentence users)
                 (when (> (length users) org-canvas-submissions-comment-shared-threshold)
                   (push (list :severity 'warning :kind 'shared :count (length users)
                               :user-ids (reverse users) :text sentence)
                         found)))
               (org-canvas--submissions-comments-sentence-users students))
      (sort found (lambda (a b)
                    (or (> (plist-get a :count) (plist-get b :count))
                        (and (= (plist-get a :count) (plist-get b :count))
                             (string< (plist-get a :text) (plist-get b :text)))))))))

(defun org-canvas--submissions-comments-checked-p (student)
  "Return non-nil when STUDENT is checked: not posted, and still enrolled."
  (not (or (plist-get student :posted)
           (equal (plist-get student :status) "left"))))

(defun org-canvas--submissions-comments-findings (students)
  "Return the findings on STUDENTS: errors per student, then shared warnings."
  (append (apply #'append
                 (mapcar (lambda (s)
                           (append (org-canvas--submissions-comments-missing s)
                                   (org-canvas--submissions-comments-flagged s)))
                         students))
          (org-canvas--submissions-comments-shared students)))

(defun org-canvas-submissions-comment-check-errors (findings)
  "Return how many of FINDINGS are errors, which fail the batch check."
  (cl-count 'error findings :key (lambda (f) (plist-get f :severity))))

(defun org-canvas--submissions-comments-one-line (text)
  "Return TEXT on one line, its line breaks as spaces."
  (replace-regexp-in-string "[ \t]*\n[ \t\n]*" " " text))

(defun org-canvas--submissions-comments-finding-line (finding)
  "Return the report's item for FINDING."
  (let ((who (format "%s (%s)" (plist-get finding :name) (plist-get finding :user-id))))
    (pcase (plist-get finding :kind)
      ('missing (format "- %s :: %s: scored %s, no comment"
                        who (plist-get finding :where) (plist-get finding :text)))
      ('flagged (format "- %s, %s :: matches =%s=: %s"
                        who (plist-get finding :where) (plist-get finding :regexp)
                        (org-canvas--submissions-comments-one-line
                         (plist-get finding :text))))
      (_ (format "- %d students :: %s (%s)"
                 (plist-get finding :count) (plist-get finding :text)
                 (string-join (plist-get finding :user-ids) ", "))))))

(defun org-canvas--submissions-comments-check-summary (findings checked skipped)
  "Return the check's summary of FINDINGS over CHECKED and SKIPPED students."
  (let ((errors (org-canvas-submissions-comment-check-errors findings)))
    (format "%d error(s), %d warning(s); %d student(s) checked, %d skipped (posted or left)"
            errors (- (length findings) errors) checked skipped)))

(defun org-canvas--submissions-comments-render (name findings summary)
  "Insert the check's report on NAME's FINDINGS, under SUMMARY, here."
  (insert (format "#+TITLE: Comment check: %s\n\n%s\n" name summary))
  (dolist (group `(("Deducted rows without a comment" missing)
                   ("Flagged text" flagged)
                   (,(format "Sentences given to more than %s students"
                             org-canvas-submissions-comment-shared-threshold)
                    shared)))
    (when-let* ((these (seq-filter (lambda (f) (eq (plist-get f :kind) (nth 1 group)))
                                   findings)))
      (insert "\n* " (nth 0 group) "\n\n")
      (dolist (f these)
        (insert (org-canvas--submissions-comments-finding-line f) "\n")))))

;;;###autoload
(defun org-canvas-submissions-check-comments (&optional file)
  "Check a grading file's comments before a push; return the findings.
FILE is taken as `org-canvas-submissions-export-comments' takes it.
Students whose grade is posted, and students who left, are not
checked.  The findings are errors and warnings, each a plist with
:severity, :kind, :name, :user-id, :where and :text:

  - error `missing': a Rubric row scored below its max with no comment;
  - error `flagged': a row comment, draft or sent comment matching one
    of `org-canvas-submissions-comment-flag-regexps' (:regexp);
  - warning `shared': a sentence given word for word to more students
    than `org-canvas-submissions-comment-shared-threshold' (:count and
    :user-ids in place of a student).

The report is shown, or printed under `noninteractive'.  Errors are
what the batch check exits non-zero on
\(`org-canvas-submissions-comment-check-errors'); a shared sentence is
a warning, since a stock sentence can be meant.  Nothing is changed."
  ;; Bare `(interactive)': a sexp spec blanks undercover's line counts.
  (interactive)
  (with-current-buffer (org-canvas--submissions-comments-buffer file)
    (let* ((all (org-canvas--submissions-comments-collect))
           (checked (seq-filter #'org-canvas--submissions-comments-checked-p all))
           (findings (org-canvas--submissions-comments-findings checked))
           (summary (org-canvas--submissions-comments-check-summary
                     findings (length checked) (- (length all) (length checked)))))
      (org-canvas--report-display
       org-canvas--submissions-comments-check-buffer
       (let ((name org-canvas-submissions--assignment-name))
         (lambda ()
           (org-canvas--submissions-comments-render name findings summary)))
       #'org-mode)
      (message "Comment check: %s" summary)
      findings)))

(provide 'org-canvas-submissions-comments)
;;; org-canvas-submissions-comments.el ends here
