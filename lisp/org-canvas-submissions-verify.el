;;; org-canvas-submissions-verify.el --- Read a pushed grading file back from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A push answers with what Canvas said to each request, which is not
;; the same as what Canvas now holds.  Reporting a hidden push of six
;; columns as done took a read-back written afresh each time: GET every
;; column's submissions with their rubric assessments and comments,
;; then check in a script that nothing was posted, that no student was
;; given the same comment twice, and that every changed score, Rubric
;; row and comment is what the file says (issue #450).
;;
;; `org-canvas-submissions-verify' is that read-back.  It reads the
;; column once, through the all-pages helper the pull uses, compares
;; it with the grading file, and counts per column:
;;
;;   - posted: submissions Canvas shows students (a difference only
;;     when the push was meant to stay hidden);
;;   - duplicates: a comment given to a student more than once by the
;;     same author, other than the student;
;;   - scores: a SCORE Canvas does not hold;
;;   - rubric-rows: a Rubric row whose score or comment Canvas does not
;;     hold;
;;   - comments: a student whose Comments section lists a different
;;     number of comments than Canvas holds;
;;   - comment-text: a sent comment, by id, gone from Canvas or holding
;;     other text;
;;   - undeleted: an item marked `- DELETE' whose comment Canvas still
;;     holds;
;;   - drafts: a drafted comment never posted.
;;
;; Nothing is written, locally or on Canvas.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules: it reads the grading files
;; the submissions module writes, through the readers the comment
;; export uses.
(require 'org-canvas-submissions)
(require 'org-canvas-submissions-comments)

(defconst org-canvas--submissions-verify-buffer "*canvas-verify*"
  "Buffer the verification report is rendered into.")

(defconst org-canvas--submissions-verify-checks
  '((posted . "posted")
    (duplicates . "duplicate comments")
    (scores . "mismatched scores")
    (rubric-rows . "mismatched rubric rows")
    (comments . "comment counts")
    (comment-text . "comment text")
    (undeleted . "undeleted DELETE items")
    (drafts . "unsent drafts"))
  "The checks, in report order, as (KIND . LABEL).
Each KIND is also the result plist's key, as a keyword.")

;;;; Reading the File

(defun org-canvas--submissions-verify-item-count ()
  "Return how many Comments items of the entry at point are not marked DELETE.
An item is a line starting with a dash; a comment's later lines are
indented under it, so they are not counted."
  (if-let* ((region (org-canvas--submissions-section-region
                     org-canvas--submissions-comments-heading)))
      (save-excursion
        (goto-char (car region))
        (let ((count 0))
          (while (re-search-forward "^-[ \t]+" (cdr region) t)
            (unless (looking-at-p "DELETE[ \t]")
              (cl-incf count)))
          count))
    0))

(defun org-canvas--submissions-verify-student-at-point (user-id)
  "Return the file's record of USER-ID's heading at point, as a plist.
The keys are those of `org-canvas--submissions-comments-at-point',
with :canvas-score, the CANVAS_SCORE baseline, and :items, the
Comments items not marked DELETE."
  (append (org-canvas--submissions-comments-at-point user-id)
          (list :canvas-score (org-canvas--submissions-comments-prop "CANVAS_SCORE")
                :items (org-canvas--submissions-verify-item-count))))

(defun org-canvas--submissions-verify-students ()
  "Return every student heading of this grading file, in file order."
  (save-excursion
    (mapcar (lambda (entry)
              (goto-char (cdr entry))
              (prog1 (org-canvas--submissions-verify-student-at-point (car entry))
                (set-marker (cdr entry) nil)))
            (org-canvas--submissions-comments-markers))))

;;;; Checks

(defun org-canvas--submissions-verify-finding (kind student &rest props)
  "Return a finding of KIND on STUDENT, a plist, with PROPS added."
  (append (list :kind kind :user-id (plist-get student :user-id)
                :name (plist-get student :name))
          props))

(defun org-canvas--submissions-verify-number (text)
  "Return TEXT, a score as typed or pulled, as a number, \"EX\" or itself.
Nil stays nil."
  (let ((parsed (org-canvas--submissions-parse-score text)))
    (cond ((null text) nil)
          ((equal parsed "EX") "EX")
          (parsed (string-to-number parsed))
          (t text))))

(defun org-canvas--submissions-verify-file-score (student)
  "Return the score STUDENT's heading asks Canvas to hold.
A number, \"EX\", nil for no grade, or the SCORE text when it reads
as none of these.  An absent SCORE leaves the grade alone, so the
CANVAS_SCORE baseline is what Canvas should hold; a clear word asks
for no grade."
  (let ((text (plist-get student :score)))
    (cond ((null text)
           (org-canvas--submissions-verify-number (plist-get student :canvas-score)))
          ((org-canvas--submissions-clear-score-p text) nil)
          (t (org-canvas--submissions-verify-number text)))))

(defun org-canvas--submissions-verify-canvas-score (submission)
  "Return the score of SUBMISSION: a number, \"EX\" or nil."
  (if (org-canvas--submissions-excused-p submission)
      "EX"
    (org-canvas--submissions-entered-score submission)))

(defun org-canvas--submissions-verify-same-score-p (a b)
  "Return non-nil when scores A and B agree."
  (or (equal a b)
      (and (numberp a) (numberp b) (< (abs (- a b)) 1e-6))))

(defun org-canvas--submissions-verify-scores (student submission)
  "Return the score finding on STUDENT against SUBMISSION, in a list, or nil."
  (let ((file (org-canvas--submissions-verify-file-score student))
        (canvas (org-canvas--submissions-verify-canvas-score submission)))
    (unless (org-canvas--submissions-verify-same-score-p file canvas)
      (list (org-canvas--submissions-verify-finding
             'scores student :file file :canvas canvas)))))

(defun org-canvas--submissions-verify-rubric-rows (student submission)
  "Return a finding for each of STUDENT's Rubric rows SUBMISSION disagrees with.
A row's score and comment are compared with the assessment Canvas
holds for its criterion; a row the file leaves empty agrees with a
criterion Canvas never assessed."
  (let ((assessment (org-canvas--submissions-assessment submission))
        (found nil))
    (dolist (row (plist-get student :rows))
      (let* ((id (plist-get row :criterion-id))
             (file (cons (org-canvas--submissions-rubric-cell-score (plist-get row :score))
                         (plist-get row :comment)))
             (canvas (or (org-canvas--submissions-assessment-entry assessment id)
                         (cons nil nil))))
        (unless (equal file canvas)
          (push (org-canvas--submissions-verify-finding
                 'rubric-rows student :criterion-id id :file file :canvas canvas)
                found))))
    (nreverse found)))

(defun org-canvas--submissions-verify-canvas-comments (submission)
  "Return SUBMISSION's comments as a list."
  (append (org-canvas--alist-get-non-null 'submission_comments submission) nil))

(defun org-canvas--submissions-verify-duplicates (student submission)
  "Return a finding for each repeated comment on SUBMISSION, of STUDENT.
A comment repeats when its author, other than the student, gave the
same text before; each repeat is one finding."
  (let ((seen (make-hash-table :test 'equal))
        (self (org-canvas--submissions-user-id submission))
        (found nil))
    (dolist (c (org-canvas--submissions-verify-canvas-comments submission))
      (let ((author (alist-get 'author_id c))
            (text (org-canvas--submissions-comment-text (alist-get 'comment c))))
        (unless (or (null text) (equal author self))
          (if (gethash (cons author text) seen)
              (push (org-canvas--submissions-verify-finding
                     'duplicates student :id (format "%s" (alist-get 'id c)) :text text)
                    found)
            (puthash (cons author text) t seen)))))
    (nreverse found)))

(defun org-canvas--submissions-verify-comment-index (submission)
  "Return SUBMISSION's comments in a hash table keyed by id as a string."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (c (org-canvas--submissions-verify-canvas-comments submission))
      (when-let* ((id (org-canvas--alist-get-non-null 'id c)))
        (puthash (format "%s" id) c index)))
    index))

(defun org-canvas--submissions-verify-sent-item (student item index)
  "Return the finding on STUDENT's sent comment ITEM, or nil.
INDEX holds Canvas's comments by id.  An item marked DELETE whose
comment Canvas still holds is `undeleted'; one not marked whose
comment is gone, or digests otherwise, is `comment-text'."
  (let* ((id (plist-get item :id))
         (live (gethash id index)))
    (cond ((plist-get item :delete)
           (and live (org-canvas--submissions-verify-finding
                      'undeleted student :id id)))
          ((null live)
           (org-canvas--submissions-verify-finding
            'comment-text student :id id :canvas nil))
          ((not (equal (org-canvas--submissions-comment-digest (plist-get item :text))
                       (org-canvas--submissions-comment-digest
                        (org-canvas--submissions-comment-org-text live))))
           (org-canvas--submissions-verify-finding
            'comment-text student :id id :canvas 'differs)))))

(defun org-canvas--submissions-verify-comments (student submission)
  "Return the findings on STUDENT's comments against SUBMISSION.
Each sent item is checked by its id; then the items not marked DELETE
are counted against the comments Canvas holds that no DELETE item
names, so a comment posted twice, or one the file never shows, tells."
  (let* ((index (org-canvas--submissions-verify-comment-index submission))
         (sent (plist-get student :sent))
         (marked (delq nil (mapcar (lambda (i) (and (plist-get i :delete) (plist-get i :id)))
                                   sent)))
         (canvas (cl-count-if-not
                  (lambda (c) (member (format "%s" (alist-get 'id c)) marked))
                  (org-canvas--submissions-verify-canvas-comments submission)))
         (items (delq nil (mapcar (lambda (i)
                                    (org-canvas--submissions-verify-sent-item student i index))
                                  sent))))
    (append items
            (unless (= canvas (plist-get student :items))
              (list (org-canvas--submissions-verify-finding
                     'comments student :file (plist-get student :items) :canvas canvas))))))

(defun org-canvas--submissions-verify-draft (student)
  "Return the finding on STUDENT's unsent draft, in a list, or nil.
A student who left is passed over, since a push never sends their
draft."
  (when (and (plist-get student :draft)
             (not (equal (plist-get student :status) "left")))
    (list (org-canvas--submissions-verify-finding 'drafts student))))

(defun org-canvas--submissions-verify-student (student submission)
  "Return every finding on STUDENT against SUBMISSION, Canvas's."
  (append (org-canvas--submissions-verify-scores student submission)
          (org-canvas--submissions-verify-rubric-rows student submission)
          (org-canvas--submissions-verify-duplicates student submission)
          (org-canvas--submissions-verify-comments student submission)))

(defun org-canvas--submissions-verify-posted (submissions students)
  "Return a finding for each of SUBMISSIONS posted to students.
STUDENTS, the file's, name them; a submission with no heading is
named by its user id."
  (let ((found nil))
    (dolist (sub submissions)
      (when (stringp (org-canvas--alist-get-non-null 'posted_at sub))
        (let* ((uid (format "%s" (org-canvas--submissions-user-id sub)))
               (student (or (seq-find (lambda (s) (equal (plist-get s :user-id) uid))
                                      students)
                            (list :user-id uid :name (format "User %s" uid)))))
          (push (org-canvas--submissions-verify-finding
                 'posted student :at (alist-get 'posted_at sub))
                found))))
    (nreverse found)))

;;;; One Column

(defun org-canvas--submissions-verify-index (submissions)
  "Return SUBMISSIONS in a hash table keyed by user id as a string."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (sub submissions)
      (when-let* ((uid (org-canvas--submissions-user-id sub)))
        (puthash (format "%s" uid) sub index)))
    index))

(defun org-canvas--submissions-verify-tally (findings hidden)
  "Return the counts of FINDINGS as a plist, with :differences their sum.
Posted submissions count toward :differences only when HIDDEN."
  (let ((plist nil) (total 0))
    (dolist (check org-canvas--submissions-verify-checks)
      (let ((n (cl-count (car check) findings :key (lambda (f) (plist-get f :kind)))))
        (setq plist (plist-put plist (intern (format ":%s" (car check))) n))
        (unless (and (eq (car check) 'posted) (not hidden))
          (cl-incf total n))))
    (plist-put plist :differences total)))

(defun org-canvas--submissions-verify-current (hidden)
  "Compare the grading buffer at hand with Canvas; return the column's plist.
HIDDEN non-nil counts a posted submission as a difference.  See
`org-canvas-submissions-verify' for the keys."
  (let* ((id org-canvas-submissions--assignment-id)
         (students (org-canvas--submissions-verify-students))
         (submissions (append (org-canvas--submissions-fetch-for-assignment id) nil))
         (index (org-canvas--submissions-verify-index submissions))
         (unmatched 0)
         (findings (org-canvas--submissions-verify-posted submissions students)))
    (dolist (student students)
      (if-let* ((sub (gethash (plist-get student :user-id) index)))
          (setq findings (append findings
                                 (org-canvas--submissions-verify-student student sub)))
        (cl-incf unmatched))
      (setq findings (append findings (org-canvas--submissions-verify-draft student))))
    (append (list :assignment-id id :name org-canvas-submissions--assignment-name
                  :file buffer-file-name :students (length students) :unmatched unmatched
                  :hidden (and hidden t))
            (org-canvas--submissions-verify-tally findings hidden)
            (list :findings findings))))

;;;; Report

(defun org-canvas--submissions-verify-summary (result)
  "Return the one-line summary of RESULT, a column's plist."
  (format "%s: %d difference(s); %s; %d student(s)%s"
          (plist-get result :name)
          (plist-get result :differences)
          (mapconcat (lambda (check)
                       (format "%s %d" (cdr check)
                               (plist-get result (intern (format ":%s" (car check))))))
                     org-canvas--submissions-verify-checks ", ")
          (plist-get result :students)
          (if (> (plist-get result :unmatched) 0)
              (format ", %d with no submission on Canvas" (plist-get result :unmatched))
            "")))

(defun org-canvas--submissions-verify-value (value)
  "Return VALUE, a score or a rubric (SCORE . COMMENT), as report text."
  (cond ((null value) "none")
        ((consp value)
         (format "%s%s" (or (car value) "-")
                 (if (cdr value)
                     (format " %S" (org-canvas--submissions-comments-one-line (cdr value)))
                   "")))
        (t (format "%s" value))))

(defun org-canvas--submissions-verify-detail (finding)
  "Return the text of FINDING, after its student's name."
  (let ((file (org-canvas--submissions-verify-value (plist-get finding :file)))
        (canvas (org-canvas--submissions-verify-value (plist-get finding :canvas))))
    (pcase (plist-get finding :kind)
      ('posted (format "posted %s" (plist-get finding :at)))
      ('duplicates (format "comment %s repeats %S" (plist-get finding :id)
                           (org-canvas--submissions-comments-one-line
                            (plist-get finding :text))))
      ('scores (format "score: file %s, Canvas %s" file canvas))
      ('rubric-rows (format "rubric row %s: file %s, Canvas %s"
                            (plist-get finding :criterion-id) file canvas))
      ('comments (format "comments: file lists %s, Canvas holds %s" file canvas))
      ('comment-text (format "comment %s: %s" (plist-get finding :id)
                             (if (plist-get finding :canvas)
                                 "Canvas holds other text"
                               "gone from Canvas")))
      ('undeleted (format "comment %s marked DELETE is still on Canvas"
                          (plist-get finding :id)))
      (_ "drafted comment never posted"))))

(defun org-canvas--submissions-verify-render (results)
  "Insert the verification report on RESULTS, one section per column."
  (insert "#+TITLE: Push verification\n")
  (dolist (result results)
    (insert (format "\n* %s (%s)\n\n%s\n"
                    (plist-get result :name) (plist-get result :assignment-id)
                    (org-canvas--submissions-verify-summary result)))
    (when-let* ((findings (plist-get result :findings)))
      (insert "\n")
      (dolist (f findings)
        (insert (format "- %s (%s) :: %s\n" (plist-get f :name) (plist-get f :user-id)
                        (org-canvas--submissions-verify-detail f)))))))

;;;###autoload
(defun org-canvas-submissions-verify (&optional assignment hidden)
  "Read a pushed grading file back from Canvas; return what differs.
ASSIGNMENT is the grading file, taken as
`org-canvas-push-submission-grades' takes it: an assignment id, a
path, or the file's or the assignment's name; nil is the grading
buffer at hand, or asks for the file.  A list of them verifies each
column and returns a list of plists, one per column.

The column's submissions are read once, with their rubric assessments
and comments, and nothing is written.  The plist carries
:assignment-id, :name, :file, :students (the file's headings) and
:unmatched (headings with no submission on Canvas), then one integer
per check:

  :posted        submissions Canvas shows students;
  :duplicates    a comment given again by the same author;
  :scores        a SCORE (or, without one, CANVAS_SCORE) Canvas does
                 not hold;
  :rubric-rows   a Rubric row whose score or comment differs;
  :comments      a student whose Comments items, less those marked
                 DELETE, are not as many as Canvas's comments;
  :comment-text  a sent comment gone from Canvas or holding other text;
  :undeleted     an item marked DELETE still on Canvas;
  :drafts        a drafted comment never posted;

:differences, their sum, and :findings, one plist per difference
\(:kind, :user-id, :name and what was compared).  Posted submissions
count toward :differences only when HIDDEN is non-nil, as for a push
meant to stay hidden; interactively, a prefix argument sets it.  So
zero :differences is a push that landed as the file says.

The report is shown, or printed under `noninteractive', and a line
per column is echoed."
  ;; A string spec: a sexp `interactive' blanks undercover's line counts.
  (interactive "i\nP")
  (let* ((targets (if (consp assignment) assignment (list assignment)))
         (results (mapcar (lambda (target)
                            (with-current-buffer (org-canvas--submissions-comments-buffer target)
                              (org-canvas--submissions-verify-current hidden)))
                          targets)))
    (org-canvas--report-display
     org-canvas--submissions-verify-buffer
     (lambda () (org-canvas--submissions-verify-render results))
     #'org-mode)
    (dolist (result results)
      (message "Verify %s" (org-canvas--submissions-verify-summary result)))
    (if (consp assignment) results (car results))))

(provide 'org-canvas-submissions-verify)
;;; org-canvas-submissions-verify.el ends here
