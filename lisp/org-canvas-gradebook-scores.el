;;; org-canvas-gradebook-scores.el --- Recompute a course's group scores  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The Group Scores half of `org-canvas-gradebook'.  Canvas's enrollments
;; carry a course total and nothing per group, so the Groups table and the
;; what-if recompute each group the way Canvas's grade calculator does: the
;; counted scores, the drop rules, the weights and the letter scheme, kept
;; in `org-canvas-gradebook-scores-file' for the next read.  The pull that
;; calls it stays in `org-canvas-gradebook.el'; the what-if that reads the
;; file is in `org-canvas-gradebook-report.el'.
;;
;; Except the one write and the one read of the scores file, everything
;; here is pure.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-gradebook)
(require 'cl-lib)

;;;; Group Scores: Computation
;;
;; Canvas's enrollments carry a course total and nothing per group, so
;; the Groups table and the what-if recompute each group the way
;; Canvas's grade calculator does, from every counted score:
;;
;;   - a score counts when its assignment is published and not omitted
;;     from the final grade, it is graded, and it is not excused; with
;;     `org-canvas-gradebook-unposted' nil, only when it is posted too
;;   - a group's drop rules drop the scores whose removal maximises the
;;     group's percentage (drop_lowest), then those whose removal
;;     minimises it (drop_highest), never one of never_drop, and always
;;     keep at least one droppable score.  The lowest score is not
;;     always the one to drop: 5/20 costs more than 0/1
;;   - weighted, the total is each group's percentage times its weight
;;     over the groups with points possible, scaled up when their
;;     weights sum under 100; unweighted, all points over all possible
;;
;; A score is an item, (ASSIGNMENT-ID SCORE POINTS); a group is a
;; plist, :id :name :weight :drop-lowest :drop-highest :never-drop.
;; Everything in this section is pure.

(defconst org-canvas--gradebook-default-scheme
  '(("A" . 94.0) ("A-" . 90.0) ("B+" . 87.0) ("B" . 84.0) ("B-" . 80.0)
    ("C+" . 77.0) ("C" . 74.0) ("C-" . 70.0) ("D+" . 67.0) ("D" . 64.0)
    ("D-" . 61.0) ("F" . 0.0))
  "Canvas's default grading scheme, as (LETTER . CUTOFF-PERCENT).")

(defun org-canvas--gradebook-sum (items index)
  "Return the sum of the INDEXth element of every item in ITEMS."
  (apply #'+ 0 (mapcar (lambda (item) (nth index item)) items)))

(defun org-canvas--gradebook-ratio (items)
  "Return ITEMS' summed score over their summed points, or nil without points."
  (let ((points (org-canvas--gradebook-sum items 2)))
    (and (> points 0) (/ (float (org-canvas--gradebook-sum items 1)) points))))

(defun org-canvas--gradebook-rank (items q keep lowest)
  "Return the KEEP items of ITEMS with the largest score less Q times points.
With LOWEST, the KEEP with the smallest."
  (let ((rated (mapcar (lambda (item) (cons (- (nth 1 item) (* q (nth 2 item))) item))
                       items)))
    (mapcar #'cdr (seq-take (sort rated (lambda (a b) (if lowest (< (car a) (car b))
                                                        (> (car a) (car b)))))
                            keep))))

(defun org-canvas--gradebook-keep (items keep fixed lowest)
  "Return the KEEP of ITEMS whose ratio, with the FIXED items, is highest.
With LOWEST, the KEEP whose ratio is lowest.  KEEP is at least one.
Dinkelbach's iteration: rank by score less Q times points, where Q is
the ratio of the set kept so far, until Q stops improving.  Each round
improves Q strictly, so it ends, and where it ends no set does better:
the exact optimum Canvas's bisection approximates."
  (let ((keep (max 1 keep)))
    (if (<= (length items) keep)
        items
      (let* ((kept (seq-take items keep))
             (q (org-canvas--gradebook-ratio (append kept fixed)))
             (done nil))
        (while (not done)
          (let* ((next (org-canvas--gradebook-rank items (or q 0.0) keep lowest))
                 (ratio (org-canvas--gradebook-ratio (append next fixed))))
            (if (and ratio (or (null q)
                               (if lowest (< ratio (- q 1e-12)) (> ratio (+ q 1e-12)))))
                (setq kept next q ratio)
              (setq done t))))
        kept))))

(defun org-canvas--gradebook-drop-from (items fixed lowest highest)
  "Return the droppable ITEMS that survive dropping LOWEST and HIGHEST.
FIXED are the never-drop items, which count toward the ratio but are
not returned.  At least one item is kept, and drop_highest gives way
when the two rules together would drop everything, as in Canvas.
Without points anywhere the rules drop by raw score."
  (let* ((n (length items))
         (lowest (min lowest (1- n)))
         (highest (if (>= (+ lowest highest) n) 0 highest))
         (keep-high (- n lowest))
         (keep-low (- keep-high highest)))
    (if (cl-some (lambda (item) (> (nth 2 item) 0)) items)
        (org-canvas--gradebook-keep
         (org-canvas--gradebook-keep items keep-high fixed nil) keep-low fixed t)
      (let ((sorted (sort (copy-sequence items) (lambda (a b) (< (nth 1 a) (nth 1 b))))))
        (seq-take (last sorted keep-high) keep-low)))))

(defun org-canvas--gradebook-drop (items group)
  "Return ITEMS less those GROUP's drop rules drop, as Canvas drops them."
  (let ((lowest (or (plist-get group :drop-lowest) 0))
        (highest (or (plist-get group :drop-highest) 0))
        (never (plist-get group :never-drop)))
    (if (and (<= lowest 0) (<= highest 0))
        items
      (let ((fixed (cl-remove-if-not (lambda (item) (member (car item) never)) items))
            (droppable (cl-remove-if (lambda (item) (member (car item) never)) items)))
        (if (null droppable)
            items
          (append (org-canvas--gradebook-drop-from droppable fixed lowest highest)
                  fixed))))))

(defun org-canvas--gradebook-group-sums (items group)
  "Return (SCORE . POSSIBLE) for ITEMS in GROUP after its drop rules.
Nil when no item counts."
  (let ((kept (org-canvas--gradebook-drop items group)))
    (and kept (cons (org-canvas--gradebook-sum kept 1)
                    (org-canvas--gradebook-sum kept 2)))))

(defun org-canvas--gradebook-percent (sums)
  "Return SUMS, a (SCORE . POSSIBLE), as a percentage, or nil."
  (and sums (> (cdr sums) 0) (* 100.0 (/ (float (car sums)) (cdr sums)))))

(defun org-canvas--gradebook-student-groups (items groups assignments)
  "Return a (GROUP . SUMS) per group of GROUPS for one student's ITEMS.
ASSIGNMENTS maps an assignment id to (GROUP-ID . POINTS); SUMS is
the group's (SCORE . POSSIBLE), nil when nothing in it counts."
  (mapcar (lambda (group)
            (cons group
                  (org-canvas--gradebook-group-sums
                   (cl-remove-if-not
                    (lambda (item) (eql (car (alist-get (car item) assignments))
                                        (plist-get group :id)))
                    items)
                   group)))
          groups))

(defun org-canvas--gradebook-weighted-total (per-group)
  "Return the weighted total of PER-GROUP, (GROUP . SUMS) pairs, or nil.
Only the groups with points possible count; when their weights sum
under 100 the total is scaled up to them, as Canvas does."
  (let* ((relevant (cl-remove-if-not (lambda (pg) (org-canvas--gradebook-percent (cdr pg)))
                                     per-group))
         (full (apply #'+ 0 (mapcar (lambda (pg) (or (plist-get (car pg) :weight) 0)) relevant))))
    (when (> full 0)
      (let ((grade (apply #'+ (mapcar (lambda (pg)
                                        (* (or (plist-get (car pg) :weight) 0)
                                           (/ (org-canvas--gradebook-percent (cdr pg)) 100.0)))
                                      relevant))))
        (if (< full 100) (/ (* grade 100.0) full) grade)))))

(defun org-canvas--gradebook-total (per-group weighted)
  "Return the course total from PER-GROUP, (GROUP . SUMS) pairs, or nil.
WEIGHTED applies the group weights; otherwise every kept point
counts over every kept point possible."
  (if weighted
      (org-canvas--gradebook-weighted-total per-group)
    (let ((sums (delq nil (mapcar #'cdr per-group))))
      (org-canvas--gradebook-percent
       (cons (apply #'+ 0 (mapcar #'car sums)) (apply #'+ 0 (mapcar #'cdr sums)))))))

(defun org-canvas--gradebook-letter (score scheme)
  "Return the letter SCHEME gives SCORE, or nil without either.
SCHEME is (LETTER . CUTOFF-PERCENT) pairs in any order; SCORE is
rounded to two decimals first, as Canvas rounds it."
  (when (and (numberp score) scheme)
    (let ((rounded (/ (fround (* score 100.0)) 100.0))
          (sorted (sort (copy-sequence scheme) (lambda (a b) (> (cdr a) (cdr b))))))
      (car (or (cl-find-if (lambda (entry) (>= rounded (cdr entry))) sorted)
               (car (last sorted)))))))

(defun org-canvas--gradebook-median (values)
  "Return the median of the numbers in VALUES, or nil when there are none."
  (let* ((numbers (sort (cl-remove-if-not #'numberp values) #'<))
         (n (length numbers)))
    (cond ((= n 0) nil)
          ((cl-oddp n) (float (nth (/ n 2) numbers)))
          (t (/ (+ (nth (1- (/ n 2)) numbers) (nth (/ n 2) numbers)) 2.0)))))

(defun org-canvas--gradebook-letter-counts (scores scheme)
  "Return (LETTER . COUNT) for the SCORES under SCHEME, best letter first.
Letters nobody gets are left out; so is a score that is nil."
  (let ((letters (mapcar (lambda (s) (org-canvas--gradebook-letter s scheme)) scores))
        (counts nil))
    (dolist (entry (sort (copy-sequence scheme) (lambda (a b) (> (cdr a) (cdr b)))))
      (let ((n (cl-count (car entry) letters :test #'equal)))
        (when (> n 0) (push (cons (car entry) n) counts))))
    (nreverse counts)))

;;;; Group Scores: From Canvas

(defun org-canvas--gradebook-rule-number (rules key)
  "Return the number under KEY in the Canvas RULES object, or nil."
  (let ((value (and (listp rules) (alist-get key rules))))
    (and (numberp value) value)))

(defun org-canvas--gradebook-canvas-group (group)
  "Return the Canvas assignment GROUP as a group plist."
  (let* ((rules (alist-get 'rules group))
         (never (and (listp rules) (alist-get 'never_drop rules))))
    (list :id (alist-get 'id group)
          :name (alist-get 'name group)
          :weight (let ((w (alist-get 'group_weight group))) (if (numberp w) w 0))
          :drop-lowest (org-canvas--gradebook-rule-number rules 'drop_lowest)
          :drop-highest (org-canvas--gradebook-rule-number rules 'drop_highest)
          :never-drop (and (sequencep never) (append never nil)))))

(defun org-canvas--gradebook-canvas-groups (raw-groups)
  "Return RAW-GROUPS, Canvas's assignment groups, as plists in position order."
  (mapcar #'org-canvas--gradebook-canvas-group
          (sort (copy-sequence raw-groups)
                (lambda (a b) (< (or (alist-get 'position a) 0)
                                 (or (alist-get 'position b) 0))))))

(defun org-canvas--gradebook-assignment-map (raw-groups)
  "Return an alist of assignment id to (GROUP-ID . POINTS) from RAW-GROUPS.
Only the assignments that count toward the grade: published and not
omitted from the final grade."
  (let (map)
    (dolist (group raw-groups)
      (dolist (a (append (alist-get 'assignments group) nil))
        (when (and (eq (alist-get 'published a) t)
                   (not (eq (alist-get 'omit_from_final_grade a) t)))
          (let ((points (alist-get 'points_possible a)))
            (push (cons (alist-get 'id a)
                        (cons (alist-get 'id group) (if (numberp points) points 0)))
                  map)))))
    (nreverse map)))

(defun org-canvas--gradebook-submission-item (submission assignments unposted)
  "Return SUBMISSION as an item when it counts toward the grade, else nil.
ASSIGNMENTS is the map from `org-canvas--gradebook-assignment-map'.
A score counts when it is graded and not excused, and, unless UNPOSTED,
when it is posted."
  (let* ((id (alist-get 'assignment_id submission))
         (entry (alist-get id assignments))
         (score (alist-get 'score submission)))
    (and entry (numberp score)
         (not (eq (alist-get 'excused submission) t))
         (or unposted (stringp (alist-get 'posted_at submission)))
         (list id score (cdr entry)))))

(defun org-canvas--gradebook-items-by-student (submissions assignments unposted)
  "Return a hash of user id to the counted items among SUBMISSIONS.
ASSIGNMENTS and UNPOSTED are as for `org-canvas--gradebook-submission-item'."
  (let ((by (make-hash-table :test 'eql)))
    (dolist (s submissions)
      (let ((item (org-canvas--gradebook-submission-item s assignments unposted)))
        (when item
          (push item (gethash (alist-get 'user_id s) by)))))
    by))

(defun org-canvas--gradebook-canvas-scheme (standard)
  "Return the Canvas grading STANDARD's entries as (LETTER . CUTOFF-PERCENT).
Canvas answers the cutoffs as fractions."
  (mapcar (lambda (e) (cons (alist-get 'name e) (* 100.0 (alist-get 'value e))))
          (append (alist-get 'grading_scheme standard) nil)))

(defun org-canvas--gradebook-letters-p (enrollments)
  "Return non-nil when any of ENROLLMENTS carries a letter grade."
  (cl-some (lambda (e) (let ((grades (alist-get 'grades e)))
                         (or (stringp (alist-get 'current_grade grades))
                             (stringp (alist-get 'unposted_current_grade grades)))))
           enrollments))

(defun org-canvas--gradebook-fetch-scheme (course enrollments)
  "Return the grading scheme of COURSE as (LETTER . CUTOFF-PERCENT), or nil.
A course naming a standard gets that standard; one naming none whose
ENROLLMENTS carry letters gets Canvas's default scheme; one without
letters gets nil.  A refused standard is logged and answered with
nil: the letters are left out, the pull goes on."
  (let ((id (alist-get 'grading_standard_id course)))
    (cond
     ((and (numberp id) (> id 0))
      (condition-case err
          (org-canvas--gradebook-canvas-scheme
           (org-canvas-api-request
            'GET (org-canvas-api-course-endpoint "grading_standards/%s" id)))
        (org-canvas-api-error
         (org-canvas--log-warning org-canvas--logger
           "[Gradebook] Could not read grading standard %s (%s); letters are left out"
           id (error-message-string err))
         nil)))
     ((org-canvas--gradebook-letters-p enrollments) org-canvas--gradebook-default-scheme))))

(defun org-canvas--gradebook-fetch-scores (rows enrollments &optional course)
  "Return every counted score of the students in ROWS, or the symbol `refused'.
The result is the plist the Groups table and the what-if read:
:pulled-at, :unposted, :weighted, :scheme, :groups, :assignments
and :students, one plist per row with :user-id, :name, :canvas (the
row's current score) and :items.  ENROLLMENTS tell whether the course
shows letters.  COURSE is the course object when already read, so it
is not read twice.  A refusal is logged and answered with `refused'
\(the #171 rule)."
  (condition-case err
      (let* ((course (or course
                         (org-canvas-api-request 'GET (org-canvas-api-course-endpoint ""))))
             (raw-groups (append (org-canvas-api-request-all-pages
                                  'GET (org-canvas-api-course-endpoint "assignment_groups")
                                  '(("include[]" . "assignments")))
                                 nil))
             (assignments (org-canvas--gradebook-assignment-map raw-groups))
             (by (org-canvas--gradebook-items-by-student
                  (append (org-canvas-api-request-all-pages
                           'GET (org-canvas-api-course-endpoint "students/submissions")
                           '(("student_ids[]" . "all")))
                          nil)
                  assignments org-canvas-gradebook-unposted)))
        (list :pulled-at (format-time-string "%Y-%m-%d %H:%M")
              :unposted org-canvas-gradebook-unposted
              :weighted (eq (alist-get 'apply_assignment_group_weights course) t)
              :scheme (org-canvas--gradebook-fetch-scheme course enrollments)
              :groups (org-canvas--gradebook-canvas-groups raw-groups)
              :assignments assignments
              :students (mapcar (lambda (row)
                                  (list :user-id (plist-get row :user-id)
                                        :name (plist-get row :name)
                                        :canvas (plist-get row :current)
                                        :items (gethash (plist-get row :user-id) by)))
                                rows)))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Gradebook] Could not read the scores (%s); the Groups table is left out"
       (error-message-string err))
     'refused)))

(defun org-canvas--gradebook-write-scores (scores)
  "Write SCORES to `org-canvas-gradebook-scores-file' for the what-if.
The file is then kept out of git by `org-canvas--student-data-ignore'."
  (let ((coding-system-for-write 'utf-8-unix)
        (print-length nil)
        (print-level nil)
        (file (expand-file-name org-canvas-gradebook-scores-file)))
    (with-temp-file file
      (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n"
              ";; org-canvas gradebook scores: every student's score on every assignment.\n"
              ";; Written by `org-canvas-pull-gradebook'; keep it out of a course repository.\n")
      (prin1 scores (current-buffer))
      (insert "\n"))
    (org-canvas--student-data-ignore file)))

(defun org-canvas--gradebook-read-scores ()
  "Return the scores the last gradebook pull kept, or nil when there are none."
  (let ((file (expand-file-name org-canvas-gradebook-scores-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (goto-char (point-min))
        (read (current-buffer))))))

;;;; Group Scores: The Groups Table

(defun org-canvas--gradebook-student-total (student groups assignments weighted)
  "Return STUDENT's recomputed total under GROUPS, ASSIGNMENTS and WEIGHTED."
  (org-canvas--gradebook-total
   (org-canvas--gradebook-student-groups (plist-get student :items) groups assignments)
   weighted))

(defun org-canvas--gradebook-differs-p (computed canvas)
  "Return non-nil when COMPUTED and CANVAS's total differ by more than 0.1.
One number without the other differs too; two nils do not."
  (if (and (numberp computed) (numberp canvas))
      (> (abs (- computed canvas)) 0.1)
    (not (eq (null computed) (null canvas)))))

(defun org-canvas--gradebook-insert-group-row (student scores)
  "Insert the Groups table line for STUDENT from SCORES at point.
Return non-nil when the recomputed total differs from Canvas's."
  (let* ((groups (plist-get scores :groups))
         (per-group (org-canvas--gradebook-student-groups
                     (plist-get student :items) groups (plist-get scores :assignments)))
         (computed (org-canvas--gradebook-total per-group (plist-get scores :weighted)))
         (canvas (plist-get student :canvas)))
    (insert (format "| %s |%s %s | %s |\n"
                    (org-canvas--gradebook-cell-text (plist-get student :name))
                    (mapconcat (lambda (pg)
                                 (format " %s |" (org-canvas--gradebook-number
                                                  (org-canvas--gradebook-percent (cdr pg)))))
                               per-group "")
                    (org-canvas--gradebook-number computed)
                    (org-canvas--gradebook-number canvas)))
    (org-canvas--gradebook-differs-p computed canvas)))

(defun org-canvas--gradebook-insert-groups (scores)
  "Insert the Groups table for SCORES at point, and the check under it.
SCORES is the plist from `org-canvas--gradebook-fetch-scores', or
`refused', which writes a note instead.  Returns the names of the
students whose recomputed total differs from Canvas's."
  (if (eq scores 'refused)
      (progn (insert "Canvas refused the scores for this token.\n") nil)
    (let ((groups (plist-get scores :groups))
          (differ nil))
      (insert (format "| Student |%s Computed | Canvas |\n"
                      (mapconcat (lambda (g) (format " %s |" (org-canvas--gradebook-cell-text
                                                             (plist-get g :name))))
                                 groups "")))
      (insert (format "|---|%s---+---|\n" (mapconcat (lambda (_) "---+") groups "")))
      (dolist (student (plist-get scores :students))
        (when (org-canvas--gradebook-insert-group-row student scores)
          (push (plist-get student :name) differ)))
      (insert (if differ
                  (format "\n%d student(s) whose computed total differs from Canvas's by more than 0.1: %s\n"
                          (length differ) (string-join (nreverse differ) "; "))
                "\nEvery computed total is within 0.1 of Canvas's.\n"))
      differ)))

(provide 'org-canvas-gradebook-scores)
;;; org-canvas-gradebook-scores.el ends here
