;;; org-canvas-rubric-results.el --- Pull how the class scored on each rubric criterion -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file pulls a course-wide view of rubric results into
;; rubric-results.org: for every assignment graded with a rubric, how
;; many submissions were assessed and, criterion by criterion, the mean
;; score and how the ratings were distributed.  It answers "which
;; criterion is the class missing?" without opening SpeedGrader or a
;; grading file per student.  It is pull-only and the file is derived:
;; every pull rewrites it whole, so nothing written by hand survives.
;;
;; FILE STRUCTURE
;; ==============
;; In rubric-results.org, one level-1 heading per assignment that
;; carries a rubric, in Canvas's assignment order, titled by the
;; assignment name and linked to its heading in assignments.org when
;; that file holds the assignment's CANVAS_ID:
;;   :ASSIGNMENT_ID:  :RUBRIC_ID:  :SUBMISSIONS:  :ASSESSED:
;;   | Criterion | Assessed | Mean | Out of | Ratings |
;; Ratings is the distribution by rating description, in the rubric's
;; order ("Excellent 12, Good 5, Poor 1"); a score matching no rating
;; counts under "other".  An assignment nobody has been assessed on gets
;; the heading and a "No assessments yet" line instead of a table.
;;
;; PERSONAL DATA
;; =============
;; Only aggregates are written: no name, no user id, no per-student
;; score.  With a small class a row can still identify someone, so
;; treat rubric-results.org as gradebook.org is treated: keep it out of
;; a course repository and out of anything shared.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/assignments
;;       each assignment with a rubric carries `rubric_settings' (id,
;;       title, points_possible) and `rubric', the criteria with their
;;       ratings.  Paginated.
;;   GET /courses/:id/assignments/:id/submissions?include[]=rubric_assessment
;;       one row per student; `rubric_assessment' maps a criterion id
;;       to (points rating_id comments).  Paginated; one request per
;;       rubric assignment.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-rubric-results-file (org-canvas--path "rubric-results.org")
  "Path to the rubric-results.org file.
It holds how the class scored on every rubric criterion; keep it out
of a course repository."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-rubric-results-file "rubric-results.org")

(org-canvas-register-properties "rubric-results"
  :label "Rubric Results"
  :file-var 'org-canvas-rubric-results-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "ASSIGNMENT_ID" :data-key :assignment_id :type number :pull-only t
     :doc "Canvas id of the assignment the rubric grades")
    (:org-prop "RUBRIC_ID" :data-key :rubric_id :type number :pull-only t
     :doc "Canvas id of the rubric attached to the assignment")
    (:org-prop "SUBMISSIONS" :data-key :submissions :type number :pull-only t
     :doc "Submission rows Canvas returned for the assignment, one per student")
    (:org-prop "ASSESSED" :data-key :assessed :type number :pull-only t
     :doc "Submissions carrying a rubric assessment")))

;;;; Fetching

(defun org-canvas--rubric-results-fetch-assignments ()
  "Return the course's assignments that carry a rubric, in Canvas's order."
  (cl-remove-if-not
   (lambda (a) (org-canvas--alist-get-non-null 'rubric_settings a))
   (append (org-canvas-api-request-all-pages
            'GET (org-canvas-api-course-endpoint "assignments"))
           nil)))

(defun org-canvas--rubric-results-fetch-assessments (assignment-id)
  "Return the rubric assessments on ASSIGNMENT-ID's submissions.
The value is (SUBMISSIONS . ASSESSMENTS): how many submission rows
Canvas returned and the non-empty `rubric_assessment' alists among
them, each mapping a criterion id to its points, rating and comment."
  (let* ((rows (append (org-canvas-api-request-all-pages
                        'GET (org-canvas-api-course-endpoint
                              "assignments/%s/submissions" assignment-id)
                        '(("include[]" . "rubric_assessment")))
                       nil))
         (assessments (delq nil (mapcar (lambda (s)
                                          (let ((ra (org-canvas--alist-get-non-null 'rubric_assessment s)))
                                            (and (consp ra) ra)))
                                        rows))))
    (cons (length rows) assessments)))

;;;; Folding Assessments Into Criterion Rows

(defun org-canvas--rubric-results-criterion-scores (criterion-id assessments)
  "Return the (POINTS . RATING-ID) pairs ASSESSMENTS hold for CRITERION-ID.
An assessment that leaves the criterion unscored contributes nothing."
  (let ((key (intern criterion-id)) (scores nil))
    (dolist (assessment assessments)
      (let* ((entry (alist-get key assessment))
             (points (and (consp entry) (org-canvas--alist-get-non-null 'points entry))))
        (when (numberp points)
          (push (cons points (org-canvas--alist-get-non-null 'rating_id entry)) scores))))
    (nreverse scores)))

(defun org-canvas--rubric-results-distribution (ratings scores)
  "Describe how SCORES fall across RATINGS, or - when there are none.
RATINGS are the criterion's rating alists in the rubric's order; a
score whose rating id matches none of them counts under other.
Ratings nobody reached are left out."
  (if (null scores)
      "-"
    (let* ((counted 0)
           (parts
            (delq nil
                  (mapcar (lambda (rating)
                            (let* ((id (alist-get 'id rating))
                                   (n (cl-count-if (lambda (s) (equal (cdr s) id)) scores)))
                              (setq counted (+ counted n))
                              (and (> n 0)
                                   (format "%s %d" (alist-get 'description rating) n))))
                          ratings)))
           (other (- (length scores) counted)))
      (when (> other 0)
        (setq parts (append parts (list (format "other %d" other)))))
      (mapconcat #'identity parts ", "))))

(defun org-canvas--rubric-results-criterion-row (criterion assessments)
  "Return the table row plist for CRITERION over ASSESSMENTS.
Keys: :description, :assessed, :mean (nil when nobody was scored),
:out-of and :ratings, the rating distribution text."
  (let* ((scores (org-canvas--rubric-results-criterion-scores
                  (format "%s" (alist-get 'id criterion)) assessments))
         (points (mapcar #'car scores)))
    (list :description (or (alist-get 'description criterion) (alist-get 'id criterion))
          :assessed (length scores)
          :mean (and points (/ (apply #'+ points) (float (length points))))
          :out-of (alist-get 'points criterion)
          :ratings (org-canvas--rubric-results-distribution
                    (append (alist-get 'ratings criterion) nil) scores))))

(defun org-canvas--rubric-results-entry (assignment)
  "Fetch ASSIGNMENT's assessments and fold them into one entry plist.
Keys: :id, :name, :rubric-id, :submissions, :assessed and :rows, one
`org-canvas--rubric-results-criterion-row' per criterion."
  (let* ((id (alist-get 'id assignment))
         (fetched (org-canvas--rubric-results-fetch-assessments id))
         (assessments (cdr fetched)))
    (list :id id
          :name (or (alist-get 'name assignment) (format "Assignment %s" id))
          :rubric-id (alist-get 'id (alist-get 'rubric_settings assignment))
          :submissions (car fetched)
          :assessed (length assessments)
          :rows (mapcar (lambda (criterion)
                          (org-canvas--rubric-results-criterion-row criterion assessments))
                        (append (alist-get 'rubric assignment) nil)))))

;;;; Rendering

(defun org-canvas--rubric-results-number (value)
  "Render VALUE for a table cell: one decimal for a mean, - when absent."
  (cond ((null value) "-")
        ((integerp value) (format "%d" value))
        ((numberp value) (format "%.1f" value))
        (t (format "%s" value))))

(defun org-canvas--rubric-results-assignment-heading (assignment-id)
  "Return the assignments.org heading carrying ASSIGNMENT-ID, or nil.
Nil as well when `org-canvas-assignments-file' is unset or missing."
  (let ((file (and (boundp 'org-canvas-assignments-file) org-canvas-assignments-file)))
    (when (and file (file-exists-p file))
      (let ((target (format "%s" assignment-id)) (heading nil))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (when (and (not heading)
                          (equal (org-entry-get (point) "CANVAS_ID") target))
                 (setq heading (org-get-heading t t t t))))
             "CANVAS_ID={.}" 'file)))
        heading))))

(defun org-canvas--rubric-results-title (entry)
  "Return ENTRY's heading title: a link to its assignments.org heading, else the name."
  (let ((heading (org-canvas--rubric-results-assignment-heading (plist-get entry :id)))
        (name (plist-get entry :name)))
    (if heading
        (org-link-make-string
         (format "file:%s::*%s" (file-name-nondirectory org-canvas-assignments-file)
                 (replace-regexp-in-string "\\\\\\([][]\\)" "\\1" heading))
         name)
      name)))

(defun org-canvas--rubric-results-insert-table (rows)
  "Insert the criterion table for ROWS at point and align it."
  (let ((start (point)))
    (insert "| Criterion | Assessed | Mean | Out of | Ratings |\n")
    (insert "|---+---+---+---+---|\n")
    (dolist (row rows)
      (insert (format "| %s | %d | %s | %s | %s |\n"
                      (replace-regexp-in-string "[|\n]" " " (format "%s" (plist-get row :description)))
                      (plist-get row :assessed)
                      (org-canvas--rubric-results-number (plist-get row :mean))
                      (org-canvas--rubric-results-number (plist-get row :out-of))
                      (plist-get row :ratings))))
    (save-excursion
      (goto-char start)
      (org-table-align))))

(defun org-canvas--rubric-results-insert-entry (entry)
  "Insert ENTRY as a level-1 heading with its properties and table at point."
  (insert (format "* %s\n" (org-canvas--rubric-results-title entry)))
  (insert ":PROPERTIES:\n")
  (dolist (prop `(("ASSIGNMENT_ID" . ,(plist-get entry :id))
                  ("RUBRIC_ID" . ,(plist-get entry :rubric-id))
                  ("SUBMISSIONS" . ,(plist-get entry :submissions))
                  ("ASSESSED" . ,(plist-get entry :assessed))))
    (when (cdr prop)
      (insert (format ":%s: %s\n" (car prop) (cdr prop)))))
  (insert ":END:\n\n")
  (if (zerop (plist-get entry :assessed))
      (insert "No assessments yet.\n\n")
    (org-canvas--rubric-results-insert-table (plist-get entry :rows))
    (insert "\n")))

;;;; Pull

;;;###autoload
(defun org-canvas-pull-rubric-results ()
  "Pull how the class scored on each rubric criterion into rubric-results.org.
One heading per assignment graded with a rubric, with a table of its
criteria: how many were assessed, the mean, the points possible and
the rating distribution.  Read-only, and the file is derived: every
pull rewrites it whole.  Only aggregates are written, but keep the
file out of a course repository all the same."
  (interactive)
  (org-canvas--start-operation "PULLING RUBRIC RESULTS")
  (let* ((file (expand-file-name org-canvas-rubric-results-file))
         (assignments (org-canvas--rubric-results-fetch-assignments))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "rubric-results")
    (if (null assignments)
        (org-canvas--pull-emit-empty-file file (org-canvas--pull-label-for "rubric-results"))
      (let ((entries (mapcar #'org-canvas--rubric-results-entry assignments)))
        (unless (file-exists-p file)
          (with-temp-file file (insert "")))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (erase-buffer)
          (insert (format "#+TITLE: %s\n\n" (org-canvas--pull-label-for "rubric-results")))
          (dolist (entry entries)
            (org-canvas--rubric-results-insert-entry entry))
          (org-canvas--pull-write-file-header)
          (org-canvas--save-buffer))
        (org-canvas--pull-kill-fresh-buffer file was-fresh)
        (let ((assessed (cl-count-if (lambda (e) (> (plist-get e :assessed) 0)) entries)))
          (org-canvas--log-info org-canvas--logger
            "Rubric results pull complete: %d assignments with a rubric, %d assessed"
            (length entries) assessed)
          (message "Rubric results pull complete: %d assignments with a rubric, %d assessed."
                   (length entries) assessed))))))

(provide 'org-canvas-rubric-results)
;;; org-canvas-rubric-results.el ends here
