;;; org-canvas-rubric-results-test.el --- Buttercup tests for the rubric results pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-rubric-results': rubric assessments on each
;; assignment's submissions folded into one table per assignment in
;; rubric-results.org.  Every request is answered by a fake keyed on
;; the URL; nothing here reaches the network (Hard Rule 2), and the log
;; is never read from the shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: the tier list lives in org-canvas.el and the
;; assignment link needs `org-canvas-assignments-file' to be special.
(require 'org-canvas)

(defvar test-rubric-results--assignments nil
  "Assignments the fake API lists for the course.")
(defvar test-rubric-results--submissions nil
  "Alist of assignment id to the submissions the fake API lists for it.")

(defun test-rubric-results--api (_method url &optional _params)
  "Answer URL from the fake tables, as the paginated helper would."
  (cond
   ((string-match "/assignments/\\([0-9]+\\)/submissions" url)
    (alist-get (string-to-number (match-string 1 url)) test-rubric-results--submissions))
   ((string-match "/assignments\\'" url) test-rubric-results--assignments)
   (t (error "Unexpected request: %s" url))))

(defun test-rubric-results--criterion (id description points &rest ratings)
  "A rubric criterion ID described as DESCRIPTION worth POINTS with RATINGS.
Each rating is (ID DESCRIPTION POINTS)."
  `((id . ,id) (description . ,description) (points . ,points)
    (ratings . ,(vconcat (mapcar (lambda (r) `((id . ,(nth 0 r)) (description . ,(nth 1 r))
                                              (points . ,(nth 2 r))))
                                 ratings)))))

(defun test-rubric-results--assignment (id name &rest criteria)
  "An assignment ID named NAME; with CRITERIA it carries a rubric."
  (append `((id . ,id) (name . ,name))
          (when criteria
            `((rubric_settings . ((id . ,(+ 500 id)) (title . ,(format "%s rubric" name))))
              (rubric . ,(vconcat criteria))))))

(defun test-rubric-results--submission (uid &rest assessment)
  "A submission by UID whose rubric assessment is ASSESSMENT.
ASSESSMENT pairs a criterion id with (POINTS RATING-ID); none means
the submission was never assessed."
  `((id . ,(+ 9000 uid)) (user_id . ,uid) (workflow_state . "graded")
    (rubric_assessment . ,(and assessment
                               (mapcar (lambda (a)
                                         (cons (intern (car a))
                                               `((points . ,(nth 0 (cdr a)))
                                                 (rating_id . ,(nth 1 (cdr a)))
                                                 (comments . ""))))
                                       assessment)))))

(defmacro test-rubric-results--with-course (assignments submissions &rest body)
  "Run BODY with the fake API serving ASSIGNMENTS and SUBMISSIONS.
The results and assignments files live in a temp directory."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "rubric-results-" t))
          (org-canvas-rubric-results-file (expand-file-name "rubric-results.org" dir))
          (org-canvas-assignments-file (expand-file-name "assignments.org" dir))
          (test-rubric-results--assignments ,assignments)
          (test-rubric-results--submissions ,submissions))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-rubric-results--api)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-rubric-results-file org-canvas-assignments-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-rubric-results--file ()
  "Return rubric-results.org's text."
  (with-temp-buffer (insert-file-contents org-canvas-rubric-results-file) (buffer-string)))

(defun test-rubric-results--row (criterion)
  "Return the table row of CRITERION, cells trimmed, or nil."
  (let ((text (test-rubric-results--file)))
    (when (string-match (format "^| %s *|\\(.*\\)$" (regexp-quote criterion)) text)
      (mapcar #'string-trim (split-string (match-string 1 text) "|" t)))))

(defconst test-rubric-results--clarity
  (test-rubric-results--criterion "_10" "Clarity" 4
                                  '("r1" "Excellent" 4) '("r2" "Good" 2) '("r3" "Poor" 0))
  "A criterion with three ratings.")

(defconst test-rubric-results--evidence
  (test-rubric-results--criterion "_20" "Evidence" 6
                                  '("r4" "Strong" 6) '("r5" "Weak" 3))
  "A second criterion with two ratings.")

(describe "org-canvas--rubric-results-criterion-row"
  (it "counts the assessed, averages the points and lists the ratings reached"
    (let* ((assessments
            (mapcar (lambda (s) (alist-get 'rubric_assessment s))
                    (list (test-rubric-results--submission 1 '("_10" 4 "r1") '("_20" 6 "r4"))
                          (test-rubric-results--submission 2 '("_10" 2 "r2"))
                          (test-rubric-results--submission 3 '("_10" 4 "r1") '("_20" 3 "r5")))))
           (row (org-canvas--rubric-results-criterion-row test-rubric-results--clarity assessments)))
      (expect (plist-get row :description) :to-equal "Clarity")
      (expect (plist-get row :assessed) :to-equal 3)
      (expect (plist-get row :mean) :to-be-close-to 3.333 2)
      (expect (plist-get row :out-of) :to-equal 4)
      (expect (plist-get row :ratings) :to-equal "Excellent 2, Good 1")
      (let ((evidence (org-canvas--rubric-results-criterion-row test-rubric-results--evidence assessments)))
        (expect (plist-get evidence :assessed) :to-equal 2)
        (expect (plist-get evidence :mean) :to-equal 4.5)
        (expect (plist-get evidence :ratings) :to-equal "Strong 1, Weak 1"))))

  (it "counts a score matching no rating under other and an unscored criterion not at all"
    (let* ((assessments
            (mapcar (lambda (s) (alist-get 'rubric_assessment s))
                    (list (test-rubric-results--submission 1 '("_10" 3 nil))
                          (test-rubric-results--submission 2 '("_10" 4 "r1"))
                          (test-rubric-results--submission 3 '("_10" :null "r2")))))
           (row (org-canvas--rubric-results-criterion-row test-rubric-results--clarity assessments)))
      (expect (plist-get row :assessed) :to-equal 2)
      (expect (plist-get row :mean) :to-equal 3.5)
      (expect (plist-get row :ratings) :to-equal "Excellent 1, other 1")))

  (it "renders a dash for a criterion nobody was scored on"
    (let ((row (org-canvas--rubric-results-criterion-row test-rubric-results--clarity nil)))
      (expect (plist-get row :assessed) :to-equal 0)
      (expect (plist-get row :mean) :to-be nil)
      (expect (plist-get row :ratings) :to-equal "-"))))

(describe "org-canvas--rubric-results-number"
  (it "renders a mean with one decimal, a count as an integer, and a dash for nothing"
    (expect (org-canvas--rubric-results-number 3.333) :to-equal "3.3")
    (expect (org-canvas--rubric-results-number 4) :to-equal "4")
    (expect (org-canvas--rubric-results-number nil) :to-equal "-")
    (expect (org-canvas--rubric-results-number "A") :to-equal "A")))

(describe "org-canvas-pull-rubric-results"
  (it "writes one heading per rubric assignment, in order, with its properties and table"
    (test-rubric-results--with-course
        (list (test-rubric-results--assignment 1 "Essay 1"
                                               test-rubric-results--clarity
                                               test-rubric-results--evidence)
              (test-rubric-results--assignment 2 "Quiz 1")
              (test-rubric-results--assignment 3 "Essay 2" test-rubric-results--clarity))
        (list (cons 1 (list (test-rubric-results--submission 11 '("_10" 4 "r1") '("_20" 6 "r4"))
                            (test-rubric-results--submission 12 '("_10" 2 "r2") '("_20" 3 "r5"))
                            (test-rubric-results--submission 13)))
              (cons 3 (list (test-rubric-results--submission 11)
                            (test-rubric-results--submission 12))))
      (org-canvas-pull-rubric-results)
      (let ((text (test-rubric-results--file)))
        (expect text :to-match "^#\\+TITLE: Rubric Results\n")
        (expect text :to-match "^#\\+LAST_SYNCED:")
        (expect text :to-match "^\\* Essay 1\n")
        (expect text :to-match "^\\* Essay 2\n")
        (expect text :not :to-match "Quiz 1")
        (expect (string-match "\\* Essay 1" text) :to-be-less-than (string-match "\\* Essay 2" text))
        (expect text :to-match ":ASSIGNMENT_ID: +1\n")
        (expect text :to-match ":RUBRIC_ID: +501\n")
        (expect text :to-match ":SUBMISSIONS: +3\n")
        (expect text :to-match ":ASSESSED: +2\n")
        (expect text :to-match "^| Criterion +| Assessed +| Mean +| Out of +| Ratings +|$")
        (expect (test-rubric-results--row "Clarity") :to-equal '("2" "3.0" "4" "Excellent 1, Good 1"))
        (expect (test-rubric-results--row "Evidence") :to-equal '("2" "4.5" "6" "Strong 1, Weak 1"))
        (expect text :to-match ":ASSESSED: +0\n")
        (expect text :to-match "^No assessments yet\\.$")
        (expect text :not :to-match "user_id\\|:USER_ID:\\|9011"))))

  (it "links a heading to its assignments.org heading when that file holds the id"
    (test-rubric-results--with-course
        (list (test-rubric-results--assignment 1 "Essay 1" test-rubric-results--clarity))
        (list (cons 1 (list (test-rubric-results--submission 11 '("_10" 4 "r1")))))
      (with-temp-file org-canvas-assignments-file
        (insert "* Essay 1 (draft)\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
      (org-canvas-pull-rubric-results)
      (expect (test-rubric-results--file)
              :to-match "^\\* \\[\\[file:assignments.org::\\*Essay 1 (draft)\\]\\[Essay 1\\]\\]\n")))

  (it "rewrites the whole file on a re-pull and keeps nothing written by hand"
    (test-rubric-results--with-course
        (list (test-rubric-results--assignment 1 "Essay 1" test-rubric-results--clarity)
              (test-rubric-results--assignment 3 "Essay 2" test-rubric-results--clarity))
        (list (cons 1 (list (test-rubric-results--submission 11 '("_10" 4 "r1"))))
              (cons 3 nil))
      (org-canvas-pull-rubric-results)
      (with-temp-buffer
        (insert-file-contents org-canvas-rubric-results-file)
        (goto-char (point-max))
        (insert "A note I wrote\n")
        (write-region (point-min) (point-max) org-canvas-rubric-results-file))
      ;; Essay 2 loses its rubric; Essay 1 gains an assessment.
      (setq test-rubric-results--assignments
            (list (test-rubric-results--assignment 1 "Essay 1" test-rubric-results--clarity)
                  (test-rubric-results--assignment 3 "Essay 2")))
      (setq test-rubric-results--submissions
            (list (cons 1 (list (test-rubric-results--submission 11 '("_10" 4 "r1"))
                                (test-rubric-results--submission 12 '("_10" 0 "r3"))))))
      (org-canvas-pull-rubric-results)
      (let ((text (test-rubric-results--file)))
        (expect text :not :to-match "Essay 2")
        (expect text :not :to-match "A note I wrote")
        (expect (test-rubric-results--row "Clarity") :to-equal '("2" "2.0" "4" "Excellent 1, Poor 1"))
        (expect (length (split-string text "^\\* " t)) :to-equal 2))))

  (it "writes the empty-file note when no assignment carries a rubric"
    (test-rubric-results--with-course
        (list (test-rubric-results--assignment 2 "Quiz 1"))
        nil
      (org-canvas-pull-rubric-results)
      (expect (test-rubric-results--file) :to-match "Canvas returned 0 items")))

  (it "is in the pull tiers after the gradebook"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-gradebook names)
              :to-be-less-than (cl-position 'org-canvas-pull-rubric-results names)))))

(provide 'org-canvas-rubric-results-test)
;;; org-canvas-rubric-results-test.el ends here
