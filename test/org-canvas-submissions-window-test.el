;;; org-canvas-submissions-window-test.el --- Tests for section-window scoring -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-score-by-window' (issue #383):
;; a grading file scored by when each student submitted, against the
;; windows of their own sections.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-window)

(defvar org-canvas-people-file)
(defvar org-canvas-sections-file)
(defvar org-canvas-assignments-file)

(defconst test-window-people
  (concat
   "* Students\n"
   "** A\n:PROPERTIES:\n:USER_ID: 1\n:SECTIONS: [[file:sections.org::*Section 101][CPSC 2921 101]]\n:END:\n"
   "** B\n:PROPERTIES:\n:USER_ID: 2\n:SECTIONS: [[file:sections.org::*Section 201][CPSC 2921 201]]\n:END:\n"
   "** C\n:PROPERTIES:\n:USER_ID: 3\n:SECTIONS: [[file:sections.org::*Section 101][CPSC 2921 101]]\n:END:\n"
   "** D\n:PROPERTIES:\n:USER_ID: 4\n:SECTIONS: [[file:sections.org::*Section 102][CPSC 2921 102]]\n:END:\n"
   "** E\n:PROPERTIES:\n:USER_ID: 5\n:SECTIONS: [[file:sections.org::*Section 102][CPSC 2921 102]]\n:END:\n"
   "** F\n:PROPERTIES:\n:USER_ID: 6\n:END:\n"
   "** G\n:PROPERTIES:\n:USER_ID: 7\n:SECTIONS: Section 101\n:END:\n"
   "** K\n:PROPERTIES:\n:USER_ID: 11\n:SECTIONS: Lecture 100, [[file:sections.org::*Section 201][CPSC 2921 201]]\n:END:\n")
  "people.org for the window specs: sections by link, by name, several, none.")

(defconst test-window-sections
  (concat "* Section 101\n:PROPERTIES:\n:CANVAS_ID: 101\n:END:\n"
          "* Section 102\n:PROPERTIES:\n:CANVAS_ID: 102\n:MEETS: F 10:10-11:00\n:END:\n"
          "* Section 201\n:PROPERTIES:\n:CANVAS_ID: 201\n:MEETS: F 07:00-07:30\n:END:\n"
          "* Lecture 100\n:PROPERTIES:\n:CANVAS_ID: 100\n:MEETS: MW 10:10-11:00\n:END:\n"
          "* Broken\n:PROPERTIES:\n:CANVAS_ID: 9\n:MEETS: whenever\n:END:\n")
  "sections.org: 102 by MEETS alone, 201's MEETS overruled by the table.")

(defconst test-window-assignments
  (concat "* Attendance 01\n:PROPERTIES:\n:CANVAS_ID: 1001\n:END:\n\n"
          "#+NAME: overrides\n"
          "| Section | Due At | Unlock At | Lock At |\n|-\n"
          "| [[file:sections.org::*Section 101][Section 101]] | <2026-09-18 Fri 09:55> | <2026-09-18 Fri 09:05> |  |\n"
          "| [[file:sections.org::*Section 201][Section 201]] | <2026-09-18 Fri 13:10> | <2026-09-18 Fri 12:20> |  |\n")
  "assignments.org: the column's overrides table gives 101 and 201.")

(defun test-window-student (name uid &rest props)
  "Return a grading-file heading NAME for USER_ID UID with PROPS (NAME VALUE ...)."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n" name uid)
          (let (lines)
            (while props
              (push (format ":%s: %s\n" (pop props) (pop props)) lines))
            (apply #'concat (nreverse lines)))
          ":END:\n\n** Notes\n# Your notes for this student.\n\n** Comment to post\n# Draft.\n\n"))

(defconst test-window-grading
  (concat
   "#+TITLE: Submissions: Attendance 01\n"
   "#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
   "#+PROPERTY: CANVAS_ASSIGNMENT_NAME Attendance 01\n"
   "#+PROPERTY: POINTS_POSSIBLE 1\n\n"
   (test-window-student "A" 1 "STATUS" "submitted" "SUBMITTED_AT" "<2026-09-18 Fri 09:30>")
   (test-window-student "B" 2 "STATUS" "submitted" "SUBMITTED_AT" "<2026-09-18 Fri 12:16>")
   (test-window-student "C" 3 "STATUS" "submitted" "SUBMITTED_AT" "<2026-09-18 Fri 11:19>")
   (test-window-student "D" 4 "STATUS" "submitted" "SUBMITTED_AT" "<2026-09-18 Fri 10:30>")
   (test-window-student "E" 5 "STATUS" "submitted" "SUBMITTED_AT" "<2026-09-19 Sat 10:30>")
   (test-window-student "F" 6 "STATUS" "submitted" "SUBMITTED_AT" "<2026-09-18 Fri 10:30>")
   (test-window-student "G" 7 "STATUS" "unsubmitted")
   (test-window-student "H" 1 "STATUS" "graded" "SUBMITTED_AT" "<2026-09-18 Fri 11:30>"
                        "SCORE" "0.5" "CANVAS_SCORE" "0.5")
   (test-window-student "I" 1 "STATUS" "graded" "SUBMITTED_AT" "<2026-09-18 Fri 11:30>"
                        "SCORE" "EX" "CANVAS_SCORE" "EX")
   (test-window-student "J" 1 "STATUS" "left")
   (test-window-student "K" 11 "STATUS" "submitted" "SUBMITTED_AT" "<2026-09-18 Fri 12:40>")
   "* Comment Bank\n")
  "A grading file with one student per case the command must tell apart.")

(defmacro with-window-course (&rest body)
  "Run BODY in a scratch course holding the window fixtures.
`dir' is the course directory; the grading file is submissions/Attendance 01.org.
Every buffer visiting a file under it is killed afterwards."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "org-canvas-window-" t)))
          (org-canvas-submissions-directory (expand-file-name "submissions" dir))
          (org-canvas-people-file (expand-file-name "people.org" dir))
          (org-canvas-sections-file (expand-file-name "sections.org" dir))
          (org-canvas-assignments-file (expand-file-name "assignments.org" dir))
          (org-canvas-submissions-window-grace 0)
          (noninteractive t))
     (unwind-protect
         (progn
           (make-directory org-canvas-submissions-directory)
           (with-temp-file org-canvas-people-file (insert test-window-people))
           (with-temp-file org-canvas-sections-file (insert test-window-sections))
           (with-temp-file org-canvas-assignments-file (insert test-window-assignments))
           (with-temp-file (expand-file-name "Attendance 01.org" org-canvas-submissions-directory)
             (insert test-window-grading))
           (cl-letf (((symbol-function 'org-canvas--report-display)
                      (lambda (_name render &optional _mode)
                        (with-temp-buffer (funcall render) (buffer-string))))
                     ((symbol-function 'org-canvas--log-warning) #'ignore))
             ,@body))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p (file-truename dir)
                                     (file-truename (buffer-file-name b))))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun test-window-score (name)
  "Return the SCORE of the heading NAME in the current buffer."
  (goto-char (point-min))
  (re-search-forward (format "^\\* %s$" (regexp-quote name)))
  (org-entry-get (point) "SCORE"))

(defun test-window-notes (name)
  "Return the Notes text of the heading NAME in the current buffer."
  (goto-char (point-min))
  (re-search-forward (format "^\\* %s$" (regexp-quote name)))
  (or (org-canvas--submissions-section-text org-canvas--submissions-notes-heading) ""))

(describe "org-canvas--submissions-window-minutes"
  (it "reads a wall clock the same whatever zone Emacs runs in"
    (let ((orig-tz (getenv "TZ")) results)
      (unwind-protect
          (dolist (zone '("UTC" "America/New_York" "Asia/Tokyo"))
            (set-time-zone-rule zone)
            (push (list (org-canvas--submissions-window-minutes "<2026-09-18 Fri 09:05>")
                        (org-canvas--submissions-window-day
                         (org-canvas--submissions-window-minutes "<2026-09-18 Fri 09:05>")))
                  results))
        (set-time-zone-rule orig-tz))
      (expect (length (delete-dups results)) :to-equal 1)
      (expect (nth 1 (car results)) :to-equal "Fri 09:05")))

  (it "counts minutes across midnight and ignores what is not a timestamp"
    (expect (- (org-canvas--submissions-window-minutes "<2026-09-19 Sat 00:10>")
               (org-canvas--submissions-window-minutes "<2026-09-18 Fri 23:50>"))
            :to-equal 20)
    (expect (org-canvas--submissions-window-minutes nil) :to-be nil)
    (expect (org-canvas--submissions-window-minutes "yesterday") :to-be nil)))

(describe "org-canvas--submissions-window-judge"
  :var (context at)
  (before-each
    (setq context (list :grace 0
                        :overrides '(("101" 100 150))
                        :meets (let ((h (make-hash-table :test 'equal)))
                                 ;; 2026-09-18 is a Friday.
                                 (puthash "102" '(((5) 600 660)) h)
                                 h))
          at (org-canvas--submissions-window-minutes "<2026-09-18 Fri 10:30>")))

  (it "is inside a MEETS window on a meeting day and outside off it"
    (let* ((judged (org-canvas--submissions-window-judge at '("102") context))
           (verdict (car judged)))
      (expect verdict :to-be 'inside))
    (let* ((sat (+ at 1440))
           (judged (org-canvas--submissions-window-judge sat '("102") context))
           (verdict (car judged)))
      (expect verdict :to-be 'outside)
      (expect (cdr judged) :to-be nil)))

  (it "has no window for a section with no rule, or for no section at all"
    (expect (car (org-canvas--submissions-window-judge at '("999") context))
            :to-be 'no-window)
    (expect (car (org-canvas--submissions-window-judge at nil context))
            :to-be 'no-window))

  (it "widens a window by the grace on both sides"
    (let ((ctx (plist-put (copy-sequence context) :grace 5)))
      (expect (car (org-canvas--submissions-window-judge 95 '("101") ctx)) :to-be 'inside)
      (expect (car (org-canvas--submissions-window-judge 155 '("101") ctx)) :to-be 'inside)
      (expect (car (org-canvas--submissions-window-judge 156 '("101") ctx)) :to-be 'outside))))

(describe "org-canvas-submissions-score-by-window"
  (it "scores inside, outside and unsubmitted, and leaves the rest alone"
    (with-window-course
      (let ((buf (org-canvas-submissions-score-by-window "Attendance 01" 5)))
        (with-current-buffer buf
          (expect (mapcar #'test-window-score '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"))
                  :to-equal '("1" "1" "0" "1" "0" nil "0" "0.5" "EX" nil "1"))
          (expect (buffer-modified-p) :to-be nil)
          (expect (test-window-notes "A")
                  :to-match "Window rule: submitted Fri 09:30, sections Section 101, window Section 101 09:05-09:55, grace 5 min: inside, score 1")
          (expect (test-window-notes "E") :to-match "outside, score 0")
          (expect (test-window-notes "F") :not :to-match "Window rule")
          (expect (test-window-notes "H") :not :to-match "Window rule")
          (expect (test-window-notes "K") :to-match "window Section 201 12:20-13:10"))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "Attendance 01.org"
                                                  org-canvas-submissions-directory))
          (expect (buffer-string) :to-match ":SCORE: +1\n")))))

  (it "reports the outliers before writing, with their times"
    (with-window-course
      (let (report)
        (cl-letf (((symbol-function 'org-canvas--report-display)
                   (lambda (_name render &optional _mode)
                     (setq report (with-temp-buffer (funcall render) (buffer-string))))))
          (org-canvas-submissions-score-by-window "Attendance 01" 5))
        (expect report :to-match "4 inside, 4 outside, 1 unsubmitted, 1 without a window; 7 to write (grace 5 min, full credit 1)")
        (expect report :to-match "\\* Outside the window (0)")
        (expect report :to-match "| C +| Section 101 +| Fri 11:19 +| Section 101 09:05-09:55 +| 0 +|")
        (expect report :to-match "| E +| Section 102 +| Sat 10:30 +| - +| 0 +|")
        (expect report :to-match "| H +| .* +| left as kept +|")
        (expect report :to-match "| I +| .* +| left as excused +|")
        (expect report :to-match "\\* No window (left unscored)\n\n| Student")
        (expect report :to-match "| F +| - +| Fri 10:30 +| - +| left as unscored +|")
        (expect report :to-match "\\* Unsubmitted (0)"))))

  (it "takes the grace from the option when a script passes none"
    (with-window-course
      (let ((org-canvas-submissions-window-grace 5))
        (with-current-buffer (org-canvas-submissions-score-by-window "Attendance 01")
          (expect (test-window-score "B") :to-equal "1")))))

  (it "is strict without grace: early and late by minutes are outside"
    (with-window-course
      (with-current-buffer (org-canvas-submissions-score-by-window "Attendance 01" 0)
        (expect (test-window-score "B") :to-equal "0")
        (expect (test-window-score "A") :to-equal "1"))))

  (it "replaces its own note and typed scores only when asked to overwrite"
    (with-window-course
      (with-current-buffer (org-canvas-submissions-score-by-window "Attendance 01" 0)
        (org-canvas-submissions-score-by-window nil 5 t)
        (expect (test-window-score "B") :to-equal "1")
        (expect (test-window-score "H") :to-equal "0")
        (expect (test-window-score "I") :to-equal "EX")
        (let ((notes (test-window-notes "B")))
          (expect (length (split-string notes "Window rule" t)) :to-equal 2)
          (expect notes :to-match "grace 5 min: inside"))
        (expect (how-many "^# Your notes" (point-min) (point-max)) :to-equal 11))))

  (it "fills the Rubric rows of full credit only"
    (with-window-course
      (let (filled)
        (cl-letf (((symbol-function 'org-canvas--submissions-rubric-fill-full)
                   (lambda () (push (org-get-heading t t t t) filled) 0)))
          (org-canvas-submissions-score-by-window "Attendance 01" 5))
        (expect (sort filled #'string<) :to-equal '("A" "B" "D" "K")))))

  (it "adds a Notes heading to an entry without one"
    (with-window-course
      (with-current-buffer (org-canvas-open-submissions "Attendance 01")
        (goto-char (point-min))
        (re-search-forward "^\\* A$")
        (org-mark-subtree)
        (delete-region (point) (mark))
        (insert "* A\n:PROPERTIES:\n:USER_ID: 1\n:SUBMITTED_AT: <2026-09-18 Fri 09:30>\n:END:\n")
        (org-canvas-submissions-score-by-window nil 0)
        (expect (test-window-score "A") :to-equal "1")
        (expect (test-window-notes "A") :to-match "\\`- Window rule: .*inside, score 1\\'"))))

  (it "writes nothing when the confirmation is declined"
    (with-window-course
      (let ((noninteractive nil) asked)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) nil)))
          (with-current-buffer (org-canvas-submissions-score-by-window "Attendance 01" 5)
            (expect asked :to-match "Write 7 score(s)")
            (expect (test-window-score "A") :to-be nil)
            (expect (test-window-notes "A") :not :to-match "Window rule"))))))

  (it "asks for the grace interactively, the prefix argument overwriting"
    (with-window-course
      (with-current-buffer (org-canvas-open-submissions "Attendance 01")
        (let ((noninteractive nil) (current-prefix-arg '(4)) default)
          (cl-letf (((symbol-function 'read-number)
                     (lambda (_prompt &optional def) (setq default def) 5))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (let ((org-canvas-submissions-window-grace 3))
              (call-interactively #'org-canvas-submissions-score-by-window))
            (expect default :to-equal 3)
            (expect (test-window-score "H") :to-equal "0")
            (expect (test-window-score "B") :to-equal "1"))))))

  (it "takes the points from the caller, and asks for them only when it can"
    (with-window-course
      (with-current-buffer (org-canvas-open-submissions "Attendance 01")
        (goto-char (point-min))
        (re-search-forward "^#\\+PROPERTY: POINTS_POSSIBLE 1\n")
        (replace-match "")
        (expect (org-canvas-submissions-score-by-window nil 5) :to-throw 'user-error)
        (org-canvas-submissions-score-by-window nil 5 nil 2)
        (expect (test-window-score "A") :to-equal "2")
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 3))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-score-by-window nil 5 t))
          (expect (test-window-score "A") :to-equal "3")))))

  (it "refuses the summary view"
    (with-window-course
      (with-current-buffer (org-canvas-open-submissions "Attendance 01")
        (setq org-canvas-submissions--current-view 'summary)
        (expect (org-canvas-submissions-score-by-window nil 5) :to-throw 'user-error))))

  (it "scores nothing it cannot place when people and sections are missing"
    (with-window-course
      (let ((org-canvas-people-file (expand-file-name "none.org" dir))
            (org-canvas-sections-file (expand-file-name "none.org" dir)))
        (with-current-buffer (org-canvas-submissions-score-by-window "Attendance 01" 5)
          (expect (test-window-score "A") :to-be nil)
          (expect (test-window-score "G") :to-equal "0")))))

  (it "warns about a MEETS it cannot read and ignores it"
    (with-window-course
      (let (warnings)
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
          (org-canvas--submissions-window-meets))
        (expect warnings :to-equal
                '("[Window] Section 'Broken': MEETS 'whenever' does not parse; ignored"))))))

;;; org-canvas-submissions-window-test.el ends here
