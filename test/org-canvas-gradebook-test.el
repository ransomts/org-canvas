;;; org-canvas-gradebook-test.el --- Buttercup tests for the gradebook overview -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-gradebook': student enrollments and the course
;; analytics folded into the three tables of gradebook.org.  Every request
;; is answered by the gradebook fake in test-fixtures.el; nothing here
;; reaches the network (Hard Rule 2), and the log is never read from the
;; shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: the tier list lives in org-canvas.el and the
;; roster link needs `org-canvas-people-file' to be special.
(require 'org-canvas)

(describe "org-canvas--gradebook-rows"
  (it "folds a student's section enrollments into one row with the first score"
    (let ((rows (org-canvas--gradebook-rows
                 (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
                       (test-gradebook--enrollment 1 "Adams, Alice" 20 91.5 88.0)
                       (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.25))
                 (test-gradebook--fold-summaries
                  (list (test-gradebook--summary 1 0 2) (test-gradebook--summary 2 3 1))))))
      (expect (length rows) :to-equal 2)
      (expect (plist-get (car rows) :section-ids) :to-equal '(10 20))
      (expect (plist-get (car rows) :current) :to-equal 91.5)
      (expect (plist-get (car rows) :final) :to-equal 88.0)
      (expect (plist-get (car rows) :missing) :to-equal 0)
      (expect (plist-get (car rows) :late) :to-equal 2)
      (expect (plist-get (cadr rows) :missing) :to-equal 3)))

  (it "takes the posted scores when unposted is off and nil when Canvas has none"
    (let* ((e (test-gradebook--enrollment 1 "A" 10 80.0 75.0
                                          '(grades . ((current_score . 80.0) (final_score . 75.0)
                                                      (unposted_current_score . 85.0)
                                                      (unposted_final_score . :null)))))
           (posted (let ((org-canvas-gradebook-unposted nil))
                     (car (org-canvas--gradebook-rows (list e) nil))))
           (unposted (let ((org-canvas-gradebook-unposted t))
                       (car (org-canvas--gradebook-rows (list e) nil)))))
      (expect (plist-get posted :current) :to-equal 80.0)
      (expect (plist-get posted :final) :to-equal 75.0)
      (expect (plist-get unposted :current) :to-equal 85.0)
      (expect (plist-get unposted :final) :to-be nil)))

  (it "keeps the latest activity, leaves the counts nil when refused, and sorts by name"
    (let ((rows (org-canvas--gradebook-rows
                 (list (test-gradebook--enrollment 2 "Zed, Zoe" 10 50.0 50.0
                                                   '(last_activity_at . "2026-09-01T10:00:00Z"))
                       (test-gradebook--enrollment 2 "Zed, Zoe" 20 50.0 50.0
                                                   '(last_activity_at . "2026-09-05T10:00:00Z"))
                       `((user_id . 7) (course_section_id . 10) (user . ((id . 7)))
                         (grades . ((current_score . :null) (final_score . :null)))))
                 'refused)))
      (expect (mapcar (lambda (r) (plist-get r :name)) rows) :to-equal '("User 7" "Zed, Zoe"))
      (expect (plist-get (cadr rows) :last-activity) :to-equal "2026-09-05T10:00:00Z")
      (expect (plist-get (cadr rows) :missing) :to-be nil)
      (expect (plist-get (car rows) :current) :to-be nil))))

(describe "org-canvas--gradebook-number"
  (it "renders a score with one decimal, a count as an integer, and a dash for nothing"
    (expect (org-canvas--gradebook-number 91.25) :to-equal "91.2")
    (expect (org-canvas--gradebook-number 3) :to-equal "3")
    (expect (org-canvas--gradebook-number nil) :to-equal "-")
    (expect (org-canvas--gradebook-number "A") :to-equal "A")))

(describe "org-canvas--gradebook-cell-text"
  (it "collapses pipes and line breaks with the blanks around them, and blanks a non-string"
    (expect (org-canvas--gradebook-cell-text "Sees me | Tue\nafternoons\r\n  late ") :to-equal "Sees me Tue afternoons late")
    (expect (org-canvas--gradebook-cell-text "plain") :to-equal "plain")
    (expect (org-canvas--gradebook-cell-text nil) :to-equal "")
    (expect (org-canvas--gradebook-cell-text :null) :to-equal "")))

(describe "org-canvas--gradebook-stat"
  (it "answers a number and nothing for a null however it was decoded"
    (expect (org-canvas--gradebook-stat '((median . 7.5)) 'median) :to-equal 7.5)
    (expect (org-canvas--gradebook-stat '((median . :null)) 'median) :to-be nil)
    (expect (org-canvas--gradebook-stat '((median . nil)) 'median) :to-be nil)
    (expect (org-canvas--gradebook-stat nil 'median) :to-be nil)))

(describe "org-canvas--gradebook-assignment-cell"
  (it "falls back to the id when the analytics row has no title and no file to link"
    (let ((org-canvas-assignments-file nil))
      (expect (org-canvas--gradebook-assignment-cell '((assignment_id . 77) (title . nil)))
              :to-equal "Assignment 77")
      (expect (org-canvas--gradebook-assignment-cell '((assignment_id . 78) (title . "Essay")))
              :to-equal "Essay"))))

(describe "org-canvas--gradebook-mean"
  (it "averages the numbers and ignores the blanks"
    (expect (org-canvas--gradebook-mean '(90.0 nil 70.0)) :to-equal 80.0)
    (expect (org-canvas--gradebook-mean '(nil)) :to-be nil)))

(describe "org-canvas-pull-gradebook"
  (it "writes the students table sorted by name and the sections table with means"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.25
                                          '(last_activity_at . "2026-09-05T10:00:00Z"))
              (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 1 "Adams, Alice" 20 91.5 88.0)
              (test-gradebook--enrollment 3 "Cruz, Cal" 20 :null :null))
        '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Recitation")))
        (list (test-gradebook--summary 1 0 2) (test-gradebook--summary 2 3 1))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Students\n")
        (expect text :to-match "^\\* Sections\n")
        (expect (string-match "\\* Students" text) :to-be-less-than (string-match "\\* Sections" text))
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +|$")
        (expect (string-match "Adams, Alice" text) :to-be-less-than (string-match "Beta, Bob" text))
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture, Recitation" "91.5" "88.0" "-" "0" "2" "-"))
        (expect (nthcdr 1 (test-gradebook--row "Beta, Bob"))
                :to-equal '("70.0" "65.2" "-" "3" "1" "<2026-09-05 Sat 10:00>"))
        (expect (test-gradebook--row "Cruz, Cal") :to-equal '("Recitation" "-" "-" "-" "-" "-" "-"))
        (expect text :to-match "^| Section +| Students +| Mean current +| Mean final +| Missing +|$")
        (expect (test-gradebook--row "Lecture") :to-equal '("2" "80.8" "76.6" "3"))
        (expect (test-gradebook--row "Recitation") :to-equal '("2" "91.5" "88.0" "0"))
        (expect text :not :to-match "user_id\\|:PROPERTIES:")
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

  (it "keeps gradebook.org out of git when the course directory is a work tree (issue #549)"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      ;; rev-parse answers 0 (a work tree); ls-files and check-ignore
      ;; answer 1 (not tracked, not ignored), so the pull adds the name.
      (cl-letf (((symbol-function 'process-file)
                 (lambda (_program _in _out _display &rest args)
                   (if (equal (car args) "rev-parse") 0 1))))
        (org-canvas-pull-gradebook))
      (expect (with-temp-buffer
                (insert-file-contents (expand-file-name ".gitignore" dir))
                (buffer-string))
              :to-equal "gradebook.org\n")))

  (it "links a student to the roster heading when people.org holds the user id"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (with-temp-file org-canvas-people-file
        (insert "* Students\n** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"))
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file)
              :to-match "^| \\[\\[file:people.org::\\*Adams, Alice\\]\\[Adams, Alice\\]\\] +|")))

  (it "leaves Missing and Late blank with a note when the analytics are refused"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        'refuse
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (car warned) :to-match "Could not read the student summaries"))
      (let ((text (test-gradebook--file)))
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "" "" "-"))
        (expect (test-gradebook--row "Lecture") :to-equal '("1" "91.5" "88.0" ""))
        (expect text :to-match "^Missing and Late are blank: Canvas refused"))))

  (it "rewrites the tables on a re-pull and keeps nothing written by hand"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (with-temp-buffer
        (insert-file-contents org-canvas-gradebook-file)
        (goto-char (point-max))
        (insert "A note I wrote under Sections\n")
        (write-region (point-min) (point-max) org-canvas-gradebook-file))
      (setq test-gradebook--enrollments
            (list (test-gradebook--enrollment 1 "Adams, Alice" 10 95.0 95.0)))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "95.0" "95.0" "-" "-" "-" "-"))
        (expect text :not :to-match "Beta, Bob")
        (expect text :not :to-match "A note I wrote")
        (expect (length (split-string text "^\\* Students" t)) :to-equal 2))))

  (it "adds one column per custom column after Last activity, in position order"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--columns
            (list (test-gradebook--column 5 "Notes" 2 '(1 . "Sees me | Tue\nafternoons"))
                  (test-gradebook--column 7 "Advisor" 1 '(1 . "Dr. Chen") '(2 . ""))))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +| Advisor +| Notes +|$")
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-" "Dr. Chen" "Sees me Tue afternoons"))
        (expect (test-gradebook--row "Beta, Bob")
                :to-equal '("Lecture" "70.0" "65.0" "-" "-" "-" "-" "" ""))
        ;; The separator row has one more cell per custom column.
        (expect text :to-match "^|-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+\\+-+|$")
        (expect (test-gradebook--row "Lecture") :to-equal '("2" "80.8" "76.5" "0"))
        (expect test-gradebook--columns-params :to-be nil))))

  (it "asks for the hidden columns only when told to"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (let ((org-canvas-gradebook-include-hidden-columns t))
        (org-canvas-pull-gradebook))
      (expect test-gradebook--columns-params :to-equal '(("include_hidden" . "true")))))

  (it "leaves the custom columns out with one warning when Canvas refuses them"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--columns 'refuse)
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (length warned) :to-equal 1)
        (expect (car warned) :to-match "Could not read the custom gradebook columns"))
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +|$")
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-"))
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

  (it "writes the assignments table in Canvas order with the spread, links and dashes"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (with-temp-file org-canvas-assignments-file
        (insert "* Homework 1\n:PROPERTIES:\n:CANVAS_ID: 501\n:END:\n"))
      (setq test-gradebook--assignments
            (list (test-gradebook--assignment 502 "Homework 2" "2026-09-20T23:59:00Z" 10.0)
                  (test-gradebook--assignment 501 "Homework 1" "2026-09-06T23:59:00Z" 20.0
                                              '(min_score . 4.0) '(first_quartile . 12.5)
                                              '(median . 16.0) '(third_quartile . 18.0)
                                              '(max_score . 20.0) '(missing . 3) '(late . 2))))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Assignments\n")
        (expect (string-match "\\* Sections" text) :to-be-less-than (string-match "\\* Assignments" text))
        (expect text :to-match "^| Assignment +| Due +| Points +| Min +| Q1 +| Median +| Q3 +| Max +| Missing +| Late +|$")
        (expect (string-match "Homework 2" text) :to-be-less-than (string-match "Homework 1" text))
        (expect text :to-match "^| \\[\\[file:assignments.org::\\*Homework 1\\]\\[Homework 1\\]\\] +|")
        (expect (test-gradebook--row "Homework 1")
                :to-equal '("<2026-09-06 Sun 23:59>" "20.0" "4.0" "12.5" "16.0" "18.0" "20.0" "3" "2"))
        (expect (test-gradebook--row "Homework 2")
                :to-equal '("<2026-09-20 Sun 23:59>" "10.0" "-" "-" "-" "-" "-" "0" "0"))
        (expect text :not :to-match "Homework 2\\]\\]"))))

  (it "leaves a note under Assignments with one warning when Canvas refuses the analytics"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--assignments 'refuse)
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (length warned) :to-equal 1)
        (expect (car warned) :to-match "Could not read the assignment analytics"))
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Assignments\n+Canvas refused the assignment analytics")
        (expect text :not :to-match "^| Assignment ")
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-"))
        (expect text :to-match "^#\\+LAST_SYNCED:"))))

  (it "writes a No assignments line when the course has none"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :to-match "^\\* Assignments\n+No assignments\n")))

  (it "writes the empty-file note for a course with no students"
    (test-gradebook--with-course nil nil nil
      (expect (org-canvas-pull-gradebook) :to-be nil)
      (expect (test-gradebook--file) :to-match "Canvas returned 0 items")
      (expect (org-canvas-gradebook-summary-text nil) :to-equal
              "Course: 0 students; current mean -, median -; final mean -, median -\n")))

  (it "returns the student rows, summed up with the Sections table (issue #451)"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0)
              (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--enrollment 3 "Cruz, Cal" 20 80.0 :null))
        '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Recitation")))
        nil
      (let* ((rows (org-canvas-pull-gradebook))
             (text (org-canvas-gradebook-summary-text rows)))
        (expect (mapcar (lambda (r) (plist-get r :name)) rows)
                :to-equal '("Adams, Alice" "Beta, Bob" "Cruz, Cal"))
        (expect text :to-match "\\`| Section +| Students +| Mean current")
        (expect text :to-match "^| Lecture +| +2 | +80.8 |")
        (expect text :to-match "^| Recitation +| +1 | +80.0 | -")
        (expect text :not :to-match "Student +| Sections\\|Assignment")
        (expect text :to-match
                "|\n\nCourse: 3 students; current mean 80.5, median 80.0; final mean 76.5, median 76.5\n\\'"))))

  (it "sums up without a table when gradebook.org has no Sections heading"
    (test-gradebook--with-course nil nil nil
      (with-temp-file org-canvas-gradebook-file (insert "* Students\n"))
      (expect (org-canvas-gradebook-summary-text (list (list :current 90 :final 85)))
              :to-equal
              "Course: 1 student; current mean 90.0, median 90.0; final mean 85.0, median 85.0\n")
      (with-temp-file org-canvas-gradebook-file (insert "* Students\n* Sections\n| Lecture | 1 |\n"))
      (expect (org-canvas--gradebook-sections-table) :to-equal "| Lecture | 1 |")))

  (it "is in the pull tiers after people"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-people names)
              :to-be-less-than (cl-position 'org-canvas-pull-gradebook names)))))

;;;; Final grade overrides (issue #556)

(defun test-gradebook--write-people (dir)
  "Write people.org with Adams, Beta and Doe in DIR; return its path."
  (let ((file (expand-file-name "people.org" dir)))
    (with-temp-file file
      (insert "* Students\n"
              "** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"
              "** Beta, Bob\n:PROPERTIES:\n:USER_ID: 2\n:END:\n"
              "** Doe, Dee\n:PROPERTIES:\n:USER_ID: 4\n:END:\n"))
    file))

(defun test-gradebook--overrides-table ()
  "Return the final grade overrides table's text, or nil without one."
  (let ((text (test-gradebook--file)))
    (when (string-match
           "^#\\+NAME: final-grade-overrides\n\\(\\(?:.*\n\\)*?\\)\n" text)
      (match-string 1 text))))

(defun test-gradebook--write-overrides-table (rows)
  "Write gradebook.org's overrides table with ROWS, (NAME . CELL) pairs."
  (with-temp-file org-canvas-gradebook-file
    (insert "* Final grade overrides\n"
            "#+NAME: final-grade-overrides\n"
            "| Student | Override |\n"
            "|---+---|\n")
    (dolist (row rows)
      (insert (format "| %s | %s |\n" (car row) (cdr row))))))

(describe "the Students table's override column"
  (it "shows the override the enrollment carries beside Final, one per student"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.25)
              (test-gradebook--enrollment 1 "Adams, Alice" 20 91.5 88.0)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Recitation")))
        nil
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text
                :to-match "^| Student +| Sections +| Current +| Final +| Override +| Missing +| Late +| Last activity +|$")
        (expect (test-gradebook--row "Adams, Alice")
                :to-equal '("Lecture, Recitation" "91.5" "88.0" "93.2" "-" "-" "-"))
        (expect (test-gradebook--row "Beta, Bob")
                :to-equal '("Lecture" "70.0" "65.0" "-" "-" "-" "-"))))))

(describe "the final grade overrides table"
  (it "writes one row per student Canvas holds an override for"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file))
            (table (test-gradebook--overrides-table)))
        (expect text :to-match "^\\* Final grade overrides\n")
        (expect table :to-match "^| Student +| Override +|$")
        (expect table :to-match "^| Adams, Alice +| +93.5 |$")
        (expect table :not :to-match "Beta"))))

  (it "falls back to the user id when people.org lacks the student"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 5 "Nobody, Noe" 10 50.0 50.0) 70.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--overrides-table) :to-match "^| #5 +| +70.0 |$")))

  (it "says none when no student has one, replacing a stale table"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5))
        '(((id . 10) (name . "Lecture")))
        nil
      (org-canvas-pull-gradebook)
      (setq test-gradebook--enrollments
            (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)))
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^\\* Final grade overrides\n")
        (expect text :to-match "No final grade overrides")
        (expect text :not :to-match "#\\+NAME: final-grade-overrides")
        (expect text :not :to-match "Adams, Alice +| 93"))))

  (it "a student's pull removes the heading a teacher pull left"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5))
        '(((id . 10) (name . "Lecture")))
        nil
      (setq test-gradebook--course
            (test-gradebook--self-course (test-gradebook--student-enrollment)))
      (setq test-gradebook--submissions (test-gradebook--self-subs))
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :not :to-match "Final grade overrides"))))

(describe "org-canvas-sync-final-grade-overrides"
  (it "sends one mutation per row that differs and one clear per missing row"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5)
              (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0)
              (test-gradebook--with-override
               (test-gradebook--enrollment 3 "Cruz, Cal" 10 88.0 88.0) 88.0)
              (test-gradebook--enrollment 4 "Doe, Dee" 10 60.0 60.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table
       '(("Adams, Alice" . "93.5") ("Beta, Bob" . "") ("#4" . "70")))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (what doc vars)
                     (push (list what doc vars) sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        ;; A row matching Canvas, and one whose blank says no override,
        ;; are sent nothing; Cruz on Canvas with no row is cleared.
        (expect (length sent) :to-equal 2)
        (let* ((clear (cl-find-if (lambda (call)
                                    (null (alist-get 'overrideScore (nth 2 call))))
                                  sent))
               (set (cl-find-if (lambda (call)
                                   (equal (alist-get 'overrideScore (nth 2 call)) 70))
                                 sent)))
          (expect (alist-get 'enrollmentId (nth 2 clear)) :to-equal "1103")
          (expect (car clear) :to-match "clear Cruz, Cal")
          (expect (alist-get 'enrollmentId (nth 2 set)) :to-equal "1104")
          (expect (car set) :to-match "set Doe, Dee")
          (expect (nth 1 set) :to-match "setOverrideScore")))))

  (it "runs the mutation for real, reading back what Canvas stored"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0)
              (test-gradebook--with-override
               (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0) 70.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((said nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-send)
                   (lambda (&rest _)
                     '((setOverrideScore
                        . ((grades . ((overrideScore . 93.5))))))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-sync-final-grade-overrides))
        ;; Adams's row is sent, Beta's Canvas override is cleared, and
        ;; both reads come back through the reply: two successes.
        (expect (car said) :to-match "1 set, 1 cleared, 0 failed"))))

  (it "counts a payload that answers errors as a failed request"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((said nil) (errored nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-send)
                   (lambda (&rest _)
                     '((setOverrideScore
                        . ((errors
                            . (((attribute . "enrollment")
                                (message . "has no score")))))))))
                   ((symbol-function 'org-canvas--log-error)
                   (lambda (_ fmt &rest args) (push (apply #'format fmt args) errored)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-sync-final-grade-overrides))
        (expect (car said) :to-match "0 set, 0 cleared, 1 failed")
        (expect (car errored) :to-match "setOverrideScore: has no score"))))

  (it "reads the Override column by its title, falling back to position 1"
    (expect (org-canvas--gradebook-overrides-column '("Student" "Override")) :to-equal 1)
    (expect (org-canvas--gradebook-overrides-column '("Student" "Grade")) :to-equal 1)
    (expect (org-canvas--gradebook-overrides-column '("Override" "Student")) :to-equal 0))

  (it "sends nothing when the table says what Canvas already holds"
    (test-gradebook--with-course
        (list (test-gradebook--with-override
               (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0) 93.5))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        (expect sent :to-be nil))))

  (it "skips a row that names nobody, and counts roster gaps and failed requests"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table
       '(("Adams, Alice" . "93.5") ("Ghost, Gus" . "70") ("#9" . "80")))
      (let ((said nil) (warned nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (signal 'org-canvas-api-error (list "403"))))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_ fmt &rest args) (push (apply #'format fmt args) warned)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
          (org-canvas-sync-final-grade-overrides))
        (expect (car said) :to-match "0 set, 0 cleared, 2 failed")
        (expect (car warned) :to-match "Could not resolve student 'Ghost, Gus'"))))

  (it "refuses an override cell that is not a number, before anything is sent"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "high")))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (expect (org-canvas-sync-final-grade-overrides)
                  :to-throw 'org-canvas-config-error))
        (expect sent :to-be nil))))

  (it "sends nothing under the dry run, and says what it would"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((sent nil) (lines nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-send)
                   (lambda (&rest _) (push 'sent t) nil))
                  ((symbol-function 'org-canvas--log-info)
                   (lambda (_ fmt &rest args) (push (apply #'format fmt args) lines))))
          (let ((org-canvas--dry-run t))
            (org-canvas-sync-final-grade-overrides)))
        (expect sent :to-be nil)
        (expect (cl-find-if (lambda (l) (string-match-p "\\[DRY-RUN\\].*Adams, Alice" l))
                            lines)
                :to-be-truthy))))

  (it "refuses a token whose allowlist does not name submissions, and allows one that does"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (expect (org-canvas-sync-final-grade-overrides)
                :to-throw 'org-canvas-write-scope-error))
      (let ((sent nil) (org-canvas-write-allowlist '("submissions")))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        (expect sent :to-be-truthy))))

  (it "aborts without sending when the enrollments read fails"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (test-gradebook--write-people dir)
      (test-gradebook--write-overrides-table '(("Adams, Alice" . "93.5")))
      (setq test-gradebook--enrollments 'refuse)
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (expect (org-canvas-sync-final-grade-overrides) :to-throw))
        (expect sent :to-be nil))))

  (it "does nothing without a table, and refuses without the file"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (expect (org-canvas-sync-final-grade-overrides)
              :to-throw 'org-canvas-config-error)
      (with-temp-file org-canvas-gradebook-file
        (insert "* Final grade overrides\nNo final grade overrides.\n"))
      (let ((sent nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (&rest _) (push 'called sent) nil)))
          (org-canvas-sync-final-grade-overrides))
        (expect sent :to-be nil)))))

;;;; My grades: a student's enrolment (issue #488)

(defun test-gradebook--self-course (&rest enrollments)
  "A course whose caller holds ENROLLMENTS, as Canvas answers a vector."
  `((id . 1) (apply_assignment_group_weights . :json-false)
    (enrollments . ,(vconcat enrollments))))

(defun test-gradebook--student-enrollment ()
  "The caller's student enrolment, as total_scores adds its totals."
  '((type . "student") (role . "StudentEnrollment") (user_id . 7)
    (enrollment_state . "active")
    (computed_current_score . 87.5) (computed_current_grade . "B+")
    (computed_final_score . 60.25) (computed_final_grade . :null)))

(defun test-gradebook--self-sub (aid name due points &rest extra)
  "The caller's submission to assignment AID, NAME, due DUE, out of POINTS.
EXTRA pairs come first, so an explicit value shadows the default."
  (append extra
          `((id . ,(+ 500 aid)) (assignment_id . ,aid) (user_id . 7)
            (score . :null) (grade . :null) (workflow_state . "unsubmitted")
            (late . :json-false) (missing . :json-false) (excused . :json-false)
            (submission_comments . [])
            (assignment . ((id . ,aid) (name . ,name) (due_at . ,due)
                           (points_possible . ,points))))))

(defun test-gradebook--self-subs ()
  "Three submissions, out of due order, one undated, one with comments."
  (vector
   (test-gradebook--self-sub 3 "Reading log" :null 5)
   (test-gradebook--self-sub 2 "Homework 2" "2026-09-20T03:59:00Z" 10
                             '(missing . t) '(late . t))
   (test-gradebook--self-sub
    1 "Homework 1" "2026-09-06T03:59:00Z" 10
    '(score . 8.5) '(grade . "8.5") '(workflow_state . "graded")
    '(submission_comments
      . [((id . 1) (author_name . "Pat Teacher") (created_at . "2026-09-08T14:00:00Z")
          (comment . "Good work.\nShow the units | next time."))]))))

(defun test-gradebook--self-row (name)
  "Return the My grades table row of the assignment NAME, cells trimmed."
  (let ((text (test-gradebook--file)))
    (when (string-match (format "^| *%s *|\\(.*\\)$" (regexp-quote name)) text)
      (mapcar #'string-trim (split-string (match-string 1 text) "|" t)))))

(describe "org-canvas--gradebook-student-enrollment"
  (it "finds the caller's student enrolment"
    (let ((e (test-gradebook--student-enrollment)))
      (expect (org-canvas--gradebook-student-enrollment (test-gradebook--self-course e))
              :to-equal e)))

  (it "answers nil for a teacher, a TA, a designer or a teacher who is also a student"
    (dolist (type '("teacher" "ta" "designer"))
      (expect (org-canvas--gradebook-student-enrollment
               (test-gradebook--self-course `((type . ,type))))
              :to-be nil))
    (expect (org-canvas--gradebook-student-enrollment
             (test-gradebook--self-course (test-gradebook--student-enrollment)
                                          '((type . "teacher"))))
            :to-be nil))

  (it "answers nil without a course, or with enrolments null"
    (expect (org-canvas--gradebook-student-enrollment nil) :to-be nil)
    (expect (org-canvas--gradebook-student-enrollment '((enrollments . :null))) :to-be nil)
    (expect (org-canvas--gradebook-student-enrollment
             (test-gradebook--self-course '((type . "observer"))))
            :to-be nil)))

(describe "org-canvas-pull-gradebook for a student (issue #488)"
  (it "writes My grades by due date, the totals and the comments, and no roster"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        'refuse
      (with-temp-file org-canvas-gradebook-file
        (insert "* Students\n| Beta, Bob |\n* Sections\n* Assignments\n"
                "Canvas refused the assignment analytics for this token.\n"))
      (let* ((test-gradebook--course (test-gradebook--self-course
                                      (test-gradebook--student-enrollment)))
             (test-gradebook--submissions (test-gradebook--self-subs))
             (urls nil)
             (api (symbol-function 'org-canvas-api-request-all-pages))
             (rows (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                              (lambda (method url &optional params)
                                (push url urls)
                                (funcall api method url params))))
                     (org-canvas-pull-gradebook)))
             (text (test-gradebook--file))
             (row (car rows)))
        (expect (length urls) :to-equal 1)
        (expect (car urls) :to-match "/students/submissions\\'")
        (expect test-gradebook--submissions-params :to-equal
                '(("include[]" . "assignment") ("include[]" . "submission_comments")))
        (expect test-gradebook--course-reads :to-equal '((("include[]" . "total_scores"))))
        (expect text :not :to-match "^\\* \\(Students\\|Sections\\|Assignments\\|Groups\\)")
        (expect text :not :to-match "Beta, Bob\\|refused")
        (expect text :to-match "^\\* My grades\n")
        (expect text :to-match
                "^| Assignment +| Due +| Points +| Score +| Grade +| Status +| Late +| Missing +|$")
        (expect (< (string-match "Homework 1" text) (string-match "Homework 2" text)) :to-be t)
        (expect (< (string-match "| Homework 2" text) (string-match "| Reading log" text)) :to-be t)
        (expect (test-gradebook--self-row "Homework 1")
                :to-equal (list (org-canvas--iso8601-to-org-timestamp "2026-09-06T03:59:00Z")
                                "10" "8.5" "8.5" "graded" "no" "no"))
        (expect (nthcdr 2 (test-gradebook--self-row "Homework 2"))
                :to-equal '("-" "-" "unsubmitted" "yes" "yes"))
        (expect (test-gradebook--self-row "Reading log")
                :to-equal '("-" "5" "-" "-" "unsubmitted" "no" "no"))
        (expect text :to-match "^Current: 87.5 (B\\+); final: 60.2$")
        (expect text :to-match
                "^Comments:\n- Homework 1, Pat Teacher <2026-09-0[89] [A-Za-z]+ [0-9:]+>: Good work. Show the units next time.$")
        (expect text :to-match "^#\\+LAST_SYNCED:")
        (expect (plist-get row :self) :to-be t)
        (expect (plist-get row :user-id) :to-equal 7)
        (expect (plist-get row :current) :to-equal 87.5)
        (expect (plist-get row :final) :to-equal 60.25)
        (expect (plist-get row :assignments) :to-equal 3)
        (expect (plist-get row :missing) :to-equal 1)
        (let ((summary (org-canvas-gradebook-summary-text rows)))
          (expect summary :to-match "\\`| Assignment +| Due")
          (expect summary :to-match "^Current: 87.5 (B\\+); final: 60.2\n")
          (expect summary :to-match
                  "\n\nMe: 3 assignments, 1 missing; current 87.5; final 60.2\n\\'")))))

  (it "keeps gradebook.org out of git for a student's own grades too (issue #549)"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 2 "Beta, Bob" 10 70.0 65.0))
        '(((id . 10) (name . "Lecture")))
        'refuse
      (let ((test-gradebook--course (test-gradebook--self-course
                                      (test-gradebook--student-enrollment)))
            (test-gradebook--submissions (test-gradebook--self-subs)))
        ;; rev-parse answers 0 (a work tree); ls-files and check-ignore
        ;; answer 1 (not tracked, not ignored), so the pull adds the name.
        (cl-letf (((symbol-function 'process-file)
                   (lambda (_program _in _out _display &rest args)
                     (if (equal (car args) "rev-parse") 0 1))))
          (org-canvas-pull-gradebook)))
      (expect (with-temp-buffer
                (insert-file-contents (expand-file-name ".gitignore" dir))
                (buffer-string))
              :to-equal "gradebook.org\n")))

  (it "marks an excused submission and writes No assignments for none"
    (test-gradebook--with-course nil nil nil
      (let ((test-gradebook--course (test-gradebook--self-course
                                     '((type . "student") (user_id . 7))))
            (test-gradebook--submissions
             (vector (test-gradebook--self-sub 2 "Survey" :null 0)
                     (test-gradebook--self-sub 1 "Quiz" :null 1 '(excused . t)))))
        (org-canvas-pull-gradebook)
        ;; Both undated: by name.
        (expect (< (string-match "| Quiz" (test-gradebook--file))
                   (string-match "| Survey" (test-gradebook--file)))
                :to-be t)
        (expect (nthcdr 4 (test-gradebook--self-row "Quiz")) :to-equal '("excused" "no" "no"))
        (expect (test-gradebook--file) :to-match "^Current: -; final: -$")
        (expect (test-gradebook--file) :not :to-match "Comments:"))
      (let* ((test-gradebook--course (test-gradebook--self-course
                                      '((type . "student") (user_id . 7))))
             (test-gradebook--submissions [])
             (rows (org-canvas-pull-gradebook)))
        (expect (test-gradebook--file) :to-match "^\\* My grades\n+No assignments\n")
        (expect (org-canvas-gradebook-summary-text rows) :to-match
                "Me: 0 assignments, 0 missing; current -; final -\n\\'"))))

  (it "prints the student's standing from the shell's gradebook command"
    (require 'org-canvas-batch)
    (test-gradebook--with-course nil nil nil
      (let ((test-gradebook--course (test-gradebook--self-course
                                     (test-gradebook--student-enrollment)))
            (test-gradebook--submissions (test-gradebook--self-subs))
            (status nil))
        (let ((out (with-output-to-string
                     (setq status (org-canvas-batch--cmd-gradebook nil)))))
          (expect status :to-equal 0)
          (expect out :to-match "| Homework 1 ")
          (expect out :to-match "Me: 3 assignments, 1 missing; current 87.5; final 60.2\n\\'")))))

  (it "writes the teacher's tables, without My grades, when the course read is refused"
    (test-gradebook--with-course
        (list (test-gradebook--enrollment 1 "Adams, Alice" 10 91.5 88.0))
        '(((id . 10) (name . "Lecture")))
        nil
      (with-temp-file org-canvas-gradebook-file
        (insert "* My grades\n| Homework 1 |\n"))
      (let ((test-gradebook--course 'refuse)
            (warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-gradebook))
        (expect (cl-some (lambda (w) (string-match-p
                                      "Could not read the course (.*403 Forbidden.*); writing the teacher's tables"
                                      w))
                         warned)
                :to-be-truthy)
        (expect (test-gradebook--row "Adams, Alice") :to-equal '("Lecture" "91.5" "88.0" "-" "-" "-" "-"))
        (expect (test-gradebook--file) :not :to-match "My grades\\|Homework 1")))))

(provide 'org-canvas-gradebook-test)
;;; org-canvas-gradebook-test.el ends here
