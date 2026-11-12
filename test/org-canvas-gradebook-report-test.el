;;; org-canvas-gradebook-report-test.el --- Buttercup tests for the standing and the what-if  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-gradebook-report': one student's standing
;; (issue #542), its still-open list and its ceiling, and the what-if
;; over assignment-groups.org (#452).  Every request is answered by the
;; gradebook fake in test-fixtures.el; nothing here reaches the network
;; (Hard Rule 2), and the log is never read from the shared buffer
;; (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defconst test-gradebook--local-groups
  "* Assignment Groups
** Quizzes
:PROPERTIES:
:CANVAS_ID: 1
:WEIGHT: 50
:END:
** Essays
:PROPERTIES:
:WEIGHT: 50
:END:
** Labs
:PROPERTIES:
:WEIGHT: 0
:END:
"
  "assignment-groups.org with the Quizzes drop rule removed and an unpushed Labs.")

(defun test-gradebook--what-if ()
  "Run the what-if and return its report, runs of blanks collapsed."
  (cl-letf (((symbol-function 'princ) #'ignore))
    (replace-regexp-in-string " +" " " (org-canvas-gradebook-what-if))))

(describe "org-canvas-gradebook-what-if"
  (it "recomputes every total under the local table and reports the letter moves"
    (test-gradebook--with-scores 40.0
      (org-canvas-pull-gradebook)
      (with-temp-file org-canvas-assignment-groups-file (insert test-gradebook--local-groups))
      (let ((report (test-gradebook--what-if)))
        (expect report :to-match "weights on. Nothing was sent to Canvas")
        (expect report :to-match "- Labs is not on Canvas as pulled")
        (expect report :to-match "^| Table | Mean | Median | A | B | F |$")
        (expect report :to-match "^| Canvas now | 63.3 | 63.3 | 1 | 0 | 1 |$")
        (expect report :to-match "^| Canvas's table, recomputed | 68.3 | 68.3 | 1 | 0 | 1 |$")
        (expect report :to-match "^| assignment-groups.org | 55.4 | 55.4 | 0 | 1 | 1 |$")
        (expect report :to-match "^| Quizzes | 50 -> 50 | 1 -> 0 | 0 -> 0 |$")
        (expect report :not :to-match "^| Essays |")
        (expect report :to-match "^| Adams, Alice | 86.7 | 77.5 | A | B |$")
        (expect report :not :to-match "^| Beta, Bob |"))))

  (it "reads unsaved edits and the weighting from settings.org"
    (test-gradebook--with-scores 40.0
      (org-canvas-pull-gradebook)
      (with-temp-file org-canvas-assignment-groups-file (insert test-gradebook--local-groups))
      (with-temp-file org-canvas-settings-file
        (insert "* Settings\n:PROPERTIES:\n:APPLY_WEIGHTS: false\n:END:\n"))
      (with-current-buffer (find-file-noselect org-canvas-assignment-groups-file)
        (goto-char (point-min))
        (re-search-forward ":CANVAS_ID: 1\n")
        (insert ":DROP_LOWEST: 1\n"))
      (let ((report (test-gradebook--what-if)))
        ;; Unweighted, Adams keeps 28/30 and 80/100: 83.1, a B.
        (expect report :to-match "weights off")
        (expect report :to-match "No group's weight or drop rules differ from Canvas's")
        (expect report :to-match "^| Adams, Alice | 86.7 | 83.1 | A | B |$"))))

  (it "keeps Canvas's table for a group the file lacks and says when there are no letters"
    (let* ((group '(:id 1 :name "Quizzes" :weight 50 :drop-lowest 1))
           (merged (org-canvas--gradebook-what-if-groups (list group) nil)))
      (expect (car merged) :to-equal (list group))
      (expect (cadr merged) :to-match "Quizzes is not in assignment-groups.org"))
    (expect (with-temp-buffer
              (org-canvas--gradebook-insert-letter-changes '(("A" 80 80 70)) nil)
              (buffer-string))
            :to-match "no grading scheme"))

  (it "says so when no letter moves, and counts one missing total as a difference"
    (expect (with-temp-buffer
              (org-canvas--gradebook-insert-letter-changes '(("A" 80 80 81)) '(("A" . 50.0)))
              (buffer-string))
            :to-match "No student's letter changes")
    (expect (org-canvas--gradebook-differs-p nil 40.0) :to-be-truthy)
    (expect (org-canvas--gradebook-differs-p nil nil) :not :to-be-truthy)
    (expect (org-canvas--gradebook-differs-p 40.05 40.0) :not :to-be-truthy))

  (it "refuses without pulled scores or without assignment-groups.org"
    (test-gradebook--with-scores 40.0
      (expect (org-canvas-gradebook-what-if) :to-throw 'user-error)
      (org-canvas-pull-gradebook)
      (expect (org-canvas-gradebook-what-if) :to-throw 'user-error))))

;;;; One student's standing (#542)

(defun test-gradebook--standing-groups ()
  "Quizzes (50%, drop lowest 1) and Essays (50%), for the standing fixtures.
Q3 sits in Quizzes, E2 is omitted from the final grade."
  `(((id . 2) (name . "Essays") (position . 2) (group_weight . 50) (rules . nil)
     (assignments . [((id . 21) (published . t) (points_possible . 100))
                     ((id . 22) (published . t) (omit_from_final_grade . t)
                      (points_possible . 100))]))
    ((id . 1) (name . "Quizzes") (position . 1) (group_weight . 50)
     (rules . ((drop_lowest . 1)))
     (assignments . [((id . 11) (published . t) (points_possible . 10))
                     ((id . 12) (published . t) (points_possible . 10))
                     ((id . 13) (published . t) (points_possible . 20))]))))

(defun test-gradebook--standing-sub (aid name group due lock points &rest extra)
  "Adams's submission to assignment AID, NAME, in group GROUP, out of POINTS.
DUE and LOCK are ISO8601 strings, or :null for no date; EXTRA pairs
come first, so an explicit value shadows the default.
Positional is right: the five are the fields a case varies and each
call reads as the submission it describes; a bundle would rename them
and nothing else (issue #599)."
  (append extra
          `((user_id . 1) (assignment_id . ,aid) (score . :null)
            (grade . :null) (workflow_state . "unsubmitted")
            (late . :json-false) (missing . :json-false) (excused . :json-false)
            (posted_at . :null)
            (assignment . ((id . ,aid) (name . ,name)
                           (assignment_group_id . ,group)
                           (published . t) (points_possible . ,points)
                           (due_at . ,due) (lock_at . ,lock))))))

(defun test-gradebook--standing-subs ()
  "Adams's submissions, by due date: Q1 graded 4/10 and closed, Q2 open
past its due date, Q3 closed and ungraded, E1 graded 80/100 with no
lock, and E2, which the final grade omits."
  (vector
   (test-gradebook--standing-sub 11 "Q1" 1 "2026-09-01T03:59:00Z" "2026-09-08T03:59:00Z" 10
                                 '(score . 4.0) '(workflow_state . "graded")
                                 '(posted_at . "2026-09-02T00:00:00Z"))
   (test-gradebook--standing-sub 12 "Q2" 1 "2026-09-15T03:59:00Z" "2030-01-01T00:00:00Z" 10)
   (test-gradebook--standing-sub 13 "Q3" 1 "2026-09-20T03:59:00Z" "2026-09-27T03:59:00Z" 20
                                 '(missing . t))
   (test-gradebook--standing-sub 21 "E1" 2 "2026-10-01T03:59:00Z" :null 100
                                 '(score . 80.0) '(workflow_state . "graded")
                                 '(posted_at . "2026-10-02T00:00:00Z"))
   (test-gradebook--standing-sub 22 "E2" 2 "2026-10-05T03:59:00Z" :null 100)))

(defmacro test-gradebook--with-standing (&rest body)
  "Run BODY with the fake API serving Adams's standing (issue #542).
Quizzes (50%, drop lowest 1) holds Q1 (4/10, graded, closed), Q2 (10
points, still open past its due date) and Q3 (20 points, closed,
ungraded); Essays (50%) holds E1 (80/100, graded) and E2 (omitted from
the final grade).  Adams's Canvas totals are current 60.0 (D), final
55.0 (F); the scheme runs A 90, B 80, C 70, D 60, F 0."
  (declare (indent 0))
  `(test-gradebook--with-course
       (list (test-gradebook--enrollment
              1 "Adams, Alice" 10 60.0 55.0
              '(grades . ((current_score . 60.0) (final_score . 55.0)
                          (unposted_current_score . 60.0)
                          (unposted_final_score . 55.0)
                          (current_grade . "D") (final_grade . "F")
                          (unposted_current_grade . "D")
                          (unposted_final_grade . "F")))))
       '(((id . 10) (name . "Lecture")))
       nil
     (let ((test-gradebook--groups (test-gradebook--standing-groups))
           (test-gradebook--submissions (test-gradebook--standing-subs))
           (test-gradebook--course '((id . 1) (apply_assignment_group_weights . t)
                                     (grading_standard_id . 5)))
           (test-gradebook--standard
            '((id . 5) (grading_scheme . [((name . "A") (value . 0.9))
                                          ((name . "B") (value . 0.8))
                                          ((name . "C") (value . 0.7))
                                          ((name . "D") (value . 0.6))
                                          ((name . "F") (value . 0))]))))
       ,@body)))

(describe "org-canvas--gradebook-student-pick (issue #542)"
  (it "resolves a sortable name, a plain name, an id and a #id"
    (let ((enrollments
           (list (test-gradebook--enrollment
                  1 "Adams, Alice" 10 60.0 55.0
                  '(user . ((id . 1) (name . "Alice Adams")
                            (sortable_name . "Adams, Alice")))))))
      (dolist (arg '("Adams, Alice" "Alice Adams" "1" "#1"))
        (expect (alist-get 'user_id
                           (car (org-canvas--gradebook-student-pick enrollments arg)))
                :to-be 1))))

  (it "says when no student matches, and when the name is ambiguous"
    (let ((enrollments (list (test-gradebook--enrollment 1 "Adams, Alice" 10 60.0 55.0)
                             (test-gradebook--enrollment 2 "Adams, Alice" 20 70.0 65.0))))
      (expect (org-canvas--gradebook-student-pick enrollments "Nobody")
              :to-throw 'user-error)
      (expect (org-canvas--gradebook-student-pick enrollments "Adams, Alice")
              :to-throw 'user-error)
      (expect (org-canvas--gradebook-student-pick enrollments "2")
              :to-be-truthy))))

(describe "org-canvas--gradebook-student-row and the ceiling (issue #542)"
  (let ((now (date-to-time "2026-10-15T12:00:00Z")))
    (it "marks a column open until its lock, closed after it, and open without one"
      (let* ((assignments (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups)))
             (rows (org-canvas--gradebook-student-rows
                    (append (test-gradebook--standing-subs) nil) assignments now))
             (by-name (lambda (name)
                        (cl-find-if (lambda (r) (equal (plist-get r :name) name))
                                    rows))))
        (expect (plist-get (funcall by-name "Q1") :open) :to-be nil)
        (expect (plist-get (funcall by-name "Q2") :open) :to-be t)
        (expect (plist-get (funcall by-name "Q3") :open) :to-be nil)
        (expect (plist-get (funcall by-name "E1") :open) :to-be t)
        (expect (plist-get (funcall by-name "Q1") :item) :to-equal '(11 4.0 10))
        (expect (plist-get (funcall by-name "Q2") :item) :to-be nil)
        (expect (plist-get (funcall by-name "Q1") :counted) :to-be t)
        (expect (plist-get (funcall by-name "E2") :counted) :to-be nil)
        (expect (plist-get (funcall by-name "E1") :excused) :to-be nil)))

    (it "builds the ceiling: graded scores stand, open columns full, closed ungraded zero"
      (let* ((assignments (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups)))
             (rows (org-canvas--gradebook-student-rows
                    (append (test-gradebook--standing-subs) nil) assignments now)))
        (expect (org-canvas--gradebook-student-ceiling-items rows)
                :to-equal '((11 4.0 10) (12 10 10) (13 0 20) (21 80.0 100)))))

    (it "names the past-due columns still open, counted and ungraded"
      (let* ((assignments (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups)))
             (rows (org-canvas--gradebook-student-rows
                    (append (test-gradebook--standing-subs) nil) assignments now)))
        (expect (mapcar (lambda (r) (plist-get r :name))
                        (org-canvas--gradebook-student-open-rows rows now))
                :to-equal '("Q2"))))

    (it "names an assignment Canvas left nameless by its id"
      (expect (plist-get (org-canvas--gradebook-student-row
                          '((assignment_id . 13) (score . :null)
                            (workflow_state . "unsubmitted")
                            (assignment . ((id . 13) (assignment_group_id . 1)
                                           (published . t) (points_possible . 20)
                                           (due_at . "2026-09-20T03:59:00Z")
                                           (lock_at . "2026-09-27T03:59:00Z"))))
                          (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups))
                          now)
                         :name)
              :to-equal "Assignment 13"))))

(describe "org-canvas-gradebook-student (issue #542)"
  (it "prints every column grouped, with the still-open list and the ceiling"
    (test-gradebook--with-standing
      (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                     (replace-regexp-in-string
                      " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
        (expect report :to-match "Student standing: Adams, Alice")
        (expect report :to-match "Adams, Alice [(]user 1[)], Lecture$")
        (expect report :to-match "Canvas: current 60.0 (D), final 55.0 (F)$")
        (expect report :to-match "nothing was sent, nothing written")
        (expect report :to-match "^\\* Quizzes$")
        (expect report :to-match "^weight 50%, drop lowest 1 — 40.0% now, 70.0% at best$")
        (expect report :to-match "^\\* Essays$")
        (expect report :to-match "^weight 50% — 80.0% now, 80.0% at best$")
        (expect report :to-match
                 "^| Column | Due | Lock | Score | Points | State | Late | Missing | Open |$")
        (expect report :to-match "| Q1 | <2026-09-01")
        (expect report :to-match
                 "| Q2 |.*| - | 10 | unsubmitted | no | no | yes |$")
        (expect report :to-match "| E2 (not counted) |")
        (expect report :to-match "^Still open and past due: Q2$")
        (expect report :to-match
                 "^Best possible: 75.0% (C), every still-open column at full marks\\.$"))))

  (it "reads one student's submissions with their assignments, by due date"
    (test-gradebook--with-standing
      (expect (org-canvas--gradebook-student-params 1)
              :to-equal '(("student_ids[]" . "1") ("include[]" . "assignment")))
      (org-canvas--gradebook-student-submissions 1)
      (expect test-gradebook--submissions-params
              :to-equal '(("student_ids[]" . "1") ("include[]" . "assignment")))
      (expect (mapcar (lambda (s) (alist-get 'name (alist-get 'assignment s)))
                      (org-canvas--gradebook-student-submissions 1))
              :to-equal '("Q1" "Q2" "Q3" "E1" "E2"))))

  (it "says so when nothing is past due and still open"
    (test-gradebook--with-standing
      (let* ((test-gradebook--submissions
              (vector (test-gradebook--standing-sub
                       12 "Q2" 1 "2026-09-15T03:59:00Z" "2026-09-22T03:59:00Z" 10)))
             (report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "1")))))
        (expect report :to-match "^No column past due is still open\\.$"))))

  (it "says so when the course holds no assignments at all"
    (test-gradebook--with-standing
      (let* ((test-gradebook--submissions (vector))
             (report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "#1")))))
        (expect report :to-match "^No assignments\\.$")
        (expect report :to-match "^No column past due is still open\\.$"))))

  (it "prints a group's hurdle against its percentage after the drop"
    (test-gradebook--with-standing
      (let ((org-canvas-gradebook-hurdles '(("Quizzes" . 30))))
        (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
          (expect report :to-match
                  "^Hurdle 30%: 40.0% now, 70.0% at best — meets it now\\.$")))
      (let ((org-canvas-gradebook-hurdles '(("Quizzes" . 60))))
        (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
          (expect report :to-match
                  "^Hurdle 60%: 40.0% now, 70.0% at best — within reach at best\\.$")))
      (let ((org-canvas-gradebook-hurdles '(("Quizzes" . 90) ("No such group" . 50))))
        (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                       (replace-regexp-in-string
                        " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
          (expect report :to-match
                  "^Hurdle 90%: 40.0% now, 70.0% at best — out of reach even at best\\.$")
          (expect report :to-match "^No group named No such group for its hurdle\\.$")))))

  (it "prompts with completion over the roster's sortable names"
    (test-gradebook--with-standing
      (let* ((test-gradebook--enrollments
              (append (org-canvas--gradebook-fetch-enrollments)
                      (list '((user_id . 3)
                              (user . ((id . 3) (name . "No Sortable")))))))
             (seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (setq seen (append collection nil))
                     "Adams, Alice")))
          (expect (org-canvas--gradebook-student-prompt) :to-equal "Adams, Alice")
          (expect seen :to-contain "Adams, Alice")
          (expect seen :to-contain "No Sortable")))))

  (it "reads a group's drop rules and never-drop ids as text"
    (expect (org-canvas--gradebook-student-drops-text
             '(:drop-lowest 1 :drop-highest 2 :never-drop (11 12)))
            :to-equal "drop lowest 1, drop highest 2, never drops 11, 12")
    (expect (org-canvas--gradebook-student-drops-text '(:weight 50))
            :to-equal ""))

  (it "answers interactively, the name completed from the roster"
    (test-gradebook--with-standing
      (let ((report (cl-letf (((symbol-function 'completing-read)
                               (lambda (_prompt _collection &rest _)
                                 "Adams, Alice"))
                              ((symbol-function 'princ) #'ignore))
                     (replace-regexp-in-string
                      " +" " " (call-interactively #'org-canvas-gradebook-student)))))
        (expect report :to-match "Student standing: Adams, Alice")
        (expect report :to-match "^Best possible: 75.0% (C),")))))

;;;; Submitted work and the student's own dates (#674)

(defun test-gradebook--override (id &rest pairs)
  "An assignment override ID carrying the field PAIRS, as Canvas reads it."
  (append `((id . ,id)) pairs))

(defun test-gradebook--q2-with-overrides (&rest overrides)
  "The standing groups with OVERRIDES on Q2, as the groups read carries them."
  (mapcar (lambda (g)
            (if (eql (alist-get 'id g) 1)
                (cons (cons 'assignments
                            (vconcat
                             (mapcar (lambda (a)
                                       (if (eql (alist-get 'id a) 12)
                                           (cons (cons 'overrides (vconcat overrides)) a)
                                         a))
                                     (alist-get 'assignments g))))
                      g)
              g))
          (test-gradebook--standing-groups)))

(defun test-gradebook--q2-row (now &rest overrides)
  "Adams's Q2 row at NOW, its lock in 2030, under OVERRIDES on Q2.
Adams is user 1 in section 10."
  (let ((groups (apply #'test-gradebook--q2-with-overrides overrides)))
    (org-canvas--gradebook-student-row
     (test-gradebook--standing-sub 12 "Q2" 1 "2026-09-15T03:59:00Z" "2030-01-01T00:00:00Z" 10)
     (org-canvas--gradebook-assignment-map groups)
     now
     (list :user-id 1 :section-ids '(10)
           :overrides (org-canvas--gradebook-override-map groups)))))

(describe "the student report's ceiling and dates (issue #674)"
  (let ((now (date-to-time "2026-10-15T12:00:00Z")))
    (it "counts a submitted, ungraded column at full marks once it has locked"
      (let* ((assignments (org-canvas--gradebook-assignment-map
                           (test-gradebook--standing-groups)))
             (subs (list (test-gradebook--standing-sub
                          13 "Q3" 1 "2026-09-20T03:59:00Z" "2026-09-27T03:59:00Z" 20
                          '(workflow_state . "submitted")
                          '(submitted_at . "2026-09-19T00:00:00Z"))
                         (test-gradebook--standing-sub
                          11 "Q1" 1 "2026-09-01T03:59:00Z" "2026-09-08T03:59:00Z" 10
                          '(workflow_state . "pending_review"))))
             (rows (org-canvas--gradebook-student-rows subs assignments now)))
        (expect (plist-get (car rows) :open) :to-be nil)
        (expect (plist-get (car rows) :submitted) :to-be t)
        (expect (org-canvas--gradebook-student-ceiling-items rows)
                :to-equal '((13 20 20) (11 10 10)))))

    (it "reads the lock from the student's section override, not the column's own"
      (let ((row (test-gradebook--q2-row
                  now (test-gradebook--override
                       1 '(course_section_id . 10)
                       '(due_at . "2026-09-14T03:59:00Z")
                       '(lock_at . "2026-09-21T03:59:00Z")))))
        (expect (plist-get row :open) :to-be nil)
        (expect (plist-get row :lock) :to-equal (date-to-time "2026-09-21T03:59:00Z"))
        (expect (plist-get row :due) :to-equal (date-to-time "2026-09-14T03:59:00Z"))))

    (it "takes the latest lock of the overrides that apply, and none beats any"
      (let ((section (test-gradebook--override
                      1 '(course_section_id . 10) '(lock_at . "2026-09-21T03:59:00Z")))
            (mine (test-gradebook--override
                   2 '(student_ids . [1 7]) '(lock_at . "2026-11-01T03:59:00Z")))
            (open (test-gradebook--override 3 '(student_ids . [1]) '(lock_at . :null))))
        (expect (plist-get (test-gradebook--q2-row now section mine) :lock)
                :to-equal (date-to-time "2026-11-01T03:59:00Z"))
        (expect (plist-get (test-gradebook--q2-row now section mine) :open) :to-be t)
        (expect (plist-get (test-gradebook--q2-row now section open) :lock) :to-be nil)
        (expect (plist-get (test-gradebook--q2-row now section open) :open) :to-be t)))

    (it "keeps the column's own date where no override that applies sets it"
      (let ((other (test-gradebook--override
                    1 '(course_section_id . 20) '(lock_at . "2026-09-21T03:59:00Z")))
            (due-only (test-gradebook--override
                       2 '(course_section_id . 10) '(due_at . "2026-09-16T03:59:00Z"))))
        (expect (plist-get (test-gradebook--q2-row now other due-only) :lock)
                :to-equal (date-to-time "2030-01-01T00:00:00Z"))
        (expect (plist-get (test-gradebook--q2-row now other due-only) :due)
                :to-equal (date-to-time "2026-09-16T03:59:00Z"))))

    (it "prints the override's lock and leaves the column out of the still-open list"
      (test-gradebook--with-standing
        (let* ((test-gradebook--groups
                (test-gradebook--q2-with-overrides
                 (test-gradebook--override
                  1 '(course_section_id . 10) '(lock_at . "2026-09-21T03:59:00Z"))))
               (report (cl-letf (((symbol-function 'princ) #'ignore))
                         (replace-regexp-in-string
                          " +" " " (org-canvas-gradebook-student "Adams, Alice")))))
          (expect test-gradebook--groups-params
                  :to-equal '(("include[]" . "assignments") ("include[]" . "overrides")))
          (expect report :to-match
                  "| Q2 |.*| <2026-09-2[01] [^>]+> | - | 10 | unsubmitted | no | no | no |$")
          (expect report :to-match "^No column past due is still open\\.$"))))))

;;;; The class-wide hurdle report (#674)

(defun test-gradebook--recitation-groups ()
  "Recitation (drop lowest 1): R1 to R4, ten points each, and Exams.
R3 carries a section 20 override that moves it to 2099."
  `(((id . 3) (name . "Recitation") (position . 1) (group_weight . 100)
     (rules . ((drop_lowest . 1)))
     (assignments
      . [((id . 31) (name . "R1") (published . t) (points_possible . 10)
          (due_at . "2020-01-08T03:59:00Z") (lock_at . "2020-01-15T03:59:00Z"))
         ((id . 32) (name . "R2") (published . t) (points_possible . 10)
          (due_at . "2020-02-08T03:59:00Z") (lock_at . "2020-02-15T03:59:00Z"))
         ((id . 33) (name . "R3") (published . t) (points_possible . 10)
          (due_at . "2020-03-08T03:59:00Z") (lock_at . "2020-03-15T03:59:00Z")
          (overrides . [((id . 9) (course_section_id . 20)
                         (due_at . "2099-03-08T03:59:00Z")
                         (lock_at . "2099-03-15T03:59:00Z"))]))
         ((id . 34) (name . "R4") (published . t) (points_possible . 10)
          (due_at . "2099-04-08T03:59:00Z") (lock_at . "2099-04-15T03:59:00Z"))]))
    ((id . 4) (name . "Exams") (position . 2) (group_weight . 0) (rules . nil)
     (assignments . [((id . 41) (name . "Midterm") (published . t)
                      (points_possible . 100))]))))

(defun test-gradebook--recitation-sub (uid aid &rest extra)
  "UID's unsubmitted submission to AID, with EXTRA pairs first."
  (append extra `((user_id . ,uid) (assignment_id . ,aid) (score . :null)
                  (workflow_state . "unsubmitted") (submitted_at . :null)
                  (posted_at . :null) (excused . :json-false))))

(defun test-gradebook--recitation-graded (uid aid score)
  "UID's graded, posted submission to AID scoring SCORE."
  (test-gradebook--recitation-sub
   uid aid `(score . ,score) '(workflow_state . "graded")
   '(submitted_at . "2020-01-01T00:00:00Z")
   '(posted_at . "2020-01-02T00:00:00Z")))

(defun test-gradebook--recitation-subs ()
  "Adams scored R1 8 and R2 6 and waits on R3; Baker scored R1 4 and has
R3 open by override; Clark turned nothing in."
  (vector (test-gradebook--recitation-graded 1 31 8.0)
          (test-gradebook--recitation-graded 1 32 6.0)
          (test-gradebook--recitation-sub 1 33 '(workflow_state . "submitted")
                                          '(submitted_at . "2020-03-01T00:00:00Z"))
          (test-gradebook--recitation-sub 1 34)
          (test-gradebook--recitation-graded 2 31 4.0)
          (test-gradebook--recitation-sub 2 32)
          (test-gradebook--recitation-sub 2 33) (test-gradebook--recitation-sub 2 34)
          (test-gradebook--recitation-sub 3 31) (test-gradebook--recitation-sub 3 32)
          (test-gradebook--recitation-sub 3 33) (test-gradebook--recitation-sub 3 34)
          (test-gradebook--recitation-graded 1 41 90.0)))

(defmacro test-gradebook--with-recitation (&rest body)
  "Run BODY with the fake API serving the recitation class (issue #674).
Adams in Lecture (10), Baker in Lab (20), Clark in Online (30); a 60%
hurdle on Recitation."
  (declare (indent 0))
  `(test-gradebook--with-course
       (list (test-gradebook--enrollment 1 "Adams, Alice" 10 80.0 80.0)
             (test-gradebook--enrollment 2 "Baker, Bob" 20 40.0 10.0)
             (test-gradebook--enrollment 3 "Clark, Cy" 30 nil 0.0))
       '(((id . 10) (name . "Lecture")) ((id . 20) (name . "Lab"))
         ((id . 30) (name . "Online")))
       nil
     (let ((test-gradebook--groups (test-gradebook--recitation-groups))
           (test-gradebook--submissions (test-gradebook--recitation-subs))
           (test-gradebook--course '((id . 1)))
           (org-canvas-gradebook-hurdles '(("Recitation" . 60))))
       ,@body)))

(defun test-gradebook--hurdle-report (&optional group)
  "Run the hurdle report for GROUP and return it, runs of blanks collapsed."
  (cl-letf (((symbol-function 'princ) #'ignore))
    (replace-regexp-in-string " +" " " (org-canvas-gradebook-hurdle-report group))))

(describe "org-canvas-gradebook-hurdle-report (issue #674)"
  (it "prints every student's standing against the hurdle, column by column"
    (test-gradebook--with-recitation
      (let ((report (test-gradebook--hurdle-report)))
        (expect report :to-match "nothing was sent, nothing written")
        (expect report :to-match "^\\* Recitation$")
        (expect report :to-match "^Hurdle 60%, drop lowest 1\\.$")
        (expect report :to-match
                "^| Student | Section | Now | Past due as 0 | At best | Needed | Verdict | R1 | R2 | R3 | R4 |$")
        (expect report :to-match
                "^| Adams, Alice | Lecture | 80.0 | 80.0 | 93.3 | 40.0 | meets it now | 8.0 | 6.0 | awaiting | ahead |$")
        (expect report :to-match
                "^| Baker, Bob | Lab | 40.0 | 40.0 | 80.0 | 70.0 | within reach | 4.0 | nothing in | ahead | ahead |$")
        (expect report :to-match
                "^| Clark, Cy | Online | - | 0.0 | 33.3 | - | out of reach | nothing in | nothing in | nothing in | ahead |$")
        (expect report :to-match
                "^3 students: 1 meets it now, 1 within reach, 1 out of reach\\.$")
        (expect report :not :to-match "Midterm")
        (expect test-gradebook--submissions-params
                :to-equal '(("student_ids[]" . "all"))))))

  (it "reads one named group, needing no hurdle for it"
    (test-gradebook--with-recitation
      (let* ((org-canvas-gradebook-hurdles nil)
             (report (test-gradebook--hurdle-report "Exams")))
        (expect report :to-match "^\\* Exams$")
        (expect report :to-match "^No hurdle set\\.$")
        (expect report :to-match
                "^| Adams, Alice | Lecture | 90.0 | 90.0 | 90.0 | - | - | 90.0 |$")
        (expect report :to-match
                "^| Baker, Bob | Lab | - | - | - | - | - | - |$")
        (expect report :not :to-match "^\\* Recitation$"))))

  (it "says when a hurdle names no group, and refuses with nothing to report"
    (test-gradebook--with-recitation
      (let ((org-canvas-gradebook-hurdles '(("Recitation" . 60) ("Labs" . 50))))
        (expect (test-gradebook--hurdle-report)
                :to-match "^No group named Labs\\.$"))
      (let ((org-canvas-gradebook-hurdles nil))
        (expect (test-gradebook--hurdle-report) :to-throw 'user-error))))

  (it "needs nothing once the hurdle is met with the rest at zero, and skips excused work"
    (let ((now (date-to-time "2026-10-15T12:00:00Z"))
          (group '(:id 3 :name "Recitation"))
          (rows (list '(:id 1 :points 10 :item (1 9.0 10))
                      '(:id 2 :points 10 :open t :due nil)
                      '(:id 3 :points 10 :excused t))))
      (expect (org-canvas--gradebook-hurdle-cell (nth 2 rows) now) :to-equal "excused")
      (expect (org-canvas--gradebook-hurdle-cell (nth 1 rows) now) :to-equal "ahead")
      (expect (org-canvas--gradebook-hurdle-item (nth 2 rows) now 1) :to-be nil)
      (expect (org-canvas--gradebook-hurdle-percent rows group now nil) :to-equal 90.0)
      (expect (org-canvas--gradebook-hurdle-percent rows group now 1) :to-equal 95.0)
      (expect (org-canvas--gradebook-hurdle-needed rows group now 40) :to-equal 0)
      (expect (org-canvas--gradebook-hurdle-needed rows group now nil) :to-be nil)
      (expect (org-canvas--gradebook-hurdle-tally '("within reach"))
              :to-equal "1 student: 0 meets it now, 1 within reach, 0 out of reach.\n")))

  (it "answers interactively for every hurdle"
    (test-gradebook--with-recitation
      (let ((report (cl-letf (((symbol-function 'princ) #'ignore))
                      (call-interactively #'org-canvas-gradebook-hurdle-report))))
        (expect report :to-match "Adams, Alice")))))

(provide 'org-canvas-gradebook-report-test)
;;; org-canvas-gradebook-report-test.el ends here
