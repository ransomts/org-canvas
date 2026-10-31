;;; test-fixtures.el --- Shared course and API fixtures for org-canvas tests  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The two things almost every spec needs, and that each feature test
;; file used to build for itself (issue #574):
;;
;; - `with-test-course' makes a temporary course directory from an alist
;;   of file names and contents, and binds every `org-canvas-*-file'
;;   variable into it from `org-canvas--file-var-registry' -- the same
;;   set `with-validate-test-dir' binds -- so a spec names only the
;;   files it cares about and never reads the developer's own course.
;; - `with-test-api' and its variants are the one mocked-Canvas harness.
;;   They compose `with-org-canvas-test-config' and `with-mock-api'
;;   instead of every file repeating the pair.
;;
;; File contents are built with `test-org-canvas-heading' and
;; `test-org-canvas-headings', for the "* HW" over ":PROPERTIES:" shape
;; most fixtures hand-wrote, and read back with
;; `test-org-canvas-course-prop'.
;;
;; A third, narrower group sits at the end: the fake Canvas the three
;; gradebook spec files answer from (#593), so a roster, its sections
;; and its submissions are built once rather than three times.
;;
;; This lives beside test-helper.el rather than inside it: the helper is
;; the suite's machinery (guards, network refusal, generators) and
;; already long, while this is one layer of fixtures that more files
;; will move onto a few at a time.  test-helper.el loads it after the
;; package, so the registry is complete; test-fixtures.el requires
;; nothing of test-helper at load time, which is what keeps the two from
;; loading each other recursively.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Org content builders

(defun test-org-canvas-heading (title &rest spec)
  "Return TITLE as one Org entry, with the drawer and body SPEC gives.
SPEC is a plist:
  :props  alist of (NAME . VALUE); a nil VALUE leaves the property out,
          and nil writes no drawer at all
  :body   text inserted after the drawer (default none)
  :level  headline depth (default 1)

The shape most specs need -- \"* HW\" over a \":PROPERTIES:\" drawer --
is (test-org-canvas-heading \"HW\" :props \\='((\"CANVAS_ID\" . \"61\")))."
  (let ((props (plist-get spec :props))
        (body (or (plist-get spec :body) ""))
        (level (or (plist-get spec :level) 1)))
    (concat (make-string level ?*)
            " " title "\n"
            (when props
              (concat ":PROPERTIES:\n"
                      (mapconcat #'identity
                                 (delq nil
                                       (mapcar (lambda (pair)
                                                 (when (cdr pair)
                                                   (format ":%s: %s\n"
                                                           (car pair) (cdr pair))))
                                               props))
                                 "")
                      ":END:\n"))
            body)))

(defun test-org-canvas-headings (entries)
  "Return ENTRIES, a list of `test-org-canvas-heading' arguments, as text.
Each entry is (TITLE :props ALIST :body TEXT :level N), so
\(test-org-canvas-headings
 \\='((\"Lab 1\" :props ((\"CANVAS_ID\" . \"61\")))
   (\"Lab 2\" :props ((\"CANVAS_ID\" . \"62\")))))
is a two-assignment file."
  (mapconcat (lambda (entry) (apply #'test-org-canvas-heading entry))
             entries ""))

(defun test-org-canvas-course-contents (value)
  "Return VALUE, a `with-test-course' file's content, as text to write.
VALUE is a string, nil for an empty file, or a plist of :prefix,
:headings and :suffix, where :headings is passed to
`test-org-canvas-headings'."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((eq (car-safe value) :headings)
    (concat (or (plist-get value :prefix) "")
            (test-org-canvas-headings (plist-get value :headings))
            (or (plist-get value :suffix) "")))
   (t (error "Cannot write a course file from %S" value))))

;;;; Course directory

(defun test-org-canvas-course-write (dir files)
  "Write FILES into DIR, creating parent directories as needed.
FILES is an alist of (RELATIVE-NAME . CONTENT).  A name ending in a
slash is made a directory; otherwise CONTENT goes through
`test-org-canvas-course-contents' into the file."
  (dolist (entry files)
    (let* ((name (car entry))
           (path (expand-file-name name dir)))
      (if (string-suffix-p "/" name)
          (make-directory path t)
        (make-directory (file-name-directory path) t)
        (with-temp-file path
          (insert (test-org-canvas-course-contents (cdr entry))))))))

(defun test-org-canvas-course-cleanup (dir)
  "Kill the buffers visiting files under DIR, then delete DIR.
A buffer left visiting a deleted file keeps one spec's course for the
next one and holds a lock; `with-temp-org-buffer' kills its buffer for
the same reason."
  (let ((root (directory-file-name (file-truename dir))))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-file-name buffer)))
        (when (and file
                   (string-prefix-p (concat root "/")
                                    (directory-file-name (file-truename file))))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))))
  (when (file-exists-p dir)
    (delete-directory dir t)))

(defun test-org-canvas-course-bindings (dir)
  "Return the course bindings for DIR as (VARIABLES . VALUES).
The variables are `org-canvas-directory' and every file-path variable
in `org-canvas--file-var-registry', each pointed at its own file under
DIR whether or not that file exists -- which is what keeps a spec from
reaching the developer's course through a variable it did not name."
  (cons (cons 'org-canvas-directory
              (mapcar #'car org-canvas--file-var-registry))
        (cons (file-name-as-directory dir)
              (mapcar (lambda (entry)
                        (expand-file-name (cdr entry) (directory-file-name dir)))
                      org-canvas--file-var-registry))))

(defmacro with-test-course (spec &rest body)
  "Run BODY with a temporary course directory built from SPEC.
SPEC is (DIR-VAR :files FILES), where DIR-VAR is bound to the
directory and FILES is any expression that evaluates to an alist of
\(RELATIVE-NAME . CONTENT) as `test-org-canvas-course-write' takes.
FILES may be nil, and may mention DIR-VAR -- the files are written
after it is bound.

Every `org-canvas-*-file' variable is bound to its own path under the
directory, so a spec lists only the files it cares about.  The
directory, and the buffers visiting its files, are removed after BODY.

  (with-test-course (dir :files \\='((\"assignments.org\"
                              :headings ((\"HW\" :props ((\"CANVAS_ID\" . \"61\")))))))
    (org-canvas-sync-assignments))"
  (declare (indent 1) (debug (sexp body)))
  (let ((dir-var (car spec))
        (files (plist-get (cdr spec) :files)))
    (unless (and (symbolp dir-var) dir-var)
      (error "with-test-course: SPEC must start with the directory variable"))
    `(let ((,dir-var (make-temp-file "org-canvas-course-" t)))
       (unwind-protect
           (progn
             (test-org-canvas-course-write ,dir-var ,files)
             (let ((test-org-canvas--course-bindings
                    (test-org-canvas-course-bindings ,dir-var)))
               (cl-progv (car test-org-canvas--course-bindings)
                         (cdr test-org-canvas--course-bindings)
                 ,@body)))
         (test-org-canvas-course-cleanup ,dir-var)))))

(defun test-org-canvas-course-prop (dir file heading property)
  "Return PROPERTY of HEADING in DIR/FILE, or nil.
HEADING is matched anywhere in a headline's text, so both a link's
description and the title it names find the entry."
  (with-current-buffer (find-file-noselect (expand-file-name file dir))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^\\*+ .*%s" (regexp-quote heading)) nil t)
        (org-back-to-heading t)
        (org-entry-get (point) property)))))

(defun test-org-canvas-course-heading-pos (dir file title)
  "Return the position of the headline TITLE in DIR/FILE.
TITLE is matched anywhere in a headline's text, as in
`test-org-canvas-course-prop'."
  (with-current-buffer (find-file-noselect (expand-file-name file dir))
    (goto-char (point-min))
    (re-search-forward (format "^\\*+ .*%s" (regexp-quote title)))
    (org-back-to-heading t)
    (point)))

;;;; API mocking

;; `with-mock-api' (test-helper.el) answers from
;; `test-org-canvas-api-responses' unless `test-org-canvas-api-handler'
;; is non-nil, in which case that function answers the request.  The
;; recording is the same either way, so `test-org-canvas-api-called-p',
;; `test-org-canvas-api-call-count' and `expect-api-called' work for
;; every variant below.

(defmacro with-test-api (&rest body)
  "Run BODY with the test Canvas configuration and a mocked API.
`with-org-canvas-test-config' and `with-mock-api' are the two things a
spec that talks to Canvas always wants together; this is the one place
they are paired."
  (declare (indent 0))
  `(with-org-canvas-test-config
     (with-mock-api
       ,@body)))

(defmacro with-test-api-responses (responses &rest body)
  "Run BODY like `with-test-api', answering from the RESPONSES table.
RESPONSES is an alist of (URL-PATTERN . RESPONSE) as
`test-org-canvas-api-responses' takes; the first pattern that matches
the URL answers, and anything unmatched gets the canned default."
  (declare (indent 1))
  `(with-test-api
     (let ((test-org-canvas-api-responses ,responses))
       ,@body)))

(defmacro with-test-api-handler (handler &rest body)
  "Run BODY like `with-test-api', answering each request by calling HANDLER.
HANDLER receives METHOD, URL and the keyword arguments
`org-canvas-api-request' was called with, the same convention the
`cl-letf' forms this replaces use, and its return value is the response
-- nil included, so a hand-written dispatcher keeps answering for the
URLs it does not know."
  (declare (indent 1))
  `(with-test-api
     (let ((test-org-canvas-api-handler ,handler))
       ,@body)))

(defmacro with-test-sync (&rest body)
  "Run BODY like `with-test-api', with sync's own side effects silenced.
Composes `with-sync-test-env' with `with-test-api'."
  (declare (indent 0))
  `(with-sync-test-env
     (with-test-api
       ,@body)))

(defmacro with-test-sync-handler (handler &rest body)
  "Run BODY like `with-test-api-handler', with sync's side effects silenced.
Composes `with-sync-test-env' with `with-test-api-handler'."
  (declare (indent 1))
  `(with-sync-test-env
     (with-test-api-handler ,handler
       ,@body)))

;;;; Gradebook API fake

;; The fake Canvas the three gradebook spec files answer from: one roster
;; with its sections, analytics, custom columns and submissions, keyed on
;; the URL the pull asks for, the builders for the rows of each, and
;; `test-gradebook--with-scores', the course of two students with groups
;; on that the Groups table and the what-if both read.  It lived in
;; org-canvas-gradebook-test.el until #593 split that file in three, and
;; the overview, the group scores and the reports share it rather than
;; keep a copy each.

(defvar test-gradebook--enrollments nil
  "Student enrollments the fake API lists for the course.")
(defvar test-gradebook--sections nil
  "Sections the fake API lists for the course.")
(defvar test-gradebook--summaries nil
  "Student summaries the fake API lists, or `refuse' to answer with a 403.")
(defvar test-gradebook--columns nil
  "Custom gradebook columns the fake API lists, or `refuse' to answer with a 403.
Each column may carry a `data' entry: the rows its data endpoint answers.")
(defvar test-gradebook--columns-params nil
  "The parameters the last request for the custom columns carried.")
(defvar test-gradebook--assignments nil
  "Assignment analytics rows the fake API lists, or `refuse' to answer with a 403.")

(defvar test-gradebook--groups nil
  "Assignment groups, with their assignments, the fake API lists.")
(defvar test-gradebook--submissions nil
  "Submissions the fake API lists for every student, or `refuse'.")
(defvar test-gradebook--course nil
  "The course object the fake single request answers.")
(defvar test-gradebook--standard nil
  "The grading standard the fake single request answers, or `refuse'.")

(defvar test-gradebook--course-reads nil
  "The parameters of each course read, latest first.")
(defvar test-gradebook--submissions-params nil
  "The parameters the last request for the submissions carried.")

(defun test-gradebook--request (_method url &rest args)
  "Answer a single request for URL: the course or its grading standard.
ARGS are the keyword arguments; a course read's :params are recorded."
  (cond
   ((string-match "/grading_standards/" url)
    (if (eq test-gradebook--standard 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--standard))
   ((string-match "/courses/[0-9]+/\\'" url)
    (push (plist-get args :params) test-gradebook--course-reads)
    (if (eq test-gradebook--course 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--course))
   (t (error "Unexpected request: %s" url))))

(defun test-gradebook--api (_method url &optional _params)
  "Answer URL from the fake tables, as the paginated helper would."
  (cond
   ((string-match "/enrollments" url)
    (if (eq test-gradebook--enrollments 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--enrollments))
   ((string-match "/sections\\'" url) test-gradebook--sections)
   ((string-match "/analytics/student_summaries" url)
    (if (eq test-gradebook--summaries 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--summaries))
   ((string-match "/analytics/assignments" url)
    (if (eq test-gradebook--assignments 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--assignments))
   ((string-match "/custom_gradebook_columns/\\([0-9]+\\)/data" url)
    (let ((id (string-to-number (match-string 1 url))))
      (alist-get 'data (cl-find-if (lambda (c) (eql (alist-get 'id c) id))
                                   test-gradebook--columns))))
   ((string-match "/assignment_groups" url) test-gradebook--groups)
   ((string-match "/students/submissions" url)
    (setq test-gradebook--submissions-params _params)
    (if (eq test-gradebook--submissions 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--submissions))
   ((string-match "/custom_gradebook_columns\\'" url)
    (setq test-gradebook--columns-params _params)
    (if (eq test-gradebook--columns 'refuse)
        (signal 'org-canvas-permission-error (list "403 Forbidden"))
      test-gradebook--columns))
   (t (error "Unexpected request: %s" url))))

(defun test-gradebook--enrollment (uid name section current final &rest extra)
  "A student enrollment for UID named NAME in SECTION scoring CURRENT and FINAL.
EXTRA pairs come first, so an explicit value shadows the default.
Positional is right: fifty-odd call sites read as the row they build —
student, section, two scores — and a bundle would add a field name per
argument at every one without changing what is passed (issue #599)."
  (append extra
          `((id . ,(+ 1000 uid (* 10 section)))
            (user_id . ,uid) (type . "StudentEnrollment")
            (course_section_id . ,section) (enrollment_state . "active")
            (user . ((id . ,uid) (name . ,name) (sortable_name . ,name)))
            (grades . ((current_score . ,current) (final_score . ,final)
                       (unposted_current_score . ,current)
                       (unposted_final_score . ,final))))))

(defun test-gradebook--column (id title position &rest data)
  "A custom gradebook column ID titled TITLE at POSITION.
DATA are (UID . CONTENT) pairs its data endpoint answers."
  `((id . ,id) (title . ,title) (position . ,position) (hidden . :json-false)
    (teacher_notes . :json-false) (read_only . :json-false)
    (data . ,(mapcar (lambda (d) `((user_id . ,(car d)) (content . ,(cdr d)))) data))))

(defun test-gradebook--assignment (id title due points &rest stats)
  "An assignment analytics row ID titled TITLE, due DUE, out of POINTS.
STATS are (KEY . VALUE) pairs for the score fields and the tardiness
counts; a score field not given is null, a count not given is 0."
  `((assignment_id . ,id) (title . ,title) (due_at . ,due) (muted . :json-false)
    (points_possible . ,points) (non_digital_submission . :json-false)
    (max_score . ,(alist-get 'max_score stats :null))
    (min_score . ,(alist-get 'min_score stats :null))
    (first_quartile . ,(alist-get 'first_quartile stats :null))
    (median . ,(alist-get 'median stats :null))
    (third_quartile . ,(alist-get 'third_quartile stats :null))
    (tardiness_breakdown . ((missing . ,(alist-get 'missing stats 0))
                            (late . ,(alist-get 'late stats 0))
                            (on_time . 5) (floating . 0) (total . 5)))))

(defun test-gradebook--summary (uid missing late)
  "A student summary for UID with MISSING and LATE submissions."
  `((id . ,uid) (page_views . 3)
    (tardiness_breakdown . ((missing . ,missing) (late . ,late)
                            (on_time . 5) (floating . 0) (total . ,(+ missing late 5))))))

(defun test-gradebook--fold-summaries (summaries)
  "Fold SUMMARIES as the fetch does: an alist of user id to tardiness."
  (mapcar (lambda (s) (cons (alist-get 'id s) (alist-get 'tardiness_breakdown s)))
          summaries))

(defun test-gradebook--with-override (enrollment override)
  "Return ENROLLMENT with OVERRIDE, a percentage, in its grades."
  (let ((copy (copy-alist enrollment)))
    (setcdr (assq 'grades copy)
            (cons (cons 'override_score override) (cdr (assq 'grades copy))))
    copy))

(defmacro test-gradebook--with-course (enrollments sections summaries &rest body)
  "Run BODY with the fake API serving ENROLLMENTS, SECTIONS and SUMMARIES.
The gradebook and roster files live in a temp directory."
  (declare (indent 3))
  `(let* ((dir (make-temp-file "gradebook-" t))
          (org-canvas-gradebook-file (expand-file-name "gradebook.org" dir))
          (org-canvas-people-file (expand-file-name "people.org" dir))
          (org-canvas-assignments-file (expand-file-name "assignments.org" dir))
          (test-gradebook--enrollments ,enrollments)
          (test-gradebook--sections ,sections)
          (test-gradebook--summaries ,summaries)
          (test-gradebook--columns nil)
          (test-gradebook--columns-params nil)
          (test-gradebook--assignments nil)
          (test-gradebook--course nil)
          (test-gradebook--course-reads nil)
          (test-gradebook--submissions-params nil))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-gradebook--api)
                     ((symbol-function 'org-canvas-api-request) #'test-gradebook--request)
                     ((symbol-function 'message) #'ignore))
             ,@body))
       (dolist (f (list org-canvas-gradebook-file org-canvas-people-file
                        org-canvas-assignments-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-gradebook--sub (uid aid score &rest extra)
  "A submission of UID on AID scoring SCORE, posted, with EXTRA pairs first."
  (append extra `((user_id . ,uid) (assignment_id . ,aid) (score . ,score)
                  (posted_at . "2026-09-01T00:00:00Z") (excused . :json-false))))

(defun test-gradebook--fixture-groups ()
  "Quizzes (50%, drop lowest 1) and Essays (50%), with assignments."
  `(((id . 2) (name . "Essays") (position . 2) (group_weight . 50) (rules . nil)
     (assignments . [((id . 21) (published . t) (points_possible . 100))
                     ((id . 22) (published . t) (points_possible . 100)
                      (omit_from_final_grade . t))]))
    ((id . 1) (name . "Quizzes") (position . 1) (group_weight . 50)
     (rules . ((drop_lowest . 1)))
     (assignments . [((id . 11) (published . t) (points_possible . 10))
                     ((id . 12) (published . t) (points_possible . 10))
                     ((id . 13) (published . t) (points_possible . 20))
                     ((id . 14) (published . :json-false) (points_possible . 10))]))))

(defun test-gradebook--fixture-submissions ()
  "Adams: Quizzes 93.3 after the drop, Essays 80.  Beta: Quizzes 50, no essay."
  (list (test-gradebook--sub 1 11 10) (test-gradebook--sub 1 12 2)
        (test-gradebook--sub 1 13 18) (test-gradebook--sub 1 21 80)
        (test-gradebook--sub 1 22 100) (test-gradebook--sub 1 14 5)
        (test-gradebook--sub 2 11 0) (test-gradebook--sub 2 12 0 '(excused . t))
        (test-gradebook--sub 2 13 10) (test-gradebook--sub 2 21 :null)))

(defmacro test-gradebook--with-scores (canvas-beta &rest body)
  "Run BODY in a course of Adams (86.67) and Beta (CANVAS-BETA) with groups on."
  (declare (indent 1))
  `(test-gradebook--with-course
       (list (test-gradebook--enrollment 1 "Adams, Alice" 10 86.67 80.0)
             (test-gradebook--enrollment 2 "Beta, Bob" 10 ,canvas-beta 20.0))
       '(((id . 10) (name . "Lecture")))
       nil
     (let ((org-canvas-gradebook-groups t)
           (org-canvas-gradebook-scores-file (expand-file-name "gradebook-scores.eld" dir))
           (org-canvas-assignment-groups-file (expand-file-name "assignment-groups.org" dir))
           (org-canvas-settings-file (expand-file-name "settings.org" dir))
           (test-gradebook--groups (test-gradebook--fixture-groups))
           (test-gradebook--submissions (test-gradebook--fixture-submissions))
           (test-gradebook--course '((id . 1) (apply_assignment_group_weights . t)
                                     (grading_standard_id . 5)))
           (test-gradebook--standard '((id . 5) (grading_scheme . [((name . "A") (value . 0.85))
                                                                   ((name . "B") (value . 0.75))
                                                                   ((name . "F") (value . 0))]))))
       (cl-letf (((symbol-function 'org-canvas-api-request) #'test-gradebook--request)
                 ;; Not a git work tree: the scores file is left alone.
                 ((symbol-function 'process-file) (lambda (&rest _) 128)))
         (unwind-protect (progn ,@body)
           (dolist (f (list org-canvas-assignment-groups-file org-canvas-settings-file))
             (let ((buf (find-buffer-visiting f)))
               (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))))))))

(defun test-gradebook--file ()
  "Return gradebook.org's text."
  (with-temp-buffer (insert-file-contents org-canvas-gradebook-file) (buffer-string)))

(defun test-gradebook--row (name)
  "Return the table row of the student NAME, cells trimmed, or nil."
  (let ((text (test-gradebook--file)))
    (when (string-match (format "^| *\\(?:\\[\\[[^]]+\\]\\[\\)?%s\\(?:\\]\\]\\)? *|\\(.*\\)$"
                                (regexp-quote name))
                        text)
      (mapcar #'string-trim (split-string (match-string 1 text) "|" t)))))

(provide 'test-fixtures)
;;; test-fixtures.el ends here
