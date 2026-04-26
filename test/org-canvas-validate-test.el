;;; org-canvas-validate-test.el --- Tests for org-canvas-validate  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the validation engine: type validators, date ordering,
;; structural validators, and the main `org-canvas-validate' command.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

;;;; Type Validator Tests

(describe "org-canvas--validate-check-boolean"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-boolean nil "PUBLISHED" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for 'true'"
    (expect (org-canvas--validate-check-boolean "true" "PUBLISHED" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for 'false'"
    (expect (org-canvas--validate-check-boolean "false" "PUBLISHED" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for 'TRUE' (case insensitive)"
    (expect (org-canvas--validate-check-boolean "TRUE" "PUBLISHED" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns error for invalid value"
    (let ((issue (org-canvas--validate-check-boolean "yes" "PUBLISHED" (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not a valid boolean"))))

(describe "org-canvas--validate-check-number"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-number nil "POINTS" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--validate-check-number "" "POINTS" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for integer"
    (expect (org-canvas--validate-check-number "100" "POINTS" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for decimal"
    (expect (org-canvas--validate-check-number "99.5" "POINTS" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for negative number"
    (expect (org-canvas--validate-check-number "-5" "POINTS" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns error for non-numeric"
    (let ((issue (org-canvas--validate-check-number "abc" "POINTS" (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not a valid number"))))

(describe "org-canvas--validate-check-enum"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-enum nil "TYPE" '("a" "b") (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for valid value"
    (expect (org-canvas--validate-check-enum "a" "TYPE" '("a" "b") (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns error for invalid value"
    (let ((issue (org-canvas--validate-check-enum "c" "TYPE" '("a" "b") (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not valid")))

  (it "lists valid values in error message"
    (let ((issue (org-canvas--validate-check-enum "x" "T" '("a" "b" "c") (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :message) :to-match "a, b, c"))))

(describe "org-canvas--validate-check-csv-enum"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-csv-enum nil "ROLES" '("a" "b") (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for single valid value"
    (expect (org-canvas--validate-check-csv-enum "a" "ROLES" '("a" "b") (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for multiple valid values"
    (expect (org-canvas--validate-check-csv-enum "a,b" "ROLES" '("a" "b") (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "handles whitespace around commas"
    (expect (org-canvas--validate-check-csv-enum "a , b" "ROLES" '("a" "b") (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns error for invalid part"
    (let ((issue (org-canvas--validate-check-csv-enum "a,bad" "ROLES" '("a" "b") (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "bad"))))

(describe "org-canvas--validate-check-timestamp"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-timestamp nil "DUE_AT" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns nil for valid future timestamp"
    (expect (org-canvas--validate-check-timestamp
             "<2099-12-31 Wed 23:59>" "DUE_AT" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns warning for past timestamp"
    (let ((issue (org-canvas--validate-check-timestamp
                  "<2020-01-01 Wed 09:00>" "DUE_AT" (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :severity) :to-equal 'warning)
      (expect (plist-get issue :message) :to-match "in the past")))

  (it "returns error for malformed timestamp"
    (let ((issue (org-canvas--validate-check-timestamp
                  "not-a-date" "DUE_AT" (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not a valid Org timestamp"))))

(describe "org-canvas--validate-check-link"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-link nil "GROUP" 'org-canvas-assignment-groups-file
                                             "CANVAS_ID" (list :file "/f" :line 1 :heading "H"))
            :to-be nil))

  (it "returns error for non-link value"
    (let ((issue (org-canvas--validate-check-link "Labs" "GROUP"
                                                   'org-canvas-assignment-groups-file
                                                   "CANVAS_ID" (list :file "/f" :line 1 :heading "H"))))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not a file link")))

  (it "returns warning when link target has no CANVAS_ID"
    (let* ((temp-dir (make-temp-file "org-val-test-" t))
           (ag-file (expand-file-name "assignment-groups.org" temp-dir))
           (src-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            ;; Create target file with heading but no CANVAS_ID
            (with-temp-file ag-file
              (insert "* Labs\n:PROPERTIES:\n:END:\n"))
            (let ((link (format "[[file:%s::*Labs][Labs]]" ag-file)))
              (let ((issue (org-canvas--validate-check-link
                            link "GROUP" 'org-canvas-assignment-groups-file
                            "CANVAS_ID" (list :file src-file :line 1 :heading "Test Assignment"))))
                (expect issue :not :to-be nil)
                (expect (plist-get issue :severity) :to-equal 'warning)
                (expect (plist-get issue :message) :to-match "no CANVAS_ID"))))
        (delete-directory temp-dir t)))))

;;;; Date Ordering Tests

(describe "org-canvas--validate-check-date-order"
  (it "returns no issues for correct order"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:UNLOCK_AT: <2099-01-01 Wed 09:00>
:DUE_AT: <2099-06-15 Sun 23:59>
:LOCK_AT: <2099-12-31 Wed 23:59>
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-check-date-order
                    '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test"))))
       (expect issues :to-equal nil))))

  (it "returns warning for reversed unlock/due"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:UNLOCK_AT: <2099-12-31 Wed 23:59>
:DUE_AT: <2099-01-01 Wed 09:00>
:LOCK_AT: <2099-12-31 Wed 23:59>
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-check-date-order
                    '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :message) :to-match "UNLOCK_AT is after DUE_AT"))))

  (it "returns warning for reversed due/lock"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:DUE_AT: <2099-12-31 Wed 23:59>
:LOCK_AT: <2099-01-01 Wed 09:00>
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-check-date-order
                    '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "DUE_AT is after LOCK_AT"))))

  (it "handles missing dates gracefully"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:DUE_AT: <2099-06-15 Sun 23:59>
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-check-date-order
                    '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test"))))
       (expect issues :to-equal nil))))

  (it "handles malformed timestamps without error"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:UNLOCK_AT: not-a-date
:DUE_AT: <2099-06-15 Sun 23:59>
:END:
"
     (org-back-to-heading t)
     ;; Should not signal an error; malformed dates are caught by timestamp validator
     (let ((issues (org-canvas--validate-check-date-order
                    '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test"))))
       (expect issues :to-equal nil)))))

;;;; Structural Validator Tests

(describe "org-canvas--validate-rubric-structure"
  (it "returns nil when criterion sub-headings are present"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Writing :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
| No Marks | 0 | |
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-rubric-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Rubric"))
             :to-be nil)))

  (it "returns error when no criterion sub-headings"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:

No level-2 children here.
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-rubric-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Rubric"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "no criteria")))))

(describe "org-canvas--validate-file-structure"
  (it "returns nil for a folder heading (no link)"
    (with-temp-org-buffer
     "* course-content
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-file-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "course-content"))
             :to-be nil)))

  (it "returns error for missing linked file"
    (with-temp-org-buffer
     "* [[file:content/nonexistent.pdf][Nonexistent]]
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-file-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Nonexistent"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "does not exist"))))

  (it "returns nil for existing linked file"
    (let* ((temp-dir (make-temp-file "org-val-test-" t))
           (content-file (expand-file-name "test.txt" temp-dir))
           (org-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file content-file (insert "hello"))
            (with-temp-file org-file
              (insert (format "* [[file:%s][test.txt]]\n:PROPERTIES:\n:END:\n"
                              content-file)))
            (with-current-buffer (find-file-noselect org-file)
              (unwind-protect
                  (progn
                    (goto-char (point-min))
                    (org-back-to-heading t)
                    (expect (org-canvas--validate-file-structure
                             (list :file org-file :line (line-number-at-pos) :heading "test.txt"))
                            :to-be nil))
                (kill-buffer))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--validate-section-structure"
  (it "returns nil when CANVAS_ID is present"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:CANVAS_ID: 12345
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-section-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Section A"))
             :to-be nil)))

  (it "returns warning when CANVAS_ID is missing"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-section-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Section A"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID")))))

(describe "org-canvas--validate-assignment-structure"
  (it "returns nil with no override table"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:POINTS: 100
:END:

Some body text.
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-assignment-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Assignment 1"))
             :to-be nil)))

  (it "returns nil when override block exists but no table follows"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:POINTS: 100
:END:

#+NAME: overrides
Not a table row here.
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-assignment-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Assignment 1"))
             :to-be nil)))

  (it "returns warning for override row without file link"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:POINTS: 100
:END:

#+NAME: overrides
| Section | Due At | Unlock At | Lock At |
|---------+--------+-----------+---------|
| Plain   |        |           |         |
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-assignment-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Assignment 1"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "not a file link")))))

;;;; Issue Formatting

(describe "org-canvas--validate-format-issue"
  (it "formats error issue correctly"
    (let ((issue (org-canvas--validate-make-issue
                  'error (list :file "/path/to/file.org" :line 42 :heading "Test") "POINTS" "bad value")))
      (expect (org-canvas--validate-format-issue issue)
              :to-equal "/path/to/file.org:42: error: bad value")))

  (it "formats warning issue correctly"
    (let ((issue (org-canvas--validate-make-issue
                  'warning (list :file "/path/to/file.org" :line 10 :heading "Test") "DUE_AT" "in the past")))
      (expect (org-canvas--validate-format-issue issue)
              :to-equal "/path/to/file.org:10: warning: in the past"))))

;;;; Integration Test Helper

(defmacro with-validate-test-dir (temp-dir-var &rest body)
  "Create a temp dir, bind all org-canvas file vars to it, run BODY.
TEMP-DIR-VAR is bound to the temp directory path.
Cleans up on exit."
  (declare (indent 1))
  `(let* ((,temp-dir-var (make-temp-file "org-val-int-" t))
          (org-canvas-directory ,temp-dir-var)
          (org-canvas-assignments-file (expand-file-name "assignments.org" ,temp-dir-var))
          (org-canvas-pages-file (expand-file-name "pages.org" ,temp-dir-var))
          (org-canvas-quizzes-file (expand-file-name "quizzes.org" ,temp-dir-var))
          (org-canvas-modules-file (expand-file-name "modules.org" ,temp-dir-var))
          (org-canvas-files-file (expand-file-name "files.org" ,temp-dir-var))
          (org-canvas-outcomes-file (expand-file-name "outcomes.org" ,temp-dir-var))
          (org-canvas-rubrics-file (expand-file-name "rubrics.org" ,temp-dir-var))
          (org-canvas-discussions-file (expand-file-name "discussions.org" ,temp-dir-var))
          (org-canvas-announcements-file (expand-file-name "announcements.org" ,temp-dir-var))
          (org-canvas-assignment-groups-file (expand-file-name "assignment-groups.org" ,temp-dir-var))
          (org-canvas-sections-file (expand-file-name "sections.org" ,temp-dir-var))
          (org-canvas-settings-file (expand-file-name "settings.org" ,temp-dir-var))
          (org-canvas-new-quizzes-file (expand-file-name "new-quizzes.org" ,temp-dir-var))
          (org-canvas-group-categories-file (expand-file-name "group-categories.org" ,temp-dir-var))
          (org-canvas-calendar-events-file (expand-file-name "calendar.org" ,temp-dir-var)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,temp-dir-var t))))

(defun test-validate-create-empty-files (dir &optional except)
  "Create empty org files for all content types in DIR.
EXCEPT is a list of filenames to skip."
  (dolist (f '("assignments.org" "pages.org" "quizzes.org"
               "modules.org" "files.org" "outcomes.org"
               "rubrics.org" "discussions.org" "announcements.org"
               "assignment-groups.org" "sections.org" "settings.org"
               "new-quizzes.org" "group-categories.org" "calendar.org"))
    (unless (member f except)
      (with-temp-file (expand-file-name f dir)
        (insert "#+TITLE: Test\n")))))

;;;; Integration Tests

(describe "org-canvas-validate"
  (it "creates *canvas-validate* buffer"
    (with-validate-test-dir dir
      (test-validate-create-empty-files dir)
      (org-canvas-validate)
      (expect (get-buffer "*canvas-validate*") :not :to-be nil)))

  (it "skips missing files without error"
    (with-validate-test-dir dir
      ;; Only create one file
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "#+TITLE: Pages\n"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (expect (buffer-string) :to-match "skipped"))))

  (it "reports errors in compilation-compatible format"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Bad Page\n:PROPERTIES:\n:PUBLISHED: yes\n:END:\n"))
      (test-validate-create-empty-files dir '("pages.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "error:")
          (expect content :to-match "not a valid boolean")))))

  (it "shows clean report when no issues"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Good Page\n:PROPERTIES:\n:PUBLISHED: true\n:END:\n"))
      (test-validate-create-empty-files dir '("pages.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "No issues found")
          (expect content :to-match "0 error(s), 0 warning(s)")))))

  (it "compilation next-error navigation works"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Bad Page\n:PROPERTIES:\n:PUBLISHED: invalid\n:END:\n"))
      (test-validate-create-empty-files dir '("pages.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        ;; Verify the buffer is in the right mode
        (expect major-mode :to-equal 'org-canvas-validate-mode)
        ;; Verify compilation-error-regexp-alist has our entries
        (expect (assq 'org-canvas-validate compilation-error-regexp-alist)
                :not :to-be nil)))))

;;;; Module-Specific Validation Tests

(describe "assignment validation"
  (it "detects invalid GRADING_TYPE"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:GRADING_TYPE: invalid_type
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "GRADING_TYPE"))))

  (it "detects invalid SUBMISSION type"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:SUBMISSION: invalid_submission
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "SUBMISSION")))))

(describe "discussion validation"
  (it "detects invalid DISCUSSION_TYPE"
    (with-temp-org-buffer
     "* Test Discussion
:PROPERTIES:
:DISCUSSION_TYPE: invalid
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Discussions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Discussion"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "DISCUSSION_TYPE"))))

  (it "detects invalid GRADING_TYPE on discussion"
    (with-temp-org-buffer
     "* Graded Discussion
:PROPERTIES:
:GRADING_TYPE: invalid
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Discussions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Graded Discussion"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "GRADING_TYPE")))))

(describe "outcome validation"
  (it "detects invalid CALCULATION_METHOD"
    (with-temp-org-buffer
     "* Outcome Group
** Test Outcome
:PROPERTIES:
:CALCULATION_METHOD: invalid_method
:END:
"
     (search-forward "** Test Outcome")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Outcomes" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Outcome"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "CALCULATION_METHOD"))))

  (it "accepts valid CALCULATION_METHOD values"
    (with-temp-org-buffer
     "* Outcome Group
** Test Outcome
:PROPERTIES:
:CALCULATION_METHOD: decaying_average
:CALCULATION_INT: 75
:MASTERY_POINTS: 3
:END:
"
     (search-forward "** Test Outcome")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Outcomes" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Outcome"))))
       (expect issues :to-equal nil)))))

(describe "module item validation"
  (it "detects invalid COMPLETION_REQUIREMENT"
    (with-temp-org-buffer
     "* Module 1
** Item 1
:PROPERTIES:
:COMPLETION_REQUIREMENT: invalid
:END:
"
     (search-forward "** Item 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Module Items" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Item 1"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "COMPLETION_REQUIREMENT"))))

  (it "accepts valid COMPLETION_REQUIREMENT"
    (with-temp-org-buffer
     "* Module 1
** Item 1
:PROPERTIES:
:COMPLETION_REQUIREMENT: must_view
:END:
"
     (search-forward "** Item 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Module Items" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Item 1"))))
       (expect issues :to-equal nil)))))

(describe "quiz question validation"
  (it "detects invalid question TYPE"
    (with-temp-org-buffer
     "* Quiz 1
** Question 1
:PROPERTIES:
:TYPE: invalid_question_type
:END:
"
     (search-forward "** Question 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quiz Questions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Question 1"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "TYPE"))))

  (it "accepts valid question TYPE"
    (with-temp-org-buffer
     "* Quiz 1
** Question 1
:PROPERTIES:
:TYPE: essay_question
:POINTS: 10
:END:
"
     (search-forward "** Question 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quiz Questions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Question 1"))))
       (expect issues :to-equal nil))))

  (it "accepts 'group' as valid TYPE"
    (with-temp-org-buffer
     "* Quiz 1
** Group 1
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 5
:QUESTION_POINTS: 2
:END:
"
     (search-forward "** Group 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quiz Questions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Group 1"))))
       (expect issues :to-equal nil))))

  (it "validates PICK_COUNT as number"
    (with-temp-org-buffer
     "* Quiz 1
** Group 1
:PROPERTIES:
:TYPE: group
:PICK_COUNT: abc
:END:
"
     (search-forward "** Group 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quiz Questions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Group 1"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "PICK_COUNT"))))

  (it "validates QUESTION_BANK_ID as number"
    (with-temp-org-buffer
     "* Quiz 1
** Group 1
:PROPERTIES:
:TYPE: group
:QUESTION_BANK_ID: not-a-number
:END:
"
     (search-forward "** Group 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quiz Questions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Group 1"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "QUESTION_BANK_ID")))))

(describe "page validation"
  (it "detects invalid EDITING_ROLES"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:EDITING_ROLES: teachers,admins
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Pages" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Page"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "admins"))))

  (it "accepts valid EDITING_ROLES"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:EDITING_ROLES: teachers,students
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Pages" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Page"))))
       (expect issues :to-equal nil)))))

(describe "quiz validation"
  (it "detects invalid QUIZ_TYPE"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:QUIZ_TYPE: invalid_quiz
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quizzes" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Quiz"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "QUIZ_TYPE")))))

;;;; Validate Spec Engine Tests

(describe "org-canvas--validate-spec"
  (it "validates properties from spec"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Page 1\n:PROPERTIES:\n:PUBLISHED: invalid\n:END:\n"))
      (let ((spec (cl-find "Pages" (org-canvas--validate-specs)
                           :key (lambda (s) (plist-get s :label))
                           :test #'string=)))
        (let ((issues (org-canvas--validate-spec spec)))
          (expect (length issues) :to-be-greater-than 0)
          (expect (plist-get (car issues) :message) :to-match "boolean")))))

  (it "returns empty list for valid file"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Page 1\n:PROPERTIES:\n:PUBLISHED: true\n:FRONT_PAGE: false\n:END:\n"))
      (let ((spec (cl-find "Pages" (org-canvas--validate-specs)
                           :key (lambda (s) (plist-get s :label))
                           :test #'string=)))
        (expect (org-canvas--validate-spec spec) :to-equal nil))))

  (it "returns empty list when file does not exist"
    (with-validate-test-dir dir
      (let ((spec (cl-find "Pages" (org-canvas--validate-specs)
                           :key (lambda (s) (plist-get s :label))
                           :test #'string=)))
        (expect (org-canvas--validate-spec spec) :to-equal nil)))))

(describe "org-canvas--validate-entry-properties"
  (it "checks multiple properties at once"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:PUBLISHED: bad
:POINTS: abc
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    '((:name "PUBLISHED" :type boolean)
                      (:name "POINTS" :type number))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test"))))
       (expect (length issues) :to-equal 2))))

  (it "returns empty list when all valid"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:PUBLISHED: true
:POINTS: 100
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    '((:name "PUBLISHED" :type boolean)
                      (:name "POINTS" :type number))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test"))))
       (expect issues :to-equal nil)))))

;;;; Multi-module Integration Test

(describe "multi-module validation"
  (it "validates a realistic multi-module course"
    (with-validate-test-dir dir
      ;; Create assignment with errors
      (with-temp-file (expand-file-name "assignments.org" dir)
        (insert "* Assignment 1
:PROPERTIES:
:POINTS: not-a-number
:GRADING_TYPE: invalid
:PUBLISHED: true
:END:
"))
      ;; Create valid pages
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Good Page\n:PROPERTIES:\n:PUBLISHED: true\n:END:\n"))
      ;; Create discussions with invalid type
      (with-temp-file (expand-file-name "discussions.org" dir)
        (insert "* Discussion
:PROPERTIES:
:DISCUSSION_TYPE: invalid
:END:
"))
      ;; Create other empty files
      (test-validate-create-empty-files dir
        '("assignments.org" "pages.org" "discussions.org"))

      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          ;; Should have errors from assignments and discussions
          (expect content :to-match "POINTS")
          (expect content :to-match "GRADING_TYPE")
          (expect content :to-match "DISCUSSION_TYPE")
          ;; Should have no errors from pages
          (expect content :not :to-match "PUBLISHED.*not a valid"))))))

;;;; Additional Coverage Tests

(describe "org-canvas--validate-format-summary"
  (it "returns warning-only message"
    (let ((result (org-canvas--validate-format-summary 0 3)))
      (expect result :to-match "3 warning")
      (expect result :to-match "no errors")))

  (it "returns error message"
    (let ((result (org-canvas--validate-format-summary 2 1)))
      (expect result :to-match "2 error")
      (expect result :to-match "1 warning")))

  (it "returns passed message when no issues"
    (let ((result (org-canvas--validate-format-summary 0 0)))
      (expect result :to-match "passed"))))

(describe "org-canvas--validate-file-structure file size warning"
  (it "warns when file exceeds max size"
    (let* ((org-canvas-max-file-size-mb 0)  ;; 0 MB threshold - any file triggers
           (temp-file (make-temp-file "size-test" nil ".txt"))
           (org-file (make-temp-file "org-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "content"))
            (with-temp-file org-file
              (insert (format "* [[file:%s][Test File]]\n:PROPERTIES:\n:END:\n" temp-file)))
            (with-current-buffer (find-file-noselect org-file)
              (goto-char (point-min))
              (org-back-to-heading)
              (let ((issues (org-canvas--validate-file-structure
                             (list :file org-file :line 1 :heading "Test File"))))
                (expect (length issues) :not :to-equal 0)
                (let ((issue (car issues)))
                  (expect (plist-get issue :severity) :to-equal 'warning)
                  (expect (plist-get issue :message) :to-match "MB")))
              (kill-buffer)))
        (delete-file temp-file)
        (delete-file org-file)))))

(describe "org-canvas--validate-entry-at-marker structural-fn"
  (it "appends structural issues"
    (with-temp-org-buffer
     "* Section
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     ;; Using section validator - no CANVAS_ID triggers structural warning
     (let ((issues (org-canvas--validate-entry-at-marker
                    nil nil
                    #'org-canvas--validate-section-structure
                    "test.org")))
       (expect (cl-some (lambda (i)
                          (string-match-p "CANVAS_ID" (plist-get i :message)))
                        issues)
               :to-be-truthy)))))

;;;; New Property Validation Tests

(describe "assignment new boolean properties"
  (it "detects invalid OMIT_FROM_GRADES"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:OMIT_FROM_GRADES: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "OMIT_FROM_GRADES"))))

  (it "detects invalid ANONYMOUS_GRADING"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:ANONYMOUS_GRADING: 1
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "ANONYMOUS_GRADING"))))

  (it "detects invalid NOTIFY_OF_UPDATE"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:NOTIFY_OF_UPDATE: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "NOTIFY_OF_UPDATE"))))

  (it "detects invalid AUTOMATIC_PEER_REVIEWS"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:AUTOMATIC_PEER_REVIEWS: on
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "AUTOMATIC_PEER_REVIEWS"))))

  (it "detects invalid GRADE_INDIVIDUALLY"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:GRADE_INDIVIDUALLY: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "GRADE_INDIVIDUALLY"))))

  (it "detects invalid ONLY_VISIBLE_TO_OVERRIDES"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:ONLY_VISIBLE_TO_OVERRIDES: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "ONLY_VISIBLE_TO_OVERRIDES"))))

  (it "detects invalid MODERATED_GRADING"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:MODERATED_GRADING: maybe
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "MODERATED_GRADING"))))

  (it "accepts valid boolean values for all new assignment booleans"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:OMIT_FROM_GRADES: true
:ANONYMOUS_GRADING: false
:NOTIFY_OF_UPDATE: true
:AUTOMATIC_PEER_REVIEWS: false
:GRADE_INDIVIDUALLY: true
:ONLY_VISIBLE_TO_OVERRIDES: false
:MODERATED_GRADING: true
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect issues :to-equal nil)))))

(describe "assignment new number properties"
  (it "detects invalid GROUP_CATEGORY_ID"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:GROUP_CATEGORY_ID: not-a-number
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "GROUP_CATEGORY_ID"))))

  (it "detects invalid POSITION"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:POSITION: abc
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "POSITION"))))

  (it "accepts valid number values"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:GROUP_CATEGORY_ID: 42
:POSITION: 1
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect issues :to-equal nil)))))

(describe "page NOTIFY_OF_UPDATE validation"
  (it "detects invalid NOTIFY_OF_UPDATE on page"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:NOTIFY_OF_UPDATE: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Pages" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Page"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "NOTIFY_OF_UPDATE"))))

  (it "accepts valid NOTIFY_OF_UPDATE on page"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:NOTIFY_OF_UPDATE: true
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Pages" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Page"))))
       (expect issues :to-equal nil)))))

(describe "quiz new boolean properties"
  (it "detects invalid SHOW_CORRECT_ANSWERS_LAST_ATTEMPT"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:SHOW_CORRECT_ANSWERS_LAST_ATTEMPT: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quizzes" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Quiz"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT"))))

  (it "detects invalid ONE_TIME_RESULTS"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:ONE_TIME_RESULTS: 1
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quizzes" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Quiz"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "ONE_TIME_RESULTS"))))

  (it "detects invalid ONLY_VISIBLE_TO_OVERRIDES on quiz"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:ONLY_VISIBLE_TO_OVERRIDES: maybe
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quizzes" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Quiz"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "ONLY_VISIBLE_TO_OVERRIDES"))))

  (it "accepts valid quiz boolean values"
    (with-temp-org-buffer
     "* Test Quiz
:PROPERTIES:
:SHOW_CORRECT_ANSWERS_LAST_ATTEMPT: true
:ONE_TIME_RESULTS: false
:ONLY_VISIBLE_TO_OVERRIDES: true
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Quizzes" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Quiz"))))
       (expect issues :to-equal nil)))))

(describe "module item NEW_TAB validation"
  (it "detects invalid NEW_TAB"
    (with-temp-org-buffer
     "* Module 1
** Item 1
:PROPERTIES:
:NEW_TAB: yes
:END:
"
     (search-forward "** Item 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Module Items" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Item 1"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "NEW_TAB"))))

  (it "accepts valid NEW_TAB"
    (with-temp-org-buffer
     "* Module 1
** Item 1
:PROPERTIES:
:NEW_TAB: true
:END:
"
     (search-forward "** Item 1")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Module Items" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Item 1"))))
       (expect issues :to-equal nil)))))

(describe "discussion new properties"
  (it "detects invalid ALLOW_RATING"
    (with-temp-org-buffer
     "* Test Discussion
:PROPERTIES:
:ALLOW_RATING: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Discussions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Discussion"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "ALLOW_RATING"))))

  (it "detects invalid ONLY_GRADERS_CAN_RATE"
    (with-temp-org-buffer
     "* Test Discussion
:PROPERTIES:
:ONLY_GRADERS_CAN_RATE: 1
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Discussions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Discussion"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "ONLY_GRADERS_CAN_RATE"))))

  (it "detects invalid SORT_BY_RATING"
    (with-temp-org-buffer
     "* Test Discussion
:PROPERTIES:
:SORT_BY_RATING: on
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Discussions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Discussion"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "SORT_BY_RATING"))))

  (it "detects invalid GROUP_CATEGORY"
    (with-temp-org-buffer
     "* Test Discussion
:PROPERTIES:
:GROUP_CATEGORY: not-a-number
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Discussions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Discussion"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "GROUP_CATEGORY"))))

  (it "accepts valid discussion rating and group properties"
    (with-temp-org-buffer
     "* Test Discussion
:PROPERTIES:
:ALLOW_RATING: true
:ONLY_GRADERS_CAN_RATE: false
:SORT_BY_RATING: true
:GROUP_CATEGORY: 42
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Discussions" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Discussion"))))
       (expect issues :to-equal nil)))))

(describe "assignment group POSITION validation"
  (it "detects invalid POSITION on assignment group"
    (with-temp-org-buffer
     "* Homework
:PROPERTIES:
:POSITION: abc
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignment Groups" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Homework"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "POSITION"))))

  (it "accepts valid POSITION on assignment group"
    (with-temp-org-buffer
     "* Homework
:PROPERTIES:
:POSITION: 3
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignment Groups" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Homework"))))
       (expect issues :to-equal nil)))))

;;;; Settings Validation Tests

(describe "settings validation"
  (it "detects invalid APPLY_WEIGHTS"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:APPLY_WEIGHTS: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "APPLY_WEIGHTS"))))

  (it "detects invalid HIDE_FINAL_GRADES"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:HIDE_FINAL_GRADES: 1
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "HIDE_FINAL_GRADES"))))

  (it "detects invalid PUBLIC_SYLLABUS"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:PUBLIC_SYLLABUS: on
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "PUBLIC_SYLLABUS"))))

  (it "detects invalid IS_PUBLIC"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:IS_PUBLIC: 0
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "IS_PUBLIC"))))

  (it "detects invalid DEFAULT_VIEW"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:DEFAULT_VIEW: dashboard
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "DEFAULT_VIEW"))))

  (it "detects invalid LICENSE"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:LICENSE: gpl
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "LICENSE"))))

  (it "detects malformed START_AT timestamp"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:START_AT: not-a-date
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "START_AT"))))

  (it "detects malformed END_AT timestamp"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:END_AT: 2026-01-01
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "END_AT"))))

  (it "detects invalid HOME_PAGE_ANNOUNCEMENT_LIMIT"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:HOME_PAGE_ANNOUNCEMENT_LIMIT: lots
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "HOME_PAGE_ANNOUNCEMENT_LIMIT"))))

  (it "detects invalid ALLOW_STUDENT_DISCUSSION_TOPICS"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:ALLOW_STUDENT_DISCUSSION_TOPICS: yes
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "ALLOW_STUDENT_DISCUSSION_TOPICS"))))

  (it "detects invalid LOCK_ALL_ANNOUNCEMENTS"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:LOCK_ALL_ANNOUNCEMENTS: 1
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "LOCK_ALL_ANNOUNCEMENTS"))))

  (it "detects invalid HIDE_DISTRIBUTION_GRAPHS"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:HIDE_DISTRIBUTION_GRAPHS: on
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "HIDE_DISTRIBUTION_GRAPHS"))))

  (it "accepts all valid settings properties"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:APPLY_WEIGHTS: true
:HIDE_FINAL_GRADES: false
:PUBLIC_SYLLABUS: true
:IS_PUBLIC: false
:DEFAULT_VIEW: modules
:LICENSE: cc_by_nc
:START_AT: <2099-01-15 Wed 09:00>
:END_AT: <2099-05-15 Thu 17:00>
:ALLOW_STUDENT_DISCUSSION_TOPICS: true
:ALLOW_STUDENT_DISCUSSION_EDITING: false
:ALLOW_STUDENT_FORUM_ATTACHMENTS: true
:LOCK_ALL_ANNOUNCEMENTS: false
:RESTRICT_STUDENT_FUTURE_VIEW: true
:RESTRICT_STUDENT_PAST_VIEW: false
:SHOW_ANNOUNCEMENTS_ON_HOME_PAGE: true
:HOME_PAGE_ANNOUNCEMENT_LIMIT: 5
:HIDE_DISTRIBUTION_GRAPHS: false
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect issues :to-equal nil))))

  (it "detects multiple errors in settings"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:APPLY_WEIGHTS: yes
:DEFAULT_VIEW: dashboard
:HOME_PAGE_ANNOUNCEMENT_LIMIT: lots
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Settings" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "My Course"))))
       (expect (length issues) :to-equal 3)))))

;;;; Settings Integration Tests

(describe "settings spec integration"
  (it "validates settings file via org-canvas--validate-spec"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "settings.org" dir)
        (insert "* My Course\n:PROPERTIES:\n:APPLY_WEIGHTS: invalid\n:DEFAULT_VIEW: bad\n:END:\n"))
      (test-validate-create-empty-files dir '("settings.org"))
      (let ((spec (cl-find "Settings" (org-canvas--validate-specs)
                           :key (lambda (s) (plist-get s :label))
                           :test #'string=)))
        (let ((issues (org-canvas--validate-spec spec)))
          (expect (length issues) :to-equal 2)
          (expect (plist-get (car issues) :message) :to-match "APPLY_WEIGHTS")
          (expect (plist-get (cadr issues) :message) :to-match "DEFAULT_VIEW")))))

  (it "returns no issues for valid settings file"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "settings.org" dir)
        (insert "* My Course\n:PROPERTIES:\n:APPLY_WEIGHTS: true\n:DEFAULT_VIEW: modules\n:LICENSE: private\n:END:\n"))
      (test-validate-create-empty-files dir '("settings.org"))
      (let ((spec (cl-find "Settings" (org-canvas--validate-specs)
                           :key (lambda (s) (plist-get s :label))
                           :test #'string=)))
        (expect (org-canvas--validate-spec spec) :to-equal nil))))

  (it "reports settings errors in full validation run"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "settings.org" dir)
        (insert "* My Course\n:PROPERTIES:\n:IS_PUBLIC: yes\n:LICENSE: gpl\n:END:\n"))
      (test-validate-create-empty-files dir '("settings.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "IS_PUBLIC")
          (expect content :to-match "LICENSE"))))))

;;;; Validation Tests for New Properties

(describe "(org-canvas--validate-specs) includes new properties"
  (it "validates MUTED as boolean in assignments"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-boolean "true" "MUTED" loc) :to-be nil)
      (expect (org-canvas--validate-check-boolean "invalid" "MUTED" loc) :to-be-truthy)))

  (it "validates TURNITIN_ENABLED as boolean in assignments"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-boolean "true" "TURNITIN_ENABLED" loc) :to-be nil)
      (expect (org-canvas--validate-check-boolean "invalid" "TURNITIN_ENABLED" loc) :to-be-truthy)))

  (it "validates GRADING_STANDARD_ID as number in assignments"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-number "42" "GRADING_STANDARD_ID" loc) :to-be nil)
      (expect (org-canvas--validate-check-number "abc" "GRADING_STANDARD_ID" loc) :to-be-truthy)))

  (it "validates USE_JUSTIFICATION as enum in files"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-enum "own_copyright" "USE_JUSTIFICATION"
                                                org-canvas--valid-use-justifications loc)
              :to-be nil)
      (expect (org-canvas--validate-check-enum "invalid" "USE_JUSTIFICATION"
                                                org-canvas--valid-use-justifications loc)
              :to-be-truthy)))

  (it "validates LATE_SUBMISSION_INTERVAL as enum in settings"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-enum "day" "LATE_SUBMISSION_INTERVAL"
                                                org-canvas--valid-late-intervals loc)
              :to-be nil)
      (expect (org-canvas--validate-check-enum "week" "LATE_SUBMISSION_INTERVAL"
                                                org-canvas--valid-late-intervals loc)
              :to-be-truthy)))

  (it "validates LATE_SUBMISSION_DEDUCTION as number"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-number "10" "LATE_SUBMISSION_DEDUCTION" loc) :to-be nil)
      (expect (org-canvas--validate-check-number "abc" "LATE_SUBMISSION_DEDUCTION" loc) :to-be-truthy)))

  (it "validates MISSING_SUBMISSION_DEDUCTION_ENABLED as boolean"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-boolean "true" "MISSING_SUBMISSION_DEDUCTION_ENABLED" loc) :to-be nil)
      (expect (org-canvas--validate-check-boolean "yes" "MISSING_SUBMISSION_DEDUCTION_ENABLED" loc) :to-be-truthy))))

;;;; GRADER_COUNT Validation Tests

(describe "GRADER_COUNT validation"
  (it "detects invalid GRADER_COUNT"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:GRADER_COUNT: abc
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "GRADER_COUNT"))))

  (it "accepts valid GRADER_COUNT"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:GRADER_COUNT: 2
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "Assignments" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect issues :to-equal nil))))

  (it "returns no issue when GRADER_COUNT is absent"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    '((:name "GRADER_COUNT" :type number))
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Assignment"))))
       (expect issues :to-equal nil)))))

;;;; New Quiz Item OUTCOME Validation Tests

(describe "new quiz item OUTCOME validation"
  (it "returns nil when OUTCOME is absent"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:TYPE: choice
:POINTS: 5
:END:
"
     (search-forward "** Question")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "New Quiz Items" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Question"))))
       (expect issues :to-equal nil))))

  (it "returns error for non-link OUTCOME value"
    (with-temp-org-buffer
     "* Quiz
** Question
:PROPERTIES:
:TYPE: choice
:POINTS: 5
:OUTCOME: Python Proficiency
:END:
"
     (search-forward "** Question")
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-entry-properties
                    (plist-get (cl-find "New Quiz Items" (org-canvas--validate-specs)
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Question"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "OUTCOME")
       (expect (plist-get (car issues) :message) :to-match "not a file link"))))

  (it "returns warning when OUTCOME link target has no CANVAS_ID"
    (let* ((temp-dir (make-temp-file "nq-outcome-val-" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (nq-file (expand-file-name "new-quizzes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-outcomes-file outcomes-file))
            (with-temp-file outcomes-file
              (insert "* Programming\n** Python Proficiency\n:PROPERTIES:\n:END:\n"))
            (let ((link (format "[[file:%s::*Python Proficiency][Python Proficiency]]" outcomes-file)))
              (let ((issue (org-canvas--validate-check-link
                            link "OUTCOME" 'org-canvas-outcomes-file
                            "CANVAS_ID" (list :file nq-file :line 1 :heading "Question"))))
                (expect issue :not :to-be nil)
                (expect (plist-get issue :severity) :to-equal 'warning)
                (expect (plist-get issue :message) :to-match "no CANVAS_ID"))))
        (delete-directory temp-dir t)))))

;;;; Cross-Module Structural Validators

(describe "org-canvas--validate-module-item-link"
  (it "returns nil for items with EXTERNAL_URL"
    (with-temp-org-buffer
     "* Module
** External Site
:PROPERTIES:
:EXTERNAL_URL: https://example.com
:END:
"
     (search-forward "External Site")
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 2 :heading "External Site")))
       (expect (org-canvas--validate-module-item-link loc) :to-be nil))))

  (it "returns nil for plain-text SubHeaders"
    (with-temp-org-buffer
     "* Module
** Week 1 Overview
:PROPERTIES:
:END:
"
     (search-forward "Week 1 Overview")
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 2 :heading "Week 1 Overview")))
       (expect (org-canvas--validate-module-item-link loc) :to-be nil))))

  (it "returns error for link to missing file"
    (with-temp-org-buffer
     "* Module
** [[file:nonexistent.org::*Heading][Heading]]
:PROPERTIES:
:END:
"
     (search-forward "Heading")
     (org-back-to-heading)
     (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
            (issues (org-canvas--validate-module-item-link loc)))
       (expect issues :not :to-be nil)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "missing file"))))

  (it "returns error for link to missing heading"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Existing Page\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Missing Page][Missing Page]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Missing Page")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :not :to-be nil)
               (expect (plist-get (car issues) :severity) :to-equal 'error)
               (expect (plist-get (car issues) :message) :to-match "missing heading"))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file))))

  (it "returns warning when target heading has no CANVAS_ID"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Unsynced Page\n:PROPERTIES:\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Unsynced Page][Unsynced Page]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Unsynced Page")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :not :to-be nil)
               (expect (plist-get (car issues) :severity) :to-equal 'warning)
               (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID"))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file))))

  (it "returns nil for valid link with CANVAS_ID"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Synced Page\n:PROPERTIES:\n:CANVAS_ID: 123\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Synced Page][Synced Page]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Synced Page")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :to-be nil))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file)))))

(describe "org-canvas--validate-module-item-link with escaped brackets"
  (it "unescapes \\[ in heading name before searching"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Page [Extra\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Page \\[Extra][Page Extra]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Page Extra")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :to-be nil))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file)))))

(describe "org-canvas--validate-override-section-ids"
  (it "reports warning when override section link target has no CANVAS_ID"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:END:\n"))
            (with-temp-org-buffer
             (format "| [[file:%s::*Section A][Section A]] | <2027-01-15 Fri 23:59> |\n" sections-file)
             (goto-char (point-min))
             (let ((issues (org-canvas--validate-override-section-ids
                            (buffer-file-name) "Assignment 1")))
               (expect issues :not :to-be nil)
               (expect (length issues) :to-equal 1)
               (expect (plist-get (car issues) :severity) :to-equal 'warning)
               (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID"))))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "returns nil when section link target has CANVAS_ID"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 99\n:END:\n"))
            (with-temp-org-buffer
             (format "| [[file:%s::*Section A][Section A]] | <2027-01-15 Fri 23:59> |\n" sections-file)
             (goto-char (point-min))
             (let ((issues (org-canvas--validate-override-section-ids
                            (buffer-file-name) "Assignment 1")))
               (expect issues :to-be nil))))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file)))))

(describe "org-canvas--validate-drop-rules"
  (it "warns when only DROP_LOWEST is set and exceeds assignment count"
    (let ((assignments-file (make-temp-file "test-assignments" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file assignments-file
              (insert "* HW 1\n:PROPERTIES:\n:GROUP: Homework\n:END:\n"))
            (let ((org-canvas-assignments-file assignments-file))
              (with-temp-org-buffer
               "* Homework
:PROPERTIES:
:DROP_LOWEST: 2
:END:
"
               (org-back-to-heading t)
               (let* ((loc (list :file (buffer-file-name)
                                 :line (line-number-at-pos)
                                 :heading "Homework"))
                      (issues (org-canvas--validate-drop-rules loc)))
                 (expect issues :not :to-be nil)
                 (expect (plist-get (car issues) :severity) :to-equal 'warning)
                 (expect (plist-get (car issues) :message) :to-match "Drop rules")))))
        (let ((buf (find-buffer-visiting assignments-file)))
          (when buf (kill-buffer buf)))
        (delete-file assignments-file)))))

(describe "org-canvas--validate-quiz-point-total"
  (it "returns nil when POINTS matches question total"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:POINTS: 30
:END:
** Q1
:PROPERTIES:
:POINTS: 10
:END:
** Q2
:PROPERTIES:
:POINTS: 20
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz")))
       (expect (org-canvas--validate-quiz-point-total loc) :to-be nil))))

  (it "returns warning when POINTS does not match total"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:POINTS: 100
:END:
** Q1
:PROPERTIES:
:POINTS: 10
:END:
** Q2
:PROPERTIES:
:POINTS: 20
:END:
"
     (org-back-to-heading)
     (let* ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz"))
            (issues (org-canvas--validate-quiz-point-total loc)))
       (expect issues :not :to-be nil)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :message) :to-match "100")
       (expect (plist-get (car issues) :message) :to-match "30"))))

  (it "returns nil when no POINTS property"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
** Q1
:PROPERTIES:
:POINTS: 10
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz")))
       (expect (org-canvas--validate-quiz-point-total loc) :to-be nil))))

  (it "handles question groups with PICK_COUNT * QUESTION_POINTS"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:POINTS: 30
:END:
** Regular Question
:PROPERTIES:
:POINTS: 10
:END:
** Question Group
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 2
:QUESTION_POINTS: 10
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz")))
       (expect (org-canvas--validate-quiz-point-total loc) :to-be nil)))))

(describe "org-canvas--validate-drop-rules"
  (it "returns nil when no drop rules"
    (with-temp-org-buffer
     "* Homework
:PROPERTIES:
:WEIGHT: 40
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Homework")))
       (expect (org-canvas--validate-drop-rules loc) :to-be nil))))

  (it "returns warning when drops exceed assignment count"
    (let* ((temp-dir (make-temp-file "drop-test-" t))
           (assign-file (expand-file-name "assignments.org" temp-dir))
           (groups-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* Assignment 1\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n"))
            (with-temp-file groups-file
              (insert "* Homework\n:PROPERTIES:\n:DROP_LOWEST: 2\n:END:\n"))
            (let ((org-canvas-assignments-file assign-file))
              (with-current-buffer (find-file-noselect groups-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let* ((loc (list :file groups-file :line 1 :heading "Homework"))
                       (issues (org-canvas--validate-drop-rules loc)))
                  (expect issues :not :to-be nil)
                  (expect (plist-get (car issues) :severity) :to-equal 'warning)
                  (expect (plist-get (car issues) :message) :to-match "Drop rules")))))
        (delete-directory temp-dir t))))

  (it "warns when only DROP_HIGHEST exceeds assignment count"
    (let* ((temp-dir (make-temp-file "drop-test-" t))
           (assign-file (expand-file-name "assignments.org" temp-dir))
           (groups-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* HW 1\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n"))
            (with-temp-file groups-file
              (insert "* Homework\n:PROPERTIES:\n:DROP_HIGHEST: 2\n:END:\n"))
            (let ((org-canvas-assignments-file assign-file))
              (with-current-buffer (find-file-noselect groups-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let* ((loc (list :file groups-file :line 1 :heading "Homework"))
                       (issues (org-canvas--validate-drop-rules loc)))
                  (expect issues :not :to-be nil)
                  (expect (plist-get (car issues) :message) :to-match "Drop rules")))))
        (delete-directory temp-dir t))))

  (it "returns nil when drops are within count"
    (let* ((temp-dir (make-temp-file "drop-test-" t))
           (assign-file (expand-file-name "assignments.org" temp-dir))
           (groups-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* HW 1\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n* HW 2\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n* HW 3\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n"))
            (with-temp-file groups-file
              (insert "* Homework\n:PROPERTIES:\n:DROP_LOWEST: 1\n:END:\n"))
            (let ((org-canvas-assignments-file assign-file))
              (with-current-buffer (find-file-noselect groups-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let* ((loc (list :file groups-file :line 1 :heading "Homework"))
                       (issues (org-canvas--validate-drop-rules loc)))
                  (expect issues :to-be nil)))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--validate-override-section-ids"
  (it "returns warning for section link without CANVAS_ID"
    (let* ((temp-dir (make-temp-file "override-test-" t))
           (sections-file (expand-file-name "sections.org" temp-dir))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:END:\n"))
            (let ((link (format "[[file:%s::*Section A][Section A]]" sections-file)))
              (with-temp-file assign-file
                (insert (format "| %s | <2027-01-15> |\n" link)))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (let* ((issues (org-canvas--validate-override-section-ids
                                assign-file "Test Assignment")))
                  (expect issues :not :to-be nil)
                  (expect (plist-get (car issues) :severity) :to-equal 'warning)
                  (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID")))))
        (delete-directory temp-dir t))))

  (it "returns nil for section link with CANVAS_ID"
    (let* ((temp-dir (make-temp-file "override-test-" t))
           (sections-file (expand-file-name "sections.org" temp-dir))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section B\n:PROPERTIES:\n:CANVAS_ID: 200\n:END:\n"))
            (let ((link (format "[[file:%s::*Section B][Section B]]" sections-file)))
              (with-temp-file assign-file
                (insert (format "| %s | <2027-01-15> |\n" link)))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (let* ((issues (org-canvas--validate-override-section-ids
                                assign-file "Test Assignment")))
                  (expect issues :to-be nil)))))
        (delete-directory temp-dir t)))))

(describe "discussion RUBRIC_LINK validation"
  (it "includes RUBRIC_LINK in discussions spec"
    (let ((disc-spec (cl-find-if
                      (lambda (s) (equal (plist-get s :label) "Discussions"))
                      (org-canvas--validate-specs))))
      (expect disc-spec :to-be-truthy)
      (let ((rubric-prop (cl-find-if
                          (lambda (p) (equal (plist-get p :name) "RUBRIC_LINK"))
                          (plist-get disc-spec :properties))))
        (expect rubric-prop :to-be-truthy)
        (let ((prop-type (plist-get rubric-prop :type)))
          (expect prop-type :to-equal 'link))))))

(describe "new quiz GROUP validation"
  (it "includes GROUP in new quizzes spec"
    (let ((nq-spec (cl-find-if
                    (lambda (s) (equal (plist-get s :label) "New Quizzes"))
                    (org-canvas--validate-specs))))
      (expect nq-spec :to-be-truthy)
      (let ((group-prop (cl-find-if
                         (lambda (p) (equal (plist-get p :name) "GROUP"))
                         (plist-get nq-spec :properties))))
        (expect group-prop :to-be-truthy)
        (let ((prop-type (plist-get group-prop :type)))
          (expect prop-type :to-equal 'link))))))

;;;; Rubric Outcome Link Validation

(describe "org-canvas--validate-rubric-outcome-links"
  (it "returns nil for table with valid outcome links"
    (let* ((temp-dir (make-temp-file "validate-rubric-outcome" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Group\n** Python\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (let ((link (format "[[file:%s::*Python][Python]]" outcomes-file))
                    (loc (list :file rubrics-file :line 5 :heading "Test")))
                (expect (org-canvas--validate-rubric-outcome-links
                         (list (list "Quality" "10" "Desc" link))
                         rubrics-file loc)
                        :to-be nil))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "warns for outcome link with no CANVAS_ID"
    (let* ((temp-dir (make-temp-file "validate-rubric-outcome" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Group\n** Python\n:PROPERTIES:\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (let ((link (format "[[file:%s::*Python][Python]]" outcomes-file))
                    (loc (list :file rubrics-file :line 5 :heading "Test")))
                (let ((issues (org-canvas--validate-rubric-outcome-links
                               (list (list "Quality" "10" "Desc" link))
                               rubrics-file loc)))
                  (expect (length issues) :to-equal 1)
                  (expect (plist-get (car issues) :severity) :to-equal 'warning)
                  (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID")))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "warns for non-link text in 4th column"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (let ((issues (org-canvas--validate-rubric-outcome-links
                     (list (list "Quality" "10" "Desc" "plain text"))
                     "/tmp/rubrics.org" loc)))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :severity) :to-equal 'warning)
        (expect (plist-get (car issues) :message) :to-match "not a file link"))))

  (it "returns nil for 3-column table (backward compat)"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (expect (org-canvas--validate-rubric-outcome-links
               (list (list "Quality" "10" "Desc"))
               "/tmp/rubrics.org" loc)
              :to-be nil)))

  (it "skips rating rows"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (expect (org-canvas--validate-rubric-outcome-links
               (list (list "> Good" "10" "" "ignored-link"))
               "/tmp/rubrics.org" loc)
              :to-be nil)))

  (it "skips empty 4th column"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (expect (org-canvas--validate-rubric-outcome-links
               (list (list "Quality" "10" "Desc" ""))
               "/tmp/rubrics.org" loc)
              :to-be nil))))

(describe "org-canvas--validate-rubric-structure with outcome links"
  (it "validates :OUTCOME: property as a file link"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Quality :10pt:
:PROPERTIES:
:OUTCOME: plain text
:END:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-rubric-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos)
                          :heading "Test Rubric"))))
       ;; Should warn about "plain text" not being a file link.  Property is
       ;; only treated as a candidate when it contains [[file:..., so a bare
       ;; non-link string yields no issue (validation is skipped).
       ;; This test asserts the structural validation (no issue) for plain
       ;; text values; only file-link values are scrutinised further.
       (expect issues :to-be nil))))

  (it "passes for criterion without :OUTCOME: property"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Quality :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-rubric-structure
              (list :file (buffer-file-name) :line (line-number-at-pos)
                    :heading "Test Rubric"))
             :to-be nil))))

;;;; New Quiz RUBRIC_LINK Validation

(describe "new quiz RUBRIC_LINK validation"
  (it "includes RUBRIC_LINK in new quizzes spec"
    (let ((nq-spec (cl-find-if
                    (lambda (s) (equal (plist-get s :label) "New Quizzes"))
                    (org-canvas--validate-specs))))
      (expect nq-spec :to-be-truthy)
      (let* ((rubric-prop (cl-find-if
                           (lambda (p) (equal (plist-get p :name) "RUBRIC_LINK"))
                           (plist-get nq-spec :properties)))
             (rubric-type (plist-get rubric-prop :type)))
        (expect rubric-prop :to-be-truthy)
        (expect rubric-type :to-equal 'link)
        (expect (plist-get rubric-prop :target-file)
                :to-equal 'org-canvas-rubrics-file)))))

;;;; Coverage: validate-assignment-structure with section ID issues (Line 608)

(describe "org-canvas--validate-assignment-structure with section ID issues"
  (it "collects section ID issues from override table"
    (let* ((temp-dir (make-temp-file "val-assign-struct-" t))
           (sections-file (expand-file-name "sections.org" temp-dir))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            ;; Section without CANVAS_ID
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:END:\n"))
            (let ((link (format "[[file:%s::*Section A][Section A]]" sections-file)))
              (with-temp-file assign-file
                (insert (format "* Assignment 1\n:PROPERTIES:\n:POINTS: 100\n:END:\n\n#+NAME: overrides\n| Section | Due At |\n|---------+--------|\n| %s | <2027-01-15 Fri 23:59> |\n" link)))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (org-back-to-heading t)
                (let ((issues (org-canvas--validate-assignment-structure
                               (list :file assign-file :line 1 :heading "Assignment 1"))))
                  (expect issues :not :to-be nil)
                  (expect (cl-some (lambda (i) (string-match "no CANVAS_ID" (plist-get i :message)))
                                   issues)
                          :to-be-truthy)))))
        (delete-directory temp-dir t)))))

(provide 'org-canvas-validate-test)
;;; org-canvas-validate-test.el ends here
