;;; org-canvas-validate-test.el --- Tests for org-canvas-validate  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the validation engine: type validators, date ordering,
;; structural validators, and the main `org-canvas-validate' command.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-validate)

;;;; Type Validator Tests

(describe "org-canvas--validate-check-boolean"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-boolean nil "PUBLISHED" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for 'true'"
    (expect (org-canvas--validate-check-boolean "true" "PUBLISHED" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for 'false'"
    (expect (org-canvas--validate-check-boolean "false" "PUBLISHED" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for 'TRUE' (case insensitive)"
    (expect (org-canvas--validate-check-boolean "TRUE" "PUBLISHED" "/f" 1 "H")
            :to-be nil))

  (it "returns error for invalid value"
    (let ((issue (org-canvas--validate-check-boolean "yes" "PUBLISHED" "/f" 1 "H")))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not a valid boolean"))))

(describe "org-canvas--validate-check-number"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-number nil "POINTS" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--validate-check-number "" "POINTS" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for integer"
    (expect (org-canvas--validate-check-number "100" "POINTS" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for decimal"
    (expect (org-canvas--validate-check-number "99.5" "POINTS" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for negative number"
    (expect (org-canvas--validate-check-number "-5" "POINTS" "/f" 1 "H")
            :to-be nil))

  (it "returns error for non-numeric"
    (let ((issue (org-canvas--validate-check-number "abc" "POINTS" "/f" 1 "H")))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not a valid number"))))

(describe "org-canvas--validate-check-enum"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-enum nil "TYPE" '("a" "b") "/f" 1 "H")
            :to-be nil))

  (it "returns nil for valid value"
    (expect (org-canvas--validate-check-enum "a" "TYPE" '("a" "b") "/f" 1 "H")
            :to-be nil))

  (it "returns error for invalid value"
    (let ((issue (org-canvas--validate-check-enum "c" "TYPE" '("a" "b") "/f" 1 "H")))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not valid")))

  (it "lists valid values in error message"
    (let ((issue (org-canvas--validate-check-enum "x" "T" '("a" "b" "c") "/f" 1 "H")))
      (expect (plist-get issue :message) :to-match "a, b, c"))))

(describe "org-canvas--validate-check-csv-enum"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-csv-enum nil "ROLES" '("a" "b") "/f" 1 "H")
            :to-be nil))

  (it "returns nil for single valid value"
    (expect (org-canvas--validate-check-csv-enum "a" "ROLES" '("a" "b") "/f" 1 "H")
            :to-be nil))

  (it "returns nil for multiple valid values"
    (expect (org-canvas--validate-check-csv-enum "a,b" "ROLES" '("a" "b") "/f" 1 "H")
            :to-be nil))

  (it "handles whitespace around commas"
    (expect (org-canvas--validate-check-csv-enum "a , b" "ROLES" '("a" "b") "/f" 1 "H")
            :to-be nil))

  (it "returns error for invalid part"
    (let ((issue (org-canvas--validate-check-csv-enum "a,bad" "ROLES" '("a" "b") "/f" 1 "H")))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "bad"))))

(describe "org-canvas--validate-check-timestamp"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-timestamp nil "DUE_AT" "/f" 1 "H")
            :to-be nil))

  (it "returns nil for valid future timestamp"
    (expect (org-canvas--validate-check-timestamp
             "<2099-12-31 Wed 23:59>" "DUE_AT" "/f" 1 "H")
            :to-be nil))

  (it "returns warning for past timestamp"
    (let ((issue (org-canvas--validate-check-timestamp
                  "<2020-01-01 Wed 09:00>" "DUE_AT" "/f" 1 "H")))
      (expect (plist-get issue :severity) :to-equal 'warning)
      (expect (plist-get issue :message) :to-match "in the past")))

  (it "returns error for malformed timestamp"
    (let ((issue (org-canvas--validate-check-timestamp
                  "not-a-date" "DUE_AT" "/f" 1 "H")))
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "not a valid Org timestamp"))))

(describe "org-canvas--validate-check-link"
  (it "returns nil for absent value"
    (expect (org-canvas--validate-check-link nil "GROUP" 'org-canvas-assignment-groups-file
                                             "CANVAS_ID" "/f" 1 "H")
            :to-be nil))

  (it "returns error for non-link value"
    (let ((issue (org-canvas--validate-check-link "Labs" "GROUP"
                                                   'org-canvas-assignment-groups-file
                                                   "CANVAS_ID" "/f" 1 "H")))
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
                            "CANVAS_ID" src-file 1 "Test Assignment")))
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
                    (buffer-file-name) (line-number-at-pos) "Test")))
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
                    (buffer-file-name) (line-number-at-pos) "Test")))
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
                    (buffer-file-name) (line-number-at-pos) "Test")))
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
                    (buffer-file-name) (line-number-at-pos) "Test")))
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
                    (buffer-file-name) (line-number-at-pos) "Test")))
       (expect issues :to-equal nil)))))

;;;; Structural Validator Tests

(describe "org-canvas--validate-rubric-structure"
  (it "returns nil when table is present"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:

| Criterion | Excellent | Good | Poor |
|-----------+-----------+------+------|
| Writing   |        10 |    7 |    3 |
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-rubric-structure
              (buffer-file-name) (line-number-at-pos) "Test Rubric")
             :to-be nil)))

  (it "returns error when no table"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:

No table here.
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-rubric-structure
                    (buffer-file-name) (line-number-at-pos) "Test Rubric")))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "no criteria table")))))

(describe "org-canvas--validate-file-structure"
  (it "returns nil for a folder heading (no link)"
    (with-temp-org-buffer
     "* course-content
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-file-structure
              (buffer-file-name) (line-number-at-pos) "course-content")
             :to-be nil)))

  (it "returns error for missing linked file"
    (with-temp-org-buffer
     "* [[file:content/nonexistent.pdf][Nonexistent]]
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-file-structure
                    (buffer-file-name) (line-number-at-pos) "Nonexistent")))
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
                             org-file (line-number-at-pos) "test.txt")
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
              (buffer-file-name) (line-number-at-pos) "Section A")
             :to-be nil)))

  (it "returns warning when CANVAS_ID is missing"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-section-structure
                    (buffer-file-name) (line-number-at-pos) "Section A")))
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
              (buffer-file-name) (line-number-at-pos) "Assignment 1")
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
                    (buffer-file-name) (line-number-at-pos) "Assignment 1")))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "not a file link")))))

;;;; Issue Formatting

(describe "org-canvas--validate-format-issue"
  (it "formats error issue correctly"
    (let ((issue (org-canvas--validate-make-issue
                  'error "/path/to/file.org" 42 "Test" "POINTS" "bad value")))
      (expect (org-canvas--validate-format-issue issue)
              :to-equal "/path/to/file.org:42: error: bad value")))

  (it "formats warning issue correctly"
    (let ((issue (org-canvas--validate-make-issue
                  'warning "/path/to/file.org" 10 "Test" "DUE_AT" "in the past")))
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
          (org-canvas-sections-file (expand-file-name "sections.org" ,temp-dir-var)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,temp-dir-var t))))

(defun test-validate-create-empty-files (dir &optional except)
  "Create empty org files for all content types in DIR.
EXCEPT is a list of filenames to skip."
  (dolist (f '("assignments.org" "pages.org" "quizzes.org"
               "modules.org" "files.org" "outcomes.org"
               "rubrics.org" "discussions.org" "announcements.org"
               "assignment-groups.org" "sections.org"))
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
                    (plist-get (cl-find "Assignments" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Assignment")))
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
                    (plist-get (cl-find "Assignments" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Assignment")))
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
                    (plist-get (cl-find "Discussions" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Discussion")))
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
                    (plist-get (cl-find "Discussions" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Graded Discussion")))
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
                    (plist-get (cl-find "Outcomes" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Outcome")))
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
                    (plist-get (cl-find "Outcomes" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Outcome")))
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
                    (plist-get (cl-find "Module Items" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Item 1")))
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
                    (plist-get (cl-find "Module Items" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Item 1")))
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
                    (plist-get (cl-find "Quiz Questions" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Question 1")))
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
                    (plist-get (cl-find "Quiz Questions" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Question 1")))
       (expect issues :to-equal nil)))))

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
                    (plist-get (cl-find "Pages" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Page")))
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
                    (plist-get (cl-find "Pages" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Page")))
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
                    (plist-get (cl-find "Quizzes" org-canvas--validate-specs
                                        :key (lambda (s) (plist-get s :label))
                                        :test #'string=)
                               :properties)
                    (buffer-file-name) (line-number-at-pos) "Test Quiz")))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "QUIZ_TYPE")))))

;;;; Validate Spec Engine Tests

(describe "org-canvas--validate-spec"
  (it "validates properties from spec"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Page 1\n:PROPERTIES:\n:PUBLISHED: invalid\n:END:\n"))
      (let ((spec (cl-find "Pages" org-canvas--validate-specs
                           :key (lambda (s) (plist-get s :label))
                           :test #'string=)))
        (let ((issues (org-canvas--validate-spec spec)))
          (expect (length issues) :to-be-greater-than 0)
          (expect (plist-get (car issues) :message) :to-match "boolean")))))

  (it "returns empty list for valid file"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Page 1\n:PROPERTIES:\n:PUBLISHED: true\n:FRONT_PAGE: false\n:END:\n"))
      (let ((spec (cl-find "Pages" org-canvas--validate-specs
                           :key (lambda (s) (plist-get s :label))
                           :test #'string=)))
        (expect (org-canvas--validate-spec spec) :to-equal nil))))

  (it "returns empty list when file does not exist"
    (with-validate-test-dir dir
      (let ((spec (cl-find "Pages" org-canvas--validate-specs
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
                    (buffer-file-name) (line-number-at-pos) "Test")))
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
                    (buffer-file-name) (line-number-at-pos) "Test")))
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
                             org-file 1 "Test File")))
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

(provide 'org-canvas-validate-test)
;;; org-canvas-validate-test.el ends here
