;;; org-canvas-validate-issue-test.el --- Buttercup tests for the validator's shared vocabulary  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-validate-issue': the issue plist and
;; its formatting, the check registry and accepts (issue #538),
;; the type-specific validators, date ordering, the property
;; spec table, the roster lookups, and the entry checks.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

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

(describe "heading date order compares the dates that are there (issue #534)"
  (it "warns when the unlock comes after the lock and there is no due date"
    (with-temp-org-buffer
        "* Test\n:PROPERTIES:\n:UNLOCK_AT: <2099-03-15 Sun 00:00>\n:LOCK_AT: <2099-03-12 Thu 23:59>\n:END:\n"
      (org-back-to-heading t)
      (let ((issues (org-canvas--validate-check-date-order
                     '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
                     (list :file "/f" :line 1 :heading "Test"))))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :message) :to-match "UNLOCK_AT is after LOCK_AT"))))

  (it "reports a pair two triples share only once"
    (with-temp-org-buffer
        "* Debate\n:PROPERTIES:\n:AVAILABLE_FROM: <2099-03-15 Sun 00:00>\n:LOCK_AT: <2099-03-12 Thu 23:59>\n:END:\n"
      (org-back-to-heading t)
      (let ((issues (org-canvas--validate-check-date-order
                     '(("AVAILABLE_FROM" "DUE_AT" "LOCK_AT")
                       ("AVAILABLE_FROM" "CHECKPOINT_REPLY_DUE_AT" "LOCK_AT"))
                     (list :file "/f" :line 1 :heading "Debate"))))
        (expect (length issues) :to-equal 1)))))

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

(describe "(org-canvas--validate-specs) includes new properties"
  (it "validates MUTED as boolean in assignments"
    (let ((loc (list :file "/f" :line 1 :heading "H")))
      (expect (org-canvas--validate-check-boolean "true" "MUTED" loc) :to-be nil)
      (expect (org-canvas--validate-check-boolean "invalid" "MUTED" loc) :to-be-truthy)))

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

(describe "org-canvas--validate-resolve-file-link pending-sync tagging"
  (it "marks unresolved link-target issues as pending-sync"
    (let* ((dir (make-temp-file "vlink-" t))
           (target (expand-file-name "t.org" dir))
           (src (expand-file-name "s.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file target (insert "* Target\n"))
            (let ((issue (org-canvas--validate-resolve-file-link
                          "[[file:t.org::*Target][Target]]"
                          :id-property "CANVAS_ID"
                          :source-file src
                          :loc '(:file "s.org" :line 1 :heading "H")
                          :property "GROUP"
                          :not-link-msg "not a link"
                          :unresolved-msg "GROUP: link target has no CANVAS_ID (sync target first)")))
              (expect (plist-get issue :pending-sync) :to-be t)
              (expect (plist-get issue :severity) :to-equal 'warning)))
        (delete-directory dir t))))

  (it "does not mark non-link errors as pending-sync"
    (let ((issue (org-canvas--validate-resolve-file-link
                  "plain text"
                  :id-property "CANVAS_ID"
                  :source-file "/tmp/s.org"
                  :loc '(:file "s.org" :line 1 :heading "H")
                  :property "GROUP"
                  :not-link-msg "not a link"
                  :unresolved-msg "unresolved")))
      (expect (plist-get issue :pending-sync) :to-be nil)
      (expect (plist-get issue :severity) :to-equal 'error))))

(describe "Canvas-owned enum values (issue #167)"
  (it "accepts SUBMISSION: online_quiz on a pulled quiz-backed assignment"
    (with-temp-org-buffer
     "* Quiz 1
:PROPERTIES:
:CANVAS_ID: 4242
:SUBMISSION: online_quiz
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-entry-properties
              (plist-get (car (seq-filter
                               (lambda (spec)
                                 (eq (plist-get spec :file) 'org-canvas-assignments-file))
                               (org-canvas--validate-specs)))
                         :properties)
              '(:file "assignments.org" :line 1 :heading "Quiz 1"))
             :to-be nil)))

  (it "still rejects a submission type Canvas has never heard of"
    (with-temp-org-buffer
     "* Homework
:PROPERTIES:
:SUBMISSION: carrier_pigeon
:END:
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-entry-properties
                    '((:name "SUBMISSION" :type csv-enum
                       :values ("online_upload" "none")
                       :read-only-values ("online_quiz")))
                    '(:file "assignments.org" :line 1 :heading "Homework"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :message) :to-match "carrier_pigeon"))))

  (it "carries :read-only-values from the registry into the validate spec"
    (let* ((spec (car (seq-filter
                       (lambda (s)
                         (eq (plist-get s :file) 'org-canvas-assignments-file))
                       (org-canvas--validate-specs))))
           (prop (car (seq-filter
                       (lambda (p) (equal (plist-get p :name) "SUBMISSION"))
                       (plist-get spec :properties)))))
      (expect (plist-get prop :read-only-values) :to-equal '("online_quiz" "discussion_topic")))))

(describe "past timestamps on headings already on Canvas (issue #208)"
  (defconst test-validate-208-course
    "* Homework 1
:PROPERTIES:
:CANVAS_ID: 11
:DUE_AT: <2001-01-05 Fri>
:END:
* Homework 2
:PROPERTIES:
:DUE_AT: <2001-01-12 Fri>
:END:
"
    "One synced and one unsynced assignment, both with a due date long past.")

  (defun test-validate-208-warnings (setting)
    "Validate the fixture under SETTING and return the past-date warnings."
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "assignments.org" dir)
        (insert test-validate-208-course))
      (test-validate-create-empty-files dir '("assignments.org"))
      (let ((org-canvas-validate-past-timestamps setting))
        (cl-remove-if-not
         (lambda (i) (string-match-p "is in the past" (plist-get i :message)))
         (plist-get (org-canvas--validate-run-all-specs) :issues)))))

  (it "warns only on the heading no push has created, by default"
    (let ((warnings (test-validate-208-warnings 'unsynced-only)))
      (expect (length warnings) :to-equal 1)
      (expect (plist-get (car warnings) :heading) :to-equal "Homework 2")
      (expect (plist-get (car warnings) :push-only) :to-be t)))

  (it "warns on every heading under always, and never under never"
    (expect (length (test-validate-208-warnings 'always)) :to-equal 2)
    (expect (test-validate-208-warnings 'never) :to-be nil))

  (it "treats a page stamped with CANVAS_URL as on Canvas"
    (with-temp-org-buffer
        "* Syllabus\n:PROPERTIES:\n:CANVAS_URL: syllabus\n:TODO_DATE: <2001-01-05 Fri>\n:END:\n"
      (goto-char (point-min))
      (expect (org-canvas--validate-entry-on-canvas-p) :to-be t)
      (expect (org-canvas--validate-check-timestamp
               "<2001-01-05 Fri>" "TODO_DATE" (list :file "f" :line 1 :heading "Syllabus"))
              :to-be nil)))

  (it "never warns on the course's own dates in settings.org"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "settings.org" dir)
        (insert "* Ethics\n:PROPERTIES:\n:START_AT: <2001-01-05 Fri>\n:END_AT: <2001-05-05 Sat>\n:END:\n"))
      (test-validate-create-empty-files dir '("settings.org"))
      (let ((issues (plist-get (org-canvas--validate-run-all-specs) :issues)))
        (expect (cl-some (lambda (i) (string-match-p "is in the past" (plist-get i :message))) issues)
                :to-be nil))))

  (it "answers nil outside an Org entry, so a bare check warns as before"
    (with-temp-buffer
      (expect (org-canvas--validate-entry-on-canvas-p) :to-be nil))))

(describe "pull-only properties and the past-timestamp warning"
  (it "still warns for an ordinary timestamp in the past"
    (let ((issue (org-canvas--validate-check-timestamp
                  "<2020-01-01 Wed 10:00>" "DUE_AT" (list :file "f" :line 1 :heading "H"))))
      (expect (plist-get issue :message) :to-match "in the past")
      (expect (plist-get issue :push-only) :to-be t)))

  (it "says nothing about a pull-only timestamp in the past"
    (expect (org-canvas--validate-check-timestamp
             "<2020-01-01 Wed 10:00>" "POSTED_AT" (list :file "f" :line 1 :heading "H") t)
            :to-be nil))

  (it "still reports a pull-only timestamp it cannot parse"
    (let ((issue (org-canvas--validate-check-timestamp
                  "yesterday-ish" "POSTED_AT" (list :file "f" :line 1 :heading "H") t)))
      (expect (plist-get issue :severity) :to-equal 'error)))

  (it "carries the flag from the registry into the validate spec"
    (expect (plist-get (org-canvas--property-to-validate-prop
                        '(:org-prop "POSTED_AT" :type timestamp :pull-only t))
                       :pull-only)
            :to-be t)
    (expect (plist-get (org-canvas--property-to-validate-prop
                        '(:org-prop "DUE_AT" :type timestamp))
                       :pull-only)
            :to-be nil))

  (it "leaves a pulled announcement's POSTED_AT out of the findings"
    (let ((file (make-temp-file "ann-" nil ".org")))
      (unwind-protect
          (with-org-canvas-test-config
            (with-temp-file file
              (insert "* Old news\n:PROPERTIES:\n:CANVAS_ID: 1\n:POSTED_AT: <2020-01-01 Wed 10:00>\n:AUTHOR: Someone\n:END:\n"))
            (let* ((org-canvas-announcements-file file)
                   (spec (cl-find "Announcements" (org-canvas--validate-specs)
                                  :key (lambda (s) (plist-get s :label)) :test #'equal))
                   (issues (org-canvas--validate-spec spec)))
              (expect (cl-some (lambda (i) (equal (plist-get i :property) "POSTED_AT")) issues)
                      :to-be nil)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas--validate-canvas-deleted (issue #392)"
  (it "warns on a heading a pull marked deleted on Canvas, not only before a push"
    (with-temp-org-buffer
     "* Sprint 0
:PROPERTIES:
:CANVAS_ID: 100
:CANVAS_DELETED: [2026-09-28 Mon 10:00]
:END:
"
     (org-back-to-heading)
     (let* ((issues (org-canvas--validate-entry-at-marker nil nil nil "a.org"))
            (issue (car issues)))
       (expect (length issues) :to-equal 1)
       (expect (plist-get issue :severity) :to-be 'warning)
       (expect (plist-get issue :property) :to-equal "CANVAS_DELETED")
       (expect (plist-get issue :push-only) :to-be nil)
       (expect (plist-get issue :message) :to-match "'Sprint 0' was deleted on Canvas"))))

  (it "says nothing of an unmarked heading"
    (with-temp-org-buffer
     "* Sprint 1
:PROPERTIES:
:CANVAS_ID: 101
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-entry-at-marker nil nil nil "a.org") :to-be nil))))

(describe "org-canvas--validate-check-canvas-owned"
  (let ((loc (list :file "/f" :line 1 :heading "Essay")))
    (it "warns, push-only, on a heading no push has created"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:DOCUMENT_PROCESSOR: Turnitin (asset processor 5)\n:END:\n"
       (org-back-to-heading)
       (let ((issue (org-canvas--validate-check-canvas-owned
                     "Turnitin (asset processor 5)" "DOCUMENT_PROCESSOR" loc)))
         (expect (plist-get issue :severity) :to-equal 'warning)
         (expect (plist-get issue :property) :to-equal "DOCUMENT_PROCESSOR")
         (expect (plist-get issue :message) :to-match "set by Canvas")
         ;; The warning names the command that opens the page (#292).
         (expect (plist-get issue :message)
                 :to-match "org-canvas-browse-edit-at-point")
         (expect (plist-get issue :push-only) :to-be t))))

    (it "says nothing on a stamped heading, where a pull wrote it"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:DOCUMENT_PROCESSOR: Turnitin (asset processor 5)\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--validate-check-canvas-owned
                "Turnitin (asset processor 5)" "DOCUMENT_PROCESSOR" loc)
               :to-be nil)))

    (it "says nothing when the property is absent or empty"
      (with-temp-org-buffer
       "* Essay\n"
       (org-back-to-heading)
       (expect (org-canvas--validate-check-canvas-owned nil "DOCUMENT_PROCESSOR" loc)
               :to-be nil)
       (expect (org-canvas--validate-check-canvas-owned "" "DOCUMENT_PROCESSOR" loc)
               :to-be nil)))

    (it "runs from the entry check through the registry's own spec"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:DOCUMENT_PROCESSOR: Turnitin (asset processor 5)\n:END:\n"
       (org-back-to-heading)
       (let* ((props (mapcar #'org-canvas--property-to-validate-prop
                             (plist-get (gethash "assignments"
                                                 org-canvas--registry)
                                        :properties)))
              (issues (org-canvas--validate-entry-properties props loc)))
         (expect (length issues) :to-equal 1)
         (expect (plist-get (car issues) :property) :to-equal "DOCUMENT_PROCESSOR"))))))

(describe "org-canvas--validate-check-intent"
  (let ((loc (list :file "/f" :line 1 :heading "Global Challenge Essay")))
    (it "warns on a stamped column the last pull found without a processor"
      (with-temp-org-buffer
       "* Global Challenge Essay\n:PROPERTIES:\n:CANVAS_ID: 2497349\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (let ((issue (org-canvas--validate-check-intent
                     "Turnitin" "WANT_DOCUMENT_PROCESSOR" "DOCUMENT_PROCESSOR" loc)))
         (expect (plist-get issue :severity) :to-equal 'warning)
         (expect (plist-get issue :property) :to-equal "WANT_DOCUMENT_PROCESSOR")
         (expect (plist-get issue :heading) :to-equal "Global Challenge Essay")
         (expect (plist-get issue :message)
                 :to-match "Turnitin wanted but the last pull recorded no DOCUMENT_PROCESSOR")
         (expect (plist-get issue :message) :to-match "web UI.*then pull")
         ;; True of the course whatever you do next (#168).
         (expect (plist-get issue :push-only) :to-be nil))))

    (it "is satisfied by the pulled name, case-insensitively"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: turnitin\n:DOCUMENT_PROCESSOR: Turnitin (asset processor 12345)\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--validate-check-intent
                "turnitin" "WANT_DOCUMENT_PROCESSOR" "DOCUMENT_PROCESSOR" loc)
               :to-be nil)))

    (it "names the processor Canvas has when it is a different one"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:DOCUMENT_PROCESSOR: Copyleaks (asset processor 9)\n:END:\n"
       (org-back-to-heading)
       (expect (plist-get (org-canvas--validate-check-intent
                           "Turnitin" "WANT_DOCUMENT_PROCESSOR" "DOCUMENT_PROCESSOR" loc)
                          :message)
               :to-match "recorded DOCUMENT_PROCESSOR: Copyleaks")))

    (it "says nothing before the column exists, or with nothing declared"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--validate-check-intent
                "Turnitin" "WANT_DOCUMENT_PROCESSOR" "DOCUMENT_PROCESSOR" loc)
               :to-be nil))
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--validate-check-intent
                nil "WANT_DOCUMENT_PROCESSOR" "DOCUMENT_PROCESSOR" loc)
               :to-be nil)
       (expect (org-canvas--validate-check-intent
                "  " "WANT_DOCUMENT_PROCESSOR" "DOCUMENT_PROCESSOR" loc)
               :to-be nil)))

    (it "runs from the entry check through the registry's own spec"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (let* ((props (mapcar #'org-canvas--property-to-validate-prop
                             (plist-get (gethash "assignments"
                                                 org-canvas--registry)
                                        :properties)))
              (issues (org-canvas--validate-entry-properties props loc)))
         (expect (length issues) :to-equal 1)
         (expect (plist-get (car issues) :property)
                 :to-equal "WANT_DOCUMENT_PROCESSOR"))))))

(describe "check ids on findings (issue #538)"
  (it "names lock-window on a missing lock and on a mismatch"
    (let ((org-canvas-validate-lock-after-due-hours 48))
      (let* ((missing (org-canvas--validate-lock-window
                       "<2099-03-10 Tue 23:59>" nil '(:file "/f" :line 1) ""))
             (mismatch (org-canvas--validate-lock-window
                        "<2099-03-10 Tue 23:59>" "<2099-03-11 Wed 23:59>"
                        '(:file "/f" :line 1) ""))
             (missing-check (plist-get missing :check))
             (mismatch-check (plist-get mismatch :check)))
        (expect missing-check :to-be 'lock-window)
        (expect mismatch-check :to-be 'lock-window))))

  (it "names require-unlock, and date-order on a heading and a row"
    (let* ((org-canvas-validate-require-unlock t)
           (unlock (car (org-canvas--validate-late-window
                         "<2099-03-10 Tue 23:59>" nil nil '(:file "/f" :line 1))))
           (unlock-check (plist-get unlock :check))
           (brief (test-validate-538--brief
                   (test-validate-538--entry
                    ":DUE_AT: <2099-03-10 Tue 23:59>\n:LOCK_AT: <2099-03-09 Mon 23:59>\n"
                    (concat "| Section | Due At | Lock At |\n|---+---+---|\n"
                            "| Group: #1 | <2099-03-20 Fri 23:59> | <2099-03-19 Thu 23:59> |\n")))))
      (expect unlock-check :to-be 'require-unlock)
      (expect (mapcar (lambda (b) (list (nth 1 b) (nth 2 b))) brief)
              :to-equal '((1 date-order) (10 date-order)))))

  (it "lists each check id with what it finds"
    (expect (mapcar #'car org-canvas--validate-checks)
            :to-equal '(date-order lock-window require-unlock))
    (expect (org-canvas--validate-checked 'date-order nil) :to-be nil)))

(describe "VALIDATE_ACCEPT on a heading (issue #538)"
  (it "takes the finding of the check it names, with the reason"
    (let* ((org-canvas-validate-lock-after-due-hours 48)
           (brief (test-validate-538--brief
                   (test-validate-538--entry
                    (concat test-validate-538--online
                            ":VALIDATE_ACCEPT: lock-window In-class exam: locks at the end of class\n")))))
      (expect (length brief) :to-equal 1)
      (expect (nth 2 (car brief)) :to-be 'lock-window)
      (expect (nth 3 (car brief))
              :to-equal "In-class exam: locks at the end of class")))

  (it "reads each VALIDATE_ACCEPT+ line whole, though Org would split them"
    (with-temp-org-buffer
        "* HW\n:PROPERTIES:\n:VALIDATE_ACCEPT: lock-window In class: locks at end\n:POINTS: 5\n:validate_accept+: date-order Extension, see the email\n:END:\n"
      (org-back-to-heading t)
      (expect (org-canvas--validate-heading-accepts)
              :to-equal '("lock-window In class: locks at end"
                          "date-order Extension, see the email"))))

  (it "takes one check of several, leaving the others' findings"
    (let* ((org-canvas-validate-lock-after-due-hours 48)
           (org-canvas-validate-require-unlock t)
           (brief (test-validate-538--brief
                   (test-validate-538--entry
                    (concat ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n"
                            ":VALIDATE_ACCEPT: require-unlock Opens on the first day\n")))))
      (expect (mapcar (lambda (b) (list (nth 2 b) (nth 3 b))) brief)
              :to-equal '((lock-window nil)
                          (require-unlock "Opens on the first day")))))

  (it "leaves the heading's override rows alone"
    (let* ((org-canvas-validate-lock-after-due-hours 48)
           (brief (test-validate-538--brief
                   (test-validate-538--entry
                    (concat test-validate-538--online
                            ":VALIDATE_ACCEPT: lock-window In class\n")
                    (concat "| Section | Due At |\n|---+---|\n"
                            "| Group: #1 | <2099-03-12 Thu 23:59> |\n")))))
      (expect (mapcar (lambda (b) (list (nth 1 b) (nth 3 b))) brief)
              :to-equal '((1 "In class") (12 nil)))))

  (it "warns of an unknown check id, and of an accept with no reason"
    (let* ((org-canvas-validate-lock-after-due-hours 48)
           (issues (test-validate-538--entry
                    (concat test-validate-538--online
                            ":VALIDATE_ACCEPT: lock-windw In class\n"
                            ":VALIDATE_ACCEPT+: lock-window\n")))
           (brief (test-validate-538--brief issues))
           (property (plist-get (nth 1 issues) :property)))
      (expect (mapcar (lambda (b) (list (nth 0 b) (nth 3 b) (nth 4 b))) brief)
              :to-equal
              '((warning nil "Lock window missing: due <2099-03-10 Tue 23:59> with no lock date; the course locks online columns 48 hour(s) after the due date")
                (warning nil "VALIDATE_ACCEPT names 'lock-windw', which is not a check validate knows (date-order, lock-window, require-unlock)")
                (warning nil "VALIDATE_ACCEPT lock-window gives no reason, so it accepts nothing: write why after the check id")))
      (expect property :to-equal "VALIDATE_ACCEPT")))

  (it "calls an accept that matched nothing stale, a note"
    (let ((brief (test-validate-538--brief
                  (test-validate-538--entry
                   ":DUE_AT: <2099-03-10 Tue 23:59>\n:VALIDATE_ACCEPT: date-order Fixed last week\n"))))
      (expect brief :to-equal
              '((note 1 nil nil "Stale accept: VALIDATE_ACCEPT date-order matched no finding here; remove it (Fixed last week)")))))

  (it "accepts no error, and says so"
    (let* ((loc '(:file "/f" :line 3 :heading "HW"))
           (err (org-canvas--validate-checked
                 'date-order (org-canvas--validate-make-issue 'error loc "DUE_AT" "bad")))
           (result (org-canvas--validate-apply-accepts
                    (list err) '("date-order We meant it") loc
                    (cons "VALIDATE_ACCEPT" "")))
           (brief (test-validate-538--brief result)))
      (expect brief :to-equal
              '((error 3 date-order nil "bad")
                (warning 3 nil nil "VALIDATE_ACCEPT date-order accepts nothing: the finding is an error, which has to be fixed"))))))

(describe "VALIDATE_ACCEPT in the property registry (issue #538)"
  (it "is a local-only string on each type whose findings carry a check"
    (dolist (feature '("assignments" "quizzes" "discussions"))
      (let* ((props (plist-get (gethash feature org-canvas--registry)
                               :properties))
             (spec (cl-find "VALIDATE_ACCEPT" props
                            :key (lambda (p) (plist-get p :org-prop))
                            :test #'equal))
             ;; Bound outside `expect': Emacs 29's oclosure shadows :type.
             (spec-type (plist-get spec :type)))
        (expect (plist-get spec :local-only) :to-be t)
        (expect spec-type :to-be 'string)
        (expect (plist-get spec :api-key) :to-be nil))))

  (it "leaves an assignment's payload, and so its PAYLOAD_HASH, as it was"
    (cl-flet ((payload (props)
                (with-temp-org-buffer
                    (format "* HW\n:PROPERTIES:\n:POINTS: 10\n%s:END:\n" props)
                  (org-back-to-heading t)
                  (json-encode (org-canvas--assignment-build-payload
                                (org-canvas--assignment-parse-entry))))))
      (expect (payload ":VALIDATE_ACCEPT: lock-window In class\n:VALIDATE_ACCEPT+: date-order Kept\n")
              :to-equal (payload ""))))

  (it "makes no drift row, and a pull leaves it alone"
    (with-temp-org-buffer
        "* HW\n:PROPERTIES:\n:VALIDATE_ACCEPT: lock-window In class\n:END:\n"
      (org-back-to-heading t)
      (let ((pulled nil))
        (cl-letf (((symbol-function 'org-canvas--registry-remote-present-p)
                   (lambda (&rest _) (setq pulled t))))
          (expect (org-canvas--diff-compare-field
                   org-canvas--validate-accept-property-spec (point)
                   '((validate_accept . "something else")))
                  :to-be nil)
          (org-canvas--pull-apply-spec
           org-canvas--validate-accept-property-spec
           '((validate_accept . "something else")) (point))
          (expect pulled :to-be nil)
          (expect (org-entry-get (point) "VALIDATE_ACCEPT")
                  :to-equal "lock-window In class"))))))

(provide 'org-canvas-validate-issue-test)
;;; org-canvas-validate-issue-test.el ends here
