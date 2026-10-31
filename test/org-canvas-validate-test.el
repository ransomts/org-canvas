;;; org-canvas-validate-test.el --- Tests for org-canvas-validate  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the validation engine: the spec runner, the
;; cross-course, body and module-coverage scans, the report
;; buffer, and the main `org-canvas-validate' commands.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

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

(describe "cross-course links (issue #172)"
  (describe "org-canvas--validate-canvas-host"
    (it "reads the host out of the configured base URL"
      (let ((org-canvas-base-url "https://clemson.instructure.com"))
        (expect (org-canvas--validate-canvas-host)
                :to-equal "clemson.instructure.com")))

    (it "tolerates a trailing path and plain http"
      (let ((org-canvas-base-url "http://canvas.example.edu/lms"))
        (expect (org-canvas--validate-canvas-host) :to-equal "canvas.example.edu")))

    (it "is nil when no instance is configured"
      (let ((org-canvas-base-url ""))
        (expect (org-canvas--validate-canvas-host) :to-be nil))))

  (describe "org-canvas--validate-cross-course-links"
    (defun test-validate-cross-course (body)
      "Scan BODY as assignments.org and return the issues.
Binds the instance and course id: `org-canvas-init' sets both
globally, so a spec that depends on them must say which it means."
      (let* ((dir (make-temp-file "org-val-xc-" t))
             (file (expand-file-name "assignments.org" dir)))
        (unwind-protect
            (progn (with-temp-file file (insert body))
                   (with-org-canvas-test-config
                     (org-canvas--validate-cross-course-links file)))
          (delete-directory dir t))))

    (it "reports a link into another course shell"
      (let ((issues (test-validate-cross-course
                     "* Sprint 1
See [[https://test.canvas.example.com/courses/284220/pages/read-this][the page]].
")))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :severity) :to-equal 'warning)
        (expect (plist-get (car issues) :message) :to-match "course 284220")
        (expect (plist-get (car issues) :cross-course) :to-equal "284220")))

    (it "names the heading and line, so next-error reaches it"
      (let ((issues (test-validate-cross-course
                     "* First
* Sprint 1
https://test.canvas.example.com/courses/284220/assignments/7
")))
        (expect (plist-get (car issues) :heading) :to-equal "Sprint 1")
        (expect (plist-get (car issues) :line) :to-equal 3)))

    (it "says nothing about a link into this course"
      (expect (test-validate-cross-course
               "* Sprint 1
[[https://test.canvas.example.com/courses/99999/pages/syllabus]]
")
              :to-be nil))

    (it "says nothing about a link to another site entirely"
      (expect (test-validate-cross-course
               "* Sprint 1
[[https://example.org/courses/284220/pages/x]]
")
              :to-be nil))

    (it "reports a file link into another shell, as the rewriter's twin"
      (let ((issues (test-validate-cross-course
                     "* Sprint 1
[[https://test.canvas.example.com/courses/208463/files/21157335/preview]]
")))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :cross-course) :to-equal "208463")))

    (it "reports a top-level users or accounts route"
      (let ((issues (test-validate-cross-course
                     "* Sprint 1
[[https://test.canvas.example.com/users/4211]]
[[https://test.canvas.example.com/accounts/1]]
")))
        (expect (length issues) :to-equal 2)
        (expect (plist-get (car issues) :message) :to-match "users route")
        (expect (plist-get (cadr issues) :message) :to-match "accounts route")))

    (it "leaves a course-scoped users route to the course check"
      (expect (test-validate-cross-course
               "* Sprint 1
[[https://test.canvas.example.com/courses/99999/users/4211]]
")
              :to-be nil))

    (it "finds a link in a property drawer, not only in a body"
      (let ((issues (test-validate-cross-course
                     "* Sprint 1
:PROPERTIES:
:EXTERNAL_TOOL_URL: https://test.canvas.example.com/courses/284220/external_tools/9
:END:
")))
        (expect (length issues) :to-equal 1)))

    (it "is quiet when no course is configured to compare against"
      (let* ((dir (make-temp-file "org-val-xc-" t))
             (file (expand-file-name "assignments.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file file
                (insert "* Sprint 1
[[https://test.canvas.example.com/courses/284220/pages/x]]
"))
              (with-org-canvas-test-config
                (let ((org-canvas-course-id ""))
                  (expect (org-canvas--validate-cross-course-links file)
                          :to-be nil))))
          (delete-directory dir t)))))

  (describe "org-canvas--validate-cross-course-summary"
    (it "groups the links by the shell they point at"
      (expect (org-canvas--validate-cross-course-summary
               (list '(:cross-course "208463") '(:cross-course "284220")
                     '(:cross-course "208463") '(:severity warning)))
              :to-equal "3 link(s) into 2 other course(s) or account route(s): 208463, 284220"))

    (it "is nil when there are none"
      (expect (org-canvas--validate-cross-course-summary
               (list '(:severity warning :message "x")))
              :to-be nil)))

  (describe "in the report"
    (it "lists each link and then groups them"
      (with-validate-test-dir dir
        (with-temp-file (expand-file-name "assignments.org" dir)
          (insert "* Sprint 1
[[https://test.canvas.example.com/courses/284220/pages/read-this]]
* Sprint 2
[[https://test.canvas.example.com/courses/208463/files/21157335/preview]]
"))
        (test-validate-create-empty-files dir '("assignments.org"))
        (cl-letf (((symbol-function 'princ) #'ignore))
          (with-org-canvas-test-config (org-canvas-validate)))
        (with-current-buffer "*canvas-validate*"
          (let ((content (buffer-string)))
            (expect content :to-match "course 284220")
            (expect content :to-match "course 208463")
            (expect content
                    :to-match "2 link(s) into 2 other course(s) or account route(s): 208463, 284220")))))

    (it "reports each foreign link once, not once per spec sharing the file"
      (with-validate-test-dir dir
        (with-temp-file (expand-file-name "modules.org" dir)
          (insert "* Week 1
[[https://test.canvas.example.com/courses/284220/pages/read-this]]
"))
        (test-validate-create-empty-files dir '("modules.org"))
        (cl-letf (((symbol-function 'princ) #'ignore))
          (with-org-canvas-test-config (org-canvas-validate)))
        (with-current-buffer "*canvas-validate*"
          (expect (buffer-string) :to-match "1 link(s) into 1 other course"))))

    (it "survives a read-only course, being course state and not push advice"
      (with-validate-test-dir dir
        (with-temp-file (expand-file-name "assignments.org" dir)
          (insert "* Sprint 1
[[https://test.canvas.example.com/courses/284220/pages/read-this]]
"))
        (test-validate-create-empty-files dir '("assignments.org"))
        (cl-letf (((symbol-function 'princ) #'ignore))
          (with-org-canvas-test-config
            (let ((org-canvas-read-only t)) (org-canvas-validate))))
        (with-current-buffer "*canvas-validate*"
          (expect (buffer-string) :to-match "course 284220"))))))

(describe "read-only validation (issue #168)"
  (defconst test-validate-push-only-course
    "* Homework 1
:PROPERTIES:
:DUE_AT: <2001-01-05 Fri>
:END:
"
    "One unsynced assignment whose only finding is a due date long past.
Unsynced, because since issue #208 a stamped heading's past date is
history and not a finding at all.")

  (it "holds push-only findings back on a read-only course"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "assignments.org" dir)
        (insert test-validate-push-only-course))
      (test-validate-create-empty-files dir '("assignments.org"))
      (cl-letf (((symbol-function 'princ) #'ignore))
        (let ((org-canvas-read-only t))
          (org-canvas-validate)))
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :not :to-match "is in the past")
          (expect content :to-match "1 push-only finding(s) suppressed")
          (expect content :to-match "org-canvas-validate-all")
          (expect content :to-match "0 error(s), 0 warning(s)")))))

  (it "reports them as usual on a course you can write to"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "assignments.org" dir)
        (insert test-validate-push-only-course))
      (test-validate-create-empty-files dir '("assignments.org"))
      (cl-letf (((symbol-function 'princ) #'ignore))
        (let ((org-canvas-read-only nil))
          (org-canvas-validate)))
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "is in the past")
          (expect content :not :to-match "suppressed")
          (expect content :to-match "0 error(s), 1 warning(s)")))))

  (it "keeps findings that describe the course, read-only or not"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Bad Page\n:PROPERTIES:\n:PUBLISHED: yes\n:END:\n"))
      (test-validate-create-empty-files dir '("pages.org"))
      (cl-letf (((symbol-function 'princ) #'ignore))
        (let ((org-canvas-read-only t))
          (org-canvas-validate)))
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "not a valid boolean")
          (expect content :not :to-match "suppressed")))))

  (it "says so even when nothing else is left to report"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "assignments.org" dir)
        (insert test-validate-push-only-course))
      (test-validate-create-empty-files dir '("assignments.org"))
      (cl-letf (((symbol-function 'princ) #'ignore))
        (let ((org-canvas-read-only t))
          (org-canvas-validate)))
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "No issues found")
          (expect content :to-match "suppressed")))))

  (describe "org-canvas-validate-all"
    (it "shows the push-only findings a read-only course suppresses"
      (with-validate-test-dir dir
        (with-temp-file (expand-file-name "assignments.org" dir)
          (insert test-validate-push-only-course))
        (test-validate-create-empty-files dir '("assignments.org"))
        (cl-letf (((symbol-function 'princ) #'ignore))
          (let ((org-canvas-read-only t))
            (org-canvas-validate-all)))
        (with-current-buffer "*canvas-validate*"
          (let ((content (buffer-string)))
            (expect content :to-match "is in the past")
            (expect content :not :to-match "suppressed")
            (expect content :to-match "0 error(s), 1 warning(s)")))))

    (it "is an interactive command"
      (expect (commandp 'org-canvas-validate-all) :to-be-truthy)))

  (describe "org-canvas--validate-push-only"
    (it "marks an issue and leaves everything else about it alone"
      (let ((issue (org-canvas--validate-push-only
                    (org-canvas--validate-make-issue
                     'warning '(:file "a.org" :line 3 :heading "H") "DUE_AT" "late"))))
        (expect (plist-get issue :push-only) :to-be t)
        (expect (plist-get issue :message) :to-equal "late")
        (expect (plist-get issue :line) :to-equal 3)))

    (it "passes nil through, so a check can wrap its result"
      (expect (org-canvas--validate-push-only nil) :to-be nil))

    (it "marks a pre-first-sync link warning, which no read-only course can act on"
      (let ((issue (org-canvas--validate-resolve-file-link
                    "[[file:nowhere.org::*Nothing][Nothing]]" "CANVAS_ID"
                    "assignments.org" '(:file "assignments.org" :line 1 :heading "H")
                    "GROUP" "not a link" "no CANVAS_ID yet")))
        (expect (plist-get issue :push-only) :to-be t)
        (expect (plist-get issue :pending-sync) :to-be t)))))

(describe "org-canvas-validate in batch (issue #169)"
  (it "prints the whole report, not only the tally"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Bad Page\n:PROPERTIES:\n:PUBLISHED: yes\n:END:\n"))
      (test-validate-create-empty-files dir '("pages.org"))
      (let ((printed ""))
        (cl-letf (((symbol-function 'princ)
                   (lambda (s &optional _) (setq printed (concat printed s)))))
          (let ((noninteractive t)) (org-canvas-validate)))
        (expect printed :to-match "pages\\.org:1: error:")
        (expect printed :to-match "not a valid boolean"))))

  (it "returns the error count so a caller can act on it"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Bad Page\n:PROPERTIES:\n:PUBLISHED: yes\n:END:\n"))
      (test-validate-create-empty-files dir '("pages.org"))
      (cl-letf (((symbol-function 'princ) #'ignore))
        (expect (org-canvas-validate) :to-equal 1))))

  (it "returns zero when the course is clean"
    (with-validate-test-dir dir
      (test-validate-create-empty-files dir)
      (cl-letf (((symbol-function 'princ) #'ignore))
        (expect (org-canvas-validate) :to-equal 0)))))

(describe "org-canvas-validate-batch (issue #169)"
  (it "exits non-zero when there are errors"
    (let (status)
      (cl-letf (((symbol-function 'org-canvas-validate) (lambda (&rest _) 3))
                ((symbol-function 'kill-emacs) (lambda (code) (setq status code))))
        (org-canvas-validate-batch))
      (expect status :to-equal 1)))

  (it "exits zero when there are none"
    (let (status)
      (cl-letf (((symbol-function 'org-canvas-validate) (lambda (&rest _) 0))
                ((symbol-function 'kill-emacs) (lambda (code) (setq status code))))
        (org-canvas-validate-batch))
      (expect status :to-equal 0)))

  (it "counts only errors, never the warnings the report also holds"
    (let (status)
      (with-validate-test-dir dir
        (with-temp-file (expand-file-name "assignments.org" dir)
          (insert "* Homework\n:PROPERTIES:\n:DUE_AT: <2001-01-01 Mon>\n:END:\n"))
        (test-validate-create-empty-files dir '("assignments.org"))
        (cl-letf (((symbol-function 'princ) #'ignore)
                  ((symbol-function 'kill-emacs) (lambda (code) (setq status code))))
          (org-canvas-validate-batch)))
      (expect status :to-equal 0))))

(describe "org-canvas-validate pending-first-sync collapse"
  (let ((fake-issues nil))
    (before-each
      (setq fake-issues
            (list (org-canvas--validate-make-issue
                   'warning '(:file "a.org" :line 1 :heading "H1") "GROUP"
                   "GROUP: link target has no CANVAS_ID (sync target first)" t)
                  (org-canvas--validate-make-issue
                   'warning '(:file "a.org" :line 5 :heading "H2") "RUBRIC_LINK"
                   "RUBRIC_LINK: link target has no CANVAS_ID (sync target first)" t)
                  (org-canvas--validate-make-issue
                   'error '(:file "a.org" :line 9 :heading "H3") "PUBLISHED"
                   "PUBLISHED: 'yes' is not a valid boolean")))
      (spy-on 'org-canvas--validate-run-all-specs
              :and-return-value (list :issues fake-issues :checked 1 :skipped 0)))

    (it "collapses pending-sync warnings into a single summary line"
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "2 link(s) pending first sync")
          (expect content :not :to-match "sync target first")
          (expect content :to-match "not a valid boolean")
          (expect content :to-match "(2 pending first sync)"))))

    (it "lists pending warnings individually with a verbose argument"
      (org-canvas-validate t)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "GROUP: link target has no CANVAS_ID")
          (expect content :to-match "RUBRIC_LINK: link target has no CANVAS_ID")
          (expect content :not :to-match "pending first sync); C-u"))))

    (it "still counts pending warnings in the totals"
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (expect (buffer-string)
                :to-match "1 error(s), 2 warning(s)")))))

(describe "org-canvas-validate front-page rule integration"
  (it "reports the unpublished front page through the full validate run"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "pages.org" dir)
        (insert "* Course Home
:PROPERTIES:
:PUBLISHED: false
:FRONT_PAGE: true
:END:
"))
      (test-validate-create-empty-files dir '("pages.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "error:")
          (expect content :to-match "unpublished front page"))))))

(describe "org-canvas-validate drop-rules integration"
  (it "flags excess drop rules on level-2 groups through the full validate run"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "assignment-groups.org" dir)
        (insert "* Assignment Groups
** Essays
:PROPERTIES:
:WEIGHT: 30.0
:DROP_LOWEST: 2
:END:
"))
      (with-temp-file (expand-file-name "assignments.org" dir)
        (insert "* Placeholder Essay
:PROPERTIES:
:GROUP: [[file:assignment-groups.org::*Essays][Essays]]
:END:
"))
      (test-validate-create-empty-files dir '("assignment-groups.org"
                                              "assignments.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "Drop rules")
          (expect content :to-match "Essays"))))))

(describe "org-canvas-validate weight-sum integration"
  (it "reports the shortfall through the full validate run"
    (with-validate-test-dir dir
      (test-validate--weights-file dir "30.0" "30.0")
      (test-validate-create-empty-files dir '("assignment-groups.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (let ((content (buffer-string)))
          (expect content :to-match "sum to 60")))))

  (it "stays quiet for a correctly weighted file"
    (with-validate-test-dir dir
      (test-validate--weights-file dir "70.0" "30.0")
      (test-validate-create-empty-files dir '("assignment-groups.org"))
      (org-canvas-validate)
      (with-current-buffer "*canvas-validate*"
        (expect (buffer-string) :not :to-match "sum to")))))

(describe "org-canvas--validate-normalize-title (issue #164)"
  (it "strips Canvas's own disambiguators, however they stack"
    (expect (org-canvas--validate-normalize-title "Some Rubric (2)")
            :to-equal "some rubric")
    (expect (org-canvas--validate-normalize-title "Some Rubric (2) (3)")
            :to-equal "some rubric"))

  (it "strips a trailing supersede marker"
    (dolist (title '("Sprint 2 (OLD)" "Sprint 2 (old)"
                     "Sprint 2 (s25 - OLD)" "Sprint 2 (S25 - old)"))
      (expect (org-canvas--validate-normalize-title title)
              :to-equal "sprint 2")))

  (it "strips a leading do-not-use marker"
    (expect (org-canvas--validate-normalize-title "OLD DON'T USE Quiz 5")
            :to-equal "quiz 5"))

  (it "strips a semester stamp"
    (expect (org-canvas--validate-normalize-title "S24 Homework Rubric")
            :to-equal "homework rubric")
    (expect (org-canvas--validate-normalize-title "Homework Rubric (F23)")
            :to-equal "homework rubric"))

  (it "ignores case and collapsed whitespace"
    (expect (org-canvas--validate-normalize-title "  Homework   Rubric ")
            :to-equal "homework rubric"))

  (it "leaves an ordinary title alone but for case"
    (expect (org-canvas--validate-normalize-title "Week 1 Reading")
            :to-equal "week 1 reading"))

  (it "is nil for a title that normalizes away"
    (expect (org-canvas--validate-normalize-title "(2)") :to-be nil)
    (expect (org-canvas--validate-normalize-title nil) :to-be nil)))

(defun test-org-canvas-164--check (content)
  "Return the duplicate-title issues for a file holding CONTENT."
  (let* ((dir (make-temp-file "dup-test" t))
         (file (expand-file-name "assignments.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert content))
          (with-current-buffer (org-canvas--find-file-noselect file)
            (goto-char (point-min))
            (org-canvas--validate-duplicate-titles file "LEVEL=1")))
      (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(describe "org-canvas--validate-duplicate-titles (issue #164)"
  (it "says nothing when every title is distinct"
    (expect (test-org-canvas-164--check "* Week 1\n* Week 2\n") :to-be nil))

  (it "warns about twins that differ only in course-copy debris"
    (let ((issues (test-org-canvas-164--check "* Some Rubric\n* Some Rubric (2)\n")))
      (expect (length issues) :to-equal 1)
      (expect (plist-get (car issues) :severity) :to-equal 'warning)
      (expect (plist-get (car issues) :message) :to-match "2 entries share this title")
      (expect (plist-get (car issues) :message) :to-match "Some Rubric (2)")))

  (it "groups all of a run together, not pairwise"
    (let ((issues (test-org-canvas-164--check
                   "* Homework Rubric\n* Homework Rubric (2)\n* S24 Homework Rubric\n")))
      (expect (length issues) :to-equal 1)
      (expect (plist-get (car issues) :message) :to-match "3 entries")))

  (it "raises a twin still carrying a due date to an error"
    ;; The gradebook hazard: two live assignments a student cannot tell
    ;; apart, sitting in different assignment groups.
    (let* ((issues (test-org-canvas-164--check
                    (concat "* Sprint 2 - READ AND FOLLOW INSTRUCTIONS (s25 - OLD)\n"
                            ":PROPERTIES:\n:DUE_AT: <2026-11-10 Tue>\n:END:\n"
                            "* Sprint 2 - READ AND FOLLOW INSTRUCTIONS\n"
                            ":PROPERTIES:\n:DUE_AT: <2026-11-11 Wed>\n:END:\n")))
           (issue (car issues)))
      (expect (length issues) :to-equal 1)
      (expect (plist-get issue :severity) :to-equal 'error)
      (expect (plist-get issue :message) :to-match "2 of them carry a DUE_AT")
      (expect (plist-get issue :message) :to-match "2026-11-10")
      (expect (plist-get issue :message) :to-match "decide which is canonical")))

  (it "stays a warning when only one twin is scheduled"
    ;; The old copy has been unscheduled already; that is the tidy state.
    (let ((issues (test-org-canvas-164--check
                   (concat "* Sprint 2 (OLD)\n"
                           "* Sprint 2\n"
                           ":PROPERTIES:\n:DUE_AT: <2026-11-11 Wed>\n:END:\n"))))
      (expect (plist-get (car issues) :severity) :to-equal 'warning)))

  (it "names the line of each twin, so next-error reaches them"
    (let ((issues (test-org-canvas-164--check "* Quiz 5\n* OLD DON'T USE Quiz 5\n")))
      (expect (plist-get (car issues) :message) :to-match "line 1")
      (expect (plist-get (car issues) :message) :to-match "line 2")))

  (it "compares only the level the feature declares (issue #164)"
    ;; Modules opt in, and two modules may each hold an item called
    ;; "Overview"; the query keeps the children out of the comparison.
    (let* ((dir (make-temp-file "dup-level" t))
           (file (expand-file-name "modules.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Week 1\n** Overview\n* Week 2\n** Overview\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (goto-char (point-min))
              (expect (org-canvas--validate-duplicate-titles file "LEVEL=1")
                      :to-be nil)
              ;; The same file does report them when asked about level 2.
              (goto-char (point-min))
              (expect (length (org-canvas--validate-duplicate-titles file "LEVEL=2"))
                      :to-equal 1)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-directory dir t))))

  (it "reads a heading that is a link by its description"
    (expect (test-org-canvas-164--check
             "* [[https://x.example.com/a][Syllabus]]\n* Syllabus (2)\n")
            :to-be-truthy)))

(describe "duplicate-titles wiring (issue #164)"
  (it "is declared for the content types that accumulate copies"
    (dolist (key '("pages" "assignments" "quizzes" "new-quizzes"
                   "rubrics" "discussions" "announcements" "modules"))
      (expect (plist-get (gethash key org-canvas--property-registry)
                         :duplicate-titles)
              :to-be-truthy)))

  (it "is not declared for child levels, whose titles repeat by design"
    ;; Two quizzes may both have a question called "True or false".
    (dolist (key '("quiz-questions" "module-items" "new-quiz-items"))
      (expect (plist-get (gethash key org-canvas--property-registry)
                         :duplicate-titles)
              :to-be nil)))

  (it "reaches the spec the validator runs"
    (let ((spec (cl-find-if (lambda (s)
                              (eq (plist-get s :file) 'org-canvas-pages-file))
                            (org-canvas--validate-specs))))
      (expect (plist-get spec :duplicate-titles) :to-be-truthy))))

(defconst test-validate-402--deleted
  ":PROPERTIES:\n:CANVAS_ID: %s\n:DUE_AT: <2026-11-%s>\n:CANVAS_DELETED: [2026-09-28 Mon 20:37]\n:END:\n"
  "A scheduled heading a pull marked deleted; format with an id and a day.")

(defconst test-validate-402--live
  ":PROPERTIES:\n:CANVAS_ID: %s\n:DUE_AT: <2026-11-%s>\n:END:\n"
  "A scheduled heading Canvas still has; format with an id and a day.")

(defun test-validate-402--assignment-issues (content)
  "Validate an assignments.org holding CONTENT through its spec; return issues."
  (with-validate-test-dir dir
    (with-temp-file (expand-file-name "assignments.org" dir) (insert content))
    (unwind-protect
        (org-canvas--validate-spec
         (cl-find "Assignments" (org-canvas--validate-specs)
                  :key (lambda (s) (plist-get s :label)) :test #'string=))
      (let ((buf (find-buffer-visiting (expand-file-name "assignments.org" dir))))
        (when buf (kill-buffer buf))))))

(defun test-validate-402--messages (issues regexp)
  "Return the messages of ISSUES matching REGEXP."
  (seq-filter (lambda (m) (string-match-p regexp m))
              (mapcar (lambda (i) (plist-get i :message)) issues)))

(describe "headings Canvas deleted leave the cross-heading checks (issue #402)"
  (it "pairs no deleted heading with its live twin, and still warns on it"
    (let ((issues (test-validate-402--assignment-issues
                   (concat "* Sprint 3 - READ AND FOLLOW INSTRUCTIONS\n"
                           (format test-validate-402--live "1" "10 Tue")
                           "* Sprint 3 - READ AND FOLLOW INSTRUCTIONS (S25 - old)\n"
                           (format test-validate-402--deleted "2" "11 Wed")))))
      (expect (test-validate-402--messages issues "share this title") :to-equal nil)
      (expect (length (test-validate-402--messages issues "was deleted on Canvas"))
              :to-equal 1)))

  (it "still raises the error when two live twins remain beside a deleted one"
    (let* ((issues (test-validate-402--assignment-issues
                    (concat "* Sprint 2\n" (format test-validate-402--live "1" "10 Tue")
                            "* Sprint 2 (OLD)\n" (format test-validate-402--deleted "2" "11 Wed")
                            "* Sprint 2 (2)\n" (format test-validate-402--live "3" "12 Thu"))))
           (dup (cl-find-if (lambda (i) (string-match-p "share this title"
                                                        (plist-get i :message)))
                            issues)))
      (expect (plist-get dup :severity) :to-be 'error)
      (expect (plist-get dup :message) :to-match "\\`2 entries")
      (expect (plist-get dup :message) :not :to-match "(OLD)")))

  (it "leaves out a heading under one Canvas deleted, as the push does"
    (let ((file (make-temp-file "dup-402-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Group\n:PROPERTIES:\n:CANVAS_DELETED: [2026-09-28 Mon]\n:END:\n"
                      "** Outcome A\n* Other\n** Outcome A\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (goto-char (point-min))
              (expect (org-canvas--validate-duplicate-titles file "LEVEL=2")
                      :to-be nil)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "does not let a deleted group's weight into the sum"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "assignment-groups.org" dir)
        (insert "* Assignment Groups\n"
                "** Homework\n:PROPERTIES:\n:WEIGHT: 60\n:END:\n"
                "** Exams\n:PROPERTIES:\n:WEIGHT: 40\n:END:\n"
                "** Old Exams\n:PROPERTIES:\n:WEIGHT: 40\n"
                ":CANVAS_DELETED: [2026-09-28 Mon]\n:END:\n"))
      (expect (org-canvas--validate-weight-sum
               (expand-file-name "assignment-groups.org" dir))
              :to-be nil)))

  (it "does not count a deleted assignment in its group"
    (let ((file (make-temp-file "count-402-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* A1\n:PROPERTIES:\n:GROUP: Homework\n:END:\n"
                      "* A2\n:PROPERTIES:\n:GROUP: Homework\n"
                      ":CANVAS_DELETED: [2026-09-28 Mon]\n:END:\n"))
            (expect (org-canvas--count-assignments-in-group "Homework" file)
                    :to-equal 1))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "lets no item under a deleted module claim an id"
    (let ((file (make-temp-file "item-402-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Week 1\n:PROPERTIES:\n:CANVAS_DELETED: [2026-09-28 Mon]\n:END:\n"
                      "** Check 3\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
                      "* Week 2\n** Check 3\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"))
            (expect (org-canvas--validate-module-item-ids file) :to-be nil))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas--validate-module-coverage (issue #413)"
  (defun test-validate-413-write (dir files)
    "Write FILES, an alist of (NAME . TEXT), into DIR; create the rest empty."
    (test-validate-create-empty-files dir (mapcar #'car files))
    (dolist (f files)
      (with-temp-file (expand-file-name (car f) dir) (insert (cdr f)))))

  (defun test-validate-413-headings (dir)
    "Return the headings the module check warns about, killing DIR's buffers."
    (unwind-protect
        (mapcar (lambda (i) (plist-get i :heading))
                (org-canvas--validate-module-coverage))
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf)
                   (string-prefix-p (file-truename dir)
                                    (file-truename (buffer-file-name buf))))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))))

  (defconst test-validate-413-modules
    "* Week 1\n** [[file:assignments.org::*Journal 01][Journal 01]]\n** [[file:pages.org::*Syllabus][Syllabus]]\n** Readings\n"
    "A module holding one assignment and one page, and a SubHeader.")

  (it "warns on a published assignment no module item links to, and only on it"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org" . ,test-validate-413-modules)
             ("assignments.org" . "* Journal 01\n* Journal 07\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n")
             ("pages.org" . "* Syllabus\n")))
      (let ((issues (org-canvas--validate-module-coverage)))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :heading) :to-equal "Journal 07")
        (expect (plist-get (car issues) :severity) :to-be 'note)
        (expect (plist-get (car issues) :line) :to-equal 2)
        (expect (plist-get (car issues) :message) :to-match "in no module")
        (expect (plist-get (car issues) :push-only) :to-be nil))
      (test-validate-413-headings dir)))

  (it "leaves out unpublished, NO_MODULE, deleted and front-page headings"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org" . ,test-validate-413-modules)
             ("assignments.org"
              . ,(concat "* Journal 01\n"
                         "* Journal 14\n:PROPERTIES:\n:PUBLISHED: false\n:END:\n"
                         "* Extra Credit\n:PROPERTIES:\n:NO_MODULE: true\n:END:\n"
                         "* Gone\n:PROPERTIES:\n:CANVAS_ID: 9\n:CANVAS_DELETED: [2026-09-01 Tue]\n:END:\n"))
             ("pages.org" . "* Syllabus\n* Home\n:PROPERTIES:\n:FRONT_PAGE: true\n:END:\n")))
      (expect (test-validate-413-headings dir) :to-be nil)))

  (it "checks classic quizzes and discussions, not announcements"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org" . ,test-validate-413-modules)
             ("assignments.org" . "* Journal 01\n")
             ("pages.org" . "* Syllabus\n")
             ("quizzes.org" . "* Quiz 1\n** Question\n")
             ("discussions.org" . "* Introductions\n")
             ("announcements.org" . "* Welcome\n")))
      (expect (test-validate-413-headings dir)
              :to-equal '("Quiz 1" "Introductions"))))

  (it "says nothing when the modules file links to nothing"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir '(("modules.org" . "* Week 1\n** Readings\n")
             ("assignments.org" . "* Journal 07\n")))
      (expect (test-validate-413-headings dir) :to-be nil)))

  (it "says nothing without a modules file, or with the check turned off"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org" . ,test-validate-413-modules)
             ("assignments.org" . "* Journal 07\n")))
      (let ((org-canvas-validate-module-coverage nil))
        (expect (org-canvas--validate-module-coverage) :to-be nil))
      (delete-file (expand-file-name "modules.org" dir))
      (expect (test-validate-413-headings dir) :to-be nil)))

  (it "does not count an item Canvas deleted, or one under a deleted module"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org"
              . ,(concat "* Week 1\n"
                         "** [[file:assignments.org::*Journal 01][Journal 01]]\n"
                         ":PROPERTIES:\n:CANVAS_DELETED: [2026-09-01 Tue]\n:END:\n"
                         "** [[file:pages.org::*Syllabus][Syllabus]]\n"
                         "* Week 2\n:PROPERTIES:\n:CANVAS_DELETED: [2026-09-01 Tue]\n:END:\n"
                         "** [[file:assignments.org::*Journal 02][Journal 02]]\n"))
             ("assignments.org" . "* Journal 01\n* Journal 02\n")
             ("pages.org" . "* Syllabus\n")))
      (expect (test-validate-413-headings dir)
              :to-equal '("Journal 01" "Journal 02"))))

  (it "follows a link whose directory is wrong to the file beside modules.org"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir '(("modules.org"
              . "* Week 1\n** [[file:../elsewhere/assignments.org::*Journal \\[01\\]][Journal 01]]\n")
             ("assignments.org" . "* Journal [01]\n* Journal 02\n")))
      (expect (test-validate-413-headings dir) :to-equal '("Journal 02"))))

  (it "checks files only when asked, by heading link or by the file's own path"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org"
              . ,(concat "* Week 1\n"
                         "** [[file:content/a.pdf][a.pdf]]\n"
                         "** [[file:files.org::*[[file:content/b.pdf\\][b.pdf\\]]][b.pdf]]\n"))
             ("files.org"
              . ,(concat "* content\n"
                         "** [[file:content/a.pdf][a.pdf]]\n"
                         "** [[file:content/b.pdf][b.pdf]]\n"
                         "** [[file:content/c.pdf][c.pdf]]\n"
                         "** [[file:content/d.png][d.png]]\n:PROPERTIES:\n:HIDDEN: true\n:END:\n"
                         "** [[file:content/e.pdf][e.pdf]]\n:PROPERTIES:\n:PUBLISHED: false\n:END:\n"))))
      (expect (test-validate-413-headings dir) :to-be nil)
      (let ((org-canvas-validate-module-coverage '(org-canvas-files-file)))
        (expect (test-validate-413-headings dir) :to-equal '("[[file:content/c.pdf][c.pdf]]")))))

  (it "joins the validation run"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org" . ,test-validate-413-modules)
             ("assignments.org" . "* Journal 01\n* Journal 07\n")
             ("pages.org" . "* Syllabus\n")))
      (unwind-protect
          (expect (cl-count-if
                   (lambda (i) (string-match-p "in no module" (plist-get i :message)))
                   (plist-get (org-canvas--validate-run-all-specs) :issues))
                  :to-equal 1)
        (test-validate-413-headings dir))))

  (it "reports notes last, counts them apart, and fails no batch run"
    (with-validate-test-dir dir
      (test-validate-413-write
       dir `(("modules.org" . ,test-validate-413-modules)
             ("assignments.org"
              . "* Journal 01\n* Journal 07\n* Bad\n:PROPERTIES:\n:PUBLISHED: maybe\n:NO_MODULE: true\n:END:\n")
             ("pages.org" . "* Syllabus\n")))
      (unwind-protect
          (let ((errors (org-canvas-validate))
                (content (with-current-buffer "*canvas-validate*" (buffer-string))))
            (expect errors :to-equal 1)
            (expect content :to-match "1 error(s), [0-9]+ warning(s), 1 note(s) across")
            (expect (string-match-p ": error: " content) :to-be-truthy)
            (expect (< (string-match ": error: " content)
                       (string-match ": note: 'Journal 07' is published" content))
                    :to-be t))
        (test-validate-413-headings dir))))

  (it "names notes in the echo-area summary only when there are some"
    (expect (org-canvas--validate-format-summary 0 0 2)
            :to-equal "Validation: no errors or warnings, 2 note(s)")
    (expect (org-canvas--validate-format-summary 1 2 3)
            :to-equal "Validation: 1 error(s), 2 warning(s), 3 note(s)")
    (expect (org-canvas--validate-format-summary 0 1 1)
            :to-equal "Validation: 1 warning(s), no errors, 1 note(s)")
    (expect (org-canvas--validate-format-summary 0 0 0)
            :to-equal "Validation passed: no issues found"))

  (it "moves notes after errors and warnings, keeping each group's order"
    (let ((n1 '(:severity note :message "n1")) (w '(:severity warning :message "w"))
          (n2 '(:severity note :message "n2")) (e '(:severity error :message "e")))
      (expect (org-canvas--validate-notes-last (list n1 w n2 e))
              :to-equal (list w e n1 n2))))

  (it "lets next-error visit a note at info level"
    (with-temp-buffer
      (org-canvas-validate-mode)
      (let ((rule (assq 'org-canvas-validate-note compilation-error-regexp-alist)))
        (expect (nth 5 rule) :to-equal 0)
        (expect (string-match-p (nth 1 rule) "/tmp/a.org:3: note: x") :to-be-truthy))))

  (it "registers NO_MODULE as a local-only boolean on each checked type"
    (dolist (feature '("assignments" "pages" "quizzes" "discussions" "files"))
      (let* ((props (plist-get (gethash feature org-canvas--property-registry)
                               :properties))
             (spec (cl-find "NO_MODULE" props
                            :key (lambda (p) (plist-get p :org-prop))
                            :test #'equal))
             ;; Bound outside `expect': Emacs 29's oclosure shadows :type.
             (spec-type (plist-get spec :type)))
        (expect (plist-get spec :local-only) :to-be t)
        (expect spec-type :to-be 'boolean)
        (expect (plist-get spec :api-key) :to-be nil)))))

(defun test-validate-538--write-course (dir)
  "Write three assignment headings into DIR, two with an accept."
  (test-validate-create-empty-files dir)
  (with-temp-file (expand-file-name "assignments.org" dir)
    (insert "* Exam\n:PROPERTIES:\n:SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n"
            ":VALIDATE_ACCEPT: lock-window In-class exam\n:NO_MODULE: true\n:END:\n"
            "* Lab\n:PROPERTIES:\n:SUBMISSION: online_upload\n:DUE_AT: <2099-03-11 Wed 23:59>\n:NO_MODULE: true\n:END:\n"
            "* Quiz\n:PROPERTIES:\n:SUBMISSION: online_upload\n:DUE_AT: <2099-03-12 Thu 23:59>\n"
            ":VALIDATE_ACCEPT: lock-window Proctored\n:NO_MODULE: true\n:END:\n")))

(defun test-validate-538--cleanup (dir)
  "Kill the buffers visiting files in DIR."
  (dolist (buf (buffer-list))
    (when (and (buffer-file-name buf)
               (string-prefix-p (file-truename dir)
                                (file-truename (buffer-file-name buf))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(describe "accepted findings in the report (issue #538)"
  (it "takes each heading's finding out of the report, counting it"
    (with-validate-test-dir dir
      (test-validate-538--write-course dir)
      (unwind-protect
          (let* ((org-canvas-validate-lock-after-due-hours 48)
                 (org-canvas-read-only nil)
                 (data (org-canvas--validate-collect))
                 (stats (plist-get data :stats))
                 (lock (cl-remove-if-not
                        (lambda (i) (eq (plist-get i :check) 'lock-window))
                        (plist-get data :issues))))
            (expect (mapcar (lambda (i) (plist-get i :heading)) lock)
                    :to-equal '("Lab"))
            (expect (mapcar (lambda (i) (list (plist-get i :heading)
                                              (plist-get i :accepted)))
                            (plist-get data :accepted))
                    :to-equal '(("Exam" "In-class exam") ("Quiz" "Proctored")))
            (expect (plist-get stats :accepted) :to-equal 2))
        (test-validate-538--cleanup dir))))

  (it "names the count in one line, and lists them with --all"
    (with-validate-test-dir dir
      (test-validate-538--write-course dir)
      (unwind-protect
          (let ((org-canvas-validate-lock-after-due-hours 48)
                (org-canvas-read-only nil))
            (org-canvas-validate)
            (let ((content (with-current-buffer "*canvas-validate*" (buffer-string))))
              (expect content :to-match
                      (concat "^" (regexp-quote "2 finding(s) accepted with a reason (VALIDATE_ACCEPT, Accept cells); M-x org-canvas-validate-all lists them") "$"))
              (expect content :not :to-match "reason: "))
            (org-canvas-validate-all)
            (let ((content (with-current-buffer "*canvas-validate*" (buffer-string))))
              (expect content :to-match
                      (regexp-quote "Accepted (2), each with the reason it is kept:"))
              (expect content :to-match
                      "assignments.org:1: accepted warning: Lock window missing: .*\n    reason: In-class exam$")
              (expect content :not :to-match "finding(s) accepted with a reason")))
        (test-validate-538--cleanup dir))))

  (it "holds a push-only finding back on a read-only course before any accept"
    (let ((issue (list :severity 'warning :line 1 :push-only t :accepted "Why")))
      (spy-on 'org-canvas--validate-run-all-specs
              :and-return-value (list :issues (list issue) :checked 1 :skipped 0))
      (let* ((org-canvas-read-only t)
             (stats (plist-get (org-canvas--validate-collect) :stats)))
        (expect (plist-get stats :suppressed) :to-equal 1)
        (expect (plist-get stats :accepted) :to-equal 0)))))

(provide 'org-canvas-validate-test)
;;; org-canvas-validate-test.el ends here
