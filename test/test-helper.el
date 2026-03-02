;;; test-helper.el --- Test helper for org-canvas tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Common fixtures, macros, and utilities for org-canvas Buttercup tests.
;;
;; Code coverage is handled by Eldev's undercover plugin.
;; Run with: eldev test --undercover
;; See coverage report: coverage/lcov.info

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)

;;;; Test Configuration

(defvar test-org-canvas-emacs-30-p (>= emacs-major-version 30)
  "Non-nil if running on Emacs 30 or later.
Some org-mode functions behave differently in Emacs 29.x regarding
heading structure recognition in programmatic buffers.")

(defvar test-org-canvas-base-url "https://test.canvas.example.com"
  "Test Canvas base URL.")

(defvar test-org-canvas-api-token "test-token-12345"
  "Test API token.")

(defvar test-org-canvas-course-id "99999"
  "Test course ID.")

(defvar test-org-canvas-request-timeout 5
  "Test timeout in seconds.")

;;;; API Mocking

(defvar test-org-canvas-api-calls nil
  "List of recorded API calls: ((method url data) ...).")

(defvar test-org-canvas-api-responses nil
  "Alist of (url-pattern . response) for mock responses.")

(defun test-org-canvas-mock-api-request (_method _url &rest _args)
  "Mock API request function that records calls and returns mock responses."
  (let ((call (list _method _url (plist-get _args :data))))
    (push call test-org-canvas-api-calls)
    ;; Find matching response
    (let ((response nil))
      (cl-loop for (pattern . resp) in test-org-canvas-api-responses
               when (string-match-p pattern _url)
               do (setq response resp)
               and return nil)
      (or response '((id . 12345) (name . "Mock Response"))))))

(defun test-org-canvas-api-called-p (method url-pattern)
  "Check if API was called with METHOD and URL matching URL-PATTERN."
  (cl-some (lambda (call)
             (and (eq (car call) method)
                  (string-match-p url-pattern (cadr call))))
           test-org-canvas-api-calls))

(defun test-org-canvas-api-call-count ()
  "Return the number of API calls made."
  (length test-org-canvas-api-calls))

(defun test-org-canvas-last-api-call ()
  "Return the most recent API call."
  (car test-org-canvas-api-calls))

;;;; Macros

(defmacro with-org-canvas-test-config (&rest body)
  "Execute BODY with test Canvas configuration."
  (declare (indent 0))
  `(let ((org-canvas-base-url test-org-canvas-base-url)
         (org-canvas-api-token test-org-canvas-api-token)
         (org-canvas-course-id test-org-canvas-course-id)
         (org-canvas-request-timeout test-org-canvas-request-timeout))
     ,@body))

(defmacro with-mock-api (&rest body)
  "Execute BODY with mocked API calls."
  (declare (indent 0))
  `(let ((test-org-canvas-api-calls nil)
         (test-org-canvas-api-responses nil))
     (cl-letf (((symbol-function 'org-canvas-api-request)
                #'test-org-canvas-mock-api-request))
       ,@body)))

(defmacro with-temp-org-buffer (content &rest body)
  "Create a temp buffer with CONTENT in Org mode, then execute BODY.
Ensures proper org-mode initialization for consistent behavior
across Emacs versions."
  (declare (indent 1))
  `(let ((temp-file (make-temp-file "org-test-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file temp-file
             (insert ,content))
           (with-current-buffer (find-file-noselect temp-file)
             (unwind-protect
                 (progn
                   (goto-char (point-min))
                   ,@body)
               (kill-buffer))))
       (delete-file temp-file))))

(defmacro with-sync-test-env (&rest body)
  "Execute BODY with sync infrastructure mocked.
Suppresses `org-canvas-clear-log' and `display-buffer' side effects."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-canvas-clear-log) (lambda () nil))
             ((symbol-function 'display-buffer) (lambda (_) nil)))
     ,@body))

(defmacro with-html-to-org-identity (&rest body)
  "Execute BODY with `org-canvas--html-to-org' as identity function."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-canvas--html-to-org) (lambda (html) html)))
     ,@body))

(defmacro with-nonexistent-canvas-files (&rest body)
  "Execute BODY with all `org-canvas-*-file' vars set to nonexistent paths."
  (declare (indent 0))
  `(let ((org-canvas-assignments-file "/tmp/nonexistent/assignments.org")
         (org-canvas-pages-file "/tmp/nonexistent/pages.org")
         (org-canvas-quizzes-file "/tmp/nonexistent/quizzes.org")
         (org-canvas-modules-file "/tmp/nonexistent/modules.org")
         (org-canvas-files-file "/tmp/nonexistent/files.org")
         (org-canvas-outcomes-file "/tmp/nonexistent/outcomes.org")
         (org-canvas-rubrics-file "/tmp/nonexistent/rubrics.org")
         (org-canvas-discussions-file "/tmp/nonexistent/discussions.org")
         (org-canvas-announcements-file "/tmp/nonexistent/announcements.org")
         (org-canvas-assignment-groups-file "/tmp/nonexistent/assignment-groups.org")
         (org-canvas-sections-file "/tmp/nonexistent/sections.org")
         (org-canvas-new-quizzes-file "/tmp/nonexistent/new-quizzes.org")
         (org-canvas-group-categories-file "/tmp/nonexistent/group-categories.org")
         (org-canvas-calendar-events-file "/tmp/nonexistent/calendar.org"))
     ,@body))

;;;; Test Generators

(defun test-org-canvas-define-common-parse-tests (parse-fn &rest opts)
  "Generate 4 standard parse-entry tests for PARSE-FN.
Call inside a `describe' block.  OPTS is a plist:
  :id-property  Org property name (default \"CANVAS_ID\")
  :id-key       Plist key in parsed data (default :canvas-id)
  :extra-props  Additional property lines to insert (e.g. \":START_AT: ...\n\")
  :body         Body text after properties (default \"\nBody.\n\")"
  (let ((id-property (or (plist-get opts :id-property) "CANVAS_ID"))
        (id-key (or (plist-get opts :id-key) :canvas-id))
        (extra-props (or (plist-get opts :extra-props) ""))
        (body (or (plist-get opts :body) "\nBody.\n")))

    (it "includes pom in data"
      (with-temp-org-buffer
       (concat "* Test\n:PROPERTIES:\n" extra-props ":END:\n" body)
       (org-back-to-heading)
       (let ((data (funcall parse-fn)))
         (expect (plist-get data :pom) :to-be-truthy))))

    (it "extracts canvas-id when present"
      (with-temp-org-buffer
       (concat "* Test\n:PROPERTIES:\n:" id-property ": 12345\n"
               extra-props ":END:\n" body)
       (org-back-to-heading)
       (let ((data (funcall parse-fn)))
         (expect (plist-get data id-key) :to-equal "12345"))))

    (it "returns nil canvas-id for new items"
      (with-temp-org-buffer
       (concat "* Test\n:PROPERTIES:\n" extra-props ":END:\n" body)
       (org-back-to-heading)
       (let ((data (funcall parse-fn)))
         (expect (plist-get data id-key) :to-be nil))))

    (it "errors on empty title"
      (with-temp-org-buffer
       (concat "* \n:PROPERTIES:\n" extra-props ":END:\n" body)
       (org-back-to-heading)
       (expect (funcall parse-fn) :to-throw 'error)))))

(defmacro with-pull-property-test (pull-fn response property matcher value)
  "Test that PULL-FN with RESPONSE sets Org PROPERTY as expected.
Creates a temp buffer with CANVAS_ID: 1, mocks html-to-org as identity,
calls PULL-FN, and asserts (expect (org-entry-get (point) PROPERTY) MATCHER VALUE)."
  (declare (indent 2))
  `(with-temp-org-buffer
    "* Test\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
    (org-back-to-heading)
    (with-html-to-org-identity
      (funcall ,pull-fn ,response (point))
      (expect (org-entry-get (point) ,property) ,matcher ,value))))

;;;; Assertion Helpers

(defun expect-api-called (method endpoint)
  "Assert that the mock API was called with METHOD matching ENDPOINT."
  (expect (test-org-canvas-api-called-p method endpoint) :to-be-truthy))

(defun expect-synced-timestamp (pom)
  "Assert that LAST_SYNCED at POM is a valid recent timestamp."
  (expect (org-entry-get pom "LAST_SYNCED") :to-match "^\\[20[0-9][0-9]-"))

(provide 'test-helper)
;;; test-helper.el ends here
