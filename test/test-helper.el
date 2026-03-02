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

(defun expect-api-called (method endpoint)
  "Assert that the mock API was called with METHOD matching ENDPOINT."
  (expect (test-org-canvas-api-called-p method endpoint) :to-be-truthy))

(defun expect-synced-timestamp (pom)
  "Assert that LAST_SYNCED at POM is a valid recent timestamp."
  (expect (org-entry-get pom "LAST_SYNCED") :to-match "^\\[20[0-9][0-9]-"))

;;;; Sample Data Generators

(defun test-org-canvas-make-announcement-org ()
  "Return sample announcement Org content."
  "* Test Announcement
:PROPERTIES:
:PUBLISHED: true
:DELAYED_POST_AT: <2024-01-15 Mon 09:00>
:END:

This is the announcement body.
")

(defun test-org-canvas-make-page-org ()
  "Return sample page Org content."
  "* Test Page
:PROPERTIES:
:PUBLISHED: true
:FRONT_PAGE: false
:EDITING_ROLES: teachers
:END:

This is the page body content.
")

(provide 'test-helper)
;;; test-helper.el ends here
