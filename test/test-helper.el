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

;; Suppress "Non-existent agenda file" prompt in batch mode.
;; Org checks agenda files when opening .org buffers; temp files deleted
;; during test cleanup trigger an interactive prompt that hangs batch runs.
(when noninteractive
  (defun org-check-agenda-file (_file) nil))

;; Redirect logging away from the user's real `org-canvas-directory'
;; (typically pinned by `org-canvas-credentials.el') to a per-process
;; temp directory.  Without this, tests write to a shared real log
;; file and create `.#org-canvas.log' locks; the pre-push hook runs
;; Emacs 29 and Emacs 30 test jobs in parallel, so the locks collide
;; and one job aborts.  We rebind the directory itself rather than
;; `org-canvas-log-destination' because a test asserts that the
;; defcustom default remains `both'.
(setq org-canvas-directory
      (file-name-as-directory
       (make-temp-file "org-canvas-test-" t)))

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
  "Mock API request function that records calls and returns mock responses.
Note: only METHOD, URL, and :data are recorded; :params and :timeout are dropped."
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
  `(let ((temp-file (make-temp-file "org-test-" nil ".org"))
         (org-agenda-files nil))
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

(defun test-org-canvas-define-common-transform-tests (transform-fn &rest opts)
  "Generate standard transform-props tests for TRANSFORM-FN.
Call inside a `describe' block.  OPTS is a plist:
  :defaults - Default raw plist for inputs (required)
  :title-key - Output plist key for title (default :title)
  :tests - List of test specs, each a plist:
    :raw-key     Raw input plist key
    :data-key    Output plist key to check
    :type        One of: boolean, timestamp, string
    :default     For boolean: default value when absent"
  (let ((defaults (plist-get opts :defaults))
        (title-key (or (plist-get opts :title-key) :title))
        (tests (plist-get opts :tests)))

    (it "strips statistics cookie from title"
      (let* ((input (plist-put (copy-sequence defaults) :title-raw "My Title [1/3]"))
             (result (funcall transform-fn input)))
        (expect (plist-get result title-key) :to-equal "My Title")))

    (dolist (spec tests)
      (let ((raw-key (plist-get spec :raw-key))
            (data-key (plist-get spec :data-key))
            (type (plist-get spec :type))
            (default (plist-get spec :default)))
        (pcase type
          ('boolean
           (if default
               (progn
                 (it (format "defaults %s to %s" data-key default)
                   (let* ((input (plist-put (copy-sequence defaults) raw-key nil))
                          (result (funcall transform-fn input)))
                     (expect (plist-get result data-key) :to-be default)))
                 (it (format "interprets %s=false" data-key)
                   (let* ((input (plist-put (copy-sequence defaults) raw-key "false"))
                          (result (funcall transform-fn input)))
                     (expect (plist-get result data-key) :to-be nil))))
             (it (format "interprets %s=true" data-key)
               (let* ((input (plist-put (copy-sequence defaults) raw-key "true"))
                      (result (funcall transform-fn input)))
                 (expect (plist-get result data-key) :to-be t)))
             (it (format "returns nil for absent %s" data-key)
               (let* ((input (plist-put (copy-sequence defaults) raw-key nil))
                      (result (funcall transform-fn input)))
                 (expect (plist-get result data-key) :to-be nil)))))
          ('timestamp
           (it (format "parses %s to ISO8601" data-key)
             (let* ((input (plist-put (copy-sequence defaults)
                                      raw-key "<2026-06-15 Mon 10:00>"))
                    (result (funcall transform-fn input)))
               (expect (plist-get result data-key) :to-match "2026-06-15T")))
           (it (format "returns nil for absent %s" data-key)
             (let* ((input (plist-put (copy-sequence defaults) raw-key nil))
                    (result (funcall transform-fn input)))
               (expect (plist-get result data-key) :to-be nil)))))))))

(defun test-org-canvas-define-common-push-tests (push-fn &rest opts)
  "Generate POST/PUT selection tests for PUSH-FN.
Call inside a `describe' block.  OPTS is a plist:
  :endpoint - API endpoint string (required)
  :id-key - Plist key for Canvas ID (default :canvas-id)"
  (let ((endpoint (plist-get opts :endpoint))
        (id-key (or (plist-get opts :id-key) :canvas-id)))

    (it "uses POST for new items"
      (with-org-canvas-test-config
        (with-mock-api
          (funcall push-fn (list id-key nil :title "Test" :pom 1) '((a . 1)))
          (expect-api-called 'POST endpoint))))

    (it "uses PUT for existing items"
      (with-org-canvas-test-config
        (with-mock-api
          (funcall push-fn (list id-key "42" :title "Test" :pom 1) '((a . 1)))
          (expect-api-called 'PUT endpoint))))))

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

;;;; Mock Data Builders

(defun test-org-canvas-make-response (defaults &optional overrides)
  "Merge OVERRIDES into DEFAULTS alist, returning a new alist.
Keys in OVERRIDES win over keys in DEFAULTS."
  (let ((result (copy-alist defaults)))
    (dolist (pair overrides)
      (setf (alist-get (car pair) result nil nil #'equal) (cdr pair)))
    result))

(defun test-org-canvas-make-page (&optional overrides)
  "Build a mock Canvas page API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((url . "test-page")
     (title . "Test Page")
     (body . "<p>Page body</p>")
     (published . t)
     (front_page . :json-false)
     (editing_roles . "teachers")
     (created_at . "2026-01-01T00:00:00Z")
     (updated_at . "2026-01-01T00:00:00Z"))
   overrides))

(defun test-org-canvas-make-assignment (&optional overrides)
  "Build a mock Canvas assignment API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 1001)
     (name . "Test Assignment")
     (description . "<p>Do the work</p>")
     (points_possible . 100)
     (grading_type . "points")
     (published . t)
     (submission_types . ("online_upload"))
     (due_at . "2026-06-01T23:59:00Z")
     (created_at . "2026-01-01T00:00:00Z")
     (updated_at . "2026-01-01T00:00:00Z"))
   overrides))

(defun test-org-canvas-make-discussion (&optional overrides)
  "Build a mock Canvas discussion API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 2001)
     (title . "Test Discussion")
     (message . "<p>Discuss this</p>")
     (discussion_type . "side_comment")
     (published . t)
     (pinned . :json-false)
     (require_initial_post . :json-false))
   overrides))

(defun test-org-canvas-make-announcement (&optional overrides)
  "Build a mock Canvas announcement API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 3001)
     (title . "Test Announcement")
     (message . "<p>Attention</p>")
     (is_announcement . t)
     (published . t))
   overrides))

(defun test-org-canvas-make-module (&optional overrides)
  "Build a mock Canvas module API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 4001)
     (name . "Test Module")
     (position . 1)
     (published . t)
     (items_count . 0)
     (items_url . "https://test.canvas.example.com/api/v1/courses/99999/modules/4001/items"))
   overrides))

(defun test-org-canvas-make-rubric (&optional overrides)
  "Build a mock Canvas rubric API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 5001)
     (title . "Test Rubric")
     (points_possible . 20)
     (free_form_criterion_comments . :json-false))
   overrides))

(defun test-org-canvas-make-quiz (&optional overrides)
  "Build a mock Canvas quiz API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 6001)
     (title . "Test Quiz")
     (quiz_type . "assignment")
     (published . t)
     (time_limit . 30)
     (points_possible . 50))
   overrides))

(defun test-org-canvas-make-calendar-event (&optional overrides)
  "Build a mock Canvas calendar event API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 7001)
     (title . "Test Event")
     (start_at . "2026-06-01T14:00:00Z")
     (end_at . "2026-06-01T15:00:00Z")
     (all_day . :json-false)
     (description . "<p>Event description</p>"))
   overrides))

(defun test-org-canvas-make-outcome (&optional overrides)
  "Build a mock Canvas outcome API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 8001)
     (title . "Test Outcome")
     (calculation_method . "highest")
     (mastery_points . 3)
     (points_possible . 5))
   overrides))

(defun test-org-canvas-make-group-category (&optional overrides)
  "Build a mock Canvas group category API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 9001)
     (name . "Test Group Category")
     (self_signup . nil)
     (group_limit . nil)
     (auto_leader . nil))
   overrides))

(defun test-org-canvas-transient--suffix-command (suffix)
  "Extract the :command from a transient SUFFIX entry, or nil."
  (when (listp suffix)
    (let ((plist (if (numberp (car suffix))
                     (nth 2 suffix)
                   (cdr suffix))))
      (plist-get plist :command))))

(defun test-org-canvas-transient--column-has-command-p (col command)
  "Return non-nil if transient column vector COL contains COMMAND."
  (when (vectorp col)
    (let* ((has-level (numberp (aref col 0)))
           (suffixes (aref col (if has-level 3 2))))
      (cl-some (lambda (s) (eq (test-org-canvas-transient--suffix-command s) command))
               suffixes))))

(defun test-org-canvas-transient-has-command-p (prefix-sym command)
  "Return non-nil if PREFIX-SYM's transient layout contains COMMAND.
Handles both Emacs 29 (transient 0.4) and Emacs 30 (transient 0.7+) layout formats."
  (let* ((layout (get prefix-sym 'transient--layout))
         (columns (cond
                   ((listp layout) layout)
                   ((vectorp layout) (aref layout 2)))))
    (cl-some (lambda (col) (test-org-canvas-transient--column-has-command-p col command))
             columns)))

(provide 'test-helper)
;;; test-helper.el ends here
