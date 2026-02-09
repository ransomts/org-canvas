;;; org-canvas-core.el --- Core utilities for org-canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains the core, shared components of the org-canvas package.
;; It provides the foundation that all feature modules build upon.
;;
;; ARCHITECTURE OVERVIEW
;; =====================
;; The core module is organized into layers:
;;
;;   1. Configuration Layer - User customizations (API token, course ID, paths)
;;   2. Logging Layer       - Debug/info/error logging via elog
;;   3. API Layer           - HTTP communication with Canvas REST API
;;   4. Org Interaction     - Reading/writing Org properties and timestamps
;;   5. Diagnostics         - Connection testing
;;   6. Sync Pipeline       - Macros for defining sync functions
;;   7. Push-to-API         - Generic helpers for POST/PUT with error recovery
;;   8. Delete              - Macros for defining delete functions
;;
;; DEPENDENCY RULES
;; ================
;; - All feature modules require org-canvas-core
;; - Feature modules must NOT depend on each other
;; - This file must NOT import any feature modules (prevents circular deps)
;;
;; 4-STAGE PIPELINE PATTERN
;; ========================
;; Every feature module follows this consistent pattern:
;;
;;   1. Parse      - Extract data from Org heading properties
;;   2. Build      - Convert to Canvas API format (hash-tables)
;;   3. Execute    - Call API with timeout/404 recovery
;;   4. Finalize   - Save CANVAS_ID and LAST_SYNCED to Org file
;;
;; Use `org-canvas-define-sync' macro to generate sync functions that
;; follow this pattern with proper logging and error handling.

;;; Code:

(require 'cl-lib)
(require 'elog)
(require 'json)
(require 'org)
(require 'plz)
(require 'subr-x)
(require 'url-util)

;;;; 1. Configuration Layer

(defgroup org-canvas nil
  "Interface for Canvas LMS."
  :group 'external)

(defcustom org-canvas-directory ""
  "The root directory for the Canvas course files.
All feature files (pages.org, assignments.org, etc.)
will be resolved relative to this path."
  :type 'directory
  :group 'org-canvas)

(defcustom org-canvas-base-url "https://canvas.instructure.com"
  "The base URL for the Canvas instance."
  :type 'string
  :group 'org-canvas)

(defcustom org-canvas-api-token ""
  "The API token for Canvas.
It is recommended to set this in org-canvas-credentials.el instead of here."
  :type 'string
  :group 'org-canvas)

(defcustom org-canvas-course-id ""
  "The ID of the course to sync with.
It is recommended to set this in org-canvas-credentials.el instead of here."
  :type 'string
  :group 'org-canvas)

(defcustom org-canvas-request-timeout 15
  "Timeout for API requests in seconds.
Default is 15 seconds.  If a request times out (e.g. slow page generation),
the system will check if the operation succeeded on the server."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-log-request-bodies nil
  "Whether to log API request and response bodies.
When non-nil, full JSON payloads are logged to *canvas-log*.
Set to nil to reduce log verbosity or hide potentially sensitive data."
  :type 'boolean
  :group 'org-canvas)

(defcustom org-canvas-rate-limit-retries 3
  "Number of times to retry after a rate-limit (429/403) response."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-rate-limit-wait 10
  "Seconds to wait between rate-limit retries."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-delete-concurrency 8
  "Maximum number of concurrent DELETE requests during mass deletion.
Used by `org-canvas--delete-items-queued' via `plz-queue'."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-detect-conflicts t
  "When non-nil, check for remote changes before overwriting.
Before a PUT request, GET the item from Canvas and compare its
`updated_at' timestamp with the local `LAST_SYNCED'.  If Canvas
is newer, warn and skip the item.  Set to nil or use
`org-canvas-force-push' to bypass."
  :type 'boolean
  :group 'org-canvas)

;; Attempt to load credentials from a separate file if present
(require 'org-canvas-credentials nil t)

;;; --- Path Utilities ---

(defun org-canvas--path (filename)
  "Return the absolute path to FILENAME in `org-canvas-directory`.
Falls back to `org-directory` or `user-emacs-directory`
if `org-canvas-directory` is nil."
  (let ((base (or org-canvas-directory
		  (if (boundp 'org-directory) org-directory user-emacs-directory))))
    (expand-file-name filename base)))

;;;; 2. Logging Layer

(defcustom org-canvas-log-destination 'both
  "Where org-canvas writes log output.
- buffer: Log to the *canvas-log* buffer
- file: Log to a file
- both: Log to buffer and file (default, see `org-canvas-log-file')"
  :type '(choice (const :tag "Buffer only" buffer)
		 (const :tag "File only" file)
		 (const :tag "Buffer and file" both))
  :group 'org-canvas)

(defcustom org-canvas-log-file nil
  "File path for log output when `org-canvas-log-destination' includes file.
When nil, defaults to \"org-canvas.log\" in `org-canvas-directory'."
  :type '(choice (const :tag "Default (org-canvas.log)" nil)
		 (file :tag "Custom file path"))
  :group 'org-canvas)

(defun org-canvas--log-handlers (destination)
  "Return the elog handler list for DESTINATION symbol.
DESTINATION should be one of: buffer, file, both."
  (pcase destination
    ('buffer '(buffer))
    ('file '(file))
    ('both '(buffer file))
    (_ '(buffer))))

(defun org-canvas--log-file-path ()
  "Return the effective log file path."
  (or org-canvas-log-file
      (org-canvas--path "org-canvas.log")))

(defvar org-canvas--logger
  (elog-logger :name "org-canvas"
	       :handlers (org-canvas--log-handlers org-canvas-log-destination)
	       :buffer "*canvas-log*"
	       :file (when (memq org-canvas-log-destination '(file both))
		       (org-canvas--log-file-path))
	       :level 'debug)
  "Logger for Org Canvas operations.")

(defcustom org-canvas-log-level 'debug
  "Logging level for org-canvas operations.
Available levels (from most to least verbose):
- trace: Full API request/response bodies (WARNING: may contain sensitive data)
- debug: Detailed operation info
- info: Normal operation messages
- warning: Potential issues
- error: Errors only

Note: Setting this to `trace' or `debug' also requires updating the
elog logger level.  Use `org-canvas-set-log-level' for convenience."
  :type '(choice (const :tag "Trace (full API details)" trace)
		 (const :tag "Debug" debug)
		 (const :tag "Info" info)
		 (const :tag "Warning" warning)
		 (const :tag "Error" error))
  :group 'org-canvas)

(defun org-canvas--trace-enabled-p ()
  "Return non-nil if trace logging is enabled."
  (eq org-canvas-log-level 'trace))

(defun org-canvas--trace (format-string &rest args)
  "Log a trace message with FORMAT-STRING and ARGS.
Trace messages show full API request/response details."
  (when (org-canvas--trace-enabled-p)
    (apply #'elog-debug org-canvas--logger
	   (concat "[TRACE] " format-string) args)))

(defun org-canvas--mask-token (headers)
  "Return a copy of HEADERS with the Authorization token masked."
  (mapcar (lambda (h)
	    (if (string= (car h) "Authorization")
		(cons "Authorization" "Bearer ***MASKED***")
	      h))
	  headers))

(defun org-canvas--pretty-json (data)
  "Return a pretty-printed JSON string for DATA.
Handles hash-tables, alists, and already-encoded strings."
  (cond
   ((null data) "null")
   ((stringp data)
    ;; Try to parse and re-encode for pretty printing
    (condition-case nil
	(let ((parsed (json-read-from-string data)))
	  (let ((json-encoding-pretty-print t))
	    (json-encode parsed)))
      (error data)))
   (t
    (let ((json-encoding-pretty-print t))
      (json-encode data)))))

(defvar org-canvas--inhibit-log-clear nil
  "When non-nil, `org-canvas-clear-log' is a no-op.
Bound to t by `org-canvas-sync' so sub-sync phases preserve each other's logs.")

(defun org-canvas-clear-log ()
  "Clear the log buffer and log file (when file logging is active).
Does nothing when `org-canvas--inhibit-log-clear' is non-nil."
  (unless org-canvas--inhibit-log-clear
    (with-current-buffer (get-buffer-create "*canvas-log*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (read-only-mode -1)))
    (when (memq org-canvas-log-destination '(file both))
      (let ((log-file (org-canvas--log-file-path)))
        (when (file-exists-p log-file)
          (delete-file log-file))))))

(defun org-canvas-set-log-level (level)
  "Set the logging level to LEVEL interactively.
LEVEL should be one of: trace, debug, info, warning, error."
  (interactive
   (list (intern (completing-read "Log level: "
				  '("trace" "debug" "info" "warning" "error")
				  nil t))))
  (setq org-canvas-log-level level)
  ;; Map 'trace to 'debug for the underlying logger, as we handle trace
  ;; filtering manually in `org-canvas--trace`.
  (let ((elog-target-level (if (eq level 'trace) 'debug level)))
    ;; Use elog-set-level which safely handles the plist update
    (setq org-canvas--logger
	  (elog-set-level org-canvas--logger elog-target-level)))
  (message "org-canvas log level set to: %s" level))

(defun org-canvas-set-log-destination (destination)
  "Set the log destination to DESTINATION interactively.
DESTINATION should be one of: buffer, file, both."
  (interactive
   (list (intern (completing-read "Log destination: "
				  '("buffer" "file" "both")
				  nil t))))
  (setq org-canvas-log-destination destination)
  (setq org-canvas--logger
	(elog-set-handlers org-canvas--logger
			   (org-canvas--log-handlers destination)))
  (when (memq destination '(file both))
    (setq org-canvas--logger
	  (elog-set-file org-canvas--logger (org-canvas--log-file-path))))
  (message "org-canvas log destination set to: %s" destination))

;;;; 3. API Layer
;;
;; The API layer handles all HTTP communication with Canvas.
;; Key features:
;;   - Automatic JSON encoding/decoding
;;   - Request timeout handling (configurable via org-canvas-request-timeout)
;;   - Debug logging with curl command generation for troubleshooting
;;   - Error normalization (plz-error -> standard error signal)

(defun org-canvas-api-course-endpoint (suffix &rest args)
  "Construct a course-specific endpoint URL.
SUFFIX is the path after /courses/:id/.  ARGS are format arguments."
  (format "%s/api/v1/courses/%s/%s"
	  org-canvas-base-url
	  org-canvas-course-id
	  (apply #'format suffix args)))

(defun org-canvas--build-curl-command (method full-url json-payload)
  "Build a curl command string for debugging.
METHOD is the HTTP method, FULL-URL is the complete URL with query
params, JSON-PAYLOAD is the JSON body (or nil).  Uses $CANVAS_TOKEN
placeholder for the API token (requires double quotes for expansion)."
  (let ((parts (list "curl")))
    ;; Method (skip for GET as it's default)
    (unless (eq method 'GET)
      (push (format "-X %s" method) parts))
    ;; Headers - use double quotes so $CANVAS_TOKEN expands
    (push "-H \"Authorization: Bearer $CANVAS_TOKEN\"" parts)
    (push "-H \"Content-Type: application/json\"" parts)
    ;; Data payload - escape double quotes in JSON
    (when json-payload
      (push (format "-d '%s'" json-payload) parts))
    ;; URL (double quoted in case of special chars)
    (push (format "\"%s\"" full-url) parts)
    ;; Join in reverse order (we pushed, so it's backwards)
    (mapconcat #'identity (nreverse parts) " \\\n     ")))

(defun org-canvas--ensure-credentials ()
  "Signal error if API token or course ID are not configured."
  (when (or (null org-canvas-api-token) (string-empty-p org-canvas-api-token))
    (error "API token not configured.  Set org-canvas-api-token in org-canvas-credentials.el"))
  (when (or (null org-canvas-course-id) (string-empty-p org-canvas-course-id))
    (error "Course ID not configured.  Set org-canvas-course-id in org-canvas-credentials.el")))

(defun org-canvas--api-handle-plz-error (err full-url)
  "Handle a plz-error ERR from a request to FULL-URL.
Return `:retry' if the request should be retried (after sleeping),
or signal an error for terminal failures."
  (let* ((plz-err (cdr err))
         (response (and (plz-error-p plz-err)
                        (plz-error-response plz-err)))
         (status (and response (plz-response-status response)))
         (body (and response (plz-response-body response)))
         (err-msg (if status
                      (format "API Request Failed (HTTP %s)" status)
                    (format "API Request Failed: %S" plz-err))))
    (elog-debug org-canvas--logger "[API] <<< RESPONSE: %s" (or status "error"))
    (cond
     ;; Rate limited (429 or 403 with rate limit indication)
     ((and status (or (= status 429)
                      (and (= status 403)
                           body (string-match-p "rate" (format "%s" body)))))
      (elog-warning org-canvas--logger
        "[API] Rate limited (HTTP %d). Waiting %ds..."
        status org-canvas-rate-limit-wait)
      (sleep-for org-canvas-rate-limit-wait)
      :retry)

     ;; Authentication failure (401)
     ((and status (= status 401))
      (signal 'error
        (list "Authentication failed (HTTP 401). Your API token may have expired.\nGenerate a new one at Canvas > Account > Settings > Approved Integrations."
              body plz-err)))

     ;; Forbidden (403, non-rate-limit)
     ((and status (= status 403))
      (signal 'error
        (list "Permission denied (HTTP 403). Your token may lack the required scope for this operation."
              body plz-err)))

     ;; Generic error
     (t
      (elog-error org-canvas--logger "%s\n  URL: %s\n  Body: %S"
        err-msg full-url body)
      (signal 'error (list err-msg body plz-err))))))

(cl-defun org-canvas-api-request (method url &key params data timeout)
  "Perform an HTTP request to the Canvas API synchronously using `plz'.
METHOD is \\='GET, \\='POST, \\='PUT, or \\='DELETE.
URL is the full endpoint.
PARAMS is an alist of query parameters.
DATA is an alist or hash-table to be sent as JSON body (for POST/PUT).
TIMEOUT is the request timeout in seconds."
  (org-canvas--ensure-credentials)
  (let* (;; Build query string from params alist
         (query-string (if params
			   (concat "?" (url-build-query-string
					(cl-loop for (k . v) in params
						 collect (list (format "%s" k)
							       (format "%s" v)))))
			 ""))
	 (full-url (concat url query-string))
         ;; JSON-encode data if not already a string
	 (json-payload (when data
			 (if (stringp data) data (json-encode data))))
	 (headers `(("Authorization" . ,(concat "Bearer " org-canvas-api-token))
		    ("Content-Type" . "application/json")))
	 (actual-timeout (or timeout org-canvas-request-timeout))
	 ;; IMPORTANT: plz requires lowercase method symbols ('post not 'POST)
         ;; Our codebase uses uppercase by convention, so convert here
	 (plz-method (intern (downcase (symbol-name method)))))

    (elog-debug org-canvas--logger "[API] >>> REQUEST: %s %s" method full-url)
    (elog-debug org-canvas--logger "[API] Timeout: %ds | Headers: %S"
      actual-timeout (org-canvas--mask-token headers))
    (when params
      (elog-debug org-canvas--logger "[API] Params: %S" params))
    (when (and json-payload org-canvas-log-request-bodies)
      (elog-debug org-canvas--logger "[API] Body:\n%s"
	(org-canvas--pretty-json json-payload)))
    (elog-debug org-canvas--logger "[API] curl:\n%s"
      (org-canvas--build-curl-command method full-url
				      (when org-canvas-log-request-bodies json-payload)))

    (let ((retry-count 0)
          (done nil)
          result)
      (while (not done)
        (condition-case err
            (progn
              (setq result
                    (plz plz-method full-url
                      :headers headers
                      :body json-payload
                      :as #'json-read
                      :timeout actual-timeout))
              (elog-debug org-canvas--logger "[API] <<< RESPONSE: success")
              (when (and result org-canvas-log-request-bodies)
                (elog-debug org-canvas--logger "[API] Response Body:\n%s"
                  (org-canvas--pretty-json result)))
              (setq done t))
          (plz-error
           ;; Handler returns :retry for rate-limits (after sleeping),
           ;; or signals an error for terminal failures (never returns).
           (when (eq (org-canvas--api-handle-plz-error err full-url) :retry)
             (if (< retry-count org-canvas-rate-limit-retries)
                 (progn
                   (elog-debug org-canvas--logger
                     "[API] Retry %d/%d" (1+ retry-count) org-canvas-rate-limit-retries)
                   (setq retry-count (1+ retry-count)))
               ;; Retries exhausted
               (setq done t)
               (let* ((plz-err (cdr err))
                      (response (and (plz-error-p plz-err)
                                     (plz-error-response plz-err)))
                      (body (and response (plz-response-body response))))
                 (signal 'error (list (format "Rate limited after %d retries" retry-count)
                                      body plz-err))))))))
      result)))

(defun org-canvas-api-request-all-pages (method url &optional params)
  "Fetch all pages of results from a paginated Canvas API endpoint.
METHOD is the HTTP method (usually \\='GET).
URL is the full endpoint URL.
PARAMS is an alist of additional query parameters.
Automatically adds per_page=100 and loops until a page returns
fewer results than per_page.
Returns a flat list of all items across all pages."
  (let ((page 1)
        (per-page 100)
        (all-items nil)
        (done nil))
    (while (not done)
      (let* ((page-params (append (or params '())
                                  `(("per_page" . ,(number-to-string per-page))
                                    ("page" . ,(number-to-string page)))))
             (page-items (append (org-canvas-api-request method url :params page-params) nil)))
        (setq all-items (append all-items page-items))
        (if (< (length page-items) per-page)
            (setq done t)
          (setq page (1+ page)))))
    all-items))

;;;; 4. Org Interaction Layer

(defun org-canvas-org-get-property (pom property)
  "Get Org PROPERTY at POM (point or marker)."
  (org-entry-get pom property))

(defun org-canvas-org-get-boolean-property (pom property &optional default-true)
  "Get PROPERTY at POM as a boolean value.
If DEFAULT-TRUE is non-nil, returns t unless property is \"false\".
Otherwise, returns t only if property is \"true\"."
  (let ((value (org-entry-get pom property)))
    (when (and value (not (member (downcase value) '("true" "false"))))
      (when (boundp 'org-canvas--logger)
        (elog-warning org-canvas--logger
          "[Validate] Property %s has value '%s' — expected 'true' or 'false'. Using %s"
          property value (if default-true "true" "false")))
      (message "Warning: %s '%s' is not true/false, using %s"
        property value (if default-true "true" "false")))
    (if default-true
	(not (string-equal "false" value))
      (string-equal "true" value))))

(defun org-canvas-org-get-number-property (pom property &optional default)
  "Get PROPERTY at POM as a number.
Return DEFAULT (or 0) if property is nil or empty.
Warn if the value is non-numeric."
  (let ((val (org-entry-get pom property)))
    (if (and val (not (string-empty-p val)))
        (org-canvas--safe-string-to-number val property)
      (or default 0))))

(defun org-canvas--safe-string-to-number (val property)
  "Convert VAL to a number, warn if it is non-numeric.
PROPERTY is the property name, used in the warning message.
Returns the result of `string-to-number'."
  (when (and val
             (not (string-match-p "\\`-?[0-9]*\\.?[0-9]+\\'" val))
             (boundp 'org-canvas--logger))
    (elog-warning org-canvas--logger
      "[Parse] Property %s has non-numeric value \"%s\", treating as %s"
      property val (string-to-number val)))
  (string-to-number val))

(defun org-canvas--validate-property (value allowed property-name &optional default)
  "Validate VALUE is in ALLOWED list for PROPERTY-NAME.
Returns VALUE if valid, DEFAULT if nil, or DEFAULT with a warning if invalid."
  (cond
   ((null value) default)
   ((member value allowed) value)
   (t (when (boundp 'org-canvas--logger)
        (elog-warning org-canvas--logger
          "[Validate] %s: '%s' is not valid (expected: %s). Using '%s'"
          property-name value (string-join allowed ", ") (or default (car allowed))))
      (message "Warning: %s '%s' is not valid, using '%s'"
        property-name value (or default (car allowed)))
      (or default (car allowed)))))

(defun org-canvas-clear-sync-properties (pom)
  "Clear all sync-related properties from entry at POM."
  (dolist (prop org-canvas--sync-property-names)
    (org-entry-delete pom prop)))

(defun org-canvas-org-set-property (pom property value)
  "Set Org PROPERTY to VALUE at POM (point or marker).
Ensures the correct buffer is used if POM is a marker."
  (let ((buf (if (markerp pom) (marker-buffer pom) (current-buffer))))
    (with-current-buffer buf
      (save-excursion
	(goto-char pom)
	(org-entry-put (point) property value)))))

(defun org-canvas-org-save-sync-state (pom id &optional id-prop)
  "Standardize saving ID and LAST_SYNCED to the heading at POM.
ID-PROP defaults to `CANVAS_ID'."
  (let ((prop (or id-prop "CANVAS_ID"))
	(id-str (if (numberp id) (number-to-string id) id))
	(timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (org-canvas-org-set-property pom prop id-str)
    (org-canvas-org-set-property pom "LAST_SYNCED" timestamp)))

(defun org-canvas-org-parse-timestamp (ts-string)
  "Transform an Org timestamp TS-STRING into a Canvas ISO8601 string."
  (when ts-string
    (format-time-string "%Y-%m-%dT%H:%M:%SZ"
			(encode-time (org-parse-time-string ts-string))
			t)))

(defun org-canvas-current-iso8601-timestamp ()
  "Return the current time as a Canvas ISO8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun org-canvas--validate-date-ordering (data)
  "Warn if UNLOCK_AT, DUE_AT, LOCK_AT in DATA are in wrong order.
Also warns if any dates are in the past.
Compares ISO8601 strings lexicographically (works for UTC timestamps)."
  (let ((unlock (plist-get data :unlock_at))
        (due (plist-get data :due_at))
        (lock (plist-get data :lock_at))
        (title (or (plist-get data :title) "unknown")))
    (when (and unlock due (string> unlock due))
      (elog-warning org-canvas--logger
        "[Dates] '%s': UNLOCK_AT (%s) is after DUE_AT (%s)" title unlock due))
    (when (and due lock (string> due lock))
      (elog-warning org-canvas--logger
        "[Dates] '%s': DUE_AT (%s) is after LOCK_AT (%s)" title due lock))
    ;; Warn about past dates
    (let ((now (org-canvas-current-iso8601-timestamp)))
      (dolist (pair `((:due_at . "DUE_AT") (:lock_at . "LOCK_AT") (:unlock_at . "UNLOCK_AT")))
        (let ((date (plist-get data (car pair))))
          (when (and date (string< date now))
            (elog-warning org-canvas--logger
              "[Dates] '%s': %s (%s) is in the past" title (cdr pair) date)))))))

(defun org-canvas--for-each-entry (file query callback)
  "Call CALLBACK at each entry matching QUERY in FILE.
CALLBACK is called with point at each matching heading.
Returns a list of (success-count . fail-count)."
  (let ((targets nil)
	(success-count 0)
	(fail-count 0))
    (with-current-buffer (find-file-noselect file)
      (setq targets (org-map-entries (lambda () (point-marker)) query 'file)))
    (dolist (marker targets)
      (with-current-buffer (marker-buffer marker)
	(save-excursion
	  (goto-char (marker-position marker))
	  (condition-case err
	      (progn
		(funcall callback)
		(setq success-count (1+ success-count)))
	    (error
	     (setq fail-count (1+ fail-count))
	     (elog-error org-canvas--logger "[FAILED] At point %d: %s"
	       (marker-position marker) (error-message-string err)))))))
    (cons success-count fail-count)))

;;;; 4b. Shared Constants

(defconst org-canvas--sync-property-names
  '("CANVAS_ID" "CANVAS_URL" "LAST_SYNCED" "PAYLOAD_HASH")
  "Properties managed by the sync pipeline.")

(defconst org-canvas--bytes-per-mb 1048576.0
  "Number of bytes in one megabyte (for file size calculations).")

(defconst org-canvas--api-max-per-page '(("per_page" . "100"))
  "Default pagination params for Canvas API list requests.")

(defconst org-canvas--answer-weight-correct 100
  "Canvas answer weight for correct answers.")

(defconst org-canvas--answer-weight-incorrect 0
  "Canvas answer weight for incorrect answers.")

(defconst org-canvas--file-to-endpoint-map
  '(("pages.org" "pages" "CANVAS_URL")
    ("files.org" "files" "CANVAS_ID")
    ("assignments.org" "assignments" "CANVAS_ID")
    ("quizzes.org" "quizzes" "CANVAS_ID")
    ("discussions.org" "discussion_topics" "CANVAS_ID")
    ("announcements.org" "discussion_topics" "CANVAS_ID")
    ("modules.org" "modules" "CANVAS_ID"))
  "Map from org filename to (endpoint id-property) for Canvas URL resolution.")

(defconst org-canvas--orphan-feature-registry
  '((:name "Assignments"
     :endpoint "assignments"
     :file-var org-canvas-assignments-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field name
     :list-params nil
     :skip-fn nil)
    (:name "Pages"
     :endpoint "pages"
     :file-var org-canvas-pages-file
     :id-field url
     :id-property "CANVAS_URL"
     :title-field title
     :list-params nil
     :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
    (:name "Quizzes"
     :endpoint "quizzes"
     :file-var org-canvas-quizzes-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn nil)
    (:name "Discussions"
     :endpoint "discussion_topics"
     :file-var org-canvas-discussions-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t)))
    (:name "Announcements"
     :endpoint "discussion_topics"
     :file-var org-canvas-announcements-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params (("only_announcements" . "true"))
     :skip-fn nil)
    (:name "Files"
     :endpoint "files"
     :file-var org-canvas-files-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field display_name
     :list-params nil
     :skip-fn nil)
    (:name "Rubrics"
     :endpoint "rubrics"
     :file-var org-canvas-rubrics-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn nil)
    (:name "Assignment Groups"
     :endpoint "assignment_groups"
     :file-var org-canvas-assignment-groups-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field name
     :list-params nil
     :skip-fn nil)
    (:name "Outcomes"
     :endpoint "outcome_groups"
     :file-var org-canvas-outcomes-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn nil)
    (:name "Modules"
     :endpoint "modules"
     :file-var org-canvas-modules-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field name
     :list-params nil
     :skip-fn nil))
  "Registry of pushable features for orphan detection.
Each entry is a plist describing how to fetch remote items and match them
against local Org headings.  Sections and overrides are excluded
\(sections are pull-only, overrides are per-assignment\).")

(defun org-canvas--strip-statistics-cookie (title)
  "Remove Org statistics cookies like [1/3] or [33%] from TITLE.
Also strips text properties to prevent propertized strings from
leaking into API payloads and log output."
  (substring-no-properties
   (string-trim (replace-regexp-in-string "\\[\\([0-9]+/[0-9]+\\|[0-9]+%\\)\\]" "" title))))

;;;; 4c. Org Link Property Resolution

(defun org-canvas--resolve-link-property (link-string id-property source-file)
  "Resolve LINK-STRING to a property value by following the Org link.
ID-PROPERTY is the property to retrieve (e.g., \"CANVAS_ID\").
SOURCE-FILE is the file containing the link, used to resolve relative paths."
  (when (and link-string
             (or
              ;; Format with display text: [[file:path::*heading][display]]
              (string-match "\\[\\[file:\\(.+\\)::\\*\\(.+\\)\\]\\[" link-string)
              ;; Format without display text: [[file:path::*heading]]
              (string-match "\\[\\[file:\\([^]]+\\)::\\*\\([^]]+\\)\\]\\]" link-string)))
    (let* ((file (match-string 1 link-string))
           (heading (match-string 2 link-string))
           (abs-file (expand-file-name file (file-name-directory source-file)))
           ;; Unescape \[ and \] in heading text (Org escapes brackets in links)
           (clean-heading (replace-regexp-in-string
                           "\\\\[][]"
                           (lambda (m) (substring m 1))
                           heading)))
      (cond
       ((not (file-exists-p abs-file))
        (elog-warning org-canvas--logger
          "[Links] File not found: %s (from link %s)" abs-file link-string)
        nil)
       (t
        (let ((heading-point (org-canvas--find-heading-in-file abs-file clean-heading)))
          (if (not heading-point)
              (progn
                (elog-warning org-canvas--logger
                  "[Links] Heading '%s' not found in %s" clean-heading abs-file)
                nil)
            (with-current-buffer (find-file-noselect abs-file)
              (let ((value (org-entry-get heading-point id-property)))
                (unless value
                  (elog-warning org-canvas--logger
                    "[Links] Property %s not set on '%s' in %s"
                    id-property clean-heading abs-file))
                value)))))))))

;;;; 4d. Cross-file Link Resolution for HTML Export

(defun org-canvas--unescape-org-brackets (s)
  "Unescape \\=\\[ and \\=\\] in S to [ and ]."
  (replace-regexp-in-string
   "\\\\\\]" "]"
   (replace-regexp-in-string "\\\\\\[" "[" s)))

(defun org-canvas--find-heading-in-file (abs-file heading)
  "Find HEADING in ABS-FILE, return point or nil.
Tries exact match first, then display-name fallback for link headings.
HEADING should already be unescaped (no \\=\\[ or \\=\\] escapes)."
  (when (file-exists-p abs-file)
    (with-current-buffer (find-file-noselect abs-file)
      (save-excursion
        (goto-char (point-min))
        (or
         ;; Exact heading match (tolerates extra whitespace after stars)
         (re-search-forward
          (format "^\\*+ +%s[ \t]*$" (regexp-quote heading)) nil t)
         ;; Display name match (for link headings like [[file:...][name]])
         (when (string-match "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" heading)
           (let ((display-name (match-string 1 heading)))
             (goto-char (point-min))
             (re-search-forward
              (format "^\\*+ +.*\\[%s\\]" (regexp-quote display-name))
              nil t))))))))

(defun org-canvas--resolve-to-canvas-url (file heading source-dir)
  "Resolve a cross-file link to a Canvas URL.
FILE is the relative path to a .org file.
HEADING is the heading search text (may contain escaped brackets).
SOURCE-DIR is the directory of the source file containing the link."
  (let* ((abs-file (expand-file-name file source-dir))
         (basename (file-name-nondirectory file))
         (url-info (assoc basename org-canvas--file-to-endpoint-map))
         (url-info (when url-info (cdr url-info))))
    (when (and url-info (file-exists-p abs-file))
      (let ((endpoint (car url-info))
            (id-prop (cadr url-info))
            (clean-heading (org-canvas--unescape-org-brackets heading)))
        (let ((heading-point (org-canvas--find-heading-in-file abs-file clean-heading)))
          (when heading-point
            (with-current-buffer (find-file-noselect abs-file)
              (let ((id (org-entry-get heading-point id-prop)))
                (when id
                  (format "%s/courses/%s/%s/%s"
                          org-canvas-base-url org-canvas-course-id
                          endpoint id))))))))))

(defun org-canvas--resolve-body-links (source-dir)
  "Resolve cross-file Org links in current buffer to Canvas URLs.
Replaces [[file:*.org::*HEADING][DISPLAY]] links with Canvas URL links.
Unresolvable links are replaced with their display text.
SOURCE-DIR is the directory of the source .org file."
  (goto-char (point-min))
  (while (re-search-forward
          "\\[\\[file:\\([^]:]+\\.org\\)::\\*" nil t)
    (let ((link-start (match-beginning 0))
          (file (match-string 1))
          (heading-start (point)))
      ;; Find ][ separator (escaped brackets use \]\[ which won't match)
      (when (search-forward "][" nil t)
        (let* ((heading (buffer-substring-no-properties
                         heading-start (- (point) 2)))
               (display-start (point)))
          ;; Find ]] end
          (when (search-forward "]]" nil t)
            (let* ((link-end (point))
                   (display (buffer-substring-no-properties
                             display-start (- link-end 2)))
                   (canvas-url (org-canvas--resolve-to-canvas-url
                                file heading source-dir)))
              ;; Replace the link
              (delete-region link-start link-end)
              (goto-char link-start)
              (if canvas-url
                  (insert (format "[[%s][%s]]" canvas-url display))
                (elog-warning org-canvas--logger
                  "[Links] Unresolved: [[file:%s::*%s][%s]] → plain text"
                  file heading display)
                (insert display)))))))))

(defvar org-export-with-broken-links)
(defvar org-export-with-sub-superscripts)
(defvar org-export-use-babel)

(defun org-canvas--export-subtree-to-html ()
  "Export current Org subtree body to HTML string.
Tolerates broken cross-file links, disables subscript markup, and
suppresses Babel evaluation to avoid requiring live kernels."
  (let ((org-export-with-broken-links 'mark)
        (org-export-with-sub-superscripts nil)
        (org-export-use-babel nil))
    (org-export-as 'html t nil t nil)))

(defun org-canvas--export-subtree-body-to-html ()
  "Export current Org subtree to HTML, resolving cross-file links.
Returns the HTML string.  Cross-file links [[file:*.org::*...][...]]
are resolved to Canvas URLs when the target has a CANVAS_ID."
  (save-excursion
    (org-back-to-heading t)
    (let* ((beg (point))
           (end (save-excursion (org-end-of-subtree t) (point)))
           (content (buffer-substring beg end))
           (source-dir (file-name-directory
                        (or (buffer-file-name) default-directory))))
      (with-temp-buffer
        (let ((default-directory source-dir))
          (insert content)
          (org-mode)
          ;; Strip override tables (#+NAME: overrides + following table rows)
          (goto-char (point-min))
          (while (re-search-forward "^#\\+NAME: overrides\n" nil t)
            (let ((start (match-beginning 0)))
              (while (looking-at "^|")
                (forward-line 1))
              (delete-region start (point))))
          ;; Resolve cross-file links to Canvas URLs
          (org-canvas--resolve-body-links source-dir)
          ;; Export the subtree to HTML (body only)
          (goto-char (point-min))
          (let ((org-export-with-broken-links 'mark)
                (org-export-with-sub-superscripts nil)
                (org-export-use-babel nil))
            (org-export-as 'html t nil t nil)))))))

;;;; 4e. Pull Helpers (Canvas → Org)

(defun org-canvas--html-to-org (html)
  "Convert HTML string to Org format using pandoc.
Returns the Org-mode text, or the raw HTML prefixed with a warning
if pandoc is not available."
  (if (not (executable-find "pandoc"))
      (concat "# WARNING: pandoc not found, raw HTML below\n" html)
    (with-temp-buffer
      (insert html)
      (let ((exit-code (call-process-region
                        (point-min) (point-max) "pandoc"
                        t t nil
                        "-f" "html" "-t" "org" "--wrap=none")))
        (if (= exit-code 0)
            (string-trim (buffer-string))
          (concat "# WARNING: pandoc conversion failed\n" html))))))

(defun org-canvas--pull-upsert-heading (file canvas-id &optional title id-property)
  "Find or create a heading in FILE matched by CANVAS-ID.
If a level-1 heading with ID-PROPERTY (default \"CANVAS_ID\") equal to
CANVAS-ID exists, return its position.  Otherwise create a new heading
at the end of the buffer with TITLE and return its position.
Returns a point in the buffer visiting FILE."
  (let ((id-prop (or id-property "CANVAS_ID"))
        (buf (find-file-noselect (expand-file-name file))))
    (with-current-buffer buf
      (save-excursion
        ;; Search for existing heading by CANVAS_ID
        (goto-char (point-min))
        (let ((found nil))
          (org-map-entries
           (lambda ()
             (when (equal (org-entry-get (point) id-prop)
                          (format "%s" canvas-id))
               (setq found (point))))
           "LEVEL=1" 'file)
          (if found
              found
            ;; Create new heading at end
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "* %s\n" (or title "Untitled")))
            (org-back-to-heading t)
            (point)))))))

(defun org-canvas--iso8601-to-org-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org active timestamp.
Returns a string like \"<2026-01-15 Thu 10:00>\" or nil."
  (when (and iso8601 (stringp iso8601) (not (equal iso8601 ""))
             (not (eq iso8601 :null)))
    (let ((time (date-to-time iso8601)))
      (format-time-string "<%Y-%m-%d %a %H:%M>" time t))))

(defun org-canvas--iso8601-to-org-inactive-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org inactive timestamp.
Returns a string like \"[2026-01-15 Thu 10:00]\" or nil."
  (when (and iso8601 (stringp iso8601) (not (equal iso8601 ""))
             (not (eq iso8601 :null)))
    (let ((time (date-to-time iso8601)))
      (format-time-string "[%Y-%m-%d %a %H:%M]" time t))))

;;;; 5. Diagnostics

(defun org-canvas-get-course-name ()
  "Fetch the name of the configured course from Canvas to verify access.
Returns the course name as a string.  Signals an error if the request fails."
  (let* ((endpoint (org-canvas-api-course-endpoint ""))
	 (response (org-canvas-api-request 'GET endpoint)))
    (alist-get 'name response)))

(defun org-canvas-test-connection ()
  "Interactive command to test the Canvas API connection."
  (interactive)
  (org-canvas-clear-log)
  (elog-info org-canvas--logger "Testing connection to %s (Course ID: %s)..."
    org-canvas-base-url org-canvas-course-id)

  (condition-case err
      (let ((name (org-canvas-get-course-name)))
	(elog-info org-canvas--logger "Success! Connected to course: %s" name)
	(message "Success! Connected to course: %s" name))
    (error
     (elog-error org-canvas--logger "Connection Failed: %s" err)
     (message "Connection Failed. Check *canvas-log* for details."))))

(defun org-canvas--preflight-check ()
  "Validate credentials and connection before syncing.
Signals error with actionable message on failure."
  (org-canvas--ensure-credentials)
  (condition-case err
      (let ((course (org-canvas-api-request 'GET
                      (org-canvas-api-course-endpoint ""))))
        (elog-info org-canvas--logger "[Preflight] Connected to: %s"
          (alist-get 'name course)))
    (error
     (error "Connection failed: %s\nCheck your API token, course ID, and network connection"
            (error-message-string err)))))

(defvar org-canvas--sync-in-progress nil
  "Non-nil when a sync is running.  Prevents concurrent syncs.")

(defvar org-canvas--dry-run nil
  "When non-nil, sync shows what would happen without making API calls.")

;;;; 6. Sync Pipeline Infrastructure
;;
;; Runtime helpers called by the generated sync functions.
;; Extracted from `org-canvas-define-sync' for readability.

(defun org-canvas--sync-validate-file (feature-upper sync-file)
  "Validate SYNC-FILE exists and prompt to save unsaved buffers.
FEATURE-UPPER is the uppercased feature name for log messages.
Opens the log buffer and logs a sync header."
  ;; When org-canvas--inhibit-log-clear is non-nil, we are inside
  ;; org-canvas-sync which already checked this guard at entry.
  (when (and org-canvas--sync-in-progress
             (not org-canvas--inhibit-log-clear))
    (user-error "A sync is already in progress.  Please wait for it to finish"))
  (unless (and sync-file (file-exists-p sync-file))
    (error "%s file not found: %s" feature-upper sync-file))
  (let ((buf (find-file-noselect sync-file)))
    (when (buffer-modified-p buf)
      (if (y-or-n-p (format "%s has unsaved changes.  Save before syncing? "
                            (file-name-nondirectory sync-file)))
          (with-current-buffer buf (save-buffer))
        (user-error "Aborted: unsaved changes in %s" sync-file))))
  (display-buffer (get-buffer-create "*canvas-log*"))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> STARTING %s SYNC" feature-upper)
  (elog-info org-canvas--logger "File: %s" sync-file)
  (elog-info org-canvas--logger "Course: %s | URL: %s"
    org-canvas-course-id org-canvas-base-url)
  (elog-info org-canvas--logger "========================================"))

(defun org-canvas--sync-collect-entries (sync-file query feature-name)
  "Collect entry markers and existing CANVAS_IDs from SYNC-FILE.
QUERY is the `org-map-entries' match string.
FEATURE-NAME is used for log messages.
Returns a plist (:targets MARKERS :all-ids-before IDS)."
  (let (targets all-ids-before)
    (with-current-buffer (find-file-noselect sync-file)
      (setq targets (org-map-entries (lambda () (point-marker)) query 'file))
      (setq all-ids-before
            (org-map-entries
             (lambda () (org-entry-get (point) "CANVAS_ID"))
             "CANVAS_ID={.}" 'file)))
    (elog-info org-canvas--logger "Found %d %s to sync"
      (length targets) feature-name)
    ;; Warn about duplicate CANVAS_IDs
    (let ((id-counts (make-hash-table :test 'equal)))
      (dolist (id all-ids-before)
        (puthash id (1+ (gethash id id-counts 0)) id-counts))
      (maphash (lambda (id count)
                 (when (> count 1)
                   (elog-warning org-canvas--logger
                     "[Duplicate] CANVAS_ID %s appears %d times in %s"
                     id count sync-file)))
               id-counts))
    (list :targets targets :all-ids-before all-ids-before)))

(defun org-canvas--sync-process-entry (marker ctx)
  "Process one entry through the 4-stage pipeline.
MARKER is the position of the entry.
CTX is a plist with keys:
  :parse-fn, :build-fn, :push-fn, :finalize-fn - pipeline stage functions
  :feature-name, :feature-upper - for log messages
  :total-count - total entries being processed
  :counters - plist (:success N :skip N :fail N) mutated in place
  :synced-ids - list (mutated via push) of processed CANVAS_IDs
  :title-key - plist key for display name (default :title)"
  (let ((parse-fn (plist-get ctx :parse-fn))
        (build-fn (plist-get ctx :build-fn))
        (push-fn (plist-get ctx :push-fn))
        (finalize-fn (plist-get ctx :finalize-fn))
        (feature-name (plist-get ctx :feature-name))
        (feature-upper (plist-get ctx :feature-upper))
        (total-count (plist-get ctx :total-count))
        (counters (plist-get ctx :counters))
        (synced-ids (plist-get ctx :synced-ids))
        (title-key (or (plist-get ctx :title-key) :title)))
    (elog-info org-canvas--logger "----------------------------------------")
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (condition-case err
            (let* ((data (funcall parse-fn))
                   (payload (funcall build-fn data))
                   (payload-hash (md5 (json-encode payload)))
                   (stored-hash (org-entry-get (point) "PAYLOAD_HASH"))
                   (canvas-id (or (plist-get data :canvas-id)
                                  (plist-get data :canvas-url)))
                   (title (plist-get data title-key))
                   (cap-feature (capitalize feature-name))
                   (progress (+ (plist-get counters :success)
                                (plist-get counters :skip)
                                (plist-get counters :fail)
                                1)))
              ;; Track synced IDs for orphan detection
              (when canvas-id
                (push canvas-id (car synced-ids)))
              ;; Skip if unchanged and already synced
              (cond
               ((and stored-hash (string= payload-hash stored-hash) canvas-id)
                (plist-put counters :skip (1+ (plist-get counters :skip)))
                (elog-info org-canvas--logger "[Skip] '%s' unchanged" title)
                (message "%s [%d/%d] Skipping '%s' (unchanged)"
                  cap-feature progress total-count title))
               (org-canvas--dry-run
                (elog-info org-canvas--logger "[DRY-RUN] Would %s '%s'"
                  (if canvas-id "UPDATE" "CREATE") title)
                (message "%s [DRY-RUN] Would %s '%s'"
                  cap-feature (if canvas-id "update" "create") title)
                (plist-put counters :success (1+ (plist-get counters :success))))
               (t
                (let ((response (funcall push-fn data payload)))
                  (if (eq response 'conflict)
                      ;; Conflict detected — skip finalize
                      (progn
                        (plist-put counters :conflict
                                   (1+ (plist-get counters :conflict)))
                        (message "%s [%d/%d] CONFLICT: '%s' (remote modified)"
                          cap-feature progress total-count title))
                    (funcall finalize-fn data response)
                    (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
                    (save-buffer)
                    (plist-put counters :success (1+ (plist-get counters :success)))
                    (message "%s [%d/%d] Synced '%s'"
                      cap-feature progress total-count title)
                    ;; Track newly created IDs
                    (unless canvas-id
                      (let ((new-id (or (org-entry-get (point) "CANVAS_ID")
                                        (org-entry-get (point) "CANVAS_URL"))))
                        (when new-id
                          (push new-id (car synced-ids))))))))))
          (error
           (plist-put counters :fail (1+ (plist-get counters :fail)))
           (elog-error org-canvas--logger "[FAILED] %s at point %d: %s"
             feature-upper (marker-position marker) (error-message-string err))
           (message "%s [%d/%d] FAILED: %s"
             (capitalize feature-name)
             (+ (plist-get counters :success) (plist-get counters :skip)
                (plist-get counters :fail))
             total-count (error-message-string err))))))))

(defun org-canvas--sync-warn-orphans (all-ids-before synced-ids feature-name)
  "Warn about CANVAS_IDs in ALL-IDS-BEFORE not present in SYNCED-IDS.
FEATURE-NAME is used in the log message."
  (dolist (old-id all-ids-before)
    (unless (member old-id synced-ids)
      (elog-warning org-canvas--logger
        "[Orphan] CANVAS_ID %s in file was not synced — may be orphaned on Canvas.\n  To clean up: delete the heading's CANVAS_ID property, or use M-x org-canvas-delete-%s-at-point"
        old-id feature-name))))

(defun org-canvas--sync-log-summary (feature-upper feature-name sync-file
                                     success-count skip-count fail-count
                                     &optional conflict-count)
  "Save SYNC-FILE and log completion summary.
FEATURE-UPPER and FEATURE-NAME are for log messages.
CONFLICT-COUNT is the number of items skipped due to remote conflicts."
  (let ((conflict-count (or conflict-count 0)))
    (with-current-buffer (find-file-noselect sync-file)
      (save-buffer)
      (elog-info org-canvas--logger "Saved %s" sync-file))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> %s SYNC COMPLETE" feature-upper)
    (if (> conflict-count 0)
        (elog-info org-canvas--logger
          "Success: %d | Skipped: %d | Failed: %d | Conflicts: %d"
          success-count skip-count fail-count conflict-count)
      (elog-info org-canvas--logger "Success: %d | Skipped: %d | Failed: %d"
        success-count skip-count fail-count))
    (elog-info org-canvas--logger "========================================")
    (if (> conflict-count 0)
        (message "%s sync: %d success, %d skipped, %d failed, %d conflicts."
                 (capitalize feature-name) success-count skip-count
                 fail-count conflict-count)
      (message "%s sync: %d success, %d skipped, %d failed."
               (capitalize feature-name) success-count skip-count fail-count))))

(defmacro org-canvas-define-sync (feature &rest args)
  "Define a sync function for FEATURE using the 4-stage pipeline pattern.

FEATURE is a symbol like \\='pages or \\='announcements.

ARGS is a plist with the following keys:
  :file - Expression that evaluates to the org file path (required)
  :query - Org match query for entries (default: \"LEVEL=1\")
  :parse - Function to parse entry at point (required)
  :build - Function to build payload from parsed data (required)
  :push - Function to push payload to API (required)
  :finalize - Function to finalize after API response (required)
  :title-key - Plist key for display name in logs (default: :title)

This macro generates an interactive function `org-canvas-sync-FEATURE'
that:
  1. Clears the log and displays the log buffer
  2. Logs sync header with feature name, file, and course info
  3. Collects all matching org entries as markers
  4. Iterates over each entry, calling parse -> build -> push -> finalize
  5. Handles errors gracefully, continuing with other entries
  6. Saves the org file after all modifications
  7. Logs sync footer with success/fail counts

Example usage:
  (org-canvas-define-sync announcements
    :file org-canvas-announcements-file
    :parse #\\='org-canvas--announcement-parse-entry
    :build #\\='org-canvas--announcement-build-payload
    :push #\\='org-canvas--announcement-push-to-api
    :finalize #\\='org-canvas--announcement-finalize)"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (feature-upper (upcase feature-name))
         (sync-fn-name (intern (format "org-canvas-sync-%s" feature-name)))
         (file-expr (plist-get args :file))
         (query (or (plist-get args :query) "LEVEL=1"))
         (parse-fn (plist-get args :parse))
         (build-fn (plist-get args :build))
         (push-fn (plist-get args :push))
         (finalize-fn (plist-get args :finalize))
         (title-key (plist-get args :title-key)))
    ;; Validate required args
    (unless file-expr (error "org-canvas-define-sync: :file is required"))
    (unless parse-fn (error "org-canvas-define-sync: :parse is required"))
    (unless build-fn (error "org-canvas-define-sync: :build is required"))
    (unless push-fn (error "org-canvas-define-sync: :push is required"))
    (unless finalize-fn (error "org-canvas-define-sync: :finalize is required"))
    `(progn
       ;;;###autoload
       (defun ,sync-fn-name ()
         ,(format "Synchronize %s to Canvas using the 4-stage pipeline." feature-name)
         (interactive)
         (org-canvas-clear-log)
         (let ((sync-file (expand-file-name ,file-expr)))
           (org-canvas--sync-validate-file ,feature-upper sync-file)
           (let* ((entries (org-canvas--sync-collect-entries
                            sync-file ,query ,feature-name))
                  (targets (plist-get entries :targets))
                  (all-ids-before (plist-get entries :all-ids-before))
                  (counters (list :success 0 :skip 0 :fail 0))
                  (synced-ids (list nil))
                  (ctx (list :parse-fn ,parse-fn
                             :build-fn ,build-fn
                             :push-fn ,push-fn
                             :finalize-fn ,finalize-fn
                             :feature-name ,feature-name
                             :feature-upper ,feature-upper
                             :total-count (length targets)
                             :counters counters
                             :synced-ids synced-ids
                             :title-key ,title-key)))
             ;; Process each entry through the pipeline
             (dolist (marker targets)
               (org-canvas--sync-process-entry marker ctx))
             (org-canvas--sync-warn-orphans all-ids-before (car synced-ids)
                                            ,feature-name)
             (org-canvas--sync-log-summary
              ,feature-upper ,feature-name sync-file
              (plist-get counters :success)
              (plist-get counters :skip)
              (plist-get counters :fail))))))))

;;;; 6b. Conflict Detection
;;
;; Before overwriting a Canvas item (PUT), check if someone edited it
;; remotely.  Compare the item's `updated_at' from a fresh GET with the
;; local `LAST_SYNCED' timestamp.  If Canvas is newer, warn and skip.

(defun org-canvas--parse-iso8601-time (iso8601)
  "Parse ISO8601 timestamp string to an Emacs time value.
Returns nil if ISO8601 is nil or :null."
  (when (and iso8601 (not (eq iso8601 :null)) (stringp iso8601))
    (date-to-time iso8601)))

(defun org-canvas--parse-last-synced (pom)
  "Parse the LAST_SYNCED Org timestamp at POM to an Emacs time value.
Returns nil if no LAST_SYNCED property exists."
  (let ((ts (org-entry-get pom "LAST_SYNCED")))
    (when ts
      (encode-time (org-parse-time-string ts)))))

(cl-defun org-canvas--conflict-check (endpoint id pom)
  "Check if the remote item at ENDPOINT/ID was modified after LAST_SYNCED at POM.
Returns \\='conflict if the remote item is newer, nil otherwise.
Returns nil on GET failure (allows push to proceed) or when
no LAST_SYNCED exists (legacy item, first sync)."
  (let ((local-time (org-canvas--parse-last-synced pom)))
    (unless local-time
      (cl-return-from org-canvas--conflict-check nil))
    (condition-case _err
        (let* ((full-url (org-canvas-api-course-endpoint
                          (format "%s/%%s" endpoint) id))
               (response (org-canvas-api-request 'GET full-url))
               (updated-at (alist-get 'updated_at response))
               (remote-time (org-canvas--parse-iso8601-time updated-at)))
          (if (and remote-time (time-less-p local-time remote-time))
              (progn
                (elog-warning org-canvas--logger
                  "[Conflict] Remote item updated at %s, local LAST_SYNCED is %s"
                  updated-at (org-entry-get pom "LAST_SYNCED"))
                'conflict)
            nil))
      (error nil))))

;;;; 7. Push-to-API Infrastructure
;;
;; These helpers standardize the "Execute" stage of the pipeline.
;; They handle common error recovery scenarios:
;;
;;   - Timeout: The request timed out but may have succeeded on the server.
;;     Uses find-fn to search for the item by title.
;;
;;   - 404 on PUT: The CANVAS_ID in our Org file is stale (item was deleted
;;     from Canvas). Automatically retries as POST to create a new item.
;;
;; Use `org-canvas--push-to-api' for most feature modules.
;; Use `org-canvas--search-item' when you need custom search logic.

(defun org-canvas--timeout-error-p (err)
  "Return non-nil if ERR represents a timeout.
ERR is a `condition-case' error value.  Check both the message and
error-thrown fields since different callers place the timeout
indicator in different positions."
  (or (let ((error-thrown (caddr err)))
        (and error-thrown (string-match-p "Timeout" (format "%s" error-thrown))))
      (let ((err-msg (cadr err)))
        (and err-msg (string-match-p "Timeout" (format "%s" err-msg))))))

(defun org-canvas--404-on-put-p (err method)
  "Return non-nil if ERR is a 404 and METHOD is PUT.
ERR is a `condition-case' error value."
  (and (eq method 'PUT)
       (string-match-p "404" (format "%s" (cadr err)))))

(cl-defun org-canvas--search-item (endpoint title &key params match-field)
  "Search for an item on Canvas by title.

ENDPOINT is the API endpoint suffix (e.g., \"pages\" or \"assignments\").
TITLE is the value to search for.

Keyword arguments:
  PARAMS - Additional params for GET request (default adds search_term).
  MATCH-FIELD - Alist key to match against TITLE (default: \\='title).

Return the matching item alist, or nil if not found."
  (let ((match-field (or match-field 'title)))
    (elog-info org-canvas--logger "[Search] Looking for item with %s='%s'..." match-field title)
    (condition-case err
        (let* ((full-endpoint (org-canvas-api-course-endpoint endpoint))
               (search-params (append (or params `(("search_term" . ,title))) nil))
               (results (append (org-canvas-api-request 'GET full-endpoint :params search-params) nil))
               (count (length results)))
          (elog-debug org-canvas--logger "[Search] Found %d results" count)
          (let ((found (cl-loop for item in results
                                when (string-equal (alist-get match-field item) title)
                                return item)))
            (if found
                (elog-info org-canvas--logger "[Search] Found exact match: ID=%s"
                  (or (alist-get 'id found) (alist-get 'url found)))
              (elog-debug org-canvas--logger "[Search] No exact match found"))
            found))
      (error
       (elog-warning org-canvas--logger "[Search] Failed: %s" (cadr err))
       nil))))

(defun org-canvas--handle-timeout-recovery (find-fn title err)
  "Search for item by TITLE after a timeout using FIND-FN.
Re-signal ERR if item not found."
  (elog-warning org-canvas--logger "[Timeout] Checking if item was created...")
  (let ((found (funcall find-fn title)))
    (if found
        (progn
          (elog-info org-canvas--logger "[Recovery] Found item after timeout!")
          found)
      (elog-error org-canvas--logger "[Recovery] Item not found after timeout")
      (signal (car err) (cdr err)))))

(defun org-canvas--handle-404-retry (endpoint payload find-fn title _err)
  "Retry as POST after 404 on PUT.
ENDPOINT is the base endpoint, PAYLOAD the data to send.
FIND-FN and TITLE are used for timeout recovery on the retry.
ERR is the original error for re-signaling."
  (elog-warning org-canvas--logger "[Recovery] Item not found (404). Retrying as POST...")
  (let ((new-endpoint (org-canvas-api-course-endpoint endpoint)))
    (condition-case post-err
        (let ((response (org-canvas-api-request 'POST new-endpoint :data payload)))
          (elog-info org-canvas--logger "[Recovery] POST successful")
          response)
      (error
       (if (and find-fn (caddr post-err)
                (string-match "Timeout" (format "%s" (caddr post-err))))
           (org-canvas--handle-timeout-recovery find-fn title post-err)
         (signal (car post-err) (cdr post-err)))))))

(cl-defun org-canvas--push-to-api (data payload
					&key
					endpoint
					id-key
					title-key
					find-fn)
  "Generic push-to-API with 404 retry and optional timeout recovery.

DATA is the parsed entry plist (must contain :canvas-id or :canvas-url).
PAYLOAD is the API payload to send.

Keyword arguments:
  ENDPOINT - API endpoint suffix (e.g., \"assignments\" or \"pages\").
  ID-KEY - Key in DATA for Canvas ID (default: :canvas-id).
  TITLE-KEY - Key in DATA for title (default: :title).
  FIND-FN - Optional function (TITLE) to search for item after timeout.

Handle:
  - POST for new items (no ID), PUT for existing items
  - 404 on PUT: retries as POST (stale ID recovery)
  - Timeout: calls FIND-FN to check if item was created

Returns the API response alist."
  (let* ((id-key (or id-key :canvas-id))
         (title-key (or title-key :title))
         (id (plist-get data id-key))
         (title (plist-get data title-key))
         (method (if id 'PUT 'POST))
         (full-endpoint (if id
                            (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) id)
                          (org-canvas-api-course-endpoint endpoint))))

    ;; Dry-run: skip API call and return a mock response
    (when org-canvas--dry-run
      (elog-info org-canvas--logger "[DRY-RUN] Would %s '%s' to %s" method title full-endpoint)
      (cl-return-from org-canvas--push-to-api '((id . "dry-run"))))

    ;; Conflict detection: for PUT only, check if remote was modified
    (when (and org-canvas-detect-conflicts
               (eq method 'PUT)
               (plist-get data :pom))
      (when (eq 'conflict
                (org-canvas--conflict-check endpoint id (plist-get data :pom)))
        (elog-warning org-canvas--logger
          "[Conflict] Skipping '%s' — remote item was modified since last sync" title)
        (cl-return-from org-canvas--push-to-api 'conflict)))

    (elog-info org-canvas--logger "[Execute] %s '%s' to %s" method title full-endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request method full-endpoint :data payload)))
          (elog-info org-canvas--logger "[Execute] %s successful for '%s'" method title)
          response)
      (error
       (elog-error org-canvas--logger "[Execute] Failed: %s" (cadr err))

       (cond
        ;; CASE 1: Timeout -> Check if item exists via find-fn
        ((and find-fn (org-canvas--timeout-error-p err))
         (org-canvas--handle-timeout-recovery find-fn title err))

        ;; CASE 2: 404 on PUT -> Retry as POST (stale ID)
        ((org-canvas--404-on-put-p err method)
         (org-canvas--handle-404-retry endpoint payload find-fn title err))

        ;; Default: Re-throw
        (t (signal (car err) (cdr err))))))))

(cl-defun org-canvas--finalize-item (data response
					  &key
					  id-field
					  id-property
					  title-key
					  post-fn)
  "Finalize sync by saving Canvas ID and LAST_SYNCED.

DATA is the parsed entry plist (must contain :pom).
RESPONSE is the API response alist.

Keyword arguments:
  ID-FIELD - Alist key for ID in response (default: \\='id).
  ID-PROPERTY - Org property name to save (default: \"CANVAS_ID\").
  TITLE-KEY - Key in DATA for title (default: :title).
  POST-FN - Optional function (DATA RESPONSE) for additional finalization.

Save the Canvas ID and LAST_SYNCED timestamp to the Org entry."
  (let* ((id-field (or id-field 'id))
         (id-property (or id-property "CANVAS_ID"))
         (title-key (or title-key :title))
         (id (alist-get id-field response))
         (pom (plist-get data :pom))
         (title (plist-get data title-key)))

    (unless pom
      (error "Finalize-item: missing :pom in data for '%s'" title))

    (elog-debug org-canvas--logger "[Finalize] Processing response for '%s'" title)

    (if id
        (progn
          (elog-info org-canvas--logger "[Finalize] Saving %s=%s for '%s'" id-property id title)
          (org-canvas-org-save-sync-state pom id id-property)
          ;; Save CANVAS_UPDATED_AT for conflict detection
          (let ((updated-at (alist-get 'updated_at response)))
            (when updated-at
              (org-canvas-org-set-property pom "CANVAS_UPDATED_AT"
                                           (format "%s" updated-at))))
          (when post-fn
            (funcall post-fn data response))
          (elog-info org-canvas--logger "[Finalize] Complete for '%s'" title))
      (elog-error org-canvas--logger "[Finalize] No ID in response for '%s'!" title)
      (error "No %s in API response for '%s'" id-field title))))

;;;; 8. Delete Infrastructure
;;
;; Two types of delete operations:
;;
;;   - delete-all: Fetches all items from Canvas and deletes them,
;;     then cleans up local CANVAS_ID properties from the Org file.
;;
;;   - delete-at-point: Deletes the single item at the current cursor
;;     position (requires confirmation).
;;
;; Use the macros `org-canvas-define-delete-all' and
;; `org-canvas-define-delete-at-point' to generate these functions.

(defun org-canvas--delete-items-queued (items endpoint-fn id-field title-field &optional skip-fn)
  "Delete ITEMS from Canvas using concurrent requests via `plz-queue'.
ENDPOINT-FN is a function taking an item ID and returning the DELETE URL.
ID-FIELD and TITLE-FIELD are alist keys for extracting ID/title from each item.
SKIP-FN, if non-nil, is called with each item; non-nil return skips that item.
Returns (DELETED-COUNT . DELETED-IDS)."
  (let* ((to-delete (if skip-fn (cl-remove-if skip-fn items) items))
         (skipped (- (length items) (length to-delete))))
    ;; Log skipped items
    (when (> skipped 0)
      (dolist (item items)
        (when (and skip-fn (funcall skip-fn item))
          (elog-info org-canvas--logger "Skipping: '%s'"
            (alist-get title-field item)))))
    ;; Short-circuit if nothing to delete
    (if (null to-delete)
        (cons 0 nil)
      (let* ((deleted-count 0)
             (deleted-ids nil)
             (done nil)
             (headers `(("Authorization" . ,(concat "Bearer " org-canvas-api-token))
                        ("Content-Type" . "application/json")))
             (queue (make-plz-queue
                     :limit org-canvas-delete-concurrency
                     :finally (lambda () (setq done t)))))
        (dolist (item to-delete)
          (let ((item-id (alist-get id-field item))
                (item-title (alist-get title-field item)))
            (elog-info org-canvas--logger "Deleting: '%s' (ID: %s)" item-title item-id)
            (plz-queue queue 'delete (funcall endpoint-fn item-id)
              :headers headers
              :then (lambda (_response)
                      (push (if (numberp item-id) (number-to-string item-id) item-id)
                            deleted-ids)
                      (setq deleted-count (1+ deleted-count))
                      (elog-info org-canvas--logger "  -> Deleted '%s' successfully" item-title))
              :else (lambda (err)
                      (elog-error org-canvas--logger "  -> Delete failed for '%s': %s"
                        item-title err)))))
        (plz-run queue)
        (while (not done)
          (accept-process-output nil 0.1))
        (cons deleted-count deleted-ids)))))

(cl-defun org-canvas--delete-all-items (feature-name
                                        &key
                                        endpoint
                                        file
                                        id-field
                                        title-field
                                        id-property
                                        list-params
                                        skip-fn)
  "Generic implementation for deleting all items of a feature type.

FEATURE-NAME is a string like \"announcements\" or \"pages\".

Keyword arguments:
  ENDPOINT - API endpoint suffix (e.g., \"assignments\").
  FILE - Path to the org file for cleaning local properties.
  ID-FIELD - Alist key for item ID in API response (default: \\='id).
  TITLE-FIELD - Alist key for item title in API response (default: \\='title).
  ID-PROPERTY - Org property name for ID (default: \"CANVAS_ID\").
  LIST-PARAMS - Extra params for GET request.
  SKIP-FN - Optional function taking an item, returns non-nil to skip.

Returns the count of successfully deleted items."
  (let* ((id-field (or id-field 'id))
         (title-field (or title-field 'title))
         (id-property (or id-property "CANVAS_ID"))
         (full-endpoint (org-canvas-api-course-endpoint endpoint))
         (remote-items (org-canvas-api-request-all-pages 'GET full-endpoint list-params)))

    (elog-info org-canvas--logger "Found %d %s on Canvas" (length remote-items) feature-name)

    (let* ((result (org-canvas--delete-items-queued
                    remote-items
                    (lambda (item-id)
                      (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) item-id))
                    id-field title-field skip-fn))
           (deleted-count (car result)))

      ;; Cleanup local properties
      (when (and file (file-exists-p file))
        (elog-info org-canvas--logger "Cleaning local properties...")
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (elog-debug org-canvas--logger "Removing properties for: %s"
               (org-entry-get (point) id-property))
             (org-canvas-clear-sync-properties (point)))
           (format "%s={.}" id-property) 'file)
          (save-buffer)
          (elog-info org-canvas--logger "Saved %s" file)))

      (elog-info org-canvas--logger "========================================")
      (elog-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d removed" deleted-count)
      (elog-info org-canvas--logger "========================================")

      deleted-count)))

(cl-defun org-canvas--delete-item-at-point (feature-name
                                            &key
                                            endpoint
                                            id-property)
  "Generic implementation for deleting the item at the current Org heading.

FEATURE-NAME is a string like \"assignment\" or \"page\".

Keyword arguments:
  ENDPOINT - API endpoint pattern with %s for ID (e.g., \"assignments/%s\").
  ID-PROPERTY - Org property name for ID (default: \"CANVAS_ID\").

Return non-nil if deletion succeeded."
  (org-back-to-heading t)
  (let* ((id-property (or id-property "CANVAS_ID"))
         (pom (point))
         (canvas-id (org-canvas-org-get-property pom id-property))
         (title (org-get-heading t t t t)))

    (unless canvas-id
      (user-error "No %s property found for this heading" id-property))

    (when (y-or-n-p (format "Delete '%s' from Canvas? " title))
      (org-canvas-clear-log)
      (display-buffer (get-buffer-create "*canvas-log*"))
      (elog-info org-canvas--logger "Deleting %s '%s' (ID: %s)..." feature-name title canvas-id)

      (condition-case err
          (progn
            (org-canvas-api-request 'DELETE
				    (org-canvas-api-course-endpoint endpoint canvas-id))
            (elog-info org-canvas--logger "Successfully deleted from Canvas")
            (org-canvas-clear-sync-properties pom)
            (elog-info org-canvas--logger "Cleaned local properties")
            (message "%s '%s' deleted." (capitalize feature-name) title)
            t)
        (error
         (elog-error org-canvas--logger "Failed to delete: %s" (cadr err))
         (message "Failed to delete %s. Check logs." feature-name)
         nil)))))

(defmacro org-canvas-define-delete-all (feature &rest args)
  "Define a delete-all function for FEATURE.

FEATURE is a symbol like \\='pages or \\='announcements.

ARGS is a plist with the following keys:
  :endpoint - API endpoint suffix (required)
  :file - Expression for org file path (required)
  :id-field - Alist key for item ID (default: \\='id)
  :title-field - Alist key for item title (default: \\='title)
  :id-property - Org property name (default: \"CANVAS_ID\")
  :list-params - Extra params for GET request
  :skip-fn - Function to skip certain items

Example:
  (org-canvas-define-delete-all announcements
    :endpoint \"discussion_topics\"
    :file org-canvas-announcements-file
    :list-params \\='((\"only_announcements\" . \"true\")))"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (feature-upper (upcase feature-name))
         (fn-name (intern (format "org-canvas-delete-all-%s" feature-name)))
         (endpoint (plist-get args :endpoint))
         (file-expr (plist-get args :file))
         (id-field (or (plist-get args :id-field) ''id))
         (title-field (or (plist-get args :title-field) ''title))
         (id-property (or (plist-get args :id-property) "CANVAS_ID"))
         (list-params (plist-get args :list-params))
         (skip-fn (plist-get args :skip-fn)))
    (unless endpoint (error "org-canvas-define-delete-all: :endpoint is required"))
    (unless file-expr (error "org-canvas-define-delete-all: :file is required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Delete ALL %s in the configured course." feature-name)
         (interactive)
         ;; Skip prompt when called from org-canvas-delete-all (which
         ;; already prompted and sets org-canvas--inhibit-log-clear).
         (unless org-canvas--inhibit-log-clear
           (unless (y-or-n-p ,(format "Delete ALL %s in this course? " feature-name))
             (user-error "Aborted")))

         (org-canvas-clear-log)
         (display-buffer (get-buffer-create "*canvas-log*"))
         (elog-warning org-canvas--logger "========================================")
         (elog-warning org-canvas--logger ">>> STARTING MASS DELETION OF %s" ,feature-upper)
         (elog-warning org-canvas--logger "========================================")

         (let ((deleted-count (org-canvas--delete-all-items ,feature-name
                                :endpoint ,endpoint
                                :file ,file-expr
                                :id-field ,id-field
                                :title-field ,title-field
                                :id-property ,id-property
                                :list-params ,list-params
                                :skip-fn ,skip-fn)))
           (message "%s deletion complete. %d removed." ,(capitalize feature-name) deleted-count))))))

(defmacro org-canvas-define-delete-at-point (feature &rest args)
  "Define a delete-at-point function for FEATURE.

FEATURE is a symbol like \\='page or \\='assignment.

ARGS is a plist with the following keys:
  :endpoint - API endpoint pattern with %s (required)
  :id-property - Org property name (default: \"CANVAS_ID\")

Example:
  (org-canvas-define-delete-at-point assignment
    :endpoint \"assignments/%s\")"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas-delete-%s-at-point" feature-name)))
         (endpoint (plist-get args :endpoint))
         (id-property (or (plist-get args :id-property) "CANVAS_ID")))
    (unless endpoint (error "org-canvas-define-delete-at-point: :endpoint is required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Delete the Canvas %s associated with the current Org heading." feature-name)
         (interactive)
         (org-canvas--delete-item-at-point ,feature-name
                                           :endpoint ,endpoint
                                           :id-property ,id-property)))))

;;;; 9. Push-at-Point Infrastructure

(defmacro org-canvas-define-push-at-point (feature &rest args)
  "Define a sync-at-point function for FEATURE.

FEATURE is a symbol like \\='page or \\='announcement.

ARGS is a plist with the following keys:
  :parse - Function to parse entry at point (required)
  :build - Function to build payload from parsed data (required)
  :push - Function to push payload to API (required)
  :finalize - Function to finalize after API response (required)
  :title-key - Plist key for display name in logs (default: :title)

Generates `org-canvas-sync-FEATURE-at-point' that runs the 4-stage
pipeline for the single heading at point."
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas-sync-%s-at-point" feature-name)))
         (parse-fn (plist-get args :parse))
         (build-fn (plist-get args :build))
         (push-fn (plist-get args :push))
         (finalize-fn (plist-get args :finalize))
         (title-key (or (plist-get args :title-key) :title)))
    (unless parse-fn (error "org-canvas-define-push-at-point: :parse is required"))
    (unless build-fn (error "org-canvas-define-push-at-point: :build is required"))
    (unless push-fn (error "org-canvas-define-push-at-point: :push is required"))
    (unless finalize-fn (error "org-canvas-define-push-at-point: :finalize is required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Sync the %s at point to Canvas." feature-name)
         (interactive)
         (org-back-to-heading t)
         (display-buffer (get-buffer-create "*canvas-log*"))
         (let* ((data (funcall ,parse-fn))
                (title (plist-get data ,title-key))
                (payload (funcall ,build-fn data))
                (payload-hash (md5 (json-encode payload)))
                (stored-hash (org-entry-get (point) "PAYLOAD_HASH"))
                (canvas-id (or (plist-get data :canvas-id)
                               (plist-get data :canvas-url))))
           (if (and stored-hash
                    (string= payload-hash stored-hash)
                    canvas-id)
               (progn
                 (elog-info org-canvas--logger "[Skip] '%s' unchanged" title)
                 (message "%s '%s' unchanged — skipped." ,(capitalize feature-name) title))
             (let ((response (funcall ,push-fn data payload)))
               (funcall ,finalize-fn data response)
               (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
               (save-buffer)
               (elog-info org-canvas--logger "[Sync] '%s' synced successfully" title)
               (message "%s '%s' synced." ,(capitalize feature-name) title))))))))

;;;; 10. Setup Wizard

(defconst org-canvas--skeleton-files
  '("assignments.org" "pages.org" "quizzes.org" "modules.org"
    "files.org" "outcomes.org" "rubrics.org" "discussions.org"
    "announcements.org" "assignment-groups.org" "sections.org"
    "settings.org")
  "List of org files to create in a new course skeleton.")

(defun org-canvas--write-credentials-file (dir url token course-id)
  "Write org-canvas-credentials.el in DIR with URL, TOKEN, COURSE-ID."
  (let ((file (expand-file-name "org-canvas-credentials.el" dir)))
    (with-temp-file file
      (insert ";;; org-canvas-credentials.el --- Course credentials  -*- lexical-binding: t; -*-\n\n")
      (insert ";; This file is NOT checked into version control.\n")
      (insert ";; It contains sensitive API credentials.\n\n")
      (insert (format "(setq org-canvas-directory %S)\n" dir))
      (insert (format "(setq org-canvas-base-url %S)\n" url))
      (insert (format "(setq org-canvas-api-token %S)\n" token))
      (insert (format "(setq org-canvas-course-id %S)\n" course-id))
      (insert "\n(provide 'org-canvas-credentials)\n")
      (insert ";;; org-canvas-credentials.el ends here\n"))
    file))

(defun org-canvas--create-skeleton-files (dir)
  "Create minimal skeleton .org files in DIR."
  (dolist (filename org-canvas--skeleton-files)
    (let ((file (expand-file-name filename dir)))
      (unless (file-exists-p file)
        (with-temp-file file
          (insert (format "#+TITLE: %s\n"
                          (capitalize (file-name-sans-extension filename))))
          (insert "# See documentation/manual.org for property reference\n"))))))

;;;###autoload
(defun org-canvas-init ()
  "Set up org-canvas for a new course.
Prompts for required configuration, tests the connection,
and writes org-canvas-credentials.el."
  (interactive)
  (let* ((dir (read-directory-name "Course directory: " nil nil t))
         (url (read-string "Canvas base URL: " "https://canvas.instructure.com"))
         (token (read-string "API token: "))
         (course-id (read-string "Course ID: ")))
    ;; Validate inputs
    (when (string-empty-p token)
      (user-error "API token cannot be empty"))
    (when (string-empty-p course-id)
      (user-error "Course ID cannot be empty"))
    ;; Test connection before writing config
    (message "Testing connection...")
    (let ((org-canvas-base-url url)
          (org-canvas-api-token token)
          (org-canvas-course-id course-id))
      (condition-case err
          (let ((course (org-canvas-api-request 'GET
                          (org-canvas-api-course-endpoint ""))))
            (message "Connected to: %s" (alist-get 'name course)))
        (error
         (if (y-or-n-p (format "Connection failed: %s\nSave credentials anyway? "
                               (error-message-string err)))
             (message "Saving credentials without connection verification...")
           (user-error "Aborted")))))
    ;; Write credentials
    (let ((cred-file (org-canvas--write-credentials-file dir url token course-id)))
      (message "Credentials saved to %s" cred-file))
    ;; Optionally create skeleton files
    (when (y-or-n-p "Create skeleton .org files for all content types? ")
      (org-canvas--create-skeleton-files dir)
      (message "Created skeleton files in %s" dir))
    ;; Load the new credentials
    (setq org-canvas-directory dir)
    (setq org-canvas-base-url url)
    (setq org-canvas-api-token token)
    (setq org-canvas-course-id course-id)
    (message "org-canvas initialized!  Use M-x org-canvas-status to see sync state.")))

(provide 'org-canvas-core)
;;; org-canvas-core.el ends here
