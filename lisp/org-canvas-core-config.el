;;; org-canvas-core-config.el --- Configuration and logging for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration defcustoms, path utilities, and the logging layer.
;; This is the lowest layer in the org-canvas-core dependency chain.

;;; Code:

(require 'json)
(require 'org-canvas-core-log)

;;;; 1. Configuration Layer

(defgroup org-canvas nil
  "Interface for Canvas LMS."
  :group 'external)

;;;; 1a. Error Symbols
;;
;; Custom error hierarchy.  All org-canvas-raised errors descend from
;; `org-canvas-error', so callers can `condition-case' on the parent to
;; catch any package-originated error, or on a specific child to handle
;; a single failure mode.

(define-error 'org-canvas-error "org-canvas error")
(define-error 'org-canvas-api-error
  "Canvas API request failed" 'org-canvas-error)
(define-error 'org-canvas-credentials-error
  "Canvas credentials missing or invalid" 'org-canvas-error)
(define-error 'org-canvas-conflict-error
  "Sync conflict between local and remote state" 'org-canvas-error)
(define-error 'org-canvas-timeout-error
  "Canvas API request timed out" 'org-canvas-api-error)
(define-error 'org-canvas-validation-error
  "Local data failed validation" 'org-canvas-error)
(define-error 'org-canvas-config-error
  "Org-canvas configuration or required file is missing" 'org-canvas-error)

(defmacro org-canvas--signal (type format-string &rest args)
  "Signal a TYPE error whose message comes from FORMAT-STRING and ARGS.
TYPE must be a symbol previously registered with `define-error', such
as `org-canvas-config-error' or `org-canvas-api-error'."
  (declare (indent 1))
  `(signal ,type (list (format ,format-string ,@args))))

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

(defvar org-canvas--base-url-normalizing nil
  "Non-nil while `org-canvas--base-url-watcher' is re-assigning the var.
Prevents the watcher from recursing on its own normalized assignment.")

(defun org-canvas--base-url-watcher (_symbol newval operation _where)
  "Strip trailing slashes from `org-canvas-base-url' when set.
NEWVAL is the incoming value; OPERATION is the kind of change
\(`set', `let', `unlet', `makunbound', `defvaralias').  Reacts only
to `set' so dynamic rebinding via `let' doesn't trigger normalization.

Many call sites concatenate the base URL directly with paths starting
with `/' (e.g. (format \"%s/api/v1/foo\" org-canvas-base-url)).  Without
this watcher, a user-supplied trailing slash produces URLs like
https://host//api/v1/...  Normalizing once at assignment time keeps every
inline call site clean.

Variable watchers fire BEFORE the actual assignment completes, so a
naive `setq' from inside the watcher would be overwritten by the
caller's pending assignment.  `run-at-time' with a 0 delay defers our
re-assignment until after the original `setq' finishes.  The
`org-canvas--base-url-normalizing' guard prevents infinite recursion
when our deferred `setq' fires the watcher again.

A `defcustom :set' slot was considered and rejected: `:set' only fires
for `customize-set-variable', not plain `setq'.  `org-canvas-credentials.el'
uses plain `setq', so `:set' would silently miss that path.  A variable
watcher fires for both."
  (when (and (eq operation 'set)
             (stringp newval)
             (not org-canvas--base-url-normalizing)
             (string-match-p "/+\\'" newval))
    (let ((normalized (replace-regexp-in-string "/+\\'" "" newval)))
      (run-at-time
       0 nil
       (lambda ()
         (let ((org-canvas--base-url-normalizing t))
           (setq-default org-canvas-base-url normalized)))))))

(add-variable-watcher 'org-canvas-base-url #'org-canvas--base-url-watcher)

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

(defcustom org-canvas-upload-timeout 120
  "Timeout for file upload requests in seconds.
Used by `url-retrieve-synchronously' during Canvas 3-step file uploads.
Separate from `org-canvas-request-timeout' which governs plz API calls."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-rate-limit-retries 3
  "Number of times to retry after a rate-limit (429/403) response."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-rate-limit-wait 10
  "Seconds to wait between rate-limit retries."
  :type 'integer
  :group 'org-canvas)

(defcustom org-canvas-transient-retry-delays '(5 10 20)
  "Sleep durations (seconds) between retries of transient API errors.
List length determines the maximum number of retries.  Transient errors
include curl timeouts (errors 28, 56, 7) and HTTP 502/503/504.  Rate
limit retries (HTTP 429) use `org-canvas-rate-limit-wait' separately."
  :type '(repeat integer)
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

;;;; 1b. Multi-Course Support

(defcustom org-canvas-courses nil
  "Alist of registered courses: ((NAME . DIRECTORY) ...).
Each DIRECTORY must contain an org-canvas-credentials.el file."
  :type '(alist :key-type string :value-type directory)
  :group 'org-canvas)

(defvar org-canvas--active-course-name nil
  "Name of the currently active course, or nil for single-course mode.")

(defvar org-canvas--file-var-registry nil
  "Alist of (VARIABLE-SYMBOL . FILENAME) for feature file paths.
Populated at module load time.  Used by `org-canvas--recompute-file-paths'.")

(defun org-canvas-register-file-var (var-symbol filename)
  "Register VAR-SYMBOL as a feature file-path variable with default FILENAME."
  (unless (assq var-symbol org-canvas--file-var-registry)
    (push (cons var-symbol filename) org-canvas--file-var-registry)))

(defvar org-canvas--feature-registry nil
  "List of feature plists for orphan detection.
Populated at module load time by `org-canvas-register-feature'.")

(defun org-canvas-register-feature (&rest plist)
  "Register a feature for orphan detection.
PLIST has keys :name :endpoint :file-var :id-field :id-property
:title-field and optionally :list-params :skip-fn."
  (let ((name (plist-get plist :name)))
    (unless (cl-find name org-canvas--feature-registry
                     :key (lambda (f) (plist-get f :name))
                     :test #'string=)
      (push plist org-canvas--feature-registry))))

(defun org-canvas--recompute-file-paths ()
  "Recompute all file-path variables from `org-canvas-directory'."
  (dolist (entry org-canvas--file-var-registry)
    (set (car entry) (org-canvas--path (cdr entry)))))

(defun org-canvas--directory-watcher (_symbol newval operation _where)
  "Auto-recompute file paths when `org-canvas-directory' is reassigned.
NEWVAL is the incoming value being assigned; OPERATION is the kind of
change (`set', `let', `unlet', `makunbound', `defvaralias').  Reacts
only to `set' so dynamic rebinding via `let' doesn't leak permanent
changes into the registered file vars during tests.

Without this watcher, defcustom forms like
\(defcustom org-canvas-modules-file (org-canvas--path \"modules.org\") ...)
bake their value at package-load time.  If a credentials file sets
`org-canvas-directory' afterwards via `setq', the file vars stay
pinned to whatever fallback `org-canvas--path' picked at load time
\(typically `org-directory')."
  (when (and (eq operation 'set)
             (stringp newval)
             (not (string-empty-p newval)))
    ;; Bind dynamically so `org-canvas--recompute-file-paths' (which
    ;; reads `org-canvas-directory' transitively via `org-canvas--path')
    ;; sees the incoming value — the watcher fires before the assignment.
    (let ((org-canvas-directory newval))
      (org-canvas--recompute-file-paths))))

(add-variable-watcher 'org-canvas-directory #'org-canvas--directory-watcher)

;;;; 1c. Property Registry

(defvar org-canvas--property-registry (make-hash-table :test 'equal)
  "Hash-table keyed by feature name (string), values are property spec plists.
Each plist has keys :file-var, :query, :properties, and optionally
:date-order and :structural-fn.  Populated at module load time by
`org-canvas-register-properties'.")

(defun org-canvas-register-properties (feature-name &rest plist)
  "Register property specs for FEATURE-NAME.
PLIST has keys :file-var, :query, :properties, and optionally
:date-order and :structural-fn.  Does nothing if FEATURE-NAME
is already registered (idempotent)."
  (unless (gethash feature-name org-canvas--property-registry)
    (puthash feature-name plist org-canvas--property-registry)))

(defun org-canvas--property-to-validate-prop (prop)
  "Convert a registry property spec PROP to validate.el format.
Registry keys: :org-prop :data-key :type :values :target-file
:link-id-property.  Validate keys: :name :type :values
:target-file :id-property."
  (let ((result (list :name (plist-get prop :org-prop)
                      :type (plist-get prop :type))))
    (when (plist-get prop :values)
      (setq result (plist-put result :values (plist-get prop :values))))
    (when (plist-get prop :target-file)
      (setq result (plist-put result :target-file (plist-get prop :target-file))))
    (when (plist-get prop :link-id-property)
      (setq result (plist-put result :id-property (plist-get prop :link-id-property))))
    result))

(defun org-canvas--get-validate-specs-from-registry ()
  "Build a list of validation specs from the property registry.
Each spec has :label, :file, :query, :properties, :date-order,
and :structural-fn, matching the format of `org-canvas--validate-specs'."
  (let (specs)
    (maphash
     (lambda (feature-name plist)
       (push (list :label (or (plist-get plist :label) feature-name)
                   :file (plist-get plist :file-var)
                   :query (plist-get plist :query)
                   :properties (mapcar #'org-canvas--property-to-validate-prop
                                       (plist-get plist :properties))
                   :date-order (plist-get plist :date-order)
                   :structural-fn (plist-get plist :structural-fn))
             specs))
     org-canvas--property-registry)
    (nreverse specs)))

;;; --- Path Utilities ---

(defun org-canvas--path (filename)
  "Return the absolute path to FILENAME in `org-canvas-directory`.
Falls back to `org-directory` or `user-emacs-directory`
if `org-canvas-directory` is not set, and warns about the fallback."
  (let* ((dir-set (and org-canvas-directory
                       (not (string-empty-p org-canvas-directory))))
         (base (if dir-set
                   org-canvas-directory
                 (if (boundp 'org-directory) org-directory user-emacs-directory))))
    (unless dir-set
      (when (boundp 'org-canvas--logger)
        (org-canvas--log-warning org-canvas--logger
          "[Config] org-canvas-directory not set, falling back to %s" base)))
    (expand-file-name filename base)))

;;;; 2. Logging Layer

(defconst org-canvas--log-buffer-name "*canvas-log*"
  "Name of the buffer used for org-canvas log output.")

(defcustom org-canvas-log-destination 'both
  "Where org-canvas writes log output.
- buffer: Log to the *canvas-log* buffer
- file: Log to a file
- both: Log to buffer and file (default, see `org-canvas-log-file')"
  :type '(choice (const :tag "Buffer only" buffer)
		 (const :tag "File only" file)
		 (const :tag "Buffer and file" both))
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'org-canvas--logger)
                    (fboundp 'org-canvas-set-log-destination))
           (org-canvas-set-log-destination val)))
  :group 'org-canvas)

(defcustom org-canvas-log-file nil
  "File path for log output when `org-canvas-log-destination' includes file.
When nil, defaults to \"org-canvas.log\" in `org-canvas-directory'."
  :type '(choice (const :tag "Default (org-canvas.log)" nil)
		 (file :tag "Custom file path"))
  :group 'org-canvas)

(defun org-canvas--log-handlers (destination)
  "Return the logger handler list for DESTINATION symbol.
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
  (org-canvas--logger-make
   :name "org-canvas"
   :handlers (org-canvas--log-handlers org-canvas-log-destination)
   :buffer org-canvas--log-buffer-name
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
underlying logger level.  Use `org-canvas-set-log-level' for convenience."
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
    (apply #'org-canvas--log-debug org-canvas--logger
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
Does nothing when `org-canvas--inhibit-log-clear' is non-nil.
Also refreshes the logger's file path to the current `org-canvas-directory',
since the logger is initialized at load time when the directory may not
yet be set."
  (unless org-canvas--inhibit-log-clear
    (with-current-buffer (get-buffer-create org-canvas--log-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (read-only-mode -1)))
    (when (memq org-canvas-log-destination '(file both))
      (let ((log-file (org-canvas--log-file-path)))
        (when (file-exists-p log-file)
          (delete-file log-file)))
      ;; Refresh the logger's file path to reflect current org-canvas-directory.
      ;; The logger defvar is evaluated once at load time, when
      ;; org-canvas-directory may still be "" (its default), causing the log
      ;; to land in the working directory instead of the course directory.
      (setq org-canvas--logger
            (org-canvas--logger-set-file org-canvas--logger
                                         (org-canvas--log-file-path))))))

(defun org-canvas--start-operation (operation-name)
  "Clear log, display log buffer, and log OPERATION-NAME banner.
Respects `org-canvas--inhibit-log-clear'."
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (org-canvas--log-info org-canvas--logger "========================================")
  (org-canvas--log-info org-canvas--logger ">>> %s" operation-name)
  (org-canvas--log-info org-canvas--logger "========================================"))

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
  (let ((target-level (if (eq level 'trace) 'debug level)))
    (setq org-canvas--logger
	  (org-canvas--logger-set-level org-canvas--logger target-level)))
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
	(org-canvas--logger-set-handlers org-canvas--logger
                                         (org-canvas--log-handlers destination)))
  (when (memq destination '(file both))
    (setq org-canvas--logger
	  (org-canvas--logger-set-file org-canvas--logger
                                       (org-canvas--log-file-path))))
  (message "org-canvas log destination set to: %s" destination))

;;;; 3. Shared Enum Constants
;;
;; Canonical definitions for enum values used by both feature modules
;; and the validation engine.  Avoids duplicating these lists in
;; multiple files.

(defconst org-canvas--valid-grading-types
  '("points" "percent" "letter_grade" "gpa_scale" "pass_fail" "not_graded")
  "Valid grading types for assignments and graded discussions.")

(defconst org-canvas--valid-submission-types
  '("online_upload" "online_url" "online_text_entry" "media_recording"
    "on_paper" "external_tool" "none")
  "Valid submission types for assignments.")

(defconst org-canvas--valid-quiz-types
  '("assignment" "practice_quiz" "graded_survey" "survey")
  "Valid quiz types.")

(defconst org-canvas--valid-question-types
  '("multiple_choice_question" "true_false_question"
    "short_answer_question" "fill_in_multiple_blanks_question"
    "multiple_dropdowns_question" "multiple_answers_question"
    "matching_question" "numerical_question" "essay_question"
    "file_upload_question" "text_only_question" "group")
  "Valid quiz question types.")

(defconst org-canvas--valid-hide-results
  '("always" "until_after_last_attempt")
  "Valid hide_results values for quizzes.")

(defconst org-canvas--valid-scoring-policies
  '("keep_highest" "keep_latest")
  "Valid scoring_policy values for quizzes.")

(defconst org-canvas--valid-editing-roles
  '("teachers" "students" "members" "public")
  "Valid editing roles for pages.")

(defconst org-canvas--valid-discussion-types
  '("side_comment" "threaded")
  "Valid discussion types.")

(defconst org-canvas--valid-completion-requirements
  '("must_view" "must_submit" "must_contribute" "min_score")
  "Valid completion requirement types for module items.")

(defconst org-canvas--valid-calculation-methods
  '("highest" "latest" "decaying_average" "n_mastery")
  "Valid calculation methods for outcomes.")

(defconst org-canvas--valid-use-justifications
  '("own_copyright" "used_by_permission" "fair_use" "public_domain" "creative_commons")
  "Valid use_justification values for file usage rights.")

(defconst org-canvas--valid-new-quiz-types
  '("choice" "true-false" "multi-answer" "short-answer"
    "essay" "file-upload" "numerical" "matching"
    "ordering" "categorization" "fill-in-the-blank" "hot-spot")
  "Valid TYPE values for New Quiz items.")

(defconst org-canvas--valid-new-quiz-scoring-policies
  '("keep_highest" "keep_latest" "keep_average")
  "Valid scoring policy values for New Quizzes.")

(defconst org-canvas--valid-views
  '("feed" "wiki" "modules" "syllabus" "assignments")
  "Valid values for DEFAULT_VIEW in settings.")

(defconst org-canvas--valid-licenses
  '("private" "cc_by" "cc_by_sa" "cc_by_nc" "cc_by_nc_sa"
    "cc_by_nd" "cc_by_nc_nd" "public_domain")
  "Valid values for LICENSE in settings.")

(defconst org-canvas--valid-late-intervals
  '("day" "hour")
  "Valid values for LATE_SUBMISSION_INTERVAL in settings.")

(defconst org-canvas--valid-self-signup-values
  '("enabled" "restricted")
  "Valid values for SELF_SIGNUP.")

(defconst org-canvas--valid-auto-leader-values
  '("first" "random")
  "Valid values for AUTO_LEADER.")

(provide 'org-canvas-core-config)
;;; org-canvas-core-config.el ends here
