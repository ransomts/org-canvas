;;; org-canvas-core-config.el --- Configuration and logging for org-canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration defcustoms, path utilities, and the elog logging layer.
;; This is the lowest layer in the org-canvas-core dependency chain.

;;; Code:

(require 'json)
(require 'elog)

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
            (elog-set-file org-canvas--logger (org-canvas--log-file-path))))))

(defun org-canvas--start-operation (operation-name)
  "Clear log, display log buffer, and log OPERATION-NAME banner.
Respects `org-canvas--inhibit-log-clear'."
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> %s" operation-name)
  (elog-info org-canvas--logger "========================================"))

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

(defconst org-canvas--valid-self-signup-values
  '("enabled" "restricted")
  "Valid values for SELF_SIGNUP.")

(defconst org-canvas--valid-auto-leader-values
  '("first" "random")
  "Valid values for AUTO_LEADER.")

(provide 'org-canvas-core-config)
;;; org-canvas-core-config.el ends here
