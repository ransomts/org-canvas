;;; org-canvas-core-log.el --- In-tree logger for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A minimal level-gated logger that writes to a buffer and/or file.  This
;; replaces the previous dependency on the third-party `elog' package, whose
;; upstream master no longer matches the 2.0 API org-canvas was written
;; against.  Only the surface area used by org-canvas is implemented.

;;; Code:

(require 'cl-lib)

(defconst org-canvas--log-level-priority
  '((trace . 0) (debug . 1) (info . 2) (warning . 3) (error . 4) (fatal . 5))
  "Priority values for log levels.  Higher means more severe.
A logger emits messages whose level priority is >= the logger's level.")

(cl-defstruct (org-canvas--logger
               (:constructor org-canvas--logger-make-internal)
               (:copier nil)
               (:conc-name org-canvas--logger-))
  name level handlers buffer file)

(cl-defun org-canvas--logger-make (&key (name "org-canvas")
                                        (level 'debug)
                                        (handlers '(buffer))
                                        (buffer "*org-canvas-log*")
                                        (file nil))
  "Create a new logger with NAME, LEVEL, HANDLERS, BUFFER, and FILE.
Keyword arguments mirror the elog 2.0 constructor used previously:
  :name     NAME — logger identifier (string).
  :level    LEVEL — minimum severity to emit (symbol; one of trace/debug/info/
            warning/error/fatal).  Defaults to debug.
  :handlers HANDLERS — list of output sinks; supported values are `buffer'
            and `file'.
  :buffer   BUFFER — buffer name used by the buffer handler.
  :file     FILE — file path used by the file handler."
  (org-canvas--logger-make-internal
   :name name :level level :handlers handlers
   :buffer buffer :file file))

(defun org-canvas--log-priority (level)
  "Return the numeric priority for log LEVEL, or nil if unknown."
  (cdr (assq level org-canvas--log-level-priority)))

(defun org-canvas--log-enabled-p (logger level)
  "Return non-nil when LOGGER should emit a message at LEVEL."
  (let ((threshold (org-canvas--log-priority (org-canvas--logger-level logger)))
        (severity (org-canvas--log-priority level)))
    (and threshold severity (>= severity threshold))))

(defun org-canvas--log-format (logger level message)
  "Format MESSAGE for LOGGER at LEVEL.
Output shape: \"[TIMESTAMP] [NAME] [LEVEL] MESSAGE\"."
  (format "[%s] [%s] [%s] %s"
          (format-time-string "%Y-%m-%d %H:%M:%S")
          (org-canvas--logger-name logger)
          (upcase (symbol-name level))
          message))

(defun org-canvas--log-write-buffer (logger formatted)
  "Append FORMATTED to LOGGER's buffer."
  (let ((name (org-canvas--logger-buffer logger)))
    (when (and name (stringp name))
      (with-current-buffer (get-buffer-create name)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert formatted)
          (insert "\n"))))))

(defun org-canvas--log-write-file (logger formatted)
  "Append FORMATTED to LOGGER's file, creating parent dirs as needed."
  (let ((file (org-canvas--logger-file logger)))
    (when (and file (stringp file))
      (let ((dir (file-name-directory file)))
        (when (and dir (not (file-exists-p dir)))
          (make-directory dir t)))
      (let ((coding-system-for-write 'utf-8))
        (write-region (concat formatted "\n") nil file 'append 'silent)))))

(defconst org-canvas--log-redactions
  '(("\\bBearer\\s-+[^\"'[:space:]]+" . "Bearer ***MASKED***")
    ("\\([A-Za-z0-9_-]*\\(?:session\\|csrf\\|token\\)[A-Za-z0-9_-]*=\\)[^;&\"'[:space:]]+"
     . "\\1***MASKED***"))
  "Redaction rules applied to every log message before it is written.
Each entry is (REGEXP . REPLACEMENT), matched case-insensitively.
The first rule masks bearer tokens; the second masks credential-bearing
cookie and query-string values (canvas_session, _csrf_token,
log_session_id, access_token, ...) wherever they appear, including
inside printed plz-error structs.")

(defun org-canvas--log-redact (message)
  "Return MESSAGE with secrets masked per `org-canvas--log-redactions'."
  (let ((case-fold-search t))
    (dolist (rule org-canvas--log-redactions message)
      (setq message (replace-regexp-in-string (car rule) (cdr rule) message)))))

(defun org-canvas--log-dispatch (logger level format-string args)
  "Emit a log entry on LOGGER at LEVEL formatted from FORMAT-STRING and ARGS.
The formatted message passes through `org-canvas--log-redact' so
credentials (bearer tokens, session cookies) never reach the log."
  (when (and logger (org-canvas--log-enabled-p logger level))
    (let* ((message (org-canvas--log-redact (apply #'format format-string args)))
           (formatted (org-canvas--log-format logger level message))
           (handlers (org-canvas--logger-handlers logger)))
      (when (memq 'buffer handlers)
        (org-canvas--log-write-buffer logger formatted))
      (when (memq 'file handlers)
        (org-canvas--log-write-file logger formatted)))))

(defun org-canvas--log-trace (logger format-string &rest args)
  "Log on LOGGER at trace level using FORMAT-STRING and ARGS."
  (org-canvas--log-dispatch logger 'trace format-string args))

(defun org-canvas--log-debug (logger format-string &rest args)
  "Log on LOGGER at debug level using FORMAT-STRING and ARGS."
  (org-canvas--log-dispatch logger 'debug format-string args))

(defun org-canvas--log-info (logger format-string &rest args)
  "Log on LOGGER at info level using FORMAT-STRING and ARGS."
  (org-canvas--log-dispatch logger 'info format-string args))

(defun org-canvas--log-warning (logger format-string &rest args)
  "Log on LOGGER at warning level using FORMAT-STRING and ARGS."
  (org-canvas--log-dispatch logger 'warning format-string args))

(defun org-canvas--log-error (logger format-string &rest args)
  "Log on LOGGER at error level using FORMAT-STRING and ARGS."
  (org-canvas--log-dispatch logger 'error format-string args))

(defun org-canvas--logger-set-level (logger level)
  "Set LOGGER's minimum level to LEVEL and return LOGGER."
  (setf (org-canvas--logger-level logger) level)
  logger)

(defun org-canvas--logger-set-file (logger file)
  "Set LOGGER's file path to FILE and return LOGGER."
  (setf (org-canvas--logger-file logger) file)
  logger)

(defun org-canvas--logger-set-handlers (logger handlers)
  "Set LOGGER's handler list to HANDLERS and return LOGGER."
  (setf (org-canvas--logger-handlers logger) handlers)
  logger)

(provide 'org-canvas-core-log)
;;; org-canvas-core-log.el ends here
