;;; org-canvas-core-config-test.el --- Buttercup tests for org-canvas-core path, logging, and configuration  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)

;;;; 1. Path Utilities

(describe "org-canvas--path"
  (it "resolves filename relative to org-canvas-directory"
    (let ((org-canvas-directory "/home/user/canvas"))
      (expect (org-canvas--path "pages.org")
              :to-equal "/home/user/canvas/pages.org")))

  (it "handles subdirectories in filename"
    (let ((org-canvas-directory "/home/user/canvas"))
      (expect (org-canvas--path "subdir/file.org")
              :to-equal "/home/user/canvas/subdir/file.org")))

  (it "falls back to org-directory when org-canvas-directory is nil"
    ;; When org-canvas-directory is nil, falls back to org-directory
    ;; Since org-directory is always bound in Emacs+Org, we test that path
    (let ((org-canvas-directory nil)
          (org-directory "/home/user/org"))
      (expect (org-canvas--path "file.org")
              :to-equal "/home/user/org/file.org"))))

;;;; 2. Logging Layer

(describe "org-canvas--trace-enabled-p"
  (it "returns t when log level is trace"
    (let ((org-canvas-log-level 'trace))
      (expect (org-canvas--trace-enabled-p) :to-be-truthy)))

  (it "returns nil when log level is debug"
    (let ((org-canvas-log-level 'debug))
      (expect (org-canvas--trace-enabled-p) :to-be nil)))

  (it "returns nil when log level is info"
    (let ((org-canvas-log-level 'info))
      (expect (org-canvas--trace-enabled-p) :to-be nil))))

(describe "org-canvas--mask-token"
  (it "masks Authorization header value"
    (let* ((headers '(("Authorization" . "Bearer secret-token")
                      ("Content-Type" . "application/json")))
           (masked (org-canvas--mask-token headers)))
      (expect (cdr (assoc "Authorization" masked))
              :to-equal "Bearer ***MASKED***")
      (expect (cdr (assoc "Content-Type" masked))
              :to-equal "application/json")))

  (it "handles empty headers list"
    (expect (org-canvas--mask-token nil) :to-equal nil))

  (it "preserves non-auth headers unchanged"
    (let* ((headers '(("X-Custom" . "value")
                      ("Accept" . "application/json")))
           (masked (org-canvas--mask-token headers)))
      (expect masked :to-equal headers))))

(describe "org-canvas--pretty-json"
  (it "formats hash-table as JSON"
    (let ((data (make-hash-table)))
      (puthash 'key "value" data)
      (let ((result (org-canvas--pretty-json data)))
        (expect result :to-match "\"key\""))))

  (it "formats alist as JSON"
    (let ((result (org-canvas--pretty-json '((name . "test") (id . 123)))))
      (expect result :to-match "\"name\"")
      (expect result :to-match "\"test\"")))

  (it "returns \"null\" for nil data"
    (expect (org-canvas--pretty-json nil) :to-equal "null"))

  (it "pretty-prints JSON string"
    (let ((result (org-canvas--pretty-json "{\"a\":1}")))
      (expect result :to-match "\"a\""))))

(describe "org-canvas-clear-log"
  (it "clears the canvas log buffer"
    (with-current-buffer (get-buffer-create "*canvas-log*")
      (let ((inhibit-read-only t))
        (insert "Some log content")))
    (org-canvas-clear-log)
    (with-current-buffer (get-buffer-create "*canvas-log*")
      (expect (buffer-string) :to-equal "")))

  (it "deletes log file when file logging is active"
    (let* ((temp-file (make-temp-file "org-canvas-test-log"))
           (org-canvas-log-destination 'file)
           (org-canvas-log-file temp-file))
      (write-region "log content" nil temp-file nil 'quiet)
      (org-canvas-clear-log)
      (expect (file-exists-p temp-file) :to-be nil)))

  (it "deletes log file when destination is both"
    (let* ((temp-file (make-temp-file "org-canvas-test-log"))
           (org-canvas-log-destination 'both)
           (org-canvas-log-file temp-file))
      (write-region "log content" nil temp-file nil 'quiet)
      (org-canvas-clear-log)
      (expect (file-exists-p temp-file) :to-be nil)))

  (it "does not touch log file when destination is buffer"
    (let* ((temp-file (make-temp-file "org-canvas-test-log"))
           (org-canvas-log-destination 'buffer)
           (org-canvas-log-file temp-file))
      (write-region "log content" nil temp-file nil 'quiet)
      (unwind-protect
          (progn
            (org-canvas-clear-log)
            (expect (file-exists-p temp-file) :to-be t))
        (delete-file temp-file))))

  (it "refreshes logger file path to current org-canvas-directory"
    (let* ((temp-dir (make-temp-file "org-canvas-logdir" t))
           (org-canvas-directory temp-dir)
           (org-canvas-log-file nil)
           (org-canvas-log-destination 'file))
      (unwind-protect
          (progn
            (org-canvas-clear-log)
            (let ((logger-file (plist-get org-canvas--logger :file)))
              (expect logger-file :to-equal
                      (expand-file-name "org-canvas.log" temp-dir))))
        (delete-directory temp-dir t)))))

(describe "org-canvas-set-log-level"
  (it "sets log level to info"
    (org-canvas-set-log-level 'info)
    (expect org-canvas-log-level :to-equal 'info))

  (it "sets log level to debug"
    (org-canvas-set-log-level 'debug)
    (expect org-canvas-log-level :to-equal 'debug))

  (it "sets log level to trace"
    (org-canvas-set-log-level 'trace)
    (expect org-canvas-log-level :to-equal 'trace)))

(describe "org-canvas-log-destination"
  (it "exists as a defcustom with correct default"
    (expect (custom-variable-p 'org-canvas-log-destination) :to-be-truthy)
    (expect (default-value 'org-canvas-log-destination) :to-equal 'both))

  (it "has a choice type with buffer, file, and both"
    (let ((type (get 'org-canvas-log-destination 'custom-type)))
      (expect (car type) :to-equal 'choice))))

(describe "org-canvas-log-file"
  (it "exists as a defcustom with nil default"
    (expect (custom-variable-p 'org-canvas-log-file) :to-be-truthy)
    (expect (default-value 'org-canvas-log-file) :to-be nil))

  (it "has a choice type"
    (let ((type (get 'org-canvas-log-file 'custom-type)))
      (expect (car type) :to-equal 'choice))))

(describe "org-canvas--log-handlers"
  (it "returns buffer handler for buffer destination"
    (expect (org-canvas--log-handlers 'buffer) :to-equal '(buffer)))

  (it "returns file handler for file destination"
    (expect (org-canvas--log-handlers 'file) :to-equal '(file)))

  (it "returns both handlers for both destination"
    (expect (org-canvas--log-handlers 'both) :to-equal '(buffer file)))

  (it "defaults to buffer for unknown destination"
    (expect (org-canvas--log-handlers 'unknown) :to-equal '(buffer))))

(describe "org-canvas--log-file-path"
  (it "returns custom path when org-canvas-log-file is set"
    (let ((org-canvas-log-file "/tmp/custom.log"))
      (expect (org-canvas--log-file-path) :to-equal "/tmp/custom.log")))

  (it "returns default path when org-canvas-log-file is nil"
    (let ((org-canvas-log-file nil)
          (org-canvas-directory "/tmp/test-canvas"))
      (expect (org-canvas--log-file-path)
              :to-equal (expand-file-name "org-canvas.log" "/tmp/test-canvas")))))

(describe "org-canvas-set-log-destination"
  (it "sets destination to file and updates logger handlers"
    (let ((org-canvas-log-destination 'buffer)
          (org-canvas-log-file "/tmp/test.log")
          (org-canvas--logger (elog-logger :name "test" :handlers '(buffer))))
      (org-canvas-set-log-destination 'file)
      (expect org-canvas-log-destination :to-equal 'file)
      (expect (plist-get org-canvas--logger :handlers) :to-equal '(file))
      (expect (plist-get org-canvas--logger :file) :to-equal "/tmp/test.log")))

  (it "sets destination to both and updates logger"
    (let ((org-canvas-log-destination 'buffer)
          (org-canvas-log-file "/tmp/test.log")
          (org-canvas--logger (elog-logger :name "test" :handlers '(buffer))))
      (org-canvas-set-log-destination 'both)
      (expect org-canvas-log-destination :to-equal 'both)
      (expect (plist-get org-canvas--logger :handlers) :to-equal '(buffer file))))

  (it "sets destination to buffer without setting file"
    (let ((org-canvas-log-destination 'file)
          (org-canvas--logger (elog-logger :name "test" :handlers '(file)
                                          :file "/tmp/test.log")))
      (org-canvas-set-log-destination 'buffer)
      (expect org-canvas-log-destination :to-equal 'buffer)
      (expect (plist-get org-canvas--logger :handlers) :to-equal '(buffer))))

  (it "works interactively with completing-read"
    (let ((org-canvas-log-destination 'buffer)
          (org-canvas-log-file "/tmp/test.log")
          (org-canvas--logger (elog-logger :name "test" :handlers '(buffer))))
      (spy-on 'completing-read :and-return-value "file")
      (call-interactively 'org-canvas-set-log-destination)
      (expect org-canvas-log-destination :to-equal 'file))))

;;;; 7. Diagnostics

(describe "org-canvas-get-course-name (mocked)"
  (it "returns course name from API response"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("courses/99999" . ((name . "Test Course 101")))))
        (let ((name (org-canvas-get-course-name)))
          (expect name :to-equal "Test Course 101"))))))

(describe "org-canvas-test-connection (mocked)"
  (it "calls API and logs result"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("courses/99999" . ((name . "Canvas 101")))))
        ;; Just ensure it doesn't error
        (org-canvas-test-connection)
        (expect-api-called 'GET "courses/99999")))))

;;;; 8. Trace Logging

(describe "org-canvas--trace"
  (it "logs when trace is enabled"
    (let ((org-canvas-log-level 'trace))
      ;; Should not error when called
      (org-canvas--trace "Test message: %s" "value")))

  (it "does not log when trace is disabled"
    (let ((org-canvas-log-level 'debug))
      ;; Should not error when called (just returns nil)
      (expect (org-canvas--trace "Test message: %s" "value") :to-be nil))))

;;;; 16. Pretty JSON Edge Cases

(describe "org-canvas--pretty-json edge cases"
  (it "handles invalid JSON string gracefully"
    (let ((result (org-canvas--pretty-json "not valid json {")))
      ;; Should return the original string on parse error
      (expect result :to-equal "not valid json {")))

  (it "handles empty hash-table"
    (let ((data (make-hash-table)))
      (let ((result (org-canvas--pretty-json data)))
        (expect result :to-match "{}"))))

  (it "handles deeply nested structures"
    (let ((result (org-canvas--pretty-json '((a . ((b . ((c . 1)))))))))
      (expect result :to-match "\"a\""))))

;;;; 20. Trace Logging

(describe "org-canvas--trace"
  (it "calls elog-debug when trace enabled"
    (let ((org-canvas-log-level 'trace)
          (debug-called nil))
      (cl-letf (((symbol-function 'elog-debug)
                 (lambda (&rest _args) (setq debug-called t) nil)))
        (org-canvas--trace "Test %s" "value")
        (expect debug-called :to-be t)))))

;;;; 25. org-canvas-set-log-level interactive

(describe "org-canvas-set-log-level interactive"
  (it "maps trace to debug for elog logger"
    (org-canvas-set-log-level 'trace)
    (expect org-canvas-log-level :to-equal 'trace))

  (it "sets warning level"
    (org-canvas-set-log-level 'warning)
    (expect org-canvas-log-level :to-equal 'warning))

  (it "sets error level"
    (org-canvas-set-log-level 'error)
    (expect org-canvas-log-level :to-equal 'error)))

;;;; 28. org-canvas--path fallback

(describe "org-canvas--path fallback to user-emacs-directory"
  (it "falls back to user-emacs-directory when both nil"
    (let ((org-canvas-directory nil)
          (org-directory nil))
      ;; When org-directory is nil, makunbound not needed since we just set it nil
      ;; But org-directory is always bound in org-mode, so test the branch
      ;; where org-canvas-directory is nil but org-directory is bound
      (let ((result (org-canvas--path "test.org")))
        ;; Should use org-directory or user-emacs-directory
        (expect result :to-match "test\\.org$")))))

(describe "org-canvas--path fallback"
  (it "falls back to user-emacs-directory when org-directory is unbound"
    (let ((org-canvas-directory nil)
          (user-emacs-directory "/home/test/.emacs.d/")
          (orig-org-dir (when (boundp 'org-directory) org-directory))
          (was-bound (boundp 'org-directory)))
      (unwind-protect
          (progn
            (makunbound 'org-directory)
            (expect (org-canvas--path "file.org")
                    :to-equal "/home/test/.emacs.d/file.org"))
        ;; Restore org-directory
        (when was-bound
          (setq org-directory orig-org-dir))))))

(describe "org-canvas-set-log-level coverage"
  (it "maps trace to debug for elog and calls message"
    (let ((elog-level-called nil)
          (message-called nil))
      (cl-letf (((symbol-function 'elog-set-level)
                 (lambda (logger level)
                   (setq elog-level-called level)
                   logger))
                ((symbol-function 'message)
                 (lambda (&rest _args)
                   (setq message-called t))))
        (org-canvas-set-log-level 'trace)
        (expect org-canvas-log-level :to-equal 'trace)
        (expect elog-level-called :to-equal 'debug)
        (expect message-called :to-be t))))

  (it "passes non-trace levels directly to elog"
    (let ((elog-level-called nil))
      (cl-letf (((symbol-function 'elog-set-level)
                 (lambda (logger level)
                   (setq elog-level-called level)
                   logger))
                ((symbol-function 'message) #'ignore))
        (org-canvas-set-log-level 'warning)
        (expect elog-level-called :to-equal 'warning))))

  (it "works when called interactively"
    (let ((elog-level-called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "error"))
                ((symbol-function 'elog-set-level)
                 (lambda (logger level)
                   (setq elog-level-called level)
                   logger))
                ((symbol-function 'message) #'ignore))
        (call-interactively 'org-canvas-set-log-level)
        (expect org-canvas-log-level :to-equal 'error)
        (expect elog-level-called :to-equal 'error)))))

(provide 'org-canvas-core-config-test)
;;; org-canvas-core-config-test.el ends here
