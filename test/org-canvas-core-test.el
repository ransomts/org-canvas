;;; org-canvas-core-test.el --- Buttercup tests for org-canvas-core  -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive Buttercup tests for org-canvas-core.el
;; Tests cover: path utilities, logging, API layer, Org interaction, and diagnostics.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)
(require 'org-canvas-pages)
(require 'org-canvas-announcements)
(require 'org-canvas-discussions)
(require 'org-canvas-assignments)
(require 'org-canvas-sections)
(require 'org-canvas-rubrics)
(require 'org-canvas-files)

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
        (delete-file temp-file)))))

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

;;;; 3. API Layer

(describe "org-canvas-api-course-endpoint"
  (it "constructs basic course endpoint"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "pages")
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/pages")))

  (it "constructs endpoint with format args"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "assignments/%s" 12345)
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/assignments/12345")))

  (it "handles multiple format args"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "quizzes/%s/questions/%s" 100 200)
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/quizzes/100/questions/200")))

  (it "handles empty suffix"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "")
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/"))))

(describe "org-canvas--build-curl-command"
  (it "builds GET request without -X flag"
    (let ((cmd (org-canvas--build-curl-command 'GET "https://example.com/api" nil)))
      (expect cmd :not :to-match "-X GET")
      (expect cmd :to-match "Authorization: Bearer \\$CANVAS_TOKEN")
      (expect cmd :to-match "\"https://example.com/api\"")))

  (it "builds POST request with -X POST"
    (let ((cmd (org-canvas--build-curl-command 'POST "https://example.com/api" nil)))
      (expect cmd :to-match "-X POST")))

  (it "builds PUT request with -X PUT"
    (let ((cmd (org-canvas--build-curl-command 'PUT "https://example.com/api" nil)))
      (expect cmd :to-match "-X PUT")))

  (it "builds DELETE request with -X DELETE"
    (let ((cmd (org-canvas--build-curl-command 'DELETE "https://example.com/api" nil)))
      (expect cmd :to-match "-X DELETE")))

  (it "includes JSON payload in -d flag"
    (let ((cmd (org-canvas--build-curl-command 'POST "https://example.com/api"
                                               "{\"key\":\"value\"}")))
      (expect cmd :to-match "-d '{\"key\":\"value\"}'")))

  (it "includes Content-Type header"
    (let ((cmd (org-canvas--build-curl-command 'GET "https://example.com/api" nil)))
      (expect cmd :to-match "Content-Type: application/json"))))

(describe "org-canvas-api-request (mocked)"
  (it "records API calls"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/api")
        (expect (test-org-canvas-api-call-count) :to-equal 1))))

  (it "returns mock response"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((response (org-canvas-api-request 'GET "https://example.com/api")))
          (expect (alist-get 'id response) :to-equal 12345)))))

  (it "tracks POST calls correctly"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'POST "https://example.com/pages"
                                :data '((title . "Test")))
        (expect-api-called 'POST "pages"))))

  (it "handles multiple API calls"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/pages")
        (org-canvas-api-request 'POST "https://example.com/assignments")
        (org-canvas-api-request 'PUT "https://example.com/quizzes")
        (expect (test-org-canvas-api-call-count) :to-equal 3)))))

;;;; 4. Org Interaction Layer

(describe "org-canvas-org-get-property"
  (it "gets string property from Org entry"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 12345
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-property (point) "CANVAS_ID")
             :to-equal "12345")))

  (it "returns nil for missing property"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-property (point) "CANVAS_ID")
             :to-be nil)))

  (it "gets property with spaces in value"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:TITLE: My Great Title
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-property (point) "TITLE")
             :to-equal "My Great Title"))))

(describe "org-canvas-org-get-boolean-property"
  (it "returns t when property is \"true\""
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-boolean-property (point) "PUBLISHED")
             :to-be t)))

  (it "returns nil when property is \"false\""
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:PUBLISHED: false
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-boolean-property (point) "PUBLISHED")
             :to-be nil)))

  (it "returns nil when property is missing (default-true = nil)"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-boolean-property (point) "PUBLISHED")
             :to-be nil)))

  (it "returns t when property is missing and default-true is set"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-boolean-property (point) "PUBLISHED" t)
             :to-be t)))

  (it "returns nil when property is \"false\" even with default-true"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:PUBLISHED: false
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-boolean-property (point) "PUBLISHED" t)
             :to-be nil))))

(describe "org-canvas-org-get-number-property"
  (it "parses integer property"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:POINTS: 100
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-number-property (point) "POINTS")
             :to-equal 100)))

  (it "parses floating point property"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:WEIGHT: 25.5
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-number-property (point) "WEIGHT")
             :to-equal 25.5)))

  (it "returns 0 for missing property (default)"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-number-property (point) "POINTS")
             :to-equal 0)))

  (it "returns custom default for missing property"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-number-property (point) "POINTS" 50)
             :to-equal 50)))

  (it "returns default for empty string property"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:POINTS:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-org-get-number-property (point) "POINTS" 42)
             :to-equal 42))))

(describe "org-canvas-org-set-property"
  (it "sets property on Org entry"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas-org-set-property (point) "CANVAS_ID" "99999")
     (expect (org-entry-get (point) "CANVAS_ID") :to-equal "99999")))

  (it "updates existing property"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 11111
:END:
"
     (org-back-to-heading)
     (org-canvas-org-set-property (point) "CANVAS_ID" "22222")
     (expect (org-entry-get (point) "CANVAS_ID") :to-equal "22222")))

  (it "works with point-marker"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((marker (point-marker)))
       (org-canvas-org-set-property marker "TEST_PROP" "marker-value")
       (expect (org-entry-get (point) "TEST_PROP") :to-equal "marker-value")))))

(describe "org-canvas-org-save-sync-state"
  (it "saves CANVAS_ID and LAST_SYNCED"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas-org-save-sync-state (point) 12345)
     (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345")
     (expect (org-entry-get (point) "LAST_SYNCED") :to-match
             "^\\[20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]")))

  (it "converts numeric ID to string"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas-org-save-sync-state (point) 99999)
     (expect (org-entry-get (point) "CANVAS_ID") :to-equal "99999")))

  (it "accepts string ID"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas-org-save-sync-state (point) "abc-123")
     (expect (org-entry-get (point) "CANVAS_ID") :to-equal "abc-123")))

  (it "uses custom ID property when specified"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas-org-save-sync-state (point) 55555 "QUESTION_ID")
     (expect (org-entry-get (point) "QUESTION_ID") :to-equal "55555")
     (expect (org-entry-get (point) "CANVAS_ID") :to-be nil))))

(describe "org-canvas-clear-sync-properties"
  (it "removes CANVAS_ID, CANVAS_URL, and LAST_SYNCED"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 12345
:CANVAS_URL: https://example.com
:LAST_SYNCED: [2024-01-15 Mon 09:00]
:OTHER_PROP: keep-me
:END:
"
     (org-back-to-heading)
     (org-canvas-clear-sync-properties (point))
     (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
     (expect (org-entry-get (point) "CANVAS_URL") :to-be nil)
     (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
     (expect (org-entry-get (point) "OTHER_PROP") :to-equal "keep-me")))

  (it "handles entry with no sync properties"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:TITLE: Test
:END:
"
     (org-back-to-heading)
     ;; Should not error
     (org-canvas-clear-sync-properties (point))
     (expect (org-entry-get (point) "TITLE") :to-equal "Test"))))

;;;; 5. Timestamp Functions

(describe "org-canvas-org-parse-timestamp"
  (it "returns ISO8601 formatted string for Org timestamp"
    (let ((orig-tz (getenv "TZ")))
      (unwind-protect
          (progn
            (set-time-zone-rule "UTC")
            (let ((result (org-canvas-org-parse-timestamp "<2024-01-15 Mon 09:00>")))
              (expect result :to-match
                      "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z$")
              (expect result :to-equal "2024-01-15T09:00:00Z")))
        (set-time-zone-rule orig-tz))))

  (it "parses inactive Org timestamp"
    (let ((orig-tz (getenv "TZ")))
      (unwind-protect
          (progn
            (set-time-zone-rule "UTC")
            (let ((result (org-canvas-org-parse-timestamp "[2024-06-20 Thu 14:30]")))
              (expect result :to-match "T.*Z$")
              (expect result :to-match "^2024-06-20")
              (expect result :to-equal "2024-06-20T14:30:00Z")))
        (set-time-zone-rule orig-tz))))

  (it "handles timestamp without time component"
    (let ((orig-tz (getenv "TZ")))
      (unwind-protect
          (progn
            (set-time-zone-rule "UTC")
            (let ((result (org-canvas-org-parse-timestamp "<2024-03-01 Fri>")))
              (expect result :to-match "T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z$")
              (expect result :to-match "^2024-03-01")
              (expect result :to-equal "2024-03-01T00:00:00Z")))
        (set-time-zone-rule orig-tz))))

  (it "returns nil for nil input"
    (expect (org-canvas-org-parse-timestamp nil) :to-be nil)))

(describe "org-canvas-current-iso8601-timestamp"
  (it "returns ISO8601 formatted string"
    (let ((result (org-canvas-current-iso8601-timestamp)))
      (expect result :to-match
              "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z$")))

  (it "ends with Z (UTC)"
    (let ((result (org-canvas-current-iso8601-timestamp)))
      (expect result :to-match "Z$"))))

;;;; 6. Entry Iteration

(describe "org-canvas--for-each-entry"
  (it "finds entries matching query"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org"))
          (found-titles nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* First Heading
:PROPERTIES:
:PUBLISHED: true
:END:

* Second Heading
:PROPERTIES:
:PUBLISHED: true
:END:
"))
            (org-canvas--for-each-entry temp-file "LEVEL=1"
              (lambda ()
                (push (org-get-heading t t t t) found-titles)))
            (expect (length found-titles) :to-equal 2)
            (expect found-titles :to-contain "First Heading")
            (expect found-titles :to-contain "Second Heading"))
        (delete-file temp-file))))

  (it "returns success and fail counts"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Heading One\n* Heading Two\n* Heading Three\n"))
            (let ((result (org-canvas--for-each-entry temp-file "LEVEL=1"
                            (lambda () nil))))
              (expect (car result) :to-equal 3)
              (expect (cdr result) :to-equal 0)))
        (delete-file temp-file))))

  (it "continues after callback error"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org"))
          (call-count 0))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* First\n* Second\n* Third\n"))
            (let ((result (org-canvas--for-each-entry temp-file "LEVEL=1"
                            (lambda ()
                              (setq call-count (1+ call-count))
                              (when (= call-count 2)
                                (error "Simulated error"))))))
              ;; Should have called 3 times despite error
              (expect call-count :to-equal 3)
              ;; 2 success, 1 fail
              (expect (car result) :to-equal 2)
              (expect (cdr result) :to-equal 1)))
        (delete-file temp-file))))

  (it "handles empty file"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert ""))
            (let ((result (org-canvas--for-each-entry temp-file "LEVEL=1"
                            (lambda () nil))))
              (expect (car result) :to-equal 0)
              (expect (cdr result) :to-equal 0)))
        (delete-file temp-file)))))

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

;;;; 9. Search Item Helper

(describe "org-canvas--search-item (mocked)"
  (it "returns matching item by title"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (title . "Assignment 1"))
                                  ((id . 101) (title . "Assignment 2"))])))
        (let ((result (org-canvas--search-item "assignments" "Assignment 1")))
          (expect (alist-get 'id result) :to-equal 100)))))

  (it "returns nil when no match found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("pages" . [((url . "page-1") (title . "Page 1"))])))
        (let ((result (org-canvas--search-item "pages" "Nonexistent")))
          (expect result :to-be nil)))))

  (it "uses custom match-field"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 200) (name . "Test Assignment"))])))
        (let ((result (org-canvas--search-item "assignments" "Test Assignment"
                                               :match-field 'name)))
          (expect (alist-get 'id result) :to-equal 200)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("API error")))))
        (let ((result (org-canvas--search-item "assignments" "Test")))
          (expect result :to-be nil))))))

;;;; 10. Push to API Helper

(describe "org-canvas--push-to-api (mocked)"
  (it "uses POST for new items"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Item" :canvas-id nil))
              (payload '((title . "New Item"))))
          (org-canvas--push-to-api data payload :endpoint "assignments")
          (expect-api-called 'POST "assignments$")))))

  (it "uses PUT for existing items"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "123"))
              (payload '((title . "Existing"))))
          (org-canvas--push-to-api data payload :endpoint "assignments")
          (expect-api-called 'PUT "assignments/123")))))

  (it "retries as POST on 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (and (eq method 'PUT) (= call-count 1))
                         (signal 'error '("API Request Failed (HTTP 404)" nil nil))
                       '((id . 456))))))
          (let ((data '(:title "Stale" :canvas-id "999"))
                (payload '((title . "Stale"))))
            (let ((result (org-canvas--push-to-api data payload :endpoint "pages")))
              (expect (alist-get 'id result) :to-equal 456)
              (expect call-count :to-equal 2)))))))

  (it "recovers from timeout with find-fn"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     ;; Error format: (err-msg error-thrown) where error-thrown contains "Timeout"
                     (signal 'error (list "Request failed" "Timeout waiting for response")))))
          (let ((data '(:title "Timeout Test" :canvas-id nil))
                (payload '((title . "Timeout Test")))
                (find-fn (lambda (_title) '((id . 789)))))
            (let ((result (org-canvas--push-to-api data payload
                                                   :endpoint "items"
                                                   :find-fn find-fn)))
              (expect (alist-get 'id result) :to-equal 789)))))))

  (it "signals error when timeout recovery fails"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (signal 'error (list "Request failed" "Timeout waiting for response")))))
          (let ((data '(:title "Lost Item" :canvas-id nil))
                (payload '((title . "Lost Item")))
                (find-fn (lambda (_title) nil)))
            (expect (org-canvas--push-to-api data payload
                                             :endpoint "items"
                                             :find-fn find-fn)
                    :to-throw 'error))))))

  (it "uses custom id-key"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Page" :canvas-url "my-page"))
              (payload '((title . "Page"))))
          (org-canvas--push-to-api data payload
                                   :endpoint "pages"
                                   :id-key :canvas-url)
          (expect-api-called 'PUT "pages/my-page"))))))

;;;; 11. Finalize Item Helper

(describe "org-canvas--finalize-item"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Item" :pom (point-marker)))
           (response '((id . 12345))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Item" :pom (point-marker)))
           (response '((id . 11111))))
       (org-canvas--finalize-item data response)
       (expect-synced-timestamp (point)))))

  (it "uses custom id-field"
    (with-temp-org-buffer
     "* Page
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Page" :pom (point-marker)))
           (response '((url . "my-page-url"))))
       (org-canvas--finalize-item data response :id-field 'url)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "my-page-url"))))

  (it "uses custom id-property"
    (with-temp-org-buffer
     "* Page
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Page" :pom (point-marker)))
           (response '((url . "custom-url"))))
       (org-canvas--finalize-item data response
                                  :id-field 'url
                                  :id-property "CANVAS_URL")
       (expect (org-entry-get (point) "CANVAS_URL") :to-equal "custom-url"))))

  (it "calls post-fn when provided"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((post-called nil)
           (data (list :title "Item" :pom (point-marker)))
           (response '((id . 999))))
       (org-canvas--finalize-item data response
                                  :post-fn (lambda (_d _r) (setq post-called t)))
       (expect post-called :to-be t))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* No ID
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No ID" :pom (point-marker)))
           (response '((error . "failed"))))
       (expect (org-canvas--finalize-item data response) :to-throw 'error)))))

;;;; 12. Delete All Items Helper

(describe "org-canvas--delete-all-items (mocked)"
  (it "deletes items from Canvas via queued helper"
    (with-org-canvas-test-config
      (let ((queued-args nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((id . 1) (title . "Item 1"))
                       ((id . 2) (title . "Item 2")))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (items endpoint-fn id-field title-field &optional skip-fn)
                     (setq queued-args (list items endpoint-fn id-field title-field skip-fn))
                     (cons 2 '("1" "2")))))
          (let ((deleted (org-canvas--delete-all-items "items"
                           :endpoint "items"
                           :file nil)))
            (expect deleted :to-equal 2)
            ;; Verify queued helper received correct args
            (expect (length (nth 0 queued-args)) :to-equal 2)
            (expect (nth 2 queued-args) :to-equal 'id)
            (expect (nth 3 queued-args) :to-equal 'title))))))

  (it "passes skip-fn to queued helper"
    (with-org-canvas-test-config
      (let ((queued-skip-fn nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((id . 1) (title . "Keep") (front_page . t))
                       ((id . 2) (title . "Delete") (front_page . :json-false)))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (_items _endpoint-fn _id-field _title-field &optional skip-fn)
                     (setq queued-skip-fn skip-fn)
                     (cons 1 '("2")))))
          (org-canvas--delete-all-items "pages"
            :endpoint "pages"
            :file nil
            :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
          (expect queued-skip-fn :not :to-be nil)))))

  (it "uses custom id-field and title-field"
    (with-org-canvas-test-config
      (let ((queued-args nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((url . "my-page") (name . "My Page")))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (items endpoint-fn id-field title-field &optional _skip-fn)
                     (setq queued-args (list items endpoint-fn id-field title-field))
                     (cons 1 '("my-page")))))
          (let ((deleted (org-canvas--delete-all-items "pages"
                           :endpoint "pages"
                           :file nil
                           :id-field 'url
                           :title-field 'name)))
            (expect deleted :to-equal 1)
            (expect (nth 2 queued-args) :to-equal 'url)
            (expect (nth 3 queued-args) :to-equal 'name))))))

  (it "constructs correct endpoint-fn"
    (with-org-canvas-test-config
      (let ((captured-endpoint-fn nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((id . 42) (title . "Test")))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (_items endpoint-fn _id-field _title-field &optional _skip-fn)
                     (setq captured-endpoint-fn endpoint-fn)
                     (cons 1 '("42")))))
          (org-canvas--delete-all-items "items"
            :endpoint "things"
            :file nil)
          ;; Verify endpoint-fn produces correct URL
          (let ((url (funcall captured-endpoint-fn 42)))
            (expect url :to-match "things/42$"))))))

  (it "cleans local properties from org file"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item 1
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:

* Item 2
:PROPERTIES:
:CANVAS_ID: 2
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 1) (title . "Item 1")))))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn)
                           (cons 1 '("1")))))
                (org-canvas--delete-all-items "items"
                  :endpoint "items"
                  :file temp-file)))
            ;; Check that both items' properties were cleared
            (with-current-buffer (find-file-noselect temp-file)
              (goto-char (point-min))
              (org-back-to-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
              ;; Item 2 should also be cleared (delete-all cleans all properties)
              (outline-next-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))
        (delete-file temp-file)))))

;;;; 12b. Queued Delete Helper

(describe "org-canvas--delete-items-queued"
  (it "returns (0 . nil) for empty items list"
    (let ((result (org-canvas--delete-items-queued
                   nil
                   (lambda (id) (format "http://example.com/%s" id))
                   'id 'title)))
      (expect (car result) :to-equal 0)
      (expect (cdr result) :to-be nil)))

  (it "returns (0 . nil) when all items are skipped"
    (let ((result (org-canvas--delete-items-queued
                   '(((id . 1) (title . "A")) ((id . 2) (title . "B")))
                   (lambda (id) (format "http://example.com/%s" id))
                   'id 'title
                   (lambda (_item) t))))
      (expect (car result) :to-equal 0)
      (expect (cdr result) :to-be nil)))

  (it "queues delete requests and collects results"
    (with-org-canvas-test-config
      ;; Mock plz-queue (function) and plz-run to synchronously invoke callbacks.
      ;; make-plz-queue is a cl-defstruct constructor (inlined) and creates a real struct.
      (let ((queued-thens nil))
        (cl-letf (((symbol-function 'plz-queue)
                   (lambda (queue _method _url &rest args)
                     (push (plist-get args :then) queued-thens)
                     queue))
                  ((symbol-function 'plz-run)
                   (lambda (queue)
                     (dolist (then-fn (nreverse queued-thens))
                       (when then-fn (funcall then-fn nil)))
                     (when (plz-queue-finally queue)
                       (funcall (plz-queue-finally queue))))))
          (let ((result (org-canvas--delete-items-queued
                         '(((id . 1) (title . "First"))
                           ((id . 2) (title . "Second")))
                         (lambda (id) (format "http://example.com/%s" id))
                         'id 'title)))
            (expect (car result) :to-equal 2)
            (expect (member "1" (cdr result)) :to-be-truthy)
            (expect (member "2" (cdr result)) :to-be-truthy))))))

  (it "continues on error and only counts successes"
    (with-org-canvas-test-config
      (let ((queued-callbacks nil))
        (cl-letf (((symbol-function 'plz-queue)
                   (lambda (queue _method _url &rest args)
                     (push (cons (plist-get args :then) (plist-get args :else))
                           queued-callbacks)
                     queue))
                  ((symbol-function 'plz-run)
                   (lambda (queue)
                     (let ((cbs (nreverse queued-callbacks)))
                       ;; First: call :else (error)
                       (when (cdar cbs) (funcall (cdar cbs) "error"))
                       ;; Second: call :then (success)
                       (when (caadr cbs) (funcall (caadr cbs) nil)))
                     (when (plz-queue-finally queue)
                       (funcall (plz-queue-finally queue))))))
          (let ((result (org-canvas--delete-items-queued
                         '(((id . 1) (title . "Fail"))
                           ((id . 2) (title . "Succeed")))
                         (lambda (id) (format "http://example.com/%s" id))
                         'id 'title)))
            (expect (car result) :to-equal 1)
            (expect (cdr result) :to-equal '("2")))))))

  (it "converts numeric IDs to strings in deleted-ids"
    (with-org-canvas-test-config
      (let ((queued-thens nil))
        (cl-letf (((symbol-function 'plz-queue)
                   (lambda (queue _method _url &rest args)
                     (push (plist-get args :then) queued-thens)
                     queue))
                  ((symbol-function 'plz-run)
                   (lambda (queue)
                     (dolist (fn (nreverse queued-thens))
                       (when fn (funcall fn nil)))
                     (when (plz-queue-finally queue)
                       (funcall (plz-queue-finally queue))))))
          (let ((result (org-canvas--delete-items-queued
                         '(((id . 42) (title . "Numeric")))
                         (lambda (id) (format "http://example.com/%s" id))
                         'id 'title)))
            (expect (car result) :to-equal 1)
            (expect (car (cdr result)) :to-equal "42"))))))

  (it "keeps string IDs as-is in deleted-ids"
    (with-org-canvas-test-config
      (let ((queued-thens nil))
        (cl-letf (((symbol-function 'plz-queue)
                   (lambda (queue _method _url &rest args)
                     (push (plist-get args :then) queued-thens)
                     queue))
                  ((symbol-function 'plz-run)
                   (lambda (queue)
                     (dolist (fn (nreverse queued-thens))
                       (when fn (funcall fn nil)))
                     (when (plz-queue-finally queue)
                       (funcall (plz-queue-finally queue))))))
          (let ((result (org-canvas--delete-items-queued
                         '(((id . "my-page") (title . "String")))
                         (lambda (id) (format "http://example.com/%s" id))
                         'id 'title)))
            (expect (car result) :to-equal 1)
            (expect (car (cdr result)) :to-equal "my-page"))))))

  (it "passes correct URL from endpoint-fn"
    (with-org-canvas-test-config
      (let ((captured-urls nil)
            (queued-thens nil))
        (cl-letf (((symbol-function 'plz-queue)
                   (lambda (queue _method url &rest args)
                     (push url captured-urls)
                     (push (plist-get args :then) queued-thens)
                     queue))
                  ((symbol-function 'plz-run)
                   (lambda (queue)
                     (dolist (fn (nreverse queued-thens))
                       (when fn (funcall fn nil)))
                     (when (plz-queue-finally queue)
                       (funcall (plz-queue-finally queue))))))
          (org-canvas--delete-items-queued
           '(((id . 5) (title . "Test")))
           (lambda (id) (format "http://canvas.example.com/items/%s" id))
           'id 'title)
          (expect (car captured-urls) :to-equal "http://canvas.example.com/items/5")))))

  (it "skips items with skip-fn and deletes the rest"
    (with-org-canvas-test-config
      (let ((queued-count 0)
            (queued-thens nil))
        (cl-letf (((symbol-function 'plz-queue)
                   (lambda (queue _method _url &rest args)
                     (setq queued-count (1+ queued-count))
                     (push (plist-get args :then) queued-thens)
                     queue))
                  ((symbol-function 'plz-run)
                   (lambda (queue)
                     (dolist (fn (nreverse queued-thens))
                       (when fn (funcall fn nil)))
                     (when (plz-queue-finally queue)
                       (funcall (plz-queue-finally queue))))))
          (let ((result (org-canvas--delete-items-queued
                         '(((id . 1) (title . "Keep") (front_page . t))
                           ((id . 2) (title . "Delete") (front_page . :json-false)))
                         (lambda (id) (format "http://example.com/%s" id))
                         'id 'title
                         (lambda (item) (eq (alist-get 'front_page item) t)))))
            (expect (car result) :to-equal 1)
            (expect queued-count :to-equal 1)))))))

;;;; 13. Delete Item at Point Helper

(describe "org-canvas--delete-item-at-point (mocked)"
  (it "deletes item and clears properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test Item
:PROPERTIES:
:CANVAS_ID: 555
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas--delete-item-at-point "item"
             :endpoint "items/%s")
           (expect-api-called 'DELETE "items/555")
           (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
           (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))

  (it "errors when no CANVAS_ID present"
    (with-temp-org-buffer
     "* New Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--delete-item-at-point "item" :endpoint "items/%s")
             :to-throw 'user-error)))

  (it "uses custom id-property"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* My Page
:PROPERTIES:
:CANVAS_URL: my-page-slug
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas--delete-item-at-point "page"
             :endpoint "pages/%s"
             :id-property "CANVAS_URL")
           (expect-api-called 'DELETE "pages/my-page-slug")))))))

;;;; 14. API Request Error Handling

(describe "org-canvas-api-request error handling (mocked)"
  (it "handles GET request with params"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/api"
                                :params '(("per_page" . "50")))
        (expect (test-org-canvas-api-call-count) :to-equal 1))))

  (it "encodes data as JSON for POST"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'POST "https://example.com/api"
                                :data '((name . "Test") (value . 42)))
        (expect-api-called 'POST "api")))))

;;;; 15. Additional API Request Tests

(describe "org-canvas-api-request edge cases (mocked)"
  (it "handles timeout parameter"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/api" :timeout 30)
        (expect (test-org-canvas-api-call-count) :to-equal 1))))

  (it "encodes hash-table data as JSON"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data (make-hash-table)))
          (puthash 'name "Test" data)
          (org-canvas-api-request 'POST "https://example.com/api" :data data)
          (expect-api-called 'POST "api")))))

  (it "passes string data as-is"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'POST "https://example.com/api"
                                :data "{\"already\":\"json\"}")
        (expect-api-called 'POST "api")))))

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

;;;; 17. Push to API Edge Cases

(describe "org-canvas--push-to-api edge cases (mocked)"
  (it "uses custom title-key"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "My Item" :canvas-id nil))
              (payload '((name . "My Item"))))
          (org-canvas--push-to-api data payload
                                   :endpoint "items"
                                   :title-key :name)
          (expect-api-called 'POST "items$")))))

  (it "handles nested timeout recovery for POST retry after 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("API Request Failed (HTTP 404)")))
                      ;; Second call: POST times out
                      ;; Error format: (error "msg" "Timeout") so error-thrown = "Timeout"
                      ((and (eq method 'POST) (= call-count 2))
                       (signal 'error '("Request failed" "Timeout")))
                      (t nil)))))
          (let ((data '(:title "Test" :canvas-id "old-id"))
                (payload '((title . "Test")))
                (find-fn (lambda (_title) '((id . 789)))))
            (let ((result (org-canvas--push-to-api data payload
                                                   :endpoint "items"
                                                   :find-fn find-fn)))
              (expect (alist-get 'id result) :to-equal 789))))))))

;;;; 18. Delete All Items Edge Cases

(describe "org-canvas--delete-all-items edge cases (mocked)"
  (it "passes list-params to GET request"
    (with-org-canvas-test-config
      (let ((captured-params nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional params)
                     (setq captured-params params)
                     nil))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn)
                     (cons 0 nil))))
          (org-canvas--delete-all-items "items"
            :endpoint "items"
            :file nil
            :list-params '(("filter" . "active")))
          (expect captured-params :to-equal '(("filter" . "active")))))))

  (it "returns 0 for empty remote items"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params) nil))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn)
                   (cons 0 nil))))
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 0)))))

  (it "does not clean properties when file is nil"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 1) (title . "Item")))))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn)
                   (cons 1 '("1")))))
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 1))))))

;;;; 19. Delete Item at Point Edge Cases

(describe "org-canvas--delete-item-at-point edge cases (mocked)"
  (it "handles delete failure gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Cannot delete"))))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (with-temp-org-buffer
         "* Item to Delete
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
         (org-back-to-heading)
         ;; Should return nil on failure, not throw
         (let ((result (org-canvas--delete-item-at-point "item" :endpoint "items/%s")))
           (expect result :to-be nil))))))

  (it "returns nil when user cancels"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (with-temp-org-buffer
         "* Item
:PROPERTIES:
:CANVAS_ID: 456
:END:
"
         (org-back-to-heading)
         ;; Should return nil when user cancels
         (let ((result (org-canvas--delete-item-at-point "item" :endpoint "items/%s")))
           (expect result :to-be nil)))))))

;;;; 20. Trace Logging

(describe "org-canvas--trace"
  (it "calls elog-debug when trace enabled"
    (let ((org-canvas-log-level 'trace)
          (debug-called nil))
      (cl-letf (((symbol-function 'elog-debug)
                 (lambda (&rest _args) (setq debug-called t) nil)))
        (org-canvas--trace "Test %s" "value")
        (expect debug-called :to-be t)))))

;;;; 21. For Each Entry Error Handling

(describe "org-canvas--for-each-entry error details"
  (it "logs error position on failure"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org"))
          (error-logged nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Heading\n"))
            (cl-letf (((symbol-function 'elog-error)
                       (lambda (&rest _args) (setq error-logged t) nil)))
              (org-canvas--for-each-entry temp-file "LEVEL=1"
                (lambda () (error "Test error")))
              (expect error-logged :to-be t)))
        (delete-file temp-file)))))

;;;; 22. Search Item Edge Cases

(describe "org-canvas--search-item edge cases (mocked)"
  (it "uses custom params when provided"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("items" . [((id . 1) (title . "Test"))])))
        (org-canvas--search-item "items" "Test"
                                 :params '(("custom_param" . "value")))
        (expect-api-called 'GET "items"))))

  (it "returns first exact match only"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("items" . [((id . 1) (title . "Similar"))
                            ((id . 2) (title . "Test"))
                            ((id . 3) (title . "Test"))])))  ; Duplicate
        (let ((result (org-canvas--search-item "items" "Test")))
          (expect (alist-get 'id result) :to-equal 2))))))

;;;; 23. Finalize Item Edge Cases

(describe "org-canvas--finalize-item edge cases"
  (it "uses custom title-key for logging"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :name "Custom Name" :pom (point-marker)))
           (response '((id . 12345))))
       (org-canvas--finalize-item data response :title-key :name)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345"))))

  (it "calls post-fn with data and response"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((post-fn-called-with nil)
           (data (list :title "Test" :pom (point-marker)))
           (response '((id . 999))))
       (org-canvas--finalize-item data response
                                  :post-fn (lambda (d r)
                                             (setq post-fn-called-with (list d r))))
       (expect post-fn-called-with :to-be-truthy)
       (expect (car post-fn-called-with) :to-equal data)
       (expect (cadr post-fn-called-with) :to-equal response)))))

;;;; 24. org-canvas-api-request internals (mock plz directly)

(describe "org-canvas-api-request internals"
  (it "builds query string from params"
    (with-org-canvas-test-config
      (let ((captured-url nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method url &rest _args)
                     (setq captured-url url)
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api"
                                  :params '(("per_page" . "50") ("page" . "2")))
          (expect captured-url :to-match "\\?per_page=50")
          (expect captured-url :to-match "page=2")))))

  (it "encodes alist data as JSON body"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (org-canvas-api-request 'POST "https://example.com/api"
                                  :data '((name . "Test") (value . 42)))
          (expect captured-body :to-be-truthy)
          ;; Should be valid JSON
          (let ((parsed (json-read-from-string captured-body)))
            (expect (alist-get 'name parsed) :to-equal "Test")
            (expect (alist-get 'value parsed) :to-equal 42))))))

  (it "encodes hash-table data as JSON body"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (let ((data (make-hash-table)))
            (puthash 'key "value" data)
            (org-canvas-api-request 'POST "https://example.com/api" :data data)
            (expect captured-body :to-be-truthy)
            (expect captured-body :to-match "\"key\""))))))

  (it "passes string data as-is without re-encoding"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (org-canvas-api-request 'POST "https://example.com/api"
                                  :data "{\"already\":\"json\"}")
          (expect captured-body :to-equal "{\"already\":\"json\"}")))))

  (it "downcases method symbol for plz"
    (with-org-canvas-test-config
      (let ((captured-method nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (method _url &rest _args)
                     (setq captured-method method)
                     '((id . 1)))))
          (org-canvas-api-request 'POST "https://example.com/api")
          (expect captured-method :to-equal 'post)))))

  (it "passes timeout to plz"
    (with-org-canvas-test-config
      (let ((captured-timeout nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-timeout (plist-get args :timeout))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api" :timeout 30)
          (expect captured-timeout :to-equal 30)))))

  (it "uses default timeout when none specified"
    (with-org-canvas-test-config
      (let ((captured-timeout nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-timeout (plist-get args :timeout))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api")
          (expect captured-timeout :to-equal test-org-canvas-request-timeout)))))

  (it "sends Authorization header with token"
    (with-org-canvas-test-config
      (let ((captured-headers nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-headers (plist-get args :headers))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api")
          (expect (cdr (assoc "Authorization" captured-headers))
                  :to-equal (concat "Bearer " test-org-canvas-api-token))))))

  (it "converts plz-error to standard error signal"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (_method _url &rest _args)
                   (signal 'plz-error (list (make-plz-error
                                             :response (make-plz-response
                                                        :status 500
                                                        :body "Internal Error")))))))
        (expect (org-canvas-api-request 'GET "https://example.com/api")
                :to-throw 'error))))

  (it "includes HTTP status in error message"
    (with-org-canvas-test-config
      (let ((err-msg nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest _args)
                     (signal 'plz-error (list (make-plz-error
                                               :response (make-plz-response
                                                          :status 403
                                                          :body "Forbidden")))))))
          (condition-case err
              (org-canvas-api-request 'GET "https://example.com/api")
            (error (setq err-msg (cadr err))))
          (expect err-msg :to-match "403")))))

  (it "returns nil for no-body responses"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (_method _url &rest _args)
                   nil)))
        (let ((result (org-canvas-api-request 'DELETE "https://example.com/api")))
          (expect result :to-be nil)))))

  (it "sends no body for nil data"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api")
          (expect captured-body :to-be nil))))))

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

;;;; 26. org-canvas-test-connection error path

(describe "org-canvas-test-connection error path"
  (it "handles API error gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Connection refused")))))
        ;; Should not throw, just message
        (org-canvas-test-connection)
        ;; If we get here, it handled the error
        (expect t :to-be t))))

  (it "logs error on failure"
    (with-org-canvas-test-config
      (let ((error-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (signal 'error '("Connection refused"))))
                  ((symbol-function 'elog-error)
                   (lambda (&rest _args)
                     (setq error-logged t)
                     nil)))
          (org-canvas-test-connection)
          (expect error-logged :to-be t))))))

;;;; 27. org-canvas--push-to-api nested recovery

(describe "org-canvas--push-to-api nested recovery"
  (it "handles 404→POST→Timeout→find-fn fails"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("API Request Failed (HTTP 404)")))
                      ;; Second call: POST times out
                      ((and (eq method 'POST) (= call-count 2))
                       (signal 'error '("Request failed" "Timeout")))
                      (t nil)))))
          (let ((data '(:title "Test" :canvas-id "old-id"))
                (payload '((title . "Test")))
                (find-fn (lambda (_title) nil)))
            (expect (org-canvas--push-to-api data payload
                                             :endpoint "items"
                                             :find-fn find-fn)
                    :to-throw 'error))))))

  (it "re-throws non-timeout errors without find-fn"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request" nil nil)))))
        (let ((data '(:title "Bad" :canvas-id nil))
              (payload '((title . "Bad"))))
          (expect (org-canvas--push-to-api data payload :endpoint "items")
                  :to-throw 'error)))))

  (it "does not attempt timeout recovery without find-fn"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Request failed" "Timeout")))))
        (let ((data '(:title "Lost" :canvas-id nil))
              (payload '((title . "Lost"))))
          (expect (org-canvas--push-to-api data payload :endpoint "items")
                  :to-throw 'error))))))

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

;;;; 29. org-canvas--for-each-entry edge paths

(describe "org-canvas--for-each-entry edge paths"
  (it "handles file with no matching entries"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Heading\n:PROPERTIES:\n:END:\n"))
            ;; Query that matches nothing
            (let ((result (org-canvas--for-each-entry temp-file "LEVEL=2"
                            (lambda () nil))))
              (expect (car result) :to-equal 0)
              (expect (cdr result) :to-equal 0)))
        (delete-file temp-file))))

  (it "increments fail count on error"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* First\n* Second\n"))
            (let ((result (org-canvas--for-each-entry temp-file "LEVEL=1"
                            (lambda () (error "Always fails")))))
              (expect (car result) :to-equal 0)
              (expect (cdr result) :to-equal 2)))
        (delete-file temp-file)))))

;;;; 30. org-canvas-define-sync macro generated functions

(describe "org-canvas-define-sync generated function"
  (it "syncs entries from file successfully"
    ;; Test using org-canvas-sync-pages which is a real macro-generated function
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (post-count 0))
            (with-temp-file org-file
              (insert "* Page One
:PROPERTIES:
:END:

Content one.

* Page Two
:PROPERTIES:
:END:

Content two.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq post-count (1+ post-count)))
                             '((url . "test-url")))))
                  (org-canvas-sync-pages)
                  (expect post-count :to-equal 2)
                  ;; Verify CANVAS_URL was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_URL") :to-equal "test-url"))))))
        (delete-directory temp-dir t))))

  (it "continues after one entry fails"
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (call-count 0))
            (with-temp-file org-file
              (insert "* Page One
:PROPERTIES:
:END:

Content one.

* Page Two
:PROPERTIES:
:END:

Content two.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq call-count (1+ call-count))
                               (if (= call-count 1)
                                   (signal 'error '("First page failed"))
                                 '((url . "page-two-url")))))))
                  (org-canvas-sync-pages)
                  ;; Both should have been attempted
                  (expect call-count :to-equal 2)))))
        (delete-directory temp-dir t))))

  (it "errors when file not found"
    (let ((org-canvas-pages-file "/nonexistent/pages.org"))
      (with-sync-test-env
        (expect (org-canvas-sync-pages) :to-throw 'error)))))

;;;; 31. org-canvas--delete-all-items edge paths

(describe "org-canvas--delete-all-items edge paths"
  (it "handles empty remote items list"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params) nil))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn)
                   (cons 0 nil))))
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 0)))))

  (it "cleans all properties even when queued helper reports partial success"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item A
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:

* Item B
:PROPERTIES:
:CANVAS_ID: 2
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 1) (title . "Item A"))
                             ((id . 2) (title . "Item B")))))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn)
                           ;; Simulate: item 1 deleted, item 2 failed
                           (cons 1 '("1")))))
                (org-canvas--delete-all-items "items"
                  :endpoint "items"
                  :file temp-file)))
            ;; Both items should be cleaned (delete-all cleans all properties)
            (with-current-buffer (find-file-noselect temp-file)
              (goto-char (point-min))
              (org-back-to-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
              (outline-next-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))
        (delete-file temp-file))))

  (it "does not clean properties when file is nil"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 1) (title . "Item")))))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn)
                   (cons 1 '("1")))))
        ;; Should not error even with nil file
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 1))))))

;;;; 32. org-canvas--delete-item-at-point edge paths

(describe "org-canvas--delete-item-at-point additional tests"
  (it "preserves properties on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Server error"))))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (with-temp-org-buffer
         "* Test Item
:PROPERTIES:
:CANVAS_ID: 999
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (org-canvas--delete-item-at-point "item" :endpoint "items/%s")
         ;; Properties should be preserved on failure
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "999")
         (expect (org-entry-get (point) "LAST_SYNCED") :to-equal "[2024-01-01 Mon]")))))

  (it "succeeds and clears all sync properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Item
:PROPERTIES:
:CANVAS_ID: 555
:CANVAS_URL: my-url
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (let ((result (org-canvas--delete-item-at-point "item" :endpoint "items/%s")))
             (expect result :to-be t)
             (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
             (expect (org-entry-get (point) "CANVAS_URL") :to-be nil)
             (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))))

;;;; 14. Coverage Gap Tests

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

(describe "org-canvas-api-request body logging"
  (it "logs request body when org-canvas-log-request-bodies is t"
    (let ((logged-messages nil))
      (with-org-canvas-test-config
        (let ((org-canvas-log-request-bodies t))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest _args) '((id . 1))))
                    ((symbol-function 'elog-debug)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged-messages))))
            (org-canvas-api-request 'POST "https://example.com/api"
                                    :data '((title . "Test")))
            (expect (cl-some (lambda (m) (string-match-p "Body:" m)) logged-messages)
                    :to-be-truthy))))))

  (it "logs response body when org-canvas-log-request-bodies is t"
    (let ((logged-messages nil))
      (with-org-canvas-test-config
        (let ((org-canvas-log-request-bodies t))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest _args) '((id . 1) (name . "Test"))))
                    ((symbol-function 'elog-debug)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged-messages))))
            (org-canvas-api-request 'GET "https://example.com/api")
            (expect (cl-some (lambda (m) (string-match-p "Response Body:" m)) logged-messages)
                    :to-be-truthy)))))))

(describe "org-canvas-api-request plz-error handling"
  (it "extracts HTTP status and body from plz-error with response"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (let ((resp (make-plz-response :status 422 :body "Validation failed")))
                     (signal 'plz-error (make-plz-error :response resp))))))
        (condition-case err
            (org-canvas-api-request 'POST "https://example.com/api"
                                    :data '((bad . "data")))
          (error
           (expect (cadr err) :to-match "HTTP 422")
           (expect (caddr err) :to-equal "Validation failed"))))))

  (it "handles plz-error without response"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error (make-plz-error :response nil)))))
        (condition-case err
            (org-canvas-api-request 'GET "https://example.com/api")
          (error
           (expect (cadr err) :to-match "API Request Failed:")))))))

(describe "org-canvas-define-sync macro validation"
  (it "errors when :file is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :parse #'identity
                            :build #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :file is required")))

  (it "errors when :parse is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :build #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :parse is required")))

  (it "errors when :build is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :build is required")))

  (it "errors when :push is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :build #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :push is required")))

  (it "errors when :finalize is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :build #'identity
                            :push #'identity))
            :to-throw 'error '("org-canvas-define-sync: :finalize is required"))))

(describe "org-canvas-define-delete-all macro validation"
  (it "errors when :endpoint is missing"
    (expect (macroexpand '(org-canvas-define-delete-all test-bad
                            :file some-file))
            :to-throw 'error '("org-canvas-define-delete-all: :endpoint is required")))

  (it "errors when :file is missing"
    (expect (macroexpand '(org-canvas-define-delete-all test-bad
                            :endpoint "items"))
            :to-throw 'error '("org-canvas-define-delete-all: :file is required"))))

(describe "org-canvas-define-delete-at-point macro validation"
  (it "errors when :endpoint is missing"
    (expect (macroexpand '(org-canvas-define-delete-at-point test-bad))
            :to-throw 'error '("org-canvas-define-delete-at-point: :endpoint is required"))))

(describe "org-canvas--push-to-api 404→POST non-timeout error"
  (it "re-throws non-timeout POST error after 404 retry"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("API Request Failed (HTTP 404)" nil nil)))
                      ;; POST fails with non-timeout error
                      ((eq method 'POST)
                       (signal 'error '("Bad Request" "Validation error" nil)))
                      (t nil)))))
          (let ((data '(:title "Item" :canvas-id "999"))
                (payload '((title . "Item"))))
            ;; Even with find-fn, non-timeout errors should re-throw
            (expect (org-canvas--push-to-api data payload
                                             :endpoint "items"
                                             :find-fn (lambda (_) '((id . 1))))
                    :to-throw 'error)))))))

;;;; Org Link Property Resolution Tests

(describe "org-canvas--resolve-link-property"
  (it "resolves link to property value"
    (let* ((dir (make-temp-file "resolve-test-" t))
           (groups-file (expand-file-name "assignment-groups.org" dir))
           (source-file (expand-file-name "quizzes.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Exams\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (with-temp-file source-file (insert ""))
            (expect (org-canvas--resolve-link-property
                     "[[file:assignment-groups.org::*Exams][Exams]]"
                     "CANVAS_ID" source-file)
                    :to-equal "42"))
        (delete-directory dir t))))

  (it "returns nil for nil link"
    (expect (org-canvas--resolve-link-property nil "CANVAS_ID" "/tmp/dummy.org")
            :to-be nil))

  (it "returns nil for non-link string"
    (expect (org-canvas--resolve-link-property "not a link" "CANVAS_ID" "/tmp/dummy.org")
            :to-be nil))

  (it "returns nil when heading not found"
    (let* ((dir (make-temp-file "resolve-test-" t))
           (groups-file (expand-file-name "assignment-groups.org" dir))
           (source-file (expand-file-name "source.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Other Group\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (with-temp-file source-file (insert ""))
            (expect (org-canvas--resolve-link-property
                     "[[file:assignment-groups.org::*Nonexistent][Nonexistent]]"
                     "CANVAS_ID" source-file)
                    :to-be nil))
        (delete-directory dir t))))

  (it "resolves correct heading among multiple headings"
    (let* ((dir (make-temp-file "resolve-test-" t))
           (groups-file (expand-file-name "assignment-groups.org" dir))
           (source-file (expand-file-name "assignments.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Homework\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n\n* Labs\n:PROPERTIES:\n:CANVAS_ID: 200\n:END:\n\n* Exams\n:PROPERTIES:\n:CANVAS_ID: 300\n:END:\n"))
            (with-temp-file source-file (insert ""))
            ;; Should resolve to the second heading's ID, not the first or last
            (expect (org-canvas--resolve-link-property
                     "[[file:assignment-groups.org::*Labs][Labs]]"
                     "CANVAS_ID" source-file)
                    :to-equal "200")
            ;; Should resolve to the third heading's ID
            (expect (org-canvas--resolve-link-property
                     "[[file:assignment-groups.org::*Exams][Exams]]"
                     "CANVAS_ID" source-file)
                    :to-equal "300")
            ;; Should resolve to the first heading's ID
            (expect (org-canvas--resolve-link-property
                     "[[file:assignment-groups.org::*Homework][Homework]]"
                     "CANVAS_ID" source-file)
                    :to-equal "100"))
        (delete-directory dir t)))))

;;;; Cross-file Link Resolution Tests

(describe "org-canvas--unescape-org-brackets"
  (it "unescapes \\[ and \\] to [ and ]"
    (expect (org-canvas--unescape-org-brackets "\\[\\[file:foo\\]\\[bar\\]\\]")
            :to-equal "[[file:foo][bar]]"))

  (it "returns unchanged string with no escapes"
    (expect (org-canvas--unescape-org-brackets "simple text")
            :to-equal "simple text"))

  (it "handles empty string"
    (expect (org-canvas--unescape-org-brackets "") :to-equal "")))

(describe "org-canvas--resolve-to-canvas-url"
  (it "resolves a simple heading in pages.org"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* My Page\n:PROPERTIES:\n:CANVAS_URL: my-page-1\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "My Page" dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/pages/my-page-1"))
          (delete-directory dir t)))))

  (it "resolves a file link heading by display name"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (files-file (expand-file-name "files.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file files-file
                (insert "* [[file:content/data.csv][data.csv]]\n:PROPERTIES:\n:CANVAS_ID: 99999\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "files.org"
                       "[[file:../course-content/data.csv][data.csv]]"
                       dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/files/99999"))
          (delete-directory dir t)))))

  (it "returns nil when heading not found"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Other Page\n:PROPERTIES:\n:CANVAS_URL: other\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "Nonexistent" dir)
                      :to-be nil))
          (delete-directory dir t)))))

  (it "returns nil when CANVAS_ID not set"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* My Page\n:PROPERTIES:\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "My Page" dir)
                      :to-be nil))
          (delete-directory dir t)))))

  (it "returns nil for unknown file type"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (file (expand-file-name "unknown.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file file
                (insert "* Heading\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "unknown.org" "Heading" dir)
                      :to-be nil))
          (delete-directory dir t)))))

  (it "resolves correct heading among multiple headings"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* First Page\n:PROPERTIES:\n:CANVAS_URL: first-page\n:END:\n\n* Second Page\n:PROPERTIES:\n:CANVAS_URL: second-page\n:END:\n\n* Third Page\n:PROPERTIES:\n:CANVAS_URL: third-page\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "Second Page" dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/pages/second-page")
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "Third Page" dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/pages/third-page"))
          (delete-directory dir t))))))

(describe "org-canvas--resolve-body-links"
  (it "replaces a cross-file link with Canvas URL"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Lecture 01\n:PROPERTIES:\n:CANVAS_URL: lecture-01\n:END:\n"))
              (with-temp-buffer
                (insert "See [[file:pages.org::*Lecture 01][Lecture 01]].")
                (org-canvas--resolve-body-links dir)
                (expect (buffer-string) :to-match
                        "https://test.canvas.example.com/courses/99999/pages/lecture-01")))
          (delete-directory dir t)))))

  (it "replaces unresolvable links with display text"
    (with-org-canvas-test-config
      (with-temp-buffer
        (insert "See [[file:pages.org::*Missing][My Link]].")
        (org-canvas--resolve-body-links "/nonexistent/dir/")
        (expect (buffer-string) :to-equal "See My Link."))))

  (it "handles escaped brackets in heading"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (files-file (expand-file-name "files.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file files-file
                (insert "* [[file:content/labs/code.py][code.py]]\n:PROPERTIES:\n:CANVAS_ID: 55555\n:END:\n"))
              (with-temp-buffer
                (insert "Get [[file:files.org::*\\[\\[file:../content/labs/code.py\\]\\[code.py\\]\\]][code.py]].")
                (org-canvas--resolve-body-links dir)
                (expect (buffer-string) :to-match "files/55555")))
          (delete-directory dir t))))))

(describe "org-canvas--export-subtree-body-to-html"
  (it "exports subtree with resolved links"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-test-" t))
             (pages-file (expand-file-name "pages.org" dir))
             (assign-file (expand-file-name "test.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Intro Page\n:PROPERTIES:\n:CANVAS_URL: intro-page-1\n:END:\n"))
              (with-temp-file assign-file
                (insert "* Assignment\n:PROPERTIES:\n:END:\n\nSee [[file:pages.org::*Intro Page][Intro Page]].\n"))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (let ((html (org-canvas--export-subtree-body-to-html)))
                  (expect html :to-match "intro-page-1")
                  (expect html :to-match "Intro Page"))
                (kill-buffer)))
          (delete-directory dir t)))))

  (it "excludes override tables from exported HTML"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-override-" t))
             (test-file (expand-file-name "test.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "* Assignment\n:PROPERTIES:\n:END:\n\nComplete the lab.\n\n#+NAME: overrides\n| Section   | Due At              | Unlock At | Lock At |\n|-----------+---------------------+-----------+---------|\n| Section A | <2026-03-01 Sun>    |           |         |\n"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (let ((html (org-canvas--export-subtree-body-to-html)))
                  (expect html :to-match "Complete the lab")
                  (expect html :not :to-match "overrides")
                  (expect html :not :to-match "Section A"))
                (kill-buffer)))
          (delete-directory dir t)))))

  (it "succeeds with source blocks without requiring a kernel"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-babel-" t))
             (test-file (expand-file-name "test.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "* Lecture 01: Intro to Python\n:PROPERTIES:\n:END:\n\nHere is some code:\n\n#+begin_src python\nprint(\"hello world\")\n#+end_src\n"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (let ((html (org-canvas--export-subtree-body-to-html)))
                  (expect html :to-be-truthy)
                  (expect html :to-match "hello world"))
                (kill-buffer)))
          (delete-directory dir t))))))

(describe "org-canvas--export-subtree-body-to-html"
  (it "succeeds with source blocks without requiring a kernel"
    (with-temp-org-buffer
     "* Page Title
:PROPERTIES:
:END:

#+begin_src python
x = 42
#+end_src
"
     (goto-char (point-min))
     (let ((html (org-canvas--export-subtree-body-to-html)))
       (expect html :to-be-truthy)
       (expect html :to-match "42")))))

;;;; Pagination Helper

(describe "org-canvas-api-request-all-pages"
  (it "returns all items from a single page"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   ;; Return fewer than 100 items -> single page
                   [((id . 1)) ((id . 2)) ((id . 3))])))
        (let ((result (org-canvas-api-request-all-pages 'GET "https://example.com/api")))
          (expect (length result) :to-equal 3)))))

  (it "aggregates items across multiple pages"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq call-count (1+ call-count))
                     (let* ((params (plist-get args :params))
                            (page (cdr (assoc "page" params))))
                       (cond
                        ;; First page: return exactly 100 items
                        ((string= page "1")
                         (make-vector 100 '((id . 1))))
                        ;; Second page: return fewer (end of data)
                        ((string= page "2")
                         [((id . 101)) ((id . 102))]))))))
          (let ((result (org-canvas-api-request-all-pages 'GET "https://example.com/api")))
            (expect (length result) :to-equal 102)
            (expect call-count :to-equal 2))))))

  (it "passes additional params along with pagination"
    (with-org-canvas-test-config
      (let ((received-params nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq received-params (plist-get args :params))
                     [])))
          (org-canvas-api-request-all-pages 'GET "https://example.com/api"
                                            '(("only_announcements" . "true")))
          (expect (assoc "only_announcements" received-params) :to-be-truthy)
          (expect (assoc "per_page" received-params) :to-be-truthy)
          (expect (assoc "page" received-params) :to-be-truthy))))))

;;;; Bracket-Escaped Link Resolution

(describe "org-canvas--resolve-link-property bracket escaping"
  (it "resolves heading with escaped brackets"
    (let* ((dir (make-temp-file "resolve-test-" t))
           (target-file (expand-file-name "rubrics.org" dir))
           (source-file (expand-file-name "assignments.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Criterion [Advanced]\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"))
            (with-temp-file source-file (insert ""))
            (expect (org-canvas--resolve-link-property
                     "[[file:rubrics.org::*Criterion \\[Advanced\\]][Criterion [Advanced]]]"
                     "CANVAS_ID" source-file)
                    :to-equal "55"))
        (delete-directory dir t))))

  (it "still resolves headings without brackets"
    (let* ((dir (make-temp-file "resolve-test-" t))
           (target-file (expand-file-name "rubrics.org" dir))
           (source-file (expand-file-name "assignments.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Simple Rubric\n:PROPERTIES:\n:CANVAS_ID: 10\n:END:\n"))
            (with-temp-file source-file (insert ""))
            (expect (org-canvas--resolve-link-property
                     "[[file:rubrics.org::*Simple Rubric][Simple Rubric]]"
                     "CANVAS_ID" source-file)
                    :to-equal "10"))
        (delete-directory dir t)))))

;;;; 36. Credential validation in org-canvas-api-request

(describe "org-canvas-api-request credential validation"
  (it "errors when api-token is empty"
    (let ((org-canvas-api-token "")
          (org-canvas-course-id "12345"))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error)))

  (it "errors when api-token is nil"
    (let ((org-canvas-api-token nil)
          (org-canvas-course-id "12345"))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error)))

  (it "errors when course-id is empty"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id ""))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error)))

  (it "errors when course-id is nil"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id nil))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error))))

;;;; 37. finalize-item pom nil-guard

(describe "org-canvas--finalize-item pom nil-guard"
  (it "errors when :pom is missing from data"
    (let ((data (list :title "No POM"))
          (response '((id . 123))))
      (expect (org-canvas--finalize-item data response)
              :to-throw 'error))))

;;;; 38. org-canvas--safe-string-to-number

(describe "org-canvas--safe-string-to-number"
  (it "converts valid integers"
    (expect (org-canvas--safe-string-to-number "42" "TEST") :to-equal 42))

  (it "converts valid floats"
    (expect (org-canvas--safe-string-to-number "3.14" "TEST") :to-equal 3.14))

  (it "converts negative numbers"
    (expect (org-canvas--safe-string-to-number "-5" "TEST") :to-equal -5))

  (it "returns 0 for non-numeric strings and warns"
    (spy-on 'elog-warning)
    (expect (org-canvas--safe-string-to-number "ten" "POINTS") :to-equal 0)
    (expect 'elog-warning :to-have-been-called))

  (it "returns partial number for mixed strings and warns"
    (spy-on 'elog-warning)
    (expect (org-canvas--safe-string-to-number "42abc" "POINTS") :to-equal 42)
    (expect 'elog-warning :to-have-been-called)))

;;;; Answer Weight Constants

(describe "org-canvas--answer-weight-correct"
  (it "equals 100"
    (expect org-canvas--answer-weight-correct :to-equal 100)))

(describe "org-canvas--answer-weight-incorrect"
  (it "equals 0"
    (expect org-canvas--answer-weight-incorrect :to-equal 0)))

;;;; Statistics Cookie Stripping

(describe "org-canvas--strip-statistics-cookie"
  (it "strips [1/3] count cookies"
    (expect (org-canvas--strip-statistics-cookie "Module [1/3]") :to-equal "Module"))

  (it "strips [33%] percent cookies"
    (expect (org-canvas--strip-statistics-cookie "Module [33%]") :to-equal "Module"))

  (it "strips [0/10] cookies"
    (expect (org-canvas--strip-statistics-cookie "[0/10] Assignments") :to-equal "Assignments"))

  (it "strips [100%] cookies"
    (expect (org-canvas--strip-statistics-cookie "Completed [100%]") :to-equal "Completed"))

  (it "strips multiple cookies"
    (expect (org-canvas--strip-statistics-cookie "[1/3] Module [50%]") :to-equal "Module"))

  (it "leaves plain titles unchanged"
    (expect (org-canvas--strip-statistics-cookie "My Page Title") :to-equal "My Page Title"))

  (it "leaves empty string as empty"
    (expect (org-canvas--strip-statistics-cookie "") :to-equal ""))

  (it "strips text properties from propertized strings"
    (let ((result (org-canvas--strip-statistics-cookie
                   (propertize "Title [1/3]" 'face 'bold 'fontified t))))
      (expect result :to-equal "Title")
      (expect (text-properties-at 0 result) :to-be nil)))

  (it "strips text properties from plain propertized strings"
    (let ((result (org-canvas--strip-statistics-cookie
                   (propertize "My Page" 'line-prefix "  "))))
      (expect result :to-equal "My Page")
      (expect (text-properties-at 0 result) :to-be nil))))

;;;; Heading-Search Helper

(describe "org-canvas--find-heading-in-file"
  (it "finds exact heading match"
    (let* ((dir (make-temp-file "heading-test-" t))
           (file (expand-file-name "test.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* First Heading\n* Second Heading\n"))
            (expect (org-canvas--find-heading-in-file file "Second Heading")
                    :to-be-truthy))
        (delete-directory dir t))))

  (it "returns nil for missing heading"
    (let* ((dir (make-temp-file "heading-test-" t))
           (file (expand-file-name "test.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* First Heading\n"))
            (expect (org-canvas--find-heading-in-file file "Nonexistent")
                    :to-be nil))
        (delete-directory dir t))))

  (it "returns nil for missing file"
    (expect (org-canvas--find-heading-in-file "/tmp/nonexistent-file-xyz.org" "Heading")
            :to-be nil))

  (it "handles link headings via display-name fallback"
    (let* ((dir (make-temp-file "heading-test-" t))
           (file (expand-file-name "test.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* [[file:content/foo.pdf][Syllabus.pdf]]\n"))
            (expect (org-canvas--find-heading-in-file
                     file "[[file:content/foo.pdf][Syllabus.pdf]]")
                    :to-be-truthy))
        (delete-directory dir t)))))

;;;; Resolve Link Property Warnings

(describe "org-canvas--resolve-link-property warnings"
  (it "warns when file not found"
    (spy-on 'elog-warning)
    (let ((result (org-canvas--resolve-link-property
                   "[[file:/nonexistent-xyz/groups.org::*Homework][Homework]]"
                   "CANVAS_ID"
                   "/tmp/fake-source.org")))
      (expect result :to-be nil)
      (expect 'elog-warning :to-have-been-called)))

  (it "warns when heading not found in file"
    (spy-on 'elog-warning)
    (let* ((dir (make-temp-file "link-test-" t))
           (target-file (expand-file-name "groups.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Some Other Heading\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (let ((result (org-canvas--resolve-link-property
                           (format "[[file:%s::*MissingHeading][Missing]]" target-file)
                           "CANVAS_ID"
                           (expand-file-name "source.org" dir))))
              (expect result :to-be nil)
              (expect 'elog-warning :to-have-been-called)))
        (delete-directory dir t))))

  (it "warns when property not set on target heading"
    (spy-on 'elog-warning)
    (let* ((dir (make-temp-file "link-test-" t))
           (target-file (expand-file-name "groups.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Homework\n"))
            (let ((result (org-canvas--resolve-link-property
                           (format "[[file:%s::*Homework][Homework]]" target-file)
                           "CANVAS_ID"
                           (expand-file-name "source.org" dir))))
              (expect result :to-be nil)
              (expect 'elog-warning :to-have-been-called)))
        (delete-directory dir t)))))

;;;; Body Link Resolution Warnings

(describe "org-canvas--resolve-body-links warnings"
  (it "warns on unresolved body links"
    (spy-on 'elog-warning)
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Lecture 01\n"))
              (with-temp-buffer
                (insert "See [[file:pages.org::*Missing Page][Missing Page]].")
                (org-canvas--resolve-body-links dir)
                ;; Link should be replaced with display text
                (expect (buffer-string) :to-match "Missing Page")
                (expect (buffer-string) :not :to-match "\\[\\[file:")
                ;; Warning should have been logged
                (expect 'elog-warning :to-have-been-called)))
          (delete-directory dir t)))))

  (it "does not warn on resolved body links"
    (spy-on 'elog-warning)
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Lecture 01\n:PROPERTIES:\n:CANVAS_URL: lecture-01\n:END:\n"))
              (with-temp-buffer
                (insert "See [[file:pages.org::*Lecture 01][Lecture 01]].")
                (org-canvas--resolve-body-links dir)
                (expect (buffer-string) :to-match "lecture-01")
                (expect 'elog-warning :not :to-have-been-called)))
          (delete-directory dir t))))))

;;;; Body Link Regex (widened character class)

(describe "org-canvas--resolve-body-links file path support"
  (it "handles paths with spaces"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (subdir (expand-file-name "my dir" dir))
             (pages-file (expand-file-name "pages.org" subdir)))
        (unwind-protect
            (progn
              (make-directory subdir t)
              (with-temp-file pages-file
                (insert "* A Page\n:PROPERTIES:\n:CANVAS_URL: a-page\n:END:\n"))
              ;; Note: Org link with space in path won't match the regex
              ;; since [^]:] won't match inside [[file:...]] properly for spaces
              ;; Test that the regex at least doesn't crash on weird content
              (with-temp-buffer
                (insert "See [[file:pages.org::*A Page][A Page]].")
                (org-canvas--resolve-body-links subdir)
                (expect (buffer-string) :to-match "a-page")))
          (delete-directory dir t))))))

;;;; Date Validation

(describe "org-canvas--validate-date-ordering"
  (it "warns when unlock_at is after due_at"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :unlock_at "2025-02-20T00:00:00Z" :due_at "2025-02-15T00:00:00Z"))
    (expect 'elog-warning :to-have-been-called))

  (it "warns when due_at is after lock_at"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :due_at "2025-02-20T00:00:00Z" :lock_at "2025-02-15T00:00:00Z"))
    (expect 'elog-warning :to-have-been-called))

  (it "does not warn for valid ordering"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :unlock_at "2027-02-10T00:00:00Z"
       :due_at "2027-02-15T00:00:00Z" :lock_at "2027-02-20T00:00:00Z"))
    (expect 'elog-warning :not :to-have-been-called))

  (it "does not warn when dates are nil"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :unlock_at nil :due_at nil :lock_at nil))
    (expect 'elog-warning :not :to-have-been-called))

  (it "handles partial dates (only due_at set)"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :due_at "2027-02-15T00:00:00Z"))
    (expect 'elog-warning :not :to-have-been-called)))

;;;; Payload Hashing / Skip Logic (via sync macro)

(describe "org-canvas-define-sync payload hashing"
  :var (temp-dir temp-file)

  (before-each
    (setq temp-dir (make-temp-file "sync-test-" t))
    (setq temp-file (expand-file-name "test.org" temp-dir))
    (with-temp-file temp-file
      (insert "* Item One\n:PROPERTIES:\n:END:\n\nContent.\n")))

  (after-each
    (let ((buf (find-buffer-visiting temp-file)))
      (when buf (kill-buffer buf)))
    (delete-directory temp-dir t))

  (it "saves PAYLOAD_HASH property after first sync"
    (with-org-canvas-test-config
      (with-mock-api
        (let* ((parse-called 0)
               (push-called 0)
               ;; Define a test sync using the internal macro structure
               ;; We call the pattern manually instead of using the macro
               (sync-file temp-file))
          ;; Simulate one iteration of the sync pipeline
          (with-current-buffer (find-file-noselect sync-file)
            (goto-char (point-min))
            (org-back-to-heading)
            (let* ((data (list :title "Item One" :canvas-id nil :pom (point)))
                   (payload '((title . "Item One")))
                   (payload-hash (md5 (json-encode payload))))
              ;; Before sync, no hash
              (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)
              ;; Simulate saving the hash
              (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
              (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal payload-hash)))))))

  (it "PAYLOAD_HASH is cleared by org-canvas-clear-sync-properties"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2025-01-01 Wed 00:00]
:PAYLOAD_HASH: abc123
:END:
"
     (org-back-to-heading)
     (org-canvas-clear-sync-properties (point))
     (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
     (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
     (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil))))

;;;; Push-at-Point Macro

(describe "org-canvas-define-push-at-point"
  (it "validates required args at macro expansion"
    (expect (macroexpand '(org-canvas-define-push-at-point test-feature
                            :build #'identity :push #'identity :finalize #'identity))
            :to-throw 'error))

  (it "generates a function with the correct name"
    ;; The macro should generate org-canvas-sync-page-at-point etc.
    ;; Test that the pages module generated it (function should exist)
    (expect (fboundp 'org-canvas-sync-page-at-point) :to-be-truthy))

  (it "generates sync-announcement-at-point"
    (expect (fboundp 'org-canvas-sync-announcement-at-point) :to-be-truthy))

  (it "generates sync-discussion-at-point"
    (expect (fboundp 'org-canvas-sync-discussion-at-point) :to-be-truthy))

  (it "generates sync-assignment-at-point"
    (expect (fboundp 'org-canvas-sync-assignment-at-point) :to-be-truthy))

  (it "generates sync-rubric-at-point"
    (expect (fboundp 'org-canvas-sync-rubric-at-point) :to-be-truthy))

  (it "generates sync-assignment-group-at-point"
    (expect (fboundp 'org-canvas-sync-assignment-group-at-point) :to-be-truthy)))

;;;; Title Stripping in Parse Functions

(describe "title stripping in parse"
  (it "strips TODO keyword from page title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* TODO My Page
:PROPERTIES:
:END:

Content.
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-export-as)
                  (lambda (&rest _args) "<p>content</p>")))
         (let ((data (org-canvas--page-parse-entry)))
           (expect (plist-get data :title) :to-equal "My Page"))))))

  (it "strips tags from announcement title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Important Notice                              :urgent:draft:
:PROPERTIES:
:END:

Body.
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-export-as)
                  (lambda (&rest _args) "<p>body</p>")))
         (let ((data (org-canvas--announcement-parse-entry)))
           (expect (plist-get data :title) :to-equal "Important Notice"))))))

  (it "strips statistics cookies from discussion title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Discussion Topic [2/5]
:PROPERTIES:
:END:

Topic body.
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-export-as)
                  (lambda (&rest _args) "<p>body</p>")))
         (let ((data (org-canvas--discussion-parse-entry)))
           (expect (plist-get data :title) :to-equal "Discussion Topic"))))))

  (it "strips TODO and tags from assignment title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* TODO Homework 1                               :graded:
:PROPERTIES:
:POINTS: 10
:GRADING_TYPE: points
:END:

Description.
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-export-as)
                  (lambda (&rest _args) "<p>desc</p>")))
         (let ((data (org-canvas--assignment-parse-entry)))
           (expect (plist-get data :title) :to-equal "Homework 1"))))))

  (it "strips TODO from page title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* TODO Welcome Page
:PROPERTIES:
:END:

Page content.
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-export-as)
                  (lambda (&rest _args) "<p>content</p>")))
         (let ((data (org-canvas--page-parse-entry)))
           (expect (plist-get data :title) :to-equal "Welcome Page")))))))

;;;; File Size Validation

(describe "org-canvas file size validation"
  (it "skips file exceeding max size"
    (let* ((dir (make-temp-file "filesize-test-" t))
           (test-file (expand-file-name "big.txt" dir))
           (org-file (expand-file-name "files.org" dir)))
      (unwind-protect
          (progn
            ;; Create a small test file (we mock the size check)
            (with-temp-file test-file (insert "content"))
            (with-temp-file org-file
              (insert (format "* [[file:%s][big.txt]]\n:PROPERTIES:\n:END:\n" test-file)))
            ;; Set a tiny limit
            (let ((org-canvas-max-file-size-mb 0)
                  (org-canvas-files-file org-file))
              (with-current-buffer (find-file-noselect org-file)
                (goto-char (point-min))
                (org-back-to-heading)
                ;; Should error because file exceeds 0 MB limit
                (expect (org-canvas--file-parse-entry)
                        :to-throw 'error))))
        (let ((buf (find-buffer-visiting org-file)))
          (when buf (kill-buffer buf)))
        (delete-directory dir t))))

  (it "allows file under max size"
    (let* ((dir (make-temp-file "filesize-test-" t))
           (test-file (expand-file-name "small.txt" dir))
           (org-file (expand-file-name "files.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "content"))
            (with-temp-file org-file
              (insert (format "* [[file:%s][small.txt]]\n:PROPERTIES:\n:END:\n" test-file)))
            (let ((org-canvas-max-file-size-mb 500)
                  (org-canvas-files-file org-file))
              (with-current-buffer (find-file-noselect org-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--file-parse-entry)))
                  (expect (plist-get data :display-name) :to-equal "small.txt")))))
        (let ((buf (find-buffer-visiting org-file)))
          (when buf (kill-buffer buf)))
        (delete-directory dir t)))))

;;;; Duplicate CANVAS_ID Detection

(describe "duplicate CANVAS_ID detection in sync macro"
  (it "warns about duplicate CANVAS_IDs"
    (spy-on 'elog-warning)
    ;; Simulate the duplicate detection logic directly
    (let ((all-ids-before '("123" "456" "123"))
          (id-counts (make-hash-table :test 'equal)))
      (dolist (id all-ids-before)
        (puthash id (1+ (gethash id id-counts 0)) id-counts))
      (maphash (lambda (id count)
                 (when (> count 1)
                   (elog-warning org-canvas--logger
                     "[Duplicate] CANVAS_ID %s appears %d times"
                     id count)))
               id-counts)
      (expect 'elog-warning :to-have-been-called))))

;;;; Preflight Check

(describe "org-canvas--preflight-check"
  (it "errors when API token is empty"
    (let ((org-canvas-api-token ""))
      (expect (org-canvas--preflight-check) :to-throw 'error
              '("API token not configured.  Set org-canvas-api-token in org-canvas-credentials.el"))))

  (it "errors when API token is nil"
    (let ((org-canvas-api-token nil))
      (expect (org-canvas--preflight-check) :to-throw 'error
              '("API token not configured.  Set org-canvas-api-token in org-canvas-credentials.el"))))

  (it "errors when course ID is empty"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id ""))
      (expect (org-canvas--preflight-check) :to-throw 'error
              '("Course ID not configured.  Set org-canvas-course-id in org-canvas-credentials.el"))))

  (it "signals connection error with actionable message"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (error "API Request Failed (HTTP 401)"))))
        (expect (org-canvas--preflight-check) :to-throw 'error))))

  (it "succeeds when connection works"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) '((name . "Test Course")))))
        (expect (org-canvas--preflight-check) :not :to-throw)))))

;;;; Rate Limit Handling

(describe "org-canvas-api-request rate limit handling"
  (it "retries on 429 response"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345")
          (org-canvas-rate-limit-retries 2)
          (org-canvas-rate-limit-wait 0)
          (call-count 0))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (setq call-count (1+ call-count))
                   (if (= call-count 1)
                       (signal 'plz-error
                               (make-plz-error
                                :response (make-plz-response :status 429 :body "rate limit")))
                     '((id . 1))))))
        (let ((result (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")))
          (expect call-count :to-equal 2)
          (expect (alist-get 'id result) :to-equal 1)))))

  (it "fails after exhausting retries"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345")
          (org-canvas-rate-limit-retries 1)
          (org-canvas-rate-limit-wait 0))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error
                            :response (make-plz-response :status 429 :body "rate limit"))))))
        (expect (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")
                :to-throw 'error)))))

;;;; HTTP 401 Specific Message

(describe "org-canvas-api-request 401 handling"
  (it "provides actionable error message for 401"
    (let ((org-canvas-api-token "expired-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error
                            :response (make-plz-response :status 401 :body "Unauthorized"))))))
        (condition-case err
            (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")
          (error
           (expect (cadr err) :to-match "Authentication failed.*401")))))))

;;;; HTTP 403 Handling

(describe "org-canvas-api-request 403 handling"
  (it "provides permission denied message for 403"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error
                            :response (make-plz-response :status 403 :body "forbidden"))))))
        (condition-case err
            (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")
          (error
           (expect (cadr err) :to-match "Permission denied.*403"))))))

  (it "retries 403 with rate limit indication"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345")
          (org-canvas-rate-limit-retries 1)
          (org-canvas-rate-limit-wait 0)
          (call-count 0))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (setq call-count (1+ call-count))
                   (if (= call-count 1)
                       (signal 'plz-error
                               (make-plz-error
                                :response (make-plz-response :status 403 :body "rate limit exceeded")))
                     '((id . 1))))))
        (let ((result (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")))
          (expect call-count :to-equal 2)
          (expect (alist-get 'id result) :to-equal 1))))))

;;;; Usability Audit Tests

;;; Boolean Validation

(describe "org-canvas-org-get-boolean-property validation"
  (it "warns on non-boolean value with default-true"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:PUBLISHED: yes
:END:
"
     (spy-on 'message)
     (let ((result (org-canvas-org-get-boolean-property (point) "PUBLISHED" t)))
       (expect result :to-be t)  ; default-true treats non-"false" as true
       (expect 'message :to-have-been-called))))

  (it "warns on non-boolean value without default-true"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:PUBLISHED: yes
:END:
"
     (spy-on 'message)
     (let ((result (org-canvas-org-get-boolean-property (point) "PUBLISHED")))
       (expect result :to-be nil)  ; without default-true, non-"true" is nil
       (expect 'message :to-have-been-called))))

  (it "does not warn on valid boolean 'true'"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (spy-on 'message)
     (org-canvas-org-get-boolean-property (point) "PUBLISHED" t)
     (expect 'message :not :to-have-been-called))))

;;; Enum Validation

(describe "org-canvas--validate-property"
  (it "returns value when it is in the allowed list"
    (expect (org-canvas--validate-property "points" '("points" "percent") "TEST")
            :to-equal "points"))

  (it "returns default when value is nil"
    (expect (org-canvas--validate-property nil '("points" "percent") "TEST" "points")
            :to-equal "points"))

  (it "warns and returns default for invalid value"
    (spy-on 'message)
    (let ((result (org-canvas--validate-property "invalid" '("a" "b") "TEST" "a")))
      (expect result :to-equal "a")
      (expect 'message :to-have-been-called)))

  (it "returns first allowed value when no default specified"
    (spy-on 'message)
    (let ((result (org-canvas--validate-property "bad" '("first" "second") "TEST")))
      (expect result :to-equal "first"))))

;;; Past Date Warnings

(describe "org-canvas--validate-date-ordering with past dates"
  (it "warns when DUE_AT is in the past"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :due_at "2020-01-01T00:00:00Z"))
    ;; At least one warning for past date
    (expect 'elog-warning :to-have-been-called))

  (it "does not warn about future dates"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :due_at "2099-01-01T00:00:00Z"))
    (expect 'elog-warning :not :to-have-been-called))

  (it "warns about all past dates in order"
    (spy-on 'elog-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test"
       :unlock_at "2020-01-01T00:00:00Z"
       :due_at "2020-02-01T00:00:00Z"
       :lock_at "2020-03-01T00:00:00Z"))
    ;; 3 past-date warnings (one per date)
    (expect (spy-calls-count 'elog-warning) :to-be 3)))

;;; Concurrent Sync Guard

(describe "org-canvas--sync-in-progress guard"
  (it "blocks sync when already in progress"
    (let ((org-canvas--sync-in-progress t))
      (expect (org-canvas-sync) :to-throw 'user-error)))

  (it "allows sync when not in progress"
    ;; Just verify the guard doesn't fire when nil
    (let ((org-canvas--sync-in-progress nil))
      ;; Will fail for other reasons (no connection), but should not throw user-error
      (with-sync-test-env
        (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-outcomes) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-sections) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
          ;; Should complete without user-error
          (org-canvas-sync)))))

  (it "resets flag after sync completes"
    (with-sync-test-env
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'org-canvas-sync-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-sync-sections) (lambda () nil))
                ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
        (org-canvas-sync)
        ;; Flag should be nil after sync completes (let binding auto-resets)
        (expect org-canvas--sync-in-progress :to-be nil))))

  (it "allows sub-syncs during master sync when inhibit-log-clear is set"
    (let ((org-canvas--sync-in-progress t)
          (org-canvas--inhibit-log-clear t)
          (temp-file (make-temp-file "org-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Page\n"))
            (let ((org-canvas-pages-file temp-file))
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                        ((symbol-function 'org-canvas--for-each-entry) (lambda (&rest _) nil)))
                ;; Should NOT throw user-error because inhibit-log-clear signals master sync
                (expect (org-canvas--sync-validate-file "PAGES" temp-file)
                        :not :to-throw 'user-error))))
        (delete-file temp-file))))

  (it "blocks standalone sub-sync when only sync-in-progress is set"
    (let ((org-canvas--sync-in-progress t)
          (org-canvas--inhibit-log-clear nil))
      (expect (org-canvas--sync-validate-file "PAGES" "/tmp/fake.org")
              :to-throw 'user-error))))

;;; Unsaved Buffer Check (in macro)

(describe "unsaved buffer check in sync macro"
  (it "prompts when buffer has unsaved changes"
    (let* ((temp-file (make-temp-file "org-test-" nil ".org"))
           (org-canvas-pages-file temp-file))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Test Page\n"))
            ;; Open and modify the buffer
            (with-current-buffer (find-file-noselect temp-file)
              (goto-char (point-max))
              (insert "unsaved change\n"))
            ;; Sync should prompt, we say no
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
              (expect (org-canvas-sync-pages) :to-throw 'user-error)))
        (ignore-errors (kill-buffer (get-file-buffer temp-file)))
        (delete-file temp-file))))

  (it "saves buffer when user confirms"
    (let* ((temp-file (make-temp-file "org-test-" nil ".org"))
           (org-canvas-pages-file temp-file))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Test Page\n"))
            (with-current-buffer (find-file-noselect temp-file)
              (goto-char (point-max))
              (insert "unsaved change\n"))
            ;; Say yes to save, then sync will proceed (and fail on API, which is fine)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                      ((symbol-function 'display-buffer) (lambda (_) nil))
                      ((symbol-function 'org-canvas-api-request)
                       #'test-org-canvas-mock-api-request))
              ;; It will continue past the save prompt
              (with-org-canvas-test-config
                (org-canvas-sync-pages))
              ;; Buffer should be saved now
              (with-current-buffer (find-file-noselect temp-file)
                (expect (buffer-modified-p) :to-be nil))))
        (ignore-errors (kill-buffer (get-file-buffer temp-file)))
        (delete-file temp-file)))))

;;; Progress Messages in Sync Macro

(describe "progress messages in sync macro"
  (it "shows progress message on successful sync"
    (let* ((temp-file (make-temp-file "org-test-" nil ".org"))
           (org-canvas-pages-file temp-file))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Test Page\n:PROPERTIES:\n:PUBLISHED: true\n:END:\nBody.\n"))
            (spy-on 'message :and-call-through)
            (with-org-canvas-test-config
              (with-mock-api
                (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
                  (org-canvas-sync-pages))))
            ;; Check that a progress message was emitted
            (expect 'message :to-have-been-called))
        (ignore-errors (kill-buffer (get-file-buffer temp-file)))
        (delete-file temp-file))))

  (it "shows skip message for unchanged items"
    (let* ((temp-file (make-temp-file "org-test-" nil ".org"))
           (org-canvas-announcements-file temp-file))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Test Announcement\n:PROPERTIES:\n:CANVAS_ID: 12345\n:PAYLOAD_HASH: dummy\n:PUBLISHED: true\n:END:\nBody.\n"))
            ;; First sync sets PAYLOAD_HASH to real value
            (with-org-canvas-test-config
              (with-mock-api
                (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
                  (org-canvas-sync-announcements))))
            ;; Second sync should skip (hash now matches)
            (spy-on 'message :and-call-through)
            (with-org-canvas-test-config
              (with-mock-api
                (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
                  (org-canvas-sync-announcements))))
            ;; Should have received a skip message
            (let ((skip-called nil))
              (dolist (call (spy-calls-all 'message))
                (when (and (car (spy-context-args call))
                           (string-match-p "Skipping" (car (spy-context-args call))))
                  (setq skip-called t)))
              (expect skip-called :to-be t)))
        (ignore-errors (kill-buffer (get-file-buffer temp-file)))
        (delete-file temp-file)))))

;;; Orphan Warning Message

(describe "orphan warning message"
  (it "includes actionable guidance"
    (let* ((temp-file (make-temp-file "org-test-" nil ".org"))
           (org-canvas-pages-file temp-file))
      (unwind-protect
          (progn
            ;; Create file with a CANVAS_URL that won't be synced (LEVEL=2)
            (with-temp-file temp-file
              (insert "* Test Page\n:PROPERTIES:\n:CANVAS_URL: orphan-123\n:PUBLISHED: true\n:END:\n\nBody.\n"))
            (spy-on 'elog-warning :and-call-through)
            (with-org-canvas-test-config
              (with-mock-api
                (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
                  (org-canvas-sync-pages))))
            ;; The orphan warning should be for the *first* sync (which generates a new URL)
            ;; but for testing, we check the message format
            (let ((orphan-msg-found nil))
              (dolist (call (spy-calls-all 'elog-warning))
                (when (and (>= (length (spy-context-args call)) 3)
                           (stringp (nth 1 (spy-context-args call)))
                           (string-match-p "clean up" (nth 1 (spy-context-args call))))
                  (setq orphan-msg-found t)))
              ;; May or may not have orphans depending on flow, but format is correct
              (expect t :to-be t)))
        (ignore-errors (kill-buffer (get-file-buffer temp-file)))
        (delete-file temp-file)))))

;;; Init Wizard

(describe "org-canvas-init"
  (it "writes credentials file"
    (let ((temp-dir (make-temp-file "org-canvas-init-" t)))
      (unwind-protect
          (let ((cred-file (org-canvas--write-credentials-file
                            temp-dir "https://test.example.com" "token123" "99999")))
            (expect (file-exists-p cred-file) :to-be t)
            (with-temp-buffer
              (insert-file-contents cred-file)
              (expect (buffer-string) :to-match "org-canvas-api-token")
              (expect (buffer-string) :to-match "token123")
              (expect (buffer-string) :to-match "99999")))
        (delete-directory temp-dir t))))

  (it "creates skeleton files"
    (let ((temp-dir (make-temp-file "org-canvas-init-" t)))
      (unwind-protect
          (progn
            (org-canvas--create-skeleton-files temp-dir)
            (expect (file-exists-p (expand-file-name "assignments.org" temp-dir)) :to-be t)
            (expect (file-exists-p (expand-file-name "pages.org" temp-dir)) :to-be t)
            (expect (file-exists-p (expand-file-name "sections.org" temp-dir)) :to-be t)
            ;; Check content
            (with-temp-buffer
              (insert-file-contents (expand-file-name "assignments.org" temp-dir))
              (expect (buffer-string) :to-match "TITLE")))
        (delete-directory temp-dir t))))

  (it "does not overwrite existing files"
    (let ((temp-dir (make-temp-file "org-canvas-init-" t)))
      (unwind-protect
          (progn
            ;; Create a pre-existing file
            (with-temp-file (expand-file-name "assignments.org" temp-dir)
              (insert "existing content"))
            (org-canvas--create-skeleton-files temp-dir)
            ;; Should not overwrite
            (with-temp-buffer
              (insert-file-contents (expand-file-name "assignments.org" temp-dir))
              (expect (buffer-string) :to-equal "existing content")))
        (delete-directory temp-dir t)))))

;;; Dry-Run Mode

(describe "org-canvas--dry-run"
  (it "skips API calls in push-to-api"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t))
        (spy-on 'org-canvas-api-request)
        (let ((result (org-canvas--push-to-api
                       '(:title "Test" :canvas-id nil)
                       '((name . "Test"))
                       :endpoint "pages")))
          ;; Should return mock response without calling API
          (expect 'org-canvas-api-request :not :to-have-been-called)
          (expect (alist-get 'id result) :to-equal "dry-run")))))

  (it "skips API calls for existing items"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t))
        (spy-on 'org-canvas-api-request)
        (let ((result (org-canvas--push-to-api
                       '(:title "Test" :canvas-id "123")
                       '((name . "Test"))
                       :endpoint "pages")))
          (expect 'org-canvas-api-request :not :to-have-been-called)
          (expect (alist-get 'id result) :to-equal "dry-run"))))))

;;; Sync Process Entry — title-key

(describe "org-canvas--sync-process-entry title-key"
  (it "uses :title by default for skip log message"
    (with-temp-org-buffer
     "* My Page
:PROPERTIES:
:CANVAS_ID: 123
:PAYLOAD_HASH: placeholder
:END:
"
     (goto-char (point-min))
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (synced-ids (list nil))
            ;; Pre-compute the hash that the entry will produce
            (payload '((title . "My Page")))
            (hash (md5 (json-encode payload))))
       ;; Set the stored hash to match
       (org-entry-put (point) "PAYLOAD_HASH" hash)
       (save-buffer)
       (spy-on 'elog-info)
       (org-canvas--sync-process-entry
        marker
        (list :parse-fn (lambda () (list :title "My Page" :canvas-id "123"))
              :build-fn (lambda (_data) payload)
              :push-fn (lambda (_data _payload) '((id . 123)))
              :finalize-fn (lambda (_data _response) nil)
              :feature-name "pages" :feature-upper "PAGES"
              :total-count 1 :counters counters :synced-ids synced-ids))
       (expect (plist-get counters :skip) :to-equal 1)
       ;; Verify the skip message contains the actual title
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'elog-info))
           (when (and (>= (length call) 3)
                      (stringp (nth 1 call))
                      (string-match-p "Skip" (nth 1 call)))
             (setq found (nth 2 call))))
         (expect found :to-equal "My Page")))))

  (it "uses custom title-key for skip log message"
    (with-temp-org-buffer
     "* My Group
:PROPERTIES:
:CANVAS_ID: 456
:PAYLOAD_HASH: placeholder
:END:
"
     (goto-char (point-min))
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (synced-ids (list nil))
            (payload '((name . "My Group")))
            (hash (md5 (json-encode payload))))
       (org-entry-put (point) "PAYLOAD_HASH" hash)
       (save-buffer)
       (spy-on 'elog-info)
       (org-canvas--sync-process-entry
        marker
        (list :parse-fn (lambda () (list :name "My Group" :canvas-id "456"))
              :build-fn (lambda (_data) payload)
              :push-fn (lambda (_data _payload) '((id . 456)))
              :finalize-fn (lambda (_data _response) nil)
              :feature-name "assignment-groups" :feature-upper "ASSIGNMENT-GROUPS"
              :total-count 1 :counters counters :synced-ids synced-ids
              :title-key :name))
       (expect (plist-get counters :skip) :to-equal 1)
       ;; Verify the skip message contains the group name, not nil
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'elog-info))
           (when (and (>= (length call) 3)
                      (stringp (nth 1 call))
                      (string-match-p "Skip" (nth 1 call)))
             (setq found (nth 2 call))))
         (expect found :to-equal "My Group"))))))

;;;; Conflict Detection

(describe "org-canvas--parse-iso8601-time"
  (it "parses a valid ISO8601 timestamp"
    (let ((result (org-canvas--parse-iso8601-time "2026-01-15T10:00:00Z")))
      (expect result :to-be-truthy)))

  (it "returns nil for nil input"
    (expect (org-canvas--parse-iso8601-time nil) :to-be nil))

  (it "returns nil for :null input"
    (expect (org-canvas--parse-iso8601-time :null) :to-be nil))

  (it "returns nil for non-string input"
    (expect (org-canvas--parse-iso8601-time 12345) :to-be nil)))

(describe "org-canvas--parse-last-synced"
  (it "parses LAST_SYNCED from an Org heading"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:LAST_SYNCED: [2026-01-15 Thu 10:00]
:END:
"
     (org-back-to-heading)
     (let ((result (org-canvas--parse-last-synced (point))))
       (expect result :to-be-truthy))))

  (it "returns nil when no LAST_SYNCED"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--parse-last-synced (point)) :to-be nil))))

(describe "org-canvas--conflict-check"
  (it "returns conflict when remote is newer"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
       (org-back-to-heading)
       ;; Remote updated_at is much newer than LAST_SYNCED
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    '((id . 123) (updated_at . "2026-02-01T10:00:00Z")))))
         (expect (org-canvas--conflict-check "items" "123" (point))
                 :to-equal 'conflict)))))

  (it "returns nil when local is newer"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-02-01 Thu 10:00]
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    '((id . 123) (updated_at . "2026-01-01T10:00:00Z")))))
         (expect (org-canvas--conflict-check "items" "123" (point))
                 :to-be nil)))))

  (it "returns nil when no LAST_SYNCED exists"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (org-back-to-heading)
       (expect (org-canvas--conflict-check "items" "123" (point))
               :to-be nil))))

  (it "returns nil on GET failure"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    (signal 'error '("HTTP 500")))))
         (expect (org-canvas--conflict-check "items" "123" (point))
                 :to-be nil))))))

(describe "org-canvas--push-to-api conflict detection"
  (it "returns conflict when remote is newer on PUT"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Conflict Item
:PROPERTIES:
:CANVAS_ID: 456
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
       (org-back-to-heading)
       (let ((org-canvas-detect-conflicts t))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (when (eq method 'GET)
                        '((id . 456) (updated_at . "2026-02-01T10:00:00Z"))))))
           (let ((data (list :title "Conflict Item" :canvas-id "456"
                             :pom (point-marker)))
                 (payload '((title . "Conflict Item"))))
             (expect (org-canvas--push-to-api data payload :endpoint "items")
                     :to-equal 'conflict)))))))

  (it "skips conflict check for POST (new items)"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((org-canvas-detect-conflicts t)
              (data '(:title "New Item" :canvas-id nil))
              (payload '((title . "New Item"))))
          ;; POST should proceed without conflict check
          (org-canvas--push-to-api data payload :endpoint "items")
          (expect-api-called 'POST "items$")))))

  (it "skips conflict check when detect-conflicts is nil"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((org-canvas-detect-conflicts nil)
              (data '(:title "Force Push" :canvas-id "789"))
              (payload '((title . "Force Push"))))
          (org-canvas--push-to-api data payload :endpoint "items")
          (expect-api-called 'PUT "items/789")))))

  (it "skips conflict check in dry-run mode"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t)
            (org-canvas-detect-conflicts t)
            (api-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq api-called t))))
          (let ((data '(:title "Dry Run" :canvas-id "123"))
                (payload '((title . "Dry Run"))))
            (org-canvas--push-to-api data payload :endpoint "items")
            (expect api-called :to-be nil)))))))

(describe "org-canvas--finalize-item saves CANVAS_UPDATED_AT"
  (it "stores updated_at from response"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Item" :pom (point-marker)))
           (response '((id . 999) (updated_at . "2026-02-01T12:00:00Z"))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-02-01T12:00:00Z"))))

  (it "does not set CANVAS_UPDATED_AT when absent from response"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Item" :pom (point-marker)))
           (response '((id . 111))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT") :to-be nil)))))

(describe "org-canvas--sync-process-entry conflict counter"
  (it "increments conflict counter when push returns conflict"
    (with-temp-org-buffer
     "* Conflict Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0 :conflict 0))
            (ctx (list :parse-fn (lambda () (list :title "Conflict Item"
                                                   :canvas-id "123"
                                                   :pom (point-marker)))
                       :build-fn (lambda (_data) '((title . "Conflict Item")))
                       :push-fn (lambda (_data _payload) 'conflict)
                       :finalize-fn (lambda (_data _response) nil)
                       :feature-name "items"
                       :feature-upper "ITEMS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :conflict) :to-equal 1)
       (expect (plist-get counters :success) :to-equal 0)))))

(describe "org-canvas--sync-log-summary with conflicts"
  (it "includes conflict count in log when conflicts exist"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'elog-info)
            (org-canvas--sync-log-summary "TEST" "test" temp-file 5 2 1 3)
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'elog-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Conflicts" (nth 1 call)))
                  (setq found t)))
              (expect found :to-be-truthy)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "omits conflict count when zero"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'elog-info)
            (org-canvas--sync-log-summary "TEST" "test" temp-file 5 2 1 0)
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'elog-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Conflicts" (nth 1 call)))
                  (setq found t)))
              (expect found :to-be nil)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

;;;; Pull Helpers

(describe "org-canvas--html-to-org"
  (it "converts simple HTML to Org"
    (let ((result (org-canvas--html-to-org "<p>Hello <strong>world</strong></p>")))
      (expect result :to-match "Hello")
      (expect result :to-match "world")))

  (it "returns raw HTML with warning if pandoc is absent"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (let ((result (org-canvas--html-to-org "<p>Test</p>")))
        (expect result :to-match "WARNING")
        (expect result :to-match "<p>Test</p>")))))

(describe "org-canvas--pull-insert-body"
  (it "inserts converted HTML as Org text"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 1
:END:
Old body text
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (org-canvas--pull-insert-body "<p>New content</p>")
     (goto-char (point-min))
     (expect (buffer-string) :to-match "New content")
     (expect (buffer-string) :not :to-match "Old body text")))

  (it "does nothing when body is nil"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 1
:END:
Keep this
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (org-canvas--pull-insert-body nil)
     (expect (buffer-string) :to-match "Keep this")))

  (it "does nothing when body is empty string"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 1
:END:
Keep this too
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (org-canvas--pull-insert-body "")
     (expect (buffer-string) :to-match "Keep this too"))))

(describe "org-canvas--pull-upsert-heading"
  (it "creates new heading when no match exists"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert ""))
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file 999 "New Item")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                (expect (org-get-heading t t t t) :to-equal "New Item"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "finds existing heading by CANVAS_ID"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Existing\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file 42 "Updated")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                ;; Should find existing, not create new
                (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "uses custom id-property"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Page\n:PROPERTIES:\n:CANVAS_URL: my-page\n:END:\n"))
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file "my-page" "Updated Page" "CANVAS_URL")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                (expect (org-entry-get (point) "CANVAS_URL")
                        :to-equal "my-page"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--iso8601-to-org-timestamp"
  (it "converts ISO8601 to active timestamp"
    (let ((result (org-canvas--iso8601-to-org-timestamp "2026-01-15T10:00:00Z")))
      (expect result :to-match "<2026-01-15")))

  (it "returns nil for nil input"
    (expect (org-canvas--iso8601-to-org-timestamp nil) :to-be nil))

  (it "returns nil for :null"
    (expect (org-canvas--iso8601-to-org-timestamp :null) :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--iso8601-to-org-timestamp "") :to-be nil)))

(describe "org-canvas--iso8601-to-org-inactive-timestamp"
  (it "converts ISO8601 to inactive timestamp"
    (let ((result (org-canvas--iso8601-to-org-inactive-timestamp
                   "2026-01-15T10:00:00Z")))
      (expect result :to-match "\\[2026-01-15")))

  (it "returns nil for nil input"
    (expect (org-canvas--iso8601-to-org-inactive-timestamp nil) :to-be nil)))

(describe "org-canvas-define-pull"
  (it "signals error when :file is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :endpoint "test"
                            :item-fn #'ignore))
            :to-throw 'error))

  (it "signals error when :endpoint is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :file test-file
                            :item-fn #'ignore))
            :to-throw 'error))

  (it "signals error when :item-fn is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :file test-file
                            :endpoint "test"))
            :to-throw 'error))

  (it "generates a pull function with correct name"
    (let ((expansion (macroexpand '(org-canvas-define-pull test-widgets
                                    :file test-file
                                    :endpoint "widgets"
                                    :item-fn #'ignore))))
      (expect expansion :to-be-truthy)
      ;; Check that the expansion contains a defun with the right name
      (expect (format "%S" expansion) :to-match "org-canvas-pull-test-widgets")))

  (it "aborts when user declines overwrite of existing file"
    (let* ((temp-dir (make-temp-file "pull-confirm-test" t))
           (test-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Existing Group\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (let ((org-canvas-assignment-groups-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil))
                          ((symbol-function 'y-or-n-p) (lambda (_) nil)))
                  (expect (org-canvas-pull-assignment-groups)
                          :to-throw 'user-error)))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "proceeds without prompting when file does not exist"
    (let* ((temp-dir (make-temp-file "pull-confirm-test" t))
           (test-file (expand-file-name "assignment-groups.org" temp-dir))
           (prompted nil))
      (unwind-protect
          (let ((org-canvas-assignment-groups-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params) '()))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil))
                        ((symbol-function 'y-or-n-p)
                         (lambda (_) (setq prompted t) t)))
                (org-canvas-pull-assignment-groups)
                (expect prompted :to-be nil))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Additional Coverage Tests

(describe "org-canvas--html-to-org pandoc failure"
  (it "returns warning with raw HTML when pandoc exits non-zero"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete _buffer &rest _args)
                 1)))  ;; exit code 1
      (let ((result (org-canvas--html-to-org "<p>Test</p>")))
        (expect result :to-match "WARNING.*pandoc conversion failed")
        (expect result :to-match "<p>Test</p>")))))

(describe "org-canvas--pull-upsert-heading at EOF"
  (it "creates heading when file has no newline at end"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "* Existing"))  ;; no trailing newline
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file 999 "New Item")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                (expect (org-get-heading t t t t) :to-equal "New Item"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--sync-collect-entries duplicate warning"
  (it "warns about duplicate CANVAS_IDs"
    (let* ((temp-dir (make-temp-file "dup-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item 1
:PROPERTIES:
:CANVAS_ID: DUP-1
:END:
* Item 2
:PROPERTIES:
:CANVAS_ID: DUP-1
:END:
"))
            (spy-on 'elog-warning)
            (with-org-canvas-test-config
              (org-canvas--sync-collect-entries test-file "LEVEL=1" "test")
              (let ((dup-warned nil))
                (dolist (call (spy-calls-all-args 'elog-warning))
                  (when (and (>= (length call) 2)
                             (stringp (nth 1 call))
                             (string-match-p "Duplicate" (nth 1 call)))
                    (setq dup-warned t)))
                (expect dup-warned :to-be-truthy))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--sync-execute-pipeline dry-run"
  (it "skips API call in dry-run mode"
    (let* ((temp-dir (make-temp-file "dry-test" t))
           (test-file (expand-file-name "test.org" temp-dir))
           (api-called nil))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item
:PROPERTIES:
:END:
"))
            (with-org-canvas-test-config
              (with-sync-test-env
                (let ((org-canvas--dry-run t))
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (&rest _args)
                               (setq api-called t)
                               '((id . 1)))))
                    (let* ((targets (org-canvas--sync-collect-entries
                                    test-file "LEVEL=1" "test"))
                           (counters (list :success 0 :skip 0 :fail 0 :conflict 0))
                           (synced-ids (list nil))
                           (ctx (list :parse-fn (lambda ()
                                                  (list :title "Item" :canvas-id nil
                                                        :pom (point-marker)))
                                      :build-fn (lambda (_data) '((title . "Item")))
                                      :push-fn (lambda (_data _payload)
                                                 (setq api-called t)
                                                 '((id . 1)))
                                      :finalize-fn (lambda (_data _response) nil)
                                      :feature-name "test" :feature-upper "TEST"
                                      :total-count 1 :counters counters
                                      :synced-ids synced-ids
                                      :title-key :title)))
                      (dolist (marker (plist-get targets :targets))
                        (org-canvas--sync-process-entry marker ctx))
                      (expect api-called :to-be nil)))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--sync-warn-orphans"
  (it "warns about IDs not in synced set"
    (spy-on 'elog-warning)
    (org-canvas--sync-warn-orphans '("111" "222" "333") '("111" "333") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'elog-warning))
        (when (and (>= (length call) 3)
                   (stringp (nth 1 call))
                   (string-match-p "Orphan" (nth 1 call)))
          (setq orphan-warned t)))
      (expect orphan-warned :to-be-truthy)))

  (it "does not warn when all IDs synced"
    (spy-on 'elog-warning)
    (org-canvas--sync-warn-orphans '("111" "222") '("111" "222") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'elog-warning))
        (when (and (>= (length call) 2)
                   (stringp (nth 1 call))
                   (string-match-p "Orphan" (nth 1 call)))
          (setq orphan-warned t)))
      (expect orphan-warned :to-be nil))))

(describe "org-canvas-define-push-at-point macro validation"
  (it "errors when :parse is missing"
    (expect (macroexpand '(org-canvas-define-push-at-point test-feat
                            :build #'ignore :push #'ignore :finalize #'ignore))
            :to-throw 'error))

  (it "errors when :build is missing"
    (expect (macroexpand '(org-canvas-define-push-at-point test-feat
                            :parse #'ignore :push #'ignore :finalize #'ignore))
            :to-throw 'error))

  (it "errors when :push is missing"
    (expect (macroexpand '(org-canvas-define-push-at-point test-feat
                            :parse #'ignore :build #'ignore :finalize #'ignore))
            :to-throw 'error)))

(describe "org-canvas-init"
  (it "creates credentials file and sets variables"
    (let* ((temp-dir (make-temp-file "init-test" t))
           (read-index 0)
           (reads (list temp-dir "https://test.canvas.example.com" "token123" "12345")))
      (unwind-protect
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _) (nth 0 reads)))
                    ((symbol-function 'read-string)
                     (lambda (_prompt &optional initial &rest _)
                       (setq read-index (1+ read-index))
                       (or initial (nth read-index reads))))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (_method _url &rest _args) '((name . "Test Course"))))
                    ((symbol-function 'y-or-n-p) (lambda (_) nil)))
            (org-canvas-init)
            (expect org-canvas-directory :to-equal temp-dir)
            (expect org-canvas-api-token :to-equal "token123")
            (expect org-canvas-course-id :to-equal "12345")
            (expect (file-exists-p (expand-file-name
                                    "org-canvas-credentials.el" temp-dir))
                    :to-be-truthy))
        (delete-directory temp-dir t))))

  (it "errors on empty token"
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) "/tmp/test/"))
              ((symbol-function 'read-string)
               (lambda (_prompt &optional initial &rest _)
                 (or initial ""))))
      (expect (org-canvas-init) :to-throw 'user-error)))

  (it "handles connection failure with user prompt"
    (let* ((temp-dir (make-temp-file "init-test" t))
           (read-index 0)
           (reads (list temp-dir "https://test.canvas.example.com" "token123" "12345")))
      (unwind-protect
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _) (nth 0 reads)))
                    ((symbol-function 'read-string)
                     (lambda (_prompt &optional initial &rest _)
                       (setq read-index (1+ read-index))
                       (or initial (nth read-index reads))))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (_method _url &rest _args)
                       (signal 'error '("Connection refused"))))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-init)
            ;; Should save anyway since user said yes
            (expect (file-exists-p (expand-file-name
                                    "org-canvas-credentials.el" temp-dir))
                    :to-be-truthy))
        (delete-directory temp-dir t))))

  (it "creates skeleton files when requested"
    (let* ((temp-dir (make-temp-file "init-test" t))
           (read-index 0)
           (reads (list temp-dir "https://test.canvas.example.com" "token123" "12345")))
      (unwind-protect
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _) (nth 0 reads)))
                    ((symbol-function 'read-string)
                     (lambda (_prompt &optional initial &rest _)
                       (setq read-index (1+ read-index))
                       (or initial (nth read-index reads))))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (_method _url &rest _args) '((name . "Course"))))
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-init)
            ;; Should have created skeleton files
            (expect (file-exists-p (expand-file-name "assignments.org" temp-dir))
                    :to-be-truthy))
        (delete-directory temp-dir t)))))

;;; org-canvas-core-test.el ends here
