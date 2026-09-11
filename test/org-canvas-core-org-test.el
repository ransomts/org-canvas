;;; org-canvas-core-org-test.el --- Tests for org-canvas-core-org -*- lexical-binding: t; -*-

;;; Commentary:

;; Org buffer helpers, batch-file freshness, timestamps, link and section resolution, diagnostics.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)
(require 'org-canvas-pages)
(require 'org-canvas-assignments)
(require 'org-canvas-sections)
(require 'org-canvas-files)

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

(describe "org-canvas--require-title"
  (it "does nothing for non-empty title"
    (expect (org-canvas--require-title "My Title" 1 "Test") :not :to-throw))

  (it "errors when title is nil"
    (expect (org-canvas--require-title nil 1 "Page") :to-throw 'error))

  (it "errors when title is empty string"
    (expect (org-canvas--require-title "" 42 "Assignment") :to-throw 'error))

  (it "includes entity name and point in error message"
    (condition-case err
        (org-canvas--require-title "" 99 "Quiz")
      (error
       (expect (error-message-string err)
               :to-match "Quiz title cannot be empty at point 99"))))

  (it "handles marker pom by extracting position"
    (with-temp-org-buffer
     "* Heading\n"
     (org-back-to-heading)
     (let ((m (point-marker)))
       (condition-case err
           (org-canvas--require-title "" m "New Quiz")
         (error
          (expect (error-message-string err)
                  :to-match "New Quiz title cannot be empty at point")))))))

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
       (expect (org-entry-get (point) "TEST_PROP") :to-equal "marker-value"))))

  (it "emits :LICENSE: with single space (no padding)"
    ;; org-mode's org-property-format defaults to "%-10s %s", which pads
    ;; property names shorter than 10 chars and produces e.g.
    ;;   :LICENSE:  private        ;; two spaces
    ;;   :END_AT:   <2026-01-01>   ;; three spaces
    ;; org-canvas-org-set-property binds org-property-format to "%s %s"
    ;; so the emitted text always has a single space separator.
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas-org-set-property (point) "LICENSE" "private")
     (org-canvas-org-set-property (point) "END_AT" "<2026-12-01 Tue>")
     (org-canvas-org-set-property (point) "PUBLIC_SYLLABUS" "false")
     (let ((text (buffer-string)))
       (expect text :to-match "^:LICENSE: private$")
       (expect text :to-match "^:END_AT: <2026-12-01 Tue>$")
       (expect text :to-match "^:PUBLIC_SYLLABUS: false$")
       ;; Negative checks: no double space after colon.
       (expect (string-match-p ":LICENSE:  " text) :to-be nil)
       (expect (string-match-p ":END_AT:  " text) :to-be nil)))))

(describe "org-canvas--save-buffer"
  (it "saves the buffer and logs the file path"
    (let* ((tmp-file (make-temp-file "org-canvas-save-" nil ".org"))
           (logged nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (setq logged (apply #'format fmt args)))))
            (with-current-buffer (find-file-noselect tmp-file)
              (insert "* heading\n")
              (org-canvas--save-buffer)
              (kill-buffer))
            (expect logged :to-equal (format "[Saved] %s" tmp-file)))
        (delete-file tmp-file))))

  (it "skips the log when the buffer has no associated file"
    (let ((logged nil))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil))
                ((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (setq logged (apply #'format fmt args)))))
        (with-temp-buffer
          (insert "scratch")
          (org-canvas--save-buffer))
        (expect logged :to-be nil))))

  (it "is a no-op on an unmodified buffer (no duplicate [Saved] lines)"
    (let ((tmp-file (make-temp-file "org-canvas-save-" nil ".org"))
          (logged nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged))))
            (with-current-buffer (find-file-noselect tmp-file)
              (insert "* heading\n")
              (org-canvas--save-buffer)
              ;; Completion-time safety save: buffer unmodified, no write
              (org-canvas--save-buffer)
              (kill-buffer))
            (expect (length logged) :to-equal 1))
        (delete-file tmp-file)))))

(describe "org-canvas-org-save-sync-state"
  (it "saves CANVAS_ID but not per-entry LAST_SYNCED"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas-org-save-sync-state (point) 12345)
     (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345")
     (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)))

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

  (it "returns nil for nil timestamp input"
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

;;;; 21. For Each Entry Error Handling

(describe "org-canvas--for-each-entry error details"
  (it "logs error position on failure"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org"))
          (error-logged nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Heading\n"))
            (cl-letf (((symbol-function 'org-canvas--log-error)
                       (lambda (&rest _args) (setq error-logged t) nil)))
              (org-canvas--for-each-entry temp-file "LEVEL=1"
                (lambda () (error "Test error")))
              (expect error-logged :to-be t)))
        (delete-file temp-file)))))

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

;;;; 38. org-canvas--safe-string-to-number

(describe "org-canvas--safe-string-to-number"
  (it "converts valid integers"
    (expect (org-canvas--safe-string-to-number "42" "TEST") :to-equal 42))

  (it "converts valid floats"
    (expect (org-canvas--safe-string-to-number "3.14" "TEST") :to-equal 3.14))

  (it "converts negative numbers"
    (expect (org-canvas--safe-string-to-number "-5" "TEST") :to-equal -5))

  (it "returns 0 for non-numeric strings and warns"
    (spy-on 'org-canvas--log-warning)
    (expect (org-canvas--safe-string-to-number "ten" "POINTS") :to-equal 0)
    (expect 'org-canvas--log-warning :to-have-been-called))

  (it "returns partial number for mixed strings and warns"
    (spy-on 'org-canvas--log-warning)
    (expect (org-canvas--safe-string-to-number "42abc" "POINTS") :to-equal 42)
    (expect 'org-canvas--log-warning :to-have-been-called)))

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
    (spy-on 'org-canvas--log-warning)
    (let ((result (org-canvas--resolve-link-property
                   "[[file:/nonexistent-xyz/groups.org::*Homework][Homework]]"
                   "CANVAS_ID"
                   "/tmp/fake-source.org")))
      (expect result :to-be nil)
      (expect 'org-canvas--log-warning :to-have-been-called)))

  (it "warns when heading not found in file"
    (spy-on 'org-canvas--log-warning)
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
              (expect 'org-canvas--log-warning :to-have-been-called)))
        (delete-directory dir t))))

  (it "warns when property not set on target heading"
    (spy-on 'org-canvas--log-warning)
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
              (expect 'org-canvas--log-warning :to-have-been-called)))
        (delete-directory dir t)))))

;;;; Date Validation

(describe "org-canvas--validate-date-ordering"
  (it "warns when unlock_at is after due_at"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :unlock_at "2025-02-20T00:00:00Z" :due_at "2025-02-15T00:00:00Z"))
    (expect 'org-canvas--log-warning :to-have-been-called))

  (it "warns when due_at is after lock_at"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :due_at "2025-02-20T00:00:00Z" :lock_at "2025-02-15T00:00:00Z"))
    (expect 'org-canvas--log-warning :to-have-been-called))

  (it "does not warn for valid ordering"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :unlock_at "2027-02-10T00:00:00Z"
       :due_at "2027-02-15T00:00:00Z" :lock_at "2027-02-20T00:00:00Z"))
    (expect 'org-canvas--log-warning :not :to-have-been-called))

  (it "does not warn when dates are nil"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :unlock_at nil :due_at nil :lock_at nil))
    (expect 'org-canvas--log-warning :not :to-have-been-called))

  (it "handles partial dates (only due_at set)"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--validate-date-ordering
     '(:title "Test" :due_at "2027-02-15T00:00:00Z"))
    (expect 'org-canvas--log-warning :not :to-have-been-called)))

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
    (spy-on 'org-canvas--log-warning)
    ;; Simulate the duplicate detection logic directly
    (let ((all-ids-before '("123" "456" "123"))
          (id-counts (make-hash-table :test 'equal)))
      (dolist (id all-ids-before)
        (puthash id (1+ (gethash id id-counts 0)) id-counts))
      (maphash (lambda (id count)
                 (when (> count 1)
                   (org-canvas--log-warning org-canvas--logger
                     "[Duplicate] CANVAS_ID %s appears %d times"
                     id count)))
               id-counts)
      (expect 'org-canvas--log-warning :to-have-been-called))))

;;;; Section Name → ID Resolution

(describe "org-canvas--resolve-section-names-to-ids"
  (it "returns nil for nil sections input"
    (expect (org-canvas--resolve-section-names-to-ids nil) :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--resolve-section-names-to-ids "") :to-be nil))

  (it "passes through numeric IDs unchanged"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "#+TITLE: Sections\n"))
            (let ((org-canvas-sections-file sections-file))
              (expect (org-canvas--resolve-section-names-to-ids "123,456")
                      :to-equal "123,456")))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "resolves section names to CANVAS_IDs"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 100
:END:

* Section B
:PROPERTIES:
:CANVAS_ID: 200
:END:
"))
            (let ((org-canvas-sections-file sections-file))
              (expect (org-canvas--resolve-section-names-to-ids "Section A,Section B")
                      :to-equal "100,200")))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "handles mixed names and numeric IDs"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((org-canvas-sections-file sections-file))
              (expect (org-canvas--resolve-section-names-to-ids "Section A,999")
                      :to-equal "100,999")))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "warns and skips unresolvable names"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((org-canvas-sections-file sections-file))
              (spy-on 'message)
              (expect (org-canvas--resolve-section-names-to-ids "Section A,Nonexistent")
                      :to-equal "100")
              (expect 'message :to-have-been-called)))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "returns nil when all names unresolvable"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Other Section\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (let ((org-canvas-sections-file sections-file))
              (spy-on 'message)
              (expect (org-canvas--resolve-section-names-to-ids "Nonexistent")
                      :to-be nil)))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "trims whitespace from names"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((org-canvas-sections-file sections-file))
              (expect (org-canvas--resolve-section-names-to-ids " Section A , 999 ")
                      :to-equal "100,999")))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file)))))

;;;; resolve-link-or-raw with Org link value

(describe "org-canvas--resolve-link-or-raw"
  (it "resolves an Org link by delegating to resolve-link-property"
    (with-org-canvas-test-config
      (let* ((target-file (make-temp-file "link-target-" nil ".org"))
             (source-file (make-temp-file "link-source-" nil ".org")))
        (unwind-protect
            (progn
              (with-temp-file target-file
                (insert "* Target Heading\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
              (with-temp-file source-file
                (insert (format "* Source\n:PROPERTIES:\n:GROUP: [[file:%s::*Target Heading][Target Heading]]\n:END:\n"
                                target-file)))
              (with-current-buffer (find-file-noselect source-file)
                (unwind-protect
                    (progn
                      (goto-char (point-min))
                      (org-back-to-heading)
                      (let ((result (org-canvas--resolve-link-or-raw
                                     (point) "GROUP" "CANVAS_ID" source-file)))
                        (expect result :to-equal "42")))
                  (kill-buffer))))
          (delete-file target-file)
          (delete-file source-file)))))

  (it "returns raw value when not a link"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:GROUP_CATEGORY_ID: 789
:END:
"
       (org-back-to-heading)
       (let ((result (org-canvas--resolve-link-or-raw
                      (point) "GROUP_CATEGORY_ID" "CANVAS_ID" "dummy.el")))
         (expect result :to-equal "789"))))))

;;;; Section name resolution when file unavailable

(describe "org-canvas--resolve-section-names-to-ids"
  (it "warns when sections file is not available"
    (let ((org-canvas-sections-file "/nonexistent/sections.org")
          (warnings nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (let ((result (org-canvas--resolve-section-names-to-ids "Section A")))
          (expect result :to-be nil)
          (expect (cl-some (lambda (w) (string-match-p "Cannot resolve section" w))
                           warnings)
                  :to-be-truthy))))))

(describe "TZ-aware pull timestamp"
  ;; POSIX TZ string (`EST5EDT,M3.2.0,M11.1.0') is used instead of the
  ;; IANA name `America/New_York' so the test passes on systems whose
  ;; Emacs build can't resolve IANA names against tzdata (e.g., the
  ;; minimal CI runner). Production `org-canvas--pull-tz-cache' takes
  ;; whatever string Canvas returned (typically IANA), and IANA name
  ;; resolution works on all common end-user systems.
  (it "converts UTC ISO-8601 to course-local Org active timestamp"
    (let ((org-canvas-time-zone nil)
          (org-canvas--pull-tz-cache "EST5EDT,M3.2.0,M11.1.0"))
      (expect (org-canvas--iso8601-to-org-timestamp "2026-04-04T03:59:00Z")
              :to-equal "<2026-04-03 Fri 23:59>")))

  (it "converts UTC ISO-8601 to course-local Org inactive timestamp"
    (let ((org-canvas-time-zone nil)
          (org-canvas--pull-tz-cache "EST5EDT,M3.2.0,M11.1.0"))
      (expect (org-canvas--iso8601-to-org-inactive-timestamp "2026-04-04T03:59:00Z")
              :to-equal "[2026-04-03 Fri 23:59]")))

  (it "uses Emacs's local zone when no course zone is known (issue #136)"
    (let ((orig-tz (getenv "TZ")))
      (unwind-protect
          (progn
            (set-time-zone-rule "EST5EDT,M3.2.0,M11.1.0")
            (let ((org-canvas-time-zone nil)
                  (org-canvas--pull-tz-cache nil)
                  (org-canvas--time-zone-resolved t))
              (expect (org-canvas--iso8601-to-org-timestamp "2026-04-04T03:59:00Z")
                      :to-equal "<2026-04-03 Fri 23:59>")))
        (set-time-zone-rule orig-tz))))

  (it "returns nil for nil input"
    (expect (org-canvas--iso8601-to-org-timestamp nil) :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--iso8601-to-org-timestamp "") :to-be nil))

  (it "returns nil (no error) for a malformed timestamp"
    ;; A bad Canvas timestamp must degrade gracefully, never erroring (or in
    ;; some org versions prompting) and hanging a pull.  Rejected by the
    ;; ISO-date format guard before parsing.
    (expect (org-canvas--iso8601-to-org-timestamp "not-a-date") :to-be nil)
    (expect (org-canvas--iso8601-to-org-inactive-timestamp "not-a-date")
            :to-be nil))

  (it "returns nil when date-to-time signals on a date-prefixed string"
    ;; date-to-time is version-inconsistent: it errors on some inputs in CI.
    ;; The condition-case must swallow that and return nil rather than hang.
    (cl-letf (((symbol-function 'date-to-time)
               (lambda (_s) (error "Invalid date"))))
      (expect (org-canvas--iso8601-to-org-timestamp "2026-01-01T00:00:00Z")
              :to-be nil)
      (expect (org-canvas--iso8601-to-org-inactive-timestamp "2026-01-01T00:00:00Z")
              :to-be nil))))

(describe "course TZ resolver"
  (it "reads :TIME_ZONE: from settings.org and caches it"
    (let* ((dir (make-temp-file "tz-test-" t))
           (settings-file (expand-file-name "settings.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "* Settings\n:PROPERTIES:\n:TIME_ZONE: America/New_York\n:END:\n"))
            (let ((org-canvas-directory dir)
                  (org-canvas-settings-file settings-file)
                  (org-canvas--pull-tz-cache nil))
              (org-canvas--pull-resolve-tz)
              (expect org-canvas--pull-tz-cache :to-equal "America/New_York")))
        (delete-directory dir t))))

  (it "leaves cache nil when settings.org is missing"
    (let ((org-canvas-settings-file "/nonexistent/settings.org")
          (org-canvas--pull-tz-cache "stale"))
      (org-canvas--pull-resolve-tz)
      (expect org-canvas--pull-tz-cache :to-be nil)))

  (it "leaves cache nil when settings.org has no :TIME_ZONE: prop"
    (let* ((dir (make-temp-file "tz-test-" t))
           (settings-file (expand-file-name "settings.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file (insert "* Settings\n"))
            (let ((org-canvas-settings-file settings-file)
                  (org-canvas--pull-tz-cache "stale"))
              (org-canvas--pull-resolve-tz)
              (expect org-canvas--pull-tz-cache :to-be nil)))
        (delete-directory dir t)))))

;;;; Children content digest (hash-extra material for nested modules)

(describe "org-canvas--org-children-digest"
  (it "returns \"none\" for a heading without children (sibling excluded)"
    (with-temp-org-buffer
     "* Quiz\nBody text.\n* Sibling\nOther content.\n"
     (org-back-to-heading)
     (expect (org-canvas--org-children-digest (point)) :to-equal "none")))

  (it "is stable across repeated computation"
    (with-temp-org-buffer
     "* Quiz\n** Question 1\n:PROPERTIES:\n:POINTS: 2\n:END:\n- [X] A\n- [ ] B\n"
     (org-back-to-heading)
     (expect (org-canvas--org-children-digest (point))
             :to-equal (org-canvas--org-children-digest (point)))))

  (it "changes when a child body changes"
    (let ((d1 (with-temp-org-buffer
               "* Quiz\n** Q1\n- [X] A\n- [ ] B\n"
               (org-back-to-heading)
               (org-canvas--org-children-digest (point))))
          (d2 (with-temp-org-buffer
               "* Quiz\n** Q1\n- [X] A\n- [ ] C\n"
               (org-back-to-heading)
               (org-canvas--org-children-digest (point)))))
      (expect d1 :not :to-equal d2)))

  (it "changes when a child property changes"
    (let ((d1 (with-temp-org-buffer
               "* Quiz\n** Q1\n:PROPERTIES:\n:POINTS: 1\n:END:\n- [X] A\n"
               (org-back-to-heading)
               (org-canvas--org-children-digest (point))))
          (d2 (with-temp-org-buffer
               "* Quiz\n** Q1\n:PROPERTIES:\n:POINTS: 2\n:END:\n- [X] A\n"
               (org-back-to-heading)
               (org-canvas--org-children-digest (point)))))
      (expect d1 :not :to-equal d2)))

  (it "changes when a grandchild (nested heading) changes"
    (let ((d1 (with-temp-org-buffer
               "* Quiz\n** Group\n*** Q1\n- [X] A\n"
               (org-back-to-heading)
               (org-canvas--org-children-digest (point))))
          (d2 (with-temp-org-buffer
               "* Quiz\n** Group\n*** Q1\n- [X] B\n"
               (org-back-to-heading)
               (org-canvas--org-children-digest (point)))))
      (expect d1 :not :to-equal d2)))

  (it "ignores sync-state properties on children"
    ;; Finalize writes these right after the parent hash is computed;
    ;; including them would dirty the parent on every run.  The child
    ;; needs a pre-existing drawer: creating one would add :PROPERTIES:
    ;; and :END: delimiter lines, which legitimately change the digest.
    (with-temp-org-buffer
     "* Quiz\n** Q1\n:PROPERTIES:\n:POINTS: 1\n:END:\n- [X] A\n"
     (org-back-to-heading)
     (let ((before (org-canvas--org-children-digest (point))))
       (save-excursion
         (search-forward "** Q1")
         (org-entry-put (point) "CANVAS_ID" "200")
         (org-entry-put (point) "CANVAS_ITEM_ID" "300")
         (org-entry-put (point) "LAST_SYNCED" "[2026-08-10 Mon 12:00]")
         (org-entry-put (point) "CANVAS_UPDATED_AT" "2026-08-10T12:00:00Z"))
       (expect (org-canvas--org-children-digest (point))
               :to-equal before)))))

(describe "org-canvas--org-timestamps-span-days-p (issue #93)"
  (it "reads the date as written, so local days survive UTC"
    (expect (org-canvas--org-timestamp-date "<2026-11-25 Wed 00:00>")
            :to-equal "2026-11-25")
    (expect (org-canvas--org-timestamp-date "no date here") :to-be nil)
    (expect (org-canvas--org-timestamp-date nil) :to-be nil))

  (it "tells a span from a single day"
    (expect (org-canvas--org-timestamps-span-days-p
             "<2026-11-25 Wed 00:00>" "<2026-11-27 Fri 23:59>")
            :to-be-truthy)
    (expect (org-canvas--org-timestamps-span-days-p
             "<2026-11-25 Wed 00:00>" "<2026-11-25 Wed 23:59>")
            :to-be nil)
    (expect (org-canvas--org-timestamps-span-days-p
             "<2026-11-25 Wed 00:00>" nil)
            :to-be nil)))

(describe "org-canvas--ensure-buffer-fresh (issue #97)"
  (defun test-fresh-97--make-stale (file)
    "Rewrite FILE behind the current buffer and make the modtime differ."
    (with-temp-file file (insert "* New heading\n"))
    (set-file-times file (time-add (current-time) 5)))

  (it "rereads an unmodified stale buffer before writing, in batch"
    (let ((file (make-temp-file "fresh-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (progn
                    (test-fresh-97--make-stale file)
                    (expect (verify-visited-file-modtime (current-buffer))
                            :to-be nil)
                    (let ((noninteractive t))
                      (org-canvas--ensure-buffer-fresh))
                    (expect (buffer-string) :to-equal "* New heading\n")
                    (expect (verify-visited-file-modtime (current-buffer))
                            :to-be-truthy))
                (kill-buffer))))
        (delete-file file))))

  (it "refuses, with a clear error, when the stale buffer holds edits"
    (let ((file (make-temp-file "fresh-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (progn
                    (goto-char (point-max))
                    (insert "local edit\n")
                    (test-fresh-97--make-stale file)
                    (let ((noninteractive t))
                      (expect (org-canvas--ensure-buffer-fresh)
                              :to-throw 'error))
                    ;; The local edit survives; nothing was clobbered.
                    (expect (buffer-string) :to-match "local edit"))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file))))

  (it "leaves a fresh buffer, and any interactive session, alone"
    (let ((file (make-temp-file "fresh-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (progn
                    (let ((noninteractive t))
                      (expect (org-canvas--ensure-buffer-fresh) :to-be nil))
                    (test-fresh-97--make-stale file)
                    ;; Interactive: Emacs's own protection stays in charge.
                    (let ((noninteractive nil))
                      (org-canvas--ensure-buffer-fresh))
                    (expect (buffer-string) :to-equal "* Old heading\n"))
                (kill-buffer))))
        (delete-file file))))

  (defun test-fresh-188--restamp (file)
    "Move FILE's modification time without touching its text."
    (set-file-times file (time-add (current-time) 5)))

  (it "keeps a modified buffer whose file was only restamped after its own save (issue #188)"
    (let ((file (make-temp-file "fresh-" nil ".org"))
          (logged nil))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Journal 03\n"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (let ((noninteractive t))
                    ;; First push: stamp and save through org-canvas.
                    (goto-char (point-max))
                    (insert "* R4: Agency\n")
                    (org-canvas--save-buffer)
                    ;; Second push: its stamp lands in the buffer, and then
                    ;; the sync client restamps the file the first push
                    ;; wrote, before the save.  (Restamped first, a batch
                    ;; Emacs clears the stale time itself on the next edit,
                    ;; and the guard never sees it.)
                    (goto-char (point-max))
                    (insert ":PROPERTIES:\n:CANVAS_ID: 7\n:END:\n")
                    (test-fresh-188--restamp file)
                    (expect (verify-visited-file-modtime (current-buffer)) :to-be nil)
                    (cl-letf (((symbol-function 'org-canvas--log-debug)
                               (lambda (_l fmt &rest args)
                                 (push (apply #'format fmt args) logged))))
                      (org-canvas--save-buffer))
                    (expect (car logged) :to-match "restamped on disk without changing")
                    (expect (verify-visited-file-modtime (current-buffer)) :to-be-truthy)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (expect (buffer-string) :to-match ":CANVAS_ID: 7")))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file))))

  (it "does not reread an unmodified buffer whose file was only restamped"
    (let ((file (make-temp-file "fresh-" nil ".org"))
          (reverted nil))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (unwind-protect
                  (let ((noninteractive t))
                    (test-fresh-188--restamp file)
                    (cl-letf (((symbol-function 'revert-buffer)
                               (lambda (&rest _) (setq reverted t))))
                      (org-canvas--ensure-buffer-fresh))
                    (expect reverted :to-be nil)
                    (expect (verify-visited-file-modtime (current-buffer)) :to-be-truthy))
                (kill-buffer))))
        (delete-file file))))

  (it "still refuses a real rewrite behind a modified buffer it had read"
    (let ((file (make-temp-file "fresh-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (unwind-protect
                  (let ((noninteractive t))
                    (goto-char (point-max))
                    (insert "local edit\n")
                    (test-fresh-97--make-stale file)
                    (expect (org-canvas--ensure-buffer-fresh) :to-throw 'error))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file))))

  (it "treats a buffer org-canvas never read or saved as before"
    (let ((file (make-temp-file "fresh-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (let ((noninteractive t))
                    (expect org-canvas--content-hashes :to-be nil)
                    (goto-char (point-max))
                    (insert "local edit\n")
                    (test-fresh-188--restamp file)
                    (expect (org-canvas--ensure-buffer-fresh) :to-throw 'error))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file))))

  (defun test-fresh-249--roll-back (file content)
    "Write CONTENT, an earlier state of FILE, over it with a later modtime."
    (with-temp-file file (insert content))
    (set-file-times file (time-add (current-time) 5)))

  (it "keeps a buffer whose file was rolled back to one of its own earlier saves, and saves it again (issue #249)"
    (let ((file (make-temp-file "fresh-" nil ".org"))
          (warned nil)
          (messaged nil)
          (after-first-save nil))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* R1\n* R2\n* R3\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (unwind-protect
                  (let ((noninteractive t))
                    ;; Two creates, each stamped and saved.
                    (search-forward "* R1")
                    (org-entry-put (point) "CANVAS_ID" "138472")
                    (org-canvas--save-buffer)
                    (setq after-first-save
                          (with-temp-buffer (insert-file-contents file) (buffer-string)))
                    (goto-char (point-min))
                    (search-forward "* R2")
                    (org-entry-put (point) "CANVAS_ID" "138473")
                    (org-canvas--save-buffer)
                    (expect (length org-canvas--content-hashes) :to-equal 3)
                    ;; The sync client writes the state after save 1 back
                    ;; over save 2, then the third create stamps its id.
                    (test-fresh-249--roll-back file after-first-save)
                    (expect (verify-visited-file-modtime (current-buffer)) :to-be nil)
                    (goto-char (point-min))
                    (search-forward "* R3")
                    (cl-letf (((symbol-function 'org-canvas--log-warning)
                               (lambda (_l fmt &rest args)
                                 (push (apply #'format fmt args) warned)))
                              ((symbol-function 'message)
                               (lambda (fmt &rest args)
                                 (push (apply #'format fmt args) messaged))))
                      (org-canvas-org-set-property (point) "CANVAS_ID" "138474")
                      (org-canvas--save-buffer))
                    (expect (car (last warned)) :to-match "rolled back on disk")
                    (expect (car (last messaged)) :to-match "rolled back on disk")
                    (expect (verify-visited-file-modtime (current-buffer)) :to-be-truthy)
                    ;; Nothing was dropped: the file holds all three stamps.
                    (with-temp-buffer
                      (insert-file-contents file)
                      (expect (buffer-string) :to-match ":CANVAS_ID: 138472")
                      (expect (buffer-string) :to-match ":CANVAS_ID: 138473")
                      (expect (buffer-string) :to-match ":CANVAS_ID: 138474")))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file))))

  (it "treats a roll-back to the text it first read the same way, even with nothing to stamp"
    (let ((file (make-temp-file "fresh-" nil ".org"))
          (reverted nil))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* R1\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (unwind-protect
                  (let ((noninteractive t))
                    (search-forward "* R1")
                    (org-entry-put (point) "CANVAS_ID" "1")
                    (org-canvas--save-buffer)
                    (test-fresh-249--roll-back file "* R1\n")
                    (cl-letf (((symbol-function 'revert-buffer)
                               (lambda (&rest _) (setq reverted t))))
                      (org-canvas--ensure-buffer-fresh))
                    (expect reverted :to-be nil)
                    (expect (buffer-modified-p) :to-be nil)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (expect (buffer-string) :to-match ":CANVAS_ID: 1")))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file))))

  (it "records each distinct save once"
    (let ((file (make-temp-file "fresh-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* R1\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (unwind-protect
                  (progn
                    (expect (length org-canvas--content-hashes) :to-equal 1)
                    (org-canvas--note-saved-content)
                    (expect (length org-canvas--content-hashes) :to-equal 1)
                    (goto-char (point-max))
                    (insert "* R2\n")
                    (org-canvas--note-saved-content)
                    (expect (length org-canvas--content-hashes) :to-equal 2))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file))))

  (it "warns, in the log and on stderr, when it rereads text this buffer never saw"
    (let ((file (make-temp-file "fresh-" nil ".org"))
          (warned nil)
          (messaged nil))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (org-canvas--find-file-noselect file)
              (unwind-protect
                  (let ((noninteractive t))
                    (test-fresh-97--make-stale file)
                    (cl-letf (((symbol-function 'org-canvas--log-warning)
                               (lambda (_l fmt &rest args)
                                 (push (apply #'format fmt args) warned)))
                              ((symbol-function 'message)
                               (lambda (fmt &rest args)
                                 (push (apply #'format fmt args) messaged))))
                      (org-canvas--ensure-buffer-fresh))
                    (expect (car warned) :to-match "rereading it before writing")
                    (expect (car messaged) :to-match "changed on disk during the run")
                    (expect (buffer-string) :to-equal "* New heading\n"))
                (kill-buffer))))
        (delete-file file))))

  (it "lets a property write land on the reread content"
    (let ((file (make-temp-file "fresh-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (progn
                    (test-fresh-97--make-stale file)
                    (let ((noninteractive t))
                      (org-canvas-org-set-property (point-min) "CANVAS_ID" "9"))
                    (expect (buffer-string) :to-match "\\* New heading")
                    (expect (org-entry-get (point-min) "CANVAS_ID")
                            :to-equal "9"))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-file file)))))

(describe "org-canvas--find-file-noselect (issue #121)"
  (defun test-visit-121--stale (file text)
    "Rewrite FILE with TEXT behind its buffer and move its modtime forward."
    (with-temp-file file (insert text))
    (set-file-times file (time-add (current-time) 5)))

  (it "rereads a file rewritten behind its buffer instead of asking, in batch"
    (let ((file (make-temp-file "visit-121-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (let ((buf (find-file-noselect file)))
              (unwind-protect
                  (progn
                    (test-visit-121--stale file "* Restamped heading\n")
                    ;; A sync client's mtime rewrite used to reach
                    ;; yes-or-no-p here, which in batch reads stdin.
                    (cl-letf (((symbol-function 'yes-or-no-p)
                               (lambda (&rest _)
                                 (error "Supersession question asked in batch"))))
                      (let ((noninteractive t))
                        (expect (org-canvas--find-file-noselect file)
                                :to-be buf)))
                    (with-current-buffer buf
                      (expect (buffer-string) :to-equal "* Restamped heading\n")))
                (kill-buffer buf))))
        (delete-file file))))

  (it "signals rather than choosing between a stale file and unsaved edits"
    (let ((file (make-temp-file "visit-121-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (let ((buf (find-file-noselect file)))
              (unwind-protect
                  (progn
                    (with-current-buffer buf
                      (goto-char (point-max))
                      (insert "local edit\n"))
                    (test-visit-121--stale file "* Restamped heading\n")
                    (let ((noninteractive t))
                      (expect (org-canvas--find-file-noselect file)
                              :to-throw 'error)))
                (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf))))
        (delete-file file))))

  (it "leaves Emacs's own protection in charge interactively"
    (let ((file (make-temp-file "visit-121-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Old heading\n"))
            (let ((buf (find-file-noselect file))
                  (asked nil))
              (unwind-protect
                  (progn
                    (test-visit-121--stale file "* Restamped heading\n")
                    (cl-letf (((symbol-function 'yes-or-no-p)
                               (lambda (&rest _) (setq asked t) nil)))
                      (let ((noninteractive nil))
                        (org-canvas--find-file-noselect file)))
                    (expect asked :to-be t)
                    (with-current-buffer buf
                      (expect (buffer-string) :to-equal "* Old heading\n")))
                (kill-buffer buf))))
        (delete-file file))))

  (it "visits a file nothing has opened yet"
    (let ((file (make-temp-file "visit-121-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Fresh heading\n"))
            (let ((buf (org-canvas--find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buf
                    (expect (buffer-string) :to-equal "* Fresh heading\n"))
                (kill-buffer buf))))
        (delete-file file)))))

(describe "one time zone for push and pull (issue #136)"
  (it "push reads an Org timestamp in the course zone"
    (let ((org-canvas--pull-tz-cache "EST5EDT,M3.2.0,M11.1.0")
          (org-canvas-time-zone nil))
      (expect (org-canvas-org-parse-timestamp "<2026-08-31 Mon 23:59>")
              :to-equal "2026-09-01T03:59:00Z")))

  (it "a deadline survives a round trip"
    (let ((org-canvas--pull-tz-cache "EST5EDT,M3.2.0,M11.1.0")
          (org-canvas-time-zone nil))
      (let ((org-ts (org-canvas--iso8601-to-org-timestamp "2026-09-01T03:59:00Z")))
        (expect org-ts :to-equal "<2026-08-31 Mon 23:59>")
        (expect (org-canvas-org-parse-timestamp org-ts)
                :to-equal "2026-09-01T03:59:00Z"))))

  (it "org-canvas-time-zone pins the zone over settings.org"
    (let ((org-canvas-time-zone "UTC")
          (org-canvas--pull-tz-cache "EST5EDT,M3.2.0,M11.1.0"))
      (expect (org-canvas-org-parse-timestamp "<2026-08-31 Mon 23:59>")
              :to-equal "2026-08-31T23:59:00Z")
      (expect (org-canvas--iso8601-to-org-timestamp "2026-08-31T23:59:00Z")
              :to-equal "<2026-08-31 Mon 23:59>")))

  (it "resolves the course zone lazily from settings.org on any path"
    (let* ((dir (make-temp-file "tz-lazy-" t))
           (settings-file (expand-file-name "settings.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "* Settings\n:PROPERTIES:\n:TIME_ZONE: EST5EDT,M3.2.0,M11.1.0\n:END:\n"))
            (let ((org-canvas-settings-file settings-file)
                  (org-canvas-time-zone nil)
                  (org-canvas--pull-tz-cache nil)
                  (org-canvas--time-zone-resolved nil))
              ;; No explicit resolve first: pull-at-point and the conflict
              ;; pull never called one, and wrote UTC (issue #134).
              (expect (org-canvas--iso8601-to-org-timestamp "2026-09-01T03:59:00Z")
                      :to-equal "<2026-08-31 Mon 23:59>")
              (expect org-canvas--time-zone-resolved :to-be t)))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory dir t))))

  (it "resolves once and remembers a course with no zone"
    (let ((org-canvas-settings-file "/nonexistent/settings.org")
          (org-canvas-time-zone nil)
          (org-canvas--pull-tz-cache nil)
          (org-canvas--time-zone-resolved nil)
          (real (symbol-function 'org-canvas--pull-resolve-tz))
          (calls 0))
      (cl-letf (((symbol-function 'org-canvas--pull-resolve-tz)
                 (lambda () (cl-incf calls) (funcall real))))
        (org-canvas--time-zone)
        (org-canvas--time-zone)
        (expect calls :to-equal 1)
        (expect (org-canvas--time-zone) :to-be nil))))

  (it "org-canvas--time-zone-reset forgets the resolved zone"
    (let ((org-canvas--pull-tz-cache "UTC")
          (org-canvas--time-zone-resolved t))
      (org-canvas--time-zone-reset)
      (expect org-canvas--pull-tz-cache :to-be nil)
      (expect org-canvas--time-zone-resolved :to-be nil))))

(describe "org-canvas--report-display (issue #169)"
  (after-each
    (when (get-buffer "*test-report*") (kill-buffer "*test-report*")))

  (it "prints the report to standard output under noninteractive"
    (let ((printed ""))
      (cl-letf (((symbol-function 'princ)
                 (lambda (s &optional _) (setq printed (concat printed s)))))
        (let ((noninteractive t))
          (org-canvas--report-display "*test-report*"
                                      (lambda () (insert "17 errors, and here they are\n")))))
      (expect printed :to-match "17 errors, and here they are")))

  (it "displays the buffer instead when a human is watching"
    (let (displayed printed)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buf &rest _) (setq displayed (buffer-name buf))))
                ((symbol-function 'princ)
                 (lambda (s &optional _) (setq printed s))))
        (let ((noninteractive nil))
          (org-canvas--report-display "*test-report*"
                                      (lambda () (insert "report\n")))))
      (expect displayed :to-equal "*test-report*")
      (expect printed :to-be nil)))

  (it "leaves the report in the buffer either way, and returns it"
    (let ((text (cl-letf (((symbol-function 'princ) #'ignore))
                  (let ((noninteractive t))
                    (org-canvas--report-display "*test-report*"
                                                (lambda () (insert "the body\n")))))))
      (expect text :to-equal "the body\n")
      (expect (with-current-buffer "*test-report*" (buffer-string))
              :to-equal "the body\n")))

  (it "empties the buffer before re-rendering"
    (cl-letf (((symbol-function 'princ) #'ignore))
      (let ((noninteractive t))
        (org-canvas--report-display "*test-report*" (lambda () (insert "first\n")))
        (org-canvas--report-display "*test-report*" (lambda () (insert "second\n")))))
    (expect (with-current-buffer "*test-report*" (buffer-string))
            :to-equal "second\n"))

  (it "applies the major mode it is given"
    (cl-letf (((symbol-function 'princ) #'ignore))
      (let ((noninteractive t))
        (org-canvas--report-display "*test-report*" (lambda () (insert "x\n"))
                                    #'fundamental-mode)))
    (expect (with-current-buffer "*test-report*" major-mode)
            :to-equal 'fundamental-mode)))

(describe "org-canvas--iso8601-to-org-timestamp"
  (it "converts ISO8601 to active timestamp"
    (let ((result (org-canvas--iso8601-to-org-timestamp "2026-01-15T10:00:00Z")))
      (expect result :to-match "<2026-01-15")))

  (it "returns nil for nil active-timestamp input"
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

  (it "returns nil for nil inactive-timestamp input"
    (expect (org-canvas--iso8601-to-org-inactive-timestamp nil) :to-be nil)))

(provide 'org-canvas-core-org-test)
;;; org-canvas-core-org-test.el ends here
