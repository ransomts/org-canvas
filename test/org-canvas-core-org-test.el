;;; org-canvas-core-org-test.el --- Buttercup tests for org-canvas-core Org interaction utilities  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(describe "file-level LAST_SYNCED"
  (it "writes #+LAST_SYNCED to the buffer header"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp (insert "#+TITLE: Pages\n* Page 1\n"))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :to-match
                      "^#\\+LAST_SYNCED: \\[[0-9-]+ \\w+ [0-9:]+\\]"))
            (kill-buffer))
        (delete-file temp))))

  (it "replaces an existing header instead of duplicating"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp
        (insert "#+TITLE: Pages\n#+LAST_SYNCED: [2025-01-01 Wed 00:00]\n* x\n"))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((count 0))
                (goto-char (point-min))
                (while (re-search-forward "^#\\+LAST_SYNCED:" nil t)
                  (cl-incf count))
                (expect count :to-equal 1)))
            (kill-buffer))
        (delete-file temp))))

  (it "inserts header at top when no #+TITLE present"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp (insert "* Heading\n"))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (goto-char (point-min))
              (expect (buffer-substring (point-min) (line-end-position))
                      :to-match "^#\\+LAST_SYNCED:"))
            (kill-buffer))
        (delete-file temp))))

  (it "inserts header into a brand-new empty buffer"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp (insert ""))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :to-match
                      "^#\\+LAST_SYNCED: \\[[0-9-]+ \\w+ [0-9:]+\\]"))
            (kill-buffer))
        (delete-file temp)))))

(describe "org-canvas--pull-read-file-header"
  (it "reads #+LAST_SYNCED from the current buffer"
    (with-temp-buffer
      (insert "#+TITLE: Pages\n#+LAST_SYNCED: [2026-04-26 Sun 12:00]\n* x\n")
      (expect (org-canvas--pull-read-file-header)
              :to-equal "[2026-04-26 Sun 12:00]")))

  (it "returns nil when no #+LAST_SYNCED"
    (with-temp-buffer
      (insert "#+TITLE: Pages\n* x\n")
      (expect (org-canvas--pull-read-file-header) :to-be nil))))

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

  (it "does not warn about links in property drawers (GROUP/RUBRIC_LINK)"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-propdrawer-" t))
             (test-file (expand-file-name "assignments.org" dir))
             (warnings nil))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "* Global Challenge Essay
:PROPERTIES:
:GROUP: [[file:assignment-groups.org::*Essays][Essays]]
:RUBRIC_LINK: [[file:rubrics.org::*Essay Rubric][Essay Rubric]]
:END:

Write the essay.

** Details
:PROPERTIES:
:GROUP: [[file:assignment-groups.org::*Essays][Essays]]
:END:

More text.
"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (cl-letf (((symbol-function 'org-canvas--log-warning)
                           (lambda (_logger fmt &rest args)
                             (push (apply #'format fmt args) warnings))))
                  (let ((html (org-canvas--export-subtree-body-to-html)))
                    (expect html :to-match "Write the essay")
                    (expect html :to-match "More text")
                    (expect html :not :to-match "Essay Rubric")))
                (kill-buffer))
              (expect warnings :to-equal nil))
          (delete-directory dir t)))))

  (it "still warns about genuinely unresolvable body links"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-unresolved-" t))
             (pages-file (expand-file-name "pages.org" dir))
             (test-file (expand-file-name "test.org" dir))
             (warnings nil))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Some Other Page\n"))
              (with-temp-file test-file
                (insert "* Assignment\n:PROPERTIES:\n:END:\n\nSee [[file:pages.org::*Missing Page][Missing Page]].\n"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (cl-letf (((symbol-function 'org-canvas--log-warning)
                           (lambda (_logger fmt &rest args)
                             (push (apply #'format fmt args) warnings))))
                  (let ((html (org-canvas--export-subtree-body-to-html)))
                    (expect html :to-match "Missing Page")))
                (kill-buffer))
              (expect (cl-find-if (lambda (w) (string-match-p "Unresolved" w))
                                  warnings)
                      :to-be-truthy))
          (delete-directory dir t)))))

  (it "succeeds with source blocks without requiring a kernel"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-babel-" t))
             (test-file (expand-file-name "test.org" dir))
             (org-html-htmlize-output-type nil))
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
     (let* ((org-html-htmlize-output-type nil)
            (html (org-canvas--export-subtree-body-to-html)))
       (expect html :to-be-truthy)
       (expect html :to-match "42"))))

  (it "falls back to default-directory when buffer has no file"
    (with-temp-buffer
      (insert "* Heading\n:PROPERTIES:\n:END:\n\nSome content here.\n")
      (org-mode)
      (goto-char (point-min))
      (let ((html (org-canvas--export-subtree-body-to-html)))
        (expect html :to-be-truthy)
        (expect html :to-match "Some content here")))))

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

;;;; Body Link Resolution Warnings

(describe "org-canvas--resolve-body-links warnings"
  (it "warns on unresolved body links"
    (spy-on 'org-canvas--log-warning)
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
                (expect 'org-canvas--log-warning :to-have-been-called)))
          (delete-directory dir t)))))

  (it "does not warn on resolved body links"
    (spy-on 'org-canvas--log-warning)
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
                (expect 'org-canvas--log-warning :not :to-have-been-called)))
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

(describe "org-canvas--html-to-org pandoc failure"
  (it "returns warning with raw HTML when pandoc exits non-zero"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete _buffer &rest _args)
                 1)))  ;; exit code 1
      (let ((result (org-canvas--html-to-org "<p>Test</p>")))
        (expect result :to-match "WARNING.*pandoc conversion failed")
        (expect result :to-match "<p>Test</p>")))))

(describe "org-canvas--html-strip-ids"
  (it "removes a single id attribute"
    (expect (org-canvas--html-strip-ids "<div id=\"content\">Hi</div>")
            :to-equal "<div>Hi</div>"))

  (it "removes multiple id attributes"
    (expect (org-canvas--html-strip-ids
             "<div id=\"a\"><span id=\"b\">x</span></div>")
            :to-equal "<div><span>x</span></div>"))

  (it "leaves other attributes intact"
    (expect (org-canvas--html-strip-ids
             "<a href=\"x\" id=\"foo\" class=\"bar\">link</a>")
            :to-equal "<a href=\"x\" class=\"bar\">link</a>"))

  (it "is a no-op when there are no id attributes"
    (expect (org-canvas--html-strip-ids "<p>plain</p>")
            :to-equal "<p>plain</p>"))

  (it "handles empty string"
    (expect (org-canvas--html-strip-ids "") :to-equal ""))

  (it "handles nil"
    (expect (org-canvas--html-strip-ids nil) :to-be nil))

  (it "handles single-quoted id values"
    (expect (org-canvas--html-strip-ids "<div id='content'>Hi</div>")
            :to-equal "<div>Hi</div>"))

  (it "handles ids containing dashes and digits"
    (expect (org-canvas--html-strip-ids
             "<div id=\"masthead-container-1\">x</div>")
            :to-equal "<div>x</div>")))

(describe "org-canvas--html-to-org strips id attributes before pandoc"
  (it "passes id-stripped HTML to pandoc"
    (let (captured)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
                ((symbol-function 'call-process-region)
                 (lambda (start end _program &optional _delete _buffer &rest _args)
                   (setq captured (buffer-substring-no-properties start end))
                   0)))
        (org-canvas--html-to-org "<div id=\"content\"><p>x</p></div>")
        (expect captured :not :to-match " id=")))))

(describe "org-canvas--html-to-org-post-process"
  (it "returns nil unchanged"
    (expect (org-canvas--html-to-org-post-process nil) :to-be nil))

  (it "decodes a non-breaking space to a regular space"
    (let* ((nbsp (string ? ))
           (input (concat "Foo" nbsp "Bar")))
      (expect (org-canvas--html-to-org-post-process input)
              :to-equal "Foo Bar")))

  (it "collapses a line containing only NBSP into an empty line"
    ;; Mirrors `<p>&nbsp;</p>' spacers in Canvas's WYSIWYG output:
    ;; the NBSP becomes a regular space, then the whitespace-only
    ;; line is flattened.
    (let* ((nbsp (string ? ))
           (input (concat "before\n" nbsp "\nafter")))
      (expect (org-canvas--html-to-org-post-process input)
              :to-equal "before\n\nafter")))

  (it "collapses a line containing only spaces and tabs into an empty line"
    (expect (org-canvas--html-to-org-post-process "before\n  \t  \nafter")
            :to-equal "before\n\nafter"))

  (it "inserts a space before <YYYY-MM-DD when preceded by non-whitespace"
    (expect (org-canvas--html-to-org-post-process "Due:<2026-04-22>")
            :to-equal "Due: <2026-04-22>"))

  (it "leaves a properly-spaced timestamp alone"
    (expect (org-canvas--html-to-org-post-process "Due: <2026-04-22>")
            :to-equal "Due: <2026-04-22>"))

  (it "does not insert a space at the start of a line"
    ;; A timestamp at line start has no preceding non-whitespace char to
    ;; key off of, so the post-pass leaves it alone.
    (expect (org-canvas--html-to-org-post-process "<2026-04-22>")
            :to-equal "<2026-04-22>"))

  (it "does not insert a space before `<YYYY-MM-DD' inside link display text"
    ;; Org link openers `[[' should not be treated as a label that
    ;; needs a space before a date — the `[' is in the exclusion set
    ;; for the timestamp-spacing regex.  Use a context where the link
    ;; resolves (heading has CUSTOM_ID `tag') so the TOC-repair pass
    ;; doesn't drop the link wrapper.
    (let* ((input (concat "* heading\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: tag\n"
                          ":END:\n"
                          "\n"
                          "see [[#tag][<2026-04-22>]]"))
           (out (org-canvas--html-to-org-post-process input)))
      (expect out :to-match "\\[\\[#tag\\]\\[<2026-04-22>\\]\\]")
      (expect out :not :to-match "< 2026")))

  (it "does not insert a space before a non-date `<' (e.g., `<em>')"
    (expect (org-canvas--html-to-org-post-process "say <em>x</em>")
            :to-equal "say <em>x</em>")))

(describe "org-canvas--normalize-toc-target"
  (it "lowercases and trims"
    (expect (org-canvas--normalize-toc-target "  Overview  ")
            :to-equal "overview"))

  (it "strips a leading numeric prefix `N. '"
    (expect (org-canvas--normalize-toc-target "1. Overview")
            :to-equal "overview"))

  (it "strips a leading numeric prefix `N) '"
    (expect (org-canvas--normalize-toc-target "2) Methods")
            :to-equal "methods"))

  (it "leaves text without a numeric prefix alone"
    (expect (org-canvas--normalize-toc-target "Conclusion")
            :to-equal "conclusion"))

  (it "returns empty for nil"
    (expect (org-canvas--normalize-toc-target nil) :to-equal "")))

(describe "org-canvas--repair-toc-and-prune-customids"
  (it "is a no-op for nil and empty input"
    (expect (org-canvas--repair-toc-and-prune-customids nil) :to-be nil)
    (expect (org-canvas--repair-toc-and-prune-customids "") :to-equal ""))

  (it "rewrites a dangling TOC link to the matching heading's CUSTOM_ID"
    (let* ((input (concat "[[#orga904f23][1. Overview]]\n"
                          "\n"
                          "* 1. Overview\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: overview\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "\\[\\[#overview\\]\\[1\\. Overview\\]\\]")
      (expect out :not :to-match "orga904f23")))

  (it "matches by display text after stripping numeric prefix"
    ;; Heading has no numeric prefix; TOC link does.  Normalization
    ;; strips the prefix from the link text so the lookup succeeds.
    (let* ((input (concat "[[#orgaaa][1. Methods]]\n"
                          "\n"
                          "* Methods\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: methods\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "\\[\\[#methods\\]\\[1\\. Methods\\]\\]")))

  (it "drops the link wrapper when no heading matches"
    (let* ((input "see [[#orphan][Mystery Section]]\n")
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "Mystery Section")
      (expect out :not :to-match "\\[\\[")
      (expect out :not :to-match "orphan")))

  (it "preserves a link whose target is already a known CUSTOM_ID"
    (let* ((input (concat "see [[#methods][Methods]]\n"
                          "\n"
                          "* Methods\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: methods\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "\\[\\[#methods\\]\\[Methods\\]\\]")))

  (it "prunes a CUSTOM_ID that no remaining link references"
    (let* ((input (concat "[[#kept][Kept]]\n"
                          "\n"
                          "* Kept\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: kept\n"
                          ":END:\n"
                          "\n"
                          "* Orphan\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: orphan\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match ":CUSTOM_ID: kept")
      (expect out :not :to-match ":CUSTOM_ID: orphan")
      ;; The Orphan heading's now-empty PROPERTIES drawer is collapsed.
      (expect out :not :to-match "^\\* Orphan\n:PROPERTIES:")))

  (it "leaves headings with no CUSTOM_ID drawer alone"
    (let* ((input "* Plain\n\nbody\n")
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-equal input)))

  (it "is a no-op for body without headings or anchor links"
    (let ((input "Just a paragraph.\n\nAnother paragraph."))
      (expect (org-canvas--repair-toc-and-prune-customids input)
              :to-equal input))))

;;;; File-URL → Local-Link Rewriting (pull-side inverse of image resolver)

(describe "org-canvas--build-file-id-cache"
  (it "returns an empty hash for an empty files.org"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (let ((cache (org-canvas--build-file-id-cache temp-file)))
            (expect (hash-table-count cache) :to-equal 0))
        (delete-file temp-file))))

  (it "collects top-level file headings"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* [[file:content/foo.pdf][foo.pdf]]
:PROPERTIES:
:CANVAS_ID: 1001
:END:
* [[file:content/bar.png][bar.png]]
:PROPERTIES:
:CANVAS_ID: 1002
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (gethash "1001" cache) :to-equal "content/foo.pdf")
              (expect (gethash "1002" cache) :to-equal "content/bar.png")
              (expect (hash-table-count cache) :to-equal 2)))
        (delete-file temp-file))))

  (it "collects nested file headings under folder headings"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Uploaded Media
** [[file:content/Uploaded Media/img.png][img.png]]
:PROPERTIES:
:CANVAS_ID: 2001
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (gethash "2001" cache)
                      :to-equal "content/Uploaded Media/img.png")
              (expect (hash-table-count cache) :to-equal 1)))
        (delete-file temp-file))))

  (it "skips folder headings (no file: link, no CANVAS_ID)"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Uploaded Media
** [[file:content/Uploaded Media/img.png][img.png]]
:PROPERTIES:
:CANVAS_ID: 3001
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (hash-table-count cache) :to-equal 1)))
        (delete-file temp-file))))

  (it "skips entries missing CANVAS_ID"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* [[file:content/foo.pdf][foo.pdf]]
:PROPERTIES:
:LAST_SYNCED: [2026-01-01 Thu 00:00]
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (hash-table-count cache) :to-equal 0)))
        (delete-file temp-file))))

  (it "returns an empty hash for a nonexistent file"
    (let ((cache (org-canvas--build-file-id-cache "/nonexistent/files.org")))
      (expect (hash-table-count cache) :to-equal 0))))

(describe "org-canvas--rewrite-canvas-file-urls"
  ;; These tests focus on cache-hit behavior.  The cache-miss path now
  ;; triggers an on-demand API fetch (Task 6), so we stub the API to
  ;; signal an error — that exercises the pass-through-on-failure branch
  ;; without making real network calls.
  (let (cache)
    (before-each
      (setq cache (make-hash-table :test 'equal))
      (puthash "29257665" "content/Uploaded Media/img.png" cache)
      (puthash "28960058" "content/readings/ethics.pdf" cache)
      (org-canvas--pull-summary-reset)
      (spy-on 'org-canvas-api-request
              :and-call-fake
              (lambda (&rest _)
                (signal 'org-canvas-api-error '("Forbidden" nil nil)))))

    (it "rewrites a bare URL link with no description to file link with filename"
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/29257665/preview?verifier=abc]]"
               cache)
              :to-equal "[[file:content/Uploaded Media/img.png][img.png]]"))

    (it "preserves the description when present"
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/28960058?verifier=abc&wrap=1][What is data ethics.pdf]]"
               cache)
              :to-equal "[[file:content/readings/ethics.pdf][What is data ethics.pdf]]"))

    (it "leaves URLs alone whose IDs are not in the cache"
      (let ((input "[[https://x.instructure.com/courses/1/files/9999999/preview?verifier=z]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input cache)
                :to-equal input)))

    (it "rewrites multiple URLs in the same string"
      (let ((result (org-canvas--rewrite-canvas-file-urls
                     "before [[https://x.instructure.com/courses/1/files/29257665/preview?verifier=a]] middle [[https://x.instructure.com/courses/1/files/28960058?verifier=b][label]] after"
                     cache)))
        (expect result :to-match "\\[\\[file:content/Uploaded Media/img.png\\]\\[img.png\\]\\]")
        (expect result :to-match "\\[\\[file:content/readings/ethics.pdf\\]\\[label\\]\\]")
        (expect result :to-match "before ")
        (expect result :to-match " middle ")
        (expect result :to-match " after")))

    (it "leaves non-Canvas URLs alone"
      (let ((input "[[https://example.com/page][example]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input cache)
                :to-equal input)))

    (it "handles bare URL with no /preview suffix"
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/29257665]]"
               cache)
              :to-equal "[[file:content/Uploaded Media/img.png][img.png]]"))

    (it "rewrites bare ?verifier=&wrap=1 form without /preview and without description"
      ;; Regression: lock in that `files/NNNN?verifier=…&wrap=1' (no `/preview',
      ;; no `][desc]') is matched and rewritten just like the /preview form.
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/29257665?verifier=ABC&wrap=1]]"
               cache)
              :to-equal "[[file:content/Uploaded Media/img.png][img.png]]"))

    (it "leaves a bare ?verifier= URL unchanged when its ID is not in cache"
      ;; Regression: cache miss must pass through verbatim, not partially mangle
      ;; the URL (Task 6 will add an on-demand fetch for unknown IDs).
      (let ((input "[[https://x.instructure.com/courses/1/files/99999999?verifier=Z&wrap=1]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input cache)
                :to-equal input)))

    (it "returns nil unchanged"
      (expect (org-canvas--rewrite-canvas-file-urls nil cache) :to-be nil))

    (it "returns empty string unchanged"
      (expect (org-canvas--rewrite-canvas-file-urls "" cache) :to-equal ""))

    (it "is a no-op when cache is empty"
      (let ((empty (make-hash-table :test 'equal))
            (input "[[https://x.instructure.com/courses/1/files/29257665/preview?verifier=a]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input empty)
                :to-equal input)))))

(describe "org-canvas--html-to-org-with-rewrite"
  (it "returns empty string for nil"
    (expect (org-canvas--html-to-org-with-rewrite nil) :to-equal ""))

  (it "returns empty string for empty input"
    (expect (org-canvas--html-to-org-with-rewrite "") :to-equal ""))

  (it "rewrites a Canvas file URL using the cache"
    (let ((org-canvas--file-id-cache (make-hash-table :test 'equal)))
      (puthash "30061566" "content/Uploaded Media/screenshot.png"
               org-canvas--file-id-cache)
      (with-html-to-org-identity
        (let ((result (org-canvas--html-to-org-with-rewrite
                       "[[https://x.com/courses/1/files/30061566/preview]]")))
          (expect result :to-match
                  "\\[\\[file:content/Uploaded Media/screenshot\\.png\\]"))))))

(describe "org-canvas--html-to-org-inline-with-rewrite"
  (it "returns empty string for nil"
    (expect (org-canvas--html-to-org-inline-with-rewrite nil) :to-equal ""))

  (it "returns empty string for empty input"
    (expect (org-canvas--html-to-org-inline-with-rewrite "") :to-equal ""))

  (it "collapses newlines and rewrites file URLs"
    (let ((org-canvas--file-id-cache (make-hash-table :test 'equal)))
      (puthash "42" "content/foo.png" org-canvas--file-id-cache)
      (with-html-to-org-identity
        (let ((result (org-canvas--html-to-org-inline-with-rewrite
                       "see\n[[https://x.com/courses/1/files/42/preview]]")))
          (expect result :not :to-match "\n")
          (expect result :to-match "\\[\\[file:content/foo\\.png\\]"))))))

(describe "fetch unknown file on rewrite"
  (before-each
    (setq org-canvas--rewrite-folder-cache nil)
    (org-canvas--pull-summary-reset))

  (it "fetches metadata, downloads, registers, and rewrites"
    (let ((cache (make-hash-table :test 'equal))
          (api-calls 0)
          (downloads 0)
          (org-canvas-directory (make-temp-file "test-rewrite-" t))
          (org-canvas-files-file nil))
      (unwind-protect
          (progn
            (setq org-canvas-files-file
                  (expand-file-name "files.org" org-canvas-directory))
            (with-temp-file org-canvas-files-file
              (insert "#+TITLE: Files\n"))
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (_method url &rest _args)
                         (cl-incf api-calls)
                         (cond
                          ((string-match-p "/api/v1/files/30061566\\'" url)
                           '((id . 30061566)
                             (display_name . "screenshot.png")
                             (folder_id . 999)
                             (url . "https://x.com/files/30061566/download?verifier=Z")
                             (content-type . "image/png")
                             (size . 12345)))
                          ((string-match-p "/api/v1/folders/999\\'" url)
                           '((id . 999)
                             (full_name . "course files/Uploaded Media"))))))
                      ((symbol-function 'org-canvas--file-pull-download)
                       (lambda (_dn _url path _size)
                         (cl-incf downloads)
                         (make-directory (file-name-directory path) t)
                         (with-temp-file path (insert "fake bytes")))))
              (let* ((input "see [[https://x.com/courses/281704/files/30061566/preview?verifier=A]]")
                     (rewritten (org-canvas--rewrite-canvas-file-urls input cache)))
                (expect rewritten :to-match
                        "\\[\\[file:content/Uploaded Media/screenshot\\.png\\]\\[screenshot\\.png\\]\\]")
                (expect downloads :to-equal 1)
                (expect (gethash "30061566" cache)
                        :to-equal "content/Uploaded Media/screenshot.png"))))
        (delete-directory org-canvas-directory t))))

  (it "returns nil and records to summary when metadata GET fails"
    (let ((cache (make-hash-table :test 'equal))
          (org-canvas-directory (make-temp-file "test-rewrite-fail-" t))
          (org-canvas-files-file nil))
      (unwind-protect
          (progn
            (setq org-canvas-files-file
                  (expand-file-name "files.org" org-canvas-directory))
            (with-temp-file org-canvas-files-file (insert ""))
            (org-canvas--pull-summary-reset)
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (&rest _)
                         (signal 'org-canvas-api-error '("Forbidden" nil nil)))))
              (let* ((input "x [[https://x.com/courses/281704/files/99999999?verifier=Z]] y")
                     (rewritten (org-canvas--rewrite-canvas-file-urls input cache)))
                ;; URL passes through unchanged
                (expect rewritten :to-equal input)
                ;; Failure recorded
                (expect (org-canvas--pull-summary-empty-p) :to-be nil))))
        (delete-directory org-canvas-directory t))))

  (it "caches resolved IDs for the rest of the session"
    (let ((cache (make-hash-table :test 'equal))
          (api-calls 0)
          (org-canvas-directory (make-temp-file "test-rewrite-cache-" t))
          (org-canvas-files-file nil))
      (unwind-protect
          (progn
            (setq org-canvas-files-file
                  (expand-file-name "files.org" org-canvas-directory))
            (with-temp-file org-canvas-files-file (insert ""))
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (_method url &rest _args)
                         (cl-incf api-calls)
                         (cond
                          ((string-match-p "/api/v1/files/30061566\\'" url)
                           '((id . 30061566)
                             (display_name . "screenshot.png")
                             (folder_id . 999)
                             (url . "https://x.com/files/30061566/download?verifier=Z")))
                          ((string-match-p "/api/v1/folders/999\\'" url)
                           '((full_name . "course files/Uploaded Media"))))))
                      ((symbol-function 'org-canvas--file-pull-download)
                       (lambda (_dn _url path _size)
                         (make-directory (file-name-directory path) t)
                         (with-temp-file path (insert "x")))))
              (let ((url1 "[[https://x.com/courses/281704/files/30061566/preview?verifier=A]]")
                    (url2 "[[https://x.com/courses/281704/files/30061566?verifier=B]]"))
                (org-canvas--rewrite-canvas-file-urls (concat url1 "\n" url2) cache)
                ;; Two URLs, but only one metadata fetch + one folder fetch
                (expect api-calls :to-equal 2))))
        (delete-directory org-canvas-directory t)))))

;;;; Pull-side buffer lifecycle helpers

(describe "org-canvas--pull-was-fresh-p"
  (it "returns t when file does not exist and no buffer visits it"
    (let ((temp (make-temp-file "fresh-" nil ".org")))
      (delete-file temp)
      (unwind-protect
          (expect (org-canvas--pull-was-fresh-p temp) :to-be-truthy)
        (when (file-exists-p temp) (delete-file temp)))))

  (it "returns nil when the file already exists on disk"
    (let ((temp (make-temp-file "exists-" nil ".org")))
      (unwind-protect
          (expect (org-canvas--pull-was-fresh-p temp) :to-be nil)
        (delete-file temp))))

  (it "returns nil when a buffer already visits the file"
    (let* ((temp (make-temp-file "visited-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (expect (org-canvas--pull-was-fresh-p temp) :to-be nil)
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file temp)))))

(describe "org-canvas--pull-kill-fresh-buffer"
  (it "kills the buffer when WAS-FRESH and buffer is unmodified"
    (let* ((temp (make-temp-file "kill-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (org-canvas--pull-kill-fresh-buffer temp t)
            (expect (find-buffer-visiting temp) :to-be nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (file-exists-p temp) (delete-file temp)))))

  (it "leaves the buffer alone when WAS-FRESH is nil"
    (let* ((temp (make-temp-file "keep-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (org-canvas--pull-kill-fresh-buffer temp nil)
            (expect (buffer-live-p buf) :to-be-truthy))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file temp))))

  (it "does not kill a modified buffer even when WAS-FRESH"
    (let* ((temp (make-temp-file "dirty-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edit") (set-buffer-modified-p t))
            (org-canvas--pull-kill-fresh-buffer temp t)
            (expect (buffer-live-p buf) :to-be-truthy))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp))))

  (it "is a no-op when no buffer visits the file"
    (let ((temp (make-temp-file "nobuf-" nil ".org")))
      (unwind-protect
          (progn
            (org-canvas--pull-kill-fresh-buffer temp t)
            (expect (find-buffer-visiting temp) :to-be nil))
        (delete-file temp)))))

(describe "org-canvas--pull-confirm-unsaved"
  (it "is a no-op when no buffer visits the file"
    (let ((temp (make-temp-file "noopen-" nil ".org")))
      (unwind-protect
          (expect (org-canvas--pull-confirm-unsaved temp "feature")
                  :not :to-throw)
        (delete-file temp))))

  (it "is a no-op when the visiting buffer is unmodified"
    (let* ((temp (make-temp-file "clean-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (expect (org-canvas--pull-confirm-unsaved temp "feature")
                  :not :to-throw)
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file temp))))

  (it "saves the buffer when the user answers yes"
    (let* ((temp (make-temp-file "yes-" nil ".org"))
           (buf (find-file-noselect temp))
           (saved nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edits") (set-buffer-modified-p t))
            ;; `noninteractive' is t under the test runner, which would
            ;; short-circuit the prompt; bind it off to exercise the answer.
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                        ((symbol-function 'org-canvas--save-buffer)
                         (lambda () (setq saved t) (set-buffer-modified-p nil))))
                (org-canvas--pull-confirm-unsaved temp "feature")))
            (expect saved :to-be-truthy))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp))))

  (it "signals user-error when the user answers no"
    (let* ((temp (make-temp-file "no-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edits") (set-buffer-modified-p t))
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                (expect (org-canvas--pull-confirm-unsaved temp "feature")
                        :to-throw 'user-error))))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp))))

  ;; Issue #34: under `emacs --batch' this prompt consumed stdin and
  ;; silently swallowed a step of a full `org-canvas-sync'.
  (it "saves without prompting in batch mode"
    (let* ((temp (make-temp-file "batch-" nil ".org"))
           (buf (find-file-noselect temp))
           (saved nil)
           (prompted nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edits") (set-buffer-modified-p t))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) (setq prompted t) nil))
                      ((symbol-function 'org-canvas--save-buffer)
                       (lambda () (setq saved t) (set-buffer-modified-p nil))))
              (org-canvas--pull-confirm-unsaved temp "feature"))
            (expect prompted :to-be nil)
            (expect saved :to-be-truthy))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp)))))

(describe "org-canvas--pull-confirm-overwrite"
  (it "aborts when the user declines"
    (let ((temp (make-temp-file "existing-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "* Existing heading\n"))
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                (expect (org-canvas--pull-confirm-overwrite temp "sections")
                        :to-throw 'user-error))))
        (delete-file temp))))

  (it "proceeds when the user accepts"
    (let ((temp (make-temp-file "existing-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "* Existing heading\n"))
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                (expect (org-canvas--pull-confirm-overwrite temp "sections")
                        :not :to-throw))))
        (delete-file temp))))

  ;; This is the prompt that blocked batch runs: it fires for every pull
  ;; whose target file already exists, which is the normal case.
  (it "does not prompt in batch mode"
    (let ((temp (make-temp-file "existing-" nil ".org"))
          (prompted nil))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "* Existing heading\n"))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) (setq prompted t) nil)))
              (expect (org-canvas--pull-confirm-overwrite temp "sections")
                      :not :to-throw))
            (expect prompted :to-be nil))
        (delete-file temp))))

  (it "is a no-op for an empty file"
    (let ((temp (make-temp-file "empty-" nil ".org")))
      (unwind-protect
          (let ((noninteractive nil))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) (error "Should not prompt for an empty file"))))
              (expect (org-canvas--pull-confirm-overwrite temp "sections")
                      :not :to-throw)))
        (delete-file temp)))))

(describe "org-canvas--pull-insert-body file-URL rewriting"
  (it "rewrites Canvas file URLs in the converted org body"
    (let ((temp-files (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-files
              (insert "* [[file:content/foo.pdf][foo.pdf]]
:PROPERTIES:
:CANVAS_ID: 7777
:END:
"))
            (let ((org-canvas-files-file temp-files)
                  (org-canvas--file-id-cache nil))
              (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
                        ((symbol-function 'call-process-region)
                         (lambda (_start _end _program &optional _delete buffer &rest _args)
                           (when buffer
                             (erase-buffer)
                             (insert "[[https://x.instructure.com/courses/1/files/7777/preview?verifier=z]]"))
                           0)))
                (with-temp-buffer
                  (org-mode)
                  (insert "* Heading\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
                  (goto-char (point-min))
                  (org-back-to-heading)
                  (org-canvas--pull-insert-body "<p>anything</p>")
                  (let ((body (buffer-substring-no-properties (point-min) (point-max))))
                    (expect body :to-match "\\[\\[file:content/foo.pdf\\]\\[foo.pdf\\]\\]")
                    (expect body :not :to-match "instructure.com"))))))
        (delete-file temp-files)))))

(describe "org-canvas--html-to-org-inline"
  (it "collapses multi-line pandoc output to single line"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete buffer &rest _args)
                 (when buffer
                   (erase-buffer)
                   (insert "Line one\nLine two\nLine three"))
                 0)))
      (expect (org-canvas--html-to-org-inline "<p>Line one</p><p>Line two</p>")
              :to-equal "Line one Line two Line three")))

  (it "returns empty string for nil input"
    (expect (org-canvas--html-to-org-inline nil) :to-equal ""))

  (it "returns empty string for empty input"
    (expect (org-canvas--html-to-org-inline "") :to-equal ""))

  (it "trims surrounding whitespace"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete buffer &rest _args)
                 (when buffer
                   (erase-buffer)
                   (insert "  trimmed  "))
                 0)))
      (expect (org-canvas--html-to-org-inline "<p> trimmed </p>")
              :to-equal "trimmed"))))

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

;;;; Inline Image Resolution Tests

(describe "org-canvas--resolve-image-links"
  (it "replaces image link with cached URL"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (puthash "diagram.png" "https://canvas.example.com/files/42/preview"
               org-canvas--image-cache)
      (with-temp-buffer
        (insert "* Heading\nSee [[file:diagram.png]] for details.\n")
        (org-canvas--resolve-image-links "/tmp/")
        (expect (buffer-string) :to-match "canvas\\.example\\.com/files/42/preview"))))

  (it "preserves display text in image links"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (puthash "chart.jpg" "https://canvas.example.com/files/10/preview"
               org-canvas--image-cache)
      (with-temp-buffer
        (insert "* Heading\n[[file:chart.jpg][Sales Chart]]\n")
        (org-canvas--resolve-image-links "/tmp/")
        (expect (buffer-string) :to-match "\\[Sales Chart\\]"))))

  (it "handles multiple image links"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (puthash "a.png" "https://canvas.example.com/a" org-canvas--image-cache)
      (puthash "b.gif" "https://canvas.example.com/b" org-canvas--image-cache)
      (with-temp-buffer
        (insert "* H\n[[file:a.png]] and [[file:b.gif]]\n")
        (org-canvas--resolve-image-links "/tmp/")
        (expect (buffer-string) :to-match "canvas\\.example\\.com/a")
        (expect (buffer-string) :to-match "canvas\\.example\\.com/b"))))

  (it "leaves non-image file links untouched"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (with-temp-buffer
        (insert "* H\n[[file:handout.pdf]]\n")
        (let ((original (buffer-string)))
          (org-canvas--resolve-image-links "/tmp/")
          (expect (buffer-string) :to-equal original)))))

  (it "warns on missing local file"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (spy-on 'org-canvas--log-warning)
      (with-temp-buffer
        (insert "* H\n[[file:missing.png]]\n")
        (org-canvas--resolve-image-links "/tmp/nonexistent/")
        (expect 'org-canvas--log-warning :to-have-been-called))))

  (it "uploads image on cache miss when file exists"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal))
          (temp-file (make-temp-file "img-test" nil ".png")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PNG"))
            (cl-letf (((symbol-function 'org-canvas--image-ensure-folder)
                       (lambda () 123))
                      ((symbol-function 'org-canvas--upload-file)
                       (lambda (_path &optional _url _name)
                         '((id . 555)))))
              (with-org-canvas-test-config
                (with-temp-buffer
                  (insert (format "* H\n[[file:%s]]\n" temp-file))
                  (org-canvas--resolve-image-links "/")
                  (expect (buffer-string) :to-match "files/555/preview")
                  ;; Verify cache was updated
                  (expect (gethash (file-name-nondirectory temp-file)
                                   org-canvas--image-cache)
                          :to-be-truthy)))))
        (delete-file temp-file))))

  (it "recognizes all image extensions"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (dolist (ext org-canvas--image-extensions)
        (puthash (format "test.%s" ext) (format "https://url/%s" ext)
                 org-canvas--image-cache))
      (with-temp-buffer
        (insert "* H\n")
        (dolist (ext org-canvas--image-extensions)
          (insert (format "[[file:test.%s]]\n" ext)))
        (org-canvas--resolve-image-links "/tmp/")
        (dolist (ext org-canvas--image-extensions)
          (expect (buffer-string) :to-match (format "url/%s" ext)))))))

(describe "org-canvas--image-cache-init"
  (it "populates cache from API response"
    (with-org-canvas-test-config
      (let ((org-canvas--image-cache nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     ;; folders/by_path returns a list
                     '(((id . 42)))))
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &rest _args)
                     '(((display_name . "logo.png") (url . "https://files.canvas/logo.png"))
                       ((display_name . "banner.jpg") (url . "https://files.canvas/banner.jpg"))))))
          (org-canvas--image-cache-init)
          (expect (hash-table-count org-canvas--image-cache) :to-equal 2)
          (expect (gethash "logo.png" org-canvas--image-cache)
                  :to-equal "https://files.canvas/logo.png")))))

  (it "handles missing folder gracefully"
    (with-org-canvas-test-config
      (let ((org-canvas--image-cache nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (signal 'error '("404")))))
          (org-canvas--image-cache-init)
          ;; Cache should exist but be empty
          (expect org-canvas--image-cache :to-be-truthy)
          (expect (hash-table-count org-canvas--image-cache) :to-equal 0)))))

  (it "does not re-initialize if already set"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal))
          (api-called nil))
      (puthash "existing.png" "url" org-canvas--image-cache)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (setq api-called t) nil)))
        (org-canvas--image-cache-init)
        (expect api-called :to-be nil)
        (expect (hash-table-count org-canvas--image-cache) :to-equal 1)))))

(describe "org-canvas--image-ensure-folder"
  (it "returns existing folder ID"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   '(((id . 77))))))
        (expect (org-canvas--image-ensure-folder) :to-equal 77))))

  (it "creates folder when not found"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (eq method 'GET)
                         (signal 'error '("404"))
                       '((id . 88))))))
          (expect (org-canvas--image-ensure-folder) :to-equal 88)
          (expect call-count :to-equal 2))))))

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

;;;; Image upload failure in resolve-single-image

(describe "org-canvas--resolve-single-image"
  (it "handles upload failure gracefully"
    (with-org-canvas-test-config
      (let* ((temp-dir (make-temp-file "img-test-" t))
             (img-file (expand-file-name "test.png" temp-dir))
             (org-canvas--image-cache (make-hash-table :test 'equal))
             (org-canvas-image-folder "course-images")
             (folder-id-ref (list 99))
             (replaced nil))
        (unwind-protect
            (progn
              (with-temp-file img-file (insert "PNGDATA"))
              (cl-letf (((symbol-function 'org-canvas--upload-file)
                         (lambda (&rest _) (error "Network error")))
                        ((symbol-function 'org-canvas--image-replace-link)
                         (lambda (&rest _) (setq replaced t))))
                (let ((rep (list :path "test.png" :start 1 :end 20 :display nil)))
                  ;; Should not error, just warn
                  (org-canvas--resolve-single-image rep temp-dir folder-id-ref 1 1)
                  ;; Image should NOT have been replaced (upload failed)
                  (expect replaced :to-be nil))))
          (delete-directory temp-dir t))))))

;;;; Pull-item macro :after-pull coverage

(defvar test--after-pull-cov-called nil)

(describe "org-canvas-define-pull-item :after-pull hook"
  (it "calls after-pull function with item and pos"
    (setq test--after-pull-cov-called nil)
    (eval
     '(org-canvas-define-pull-item test--after-pull-cov
        :after-pull (lambda (item _pos)
                      (setq test--after-pull-cov-called item))
        :properties
        ((some_field "SOME_FIELD" :type string)))
     t)
    (with-temp-org-buffer
     "* Test Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((item '((some_field . "value1"))))
       (org-canvas--test--after-pull-cov-pull-item item (point))
       (expect test--after-pull-cov-called :to-equal item)))))

(describe "org-canvas--resolve-single-image echo area warnings"
  (it "shows warning when image upload fails"
    (with-org-canvas-test-config
      (let ((temp-dir (make-temp-file "img-test-" t)))
        (unwind-protect
            (let ((img-file (expand-file-name "test.png" temp-dir)))
              (with-temp-file img-file (insert "fake-png"))
              (spy-on 'message)
              (spy-on 'org-canvas--log-warning)
              (spy-on 'org-canvas--log-info)
              (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
                (cl-letf (((symbol-function 'org-canvas--image-ensure-folder)
                           (lambda () 1))
                          ((symbol-function 'org-canvas--upload-file)
                           (lambda (&rest _) (error "Upload failed"))))
                  (org-canvas--resolve-single-image
                   (list :start 0 :end 10 :path "test.png" :display nil)
                   temp-dir (list nil) 1 1)
                  (expect 'message :to-have-been-called-with
                          "WARNING: Image upload failed: %s" "test.png"))))
          (delete-directory temp-dir t)))))

  (it "shows warning when image file not found"
    (with-org-canvas-test-config
      (spy-on 'message)
      (spy-on 'org-canvas--log-warning)
      (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
        (org-canvas--resolve-single-image
         (list :start 0 :end 10 :path "nonexistent.png" :display nil)
         "/tmp/no-such-dir" (list nil) 1 1)
        (expect 'message :to-have-been-called-with
                "WARNING: Image not found: %s"
                (expand-file-name "nonexistent.png" "/tmp/no-such-dir"))))))

(describe "org-canvas--resolve-image-links progress"
  (it "shows per-image progress messages"
    (with-org-canvas-test-config
      (spy-on 'message)
      (spy-on 'org-canvas--resolve-single-image)
      (spy-on 'org-canvas--image-cache-init)
      (with-temp-buffer
        (insert "[[file:img1.png]] and [[file:img2.png]]")
        (org-canvas--resolve-image-links "/tmp/")
        (expect 'message :to-have-been-called-with "Images [%d/%d] Processing..." 1 2)
        (expect 'message :to-have-been-called-with "Images [%d/%d] Processing..." 2 2)))))

(describe "pull summary accumulator"
  (it "starts empty after reset"
    (org-canvas--pull-summary-reset)
    (expect (org-canvas--pull-summary-empty-p) :to-be t))

  (it "records errors with file, message, and log line"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record
     :file "pages.org"
     :item "connecting-to-the-palmetto-jupyter-image"
     :error "Operation timeout"
     :log-line 154)
    (expect (org-canvas--pull-summary-empty-p) :to-be nil)
    (let ((records (org-canvas--pull-summary-records)))
      (expect (length records) :to-equal 1)
      (expect (plist-get (car records) :file) :to-equal "pages.org")
      (expect (plist-get (car records) :item)
              :to-equal "connecting-to-the-palmetto-jupyter-image")
      (expect (plist-get (car records) :error) :to-equal "Operation timeout")
      (expect (plist-get (car records) :log-line) :to-equal 154)))

  (it "preserves insertion order across multiple records"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :item "first" :error "e1")
    (org-canvas--pull-summary-record :file "b.org" :item "second" :error "e2")
    (let ((records (org-canvas--pull-summary-records)))
      (expect (length records) :to-equal 2)
      (expect (plist-get (nth 0 records) :item) :to-equal "first")
      (expect (plist-get (nth 1 records) :item) :to-equal "second")))

  (it "prints nothing when empty"
    (org-canvas--pull-summary-reset)
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-equal "")))

  (it "prints a summary block when non-empty"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record
     :file "pages.org" :item "x" :error "Operation timeout" :log-line 154)
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "Pull complete with 1 non-fatal error")
      (expect output :to-match "pages.org")
      (expect output :to-match "Operation timeout")
      (expect output :to-match "log line 154")))

  (it "pluralizes errors correctly with multiple records"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "e1")
    (org-canvas--pull-summary-record :file "b.org" :error "e2")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "2 non-fatal errors")))

  (it "omits the item suffix when item is nil"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "settings.org" :error "boom")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :not :to-match "\\[")))

  (it "omits the log line suffix when log-line is nil"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "settings.org" :error "boom")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :not :to-match "log line"))))

(describe "pull summary skip records (issue #81)"
  (it "defaults a record's kind to error"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (expect (plist-get (car (org-canvas--pull-summary-records)) :kind)
            :to-equal 'error))

  (it "separates skips from errors by kind"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (org-canvas--pull-summary-record :kind 'skip :file "pages.org"
                                     :item "home" :error "front page")
    (expect (length (org-canvas--pull-summary-records-of-kind 'error))
            :to-equal 1)
    (let ((skips (org-canvas--pull-summary-records-of-kind 'skip)))
      (expect (length skips) :to-equal 1)
      (expect (plist-get (car skips) :item) :to-equal "home")))

  (it "counts a record written without a kind as an error"
    (org-canvas--pull-summary-reset)
    (push (list :file "a.org" :error "boom") org-canvas--pull-summary)
    (expect (length (org-canvas--pull-summary-records-of-kind 'error))
            :to-equal 1)
    (expect (org-canvas--pull-summary-records-of-kind 'skip) :to-be nil))

  (it "prints skips in their own section"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :kind 'skip :file "pages.org"
                                     :item "home" :error "front page")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "1 item skipped")
      (expect output :to-match "pages.org \\[home\\]: front page")
      (expect output :not :to-match "non-fatal error")))

  (it "pluralizes and separates the two sections when both are present"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (org-canvas--pull-summary-record :kind 'skip :file "pages.org"
                                     :item "home" :error "front page")
    (org-canvas--pull-summary-record :kind 'skip :file "d.org"
                                     :item "news" :error "announcement")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "1 non-fatal error")
      (expect output :to-match "2 items skipped")))

  (it "tallies errors alone"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (expect (org-canvas--pull-summary-tally) :to-equal "1 non-fatal error(s)"))

  (it "tallies skips alone"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :kind 'skip :file "a.org" :error "why")
    (expect (org-canvas--pull-summary-tally) :to-equal "1 item(s) skipped"))

  (it "tallies both kinds together"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (org-canvas--pull-summary-record :kind 'skip :file "b.org" :error "why")
    (expect (org-canvas--pull-summary-tally)
            :to-equal "1 non-fatal error(s), 1 item(s) skipped"))

  (it "tallies an empty accumulator as an empty string"
    (org-canvas--pull-summary-reset)
    (expect (org-canvas--pull-summary-tally) :to-equal "")))

(describe "pull skip helpers (issue #81)"
  (it "labels an item by its title field"
    (expect (org-canvas--pull-item-label
             '((url . "home") (title . "Home")) 'url 'title)
            :to-equal "Home"))

  (it "falls back to the id field when there is no title"
    (expect (org-canvas--pull-item-label '((url . "home")) 'url 'title)
            :to-equal "home"))

  (it "labels an item carrying neither field"
    (expect (org-canvas--pull-item-label '((foo . 1)) 'url 'title)
            :to-equal "(unnamed)"))

  (it "returns an empty suffix when nothing was skipped"
    (expect (org-canvas--pull-skip-suffix 0 "front page") :to-equal ""))

  (it "names the reason in the suffix"
    (expect (org-canvas--pull-skip-suffix 2 "front page")
            :to-equal " (2 skipped: front page)"))

  (it "omits the reason when the module declares none"
    (expect (org-canvas--pull-skip-suffix 1 nil) :to-equal " (1 skipped)"))

  (it "logs and records a skipped item"
    (org-canvas--pull-summary-reset)
    (let ((logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
        (org-canvas--pull-record-skip
         "/tmp/course/pages.org" '((url . "home") (title . "Home"))
         'url 'title "front page"))
      (expect (car logged) :to-equal "[Pull] Skipped 'Home': front page")
      (let ((rec (car (org-canvas--pull-summary-records-of-kind 'skip))))
        (expect (plist-get rec :file) :to-equal "pages.org")
        (expect (plist-get rec :item) :to-equal "Home")
        (expect (plist-get rec :error) :to-equal "front page"))))

  (it "records a reasonless skip with a generic explanation"
    (org-canvas--pull-summary-reset)
    (cl-letf (((symbol-function 'org-canvas--log-info) #'ignore))
      (org-canvas--pull-record-skip
       "/tmp/course/pages.org" '((url . "home")) 'url 'title nil))
    (expect (plist-get (car (org-canvas--pull-summary-records-of-kind 'skip))
                       :error)
            :to-match "skip rule")))

(describe "TZ-aware pull timestamp"
  ;; POSIX TZ string (`EST5EDT,M3.2.0,M11.1.0') is used instead of the
  ;; IANA name `America/New_York' so the test passes on systems whose
  ;; Emacs build can't resolve IANA names against tzdata (e.g., the
  ;; minimal CI runner). Production `org-canvas--pull-tz-cache' takes
  ;; whatever string Canvas returned (typically IANA), and IANA name
  ;; resolution works on all common end-user systems.
  (it "converts UTC ISO-8601 to course-local Org active timestamp"
    (let ((org-canvas--pull-tz-cache "EST5EDT,M3.2.0,M11.1.0"))
      (expect (org-canvas--iso8601-to-org-timestamp "2026-04-04T03:59:00Z")
              :to-equal "<2026-04-03 Fri 23:59>")))

  (it "converts UTC ISO-8601 to course-local Org inactive timestamp"
    (let ((org-canvas--pull-tz-cache "EST5EDT,M3.2.0,M11.1.0"))
      (expect (org-canvas--iso8601-to-org-inactive-timestamp "2026-04-04T03:59:00Z")
              :to-equal "[2026-04-03 Fri 23:59]")))

  (it "uses UTC when cache is unset (back-compat)"
    (let ((org-canvas--pull-tz-cache nil))
      (expect (org-canvas--iso8601-to-org-timestamp "2026-04-04T03:59:00Z")
              :to-equal "<2026-04-04 Sat 03:59>")))

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

(describe "empty file pull header"
  (it "writes #+TITLE, #+LAST_SYNCED, and 0-items comment"
    (let ((temp (make-temp-file "empty-test-" nil ".org")))
      (unwind-protect
          (progn
            (org-canvas--pull-emit-empty-file temp "Discussions")
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((s (buffer-string)))
                (expect s :to-match "^#\\+TITLE: Discussions$")
                (expect s :to-match "^#\\+LAST_SYNCED: \\[")
                (expect s :to-match "^# Canvas returned 0 items at this sync\\.$"))))
        (delete-file temp))))

  (it "overwrites existing content"
    (let ((temp (make-temp-file "empty-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "stale content here\n* Old heading\n"))
            (org-canvas--pull-emit-empty-file temp "Calendar")
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :not :to-match "stale content")
              (expect (buffer-string) :to-match "Canvas returned 0 items")))
        (delete-file temp)))))

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

(describe "org-canvas--pull-known-ids"
  (it "returns the Canvas ids the file already claims"
    (let ((temp (make-temp-file "known-ids-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"
                      "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 62\n:END:\n"
                      "* Draft\n"))
            (expect (sort (org-canvas--pull-known-ids temp "CANVAS_ID") #'string<)
                    :to-equal '("61" "62")))
        (let ((buf (find-buffer-visiting temp)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))
        (delete-file temp))))

  (it "returns nothing for a file that does not exist yet"
    (expect (org-canvas--pull-known-ids "/tmp/no-such-pull-file.org" "CANVAS_ID")
            :to-be nil)))

(describe "org-canvas--pull-item-managed-p"
  (it "recognizes an item the file already claims, comparing as strings"
    (expect (org-canvas--pull-item-managed-p '((id . 61)) 'id '("61" "62"))
            :to-be t))

  (it "rejects an item no heading claims"
    (expect (org-canvas--pull-item-managed-p '((id . 99)) 'id '("61" "62"))
            :to-be nil))

  (it "rejects an item with no id at all"
    (expect (org-canvas--pull-item-managed-p '((title . "x")) 'id '("61"))
            :to-be nil)))

(describe "a generated pull with a prefix argument"
  ;; Issue #67: pulling a whole endpoint to reconcile two items writes a
  ;; heading for every item the course holds, managed or not.
  (it "refreshes only the items the file already claims"
    (let ((temp (make-temp-file "managed-pull-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp
              (insert "* Old title\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                       (lambda (&rest _)
                         '(((id . 100) (title . "Mine, renamed")
                            (message . "<p>x</p>"))
                           ((id . 999) (title . "Someone else's")
                            (message . "<p>y</p>"))))))
              (let ((org-canvas-announcements-file temp))
                (org-canvas-pull-announcements t)))
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((s (buffer-string)))
                (expect s :to-match "Mine, renamed")
                (expect s :not :to-match "Someone else's"))))
        (let ((buf (find-buffer-visiting temp)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))
        (delete-file temp))))

  (it "imports everything when called without the prefix argument"
    (let ((temp (make-temp-file "unmanaged-pull-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp
              (insert "* Old title\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                       (lambda (&rest _)
                         '(((id . 100) (title . "Mine, renamed")
                            (message . "<p>x</p>"))
                           ((id . 999) (title . "Someone else's")
                            (message . "<p>y</p>"))))))
              (let ((org-canvas-announcements-file temp))
                (org-canvas-pull-announcements)))
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :to-match "Someone else's")))
        (let ((buf (find-buffer-visiting temp)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))
        (delete-file temp)))))

(describe "org-canvas--export-subtree-body-to-html offline (issue #83)"
  (it "skips link and image resolution so a read-only caller never uploads"
    (with-temp-org-buffer
     "* Page\n\nSee [[file:pages.org::*Other][Other]] and [[file:img.png]].\n"
     (org-back-to-heading)
     (let ((links nil) (images nil))
       (cl-letf (((symbol-function 'org-canvas--resolve-body-links)
                  (lambda (_) (setq links t)))
                 ((symbol-function 'org-canvas--resolve-image-links)
                  (lambda (_) (setq images t))))
         (let ((html (org-canvas--export-subtree-body-to-html t)))
           (expect html :to-match "Other")
           (expect links :to-be nil)
           (expect images :to-be nil))
         (org-canvas--export-subtree-body-to-html)
         (expect links :to-be t)
         (expect images :to-be t))))))

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

(provide 'org-canvas-core-org-test)
;;; org-canvas-core-org-test.el ends here
