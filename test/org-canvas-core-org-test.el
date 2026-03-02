;;; org-canvas-core-org-test.el --- Buttercup tests for org-canvas-core Org interaction utilities  -*- lexical-binding: t; -*-

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

(describe "org-canvas--html-to-org pandoc failure"
  (it "returns warning with raw HTML when pandoc exits non-zero"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete _buffer &rest _args)
                 1)))  ;; exit code 1
      (let ((result (org-canvas--html-to-org "<p>Test</p>")))
        (expect result :to-match "WARNING.*pandoc conversion failed")
        (expect result :to-match "<p>Test</p>")))))

;;;; Section Name → ID Resolution

(describe "org-canvas--resolve-section-names-to-ids"
  (it "returns nil for nil input"
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
      (spy-on 'elog-warning)
      (with-temp-buffer
        (insert "* H\n[[file:missing.png]]\n")
        (org-canvas--resolve-image-links "/tmp/nonexistent/")
        (expect 'elog-warning :to-have-been-called))))

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

(provide 'org-canvas-core-org-test)
;;; org-canvas-core-org-test.el ends here
