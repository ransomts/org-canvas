;;; org-canvas-files-test.el --- Buttercup tests for org-canvas-files  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Comprehensive Buttercup tests for org-canvas-files.el
;; Tests cover: helper functions, folder operations, and all 4 pipeline stages.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-files)

;;;; Helper Functions

(describe "org-canvas--file-extract-link-path"
  (before-each (test-org-canvas-reset-file-caches))

  (it "extracts path from [[file:path][name]] link"
    (expect (org-canvas--file-extract-link-path "[[file:docs/syllabus.pdf][Syllabus]]")
            :to-equal "docs/syllabus.pdf"))

  (it "extracts path from [[pdf:path::page][name]] link"
    (expect (org-canvas--file-extract-link-path "[[pdf:lecture.pdf::5][Lecture Notes]]")
            :to-equal "lecture.pdf"))

  (it "extracts path from bare [[file:path]] link"
    (expect (org-canvas--file-extract-link-path "[[file:notes.txt]]")
            :to-equal "notes.txt"))

  (it "returns nil for plain heading text"
    (expect (org-canvas--file-extract-link-path "Just a folder heading")
            :to-be nil))

  (it "returns nil for nil link-path input"
    (expect (org-canvas--file-extract-link-path nil)
            :to-be nil))

  (it "handles paths with spaces"
    (expect (org-canvas--file-extract-link-path "[[file:my documents/file.pdf][File]]")
            :to-equal "my documents/file.pdf")))

(describe "org-canvas--file-get-display-name"
  (before-each (test-org-canvas-reset-file-caches))

  (it "extracts display name from link"
    (expect (org-canvas--file-get-display-name "[[file:doc.pdf][My Document]]")
            :to-equal "My Document"))

  (it "returns plain heading text for non-links"
    (expect (org-canvas--file-get-display-name "Folder Name")
            :to-equal "Folder Name"))

  (it "returns nil for nil display-name input"
    (expect (org-canvas--file-get-display-name nil)
            :to-be nil))

  (it "handles complex display names"
    (expect (org-canvas--file-get-display-name "[[file:week01.pdf][Week 1 - Introduction]]")
            :to-equal "Week 1 - Introduction")))

(describe "org-canvas--file-guess-content-type"
  (before-each (test-org-canvas-reset-file-caches))

  (describe "document types"
    (it "returns application/pdf for PDF files"
      (expect (org-canvas--file-guess-content-type "document.pdf")
              :to-equal "application/pdf"))

    (it "returns text/plain for TXT files"
      (expect (org-canvas--file-guess-content-type "notes.txt")
              :to-equal "text/plain"))

    (it "returns text/html for HTML files"
      (expect (org-canvas--file-guess-content-type "page.html")
              :to-equal "text/html"))

    (it "returns text/csv for CSV files"
      (expect (org-canvas--file-guess-content-type "data.csv")
              :to-equal "text/csv"))

    (it "returns text/markdown for MD files"
      (expect (org-canvas--file-guess-content-type "readme.md")
              :to-equal "text/markdown"))

    (it "returns application/msword for DOC files"
      (expect (org-canvas--file-guess-content-type "document.doc")
              :to-equal "application/msword"))

    (it "returns application/vnd.ms-excel for XLS files"
      (expect (org-canvas--file-guess-content-type "spreadsheet.xls")
              :to-equal "application/vnd.ms-excel"))

    (it "returns application/vnd.ms-powerpoint for PPT files"
      (expect (org-canvas--file-guess-content-type "slides.ppt")
              :to-equal "application/vnd.ms-powerpoint")))

  (describe "image types"
    (it "returns image/png for PNG files"
      (expect (org-canvas--file-guess-content-type "image.png")
              :to-equal "image/png"))

    (it "returns image/jpeg for JPG files"
      (expect (org-canvas--file-guess-content-type "photo.jpg")
              :to-equal "image/jpeg"))

    (it "returns image/gif for GIF files"
      (expect (org-canvas--file-guess-content-type "animation.gif")
              :to-equal "image/gif"))

    (it "returns image/svg+xml for SVG files"
      (expect (org-canvas--file-guess-content-type "icon.svg")
              :to-equal "image/svg+xml")))

  (describe "code and data types"
    (it "returns application/json for JSON files"
      (expect (org-canvas--file-guess-content-type "data.json")
              :to-equal "application/json"))

    (it "returns text/x-python for Python files"
      (expect (org-canvas--file-guess-content-type "script.py")
              :to-equal "text/x-python"))

    (it "returns text/css for CSS files"
      (expect (org-canvas--file-guess-content-type "style.css")
              :to-equal "text/css"))

    (it "returns application/javascript for JS files"
      (expect (org-canvas--file-guess-content-type "script.js")
              :to-equal "application/javascript"))

    (it "returns application/xml for XML files"
      (expect (org-canvas--file-guess-content-type "config.xml")
              :to-equal "application/xml")))

  (describe "archive types"
    (it "returns application/zip for ZIP files"
      (expect (org-canvas--file-guess-content-type "archive.zip")
              :to-equal "application/zip"))

    (it "returns application/gzip for GZ files"
      (expect (org-canvas--file-guess-content-type "archive.gz")
              :to-equal "application/gzip"))

    (it "returns application/x-tar for TAR files"
      (expect (org-canvas--file-guess-content-type "backup.tar")
              :to-equal "application/x-tar")))

  (describe "other types"
    (it "returns application/octet-stream for unknown extensions"
      (expect (org-canvas--file-guess-content-type "file.xyz")
              :to-equal "application/octet-stream"))

    (it "handles uppercase extensions"
      (expect (org-canvas--file-guess-content-type "DOCUMENT.PDF")
              :to-equal "application/pdf"))

    (it "handles files without extensions"
      (expect (org-canvas--file-guess-content-type "Makefile")
              :to-equal "application/octet-stream"))))

(describe "org-canvas--file-get-folder-path"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns empty string for top-level headings"
    (with-temp-org-buffer
     "* [[file:doc.pdf][Document]]
"
     (org-back-to-heading)
     (expect (org-canvas--file-get-folder-path (point) "/tmp/")
             :to-equal "")))

  (it "returns parent folder name for nested file"
    (with-temp-org-buffer
     "* Labs
** [[file:lab1.pdf][Lab 1]]
"
     (search-forward "Lab 1")
     (org-back-to-heading)
     (expect (org-canvas--file-get-folder-path (point) "/tmp/")
             :to-equal "Labs")))

  (it "builds multi-level folder path"
    (with-temp-org-buffer
     "* Course Materials
** Week 01
*** [[file:notes.pdf][Notes]]
"
     (search-forward "Notes")
     (org-back-to-heading)
     (expect (org-canvas--file-get-folder-path (point) "/tmp/")
             :to-equal "Course Materials/Week 01")))

  (it "skips parent headings that are files"
    (with-temp-org-buffer
     "* Folder
** [[file:parent.pdf][Parent File]]
*** [[file:child.pdf][Child File]]
"
     (search-forward "Child File")
     (org-back-to-heading)
     ;; Should only include "Folder", not the parent file heading
     (expect (org-canvas--file-get-folder-path (point) "/tmp/")
             :to-equal "Folder"))))

(describe "org-canvas--file-transform-props"
  (before-each (test-org-canvas-reset-file-caches))

  (it "defaults hidden to nil and reads HIDDEN when set"
    (let ((off (org-canvas--file-transform-props
                '(:display-name "Test" :local-path "/tmp/test.pdf"
                  :folder-path "" :canvas-id nil
                  :published-raw nil :hidden-raw nil
                  :unlock-at-raw nil :lock-at-raw nil
                  :use-justification nil :usage-license nil :copyright nil)))
          (on (org-canvas--file-transform-props
               '(:display-name "Test" :local-path "/tmp/test.pdf"
                 :folder-path "" :canvas-id nil
                 :published-raw nil :hidden-raw "true"
                 :unlock-at-raw nil :lock-at-raw nil
                 :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get off :hidden) :to-be nil)
      (expect (plist-get on :hidden) :to-be t)))

  (it "passes through display-name unchanged"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Syllabus.pdf" :local-path "/tmp/syllabus.pdf"
                     :folder-path "Documents" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :display-name) :to-equal "Syllabus.pdf")))

  (it "passes through local-path unchanged"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/home/user/docs/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :local-path) :to-equal "/home/user/docs/test.pdf")))

  (it "passes through folder-path unchanged"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "Labs/Week1" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :folder-path) :to-equal "Labs/Week1")))

  (it "passes through canvas-id unchanged"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id "42"
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :canvas-id) :to-equal "42")))

  (it "defaults published to true when nil"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :published) :to-be t)))

  (it "interprets published=false"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw "false" :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :published) :to-be nil)))

  (it "interprets published=true"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw "true" :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :published) :to-be t)))

  (it "parses UNLOCK_AT to ISO8601"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw "<2024-06-15 Sat 10:00>"
                     :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :unlock-at) :to-match "2024-06-15T")))

  (it "parses LOCK_AT to ISO8601"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil
                     :lock-at-raw "<2024-09-01 Sun 10:00>"
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :lock-at) :to-match "2024-09-01T")))

  (it "returns nil for absent UNLOCK_AT"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :unlock-at) :to-be nil)))

  (it "returns nil for absent LOCK_AT"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil :copyright nil))))
      (expect (plist-get result :lock-at) :to-be nil)))

  (it "passes through use-justification unchanged"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification "fair_use" :usage-license nil
                     :copyright nil))))
      (expect (plist-get result :use-justification) :to-equal "fair_use")))

  (it "passes through usage-license unchanged"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license "cc_by_sa"
                     :copyright nil))))
      (expect (plist-get result :usage-license) :to-equal "cc_by_sa")))

  (it "passes through copyright unchanged"
    (let ((result (org-canvas--file-transform-props
                   '(:display-name "Test" :local-path "/tmp/test.pdf"
                     :folder-path "" :canvas-id nil
                     :published-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :use-justification nil :usage-license nil
                     :copyright "2024 University"))))
      (expect (plist-get result :copyright) :to-equal "2024 University"))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--file-parse-entry"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas-files-file "/tmp/test-files.org"))

  (it "returns nil for folder-only headings"
    (with-temp-org-buffer
     "* Just a folder
"
     (org-back-to-heading)
     (expect (org-canvas--file-parse-entry) :to-be nil)))

  (it "extracts display-name from file link"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][Test Document]]
:PROPERTIES:
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               (expect (plist-get data :display-name) :to-equal "Test Document"))))
        (delete-file temp-file))))

  (it "extracts canvas-id when present"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][Doc]]
:PROPERTIES:
:CANVAS_ID: 12345
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               (expect (plist-get data :canvas-id) :to-equal "12345"))))
        (delete-file temp-file))))

  (it "returns nil canvas-id for new files"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][New Doc]]
:PROPERTIES:
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               (expect (plist-get data :canvas-id) :to-be nil))))
        (delete-file temp-file))))

  (it "parses published property (default true)"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][Doc]]
:PROPERTIES:
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               (expect (plist-get data :published) :to-be t))))
        (delete-file temp-file))))

  (it "parses published=false"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][Doc]]
:PROPERTIES:
:PUBLISHED: false
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               (expect (plist-get data :published) :to-be nil))))
        (delete-file temp-file))))

  (it "includes pom in returned data"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][Doc]]
:PROPERTIES:
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               (expect (plist-get data :pom) :to-be-truthy))))
        (delete-file temp-file))))

  (it "includes local-path in returned data"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][Doc]]
:PROPERTIES:
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               (expect (plist-get data :local-path) :to-be-truthy))))
        (delete-file temp-file))))

  (it "includes folder-path in returned data"
    (let ((temp-file (make-temp-file "test" nil ".pdf")))
      (unwind-protect
          (let ((org-canvas-files-file temp-file))
            (with-temp-org-buffer
             (format "* [[file:%s][Doc]]
:PROPERTIES:
:END:
" (file-name-nondirectory temp-file))
             (org-back-to-heading)
             (let ((data (org-canvas--file-parse-entry)))
               ;; Top-level file has empty folder path
               (expect (plist-get data :folder-path) :to-equal ""))))
        (delete-file temp-file))))

  (it "errors when file does not exist"
    (let ((org-canvas-files-file "/tmp/test-files.org"))
      (with-temp-org-buffer
       "* [[file:nonexistent-file.pdf][Missing]]
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (expect (org-canvas--file-parse-entry) :to-throw 'error)))))

;;;; Stage 2: Build Upload Request

(describe "org-canvas--file-build-upload-request"
  :var (temp-file)
  (before-each (test-org-canvas-reset-file-caches))


  (before-each
    (setq temp-file (make-temp-file "test-upload" nil ".pdf"))
    (with-temp-file temp-file
      (insert "test content")))

  (after-each
    (when (file-exists-p temp-file)
      (delete-file temp-file)))

  (it "includes file name in payload"
    (let* ((data (list :display-name "My File.pdf"
                       :local-path temp-file
                       :published t))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "name" payload) :to-equal "My File.pdf")))

  (it "includes content type based on extension"
    (let* ((data (list :display-name "Document"
                       :local-path temp-file
                       :published t))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "content_type" payload) :to-equal "application/pdf")))

  (it "sets on_duplicate to overwrite"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published t))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "on_duplicate" payload) :to-equal "overwrite")))

  (it "includes file size in payload"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published t))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "size" payload) :to-be-truthy)
      (expect (gethash "size" payload) :to-be-greater-than 0)))

  (it "carries no visibility fields — the preflight discards them"
    ;; Issue #50: api_attachment_preflight reads only name, size,
    ;; content_type, parent_folder_id and on_duplicate.  Anything else was
    ;; being sent into a void; visibility is applied by a later PUT.
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published nil
                       :hidden t
                       :unlock-at "2024-01-15T09:00:00Z"
                       :lock-at "2024-06-01T23:59:00Z"))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "hidden" payload) :to-be nil)
      (expect (gethash "locked" payload) :to-be nil)
      (expect (gethash "unlock_at" payload) :to-be nil)
      (expect (gethash "lock_at" payload) :to-be nil))))

(describe "org-canvas--file-build-settings-payload"
  (it "maps PUBLISHED: false to locked, not hidden"
    ;; Issue #50: `hidden' is the weaker "reachable by direct link" state.
    (let* ((data (list :display-name "Doc" :published nil))
           (payload (org-canvas--file-build-settings-payload data)))
      (expect (gethash "locked" payload) :to-be t)
      (expect (gethash "hidden" payload) :to-equal :json-false)))

  (it "sends locked in both directions so the property is a real toggle"
    ;; The old mapping only ever set the flag, so a file could not be
    ;; republished by editing files.org: omitting a field on a partial
    ;; update leaves the stored value alone.
    (let* ((data (list :display-name "Doc" :published t))
           (payload (org-canvas--file-build-settings-payload data)))
      (expect (gethash "locked" payload) :to-equal :json-false)))

  (it "exposes the unlisted state through its own property"
    (let* ((data (list :display-name "Doc" :published t :hidden t))
           (payload (org-canvas--file-build-settings-payload data)))
      (expect (gethash "locked" payload) :to-equal :json-false)
      (expect (gethash "hidden" payload) :to-be t)))

  (it "includes unlock_at when specified"
    (let* ((data (list :display-name "Doc" :published t
                       :unlock-at "2024-01-15T09:00:00Z"))
           (payload (org-canvas--file-build-settings-payload data)))
      (expect (gethash "unlock_at" payload) :to-equal "2024-01-15T09:00:00Z")))

  (it "includes lock_at when specified"
    (let* ((data (list :display-name "Doc" :published t
                       :lock-at "2024-06-01T23:59:00Z"))
           (payload (org-canvas--file-build-settings-payload data)))
      (expect (gethash "lock_at" payload) :to-equal "2024-06-01T23:59:00Z")))

  (it "excludes unlock_at when not specified"
    (let* ((data (list :display-name "Doc" :published t))
           (payload (org-canvas--file-build-settings-payload data)))
      (expect (gethash "unlock_at" payload) :to-be nil)))

  (it "excludes lock_at when not specified"
    (let* ((data (list :display-name "Doc" :published t))
           (payload (org-canvas--file-build-settings-payload data)))
      (expect (gethash "lock_at" payload) :to-be nil))))

(describe "org-canvas--file-apply-settings"
  (it "PUTs the settings to the file endpoint"
    (let (calls)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest args)
                   (push (list method url (plist-get args :data)) calls)
                   '((id . 42)))))
        (org-canvas--file-apply-settings 42 '(:display-name "Doc" :published nil))
        (let ((call (car calls)))
          (expect (nth 0 call) :to-equal 'PUT)
          (expect (nth 1 call) :to-match "/api/v1/files/42")
          (expect (gethash "locked" (nth 2 call)) :to-be t)))))

  (it "sends nothing during a dry run"
    (let ((org-canvas--dry-run t))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Must not contact the API"))))
        (expect (org-canvas--file-apply-settings 42 '(:display-name "Doc" :published nil))
                :to-be org-canvas--dry-run-response)))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--file-upload-step1-notify (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "posts to folder files endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/456/files" . ((upload_url . "https://upload.example.com")
                                        (upload_params . ((key . "value")))))))
        (let ((payload (make-hash-table :test 'equal)))
          (puthash "name" "test.pdf" payload)
          (org-canvas--file-upload-step1-notify 456 payload)
          (expect-api-called 'POST "folders/456/files")))))

  (it "returns upload info with upload_url"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders" . ((upload_url . "https://s3.example.com/upload")
                              (upload_params . ((token . "abc123")))))))
        (let ((payload (make-hash-table :test 'equal)))
          (puthash "name" "test.pdf" payload)
          (let ((response (org-canvas--file-upload-step1-notify 123 payload)))
            (expect (alist-get 'upload_url response)
                    :to-equal "https://s3.example.com/upload")))))))

(describe "org-canvas--file-upload-step3-confirm"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns response directly if it contains id"
    (let ((step2-response '((id . 99999) (display_name . "test.pdf"))))
      (expect (alist-get 'id (org-canvas--file-upload-step3-confirm step2-response))
              :to-equal 99999)))

  (it "follows location URL when no id present"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files/confirmation" . ((id . 88888) (display_name . "uploaded.pdf")))))
        (let ((step2-response '((location . "https://test.canvas.example.com/api/v1/files/confirmation"))))
          (let ((result (org-canvas--file-upload-step3-confirm step2-response)))
            (expect (alist-get 'id result) :to-equal 88888))))))

  (it "handles relative location URLs"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("" . ((id . 77777) (display_name . "file.pdf")))))
        (let ((step2-response '((location . "/api/v1/files/77777"))))
          (let ((result (org-canvas--file-upload-step3-confirm step2-response)))
            (expect (alist-get 'id result) :to-equal 77777))))))

  (it "errors when no id or location in response"
    (let ((step2-response '((error . "something wrong"))))
      (expect (org-canvas--file-upload-step3-confirm step2-response)
              :to-throw 'error))))

(describe "org-canvas--file-search-by-name (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "searches files endpoint with search_term"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [((id . 111) (display_name . "Lab 1.pdf"))])))
        (org-canvas--file-search-by-name "Lab 1.pdf" "")
        (expect-api-called 'GET "files"))))

  (it "returns matching file object"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [((id . 222) (display_name . "Syllabus.pdf"))
                            ((id . 333) (display_name . "Other.pdf"))])))
        (let ((result (org-canvas--file-search-by-name "Syllabus.pdf" "")))
          (expect (alist-get 'id result) :to-equal 222)))))

  (it "returns nil when file not found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [])))
        (let ((result (org-canvas--file-search-by-name "Missing.pdf" "")))
          (expect result :to-be nil)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error")))))
        (let ((result (org-canvas--file-search-by-name "Any.pdf" "")))
          (expect result :to-be nil))))))

(describe "org-canvas--file-push-to-api (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil)
    (setq org-canvas--file-folder-cache (make-hash-table :test 'equal)))

  (it "deletes existing file before re-upload when canvas-id present"
    (with-org-canvas-test-config
      (let ((delete-called nil)
            (call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; DELETE call for existing file
                      ((eq method 'DELETE)
                       (setq delete-called t)
                       nil)
                      ;; GET for root folder
                      ((and (eq method 'GET) (string-match "folders/root" url))
                       '((id . 100) (name . "course files")))
                      ;; POST for upload step 1
                      ((eq method 'POST)
                       '((upload_url . "https://upload.example.com")
                         (upload_params . ((key . "value")))))
                      (t nil))))
                  ;; Mock the upload step 2
                  ((symbol-function 'org-canvas--file-upload-step2-send)
                   (lambda (_info _path) '((id . 999))))
                  ;; Mock step 3
                  ((symbol-function 'org-canvas--file-upload-step3-confirm)
                   (lambda (resp) resp)))
          (let ((temp-file (make-temp-file "test" nil ".pdf")))
            (unwind-protect
                (progn
                  (with-temp-file temp-file (insert "content"))
                  (let ((data (list :canvas-id "123"
                                    :display-name "Test.pdf"
                                    :local-path temp-file
                                    :folder-path "")))
                    (org-canvas--file-push-to-api data)
                    (expect delete-called :to-be t)))
              (delete-file temp-file)))))))

  (it "uses root folder when folder-path is empty"
    (with-org-canvas-test-config
      (let ((root-folder-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((and (eq method 'GET) (string-match "folders/root" url))
                       (setq root-folder-called t)
                       '((id . 100) (name . "course files")))
                      ((eq method 'POST)
                       '((upload_url . "https://upload.example.com")
                         (upload_params . nil)))
                      (t nil))))
                  ((symbol-function 'org-canvas--file-upload-step2-send)
                   (lambda (_info _path) '((id . 888))))
                  ((symbol-function 'org-canvas--file-upload-step3-confirm)
                   (lambda (resp) resp)))
          (let ((temp-file (make-temp-file "test" nil ".pdf")))
            (unwind-protect
                (progn
                  (with-temp-file temp-file (insert "content"))
                  (let ((data (list :display-name "Test.pdf"
                                    :local-path temp-file
                                    :folder-path "")))
                    (org-canvas--file-push-to-api data)
                    (expect root-folder-called :to-be t)))
              (delete-file temp-file)))))))

  (it "resolves folder path when folder-path is non-empty"
    (with-org-canvas-test-config
      (let ((resolve-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((and (eq method 'GET) (string-match "folders/root" url))
                       '((id . 100) (name . "course files")))
                      ((eq method 'POST)
                       '((upload_url . "https://upload.example.com")
                         (upload_params . nil)))
                      (t nil))))
                  ((symbol-function 'org-canvas--file-resolve-folder-by-path)
                   (lambda (path)
                     (setq resolve-called path)
                     '((id . 200) (name . "Labs"))))
                  ((symbol-function 'org-canvas--file-upload-step2-send)
                   (lambda (_info _path) '((id . 777))))
                  ((symbol-function 'org-canvas--file-upload-step3-confirm)
                   (lambda (resp) resp)))
          (let ((temp-file (make-temp-file "test" nil ".pdf")))
            (unwind-protect
                (progn
                  (with-temp-file temp-file (insert "content"))
                  (let ((data (list :display-name "Lab1.pdf"
                                    :local-path temp-file
                                    :folder-path "Labs")))
                    (org-canvas--file-push-to-api data)
                    (expect resolve-called :to-equal "Labs")))
              (delete-file temp-file)))))))

  (it "signals error when upload fails"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest _args)
                   (cond
                    ((and (eq method 'GET) (string-match "folders/root" url))
                     '((id . 100)))
                    ((eq method 'POST)
                     (signal 'error '("Upload failed")))
                    (t nil)))))
        (let ((temp-file (make-temp-file "test" nil ".pdf")))
          (unwind-protect
              (progn
                (with-temp-file temp-file (insert "content"))
                (let ((data (list :display-name "Test.pdf"
                                  :local-path temp-file
                                  :folder-path "")))
                  (expect (org-canvas--file-push-to-api data) :to-throw 'error)))
            (delete-file temp-file)))))))

;;;; Stage 4: Finalize

(describe "org-canvas--file-finalize"
  (before-each (test-org-canvas-reset-file-caches))

  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* [[file:doc.pdf][Document]]
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :display-name "Document" :pom (point-marker)))
           (response '((id . 55555) (display_name . "Document"))))
       (org-canvas--file-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "55555"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* [[file:doc.pdf][Document]]
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :display-name "Document" :pom (point-marker)))
           (response '((id . 66666))))
       (org-canvas--file-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))

  (it "signals error when response lacks id"
    (with-temp-org-buffer
     "* [[file:doc.pdf][Document]]
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :display-name "Document" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (expect (org-canvas--file-finalize data response)
               :to-throw 'error)))))

;;;; Folder Operations (mocked)

(describe "org-canvas--file-get-root-folder (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil))

  (it "fetches root folder from API"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/root" . ((id . 1001) (name . "course files")))))
        (let ((result (org-canvas--file-get-root-folder)))
          (expect (alist-get 'id result) :to-equal 1001)
          (expect-api-called 'GET "folders/root")))))

  (it "caches root folder for subsequent calls"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/root" . ((id . 1002) (name . "course files")))))
        (org-canvas--file-get-root-folder)
        (org-canvas--file-get-root-folder)
        ;; Should only call API once due to caching
        (expect (test-org-canvas-api-call-count) :to-equal 1)))))

(describe "org-canvas--file-ensure-subfolder (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns existing folder when found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/100/folders" . [((id . 200) (name . "Labs"))
                                          ((id . 201) (name . "Homework"))])))
        (let ((parent '((id . 100)))
              (result (org-canvas--file-ensure-subfolder '((id . 100)) "Labs")))
          (expect (alist-get 'id result) :to-equal 200)))))

  (it "creates folder when not found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/100/folders" . [])
                ("" . ((id . 999) (name . "NewFolder")))))
        (let ((result (org-canvas--file-ensure-subfolder '((id . 100)) "NewFolder")))
          ;; Should have made POST call to create folder
          (expect-api-called 'POST "folders/100/folders"))))))

(describe "org-canvas--file-resolve-folder-by-path (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-folder-cache (make-hash-table :test 'equal))
    (setq org-canvas--file-root-folder-cache nil))

  (it "uses cached folder when available"
    (with-org-canvas-test-config
      (puthash "Labs" '((id . 200) (name . "Labs")) org-canvas--file-folder-cache)
      (let ((api-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq api-called t)
                     nil)))
          (let ((result (org-canvas--file-resolve-folder-by-path "Labs")))
            (expect (alist-get 'id result) :to-equal 200)
            (expect api-called :to-be nil))))))

  (it "resolves multi-level paths"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; Root folder
                      ((string-match "folders/root" url)
                       '((id . 100) (name . "course files")))
                      ;; Get subfolders of root
                      ((string-match "folders/100/folders" url)
                       (if (eq method 'GET)
                           [((id . 200) (name . "Materials"))]
                         '((id . 200) (name . "Materials"))))
                      ;; Get subfolders of Materials
                      ((string-match "folders/200/folders" url)
                       (if (eq method 'GET)
                           [((id . 300) (name . "Week 01"))]
                         '((id . 300) (name . "Week 01"))))
                      (t nil)))))
          (let ((result (org-canvas--file-resolve-folder-by-path "Materials/Week 01")))
            (expect (alist-get 'id result) :to-equal 300))))))

  (it "caches intermediate folders"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest _args)
                   (cond
                    ((string-match "folders/root" url)
                     '((id . 100) (name . "course files")))
                    ((string-match "folders/100/folders" url)
                     (if (eq method 'GET)
                         [((id . 200) (name . "Labs"))]
                       '((id . 200) (name . "Labs"))))
                    (t nil)))))
        (org-canvas--file-resolve-folder-by-path "Labs")
        ;; Check that the folder was cached
        (expect (gethash "Labs" org-canvas--file-folder-cache) :to-be-truthy)
        (expect (alist-get 'id (gethash "Labs" org-canvas--file-folder-cache)) :to-equal 200)))))

(describe "org-canvas--file-get-or-create-folder (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns parent folder directly for empty path"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/100" . ((id . 100) (name . "root")))))
        (let ((result (org-canvas--file-get-or-create-folder "" 100)))
          (expect (alist-get 'id result) :to-equal 100)))))

  (it "returns parent folder directly for nil path"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/100" . ((id . 100) (name . "root")))))
        (let ((result (org-canvas--file-get-or-create-folder nil 100)))
          (expect (alist-get 'id result) :to-equal 100)))))

  (it "resolves existing folder by path"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders/by_path" . [((id . 200) (name . "Labs"))])))
        (let ((result (org-canvas--file-get-or-create-folder "Labs" 100)))
          (expect (alist-get 'id result) :to-equal 200))))))

(describe "org-canvas--file-create-folder (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "creates folder with correct payload"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders" . ((id . 999) (name . "NewFolder")))))
        (let ((result (org-canvas--file-create-folder "Parent/NewFolder" 100)))
          (expect-api-called 'POST "folders")
          (expect (alist-get 'id result) :to-equal 999)))))

  (it "falls back to resolve when creation fails"
    (with-org-canvas-test-config
      ;; State the precondition locally.  The fallback resolves the path
      ;; from the root folder, and this spec used to rely on an earlier
      ;; spec having left the root cached — passing for a reason that had
      ;; nothing to do with it (issue #43).
      (setq org-canvas--file-root-folder-cache
            '((id . 100) (name . "course files")))
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that fails (folder exists)
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Folder already exists")))
                      ;; Second call: resolve by path
                      ((eq method 'GET)
                       [((id . 888) (name . "ExistingFolder"))])
                      (t nil)))))
          (let ((result (org-canvas--file-create-folder "ExistingFolder" 100)))
            (expect (alist-get 'id result) :to-equal 888)))))))

;;;; Pre-flight Operations

(describe "org-canvas--file-collect-folder-paths"
  (before-each (test-org-canvas-reset-file-caches))

  (it "collects unique folder paths from file"
    (let ((temp-file (make-temp-file "files" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Labs
** [[file:lab1.pdf][Lab 1]]
** [[file:lab2.pdf][Lab 2]]
* Homework
** [[file:hw1.pdf][HW 1]]
"))
            (let ((paths (org-canvas--file-collect-folder-paths temp-file)))
              (expect (length paths) :to-equal 2)
              (expect paths :to-contain "Labs")
              (expect paths :to-contain "Homework")))
        (delete-file temp-file))))

  (it "handles nested folder paths"
    (let ((temp-file (make-temp-file "files" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Materials
** Week 01
*** [[file:notes.pdf][Notes]]
"))
            (let ((paths (org-canvas--file-collect-folder-paths temp-file)))
              (expect paths :to-contain "Materials/Week 01")))
        (delete-file temp-file))))

  (it "excludes root level files"
    (let ((temp-file (make-temp-file "files" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* [[file:syllabus.pdf][Syllabus]]
* Folder
** [[file:nested.pdf][Nested]]
"))
            (let ((paths (org-canvas--file-collect-folder-paths temp-file)))
              ;; Only "Folder" should be collected, not empty string for root
              (expect paths :to-equal '("Folder"))))
        (delete-file temp-file))))

  (it "returns empty list for file with no folders"
    (let ((temp-file (make-temp-file "files" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* [[file:doc1.pdf][Doc 1]]
* [[file:doc2.pdf][Doc 2]]
"))
            (let ((paths (org-canvas--file-collect-folder-paths temp-file)))
              (expect paths :to-equal '())))
        (delete-file temp-file)))))

;;;; Ensure Folders Exist

(describe "org-canvas--file-ensure-folders-exist (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "does nothing when folder-paths is nil"
    (with-org-canvas-test-config
      (let ((api-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq api-called t)
                     nil)))
          (org-canvas--file-ensure-folders-exist nil)
          (expect api-called :to-be nil)))))

  (it "does nothing when folder-paths is empty"
    (with-org-canvas-test-config
      (let ((api-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq api-called t)
                     nil)))
          (org-canvas--file-ensure-folders-exist '())
          (expect api-called :to-be nil)))))

  (it "creates each folder in the list"
    (with-org-canvas-test-config
      (let ((resolved-paths nil))
        (cl-letf (((symbol-function 'org-canvas--file-resolve-folder-by-path)
                   (lambda (path)
                     (push path resolved-paths)
                     '((id . 100) (name . "folder"))))
                  ((symbol-function 'sleep-for)
                   (lambda (_sec) nil)))
          (org-canvas--file-ensure-folders-exist '("Labs" "Homework"))
          (expect (length resolved-paths) :to-equal 2)
          (expect resolved-paths :to-contain "Labs")
          (expect resolved-paths :to-contain "Homework")))))

  (it "signals error when folder creation fails"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--file-resolve-folder-by-path)
                 (lambda (_path)
                   (signal 'error '("Cannot create folder")))))
        (expect (org-canvas--file-ensure-folders-exist '("BadFolder"))
                :to-throw 'error)))))

;;;; Delete Functions

(describe "org-canvas-delete-file-at-point"
  (before-each (test-org-canvas-reset-file-caches))

  (it "errors when no CANVAS_ID property found"
    (with-temp-org-buffer
     "* [[file:doc.pdf][Document]]
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-file-at-point)
             :to-throw 'user-error))))

(describe "org-canvas--file-get-all-folders (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil))

  (it "excludes root folder from results"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders$" . [((id . 100) (name . "root") (full_name . "course files"))
                               ((id . 200) (name . "Labs") (full_name . "course files/Labs"))])
                ("folders/root" . ((id . 100) (name . "course files")))))
        (let ((result (org-canvas--file-get-all-folders)))
          (expect (length result) :to-equal 1)
          (expect (alist-get 'id (car result)) :to-equal 200)))))

  (it "sorts folders by depth (deepest first)"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("folders$" . [((id . 100) (full_name . "course files"))
                               ((id . 200) (full_name . "course files/A"))
                               ((id . 300) (full_name . "course files/A/B"))
                               ((id . 400) (full_name . "course files/A/B/C"))])
                ("folders/root" . ((id . 100) (name . "course files")))))
        (let ((result (org-canvas--file-get-all-folders)))
          ;; Deepest folder should be first
          (expect (alist-get 'id (car result)) :to-equal 400))))))

(describe "org-canvas--file-delete-all-folders (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil))

  (it "deletes all folders and returns count"
    (with-org-canvas-test-config
      (let ((delete-count 0))
        (cl-letf (((symbol-function 'org-canvas--file-get-all-folders)
                   (lambda ()
                     '(((id . 300) (full_name . "course files/A/B"))
                       ((id . 200) (full_name . "course files/A")))))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (when (eq method 'DELETE)
                       (setq delete-count (1+ delete-count)))
                     nil)))
          (let ((result (org-canvas--file-delete-all-folders)))
            (expect result :to-equal 2)
            (expect delete-count :to-equal 2))))))

  (it "continues on delete errors"
    (with-org-canvas-test-config
      (let ((delete-attempts 0))
        (cl-letf (((symbol-function 'org-canvas--file-get-all-folders)
                   (lambda ()
                     '(((id . 200) (full_name . "A"))
                       ((id . 300) (full_name . "B")))))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq delete-attempts (1+ delete-attempts))
                     (when (eq method 'DELETE)
                       (if (= delete-attempts 1)
                           (signal 'error '("Cannot delete"))
                         nil)))))
          (let ((result (org-canvas--file-delete-all-folders)))
            ;; Should have attempted both, succeeded on one
            (expect delete-attempts :to-equal 2)
            (expect result :to-equal 1)))))))

;;;; Delete File at Point

(describe "org-canvas-delete-file-at-point (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "deletes file and clears properties when confirmed"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* [[file:doc.pdf][My Document]]
:PROPERTIES:
:CANVAS_ID: 54321
:LAST_SYNCED: [2024-01-15]
:END:
"
       (org-back-to-heading)
       (let ((delete-called nil))
         (with-sync-test-env
           (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                     ((symbol-function 'org-canvas-api-request)
                      (lambda (method _url &rest _args)
                        (when (eq method 'DELETE)
                          (setq delete-called t))
                        nil)))
             (org-canvas-delete-file-at-point)
             (expect delete-called :to-be t)
             ;; Properties should be cleared
             (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))))))

  (it "does not delete when user declines"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* [[file:doc.pdf][Document]]
:PROPERTIES:
:CANVAS_ID: 11111
:END:
"
       (org-back-to-heading)
       (let ((delete-called nil))
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
                   ((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (when (eq method 'DELETE)
                        (setq delete-called t))
                      nil)))
           (org-canvas-delete-file-at-point)
           (expect delete-called :to-be nil)
           ;; Properties should remain
           (expect (org-entry-get (point) "CANVAS_ID") :to-equal "11111"))))))

  (it "handles API errors gracefully"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* [[file:doc.pdf][Doc]]
:PROPERTIES:
:CANVAS_ID: 22222
:END:
"
       (org-back-to-heading)
       (with-sync-test-env
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                   ((symbol-function 'org-canvas-api-request)
                    (lambda (_method _url &rest _args)
                      (signal 'error '("API error")))))
           ;; Should not throw, should handle gracefully
           (org-canvas-delete-file-at-point)
           ;; Properties should still be there (delete failed)
           (expect (org-entry-get (point) "CANVAS_ID") :to-equal "22222")))))))

;;;; Sync Files Pipeline

(describe "org-canvas-sync-files (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil)
    (setq org-canvas--file-folder-cache nil))

  (it "errors when files-file doesn't exist"
    (let ((org-canvas-files-file "/nonexistent/files.org"))
      (expect (org-canvas-sync-files) :to-throw 'error)))

  (it "processes files and updates CANVAS_ID"
    (let ((temp-dir (make-temp-file "files-test" t))
          (files-processed nil))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (pdf-file (expand-file-name "test.pdf" temp-dir)))
            ;; Create test file
            (with-temp-file pdf-file (insert "PDF content"))
            ;; Create org file
            (with-temp-file org-file
              (insert (format "* [[file:%s][Test PDF]]
:PROPERTIES:
:END:
" (file-name-nondirectory pdf-file))))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ;; Course access check
                              ((string-match "courses/99999$" url) '((id . 99999)))
                              ;; Root folder
                              ((string-match "folders/root" url)
                               '((id . 100) (name . "course files")))
                              ;; Upload step 1
                              ((and (eq method 'POST) (string-match "folders/100/files" url))
                               '((upload_url . "https://s3.example.com/upload")
                                 (upload_params . ((key . "abc123")))))
                              (t nil))))
                          ((symbol-function 'org-canvas--file-upload-step2-send)
                           (lambda (_info _path)
                             (push "uploaded" files-processed)
                             '((id . 77777) (display_name . "test.pdf"))))
                          ((symbol-function 'org-canvas--file-upload-step3-confirm)
                           (lambda (resp) resp)))
                  (org-canvas-sync-files)
                  (expect files-processed :to-contain "uploaded")
                  ;; Check that CANVAS_ID was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_ID") :to-equal "77777"))))))
        (delete-directory temp-dir t))))

  (it "skips folder headings"
    (let ((temp-dir (make-temp-file "files-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (pdf-file (expand-file-name "test.pdf" temp-dir))
                 (upload-count 0))
            (with-temp-file pdf-file (insert "content"))
            (with-temp-file org-file
              (insert (format "* Folder Only
* [[file:%s][File]]
:PROPERTIES:
:END:
" (file-name-nondirectory pdf-file))))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method url &rest _args)
                             (cond
                              ((string-match "courses/99999$" url) '((id . 99999)))
                              ((string-match "folders/root" url) '((id . 100)))
                              (t nil))))
                          ((symbol-function 'org-canvas--file-push-to-api)
                           (lambda (_data)
                             (setq upload-count (1+ upload-count))
                             '((id . 88888))))
                          ((symbol-function 'org-canvas--file-apply-settings)
                           (lambda (_fid _data) nil))
                          ((symbol-function 'org-canvas--file-finalize)
                           (lambda (_data _resp) nil)))
                  (org-canvas-sync-files)
                  ;; Only 1 file should be uploaded (folder is skipped)
                  (expect upload-count :to-equal 1)))))
        (delete-directory temp-dir t))))

  (it "continues processing after individual file errors"
    (let ((temp-dir (make-temp-file "files-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (pdf1 (expand-file-name "test1.pdf" temp-dir))
                 (pdf2 (expand-file-name "test2.pdf" temp-dir))
                 (push-attempts 0))
            (with-temp-file pdf1 (insert "content1"))
            (with-temp-file pdf2 (insert "content2"))
            (with-temp-file org-file
              (insert (format "* [[file:%s][File 1]]
:PROPERTIES:
:END:
* [[file:%s][File 2]]
:PROPERTIES:
:END:
" (file-name-nondirectory pdf1) (file-name-nondirectory pdf2))))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method url &rest _args)
                             (cond
                              ((string-match "courses/99999$" url) '((id . 99999)))
                              ((string-match "folders/root" url) '((id . 100)))
                              (t nil))))
                          ((symbol-function 'org-canvas--file-push-to-api)
                           (lambda (_data)
                             (setq push-attempts (1+ push-attempts))
                             (if (= push-attempts 1)
                                 (signal 'error '("First file failed"))
                               '((id . 99999)))))
                          ((symbol-function 'org-canvas--file-apply-settings)
                           (lambda (_fid _data) nil))
                          ((symbol-function 'org-canvas--file-finalize)
                           (lambda (_data _resp) nil)))
                  (org-canvas-sync-files)
                  ;; Both files should be attempted
                  (expect push-attempts :to-equal 2)))))
        (delete-directory temp-dir t)))))

;;;; Delete All Files

(describe "org-canvas-delete-all-files (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil)
    (setq org-canvas--file-folder-cache nil))

  (it "aborts when user declines confirmation"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (expect (org-canvas-delete-all-files) :to-throw 'user-error)))

  (it "deletes all files and folders when confirmed"
    (let ((temp-dir (make-temp-file "files-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (deleted-files nil)
                 (deleted-folders 0))
            (with-temp-file org-file
              (insert "* [[file:test.pdf][Test]]
:PROPERTIES:
:CANVAS_ID: 12345
:END:
"))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ;; List files
                              ((and (eq method 'GET) (string-match "courses/.*/files$" url))
                               [((id . 111) (display_name . "file1.pdf"))
                                ((id . 222) (display_name . "file2.pdf"))])
                              ;; Delete file
                              ((and (eq method 'DELETE) (string-match "/files/" url))
                               (push url deleted-files)
                               nil)
                              (t nil))))
                          ((symbol-function 'org-canvas--file-delete-all-folders)
                           (lambda ()
                             (setq deleted-folders 3)
                             3))
                          ((symbol-function 'org-canvas-clear-sync-properties)
                           (lambda (_pom) nil)))
                  (org-canvas-delete-all-files)
                  (expect (length deleted-files) :to-equal 2)
                  (expect deleted-folders :to-equal 3)))))
        (delete-directory temp-dir t))))

  (it "clears local CANVAS_ID properties for deleted files"
    (let ((temp-dir (make-temp-file "files-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (cleared-ids nil))
            (with-temp-file org-file
              (insert "* [[file:file1.pdf][File 1]]
:PROPERTIES:
:CANVAS_ID: 111
:LAST_SYNCED: [2024-01-01]
:END:
* [[file:file2.pdf][File 2]]
:PROPERTIES:
:CANVAS_ID: 222
:END:
"))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((and (eq method 'GET) (string-match "/files$" url))
                               [((id . 111) (display_name . "file1.pdf"))])
                              ((eq method 'DELETE) nil)
                              (t nil))))
                          ((symbol-function 'org-canvas--file-delete-all-folders)
                           (lambda () 0))
                          ((symbol-function 'org-canvas-clear-sync-properties)
                           (lambda (pom)
                             (push (org-entry-get pom "CANVAS_ID") cleared-ids))))
                  (org-canvas-delete-all-files)
                  ;; All files with CANVAS_ID should be cleared
                  (expect cleared-ids :to-contain "111")
                  (expect cleared-ids :to-contain "222")))))
        (delete-directory temp-dir t))))

  (it "continues on individual file delete errors"
    (let ((temp-dir (make-temp-file "files-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (delete-attempts 0))
            (with-temp-file org-file (insert ""))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((and (eq method 'GET) (string-match "/files$" url))
                               [((id . 1) (display_name . "a.pdf"))
                                ((id . 2) (display_name . "b.pdf"))])
                              ((eq method 'DELETE)
                               (setq delete-attempts (1+ delete-attempts))
                               (when (= delete-attempts 1)
                                 (signal 'error '("Delete failed")))
                               nil)
                              (t nil))))
                          ((symbol-function 'org-canvas--file-delete-all-folders)
                           (lambda () 0)))
                  (org-canvas-delete-all-files)
                  ;; Both should be attempted
                  (expect delete-attempts :to-equal 2)))))
        (delete-directory temp-dir t))))

  (it "deletes files that span multiple pages"
    ;; Regression: the previous implementation hard-coded ?per_page=100&page=1,
    ;; missing files beyond the first 100.
    (let ((temp-dir (make-temp-file "files-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (deleted-ids nil)
                 (get-calls 0))
            (with-temp-file org-file (insert ""))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((and (eq method 'GET) (string-match "/files$" url))
                               (cl-incf get-calls)
                               (pcase get-calls
                                 (1 (vconcat
                                     (cl-loop for i from 1 to 100
                                              collect `((id . ,i)
                                                        (display_name
                                                         . ,(format "f%d.pdf" i))))))
                                 (2 (vector '((id . 555) (display_name . "page2-only.pdf"))))
                                 (_ (vector))))
                              ((eq method 'DELETE)
                               (when (string-match "/files/\\([0-9]+\\)" url)
                                 (push (string-to-number (match-string 1 url))
                                       deleted-ids))
                               nil)
                              (t nil))))
                          ((symbol-function 'org-canvas--file-delete-all-folders)
                           (lambda () 0)))
                  (org-canvas-delete-all-files)
                  (expect get-calls :to-equal 2)
                  (expect (length deleted-ids) :to-equal 101)
                  (expect (memq 555 deleted-ids) :to-be-truthy)))))
        (delete-directory temp-dir t)))))

;;;; Upload Step 2 Edge Cases

(describe "org-canvas--file-upload-step2-send edge cases"
  (before-each (test-org-canvas-reset-file-caches))

  (it "handles upload_params with null filename"
    ;; This tests the fix for Canvas returning null filename
    (let* ((temp-file (make-temp-file "upload-test" nil ".pdf"))
           (upload-info '((upload_url . "https://s3.example.com/upload")
                          (upload_params . ((key . "abc123")
                                            (filename . nil)
                                            (content_type . "unknown/unknown")))))
           (sent-body nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PDF content"))
            (cl-letf (((symbol-function 'url-retrieve-synchronously)
                       (lambda (_url &rest _args)
                         ;; Create a mock response buffer
                         (let ((buf (generate-new-buffer "*mock-response*")))
                           (with-current-buffer buf
                             (insert "HTTP/1.1 200 OK\r\n")
                             (insert "Content-Type: application/json\r\n\r\n")
                             (insert "{\"id\": 12345, \"display_name\": \"test.pdf\"}"))
                           buf))))
              (let ((result (org-canvas--file-upload-step2-send upload-info temp-file)))
                ;; Should return the file object
                (expect (alist-get 'id result) :to-equal 12345))))
        (delete-file temp-file))))

  (it "handles redirect with Location header"
    (let* ((temp-file (make-temp-file "upload-test" nil ".pdf"))
           (upload-info '((upload_url . "https://s3.example.com/upload")
                          (upload_params . ((key . "abc"))))))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "content"))
            (cl-letf (((symbol-function 'url-retrieve-synchronously)
                       (lambda (_url &rest _args)
                         (let ((buf (generate-new-buffer "*mock-response*")))
                           (with-current-buffer buf
                             (insert "HTTP/1.1 301 Moved Permanently\r\n")
                             (insert "Location: https://canvas.example.com/api/v1/files/99999\r\n\r\n"))
                           buf))))
              (let ((result (org-canvas--file-upload-step2-send upload-info temp-file)))
                (expect (alist-get 'location result)
                        :to-equal "https://canvas.example.com/api/v1/files/99999"))))
        (delete-file temp-file))))

  (it "throws error when no JSON or Location in response"
    (let* ((temp-file (make-temp-file "upload-test" nil ".pdf"))
           (upload-info '((upload_url . "https://s3.example.com/upload")
                          (upload_params . ((key . "abc"))))))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "content"))
            (cl-letf (((symbol-function 'url-retrieve-synchronously)
                       (lambda (_url &rest _args)
                         (let ((buf (generate-new-buffer "*mock-response*")))
                           (with-current-buffer buf
                             (insert "HTTP/1.1 500 Internal Server Error\r\n\r\n")
                             (insert "Something went wrong"))
                           buf))))
              (expect (org-canvas--file-upload-step2-send upload-info temp-file)
                      :to-throw 'error)))
        (delete-file temp-file))))

  (it "errors when upload_url is nil"
    (let ((upload-info '((upload_params . ((key . "abc"))))))
      (expect (org-canvas--file-upload-step2-send upload-info "/tmp/fake.pdf")
              :to-throw 'error))))

;;;; Folder Path Resolution Edge Cases

(describe "org-canvas--file-get-or-create-folder edge cases"
  (before-each (test-org-canvas-reset-file-caches))

  (it "handles API error by creating folder"
    (with-org-canvas-test-config
      (let ((create-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ;; by_path lookup fails
                      ((string-match "by_path" url)
                       (signal 'error '("Not found")))
                      ;; folder creation succeeds
                      ((eq method 'POST)
                       (setq create-called t)
                       '((id . 999) (name . "NewFolder")))
                      (t nil))))
                  ((symbol-function 'org-canvas--file-create-folder)
                   (lambda (_path _parent)
                     (setq create-called t)
                     '((id . 999)))))
          (let ((result (org-canvas--file-get-or-create-folder "NewFolder" 100)))
            (expect create-called :to-be t)))))))

(describe "org-canvas--file-ensure-subfolder"
  (before-each (test-org-canvas-reset-file-caches))

  (it "creates subfolder when not found in existing list"
    (with-org-canvas-test-config
      (let ((create-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ;; GET returns empty list
                      ((eq method 'GET)
                       [])
                      ;; POST creates the folder
                      ((eq method 'POST)
                       (setq create-called t)
                       '((id . 500) (name . "NewSub")))
                      (t nil)))))
          (let ((result (org-canvas--file-ensure-subfolder '((id . 100)) "NewSub")))
            (expect create-called :to-be t)
            (expect (alist-get 'id result) :to-equal 500)))))))

;;;; Pre-flight Folder Operations

(describe "org-canvas--file-collect-folder-paths edge cases"
  (before-each (test-org-canvas-reset-file-caches))

  (it "handles file with only root-level files"
    (let ((temp-file (make-temp-file "files" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* [[file:a.pdf][A]]
* [[file:b.pdf][B]]
"))
            (let ((paths (org-canvas--file-collect-folder-paths temp-file)))
              (expect paths :to-equal '())))
        (delete-file temp-file))))

  (it "handles deeply nested structure"
    (let ((temp-file (make-temp-file "files" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Level1
** Level2
*** Level3
**** [[file:deep.pdf][Deep File]]
"))
            (let ((paths (org-canvas--file-collect-folder-paths temp-file)))
              (expect paths :to-contain "Level1/Level2/Level3")))
        (delete-file temp-file))))

  (it "deduplicates folder paths"
    (let ((temp-file (make-temp-file "files" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Folder
** [[file:a.pdf][A]]
** [[file:b.pdf][B]]
** [[file:c.pdf][C]]
"))
            (let ((paths (org-canvas--file-collect-folder-paths temp-file)))
              ;; Should only have "Folder" once, not 3 times
              (expect (length paths) :to-equal 1)
              (expect (car paths) :to-equal "Folder")))
        (delete-file temp-file)))))

;;;; Upload Step 3 Confirm

(describe "org-canvas--file-upload-step3-confirm edge cases"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs warning when confirmation response has no ID"
    (with-org-canvas-test-config
      (let ((warning-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     '((error . "something"))))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger &rest _args)
                     (setq warning-logged t))))
          (let ((step2-response '((location . "https://example.com/confirm"))))
            (org-canvas--file-upload-step3-confirm step2-response)
            (expect warning-logged :to-be t)))))))

;;;; Coverage Gap Tests

(describe "org-canvas--file-get-or-create-folder empty list"
  (before-each (test-org-canvas-reset-file-caches))

  (it "falls through to create-folder when API returns empty list"
    (with-org-canvas-test-config
      (let ((create-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     ;; Return empty list for by_path lookup
                     []))
                  ((symbol-function 'org-canvas--file-create-folder)
                   (lambda (path parent-id)
                     (setq create-called (list path parent-id))
                     '((id . 999) (name . "new-folder")))))
          (let ((result (org-canvas--file-get-or-create-folder "Labs" 100)))
            (expect create-called :to-be-truthy)
            (expect (car create-called) :to-equal "Labs")
            (expect (alist-get 'id result) :to-equal 999)))))))

(describe "org-canvas--file-resolve-folder-by-path cached intermediate"
  (before-each (test-org-canvas-reset-file-caches))

  (it "uses cached intermediate folder path"
    (with-org-canvas-test-config
      (let ((ensure-calls nil)
            (org-canvas--file-folder-cache (make-hash-table :test 'equal))
            (org-canvas--file-root-folder-cache '((id . 1) (name . "root"))))
        ;; Pre-populate cache with "Level1"
        (puthash "Level1" '((id . 50) (name . "Level1")) org-canvas--file-folder-cache)
        (cl-letf (((symbol-function 'org-canvas--file-get-root-folder)
                   (lambda () '((id . 1) (name . "root"))))
                  ((symbol-function 'org-canvas--file-ensure-subfolder)
                   (lambda (_parent name)
                     (push name ensure-calls)
                     `((id . 51) (name . ,name)))))
          (org-canvas--file-resolve-folder-by-path "Level1/Level2")
          ;; Should NOT call ensure-subfolder for Level1 (cached)
          (expect ensure-calls :not :to-contain "Level1")
          ;; Should call ensure-subfolder for Level2
          (expect ensure-calls :to-contain "Level2"))))))

(describe "org-canvas--file-parse-entry debug log"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs canvas folder path during parsing"
    (let ((temp-file (make-temp-file "org-test-" nil ".org"))
          (data-file (make-temp-file "upload-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert (format "* [[file:%s][Test File]]\n:PROPERTIES:\n:END:\n" data-file)))
            (let ((org-canvas-files-file temp-file))
              (with-current-buffer (find-file-noselect temp-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--file-parse-entry)))
                  ;; Should parse successfully and include folder-path
                  (expect (plist-get data :display-name) :to-equal "Test File")
                  (expect (plist-get data :folder-path) :to-equal ""))
                (kill-buffer))))
        (delete-file temp-file)
        (delete-file data-file)))))

(describe "org-canvas--file-upload-step2-send JSON without id"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns JSON response when no id field present"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "upload-test-" nil ".txt")))
        (unwind-protect
            (progn
              (with-temp-file temp-file (insert "test content"))
              (let ((upload-info '((upload_url . "https://example.com/upload")
                                   (upload_params . ((key . "abc123"))))))
                (cl-letf (((symbol-function 'url-retrieve-synchronously)
                           (lambda (_url &rest _args)
                             (let ((buf (generate-new-buffer " *test-upload*")))
                               (with-current-buffer buf
                                 (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"status\":\"pending\"}"))
                               buf))))
                  (let ((result (org-canvas--file-upload-step2-send upload-info temp-file)))
                    (expect (alist-get 'status result) :to-equal "pending")
                    ;; No id field in response
                    (expect (alist-get 'id result) :to-be nil)))))
          (delete-file temp-file))))))

(describe "org-canvas--file-push-to-api delete warning"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs warning when delete of old file fails"
    (with-org-canvas-test-config
      (let ((warning-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((eq method 'DELETE)
                       (signal 'error '("Delete failed")))
                      ;; GET for root folder
                      ((and (eq method 'GET) (string-match "root" url))
                       '((id . 1)))
                      ;; GET for folder
                      ((eq method 'GET)
                       '((id . 1) (name . "root")))
                      ;; POST for upload step 1
                      ((eq method 'POST)
                       '((upload_url . "https://up.example.com") (upload_params . nil)))
                      (t nil))))
                  ((symbol-function 'org-canvas--file-upload-step2-send)
                   (lambda (&rest _args) '((id . 999))))
                  ((symbol-function 'org-canvas--file-upload-step3-confirm)
                   (lambda (resp) resp))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (let ((msg (apply #'format fmt args)))
                       (when (string-match-p "Could not delete old file" msg)
                         (setq warning-logged t)))))
                  ((symbol-function 'org-canvas--file-get-root-folder)
                   (lambda () '((id . 1))))
                  ((symbol-function 'org-canvas--file-resolve-folder-by-path)
                   (lambda (_path) '((id . 2)))))
          (let ((data '(:canvas-id "123" :display-name "test.pdf"
                        :local-path "/tmp/test.pdf" :folder-path "")))
            (org-canvas--file-push-to-api data)
            (expect warning-logged :to-be t)))))))

(describe "org-canvas--file-search-by-name folder path log"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs search location with non-empty folder path"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [((display_name . "test.pdf") (id . 100))])))
        ;; Should not error when folder-path is non-empty
        (let ((result (org-canvas--file-search-by-name "test.pdf" "Labs/Week1")))
          (expect (alist-get 'id result) :to-equal 100))))))

(describe "org-canvas-sync-files pre-flight warning"
  (before-each (test-org-canvas-reset-file-caches))

  (it "continues sync when pre-flight check fails"
    (let ((temp-file (make-temp-file "org-test-" nil ".org"))
          (upload-file (make-temp-file "test-upload-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file upload-file (insert "content"))
            (with-temp-file temp-file
              (insert (format "* [[file:%s][Upload File]]\n:PROPERTIES:\n:END:\n" upload-file)))
            (let ((org-canvas-files-file temp-file)
                  (push-called nil))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ;; Pre-flight GET fails
                              ((and (eq method 'GET)
                                    (string-match "courses.*/$" url))
                               (signal 'error '("Connection refused")))
                              (t '((id . 1))))))
                          ((symbol-function 'org-canvas--file-push-to-api)
                           (lambda (data)
                             (setq push-called t)
                             '((id . 999))))
                          ((symbol-function 'org-canvas--file-apply-settings)
                           (lambda (_fid _data) nil))
                          ((symbol-function 'org-canvas--file-finalize)
                           (lambda (&rest _args) nil))
                          ((symbol-function 'org-canvas--file-collect-folder-paths)
                           (lambda (_) nil))
                          ((symbol-function 'org-canvas--file-ensure-folders-exist)
                           (lambda (_) nil))
                          ((symbol-function 'display-buffer)
                           (lambda (&rest _) nil)))
                  (org-canvas-sync-files)
                  (expect push-called :to-be-truthy)))))
        (delete-file temp-file)
        (delete-file upload-file)))))

;;;; File Upload Step 3 Retry

(describe "org-canvas--file-upload-step3-confirm retry"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns directly when step2 response has ID"
    (let ((response '((id . 42) (display_name . "test.pdf"))))
      (expect (alist-get 'id (org-canvas--file-upload-step3-confirm response))
              :to-equal 42)))

  (it "retries on failure and succeeds on second attempt"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         (signal 'error '("Connection failed"))
                       '((id . 99) (display_name . "recovered.pdf")))))
                  ;; Skip sleep-for in tests
                  ((symbol-function 'sleep-for) (lambda (&rest _) nil)))
          (let ((result (org-canvas--file-upload-step3-confirm
                         '((location . "https://example.com/confirm")))))
            (expect (alist-get 'id result) :to-equal 99)
            (expect call-count :to-equal 2))))))

  (it "signals error after all retries exhausted"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Server error"))))
                ((symbol-function 'sleep-for) (lambda (&rest _) nil)))
        (expect (org-canvas--file-upload-step3-confirm
                 '((location . "https://example.com/confirm")))
                :to-throw 'error)))))

;;;; Pull Function Tests

(describe "org-canvas--file-pull-download"
  (before-each (test-org-canvas-reset-file-caches))

  (it "downloads when file is absent"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "test.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url path &rest _args)
                       (setq downloaded path))))
            (org-canvas--file-pull-download "test.pdf" "https://example.com/test.pdf" local-path 1024)
            (expect downloaded :to-equal local-path))
        (delete-directory temp-dir t))))

  (it "skips when file already exists"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "existing.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (progn
            (with-temp-file local-path (insert "existing"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args)
                         (setq downloaded t))))
              (org-canvas--file-pull-download "existing.pdf" "https://example.com/f" local-path 100)
              (expect downloaded :to-be nil)))
        (delete-directory temp-dir t))))

  (it "handles download errors gracefully"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "fail.pdf" temp-dir)))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url _path &rest _args)
                       (signal 'error '("Network error")))))
            ;; Should not throw
            (org-canvas--file-pull-download "fail.pdf" "https://example.com/f" local-path nil))
        (delete-directory temp-dir t))))

  (it "skips when download-url is nil"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "nourl.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url _path &rest _args) (setq downloaded t))))
            (org-canvas--file-pull-download "nourl.pdf" nil local-path 100)
            (expect downloaded :to-be nil))
        (delete-directory temp-dir t))))

  (it "creates the parent directory tree before downloading"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (nested-path (expand-file-name "Labs/Week 1/lab.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url path &rest _args)
                       (setq downloaded path)
                       (with-temp-file path (insert "x")))))
            (org-canvas--file-pull-download
             "lab.pdf" "https://example.com/lab.pdf" nested-path 1)
            (expect downloaded :to-equal nested-path)
            (expect (file-directory-p
                     (expand-file-name "Labs/Week 1" temp-dir))
                    :to-be-truthy))
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-files"
  (before-each (test-org-canvas-reset-file-caches))

  (defun test-files--mock-pages (folders files)
    "Return a lambda that dispatches on URL to FOLDERS or FILES."
    (lambda (_method url &optional _params)
      (cond
       ((string-match-p "/folders" url) folders)
       ((string-match-p "/files" url) files)
       (t '()))))

  (it "creates flat headings on a fresh empty files.org with all-root files"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files")))
                            '(((id . 1) (display_name . "syllabus.pdf")
                               (folder_id . 100)
                               (url . "https://example.com/syllabus.pdf")
                               (content-type . "application/pdf")
                               (size . 2048)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (expect (buffer-string) :to-match "syllabus.pdf")
                    (expect (buffer-string) :to-match "CANVAS_ID: +1")
                    (expect (buffer-string) :to-match "CONTENT_TYPE: +application/pdf")
                    (expect (buffer-string) :to-match "SIZE: +2048"))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates files.org when it does not exist"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect (file-exists-p files-file) :to-be-truthy)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "invalidates org-canvas--file-id-cache after rewriting files.org"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file)
                (org-canvas--file-id-cache
                 (let ((h (make-hash-table :test 'equal)))
                   (puthash "stale" "stale/path.pdf" h)
                   h)))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect org-canvas--file-id-cache :to-be nil)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates content directory"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect (file-directory-p
                           (expand-file-name "content" temp-dir))
                          :to-be-truthy)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "fresh pull builds folder hierarchy from non-root files"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files"))
                              ((id . 200) (full_name . "course files/Labs")))
                            '(((id . 1) (display_name . "lab1.pdf")
                               (folder_id . 200)
                               (url . "https://example.com/lab1.pdf")
                               (content-type . "application/pdf")
                               (size . 100)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (let ((body (buffer-string)))
                      (expect body :to-match "^\\* Labs$")
                      (expect body :to-match "^\\*\\* \\[\\[file:content/Labs/lab1\\.pdf\\]\\[lab1\\.pdf\\]\\]$")))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "re-pull on an existing flat files.org keeps flat structure"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file
              (insert "#+TITLE: Files\n* [[file:content/syllabus.pdf][syllabus.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files"))
                              ((id . 200) (full_name . "course files/Labs")))
                            '(((id . 1) (display_name . "syllabus.pdf")
                               (folder_id . 200)
                               (url . "https://example.com/syllabus.pdf")
                               (content-type . "application/pdf")
                               (size . 100)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (let ((body (buffer-string)))
                      ;; flat layout preserved — no folder heading inserted
                      (expect body :not :to-match "^\\* Labs$")
                      ;; existing heading still found and updated
                      (expect body :to-match "syllabus.pdf")
                      (expect body :to-match "CANVAS_ID: +1")))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "re-pull on a hierarchical files.org refuses with user-error"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file
              (insert "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (expect (org-canvas-pull-files) :to-throw 'user-error)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "falls back to flat emission when folder fetch fails"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method url &optional _params)
                             (cond
                              ((string-match-p "/folders" url)
                               (signal 'plz-error '("simulated folder fetch failure")))
                              ((string-match-p "/files" url)
                               '(((id . 1) (display_name . "fallback.pdf")
                                  (folder_id . 100)
                                  (url . "https://example.com/fallback.pdf")
                                  (content-type . "application/pdf")
                                  (size . 100)))))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  ;; Should not throw — falls back to empty folder map → flat emission.
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (let ((body (buffer-string)))
                      ;; File is emitted at root since folder map is empty.
                      (expect body :to-match "^\\* \\[\\[file:content/fallback\\.pdf\\]\\[fallback\\.pdf\\]\\]$")))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "escapes brackets in display_name to produce a parseable Org heading"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (bracketed "IML [Molnar] 2ed.pdf"))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files")))
                            `(((id . 7) (display_name . ,bracketed)
                               (folder_id . 100)
                               (url . "https://example.com/x.pdf")
                               (content-type . "application/pdf")
                               (size . 100)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (goto-char (point-min))
                    (search-forward "[[file:")
                    (goto-char (match-beginning 0))
                    (let* ((link (org-element-link-parser))
                           (link-type (org-element-property :type link)))
                      (expect link-type :to-equal "file"))
                    (expect (buffer-string) :to-match "CANVAS_ID: +7"))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; New Property Tests: Usage Rights

(describe "org-canvas--file-parse-entry (usage rights)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "parses USE_JUSTIFICATION property"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (content-dir (expand-file-name "content" temp-dir))
           (test-file (progn (make-directory content-dir t)
                             (expand-file-name "test.pdf" content-dir)))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "dummy"))
            (with-temp-file files-file
              (insert "* [[file:content/test.pdf][Test PDF]]
:PROPERTIES:
:USE_JUSTIFICATION: own_copyright
:USAGE_LICENSE: cc_by_sa
:COPYRIGHT: 2026 Me
:END:
"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--file-parse-entry)))
                  (expect (plist-get data :use-justification) :to-equal "own_copyright")
                  (expect (plist-get data :usage-license) :to-equal "cc_by_sa")
                  (expect (plist-get data :copyright) :to-equal "2026 Me")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns nil for missing usage rights"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (content-dir (expand-file-name "content" temp-dir))
           (test-file (progn (make-directory content-dir t)
                             (expand-file-name "test.pdf" content-dir)))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "dummy"))
            (with-temp-file files-file
              (insert "* [[file:content/test.pdf][Test PDF]]
:PROPERTIES:
:END:
"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--file-parse-entry)))
                  (expect (plist-get data :use-justification) :to-be nil)
                  (expect (plist-get data :usage-license) :to-be nil)
                  (expect (plist-get data :copyright) :to-be nil)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--file-set-usage-rights"
  (before-each (test-org-canvas-reset-file-caches))

  (it "calls PUT on usage_rights endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--file-set-usage-rights
         42 '(:use-justification "own_copyright"
              :usage-license "cc_by_sa"
              :copyright "2026 Me"))
        (expect-api-called 'PUT "usage_rights"))))

  (it "does nothing when use-justification is nil"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--file-set-usage-rights 42 '(:use-justification nil))
        (expect (test-org-canvas-api-called-p 'PUT "usage_rights") :to-be nil))))

  (it "handles API error gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("403 Forbidden")))))
        ;; Should not throw
        (org-canvas--file-set-usage-rights
         42 '(:use-justification "own_copyright"))))))

;;;; Coverage: debug log root folder path (Line 324)

(describe "org-canvas--file-parse-entry root folder debug log"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs 'root' when folder-path is empty (top-level file)"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (content-dir (expand-file-name "content" temp-dir))
           (test-file (progn (make-directory content-dir t)
                             (expand-file-name "doc.pdf" content-dir)))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "dummy"))
            (with-temp-file files-file
              (insert "* [[file:content/doc.pdf][My Document]]
:PROPERTIES:
:END:
"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--file-parse-entry)))
                  ;; Top-level file: folder-path should be empty
                  (expect (plist-get data :folder-path) :to-equal "")
                  (expect (plist-get data :display-name) :to-equal "My Document"))
                (kill-buffer))))
        (delete-directory temp-dir t))))

  (it "logs folder name when folder-path is non-empty"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (content-dir (expand-file-name "content" temp-dir))
           (test-file (progn (make-directory content-dir t)
                             (expand-file-name "doc.pdf" content-dir)))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "dummy"))
            (with-temp-file files-file
              (insert "* Labs
** [[file:content/doc.pdf][Lab 1]]
:PROPERTIES:
:END:
"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (search-forward "Lab 1")
                (org-back-to-heading)
                (let ((data (org-canvas--file-parse-entry)))
                  (expect (plist-get data :folder-path) :to-equal "Labs"))
                (kill-buffer))))
        (delete-directory temp-dir t)))))

;;;; Coverage: push-to-api error path with root folder (Line 618)

(describe "org-canvas--file-push-to-api error log with root folder"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil)
    (setq org-canvas--file-folder-cache (make-hash-table :test 'equal)))

  (it "logs 'root' in error message when folder-path is empty and upload fails"
    (with-org-canvas-test-config
      (let ((error-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((and (eq method 'GET) (string-match "folders/root" url))
                       '((id . 100) (name . "course files")))
                      ((eq method 'POST)
                       (signal 'error '("Step 1 notify failed"))))))
                  ((symbol-function 'org-canvas--log-error)
                   (lambda (_logger fmt &rest args)
                     (when (string-match "Upload failed" fmt)
                       (setq error-logged (apply #'format fmt args))))))
          (let ((temp-file (make-temp-file "test" nil ".pdf")))
            (unwind-protect
                (progn
                  (with-temp-file temp-file (insert "content"))
                  (let ((data (list :display-name "Test.pdf"
                                    :local-path temp-file
                                    :folder-path "")))
                    (expect (org-canvas--file-push-to-api data) :to-throw 'error)
                    (expect error-logged :to-match "root")))
              (delete-file temp-file)))))))

  (it "logs folder name in error message when folder-path is non-empty"
    (with-org-canvas-test-config
      (let ((error-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((and (eq method 'GET) (string-match "folders/root" url))
                       '((id . 100) (name . "course files")))
                      ((eq method 'POST)
                       (signal 'error '("Step 1 notify failed"))))))
                  ((symbol-function 'org-canvas--file-resolve-folder-by-path)
                   (lambda (_path) '((id . 200) (name . "Labs"))))
                  ((symbol-function 'org-canvas--log-error)
                   (lambda (_logger fmt &rest args)
                     (when (string-match "Upload failed" fmt)
                       (setq error-logged (apply #'format fmt args))))))
          (let ((temp-file (make-temp-file "test" nil ".pdf")))
            (unwind-protect
                (progn
                  (with-temp-file temp-file (insert "content"))
                  (let ((data (list :display-name "Lab1.pdf"
                                    :local-path temp-file
                                    :folder-path "Labs")))
                    (expect (org-canvas--file-push-to-api data) :to-throw 'error)
                    (expect error-logged :to-match "Labs")))
              (delete-file temp-file))))))))

;;;; Coverage: file-sync-single-entry with usage rights (Line 735)

(describe "org-canvas--file-sync-single-entry with usage rights"
  (before-each (test-org-canvas-reset-file-caches))

  (it "calls set-usage-rights when USE_JUSTIFICATION is present and push returns id"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (content-dir (expand-file-name "content" temp-dir))
           (test-file (progn (make-directory content-dir t)
                             (expand-file-name "test.pdf" content-dir)))
           (files-file (expand-file-name "files.org" temp-dir))
           (usage-rights-called nil))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "dummy"))
            (with-temp-file files-file
              (insert "* [[file:content/test.pdf][Test PDF]]
:PROPERTIES:
:USE_JUSTIFICATION: own_copyright
:END:
"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data)
                               '((id . 42) (display_name . "test.pdf"))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil))
                            ((symbol-function 'org-canvas--file-set-usage-rights)
                             (lambda (fid _data)
                               (setq usage-rights-called fid))))
                    (let ((result (org-canvas--file-sync-single-entry marker)))
                      (expect result :to-equal :success)
                      (expect usage-rights-called :to-equal 42))))
                (kill-buffer))))
        (delete-directory temp-dir t))))

  (it "does not call set-usage-rights when USE_JUSTIFICATION is absent"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (content-dir (expand-file-name "content" temp-dir))
           (test-file (progn (make-directory content-dir t)
                             (expand-file-name "test.pdf" content-dir)))
           (files-file (expand-file-name "files.org" temp-dir))
           (usage-rights-called nil))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "dummy"))
            (with-temp-file files-file
              (insert "* [[file:content/test.pdf][Test PDF]]
:PROPERTIES:
:END:
"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data)
                               '((id . 42) (display_name . "test.pdf"))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil))
                            ((symbol-function 'org-canvas--file-set-usage-rights)
                             (lambda (_fid _data)
                               (setq usage-rights-called t))))
                    (let ((result (org-canvas--file-sync-single-entry marker)))
                      (expect result :to-equal :success)
                      (expect usage-rights-called :to-be nil))))
                (kill-buffer))))
        (delete-directory temp-dir t)))))

;;;; Coverage: file-pull-set-properties usage_rights (Lines 955-963)

(describe "org-canvas--file-record-upload visibility"
  (it "applies the visibility settings after the id is recorded"
    ;; Issue #50: ordering is the failure contract — CANVAS_ID first so a
    ;; settings error cannot cause a duplicate upload, PAYLOAD_HASH last so
    ;; the entry stays dirty and the settings are retried.
    (let (order)
      (cl-letf (((symbol-function 'org-canvas--file-finalize)
                 (lambda (_data _resp) (push 'finalize order)))
                ((symbol-function 'org-canvas--file-apply-settings)
                 (lambda (fid _data) (push (cons 'settings fid) order)))
                ((symbol-function 'org-canvas-org-set-property)
                 (lambda (&rest _) (push 'hash order))))
        (org-canvas--file-record-upload
         '(:display-name "Doc" :published nil) '((id . 42)) "abc123" nil)
        (expect (nreverse order) :to-equal '(finalize (settings . 42) hash)))))

  (it "does not apply settings when the response carries no id"
    (let ((called nil))
      (cl-letf (((symbol-function 'org-canvas--file-finalize)
                 (lambda (_data _resp) nil))
                ((symbol-function 'org-canvas--file-apply-settings)
                 (lambda (_fid _data) (setq called t)))
                ((symbol-function 'org-canvas-org-set-property)
                 (lambda (&rest _) nil)))
        (org-canvas--file-record-upload
         '(:display-name "Doc" :published nil) '((display_name . "Doc")) "abc" nil)
        (expect called :to-be nil)))))

(describe "org-canvas--file-pull-set-properties visibility"
  (before-each (test-org-canvas-reset-file-caches))

  (it "writes PUBLISHED: false for a locked file"
    ;; Issue #50: `locked' is the Publish control, so it is what PUBLISHED
    ;; must round-trip against.
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point) '((locked . t) (hidden . :json-false)))
     (expect (org-entry-get (point) "PUBLISHED") :to-equal "false")
     (expect (org-entry-get (point) "HIDDEN") :to-be nil)))

  (it "writes HIDDEN: true for an unlisted file and leaves it published"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point) '((locked . :json-false) (hidden . t)))
     (expect (org-entry-get (point) "HIDDEN") :to-equal "true")
     (expect (org-entry-get (point) "PUBLISHED") :to-be nil)))

  (it "leaves both properties off for a plain published file"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point) '((locked . :json-false) (hidden . :json-false)))
     (expect (org-entry-get (point) "PUBLISHED") :to-be nil)
     (expect (org-entry-get (point) "HIDDEN") :to-be nil))))

(describe "org-canvas--file-pull-set-properties usage rights"
  (before-each (test-org-canvas-reset-file-caches))

  (it "sets USE_JUSTIFICATION, USAGE_LICENSE, and COPYRIGHT from usage_rights"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((content-type . "application/pdf")
                   (size . 2048)
                   (usage_rights . ((use_justification . "own_copyright")
                                    (license . "cc_by_sa")
                                    (legal_copyright . "2026 Author"))))))
       (org-canvas--file-pull-set-properties (point) item)
       (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-equal "own_copyright")
       (expect (org-entry-get (point) "USAGE_LICENSE") :to-equal "cc_by_sa")
       (expect (org-entry-get (point) "COPYRIGHT") :to-equal "2026 Author"))))

  (it "sets partial usage rights when only justification is present"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((content-type . "application/pdf")
                   (size . 512)
                   (usage_rights . ((use_justification . "fair_use"))))))
       (org-canvas--file-pull-set-properties (point) item)
       (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-equal "fair_use")
       (expect (org-entry-get (point) "USAGE_LICENSE") :to-be nil)
       (expect (org-entry-get (point) "COPYRIGHT") :to-be nil))))

  (it "does not set usage properties when usage_rights is absent"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((content-type . "application/pdf")
                   (size . 1024))))
       (org-canvas--file-pull-set-properties (point) item)
       (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-be nil)
       (expect (org-entry-get (point) "USAGE_LICENSE") :to-be nil)
       (expect (org-entry-get (point) "COPYRIGHT") :to-be nil)))))

(describe "org-canvas--file-confirm-with-retry echo area message"
  (before-each (test-org-canvas-reset-file-caches))

  (it "shows retry progress in echo area"
    (with-org-canvas-test-config
      (let ((attempt-count 0))
        (spy-on 'message)
        (spy-on 'sleep-for)
        (spy-on 'org-canvas--log-warning)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _)
                     (setq attempt-count (1+ attempt-count))
                     (if (< attempt-count 3)
                         (error "Not ready yet")
                       '((id . 42))))))
          (org-canvas--file-confirm-with-retry "https://test.example.com/confirm" 3)
          (expect 'message :to-have-been-called-with
                  "File upload: retry %d/%d after %ds..." 2 3 2))))))

(describe "org-canvas-pull-files progress"
  (before-each (test-org-canvas-reset-file-caches))

  (it "shows per-file progress in echo area"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (with-temp-file files-file (insert ""))
                (make-directory content-dir t)
                (spy-on 'message)
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 1) (display_name . "file1.pdf") (url . "http://x") (size . 100))
                               ((id . 2) (display_name . "file2.pdf") (url . "http://y") (size . 200)))))
                          ((symbol-function 'org-canvas--file-pull-download)
                           (lambda (&rest _) nil))
                          ((symbol-function 'org-canvas--html-to-org)
                           (lambda (html) html)))
                  (org-canvas-pull-files)
                  (expect 'message :to-have-been-called-with
                          "Files [%d/%d] Pulling '%s'..." 1 2 "file1.pdf")
                  (expect 'message :to-have-been-called-with
                          "Files [%d/%d] Pulling '%s'..." 2 2 "file2.pdf")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--file-pull-folder-relative-path"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns empty string for the Canvas root folder"
    (expect (org-canvas--file-pull-folder-relative-path "course files")
            :to-equal ""))
  (it "strips the prefix from a one-deep folder"
    (expect (org-canvas--file-pull-folder-relative-path "course files/Labs")
            :to-equal "Labs"))
  (it "strips the prefix from a deeply nested folder"
    (expect (org-canvas--file-pull-folder-relative-path "course files/Labs/Week 1")
            :to-equal "Labs/Week 1"))
  (it "leaves an unrecognized prefix unchanged"
    (expect (org-canvas--file-pull-folder-relative-path "other root/x")
            :to-equal "other root/x"))
  (it "returns empty string for nil"
    (expect (org-canvas--file-pull-folder-relative-path nil)
            :to-equal "")))

(describe "org-canvas--file-pull-fetch-folders"
  (before-each (test-org-canvas-reset-file-caches))

  (it "builds id-to-relative-path hash from folders API response"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 100) (full_name . "course files"))
                     ((id . 101) (full_name . "course files/Labs"))
                     ((id . 102) (full_name . "course files/Labs/Week 1"))))))
        (let ((map (org-canvas--file-pull-fetch-folders)))
          (expect (gethash 100 map) :to-equal "")
          (expect (gethash 101 map) :to-equal "Labs")
          (expect (gethash 102 map) :to-equal "Labs/Week 1")
          (expect (hash-table-count map) :to-equal 3)))))

  (it "skips folders that have no id"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((full_name . "no id"))
                     ((id . 5) (full_name . "course files/Ok"))))))
        (let ((map (org-canvas--file-pull-fetch-folders)))
          (expect (gethash 5 map) :to-equal "Ok")
          (expect (hash-table-count map) :to-equal 1)))))

  (it "returns an empty hash when API returns no folders"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params) '())))
        (let ((map (org-canvas--file-pull-fetch-folders)))
          (expect (hash-table-count map) :to-equal 0))))))

(describe "org-canvas--file-pull-mode"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns 'fresh for an empty buffer"
    (with-temp-org-buffer ""
      (expect (org-canvas--file-pull-mode) :to-equal 'fresh)))

  (it "returns 'fresh for a buffer with only #+TITLE: header"
    (with-temp-org-buffer "#+TITLE: Files\n# comment line\n"
      (expect (org-canvas--file-pull-mode) :to-equal 'fresh)))

  (it "returns 'flat when every heading has CANVAS_ID and a file link"
    (with-temp-org-buffer
        "* [[file:content/a.pdf][a.pdf]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
* [[file:content/b.pdf][b.pdf]]
:PROPERTIES:
:CANVAS_ID: 2
:END:
"
      (expect (org-canvas--file-pull-mode) :to-equal 'flat)))

  (it "returns 'hierarchical when a folder-only heading is present"
    (with-temp-org-buffer
        "* Labs
** [[file:content/Labs/a.pdf][a.pdf]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
      (expect (org-canvas--file-pull-mode) :to-equal 'hierarchical)))

  (it "returns 'hierarchical when one heading lacks both CANVAS_ID and a link"
    (with-temp-org-buffer
        "* Some folder name
* [[file:content/a.pdf][a.pdf]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
      (expect (org-canvas--file-pull-mode) :to-equal 'hierarchical))))

(describe "org-canvas--file-pull-emit-fresh-tree"
  (before-each (test-org-canvas-reset-file-caches))

  (it "emits one top-level heading per file when all files are at root"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql))
           (downloads nil))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url path &rest _args)
                         (push path downloads))))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "syllabus.pdf")
                    (folder_id . 100) (url . "https://example.com/syllabus.pdf")
                    (content-type . "application/pdf") (size . 2048))
                   ((id . 2) (display_name . "schedule.pdf")
                    (folder_id . 100) (url . "https://example.com/schedule.pdf")
                    (content-type . "application/pdf") (size . 1024)))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect body :to-match "^\\* \\[\\[file:content/schedule\\.pdf\\]\\[schedule\\.pdf\\]\\]$")
                  (expect body :to-match "^\\* \\[\\[file:content/syllabus\\.pdf\\]\\[syllabus\\.pdf\\]\\]$")
                  (expect body :to-match ":CANVAS_ID: 1")
                  (expect body :to-match ":CANVAS_ID: 2")
                  (expect body :to-match ":CONTENT_TYPE: +application/pdf")
                  (expect body :to-match ":SIZE: +2048")))
              (expect (length downloads) :to-equal 2)
              (expect (cl-some (lambda (p)
                                 (string-suffix-p "content/syllabus.pdf" p))
                               downloads)
                      :to-be-truthy)))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "emits a folder heading once for multiple files in the same folder"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (puthash 200 "Labs" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "lab1.pdf") (folder_id . 200)
                    (url . "https://example.com/lab1.pdf"))
                   ((id . 2) (display_name . "lab2.pdf") (folder_id . 200)
                    (url . "https://example.com/lab2.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect body :to-match "^\\* Labs$")
                  ;; Exactly one "* Labs" heading
                  (expect (with-temp-buffer
                            (insert body)
                            (goto-char (point-min))
                            (let ((n 0))
                              (while (re-search-forward "^\\* Labs$" nil t)
                                (cl-incf n))
                              n))
                          :to-equal 1)
                  (expect body :to-match "^\\*\\* \\[\\[file:content/Labs/lab1\\.pdf\\]\\[lab1\\.pdf\\]\\]$")
                  (expect body :to-match "^\\*\\* \\[\\[file:content/Labs/lab2\\.pdf\\]\\[lab2\\.pdf\\]\\]$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "emits ancestor folder headings for a deeply nested folder"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (puthash 300 "Labs/Week 1" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "intro.pdf") (folder_id . 300)
                    (url . "https://example.com/intro.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect body :to-match "^\\* Labs$")
                  (expect body :to-match "^\\*\\* Week 1$")
                  (expect body :to-match
                          "^\\*\\*\\* \\[\\[file:content/Labs/Week 1/intro\\.pdf\\]\\[intro\\.pdf\\]\\]$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "does not duplicate shared ancestor headings between sibling subfolders"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (puthash 301 "Labs/Week 1" folder-map)
            (puthash 302 "Labs/Week 2" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "w1.pdf") (folder_id . 301)
                    (url . "https://example.com/w1.pdf"))
                   ((id . 2) (display_name . "w2.pdf") (folder_id . 302)
                    (url . "https://example.com/w2.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect (with-temp-buffer
                            (insert body)
                            (goto-char (point-min))
                            (let ((n 0))
                              (while (re-search-forward "^\\* Labs$" nil t)
                                (cl-incf n))
                              n))
                          :to-equal 1)
                  (expect body :to-match "^\\*\\* Week 1$")
                  (expect body :to-match "^\\*\\* Week 2$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "places a file with unknown folder_id at the root"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            ;; Note: folder_id 999 is NOT in folder-map.
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "orphan.pdf") (folder_id . 999)
                    (url . "https://example.com/orphan.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect body :to-match "^\\* \\[\\[file:content/orphan\\.pdf\\]\\[orphan\\.pdf\\]\\]$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "file duplicate detection"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs a warning for files sharing (size, content-type)"
    (let* ((files '(((id . 1) (display_name . "foo.pdf")
                     (size . 100) (content-type . "application/pdf"))
                    ((id . 2) (display_name . "foo-1.pdf")
                     (size . 100) (content-type . "application/pdf"))
                    ((id . 3) (display_name . "bar.pdf")
                     (size . 200) (content-type . "application/pdf"))))
           (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--file-detect-duplicates files)
        (expect (and (cl-some (lambda (w) (string-match-p "duplicate" w))
                              warnings)
                     t)
                :to-be t))))

  (it "does not warn when all files have distinct (size, content-type)"
    (let* ((files '(((id . 1) (display_name . "a.pdf") (size . 100)
                     (content-type . "application/pdf"))
                    ((id . 2) (display_name . "b.pdf") (size . 200)
                     (content-type . "application/pdf"))))
           (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--file-detect-duplicates files)
        (expect warnings :to-be nil))))

  (it "lists the duplicate group's display names in the warning"
    (let* ((files '(((id . 1) (display_name . "Position.pdf") (size . 2644805)
                     (content-type . "application/pdf"))
                    ((id . 2) (display_name . "Position-1.pdf") (size . 2644805)
                     (content-type . "application/pdf"))))
           (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--file-detect-duplicates files)
        (let ((joined (mapconcat #'identity warnings " ")))
          (expect joined :to-match "Position\\.pdf")
          (expect joined :to-match "Position-1\\.pdf"))))))

(describe "file headline sanitization"
  (before-each (test-org-canvas-reset-file-caches))

  (it "escapes brackets in display names used as link descriptions"
    (expect (org-canvas--file-sanitize-headline-desc "[bracketed].pdf")
            :to-equal "\\[bracketed\\].pdf"))

  (it "leaves filenames without brackets unchanged"
    (expect (org-canvas--file-sanitize-headline-desc "normal.pdf")
            :to-equal "normal.pdf"))

  (it "preserves parens and underscores"
    (expect (org-canvas--file-sanitize-headline-desc "image(elongated).JPG")
            :to-equal "image(elongated).JPG"))

  (it "escapes both [ and ] in the same name"
    (expect (org-canvas--file-sanitize-headline-desc "x[1][2].png")
            :to-equal "x\\[1\\]\\[2\\].png")))

(describe "org-canvas--file-safe-local-path (path-traversal guard)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "keeps a normal file inside the content directory"
    (expect (org-canvas--file-safe-local-path "report.pdf" "/course/content")
            :to-equal "/course/content/report.pdf"))

  (it "keeps a legitimate subfolder path inside the content directory"
    (expect (org-canvas--file-safe-local-path "labs/lab01.py" "/course/content")
            :to-equal "/course/content/labs/lab01.py"))

  (it "neutralizes a ../ traversal to a basename inside the content directory"
    (let ((result (org-canvas--file-safe-local-path
                   "../../etc/evil" "/course/content")))
      (expect result :to-equal "/course/content/evil")
      (expect (string-prefix-p "/course/content/" result) :to-be-truthy)))

  (it "neutralizes an absolute path to a basename inside the content directory"
    (expect (org-canvas--file-safe-local-path "/etc/passwd" "/course/content")
            :to-equal "/course/content/passwd")))

;;;; Unchanged-File Skip and ID-Change Propagation (issue #22)

(describe "org-canvas--file-content-hash"
  (before-each (test-org-canvas-reset-file-caches))

  (it "is stable for identical content and settings"
    (let ((temp-file (make-temp-file "hash-test" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PDF content"))
            (let ((data (list :local-path temp-file :display-name "Test"
                              :folder-path "" :published t)))
              (expect (org-canvas--file-content-hash data)
                      :to-equal (org-canvas--file-content-hash data))))
        (delete-file temp-file))))

  (it "changes when the file content changes"
    (let ((temp-file (make-temp-file "hash-test" nil ".pdf")))
      (unwind-protect
          (let ((data (list :local-path temp-file :display-name "Test"
                            :folder-path "" :published t)))
            (with-temp-file temp-file (insert "version 1"))
            (let ((h1 (org-canvas--file-content-hash data)))
              (with-temp-file temp-file (insert "version 2"))
              (expect (org-canvas--file-content-hash data)
                      :not :to-equal h1)))
        (delete-file temp-file))))

  (it "changes when HIDDEN changes"
    (let ((temp-file (make-temp-file "hash-test" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PDF content"))
            (expect (org-canvas--file-content-hash
                     (list :local-path temp-file :display-name "Test"
                           :folder-path "" :published t :hidden t))
                    :not :to-equal
                    (org-canvas--file-content-hash
                     (list :local-path temp-file :display-name "Test"
                           :folder-path "" :published t))))
        (delete-file temp-file))))

  (it "changes when upload-relevant settings change"
    (let ((temp-file (make-temp-file "hash-test" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PDF content"))
            (let ((h1 (org-canvas--file-content-hash
                       (list :local-path temp-file :display-name "Test"
                             :folder-path "" :published t)))
                  (h2 (org-canvas--file-content-hash
                       (list :local-path temp-file :display-name "Test"
                             :folder-path "" :published nil))))
              (expect h1 :not :to-equal h2)))
        (delete-file temp-file)))))

(describe "org-canvas--file-sync-single-entry unchanged skip"
  (before-each (test-org-canvas-reset-file-caches))

  (it "skips an unchanged file without touching the API"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (pdf-file (expand-file-name "test.pdf" temp-dir))
           (files-file (expand-file-name "files.org" temp-dir))
           (push-called nil))
      (unwind-protect
          (progn
            (with-temp-file pdf-file (insert "PDF content"))
            (with-temp-file files-file
              (insert "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((org-canvas-files-file files-file))
              ;; First compute the hash the same way production will
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let* ((data (org-canvas--file-parse-entry))
                       (hash (org-canvas--file-content-hash data)))
                  (org-canvas-org-set-property (point) org-canvas--prop-payload-hash hash)
                  (let ((marker (point-marker)))
                    (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                               (lambda (_data) (setq push-called t) '((id . 100)))))
                      (expect (org-canvas--file-sync-single-entry marker)
                              :to-equal :skip)
                      (expect push-called :to-be nil))))
                (kill-buffer))))
        (delete-directory temp-dir t))))

  (it "uploads and stores the hash when the file has no stored hash"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (pdf-file (expand-file-name "test.pdf" temp-dir))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file pdf-file (insert "PDF content"))
            (with-temp-file files-file
              (insert "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:END:
"))
            (let ((org-canvas-files-file files-file)
                  (org-canvas--file-changed-ids nil))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data) '((id . 200))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil)))
                    (expect (org-canvas--file-sync-single-entry marker)
                            :to-equal :success))
                  (expect (org-entry-get (marker-position marker)
                                         org-canvas--prop-payload-hash)
                          :not :to-be nil)
                  ;; New file (no previous id): no changed-id recorded
                  (expect org-canvas--file-changed-ids :to-be nil))
                (kill-buffer))))
        (delete-directory temp-dir t))))

  (it "records the display name when the Canvas file ID changes"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (pdf-file (expand-file-name "test.pdf" temp-dir))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file pdf-file (insert "PDF content"))
            (with-temp-file files-file
              (insert "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((org-canvas-files-file files-file)
                  (org-canvas--file-changed-ids nil))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data) '((id . 200))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil)))
                    (expect (org-canvas--file-sync-single-entry marker)
                            :to-equal :success))
                  (expect org-canvas--file-changed-ids
                          :to-equal '("Test PDF")))
                (kill-buffer))))
        (delete-directory temp-dir t))))

  (it "does not record a change when the ID stays the same"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (pdf-file (expand-file-name "test.pdf" temp-dir))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file pdf-file (insert "PDF content"))
            (with-temp-file files-file
              (insert "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((org-canvas-files-file files-file)
                  (org-canvas--file-changed-ids nil))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data) '((id . 100))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil)))
                    (expect (org-canvas--file-sync-single-entry marker)
                            :to-equal :success))
                  (expect org-canvas--file-changed-ids :to-be nil))
                (kill-buffer))))
        (delete-directory temp-dir t)))))

(describe "org-canvas-sync-files ID-change propagation"
  (before-each (test-org-canvas-reset-file-caches))

  (it "keeps module hashes intact; the items digest picks up the new ID"
    ;; The old behavior cleared PAYLOAD_HASH on every module.  Now the
    ;; hash survives and dirtying happens naturally: a module item
    ;; linking the replaced file resolves to the NEW content id, so its
    ;; items digest changes and only that module re-pushes.
    (let ((temp-dir (make-temp-file "files-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "files.org" temp-dir))
                 (pdf-file (expand-file-name "test.pdf" temp-dir))
                 (modules-file (expand-file-name "modules.org" temp-dir)))
            (with-temp-file pdf-file (insert "PDF content"))
            (with-temp-file org-file
              (insert "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (with-temp-file modules-file
              (insert "* Week 1
:PROPERTIES:
:CANVAS_ID: 1
:PAYLOAD_HASH: aaa
:END:
** [[file:test.pdf][Test PDF]]
"))
            (let ((org-canvas-files-file org-file)
                  (org-canvas-modules-file modules-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (cond
                              ((string-match "courses/99999$" url) '((id . 99999)))
                              ((string-match "folders/root" url)
                               '((id . 100) (name . "course files")))
                              ((and (eq method 'POST) (string-match "folders/100/files" url))
                               '((upload_url . "https://s3.example.com/upload")
                                 (upload_params . ((key . "abc123")))))
                              (t nil))))
                          ((symbol-function 'org-canvas--file-upload-step2-send)
                           (lambda (_info _path)
                             '((id . 77777) (display_name . "test.pdf"))))
                          ((symbol-function 'org-canvas--file-upload-step3-confirm)
                           (lambda (resp) resp)))
                  (let ((digest-before
                         (with-current-buffer (find-file-noselect modules-file)
                           (save-excursion
                             (goto-char (point-min))
                             (org-back-to-heading)
                             (org-canvas--module-items-digest
                              (list :pom (point)))))))
                    (org-canvas-sync-files)
                    (with-current-buffer (find-file-noselect modules-file)
                      (goto-char (point-min))
                      (expect (buffer-string) :to-match ":PAYLOAD_HASH: aaa")
                      (org-back-to-heading)
                      (expect (org-canvas--module-items-digest
                               (list :pom (point)))
                              :not :to-equal digest-before)
                      (kill-buffer))
                    (expect org-canvas--file-changed-ids :to-be nil))))))
        (delete-directory temp-dir t)))))

;;;; Forced re-upload (correcting bytes a hash cannot see)

(describe "org-canvas--file-sync-parsed-entry forced"
  (it "re-uploads an entry whose hash matches exactly"
    ;; The case a hash cannot see: local and last-upload agree, but the
    ;; bytes on Canvas carry issue #70's leading CRLF.
    (let ((uploaded nil))
      (with-temp-org-buffer
       "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: aaa:bbb
:END:
"
       (org-back-to-heading)
       (let ((org-canvas--file-force-upload t))
         (cl-letf (((symbol-function 'org-canvas--file-content-hash)
                    (lambda (&rest _) "aaa:bbb"))
                   ((symbol-function 'org-canvas--file-sync-upload)
                    (lambda (&rest _) (setq uploaded t) :success)))
           (expect (org-canvas--file-sync-parsed-entry
                    (list :canvas-id "42" :display-name "doc.pdf"))
                   :to-be :success)
           (expect uploaded :to-be t))))))

  (it "does not consult the conflict check or the legacy migration"
    ;; Both reason from the same hashes the force is overriding.
    (let ((consulted nil))
      (with-temp-org-buffer
       "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: 5d41402abc4b2a76b9719d911017c592
:END:
"
       (org-back-to-heading)
       (let ((org-canvas--file-force-upload t))
         (cl-letf (((symbol-function 'org-canvas--file-content-hash)
                    (lambda (&rest _) "aaa:bbb"))
                   ((symbol-function 'org-canvas--file-check-conflict)
                    (lambda (&rest _) (setq consulted 'conflict) 'push))
                   ((symbol-function 'org-canvas--file-migrate-legacy-hash)
                    (lambda (&rest _) (setq consulted 'migrate) t))
                   ((symbol-function 'org-canvas--file-sync-upload)
                    (lambda (&rest _) :success)))
           (org-canvas--file-sync-parsed-entry
            (list :canvas-id "42" :display-name "doc.pdf"))
           (expect consulted :to-be nil))))))

  (it "leaves the ordinary skip alone when not forced"
    (let ((uploaded nil))
      (with-temp-org-buffer
       "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: aaa:bbb
:END:
"
       (org-back-to-heading)
       (let ((org-canvas--file-force-upload nil))
         (cl-letf (((symbol-function 'org-canvas--file-content-hash)
                    (lambda (&rest _) "aaa:bbb"))
                   ((symbol-function 'org-canvas--file-sync-upload)
                    (lambda (&rest _) (setq uploaded t) :success)))
           (expect (org-canvas--file-sync-parsed-entry
                    (list :canvas-id "42" :display-name "doc.pdf"))
                   :to-be :skip)
           (expect uploaded :to-be nil)))))))

(describe "org-canvas-files-force-reupload"
  (it "binds the force flag for one run and asks first"
    (let ((forced-during-run nil)
          (asked nil))
      (cl-letf (((symbol-function 'org-canvas--confirm)
                 (lambda (prompt) (setq asked prompt) t))
                ((symbol-function 'org-canvas-sync-files)
                 (lambda () (setq forced-during-run org-canvas--file-force-upload))))
        (org-canvas-files-force-reupload)
        (expect forced-during-run :to-be t)
        (expect asked :to-match "new Canvas file id")
        ;; and the flag does not outlive the run
        (expect org-canvas--file-force-upload :to-be nil))))

  (it "does nothing when the confirmation is declined"
    (let ((synced nil))
      (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil))
                ((symbol-function 'org-canvas-sync-files)
                 (lambda () (setq synced t))))
        (org-canvas-files-force-reupload)
        (expect synced :to-be nil)))))

(describe "org-canvas-force-reupload-file-at-point"
  (it "forces just the entry at point"
    (let ((forced-during-run nil))
      (with-temp-org-buffer
       "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) t))
                 ((symbol-function 'org-canvas--file-sync-single-entry)
                  (lambda (_marker)
                    (setq forced-during-run org-canvas--file-force-upload)
                    :success))
                 ((symbol-function 'message) #'ignore))
         (org-canvas-force-reupload-file-at-point)
         (expect forced-during-run :to-be t)))))

  (it "warns when the forced upload had to recreate the file"
    (let ((warnings nil))
      (with-temp-org-buffer
       "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) t))
                 ((symbol-function 'org-canvas--file-sync-single-entry)
                  (lambda (_marker)
                    (push "doc.pdf" org-canvas--file-recreated-ids)
                    :success))
                 ((symbol-function 'org-canvas--log-warning)
                  (lambda (_l fmt &rest args)
                    (push (apply #'format fmt args) warnings)))
                 ((symbol-function 'message) #'ignore))
         (org-canvas-force-reupload-file-at-point)
         (expect (car warnings) :to-match "moved folders")))))

  (it "does nothing when the confirmation is declined"
    (let ((synced nil))
      (with-temp-org-buffer
       "* doc.pdf\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil))
                 ((symbol-function 'org-canvas--file-sync-single-entry)
                  (lambda (_m) (setq synced t) :success)))
         (org-canvas-force-reupload-file-at-point)
         (expect synced :to-be nil))))))

;;;; Overwrite in place instead of delete-and-upload (issue #77)

(describe "org-canvas--file-replace-in-place-p"
  (it "is true when Canvas already holds the file in the target folder"
    (cl-letf (((symbol-function 'org-canvas-api-request)
               (lambda (&rest _) '((id . 42) (folder_id . 7)))))
      (expect (org-canvas--file-replace-in-place-p "42" 7) :to-be-truthy)))

  (it "compares ids across string and number spellings"
    (cl-letf (((symbol-function 'org-canvas-api-request)
               (lambda (&rest _) '((id . 42) (folder_id . 7)))))
      (expect (org-canvas--file-replace-in-place-p "42" "7") :to-be-truthy)))

  (it "is false when the entry moved to another folder"
    ;; Nothing at the destination to overwrite, so the old object would
    ;; survive as an orphan holding the module items.
    (cl-letf (((symbol-function 'org-canvas-api-request)
               (lambda (&rest _) '((id . 42) (folder_id . 7)))))
      (expect (org-canvas--file-replace-in-place-p "42" 9) :to-be nil)))

  (it "is false when the folder cannot be read, keeping the old behaviour"
    (cl-letf (((symbol-function 'org-canvas-api-request)
               (lambda (&rest _) (error "404 Not Found")))
              ((symbol-function 'org-canvas--log-warning) #'ignore))
      (expect (org-canvas--file-replace-in-place-p "42" 7) :to-be nil))))

(describe "org-canvas--file-clear-way-for-upload"
  (before-each (test-org-canvas-reset-file-caches))

  (it "does not delete a file that is being overwritten where it stands"
    ;; The whole point of #77: the delete is what took module item
    ;; 5832544 down with it.
    (let ((deleted nil)
          (org-canvas--file-recreated-ids nil))
      (cl-letf (((symbol-function 'org-canvas--file-replace-in-place-p)
                 (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _)
                   (when (eq method 'DELETE) (setq deleted t)))))
        (org-canvas--file-clear-way-for-upload
         '(:display-name "syllabus.pdf") "42" 7)
        (expect deleted :to-be nil)
        (expect org-canvas--file-recreated-ids :to-be nil))))

  (it "still deletes a file that moved folders"
    (let ((deleted nil)
          (org-canvas--file-recreated-ids nil))
      (cl-letf (((symbol-function 'org-canvas--file-replace-in-place-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _)
                   (when (eq method 'DELETE) (setq deleted t)))))
        (org-canvas--file-clear-way-for-upload
         '(:display-name "syllabus.pdf") "42" 9)
        (expect deleted :to-be t)
        (expect org-canvas--file-recreated-ids :to-equal '("syllabus.pdf")))))

  (it "carries on when the delete fails"
    (let ((warnings nil)
          (org-canvas--file-recreated-ids nil))
      (cl-letf (((symbol-function 'org-canvas--file-replace-in-place-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Delete failed")))
                ((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--file-clear-way-for-upload
         '(:display-name "syllabus.pdf") "42" 9)
        (expect (car warnings) :to-match "Could not delete old file")))))

(describe "org-canvas--file-warn-recreated-ids"
  (it "says the module items are gone and how they come back"
    (let ((warned nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args)
                   (setq warned (apply #'format fmt args)))))
        (org-canvas--file-warn-recreated-ids '("lecture.pdf"))
        (expect warned :to-match "moved folders")
        (expect warned :to-match "'lecture.pdf'")
        (expect warned :to-match "modules sync restores them")))))

(describe "org-canvas--file-warn-changed-ids"
  (before-each (test-org-canvas-reset-file-caches))

  (it "reports the changed file names"
    ;; Issue #77: an id change no longer costs the module items, so this
    ;; is news rather than a warning.
    (let ((noted nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (setq noted (apply #'format fmt args)))))
        (org-canvas--file-warn-changed-ids '("syllabus.pdf" "notes.pdf")))
      (expect noted :to-match "2 file ID(s) changed")
      (expect noted :to-match "'syllabus.pdf', 'notes.pdf'")
      (expect noted :to-match "old ids still resolve")))

  (it "does not clear module PAYLOAD_HASH (items digest handles dirtying)"
    ;; The old blunt invalidation forced EVERY module to re-push; the
    ;; items digest folded into each module's hash dirties exactly the
    ;; affected ones, so file-id rotation must leave hashes alone.
    (let* ((temp-dir (make-temp-file "files-test" t))
           (modules-file (expand-file-name "modules.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file modules-file
              (insert "* Week 1
:PROPERTIES:
:CANVAS_ID: 1
:PAYLOAD_HASH: aaa
:END:
** Item
"))
            (let ((org-canvas-modules-file modules-file))
              (org-canvas--file-warn-changed-ids '("syllabus.pdf")))
            (with-current-buffer (find-file-noselect modules-file)
              (expect (buffer-string) :to-match ":PAYLOAD_HASH: aaa")
              (kill-buffer)))
        (delete-directory temp-dir t))))

  (it "sync-files warns about changed ids and resets the list"
    (let* ((temp-dir (make-temp-file "files-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (warned nil))
      (unwind-protect
          (progn
            (with-temp-file files-file (insert "* Folder\n"))
            (let ((org-canvas-files-file files-file)
                  (org-canvas--file-changed-ids nil))
              (with-org-canvas-test-config
                (with-mock-api
                  (cl-letf (((symbol-function 'org-canvas--file-sync-single-entry)
                             (lambda (_marker)
                               (push "Test PDF" org-canvas--file-changed-ids)
                               :success))
                            ((symbol-function 'org-canvas--log-info)
                             (lambda (_l fmt &rest args)
                               (when (string-match-p "file ID(s) changed" fmt)
                                 (setq warned (apply #'format fmt args))))))
                    (org-canvas-sync-files))
                  (expect warned :to-match "'Test PDF'")
                  (expect org-canvas--file-changed-ids :to-be nil)))))
        (delete-directory temp-dir t)))))

;;;; Upload timeout recovery (issue #34)
;;
;; A 6 MB PDF took ~157s against a 120s `org-canvas-upload-timeout'.
;; `url-retrieve-synchronously' returned nil, the nil reached
;; `with-current-buffer', and the user saw "Wrong type argument: stringp,
;; nil" -- while Canvas had in fact stored the file.  The DELETE of the old
;; object had already run, so CANVAS_ID was left pointing at a dead id.

(describe "org-canvas--file-upload-step2-send timeout"
  (before-each (test-org-canvas-reset-file-caches))

  (it "signals a timeout error naming the timeout variable"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'url-retrieve-synchronously)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas--file-build-multipart-body)
                 (lambda (&rest _) "body")))
        (let ((err (condition-case e
                       (org-canvas--file-upload-step2-send
                        '((upload_url . "https://upload.example.com")) "/tmp/big.pdf")
                     (error e))))
          (expect (car err) :to-equal 'org-canvas-api-error)
          ;; The old failure mode: nil reaching `with-current-buffer'.
          (expect (error-message-string err) :not :to-match "stringp")
          (expect (error-message-string err) :to-match "timed out")
          (expect (error-message-string err) :to-match "org-canvas-upload-timeout")))))

  (it "is detected as a timeout by the shared predicate"
    ;; So the message stays compatible with `org-canvas--timeout-error-p',
    ;; which the rest of the package uses to trigger search-and-recover.
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'url-retrieve-synchronously)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas--file-build-multipart-body)
                 (lambda (&rest _) "body")))
        (let ((err (condition-case e
                       (org-canvas--file-upload-step2-send
                        '((upload_url . "https://upload.example.com")) "/tmp/big.pdf")
                     (error e))))
          (expect (org-canvas--timeout-error-p err) :to-be-truthy))))))

(describe "org-canvas--file-search-by-name folder filtering"
  (before-each (test-org-canvas-reset-file-caches))

  (it "ignores a same-named file living in another folder"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [((id . 111) (display_name . "notes.pdf") (folder_id . 7))
                            ((id . 222) (display_name . "notes.pdf") (folder_id . 9))])))
        (let ((result (org-canvas--file-search-by-name "notes.pdf" "Labs" 9)))
          (expect (alist-get 'id result) :to-equal 222)))))

  (it "matches on name alone when no folder id is supplied"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [((id . 111) (display_name . "notes.pdf") (folder_id . 7))])))
        (let ((result (org-canvas--file-search-by-name "notes.pdf" "")))
          (expect (alist-get 'id result) :to-equal 111)))))

  (it "returns nil when the only match is in a different folder"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [((id . 111) (display_name . "notes.pdf") (folder_id . 7))])))
        (expect (org-canvas--file-search-by-name "notes.pdf" "Labs" 9) :to-be nil)))))

(describe "org-canvas--file-recover-upload-id"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns the file Canvas stored despite the error"
    (cl-letf (((symbol-function 'org-canvas--file-search-by-name)
               (lambda (&rest _) '((id . 555) (display_name . "big.pdf")))))
      (let ((result (org-canvas--file-recover-upload-id
                     '(:display-name "big.pdf" :folder-path "" :canvas-id "123") 100)))
        (expect (alist-get 'id result) :to-equal 555))))

  (it "rejects a match still carrying the old id"
    ;; Neither the delete nor the upload landed.  Recording this as success
    ;; would store the new content hash against the old object and the
    ;; changed file would never upload again.
    (cl-letf (((symbol-function 'org-canvas--file-search-by-name)
               (lambda (&rest _) '((id . 123) (display_name . "big.pdf")))))
      (expect (org-canvas--file-recover-upload-id
               '(:display-name "big.pdf" :folder-path "" :canvas-id "123") 100)
              :to-be nil)))

  (it "returns nil when nothing matches"
    (cl-letf (((symbol-function 'org-canvas--file-search-by-name)
               (lambda (&rest _) nil)))
      (expect (org-canvas--file-recover-upload-id
               '(:display-name "big.pdf" :folder-path "" :canvas-id "123") 100)
              :to-be nil)))

  (it "recovers a first upload that has no previous id"
    (cl-letf (((symbol-function 'org-canvas--file-search-by-name)
               (lambda (&rest _) '((id . 777) (display_name . "new.pdf")))))
      (let ((result (org-canvas--file-recover-upload-id
                     '(:display-name "new.pdf" :folder-path "") 100)))
        (expect (alist-get 'id result) :to-equal 777)))))

(describe "org-canvas--file-push-to-api upload recovery"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil)
    (setq org-canvas--file-folder-cache (make-hash-table :test 'equal)))

  (it "returns the recovered file object instead of signaling"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest _args)
                   (cond
                    ((and (eq method 'GET) (string-match "folders/root" url))
                     '((id . 100) (name . "course files")))
                    ((eq method 'POST)
                     '((upload_url . "https://upload.example.com")
                       (upload_params . nil)))
                    (t nil))))
                ((symbol-function 'org-canvas--file-upload-step2-send)
                 (lambda (&rest _)
                   (org-canvas--signal 'org-canvas-api-error "Upload timed out")))
                ((symbol-function 'org-canvas--file-search-by-name)
                 (lambda (&rest _) '((id . 4242) (display_name . "Test.pdf")))))
        (let ((temp-file (make-temp-file "test" nil ".pdf")))
          (unwind-protect
              (progn
                (with-temp-file temp-file (insert "content"))
                (let* ((data (list :canvas-id "123"
                                   :display-name "Test.pdf"
                                   :local-path temp-file
                                   :folder-path ""))
                       (response (org-canvas--file-push-to-api data)))
                  ;; Finalize can now record the live id rather than leaving
                  ;; the entry pointing at the deleted one.
                  (expect (alist-get 'id response) :to-equal 4242)))
            (delete-file temp-file))))))

  (it "still signals when the file is nowhere on Canvas"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest _args)
                   (cond
                    ((and (eq method 'GET) (string-match "folders/root" url))
                     '((id . 100) (name . "course files")))
                    ((eq method 'POST)
                     '((upload_url . "https://upload.example.com")
                       (upload_params . nil)))
                    (t nil))))
                ((symbol-function 'org-canvas--file-upload-step2-send)
                 (lambda (&rest _)
                   (org-canvas--signal 'org-canvas-api-error "Upload timed out")))
                ((symbol-function 'org-canvas--file-search-by-name)
                 (lambda (&rest _) nil)))
        (let ((temp-file (make-temp-file "test" nil ".pdf")))
          (unwind-protect
              (progn
                (with-temp-file temp-file (insert "content"))
                (let ((data (list :canvas-id "123"
                                  :display-name "Test.pdf"
                                  :local-path temp-file
                                  :folder-path "")))
                  (expect (org-canvas--file-push-to-api data) :to-throw)))
            (delete-file temp-file)))))))

;;;; Dry run (issue #34)

(describe "org-canvas--file-push-to-api dry run"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns the sentinel without contacting Canvas"
    (with-org-canvas-test-config
      (let ((calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args) (push (cons method url) calls) nil)))
          (let* ((org-canvas--dry-run t)
                 (response (org-canvas--file-push-to-api
                            '(:canvas-id "123" :display-name "Test.pdf"
                              :local-path "/nonexistent.pdf" :folder-path ""))))
            (expect (org-canvas--dry-run-response-p response) :to-be-truthy)
            ;; Notably no DELETE /api/v1/files/123.
            (expect calls :to-equal nil))))))

  (it "does not read the local file at all"
    ;; The guard sits ahead of every step, so a preview works even for an
    ;; entry whose local file is missing.
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t))
        (expect (org-canvas--file-push-to-api
                 '(:display-name "Gone.pdf" :local-path "/nonexistent.pdf"
                   :folder-path ""))
                :not :to-throw)))))

(describe "org-canvas--file-sync-single-entry dry run"
  (before-each (test-org-canvas-reset-file-caches))

  (it "records neither CANVAS_ID nor PAYLOAD_HASH"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* [[file:missing.pdf][Test.pdf]]
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (org-back-to-heading t)
       (let ((marker (point-marker))
             (org-canvas--file-changed-ids nil))
         (cl-letf (((symbol-function 'org-canvas--file-parse-entry)
                    (lambda () (list :canvas-id "123" :display-name "Test.pdf"
                                     :local-path "/nonexistent.pdf" :folder-path "")))
                   ((symbol-function 'org-canvas--file-content-hash)
                    (lambda (_data) "hash-of-new-content")))
           (let* ((org-canvas--dry-run t)
                  (result (org-canvas--file-sync-single-entry marker)))
             (expect result :to-equal :dry-run)
             ;; Untouched: a preview must not mark the entry as synced.
             (expect (org-entry-get (point) "CANVAS_ID") :to-equal "123")
             (expect (org-entry-get (point) org-canvas--prop-payload-hash) :to-be nil)
             (expect (org-entry-get (point) org-canvas--prop-last-synced) :to-be nil)
             (expect org-canvas--file-changed-ids :to-be nil))))))))

;;;; Metadata-only updates and conflict detection (issue #49)

(describe "org-canvas--file-hash-parts"
  (it "splits a stored bytes:metadata hash"
    (expect (org-canvas--file-hash-parts
             "5d41402abc4b2a76b9719d911017c592:098f6bcd4621d373cade4e832627b4f6")
            :to-equal '("5d41402abc4b2a76b9719d911017c592"
                        . "098f6bcd4621d373cade4e832627b4f6")))

  (it "returns nil for a legacy single hash"
    ;; Entries synced before the split carry one opaque md5; nothing can be
    ;; concluded about their bytes, so they re-upload once more.
    (expect (org-canvas--file-hash-parts "5d41402abc4b2a76b9719d911017c592")
            :to-be nil))

  (it "returns nil for nil"
    (expect (org-canvas--file-hash-parts nil) :to-be nil)))

;;;; Legacy hash migration (issue #71)

(describe "org-canvas--file-legacy-hash-p"
  (it "recognizes a pre-split opaque hash"
    (expect (org-canvas--file-legacy-hash-p "5d41402abc4b2a76b9719d911017c592")
            :to-be-truthy))

  (it "does not claim a split hash"
    (expect (org-canvas--file-legacy-hash-p
             "5d41402abc4b2a76b9719d911017c592:098f6bcd4621d373cade4e832627b4f6")
            :to-be nil))

  (it "does not claim an absent or malformed hash"
    (expect (org-canvas--file-legacy-hash-p nil) :to-be nil)
    (expect (org-canvas--file-legacy-hash-p "") :to-be nil)
    (expect (org-canvas--file-legacy-hash-p "not-a-hash") :to-be nil)))

(describe "org-canvas--file-bytes-match-p"
  (it "matches identical bytes"
    (expect (org-canvas--file-bytes-match-p "abc" "abc") :to-be-truthy))

  (it "accepts a remote copy carrying the pre-fix leading CRLF"
    ;; Issue #70 put two bytes in front of every uploaded file; shedding
    ;; them is not worth rotating every Canvas file id.
    (expect (org-canvas--file-bytes-match-p "%PDF-1.7" "\r\n%PDF-1.7")
            :to-be-truthy))

  (it "rejects genuinely different content"
    (expect (org-canvas--file-bytes-match-p "abc" "abd") :to-be nil)
    (expect (org-canvas--file-bytes-match-p "abc" "\r\nabd") :to-be nil))

  (it "rejects a missing side rather than calling it a match"
    (expect (org-canvas--file-bytes-match-p "abc" nil) :to-be nil)
    (expect (org-canvas--file-bytes-match-p nil "abc") :to-be nil)))

(describe "org-canvas--file-remote-bytes"
  (it "returns the downloaded bytes for a Canvas file"
    (cl-letf (((symbol-function 'org-canvas-api-request)
               (lambda (&rest _) '((id . 42) (url . "https://example.com/f"))))
              ((symbol-function 'url-copy-file)
               (lambda (_url dest &rest _)
                 (with-temp-file dest (insert "remote bytes")))))
      (expect (org-canvas--file-remote-bytes "42" "f.pdf")
              :to-equal "remote bytes")))

  (it "returns nil when Canvas gives no download url"
    (cl-letf (((symbol-function 'org-canvas-api-request)
               (lambda (&rest _) '((id . 42)))))
      (expect (org-canvas--file-remote-bytes "42" "f.pdf") :to-be nil)))

  (it "answers a failed fetch with nil, not an error"
    (let (warnings)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "404 Not Found")))
                ((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (expect (org-canvas--file-remote-bytes "42" "f.pdf") :to-be nil)
        (expect (car warnings) :to-match "treating it as changed")))))

(describe "org-canvas--file-migrate-legacy-hash"
  (it "adopts the split hash and keeps the Canvas id when bytes agree"
    (let ((temp (make-temp-file "migrate-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "same bytes"))
            (with-temp-org-buffer
             "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: 5d41402abc4b2a76b9719d911017c592
:END:
"
             (org-back-to-heading)
             (cl-letf (((symbol-function 'org-canvas--file-remote-bytes)
                        (lambda (&rest _) "same bytes")))
               (expect (org-canvas--file-migrate-legacy-hash
                        (list :canvas-id "42" :display-name "doc.pdf"
                              :local-path temp)
                        "aaa:bbb")
                       :to-be-truthy)
               (expect (org-entry-get (point) "PAYLOAD_HASH")
                       :to-equal "aaa:bbb")
               (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))
        (delete-file temp))))

  (it "declines when the bytes differ, leaving the entry to re-upload"
    (let ((temp (make-temp-file "migrate-no-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "local bytes"))
            (with-temp-org-buffer
             "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: 5d41402abc4b2a76b9719d911017c592
:END:
"
             (org-back-to-heading)
             (cl-letf (((symbol-function 'org-canvas--file-remote-bytes)
                        (lambda (&rest _) "different bytes")))
               (expect (org-canvas--file-migrate-legacy-hash
                        (list :canvas-id "42" :display-name "doc.pdf"
                              :local-path temp)
                        "aaa:bbb")
                       :to-be nil)
               (expect (org-entry-get (point) "PAYLOAD_HASH")
                       :to-equal "5d41402abc4b2a76b9719d911017c592"))))
        (delete-file temp))))

  (it "writes nothing during a dry run"
    (let ((temp (make-temp-file "migrate-dry-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "same bytes"))
            (with-temp-org-buffer
             "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: 5d41402abc4b2a76b9719d911017c592
:END:
"
             (org-back-to-heading)
             (let ((org-canvas--dry-run t))
               (cl-letf (((symbol-function 'org-canvas--file-remote-bytes)
                          (lambda (&rest _) "same bytes")))
                 (expect (org-canvas--file-migrate-legacy-hash
                          (list :canvas-id "42" :display-name "doc.pdf"
                                :local-path temp)
                          "aaa:bbb")
                         :to-be-truthy)
                 (expect (org-entry-get (point) "PAYLOAD_HASH")
                         :to-equal "5d41402abc4b2a76b9719d911017c592")))))
        (delete-file temp)))))

(describe "org-canvas--file-sync-parsed-entry legacy migration"
  (it "skips an unchanged legacy entry instead of re-uploading it"
    ;; The reported symptom: 25 files re-uploaded, ids rotated, module
    ;; items dropped, for content nobody had touched.
    (let ((temp (make-temp-file "legacy-entry-" nil ".pdf"))
          (uploaded nil))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "unchanged"))
            (with-temp-org-buffer
             "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: 5d41402abc4b2a76b9719d911017c592
:END:
"
             (org-back-to-heading)
             (cl-letf (((symbol-function 'org-canvas--file-content-hash)
                        (lambda (&rest _) "aaa:bbb"))
                       ((symbol-function 'org-canvas--file-check-conflict)
                        (lambda (&rest _) 'push))
                       ((symbol-function 'org-canvas--file-remote-bytes)
                        (lambda (&rest _) "unchanged"))
                       ((symbol-function 'org-canvas--file-sync-upload)
                        (lambda (&rest _) (setq uploaded t) :success)))
               (expect (org-canvas--file-sync-parsed-entry
                        (list :canvas-id "42" :display-name "doc.pdf"
                              :local-path temp))
                       :to-be :skip)
               (expect uploaded :to-be nil)
               (expect (org-entry-get (point) "PAYLOAD_HASH")
                       :to-equal "aaa:bbb"))))
        (delete-file temp))))

  (it "still re-uploads a legacy entry whose bytes really changed"
    (let ((temp (make-temp-file "legacy-changed-" nil ".pdf"))
          (uploaded nil)
          (warnings nil))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "new content"))
            (with-temp-org-buffer
             "* doc.pdf
:PROPERTIES:
:CANVAS_ID: 42
:PAYLOAD_HASH: 5d41402abc4b2a76b9719d911017c592
:END:
"
             (org-back-to-heading)
             (cl-letf (((symbol-function 'org-canvas--file-content-hash)
                        (lambda (&rest _) "aaa:bbb"))
                       ((symbol-function 'org-canvas--file-check-conflict)
                        (lambda (&rest _) 'push))
                       ((symbol-function 'org-canvas--file-remote-bytes)
                        (lambda (&rest _) "old content"))
                       ((symbol-function 'org-canvas--log-warning)
                        (lambda (_l fmt &rest args)
                          (push (apply #'format fmt args) warnings)))
                       ((symbol-function 'org-canvas--file-sync-upload)
                        (lambda (&rest _) (setq uploaded t) :success)))
               (expect (org-canvas--file-sync-parsed-entry
                        (list :canvas-id "42" :display-name "doc.pdf"
                              :local-path temp))
                       :to-be :success)
               (expect uploaded :to-be t)
               (expect (car warnings) :to-match "rotates its file id"))))
        (delete-file temp)))))

(describe "org-canvas--file-announce-legacy-hashes"
  (it "counts and names how many entries carry a pre-split hash"
    (let (infos)
      (with-temp-org-buffer
       "* a.pdf
:PROPERTIES:
:PAYLOAD_HASH: 5d41402abc4b2a76b9719d911017c592
:END:
* b.pdf
:PROPERTIES:
:PAYLOAD_HASH: aaa:bbb
:END:
* c.pdf
:PROPERTIES:
:PAYLOAD_HASH: 098f6bcd4621d373cade4e832627b4f6
:END:
"
       (let ((markers (org-map-entries (lambda () (point-marker)) t 'file)))
         (cl-letf (((symbol-function 'org-canvas--log-info)
                    (lambda (_l fmt &rest args)
                      (push (apply #'format fmt args) infos))))
           (expect (org-canvas--file-announce-legacy-hashes markers) :to-equal 2)
           (expect (car infos) :to-match "2 entries carry a pre-split"))))))

  (it "says nothing when every entry has a split hash"
    (let (infos)
      (with-temp-org-buffer
       "* a.pdf
:PROPERTIES:
:PAYLOAD_HASH: aaa:bbb
:END:
"
       (let ((markers (org-map-entries (lambda () (point-marker)) t 'file)))
         (cl-letf (((symbol-function 'org-canvas--log-info)
                    (lambda (_l fmt &rest args)
                      (push (apply #'format fmt args) infos))))
           (expect (org-canvas--file-announce-legacy-hashes markers) :to-equal 0)
           (expect infos :to-be nil)))))))

(describe "org-canvas--file-content-hash split"
  (it "keeps the bytes half stable when only metadata changes"
    (let ((temp-file (make-temp-file "split-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PDF content"))
            (let* ((published (org-canvas--file-content-hash
                               (list :local-path temp-file :display-name "Doc"
                                     :folder-path "" :published t)))
                   (hidden (org-canvas--file-content-hash
                            (list :local-path temp-file :display-name "Doc"
                                  :folder-path "" :published nil))))
              (expect (car (org-canvas--file-hash-parts published))
                      :to-equal (car (org-canvas--file-hash-parts hidden)))
              (expect (cdr (org-canvas--file-hash-parts published))
                      :not :to-equal (cdr (org-canvas--file-hash-parts hidden)))))
        (delete-file temp-file))))

  (it "treats a rename as metadata, not content"
    ;; PUT /api/v1/files/:id takes `name', so renaming needs no re-upload.
    (let ((temp-file (make-temp-file "split-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PDF content"))
            (expect (car (org-canvas--file-hash-parts
                          (org-canvas--file-content-hash
                           (list :local-path temp-file :display-name "Old"
                                 :folder-path "" :published t))))
                    :to-equal
                    (car (org-canvas--file-hash-parts
                          (org-canvas--file-content-hash
                           (list :local-path temp-file :display-name "New"
                                 :folder-path "" :published t))))))
        (delete-file temp-file))))

  (it "changes the bytes half when the content changes"
    (let ((temp-file (make-temp-file "split-" nil ".pdf")))
      (unwind-protect
          (let ((data (list :local-path temp-file :display-name "Doc"
                            :folder-path "" :published t)))
            (with-temp-file temp-file (insert "version 1"))
            (let ((h1 (car (org-canvas--file-hash-parts
                            (org-canvas--file-content-hash data)))))
              (with-temp-file temp-file (insert "version 2"))
              (expect (car (org-canvas--file-hash-parts
                            (org-canvas--file-content-hash data)))
                      :not :to-equal h1)))
        (delete-file temp-file)))))

(describe "org-canvas--file-update-metadata"
  (it "PUTs name, folder and visibility without deleting anything"
    (with-org-canvas-test-config
      (let (calls)
        (cl-letf (((symbol-function 'org-canvas--file-target-folder-id)
                   (lambda (_path) 200))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (push (list method url (plist-get args :data)) calls)
                     '((id . 42)))))
          (org-canvas--file-update-metadata
           '(:canvas-id "42" :display-name "Syllabus.pdf"
             :folder-path "Docs" :published nil))
          (expect (length calls) :to-equal 1)
          (let ((call (car calls)))
            (expect (nth 0 call) :to-equal 'PUT)
            (expect (nth 1 call) :to-match "/api/v1/files/42")
            (expect (gethash "name" (nth 2 call)) :to-equal "Syllabus.pdf")
            (expect (gethash "parent_folder_id" (nth 2 call)) :to-equal 200)
            (expect (gethash "on_duplicate" (nth 2 call)) :to-equal "overwrite")
            (expect (gethash "locked" (nth 2 call)) :to-be t))))))

  (it "sends nothing during a dry run"
    (let ((org-canvas--dry-run t))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Must not contact the API"))))
        (expect (org-canvas--file-update-metadata
                 '(:canvas-id "42" :display-name "Doc" :folder-path ""))
                :to-be org-canvas--dry-run-response)))))

(describe "org-canvas--file-sync-metadata-only"
  (it "records the update and stores the hash on success"
    (let (recorded)
      (cl-letf (((symbol-function 'org-canvas--file-update-metadata)
                 (lambda (_data) '((id . 42) (updated_at . "2026-08-25T00:00:00Z"))))
                ((symbol-function 'org-canvas--file-record-metadata-update)
                 (lambda (_data response hash) (setq recorded (list response hash)))))
        (expect (org-canvas--file-sync-metadata-only
                 '(:canvas-id "42" :display-name "Doc") "bytes:meta")
                :to-equal :success)
        (expect (nth 1 recorded) :to-equal "bytes:meta"))))

  (it "records nothing during a dry run"
    (let ((org-canvas--dry-run t))
      (cl-letf (((symbol-function 'org-canvas--file-update-metadata)
                 (lambda (_data) org-canvas--dry-run-response))
                ((symbol-function 'org-canvas--file-record-metadata-update)
                 (lambda (&rest _) (error "A preview must not record anything"))))
        (expect (org-canvas--file-sync-metadata-only
                 '(:canvas-id "42" :display-name "Doc") "bytes:meta")
                :to-equal :dry-run)))))

(describe "org-canvas--file-record-metadata-update"
  (it "finalizes, applies usage rights, and stores the hash"
    (let (order)
      (with-temp-org-buffer
       "* [[file:doc.pdf][Doc]]\n"
       (org-back-to-heading t)
       (cl-letf (((symbol-function 'org-canvas--file-finalize)
                  (lambda (&rest _) (push 'finalize order)))
                 ((symbol-function 'org-canvas--file-set-usage-rights)
                  (lambda (fid _data) (push (cons 'rights fid) order)))
                 ((symbol-function 'org-canvas-org-set-property)
                  (lambda (&rest _) (push 'hash order))))
         (org-canvas--file-record-metadata-update
          '(:display-name "Doc" :use-justification "own_copyright")
          '((id . 42)) "bytes:meta")
         (expect (nreverse order) :to-equal '(finalize (rights . 42) hash))))))

  (it "does not re-apply the visibility settings — the PUT carried them"
    (with-temp-org-buffer
     "* [[file:doc.pdf][Doc]]\n"
     (org-back-to-heading t)
     (cl-letf (((symbol-function 'org-canvas--file-finalize) #'ignore)
               ((symbol-function 'org-canvas--file-apply-settings)
                (lambda (&rest _) (error "Already sent with the metadata PUT")))
               ((symbol-function 'org-canvas-org-set-property) #'ignore))
       (expect (org-canvas--file-record-metadata-update
                '(:display-name "Doc") '((id . 42)) "bytes:meta")
               :not :to-throw)))))

(describe "org-canvas--file-sync-parsed-entry routing"
  (before-each (test-org-canvas-reset-file-caches))

  (defun test-files-49--route (stored data)
    "Run the entry decision with STORED as the recorded hash.
Returns the symbol naming the path taken."
    (let ((taken nil))
      (with-temp-org-buffer
       "* [[file:doc.pdf][Doc]]\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"
       (org-back-to-heading t)
       (when stored
         (org-canvas-org-set-property (point) org-canvas--prop-payload-hash stored))
       (cl-letf (((symbol-function 'org-canvas--file-check-conflict)
                  (lambda (_data) 'push))
                 ((symbol-function 'org-canvas--file-sync-metadata-only)
                  (lambda (&rest _) (setq taken 'metadata) :success))
                 ((symbol-function 'org-canvas--file-sync-upload)
                  (lambda (&rest _) (setq taken 'upload) :success)))
         (let ((result (org-canvas--file-sync-parsed-entry data)))
           (or taken (and (eq result :skip) 'skip)))))))

  (it "skips when nothing changed"
    (let ((temp-file (make-temp-file "route-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "bytes"))
            (let ((data (list :canvas-id "42" :display-name "Doc"
                              :local-path temp-file :folder-path ""
                              :published t)))
              (expect (test-files-49--route
                       (org-canvas--file-content-hash data) data)
                      :to-equal 'skip)))
        (delete-file temp-file))))

  (it "updates in place when only the metadata changed"
    ;; The reported case: flipping PUBLISHED on an unchanged PDF deleted the
    ;; Canvas file and re-uploaded it under a new id.
    (let ((temp-file (make-temp-file "route-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "bytes"))
            (let* ((before (org-canvas--file-content-hash
                            (list :canvas-id "42" :display-name "Doc"
                                  :local-path temp-file :folder-path ""
                                  :published nil)))
                   (data (list :canvas-id "42" :display-name "Doc"
                               :local-path temp-file :folder-path ""
                               :published t)))
              (expect (test-files-49--route before data) :to-equal 'metadata)))
        (delete-file temp-file))))

  (it "re-uploads when the bytes changed"
    (let ((temp-file (make-temp-file "route-" nil ".pdf")))
      (unwind-protect
          (let ((data (list :canvas-id "42" :display-name "Doc"
                            :local-path temp-file :folder-path ""
                            :published t)))
            (with-temp-file temp-file (insert "version 1"))
            (let ((before (org-canvas--file-content-hash data)))
              (with-temp-file temp-file (insert "version 2"))
              (expect (test-files-49--route before data) :to-equal 'upload)))
        (delete-file temp-file))))

  (it "re-uploads once for an entry carrying a legacy single hash"
    (let ((temp-file (make-temp-file "route-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "bytes"))
            (let ((data (list :canvas-id "42" :display-name "Doc"
                              :local-path temp-file :folder-path ""
                              :published t)))
              (expect (test-files-49--route "5d41402abc4b2a76b9719d911017c592" data)
                      :to-equal 'upload)))
        (delete-file temp-file))))

  (it "writes nothing when the user resolves the conflict by skipping"
    (let ((temp-file (make-temp-file "route-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "bytes"))
            (with-temp-org-buffer
             "* [[file:doc.pdf][Doc]]\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"
             (org-back-to-heading t)
             (cl-letf (((symbol-function 'org-canvas--file-check-conflict)
                        (lambda (_data) 'skip))
                       ((symbol-function 'org-canvas--file-sync-upload)
                        (lambda (&rest _) (error "Must not overwrite Canvas")))
                       ((symbol-function 'org-canvas--file-sync-metadata-only)
                        (lambda (&rest _) (error "Must not overwrite Canvas"))))
               (expect (org-canvas--file-sync-parsed-entry
                        (list :canvas-id "42" :display-name "Doc"
                              :local-path temp-file :folder-path ""
                              :published t))
                       :to-equal :skip))))
        (delete-file temp-file))))

  (it "does not prompt for a file Canvas has never seen"
    (let ((temp-file (make-temp-file "route-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "bytes"))
            (with-temp-org-buffer
             "* [[file:doc.pdf][Doc]]\n"
             (org-back-to-heading t)
             (cl-letf (((symbol-function 'org-canvas--file-check-conflict)
                        (lambda (_data) (error "No remote item to compare")))
                       ((symbol-function 'org-canvas--file-sync-upload)
                        (lambda (&rest _) :success)))
               (expect (org-canvas--file-sync-parsed-entry
                        (list :display-name "Doc" :local-path temp-file
                              :folder-path "" :published t))
                       :to-equal :success))))
        (delete-file temp-file)))))

(describe "org-canvas--file-check-conflict"
  (it "asks about the course-scoped file and offers a working pull"
    (let (args)
      (cl-letf (((symbol-function 'org-canvas--push-check-and-resolve-conflict)
                 (lambda (endpoint id _data title &optional _modified-field)
                   (setq args (list endpoint id title
                                    org-canvas--current-pull-item-fn))
                   'push)))
        (org-canvas--file-check-conflict
         '(:canvas-id "42" :display-name "Syllabus.pdf"))
        (expect (nth 0 args) :to-equal "files")
        (expect (nth 1 args) :to-equal "42")
        (expect (nth 2 args) :to-equal "Syllabus.pdf")
        ;; Without a pull function the prompt's pull option degrades to a
        ;; skip, which would make the offer a lie.
        (expect (nth 3 args) :to-be #'org-canvas--file-pull-item)))))

(describe "org-canvas--file-pull-item"
  (it "overwrites the local copy and refreshes the properties"
    (let* ((temp-dir (make-temp-file "pull-item-" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (local (expand-file-name "doc.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (progn
            (with-temp-file local (insert "local version"))
            (with-temp-file files-file
              (insert "* [[file:doc.pdf][Doc]]\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading t)
                (cl-letf (((symbol-function 'org-canvas--file-pull-download)
                           (lambda (_name url path _size &optional force)
                             (setq downloaded (list url path force)))))
                  (org-canvas--file-pull-item
                   '((id . 42) (display_name . "Doc")
                     (url . "https://canvas.example.com/files/42/download")
                     (size . 12) (locked . t))
                   (point)))
                (expect (nth 0 downloaded)
                        :to-equal "https://canvas.example.com/files/42/download")
                (expect (nth 1 downloaded) :to-equal local)
                ;; Forced: the local file exists, and accepting Canvas's
                ;; version is the whole point of choosing pull.
                (expect (nth 2 downloaded) :to-be t)
                (expect (org-entry-get (point) "PUBLISHED") :to-equal "false")
                (kill-buffer))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--file-pull-item display name"
  (it "falls back to the local name when Canvas sends none"
    (let* ((dir (make-temp-file "pull-name-" t))
           (files-file (expand-file-name "files.org" dir))
           (local (expand-file-name "doc.pdf" dir))
           (name nil))
      (unwind-protect
          (progn
            (with-temp-file local (insert "local"))
            (with-temp-file files-file
              (insert "* [[file:doc.pdf][Doc]]\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((org-canvas-files-file files-file))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading t)
                (cl-letf (((symbol-function 'org-canvas--file-pull-download)
                           (lambda (n &rest _) (setq name n))))
                  (org-canvas--file-pull-item
                   '((id . 42) (url . "https://x/y") (size . 5)) (point)))
                (expect name :to-equal "Doc")
                (kill-buffer))))
        (delete-directory dir t)))))

(describe "org-canvas--file-pull-download force"
  (it "leaves an existing file alone by default"
    (let ((local (make-temp-file "dl-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file local (insert "local"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (&rest _) (error "Must not download"))))
              (expect (org-canvas--file-pull-download "Doc" "https://x/y" local 5)
                      :not :to-throw)))
        (delete-file local))))

  (it "overwrites when forced"
    (let ((local (make-temp-file "dl-" nil ".pdf"))
          (called nil))
      (unwind-protect
          (progn
            (with-temp-file local (insert "local"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (&rest _) (setq called t))))
              (org-canvas--file-pull-download "Doc" "https://x/y" local 5 t)
              (expect called :to-be t)))
        (delete-file local)))))

;;;; Mutation hardening (issue #38)
;;
;; A full mutation pass scored these paths poorly despite ~99.5% line
;; coverage: the lines ran, but nothing asserted on what they produced.
;; The specs below pin the values, not merely the execution.

(describe "org-canvas--file-validate-local size boundary"
  (before-each (test-org-canvas-reset-file-caches))

  ;; `>' vs `>=' survived mutation: no fixture sat exactly on the limit.
  (defun test-files--sized-file (dir mb)
    "Create a file in DIR of exactly MB megabytes and return its path."
    (let ((path (expand-file-name (format "sized-%s.bin" mb) dir)))
      (with-temp-file path
        (set-buffer-multibyte nil)
        (insert (make-string (round (* mb org-canvas--bytes-per-mb)) ?x)))
      path))

  (it "accepts a file exactly on the limit"
    (let ((dir (make-temp-file "size-" t)))
      (unwind-protect
          (let* ((org-canvas-max-file-size-mb 1)
                 (path (test-files--sized-file dir 1)))
            (expect (org-canvas--file-validate-local path "exact.bin")
                    :not :to-throw))
        (delete-directory dir t))))

  (it "rejects a file over the limit"
    (let ((dir (make-temp-file "size-" t)))
      (unwind-protect
          (let* ((org-canvas-max-file-size-mb 1)
                 (path (test-files--sized-file dir 2)))
            (expect (org-canvas--file-validate-local path "big.bin")
                    :to-throw 'org-canvas-validation-error))
        (delete-directory dir t)))))

(describe "org-canvas--file-confirm-with-retry attempt count"
  (before-each (test-org-canvas-reset-file-caches))

  ;; `<' vs `<=' survived: nothing pinned how many attempts actually run.
  (it "makes exactly max-retries attempts before giving up"
    (let ((attempts 0))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (setq attempts (1+ attempts))
                   (org-canvas--signal 'org-canvas-api-error "nope")))
                ((symbol-function 'sleep-for) #'ignore)
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (expect (org-canvas--file-confirm-with-retry "https://x.example/1" 3)
                :to-throw)
        (expect attempts :to-equal 3))))

  (it "does not back off before the first attempt"
    ;; `(> attempt 1)' guards the exponential sleep.  With `>=' the very
    ;; first attempt would sleep a second and log a spurious "Retry 1/N"
    ;; before it had failed at anything.
    (let ((slept nil))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) '((id . 5))))
                ((symbol-function 'sleep-for)
                 (lambda (&rest args) (push args slept)))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (org-canvas--file-confirm-with-retry "https://x.example/1" 3)
        (expect slept :to-be nil))))

  (it "stops at the first success without burning the remaining retries"
    (let ((attempts 0))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (setq attempts (1+ attempts))
                   (if (= attempts 2)
                       '((id . 5))
                     (org-canvas--signal 'org-canvas-api-error "nope"))))
                ((symbol-function 'sleep-for) #'ignore)
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (expect (alist-get 'id (org-canvas--file-confirm-with-retry
                                "https://x.example/1" 5))
                :to-equal 5)
        (expect attempts :to-equal 2)))))

(describe "org-canvas-sync-files reported counts"
  (before-each (test-org-canvas-reset-file-caches))

  ;; Every `1+' in the counter dispatch survived being flipped to `1-':
  ;; tests asserted that a sync ran and which requests it made, never the
  ;; tallies it reports.  The counts are what the user reads.
  (defun test-files--sync-with (results)
    "Run `org-canvas-sync-files' with one entry per element of RESULTS.
Each element is what `org-canvas--file-sync-single-entry' should return.
Returns (COUNTERS . FINAL-MESSAGE)."
    (let ((dir (make-temp-file "counts-" t))
          (recorded nil)
          (final-message nil)
          (remaining results))
      (unwind-protect
          (let ((org-file (expand-file-name "files.org" dir)))
            (with-temp-file org-file
              (dotimes (i (length results))
                (insert (format "* Entry %d\n:PROPERTIES:\n:END:\n" i))))
            (let ((org-canvas-files-file org-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (&rest _) nil))
                            ((symbol-function 'org-canvas--file-collect-folder-paths)
                             (lambda (&rest _) nil))
                            ((symbol-function 'org-canvas--file-sync-single-entry)
                             (lambda (_marker) (pop remaining)))
                            ((symbol-function 'org-canvas--sync-record-feature-stats)
                             (lambda (_label counters) (setq recorded counters)))
                            ((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (setq final-message (apply #'format fmt args)))))
                    (org-canvas-sync-files))))))
        (delete-directory dir t))
      (cons recorded final-message)))

  (it "counts each outcome exactly once"
    (let ((counters (car (test-files--sync-with
                          '(:success :success :skip :fail :dry-run)))))
      (expect (plist-get counters :success) :to-equal 2)
      (expect (plist-get counters :skip) :to-equal 1)
      (expect (plist-get counters :fail) :to-equal 1)
      (expect (plist-get counters :dry-run) :to-equal 1)))

  (it "reports zeros when nothing matched"
    (let ((counters (car (test-files--sync-with '()))))
      (expect (plist-get counters :success) :to-equal 0)
      (expect (plist-get counters :skip) :to-equal 0)
      (expect (plist-get counters :fail) :to-equal 0)
      (expect (plist-get counters :dry-run) :to-equal 0)))

  (it "mentions would-upload only when a dry run produced some"
    ;; Guards the `(> dry-run-count 0)' branch that picks the wording.
    (expect (cdr (test-files--sync-with '(:dry-run))) :to-match "1 would upload")
    (expect (cdr (test-files--sync-with '(:success))) :not :to-match "would upload")))

(describe "org-canvas--file-get-or-create-folder by_path result"
  (before-each (test-org-canvas-reset-file-caches))

  ;; No test drove by_path returning an empty list, which is what Canvas
  ;; gives for a path that does not exist yet.  Note the `and'->`or'
  ;; mutant here is equivalent and stays alive by design: an empty vector
  ;; is truthy in Elisp, so `or' takes the "use last folder" branch,
  ;; `(elt [] -1)' signals, and the enclosing condition-case creates the
  ;; folder anyway — the same observable outcome.  These specs are for
  ;; the behavior, not for that mutant.
  (it "creates the folder when by_path returns an empty list"
    (with-org-canvas-test-config
      (let ((created nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) []))
                  ((symbol-function 'org-canvas--file-create-folder)
                   (lambda (path _parent) (setq created path) '((id . 9)))))
          (expect (alist-get 'id (org-canvas--file-get-or-create-folder "Labs" 1))
                  :to-equal 9)
          (expect created :to-equal "Labs")))))

  (it "returns the last folder of a non-empty by_path result"
    ;; by_path returns the whole chain; the target is the final element.
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) [((id . 5) (name . "course files"))
                                    ((id . 6) (name . "Labs"))]))
                ((symbol-function 'org-canvas--file-create-folder)
                 (lambda (&rest _) (error "Should not create an existing folder"))))
        (expect (alist-get 'id (org-canvas--file-get-or-create-folder "Labs" 1))
                :to-equal 6))))

  (it "creates the folder when by_path signals"
    (with-org-canvas-test-config
      (let ((created nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _)
                     (org-canvas--signal 'org-canvas-api-error "404")))
                  ((symbol-function 'org-canvas--file-create-folder)
                   (lambda (path _parent) (setq created path) '((id . 9)))))
          (org-canvas--file-get-or-create-folder "Labs" 1)
          (expect created :to-equal "Labs"))))))

(describe "org-canvas--file-get-all-folders ordering"
  (before-each (test-org-canvas-reset-file-caches))

  ;; The comparator coalesces a missing full_name with `or ... ""'.  Both
  ;; the `or' and the `>' survived, because no fixture folder lacked a
  ;; full_name and none tied on length.
  (it "sorts deepest path first so children delete before parents"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((id . 2) (full_name . "course files/Labs"))
                     ((id . 3) (full_name . "course files/Labs/Week1"))
                     ((id . 1) (full_name . "course files")))))
                ((symbol-function 'org-canvas--file-get-root-folder)
                 (lambda () '((id . 1)))))
        (expect (mapcar (lambda (f) (alist-get 'id f))
                        (org-canvas--file-get-all-folders))
                :to-equal '(3 2)))))

  (it "tolerates a folder with no full_name instead of erroring"
    ;; The `or ... ""' exists for exactly this; nothing exercised it.
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((id . 2) (full_name . nil))
                     ((id . 3) (full_name . "course files/Labs/Week1")))))
                ((symbol-function 'org-canvas--file-get-root-folder)
                 (lambda () '((id . 1)))))
        (expect (mapcar (lambda (f) (alist-get 'id f))
                        (org-canvas--file-get-all-folders))
                :to-equal '(3 2)))))

  (it "orders by actual path length, not merely by having a path"
    ;; Three folders given shortest-first, so the expected result is a
    ;; full reversal.  If either side of the comparison stopped yielding
    ;; the real length — collapsing to 0 or "" — the input order would
    ;; survive unchanged and children could be deleted after parents.
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((id . 2) (full_name . "cf/a"))
                     ((id . 3) (full_name . "cf/a/bb"))
                     ((id . 4) (full_name . "cf/a/bb/cccc")))))
                ((symbol-function 'org-canvas--file-get-root-folder)
                 (lambda () '((id . 1)))))
        (expect (mapcar (lambda (f) (alist-get 'id f))
                        (org-canvas--file-get-all-folders))
                :to-equal '(4 3 2)))))

  (it "leaves an already deepest-first list alone"
    ;; Paired with the test above, which supplies the reverse order.  A
    ;; comparator that stopped reading the right-hand length would degrade
    ;; to "always true", which reverses whatever it is given — passing the
    ;; ascending case by luck and failing this one.
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((id . 4) (full_name . "cf/a/bb/cccc"))
                     ((id . 3) (full_name . "cf/a/bb"))
                     ((id . 2) (full_name . "cf/a")))))
                ((symbol-function 'org-canvas--file-get-root-folder)
                 (lambda () '((id . 1)))))
        (expect (mapcar (lambda (f) (alist-get 'id f))
                        (org-canvas--file-get-all-folders))
                :to-equal '(4 3 2)))))

  (it "tolerates a missing full_name on either side of the comparison"
    ;; Both operands coalesce independently, and `sort' decides which
    ;; folder is which argument — so the nil has to be exercised from the
    ;; other side too, with the input order reversed.
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((id . 3) (full_name . "course files/Labs/Week1"))
                     ((id . 2) (full_name . nil)))))
                ((symbol-function 'org-canvas--file-get-root-folder)
                 (lambda () '((id . 1)))))
        (expect (mapcar (lambda (f) (alist-get 'id f))
                        (org-canvas--file-get-all-folders))
                :to-equal '(3 2))))))

(describe "org-canvas--file-parse-upload-response branches"
  (before-each (test-org-canvas-reset-file-caches))

  ;; The status-line guard and the json/location dispatch both survived:
  ;; tests only ever fed it a healthy response carrying an id.
  (it "returns the file object when the body carries an id"
    (with-temp-buffer
      (insert "HTTP/1.1 201 Created\nContent-Type: application/json\n\n"
              "{\"id\": 42, \"display_name\": \"a.pdf\"}")
      (goto-char (point-min))
      (let ((result (org-canvas--file-parse-upload-response "/tmp/a.pdf" "https://u")))
        (expect (alist-get 'id result) :to-equal 42))))

  (it "returns the Location redirect when the body carries no id"
    (with-temp-buffer
      (insert "HTTP/1.1 302 Found\nLocation: https://canvas.example/api/v1/files/7\n\n")
      (goto-char (point-min))
      (let ((result (org-canvas--file-parse-upload-response "/tmp/a.pdf" "https://u")))
        (expect (alist-get 'location result)
                :to-equal "https://canvas.example/api/v1/files/7"))))

  (it "prefers the Location when a JSON body exists but has no id"
    ;; The discriminating case for the first cond clause: with a JSON
    ;; body present but no id, `and' must fall through to Location while
    ;; `or' would wrongly return the idless body as the file object.
    (with-temp-buffer
      (insert "HTTP/1.1 201 Created\n"
              "Location: https://canvas.example/api/v1/files/7\n"
              "Content-Type: application/json\n\n"
              "{\"upload_status\": \"pending\"}")
      (goto-char (point-min))
      (let ((result (org-canvas--file-parse-upload-response "/tmp/a.pdf" "https://u")))
        (expect (alist-get 'location result)
                :to-equal "https://canvas.example/api/v1/files/7")
        (expect (alist-get 'upload_status result) :to-be nil))))

  (it "signals when there is neither an id nor a Location"
    (with-temp-buffer
      (insert "HTTP/1.1 500 Internal Server Error\n\nboom")
      (goto-char (point-min))
      (expect (org-canvas--file-parse-upload-response "/tmp/a.pdf" "https://u")
              :to-throw 'org-canvas-api-error)))

  (it "reports a truncated response as an API error, not a search failure"
    ;; A response cut off before the header/body separator has no blank
    ;; line at all.  `re-search-forward' is called with NOERROR so it
    ;; returns nil and the body stays empty; without it the search
    ;; signals `search-failed', which escapes as an opaque Lisp error
    ;; instead of the actionable upload failure.
    (with-temp-buffer
      (insert "HTTP/1.1 502 Bad Gateway\nContent-Type: text/plain")
      (goto-char (point-min))
      (expect (org-canvas--file-parse-upload-response "/tmp/a.pdf" "https://u")
              :to-throw 'org-canvas-api-error)))

  (it "extracts no body from a response lacking a separator"
    (with-temp-buffer
      (insert "HTTP/1.1 502 Bad Gateway\nContent-Type: text/plain")
      (goto-char (point-min))
      (let ((parts (org-canvas--file-extract-response-parts)))
        (expect (plist-get parts :status) :to-equal "502")
        (expect (plist-get parts :body) :to-be nil)
        (expect (plist-get parts :json) :to-be nil)))))

(describe "org-canvas--file-pull-mode partial heading"
  (before-each (test-org-canvas-reset-file-caches))

  ;; `(unless (or cid link-path) ...)' marks a heading as a folder
  ;; container only when BOTH are absent.  Existing tests covered
  ;; neither-present and both-present, where `or' and `and' agree, so
  ;; the mutation survived.  The discriminating case is a heading with
  ;; exactly one of the two: under `and' it would be mistaken for a
  ;; folder and the pull would reshape the whole file hierarchically.
  (it "is fresh for a link that has not been synced yet"
    (with-temp-org-buffer
     "* [[file:a.pdf][a.pdf]]
"
     (expect (org-canvas--file-pull-mode) :to-equal 'fresh))))

(describe "org-canvas-delete-all-files reported count"
  (before-each (test-org-canvas-reset-file-caches))

  ;; `(setq deleted-file-count (1+ deleted-file-count))' survived: the
  ;; closing message was never read back, so the tally could be wrong.
  (it "counts only the deletions that succeeded"
    (with-org-canvas-test-config
      (let ((said nil)
            (dir (make-temp-file "delall-" t)))
        (unwind-protect
            (let ((files-file (expand-file-name "files.org" dir)))
              (with-temp-file files-file (insert "* Files\n"))
              (let ((org-canvas-files-file files-file))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-clear-log) #'ignore)
                          ((symbol-function 'display-buffer) #'ignore)
                          ((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _)
                             '(((id . 1) (display_name . "a.pdf"))
                               ((id . 2) (display_name . "b.pdf"))
                               ((id . 3) (display_name . "c.pdf")))))
                          ((symbol-function 'org-canvas--file-delete-all-folders)
                           (lambda () 0))
                          ((symbol-function 'org-canvas--clean-local-sync-properties)
                           #'ignore)
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_method url &rest _)
                             ;; The middle delete fails.
                             (when (string-match-p "files/2" url)
                               (org-canvas--signal 'org-canvas-api-error "nope"))
                             nil))
                          ((symbol-function 'message)
                           (lambda (fmt &rest args)
                             (setq said (apply #'format fmt args)))))
                  (org-canvas-delete-all-files))))
          (delete-directory dir t))
        ;; Full equality, not a substring match: "2 files" also matches
        ;; "-2 files", so a negated counter would slip through a regex.
        (expect said :to-equal "Deletion complete. 2 files, 0 folders removed.")))))

(describe "file conflict and finalize use modified_at (issue #94)"
  (it "does not raise a conflict for a metadata-only touch"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* syllabus.pdf
:PROPERTIES:
:CANVAS_ID: 31495932
:CANVAS_UPDATED_AT: 2026-08-31T18:34:37Z
:END:
"
        (org-back-to-heading)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _)
                     '((id . 31495932)
                       (updated_at . "2026-09-01T12:12:24Z")
                       (modified_at . "2026-08-31T18:34:37Z"))))
                  ((symbol-function 'org-canvas--resolve-conflict)
                   (lambda (&rest _) (error "Must not prompt"))))
          (expect (org-canvas--file-check-conflict
                   (list :canvas-id "31495932" :display-name "syllabus.pdf"
                         :pom (point)))
                  :to-equal 'push)))))

  (it "stamps the content timestamp after an upload"
    (with-temp-org-buffer "* syllabus.pdf\n"
      (org-back-to-heading)
      (org-canvas--file-finalize
       (list :display-name "syllabus.pdf" :pom (point))
       '((id . 31495932) (updated_at . "2026-09-01T12:12:24Z")
         (modified_at . "2026-08-31T18:34:37Z")))
      (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
              :to-equal "2026-08-31T18:34:37Z"))))


(describe "file pull reads usage rights through the registry (issue #135)"
  (it "writes USE_JUSTIFICATION, USAGE_LICENSE and COPYRIGHT from usage_rights"
    (with-temp-org-buffer
     "* [[file:content/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point)
      '((id . 1) (locked . :json-false)
        (usage_rights . ((use_justification . "own_copyright")
                         (license . "cc_by")
                         (legal_copyright . "(C) 2026 Ada")))))
     (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-equal "own_copyright")
     (expect (org-entry-get (point) "USAGE_LICENSE") :to-equal "cc_by")
     (expect (org-entry-get (point) "COPYRIGHT") :to-equal "(C) 2026 Ada")
     (expect (org-entry-get (point) "PUBLISHED") :to-be nil)))

  (it "writes PUBLISHED false for a locked file"
    (with-temp-org-buffer
     "* [[file:content/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties (point) '((id . 1) (locked . t)))
     (expect (org-entry-get (point) "PUBLISHED") :to-equal "false"))))

;;; org-canvas-files-test.el ends here
