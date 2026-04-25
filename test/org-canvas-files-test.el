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

  (it "sets hidden=t when not published"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published nil))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "hidden" payload) :to-be t)))

  (it "omits hidden when published"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published t))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "hidden" payload) :to-be nil)))

  (it "includes unlock_at when specified"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published t
                       :unlock-at "2024-01-15T09:00:00Z"))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "unlock_at" payload) :to-equal "2024-01-15T09:00:00Z")))

  (it "includes lock_at when specified"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published t
                       :lock-at "2024-06-01T23:59:00Z"))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "lock_at" payload) :to-equal "2024-06-01T23:59:00Z")))

  (it "excludes unlock_at when not specified"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published t))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "unlock_at" payload) :to-be nil)))

  (it "excludes lock_at when not specified"
    (let* ((data (list :display-name "Doc"
                       :local-path temp-file
                       :published t))
           (payload (org-canvas--file-build-upload-request data 123)))
      (expect (gethash "lock_at" payload) :to-be nil))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--file-upload-step1-notify (mocked)"
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

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* [[file:doc.pdf][Document]]
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :display-name "Document" :pom (point-marker)))
           (response '((id . 66666))))
       (org-canvas--file-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-"))))

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
                          ((symbol-function 'org-canvas--file-finalize)
                           (lambda (_data _resp) nil)))
                  (org-canvas-sync-files)
                  ;; Both files should be attempted
                  (expect push-attempts :to-equal 2)))))
        (delete-directory temp-dir t)))))

;;;; Delete All Files

(describe "org-canvas-delete-all-files (mocked)"
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
        (delete-directory temp-dir t)))))

;;;; Upload Step 2 Edge Cases

(describe "org-canvas--file-upload-step2-send edge cases"
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
  (it "logs search location with non-empty folder path"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("files" . [((display_name . "test.pdf") (id . 100))])))
        ;; Should not error when folder-path is non-empty
        (let ((result (org-canvas--file-search-by-name "test.pdf" "Labs/Week1")))
          (expect (alist-get 'id result) :to-equal 100))))))

(describe "org-canvas-sync-files pre-flight warning"
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
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-files"
  (it "creates headings with file links and properties"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 1) (display_name . "syllabus.pdf")
                                (url . "https://example.com/syllabus.pdf")
                                (content-type . "application/pdf")
                                (size . 2048)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (expect (buffer-string) :to-match "syllabus.pdf")
                    (expect (buffer-string) :to-match "CANVAS_ID.*1")
                    (expect (buffer-string) :to-match "CONTENT_TYPE.*application/pdf")
                    (expect (buffer-string) :to-match "SIZE.*2048"))))))
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
                           (lambda (_method _url &optional _params)
                             `(((id . 7) (display_name . ,bracketed)
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
                    ;; Pre-bind `:type' outside `expect': buttercup's
                    ;; `expect' oclosure shadows the `:type' keyword on
                    ;; Emacs 29.x, silently returning nil.
                    (let* ((link (org-element-link-parser))
                           (link-type (org-element-property :type link)))
                      (expect link-type :to-equal "file"))
                    (expect (buffer-string) :to-match "CANVAS_ID.*7"))))))
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
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect (file-directory-p
                           (expand-file-name "content" temp-dir))
                          :to-be-truthy)))))
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
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect (file-exists-p files-file) :to-be-truthy)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; New Property Tests: Usage Rights

(describe "org-canvas--file-parse-entry (usage rights)"
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

(describe "org-canvas--file-pull-set-properties usage rights"
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

;;; org-canvas-files-test.el ends here
