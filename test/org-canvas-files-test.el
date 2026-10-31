;;; org-canvas-files-test.el --- Buttercup tests for org-canvas-files  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-files': the configuration and session caches,
;; the heading helpers, the folder operations, the pre-flight folder
;; check, the sync itself and the delete commands.  The four-stage
;; upload is in org-canvas-files-upload-test.el and the pull in
;; org-canvas-files-pull-test.el; the three files were one until #593
;; split the 2,078-line source, and the specs moved with their code.

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

;;;; Folder Operations (mocked)

(describe "org-canvas--file-get-root-folder (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq org-canvas--file-root-folder-cache nil))

  (it "fetches root folder from API"
    (with-test-api-responses '(("folders/root" . ((id . 1001) (name . "course files"))))
      (let ((result (org-canvas--file-get-root-folder)))
        (expect (alist-get 'id result) :to-equal 1001)
        (expect-api-called 'GET "folders/root"))))

  (it "caches root folder for subsequent calls"
    (with-test-api-responses '(("folders/root" . ((id . 1002) (name . "course files"))))
      (org-canvas--file-get-root-folder)
      (org-canvas--file-get-root-folder)
      (expect (test-org-canvas-api-call-count) :to-equal 1))))

(describe "org-canvas--file-ensure-subfolder (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns existing folder when found"
    (with-test-api-responses '(("folders/100/folders" . [((id . 200) (name . "Labs"))
                                                          ((id . 201) (name . "Homework"))]))
      (let ((parent '((id . 100)))
            (result (org-canvas--file-ensure-subfolder '((id . 100)) "Labs")))
        (expect (alist-get 'id result) :to-equal 200))))

  (it "creates folder when not found"
    (with-test-api-responses '(("folders/100/folders" . [])
                                ("" . ((id . 999) (name . "NewFolder"))))
      (let ((result (org-canvas--file-ensure-subfolder '((id . 100)) "NewFolder")))
        ;; Should have made POST call to create folder
        (expect-api-called 'POST "folders/100/folders")))))

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
    (with-test-api-handler (lambda (method url &rest _args)
                             (cond
                               ((string-match "folders/root" url)
                                 '((id . 100) (name . "course files")))
                               ((string-match "folders/100/folders" url)
                                 (if (eq method 'GET)
                                   [((id . 200) (name . "Labs"))]
                                   '((id . 200) (name . "Labs"))))
                               (t nil)))
      (org-canvas--file-resolve-folder-by-path "Labs")
      (expect (gethash "Labs" org-canvas--file-folder-cache) :to-be-truthy)
      (expect (alist-get 'id (gethash "Labs" org-canvas--file-folder-cache)) :to-equal 200))))

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
    (with-test-api-responses '(("folders$" . [((id . 100) (name . "root") (full_name . "course files"))
                                               ((id . 200) (name . "Labs") (full_name . "course files/Labs"))])
                                ("folders/root" . ((id . 100) (name . "course files"))))
      (let ((result (org-canvas--file-get-all-folders)))
        (expect (length result) :to-equal 1)
        (expect (alist-get 'id (car result)) :to-equal 200))))

  (it "sorts folders by depth (deepest first)"
    (with-test-api-responses '(("folders$" . [((id . 100) (full_name . "course files"))
                                               ((id . 200) (full_name . "course files/A"))
                                               ((id . 300) (full_name . "course files/A/B"))
                                               ((id . 400) (full_name . "course files/A/B/C"))])
                                ("folders/root" . ((id . 100) (name . "course files"))))
      (let ((result (org-canvas--file-get-all-folders)))
        ;; Deepest folder should be first
        (expect (alist-get 'id (car result)) :to-equal 400)))))

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
                           (lambda (_data &optional _ctx)
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
                           (lambda (_data &optional _ctx)
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

  (it "snapshots the files it is about to delete (issue #555)"
    (with-org-canvas-test-config
      (with-delete-snapshot-dir
        (with-sync-test-env
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _args)
                       (when (and (eq method 'GET)
                                  (string-match "courses/.*/files$" url))
                         ;; A file uploaded through the web UI that Org
                         ;; never held: no re-push can bring it back.
                         [((id . 333) (display_name . "web-upload.png"))])))
                    ((symbol-function 'org-canvas--file-delete-all-folders)
                     (lambda () 0)))
            (let ((org-canvas-files-file "/nonexistent"))
              (org-canvas-delete-all-files)))
          (let ((files (directory-files snapshot-dir t "-files-all\\.json\\'")))
            (expect (length files) :to-equal 1)
            ;; The snapshot holds the item as the listing returned it.
            (let ((items (append (json-read-file (car files)) nil)))
              (expect (alist-get 'display_name (car items))
                      :to-equal "web-upload.png")))))))

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

;;;; Folder Path Resolution Edge Cases

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

;;;; Coverage Gap Tests

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
                           (lambda (data &optional _ctx)
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

;;;; Coverage: file-sync-single-entry with usage rights (Line 735)

(defun test-files--sync-at (marker &optional ctx)
  "Sync the entry at MARKER as the pipeline's push does; return the tier's outcome.
Parses at point and hands the result to `org-canvas--file-sync-parsed-entry';
a heading the parser declines answers :skip, as the runner would count it."
  (org-with-point-at marker
    (let ((data (org-canvas--file-parse-entry)))
      (if data (org-canvas--file-sync-parsed-entry data ctx) :skip))))

(describe "org-canvas--file-sync-parsed-entry with usage rights"
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
                             (lambda (_data &optional _ctx)
                               '((id . 42) (display_name . "test.pdf"))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil))
                            ((symbol-function 'org-canvas--file-set-usage-rights)
                             (lambda (fid _data)
                               (setq usage-rights-called fid))))
                    (let ((result (test-files--sync-at marker)))
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
                             (lambda (_data &optional _ctx)
                               '((id . 42) (display_name . "test.pdf"))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil))
                            ((symbol-function 'org-canvas--file-set-usage-rights)
                             (lambda (_fid _data)
                               (setq usage-rights-called t))))
                    (let ((result (test-files--sync-at marker)))
                      (expect result :to-equal :success)
                      (expect usage-rights-called :to-be nil))))
                (kill-buffer))))
        (delete-directory temp-dir t)))))

;;;; Helper and sync coverage

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

(describe "org-canvas--file-sync-parsed-entry unchanged skip"
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
                               (lambda (_data &optional _ctx) (setq push-called t) '((id . 100)))))
                      (expect (test-files--sync-at marker)
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
                  (ctx (org-canvas--sync-make-ctx)))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data &optional _ctx) '((id . 200))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil)))
                    (expect (test-files--sync-at marker ctx)
                            :to-equal :success))
                  (expect (org-entry-get (marker-position marker)
                                         org-canvas--prop-payload-hash)
                          :not :to-be nil)
                  ;; New file (no previous id): no changed-id recorded
                  (expect (plist-get ctx :file-changed-ids) :to-be nil))
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
                  (ctx (org-canvas--sync-make-ctx)))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data &optional _ctx) '((id . 200))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil)))
                    (expect (test-files--sync-at marker ctx)
                            :to-equal :success))
                  (expect (plist-get ctx :file-changed-ids)
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
                  (ctx (org-canvas--sync-make-ctx)))
              (with-current-buffer (find-file-noselect files-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((marker (point-marker)))
                  (cl-letf (((symbol-function 'org-canvas--file-push-to-api)
                             (lambda (_data &optional _ctx) '((id . 100))))
                            ((symbol-function 'org-canvas--file-apply-settings)
                             (lambda (_fid _data) nil))
                            ((symbol-function 'org-canvas--file-finalize)
                             (lambda (_data _resp) nil)))
                    (expect (test-files--sync-at marker ctx)
                            :to-equal :success))
                  (expect (plist-get ctx :file-changed-ids) :to-be nil))
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
                      (kill-buffer)))))))
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
                 ((symbol-function 'org-canvas-sync-file-at-point)
                  (lambda ()
                    (setq forced-during-run org-canvas--file-force-upload)
                    (org-canvas--sync-make-ctx)))
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
                 ((symbol-function 'org-canvas-sync-file-at-point)
                  (lambda ()
                    (let ((ctx (org-canvas--sync-make-ctx)))
                      (org-canvas--ctx-push ctx :file-recreated-ids "doc.pdf")
                      ctx)))
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
                 ((symbol-function 'org-canvas-sync-file-at-point)
                  (lambda () (setq synced t) (org-canvas--sync-make-ctx))))
         (org-canvas-force-reupload-file-at-point)
         (expect synced :to-be nil))))))

;;;; Overwrite in place instead of delete-and-upload (issue #77)

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
            (let ((org-canvas-files-file files-file))
              (with-test-api
                (cl-letf (((symbol-function 'org-canvas--file-parse-entry)
                           (lambda () (list :display-name "Test PDF" :pom (point))))
                          ((symbol-function 'org-canvas--file-sync-parsed-entry)
                           (lambda (_data &optional ctx)
                             (org-canvas--ctx-push ctx :file-changed-ids "Test PDF")
                             :success))
                          ((symbol-function 'org-canvas--log-info)
                           (lambda (_l fmt &rest args)
                             (when (string-match-p "file ID(s) changed" fmt)
                               (setq warned (apply #'format fmt args))))))
                  (org-canvas-sync-files))
                (expect warned :to-match "'Test PDF'"))))
        (delete-directory temp-dir t)))))

;;;; Dry run (issue #34)

(describe "org-canvas--file-sync-parsed-entry dry run"
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
             (ctx (org-canvas--sync-make-ctx)))
         (cl-letf (((symbol-function 'org-canvas--file-parse-entry)
                    (lambda () (list :canvas-id "123" :display-name "Test.pdf"
                                     :local-path "/nonexistent.pdf" :folder-path "")))
                   ((symbol-function 'org-canvas--file-content-hash)
                    (lambda (_data) "hash-of-new-content")))
           (let* ((org-canvas--dry-run t)
                  (result (test-files--sync-at marker ctx)))
             (expect result :to-equal :dry-run)
             ;; Untouched: a preview must not mark the entry as synced.
             (expect (org-entry-get (point) "CANVAS_ID") :to-equal "123")
             (expect (org-entry-get (point) org-canvas--prop-payload-hash) :to-be nil)
             (expect (org-entry-get (point) org-canvas--prop-last-synced) :to-be nil)
             (expect (plist-get ctx :file-changed-ids) :to-be nil))))))))

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
                  (lambda (_data &optional _ctx) 'push))
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
                        (lambda (_data &optional _ctx) 'skip))
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
                        (lambda (_data &optional _ctx) (error "No remote item to compare")))
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
                 (lambda (data id options)
                   (setq args (list (org-canvas--push-options-endpoint options)
                                    id
                                    (plist-get data (org-canvas--push-options-title-key
                                                     options))
                                    (org-canvas--push-options-updated-field options)
                                    (plist-get (org-canvas--push-options-ctx options)
                                               :pull-item-fn)))
                   'push)))
        (org-canvas--file-check-conflict
         '(:canvas-id "42" :display-name "Syllabus.pdf"))
        (expect (nth 0 args) :to-equal "files")
        (expect (nth 1 args) :to-equal "42")
        (expect (nth 2 args) :to-equal "Syllabus.pdf")
        ;; A metadata-only touch must not read as a remote change
        ;; (issue #94).
        (expect (nth 3 args) :to-be 'modified_at)
        ;; Without a pull function the prompt's pull option degrades to a
        ;; skip, which would make the offer a lie.
        (expect (nth 4 args) :to-be #'org-canvas--file-pull-item)))))

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

;;;; Mutation hardening (issue #38)

(describe "org-canvas-sync-files reported counts"
  (before-each (test-org-canvas-reset-file-caches))

  ;; Every `1+' in the counter dispatch survived being flipped to `1-':
  ;; tests asserted that a sync ran and which requests it made, never the
  ;; tallies it reports.  The counts are what the user reads.  Files run
  ;; on the shared pipeline, so this pins the runner's counting of what
  ;; the files push returns: an outcome per entry, :fail as an error.
  (defun test-files--sync-with (results)
    "Run `org-canvas-sync-files' with one entry per element of RESULTS.
Each element is what `org-canvas--file-sync-parsed-entry' should return,
or :fail to signal.  Returns (COUNTERS . FINAL-MESSAGE)."
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
                            ((symbol-function 'org-canvas--file-parse-entry)
                             (lambda () (list :display-name (org-get-heading t t t t)
                                              :pom (point))))
                            ((symbol-function 'org-canvas--file-sync-parsed-entry)
                             (lambda (_data &optional _ctx)
                               (let ((outcome (pop remaining)))
                                 (if (eq outcome :fail) (error "upload failed") outcome))))
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

  (it "reports a dry run in the dry-run wording, and only then"
    (expect (cdr (test-files--sync-with '(:dry-run))) :to-match "1 would sync")
    (expect (cdr (test-files--sync-with '(:success))) :not :to-match "would sync")))

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
                          ;; The snapshot's path is pinned so the message
                          ;; below can stay a full equality (issue #555).
                          ((symbol-function 'org-canvas--delete-all-snapshot)
                           (lambda (_feature _items) "/tmp/snapshot.json"))
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
        (expect said
                :to-equal "Deletion complete. 2 files, 0 folders removed; its JSON is in /tmp/snapshot.json.")))))

(describe "file conflict uses modified_at (issue #94)"
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
                  :to-equal 'push))))))

(describe "files on the sync macro"
  (before-each (test-org-canvas-reset-file-caches))

  (it "generates the sync and the push at point"
    (expect (commandp 'org-canvas-sync-files) :to-be t)
    (expect (commandp 'org-canvas-sync-file-at-point) :to-be t))

  (it "maps the tiers' outcomes onto the runner's push results"
    (cl-letf (((symbol-function 'org-canvas--file-sync-parsed-entry)
               (lambda (data &optional _ctx) (plist-get data :outcome))))
      (expect (org-canvas--file-push (list :outcome :skip) nil) :to-be 'skip)
      (expect (org-canvas--dry-run-response-p
               (org-canvas--file-push (list :outcome :dry-run) nil))
              :to-be-truthy)
      (expect (org-canvas--file-push (list :outcome :success) nil) :to-be t)))

  (it "pre-creates the manifest's folders in :prepare, or only names them in a dry run"
    (let ((files-file (make-temp-file "prepare-" nil ".org"))
          (ensured nil) (logged nil))
      (unwind-protect
          (progn
            (with-temp-file files-file (insert "* Week 1\n** [[file:content/Week 1/a.pdf][a.pdf]]\n"))
            (let ((org-canvas-files-file files-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request) (lambda (&rest _) nil))
                          ((symbol-function 'org-canvas--file-ensure-folders-exist)
                           (lambda (paths) (setq ensured paths)))
                          ((symbol-function 'org-canvas--log-info)
                           (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
                  (org-canvas--file-sync-prepare (org-canvas--sync-make-ctx))
                  (expect ensured :to-equal '("Week 1"))
                  (setq ensured nil)
                  (let ((org-canvas--dry-run t))
                    (org-canvas--file-sync-prepare (org-canvas--sync-make-ctx)))
                  (expect ensured :to-be nil)
                  (expect (cl-some (lambda (l) (string-match-p "Would ensure folder exists: Week 1" l)) logged)
                          :to-be-truthy)))))
        (let ((buf (find-buffer-visiting files-file))) (when buf (kill-buffer buf)))
        (delete-file files-file))))

  (it "keeps going when the course access check fails in :prepare"
    (let ((files-file (make-temp-file "prepare-" nil ".org"))
          (warned nil))
      (unwind-protect
          (progn
            (with-temp-file files-file (insert "* a.pdf\n"))
            (let ((org-canvas-files-file files-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (&rest _) (signal 'error '("Connection refused"))))
                          ((symbol-function 'org-canvas--log-warning)
                           (lambda (_l fmt &rest args) (push (apply #'format fmt args) warned))))
                  (expect (org-canvas--file-sync-prepare (org-canvas--sync-make-ctx)) :not :to-throw)
                  (expect (car (last warned)) :to-match "Connection refused")))))
        (let ((buf (find-buffer-visiting files-file))) (when buf (kill-buffer buf)))
        (delete-file files-file))))

  (it "reports changed and recreated ids from the context in :after-sync"
    (let ((ctx (org-canvas--sync-make-ctx)) (infos nil) (warns nil))
      (org-canvas--ctx-push ctx :file-changed-ids "a.pdf")
      (org-canvas--ctx-push ctx :file-recreated-ids "b.pdf")
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) infos)))
                ((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) warns))))
        (org-canvas--file-sync-after ctx))
      (expect (car infos) :to-match "'a.pdf'")
      (expect (car warns) :to-match "'b.pdf'")))

  (it "declares the pipeline options the tiers rely on"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas--sync-run-pipeline)
                 (lambda (spec) (setq seen spec) nil)))
        (org-canvas-sync-files))
      (expect (plist-get seen :hash) :to-be 'push)
      (expect (plist-get seen :dry-run) :to-be 'push)
      (expect (plist-get seen :query) :to-equal "LEVEL>0")
      (expect (plist-get seen :title-key) :to-be :display-name)
      (expect (plist-get seen :prepare) :to-be #'org-canvas--file-sync-prepare)
      (expect (plist-get seen :after-sync) :to-be #'org-canvas--file-sync-after))))

(provide 'org-canvas-files-test)
;;; org-canvas-files-test.el ends here
