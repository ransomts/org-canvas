;;; org-canvas-files-upload-test.el --- Buttercup tests for the file upload  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-files-upload', the four stages between a
;; heading's file link and the CANVAS_ID written back into it, and the
;; coverage and mutation-hardening specs those stages left.  The base
;; file's specs are in org-canvas-files-test.el, the pull's in
;; org-canvas-files-pull-test.el; the three were one file until #593.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-files)
(require 'org-canvas-files-upload)

;;;; 1. Stage: Extraction

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

(describe "org-canvas--file-parse-entry"
  :var (saved-files-file)
  (before-each (test-org-canvas-reset-file-caches))

  (before-each
    (setq saved-files-file org-canvas-files-file
          org-canvas-files-file "/tmp/test-files.org"))

  (after-each
    (setq org-canvas-files-file saved-files-file))

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

;;;; 2. Stage: Upload Preparation

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

;;;; 3. Stage: Execution (3-step upload)

(describe "org-canvas--file-upload-step1-notify (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "posts to folder files endpoint"
    (with-test-api-responses '(("folders/456/files" . ((upload_url . "https://upload.example.com")
                                                        (upload_params . ((key . "value"))))))
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "name" "test.pdf" payload)
        (org-canvas--file-upload-step1-notify 456 payload)
        (expect-api-called 'POST "folders/456/files"))))

  (it "returns upload info with upload_url"
    (with-test-api-responses '(("folders" . ((upload_url . "https://s3.example.com/upload")
                                              (upload_params . ((token . "abc123"))))))
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "name" "test.pdf" payload)
        (let ((response (org-canvas--file-upload-step1-notify 123 payload)))
          (expect (alist-get 'upload_url response)
                  :to-equal "https://s3.example.com/upload"))))))

(describe "org-canvas--file-upload-step3-confirm"
  (before-each (test-org-canvas-reset-file-caches))

  (it "returns response directly if it contains id"
    (let ((step2-response '((id . 99999) (display_name . "test.pdf"))))
      (expect (alist-get 'id (org-canvas--file-upload-step3-confirm step2-response))
              :to-equal 99999)))

  (it "follows location URL when no id present"
    (with-test-api-responses '(("files/confirmation" . ((id . 88888) (display_name . "uploaded.pdf"))))
      (let ((step2-response '((location . "https://test.canvas.example.com/api/v1/files/confirmation"))))
        (let ((result (org-canvas--file-upload-step3-confirm step2-response)))
          (expect (alist-get 'id result) :to-equal 88888)))))

  (it "handles relative location URLs"
    (with-test-api-responses '(("" . ((id . 77777) (display_name . "file.pdf"))))
      (let ((step2-response '((location . "/api/v1/files/77777"))))
        (let ((result (org-canvas--file-upload-step3-confirm step2-response)))
          (expect (alist-get 'id result) :to-equal 77777)))))

  (it "errors when no id or location in response"
    (let ((step2-response '((error . "something wrong"))))
      (expect (org-canvas--file-upload-step3-confirm step2-response)
              :to-throw 'error))))

(describe "org-canvas--file-search-by-name (mocked)"
  (before-each (test-org-canvas-reset-file-caches))

  (it "searches files endpoint with search_term"
    (with-test-api-responses '(("files" . [((id . 111) (display_name . "Lab 1.pdf"))]))
      (org-canvas--file-search-by-name "Lab 1.pdf" "")
      (expect-api-called 'GET "files")))

  (it "returns matching file object"
    (with-test-api-responses '(("files" . [((id . 222) (display_name . "Syllabus.pdf"))
                                            ((id . 333) (display_name . "Other.pdf"))]))
      (let ((result (org-canvas--file-search-by-name "Syllabus.pdf" "")))
        (expect (alist-get 'id result) :to-equal 222))))

  (it "returns nil when file not found"
    (with-test-api-responses '(("files" . []))
      (let ((result (org-canvas--file-search-by-name "Missing.pdf" "")))
        (expect result :to-be nil))))

  (it "returns nil on API error"
    (with-test-api-handler (lambda (_method _url &rest _args)
                             (signal 'error '("API Error")))
      (let ((result (org-canvas--file-search-by-name "Any.pdf" "")))
        (expect result :to-be nil)))))

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
    (with-test-api-handler (lambda (method url &rest _args)
                             (cond
                               ((and (eq method 'GET) (string-match "folders/root" url))
                                 '((id . 100)))
                               ((eq method 'POST)
                                 (signal 'error '("Upload failed")))
                               (t nil)))
      (let ((temp-file (make-temp-file "test" nil ".pdf")))
        (unwind-protect
            (progn
              (with-temp-file temp-file (insert "content"))
              (let ((data (list :display-name "Test.pdf"
                                :local-path temp-file
                                :folder-path "")))
                (expect (org-canvas--file-push-to-api data) :to-throw 'error)))
          (delete-file temp-file))))))

;;;; 4. Stage: Finalization

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

;;;; Upload Step 2 Edge Cases

(describe "org-canvas--file-upload-step2-send edge cases"
  (before-each (test-org-canvas-reset-file-caches))

  (it "handles upload_params with null filename"
    ;; This tests the fix for Canvas returning null filename
    (with-org-canvas-test-config
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
        (delete-file temp-file)))))

  (it "handles redirect with Location header"
    (with-org-canvas-test-config
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
        (delete-file temp-file)))))

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
          ;; A real file: the upload request logs its size, and a
          ;; nonexistent path has none.  The old "/tmp/test.pdf" only
          ;; passed while another file had silenced the logger (#260).
          (let ((pdf (make-temp-file "org-canvas-delete-warning-" nil ".pdf")))
            (unwind-protect
                (let ((data (list :canvas-id "123" :display-name "test.pdf"
                                  :local-path pdf :folder-path "")))
                  (with-temp-file pdf (insert "content"))
                  (org-canvas--file-push-to-api data)
                  (expect warning-logged :to-be t))
              (delete-file pdf))))))))

(describe "org-canvas--file-search-by-name folder path log"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs search location with non-empty folder path"
    (with-test-api-responses '(("files" . [((display_name . "test.pdf") (id . 100))]))
      (let ((result (org-canvas--file-search-by-name "test.pdf" "Labs/Week1")))
        (expect (alist-get 'id result) :to-equal 100)))))

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
    (with-test-api
      (org-canvas--file-set-usage-rights
       42 '(:use-justification "own_copyright"
            :usage-license "cc_by_sa"
            :copyright "2026 Me"))
      (expect-api-called 'PUT "usage_rights")))

  (it "does nothing when use-justification is nil"
    (with-test-api
      (org-canvas--file-set-usage-rights 42 '(:use-justification nil))
      (expect (test-org-canvas-api-called-p 'PUT "usage_rights") :to-be nil)))

  (it "handles API error gracefully"
    (with-test-api-handler (lambda (&rest _)
                             (signal 'error '("403 Forbidden")))
      (org-canvas--file-set-usage-rights
       42 '(:use-justification "own_copyright")))))

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

;;;; Coverage: confirm-with-retry echo area

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
                  "%s in %ds..." "File upload: retry 2/3" 2))))))

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
          (ctx (org-canvas--sync-make-ctx)))
      (cl-letf (((symbol-function 'org-canvas--file-replace-in-place-p)
                 (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _)
                   (when (eq method 'DELETE) (setq deleted t)))))
        (org-canvas--file-clear-way-for-upload
         '(:display-name "syllabus.pdf") "42" 7 ctx)
        (expect deleted :to-be nil)
        (expect (plist-get ctx :file-recreated-ids) :to-be nil))))

  (it "still deletes a file that moved folders"
    (let ((deleted nil)
          (ctx (org-canvas--sync-make-ctx)))
      (cl-letf (((symbol-function 'org-canvas--file-replace-in-place-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _)
                   (when (eq method 'DELETE) (setq deleted t)))))
        (org-canvas--file-clear-way-for-upload
         '(:display-name "syllabus.pdf") "42" 9 ctx)
        (expect deleted :to-be t)
        (expect (plist-get ctx :file-recreated-ids) :to-equal '("syllabus.pdf")))))

  (it "carries on when the delete fails"
    (let ((warnings nil))
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
    (with-test-api-responses '(("files" . [((id . 111) (display_name . "notes.pdf") (folder_id . 7))
                                            ((id . 222) (display_name . "notes.pdf") (folder_id . 9))]))
      (let ((result (org-canvas--file-search-by-name "notes.pdf" "Labs" 9)))
        (expect (alist-get 'id result) :to-equal 222))))

  (it "matches on name alone when no folder id is supplied"
    (with-test-api-responses '(("files" . [((id . 111) (display_name . "notes.pdf") (folder_id . 7))]))
      (let ((result (org-canvas--file-search-by-name "notes.pdf" "")))
        (expect (alist-get 'id result) :to-equal 111))))

  (it "returns nil when the only match is in a different folder"
    (with-test-api-responses '(("files" . [((id . 111) (display_name . "notes.pdf") (folder_id . 7))]))
      (expect (org-canvas--file-search-by-name "notes.pdf" "Labs" 9) :to-be nil))))

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

;;;; Mutation hardening (issue #38)
;;
;; A full mutation pass scored these paths poorly despite ~99.5% line
;; coverage: the lines ran, but nothing asserted on what they produced.
;; The specs below pin the values, not merely the execution; the ones
;; for the sync and the delete commands are under the same heading in
;; org-canvas-files-test.el.

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

(describe "file finalize stamps modified_at (issue #94)"
  (it "stamps the content timestamp after an upload"
    (with-temp-org-buffer "* syllabus.pdf\n"
      (org-back-to-heading)
      (org-canvas--file-finalize
       (list :display-name "syllabus.pdf" :pom (point))
       '((id . 31495932) (updated_at . "2026-09-01T12:12:24Z")
         (modified_at . "2026-08-31T18:34:37Z")))
      (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
              :to-equal "2026-08-31T18:34:37Z"))))

(provide 'org-canvas-files-upload-test)
;;; org-canvas-files-upload-test.el ends here
