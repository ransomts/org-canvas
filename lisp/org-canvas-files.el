;;; org-canvas-files.el --- Pipeline-based File Sync -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the sync pipeline for Canvas Files.
;;
;; FILE STRUCTURE
;; ==============
;; In files.org:
;;   - Headings with file links = Files to upload
;;     Example: [[file:materials/syllabus.pdf][Course Syllabus]]
;;   - Headings without links = Folder containers
;;     Example: * Course Materials (creates "Course Materials" folder)
;;   - Nesting creates folder hierarchy
;;     * Labs
;;     ** [[file:lab1.zip][Lab 1 Starter]]  -> uploads to Labs/ folder
;;
;; 3-STEP UPLOAD PROCESS
;; =====================
;; Canvas uses a unique file upload flow:
;;
;;   Step 1: Notify Canvas
;;     POST /api/v1/folders/:folder_id/files
;;     Body: { name, size, content_type }
;;     Response: { upload_url, upload_params }
;;
;;   Step 2: Upload File Content
;;     POST to upload_url (NOT the Canvas API)
;;     Body: multipart/form-data with upload_params + file
;;     Response: Either file object or Location header
;;
;;   Step 3: Confirm Upload (if needed)
;;     GET the Location URL (authenticated)
;;     Returns: Final file object with Canvas ID
;;
;; QUIRKS
;; ======
;; - Canvas returns upload_params with null filename and unknown/unknown
;;   content_type.  We override these with actual values.
;; - The 'file' parameter MUST be last in the multipart form.
;; - Step 3 may not be needed if Step 2 returns the file object directly.
;;
;; SESSION CACHING
;; ===============
;; Folder lookups are cached per sync session to avoid redundant API calls.
;; Caches are cleared at the start of each sync operation.

;;; Code:

(require 'org-canvas-core)
(require 'elog)
(require 'cl-lib)
(require 'url-util)
(require 'json)

;;;; Configuration

(defcustom org-canvas-files-file (org-canvas--path "files.org")
  "Path to the files.org file."
  :type 'file
  :group 'org-canvas)

(defcustom org-canvas-max-file-size-mb 500
  "Maximum file size in MB.  Files larger than this are skipped with a warning."
  :type 'integer
  :group 'org-canvas)

;;;; Session State (cleared at start of each sync)

(defvar org-canvas--file-root-folder-cache nil
  "Cached root folder object for the current sync session.
Cleared at the start of each sync operation to avoid stale data.")

(defvar org-canvas--file-folder-cache nil
  "Hash table mapping folder paths to folder objects for the current sync session.
Keys are full folder paths like \"Labs/Week 01\", values are folder API objects.
Cleared at the start of each sync operation.")

;;;; Helper Functions

(defun org-canvas--file-extract-link-path (heading-text)
  "Extract the file path from HEADING-TEXT containing an Org link.
Handles [[file:path][name]], [[pdf:path::page][name]], and similar formats.
Returns nil if no file link found."
  (when heading-text
    (cond
     ;; Match [[file:path][name]]
     ((string-match "\\[\\[file:\\([^]]+\\)\\]\\[" heading-text)
      (match-string 1 heading-text))
     ;; Match [[pdf:path::page][name]] (with optional page reference)
     ((string-match "\\[\\[pdf:\\([^]:]+\\)" heading-text)
      (match-string 1 heading-text))
     ;; Match bare [[file:path]]
     ((string-match "\\[\\[file:\\([^]]+\\)\\]\\]" heading-text)
      (match-string 1 heading-text)))))

(defun org-canvas--file-get-display-name (heading-text)
  "Extract the display name from HEADING-TEXT.
For links like [[file:path][name]], returns name.
For plain headings, returns the heading text."
  (if (and heading-text (string-match "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" heading-text))
      (match-string 1 heading-text)
    heading-text))

(defun org-canvas--file-get-folder-path (_pom _files-file-dir)
  "Build the Canvas folder path for the entry at point.
Uses ancestor headings that don't have file links to build the path."
  (let ((path-parts nil)
        (current-level (org-current-level)))
    (save-excursion
      (while (and current-level (> current-level 1))
        (org-up-heading-safe)
        (let* ((heading (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
               (link-path (org-canvas--file-extract-link-path heading)))
          ;; Only include headings that are folders (no file link)
          (unless link-path
            (push heading path-parts)))
        (setq current-level (org-current-level))))
    (if path-parts
        (mapconcat #'identity path-parts "/")
      "")))

(defun org-canvas--file-guess-content-type (filename)
  "Guess the MIME content type based on FILENAME extension."
  (let ((ext (downcase (or (file-name-extension filename) ""))))
    (cond
     ;; Documents
     ((string= ext "pdf") "application/pdf")
     ((member ext '("doc" "docx")) "application/msword")
     ((member ext '("xls" "xlsx")) "application/vnd.ms-excel")
     ((member ext '("ppt" "pptx")) "application/vnd.ms-powerpoint")
     ;; Code
     ((member ext '("py" "python")) "text/x-python")
     ((member ext '("js" "javascript")) "application/javascript")
     ((member ext '("html" "htm")) "text/html")
     ((member ext '("css")) "text/css")
     ((member ext '("json")) "application/json")
     ((member ext '("xml")) "application/xml")
     ((member ext '("txt" "text")) "text/plain")
     ((member ext '("md" "markdown")) "text/markdown")
     ((member ext '("csv")) "text/csv")
     ;; Images
     ((member ext '("png")) "image/png")
     ((member ext '("jpg" "jpeg")) "image/jpeg")
     ((member ext '("gif")) "image/gif")
     ((member ext '("svg")) "image/svg+xml")
     ;; Archives
     ((member ext '("zip")) "application/zip")
     ((member ext '("gz" "gzip")) "application/gzip")
     ((member ext '("tar")) "application/x-tar")
     ;; Default
     (t "application/octet-stream"))))

;;;; Folder Operations

(defun org-canvas--file-get-root-folder ()
  "Get the root folder for the course (cached per sync session)."
  (if org-canvas--file-root-folder-cache
      (progn
        (elog-debug org-canvas--logger "[Files] Using cached root folder ID: %s"
          (alist-get 'id org-canvas--file-root-folder-cache))
        org-canvas--file-root-folder-cache)
    (elog-debug org-canvas--logger "[Files] Getting root folder...")
    (let* ((endpoint (org-canvas-api-course-endpoint "folders/root"))
           (response (org-canvas-api-request 'GET endpoint)))
      (elog-debug org-canvas--logger "[Files] Root folder ID: %s" (alist-get 'id response))
      (setq org-canvas--file-root-folder-cache response)
      response)))

(defun org-canvas--file-get-or-create-folder (folder-path parent-folder-id)
  "Get or create a folder at FOLDER-PATH under PARENT-FOLDER-ID.
Return the folder object."
  (if (or (null folder-path) (string-empty-p folder-path))
      ;; Empty path means use parent directly
      (org-canvas-api-request 'GET (format "%s/api/v1/folders/%s"
                                           org-canvas-base-url parent-folder-id))
    ;; Try to resolve the path, create if not found
    (condition-case _err
        (let* ((encoded-path (url-hexify-string folder-path))
               (endpoint (org-canvas-api-course-endpoint "folders/by_path/%s" encoded-path))
               (folders (org-canvas-api-request 'GET endpoint)))
          ;; Returns list of folders in path, last one is the target
          (if (and folders (> (length folders) 0))
              (elt folders (1- (length folders)))
            (org-canvas--file-create-folder folder-path parent-folder-id)))
      (error
       ;; Folder doesn't exist, create it
       (org-canvas--file-create-folder folder-path parent-folder-id)))))

(defun org-canvas--file-create-folder (folder-path parent-folder-id)
  "Create a folder at FOLDER-PATH under PARENT-FOLDER-ID."
  (elog-info org-canvas--logger "[Files] Creating folder: %s" folder-path)
  (let* ((endpoint (org-canvas-api-course-endpoint "folders"))
         (payload (make-hash-table :test 'equal)))
    (puthash "name" (file-name-nondirectory folder-path) payload)
    (puthash "parent_folder_path" (file-name-directory folder-path) payload)
    (puthash "parent_folder_id" parent-folder-id payload)
    (condition-case err
        (let ((response (org-canvas-api-request 'POST endpoint :data payload)))
          (elog-info org-canvas--logger "[Files] Created folder ID: %s" (alist-get 'id response))
          response)
      (error
       ;; If it already exists, try to get it
       (elog-debug org-canvas--logger "[Files] Folder creation failed (may exist): %s" (cadr err))
       (org-canvas--file-resolve-folder-by-path folder-path)))))

(defun org-canvas--file-resolve-folder-by-path (folder-path)
  "Resolve a folder by its path (FOLDER-PATH), creating parent folders if needed.
Uses session cache to avoid redundant API calls."
  ;; Check cache first
  (let ((cached (and org-canvas--file-folder-cache
                     (gethash folder-path org-canvas--file-folder-cache))))
    (if cached
        (progn
          (elog-debug org-canvas--logger "[Files] Using cached folder for path: %s" folder-path)
          cached)
      ;; Not cached - resolve and cache each level
      (let* ((parts (split-string folder-path "/" t))
             (root (org-canvas--file-get-root-folder))
             (current-folder root)
             (current-path ""))
        (dolist (part parts)
          (setq current-path (if (string-empty-p current-path)
                                 part
                               (concat current-path "/" part)))
          ;; Check if this intermediate path is cached
          (let ((cached-intermediate (and org-canvas--file-folder-cache
                                          (gethash current-path org-canvas--file-folder-cache))))
            (if cached-intermediate
                (progn
                  (elog-debug org-canvas--logger "[Files] Using cached folder for: %s" current-path)
                  (setq current-folder cached-intermediate))
              ;; Not cached - resolve and cache it
              (setq current-folder (org-canvas--file-ensure-subfolder current-folder part))
              (when org-canvas--file-folder-cache
                (puthash current-path current-folder org-canvas--file-folder-cache)
                (elog-debug org-canvas--logger "[Files] Cached folder: %s -> ID %s"
                  current-path (alist-get 'id current-folder))))))
        current-folder))))

(defun org-canvas--file-ensure-subfolder (parent-folder folder-name)
  "Ensure FOLDER-NAME exists under PARENT-FOLDER, creating it if needed.
PARENT-FOLDER is a Canvas folder API object (alist with \\='id key).
Returns the folder API object for the subfolder."
  (let* ((parent-id (alist-get 'id parent-folder))
         (endpoint (format "%s/api/v1/folders/%s/folders" org-canvas-base-url parent-id))
         (existing (append (org-canvas-api-request 'GET endpoint) nil))
         (found (cl-find-if (lambda (f)
                              (string= (alist-get 'name f) folder-name))
                            existing)))
    (or found
        (let ((payload (make-hash-table :test 'equal)))
          (puthash "name" folder-name payload)
          (org-canvas-api-request 'POST endpoint :data payload)))))

;;;; 1. Stage: Extraction

(defun org-canvas--file-parse-entry ()
  "Extract file data from the Org heading at point.
Returns nil for folder-only headings (no file link)."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         ;; Get raw heading text with link syntax preserved.
         ;; org-get-heading strips [[...][...]] on Emacs 30 / Org 9.7+.
         (heading-with-links
          (save-excursion
            (beginning-of-line)
            (when (looking-at org-complex-heading-regexp)
              (match-string-no-properties 4))))
         (raw-heading (or heading-with-links (org-get-heading t t t t)))
         (link-path (org-canvas--file-extract-link-path raw-heading))
         (display-name (org-canvas--file-get-display-name raw-heading))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (published (org-canvas-org-get-boolean-property pom "PUBLISHED" t))
         (unlock-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "UNLOCK_AT")))
         (lock-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "LOCK_AT")))
         (files-dir (file-name-directory org-canvas-files-file)))

    (elog-debug org-canvas--logger "[Stage 1: Parse] Heading text: %s" raw-heading)
    (elog-debug org-canvas--logger "[Stage 1: Parse] Link path: %s" (or link-path "NONE"))

    ;; Only process entries with file links
    (if (not link-path)
        (progn
          (elog-debug org-canvas--logger "[Stage 1: Parse] Skipping folder heading: %s" raw-heading)
          nil)
      (let* ((folder-path (org-canvas--file-get-folder-path pom files-dir))
             (abs-path (expand-file-name link-path files-dir)))

        (elog-info org-canvas--logger "[Stage 1: Parse] Processing File: '%s' (ID: %s)"
          display-name (or canvas-id "NEW"))
        (elog-debug org-canvas--logger "[Stage 1: Parse] Local path: %s" abs-path)
        (elog-debug org-canvas--logger "[Stage 1: Parse] Canvas folder: %s" (if (string-empty-p folder-path) "root" folder-path))

        (unless (file-exists-p abs-path)
          (elog-error org-canvas--logger "[Stage 1: Parse] File not found: %s" abs-path)
          (error "File not found: %s" abs-path))

        (let ((size-mb (/ (file-attribute-size (file-attributes abs-path)) org-canvas--bytes-per-mb)))
          (when (> size-mb org-canvas-max-file-size-mb)
            (elog-warning org-canvas--logger
              "[Stage 1: Parse] Skipping '%s': %.1f MB exceeds limit of %d MB"
              display-name size-mb org-canvas-max-file-size-mb)
            (error "File '%s' (%.1f MB) exceeds max size of %d MB"
                   display-name size-mb org-canvas-max-file-size-mb)))

        (list :display-name display-name
              :local-path abs-path
              :folder-path folder-path
              :canvas-id canvas-id
              :published published
              :unlock-at unlock-at
              :lock-at lock-at
              :pom pom)))))

;;;; 2. Stage: Upload Preparation

(defun org-canvas--file-build-upload-request (data _folder-id)
  "Build the upload notification payload for DATA."
  (let* ((local-path (plist-get data :local-path))
         (display-name (plist-get data :display-name))
         (file-size (file-attribute-size (file-attributes local-path)))
         (content-type (org-canvas--file-guess-content-type local-path))
         (payload (make-hash-table :test 'equal)))

    (elog-info org-canvas--logger "[Stage 2: Prepare] Building upload for '%s'" display-name)
    (elog-debug org-canvas--logger "[Stage 2: Prepare] Size: %d bytes, Type: %s" file-size content-type)

    (puthash "name" display-name payload)
    (puthash "size" file-size payload)
    (puthash "content_type" content-type payload)
    (puthash "on_duplicate" "overwrite" payload)

    ;; Add visibility settings
    (unless (plist-get data :published)
      (puthash "hidden" t payload))
    (when (plist-get data :unlock-at)
      (puthash "unlock_at" (plist-get data :unlock-at) payload))
    (when (plist-get data :lock-at)
      (puthash "lock_at" (plist-get data :lock-at) payload))

    payload))

;;;; 3. Stage: Execution (3-step upload)

(defconst org-canvas--file-upload-confirm-retries 3
  "Number of retry attempts when confirming a file upload (step 3).")

(defconst org-canvas--folder-creation-delay 1
  "Seconds to wait after creating folders before uploading files.
Canvas needs time to process newly created folders.")

(defun org-canvas--file-upload-step1-notify (folder-id payload)
  "Step 1: Notify Canvas about the upcoming upload.
FOLDER-ID is the target folder.
PAYLOAD contains file metadata.
Returns the upload parameters."
  (elog-info org-canvas--logger "[Stage 3: Upload Step 1] Notifying Canvas...")
  (let* ((endpoint (format "%s/api/v1/folders/%s/files" org-canvas-base-url folder-id))
         (response (org-canvas-api-request 'POST endpoint :data payload)))
    (elog-debug org-canvas--logger "[Stage 3: Upload Step 1] Got upload URL: %s"
      (alist-get 'upload_url response))
    response))

(defun org-canvas--file-build-multipart-body (upload-params local-path boundary)
  "Build a multipart/form-data body for file upload.
UPLOAD-PARAMS is the alist of parameters from Canvas step 1 response.
LOCAL-PATH is the path to the local file to upload.
BOUNDARY is the multipart boundary string.
Returns the full unibyte body string ready for HTTP upload."
  (let ((body-parts nil)
        (file-content (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally local-path)
                        (buffer-string)))
        (actual-filename (file-name-nondirectory local-path))
        (actual-content-type (org-canvas--file-guess-content-type local-path)))

    (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Building multipart form with params: %s"
      (mapconcat (lambda (p) (format "%s" (car p))) upload-params ", "))

    ;; Add all the upload params from Canvas, but fix null values
    (dolist (param (append upload-params nil))
      (let* ((key (car param))
             (raw-value (cdr param))
             ;; Fix null or bad values from Canvas
             (value (cond
                     ;; filename is null - use actual filename
                     ((and (eq key 'filename) (null raw-value))
                      actual-filename)
                     ;; content_type is unknown - use guessed type
                     ((and (eq key 'content_type)
                           (or (null raw-value)
                               (string= raw-value "unknown/unknown")))
                      actual-content-type)
                     ;; Otherwise use the value Canvas provided
                     (t raw-value))))
        (when value  ; Skip if still nil
          (push (format "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s"
                        boundary key value)
                body-parts))))

    ;; Then add the file - MUST be last parameter per Canvas docs
    (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Adding file parameter LAST: %s" actual-filename)
    (push (format "--%s\r\nContent-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\nContent-Type: %s\r\n\r\n"
                  boundary actual-filename actual-content-type)
          body-parts)

    ;; Note: body-parts is built in reverse (push), then nreverse'd below
    ;; Final order will be: [upload_params...] then [file] - file is last as required
    ;; Encode form parts as unibyte so the entire body stays unibyte when
    ;; concatenated with binary file content (avoids "Multibyte text in HTTP
    ;; request" errors from url.el for PDFs, images, etc.)
    (let* ((body-prefix (encode-coding-string
                         (concat (mapconcat #'identity (nreverse body-parts) "\r\n") "\r\n")
                         'raw-text))
           (body-suffix (encode-coding-string
                         (format "\r\n--%s--\r\n" boundary)
                         'raw-text)))
      (concat body-prefix file-content body-suffix))))

(defun org-canvas--file-parse-upload-response (local-path upload-url)
  "Parse the HTTP response in the current buffer after file upload.
LOCAL-PATH is the path to the uploaded file (for logging).
UPLOAD-URL is the URL the file was uploaded to (for logging).
Must be called in the `url-retrieve' response buffer.
Returns either a file object alist, a location alist, or signals an error."
  (goto-char (point-min))
  ;; Parse headers and body BEFORE killing buffer
  (let ((location-header nil)
        (status-line nil)
        (json-response nil)
        (response-body nil))
    ;; Get status line
    (when (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
      (setq status-line (match-string 1))
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] HTTP status: %s" status-line))
    ;; Look for Location header in the headers section
    (save-excursion
      (when (re-search-forward "^[Ll]ocation: \\(.*\\)\r?$" nil t)
        (setq location-header (string-trim (match-string 1)))))
    ;; Find the response body (after headers)
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (setq response-body (buffer-substring-no-properties (point) (point-max)))
      (setq json-response (condition-case nil
                              (json-read-from-string response-body)
                            (error nil))))
    ;; Now we can kill the buffer
    (kill-buffer)
    ;; Log HTTP errors with diagnostic details
    (when (and status-line (>= (string-to-number status-line) 400))
      (elog-error org-canvas--logger
        "[Stage 3: Upload Step 2] HTTP %s uploading '%s' to %s"
        status-line (file-name-nondirectory local-path) upload-url)
      (when response-body
        (elog-error org-canvas--logger
          "[Stage 3: Upload Step 2] Response: %s"
          (truncate-string-to-width response-body 500))))
    ;; Determine what to return
    (cond
     ;; If we got a JSON response with an ID, return it directly
     ((and json-response (alist-get 'id json-response))
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Got file object directly")
      json-response)
     ;; If we have a location header (3XX redirect or 201 Created), return it
     (location-header
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Got redirect to: %s" location-header)
      `((location . ,location-header)))
     ;; If we got some other JSON response, return it
     (json-response
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Got JSON response")
      json-response)
     ;; Otherwise error
     (t
      (error "Upload failed: no JSON response or Location header.  Status: %s, Body: %s"
             status-line (or response-body "empty"))))))

(defun org-canvas--file-upload-step2-send (upload-info local-path)
  "Step 2: Upload the actual file content.
UPLOAD-INFO contains the upload URL and parameters from step 1.
LOCAL-PATH is the path to the local file.
Return either a file object (if upload returns JSON) or an alist
with `location' key."
  (elog-info org-canvas--logger "[Stage 3: Upload Step 2] Uploading file content...")
  (let* ((upload-url (alist-get 'upload_url upload-info))
         (upload-params (alist-get 'upload_params upload-info))
         (boundary (format "----FormBoundary%s" (md5 (format "%s%s" (current-time) (random))))))

    ;; Log upload_params from Canvas (these must be sent exactly as-is)
    (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] upload_url: %s" upload-url)
    (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] upload_params from Canvas: %S" upload-params)

    ;; Build multipart form data
    (let* ((full-body (org-canvas--file-build-multipart-body upload-params local-path boundary))
           ;; Use url-retrieve-synchronously for the upload (not to Canvas API directly)
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
           (url-request-data full-body))

      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Sending to %s" upload-url)

      (with-current-buffer (url-retrieve-synchronously upload-url nil nil 120)
        (org-canvas--file-parse-upload-response local-path upload-url)))))

(defun org-canvas--file-upload-step3-confirm (step2-response)
  "Step 3: Confirm the upload and get the file object.
STEP2-RESPONSE is the response from step 2.
Per Canvas docs, this GET request must be authenticated."
  (elog-info org-canvas--logger "[Stage 3: Upload Step 3] Confirming upload...")

  ;; Check if we already have the file object (some uploads return it directly)
  (if (alist-get 'id step2-response)
      (progn
        (elog-debug org-canvas--logger "[Stage 3: Upload Step 3] File object returned directly")
        step2-response)
    ;; Otherwise we need to follow the location URL with an authenticated GET
    (let ((location (alist-get 'location step2-response)))
      (if location
          (let* ((full-url (if (string-prefix-p "http" location)
                               location
                             (concat org-canvas-base-url location)))
                 (max-retries org-canvas--file-upload-confirm-retries)
                 (attempt 0)
                 (response nil)
                 (last-err nil))
            ;; Retry loop with exponential backoff
            (while (and (< attempt max-retries) (null response))
              (setq attempt (1+ attempt))
              (condition-case err
                  (progn
                    (when (> attempt 1)
                      (let ((delay (expt 2 (1- attempt))))
                        (elog-warning org-canvas--logger
                          "[Stage 3: Upload Step 3] Retry %d/%d after %ds..."
                          attempt max-retries delay)
                        (sleep-for delay)))
                    (setq response (org-canvas-api-request 'GET full-url)))
                (error
                 (setq last-err err)
                 (elog-warning org-canvas--logger
                   "[Stage 3: Upload Step 3] Attempt %d/%d failed: %s"
                   attempt max-retries (error-message-string err)))))
            (if response
                (progn
                  (elog-debug org-canvas--logger
                    "[Stage 3: Upload Step 3] Confirmed, file ID: %s"
                    (alist-get 'id response))
                  (unless (alist-get 'id response)
                    (elog-warning org-canvas--logger
                      "[Stage 3: Upload Step 3] No ID in confirmation response: %S"
                      response))
                  response)
              (elog-error org-canvas--logger
                "[Stage 3: Upload Step 3] All %d confirmation attempts failed. File may exist on Canvas without local tracking."
                max-retries)
              (signal (car last-err) (cdr last-err))))
        (error "No file ID or location in upload response")))))

(defun org-canvas--file-push-to-api (data)
  "Execute the full 3-step upload process for DATA."
  (let* ((canvas-id (plist-get data :canvas-id))
         (display-name (plist-get data :display-name))
         (local-path (plist-get data :local-path))
         (folder-path (plist-get data :folder-path)))

    (elog-info org-canvas--logger "[Stage 3: Execute] Uploading '%s'" display-name)

    ;; If we already have a canvas ID, we're updating - delete old and re-upload
    ;; (Canvas doesn't support direct file content updates)
    (when canvas-id
      (elog-debug org-canvas--logger "[Stage 3: Execute] Replacing existing file ID: %s" canvas-id)
      (condition-case err
          (org-canvas-api-request 'DELETE (format "%s/api/v1/files/%s" org-canvas-base-url canvas-id))
        (error
         (elog-warning org-canvas--logger "[Stage 3: Execute] Could not delete old file: %s" (cadr err)))))

    ;; Get or create the target folder
    (let* ((root-folder (org-canvas--file-get-root-folder))
           (target-folder (if (string-empty-p folder-path)
                              root-folder
                            (org-canvas--file-resolve-folder-by-path folder-path)))
           (folder-id (alist-get 'id target-folder)))

      (elog-debug org-canvas--logger "[Stage 3: Execute] Target folder ID: %s" folder-id)

      ;; Build upload payload
      (let ((payload (org-canvas--file-build-upload-request data folder-id)))

        ;; Execute 3-step upload
        (condition-case err
            (let* ((_ (message "Files: '%s' step 1/3 (notifying Canvas)..." display-name))
                   (step1-response (org-canvas--file-upload-step1-notify folder-id payload))
                   (_ (message "Files: '%s' step 2/3 (uploading %s)..."
                        display-name (file-name-nondirectory local-path)))
                   (step2-response (org-canvas--file-upload-step2-send step1-response local-path))
                   (_ (message "Files: '%s' step 3/3 (confirming)..." display-name))
                   (final-response (org-canvas--file-upload-step3-confirm step2-response)))
              (elog-info org-canvas--logger "[Stage 3: Execute] Upload complete for '%s'" display-name)
              final-response)
          (error
           (elog-error org-canvas--logger
             "[Stage 3: Execute] Upload failed for '%s' (path: %s, folder: %s): %s"
             display-name local-path
             (if (string-empty-p folder-path) "root" folder-path)
             (error-message-string err))
           (signal (car err) (cdr err))))))))

(defun org-canvas--file-find-by-name (display-name folder-path)
  "Search for a file with DISPLAY-NAME in FOLDER-PATH on Canvas."
  (elog-info org-canvas--logger "[Stage 3: Search] Looking for '%s' in '%s'..."
    display-name (if (string-empty-p folder-path) "root" folder-path))
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "files"))
             (params `(("search_term" . ,display-name)))
             (results (append (org-canvas-api-request 'GET endpoint :params params) nil)))
        (elog-debug org-canvas--logger "[Stage 3: Search] Found %d results" (length results))
        (cl-find-if (lambda (f)
                      (string= (alist-get 'display_name f) display-name))
                    results))
    (error
     (elog-warning org-canvas--logger "[Stage 3: Search] Search failed: %s" (cadr err))
     nil)))

;;;; 4. Stage: Finalization

(defun org-canvas--file-finalize (data response)
  "Update local Org file with CANVAS_ID using DATA and RESPONSE."
  (org-canvas--finalize-item data response :title-key :display-name))

;;;; Pre-flight Folder Creation

(defun org-canvas--file-collect-folder-paths (files-file)
  "Collect all unique folder paths from FILES-FILE.
Returns a list of folder paths that need to exist for file uploads."
  (let ((folder-paths nil))
    (with-current-buffer (find-file-noselect files-file)
      (org-map-entries
       (lambda ()
         (let* ((heading-with-links
                 (save-excursion
                   (beginning-of-line)
                   (when (looking-at org-complex-heading-regexp)
                     (match-string-no-properties 4))))
                (raw-heading (or heading-with-links (org-get-heading t t t t)))
                (link-path (org-canvas--file-extract-link-path raw-heading)))
           ;; Only process actual files (headings with links)
           (when link-path
             (let ((folder-path (org-canvas--file-get-folder-path
                                 (point)
                                 (file-name-directory files-file))))
               (unless (or (string-empty-p folder-path)
                           (member folder-path folder-paths))
                 (push folder-path folder-paths))))))
       t 'file))
    (nreverse folder-paths)))

(defun org-canvas--file-ensure-folders-exist (folder-paths)
  "Ensure all FOLDER-PATHS exist on Canvas before uploading files.
Creates folders as needed and populates the folder cache."
  (when folder-paths
    (elog-info org-canvas--logger "[Pre-flight] Ensuring %d folder path(s) exist..."
      (length folder-paths))
    (dolist (path folder-paths)
      (elog-info org-canvas--logger "[Pre-flight] Creating/verifying folder: %s" path)
      (condition-case err
          (org-canvas--file-resolve-folder-by-path path)
        (error
         (elog-error org-canvas--logger "[Pre-flight] Failed to create folder '%s': %s"
           path (error-message-string err))
         (error "Cannot create required folder '%s': %s" path (error-message-string err)))))
    ;; Brief delay to let Canvas process newly created folders
    (elog-debug org-canvas--logger "[Pre-flight] Waiting for Canvas to process folders...")
    (sleep-for org-canvas--folder-creation-delay)
    (elog-info org-canvas--logger "[Pre-flight] All folders ready")))

;;;; Main Sync Functions

;;;###autoload
(defun org-canvas-sync-files ()
  "Synchronize files to Canvas."
  (interactive)
  (org-canvas-clear-log)
  ;; Clear session caches
  (setq org-canvas--file-root-folder-cache nil)
  (setq org-canvas--file-folder-cache (make-hash-table :test 'equal))

  (let ((files-file (expand-file-name org-canvas-files-file)))
    (unless (and files-file (file-exists-p files-file))
      (error "Files manifest not found: %s" files-file))

    (display-buffer (get-buffer-create "*canvas-log*"))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> STARTING FILE SYNC")
    (elog-info org-canvas--logger "File: %s" files-file)
    (elog-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
    (elog-info org-canvas--logger "========================================")

    (elog-info org-canvas--logger "[Pre-flight] Verifying course access...")
    (condition-case err
        (progn
          (org-canvas-api-request 'GET (org-canvas-api-course-endpoint ""))
          (elog-info org-canvas--logger "[Pre-flight] Course accessible"))
      (error
       (elog-warning org-canvas--logger "[Pre-flight] Warning: %s" (cadr err))))

    ;; Pre-create all necessary folders before uploading any files
    (let ((folder-paths (org-canvas--file-collect-folder-paths files-file)))
      (org-canvas--file-ensure-folders-exist folder-paths))

    (let ((targets nil)
          (success-count 0)
          (fail-count 0)
          (skip-count 0))
      ;; Gather all entries (at any level)
      (with-current-buffer (find-file-noselect files-file)
        (setq targets (org-map-entries (lambda () (point-marker)) t 'file)))

      (elog-info org-canvas--logger "Found %d entries to process" (length targets))

      (dolist (marker targets)
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char (marker-position marker))
            (condition-case err
                (let ((data (org-canvas--file-parse-entry)))
                  (if data
                      (progn
                        (elog-info org-canvas--logger "----------------------------------------")
                        (let ((response (org-canvas--file-push-to-api data)))
                          (org-canvas--file-finalize data response)
                          (setq success-count (1+ success-count))
                          (message "Files [%d/%d] Synced '%s'"
                            (+ success-count skip-count fail-count)
                            (length targets)
                            (plist-get data :display-name))))
                    ;; Folder heading - skip
                    (setq skip-count (1+ skip-count))))
              (error
               (setq fail-count (1+ fail-count))
               (elog-error org-canvas--logger "[FAILED] At point %d: %s"
                 (marker-position marker) (error-message-string err))
               (message "Files [%d/%d] FAILED: %s"
                 (+ success-count skip-count fail-count)
                 (length targets)
                 (error-message-string err)))))))

      ;; Save the org file after all modifications
      (with-current-buffer (find-file-noselect files-file)
        (save-buffer)
        (elog-info org-canvas--logger "Saved %s" files-file))

      (elog-info org-canvas--logger "========================================")
      (elog-info org-canvas--logger ">>> FILE SYNC COMPLETE")
      (elog-info org-canvas--logger "Success: %d | Failed: %d | Skipped (folders): %d"
        success-count fail-count skip-count)
      (elog-info org-canvas--logger "========================================")
      (message "File Sync: %d success, %d failed, %d folders skipped."
               success-count fail-count skip-count))))

;;;; Delete Functions

(defun org-canvas--file-get-all-folders ()
  "Get all folders in the course, sorted by depth (deepest first for deletion)."
  (let* ((endpoint (org-canvas-api-course-endpoint "folders"))
         (params org-canvas--api-max-per-page)
         (folders (append (org-canvas-api-request 'GET endpoint :params params) nil))
         (root-folder (org-canvas--file-get-root-folder))
         (root-id (alist-get 'id root-folder)))
    ;; Filter out root folder (can't delete it) and sort by full_name length descending
    ;; (deeper folders have longer paths and should be deleted first)
    (sort (cl-remove-if (lambda (f) (= (alist-get 'id f) root-id)) folders)
          (lambda (a b)
            (> (length (or (alist-get 'full_name a) ""))
               (length (or (alist-get 'full_name b) "")))))))

(defun org-canvas--file-delete-all-folders ()
  "Delete all folders in the course (except root).  Return count deleted."
  (let ((folders (org-canvas--file-get-all-folders))
        (deleted-count 0))
    (elog-info org-canvas--logger "Found %d folders to delete (excluding root)" (length folders))
    (dolist (folder folders)
      (let* ((id (alist-get 'id folder))
             (name (alist-get 'full_name folder)))
        (elog-info org-canvas--logger "Deleting folder: '%s' (ID: %s)" name id)
        (condition-case err
            (progn
              (org-canvas-api-request 'DELETE (format "%s/api/v1/folders/%s" org-canvas-base-url id)
                                      :params '(("force" . "true")))
              (setq deleted-count (1+ deleted-count))
              (elog-info org-canvas--logger "  -> Deleted successfully"))
          (error
           (elog-error org-canvas--logger "  -> Delete failed: %s" (cadr err))))))
    deleted-count))

(defun org-canvas-delete-all-files ()
  "Delete ALL files and folders in the course (Danger Zone)."
  (interactive)
  (let ((files-file (expand-file-name org-canvas-files-file)))
    (if (and (not org-canvas--inhibit-log-clear)
             (not (y-or-n-p "Delete ALL files and folders in this course? ")))
        (message "Aborted.")
      (org-canvas-clear-log)
      ;; Clear session caches
      (setq org-canvas--file-root-folder-cache nil)
      (setq org-canvas--file-folder-cache (make-hash-table :test 'equal))
      (display-buffer (get-buffer-create "*canvas-log*"))
      (elog-warning org-canvas--logger "========================================")
      (elog-warning org-canvas--logger ">>> STARTING MASS DELETION OF FILES AND FOLDERS")
      (elog-warning org-canvas--logger "========================================")

      (let* ((endpoint (org-canvas-api-course-endpoint "files"))
             (params org-canvas--api-max-per-page)
             (remote-items (append (org-canvas-api-request 'GET endpoint :params params) nil))
             (deleted-ids nil)
             (deleted-file-count 0)
             (deleted-folder-count 0))

        (elog-info org-canvas--logger "Found %d files on Canvas" (length remote-items))

        ;; Delete all files first
        (dolist (item remote-items)
          (let* ((id (alist-get 'id item))
                 (name (alist-get 'display_name item)))
            (elog-info org-canvas--logger "Deleting file: '%s' (ID: %s)" name id)
            (condition-case err
                (progn
                  (org-canvas-api-request 'DELETE (format "%s/api/v1/files/%s" org-canvas-base-url id))
                  (push (number-to-string id) deleted-ids)
                  (setq deleted-file-count (1+ deleted-file-count))
                  (elog-info org-canvas--logger "  -> Deleted successfully"))
              (error
               (elog-error org-canvas--logger "  -> Delete failed: %s" (cadr err))))))

        ;; Delete all folders (after files are gone)
        (elog-info org-canvas--logger "----------------------------------------")
        (elog-info org-canvas--logger "Now deleting folders...")
        (setq deleted-folder-count (org-canvas--file-delete-all-folders))

        ;; Clean local properties
        (when (file-exists-p files-file)
          (elog-info org-canvas--logger "Cleaning local properties...")
          (with-current-buffer (find-file-noselect files-file)
            (org-map-entries
             (lambda ()
               (elog-debug org-canvas--logger "Removing properties for ID: %s"
                           (org-entry-get (point) "CANVAS_ID"))
               (org-canvas-clear-sync-properties (point)))
             "CANVAS_ID={.}" 'file)
            (save-buffer)))

        (elog-info org-canvas--logger "========================================")
        (elog-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d files, %d folders removed"
          deleted-file-count deleted-folder-count)
        (elog-info org-canvas--logger "========================================")
        (message "Deletion complete. %d files, %d folders removed."
                 deleted-file-count deleted-folder-count)))))

(defun org-canvas-delete-file-at-point ()
  "Delete the Canvas file associated with the current Org heading."
  (interactive)
  (org-back-to-heading t)
  (let* ((pom (point))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (raw-heading (org-get-heading t t t t))
         (display-name (org-canvas--file-get-display-name raw-heading)))

    (unless canvas-id
      (user-error "No CANVAS_ID property found for this heading"))

    (when (y-or-n-p (format "Delete '%s' from Canvas? " display-name))
      (org-canvas-clear-log)
      (display-buffer (get-buffer-create "*canvas-log*"))
      (elog-info org-canvas--logger "Deleting file '%s' (ID: %s)..." display-name canvas-id)

      (condition-case err
          (progn
            (org-canvas-api-request 'DELETE (format "%s/api/v1/files/%s" org-canvas-base-url canvas-id))
            (elog-info org-canvas--logger "Successfully deleted from Canvas")
            (org-canvas-clear-sync-properties pom)
            (elog-info org-canvas--logger "Cleaned local properties")
            (message "File '%s' deleted." display-name))
        (error
         (elog-error org-canvas--logger "Failed to delete: %s" (cadr err))
         (message "Failed to delete file. Check logs."))))))

;;;; Pull

;;;###autoload
(defun org-canvas-pull-files ()
  "Pull file metadata from Canvas into files.org.
Downloads file contents to the content/ directory."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> PULLING FILES")
  (elog-info org-canvas--logger "========================================")
  (let* ((file (expand-file-name org-canvas-files-file))
         (content-dir (expand-file-name
                       "content" (file-name-directory file)))
         (endpoint (org-canvas-api-course-endpoint "files"))
         (remote (org-canvas-api-request-all-pages 'GET endpoint))
         (count 0))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (unless (file-directory-p content-dir)
      (make-directory content-dir t))
    (with-current-buffer (find-file-noselect file)
      (dolist (item remote)
        (let* ((id (alist-get 'id item))
               (display-name (alist-get 'display_name item))
               (download-url (alist-get 'url item))
               (content-type (alist-get 'content-type item))
               (size (alist-get 'size item))
               (local-path (expand-file-name display-name content-dir))
               (heading-text (format "[[file:content/%s][%s]]"
                                     display-name display-name))
               (pos (org-canvas--pull-upsert-heading file id heading-text)))
          (goto-char pos)
          (org-canvas-org-save-sync-state pos id)
          (when content-type
            (org-canvas-org-set-property pos "CONTENT_TYPE" content-type))
          (when size
            (org-canvas-org-set-property pos "SIZE" (format "%s" size)))
          ;; Download file if it doesn't exist locally
          (when (and download-url (not (file-exists-p local-path)))
            (condition-case err
                (progn
                  (elog-info org-canvas--logger
                    "[Download] %s (%s bytes)" display-name (or size "?"))
                  (url-copy-file download-url local-path t))
              (error
               (elog-warning org-canvas--logger
                 "[Download] Failed for %s: %s"
                 display-name (error-message-string err)))))
          (cl-incf count)))
      (save-buffer))
    (elog-info org-canvas--logger "Files pull complete: %d files" count)
    (message "Files pull complete: %d files." count)))

(provide 'org-canvas-files)
;;; org-canvas-files.el ends here
