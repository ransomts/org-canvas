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

(defalias 'org-canvas--file-guess-content-type #'org-canvas--guess-content-type
  "Alias — canonical definition is in core-api.el.")

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
       (elog-debug org-canvas--logger "[Files] Folder creation failed (may exist): %s" (error-message-string err))
       (org-canvas--file-resolve-folder-by-path folder-path)))))

(defun org-canvas--file-resolve-or-cache-folder (current-path part current-folder)
  "Resolve folder for PART under CURRENT-FOLDER, using cache at CURRENT-PATH.
Returns the resolved folder object."
  (let ((cached (and org-canvas--file-folder-cache
                     (gethash current-path org-canvas--file-folder-cache))))
    (if cached
        (progn
          (elog-debug org-canvas--logger "[Files] Using cached folder for: %s" current-path)
          cached)
      (let ((folder (org-canvas--file-ensure-subfolder current-folder part)))
        (when org-canvas--file-folder-cache
          (puthash current-path folder org-canvas--file-folder-cache)
          (elog-debug org-canvas--logger "[Files] Cached folder: %s -> ID %s"
            current-path (alist-get 'id folder)))
        folder))))

(defun org-canvas--file-resolve-folder-by-path (folder-path)
  "Resolve a folder by its path (FOLDER-PATH), creating parent folders if needed.
Uses session cache to avoid redundant API calls."
  (let ((cached (and org-canvas--file-folder-cache
                     (gethash folder-path org-canvas--file-folder-cache))))
    (if cached
        (progn
          (elog-debug org-canvas--logger "[Files] Using cached folder for path: %s" folder-path)
          cached)
      (let* ((parts (split-string folder-path "/" t))
             (current-folder (org-canvas--file-get-root-folder))
             (current-path ""))
        (dolist (part parts)
          (setq current-path (if (string-empty-p current-path)
                                 part
                               (concat current-path "/" part)))
          (setq current-folder
                (org-canvas--file-resolve-or-cache-folder
                 current-path part current-folder)))
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

(defun org-canvas--file-validate-local (abs-path display-name)
  "Validate that ABS-PATH exists and is within the size limit.
DISPLAY-NAME is used for error messages.  Signals an error on failure."
  (unless (file-exists-p abs-path)
    (elog-error org-canvas--logger "[Stage 1: Parse] File not found: %s" abs-path)
    (error "File not found: %s" abs-path))
  (let ((size-mb (/ (file-attribute-size (file-attributes abs-path)) org-canvas--bytes-per-mb)))
    (when (> size-mb org-canvas-max-file-size-mb)
      (elog-warning org-canvas--logger
        "[Stage 1: Parse] Skipping '%s': %.1f MB exceeds limit of %d MB"
        display-name size-mb org-canvas-max-file-size-mb)
      (error "File '%s' (%.1f MB) exceeds max size of %d MB"
             display-name size-mb org-canvas-max-file-size-mb))))

(defun org-canvas--file-extract-heading-link ()
  "Extract link path and display name from the heading at point.
Uses `org-complex-heading-regexp' group 4 to preserve link syntax
on Emacs 30 / Org 9.7+.
Returns a plist (:link-path PATH :display-name NAME :raw-heading TEXT)."
  (let* ((heading-with-links
          (save-excursion
            (beginning-of-line)
            (when (looking-at org-complex-heading-regexp)
              (match-string-no-properties 4))))
         (raw-heading heading-with-links))
    (list :link-path (org-canvas--file-extract-link-path raw-heading)
          :display-name (org-canvas--file-get-display-name raw-heading)
          :raw-heading raw-heading)))

(defun org-canvas--file-read-props (pom)
  "Read raw property strings from the file heading at POM.
Returns a plist with raw values, or nil for folder-only headings."
  (let* ((heading-info (org-canvas--file-extract-heading-link))
         (link-path (plist-get heading-info :link-path))
         (display-name (plist-get heading-info :display-name))
         (raw-heading (plist-get heading-info :raw-heading)))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Heading text: %s" raw-heading)
    (elog-debug org-canvas--logger "[Stage 1: Parse] Link path: %s" (or link-path "NONE"))
    (if (not link-path)
        (progn
          (elog-debug org-canvas--logger "[Stage 1: Parse] Skipping folder heading: %s" raw-heading)
          nil)
      (let* ((files-dir (file-name-directory org-canvas-files-file))
             (folder-path (org-canvas--file-get-folder-path pom files-dir))
             (abs-path (expand-file-name link-path files-dir)))
        (list :display-name display-name
              :local-path abs-path
              :folder-path folder-path
              :canvas-id (org-entry-get pom "CANVAS_ID")
              :published-raw (org-entry-get pom "PUBLISHED")
              :unlock-at-raw (org-entry-get pom "UNLOCK_AT")
              :lock-at-raw (org-entry-get pom "LOCK_AT")
              :use-justification (org-entry-get pom "USE_JUSTIFICATION")
              :usage-license (org-entry-get pom "USAGE_LICENSE")
              :copyright (org-entry-get pom "COPYRIGHT"))))))

(defun org-canvas--file-transform-props (raw)
  "Transform raw property strings RAW into typed file data.
Pure function — no buffer access."
  (list :display-name (plist-get raw :display-name)
        :local-path (plist-get raw :local-path)
        :folder-path (plist-get raw :folder-path)
        :canvas-id (plist-get raw :canvas-id)
        :published (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
        :unlock-at (org-canvas-org-parse-timestamp (plist-get raw :unlock-at-raw))
        :lock-at (org-canvas-org-parse-timestamp (plist-get raw :lock-at-raw))
        :use-justification (plist-get raw :use-justification)
        :usage-license (plist-get raw :usage-license)
        :copyright (plist-get raw :copyright)))

(defun org-canvas--file-parse-entry ()
  "Extract file data from the Org heading at point.
Returns nil for folder-only headings (no file link)."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--file-read-props pom)))

    ;; Only process entries with file links
    (when raw
      (let ((data (org-canvas--file-transform-props raw)))

        (elog-info org-canvas--logger "[Stage 1: Parse] Processing File: '%s' (ID: %s)"
          (plist-get data :display-name) (or (plist-get data :canvas-id) "NEW"))
        (elog-debug org-canvas--logger "[Stage 1: Parse] Local path: %s" (plist-get data :local-path))
        (let ((folder-display (if (string-empty-p (plist-get data :folder-path))
                                  "root"
                                (plist-get data :folder-path))))
          (elog-debug org-canvas--logger "[Stage 1: Parse] Canvas folder: %s" folder-display))

        (org-canvas--file-validate-local (plist-get data :local-path) (plist-get data :display-name))

        (plist-put data :pom pom)
        data))))

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

(defalias 'org-canvas--file-build-multipart-body #'org-canvas--upload-build-multipart
  "Alias — canonical definition is in core-api.el.")

(defun org-canvas--file-extract-response-parts ()
  "Parse HTTP status, Location header, and JSON body from current buffer.
Must be called at `point-min' of an HTTP response buffer.
Returns a plist (:status STRING :location URL :json DATA :body STRING)."
  (let (status-line location-header json-response response-body)
    (when (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
      (setq status-line (match-string 1)))
    (save-excursion
      (when (re-search-forward "^[Ll]ocation: \\(.*\\)\r?$" nil t)
        (setq location-header (string-trim (match-string 1)))))
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (setq response-body (buffer-substring-no-properties (point) (point-max)))
      (setq json-response (condition-case nil
                              (json-read-from-string response-body)
                            (error nil))))
    (list :status status-line :location location-header
          :json json-response :body response-body)))

(defun org-canvas--file-parse-upload-response (local-path upload-url)
  "Parse the HTTP response in the current buffer after file upload.
LOCAL-PATH is the path to the uploaded file (for logging).
UPLOAD-URL is the URL the file was uploaded to (for logging).
Must be called in the `url-retrieve' response buffer.
Returns either a file object alist, a location alist, or signals an error."
  (goto-char (point-min))
  (let* ((parts (org-canvas--file-extract-response-parts))
         (status-line (plist-get parts :status))
         (location-header (plist-get parts :location))
         (json-response (plist-get parts :json))
         (response-body (plist-get parts :body)))
    (when status-line
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] HTTP status: %s" status-line))
    ;; Buffer cleanup is handled by caller's unwind-protect
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
     ((and json-response (alist-get 'id json-response))
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Got file object directly")
      json-response)
     (location-header
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Got redirect to: %s" location-header)
      `((location . ,location-header)))
     (json-response
      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Got JSON response")
      json-response)
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

    (unless upload-url
      (error "Canvas API returned no upload_url in step 1 response: %S" upload-info))

    ;; Log upload_params from Canvas (these must be sent exactly as-is)
    (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] upload_url: %s" upload-url)
    (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] upload_params from Canvas: %S" upload-params)

    ;; Build multipart form data
    (let* ((full-body (org-canvas--file-build-multipart-body upload-params local-path boundary))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
           (url-request-data full-body)
           (buf (url-retrieve-synchronously
                 upload-url nil nil org-canvas-upload-timeout)))

      (elog-debug org-canvas--logger "[Stage 3: Upload Step 2] Sending to %s" upload-url)

      (unwind-protect
          (with-current-buffer buf
            (org-canvas--file-parse-upload-response local-path upload-url))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(defun org-canvas--file-confirm-with-retry (url max-retries)
  "GET URL with exponential-backoff retries up to MAX-RETRIES.
Returns the API response or signals the last error."
  (let ((attempt 0) response last-err)
    (while (and (< attempt max-retries) (null response))
      (setq attempt (1+ attempt))
      (condition-case err
          (progn
            (when (> attempt 1)
              (let ((delay (expt 2 (1- attempt))))
                (elog-warning org-canvas--logger
                  "[Stage 3: Upload Step 3] Retry %d/%d after %ds..."
                  attempt max-retries delay)
                (message "File upload: retry %d/%d after %ds..." attempt max-retries delay)
                (sleep-for delay)))
            (setq response (org-canvas-api-request 'GET url)))
        (error
         (setq last-err err)
         (elog-warning org-canvas--logger
           "[Stage 3: Upload Step 3] Attempt %d/%d failed: %s"
           attempt max-retries (error-message-string err)))))
    (if response
        response
      (elog-error org-canvas--logger
        "[Stage 3: Upload Step 3] All %d confirmation attempts failed. File may exist on Canvas without local tracking."
        max-retries)
      (signal (car last-err) (cdr last-err)))))

(defun org-canvas--file-upload-step3-confirm (step2-response)
  "Step 3: Confirm the upload and get the file object.
STEP2-RESPONSE is the response from step 2.
Per Canvas docs, this GET request must be authenticated."
  (elog-info org-canvas--logger "[Stage 3: Upload Step 3] Confirming upload...")
  (if (alist-get 'id step2-response)
      (progn
        (elog-debug org-canvas--logger "[Stage 3: Upload Step 3] File object returned directly")
        step2-response)
    (let ((location (alist-get 'location step2-response)))
      (if location
          (let* ((full-url (if (string-prefix-p "http" location)
                               location
                             (concat org-canvas-base-url location)))
                 (response (org-canvas--file-confirm-with-retry
                            full-url org-canvas--file-upload-confirm-retries)))
            (elog-debug org-canvas--logger
              "[Stage 3: Upload Step 3] Confirmed, file ID: %s"
              (alist-get 'id response))
            (unless (alist-get 'id response)
              (elog-warning org-canvas--logger
                "[Stage 3: Upload Step 3] No ID in confirmation response: %S"
                response))
            response)
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
         (elog-warning org-canvas--logger "[Stage 3: Execute] Could not delete old file: %s" (error-message-string err)))))

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

(defun org-canvas--file-search-by-name (display-name folder-path)
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
     (elog-warning org-canvas--logger "[Stage 3: Search] Search failed: %s" (error-message-string err))
     nil)))

;;;; 4. Stage: Finalization

(defun org-canvas--file-finalize (data response)
  "Update local Org file with CANVAS_ID using DATA and RESPONSE."
  (org-canvas--finalize-item data response :title-key :display-name))

(defun org-canvas--file-set-usage-rights (file-id data)
  "Set usage rights on FILE-ID using DATA plist properties.
Calls PUT /api/v1/courses/:id/usage_rights with file_ids[] and
usage_rights[...] parameters.  Only called when at least
USE_JUSTIFICATION is set."
  (when-let* ((justification (plist-get data :use-justification)))
    (let* ((endpoint (org-canvas-api-course-endpoint "usage_rights"))
           (payload (make-hash-table :test 'equal))
           (rights (make-hash-table :test 'equal)))
      (puthash "file_ids" (vector file-id) payload)
      (puthash "use_justification" justification rights)
      (org-canvas--puthash-when rights data :usage-license "license")
      (org-canvas--puthash-when rights data :copyright "legal_copyright")
      (puthash "usage_rights" rights payload)
      (elog-info org-canvas--logger
        "[Usage Rights] Setting usage rights for file %s: %s" file-id justification)
      (condition-case err
          (org-canvas-api-request 'PUT endpoint :data payload)
        (error
         (elog-warning org-canvas--logger
           "[Usage Rights] Failed for file %s: %s"
           file-id (error-message-string err)))))))

;;;; Pre-flight Folder Creation

(defun org-canvas--file-collect-folder-paths (files-file)
  "Collect all unique folder paths from FILES-FILE.
Returns a list of folder paths that need to exist for file uploads."
  (let ((folder-paths nil)
        (seen (make-hash-table :test 'equal)))
    (with-current-buffer (find-file-noselect files-file)
      (org-map-entries
       (lambda ()
         (let* ((heading-with-links
                 (save-excursion
                   (beginning-of-line)
                   (when (looking-at org-complex-heading-regexp)
                     (match-string-no-properties 4))))
                (raw-heading heading-with-links)
                (link-path (org-canvas--file-extract-link-path raw-heading)))
           ;; Only process actual files (headings with links)
           (when link-path
             (let ((folder-path (org-canvas--file-get-folder-path
                                 (point)
                                 (file-name-directory files-file))))
               (unless (or (string-empty-p folder-path)
                           (gethash folder-path seen))
                 (puthash folder-path t seen)
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

(defun org-canvas--file-sync-single-entry (marker)
  "Process a single file entry at MARKER.
Returns :success, :skip (folder heading), or :fail."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (condition-case err
          (let ((data (org-canvas--file-parse-entry)))
            (if (not data)
                :skip
              (elog-info org-canvas--logger "----------------------------------------")
              (let ((response (org-canvas--file-push-to-api data)))
                (org-canvas--file-finalize data response)
                (let ((fid (alist-get 'id response)))
                  (when (and fid (plist-get data :use-justification))
                    (org-canvas--file-set-usage-rights fid data))))
              :success))
        (error
         (elog-error org-canvas--logger "[FAILED] At point %d: %s"
           (marker-position marker) (error-message-string err))
         :fail)))))

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

    (display-buffer (get-buffer-create org-canvas--log-buffer-name))
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
       (elog-warning org-canvas--logger "[Pre-flight] Warning: %s" (error-message-string err))))

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
        (let ((result (org-canvas--file-sync-single-entry marker)))
          (pcase result
            (:success (setq success-count (1+ success-count))
                      (message "Files [%d/%d] Synced"
                        (+ success-count skip-count fail-count)
                        (length targets)))
            (:skip (setq skip-count (1+ skip-count)))
            (:fail (setq fail-count (1+ fail-count))
                   (message "Files [%d/%d] FAILED"
                     (+ success-count skip-count fail-count)
                     (length targets))))))

      (dolist (m targets) (set-marker m nil))

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
           (elog-error org-canvas--logger "  -> Delete failed: %s" (error-message-string err))))))
    deleted-count))

;;;###autoload
(defun org-canvas-delete-all-files ()
  "Delete ALL files and folders in the course (Danger Zone)."
  (interactive)
  (unless org-canvas--inhibit-log-clear
    (unless (y-or-n-p "Delete ALL files and folders in this course? ")
      (user-error "Aborted")))
  (let ((files-file (expand-file-name org-canvas-files-file)))
    (org-canvas-clear-log)
    ;; Clear session caches
    (setq org-canvas--file-root-folder-cache nil)
    (setq org-canvas--file-folder-cache (make-hash-table :test 'equal))
    (display-buffer (get-buffer-create org-canvas--log-buffer-name))
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
             (elog-error org-canvas--logger "  -> Delete failed: %s" (error-message-string err))))))

      ;; Delete all folders (after files are gone)
      (elog-info org-canvas--logger "----------------------------------------")
      (elog-info org-canvas--logger "Now deleting folders...")
      (setq deleted-folder-count (org-canvas--file-delete-all-folders))

      ;; Clean local properties
      (org-canvas--clean-local-sync-properties files-file)

      (elog-info org-canvas--logger "========================================")
      (elog-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d files, %d folders removed"
        deleted-file-count deleted-folder-count)
      (elog-info org-canvas--logger "========================================")
      (message "Deletion complete. %d files, %d folders removed."
               deleted-file-count deleted-folder-count))))

(org-canvas-define-delete-at-point file
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/files/%s"
                           org-canvas-base-url id)))

;;;; Pull

(defun org-canvas--file-pull-download (display-name download-url local-path size)
  "Download file DISPLAY-NAME from DOWNLOAD-URL to LOCAL-PATH if not present.
SIZE is used for logging; may be nil."
  (when (and download-url (not (file-exists-p local-path)))
    (condition-case err
        (progn
          (elog-info org-canvas--logger
            "[Download] %s (%s bytes)" display-name (or size "?"))
          (url-copy-file download-url local-path t))
      (error
       (elog-warning org-canvas--logger
         "[Download] Failed for %s: %s"
         display-name (error-message-string err))))))

(defun org-canvas--file-pull-set-properties (pos item)
  "Set content-type, size, and usage-rights properties at POS from ITEM."
  (let ((content-type (alist-get 'content-type item))
        (size (alist-get 'size item)))
    (when content-type
      (org-canvas-org-set-property pos "CONTENT_TYPE" content-type))
    (when size
      (org-canvas-org-set-property pos "SIZE" (format "%s" size))))
  (let ((usage-rights (alist-get 'usage_rights item)))
    (when usage-rights
      (let ((justification (alist-get 'use_justification usage-rights))
            (license (alist-get 'license usage-rights))
            (legal-copyright (alist-get 'legal_copyright usage-rights)))
        (when justification
          (org-canvas-org-set-property pos "USE_JUSTIFICATION" justification))
        (when license
          (org-canvas-org-set-property pos "USAGE_LICENSE" license))
        (when legal-copyright
          (org-canvas-org-set-property pos "COPYRIGHT" legal-copyright))))))

;;;###autoload
(defun org-canvas-pull-files ()
  "Pull file metadata from Canvas into files.org.
Downloads file contents to the content/ directory."
  (interactive)
  (org-canvas--start-operation "PULLING FILES")
  (let* ((file (expand-file-name org-canvas-files-file))
         (content-dir (expand-file-name
                       "content" (file-name-directory file)))
         (endpoint (org-canvas-api-course-endpoint "files"))
         (remote (org-canvas-api-request-all-pages 'GET endpoint))
         (total (length remote))
         (count 0))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (unless (file-directory-p content-dir)
      (make-directory content-dir t))
    (with-current-buffer (find-file-noselect file)
      (dolist (item remote)
        (cl-incf count)
        (let* ((id (alist-get 'id item))
               (display-name (alist-get 'display_name item))
               (download-url (alist-get 'url item))
               (local-path (expand-file-name display-name content-dir))
               (heading-text (format "[[file:content/%s][%s]]"
                                     display-name display-name))
               (pos (org-canvas--pull-upsert-heading file id heading-text)))
          (message "Files [%d/%d] Pulling '%s'..." count total display-name)
          (goto-char pos)
          (org-canvas-org-save-sync-state pos id)
          (org-canvas--file-pull-set-properties pos item)
          (org-canvas--file-pull-download
           display-name download-url local-path (alist-get 'size item))))
      (save-buffer))
    (elog-info org-canvas--logger "Files pull complete: %d files" count)
    (message "Files pull complete: %d files." count)))

(provide 'org-canvas-files)
;;; org-canvas-files.el ends here
