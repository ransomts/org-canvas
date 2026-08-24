;;; org-canvas-files.el --- Pipeline-based File Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'cl-lib)
(require 'url-util)
(require 'json)

;;;; Configuration

(defcustom org-canvas-files-file (org-canvas--path "files.org")
  "Path to the files.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-files-file "files.org")
(org-canvas-register-feature
 :name "Files" :endpoint "files"
 :file-var 'org-canvas-files-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'display_name)

(defun org-canvas--file-remote-published (item)
  "Return non-nil when the Canvas file ITEM is published to students.
A Canvas file object has no `published' field — it carries `locked',
`hidden', `hidden_for_user', `lock_at', `unlock_at' and
`visibility_level' — so reading `published' returned nil for every
file and the drift report called them all unpublished (issue #61).
PUBLISHED is the inverse of `locked', the same mapping the pull path
uses (issue #50)."
  (not (eq (alist-get 'locked item) t)))

(org-canvas-register-properties "files"
  :label "Files"
  :file-var 'org-canvas-files-file
  :query "LEVEL>0"
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :remote-fn org-canvas--file-remote-published
     :doc "Whether the file is published to students (false sets locked)")
    (:org-prop "HIDDEN" :data-key :hidden :type boolean
     :doc "Published but unlisted: reachable only by direct link")
    (:org-prop "UNLOCK_AT" :data-key :unlock_at :type timestamp
     :doc "Date to make file available")
    (:org-prop "LOCK_AT" :data-key :lock_at :type timestamp
     :doc "Date to hide file")
    (:org-prop "USE_JUSTIFICATION" :data-key :use_justification :type enum
     :values ,org-canvas--valid-use-justifications
     :doc "How to resolve a name collision on upload (rename vs overwrite)"))
  :structural-fn #'org-canvas--validate-file-structure)

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

(defun org-canvas--file-sanitize-headline-desc (display-name)
  "Escape Org link-breaking chars in DISPLAY-NAME for safe link description use.
Only escapes `[' and `]' — the only chars that break Org link
description parsing.  Other special chars (parens, underscores,
slashes, spaces) are valid inside link descriptions."
  (replace-regexp-in-string
   "\\[\\|\\]"
   (lambda (m) (concat "\\\\" m))
   display-name))

(defun org-canvas--file-safe-local-path (rel content-dir)
  "Resolve REL under CONTENT-DIR, guarding against path traversal.
Canvas-supplied names (`display_name', folder paths) are untrusted; a name
like \"../../etc/x\" would otherwise let `expand-file-name' escape
CONTENT-DIR.  If REL would resolve outside CONTENT-DIR, fall back to its bare
file name inside CONTENT-DIR and warn."
  (let* ((base (file-name-as-directory (expand-file-name content-dir)))
         (path (expand-file-name rel content-dir))
         (dir (file-name-as-directory (or (file-name-directory path) base))))
    (if (string-prefix-p base dir)
        path
      (let ((safe (expand-file-name (file-name-nondirectory rel) content-dir)))
        (when (boundp 'org-canvas--logger)
          (org-canvas--log-warning org-canvas--logger
            "[Files] Suspicious path '%s' from Canvas; writing to '%s' instead"
            rel safe))
        safe))))

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
        (org-canvas--log-debug org-canvas--logger "[Files] Using cached root folder ID: %s"
          (alist-get 'id org-canvas--file-root-folder-cache))
        org-canvas--file-root-folder-cache)
    (org-canvas--log-debug org-canvas--logger "[Files] Getting root folder...")
    (let* ((endpoint (org-canvas-api-course-endpoint "folders/root"))
           (response (org-canvas-api-request 'GET endpoint)))
      (org-canvas--log-debug org-canvas--logger "[Files] Root folder ID: %s" (alist-get 'id response))
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
  (org-canvas--log-info org-canvas--logger "[Files] Creating folder: %s" folder-path)
  (let* ((endpoint (org-canvas-api-course-endpoint "folders"))
         (payload (make-hash-table :test 'equal)))
    (puthash "name" (file-name-nondirectory folder-path) payload)
    (puthash "parent_folder_path" (file-name-directory folder-path) payload)
    (puthash "parent_folder_id" parent-folder-id payload)
    (condition-case err
        (let ((response (org-canvas-api-request 'POST endpoint :data payload)))
          (org-canvas--log-info org-canvas--logger "[Files] Created folder ID: %s" (alist-get 'id response))
          response)
      (error
       ;; If it already exists, try to get it
       (org-canvas--log-debug org-canvas--logger "[Files] Folder creation failed (may exist): %s" (error-message-string err))
       (org-canvas--file-resolve-folder-by-path folder-path)))))

(defun org-canvas--file-resolve-or-cache-folder (current-path part current-folder)
  "Resolve folder for PART under CURRENT-FOLDER, using cache at CURRENT-PATH.
Returns the resolved folder object."
  (let ((cached (and org-canvas--file-folder-cache
                     (gethash current-path org-canvas--file-folder-cache))))
    (if cached
        (progn
          (org-canvas--log-debug org-canvas--logger "[Files] Using cached folder for: %s" current-path)
          cached)
      (let ((folder (org-canvas--file-ensure-subfolder current-folder part)))
        (when org-canvas--file-folder-cache
          (puthash current-path folder org-canvas--file-folder-cache)
          (org-canvas--log-debug org-canvas--logger "[Files] Cached folder: %s -> ID %s"
            current-path (alist-get 'id folder)))
        folder))))

(defun org-canvas--file-resolve-folder-by-path (folder-path)
  "Resolve a folder by its path (FOLDER-PATH), creating parent folders if needed.
Uses session cache to avoid redundant API calls."
  (let ((cached (and org-canvas--file-folder-cache
                     (gethash folder-path org-canvas--file-folder-cache))))
    (if cached
        (progn
          (org-canvas--log-debug org-canvas--logger "[Files] Using cached folder for path: %s" folder-path)
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
    (org-canvas--log-error org-canvas--logger "[Stage 1: Parse] File not found: %s" abs-path)
    (org-canvas--signal 'org-canvas-config-error "File not found: %s" abs-path))
  (let ((size-mb (/ (file-attribute-size (file-attributes abs-path)) org-canvas--bytes-per-mb)))
    (when (> size-mb org-canvas-max-file-size-mb)
      (org-canvas--log-warning org-canvas--logger
        "[Stage 1: Parse] Skipping '%s': %.1f MB exceeds limit of %d MB"
        display-name size-mb org-canvas-max-file-size-mb)
      (org-canvas--signal 'org-canvas-validation-error
        "File '%s' (%.1f MB) exceeds max size of %d MB"
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
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Heading text: %s" raw-heading)
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Link path: %s" (or link-path "NONE"))
    (if (not link-path)
        (progn
          (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Skipping folder heading: %s" raw-heading)
          nil)
      (let* ((files-dir (file-name-directory org-canvas-files-file))
             (folder-path (org-canvas--file-get-folder-path pom files-dir))
             (abs-path (expand-file-name link-path files-dir)))
        (list :display-name display-name
              :local-path abs-path
              :folder-path folder-path
              :canvas-id (org-entry-get pom "CANVAS_ID")
              :published-raw (org-entry-get pom "PUBLISHED")
              :hidden-raw (org-entry-get pom "HIDDEN")
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
        :hidden (org-canvas--interpret-boolean (plist-get raw :hidden-raw))
        :unlock-at (org-canvas-org-parse-timestamp (plist-get raw :unlock-at-raw))
        :lock-at (org-canvas-org-parse-timestamp (plist-get raw :lock-at-raw))
        :use-justification (plist-get raw :use-justification)
        :usage-license (plist-get raw :usage-license)
        :copyright (plist-get raw :copyright)))

(defun org-canvas--file-parse-entry ()
  "Extract file data from the Org heading at point.
Returns nil for folder-only headings (no file link)."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--file-read-props pom)))

    ;; Only process entries with file links
    (when raw
      (let ((data (org-canvas--file-transform-props raw)))

        (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Processing File: '%s' (ID: %s)"
          (plist-get data :display-name) (or (plist-get data :canvas-id) "NEW"))
        (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Local path: %s" (plist-get data :local-path))
        (let ((folder-display (if (string-empty-p (plist-get data :folder-path))
                                  "root"
                                (plist-get data :folder-path))))
          (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Canvas folder: %s" folder-display))

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

    (org-canvas--log-info org-canvas--logger "[Stage 2: Prepare] Building upload for '%s'" display-name)
    (org-canvas--log-debug org-canvas--logger "[Stage 2: Prepare] Size: %d bytes, Type: %s" file-size content-type)

    (puthash "name" display-name payload)
    (puthash "size" file-size payload)
    (puthash "content_type" content-type payload)
    (puthash "on_duplicate" "overwrite" payload)

    ;; No visibility fields here.  The upload preflight
    ;; (Api::V1::Attachment#api_attachment_preflight) reads only name, size,
    ;; content_type, parent_folder_id and on_duplicate; locked, hidden,
    ;; unlock_at and lock_at are silently discarded.  They are applied after
    ;; the upload lands, by `org-canvas--file-apply-settings' (issue #50).
    payload))

(defun org-canvas--file-build-settings-payload (data)
  "Build the file-settings payload carrying DATA's visibility.
Canvas models a file's visibility with two independent flags.  `locked'
is the Publish control; `hidden' is the weaker \"only available to
students with the link\" state, which leaves the file served to anyone
holding its URL.  PUBLISHED means the former, so it maps to `locked'
and is sent in both directions — flipping the property back to true
republishes the file instead of merely omitting the field, which on a
partial update would leave the stored value alone (issue #50).  HIDDEN
is the separate opt-in for the unlisted state.

UNLOCK_AT and LOCK_AT are sent only when set, as in every other module:
removing the property leaves the date stored on Canvas alone."
  (let ((payload (make-hash-table :test 'equal)))
    (puthash "locked" (org-canvas--to-json-boolean
                       (not (plist-get data :published)))
             payload)
    (puthash "hidden" (org-canvas--to-json-boolean (plist-get data :hidden))
             payload)
    (when (plist-get data :unlock-at)
      (puthash "unlock_at" (plist-get data :unlock-at) payload))
    (when (plist-get data :lock-at)
      (puthash "lock_at" (plist-get data :lock-at) payload))
    payload))

(defun org-canvas--file-apply-settings (file-id data)
  "Apply DATA's visibility settings to FILE-ID on Canvas.
The upload flow cannot carry them: the preflight endpoint reads only
name, size, content_type, parent_folder_id and on_duplicate, and
discards locked/hidden/unlock_at/lock_at without complaint.  They are
applied afterwards through `PUT /api/v1/files/:id', the documented
update_file operation.

Returns the updated Canvas file object, or `org-canvas--dry-run-response'
when previewing."
  (if org-canvas--dry-run
      (progn
        (org-canvas--log-info org-canvas--logger
          "[DRY-RUN] Would set visibility on '%s'" (plist-get data :display-name))
        org-canvas--dry-run-response)
    (org-canvas--log-info org-canvas--logger
      "[Stage 3: Settings] Applying visibility to file %s (published: %s, hidden: %s)"
      file-id
      (if (plist-get data :published) "yes" "no")
      (if (plist-get data :hidden) "yes" "no"))
    (org-canvas-api-request
     'PUT (format "%s/api/v1/files/%s" org-canvas-base-url file-id)
     :data (org-canvas--file-build-settings-payload data))))

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
  (org-canvas--log-info org-canvas--logger "[Stage 3: Upload Step 1] Notifying Canvas...")
  (let* ((endpoint (format "%s/api/v1/folders/%s/files" org-canvas-base-url folder-id))
         (response (org-canvas-api-request 'POST endpoint :data payload)))
    (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 1] Got upload URL: %s"
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
      (setq json-response (condition-case err
                              (json-read-from-string response-body)
                            (error
                             (org-canvas--log-debug org-canvas--logger
                               "[Files] Upload response body was not valid JSON (%s)"
                               (error-message-string err))
                             nil))))
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
      (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 2] HTTP status: %s" status-line))
    ;; Buffer cleanup is handled by caller's unwind-protect
    ;; Log HTTP errors with diagnostic details
    (when (and status-line (>= (string-to-number status-line) 400))
      (org-canvas--log-error org-canvas--logger
        "[Stage 3: Upload Step 2] HTTP %s uploading '%s' to %s"
        status-line (file-name-nondirectory local-path) upload-url)
      (when response-body
        (org-canvas--log-error org-canvas--logger
          "[Stage 3: Upload Step 2] Response: %s"
          (truncate-string-to-width response-body 500))))
    ;; Determine what to return
    (cond
     ((and json-response (alist-get 'id json-response))
      (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 2] Got file object directly")
      json-response)
     (location-header
      (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 2] Got redirect to: %s" location-header)
      `((location . ,location-header)))
     (json-response
      (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 2] Got JSON response")
      json-response)
     (t
      (org-canvas--signal 'org-canvas-api-error
        "Upload failed: no JSON response or Location header.  Status: %s, Body: %s"
        status-line (or response-body "empty"))))))

(defun org-canvas--file-upload-step2-send (upload-info local-path)
  "Step 2: Upload the actual file content.
UPLOAD-INFO contains the upload URL and parameters from step 1.
LOCAL-PATH is the path to the local file.
Return either a file object (if upload returns JSON) or an alist
with `location' key."
  (org-canvas--log-info org-canvas--logger "[Stage 3: Upload Step 2] Uploading file content...")
  (let* ((upload-url (alist-get 'upload_url upload-info))
         (upload-params (alist-get 'upload_params upload-info))
         (boundary (format "----FormBoundary%s" (md5 (format "%s%s" (current-time) (random))))))

    (unless upload-url
      (org-canvas--signal 'org-canvas-api-error
        "Canvas API returned no upload_url in step 1 response: %S" upload-info))

    ;; Log upload_params from Canvas (these must be sent exactly as-is)
    (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 2] upload_url: %s" upload-url)
    (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 2] upload_params from Canvas: %S" upload-params)

    ;; Build multipart form data
    (let* ((full-body (org-canvas--file-build-multipart-body upload-params local-path boundary))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
           (url-request-data full-body)
           (buf (url-retrieve-synchronously
                 upload-url nil nil org-canvas-upload-timeout)))

      (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 2] Sending to %s" upload-url)

      ;; `url-retrieve-synchronously' returns nil when it hits its timeout.
      ;; Without this check the nil falls through to `with-current-buffer'
      ;; and surfaces as "Wrong type argument: stringp, nil", which says
      ;; nothing about the timeout that actually caused it.  Canvas may well
      ;; have finished storing the file, so the caller retries by name.
      (unless buf
        (org-canvas--signal 'org-canvas-api-error
          "Upload of '%s' timed out after %ss (see `org-canvas-upload-timeout'); Canvas may have stored the file anyway"
          (file-name-nondirectory local-path) org-canvas-upload-timeout))

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
                (org-canvas--log-warning org-canvas--logger
                  "[Stage 3: Upload Step 3] Retry %d/%d after %ds..."
                  attempt max-retries delay)
                (message "File upload: retry %d/%d after %ds..." attempt max-retries delay)
                (sleep-for delay)))
            (setq response (org-canvas-api-request 'GET url)))
        (error
         (setq last-err err)
         (org-canvas--log-warning org-canvas--logger
           "[Stage 3: Upload Step 3] Attempt %d/%d failed: %s"
           attempt max-retries (error-message-string err)))))
    (if response
        response
      (org-canvas--log-error org-canvas--logger
        "[Stage 3: Upload Step 3] All %d confirmation attempts failed. File may exist on Canvas without local tracking."
        max-retries)
      (signal (car last-err) (cdr last-err)))))

(defun org-canvas--file-upload-step3-confirm (step2-response)
  "Step 3: Confirm the upload and get the file object.
STEP2-RESPONSE is the response from step 2.
Per Canvas docs, this GET request must be authenticated."
  (org-canvas--log-info org-canvas--logger "[Stage 3: Upload Step 3] Confirming upload...")
  (if (alist-get 'id step2-response)
      (progn
        (org-canvas--log-debug org-canvas--logger "[Stage 3: Upload Step 3] File object returned directly")
        step2-response)
    (let ((location (alist-get 'location step2-response)))
      (if location
          (let* ((full-url (if (string-prefix-p "http" location)
                               location
                             (concat org-canvas-base-url location)))
                 (response (org-canvas--file-confirm-with-retry
                            full-url org-canvas--file-upload-confirm-retries)))
            (org-canvas--log-debug org-canvas--logger
              "[Stage 3: Upload Step 3] Confirmed, file ID: %s"
              (alist-get 'id response))
            (unless (alist-get 'id response)
              (org-canvas--log-warning org-canvas--logger
                "[Stage 3: Upload Step 3] No ID in confirmation response: %S"
                response))
            response)
        (org-canvas--signal 'org-canvas-api-error
          "No file ID or location in upload response")))))

(defun org-canvas--file-target-folder-id (folder-path)
  "Return the Canvas folder id for FOLDER-PATH, creating folders as needed.
An empty FOLDER-PATH means the course root folder."
  (alist-get 'id (if (string-empty-p folder-path)
                     (org-canvas--file-get-root-folder)
                   (org-canvas--file-resolve-folder-by-path folder-path))))

(defvar org-canvas--file-recreated-ids nil
  "Display names of files deleted and re-uploaded during the current sync.
The subset of `org-canvas--file-changed-ids' that could not be
overwritten in place because the file moved folders, and so did lose
its module items.  Reset by `org-canvas-sync-files'; consumed by
`org-canvas--file-warn-recreated-ids'.")

(defun org-canvas--file-remote-folder-id (canvas-id)
  "Return the id of the Canvas folder CANVAS-ID currently lives in, or nil.
Nil also on a failed lookup, which sends the caller down the older
delete-and-upload path rather than guessing."
  (condition-case err
      (alist-get 'folder_id
                 (org-canvas-api-request
                  'GET (format "%s/api/v1/files/%s"
                               org-canvas-base-url canvas-id)))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Stage 3: Execute] Could not read the folder of file %s (%s)"
       canvas-id (error-message-string err))
     nil)))

(defun org-canvas--file-replace-in-place-p (canvas-id folder-id)
  "Return non-nil when an upload can overwrite CANVAS-ID where it stands.
True when Canvas already holds the file in FOLDER-ID, since
`on_duplicate=overwrite' matches on name within a folder.  A file whose
FOLDER-PATH changed has no same-named file at its destination, so there
is nothing there to overwrite: the old object would survive as an
orphan and its module items would keep pointing at it."
  (let ((remote-folder (org-canvas--file-remote-folder-id canvas-id)))
    (and remote-folder folder-id
         (equal (format "%s" remote-folder) (format "%s" folder-id)))))

(defun org-canvas--file-clear-way-for-upload (data canvas-id folder-id)
  "Prepare Canvas to receive a replacement upload of DATA.
CANVAS-ID is the file being replaced and FOLDER-ID its destination.

An upload into the folder the file already lives in replaces it: the
preflight carries `on_duplicate=overwrite', and Canvas then repoints the
module items at the new object and keeps the old id resolving as an
alias.  Deleting first instead tears down everything hanging off the
file — that is what removed module item 5832544 from its module and
left the week with a hole in it (issues #71, #77).  The id rotates
either way; only the collateral differs.

A file that moved folders still deletes: there is no same-named file at
the destination for the overwrite to replace, so skipping the delete
would leave the old object behind with the module items still on it."
  (if (org-canvas--file-replace-in-place-p canvas-id folder-id)
      (org-canvas--log-debug org-canvas--logger
        "[Stage 3: Execute] Overwriting file ID %s in place; Canvas repoints its module items"
        canvas-id)
    (org-canvas--log-debug org-canvas--logger
      "[Stage 3: Execute] Replacing existing file ID: %s (moved folders, so the old object is deleted)"
      canvas-id)
    (push (plist-get data :display-name) org-canvas--file-recreated-ids)
    (condition-case err
        (org-canvas-api-request
         'DELETE (format "%s/api/v1/files/%s" org-canvas-base-url canvas-id))
      (error
       (org-canvas--log-warning org-canvas--logger
         "[Stage 3: Execute] Could not delete old file: %s"
         (error-message-string err))))))

(cl-defun org-canvas--file-push-to-api (data)
  "Execute the full 3-step upload process for DATA.
Returns the dry-run sentinel `org-canvas--dry-run-response' without
contacting Canvas when `org-canvas--dry-run' is non-nil.  The guard sits
here rather than at the call site so a single check covers both the
DELETE of the old file object and the 3-step upload."
  (let* ((canvas-id (plist-get data :canvas-id))
         (display-name (plist-get data :display-name))
         (local-path (plist-get data :local-path))
         (folder-path (plist-get data :folder-path)))

    (when org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s '%s'"
        (if canvas-id "REPLACE" "UPLOAD") display-name)
      (cl-return-from org-canvas--file-push-to-api org-canvas--dry-run-response))

    (org-canvas--log-info org-canvas--logger "[Stage 3: Execute] Uploading '%s'" display-name)

    ;; Get or create the target folder
    (let ((folder-id (org-canvas--file-target-folder-id folder-path)))

      (org-canvas--log-debug org-canvas--logger "[Stage 3: Execute] Target folder ID: %s" folder-id)

      (when canvas-id
        (org-canvas--file-clear-way-for-upload data canvas-id folder-id))

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
              (org-canvas--log-info org-canvas--logger "[Stage 3: Execute] Upload complete for '%s'" display-name)
              final-response)
          (error
           (org-canvas--log-error org-canvas--logger
             "[Stage 3: Execute] Upload failed for '%s' (path: %s, folder: %s): %s"
             display-name local-path
             (if (string-empty-p folder-path) "root" folder-path)
             (error-message-string err))
           ;; The old file object is already gone by now, so before giving
           ;; up check whether Canvas stored the upload anyway.
           (or (org-canvas--file-recover-upload-id data folder-id)
               (signal (car err) (cdr err)))))))))

(defun org-canvas--file-search-by-name (display-name folder-path &optional folder-id)
  "Search for a file with DISPLAY-NAME in FOLDER-PATH on Canvas.
The Canvas search is course-wide, so when FOLDER-ID is given only files
actually living in that folder are considered — otherwise a same-named
file elsewhere in the course could be mistaken for this one."
  (org-canvas--log-info org-canvas--logger "[Stage 3: Search] Looking for '%s' in '%s'..."
    display-name (if (string-empty-p folder-path) "root" folder-path))
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "files"))
             (params `(("search_term" . ,display-name)))
             (results (append (org-canvas-api-request 'GET endpoint :params params) nil)))
        (org-canvas--log-debug org-canvas--logger "[Stage 3: Search] Found %d results" (length results))
        (cl-find-if (lambda (f)
                      (and (string= (alist-get 'display_name f) display-name)
                           (or (null folder-id)
                               (equal (alist-get 'folder_id f) folder-id))))
                    results))
    (error
     (org-canvas--log-warning org-canvas--logger "[Stage 3: Search] Search failed: %s" (error-message-string err))
     nil)))

(defun org-canvas--file-recover-upload-id (data folder-id)
  "Look up DATA's file on Canvas in FOLDER-ID after a failed upload.
An upload that errors part-way — a timeout on a large file, or step 3
exhausting its confirmation retries — can still have been stored by
Canvas.  The old file object was already deleted at that point, so
giving up would leave CANVAS_ID pointing at a dead id while a live file
sits under a new one; the next sync would then upload a duplicate.

Returns the recovered Canvas file object, or nil when nothing usable was
found.  A match carrying the *old* id is rejected: that means the upload
never landed (and the delete did not either), so the entry must stay
dirty rather than be recorded as synced."
  (let* ((display-name (plist-get data :display-name))
         (old-id (plist-get data :canvas-id))
         (found (org-canvas--file-search-by-name
                 display-name (plist-get data :folder-path) folder-id)))
    (cond
     ((null found)
      (org-canvas--log-debug org-canvas--logger
        "[Stage 3: Recover] No Canvas file named '%s' after the failure" display-name)
      nil)
     ((and old-id (string= (format "%s" (alist-get 'id found)) old-id))
      (org-canvas--log-debug org-canvas--logger
        "[Stage 3: Recover] '%s' still carries the old ID %s — upload did not land"
        display-name old-id)
      nil)
     (t
      (org-canvas--log-warning org-canvas--logger
        "[Stage 3: Recover] Upload of '%s' errored but the file exists on Canvas as ID %s — recording that ID"
        display-name (alist-get 'id found))
      found))))

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
      (org-canvas--log-info org-canvas--logger
        "[Usage Rights] Setting usage rights for file %s: %s" file-id justification)
      (condition-case err
          (org-canvas-api-request 'PUT endpoint :data payload)
        (error
         (org-canvas--log-warning org-canvas--logger
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
    (org-canvas--log-info org-canvas--logger "[Pre-flight] Ensuring %d folder path(s) exist..."
      (length folder-paths))
    (dolist (path folder-paths)
      (org-canvas--log-info org-canvas--logger "[Pre-flight] Creating/verifying folder: %s" path)
      (condition-case err
          (org-canvas--file-resolve-folder-by-path path)
        (error
         (org-canvas--log-error org-canvas--logger "[Pre-flight] Failed to create folder '%s': %s"
           path (error-message-string err))
         (org-canvas--signal 'org-canvas-api-error
           "Cannot create required folder '%s': %s" path (error-message-string err)))))
    ;; Brief delay to let Canvas process newly created folders
    (org-canvas--log-debug org-canvas--logger "[Pre-flight] Waiting for Canvas to process folders...")
    (sleep-for org-canvas--folder-creation-delay)
    (org-canvas--log-info org-canvas--logger "[Pre-flight] All folders ready")))

;;;; Main Sync Functions

(defvar org-canvas--file-force-upload nil
  "When non-nil, re-upload every file entry whatever its hash says.
Every skip in `org-canvas--file-sync-parsed-entry' reasons from a hash,
which describes the local file and the last successful upload — not
what Canvas actually holds.  When those disagree, and the reason is not
something a hash can see, the hash has no answer and the run needs
overriding.  The case this was built for: the files uploaded before
issue #70 was fixed, which carry a stray leading CRLF that the issue
#71 migration deliberately tolerates.

Bound by `org-canvas-files-force-reupload' and its at-point sibling.
Never set globally: a sync that re-uploads everything on every run is
the behaviour issue #71 was about.")

(defvar org-canvas--file-changed-ids nil
  "Display names of files whose CANVAS_ID changed during the current sync.
A replacement upload always lands under a NEW id, even when it
overwrites the file in place.  What it no longer costs is the module
items: Canvas repoints them at the new object and keeps the old id
resolving as an alias (issue #77).  Reset by `org-canvas-sync-files';
consumed by `org-canvas--file-warn-changed-ids' for the log.  No hash
invalidation is needed: each module's items digest (folded into its
PAYLOAD_HASH) includes resolved content ids, so exactly the affected
modules re-push their items on the next modules sync.")

(defun org-canvas--file-bytes-hash (data)
  "Return the md5 of DATA's local file bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (plist-get data :local-path))
    (md5 (current-buffer))))

(defun org-canvas--file-metadata-hash (data)
  "Return a hash of everything about DATA that Canvas can change in place.
Display name and folder belong here, not with the bytes: renaming or
moving a file is `PUT /api/v1/files/:id' with `name' and
`parent_folder_id', not a re-upload."
  (md5 (format "%s|%s|%s|%s|%s|%s|%s|%s|%s"
               (plist-get data :display-name)
               (plist-get data :folder-path)
               (plist-get data :published)
               (plist-get data :hidden)
               (plist-get data :unlock-at)
               (plist-get data :lock-at)
               (plist-get data :use-justification)
               (plist-get data :usage-license)
               (plist-get data :copyright))))

(defun org-canvas--file-content-hash (data)
  "Return DATA's stored sync hash, \"BYTES:METADATA\".
The two halves are kept separate because only the first requires a
re-upload.  Canvas cannot replace a file's bytes in place, so a content
change means delete-and-upload — which mints a new file id and breaks
every link that referenced the old one.  Metadata changes do not: they
are one PUT, and the id survives (issue #49)."
  (format "%s:%s"
          (org-canvas--file-bytes-hash data)
          (org-canvas--file-metadata-hash data)))

(defun org-canvas--file-hash-parts (stored)
  "Split STORED into a (BYTES . METADATA) cons, or nil if it is not split.
Entries synced before the split carry a single opaque md5, which cannot
say whether the bytes changed.  Rather than re-upload such an entry on
faith, `org-canvas--file-migrate-legacy-hash' compares it against the
copy Canvas holds and adopts a split hash when they agree (issue #71)."
  (when (and stored
             (string-match "\\`\\([0-9a-f]+\\):\\([0-9a-f]+\\)\\'" stored))
    (cons (match-string 1 stored) (match-string 2 stored))))

(defun org-canvas--file-legacy-hash-p (stored)
  "Return non-nil when STORED is a pre-split, opaque content hash."
  (and stored
       (not (org-canvas--file-hash-parts stored))
       (string-match-p "\\`[0-9a-f]+\\'" stored)))

(defun org-canvas--file-read-bytes (path)
  "Return the contents of PATH as a unibyte string, or nil if unreadable."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (buffer-string))))

(defun org-canvas--file-remote-bytes (canvas-id display-name)
  "Return the Canvas copy of CANVAS-ID as a unibyte string, or nil.
DISPLAY-NAME is used for logging.  One GET for the file object and one
download; failures are logged and answered with nil, which sends the
caller down the ordinary upload path."
  (let ((temp (make-temp-file "org-canvas-remote-")))
    (unwind-protect
        (condition-case err
            (let* ((item (org-canvas-api-request
                          'GET (format "%s/api/v1/files/%s"
                                       org-canvas-base-url canvas-id)))
                   (url (alist-get 'url item)))
              (when url
                (url-copy-file url temp t)
                (org-canvas--file-read-bytes temp)))
          (error
           (org-canvas--log-warning org-canvas--logger
             "[Files] Could not fetch Canvas copy of '%s' (%s); treating it as changed"
             display-name (error-message-string err))
           nil))
      (when (file-exists-p temp) (delete-file temp)))))

(defun org-canvas--file-bytes-match-p (local remote)
  "Return non-nil when LOCAL and REMOTE are the same file.
A copy uploaded before issue #70 was fixed carries two extra bytes in
front of its content, so a leading CRLF on the remote side is accepted
as a match: those files differ from their source only by the bug, and
re-uploading them to shed two bytes would cost every Canvas file id."
  (and local remote
       (or (equal local remote)
           (and (> (length remote) 2)
                (equal (substring remote 0 2) "\r\n")
                (equal (substring remote 2) local)))))

(defun org-canvas--file-migrate-legacy-hash (data file-hash)
  "Adopt FILE-HASH for DATA when the Canvas copy already matches.
An entry whose PAYLOAD_HASH predates the BYTES:METADATA split says
nothing about whether the content changed, and the old answer was to
re-upload it once to find out.  That is not free: a re-upload mints a
new Canvas file id, and Canvas silently drops the module items pointing
at the old one, so a routine sync could gut a course's module structure
until a later modules sync rebuilt it (issue #71).

One GET and one download settle the question instead.  When the bytes
agree, the split hash is written and the entry skips with its id
intact.  Returns non-nil when the entry was migrated."
  (let* ((display-name (plist-get data :display-name))
         (canvas-id (plist-get data :canvas-id))
         (local (org-canvas--file-read-bytes (plist-get data :local-path)))
         (remote (org-canvas--file-remote-bytes canvas-id display-name)))
    (when (org-canvas--file-bytes-match-p local remote)
      (if org-canvas--dry-run
          (org-canvas--log-info org-canvas--logger
            "[DRY-RUN] Would adopt a split hash for '%s' — Canvas already holds these bytes, id %s kept"
            display-name canvas-id)
        (org-canvas-org-set-property (point) org-canvas--prop-payload-hash
                                     file-hash)
        (org-canvas--log-info org-canvas--logger
          "[Migrate] '%s' matches Canvas — split hash adopted, id %s kept"
          display-name canvas-id))
      t)))

(cl-defun org-canvas--file-update-metadata (data)
  "Update DATA's Canvas file in place, without touching its bytes.
Canvas has no endpoint to replace a file's content, which is why a
content change still means delete-and-upload.  But it does support
`PUT /api/v1/files/:id' for name, folder and visibility, so flipping
PUBLISHED on an unchanged PDF no longer rotates the file id and breaks
every link pointing at it (issue #49).

Returns the updated Canvas file object, or `org-canvas--dry-run-response'
when previewing."
  (let ((canvas-id (plist-get data :canvas-id))
        (display-name (plist-get data :display-name)))
    (when org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger
        "[DRY-RUN] Would update metadata for '%s' (id %s kept)"
        display-name canvas-id)
      (cl-return-from org-canvas--file-update-metadata
        org-canvas--dry-run-response))
    (org-canvas--log-info org-canvas--logger
      "[Stage 3: Execute] Updating metadata for '%s' (id %s, no re-upload)"
      display-name canvas-id)
    (let ((payload (org-canvas--file-build-settings-payload data))
          (folder-id (org-canvas--file-target-folder-id
                      (plist-get data :folder-path))))
      (puthash "name" display-name payload)
      (when folder-id
        (puthash "parent_folder_id" folder-id payload))
      ;; Canvas rejects a rename or move that collides unless told what to
      ;; do; matching the upload path, files.org wins.
      (puthash "on_duplicate" "overwrite" payload)
      (org-canvas-api-request
       'PUT (format "%s/api/v1/files/%s" org-canvas-base-url canvas-id)
       :data payload))))

(defun org-canvas--file-pull-item (item pos)
  "Replace the local copy of the file at POS with Canvas's ITEM.
The pull option in conflict resolution: the heading's properties are
refreshed from the remote object and the local bytes are overwritten
with what Canvas currently serves.  Without this, a files conflict
would offer a pull that silently degraded to a skip."
  (org-canvas--file-pull-set-properties pos item)
  (let* ((raw (org-with-point-at pos (org-canvas--file-read-props pos)))
         (local-path (plist-get raw :local-path))
         (url (alist-get 'url item)))
    (when (and local-path url)
      (org-canvas--file-pull-download
       (or (alist-get 'display_name item) (plist-get raw :display-name))
       url local-path (alist-get 'size item) t))))

;; Files sync through their own loop rather than `org-canvas-define-sync',
;; so the macro never registers this for them (issue #67).
(org-canvas-register-pull-item-fn "Files" #'org-canvas--file-pull-item)

(defun org-canvas--file-check-conflict (data)
  "Return `push', `skip' or `pulled' for DATA's file.
Files never reach `org-canvas--push-to-api', whose conflict guard is
gated on PUT, so they were exempt from conflict detection entirely: a
file replaced in the Canvas web UI was overwritten with no diff, no
prompt and no warning (issue #49).  That matters most here, because a
content change is a delete plus re-upload — the least recoverable thing
the package does."
  (let ((org-canvas--current-pull-item-fn #'org-canvas--file-pull-item))
    (org-canvas--push-check-and-resolve-conflict
     "files" (plist-get data :canvas-id) data (plist-get data :display-name))))

(defun org-canvas--file-record-metadata-update (data response file-hash)
  "Record an in-place metadata update of DATA at point.
RESPONSE is the updated Canvas file object and FILE-HASH the hash to
store.  Unlike `org-canvas--file-record-upload' this does not re-apply
the visibility settings — the PUT already carried them — and the file
id is unchanged by construction, so there is no id rotation to report."
  (org-canvas--file-finalize data response)
  (let ((fid (alist-get 'id response)))
    (when (and fid (plist-get data :use-justification))
      (org-canvas--file-set-usage-rights fid data)))
  (org-canvas-org-set-property (point) org-canvas--prop-payload-hash file-hash))

(defun org-canvas--file-record-upload (data response file-hash old-id)
  "Record a completed upload of DATA at point.
RESPONSE is the Canvas file object, FILE-HASH the content hash to store
so an unchanged file is skipped next time, and OLD-ID the CANVAS_ID the
entry carried beforehand (nil on a first upload).  Saves the new id,
applies visibility and usage rights, and notes an id change for
`org-canvas--file-warn-changed-ids'.

Ordering matters on failure: CANVAS_ID is written first and
PAYLOAD_HASH last, so a settings or usage-rights error leaves the id
recorded (no duplicate upload next run) with the entry still dirty (the
settings are retried)."
  (org-canvas--file-finalize data response)
  (let ((fid (alist-get 'id response)))
    (when fid
      (org-canvas--file-apply-settings fid data))
    (when (and fid (plist-get data :use-justification))
      (org-canvas--file-set-usage-rights fid data))
    (when (and old-id fid (not (string= (format "%s" fid) old-id)))
      (push (plist-get data :display-name) org-canvas--file-changed-ids)))
  (org-canvas-org-set-property (point) org-canvas--prop-payload-hash file-hash))

(defun org-canvas--file-sync-metadata-only (data file-hash)
  "Apply DATA's metadata to Canvas in place and record the result.
FILE-HASH is stored on success.  Returns :success or :dry-run."
  (let ((response (org-canvas--file-update-metadata data)))
    (cond
     ((org-canvas--dry-run-response-p response)
      (message "Files [DRY-RUN] Would update metadata for '%s'"
               (plist-get data :display-name))
      :dry-run)
     (t
      (org-canvas--file-record-metadata-update data response file-hash)
      :success))))

(defun org-canvas--file-sync-upload (data file-hash old-id)
  "Upload DATA's file to Canvas and record the result.
FILE-HASH is stored on success; OLD-ID is the id the entry carried
beforehand.  Returns :success or :dry-run."
  (org-canvas--log-info org-canvas--logger "----------------------------------------")
  (let ((response (org-canvas--file-push-to-api data)))
    (cond
     ((org-canvas--dry-run-response-p response)
      (message "Files [DRY-RUN] Would %s '%s'"
               (if old-id "replace" "upload")
               (plist-get data :display-name))
      :dry-run)
     (t
      (org-canvas--file-record-upload data response file-hash old-id)
      :success))))

(defun org-canvas--file-sync-parsed-entry (data)
  "Sync the file described by DATA, with point on its heading.
Returns :success, :skip (nothing changed, or the user resolved a
conflict by skipping or pulling), or :dry-run.

Three outcomes, cheapest first: an entry whose stored hash still
matches is skipped; one whose bytes match but whose metadata does not
is updated in place, keeping its Canvas file id; only a genuine content
change falls back to delete-and-re-upload.  Before either write, a file
that already exists on Canvas is checked for remote modification —
files used to bypass conflict detection completely (issue #49).

During a dry run the writes return the sentinel and none of the
bookkeeping runs: no CANVAS_ID is written, no PAYLOAD_HASH is stored,
and no usage rights are set — a preview must never leave an entry
looking synced."
  (let* ((file-hash (org-canvas--file-content-hash data))
         (stored-hash (org-entry-get (point) org-canvas--prop-payload-hash))
         (old-id (plist-get data :canvas-id))
         (fresh (org-canvas--file-hash-parts file-hash))
         (parts (org-canvas--file-hash-parts stored-hash))
         (display-name (plist-get data :display-name)))
    (cond
     ;; Every skip below reasons from a hash; a forced run is the answer
     ;; when the hash is right and the bytes on Canvas are not.
     (org-canvas--file-force-upload
      (org-canvas--log-info org-canvas--logger
        "[Force] Re-uploading '%s' regardless of its hash" display-name)
      (org-canvas--file-sync-upload data file-hash old-id))
     ((and old-id stored-hash (string= file-hash stored-hash))
      (org-canvas--log-info org-canvas--logger
        "[Skip] '%s' unchanged — keeping Canvas file ID %s" display-name old-id)
      :skip)
     ((and old-id (not org-canvas--dry-run)
           (not (eq (org-canvas--file-check-conflict data) 'push)))
      :skip)
     ((and old-id parts fresh (string= (car parts) (car fresh)))
      (org-canvas--file-sync-metadata-only data file-hash))
     ;; A pre-split hash is not evidence of a content change (issue #71).
     ((and old-id (org-canvas--file-legacy-hash-p stored-hash)
           (org-canvas--file-migrate-legacy-hash data file-hash))
      :skip)
     (t
      (when (org-canvas--file-legacy-hash-p stored-hash)
        (org-canvas--log-warning org-canvas--logger
          "[Files] '%s' does not match its Canvas copy — re-uploading, which rotates its file id"
          display-name))
      (org-canvas--file-sync-upload data file-hash old-id)))))

(defun org-canvas--file-sync-single-entry (marker)
  "Process a single file entry at MARKER.
Returns :success, :skip (folder heading or unchanged file), :dry-run, or
:fail.  Unchanged files (same content hash as the last successful
upload) are skipped to keep their Canvas file ID stable.  When an upload
does replace a file's CANVAS_ID, the display name is recorded in
`org-canvas--file-changed-ids'."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (condition-case err
          (let ((data (org-canvas--file-parse-entry)))
            (if data
                (org-canvas--file-sync-parsed-entry data)
              :skip))
        (error
         (org-canvas--log-error org-canvas--logger "[FAILED] At point %d: %s"
           (marker-position marker) (error-message-string err))
         :fail)))))

(defun org-canvas--file-announce-legacy-hashes (targets)
  "Say up front how many TARGETS carry a pre-split PAYLOAD_HASH.
Those entries each cost one GET and one download before the run can
tell whether their bytes changed, and a course that predates the split
carries them on every file at once.  Silence here was the reported
surprise: a dry run announcing `Would REPLACE' for a file nobody had
touched read like a bug rather than a migration (issue #71)."
  (let ((legacy 0))
    (dolist (marker targets)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (when (org-canvas--file-legacy-hash-p
                 (org-entry-get (point) org-canvas--prop-payload-hash))
            (setq legacy (1+ legacy))))))
    (when (> legacy 0)
      (org-canvas--log-info org-canvas--logger
        "[Files] %d entr%s carry a pre-split PAYLOAD_HASH; each is compared against its Canvas copy once and migrated in place when unchanged.  Only a genuine difference is re-uploaded, which rotates the file id and drops the module items pointing at it"
        legacy (if (= legacy 1) "y" "ies")))
    legacy))

(defun org-canvas--file-warn-changed-ids (changed-names)
  "Log that CHANGED-NAMES were re-uploaded under new Canvas file ids.
An overwrite in place still mints a new id, so this stays true, but it
is no longer the news it was: Canvas repoints the module items at the
new object itself and keeps the old id resolving as an alias, so
nothing downstream is broken while it waits for a modules sync (issue
#77).  `org-canvas--file-warn-recreated-ids' covers the case that does
still lose its items."
  (org-canvas--log-info org-canvas--logger
    "[Files] %d file ID(s) changed (%s) — Canvas repointed their module items; the old ids still resolve"
    (length changed-names)
    (mapconcat (lambda (x) (format "'%s'" x)) changed-names ", ")))

(defun org-canvas--file-warn-recreated-ids (recreated-names)
  "Warn that RECREATED-NAMES were deleted and re-uploaded, not overwritten.
A file that moved folders cannot be overwritten in place — there is no
same-named file at the destination — so the old object is deleted, and
Canvas takes its module items down with it.  The module items digest
includes resolved content ids, so the modules referencing these files
come out dirty and re-push their items on the next modules sync (tier 2
of the same global run); until then those items are missing."
  (org-canvas--log-warning org-canvas--logger
    "[Files] %d file(s) moved folders and were recreated (%s) — Canvas dropped their module items; a modules sync restores them"
    (length recreated-names)
    (mapconcat (lambda (x) (format "'%s'" x)) recreated-names ", ")))

;;;###autoload
(defun org-canvas-sync-files ()
  "Synchronize files to Canvas."
  (interactive)
  (org-canvas-clear-log)
  ;; Clear session caches
  (setq org-canvas--file-root-folder-cache nil)
  (setq org-canvas--file-folder-cache (make-hash-table :test 'equal))
  (setq org-canvas--file-changed-ids nil)
  (setq org-canvas--file-recreated-ids nil)

  (let ((files-file (expand-file-name org-canvas-files-file)))
    (unless (and files-file (file-exists-p files-file))
      (org-canvas--signal 'org-canvas-config-error
        "Files manifest not found: %s" files-file))

    (display-buffer (get-buffer-create org-canvas--log-buffer-name))
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> STARTING FILE SYNC")
    (org-canvas--log-info org-canvas--logger "File: %s" files-file)
    (org-canvas--log-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
    (org-canvas--log-info org-canvas--logger "========================================")

    (org-canvas--log-info org-canvas--logger "[Pre-flight] Verifying course access...")
    (condition-case err
        (progn
          (org-canvas-api-request 'GET (org-canvas-api-course-endpoint ""))
          (org-canvas--log-info org-canvas--logger "[Pre-flight] Course accessible"))
      (error
       (org-canvas--log-warning org-canvas--logger "[Pre-flight] Warning: %s" (error-message-string err))))

    ;; Pre-create all necessary folders before uploading any files.
    ;; Folder creation is a POST, so a dry run only reports the paths.
    (let ((folder-paths (org-canvas--file-collect-folder-paths files-file)))
      (if org-canvas--dry-run
          (dolist (path folder-paths)
            (org-canvas--log-info org-canvas--logger
              "[DRY-RUN] Would ensure folder exists: %s" path))
        (org-canvas--file-ensure-folders-exist folder-paths)))

    (let ((targets nil)
          (success-count 0)
          (fail-count 0)
          (skip-count 0)
          (dry-run-count 0)
          ;; Batch conflict decisions (capital P/L/S) apply across the run,
          ;; as they do in the macro pipeline.
          (org-canvas--conflict-apply-all nil))
      ;; Gather all entries (at any level)
      (with-current-buffer (find-file-noselect files-file)
        (setq targets (org-map-entries (lambda () (point-marker)) t 'file)))

      (org-canvas--log-info org-canvas--logger "Found %d entries to process" (length targets))
      (org-canvas--file-announce-legacy-hashes targets)

      (dolist (marker targets)
        (let ((result (org-canvas--file-sync-single-entry marker)))
          (pcase result
            (:success (setq success-count (1+ success-count))
                      (message "Files [%d/%d] Synced"
                        (+ success-count skip-count fail-count)
                        (length targets)))
            (:skip (setq skip-count (1+ skip-count)))
            (:dry-run (setq dry-run-count (1+ dry-run-count)))
            (:fail (setq fail-count (1+ fail-count))
                   (message "Files [%d/%d] FAILED"
                     (+ success-count skip-count fail-count)
                     (length targets))))))

      (dolist (m targets) (set-marker m nil))

      ;; Save the org file after all modifications
      (with-current-buffer (find-file-noselect files-file)
        (org-canvas--save-buffer))

      (when org-canvas--file-changed-ids
        (org-canvas--file-warn-changed-ids
         (nreverse org-canvas--file-changed-ids))
        (setq org-canvas--file-changed-ids nil))

      (when org-canvas--file-recreated-ids
        (org-canvas--file-warn-recreated-ids
         (nreverse org-canvas--file-recreated-ids))
        (setq org-canvas--file-recreated-ids nil))

      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger ">>> FILE SYNC COMPLETE")
      (org-canvas--log-info org-canvas--logger
        "Success: %d | Failed: %d | Skipped (folders/unchanged): %d | Dry-run: %d"
        success-count fail-count skip-count dry-run-count)
      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--sync-record-feature-stats "Files"
        (list :success success-count :skip skip-count :fail fail-count
              :dry-run dry-run-count))
      (message "File Sync: %d success, %d failed, %d skipped%s."
               success-count fail-count skip-count
               (if (> dry-run-count 0)
                   (format ", %d would upload" dry-run-count)
                 "")))))

(defun org-canvas--file-force-confirm (what)
  "Ask before forcing a re-upload of WHAT.  Return non-nil to proceed."
  (org-canvas--confirm
   (format "Re-upload %s regardless of content, minting new Canvas file id(s)? "
           what)))

;;;###autoload
(defun org-canvas-files-force-reupload ()
  "Re-upload every file in files.org, ignoring its stored PAYLOAD_HASH.
For the case a hash cannot see: the local file and the last upload
agree, but the bytes on Canvas are wrong anyway.  That is true of every
file uploaded before issue #70 was fixed, which carries a stray leading
CRLF the issue #71 migration deliberately tolerates.

Each file lands under a new Canvas id.  Since issue #77 that is cheap —
the upload overwrites in place, Canvas repoints the module items itself
and the old ids keep resolving — but it is still a write against every
file in the course, so it asks first.  A file that moved folders is the
exception: it is deleted and recreated, and its module items are
restored by the next modules sync.

Run `org-canvas-sync' rather than this command if you also want those
module items repaired in the same pass."
  (interactive)
  (when (org-canvas--file-force-confirm "every file in files.org")
    (let ((org-canvas--file-force-upload t))
      (org-canvas-sync-files))))

;;;###autoload
(defun org-canvas-force-reupload-file-at-point ()
  "Re-upload the file at point, ignoring its stored PAYLOAD_HASH.
The single-entry form of `org-canvas-files-force-reupload', for
correcting one file rather than a course."
  (interactive)
  (org-back-to-heading t)
  (let ((display-name (org-get-heading t t t t)))
    (when (org-canvas--file-force-confirm (format "'%s'" display-name))
      (let ((org-canvas--file-force-upload t)
            (org-canvas--file-changed-ids nil)
            (org-canvas--file-recreated-ids nil))
        (org-canvas--file-sync-single-entry (point-marker))
        (org-canvas--save-buffer)
        (when org-canvas--file-recreated-ids
          (org-canvas--file-warn-recreated-ids
           (nreverse org-canvas--file-recreated-ids)))
        (message "Re-uploaded '%s'." display-name)))))

;;;; Delete Functions

(defun org-canvas--file-get-all-folders ()
  "Get all folders in the course, sorted by depth (deepest first for deletion)."
  (let* ((endpoint (org-canvas-api-course-endpoint "folders"))
         (folders (org-canvas-api-request-all-pages 'GET endpoint))
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
    (org-canvas--log-info org-canvas--logger "Found %d folders to delete (excluding root)" (length folders))
    (dolist (folder folders)
      (let* ((id (alist-get 'id folder))
             (name (alist-get 'full_name folder)))
        (org-canvas--log-info org-canvas--logger "Deleting folder: '%s' (ID: %s)" name id)
        (condition-case err
            (progn
              (org-canvas-api-request 'DELETE (format "%s/api/v1/folders/%s" org-canvas-base-url id)
                                      :params '(("force" . "true")))
              (setq deleted-count (1+ deleted-count))
              (org-canvas--log-info org-canvas--logger "  -> Deleted successfully"))
          (error
           (org-canvas--log-error org-canvas--logger "  -> Delete failed: %s" (error-message-string err))))))
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
    (org-canvas--log-warning org-canvas--logger "========================================")
    (org-canvas--log-warning org-canvas--logger ">>> STARTING MASS DELETION OF FILES AND FOLDERS")
    (org-canvas--log-warning org-canvas--logger "========================================")

    (let* ((endpoint (org-canvas-api-course-endpoint "files"))
           (remote-items (org-canvas-api-request-all-pages 'GET endpoint))
           (deleted-ids nil)
           (deleted-file-count 0)
           (deleted-folder-count 0))

      (org-canvas--log-info org-canvas--logger "Found %d files on Canvas" (length remote-items))

      ;; Delete all files first
      (dolist (item remote-items)
        (let* ((id (alist-get 'id item))
               (name (alist-get 'display_name item)))
          (org-canvas--log-info org-canvas--logger "Deleting file: '%s' (ID: %s)" name id)
          (condition-case err
              (progn
                (org-canvas-api-request 'DELETE (format "%s/api/v1/files/%s" org-canvas-base-url id))
                (push (number-to-string id) deleted-ids)
                (setq deleted-file-count (1+ deleted-file-count))
                (org-canvas--log-info org-canvas--logger "  -> Deleted successfully"))
            (error
             (org-canvas--log-error org-canvas--logger "  -> Delete failed: %s" (error-message-string err))))))

      ;; Delete all folders (after files are gone)
      (org-canvas--log-info org-canvas--logger "----------------------------------------")
      (org-canvas--log-info org-canvas--logger "Now deleting folders...")
      (setq deleted-folder-count (org-canvas--file-delete-all-folders))

      ;; Clean local properties
      (org-canvas--clean-local-sync-properties files-file)

      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d files, %d folders removed"
        deleted-file-count deleted-folder-count)
      (org-canvas--log-info org-canvas--logger "========================================")
      (message "Deletion complete. %d files, %d folders removed."
               deleted-file-count deleted-folder-count))))

(org-canvas-define-delete-at-point file
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/files/%s"
                           org-canvas-base-url id)))

;;;; Pull

(defun org-canvas--file-pull-download (display-name download-url local-path size
                                                    &optional force)
  "Download file DISPLAY-NAME from DOWNLOAD-URL to LOCAL-PATH if not present.
SIZE is used for logging; may be nil.  With FORCE, overwrite an
existing local file — that is what resolving a conflict by pulling
means, and the user chose it at the diff prompt."
  (when (and download-url (or force (not (file-exists-p local-path))))
    (condition-case err
        (progn
          (make-directory (file-name-directory local-path) t)
          (org-canvas--log-info org-canvas--logger
            "[Download] %s (%s bytes)" display-name (or size "?"))
          (url-copy-file download-url local-path t))
      (error
       (org-canvas--log-warning org-canvas--logger
         "[Download] Failed for %s: %s"
         display-name (error-message-string err))))))

(defun org-canvas--file-pull-folder-relative-path (full-name)
  "Return FULL-NAME with the Canvas \"course files\" prefix stripped.
Canvas folder full_names start with \"course files\" by convention.
Return \"\" for the root folder, the suffix when the prefix matches,
or FULL-NAME unchanged otherwise (so unrecognized layouts don't
silently lose path components)."
  (cond
   ((null full-name) "")
   ((string= full-name "course files") "")
   ((string-prefix-p "course files/" full-name)
    (substring full-name (length "course files/")))
   (t full-name)))

(defun org-canvas--file-pull-fetch-folders ()
  "Fetch all course folders from Canvas.
Return a hash table mapping folder id (number, `eql' test) to its
path relative to the Canvas root (string; `\"\"' for the root)."
  (let ((map (make-hash-table :test 'eql))
        (endpoint (org-canvas-api-course-endpoint "folders")))
    (dolist (folder (org-canvas-api-request-all-pages 'GET endpoint))
      (let ((id (alist-get 'id folder)))
        (when id
          (puthash id
                   (org-canvas--file-pull-folder-relative-path
                    (alist-get 'full_name folder))
                   map))))
    map))

(defun org-canvas--file-pull-mode ()
  "Return the pull mode for the current `files.org' buffer.

Returns one of three symbols:
- `fresh' if no entry has a CANVAS_ID property and no folder-only
  heading is present (initial pull, file empty or header-only).
- `flat' if at least one CANVAS_ID exists and every heading either
  has a CANVAS_ID or contains a file link (legacy layout from before
  folder hierarchy support).
- `hierarchical' if any heading lacks both a CANVAS_ID and a file link
  (a folder heading from a previous fresh-tree pull).  The dispatcher
  uses this to refuse re-pull on a nested layout."
  (let ((has-cid nil)
        (folder-only nil))
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-map-entries
      (lambda ()
        (let* ((heading (org-canvas--strip-statistics-cookie
                         (org-get-heading t t t t)))
               (link-path (org-canvas--file-extract-link-path heading))
               (cid (org-entry-get (point) "CANVAS_ID")))
          (when cid (setq has-cid t))
          (unless (or cid link-path)
            (setq folder-only t))))
      t 'file))
    (cond (folder-only 'hierarchical)
          (has-cid 'flat)
          (t 'fresh))))

(defun org-canvas--file-pull-group-by-folder (folder-map remote-items)
  "Group REMOTE-ITEMS by their folder-relative path.
FOLDER-MAP is the hash from `org-canvas--file-pull-fetch-folders'.
Returns an alist ((REL-PATH . ITEMS) ...).  Items whose `folder_id'
is missing from FOLDER-MAP fall under \"\" (root)."
  (let ((groups nil))
    (dolist (item remote-items)
      (let* ((folder-id (alist-get 'folder_id item))
             (rel-path (or (and folder-id (gethash folder-id folder-map)) ""))
             (cell (assoc rel-path groups)))
        (if cell
            (setcdr cell (cons item (cdr cell)))
          (push (cons rel-path (list item)) groups))))
    groups))

(defun org-canvas--file-pull-emit-file-heading
    (item depth rel-path content-dir total counter)
  "Emit a heading + properties for ITEM at DEPTH under REL-PATH.
COUNTER is a cons cell whose car is the running file count (mutated).
TOTAL is the count of all files for the progress message.
Downloads to CONTENT-DIR/REL-PATH/DISPLAY_NAME."
  (let* ((id (alist-get 'id item))
         (display-name (alist-get 'display_name item))
         (download-url (alist-get 'url item))
         (local-rel (if (string-empty-p rel-path)
                        display-name
                      (concat rel-path "/" display-name)))
         (link-target (concat "content/" local-rel))
         (heading-text (org-link-make-string
                        (concat "file:" link-target)
                        (org-canvas--file-sanitize-headline-desc
                         display-name)))
         (local-path (org-canvas--file-safe-local-path local-rel content-dir)))
    (insert (make-string depth ?*) " " heading-text "\n")
    (let ((pos (save-excursion (forward-line -1) (point))))
      (org-canvas-org-save-sync-state pos id)
      (org-canvas--file-pull-set-properties pos item))
    (setcar counter (1+ (car counter)))
    (message "Files [%d/%d] Pulling '%s'..." (car counter) total display-name)
    (org-canvas--file-pull-download
     display-name download-url local-path (alist-get 'size item))))

(defun org-canvas--file-pull-common-prefix-len (a b)
  "Return the count of leading elements shared between lists A and B."
  (let ((i 0))
    (while (and (nth i a) (nth i b)
                (string= (nth i a) (nth i b)))
      (setq i (1+ i)))
    i))

(defun org-canvas--file-pull-emit-folder-ancestors (current-parts new-parts)
  "Insert folder headings to descend from CURRENT-PARTS to NEW-PARTS.
Both are lists of path components.  Headings are emitted only for
the suffix of NEW-PARTS that's not already shared with CURRENT-PARTS."
  (let* ((common (org-canvas--file-pull-common-prefix-len current-parts new-parts))
         (i common))
    (while (< i (length new-parts))
      (insert (make-string (1+ i) ?*) " " (nth i new-parts) "\n")
      (setq i (1+ i)))))

(defun org-canvas--file-pull-emit-fresh-tree (folder-map remote-items content-dir)
  "Emit a folder-aware heading tree for REMOTE-ITEMS into the current buffer.
FOLDER-MAP maps folder id to relative path; empty path is root.
CONTENT-DIR is the local directory under which files are downloaded.

Sorts folder paths lexically and files within each folder by
`display_name'.  Emits each ancestor folder heading exactly once.
Files at the root land at level 1; files at depth N land at
level N+1.  Properties are written via `org-canvas-org-save-sync-state'
and `org-canvas--file-pull-set-properties' for parity with flat mode."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let* ((groups (org-canvas--file-pull-group-by-folder folder-map remote-items))
         (sorted-paths (sort (mapcar #'car groups) #'string<))
         (total (length remote-items))
         (counter (list 0))
         (current-parts nil))
    (dolist (rel-path sorted-paths)
      (let* ((parts (if (string-empty-p rel-path) nil
                      (split-string rel-path "/" t)))
             (file-depth (1+ (length parts)))
             (items (sort (cdr (assoc rel-path groups))
                          (lambda (a b)
                            (string< (alist-get 'display_name a)
                                     (alist-get 'display_name b))))))
        (org-canvas--file-pull-emit-folder-ancestors current-parts parts)
        (setq current-parts parts)
        (dolist (item items)
          (org-canvas--file-pull-emit-file-heading
           item file-depth rel-path content-dir total counter))))
    (car counter)))

(defun org-canvas--file-pull-set-properties (pos item)
  "Set visibility, content-type, size, and usage-rights properties at POS.
ITEM is the Canvas file object.  `locked' is the Publish control, so it
drives PUBLISHED; `hidden' is the separate unlisted state (issue #50)."
  (org-canvas--pull-set-boolean-property
   pos "PUBLISHED" (not (eq (alist-get 'locked item) t)))
  (org-canvas--pull-set-boolean-property
   pos "HIDDEN" (alist-get 'hidden item))
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

(defun org-canvas--file-pull-emit-flat (file remote content-dir)
  "Upsert REMOTE files as flat top-level headings in FILE.
FILE is the path to files.org, REMOTE is the list of file alists from
the Canvas API, CONTENT-DIR is the local download directory.
Preserves existing CANVAS_ID matches in place; new files are appended."
  (let ((total (length remote))
        (count 0))
    (dolist (item remote)
      (cl-incf count)
      (let* ((id (alist-get 'id item))
             (display-name (alist-get 'display_name item))
             (download-url (alist-get 'url item))
             (local-path (org-canvas--file-safe-local-path display-name content-dir))
             (heading-text (org-link-make-string
                            (format "file:content/%s" display-name)
                            (org-canvas--file-sanitize-headline-desc
                             display-name)))
             (pos (org-canvas--pull-upsert-heading file id heading-text)))
        (message "Files [%d/%d] Pulling '%s'..." count total display-name)
        (goto-char pos)
        (org-canvas-org-save-sync-state pos id)
        (org-canvas--file-pull-set-properties pos item)
        (org-canvas--file-pull-download
         display-name download-url local-path (alist-get 'size item))))
    count))

(defun org-canvas--file-detect-duplicates (items)
  "Warn for any group of items in ITEMS sharing size and content-type.
ITEMS is a list (or vector) of file API alists.  Two or more files
sharing the same `(size, content-type)` are flagged as possible
duplicates.  This is a heuristic: identical size and content-type does
not guarantee identical bytes, but it surfaces likely candidates such
as the near-duplicate PDFs Canvas accumulates in \"Uploaded Media\"
from RCE auto-uploads."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (item (append items nil))
      (let ((key (list (alist-get 'size item)
                       (alist-get 'content-type item))))
        (push item (gethash key groups))))
    (maphash
     (lambda (key entries)
       (when (> (length entries) 1)
         (org-canvas--log-warning org-canvas--logger
           "[Files] Possible duplicate group (size=%s type=%s): %s"
           (or (car key) "?") (or (cadr key) "?")
           (mapconcat (lambda (e) (alist-get 'display_name e))
                      entries ", "))))
     groups)))

;;;###autoload
(defun org-canvas-pull-files ()
  "Pull file metadata from Canvas into files.org.
Downloads file contents to the content/ directory.

On a fresh pull (no CANVAS_IDs in files.org and no folder-only
headings), reconstructs the Canvas folder hierarchy as nested Org
headings and downloads under content/<rel-path>/.

On a re-pull of an existing flat files.org, updates entries in place
without restructuring.  Refuses to re-pull a hierarchical files.org
with `user-error' (delete files.org and re-run to refresh)."
  (interactive)
  (org-canvas--start-operation "PULLING FILES")
  (let* ((file (expand-file-name org-canvas-files-file))
         (content-dir (expand-file-name
                       "content" (file-name-directory file)))
         (folder-map (condition-case err
                         (org-canvas--file-pull-fetch-folders)
                       (error
                        (org-canvas--log-warning org-canvas--logger
                          "[Files] Folder fetch failed (%s); falling back to flat layout."
                          (error-message-string err))
                        (make-hash-table :test 'eql))))
         (endpoint (org-canvas-api-course-endpoint "files"))
         (remote (org-canvas-api-request-all-pages 'GET endpoint))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--file-detect-duplicates remote)
    (org-canvas--pull-confirm-unsaved file "files")
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (unless (file-directory-p content-dir)
      (make-directory content-dir t))
    (with-current-buffer (find-file-noselect file)
      (let ((mode (org-canvas--file-pull-mode)))
        (pcase mode
          ('hierarchical
           (user-error
            "Hierarchical files.org detected; re-pull is not yet supported on a nested layout.  Delete files.org and re-run org-canvas-pull-files"))
          ('fresh
           (let ((emitted
                  (org-canvas--file-pull-emit-fresh-tree
                   folder-map remote content-dir)))
             (org-canvas--pull-write-file-header)
             (org-canvas--save-buffer)
             (org-canvas--log-info org-canvas--logger
               "Files pull complete (fresh tree): %d files" emitted)
             (message "Files pull complete: %d files." emitted)))
          ('flat
           (org-canvas--log-info org-canvas--logger
             "[Files] Existing flat files.org detected; running flat upsert. Delete files.org and re-pull to rebuild folder hierarchy.")
           (let ((emitted
                  (org-canvas--file-pull-emit-flat file remote content-dir)))
             (org-canvas--pull-write-file-header)
             (org-canvas--save-buffer)
             (org-canvas--log-info org-canvas--logger
               "Files pull complete (flat): %d files" emitted)
             (message "Files pull complete: %d files." emitted))))))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (setq org-canvas--file-id-cache nil)))

(provide 'org-canvas-files)
;;; org-canvas-files.el ends here
