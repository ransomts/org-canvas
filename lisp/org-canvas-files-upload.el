;;; org-canvas-files-upload.el --- The four-stage upload of course files  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The upload half of `org-canvas-files': what stands between a
;; heading's file link and the CANVAS_ID written back into it.
;;
;;   Stage 1, extraction: the local file exists and is within
;;     `org-canvas-max-file-size-mb'; the link, the display name and the
;;     heading's properties are read into the plist the rest uses.
;;   Stage 2, preparation: the upload request and the settings payload,
;;     with the naming quirks Canvas requires (a null filename and an
;;     unknown content type in the parameters it hands back).
;;   Stage 3, execution: the three-step upload — notify, send the bytes,
;;     confirm — with the confirm retry, the in-place replacement of an
;;     existing file, and the recovery of the id when Canvas answers
;;     with a Location header rather than a file object.
;;   Stage 4, finalization: CANVAS_ID into the heading, and the
;;     visibility settings and usage rights Canvas keeps beside the bytes.
;;
;; None of the four decides *whether* to upload: the sync in
;; `org-canvas-files.el' compares hashes and calls them.  This file
;; requires that one back, never the other way.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
(require 'json)
;; The configuration, the folder helpers and the session caches this
;; half reads.
(require 'org-canvas-files)

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

    (org-canvas--check-writable 'POST "a file upload")
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
                (org-canvas--wait delay (format "File upload: retry %d/%d"
                                                attempt max-retries))))
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

(defun org-canvas--file-clear-way-for-upload (data canvas-id folder-id &optional ctx)
  "Prepare Canvas to receive a replacement upload of DATA.
CANVAS-ID is the file being replaced and FOLDER-ID its destination.
CTX is the run context; a file deleted here is noted in its
:file-recreated-ids so the run can say whose module items it lost.

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
    (org-canvas--ctx-push ctx :file-recreated-ids (plist-get data :display-name))
    (condition-case err
        (org-canvas-api-request
         'DELETE (format "%s/api/v1/files/%s" org-canvas-base-url canvas-id))
      (error
       (org-canvas--log-warning org-canvas--logger
         "[Stage 3: Execute] Could not delete old file: %s"
         (error-message-string err))))))

(cl-defun org-canvas--file-push-to-api (data &optional ctx)
  "Execute the full 3-step upload process for DATA.
CTX is the run context, passed on to the replacement bookkeeping.
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
        (org-canvas--file-clear-way-for-upload data canvas-id folder-id ctx))

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
  "Update local Org file with CANVAS_ID using DATA and RESPONSE.
CANVAS_UPDATED_AT is stamped from `modified_at', the content
timestamp, so the baseline agrees with what conflict detection and the
drift report compare for files (issue #94)."
  (org-canvas--finalize-item data response
                             (org-canvas--finalize-options :title-key :display-name
                                                           :updated-field 'modified_at)))

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

(provide 'org-canvas-files-upload)
;;; org-canvas-files-upload.el ends here
