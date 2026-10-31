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

;; The code is in three files: this one holds the configuration, the
;; folder helpers, the pre-flight folder check, the sync itself and the
;; delete commands; `org-canvas-files-upload.el' is the four-stage
;; upload, from the heading's link to the CANVAS_ID written back; and
;; `org-canvas-files-pull.el' rebuilds files.org from Canvas.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
(require 'url-util)

(declare-function org-canvas--validate-file-structure "org-canvas-validate-content")

;; The upload and pull halves require this file back; requiring them here
;; would make a loop of it.  `org-canvas.el' loads all three.
(declare-function org-canvas--file-parse-entry "org-canvas-files-upload")
(declare-function org-canvas--file-target-folder-id "org-canvas-files-upload"
                  (folder-path))
(declare-function org-canvas--file-build-settings-payload "org-canvas-files-upload"
                  (data))
(declare-function org-canvas--file-push-to-api "org-canvas-files-upload"
                  (data &optional ctx))
(declare-function org-canvas--file-read-props "org-canvas-files-upload"
                  (pom))
(declare-function org-canvas--file-apply-settings "org-canvas-files-upload"
                  (file-id data))
(declare-function org-canvas--file-finalize "org-canvas-files-upload"
                  (data response))
(declare-function org-canvas--file-set-usage-rights "org-canvas-files-upload"
                  (file-id data))
(declare-function org-canvas--file-pull-download "org-canvas-files-pull"
                  (display-name download-url local-path size
                                &optional force))
(declare-function org-canvas--file-pull-set-properties "org-canvas-files-pull"
                  (pos item))

;;;; Configuration

(defcustom org-canvas-files-file (org-canvas--path "files.org")
  "Path to the files.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-files-file "files.org")
(org-canvas-register-feature
 :name "Files" :endpoint "files"
 :file-var 'org-canvas-files-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'display_name
 :web-pages '((:id-property "CANVAS_ID" :path "files/%s")
              (:path-fn org-canvas--file-folder-web-path))
 ;; updated_at moves on metadata-only touches (a lock, a usage-rights
 ;; edit, a module-item relink); modified_at is the content timestamp
 ;; (issue #94).
 :modified-field 'modified_at)

(defun org-canvas--file-remote-published (item)
  "Return non-nil when the Canvas file ITEM is published to students.
A Canvas file object has no `published' field — it carries `locked',
`hidden', `hidden_for_user', `lock_at', `unlock_at' and
`visibility_level' — so reading `published' returned nil for every
file and the drift report called them all unpublished (issue #61).
PUBLISHED is the inverse of `locked', the same mapping the pull path
uses (issue #50)."
  (not (eq (alist-get 'locked item) t)))

(defun org-canvas--file-remote-usage-right (item key)
  "Return KEY from the Canvas file ITEM's nested `usage_rights' object."
  (let ((rights (alist-get 'usage_rights item)))
    (and (listp rights) (alist-get key rights))))

(defun org-canvas--file-remote-use-justification (item)
  "Return ITEM's usage-rights justification, or nil."
  (org-canvas--file-remote-usage-right item 'use_justification))

(defun org-canvas--file-remote-usage-license (item)
  "Return ITEM's usage-rights license code, or nil."
  (org-canvas--file-remote-usage-right item 'license))

(defun org-canvas--file-remote-copyright (item)
  "Return ITEM's usage-rights copyright holder, or nil."
  (org-canvas--file-remote-usage-right item 'legal_copyright))

(org-canvas-register-properties "files"
  :label "Files"
  :file-var 'org-canvas-files-file
  :query "LEVEL>0"
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :remote-fn org-canvas--file-remote-published
     :doc "Whether the file is published to students (false sets locked)")
    ,org-canvas--no-module-property-spec
    (:org-prop "HIDDEN" :data-key :hidden :type boolean
     :doc "Published but unlisted: reachable only by direct link")
    (:org-prop "UNLOCK_AT" :data-key :unlock_at :type timestamp
     :doc "Date to make file available")
    (:org-prop "LOCK_AT" :data-key :lock_at :type timestamp
     :doc "Date to hide file")
    (:org-prop "USE_JUSTIFICATION" :data-key :use_justification :type enum
     :values ,org-canvas--valid-use-justifications
     :remote-fn org-canvas--file-remote-use-justification
     :doc "How to resolve a name collision on upload (rename vs overwrite)")
    (:org-prop "USAGE_LICENSE" :data-key :usage-license :type string
     :remote-fn org-canvas--file-remote-usage-license
     :doc "Usage-rights license code (for example cc_by, public_domain)")
    (:org-prop "COPYRIGHT" :data-key :copyright :type string
     :remote-fn org-canvas--file-remote-copyright
     :doc "Copyright holder recorded with the usage rights"))
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

(defun org-canvas--file-folder-web-path ()
  "Return the Canvas web path of the folder heading at point, or nil.
Nil for a file heading, which is one whose headline holds a link; the
link is read from the raw headline, since `org-get-heading' strips it
on Org 9.7.  A folder's page is its path of folder names under the
course files, each escaped as Canvas's own folder links escape it: a
percent sign is first written \"&#37;\", since the Files page decodes
each name once more and a bare one is a malformed URI (issue #300)."
  (save-excursion
    (org-back-to-heading t)
    (let ((raw (when (looking-at org-complex-heading-regexp)
                 (match-string-no-properties 4)))
          (parts nil))
      (unless (org-canvas--file-extract-link-path raw)
        (push (org-canvas--strip-statistics-cookie (or raw "")) parts)
        (while (org-up-heading-safe)
          (push (org-canvas--strip-statistics-cookie (org-get-heading t t t t))
                parts))
        (concat "files/folder/"
                (mapconcat (lambda (name)
                             (url-hexify-string (string-replace "%" "&#37;" name)))
                           parts "/"))))))

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

;;;; Pre-flight Folder Creation

(defconst org-canvas--folder-creation-delay 1
  "Seconds to wait after creating folders before uploading files.
Canvas needs time to process newly created folders.")

(defun org-canvas--file-collect-folder-paths (files-file)
  "Collect all unique folder paths from FILES-FILE.
Returns a list of folder paths that need to exist for file uploads."
  (let ((folder-paths nil)
        (seen (make-hash-table :test 'equal)))
    (with-current-buffer (org-canvas--find-file-noselect files-file)
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
    (org-canvas--wait org-canvas--folder-creation-delay
                      "Files: waiting for Canvas to process new folders")
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

;; `org-canvas-define-sync' below registers this under the feature name
;; it is given; the registry entry is named "Files", so it is recorded
;; here by that name as well (issue #67).
(org-canvas-register-pull-item-fn "Files" #'org-canvas--file-pull-item)

(defun org-canvas--file-check-conflict (data &optional ctx)
  "Return `push', `skip' or `pulled' for DATA's file.
CTX is the run context; without one, a context naming the files pull
function is made so the pull option is still offered.
Files never reach `org-canvas--push-to-api', whose conflict guard is
gated on PUT, so they were exempt from conflict detection entirely: a
file replaced in the Canvas web UI was overwritten with no diff, no
prompt and no warning (issue #49).  That matters most here, because a
content change is a delete plus re-upload — the least recoverable thing
the package does."
  (let ((ctx (or ctx (org-canvas--sync-make-ctx
                      :feature-name "files"
                      :pull-item-fn #'org-canvas--file-pull-item))))
    ;; modified_at, not updated_at: a metadata-only touch is not a
    ;; remote content change (issue #94).
    (org-canvas--push-check-and-resolve-conflict
     data (plist-get data :canvas-id)
     (org-canvas--push-options
      :endpoint "files"
      :title-key :display-name
      :updated-field 'modified_at
      :ctx ctx))))

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

(defun org-canvas--file-record-upload (data response file-hash old-id &optional ctx)
  "Record a completed upload of DATA at point.
RESPONSE is the Canvas file object, FILE-HASH the content hash to store
so an unchanged file is skipped next time, and OLD-ID the CANVAS_ID the
entry carried beforehand (nil on a first upload).  Saves the new id,
applies visibility and usage rights, and notes an id change in CTX's
:file-changed-ids for `org-canvas--file-warn-changed-ids'.

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
      (org-canvas--ctx-push ctx :file-changed-ids (plist-get data :display-name))))
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

(defun org-canvas--file-sync-upload (data file-hash old-id &optional ctx)
  "Upload DATA's file to Canvas and record the result.
FILE-HASH is stored on success; OLD-ID is the id the entry carried
beforehand; CTX is the run context.  Returns :success or :dry-run."
  (org-canvas--log-info org-canvas--logger "----------------------------------------")
  (let ((response (org-canvas--file-push-to-api data ctx)))
    (cond
     ((org-canvas--dry-run-response-p response)
      (message "Files [DRY-RUN] Would %s '%s'"
               (if old-id "replace" "upload")
               (plist-get data :display-name))
      :dry-run)
     (t
      (org-canvas--file-record-upload data response file-hash old-id ctx)
      :success))))

(defun org-canvas--file-sync-parsed-entry (data &optional ctx)
  "Sync the file described by DATA, with point on its heading.
CTX is the run context.  Returns :success, :skip (nothing changed, or
the user resolved a conflict by skipping or pulling), or :dry-run.

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
      (org-canvas--file-sync-upload data file-hash old-id ctx))
     ((and old-id stored-hash (string= file-hash stored-hash))
      (org-canvas--log-info org-canvas--logger
        "[Skip] '%s' unchanged — keeping Canvas file ID %s" display-name old-id)
      :skip)
     ((and old-id (not org-canvas--dry-run)
           (not (eq (org-canvas--file-check-conflict data ctx) 'push)))
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
      (org-canvas--file-sync-upload data file-hash old-id ctx)))))

(defun org-canvas--file-push (data _payload &optional ctx)
  "Sync the file DATA describes; the `:push' of the files pipeline.
Files own their change detection (`:hash' is `push'): the tiers in
`org-canvas--file-sync-parsed-entry' compare bytes and metadata, decide
between a metadata PUT, a legacy-hash migration and a re-upload, and
record the outcome themselves, hash included.  So the runner sees only
the outcome: `skip' when nothing was sent, the dry-run sentinel when a
preview ran (`:dry-run' is `push', so the tiers' own [DRY-RUN] lines
reach the log), and t for an entry the tiers recorded — there is no
response to finalize and no hash for the runner to stamp.  CTX is the
run context, where an id change is noted."
  (pcase (org-canvas--file-sync-parsed-entry data ctx)
    (:skip 'skip)
    (:dry-run org-canvas--dry-run-response)
    (_ t)))

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

(defun org-canvas--file-sync-prepare (_ctx)
  "Ready a files run: caches, course access, folders, legacy hashes.
The `:prepare' of the files pipeline, run once before the first entry
\(and before a push at point).  Resets the session folder caches,
checks the course is reachable (a failure is a warning, not a stop),
pre-creates every folder the manifest names — a POST, so a dry run
only lists them — and says how many entries carry a pre-split hash
\(issue #71)."
  (setq org-canvas--file-root-folder-cache nil)
  (setq org-canvas--file-folder-cache (make-hash-table :test 'equal))
  (let ((files-file (expand-file-name org-canvas-files-file)))
    (org-canvas--log-info org-canvas--logger "[Pre-flight] Verifying course access...")
    (condition-case err
        (progn
          (org-canvas-api-request 'GET (org-canvas-api-course-endpoint ""))
          (org-canvas--log-info org-canvas--logger "[Pre-flight] Course accessible"))
      (error
       (org-canvas--log-warning org-canvas--logger "[Pre-flight] Warning: %s" (error-message-string err))))
    (let ((folder-paths (org-canvas--file-collect-folder-paths files-file)))
      (if org-canvas--dry-run
          (dolist (path folder-paths)
            (org-canvas--log-info org-canvas--logger
              "[DRY-RUN] Would ensure folder exists: %s" path))
        (org-canvas--file-ensure-folders-exist folder-paths)))
    (let ((markers (with-current-buffer (org-canvas--find-file-noselect files-file)
                     (org-map-entries (lambda () (point-marker)) t 'file))))
      (org-canvas--file-announce-legacy-hashes markers)
      (dolist (m markers) (set-marker m nil)))
    nil))

(defun org-canvas--file-sync-after (ctx)
  "Report the file ids a run rotated or recreated, as recorded in CTX.
The `:after-sync' of the files pipeline."
  (when (org-canvas--ctx-get ctx :file-changed-ids)
    (org-canvas--file-warn-changed-ids
     (reverse (org-canvas--ctx-get ctx :file-changed-ids))))
  (when (org-canvas--ctx-get ctx :file-recreated-ids)
    (org-canvas--file-warn-recreated-ids
     (reverse (org-canvas--ctx-get ctx :file-recreated-ids)))))

;; Files run on the shared pipeline with two declarations the other
;; content types do not need.  `:hash push': change detection is theirs —
;; a stored hash is bytes and metadata in two halves, compared by the
;; tiers in `org-canvas--file-sync-parsed-entry', so the runner neither
;; skips on it nor stamps it.  `:dry-run push': every write in the
;; three-step upload, the metadata PUT and the migration guards itself
;; (Hard Rule 1) and says which tier it would take, which is what a
;; files preview is for (issue #71), so the runner lets the push preview
;; rather than reporting from the snapshot.  Entries sit at any level
;; under folder headings, which the parser declines and the runner
;; counts as skips.
(org-canvas-define-sync files
  :file org-canvas-files-file
  :query "LEVEL>0"
  :parse #'org-canvas--file-parse-entry
  :build #'ignore
  :push #'org-canvas--file-push
  :finalize #'ignore
  :title-key :display-name
  :pull-item-fn #'org-canvas--file-pull-item
  :hash 'push
  :dry-run 'push
  :prepare #'org-canvas--file-sync-prepare
  :after-sync #'org-canvas--file-sync-after)

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
      (let* ((org-canvas--file-force-upload t)
             (ctx (org-canvas-sync-file-at-point)))
        (when (org-canvas--ctx-get ctx :file-recreated-ids)
          (org-canvas--file-warn-recreated-ids
           (reverse (org-canvas--ctx-get ctx :file-recreated-ids))))
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
  ;; A delete is a write on files.org, so a scoped identity is refused
  ;; before the Danger Zone runs (issue #550).
  (org-canvas--check-write-scope org-canvas-files-file "a delete-all")
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
           ;; The files about to go are written down before the deletes;
           ;; the folders after them are paths, not content (issue #555).
           (snapshot (org-canvas--delete-all-snapshot "files" remote-items))
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
                (push (org-canvas--normalize-id id) deleted-ids)
                (setq deleted-file-count (1+ deleted-file-count))
                (org-canvas--log-info org-canvas--logger "  -> Deleted successfully"))
            (error
             (org-canvas--log-error org-canvas--logger "  -> Delete failed: %s" (error-message-string err))))))

      ;; Delete all folders (after files are gone)
      (org-canvas--log-info org-canvas--logger "----------------------------------------")
      (org-canvas--log-info org-canvas--logger "Now deleting folders...")
      (setq deleted-folder-count (org-canvas--file-delete-all-folders))

      ;; Clean local properties
      (org-canvas--clean-local-sync-properties
       files-file (org-canvas--delete-kept-ids remote-items 'id deleted-ids))

      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d files, %d folders removed"
        deleted-file-count deleted-folder-count)
      (org-canvas--log-info org-canvas--logger "========================================")
      (message "Deletion complete. %d files, %d folders removed%s."
               deleted-file-count deleted-folder-count
               (org-canvas--delete-snapshot-note snapshot)))))

(org-canvas-define-delete-at-point file
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/files/%s"
                           org-canvas-base-url id)))

(provide 'org-canvas-files)
;;; org-canvas-files.el ends here
