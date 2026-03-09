;;; org-canvas-core-org.el --- Org interaction utilities for org-canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; Org-mode property access, timestamp parsing, link resolution,
;; image resolution, pull helpers, the pull macro, and diagnostics.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)

;;;; 4. Org Interaction Layer

(defun org-canvas-org-get-property (pom property)
  "Get Org PROPERTY at POM (point or marker)."
  (org-entry-get pom property))

(defun org-canvas--interpret-boolean (value &optional default-true)
  "Interpret string VALUE as a boolean (pure, no buffer access).
If DEFAULT-TRUE is non-nil, returns t unless VALUE is \"false\".
Otherwise returns t only if VALUE is \"true\".
VALUE may be nil (property not set)."
  (if default-true
      (not (string-equal "false" value))
    (string-equal "true" value)))

(defun org-canvas--interpret-number (value &optional default)
  "Interpret string VALUE as a number (pure, no buffer access).
Returns DEFAULT (or 0) if VALUE is nil or empty."
  (if (and value (stringp value) (not (string-empty-p value)))
      (string-to-number value)
    (or default 0)))

(defun org-canvas-org-get-boolean-property (pom property &optional default-true)
  "Get PROPERTY at POM as a boolean value.
If DEFAULT-TRUE is non-nil, returns t unless property is \"false\".
Otherwise, returns t only if property is \"true\"."
  (let ((value (org-entry-get pom property)))
    (when (and value (not (member (downcase value) '("true" "false"))))
      (when (boundp 'org-canvas--logger)
        (elog-warning org-canvas--logger
          "[Validate] Property %s has value '%s' — expected 'true' or 'false'. Using %s"
          property value (if default-true "true" "false")))
      (message "Warning: %s '%s' is not true/false, using %s"
        property value (if default-true "true" "false")))
    (org-canvas--interpret-boolean value default-true)))

(defun org-canvas--to-json-boolean (value)
  "Convert VALUE to Canvas JSON boolean (t or :json-false)."
  (if value t :json-false))

(defun org-canvas-org-get-number-property (pom property &optional default)
  "Get PROPERTY at POM as a number.
Return DEFAULT (or 0) if property is nil or empty.
Warn if the value is non-numeric."
  (let ((val (org-entry-get pom property)))
    (if (and val (not (string-empty-p val)))
        (org-canvas--safe-string-to-number val property)
      (or default 0))))

(defun org-canvas--safe-string-to-number (val property)
  "Convert VAL to a number, warn if it is non-numeric.
PROPERTY is the property name, used in the warning message.
Returns the result of `string-to-number'."
  (when (and val
             (not (string-match-p "\\`-?[0-9]*\\.?[0-9]+\\'" val))
             (boundp 'org-canvas--logger))
    (elog-warning org-canvas--logger
      "[Parse] Property %s has non-numeric value \"%s\", treating as %s"
      property val (string-to-number val)))
  (string-to-number val))

(defun org-canvas--validate-property (value allowed property-name &optional default)
  "Validate VALUE is in ALLOWED list for PROPERTY-NAME.
Returns VALUE if valid, DEFAULT if nil, or DEFAULT with a warning if invalid."
  (cond
   ((null value) default)
   ((member value allowed) value)
   (t (when (boundp 'org-canvas--logger)
        (elog-warning org-canvas--logger
          "[Validate] %s: '%s' is not valid (expected: %s). Using '%s'"
          property-name value (string-join allowed ", ") (or default (car allowed))))
      (message "Warning: %s '%s' is not valid, using '%s'"
        property-name value (or default (car allowed)))
      (or default (car allowed)))))

(defun org-canvas--require-title (title pom entity-name)
  "Signal an error when TITLE is nil or empty.
POM is the point-or-marker (printed as integer in the message).
ENTITY-NAME is a human-readable label like \"Announcement\"."
  (when (or (null title) (string-empty-p title))
    (error "%s title cannot be empty at point %d"
           entity-name (if (markerp pom) (marker-position pom) pom))))

(defun org-canvas--push-non-nil-fields (data fields base)
  "Push non-nil fields from DATA plist into BASE alist.
FIELDS is a list of (PLIST-KEY . API-KEY) cons cells.
Returns the modified BASE."
  (dolist (field fields)
    (when-let* ((value (plist-get data (car field))))
      (push (cons (cdr field) value) base)))
  base)

(defun org-canvas--puthash-when (hash data key api-key &optional boolean-p)
  "Conditionally set API-KEY in HASH from DATA plist KEY when non-nil.
When BOOLEAN-P is non-nil, convert \"true\"/\"false\" to t/:json-false."
  (when-let* ((val (plist-get data key)))
    (puthash api-key
             (if boolean-p
                 (if (equal val "true") t :json-false)
               val)
             hash)))

(defun org-canvas-clear-sync-properties (pom)
  "Clear all sync-related properties from entry at POM."
  (dolist (prop org-canvas--sync-property-names)
    (org-entry-delete pom prop)))

(defun org-canvas--clean-local-sync-properties (file &optional id-property)
  "Remove sync properties from all headings with IDs in FILE.
ID-PROPERTY defaults to \"CANVAS_ID\"."
  (let ((id-prop (or id-property "CANVAS_ID")))
    (when (and file (file-exists-p file))
      (elog-info org-canvas--logger "Cleaning local properties...")
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (elog-debug org-canvas--logger "Removing properties for: %s"
                       (org-entry-get (point) id-prop))
           (org-canvas-clear-sync-properties (point)))
         (format "%s={.}" id-prop) 'file)
        (save-buffer)
        (elog-info org-canvas--logger "Saved %s" file)))))

(defun org-canvas-org-set-property (pom property value)
  "Set Org PROPERTY to VALUE at POM (point or marker).
Ensures the correct buffer is used if POM is a marker."
  (let ((buf (if (markerp pom) (marker-buffer pom) (current-buffer))))
    (with-current-buffer buf
      (save-excursion
	(goto-char pom)
	(org-entry-put (point) property value)))))

(defun org-canvas--normalize-id (id)
  "Ensure ID is a string.  Convert numbers; pass strings through."
  (if (numberp id) (number-to-string id) id))

(defun org-canvas--pull-set-boolean-property (pom property value)
  "Set boolean PROPERTY at POM.  Convert t to \"true\", else to \"false\"."
  (org-canvas-org-set-property
   pom property (if (eq value t) "true" "false")))

(defun org-canvas--alist-get-non-null (key alist)
  "Get KEY from ALIST, returning nil for null or :null values."
  (let ((v (alist-get key alist)))
    (if (or (null v) (eq v :null)) nil v)))

(defun org-canvas-org-save-sync-state (pom id &optional id-prop)
  "Standardize saving ID and LAST_SYNCED to the heading at POM.
ID-PROP defaults to `CANVAS_ID'."
  (let ((prop (or id-prop "CANVAS_ID"))
	(id-str (org-canvas--normalize-id id))
	(timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (org-canvas-org-set-property pom prop id-str)
    (org-canvas-org-set-property pom "LAST_SYNCED" timestamp)))

(defun org-canvas-org-parse-timestamp (ts-string)
  "Transform an Org timestamp TS-STRING into a Canvas ISO8601 string."
  (when ts-string
    (format-time-string "%Y-%m-%dT%H:%M:%SZ"
			(encode-time (org-parse-time-string ts-string))
			t)))

(defun org-canvas-current-iso8601-timestamp ()
  "Return the current time as a Canvas ISO8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun org-canvas--validate-date-ordering (data)
  "Warn if UNLOCK_AT, DUE_AT, LOCK_AT in DATA are in wrong order.
Also warns if any dates are in the past.
Compares ISO8601 strings lexicographically (works for UTC timestamps)."
  (let ((unlock (plist-get data :unlock_at))
        (due (plist-get data :due_at))
        (lock (plist-get data :lock_at))
        (title (or (plist-get data :title) "unknown")))
    (when (and unlock due (string> unlock due))
      (elog-warning org-canvas--logger
        "[Dates] '%s': UNLOCK_AT (%s) is after DUE_AT (%s)" title unlock due))
    (when (and due lock (string> due lock))
      (elog-warning org-canvas--logger
        "[Dates] '%s': DUE_AT (%s) is after LOCK_AT (%s)" title due lock))
    ;; Warn about past dates
    (let ((now (org-canvas-current-iso8601-timestamp)))
      (dolist (pair `((:due_at . "DUE_AT") (:lock_at . "LOCK_AT") (:unlock_at . "UNLOCK_AT")))
        (let ((date (plist-get data (car pair))))
          (when (and date (string< date now))
            (elog-warning org-canvas--logger
              "[Dates] '%s': %s (%s) is in the past" title (cdr pair) date)))))))

(defun org-canvas--for-each-entry (file query callback)
  "Call CALLBACK at each entry matching QUERY in FILE.
CALLBACK is called with point at each matching heading.
Returns a list of (success-count . fail-count)."
  (let ((targets nil)
	(success-count 0)
	(fail-count 0))
    (with-current-buffer (find-file-noselect file)
      (setq targets (org-map-entries (lambda () (point-marker)) query 'file)))
    (dolist (marker targets)
      (with-current-buffer (marker-buffer marker)
	(save-excursion
	  (goto-char (marker-position marker))
	  (condition-case err
	      (progn
		(funcall callback)
		(setq success-count (1+ success-count)))
	    (error
	     (setq fail-count (1+ fail-count))
	     (elog-error org-canvas--logger "[FAILED] At point %d: %s"
	       (marker-position marker) (error-message-string err)))))))
    (dolist (m targets) (set-marker m nil))
    (cons success-count fail-count)))

;;;; 4b. Shared Constants

(defconst org-canvas--sync-property-names
  '("CANVAS_ID" "CANVAS_URL" "LAST_SYNCED" "PAYLOAD_HASH")
  "Properties managed by the sync pipeline.")

(defconst org-canvas--bytes-per-mb 1048576.0
  "Number of bytes in one megabyte (for file size calculations).")

(defconst org-canvas--api-max-per-page '(("per_page" . "100"))
  "Default pagination params for Canvas API list requests.")

(defconst org-canvas--answer-weight-correct 100
  "Canvas answer weight for correct answers.")

(defconst org-canvas--answer-weight-incorrect 0
  "Canvas answer weight for incorrect answers.")

(defconst org-canvas--file-to-endpoint-map
  '(("pages.org" "pages" "CANVAS_URL")
    ("files.org" "files" "CANVAS_ID")
    ("assignments.org" "assignments" "CANVAS_ID")
    ("quizzes.org" "quizzes" "CANVAS_ID")
    ("discussions.org" "discussion_topics" "CANVAS_ID")
    ("announcements.org" "discussion_topics" "CANVAS_ID")
    ("modules.org" "modules" "CANVAS_ID"))
  "Map from org filename to (endpoint id-property) for Canvas URL resolution.")

(defconst org-canvas--orphan-feature-registry
  '((:name "Assignments"
     :endpoint "assignments"
     :file-var org-canvas-assignments-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field name
     :list-params nil
     :skip-fn nil)
    (:name "Pages"
     :endpoint "pages"
     :file-var org-canvas-pages-file
     :id-field url
     :id-property "CANVAS_URL"
     :title-field title
     :list-params nil
     :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
    (:name "Quizzes"
     :endpoint "quizzes"
     :file-var org-canvas-quizzes-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn nil)
    (:name "Discussions"
     :endpoint "discussion_topics"
     :file-var org-canvas-discussions-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn (lambda (item) (eq (alist-get 'is_announcement item) t)))
    (:name "Announcements"
     :endpoint "discussion_topics"
     :file-var org-canvas-announcements-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params (("only_announcements" . "true"))
     :skip-fn nil)
    (:name "Files"
     :endpoint "files"
     :file-var org-canvas-files-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field display_name
     :list-params nil
     :skip-fn nil)
    (:name "Rubrics"
     :endpoint "rubrics"
     :file-var org-canvas-rubrics-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn nil)
    (:name "Assignment Groups"
     :endpoint "assignment_groups"
     :file-var org-canvas-assignment-groups-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field name
     :list-params nil
     :skip-fn nil)
    (:name "Outcomes"
     :endpoint "outcome_groups"
     :file-var org-canvas-outcomes-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field title
     :list-params nil
     :skip-fn nil)
    (:name "Modules"
     :endpoint "modules"
     :file-var org-canvas-modules-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field name
     :list-params nil
     :skip-fn nil)
    (:name "Group Categories"
     :endpoint "group_categories"
     :file-var org-canvas-group-categories-file
     :id-field id
     :id-property "CANVAS_ID"
     :title-field name
     :list-params nil
     :skip-fn nil))
  "Registry of pushable features for orphan detection.
Each entry is a plist describing how to fetch remote items and match them
against local Org headings.  Sections and overrides are excluded
\(sections are pull-only, overrides are per-assignment\).")

(defun org-canvas--strip-statistics-cookie (title)
  "Remove Org statistics cookies like [1/3] or [33%] from TITLE.
Also strips text properties to prevent propertized strings from
leaking into API payloads and log output."
  (substring-no-properties
   (string-trim (replace-regexp-in-string "\\[\\([0-9]+/[0-9]+\\|[0-9]+%\\)\\]" "" title))))

;;;; 4c. Org Link Property Resolution

(defun org-canvas--resolve-link-property (link-string id-property source-file)
  "Resolve LINK-STRING to a property value by following the Org link.
ID-PROPERTY is the property to retrieve (e.g., \"CANVAS_ID\").
SOURCE-FILE is the file containing the link, used to resolve relative paths."
  (when (and link-string
             (or
              ;; Format with display text: [[file:path::*heading][display]]
              (string-match "\\[\\[file:\\(.+\\)::\\*\\(.+\\)\\]\\[" link-string)
              ;; Format without display text: [[file:path::*heading]]
              (string-match "\\[\\[file:\\([^]]+\\)::\\*\\([^]]+\\)\\]\\]" link-string)))
    (let* ((file (match-string 1 link-string))
           (heading (match-string 2 link-string))
           (abs-file (expand-file-name file (file-name-directory source-file)))
           ;; Unescape \[ and \] in heading text (Org escapes brackets in links)
           (clean-heading (replace-regexp-in-string
                           "\\\\[][]"
                           (lambda (m) (substring m 1))
                           heading)))
      (cond
       ((not (file-exists-p abs-file))
        (elog-warning org-canvas--logger
          "[Links] File not found: %s (from link %s)" abs-file link-string)
        nil)
       (t
        (let ((heading-point (org-canvas--find-heading-in-file abs-file clean-heading)))
          (if (not heading-point)
              (progn
                (elog-warning org-canvas--logger
                  "[Links] Heading '%s' not found in %s" clean-heading abs-file)
                nil)
            (with-current-buffer (find-file-noselect abs-file)
              (let ((value (org-entry-get heading-point id-property)))
                (unless value
                  (elog-warning org-canvas--logger
                    "[Links] Property %s not set on '%s' in %s"
                    id-property clean-heading abs-file))
                value)))))))))

(defun org-canvas--resolve-link-or-raw (pom property id-property source-file)
  "Get PROPERTY at POM, resolving Org links or returning raw value.
If the value starts with [[, resolve via `org-canvas--resolve-link-property'
using ID-PROPERTY from the link target.  SOURCE-FILE provides resolution
context.  Otherwise return the raw string."
  (let ((raw (org-canvas-org-get-property pom property)))
    (if (and raw (string-prefix-p "[[" raw))
        (org-canvas--resolve-link-property raw id-property source-file)
      raw)))

;;;; 4d. Section Name -> ID Resolution

(defun org-canvas--find-section-id-by-name (name sections-file)
  "Look up section NAME in SECTIONS-FILE and return its CANVAS_ID, or nil."
  (with-current-buffer (find-file-noselect sections-file)
    (save-excursion
      (goto-char (point-min))
      (let ((found nil))
        (org-map-entries
         (lambda ()
           (when (string= (org-get-heading t t t t) name)
             (setq found (org-entry-get (point) "CANVAS_ID"))))
         "LEVEL=1" 'file)
        found))))

(defun org-canvas--resolve-single-section-name (name sections-file)
  "Resolve a single section NAME to its CANVAS_ID.
SECTIONS-FILE is the expanded path to sections.org (may be nil).
Returns the ID string, or nil if unresolvable."
  (cond
   ((string-match-p "\\`[0-9]+\\'" name) name)
   ((and sections-file (file-exists-p sections-file))
    (let ((canvas-id (org-canvas--find-section-id-by-name name sections-file)))
      (unless canvas-id
        (elog-warning org-canvas--logger
          "[Sections] Could not resolve section name '%s' to CANVAS_ID" name)
        (message "Warning: Section '%s' not found in sections.org" name))
      canvas-id))
   (t
    (elog-warning org-canvas--logger
      "[Sections] Cannot resolve section name '%s' (sections file not available)" name)
    (message "Warning: Cannot resolve section '%s' (no sections file)" name)
    nil)))

(defun org-canvas--resolve-section-names-to-ids (names-string)
  "Resolve comma-separated section NAMES-STRING to comma-separated CANVAS_IDs.
Look up each name as a heading in the sections file and return its CANVAS_ID.
Names that are already numeric are passed through unchanged.
Unresolvable names are warned about and skipped.
Returns the resolved ID string, or nil if nothing resolved."
  (when (and names-string (not (string-empty-p names-string)))
    (let* ((sections-file (when (boundp 'org-canvas-sections-file)
                            (expand-file-name (symbol-value 'org-canvas-sections-file))))
           (names (mapcar #'string-trim (split-string names-string "," t)))
           (ids (delq nil (mapcar (lambda (name)
                                    (org-canvas--resolve-single-section-name
                                     name sections-file))
                                  names))))
      (when ids
        (mapconcat #'identity ids ",")))))

;;;; 4e. Cross-file Link Resolution for HTML Export

(defun org-canvas--unescape-org-brackets (s)
  "Unescape \\=\\[ and \\=\\] in S to [ and ]."
  (replace-regexp-in-string
   "\\\\\\]" "]"
   (replace-regexp-in-string "\\\\\\[" "[" s)))

(defun org-canvas--find-heading-in-file (abs-file heading)
  "Find HEADING in ABS-FILE, return point or nil.
Tries exact match first, then display-name fallback for link headings.
HEADING should already be unescaped (no \\=\\[ or \\=\\] escapes)."
  (when (file-exists-p abs-file)
    (with-current-buffer (find-file-noselect abs-file)
      (save-excursion
        (goto-char (point-min))
        (or
         ;; Exact heading match (tolerates extra whitespace after stars)
         (re-search-forward
          (format "^\\*+ +%s[ \t]*$" (regexp-quote heading)) nil t)
         ;; Display name match (for link headings like [[file:...][name]])
         (when (string-match "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" heading)
           (let ((display-name (match-string 1 heading)))
             (goto-char (point-min))
             (re-search-forward
              (format "^\\*+ +.*\\[%s\\]" (regexp-quote display-name))
              nil t))))))))

(defun org-canvas--resolve-to-canvas-url (file heading source-dir)
  "Resolve a cross-file link to a Canvas URL.
FILE is the relative path to a .org file.
HEADING is the heading search text (may contain escaped brackets).
SOURCE-DIR is the directory of the source file containing the link."
  (let* ((abs-file (expand-file-name file source-dir))
         (basename (file-name-nondirectory file))
         (url-info (assoc basename org-canvas--file-to-endpoint-map))
         (url-info (when url-info (cdr url-info))))
    (when (and url-info (file-exists-p abs-file))
      (let ((endpoint (car url-info))
            (id-prop (cadr url-info))
            (clean-heading (org-canvas--unescape-org-brackets heading)))
        (let ((heading-point (org-canvas--find-heading-in-file abs-file clean-heading)))
          (when heading-point
            (with-current-buffer (find-file-noselect abs-file)
              (let ((id (org-entry-get heading-point id-prop)))
                (when id
                  (format "%s/courses/%s/%s/%s"
                          org-canvas-base-url org-canvas-course-id
                          endpoint id))))))))))

(defun org-canvas--replace-link-with-canvas-url (link-info source-dir)
  "Replace Org link described by LINK-INFO with a Canvas URL link.
LINK-INFO is a plist with :start :end :file :heading :display.
SOURCE-DIR is the directory of the source .org file.
If resolution fails, replaces with plain display text."
  (let ((link-start (plist-get link-info :start))
        (link-end (plist-get link-info :end))
        (file (plist-get link-info :file))
        (heading (plist-get link-info :heading))
        (display (plist-get link-info :display)))
    (let ((canvas-url (org-canvas--resolve-to-canvas-url
                       file heading source-dir)))
      (delete-region link-start link-end)
      (goto-char link-start)
      (if canvas-url
          (insert (format "[[%s][%s]]" canvas-url display))
        (elog-warning org-canvas--logger
          "[Links] Unresolved: [[file:%s::*%s][%s]] → plain text"
          file heading display)
        (insert display)))))

(defun org-canvas--resolve-body-links (source-dir)
  "Resolve cross-file Org links in current buffer to Canvas URLs.
Replaces [[file:*.org::*HEADING][DISPLAY]] links with Canvas URL links.
Unresolvable links are replaced with their display text.
SOURCE-DIR is the directory of the source .org file."
  (goto-char (point-min))
  (while (re-search-forward
          "\\[\\[file:\\([^]:]+\\.org\\)::\\*" nil t)
    (let ((link-start (match-beginning 0))
          (file (match-string 1))
          (heading-start (point)))
      (when (search-forward "][" nil t)
        (let* ((heading (buffer-substring-no-properties
                         heading-start (- (point) 2)))
               (display-start (point)))
          (when (search-forward "]]" nil t)
            (let* ((link-end (point))
                   (display (buffer-substring-no-properties
                             display-start (- link-end 2))))
              (org-canvas--replace-link-with-canvas-url
               (list :start link-start :end link-end :file file
                     :heading heading :display display)
               source-dir))))))))

;;;; 4e-img. Inline Image Resolution

(defvar org-canvas--image-cache nil
  "Hash-table mapping local filename to Canvas preview URL.
Session-scoped; cleared at end of master sync.")

(defcustom org-canvas-image-folder "org-canvas-images"
  "Canvas folder name for inline images uploaded by org-canvas."
  :type 'string
  :group 'org-canvas)

(defconst org-canvas--image-extensions
  '("png" "jpg" "jpeg" "gif" "svg" "webp" "bmp")
  "File extensions recognized as inline images.")

(defun org-canvas--image-cache-load-folder ()
  "Fetch image folder contents and populate `org-canvas--image-cache'."
  (let* ((folders-url (org-canvas-api-course-endpoint
                       (format "folders/by_path/%s" org-canvas-image-folder)))
         (folder (car (last (org-canvas-api-request 'GET folders-url))))
         (folder-id (alist-get 'id folder)))
    (when folder-id
      (let ((files (org-canvas-api-request-all-pages
                    'GET (format "%s/api/v1/folders/%s/files"
                                 org-canvas-base-url folder-id))))
        (dolist (file files)
          (let ((name (alist-get 'display_name file))
                (url (alist-get 'url file)))
            (when (and name url)
              (puthash name url org-canvas--image-cache))))
        (elog-debug org-canvas--logger
          "[Images] Cache initialized: %d files in %s/"
          (hash-table-count org-canvas--image-cache)
          org-canvas-image-folder)))))

(defun org-canvas--image-cache-init ()
  "Initialize the image cache from Canvas folder listing.
Fetches the image folder contents and builds a filename->URL map."
  (unless org-canvas--image-cache
    (setq org-canvas--image-cache (make-hash-table :test 'equal))
    (condition-case nil
        (org-canvas--image-cache-load-folder)
      (error
       (elog-debug org-canvas--logger
         "[Images] Folder '%s' not found, will create on first upload"
         org-canvas-image-folder)))))

(defun org-canvas--image-ensure-folder ()
  "Ensure the image upload folder exists on Canvas.
Returns the folder ID."
  (condition-case nil
      (let* ((url (org-canvas-api-course-endpoint
                   (format "folders/by_path/%s" org-canvas-image-folder)))
             (folder (car (last (org-canvas-api-request 'GET url)))))
        (alist-get 'id folder))
    (error
     ;; Create the folder
     (elog-info org-canvas--logger "[Images] Creating folder: %s" org-canvas-image-folder)
     (let ((response (org-canvas-api-request
                      'POST (org-canvas-api-course-endpoint "folders")
                      :data `((name . ,org-canvas-image-folder)
                              (parent_folder_path . "/")))))
       (alist-get 'id response)))))

(defun org-canvas--image-replace-link (rep url)
  "Replace image link described by REP plist with Canvas URL link."
  (let ((display (plist-get rep :display))
        (filename (file-name-nondirectory (plist-get rep :path))))
    (goto-char (plist-get rep :start))
    (delete-region (plist-get rep :start) (plist-get rep :end))
    (insert (format "[[%s][%s]]" url (or display filename)))))

(defun org-canvas--resolve-single-image (rep source-dir folder-id-ref count total)
  "Process a single image link REP in SOURCE-DIR.
FOLDER-ID-REF is a cons cell whose car is the folder ID (lazily initialized).
COUNT and TOTAL are for progress logging.
Checks cache first, then uploads if file exists."
  (let* ((rel-path (plist-get rep :path))
         (abs-path (expand-file-name rel-path source-dir))
         (filename (file-name-nondirectory rel-path))
         (cached-url (gethash filename org-canvas--image-cache)))
    (cond
     (cached-url
      (elog-debug org-canvas--logger "[Images] Cache hit: %s" filename)
      (org-canvas--image-replace-link rep cached-url))
     ((file-exists-p abs-path)
      (condition-case err
          (progn
            (elog-info org-canvas--logger
              "[Images] Uploading %s (%d/%d)..." filename count total)
            (unless (car folder-id-ref)
              (setcar folder-id-ref (org-canvas--image-ensure-folder)))
            (let* ((notify-url (format "%s/api/v1/folders/%s/files"
                                       org-canvas-base-url (car folder-id-ref)))
                   (file-obj (org-canvas--upload-file abs-path notify-url))
                   (preview-url (format "%s/courses/%s/files/%s/preview"
                                        org-canvas-base-url
                                        org-canvas-course-id
                                        (alist-get 'id file-obj))))
              (puthash filename preview-url org-canvas--image-cache)
              (org-canvas--image-replace-link rep preview-url)
              t))
        (error
         (elog-warning org-canvas--logger
           "[Images] Failed to upload %s: %s"
           filename (error-message-string err))
         (message "WARNING: Image upload failed: %s" filename)
         nil)))
     (t
      (elog-warning org-canvas--logger
        "[Images] File not found: %s" abs-path)
      (message "WARNING: Image not found: %s" abs-path)
      nil))))

(defun org-canvas--resolve-image-links (source-dir)
  "Resolve inline image links in current buffer to Canvas URLs.
Replaces [[file:IMAGE]] links with Canvas preview URLs.
SOURCE-DIR is the directory of the source .org file.
Images are uploaded to the `org-canvas-image-folder' on Canvas."
  (goto-char (point-min))
  (let ((image-re (format "\\[\\[file:\\([^]]+\\.\\(%s\\)\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
                          (regexp-opt org-canvas--image-extensions)))
        (replacements nil))
    ;; Collect all matches first (avoid modifying buffer during search)
    (while (re-search-forward image-re nil t)
      (let ((link-start (match-beginning 0))
            (link-end (match-end 0))
            (rel-path (match-string 1))
            (display (match-string 3)))
        (push (list :start link-start :end link-end
                    :path rel-path :display display)
              replacements)))
    ;; Process in reverse order (last match first) to preserve positions
    (when replacements
      (org-canvas--image-cache-init)
      (let ((folder-id-ref (list nil))
            (count 0)
            (failed 0)
            (total (length replacements)))
        (dolist (rep replacements)
          (setq count (1+ count))
          (message "Images [%d/%d] Processing..." count total)
          (unless (org-canvas--resolve-single-image
                   rep source-dir folder-id-ref count total)
            (setq failed (1+ failed))))
        (when (> failed 0)
          (elog-warning org-canvas--logger
            "[Images] %d of %d images failed to process" failed total)
          (message "WARNING: %d of %d images failed. See *canvas-log*."
                   failed total))))))

(defvar org-export-with-broken-links)
(defvar org-export-with-sub-superscripts)
(defvar org-export-use-babel)

(defun org-canvas--export-subtree-body-to-html ()
  "Export current Org subtree to HTML, resolving cross-file links.
Returns the HTML string.  Cross-file links [[file:*.org::*...][...]]
are resolved to Canvas URLs when the target has a CANVAS_ID."
  (save-excursion
    (org-back-to-heading t)
    (let* ((beg (point))
           (end (save-excursion (org-end-of-subtree t) (point)))
           (content (buffer-substring beg end))
           (source-dir (file-name-directory
                        (or (buffer-file-name) default-directory))))
      (with-temp-buffer
        (let ((default-directory source-dir))
          (insert content)
          (org-mode)
          ;; Strip override tables (#+NAME: overrides + following table rows)
          (goto-char (point-min))
          (while (re-search-forward "^#\\+NAME: overrides\n" nil t)
            (let ((start (match-beginning 0)))
              (while (looking-at "^|")
                (forward-line 1))
              (delete-region start (point))))
          ;; Resolve cross-file links to Canvas URLs
          (org-canvas--resolve-body-links source-dir)
          ;; Resolve inline image links to Canvas URLs
          (org-canvas--resolve-image-links source-dir)
          ;; Export the subtree to HTML (body only)
          (goto-char (point-min))
          (let ((org-export-with-broken-links 'mark)
                (org-export-with-sub-superscripts nil)
                (org-export-use-babel nil))
            (org-export-as 'html t nil t nil)))))))

;;;; 4e. Pull Helpers (Canvas -> Org)

(defun org-canvas--html-to-org (html)
  "Convert HTML string to Org format using pandoc.
Returns the Org-mode text, or the raw HTML prefixed with a warning
if pandoc is not available."
  (if (not (executable-find "pandoc"))
      (concat "# WARNING: pandoc not found, raw HTML below\n" html)
    (with-temp-buffer
      (insert html)
      (let ((exit-code (call-process-region
                        (point-min) (point-max) "pandoc"
                        t t nil
                        "-f" "html" "-t" "org" "--wrap=none")))
        (if (= exit-code 0)
            (string-trim (buffer-string))
          (concat "# WARNING: pandoc conversion failed\n" html))))))

(defun org-canvas--html-to-org-inline (html)
  "Convert HTML to Org and collapse to a single line.
Delegates to `org-canvas--html-to-org', then replaces newlines with spaces
and trims whitespace.  Suitable for table cells, list items, and heading
titles where multi-line output would break formatting.
Returns empty string for nil or empty HTML."
  (if (or (null html) (string-empty-p html))
      ""
    (string-trim
     (replace-regexp-in-string "[\n\r]+" " "
                               (org-canvas--html-to-org html)))))

(defun org-canvas--pull-insert-body (body-html)
  "Replace current heading's body with Org-converted BODY-HTML.
Point must be at a heading.  Does nothing if BODY-HTML is nil or empty."
  (when (and body-html (not (string-empty-p body-html)))
    (let ((body-start (save-excursion (org-end-of-meta-data t) (point)))
          (body-end (save-excursion (org-end-of-subtree t) (point))))
      (delete-region body-start body-end)
      (goto-char body-start)
      (insert "\n" (org-canvas--html-to-org body-html) "\n"))))

(defun org-canvas--pull-set-timestamp-property (pos property iso8601)
  "Set PROPERTY at POS from ISO8601 string, converting to Org timestamp.
Does nothing if ISO8601 is nil or conversion fails."
  (when iso8601
    (let ((ts (org-canvas--iso8601-to-org-timestamp iso8601)))
      (when ts (org-canvas-org-set-property pos property ts)))))

(defun org-canvas--pull-upsert-heading (file canvas-id &optional title id-property)
  "Find or create a heading in FILE matched by CANVAS-ID.
If a level-1 heading with ID-PROPERTY (default \"CANVAS_ID\") equal to
CANVAS-ID exists, return its position.  Otherwise create a new heading
at the end of the buffer with TITLE and return its position.
Returns a point in the buffer visiting FILE."
  (let ((id-prop (or id-property "CANVAS_ID"))
        (buf (find-file-noselect (expand-file-name file))))
    (with-current-buffer buf
      (save-excursion
        ;; Search for existing heading by CANVAS_ID
        (goto-char (point-min))
        (let ((found nil))
          (org-map-entries
           (lambda ()
             (when (equal (org-entry-get (point) id-prop)
                          (format "%s" canvas-id))
               (setq found (point))))
           "LEVEL=1" 'file)
          (if found
              found
            ;; Create new heading at end
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "* %s\n" (or title "Untitled")))
            (org-back-to-heading t)
            (point)))))))

(defun org-canvas--iso8601-to-org-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org active timestamp.
Returns a string like \"<2026-01-15 Thu 10:00>\" or nil."
  (when (and iso8601 (stringp iso8601) (not (equal iso8601 ""))
             (not (eq iso8601 :null)))
    (let ((time (date-to-time iso8601)))
      (format-time-string "<%Y-%m-%d %a %H:%M>" time t))))

(defun org-canvas--iso8601-to-org-inactive-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org inactive timestamp.
Returns a string like \"[2026-01-15 Thu 10:00]\" or nil."
  (when (and iso8601 (stringp iso8601) (not (equal iso8601 ""))
             (not (eq iso8601 :null)))
    (let ((time (date-to-time iso8601)))
      (format-time-string "[%Y-%m-%d %a %H:%M]" time t))))

;;;; 4f. Declarative Pull-Item Macro

(defun org-canvas--pull-item-set-property (pos api-field property
                                               type item)
  "Set PROPERTY on heading at POS from ITEM's API-FIELD.
TYPE controls conversion: string, boolean, timestamp, number."
  (let ((value (alist-get api-field item)))
    (pcase type
      ('boolean
       (org-canvas--pull-set-boolean-property pos property value))
      ('timestamp
       (org-canvas--pull-set-timestamp-property pos property value))
      ('number
       (when (and value (not (eq value :null))
                  (or (not (numberp value)) (/= value 0)))
         (org-canvas-org-set-property pos property
                                      (format "%s" value))))
      ('non-null
       (let ((v (org-canvas--alist-get-non-null api-field item)))
         (when v (org-canvas-org-set-property pos property v))))
      (_  ;; string (default)
       (when (and value (not (eq value :null)))
         (org-canvas-org-set-property pos property
                                      (format "%s" value)))))))

(defmacro org-canvas-define-pull-item (feature &rest args)
  "Define a pull-item function for FEATURE from a property spec.

FEATURE is a symbol like \\='announcement or \\='discussion.

ARGS is a plist with the following keys:
  :body-field  - API alist key for body HTML (optional)
  :after-pull  - Function (item pos) for custom logic (optional)
  :properties  - List of (API-FIELD ORG-PROPERTY :type TYPE) specs

Type can be: string (default), boolean, timestamp, number, non-null.

Example:
  (org-canvas-define-pull-item discussion
    :body-field \\='message
    :properties
    ((discussion_type  \"DISCUSSION_TYPE\"  :type string)
     (allow_rating     \"ALLOW_RATING\"     :type boolean)))"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas--%s-pull-item"
                                  feature-name)))
         (body-field (plist-get args :body-field))
         (after-pull (plist-get args :after-pull))
         (properties (plist-get args :properties)))
    `(defun ,fn-name (item pos)
       ,(format "Set per-item properties for a pulled %s.\n\
ITEM is the API response alist, POS is the heading position."
                feature-name)
       ,@(mapcar
          (lambda (spec)
            (let ((api-field (nth 0 spec))
                  (org-prop (nth 1 spec))
                  (type (or (plist-get (nthcdr 2 spec) :type)
                            'string)))
              `(org-canvas--pull-item-set-property
                pos ',api-field ,org-prop ',type item)))
          properties)
       ,@(when body-field
           `((org-canvas--pull-insert-body
              (alist-get ',body-field item))))
       ,@(when after-pull
           `((funcall ,after-pull item pos))))))

;;;; 4g. Pull Macro

(defun org-canvas--pull-confirm-overwrite (file feature-name)
  "Prompt user to confirm overwrite if FILE already has content.
Signals `user-error' with FEATURE-NAME if aborted."
  (when (and (file-exists-p file)
             (> (file-attribute-size (file-attributes file)) 0)
             (not (y-or-n-p
                   (format "%s already exists.  Pull will overwrite headings.  Continue? "
                           (file-name-nondirectory file)))))
    (user-error "%s pull aborted" (capitalize feature-name))))

(defun org-canvas--pull-process-item (item file pull-config)
  "Process a single pulled ITEM into FILE.
PULL-CONFIG is a plist with :id-field :title-field :id-property :item-fn."
  (let* ((id-field (plist-get pull-config :id-field))
         (title-field (plist-get pull-config :title-field))
         (id-property (plist-get pull-config :id-property))
         (item-fn (plist-get pull-config :item-fn))
         (id (alist-get id-field item))
         (title (alist-get title-field item))
         (pos (org-canvas--pull-upsert-heading file id title id-property)))
    (goto-char pos)
    (when title (org-edit-headline title))
    (org-canvas-org-save-sync-state pos id id-property)
    (funcall item-fn item pos)))

(defmacro org-canvas-define-pull (feature &rest args)
  "Define `org-canvas-pull-FEATURE' function.
FEATURE is a symbol like \\='pages or \\='announcements.

ARGS is a plist with the following keys:
  :file        - Symbol for file path defcustom (required)
  :endpoint    - API endpoint suffix string (required)
  :params      - Extra GET params alist (optional)
  :item-fn     - Function (item pos) for per-item property setting (required)
  :skip-fn     - Predicate (item) to skip item when non-nil (optional)
  :id-field    - Alist key for item ID (default: \\='id)
  :title-field - Alist key for item title (default: \\='title)
  :id-property - Org property name for Canvas ID (default: \"CANVAS_ID\")

Generates an interactive function `org-canvas-pull-FEATURE' that:
  1. Clears the log and displays the log buffer
  2. Fetches all items from the Canvas API endpoint
  3. Upserts a heading for each item, saves sync state
  4. Calls ITEM-FN for module-specific property setting
  5. Saves the buffer and logs completion

Example:
  (org-canvas-define-pull announcements
    :file org-canvas-announcements-file
    :endpoint \"discussion_topics\"
    :params \\='((\"only_announcements\" . \"true\"))
    :item-fn #\\='org-canvas--announcement-pull-item)"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (pull-fn-name (intern (format "org-canvas-pull-%s" feature-name)))
         (file-expr (plist-get args :file))
         (endpoint-expr (plist-get args :endpoint))
         (params-expr (plist-get args :params))
         (item-fn (plist-get args :item-fn))
         (skip-fn (plist-get args :skip-fn))
         (id-field (or (plist-get args :id-field) ''id))
         (title-field (or (plist-get args :title-field) ''title))
         (id-property (or (plist-get args :id-property) "CANVAS_ID"))
         (op-label (upcase (replace-regexp-in-string "-" " " feature-name))))
    (unless file-expr (error "org-canvas-define-pull: :file is required"))
    (unless endpoint-expr (error "org-canvas-define-pull: :endpoint is required"))
    (unless item-fn (error "org-canvas-define-pull: :item-fn is required"))
    `(progn
       ;;;###autoload
       (defun ,pull-fn-name ()
         ,(format "Pull %s from Canvas into the local Org file." feature-name)
         (interactive)
         (org-canvas--start-operation ,(format "PULLING %s" op-label))
         (let* ((file (expand-file-name ,file-expr))
                (endpoint (org-canvas-api-course-endpoint ,endpoint-expr))
                (remote (org-canvas-api-request-all-pages
                         'GET endpoint ,params-expr))
                (count 0))
           (org-canvas--pull-confirm-overwrite file ,feature-name)
           (unless (file-exists-p file)
             (with-temp-file file (insert "")))
           (with-current-buffer (find-file-noselect file)
             (dolist (item remote)
               ,(let ((body
                       `(progn
                          (org-canvas--pull-process-item
                           item file
                           (list :id-field ,id-field :title-field ,title-field
                                 :id-property ,id-property :item-fn ,item-fn))
                          (cl-incf count))))
                  (if skip-fn
                      `(unless (funcall ,skip-fn item)
                         ,body)
                    body)))
             (save-buffer))
           (elog-info org-canvas--logger
             ,(format "%s pull complete: %%d items"
                      (capitalize feature-name)) count)
           (message ,(format "%s pull complete: %%d items."
                             (capitalize feature-name)) count))))))

;;;; 5. Diagnostics

(defun org-canvas-get-course-name ()
  "Fetch the name of the configured course from Canvas to verify access.
Returns the course name as a string.  Signals an error if the request fails."
  (let* ((endpoint (org-canvas-api-course-endpoint ""))
	 (response (org-canvas-api-request 'GET endpoint)))
    (alist-get 'name response)))

;;;###autoload
(defun org-canvas-test-connection ()
  "Interactive command to test the Canvas API connection."
  (interactive)
  (org-canvas-clear-log)
  (elog-info org-canvas--logger "Testing connection to %s (Course ID: %s)..."
    org-canvas-base-url org-canvas-course-id)

  (condition-case err
      (let ((name (org-canvas-get-course-name)))
	(elog-info org-canvas--logger "Success! Connected to course: %s" name)
	(message "Success! Connected to course: %s" name))
    (error
     (let ((msg (error-message-string err)))
       (elog-error org-canvas--logger "Connection Failed: %s" msg)
       (cond
        ((string-match-p "401" msg)
         (message "Connection failed: authentication error (HTTP 401). Regenerate your API token."))
        ((string-match-p "403" msg)
         (message "Connection failed: permission denied (HTTP 403). Check your token scope."))
        ((string-match-p "404" msg)
         (message "Connection failed: course not found (HTTP 404). Check your course ID."))
        ((string-match-p "resolve\\|getaddrinfo\\|network\\|unreachable" msg)
         (message "Connection failed: network error. Check your URL and internet connection."))
        (t
         (message "Connection failed: %s" msg)))))))

(defun org-canvas--preflight-check ()
  "Validate credentials and connection before syncing.
Signals error with actionable message on failure."
  (org-canvas--ensure-credentials)
  (condition-case err
      (let ((course (org-canvas-api-request 'GET
                      (org-canvas-api-course-endpoint ""))))
        (elog-info org-canvas--logger "[Preflight] Connected to: %s"
          (alist-get 'name course)))
    (error
     (error "Connection failed: %s\nCheck your API token, course ID, and network connection"
            (error-message-string err)))))

(provide 'org-canvas-core-org)
;;; org-canvas-core-org.el ends here
