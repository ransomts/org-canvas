;;; org-canvas-core-org.el --- Org interaction utilities for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Org-mode property access, timestamp parsing, link resolution,
;; image resolution, pull helpers, the pull macro, and diagnostics.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)

;; Forward declaration: defined in `org-canvas-files', loaded by the time
;; the rewriter runs during a pull.  Declared here to keep `core-org' free
;; of a feature-module dependency while still satisfying byte-compile.
(declare-function org-canvas--file-pull-download "org-canvas-files"
                  (display-name download-url local-path size))

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
        (org-canvas--log-warning org-canvas--logger
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
    (org-canvas--log-warning org-canvas--logger
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
        (org-canvas--log-warning org-canvas--logger
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
    (org-canvas--signal 'org-canvas-validation-error
      "%s title cannot be empty at point %d"
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

(defun org-canvas--save-buffer ()
  "Save the current buffer when modified and log the file path written.
Use in place of `save-buffer' so the sync log records each modified
Org file.  No-op when the buffer has no unsaved changes: completion-time
safety saves after per-item saves would otherwise log duplicate
\[Saved] lines for writes that never happened.  When the current buffer
has no associated file (e.g., a scratch buffer used for HTML export)
the save still happens but no log line is emitted."
  (when (buffer-modified-p)
    (save-buffer)
    (when buffer-file-name
      (org-canvas--log-info org-canvas--logger "[Saved] %s" buffer-file-name))))

(defun org-canvas--clean-local-sync-properties (file &optional id-property)
  "Remove sync properties from all headings with IDs in FILE.
ID-PROPERTY defaults to \"CANVAS_ID\"."
  (let ((id-prop (or id-property "CANVAS_ID")))
    (when (and file (file-exists-p file))
      (org-canvas--log-info org-canvas--logger "Cleaning local properties...")
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (org-canvas--log-debug org-canvas--logger "Removing properties for: %s"
                       (org-entry-get (point) id-prop))
           (org-canvas-clear-sync-properties (point)))
         (format "%s={.}" id-prop) 'file)
        (org-canvas--save-buffer)))))

(defun org-canvas-org-set-property (pom property value)
  "Set Org PROPERTY to VALUE at POM (point or marker).
Ensures the correct buffer is used if POM is a marker.

Binds `org-property-format' to \"%s %s\" so property names shorter than
10 characters are not padded with extra spaces (e.g. `:LICENSE:  private'
with two spaces).  We always emit a single space between the property
name and value for consistent diffs and easier scripting."
  (let ((buf (if (markerp pom) (marker-buffer pom) (current-buffer))))
    (with-current-buffer buf
      (save-excursion
	(goto-char pom)
        (let ((org-property-format "%s %s"))
	  (org-entry-put (point) property value))))))

(defun org-canvas--normalize-id (id)
  "Ensure ID is a string.  Convert numbers; pass strings through."
  (if (numberp id) (number-to-string id) id))

(defun org-canvas--registry-find-property (org-prop)
  "Scan `org-canvas--property-registry' for a spec with :org-prop = ORG-PROP.
Return the property spec plist, or nil.  First match wins (properties
registered under multiple feature keys are expected to share defaults)."
  (catch 'found
    (maphash
     (lambda (_feature feature-plist)
       (dolist (spec (plist-get feature-plist :properties))
         (when (string= (plist-get spec :org-prop) org-prop)
           (throw 'found spec))))
     org-canvas--property-registry)
    nil))

(defun org-canvas--pull-set-boolean-property (pom property value)
  "Set boolean PROPERTY at POM.
Convert t to \"true\", :json-false/nil to \"false\".  When the registry
declares PROPERTY as `:type boolean' and the resolved value matches the
registered (or implicit nil) default, emission is suppressed unless
`org-canvas-emit-defaults' is non-nil."
  (let* ((spec (org-canvas--registry-find-property property))
         (boolean-spec (and spec (eq (plist-get spec :type) 'boolean)))
         (default (plist-get spec :default))
         (normalized (cond ((eq value t) t)
                           ((eq value :json-false) nil)
                           ((null value) nil)
                           ((stringp value)
                            (cond ((string= value "true") t)
                                  ((string= value "false") nil)
                                  (t value)))
                           (t value))))
    (when (or org-canvas-emit-defaults
              (not boolean-spec)
              (not (eq (and normalized t) (and default t))))
      (org-canvas-org-set-property
       pom property (if normalized "true" "false")))))

(defun org-canvas--alist-get-non-null (key alist)
  "Get KEY from ALIST, returning nil for null or :null values."
  (let ((v (alist-get key alist)))
    (if (or (null v) (eq v :null)) nil v)))

(defun org-canvas-org-save-sync-state (pom id &optional id-prop)
  "Standardize saving the Canvas ID to the heading at POM.
ID-PROP defaults to `CANVAS_ID'.  File-level LAST_SYNCED is written
separately by `org-canvas--pull-write-file-header'."
  (let ((prop (or id-prop "CANVAS_ID"))
	(id-str (org-canvas--normalize-id id)))
    (org-canvas-org-set-property pom prop id-str)))

(defun org-canvas--pull-write-file-header (&optional time)
  "Write or replace the #+LAST_SYNCED header in the current buffer.
TIME is the moment to record, defaulting to now.  Push passes an
explicit time derived from Canvas\\='s own timestamps rather than the
local clock (see `org-canvas--sync-write-push-header\').
Idempotent: replaces an existing header in place; otherwise inserts
after the existing #+TITLE line, or at the top of the buffer."
  (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]" time)))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward "^#\\+LAST_SYNCED:.*$" nil t)
        (replace-match (format "#+LAST_SYNCED: %s" timestamp) t t))
       ((progn (goto-char (point-min))
               (re-search-forward "^#\\+TITLE:.*$" nil t))
        (end-of-line)
        (insert "\n#+LAST_SYNCED: " timestamp))
       (t
        (goto-char (point-min))
        (insert "#+LAST_SYNCED: " timestamp "\n"))))))

(defun org-canvas--pull-emit-empty-file (path label)
  "Write an empty-file self-documenting header to PATH for LABEL.
Overwrites any existing content.  Used when a successful pull
returned zero items so the resulting Org file is not silently blank."
  (with-temp-file path
    (insert (format "#+TITLE: %s\n" label))
    (insert (format "#+LAST_SYNCED: %s\n"
                    (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (insert "# Canvas returned 0 items at this sync.\n")))

(defun org-canvas--pull-label-for (feature-name)
  "Look up the human-readable label for FEATURE-NAME in the property registry.
Falls back to a capitalized feature name when no entry is registered."
  (or (plist-get (gethash feature-name org-canvas--property-registry) :label)
      (capitalize (replace-regexp-in-string "-" " " feature-name))))

(defun org-canvas--pull-read-file-header ()
  "Return the #+LAST_SYNCED timestamp from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+LAST_SYNCED: \\(.+\\)$" nil t)
      (match-string-no-properties 1))))

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
      (org-canvas--log-warning org-canvas--logger
        "[Dates] '%s': UNLOCK_AT (%s) is after DUE_AT (%s)" title unlock due))
    (when (and due lock (string> due lock))
      (org-canvas--log-warning org-canvas--logger
        "[Dates] '%s': DUE_AT (%s) is after LOCK_AT (%s)" title due lock))
    ;; Warn about past dates
    (let ((now (org-canvas-current-iso8601-timestamp)))
      (dolist (pair `((:due_at . "DUE_AT") (:lock_at . "LOCK_AT") (:unlock_at . "UNLOCK_AT")))
        (let ((date (plist-get data (car pair))))
          (when (and date (string< date now))
            (org-canvas--log-warning org-canvas--logger
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
	     (org-canvas--log-error org-canvas--logger "[FAILED] At point %d: %s"
	       (marker-position marker) (error-message-string err)))))))
    (dolist (m targets) (set-marker m nil))
    (cons success-count fail-count)))

;;;; 4b. Shared Constants

(defconst org-canvas--sync-property-names
  `("CANVAS_ID" "CANVAS_URL" "CANVAS_UPDATED_AT"
    ,org-canvas--prop-last-synced ,org-canvas--prop-payload-hash)
  "Properties managed by the sync pipeline.")

(defconst org-canvas--children-digest-excluded-props
  '("CANVAS_ID" "CANVAS_ITEM_ID" "CANVAS_URL" "LAST_SYNCED"
    "CANVAS_UPDATED_AT" "PAYLOAD_HASH")
  "Sync-state properties excluded from `org-canvas--org-children-digest'.
Finalize writes these right after the parent's payload hash is
computed, so including them would dirty the parent on every run.")

(defun org-canvas--org-children-digest (pom)
  "Digest the raw content of all child subtrees under the heading at POM.
Covers child headings, their property drawers, and their bodies —
everything below the parent's own body to the end of its subtree.
Property lines named in `org-canvas--children-digest-excluded-props'
are stripped first.  Returns \"none\" when the heading has no children.

Intended as `:hash-extra' material for modules whose child headings
sync inside finalize (quiz questions, new-quiz items): folding this
into the parent's payload hash makes child-level edits trigger a
re-sync instead of being skipped as unchanged."
  (save-excursion
    (goto-char pom)
    (org-back-to-heading t)
    (let* ((end (save-excursion (org-end-of-subtree t t) (point)))
           (start (save-excursion (outline-next-heading) (point))))
      (if (>= start end)
          "none"
        (md5 (replace-regexp-in-string
              (format "^[ \t]*:%s:.*\n?"
                      (regexp-opt org-canvas--children-digest-excluded-props t))
              ""
              (buffer-substring-no-properties start end)))))))

(defconst org-canvas--bytes-per-mb 1048576.0
  "Number of bytes in one megabyte (for file size calculations).")

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

;; Feature registry for orphan detection is populated dynamically
;; by `org-canvas-register-feature' calls in each feature module.
;; See `org-canvas--feature-registry' in org-canvas-core-config.el.

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
        (org-canvas--log-warning org-canvas--logger
          "[Links] File not found: %s (from link %s)" abs-file link-string)
        nil)
       (t
        (let ((heading-point (org-canvas--find-heading-in-file abs-file clean-heading)))
          (if (not heading-point)
              (progn
                (org-canvas--log-warning org-canvas--logger
                  "[Links] Heading '%s' not found in %s" clean-heading abs-file)
                nil)
            (with-current-buffer (find-file-noselect abs-file)
              (let ((value (org-entry-get heading-point id-property)))
                (unless value
                  (org-canvas--log-warning org-canvas--logger
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
        (org-canvas--log-warning org-canvas--logger
          "[Sections] Could not resolve section name '%s' to CANVAS_ID" name)
        (message "Warning: Section '%s' not found in sections.org" name))
      canvas-id))
   (t
    (org-canvas--log-warning org-canvas--logger
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
        (org-canvas--log-warning org-canvas--logger
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
        (org-canvas--log-debug org-canvas--logger
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
       (org-canvas--log-debug org-canvas--logger
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
     (org-canvas--log-info org-canvas--logger "[Images] Creating folder: %s" org-canvas-image-folder)
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
      (org-canvas--log-debug org-canvas--logger "[Images] Cache hit: %s" filename)
      (org-canvas--image-replace-link rep cached-url))
     ((file-exists-p abs-path)
      (condition-case err
          (progn
            (org-canvas--log-info org-canvas--logger
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
         (org-canvas--log-warning org-canvas--logger
           "[Images] Failed to upload %s: %s"
           filename (error-message-string err))
         (message "WARNING: Image upload failed: %s" filename)
         nil)))
     (t
      (org-canvas--log-warning org-canvas--logger
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
          (org-canvas--log-warning org-canvas--logger
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
          ;; Strip property drawers: the HTML exporter drops them anyway,
          ;; and property links (GROUP:, RUBRIC_LINK:) must not reach the
          ;; body link resolver, which cannot resolve them to Canvas URLs
          ;; and would emit false "Unresolved" warnings
          (goto-char (point-min))
          (while (re-search-forward org-property-drawer-re nil t)
            (delete-region (match-beginning 0)
                           (min (1+ (match-end 0)) (point-max))))
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

(defun org-canvas--html-strip-ids (html)
  "Remove all `id=\"...\"' attributes from HTML.
Pandoc renders HTML id attributes as Org radio targets (\"<<foo>>\"),
which leak structural anchors from Canvas's page templates into pulled
bodies.  Stripping ids before conversion suppresses the artifact."
  (when html
    (replace-regexp-in-string
     " id=\\(\"[^\"]*\"\\|'[^']*'\\)" "" html)))

(defconst org-canvas--customid-line-re
  "^[ \t]*:CUSTOM_ID:[ \t]+\\(.+?\\)[ \t]*$"
  "Match a `:CUSTOM_ID:' property drawer entry.  G1 = id value.")

(defconst org-canvas--anchor-link-re
  "\\[\\[#\\([^]\n]+\\)\\]\\[\\([^]\n]+\\)\\]\\]"
  "Match an Org link to a CUSTOM_ID anchor.  G1 = id, G2 = display text.")

(defun org-canvas--normalize-toc-target (s)
  "Return a fuzzy-match key for TOC display text S.
Lowercases, trims, and strips a leading `N.' or `N) ' numeric prefix
so a heading titled \"Overview\" matches a TOC link reading
\"1. Overview\".  Returns the empty string for nil."
  (if (or (null s) (string-empty-p s))
      ""
    (let ((s (downcase (string-trim s))))
      (replace-regexp-in-string "\\`[0-9]+[.)][ \t]*" "" s))))

(defun org-canvas--collect-customids (text)
  "Return an alist of (NORMALIZED-HEADING . CUSTOM-ID) for TEXT.
Walks Org headings in TEXT and pairs each one with its CUSTOM_ID
property drawer entry (if any).  Headings without a CUSTOM_ID are
not in the result."
  (let ((result nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ +\\(.+\\)$" nil t)
        (let ((heading (match-string-no-properties 1))
              (heading-end (match-end 0))
              (custom-id nil))
          (save-excursion
            (goto-char heading-end)
            (forward-line)
            ;; A pandoc-generated PROPERTIES drawer sits right after the
            ;; heading line.  Bound the search to the next ~10 lines AND
            ;; refuse to cross another heading.
            (let* ((bound-pos (save-excursion (forward-line 10) (point)))
                   (next-heading (save-excursion
                                   (and (re-search-forward "^\\*+ "
                                                           bound-pos t)
                                        (match-beginning 0))))
                   (bound (or next-heading bound-pos)))
              (when (re-search-forward
                     org-canvas--customid-line-re bound t)
                (setq custom-id
                      (string-trim (match-string-no-properties 1))))))
          (when custom-id
            (push (cons (org-canvas--normalize-toc-target heading)
                        custom-id)
                  result)))))
    (nreverse result)))

(defun org-canvas--rewrite-toc-links (text customid-by-heading)
  "Rewrite dangling `[[#X][Y]]' anchor links in TEXT.
CUSTOMID-BY-HEADING is the alist returned by
`org-canvas--collect-customids'.

For each link:
- If X is a known CUSTOM_ID (a value in CUSTOMID-BY-HEADING), the
  link already resolves and is kept unchanged.
- Else the link's display text Y is normalized and looked up
  against the heading map.  If a heading matches, the link target
  is rewritten to that heading's CUSTOM_ID.
- Else the link wrapper is dropped and only Y survives — better a
  plain phrase than a dangling anchor."
  (let ((known-ids (mapcar #'cdr customid-by-heading)))
    (replace-regexp-in-string
     org-canvas--anchor-link-re
     (lambda (match)
       (save-match-data
         (string-match org-canvas--anchor-link-re match)
         (let ((id (match-string 1 match))
               (display (match-string 2 match)))
           (cond
            ((member id known-ids) match)
            ((let ((target
                    (cdr (assoc (org-canvas--normalize-toc-target display)
                                customid-by-heading))))
               (and target (format "[[#%s][%s]]" target display))))
            (t display)))))
     text t t)))

(defun org-canvas--collect-referenced-customids (text)
  "Return de-duplicated CUSTOM_IDs referenced by `[[#X][Y]]' links in TEXT."
  (let ((ids nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward org-canvas--anchor-link-re nil t)
        (push (match-string-no-properties 1) ids)))
    (delete-dups ids)))

(defun org-canvas--prune-unreferenced-customids (text referenced-ids)
  "Strip `:CUSTOM_ID:' drawer lines from TEXT not in REFERENCED-IDS.
Collapses now-empty `:PROPERTIES:'/`:END:' blocks.  Returns the
processed text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward org-canvas--customid-line-re nil t)
      (let ((id (string-trim (match-string-no-properties 1))))
        (unless (member id referenced-ids)
          (delete-region (line-beginning-position)
                         (min (point-max) (1+ (line-end-position)))))))
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*:PROPERTIES:[ \t]*\n[ \t]*:END:[ \t]*\n?"
            nil t)
      (replace-match ""))
    (buffer-string)))

(defun org-canvas--repair-toc-and-prune-customids (text)
  "Repair dangling TOC links and prune orphan CUSTOM_IDs in TEXT.

Pandoc's HTML→Org conversion produces two correlated artifacts when
a Canvas page contains a table of contents:

1. Anchor links like `[[#orga904f23][1. Overview]]' that target the
   *original* HTML `id' attributes (which we strip before pandoc),
   so every TOC link points at a non-existent anchor.
2. A `:CUSTOM_ID:' drawer entry on every heading, pandoc-derived
   from the heading text — most are inert, since nothing in the
   converted body links to them.

This pass first re-targets dangling TOC links to the matching
heading (by display text), then drops any CUSTOM_ID that no
remaining link references.  Returns TEXT unchanged when nil or
empty."
  (if (or (null text) (string-empty-p text))
      text
    (let* ((map (org-canvas--collect-customids text))
           (rewritten (org-canvas--rewrite-toc-links text map))
           (used (org-canvas--collect-referenced-customids rewritten)))
      (org-canvas--prune-unreferenced-customids rewritten used))))

(defun org-canvas--html-to-org-post-process (text)
  "Clean up pandoc's Org output before insertion.

- Decode U+00A0 (non-breaking space, from `&nbsp;') to a regular
  space.  Without this, lines that originated as `<p>&nbsp;</p>'
  spacers in Canvas's WYSIWYG output stay as literal NBSP-only
  lines that look blank but aren't (`cat -A' shows `M-BM-').
- Collapse lines containing only whitespace into empty lines so
  spacer paragraphs don't survive as decorated blanks.
- Insert a space before an Org timestamp `<YYYY-MM-DD' when the
  preceding character is non-whitespace and not `<' or `[' —
  pandoc occasionally emits `Due:<2026-04-22>' with no separator
  when the source HTML had a narrow space or NBSP between label
  and date.
- Repair dangling TOC links and prune orphan `:CUSTOM_ID:' drawer
  entries (see `org-canvas--repair-toc-and-prune-customids').

Returns TEXT unchanged when nil."
  (when text
    (let* ((s (replace-regexp-in-string " " " " text))
           (s (replace-regexp-in-string "^[ \t]+$" "" s))
           (s (replace-regexp-in-string
               "\\([^][ \t<\n]\\)\\(<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
               "\\1 \\2"
               s))
           (s (org-canvas--repair-toc-and-prune-customids s)))
      s)))

(defun org-canvas--html-to-org (html)
  "Convert HTML string to Org format using pandoc.
Returns the Org-mode text, or the raw HTML prefixed with a warning
if pandoc is not available.  Output passes through
`org-canvas--html-to-org-post-process' to clean up NBSP characters,
whitespace-only lines, and missing spacing before inline timestamps."
  (if (not (executable-find "pandoc"))
      (concat "# WARNING: pandoc not found, raw HTML below\n" html)
    (with-temp-buffer
      (insert (org-canvas--html-strip-ids html))
      (let ((exit-code (call-process-region
                        (point-min) (point-max) "pandoc"
                        t t nil
                        "-f" "html" "-t" "org" "--wrap=none")))
        (if (= exit-code 0)
            (org-canvas--html-to-org-post-process
             (string-trim (buffer-string)))
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

(defvar org-canvas--file-id-cache nil
  "Hash mapping CANVAS_ID strings to relative file paths from `files.org'.
Lazily populated by `org-canvas--pull-insert-body'.  Reset to nil to
force a rebuild after `org-canvas-pull-files' rewrites the file list.")

(defconst org-canvas--canvas-file-url-re
  "\\[\\[\\(https?://[^]\n]+/files/\\([0-9]+\\)[^]\n]*\\)\\(?:\\]\\[\\([^]\n]*\\)\\)?\\]\\]"
  "Match an Org link wrapping a Canvas file URL.
Group 1 = full URL, group 2 = file ID, group 3 = optional description.")

(defun org-canvas--heading-file-link-path ()
  "If the current heading is a [[file:PATH][...]] link, return PATH; else nil."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (let ((title (match-string-no-properties 4)))
        (when (and title
                   (string-match "\\`\\[\\[file:\\([^]]+\\)\\]" title))
          (match-string 1 title))))))

(defun org-canvas--build-file-id-cache (files-file)
  "Walk FILES-FILE and return a hash of CANVAS_ID -> relative path.
Headings without a CANVAS_ID property or without a `[[file:...]]' title
link are skipped.  Returns an empty hash if FILES-FILE does not exist."
  (let ((cache (make-hash-table :test 'equal)))
    (when (and files-file (file-exists-p files-file))
      (with-current-buffer (find-file-noselect files-file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((id (org-entry-get (point) "CANVAS_ID"))
                  (path (org-canvas--heading-file-link-path)))
              (when (and id path)
                (puthash id path cache))))))))
    cache))

(defvar org-canvas--rewrite-folder-cache nil
  "Hash mapping Canvas folder ID (number) to a folder-relative path string.
Populated lazily by `org-canvas--rewrite-fetch-unknown-file' so a
batch of body rewrites in the same pull session reuses a single
GET per folder.  Reset to nil between sessions.")

(defun org-canvas--strip-course-files-prefix (full-name)
  "Strip the Canvas \"course files\" prefix from FULL-NAME.
Returns the empty string for nil or the bare \"course files\" root,
the suffix when the prefix matches, or FULL-NAME unchanged otherwise.
Mirrors `org-canvas--file-pull-folder-relative-path' so this module
does not depend on `org-canvas-files'."
  (cond
   ((null full-name) "")
   ((string= full-name "course files") "")
   ((string-prefix-p "course files/" full-name)
    (substring full-name (length "course files/")))
   (t full-name)))

(defun org-canvas--rewrite-fetch-folder-relpath (folder-id)
  "Return the folder-relative path for Canvas FOLDER-ID, fetching if needed.
Uses (and populates) `org-canvas--rewrite-folder-cache'.  Returns
the empty string when FOLDER-ID is nil or for the course root.
Returns nil on API failure so callers can decide whether to fall
back to a placeholder folder."
  (cond
   ((null folder-id) "")
   ((and org-canvas--rewrite-folder-cache
         (gethash folder-id org-canvas--rewrite-folder-cache)))
   (t
    (unless org-canvas--rewrite-folder-cache
      (setq org-canvas--rewrite-folder-cache (make-hash-table :test 'eql)))
    (condition-case err
        (let* ((url (format "%s/api/v1/folders/%s"
                            org-canvas-base-url folder-id))
               (folder (org-canvas-api-request 'GET url))
               (full-name (alist-get 'full_name folder))
               (rel (org-canvas--strip-course-files-prefix full-name)))
          (puthash folder-id rel org-canvas--rewrite-folder-cache)
          rel)
      (org-canvas-api-error
       (org-canvas--log-warning org-canvas--logger
         "[Rewrite] folder %s lookup failed: %s"
         folder-id (error-message-string err))
       nil)))))

(defun org-canvas--files-org-append-fetched-entry
    (rel-path display-name canvas-id content-type size)
  "Append a heading for a fetched file to `org-canvas-files-file'.
REL-PATH is the path under content/ (e.g. \"Uploaded Media/img.png\");
DISPLAY-NAME is the file's display name; CANVAS-ID is the Canvas file
ID (string or number); CONTENT-TYPE and SIZE may be nil.

Finds (or creates) a top-level `* <folder>' heading whose name matches
the parent folder of REL-PATH (or `* Uploaded Media' for files at the
content/ root) and appends a child file-link heading beneath it.
Saves the buffer.  Creates the file if it does not yet exist."
  (let* ((files-file (and (boundp 'org-canvas-files-file)
                          org-canvas-files-file))
         (parent (let ((dir (file-name-directory rel-path)))
                   (if dir
                       (directory-file-name dir)
                     "Uploaded Media")))
         (id-str (if (numberp canvas-id) (number-to-string canvas-id)
                   (format "%s" canvas-id))))
    (unless files-file
      (error "Variable `org-canvas-files-file' not set"))
    (unless (file-exists-p files-file)
      (with-temp-file files-file (insert "")))
    (with-current-buffer (find-file-noselect files-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((parent-re (format "^\\* +%s\\s-*$" (regexp-quote parent))))
         (unless (re-search-forward parent-re nil t)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "* " parent "\n")))
       ;; Point is on (or just past) the parent heading; descend to its end.
       (org-back-to-heading t)
       (org-end-of-subtree t t)
       (unless (bolp) (insert "\n"))
       (insert (format "** [[file:content/%s][%s]]\n" rel-path display-name))
       (let ((pos (save-excursion (forward-line -1) (point))))
         (org-canvas-org-save-sync-state pos id-str)
         (when content-type
           (org-canvas-org-set-property pos "CONTENT_TYPE" content-type))
         (when size
           (org-canvas-org-set-property pos "SIZE" (format "%s" size)))))
      (save-buffer))))

(defun org-canvas--rewrite-fetch-unknown-file (id cache)
  "Fetch Canvas file ID, download it, register it, and return the relpath.
On any API failure (401/403/404/timeout) record to the pull summary
and return nil so the rewriter passes the URL through unchanged.

On success: GET /api/v1/files/:id, derive the folder-relative path
via /api/v1/folders/:fid (cached in `org-canvas--rewrite-folder-cache'),
download to <org-canvas-directory>/content/<folder>/<display_name>
via `org-canvas--file-pull-download', append a heading to
`org-canvas-files-file' via `org-canvas--files-org-append-fetched-entry',
and `puthash' the resolved relpath into CACHE so subsequent calls in
the same session hit the cache.

Returns the relpath (e.g. \"content/Uploaded Media/screenshot.png\")
on success, or nil on failure."
  (condition-case err
      (let* ((url (format "%s/api/v1/files/%s" org-canvas-base-url id))
             (item (org-canvas-api-request 'GET url))
             (display-name (alist-get 'display_name item))
             (folder-id (alist-get 'folder_id item))
             (download-url (alist-get 'url item))
             (content-type (alist-get 'content-type item))
             (size (alist-get 'size item))
             (folder-rel (or (org-canvas--rewrite-fetch-folder-relpath folder-id)
                             "Uploaded Media"))
             (local-rel (if (string-empty-p folder-rel)
                            display-name
                          (concat folder-rel "/" display-name)))
             (rel-path (concat "content/" local-rel))
             (local-path (expand-file-name rel-path org-canvas-directory)))
        (org-canvas--file-pull-download
         display-name download-url local-path size)
        (org-canvas--files-org-append-fetched-entry
         local-rel display-name id content-type size)
        (puthash id rel-path cache)
        rel-path)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Rewrite] file %s fetch failed: %s"
       id (error-message-string err))
     (org-canvas--pull-summary-record
      :file (and (boundp 'org-canvas-files-file)
                 org-canvas-files-file
                 (file-name-nondirectory org-canvas-files-file))
      :item id
      :error (error-message-string err)
      :log-line (org-canvas--pull-summary-current-log-line))
     nil)))

(defun org-canvas--rewrite-canvas-file-urls (text cache)
  "Rewrite Org-bracketed Canvas file URLs in TEXT using CACHE.
A link `[[https://.../files/ID...]]' (optionally with a `][DESC]' tail)
is replaced by `[[file:RELPATH][DESC-or-FILENAME]]' when ID is a key in
CACHE.  When ID is missing from CACHE, attempts to fetch its metadata
from Canvas, download the file, register it in `org-canvas-files-file',
and rewrite the link; if the fetch fails the URL passes through
unchanged (the failure is recorded in the pull summary).
Returns TEXT unchanged when nil or empty."
  (if (or (null text) (string-empty-p text))
      text
    (replace-regexp-in-string
     org-canvas--canvas-file-url-re
     (lambda (match)
       ;; Capture the substring matches eagerly: the unknown-file fetch
       ;; below performs buffer operations that can clobber match data,
       ;; even though `replace-regexp-in-string' nominally guards it.
       (let* ((id (match-string 2 match))
              (desc (match-string 3 match))
              (relpath (or (gethash id cache)
                           (save-match-data
                             (org-canvas--rewrite-fetch-unknown-file
                              id cache)))))
         (if relpath
             (format "[[file:%s][%s]]"
                     relpath
                     (or desc (file-name-nondirectory relpath)))
           match)))
     text t t)))

(defun org-canvas--html-to-org-with-rewrite (html)
  "Convert HTML to Org text and rewrite Canvas file URLs to local links.
Uses (and lazily populates) `org-canvas--file-id-cache' to resolve
Canvas file IDs against `org-canvas-files-file'.  Returns the rewritten
Org text, or the empty string when HTML is nil or empty."
  (if (or (null html) (string-empty-p html))
      ""
    (let* ((org-text (org-canvas--html-to-org html))
           (cache (or org-canvas--file-id-cache
                      (setq org-canvas--file-id-cache
                            (org-canvas--build-file-id-cache
                             (bound-and-true-p org-canvas-files-file))))))
      (org-canvas--rewrite-canvas-file-urls org-text cache))))

(defun org-canvas--html-to-org-inline-with-rewrite (html)
  "Convert HTML to a single-line Org string with Canvas file URLs rewritten.
Returns the empty string for nil or empty HTML."
  (if (or (null html) (string-empty-p html))
      ""
    (string-trim
     (replace-regexp-in-string "[\n\r]+" " "
                               (org-canvas--html-to-org-with-rewrite html)))))

(defun org-canvas--pull-insert-body (body-html)
  "Replace current heading's body with Org-converted BODY-HTML.
Point must be at a heading.  Does nothing if BODY-HTML is nil or empty.
Canvas file URLs in the converted body are rewritten to local
`[[file:...]]' links via `org-canvas--rewrite-canvas-file-urls'."
  (when (and body-html (not (string-empty-p body-html)))
    ;; Anchor the deletion at the end of the metadata's last non-blank line
    ;; (the `:END:' of the drawer, or the heading line when there is no
    ;; drawer).  `org-end-of-meta-data' lands in a different spot depending on
    ;; whether a body already exists, which makes naive re-pull
    ;; non-idempotent: each sync prepends a blank line and appends a newline,
    ;; accumulating whitespace and churning the .org file.  Skipping back over
    ;; whitespace yields the same anchor every time, so re-pulling identical
    ;; content is a true no-op.
    (let* ((meta-end (save-excursion (org-end-of-meta-data t) (point)))
           ;; `to-end' (second t) extends past trailing blank lines so they
           ;; are part of the replaced region; otherwise a trailing newline
           ;; accumulates on every re-pull.
           (body-end (save-excursion (org-end-of-subtree t t) (point)))
           (body-start (save-excursion
                         (goto-char (min meta-end body-end))
                         (skip-chars-backward " \t\n")
                         (point)))
           (rewritten (org-canvas--html-to-org-with-rewrite body-html)))
      (delete-region body-start body-end)
      (goto-char body-start)
      (insert "\n" rewritten "\n"))))

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

(defvar org-canvas--pull-tz-cache nil
  "Resolved course timezone for the current pull session.
String IANA TZ name (e.g., \"America/New_York\") or nil.
nil means UTC will be used (back-compat with pre-Task-15 behavior).
Set by `org-canvas--pull-resolve-tz' at the start of a pull.")

(defun org-canvas--pull-resolve-tz ()
  "Resolve the course TZ from settings.org and cache it.
Sets `org-canvas--pull-tz-cache' to the IANA TZ string or nil."
  (setq org-canvas--pull-tz-cache
        (let ((settings-file (and (boundp 'org-canvas-settings-file)
                                  org-canvas-settings-file)))
          (when (and settings-file (file-exists-p settings-file))
            (with-current-buffer (find-file-noselect settings-file)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^[ \t]*:TIME_ZONE:[ \t]+\\(.+\\)$" nil t)
                  (string-trim (match-string-no-properties 1)))))))))

(defun org-canvas--iso8601-date-p (value)
  "Return non-nil when VALUE is a string beginning with an ISO8601 date.
Used to reject malformed timestamps before parsing.  Canvas always sends
\"YYYY-MM-DD...\" so a leading date is a safe, version-independent gate."
  (and (stringp value)
       (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" value)))

(defun org-canvas--iso8601-to-org-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org active timestamp.
Localizes to `org-canvas--pull-tz-cache' (course TZ) when set, else UTC.
Returns a string like \"<2026-01-15 Thu 10:00>\" or nil."
  ;; Require an ISO date prefix before parsing: `date-to-time' is version-
  ;; inconsistent on garbage (errors on some inputs, returns a bogus epoch
  ;; date on others), so a malformed Canvas timestamp must be rejected
  ;; deterministically rather than erroring/hanging or yielding a wrong date.
  (when (org-canvas--iso8601-date-p iso8601)
    (condition-case nil
        (let ((time (date-to-time iso8601))
              (zone (or org-canvas--pull-tz-cache t)))
          (format-time-string "<%Y-%m-%d %a %H:%M>" time zone))
      (error nil))))

(defun org-canvas--iso8601-to-org-inactive-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org inactive timestamp.
Localizes to `org-canvas--pull-tz-cache' (course TZ) when set, else UTC.
Returns a string like \"[2026-01-15 Thu 10:00]\" or nil."
  (when (org-canvas--iso8601-date-p iso8601)
    (condition-case nil
        (let ((time (date-to-time iso8601))
              (zone (or org-canvas--pull-tz-cache t)))
          (format-time-string "[%Y-%m-%d %a %H:%M]" time zone))
      (error nil))))

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

(defun org-canvas--pull-known-ids (file id-property)
  "Return the ID-PROPERTY values FILE's headings already carry.
The set a scoped pull refreshes: what this course file manages, as
opposed to everything the Canvas course happens to hold."
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (delq nil (org-map-entries
                 (lambda () (org-entry-get (point) id-property))
                 nil 'file)))))

(defun org-canvas--pull-item-managed-p (item id-field known-ids)
  "Return non-nil when ITEM's ID-FIELD is one of KNOWN-IDS."
  (let ((id (alist-get id-field item)))
    (and id (member (format "%s" id) known-ids) t)))

(defun org-canvas--pull-confirm-overwrite (file feature-name)
  "Prompt user to confirm overwrite if FILE already has content.
Signals `user-error' with FEATURE-NAME if aborted.  Uses
`org-canvas--confirm', so a batch run proceeds instead of blocking on a
prompt that fires for every pull whose file already exists."
  (when (and (file-exists-p file)
             (> (file-attribute-size (file-attributes file)) 0)
             (not (org-canvas--confirm
                   (format "%s already exists.  Pull will overwrite headings.  Continue? "
                           (file-name-nondirectory file)))))
    (user-error "%s pull aborted" (capitalize feature-name))))

(defun org-canvas--pull-confirm-unsaved (file feature-name)
  "If a buffer visits FILE with unsaved change, save it or abort.
Prompt the user; on `yes' save the buffer, on `no' signal a user-error
mentioning FEATURE-NAME.  Batch runs save (see `org-canvas--confirm')."
  (let ((buf (find-buffer-visiting file)))
    (when (and buf (buffer-modified-p buf))
      (if (org-canvas--confirm
           (format "%s has unsaved changes.  Save before pulling? "
                   (file-name-nondirectory file)))
          (with-current-buffer buf (org-canvas--save-buffer))
        (user-error "%s pull aborted: unsaved changes in %s"
                    (capitalize feature-name)
                    (file-name-nondirectory file))))))

(defun org-canvas--pull-was-fresh-p (file)
  "Return non-nil if FILE neither exists on disk nor has a visiting buffer.
Captured before a pull so `org-canvas--pull-kill-fresh-buffer' knows
whether the buffer the pull will create is one the user opened."
  (and (not (file-exists-p file))
       (not (find-buffer-visiting file))))

(defun org-canvas--pull-kill-fresh-buffer (file was-fresh)
  "Kill the buffer visiting FILE iff WAS-FRESH and the buffer is unmodified.
Used at the end of a pull to avoid leaving freshly-created files open
in buffer lists."
  (when was-fresh
    (let ((buf (find-buffer-visiting file)))
      (when (and buf (not (buffer-modified-p buf)))
        (kill-buffer buf)))))

(defun org-canvas--pull-sort-cmp-numeric (a b)
  "Compare numeric keys A and B from items.
Return `lt'/`gt'/`eq' for ordering, or `none' when either is nil.
A nil key sorts AFTER any present key (so partial data falls to the end)."
  (cond
   ((and (null a) (null b)) 'eq)
   ((null a) 'gt)
   ((null b) 'lt)
   ((= a b) 'eq)
   ((< a b) 'lt)
   (t 'gt)))

(defun org-canvas--pull-sort-cmp-string (a b)
  "Compare string keys A and B from items.
Return `lt'/`gt'/`eq' for ordering.  A nil key sorts AFTER any present key."
  (cond
   ((and (null a) (null b)) 'eq)
   ((null a) 'gt)
   ((null b) 'lt)
   ((string= a b) 'eq)
   ((string< a b) 'lt)
   (t 'gt)))

(defun org-canvas--pull-sort-less-p (a b secondary-key &optional tertiary-key)
  "Return non-nil if item A should sort before item B.
Tier order: SECONDARY-KEY (numeric, optional), TERTIARY-KEY (string,
optional — used by assignments to sort by `due_at' within a group when
`org-canvas-assignment-sort' is set to `due-at'), `position',
`name'/`title', `id'.  Each tier short-circuits on a definite ordering;
ties fall through.  Used by `org-canvas--pull-sort-items'."
  (let* ((ax (cdr a)) (bx (cdr b))
         (ai (car a)) (bi (car b))
         (sec (when secondary-key
                (org-canvas--pull-sort-cmp-numeric
                 (alist-get secondary-key ax)
                 (alist-get secondary-key bx))))
         (ter (when tertiary-key
                (org-canvas--pull-sort-cmp-string
                 (alist-get tertiary-key ax)
                 (alist-get tertiary-key bx))))
         (pos (org-canvas--pull-sort-cmp-numeric
               (alist-get 'position ax)
               (alist-get 'position bx)))
         (nm (org-canvas--pull-sort-cmp-string
              (or (alist-get 'name ax) (alist-get 'title ax))
              (or (alist-get 'name bx) (alist-get 'title bx))))
         (id (org-canvas--pull-sort-cmp-numeric
              (alist-get 'id ax)
              (alist-get 'id bx))))
    (cond
     ((and sec (not (eq sec 'eq))) (eq sec 'lt))
     ((and ter (not (eq ter 'eq))) (eq ter 'lt))
     ((not (eq pos 'eq)) (eq pos 'lt))
     ((not (eq nm 'eq)) (eq nm 'lt))
     ((not (eq id 'eq)) (eq id 'lt))
     (t (< ai bi)))))

(defun org-canvas--pull-sort-items (items &optional secondary-key tertiary-key)
  "Return ITEMS sorted by SECONDARY-KEY, TERTIARY-KEY, position, name, id.
ITEMS is a list of alists from a Canvas API response.  Stable sort:
items with equal sort keys preserve input order.

Items missing the relevant key sort after items that have it.  When
SECONDARY-KEY is non-nil, that key (compared as a number) is the
PRIMARY tier — used by assignments to group by `assignment_group_id'.
TERTIARY-KEY (compared as a string) is inserted between secondary and
`position'; used by assignments to sort by `due_at' within a group."
  (let ((indexed (cl-loop for it in items
                          for i from 0
                          collect (cons i it))))
    (mapcar #'cdr
            (sort indexed
                  (lambda (a b)
                    (org-canvas--pull-sort-less-p
                     a b secondary-key tertiary-key))))))

(defun org-canvas--pull-process-item (item file pull-config)
  "Process a single pulled ITEM into FILE.
PULL-CONFIG is a plist with :id-field :title-field :id-property :pull-item-fn."
  (let* ((id-field (plist-get pull-config :id-field))
         (title-field (plist-get pull-config :title-field))
         (id-property (plist-get pull-config :id-property))
         (item-fn (plist-get pull-config :pull-item-fn))
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
  :pull-item-fn - Function (item pos) for per-item property setting (required)
  :skip-fn     - Predicate (item) to skip item when non-nil (optional)
  :id-field    - Alist key for item ID (default: \\='id)
  :title-field - Alist key for item title (default: \\='title)
  :id-property - Org property name for Canvas ID (default: \"CANVAS_ID\")
  :secondary-sort-key - Alist key used as primary tier in pull-sort
                        (e.g., \\='assignment_group_id for assignments).
  :tertiary-sort-key  - Form (evaluated at call time) yielding an alist
                        key (or nil) used as a string-compared tier
                        between secondary and `position'.  Use this to
                        thread a defcustom-driven sort mode (e.g., the
                        assignments module passes
                        `(when (eq org-canvas-assignment-sort \\='due-at)
                           \\='due_at)').

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
    :pull-item-fn #\\='org-canvas--announcement-pull-item)"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (pull-fn-name (intern (format "org-canvas-pull-%s" feature-name)))
         (file-expr (plist-get args :file))
         (endpoint-expr (plist-get args :endpoint))
         (params-expr (plist-get args :params))
         (item-fn (plist-get args :pull-item-fn))
         (skip-fn (plist-get args :skip-fn))
         (id-field (or (plist-get args :id-field) ''id))
         (title-field (or (plist-get args :title-field) ''title))
         (id-property (or (plist-get args :id-property) "CANVAS_ID"))
         (secondary-sort-key (plist-get args :secondary-sort-key))
         (tertiary-sort-key (plist-get args :tertiary-sort-key))
         (op-label (upcase (replace-regexp-in-string "-" " " feature-name))))
    (unless file-expr (error "org-canvas-define-pull: :file is required"))
    (unless endpoint-expr (error "org-canvas-define-pull: :endpoint is required"))
    (unless item-fn (error "org-canvas-define-pull: :pull-item-fn is required"))
    `(progn
       (org-canvas-register-pull-item-fn ,feature-name ,item-fn)
       ;;;###autoload
       (defun ,pull-fn-name (&optional managed-only)
         ,(format "Pull %s from Canvas into the local Org file.

With a prefix argument, MANAGED-ONLY restricts the pull to items the
Org file already claims — headings carrying a Canvas id — so a course
holding items you do not manage is refreshed rather than imported
wholesale (issue #67)." feature-name)
         (interactive "P")
         (org-canvas--start-operation ,(format "PULLING %s" op-label))
         (let* ((file (expand-file-name ,file-expr))
                (endpoint (org-canvas-api-course-endpoint ,endpoint-expr))
                (remote (org-canvas-api-request-all-pages
                         'GET endpoint ,params-expr))
                (count 0)
                (known-ids (when managed-only
                             (org-canvas--pull-known-ids file ,id-property)))
                (was-fresh (org-canvas--pull-was-fresh-p file)))
           (org-canvas--pull-confirm-overwrite file ,feature-name)
           (org-canvas--pull-confirm-unsaved file ,feature-name)
           (if (zerop (length remote))
               (org-canvas--pull-emit-empty-file
                file (org-canvas--pull-label-for ,feature-name))
             (unless (file-exists-p file)
               (with-temp-file file (insert "")))
             (with-current-buffer (find-file-noselect file)
               (dolist (item (org-canvas--pull-sort-items
                              remote ,secondary-sort-key ,tertiary-sort-key))
                 ,(let* ((body
                          `(progn
                             (org-canvas--pull-process-item
                              item file
                              (list :id-field ,id-field :title-field ,title-field
                                    :id-property ,id-property :pull-item-fn ,item-fn))
                             (cl-incf count)))
                         (body (if skip-fn
                                   `(unless (funcall ,skip-fn item) ,body)
                                 body)))
                    `(when (or (not managed-only)
                               (org-canvas--pull-item-managed-p
                                item ,id-field known-ids))
                       ,body)))
               (org-canvas--pull-write-file-header)
               (org-canvas--save-buffer)))
           (org-canvas--pull-kill-fresh-buffer file was-fresh)
           (org-canvas--log-info org-canvas--logger
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
  (org-canvas--log-info org-canvas--logger "Testing connection to %s (Course ID: %s)..."
    org-canvas-base-url org-canvas-course-id)

  (condition-case err
      (let ((name (org-canvas-get-course-name)))
	(org-canvas--log-info org-canvas--logger "Success! Connected to course: %s" name)
	(message "Success! Connected to course: %s" name))
    (error
     (let ((msg (error-message-string err)))
       (org-canvas--log-error org-canvas--logger "Connection Failed: %s" msg)
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

;;;; Pull Summary Accumulator
;;
;; Per-pull non-fatal error tracking.  Pull functions that catch a
;; per-item failure (e.g. the page-detail fetch timing out) record the
;; failure here so `org-canvas-pull-all' can print a single end-of-pull
;; summary buffer without losing the error to the log file.

(defvar org-canvas--pull-summary nil
  "Accumulator for non-fatal errors during a pull.
Each element is a plist with :file, :item, :error, :log-line.
Newest records are pushed onto the head; use
`org-canvas--pull-summary-records' to read them in insertion order.")

(defun org-canvas--pull-summary-reset ()
  "Clear the pull summary accumulator."
  (setq org-canvas--pull-summary nil))

(defun org-canvas--pull-summary-empty-p ()
  "Return non-nil when no errors have been recorded this pull."
  (null org-canvas--pull-summary))

(defun org-canvas--pull-summary-records ()
  "Return the list of recorded summary entries in insertion order."
  (reverse org-canvas--pull-summary))

(cl-defun org-canvas--pull-summary-record (&key file item error log-line)
  "Record a non-fatal pull failure.
FILE is the .org file (basename) the failure was scoped to.
ITEM is an optional identifier for the failing item (slug, id, title).
ERROR is a human-readable error message.
LOG-LINE is an optional pointer into the log buffer/file."
  (push (list :file file :item item :error error :log-line log-line)
        org-canvas--pull-summary))

(defun org-canvas--pull-summary-format-record (rec)
  "Format a single summary REC plist as a one-line string."
  (format "  %s%s: %s%s\n"
          (or (plist-get rec :file) "(unknown)")
          (if (plist-get rec :item)
              (format " [%s]" (plist-get rec :item))
            "")
          (plist-get rec :error)
          (if (plist-get rec :log-line)
              (format " (log line %d)" (plist-get rec :log-line))
            "")))

(defun org-canvas--pull-summary-current-log-line ()
  "Return the current line number in the canvas log buffer, or nil.
Useful for capturing a pointer into the running log when recording a
non-fatal pull error."
  (let ((buf (and (boundp 'org-canvas--log-buffer-name)
                  (get-buffer org-canvas--log-buffer-name))))
    (when buf
      (with-current-buffer buf
        (line-number-at-pos (point-max))))))

(defun org-canvas--pull-summary-print ()
  "Print the pull summary to standard output.
Emits nothing when the accumulator is empty so callers can wrap this
unconditionally in `with-output-to-temp-buffer'."
  (let ((records (org-canvas--pull-summary-records)))
    (when records
      (princ (format "Pull complete with %d non-fatal error%s:\n"
                     (length records)
                     (if (= (length records) 1) "" "s")))
      (dolist (rec records)
        (princ (org-canvas--pull-summary-format-record rec))))))

(defun org-canvas--preflight-check ()
  "Validate credentials and connection before syncing.
Signals error with actionable message on failure."
  (org-canvas--ensure-credentials)
  (condition-case err
      (let ((course (org-canvas-api-request 'GET
                      (org-canvas-api-course-endpoint ""))))
        (org-canvas--log-info org-canvas--logger "[Preflight] Connected to: %s"
          (alist-get 'name course)))
    (error
     (org-canvas--signal 'org-canvas-api-error
       "Connection failed: %s\nCheck your API token, course ID, and network connection"
       (error-message-string err)))))

(provide 'org-canvas-core-org)
;;; org-canvas-core-org.el ends here
