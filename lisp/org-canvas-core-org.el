;;; org-canvas-core-org.el --- Org interaction utilities for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Org-mode property access, sync state, course files in batch,
;; timestamp parsing, link-property and section resolution, and the
;; connection diagnostics.  HTML export and conversion live in
;; `org-canvas-core-html'; the pull helpers, macros and summary in
;; `org-canvas-core-pull'.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)

;;;; Org Property Access and Sync State

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

(defconst org-canvas--sync-property-names
  `("CANVAS_ID" "CANVAS_URL" "CANVAS_UPDATED_AT"
    ,org-canvas--prop-last-synced ,org-canvas--prop-payload-hash)
  "Properties managed by the sync pipeline.")

(defun org-canvas-clear-sync-properties (pom)
  "Clear all sync-related properties from entry at POM."
  (dolist (prop org-canvas--sync-property-names)
    (org-entry-delete pom prop)))

;;;; Course Files in Batch: Freshness and Saving

(defun org-canvas--save-buffer ()
  "Save the current buffer when modified and log the file path written.
Use in place of `save-buffer' so the sync log records each modified
Org file.  No-op when the buffer has no unsaved changes: completion-time
safety saves after per-item saves would otherwise log duplicate
\[Saved] lines for writes that never happened.  When the current buffer
has no associated file (e.g., a scratch buffer used for HTML export)
the save still happens but no log line is emitted."
  (org-canvas--ensure-buffer-fresh)
  (when (buffer-modified-p)
    (save-buffer)
    (org-canvas--note-saved-content)
    (when buffer-file-name
      (org-canvas--log-info org-canvas--logger "[Saved] %s" buffer-file-name))))

(defun org-canvas--clean-local-sync-properties (file &optional id-property)
  "Remove sync properties from all headings with IDs in FILE.
ID-PROPERTY defaults to \"CANVAS_ID\"."
  (let ((id-prop (or id-property "CANVAS_ID")))
    (when (and file (file-exists-p file))
      (org-canvas--log-info org-canvas--logger "Cleaning local properties...")
      (with-current-buffer (org-canvas--find-file-noselect file)
        (org-map-entries
         (lambda ()
           (org-canvas--log-debug org-canvas--logger "Removing properties for: %s"
                       (org-entry-get (point) id-prop))
           (org-canvas-clear-sync-properties (point)))
         (format "%s={.}" id-prop) 'file)
        (org-canvas--save-buffer)))))

(defvar-local org-canvas--content-hashes nil
  "Hashes of this buffer's text as read from or written to its file.
Newest first.  `org-canvas--find-file-noselect' and
`org-canvas--save-buffer' add to it; `org-canvas--ensure-buffer-fresh'
reads it to tell a file whose modification time moved but whose text
did not — a sync client's restamp after this buffer's own save (issue
#188) — and a file a sync client rolled back to an earlier state of
this same buffer (issue #249) from a file another writer rewrote.  Nil
until the buffer has been read or saved through org-canvas.")

(defun org-canvas--note-saved-content ()
  "Record the current buffer's text as the content its file now has."
  (let ((hash (buffer-hash)))
    (unless (equal hash (car org-canvas--content-hashes))
      (push hash org-canvas--content-hashes))))

(defun org-canvas--disk-content-hash ()
  "Return the hash of the visited file's text on disk, or nil if unreadable."
  (let ((file buffer-file-name))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-hash)))))

(defun org-canvas--file-restamped-p ()
  "Return non-nil when the visited file's text is what this buffer last saw.
A true answer means the modification time moved without the content
changing, so the buffer is not being clobbered and may keep its edits."
  (and org-canvas--content-hashes
       (equal (car org-canvas--content-hashes)
              (org-canvas--disk-content-hash))))

(defun org-canvas--file-rolled-back-p ()
  "Return non-nil when the visited file holds an earlier state of this buffer.
The text on disk is not this buffer's last save but one of its earlier
saves, or the text it first read: a sync client wrote an older copy of
the file back over a newer one (issue #249).  Nothing on disk is news
to the buffer, so the buffer is the newer side and may keep its edits."
  (let ((disk (org-canvas--disk-content-hash)))
    (and disk
         (member disk (cdr org-canvas--content-hashes))
         t)))

(defun org-canvas--restore-rolled-back-file ()
  "Write the current buffer back over a file a sync client rolled back.
The buffer is the newer side (`org-canvas--file-rolled-back-p'), so
disk catches up with it: the visited time is refreshed first, since
`save-buffer' would otherwise ask the batch-fatal \"changed since
visited\" question, and the write is forced even for an unmodified
buffer, since its last save is exactly what the client undid."
  (set-visited-file-modtime)
  (set-buffer-modified-p t)
  (save-buffer)
  (org-canvas--note-saved-content)
  (org-canvas--log-info org-canvas--logger "[Saved] %s" buffer-file-name))

(defun org-canvas--ensure-buffer-fresh ()
  "Under `noninteractive', reconcile the current buffer with its file.
A course file rewritten on disk behind a buffer trips Emacs's
\"changed on disk; really edit the buffer?\" protection, and a batch
Emacs cannot answer it — the run dies reading input, after the API
call already landed (issue #97).  A file whose modification time moved
but whose text is what this buffer last read or wrote was merely
restamped by a sync client after the buffer's own save; the buffer,
edits and all, is kept and the recorded time refreshed (issue #188).
A file holding an earlier state of this buffer was rolled back by a
sync client that wrote an older copy over a newer save; the buffer is
kept and written again so disk catches up, since rereading would drop
stamps whose API calls already landed (issue #249).  Otherwise an
unmodified stale buffer is reread — text this buffer never saw is on
disk, which mid-run is never routine, so the reread is a warning and a
batch message.  A modified stale buffer is a dual-buffer clobber in
progress, so this signals a clear error instead of letting the
unanswerable prompt kill the run.  Interactive sessions keep Emacs's
own protection and are left alone."
  (when (and noninteractive
             buffer-file-name
             (not (verify-visited-file-modtime (current-buffer))))
    (let ((name (file-name-nondirectory buffer-file-name)))
      (cond
       ((org-canvas--file-restamped-p)
        (org-canvas--log-debug org-canvas--logger
          "[Fresh] %s was restamped on disk without changing; keeping the buffer (issue #188)"
          name)
        (set-visited-file-modtime))
       ((org-canvas--file-rolled-back-p)
        (org-canvas--log-warning org-canvas--logger
          "[Fresh] %s was rolled back on disk to an earlier state of this buffer, most likely by a file-sync client; keeping the buffer and saving it again (issue #249)"
          name)
        (message "Warning: %s was rolled back on disk during the run; saving the buffer again" name)
        (org-canvas--restore-rolled-back-file))
       ((buffer-modified-p)
        (error "%s changed on disk while this buffer holds unsaved edits — refusing to write over either (issue #97: two buffers for one file, usually a symlinked org-canvas-directory)"
               name))
       (t
        (org-canvas--log-warning org-canvas--logger
          "[Fresh] %s changed on disk behind this buffer; rereading it before writing"
          name)
        (message "Warning: %s changed on disk during the run; rereading it" name)
        (revert-buffer t t t)
        (org-canvas--note-saved-content))))))

(defun org-canvas--find-file-noselect (file)
  "Visit FILE and return its buffer, without the batch supersession prompt.
`find-file-noselect' asks \"File ... changed on disk.  Reread from
disk?\" when a file it already visits was rewritten behind the buffer,
and a file-sync client that restamps mtimes (OneDrive, Dropbox) provokes
exactly that between one entry's save and the next entry's link
resolution.  A batch Emacs cannot answer: the question reads stdin, gets
EOF, and the run dies with some entries pushed and the rest not (issue
#121).  Under `noninteractive' the question is therefore suppressed and
`org-canvas--ensure-buffer-fresh' decides instead — an unmodified stale
buffer is reread, a modified one signals a clear error.  Interactive
sessions keep Emacs's own prompt."
  (let ((buffer (find-file-noselect file noninteractive)))
    (with-current-buffer buffer
      (org-canvas--ensure-buffer-fresh)
      (unless (buffer-modified-p)
        (org-canvas--note-saved-content)))
    buffer))

(defun org-canvas-org-set-property (pom property value)
  "Set Org PROPERTY to VALUE at POM (point or marker).
Ensures the correct buffer is used if POM is a marker.  Reverts an
unmodified buffer whose file changed on disk before writing, and
refuses to write over a modified one (`org-canvas--ensure-buffer-fresh',
issue #97).

Binds `org-property-format' to \"%s %s\" so property names shorter than
10 characters are not padded with extra spaces (e.g. `:LICENSE:  private'
with two spaces).  We always emit a single space between the property
name and value for consistent diffs and easier scripting."
  (let ((buf (if (markerp pom) (marker-buffer pom) (current-buffer))))
    (with-current-buffer buf
      (org-canvas--ensure-buffer-fresh)
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

;;;; Timestamps and Time Zone

(defun org-canvas--org-timestamp-date (ts)
  "Return the YYYY-MM-DD written in Org timestamp TS, or nil."
  (when (and (stringp ts)
             (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" ts))
    (match-string 0 ts)))

(defun org-canvas--org-timestamps-span-days-p (start end)
  "Return non-nil when Org timestamps START and END fall on different dates.
Dates are compared as written — the author's local dates — because a
single-day event expressed in UTC can cross midnight and would look
like a span (issue #93)."
  (let ((a (org-canvas--org-timestamp-date start))
        (b (org-canvas--org-timestamp-date end)))
    (and a b (not (string= a b)))))

(defun org-canvas-org-parse-timestamp (ts-string)
  "Transform an Org timestamp TS-STRING into a Canvas ISO8601 string.
The timestamp is read in `org-canvas--time-zone', the same zone the
pull side writes in, so a deadline survives a round trip (issue #136).
A timestamp that names no time is midnight in that zone."
  (when ts-string
    (let ((decoded (org-parse-time-string ts-string))
          (zone (org-canvas--time-zone)))
      (when zone
        (setf (nth 8 decoded) zone))
      (format-time-string "%Y-%m-%dT%H:%M:%SZ" (encode-time decoded) t))))

(defun org-canvas-current-iso8601-timestamp ()
  "Return the current time as a Canvas ISO8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun org-canvas--pull-resolve-tz ()
  "Resolve the course time zone from settings.org and cache it.
Sets `org-canvas--pull-tz-cache' to the TIME_ZONE string settings.org
carries, or nil when it has none, and marks the zone resolved.  Runs
lazily from `org-canvas--time-zone'; the settings pull calls it
directly after writing a fresh TIME_ZONE."
  (setq org-canvas--time-zone-resolved t
        org-canvas--pull-tz-cache
        (let ((settings-file (and (boundp 'org-canvas-settings-file)
                                  org-canvas-settings-file)))
          (when (and settings-file (file-exists-p settings-file))
            (with-current-buffer (org-canvas--find-file-noselect settings-file)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^[ \t]*:TIME_ZONE:[ \t]+\\(.+\\)$" nil t)
                  (string-trim (match-string-no-properties 1)))))))))

(defun org-canvas--time-zone ()
  "Return the zone Org timestamps are read and written in, or nil for local.
`org-canvas-time-zone' when the user pinned one; otherwise the course
zone from settings.org, resolved once per operation (a zone already in
`org-canvas--pull-tz-cache' is taken as resolved, so a test can bind
it).  Nil means Emacs's local zone.  Both `org-canvas-org-parse-timestamp'
and the ISO-to-Org converters read this and nothing else, which is
what keeps push and pull agreeing (issue #136)."
  (or org-canvas-time-zone
      (progn
        (unless (or org-canvas--pull-tz-cache org-canvas--time-zone-resolved)
          (org-canvas--pull-resolve-tz))
        org-canvas--pull-tz-cache)))

(defun org-canvas--iso8601-date-p (value)
  "Return non-nil when VALUE is a string beginning with an ISO8601 date.
Used to reject malformed timestamps before parsing.  Canvas always sends
\"YYYY-MM-DD...\" so a leading date is a safe, version-independent gate."
  (and (stringp value)
       (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" value)))

(defun org-canvas--iso8601-to-org-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org active timestamp.
Written in `org-canvas--time-zone', the zone push reads timestamps in.
Returns a string like \"<2026-01-15 Thu 10:00>\" or nil."
  ;; Require an ISO date prefix before parsing: `date-to-time' is version-
  ;; inconsistent on garbage (errors on some inputs, returns a bogus epoch
  ;; date on others), so a malformed Canvas timestamp must be rejected
  ;; deterministically rather than erroring/hanging or yielding a wrong date.
  (when (org-canvas--iso8601-date-p iso8601)
    (condition-case nil
        (let ((time (date-to-time iso8601))
              (zone (org-canvas--time-zone)))
          (format-time-string "<%Y-%m-%d %a %H:%M>" time zone))
      (error nil))))

(defun org-canvas--iso8601-to-org-inactive-timestamp (iso8601)
  "Convert ISO8601 timestamp to Org inactive timestamp.
Written in `org-canvas--time-zone', the zone push reads timestamps in.
Returns a string like \"[2026-01-15 Thu 10:00]\" or nil."
  (when (org-canvas--iso8601-date-p iso8601)
    (condition-case nil
        (let ((time (date-to-time iso8601))
              (zone (org-canvas--time-zone)))
          (format-time-string "[%Y-%m-%d %a %H:%M]" time zone))
      (error nil))))

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
    (with-current-buffer (org-canvas--find-file-noselect file)
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

;;;; Shared Constants and Digests

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

;;;; Org Link Property Resolution

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
            (with-current-buffer (org-canvas--find-file-noselect abs-file)
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

;;;; Section Name -> ID Resolution

(defun org-canvas--find-section-id-by-name (name sections-file)
  "Look up section NAME in SECTIONS-FILE and return its CANVAS_ID, or nil."
  (with-current-buffer (org-canvas--find-file-noselect sections-file)
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

;;;; Heading Lookup Across Files

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
    (with-current-buffer (org-canvas--find-file-noselect abs-file)
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

(defun org-canvas--strip-named-tables (text names)
  "Return TEXT without the `#+NAME:' tables named in NAMES.
Each is the `#+NAME: <name>' line and the table rows after it: the
overrides and accommodations tables feed a sync and are not body
text.  TEXT without any such table is returned unchanged."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward
            (format "^#\\+NAME:[ \t]+\\(%s\\)[ \t]*\n" (regexp-opt names)) nil t)
      (let ((start (match-beginning 0)))
        (while (looking-at "^|")
          (forward-line 1))
        (delete-region start (point))))
    (buffer-string)))

(defun org-canvas--heading-property-by-title (file title property &optional match)
  "Return PROPERTY of the first heading titled TITLE in FILE, or nil.
MATCH is an `org-map-entries' match string limiting the search
\(\"LEVEL=2\", say); nil searches every heading.  Nil when FILE is nil
or missing, so a caller may pass a file variable guarded only by
`boundp'.  The inverse of `org-canvas--heading-title-by-property'."
  (when (and file (file-exists-p file))
    (let ((found nil))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (when (and (not found) (equal (org-get-heading t t t t) title))
               (setq found (or (org-entry-get (point) property) 'missing))))
           match 'file)))
      (and (stringp found) found))))

(defun org-canvas--heading-title-by-property (file property value &optional match)
  "Return the title of the first heading in FILE whose PROPERTY is VALUE, or nil.
VALUE is compared as a string, so a number matches its digits.  MATCH
limits the search as in `org-canvas--heading-property-by-title'; nil when
FILE is nil or missing."
  (when (and file (file-exists-p file))
    (let ((target (format "%s" value)) (title nil))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (when (and (not title) (equal (org-entry-get (point) property) target))
               (setq title (org-get-heading t t t t))))
           match 'file)))
      title)))

;;;; Diagnostics and Report Output

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
         (org-canvas--user-message "Connection failed: %s" msg)))))))

(defun org-canvas--report-display (buffer-name render-fn &optional mode-fn)
  "Render a report into BUFFER-NAME and put it where the caller can read it.
RENDER-FN is called with no arguments and the report buffer current
and empty; it inserts the report.  MODE-FN, when given, is called
afterwards to set the buffer's major mode.

Interactively the buffer is displayed.  Under `noninteractive' it is
printed to standard output instead: `display-buffer' and
`with-output-to-temp-buffer' both leave a batch Emacs with nothing to
look at, which is how a scripted pull lost its whole summary (issue
#155) and a scripted validation its whole report, leaving only a tally
naming no file (issue #169).  A report-producing command reaches for
this rather than rediscovering that.

Returns the report text."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (funcall render-fn))
    (when mode-fn (funcall mode-fn))
    (goto-char (point-min))
    (let ((text (buffer-string)))
      (if noninteractive
          (princ text)
        (display-buffer (current-buffer)))
      text)))

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
