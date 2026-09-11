;;; org-canvas-core-pull.el --- Pull helpers, macros and summary for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Canvas -> Org: the file header and property writers a pull uses, the
;; rewriter that turns Canvas file URLs in a pulled body into local
;; links (downloading and registering the file), heading upsert and
;; body insertion, the registry-driven pull-item path,
;; `org-canvas-define-pull-item', `org-canvas-define-pull', and the
;; pull summary accumulator.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)
(require 'org-canvas-core-html)

;; Forward declaration: defined in `org-canvas-files', loaded by the time
;; the rewriter runs during a pull.  Declared here to keep core free of
;; a feature-module dependency while still satisfying byte-compile.
(declare-function org-canvas--file-pull-download "org-canvas-files"
                  (display-name download-url local-path size))

;;;; File Header and Pulled Properties

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

;;;; Canvas File URL Rewriting

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
      (with-current-buffer (org-canvas--find-file-noselect files-file)
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
    (with-current-buffer (org-canvas--find-file-noselect files-file)
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
On a failed request (403/404/timeout, and the rest of
`org-canvas-api-error') record to the pull summary and return nil so
the rewriter passes the URL through unchanged: one unreadable file —
a cross-course link, a locked folder — costs that one link, not the
content type (issue #171).  A 401 is deliberately not among them: an
expired token will fail every remaining request too, so it aborts
rather than filling the summary with one entry per item.

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

;;;; Heading Upsert and Body Insertion

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
        (buf (org-canvas--find-file-noselect (expand-file-name file))))
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

;;;; Children of a Pulled Entry

(defun org-canvas--pull-find-child (id-property id title)
  "Return the position of the direct child of the entry at point for ID or TITLE.
A child is a heading one level below the entry.  A child carrying ID
in ID-PROPERTY wins; failing that, a child without ID-PROPERTY whose
title is TITLE, so a heading written before the pull stamped ids, or
by hand under the same name, is taken over rather than duplicated
\(issue #239).  Nil when there is none.  Point stays."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (child-level (1+ (org-outline-level)))
          (target (and id (format "%s" id)))
          (by-id nil) (by-title nil))
      (while (and (not by-id) (outline-next-heading) (< (point) end))
        (when (= (org-outline-level) child-level)
          (let ((have (org-entry-get (point) id-property)))
            (cond ((and target have (equal have target)) (setq by-id (point)))
                  ((and (not have) (not by-title) title
                        (equal (org-get-heading t t t t) title))
                   (setq by-title (point)))))))
      (or by-id by-title))))

(defun org-canvas--pull-remove-child (pos)
  "Delete the subtree of the child heading at POS and return POS.
The pull writes the fresh child there, so the entry keeps its place
among its siblings.  What follows the child starts at the beginning
of a line afterwards, as it did before."
  (save-excursion
    (goto-char pos)
    (org-back-to-heading t)
    (let ((beg (point))
          (end (save-excursion (org-end-of-subtree t t) (point))))
      (delete-region beg end)
      beg)))

(defun org-canvas--pull-child-insert-point (id-property id title)
  "Return where the pull writes the child ID or TITLE of the entry at point.
ID is looked up in ID-PROPERTY; TITLE matches an unstamped child.  The
position of the existing child, emptied, when
`org-canvas--pull-find-child' finds one; else the end of the entry's
subtree, with a newline added so the child starts on its own line.
Point is left there.  The entry may sit at any level; the child goes
one level below it (`org-canvas--pull-child-stars' spells the stars)."
  (let ((existing (org-canvas--pull-find-child id-property id title)))
    (if existing
        (goto-char (org-canvas--pull-remove-child existing))
      (goto-char (save-excursion (org-end-of-subtree t) (point)))
      (unless (bolp) (insert "\n")))
    (point)))

(defun org-canvas--pull-child-stars ()
  "Return the stars of a heading one level below the entry at point."
  (make-string (1+ (save-excursion (org-back-to-heading t) (org-outline-level))) ?*))

(defun org-canvas--pull-child-close (next)
  "Keep whatever followed a rewritten child at NEXT on its own line.
NEXT is a marker at the text that followed the child's old subtree; a
child written in place ends where it ends, so a newline is added when
the following heading would otherwise share its line."
  (when (and next (< (marker-position next) (point-max)))
    (save-excursion
      (goto-char next)
      (unless (bolp) (insert "\n")))))

;;;; Titled Headings

(defun org-canvas--pull-heading-by-title (title)
  "Return the position of the level-1 heading TITLE, creating it if absent.
For files organised by fixed headings rather than by Canvas id — the
roster's roles, the gradebook's tables.  A new heading is appended
after the last line of the buffer, so headings created in order stay
in order."
  (let ((pos nil))
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when (and (not pos) (equal (org-get-heading t t t t) title))
           (setq pos (point))))
       "LEVEL=1" 'file))
    (unless pos
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (unless (bobp) (insert "\n\n"))
      (insert (format "* %s\n" title))
      (forward-line -1)
      (org-back-to-heading t)
      (setq pos (point)))
    pos))

;;;; Registry-Driven Pull

;;
;; The property registry is where a module says which Org properties it
;; owns, how each is typed, and which Canvas field holds it.  Push parse,
;; validation, the drift report and the payload builder all read it.
;; Pull did not: the same mapping was spelled a second time in each
;; module's pull spec, or written out by hand, and the copies drifted —
;; the assignment pull wrote SUBMISSION_TYPES and never PUBLISHED, the
;; discussion pull wrote DELAYED_POST_AT where push reads AVAILABLE_FROM
;; (issue #134).  Everything below reads the registry, and the drift
;; report's remote-field readers live here too so both sides agree on
;; which Canvas field a property means (issue #135).

(defun org-canvas--registry-normalize-remote (value)
  "Return VALUE with Canvas's JSON null and false spellings folded to nil."
  (if (memq value '(:json-false :null)) nil value))

(defun org-canvas--registry-remote-list (remote)
  "Return REMOTE as a list of strings, however Canvas spelled it.
A `csv-enum' field arrives either as a JSON array or as one comma
separated string — pages return `editing_roles' as \"teachers\" —
and `append' on a string yields character codes, which is how
\"teachers\" came to be reported as 116,101,97,... (issue #63)."
  (cond ((null remote) nil)
        ((stringp remote) (split-string remote "," t "[ \t]+"))
        (t (mapcar (lambda (v) (format "%s" v)) (append remote nil)))))

(defun org-canvas--registry-remote-key (spec)
  "Return the Canvas field for SPEC when it names no `:remote-fn'.
The spec's `:api-key' when it declares one, and failing that the
`:data-key', which every module already names after the Canvas field."
  (intern (or (plist-get spec :api-key)
              (substring (symbol-name (plist-get spec :data-key)) 1))))

(defun org-canvas--registry-remote-field (spec item)
  "Return ITEM's value for the Canvas field described by SPEC.
A spec may name a `:remote-fn' of one argument, the Canvas item, for a
property the payload does not hold under a flat key of its own: a
file's publish state lives in `locked' (issue #61) and a group's drop
rules under `rules' (issue #62), so a flat lookup silently returned
nil and reported every such item as drifted."
  (let ((remote-fn (plist-get spec :remote-fn)))
    (if remote-fn
        (funcall remote-fn item)
      (alist-get (org-canvas--registry-remote-key spec) item))))

(defun org-canvas--registry-remote-present-p (spec item)
  "Return non-nil when ITEM carries the field named by SPEC.
A `:remote-fn' answers for its field, unless the spec's
`:remote-known-p' says it cannot for this ITEM (issue #216); a flat
key must be in ITEM.  A conflict-check GET can return a partial
object, and a field Canvas did not send is no reason to touch the
local property."
  (let ((known-p (plist-get spec :remote-known-p)))
    (or (and (plist-get spec :remote-fn)
             (or (null known-p) (funcall known-p item))
             t)
        (and (assq (org-canvas--registry-remote-key spec) item) t))))

(defvar org-canvas--pull-link-index-cache nil
  "Alist of (FILE ID-PROPERTY TICK) to an id-to-heading hash.
Built by `org-canvas--pull-link-index'; TICK is the visiting buffer's
`buffer-chars-modified-tick', so an edit to the target file invalidates
its entry without anyone having to remember to clear it.")

(defun org-canvas--pull-link-index (file id-property)
  "Return a hash of ID-PROPERTY value to heading text for FILE's headings."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (let* ((key (list file id-property (buffer-chars-modified-tick)))
           (hit (assoc key org-canvas--pull-link-index-cache)))
      (or (cdr hit)
          (let ((index (make-hash-table :test 'equal)))
            (save-excursion
              (goto-char (point-min))
              (org-map-entries
               (lambda ()
                 (let ((id (org-entry-get (point) id-property)))
                   (when id
                     (puthash id (org-get-heading t t t t) index))))
               nil 'file))
            (setq org-canvas--pull-link-index-cache
                  (cons (cons key index)
                        (cl-remove-if
                         (lambda (cell)
                           (and (equal (nth 0 (car cell)) file)
                                (equal (nth 1 (car cell)) id-property)))
                         org-canvas--pull-link-index-cache)))
            index)))))

(defun org-canvas--pull-resolve-link (spec id)
  "Return an Org link to the heading in SPEC's target file with Canvas ID.
SPEC is a `link' property spec naming a `:target-file' variable and,
optionally, the `:link-id-property' the target headings are keyed by
\(default CANVAS_ID).  The inverse of `org-canvas--resolve-link-property',
which push uses to turn the link back into the id.  Nil when the file
or the heading is not there."
  (let* ((target-var (plist-get spec :target-file))
         (file (and target-var (boundp target-var) (symbol-value target-var)))
         (id-prop (or (plist-get spec :link-id-property) "CANVAS_ID")))
    (when (and id file (file-exists-p file))
      (let ((heading (gethash (format "%s" id)
                              (org-canvas--pull-link-index file id-prop))))
        (when heading
          (org-link-make-string
           (format "file:%s::*%s" (file-name-nondirectory file) heading)
           heading))))))

(defun org-canvas--registry-remote-as-org (spec value)
  "Return VALUE spelled as an Org property string of SPEC's type.
Nil means the value has no Org spelling: Canvas holds nothing, or a
link's target heading does not exist locally."
  (let ((value (org-canvas--registry-normalize-remote value)))
    (pcase (plist-get spec :type)
      ('boolean (if value "true" "false"))
      ('timestamp (org-canvas--iso8601-to-org-timestamp value))
      ('csv-enum (let ((parts (org-canvas--registry-remote-list value)))
                   (and parts (mapconcat #'identity parts ","))))
      ('link (org-canvas--pull-resolve-link spec value))
      (_ (and value (not (equal value ""))
              (format "%s" value))))))

(defun org-canvas--registry-value-default-p (spec value)
  "Return non-nil when VALUE is what SPEC's property means when absent.
A terse drawer leaves such values implicit: the registered `:default'
for a boolean (nil when none is declared), zero for a number, and
nothing at all for the rest."
  (let ((value (org-canvas--registry-normalize-remote value))
        (default (plist-get spec :default)))
    (pcase (plist-get spec :type)
      ('boolean (eq (and value t) (and default t)))
      ('number (or (null value) (equal value default)
                   (and (numberp value) (zerop value))))
      (_ (or (null value) (equal value "") (equal value default)
             (and (vectorp value) (zerop (length value))))))))

(defun org-canvas--pull-apply-spec (spec item pos)
  "Write the property SPEC describes at POS from the Canvas ITEM.
A field ITEM does not carry leaves the property untouched.  A value
that is the property's default is deleted so the drawer stays terse
\(written out when `org-canvas-emit-defaults' is set).  Anything else
is written in its Org spelling; a value with no Org spelling — a link
whose target heading is missing — is logged and left alone rather than
erased.  A `:local-only' spec is org-canvas's own bookkeeping, never
Canvas's opinion, and is skipped."
  (unless (plist-get spec :local-only)
    (when (org-canvas--registry-remote-present-p spec item)
      (let* ((org-prop (plist-get spec :org-prop))
             (value (org-canvas--registry-remote-field spec item))
             (text (org-canvas--registry-remote-as-org spec value)))
        (cond
         ((org-canvas--registry-value-default-p spec value)
          (if (and org-canvas-emit-defaults text)
              (org-canvas-org-set-property pos org-prop text)
            (org-entry-delete pos org-prop)))
         (text
          (org-canvas-org-set-property pos org-prop text))
         (t
          (org-canvas--log-warning org-canvas--logger
            "[Pull] %s: no local spelling for Canvas value %S; property left unchanged"
            org-prop value)))))))

(defun org-canvas--pull-item-from-registry (registry-key item pos)
  "Write every property the registry lists under REGISTRY-KEY at POS from ITEM.
REGISTRY-KEY is the string the module passed to
`org-canvas-register-properties'.  This is the whole of a pull for the
properties a module owns; a module adds only what the registry cannot
express — a body, a child table — through `:after-pull'."
  (let ((entry (gethash registry-key org-canvas--property-registry)))
    (unless entry
      (error "No property registry entry named %S" registry-key))
    (dolist (spec (plist-get entry :properties))
      (org-canvas--pull-apply-spec spec item pos))))

(defun org-canvas--pull-item-set-property (pos api-field property
                                               type item)
  "Set PROPERTY on heading at POS from ITEM's API-FIELD.
TYPE controls conversion: string, boolean, timestamp, number.  The
explicit-spec path of `org-canvas-define-pull-item', for a property a
module keeps outside the registry."
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
  :registry-key - String naming the module's `org-canvas-register-properties'
                  entry; every property it lists is written from the
                  item through `org-canvas--pull-item-from-registry'
  :body-field   - API alist key for body HTML (optional)
  :after-pull   - Function (item pos) for custom logic (optional)
  :properties   - List of (API-FIELD ORG-PROPERTY :type TYPE) specs for
                  properties kept outside the registry (optional)

Type can be: string (default), boolean, timestamp, number, non-null.

Example:
  (org-canvas-define-pull-item discussion
    :registry-key \"discussions\"
    :body-field \\='message)"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas--%s-pull-item"
                                  feature-name)))
         (registry-key (plist-get args :registry-key))
         (body-field (plist-get args :body-field))
         (after-pull (plist-get args :after-pull))
         (properties (plist-get args :properties)))
    `(defun ,fn-name (item pos)
       ,(format "Set per-item properties for a pulled %s.\n\
ITEM is the API response alist, POS is the heading position."
                feature-name)
       ,@(when registry-key
           `((org-canvas--pull-item-from-registry ,registry-key item pos)))
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
           `((org-with-point-at pos
               (org-canvas--pull-insert-body
                (alist-get ',body-field item)))))
       ,@(when after-pull
           `((funcall ,after-pull item pos))))))

;;;; Pull Macro

(defun org-canvas--pull-known-ids (file id-property)
  "Return the ID-PROPERTY values FILE's headings already carry.
The set a scoped pull refreshes: what this course file manages, as
opposed to everything the Canvas course happens to hold."
  (when (file-exists-p file)
    (with-current-buffer (org-canvas--find-file-noselect file)
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

(defun org-canvas--pull-item-label (item id-field title-field)
  "Return a display label for ITEM using TITLE-FIELD, then ID-FIELD."
  (format "%s" (or (alist-get title-field item)
                   (alist-get id-field item)
                   "(unnamed)")))

(defun org-canvas--pull-record-skip (file item id-field title-field reason)
  "Log ITEM as skipped by a pull `:skip-fn' and record it in the summary.
FILE is the .org file the pull writes, ID-FIELD and TITLE-FIELD name
the item alist keys used for the label, REASON is the module's
`:skip-reason' string or nil.  Without this a skipped item leaves no
trace anywhere: the completion count reports what was written, which
reads as what was available (issue #81)."
  (let ((label (org-canvas--pull-item-label item id-field title-field)))
    (org-canvas--log-info org-canvas--logger
      "[Pull] Skipped '%s'%s" label
      (if reason (format ": %s" reason) ""))
    (org-canvas--pull-summary-record
     :kind 'skip
     :file (file-name-nondirectory file)
     :item label
     :error (or reason "excluded by this module's skip rule"))))

(defun org-canvas--pull-skip-suffix (skipped reason)
  "Return the \" (N skipped: REASON)\" tail of a pull completion line.
Empty when SKIPPED is zero."
  (if (zerop skipped)
      ""
    (format " (%d skipped%s)" skipped (if reason (format ": %s" reason) ""))))

(defun org-canvas--pull-handle-item (item file config managed-only known-ids)
  "Process, skip, or pass over ITEM for a generated pull writing FILE.
CONFIG is the pull plist (:id-field :title-field :id-property
:pull-item-fn :skip-fn :skip-reason).  MANAGED-ONLY and KNOWN-IDS come
from the prefix argument (issue #67).  Returns `processed' when the
item was written, `skipped' when a module's `:skip-fn' held it
back, and nil when MANAGED-ONLY excluded it.

This runs at pull time rather than being spliced into the macro so the
generated loop stays a two-branch dispatch."
  (let ((skip-fn (plist-get config :skip-fn))
        (id-field (plist-get config :id-field)))
    (cond
     ((and managed-only
           (not (org-canvas--pull-item-managed-p item id-field known-ids)))
      nil)
     ((and skip-fn (funcall skip-fn item))
      (org-canvas--pull-record-skip file item id-field
                                    (plist-get config :title-field)
                                    (plist-get config :skip-reason))
      'skipped)
     (t
      (org-canvas--pull-process-item item file config)
      'processed))))

(defun org-canvas--pull-idless-entry-count (id-property)
  "Count the level-1 entries of the current buffer carrying no ID-PROPERTY."
  (let ((n 0))
    (org-map-entries
     (lambda () (unless (org-entry-get (point) id-property) (cl-incf n)))
     "LEVEL=1" 'file)
    n))

(defun org-canvas--pull-check-entry-count (label file id-property
                                                 idless-before written)
  "Warn when a pull left FILE with level-1 entries it did not mean to write.
LABEL names the feature.  IDLESS-BEFORE is what
`org-canvas--pull-idless-entry-count' returned before the items were
written and WRITTEN how many entries the pull wrote or matched.  A
pull stamps ID-PROPERTY on every entry it touches, so a level-1 entry
without one that was not there before came from a body that split
its item (issue #175).  The warning goes to the log and the pull
summary; nothing prompts, so a batch pull runs on.  Counting is one
pass over the level-1 headings of the current buffer."
  (let* ((idless (org-canvas--pull-idless-entry-count id-property))
         (extra (- idless idless-before)))
    (when (> extra 0)
      (let ((msg (format "%d level-1 entr%s without %s appeared while \
writing %d %s: a body heading split an entry?  Check the file before pushing"
                         extra (if (= extra 1) "y" "ies") id-property
                         written label)))
        (org-canvas--log-warning org-canvas--logger "[Pull] %s: %s"
          (file-name-nondirectory file) msg)
        (org-canvas--pull-summary-record
         :file (file-name-nondirectory file) :error msg
         :log-line (org-canvas--pull-summary-current-log-line))))))

(defmacro org-canvas-define-pull (feature &rest args)
  "Define `org-canvas-pull-FEATURE' function.
FEATURE is a symbol like \\='pages or \\='announcements.

ARGS is a plist with the following keys:
  :file        - Symbol for file path defcustom (required)
  :endpoint    - API endpoint suffix string (required)
  :params      - Extra GET params alist (optional)
  :pull-item-fn - Function (item pos) for per-item property setting (required)
  :skip-fn     - Predicate (item) to skip item when non-nil (optional)
  :skip-reason - Short phrase naming why `:skip-fn' skips, reported in
                 the completion line and the pull summary (optional)
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
  5. Warns when a level-1 entry without the id appeared (issue #175)
  6. Saves the buffer and logs completion

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
         (skip-reason (plist-get args :skip-reason))
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
                (skipped 0)
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
             (with-current-buffer (org-canvas--find-file-noselect file)
               (let ((idless-before
                      (org-canvas--pull-idless-entry-count ,id-property)))
                 (dolist (item (org-canvas--pull-sort-items
                                remote ,secondary-sort-key ,tertiary-sort-key))
                   (pcase (org-canvas--pull-handle-item
                           item file
                           (list :id-field ,id-field :title-field ,title-field
                                 :id-property ,id-property :pull-item-fn ,item-fn
                                 :skip-fn ,skip-fn :skip-reason ,skip-reason)
                           managed-only known-ids)
                     ('processed (cl-incf count))
                     ('skipped (cl-incf skipped))))
                 (org-canvas--pull-check-entry-count
                  ,feature-name file ,id-property idless-before count))
               (org-canvas--pull-write-file-header)
               (org-canvas--save-buffer)))
           (org-canvas--pull-kill-fresh-buffer file was-fresh)
           (let ((skip-note (org-canvas--pull-skip-suffix skipped ,skip-reason)))
             (org-canvas--log-info org-canvas--logger
               ,(format "%s pull complete: %%d items%%s"
                        (capitalize feature-name)) count skip-note)
             (message ,(format "%s pull complete: %%d items%%s."
                               (capitalize feature-name)) count skip-note)))))))

;;;; Pull Summary Accumulator

;;
;; Per-pull non-fatal error tracking.  Pull functions that catch a
;; per-item failure (e.g. the page-detail fetch timing out) record the
;; failure here so `org-canvas-pull-all' can print a single end-of-pull
;; summary buffer without losing the error to the log file.

(defvar org-canvas--pull-summary nil
  "Accumulator for non-fatal errors and skips during a pull.
Each element is a plist with :kind, :file, :item, :error, :log-line.
:kind is `error' for a failure that lost content and `skip' for an
item a module's `:skip-fn' deliberately left out of the local file
\(issue #81 — a skip with no record is indistinguishable from an item
that was never on Canvas).  Newest records are pushed onto the head;
use `org-canvas--pull-summary-records' to read them in insertion order.")

(defun org-canvas--pull-summary-reset ()
  "Clear the pull summary accumulator."
  (setq org-canvas--pull-summary nil))

(defun org-canvas--pull-summary-empty-p ()
  "Return non-nil when no errors or skips have been recorded this pull."
  (null org-canvas--pull-summary))

(defun org-canvas--pull-summary-records ()
  "Return the list of recorded summary entries in insertion order."
  (reverse org-canvas--pull-summary))

(cl-defun org-canvas--pull-summary-record (&key file item error log-line
                                                (kind 'error))
  "Record a non-fatal pull failure or a deliberate skip.
FILE is the .org file (basename) the record is scoped to.
ITEM is an optional identifier for the item (slug, id, title).
ERROR is a human-readable message — the failure for an error record,
the reason for a skip record.  It is usually raw
`error-message-string' text, so it is masked on the way in: the
summary is rendered into a buffer the user is invited to read and
share, and redaction used to stop at the logger (issue #154).
LOG-LINE is an optional pointer into the log buffer/file.
KIND is `error' (default) or `skip'."
  (push (list :kind kind :file file :item item
              :error (and error (org-canvas--log-redact error))
              :log-line log-line)
        org-canvas--pull-summary))

(defun org-canvas--pull-summary-records-of-kind (kind)
  "Return recorded summary entries of KIND, in insertion order.
Records written before :kind existed count as `error'."
  (cl-remove-if-not (lambda (rec) (eq (or (plist-get rec :kind) 'error) kind))
                    (org-canvas--pull-summary-records)))

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
Errors and skips are printed as separate sections.  Emits nothing when
the accumulator is empty so callers can wrap this unconditionally in
`with-output-to-temp-buffer'."
  (let ((errors (org-canvas--pull-summary-records-of-kind 'error))
        (skips (org-canvas--pull-summary-records-of-kind 'skip)))
    (when errors
      (princ (format "Pull complete with %d non-fatal error%s:\n"
                     (length errors)
                     (if (= (length errors) 1) "" "s")))
      (dolist (rec errors)
        (princ (org-canvas--pull-summary-format-record rec))))
    (when skips
      (when errors (princ "\n"))
      (princ (format "%d item%s skipped — on Canvas, not written locally:\n"
                     (length skips)
                     (if (= (length skips) 1) "" "s")))
      (dolist (rec skips)
        (princ (org-canvas--pull-summary-format-record rec))))))

(defun org-canvas--pull-summary-tally ()
  "Return a short phrase counting the errors and skips recorded this pull."
  (let ((errors (length (org-canvas--pull-summary-records-of-kind 'error)))
        (skips (length (org-canvas--pull-summary-records-of-kind 'skip))))
    (mapconcat #'identity
               (delq nil
                     (list (when (> errors 0)
                             (format "%d non-fatal error(s)" errors))
                           (when (> skips 0)
                             (format "%d item(s) skipped" skips))))
               ", ")))

(provide 'org-canvas-core-pull)
;;; org-canvas-core-pull.el ends here
