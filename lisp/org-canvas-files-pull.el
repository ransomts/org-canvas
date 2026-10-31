;;; org-canvas-files-pull.el --- Rebuild files.org from Canvas  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The pull half of `org-canvas-files', the read-only direction: list the
;; course folders, download every file whose local copy is missing or a
;; different length, and write the entries back into files.org.
;;
;; Three layouts, decided from the file itself: the fresh tree (no
;; CANVAS_IDs yet), the flat list a course pulled before folder
;; hierarchy, and the hierarchical one after it.  A re-pull keeps every
;; CANVAS_ID the sync needs, leaves a file whose size still matches
;; alone (issue #158), and marks what Canvas no longer lists rather than
;; deleting the heading outright.
;;
;; Nothing here uploads: the sync in `org-canvas-files.el' owns that,
;; and calls the download and property writers declared there.  This
;; file requires that one back, never the other way.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
(require 'org-canvas-files)

;;;; Pull

(defun org-canvas--file-pull-stale-p (local-path size)
  "Return non-nil when LOCAL-PATH should be fetched again.
True when nothing is there, or when SIZE is known and the local file is
a different length.  A file whose size matches is left alone, which is
what keeps a re-pull of a 192 MB course from downloading it again
\(issue #158); it also means a Canvas-side edit that happens to
preserve the byte count is not noticed, which is the trade the size
check makes."
  (or (not (file-exists-p local-path))
      (and (numberp size)
           (/= size (file-attribute-size (file-attributes local-path))))))

(defun org-canvas--file-pull-download (display-name download-url local-path size
                                                    &optional force)
  "Download file DISPLAY-NAME from DOWNLOAD-URL to LOCAL-PATH when it is stale.
SIZE is used for logging and to decide staleness; may be nil.  With
FORCE, overwrite the local file whatever its size — that is what
resolving a conflict by pulling means, and the user chose it at the
diff prompt."
  (when (and download-url
             (or force (org-canvas--file-pull-stale-p local-path size)))
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

(cl-defstruct (org-canvas--file-pull-context (:copier nil)
               (:constructor org-canvas--file-pull-context))
  "The state one files.org tree pull carries across the files it emits.
CONTENT-DIR is the directory files download into, TOTAL how many files
the pull has to emit, and COUNTER a cons whose car is the number it has
emitted so far."
  content-dir total counter)

(defun org-canvas--file-pull-emit-file-heading (item depth rel-path context)
  "Emit a heading + properties for ITEM at DEPTH under REL-PATH.
CONTEXT is the pull's `org-canvas--file-pull-context': it counts the
file and is asked to download it to CONTENT-DIR/REL-PATH/DISPLAY_NAME."
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
         (counter (org-canvas--file-pull-context-counter context))
         (local-path (org-canvas--file-safe-local-path
                      local-rel (org-canvas--file-pull-context-content-dir context))))
    (insert (make-string depth ?*) " " heading-text "\n")
    (let ((pos (save-excursion (forward-line -1) (point))))
      (org-canvas-org-save-sync-state pos id)
      (org-canvas--file-pull-set-properties pos item))
    (setcar counter (1+ (car counter)))
    (message "Files [%d/%d] Pulling '%s'..." (car counter)
             (org-canvas--file-pull-context-total context) display-name)
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
         (context (org-canvas--file-pull-context
                   :content-dir content-dir :total (length remote-items)
                   :counter (list 0)))
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
           item file-depth rel-path context))))
    (car (org-canvas--file-pull-context-counter context))))

(defun org-canvas--file-pull-set-properties (pos item)
  "Set visibility, usage-rights, content-type and size properties at POS.
ITEM is the Canvas file object.  Everything push reads comes from the
registry (issue #135): PUBLISHED is the inverse of `locked', the
Publish control, and `hidden' is the separate unlisted state (issue
#50); the usage rights are nested and read through `:remote-fn's.
CONTENT_TYPE and SIZE are informational and written directly."
  (org-canvas--pull-item-from-registry "files" item pos)
  (let ((content-type (alist-get 'content-type item))
        (size (alist-get 'size item)))
    (when content-type
      (org-canvas-org-set-property pos "CONTENT_TYPE" content-type))
    (when size
      (org-canvas-org-set-property pos "SIZE" (format "%s" size)))))

(defun org-canvas--file-pull-folder-heading-p ()
  "Return non-nil when the heading at point is a folder rather than a file.
A folder heading is the one thing the tree emits that carries neither a
Canvas id nor a file link; the mode detector reads the same two marks."
  (let* ((heading (org-canvas--strip-statistics-cookie
                   (org-get-heading t t t t)))
         (link-path (org-canvas--file-extract-link-path heading)))
    (and (null (org-entry-get (point) "CANVAS_ID"))
         (null link-path))))

(defun org-canvas--file-pull-find-by-id (id)
  "Return the position of the heading whose CANVAS_ID is ID, or nil.
Searches the whole buffer, since a re-pull may find a file the operator
has moved to a different folder heading by hand."
  (let ((target (format "%s" id))
        (found nil))
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-map-entries
      (lambda ()
        (when (and (not found)
                   (equal (org-entry-get (point) "CANVAS_ID") target))
          (setq found (point))))
      t 'file))
    found))

(defun org-canvas--file-pull-heading-folder-parts (pos)
  "Return the folder path of the heading at POS, as a list of names.
Nil for a heading at level 1, which lives at the root."
  (org-with-wide-buffer
   (goto-char pos)
   (org-back-to-heading t)
   (let (parts)
     (while (org-up-heading-safe)
       (push (org-canvas--strip-statistics-cookie (org-get-heading t t t t))
             parts))
     parts)))

(defun org-canvas--file-pull-child-folder (parent name)
  "Return the position of folder heading NAME under PARENT, creating it.
PARENT is the position of the containing folder heading, or nil for the
top level.  A folder heading is matched by name among PARENT's direct
children; a file heading of the same name never matches, so a folder and
a file may share a name without colliding."
  (let* ((level (if parent
                    (1+ (org-with-wide-buffer (goto-char parent)
                                              (org-current-level)))
                  1))
         (found nil))
    (org-with-wide-buffer
     (if parent (goto-char parent) (goto-char (point-min)))
     (let ((limit (if parent
                      (org-with-wide-buffer (goto-char parent)
                                            (org-end-of-subtree t t)
                                            (point))
                    (point-max))))
       (when parent (org-back-to-heading t) (forward-line 1))
       (while (and (not found)
                   (re-search-forward org-complex-heading-regexp limit t))
         (org-back-to-heading t)
         (when (and (= (org-current-level) level)
                    (org-canvas--file-pull-folder-heading-p)
                    (equal (org-canvas--strip-statistics-cookie
                            (org-get-heading t t t t))
                           name))
           (setq found (point)))
         (org-end-of-subtree t t))))
    (or found
        (org-canvas--file-pull-insert-folder parent name level))))

(defun org-canvas--file-pull-insert-folder (parent name level)
  "Insert folder heading NAME at LEVEL at the end of PARENT.
Returns its position.  PARENT is nil for the top level."
  (org-with-wide-buffer
   (if parent
       (progn (goto-char parent) (org-end-of-subtree t t))
     (goto-char (point-max)))
   (unless (bolp) (insert "\n"))
   (let ((start (point)))
     (insert (make-string level ?*) " " name "\n")
     start)))

(defun org-canvas--file-pull-folder-position (parts)
  "Return the position a file under folder PARTS should be inserted at.
Creates any folder heading PARTS names that does not exist yet.  Nil
PARTS means the root, whose insertion point is the end of the buffer."
  (let ((parent nil))
    (dolist (part parts)
      (setq parent (org-canvas--file-pull-child-folder parent part)))
    (org-with-wide-buffer
     (if parent
         (progn (goto-char parent) (org-end-of-subtree t t))
       (goto-char (point-max)))
     (unless (bolp) (insert "\n"))
     (point))))

(defun org-canvas--file-pull-heading-text (item rel-path)
  "Return the Org heading text for ITEM living under REL-PATH."
  (let* ((display-name (alist-get 'display_name item))
         (local-rel (if (string-empty-p rel-path)
                        display-name
                      (concat rel-path "/" display-name))))
    (org-link-make-string
     (concat "file:content/" local-rel)
     (org-canvas--file-sanitize-headline-desc display-name))))

(defun org-canvas--file-pull-insert-at (item rel-path parts content-dir)
  "Insert a heading for ITEM under folder PARTS, and return its position.
REL-PATH is the folder path as a string, CONTENT-DIR the download root."
  (let ((pos (org-canvas--file-pull-folder-position parts))
        (level (1+ (length parts))))
    (org-with-wide-buffer
     (goto-char pos)
     (insert (make-string level ?*) " "
             (org-canvas--file-pull-heading-text item rel-path) "\n")
     (goto-char pos)
     (org-canvas--file-pull-record item rel-path content-dir)
     pos)))

(defun org-canvas--file-pull-record (item rel-path content-dir)
  "Stamp the heading at point from ITEM and fetch its bytes.
REL-PATH places the download under CONTENT-DIR."
  (let* ((display-name (alist-get 'display_name item))
         (local-rel (if (string-empty-p rel-path)
                        display-name
                      (concat rel-path "/" display-name)))
         (local-path (org-canvas--file-safe-local-path local-rel content-dir)))
    (org-canvas-org-save-sync-state (point) (alist-get 'id item))
    (org-canvas--file-pull-set-properties (point) item)
    (org-canvas--file-pull-download
     display-name (alist-get 'url item) local-path (alist-get 'size item))))

(defun org-canvas--file-pull-refresh-at (pos item rel-path content-dir)
  "Refresh the heading at POS in place from ITEM under REL-PATH.
The heading text is rewritten so a rename on Canvas lands locally, and
CONTENT-DIR receives the bytes."
  (org-with-wide-buffer
   (goto-char pos)
   (org-back-to-heading t)
   (org-edit-headline (org-canvas--file-pull-heading-text item rel-path))
   (org-canvas--file-pull-record item rel-path content-dir)))

(defun org-canvas--file-pull-cut-subtree (pos)
  "Delete the heading subtree at POS."
  (org-with-wide-buffer
   (goto-char pos)
   (org-back-to-heading t)
   (delete-region (point) (org-end-of-subtree t t))))

(defun org-canvas--file-pull-upsert-item (item folder-map content-dir)
  "Place ITEM in the current buffer, under the folder Canvas now names.
FOLDER-MAP maps folder id to relative path; CONTENT-DIR is the download
root.  Returns `added', `moved' or `updated'.  A file whose folder changed is
cut from where it was and written under the new one, rather than left
behind as a second copy of itself; a file the buffer has never seen is
created, and its folder headings with it."
  (let* ((folder-id (alist-get 'folder_id item))
         (rel-path (or (and folder-id (gethash folder-id folder-map)) ""))
         (parts (if (string-empty-p rel-path) nil (split-string rel-path "/" t)))
         (pos (org-canvas--file-pull-find-by-id (alist-get 'id item))))
    (cond
     ((null pos)
      (org-canvas--file-pull-insert-at item rel-path parts content-dir)
      'added)
     ((equal (org-canvas--file-pull-heading-folder-parts pos) parts)
      (org-canvas--file-pull-refresh-at pos item rel-path content-dir)
      'updated)
     (t
      (org-canvas--file-pull-cut-subtree pos)
      (org-canvas--file-pull-insert-at item rel-path parts content-dir)
      'moved))))

(defun org-canvas--file-pull-emit-hierarchical (folder-map remote content-dir)
  "Upsert REMOTE into the nested files.org in the current buffer.
FOLDER-MAP maps folder id to relative path; CONTENT-DIR is the download
root.  Returns a plist (:added N :moved N :updated N).

A nested layout used to refuse re-pull outright, so refreshing files
meant deleting files.org — the file holding every CANVAS_ID mapping —
and rebuilding, which also discarded anything hand-written in it and
was hit by `org-canvas-pull-all' on every run after the first (issue
#158).  Each file is matched by its Canvas id wherever it sits, so a
heading an operator moved by hand is found; only a file whose folder
changed on Canvas is relocated.  Nothing local is deleted for being
absent from Canvas: pruning stays with `org-canvas-cleanup-orphans'."
  (let ((total (length remote))
        (count 0)
        (added 0) (moved 0) (updated 0))
    (dolist (item remote)
      (cl-incf count)
      (message "Files [%d/%d] Pulling '%s'..."
               count total (alist-get 'display_name item))
      (pcase (org-canvas--file-pull-upsert-item item folder-map content-dir)
        ('added (cl-incf added))
        ('moved (cl-incf moved))
        (_ (cl-incf updated))))
    (list :added added :moved moved :updated updated)))

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

(defun org-canvas--file-pull-mark-unlisted (file remote)
  "Mark the file headings of FILE that REMOTE lacks and Canvas deleted.
Return the closing line's tail naming them (`org-canvas--pull-gone-suffix').
A file heading is matched at any level, since a nested files.org keeps
files under folder headings; a folder heading carries no Canvas id, so
it is never read or marked: a folder deleted on Canvas takes its files
with it, and those are what get marked (issue #399).  A fresh pull
builds the tree from the list and has nothing to mark."
  (org-canvas--pull-gone-suffix
   (org-canvas--pull-mark-unlisted
    (org-canvas--pull-gone-request
     :file file :feature "files" :remote remote
     :id-field 'id :id-property "CANVAS_ID" :query "LEVEL>0"))))

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

On a re-pull, entries are updated in place: a flat files.org stays
flat, and a nested one is upserted folder by folder, with a file whose
folder changed on Canvas moved rather than duplicated.  Nothing local
is deleted for being absent from Canvas — that is
`org-canvas-cleanup-orphans' — but a heading whose file Canvas deleted
is marked CANVAS_DELETED, so a push skips it (issue #399).

A file whose local copy is already the right size is not downloaded
again, so refreshing a large course costs one listing rather than every
byte."
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
    (org-canvas--create-file file)
    (unless (file-directory-p content-dir)
      (make-directory content-dir t))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (let ((mode (org-canvas--file-pull-mode)))
        (pcase mode
          ('hierarchical
           (let* ((stats (org-canvas--file-pull-emit-hierarchical
                          folder-map remote content-dir))
                  (added (plist-get stats :added))
                  (moved (plist-get stats :moved))
                  (updated (plist-get stats :updated))
                  (note (org-canvas--file-pull-mark-unlisted file remote)))
             (org-canvas--pull-write-file-header)
             (org-canvas--save-buffer)
             (org-canvas--log-info org-canvas--logger
               "Files pull complete (nested upsert): %d new, %d moved, %d refreshed%s"
               added moved updated note)
             (message "Files pull complete: %d new, %d moved, %d refreshed%s."
                      added moved updated note)))
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
           (let* ((emitted
                   (org-canvas--file-pull-emit-flat file remote content-dir))
                  (note (org-canvas--file-pull-mark-unlisted file remote)))
             (org-canvas--pull-write-file-header)
             (org-canvas--save-buffer)
             (org-canvas--log-info org-canvas--logger
               "Files pull complete (flat): %d files%s" emitted note)
             (message "Files pull complete: %d files%s." emitted note))))))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (setq org-canvas--file-id-cache nil)))

(provide 'org-canvas-files-pull)
;;; org-canvas-files-pull.el ends here
