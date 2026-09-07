;;; org-canvas-orphans.el --- Find Canvas items no Org heading claims -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-cleanup-orphans' walks the feature registry, lists each
;; feature's remote items, and reports the ones no local heading claims
;; by id — what a heading deleted from an Org file leaves behind on
;; Canvas.  Items a feature's `:skip-fn' holds back are counted and
;; named, never silently absent (issue #81).  Deletion asks first.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)

;;;; Orphan Cleanup

(defun org-canvas--collect-local-ids (file id-property)
  "Collect all values of ID-PROPERTY from level-1 headings in FILE.
Returns a list of strings (CANVAS_ID or CANVAS_URL values).
Returns nil if file does not exist."
  (let ((file (expand-file-name file)))
    (when (file-exists-p file)
      (let (ids)
        (with-current-buffer (org-canvas--find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (let ((id (org-entry-get (point) id-property)))
                 (when id (push id ids))))
             "LEVEL=1" 'file)))
        (nreverse ids)))))

(defun org-canvas--log-protected-items (name items skip-fn reason)
  "Log the number of ITEMS that SKIP-FN will keep out of cleanup for NAME.
REASON is the `:skip-reason' of the feature, or nil.  A protected item
is simply absent from the orphan list otherwise, which reads the same
as nothing being there (issue #81)."
  (when skip-fn
    (let ((protected (cl-count-if skip-fn items)))
      (when (> protected 0)
        (org-canvas--log-info org-canvas--logger
          "[Orphan] %s: %d item(s) never considered for cleanup%s"
          name protected (if reason (format " (%s)" reason) ""))))))

(defun org-canvas--filter-orphans (remote-items local-ids id-field skip-fn)
  "Return items from REMOTE-ITEMS not present in LOCAL-IDS.
ID-FIELD is the alist key for item IDs.  SKIP-FN, when non-nil,
filters out items before the orphan check."
  (let (orphans)
    (dolist (item remote-items)
      (unless (and skip-fn (funcall skip-fn item))
        (let ((remote-id (format "%s" (alist-get id-field item))))
          (unless (member remote-id local-ids)
            (push item orphans)))))
    (nreverse orphans)))

(cl-defun org-canvas--find-orphans-for-feature (feature)
  "Find orphaned Canvas items for FEATURE.
FEATURE is a plist from `org-canvas--feature-registry'.
Returns a list of orphaned items (alists from the Canvas API),
or nil if no orphans found."
  (let* ((name (plist-get feature :name))
         (file-var (plist-get feature :file-var))
         (id-field (plist-get feature :id-field))
         (id-property (plist-get feature :id-property))
         (skip-fn (plist-get feature :skip-fn))
         (file (and (boundp file-var) (symbol-value file-var))))
    (unless file
      (org-canvas--log-info org-canvas--logger "[Orphan] %s: file var not set, skipping" name)
      (cl-return-from org-canvas--find-orphans-for-feature nil))
    (let ((local-ids (org-canvas--collect-local-ids file id-property)))
      (condition-case err
          (let* ((url (org-canvas--feature-list-url feature))
                 (remote-items (org-canvas-api-request-all-pages
                                'GET url (org-canvas--feature-list-params feature))))
            (org-canvas--log-protected-items
             name remote-items skip-fn (plist-get feature :skip-reason))
            (org-canvas--filter-orphans remote-items local-ids id-field skip-fn))
        (error
         (org-canvas--log-warning org-canvas--logger
           "[Orphan] %s: failed to fetch remote items: %s"
           name (error-message-string err))
         nil)))))

(defun org-canvas--orphan-format-buffer (all-orphans)
  "Format orphan results into the *canvas-orphans* buffer.
ALL-ORPHANS is an alist of (feature-plist . orphan-list) pairs.
Returns the buffer."
  (let ((buf (get-buffer-create "*canvas-orphans*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "org-canvas Orphan Report\n")
        (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
        (insert (make-string 60 ?=))
        (insert "\n\n")
        (let ((total 0))
          (dolist (entry all-orphans)
            (let* ((feature (car entry))
                   (orphans (cdr entry))
                   (name (plist-get feature :name))
                   (title-field (plist-get feature :title-field))
                   (id-field (plist-get feature :id-field)))
              (insert (format "%s: %d orphan(s)\n" name (length orphans)))
              (dolist (item orphans)
                (let ((title (alist-get title-field item))
                      (id (alist-get id-field item)))
                  (insert (format "  - [%s] %s\n" id (or title "(untitled)")))))
              (setq total (+ total (length orphans)))
              (insert "\n")))
          (insert (make-string 60 ?=))
          (insert (format "\nTotal orphans: %d\n" total)))
        (special-mode)))
    buf))

(defun org-canvas--orphan-delete-all (all-orphans)
  "Delete all orphaned items in ALL-ORPHANS from Canvas.
ALL-ORPHANS is a list of (FEATURE . ITEMS) pairs."
  (dolist (entry all-orphans)
    (let* ((feature (car entry))
           (orphans (cdr entry))
           (name (plist-get feature :name))
           (delete-data (plist-get feature :delete-data))
           (id-field (plist-get feature :id-field)))
      (dolist (item orphans)
        (let* ((id (alist-get id-field item))
               (url (org-canvas--feature-item-url feature id)))
          (condition-case err
              (progn
                (apply #'org-canvas-api-request 'DELETE url
                       (and delete-data (list :data delete-data)))
                (org-canvas--log-info org-canvas--logger
                  "[Orphan] Deleted %s #%s" name id))
            (error
             (org-canvas--log-warning org-canvas--logger
               "[Orphan] Failed to delete %s #%s: %s"
               name id (error-message-string err)))))))))

;;;###autoload
(defun org-canvas-cleanup-orphans ()
  "Find and optionally delete Canvas items not present in local Org files.
Scans all pushable features, compares remote Canvas items against local
CANVAS_ID/CANVAS_URL properties, and reports orphaned items.

Orphans are Canvas items that exist remotely but have no corresponding
Org heading locally (e.g., because the heading was deleted from the Org file)."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (org-canvas--log-info org-canvas--logger "========================================")
  (org-canvas--log-info org-canvas--logger ">>> SCANNING FOR ORPHANED ITEMS")
  (org-canvas--log-info org-canvas--logger "========================================")
  (let ((all-orphans nil)
        (total-orphans 0))
    ;; Scan each feature
    (dolist (feature org-canvas--feature-registry)
      (let ((name (plist-get feature :name)))
        (message "Scanning %s..." name)
        (org-canvas--log-info org-canvas--logger "[Orphan] Scanning %s..." name)
        (let ((orphans (org-canvas--find-orphans-for-feature feature)))
          (when orphans
            (push (cons feature orphans) all-orphans)
            (setq total-orphans (+ total-orphans (length orphans)))
            (org-canvas--log-info org-canvas--logger
              "[Orphan] %s: found %d orphan(s)" name (length orphans))))))
    (setq all-orphans (nreverse all-orphans))
    ;; Display results
    (if (= total-orphans 0)
        (progn
          (org-canvas--log-info org-canvas--logger "[Orphan] No orphans found.")
          (message "No orphaned items found."))
      (let ((buf (org-canvas--orphan-format-buffer all-orphans)))
        (display-buffer buf)
        (when (yes-or-no-p
               (format "Found %d orphan(s) (listed in *canvas-orphans*).  Delete them from Canvas? "
                       total-orphans))
          (org-canvas--orphan-delete-all all-orphans)
          (message "Orphan cleanup complete. %d item(s) deleted." total-orphans))))))

(provide 'org-canvas-orphans)
;;; org-canvas-orphans.el ends here
