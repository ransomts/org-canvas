;;; org-canvas.el --- Sync Org Mode files with Canvas LMS  -*- lexical-binding: t; -*-

;; Author: Tim Ransom
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (plz "0.9") (elog "2.0") (org "9.6"))

;; Keywords: comm, tools
;; URL: https://github.com/ransomts/org-canvas

;;; Commentary:

;; org-canvas synchronizes course content from Org Mode files to Canvas LMS.
;; It enables a "source of truth" workflow where you design your entire course
;; in Org Mode and push changes to Canvas via its REST API.
;;
;; MAIN COMMANDS
;; =============
;; org-canvas-sync       - Sync all course content (proper dependency order)
;; org-canvas-delete-all - Delete all synced content from Canvas
;;
;; SYNC ORDER
;; ==========
;; Content is synced in dependency order:
;;
;;   Tier 0 (no deps):      outcomes, rubrics, assignment-groups,
;;                          sections (pull), files, pages, discussions,
;;                          announcements
;;   Tier 1 (needs Tier 0): quizzes, assignments
;;   Tier 1.5:              assignment-groups (re-sync for drop rules)
;;   Tier 1.75:             overrides (needs sections + assignments)
;;   Tier 2 (needs all):    modules
;;
;; This ensures that when an assignment links to a rubric, the rubric's
;; CANVAS_ID is already available.
;;
;; CONFIGURATION
;; =============
;; Create org-canvas-credentials.el in your course directory:
;;
;;   (setq org-canvas-api-token "your-token")
;;   (setq org-canvas-course-id "12345")
;;   (setq org-canvas-base-url "https://canvas.instructure.com")
;;   (setq org-canvas-directory "/path/to/course/")
;;
;; SEE ALSO
;; ========
;; - documentation/manual.org for full documentation
;; - example-course/ for sample file formats

;;; Code:

;; Import Core (Utilities & Globals)
(require 'org-canvas-core)

;; Import Feature Modules
(require 'org-canvas-announcements)
(require 'org-canvas-assignment-groups)
(require 'org-canvas-assignments)
(require 'org-canvas-discussions)
(require 'org-canvas-files)
(require 'org-canvas-modules)
(require 'org-canvas-outcomes)
(require 'org-canvas-pages)
(require 'org-canvas-new-quizzes)
(require 'org-canvas-quizzes)
(require 'org-canvas-rubrics)
(require 'org-canvas-sections)
(require 'org-canvas-settings)
(require 'org-canvas-group-categories)
(require 'org-canvas-calendar)
(require 'org-canvas-validate)
(require 'org-canvas-submissions)
(require 'org-canvas-setup)

;; Note: Feature-specific file paths (e.g., `org-canvas-rubrics-file`) are now
;; defined in their respective modules.

(defun org-canvas--safe-sync (sync-fn label)
  "Call SYNC-FN, catching file-not-found errors gracefully.
LABEL is used for logging (e.g., \"Pages\")."
  (condition-case err
      (funcall sync-fn)
    (error
     (let ((msg (error-message-string err)))
       (if (string-match-p "file not found\\|no such file" (downcase msg))
           (elog-info org-canvas--logger "[Skip] %s: %s" label msg)
         (elog-error org-canvas--logger "[FAILED] %s: %s" label msg))))))

(defun org-canvas--run-tier (tier wrapper-fn)
  "Run each (FUNCTION LABEL) entry in TIER through WRAPPER-FN."
  (dolist (entry tier)
    (funcall wrapper-fn (car entry) (cadr entry))))

;; Sync in dependency order (see documentation/manual.org for details):
;;   Tier -1: Course settings (before any content)
;;   Tier 0:  No dependencies — synced in any order
;;   Tier 1:  Depends on Tier 0 (quizzes, assignments link to Tier 0 items)
;;   Tier 1.5: Re-sync assignment groups to apply drop rules
;;   Tier 1.75: Overrides need sections + assignments
;;   Tier 2:  Modules reference all content types
(defconst org-canvas--sync-tiers
  '(((org-canvas-sync-settings "Settings"))
    ((org-canvas-sync-outcomes "Outcomes")
     (org-canvas-sync-rubrics "Rubrics")
     (org-canvas-sync-assignment-groups "Assignment Groups")
     (org-canvas-sync-group-categories "Group Categories")
     (org-canvas-pull-sections "Sections")
     (org-canvas-sync-files "Files")
     (org-canvas-sync-pages "Pages")
     (org-canvas-sync-discussions "Discussions")
     (org-canvas-sync-announcements "Announcements")
     (org-canvas-sync-calendar-events "Calendar Events"))
    ((org-canvas-sync-quizzes "Quizzes")
     (org-canvas-sync-new-quizzes "New Quizzes")
     (org-canvas-sync-assignments "Assignments"))
    ((org-canvas-sync-assignment-groups "Assignment Groups"))
    ((org-canvas-sync-overrides "Overrides"))
    ((org-canvas-sync-modules "Modules")))
  "Sync tiers in dependency order.  Each tier is a list of (FN LABEL) pairs.")

;;;###autoload
(defun org-canvas-sync ()
  "Sync all enabled Canvas features."
  (interactive)
  (when org-canvas--sync-in-progress
    (user-error "A sync is already in progress.  Please wait for it to finish"))
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((org-canvas--inhibit-log-clear t)
        (org-canvas--sync-in-progress t))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> STARTING GLOBAL SYNC")
    (elog-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
    (elog-info org-canvas--logger "========================================")
    (org-canvas--preflight-check)
    ;; Tiers -1 and 0
    (org-canvas--run-tier (nth 0 org-canvas--sync-tiers) #'org-canvas--safe-sync)
    (org-canvas--run-tier (nth 1 org-canvas--sync-tiers) #'org-canvas--safe-sync)
    (elog-info org-canvas--logger
      "[Note] Same-tier cross-references (e.g., page→page) may require a second sync to fully resolve")
    ;; Tiers 1 through 2
    (dolist (tier (nthcdr 2 org-canvas--sync-tiers))
      (org-canvas--run-tier tier #'org-canvas--safe-sync))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> GLOBAL SYNC COMPLETE")
    (elog-info org-canvas--logger "========================================")
    ;; Clear session-scoped caches
    (setq org-canvas--image-cache nil)
    (message "Sync complete. See *canvas-log* for details.")))

;; Delete in REVERSE dependency order:
;;   Tier 2:  Modules (reference all content types)
;;   Tier 1:  Assignments, Quizzes (may reference Tier 0)
;;   Tier 0:  Everything else (no dependencies, safe to delete last)
(defconst org-canvas--delete-tiers
  '(((org-canvas-delete-all-modules "Modules"))
    ((org-canvas-delete-all-assignments "Assignments")
     (org-canvas-delete-all-quizzes "Quizzes")
     (org-canvas-delete-all-new-quizzes "New Quizzes"))
    ((org-canvas-delete-all-files "Files")
     (org-canvas-delete-all-announcements "Announcements")
     (org-canvas-delete-all-discussions "Discussions")
     (org-canvas-delete-all-pages "Pages")
     (org-canvas-delete-all-assignment-groups "Assignment Groups")
     (org-canvas-delete-all-group-categories "Group Categories")
     (org-canvas-delete-all-rubrics "Rubrics")
     (org-canvas-delete-all-outcomes "Outcomes")
     (org-canvas-delete-all-calendar-events "Calendar Events")))
  "Delete tiers in reverse dependency order.  Each tier is a list of (FN LABEL) pairs.")

;;;###autoload
(defun org-canvas-delete-all ()
  "Delete ALL Canvas content for this course.
This is a destructive operation that removes all synced content from Canvas.
Deletion order is reverse of sync order to respect dependencies."
  (interactive)
  (unless (yes-or-no-p
           (format "WARNING: This will DELETE ALL content from Canvas course %s.  Are you sure? "
                   org-canvas-course-id))
    (user-error "Aborted"))
  (unless (yes-or-no-p "This cannot be undone.  Do you wish to continue? ")
    (user-error "Aborted"))
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((org-canvas--inhibit-log-clear t))
    (elog-info org-canvas--logger "Starting Global Delete...")
    (dolist (tier org-canvas--delete-tiers)
      (org-canvas--run-tier
       tier (lambda (fn label)
              (elog-info org-canvas--logger "[Delete] %s..." label)
              (funcall fn))))
    (elog-info org-canvas--logger "Global Delete Complete.")
    (message "Global Delete Complete. See *canvas-log*.")))

;;;; Status Overview

(defconst org-canvas--status-content-types
  '(("Assignments"       org-canvas-assignments-file       "CANVAS_ID")
    ("Pages"             org-canvas-pages-file             "CANVAS_URL")
    ("Quizzes"           org-canvas-quizzes-file           "CANVAS_ID")
    ("Modules"           org-canvas-modules-file           "CANVAS_ID")
    ("Files"             org-canvas-files-file             "CANVAS_ID")
    ("Outcomes"          org-canvas-outcomes-file          "CANVAS_ID")
    ("Rubrics"           org-canvas-rubrics-file           "CANVAS_ID")
    ("Discussions"       org-canvas-discussions-file       "CANVAS_ID")
    ("Announcements"     org-canvas-announcements-file     "CANVAS_ID")
    ("Assignment Groups" org-canvas-assignment-groups-file "CANVAS_ID")
    ("Sections"          org-canvas-sections-file          "CANVAS_ID")
    ("New Quizzes"       org-canvas-new-quizzes-file       "CANVAS_ASSIGNMENT_ID")
    ("Group Categories"  org-canvas-group-categories-file  "CANVAS_ID")
    ("Calendar Events"   org-canvas-calendar-events-file   "CANVAS_ID"))
  "Content types for status reporting: (label file-var id-property).")

(defun org-canvas--status-count-entries (file id-prop)
  "Count synced and pending entries in FILE using ID-PROP.
Returns a plist (:synced N :pending N :last-synced TS-OR-NIL)."
  (let ((synced 0) (pending 0) (last-synced nil))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let ((id (org-entry-get (point) id-prop))
                 (ts (org-entry-get (point) "LAST_SYNCED")))
             (if id
                 (progn
                   (setq synced (1+ synced))
                   (when (and ts (or (not last-synced) (string> ts last-synced)))
                     (setq last-synced ts)))
               (setq pending (1+ pending)))))
         "LEVEL=1" 'file)))
    (list :synced synced :pending pending :last-synced last-synced)))

(defun org-canvas--status-report-file (buf label file-var id-prop)
  "Report sync status for content type LABEL to buffer BUF.
FILE-VAR is the symbol of the file path variable.
ID-PROP is the property used to identify synced items."
  (let ((file (and (boundp file-var)
                   (expand-file-name (symbol-value file-var)))))
    (with-current-buffer buf
      (insert (format "\n%s" label))
      (if (or (not file) (not (file-exists-p file)))
          (insert " — file not found, skipped\n")
        (let* ((counts (org-canvas--status-count-entries file id-prop))
               (synced (plist-get counts :synced))
               (pending (plist-get counts :pending))
               (last-synced (plist-get counts :last-synced)))
          (insert (format " (%s)\n" (file-name-nondirectory file)))
          (insert (format "  Synced: %-4d  Pending: %-4d" synced pending))
          (when last-synced
            (insert (format "  Last: %s" last-synced)))
          (insert "\n"))))))

;;;###autoload
(defun org-canvas-status ()
  "Display sync status overview for all content types."
  (interactive)
  (let ((buf (get-buffer-create "*canvas-status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "org-canvas Sync Status\n")
        (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
        (insert (make-string 60 ?=))
        (insert "\n")
        (dolist (entry org-canvas--status-content-types)
          (org-canvas--status-report-file
           buf (car entry) (cadr entry) (caddr entry)))
        (insert (format "\n%s\n" (make-string 60 ?=)))
        (insert "Use M-x org-canvas-sync to sync, M-x org-canvas-sync-dry-run to preview.\n"))
      (special-mode))
    (display-buffer buf)))

;;;; Force Push (Bypass Conflict Detection)

;;;###autoload
(defun org-canvas-force-push ()
  "Sync all features, bypassing conflict detection.
Like `org-canvas-sync' but with `org-canvas-detect-conflicts' set to nil,
so remote changes are overwritten without warning."
  (interactive)
  (let ((org-canvas-detect-conflicts nil))
    (org-canvas-sync)))

;;;; Dry-Run Preview

;;;###autoload
(defun org-canvas-sync-dry-run ()
  "Preview what a full sync would do without contacting the API.
No properties are modified and no API requests are sent."
  (interactive)
  (let ((org-canvas--dry-run t))
    (org-canvas-sync)))

;;;; Pull All (Canvas → Org Migration)

(defun org-canvas--safe-pull (pull-fn label)
  "Call PULL-FN, catching errors gracefully.
LABEL is used for logging."
  (condition-case err
      (funcall pull-fn)
    (error
     (elog-warning org-canvas--logger "[Pull] %s failed: %s"
       label (error-message-string err)))))

;; Pull in dependency order:
;;   Settings, then structural items, then linked items, then modules
(defconst org-canvas--pull-tiers
  '(((org-canvas-pull-settings "Settings"))
    ((org-canvas-pull-sections "Sections")
     (org-canvas-pull-assignment-groups "Assignment Groups")
     (org-canvas-pull-group-categories "Group Categories")
     (org-canvas-pull-outcomes "Outcomes")
     (org-canvas-pull-rubrics "Rubrics")
     (org-canvas-pull-pages "Pages")
     (org-canvas-pull-files "Files")
     (org-canvas-pull-discussions "Discussions")
     (org-canvas-pull-announcements "Announcements")
     (org-canvas-pull-calendar-events "Calendar Events"))
    ((org-canvas-pull-assignments "Assignments")
     (org-canvas-pull-quizzes "Quizzes")
     (org-canvas-pull-new-quizzes "New Quizzes"))
    ((org-canvas-pull-modules "Modules")))
  "Pull tiers in dependency order.  Each tier is a list of (FN LABEL) pairs.")

;;;###autoload
(defun org-canvas-pull-all ()
  "Import an entire Canvas course into Org files.
Pulls all content types in dependency order, creating .org files
as needed.  HTML content is converted to Org format via pandoc.

This is the migration entry point for instructors with existing
Canvas courses who want to adopt org-canvas."
  (interactive)
  (unless (executable-find "pandoc")
    (unless (yes-or-no-p
             "Pandoc not found.  HTML will be stored raw.  Continue? ")
      (user-error "Aborted")))
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((org-canvas--inhibit-log-clear t))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> STARTING FULL COURSE PULL")
    (elog-info org-canvas--logger "Course: %s | URL: %s"
      org-canvas-course-id org-canvas-base-url)
    (elog-info org-canvas--logger "========================================")
    (org-canvas--preflight-check)
    (dolist (tier org-canvas--pull-tiers)
      (org-canvas--run-tier tier #'org-canvas--safe-pull))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> FULL COURSE PULL COMPLETE")
    (elog-info org-canvas--logger "========================================")
    (message "Course pull complete.  See *canvas-log* for details.")))

;;;; Orphan Cleanup

(defun org-canvas--collect-local-ids (file id-property)
  "Collect all values of ID-PROPERTY from level-1 headings in FILE.
Returns a list of strings (CANVAS_ID or CANVAS_URL values).
Returns nil if file does not exist."
  (let ((file (expand-file-name file)))
    (when (file-exists-p file)
      (let (ids)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (let ((id (org-entry-get (point) id-property)))
                 (when id (push id ids))))
             "LEVEL=1" 'file)))
        (nreverse ids)))))

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
FEATURE is a plist from `org-canvas--orphan-feature-registry'.
Returns a list of orphaned items (alists from the Canvas API),
or nil if no orphans found."
  (let* ((name (plist-get feature :name))
         (endpoint (plist-get feature :endpoint))
         (file-var (plist-get feature :file-var))
         (id-field (plist-get feature :id-field))
         (id-property (plist-get feature :id-property))
         (list-params (plist-get feature :list-params))
         (skip-fn (plist-get feature :skip-fn))
         (file (and (boundp file-var) (symbol-value file-var))))
    (unless file
      (elog-info org-canvas--logger "[Orphan] %s: file var not set, skipping" name)
      (cl-return-from org-canvas--find-orphans-for-feature nil))
    (let ((local-ids (org-canvas--collect-local-ids file id-property)))
      (condition-case err
          (let* ((url (org-canvas-api-course-endpoint endpoint))
                 (remote-items (org-canvas-api-request-all-pages
                                'GET url list-params)))
            (org-canvas--filter-orphans remote-items local-ids id-field skip-fn))
        (error
         (elog-warning org-canvas--logger
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
           (endpoint (plist-get feature :endpoint))
           (id-field (plist-get feature :id-field)))
      (dolist (item orphans)
        (let* ((id (alist-get id-field item))
               (url (org-canvas-api-course-endpoint
                     (format "%s/%%s" endpoint) id)))
          (condition-case err
              (progn
                (org-canvas-api-request 'DELETE url)
                (elog-info org-canvas--logger
                  "[Orphan] Deleted %s #%s" name id))
            (error
             (elog-warning org-canvas--logger
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
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> SCANNING FOR ORPHANED ITEMS")
  (elog-info org-canvas--logger "========================================")
  (let ((all-orphans nil)
        (total-orphans 0))
    ;; Scan each feature
    (dolist (feature org-canvas--orphan-feature-registry)
      (let ((name (plist-get feature :name)))
        (message "Scanning %s..." name)
        (elog-info org-canvas--logger "[Orphan] Scanning %s..." name)
        (let ((orphans (org-canvas--find-orphans-for-feature feature)))
          (when orphans
            (push (cons feature orphans) all-orphans)
            (setq total-orphans (+ total-orphans (length orphans)))
            (elog-info org-canvas--logger
              "[Orphan] %s: found %d orphan(s)" name (length orphans))))))
    (setq all-orphans (nreverse all-orphans))
    ;; Display results
    (if (= total-orphans 0)
        (progn
          (elog-info org-canvas--logger "[Orphan] No orphans found.")
          (message "No orphaned items found."))
      (let ((buf (org-canvas--orphan-format-buffer all-orphans)))
        (display-buffer buf)
        (when (yes-or-no-p
               (format "Found %d orphan(s).  Delete them from Canvas? " total-orphans))
          (org-canvas--orphan-delete-all all-orphans)
          (message "Orphan cleanup complete. %d item(s) deleted." total-orphans))))))

(provide 'org-canvas)
;;; org-canvas.el ends here
