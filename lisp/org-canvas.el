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
(require 'org-canvas-quizzes)
(require 'org-canvas-rubrics)
(require 'org-canvas-sections)
(require 'org-canvas-validate)

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

;;;###autoload
(defun org-canvas-sync ()
  "Sync all enabled Canvas features."
  (interactive)
  (when org-canvas--sync-in-progress
    (user-error "A sync is already in progress.  Please wait for it to finish"))
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))
  (let ((org-canvas--inhibit-log-clear t)
        (org-canvas--sync-in-progress t))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> STARTING GLOBAL SYNC")
    (elog-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
    (elog-info org-canvas--logger "========================================")

    ;; Preflight: validate credentials and connection
    (org-canvas--preflight-check)

    ;; ----------------------------------------------------------------
    ;; Sync in dependency order (see documentation/manual.org for details)
    ;; ----------------------------------------------------------------

    ;; Tier 0: No dependencies - these can be synced in any order
    (org-canvas--safe-sync #'org-canvas-sync-outcomes "Outcomes")
    (org-canvas--safe-sync #'org-canvas-sync-rubrics "Rubrics")
    (org-canvas--safe-sync #'org-canvas-sync-assignment-groups "Assignment Groups")
    (org-canvas--safe-sync #'org-canvas-pull-sections "Sections")
    (org-canvas--safe-sync #'org-canvas-sync-files "Files")
    (org-canvas--safe-sync #'org-canvas-sync-pages "Pages")
    (org-canvas--safe-sync #'org-canvas-sync-discussions "Discussions")
    (org-canvas--safe-sync #'org-canvas-sync-announcements "Announcements")

    ;; Note: Same-tier cross-references (e.g., page→page) may not fully
    ;; resolve on first sync since the target may not have a CANVAS_ID yet.
    ;; A second sync will resolve them.
    (elog-info org-canvas--logger
      "[Note] Same-tier cross-references (e.g., page→page) may require a second sync to fully resolve")

    ;; Tier 1: Depends on Tier 0 - these may link to Tier 0 items
    (org-canvas--safe-sync #'org-canvas-sync-quizzes "Quizzes")
    (org-canvas--safe-sync #'org-canvas-sync-assignments "Assignments")

    ;; Tier 1.5: Re-sync assignment groups to apply drop rules
    ;; Canvas rejects drop rules for empty groups, so we create groups first,
    ;; sync assignments into them, then update groups with drop rules.
    (org-canvas--safe-sync #'org-canvas-sync-assignment-groups "Assignment Groups")

    ;; Tier 1.75: Overrides need both sections and assignments synced
    (org-canvas--safe-sync #'org-canvas-sync-overrides "Overrides")

    ;; Tier 2: Depends on all - modules reference all content types
    (org-canvas--safe-sync #'org-canvas-sync-modules "Modules")

    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> GLOBAL SYNC COMPLETE")
    (elog-info org-canvas--logger "========================================")
    (message "Sync complete. See *canvas-log* for details.")))

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
  (display-buffer (get-buffer-create "*canvas-log*"))
  (let ((org-canvas--inhibit-log-clear t))
    (elog-info org-canvas--logger "Starting Global Delete...")

    ;; ----------------------------------------------------------------
    ;; Delete in REVERSE dependency order
    ;; Items that reference others must be deleted first
    ;; ----------------------------------------------------------------

    ;; Tier 2 first: Modules reference all content types
    (elog-info org-canvas--logger "[Delete] Modules...")
    (org-canvas-delete-all-modules)

    ;; Tier 1: Assignments and Quizzes may reference Tier 0 items
    (elog-info org-canvas--logger "[Delete] Assignments...")
    (org-canvas-delete-all-assignments)
    (elog-info org-canvas--logger "[Delete] Quizzes...")
    (org-canvas-delete-all-quizzes)

    ;; Tier 0: No dependencies - safe to delete last
    (elog-info org-canvas--logger "[Delete] Files...")
    (org-canvas-delete-all-files)
    (elog-info org-canvas--logger "[Delete] Announcements...")
    (org-canvas-delete-all-announcements)
    (elog-info org-canvas--logger "[Delete] Discussions...")
    (org-canvas-delete-all-discussions)
    (elog-info org-canvas--logger "[Delete] Pages...")
    (org-canvas-delete-all-pages)
    (elog-info org-canvas--logger "[Delete] Assignment Groups...")
    (org-canvas-delete-all-assignment-groups)
    (elog-info org-canvas--logger "[Delete] Rubrics...")
    (org-canvas-delete-all-rubrics)
    (elog-info org-canvas--logger "[Delete] Outcomes...")
    (org-canvas-delete-all-outcomes)

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
    ("Sections"          org-canvas-sections-file          "CANVAS_ID"))
  "Content types for status reporting: (label file-var id-property).")

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

;;;; Dry-Run Preview

;;;###autoload
(defun org-canvas-sync-dry-run ()
  "Preview what a full sync would do without contacting the API.
No properties are modified and no API requests are sent."
  (interactive)
  (let ((org-canvas--dry-run t))
    (org-canvas-sync)))

(provide 'org-canvas)
;;; org-canvas.el ends here
