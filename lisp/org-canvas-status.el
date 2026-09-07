;;; org-canvas-status.el --- Local sync status overview -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-status' reads the course's Org files and reports what they
;; say about themselves: how many headings carry a Canvas id, how many
;; are still pending, and when each file was last synced.  It makes no
;; API calls; `org-canvas-diff' is the command that asks Canvas.
;;
;; `org-canvas--status-content-types' is also the table the master
;; delete and pull-all commands read to count what a run would touch.

;;; Code:

(require 'org-canvas-core)

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

;;;; Status Overview

(defun org-canvas--status-count-entries (file id-prop)
  "Count synced and pending entries in FILE using ID-PROP.
Returns a plist (:synced N :pending N :legacy N :last-synced TS-OR-NIL).
LAST-SYNCED reads the file-level #+LAST_SYNCED header.  LEGACY counts
entries with a Canvas ID but missing a file header (i.e., the file has
no #+LAST_SYNCED, indicating it has not been re-pulled since the
schema cutover)."
  (let ((synced 0) (pending 0) (legacy 0) (last-synced nil))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (setq last-synced (org-canvas--pull-read-file-header))
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let ((id (org-entry-get (point) id-prop)))
             (if id
                 (progn
                   (setq synced (1+ synced))
                   (unless last-synced (setq legacy (1+ legacy))))
               (setq pending (1+ pending)))))
         "LEVEL=1" 'file)))
    (list :synced synced :pending pending :legacy legacy :last-synced last-synced)))

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
               (legacy (plist-get counts :legacy))
               (last-synced (plist-get counts :last-synced))
               (unsaved (buffer-modified-p (find-buffer-visiting file))))
          (insert (format " (%s)%s\n"
                          (file-name-nondirectory file)
                          (if unsaved " [unsaved]" "")))
          (insert (format "  Synced: %-4d  Pending: %-4d" synced pending))
          (when (and legacy (> legacy 0))
            (insert (format "  Legacy: %d (synced before change tracking)" legacy)))
          (when last-synced
            (insert (format "  Last: %s" last-synced)))
          (insert "\n"))))))

;;;###autoload
(defun org-canvas-status ()
  "Display sync status overview for all content types.
Reads the Org files only — it reports what they say about themselves,
not what Canvas currently holds.  Use `org-canvas-diff' for that."
  (interactive)
  (let ((buf (get-buffer-create "*canvas-status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "org-canvas Sync Status\n")
        (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
        (when org-canvas--active-course-name
          (insert (format "Active course: %s\n" org-canvas--active-course-name)))
        (insert (make-string 60 ?=))
        (insert "\n")
        (dolist (entry org-canvas--status-content-types)
          (org-canvas--status-report-file
           buf (car entry) (cadr entry) (caddr entry)))
        (insert (format "\n%s\n" (make-string 60 ?=)))
        (insert "This is a local view: it reads the Org files and makes no API calls.\n")
        (insert "Use M-x org-canvas-diff to compare against Canvas.\n")
        (insert "Use M-x org-canvas-sync to sync, M-x org-canvas-sync-dry-run to preview.\n"))
      (special-mode))
    (display-buffer buf)))

(provide 'org-canvas-status)
;;; org-canvas-status.el ends here
