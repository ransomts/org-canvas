;;; org-canvas.el --- Sync Org Mode files with Canvas LMS  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Tim Ransom <ransomtim8078@gmail.com>
;; Maintainer: Tim Ransom <ransomtim8078@gmail.com>
;; Created: 2026-02-08
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (plz "0.9") (org "9.6") (transient "0.4"))

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
;; The token may instead come from auth-source, keyed on the host and
;; course, so it can stay in an encrypted ~/.authinfo.gpg:
;;
;;   machine canvas.instructure.com login 12345 password your-token
;;
;; SEE ALSO
;; ========
;; - documentation/manual.org for full documentation
;; - example-course/ for sample file formats

;;; Code:

(require 'lisp-mnt)

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
(require 'org-canvas-diff)
(require 'org-canvas-validate)
(require 'org-canvas-submissions)
(require 'org-canvas-setup)

;; Import command files: built on core and the feature modules, never
;; required by them.  Each is one user-facing job the orchestrator used
;; to carry (issue #142).
(require 'org-canvas-status)
(require 'org-canvas-publish)
(require 'org-canvas-adopt)
(require 'org-canvas-orphans)
(when (require 'transient nil t)
  (require 'org-canvas-transient))

;; Note: Feature-specific file paths (e.g., `org-canvas-rubrics-file`) are now
;; defined in their respective modules.

;;;###autoload
(defun org-canvas-version (&optional show)
  "Return the org-canvas version string.
When called interactively, or when SHOW is non-nil, also display the
version via `message'.  The version is read from this file's
\"Version:\" header at runtime so a single source of truth in the
package header survives byte-compilation."
  ;; Note: `(interactive "p")' rather than `(interactive (list t))'.
  ;; A sexp argument to `interactive' makes edebug bail on the defun, which
  ;; in turn blocks undercover from instrumenting the body.  The string
  ;; form passes the prefix arg (always 1 when called with no prefix), which
  ;; is truthy and triggers the same `message' branch.
  (interactive "p")
  (let* ((file (or (locate-library "org-canvas")
                   (error "Cannot locate org-canvas source file")))
         (source (replace-regexp-in-string "\\.elc\\'" ".el" file))
         (version (with-temp-buffer
                    (insert-file-contents source)
                    (or (lm-version) "unknown"))))
    (when show
      (message "org-canvas %s" version))
    version))

(defconst org-canvas--bug-report-settings
  '(org-canvas-base-url
    org-canvas-course-id
    org-canvas-directory
    org-canvas-request-timeout
    org-canvas-upload-timeout
    org-canvas-rate-limit-retries
    org-canvas-rate-limit-wait
    org-canvas-detect-conflicts
    org-canvas-log-level
    org-canvas-log-destination
    org-canvas-log-file
    org-canvas-log-request-bodies)
  "Settings that `org-canvas-submit-bug-report' includes verbatim.
`org-canvas-api-token' is redacted separately; secrets must never be
added to this list.")

(defun org-canvas--bug-report-format-setting (sym)
  "Return a single-line \"  NAME: VALUE\" string for SYM."
  (format "  %s: %S" sym (and (boundp sym) (symbol-value sym))))

;;;###autoload
(defun org-canvas-submit-bug-report ()
  "Open a buffer prepopulated with diagnostic info for a bug report.
The buffer contains the org-canvas version, the host Emacs version,
relevant configuration, and the list of currently loaded org-canvas
modules.  `org-canvas-api-token' is redacted.  Edit the description
above the marker, then file an issue at the project URL."
  (interactive)
  (let ((buf (get-buffer-create "*org-canvas-bug-report*"))
        (modules (sort (mapcar #'symbol-name
                               (seq-filter
                                (lambda (f)
                                  (string-prefix-p "org-canvas" (symbol-name f)))
                                features))
                       #'string<)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Describe the bug above this line, then file at:\n"
                "  https://github.com/ransomts/org-canvas/issues\n\n"
                "------------------- system info (do not edit) -------------------\n"
                (format "org-canvas version : %s\n" (org-canvas-version))
                (format "Emacs version      : %s\n" emacs-version)
                (format "System type        : %s\n" system-type)
                (format "System config      : %s\n" system-configuration)
                "\nConfiguration:\n")
        (dolist (sym org-canvas--bug-report-settings)
          (insert (org-canvas--bug-report-format-setting sym) "\n"))
        (insert (format "  org-canvas-api-token: %s\n"
                        (cond ((org-canvas--nonempty-string-p org-canvas-api-token)
                               "[redacted]")
                              ((ignore-errors (org-canvas--api-token))
                               "[redacted, resolved via auth-source]")
                              (t "[unset]"))))
        (insert "\nLoaded modules:\n")
        (dolist (m modules)
          (insert "  " m "\n")))
      (text-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun org-canvas--safe-sync (sync-fn label)
  "Call SYNC-FN, catching file-not-found errors gracefully.
LABEL is used for logging (e.g., \"Pages\")."
  (condition-case err
      (funcall sync-fn)
    (error
     (let ((msg (error-message-string err)))
       (if (string-match-p "file not found\\|no such file" (downcase msg))
           (org-canvas--log-info org-canvas--logger
             "[Skip] %s: %s\n  To import from Canvas: M-x org-canvas-pull-%s\n  To create skeleton files: M-x org-canvas-init"
             label msg (downcase (replace-regexp-in-string " " "-" label)))
         (org-canvas--log-error org-canvas--logger "[FAILED] %s: %s" label msg))))))

(defun org-canvas--tier-description (tier)
  "Return a comma-separated string of labels in TIER."
  (mapconcat #'cadr tier ", "))

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
        (org-canvas--sync-in-progress t)
        (org-canvas--sync-global-counters
         (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
        (org-canvas--sync-global-feature-stats nil)
        (org-canvas--module-items-pending nil))
    (org-canvas--log-info org-canvas--logger "========================================")
    (if org-canvas--dry-run
        (org-canvas--log-info org-canvas--logger ">>> DRY RUN — no changes will be made")
      (org-canvas--log-info org-canvas--logger ">>> STARTING GLOBAL SYNC"))
    (org-canvas--log-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--preflight-check)
    ;; Apply the release schedule first: publishing a module edits objects
    ;; owned by other features, which sync in earlier tiers.
    (unless org-canvas--dry-run
      (org-canvas-apply-scheduled-releases))
    ;; Tiers -1 and 0
    (org-canvas--log-info org-canvas--logger "--- Tier -1: Settings ---")
    (message "Syncing: Settings...")
    (org-canvas--run-tier (nth 0 org-canvas--sync-tiers) #'org-canvas--safe-sync)
    (org-canvas--log-info org-canvas--logger "--- Tier 0: %s ---"
      (org-canvas--tier-description (nth 1 org-canvas--sync-tiers)))
    (message "Syncing: %s..." (org-canvas--tier-description (nth 1 org-canvas--sync-tiers)))
    (org-canvas--run-tier (nth 1 org-canvas--sync-tiers) #'org-canvas--safe-sync)
    (org-canvas--log-info org-canvas--logger
      "[Note] Same-tier cross-references (e.g., page→page) may require a second sync to fully resolve")
    ;; Tiers 1 through 2
    (let ((tier-num 1))
      (dolist (tier (nthcdr 2 org-canvas--sync-tiers))
        (org-canvas--log-info org-canvas--logger "--- Tier %s: %s ---"
          (pcase tier-num (1 "1") (2 "1.5") (3 "1.75") (4 "2"))
          (org-canvas--tier-description tier))
        (message "Syncing: %s..." (org-canvas--tier-description tier))
        (org-canvas--run-tier tier #'org-canvas--safe-sync)
        (setq tier-num (1+ tier-num))))
    ;; Heal module items skipped earlier whose targets have IDs by now
    (org-canvas--module-retry-pending-items)
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> GLOBAL SYNC COMPLETE")
    (org-canvas--sync-log-global-summary)
    (org-canvas--log-info org-canvas--logger "========================================")
    ;; Clear session-scoped caches
    (setq org-canvas--image-cache nil)
    (if (> (plist-get org-canvas--sync-global-counters :dry-run) 0)
        (message "Dry-run complete: %d would sync, %d skipped. See *canvas-log* for details."
                 (plist-get org-canvas--sync-global-counters :dry-run)
                 (plist-get org-canvas--sync-global-counters :skip))
      (let ((fail-count (plist-get org-canvas--sync-global-counters :fail))
            (deferred-count (or (plist-get org-canvas--sync-global-counters
                                           :deferred)
                                0)))
        (message "%sSync complete: %d synced, %d skipped, %d failed.%s%s"
                 (if (> fail-count 0) "WARNING: " "")
                 (plist-get org-canvas--sync-global-counters :success)
                 (plist-get org-canvas--sync-global-counters :skip)
                 fail-count
                 (if (> deferred-count 0)
                     (format " %d deferred (will apply on a future sync)."
                             deferred-count)
                   "")
                 (if (> fail-count 0)
                     " Check *canvas-log* for error details."
                   ""))))))

;; Delete in REVERSE dependency order:
;;   Tier 2:  Modules (reference all content types)
;;   Tier 1:  Assignments, Quizzes (may reference Tier 0)
;;   Tier 0:  Everything else (no deps, safe to delete last)
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
  "Delete tiers in reverse dependency order.
Each tier is a list of (FN LABEL) pairs.")

;;;###autoload
(defun org-canvas-delete-all ()
  "Delete ALL Canvas content for this course.
This is a destructive operation that removes all synced content from Canvas.
Deletion order is reverse of sync order to respect dependencies."
  (interactive)
  (let ((synced-count 0))
    (dolist (entry org-canvas--status-content-types)
      (let ((file-var (cadr entry))
            (id-prop (caddr entry)))
        (when (and (boundp file-var)
                   (file-exists-p (expand-file-name (symbol-value file-var))))
          (let ((counts (org-canvas--status-count-entries
                         (expand-file-name (symbol-value file-var)) id-prop)))
            (setq synced-count (+ synced-count (plist-get counts :synced)))))))
    (unless (yes-or-no-p
             (format "WARNING: This will DELETE ALL content from Canvas course %s (~%d synced items).  Are you sure? "
                     org-canvas-course-id synced-count))
      (user-error "Aborted")))
  (unless (yes-or-no-p "This cannot be undone.  Do you wish to continue? ")
    (user-error "Aborted"))
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((org-canvas--inhibit-log-clear t))
    (org-canvas--log-info org-canvas--logger "Starting Global Delete...")
    (dolist (tier org-canvas--delete-tiers)
      (org-canvas--run-tier
       tier (lambda (fn label)
              (org-canvas--log-info org-canvas--logger "[Delete] %s..." label)
              (funcall fn))))
    (org-canvas--log-info org-canvas--logger "Global Delete Complete.")
    (message "Global Delete Complete. See *canvas-log*.")))

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

;;;; Pull One Heading
;;
;; `org-canvas-diff' names the items Canvas changed behind your back, and
;; until now nothing accepted one of them: whole-file pull would write a
;; heading for every item the course holds, and the only single-item pull
;; was answering `l' at a conflict prompt during a sync you may not want
;; to run yet (issue #67).  The hard part already existed —
;; `org-canvas--conflict-pull-local' — and simply had no caller outside
;; conflict resolution.

(defun org-canvas--pull-at-point-feature ()
  "Return the feature entry for the current buffer's file.
Signals a `user-error' when the buffer is not a course file, or when
its feature has no pull-item function to refresh a heading with."
  (let* ((file (buffer-file-name))
         (feature (org-canvas--registry-feature-for-file file)))
    (unless feature
      (user-error "%s is not one of this course's Canvas files"
                  (if file (file-name-nondirectory file) "This buffer")))
    (unless (plist-get feature :pull-item-fn)
      (user-error "%s has no single-item pull; use M-x org-canvas-pull-%s"
                  (plist-get feature :name)
                  (downcase (replace-regexp-in-string
                             " " "-" (plist-get feature :name)))))
    feature))

(defun org-canvas--pull-at-point-1 (feature id title)
  "Overwrite the heading at point with FEATURE's Canvas item ID.
TITLE names the heading in the log."
  (let* ((endpoint (org-canvas--feature-item-url feature id))
         (remote (org-canvas-api-request 'GET endpoint)))
    (org-canvas--conflict-pull-local
     (list :pom (point-marker)) remote (plist-get feature :pull-item-fn))
    (org-canvas--log-info org-canvas--logger
      "[Pull] Refreshed '%s' from Canvas (%s %s)"
      title (plist-get feature :name) id)
    (message "Pulled '%s' from Canvas." title)))

;;;###autoload
(defun org-canvas-pull-at-point ()
  "Replace the heading at point with Canvas's version of it.
Accepts one Canvas-side edit — the kind `org-canvas-diff' reports —
without pulling the whole file.  Fetches just this item and hands it to
the same code the conflict prompt's pull option uses: the heading is
renamed, its properties and body are rewritten, CANVAS_UPDATED_AT is
stamped from the remote, and the now-stale PAYLOAD_HASH is dropped so
the next sync does not see drift that is no longer there.

Local edits to this heading are discarded, which is the point; you are
asked to confirm first."
  (interactive)
  (org-back-to-heading t)
  (let* ((feature (org-canvas--pull-at-point-feature))
         (id-property (or (plist-get feature :id-property) "CANVAS_ID"))
         (id (org-entry-get (point) id-property))
         (title (org-get-heading t t t t)))
    (unless id
      (user-error "'%s' has no %s — nothing on Canvas to pull from" title
                  id-property))
    (when (org-canvas--confirm
           (format "Replace '%s' with the version on Canvas? " title))
      (org-canvas--pull-at-point-1 feature id title))))

;;;; Pull All (Canvas → Org Migration)

(defvar org-canvas--pull-counters nil
  "When non-nil, a plist accumulating counts across pull modules.
Bound by `org-canvas-pull-all' to aggregate pull/fail totals.")

(defun org-canvas--safe-pull (pull-fn label)
  "Call PULL-FN, catching errors gracefully.
LABEL is used for logging."
  (condition-case err
      (progn
        (funcall pull-fn)
        (when org-canvas--pull-counters
          (plist-put org-canvas--pull-counters :success
                     (1+ (plist-get org-canvas--pull-counters :success)))))
    (error
     (org-canvas--log-warning org-canvas--logger "[Pull] %s failed: %s"
       label (error-message-string err))
     (when org-canvas--pull-counters
       (plist-put org-canvas--pull-counters :fail
                  (1+ (plist-get org-canvas--pull-counters :fail)))))))

;; Pull in dependency order:
;;   Settings, then structural items, then linked items, then modules
(defconst org-canvas--pull-tiers
  '(((org-canvas-pull-settings "Settings"))
    ((org-canvas-pull-sections "Sections")
     (org-canvas-pull-files "Files")
     (org-canvas-pull-assignment-groups "Assignment Groups")
     (org-canvas-pull-group-categories "Group Categories")
     (org-canvas-pull-outcomes "Outcomes")
     (org-canvas-pull-rubrics "Rubrics")
     (org-canvas-pull-pages "Pages")
     (org-canvas-pull-discussions "Discussions")
     (org-canvas-pull-announcements "Announcements")
     (org-canvas-pull-calendar-events "Calendar Events"))
    ((org-canvas-pull-assignments "Assignments")
     (org-canvas-pull-quizzes "Quizzes")
     (org-canvas-pull-new-quizzes "New Quizzes"))
    ((org-canvas-pull-modules "Modules")))
  "Pull tiers in dependency order.  Each tier is a list of (FN LABEL) pairs.
Within tier 2, `pull-files' runs early so its CANVAS_ID -> path map is
available when later modules (pages, assignments, etc.) rewrite Canvas
file URLs in their HTML bodies via `org-canvas--pull-insert-body'.")

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
  ;; Warn about existing content that will be overwritten
  (let ((existing 0))
    (dolist (entry org-canvas--status-content-types)
      (let ((file-var (cadr entry)))
        (when (and (boundp file-var)
                   (file-exists-p (expand-file-name (symbol-value file-var))))
          (let ((counts (org-canvas--status-count-entries
                         (expand-file-name (symbol-value file-var))
                         (caddr entry))))
            (setq existing (+ existing (plist-get counts :synced)))))))
    (when (> existing 0)
      (unless (yes-or-no-p
               (format "Pull will overwrite %d existing local headings.  Continue? " existing))
        (user-error "Aborted"))))
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (org-canvas--pull-summary-reset)
  (let ((org-canvas--inhibit-log-clear t)
        (org-canvas--pull-counters (list :success 0 :fail 0)))
    (unwind-protect
        (progn
          (org-canvas--log-info org-canvas--logger "========================================")
          (org-canvas--log-info org-canvas--logger ">>> STARTING FULL COURSE PULL")
          (org-canvas--log-info org-canvas--logger "Course: %s | URL: %s"
            org-canvas-course-id org-canvas-base-url)
          (org-canvas--log-info org-canvas--logger "========================================")
          (org-canvas--preflight-check)
          ;; Resolve course TZ from any pre-existing settings.org so
          ;; timestamps emitted by tier-0 pulls (before settings is
          ;; refreshed) localize correctly.  `org-canvas-pull-settings'
          ;; calls this again after writing the new file.
          (org-canvas--pull-resolve-tz)
          (dolist (tier org-canvas--pull-tiers)
            (message "Pulling: %s..." (org-canvas--tier-description tier))
            (org-canvas--run-tier tier #'org-canvas--safe-pull))
          (org-canvas--log-info org-canvas--logger "========================================")
          (org-canvas--log-info org-canvas--logger ">>> FULL COURSE PULL COMPLETE")
          (org-canvas--log-info org-canvas--logger "========================================")
          (message "Pull complete: %d pulled, %d failed. See *canvas-log* for details."
                   (plist-get org-canvas--pull-counters :success)
                   (plist-get org-canvas--pull-counters :fail)))
      (unless (org-canvas--pull-summary-empty-p)
        (with-output-to-temp-buffer "*org-canvas-pull-summary*"
          (org-canvas--pull-summary-print))
        (message "Pull complete: %s - see *org-canvas-pull-summary*."
                 (org-canvas--pull-summary-tally))))))

(provide 'org-canvas)
;;; org-canvas.el ends here
