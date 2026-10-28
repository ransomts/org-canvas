;;; org-canvas-new-semester.el --- Carry a copied course into its next semester  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-new-semester' turns a copied course directory into the
;; next semester's course (issue #547): it retargets
;; org-canvas-credentials.el at the new course id and strips every sync
;; stamp the registries name from every Org file, so the first sync of
;; the new semester creates each item instead of updating the old
;; course's.  A dry run by default: the report names every heading it
;; would strip and the new course id, and nothing is written until the
;; user confirms.  Dates and section names stay with the editor.
;;
;; A one-shot local transform, not a sync: it sends nothing to Canvas,
;; so it is deliberately absent from `org-canvas-dry-run--sync-commands'
;; (that list exists to keep every command that writes to Canvas
;; previewable; this one has no write path to Canvas at all).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-canvas-core)

;;;; What Gets Stripped

(defun org-canvas--semester-strip-property-names ()
  "Return every sync stamp a semester rollover strips from a heading.
The same registry union a delete clears,
`org-canvas--delete-cleared-property-names' (the pipeline's own
property names, each feature's `:id-property' and every stamp
`org-canvas-register-id-property' names, issue #331), plus the
pull's CANVAS_DELETED mark: a delete need not clear that one, since
stamping a fresh id clears it, but a rollover writes no fresh id, and
a heading still marked deleted is skipped by the next push (issue
#392).  A stamp registered after the sed this command replaces was
written joins the union the moment its module loads (issue #547)."
  (delete-dups
   (cons org-canvas--prop-canvas-deleted
         (org-canvas--delete-cleared-property-names))))

(defun org-canvas--semester-org-files (directory)
  "Return DIRECTORY's Org files: every file named *.org directly in it.
Dotfiles (Emacs lockfiles above all) are left alone and
subdirectories are not followed, matching the reach of the sed the
workflow used to run."
  (seq-remove (lambda (file)
                (string-prefix-p "." (file-name-nondirectory file)))
              (directory-files directory t "\\.org\\'")))

(defun org-canvas--semester-heading-stamps ()
  "Return the strip-able stamp properties the heading at point carries."
  (seq-filter (lambda (prop) (org-entry-get (point) prop))
              (org-canvas--semester-strip-property-names)))

(defun org-canvas--semester-header-p ()
  "Return non-nil when the current buffer carries a #+LAST_SYNCED header."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^#\\+LAST_SYNCED:" nil t)))

(defun org-canvas--semester-file-plan (file)
  "Return FILE's piece of the rollover plan, a plist.
:file is FILE, :header non-nil when FILE carries a #+LAST_SYNCED
line, and :entries one (TITLE . STAMPS) for each heading carrying a
strip-able stamp.  Reading only: the dry run is built from plans."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (list :file file
          :header (org-canvas--semester-header-p)
          :entries (delq nil (org-map-entries
                              (lambda ()
                                (let ((stamps (org-canvas--semester-heading-stamps)))
                                  (when stamps
                                    (cons (org-get-heading t t t t) stamps))))
                              nil 'file)))))

(defun org-canvas--semester-plan-heading-count (plan)
  "Return how many headings PLAN strips."
  (apply #'+ (mapcar (lambda (entry)
                       (length (plist-get entry :entries)))
                     plan)))

;;;; Applying the Rollover

(defun org-canvas--semester-delete-header ()
  "Delete the #+LAST_SYNCED header line in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+LAST_SYNCED:.*\n?" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun org-canvas--semester-strip-file (file)
  "Strip FILE's sync stamps and #+LAST_SYNCED header, and save it.
Org's own property API does the editing, the same `org-entry-delete'
the delete machinery uses, so drawers, bodies and spacing are left as
their author wrote them.  Markers are collected before any buffer
change (Hard Rule 9)."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (org-with-wide-buffer
     (dolist (marker (org-map-entries #'point-marker nil 'file))
       (dolist (prop (org-canvas--semester-strip-property-names))
         (org-entry-delete marker prop))
       (set-marker marker nil)))
    (org-canvas--semester-delete-header)
    (org-canvas--save-buffer)))

(defun org-canvas--semester-retarget-setq (var value)
  "In the current buffer, point VAR's setq line at VALUE.
The string on the existing (setq VAR ...) line is replaced and
everything around it on the line kept; a file with no such line gets
one appended, which load-time evaluation honours all the same.  The
credentials file is Elisp its user may have edited by hand, so this is
a line edit, not a regeneration: the token never passes through a
rewritten file's neighbours."
  (goto-char (point-min))
  (let ((pattern (format "^(setq[ \t]+%s[ \t]+\"\\(?:[^\"\\]\\|\\\\.\\)*\""
                         (symbol-name var))))
    (if (not (re-search-forward pattern nil t))
        (progn (goto-char (point-max))
               (unless (bolp) (insert "\n"))
               (insert (format "(setq %s %S)\n" var value)))
      (replace-match (format "(setq %s %S" var value) t t))))

(defun org-canvas--semester-retarget-credentials (directory course-id)
  "Rewrite DIRECTORY's org-canvas-credentials.el for the new semester.
The course id becomes COURSE-ID and, where the file pins one, the
directory becomes DIRECTORY: a copy's credentials still name the
semester the directory was copied from.  Token, base URL, read-only
flag and hand edits are kept.  Return the credentials file, or nil
when DIRECTORY has none, in which case only the caller's session
retarget applies."
  (let ((file (expand-file-name "org-canvas-credentials.el" directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-canvas--semester-retarget-setq
         'org-canvas-directory (file-name-as-directory directory))
        (org-canvas--semester-retarget-setq 'org-canvas-course-id course-id)
        (write-region (point-min) (point-max) file nil 'silent))
      file)))

(defun org-canvas--semester-apply (directory course-id files)
  "Apply the rollover: strip FILES, retarget credentials, load them.
DIRECTORY is the copied course directory and COURSE-ID the new
course.  The session ends on the new course: the rewritten
credentials file is loaded after the Org files are stripped, the
token cache and course zone are forgotten the way
`org-canvas-activate-course' forgets them, and the directory watcher
recomputes every registered file path."
  (dolist (file files)
    (org-canvas--semester-strip-file file))
  (let ((credentials (org-canvas--semester-retarget-credentials
                      directory course-id)))
    (setq org-canvas-directory directory
          org-canvas-course-id course-id)
    (when credentials
      (load credentials nil t t))
    (org-canvas--api-token-forget)
    (org-canvas--time-zone-reset)
    credentials))

;;;; The Report

(defun org-canvas--semester-credentials-line (file course-id directory)
  "Return the report line naming what happens to credentials FILE.
COURSE-ID and DIRECTORY are the retarget the line names."
  (if (file-exists-p file)
      (format "Credentials: %s — course ID %s, directory %s\n"
              (file-name-nondirectory file) course-id directory)
    (format "Credentials: no org-canvas-credentials.el in %s — only this session's course ID is set\n"
            directory)))

(defun org-canvas--semester-file-section (entry)
  "Return ENTRY's report section, the empty string for a file with nothing to strip."
  (let ((entries (plist-get entry :entries))
        (header (plist-get entry :header)))
    (if (and (null entries) (not header))
        ""
      (concat
       (format "%s%s\n"
               (file-name-nondirectory (plist-get entry :file))
               (if header " — drops its #+LAST_SYNCED header" ""))
       (mapconcat (lambda (entry)
                    (format "  - %s: %s\n"
                            (car entry) (string-join (cdr entry) ", ")))
                  entries "")))))

(defun org-canvas--semester-report (directory course-id plan)
  "Return the rollover's report text for DIRECTORY, COURSE-ID and PLAN.
PLAN holds one `org-canvas--semester-file-plan' entry per Org file.
The report is the dry run the user reads before confirming: every
heading the rollover would strip, the #+LAST_SYNCED headers it would
drop, the new course ID, and what becomes of the credentials file."
  (let ((credentials (expand-file-name "org-canvas-credentials.el" directory))
        (sections (mapconcat #'org-canvas--semester-file-section plan ""))
        (headings 0)
        (headers 0))
    (dolist (entry plan)
      (setq headers (+ headers (if (plist-get entry :header) 1 0))
            headings (+ headings (length (plist-get entry :entries)))))
    (concat
     "org-canvas New Semester\n\n"
     (format "Course directory: %s\n" directory)
     (format "New course ID: %s\n" course-id)
     (org-canvas--semester-credentials-line credentials course-id directory)
     (format "\nWould strip sync state from %d heading(s) and drop %d #+LAST_SYNCED header(s):\n\n"
             headings headers)
     (if (string-empty-p sections)
         "Nothing to strip; only the credentials retarget applies.\n"
       sections)
     "\nNothing is written until you confirm.")))

;;;; The Command

(defun org-canvas--semester-default-directory ()
  "Return the initial default directory for the rollover prompt.
The active course directory when one is set, else `default-directory'."
  (if (and (stringp org-canvas-directory)
           (not (string-empty-p org-canvas-directory)))
      org-canvas-directory
    default-directory))

;;;###autoload
(defun org-canvas-new-semester ()
  "Carry a copied course directory into its next semester (issue #547).
Prompt for the copied directory and the new Canvas course ID, print
every heading whose sync stamps it would strip together with the new
course ID, and write nothing until confirmed.  On confirmation,
strip every registry-known stamp (CANVAS_ID, CANVAS_URL, the
companion id stamps a child heading carries, CANVAS_UPDATED_AT,
PAYLOAD_HASH, the legacy per-entry LAST_SYNCED and the CANVAS_DELETED
mark) from every Org file in the directory, drop each file's
#+LAST_SYNCED header, retarget org-canvas-credentials.el at the new
course ID and directory, and load it, so this session points at the
new course.  Content is otherwise untouched: dates and section names
are the editor's.  Sends nothing to Canvas."
  (interactive)
  (let* ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name
                       "Course directory (the copy) to roll over: "
                       (org-canvas--semester-default-directory) nil t))))
         (course-id (string-trim (read-string "New Canvas course ID: "))))
    (when (string-empty-p course-id)
      (user-error "Course ID cannot be empty"))
    (let* ((files (org-canvas--semester-org-files directory))
           (plan (mapcar #'org-canvas--semester-file-plan files))
           (report (org-canvas--semester-report directory course-id plan))
           (headings (org-canvas--semester-plan-heading-count plan)))
      (org-canvas--report-display "*canvas-new-semester*"
        (lambda () (insert report)) #'special-mode)
      (when (org-canvas--confirm
             (format "Strip the sync state of %d heading(s) and retarget the course to %s? "
                     headings course-id))
        (org-canvas--semester-apply directory course-id files)
        (org-canvas--user-message
          "New semester ready: %d heading(s) stripped; fix the dates and section names, then validate and sync"
          headings))
      report)))

(provide 'org-canvas-new-semester)
;;; org-canvas-new-semester.el ends here
