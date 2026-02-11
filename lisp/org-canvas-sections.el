;;; org-canvas-sections.el --- Pull sections from Canvas & sync overrides -*- lexical-binding: t; -*-

;;; Commentary:

;; This module pulls Canvas Course Sections into sections.org (read-only)
;; and syncs per-section assignment date overrides.
;;
;; Sections are created by Canvas when classes are cross-listed, not by
;; instructors.  This module fetches them for local reference and for
;; override resolution.  Headings may be renamed locally — override
;; links resolve via CANVAS_ID, not heading name.
;;
;; FILE STRUCTURE
;; ==============
;; In sections.org:
;;   - Level 1 headings = Course Sections (pulled from Canvas)
;;
;; PROPERTIES (set by pull, read-only)
;; ====================================
;; CANVAS_ID             - Canvas section ID
;; START_AT              - Section start date (Org timestamp)
;; END_AT                - Section end date (Org timestamp)
;; RESTRICT_TO_DATES     - Restrict enrollments to section dates ("true"/"false")
;; LAST_SYNCED           - Timestamp of last pull
;;
;; ASSIGNMENT OVERRIDES
;; ====================
;; Assignment headings in assignments.org may contain an optional
;; `#+NAME: overrides' table with per-section date overrides:
;;
;;   #+NAME: overrides
;;   | Section                                      | Due At           | Unlock At        | Lock At          |
;;   |----------------------------------------------+------------------+------------------+------------------|
;;   | [[file:sections.org::*Section A][Section A]] | <2026-02-15 Sun> | <2026-02-01 Sat> |                  |
;;
;; Each row links to a section in sections.org.  The section's CANVAS_ID
;; is resolved from that file.  Overrides are synced after both sections
;; and assignments exist on Canvas.
;;
;; API NOTES
;; =========
;; Sections (read-only pull):
;;   GET /courses/:id/sections         - list all sections
;;
;; Assignment Overrides:
;;   GET    /courses/:id/assignments/:aid/overrides     - list
;;   POST   /courses/:id/assignments/:aid/overrides     - create
;;   PUT    /courses/:id/assignments/:aid/overrides/:oid - update
;;   DELETE /courses/:id/assignments/:aid/overrides/:oid - delete

;;; Code:

(require 'org-canvas-core)
(require 'elog)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-sections-file (org-canvas--path "sections.org")
  "Path to the sections.org file."
  :type 'file
  :group 'org-canvas)

;;;; Pull Sections from Canvas

(defun org-canvas--section-find-heading-by-id (canvas-id)
  "Find heading with CANVAS_ID property matching CANVAS-ID in current buffer.
Return marker or nil."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get (point) "CANVAS_ID") canvas-id)
           (setq found (point-marker))))
       "LEVEL=1" 'file))
    found))

(defun org-canvas--section-format-timestamp (iso8601)
  "Convert ISO8601 timestamp from Canvas API to Org timestamp format.
Returns an Org active timestamp string, or nil if ISO8601 is nil."
  (when (and iso8601 (not (equal iso8601 :null)))
    (let ((time (date-to-time iso8601)))
      (format-time-string "<%Y-%m-%d %a %H:%M>" time t))))

(defun org-canvas--section-set-properties (pom id start-at end-at restrict)
  "Set section properties at POM from Canvas API data.
ID is the section's Canvas ID string.
START-AT, END-AT are ISO8601 timestamps (or nil).
RESTRICT is the restrict_enrollments_to_section_dates value."
  (org-canvas-org-set-property pom "CANVAS_ID" id)
  (when (org-canvas--section-format-timestamp start-at)
    (org-canvas-org-set-property
     pom "START_AT"
     (org-canvas--section-format-timestamp start-at)))
  (when (org-canvas--section-format-timestamp end-at)
    (org-canvas-org-set-property
     pom "END_AT"
     (org-canvas--section-format-timestamp end-at)))
  (org-canvas-org-set-property
   pom "RESTRICT_TO_DATES"
   (if (eq restrict t) "true" "false"))
  (org-canvas-org-set-property
   pom "LAST_SYNCED"
   (format-time-string "[%Y-%m-%d %a %H:%M]")))

;;;###autoload
(defun org-canvas-pull-sections ()
  "Pull course sections from Canvas into sections.org.
Fetches all sections via the API, then updates or creates headings
in the sections file.  Existing headings are matched by CANVAS_ID;
heading names are preserved (user may rename).  Warns about stale
local headings whose CANVAS_ID no longer exists on Canvas."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))

  (let ((sections-file (expand-file-name org-canvas-sections-file)))
    (unless (file-directory-p (file-name-directory sections-file))
      (error "Sections file directory does not exist: %s"
             (file-name-directory sections-file)))
    (when (and (file-exists-p sections-file)
               (> (file-attribute-size (file-attributes sections-file)) 0)
               (not (y-or-n-p
                     (format "%s already exists.  Pull will overwrite headings.  Continue? "
                             (file-name-nondirectory sections-file)))))
      (user-error "Sections pull aborted"))

    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> PULLING SECTIONS FROM CANVAS")
    (elog-info org-canvas--logger "File: %s" sections-file)
    (elog-info org-canvas--logger "========================================")

    (let* ((endpoint (org-canvas-api-course-endpoint "sections"))
           (remote-sections (append (org-canvas-api-request
                                     'GET endpoint
                                     :params org-canvas--api-max-per-page)
                                    nil))
           (remote-ids nil)
           (created 0) (updated 0))

      (elog-info org-canvas--logger "[Pull] Fetched %d sections from Canvas"
                 (length remote-sections))

      ;; Open or create the sections file
      (with-current-buffer (find-file-noselect sections-file)
        ;; Process each remote section
        (dolist (section remote-sections)
          (let* ((id (number-to-string (alist-get 'id section)))
                 (name (alist-get 'name section))
                 (start-at (alist-get 'start_at section))
                 (end-at (alist-get 'end_at section))
                 (restrict (alist-get 'restrict_enrollments_to_section_dates section))
                 (marker (org-canvas--section-find-heading-by-id id)))
            (push id remote-ids)

            (if marker
                ;; Update existing heading (preserve name)
                (progn
                  (goto-char (marker-position marker))
                  (org-canvas--section-set-properties (point) id start-at end-at restrict)
                  (elog-info org-canvas--logger
                             "[Pull] Updated existing section '%s' (ID: %s)"
                             (org-get-heading t t t t) id)
                  (setq updated (1+ updated)))

              ;; Create new heading at end of buffer
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (format "* %s\n" name))
              (org-back-to-heading t)
              (org-canvas--section-set-properties (point) id start-at end-at restrict)
              (elog-info org-canvas--logger
                         "[Pull] Created new section '%s' (ID: %s)" name id)
              (setq created (1+ created)))))

        ;; Warn about stale local headings
        (org-map-entries
         (lambda ()
           (let ((local-id (org-entry-get (point) "CANVAS_ID")))
             (when (and local-id (not (member local-id remote-ids)))
               (elog-warning org-canvas--logger
                             "[Pull] Stale section '%s' (ID: %s) — no longer on Canvas"
                             (org-get-heading t t t t) local-id)
               (message "Warning: Stale section '%s' (ID: %s) not found on Canvas"
                        (org-get-heading t t t t) local-id))))
         "LEVEL=1" 'file)

        ;; Save the buffer
        (save-buffer))

      (elog-info org-canvas--logger "========================================")
      (elog-info org-canvas--logger ">>> SECTION PULL COMPLETE")
      (elog-info org-canvas--logger "Created: %d | Updated: %d | Total remote: %d"
                 created updated (length remote-sections))
      (elog-info org-canvas--logger "========================================")
      (message "Section pull: %d created, %d updated." created updated))))

;;;; ================================================================
;;;; Assignment Overrides
;;;; ================================================================

(defun org-canvas--override-find-table (end)
  "Search for a `#+NAME: overrides' table between point and END.
Return parsed table via `org-table-to-lisp', or nil if not found."
  (save-excursion
    (when (re-search-forward "^#\\+NAME:[ \t]+overrides[ \t]*$" end t)
      (forward-line 1)
      (when (looking-at-p org-table-line-regexp)
        (org-table-to-lisp)))))

(defun org-canvas--override-resolve-section-id (link-string source-dir)
  "Resolve an Org file link to a section's CANVAS_ID.
LINK-STRING should be in the form [[file:sections.org::*Heading][Display]].
SOURCE-DIR is the directory of the file containing the link.
Follows the link to find the heading and reads its CANVAS_ID property.
Returns the CANVAS_ID string, or nil if the link cannot be resolved."
  (when (string-match "\\[\\[file:\\(.+?\\)::\\*\\(.+?\\)\\]\\[.+?\\]\\]" link-string)
    (let* ((file (match-string 1 link-string))
           (heading (match-string 2 link-string))
           (abs-file (expand-file-name file source-dir)))
      (when (file-exists-p abs-file)
        (with-current-buffer (find-file-noselect abs-file)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward
                   (format "^\\*+ +%s" (regexp-quote heading)) nil t)
              (org-entry-get (point) "CANVAS_ID"))))))))

(defun org-canvas--override-parse-timestamp-cell (cell)
  "Parse a table CELL containing an Org timestamp.
Returns an ISO8601 string or nil for empty/whitespace cells."
  (let ((trimmed (string-trim cell)))
    (when (and (not (string-empty-p trimmed))
               (string-match-p "^<" trimmed))
      (org-canvas-org-parse-timestamp trimmed))))

(defun org-canvas--override-parse-table (table source-dir)
  "Parse an overrides TABLE into a list of override plists.
TABLE is the result of `org-table-to-lisp'.
SOURCE-DIR is the directory containing the assignments.org file.
Returns a list of plists: (:section-id ID :due-at TS :unlock-at TS :lock-at TS)."
  (let ((rows (cdr table))  ; skip header row
        (overrides nil))
    (dolist (row rows)
      (unless (eq row 'hline)
        (let* ((section-cell (string-trim (nth 0 row)))
               (due-cell     (string-trim (nth 1 row)))
               (unlock-cell  (string-trim (nth 2 row)))
               (lock-cell    (string-trim (nth 3 row)))
               (section-id (org-canvas--override-resolve-section-id
                            section-cell source-dir)))
          (when section-id
            (push (list :section-id section-id
                        :due-at (org-canvas--override-parse-timestamp-cell due-cell)
                        :unlock-at (org-canvas--override-parse-timestamp-cell unlock-cell)
                        :lock-at (org-canvas--override-parse-timestamp-cell lock-cell))
                  overrides))
          (unless section-id
            (elog-warning org-canvas--logger
                          "[Override] Could not resolve section ID from: %s" section-cell)))))
    (nreverse overrides)))

(defun org-canvas--override-build-payload (override)
  "Build a Canvas assignment_override payload from OVERRIDE plist."
  (let ((payload `((assignment_override
                    . ((course_section_id . ,(string-to-number
                                             (plist-get override :section-id))))))))
    (when (plist-get override :due-at)
      (push `(due_at . ,(plist-get override :due-at))
            (alist-get 'assignment_override payload)))
    (when (plist-get override :unlock-at)
      (push `(unlock_at . ,(plist-get override :unlock-at))
            (alist-get 'assignment_override payload)))
    (when (plist-get override :lock-at)
      (push `(lock_at . ,(plist-get override :lock-at))
            (alist-get 'assignment_override payload)))
    payload))

(defun org-canvas--override-sync-for-assignment (assignment-id overrides)
  "Reconcile OVERRIDES for ASSIGNMENT-ID on Canvas.
OVERRIDES is a list of parsed override plists from
`org-canvas--override-parse-table'.
Fetches existing overrides from Canvas, then for each local override:
  - Updates the existing override if one matches by course_section_id (PUT)
  - Creates a new override if none matches (POST)
  - Deletes remote overrides whose section_id is absent from the table (DELETE)
Returns a list (CREATED UPDATED DELETED) as integer counts."
  (let* ((endpoint (org-canvas-api-course-endpoint
                    "assignments/%s/overrides" assignment-id))
         (existing (condition-case nil
                       (append (org-canvas-api-request 'GET endpoint) nil)
                     (error nil)))
         (created 0) (updated 0) (deleted 0)
         (seen-section-ids nil))

    ;; Process each override from the table
    (dolist (override overrides)
      (let* ((section-id (string-to-number (plist-get override :section-id)))
             (payload (org-canvas--override-build-payload override))
             (existing-override
              (cl-find-if (lambda (item)
                            (equal (alist-get 'course_section_id item) section-id))
                          existing)))
        (push section-id seen-section-ids)

        (condition-case err
            (if existing-override
                ;; Update existing override
                (let ((override-id (alist-get 'id existing-override)))
                  (elog-debug org-canvas--logger
                              "[Override] Updating override %s for section %s"
                              override-id section-id)
                  (org-canvas-api-request 'PUT
                    (format "%s/%s" endpoint override-id) :data payload)
                  (setq updated (1+ updated)))
              ;; Create new override
              (elog-debug org-canvas--logger
                          "[Override] Creating override for section %s" section-id)
              (org-canvas-api-request 'POST endpoint :data payload)
              (setq created (1+ created)))
          (error
           (elog-error org-canvas--logger
                       "[Override] Failed for section %s: %s"
                       section-id (error-message-string err))))))

    ;; Delete overrides whose section_id is not in the table
    (dolist (item existing)
      (let ((item-section-id (alist-get 'course_section_id item))
            (item-id (alist-get 'id item)))
        (unless (memq item-section-id seen-section-ids)
          (condition-case err
              (progn
                (elog-debug org-canvas--logger
                            "[Override] Deleting override %s (section %s no longer in table)"
                            item-id item-section-id)
                (org-canvas-api-request 'DELETE
                  (format "%s/%s" endpoint item-id))
                (setq deleted (1+ deleted)))
            (error
             (elog-error org-canvas--logger
                         "[Override] Delete failed for override %s: %s"
                         item-id (error-message-string err)))))))

    (list created updated deleted)))

(defun org-canvas-sync-overrides ()
  "Sync per-section date overrides for all assignments.
Scans assignments.org for `#+NAME: overrides' tables and
reconciles them with Canvas assignment overrides."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))

  (let ((assignments-file (expand-file-name
                           (if (boundp 'org-canvas-assignments-file)
                               org-canvas-assignments-file
                             (org-canvas--path "assignments.org")))))
    (unless (file-exists-p assignments-file)
      (error "Assignments file not found: %s" assignments-file))

    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> STARTING OVERRIDE SYNC")
    (elog-info org-canvas--logger "File: %s" assignments-file)
    (elog-info org-canvas--logger "========================================")

    (let ((source-dir (file-name-directory assignments-file))
          (total-created 0) (total-updated 0) (total-deleted 0)
          (assignments-processed 0))

      (with-current-buffer (find-file-noselect assignments-file)
        (let ((markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))
          (dolist (marker markers)
            (goto-char (marker-position marker))
            (let* ((canvas-id (org-canvas-org-get-property (point) "CANVAS_ID"))
                   (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
                   (end (save-excursion (org-end-of-subtree t) (point)))
                   (table (org-canvas--override-find-table end)))
              (when (and canvas-id table)
                (elog-info org-canvas--logger
                           "[Override] Processing overrides for '%s' (ID: %s)"
                           title canvas-id)
                (let* ((overrides (org-canvas--override-parse-table table source-dir))
                       (counts (org-canvas--override-sync-for-assignment canvas-id overrides)))
                  (setq total-created (+ total-created (nth 0 counts)))
                  (setq total-updated (+ total-updated (nth 1 counts)))
                  (setq total-deleted (+ total-deleted (nth 2 counts)))
                  (setq assignments-processed (1+ assignments-processed))))))))

      (elog-info org-canvas--logger "========================================")
      (elog-info org-canvas--logger ">>> OVERRIDE SYNC COMPLETE")
      (elog-info org-canvas--logger "Assignments: %d | Created: %d | Updated: %d | Deleted: %d"
                 assignments-processed total-created total-updated total-deleted)
      (elog-info org-canvas--logger "========================================")
      (message "Override sync: %d assignments, %d created, %d updated, %d deleted."
               assignments-processed total-created total-updated total-deleted))))

(provide 'org-canvas-sections)
;;; org-canvas-sections.el ends here
