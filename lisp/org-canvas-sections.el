;;; org-canvas-sections.el --- Pull sections from Canvas & sync overrides -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; The first cell says who the override is for, in one of three forms:
;;   - a link to a section heading in sections.org (its CANVAS_ID)
;;   - `Group: <title>'      a group heading in groups.org (its CANVAS_ID)
;;   - `Students: <name>; <name>'  person headings in people.org (USER_ID),
;;                            separated by `;' since sortable names carry
;;                            commas
;; A literal `#<id>' stands in for a title in the last two.  Date columns
;; are found by their header (Due At, Unlock At, Lock At), so a pulled
;; table that dropped an empty column still parses.  Overrides are synced
;; after sections, groups, people and assignments exist on Canvas.
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
(require 'cl-lib)

(declare-function org-canvas--validate-section-structure "org-canvas-validate")

;;;; Configuration

(defcustom org-canvas-sections-file (org-canvas--path "sections.org")
  "Path to the sections.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-sections-file "sections.org")
(org-canvas-register-properties "sections"
  :label "Sections"
  :file-var 'org-canvas-sections-file
  :query "LEVEL=1"
  :properties nil
  :structural-fn #'org-canvas--validate-section-structure)

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

(defun org-canvas--section-set-properties (pom id start-at end-at restrict)
  "Set section properties at POM from Canvas API data.
ID is the section's Canvas ID string.
START-AT, END-AT are ISO8601 timestamps (or nil).
RESTRICT is the restrict_enrollments_to_section_dates value."
  (org-canvas-org-set-property pom "CANVAS_ID" id)
  (org-canvas--pull-set-timestamp-property pom "START_AT" start-at)
  (org-canvas--pull-set-timestamp-property pom "END_AT" end-at)
  (org-canvas--pull-set-boolean-property pom "RESTRICT_TO_DATES" restrict))

(defun org-canvas--pull-sections-upsert (section)
  "Update or create a heading for SECTION in the current buffer.
Returns the symbol `created' or `updated'."
  (let* ((id (number-to-string (alist-get 'id section)))
         (name (alist-get 'name section))
         (start-at (alist-get 'start_at section))
         (end-at (alist-get 'end_at section))
         (restrict (alist-get 'restrict_enrollments_to_section_dates section))
         (marker (org-canvas--section-find-heading-by-id id)))
    (if marker
        (progn
          (goto-char (marker-position marker))
          (org-canvas--section-set-properties (point) id start-at end-at restrict)
          (org-canvas--log-info org-canvas--logger
                     "[Pull] Updated existing section '%s' (ID: %s)"
                     (org-get-heading t t t t) id)
          'updated)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" name))
      (org-back-to-heading t)
      (org-canvas--section-set-properties (point) id start-at end-at restrict)
      (org-canvas--log-info org-canvas--logger
                 "[Pull] Created new section '%s' (ID: %s)" name id)
      'created)))

(defun org-canvas--pull-sections-warn-stale (remote-ids)
  "Warn about local headings whose CANVAS_ID is not in REMOTE-IDS.
Must be called in the sections file buffer."
  (org-map-entries
   (lambda ()
     (let ((local-id (org-entry-get (point) "CANVAS_ID")))
       (when (and local-id (not (member local-id remote-ids)))
         (org-canvas--log-warning org-canvas--logger
                       "[Pull] Stale section '%s' (ID: %s) — no longer on Canvas"
                       (org-get-heading t t t t) local-id)
         (message "Warning: Stale section '%s' (ID: %s) not found on Canvas"
                  (org-get-heading t t t t) local-id))))
   "LEVEL=1" 'file))

(defun org-canvas--pull-sections-preflight ()
  "Validate sections file directory and confirm overwrite.
Returns the expanded sections file path."
  (let ((sections-file (expand-file-name org-canvas-sections-file)))
    (unless (file-directory-p (file-name-directory sections-file))
      (org-canvas--signal 'org-canvas-config-error
        "Sections file directory does not exist: %s"
        (file-name-directory sections-file)))
    (org-canvas--pull-confirm-overwrite sections-file "sections")
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> PULLING SECTIONS FROM CANVAS")
    (org-canvas--log-info org-canvas--logger "File: %s" sections-file)
    (org-canvas--log-info org-canvas--logger "========================================")
    sections-file))

;;;###autoload
(defun org-canvas-pull-sections ()
  "Pull course sections from Canvas into sections.org.
Fetches all sections via the API, then updates or creates headings
in the sections file.  Existing headings are matched by CANVAS_ID;
heading names are preserved (user may rename).  Warns about stale
local headings whose CANVAS_ID no longer exists on Canvas."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let* ((sections-file (org-canvas--pull-sections-preflight))
         (endpoint (org-canvas-api-course-endpoint "sections"))
           (remote-sections (org-canvas-api-request-all-pages 'GET endpoint))
           (remote-ids nil)
           (created 0) (updated 0)
           (was-fresh (org-canvas--pull-was-fresh-p sections-file)))

      (org-canvas--pull-confirm-unsaved sections-file "sections")
      (org-canvas--log-info org-canvas--logger "[Pull] Fetched %d sections from Canvas"
                 (length remote-sections))

      (unless (file-exists-p sections-file)
        (with-temp-file sections-file (insert "")))
      (with-current-buffer (org-canvas--find-file-noselect sections-file)
        (dolist (section remote-sections)
          (push (number-to-string (alist-get 'id section)) remote-ids)
          (let ((result (org-canvas--pull-sections-upsert section)))
            (if (eq result 'updated)
                (setq updated (1+ updated))
              (setq created (1+ created)))))

        (org-canvas--pull-sections-warn-stale remote-ids)
        (org-canvas--pull-write-file-header)
        (org-canvas--save-buffer))

      (org-canvas--pull-kill-fresh-buffer sections-file was-fresh)
      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger ">>> SECTION PULL COMPLETE")
      (org-canvas--log-info org-canvas--logger "Created: %d | Updated: %d | Total remote: %d"
                 created updated (length remote-sections))
      (org-canvas--log-info org-canvas--logger "========================================")
      (message "Section pull: %d created, %d updated." created updated)))

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
        (with-current-buffer (org-canvas--find-file-noselect abs-file)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward
                   (format "^\\*+ +%s" (regexp-quote heading)) nil t)
              (org-entry-get (point) "CANVAS_ID"))))))))

(defconst org-canvas--override-group-regexp "\\`Group:[ \t]*\\(.+?\\)[ \t]*\\'"
  "Match the first cell of a group override row; group 1 is the title or #id.")

(defconst org-canvas--override-students-regexp "\\`Students?:[ \t]*\\(.+?\\)[ \t]*\\'"
  "Match the first cell of a student override row; group 1 lists the people.")

(defun org-canvas--override-lookup-file (var)
  "Return the file VAR names when VAR is bound and the file exists, else nil.
The groups and people files belong to other feature modules, which
sections.el must not require; it reads them by name when they exist."
  (let ((file (and (boundp var) (symbol-value var))))
    (and file (file-exists-p file) file)))

(defun org-canvas--override-literal-id (text)
  "Return the digits of TEXT when it is a literal #123, else nil."
  (and (string-match "\\`#\\([0-9]+\\)\\'" text) (match-string 1 text)))

(defun org-canvas--override-resolve-group-id (text)
  "Resolve TEXT, a group heading title or #id, to a group's CANVAS_ID string."
  (or (org-canvas--override-literal-id text)
      (org-canvas--heading-property-by-title
       (org-canvas--override-lookup-file 'org-canvas-groups-file)
       text "CANVAS_ID" "LEVEL=2")))

(defun org-canvas--override-resolve-student-id (text)
  "Resolve TEXT, a person's heading title or #id, to a USER_ID string."
  (or (org-canvas--override-literal-id text)
      (org-canvas--heading-property-by-title
       (org-canvas--override-lookup-file 'org-canvas-people-file)
       text "USER_ID" "LEVEL=2")))

(defun org-canvas--override-parse-students (text)
  "Parse TEXT, names separated by `;', into a list of USER_ID strings.
Nil, with one warning naming what did not resolve, unless every name
resolves: an override for half the people named would be a surprise."
  (let* ((names (split-string text ";" t "[ \t]+"))
         (unresolved (cl-remove-if #'org-canvas--override-resolve-student-id names)))
    (cond
     ((null names) nil)
     (unresolved
      (org-canvas--log-warning org-canvas--logger
        "[Override] Could not resolve student(s) %s (pull people first, or write #id)"
        (mapconcat (lambda (n) (format "'%s'" n)) unresolved ", "))
      nil)
     (t (mapcar #'org-canvas--override-resolve-student-id names)))))

(defun org-canvas--override-parse-target (cell source-dir)
  "Parse CELL, the first cell of an override row, into who the override is for.
Returns (:section-id ID), (:group-id ID) or (:student-ids IDS), or
nil with one warning when the cell resolves to nothing.  SOURCE-DIR
is the directory of the file holding the table, for section links."
  (cond
   ((string-match org-canvas--override-group-regexp cell)
    (let* ((title (match-string 1 cell))
           (id (org-canvas--override-resolve-group-id title)))
      (if id
          (list :group-id id)
        (org-canvas--log-warning org-canvas--logger
          "[Override] Could not resolve group '%s' (pull groups first, or write #id)" title)
        nil)))
   ((string-match org-canvas--override-students-regexp cell)
    (let ((ids (org-canvas--override-parse-students (match-string 1 cell))))
      (and ids (list :student-ids ids))))
   (t
    (let ((id (org-canvas--override-resolve-section-id cell source-dir)))
      (if id
          (list :section-id id)
        (org-canvas--log-warning org-canvas--logger
          "[Override] Could not resolve section ID from: %s" cell)
        nil)))))

(defun org-canvas--section-link-by-id (section-id)
  "Return an Org file link to the section heading with CANVAS_ID = SECTION-ID.
Returns the section's heading wrapped in a
\"[[file:sections.org::*Name][Name]]\" form when found, the literal
\"All Sections\" when SECTION-ID is nil (Canvas overrides without a
`course_section_id' apply to anyone the assignment is published for),
or SECTION-ID coerced to a string if the section can't be resolved.

This is the inverse of `org-canvas--override-resolve-section-id': that
function turns a link into an ID; this one turns an ID into a link."
  (cond
   ((null section-id) "All Sections")
   (t
    (let ((sections-file (and (boundp 'org-canvas-sections-file)
                              org-canvas-sections-file))
          (target (if (numberp section-id)
                      (number-to-string section-id)
                    (format "%s" section-id))))
      (or (and sections-file
               (file-exists-p sections-file)
               (with-current-buffer (org-canvas--find-file-noselect sections-file)
                 (save-excursion
                   (goto-char (point-min))
                   (let (found name)
                     (while (and (not found)
                                 (re-search-forward
                                  "^[ \t]*:CANVAS_ID:[ \t]+\\(.+\\)$" nil t))
                       (when (string= (string-trim
                                       (match-string-no-properties 1))
                                      target)
                         (save-excursion
                           (org-back-to-heading t)
                           (setq name (org-get-heading t t t t))
                           (setq found t))))
                     (when found
                       (format "[[file:%s::*%s][%s]]"
                               (file-name-nondirectory sections-file)
                               name name))))))
          target)))))

(defun org-canvas--override-format-cell (iso8601)
  "Format ISO8601 as an Org timestamp string, or empty string if nil."
  (or (and iso8601 (org-canvas--iso8601-to-org-timestamp iso8601)) ""))

(defun org-canvas--override-fetch (assignment-id)
  "GET overrides for ASSIGNMENT-ID; return a list of override alists or nil.
Records to the pull-summary on error and returns nil.  Catches the
parent `org-canvas-error' so credentials/config/api errors all degrade
gracefully — without this, calling `org-canvas--assignment-pull-item'
in a test environment without credentials propagates an uncaught
`org-canvas-credentials-error' from `org-canvas--ensure-credentials',
which CI's coverage-instrumented runner handles poorly."
  (let ((url (org-canvas-api-course-endpoint
              "assignments/%s/overrides" assignment-id)))
    (condition-case err
        (let ((response (org-canvas-api-request-all-pages 'GET url)))
          (and response (append response nil)))
      (org-canvas-error
       (org-canvas--pull-summary-record
        :file (and (boundp 'org-canvas-assignments-file)
                   (file-name-nondirectory org-canvas-assignments-file))
        :item (format "assignment %s overrides" assignment-id)
        :error (error-message-string err))
       nil))))

(defun org-canvas--override-row-redundant-p (ov parent-due parent-unlock parent-lock)
  "Return non-nil when override OV conveys no date difference from parent.
PARENT-DUE/PARENT-UNLOCK/PARENT-LOCK are the parent assignment's
ISO8601 strings (or nil).  An override row is redundant when every
date it carries is either absent or identical to the parent's
corresponding date — emitting it would clutter the file with rows
that say nothing."
  (let ((due    (alist-get 'due_at    ov))
        (unlock (alist-get 'unlock_at ov))
        (lock   (alist-get 'lock_at   ov)))
    (and (or (null due)    (equal due    parent-due))
         (or (null unlock) (equal unlock parent-unlock))
         (or (null lock)   (equal lock   parent-lock)))))

(defun org-canvas--override-group-cell (group-id)
  "Render GROUP-ID as a `Group: <title>' cell, `Group: #id' when unresolved."
  (format "Group: %s"
          (or (org-canvas--heading-title-by-property
               (org-canvas--override-lookup-file 'org-canvas-groups-file)
               "CANVAS_ID" group-id "LEVEL=2")
              (format "#%s" group-id))))

(defun org-canvas--override-students-cell (student-ids)
  "Render STUDENT-IDS as a `Students: <name>; <name>' cell, `#id' when unresolved."
  (let ((file (org-canvas--override-lookup-file 'org-canvas-people-file)))
    (format "Students: %s"
            (mapconcat (lambda (id)
                         (or (org-canvas--heading-title-by-property
                              file "USER_ID" id "LEVEL=2")
                             (format "#%s" id)))
                       student-ids "; "))))

(defun org-canvas--override-target-cell (ov)
  "Render who override OV is for: a section link, a group, students, or everyone."
  (let ((students (append (alist-get 'student_ids ov) nil)))
    (cond
     ((alist-get 'course_section_id ov)
      (org-canvas--section-link-by-id (alist-get 'course_section_id ov)))
     ((alist-get 'group_id ov)
      (org-canvas--override-group-cell (alist-get 'group_id ov)))
     (students (org-canvas--override-students-cell students))
     (t "All Sections"))))

(defun org-canvas--override-build-row (ov)
  "Build a 4-cell display row for override OV.
Returns a list (TARGET DUE UNLOCK LOCK) of strings; TARGET is the
section link, `Group: ...' or `Students: ...' cell."
  (list (org-canvas--override-target-cell ov)
        (org-canvas--override-format-cell (alist-get 'due_at    ov))
        (org-canvas--override-format-cell (alist-get 'unlock_at ov))
        (org-canvas--override-format-cell (alist-get 'lock_at   ov))))

(defun org-canvas--override-emit-table
    (overrides &optional parent-due parent-unlock parent-lock)
  "Emit a `#+NAME: overrides' table for OVERRIDES (list of API alists).
Inserts at point.  Does nothing when OVERRIDES is nil/empty or when
every override is redundant against the parent dates.

PARENT-DUE, PARENT-UNLOCK, PARENT-LOCK are the parent assignment's
ISO8601 strings (or nil); rows whose every populated date matches the
parent are dropped (see `org-canvas--override-row-redundant-p').
Columns whose every cell is empty are dropped from the emitted table.
Section column renders as \"All Sections\" when `course_section_id'
is nil, otherwise as a `[[file:sections.org::*Name][Name]]' link."
  (when (and overrides (> (length overrides) 0))
    (let* ((kept-overrides
            (cl-remove-if (lambda (ov)
                            (org-canvas--override-row-redundant-p
                             ov parent-due parent-unlock parent-lock))
                          overrides))
           (rows (mapcar #'org-canvas--override-build-row kept-overrides))
           (keep-due    (cl-some (lambda (r) (not (string-empty-p (nth 1 r)))) rows))
           (keep-unlock (cl-some (lambda (r) (not (string-empty-p (nth 2 r)))) rows))
           (keep-lock   (cl-some (lambda (r) (not (string-empty-p (nth 3 r)))) rows)))
      (when rows
        (let ((headers (cons "Section"
                             (delq nil (list (and keep-due    "Due At")
                                             (and keep-unlock "Unlock At")
                                             (and keep-lock   "Lock At")))))
              (seps (cons "---------"
                          (delq nil (list (and keep-due    "--------")
                                          (and keep-unlock "-----------")
                                          (and keep-lock   "---------"))))))
          (insert "#+NAME: overrides\n")
          (insert (concat "| " (mapconcat #'identity headers " | ") " |\n"))
          (insert (concat "|" (mapconcat #'identity seps "+") "|\n"))
          (dolist (r rows)
            (let ((cells (cons (nth 0 r)
                               (delq nil
                                     (list (and keep-due    (nth 1 r))
                                           (and keep-unlock (nth 2 r))
                                           (and keep-lock   (nth 3 r)))))))
              (insert (concat "| " (mapconcat #'identity cells " | ") " |\n"))))
          (insert "\n"))))))

(defun org-canvas--override-parse-timestamp-cell (cell)
  "Parse a table CELL containing an Org timestamp.
Returns an ISO8601 string or nil for empty/whitespace cells."
  (let ((trimmed (string-trim cell)))
    (when (and (not (string-empty-p trimmed))
               (string-match-p "^<" trimmed))
      (org-canvas-org-parse-timestamp trimmed))))

(defun org-canvas--override-date-columns (header)
  "Return the column indexes of the due, unlock and lock cells from HEADER.
HEADER is the table's first row.  When it names any of the three
\(Due At, Unlock At, Lock At, case aside) each column is found by its
title and an unnamed one is absent (nil): a pulled table drops a
column no row fills, so position alone would read a lock date as a
due date.  A header naming none falls back to positions 1, 2 and 3."
  (let* ((titles (mapcar (lambda (cell) (downcase (string-trim cell))) header))
         (specs '(("due at" . 1) ("unlock at" . 2) ("lock at" . 3)))
         (named (cl-some (lambda (spec) (member (car spec) titles)) specs)))
    (mapcar (lambda (spec)
              (if named
                  (cl-position (car spec) titles :test #'string=)
                (cdr spec)))
            specs)))

(defun org-canvas--override-parse-table (table source-dir)
  "Parse an overrides TABLE into a list of override plists.
TABLE is the result of `org-table-to-lisp'.
SOURCE-DIR is the directory containing the assignments.org file.
Each plist names who the override is for, as :section-id, :group-id
or :student-ids (see `org-canvas--override-parse-target'), and
carries :due-at, :unlock-at and :lock-at as ISO8601 strings or nil.
A row that resolves to nothing is skipped with a warning."
  (let* ((header (car table))
         (columns (org-canvas--override-date-columns header))
         (overrides nil))
    (dolist (row (cdr table))
      (unless (eq row 'hline)
        (let ((target (org-canvas--override-parse-target
                       (string-trim (or (nth 0 row) "")) source-dir)))
          (when target
            (push (append target
                          (cl-mapcan (lambda (key col)
                                       (list key (org-canvas--override-parse-timestamp-cell
                                                  (or (and col (nth col row)) ""))))
                                     '(:due-at :unlock-at :lock-at) columns))
                  overrides)))))
    (nreverse overrides)))

(defun org-canvas--override-target-field (override)
  "Return the alist entry naming who OVERRIDE is for, as Canvas wants it.
One of `course_section_id', `group_id' or `student_ids' (a vector, so
it encodes as a JSON array)."
  (cond
   ((plist-get override :section-id)
    (cons 'course_section_id (string-to-number (plist-get override :section-id))))
   ((plist-get override :group-id)
    (cons 'group_id (string-to-number (plist-get override :group-id))))
   (t
    (cons 'student_ids (vconcat (mapcar #'string-to-number
                                        (plist-get override :student-ids)))))))

(defun org-canvas--override-build-payload (override)
  "Build a Canvas assignment_override payload from OVERRIDE plist."
  (let ((payload `((assignment_override
                    . (,(org-canvas--override-target-field override))))))
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

(defun org-canvas--override-existing-label (item)
  "Describe who the Canvas override ITEM is for, for a log line."
  (cond
   ((alist-get 'course_section_id item)
    (format "section %s" (alist-get 'course_section_id item)))
   ((alist-get 'group_id item) (format "group %s" (alist-get 'group_id item)))
   ((alist-get 'student_ids item)
    (format "students %s" (mapconcat (lambda (id) (format "%s" id))
                                     (append (alist-get 'student_ids item) nil) ", ")))
   (t "everyone")))

(defun org-canvas--override-delete-removed (endpoint existing matched-ids)
  "Delete overrides in EXISTING whose id is not in MATCHED-IDS.
MATCHED-IDS are the ids of the Canvas overrides a table row claimed;
an override is deleted only when the table no longer matches it by
kind, never for lacking a section id, which a student's or a group's
override lacks by nature (issue #224).  ENDPOINT is the overrides API
URL.  Returns the number deleted.  During a dry run the deletions are
logged but never issued."
  (let ((deleted 0))
    (dolist (item existing)
      (let ((item-id (alist-get 'id item))
            (label (org-canvas--override-existing-label item)))
        (unless (memq item-id matched-ids)
          (condition-case err
              (progn
                (org-canvas--log-debug org-canvas--logger
                            "[Override] Deleting override %s (%s no longer in table)"
                            item-id label)
                (if org-canvas--dry-run
                    (org-canvas--log-info org-canvas--logger
                      "[DRY-RUN] Would DELETE override %s (%s no longer in table)"
                      item-id label)
                  (org-canvas-api-request 'DELETE
                    (format "%s/%s" endpoint item-id)))
                (setq deleted (1+ deleted)))
            (error
             (org-canvas--log-error org-canvas--logger
                         "[Override] Delete failed for override %s: %s"
                         item-id (error-message-string err)))))))
    deleted))

(defun org-canvas--override-label (override)
  "Describe who the table OVERRIDE is for, for a log line."
  (cond
   ((plist-get override :section-id) (format "section %s" (plist-get override :section-id)))
   ((plist-get override :group-id) (format "group %s" (plist-get override :group-id)))
   (t (format "students %s" (mapconcat #'identity (plist-get override :student-ids) ", ")))))

(defun org-canvas--override-find-existing (override existing)
  "Return the Canvas override in EXISTING that the table OVERRIDE claims, or nil.
A section row matches by `course_section_id', a group row by
`group_id', and a student row by an identical set of `student_ids':
a changed set is a new override, and the old one is deleted."
  (cl-find-if
   (lambda (item)
     (cond
      ((plist-get override :section-id)
       (equal (alist-get 'course_section_id item)
              (string-to-number (plist-get override :section-id))))
      ((plist-get override :group-id)
       (equal (alist-get 'group_id item)
              (string-to-number (plist-get override :group-id))))
      (t
       (let ((theirs (append (alist-get 'student_ids item) nil))
             (ours (mapcar #'string-to-number (plist-get override :student-ids))))
         (and theirs
              (null (cl-set-exclusive-or theirs ours)))))))
   existing))

(defun org-canvas--override-push-one (endpoint override existing-override)
  "PUT OVERRIDE onto EXISTING-OVERRIDE, or POST it, at ENDPOINT.
Returns `updated' or `created'; an API error is logged and answered
with nil.  Under `org-canvas--dry-run' the write is logged, not sent."
  (let ((payload (org-canvas--override-build-payload override))
        (label (org-canvas--override-label override))
        (override-id (alist-get 'id existing-override)))
    (condition-case err
        (progn
          (cond
           ((and existing-override org-canvas--dry-run)
            (org-canvas--log-info org-canvas--logger
              "[DRY-RUN] Would UPDATE override %s for %s" override-id label))
           (existing-override
            (org-canvas--log-debug org-canvas--logger
              "[Override] Updating override %s for %s" override-id label)
            (org-canvas-api-request 'PUT (format "%s/%s" endpoint override-id)
                                    :data payload))
           (org-canvas--dry-run
            (org-canvas--log-info org-canvas--logger
              "[DRY-RUN] Would CREATE override for %s" label))
           (t
            (org-canvas--log-debug org-canvas--logger
              "[Override] Creating override for %s" label)
            (org-canvas-api-request 'POST endpoint :data payload)))
          (if existing-override 'updated 'created))
      (error
       (org-canvas--log-error org-canvas--logger
                              "[Override] Failed for %s: %s"
                              label (error-message-string err))
       nil))))

(defun org-canvas--override-fetch-existing (endpoint assignment-id)
  "Return the overrides on Canvas at ENDPOINT for ASSIGNMENT-ID, as a list.
A failed read is warned about and answered with nil: without the
existing overrides, reconcile treats the remote as empty and
re-creates everything, so the user should know."
  (condition-case err
      (append (org-canvas-api-request-all-pages 'GET endpoint) nil)
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Sections] Failed to fetch existing overrides for assignment %s (%s); treating remote as empty"
       assignment-id (error-message-string err))
     nil)))

(defun org-canvas--override-sync-for-assignment (assignment-id overrides)
  "Reconcile OVERRIDES for ASSIGNMENT-ID on Canvas.
OVERRIDES is a list of parsed override plists from
`org-canvas--override-parse-table'.
Fetches existing overrides from Canvas, then for each local override:
  - Updates the existing override it claims (PUT); a section row claims
    by `course_section_id', a group row by `group_id', a student row by
    an identical set of `student_ids'
  - Creates a new override when none is claimed (POST)
  - Deletes the remote overrides no row claimed (DELETE), by id
Returns a list (CREATED UPDATED DELETED) as integer counts.
Under `org-canvas--dry-run' the remote is still read but no write is
issued; the counts then report what would have been done."
  (let* ((endpoint (org-canvas-api-course-endpoint
                    "assignments/%s/overrides" assignment-id))
         (existing (org-canvas--override-fetch-existing endpoint assignment-id))
         (created 0) (updated 0)
         (matched-ids nil))
    (dolist (override overrides)
      (let* ((existing-override (org-canvas--override-find-existing override existing))
             (outcome (org-canvas--override-push-one endpoint override existing-override)))
        (when existing-override
          (push (alist-get 'id existing-override) matched-ids))
        (pcase outcome
          ('created (setq created (1+ created)))
          ('updated (setq updated (1+ updated))))))
    (list created updated
          (org-canvas--override-delete-removed endpoint existing matched-ids))))

(defun org-canvas--override-sync-preflight ()
  "Validate assignments file and log header for override sync.
Returns the expanded assignments file path."
  (let ((assignments-file (expand-file-name
                           (if (boundp 'org-canvas-assignments-file)
                               org-canvas-assignments-file
                             (org-canvas--path "assignments.org")))))
    (unless (file-exists-p assignments-file)
      (org-canvas--signal 'org-canvas-config-error
        "Assignments file not found: %s" assignments-file))
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> STARTING OVERRIDE SYNC")
    (org-canvas--log-info org-canvas--logger "File: %s" assignments-file)
    (org-canvas--log-info org-canvas--logger "========================================")
    assignments-file))

;;;###autoload
(defun org-canvas-sync-overrides ()
  "Sync per-section date overrides for all assignments.
Scans assignments.org for `#+NAME: overrides' tables and
reconciles them with Canvas assignment overrides."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let* ((assignments-file (org-canvas--override-sync-preflight))
         (source-dir (file-name-directory assignments-file))
          (total-created 0) (total-updated 0) (total-deleted 0)
          (assignments-processed 0))

      (with-current-buffer (org-canvas--find-file-noselect assignments-file)
        (let ((markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))
          (dolist (marker markers)
            (goto-char (marker-position marker))
            (let* ((canvas-id (org-canvas-org-get-property (point) "CANVAS_ID"))
                   (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
                   (end (save-excursion (org-end-of-subtree t) (point)))
                   (table (org-canvas--override-find-table end)))
              (when (and canvas-id table)
                (org-canvas--log-info org-canvas--logger
                           "[Override] Processing overrides for '%s' (ID: %s)"
                           title canvas-id)
                (let* ((overrides (org-canvas--override-parse-table table source-dir))
                       (counts (org-canvas--override-sync-for-assignment canvas-id overrides)))
                  (setq total-created (+ total-created (nth 0 counts)))
                  (setq total-updated (+ total-updated (nth 1 counts)))
                  (setq total-deleted (+ total-deleted (nth 2 counts)))
                  (setq assignments-processed (1+ assignments-processed))))))
          (dolist (m markers) (set-marker m nil))))

      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger ">>> OVERRIDE SYNC COMPLETE")
      (org-canvas--log-info org-canvas--logger "Assignments: %d | Created: %d | Updated: %d | Deleted: %d"
                 assignments-processed total-created total-updated total-deleted)
      (org-canvas--log-info org-canvas--logger "========================================")
      (let ((total (+ total-created total-updated total-deleted)))
        (org-canvas--sync-record-feature-stats "Overrides"
          (if org-canvas--dry-run
              (list :dry-run total)
            (list :success total))))
      (message "Override sync: %d assignments, %d created, %d updated, %d deleted.%s"
               assignments-processed total-created total-updated total-deleted
               (if org-canvas--dry-run " (dry run — nothing sent)" ""))))

(provide 'org-canvas-sections)
;;; org-canvas-sections.el ends here
