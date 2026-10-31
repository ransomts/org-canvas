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
;; PROPERTIES (typed by you, never pulled or pushed)
;; =================================================
;; MEETS                 - When the section meets, e.g. "MWF 10:10-11:00"
;;                         or "Tue 09:05-09:55; Thu 10:10-11:00"; the
;;                         window `org-canvas-submissions-score-by-window'
;;                         scores a column against when the assignment's
;;                         overrides table gives the section none (#383).
;;                         A pull sets only the properties above, so it
;;                         keeps MEETS as typed.
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
(require 'iso8601)

(declare-function org-canvas--validate-section-structure "org-canvas-validate-content")

;;;; Configuration

(defcustom org-canvas-sections-file (org-canvas--path "sections.org")
  "Path to the sections.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-sections-file "sections.org")
(org-canvas-register-web-pages
 "Sections" 'org-canvas-sections-file
 '(:level 1 :id-property "CANVAS_ID" :path "sections/%s"))
(org-canvas-register-properties "sections"
  :label "Sections"
  :file-var 'org-canvas-sections-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "MEETS" :data-key :meets :type string
     :doc "When the section meets (\"MWF 10:10-11:00\", several joined by \";\"); typed locally, never pulled or pushed, read by the section-window scoring of a grading file"))
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
  (org-canvas--pull-set-boolean-property
   pom "RESTRICT_TO_DATES" restrict "sections"))

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

      (org-canvas--create-file sections-file)
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

(defconst org-canvas--override-emit-columns
  '(("Due At" "--------" 1) ("Unlock At" "-----------" 2)
    ("Lock At" "---------" 3) ("Accept" "--------" 4))
  "The columns a pulled table may carry after Section: title, rule, cell.
The cell is the index in a row of `org-canvas--override-emit-row'.")

(defun org-canvas--override-emit-row (ov parent-due parent-unlock parent-lock
                                         accept-fn)
  "Return the cells of a pulled row for override OV, or nil to leave it out.
The four of `org-canvas--override-build-row', then the Accept cell
ACCEPT-FN returns for OV (empty when it is nil or returns nil).  A
row redundant against PARENT-DUE, PARENT-UNLOCK and PARENT-LOCK is
left out unless it keeps an Accept cell, which a pull never drops
\(issue #538)."
  (let ((accept (or (and accept-fn (funcall accept-fn ov)) "")))
    (unless (and (string-empty-p accept)
                 (org-canvas--override-row-redundant-p
                  ov parent-due parent-unlock parent-lock))
      (append (org-canvas--override-build-row ov) (list accept)))))

(defun org-canvas--override-emit-table
    (overrides &optional parent-due parent-unlock parent-lock accept-fn)
  "Emit a `#+NAME: overrides' table for OVERRIDES (list of API alists).
Inserts at point.  Does nothing when OVERRIDES is nil/empty or when
every override is redundant against the parent dates.

PARENT-DUE, PARENT-UNLOCK, PARENT-LOCK are the parent assignment's
ISO8601 strings (or nil); rows whose every populated date matches the
parent are dropped (see `org-canvas--override-row-redundant-p').
ACCEPT-FN, when given, returns the Accept cell to keep for an
override, which the table the pull replaces held
\(`org-canvas--override-match-accepts', issue #538).
Columns whose every cell is empty are dropped from the emitted table.
Section column renders as \"All Sections\" when `course_section_id'
is nil, otherwise as a `[[file:sections.org::*Name][Name]]' link."
  (when (and overrides (> (length overrides) 0))
    (let* ((rows (delq nil (mapcar (lambda (ov)
                                     (org-canvas--override-emit-row
                                      ov parent-due parent-unlock parent-lock
                                      accept-fn))
                                   overrides)))
           (columns (cl-remove-if-not
                     (lambda (column)
                       (cl-some (lambda (r) (not (string-empty-p (nth (nth 2 column) r))))
                                rows))
                     org-canvas--override-emit-columns)))
      (when rows
        (insert "#+NAME: overrides\n")
        (insert (concat "| " (mapconcat #'identity
                                        (cons "Section" (mapcar #'car columns))
                                        " | ")
                        " |\n"))
        (insert (concat "|" (mapconcat #'identity
                                       (cons "---------" (mapcar #'cadr columns))
                                       "+")
                        "|\n"))
        (dolist (r rows)
          (insert (concat "| "
                          (mapconcat #'identity
                                     (cons (car r)
                                           (mapcar (lambda (column)
                                                     (nth (nth 2 column) r))
                                                   columns))
                                     " | ")
                          " |\n")))
        (insert "\n")))))

;;;; Accept Cells Survive a Pull (issue #538)

(defun org-canvas--override-accept-column (header)
  "Return the index of HEADER's Accept column, or nil without one.
The cell holds what `org-canvas-validate' accepts on the row; the
push reads its columns by title and never this one."
  (cl-position "accept" header
               :test (lambda (title cell)
                       (string= title (downcase (string-trim cell))))))

(defun org-canvas--override-normalize-target (target)
  "Return TARGET, a (KEY ID-OR-IDS) list, its ids as sorted strings."
  (when target
    (let ((ids (cadr target)))
      (list (car target)
            (if (listp ids)
                (sort (mapcar (lambda (id) (format "%s" id)) ids) #'string<)
              (format "%s" ids))))))

(defun org-canvas--override-ov-key (ov)
  "Return who override OV is for, as a key the table's rows resolve to."
  (let ((students (append (alist-get 'student_ids ov) nil)))
    (org-canvas--override-normalize-target
     (cond
      ((alist-get 'course_section_id ov)
       (list :section-id (alist-get 'course_section_id ov)))
      ((alist-get 'group_id ov) (list :group-id (alist-get 'group_id ov)))
      (students (list :student-ids students))))))

(defun org-canvas--override-row-accept (row column)
  "Return (TARGET ACCEPT) for override ROW with text at COLUMN, else nil.
TARGET is its first cell as written and ACCEPT the cell's text."
  (when (listp row)
    (let ((accept (string-trim (or (nth column row) ""))))
      (unless (string-empty-p accept)
        (list (string-trim (or (nth 0 row) "")) accept)))))

(defun org-canvas--override-read-accepts ()
  "Return the Accept cells of the overrides table under the heading at point.
A list of (TARGET ACCEPT), one per row whose Accept cell holds text
\(`org-canvas--override-row-accept'); nil without such a column."
  (let* ((end (save-excursion (org-end-of-subtree t) (point)))
         (table (org-canvas--override-find-table end))
         (column (and table (org-canvas--override-accept-column (car table)))))
    (when column
      (delq nil (mapcar (lambda (row) (org-canvas--override-row-accept row column))
                        (cdr table))))))

(defun org-canvas--override-match-accepts (overrides entries source-dir)
  "Pair OVERRIDES with ENTRIES, the Accept cells of the table a pull rewrote.
Return (PAIRS . LEFT): PAIRS an alist of (OVERRIDE . ACCEPT), LEFT the
entries no override claimed.  An entry is claimed first by its row's
first cell as the pull writes it, then, for those left, by whom that
cell resolves to against SOURCE-DIR, so a row written by hand keeps
its reason too (issue #538)."
  (let ((left entries) (pairs nil) (unmatched nil))
    (dolist (ov overrides)
      (if-let* ((entry (assoc (org-canvas--override-target-cell ov) left)))
          (progn (push (cons ov (cadr entry)) pairs)
                 (setq left (delq entry left)))
        (push ov unmatched)))
    (when (and unmatched left)
      (let ((keyed (mapcar (lambda (entry)
                             (cons (org-canvas--override-normalize-target
                                    (org-canvas--override-parse-target
                                     (car entry) source-dir))
                                   entry))
                           left)))
        (dolist (ov (nreverse unmatched))
          (when-let* ((key (org-canvas--override-ov-key ov))
                      (hit (assoc key keyed)))
            (push (cons ov (cadr (cdr hit))) pairs)
            (setq keyed (delq hit keyed)
                  left (delq (cdr hit) left))))))
    (cons pairs left)))

(defun org-canvas--override-warn-unclaimed-accepts (entries title)
  "Log each of ENTRIES, Accept cells no pulled override of TITLE claimed.
Their rows are gone because Canvas no longer holds the override; the
log keeps the reason, which the file no longer can."
  (dolist (entry entries)
    (org-canvas--log-warning org-canvas--logger
      "[Override] Pull of '%s' dropped row '%s' (Canvas holds no such override); its Accept cell was: %s"
      title (car entry) (cadr entry))))

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

(defun org-canvas--override-unclaimed (existing matched-ids)
  "Return the overrides of EXISTING whose id is not in MATCHED-IDS."
  (cl-remove-if (lambda (item) (memq (alist-get 'id item) matched-ids))
                existing))

(defun org-canvas--override-describe-unclaimed (items)
  "Describe ITEMS, Canvas overrides no table row claims, for a prompt."
  (mapconcat (lambda (item)
               (format "%s%s"
                       (org-canvas--override-existing-label item)
                       (if (alist-get 'due_at item)
                           (format " due %s" (alist-get 'due_at item))
                         "")))
             items "; "))

(defvar org-canvas--override-prune nil
  "When non-nil, a push by heading deletes the overrides its table lacks.
Bound by `push --prune-overrides' for that run, the way `delete --yes'
answers its own question (issue #566).  A student's extension is still
not recoverable, so the flag is the consent: without it an interactive
push asks, and a batch push keeps them (issue #411).")

(defun org-canvas--override-confirm-deletes (items)
  "Ask whether a push by heading may delete ITEMS, overrides its table lacks.
Answers t without asking when ITEMS is empty, or when
`org-canvas--override-prune' is non-nil — the user said
`--prune-overrides' (issue #566).  Otherwise asks with `y-or-n-p'
whatever `org-canvas-assume-yes' says, since a student's extension is
not recoverable from Org once gone; under `noninteractive' answers nil,
keeping them, so a batch push of one heading never deletes an override
no one has seen (issue #411).  A dry run asks nothing and answers as
the same batch push would, keeping them: the only other answer a live
push can get is a prompt nobody has answered yet (issue #566)."
  (cond
   ((null items) t)
   (org-canvas--override-prune t)
   ((or noninteractive org-canvas--dry-run)
    (org-canvas--log-warning org-canvas--logger
      "[Override] Keeping %d override(s) Canvas holds that the table lacks (%s): a push of one heading deletes none in a batch Emacs or under a dry run; drop the rows and push interactively, or push --prune-overrides (issue #566)"
      (length items) (org-canvas--override-describe-unclaimed items))
    nil)
   (t (y-or-n-p (format "Delete %d override(s) Canvas holds that this table lacks (%s)? "
                        (length items)
                        (org-canvas--override-describe-unclaimed items))))))

(defun org-canvas--override-delete-removed (endpoint existing matched-ids
                                                     &optional confirm)
  "Delete overrides in EXISTING whose id is not in MATCHED-IDS.
MATCHED-IDS are the ids of the Canvas overrides a table row claimed;
an override is deleted only when the table no longer matches it by
kind, never for lacking a section id, which a student's or a group's
override lacks by nature (issue #224).  ENDPOINT is the overrides API
URL.  Returns (DELETED . KEPT): the number deleted, and the number
kept because CONFIRM was non-nil and `org-canvas--override-confirm-deletes'
said no (issue #411).  During a dry run the deletions are logged but
never issued, and the confirm answers as the same batch push would, so
a dry run reports the kept overrides the real push would leave
\(issue #566)."
  (let ((deleted 0)
        (unclaimed (org-canvas--override-unclaimed existing matched-ids)))
    (when (and confirm unclaimed
               (not (org-canvas--override-confirm-deletes unclaimed)))
      (org-canvas--log-info org-canvas--logger
        "[Override] %d override(s) Canvas holds kept, not deleted" (length unclaimed))
      (setq existing nil))
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
    (cons deleted (if existing 0 (length unclaimed)))))

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

(defun org-canvas--override-same-time-p (ours theirs)
  "Return non-nil when OURS and THEIRS, ISO8601 strings or nil, agree.
Both blank agree; one blank does not, since a PUT that omits a date
clears it.  Two strings agree when they name the same instant, however
each spells its zone.  A string that does not parse agrees with
nothing, so the row is sent as before."
  (let ((a (and (stringp ours) (not (string-empty-p ours)) ours))
        (b (and (stringp theirs) (not (string-empty-p theirs)) theirs)))
    (if (and a b)
        ;; `iso8601-valid-p' first: Emacs 29's `date-to-time' reads an
        ;; unparseable string outside UTC as 1999-12-31, so two of them
        ;; would agree.
        (and (iso8601-valid-p a) (iso8601-valid-p b)
             (ignore-errors (time-equal-p (date-to-time a) (date-to-time b))))
      (not (or a b)))))

(defun org-canvas--override-dates-match-p (override existing-override)
  "Return non-nil when EXISTING-OVERRIDE already carries OVERRIDE's dates.
OVERRIDE is a parsed table row, EXISTING-OVERRIDE the Canvas override it
claims.  Every one of the three dates is compared, a blank cell against
an absent one, because the PUT sends only the dates the row fills and
Canvas clears the ones it omits: a match means the PUT would change
nothing (issue #380)."
  (cl-every (lambda (pair)
              (org-canvas--override-same-time-p
               (plist-get override (car pair))
               (alist-get (cdr pair) existing-override)))
            '((:due-at . due_at) (:unlock-at . unlock_at) (:lock-at . lock_at))))

(defun org-canvas--override-reconcile-one (endpoint override existing-override)
  "Push OVERRIDE at ENDPOINT unless EXISTING-OVERRIDE already matches it.
Return `unchanged' without a request when the claimed override carries
the row's dates (issue #380), else what `org-canvas--override-push-one'
returns."
  (if (and existing-override
           (org-canvas--override-dates-match-p override existing-override))
      (progn
        (org-canvas--log-debug org-canvas--logger
          "[Override] Override %s for %s unchanged; not sent"
          (alist-get 'id existing-override) (org-canvas--override-label override))
        'unchanged)
    (org-canvas--override-push-one endpoint override existing-override)))

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

(defun org-canvas--override-sync-for-assignment (assignment-id overrides
                                                              &optional confirm)
  "Reconcile OVERRIDES for ASSIGNMENT-ID on Canvas.
OVERRIDES is a list of parsed override plists from
`org-canvas--override-parse-table'.
Fetches existing overrides from Canvas, then for each local override:
  - Updates the existing override it claims (PUT); a section row claims
    by `course_section_id', a group row by `group_id', a student row by
    an identical set of `student_ids'.  A claimed override that already
    carries the row's dates is left alone and counted nowhere (#380)
  - Creates a new override when none is claimed (POST)
  - Deletes the remote overrides no row claimed (DELETE), by id — after
    asking, when CONFIRM is non-nil (issue #411), or without asking
    when `org-canvas--override-prune' says the user asked for it
    (issue #566)
Returns a plist (:created N :updated N :deleted N :kept N), KEPT the
overrides Canvas holds that no row claims and the reconcile did
not delete: always 0 without CONFIRM.
Under `org-canvas--dry-run' the remote is still read but no write is
issued; the counts then report what the same batch push would have
done, overrides the table lacks counted as kept (issue #566)."
  (let* ((endpoint (org-canvas-api-course-endpoint
                    "assignments/%s/overrides" assignment-id))
         (existing (org-canvas--override-fetch-existing endpoint assignment-id))
         (created 0) (updated 0)
         (matched-ids nil))
    (dolist (override overrides)
      (let* ((existing-override (org-canvas--override-find-existing override existing))
             (outcome (org-canvas--override-reconcile-one
                       endpoint override existing-override)))
        (when existing-override
          (push (alist-get 'id existing-override) matched-ids))
        (pcase outcome
          ('created (setq created (1+ created)))
          ('updated (setq updated (1+ updated))))))
    (let ((removed (org-canvas--override-delete-removed
                    endpoint existing matched-ids confirm)))
      (list :created created :updated updated
            :deleted (car removed) :kept (cdr removed)))))

;;;; Keeping the Assignment's Baseline (issue #348)
;;
;; Every override written bumps the parent assignment's `updated_at',
;; which is the heading's conflict baseline.  Left alone, the next push
;; of the heading sees the package's own override write as remote drift
;; and refuses.  The heading is restamped after a reconcile that wrote
;; anything, but only when it agreed with Canvas before the first write:
;; a web-UI edit made since the last push must stay visible as drift.

(defun org-canvas--override-read-assignment-item (assignment-id)
  "GET ASSIGNMENT-ID through the feature registry; return the reply alist.
The read carries the registry's `:item-params', as every single
assignment read does (issue #273).  Answer nil when the read fails or
the reply is not an object; a failure is logged, never signalled."
  (let ((feature (org-canvas--registry-find-feature "Assignments")))
    (condition-case err
        (let* ((url (if feature
                        (org-canvas--feature-item-url feature assignment-id)
                      (org-canvas-api-course-endpoint
                       "assignments/%s" assignment-id)))
               (params (and feature
                            (org-canvas--feature-item-params feature)))
               (reply (org-canvas-api-request 'GET url :params params)))
          (and (listp reply) reply))
      (error
       (org-canvas--log-warning org-canvas--logger
         (concat "[Override] Could not read assignment %s (%s); "
                 "its baseline is left alone")
         assignment-id (error-message-string err))
       nil))))

(defun org-canvas--override-item-updated (item)
  "Return the `updated_at' of the assignment ITEM as a string, or nil."
  (let ((updated (alist-get 'updated_at item)))
    (and (stringp updated) updated)))

(defun org-canvas--override-read-assignment (assignment-id)
  "GET ASSIGNMENT-ID through the feature registry; return its `updated_at'.
Nil when the read fails or names no timestamp; see
`org-canvas--override-read-assignment-item'."
  (org-canvas--override-item-updated
   (org-canvas--override-read-assignment-item assignment-id)))

(defun org-canvas--assignment-stamp-current-p (pom assignment-id title)
  "Return non-nil if the stamp at POM is current for assignment ASSIGNMENT-ID.
The stamp is current when its CANVAS_UPDATED_AT is no earlier than
Canvas's `updated_at', or when a date at which Canvas touches the
assignment by itself explains the newer one, as the push's conflict
check allows (issue #453).  TITLE names the assignment in the log.
A heading with no stamp of its own answers nil without a read.  The
override reconcile (issue #348) and the rubric update (issue #470)
both ask this before restamping, so they agree with the push on what
drift is (issue #478)."
  (let ((stamp (org-canvas--parse-iso8601-time
                (org-entry-get pom "CANVAS_UPDATED_AT"))))
    (when stamp
      (let* ((item (org-canvas--override-read-assignment-item assignment-id))
             (remote (org-canvas--parse-iso8601-time
                      (org-canvas--override-item-updated item))))
        (and remote
             (or (not (time-less-p stamp remote))
                 (org-canvas--conflict-scheduled-bump
                  stamp remote
                  (org-canvas--conflict-scheduled-dates
                   (org-canvas--feature-scheduled-dates-fn
                    (org-canvas--registry-find-feature "Assignments"))
                   item title))))))))

(defun org-canvas--override-baseline-clean-p (pom assignment-id title)
  "Return non-nil when the heading at POM agrees with Canvas's ASSIGNMENT-ID.
Agreement is what `org-canvas--assignment-stamp-current-p' asks,
which is what the push's conflict check asks, a newer `updated_at'
that a passed unlock or lock date explains included (issue #478).
TITLE names the heading in the log.  Under a dry run every heading
answers nil without a read: nothing will be written that moves the
baseline."
  (unless org-canvas--dry-run
    (org-canvas--assignment-stamp-current-p pom assignment-id title)))

(defun org-canvas--override-restamp (pom assignment-id title &optional item)
  "Restamp CANVAS_UPDATED_AT at POM from ASSIGNMENT-ID's `updated_at'.
TITLE names the heading in the log.  ITEM, when given, is the
assignment as already read after the override writes, so no second
read is made.  PAYLOAD_HASH is kept: the assignment's own payload did
not change, only its overrides."
  (let ((updated (if item
                     (org-canvas--override-item-updated item)
                   (org-canvas--override-read-assignment assignment-id))))
    (when updated
      (condition-case err
          (progn
            (org-canvas-org-set-property pom "CANVAS_UPDATED_AT" updated)
            (org-canvas--log-info org-canvas--logger
              (concat "[Override] Baseline for '%s' restamped to %s "
                      "after its override writes")
              title updated))
        (error
         (org-canvas--log-warning org-canvas--logger
           (concat "[Override] Could not restamp '%s' (%s); "
                   "its next push may report a conflict")
           title (error-message-string err)))))))

(defun org-canvas--override-restamp-explained (pom assignment-id title
                                                       mismatch-fn)
  "Restamp POM after override writes on a drifted heading, if only they drifted.
ASSIGNMENT-ID is read once, after the writes; MISMATCH-FN, called
with the reply, returns the heading's own fields Canvas holds
otherwise, as (ORG-PROP LOCAL REMOTE) triples.  When it returns none,
the drift the stamp recorded was the overrides the reconcile has just
matched — an extension given in the web UI, copied into the table —
and the stamp is moved to the reply's `updated_at', TITLE naming the
heading in the log (issue #410).  A field that differs keeps the
stamp, so the next push still reports the conflict."
  (let ((item (org-canvas--override-read-assignment-item assignment-id)))
    (when item
      (let ((mismatches (funcall mismatch-fn item)))
        (if mismatches
            (org-canvas--log-info org-canvas--logger
              (concat "[Override] '%s' had drifted from Canvas before its "
                      "override writes, and %s differs; stamp left alone "
                      "so the next push reports the conflict")
              title (mapconcat (lambda (m) (format "%s (org %s, canvas %s)"
                                                   (nth 0 m) (nth 1 m) (nth 2 m)))
                               mismatches ", "))
          (org-canvas--log-info org-canvas--logger
            (concat "[Override] '%s' had drifted from Canvas before its "
                    "override writes, but its own fields agree with Canvas: "
                    "the drift was its overrides")
            title)
          (org-canvas--override-restamp pom assignment-id title item))))))

(defun org-canvas--override-sync-heading (pom assignment-id overrides title
                                              &optional options)
  "Reconcile OVERRIDES for ASSIGNMENT-ID and keep the heading's baseline.
POM is the assignment heading and TITLE its name for the log.  When
the heading agreed with Canvas before the reconcile and the reconcile
wrote anything, CANVAS_UPDATED_AT is restamped from a fresh read, so
the next push does not take the override writes for a web-UI edit
\(issue #348).  A heading that had already drifted keeps its stamp,
since restamping it would hide the edit — unless OPTIONS, a plist,
carries a `:mismatch-fn' that finds the heading's own fields agreeing
with Canvas, in which case the drift was the overrides just matched
and the stamp moves (issue #410; `org-canvas--override-restamp-explained').
`(:confirm-deletes' in OPTIONS asks before an override Canvas holds is
deleted (issue #411).  Return the reconcile's counts plist."
  (let* ((clean (org-canvas--override-baseline-clean-p
                 pom assignment-id title))
         (counts (org-canvas--override-sync-for-assignment
                  assignment-id overrides (plist-get options :confirm-deletes)))
         (mismatch-fn (plist-get options :mismatch-fn)))
    (cond
     ((zerop (+ (plist-get counts :created)
                (plist-get counts :updated)
                (plist-get counts :deleted))))
     (clean (org-canvas--override-restamp pom assignment-id title))
     (org-canvas--dry-run)
     (mismatch-fn (org-canvas--override-restamp-explained
                   pom assignment-id title mismatch-fn))
     (t
      (org-canvas--log-debug org-canvas--logger
        (concat "[Override] '%s' had drifted from Canvas before its "
                "override writes; stamp left alone")
        title)))
    counts))

(defun org-canvas--override-sync-entry (pom &optional source-dir options)
  "Reconcile the overrides table under the assignment heading at POM.
POM is a marker at the heading, or within its entry.  SOURCE-DIR is
the directory section links resolve against, by default that of the
buffer's file.  OPTIONS is the plist `org-canvas--override-sync-heading'
takes: a push by heading passes its `:mismatch-fn' and
`:confirm-deletes'.  Return the counts plist of
`org-canvas--override-sync-heading' — :created, :updated, :deleted
and :kept — or nil when the heading has no CANVAS_ID or no `#+NAME:
`overrides' table: the one heading's reconcile, which
`org-canvas-sync-overrides' runs for every heading and a push by
heading runs for its own (issue #380)."
  (org-with-point-at pom
    (org-back-to-heading t)
    (let* ((canvas-id (org-canvas-org-get-property (point) "CANVAS_ID"))
           (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
           (end (save-excursion (org-end-of-subtree t) (point)))
           (table (org-canvas--override-find-table end)))
      (when (and canvas-id table)
        (org-canvas--log-info org-canvas--logger
          "[Override] Processing overrides for '%s' (ID: %s)" title canvas-id)
        (org-canvas--override-sync-heading
         pom canvas-id
         (org-canvas--override-parse-table
          table (or source-dir (file-name-directory (buffer-file-name))))
         title options)))))

(defun org-canvas--override-sync-preflight ()
  "Validate assignments file and log header for override sync.
Returns the expanded assignments file path."
  (let ((assignments-file (expand-file-name
                           (if (boundp 'org-canvas-assignments-file)
                               org-canvas-assignments-file
                             (org-canvas--path "assignments.org")))))
    ;; The overrides table lives in assignments.org, so its file is
    ;; what an allowlist must name to let a scoped identity send them
    ;; (issue #550).
    (org-canvas--check-write-scope assignments-file "the override sync")
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
reconciles them with Canvas assignment overrides.  An override that
already carries its row's dates is not sent again (issue #380); a
push of one assignment heading reconciles that heading's table."
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
            (let ((counts (org-canvas--override-sync-entry marker source-dir)))
              (when counts
                (setq total-created (+ total-created (plist-get counts :created)))
                (setq total-updated (+ total-updated (plist-get counts :updated)))
                (setq total-deleted (+ total-deleted (plist-get counts :deleted)))
                (setq assignments-processed (1+ assignments-processed)))))
          (dolist (m markers) (set-marker m nil))
          (org-canvas--save-buffer)))

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

;;;; Meeting Times (issue #383)

(defconst org-canvas--section-day-letters
  '(("U" . 0) ("M" . 1) ("T" . 2) ("W" . 3) ("Th" . 4) ("R" . 4)
    ("F" . 5) ("S" . 6))
  "Day letters a MEETS property may run together, as in MWF or TTh.
The value is the day of the week, Sunday 0, as `decode-time' numbers it.")

(defconst org-canvas--section-day-names
  '(("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3) ("thu" . 4)
    ("fri" . 5) ("sat" . 6))
  "Day names a MEETS property may list, by their first three letters.")

(defun org-canvas--section-meets-letters (text)
  "Return the days TEXT spells as run-together letters (MWF, TTh), or nil."
  (let ((case-fold-search nil) (pos 0) days)
    (when (string-match-p "\\`\\(?:Th\\|[MTWRFSU]\\)+\\'" text)
      (while (string-match "Th\\|[MTWRFSU]" text pos)
        (push (cdr (assoc (match-string 0 text) org-canvas--section-day-letters))
              days)
        (setq pos (match-end 0)))
      (nreverse days))))

(defun org-canvas--section-meets-names (text)
  "Return the days TEXT names (Mon Wed, Tue/Thu), or nil when one is unknown."
  (let* ((words (split-string (downcase text) "[ ,/]+" t))
         (days (mapcar (lambda (w)
                         (and (>= (length w) 3)
                              (cdr (assoc (substring w 0 3)
                                          org-canvas--section-day-names))))
                       words)))
    (and days (not (memq nil days)) days)))

(defun org-canvas--section-meets-clock (hours minutes)
  "Return HOURS:MINUTES (strings) as minutes after midnight, or nil if invalid."
  (let ((h (string-to-number hours)) (m (string-to-number minutes)))
    (and (< h 24) (< m 60) (+ (* 60 h) m))))

(defun org-canvas--section-meets-pattern (text)
  "Parse one MEETS pattern TEXT, \"MWF 10:10-11:00\", into (DAYS START END).
DAYS lists days of the week (Sunday 0); START and END are minutes
after midnight.  Nil when TEXT is not a pattern or ends before it starts."
  (when (string-match
         (concat "\\`[ \t]*\\(.+?\\)[ \t]+\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)"
                 "[ \t]*-[ \t]*\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)[ \t]*\\'")
         text)
    ;; Every group is read before the day parsers run their own matches.
    (let* ((day-text (match-string 1 text))
           (start (org-canvas--section-meets-clock
                   (match-string 2 text) (match-string 3 text)))
           (end (org-canvas--section-meets-clock
                 (match-string 4 text) (match-string 5 text)))
           (days (or (org-canvas--section-meets-letters day-text)
                     (org-canvas--section-meets-names day-text))))
      (and days start end (< start end) (list days start end)))))

(defun org-canvas--section-meets-parse (text)
  "Parse a MEETS property TEXT into a list of (DAYS START END) patterns.
Patterns are separated by `;', each a day list and a clock range:
\"MWF 10:10-11:00\", \"TTh 13:25-14:15\", \"Mon Wed 9:05-9:55\".
Days run together as letters (M T W Th or R F S U) or are named by
their first three letters.  Nil when any pattern does not parse, so a
typo never narrows a window silently; validation reports it."
  (let* ((parts (split-string (or text "") ";" t "[ \t]+"))
         (patterns (mapcar #'org-canvas--section-meets-pattern parts)))
    (and patterns (not (memq nil patterns)) patterns)))

(defun org-canvas--section-link-key (link description)
  "Return the section a bracket link names, from its LINK and DESCRIPTION.
A link into sections.org names the heading after its `::*'; any
other link is known by its description."
  (let ((key (or (and (string-match "::\\*\\(.+\\)\\'" link) (match-string 1 link))
                 description
                 link)))
    (string-trim (replace-regexp-in-string "\\\\\\([][]\\)" "\\1" key))))

(defun org-canvas--section-keys (text)
  "Return the sections TEXT names, as sections.org heading titles.
TEXT is a people.org SECTIONS value or an overrides table's first
cell: links to sections.org headings, and plain section names,
separated by commas.  A link gives its heading, so a section is
matched by the same key wherever it is named."
  (let ((pos 0) (rest "") keys)
    (while (string-match org-link-bracket-re text pos)
      ;; Read the match before `org-canvas--section-link-key' matches again.
      (let ((link (match-string 1 text)) (description (match-string 2 text)))
        (setq rest (concat rest (substring text pos (match-beginning 0)))
              pos (match-end 0)
              keys (cons (org-canvas--section-link-key link description) keys))))
    (setq rest (concat rest (substring text pos)))
    (append (nreverse keys) (split-string rest "," t "[ \t]+"))))

(defun org-canvas--override-row-window (row columns)
  "Return (KEY OPENS CLOSES) for the overrides table ROW, or nil.
COLUMNS are the due, unlock and lock column indexes.  OPENS is the
Unlock At cell; CLOSES the Lock At cell, or the Due At cell when the
row has no lock.  A group's or students' row, and a row lacking either
end, gives no window."
  (let* ((cell (string-trim (or (nth 0 row) "")))
         (text (lambda (col) (let ((v (and col (string-trim (or (nth col row) "")))))
                               (and v (string-match-p "\\`[<[]" v) v))))
         (opens (funcall text (nth 1 columns)))
         (closes (or (funcall text (nth 2 columns)) (funcall text (nth 0 columns)))))
    (unless (or (string-match-p org-canvas--override-group-regexp cell)
                (string-match-p org-canvas--override-students-regexp cell)
                (null opens) (null closes))
      (list (car (org-canvas--section-keys cell)) opens closes))))

(defun org-canvas--override-section-windows (assignment-id)
  "Return the section windows the overrides table of ASSIGNMENT-ID gives.
Each is (KEY OPENS CLOSES): the section's sections.org heading title
and two Org timestamp strings, Unlock At and Lock At (Due At when the
row has no Lock At).  The table is the one under the level-1 heading
of `org-canvas-assignments-file' whose CANVAS_ID is ASSIGNMENT-ID;
nil when there is no such heading or table."
  (let ((file (org-canvas--override-lookup-file 'org-canvas-assignments-file)))
    (when file
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (let ((pos (org-find-property "CANVAS_ID" (format "%s" assignment-id))))
            (when pos
              (goto-char pos)
              (let* ((end (save-excursion (org-end-of-subtree t) (point)))
                     (table (org-canvas--override-find-table end))
                     (columns (and table (org-canvas--override-date-columns
                                          (car table)))))
                (delq nil (mapcar (lambda (row)
                                    (and (listp row)
                                         (org-canvas--override-row-window row columns)))
                                  (cdr table)))))))))))

;;;; One Student's New Deadline (issue #526)
;;
;; `org-canvas-extend-student' writes one `Students:' row into the
;; overrides table of the assignment heading at point and sends that
;; override alone: the reconcile that `org-canvas-sync-overrides' and a
;; push of the heading run would also send, or delete, every other row.

(defcustom org-canvas-extend-student-late-hours 48
  "Hours after a new due time that `org-canvas-extend-student' locks at.
The lock prompt offers the due time plus this many hours, and
`org-canvas-extend-student-named' uses it when no lock is named; 0
locks at the due time itself."
  :type 'number
  :group 'org-canvas)

(defun org-canvas--extend-students ()
  "Return the students of people.org as (TITLE USER-ID SECTIONS) lists.
In file order: the level-2 headings whose ROLE is student and which
carry a USER_ID.  A missing people.org, or one naming no student, is
a `user-error' that says to pull the roster."
  (let* ((file (org-canvas--override-lookup-file 'org-canvas-people-file))
         (students
          (and file
               (with-current-buffer (org-canvas--find-file-noselect file)
                 (org-with-wide-buffer
                  (delq nil
                        (org-map-entries
                         (lambda ()
                           (let ((id (org-entry-get (point) "USER_ID")))
                             (and id (list (org-get-heading t t t t) id
                                           (org-entry-get (point) "SECTIONS")))))
                         "LEVEL=2+ROLE=\"student\"" 'file)))))))
    (or students
        (user-error "No students in people.org; run org-canvas-pull-people first"))))

(defun org-canvas--extend-find-student (students name)
  "Return the entry of STUDENTS that NAME, a heading title or #USER_ID, names."
  (let ((id (org-canvas--override-literal-id name)))
    (or (cl-find-if (lambda (s) (if id (equal (nth 1 s) id) (equal (car s) name)))
                    students)
        (user-error "No student %s in people.org" name))))

(defun org-canvas--extend-timestamp (time)
  "Return TIME, a Lisp time value, as an active Org timestamp with a time."
  (format-time-string "<%Y-%m-%d %a %H:%M>" time))

(defun org-canvas--extend-lock-time (due-time)
  "Return DUE-TIME moved on by `org-canvas-extend-student-late-hours'."
  (time-add due-time (round (* 3600 org-canvas-extend-student-late-hours))))

(defun org-canvas--extend-row-claim (cell user-id)
  "Say how the overrides row whose first cell is CELL names USER-ID.
`exact' when the row is that student's alone, `shared' when it names
them beside others, and nil when it is another row or names no one."
  (when (string-match org-canvas--override-students-regexp cell)
    (let ((ids (mapcar #'org-canvas--override-resolve-student-id
                       (split-string (match-string 1 cell) ";" t "[ \t]+"))))
      (cond ((equal ids (list user-id)) 'exact)
            ((member user-id ids) 'shared)))))

(defun org-canvas--extend-first-cell (row)
  "Return the first cell of the table ROW, trimmed; \"\" for a rule line."
  (if (listp row) (string-trim (or (car row) "")) ""))

(defun org-canvas--extend-cell (row index)
  "Return the trimmed cell INDEX of ROW if a timestamp, else nil."
  (let ((cell (and index (listp row) (string-trim (or (nth index row) "")))))
    (and cell (string-prefix-p "<" cell) cell)))

(defun org-canvas--extend-section-row-p (cell keys)
  "Return non-nil when CELL is a section row naming one of KEYS."
  (and (not (string-match-p org-canvas--override-group-regexp cell))
       (not (string-match-p org-canvas--override-students-regexp cell))
       (member (car (org-canvas--section-keys cell)) keys)))

(defun org-canvas--extend-unlock (rows student)
  "Return the Unlock At timestamp STUDENT's new row inherits, or nil.
ROWS is the overrides table as `org-table-to-lisp' reads it, nil when
the heading at point has none.  The student's own row keeps its
unlock; failing that, the row of a section people.org's SECTIONS puts
them in gives it; failing that, the heading's UNLOCK_AT."
  (let ((keys (org-canvas--section-keys (or (nth 2 student) "")))
        (col (nth 1 (org-canvas--override-date-columns (car rows)))))
    (or (cl-some (lambda (row)
                   (and (eq (org-canvas--extend-row-claim
                             (org-canvas--extend-first-cell row) (nth 1 student))
                            'exact)
                        (org-canvas--extend-cell row col)))
                 (cdr rows))
        (cl-some (lambda (row)
                   (and (org-canvas--extend-section-row-p
                         (org-canvas--extend-first-cell row) keys)
                        (org-canvas--extend-cell row col)))
                 (cdr rows))
        (org-entry-get (point) "UNLOCK_AT"))))

(defun org-canvas--extend-widen (rows)
  "Return (ROWS . COLUMNS), ROWS with every date column it lacks appended.
ROWS is an overrides table as `org-table-to-lisp' reads it, header
first; COLUMNS are the due, unlock and lock indexes as
`org-canvas--override-date-columns' finds them.  A pulled table drops
the columns no row fills, and the new row may fill one."
  (let ((columns (org-canvas--override-date-columns (car rows))))
    (cl-loop for title in '("Due At" "Unlock At" "Lock At")
             for k from 0
             unless (nth k columns)
             do (setf (nth k columns) (length (car rows)))
             (setq rows (cons (append (car rows) (list title))
                              (mapcar (lambda (r) (if (listp r) (append r (list "")) r))
                                      (cdr rows)))))
    (cons rows columns)))

(defun org-canvas--extend-build-row (width columns target dates)
  "Return a row WIDTH cells wide naming TARGET, DATES in its date COLUMNS.
DATES are the due, unlock and lock cells, a nil one left empty."
  (let ((row (make-list (max width (1+ (apply #'max columns))) "")))
    (setf (nth 0 row) target)
    (cl-mapc (lambda (col date) (setf (nth col row) (or date ""))) columns dates)
    row))

(defun org-canvas--extend-keep-cells (old new columns)
  "Return NEW, a replacement for row OLD, with OLD's cells beyond COLUMNS.
COLUMNS are the due, unlock and lock indexes; every other cell of OLD
but the first (the target) is kept, so a written reason survives."
  (cl-loop for cell in new
           for kept in (append old (make-list (length new) ""))
           for k from 0
           collect (if (or (zerop k) (memq k columns)) cell kept)))

(defun org-canvas--extend-rows (rows student dates)
  "Return ROWS, an overrides table, with STUDENT's row set to DATES.
A row naming STUDENT alone has its date cells replaced in place,
keeping its other cells (an Accept cell's reason, issue #538), and
otherwise the new row goes last; a row naming STUDENT beside others is
a `user-error', since that override would change for everyone it
names.  ROWS nil starts a new table."
  (let* ((widened (org-canvas--extend-widen
                   (or rows (list (list "Section" "Due At" "Unlock At" "Lock At")
                                  'hline))))
         (new (org-canvas--extend-build-row
               (length (car (car widened))) (cdr widened)
               (org-canvas--override-students-cell (list (nth 1 student))) dates))
         (replaced nil)
         (kept (mapcar
                (lambda (row)
                  (pcase (org-canvas--extend-row-claim
                          (org-canvas--extend-first-cell row) (nth 1 student))
                    ('exact (setq replaced t)
                     (org-canvas--extend-keep-cells row new (cdr widened)))
                    ('shared (user-error "%s shares an override row with others; edit that row instead"
                                         (car student)))
                    (_ row)))
                (car widened))))
    (if replaced kept (append kept (list new)))))

(defun org-canvas--extend-render (rows)
  "Return ROWS, a table in `org-table-to-lisp' form, as Org table text."
  (mapconcat (lambda (row)
               (if (listp row)
                   (concat "| " (mapconcat #'identity row " | ") " |\n")
                 "|-|\n"))
             rows ""))

(defun org-canvas--extend-table-region (end)
  "Return (BEG . TABLE-END) of the overrides table between point and END.
Nil when there is none."
  (save-excursion
    (when (re-search-forward "^#\\+NAME:[ \t]+overrides[ \t]*$" end t)
      (forward-line 1)
      (when (looking-at-p org-table-line-regexp)
        (cons (point) (org-table-end))))))

(defun org-canvas--extend-new-table-position ()
  "Move to the place for a new overrides table of the heading at point.
That is the end of the heading's own text, before any child heading,
after a blank line; the `#+NAME:' line is inserted there."
  (goto-char (save-excursion (outline-next-heading) (point)))
  (unless (bolp) (insert "\n"))
  (unless (save-excursion (forward-line -1) (looking-at-p "[ \t]*$"))
    (insert "\n"))
  (insert "#+NAME: overrides\n"))

(defun org-canvas--extend-write-table (rows)
  "Write ROWS as the overrides table of the heading at point, and align it.
The table under the heading is replaced; a heading with none gets one
at the end of its own text."
  (let* ((end (save-excursion (org-end-of-subtree t) (point)))
         (region (org-canvas--extend-table-region end)))
    (save-excursion
      (if region
          (progn (delete-region (car region) (cdr region))
                 (goto-char (car region)))
        (org-canvas--extend-new-table-position))
      (let ((start (point)))
        (insert (org-canvas--extend-render rows))
        (goto-char start)
        (org-table-align)))))

(defun org-canvas--override-shared-remote (user-id existing)
  "Return the override of EXISTING naming USER-ID beside other students."
  (let ((id (string-to-number user-id)))
    (cl-find-if (lambda (item)
                  (let ((ids (append (alist-get 'student_ids item) nil)))
                    (and (memql id ids) (cdr ids))))
                existing)))

(defun org-canvas--override-push-alone (pom assignment-id override title)
  "Send OVERRIDE for ASSIGNMENT-ID and no other; keep the heading's baseline.
POM is the assignment heading, TITLE its name for the log.  The
override Canvas holds for exactly OVERRIDE's students is updated, or
left alone when it carries the dates already; else one is created.
Nothing is deleted, and a failed read of the overrides signals rather
than create a twin (Hard Rule 16).  A Canvas override naming the
student beside others is a `user-error'.  The heading is restamped as
after any reconcile that wrote, when it agreed with Canvas before
\(issue #348).  Return `created', `updated', `unchanged', or nil when
the write failed, which the log names."
  (let* ((clean (org-canvas--override-baseline-clean-p pom assignment-id title))
         (endpoint (org-canvas-api-course-endpoint
                    "assignments/%s/overrides" assignment-id))
         (existing (append (org-canvas-api-request-all-pages 'GET endpoint) nil))
         (claimed (org-canvas--override-find-existing override existing))
         (shared (and (not claimed)
                      (org-canvas--override-shared-remote
                       (car (plist-get override :student-ids)) existing))))
    (when shared
      (user-error "Canvas holds override %s for %s; edit that override's row instead"
                  (alist-get 'id shared)
                  (org-canvas--override-existing-label shared)))
    (let ((outcome (org-canvas--override-reconcile-one endpoint override claimed)))
      (when (and clean (memq outcome '(created updated)))
        (org-canvas--override-restamp pom assignment-id title))
      outcome)))

(defun org-canvas--extend-heading ()
  "Move to the assignment heading at point; return (ASSIGNMENT-ID . TITLE).
A heading without CANVAS_ID is a `user-error': the assignment has to
be on Canvas before an override can be."
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (org-back-to-heading t)
  (while (> (org-current-level) 1) (org-up-heading-safe))
  (let ((id (org-entry-get (point) "CANVAS_ID"))
        (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t))))
    (unless id
      (user-error "'%s' has no CANVAS_ID; push the assignment first" title))
    (cons id title)))

(defun org-canvas--extend-report (student title dates outcome)
  "Say how giving STUDENT DATES on TITLE came out, OUTCOME the push's answer."
  (let ((head (format "%s on '%s': due %s, lock %s" (car student) title
                      (nth 0 dates) (or (nth 2 dates) "none"))))
    (org-canvas--log-info org-canvas--logger "[Extend] %s: %s"
      head (or outcome "failed"))
    (message "%s; %s." head
             (pcase outcome
               ('created "override created")
               ('updated "override updated")
               ('unchanged "Canvas already held these dates, nothing sent")
               (_ "row written but the override was not sent, see the log")))))

(defun org-canvas--extend-dry-run (pom heading student override)
  "Preview giving STUDENT OVERRIDE on HEADING, (ID . TITLE) at POM.
The overrides are read and the write is logged, never sent; no row is
written.  Return `dry-run'."
  (org-canvas--log-info org-canvas--logger
    "[DRY-RUN] Would write %s's row under '%s'" (car student) (cdr heading))
  (org-canvas--override-push-alone pom (car heading) override (cdr heading))
  (message "Would extend %s on '%s' (dry run); nothing sent or written."
           (car student) (cdr heading))
  'dry-run)

(defun org-canvas--extend-student-here (student due-time lock-time)
  "Give STUDENT DUE-TIME and LOCK-TIME on the assignment heading at point.
STUDENT is an entry of `org-canvas--extend-students'; the times are
Lisp time values, LOCK-TIME nil for no lock.  The student's row of
the overrides table is written, with the unlock
`org-canvas--extend-unlock' finds, and saved; then that override alone
is sent (`org-canvas--override-push-alone').  A dry run writes no row
and sends nothing.  Return what the push returned, or `dry-run'."
  (org-canvas-clear-log)
  (let* ((heading (org-canvas--extend-heading))
         (pom (point-marker))
         (rows (org-canvas--override-find-table
                (save-excursion (org-end-of-subtree t) (point))))
         (dates (list (org-canvas--extend-timestamp due-time)
                      (org-canvas--extend-unlock rows student)
                      (and lock-time (org-canvas--extend-timestamp lock-time))))
         (new-rows (org-canvas--extend-rows rows student dates))
         (override (list :student-ids (list (nth 1 student))
                         :due-at (org-canvas-org-parse-timestamp (nth 0 dates))
                         :unlock-at (org-canvas-org-parse-timestamp (nth 1 dates))
                         :lock-at (org-canvas-org-parse-timestamp (nth 2 dates)))))
    (unwind-protect
        (if org-canvas--dry-run
            (org-canvas--extend-dry-run pom heading student override)
          (org-canvas--check-writable 'POST (format "extending '%s'" (cdr heading)))
          (org-canvas--ensure-buffer-fresh)
          (org-canvas--extend-write-table new-rows)
          (org-canvas--save-buffer)
          (let ((outcome (org-canvas--override-push-alone
                          pom (car heading) override (cdr heading))))
            (org-canvas--save-buffer)
            (org-canvas--extend-report student (cdr heading) dates outcome)
            outcome))
      (set-marker pom nil))))

(defun org-canvas--extend-read-args ()
  "Read a student, a due time and a lock time; return (STUDENT DUE LOCK).
The lock prompt offers the due time plus
`org-canvas-extend-student-late-hours'."
  (let* ((students (org-canvas--extend-students))
         (name (completing-read "Student: " (mapcar #'car students) nil t))
         (student (org-canvas--extend-find-student students name))
         (due (org-read-date t t nil (format "Due for %s: " name)))
         (lock (org-read-date t t nil "Lock: " nil
                              (format-time-string
                               "%Y-%m-%d %H:%M"
                               (org-canvas--extend-lock-time due)))))
    (list student due lock)))

;;;###autoload
(defun org-canvas-extend-student ()
  "Give one student a new deadline on the assignment heading at point.
Ask for a student from people.org, a due time and a lock time (the
due time plus `org-canvas-extend-student-late-hours' by default).
The unlock is the student's own row's, else their section's row's,
else the heading's UNLOCK_AT.  A `Students: NAME' row is written into
the heading's `#+NAME: overrides' table, replacing the student's own
row when there is one, and that override alone is sent: updated when
Canvas holds one for exactly that student, created otherwise, nothing
else touched (issue #526).  CANVAS_UPDATED_AT is restamped when the
heading agreed with Canvas before (issue #348).  A dry run writes and
sends nothing; a read-only course refuses."
  (interactive)
  (let ((args (org-canvas--extend-read-args)))
    (org-canvas--extend-student-here (nth 0 args) (nth 1 args) (nth 2 args))))

;;;###autoload
(defun org-canvas-extend-student-named (assignment student due &optional lock)
  "Give STUDENT a DUE time on ASSIGNMENT, all named, without asking.
ASSIGNMENT is an assignments.org heading's exact title, or #CANVAS_ID;
STUDENT a people.org title, or #USER_ID.  DUE and LOCK are dates as
`org-read-date' reads them (\"2026-10-22 16:00\"); LOCK nil is DUE
plus `org-canvas-extend-student-late-hours'.  Otherwise as
`org-canvas-extend-student'; return what it returns."
  (let* ((file (or (org-canvas--override-lookup-file 'org-canvas-assignments-file)
                   (user-error "No assignments.org in this course")))
         (id (org-canvas--override-literal-id assignment))
         (person (org-canvas--extend-find-student (org-canvas--extend-students) student))
         (due-time (org-read-date t t due))
         (lock-time (if lock (org-read-date t t lock)
                      (org-canvas--extend-lock-time due-time)))
         (marker (org-canvas--sync-find-heading
                  file "LEVEL=1" (or id assignment) (if id 'canvas-id 'title)
                  "CANVAS_ID" "assignment" "extend")))
    (unwind-protect
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (org-canvas--extend-student-here person due-time lock-time)))
      (set-marker marker nil))))

(provide 'org-canvas-sections)
;;; org-canvas-sections.el ends here
