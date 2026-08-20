;;; org-canvas-modules.el --- Pipeline-based Module Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas Modules.
;; Modules are the primary navigation structure in Canvas, organizing
;; course content into a sequential, optionally locked progression.
;;
;; FILE STRUCTURE
;; ==============
;; In modules.org:
;;   - Level 1 headings = Modules (course sections/weeks)
;;   - Level 2 headings = Module Items (links to content)
;;
;; ITEM TYPES
;; ==========
;; Module items link to content in other org files:
;;
;;   [[file:assignments.org::*Lab 1][Lab 1]]     -> Assignment
;;   [[file:quizzes.org::*Quiz 1][Quiz 1]]      -> Quiz
;;   [[file:pages.org::*Welcome][Welcome]]      -> Page
;;   [[file:discussions.org::*...][...]]        -> Discussion
;;   [[file:files.org::*...][...]]              -> File
;;
;; Headings without links are treated as SubHeaders (text dividers).
;;
;; LINK RESOLUTION
;; ===============
;; The sync process:
;;   1. Parse the link to extract file path and heading name
;;   2. Open the linked file and find the heading
;;   3. Read the CANVAS_ID (or CANVAS_URL for pages)
;;   4. Use that ID when creating the module item
;;
;; This requires syncing all content types BEFORE syncing modules.
;;
;; COMPLETION REQUIREMENTS
;; =======================
;; Use COMPLETION_REQUIREMENT property:
;;   must_view, must_submit, must_contribute, min_score
;;
;; For min_score, also set MIN_SCORE property to the threshold.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-modules-file (org-canvas--path "modules.org")
  "Path to the modules.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-modules-file "modules.org")
(org-canvas-register-feature
 :name "Modules" :endpoint "modules"
 :file-var 'org-canvas-modules-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'name)
(org-canvas-register-properties "modules"
  :label "Modules"
  :file-var 'org-canvas-modules-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "PUBLISHED" :data-key :published :type boolean
     :doc "Whether the module is visible to students (default: true)")
    (:org-prop "UNLOCK_AT" :data-key :unlock_at :type timestamp
     :doc "Date to unlock the module")
    (:org-prop "REQUIRE_SEQUENTIAL_PROGRESSION" :data-key :require_sequential_progression :type boolean
     :doc "Force items to be completed in order")
    (:org-prop "PUBLISH_FINAL_GRADE" :data-key :publish_final_grade :type boolean
     :doc "Publish the final grade when this module is completed")))
(org-canvas-register-properties "module-items"
  :label "Module Items"
  :file-var 'org-canvas-modules-file
  :query "LEVEL=2"
  :properties
  `((:org-prop "INDENT" :data-key :indent :type number
     :doc "Indentation level (0-5)")
    (:org-prop "COMPLETION_REQUIREMENT" :data-key :completion_requirement :type enum
     :values ,org-canvas--valid-completion-requirements
     :doc "must_view, must_submit, must_contribute, min_score")
    (:org-prop "MIN_SCORE" :data-key :min_score :type number
     :doc "Required score for min_score completion")
    (:org-prop "NEW_TAB" :data-key :new_tab :type boolean
     :doc "Open external URL in new tab")
    (:org-prop "PUBLISHED" :data-key :published :type boolean
     :doc "Publish state for this item; omit to keep the linked content's own state"))
  :structural-fn #'org-canvas--validate-module-item-link)

;;;; Helper Functions

(defconst org-canvas--file-to-item-type-alist
  '(("pages" . "Page") ("assignments" . "Assignment") ("discussions" . "Discussion")
    ("quizzes" . "Quiz") ("files" . "File") ("announcements" . "Discussion"))
  "Alist mapping Org filename stems to Canvas module item types.")

(defun org-canvas--module-item-type-from-file (filepath)
  "Determine the Canvas module item type from FILEPATH.
Returns one of: File, Page, Discussion, Assignment, Quiz,
SubHeader, ExternalUrl."
  (let ((stem (file-name-base filepath)))
    (or (alist-get stem org-canvas--file-to-item-type-alist nil nil #'equal)
        "Page")))

(defun org-canvas--module-resolve-file-path (file modules-file-dir)
  "Resolve FILE relative to MODULES-FILE-DIR, with basename fallback.
First tries the full relative path.  If that fails, tries just the
filename in MODULES-FILE-DIR (handles ../sibling-dir/file.org links
where sibling dirs aren't nested).  Returns absolute path, or nil."
  (let ((abs-file (expand-file-name file modules-file-dir)))
    (if (file-exists-p abs-file) abs-file
      (let ((fallback (expand-file-name (file-name-nondirectory file) modules-file-dir)))
        (when (file-exists-p fallback)
          (org-canvas--log-warning org-canvas--logger
                        "[Module Item] Path not found: %s, using fallback: %s"
                        abs-file fallback))
        (if (file-exists-p fallback) fallback nil)))))

(defun org-canvas--module-search-by-display-title (title)
  "Search current buffer for a heading whose display TITLE matches.
Returns a (CANVAS_ID . CANVAS_URL) cons cell, or nil."
  (goto-char (point-min))
  (let (canvas-id page-url)
    (while (and (not (or canvas-id page-url))
                (re-search-forward "^\\*+ " nil t))
      (let ((h (org-get-heading t t t t)))
        (when (or (string= h title)
                  (and h (string-match-p (regexp-quote title) h)))
          (setq canvas-id (org-entry-get (point) "CANVAS_ID"))
          (setq page-url (org-entry-get (point) "CANVAS_URL")))))
    (when (or canvas-id page-url)
      (cons canvas-id page-url))))

(defun org-canvas--module-get-ids-at-point ()
  "Return (CANVAS_ID . CANVAS_URL) cons at point, or nil if neither is set."
  (let ((canvas-id (org-entry-get (point) "CANVAS_ID"))
        (page-url (org-entry-get (point) "CANVAS_URL")))
    (when (or canvas-id page-url)
      (cons canvas-id page-url))))

(defun org-canvas--module-search-heading-for-id (abs-file heading title)
  "Search ABS-FILE for HEADING and return its Canvas IDs.
First tries exact match on HEADING (with Org bracket unescaping).
Falls back to matching TITLE against `org-get-heading' output, which
handles Emacs 29/30 differences in link stripping.
Returns a (CANVAS_ID . CANVAS_URL) cons cell, or nil if not found."
  (with-current-buffer (find-file-noselect abs-file)
    (save-excursion
      (goto-char (point-min))
      (if heading
          (let* ((unescaped (replace-regexp-in-string
                            "\\\\[][]"
                            (lambda (m) (substring m 1))
                            heading))
                 (search-re (format "^\\*+ +%s" (regexp-quote unescaped))))
            (if (re-search-forward search-re nil t)
                (org-canvas--module-get-ids-at-point)
              (org-canvas--module-search-by-display-title title)))
        (when (re-search-forward "^\\* " nil t)
          (org-canvas--module-get-ids-at-point))))))

(defun org-canvas--module-lookup-ids-in-file (abs-file heading title)
  "Look up Canvas IDs in ABS-FILE for HEADING (with TITLE fallback).
Returns a plist (:canvas-id ID :canvas-url URL) or nil."
  (let ((ids (org-canvas--module-search-heading-for-id abs-file heading title)))
    (when ids
      (list :canvas-id (car ids) :canvas-url (cdr ids)))))

(defun org-canvas--module-extract-heading-link-path (heading)
  "Return the file-link target embedded in HEADING text, or nil.
Matches `[[file:PATH][...]]' or bare `[[file:PATH]]' anywhere in
HEADING.  Used to look up file-typed module items in files.org."
  (when (and heading
             (or (string-match "\\[\\[file:\\([^]]+\\)\\]\\[" heading)
                 (string-match "\\[\\[file:\\([^]]+\\)\\]\\]" heading)))
    (match-string 1 heading)))

(defun org-canvas--module-find-file-canvas-id-by-path (files-org rel-path)
  "Find a heading in FILES-ORG whose link target is REL-PATH.
Returns its CANVAS_ID (string) or nil."
  (let (result)
    (with-current-buffer (find-file-noselect files-org)
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (when (and (not result)
                      (looking-at org-complex-heading-regexp))
             (let* ((heading (match-string-no-properties 4))
                    (path (org-canvas--module-extract-heading-link-path heading)))
               (when (equal path rel-path)
                 (setq result (org-entry-get (point) "CANVAS_ID"))))))
         t 'file)))
    result))

(defun org-canvas--module-resolve-content-link (rel-path title modules-file-dir)
  "Resolve a direct-file module-item link to a File-type result.
REL-PATH is the path from the module item link, relative to
MODULES-FILE-DIR.  TITLE is the link description.  Looks the path up
in files.org by matching it against file-heading link targets."
  (let ((files-org (expand-file-name "files.org" modules-file-dir)))
    (cond
     ((not (file-exists-p files-org))
      (org-canvas--log-warning org-canvas--logger
        "[Module Item] files.org not found while resolving direct file link: %s"
        rel-path)
      nil)
     (t
      (let ((canvas-id (org-canvas--module-find-file-canvas-id-by-path
                        files-org rel-path)))
        (cond
         (canvas-id
          (org-canvas--log-debug org-canvas--logger
            "[Module Item] Resolved direct file link: path=%s id=%s"
            rel-path canvas-id)
          (list :type "File"
                :content-id (string-to-number canvas-id)
                :page-url nil
                :title title))
         (t
          (org-canvas--log-warning org-canvas--logger
            "[Module Item] No CANVAS_ID found for direct file link: %s"
            rel-path)
          nil)))))))

(defun org-canvas--module-resolve-link (link-string modules-file-dir)
  "Resolve LINK-STRING to get the linked item's Canvas ID and type.
MODULES-FILE-DIR is the directory containing modules.org.
Returns a plist (:type TYPE :content-id ID :page-url URL :title TITLE) or nil."
  (when (and link-string
             (string-match "\\[\\[file:\\([^]:]+\\)\\(?:::\\*\\(.+\\)\\)?\\]\\[\\([^]]+\\)\\]\\]" link-string))
    (let* ((file (match-string 1 link-string))
           (heading (match-string 2 link-string))
           (title (match-string 3 link-string)))
      (cond
       ;; Direct file link (e.g. [[file:content/foo.pdf][foo.pdf]]).
       ;; A module item that links straight at a file path is a
       ;; File-type item; resolve it via files.org by matching path.
       ((not (string-match-p "\\.org\\'" file))
        (org-canvas--module-resolve-content-link file title modules-file-dir))
       ;; Otherwise this points to one of the per-type .org files.
       (t
        (let ((item-type (org-canvas--module-item-type-from-file file))
              (abs-file (org-canvas--module-resolve-file-path file modules-file-dir)))
          (org-canvas--log-debug org-canvas--logger "[Module Item] Resolving: file=%s heading=%s type=%s"
            file (or heading "N/A") item-type)
          (cond
           ((not abs-file)
            (org-canvas--log-warning org-canvas--logger "[Module Item] File not found: %s"
              (expand-file-name file modules-file-dir))
            nil)
           (t
            (let ((found (org-canvas--module-lookup-ids-in-file abs-file heading title)))
              (cond
               (found
                (let ((canvas-id (plist-get found :canvas-id))
                      (page-url (plist-get found :canvas-url)))
                  (org-canvas--log-debug org-canvas--logger "[Module Item] Resolved: type=%s id=%s url=%s"
                    item-type (or canvas-id "N/A") (or page-url "N/A"))
                  (list :type item-type
                        :content-id (when canvas-id (string-to-number canvas-id))
                        :page-url page-url
                        :title title)))
               (t
                (org-canvas--log-warning org-canvas--logger
                  "[Module Item] No CANVAS_ID found for: %s" heading)
                nil)))))))))))

(defun org-canvas--module-parse-prerequisite-ids (prereq-string)
  "Parse PREREQ-STRING into a list of module IDs.
Accepts comma-separated Canvas module IDs."
  (when prereq-string
    (mapcar #'string-trim (split-string prereq-string "," t))))

;;;; 1. Stage: Extraction - Module

(defun org-canvas--module-read-props (pom)
  "Read raw property strings from the module heading at POM."
  (list :title-raw (org-get-heading t t t t)
        :canvas-id (org-entry-get pom "CANVAS_ID")
        :published-raw (org-entry-get pom "PUBLISHED")
        :position-raw (org-entry-get pom "POSITION")
        :unlock-at-raw (org-entry-get pom "UNLOCK_AT")
        :prerequisite-module-ids (org-entry-get pom "PREREQUISITE_MODULE_IDS")
        :require-sequential-raw (org-entry-get pom "REQUIRE_SEQUENTIAL_PROGRESSION")
        :publish-final-grade-raw (org-entry-get pom "PUBLISH_FINAL_GRADE")))

(defun org-canvas--module-transform-props (raw)
  "Transform raw property strings RAW into typed module data.
Pure function — no buffer access."
  (let* ((title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw)))
         (position (org-canvas--interpret-number (plist-get raw :position-raw))))
    (list :title title
          :canvas-id (plist-get raw :canvas-id)
          :published (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
          :position (when (> position 0) position)
          :unlock-at (org-canvas-org-parse-timestamp (plist-get raw :unlock-at-raw))
          :prerequisite-module-ids (org-canvas--module-parse-prerequisite-ids
                                   (plist-get raw :prerequisite-module-ids))
          :require-sequential-progress (org-canvas--interpret-boolean
                                        (plist-get raw :require-sequential-raw))
          :publish-final-grade (org-canvas--interpret-boolean
                                (plist-get raw :publish-final-grade-raw)))))

(defun org-canvas--module-parse-entry ()
  "Extract module data from the Org heading at point (level 1)."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Starting module extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--module-read-props pom))
         (data (org-canvas--module-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Module")

    (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Processing Module: '%s' (ID: %s)"
      (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Position: %s, Unlock: %s, Sequential: %s"
      (or (plist-get data :position) "none")
      (or (plist-get data :unlock-at) "none")
      (plist-get data :require-sequential-progress))

    (plist-put data :pom pom)
    data))

;;;; 1. Stage: Extraction - Module Items

(defun org-canvas--module-item-read-props (pom modules-file-dir)
  "Read raw property strings from the module item heading at POM.
MODULES-FILE-DIR is used to resolve relative file links."
  (let* ((raw-heading (org-get-heading t t t t))
         ;; Get raw heading text from buffer with link syntax preserved.
         ;; org-get-heading strips [[...][...]] markup in Org 9.7+ (Emacs 30).
         (heading-with-links
          (save-excursion
            (beginning-of-line)
            (when (looking-at org-complex-heading-regexp)
              (match-string-no-properties 4))))
         (external-url (org-entry-get pom "EXTERNAL_URL"))
         ;; Resolve the link - try raw buffer text first (has link syntax),
         ;; then fall back to org-get-heading result
         (link-info (unless external-url
                      (or (org-canvas--module-resolve-link heading-with-links modules-file-dir)
                          (org-canvas--module-resolve-link raw-heading modules-file-dir)))))
    (list :title-raw raw-heading
          :heading-with-links heading-with-links
          :canvas-id (org-entry-get pom "CANVAS_ID")
          :item-type-raw (org-entry-get pom "ITEM_TYPE")
          :indent-raw (org-entry-get pom "INDENT")
          :completion-requirement (org-entry-get pom "COMPLETION_REQUIREMENT")
          :min-score-raw (org-entry-get pom "MIN_SCORE")
          :external-url external-url
          :new-tab-raw (org-entry-get pom "NEW_TAB")
          :published-raw (org-entry-get pom "PUBLISHED")
          :link-info link-info)))

(defun org-canvas--module-item-transform-props (raw)
  "Transform raw property strings RAW into typed module item data.
Pure function — no buffer access.
When :item-type-raw is present, it overrides link-based inference for the
:type field, but link-info is still used to extract :content-id/:page-url
when available."
  (let* ((title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw)))
         (indent (org-canvas--interpret-number (plist-get raw :indent-raw) 0))
         (min-score (org-canvas--interpret-number (plist-get raw :min-score-raw)))
         (new-tab (org-canvas--interpret-boolean (plist-get raw :new-tab-raw)))
         (published (org-canvas--interpret-boolean (plist-get raw :published-raw) t))
         (published-specified (and (plist-get raw :published-raw) t))
         (external-url (plist-get raw :external-url))
         (link-info (plist-get raw :link-info))
         (explicit-type (plist-get raw :item-type-raw))
         (completion-req (plist-get raw :completion-requirement)))
    (cond
     ;; Explicit SubHeader: emit a SubHeader plist regardless of link state.
     ((and explicit-type (string= explicit-type "SubHeader"))
      (list :type "SubHeader"
            :title title
            :canvas-id (plist-get raw :canvas-id)
            :indent indent
            :published published
            :published-specified published-specified))
     ;; External URL item
     (external-url
      (list :type "ExternalUrl"
            :title title
            :canvas-id (plist-get raw :canvas-id)
            :indent indent
            :external-url external-url
            :new-tab new-tab
            :published published
            :published-specified published-specified
            :completion-requirement completion-req
            :min-score (when (and min-score (> min-score 0)) min-score)))
     ;; Regular linked item — explicit type (when present) overrides inference
     (link-info
      (list :type (or explicit-type (plist-get link-info :type))
            :title (plist-get link-info :title)
            :content-id (plist-get link-info :content-id)
            :page-url (plist-get link-info :page-url)
            :canvas-id (plist-get raw :canvas-id)
            :indent indent
            :published published
            :published-specified published-specified
            :completion-requirement completion-req
            :min-score (when (and min-score (> min-score 0)) min-score)))
     ;; No link resolved — return nil to signal wrapper should classify
     (t nil))))

(defun org-canvas--module-classify-unlinked-heading
    (heading-with-links raw-heading canvas-id indent published pom
                        &optional published-specified)
  "Classify a module item heading whose link could not be fully resolved.
Distinguishes two cases:
  1. Heading contains a [[file:...]] link but the target has no CANVAS_ID
     yet — returns a plist with the correct :type (Page/Assignment/etc.)
     and nil :content-id so `sync-items' skips it.
  2. Heading is genuinely plain text — returns a SubHeader plist.
HEADING-WITH-LINKS is the raw buffer text (from `org-complex-heading-regexp'
group 4), RAW-HEADING is from `org-get-heading'.  CANVAS-ID, INDENT,
PUBLISHED, POM, and PUBLISHED-SPECIFIED are passed through to the
returned plist."
  (let* ((link-src (or heading-with-links raw-heading))
         (link-file (when (and link-src
                               (string-match "\\[\\[file:\\([^]:]+\\)" link-src))
                      (match-string 1 link-src)))
         (link-type (when link-file
                      (org-canvas--module-item-type-from-file link-file)))
         (link-title (when (and link-file
                                (string-match "\\]\\[\\([^]]+\\)\\]\\]$" link-src))
                       (match-string 1 link-src)))
         (result (if link-file
                     (list :type link-type
                           :title (or link-title raw-heading)
                           :canvas-id canvas-id
                           :indent indent
                           :published published
                           :published-specified published-specified
                           :pom pom)
                   (list :type "SubHeader"
                         :title raw-heading
                         :canvas-id canvas-id
                         :indent indent
                         :published published
                         :published-specified published-specified
                         :pom pom))))
    (if link-file
        (org-canvas--log-warning org-canvas--logger
          "[Stage 1: Parse] Unresolved link: '%s' (type: %s)"
          (or link-title raw-heading) link-type)
      (org-canvas--log-info org-canvas--logger
        "[Stage 1: Parse] Processing SubHeader: '%s'" raw-heading))
    result))

(defun org-canvas--module-item-parse-entry (modules-file-dir)
  "Extract module item data from the Org heading at point (level 2).
MODULES-FILE-DIR is used to resolve relative file links."
  (org-back-to-heading t)
  (org-canvas--log-debug org-canvas--logger "[Stage 1: Parse] Starting item extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--module-item-read-props pom modules-file-dir))
         (data (org-canvas--module-item-transform-props raw)))

    (cond
     ;; External URL or linked item — transform-props returned a result
     (data
      (let ((item-type (plist-get data :type))
            (title (plist-get data :title))
            (canvas-id (plist-get data :canvas-id)))
        (if (string= item-type "ExternalUrl")
            (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Processing ExternalUrl: '%s' (ID: %s)"
              title (or canvas-id "NEW"))
          (org-canvas--log-info org-canvas--logger "[Stage 1: Parse] Processing Item: '%s' -> %s (ID: %s)"
            title item-type (or canvas-id "NEW"))))
      (plist-put data :pom pom)
      data)
     ;; No link resolved — classify as SubHeader or unresolved link
     (t
      (let ((result (org-canvas--module-classify-unlinked-heading
                     (plist-get raw :heading-with-links)
                     (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
                     (plist-get raw :canvas-id)
                     (org-canvas--interpret-number (plist-get raw :indent-raw) 0)
                     (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
                     pom
                     (and (plist-get raw :published-raw) t))))
        result)))))

;;;; 2. Stage: Transformation - Module

(defun org-canvas--module-build-payload (data)
  "Convert module DATA to Canvas payload."
  (let ((title (plist-get data :title)))
    (org-canvas--log-info org-canvas--logger "[Stage 2: Transform] Building module payload for '%s'" title)

    (let ((module (make-hash-table :test 'equal)))
      (puthash "name" title module)
      (puthash "published" (org-canvas--to-json-boolean (plist-get data :published)) module)
      ;; A module owns its own publish state, but Canvas cascades it: an
      ;; update carrying module[published] runs publish_items!/unpublish_items!
      ;; over every item, which publishes or unpublishes the *content* each
      ;; item points at, overriding PUBLISHED in files.org, pages.org and the
      ;; rest (issue #47).  skip_content_tags stops the cascade at the module.
      (puthash "skip_content_tags" t module)

      (when (plist-get data :position)
        (puthash "position" (plist-get data :position) module))

      (when (plist-get data :unlock-at)
        (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Unlock at: %s" (plist-get data :unlock-at))
        (puthash "unlock_at" (plist-get data :unlock-at) module))

      (when (plist-get data :prerequisite-module-ids)
        (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Prerequisites: %s"
          (plist-get data :prerequisite-module-ids))
        (puthash "prerequisite_module_ids" (plist-get data :prerequisite-module-ids) module))

      (when (plist-get data :require-sequential-progress)
        (puthash "require_sequential_progress" t module))

      (when (plist-get data :publish-final-grade)
        (puthash "publish_final_grade" t module))

      (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Module payload complete")

      ;; Wrap in "module" key as required by Canvas API
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "module" module payload)
        payload))))

;;;; 2. Stage: Transformation - Module Item

(defconst org-canvas--module-item-self-owned-types '("SubHeader" "ExternalUrl")
  "Module item types whose published state belongs to the item itself.
An item of any other type points at a Canvas object owned by another
org-canvas feature — a file, page, assignment, quiz or discussion —
and publishing the item publishes that object.  See
`org-canvas--module-item-build-payload'.")

(defun org-canvas--module-item-build-payload (data position)
  "Convert module item DATA to Canvas payload at POSITION."
  (let ((item-type (plist-get data :type))
        (title (plist-get data :title)))
    (org-canvas--log-info org-canvas--logger "[Stage 2: Transform] Building item payload for '%s' (type: %s)"
      title item-type)

    (let ((item (make-hash-table :test 'equal)))
      (puthash "type" item-type item)
      (puthash "title" title item)
      (puthash "position" position item)

      ;; Content ID (required for most types)
      (when (plist-get data :content-id)
        (puthash "content_id" (plist-get data :content-id) item))

      ;; Page URL (for Page type)
      (when (and (string= item-type "Page") (plist-get data :page-url))
        (puthash "page_url" (plist-get data :page-url) item))

      ;; Indent
      (when (plist-get data :indent)
        (puthash "indent" (plist-get data :indent) item))

      ;; External URL
      (when (plist-get data :external-url)
        (puthash "external_url" (plist-get data :external-url) item))
      (when (plist-get data :new-tab)
        (puthash "new_tab" t item))

      ;; Published state.  For an item pointing at content another feature
      ;; owns, Canvas reads module_item[published] as an instruction to publish
      ;; that content, so sending it unconditionally overrode PUBLISHED: false
      ;; in files.org and friends (issue #47).  Send it only when this heading
      ;; declares it: omitted, a new item inherits the content's own state and
      ;; an existing one keeps its state.  Self-owned types always carry it —
      ;; they have no content behind them, and Canvas creates them unpublished.
      (when (or (plist-get data :published-specified)
                (member item-type org-canvas--module-item-self-owned-types))
        (puthash "published" (org-canvas--to-json-boolean (plist-get data :published)) item))

      ;; Completion requirement
      (when (plist-get data :completion-requirement)
        (puthash "completion_requirement[type]" (plist-get data :completion-requirement) item)
        (when (plist-get data :min-score)
          (puthash "completion_requirement[min_score]" (plist-get data :min-score) item)))

      (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Item payload complete")

      ;; Wrap in "module_item" key
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "module_item" item payload)
        payload))))

;;;; 3. Stage: Execution - Module

(defun org-canvas--module-search-by-name (name)
  "Search for a module with NAME on Canvas.  Return nil on error."
  (org-canvas--log-info org-canvas--logger "[Stage 3: Search] Looking for module '%s'..." name)
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "modules"))
             (params `(("search_term" . ,name)))
             (results (append (org-canvas-api-request 'GET endpoint :params params) nil))
             (count (length results)))
        (org-canvas--log-debug org-canvas--logger "[Stage 3: Search] Found %d results" count)
        (let ((found (cl-find-if (lambda (m) (string-equal (alist-get 'name m) name)) results)))
          (if found
              (org-canvas--log-info org-canvas--logger "[Stage 3: Search] Found exact match: ID=%s"
                (alist-get 'id found))
            (org-canvas--log-debug org-canvas--logger "[Stage 3: Search] No exact match found"))
          found))
    (error
     (org-canvas--log-warning org-canvas--logger "[Stage 3: Search] Failed: %s" (error-message-string err))
     nil)))

(defun org-canvas--module-push-to-api (data payload)
  "Send module PAYLOAD to Canvas API based on DATA."
  (org-canvas--push-to-api data payload
    :endpoint "modules"
    :find-fn #'org-canvas--module-search-by-name))

;;;; 3. Stage: Execution - Module Item

(defun org-canvas--module-find-item-by-title (module-id title)
  "Search for an item named TITLE in module MODULE-ID.
Return the matching item alist, or nil if not found."
  (let* ((items-endpoint (org-canvas-api-course-endpoint "modules/%s/items" module-id))
         (items (org-canvas-api-request-all-pages 'GET items-endpoint)))
    (cl-find-if (lambda (item)
                  (string-equal (alist-get 'title item) title))
                items)))

(defun org-canvas--module-item-push-to-api (module-id data payload)
  "Send module item PAYLOAD to MODULE-ID on Canvas based on DATA."
  (let* ((id (plist-get data :canvas-id))
         (title (plist-get data :title))
         (method (if id 'PUT 'POST))
         (endpoint (if id
                       (org-canvas-api-course-endpoint "modules/%s/items/%s" module-id id)
                     (org-canvas-api-course-endpoint "modules/%s/items" module-id)))
         (base-endpoint (format "modules/%s/items" module-id))
         (find-fn (lambda (ttl) (org-canvas--module-find-item-by-title module-id ttl))))

    (org-canvas--log-info org-canvas--logger "[Stage 3: Execute] %s Item '%s' to module %s"
      method title module-id)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data payload)))
          (org-canvas--log-info org-canvas--logger "[Stage 3: Execute] %s successful for item '%s'" method title)
          response)
      (error
       (org-canvas--log-error org-canvas--logger "[Stage 3: Execute] Item failed: %s" (error-message-string err))

       (cond
        ;; CASE 1: Timeout -> Search for item in module
        ((org-canvas--timeout-error-p err)
         (org-canvas--handle-timeout-recovery find-fn title err))

        ;; CASE 2: 404 on PUT -> Retry as POST (stale ID)
        ((org-canvas--404-on-put-p err method)
         (org-canvas--handle-404-retry base-endpoint payload find-fn title err))

        ;; Default: Re-throw
        (t (signal (car err) (cdr err))))))))

;;;; 4. Stage: Finalization

(defun org-canvas--module-sync-children (_data response)
  "Sync module items for the module described by DATA and RESPONSE."
  (let ((module-id (alist-get 'id response))
        (modules-file-dir (file-name-directory
                           (expand-file-name org-canvas-modules-file))))
    (when module-id
      (org-canvas--log-info org-canvas--logger "[Module Items] Syncing items for module %s..." module-id)
      (org-canvas--module-sync-items module-id (point) modules-file-dir))))

(defun org-canvas--module-finalize (data response)
  "Finalize module sync for DATA/RESPONSE and sync child items."
  (org-canvas--finalize-item data response
    :post-fn #'org-canvas--module-sync-children))

;;;; Main Sync Functions

(defun org-canvas--module-collect-item-markers (module-pom)
  "Collect deduplicated markers for all child headings of MODULE-POM."
  (let ((item-markers nil))
    (save-excursion
      (goto-char module-pom)
      (when (org-goto-first-child)
        (push (point-marker) item-markers)
        (while (org-get-next-sibling)
          (push (point-marker) item-markers))))
    (nreverse item-markers)))

(defun org-canvas--module-items-digest (data)
  "Return a digest string of the child items of the module described by DATA.
Folded into the module's payload hash (via the sync macro's
`:hash-extra' option) so item-level edits — adding, renaming,
re-linking, re-ordering, or changing properties of items — dirty the
module and trigger a re-sync even when the module's own attributes are
unchanged (issue #26).  Covers each item's raw properties and resolved
link target, but not the item's own CANVAS_ID: IDs assigned during
finalize must not dirty the hash for the next run."
  (let ((modules-file-dir (file-name-directory
                           (expand-file-name org-canvas-modules-file)))
        (markers (org-canvas--module-collect-item-markers (plist-get data :pom)))
        (parts nil))
    (dolist (marker markers)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (let ((raw (org-canvas--module-item-read-props (point) modules-file-dir)))
            (plist-put raw :canvas-id nil)
            (push (format "%S" raw) parts))))
      (set-marker marker nil))
    (format "%S" (nreverse parts))))

(defvar org-canvas--module-items-pending nil
  "Module items skipped because their target lacked a CANVAS_ID.
Each entry is a plist (:module-id ID :marker MARKER :title STRING
:dir DIR).  Only populated during a global sync
\(`org-canvas--sync-global-counters' non-nil); consumed and cleared by
`org-canvas--module-retry-pending-items' at the end of the run.")

(defun org-canvas--module-sync-items (module-id module-pom modules-file-dir)
  "Sync all items for MODULE-ID starting from MODULE-POM.
MODULES-FILE-DIR is used for resolving links.
Items whose linked content has no CANVAS_ID yet count as skips (not
failures), with titles recorded.  Item outcomes roll into the global
sync summary under \"Module Items\".
Returns (success-count skip-count fail-count)."
  (let ((item-markers (org-canvas--module-collect-item-markers module-pom))
        (success-count 0)
        (skip-count 0)
        (fail-count 0)
        (skipped-titles nil)
        (failed-titles nil)
        (position 1))
    (org-canvas--log-info org-canvas--logger "[Module Items] Found %d items to sync" (length item-markers))

    (dolist (marker item-markers)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (condition-case err
              (let* ((data (org-canvas--module-item-parse-entry modules-file-dir))
                     (item-type (plist-get data :type)))
                ;; Skip items without content ID (except SubHeader and ExternalUrl)
                (if (and (not (string= item-type "SubHeader"))
                         (not (string= item-type "ExternalUrl"))
                         (not (plist-get data :content-id))
                         (not (plist-get data :page-url)))
                    (progn
                      (org-canvas--log-warning org-canvas--logger "[Module Item] Skipping '%s': no linked content synced"
                        (plist-get data :title))
                      (setq skip-count (1+ skip-count))
                      (push (format "%s (no linked content synced)"
                                    (plist-get data :title))
                            skipped-titles)
                      ;; Remember the item for the end-of-run retry pass
                      (when org-canvas--sync-global-counters
                        (push (list :module-id module-id
                                    :marker (copy-marker marker)
                                    :title (plist-get data :title)
                                    :dir modules-file-dir)
                              org-canvas--module-items-pending)))
                  (let* ((payload (org-canvas--module-item-build-payload data position))
                         (response (org-canvas--module-item-push-to-api module-id data payload)))
                    (org-canvas--module-finalize data response)
                    (setq success-count (1+ success-count))
                    (setq position (1+ position)))))
            (error
             (setq fail-count (1+ fail-count))
             (push (format "item at point %d" (marker-position marker))
                   failed-titles)
             (org-canvas--log-error org-canvas--logger "[FAILED] Item at point %d: %s"
               (marker-position marker) (error-message-string err)))))))

    ;; Release markers to avoid memory leaks
    (dolist (m item-markers) (set-marker m nil))

    (org-canvas--sync-record-feature-stats "Module Items"
      (list :success success-count :skip skip-count :fail fail-count
            :skipped-titles skipped-titles :failed-titles failed-titles))

    (list success-count skip-count fail-count)))

(defun org-canvas--module-item-position (item-pom)
  "Return the 1-based position of the item heading at ITEM-POM among siblings."
  (save-excursion
    (goto-char item-pom)
    (org-back-to-heading t)
    (let ((target (point))
          (pos 1))
      (when (and (org-up-heading-safe)
                 (org-goto-first-child))
        (while (and (< (point) target)
                    (org-get-next-sibling))
          (setq pos (1+ pos))))
      pos)))

(defun org-canvas--module-refresh-payload-hash ()
  "Recompute and save PAYLOAD_HASH for the module heading at point.
Used after the retry pass heals a pending item: the hash stored when
the module synced predates the item's link resolution, so without a
refresh the next sync would re-push the whole module once for no
reason."
  (save-excursion
    (org-back-to-heading t)
    (let* ((data (org-canvas--module-parse-entry))
           (payload (org-canvas--module-build-payload data))
           (hash (org-canvas--sync-payload-hash
                  payload data #'org-canvas--module-items-digest)))
      (org-canvas-org-set-property (point) org-canvas--prop-payload-hash hash))))

(defun org-canvas--module-retry-single-pending (entry)
  "Retry the pending module item described by ENTRY.
Returns the item title when it synced, nil when it is still pending
\(target still has no CANVAS_ID, or the push failed).  On success the
parent module's PAYLOAD_HASH is refreshed so the healed state counts
as already-synced on the next run."
  (let ((marker (plist-get entry :marker))
        (module-id (plist-get entry :module-id))
        (dir (plist-get entry :dir))
        (title (plist-get entry :title)))
    (when (and (markerp marker) (marker-buffer marker))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (condition-case err
              (let ((data (org-canvas--module-item-parse-entry dir)))
                (when (or (plist-get data :content-id)
                          (plist-get data :page-url))
                  (let* ((position (org-canvas--module-item-position (point)))
                         (payload (org-canvas--module-item-build-payload data position))
                         (response (org-canvas--module-item-push-to-api module-id data payload)))
                    (org-canvas--module-finalize data response)
                    (save-excursion
                      (goto-char marker)
                      (when (org-up-heading-safe)
                        (org-canvas--module-refresh-payload-hash)))
                    (org-canvas--save-buffer)
                    title)))
            (error
             (org-canvas--log-warning org-canvas--logger
               "[Retry] Module item '%s' failed: %s"
               title (error-message-string err))
             nil)))))))

(defun org-canvas--module-retry-pending-items ()
  "Retry module items skipped because their target lacked a CANVAS_ID.
Called at the end of `org-canvas-sync': items whose target gained an
ID during the run are synced now (healing same-run ordering
casualties) and reclassified from skip to success in the global
summary; the rest produce a closing hint naming them.  Consumes and
clears `org-canvas--module-items-pending'."
  (when org-canvas--module-items-pending
    (org-canvas--log-info org-canvas--logger
      "--- Retry pass: %d module item(s) skipped earlier ---"
      (length org-canvas--module-items-pending))
    (let ((still-pending nil))
      (dolist (entry (nreverse org-canvas--module-items-pending))
        (let ((synced-title (org-canvas--module-retry-single-pending entry)))
          (if synced-title
              (progn
                (org-canvas--log-info org-canvas--logger
                  "[Retry] Synced module item '%s'" synced-title)
                (org-canvas--sync-reclassify-skip-as-success
                 "Module Items" synced-title))
            (push (plist-get entry :title) still-pending)))
        (let ((m (plist-get entry :marker)))
          (when (markerp m) (set-marker m nil))))
      (setq org-canvas--module-items-pending nil)
      (when still-pending
        (org-canvas--log-warning org-canvas--logger
          "%d module item(s) still pending: %s — sync their targets, then re-run M-x org-canvas-sync-modules"
          (length still-pending)
          (mapconcat (lambda (x) (format "'%s'" x))
                     (nreverse still-pending) ", "))))))

(org-canvas-define-sync modules
  :file org-canvas-modules-file
  :parse #'org-canvas--module-parse-entry
  :build #'org-canvas--module-build-payload
  :push #'org-canvas--module-push-to-api
  :finalize #'org-canvas--module-finalize
  ;; Module attributes alone miss item-level edits — fold an items
  ;; digest into the payload hash so they dirty the module (issue #26).
  :hash-extra #'org-canvas--module-items-digest
  :pull-item-fn #'org-canvas--module-pull-item)

;;;; Delete Functions

(org-canvas-define-delete-all modules
  :endpoint "modules"
  :file org-canvas-modules-file
  :title-field 'name)

(defun org-canvas--module-clear-children-properties (pom)
  "Clear sync properties from POM and all its child headings."
  (org-canvas-clear-sync-properties pom)
  (save-excursion
    (goto-char pom)
    (when (org-goto-first-child)
      (org-canvas-clear-sync-properties (point))
      (while (org-get-next-sibling)
        (org-canvas-clear-sync-properties (point))))))

(org-canvas-define-delete-at-point module
  :endpoint "modules/%s"
  :post-delete-fn #'org-canvas--module-clear-children-properties)

;;;; Pull

(defconst org-canvas--module-type-to-file-map
  '(("Page" . "pages.org")
    ("Assignment" . "assignments.org")
    ("Quiz" . "quizzes.org")
    ("Discussion" . "discussions.org")
    ("File" . "files.org"))
  "Map Canvas module item types to their corresponding Org files.")

(defun org-canvas--module-resolve-file-item-link (content-id title)
  "Resolve a File-typed module item to a direct file link.
Looks up CONTENT-ID in files.org (any heading depth, since files
may live under nested folder headings) and returns a link of the
form `[[file:PATH][TITLE]]', where PATH is the relative path stored
in the matching heading's link target.  Falls back to TITLE if
files.org or the heading is not found.

This bypasses the indirection through files.org so that clicking
the module item opens the file directly, and avoids the brittle
`*[[file:...][...]]' search target that arises when the files.org
heading is itself a link."
  (let ((files-org (expand-file-name (org-canvas--path "files.org"))))
    (cond
     ((not (file-exists-p files-org))
      (or title "Untitled"))
     (t
      (let ((target (format "%s" content-id))
            rel-path heading-name)
        (with-current-buffer (find-file-noselect files-org)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (when (and (not rel-path)
                          (equal (org-entry-get (point) "CANVAS_ID") target)
                          (looking-at org-complex-heading-regexp))
                 (let ((heading (match-string-no-properties 4)))
                   (setq heading-name heading)
                   (setq rel-path
                         (org-canvas--module-extract-heading-link-path heading)))))
             t 'file)))
        (cond
         (rel-path
          (org-link-make-string
           (format "file:%s" rel-path)
           (or title heading-name (file-name-nondirectory rel-path))))
         (t (or title "Untitled"))))))))

(defun org-canvas--module-resolve-org-item-link (item-type content-id title)
  "Resolve a non-File module item to a `[[file:TYPE-FILE.org::*HEADING]]' link.
ITEM-TYPE is one of `Page', `Assignment', `Quiz', or `Discussion'.
CONTENT-ID is the Canvas ID (or page URL for Pages).  TITLE is the
display title."
  (let* ((org-file (alist-get item-type org-canvas--module-type-to-file-map
                              nil nil #'equal))
         (id-prop (if (equal item-type "Page") "CANVAS_URL" "CANVAS_ID")))
    (if (not org-file)
        (or title "Untitled")
      (let ((file-path (expand-file-name
                        (org-canvas--path org-file))))
        (if (not (file-exists-p file-path))
            (or title "Untitled")
          (let ((heading-name nil))
            (with-current-buffer (find-file-noselect file-path)
              (save-excursion
                (goto-char (point-min))
                (org-map-entries
                 (lambda ()
                   (when (equal (org-entry-get (point) id-prop)
                                (format "%s" content-id))
                     (setq heading-name (org-get-heading t t t t))))
                 "LEVEL=1" 'file)))
            (if heading-name
                ;; `heading-name' may already contain escape sequences like `\[';
                ;; unescape so `org-link-make-string' doesn't double-escape them.
                (let ((unescaped (replace-regexp-in-string
                                  "\\\\\\([][]\\)" "\\1" heading-name)))
                  (org-link-make-string
                   (format "file:%s::*%s" org-file unescaped)
                   (or title unescaped)))
              (or title "Untitled"))))))))

(defun org-canvas--module-resolve-item-link (item-type content-id title)
  "Resolve a module item to an Org cross-file link.
ITEM-TYPE is a Canvas type string (\"Page\", \"Assignment\", etc.).
CONTENT-ID is the Canvas ID of the target item.
TITLE is the item title for display.
Returns a link string or just the title if resolution fails."
  (cond
   ;; File items resolve via files.org by CANVAS_ID, then emit a link
   ;; that points directly at the file's own path (the same form used
   ;; by file headings inside files.org).
   ((equal item-type "File")
    (org-canvas--module-resolve-file-item-link content-id title))
   (t
    (org-canvas--module-resolve-org-item-link item-type content-id title))))

(defun org-canvas--module-pull-insert-subheader (item-title item-id item-published &optional indent)
  "Insert a SubHeader heading with ITEM-TITLE, ITEM-ID, and ITEM-PUBLISHED.
Optional INDENT is emitted as :INDENT: only when nonzero."
  (insert (format "** %s\n" (or item-title "Section")))
  (org-back-to-heading t)
  (org-canvas-org-set-property (point) "CANVAS_ID" (format "%s" item-id))
  (org-canvas-org-set-property (point) "ITEM_TYPE" "SubHeader")
  (when (and indent (numberp indent) (> indent 0))
    (org-canvas-org-set-property (point) "INDENT" (format "%s" indent)))
  (org-canvas--pull-set-boolean-property (point) "PUBLISHED" item-published)
  (goto-char (save-excursion (org-end-of-subtree t t) (point))))

(defun org-canvas--module-pull-insert-external-url (item item-id item-published)
  "Insert an ExternalUrl heading from ITEM with ITEM-ID and ITEM-PUBLISHED."
  (let ((item-title (alist-get 'title item))
        (ext-url (alist-get 'external_url item))
        (new-tab (alist-get 'new_tab item))
        (indent (alist-get 'indent item)))
    (insert (format "** %s\n" (or item-title "External Link")))
    (org-back-to-heading t)
    (org-canvas-org-set-property (point) "CANVAS_ID" (format "%s" item-id))
    (org-canvas-org-set-property (point) "ITEM_TYPE" "ExternalUrl")
    (when ext-url
      (org-canvas-org-set-property (point) "EXTERNAL_URL" ext-url))
    (when new-tab
      (org-canvas-org-set-property (point) "NEW_TAB" "true"))
    (when (and indent (numberp indent) (> indent 0))
      (org-canvas-org-set-property (point) "INDENT" (format "%s" indent)))
    (org-canvas--pull-set-boolean-property (point) "PUBLISHED" item-published)
    (goto-char (save-excursion (org-end-of-subtree t t) (point)))))

(defun org-canvas--module-pull-insert-content-item (item item-id item-published)
  "Insert a content-linked heading from ITEM with ITEM-ID and ITEM-PUBLISHED."
  (let ((item-type (alist-get 'type item))
        (item-title (alist-get 'title item))
        (content-id (alist-get 'content_id item))
        (indent (alist-get 'indent item)))
    (let ((link (org-canvas--module-resolve-item-link
                 item-type content-id item-title)))
      (insert (format "** %s\n" link))
      (org-back-to-heading t)
      (org-canvas-org-set-property (point) "CANVAS_ID" (format "%s" item-id))
      (when item-type
        (org-canvas-org-set-property (point) "ITEM_TYPE" item-type))
      (when (and indent (numberp indent) (> indent 0))
        (org-canvas-org-set-property (point) "INDENT" (format "%s" indent)))
      (org-canvas--pull-set-boolean-property (point) "PUBLISHED" item-published)
      (goto-char (save-excursion (org-end-of-subtree t t) (point))))))

(defun org-canvas--module-pull-insert-items (items)
  "Insert level-2 headings for module ITEMS at point.
Returns the count of items inserted."
  (let ((count 0))
    (insert "\n")
    (dolist (item (org-canvas--pull-sort-items (append items nil)))
      (let ((item-type (alist-get 'type item))
            (item-id (alist-get 'id item))
            (item-published (alist-get 'published item)))
        (cond
         ((equal item-type "SubHeader")
          (org-canvas--module-pull-insert-subheader
           (alist-get 'title item) item-id item-published
           (alist-get 'indent item)))
         ((equal item-type "ExternalUrl")
          (org-canvas--module-pull-insert-external-url item item-id item-published))
         (t
          (org-canvas--module-pull-insert-content-item item item-id item-published)))
        (cl-incf count)))
    count))

(defun org-canvas--module-pull-item (item pos)
  "Set properties and child items for a pulled module.
ITEM is the module API response alist, POS is the heading position.
Used as the `:pull-item-fn' during interactive conflict resolution:
sets the module attributes, then replaces the child item headings with
the remote item list.  The conflict-check GET returns the module
without its items, so they are fetched from the items endpoint when
ITEM lacks an `items' key."
  (save-excursion
    (goto-char pos)
    (org-back-to-heading t)
    (let ((pom (point)))
      (let ((position (alist-get 'position item)))
        (when position
          (org-canvas-org-set-property pom "POSITION" (format "%s" position))))
      (org-canvas--pull-set-timestamp-property pom "UNLOCK_AT"
                                               (alist-get 'unlock_at item))
      (org-canvas--pull-set-boolean-property pom "PUBLISHED"
                                             (alist-get 'published item))
      (org-canvas--pull-set-boolean-property
       pom "REQUIRE_SEQUENTIAL_PROGRESSION"
       (alist-get 'require_sequential_progress item))
      (org-canvas--pull-set-boolean-property
       pom "PUBLISH_FINAL_GRADE" (alist-get 'publish_final_grade item))
      (let ((prereqs (append (alist-get 'prerequisite_module_ids item) nil)))
        (when prereqs
          (org-canvas-org-set-property
           pom "PREREQUISITE_MODULE_IDS"
           (mapconcat (lambda (id) (format "%s" id)) prereqs ","))))
      ;; Replace child items with the remote list.  Distinguish a
      ;; present-but-empty `items' key (module has no items) from an
      ;; absent one (conflict-check GET omits items — fetch them).
      (let* ((mid (alist-get 'id item))
             (items-cell (assq 'items item))
             (items (if items-cell
                        (append (cdr items-cell) nil)
                      (when mid
                        (append (org-canvas-api-request-all-pages
                                 'GET (org-canvas-api-course-endpoint
                                       "modules/%s/items" mid))
                                nil)))))
        (goto-char pom)
        (let ((body-start (save-excursion (org-end-of-meta-data t) (point)))
              (body-end (save-excursion (org-end-of-subtree t) (point))))
          (delete-region body-start body-end)
          (goto-char body-start)
          (when items
            (org-canvas--module-pull-insert-items items)))))))

;;;###autoload
(defun org-canvas-pull-modules ()
  "Pull modules from Canvas into modules.org."
  (interactive)
  (org-canvas--start-operation "PULLING MODULES")
  (let* ((file (expand-file-name org-canvas-modules-file))
         (endpoint (org-canvas-api-course-endpoint "modules"))
         (remote (org-canvas-api-request-all-pages
                  'GET endpoint '(("include[]" . "items"))))
         (mod-count 0) (item-count 0)
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "modules")
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (find-file-noselect file)
      (dolist (mod (org-canvas--pull-sort-items remote))
        (let* ((mid (alist-get 'id mod))
               (mname (alist-get 'name mod))
               (items (alist-get 'items mod))
               (pos (org-canvas--pull-upsert-heading file mid mname)))
          (goto-char pos)
          (when mname (org-edit-headline mname))
          (org-canvas-org-save-sync-state pos mid)
          (cl-incf mod-count)
          (let ((body-start (save-excursion
                              (org-end-of-meta-data t) (point)))
                (body-end (save-excursion
                            (org-end-of-subtree t) (point))))
            (delete-region body-start body-end)
            (goto-char body-start)
            (when items
              (setq item-count (+ item-count
                                  (org-canvas--module-pull-insert-items items)))))))
      (org-canvas--pull-write-file-header)
      (org-canvas--save-buffer))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger
      "Modules pull complete: %d modules, %d items" mod-count item-count)
    (message "Modules pull complete: %d modules, %d items."
             mod-count item-count)))

(provide 'org-canvas-modules)
;;; org-canvas-modules.el ends here
