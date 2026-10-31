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
;;
;; The item layer -- link resolution, the item stages, cross-module
;; moves, twin adoption, pruning and the pull's item insertion -- lives
;; in org-canvas-module-items.el (issue #593).

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(declare-function org-canvas--validate-module-item-link "org-canvas-validate-content")
(declare-function org-canvas--validate-module-item-ids "org-canvas-validate-content")
;; The item layer lives in org-canvas-module-items.el (issue #593), which
;; requires this file; these are the calls back into it.
(declare-function org-canvas--module-item-read-props "org-canvas-module-items")
(declare-function org-canvas--module-pull-insert-items "org-canvas-module-items")
(declare-function org-canvas--module-sync-items "org-canvas-module-items")

;;;; Configuration

(defcustom org-canvas-modules-file (org-canvas--path "modules.org")
  "Path to the modules.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-modules-file "modules.org")
(org-canvas-register-feature
 :name "Modules" :endpoint "modules"
 :file-var 'org-canvas-modules-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'name
 ;; A module's page redirects to its anchor on the Modules page, where
 ;; modules are edited in place; an item's redirects to the content it
 ;; links, except a SubHeader's, which Canvas cannot follow, so a
 ;; SubHeader opens its module (issue #300).
 :web-pages '((:level 1 :id-property "CANVAS_ID" :path "modules/%s")
              (:level 2 :path-fn org-canvas--module-subheader-web-path)
              (:level 2 :id-property "CANVAS_ID" :path "modules/items/%s")))
(org-canvas-register-properties "modules"
  :duplicate-titles t
  :label "Modules"
  :file-var 'org-canvas-modules-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :doc "Whether the module is visible to students (default: true)")
    (:org-prop "UNLOCK_AT" :data-key :unlock_at :type timestamp
     :doc "Date to unlock the module")
    ;; Canvas spells the field `require_sequential_progress'; the Org
    ;; property keeps the longer word.  The data-key is the Canvas field,
    ;; which is what the drift report and the pull read it by (issue #137).
    (:org-prop "REQUIRE_SEQUENTIAL_PROGRESSION" :data-key :require_sequential_progress :type boolean
     :doc "Force items to be completed in order")
    (:org-prop "PUBLISH_FINAL_GRADE" :data-key :publish_final_grade :type boolean
     :doc "Publish the final grade when this module is completed")
    (:org-prop "POSITION" :data-key :position :type number
     :doc "Position in the module list")
    (:org-prop "PREREQUISITE_MODULE_IDS" :data-key :prerequisite_module_ids :type csv-enum
     :doc "Canvas ids of modules that must be completed first (comma separated)")
    (:org-prop "PUBLISH_AT" :data-key :publish_at :type timestamp :local-only t
     :doc "Release date: sync publishes this module and its contents once passed")))
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
    ;; Absent means "keep the content's own state", so true and false
    ;; are both statements a pull must write (issue #323).
    (:org-prop "PUBLISHED" :data-key :published :type boolean
     :absent-inherits t
     :doc "Publish state for this item; omit to keep the linked content's own state")
    (:org-prop "PUBLISH_AT" :data-key :publish_at :type timestamp :local-only t
     :doc "Release date for this item; overrides the module's own PUBLISH_AT"))
  :structural-fn #'org-canvas--validate-module-item-link
  :file-fn #'org-canvas--validate-module-item-ids)

;;;; Helper Functions

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

(defun org-canvas--module-heading-search-regexp (heading)
  "Return a regexp matching the Org line whose heading text is HEADING.
Org escapes brackets inside link components, so `\\[' and `\\]' are
unescaped before quoting; the stars tolerate extra spaces."
  (format "^\\*+ +%s"
          (regexp-quote (org-canvas--unescape-org-brackets heading))))

(defun org-canvas--module-extract-heading-link-path (heading)
  "Return the file-link target embedded in HEADING text, or nil.
Matches `[[file:PATH][...]]' or bare `[[file:PATH]]' anywhere in
HEADING.  Used to look up file-typed module items in files.org."
  (when (and heading
             (or (string-match "\\[\\[file:\\([^]]+\\)\\]\\[" heading)
                 (string-match "\\[\\[file:\\([^]]+\\)\\]\\]" heading)))
    (match-string 1 heading)))

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

(defun org-canvas--module-push-to-api (data payload &optional ctx)
  "Send module PAYLOAD to Canvas API based on DATA in run context CTX."
  (org-canvas--push-to-api data payload
                           (org-canvas--push-options
                            :ctx ctx
                            :endpoint "modules"
                            :find-fn #'org-canvas--module-search-by-name)))

;;;; 4. Stage: Finalization

(defun org-canvas--module-sync-children (_data response &optional ctx)
  "Sync module items for the module described by DATA and RESPONSE.
CTX is the run context the items record their moves and skips in."
  (let ((module-id (alist-get 'id response))
        (modules-file-dir (file-name-directory
                           (expand-file-name org-canvas-modules-file))))
    (when module-id
      (org-canvas--log-info org-canvas--logger "[Module Items] Syncing items for module %s..." module-id)
      (org-canvas--module-sync-items module-id (point) modules-file-dir ctx))))

(defun org-canvas--module-finalize (data response &optional ctx)
  "Finalize module sync for DATA/RESPONSE in run context CTX; sync child items."
  (org-canvas--finalize-item data response
                             (org-canvas--finalize-options
                              :ctx ctx
                              :post-fn #'org-canvas--module-sync-children)))

;;;; Bulk Publish
;;
;; Releasing a week of content is the most repeated action in running a
;; course, and it used to mean hand-editing PUBLISHED across several
;; files: the module in modules.org, its lecture PDFs in files.org, the
;; assignments due that week, sometimes a page.  Six to ten edits in
;; three or four files, every week (issue #52).
;;
;; The rule here is the one issue #47 settled: publish state belongs to
;; the object, so publishing a module edits the heading that owns each
;; object, in whichever file that is.  Nothing is inferred from a module
;; item at push time.  Items with no object of their own — SubHeaders and
;; external URLs — carry their own state, so those are set in place.

(defun org-canvas--module-heading-position-by-title (title)
  "Return the position of the heading in this buffer whose text is TITLE.
Falls back to a substring match, which is how a heading that *is* a
link is found: `org-get-heading' strips link syntax on Org 9.7 but not
on 9.6."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (re-search-forward "^\\*+ " nil t))
      (let ((h (org-get-heading t t t t)))
        (when (and h (or (string= h title)
                         (string-match-p (regexp-quote title) h)))
          (org-back-to-heading t)
          (setq found (point)))))
    found))

(defun org-canvas--module-heading-position (abs-file heading title)
  "Return the position of HEADING in ABS-FILE, or nil.
Falls back to matching TITLE when the exact heading text is not found."
  (with-current-buffer (org-canvas--find-file-noselect abs-file)
    (save-excursion
      (goto-char (point-min))
      (if heading
          (if (re-search-forward
               (org-canvas--module-heading-search-regexp heading) nil t)
              (progn (org-back-to-heading t) (point))
            (org-canvas--module-heading-position-by-title title))
        (org-canvas--module-heading-position-by-title title)))))

(defun org-canvas--module-file-heading-position (files-org rel-path)
  "Return the position in FILES-ORG of the heading linking to REL-PATH."
  (let (result)
    (with-current-buffer (org-canvas--find-file-noselect files-org)
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (when (and (not result)
                      (looking-at org-complex-heading-regexp))
             (let ((path (org-canvas--module-extract-heading-link-path
                          (match-string-no-properties 4))))
               (when (equal path rel-path)
                 (setq result (point))))))
         t 'file)))
    result))

(defun org-canvas--module-content-target (rel-path modules-file-dir)
  "Return (FILE . POSITION) in files.org for the file at REL-PATH, or nil.
MODULES-FILE-DIR is where files.org is looked for."
  (let ((files-org (expand-file-name "files.org" modules-file-dir)))
    (when (file-exists-p files-org)
      (let ((pos (org-canvas--module-file-heading-position files-org rel-path)))
        (when pos (cons files-org pos))))))

(defun org-canvas--module-org-target (file heading title modules-file-dir)
  "Return (FILE . POSITION) for HEADING in FILE, or nil.
FILE is relative to MODULES-FILE-DIR; TITLE is the link description,
used as a fallback when the exact heading text is not found."
  (let ((abs-file (org-canvas--module-resolve-file-path file modules-file-dir)))
    (when abs-file
      (let ((pos (org-canvas--module-heading-position abs-file heading title)))
        (when pos (cons abs-file pos))))))

(defun org-canvas--module-item-target (link-string modules-file-dir)
  "Return (FILE . POSITION) for the object LINK-STRING points at, or nil.
MODULES-FILE-DIR resolves relative paths.  The same links
`org-canvas--module-resolve-link' follows, resolved to the owning
*heading* rather than to its Canvas id — publishing a week means
editing the file that owns each object."
  (when (and link-string
             (string-match
              "\\[\\[file:\\([^]:]+\\)\\(?:::\\*\\(.+\\)\\)?\\]\\[\\(.+\\)\\]\\]"
              link-string))
    (let ((file (match-string 1 link-string))
          (heading (match-string 2 link-string))
          (title (match-string 3 link-string)))
      (if (string-match-p "\\.org\\'" file)
          (org-canvas--module-org-target file heading title modules-file-dir)
        ;; A direct file link is owned by a heading in files.org.
        (org-canvas--module-content-target file modules-file-dir)))))

(defun org-canvas--module-item-display-title (heading-with-links fallback)
  "Return the display text of HEADING-WITH-LINKS, or FALLBACK.
`org-get-heading' strips link syntax on Org 9.7 but not on 9.6, so a
heading that *is* a link cannot be relied on for a readable name."
  (or (when (and heading-with-links
                 (string-match "\\]\\[\\(.+\\)\\]\\]" heading-with-links))
        (match-string 1 heading-with-links))
      fallback))

(defun org-canvas--module-set-published (pom state)
  "Set PUBLISHED to STATE at POM.  Return non-nil when the value changed."
  (let ((new (if state "true" "false")))
    (unless (equal (org-entry-get pom "PUBLISHED") new)
      (org-canvas-org-set-property pom "PUBLISHED" new)
      t)))

(defun org-canvas--module-publish-item (marker state modules-file-dir touched)
  "Apply publish STATE for the module item at MARKER.
MODULES-FILE-DIR resolves its link.  TOUCHED is a cons cell whose car
accumulates the buffers to save.  Return a cons (STATUS . TITLE) where
STATUS is `changed', `unchanged', `held' (the item has a PUBLISH_AT
still in the future, see `org-canvas--module-item-held-p'), or
`unresolved' (its link points at no object)."
  (goto-char (marker-position marker))
  (org-back-to-heading t)
  (let* ((heading-with-links
          (save-excursion
            (beginning-of-line)
            (when (looking-at org-complex-heading-regexp)
              (match-string-no-properties 4))))
         (title (org-canvas--module-item-display-title
                 heading-with-links
                 (org-canvas--strip-statistics-cookie
                  (org-get-heading t t t t))))
         (self-owned (or (org-entry-get (point) "EXTERNAL_URL")
                         (not (and heading-with-links
                                   (string-match-p "\\[\\[file:"
                                                   heading-with-links)))))
         (target (unless self-owned
                   (org-canvas--module-item-target
                    heading-with-links modules-file-dir))))
    (cond
     ;; The item's own release date outranks the module's.
     ((org-canvas--module-item-held-p (point) state) (cons 'held title))
     ;; SubHeaders and external URLs own their publish state (issue #47).
     (self-owned
      (push (current-buffer) (car touched))
      (cons (if (org-canvas--module-set-published (point) state)
                'changed 'unchanged)
            title))
     ((null target) (cons 'unresolved title))
     (t
      (with-current-buffer (org-canvas--find-file-noselect (car target))
        (push (current-buffer) (car touched))
        (cons (if (org-canvas--module-set-published (cdr target) state)
                  'changed 'unchanged)
              title))))))

(defun org-canvas--module-publish-apply (module-pom state)
  "Set PUBLISHED to STATE on the module at MODULE-POM and everything it lists.
Items carrying their own PUBLISH_AT are held back until that date and
named under `:held' rather than published with the rest of the module.
Saves every file touched.  Returns a plist
\(:changed N :unresolved TITLES :held TITLES)."
  (let ((modules-file-dir (file-name-directory
                           (expand-file-name org-canvas-modules-file)))
        (touched (list nil))
        (changed 0)
        (unresolved nil)
        (held nil))
    (save-excursion
      (goto-char module-pom)
      (when (org-canvas--module-set-published (point) state)
        (setq changed (1+ changed)))
      (push (current-buffer) (car touched))
      (let ((markers (org-canvas--module-collect-item-markers module-pom)))
        (dolist (marker markers)
          (save-excursion
            (let ((result (org-canvas--module-publish-item
                           marker state modules-file-dir touched)))
              (pcase (car result)
                ('changed (setq changed (1+ changed)))
                ('unresolved (push (cdr result) unresolved))
                ('held (push (cdr result) held))))))
        (dolist (m markers) (set-marker m nil))))
    (dolist (buf (delete-dups (car touched)))
      (with-current-buffer buf (org-canvas--save-buffer)))
    (list :changed changed
          :unresolved (nreverse unresolved)
          :held (nreverse held))))

(defun org-canvas--module-positions ()
  "Return an alist of (TITLE . POSITION) for the modules in modules.org."
  (let ((file (expand-file-name org-canvas-modules-file)))
    (when (file-exists-p file)
      (with-current-buffer (org-canvas--find-file-noselect file)
        (org-map-entries
         (lambda () (cons (org-get-heading t t t t) (point)))
         "LEVEL=1" 'file)))))

(defun org-canvas--module-publish-at-time (pom)
  "Return the PUBLISH_AT of the heading at POM as an encoded time, or nil.
Nil for a heading with no release date and for one whose timestamp
cannot be parsed: an unreadable date is not a schedule, so it neither
releases the heading nor holds it back.  Validation reports the bad
timestamp separately."
  (let ((publish-at (org-entry-get pom "PUBLISH_AT")))
    (when publish-at
      (ignore-errors (encode-time (org-parse-time-string publish-at))))))

(defun org-canvas--module-due-for-release-p (pom)
  "Return non-nil when the heading at POM has a PUBLISH_AT that has passed."
  (let ((time (org-canvas--module-publish-at-time pom)))
    (and time (not (time-less-p (current-time) time)))))

(defun org-canvas--module-item-held-p (pom state)
  "Return non-nil when the item at POM must not be published yet.
An item carrying its own PUBLISH_AT owns its release moment: publishing
the module around it — by hand or on schedule — leaves the item alone
until that date arrives, so a week can go live with pieces of it still
scheduled ahead.  Only publishing is held; STATE nil (unpublishing a
module) takes everything down with it, since leaving content live
behind an unpublished module is never what was asked for."
  (and state
       (let ((time (org-canvas--module-publish-at-time pom)))
         (and time (time-less-p (current-time) time)))))

(defun org-canvas--module-item-release-markers (module-pom)
  "Return markers for the items under MODULE-POM whose PUBLISH_AT has passed.
Markers for items that are not due are released before returning; the
caller owns the ones it gets back."
  (let (due)
    (dolist (marker (org-canvas--module-collect-item-markers module-pom))
      (if (org-canvas--module-due-for-release-p (marker-position marker))
          (push marker due)
        (set-marker marker nil)))
    (nreverse due)))

(defun org-canvas--module-release-module-items (module-pom modules-file-dir touched)
  "Publish the items under MODULE-POM whose own PUBLISH_AT has passed.
MODULES-FILE-DIR resolves their links; TOUCHED accumulates buffers to
save, exactly as in `org-canvas--module-publish-apply'.  Returns a
plist (:released TITLES :unresolved TITLES :hidden TITLES), where
`:hidden' repeats the released titles when the module around them is
explicitly unpublished — the content is live but nobody can reach it."
  (let ((released nil)
        (unresolved nil)
        (module-unpublished
         (equal (org-entry-get module-pom "PUBLISHED") "false")))
    (dolist (marker (org-canvas--module-item-release-markers module-pom))
      (save-excursion
        (let ((result (org-canvas--module-publish-item
                       marker t modules-file-dir touched)))
          (pcase (car result)
            ('changed (push (cdr result) released))
            ('unresolved (push (cdr result) unresolved)))))
      (set-marker marker nil))
    (setq released (nreverse released))
    (list :released released
          :unresolved (nreverse unresolved)
          :hidden (and module-unpublished released))))

(defun org-canvas--module-release-items ()
  "Publish every module item whose own PUBLISH_AT has passed.
An item's date outranks its module's: the module publish leaves it
alone until this pass releases it, so a week can go live in pieces.
Idempotent — an item already published is reported as nothing changed.

Returns a plist (:released TITLES :unresolved TITLES :hidden ALIST),
where ALIST maps a module title to the items released inside it while
that module is unpublished."
  (let ((positions (org-canvas--module-positions)))
    (when positions
      (let ((modules-file-dir (file-name-directory
                               (expand-file-name org-canvas-modules-file)))
            (touched (list nil))
            (released nil)
            (unresolved nil)
            (hidden nil))
        (with-current-buffer (org-canvas--find-file-noselect
                              (expand-file-name org-canvas-modules-file))
          (dolist (entry positions)
            (save-excursion
              (let ((result (org-canvas--module-release-module-items
                             (cdr entry) modules-file-dir touched)))
                (setq released (append released (plist-get result :released)))
                (setq unresolved
                      (append unresolved (plist-get result :unresolved)))
                (when (plist-get result :hidden)
                  (push (cons (car entry) (plist-get result :hidden))
                        hidden))))))
        (dolist (buf (delete-dups (car touched)))
          (with-current-buffer buf (org-canvas--save-buffer)))
        (list :released released
              :unresolved unresolved
              :hidden (nreverse hidden))))))

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

;; The generic delete-at-point clears the module's items with it (#331).
(org-canvas-define-delete-at-point module
  :endpoint "modules/%s")

;;;; Pull

(defun org-canvas--module-file-for-type (item-type)
  "Return the course file holding ITEM-TYPE's headings, or nil.
The file vars are read at call time, so a course that renames one
keeps its module-item links resolving (#435)."
  (pcase item-type
    ("Page" org-canvas-pages-file)
    ("Assignment" org-canvas-assignments-file)
    ("Quiz" org-canvas-quizzes-file)
    ("Discussion" org-canvas-discussions-file)
    ("File" org-canvas-files-file)
    (_ nil)))

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
         (was-fresh (org-canvas--pull-was-fresh-p file))
         (unlisted nil))
    (org-canvas--pull-confirm-unsaved file "modules")
    (org-canvas--create-file file)
    (with-current-buffer (org-canvas--find-file-noselect file)
      (dolist (mod (org-canvas--pull-sort-items remote))
        (let* ((mid (alist-get 'id mod))
               (mname (alist-get 'name mod))
               (items (alist-get 'items mod))
               (pos (org-canvas--pull-upsert-heading file mid mname)))
          (goto-char pos)
          (when mname (org-edit-headline mname))
          (org-canvas-org-save-sync-state pos mid)
          (cl-incf mod-count)
          ;; The items are rewritten from scratch, blank lines around
          ;; the old ones included, so a re-pull is byte-stable (#403).
          (org-canvas--pull-clear-entry-contents)
          (when items
            (setq item-count (+ item-count
                                (org-canvas--module-pull-insert-items items))))))
      ;; A module Canvas deleted is marked, never an item (#399).
      (setq unlisted (org-canvas--pull-mark-unlisted
                      (org-canvas--pull-gone-request
                       :file file :feature "modules" :remote remote
                       :id-field 'id :id-property "CANVAS_ID")))
      (org-canvas--pull-write-file-header)
      (org-canvas--save-buffer))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (let ((note (org-canvas--pull-gone-suffix unlisted)))
      (org-canvas--log-info org-canvas--logger
        "Modules pull complete: %d modules, %d items%s" mod-count item-count note)
      (message "Modules pull complete: %d modules, %d items%s."
               mod-count item-count note))))

(provide 'org-canvas-modules)
;;; org-canvas-modules.el ends here