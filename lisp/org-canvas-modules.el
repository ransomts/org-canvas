;;; org-canvas-modules.el --- Pipeline-based Module Sync -*- lexical-binding: t; -*-

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
(require 'elog)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-modules-file (org-canvas--path "modules.org")
  "Path to the modules.org file."
  :type 'file
  :group 'org-canvas)

;;;; Helper Functions

(defun org-canvas--module-item-type-from-file (filepath)
  "Determine the Canvas module item type from FILEPATH.
Returns one of: File, Page, Discussion, Assignment, Quiz,
SubHeader, ExternalUrl."
  (let ((basename (file-name-nondirectory filepath)))
    (cond
     ((string-match-p "pages\\.org$" basename) "Page")
     ((string-match-p "assignments\\.org$" basename) "Assignment")
     ((string-match-p "discussions\\.org$" basename) "Discussion")
     ((string-match-p "quizzes\\.org$" basename) "Quiz")
     ((string-match-p "files\\.org$" basename) "File")
     ((string-match-p "announcements\\.org$" basename) "Discussion") ; Announcements are discussions
     (t "Page")))) ; Default to Page

(defun org-canvas--module-resolve-file-path (file modules-file-dir)
  "Resolve FILE relative to MODULES-FILE-DIR, with basename fallback.
First tries the full relative path.  If that fails, tries just the
filename in MODULES-FILE-DIR (handles ../sibling-dir/file.org links
where sibling dirs aren't nested).  Returns absolute path, or nil."
  (let ((abs-file (expand-file-name file modules-file-dir)))
    (if (file-exists-p abs-file) abs-file
      (let ((fallback (expand-file-name (file-name-nondirectory file) modules-file-dir)))
        (when (file-exists-p fallback)
          (elog-warning org-canvas--logger
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

(defun org-canvas--module-resolve-link (link-string modules-file-dir)
  "Resolve LINK-STRING to get the linked item's Canvas ID and type.
MODULES-FILE-DIR is the directory containing modules.org.
Returns a plist (:type TYPE :content-id ID :page-url URL :title TITLE) or nil."
  (when (and link-string
             (string-match "\\[\\[file:\\([^]:]+\\)\\(?:::\\*\\(.+\\)\\)?\\]\\[\\([^]]+\\)\\]\\]" link-string))
    (let* ((file (match-string 1 link-string))
           (heading (match-string 2 link-string))
           (title (match-string 3 link-string))
           (item-type (org-canvas--module-item-type-from-file file))
           (abs-file (org-canvas--module-resolve-file-path file modules-file-dir)))

      (elog-debug org-canvas--logger "[Module Item] Resolving: file=%s heading=%s type=%s"
        file (or heading "N/A") item-type)

      (cond
       ((not abs-file)
        (elog-warning org-canvas--logger "[Module Item] File not found: %s"
          (expand-file-name file modules-file-dir))
        nil)
       (t
        (let ((found (org-canvas--module-lookup-ids-in-file abs-file heading title)))
          (if found
              (let ((canvas-id (plist-get found :canvas-id))
                    (page-url (plist-get found :canvas-url)))
                (elog-debug org-canvas--logger "[Module Item] Resolved: type=%s id=%s url=%s"
                  item-type (or canvas-id "N/A") (or page-url "N/A"))
                (list :type item-type
                      :content-id (when canvas-id (string-to-number canvas-id))
                      :page-url page-url
                      :title title))
            (elog-warning org-canvas--logger "[Module Item] No CANVAS_ID found for: %s" heading)
            nil)))))))

(defun org-canvas--module-parse-prerequisite-ids (prereq-string)
  "Parse PREREQ-STRING into a list of module IDs.
Accepts comma-separated Canvas module IDs."
  (when prereq-string
    (mapcar #'string-trim (split-string prereq-string "," t))))

;;;; 1. Stage: Extraction - Module

(defun org-canvas--module-parse-entry ()
  "Extract module data from the Org heading at point (level 1)."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting module extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (published (org-canvas-org-get-boolean-property pom "PUBLISHED" t))
         (position (org-canvas-org-get-number-property pom "POSITION"))
         (unlock-at (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "UNLOCK_AT")))
         (prereq-ids (org-canvas--module-parse-prerequisite-ids
                      (org-canvas-org-get-property pom "PREREQUISITE_MODULE_IDS")))
         (require-sequential (org-canvas-org-get-boolean-property pom "REQUIRE_SEQUENTIAL_PROGRESSION"))
         (publish-final-grade (org-canvas-org-get-boolean-property pom "PUBLISH_FINAL_GRADE")))

    (when (or (null title) (string-empty-p title))
      (error "Module title cannot be empty at point %d" pom))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Module: '%s' (ID: %s)"
      title (or canvas-id "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Position: %d, Unlock: %s, Sequential: %s"
      position (or unlock-at "none") require-sequential)

    (list :title title
          :canvas-id canvas-id
          :published published
          :position (when (> position 0) position)
          :unlock-at unlock-at
          :prerequisite-module-ids prereq-ids
          :require-sequential-progress require-sequential
          :publish-final-grade publish-final-grade
          :pom pom)))

;;;; 1. Stage: Extraction - Module Items

(defun org-canvas--module-classify-unlinked-heading
    (heading-with-links raw-heading canvas-id indent pom)
  "Classify a module item heading whose link could not be fully resolved.
Distinguishes two cases:
  1. Heading contains a [[file:...]] link but the target has no CANVAS_ID
     yet — returns a plist with the correct :type (Page/Assignment/etc.)
     and nil :content-id so `sync-items' skips it.
  2. Heading is genuinely plain text — returns a SubHeader plist.
HEADING-WITH-LINKS is the raw buffer text (from `org-complex-heading-regexp'
group 4), RAW-HEADING is from `org-get-heading'.  CANVAS-ID, INDENT,
and POM are passed through to the returned plist."
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
                           :pom pom)
                   (list :type "SubHeader"
                         :title raw-heading
                         :canvas-id canvas-id
                         :indent indent
                         :pom pom))))
    (if link-file
        (elog-warning org-canvas--logger
          "[Stage 1: Parse] Unresolved link: '%s' (type: %s)"
          (or link-title raw-heading) link-type)
      (elog-info org-canvas--logger
        "[Stage 1: Parse] Processing SubHeader: '%s'" raw-heading))
    result))

(defun org-canvas--module-item-parse-entry (modules-file-dir)
  "Extract module item data from the Org heading at point (level 2).
MODULES-FILE-DIR is used to resolve relative file links."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting item extraction at point %d" (point))

  (let* ((pom (point))
         (raw-heading (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         ;; Get raw heading text from buffer with link syntax preserved.
         ;; org-get-heading strips [[...][...]] markup in Org 9.7+ (Emacs 30).
         (heading-with-links
          (save-excursion
            (beginning-of-line)
            (when (looking-at org-complex-heading-regexp)
              (match-string-no-properties 4))))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (indent (org-canvas-org-get-number-property pom "INDENT" 0))
         (completion-req (org-canvas-org-get-property pom "COMPLETION_REQUIREMENT"))
         (min-score (org-canvas-org-get-number-property pom "MIN_SCORE"))
         (external-url (org-canvas-org-get-property pom "EXTERNAL_URL"))
         (new-tab (org-canvas-org-get-boolean-property pom "NEW_TAB"))
         ;; Resolve the link - try raw buffer text first (has link syntax),
         ;; then fall back to org-get-heading result
         (link-info (unless external-url
                      (or (org-canvas--module-resolve-link heading-with-links modules-file-dir)
                          (org-canvas--module-resolve-link raw-heading modules-file-dir)))))

    (cond
     ;; External URL item
     (external-url
      (elog-info org-canvas--logger "[Stage 1: Parse] Processing ExternalUrl: '%s' (ID: %s)"
        raw-heading (or canvas-id "NEW"))
      (list :type "ExternalUrl"
            :title raw-heading
            :canvas-id canvas-id
            :indent indent
            :external-url external-url
            :new-tab new-tab
            :completion-requirement completion-req
            :min-score (when (and min-score (> min-score 0)) min-score)
            :pom pom))
     ;; No link resolved
     ((not link-info)
      (org-canvas--module-classify-unlinked-heading
       heading-with-links raw-heading canvas-id indent pom))
     ;; Regular linked item
     (t
      (elog-info org-canvas--logger "[Stage 1: Parse] Processing Item: '%s' -> %s (ID: %s)"
        (plist-get link-info :title)
        (plist-get link-info :type)
        (or canvas-id "NEW"))
      (list :type (plist-get link-info :type)
            :title (plist-get link-info :title)
            :content-id (plist-get link-info :content-id)
            :page-url (plist-get link-info :page-url)
            :canvas-id canvas-id
            :indent indent
            :completion-requirement completion-req
            :min-score (when (and min-score (> min-score 0)) min-score)
            :pom pom)))))

;;;; 2. Stage: Transformation - Module

(defun org-canvas--module-build-payload (data)
  "Convert module DATA to Canvas payload."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building module payload for '%s'" title)

    (let ((module (make-hash-table :test 'equal)))
      (puthash "name" title module)
      (puthash "published" (org-canvas--to-json-boolean (plist-get data :published)) module)

      (when (plist-get data :position)
        (puthash "position" (plist-get data :position) module))

      (when (plist-get data :unlock-at)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Unlock at: %s" (plist-get data :unlock-at))
        (puthash "unlock_at" (plist-get data :unlock-at) module))

      (when (plist-get data :prerequisite-module-ids)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Prerequisites: %s"
          (plist-get data :prerequisite-module-ids))
        (puthash "prerequisite_module_ids" (plist-get data :prerequisite-module-ids) module))

      (when (plist-get data :require-sequential-progress)
        (puthash "require_sequential_progress" t module))

      (when (plist-get data :publish-final-grade)
        (puthash "publish_final_grade" t module))

      (elog-debug org-canvas--logger "[Stage 2: Transform] Module payload complete")

      ;; Wrap in "module" key as required by Canvas API
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "module" module payload)
        payload))))

;;;; 2. Stage: Transformation - Module Item

(defun org-canvas--module-item-build-payload (data position)
  "Convert module item DATA to Canvas payload at POSITION."
  (let ((item-type (plist-get data :type))
        (title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building item payload for '%s' (type: %s)"
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

      ;; Completion requirement
      (when (plist-get data :completion-requirement)
        (puthash "completion_requirement[type]" (plist-get data :completion-requirement) item)
        (when (plist-get data :min-score)
          (puthash "completion_requirement[min_score]" (plist-get data :min-score) item)))

      (elog-debug org-canvas--logger "[Stage 2: Transform] Item payload complete")

      ;; Wrap in "module_item" key
      (let ((payload (make-hash-table :test 'equal)))
        (puthash "module_item" item payload)
        payload))))

;;;; 3. Stage: Execution - Module

(defun org-canvas--module-search-by-name (name)
  "Search for a module with NAME on Canvas.  Return nil on error."
  (elog-info org-canvas--logger "[Stage 3: Search] Looking for module '%s'..." name)
  (condition-case err
      (let* ((endpoint (org-canvas-api-course-endpoint "modules"))
             (params `(("search_term" . ,name)))
             (results (append (org-canvas-api-request 'GET endpoint :params params) nil))
             (count (length results)))
        (elog-debug org-canvas--logger "[Stage 3: Search] Found %d results" count)
        (let ((found (cl-find-if (lambda (m) (string-equal (alist-get 'name m) name)) results)))
          (if found
              (elog-info org-canvas--logger "[Stage 3: Search] Found exact match: ID=%s"
                (alist-get 'id found))
            (elog-debug org-canvas--logger "[Stage 3: Search] No exact match found"))
          found))
    (error
     (elog-warning org-canvas--logger "[Stage 3: Search] Failed: %s" (error-message-string err))
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
         (items (append (org-canvas-api-request 'GET items-endpoint
                          :params org-canvas--api-max-per-page) nil)))
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

    (elog-info org-canvas--logger "[Stage 3: Execute] %s Item '%s' to module %s"
      method title module-id)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data payload)))
          (elog-info org-canvas--logger "[Stage 3: Execute] %s successful for item '%s'" method title)
          response)
      (error
       (elog-error org-canvas--logger "[Stage 3: Execute] Item failed: %s" (error-message-string err))

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

(defun org-canvas--module-finalize (data response)
  "Update local Org file with CANVAS_ID using DATA and RESPONSE."
  (org-canvas--finalize-item data response))

;;;; Main Sync Functions

(defun org-canvas--module-collect-item-markers (module-pom)
  "Collect deduplicated markers for all child headings of MODULE-POM."
  (let ((item-markers nil))
    ;; Collect all level-2 children of this module
    (save-excursion
      (goto-char module-pom)
      (let ((module-level (org-current-level)))
        (while (and (org-get-next-sibling)
                    (> (org-current-level) module-level))
          (when (= (org-current-level) (1+ module-level))
            (push (point-marker) item-markers)))))
    ;; Also check immediate children using outline structure
    (save-excursion
      (goto-char module-pom)
      (when (org-goto-first-child)
        (push (point-marker) item-markers)
        (while (org-get-next-sibling)
          (push (point-marker) item-markers))))
    (delete-dups (nreverse item-markers))))

(defun org-canvas--module-sync-items (module-id module-pom modules-file-dir)
  "Sync all items for MODULE-ID starting from MODULE-POM.
MODULES-FILE-DIR is used for resolving links.
Returns (success-count . fail-count)."
  (let ((item-markers (org-canvas--module-collect-item-markers module-pom))
        (success-count 0)
        (fail-count 0)
        (position 1))
    (elog-info org-canvas--logger "[Module Items] Found %d items to sync" (length item-markers))

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
                      (elog-warning org-canvas--logger "[Module Item] Skipping '%s': no linked content synced"
                        (plist-get data :title))
                      (setq fail-count (1+ fail-count)))
                  (let* ((payload (org-canvas--module-item-build-payload data position))
                         (response (org-canvas--module-item-push-to-api module-id data payload)))
                    (org-canvas--module-finalize data response)
                    (setq success-count (1+ success-count))
                    (setq position (1+ position)))))
            (error
             (setq fail-count (1+ fail-count))
             (elog-error org-canvas--logger "[FAILED] Item at point %d: %s"
               (marker-position marker) (error-message-string err)))))))

    (cons success-count fail-count)))

(defun org-canvas--module-sync-one (marker modules-file-dir)
  "Sync a single module at MARKER and its child items.
MODULES-FILE-DIR is the directory containing modules.org.
Returns a plist (:module-ok BOOL :item-success N :item-fail N)."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (condition-case err
          (let* ((data (org-canvas--module-parse-entry))
                 (payload (org-canvas--module-build-payload data))
                 (response (org-canvas--module-push-to-api data payload))
                 (module-id (alist-get 'id response))
                 (i-success 0) (i-fail 0))
            (org-canvas--module-finalize data response)
            (when module-id
              (elog-info org-canvas--logger "[Module Items] Syncing items for module %s..." module-id)
              (let ((item-counts (org-canvas--module-sync-items
                                  module-id (marker-position marker) modules-file-dir)))
                (setq i-success (car item-counts))
                (setq i-fail (cdr item-counts))))
            (list :module-ok t :title (plist-get data :title)
                  :item-success i-success :item-fail i-fail))
        (error
         (elog-error org-canvas--logger "[FAILED] Module at point %d: %s"
           (marker-position marker) (error-message-string err))
         (list :module-ok nil :error-msg (error-message-string err)
               :item-success 0 :item-fail 0))))))

(defun org-canvas--module-sync-preflight ()
  "Validate modules file and verify course access.
Returns the expanded modules file path."
  (let ((modules-file (expand-file-name org-canvas-modules-file)))
    (unless (and modules-file (file-exists-p modules-file))
      (error "Modules file not found: %s" modules-file))
    (display-buffer (get-buffer-create org-canvas--log-buffer-name))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> STARTING MODULE SYNC")
    (elog-info org-canvas--logger "File: %s" modules-file)
    (elog-info org-canvas--logger "Course: %s | URL: %s" org-canvas-course-id org-canvas-base-url)
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger "[Pre-flight] Verifying course access...")
    (condition-case err
        (progn
          (org-canvas-api-request 'GET (org-canvas-api-course-endpoint ""))
          (elog-info org-canvas--logger "[Pre-flight] Course accessible"))
      (error
       (elog-warning org-canvas--logger "[Pre-flight] Warning: %s" (error-message-string err))))
    modules-file))

;;;###autoload
(defun org-canvas-sync-modules ()
  "Synchronize modules and their items to Canvas."
  (interactive)
  (org-canvas-clear-log)
  (let* ((modules-file (org-canvas--module-sync-preflight))
         (module-markers nil)
          (module-success 0)
          (module-fail 0)
          (item-success 0)
          (item-fail 0)
          (modules-file-dir (file-name-directory modules-file)))

      (with-current-buffer (find-file-noselect modules-file)
        (setq module-markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))

      (elog-info org-canvas--logger "Found %d modules to sync" (length module-markers))

      (dolist (marker module-markers)
        (elog-info org-canvas--logger "========================================")
        (let ((result (org-canvas--module-sync-one marker modules-file-dir)))
          (if (plist-get result :module-ok)
              (progn
                (setq module-success (1+ module-success))
                (message "Modules [%d/%d] Synced '%s'"
                  (+ module-success module-fail) (length module-markers)
                  (plist-get result :title)))
            (setq module-fail (1+ module-fail))
            (message "Modules [%d/%d] FAILED: %s"
              (+ module-success module-fail) (length module-markers)
              (plist-get result :error-msg)))
          (setq item-success (+ item-success (plist-get result :item-success)))
          (setq item-fail (+ item-fail (plist-get result :item-fail)))))

      (with-current-buffer (find-file-noselect modules-file)
        (save-buffer)
        (elog-info org-canvas--logger "Saved %s" modules-file))

      (elog-info org-canvas--logger "========================================")
      (elog-info org-canvas--logger ">>> MODULE SYNC COMPLETE")
      (elog-info org-canvas--logger "Modules: %d success, %d failed" module-success module-fail)
      (elog-info org-canvas--logger "Items: %d success, %d failed" item-success item-fail)
      (elog-info org-canvas--logger "========================================")
      (message "Module Sync: %d modules (%d items) succeeded, %d modules (%d items) failed."
               module-success item-success module-fail item-fail)))

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

;;;###autoload
(defun org-canvas-delete-module-at-point ()
  "Delete the Canvas module associated with the current Org heading."
  (interactive)
  (org-back-to-heading t)
  (let* ((pom (point))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t))))

    (unless canvas-id
      (user-error "No CANVAS_ID property found for this heading"))

    (when (y-or-n-p (format "Delete module '%s' (and all items) from Canvas? " title))
      (org-canvas-clear-log)
      (display-buffer (get-buffer-create org-canvas--log-buffer-name))
      (elog-info org-canvas--logger "Deleting module '%s' (ID: %s)..." title canvas-id)

      (condition-case err
          (progn
            (org-canvas-api-request 'DELETE (org-canvas-api-course-endpoint "modules/%s" canvas-id))
            (elog-info org-canvas--logger "Successfully deleted from Canvas")
            (org-canvas--module-clear-children-properties pom)
            (elog-info org-canvas--logger "Cleaned local properties")
            (message "Module '%s' deleted." title))
        (error
         (elog-error org-canvas--logger "Failed to delete: %s" (error-message-string err))
         (message "Failed to delete module. Check logs."))))))

;;;; Pull

(defconst org-canvas--module-type-to-file-map
  '(("Page" . "pages.org")
    ("Assignment" . "assignments.org")
    ("Quiz" . "quizzes.org")
    ("Discussion" . "discussions.org")
    ("File" . "files.org"))
  "Map Canvas module item types to their corresponding Org files.")

(defun org-canvas--module-resolve-item-link (item-type content-id title)
  "Resolve a module item to an Org cross-file link.
ITEM-TYPE is a Canvas type string (\"Page\", \"Assignment\", etc.).
CONTENT-ID is the Canvas ID of the target item.
TITLE is the item title for display.
Returns a link string or just the title if resolution fails."
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
                (format "[[file:%s::*%s][%s]]"
                        org-file heading-name (or title heading-name))
              (or title "Untitled"))))))))

(defun org-canvas--module-pull-insert-items (items)
  "Insert level-2 headings for module ITEMS at point.
Returns the count of items inserted."
  (let ((count 0))
    (insert "\n")
    (dolist (item (append items nil))
      (let* ((item-type (alist-get 'type item))
             (item-title (alist-get 'title item))
             (item-id (alist-get 'id item))
             (content-id (alist-get 'content_id item))
             (indent (alist-get 'indent item)))
        (cond
         ((equal item-type "SubHeader")
          (insert (format "** %s\n" (or item-title "Section")))
          (org-back-to-heading t)
          (org-canvas-org-set-property (point) "CANVAS_ID"
                                       (format "%s" item-id))
          (goto-char (save-excursion (org-end-of-subtree t) (point))))
         ((equal item-type "ExternalUrl")
          (let ((ext-url (alist-get 'external_url item))
                (new-tab (alist-get 'new_tab item)))
            (insert (format "** %s\n" (or item-title "External Link")))
            (org-back-to-heading t)
            (org-canvas-org-set-property (point) "CANVAS_ID"
                                         (format "%s" item-id))
            (when ext-url
              (org-canvas-org-set-property (point) "EXTERNAL_URL" ext-url))
            (when new-tab
              (org-canvas-org-set-property (point) "NEW_TAB" "true"))
            (when indent
              (org-canvas-org-set-property (point) "INDENT"
                                           (format "%s" indent)))
            (goto-char (save-excursion
                         (org-end-of-subtree t) (point)))))
         (t
          (let ((link (org-canvas--module-resolve-item-link
                       item-type content-id item-title)))
            (insert (format "** %s\n" link))
            (org-back-to-heading t)
            (org-canvas-org-set-property (point) "CANVAS_ID"
                                         (format "%s" item-id))
            (when indent
              (org-canvas-org-set-property (point) "INDENT"
                                           (format "%s" indent)))
            (goto-char (save-excursion
                         (org-end-of-subtree t) (point))))))
        (cl-incf count)))
    count))

;;;###autoload
(defun org-canvas-pull-modules ()
  "Pull modules from Canvas into modules.org."
  (interactive)
  (org-canvas--start-operation "PULLING MODULES")
  (let* ((file (expand-file-name org-canvas-modules-file))
         (endpoint (org-canvas-api-course-endpoint "modules"))
         (remote (org-canvas-api-request-all-pages
                  'GET endpoint '(("include[]" . "items"))))
         (mod-count 0) (item-count 0))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (find-file-noselect file)
      (dolist (mod remote)
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
      (save-buffer))
    (elog-info org-canvas--logger
      "Modules pull complete: %d modules, %d items" mod-count item-count)
    (message "Modules pull complete: %d modules, %d items."
             mod-count item-count)))

(provide 'org-canvas-modules)
;;; org-canvas-modules.el ends here
