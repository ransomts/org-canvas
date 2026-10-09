;;; org-canvas-pages.el --- Pipeline-based Wiki Page Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas Wiki Pages.
;;
;; FILE STRUCTURE
;; ==============
;; In pages.org:
;;   - Level 1 headings = Wiki Pages
;;   - Heading body = Page content (exported to HTML)
;;
;; PROPERTIES
;; ==========
;; CANVAS_URL   - URL slug (auto-populated after first sync)
;; FRONT_PAGE   - Set to "true" to make this the course home page
;; PUBLISHED    - Visibility ("true"/"false", defaults to true)
;; PUBLISH_AT   - Canvas publishes the page itself at this time
;; EDITING_ROLES - Who can edit ("teachers", "students")
;;
;; URL HANDLING
;; ============
;; Pages use CANVAS_URL instead of CANVAS_ID for identification.
;; Canvas generates the URL from the title (slugified).
;; After first sync, the URL is saved and used for updates.
;;
;; SCHEDULED PUBLICATION
;; =====================
;; PUBLISH_AT is Canvas's own `publish_at' (issue #379), not the
;; local-only schedule modules keep under the same name: Canvas holds
;; the page unpublished and publishes it at that time with no Emacs
;; running.  A heading with PUBLISH_AT never sends `published', since
;; any request that changes the published state clears the schedule.
;;
;; FRONT PAGE
;; ==========
;; Only one page can be the front page.  Setting FRONT_PAGE=true
;; will change the course home page to this page.
;;
;; HTML EXPORT
;; ===========
;; The heading body is exported to HTML using ox-html.
;; Subheadings, lists, code blocks, etc. are all converted.

;;; Code:

(require 'org-canvas-core)
(require 'ox-html)

(declare-function org-canvas--validate-page-structure "org-canvas-validate")

;;;; Configuration

(defcustom org-canvas-pages-file (org-canvas--path "pages.org")
  "Path to the pages.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-pages-file "pages.org")
(org-canvas-register-feature
 :name "Pages" :endpoint "pages"
 :file-var 'org-canvas-pages-file
 :id-field 'url :id-property "CANVAS_URL" :title-field 'title
 :web-pages '((:level 1 :id-property "CANVAS_URL" :path "pages/%s"
               :edit "pages/%s/edit"))
 :skip-fn (lambda (item) (eq (alist-get 'front_page item) t))
 :skip-reason "front page")
(org-canvas-register-properties "pages"
  :duplicate-titles t
  :label "Pages"
  :file-var 'org-canvas-pages-file
  :query "LEVEL=1"
  :body-api-key "body"
  :body-list-params '(("include[]" . "body"))
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :api-key "published" :boolean-json t
     :compare-p org-canvas--page-published-comparable-p
     :doc "Whether item is visible (default: true); not sent when PUBLISH_AT is set")
    ,org-canvas--no-module-property-spec
    (:org-prop "PUBLISH_AT" :data-key :publish_at :type timestamp
     :api-key "publish_at"
     :doc "Canvas publishes the page at this time (needs Scheduled Page Publication)")
    (:org-prop "FRONT_PAGE" :data-key :front_page :type boolean
     :api-key "front_page"
     :doc "Set as course front page")
    (:org-prop "EDITING_ROLES" :data-key :editing_roles :type csv-enum
     :values ,org-canvas--valid-editing-roles :api-key "editing_roles"
     :doc "Who can edit (teachers, students)")
    (:org-prop "TODO_DATE" :data-key :student_todo_at :type timestamp
     :api-key "student_todo_at"
     :doc "Add to student to-do list on date")
    (:org-prop "NOTIFY_OF_UPDATE" :data-key :notify_of_update :type boolean
     :api-key "notify_of_update"
     :doc "Notify students of changes (write-only)"))
  :structural-fn #'org-canvas--validate-page-structure)

;;;; Scheduled Publication

(defun org-canvas--page-scheduled-p (pom)
  "Return non-nil when the page heading at POM carries a PUBLISH_AT."
  (let ((publish-at (org-entry-get pom "PUBLISH_AT")))
    (and publish-at (not (string-empty-p (string-trim publish-at))))))

(defun org-canvas--page-published-comparable-p (pom _item)
  "Return non-nil when PUBLISHED at POM is a comparable opinion.
A scheduled page's published state is Canvas's to set: unpublished
until PUBLISH_AT, published after, and the push never sends it, so
comparing it would flag every page still waiting (issue #379).  Named
as the PUBLISHED spec's `:compare-p'."
  (not (org-canvas--page-scheduled-p pom)))

(defun org-canvas--page-drop-published-when-scheduled (data payload)
  "Remove `published' from PAYLOAD when DATA carries a publish time.
Canvas clears `publish_at' on any request that changes the published
state, so the default PUBLISHED: true would publish a waiting page at
once and cancel its schedule; with the field absent, a future
`publish_at' unpublishes the page and schedules it, and a past one
publishes it (issue #379).  Returns PAYLOAD."
  (when (plist-get data :publish_at)
    (remhash "published" (gethash "wiki_page" payload)))
  payload)

;;;; 1. Stage: Extraction

(defun org-canvas--page-validate-editing-roles (raw)
  "Validate comma-separated EDITING_ROLES string RAW.
Logs warnings for invalid roles.  Returns RAW unchanged."
  (when raw
    (let ((roles (mapcar #'string-trim (split-string raw "," t))))
      (dolist (role roles)
        (unless (member role org-canvas--valid-editing-roles)
          (when (boundp 'org-canvas--logger)
            (org-canvas--log-warning org-canvas--logger
              "[Validate] EDITING_ROLES: '%s' is not valid (expected: teachers, students, members, public)"
              role))
          (message "Warning: EDITING_ROLES '%s' is not valid" role)))))
  raw)

(org-canvas-define-parse page
  :body :body
  :id-key :canvas-url
  :id-property "CANVAS_URL"
  :entity-name "Page"
  :after-transform
  (lambda (data)
    (org-canvas--page-validate-editing-roles (plist-get data :editing_roles))
    data)
  :properties
  (("PUBLISHED"       :published       :type boolean :default t)
   ("PUBLISH_AT"      :publish_at      :type timestamp)
   ("FRONT_PAGE"      :front_page      :type boolean)
   ("EDITING_ROLES"   :editing_roles   :type string)
   ("TODO_DATE"       :student_todo_at :type timestamp)
   ("NOTIFY_OF_UPDATE" :notify_of_update :type boolean)))

;;;; 2. Stage: Transformation

(defun org-canvas--page-pre-build-check (data payload)
  "Validate page DATA before building payload.  Return PAYLOAD.
A scheduled page's payload loses `published', through
`org-canvas--page-drop-published-when-scheduled'."
  (when (string-empty-p (plist-get data :title))
    (org-canvas--log-error org-canvas--logger "[Stage 2: Transform] Empty title!")
    (org-canvas--signal 'org-canvas-validation-error
      "Page title cannot be empty during payload build"))
  (org-canvas--page-drop-published-when-scheduled data payload))

(org-canvas-define-payload page
  :registry-key "pages"
  :format hash-table
  :wrapper-key "wiki_page"
  :title-key :title
  :title-api-key "title"
  :body-key :body
  :body-api-key "body"
  :post-build-fn #'org-canvas--page-pre-build-check)

;;;; Main Sync Functions

;; Generate org-canvas-sync-pages using the pipeline macro
(org-canvas-define-sync pages
  :file org-canvas-pages-file
  :parse #'org-canvas--page-parse-entry
  :build #'org-canvas--page-build-payload
  :endpoint "pages"
  :id-key :canvas-url
  :id-field 'url
  :id-property "CANVAS_URL"
  :find-fn (lambda (title) (org-canvas--search-item "pages" title))
  :pull-item-fn #'org-canvas--page-pull-item)

(org-canvas-define-delete-all pages
  :endpoint "pages"
  :file org-canvas-pages-file
  :id-field 'url
  :id-property "CANVAS_URL"
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t))
  :skip-reason "front page")

(org-canvas-define-delete-at-point page
  :endpoint "pages/%s"
  :id-property "CANVAS_URL")

;;;; Pull

(defun org-canvas--page-pull-detail (item url)
  "Return the full page ITEM lists as URL, or nil when it cannot be read.
An ITEM that already carries `body' is the detail already: the module
fallback reads each page one at a time (issue #485).  A failed fetch is
logged and recorded in `org-canvas--pull-summary'."
  (if (assq 'body item)
      item
    (condition-case err
        (org-canvas-api-request
         'GET (org-canvas-api-course-endpoint "pages/%s" url))
      (org-canvas-api-error
       (org-canvas--log-warning org-canvas--logger
         "[Pull] page detail fetch failed for %s: %s"
         url (error-message-string err))
       (org-canvas--pull-summary-record
        :file (file-name-nondirectory org-canvas-pages-file)
        :item url
        :error (error-message-string err)
        :log-line (org-canvas--pull-summary-current-log-line))
       nil))))

(defun org-canvas--page-pull-item (item pos)
  "Set per-item properties for a pulled page.
ITEM is the API response alist, POS is the heading position.
Fetches the full page detail to get body content.  When the detail
fetch fails (after retries are exhausted) the error is recorded in
`org-canvas--pull-summary' and the body is left empty.  The heading's
:CANVAS_URL: property has already been set by `pull-process-item' from
the list response.  Also sets `:CANVAS_ID:' from the numeric page_id
for schema consistency with other content types — CANVAS_URL remains
the primary identifier used for push/sync, but pages now expose both.
An ITEM that already carries `body' is not fetched again.

Records :FRONT_PAGE: on the course home page so the pull round-trips
\(issue #82): pull no longer skips the front page, and without the
property it would come back as an ordinary page.  The property is
removed from a page Canvas no longer serves as the home page, so a
home page moved in the web UI does not leave two headings claiming it."
  (let* ((url (alist-get 'url item))
         (page-id (alist-get 'page_id item))
         (detail (org-canvas--page-pull-detail item url))
         (body (when detail (alist-get 'body detail))))
    (when page-id
      (org-canvas-org-set-property pos "CANVAS_ID" (format "%s" page-id)))
    ;; PUBLISHED, FRONT_PAGE, EDITING_ROLES and the rest come from the
    ;; registry (issue #135).  A page that is no longer the home page
    ;; arrives with `front_page' false, the property's default, so the
    ;; stale FRONT_PAGE is deleted rather than left claiming it.
    (org-canvas--pull-item-from-registry "pages" item pos)
    ;; A scheduled page's published state follows its PUBLISH_AT, and
    ;; the push never sends it, so a PUBLISHED: false written beside it
    ;; would only read as an intent the file cannot carry (issue #379).
    (when (alist-get 'publish_at item)
      (org-entry-delete pos "PUBLISHED"))
    (when detail
      (org-with-point-at pos
        (org-canvas--pull-insert-body body)))))

(defun org-canvas--pages-module-items (feature mod)
  "Return the items of MOD, a module of the Modules FEATURE, as a list.
A module whose listing left its items out is read through its own
items endpoint."
  (append (if (assq 'items mod)
              (alist-get 'items mod)
            (org-canvas-api-request-all-pages
             'GET (org-canvas--feature-item-url
                   feature (format "%s/items" (alist-get 'id mod)))))
          nil))

(defun org-canvas--pages-module-page-urls ()
  "Return the distinct `page_url's the course's module items name.
Nil when the modules cannot be read either."
  (condition-case err
      (let ((feature (org-canvas--registry-find-feature "Modules"))
            (urls nil))
        (dolist (mod (append (org-canvas-api-request-all-pages
                              'GET (org-canvas--feature-list-url feature)
                              '(("include[]" . "items")))
                             nil))
          (dolist (item (org-canvas--pages-module-items feature mod))
            (when (equal (alist-get 'type item) "Page")
              (when-let* ((url (alist-get 'page_url item)))
                (cl-pushnew url urls :test #'equal)))))
        (nreverse urls))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Pull] Pages: module items not readable either (%s)"
       (error-message-string err))
     nil)))

(defun org-canvas--pages-read-linked (url)
  "Return the page whose slug is URL, or nil when Canvas refuses it.
A refusal is recorded in the pull summary, a skip or an error as
`org-canvas--api-skip-error-p' says."
  (condition-case err
      (org-canvas-api-request
       'GET (org-canvas--feature-item-url
             (org-canvas--registry-find-feature "Pages") url))
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Pull] page %s not readable: %s" url (error-message-string err))
     (org-canvas--pull-summary-record
      :kind (if (org-canvas--api-skip-error-p err) 'skip 'error)
      :file (file-name-nondirectory org-canvas-pages-file)
      :item url
      :error (error-message-string err)
      :log-line (org-canvas--pull-summary-current-log-line))
     nil)))

(defun org-canvas--pages-list-from-modules (err)
  "Return the pages the course's modules link, read one at a time.
The `:list-fallback-fn' of the pages pull, called with ERR when Canvas
refuses the pages list: a course can disable its Pages tab, which 404s
the list, and still serve each page a module links (issue #485).  The
pull summary says the list came from modules, so a page no module
links is known to be missing.  Nil when no page could be read, and the
refusal then stands."
  (let ((pages (delq nil (mapcar #'org-canvas--pages-read-linked
                                 (org-canvas--pages-module-page-urls)))))
    (when pages
      (org-canvas--log-warning org-canvas--logger
        "[Pull] Pages list refused (%s); read %d page(s) the modules link"
        (error-message-string err) (length pages))
      (org-canvas--pull-summary-record
       :kind 'skip
       :file (file-name-nondirectory org-canvas-pages-file)
       :item "pages no module links"
       :error (format "%s; pulled the %d page(s) modules link, one at a time, so a page no module links is not in pages.org"
                      (error-message-string err) (length pages))
       :log-line (org-canvas--pull-summary-current-log-line))
      pages)))

;; No :skip-fn here on purpose (issue #82).  Push and delete-all guard
;; the front page because clobbering or deleting the course home page is
;; destructive; pull only writes local files, so skipping it left the one
;; page students land on as the one page missing from the source of truth
;; — invisible to `org-canvas-diff' and absent from a migration.
(org-canvas-define-pull pages
  :file org-canvas-pages-file
  :endpoint "pages"
  :id-field 'url
  :id-property "CANVAS_URL"
  :list-fallback-fn #'org-canvas--pages-list-from-modules
  :pull-item-fn #'org-canvas--page-pull-item)

(provide 'org-canvas-pages)
;;; org-canvas-pages.el ends here
