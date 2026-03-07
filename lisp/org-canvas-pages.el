;;; org-canvas-pages.el --- Pipeline-based Wiki Page Sync -*- lexical-binding: t; -*-

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
;; EDITING_ROLES - Who can edit ("teachers", "students")
;;
;; URL HANDLING
;; ============
;; Pages use CANVAS_URL instead of CANVAS_ID for identification.
;; Canvas generates the URL from the title (slugified).
;; After first sync, the URL is saved and used for updates.
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
(require 'elog)

;;;; Configuration

(defcustom org-canvas-pages-file (org-canvas--path "pages.org")
  "Path to the pages.org file."
  :type 'file
  :group 'org-canvas)

;;;; 1. Stage: Extraction

(defun org-canvas--page-validate-editing-roles (raw)
  "Validate comma-separated EDITING_ROLES string RAW.
Logs warnings for invalid roles.  Returns RAW unchanged."
  (when raw
    (let ((roles (mapcar #'string-trim (split-string raw "," t))))
      (dolist (role roles)
        (unless (member role '("teachers" "students" "members" "public"))
          (when (boundp 'org-canvas--logger)
            (elog-warning org-canvas--logger
              "[Validate] EDITING_ROLES: '%s' is not valid (expected: teachers, students, members, public)"
              role))
          (message "Warning: EDITING_ROLES '%s' is not valid" role)))))
  raw)

(defun org-canvas--page-read-props (pom)
  "Read raw property strings from the page heading at POM."
  (list :title-raw (org-get-heading t t t t)
        :canvas-url (org-entry-get pom "CANVAS_URL")
        :published-raw (org-entry-get pom "PUBLISHED")
        :front-page-raw (org-entry-get pom "FRONT_PAGE")
        :editing-roles-raw (org-entry-get pom "EDITING_ROLES")
        :todo-date-raw (org-entry-get pom "TODO_DATE")
        :notify-of-update-raw (org-entry-get pom "NOTIFY_OF_UPDATE")))

(defun org-canvas--page-transform-props (raw)
  "Transform raw property strings RAW into typed page data.
Pure function — no buffer access."
  (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
        :canvas-url (plist-get raw :canvas-url)
        :published (org-canvas--interpret-boolean (plist-get raw :published-raw) t)
        :front_page (org-canvas--interpret-boolean (plist-get raw :front-page-raw))
        :editing_roles (org-canvas--page-validate-editing-roles
                        (plist-get raw :editing-roles-raw))
        :student_todo_at (org-canvas-org-parse-timestamp (plist-get raw :todo-date-raw))
        :notify_of_update (org-canvas--interpret-boolean
                           (plist-get raw :notify-of-update-raw))))

(defun org-canvas--page-parse-entry ()
  "Extract wiki page data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (raw (org-canvas--page-read-props pom))
         (data (org-canvas--page-transform-props raw)))

    (org-canvas--require-title (plist-get data :title) pom "Page")

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Page: '%s' (URL: %s)"
              (plist-get data :title) (or (plist-get data :canvas-url) "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: published=%s, front-page=%s, editing-roles=%s"
      (plist-get data :published) (plist-get data :front_page)
      (or (plist-get data :editing_roles) "default"))

    ;; Perform the export safely, resolving cross-file links to Canvas URLs
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((content
           (condition-case export-err
               (org-canvas--export-subtree-body-to-html)
             (error
              (elog-error org-canvas--logger "[Stage 1: Export] Failed: %s" (error-message-string export-err))
              (signal (car export-err) (cdr export-err))))))

      (elog-info org-canvas--logger "[Stage 1: Parse] Body size: %d chars" (length content))

      (plist-put data :body content)
      (plist-put data :pom pom)
      data)))

;;;; 2. Stage: Transformation

(defun org-canvas--page-build-payload (data)
  "Convert DATA to Canvas payload using Hash Tables."
  (let ((title (plist-get data :title)))
    (when (string-empty-p title)
      (elog-error org-canvas--logger "[Stage 2: Transform] Empty title!")
      (error "Page title cannot be empty during payload build"))

    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((inner-page (make-hash-table :test 'equal))
          (outer-wrapper (make-hash-table :test 'equal)))

      (puthash "title" title inner-page)
      (puthash "body" (plist-get data :body) inner-page)
      (puthash "published" (org-canvas--to-json-boolean (plist-get data :published)) inner-page)

      (when (plist-get data :front_page)
        (puthash "front_page" t inner-page)
        (elog-debug org-canvas--logger "[Stage 2: Transform] Setting as front page"))

      (org-canvas--puthash-when inner-page data :editing_roles "editing_roles")
      (org-canvas--puthash-when inner-page data :student_todo_at "student_todo_at")
      (when (plist-get data :notify_of_update)
        (puthash "notify_of_update" t inner-page))

      (puthash "wiki_page" inner-page outer-wrapper)

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      outer-wrapper)))

;;;; 3. Stage: Execution Helper

(defun org-canvas--page-search-by-title (title)
  "Search for a page with TITLE on Canvas.  Return nil on error."
  (org-canvas--search-item "pages" title))

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
  :find-fn #'org-canvas--page-search-by-title
  :pull-item-fn #'org-canvas--page-pull-item)

;; Pages use 'url instead of 'id, and we skip the front page
(org-canvas-define-push-at-point page
  :parse #'org-canvas--page-parse-entry
  :build #'org-canvas--page-build-payload
  :endpoint "pages"
  :id-key :canvas-url
  :id-field 'url
  :id-property "CANVAS_URL"
  :find-fn #'org-canvas--page-search-by-title
  :pull-item-fn #'org-canvas--page-pull-item)

(org-canvas-define-delete-all pages
  :endpoint "pages"
  :file org-canvas-pages-file
  :id-field 'url
  :id-property "CANVAS_URL"
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))

(org-canvas-define-delete-at-point page
  :endpoint "pages/%s"
  :id-property "CANVAS_URL")

;;;; Pull

(defun org-canvas--page-pull-item (item pos)
  "Set per-item properties for a pulled page.
ITEM is the API response alist, POS is the heading position.
Fetches the full page detail to get body content."
  (let* ((url (alist-get 'url item))
         (detail-url (org-canvas-api-course-endpoint "pages/%s" url))
         (detail (condition-case nil
                     (org-canvas-api-request 'GET detail-url)
                   (error nil)))
         (body (when detail (alist-get 'body detail))))
    (org-canvas--pull-insert-body body)))

(org-canvas-define-pull pages
  :file org-canvas-pages-file
  :endpoint "pages"
  :id-field 'url
  :id-property "CANVAS_URL"
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t))
  :item-fn #'org-canvas--page-pull-item)

(provide 'org-canvas-pages)
;;; org-canvas-pages.el ends here
