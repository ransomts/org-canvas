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
  (let ((roles (mapcar #'string-trim (split-string raw "," t))))
    (dolist (role roles)
      (unless (member role '("teachers" "students" "members" "public"))
        (when (boundp 'org-canvas--logger)
          (elog-warning org-canvas--logger
            "[Validate] EDITING_ROLES: '%s' is not valid (expected: teachers, students, members, public)"
            role))
        (message "Warning: EDITING_ROLES '%s' is not valid" role))))
  raw)

(defun org-canvas--page-parse-entry ()
  "Extract wiki page data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-url (org-canvas-org-get-property pom "CANVAS_URL"))
         (published (org-canvas-org-get-boolean-property pom "PUBLISHED" t))
         (front-page (org-canvas-org-get-boolean-property pom "FRONT_PAGE"))
         (editing-roles (let ((raw (org-canvas-org-get-property pom "EDITING_ROLES")))
                          (when raw
                            (org-canvas--page-validate-editing-roles raw))))
         (todo-date (org-canvas-org-parse-timestamp (org-canvas-org-get-property pom "TODO_DATE"))))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Page: '%s' (URL: %s)" title (or canvas-url "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: published=%s, front-page=%s, editing-roles=%s"
      published front-page (or editing-roles "default"))

    ;; Perform the export safely, resolving cross-file links to Canvas URLs
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((content
           (condition-case export-err
               (org-canvas--export-subtree-body-to-html)
             (error
              (elog-error org-canvas--logger "[Stage 1: Export] Failed: %s" (error-message-string export-err))
              (signal (car export-err) (cdr export-err))))))

      (elog-info org-canvas--logger "[Stage 1: Parse] Body size: %d chars" (length content))

      (list :title title
            :body content
            :canvas-url canvas-url
            :published published
            :front_page front-page
            :editing_roles editing-roles
            :student_todo_at todo-date
            :pom pom))))

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

      (when (plist-get data :editing_roles)
        (puthash "editing_roles" (plist-get data :editing_roles) inner-page))
      (when (plist-get data :student_todo_at)
        (puthash "student_todo_at" (plist-get data :student_todo_at) inner-page))

      (puthash "wiki_page" inner-page outer-wrapper)

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      outer-wrapper)))

;;;; 3. Stage: Execution

(defun org-canvas--page-search-by-title (title)
  "Search for a page with TITLE on Canvas.  Return nil on error."
  (org-canvas--search-item "pages" title))

(defun org-canvas--page-push-to-api (data payload)
  "Send PAYLOAD to Canvas API based on DATA.
Handles 404 on PUT by retrying as POST.
Handles Timeout by searching for the page."
  (org-canvas--push-to-api data payload
    :endpoint "pages"
    :id-key :canvas-url
    :find-fn #'org-canvas--page-search-by-title))

;;;; 4. Stage: Finalization

(defun org-canvas--page-finalize (data response)
  "Update local Org file with CANVAS_URL using DATA and RESPONSE."
  (org-canvas--finalize-item data response
    :id-field 'url
    :id-property "CANVAS_URL"))

;;;; Main Sync Functions

;; Generate org-canvas-sync-pages using the pipeline macro
(org-canvas-define-sync pages
  :file org-canvas-pages-file
  :parse #'org-canvas--page-parse-entry
  :build #'org-canvas--page-build-payload
  :push #'org-canvas--page-push-to-api
  :finalize #'org-canvas--page-finalize)

;; Generate org-canvas-delete-all-pages using the delete macro
;; Pages use 'url instead of 'id, and we skip the front page
(org-canvas-define-push-at-point page
  :parse #'org-canvas--page-parse-entry
  :build #'org-canvas--page-build-payload
  :push #'org-canvas--page-push-to-api
  :finalize #'org-canvas--page-finalize)

(org-canvas-define-delete-all pages
  :endpoint "pages"
  :file org-canvas-pages-file
  :id-field 'url
  :id-property "CANVAS_URL"
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))

(defun org-canvas-delete-page-at-point ()
  "Delete the Canvas page associated with the current Org heading."
  (interactive)
  (org-back-to-heading t)
  (let* ((pom (point))
         (canvas-url (org-canvas-org-get-property pom "CANVAS_URL"))
         (title (org-get-heading t t t t)))

    (unless canvas-url
      (user-error "No CANVAS_URL property found for this heading"))

    (when (y-or-n-p (format "Delete '%s' from Canvas? " title))
      (org-canvas-clear-log)
      (display-buffer (get-buffer-create "*canvas-log*"))
      (elog-info org-canvas--logger "Deleting page '%s' (URL: %s)..." title canvas-url)

      (condition-case err
          (progn
            (org-canvas-api-request 'DELETE (org-canvas-api-course-endpoint "pages/%s" canvas-url))
            (elog-info org-canvas--logger "Successfully deleted from Canvas")
            (org-canvas-clear-sync-properties pom)
            (elog-info org-canvas--logger "Cleaned local properties")
            (message "Page '%s' deleted." title))
        (error
         (elog-error org-canvas--logger "Failed to delete: %s" (cadr err))
         (message "Failed to delete page. Check logs."))))))

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
  :filter-fn (lambda (item) (eq (alist-get 'front_page item) t))
  :item-fn #'org-canvas--page-pull-item)

(provide 'org-canvas-pages)
;;; org-canvas-pages.el ends here
