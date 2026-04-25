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
 :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
(org-canvas-register-properties "pages"
  :label "Pages"
  :file-var 'org-canvas-pages-file
  :query "LEVEL=1"
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :api-key "published" :boolean-json t)
    (:org-prop "FRONT_PAGE" :data-key :front_page :type boolean
     :api-key "front_page")
    (:org-prop "EDITING_ROLES" :data-key :editing_roles :type csv-enum
     :values ,org-canvas--valid-editing-roles :api-key "editing_roles")
    (:org-prop "TODO_DATE" :data-key :student_todo_at :type timestamp
     :api-key "student_todo_at")
    (:org-prop "NOTIFY_OF_UPDATE" :data-key :notify_of_update :type boolean
     :api-key "notify_of_update")))

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
   ("FRONT_PAGE"      :front_page      :type boolean)
   ("EDITING_ROLES"   :editing_roles   :type string)
   ("TODO_DATE"       :student_todo_at :type timestamp)
   ("NOTIFY_OF_UPDATE" :notify_of_update :type boolean)))

;;;; 2. Stage: Transformation

(defun org-canvas--page-pre-build-check (data _payload)
  "Validate page DATA before building payload.  Return PAYLOAD unchanged."
  (when (string-empty-p (plist-get data :title))
    (org-canvas--log-error org-canvas--logger "[Stage 2: Transform] Empty title!")
    (org-canvas--signal 'org-canvas-validation-error
      "Page title cannot be empty during payload build"))
  _payload)

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
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))

(org-canvas-define-delete-at-point page
  :endpoint "pages/%s"
  :id-property "CANVAS_URL")

;;;; Pull

(defun org-canvas--page-pull-item (item _pos)
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
  :pull-item-fn #'org-canvas--page-pull-item)

(provide 'org-canvas-pages)
;;; org-canvas-pages.el ends here
