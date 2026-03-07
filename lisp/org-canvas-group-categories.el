;;; org-canvas-group-categories.el --- Pipeline-based Group Category Sync -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the sync pipeline for Canvas Group Categories.
;;
;; FILE STRUCTURE
;; ==============
;; In group-categories.org:
;;   - Level 1 headings = Group Categories
;;
;; PROPERTIES
;; ==========
;; SELF_SIGNUP        - "enabled" or "restricted" (optional)
;; GROUP_LIMIT        - Max members per group (requires SELF_SIGNUP)
;; AUTO_LEADER        - "first" or "random" (optional)
;; CREATE_GROUP_COUNT - Number of groups to create (optional)
;;
;; API NOTES
;; =========
;; Group categories have a split URL pattern:
;;   POST: /api/v1/courses/:course_id/group_categories
;;   PUT:  /api/v1/group_categories/:id
;;   DELETE: /api/v1/group_categories/:id
;; This prevents use of the generic push/delete helpers.

;;; Code:

(require 'org-canvas-core)
(require 'elog)

;;;; Configuration

(defcustom org-canvas-group-categories-file (org-canvas--path "group-categories.org")
  "Path to the group-categories.org file."
  :type 'file
  :group 'org-canvas)

(defconst org-canvas--valid-self-signup-values
  '("enabled" "restricted")
  "Valid values for SELF_SIGNUP.")

(defconst org-canvas--valid-auto-leader-values
  '("first" "random")
  "Valid values for AUTO_LEADER.")

;;;; 1. Stage: Extraction

(defun org-canvas--group-category-read-props (pom)
  "Read raw property strings from the group category heading at POM."
  (list :title-raw (org-get-heading t t t t)
        :canvas-id (org-entry-get pom "CANVAS_ID")
        :self-signup-raw (org-entry-get pom "SELF_SIGNUP")
        :group-limit-raw (org-entry-get pom "GROUP_LIMIT")
        :auto-leader-raw (org-entry-get pom "AUTO_LEADER")
        :create-group-count-raw (org-entry-get pom "CREATE_GROUP_COUNT")))

(defun org-canvas--group-category-transform-props (raw)
  "Transform raw property strings RAW into typed group category data.
Pure function — no buffer access."
  (let ((group-limit (plist-get raw :group-limit-raw))
        (create-count (plist-get raw :create-group-count-raw)))
    (list :title (org-canvas--strip-statistics-cookie (plist-get raw :title-raw))
          :canvas-id (plist-get raw :canvas-id)
          :self_signup (org-canvas--validate-property
                        (plist-get raw :self-signup-raw)
                        org-canvas--valid-self-signup-values
                        "SELF_SIGNUP")
          :group_limit (when group-limit
                         (org-canvas--safe-string-to-number group-limit "GROUP_LIMIT"))
          :auto_leader (org-canvas--validate-property
                        (plist-get raw :auto-leader-raw)
                        org-canvas--valid-auto-leader-values
                        "AUTO_LEADER")
          :create_group_count (when create-count
                                (org-canvas--safe-string-to-number
                                 create-count "CREATE_GROUP_COUNT")))))

(defun org-canvas--group-category-parse-entry ()
  "Extract data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (data (org-canvas--group-category-transform-props
                (org-canvas--group-category-read-props pom))))

    (org-canvas--require-title (plist-get data :title) pom "Group category")

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Group Category: '%s' (ID: %s)"
              (plist-get data :title) (or (plist-get data :canvas-id) "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: self-signup=%s, group-limit=%s, auto-leader=%s"
               (or (plist-get data :self_signup) "nil")
               (or (plist-get data :group_limit) "nil")
               (or (plist-get data :auto_leader) "nil"))

    (plist-put data :pom pom)
    data))

;;;; 2. Stage: Transformation

(defun org-canvas--group-category-build-payload (data)
  "Convert the extracted DATA plist into a Canvas API-compatible alist."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((base `((name . ,title))))
      (setq base (org-canvas--push-non-nil-fields data
                   '((:self_signup . self_signup)
                     (:group_limit . group_limit)
                     (:auto_leader . auto_leader)
                     (:create_group_count . create_group_count))
                   base))
      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      base)))

;;;; 3. Stage: Execution

(defun org-canvas--group-category-push-to-api (data payload)
  "Send PAYLOAD derived from DATA to Canvas API.
Group categories use split endpoints: POST is course-scoped,
PUT is global."
  (org-canvas--push-to-api data payload
    :endpoint "group_categories"
    :find-fn (lambda (title) (org-canvas--search-item "group_categories" title :match-field 'name))
    :put-url-fn (lambda (id) (format "%s/api/v1/group_categories/%s" org-canvas-base-url id))))

;;;; Main Sync Function

;; Generate org-canvas-sync-group-categories using the pipeline macro
(org-canvas-define-sync group-categories
  :file org-canvas-group-categories-file
  :parse #'org-canvas--group-category-parse-entry
  :build #'org-canvas--group-category-build-payload
  :push #'org-canvas--group-category-push-to-api
  :endpoint "group_categories"
  :pull-item-fn #'org-canvas--group-category-pull-item)

(org-canvas-define-push-at-point group-category
  :parse #'org-canvas--group-category-parse-entry
  :build #'org-canvas--group-category-build-payload
  :push #'org-canvas--group-category-push-to-api
  :endpoint "group_categories"
  :pull-item-fn #'org-canvas--group-category-pull-item)

;;;; Delete

(defun org-canvas-delete-all-group-categories ()
  "Delete all group categories from Canvas.
Uses course-scoped GET but global DELETE endpoint."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((url (org-canvas-api-course-endpoint "group_categories")))
    (elog-info org-canvas--logger "[Delete] Fetching group categories...")
    (let ((items (org-canvas-api-request-all-pages 'GET url)))
      (elog-info org-canvas--logger "[Delete] Found %d group categories" (length items))
      (dolist (item items)
        (let* ((id (alist-get 'id item))
               (name (alist-get 'name item))
               (del-url (format "%s/api/v1/group_categories/%s" org-canvas-base-url id)))
          (condition-case err
              (progn
                (org-canvas-api-request 'DELETE del-url)
                (elog-info org-canvas--logger "[Delete] Deleted group category '%s' (ID: %s)" name id))
            (error
             (elog-warning org-canvas--logger "[Delete] Failed to delete '%s': %s"
                           name (error-message-string err))))))
      ;; Clear CANVAS_ID and LAST_SYNCED from local file
      (let ((file (expand-file-name org-canvas-group-categories-file)))
        (when (file-exists-p file)
          (org-canvas--for-each-entry
           file "LEVEL=1"
           (lambda ()
             (let ((pom (point)))
               (org-entry-delete pom "CANVAS_ID")
               (org-entry-delete pom "LAST_SYNCED")
               (org-entry-delete pom "PAYLOAD_HASH"))))
          (with-current-buffer (find-file-noselect file)
            (save-buffer))))
      (elog-info org-canvas--logger "[Delete] Group categories deletion complete"))))

;;;###autoload
(defun org-canvas-delete-group-category-at-point ()
  "Delete the Canvas group category at the current Org heading.
Uses global endpoint DELETE /group_categories/:id."
  (interactive)
  (org-back-to-heading t)
  (let* ((pom (point))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (title (org-get-heading t t t t)))
    (unless canvas-id
      (user-error "No CANVAS_ID property found for this heading"))
    (when (y-or-n-p (format "Delete '%s' from Canvas? " title))
      (org-canvas-clear-log)
      (display-buffer (get-buffer-create org-canvas--log-buffer-name))
      (elog-info org-canvas--logger "Deleting group category '%s' (ID: %s)..." title canvas-id)
      (condition-case err
          (let ((url (format "%s/api/v1/group_categories/%s" org-canvas-base-url canvas-id)))
            (org-canvas-api-request 'DELETE url)
            (org-entry-delete pom "CANVAS_ID")
            (org-entry-delete pom "LAST_SYNCED")
            (org-entry-delete pom "PAYLOAD_HASH")
            (save-buffer)
            (elog-info org-canvas--logger "Deleted group category '%s'" title)
            (message "Deleted '%s' from Canvas" title))
        (error
         (elog-error org-canvas--logger "Failed to delete '%s': %s"
                     title (error-message-string err))
         (message "Delete failed: %s" (error-message-string err)))))))

;;;; Pull

(defun org-canvas--group-category-pull-item (item pos)
  "Set per-item properties for a pulled group category.
ITEM is the API response alist, POS is the heading position."
  (let ((self-signup (org-canvas--alist-get-non-null 'self_signup item))
        (group-limit (alist-get 'group_limit item))
        (auto-leader (org-canvas--alist-get-non-null 'auto_leader item)))
    (when self-signup
      (org-canvas-org-set-property pos "SELF_SIGNUP" self-signup))
    (when (and group-limit (> group-limit 0))
      (org-canvas-org-set-property pos "GROUP_LIMIT" (format "%s" group-limit)))
    (when auto-leader
      (org-canvas-org-set-property pos "AUTO_LEADER" auto-leader))))

(org-canvas-define-pull group-categories
  :file org-canvas-group-categories-file
  :endpoint "group_categories"
  :title-field 'name
  :item-fn #'org-canvas--group-category-pull-item)

(provide 'org-canvas-group-categories)
;;; org-canvas-group-categories.el ends here
