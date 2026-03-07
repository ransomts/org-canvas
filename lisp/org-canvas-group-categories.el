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

;;;; 1. Stage: Extraction

(org-canvas-define-parse group-category
  :entity-name "Group category"
  :properties
  (("SELF_SIGNUP"        :self_signup        :type enum
    :values org-canvas--valid-self-signup-values)
   ("GROUP_LIMIT"        :group_limit        :type number)
   ("AUTO_LEADER"        :auto_leader        :type enum
    :values org-canvas--valid-auto-leader-values)
   ("CREATE_GROUP_COUNT" :create_group_count :type number)))

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

(org-canvas-define-delete-all group-categories
  :endpoint "group_categories"
  :file org-canvas-group-categories-file
  :title-field 'name
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/group_categories/%s" org-canvas-base-url id)))

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
