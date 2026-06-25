;;; org-canvas-group-categories.el --- Pipeline-based Group Category Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;;; Configuration

(defcustom org-canvas-group-categories-file (org-canvas--path "group-categories.org")
  "Path to the group-categories.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-group-categories-file "group-categories.org")
(org-canvas-register-feature
 :name "Group Categories" :endpoint "group_categories"
 :file-var 'org-canvas-group-categories-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'name)
(org-canvas-register-properties "group-categories"
  :label "Group Categories"
  :file-var 'org-canvas-group-categories-file
  :query "LEVEL=1"
  :properties
  `((:org-prop "SELF_SIGNUP" :data-key :self_signup :type enum
     :values ,org-canvas--valid-self-signup-values :api-key "self_signup"
     :doc "Allow students to self-enroll in groups")
    (:org-prop "GROUP_LIMIT" :data-key :group_limit :type number
     :api-key "group_limit"
     :doc "Max members per group (requires SELF_SIGNUP)")
    (:org-prop "AUTO_LEADER" :data-key :auto_leader :type enum
     :values ,org-canvas--valid-auto-leader-values :api-key "auto_leader"
     :doc "How to assign group leaders")
    (:org-prop "CREATE_GROUP_COUNT" :data-key :create_group_count :type number
     :api-key "create_group_count"
     :doc "Number of groups to create automatically")))

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

(org-canvas-define-payload group-category
  :registry-key "group-categories"
  :format alist
  :title-key :title
  :title-api-key name)

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

;;;; Delete

(org-canvas-define-delete-all group-categories
  :endpoint "group_categories"
  :file org-canvas-group-categories-file
  :title-field 'name
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/group_categories/%s" org-canvas-base-url id)))

(org-canvas-define-delete-at-point group-category
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/group_categories/%s"
                           org-canvas-base-url id)))

;;;; Pull

(org-canvas-define-pull-item group-category
  :properties
  ((self_signup  "SELF_SIGNUP"  :type non-null)
   (group_limit  "GROUP_LIMIT"  :type number)
   (auto_leader  "AUTO_LEADER"  :type non-null)))

(org-canvas-define-pull group-categories
  :file org-canvas-group-categories-file
  :endpoint "group_categories"
  :title-field 'name
  :pull-item-fn #'org-canvas--group-category-pull-item)

(provide 'org-canvas-group-categories)
;;; org-canvas-group-categories.el ends here
