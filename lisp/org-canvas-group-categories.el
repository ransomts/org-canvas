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
(require 'cl-lib)

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

(defun org-canvas--group-category-parse-entry ()
  "Extract data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (self-signup (org-canvas--validate-property
                       (org-canvas-org-get-property pom "SELF_SIGNUP")
                       org-canvas--valid-self-signup-values
                       "SELF_SIGNUP"))
         (group-limit (org-canvas-org-get-property pom "GROUP_LIMIT"))
         (auto-leader (org-canvas--validate-property
                       (org-canvas-org-get-property pom "AUTO_LEADER")
                       org-canvas--valid-auto-leader-values
                       "AUTO_LEADER"))
         (create-group-count (org-canvas-org-get-property pom "CREATE_GROUP_COUNT")))

    (when (or (null title) (string-empty-p title))
      (error "Group category title cannot be empty at point %d" pom))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Group Category: '%s' (ID: %s)" title (or canvas-id "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: self-signup=%s, group-limit=%s, auto-leader=%s"
               (or self-signup "nil") (or group-limit "nil") (or auto-leader "nil"))

    (list :title title
          :canvas-id canvas-id
          :self_signup self-signup
          :group_limit (when group-limit (org-canvas--safe-string-to-number group-limit "GROUP_LIMIT"))
          :auto_leader auto-leader
          :create_group_count (when create-group-count (org-canvas--safe-string-to-number create-group-count "CREATE_GROUP_COUNT"))
          :pom pom)))

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

(cl-defun org-canvas--group-category-push-to-api (data payload)
  "Send PAYLOAD derived from DATA to Canvas API.
Group categories use split endpoints: POST is course-scoped,
PUT is global."
  (let* ((canvas-id (plist-get data :canvas-id))
         (title (plist-get data :title))
         (method (if canvas-id 'PUT 'POST))
         (endpoint (if canvas-id
                       (format "%s/api/v1/group_categories/%s" org-canvas-base-url canvas-id)
                     (org-canvas-api-course-endpoint "group_categories"))))

    ;; Dry-run check
    (when org-canvas--dry-run
      (elog-info org-canvas--logger "[DRY-RUN] Would %s group category '%s' to %s"
                 method title endpoint)
      (cl-return-from org-canvas--group-category-push-to-api
        `((id . ,(or canvas-id "dry-run")))))

    (elog-info org-canvas--logger "[Stage 3: Execute] %s '%s' to %s" method title endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data payload)))
          (elog-info org-canvas--logger "[Stage 3: Execute] Success for '%s'" title)
          response)
      (error
       (let ((msg (error-message-string err)))
         (cond
          ;; Timeout recovery: search Canvas for the item by name
          ((org-canvas--timeout-error-p err)
           (elog-warning org-canvas--logger "[Stage 3: Timeout] Searching for '%s' on Canvas..." title)
           (let ((found (org-canvas--search-item "group_categories" title :match-field 'name)))
             (if found
                 (progn
                   (elog-info org-canvas--logger "[Stage 3: Recovery] Found '%s' with ID %s" title (alist-get 'id found))
                   found)
               (elog-error org-canvas--logger "[Stage 3: Recovery] Could not find '%s' after timeout" title)
               (signal (car err) (cdr err)))))
          ;; 404 on PUT: retry as POST
          ((and canvas-id (string-match-p "404" msg))
           (elog-warning org-canvas--logger "[Stage 3: 404] Stale CANVAS_ID %s, retrying as POST..." canvas-id)
           (let* ((post-url (org-canvas-api-course-endpoint "group_categories"))
                  (response (org-canvas-api-request 'POST post-url :data payload)))
             (elog-info org-canvas--logger "[Stage 3: Recovery] POST succeeded for '%s'" title)
             response))
          (t
           (elog-error org-canvas--logger "[Stage 3: FAILED] '%s': %s" title msg)
           (signal (car err) (cdr err)))))))))

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
