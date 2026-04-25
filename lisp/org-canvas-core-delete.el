;;; org-canvas-core-delete.el --- Delete infrastructure and macros -*- lexical-binding: t; -*-

;;; Commentary:

;; Two types of delete operations:
;;
;;   - delete-all: Fetches all items from Canvas and deletes them,
;;     then cleans up local CANVAS_ID properties from the Org file.
;;
;;   - delete-at-point: Deletes the single item at the current cursor
;;     position (requires confirmation).
;;
;; Use the macros `org-canvas-define-delete-all' and
;; `org-canvas-define-delete-at-point' to generate these functions.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)

(defun org-canvas--delete-log-skipped (items skip-fn title-field)
  "Log skipped items from ITEMS using SKIP-FN.
TITLE-FIELD is the alist key for item display names."
  (dolist (item items)
    (when (funcall skip-fn item)
      (org-canvas--log-info org-canvas--logger "Skipping: '%s'"
        (alist-get title-field item)))))

(defun org-canvas--delete-items-queued (items endpoint-fn id-field
                                       title-field &optional skip-fn
                                       delete-data)
  "Delete ITEMS from Canvas using synchronous requests.
ENDPOINT-FN is a function taking an item ID and returning the DELETE URL.
ID-FIELD and TITLE-FIELD are alist keys for extracting ID/title from each item.
SKIP-FN, if non-nil, is called with each item; non-nil return skips that item.
DELETE-DATA, if non-nil, is an alist of extra data sent with DELETE.
Returns (DELETED-COUNT . DELETED-IDS)."
  (let* ((to-delete (if skip-fn (cl-remove-if skip-fn items) items))
         (skipped (- (length items) (length to-delete))))
    (when (and (> skipped 0) skip-fn)
      (org-canvas--delete-log-skipped items skip-fn title-field))
    ;; Short-circuit if nothing to delete
    (if (null to-delete)
        (cons 0 nil)
      (let ((deleted-count 0)
            (failed-count 0)
            (deleted-ids nil)
            (del-total (length to-delete))
            (del-idx 0))
        (dolist (item to-delete)
          (cl-incf del-idx)
          (let ((item-id (alist-get id-field item))
                (item-title (alist-get title-field item)))
            (org-canvas--log-info org-canvas--logger "Deleting: '%s' (ID: %s)" item-title item-id)
            (message "Deleting [%d/%d] '%s'..." del-idx del-total item-title)
            (condition-case err
                (progn
                  (if delete-data
                      (org-canvas-api-request 'DELETE (funcall endpoint-fn item-id)
                                              :data delete-data)
                    (org-canvas-api-request 'DELETE (funcall endpoint-fn item-id)))
                  (push (org-canvas--normalize-id item-id) deleted-ids)
                  (setq deleted-count (1+ deleted-count))
                  (org-canvas--log-info org-canvas--logger "  -> Deleted '%s' successfully" item-title))
              (error
               (setq failed-count (1+ failed-count))
               (org-canvas--log-error org-canvas--logger "  -> Delete failed for '%s': %s"
                 item-title (error-message-string err))
               (message "WARNING: Delete failed for '%s': %s"
                        item-title (error-message-string err))))))
        (when (> failed-count 0)
          (org-canvas--log-warning org-canvas--logger
            "Delete summary: %d succeeded, %d failed out of %d"
            deleted-count failed-count (length to-delete))
          (message "WARNING: %d of %d deletions failed. See *canvas-log*."
                   failed-count (length to-delete)))
        (cons deleted-count deleted-ids)))))

(cl-defun org-canvas--delete-all-items (feature-name
                                        &key
                                        endpoint
                                        file
                                        id-field
                                        title-field
                                        id-property
                                        list-params
                                        skip-fn
                                        list-url-fn
                                        delete-url-fn
                                        delete-data)
  "Generic implementation for deleting all items of a feature type.

FEATURE-NAME is a string like \"announcements\" or \"pages\".

Keyword arguments:
  ENDPOINT - API endpoint suffix (e.g., \"assignments\").
  FILE - Path to the org file for cleaning local properties.
  ID-FIELD - Alist key for item ID in API response (default: \\='id).
  TITLE-FIELD - Alist key for item title in API response (default: \\='title).
  ID-PROPERTY - Org property name for ID (default: \"CANVAS_ID\").
  LIST-PARAMS - Extra params for GET request.
  SKIP-FN - Optional function taking an item, returns non-nil to skip.
  LIST-URL-FN - Optional function returning the list URL (overrides ENDPOINT).
  DELETE-URL-FN - Optional function taking item-id, returning the delete URL.
  DELETE-DATA - Optional alist of extra data to send with DELETE request.

Returns the count of successfully deleted items."
  (let* ((id-field (or id-field 'id))
         (title-field (or title-field 'title))
         (id-property (or id-property "CANVAS_ID"))
         (full-endpoint (if list-url-fn
                            (funcall list-url-fn)
                          (org-canvas-api-course-endpoint endpoint)))
         (remote-items (org-canvas-api-request-all-pages 'GET full-endpoint list-params)))

    (org-canvas--log-info org-canvas--logger "Found %d %s on Canvas" (length remote-items) feature-name)

    (let* ((del-fn (or delete-url-fn
                       (lambda (item-id)
                         (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) item-id))))
           (result (org-canvas--delete-items-queued
                    remote-items del-fn
                    id-field title-field skip-fn delete-data))
           (deleted-count (car result)))

      ;; Cleanup local properties
      (org-canvas--clean-local-sync-properties file id-property)

      (org-canvas--log-info org-canvas--logger "========================================")
      (org-canvas--log-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d removed" deleted-count)
      (org-canvas--log-info org-canvas--logger "========================================")

      deleted-count)))

(cl-defun org-canvas--delete-item-at-point (feature-name
                                            &key
                                            endpoint
                                            id-property
                                            delete-url-fn
                                            delete-data
                                            post-delete-fn)
  "Generic implementation for deleting the item at the current Org heading.

FEATURE-NAME is a string like \"assignment\" or \"page\".

Keyword arguments:
  ENDPOINT - API endpoint pattern with %s for ID.
  ID-PROPERTY - Org property name for ID (default: \"CANVAS_ID\").
  DELETE-URL-FN - Function (id) returning full URL, overrides ENDPOINT.
  DELETE-DATA - Extra alist data to include in DELETE request.
  POST-DELETE-FN - Function (pom) called after API delete for cleanup.

Return non-nil if deletion succeeded."
  (org-back-to-heading t)
  (let* ((id-property (or id-property "CANVAS_ID"))
         (pom (point))
         (canvas-id (org-canvas-org-get-property pom id-property))
         (title (org-get-heading t t t t)))

    (unless canvas-id
      (user-error "No %s property found for this heading" id-property))

    (when (y-or-n-p (format "Delete '%s' from Canvas? " title))
      (org-canvas-clear-log)
      (display-buffer (get-buffer-create org-canvas--log-buffer-name))
      (org-canvas--log-info org-canvas--logger "Deleting %s '%s' (ID: %s)..."
                 feature-name title canvas-id)

      (condition-case err
          (let ((url (if delete-url-fn
                        (funcall delete-url-fn canvas-id)
                      (org-canvas-api-course-endpoint
                       endpoint canvas-id))))
            (if delete-data
                (org-canvas-api-request 'DELETE url
                                        :data delete-data)
              (org-canvas-api-request 'DELETE url))
            (org-canvas--log-info org-canvas--logger
                       "Successfully deleted from Canvas")
            (when post-delete-fn
              (funcall post-delete-fn pom))
            (org-canvas-clear-sync-properties pom)
            (org-canvas--log-info org-canvas--logger "Cleaned local properties")
            (message "%s '%s' deleted." (capitalize feature-name) title)
            t)
        (error
         (org-canvas--log-error org-canvas--logger "Failed to delete: %s"
                     (error-message-string err))
         (message "Failed to delete %s. Check logs." feature-name)
         nil)))))

(cl-defun org-canvas--delete-all-runtime (feature-name &key endpoint file
                                                        id-field title-field
                                                        id-property list-params
                                                        skip-fn list-url-fn
                                                        delete-url-fn delete-data)
  "Runtime body for generated delete-all functions.
FEATURE-NAME is the module name string.  ENDPOINT, FILE, ID-FIELD,
TITLE-FIELD, ID-PROPERTY, LIST-PARAMS, SKIP-FN, LIST-URL-FN,
DELETE-URL-FN, and DELETE-DATA are passed through to
`org-canvas--delete-all-items'."
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((feature-upper (upcase feature-name)))
    (org-canvas--log-warning org-canvas--logger "========================================")
    (org-canvas--log-warning org-canvas--logger ">>> STARTING MASS DELETION OF %s" feature-upper)
    (org-canvas--log-warning org-canvas--logger "========================================"))
  (let ((deleted-count (org-canvas--delete-all-items feature-name
                          :endpoint endpoint :file file
                          :id-field id-field :title-field title-field
                          :id-property id-property :list-params list-params
                          :skip-fn skip-fn :list-url-fn list-url-fn
                          :delete-url-fn delete-url-fn :delete-data delete-data)))
    (message "%s deletion complete. %d removed." (capitalize feature-name) deleted-count)))

(defmacro org-canvas-define-delete-all (feature &rest args)
  "Define a delete-all function for FEATURE.
FEATURE is a symbol like \\='pages.  ARGS is a plist with keys:
  :endpoint, :file (required), :id-field, :title-field,
  :id-property, :list-params, :skip-fn, :list-url-fn,
  :delete-url-fn, :delete-data."
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas-delete-all-%s" feature-name)))
         (endpoint (plist-get args :endpoint))
         (file-expr (plist-get args :file))
         (id-field (or (plist-get args :id-field) ''id))
         (title-field (or (plist-get args :title-field) ''title))
         (id-property (or (plist-get args :id-property) "CANVAS_ID"))
         (list-params (plist-get args :list-params))
         (skip-fn (plist-get args :skip-fn))
         (list-url-fn (plist-get args :list-url-fn))
         (delete-url-fn (plist-get args :delete-url-fn))
         (delete-data (plist-get args :delete-data)))
    (unless endpoint (error "org-canvas-define-delete-all: :endpoint is required"))
    (unless file-expr (error "org-canvas-define-delete-all: :file is required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Delete ALL %s in the configured course." feature-name)
         (interactive)
         (unless org-canvas--inhibit-log-clear
           (unless (y-or-n-p ,(format "Delete ALL %s in this course? " feature-name))
             (user-error "Aborted")))
         (org-canvas--delete-all-runtime ,feature-name
           :endpoint ,endpoint :file ,file-expr
           :id-field ,id-field :title-field ,title-field
           :id-property ,id-property :list-params ,list-params
           :skip-fn ,skip-fn :list-url-fn ,list-url-fn
           :delete-url-fn ,delete-url-fn :delete-data ,delete-data)))))

(defmacro org-canvas-define-delete-at-point (feature &rest args)
  "Define a delete-at-point function for FEATURE.

FEATURE is a symbol like \\='page or \\='assignment.

ARGS is a plist with the following keys:
  :endpoint      - API endpoint pattern with %s (required unless
                   :delete-url-fn provided)
  :id-property   - Org property name (default: \"CANVAS_ID\")
  :delete-url-fn - Lambda (id) returning full URL (overrides :endpoint)
  :delete-data   - Alist data to include in DELETE request
  :post-delete-fn - Function (pom) for post-delete cleanup

Example:
  (org-canvas-define-delete-at-point assignment
    :endpoint \"assignments/%s\")"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas-delete-%s-at-point"
                                  feature-name)))
         (endpoint (plist-get args :endpoint))
         (id-property (or (plist-get args :id-property) "CANVAS_ID"))
         (delete-url-fn (plist-get args :delete-url-fn))
         (delete-data (plist-get args :delete-data))
         (post-delete-fn (plist-get args :post-delete-fn)))
    (unless (or endpoint delete-url-fn)
      (error "org-canvas-define-delete-at-point: :endpoint or :delete-url-fn required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Delete the Canvas %s associated with the current Org heading." feature-name)
         (interactive)
         (org-canvas--delete-item-at-point ,feature-name
           :endpoint ,endpoint
           :id-property ,id-property
           :delete-url-fn ,delete-url-fn
           :delete-data ',delete-data
           :post-delete-fn ,post-delete-fn)))))

(provide 'org-canvas-core-delete)
;;; org-canvas-core-delete.el ends here
