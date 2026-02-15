;;; org-canvas-calendar.el --- Pipeline-based Calendar Event Sync -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the sync pipeline for Canvas Calendar Events.
;;
;; FILE STRUCTURE
;; ==============
;; In calendar.org:
;;   - Level 1 headings = Calendar Events
;;   - Heading body = Event description (exported to HTML)
;;
;; PROPERTIES
;; ==========
;; START_AT         - Event start time (Org timestamp, required)
;; END_AT           - Event end time (Org timestamp, optional)
;; ALL_DAY          - All-day event ("true"/"false", default false)
;; LOCATION_NAME    - Location name (optional)
;; LOCATION_ADDRESS - Location address (optional)
;;
;; API NOTES
;; =========
;; Calendar events use a global endpoint, NOT course-scoped:
;;   POST: /api/v1/calendar_events
;;   PUT:  /api/v1/calendar_events/:id
;;   DELETE: /api/v1/calendar_events/:id
;;   GET (list): /api/v1/calendar_events?context_codes[]=course_:id&type=event
;;
;; Events are tied to a course via the context_code parameter.

;;; Code:

(require 'org-canvas-core)
(require 'ox-html)
(require 'elog)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-calendar-events-file (org-canvas--path "calendar.org")
  "Path to the calendar.org file."
  :type 'file
  :group 'org-canvas)

;;;; 1. Stage: Extraction

(defun org-canvas--calendar-event-parse-entry ()
  "Extract data from the Org heading at point."
  (org-back-to-heading t)
  (elog-debug org-canvas--logger "[Stage 1: Parse] Starting extraction at point %d" (point))

  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
         (start-at (org-canvas-org-parse-timestamp
                    (org-canvas-org-get-property pom "START_AT")))
         (end-at (org-canvas-org-parse-timestamp
                  (org-canvas-org-get-property pom "END_AT")))
         (all-day (org-canvas-org-get-boolean-property pom "ALL_DAY"))
         (location-name (org-canvas-org-get-property pom "LOCATION_NAME"))
         (location-address (org-canvas-org-get-property pom "LOCATION_ADDRESS")))

    (when (or (null title) (string-empty-p title))
      (error "Calendar event title cannot be empty at point %d" pom))

    (unless start-at
      (error "Calendar event '%s' requires START_AT property" title))

    (elog-info org-canvas--logger "[Stage 1: Parse] Processing Calendar Event: '%s' (ID: %s)" title (or canvas-id "NEW"))
    (elog-debug org-canvas--logger "[Stage 1: Parse] Properties: start=%s, end=%s, all-day=%s, location=%s"
               start-at (or end-at "nil") all-day (or location-name "nil"))

    ;; Extract description (resolves cross-file links to Canvas URLs)
    (elog-debug org-canvas--logger "[Stage 1: Export] Exporting subtree to HTML...")
    (let ((description (org-canvas--export-subtree-body-to-html)))
      (elog-info org-canvas--logger "[Stage 1: Parse] Description size: %d chars" (length description))

      (list :title title
            :canvas-id canvas-id
            :start_at start-at
            :end_at end-at
            :all_day all-day
            :location_name location-name
            :location_address location-address
            :description description
            :pom pom))))

;;;; 2. Stage: Transformation

(defun org-canvas--calendar-event-build-payload (data)
  "Convert the extracted DATA plist into a Canvas API-compatible hash-table.
The payload is wrapped in a `calendar_event' key as required by the API."
  (let ((title (plist-get data :title)))
    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" title)

    (let ((event (make-hash-table :test 'equal))
          (payload (make-hash-table :test 'equal)))
      ;; Required fields
      (puthash "context_code" (format "course_%s" org-canvas-course-id) event)
      (puthash "title" title event)
      (puthash "start_at" (plist-get data :start_at) event)

      ;; Optional fields
      (when (plist-get data :end_at)
        (puthash "end_at" (plist-get data :end_at) event))

      (when (plist-get data :all_day)
        (puthash "all_day" t event))

      (when (plist-get data :description)
        (puthash "description" (plist-get data :description) event))

      (when (plist-get data :location_name)
        (puthash "location_name" (plist-get data :location_name) event))

      (when (plist-get data :location_address)
        (puthash "location_address" (plist-get data :location_address) event))

      (puthash "calendar_event" event payload)
      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      payload)))

;;;; 3. Stage: Execution

(cl-defun org-canvas--calendar-event-push-to-api (data payload)
  "Send PAYLOAD derived from DATA to Canvas API.
Calendar events use global endpoints (not course-scoped)."
  (let* ((canvas-id (plist-get data :canvas-id))
         (title (plist-get data :title))
         (method (if canvas-id 'PUT 'POST))
         (endpoint (if canvas-id
                       (format "%s/api/v1/calendar_events/%s" org-canvas-base-url canvas-id)
                     (format "%s/api/v1/calendar_events" org-canvas-base-url))))

    ;; Dry-run check
    (when org-canvas--dry-run
      (elog-info org-canvas--logger "[DRY-RUN] Would %s calendar event '%s' to %s"
                 method title endpoint)
      (cl-return-from org-canvas--calendar-event-push-to-api
        `((id . ,(or canvas-id "dry-run")))))

    (elog-info org-canvas--logger "[Stage 3: Execute] %s '%s' to %s" method title endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request method endpoint :data payload)))
          (elog-info org-canvas--logger "[Stage 3: Execute] Success for '%s'" title)
          response)
      (error
       (let ((msg (error-message-string err)))
         (cond
          ;; Timeout recovery: search for event by title
          ((org-canvas--timeout-error-p err)
           (elog-warning org-canvas--logger "[Stage 3: Timeout] Searching for '%s' on Canvas..." title)
           (let ((found (org-canvas--calendar-event-search title)))
             (if found
                 (progn
                   (elog-info org-canvas--logger "[Stage 3: Recovery] Found '%s' with ID %s" title (alist-get 'id found))
                   found)
               (elog-error org-canvas--logger "[Stage 3: Recovery] Could not find '%s' after timeout" title)
               (signal (car err) (cdr err)))))
          ;; 404 on PUT: retry as POST
          ((and canvas-id (string-match-p "404" msg))
           (elog-warning org-canvas--logger "[Stage 3: 404] Stale CANVAS_ID %s, retrying as POST..." canvas-id)
           (let* ((post-url (format "%s/api/v1/calendar_events" org-canvas-base-url))
                  (response (org-canvas-api-request 'POST post-url :data payload)))
             (elog-info org-canvas--logger "[Stage 3: Recovery] POST succeeded for '%s'" title)
             response))
          (t
           (elog-error org-canvas--logger "[Stage 3: FAILED] '%s': %s" title msg)
           (signal (car err) (cdr err)))))))))

(defun org-canvas--calendar-event-search (title)
  "Search Canvas for a calendar event matching TITLE.
Returns the matching item alist or nil."
  (let* ((context-code (format "course_%s" org-canvas-course-id))
         (url (format "%s/api/v1/calendar_events" org-canvas-base-url))
         (items (org-canvas-api-request-all-pages
                 'GET url
                 `(("context_codes[]" . ,context-code)
                   ("type" . "event")))))
    (cl-find-if (lambda (item)
                  (equal (alist-get 'title item) title))
                items)))

;;;; 4. Stage: Finalization

(defun org-canvas--calendar-event-finalize (data response)
  "Update local Org file using DATA and metadata from API RESPONSE."
  (org-canvas--finalize-item data response))

;;;; Main Sync Function

;; Generate org-canvas-sync-calendar-events using the pipeline macro
(org-canvas-define-sync calendar-events
  :file org-canvas-calendar-events-file
  :parse #'org-canvas--calendar-event-parse-entry
  :build #'org-canvas--calendar-event-build-payload
  :push #'org-canvas--calendar-event-push-to-api
  :finalize #'org-canvas--calendar-event-finalize
  :pull-item-fn #'org-canvas--calendar-event-pull-item)

(org-canvas-define-push-at-point calendar-event
  :parse #'org-canvas--calendar-event-parse-entry
  :build #'org-canvas--calendar-event-build-payload
  :push #'org-canvas--calendar-event-push-to-api
  :finalize #'org-canvas--calendar-event-finalize)

;;;; Delete

(defun org-canvas-delete-all-calendar-events ()
  "Delete all calendar events for this course from Canvas.
Uses global endpoint with context_codes filter."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let* ((context-code (format "course_%s" org-canvas-course-id))
         (url (format "%s/api/v1/calendar_events" org-canvas-base-url))
         (items (org-canvas-api-request-all-pages
                 'GET url
                 `(("context_codes[]" . ,context-code)
                   ("type" . "event")))))
    (elog-info org-canvas--logger "[Delete] Found %d calendar events" (length items))
    (dolist (item items)
      (let* ((id (alist-get 'id item))
             (title (alist-get 'title item))
             (del-url (format "%s/api/v1/calendar_events/%s" org-canvas-base-url id)))
        (condition-case err
            (progn
              (org-canvas-api-request 'DELETE del-url
                                      :data '((cancel_reason . "Deleted by org-canvas")))
              (elog-info org-canvas--logger "[Delete] Deleted calendar event '%s' (ID: %s)" title id))
          (error
           (elog-warning org-canvas--logger "[Delete] Failed to delete '%s': %s"
                         title (error-message-string err))))))
    ;; Clear CANVAS_ID and LAST_SYNCED from local file
    (let ((file (expand-file-name org-canvas-calendar-events-file)))
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
    (elog-info org-canvas--logger "[Delete] Calendar events deletion complete")))

;;;###autoload
(defun org-canvas-delete-calendar-event-at-point ()
  "Delete the Canvas calendar event at the current Org heading.
Uses global endpoint DELETE /calendar_events/:id."
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
      (elog-info org-canvas--logger "Deleting calendar event '%s' (ID: %s)..." title canvas-id)
      (condition-case err
          (let ((url (format "%s/api/v1/calendar_events/%s" org-canvas-base-url canvas-id)))
            (org-canvas-api-request 'DELETE url
                                    :data '((cancel_reason . "Deleted by org-canvas")))
            (org-entry-delete pom "CANVAS_ID")
            (org-entry-delete pom "LAST_SYNCED")
            (org-entry-delete pom "PAYLOAD_HASH")
            (save-buffer)
            (elog-info org-canvas--logger "Deleted calendar event '%s'" title)
            (message "Deleted '%s' from Canvas" title))
        (error
         (elog-error org-canvas--logger "Failed to delete '%s': %s"
                     title (error-message-string err))
         (message "Delete failed: %s" (error-message-string err)))))))

;;;; Pull

(defun org-canvas--calendar-event-pull-item (item pos)
  "Set per-item properties for a pulled calendar event.
ITEM is the API response alist, POS is the heading position."
  (let ((start-at (alist-get 'start_at item))
        (end-at (alist-get 'end_at item))
        (all-day (alist-get 'all_day item))
        (location-name (org-canvas--alist-get-non-null 'location_name item))
        (location-address (org-canvas--alist-get-non-null 'location_address item))
        (description (org-canvas--alist-get-non-null 'description item)))
    (org-canvas--pull-set-timestamp-property pos "START_AT" start-at)
    (org-canvas--pull-set-timestamp-property pos "END_AT" end-at)
    (org-canvas--pull-set-boolean-property pos "ALL_DAY" all-day)
    (when location-name
      (org-canvas-org-set-property pos "LOCATION_NAME" location-name))
    (when location-address
      (org-canvas-org-set-property pos "LOCATION_ADDRESS" location-address))
    (when description
      (org-canvas--pull-insert-body description))))

;;;###autoload
(defun org-canvas-pull-calendar-events ()
  "Pull calendar events from Canvas into calendar.org.
Fetches events via the global calendar API filtered by course context code."
  (interactive)
  (org-canvas--start-operation "PULLING CALENDAR EVENTS FROM CANVAS")
  (let* ((context-code (format "course_%s" org-canvas-course-id))
         (url (format "%s/api/v1/calendar_events" org-canvas-base-url))
         (items (org-canvas-api-request-all-pages
                 'GET url
                 `(("context_codes[]" . ,context-code)
                   ("type" . "event"))))
         (file (expand-file-name org-canvas-calendar-events-file)))
    (org-canvas--pull-confirm-overwrite file "calendar events")
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "#+TITLE: Calendar Events\n")))
    (with-current-buffer (find-file-noselect file)
      (dolist (item items)
        (let ((id (alist-get 'id item))
              (title (or (alist-get 'title item) "(untitled)")))
          (elog-info org-canvas--logger "[Pull] Importing calendar event: %s" title)
          (let ((pos (org-canvas--pull-upsert-heading file id title "CANVAS_ID")))
            (when pos
              (goto-char pos)
              (org-canvas-org-save-sync-state pos id "CANVAS_ID")
              (org-canvas--calendar-event-pull-item item pos)))))
      (save-buffer))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> CALENDAR EVENTS PULL COMPLETE (%d events)" (length items))
    (elog-info org-canvas--logger "========================================")
    (message "Calendar events pull complete: %d events" (length items))))

(provide 'org-canvas-calendar)
;;; org-canvas-calendar.el ends here
