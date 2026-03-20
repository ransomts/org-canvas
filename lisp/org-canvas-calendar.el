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
(org-canvas-register-file-var 'org-canvas-calendar-events-file "calendar.org")

;;;; 1. Stage: Extraction

(org-canvas-define-parse calendar-event
  :body :description
  :entity-name "Calendar event"
  :after-transform
  (lambda (data)
    (unless (plist-get data :start_at)
      (error "Calendar event '%s' requires START_AT property"
             (plist-get data :title)))
    data)
  :properties
  (("START_AT"         :start_at         :type timestamp)
   ("END_AT"           :end_at           :type timestamp)
   ("ALL_DAY"          :all_day          :type boolean)
   ("LOCATION_NAME"    :location_name    :type string)
   ("LOCATION_ADDRESS" :location_address :type string)))

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
      (org-canvas--puthash-when event data :end_at "end_at")
   (unless (plist-get data :all_day)
        (puthash "all_day" t event))
      (org-canvas--puthash-when event data :description "description")
      (org-canvas--puthash-when event data :location_name "location_name")
      (org-canvas--puthash-when event data :location_address "location_address")

      (puthash "calendar_event" event payload)
      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
      payload)))

;;;; 3. Stage: Execution

(defun org-canvas--calendar-event-push-to-api (data payload)
  "Send PAYLOAD derived from DATA to Canvas API.
Calendar events use global endpoints (not course-scoped)."
  (org-canvas--push-to-api data payload
    :endpoint "calendar_events"
    :find-fn #'org-canvas--calendar-event-search
    :post-url-fn (lambda () (format "%s/api/v1/calendar_events" org-canvas-base-url))
    :put-url-fn (lambda (id) (format "%s/api/v1/calendar_events/%s" org-canvas-base-url id))))

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

;;;; Main Sync Function

;; Generate org-canvas-sync-calendar-events using the pipeline macro
(org-canvas-define-sync calendar-events
  :file org-canvas-calendar-events-file
  :parse #'org-canvas--calendar-event-parse-entry
  :build #'org-canvas--calendar-event-build-payload
  :push #'org-canvas--calendar-event-push-to-api
  :endpoint "calendar_events"
  :pull-item-fn #'org-canvas--calendar-event-pull-item)

;;;; Delete

(org-canvas-define-delete-all calendar-events
  :endpoint "calendar_events"
  :file org-canvas-calendar-events-file
  :list-url-fn (lambda ()
                 (format "%s/api/v1/calendar_events" org-canvas-base-url))
  :list-params (list (cons "context_codes[]" (format "course_%s" org-canvas-course-id))
                     (cons "type" "event"))
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/calendar_events/%s" org-canvas-base-url id))
  :delete-data '((cancel_reason . "Deleted by org-canvas")))

(org-canvas-define-delete-at-point calendar-event
  :delete-url-fn (lambda (id)
                   (format "%s/api/v1/calendar_events/%s"
                           org-canvas-base-url id))
  :delete-data ((cancel_reason . "Deleted by org-canvas")))

;;;; Pull

(org-canvas-define-pull-item calendar-event
  :body-field description
  :properties
  ((start_at         "START_AT"         :type timestamp)
   (end_at           "END_AT"           :type timestamp)
   (all_day          "ALL_DAY"          :type boolean)
   (location_name    "LOCATION_NAME"    :type non-null)
   (location_address "LOCATION_ADDRESS" :type non-null)))

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
   (when (file-exists-p file)
      (with-temp-file file (insert "")))
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
