;;; org-canvas-calendar.el --- Pipeline-based Calendar Event Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; ALL_DAY          - All-day event ("true"/"false", default false).
;;                    Single-day events only: Canvas keeps the times but
;;                    drops the flag on a multi-day span (issue #93)
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
(require 'cl-lib)

(declare-function org-canvas--validate-all-day-span "org-canvas-validate")

;;;; Configuration

(defcustom org-canvas-calendar-events-file (org-canvas--path "calendar.org")
  "Path to the calendar.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-calendar-events-file "calendar.org")

;; Calendar events are not course-scoped: they live at the global
;; /api/v1/calendar_events and are filtered by a context code.  These
;; three functions are the one spelling of that, shared by the sync, the
;; search, delete-all and the feature registry (issue #87).

(defun org-canvas--calendar-event-list-url ()
  "Return the global calendar-events endpoint."
  (format "%s/api/v1/calendar_events" org-canvas-base-url))

(defun org-canvas--calendar-event-item-url (id)
  "Return the URL of calendar event ID."
  (format "%s/api/v1/calendar_events/%s" org-canvas-base-url id))

(defun org-canvas--calendar-event-list-params ()
  "Return the query parameters that list this course's calendar events.
A function rather than a constant because the context code embeds
`org-canvas-course-id', which is not known when this file loads.

`all_events' is essential.  Without it Canvas returns only the events
dated today — `start_date' defaults to today and `end_date' to
`start_date' — so prune, delete-all and the title search saw zero
events on a course that held six (issue #87)."
  (list (cons "context_codes[]" (format "course_%s" org-canvas-course-id))
        (cons "type" "event")
        (cons "all_events" "true")))

(defun org-canvas--calendar-all-day-comparable-p (pom _item)
  "Return non-nil when ALL_DAY at POM is a comparable opinion.
Canvas does not store `all_day' on an event spanning days: it keeps
the times, sets the flag false and fills `all_day_date' instead, so a
span's ALL_DAY can never round-trip and the drift report would flag
the entry on every run, forever (issue #93).  Named as the ALL_DAY
spec's `:compare-p'; `org-canvas--validate-all-day-span' warns about
the same state at validation time."
  (not (org-canvas--org-timestamps-span-days-p
        (org-entry-get pom "START_AT")
        (org-entry-get pom "END_AT"))))

(org-canvas-register-feature
 :name "Calendar Events" :endpoint "calendar_events"
 :file-var 'org-canvas-calendar-events-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title
 :list-url-fn #'org-canvas--calendar-event-list-url
 :item-url-fn #'org-canvas--calendar-event-item-url
 :list-params #'org-canvas--calendar-event-list-params
 :delete-data '((cancel_reason . "Deleted by org-canvas")))
(org-canvas-register-properties "calendar-events"
  :label "Calendar Events"
  :file-var 'org-canvas-calendar-events-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "START_AT" :data-key :start_at :type timestamp
     :api-key "start_at" :required t
     :doc "Event start time (required)")
    (:org-prop "END_AT" :data-key :end_at :type timestamp
     :api-key "end_at"
     :doc "Event end time")
    (:org-prop "ALL_DAY" :data-key :all_day :type boolean
     :api-key "all_day"
     :compare-p org-canvas--calendar-all-day-comparable-p
     :doc "All-day event (default: false; single-day events only — Canvas drops the flag on a multi-day span)")
    (:org-prop "LOCATION_NAME" :data-key :location_name :type string
     :api-key "location_name"
     :doc "Location name")
    (:org-prop "LOCATION_ADDRESS" :data-key :location_address :type string
     :api-key "location_address"
     :doc "Location address"))
  :structural-fn #'org-canvas--validate-all-day-span)

;;;; 1. Stage: Extraction

(org-canvas-define-parse calendar-event
  :body :description
  :entity-name "Calendar event"
  :after-transform
  (lambda (data)
    (unless (plist-get data :start_at)
      (org-canvas--signal 'org-canvas-validation-error
        "Calendar event '%s' requires START_AT property"
        (plist-get data :title)))
    data)
  :properties
  (("START_AT"         :start_at         :type timestamp)
   ("END_AT"           :end_at           :type timestamp)
   ("ALL_DAY"          :all_day          :type boolean)
   ("LOCATION_NAME"    :location_name    :type string)
   ("LOCATION_ADDRESS" :location_address :type string)))

;;;; 2. Stage: Transformation

(defun org-canvas--calendar-event-extra-required (_data inner)
  "Add context_code to calendar event INNER hash."
  (puthash "context_code" (format "course_%s" org-canvas-course-id) inner))

(org-canvas-define-payload calendar-event
  :registry-key "calendar-events"
  :format hash-table
  :wrapper-key "calendar_event"
  :title-key :title
  :title-api-key "title"
  :body-key :description
  :body-api-key "description"
  :extra-required-fn #'org-canvas--calendar-event-extra-required)

;;;; 3. Stage: Execution

(defun org-canvas--calendar-event-push-to-api (data payload)
  "Send PAYLOAD derived from DATA to Canvas API.
Calendar events use global endpoints (not course-scoped)."
  (org-canvas--push-to-api data payload
    :endpoint "calendar_events"
    :find-fn #'org-canvas--calendar-event-search
    :post-url-fn #'org-canvas--calendar-event-list-url
    :put-url-fn #'org-canvas--calendar-event-item-url))

(defun org-canvas--calendar-event-search (title)
  "Search Canvas for a calendar event matching TITLE.
Returns the matching item alist or nil.  Lists all of the course's
events, not only today's (issue #87)."
  (let ((items (org-canvas-api-request-all-pages
                'GET (org-canvas--calendar-event-list-url)
                (org-canvas--calendar-event-list-params))))
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
  :list-url-fn #'org-canvas--calendar-event-list-url
  :list-params (org-canvas--calendar-event-list-params)
  :delete-url-fn #'org-canvas--calendar-event-item-url
  :delete-data '((cancel_reason . "Deleted by org-canvas")))

(org-canvas-define-delete-at-point calendar-event
  :delete-url-fn #'org-canvas--calendar-event-item-url
  :delete-data ((cancel_reason . "Deleted by org-canvas")))

;;;; Pull

(org-canvas-define-pull-item calendar-event
  :registry-key "calendar-events"
  :body-field description)

;;;###autoload
(defun org-canvas-pull-calendar-events ()
  "Pull calendar events from Canvas into calendar.org.
Fetches events via the global calendar API filtered by course context
code — all of them, not only today's (issue #87)."
  (interactive)
  (org-canvas--start-operation "PULLING CALENDAR EVENTS FROM CANVAS")
  (let* ((items (org-canvas-api-request-all-pages
                 'GET (org-canvas--calendar-event-list-url)
                 (org-canvas--calendar-event-list-params)))
         (file (expand-file-name org-canvas-calendar-events-file))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-overwrite file "calendar events")
    (org-canvas--pull-confirm-unsaved file "calendar events")
    (if (zerop (length items))
        (org-canvas--pull-emit-empty-file
         file (org-canvas--pull-label-for "calendar-events"))
      (unless (file-exists-p file)
        (with-temp-file file (insert "")))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (dolist (item items)
          (let ((id (alist-get 'id item))
                (title (or (alist-get 'title item) "(untitled)")))
            (org-canvas--log-info org-canvas--logger "[Pull] Importing calendar event: %s" title)
            (let ((pos (org-canvas--pull-upsert-heading file id title "CANVAS_ID")))
              (when pos
                (goto-char pos)
                (org-canvas-org-save-sync-state pos id "CANVAS_ID")
                (org-canvas--calendar-event-pull-item item pos)))))
        (org-canvas--pull-write-file-header)
        (org-canvas--save-buffer)))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> CALENDAR EVENTS PULL COMPLETE (%d events)" (length items))
    (org-canvas--log-info org-canvas--logger "========================================")
    (message "Calendar events pull complete: %d events" (length items))))

(provide 'org-canvas-calendar)
;;; org-canvas-calendar.el ends here
