;;; org-canvas-calendar-test.el --- Buttercup tests for calendar events  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-calendar)

;;;; Transform (pure, no buffer)

(describe "org-canvas--calendar-event-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--calendar-event-transform-props
                   '(:title-raw "Office Hours [2/3]" :canvas-id nil
                     :start-at-raw "<2026-09-01 Tue 14:00>" :end-at-raw nil
                     :all-day-raw nil :location-name-raw nil :location-address-raw nil))))
      (expect (plist-get result :title) :to-equal "Office Hours")))

  (it "parses START_AT to ISO8601"
    (let ((result (org-canvas--calendar-event-transform-props
                   '(:title-raw "Event" :canvas-id nil
                     :start-at-raw "<2026-09-01 Tue 14:00>" :end-at-raw nil
                     :all-day-raw nil :location-name-raw nil :location-address-raw nil))))
      (expect (plist-get result :start_at) :to-match "2026-09-01T")))

  (it "returns nil for absent END_AT"
    (let ((result (org-canvas--calendar-event-transform-props
                   '(:title-raw "Event" :canvas-id nil
                     :start-at-raw "<2026-09-01 Tue 14:00>" :end-at-raw nil
                     :all-day-raw nil :location-name-raw nil :location-address-raw nil))))
      (expect (plist-get result :end_at) :to-be nil)))

  (it "interprets ALL_DAY boolean"
    (let ((result (org-canvas--calendar-event-transform-props
                   '(:title-raw "Event" :canvas-id nil
                     :start-at-raw "<2026-09-01 Tue 14:00>" :end-at-raw nil
                     :all-day-raw "true" :location-name-raw nil :location-address-raw nil))))
      (expect (plist-get result :all_day) :to-be t)))

  (it "passes through location strings"
    (let ((result (org-canvas--calendar-event-transform-props
                   '(:title-raw "Event" :canvas-id nil
                     :start-at-raw "<2026-09-01 Tue 14:00>" :end-at-raw nil
                     :all-day-raw nil :location-name-raw "Room 101"
                     :location-address-raw "123 Main St"))))
      (expect (plist-get result :location_name) :to-equal "Room 101")
      (expect (plist-get result :location_address) :to-equal "123 Main St"))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--calendar-event-parse-entry"
  (it "extracts calendar event title from heading"
    (with-temp-org-buffer
     "* Office Hours
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:END:

Description.
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :title) :to-equal "Office Hours"))))

  (test-org-canvas-define-common-parse-tests
   #'org-canvas--calendar-event-parse-entry
   :extra-props ":START_AT: <2026-09-01 Tue 14:00>\n")

  (it "parses START_AT timestamp to ISO8601"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :start_at) :to-be-truthy)
       (expect (plist-get data :start_at) :to-match "2026-09-01T"))))

  (it "parses END_AT timestamp"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:END_AT: <2026-09-01 Tue 15:30>
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :end_at) :to-be-truthy)
       (expect (plist-get data :end_at) :to-match "2026-09-01T"))))

  (it "returns nil for absent END_AT"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :end_at) :to-be nil))))

  (it "parses ALL_DAY=true"
    (with-temp-org-buffer
     "* Holiday
:PROPERTIES:
:START_AT: <2026-09-07 Mon>
:ALL_DAY: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :all_day) :to-be t))))

  (it "parses ALL_DAY=false"
    (with-temp-org-buffer
     "* Meeting
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:ALL_DAY: false
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :all_day) :to-be nil))))

  (it "parses LOCATION_NAME"
    (with-temp-org-buffer
     "* Office Hours
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:LOCATION_NAME: Room 210
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :location_name) :to-equal "Room 210"))))

  (it "parses LOCATION_ADDRESS"
    (with-temp-org-buffer
     "* Office Hours
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:LOCATION_ADDRESS: 123 University Ave
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :location_address) :to-equal "123 University Ave"))))

  (it "errors on missing START_AT"
    (with-temp-org-buffer
     "* No Start Time
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--calendar-event-parse-entry) :to-throw 'error)))

  (it "exports body to HTML for description"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:END:

This is the event description.
"
     (org-back-to-heading)
     (let ((data (org-canvas--calendar-event-parse-entry)))
       (expect (plist-get data :description) :to-be-truthy)))))

;;;; Stage 2: Build Payload

(describe "org-canvas--calendar-event-build-payload"
  (it "wraps payload in calendar_event key"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"))
           (payload (org-canvas--calendar-event-build-payload data)))
      (expect (gethash "calendar_event" payload) :to-be-truthy)))

  (it "wraps title inside calendar_event key"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Office Hours" :start_at "2026-09-01T14:00:00Z"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "title" event) :to-equal "Office Hours")))

  (it "includes context_code with course ID"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "context_code" event) :to-equal "course_99999")))

  (it "converts Org timestamp to ISO8601 start_at"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "start_at" event) :to-equal "2026-09-01T14:00:00Z")))

  (it "includes end_at when set"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"
                   :end_at "2026-09-01T15:30:00Z"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "end_at" event) :to-equal "2026-09-01T15:30:00Z")))

  (it "omits end_at when nil"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "end_at" event) :to-be nil)))

  (it "includes all_day when true"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Holiday" :start_at "2026-09-07T00:00:00Z" :all_day t))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "all_day" event) :to-be t)))

  (it "includes location_name when set"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"
                   :location_name "Room 210"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "location_name" event) :to-equal "Room 210")))

  (it "includes location_address when set"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"
                   :location_address "123 University Ave"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "location_address" event) :to-equal "123 University Ave")))

  (it "includes description when set"
    (let* ((org-canvas-course-id "99999")
           (data '(:title "Event" :start_at "2026-09-01T14:00:00Z"
                   :description "<p>Event details</p>"))
           (payload (org-canvas--calendar-event-build-payload data))
           (event (gethash "calendar_event" payload)))
      (expect (gethash "description" event) :to-equal "<p>Event details</p>"))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--calendar-event-push-to-api (mocked)"
  (it "uses POST with global endpoint for new events"
    (with-org-canvas-test-config
      (let ((api-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (push (list method url) api-calls)
                     '((id . 42) (title . "New Event")))))
          (let ((data '(:title "New Event" :canvas-id nil))
                (payload (make-hash-table)))
            (org-canvas--calendar-event-push-to-api data payload)
            (let ((call (car api-calls)))
              (expect (car call) :to-equal 'POST)
              (expect (cadr call) :to-match "api/v1/calendar_events$")))))))

  (it "uses PUT with global endpoint for existing events"
    (with-org-canvas-test-config
      (let ((api-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (push (list method url) api-calls)
                     '((id . 42) (title . "Existing Event")))))
          (let ((data '(:title "Existing Event" :canvas-id "42"))
                (payload (make-hash-table)))
            (org-canvas--calendar-event-push-to-api data payload)
            (let ((call (car api-calls)))
              (expect (car call) :to-equal 'PUT)
              (expect (cadr call) :to-match "api/v1/calendar_events/42")))))))

  (it "retries as POST on 404 for stale canvas-id"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         (signal 'error '("HTTP 404 Not Found"))
                       '((id . 99) (title . "Recovered"))))))
          (let ((data '(:title "Stale" :canvas-id "999"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--calendar-event-push-to-api data payload)))
              (expect (alist-get 'id result) :to-equal 99)))))))

  (it "searches Canvas on timeout"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Request timed out"))))
                ((symbol-function 'org-canvas--calendar-event-search)
                 (lambda (_title)
                   '((id . 77) (title . "Found After Timeout")))))
        (let ((data '(:title "Timeout Test" :canvas-id nil))
              (payload (make-hash-table)))
          (let ((result (org-canvas--calendar-event-push-to-api data payload)))
            (expect (alist-get 'id result) :to-equal 77))))))

  (it "returns dry-run response when dry-run is active"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t))
        (let ((data '(:title "Dry" :canvas-id nil))
              (payload (make-hash-table)))
          (let ((result (org-canvas--calendar-event-push-to-api data payload)))
            (expect (alist-get 'id result) :to-equal "dry-run")))))))

;;;; Stage 4: Finalize

(describe "calendar-event finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Event" :pom (point-marker)))
           (response '((id . 42) (title . "Event"))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Event" :pom (point-marker)))
           (response '((id . 42))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Event" :pom (point-marker)))
           (response '((error . "bad"))))
       (expect (org-canvas--finalize-item data response) :to-throw 'error)))))

;;;; Sync Pipeline Tests

(describe "org-canvas-sync-calendar-events (mocked)"
  (it "errors when file not found"
    (let ((org-canvas-calendar-events-file "/nonexistent/calendar.org"))
      (expect (org-canvas-sync-calendar-events) :to-throw 'error)))

  (it "syncs calendar events from file"
    (let ((temp-dir (make-temp-file "calendar-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "calendar.org" temp-dir))
                 (synced-count 0))
            (with-temp-file org-file
              (insert "* Office Hours
:PROPERTIES:
:START_AT: <2026-09-01 Tue 14:00>
:END_AT: <2026-09-01 Tue 15:30>
:END:

Weekly office hours.
"))
            (let ((org-canvas-calendar-events-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq synced-count (1+ synced-count)))
                             '((id . 42)))))
                  (org-canvas-sync-calendar-events)
                  (expect synced-count :to-equal 1)
                  ;; Check CANVAS_ID was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))))
        (delete-directory temp-dir t)))))

;;;; Delete All Tests

(describe "org-canvas-delete-all-calendar-events (mocked)"
  (it "deletes calendar events using global endpoint"
    (let ((temp-dir (make-temp-file "calendar-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "calendar.org" temp-dir))
                 (deleted-urls nil))
            (with-temp-file org-file
              (insert "* Event
:PROPERTIES:
:CANVAS_ID: 42
:END:
"))
            (let ((org-canvas-calendar-events-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 42) (title . "Event")))))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (when (eq method 'DELETE)
                               (push url deleted-urls))
                             nil)))
                  (org-canvas-delete-all-calendar-events)
                  (expect (length deleted-urls) :to-equal 1)
                  (expect (car deleted-urls) :to-match "api/v1/calendar_events/42")))))
        (delete-directory temp-dir t)))))

;;;; Pull Function Tests

(describe "org-canvas--calendar-event-pull-item"
  (it "sets START_AT property"
    (with-pull-property-test #'org-canvas--calendar-event-pull-item
      '((id . 1) (title . "Event")
        (start_at . "2026-09-01T14:00:00Z"))
      "START_AT" :to-match "<2026-09-01"))

  (it "sets END_AT property"
    (with-pull-property-test #'org-canvas--calendar-event-pull-item
      '((id . 1) (title . "Event")
        (start_at . "2026-09-01T14:00:00Z")
        (end_at . "2026-09-01T15:30:00Z"))
      "END_AT" :to-match "<2026-09-01"))

  (it "sets ALL_DAY property"
    (with-temp-org-buffer
     "* Holiday
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--calendar-event-pull-item
      '((id . 1) (title . "Holiday")
        (start_at . "2026-09-07T00:00:00Z")
        (all_day . t))
      (point))
     (expect (org-entry-get (point) "ALL_DAY") :to-equal "true")))

  (it "sets LOCATION_NAME property"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--calendar-event-pull-item
      '((id . 1) (title . "Event")
        (start_at . "2026-09-01T14:00:00Z")
        (location_name . "Room 210"))
      (point))
     (expect (org-entry-get (point) "LOCATION_NAME") :to-equal "Room 210")))

  (it "sets LOCATION_ADDRESS property"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--calendar-event-pull-item
      '((id . 1) (title . "Event")
        (start_at . "2026-09-01T14:00:00Z")
        (location_address . "123 University Ave"))
      (point))
     (expect (org-entry-get (point) "LOCATION_ADDRESS") :to-equal "123 University Ave")))

  (it "inserts body text from description"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas--html-to-org)
                (lambda (html) (replace-regexp-in-string "<[^>]+>" "" html))))
       (org-canvas--calendar-event-pull-item
        '((id . 1) (title . "Event")
          (start_at . "2026-09-01T14:00:00Z")
          (description . "<p>Event details here</p>"))
        (point))
       (expect (buffer-string) :to-match "Event details here")))))

;;;; Search Function Tests

(describe "org-canvas--calendar-event-search"
  (it "finds event matching title"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 10) (title . "Other Event"))
                     ((id . 20) (title . "Office Hours"))))))
        (let ((result (org-canvas--calendar-event-search "Office Hours")))
          (expect (alist-get 'id result) :to-equal 20)))))

  (it "returns nil when no match"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 10) (title . "Other Event"))))))
        (let ((result (org-canvas--calendar-event-search "Nonexistent")))
          (expect result :to-be nil))))))

;;;; Additional Coverage Tests

(describe "org-canvas--calendar-event-push-to-api error handling"
  (it "re-signals timeout when event not found on Canvas"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Connection timed out"))))
                ((symbol-function 'org-canvas--calendar-event-search)
                 (lambda (_title) nil)))
        (let ((data '(:title "Missing Event" :canvas-id nil :pom 1)))
          (expect (org-canvas--calendar-event-push-to-api
                   data (make-hash-table))
                  :to-throw 'error)))))

  (it "re-signals generic API error (non-404, non-timeout)"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("500 Internal Server Error")))))
        (let ((data '(:title "Event" :canvas-id nil :pom 1)))
          (expect (org-canvas--calendar-event-push-to-api
                   data (make-hash-table))
                  :to-throw 'error))))))

(describe "org-canvas-delete-all-calendar-events"
  (it "deletes events and clears local properties"
    (let* ((temp-dir (make-temp-file "cal-del-test" t))
           (cal-file (expand-file-name "calendar.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file cal-file
              (insert "#+TITLE: Calendar Events\n* Event A\n:PROPERTIES:\n:CANVAS_ID: 10\n:LAST_SYNCED: [2026-01-01 Wed 12:00]\n:PAYLOAD_HASH: abc\n:END:\n"))
            (let ((org-canvas-calendar-events-file cal-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                            ((symbol-function 'org-canvas-api-request-all-pages)
                             (lambda (_method _url &optional _params)
                               '(((id . 10) (title . "Event A")))))
                            ((symbol-function 'org-canvas-api-request)
                             (lambda (_method _url &rest _args) nil)))
                    (org-canvas-delete-all-calendar-events)
                    (with-current-buffer (find-file-noselect cal-file)
                      (goto-char (point-min))
                      (re-search-forward "^\\*+ " nil t)
                      (org-back-to-heading)
                      (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
                      (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
                      (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)))))))
        (let ((buf (find-buffer-visiting cal-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "handles API failure on individual delete"
    (with-org-canvas-test-config
      (with-sync-test-env
        (let ((org-canvas-calendar-events-file "/tmp/nonexistent-cal.org"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                    ((symbol-function 'org-canvas-api-request-all-pages)
                     (lambda (_method _url &optional _params)
                       '(((id . 99) (title . "Bad Event")))))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (_method _url &rest _args)
                       (signal 'error '("403 Forbidden")))))
            ;; Should not throw — errors are caught and logged
            (org-canvas-delete-all-calendar-events)))))))

(describe "org-canvas-delete-calendar-event-at-point"
  (it "deletes event and clears properties"
    (with-temp-org-buffer
     "* My Event
:PROPERTIES:
:CANVAS_ID: 42
:LAST_SYNCED: [2026-01-01 Wed 12:00]
:PAYLOAD_HASH: xyz
:END:
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                 ((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args) nil))
                 ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                 ((symbol-function 'display-buffer) (lambda (_) nil))
                 ((symbol-function 'save-buffer) (lambda () nil)))
         (org-canvas-delete-calendar-event-at-point)
         (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
         (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)))))

  (it "errors when no CANVAS_ID"
    (with-temp-org-buffer
     "* New Event
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-calendar-event-at-point) :to-throw 'user-error)))

  (it "handles API error gracefully"
    (with-temp-org-buffer
     "* Event
:PROPERTIES:
:CANVAS_ID: 55
:END:
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                 ((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    (signal 'error '("500 Error"))))
                 ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                 ((symbol-function 'display-buffer) (lambda (_) nil)))
         ;; Should not throw — error is caught
         (org-canvas-delete-calendar-event-at-point)
         ;; CANVAS_ID should still be present (delete failed)
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "55"))))))

(describe "org-canvas-pull-calendar-events"
  (it "imports events into new file"
    (let* ((temp-dir (make-temp-file "cal-pull-test" t))
           (cal-file (expand-file-name "calendar.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-calendar-events-file cal-file)
                (org-agenda-files nil))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 100) (title . "Office Hours")
                              (start_at . "2026-09-01T14:00:00Z")
                              (end_at . "2026-09-01T15:30:00Z")
                              (all_day . :json-false)
                              (location_name . "Room 210")))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil))
                        ((symbol-function 'org-canvas--pull-confirm-overwrite)
                         (lambda (&rest _) nil)))
                (org-canvas-pull-calendar-events)
                (with-current-buffer (find-file-noselect cal-file)
                  (expect (buffer-string) :to-match "Office Hours")
                  (expect (buffer-string) :to-match "CANVAS_ID")))))
        (let ((buf (find-buffer-visiting cal-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;; org-canvas-calendar-test.el ends here
