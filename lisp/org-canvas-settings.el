;;; org-canvas-settings.el --- Course settings sync -*- lexical-binding: t; -*-

;;; Commentary:

;; This module syncs course-level settings to/from Canvas.
;;
;; FILE STRUCTURE
;; ==============
;; In settings.org:
;;   - Single level-1 heading = Course name
;;   - Heading body = Syllabus content (exported to HTML)
;;
;; PROPERTIES
;; ==========
;; TIME_ZONE          - IANA timezone (e.g. "America/New_York")
;; DEFAULT_VIEW       - Course home page view
;; APPLY_WEIGHTS      - Weight assignment groups ("true"/"false")
;; HIDE_FINAL_GRADES  - Hide grades from students ("true"/"false")
;; PUBLIC_SYLLABUS     - Public syllabus ("true"/"false")
;; IS_PUBLIC          - Public course ("true"/"false")
;; LICENSE            - Content license
;; START_AT           - Course start date (Org timestamp)
;; END_AT             - Course end date (Org timestamp)
;; LAST_SYNCED        - Last sync timestamp (auto-populated)
;;
;; SYNC
;; ====
;; Push: PUT /api/v1/courses/:course_id
;; Pull: GET /api/v1/courses/:course_id?include[]=syllabus_body
;;
;; Unlike other modules, settings operates on a single item (the course
;; itself) rather than iterating over multiple headings.

;;; Code:

(require 'org-canvas-core)
(require 'ox-html)
(require 'elog)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-settings-file (org-canvas--path "settings.org")
  "Path to the settings.org file."
  :type 'file
  :group 'org-canvas)

(defconst org-canvas--settings-valid-views
  '("feed" "wiki" "modules" "syllabus" "assignments")
  "Valid values for DEFAULT_VIEW.")

(defconst org-canvas--settings-valid-licenses
  '("private" "cc_by" "cc_by_sa" "cc_by_nc" "cc_by_nc_sa"
    "cc_by_nd" "cc_by_nc_nd" "public_domain")
  "Valid values for LICENSE.")

;;;; Helpers

(defun org-canvas--settings-format-timestamp (iso8601)
  "Convert ISO8601 timestamp from Canvas API to Org timestamp format.
Returns an Org active timestamp string, or nil if ISO8601 is nil."
  (when (and iso8601 (not (equal iso8601 :null)))
    (let ((time (date-to-time iso8601)))
      (format-time-string "<%Y-%m-%d %a %H:%M>" time t))))

;;;; 1. Parse

(defun org-canvas--settings-parse-entry ()
  "Parse course settings from the first heading in the current buffer.
Returns a plist with keys :title, :pom, :time-zone, :default-view,
:apply-weights, :hide-final-grades, :public-syllabus, :is-public,
:license, :start-at, :end-at, :syllabus-body."
  (org-back-to-heading t)
  (let* ((pom (point-marker))
         (title (org-canvas--strip-statistics-cookie
                 (org-get-heading t t t t)))
         (time-zone (org-canvas-org-get-property pom "TIME_ZONE"))
         (default-view (org-canvas--validate-property
                        (org-canvas-org-get-property pom "DEFAULT_VIEW")
                        org-canvas--settings-valid-views
                        "DEFAULT_VIEW"))
         (apply-weights (org-canvas-org-get-property pom "APPLY_WEIGHTS"))
         (hide-final (org-canvas-org-get-property pom "HIDE_FINAL_GRADES"))
         (public-syllabus (org-canvas-org-get-property pom "PUBLIC_SYLLABUS"))
         (is-public (org-canvas-org-get-property pom "IS_PUBLIC"))
         (license (org-canvas--validate-property
                   (org-canvas-org-get-property pom "LICENSE")
                   org-canvas--settings-valid-licenses
                   "LICENSE"))
         (start-at (org-canvas-org-parse-timestamp
                    (org-canvas-org-get-property pom "START_AT")))
         (end-at (org-canvas-org-parse-timestamp
                  (org-canvas-org-get-property pom "END_AT")))
         (syllabus-body (org-canvas--export-subtree-to-html)))
    (list :title title
          :pom pom
          :time-zone time-zone
          :default-view default-view
          :apply-weights apply-weights
          :hide-final-grades hide-final
          :public-syllabus public-syllabus
          :is-public is-public
          :license license
          :start-at start-at
          :end-at end-at
          :syllabus-body syllabus-body)))

;;;; 2. Build Payload

(defun org-canvas--settings-build-payload (data)
  "Build a Canvas course update payload from parsed DATA plist.
Returns a hash-table suitable for `json-encode'."
  (let ((payload (make-hash-table :test 'equal)))
    (let ((course (make-hash-table :test 'equal)))
      (when (plist-get data :title)
        (puthash "name" (plist-get data :title) course))
      (when (plist-get data :time-zone)
        (puthash "time_zone" (plist-get data :time-zone) course))
      (when (plist-get data :default-view)
        (puthash "default_view" (plist-get data :default-view) course))
      (when (plist-get data :apply-weights)
        (puthash "apply_assignment_group_weights"
                 (if (equal (plist-get data :apply-weights) "true") t :json-false)
                 course))
      (when (plist-get data :hide-final-grades)
        (puthash "hide_final_grades"
                 (if (equal (plist-get data :hide-final-grades) "true") t :json-false)
                 course))
      (when (plist-get data :public-syllabus)
        (puthash "public_syllabus"
                 (if (equal (plist-get data :public-syllabus) "true") t :json-false)
                 course))
      (when (plist-get data :is-public)
        (puthash "is_public"
                 (if (equal (plist-get data :is-public) "true") t :json-false)
                 course))
      (when (plist-get data :license)
        (puthash "license" (plist-get data :license) course))
      (when (plist-get data :start-at)
        (puthash "start_at" (plist-get data :start-at) course))
      (when (plist-get data :end-at)
        (puthash "end_at" (plist-get data :end-at) course))
      (when (plist-get data :syllabus-body)
        (puthash "syllabus_body" (plist-get data :syllabus-body) course))
      (puthash "course" course payload))
    payload))

;;;; 3. Push

(cl-defun org-canvas--settings-push (data payload)
  "Push course settings PAYLOAD to Canvas.
DATA is the parsed settings plist (used for logging).
Always uses PUT since the course already exists."
  (let ((title (plist-get data :title)))
    ;; Dry-run check
    (when org-canvas--dry-run
      (elog-info org-canvas--logger
        "[DRY-RUN] Would update course settings for '%s'" title)
      (cl-return-from org-canvas--settings-push '((id . "dry-run"))))
    (let ((endpoint (org-canvas-api-course-endpoint "")))
      (elog-info org-canvas--logger
        "[Execute] PUT course settings for '%s' to %s" title endpoint)
      (condition-case err
          (let ((response (org-canvas-api-request 'PUT endpoint :data payload)))
            (elog-info org-canvas--logger
              "[Execute] Course settings update successful")
            response)
        (error
         (elog-error org-canvas--logger
           "[Execute] Course settings update failed: %s"
           (error-message-string err))
         (signal (car err) (cdr err)))))))

;;;; 4. Finalize

(defun org-canvas--settings-finalize (data _response)
  "Save LAST_SYNCED timestamp for course settings.
DATA is the parsed settings plist."
  (let ((pom (plist-get data :pom)))
    (org-canvas-org-set-property
     pom "LAST_SYNCED"
     (format-time-string "[%Y-%m-%d %a %H:%M]"))
    (elog-info org-canvas--logger
      "[Finalize] Saved LAST_SYNCED for course settings")))

;;;; Interactive Commands

;;;###autoload
(defun org-canvas-sync-settings ()
  "Synchronize course settings to Canvas.
Reads settings from the first heading in `org-canvas-settings-file'
and pushes them to Canvas via PUT /courses/:id."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))
  (let ((settings-file (expand-file-name org-canvas-settings-file)))
    (unless (file-exists-p settings-file)
      (error "Settings file not found: %s" settings-file))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> STARTING SETTINGS SYNC")
    (elog-info org-canvas--logger "File: %s" settings-file)
    (elog-info org-canvas--logger "========================================")
    (with-current-buffer (find-file-noselect settings-file)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^\\*+ " nil t)
          (error "No heading found in settings file"))
        (org-back-to-heading t)
        (condition-case err
            (let* ((data (org-canvas--settings-parse-entry))
                   (payload (org-canvas--settings-build-payload data))
                   (response (org-canvas--settings-push data payload)))
              (org-canvas--settings-finalize data response)
              (save-buffer)
              (elog-info org-canvas--logger "========================================")
              (elog-info org-canvas--logger ">>> SETTINGS SYNC COMPLETE")
              (elog-info org-canvas--logger "========================================")
              (message "Settings sync complete."))
          (error
           (elog-error org-canvas--logger "[FAILED] Settings sync: %s"
             (error-message-string err))
           (message "Settings sync FAILED: %s" (error-message-string err))))))))

;;;###autoload
(defun org-canvas-pull-settings ()
  "Pull course settings from Canvas into settings.org.
Fetches course data via GET /courses/:id?include[]=syllabus_body
and populates the first heading's properties.  Creates the file
and heading if they don't exist."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create "*canvas-log*"))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> PULLING SETTINGS FROM CANVAS")
  (elog-info org-canvas--logger "========================================")
  (let* ((endpoint (org-canvas-api-course-endpoint ""))
         (response (org-canvas-api-request
                    'GET endpoint
                    :params '(("include[]" . "syllabus_body"))))
         (name (alist-get 'name response))
         (time-zone (alist-get 'time_zone response))
         (default-view (alist-get 'default_view response))
         (apply-weights (alist-get 'apply_assignment_group_weights response))
         (hide-final (alist-get 'hide_final_grades response))
         (public-syllabus (alist-get 'public_syllabus response))
         (is-public (alist-get 'is_public response))
         (license (let ((v (alist-get 'license response)))
                    (if (or (null v) (eq v :null)) nil v)))
         (start-at (alist-get 'start_at response))
         (end-at (alist-get 'end_at response))
         (syllabus-body (let ((v (alist-get 'syllabus_body response)))
                          (if (or (null v) (eq v :null)) nil v)))
         (settings-file (expand-file-name org-canvas-settings-file)))
    ;; Open or create the settings file
    (unless (file-exists-p settings-file)
      (with-temp-file settings-file
        (insert (format "#+TITLE: Settings\n* %s\n" (or name "Course")))))
    (with-current-buffer (find-file-noselect settings-file)
      (goto-char (point-min))
      (unless (re-search-forward "^\\*+ " nil t)
        (goto-char (point-max))
        (insert (format "\n* %s\n" (or name "Course"))))
      (org-back-to-heading t)
      ;; Update heading title
      (when name
        (org-edit-headline name))
      ;; Set properties
      (let ((pom (point)))
        (when time-zone
          (org-canvas-org-set-property pom "TIME_ZONE" time-zone))
        (when default-view
          (org-canvas-org-set-property pom "DEFAULT_VIEW" default-view))
        (org-canvas-org-set-property
         pom "APPLY_WEIGHTS" (if (eq apply-weights t) "true" "false"))
        (org-canvas-org-set-property
         pom "HIDE_FINAL_GRADES" (if (eq hide-final t) "true" "false"))
        (org-canvas-org-set-property
         pom "PUBLIC_SYLLABUS" (if (eq public-syllabus t) "true" "false"))
        (org-canvas-org-set-property
         pom "IS_PUBLIC" (if (eq is-public t) "true" "false"))
        (when license
          (org-canvas-org-set-property pom "LICENSE" license))
        (when (and start-at (not (eq start-at :null)))
          (let ((ts (org-canvas--settings-format-timestamp start-at)))
            (when ts (org-canvas-org-set-property pom "START_AT" ts))))
        (when (and end-at (not (eq end-at :null)))
          (let ((ts (org-canvas--settings-format-timestamp end-at)))
            (when ts (org-canvas-org-set-property pom "END_AT" ts))))
        (org-canvas-org-set-property
         pom "LAST_SYNCED"
         (format-time-string "[%Y-%m-%d %a %H:%M]"))
        ;; Handle syllabus body — insert/replace after properties
        (when syllabus-body
          (let ((body-start (save-excursion
                              (org-end-of-meta-data t)
                              (point)))
                (body-end (save-excursion
                            (org-end-of-subtree t)
                            (point))))
            (delete-region body-start body-end)
            (goto-char body-start)
            (insert "\n" syllabus-body "\n"))))
      (save-buffer))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> SETTINGS PULL COMPLETE")
    (elog-info org-canvas--logger "Course: %s" name)
    (elog-info org-canvas--logger "========================================")
    (message "Settings pull complete: %s" name)))

(provide 'org-canvas-settings)
;;; org-canvas-settings.el ends here
