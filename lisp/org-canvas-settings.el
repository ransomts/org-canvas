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

(defconst org-canvas--settings-valid-late-intervals
  '("day" "hour")
  "Valid values for LATE_SUBMISSION_INTERVAL.")

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
         (allow-student-discussion-topics (org-canvas-org-get-property pom "ALLOW_STUDENT_DISCUSSION_TOPICS"))
         (allow-student-discussion-editing (org-canvas-org-get-property pom "ALLOW_STUDENT_DISCUSSION_EDITING"))
         (allow-student-forum-attachments (org-canvas-org-get-property pom "ALLOW_STUDENT_FORUM_ATTACHMENTS"))
         (lock-all-announcements (org-canvas-org-get-property pom "LOCK_ALL_ANNOUNCEMENTS"))
         (restrict-student-future-view (org-canvas-org-get-property pom "RESTRICT_STUDENT_FUTURE_VIEW"))
         (restrict-student-past-view (org-canvas-org-get-property pom "RESTRICT_STUDENT_PAST_VIEW"))
         (show-announcements-on-home-page (org-canvas-org-get-property pom "SHOW_ANNOUNCEMENTS_ON_HOME_PAGE"))
         (home-page-announcement-limit (org-canvas-org-get-property pom "HOME_PAGE_ANNOUNCEMENT_LIMIT"))
         (hide-distribution-graphs (org-canvas-org-get-property pom "HIDE_DISTRIBUTION_GRAPHS"))
         (grading-standard-id (org-canvas-org-get-property pom "GRADING_STANDARD_ID"))
         ;; Late policy properties
         (late-submission-deduction (org-canvas-org-get-property pom "LATE_SUBMISSION_DEDUCTION"))
         (late-submission-deduction-enabled (org-canvas-org-get-property pom "LATE_SUBMISSION_DEDUCTION_ENABLED"))
         (late-submission-interval (org-canvas--validate-property
                                    (org-canvas-org-get-property pom "LATE_SUBMISSION_INTERVAL")
                                    org-canvas--settings-valid-late-intervals
                                    "LATE_SUBMISSION_INTERVAL"))
         (late-submission-minimum-percent (org-canvas-org-get-property pom "LATE_SUBMISSION_MINIMUM_PERCENT"))
         (late-submission-minimum-percent-enabled (org-canvas-org-get-property pom "LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED"))
         (missing-submission-deduction (org-canvas-org-get-property pom "MISSING_SUBMISSION_DEDUCTION"))
         (missing-submission-deduction-enabled (org-canvas-org-get-property pom "MISSING_SUBMISSION_DEDUCTION_ENABLED"))
         ;; Course image (file link or URL)
         (course-image-raw (org-canvas-org-get-property pom "COURSE_IMAGE"))
         (syllabus-body (org-canvas--export-subtree-body-to-html)))
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
          :allow-student-discussion-topics allow-student-discussion-topics
          :allow-student-discussion-editing allow-student-discussion-editing
          :allow-student-forum-attachments allow-student-forum-attachments
          :lock-all-announcements lock-all-announcements
          :restrict-student-future-view restrict-student-future-view
          :restrict-student-past-view restrict-student-past-view
          :show-announcements-on-home-page show-announcements-on-home-page
          :home-page-announcement-limit home-page-announcement-limit
          :hide-distribution-graphs hide-distribution-graphs
          :grading-standard-id grading-standard-id
          :late-submission-deduction late-submission-deduction
          :late-submission-deduction-enabled late-submission-deduction-enabled
          :late-submission-interval late-submission-interval
          :late-submission-minimum-percent late-submission-minimum-percent
          :late-submission-minimum-percent-enabled late-submission-minimum-percent-enabled
          :missing-submission-deduction missing-submission-deduction
          :missing-submission-deduction-enabled missing-submission-deduction-enabled
          :course-image-path (when (and course-image-raw
                                        (string-match "\\[\\[file:\\([^]]+\\)\\]" course-image-raw))
                               (expand-file-name
                                (match-string 1 course-image-raw)
                                (file-name-directory
                                 (buffer-file-name))))
          :course-image-url (when (and course-image-raw
                                       (not (string-prefix-p "[[" course-image-raw))
                                       (string-match-p "^https?://" course-image-raw))
                              course-image-raw)
          :syllabus-body syllabus-body)))

;;;; 2. Build Payload

(defun org-canvas--settings-puthash-when (course data key api-key &optional boolean-p)
  "Conditionally set API-KEY in COURSE hash from DATA plist KEY.
When BOOLEAN-P is non-nil, convert \"true\"/\"false\" to t/:json-false."
  (let ((val (plist-get data key)))
    (when val
      (puthash api-key
               (if boolean-p
                   (if (equal val "true") t :json-false)
                 val)
               course))))

(defun org-canvas--settings-build-payload (data)
  "Build a Canvas course update payload from parsed DATA plist.
Returns a hash-table suitable for `json-encode'."
  (let ((payload (make-hash-table :test 'equal))
        (course (make-hash-table :test 'equal)))
    (org-canvas--settings-puthash-when course data :title "name")
    (org-canvas--settings-puthash-when course data :time-zone "time_zone")
    (org-canvas--settings-puthash-when course data :default-view "default_view")
    (org-canvas--settings-puthash-when course data :apply-weights "apply_assignment_group_weights" t)
    (org-canvas--settings-puthash-when course data :hide-final-grades "hide_final_grades" t)
    (org-canvas--settings-puthash-when course data :public-syllabus "public_syllabus" t)
    (org-canvas--settings-puthash-when course data :is-public "is_public" t)
    (org-canvas--settings-puthash-when course data :license "license")
    (org-canvas--settings-puthash-when course data :start-at "start_at")
    (org-canvas--settings-puthash-when course data :end-at "end_at")
    (org-canvas--settings-puthash-when course data :allow-student-discussion-topics "allow_student_discussion_topics" t)
    (org-canvas--settings-puthash-when course data :allow-student-discussion-editing "allow_student_discussion_editing" t)
    (org-canvas--settings-puthash-when course data :allow-student-forum-attachments "allow_student_forum_attachments" t)
    (org-canvas--settings-puthash-when course data :lock-all-announcements "lock_all_announcements" t)
    (org-canvas--settings-puthash-when course data :restrict-student-future-view "restrict_student_future_view" t)
    (org-canvas--settings-puthash-when course data :restrict-student-past-view "restrict_student_past_view" t)
    (org-canvas--settings-puthash-when course data :show-announcements-on-home-page "show_announcements_on_home_page" t)
    (org-canvas--settings-puthash-when course data :hide-distribution-graphs "hide_distribution_graphs" t)
    (let ((limit (plist-get data :home-page-announcement-limit)))
      (when limit
        (puthash "home_page_announcement_limit"
                 (org-canvas--safe-string-to-number limit "HOME_PAGE_ANNOUNCEMENT_LIMIT")
                 course)))
    (let ((gs-id (plist-get data :grading-standard-id)))
      (when gs-id
        (puthash "grading_standard_id"
                 (org-canvas--safe-string-to-number gs-id "GRADING_STANDARD_ID")
                 course)))
    ;; Course image: image_id (from file upload) or image_url (plain URL)
    (let ((image-id (plist-get data :course-image-id)))
      (when image-id
        (puthash "image_id" image-id course)))
    (let ((image-url (plist-get data :course-image-url)))
      (when image-url
        (puthash "image_url" image-url course)))
    (org-canvas--settings-puthash-when course data :syllabus-body "syllabus_body")
    (puthash "course" course payload)
    payload))

(defun org-canvas--settings-build-late-policy-payload (data)
  "Build a Canvas late policy payload from parsed DATA plist.
Returns a hash-table wrapped in `late_policy' key, or nil if no
late policy properties are set."
  (let ((deduction (plist-get data :late-submission-deduction))
        (deduction-enabled (plist-get data :late-submission-deduction-enabled))
        (interval (plist-get data :late-submission-interval))
        (min-pct (plist-get data :late-submission-minimum-percent))
        (min-pct-enabled (plist-get data :late-submission-minimum-percent-enabled))
        (missing-deduction (plist-get data :missing-submission-deduction))
        (missing-enabled (plist-get data :missing-submission-deduction-enabled)))
    (when (or deduction deduction-enabled interval min-pct min-pct-enabled
              missing-deduction missing-enabled)
      (let ((lp (make-hash-table :test 'equal))
            (payload (make-hash-table :test 'equal)))
        (when deduction
          (puthash "late_submission_deduction"
                   (org-canvas--safe-string-to-number deduction "LATE_SUBMISSION_DEDUCTION")
                   lp))
        (when deduction-enabled
          (puthash "late_submission_deduction_enabled"
                   (if (equal deduction-enabled "true") t :json-false)
                   lp))
        (when interval
          (puthash "late_submission_interval" interval lp))
        (when min-pct
          (puthash "late_submission_minimum_percent"
                   (org-canvas--safe-string-to-number min-pct "LATE_SUBMISSION_MINIMUM_PERCENT")
                   lp))
        (when min-pct-enabled
          (puthash "late_submission_minimum_percent_enabled"
                   (if (equal min-pct-enabled "true") t :json-false)
                   lp))
        (when missing-deduction
          (puthash "missing_submission_deduction"
                   (org-canvas--safe-string-to-number missing-deduction "MISSING_SUBMISSION_DEDUCTION")
                   lp))
        (when missing-enabled
          (puthash "missing_submission_deduction_enabled"
                   (if (equal missing-enabled "true") t :json-false)
                   lp))
        (puthash "late_policy" lp payload)
        payload))))

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

(defun org-canvas--settings-push-late-policy (late-policy-payload)
  "Push LATE-POLICY-PAYLOAD to Canvas late policy endpoint.
Tries PATCH first; if Canvas returns an error (no existing policy),
falls back to POST."
  (when late-policy-payload
    (let ((endpoint (org-canvas-api-course-endpoint "late_policy")))
      (elog-info org-canvas--logger "[Execute] Syncing late policy...")
      (condition-case _err
          (progn
            (org-canvas-api-request 'PATCH endpoint :data late-policy-payload)
            (elog-info org-canvas--logger "[Execute] Late policy updated via PATCH"))
        (error
         (elog-debug org-canvas--logger "[Execute] PATCH failed, trying POST...")
         (condition-case err2
             (progn
               (org-canvas-api-request 'POST endpoint :data late-policy-payload)
               (elog-info org-canvas--logger "[Execute] Late policy created via POST"))
           (error
            (elog-error org-canvas--logger "[Execute] Late policy sync failed: %s"
              (error-message-string err2)))))))))

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

;;;; Navigation Tabs

(defconst org-canvas--settings-immutable-tabs '("home" "settings")
  "Tab labels that Canvas refuses to modify (case-insensitive).")

(defun org-canvas--settings-parse-navigation ()
  "Parse the ** Navigation sub-heading under the current course heading.
Returns a list of plists (:label STRING :hidden BOOL :position INT),
or nil if no Navigation heading exists.
Items in strikethrough (+Tab+) are hidden."
  (save-excursion
    (let ((subtree-end (save-excursion (org-end-of-subtree t) (point)))
          result)
      (when (re-search-forward "^\\*\\* Navigation" subtree-end t)
        (let ((nav-end (save-excursion
                         (if (re-search-forward "^\\*\\* " subtree-end t)
                             (match-beginning 0)
                           subtree-end)))
              (pos 1))
          (while (re-search-forward
                  "^[ \t]*[0-9]+\\.[ \t]+\\(\\+\\(.+\\)\\+\\|\\(.+\\)\\)$"
                  nav-end t)
            (let* ((struck (match-string 2))
                   (plain (match-string 3))
                   (label (or struck plain)))
              (push (list :label label :hidden (not (null struck)) :position pos)
                    result)
              (setq pos (1+ pos))))))
      (nreverse result))))

(cl-defun org-canvas--settings-sync-tabs (navigation)
  "Sync NAVIGATION tab state to Canvas.
NAVIGATION is a list of (:label :hidden :position) plists.
Fetches current tabs, diffs against desired state, and PUTs changes."
  (unless navigation
    (cl-return-from org-canvas--settings-sync-tabs nil))

  (when org-canvas--dry-run
    (elog-info org-canvas--logger "[DRY-RUN] Would sync %d navigation tabs" (length navigation))
    (dolist (tab navigation)
      (elog-info org-canvas--logger "[DRY-RUN]   %s: position=%d hidden=%s"
                 (plist-get tab :label) (plist-get tab :position)
                 (if (plist-get tab :hidden) "yes" "no")))
    (cl-return-from org-canvas--settings-sync-tabs nil))

  (let* ((url (org-canvas-api-course-endpoint "tabs"))
         (current-tabs (org-canvas-api-request 'GET url))
         (changes 0))
    (dolist (desired navigation)
      (let* ((label (plist-get desired :label))
             (label-down (downcase label))
             (hidden (plist-get desired :hidden))
             (position (plist-get desired :position)))
        ;; Guard: skip immutable tabs
        (if (member label-down org-canvas--settings-immutable-tabs)
            (when hidden
              (elog-warning org-canvas--logger
                "[Tabs] Cannot hide '%s' — Canvas does not allow it" label))
          ;; Find matching tab by label
          (let ((tab (cl-find-if
                      (lambda (t-item)
                        (string= (downcase (alist-get 'label t-item)) label-down))
                      current-tabs)))
            (if (not tab)
                (elog-warning org-canvas--logger "[Tabs] Tab '%s' not found on Canvas" label)
              (let* ((tab-id (alist-get 'id tab))
                     (cur-hidden (eq (alist-get 'hidden tab) t))
                     (cur-pos (alist-get 'position tab))
                     (needs-update (or (not (eq hidden cur-hidden))
                                       (not (equal position cur-pos)))))
                (when needs-update
                  (let ((payload `((hidden . ,(if hidden t :json-false))
                                   (position . ,position)))
                        (tab-url (org-canvas-api-course-endpoint
                                  (format "tabs/%s" tab-id))))
                    (condition-case err
                        (progn
                          (org-canvas-api-request 'PUT tab-url :data payload)
                          (setq changes (1+ changes))
                          (elog-info org-canvas--logger
                            "[Tabs] Updated '%s': hidden=%s position=%d"
                            label (if hidden "yes" "no") position))
                      (error
                       (elog-warning org-canvas--logger
                         "[Tabs] Failed to update '%s': %s"
                         label (error-message-string err))))))))))))
    (elog-info org-canvas--logger "[Tabs] %d tab(s) updated" changes)))

(defun org-canvas--settings-pull-tabs ()
  "Pull navigation tabs from Canvas and write as ** Navigation sub-heading.
Returns the formatted Org text, or nil if no tabs."
  (let* ((url (org-canvas-api-course-endpoint "tabs"))
         (tabs (org-canvas-api-request 'GET url)))
    (when tabs
      ;; Sort by position
      (setq tabs (sort (copy-sequence tabs)
                       (lambda (a b)
                         (< (or (alist-get 'position a) 999)
                            (or (alist-get 'position b) 999)))))
      (let ((lines nil)
            (pos 1))
        (dolist (tab tabs)
          (let ((label (alist-get 'label tab))
                (hidden (eq (alist-get 'hidden tab) t)))
            (push (format "%d. %s"
                          pos
                          (if hidden (format "+%s+" label) label))
                  lines)
            (setq pos (1+ pos))))
        (concat "** Navigation\n" (string-join (nreverse lines) "\n") "\n")))))

;;;; Course Image

(defun org-canvas--settings-resolve-course-image (data)
  "Upload local course image if needed and add :course-image-id to DATA.
If :course-image-path is set and file exists, uploads it to Canvas
and returns DATA with :course-image-id added.
If :course-image-url is set, returns DATA unchanged.
Otherwise returns DATA unchanged."
  (let ((image-path (plist-get data :course-image-path)))
    (if (and image-path (not org-canvas--dry-run))
        (if (file-exists-p image-path)
            (progn
              (elog-info org-canvas--logger "[Image] Uploading course image: %s"
                         (file-name-nondirectory image-path))
              (condition-case err
                  (let* ((file-obj (org-canvas--upload-file image-path))
                         (file-id (alist-get 'id file-obj)))
                    (elog-info org-canvas--logger "[Image] Upload complete, file ID: %s" file-id)
                    (plist-put data :course-image-id file-id))
                (error
                 (elog-warning org-canvas--logger "[Image] Upload failed: %s"
                               (error-message-string err))
                 data)))
          (progn
            (elog-warning org-canvas--logger "[Image] File not found: %s" image-path)
            data))
      (when (and image-path org-canvas--dry-run)
        (elog-info org-canvas--logger "[DRY-RUN] Would upload course image: %s"
                   (file-name-nondirectory image-path)))
      data)))

;;;; Interactive Commands

;;;###autoload
(defun org-canvas-sync-settings ()
  "Synchronize course settings to Canvas.
Reads settings from the first heading in `org-canvas-settings-file'
and pushes them to Canvas via PUT /courses/:id."
  (interactive)
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
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
                   (navigation (org-canvas--settings-parse-navigation))
                   ;; Upload course image if local file specified
                   (data (org-canvas--settings-resolve-course-image data))
                   (payload (org-canvas--settings-build-payload data))
                   (late-policy-payload (org-canvas--settings-build-late-policy-payload data))
                   (response (org-canvas--settings-push data payload)))
              (org-canvas--settings-push-late-policy late-policy-payload)
              (org-canvas--settings-sync-tabs navigation)
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

(defun org-canvas--settings-replace-syllabus-body (syllabus-body)
  "Replace the body under the current heading with SYLLABUS-BODY.
Point must be at the heading."
  (let ((body-start (save-excursion
                      (org-end-of-meta-data t)
                      (point)))
        (body-end (save-excursion
                    (org-end-of-subtree t)
                    (point))))
    (delete-region body-start body-end)
    (goto-char body-start)
    (insert "\n" syllabus-body "\n")))

(defun org-canvas--settings-pull-set-properties (pom response syllabus-body
                                                     &optional late-policy)
  "Set all settings properties at POM from API RESPONSE.
SYLLABUS-BODY is the pre-extracted syllabus HTML (may be nil).
LATE-POLICY is the late policy API response (may be nil)."
  (let ((time-zone (alist-get 'time_zone response))
        (default-view (alist-get 'default_view response))
        (license (org-canvas--alist-get-non-null 'license response))
        (start-at (alist-get 'start_at response))
        (end-at (alist-get 'end_at response)))
    (when time-zone
      (org-canvas-org-set-property pom "TIME_ZONE" time-zone))
    (when default-view
      (org-canvas-org-set-property pom "DEFAULT_VIEW" default-view))
    (org-canvas--pull-set-boolean-property
     pom "APPLY_WEIGHTS" (alist-get 'apply_assignment_group_weights response))
    (org-canvas--pull-set-boolean-property
     pom "HIDE_FINAL_GRADES" (alist-get 'hide_final_grades response))
    (org-canvas--pull-set-boolean-property
     pom "PUBLIC_SYLLABUS" (alist-get 'public_syllabus response))
    (org-canvas--pull-set-boolean-property
     pom "IS_PUBLIC" (alist-get 'is_public response))
    (when license
      (org-canvas-org-set-property pom "LICENSE" license))
    (org-canvas--pull-set-timestamp-property pom "START_AT" start-at)
    (org-canvas--pull-set-timestamp-property pom "END_AT" end-at)
    (org-canvas--pull-set-boolean-property
     pom "ALLOW_STUDENT_DISCUSSION_TOPICS" (alist-get 'allow_student_discussion_topics response))
    (org-canvas--pull-set-boolean-property
     pom "ALLOW_STUDENT_DISCUSSION_EDITING" (alist-get 'allow_student_discussion_editing response))
    (org-canvas--pull-set-boolean-property
     pom "ALLOW_STUDENT_FORUM_ATTACHMENTS" (alist-get 'allow_student_forum_attachments response))
    (org-canvas--pull-set-boolean-property
     pom "LOCK_ALL_ANNOUNCEMENTS" (alist-get 'lock_all_announcements response))
    (org-canvas--pull-set-boolean-property
     pom "RESTRICT_STUDENT_FUTURE_VIEW" (alist-get 'restrict_student_future_view response))
    (org-canvas--pull-set-boolean-property
     pom "RESTRICT_STUDENT_PAST_VIEW" (alist-get 'restrict_student_past_view response))
    (org-canvas--pull-set-boolean-property
     pom "SHOW_ANNOUNCEMENTS_ON_HOME_PAGE" (alist-get 'show_announcements_on_home_page response))
    (org-canvas--pull-set-boolean-property
     pom "HIDE_DISTRIBUTION_GRAPHS" (alist-get 'hide_distribution_graphs response))
    (let ((limit (alist-get 'home_page_announcement_limit response)))
      (when limit
        (org-canvas-org-set-property pom "HOME_PAGE_ANNOUNCEMENT_LIMIT"
                                     (format "%s" limit))))
    (let ((gs-id (alist-get 'grading_standard_id response)))
      (when gs-id
        (org-canvas-org-set-property pom "GRADING_STANDARD_ID" (format "%s" gs-id))))
    ;; Late policy properties
    (when late-policy
      (let ((lp (alist-get 'late_policy late-policy)))
        (when lp
          (let ((deduction (alist-get 'late_submission_deduction lp))
                (interval (alist-get 'late_submission_interval lp))
                (min-pct (alist-get 'late_submission_minimum_percent lp))
                (missing-deduction (alist-get 'missing_submission_deduction lp)))
            (when deduction
              (org-canvas-org-set-property pom "LATE_SUBMISSION_DEDUCTION"
                                           (format "%s" deduction)))
            (org-canvas--pull-set-boolean-property
             pom "LATE_SUBMISSION_DEDUCTION_ENABLED"
             (alist-get 'late_submission_deduction_enabled lp))
            (when interval
              (org-canvas-org-set-property pom "LATE_SUBMISSION_INTERVAL" interval))
            (when min-pct
              (org-canvas-org-set-property pom "LATE_SUBMISSION_MINIMUM_PERCENT"
                                           (format "%s" min-pct)))
            (org-canvas--pull-set-boolean-property
             pom "LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED"
             (alist-get 'late_submission_minimum_percent_enabled lp))
            (when missing-deduction
              (org-canvas-org-set-property pom "MISSING_SUBMISSION_DEDUCTION"
                                           (format "%s" missing-deduction)))
            (org-canvas--pull-set-boolean-property
             pom "MISSING_SUBMISSION_DEDUCTION_ENABLED"
             (alist-get 'missing_submission_deduction_enabled lp))))))
    ;; Course image
    (let ((image-url (org-canvas--alist-get-non-null 'image_download_url response)))
      (when image-url
        (org-canvas-org-set-property pom "COURSE_IMAGE" image-url)))
    (org-canvas-org-set-property
     pom "LAST_SYNCED"
     (format-time-string "[%Y-%m-%d %a %H:%M]"))
    (when syllabus-body
      (org-canvas--settings-replace-syllabus-body syllabus-body))))

;;;###autoload
(defun org-canvas-pull-settings ()
  "Pull course settings from Canvas into settings.org.
Fetches course data via GET /courses/:id?include[]=syllabus_body
and populates the first heading's properties.  Creates the file
and heading if they don't exist."
  (interactive)
  (org-canvas--start-operation "PULLING SETTINGS FROM CANVAS")
  (let* ((endpoint (org-canvas-api-course-endpoint ""))
         (response (org-canvas-api-request
                    'GET endpoint
                    :params '(("include[]" . "syllabus_body")
                              ("include[]" . "course_image"))))
         (name (alist-get 'name response))
         (syllabus-body (org-canvas--alist-get-non-null 'syllabus_body response))
         (settings-file (expand-file-name org-canvas-settings-file)))
    ;; Fetch late policy (separate endpoint)
    (let ((late-policy (condition-case nil
                           (org-canvas-api-request
                            'GET (org-canvas-api-course-endpoint "late_policy"))
                         (error nil))))
      (org-canvas--pull-confirm-overwrite settings-file "settings")
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
        (org-canvas--settings-pull-set-properties
         (point) response syllabus-body late-policy)
        ;; Pull navigation tabs
        (let ((nav-text (condition-case nil
                            (org-canvas--settings-pull-tabs)
                          (error nil))))
          (when nav-text
            (save-excursion
              ;; Remove existing ** Navigation if present
              (goto-char (point-min))
              (when (re-search-forward "^\\*\\* Navigation" nil t)
                (beginning-of-line)
                (let ((start (point))
                      (end (save-excursion
                             (if (re-search-forward "^\\*\\* " nil t)
                                 (match-beginning 0)
                               (point-max)))))
                  (delete-region start end)))
              ;; Insert at end of course heading subtree
              (goto-char (point-min))
              (re-search-forward "^\\*+ " nil t)
              (org-back-to-heading t)
              (org-end-of-subtree t)
              (insert "\n" nav-text))))
        (save-buffer)))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> SETTINGS PULL COMPLETE")
    (elog-info org-canvas--logger "Course: %s" name)
    (elog-info org-canvas--logger "========================================")
    (message "Settings pull complete: %s" name)))

(provide 'org-canvas-settings)
;;; org-canvas-settings.el ends here
