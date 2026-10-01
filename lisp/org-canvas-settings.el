;;; org-canvas-settings.el --- Course settings sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'cl-lib)

(declare-function org-canvas--file-pull-download "org-canvas-files"
                  (display-name download-url local-path size))
(declare-function org-canvas--validate-settings-structure "org-canvas-validate")

;;;; Configuration

(defcustom org-canvas-settings-file (org-canvas--path "settings.org")
  "Path to the settings.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-settings-file "settings.org")
;; Every heading of settings.org is the course itself (issue #292).
(org-canvas-register-web-pages
 "Settings" 'org-canvas-settings-file '(:path "settings"))
(org-canvas-register-properties "settings"
  :label "Settings"
  :file-var 'org-canvas-settings-file
  :query "LEVEL=1"
  :always-on-canvas t
  :structural-fn #'org-canvas--validate-settings-structure
  :properties
  `((:org-prop "APPLY_WEIGHTS" :data-key :apply_weights :type boolean
     :doc "Weight assignment groups")
    (:org-prop "HIDE_FINAL_GRADES" :data-key :hide_final_grades :type boolean
     :doc "Hide grades from students")
    (:org-prop "PUBLIC_SYLLABUS" :data-key :public_syllabus :type boolean
     :doc "Make syllabus publicly visible")
    (:org-prop "IS_PUBLIC" :data-key :is_public :type boolean
     :doc "Make course publicly visible")
    (:org-prop "DEFAULT_VIEW" :data-key :default_view :type enum
     :values ,org-canvas--valid-views
     :doc "Course homepage view")
    (:org-prop "LICENSE" :data-key :license :type enum
     :values ,org-canvas--valid-licenses
     :doc "Content license")
    (:org-prop "POST_POLICY" :data-key :post_policy :type enum
     :values ,org-canvas--valid-post-policies
     :doc "Grade post policy: manual holds every grade until it is posted, automatic posts as graded; set through GraphQL, since REST only reads it")
    (:org-prop "START_AT" :data-key :start_at :type timestamp
     :doc "Course start date")
    (:org-prop "END_AT" :data-key :end_at :type timestamp
     :doc "Course end date")
    (:org-prop "ALLOW_STUDENT_DISCUSSION_TOPICS" :data-key :allow_student_discussion_topics :type boolean
     :doc "Students can create discussions")
    (:org-prop "ALLOW_STUDENT_DISCUSSION_EDITING" :data-key :allow_student_discussion_editing :type boolean
     :doc "Students can edit discussions")
    (:org-prop "ALLOW_STUDENT_FORUM_ATTACHMENTS" :data-key :allow_student_forum_attachments :type boolean
     :doc "Students can attach files")
    (:org-prop "LOCK_ALL_ANNOUNCEMENTS" :data-key :lock_all_announcements :type boolean
     :doc "Lock all announcements")
    (:org-prop "RESTRICT_STUDENT_FUTURE_VIEW" :data-key :restrict_student_future_view :type boolean
     :doc "Hide future content from students")
    (:org-prop "RESTRICT_STUDENT_PAST_VIEW" :data-key :restrict_student_past_view :type boolean
     :doc "Hide past content from students")
    (:org-prop "SHOW_ANNOUNCEMENTS_ON_HOME_PAGE" :data-key :show_announcements_on_home_page :type boolean
     :doc "Show announcements on homepage")
    (:org-prop "HOME_PAGE_ANNOUNCEMENT_LIMIT" :data-key :home_page_announcement_limit :type number
     :doc "Number of announcements shown")
    (:org-prop "HIDE_DISTRIBUTION_GRAPHS" :data-key :hide_distribution_graphs :type boolean
     :doc "Hide grade distribution graphs")
    (:org-prop "GRADING_STANDARD_ID" :data-key :grading_standard_id :type number
     :doc "Canvas grading standard ID")
    (:org-prop "LATE_SUBMISSION_DEDUCTION" :data-key :late_submission_deduction :type number
     :doc "Points deducted per interval")
    (:org-prop "LATE_SUBMISSION_DEDUCTION_ENABLED" :data-key :late_submission_deduction_enabled :type boolean
     :doc "Enable late submission penalty")
    (:org-prop "LATE_SUBMISSION_INTERVAL" :data-key :late_submission_interval :type enum
     :values ,org-canvas--valid-late-intervals
     :doc "Deduction frequency")
    (:org-prop "LATE_SUBMISSION_MINIMUM_PERCENT" :data-key :late_submission_minimum_percent :type number
     :doc "Minimum score for late work")
    (:org-prop "LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED" :data-key :late_submission_minimum_percent_enabled :type boolean
     :doc "Enable minimum percent floor")
    (:org-prop "MISSING_SUBMISSION_DEDUCTION" :data-key :missing_submission_deduction :type number
     :doc "Deduction for missing work")
    (:org-prop "MISSING_SUBMISSION_DEDUCTION_ENABLED" :data-key :missing_submission_deduction_enabled :type boolean
     :doc "Enable missing work penalty")))

;;;; 1. Parse

(defconst org-canvas--settings-field-specs
  `(("TIME_ZONE" :time-zone "time_zone" string)
    ("APPLY_WEIGHTS" :apply-weights "apply_assignment_group_weights" boolean)
    ("HIDE_FINAL_GRADES" :hide-final-grades "hide_final_grades" boolean)
    ("PUBLIC_SYLLABUS" :public-syllabus "public_syllabus" boolean)
    ("IS_PUBLIC" :is-public "is_public" boolean)
    ("DEFAULT_VIEW" :default-view-raw "default_view" enum ,org-canvas--valid-views)
    ("LICENSE" :license-raw "license" enum ,org-canvas--valid-licenses)
    ("START_AT" :start-at-raw "start_at" timestamp)
    ("END_AT" :end-at-raw "end_at" timestamp)
    ("ALLOW_STUDENT_DISCUSSION_TOPICS" :allow-student-discussion-topics "allow_student_discussion_topics" boolean)
    ("ALLOW_STUDENT_DISCUSSION_EDITING" :allow-student-discussion-editing "allow_student_discussion_editing" boolean)
    ("ALLOW_STUDENT_FORUM_ATTACHMENTS" :allow-student-forum-attachments "allow_student_forum_attachments" boolean)
    ("LOCK_ALL_ANNOUNCEMENTS" :lock-all-announcements "lock_all_announcements" boolean)
    ("RESTRICT_STUDENT_FUTURE_VIEW" :restrict-student-future-view "restrict_student_future_view" boolean)
    ("RESTRICT_STUDENT_PAST_VIEW" :restrict-student-past-view "restrict_student_past_view" boolean)
    ("SHOW_ANNOUNCEMENTS_ON_HOME_PAGE" :show-announcements-on-home-page "show_announcements_on_home_page" boolean)
    ("HIDE_DISTRIBUTION_GRAPHS" :hide-distribution-graphs "hide_distribution_graphs" boolean)
    ("HOME_PAGE_ANNOUNCEMENT_LIMIT" :home-page-announcement-limit "home_page_announcement_limit" number)
    ("GRADING_STANDARD_ID" :grading-standard-id "grading_standard_id" number))
  "One line per course property the REST settings payload carries.
Each entry is (ORG-PROP PLIST-KEY API-KEY TYPE VALUES): ORG-PROP and
API-KEY are strings, the Org property and the Canvas field; PLIST-KEY
is the raw plist key the parse reads, whose `-raw' suffix \(present
for an enum or timestamp) is what the transform drops; TYPE is
boolean, enum, timestamp, number or string, and drives the transform,
the payload and the pull alike; VALUES is the enum's allowed values,
enums only.  Adding a setting is one line here plus its registry
entry — the property registry feeds validate, diff and the manual,
this list the pipeline.  POST_POLICY and COURSE_IMAGE stay
hand-written in each pass: a GraphQL mutation, a file upload.  The
late policy fields live in `org-canvas--late-policy-field-specs' with
their own payload.")

(defconst org-canvas--late-policy-field-specs
  `(("LATE_SUBMISSION_DEDUCTION" :late-submission-deduction "late_submission_deduction" number)
    ("LATE_SUBMISSION_DEDUCTION_ENABLED" :late-submission-deduction-enabled "late_submission_deduction_enabled" boolean)
    ("LATE_SUBMISSION_INTERVAL" :late-submission-interval-raw "late_submission_interval" enum ,org-canvas--valid-late-intervals)
    ("LATE_SUBMISSION_MINIMUM_PERCENT" :late-submission-minimum-percent "late_submission_minimum_percent" number)
    ("LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED" :late-submission-minimum-percent-enabled "late_submission_minimum_percent_enabled" boolean)
    ("MISSING_SUBMISSION_DEDUCTION" :missing-submission-deduction "missing_submission_deduction" number)
    ("MISSING_SUBMISSION_DEDUCTION_ENABLED" :missing-submission-deduction-enabled "missing_submission_deduction_enabled" boolean))
  "The late policy's fields, in the shape `org-canvas--settings-field-specs' names.
Read and transformed with the course settings, but pushed through the
late-policy payload of `org-canvas--settings-build-late-policy-payload'
and pulled from the nested late-policy reply, so the course payload
leaves them out.  A `number' is Canvas's side of the property: a number
on Canvas, a string in the Org heading.")

(defun org-canvas--settings-read-props (pom)
  "Read raw property strings from the Org buffer at POM.
Returns a plist of raw string values keyed by their property names.
No transformations are applied; all values are raw `org-entry-get' results."
  (append
   (list :title-raw (org-canvas--strip-statistics-cookie
                     (org-get-heading t t t t))
         :post-policy-raw (org-entry-get pom "POST_POLICY")
         ;; Course image (file link or URL)
         :course-image-raw (org-entry-get pom "COURSE_IMAGE"))
   (cl-loop for spec in (append org-canvas--settings-field-specs
                                 org-canvas--late-policy-field-specs)
            append (list (nth 1 spec) (org-entry-get pom (nth 0 spec))))))

(defun org-canvas--settings-transform-key (plist-key)
  "Return the transformed plist key for the raw PLIST-KEY.
Drops the `-raw' suffix an enum or timestamp carries; everything else
passes through as its own key."
  (intern (replace-regexp-in-string "-raw\\'" "" (symbol-name plist-key))))

(defun org-canvas--settings-transform-field (spec raw)
  "Return (KEY VALUE) transforming the raw field SPEC from RAW."
  (let ((value (plist-get raw (nth 1 spec))))
    (list (org-canvas--settings-transform-key (nth 1 spec))
          (pcase (nth 3 spec)
            ('enum (org-canvas--validate-property value (nth 4 spec) (nth 0 spec)))
            ('timestamp (org-canvas-org-parse-timestamp value))
            (_ value)))))

(defun org-canvas--settings-transform-props (raw)
  "Apply pure transformations to RAW property plist.
Validates enums, parses timestamps, and detects course image type.
Returns a plist with transformed keys (no `-raw' suffixes)."
  (let ((course-image-raw (plist-get raw :course-image-raw)))
    (append
     (list :title (plist-get raw :title-raw)
           :post-policy (org-canvas--post-policy-from-property
                         (plist-get raw :post-policy-raw) "POST_POLICY")
           ;; Course image: extract file path or detect URL
           :course-image-file-path (when (and course-image-raw
                                              (string-match "\\[\\[file:\\([^]]+\\)\\]" course-image-raw))
                                     (match-string 1 course-image-raw))
           :course-image-url (when (and course-image-raw
                                        (not (string-prefix-p "[[" course-image-raw))
                                        (string-match-p "^https?://" course-image-raw))
                               course-image-raw))
     (cl-loop for spec in (append org-canvas--settings-field-specs
                                   org-canvas--late-policy-field-specs)
              append (org-canvas--settings-transform-field spec raw)))))

(defun org-canvas--settings-parse-entry ()
  "Parse course settings from the first heading in the current buffer.
Returns a plist with keys :title, :pom, :time-zone, :default-view,
:apply-weights, :hide-final-grades, :public-syllabus, :is-public,
:license, :start-at, :end-at, :syllabus-body, and more.
Delegates to `org-canvas--settings-read-props' for buffer access
and `org-canvas--settings-transform-props' for pure transformations.

The syllabus is the text above the first sub-heading.  The
`** Navigation' child feeds the tab sync and is never body: exported
with the subtree, it reached the syllabus page every student reads
as a numbered \"Navigation\" section under a table of contents
\(issue #275)."
  (org-back-to-heading t)
  (let* ((pom (point-marker))
         (raw (org-canvas--settings-read-props pom))
         (transformed (org-canvas--settings-transform-props raw))
         (syllabus-body (org-canvas--export-subtree-body-to-html nil 'no-children))
         ;; Resolve course image file path relative to buffer
         (course-image-file-path (plist-get transformed :course-image-file-path))
         (course-image-path (when course-image-file-path
                              (expand-file-name
                               course-image-file-path
                               (file-name-directory
                                (buffer-file-name))))))
    (plist-put transformed :pom pom)
    (plist-put transformed :syllabus-body syllabus-body)
    (plist-put transformed :course-image-path course-image-path)
    transformed))

;;;; 2. Build Payload

(defun org-canvas--settings-puthash-field (course data spec)
  "Set SPEC's entry in the COURSE hash from DATA, when DATA carries it.
A boolean converts \"true\"/\"false\" to t/:json-false; a number parses
the string; anything else is stored as the string itself."
  (let* ((key (org-canvas--settings-transform-key (nth 1 spec)))
         (val (plist-get data key)))
    (when val
      (puthash (nth 2 spec)
               (org-canvas--settings-convert-field-value val (nth 0 spec) (nth 3 spec))
               course))))

(defun org-canvas--settings-build-payload (data)
  "Build a Canvas course update payload from parsed DATA plist.
Returns a hash-table suitable for `json-encode'."
  (let ((payload (make-hash-table :test 'equal))
        (course (make-hash-table :test 'equal)))
    (dolist (spec org-canvas--settings-field-specs)
      (org-canvas--settings-puthash-field course data spec))
    (org-canvas--puthash-when course data :title "name")
    ;; Course image: image_id (from file upload) or image_url (plain URL)
    (org-canvas--puthash-when course data :course-image-id "image_id")
    (org-canvas--puthash-when course data :course-image-url "image_url")
    (org-canvas--puthash-when course data :syllabus-body "syllabus_body")
    (puthash "course" course payload)
    payload))

(defun org-canvas--settings-convert-field-value (val org-prop type)
  "Convert the Org string VAL to the Canvas value of ORG-PROP's field.
TYPE is one of: number, boolean, or anything else (the string as-is)."
  (pcase type
    ('number (org-canvas--safe-string-to-number val org-prop))
    ('boolean (if (equal val "true") t :json-false))
    (_ val)))

(defun org-canvas--settings-build-late-policy-payload (data)
  "Build a Canvas late policy payload from parsed DATA plist.
Returns a hash-table wrapped in `late_policy' key, or nil if no
late policy properties are set."
  (let ((has-any (cl-some (lambda (spec)
                            (plist-get data (org-canvas--settings-transform-key (nth 1 spec))))
                          org-canvas--late-policy-field-specs)))
    (when has-any
      (let ((lp (make-hash-table :test 'equal))
            (payload (make-hash-table :test 'equal)))
        (dolist (spec org-canvas--late-policy-field-specs)
          (let ((val (plist-get data (org-canvas--settings-transform-key (nth 1 spec)))))
            (when val
              (puthash (nth 2 spec)
                       (org-canvas--settings-convert-field-value val (nth 0 spec) (nth 3 spec))
                       lp))))
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
      (org-canvas--log-info org-canvas--logger
        "[DRY-RUN] Would update course settings for '%s'" title)
      (cl-return-from org-canvas--settings-push '((id . "dry-run"))))
    (let ((endpoint (org-canvas-api-course-endpoint "")))
      (org-canvas--log-info org-canvas--logger
        "[Execute] PUT course settings for '%s' to %s" title endpoint)
      (condition-case err
          (let ((response (org-canvas-api-request 'PUT endpoint :data payload)))
            (org-canvas--log-info org-canvas--logger
              "[Execute] Course settings update successful")
            response)
        (error
         (org-canvas--log-error org-canvas--logger
           "[Execute] Course settings update failed: %s"
           (error-message-string err))
         (signal (car err) (cdr err)))))))

(defun org-canvas--settings-create-late-policy (endpoint late-policy-payload)
  "POST LATE-POLICY-PAYLOAD to ENDPOINT to create a new late policy.
A 400 saying \"only one late policy\" means a policy exists but the
PATCH update leg failed to reach it — a contradiction worth naming
explicitly instead of a generic failure (issue #13)."
  (condition-case err
      (progn
        (org-canvas-api-request 'POST endpoint :data late-policy-payload)
        (org-canvas--log-info org-canvas--logger "[Execute] Late policy created via POST"))
    (error
     (if (string-match-p "only one late policy" (error-message-string err))
         (org-canvas--log-error org-canvas--logger
           "[Execute] Late policy sync failed: a policy exists on Canvas but the PATCH update did not reach it (route or transport problem) — %s"
           (error-message-string err))
       (org-canvas--log-error org-canvas--logger "[Execute] Late policy sync failed: %s"
         (error-message-string err))))))

(cl-defun org-canvas--settings-push-late-policy (late-policy-payload)
  "Push LATE-POLICY-PAYLOAD to Canvas late policy endpoint.
Tries PATCH first to update an existing policy; on a 404 (course has
no late policy yet) falls back to POST to create one.

Canvas routes the late-policy update as PATCH only — PUT is not
routed and 404s even when a policy exists, which made the POST
fallback fire and 400 on every re-sync (issue #13).  PATCH is real
now: it goes through the direct curl fallback
`org-canvas--api-curl-patch' since plz cannot send it.

The dry-run check lives here rather than at the call site so it covers
both the PATCH and the POST fallback."
  (when late-policy-payload
    (let ((endpoint (org-canvas-api-course-endpoint "late_policy")))
      (when org-canvas--dry-run
        (org-canvas--log-info org-canvas--logger
          "[DRY-RUN] Would update the late policy")
        (cl-return-from org-canvas--settings-push-late-policy nil))
      (org-canvas--log-info org-canvas--logger "[Execute] Syncing late policy...")
      (condition-case err
          (progn
            (org-canvas-api-request 'PATCH endpoint :data late-policy-payload)
            (org-canvas--log-info org-canvas--logger "[Execute] Late policy updated via PATCH"))
        (error
         (if (org-canvas--404-error-p err)
             (progn
               (org-canvas--log-debug org-canvas--logger
                 "[Execute] No existing late policy (404), creating via POST...")
               (org-canvas--settings-create-late-policy endpoint late-policy-payload))
           (org-canvas--log-error org-canvas--logger "[Execute] Late policy sync failed: %s"
             (error-message-string err))))))))

(defconst org-canvas--settings-post-policy-mutation
  "mutation ($courseId: ID!, $manual: Boolean!) { setCoursePostPolicy(input: {courseId: $courseId, postManually: $manual}) { postPolicy { postManually } } }"
  "The GraphQL mutation that sets the course's grade post policy.
Checked against the Canvas schema by the GraphQL contract test
\(issue #269), which names it by this symbol.")

(defun org-canvas--settings-push-post-policy (data)
  "Set the course grade post policy from DATA's :post-policy, when given.
REST only reads the policy, so this is the `setCoursePostPolicy'
GraphQL mutation (issue #202); the cached course policy is forgotten
afterwards so the assignments pull and the drift report re-read it.
A dry run reports and sends nothing.  A sent mutation is followed by
a warning naming the chore it creates: Canvas rewrites the policy of
every assignment, bumping each one's `updated_at', and the next drift
report lists them all as CHANGED with no compared property differing
until `org-canvas-diff-adopt-stamps' restamps them (issue #257)."
  (let ((policy (plist-get data :post-policy)))
    (when policy
      (let ((reply (org-canvas--graphql-mutate
                    (format "set the course post policy to %s" policy)
                    org-canvas--settings-post-policy-mutation
                    ;; A GraphQL Boolean! must arrive as true or false; nil would
                    ;; encode as null, which Canvas rejects (the live probe caught it).
                    (list (cons 'courseId (format "%s" org-canvas-course-id))
                          (cons 'manual (if (equal policy "manual") t :json-false))))))
        (org-canvas--course-post-policy-forget)
        (unless (org-canvas--dry-run-response-p reply)
          (org-canvas--log-warning org-canvas--logger
            "[Execute] The course post policy is now %s; Canvas rewrites every assignment's policy with it, so the next drift report lists each assignment as CHANGED with no compared property differing — run org-canvas-diff-adopt-stamps to restamp them"
            policy))))))

;;;; 4. Finalize

(defun org-canvas--settings-finalize (_data _response)
  "Finalize the settings push.
Per-entry LAST_SYNCED is no longer written; the file-level header
is updated only by `org-canvas-pull-settings' (pulls populate the
canonical `LAST_SYNCED' for conflict detection)."
  (org-canvas--log-info org-canvas--logger
    "[Finalize] Settings push complete"))

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

(cl-defun org-canvas--settings-sync-single-tab (desired current-tabs)
  "Sync a single tab DESIRED against CURRENT-TABS from Canvas.
DESIRED is a (:label :hidden :position) plist.
Returns t if the tab was updated, nil otherwise."
  (let* ((label (plist-get desired :label))
         (label-down (downcase label))
         (hidden (plist-get desired :hidden))
         (position (plist-get desired :position)))
    ;; Guard: skip immutable tabs
    (when (member label-down org-canvas--settings-immutable-tabs)
      (when hidden
        (org-canvas--log-warning org-canvas--logger
          "[Tabs] Cannot hide '%s' — Canvas does not allow it" label))
      (cl-return-from org-canvas--settings-sync-single-tab nil))
    ;; Find matching tab by label
    (let ((tab (cl-find-if
                (lambda (t-item)
                  (string= (downcase (alist-get 'label t-item)) label-down))
                current-tabs)))
      (unless tab
        (org-canvas--log-warning org-canvas--logger "[Tabs] Tab '%s' not found on Canvas" label)
        (cl-return-from org-canvas--settings-sync-single-tab nil))
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
                  (org-canvas--log-info org-canvas--logger
                    "[Tabs] Updated '%s': hidden=%s position=%d"
                    label (if hidden "yes" "no") position)
                  t)
              (error
               (org-canvas--log-warning org-canvas--logger
                 "[Tabs] Failed to update '%s': %s"
                 label (error-message-string err))
               nil))))))))

(cl-defun org-canvas--settings-sync-tabs (navigation)
  "Sync NAVIGATION tab state to Canvas.
NAVIGATION is a list of (:label :hidden :position) plists.
Fetches current tabs, diffs against desired state, and PUTs changes."
  (unless navigation
    (cl-return-from org-canvas--settings-sync-tabs nil))

  (when org-canvas--dry-run
    (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would sync %d navigation tabs" (length navigation))
    (dolist (tab navigation)
      (org-canvas--log-info org-canvas--logger "[DRY-RUN]   %s: position=%d hidden=%s"
                 (plist-get tab :label) (plist-get tab :position)
                 (if (plist-get tab :hidden) "yes" "no")))
    (cl-return-from org-canvas--settings-sync-tabs nil))

  (let* ((url (org-canvas-api-course-endpoint "tabs"))
         (current-tabs (org-canvas-api-request 'GET url))
         (changes 0))
    (dolist (desired navigation)
      (when (org-canvas--settings-sync-single-tab desired current-tabs)
        (setq changes (1+ changes))))
    (org-canvas--log-info org-canvas--logger "[Tabs] %d tab(s) updated" changes)))

(defun org-canvas--settings-pull-tabs ()
  "Pull navigation tabs from Canvas and write as ** Navigation sub-heading.
Returns the formatted Org text, or nil if no tabs."
  (let* ((url (org-canvas-api-course-endpoint "tabs"))
         ;; `json-read' decodes a JSON array as a vector, so Canvas's 46
         ;; tabs arrive as a vector of alists.  `sort' takes a vector and
         ;; returns one, so the type survived all the way to `dolist',
         ;; which signals `listp' — and an empty vector is non-nil, so the
         ;; guard below needs a list too (issue #153).  `append' with a nil
         ;; tail copies either shape into a fresh list, which also keeps
         ;; the destructive `sort' off anything a caller holds.
         (tabs (append (org-canvas-api-request 'GET url) nil)))
    (when tabs
      ;; Sort by position
      (setq tabs (sort tabs
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

(defun org-canvas--settings-course-image-basename (url)
  "Return the file basename from URL, dropping query string and fragment."
  (when url
    (let ((path (car (split-string url "[?#]"))))
      (file-name-nondirectory path))))

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
              (org-canvas--log-info org-canvas--logger "[Image] Uploading course image: %s"
                         (file-name-nondirectory image-path))
              (condition-case err
                  (let* ((file-obj (org-canvas--upload-file image-path))
                         (file-id (alist-get 'id file-obj)))
                    (org-canvas--log-info org-canvas--logger "[Image] Upload complete, file ID: %s" file-id)
                    (plist-put data :course-image-id file-id))
                (error
                 (org-canvas--log-warning org-canvas--logger "[Image] Upload failed: %s"
                               (error-message-string err))
                 data)))
          (progn
            (org-canvas--log-warning org-canvas--logger "[Image] File not found: %s" image-path)
            data))
      (when (and image-path org-canvas--dry-run)
        (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would upload course image: %s"
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
      (org-canvas--signal 'org-canvas-config-error
        "Settings file not found: %s" settings-file))
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> STARTING SETTINGS SYNC")
    (org-canvas--log-info org-canvas--logger "File: %s" settings-file)
    (org-canvas--log-info org-canvas--logger "========================================")
    (with-current-buffer (org-canvas--find-file-noselect settings-file)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^\\*+ " nil t)
          (org-canvas--signal 'org-canvas-validation-error
            "No heading found in settings file"))
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
              (org-canvas--settings-push-post-policy data)
              (org-canvas--settings-sync-tabs navigation)
              (org-canvas--settings-finalize data response)
              (org-canvas--save-buffer)
              (org-canvas--log-info org-canvas--logger "========================================")
              (org-canvas--log-info org-canvas--logger ">>> SETTINGS SYNC COMPLETE")
              (org-canvas--log-info org-canvas--logger "========================================")
              ;; A dry run issues only a GET; counting it as a success made
              ;; the preview table claim work that did not happen (issue #66).
              (org-canvas--sync-record-feature-stats "Settings"
                                                     (if org-canvas--dry-run
                                                         '(:dry-run 1)
                                                       '(:success 1)))
              (message "Settings sync complete."))
          (error
           (org-canvas--log-error org-canvas--logger "[FAILED] Settings sync: %s"
             (error-message-string err))
           (org-canvas--sync-record-feature-stats "Settings"
                                                  '(:fail 1 :failed-titles ("Settings")))
           (org-canvas--user-message "Settings sync FAILED: %s" (error-message-string err))))))))

(defun org-canvas--settings-replace-syllabus-body (syllabus-body)
  "Replace the syllabus under the current heading with SYLLABUS-BODY.
Point must be at the heading.  The syllabus is the text above the
first sub-heading; the sub-headings stay.  Replacing to the end of
the subtree took `** Navigation' with it, and when the tab read then
failed nothing put it back, so one 504 on the tabs endpoint threw the
hand-ordered list away (issue #277)."
  (let ((body-start (save-excursion
                      (org-end-of-meta-data t)
                      (point)))
        (body-end (save-excursion
                    (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
                      (outline-next-heading)
                      (min (point) subtree-end)))))
    (delete-region body-start body-end)
    (goto-char body-start)
    ;; One blank line on each side: `org-end-of-meta-data' has skipped
    ;; the blank lines the file already had, and a sub-heading that
    ;; follows keeps the one it had.
    (unless (looking-back "\n\n" (max (point-min) (- (point) 2)))
      (insert "\n"))
    (insert syllabus-body "\n")
    (when (looking-at-p org-outline-regexp-bol)
      (insert "\n"))))

(defun org-canvas--settings-pull-single-field (pom spec response)
  "Write the Org property of SPEC at POM from the course RESPONSE.
The reply's field is read through `org-canvas--alist-get-non-null':
a JSON null writes nothing, never a property holding the `:null'
keyword.  A boolean reaches `org-canvas--pull-set-boolean-property'
whatever Canvas said, a number is formatted, anything else is written
as it came."
  (let ((org-prop (nth 0 spec))
        (val (org-canvas--alist-get-non-null (intern (nth 2 spec)) response)))
    (pcase (nth 3 spec)
      ('boolean (org-canvas--pull-set-boolean-property pom org-prop val "settings"))
      ('timestamp (org-canvas--pull-set-timestamp-property pom org-prop val))
      ('number (when val (org-canvas-org-set-property pom org-prop (format "%s" val))))
      (_ (when val (org-canvas-org-set-property pom org-prop val))))))

(defun org-canvas--settings-pull-late-policy-properties (pom late-policy)
  "Set late policy properties at POM from LATE-POLICY API response."
  (when late-policy
    (let ((lp (alist-get 'late_policy late-policy)))
      (when lp
        (dolist (spec org-canvas--late-policy-field-specs)
          (org-canvas--settings-pull-single-field pom spec lp))))))

(defun org-canvas--settings-pull-set-properties (pom response syllabus-body
                                                     &optional late-policy)
  "Set all settings properties at POM from API RESPONSE.
SYLLABUS-BODY is the pre-extracted syllabus HTML (may be nil).
LATE-POLICY is the late policy API response (may be nil)."
  (dolist (spec org-canvas--settings-field-specs)
    (org-canvas--settings-pull-single-field pom spec response))
  ;; The post policy rides GraphQL: REST only answers post_manually.
  (let ((policy (org-canvas--post-manually-to-policy
                 (alist-get 'post_manually response))))
    (when policy
      (org-canvas-org-set-property pom "POST_POLICY" policy)))
  (org-canvas--settings-pull-late-policy-properties pom late-policy)
  ;; Course image: download into content/course_image/ and store relpath link
  (let ((image-url (org-canvas--alist-get-non-null 'image_download_url response)))
    (when image-url
      (let* ((basename (org-canvas--settings-course-image-basename image-url))
             (rel-path (concat "content/course_image/" basename))
             (abs-path (expand-file-name rel-path org-canvas-directory)))
        (org-canvas--file-pull-download
         basename image-url abs-path
         (org-canvas--alist-get-non-null 'image_size response))
        (org-canvas-org-set-property
         pom "COURSE_IMAGE"
         (format "[[file:%s][%s]]" rel-path basename)))))
  (when syllabus-body
    (org-canvas--settings-replace-syllabus-body syllabus-body)))

(defun org-canvas--settings-insert-navigation-heading (nav-text)
  "Remove existing ** Navigation heading and insert NAV-TEXT."
  (save-excursion
    ;; Remove existing ** Navigation if present
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\* Navigation" nil t)
      (beginning-of-line)
      (let ((start (point))
            (end (save-excursion
                   (forward-line 1)
                   (if (re-search-forward "^\\*\\* " nil t)
                       (match-beginning 0)
                     (point-max)))))
        (delete-region start end)))
    ;; Insert at end of course heading subtree
    (goto-char (point-min))
    (re-search-forward "^\\*+ " nil t)
    (org-back-to-heading t)
    (org-end-of-subtree t)
    (insert "\n" nav-text)))

(defun org-canvas--settings-pull-optional-record (what err)
  "Log, echo and record in the pull summary that WHAT failed with ERR.
A role refusal (`org-canvas-permission-error') is a skip, as
`org-canvas--safe-pull' counts one for a whole type (issue #155) and
`org-canvas--rewrite-record-failure' for a body file link (issue
#390): a Designer enrolment 403s on the late policy, which is a gap
to accept, not something that broke (issue #397).  Anything else is
an error."
  (let ((skip (memq 'org-canvas-permission-error
                    (get (car err) 'error-conditions)))
        (msg (error-message-string err)))
    (org-canvas--log-warning org-canvas--logger
      "[Pull] Settings: %s not pulled (%s); settings.org keeps what it had"
      what msg)
    (org-canvas--user-message "Settings: %s not pulled (%s)" what msg)
    (org-canvas--pull-summary-record
     :kind (if skip 'skip 'error)
     :file (file-name-nondirectory org-canvas-settings-file)
     :item what
     :error (if skip msg (format "not pulled: %s" msg))
     :log-line (org-canvas--pull-summary-current-log-line))))

(defun org-canvas--settings-pull-optional (what fetch-fn &optional absent-on-404)
  "Call FETCH-FN for WHAT, returning nil and saying so when it fails.
WHAT names an optional piece of the settings pull (\"late policy\",
\"navigation tabs\").  The failure used to be swallowed, so a 403 on
the late-policy endpoint read as a course with no late policy — a
skip that did not say so (issues #81, #142).  Any failure is now a
warning in the log, an echo-area line, and a record in the pull
summary — a skip for a role refusal, an error otherwise
\(`org-canvas--settings-pull-optional-record') — except the one that
is an answer rather than a failure: ABSENT-ON-404 non-nil says a 404
means the course has no WHAT, which is noted at INFO and nothing
more."
  (condition-case err
      (funcall fetch-fn)
    (error
     (if (and absent-on-404 (org-canvas--404-error-p err))
         (org-canvas--log-info org-canvas--logger
           "[Pull] The course has no %s (404)" what)
       (org-canvas--settings-pull-optional-record what err))
     nil)))

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
                              ("include[]" . "course_image")
                              ("include[]" . "post_manually"))))
         (name (alist-get 'name response))
         (syllabus-body (org-canvas--alist-get-non-null 'syllabus_body response))
         (settings-file (expand-file-name org-canvas-settings-file))
         (was-fresh (org-canvas--pull-was-fresh-p settings-file)))
    ;; Fetch late policy (separate endpoint)
    (let ((late-policy (org-canvas--settings-pull-optional
                        "late policy"
                        (lambda ()
                          (org-canvas-api-request
                           'GET (org-canvas-api-course-endpoint "late_policy")))
                        'absent-on-404)))
      (org-canvas--pull-confirm-overwrite settings-file "settings")
      (org-canvas--pull-confirm-unsaved settings-file "settings")
      ;; Open or create the settings file
      (unless (file-exists-p settings-file)
        (with-temp-file settings-file
          (insert (format "#+TITLE: Settings\n* %s\n" (or name "Course")))))
      (with-current-buffer (org-canvas--find-file-noselect settings-file)
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
        (let ((nav-text (org-canvas--settings-pull-optional
                         "navigation tabs" #'org-canvas--settings-pull-tabs)))
          (when nav-text
            (org-canvas--settings-insert-navigation-heading nav-text)))
        (org-canvas--pull-write-file-header)
        (org-canvas--save-buffer))
      (org-canvas--pull-kill-fresh-buffer settings-file was-fresh)
      ;; Refresh the course TZ cache now that settings.org reflects
      ;; the just-pulled :TIME_ZONE: so subsequent pulls localize
      ;; their timestamps correctly.
      (org-canvas--pull-resolve-tz))
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> SETTINGS PULL COMPLETE")
    (org-canvas--log-info org-canvas--logger "Course: %s" name)
    (org-canvas--log-info org-canvas--logger "========================================")
    (message "Settings pull complete: %s" name)))

(provide 'org-canvas-settings)
;;; org-canvas-settings.el ends here
