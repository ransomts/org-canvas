;;; org-canvas-settings-test.el --- Tests for org-canvas-settings  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-settings)

;;;; Parse

(describe "org-canvas--settings-parse-entry"
  (it "extracts all properties from heading"
    (with-temp-org-buffer
     "* Data Science 101
:PROPERTIES:
:TIME_ZONE: America/New_York
:DEFAULT_VIEW: modules
:APPLY_WEIGHTS: true
:HIDE_FINAL_GRADES: false
:PUBLIC_SYLLABUS: true
:IS_PUBLIC: false
:LICENSE: private
:END:

Syllabus text here.
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :title) :to-equal "Data Science 101")
       (expect (plist-get data :time-zone) :to-equal "America/New_York")
       (expect (plist-get data :default-view) :to-equal "modules")
       (expect (plist-get data :apply-weights) :to-equal "true")
       (expect (plist-get data :hide-final-grades) :to-equal "false")
       (expect (plist-get data :public-syllabus) :to-equal "true")
       (expect (plist-get data :is-public) :to-equal "false")
       (expect (plist-get data :license) :to-equal "private")
       (expect (plist-get data :pom) :to-be-truthy))))

  (it "handles missing optional properties"
    (with-temp-org-buffer
     "* My Course
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :title) :to-equal "My Course")
       (expect (plist-get data :time-zone) :to-be nil)
       (expect (plist-get data :default-view) :to-be nil))))

  (it "validates DEFAULT_VIEW against allowed values"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:DEFAULT_VIEW: invalid_view
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       ;; Falls back to first allowed value when invalid
       (expect (plist-get data :default-view) :to-equal "feed"))))

  (it "validates LICENSE against allowed values"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:LICENSE: invalid_license
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :license) :to-equal "private"))))

  (it "extracts new boolean settings properties"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:ALLOW_STUDENT_DISCUSSION_TOPICS: true
:ALLOW_STUDENT_DISCUSSION_EDITING: false
:ALLOW_STUDENT_FORUM_ATTACHMENTS: true
:LOCK_ALL_ANNOUNCEMENTS: true
:RESTRICT_STUDENT_FUTURE_VIEW: false
:RESTRICT_STUDENT_PAST_VIEW: true
:SHOW_ANNOUNCEMENTS_ON_HOME_PAGE: true
:HIDE_DISTRIBUTION_GRAPHS: false
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :allow-student-discussion-topics) :to-equal "true")
       (expect (plist-get data :allow-student-discussion-editing) :to-equal "false")
       (expect (plist-get data :allow-student-forum-attachments) :to-equal "true")
       (expect (plist-get data :lock-all-announcements) :to-equal "true")
       (expect (plist-get data :restrict-student-future-view) :to-equal "false")
       (expect (plist-get data :restrict-student-past-view) :to-equal "true")
       (expect (plist-get data :show-announcements-on-home-page) :to-equal "true")
       (expect (plist-get data :hide-distribution-graphs) :to-equal "false"))))

  (it "extracts HOME_PAGE_ANNOUNCEMENT_LIMIT as string"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:HOME_PAGE_ANNOUNCEMENT_LIMIT: 3
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :home-page-announcement-limit) :to-equal "3"))))

  (it "returns nil for missing new settings properties"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :allow-student-discussion-topics) :to-be nil)
       (expect (plist-get data :lock-all-announcements) :to-be nil)
       (expect (plist-get data :home-page-announcement-limit) :to-be nil)
       (expect (plist-get data :hide-distribution-graphs) :to-be nil))))

  (it "exports body as HTML"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Welcome to the course.
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :syllabus-body) :to-match "Welcome")))))

;;;; Build Payload

(describe "org-canvas--settings-build-payload"
  (it "builds hash-table with course key"
    (let* ((data '(:title "My Course"
                   :time-zone "UTC"
                   :default-view "modules"
                   :apply-weights "true"
                   :hide-final-grades "false"
                   :license "private"
                   :syllabus-body "<p>Hello</p>"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "name" course) :to-equal "My Course")
      (expect (gethash "time_zone" course) :to-equal "UTC")
      (expect (gethash "default_view" course) :to-equal "modules")
      (expect (gethash "apply_assignment_group_weights" course) :to-be t)
      (expect (gethash "hide_final_grades" course) :to-equal :json-false)
      (expect (gethash "license" course) :to-equal "private")
      (expect (gethash "syllabus_body" course) :to-equal "<p>Hello</p>")))

  (it "omits nil properties"
    (let* ((data '(:title "Course" :time-zone nil :default-view nil))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "name" course) :to-equal "Course")
      (expect (gethash "time_zone" course nil) :to-be nil)
      (expect (gethash "default_view" course nil) :to-be nil)))

  (it "handles boolean conversion for public settings"
    (let* ((data '(:title "Course"
                   :public-syllabus "true"
                   :is-public "false"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "public_syllabus" course) :to-be t)
      (expect (gethash "is_public" course) :to-equal :json-false)))

  (it "converts new boolean settings to t/:json-false"
    (let* ((data '(:title "Course"
                   :allow-student-discussion-topics "true"
                   :allow-student-discussion-editing "false"
                   :allow-student-forum-attachments "true"
                   :lock-all-announcements "true"
                   :restrict-student-future-view "false"
                   :restrict-student-past-view "true"
                   :show-announcements-on-home-page "true"
                   :hide-distribution-graphs "false"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "allow_student_discussion_topics" course) :to-be t)
      (expect (gethash "allow_student_discussion_editing" course) :to-equal :json-false)
      (expect (gethash "allow_student_forum_attachments" course) :to-be t)
      (expect (gethash "lock_all_announcements" course) :to-be t)
      (expect (gethash "restrict_student_future_view" course) :to-equal :json-false)
      (expect (gethash "restrict_student_past_view" course) :to-be t)
      (expect (gethash "show_announcements_on_home_page" course) :to-be t)
      (expect (gethash "hide_distribution_graphs" course) :to-equal :json-false)))

  (it "converts HOME_PAGE_ANNOUNCEMENT_LIMIT to number"
    (let* ((data '(:title "Course"
                   :home-page-announcement-limit "3"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "home_page_announcement_limit" course) :to-equal 3)))

  (it "omits new settings when nil"
    (let* ((data '(:title "Course"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "allow_student_discussion_topics" course nil) :to-be nil)
      (expect (gethash "lock_all_announcements" course nil) :to-be nil)
      (expect (gethash "home_page_announcement_limit" course nil) :to-be nil)
      (expect (gethash "hide_distribution_graphs" course nil) :to-be nil))))

;;;; Push

(describe "org-canvas--settings-push"
  (it "calls PUT on the course endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Course")))
          (org-canvas--settings-push data '((course . ((name . "Course")))))
          (expect-api-called 'PUT "courses/99999/$")))))

  (it "returns dry-run response in dry-run mode"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t)
            (api-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _args) (setq api-called t))))
          (let ((result (org-canvas--settings-push '(:title "X") '())))
            (expect api-called :to-be nil)
            (expect (alist-get 'id result) :to-equal "dry-run")))))))

;;;; Finalize

(describe "org-canvas--settings-finalize"
  (it "saves LAST_SYNCED"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :pom (point-marker))))
       (org-canvas--settings-finalize data nil)
       (expect-synced-timestamp (point))))))

;;;; Sync Integration

(describe "org-canvas-sync-settings"
  (it "syncs settings from file"
    (let* ((temp-dir (make-temp-file "settings-test" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "* Test Course\n:PROPERTIES:\n:TIME_ZONE: UTC\n:END:\n\nHello.\n"))
            (let ((org-canvas-settings-file settings-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             '((id . 1234) (name . "Test Course"))))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil)))
                  (org-canvas-sync-settings)
                  (with-current-buffer (find-file-noselect settings-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect-synced-timestamp (point)))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "signals error for missing file"
    (let ((org-canvas-settings-file "/tmp/nonexistent/settings.org"))
      (cl-letf (((symbol-function 'org-canvas-clear-log) (lambda () nil))
                ((symbol-function 'display-buffer) (lambda (_) nil)))
        (expect (org-canvas-sync-settings) :to-throw 'error)))))

;;;; Pull

(describe "org-canvas-pull-settings"
  (it "creates file and populates properties"
    (let* ((temp-dir (make-temp-file "pull-settings" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-settings-file settings-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           '((name . "Data Science")
                             (time_zone . "America/New_York")
                             (default_view . "modules")
                             (apply_assignment_group_weights . t)
                             (hide_final_grades . :json-false)
                             (public_syllabus . t)
                             (is_public . :json-false)
                             (license . "private")
                             (start_at . "2026-01-15T10:00:00Z")
                             (end_at . "2026-05-15T10:00:00Z")
                             (syllabus_body . "<p>Welcome</p>"))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-settings)
                ;; Verify the saved file by reading it into a temp org buffer
                (let ((content (with-temp-buffer
                                 (insert-file-contents settings-file)
                                 (buffer-string))))
                  (with-temp-org-buffer
                   content
                   (re-search-forward "^\\*+ " nil t)
                   (org-back-to-heading)
                   (expect (org-entry-get (point) "TIME_ZONE")
                           :to-equal "America/New_York")
                   (expect (org-entry-get (point) "DEFAULT_VIEW")
                           :to-equal "modules")
                   (expect (org-entry-get (point) "APPLY_WEIGHTS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "HIDE_FINAL_GRADES")
                           :to-equal "false")
                   (expect (org-entry-get (point) "PUBLIC_SYLLABUS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "IS_PUBLIC")
                           :to-equal "false")
                   (expect (org-entry-get (point) "LICENSE")
                           :to-equal "private")
                   (expect (org-entry-get (point) "START_AT")
                           :to-match "2026-01-15")
                   (expect (org-entry-get (point) "END_AT")
                           :to-match "2026-05-15")
                   (expect-synced-timestamp (point))
                   (expect (buffer-string) :to-match "Welcome"))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "sets new boolean and number settings properties on pull"
    (let* ((temp-dir (make-temp-file "pull-settings" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-settings-file settings-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           '((name . "Course")
                             (time_zone . "UTC")
                             (default_view . "modules")
                             (apply_assignment_group_weights . t)
                             (hide_final_grades . :json-false)
                             (public_syllabus . :json-false)
                             (is_public . :json-false)
                             (allow_student_discussion_topics . t)
                             (allow_student_discussion_editing . :json-false)
                             (allow_student_forum_attachments . t)
                             (lock_all_announcements . t)
                             (restrict_student_future_view . :json-false)
                             (restrict_student_past_view . t)
                             (show_announcements_on_home_page . t)
                             (home_page_announcement_limit . 5)
                             (hide_distribution_graphs . :json-false))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-settings)
                (let ((content (with-temp-buffer
                                 (insert-file-contents settings-file)
                                 (buffer-string))))
                  (with-temp-org-buffer
                   content
                   (re-search-forward "^\\*+ " nil t)
                   (org-back-to-heading)
                   (expect (org-entry-get (point) "ALLOW_STUDENT_DISCUSSION_TOPICS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "ALLOW_STUDENT_DISCUSSION_EDITING")
                           :to-equal "false")
                   (expect (org-entry-get (point) "ALLOW_STUDENT_FORUM_ATTACHMENTS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "LOCK_ALL_ANNOUNCEMENTS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "RESTRICT_STUDENT_FUTURE_VIEW")
                           :to-equal "false")
                   (expect (org-entry-get (point) "RESTRICT_STUDENT_PAST_VIEW")
                           :to-equal "true")
                   (expect (org-entry-get (point) "SHOW_ANNOUNCEMENTS_ON_HOME_PAGE")
                           :to-equal "true")
                   (expect (org-entry-get (point) "HOME_PAGE_ANNOUNCEMENT_LIMIT")
                           :to-equal "5")
                   (expect (org-entry-get (point) "HIDE_DISTRIBUTION_GRAPHS")
                           :to-equal "false"))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "updates existing file without duplicating headings"
    (let* ((temp-dir (make-temp-file "pull-settings" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "#+TITLE: Settings\n* Old Name\n:PROPERTIES:\n:TIME_ZONE: UTC\n:END:\n"))
            (let ((org-canvas-settings-file settings-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             '((name . "New Name")
                               (time_zone . "America/Chicago")
                               (default_view . "feed")
                               (apply_assignment_group_weights . :json-false)
                               (hide_final_grades . :json-false)
                               (public_syllabus . :json-false)
                               (is_public . :json-false))))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil))
                          ((symbol-function 'y-or-n-p) (lambda (_) t)))
                  (org-canvas-pull-settings)
                  ;; Verify the saved file by reading it into a temp org buffer
                  (let ((content (with-temp-buffer
                                   (insert-file-contents settings-file)
                                   (buffer-string))))
                    (with-temp-org-buffer
                     content
                     ;; Should only have one level-1 heading
                     (let ((count 0))
                       (org-map-entries (lambda () (setq count (1+ count)))
                                        "LEVEL=1" 'file)
                       (expect count :to-equal 1))
                     (re-search-forward "^\\*+ " nil t)
                     (org-back-to-heading)
                     (expect (org-entry-get (point) "TIME_ZONE")
                             :to-equal "America/Chicago")))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "aborts when user declines overwrite of existing file"
    (let* ((temp-dir (make-temp-file "pull-settings" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "#+TITLE: Settings\n* Old Name\n:PROPERTIES:\n:TIME_ZONE: UTC\n:END:\n"))
            (let ((org-canvas-settings-file settings-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             '((name . "New Name"))))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil))
                          ((symbol-function 'y-or-n-p) (lambda (_) nil)))
                  (expect (org-canvas-pull-settings) :to-throw 'user-error)))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Additional Coverage Tests

(describe "org-canvas--settings-push error handling"
  (it "re-signals API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("500 Internal Server Error")))))
        (let ((data '(:title "Course" :pom 1 :canvas-id nil))
              (payload '((course . ((name . "Course"))))))
          (expect (org-canvas--settings-push data payload) :to-throw 'error))))))

(describe "org-canvas-sync-settings error paths"
  (it "errors when no heading found"
    (let* ((temp-dir (make-temp-file "settings-test" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "#+TITLE: Settings\n"))
            (let ((org-canvas-settings-file settings-file))
              (with-sync-test-env
                (expect (org-canvas-sync-settings) :to-throw 'error))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "logs error on sync failure"
    (let* ((temp-dir (make-temp-file "settings-test" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "#+TITLE: Settings\n* Course\n:PROPERTIES:\n:END:\n"))
            (let ((org-canvas-settings-file settings-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas--settings-parse-entry)
                             (lambda () (signal 'error '("Parse error")))))
                    ;; Should not throw (errors are caught and logged)
                    (org-canvas-sync-settings))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-settings heading creation"
  (it "creates heading when file has no headings"
    (let* ((temp-dir (make-temp-file "settings-test" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file settings-file
              (insert "#+TITLE: Settings\n"))
            (let ((org-canvas-settings-file settings-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             '((name . "My Course"))))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil))
                          ((symbol-function 'org-canvas--pull-confirm-overwrite)
                           (lambda (&rest _) nil))
                          ((symbol-function 'org-canvas--settings-pull-set-properties)
                           (lambda (&rest _) nil)))
                  (org-canvas-pull-settings)
                  (with-current-buffer (find-file-noselect settings-file)
                    (expect (buffer-string) :to-match "My Course"))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Late Policy + GRADING_STANDARD_ID Tests

(describe "org-canvas--settings-parse-entry (late policy + grading_standard_id)"
  (it "extracts GRADING_STANDARD_ID"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:GRADING_STANDARD_ID: 42
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :grading-standard-id) :to-equal "42"))))

  (it "extracts late policy properties"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:LATE_SUBMISSION_DEDUCTION: 10
:LATE_SUBMISSION_DEDUCTION_ENABLED: true
:LATE_SUBMISSION_INTERVAL: day
:LATE_SUBMISSION_MINIMUM_PERCENT: 50
:LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED: true
:MISSING_SUBMISSION_DEDUCTION: 100
:MISSING_SUBMISSION_DEDUCTION_ENABLED: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :late-submission-deduction) :to-equal "10")
       (expect (plist-get data :late-submission-deduction-enabled) :to-equal "true")
       (expect (plist-get data :late-submission-interval) :to-equal "day")
       (expect (plist-get data :late-submission-minimum-percent) :to-equal "50")
       (expect (plist-get data :late-submission-minimum-percent-enabled) :to-equal "true")
       (expect (plist-get data :missing-submission-deduction) :to-equal "100")
       (expect (plist-get data :missing-submission-deduction-enabled) :to-equal "true"))))

  (it "validates LATE_SUBMISSION_INTERVAL"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:LATE_SUBMISSION_INTERVAL: invalid
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       ;; Falls back to first valid value
       (expect (plist-get data :late-submission-interval) :to-equal "day"))))

  (it "returns nil for missing late policy properties"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :grading-standard-id) :to-be nil)
       (expect (plist-get data :late-submission-deduction) :to-be nil)
       (expect (plist-get data :missing-submission-deduction) :to-be nil)))))

(describe "org-canvas--settings-build-payload (grading_standard_id)"
  (it "includes grading_standard_id in course"
    (let* ((data '(:title "Course" :grading-standard-id "42"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "grading_standard_id" course) :to-equal 42)))

  (it "omits grading_standard_id when nil"
    (let* ((data '(:title "Course"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "grading_standard_id" course nil) :to-be nil))))

(describe "org-canvas--settings-build-late-policy-payload"
  (it "builds payload with all properties"
    (let* ((data '(:late-submission-deduction "10"
                   :late-submission-deduction-enabled "true"
                   :late-submission-interval "day"
                   :late-submission-minimum-percent "50"
                   :late-submission-minimum-percent-enabled "true"
                   :missing-submission-deduction "100"
                   :missing-submission-deduction-enabled "false"))
           (payload (org-canvas--settings-build-late-policy-payload data))
           (lp (gethash "late_policy" payload)))
      (expect (gethash "late_submission_deduction" lp) :to-equal 10)
      (expect (gethash "late_submission_deduction_enabled" lp) :to-be t)
      (expect (gethash "late_submission_interval" lp) :to-equal "day")
      (expect (gethash "late_submission_minimum_percent" lp) :to-equal 50)
      (expect (gethash "late_submission_minimum_percent_enabled" lp) :to-be t)
      (expect (gethash "missing_submission_deduction" lp) :to-equal 100)
      (expect (gethash "missing_submission_deduction_enabled" lp) :to-equal :json-false)))

  (it "returns nil when no late policy properties"
    (let ((data '(:title "Course")))
      (expect (org-canvas--settings-build-late-policy-payload data) :to-be nil)))

  (it "builds partial payload when some properties set"
    (let* ((data '(:late-submission-deduction "5"))
           (payload (org-canvas--settings-build-late-policy-payload data))
           (lp (gethash "late_policy" payload)))
      (expect (gethash "late_submission_deduction" lp) :to-equal 5)
      (expect (gethash "late_submission_interval" lp nil) :to-be nil))))

(describe "org-canvas--settings-push-late-policy"
  (it "calls PATCH on late_policy endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((payload (make-hash-table :test 'equal))
              (lp (make-hash-table :test 'equal)))
          (puthash "late_submission_deduction" 10 lp)
          (puthash "late_policy" lp payload)
          (org-canvas--settings-push-late-policy payload)
          (expect-api-called 'PATCH "late_policy")))))

  (it "does nothing when payload is nil"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--settings-push-late-policy nil)
        (expect (test-org-canvas-api-called-p 'PATCH "late_policy") :to-be nil)
        (expect (test-org-canvas-api-called-p 'POST "late_policy") :to-be nil))))

  (it "falls back to POST when PATCH fails"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (eq method 'PATCH)
                         (signal 'error '("400 Bad Request"))
                       '((id . 1))))))
          (let ((payload (make-hash-table :test 'equal))
                (lp (make-hash-table :test 'equal)))
            (puthash "late_submission_deduction" 10 lp)
            (puthash "late_policy" lp payload)
            (org-canvas--settings-push-late-policy payload)
            (expect call-count :to-equal 2)))))))

(describe "org-canvas-pull-settings (late policy + grading_standard_id)"
  (it "sets GRADING_STANDARD_ID and late policy properties on pull"
    (let* ((temp-dir (make-temp-file "pull-settings" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-settings-file settings-file)
                (call-count 0))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method url &rest _args)
                           (setq call-count (1+ call-count))
                           (if (string-match "late_policy" url)
                               '((late_policy .
                                  ((late_submission_deduction . 10)
                                   (late_submission_deduction_enabled . t)
                                   (late_submission_interval . "day")
                                   (late_submission_minimum_percent . 50)
                                   (late_submission_minimum_percent_enabled . t)
                                   (missing_submission_deduction . 100)
                                   (missing_submission_deduction_enabled . :json-false))))
                             '((name . "Course")
                               (time_zone . "UTC")
                               (default_view . "modules")
                               (grading_standard_id . 42)
                               (apply_assignment_group_weights . :json-false)
                               (hide_final_grades . :json-false)
                               (public_syllabus . :json-false)
                               (is_public . :json-false)))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-settings)
                (let ((content (with-temp-buffer
                                 (insert-file-contents settings-file)
                                 (buffer-string))))
                  (with-temp-org-buffer
                   content
                   (re-search-forward "^\\*+ " nil t)
                   (org-back-to-heading)
                   (expect (org-entry-get (point) "GRADING_STANDARD_ID")
                           :to-equal "42")
                   (expect (org-entry-get (point) "LATE_SUBMISSION_DEDUCTION")
                           :to-equal "10")
                   (expect (org-entry-get (point) "LATE_SUBMISSION_DEDUCTION_ENABLED")
                           :to-equal "true")
                   (expect (org-entry-get (point) "LATE_SUBMISSION_INTERVAL")
                           :to-equal "day")
                   (expect (org-entry-get (point) "LATE_SUBMISSION_MINIMUM_PERCENT")
                           :to-equal "50")
                   (expect (org-entry-get (point) "LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED")
                           :to-equal "true")
                   (expect (org-entry-get (point) "MISSING_SUBMISSION_DEDUCTION")
                           :to-equal "100")
                   (expect (org-entry-get (point) "MISSING_SUBMISSION_DEDUCTION_ENABLED")
                           :to-equal "false"))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;; org-canvas-settings-test.el ends here
