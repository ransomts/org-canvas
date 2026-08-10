;;; org-canvas-settings-test.el --- Tests for org-canvas-settings  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-settings)

;;;; Transform Props

(describe "org-canvas--settings-transform-props"
  (it "passes title through from :title-raw"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Data Science 101"))))
      (expect (plist-get result :title) :to-equal "Data Science 101")))

  (it "validates DEFAULT_VIEW against allowed values"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course" :default-view-raw "invalid_view"))))
      (expect (plist-get result :default-view) :to-equal "feed")))

  (it "passes valid DEFAULT_VIEW through"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course" :default-view-raw "modules"))))
      (expect (plist-get result :default-view) :to-equal "modules")))

  (it "validates LICENSE against allowed values"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course" :license-raw "bad_license"))))
      (expect (plist-get result :license) :to-equal "private")))

  (it "passes valid LICENSE through"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course" :license-raw "cc_by_sa"))))
      (expect (plist-get result :license) :to-equal "cc_by_sa")))

  (it "parses START_AT timestamp to ISO8601"
    (let* ((tz (getenv "TZ")))
      (unwind-protect
          (progn
            (set-time-zone-rule "UTC")
            (let ((result (org-canvas--settings-transform-props
                           '(:title-raw "Course"
                             :start-at-raw "[2026-01-15 Thu 09:00]"))))
              (expect (plist-get result :start-at)
                      :to-equal "2026-01-15T09:00:00Z")))
        (set-time-zone-rule tz))))

  (it "parses END_AT timestamp to ISO8601"
    (let* ((tz (getenv "TZ")))
      (unwind-protect
          (progn
            (set-time-zone-rule "UTC")
            (let ((result (org-canvas--settings-transform-props
                           '(:title-raw "Course"
                             :end-at-raw "[2026-05-15 Fri 17:00]"))))
              (expect (plist-get result :end-at)
                      :to-equal "2026-05-15T17:00:00Z")))
        (set-time-zone-rule tz))))

  (it "returns nil for missing timestamp properties"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course"))))
      (expect (plist-get result :start-at) :to-be nil)
      (expect (plist-get result :end-at) :to-be nil)))

  (it "passes string properties through unchanged"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course"
                     :time-zone "America/New_York"
                     :apply-weights "true"
                     :hide-final-grades "false"
                     :public-syllabus "true"
                     :is-public "false"
                     :grading-standard-id "42"
                     :home-page-announcement-limit "3"
                     :late-submission-deduction "10"
                     :missing-submission-deduction "50"))))
      (expect (plist-get result :time-zone) :to-equal "America/New_York")
      (expect (plist-get result :apply-weights) :to-equal "true")
      (expect (plist-get result :hide-final-grades) :to-equal "false")
      (expect (plist-get result :public-syllabus) :to-equal "true")
      (expect (plist-get result :is-public) :to-equal "false")
      (expect (plist-get result :grading-standard-id) :to-equal "42")
      (expect (plist-get result :home-page-announcement-limit) :to-equal "3")
      (expect (plist-get result :late-submission-deduction) :to-equal "10")
      (expect (plist-get result :missing-submission-deduction) :to-equal "50")))

  (it "detects course-image URL"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course"
                     :course-image-raw "https://example.com/image.png"))))
      (expect (plist-get result :course-image-url)
              :to-equal "https://example.com/image.png")
      (expect (plist-get result :course-image-file-path) :to-be nil)))

  (it "extracts file path from course-image Org link"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course"
                     :course-image-raw "[[file:images/banner.png]]"))))
      (expect (plist-get result :course-image-file-path)
              :to-equal "images/banner.png")
      (expect (plist-get result :course-image-url) :to-be nil)))

  (it "returns nil for course-image when not set"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course"))))
      (expect (plist-get result :course-image-file-path) :to-be nil)
      (expect (plist-get result :course-image-url) :to-be nil)))

  (it "validates LATE_SUBMISSION_INTERVAL against allowed values"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course"
                     :late-submission-interval-raw "week"))))
      (expect (plist-get result :late-submission-interval) :to-equal "day")))

  (it "passes valid LATE_SUBMISSION_INTERVAL through"
    (let ((result (org-canvas--settings-transform-props
                   '(:title-raw "Course"
                     :late-submission-interval-raw "hour"))))
      (expect (plist-get result :late-submission-interval) :to-equal "hour"))))

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
  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :pom (point-marker))))
       (org-canvas--settings-finalize data nil)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)))))

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
                           :to-be nil)
                   (expect (org-entry-get (point) "PUBLIC_SYLLABUS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "IS_PUBLIC")
                           :to-be nil)
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
                           :to-be nil)
                   (expect (org-entry-get (point) "ALLOW_STUDENT_FORUM_ATTACHMENTS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "LOCK_ALL_ANNOUNCEMENTS")
                           :to-equal "true")
                   (expect (org-entry-get (point) "RESTRICT_STUDENT_FUTURE_VIEW")
                           :to-be nil)
                   (expect (org-entry-get (point) "RESTRICT_STUDENT_PAST_VIEW")
                           :to-equal "true")
                   (expect (org-entry-get (point) "SHOW_ANNOUNCEMENTS_ON_HOME_PAGE")
                           :to-equal "true")
                   (expect (org-entry-get (point) "HOME_PAGE_ANNOUNCEMENT_LIMIT")
                           :to-equal "5")
                   (expect (org-entry-get (point) "HIDE_DISTRIBUTION_GRAPHS")
                           :to-be nil))))))
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
  (it "calls PUT on late_policy endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((payload (make-hash-table :test 'equal))
              (lp (make-hash-table :test 'equal)))
          (puthash "late_submission_deduction" 10 lp)
          (puthash "late_policy" lp payload)
          (org-canvas--settings-push-late-policy payload)
          (expect-api-called 'PUT "late_policy")
          (expect (test-org-canvas-api-called-p 'POST "late_policy") :to-be nil)))))

  (it "does nothing when payload is nil"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--settings-push-late-policy nil)
        (expect (test-org-canvas-api-called-p 'PUT "late_policy") :to-be nil)
        (expect (test-org-canvas-api-called-p 'POST "late_policy") :to-be nil))))

  (it "falls back to POST when PUT returns 404 (no policy yet)"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (eq method 'PUT)
                         (signal 'error '("API Request Failed (HTTP 404)"))
                       '((id . 1))))))
          (let ((payload (make-hash-table :test 'equal))
                (lp (make-hash-table :test 'equal)))
            (puthash "late_submission_deduction" 10 lp)
            (puthash "late_policy" lp payload)
            (org-canvas--settings-push-late-policy payload)
            (expect call-count :to-equal 2))))))

  (it "reports a non-404 PUT failure without attempting POST"
    (with-org-canvas-test-config
      (let ((post-called nil)
            (error-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (if (eq method 'PUT)
                         (signal 'error '("API Request Failed (HTTP 422)"))
                       (setq post-called t))))
                  ((symbol-function 'org-canvas--log-error)
                   (lambda (_logger fmt &rest args)
                     (when (string-match "Late policy sync failed" fmt)
                       (setq error-logged (apply #'format fmt args))))))
          (let ((payload (make-hash-table :test 'equal))
                (lp (make-hash-table :test 'equal)))
            (puthash "late_submission_deduction" 10 lp)
            (puthash "late_policy" lp payload)
            (org-canvas--settings-push-late-policy payload)
            (expect post-called :to-be nil)
            (expect error-logged :to-match "HTTP 422")))))))

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
                           :to-be nil))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Navigation Tab Tests

(describe "org-canvas--settings-parse-navigation"
  (it "parses visible and hidden tabs from numbered list"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

** Navigation
1. Home
2. Modules
3. +Discussions+
4. +People+
"
     (org-back-to-heading)
     (let ((nav (org-canvas--settings-parse-navigation)))
       (expect (length nav) :to-equal 4)
       (expect (plist-get (nth 0 nav) :label) :to-equal "Home")
       (expect (plist-get (nth 0 nav) :hidden) :to-be nil)
       (expect (plist-get (nth 0 nav) :position) :to-equal 1)
       (expect (plist-get (nth 2 nav) :label) :to-equal "Discussions")
       (expect (plist-get (nth 2 nav) :hidden) :to-be t)
       (expect (plist-get (nth 2 nav) :position) :to-equal 3))))

  (it "returns nil when no Navigation heading"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Syllabus text.
"
     (org-back-to-heading)
     (let ((nav (org-canvas--settings-parse-navigation)))
       (expect nav :to-be nil))))

  (it "handles empty navigation section"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

** Navigation
"
     (org-back-to-heading)
     (let ((nav (org-canvas--settings-parse-navigation)))
       (expect nav :to-be nil))))

  (it "handles mixed visible and hidden tabs"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

** Navigation
1. Home
2. +Modules+
3. Assignments
"
     (org-back-to-heading)
     (let ((nav (org-canvas--settings-parse-navigation)))
       (expect (length nav) :to-equal 3)
       (expect (plist-get (nth 1 nav) :label) :to-equal "Modules")
       (expect (plist-get (nth 1 nav) :hidden) :to-be t)
       (expect (plist-get (nth 2 nav) :label) :to-equal "Assignments")
       (expect (plist-get (nth 2 nav) :hidden) :to-be nil)))))

(describe "org-canvas--settings-sync-tabs"
  (it "PUTs changed tabs"
    (with-org-canvas-test-config
      (let ((put-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (cond
                      ((eq method 'GET)
                       ;; Return current tabs
                       '(((id . "home") (label . "Home") (hidden . :json-false) (position . 1))
                         ((id . "modules") (label . "Modules") (hidden . :json-false) (position . 2))
                         ((id . "discussions") (label . "Discussions") (hidden . :json-false) (position . 3))))
                      ((eq method 'PUT)
                       (push (list url (plist-get args :data)) put-calls)
                       nil)))))
          (org-canvas--settings-sync-tabs
           '((:label "Home" :hidden nil :position 1)
             (:label "Modules" :hidden nil :position 2)
             (:label "Discussions" :hidden t :position 3)))
          ;; Only Discussions should change (hidden)
          (expect (length put-calls) :to-equal 1)
          (expect (caar put-calls) :to-match "tabs/discussions")))))

  (it "skips when no navigation specified"
    (let ((api-called nil))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (setq api-called t) nil)))
        (org-canvas--settings-sync-tabs nil)
        (expect api-called :to-be nil))))

  (it "warns when trying to hide Home tab"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _args)
                   (when (eq method 'GET)
                     '(((id . "home") (label . "Home") (hidden . :json-false) (position . 1)))))))
        (org-canvas--settings-sync-tabs
         '((:label "Home" :hidden t :position 1)))
        (expect 'org-canvas--log-warning :to-have-been-called))))

  (it "logs tab changes in dry-run mode"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t)
            (api-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) (setq api-called t) nil)))
          (org-canvas--settings-sync-tabs
           '((:label "Modules" :hidden nil :position 1)))
          (expect api-called :to-be nil))))))

(describe "org-canvas--settings-pull-tabs"
  (it "formats tabs as numbered Org list with strikethrough"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   '(((id . "home") (label . "Home") (hidden . :json-false) (position . 1))
                     ((id . "modules") (label . "Modules") (hidden . :json-false) (position . 2))
                     ((id . "discussions") (label . "Discussions") (hidden . t) (position . 3))))))
        (let ((result (org-canvas--settings-pull-tabs)))
          (expect result :to-match "\\*\\* Navigation")
          (expect result :to-match "1\\. Home")
          (expect result :to-match "2\\. Modules")
          (expect result :to-match "3\\. \\+Discussions\\+")))))

  (it "returns nil when no tabs"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   nil)))
        (let ((result (org-canvas--settings-pull-tabs)))
          (expect result :to-be nil))))))

;;;; Course Image Tests

(describe "org-canvas--settings-parse-entry (course image)"
  (it "parses file link as course-image-path"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:COURSE_IMAGE: [[file:images/banner.png]]
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :course-image-path) :to-match "images/banner\\.png$")
       (expect (plist-get data :course-image-url) :to-be nil))))

  (it "parses URL as course-image-url"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:COURSE_IMAGE: https://example.com/banner.png
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :course-image-url) :to-equal "https://example.com/banner.png")
       (expect (plist-get data :course-image-path) :to-be nil))))

  (it "returns nil for both when COURSE_IMAGE is absent"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--settings-parse-entry)))
       (expect (plist-get data :course-image-path) :to-be nil)
       (expect (plist-get data :course-image-url) :to-be nil)))))

(describe "org-canvas--settings-build-payload (course image)"
  (it "includes image_url for URL-based image"
    (let* ((data '(:title "Course" :course-image-url "https://example.com/img.png"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "image_url" course) :to-equal "https://example.com/img.png")
      (expect (gethash "image_id" course nil) :to-be nil)))

  (it "includes image_id when file was uploaded"
    (let* ((data '(:title "Course" :course-image-id 42))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "image_id" course) :to-equal 42)
      (expect (gethash "image_url" course nil) :to-be nil)))

  (it "omits image fields when neither set"
    (let* ((data '(:title "Course"))
           (payload (org-canvas--settings-build-payload data))
           (course (gethash "course" payload)))
      (expect (gethash "image_id" course nil) :to-be nil)
      (expect (gethash "image_url" course nil) :to-be nil))))

(describe "org-canvas--settings-resolve-course-image"
  (it "uploads file and adds :course-image-id"
    (cl-letf (((symbol-function 'org-canvas--upload-file)
               (lambda (_path &optional _url _name)
                 '((id . 999)))))
      (let* ((temp-file (make-temp-file "img-test" nil ".png"))
             (data (list :course-image-path temp-file)))
        (unwind-protect
            (let ((result (org-canvas--settings-resolve-course-image data)))
              (expect (plist-get result :course-image-id) :to-equal 999))
          (delete-file temp-file)))))

  (it "warns when file not found"
    (spy-on 'org-canvas--log-warning)
    (let ((data '(:course-image-path "/tmp/nonexistent-img-test.png")))
      (let ((result (org-canvas--settings-resolve-course-image data)))
        (expect (plist-get result :course-image-id) :to-be nil)
        (expect 'org-canvas--log-warning :to-have-been-called))))

  (it "skips upload in dry-run mode"
    (let ((org-canvas--dry-run t)
          (upload-called nil))
      (cl-letf (((symbol-function 'org-canvas--upload-file)
                 (lambda (&rest _) (setq upload-called t) '((id . 1)))))
        (let ((data '(:course-image-path "/tmp/some-image.png")))
          (org-canvas--settings-resolve-course-image data)
          (expect upload-called :to-be nil)))))

  (it "passes through data when no image path"
    (let* ((data '(:title "Course" :course-image-url "https://example.com/x.png"))
           (result (org-canvas--settings-resolve-course-image data)))
      (expect (plist-get result :title) :to-equal "Course")
      (expect (plist-get result :course-image-url) :to-equal "https://example.com/x.png"))))

(describe "org-canvas-pull-settings (course image)"
  (it "downloads image and stores [[file:...]] link in COURSE_IMAGE"
    (let* ((temp-dir (make-temp-file "pull-settings" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-settings-file settings-file)
                (org-canvas-directory temp-dir))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           '((name . "Course")
                             (time_zone . "UTC")
                             (default_view . "modules")
                             (apply_assignment_group_weights . :json-false)
                             (hide_final_grades . :json-false)
                             (public_syllabus . :json-false)
                             (is_public . :json-false)
                             (image_download_url . "https://canvas.example.com/files/123/preview.jpg?token=xyz"))))
                        ((symbol-function 'org-canvas--file-pull-download)
                         (lambda (_dn _url path _sz)
                           (make-directory (file-name-directory path) t)
                           (with-temp-file path (insert "image bytes"))))
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
                   (let ((stored (org-entry-get (point) "COURSE_IMAGE")))
                     (expect stored :to-match "\\[\\[file:content/course_image/preview\\.jpg\\]")
                     (expect stored :not :to-match "token=xyz")
                     (expect stored :not :to-match "https://")))))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--settings-course-image-basename"
  (it "drops query string from URL"
    (expect (org-canvas--settings-course-image-basename
             "https://inst-fs.example/files/img.jpg?token=ABC&download=1")
            :to-equal "img.jpg"))
  (it "drops fragment from URL"
    (expect (org-canvas--settings-course-image-basename
             "https://example.com/path/banner.png#frag")
            :to-equal "banner.png"))
  (it "returns nil when URL is nil"
    (expect (org-canvas--settings-course-image-basename nil) :to-be nil))
  (it "handles plain URL without query"
    (expect (org-canvas--settings-course-image-basename
             "https://example.com/img.gif")
            :to-equal "img.gif")))

(describe "course image download"
  (it "downloads image_download_url into content/course_image/ and stores relpath link"
    (let* ((dir (make-temp-file "settings-test-" t))
           (downloaded-paths nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--file-pull-download)
                     (lambda (_display-name _url path _size)
                       (push path downloaded-paths)
                       (make-directory (file-name-directory path) t)
                       (with-temp-file path (insert "image bytes")))))
            (let ((org-canvas-directory dir))
              (with-temp-org-buffer
               "* Settings\n"
               (org-back-to-heading)
               (org-canvas--settings-pull-set-properties
                (point)
                '((id . 1) (name . "Test")
                  (image_download_url . "https://inst-fs.example/files/img.jpg?token=ABC&download=1&exp=999"))
                nil nil)
               (let ((stored (org-entry-get (point) "COURSE_IMAGE")))
                 (expect stored :to-match "\\[\\[file:content/course_image/img\\.jpg\\]")
                 (expect downloaded-paths :to-have-same-items-as
                         (list (expand-file-name "content/course_image/img.jpg" dir)))))))
        (delete-directory dir t))))

  (it "stores the local relpath form, not the signed URL"
    (let* ((dir (make-temp-file "settings-test-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--file-pull-download)
                     (lambda (_dn _url path _sz)
                       (make-directory (file-name-directory path) t)
                       (with-temp-file path (insert "x")))))
            (let ((org-canvas-directory dir))
              (with-temp-org-buffer
               "* Settings\n"
               (org-back-to-heading)
               (org-canvas--settings-pull-set-properties
                (point)
                '((image_download_url . "https://x.example/files/img.jpg?token=secret"))
                nil nil)
               (let ((stored (org-entry-get (point) "COURSE_IMAGE")))
                 (expect stored :not :to-match "token=secret")
                 (expect stored :not :to-match "https://")))))
        (delete-directory dir t))))

  (it "skips download when image_download_url is absent"
    (let* ((dir (make-temp-file "settings-test-" t))
           (call-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--file-pull-download)
                     (lambda (&rest _) (cl-incf call-count))))
            (let ((org-canvas-directory dir))
              (with-temp-org-buffer
               "* Settings\n"
               (org-back-to-heading)
               (org-canvas--settings-pull-set-properties
                (point) '((id . 1)) nil nil)
               (expect (org-entry-get (point) "COURSE_IMAGE") :to-be nil)
               (expect call-count :to-equal 0))))
        (delete-directory dir t)))))

;;;; Additional Navigation Tab Tests

(describe "org-canvas--settings-sync-tabs (edge cases)"
  (it "warns when tab label not found on Canvas"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _args)
                   (when (eq method 'GET)
                     '(((id . "home") (label . "Home") (hidden . :json-false) (position . 1)))))))
        (org-canvas--settings-sync-tabs
         '((:label "Nonexistent Tab" :hidden nil :position 2)))
        (expect 'org-canvas--log-warning :to-have-been-called))))

  (it "updates tab position without hidden change"
    (with-org-canvas-test-config
      (let ((put-payload nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest args)
                     (cond
                      ((eq method 'GET)
                       '(((id . "modules") (label . "Modules") (hidden . :json-false) (position . 3))))
                      ((eq method 'PUT)
                       (setq put-payload (plist-get args :data))
                       nil)))))
          (org-canvas--settings-sync-tabs
           '((:label "Modules" :hidden nil :position 1)))
          ;; Should PUT because position changed
          (expect put-payload :to-be-truthy)
          (expect (alist-get 'position put-payload) :to-equal 1)))))

  (it "handles PUT error gracefully"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _args)
                   (cond
                    ((eq method 'GET)
                     '(((id . "modules") (label . "Modules") (hidden . :json-false) (position . 1))))
                    ((eq method 'PUT)
                     (signal 'error '("500 Internal Server Error")))))))
        (org-canvas--settings-sync-tabs
         '((:label "Modules" :hidden t :position 1)))
        (expect 'org-canvas--log-warning :to-have-been-called)))))

(describe "org-canvas--settings-pull-tabs (edge cases)"
  (it "sorts tabs by position"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   '(((id . "disc") (label . "Discussions") (hidden . :json-false) (position . 3))
                     ((id . "home") (label . "Home") (hidden . :json-false) (position . 1))
                     ((id . "mod") (label . "Modules") (hidden . :json-false) (position . 2))))))
        (let ((result (org-canvas--settings-pull-tabs)))
          ;; Home should be first despite being second in API response
          (expect result :to-match "1\\. Home")
          (expect result :to-match "2\\. Modules")
          (expect result :to-match "3\\. Discussions"))))))

;;;; Core Upload Infrastructure Tests

(describe "org-canvas--guess-content-type"
  (it "returns image/png for .png"
    (expect (org-canvas--guess-content-type "banner.png") :to-equal "image/png"))

  (it "returns image/jpeg for .jpg"
    (expect (org-canvas--guess-content-type "photo.jpg") :to-equal "image/jpeg"))

  (it "returns image/jpeg for .jpeg"
    (expect (org-canvas--guess-content-type "photo.jpeg") :to-equal "image/jpeg"))

  (it "returns image/gif for .gif"
    (expect (org-canvas--guess-content-type "anim.gif") :to-equal "image/gif"))

  (it "returns image/svg+xml for .svg"
    (expect (org-canvas--guess-content-type "logo.svg") :to-equal "image/svg+xml"))

  (it "returns application/pdf for .pdf"
    (expect (org-canvas--guess-content-type "doc.pdf") :to-equal "application/pdf"))

  (it "returns application/octet-stream for unknown"
    (expect (org-canvas--guess-content-type "file.xyz") :to-equal "application/octet-stream")))

(describe "org-canvas--upload-build-multipart"
  (it "builds unibyte multipart body"
    (let* ((temp-file (make-temp-file "upload-test" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "hello"))
            (let ((body (org-canvas--upload-build-multipart
                         '((key . "val"))
                         temp-file
                         "BOUNDARY")))
              (expect (multibyte-string-p body) :to-be nil)
              (expect body :to-match "BOUNDARY")
              (expect body :to-match "name=\"key\"")
              (expect body :to-match "name=\"file\"")))
        (delete-file temp-file))))

  (it "fixes null filename param"
    (let* ((temp-file (make-temp-file "upload-fix" nil ".png")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "PNG"))
            (let ((body (org-canvas--upload-build-multipart
                         '((filename . nil) (content_type . "unknown/unknown"))
                         temp-file
                         "BOUNDARY")))
              ;; Should have actual filename, not "nil"
              (expect body :not :to-match "name=\"filename\"\r\n\r\nnil")
              ;; Should have actual content type
              (expect body :not :to-match "unknown/unknown")))
        (delete-file temp-file)))))

;;;; Coverage: late policy POST also fails (Lines 288-289)

(describe "org-canvas--settings-push-late-policy (both PUT and POST fail)"
  (it "logs error when PUT 404s and POST fails"
    (with-org-canvas-test-config
      (let ((error-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (cond
                      ((eq method 'PUT)
                       (signal 'error '("404 Not Found")))
                      ((eq method 'POST)
                       (signal 'error '("403 Forbidden"))))))
                  ((symbol-function 'org-canvas--log-error)
                   (lambda (_logger fmt &rest args)
                     (when (string-match "Late policy sync failed" fmt)
                       (setq error-logged (apply #'format fmt args))))))
          (let ((payload (make-hash-table :test 'equal))
                (lp (make-hash-table :test 'equal)))
            (puthash "late_submission_deduction" 10 lp)
            (puthash "late_policy" lp payload)
            ;; Should not throw — the error is caught and logged
            (org-canvas--settings-push-late-policy payload)
            (expect error-logged :to-match "403 Forbidden")))))))

;;;; Coverage: Navigation parse when no next ** heading (Line 319)

(describe "org-canvas--settings-parse-navigation (last heading)"
  (it "parses navigation bounded by next level-2 heading"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

** Navigation
1. Home
2. Modules

** Syllabus
Some text.
"
     (org-back-to-heading)
     (let ((result (org-canvas--settings-parse-navigation)))
       (expect (length result) :to-equal 2)
       (expect (plist-get (nth 0 result) :label) :to-equal "Home")
       (expect (plist-get (nth 1 result) :label) :to-equal "Modules"))))

  (it "parses navigation when it is the last level-2 heading"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

** Navigation
1. Home
2. +Grades+
3. Modules
"
     (org-back-to-heading)
     (let ((result (org-canvas--settings-parse-navigation)))
       (expect (length result) :to-equal 3)
       (expect (plist-get (nth 0 result) :label) :to-equal "Home")
       (expect (plist-get (nth 0 result) :hidden) :to-be nil)
       (expect (plist-get (nth 1 result) :label) :to-equal "Grades")
       (expect (plist-get (nth 1 result) :hidden) :to-be-truthy)
       (expect (plist-get (nth 2 result) :label) :to-equal "Modules")
       (expect (plist-get (nth 2 result) :hidden) :to-be nil)))))

;;;; Coverage: image upload error (Lines 444-446)

(describe "org-canvas--settings-resolve-course-image (upload error)"
  (it "logs warning and returns data unchanged when upload raises error"
    (let* ((temp-file (make-temp-file "img-test" nil ".png"))
           (warning-logged nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--upload-file)
                     (lambda (_path &optional _url _name)
                       (signal 'error '("Network timeout"))))
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_logger fmt &rest args)
                       (when (string-match "Upload failed" fmt)
                         (setq warning-logged (apply #'format fmt args))))))
            (let* ((data (list :course-image-path temp-file :title "Course"))
                   (result (org-canvas--settings-resolve-course-image data)))
              ;; Should return data without :course-image-id
              (expect (plist-get result :course-image-id) :to-be nil)
              (expect (plist-get result :title) :to-equal "Course")
              (expect warning-logged :to-match "Network timeout")))
        (delete-file temp-file)))))

;;;; Coverage: insert-navigation-heading (Lines 604-620)

(describe "org-canvas--settings-insert-navigation-heading"
  (it "inserts navigation at end of subtree when none existed before"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Body text.
"
     (org-canvas--settings-insert-navigation-heading
      "** Navigation\n1. Home\n2. Modules\n")
     (expect (buffer-string) :to-match "\\*\\* Navigation")
     (expect (buffer-string) :to-match "1\\. Home")
     (expect (buffer-string) :to-match "2\\. Modules")))

  (it "removes existing Navigation and preserves following heading"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

** Navigation
1. Old Tab

** Syllabus
Keep this.
"
     (org-canvas--settings-insert-navigation-heading
      "** Navigation\n1. New Tab\n")
     (let ((content (buffer-string)))
       (expect content :to-match "New Tab")
       (expect content :not :to-match "Old Tab")
       (expect content :to-match "Syllabus")
       (expect content :to-match "Keep this"))))

  (it "removes existing Navigation when it is the last heading"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

** Navigation
1. Old Tab
"
     (org-canvas--settings-insert-navigation-heading
      "** Navigation\n1. New Tab\n")
     (let ((content (buffer-string)))
       (expect content :to-match "New Tab")
       (expect content :not :to-match "Old Tab")))))

;;;; Coverage: pull-settings with navigation tabs (Line 664)

(describe "org-canvas-pull-settings (with navigation tabs)"
  (it "inserts navigation heading when tabs are pulled successfully"
    (let* ((temp-dir (make-temp-file "pull-settings" t))
           (settings-file (expand-file-name "settings.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-settings-file settings-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method url &rest _args)
                           (cond
                            ((string-match "late_policy" url)
                             nil)
                            ((string-match "tabs" url)
                             '(((id . "home") (label . "Home") (hidden . :json-false) (position . 1))
                               ((id . "modules") (label . "Modules") (hidden . :json-false) (position . 2))
                               ((id . "grades") (label . "Grades") (hidden . t) (position . 3))))
                            (t
                             '((name . "Test Course")
                               (time_zone . "UTC")
                               (default_view . "modules")
                               (apply_assignment_group_weights . :json-false)
                               (hide_final_grades . :json-false)
                               (public_syllabus . :json-false)
                               (is_public . :json-false))))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-settings)
                (let ((content (with-temp-buffer
                                 (insert-file-contents settings-file)
                                 (buffer-string))))
                  ;; Navigation heading should be present
                  (expect content :to-match "\\*\\* Navigation")
                  (expect content :to-match "1\\. Home")
                  (expect content :to-match "2\\. Modules")
                  (expect content :to-match "3\\. \\+Grades\\+")))))
        (let ((buf (find-buffer-visiting settings-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;; org-canvas-settings-test.el ends here
