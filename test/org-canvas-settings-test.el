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
      (expect (gethash "is_public" course) :to-equal :json-false))))

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

;;;; Timestamp helper

(describe "org-canvas--settings-format-timestamp"
  (it "converts ISO8601 to Org timestamp"
    (let ((result (org-canvas--settings-format-timestamp "2026-01-15T10:00:00Z")))
      (expect result :to-match "2026-01-15")))

  (it "returns nil for nil input"
    (expect (org-canvas--settings-format-timestamp nil) :to-be nil))

  (it "returns nil for :null input"
    (expect (org-canvas--settings-format-timestamp :null) :to-be nil)))

;;; org-canvas-settings-test.el ends here
