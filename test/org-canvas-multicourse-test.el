;;; org-canvas-multicourse-test.el --- Tests for multi-course support  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)
(require 'org-canvas-setup)

;; Dynamic variables for recompute tests (set uses symbol-value)
(defvar test-var-a nil)
(defvar test-var-b nil)
(defvar test-var-c nil)
(defvar test-mc-var nil)
(defvar test-watcher-var nil)

;;;; File Variable Registry

(describe "org-canvas-register-file-var"
  (it "adds entry to the registry"
    (let ((org-canvas--file-var-registry nil))
      (org-canvas-register-file-var 'org-canvas-pages-file "pages.org")
      (expect org-canvas--file-var-registry
              :to-equal '((org-canvas-pages-file . "pages.org")))))

  (it "does not duplicate entries"
    (let ((org-canvas--file-var-registry
           '((org-canvas-pages-file . "pages.org"))))
      (org-canvas-register-file-var 'org-canvas-pages-file "pages.org")
      (expect (length org-canvas--file-var-registry) :to-equal 1)))

  (it "allows multiple different vars"
    (let ((org-canvas--file-var-registry nil))
      (org-canvas-register-file-var 'org-canvas-pages-file "pages.org")
      (org-canvas-register-file-var 'org-canvas-rubrics-file "rubrics.org")
      (expect (length org-canvas--file-var-registry) :to-equal 2))))

;;;; File Path Recomputation

(describe "org-canvas--recompute-file-paths"
  (it "updates all registered vars from org-canvas-directory"
    (let ((org-canvas--file-var-registry
           '((test-var-a . "pages.org")
             (test-var-b . "rubrics.org")))
          (org-canvas-directory "/tmp/course-a")
          (test-var-a nil)
          (test-var-b nil))
      (org-canvas--recompute-file-paths)
      (expect test-var-a :to-equal "/tmp/course-a/pages.org")
      (expect test-var-b :to-equal "/tmp/course-a/rubrics.org")))

  (it "reflects directory changes on subsequent calls"
    (let ((org-canvas--file-var-registry
           '((test-var-c . "assignments.org")))
          (org-canvas-directory "/tmp/course-a")
          (test-var-c nil))
      (org-canvas--recompute-file-paths)
      (expect test-var-c :to-equal "/tmp/course-a/assignments.org")
      (setq org-canvas-directory "/tmp/course-b")
      (org-canvas--recompute-file-paths)
      (expect test-var-c :to-equal "/tmp/course-b/assignments.org"))))

;;;; Directory Watcher

(describe "org-canvas--directory-watcher"
  (it "recomputes file vars when called with `set' operation"
    (let ((org-canvas--file-var-registry
           '((test-watcher-var . "pages.org")))
          (test-watcher-var nil))
      (org-canvas--directory-watcher 'org-canvas-directory
                                     "/tmp/from-watcher" 'set nil)
      (expect test-watcher-var :to-equal "/tmp/from-watcher/pages.org")))

  (it "ignores `let' operations to avoid leaking test rebindings"
    (let ((org-canvas--file-var-registry
           '((test-watcher-var . "pages.org")))
          (test-watcher-var "/old/value"))
      (org-canvas--directory-watcher 'org-canvas-directory
                                     "/tmp/from-let" 'let nil)
      (expect test-watcher-var :to-equal "/old/value")))

  (it "ignores nil and empty new values"
    (let ((org-canvas--file-var-registry
           '((test-watcher-var . "pages.org")))
          (test-watcher-var "/keep/me"))
      (org-canvas--directory-watcher 'org-canvas-directory nil 'set nil)
      (expect test-watcher-var :to-equal "/keep/me")
      (org-canvas--directory-watcher 'org-canvas-directory "" 'set nil)
      (expect test-watcher-var :to-equal "/keep/me")))

  (it "fires automatically on setq of org-canvas-directory"
    (let ((org-canvas--file-var-registry
           '((test-watcher-var . "modules.org")))
          (saved-dir org-canvas-directory)
          (test-watcher-var nil))
      (unwind-protect
          (progn
            (setq org-canvas-directory "/tmp/setq-test/")
            (expect test-watcher-var
                    :to-equal "/tmp/setq-test/modules.org"))
        (setq org-canvas-directory saved-dir)))))

;;;; Course Activation

(describe "org-canvas-activate-course"
  (it "errors on unknown course name"
    (let ((org-canvas-courses '(("Math" . "/tmp/math"))))
      (let ((err (should-error (org-canvas-activate-course "Physics")
                               :type 'user-error)))
        (expect (cadr err) :to-match "not found"))))

  (it "errors when directory does not exist"
    (let ((org-canvas-courses '(("Math" . "/tmp/nonexistent-dir-xyz"))))
      (let ((err (should-error (org-canvas-activate-course "Math")
                               :type 'user-error)))
        (expect (cadr err) :to-match "directory does not exist"))))

  (it "errors when credentials file is missing"
    (let* ((dir (make-temp-file "course-" t))
           (org-canvas-courses `(("Math" . ,dir))))
      (unwind-protect
          (let ((err (should-error (org-canvas-activate-course "Math")
                                   :type 'user-error)))
            (expect (cadr err) :to-match "credentials"))
        (delete-directory dir t))))

  (it "loads credentials and recomputes paths on success"
    (let* ((dir (make-temp-file "course-" t))
           (cred-file (expand-file-name "org-canvas-credentials.el" dir))
           (org-canvas-courses `(("Math" . ,dir)))
           (org-canvas--file-var-registry
            '((test-mc-var . "pages.org")))
           (org-canvas--active-course-name nil)
           (org-canvas--inhibit-log-clear t)
           (test-mc-var nil)
           ;; Save globals that `load' will overwrite
           (saved-dir org-canvas-directory)
           (saved-url org-canvas-base-url)
           (saved-token org-canvas-api-token)
           (saved-course-id org-canvas-course-id))
      (unwind-protect
          (progn
            (with-temp-file cred-file
              (insert (format "(setq org-canvas-directory %S)\n" dir))
              (insert "(setq org-canvas-base-url \"https://test.canvas.com\")\n")
              (insert "(setq org-canvas-api-token \"test-token\")\n")
              (insert "(setq org-canvas-course-id \"42\")\n"))
            (org-canvas-activate-course "Math")
            (expect org-canvas--active-course-name :to-equal "Math")
            (expect org-canvas-course-id :to-equal "42")
            (expect org-canvas-base-url :to-equal "https://test.canvas.com")
            (expect org-canvas-directory :to-equal dir)
            (expect test-mc-var :to-equal (expand-file-name "pages.org" dir)))
        ;; Restore globals to avoid contaminating other tests
        (setq org-canvas-directory saved-dir
              org-canvas-base-url saved-url
              org-canvas-api-token saved-token
              org-canvas-course-id saved-course-id)
        (delete-directory dir t))))

  (it "offers completing-read from org-canvas-courses"
    (let ((org-canvas-courses '(("Math" . "/tmp/math")
                                ("English" . "/tmp/english")))
          (org-canvas--active-course-name nil))
      (spy-on 'completing-read :and-return-value "Math")
      (spy-on 'file-directory-p :and-return-value t)
      (spy-on 'file-exists-p :and-return-value t)
      (spy-on 'load)
      (spy-on 'org-canvas--recompute-file-paths)
      (spy-on 'org-canvas-clear-log)
      (call-interactively 'org-canvas-activate-course)
      (expect 'completing-read :to-have-been-called)
      (let ((candidates (nth 1 (spy-calls-args-for 'completing-read 0))))
        (expect candidates :to-contain "Math")
        (expect candidates :to-contain "English")))))

;;;; Backwards Compatibility

(describe "multi-course backwards compatibility"
  (it "defaults org-canvas-courses to nil"
    (expect (default-value 'org-canvas-courses) :to-be nil))

  (it "defaults org-canvas--active-course-name to nil"
    (expect (default-value 'org-canvas--active-course-name) :to-be nil)))

;;;; Feature Module Registration

;;;; org-canvas-init course registration

(describe "org-canvas-init course registration"
  (it "registers course when user provides a name"
    (let* ((temp-dir (make-temp-file "init-register-" t))
           (org-canvas-courses nil)
           (org-canvas--active-course-name nil))
      (unwind-protect
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _) temp-dir))
                    ((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (cond
                        ((string-match-p "^Canvas base" prompt) "https://canvas.example.com")
                        ((string-match-p "^Course ID" prompt) "12345")
                        ((string-match-p "Register" prompt) "My Course")
                        (t ""))))
                    ((symbol-function 'read-passwd)
                     (lambda (&rest _) "valid-token"))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (&rest _) '((name . "Test Course"))))
                    ((symbol-function 'y-or-n-p) (lambda (_) nil))
                    ((symbol-function 'customize-save-variable)
                     (lambda (_sym val) (setq org-canvas-courses val))))
            (org-canvas-init)
            (expect org-canvas--active-course-name :to-equal "My Course")
            (expect (assoc "My Course" org-canvas-courses) :to-be-truthy))
        (delete-directory temp-dir t)))))

;;;; org-canvas-status active course display

(describe "org-canvas-status active course name"
  (it "shows active course name in status buffer"
    (let ((org-canvas--active-course-name "Math 101")
          (org-canvas-course-id "99999")
          (org-canvas-base-url "https://test.example.com"))
      (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil))
                ((symbol-function 'org-canvas--status-report-file)
                 (lambda (&rest _) nil)))
        (org-canvas-status)
        (with-current-buffer "*canvas-status*"
          (expect (buffer-string) :to-match "Active course: Math 101")
          (kill-buffer))))))

;;;; Feature Module Registration

(describe "feature module file var registration"
  (it "registers all 15 feature file vars at load time"
    ;; After requiring all modules (which happens via org-canvas),
    ;; the registry should have entries for all feature files
    (require 'org-canvas)
    (let ((expected-vars '(org-canvas-announcements-file
                           org-canvas-assignment-groups-file
                           org-canvas-assignments-file
                           org-canvas-calendar-events-file
                           org-canvas-discussions-file
                           org-canvas-files-file
                           org-canvas-group-categories-file
                           org-canvas-modules-file
                           org-canvas-new-quizzes-file
                           org-canvas-outcomes-file
                           org-canvas-pages-file
                           org-canvas-quizzes-file
                           org-canvas-rubrics-file
                           org-canvas-sections-file
                           org-canvas-settings-file)))
      (dolist (var expected-vars)
        (expect (assq var org-canvas--file-var-registry)
                :to-be-truthy)))))

(describe "org-canvas-activate-course forgets the course zone (issue #136)"
  (it "resets the resolved zone when a course is activated"
    (let* ((dir (make-temp-file "course-" t))
           (cred-file (expand-file-name "org-canvas-credentials.el" dir))
           (org-canvas-courses `(("Math" . ,dir)))
           (org-canvas--active-course-name nil)
           (org-canvas--inhibit-log-clear t)
           (org-canvas--pull-tz-cache "America/Chicago")
           (org-canvas--time-zone-resolved t))
      (unwind-protect
          (progn
            (with-temp-file cred-file (insert ""))
            (cl-letf (((symbol-function 'load) #'ignore)
                      ((symbol-function 'message) #'ignore))
              (org-canvas-activate-course "Math"))
            (expect org-canvas--pull-tz-cache :to-be nil)
            (expect org-canvas--time-zone-resolved :to-be nil))
        (delete-directory dir t)))))

(provide 'org-canvas-multicourse-test)
;;; org-canvas-multicourse-test.el ends here
