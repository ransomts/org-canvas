;;; org-canvas-multicourse-test.el --- Tests for multi-course support  -*- lexical-binding: t; -*-

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

;;;; Course Activation

(describe "org-canvas-activate-course"
  (it "errors on unknown course name"
    (let ((org-canvas-courses '(("Math" . "/tmp/math"))))
      (expect (org-canvas-activate-course "Physics")
              :to-throw 'user-error)))

  (it "errors when directory does not exist"
    (let ((org-canvas-courses '(("Math" . "/tmp/nonexistent-dir-xyz"))))
      (expect (org-canvas-activate-course "Math")
              :to-throw 'user-error)))

  (it "errors when credentials file is missing"
    (let* ((dir (make-temp-file "course-" t))
           (org-canvas-courses `(("Math" . ,dir))))
      (unwind-protect
          (expect (org-canvas-activate-course "Math")
                  :to-throw 'user-error)
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

(provide 'org-canvas-multicourse-test)
;;; org-canvas-multicourse-test.el ends here
