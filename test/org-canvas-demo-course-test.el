;;; org-canvas-demo-course-test.el --- Dogfood the bundled demo course  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Integration test that runs the offline validation engine over the bundled
;; demo-course/ (16 .org files covering every content type) and asserts it has
;; zero validation errors.  This dogfoods the example users learn from on REAL
;; representative data, so it can't silently rot, and it exercises
;; parse/validate/link-resolution across all modules at once.
;;
;; Warnings (e.g. "sync target first" for unsynced link targets) are expected
;; for a never-synced example and are allowed; only errors fail the test.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'seq)

(defconst org-canvas-demo--dir
  (let ((rel "demo-course"))
    (cond ((file-directory-p rel) (expand-file-name rel))
          ((file-directory-p (expand-file-name rel default-directory))
           (expand-file-name rel default-directory))
          (t rel)))
  "Absolute path to the bundled demo course.")

(defmacro org-canvas-demo--with-files (&rest body)
  "Run BODY with every org-canvas file var bound to its demo-course file."
  (declare (indent 0))
  `(let ((org-canvas-assignments-file (expand-file-name "assignments.org" org-canvas-demo--dir))
         (org-canvas-quizzes-file (expand-file-name "quizzes.org" org-canvas-demo--dir))
         (org-canvas-new-quizzes-file (expand-file-name "new-quizzes.org" org-canvas-demo--dir))
         (org-canvas-pages-file (expand-file-name "pages.org" org-canvas-demo--dir))
         (org-canvas-modules-file (expand-file-name "modules.org" org-canvas-demo--dir))
         (org-canvas-rubrics-file (expand-file-name "rubrics.org" org-canvas-demo--dir))
         (org-canvas-outcomes-file (expand-file-name "outcomes.org" org-canvas-demo--dir))
         (org-canvas-discussions-file (expand-file-name "discussions.org" org-canvas-demo--dir))
         (org-canvas-announcements-file (expand-file-name "announcements.org" org-canvas-demo--dir))
         (org-canvas-assignment-groups-file (expand-file-name "assignment-groups.org" org-canvas-demo--dir))
         (org-canvas-files-file (expand-file-name "files.org" org-canvas-demo--dir))
         (org-canvas-group-categories-file (expand-file-name "group-categories.org" org-canvas-demo--dir))
         (org-canvas-calendar-events-file (expand-file-name "calendar.org" org-canvas-demo--dir))
         (org-canvas-sections-file (expand-file-name "sections.org" org-canvas-demo--dir))
         (org-canvas-settings-file (expand-file-name "settings.org" org-canvas-demo--dir)))
     (unwind-protect
         (progn ,@body)
       ;; Don't leave demo files open for later tests.  Suppress kill queries
       ;; and clear the modified flag so a noninteractive run can never block
       ;; on a "kill modified buffer?" prompt.
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (let ((bf (buffer-file-name buf)))
             (when (and bf (string-prefix-p org-canvas-demo--dir bf))
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf))))))))

(describe "demo-course dogfood"
  (it "exists and ships the documented content files"
    (expect (file-directory-p org-canvas-demo--dir) :to-be-truthy)
    (expect (file-exists-p (expand-file-name "assignments.org" org-canvas-demo--dir))
            :to-be-truthy))

  (it "validates with zero errors"
    (org-canvas-demo--with-files
      (let* ((result (org-canvas--validate-run-all-specs))
             (issues (plist-get result :issues))
             (errors (seq-filter (lambda (i) (eq (plist-get i :severity) 'error))
                                 issues)))
        ;; It actually validated files (not silently a no-op).
        (expect (> (plist-get result :checked) 0) :to-be t)
        ;; Surface any error messages in the failure output.
        (expect (mapcar (lambda (e) (plist-get e :message)) errors)
                :to-equal nil)))))

(provide 'org-canvas-demo-course-test)
;;; org-canvas-demo-course-test.el ends here
