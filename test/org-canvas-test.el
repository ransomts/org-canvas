;;; org-canvas-test.el --- Buttercup tests for org-canvas orchestration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the top-level org-canvas-sync and org-canvas-delete-all functions.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

;;;; org-canvas-sync orchestration

(describe "org-canvas-sync"
  (it "calls all sync functions in dependency order"
    (let ((call-order nil))
      (with-sync-test-env
        (cl-letf (((symbol-function 'org-canvas-sync-outcomes)
                   (lambda () (push 'outcomes call-order)))
                  ((symbol-function 'org-canvas-sync-rubrics)
                   (lambda () (push 'rubrics call-order)))
                  ((symbol-function 'org-canvas-sync-assignment-groups)
                   (lambda () (push 'assignment-groups call-order)))
                  ((symbol-function 'org-canvas-pull-sections)
                   (lambda () (push 'sections call-order)))
                  ((symbol-function 'org-canvas-sync-files)
                   (lambda () (push 'files call-order)))
                  ((symbol-function 'org-canvas-sync-pages)
                   (lambda () (push 'pages call-order)))
                  ((symbol-function 'org-canvas-sync-discussions)
                   (lambda () (push 'discussions call-order)))
                  ((symbol-function 'org-canvas-sync-announcements)
                   (lambda () (push 'announcements call-order)))
                  ((symbol-function 'org-canvas-sync-quizzes)
                   (lambda () (push 'quizzes call-order)))
                  ((symbol-function 'org-canvas-sync-assignments)
                   (lambda () (push 'assignments call-order)))
                  ((symbol-function 'org-canvas-sync-overrides)
                   (lambda () (push 'overrides call-order)))
                  ((symbol-function 'org-canvas-sync-modules)
                   (lambda () (push 'modules call-order))))
        (org-canvas-sync)
        (setq call-order (nreverse call-order))
        ;; Verify order: Tier 0 first, then Tier 1, then assignment-groups again, then modules
        ;; outcomes should come before quizzes/assignments
        (let ((outcomes-pos (cl-position 'outcomes call-order))
              (assignments-pos (cl-position 'assignments call-order))
              (modules-pos (cl-position 'modules call-order)))
          (expect outcomes-pos :to-be-less-than assignments-pos)
          (expect assignments-pos :to-be-less-than modules-pos))
        ;; modules should be last
        (expect (car (last call-order)) :to-equal 'modules)
        ;; assignment-groups called twice (first pass and re-sync)
        (expect (cl-count 'assignment-groups call-order) :to-equal 2))))))

;;;; org-canvas-delete-all orchestration

(describe "org-canvas-delete-all"
  (it "calls all delete functions in reverse dependency order"
    (let ((call-order nil))
      (with-sync-test-env
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'org-canvas-delete-all-modules)
                   (lambda () (push 'modules call-order)))
                  ((symbol-function 'org-canvas-delete-all-assignments)
                   (lambda () (push 'assignments call-order)))
                  ((symbol-function 'org-canvas-delete-all-quizzes)
                   (lambda () (push 'quizzes call-order)))
                  ((symbol-function 'org-canvas-delete-all-files)
                   (lambda () (push 'files call-order)))
                  ((symbol-function 'org-canvas-delete-all-announcements)
                   (lambda () (push 'announcements call-order)))
                  ((symbol-function 'org-canvas-delete-all-discussions)
                   (lambda () (push 'discussions call-order)))
                  ((symbol-function 'org-canvas-delete-all-pages)
                   (lambda () (push 'pages call-order)))
                  ((symbol-function 'org-canvas-delete-all-assignment-groups)
                   (lambda () (push 'assignment-groups call-order)))
                  ((symbol-function 'org-canvas-delete-all-rubrics)
                   (lambda () (push 'rubrics call-order)))
                  ((symbol-function 'org-canvas-delete-all-outcomes)
                   (lambda () (push 'outcomes call-order))))
        (org-canvas-delete-all)
        (setq call-order (nreverse call-order))
        ;; modules should be first (reverse of sync)
        (expect (car call-order) :to-equal 'modules)
        ;; outcomes should be last
        (expect (car (last call-order)) :to-equal 'outcomes)))))

  (it "aborts on first confirmation refusal"
    (let ((delete-called nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil))
                ((symbol-function 'org-canvas-delete-all-modules)
                 (lambda () (setq delete-called t))))
        (expect (org-canvas-delete-all) :to-throw 'user-error)
        (expect delete-called :to-be nil))))

  (it "aborts on second confirmation refusal"
    (let ((confirm-count 0)
          (delete-called nil))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (setq confirm-count (1+ confirm-count))
                   (if (= confirm-count 1) t nil)))
                ((symbol-function 'org-canvas-delete-all-modules)
                 (lambda () (setq delete-called t))))
        (expect (org-canvas-delete-all) :to-throw 'user-error)
        (expect delete-called :to-be nil)))))

;;;; org-canvas--safe-sync

(describe "org-canvas--safe-sync"
  (it "skips gracefully when file not found"
    (spy-on 'elog-info)
    (org-canvas--safe-sync
     (lambda () (error "PAGES file not found: /tmp/nonexistent.org"))
     "Pages")
    (expect 'elog-info :to-have-been-called))

  (it "logs error for non-file-related failures"
    (spy-on 'elog-error)
    (org-canvas--safe-sync
     (lambda () (error "API Request Failed (HTTP 500)"))
     "Pages")
    (expect 'elog-error :to-have-been-called))

  (it "does not throw on any error"
    (expect (org-canvas--safe-sync
             (lambda () (error "Something went wrong"))
             "Test")
            :not :to-throw)))

;;;; org-canvas-sync with preflight

(describe "org-canvas-sync with preflight"
  (it "calls preflight check before syncing"
    (let ((preflight-called nil)
          (sync-called nil))
      (with-sync-test-env
        (cl-letf (((symbol-function 'org-canvas--preflight-check)
                   (lambda () (setq preflight-called t)))
                  ((symbol-function 'org-canvas-sync-outcomes)
                   (lambda () (setq sync-called t)))
                  ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                  ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
        (org-canvas-sync)
        (expect preflight-called :to-be t)
        (expect sync-called :to-be t))))))

;;;; org-canvas-status

(describe "org-canvas-status"
  (it "creates a *canvas-status* buffer"
    (let ((org-canvas-course-id "99999")
          (org-canvas-base-url "https://test.canvas.example.com"))
      (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
        ;; Set all file vars to nonexistent paths so status reports "file not found"
        (let ((org-canvas-assignments-file "/tmp/nonexistent/assignments.org")
              (org-canvas-pages-file "/tmp/nonexistent/pages.org")
              (org-canvas-quizzes-file "/tmp/nonexistent/quizzes.org")
              (org-canvas-modules-file "/tmp/nonexistent/modules.org")
              (org-canvas-files-file "/tmp/nonexistent/files.org")
              (org-canvas-outcomes-file "/tmp/nonexistent/outcomes.org")
              (org-canvas-rubrics-file "/tmp/nonexistent/rubrics.org")
              (org-canvas-discussions-file "/tmp/nonexistent/discussions.org")
              (org-canvas-announcements-file "/tmp/nonexistent/announcements.org")
              (org-canvas-assignment-groups-file "/tmp/nonexistent/assignment-groups.org")
              (org-canvas-sections-file "/tmp/nonexistent/sections.org"))
          (org-canvas-status)
          (expect (get-buffer "*canvas-status*") :to-be-truthy)
          (with-current-buffer "*canvas-status*"
            (expect (buffer-string) :to-match "org-canvas Sync Status")
            (expect (buffer-string) :to-match "99999"))
          (kill-buffer "*canvas-status*")))))

  (it "reports synced and pending counts for existing files"
    (let ((temp-file (make-temp-file "status-test" nil ".org"))
          (org-canvas-course-id "99999")
          (org-canvas-base-url "https://test.canvas.example.com"))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Synced Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-01-01 Thu]
:END:

* Pending Item
:PROPERTIES:
:END:
"))
            (let ((org-canvas-assignments-file temp-file)
                  (org-canvas-pages-file "/tmp/nonexistent/pages.org")
                  (org-canvas-quizzes-file "/tmp/nonexistent/quizzes.org")
                  (org-canvas-modules-file "/tmp/nonexistent/modules.org")
                  (org-canvas-files-file "/tmp/nonexistent/files.org")
                  (org-canvas-outcomes-file "/tmp/nonexistent/outcomes.org")
                  (org-canvas-rubrics-file "/tmp/nonexistent/rubrics.org")
                  (org-canvas-discussions-file "/tmp/nonexistent/discussions.org")
                  (org-canvas-announcements-file "/tmp/nonexistent/announcements.org")
                  (org-canvas-assignment-groups-file "/tmp/nonexistent/assignment-groups.org")
                  (org-canvas-sections-file "/tmp/nonexistent/sections.org"))
              (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-status)
                (with-current-buffer "*canvas-status*"
                  (expect (buffer-string) :to-match "Synced: 1")
                  (expect (buffer-string) :to-match "Pending: 1"))
                (kill-buffer "*canvas-status*"))))
        (delete-file temp-file))))

  (it "shows file not found for missing files"
    (let ((org-canvas-course-id "99999")
          (org-canvas-base-url "https://test.canvas.example.com"))
      (let ((org-canvas-assignments-file "/tmp/nonexistent/assignments.org")
            (org-canvas-pages-file "/tmp/nonexistent/pages.org")
            (org-canvas-quizzes-file "/tmp/nonexistent/quizzes.org")
            (org-canvas-modules-file "/tmp/nonexistent/modules.org")
            (org-canvas-files-file "/tmp/nonexistent/files.org")
            (org-canvas-outcomes-file "/tmp/nonexistent/outcomes.org")
            (org-canvas-rubrics-file "/tmp/nonexistent/rubrics.org")
            (org-canvas-discussions-file "/tmp/nonexistent/discussions.org")
            (org-canvas-announcements-file "/tmp/nonexistent/announcements.org")
            (org-canvas-assignment-groups-file "/tmp/nonexistent/assignment-groups.org")
            (org-canvas-sections-file "/tmp/nonexistent/sections.org"))
        (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
          (org-canvas-status)
          (with-current-buffer "*canvas-status*"
            (expect (buffer-string) :to-match "file not found"))
          (kill-buffer "*canvas-status*"))))))

;;;; org-canvas-sync-dry-run

(describe "org-canvas-sync-dry-run"
  (it "sets org-canvas--dry-run to t during sync"
    (let ((dry-run-value nil))
      (cl-letf (((symbol-function 'org-canvas-sync)
                 (lambda ()
                   (setq dry-run-value org-canvas--dry-run))))
        (org-canvas-sync-dry-run)
        (expect dry-run-value :to-be t))))

  (it "dry-run flag is nil after dry-run completes"
    (cl-letf (((symbol-function 'org-canvas-sync) (lambda () nil)))
      (org-canvas-sync-dry-run)
      (expect org-canvas--dry-run :to-be nil))))

;;; org-canvas-test.el ends here
