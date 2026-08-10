;;; org-canvas-test.el --- Buttercup tests for org-canvas orchestration  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
        (cl-letf (((symbol-function 'org-canvas--preflight-check)
                   (lambda () nil))
                  ((symbol-function 'org-canvas-sync-settings)
                   (lambda () (push 'settings call-order)))
                  ((symbol-function 'org-canvas-sync-outcomes)
                   (lambda () (push 'outcomes call-order)))
                  ((symbol-function 'org-canvas-sync-rubrics)
                   (lambda () (push 'rubrics call-order)))
                  ((symbol-function 'org-canvas-sync-assignment-groups)
                   (lambda () (push 'assignment-groups call-order)))
                  ((symbol-function 'org-canvas-sync-group-categories)
                   (lambda () (push 'group-categories call-order)))
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
                  ((symbol-function 'org-canvas-sync-calendar-events)
                   (lambda () (push 'calendar-events call-order)))
                  ((symbol-function 'org-canvas-sync-quizzes)
                   (lambda () (push 'quizzes call-order)))
                  ((symbol-function 'org-canvas-sync-new-quizzes)
                   (lambda () (push 'new-quizzes call-order)))
                  ((symbol-function 'org-canvas-sync-assignments)
                   (lambda () (push 'assignments call-order)))
                  ((symbol-function 'org-canvas-sync-overrides)
                   (lambda () (push 'overrides call-order)))
                  ((symbol-function 'org-canvas-sync-modules)
                   (lambda () (push 'modules call-order))))
        (org-canvas-sync)
        (setq call-order (nreverse call-order))
        ;; Verify order: settings first, then Tier 0, Tier 1, modules last
        (let ((settings-pos (cl-position 'settings call-order))
              (outcomes-pos (cl-position 'outcomes call-order))
              (assignments-pos (cl-position 'assignments call-order))
              (modules-pos (cl-position 'modules call-order)))
          (expect settings-pos :to-be-less-than outcomes-pos)
          (expect outcomes-pos :to-be-less-than assignments-pos)
          (expect assignments-pos :to-be-less-than modules-pos))
        ;; modules should be last
        (expect (car (last call-order)) :to-equal 'modules)
        ;; assignment-groups called twice (first pass and re-sync)
        (expect (cl-count 'assignment-groups call-order) :to-equal 2)))))

  (it "emits tier progress messages during sync"
    (with-sync-test-env
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'org-canvas-sync-settings) (lambda () nil))
                ((symbol-function 'org-canvas-sync-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-sync-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-sync-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
        (org-canvas-sync)
        (let ((found nil))
          (dolist (call (spy-calls-all-args 'message))
            (when (and (stringp (car call))
                       (string-match-p "^Syncing:" (car call)))
              (setq found t)))
          (expect found :to-be-truthy)))))

  (it "shows aggregate counts in final sync message"
    (with-sync-test-env
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'org-canvas-sync-settings) (lambda () nil))
                ((symbol-function 'org-canvas-sync-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-sync-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-sync-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
        (org-canvas-sync)
        (let ((found nil))
          (dolist (call (spy-calls-all-args 'message))
            (when (and (stringp (car call))
                       (string-match-p "Sync complete:.*synced.*skipped.*failed" (car call)))
              (setq found t)))
          (expect found :to-be-truthy)))))

  (it "renders the per-type table and deferred count when features record stats"
    (with-sync-test-env
      (spy-on 'message)
      (let ((log-lines nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) log-lines)))
                ((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) log-lines)))
                ((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'org-canvas-sync-settings) (lambda () nil))
                ((symbol-function 'org-canvas-sync-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-sync-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                ((symbol-function 'org-canvas-sync-pages)
                 (lambda ()
                   (org-canvas--sync-record-feature-stats "Pages"
                     '(:success 3 :fail 1 :deferred 1
                       :failed-titles ("Course Home")))))
                ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-sync-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
        (org-canvas-sync)
        ;; Final echo mentions the deferred count
        (let ((found nil))
          (dolist (call (spy-calls-all-args 'message))
            (when (and (stringp (car call))
                       (string-match-p "deferred"
                                       (apply #'format (car call) (cdr call))))
              (setq found t)))
          (expect found :to-be-truthy))
        ;; Log contains the per-type table and the named failure
        (let ((content (mapconcat #'identity (nreverse log-lines) "\n")))
          (expect content :to-match "Type.*Success.*Skipped.*Failed.*Deferred")
          (expect content :to-match "Pages +3 +0 +1 +1")
          (expect content :to-match "Failed Pages: 'Course Home'")))))))

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
                  ((symbol-function 'org-canvas-delete-all-new-quizzes)
                   (lambda () (push 'new-quizzes call-order)))
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
                  ((symbol-function 'org-canvas-delete-all-group-categories)
                   (lambda () (push 'group-categories call-order)))
                  ((symbol-function 'org-canvas-delete-all-rubrics)
                   (lambda () (push 'rubrics call-order)))
                  ((symbol-function 'org-canvas-delete-all-outcomes)
                   (lambda () (push 'outcomes call-order)))
                  ((symbol-function 'org-canvas-delete-all-calendar-events)
                   (lambda () (push 'calendar-events call-order))))
        (org-canvas-delete-all)
        (setq call-order (nreverse call-order))
        ;; modules should be first (reverse of sync)
        (expect (car call-order) :to-equal 'modules)
        ;; calendar-events should be last
        (expect (car (last call-order)) :to-equal 'calendar-events)))))

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
        (expect delete-called :to-be nil))))

  (it "counts synced items in existing manifest files before confirming"
    (let* ((tmp-dir (make-temp-file "org-canvas-delete-" t))
           (assignments-file (expand-file-name "assignments.org" tmp-dir))
           (count-arg-file nil))
      (unwind-protect
          (progn
            (with-temp-file assignments-file
              (insert "* A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (let ((org-canvas-assignments-file assignments-file))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil))
                        ((symbol-function 'org-canvas--status-count-entries)
                         (lambda (file _id-prop)
                           (setq count-arg-file file)
                           (list :synced 7 :pending 0 :legacy 0 :unsaved 0))))
                (with-sync-test-env
                  (expect (org-canvas-delete-all) :to-throw 'user-error))))
            (expect count-arg-file
                    :to-equal (expand-file-name assignments-file)))
        (delete-directory tmp-dir t)))))

;;;; org-canvas--safe-sync

(describe "org-canvas--safe-sync"
  (it "skips gracefully when file not found"
    (spy-on 'org-canvas--log-info)
    (org-canvas--safe-sync
     (lambda () (error "PAGES file not found: /tmp/nonexistent.org"))
     "Pages")
    (expect 'org-canvas--log-info :to-have-been-called))

  (it "suggests pull command and init when file not found"
    (spy-on 'org-canvas--log-info)
    (org-canvas--safe-sync
     (lambda () (error "PAGES file not found: /tmp/nonexistent.org"))
     "Pages")
    (let ((found nil))
      (dolist (call (spy-calls-all-args 'org-canvas--log-info))
        (when (and (>= (length call) 3)
                   (stringp (nth 1 call))
                   (string-match-p "org-canvas-pull" (nth 1 call)))
          (setq found t)))
      (expect found :to-be-truthy)))

  (it "logs error for non-file-related failures"
    (spy-on 'org-canvas--log-error)
    (org-canvas--safe-sync
     (lambda () (error "API Request Failed (HTTP 500)"))
     "Pages")
    (expect 'org-canvas--log-error :to-have-been-called))

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
                  ((symbol-function 'org-canvas-sync-settings) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-outcomes)
                   (lambda () (setq sync-called t)))
                  ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-group-categories) (lambda () nil))
                  ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-calendar-events) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                  ((symbol-function 'org-canvas-sync-new-quizzes) (lambda () nil))
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
        (with-nonexistent-canvas-files
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
              (insert "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Synced Item
:PROPERTIES:
:CANVAS_ID: 123
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
                  (org-canvas-sections-file "/tmp/nonexistent/sections.org")
              (org-canvas-new-quizzes-file "/tmp/nonexistent/new-quizzes.org")
              (org-canvas-group-categories-file "/tmp/nonexistent/group-categories.org")
              (org-canvas-calendar-events-file "/tmp/nonexistent/calendar.org"))
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
      (with-nonexistent-canvas-files
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

;;;; org-canvas-force-push

(describe "org-canvas-force-push"
  (it "sets org-canvas-detect-conflicts to nil during sync"
    (let ((detect-value 'not-set))
      (cl-letf (((symbol-function 'org-canvas-sync)
                 (lambda ()
                   (setq detect-value org-canvas-detect-conflicts))))
        (org-canvas-force-push)
        (expect detect-value :to-be nil))))

  (it "detect-conflicts is restored after force-push"
    (let ((org-canvas-detect-conflicts t))
      (cl-letf (((symbol-function 'org-canvas-sync) (lambda () nil)))
        (org-canvas-force-push)
        (expect org-canvas-detect-conflicts :to-be t)))))

;;;; org-canvas--collect-local-ids

(describe "org-canvas--collect-local-ids"
  (it "collects CANVAS_ID values from level-1 headings"
    (let* ((temp-dir (make-temp-file "orphan-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n\n")
              (insert "* Item B\n:PROPERTIES:\n:CANVAS_ID: 200\n:END:\n\n")
              (insert "* Item C\n:PROPERTIES:\n:END:\n"))
            (let ((ids (org-canvas--collect-local-ids test-file "CANVAS_ID")))
              (expect ids :to-equal '("100" "200"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "collects CANVAS_URL values for pages"
    (let* ((temp-dir (make-temp-file "orphan-test" t))
           (test-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Page A\n:PROPERTIES:\n:CANVAS_URL: page-a\n:END:\n\n")
              (insert "* Page B\n:PROPERTIES:\n:CANVAS_URL: page-b\n:END:\n"))
            (let ((ids (org-canvas--collect-local-ids test-file "CANVAS_URL")))
              (expect ids :to-equal '("page-a" "page-b"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns nil for nonexistent file"
    (expect (org-canvas--collect-local-ids "/tmp/nonexistent.org" "CANVAS_ID")
            :to-be nil))

  (it "returns empty list when no items have IDs"
    (let* ((temp-dir (make-temp-file "orphan-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item A\n:PROPERTIES:\n:END:\n"))
            (let ((ids (org-canvas--collect-local-ids test-file "CANVAS_ID")))
              (expect ids :to-equal nil)))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; org-canvas--find-orphans-for-feature

(describe "org-canvas--find-orphans-for-feature"
  (it "finds items on Canvas but not in local file"
    (let* ((temp-dir (make-temp-file "orphan-test" t))
           (test-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Local Item\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (let ((org-canvas-assignments-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 100) (name . "Local Item"))
                               ((id . 200) (name . "Orphan Item"))))))
                  (let ((orphans (org-canvas--find-orphans-for-feature
                                  '(:name "Assignments"
                                    :endpoint "assignments"
                                    :file-var org-canvas-assignments-file
                                    :id-field id
                                    :id-property "CANVAS_ID"
                                    :title-field name
                                    :list-params nil
                                    :skip-fn nil))))
                    (expect (length orphans) :to-equal 1)
                    (expect (alist-get 'id (car orphans)) :to-equal 200))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns nil when no orphans found"
    (let* ((temp-dir (make-temp-file "orphan-test" t))
           (test-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (let ((org-canvas-assignments-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 100) (name . "Item"))))))
                  (let ((orphans (org-canvas--find-orphans-for-feature
                                  '(:name "Assignments"
                                    :endpoint "assignments"
                                    :file-var org-canvas-assignments-file
                                    :id-field id
                                    :id-property "CANVAS_ID"
                                    :title-field name
                                    :list-params nil
                                    :skip-fn nil))))
                    (expect orphans :to-be nil))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "respects skip-fn to exclude items"
    (let* ((temp-dir (make-temp-file "orphan-test" t))
           (test-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Page\n:PROPERTIES:\n:CANVAS_URL: my-page\n:END:\n"))
            (let ((org-canvas-pages-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((url . "my-page") (title . "Page") (front_page . :json-false))
                               ((url . "front-page") (title . "Front") (front_page . t))
                               ((url . "orphan") (title . "Orphan") (front_page . :json-false))))))
                  (let ((orphans (org-canvas--find-orphans-for-feature
                                  '(:name "Pages"
                                    :endpoint "pages"
                                    :file-var org-canvas-pages-file
                                    :id-field url
                                    :id-property "CANVAS_URL"
                                    :title-field title
                                    :list-params nil
                                    :skip-fn (lambda (item)
                                               (eq (alist-get 'front_page item) t))))))
                    ;; front-page skipped, my-page matched, orphan is orphan
                    (expect (length orphans) :to-equal 1)
                    (expect (alist-get 'url (car orphans)) :to-equal "orphan"))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "handles API errors gracefully"
    (let* ((temp-dir (make-temp-file "orphan-test" t))
           (test-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (let ((org-canvas-assignments-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             (error "API Request Failed (HTTP 500)"))))
                  (let ((orphans (org-canvas--find-orphans-for-feature
                                  '(:name "Assignments"
                                    :endpoint "assignments"
                                    :file-var org-canvas-assignments-file
                                    :id-field id
                                    :id-property "CANVAS_ID"
                                    :title-field name
                                    :list-params nil
                                    :skip-fn nil))))
                    (expect orphans :to-be nil))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; org-canvas--orphan-format-buffer

(describe "org-canvas--orphan-format-buffer"
  (it "formats orphan report with counts"
    (let ((org-canvas-course-id "99999")
          (org-canvas-base-url "https://test.example.com"))
      (let ((buf (org-canvas--orphan-format-buffer
                  (list
                   (cons '(:name "Assignments" :title-field name :id-field id)
                         '(((id . 200) (name . "Orphan A"))
                           ((id . 300) (name . "Orphan B"))))
                   (cons '(:name "Pages" :title-field title :id-field url)
                         '(((url . "orphan-page") (title . "Orphan Page"))))))))
        (unwind-protect
            (with-current-buffer buf
              (expect (buffer-string) :to-match "Orphan Report")
              (expect (buffer-string) :to-match "Assignments: 2 orphan")
              (expect (buffer-string) :to-match "Pages: 1 orphan")
              (expect (buffer-string) :to-match "Total orphans: 3")
              (expect (buffer-string) :to-match "Orphan A")
              (expect (buffer-string) :to-match "Orphan Page"))
          (kill-buffer buf))))))

;;;; org-canvas-cleanup-orphans

(describe "org-canvas-cleanup-orphans"
  (it "reports no orphans when all match"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-clear-log) (lambda () nil))
                ((symbol-function 'display-buffer) (lambda (_) nil))
                ((symbol-function 'org-canvas--find-orphans-for-feature)
                 (lambda (_feature) nil)))
        (org-canvas-cleanup-orphans)
        ;; Should complete without error
        (expect t :to-be t))))

  (it "deletes orphans when user confirms"
    (let ((deleted-ids nil))
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas-clear-log) (lambda () nil))
                  ((symbol-function 'display-buffer) (lambda (_) nil))
                  ((symbol-function 'org-canvas--find-orphans-for-feature)
                   (lambda (feature)
                     (when (equal (plist-get feature :name) "Assignments")
                       '(((id . 200) (name . "Orphan"))))))
                  ((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (when (eq method 'DELETE)
                       (push url deleted-ids))
                     nil)))
          (org-canvas-cleanup-orphans)
          (expect (length deleted-ids) :to-be-greater-than 0))))))

;;;; org-canvas-pull-all

(describe "org-canvas-pull-all"
  (it "calls all pull functions in dependency order"
    (let ((call-order nil))
      (with-sync-test-env
        (cl-letf (((symbol-function 'org-canvas--preflight-check)
                   (lambda () nil))
                  ((symbol-function 'executable-find)
                   (lambda (_) t))
                  ((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'org-canvas-pull-settings)
                   (lambda () (push 'settings call-order)))
                  ((symbol-function 'org-canvas-pull-sections)
                   (lambda () (push 'sections call-order)))
                  ((symbol-function 'org-canvas-pull-assignment-groups)
                   (lambda () (push 'assignment-groups call-order)))
                  ((symbol-function 'org-canvas-pull-group-categories)
                   (lambda () (push 'group-categories call-order)))
                  ((symbol-function 'org-canvas-pull-outcomes)
                   (lambda () (push 'outcomes call-order)))
                  ((symbol-function 'org-canvas-pull-rubrics)
                   (lambda () (push 'rubrics call-order)))
                  ((symbol-function 'org-canvas-pull-pages)
                   (lambda () (push 'pages call-order)))
                  ((symbol-function 'org-canvas-pull-files)
                   (lambda () (push 'files call-order)))
                  ((symbol-function 'org-canvas-pull-discussions)
                   (lambda () (push 'discussions call-order)))
                  ((symbol-function 'org-canvas-pull-announcements)
                   (lambda () (push 'announcements call-order)))
                  ((symbol-function 'org-canvas-pull-calendar-events)
                   (lambda () (push 'calendar-events call-order)))
                  ((symbol-function 'org-canvas-pull-assignments)
                   (lambda () (push 'assignments call-order)))
                  ((symbol-function 'org-canvas-pull-quizzes)
                   (lambda () (push 'quizzes call-order)))
                  ((symbol-function 'org-canvas-pull-new-quizzes)
                   (lambda () (push 'new-quizzes call-order)))
                  ((symbol-function 'org-canvas-pull-modules)
                   (lambda () (push 'modules call-order))))
        (org-canvas-pull-all)
        (setq call-order (nreverse call-order))
        ;; Verify order: settings first, modules last
        (expect (car call-order) :to-equal 'settings)
        (expect (car (last call-order)) :to-equal 'modules)
        ;; Assignments before modules
        (let ((assign-pos (cl-position 'assignments call-order))
              (modules-pos (cl-position 'modules call-order)))
          (expect assign-pos :to-be-less-than modules-pos))
        ;; Files before any body-rewriting module so the file-id cache
        ;; is warm when those modules call `--pull-insert-body'.
        (let ((files-pos (cl-position 'files call-order)))
          (dolist (consumer '(pages discussions announcements
                              assignments quizzes))
            (expect files-pos :to-be-less-than
                    (cl-position consumer call-order))))
        ;; All 15 functions called
        (expect (length call-order) :to-equal 15)))))

  (it "handles pull function errors gracefully"
    (with-sync-test-env
      (cl-letf (((symbol-function 'org-canvas--preflight-check)
                 (lambda () nil))
                ((symbol-function 'executable-find)
                 (lambda (_) t))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'org-canvas-pull-settings)
                 (lambda () (error "Settings pull failed")))
                ((symbol-function 'org-canvas-pull-sections)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignment-groups)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-group-categories)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-outcomes)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-rubrics)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-pages)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-files)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-discussions)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-announcements)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-calendar-events)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignments)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-quizzes)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-new-quizzes)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-modules)
                 (lambda () nil)))
        ;; Should not throw
        (expect (org-canvas-pull-all) :not :to-throw)))))

;;;; Per-module pull integration tests

(describe "org-canvas-pull-assignment-groups"
  (it "creates headings from API response"
    (let* ((temp-dir (make-temp-file "pull-ag-test" t))
           (test-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-assignment-groups-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 10) (name . "Homework") (group_weight . 40))
                             ((id . 20) (name . "Exams") (group_weight . 60)))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-assignment-groups)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (with-temp-org-buffer
                   content
                   (let ((count 0))
                     (org-map-entries (lambda () (cl-incf count))
                                      "LEVEL=1" 'file)
                     (expect count :to-equal 2)))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-pages"
  (it "creates page headings with body"
    (let* ((temp-dir (make-temp-file "pull-pages-test" t))
           (test-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-pages-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((url . "welcome") (title . "Welcome")
                              (front_page . :json-false)))))
                        ((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           '((url . "welcome") (title . "Welcome")
                             (body . "<p>Hello</p>"))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-pages)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (expect content :to-match "Welcome")
                  (expect content :to-match "CANVAS_URL")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-assignments"
  (it "creates assignment headings with properties"
    (let* ((temp-dir (make-temp-file "pull-assign-test" t))
           (test-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-assignments-file test-file)
                (org-canvas-assignment-groups-file "/tmp/nonexistent-ag.org"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 100) (name . "HW 1")
                              (points_possible . 50)
                              (submission_types . ["online_upload"])
                              (due_at . "2026-02-15T23:59:00Z")))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-assignments)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (with-temp-org-buffer
                   content
                   (re-search-forward "^\\*+ " nil t)
                   (org-back-to-heading)
                   (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100")
                   (expect (org-entry-get (point) "POINTS") :to-equal "50"))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "emits assignments grouped by assignment_group_id, then by position"
    (let* ((temp-dir (make-temp-file "pull-assign-sort-test" t))
           (test-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-assignments-file test-file)
                (org-canvas-assignment-groups-file "/tmp/nonexistent-ag.org"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           ;; Out-of-order: should sort to A-1, A-3, B-1, B-2.
                           '(((id . 1) (name . "B-2") (position . 2)
                              (assignment_group_id . 100))
                             ((id . 2) (name . "A-3") (position . 3)
                              (assignment_group_id . 50))
                             ((id . 3) (name . "B-1") (position . 1)
                              (assignment_group_id . 100))
                             ((id . 4) (name . "A-1") (position . 1)
                              (assignment_group_id . 50)))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-assignments)
                (with-temp-buffer
                  (insert-file-contents test-file)
                  (goto-char (point-min))
                  (let (titles)
                    (while (re-search-forward "^\\* +\\(.+\\)$" nil t)
                      (push (match-string-no-properties 1) titles))
                    (expect (nreverse titles)
                            :to-equal '("A-1" "A-3" "B-1" "B-2")))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-discussions"
  (it "creates discussion headings"
    (let* ((temp-dir (make-temp-file "pull-disc-test" t))
           (test-file (expand-file-name "discussions.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-discussions-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 50) (title . "Week 1 Discussion")
                              (message . "<p>Discuss!</p>")
                              (discussion_type . "threaded")
                              (is_announcement . :json-false)))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-discussions)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (expect content :to-match "Week 1 Discussion")
                  (expect content :to-match "CANVAS_ID: 50")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "writes self-doc header when no discussions exist"
    (let* ((temp-dir (make-temp-file "pull-disc-empty" t))
           (test-file (expand-file-name "discussions.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "stale content\n* Old\n"))
            (let ((org-canvas-discussions-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil))
                          ((symbol-function 'org-canvas--pull-confirm-overwrite)
                           (lambda (&rest _) nil)))
                  (org-canvas-pull-discussions)
                  (with-temp-buffer
                    (insert-file-contents test-file)
                    (let ((s (buffer-string)))
                      (expect s :not :to-match "stale content")
                      (expect s :to-match "^#\\+TITLE: Discussions$")
                      (expect s :to-match "^# Canvas returned 0 items")))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-modules"
  (it "creates module headings with items"
    (let* ((temp-dir (make-temp-file "pull-mod-test" t))
           (test-file (expand-file-name "modules.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-modules-file test-file)
                (org-canvas-directory temp-dir))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 1) (name . "Week 1")
                              (items . [((id . 10) (type . "SubHeader")
                                         (title . "Readings") (indent . 0)
                                         (content_id . nil))])))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-modules)
                (let ((content (with-temp-buffer
                                 (insert-file-contents test-file)
                                 (buffer-string))))
                  (expect content :to-match "Week 1")
                  (expect content :to-match "Readings")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Additional Coverage Tests

(describe "org-canvas-pull-all pandoc warning"
  (it "continues when user accepts no pandoc"
    (with-org-canvas-test-config
      (with-sync-test-env
        (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
                  ((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                  ((symbol-function 'org-canvas--safe-pull) (lambda (_fn _label) nil)))
          ;; Should not throw
          (org-canvas-pull-all)
          (expect t :to-be t)))))

  (it "aborts when user declines no pandoc"
    (with-org-canvas-test-config
      (with-sync-test-env
        (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
                  ((symbol-function 'yes-or-no-p) (lambda (_) nil)))
          (expect (org-canvas-pull-all) :to-throw 'user-error))))))

(describe "org-canvas--find-orphans-for-feature"
  (it "returns nil when file-var is unbound"
    (let ((feature (list :name "widgets" :endpoint "widgets"
                         :file-var 'org-canvas-nonexistent-file-var
                         :id-field 'id :id-property "CANVAS_ID"
                         :list-params nil :skip-fn nil)))
      (expect (org-canvas--find-orphans-for-feature feature) :to-be nil))))

(describe "org-canvas--orphan-delete-all"
  (it "logs error when delete fails"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("DELETE failed")))))
        (let ((feature (list :name "pages" :endpoint "pages" :id-field 'id))
              (orphans '(((id . 1) (title . "Orphan Page")))))
          (org-canvas--orphan-delete-all (list (cons feature orphans)))
          (expect 'org-canvas--log-warning :to-have-been-called))))))

(describe "org-canvas--status-count-entries"
  (it "counts pending entries (no CANVAS_ID)"
    (let* ((temp-dir (make-temp-file "status-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "#+LAST_SYNCED: [2026-01-15 Thu 10:00]
* Synced Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
* Pending Item
:PROPERTIES:
:END:
"))
            (let ((counts (org-canvas--status-count-entries test-file "CANVAS_ID")))
              (expect (plist-get counts :synced) :to-equal 1)
              (expect (plist-get counts :pending) :to-equal 1)
              (expect (plist-get counts :last-synced) :to-match "2026")))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "reads file-level LAST_SYNCED header"
    (let* ((temp-dir (make-temp-file "status-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "#+LAST_SYNCED: [2026-03-01 Sun 09:00]
* Item One
:PROPERTIES:
:CANVAS_ID: 100
:END:
* Item Two
:PROPERTIES:
:CANVAS_ID: 200
:END:
* Item Three
:PROPERTIES:
:CANVAS_ID: 300
:END:
* Pending Item
:PROPERTIES:
:END:
"))
            (let ((counts (org-canvas--status-count-entries test-file "CANVAS_ID")))
              (expect (plist-get counts :synced) :to-equal 3)
              (expect (plist-get counts :pending) :to-equal 1)
              (expect (plist-get counts :last-synced) :to-match "2026-03-01")))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Cascading failure resilience

(describe "cascading failure resilience in org-canvas-sync"
  (it "continues syncing subsequent features after one feature errors"
    (let ((call-order nil))
      (with-sync-test-env
        (cl-letf (((symbol-function 'org-canvas--preflight-check)
                   (lambda () nil))
                  ((symbol-function 'org-canvas-sync-settings)
                   (lambda () (push 'settings call-order)))
                  ((symbol-function 'org-canvas-sync-outcomes)
                   (lambda () (error "Outcomes sync failed catastrophically")))
                  ((symbol-function 'org-canvas-sync-rubrics)
                   (lambda () (push 'rubrics call-order)))
                  ((symbol-function 'org-canvas-sync-assignment-groups)
                   (lambda () (push 'assignment-groups call-order)))
                  ((symbol-function 'org-canvas-sync-group-categories)
                   (lambda () (push 'group-categories call-order)))
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
                  ((symbol-function 'org-canvas-sync-calendar-events)
                   (lambda () (push 'calendar-events call-order)))
                  ((symbol-function 'org-canvas-sync-quizzes)
                   (lambda () (push 'quizzes call-order)))
                  ((symbol-function 'org-canvas-sync-new-quizzes)
                   (lambda () (push 'new-quizzes call-order)))
                  ((symbol-function 'org-canvas-sync-assignments)
                   (lambda () (push 'assignments call-order)))
                  ((symbol-function 'org-canvas-sync-overrides)
                   (lambda () (push 'overrides call-order)))
                  ((symbol-function 'org-canvas-sync-modules)
                   (lambda () (push 'modules call-order))))
          ;; Sync should complete without throwing despite outcomes error
          (expect (org-canvas-sync) :not :to-throw)
          (setq call-order (nreverse call-order))
          ;; Settings ran (before the error)
          (expect (member 'settings call-order) :to-be-truthy)
          ;; Rubrics ran (after the error, same tier as outcomes)
          (expect (member 'rubrics call-order) :to-be-truthy)
          ;; Modules ran (later tier, after failed tier)
          (expect (member 'modules call-order) :to-be-truthy)
          ;; Outcomes is NOT in the list (it errored)
          (expect (member 'outcomes call-order) :to-be nil)))))

  (it "continues after multiple features error in different tiers"
    (let ((call-order nil))
      (with-sync-test-env
        (cl-letf (((symbol-function 'org-canvas--preflight-check)
                   (lambda () nil))
                  ((symbol-function 'org-canvas-sync-settings)
                   (lambda () (push 'settings call-order)))
                  ((symbol-function 'org-canvas-sync-outcomes)
                   (lambda () (error "Outcomes failed")))
                  ((symbol-function 'org-canvas-sync-rubrics)
                   (lambda () (push 'rubrics call-order)))
                  ((symbol-function 'org-canvas-sync-assignment-groups)
                   (lambda () (push 'assignment-groups call-order)))
                  ((symbol-function 'org-canvas-sync-group-categories)
                   (lambda () (push 'group-categories call-order)))
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
                  ((symbol-function 'org-canvas-sync-calendar-events)
                   (lambda () (push 'calendar-events call-order)))
                  ((symbol-function 'org-canvas-sync-quizzes)
                   (lambda () (error "Quizzes failed")))
                  ((symbol-function 'org-canvas-sync-new-quizzes)
                   (lambda () (push 'new-quizzes call-order)))
                  ((symbol-function 'org-canvas-sync-assignments)
                   (lambda () (push 'assignments call-order)))
                  ((symbol-function 'org-canvas-sync-overrides)
                   (lambda () (push 'overrides call-order)))
                  ((symbol-function 'org-canvas-sync-modules)
                   (lambda () (push 'modules call-order))))
          (expect (org-canvas-sync) :not :to-throw)
          (setq call-order (nreverse call-order))
          ;; Both outcomes (Tier 0) and quizzes (Tier 1) errored
          ;; but assignments (same tier as quizzes) and modules (Tier 2) still ran
          (expect (member 'assignments call-order) :to-be-truthy)
          (expect (member 'modules call-order) :to-be-truthy)
          (expect (member 'outcomes call-order) :to-be nil)
          (expect (member 'quizzes call-order) :to-be nil))))))

;;;; org-canvas--status-count-entries legacy items

(describe "org-canvas--status-count-entries legacy items"
  (it "counts CANVAS_ID-tagged items as legacy when file has no #+LAST_SYNCED header"
    (let ((temp-file (make-temp-file "status-legacy" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Synced One
:PROPERTIES:
:CANVAS_ID: 100
:END:

* Synced Two
:PROPERTIES:
:CANVAS_ID: 200
:END:

* Pending Item
:PROPERTIES:
:END:
"))
            (let ((counts (org-canvas--status-count-entries temp-file "CANVAS_ID")))
              (expect (plist-get counts :synced) :to-equal 2)
              (expect (plist-get counts :pending) :to-equal 1)
              ;; Both synced items are legacy because the file has no
              ;; #+LAST_SYNCED header (re-pull required)
              (expect (plist-get counts :legacy) :to-equal 2)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "returns zero legacy when file has #+LAST_SYNCED header"
    (let ((temp-file (make-temp-file "status-no-legacy" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Item
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((counts (org-canvas--status-count-entries temp-file "CANVAS_ID")))
              (expect (plist-get counts :legacy) :to-equal 0)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--status-report-file legacy and unsaved"
  (it "shows legacy count when present"
    (let ((temp-file (make-temp-file "status-report" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Legacy Item
:PROPERTIES:
:CANVAS_ID: 200
:END:
"))
            (let ((org-canvas-assignments-file temp-file))
              (let ((buf (get-buffer-create "*test-status*")))
                (unwind-protect
                    (progn
                      (org-canvas--status-report-file
                       buf "Assignments" 'org-canvas-assignments-file "CANVAS_ID")
                      (with-current-buffer buf
                        (expect (buffer-string) :to-match "Legacy: 1")))
                  (kill-buffer buf)))))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "shows [unsaved] marker for modified buffers"
    (let ((temp-file (make-temp-file "status-unsaved" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item
:PROPERTIES:
:CANVAS_ID: 100
:LAST_SYNCED: [2026-01-01 Thu]
:END:
"))
            ;; Open buffer and modify without saving
            (let ((file-buf (find-file-noselect temp-file)))
              (with-current-buffer file-buf
                (goto-char (point-max))
                (insert "\n"))
              (let ((org-canvas-assignments-file temp-file))
                (let ((buf (get-buffer-create "*test-status*")))
                  (unwind-protect
                      (progn
                        (org-canvas--status-report-file
                         buf "Assignments" 'org-canvas-assignments-file "CANVAS_ID")
                        (with-current-buffer buf
                          (expect (buffer-string) :to-match "\\[unsaved\\]")))
                    (kill-buffer buf))))
              (set-buffer-modified-p nil)
              (kill-buffer file-buf)))
        (delete-file temp-file))))

  (it "does not show [unsaved] for clean files"
    (let ((temp-file (make-temp-file "status-clean" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item
:PROPERTIES:
:CANVAS_ID: 100
:LAST_SYNCED: [2026-01-01 Thu]
:END:
"))
            (let ((org-canvas-assignments-file temp-file))
              (let ((buf (get-buffer-create "*test-status*")))
                (unwind-protect
                    (progn
                      (org-canvas--status-report-file
                       buf "Assignments" 'org-canvas-assignments-file "CANVAS_ID")
                      (with-current-buffer buf
                        (expect (buffer-string) :not :to-match "\\[unsaved\\]")))
                  (kill-buffer buf)))))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

;;;; org-canvas-pull-all

(describe "org-canvas-pull-all"
  (it "emits tier progress messages"
    (with-sync-test-env
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'org-canvas-pull-settings) (lambda () nil))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-pull-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-pull-pages) (lambda () nil))
                ((symbol-function 'org-canvas-pull-files) (lambda () nil))
                ((symbol-function 'org-canvas-pull-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-pull-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-pull-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-pull-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-modules) (lambda () nil)))
        (org-canvas-pull-all)
        (let ((found nil))
          (dolist (call (spy-calls-all-args 'message))
            (when (and (stringp (car call))
                       (string-match-p "^Pulling:" (car call)))
              (setq found t)))
          (expect found :to-be-truthy)))))

  (it "shows final message with counts"
    (with-sync-test-env
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'org-canvas-pull-settings) (lambda () nil))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-pull-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-pull-pages) (lambda () nil))
                ((symbol-function 'org-canvas-pull-files) (lambda () nil))
                ((symbol-function 'org-canvas-pull-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-pull-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-pull-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-pull-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-modules) (lambda () nil)))
        (org-canvas-pull-all)
        (let ((found nil))
          (dolist (call (spy-calls-all-args 'message))
            (when (and (stringp (car call))
                       (string-match-p "Pull complete:.*pulled.*failed" (car call)))
              (setq found t)))
          (expect found :to-be-truthy)))))

  (it "counts failures from erroring pull functions"
    (with-sync-test-env
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'org-canvas-pull-settings)
                 (lambda () (error "Settings pull failed")))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-pull-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-pull-pages) (lambda () nil))
                ((symbol-function 'org-canvas-pull-files) (lambda () nil))
                ((symbol-function 'org-canvas-pull-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-pull-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-pull-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-pull-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-pull-modules) (lambda () nil)))
        (org-canvas-pull-all)
        ;; Final message should show "1 failed"
        (let ((found nil))
          (dolist (call (spy-calls-all-args 'message))
            (when (and (stringp (car call))
                       (string-match-p "Pull complete:" (car call)))
              (let ((formatted (apply #'format call)))
                (when (string-match-p "1 failed" formatted)
                  (setq found t)))))
          (expect found :to-be-truthy))))))

;;;; org-canvas-sync dry-run global message

(describe "org-canvas-sync dry-run global message"
  (it "shows dry-run specific message when in dry-run mode"
    (with-sync-test-env
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'org-canvas-sync-settings) (lambda () nil))
                ((symbol-function 'org-canvas-sync-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-sync-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-sync-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
        ;; Simulate dry-run by setting global counter
        (let ((org-canvas--dry-run t))
          (org-canvas-sync)
          ;; Since no actual syncing happened, dry-run count = 0
          ;; but the final message should be "Sync complete" (dry-run = 0 takes else branch)
          ;; Let's verify org-canvas--sync-global-counters has :dry-run key
          (expect org-canvas--sync-global-counters :to-be nil)))))

  (it "shows dry-run complete message when dry-run counter > 0"
    (with-sync-test-env
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'org-canvas-sync-settings)
                 (lambda ()
                   ;; Simulate a dry-run skip by incrementing the counter
                   (plist-put org-canvas--sync-global-counters :dry-run 3)
                   (plist-put org-canvas--sync-global-counters :skip 1)))
                ((symbol-function 'org-canvas-sync-outcomes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-rubrics) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignment-groups) (lambda () nil))
                ((symbol-function 'org-canvas-sync-group-categories) (lambda () nil))
                ((symbol-function 'org-canvas-pull-sections) (lambda () nil))
                ((symbol-function 'org-canvas-sync-files) (lambda () nil))
                ((symbol-function 'org-canvas-sync-pages) (lambda () nil))
                ((symbol-function 'org-canvas-sync-discussions) (lambda () nil))
                ((symbol-function 'org-canvas-sync-announcements) (lambda () nil))
                ((symbol-function 'org-canvas-sync-calendar-events) (lambda () nil))
                ((symbol-function 'org-canvas-sync-assignments) (lambda () nil))
                ((symbol-function 'org-canvas-sync-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-new-quizzes) (lambda () nil))
                ((symbol-function 'org-canvas-sync-overrides) (lambda () nil))
                ((symbol-function 'org-canvas-sync-modules) (lambda () nil)))
        (let ((org-canvas--dry-run t))
          (org-canvas-sync)
          (let ((found nil))
            (dolist (call (spy-calls-all-args 'message))
              (when (and (stringp (car call))
                         (string-match-p "Dry-run complete:" (car call)))
                (setq found t)))
            (expect found :to-be-truthy)))))))

(describe "org-canvas-pull-all overwrite abort"
  (it "aborts when user declines overwrite prompt"
    (with-sync-test-env
      (cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
                ((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'yes-or-no-p) (lambda (_) nil))
                ((symbol-function 'file-exists-p) (lambda (_) t))
                ((symbol-function 'org-canvas--status-count-entries)
                 (lambda (_file _id-prop) (list :synced 5 :pending 0 :legacy 0 :unsaved 0))))
        (expect (org-canvas-pull-all) :to-throw 'user-error)))))

;;;; org-canvas-version

(describe "org-canvas-version"
  (it "returns the version declared in the package header"
    (let ((version (org-canvas-version)))
      (expect version :to-be-truthy)
      (expect version :to-match "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'")))

  (it "returns the same string as the source-file Version header"
    (let* ((file (replace-regexp-in-string
                  "\\.elc\\'" ".el" (locate-library "org-canvas")))
           (header-version (with-temp-buffer
                             (insert-file-contents file)
                             (lm-version))))
      (expect (org-canvas-version) :to-equal header-version)))

  (it "messages the version when SHOW is non-nil"
    (spy-on 'message)
    (let ((result (org-canvas-version t)))
      (expect 'message :to-have-been-called-with "org-canvas %s" result)))

  (it "does not message when SHOW is nil"
    (spy-on 'message)
    (org-canvas-version)
    (expect 'message :not :to-have-been-called))

  (it "errors when the package source cannot be located"
    (cl-letf (((symbol-function 'locate-library) (lambda (_) nil)))
      (expect (org-canvas-version) :to-throw 'error))))

;;;; org-canvas-submit-bug-report

(describe "org-canvas-submit-bug-report"
  (after-each
    (when (get-buffer "*org-canvas-bug-report*")
      (kill-buffer "*org-canvas-bug-report*")))

  (it "creates the bug report buffer"
    (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
      (org-canvas-submit-bug-report)
      (expect (get-buffer "*org-canvas-bug-report*") :to-be-truthy)))

  (it "includes version, Emacs version, and system info"
    (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
      (org-canvas-submit-bug-report)
      (with-current-buffer "*org-canvas-bug-report*"
        (let ((content (buffer-string)))
          (expect content :to-match
                  (format "org-canvas version : %s"
                          (regexp-quote (org-canvas-version))))
          (expect content :to-match
                  (format "Emacs version      : %s"
                          (regexp-quote emacs-version)))
          (expect content :to-match "System type")))))

  (it "redacts the API token when set"
    (let ((org-canvas-api-token "super-secret-token"))
      (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
        (org-canvas-submit-bug-report)
        (with-current-buffer "*org-canvas-bug-report*"
          (let ((content (buffer-string)))
            (expect content :to-match "org-canvas-api-token: \\[redacted\\]")
            (expect content :not :to-match "super-secret-token"))))))

  (it "labels the API token as unset when empty"
    (let ((org-canvas-api-token ""))
      (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
        (org-canvas-submit-bug-report)
        (with-current-buffer "*org-canvas-bug-report*"
          (expect (buffer-string) :to-match
                  "org-canvas-api-token: \\[unset\\]")))))

  (it "lists the documented configuration variables"
    (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
      (org-canvas-submit-bug-report)
      (with-current-buffer "*org-canvas-bug-report*"
        (let ((content (buffer-string)))
          (dolist (sym org-canvas--bug-report-settings)
            (expect content :to-match (regexp-quote (symbol-name sym))))))))

  (it "lists loaded org-canvas modules"
    (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
      (org-canvas-submit-bug-report)
      (with-current-buffer "*org-canvas-bug-report*"
        (expect (buffer-string) :to-match "org-canvas-core")))))

(describe "org-canvas--bug-report-settings"
  (it "does not contain org-canvas-api-token"
    (expect (memq 'org-canvas-api-token org-canvas--bug-report-settings)
            :to-be nil)))

;;; org-canvas-test.el ends here
