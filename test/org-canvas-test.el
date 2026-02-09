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
                  ((symbol-function 'org-canvas-sync-settings) (lambda () nil))
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
                  ((symbol-function 'org-canvas-pull-settings)
                   (lambda () (push 'settings call-order)))
                  ((symbol-function 'org-canvas-pull-sections)
                   (lambda () (push 'sections call-order)))
                  ((symbol-function 'org-canvas-pull-assignment-groups)
                   (lambda () (push 'assignment-groups call-order)))
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
                  ((symbol-function 'org-canvas-pull-assignments)
                   (lambda () (push 'assignments call-order)))
                  ((symbol-function 'org-canvas-pull-quizzes)
                   (lambda () (push 'quizzes call-order)))
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
        ;; All 12 functions called
        (expect (length call-order) :to-equal 12)))))

  (it "handles pull function errors gracefully"
    (with-sync-test-env
      (cl-letf (((symbol-function 'org-canvas--preflight-check)
                 (lambda () nil))
                ((symbol-function 'executable-find)
                 (lambda (_) t))
                ((symbol-function 'org-canvas-pull-settings)
                 (lambda () (error "Settings pull failed")))
                ((symbol-function 'org-canvas-pull-sections)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-assignment-groups)
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
                ((symbol-function 'org-canvas-pull-assignments)
                 (lambda () nil))
                ((symbol-function 'org-canvas-pull-quizzes)
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

;;; org-canvas-test.el ends here
