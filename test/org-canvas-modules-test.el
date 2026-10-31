;;; org-canvas-modules-test.el --- Buttercup tests for Canvas modules  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; lisp/org-canvas-modules.el: the module's own pipeline (extraction,
;; transformation, push, finalization), bulk publish, the payload
;; digest and the sync, delete and pull commands.  The item layer's
;; specs are in org-canvas-module-items-test.el.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-modules)

(describe "org-canvas--module-parse-prerequisite-ids"
  (it "parses comma-separated IDs"
    (expect (org-canvas--module-parse-prerequisite-ids "123, 456, 789")
            :to-equal '("123" "456" "789")))

  (it "returns nil for nil prerequisite-ids input"
    (expect (org-canvas--module-parse-prerequisite-ids nil) :to-be nil)))

;;;; Transform Functions

(describe "org-canvas--module-transform-props"
  (it "strips statistics cookie from title"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Week 1 [1/3]" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :title) :to-equal "Week 1")))

  (it "interprets published true by default"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :published) :to-be t)))

  (it "interprets published false"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw "false" :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :published) :to-be nil)))

  (it "converts position to number and omits zero"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw "3"
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :position) :to-equal 3)))

  (it "omits position when zero or nil"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :position) :to-be nil)))

  (it "passes through prerequisite-module-ids"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids "100, 200"
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :prerequisite-module-ids) :to-equal '("100" "200"))))

  (it "interprets require-sequential-progress boolean"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw "true" :publish-final-grade-raw nil))))
      (expect (plist-get data :require-sequential-progress) :to-be t)))

  (it "interprets publish-final-grade boolean"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw "true"))))
      (expect (plist-get data :publish-final-grade) :to-be t)))

  (it "passes through canvas-id"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id "999"
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :canvas-id) :to-equal "999"))))

;;;; Stage 1: Parse Module Entry

(describe "org-canvas--module-parse-entry"
  (it "extracts module title from heading"
    (with-temp-org-buffer
     "* Week 1 - Introduction
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :title) :to-equal "Week 1 - Introduction"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:PUBLISHED: true\n:END:\n")
     (org-back-to-heading)
     (expect (org-canvas--module-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 66666
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "66666"))))

  (it "parses published property"
    (with-temp-org-buffer
     "* Hidden Module
:PROPERTIES:
:PUBLISHED: false
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :published) :to-be nil))))

  (it "defaults published to true when missing"
    (with-temp-org-buffer
     "* Module Without Published
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :published) :to-be t))))

  (it "parses position property"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:PUBLISHED: true
:POSITION: 3
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :position) :to-equal 3))))

  (it "returns nil for position when zero or missing"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:PUBLISHED: true
:POSITION: 0
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :position) :to-be nil))))

  (it "parses require_sequential_progression"
    (with-temp-org-buffer
     "* Sequential Module
:PROPERTIES:
:PUBLISHED: true
:REQUIRE_SEQUENTIAL_PROGRESSION: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :require-sequential-progress) :to-be t))))

  (it "parses prerequisite_module_ids"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:PUBLISHED: true
:PREREQUISITE_MODULE_IDS: 100, 101
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :prerequisite-module-ids) :to-equal '("100" "101")))))

  (it "returns nil for unlock_at when not specified"
    (with-temp-org-buffer
     "* Module Without Unlock
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :unlock-at) :to-be nil))))

  (it "parses publish_final_grade"
    (with-temp-org-buffer
     "* Final Module
:PROPERTIES:
:PUBLISHED: true
:PUBLISH_FINAL_GRADE: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :publish-final-grade) :to-be t))))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy)))))

;;;; Stage 2: Build Module Payload

(describe "org-canvas--module-build-payload"
  (it "wraps in module key"
    (let* ((data '(:title "Test Module" :published t))
           (payload (org-canvas--module-build-payload data)))
      (expect (gethash "module" payload) :to-be-truthy)))

  (it "includes name in payload"
    (let* ((data '(:title "Week 1" :published t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "name" module) :to-equal "Week 1")))

  (it "includes published status as t"
    (let* ((data '(:title "Test" :published t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "published" module) :to-be t)))

  (it "includes published status as :json-false when unpublished"
    (let* ((data '(:title "Test" :published nil))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "published" module) :to-equal :json-false)))

  (it "sends skip_content_tags so the publish state stops at the module"
    ;; Issue #47: without it, Canvas runs publish_items!/unpublish_items!
    ;; over every item and rewrites the publish state of the content each
    ;; one points at, overriding PUBLISHED in the file that owns it.
    (let* ((data '(:title "Week 1" :published t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "skip_content_tags" module) :to-be t)))

  (it "sends skip_content_tags when unpublishing too"
    (let* ((data '(:title "Week 1" :published nil))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "skip_content_tags" module) :to-be t)))

  (it "includes position when specified"
    (let* ((data '(:title "Test" :published t :position 5))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "position" module) :to-equal 5)))

  (it "excludes position when not specified"
    (let* ((data '(:title "Test" :published t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "position" module) :to-be nil)))

  (it "includes prerequisite_module_ids"
    (let* ((data '(:title "Test" :published t :prerequisite-module-ids ("100" "101")))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "prerequisite_module_ids" module) :to-equal '("100" "101"))))

  (it "includes unlock_at when specified"
    (let* ((data '(:title "Test" :published t :unlock-at "2024-02-01T08:00:00Z"))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "unlock_at" module) :to-equal "2024-02-01T08:00:00Z")))

  (it "includes require_sequential_progress when true"
    (let* ((data '(:title "Test" :published t :require-sequential-progress t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "require_sequential_progress" module) :to-be t)))

  (it "excludes require_sequential_progress when nil"
    (let* ((data '(:title "Test" :published t :require-sequential-progress nil))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "require_sequential_progress" module) :to-be nil)))

  (it "includes publish_final_grade when true"
    (let* ((data '(:title "Test" :published t :publish-final-grade t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "publish_final_grade" module) :to-be t))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--module-push-to-api (mocked)"
  (it "uses POST for new modules"
    (with-test-api
      (let ((data '(:title "New" :canvas-id nil))
            (payload (make-hash-table)))
        (org-canvas--module-push-to-api data payload)
        (expect-api-called 'POST "modules$"))))

  (it "uses PUT for existing modules"
    (with-test-api
      (let ((data '(:title "Existing" :canvas-id "555"))
            (payload (make-hash-table)))
        (org-canvas--module-push-to-api data payload)
        (expect-api-called 'PUT "modules/555"))))

  (it "recovers from timeout by searching for created module"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that times out
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Request failed" "Timeout")))
                      ;; Second call: recovery search finds the module
                      ((eq method 'GET)
                       [((id . 999) (name . "Timeout Module"))])
                      (t nil)))))
          (let ((data '(:title "Timeout Module" :canvas-id nil))
                (payload (make-hash-table)))
            (let ((result (org-canvas--module-push-to-api data payload)))
              (expect (alist-get 'id result) :to-equal 999)))))))

  (it "retries as POST when PUT returns 404"
    (with-org-canvas-test-config
      (let ((call-count 0)
            (post-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: PUT that returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("404 Not Found")))
                      ;; Second call: POST succeeds
                      ((eq method 'POST)
                       (setq post-called t)
                       '((id . 888) (name . "Recovered Module")))
                      (t nil)))))
          (let ((data '(:title "Missing Module" :canvas-id "999"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--module-push-to-api data payload)))
              (expect post-called :to-be t)
              (expect (alist-get 'id result) :to-equal 888)))))))

  (it "signals error when timeout recovery finds no module"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that times out
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Request failed" "Timeout")))
                      ;; Second call: recovery search finds nothing
                      ((eq method 'GET)
                       [])
                      (t nil)))))
          (let ((data '(:title "Lost Module" :canvas-id nil))
                (payload (make-hash-table)))
            (expect (org-canvas--module-push-to-api data payload)
                    :to-throw 'error))))))

  (it "signals non-timeout/non-404 errors"
    (with-test-api-handler (lambda (_method _url &rest _args)
                             (signal 'error '("Bad Request: Invalid module")))
      (let ((data '(:title "Bad Module" :canvas-id nil))
            (payload (make-hash-table)))
        (expect (org-canvas--module-push-to-api data payload)
                :to-throw 'error)))))

;;;; Stage 4: Finalize

(describe "org-canvas--module-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Module
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Module" :pom (point-marker)))
           (response '((id . 88888) (name . "Test Module"))))
       (org-canvas--module-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "88888"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 77777))))
       (org-canvas--module-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* No ID Module
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No ID Module" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (expect (org-canvas--module-finalize data response) :to-throw 'error))))

  (it "handles numeric id in response"
    (with-temp-org-buffer
     "* Numeric ID Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Numeric ID Test" :pom (point-marker)))
           (response '((id . 12345))))
       (org-canvas--module-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345")))))

;;;; Helper Functions

(describe "org-canvas--module-search-by-name (mocked)"
  (it "searches modules by name"
    (with-test-api-responses '(("modules" . [((id . 100) (name . "Week 1"))
                                              ((id . 101) (name . "Week 2"))]))
      (org-canvas--module-search-by-name "Week 1")
      (expect-api-called 'GET "modules")))

  (it "returns matching module"
    (with-test-api-responses '(("modules" . [((id . 100) (name . "Week 1"))
                                              ((id . 101) (name . "Week 2"))]))
      (let ((result (org-canvas--module-search-by-name "Week 1")))
        (expect (alist-get 'id result) :to-equal 100))))

  (it "returns nil when no match found"
    (with-test-api-responses '(("modules" . [((id . 100) (name . "Week 1"))]))
      (let ((result (org-canvas--module-search-by-name "Nonexistent Module")))
        (expect result :to-be nil))))

  (it "returns nil on API error"
    (with-test-api-handler (lambda (_method _url &rest _args)
                             (signal 'error '("API Error")))
      (let ((result (org-canvas--module-search-by-name "Any Module")))
        (expect result :to-be nil)))))

;;;; Delete Functions (mocked)

(describe "org-canvas-delete-module-at-point"
  (it "errors when no CANVAS_ID property found"
    (with-temp-org-buffer
     "* Module Without ID
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-module-at-point)
             :to-throw 'user-error))))

;;;; Delete All Modules Tests

(describe "org-canvas-delete-all-modules (mocked)"
  (it "deletes modules and cleans local properties"
    (let ((temp-file (make-temp-file "modules" nil ".org"))
          (org-canvas-modules-file nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Module 1
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:

* Module 2
:PROPERTIES:
:CANVAS_ID: 2
:END:
"))
            (setq org-canvas-modules-file temp-file)
            (with-org-canvas-test-config
              (let ((deleted-ids nil))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             [((id . 1) (name . "Module 1"))
                              ((id . 2) (name . "Module 2"))]))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _options)
                             (dolist (item items)
                               (push (number-to-string (alist-get 'id item)) deleted-ids))
                             (cons (length items) deleted-ids))))
                  (org-canvas-delete-all-modules)
                  ;; Should have deleted 2 modules
                  (expect (length deleted-ids) :to-equal 2)))))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))))

  (it "aborts when user cancels"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (expect (org-canvas-delete-all-modules) :to-throw 'user-error))))

;;;; Delete Module at Point Tests

(describe "org-canvas-delete-module-at-point (mocked)"
  (it "deletes module and clears child item properties"
    (with-test-api
      (with-temp-org-buffer
       "* Module
:PROPERTIES:
:CANVAS_ID: 100
:LAST_SYNCED: [2024-01-01 Mon]
:END:
** Item 1
:PROPERTIES:
:CANVAS_ID: 101
:END:
** Item 2
:PROPERTIES:
:CANVAS_ID: 102
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
         (org-canvas-delete-module-at-point)
         ;; Module properties should be cleared
         (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
         ;; Child item properties should also be cleared
         (org-goto-first-child)
         (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))))

  (it "handles delete API failure gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Cannot delete"))))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
         (org-back-to-heading)
         ;; Should not throw, just message
         (org-canvas-delete-module-at-point)
         ;; Properties should still exist since delete failed
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100")))))

  (it "aborts when user cancels"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
         (org-back-to-heading)
         ;; Should not delete
         (org-canvas-delete-module-at-point)
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100"))))))

;;;; Module Push API Timeout Recovery Failure

(describe "org-canvas--module-push-to-api timeout recovery failure"
  (it "signals error when timeout recovery fails and item not found"
    (with-test-api-handler (lambda (_method _url &rest _args)
                             (signal 'error (list "Request failed" "Timeout")))
      (let ((data '(:title "Lost" :canvas-id nil))
            (payload (make-hash-table)))
        (expect (org-canvas--module-push-to-api data payload)
                :to-throw 'error)))))

;;;; Sync Modules Integration Tests

(describe "org-canvas-sync-modules integration (mocked)"
  (it "syncs module and its items"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-post-count 0)
                 (item-post-count 0))
            (with-temp-file modules-file
              (insert "* Week 1
:PROPERTIES:
:PUBLISHED: true
:END:
** Section Header
:PROPERTIES:
:END:
"))
            (let ((org-canvas-modules-file modules-file))
              (with-test-sync-handler (lambda (method url &rest _args)
                                        (cond
                                          ;; Pre-flight course check
                                          ((string-match "courses/99999/$" url)
                                            '((id . 99999)))
                                          ;; POST module
                                          ((and (eq method 'POST) (string-match "modules$" url))
                                            (setq module-post-count (1+ module-post-count))
                                            '((id . 500) (name . "Week 1")))
                                          ;; POST module item
                                          ((and (eq method 'POST) (string-match "modules/500/items" url))
                                            (setq item-post-count (1+ item-post-count))
                                            '((id . 501) (title . "Section Header")))
                                          ;; PUT
                                          ((eq method 'PUT)
                                            '((id . 500)))
                                          (t '((id . 1)))))
                (org-canvas-sync-modules)
                (expect module-post-count :to-equal 1)
                (with-current-buffer (find-file-noselect modules-file)
                  (goto-char (point-min))
                  (org-back-to-heading)
                  (expect (org-entry-get (point) "CANVAS_ID") :to-equal "500")))))
        (delete-directory temp-dir t))))

  (it "sends a module-create body reflecting the org input"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "modules.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Week 1
:PROPERTIES:
:PUBLISHED: true
:POSITION: 3
:END:
"))
            (let ((org-canvas-modules-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-modules)
                  ;; The module-create POST goes to a URL ending in
                  ;; "modules"; item POSTs go to ".../modules/<id>/items".
                  (let* ((body (test-org-canvas-api-call-data 'POST "modules$"))
                         (module (and (hash-table-p body)
                                      (gethash "module" body))))
                    (expect (hash-table-p body) :to-be-truthy)
                    (expect (hash-table-p module) :to-be-truthy)
                    ;; Real property->API-key mapping: name, published, position.
                    (expect (gethash "name" module) :to-equal "Week 1")
                    (expect (gethash "published" module) :to-be t)
                    (expect (gethash "position" module) :to-equal 3))))))
        (delete-directory temp-dir t))))

  (it "errors when file not found"
    (let ((org-canvas-modules-file "/nonexistent/modules.org"))
      (with-sync-test-env
        (expect (org-canvas-sync-modules) :to-throw 'error))))

  (it "continues when module fails but processes next"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-count 0))
            (with-temp-file modules-file
              (insert "* Module 1
:PROPERTIES:
:PUBLISHED: true
:END:

* Module 2
:PROPERTIES:
:PUBLISHED: true
:END:
"))
            (let ((org-canvas-modules-file modules-file))
              (with-test-sync-handler (lambda (method url &rest _args)
                                        (cond
                                          ((string-match "courses/99999/$" url) '((id . 99999)))
                                          ((and (eq method 'POST) (string-match "modules$" url))
                                            (setq module-count (1+ module-count))
                                            (if (= module-count 1)
                                              (signal 'error '("Module 1 failed"))
                                              '((id . 600) (name . "Module 2"))))
                                          (t '((id . 1)))))
                (org-canvas-sync-modules)
                (expect module-count :to-equal 2))))
        (delete-directory temp-dir t))))

  (it "pre-flight warning does not abort sync"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-count 0))
            (with-temp-file modules-file
              (insert "* Module
:PROPERTIES:
:PUBLISHED: true
:END:
"))
            (let ((org-canvas-modules-file modules-file))
              (with-test-sync-handler (lambda (method url &rest _args)
                                        (cond
                                          ;; Pre-flight fails
                                          ((string-match "courses/99999/$" url)
                                            (signal 'error '("Pre-flight failed")))
                                          ;; Module POST still works
                                          ((and (eq method 'POST) (string-match "modules$" url))
                                            (setq module-count (1+ module-count))
                                            '((id . 700) (name . "Module")))
                                          (t '((id . 1)))))
                (org-canvas-sync-modules)
                (expect module-count :to-equal 1))))
        (delete-directory temp-dir t)))))

;;;; Delete All Modules Edge Cases

(describe "org-canvas-delete-all-modules edge cases (mocked)"
  (it "handles delete error and continues to next"
    (let ((temp-file (make-temp-file "modules" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Module 1
:PROPERTIES:
:CANVAS_ID: 1
:END:
"))
            (let ((org-canvas-modules-file temp-file)
                  (delete-attempts 0))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                            ((symbol-function 'org-canvas-api-request)
                             (lambda (_method _url &rest _args)
                               [((id . 1) (name . "M1")) ((id . 2) (name . "M2"))]))
                            ((symbol-function 'org-canvas--delete-items-queued)
                             (lambda (items _options)
                               (setq delete-attempts (length items))
                               (cons (length items) nil))))
                    (org-canvas-delete-all-modules)
                    ;; Both should have been attempted
                    (expect delete-attempts :to-equal 2))))))
        (when (file-exists-p temp-file) (delete-file temp-file)))))

  (it "cleans local properties for deleted modules"
    (let ((temp-file (make-temp-file "modules" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Module 1
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"))
            (let ((org-canvas-modules-file temp-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                            ((symbol-function 'org-canvas-api-request)
                             (lambda (method _url &rest _args)
                               (cond
                                ((eq method 'GET)
                                 [((id . 1) (name . "Module 1"))])
                                ((eq method 'DELETE) nil)))))
                    (org-canvas-delete-all-modules)
                    ;; CANVAS_ID should be cleared
                    (with-current-buffer (find-file-noselect temp-file)
                      (goto-char (point-min))
                      (org-back-to-heading)
                      (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))))))
        (when (file-exists-p temp-file) (delete-file temp-file))))))

;;;; Module Push 404→POST additional tests

(describe "org-canvas--module-push-to-api 404→POST"
  (it "signals error when POST also fails after 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("404 Not Found")))
                      ;; POST also fails
                      ((eq method 'POST)
                       (signal 'error '("Server Error")))
                      (t nil)))))
          (let ((data '(:title "Module" :canvas-id "999"))
                (payload (make-hash-table)))
            (expect (org-canvas--module-push-to-api data payload)
                    :to-throw 'error)))))))

;;;; Extracted Helper Coverage Tests

(describe "org-canvas--module-resolve-file-path"
  (it "returns absolute path when file exists"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (let* ((dir (file-name-directory temp-file))
                 (basename (file-name-nondirectory temp-file))
                 (result (org-canvas--module-resolve-file-path basename dir)))
            (expect result :to-equal temp-file))
        (delete-file temp-file))))

  (it "falls back to basename when absolute path fails"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (let* ((dir (file-name-directory temp-file))
                 (basename (file-name-nondirectory temp-file))
                 ;; Use a relative path that will fail as absolute
                 (result (org-canvas--module-resolve-file-path
                          (concat "../nonexistent-dir/" basename) dir)))
            (expect result :to-equal (expand-file-name basename dir)))
        (delete-file temp-file))))

  (it "returns nil when neither path exists"
    (let ((result (org-canvas--module-resolve-file-path
                   "totally-nonexistent-file.org" "/tmp/")))
      (expect result :to-be nil))))

;;;; Pull

(describe "org-canvas-pull-modules"
  (it "creates module headings with items"
    (let* ((temp-dir (make-temp-file "pull-mod-test" t))
           (modules-file (expand-file-name "modules.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-modules-file modules-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 10) (name . "Week 1")
                                (items . [((type . "SubHeader")
                                           (title . "Readings")
                                           (id . 101))])))))
                          ((symbol-function 'org-canvas--module-resolve-item-link)
                           (lambda (_type _cid title) (or title "Untitled"))))
                  (org-canvas-pull-modules)
                  (with-current-buffer (find-file-noselect modules-file)
                    (expect (buffer-string) :to-match "Week 1")
                    (expect (buffer-string) :to-match "Readings"))))))
        (let ((buf (find-buffer-visiting modules-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates modules.org when it does not exist"
    (let* ((temp-dir (make-temp-file "pull-mod-test" t))
           (modules-file (expand-file-name "modules.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-modules-file modules-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '())))
                  (org-canvas-pull-modules)
                  (expect (file-exists-p modules-file) :to-be-truthy)))))
        (let ((buf (find-buffer-visiting modules-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; A Re-pull Writes What the First Pull Wrote (issue #403)

(defconst test-modules-403--remote
  '(((id . 10) (name . "Week 1")
     (items . [((type . "SubHeader") (title . "Readings") (id . 101)
                (position . 1))
               ((type . "ExternalUrl") (title . "Link") (id . 102)
                (external_url . "https://example.com") (position . 2))]))
    ((id . 11) (name . "Week 2")
     (items . [((type . "SubHeader") (title . "Labs") (id . 201))]))
    ((id . 12) (name . "Week 3") (items . [])))
  "Three modules, the last with no items, as Canvas lists them.")

(defun test-modules-403--pull (&optional initial times)
  "Pull `test-modules-403--remote' TIMES times over INITIAL; return the texts."
  (test-org-canvas-pull-texts
   'org-canvas-modules-file #'org-canvas-pull-modules
   (lambda (&rest _) test-modules-403--remote) initial times))

(describe "A modules re-pull leaves the file as it was (issue #403)"
  (it "writes the same text on the second and third pulls as on the first"
    (let ((texts (test-modules-403--pull nil 3)))
      (expect (nth 1 texts) :to-equal (nth 0 texts))
      (expect (nth 2 texts) :to-equal (nth 0 texts))))

  (it "puts each module's first item directly under its drawer"
    (let ((text (car (test-modules-403--pull nil 1))))
      (expect text :to-match ":CANVAS_ID: +10\n:END:\n\\*\\* Readings\n")
      (expect text :to-match ":CANVAS_ID: +11\n:END:\n\\*\\* Labs\n")
      (expect text :not :to-match "\n\n")))

  (it "collapses the blank lines a mirror already grew back to the first pull's shape"
    (let* ((clean (car (test-modules-403--pull nil 1)))
           (grown (replace-regexp-in-string
                   ":END:\n\\*\\*" ":END:\n\n\n\n**"
                   (replace-regexp-in-string "\n\\* " "\n\n\n* " clean)))
           (repulled (car (test-modules-403--pull (concat grown "\n\n") 1))))
      (expect grown :not :to-equal clean)
      (expect repulled :to-equal clean)))

  (it "rewrites a module's items at point without growing a blank line"
    (with-temp-org-buffer
     "* Week 1
:PROPERTIES:
:CANVAS_ID: 10
:END:
** Readings
:PROPERTIES:
:CANVAS_ID: 101
:ITEM_TYPE: SubHeader
:END:
* Week 2
:PROPERTIES:
:CANVAS_ID: 11
:END:
"
     (let ((module (car test-modules-403--remote)))
       (org-canvas--module-pull-item module (point-min))
       (let ((once (buffer-string)))
         (org-canvas--module-pull-item module (point-min))
         (expect (buffer-string) :to-equal once)
         (expect once :to-match ":CANVAS_ID: +10\n:END:\n\\*\\* Readings\n")
         (expect once :to-match ":END:\n\\* Week 2\n"))))))

(describe "org-canvas--module-items-digest"
  (it "is stable across repeated computation"
    (with-temp-org-buffer
     "* Week 1
:PROPERTIES:
:PUBLISHED: true
:END:
** Item One
** Item Two
"
     (let ((org-canvas-modules-file (buffer-file-name)))
       (org-back-to-heading)
       (let ((data (list :pom (point))))
         (expect (org-canvas--module-items-digest data)
                 :to-equal (org-canvas--module-items-digest data))))))

  (it "changes when an item is added"
    (let ((d1 (with-temp-org-buffer
               "* Week 1\n** Item One\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point))))))
          (d2 (with-temp-org-buffer
               "* Week 1\n** Item One\n** Item Two\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point)))))))
      (expect d1 :not :to-equal d2)))

  (it "changes when an item is renamed"
    (let ((d1 (with-temp-org-buffer
               "* Week 1\n** Old Title\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point))))))
          (d2 (with-temp-org-buffer
               "* Week 1\n** New Title\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point)))))))
      (expect d1 :not :to-equal d2)))

  (it "changes when an item property changes"
    (let ((d1 (with-temp-org-buffer
               "* Week 1\n** Item One\n:PROPERTIES:\n:INDENT: 0\n:END:\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point))))))
          (d2 (with-temp-org-buffer
               "* Week 1\n** Item One\n:PROPERTIES:\n:INDENT: 1\n:END:\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point)))))))
      (expect d1 :not :to-equal d2)))

  (it "does not change when an item gains a CANVAS_ID"
    ;; IDs are assigned by finalize right after the hash is computed;
    ;; if they entered the digest, every sync would dirty the module.
    (with-temp-org-buffer
     "* Week 1
** Item One
"
     (let ((org-canvas-modules-file (buffer-file-name)))
       (org-back-to-heading)
       (let* ((data (list :pom (point)))
              (before (org-canvas--module-items-digest data)))
         (save-excursion
           (search-forward "Item One")
           (org-entry-put (point) "CANVAS_ID" "501"))
         (expect (org-canvas--module-items-digest data) :to-equal before)))))

  (it "changes when a linked target gains a CANVAS_ID"
    ;; Interplay with #22: rotation of a target's id must dirty the
    ;; module so the stale item gets repaired on the next sync.
    (let ((pages-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file pages-file
              (insert "* Test Page\n:PROPERTIES:\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Test Page][Link Text]]\n"
                     (file-name-nondirectory pages-file))
             (let ((org-canvas-modules-file (buffer-file-name)))
               (org-back-to-heading)
               (let* ((data (list :pom (point)))
                      (before (org-canvas--module-items-digest data)))
                 (with-current-buffer (find-file-noselect pages-file)
                   (goto-char (point-min))
                   (org-back-to-heading)
                   (org-entry-put (point) "CANVAS_ID" "4242")
                   (save-buffer))
                 (expect (org-canvas--module-items-digest data)
                         :not :to-equal before)))))
        (let ((buf (find-buffer-visiting pages-file)))
          (when buf (kill-buffer buf)))
        (delete-file pages-file)))))

(describe "org-canvas-sync-modules payload-hash with items (mocked)"
  (it "skips an unchanged module but re-syncs after an item is added"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-writes 0)
                 (item-writes 0))
            (with-temp-file modules-file
              (insert "* Week 1
:PROPERTIES:
:PUBLISHED: true
:END:
** Section Header
"))
            (let ((org-canvas-modules-file modules-file))
              (with-test-sync-handler (lambda (method url &rest _args)
                                        (cond
                                          ((string-match "courses/99999/$" url)
                                            '((id . 99999)))
                                          ((and (eq method 'POST) (string-match "modules$" url))
                                            (setq module-writes (1+ module-writes))
                                            '((id . 500) (name . "Week 1")))
                                          ((and (eq method 'PUT) (string-match "modules/500$" url))
                                            (setq module-writes (1+ module-writes))
                                            '((id . 500) (name . "Week 1")))
                                          ((and (memq method '(POST PUT))
                                             (string-match "modules/500/items" url))
                                            (setq item-writes (1+ item-writes))
                                            '((id . 501) (title . "Item")))
                                          (t '((id . 1)))))
                (org-canvas-sync-modules)
                (expect module-writes :to-equal 1)
                (org-canvas-sync-modules)
                (expect module-writes :to-equal 1)
                (with-current-buffer (find-file-noselect modules-file)
                  (goto-char (point-max))
                  (insert "** Another Header\n")
                  (save-buffer))
                (setq item-writes 0)
                (org-canvas-sync-modules)
                (expect module-writes :to-equal 2)
                (expect item-writes :to-equal 2))))
        (delete-directory temp-dir t)))))

;;;; Bulk Publish (issue #52)

(defconst test-publish-52--files
  '(("content/lecture.pdf" . "pdf")
    ("assignments.org"
     :headings (("Lab 1" :props (("CANVAS_ID" . "61")
                                 ("PUBLISHED" . "false")))
                ("Lab 2" :props (("CANVAS_ID" . "62")
                                 ("PUBLISHED" . "false")))))
    ("pages.org"
     :headings (("Welcome" :props (("CANVAS_URL" . "welcome")
                                   ("PUBLISHED" . "false")))))
    ("files.org"
     :headings (("[[file:content/lecture.pdf][lecture.pdf]]"
                 :props (("CANVAS_ID" . "42")
                         ("PUBLISHED" . "false")))))
    ("modules.org"
     :headings (("Week 03" :props (("PUBLISHED" . "false"))
                  :body "** Readings
** [[file:assignments.org::*Lab 1][Lab 1]]
** [[file:pages.org::*Welcome][Welcome]]
** [[file:content/lecture.pdf][lecture.pdf]]
")
                ("Week 04" :props (("PUBLISHED" . "false"))
                  :body "** [[file:assignments.org::*Lab 2][Lab 2]]
"))))
  "A course with a module, an assignment, a page and a file.")

(describe "org-canvas--module-item-target"
  (it "resolves a heading link to the file and heading that own the object"
    (with-test-course (dir :files test-publish-52--files)
      (let ((target (org-canvas--module-item-target
                     "[[file:assignments.org::*Lab 1][Lab 1]]" dir)))
        (expect (car target) :to-equal (expand-file-name "assignments.org" dir))
        (with-current-buffer (find-file-noselect (car target))
          (expect (org-entry-get (cdr target) "CANVAS_ID") :to-equal "61")))))

  (it "resolves a direct file link through files.org"
    (with-test-course (dir :files test-publish-52--files)
      (let ((target (org-canvas--module-item-target
                     "[[file:content/lecture.pdf][lecture.pdf]]" dir)))
        (expect (car target) :to-equal (expand-file-name "files.org" dir))
        (with-current-buffer (find-file-noselect (car target))
          (expect (org-entry-get (cdr target) "CANVAS_ID") :to-equal "42")))))

  (it "returns nil for a heading that is not a link"
    (expect (org-canvas--module-item-target "Readings" "/tmp") :to-be nil))

  (it "returns nil when the target heading is absent"
    (with-test-course (dir :files test-publish-52--files)
      (expect (org-canvas--module-item-target
               "[[file:assignments.org::*Nonexistent][Nonexistent]]" dir)
              :to-be nil))))

(describe "org-canvas--module-heading-search-regexp"
  (it "matches a heading with extra spaces after the stars"
    (expect (string-match-p (org-canvas--module-heading-search-regexp "Lab 1")
                            "**  Lab 1")
            :to-be 0))

  (it "unescapes the brackets Org puts inside link components"
    ;; A files.org heading reached through the legacy nested form arrives
    ;; with its brackets escaped.
    (expect (string-match-p
             (org-canvas--module-heading-search-regexp
              "\\[\\[file:content/foo.pdf\\]\\[foo.pdf\\]\\]")
             "* [[file:content/foo.pdf][foo.pdf]]")
            :to-be 0))

  (it "quotes regexp metacharacters in the heading"
    (expect (string-match-p (org-canvas--module-heading-search-regexp "Week 1 (a+b)")
                            "* Week 1 (a+b)")
            :to-be 0)))

(describe "org-canvas--module-heading-position"
  (it "falls back to the display title when the heading text does not match"
    ;; A files.org heading *is* a link, so the exact-text search misses it —
    ;; and `org-get-heading' strips link syntax on Org 9.7 but not on 9.6.
    (with-test-course (dir :files test-publish-52--files)
      (let ((pos (org-canvas--module-heading-position
                  (expand-file-name "files.org" dir)
                  "content/lecture.pdf" "lecture.pdf")))
        (expect pos :to-be-truthy)
        (with-current-buffer (find-file-noselect
                              (expand-file-name "files.org" dir))
          (expect (org-entry-get pos "CANVAS_ID") :to-equal "42")))))

  (it "matches on the title alone for a link with no heading component"
    (with-test-course (dir :files test-publish-52--files)
      (let ((pos (org-canvas--module-heading-position
                  (expand-file-name "assignments.org" dir) nil "Lab 2")))
        (expect pos :to-be-truthy)
        (with-current-buffer (find-file-noselect
                              (expand-file-name "assignments.org" dir))
          (expect (org-entry-get pos "CANVAS_ID") :to-equal "62")))))

  (it "returns nil when nothing matches either way"
    (with-test-course (dir :files test-publish-52--files)
      (expect (org-canvas--module-heading-position
               (expand-file-name "assignments.org" dir) nil "Nonexistent")
              :to-be nil))))

(describe "org-canvas--module-publish-apply"
  (it "publishes the module and every object it lists, across files"
    ;; The reported chore: six to ten edits in three or four files.
    (with-test-course (dir :files test-publish-52--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-min))
          (org-canvas--module-publish-apply (point) t))
        (expect (test-org-canvas-course-prop dir "modules.org" "Week 03" "PUBLISHED")
                :to-equal "true")
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 1" "PUBLISHED")
                :to-equal "true")
        (expect (test-org-canvas-course-prop dir "pages.org" "Welcome" "PUBLISHED")
                :to-equal "true")
        (expect (test-org-canvas-course-prop dir "files.org" "lecture.pdf" "PUBLISHED")
                :to-equal "true"))))

  (it "leaves objects belonging to other modules alone"
    (with-test-course (dir :files test-publish-52--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-min))
          (org-canvas--module-publish-apply (point) t))
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 2" "PUBLISHED")
                :to-equal "false"))))

  (it "sets the property in place for a SubHeader, which owns its own state"
    ;; Issue #47's rule: an item with no object behind it carries its own
    ;; publish state, so that is where the property belongs.
    (with-test-course (dir :files test-publish-52--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-min))
          (org-canvas--module-publish-apply (point) t))
        (expect (test-org-canvas-course-prop dir "modules.org" "Readings" "PUBLISHED")
                :to-equal "true"))))

  (it "unpublishes the same set"
    (with-test-course (dir :files test-publish-52--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-min))
          (org-canvas--module-publish-apply (point) t)
          (goto-char (point-min))
          (org-canvas--module-publish-apply (point) nil))
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 1" "PUBLISHED")
                :to-equal "false")
        (expect (test-org-canvas-course-prop dir "files.org" "lecture.pdf" "PUBLISHED")
                :to-equal "false"))))

  (it "counts only the headings it actually changed"
    (with-test-course (dir :files test-publish-52--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-min))
          (let ((first (org-canvas--module-publish-apply (point) t)))
            (goto-char (point-min))
            (let ((again (org-canvas--module-publish-apply (point) t)))
              (expect (plist-get first :changed) :to-be-greater-than 0)
              (expect (plist-get again :changed) :to-equal 0)))))))

  (it "names the items it could not resolve instead of failing"
    (with-test-course (dir :files test-publish-52--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-max))
          (insert "** [[file:assignments.org::*Gone][Gone]]\n")
          (save-buffer)
          (goto-char (point-min))
          (re-search-forward "^\\* Week 04")
          (org-back-to-heading t)
          (let ((result (org-canvas--module-publish-apply (point) t)))
            (expect (plist-get result :unresolved) :to-equal '("Gone"))))))))

(describe "org-canvas--module-due-for-release-p"
  (it "is true once PUBLISH_AT has passed"
    (with-temp-org-buffer
     "* Week 03
:PROPERTIES:
:PUBLISH_AT: <2020-01-01 Wed 06:00>
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--module-due-for-release-p (point)) :to-be-truthy)))

  (it "is false while PUBLISH_AT is still in the future"
    (with-temp-org-buffer
     "* Week 03
:PROPERTIES:
:PUBLISH_AT: <2099-01-01 Thu 06:00>
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--module-due-for-release-p (point)) :to-be nil)))

  (it "is false for a module with no release date"
    (with-temp-org-buffer
     "* Week 03\n"
     (org-back-to-heading)
     (expect (org-canvas--module-due-for-release-p (point)) :to-be nil)))

  (it "is false for a release date that cannot be parsed"
    (with-temp-org-buffer
     "* Week 03
:PROPERTIES:
:PUBLISH_AT: whenever
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--module-due-for-release-p (point)) :to-be nil))))

(describe "org-canvas--module-publish-at-time"
  (it "returns nil for an unparsable timestamp rather than signalling"
    (with-temp-org-buffer
     "* Week 03
:PROPERTIES:
:PUBLISH_AT: not-a-date
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--module-publish-at-time (point)) :to-be nil))))

(describe "org-canvas--module-item-held-p"
  (it "holds an item whose own release date is still ahead"
    (with-temp-org-buffer
     "* Week 03
** Lab 2
:PROPERTIES:
:PUBLISH_AT: <2099-01-01 Thu 06:00>
:END:
"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* Lab 2")
     (org-back-to-heading t)
     (expect (org-canvas--module-item-held-p (point) t) :to-be-truthy)))

  (it "does not hold an item whose release date has passed"
    (with-temp-org-buffer
     "* Week 03
** Lab 2
:PROPERTIES:
:PUBLISH_AT: <2020-01-01 Wed 06:00>
:END:
"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* Lab 2")
     (org-back-to-heading t)
     (expect (org-canvas--module-item-held-p (point) t) :to-be nil)))

  (it "does not hold an item with no release date of its own"
    (with-temp-org-buffer
     "* Week 03\n** Lab 2\n"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* Lab 2")
     (org-back-to-heading t)
     (expect (org-canvas--module-item-held-p (point) t) :to-be nil)))

  (it "never holds an unpublish, so a week comes down whole"
    (with-temp-org-buffer
     "* Week 03
** Lab 2
:PROPERTIES:
:PUBLISH_AT: <2099-01-01 Thu 06:00>
:END:
"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* Lab 2")
     (org-back-to-heading t)
     (expect (org-canvas--module-item-held-p (point) nil) :to-be nil))))

;;; Per-item release dates (PUBLISH_AT on a module item)

(defconst test-release-item--files
  '(("assignments.org"
     :headings (("Lab 1" :props (("CANVAS_ID" . "61")
                                 ("PUBLISHED" . "false")))
                ("Lab 2" :props (("CANVAS_ID" . "62")
                                 ("PUBLISHED" . "false")))
                ("Lab 3" :props (("CANVAS_ID" . "63")
                                 ("PUBLISHED" . "false")))))
    ("modules.org"
     :headings (("Week 03" :props (("PUBLISHED" . "false"))
                  :body "** [[file:assignments.org::*Lab 1][Lab 1]]
** [[file:assignments.org::*Lab 2][Lab 2]]
:PROPERTIES:
:PUBLISH_AT: <2099-01-01 Thu 06:00>
:END:
** [[file:assignments.org::*Lab 3][Lab 3]]
:PROPERTIES:
:PUBLISH_AT: <2020-01-01 Wed 06:00>
:END:
"))))
  "A course whose module items carry their own release dates.
Lab 1 has none, Lab 2's is in the future, Lab 3's has passed.")

(describe "publishing a module around a scheduled item"
  (it "holds back the item whose own date is still ahead"
    (with-test-course (dir :files test-release-item--files)
      (let* ((org-canvas-modules-file (expand-file-name "modules.org" dir))
             (result (with-current-buffer
                         (find-file-noselect org-canvas-modules-file)
                       (org-canvas--module-publish-apply
                        (test-org-canvas-course-heading-pos dir "modules.org" "Week 03") t))))
        (expect (plist-get result :held) :to-equal '("Lab 2"))
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 2" "PUBLISHED")
                :to-equal "false")
        ;; Everything without a date of its own goes live with the module.
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 1" "PUBLISHED")
                :to-equal "true"))))

  (it "publishes an item whose own date has already passed"
    (with-test-course (dir :files test-release-item--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (org-canvas--module-publish-apply
           (test-org-canvas-course-heading-pos dir "modules.org" "Week 03") t))
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 3" "PUBLISHED")
                :to-equal "true"))))

  (it "unpublishes the held item along with the rest of the week"
    (with-test-course (dir :files test-release-item--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect
                              (expand-file-name "assignments.org" dir))
          (goto-char (point-min))
          (re-search-forward "^\\* Lab 2")
          (org-back-to-heading t)
          (org-entry-put (point) "PUBLISHED" "true")
          (save-buffer))
        (let ((result (with-current-buffer
                          (find-file-noselect org-canvas-modules-file)
                        (org-canvas--module-publish-apply
                         (test-org-canvas-course-heading-pos dir "modules.org" "Week 03") nil))))
          (expect (plist-get result :held) :to-be nil))
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 2" "PUBLISHED")
                :to-equal "false")))))

(describe "org-canvas--module-item-release-markers"
  (it "returns only the items whose release date has passed"
    (with-test-course (dir :files test-release-item--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (let ((markers (org-canvas--module-item-release-markers
                          (test-org-canvas-course-heading-pos dir "modules.org" "Week 03"))))
            (expect (length markers) :to-equal 1)
            (expect (org-entry-get (marker-position (car markers)) "PUBLISH_AT")
                    :to-match "2020-01-01")
            (dolist (m markers) (set-marker m nil))))))))

(describe "org-canvas--module-release-items"
  (it "releases only the item whose own date has passed"
    (with-test-course (dir :files test-release-item--files)
      (let* ((org-canvas-modules-file (expand-file-name "modules.org" dir))
             (result (org-canvas--module-release-items)))
        (expect (plist-get result :released) :to-equal '("Lab 3"))
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 3" "PUBLISHED")
                :to-equal "true")
        ;; The module and its undated items are left where they were.
        (expect (test-org-canvas-course-prop dir "assignments.org" "Lab 1" "PUBLISHED")
                :to-equal "false")
        (expect (test-org-canvas-course-prop dir "modules.org" "Week 03" "PUBLISHED")
                :to-equal "false"))))

  (it "reports an item released inside a module that is still unpublished"
    (with-test-course (dir :files test-release-item--files)
      (let* ((org-canvas-modules-file (expand-file-name "modules.org" dir))
             (result (org-canvas--module-release-items)))
        (expect (plist-get result :hidden) :to-equal '(("Week 03" "Lab 3"))))))

  (it "says nothing about hiding when the module is published"
    (with-test-course (dir :files test-release-item--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (org-entry-put (test-org-canvas-course-heading-pos dir "modules.org" "Week 03") "PUBLISHED" "true")
          (save-buffer))
        (expect (plist-get (org-canvas--module-release-items) :hidden)
                :to-be nil))))

  (it "is idempotent once the item has been released"
    (with-test-course (dir :files test-release-item--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (org-canvas--module-release-items)
        (expect (plist-get (org-canvas--module-release-items) :released)
                :to-be nil))))

  (it "names a scheduled item whose link resolves to nothing"
    (with-test-course (dir :files test-release-item--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-max))
          (insert "** [[file:assignments.org::*Gone][Gone]]\n"
                  ":PROPERTIES:\n:PUBLISH_AT: <2020-01-01 Wed 06:00>\n:END:\n")
          (save-buffer))
        (expect (plist-get (org-canvas--module-release-items) :unresolved)
                :to-equal '("Gone")))))

  (it "releases a SubHeader in place, since it owns its own state"
    (with-test-course (dir :files test-release-item--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (with-current-buffer (find-file-noselect org-canvas-modules-file)
          (goto-char (point-max))
          (insert "** Readings\n"
                  ":PROPERTIES:\n:PUBLISH_AT: <2020-01-01 Wed 06:00>\n:END:\n")
          (save-buffer))
        (org-canvas--module-release-items)
        (expect (test-org-canvas-course-prop dir "modules.org" "Readings" "PUBLISHED")
                :to-equal "true"))))

  (it "returns nil when there is no modules file to read"
    (let ((org-canvas-modules-file "/tmp/nonexistent-release-modules.org"))
      (expect (org-canvas--module-release-items) :to-be nil))))

(describe "org-canvas--module-positions"
  (it "lists the level-1 module headings with their positions"
    (with-test-course (dir :files test-publish-52--files)
      (let ((org-canvas-modules-file (expand-file-name "modules.org" dir)))
        (expect (mapcar #'car (org-canvas--module-positions))
                :to-equal '("Week 03" "Week 04")))))

  (it "returns nil when modules.org does not exist"
    (let ((org-canvas-modules-file "/tmp/nonexistent-modules.org"))
      (expect (org-canvas--module-positions) :to-be nil))))

;;; org-canvas-modules-test.el ends here
