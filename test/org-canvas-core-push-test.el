;;; org-canvas-core-push-test.el --- Buttercup tests for the push guards  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; What a push passes through: remote drift detection and the snapshot
;; behind the payload-hash skip, the conflict check and its prompt, the
;; timeout/404 recovery, the duplicate-title guard and its twins, the
;; duplicate guard, the child reconcilers, and the echo check.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)
(require 'org-canvas-pages)
(require 'org-canvas-announcements)
(require 'org-canvas-discussions)
(require 'org-canvas-assignments)
(require 'org-canvas-rubrics)
(require 'org-canvas-files)

(describe "org-canvas--search-item (mocked)"
  (it "returns matching item by title"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (title . "Assignment 1"))
                                  ((id . 101) (title . "Assignment 2"))])))
        (let ((result (org-canvas--search-item "assignments" "Assignment 1")))
          (expect (alist-get 'id result) :to-equal 100)))))

  (it "returns nil when no match found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("pages" . [((url . "page-1") (title . "Page 1"))])))
        (let ((result (org-canvas--search-item "pages" "Nonexistent")))
          (expect result :to-be nil)))))

  (it "uses custom match-field"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 200) (name . "Test Assignment"))])))
        (let ((result (org-canvas--search-item "assignments" "Test Assignment"
                                               :match-field 'name)))
          (expect (alist-get 'id result) :to-equal 200)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("API error")))))
        (let ((result (org-canvas--search-item "assignments" "Test")))
          (expect result :to-be nil))))))

(describe "org-canvas--push-to-api (mocked)"
  (it "sends POST when no canvas-id (new item)"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Item" :canvas-id nil))
              (payload '((title . "New Item"))))
          (org-canvas--push-to-api data payload :endpoint "assignments")
          (expect-api-called 'POST "assignments$")))))

  (it "sends PUT when canvas-id present (update)"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "123"))
              (payload '((title . "Existing"))))
          (org-canvas--push-to-api data payload :endpoint "assignments")
          (expect-api-called 'PUT "assignments/123")))))

  (it "retries as POST on 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (and (eq method 'PUT) (= call-count 1))
                         (signal 'error '("API Request Failed (HTTP 404)" nil nil))
                       '((id . 456))))))
          (let ((data '(:title "Stale" :canvas-id "999"))
                (payload '((title . "Stale"))))
            (let ((result (org-canvas--push-to-api data payload :endpoint "pages")))
              (expect (alist-get 'id result) :to-equal 456)))))))

  (it "recovers from timeout with find-fn"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     ;; Error format: (err-msg error-thrown) where error-thrown contains "Timeout"
                     (signal 'error (list "Request failed" "Timeout waiting for response")))))
          (let ((data '(:title "Timeout Test" :canvas-id nil))
                (payload '((title . "Timeout Test")))
                (find-fn (lambda (_title) '((id . 789)))))
            (let ((result (org-canvas--push-to-api data payload
                                                   :endpoint "items"
                                                   :find-fn find-fn)))
              (expect (alist-get 'id result) :to-equal 789)))))))

  (it "signals error when timeout recovery fails"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (signal 'error (list "Request failed" "Timeout waiting for response")))))
          (let ((data '(:title "Lost Item" :canvas-id nil))
                (payload '((title . "Lost Item")))
                (find-fn (lambda (_title) nil)))
            (expect (org-canvas--push-to-api data payload
                                             :endpoint "items"
                                             :find-fn find-fn)
                    :to-throw 'error))))))

  (it "uses custom id-key"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Page" :canvas-url "my-page"))
              (payload '((title . "Page"))))
          (org-canvas--push-to-api data payload
                                   :endpoint "pages"
                                   :id-key :canvas-url)
          (expect-api-called 'PUT "pages/my-page"))))))

(describe "org-canvas--finalize-item"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Item" :pom (point-marker)))
           (response '((id . 12345))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Item" :pom (point-marker)))
           (response '((id . 11111))))
       (org-canvas--finalize-item data response)
       (expect-synced-timestamp (point)))))

  (it "uses custom id-field"
    (with-temp-org-buffer
     "* Page
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Page" :pom (point-marker)))
           (response '((url . "my-page-url"))))
       (org-canvas--finalize-item data response :id-field 'url)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "my-page-url"))))

  (it "uses custom id-property"
    (with-temp-org-buffer
     "* Page
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Page" :pom (point-marker)))
           (response '((url . "custom-url"))))
       (org-canvas--finalize-item data response
                                  :id-field 'url
                                  :id-property "CANVAS_URL")
       (expect (org-entry-get (point) "CANVAS_URL") :to-equal "custom-url"))))

  (it "calls post-fn when provided"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((post-called nil)
           (data (list :title "Item" :pom (point-marker)))
           (response '((id . 999))))
       (org-canvas--finalize-item data response
                                  :post-fn (lambda (_d _r &optional _ctx) (setq post-called t)))
       (expect post-called :to-be t))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* No ID
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No ID" :pom (point-marker)))
           (response '((error . "failed"))))
       (expect (org-canvas--finalize-item data response) :to-throw 'error)))))

(describe "org-canvas--push-to-api edge cases (mocked)"
  (it "uses custom title-key"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "My Item" :canvas-id nil))
              (payload '((name . "My Item"))))
          (org-canvas--push-to-api data payload
                                   :endpoint "items"
                                   :title-key :name)
          (expect-api-called 'POST "items$")))))

  (it "handles nested timeout recovery for POST retry after 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("API Request Failed (HTTP 404)")))
                      ;; Second call: POST times out
                      ;; Error format: (error "msg" "Timeout") so error-thrown = "Timeout"
                      ((and (eq method 'POST) (= call-count 2))
                       (signal 'error '("Request failed" "Timeout")))
                      (t nil)))))
          (let ((data '(:title "Test" :canvas-id "old-id"))
                (payload '((title . "Test")))
                ;; The 404 recovery asks first whether the title lives on
                ;; under another id (issue #179): nothing yet.  After the
                ;; POST times out, the same search finds what it made.
                (asked 0)
                (find-fn nil))
            (setq find-fn (lambda (_title)
                            (setq asked (1+ asked))
                            (when (> asked 1) '((id . 789)))))
            (let ((result (org-canvas--push-to-api data payload
                                                   :endpoint "items"
                                                   :find-fn find-fn)))
              (expect (alist-get 'id result) :to-equal 789)
              (expect asked :to-equal 2))))))))

(describe "org-canvas--search-item edge cases (mocked)"
  (it "uses custom params when provided"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("items" . [((id . 1) (title . "Test"))])))
        (org-canvas--search-item "items" "Test"
                                 :params '(("custom_param" . "value")))
        (expect-api-called 'GET "items"))))

  (it "returns first exact match only"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("items" . [((id . 1) (title . "Similar"))
                            ((id . 2) (title . "Test"))
                            ((id . 3) (title . "Test"))])))  ; Duplicate
        (let ((result (org-canvas--search-item "items" "Test")))
          (expect (alist-get 'id result) :to-equal 2))))))

(describe "org-canvas--finalize-item edge cases"
  (it "uses custom title-key for logging"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :name "Custom Name" :pom (point-marker)))
           (response '((id . 12345))))
       (org-canvas--finalize-item data response :title-key :name)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345"))))

  (it "calls post-fn with data and response"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((post-fn-called-with nil)
           (data (list :title "Test" :pom (point-marker)))
           (response '((id . 999))))
       (org-canvas--finalize-item data response
                                  :post-fn (lambda (d r &optional _ctx)
                                             (setq post-fn-called-with (list d r))))
       (expect post-fn-called-with :to-be-truthy)
       (expect (car post-fn-called-with) :to-equal data)
       (expect (cadr post-fn-called-with) :to-equal response)))))

(describe "org-canvas--finalize-item restamps after a post-fn write (issue #124)"
  (it "re-reads the item and stamps what Canvas holds after the association"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Essay
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (let ((urls nil))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (_method url &rest _args)
                      (push url urls)
                      '((id . 2497349) (updated_at . "2026-09-02T14:07:34Z")))))
           (org-canvas--finalize-item
            (list :title "Essay" :pom (point-marker))
            '((id . 2497349) (updated_at . "2026-09-02T14:07:31Z"))
            :endpoint "assignments"
            :post-fn (lambda (_d _r ctx) (org-canvas--finalize-note-remote-write ctx))))
         (expect (length urls) :to-equal 1)
         (expect (car urls) :to-match "assignments/2497349")
         ;; The push response's stamp was three seconds behind what the
         ;; association left on Canvas; the baseline follows Canvas.
         (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
                 :to-equal "2026-09-02T14:07:34Z")))))

  (it "spends no request when the post-fn wrote nothing"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Essay
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (let ((called nil))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (&rest _) (setq called t) nil)))
           (org-canvas--finalize-item
            (list :title "Essay" :pom (point-marker))
            '((id . 7) (updated_at . "2026-09-02T14:07:31Z"))
            :endpoint "assignments"
            :post-fn #'ignore))
         (expect called :to-be nil)
         (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
                 :to-equal "2026-09-02T14:07:31Z")))))

  (it "spends no request when the module named no endpoint"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Topic
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (let ((called nil))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (&rest _) (setq called t) nil)))
           (org-canvas--finalize-item
            (list :title "Topic" :pom (point-marker))
            '((id . 7) (updated_at . "2026-09-02T14:07:31Z"))
            :post-fn (lambda (_d _r ctx) (org-canvas--finalize-note-remote-write ctx))))
         (expect called :to-be nil)))))

  (it "keeps the stamp it has when the re-read fails"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Essay
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (let ((warned nil))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (&rest _) (signal 'error '("HTTP 500"))))
                   ((symbol-function 'org-canvas--log-warning)
                    (lambda (_logger fmt &rest args)
                      (push (apply #'format fmt args) warned))))
           (org-canvas--finalize-item
            (list :title "Essay" :pom (point-marker))
            '((id . 7) (updated_at . "2026-09-02T14:07:31Z"))
            :endpoint "assignments"
            :post-fn (lambda (_d _r ctx) (org-canvas--finalize-note-remote-write ctx))))
         (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
                 :to-equal "2026-09-02T14:07:31Z")
         (expect (car warned) :to-match "may report this push as a change")))))

  (it "leaves the stamp alone when the re-read carries no timestamp"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Essay
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (&rest _) '((id . 7)))))
         (org-canvas--finalize-item
          (list :title "Essay" :pom (point-marker))
          '((id . 7) (updated_at . "2026-09-02T14:07:31Z"))
          :endpoint "assignments"
          :post-fn (lambda (_d _r ctx) (org-canvas--finalize-note-remote-write ctx))))
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-09-02T14:07:31Z"))))

  (it "reads the field the feature tracks, not always updated_at"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Handout
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (&rest _)
                    '((id . 7) (updated_at . "2026-09-02T15:00:00Z")
                      (modified_at . "2026-09-02T14:00:00Z")))))
         (org-canvas--finalize-item
          (list :title "Handout" :pom (point-marker))
          '((id . 7) (modified_at . "2026-09-02T13:00:00Z"))
          :endpoint "files"
          :updated-field 'modified_at
          :post-fn (lambda (_d _r ctx) (org-canvas--finalize-note-remote-write ctx))))
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-09-02T14:00:00Z"))))

  (it "writes nothing during a dry run"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Essay
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (let ((called nil)
             (org-canvas--dry-run t))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (&rest _) (setq called t) nil)))
           (org-canvas--finalize-item
            (list :title "Essay" :pom (point-marker))
            '((id . 7) (updated_at . "2026-09-02T14:07:31Z"))
            :endpoint "assignments"
            :post-fn (lambda (_d _r ctx) (org-canvas--finalize-note-remote-write ctx))))
         (expect called :to-be nil))))))

(describe "org-canvas--push-to-api nested recovery"
  (it "handles 404→POST→Timeout→find-fn fails"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("API Request Failed (HTTP 404)")))
                      ;; Second call: POST times out
                      ((and (eq method 'POST) (= call-count 2))
                       (signal 'error '("Request failed" "Timeout")))
                      (t nil)))))
          (let ((data '(:title "Test" :canvas-id "old-id"))
                (payload '((title . "Test")))
                (find-fn (lambda (_title) nil)))
            (expect (org-canvas--push-to-api data payload
                                             :endpoint "items"
                                             :find-fn find-fn)
                    :to-throw 'error))))))

  (it "re-throws non-timeout errors without find-fn"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request" nil nil)))))
        (let ((data '(:title "Bad" :canvas-id nil))
              (payload '((title . "Bad"))))
          (expect (org-canvas--push-to-api data payload :endpoint "items")
                  :to-throw 'error)))))

  (it "does not attempt timeout recovery without find-fn"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Request failed" "Timeout")))))
        (let ((data '(:title "Lost" :canvas-id nil))
              (payload '((title . "Lost"))))
          (expect (org-canvas--push-to-api data payload :endpoint "items")
                  :to-throw 'error))))))

(describe "org-canvas--push-to-api 404→POST non-timeout error"
  (it "re-throws non-timeout POST error after 404 retry"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("API Request Failed (HTTP 404)" nil nil)))
                      ;; POST fails with non-timeout error
                      ((eq method 'POST)
                       (signal 'error '("Bad Request" "Validation error" nil)))
                      (t nil)))))
          (let ((data '(:title "Item" :canvas-id "999"))
                (payload '((title . "Item"))))
            ;; Even with find-fn, non-timeout errors should re-throw.  The
            ;; search finds no twin (issue #179), so the recovery POSTs.
            (expect (org-canvas--push-to-api data payload
                                             :endpoint "items"
                                             :find-fn (lambda (_) nil))
                    :to-throw 'error)))))))

(describe "org-canvas--finalize-item pom nil-guard"
  (it "errors when :pom is missing from data"
    (let ((data (list :title "No POM"))
          (response '((id . 123))))
      (expect (org-canvas--finalize-item data response)
              :to-throw 'error))))

(describe "org-canvas--parse-iso8601-time"
  (it "parses a valid ISO8601 timestamp"
    (let ((result (org-canvas--parse-iso8601-time "2026-01-15T10:00:00Z")))
      (expect result :to-be-truthy)))

  (it "returns nil for nil ISO8601 input"
    (expect (org-canvas--parse-iso8601-time nil) :to-be nil))

  (it "returns nil for :null input"
    (expect (org-canvas--parse-iso8601-time :null) :to-be nil))

  (it "returns nil for non-string input"
    (expect (org-canvas--parse-iso8601-time 12345) :to-be nil)))

(describe "org-canvas--parse-last-synced"
  (it "parses #+LAST_SYNCED file header into a time value"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-01-15 Thu 10:00]
* Item
:PROPERTIES:
:END:
"
     (re-search-forward "^\\* ")
     (org-back-to-heading)
     (let ((result (org-canvas--parse-last-synced (point-marker))))
       (expect result :to-be-truthy))))

  (it "returns nil when no #+LAST_SYNCED header"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--parse-last-synced (point-marker)) :to-be nil))))

(describe "org-canvas--conflict-check"
  (it "returns conflict cons when remote is newer"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       ;; Remote updated_at is much newer than the file-level LAST_SYNCED
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    '((id . 123) (updated_at . "2026-02-01T10:00:00Z")))))
         (let ((result (org-canvas--conflict-check "items" "123" (point-marker))))
           (expect (car result) :to-equal 'conflict)
           (expect (alist-get 'id (cdr result)) :to-equal 123))))))

  (it "returns nil when local is newer"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-02-01 Thu 10:00]
* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    '((id . 123) (updated_at . "2026-01-01T10:00:00Z")))))
         (expect (org-canvas--conflict-check "items" "123" (point-marker))
                 :to-be nil)))))

  (it "returns nil when no #+LAST_SYNCED header exists"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (org-back-to-heading)
       (expect (org-canvas--conflict-check "items" "123" (point-marker))
               :to-be nil))))

  (it "returns nil on GET failure"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    (signal 'error '("HTTP 500")))))
         (expect (org-canvas--conflict-check "items" "123" (point-marker))
                 :to-be nil)))))

  (it "warns (does not silently swallow) on GET failure"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (spy-on 'org-canvas--log-warning)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    (signal 'error '("HTTP 500")))))
         (org-canvas--conflict-check "items" "123" (point-marker))
         (let ((warned nil))
           (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
             (when (string-match-p "Remote check.*failed"
                                   (apply #'format (cdr call)))
               (setq warned t)))
           (expect warned :to-be t)))))))

(describe "org-canvas--sync-item-url (issue #344)"
  (it "asks the push's own item-URL function first"
    (with-org-canvas-test-config
      (expect (org-canvas--sync-item-url
               "items" 7 (org-canvas--sync-make-ctx :feature-name "assignments")
               (lambda (id) (format "https://global/items/%s" id)))
              :to-equal "https://global/items/7")))

  (it "falls back to the course endpoint when the feature has no item-URL fn"
    (with-org-canvas-test-config
      (expect (org-canvas--sync-item-url
               "assignments" 7
               (org-canvas--sync-make-ctx :feature-name "assignments"))
              :to-equal (org-canvas-api-course-endpoint "assignments/7"))))

  (it "falls back to the course endpoint without a run context"
    (with-org-canvas-test-config
      (expect (org-canvas--sync-item-url "items" 7)
              :to-equal (org-canvas-api-course-endpoint "items/7")))))

(describe "org-canvas--conflict-check with an item URL (issue #344)"
  (it "reads the URL it is handed instead of the course endpoint"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (with-mock-api
         (org-canvas--conflict-check "items" "123" (point-marker) nil nil nil
                                     "https://global/items/123")
         (expect (cadr (car test-org-canvas-api-calls))
                 :to-equal "https://global/items/123"))))))

(describe "org-canvas--push-to-api conflict detection"
  (it "returns conflict when user chooses skip"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Conflict Item
:PROPERTIES:
:CANVAS_ID: 456
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let ((org-canvas-detect-conflicts t))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (when (eq method 'GET)
                        '((id . 456) (updated_at . "2026-02-01T10:00:00Z")))))
                   ((symbol-function 'org-canvas--resolve-conflict)
                    (lambda (_data _remote &optional _ctx) 'skip)))
           (let ((data (list :title "Conflict Item" :canvas-id "456"
                             :pom (point-marker)))
                 (payload '((title . "Conflict Item"))))
             (expect (org-canvas--push-to-api data payload :endpoint "items")
                     :to-equal 'conflict)))))))

  (it "skips conflict check for POST (new items)"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((org-canvas-detect-conflicts t)
              (data '(:title "New Item" :canvas-id nil))
              (payload '((title . "New Item"))))
          ;; POST should proceed without conflict check
          (org-canvas--push-to-api data payload :endpoint "items")
          (expect-api-called 'POST "items$")))))

  (it "skips conflict check (no GET) when detect-conflicts is nil"
    ;; LAST_SYNCED + pom are present, so if the conflict gate were wrongly
    ;; open the check would fire a GET.  detect-conflicts is nil, so it must
    ;; not.  Guards the `(and org-canvas-detect-conflicts (eq method 'PUT)
    ;; ...)' gate against an `or' that would conflict-check on every PUT.
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Item
:PROPERTIES:
:CANVAS_ID: 789
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (with-mock-api
         (let ((org-canvas-detect-conflicts nil)
               (data (list :title "Force Push" :canvas-id "789"
                           :pom (point-marker)))
               (payload '((title . "Force Push"))))
           (org-canvas--push-to-api data payload :endpoint "items")
           (expect-api-called 'PUT "items/789")
           (expect (test-org-canvas-api-called-p 'GET "items") :to-be nil))))))

  (it "skips conflict check in dry-run mode"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t)
            (org-canvas-detect-conflicts t)
            (api-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq api-called t))))
          (let ((data '(:title "Dry Run" :canvas-id "123"))
                (payload '((title . "Dry Run"))))
            (org-canvas--push-to-api data payload :endpoint "items")
            (expect api-called :to-be nil)))))))

(describe "org-canvas--404-on-put-p"
  (it "is non-nil only for a 404 on PUT/PATCH"
    (expect (org-canvas--404-on-put-p '(error "HTTP 404 Not Found") 'PUT)
            :to-be-truthy)
    (expect (org-canvas--404-on-put-p '(error "HTTP 404 Not Found") 'PATCH)
            :to-be-truthy))

  (it "is nil for a 404 on a non-PUT method"
    ;; Guards the `(and (memq method ...) (404-error-p err))': an `or' would
    ;; wrongly retry a 404 on GET/POST as a POST.
    (expect (org-canvas--404-on-put-p '(error "HTTP 404 Not Found") 'GET)
            :to-be nil)
    (expect (org-canvas--404-on-put-p '(error "HTTP 404 Not Found") 'POST)
            :to-be nil))

  (it "is nil for a non-404 error on PUT"
    (expect (org-canvas--404-on-put-p '(error "HTTP 500 Server Error") 'PUT)
            :to-be nil)))

(describe "org-canvas--finalize-item saves CANVAS_UPDATED_AT"
  (it "stores updated_at from response"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Item" :pom (point-marker)))
           (response '((id . 999) (updated_at . "2026-02-01T12:00:00Z"))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-02-01T12:00:00Z"))))

  (it "does not set CANVAS_UPDATED_AT when absent from response"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Item" :pom (point-marker)))
           (response '((id . 111))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT") :to-be nil)))))

(describe "org-canvas--push-to-api conflict resolution"
  (it "proceeds with PUT when user chooses push"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Push Item
:PROPERTIES:
:CANVAS_ID: 789
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let ((org-canvas-detect-conflicts t)
             (put-called nil))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (pcase method
                        ('GET '((id . 789) (updated_at . "2026-02-01T10:00:00Z")))
                        ('PUT (setq put-called t) '((id . 789))))))
                   ((symbol-function 'org-canvas--resolve-conflict)
                    (lambda (_data _remote &optional _ctx) 'push)))
           (let ((data (list :title "Push Item" :canvas-id "789"
                             :pom (point-marker)))
                 (payload '((title . "Push Item"))))
             (org-canvas--push-to-api data payload :endpoint "items")
             (expect put-called :to-be-truthy)))))))

  (it "returns pulled when user chooses pull"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Pull Item
:PROPERTIES:
:CANVAS_ID: 111
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let ((org-canvas-detect-conflicts t)
             (ctx (org-canvas--sync-make-ctx :pull-item-fn (lambda (_item _pos) nil))))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (when (eq method 'GET)
                        '((id . 111) (updated_at . "2026-02-01T10:00:00Z")))))
                   ((symbol-function 'org-canvas--resolve-conflict)
                    (lambda (_data _remote &optional _ctx) 'pull)))
           (let ((data (list :title "Pull Item" :canvas-id "111"
                             :pom (point-marker)))
                 (payload '((title . "Pull Item"))))
             (expect (org-canvas--push-to-api data payload :endpoint "items" :ctx ctx)
                     :to-equal 'pulled)))))))

  (it "falls back to conflict when pull chosen but no pull-fn"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* No Pull
:PROPERTIES:
:CANVAS_ID: 222
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let ((org-canvas-detect-conflicts t))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (when (eq method 'GET)
                        '((id . 222) (updated_at . "2026-02-01T10:00:00Z")))))
                   ((symbol-function 'org-canvas--resolve-conflict)
                    (lambda (_data _remote &optional _ctx) 'pull)))
           (let ((data (list :title "No Pull" :canvas-id "222"
                             :pom (point-marker)))
                 (payload '((title . "No Pull"))))
             (expect (org-canvas--push-to-api data payload :endpoint "items")
                     :to-equal 'conflict))))))))

;;; org-canvas-core-test.el ends here


(describe "rate-limit retry through push pipeline"
  (it "succeeds after 429 retry on POST"
    (with-org-canvas-test-config
      (let ((call-count 0)
            (org-canvas-rate-limit-retries 2)
            (org-canvas-rate-limit-wait 0))
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         (signal 'plz-error
                                 (make-plz-error
                                  :response (make-plz-response :status 429 :body "rate limit")))
                       '((id . 42) (title . "New Item"))))))
          (let ((data '(:title "New Item" :canvas-id nil))
                (payload '((title . "New Item"))))
            (let ((result (org-canvas--push-to-api data payload :endpoint "assignments")))
              (expect (alist-get 'id result) :to-equal 42)
              (expect call-count :to-equal 2)))))))

  (it "succeeds after 429 retry on PUT"
    (with-org-canvas-test-config
      (let ((call-count 0)
            (org-canvas-rate-limit-retries 2)
            (org-canvas-rate-limit-wait 0)
            (org-canvas-detect-conflicts nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         (signal 'plz-error
                                 (make-plz-error
                                  :response (make-plz-response :status 429 :body "rate limit")))
                       '((id . 100) (title . "Updated"))))))
          (let ((data '(:title "Updated" :canvas-id "100"))
                (payload '((title . "Updated"))))
            (let ((result (org-canvas--push-to-api data payload :endpoint "assignments")))
              (expect (alist-get 'id result) :to-equal 100)
              (expect call-count :to-equal 2))))))))

(describe "org-canvas--sync-deferred-error-p"
  (it "matches Canvas drop-rule rejections"
    (expect (org-canvas--sync-deferred-error-p
             '(org-canvas-api-error
               "Drop rules cannot be higher than the number of assignments (HTTP 400)"))
            :to-be-truthy))

  (it "does not match other errors"
    (expect (org-canvas--sync-deferred-error-p
             '(org-canvas-api-error "API Request Failed (HTTP 404)"))
            :to-be nil)
    (expect (org-canvas--sync-deferred-error-p '(error "Parse failed"))
            :to-be nil)))

(describe "org-canvas--conflict-baseline"
  (it "prefers the entry's own CANVAS_UPDATED_AT"
    ;; Issue #48: a course that is only ever pushed never acquires the
    ;; file-level header, so the check that depended on it never ran.  The
    ;; per-entry stamp is written by finalize on every push.
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--conflict-baseline (point))
             :to-equal (date-to-time "2026-08-19T13:19:49Z"))))

  (it "falls back to the file header when the entry has no stamp"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-08-19 Wed 12:00]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (goto-char (point-min))
     (re-search-forward "^\\* ")
     (org-back-to-heading)
     (expect (org-canvas--conflict-baseline (point))
             :to-equal (encode-time
                        (org-parse-time-string "[2026-08-19 Wed 12:00]")))))

  (it "prefers an explicit fallback over the file header"
    (let ((explicit (date-to-time "2026-01-01T00:00:00Z")))
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-08-19 Wed 12:00]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (goto-char (point-min))
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (expect (org-canvas--conflict-baseline (point) explicit)
               :to-equal explicit))))

  (it "returns nil when nothing has ever been recorded"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--conflict-baseline (point)) :to-be nil))))

(describe "org-canvas--sync-fetch-remote-updated"
  (it "maps remote ids to their updated_at"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((id . 1) (updated_at . "2026-08-19T13:19:49Z"))
                     ((id . 2) (updated_at . "2026-08-01T00:00:00Z"))))))
        (let ((map (org-canvas--sync-fetch-remote-updated "assignments")))
          (expect (gethash "1" map) :to-equal "2026-08-19T13:19:49Z")
          (expect (gethash "2" map) :to-equal "2026-08-01T00:00:00Z")))))

  (it "keys pages on url so the map matches CANVAS_URL"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((url . "welcome") (id . 9)
                      (updated_at . "2026-08-19T13:19:49Z"))))))
        (let ((map (org-canvas--sync-fetch-remote-updated "pages")))
          (expect (gethash "welcome" map) :to-equal "2026-08-19T13:19:49Z")
          (expect (gethash "9" map) :to-be nil)))))

  (it "returns nil for an unregistered feature without calling the API"
    (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
               (lambda (&rest _) (error "Must not be called"))))
      (expect (org-canvas--sync-fetch-remote-updated "not-a-feature") :to-be nil)))

  (it "returns nil and warns when the list request fails"
    (with-org-canvas-test-config
      (let ((warnings nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (error "Connection refused")))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) warnings))))
          (expect (org-canvas--sync-fetch-remote-updated "assignments") :to-be nil)
          (expect (car warnings) :to-match "without checking Canvas"))))))

(describe "org-canvas--sync-remote-drifted-p"
  (let ((baseline (encode-time (org-parse-time-string "[2026-08-19 Wed 12:00]"))))

    (it "flags an item Canvas updated after the baseline"
      (let ((map (make-hash-table :test 'equal)))
        (puthash "61" "2026-08-25T00:00:00Z" map)
        (expect (org-canvas--sync-remote-drifted-p
                 "61" (list :remote-updated map :baseline baseline) "Lab 1")
                :to-be-truthy)))

    (it "leaves an item Canvas has not touched since the baseline alone"
      (let ((map (make-hash-table :test 'equal)))
        (puthash "61" "2026-08-01T00:00:00Z" map)
        (expect (org-canvas--sync-remote-drifted-p
                 "61" (list :remote-updated map :baseline baseline) "Lab 1")
                :to-be nil)))

    (it "is inert without a remote snapshot"
      (expect (org-canvas--sync-remote-drifted-p
               "61" (list :remote-updated nil :baseline baseline) "Lab 1")
              :to-be nil))

    (it "is inert without a baseline"
      (let ((map (make-hash-table :test 'equal)))
        (puthash "61" "2026-08-25T00:00:00Z" map)
        (expect (org-canvas--sync-remote-drifted-p
                 "61" (list :remote-updated map :baseline nil) "Lab 1")
                :to-be nil)))

    (it "is inert for an id Canvas does not know"
      (let ((map (make-hash-table :test 'equal)))
        (expect (org-canvas--sync-remote-drifted-p
                 "61" (list :remote-updated map :baseline baseline) "Lab 1")
                :to-be nil)))))

(describe "org-canvas--sync-write-push-header"
  (it "stamps the header from the newest remote timestamp, not the local clock"
    ;; The header is only ever compared against remote timestamps, so it has
    ;; to be expressed in Canvas time or clock skew produces false conflicts.
    (let ((file (make-temp-file "hdr-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Item\n"))
            (org-canvas--sync-write-push-header
             file (list :remote-times (list (list "2026-08-01T00:00:00Z"
                                                  "2026-08-25T10:30:45Z"
                                                  "2026-08-10T00:00:00Z"))))
            (with-current-buffer (find-file-noselect file)
              (let ((header (org-canvas--pull-read-file-header)))
                ;; Rounded up to the next minute: rounding down would put the
                ;; header before the push it records.
                (expect header :to-equal
                        (format-time-string
                         "[%Y-%m-%d %a %H:%M]"
                         (time-add (date-to-time "2026-08-25T10:30:45Z") 60))))
              (kill-buffer)))
        (delete-file file))))

  (it "writes nothing when the run pushed nothing"
    (let ((file (make-temp-file "hdr-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Item\n"))
            (org-canvas--sync-write-push-header file (list :remote-times (list nil)))
            (with-current-buffer (find-file-noselect file)
              (expect (org-canvas--pull-read-file-header) :to-be nil)
              (kill-buffer)))
        (delete-file file)))))

(describe "org-canvas--sync-note-remote-time"
  (it "collects updated_at from a push response"
    (let ((ref (list nil)))
      (org-canvas--sync-note-remote-time
       '((id . 1) (updated_at . "2026-08-25T10:30:45Z")) (list :remote-times ref))
      (expect (car ref) :to-equal '("2026-08-25T10:30:45Z"))))

  (it "is a no-op for a single-entry push with no accumulator"
    (expect (org-canvas--sync-note-remote-time
             '((id . 1) (updated_at . "2026-08-25T10:30:45Z")) nil)
            :not :to-throw)))

(describe "org-canvas--sync-warn-unverified-skips"
  (it "says so when there is no baseline yet"
    (let ((warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--sync-warn-unverified-skips
         "assignments" (list :skip 61) (list :baseline nil :remote-updated nil))
        (expect (car warnings) :to-match "61 assignments")
        (expect (car warnings) :to-match "no #\\+LAST_SYNCED baseline"))))

  (it "says so when the snapshot could not be fetched"
    (let ((warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--sync-warn-unverified-skips
         "assignments" (list :skip 1)
         (list :baseline (current-time) :remote-updated nil))
        (expect (car warnings) :to-match "remote snapshot was unavailable"))))

  (it "stays quiet when the snapshot was consulted"
    (let ((warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--sync-warn-unverified-skips
         "assignments" (list :skip 61)
         (list :baseline (current-time)
               :remote-updated (make-hash-table :test 'equal)))
        (expect warnings :to-be nil)))))

(describe "the payload-hash skip consults Canvas (issue #48)"
  ;; The reported failure: 59 assignments were published in the web UI, the
  ;; next sync reported "0 pushed, 61 skipped", and nothing said the Org
  ;; files and Canvas now disagreed.
  (defun test-sync-48--run (file remote-updated)
    "Run the pipeline over FILE with REMOTE-UPDATED as the snapshot.
Returns the list of titles that reached the push stage."
    (let ((pushed nil))
      (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                ((symbol-function 'org-canvas--sync-payload-hash)
                 (lambda (&rest _) "SAME"))
                ((symbol-function 'org-canvas--sync-fetch-remote-snapshot)
                 (lambda (&rest _)
                   (and remote-updated (list :updated remote-updated))))
                ((symbol-function 'org-canvas--sync-log-summary) #'ignore)
                ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                ((symbol-function 'org-canvas--save-buffer) #'ignore))
               (org-canvas--sync-run-pipeline (list
					       :feature "assignments" :file file :query "LEVEL=1"
					       :parse (lambda () (list :title (org-get-heading t t t t)
								       :canvas-id (org-entry-get (point) "CANVAS_ID")))
					       :build (lambda (_data) '((name . "x")))
					       :push (lambda (data _payload &optional _ctx)
						       (push (plist-get data :title) pushed)
						       '((id . 61) (updated_at . "2026-08-25T00:00:00Z")))
					       :finalize #'ignore)))
      (nreverse pushed)))

  (it "skips an unchanged entry Canvas has not touched"
    (let ((file (make-temp-file "drift-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "#+LAST_SYNCED: [2026-08-19 Wed 12:00]\n"
                      "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n"
                      ":PAYLOAD_HASH: SAME\n:END:\n"))
            (let ((map (make-hash-table :test 'equal)))
              (puthash "61" "2026-08-01T00:00:00Z" map)
              (expect (test-sync-48--run file map) :to-be nil)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "pushes an unchanged entry that Canvas has since modified"
    (let ((file (make-temp-file "drift-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "#+LAST_SYNCED: [2026-08-19 Wed 12:00]\n"
                      "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n"
                      ":PAYLOAD_HASH: SAME\n:END:\n"))
            (let ((map (make-hash-table :test 'equal)))
              (puthash "61" "2026-08-25T00:00:00Z" map)
              (expect (test-sync-48--run file map) :to-equal '("Lab 1"))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "detects drift on a push-only course, with no file header at all"
    ;; The reported course had never been pulled, so #+LAST_SYNCED did not
    ;; exist.  The per-entry CANVAS_UPDATED_AT written by finalize is what
    ;; makes the comparison possible there.
    (let ((file (make-temp-file "drift-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n"
                      ":CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z\n"
                      ":PAYLOAD_HASH: SAME\n:END:\n"))
            (let ((map (make-hash-table :test 'equal)))
              (puthash "61" "2026-08-25T00:00:00Z" map)
              (expect (test-sync-48--run file map) :to-equal '("Lab 1"))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "leaves a push-only entry alone when Canvas matches what we recorded"
    (let ((file (make-temp-file "drift-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n"
                      ":CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z\n"
                      ":PAYLOAD_HASH: SAME\n:END:\n"))
            (let ((map (make-hash-table :test 'equal)))
              (puthash "61" "2026-08-19T13:19:49Z" map)
              (expect (test-sync-48--run file map) :to-be nil)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "still skips when detection is off"
    (let ((file (make-temp-file "drift-" nil ".org"))
          (org-canvas-detect-conflicts nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "#+LAST_SYNCED: [2026-08-19 Wed 12:00]\n"
                      "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n"
                      ":PAYLOAD_HASH: SAME\n:END:\n"))
            ;; fetch-remote-updated is mocked but run-pipeline must not even
            ;; ask for it, so the snapshot never reaches the skip check.
            (expect (test-sync-48--run file nil) :to-be nil))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "a push run writes the #+LAST_SYNCED baseline (issue #48)"
  (it "gives a push-only file the header conflict detection needs"
    (let ((file (make-temp-file "baseline-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                      ((symbol-function 'org-canvas--sync-log-summary) #'ignore)
                      ((symbol-function 'org-canvas--sync-fetch-remote-snapshot)
                       (lambda (&rest _) nil))
                      ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore))
		     (org-canvas--sync-run-pipeline (list
						     :feature "assignments" :file file :query "LEVEL=1"
						     :parse (lambda () (list :title (org-get-heading t t t t)
									     :canvas-id (org-entry-get (point) "CANVAS_ID")))
						     :build (lambda (_data) '((name . "x")))
						     :push (lambda (_data _payload &optional _ctx)
							     '((id . 61) (updated_at . "2026-08-25T10:30:45Z")))
						     :finalize #'ignore)))
            (with-current-buffer (find-file-noselect file)
              (expect (org-canvas--pull-read-file-header) :to-be-truthy)
              (kill-buffer)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "leaves the file alone during a dry run"
    (let ((file (make-temp-file "baseline-" nil ".org"))
          (org-canvas--dry-run t))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                      ((symbol-function 'org-canvas--sync-log-summary) #'ignore)
                      ((symbol-function 'org-canvas--sync-fetch-remote-snapshot)
                       (lambda (&rest _) nil))
                      ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore))
		     (org-canvas--sync-run-pipeline (list
						     :feature "assignments" :file file :query "LEVEL=1"
						     :parse (lambda () (list :title (org-get-heading t t t t)
									     :canvas-id (org-entry-get (point) "CANVAS_ID")))
						     :build (lambda (_data) '((name . "x")))
						     :push (lambda (&rest _) (error "Must not push during a dry run"))
						     :finalize #'ignore)))
            (with-current-buffer (find-file-noselect file)
              (expect (org-canvas--pull-read-file-header) :to-be nil)
              (kill-buffer)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas--conflict-baseline-source (issue #86)"
  (it "labels the entry's own CANVAS_UPDATED_AT"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-08-31 Mon 12:43]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
"
     (re-search-forward "^\\* ")
     (org-back-to-heading)
     (let ((source (org-canvas--conflict-baseline-source (point))))
       (expect (car source)
               :to-equal (org-canvas--parse-iso8601-time "2026-08-19T13:19:49Z"))
       (expect (cdr source) :to-equal "CANVAS_UPDATED_AT 2026-08-19T13:19:49Z"))))

  (it "labels the file header when the entry has no CANVAS_UPDATED_AT"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (re-search-forward "^\\* ")
     (org-back-to-heading)
     (let ((source (org-canvas--conflict-baseline-source (point))))
       (expect (car source)
               :to-equal (encode-time (org-parse-time-string "[2026-08-19 Wed 09:59]")))
       (expect (cdr source)
               :to-equal "#+LAST_SYNCED [2026-08-19 Wed 09:59] (entry has no CANVAS_UPDATED_AT)"))))

  (it "labels a caller-supplied fallback as the header, formatted"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading)
     (let* ((fallback (encode-time (org-parse-time-string "[2026-08-19 Wed 09:59]")))
            (source (org-canvas--conflict-baseline-source (point) fallback)))
       (expect (car source) :to-equal fallback)
       ;; The day name is locale-dependent, so it is not pinned.
       (expect (cdr source)
               :to-match "\\`#\\+LAST_SYNCED \\[2026-08-19 [A-Za-z]+ 09:59\\] (entry has no CANVAS_UPDATED_AT)\\'"))))

  (it "is nil for an entry with no baseline at all"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--conflict-baseline-source (point)) :to-be nil)
     (expect (org-canvas--conflict-baseline (point)) :to-be nil))))

(describe "org-canvas--last-synced-header"
  (it "reads the header through a marker from another buffer"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
"
     (re-search-forward "^\\* ")
     (let ((m (point-marker)))
       (with-temp-buffer
         (expect (org-canvas--last-synced-header m)
                 :to-equal "[2026-08-19 Wed 09:59]")))))

  (it "returns nil for nil"
    (expect (org-canvas--last-synced-header nil) :to-be nil)))

(describe "org-canvas--conflict-check log line (issue #86)"
  (defun test-conflict-86--check (content &optional title)
    "Run the conflict check for the heading in CONTENT against a newer remote.
Returns (RESULT . WARNINGS)."
    (with-org-canvas-test-config
      (with-temp-org-buffer content
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let ((warnings nil) (result nil))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (&rest _)
                      '((id . 61) (updated_at . "2026-08-31T15:00:53Z"))))
                   ((symbol-function 'org-canvas--log-warning)
                    (lambda (_logger fmt &rest args)
                      (push (apply #'format fmt args) warnings))))
           ;; A bare position, which used to print nil for the header.
           (setq result (org-canvas--conflict-check "assignments" "61" (point) title)))
         (cons result warnings)))))

  (it "names the entry and the CANVAS_UPDATED_AT it compared, not the file header"
    (let ((run (test-conflict-86--check
                "#+LAST_SYNCED: [2026-08-31 Mon 12:43]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
" "Lab 1")))
      (expect (car (car run)) :to-equal 'conflict)
      (expect (car (cdr run))
              :to-equal "[Conflict] 'Lab 1': remote updated_at 2026-08-31T15:00:53Z is newer than CANVAS_UPDATED_AT 2026-08-19T13:19:49Z")))

  (it "names the header when that is what it compared"
    (let ((run (test-conflict-86--check
                "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
" "Lab 1")))
      (expect (car (car run)) :to-equal 'conflict)
      (expect (car (cdr run))
              :to-match "is newer than #\\+LAST_SYNCED \\[2026-08-19 Wed 09:59\\] (entry has no CANVAS_UPDATED_AT)$")
      (expect (car (cdr run)) :not :to-match "is nil")))

  (it "falls back to endpoint/id when no title is given"
    (let ((run (test-conflict-86--check
                "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
")))
      (expect (car (cdr run)) :to-match "\\`\\[Conflict\\] 'assignments/61':"))))

(describe "org-canvas--sync-warn-unverified-skips during a dry run (issue #84)"
  (it "says would-sync entries were not checked for conflicts without a snapshot"
    (let ((warnings nil) (org-canvas--dry-run t))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--sync-warn-unverified-skips
         "assignments" (list :skip 0 :dry-run 37)
         (list :baseline (current-time) :remote-updated nil))
        (expect (length warnings) :to-equal 1)
        (expect (car warnings)
                :to-match "37 assignments entries reported as would-sync were not checked for conflicts — the remote snapshot was unavailable"))))

  (it "stays quiet about previews outside a dry run"
    (let ((warnings nil) (org-canvas--dry-run nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--sync-warn-unverified-skips
         "assignments" (list :skip 0 :dry-run 37)
         (list :baseline (current-time) :remote-updated nil))
        (expect warnings :to-be nil)))))

(describe "org-canvas--sync-fetch-remote-snapshot (issue #85)"
  (it "indexes titles alongside updated_at, keeping every holder of a title"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((id . 1) (name . "R11") (updated_at . "2026-08-19T13:19:49Z"))
                     ((id . 2) (name . "R11") (updated_at . "2026-08-01T00:00:00Z"))
                     ((id . 3) (name . "R12"))))))
        (let* ((snapshot (org-canvas--sync-fetch-remote-snapshot "assignments"))
               (titles (plist-get snapshot :titles)))
          (expect (gethash "1" (plist-get snapshot :updated))
                  :to-equal "2026-08-19T13:19:49Z")
          (expect (mapcar (lambda (i) (alist-get 'id i)) (gethash "R11" titles))
                  :to-equal '(1 2))
          (expect (length (gethash "R12" titles)) :to-equal 1)
          (expect (gethash "R13" titles) :to-be nil)))))

  (it "keys pages by title even though their id is a url"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _)
                   '(((url . "welcome") (title . "Welcome")
                      (updated_at . "2026-08-19T13:19:49Z"))))))
        (let ((titles (plist-get (org-canvas--sync-fetch-remote-snapshot "pages") :titles)))
          (expect (alist-get 'url (car (gethash "Welcome" titles))) :to-equal "welcome")))))

  (it "warns that creates go unchecked when the list request fails"
    (with-org-canvas-test-config
      (let ((warnings nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (error "Connection refused")))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) warnings))))
          (expect (org-canvas--sync-fetch-remote-snapshot "assignments") :to-be nil)
          (expect (car warnings) :to-match "creates will not be checked"))))))

(describe "org-canvas--sync-remote-items-titled"
  (it "is nil without a snapshot"
    (expect (org-canvas--sync-remote-items-titled "R11" (list :remote-titles nil))
            :to-be nil))

  (it "returns the holders of a title"
    (let ((titles (make-hash-table :test 'equal)))
      (puthash "R11" '(((id . 5))) titles)
      (expect (org-canvas--sync-remote-items-titled "R11" (list :remote-titles titles))
              :to-equal '(((id . 5)))))))

(describe "org-canvas--push-remote-items-titled (issue #85)"
  (it "reads the sync's title index without calling find-fn"
    (let ((titles (make-hash-table :test 'equal)))
      (puthash "R11" '(((id . 5))) titles)
      (let ((ctx (org-canvas--sync-make-ctx :remote-titles titles))
            (find-fn (lambda (_) (error "Must not be asked"))))
        (expect (org-canvas--push-remote-items-titled "R11" find-fn ctx) :to-equal '(((id . 5))))
        (expect (org-canvas--push-remote-items-titled "R12" find-fn ctx) :to-be nil))))

  (it "checks nothing when a sync had no snapshot"
    (let ((ctx (org-canvas--sync-make-ctx :remote-titles 'none)))
      (expect (org-canvas--push-remote-items-titled "R11" (lambda (_) '((id . 5))) ctx)
              :to-be nil)))

  (it "asks find-fn outside a sync"
    (let ((ctx (org-canvas--sync-make-ctx)))
      (expect (org-canvas--push-remote-items-titled "R11" (lambda (_) '((id . 5))) ctx)
              :to-equal '(((id . 5))))
      (expect (org-canvas--push-remote-items-titled "R11" (lambda (_) '((id . 5))))
              :to-equal '(((id . 5))))
      (expect (org-canvas--push-remote-items-titled "R11" (lambda (_) nil) ctx) :to-be nil)
      (expect (org-canvas--push-remote-items-titled "R11" nil) :to-be nil))))

(describe "org-canvas--push-item-id"
  (it "reads id, or url for pages"
    (expect (org-canvas--push-item-id '((id . 42)) :canvas-id) :to-equal "42")
    (expect (org-canvas--push-item-id '((id . 42) (url . "welcome")) :canvas-url)
            :to-equal "welcome")
    (expect (org-canvas--push-item-id '((title . "x")) :canvas-id) :to-be nil)))

(describe "org-canvas--push-adopt-item (issue #85)"
  (it "stamps the heading and the data with the item's id and clock"
    (with-temp-org-buffer "* R11\n"
      (org-back-to-heading)
      (let ((data (list :title "R11" :canvas-id nil :pom (point-marker))))
        (expect (org-canvas--push-adopt-item
                 data :canvas-id "R11"
                 '((id . 2563810) (updated_at . "2026-08-28T10:00:00Z")))
                :to-equal "2563810")
        (expect (plist-get data :canvas-id) :to-equal "2563810")
        (expect (org-entry-get (point) "CANVAS_ID") :to-equal "2563810")
        (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
                :to-equal "2026-08-28T10:00:00Z"))))

  (it "stamps CANVAS_URL for a page, and no clock when the item has none"
    (with-temp-org-buffer "* Welcome\n"
      (org-back-to-heading)
      (let ((data (list :title "Welcome" :canvas-url nil :pom (point-marker))))
        (org-canvas--push-adopt-item data :canvas-url "Welcome" '((url . "welcome")))
        (expect (org-entry-get (point) "CANVAS_URL") :to-equal "welcome")
        (expect (org-entry-get (point) "CANVAS_UPDATED_AT") :to-be nil)))))

(describe "org-canvas--push-guard-duplicate (issue #85)"
  (defun test-dup-85--titles (&rest items)
    "Return a title index holding ITEMS under R11."
    (let ((titles (make-hash-table :test 'equal)))
      (puthash "R11" items titles)
      titles))

  (it "does not look when the strategy is create"
    (let ((org-canvas-duplicate-title-strategy 'create)
          (ctx (org-canvas--sync-make-ctx :remote-titles nil)))
      (expect (org-canvas--push-guard-duplicate
               (list :pom 1) :canvas-id "R11" (lambda (_) (error "Must not look")))
              :to-be nil)))

  (it "does not look for data it could not stamp"
    (let ((org-canvas-duplicate-title-strategy nil)
          (ctx (org-canvas--sync-make-ctx :remote-titles nil)))
      (expect (org-canvas--push-guard-duplicate
               (list :title "R11") :canvas-id "R11" (lambda (_) (error "Must not look")))
              :to-be nil)))

  (it "is nil when Canvas has no such title"
    (let ((org-canvas-duplicate-title-strategy nil)
          (ctx (org-canvas--sync-make-ctx :remote-titles (make-hash-table :test 'equal))))
      (expect (org-canvas--push-guard-duplicate (list :pom 1) :canvas-id "R11" nil ctx)
              :to-be nil)))

  (it "adopts a single holder"
    (with-temp-org-buffer "* R11\n"
      (org-back-to-heading)
      (let ((org-canvas-duplicate-title-strategy 'adopt)
            (ctx (org-canvas--sync-make-ctx :remote-titles
             (test-dup-85--titles '((id . 2563810) (updated_at . "2026-08-28T10:00:00Z")))))
            (data (list :title "R11" :canvas-id nil :pom (point-marker))))
        (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
          (expect (org-canvas--push-guard-duplicate data :canvas-id "R11" nil ctx)
                  :to-equal "2563810"))
        (expect (org-entry-get (point) "CANVAS_ID") :to-equal "2563810"))))

  (it "skips an ambiguous title even under adopt, and says why"
    (let ((org-canvas-duplicate-title-strategy 'adopt)
          (ctx (org-canvas--sync-make-ctx :remote-titles
           (test-dup-85--titles '((id . 1)) '((id . 2)))))
          (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
        (expect (org-canvas--push-guard-duplicate (list :pom 1) :canvas-id "R11" nil ctx)
                :to-equal 'skip))
      (expect warnings :to-contain "[Duplicate] 'R11' is held by 2 Canvas items (1, 2); cannot adopt one, skipping")
      (expect (car warnings) :to-match "Skipping 'R11' — Canvas already holds it as id 1, 2; adopt it with M-x org-canvas-adopt-at-point (which stamps CANVAS_ID), or rename")))

  (it "skips and names the property to stamp for a page"
    (let ((org-canvas-duplicate-title-strategy 'skip)
          (ctx (org-canvas--sync-make-ctx :remote-titles (test-dup-85--titles '((url . "r11")))))
          (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
        (expect (org-canvas--push-guard-duplicate (list :pom 1) :canvas-url "R11" nil ctx)
                :to-equal 'skip))
      (expect (car warnings) :to-match "as id r11; adopt it with M-x org-canvas-adopt-at-point (which stamps CANVAS_URL), or rename")))

  (it "creates when told, saying so"
    (let ((org-canvas-duplicate-title-strategy nil)
          (ctx (org-canvas--sync-make-ctx :duplicate-apply-all 'create
                                          :remote-titles (test-dup-85--titles '((id . 1)))))
          (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
        (expect (org-canvas--push-guard-duplicate (list :pom 1) :canvas-id "R11" nil ctx)
                :to-be nil))
      (expect (car warnings)
              :to-equal "[Duplicate] Creating 'R11' although Canvas already holds it as id 1"))))

(describe "org-canvas--push-to-api duplicate guard (issue #85)"
  (it "returns duplicate instead of POSTing when the heading is skipped"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer "* R11\n"
          (org-back-to-heading)
          (let ((titles (make-hash-table :test 'equal)))
            (puthash "R11" '(((id . 2563810) (name . "R11"))) titles)
            (let* ((ctx (org-canvas--sync-make-ctx :remote-titles titles))
                   (org-canvas-duplicate-title-strategy 'skip)
                   (data (list :title "R11" :canvas-id nil :pom (point-marker)))
                   (result (org-canvas--push-to-api data '((name . "R11"))
                                                    :endpoint "assignments" :ctx ctx)))
              (expect result :to-equal 'duplicate)
              (expect (test-org-canvas-api-called-p 'POST "assignments") :to-be nil)))))))

  (it "adopts the existing item and updates it in place"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer "* R11\n"
          (org-back-to-heading)
          (let ((titles (make-hash-table :test 'equal)))
            (puthash "R11" '(((id . 2563810) (name . "R11")
                              (updated_at . "2026-08-28T10:00:00Z")))
                     titles)
            (let* ((ctx (org-canvas--sync-make-ctx :remote-titles titles))
                   (org-canvas-duplicate-title-strategy 'adopt)
                   (data (list :title "R11" :canvas-id nil :pom (point-marker))))
              (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
                (org-canvas--push-to-api data '((name . "R11")) :endpoint "assignments" :ctx ctx))
              (expect (test-org-canvas-api-called-p 'PUT "assignments/2563810") :to-be-truthy)
              (expect (test-org-canvas-api-called-p 'POST "assignments$") :to-be nil)
              (expect (org-entry-get (point) "CANVAS_ID") :to-equal "2563810")))))))

  (it "creates as before when nothing on Canvas has the title"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer "* R11\n"
          (org-back-to-heading)
          (let* ((ctx (org-canvas--sync-make-ctx :remote-titles (make-hash-table :test 'equal)))
                 (org-canvas-duplicate-title-strategy nil)
                 (data (list :title "R11" :canvas-id nil :pom (point-marker))))
            (org-canvas--push-to-api data '((name . "R11")) :endpoint "assignments" :ctx ctx)
            (expect (test-org-canvas-api-called-p 'POST "assignments") :to-be-truthy))))))

  (it "leaves a dry run alone"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer "* R11\n"
          (org-back-to-heading)
          (let ((titles (make-hash-table :test 'equal)))
            (puthash "R11" '(((id . 2563810))) titles)
            (let* ((org-canvas--dry-run t)
                   (ctx (org-canvas--sync-make-ctx :remote-titles titles))
                   (org-canvas-duplicate-title-strategy 'adopt)
                   (data (list :title "R11" :canvas-id nil :pom (point-marker))))
              (expect (org-canvas--push-to-api data '((name . "R11")) :endpoint "assignments" :ctx ctx)
                      :to-equal org-canvas--dry-run-response)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
              (expect (test-org-canvas-api-call-count) :to-equal 0))))))))

(describe "org-canvas--sync-remote-updated-index modified field (issue #94)"
  (it "reads the declared field instead of updated_at"
    (let ((map (org-canvas--sync-remote-updated-index
                '(((id . 1) (updated_at . "2026-09-01T12:12:24Z")
                   (modified_at . "2026-08-31T18:34:37Z")))
                'id 'modified_at)))
      (expect (gethash "1" map) :to-equal "2026-08-31T18:34:37Z")))

  (it "keeps updated_at as the default"
    (let ((map (org-canvas--sync-remote-updated-index
                '(((id . 1) (updated_at . "2026-09-01T12:12:24Z")))
                'id)))
      (expect (gethash "1" map) :to-equal "2026-09-01T12:12:24Z"))))

(describe "org-canvas--sync-item-params (issue #273)"
  (it "answers the registered feature's item-params from the run context"
    (expect (org-canvas--sync-item-params
             (org-canvas--sync-make-ctx :feature-name "assignments"))
            :to-contain '("override_assignment_dates" . "false")))

  (it "is nil without a context, and for a feature outside the registry"
    (expect (org-canvas--sync-item-params nil) :to-be nil)
    (expect (org-canvas--sync-item-params
             (org-canvas--sync-make-ctx :feature-name "no-such-feature"))
            :to-be nil)
    (expect (org-canvas--sync-item-params
             (org-canvas--sync-make-ctx :feature-name "pages"))
            :to-be nil)))

(describe "org-canvas--conflict-check modified field (issue #94)"
  (defconst test-mod-94--entry "* syllabus.pdf
:PROPERTIES:
:CANVAS_ID: 31495932
:CANVAS_UPDATED_AT: 2026-08-31T18:34:37Z
:END:
")

  (it "ignores a metadata-only touch when told the content field"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-mod-94--entry
        (org-back-to-heading)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _)
                     '((id . 31495932)
                       (updated_at . "2026-09-01T12:12:24Z")
                       (modified_at . "2026-08-31T18:34:37Z")))))
          (expect (org-canvas--conflict-check "files" "31495932" (point)
                                              "syllabus.pdf" 'modified_at)
                  :to-be nil)))))

  (it "still flags a real content change, naming the field it compared"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-mod-94--entry
        (org-back-to-heading)
        (let ((warnings nil))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (lambda (&rest _)
                       '((id . 31495932)
                         (updated_at . "2026-09-01T12:12:24Z")
                         (modified_at . "2026-09-01T09:00:00Z"))))
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_l fmt &rest args)
                       (push (apply #'format fmt args) warnings))))
            (expect (car (org-canvas--conflict-check "files" "31495932" (point)
                                                     "syllabus.pdf" 'modified_at))
                    :to-equal 'conflict))
          (expect (car warnings)
                  :to-equal "[Conflict] 'syllabus.pdf': remote modified_at 2026-09-01T09:00:00Z is newer than CANVAS_UPDATED_AT 2026-08-31T18:34:37Z"))))))

;;; A scheduled date explains a newer timestamp (issue #453)

(defconst test-453--entry "* Column
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-10-05T13:04:21Z
:END:
"
  "An assignment heading stamped at its last push, before it unlocked.")

(defun test-453--time (iso)
  "Parse ISO for the scheduled-bump specs."
  (org-canvas--parse-iso8601-time iso))

(defun test-453--reply (&rest _)
  "The live column, bumped five seconds after it unlocked."
  '((id . 61) (updated_at . "2026-10-05T14:10:05Z")
    (unlock_at . "2026-10-05T14:10:00Z")))

(describe "org-canvas--conflict-scheduled-bump (issue #453)"
  (let ((base "2026-10-05T13:04:21Z")
        (dates '(("unlock_at" . "2026-10-05T14:10:00Z"))))
    (it "explains a bump a few seconds after a date passed since the baseline"
      (expect (org-canvas--conflict-scheduled-bump
               (test-453--time base) (test-453--time "2026-10-05T14:10:05Z")
               dates)
              :to-equal '("unlock_at" . "2026-10-05T14:10:00Z")))

    (it "explains a bump at the date itself"
      (expect (org-canvas--conflict-scheduled-bump
               (test-453--time base) (test-453--time "2026-10-05T14:10:00Z")
               dates)
              :to-be-truthy))

    (it "does not explain a bump past the window"
      (expect (org-canvas--conflict-scheduled-bump
               (test-453--time base)
               (test-453--time "2026-10-05T14:11:01Z") dates)
              :to-be nil))

    (it "does not explain a bump before the date"
      (expect (org-canvas--conflict-scheduled-bump
               (test-453--time base)
               (test-453--time "2026-10-05T14:09:59Z") dates)
              :to-be nil))

    (it "does not explain a date that had passed before the baseline"
      (expect (org-canvas--conflict-scheduled-bump
               (test-453--time "2026-10-05T14:10:00Z")
               (test-453--time "2026-10-05T14:10:05Z") dates)
              :to-be nil))

    (it "skips a date that does not parse and finds a later one"
      (expect (org-canvas--conflict-scheduled-bump
               (test-453--time base) (test-453--time "2026-10-05T15:00:03Z")
               '(("unlock_at" . :null)
                 ("override 9 lock_at" . "2026-10-05T15:00:00Z")))
              :to-equal '("override 9 lock_at" . "2026-10-05T15:00:00Z")))))

(describe "org-canvas--conflict-check with scheduled dates (issue #453)"
  (it "lets the push past a bump its unlock date explains, and says why"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-453--entry
        (org-back-to-heading)
        (let ((infos nil))
          (cl-letf (((symbol-function 'org-canvas-api-request) #'test-453--reply)
                    ((symbol-function 'org-canvas--log-info)
                     (lambda (_l fmt &rest args)
                       (push (apply #'format fmt args) infos))))
            (expect (org-canvas--conflict-check
                     "assignments" "61" (point) "Column" nil nil nil
                     (lambda (item)
                       (list (cons "unlock_at" (alist-get 'unlock_at item)))))
                    :to-be nil))
          (expect (car infos)
                  :to-equal "[Conflict] 'Column': remote updated_at 2026-10-05T14:10:05Z is newer than CANVAS_UPDATED_AT 2026-10-05T13:04:21Z, but it follows unlock_at 2026-10-05T14:10:00Z, a bump Canvas makes itself; not a conflict")))))

  (it "still reports the conflict without a scheduled-dates function"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-453--entry
        (org-back-to-heading)
        (cl-letf (((symbol-function 'org-canvas-api-request) #'test-453--reply))
          (expect (car (org-canvas--conflict-check
                        "assignments" "61" (point) "Column"))
                  :to-equal 'conflict)))))

  (it "keeps the conflict, with a warning, when the dates cannot be read"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-453--entry
        (org-back-to-heading)
        (let ((warnings nil))
          (cl-letf (((symbol-function 'org-canvas-api-request) #'test-453--reply)
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_l fmt &rest args)
                       (push (apply #'format fmt args) warnings))))
            (expect (car (org-canvas--conflict-check
                          "assignments" "61" (point) "Column" nil nil nil
                          (lambda (_item) (error "HTTP 500"))))
                    :to-equal 'conflict))
          (expect (cl-some (lambda (w)
                             (string-match-p "Could not read the scheduled dates of 'Column' (HTTP 500)" w))
                           warnings)
                  :to-be-truthy))))))

(describe "org-canvas--sync-item-scheduled-fn (issue #453)"
  (it "answers the running feature's scheduled-dates function"
    (let ((org-canvas--feature-registry
           (list (list :name "Widgets" :scheduled-dates-fn #'ignore))))
      (expect (org-canvas--sync-item-scheduled-fn
               (org-canvas--sync-make-ctx :feature-name "widgets"))
              :to-be #'ignore)))

  (it "answers nil without a run context"
    (expect (org-canvas--sync-item-scheduled-fn nil) :to-be nil)))

(describe "org-canvas--push-to-api over a scheduled bump (issue #453)"
  (it "sends the PUT that a bump at the item's unlock date stopped before"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-453--entry
        (org-back-to-heading)
        (let ((org-canvas-detect-conflicts t)
              (org-canvas-conflict-strategy nil)
              (org-canvas--feature-registry
               (list (list :name "Widgets" :endpoint "widgets"
                           :scheduled-dates-fn
                           (lambda (item)
                             (list (cons "unlock_at"
                                         (alist-get 'unlock_at item)))))))
              (calls nil))
          (cl-letf (((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _)
                       (push (cons method url) calls)
                       (if (eq method 'GET)
                           (test-453--reply)
                         '((id . 61) (updated_at . "2026-10-06T09:00:00Z"))))))
            (org-canvas--push-to-api
             (list :title "Column" :canvas-id "61" :pom (point-marker))
             '((name . "Column"))
             :endpoint "widgets"
             :ctx (org-canvas--sync-make-ctx :feature-name "widgets")))
          (expect (mapcar #'car (reverse calls)) :to-equal '(GET PUT)))))))

(describe "org-canvas--finalize-item :updated-field (issue #94)"
  (it "stamps CANVAS_UPDATED_AT from the declared field"
    (with-temp-org-buffer "* syllabus.pdf\n"
      (org-back-to-heading)
      (org-canvas--finalize-item
       (list :title "syllabus.pdf" :pom (point))
       '((id . 31495932) (updated_at . "2026-09-01T12:12:24Z")
         (modified_at . "2026-08-31T18:34:37Z"))
       :updated-field 'modified_at)
      (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
              :to-equal "2026-08-31T18:34:37Z"))))

(describe "stamp failure after a landed push (issue #97)"
  (it "says the PUT landed and only the stamp failed, in the pipeline"
    (with-temp-org-buffer "* Lab 1\n"
      (org-back-to-heading)
      (let* ((logged nil)
             (counters (list :success 0 :skip 0 :fail 0))
             (ctx (list :parse-fn (lambda () (list :title "Lab 1" :canvas-id "61"
                                                   :pom (point-marker)))
                        :build-fn (lambda (_d) '((name . "x")))
                        :push-fn (lambda (_d _p &optional _ctx) '((id . 61)))
                        :finalize-fn (lambda (_d _r &optional _ctx) (error "disk full"))
                        :feature-name "assignments" :feature-upper "ASSIGNMENTS"
                        :total-count 1 :counters counters :synced-ids (list nil))))
        (cl-letf (((symbol-function 'org-canvas--log-error)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged)))
                  ((symbol-function 'message) #'ignore))
          (org-canvas--sync-process-entry (point-marker) ctx))
        (expect (plist-get counters :fail) :to-equal 1)
        (expect (cl-find-if
                 (lambda (l)
                   (string-match-p "\\[Stamp\\] The push of 'Lab 1' landed on Canvas" l))
                 logged)
                :to-be-truthy))))

  (it "says the same for a single-entry push"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* Lab 1\n"
        (org-back-to-heading)
        (let ((logged nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'org-canvas--log-error)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged)))
                    ((symbol-function 'message) #'ignore))
            (condition-case nil
                (org-canvas--push-at-point-runtime (list
						    :feature "assignment"
						    :parse (lambda () (list :title "Lab 1" :canvas-id "61" :pom (point)))
						    :build (lambda (_d) '((name . "x")))
						    :push (lambda (_d _p &optional _ctx) '((id . 61)))
						    :finalize (lambda (_d _r &optional _ctx) (error "disk full"))
						    :title-key :title))
              (error nil)))
          (expect (car logged) :to-match "\\[Stamp\\].*landed on Canvas"))))))

(describe "org-canvas--adopt-stamp (issue #101)"
  (it "stamps the id, the clock, and drops the hash"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:PAYLOAD_HASH: deadbeef
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--adopt-stamp
              (point) "CANVAS_ID"
              '((id . 61) (updated_at . "2026-08-28T12:42:35Z")))
             :to-equal "61")
     (expect (org-entry-get (point) "CANVAS_ID") :to-equal "61")
     (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
             :to-equal "2026-08-28T12:42:35Z")
     (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)))

  (it "reads url for CANVAS_URL and an alternate modified field"
    (with-temp-org-buffer "* Welcome\n"
      (org-back-to-heading t)
      (expect (org-canvas--adopt-stamp
               (point) "CANVAS_URL"
               '((url . "welcome") (id . 3)
                 (updated_at . "2026-08-30T00:00:00Z")
                 (modified_at . "2026-08-28T00:00:00Z"))
               'modified_at)
              :to-equal "welcome")
      (expect (org-entry-get (point) "CANVAS_URL") :to-equal "welcome")
      (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
              :to-equal "2026-08-28T00:00:00Z")))

  (it "writes nothing for an item with no id"
    (with-temp-org-buffer "* Lab 1\n"
      (org-back-to-heading t)
      (expect (org-canvas--adopt-stamp (point) "CANVAS_ID" '((title . "x")))
              :to-be nil)
      (expect (org-entry-get (point) "CANVAS_ID") :to-be nil))))

(describe "org-canvas--sync-advance-file-header (issue #104)"
  (let ((stamp-of (lambda (iso)
                    (format-time-string "[%Y-%m-%d %a %H:%M]"
                                        (time-add (date-to-time iso) 60)))))

    (it "writes a header, rounded up a minute, when the file has none"
      (with-temp-org-buffer "* Item\n"
        (expect (org-canvas--sync-advance-file-header
                 (date-to-time "2026-09-01T14:10:30Z"))
                :to-equal (funcall stamp-of "2026-09-01T14:10:30Z"))
        (expect (org-canvas--pull-read-file-header)
                :to-equal (funcall stamp-of "2026-09-01T14:10:30Z"))))

    (it "moves an older header forward"
      (with-temp-org-buffer "* Item\n"
        (org-canvas--sync-advance-file-header (date-to-time "2026-08-19T09:58:00Z"))
        (expect (org-canvas--sync-advance-file-header
                 (date-to-time "2026-09-01T14:10:30Z"))
                :to-equal (funcall stamp-of "2026-09-01T14:10:30Z"))
        (expect (org-canvas--pull-read-file-header)
                :to-equal (funcall stamp-of "2026-09-01T14:10:30Z"))))

    (it "leaves a later header alone"
      ;; A baseline that moves backward can only manufacture conflicts.
      (with-temp-org-buffer "* Item\n"
        (org-canvas--sync-advance-file-header (date-to-time "2026-09-01T14:10:30Z"))
        (set-buffer-modified-p nil)
        (expect (org-canvas--sync-advance-file-header
                 (date-to-time "2026-08-19T09:58:00Z"))
                :to-be nil)
        (expect (buffer-modified-p) :to-be nil)
        (expect (org-canvas--pull-read-file-header)
                :to-equal (funcall stamp-of "2026-09-01T14:10:30Z"))))

    (it "does not rewrite a header already on the same minute"
      (with-temp-org-buffer "* Item\n"
        (org-canvas--sync-advance-file-header (date-to-time "2026-09-01T14:10:05Z"))
        (set-buffer-modified-p nil)
        (expect (org-canvas--sync-advance-file-header
                 (date-to-time "2026-09-01T14:10:50Z"))
                :to-be nil)
        (expect (buffer-modified-p) :to-be nil)))))

(describe "org-canvas--sync-write-push-header is forward-only (issue #104)"
  (it "keeps a header later than the run's newest remote time"
    (let ((file (make-temp-file "hdr-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "#+LAST_SYNCED: [2026-09-01 Tue 16:00]\n* Item\n"))
            (org-canvas--sync-write-push-header
             file (list :remote-times (list (list "2026-08-01T00:00:00Z"))))
            (with-current-buffer (find-file-noselect file)
              (expect (org-canvas--pull-read-file-header)
                      :to-equal "[2026-09-01 Tue 16:00]")
              (kill-buffer)))
        (delete-file file)))))

(describe "org-canvas--sync-advance-header-from-entry (issue #104)"
  (it "advances the header from the entry's CANVAS_UPDATED_AT"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-09-01T14:10:30Z
:END:
"
     (search-forward "* Lab 1")
     (org-back-to-heading t)
     (let ((logged nil))
       (cl-letf (((symbol-function 'org-canvas--log-info)
                  (lambda (_logger fmt &rest args)
                    (push (apply #'format fmt args) logged))))
         (expect (org-canvas--sync-advance-header-from-entry)
                 :to-equal (format-time-string
                            "[%Y-%m-%d %a %H:%M]"
                            (time-add (date-to-time "2026-09-01T14:10:30Z") 60))))
       (expect (car logged) :to-match "#\\+LAST_SYNCED advanced")
       ;; Point stays on the heading the caller is still stamping.
       (expect (org-get-heading t t t t) :to-equal "Lab 1"))))

  (it "does nothing for an entry with no CANVAS_UPDATED_AT"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--sync-advance-header-from-entry) :to-be nil)
     (expect (org-canvas--pull-read-file-header) :to-be nil))))

(describe "org-canvas--sync-backfill-baseline (issue #104)"
  (let ((baseline (encode-time (org-parse-time-string "[2026-08-19 Wed 12:00]"))))

    (it "stamps a legacy entry with the remote updated_at the skip verified"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (org-back-to-heading t)
       (let ((map (make-hash-table :test 'equal)))
         (puthash "61" "2026-08-01T00:00:00Z" map)
         (org-canvas--sync-backfill-baseline
          "61" "Lab 1" (list :remote-updated map :baseline baseline))
         (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
                 :to-equal "2026-08-01T00:00:00Z"))))

    (it "leaves an entry that already has a stamp alone"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-07-01T00:00:00Z
:END:
"
       (org-back-to-heading t)
       (let ((map (make-hash-table :test 'equal)))
         (puthash "61" "2026-08-01T00:00:00Z" map)
         (org-canvas--sync-backfill-baseline
          "61" "Lab 1" (list :remote-updated map :baseline baseline))
         (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
                 :to-equal "2026-07-01T00:00:00Z"))))

    (it "writes nothing without a baseline, since nothing was proven"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (org-back-to-heading t)
       (let ((map (make-hash-table :test 'equal)))
         (puthash "61" "2026-08-01T00:00:00Z" map)
         (org-canvas--sync-backfill-baseline
          "61" "Lab 1" (list :remote-updated map :baseline nil))
         (expect (org-entry-get (point) "CANVAS_UPDATED_AT") :to-be nil))))

    (it "writes nothing during a dry run"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (org-back-to-heading t)
       (let ((map (make-hash-table :test 'equal))
             (org-canvas--dry-run t))
         (puthash "61" "2026-08-01T00:00:00Z" map)
         (org-canvas--sync-backfill-baseline
          "61" "Lab 1" (list :remote-updated map :baseline baseline))
         (expect (org-entry-get (point) "CANVAS_UPDATED_AT") :to-be nil))))

    (it "writes nothing when the snapshot does not know the id"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (org-back-to-heading t)
       (org-canvas--sync-backfill-baseline
        "61" "Lab 1" (list :remote-updated (make-hash-table :test 'equal)
                           :baseline baseline))
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT") :to-be nil)))))

(describe "org-canvas--child-twins (issue #179)"
  (it "returns the matching items nothing claims, earliest position first"
    (let ((remote [((id . 3) (position . 2) (name . "Q"))
                   ((id . 1) (position . 1) (name . "Q"))
                   ((id . 2) (position . 1) (name . "Other"))
                   ((id . 4) (position . 3) (name . "Q"))]))
      (expect (mapcar (lambda (item) (alist-get 'id item))
                      (org-canvas--child-twins
                       remote (lambda (item) (equal (alist-get 'name item) "Q")) '("4")))
              :to-equal '(1 3))))

  (it "orders equal positions by id, and takes a list as well as a vector"
    (let ((remote '(((id . 7) (position . 1) (name . "Q"))
                    ((id . 6) (position . 1) (name . "Q")))))
      (expect (mapcar (lambda (item) (alist-get 'id item))
                      (org-canvas--child-twins remote (lambda (_) t) nil))
              :to-equal '(6 7))))

  (it "yields nothing for a list that could not be read"
    (expect (org-canvas--child-twins 'unknown (lambda (_) t) nil) :to-be nil))

  (it "orders string ids at one position lexically"
    (let ((remote [((id . "b2") (position . 1)) ((id . "a1") (position . 1))]))
      (expect (mapcar (lambda (item) (alist-get 'id item))
                      (org-canvas--child-twins remote (lambda (_) t) nil))
              :to-equal '("a1" "b2"))))

  (it "ignores an item without an id"
    (expect (org-canvas--child-twins [((name . "Q"))] (lambda (_) t) nil) :to-be nil)))

(describe "org-canvas--adopt-child-twin (issue #179)"
  (let ((match (lambda (item) (equal (alist-get 'name item) "Q"))))
    (it "writes the twin's id into the data and returns it"
      (let ((data (list :name "Q" :canvas-id nil)))
        (expect (org-canvas--adopt-child-twin data [((id . 9) (name . "Q"))] match nil "[T]")
                :to-equal "9")
        (expect (plist-get data :canvas-id) :to-equal "9")))

    (it "adopts nothing for stamped data, an unknown list, or the create strategy"
      (let ((remote [((id . 9) (name . "Q"))]))
        (expect (org-canvas--adopt-child-twin (list :name "Q" :canvas-id "1") remote match nil "[T]")
                :to-be nil)
        (expect (org-canvas--adopt-child-twin (list :name "Q" :canvas-id nil) 'unknown match nil "[T]")
                :to-be nil)
        (let ((org-canvas-duplicate-title-strategy 'create)
              (data (list :name "Q" :canvas-id nil)))
          (expect (org-canvas--adopt-child-twin data remote match nil "[T]") :to-be nil)
          (expect (plist-get data :canvas-id) :to-be nil))))

    (it "leaves alone a twin a sibling claims"
      (let ((data (list :name "Q" :canvas-id nil)))
        (expect (org-canvas--adopt-child-twin data [((id . 9) (name . "Q"))] match '("9") "[T]")
                :to-be nil)))

    (it "adopts the first twin, names the rest, and deletes nothing"
      (let ((data (list :name "Q" :canvas-id nil))
            (warnings nil)
            (requests nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) warnings)))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _) (push (list method url) requests) nil)))
          (expect (org-canvas--adopt-child-twin
                   data [((id . 5) (position . 2) (name . "Q"))
                         ((id . 4) (position . 1) (name . "Q"))]
                   match nil "[T]")
                  :to-equal "4"))
        (expect (car warnings) :to-match "1 more item")
        (expect (car warnings) :to-match "5")
        (expect requests :to-be nil)))))

(describe "org-canvas--sync-children"
  (it "counts successes, skips and failures, and visits every child"
    (with-temp-org-buffer "* Quiz
** One
** Two
** Three
"
      (search-forward "Quiz")
      (org-back-to-heading t)
      (let* ((visited nil)
             (counts (org-canvas--sync-children
                      (org-canvas--collect-subtree-markers (point-marker))
                      "[Test]"
                      (lambda ()
                        (let ((title (org-get-heading t t t t)))
                          (push title visited)
                          (cond ((string= title "Two") 'skip)
                                ((string= title "Three") (error "Simulated failure"))
                                (t 'ok)))))))
        (expect (length visited) :to-equal 3)
        (expect (plist-get counts :success) :to-be 1)
        (expect (plist-get counts :skip) :to-be 1)
        (expect (plist-get counts :fail) :to-be 1)
        (expect (plist-get counts :total) :to-be 3))))

  (it "logs one line per failing item, prefixed by the label"
    (with-temp-org-buffer "* Quiz
** One
** Two
"
      (search-forward "Quiz")
      (org-back-to-heading t)
      (let ((errors nil))
        (cl-letf (((symbol-function 'org-canvas--log-error)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) errors))))
          (org-canvas--sync-children
           (org-canvas--collect-subtree-markers (point-marker))
           "[Test]"
           (lambda () (error "Nope"))))
        (expect (length errors) :to-equal 2)
        (expect (car errors) :to-match "\\[Test\\] Failed: Nope")))))

(describe "org-canvas--handle-404-retry adopts the title's twin (issue #179)"
  (defun test-org-canvas-179-404--push (find-fn &optional put-url-fn)
    "Push a stamped entry whose PUT 404s; return (RESULT . REQUESTS)."
    (let ((requests nil))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest _)
                   (push (list method url) requests)
                   (cond
                    ((and (eq method 'PUT) (string-match-p "/999$" url))
                     (signal 'error '("API Request Failed (HTTP 404)")))
                    ((eq method 'PUT) '((id . 789) (title . "Stale")))
                    (t '((id . 900) (title . "Stale")))))))
        (let ((result (org-canvas--push-to-api
                       '(:title "Stale" :canvas-id "999") '((title . "Stale"))
                       :endpoint "pages" :find-fn find-fn :put-url-fn put-url-fn)))
          (cons result requests)))))

  (it "updates the item Canvas holds under the title instead of POSTing"
    (with-org-canvas-test-config
      (let* ((run (test-org-canvas-179-404--push (lambda (_title) '((id . 789)))))
             (requests (cdr run)))
        (expect (alist-get 'id (car run)) :to-equal 789)
        (expect (cl-some (lambda (r) (eq (car r) 'POST)) requests) :to-be nil)
        (expect (cl-some (lambda (r) (and (eq (car r) 'PUT)
                                          (string-match-p "pages/789$" (cadr r))))
                         requests)
                :to-be-truthy))))

  (it "builds the twin's URL with put-url-fn"
    (with-org-canvas-test-config
      (let ((requests (cdr (test-org-canvas-179-404--push
                            (lambda (_title) '((id . 789)))
                            (lambda (id) (format "https://example.test/x/%s" id))))))
        (expect (cl-some (lambda (r) (equal r '(PUT "https://example.test/x/789"))) requests)
                :to-be-truthy))))

  (it "POSTs when nothing on Canvas carries the title"
    (with-org-canvas-test-config
      (let* ((run (test-org-canvas-179-404--push (lambda (_title) nil)))
             (requests (cdr run)))
        (expect (alist-get 'id (car run)) :to-equal 900)
        (expect (cl-some (lambda (r) (eq (car r) 'POST)) requests) :to-be-truthy))))

  (it "asks nothing and POSTs under the create strategy"
    (with-org-canvas-test-config
      (let* ((org-canvas-duplicate-title-strategy 'create)
             (asked nil)
             (run (test-org-canvas-179-404--push (lambda (_title) (setq asked t) '((id . 789))))))
        (expect asked :to-be nil)
        (expect (cl-some (lambda (r) (eq (car r) 'POST)) (cdr run)) :to-be-truthy)))))

;;;; Push Echo Check (issue #349)

(defmacro test-org-canvas-with-echo-registry (&rest body)
  "Run BODY with a scratch \"echo\" feature in the property registry."
  (declare (indent 0))
  `(let ((org-canvas--property-registry (make-hash-table :test 'equal)))
     (org-canvas-register-properties
      "echo"
      :properties
      `((:org-prop "POLICY" :data-key :policy :type enum)
        (:org-prop "FLAG" :data-key :flag :type boolean)
        (:org-prop "LIMIT" :data-key :limit :type number)
        (:org-prop "DUE" :data-key :due :type timestamp)
        (:org-prop "ROLES" :data-key :roles :type csv-enum)
        (:org-prop "NESTED" :data-key :nested :type boolean
         :api-key "nested_flag"
         :remote-fn ,(lambda (item)
                       (alist-get 'on (alist-get 'nested item))))
        (:org-prop "OWNED" :data-key :owned :type string :canvas-owned t)
        (:org-prop "GROUP" :data-key :group :type link)))
     (cl-letf (((symbol-function 'org-canvas--time-zone)
                (lambda () "UTC")))
       ,@body)))

(describe "org-canvas--registry-echo-mismatches"
  (it "names a property Canvas stored otherwise than it was sent"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((policy . "keep_latest"))
               '((policy . "keep_highest")))
              :to-equal '(("POLICY" "keep_latest" "keep_highest")))))

  (it "is silent when Canvas stored every field as sent"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo"
               '((policy . "keep_latest") (flag . :json-false) (limit . 10)
                 (due . "2026-10-01T15:00:00Z") (roles . "teachers,students"))
               '((policy . "keep_latest") (flag . :null) (limit . 10.0)
                 (due . "2026-10-01T15:00:59Z")
                 (roles . ["students" "teachers"])))
              :to-be nil)))

  (it "reports booleans, numbers and timestamps in their Org spelling"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((flag . t) (limit . 3) (due . "2026-10-01T15:00:00Z"))
               '((flag . :json-false) (limit . 1) (due . :null)))
              :to-equal '(("FLAG" "true" "false")
                          ("LIMIT" "3" "1")
                          ("DUE" "<2026-10-01 Thu 15:00>" "(unset)")))))

  (it "compares a number Canvas returns as a string by its spelling"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((limit . 3)) '((limit . "3")))
              :to-be nil)))

  (it "compares CSV lists as sets"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((roles . "teachers")) '((roles . "teachers,public")))
              :to-equal '(("ROLES" "teachers" "teachers,public")))))

  (it "reads the stored side through a :remote-fn (Hard Rule 18)"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((nested_flag . t)) '((nested (on . t))))
              :to-be nil)
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((nested_flag . t)) '((nested (on . :json-false))))
              :to-equal '(("NESTED" "true" "false")))))

  (it "skips a field the payload did not carry or the reply did not report"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((policy . "keep_latest")) '((flag . t)))
              :to-be nil)))

  (it "skips Canvas-owned and link properties"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "echo" '((owned . "a") (group . 1))
               '((owned . "b") (group . 2)))
              :to-be nil)))

  (it "reads a hash-table payload keyed by symbols or strings"
    (test-org-canvas-with-echo-registry
      (let ((sent (make-hash-table :test 'equal)))
        (puthash 'policy "keep_latest" sent)
        (puthash "limit" 3 sent)
        (expect (org-canvas--registry-echo-mismatches
                 "echo" sent '((policy . "keep_highest") (limit . 3)
                               (flag . t)))
                :to-equal '(("POLICY" "keep_latest" "keep_highest"))))))

  (it "finds nothing for a feature with no registry entry"
    (test-org-canvas-with-echo-registry
      (expect (org-canvas--registry-echo-mismatches
               "nobody" '((policy . "a")) '((policy . "b")))
              :to-be nil))))

(describe "org-canvas--registry-warn-echo"
  (it "logs one warning per property Canvas changed and returns them"
    (test-org-canvas-with-echo-registry
      (let (warnings)
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) warnings))))
          (let ((result (org-canvas--registry-warn-echo
                         "echo" "Survey"
                         '((policy . "keep_latest") (flag . t))
                         '((policy . "keep_highest") (flag . :json-false)))))
            (expect (length result) :to-equal 2)
            (expect (length warnings) :to-equal 2)
            (expect (car (last warnings)) :to-equal
                    (concat "[Verify] 'Survey': sent POLICY keep_latest,"
                            " Canvas stored keep_highest")))))))

  (it "logs nothing when Canvas stored what it was sent"
    (test-org-canvas-with-echo-registry
      (let (warnings)
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (&rest args) (push args warnings))))
          (expect (org-canvas--registry-warn-echo
                   "echo" "Quiz" '((flag . t)) '((flag . t)))
                  :to-be nil)
          (expect warnings :to-be nil))))))

(provide 'org-canvas-core-push-test)
;;; org-canvas-core-push-test.el ends here
