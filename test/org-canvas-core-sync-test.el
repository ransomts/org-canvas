;;; org-canvas-core-sync-test.el --- Buttercup tests for org-canvas-core sync pipeline helpers  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(describe "with-mock-api request recording"
  (it "records :params and :timeout, not just :data"
    (with-mock-api
      (org-canvas-api-request 'GET "https://x/api/v1/courses/1/items"
                              :data '((a . 1))
                              :params '(("per_page" . "100"))
                              :timeout 42)
      (let ((call (test-org-canvas-find-api-call 'GET "items")))
        (expect (nth 2 call) :to-equal '((a . 1)))
        (expect (test-org-canvas-call-arg call :params)
                :to-equal '(("per_page" . "100")))
        (expect (test-org-canvas-call-arg call :timeout) :to-equal 42))))

  (it "exposes payload via test-org-canvas-api-call-data"
    (with-mock-api
      (org-canvas-api-request 'POST "https://x/api/v1/courses/1/pages"
                              :data '((title . "T")))
      (expect (test-org-canvas-api-call-data 'POST "pages")
              :to-equal '((title . "T")))
      (expect (test-org-canvas-api-call-data 'POST "no-such") :to-be nil))))

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

(describe "org-canvas-define-sync generated function"
  (it "syncs entries from file successfully"
    ;; Test using org-canvas-sync-pages which is a real macro-generated function
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (post-count 0))
            (with-temp-file org-file
              (insert "* Page One
:PROPERTIES:
:END:

Content one.

* Page Two
:PROPERTIES:
:END:

Content two.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq post-count (1+ post-count)))
                             '((url . "test-url")))))
                  (org-canvas-sync-pages)
                  (expect post-count :to-equal 2)
                  ;; Verify CANVAS_URL was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_URL") :to-equal "test-url"))))))
        (delete-directory temp-dir t))))

  (it "reports the number of successfully synced items"
    ;; Guards the `:success' counter increment: a broken increment would
    ;; mis-report the user-facing \"N success\" summary.
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "pages.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Page One\n:PROPERTIES:\n:END:\n\nc1.\n\n"
                      "* Page Two\n:PROPERTIES:\n:END:\n\nc2.\n"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_m _u &rest _a) '((url . "u")))))
                  (spy-on 'message :and-call-through)
                  (org-canvas-sync-pages)
                  (let ((reported nil))
                    (dolist (call (spy-calls-all-args 'message))
                      (let ((s (and (car call) (stringp (car call))
                                    (ignore-errors (apply #'format call)))))
                        ;; Leading space so a mutated "-2 success" can't match.
                        (when (and s (string-match-p " 2 success" s))
                          (setq reported t))))
                    (expect reported :to-be t))))))
        (delete-directory temp-dir t))))

  (it "continues after one entry fails"
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (call-count 0))
            (with-temp-file org-file
              (insert "* Page One
:PROPERTIES:
:END:

Content one.

* Page Two
:PROPERTIES:
:END:

Content two.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq call-count (1+ call-count))
                               (if (= call-count 1)
                                   (signal 'error '("First page failed"))
                                 '((url . "page-two-url")))))))
                  (org-canvas-sync-pages)
                  ;; Both should have been attempted
                  (expect call-count :to-equal 2)
                  ;; Failure isolation: page one's failure must not prevent
                  ;; page two from succeeding and being finalized.
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (expect (org-entry-get (point) "CANVAS_URL") :to-be nil)
                    (re-search-forward "^\\* Page Two")
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_URL")
                            :to-equal "page-two-url"))))))
        (delete-directory temp-dir t))))

  (it "sends a payload with the expected structure (not just any call)"
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "pages.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Welcome
:PROPERTIES:
:END:

Hello world.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-pages)
                  (let* ((payload (test-org-canvas-api-call-data 'POST "pages"))
                         (page (gethash "wiki_page" payload)))
                    ;; Validate the actual body shape, so a renamed/dropped
                    ;; field is caught — not merely that a POST happened.
                    (expect (hash-table-p page) :to-be-truthy)
                    (expect (gethash "title" page) :to-equal "Welcome"))))))
        (delete-directory temp-dir t))))

  (it "errors when file not found"
    (let ((org-canvas-pages-file "/nonexistent/pages.org"))
      (with-sync-test-env
        (expect (org-canvas-sync-pages) :to-throw 'error)))))

(describe "org-canvas-define-sync macro validation"
  (it "errors when :file is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :parse #'identity
                            :build #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :file is required")))

  (it "errors when :parse is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :build #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :parse is required")))

  (it "errors when :build is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :build is required")))

  (it "errors when :push and :endpoint are missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :build #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :push or :endpoint is required")))

  (it "errors when :finalize and :endpoint are missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :build #'identity
                            :push #'identity))
            :to-throw 'error '("org-canvas-define-sync: :finalize or :endpoint is required"))))

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

(describe "org-canvas-define-sync payload hashing"
  :var (temp-dir temp-file)

  (before-each
    (setq temp-dir (make-temp-file "sync-test-" t))
    (setq temp-file (expand-file-name "test.org" temp-dir))
    (with-temp-file temp-file
      (insert "* Item One\n:PROPERTIES:\n:END:\n\nContent.\n")))

  (after-each
    (let ((buf (find-buffer-visiting temp-file)))
      (when buf (kill-buffer buf)))
    (delete-directory temp-dir t))

  (it "saves PAYLOAD_HASH property after first sync"
    (with-org-canvas-test-config
      (with-mock-api
        (let* ((parse-called 0)
               (push-called 0)
               ;; Define a test sync using the internal macro structure
               ;; We call the pattern manually instead of using the macro
               (sync-file temp-file))
          ;; Simulate one iteration of the sync pipeline
          (with-current-buffer (find-file-noselect sync-file)
            (goto-char (point-min))
            (org-back-to-heading)
            (let* ((data (list :title "Item One" :canvas-id nil :pom (point)))
                   (payload '((title . "Item One")))
                   (payload-hash (md5 (json-encode payload))))
              ;; Before sync, no hash
              (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)
              ;; Simulate saving the hash
              (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
              (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal payload-hash)))))))

  (it "PAYLOAD_HASH is cleared by org-canvas-clear-sync-properties"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2025-01-01 Wed 00:00]
:PAYLOAD_HASH: abc123
:END:
"
     (org-back-to-heading)
     (org-canvas-clear-sync-properties (point))
     (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
     (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
     (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil))))

(describe "org-canvas--sync-payload-hash"
  (it "matches plain md5 of the encoded payload when no hash-extra fn is given"
    (let ((payload '((name . "X"))))
      (expect (org-canvas--sync-payload-hash payload nil nil)
              :to-equal (md5 (json-encode payload)))))

  (it "folds the hash-extra result into the hash"
    (let ((payload '((name . "X"))))
      (expect (org-canvas--sync-payload-hash payload nil (lambda (_) "extra"))
              :not :to-equal (md5 (json-encode payload)))))

  (it "passes the parsed data to the hash-extra fn"
    (let (seen)
      (org-canvas--sync-payload-hash '((a . 1)) '(:pom 42)
                                     (lambda (d) (setq seen d) ""))
      (expect seen :to-equal '(:pom 42)))))

(describe "metamorphic relations"
  (it "re-syncing unchanged content is a no-op (push idempotence)"
    ;; sync ∘ sync = sync: the second run must skip via PAYLOAD_HASH and make
    ;; no API write.  Runs the REAL pipeline twice (the existing payload-hash
    ;; test only simulates the save).
    (let ((temp-dir (make-temp-file "idem-" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "pages.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Welcome\n:PROPERTIES:\n:END:\n\nHello.\n"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (setq test-org-canvas-api-responses
                        '(("pages" . ((url . "welcome") (page_id . 5)))))
                  (org-canvas-sync-pages)             ; creates the page
                  (expect (test-org-canvas-api-called-p 'POST "pages")
                          :to-be-truthy)
                  (setq test-org-canvas-api-calls nil) ; observe only run 2
                  (org-canvas-sync-pages)             ; identical content
                  (expect (test-org-canvas-api-called-p 'POST "pages") :to-be nil)
                  (expect (test-org-canvas-api-called-p 'PUT "pages") :to-be nil)))))
        (delete-directory temp-dir t))))

  (it "heading parse is invariant to extra spaces after the stars"
    ;; `* Welcome' and `*  Welcome' must parse to the same title.
    (let ((d1 (with-temp-org-buffer "* Welcome\n:PROPERTIES:\n:END:\n"
                (org-back-to-heading) (org-canvas--page-parse-entry)))
          (d2 (with-temp-org-buffer "*  Welcome\n:PROPERTIES:\n:END:\n"
                (org-back-to-heading) (org-canvas--page-parse-entry))))
      (expect (plist-get d1 :title) :to-equal "Welcome")
      (expect (plist-get d1 :title) :to-equal (plist-get d2 :title)))))

(describe "org-canvas-define-sync at-point generation"
  (it "generates sync-page-at-point from sync macro"
    (expect (fboundp 'org-canvas-sync-page-at-point) :to-be-truthy))

  (it "generates sync-announcement-at-point"
    (expect (fboundp 'org-canvas-sync-announcement-at-point) :to-be-truthy))

  (it "generates sync-discussion-at-point"
    (expect (fboundp 'org-canvas-sync-discussion-at-point) :to-be-truthy))

  (it "generates sync-assignment-at-point"
    (expect (fboundp 'org-canvas-sync-assignment-at-point) :to-be-truthy))

  (it "generates sync-rubric-at-point"
    (expect (fboundp 'org-canvas-sync-rubric-at-point) :to-be-truthy))

  (it "generates sync-assignment-group-at-point"
    (expect (fboundp 'org-canvas-sync-assignment-group-at-point) :to-be-truthy))

  (it "singularizes group-categories correctly"
    (expect (fboundp 'org-canvas-sync-group-category-at-point) :to-be-truthy))

  (it "singularizes calendar-events correctly"
    (expect (fboundp 'org-canvas-sync-calendar-event-at-point) :to-be-truthy))

  (it "suppresses at-point with :no-at-point"
    ;; new-quizzes has :no-at-point t, its at-point is hand-written
    (expect (fboundp 'org-canvas-sync-new-quiz-at-point) :to-be-truthy)))

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

(describe "org-canvas--singularize"
  (it "uses the irregular-plural override when present"
    ;; Guards `(or (cdr (assoc ...)) (if ...))': an `and' there would fall
    ;; through to the naive trailing-s strip and mis-singularize.
    (expect (org-canvas--singularize "quizzes") :to-equal "quiz")
    (expect (org-canvas--singularize "new-quizzes") :to-equal "new-quiz")
    (expect (org-canvas--singularize "group-categories") :to-equal "group-category"))

  (it "strips a trailing s for regular plurals"
    (expect (org-canvas--singularize "pages") :to-equal "page")
    (expect (org-canvas--singularize "assignments") :to-equal "assignment")))

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

(describe "org-canvas--sync-process-entry conflict counter"
  (it "increments conflict counter when push returns conflict"
    (with-temp-org-buffer
     "* Conflict Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0 :conflict 0))
            (ctx (list :parse-fn (lambda () (list :title "Conflict Item"
                                                   :canvas-id "123"
                                                   :pom (point-marker)))
                       :build-fn (lambda (_data) '((title . "Conflict Item")))
                       :push-fn (lambda (_data _payload &optional _ctx) 'conflict)
                       :finalize-fn (lambda (_data _response &optional _ctx) nil)
                       :feature-name "items"
                       :feature-upper "ITEMS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :conflict) :to-equal 1)
       (expect (plist-get counters :success) :to-equal 0)))))

(describe "org-canvas--sync-log-summary with conflicts"
  (it "includes conflict and pulled counts in log when present"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 5 :skip 2 :fail 1 :conflict 3 :pulled 1))
            (let ((found-conflicts nil)
                  (found-pulled nil))
              (dolist (call (spy-calls-all-args 'org-canvas--log-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Conflicts" (nth 1 call)))
                  (setq found-conflicts t))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Pulled" (nth 1 call)))
                  (setq found-pulled t)))
              (expect found-conflicts :to-be-truthy)
              (expect found-pulled :to-be-truthy)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "omits conflict and pulled counts when zero"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 5 :skip 2 :fail 1 :conflict 0 :pulled 0))
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'org-canvas--log-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Conflicts" (nth 1 call)))
                  (setq found t)))
              (expect found :to-be nil)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

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

(describe "org-canvas--sync-execute-pipeline pulled counter"
  (it "increments pulled counter when push returns pulled"
    (with-temp-org-buffer
     "* Pulled Item
:PROPERTIES:
:CANVAS_ID: 333
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0 :conflict 0 :pulled 0))
            (ctx (list :parse-fn (lambda () (list :title "Pulled Item"
                                                   :canvas-id "333"
                                                   :pom (point-marker)))
                       :build-fn (lambda (_data) '((title . "Pulled Item")))
                       :push-fn (lambda (_data _payload &optional _ctx) 'pulled)
                       :finalize-fn (lambda (_data _response &optional _ctx) nil)
                       :feature-name "items"
                       :feature-upper "ITEMS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :pulled) :to-equal 1)
       (expect (plist-get counters :success) :to-equal 0)))))

(describe "org-canvas-define-sync conflict bindings"
  (it "binds conflict-apply-all to nil per sync"
    ;; Verify the macro delegates to sync-run-pipeline (which handles bindings)
    (let ((expanded (macroexpand
                     '(org-canvas-define-sync test-feature
                        :file "/tmp/test.org"
                        :parse #'identity
                        :build #'identity
                        :push #'identity
                        :finalize #'identity
                        :pull-item-fn #'ignore))))
      ;; The expanded form should call sync-run-pipeline
      (expect (format "%S" expanded)
              :to-match "org-canvas--sync-run-pipeline")
      ;; Pull-item-fn should be passed through
      (expect (format "%S" expanded)
              :to-match "ignore"))))

(describe "org-canvas--sync-collect-entries duplicate warning"
  (it "warns about duplicate CANVAS_IDs"
    (let* ((temp-dir (make-temp-file "dup-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item 1
:PROPERTIES:
:CANVAS_ID: DUP-1
:END:
* Item 2
:PROPERTIES:
:CANVAS_ID: DUP-1
:END:
"))
            (spy-on 'org-canvas--log-warning)
            (with-org-canvas-test-config
              (org-canvas--sync-collect-entries test-file "LEVEL=1" "test")
              (let ((dup-warned nil))
                (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
                  (when (and (>= (length call) 2)
                             (stringp (nth 1 call))
                             (string-match-p "Duplicate" (nth 1 call)))
                    (setq dup-warned t)))
                (expect dup-warned :to-be-truthy))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "does not warn when all CANVAS_IDs are unique"
    ;; Guards the `(> count 1)' boundary: a >= would warn for every id
    ;; (each appears once), producing false duplicate warnings.
    (let* ((temp-dir (make-temp-file "dup-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item One
:PROPERTIES:
:CANVAS_ID: UNIQ-1
:END:
* Item Two
:PROPERTIES:
:CANVAS_ID: UNIQ-2
:END:
"))
            (spy-on 'org-canvas--log-warning)
            (with-org-canvas-test-config
              (org-canvas--sync-collect-entries test-file "LEVEL=1" "test")
              (let ((dup-warned nil))
                (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
                  (when (and (>= (length call) 2)
                             (stringp (nth 1 call))
                             (string-match-p "Duplicate\\] CANVAS_ID" (nth 1 call)))
                    (setq dup-warned t)))
                (expect dup-warned :to-be nil))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--sync-execute-pipeline dry-run"
  (it "skips API call in dry-run mode"
    (let* ((temp-dir (make-temp-file "dry-test" t))
           (test-file (expand-file-name "test.org" temp-dir))
           (api-called nil))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item
:PROPERTIES:
:END:
"))
            (with-org-canvas-test-config
              (with-sync-test-env
                (let ((org-canvas--dry-run t))
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (&rest _args)
                               (setq api-called t)
                               '((id . 1)))))
                    (let* ((targets (org-canvas--sync-collect-entries
                                    test-file "LEVEL=1" "test"))
                           (counters (list :success 0 :skip 0 :fail 0 :conflict 0 :pulled 0))
                           (synced-ids (list nil))
                           (ctx (list :parse-fn (lambda ()
                                                  (list :title "Item" :canvas-id nil
                                                        :pom (point-marker)))
                                      :build-fn (lambda (_data) '((title . "Item")))
                                      :push-fn (lambda (_data _payload &optional _ctx)
                                                 (setq api-called t)
                                                 '((id . 1)))
                                      :finalize-fn (lambda (_data _response &optional _ctx) nil)
                                      :feature-name "test" :feature-upper "TEST"
                                      :total-count 1 :counters counters
                                      :synced-ids synced-ids
                                      :title-key :title)))
                      (dolist (marker (plist-get targets :targets))
                        (org-canvas--sync-process-entry marker ctx))
                      (expect api-called :to-be nil)))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--sync-warn-orphans"
  (it "warns about IDs not in synced set"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--sync-warn-orphans '("111" "222" "333") '("111" "333") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
        (when (and (>= (length call) 3)
                   (stringp (nth 1 call))
                   (string-match-p "Orphan" (nth 1 call)))
          (setq orphan-warned t)))
      (expect orphan-warned :to-be-truthy)))

  (it "does not warn when all IDs synced"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--sync-warn-orphans '("111" "222") '("111" "222") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
        (when (and (>= (length call) 2)
                   (stringp (nth 1 call))
                   (string-match-p "Orphan" (nth 1 call)))
          (setq orphan-warned t)))
      (expect orphan-warned :to-be nil))))

(describe "org-canvas--push-at-point-runtime"
  (it "hands the pull-item-fn to the push through the run context"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test Page
:PROPERTIES:
:CANVAS_ID: 99
:END:
"
         (org-back-to-heading)
         (let ((captured-pull-fn nil))
           (cl-letf (((symbol-function 'display-buffer) #'ignore))
		    (org-canvas--push-at-point-runtime (list
							:feature "test"
							:parse (lambda () (list :title "Test" :canvas-id "99" :pom (point)))
							:build (lambda (_data) '((title . "Test")))
							:push (lambda (_data _payload &optional ctx)
								(setq captured-pull-fn (plist-get ctx :pull-item-fn))
								'((id . 99)))
							:finalize (lambda (_data _response &optional _ctx) nil)
							:title-key :title
							:pull-item-fn #'my-pull-fn)))
           (expect captured-pull-fn :to-equal #'my-pull-fn)))))))

(describe "org-canvas--make-push-fn-form"
  (it "generates a lambda with endpoint only"
    (let ((form (org-canvas--make-push-fn-form "pages" nil nil nil)))
      (expect (car form) :to-equal 'lambda)
      (expect (format "%S" form) :to-match ":endpoint \"pages\"")))

  (it "includes id-key when provided"
    (let ((form (org-canvas--make-push-fn-form "pages" :canvas-url nil nil)))
      (expect (format "%S" form) :to-match ":id-key :canvas-url")))

  (it "includes title-key when provided"
    (let ((form (org-canvas--make-push-fn-form "pages" nil :name nil)))
      (expect (format "%S" form) :to-match ":title-key :name")))

  (it "includes find-fn when provided"
    (let ((form (org-canvas--make-push-fn-form "pages" nil nil '#'my-find)))
      (expect (format "%S" form) :to-match "my-find")))

  (it "omits optional keys when nil"
    (let ((form-str (format "%S" (org-canvas--make-push-fn-form "items" nil nil nil))))
      (expect form-str :not :to-match ":id-key")
      (expect form-str :not :to-match ":title-key")
      (expect form-str :not :to-match ":find-fn"))))

(describe "org-canvas--make-finalize-fn-form"
  (it "generates a lambda with no optional keys"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil nil)))
      (expect (car form) :to-equal 'lambda)
      (let ((form-str (format "%S" form)))
        (expect form-str :not :to-match ":id-field")
        (expect form-str :not :to-match ":id-property")
        (expect form-str :not :to-match ":post-fn"))))

  (it "includes id-field when provided"
    (let ((form (org-canvas--make-finalize-fn-form 'url nil nil nil)))
      (expect (format "%S" form) :to-match ":id-field")))

  (it "includes id-property when provided"
    (let ((form (org-canvas--make-finalize-fn-form nil "CANVAS_URL" nil nil)))
      (expect (format "%S" form) :to-match ":id-property \"CANVAS_URL\"")))

  (it "includes post-fn when provided"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil '#'my-post)))
      (expect (format "%S" form) :to-match "my-post")))

  (it "carries the endpoint alongside a post-fn, for the restamp (issue #124)"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil '#'my-post
                                                   "assignments")))
      (expect (format "%S" form) :to-match ":endpoint \"assignments\"")))

  (it "leaves the endpoint out when there is no post-fn to restamp after"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil nil "pages")))
      (expect (format "%S" form) :not :to-match ":endpoint"))))

(describe "org-canvas--sync-run-pipeline"
  (it "runs the full pipeline for entries in a file"
    (let ((temp-dir (make-temp-file "pipeline-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "test.org" temp-dir))
                 (parse-count 0))
            (with-temp-file org-file
              (insert "* Entry One\n:PROPERTIES:\n:END:\n\n* Entry Two\n:PROPERTIES:\n:END:\n"))
            (let ((org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             (setq parse-count (1+ parse-count))
                             '((id . 1)))))
			 (org-canvas--sync-run-pipeline (list
							 :feature "test" :file org-file :query "LEVEL=1"
							 :parse #'org-canvas--announcement-parse-entry
							 :build #'org-canvas--announcement-build-payload
							 :push (lambda (data payload &optional ctx)
								 (org-canvas--push-to-api data payload :endpoint "test" :ctx ctx))
							 :finalize (lambda (data response &optional ctx)
								     (org-canvas--finalize-item data response :ctx ctx))))
                  (expect parse-count :to-be-truthy)))))
        (delete-directory temp-dir t))))

  (it "starts every run with a context holding no apply-all answer"
    (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
              ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
              ((symbol-function 'org-canvas--sync-collect-entries)
               (lambda (&rest _) (list :targets nil :all-ids-before nil)))
              ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
              ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
	     (let ((ctx (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
							     :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore))))
        (expect (plist-get ctx :conflict-apply-all) :to-be nil)
        (expect (plist-get ctx :duplicate-apply-all) :to-be nil)
        (expect (plist-get ctx :feature-name) :to-equal "test"))))

  (it "carries the pull-item-fn argument in the returned context"
    (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
              ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
              ((symbol-function 'org-canvas--sync-collect-entries)
               (lambda (&rest _) (list :targets nil :all-ids-before nil)))
              ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
              ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
	     (let ((ctx (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
							     :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore
							     :pull-item-fn #'my-pull-fn))))
        (expect (plist-get ctx :pull-item-fn) :to-equal #'my-pull-fn)))))

(describe "org-canvas--push-at-point-runtime"
  (it "skips sync when payload hash matches"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Test Page
:PROPERTIES:
:CANVAS_ID: 100
:END:

Body.
"
       (org-back-to-heading)
       (let* ((payload '((title . "Test Page") (body . "Body.")))
              (payload-hash (md5 (json-encode payload)))
              (api-called nil))
         ;; Set the stored hash to match
         (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
         (save-buffer)
         (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                   ((symbol-function 'display-buffer) #'ignore))
		  (org-canvas--push-at-point-runtime (list
						      :feature "page"
						      :parse (lambda () (list :title "Test Page" :canvas-id "100" :pom (point)))
						      :build (lambda (_data) payload)
						      :push (lambda (_data _payload &optional _ctx) (setq api-called t) '((id . 100)))
						      :finalize (lambda (_data _response &optional _ctx) nil)
						      :title-key :title)))
         ;; API should NOT have been called (skipped)
         (expect api-called :to-be nil))))))

(describe "org-canvas--push-at-point-runtime"
  (it "uses canvas-url for skip detection when canvas-id absent"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Test Page
:PROPERTIES:
:CANVAS_URL: my-page-slug
:END:

Body.
"
       (org-back-to-heading)
       (let* ((payload '((wiki_page (title . "Test Page") (body . "Body."))))
              (payload-hash (md5 (json-encode payload)))
              (api-called nil))
         ;; Set stored hash to match — should skip
         (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
         (save-buffer)
         (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                   ((symbol-function 'display-buffer) #'ignore))
		  (org-canvas--push-at-point-runtime (list
						      :feature "page"
						      :parse (lambda () (list :title "Test Page" :canvas-url "my-page-slug" :pom (point)))
						      :build (lambda (_data) payload)
						      :push (lambda (_data _payload &optional _ctx) (setq api-called t) '((url . "my-page-slug")))
						      :finalize (lambda (_data _response &optional _ctx) nil)
						      :title-key :title)))
         ;; Should be skipped because hash matches AND canvas-url is truthy
         (expect api-called :to-be nil))))))

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

(describe "org-canvas--parse-gen-transform-form"
  (it "generates enum form with default value"
    (let ((form (org-canvas--parse-gen-transform-form
                 "STATUS" :status 'enum "active"
                 '("active" "inactive"))))
      ;; Should produce a validate-property call with the default
      (expect form :to-contain 'org-canvas--validate-property)
      (expect (nth 4 form) :to-equal "active"))))

(describe "org-canvas-define-parse :after-read hook"
  (it "generates read-fn that calls after-read"
    (eval
     '(org-canvas-define-parse test--after-read-cov
        :after-read (lambda (raw _pom)
                      (plist-put raw :extra "injected")
                      raw)
        :properties
        (("TITLE_PROP" :title-prop :type string)))
     t)
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:TITLE_PROP: hello
:END:
"
     (org-back-to-heading)
     (let ((raw (org-canvas--test--after-read-cov-read-props (point))))
       (expect (plist-get raw :extra) :to-equal "injected")))))

(describe "org-canvas--sync-log-summary global counters"
  (it "accumulates counts into org-canvas--sync-global-counters"
    (let* ((temp-file (make-temp-file "sum-test" nil ".org"))
           (org-canvas--sync-global-counters (list :success 0 :skip 0 :fail 0)))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Test\n"))
            (org-canvas--sync-log-summary "test" temp-file
                                          (list :success 3 :skip 1 :fail 2))
            (org-canvas--sync-log-summary "test2" temp-file
                                          (list :success 5 :skip 0 :fail 1))
            (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 8)
            (expect (plist-get org-canvas--sync-global-counters :skip) :to-equal 1)
            (expect (plist-get org-canvas--sync-global-counters :fail) :to-equal 3))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "does not accumulate when global counters are nil"
    (let* ((temp-file (make-temp-file "sum-test" nil ".org"))
           (org-canvas--sync-global-counters nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Test\n"))
            ;; Should not error when counters are nil
            (expect (org-canvas--sync-log-summary "test" temp-file
                                                  (list :success 1 :skip 0 :fail 0))
                    :not :to-throw))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--sync-collect-entries"
  (it "warns in minibuffer about duplicate CANVAS_IDs"
    (let* ((temp-file (make-temp-file "dup-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n\n")
              (insert "* Item B\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (spy-on 'message)
            (spy-on 'org-canvas--log-warning)
            (org-canvas--sync-collect-entries temp-file "LEVEL=1" "test")
            (expect 'message :to-have-been-called)
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'message))
                (when (and (stringp (car call))
                           (string-match-p "CANVAS_ID 100 appears 2 times" (apply #'format call)))
                  (setq found t)))
              (expect found :to-be-truthy)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--sync-process-entry error includes heading title"
  (it "includes heading title when parse-fn errors"
    (with-temp-org-buffer
     "* My Assignment
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :parse-fn (lambda () (error "Parse failed"))
                       :build-fn #'ignore
                       :push-fn #'ignore
                       :finalize-fn #'ignore
                       :feature-name "assignments"
                       :feature-upper "ASSIGNMENTS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (spy-on 'org-canvas--log-error)
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :fail) :to-equal 1)
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-error))
           (when (>= (length call) 3)
             (let ((formatted (apply #'format (cdr call))))
               (when (and (string-match-p "ASSIGNMENTS" formatted)
                          (string-match-p "My Assignment" formatted))
                 (setq found t)))))
         (expect found :to-be-truthy)))))

  (it "includes heading title when build-fn errors"
    (with-temp-org-buffer
     "* Quiz One
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :parse-fn (lambda () (list :title "Quiz One"))
                       :build-fn (lambda (_) (error "Build failed"))
                       :push-fn #'ignore
                       :finalize-fn #'ignore
                       :feature-name "quizzes"
                       :feature-upper "QUIZZES"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (spy-on 'org-canvas--log-error)
       (org-canvas--sync-process-entry marker ctx)
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-error))
           (when (and (>= (length call) 3)
                      (stringp (nth 1 call)))
             (let ((formatted (apply #'format (cdr call))))
               (when (string-match-p "Quiz One" formatted)
                 (setq found t)))))
         (expect found :to-be-truthy))))))

(describe "org-canvas--push-at-point-runtime stage logging"
  (it "logs stage markers during sync"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:END:

Content here.
"
     (org-back-to-heading)
     (spy-on 'org-canvas--log-info)
     (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil))
               ((symbol-function 'save-buffer) (lambda () nil)))
	      (org-canvas--push-at-point-runtime (list
						  :feature "pages"
						  :parse (lambda () (list :title "Test Page" :canvas-id nil :pom (point-marker)))
						  :build (lambda (_) '((title . "Test Page")))
						  :push (lambda (_data _payload &optional _ctx) '((url . "test-page")))
						  :finalize (lambda (_data _response &optional _ctx) nil)
						  :title-key :title)))
     (let ((found-sync-at-point nil)
           (found-stage-2 nil)
           (found-stage-3 nil)
           (found-stage-4 nil))
       (dolist (call (spy-calls-all-args 'org-canvas--log-info))
         (when (>= (length call) 2)
           (let ((fmt (nth 1 call)))
             (when (stringp fmt)
               (when (string-match-p "SYNC-AT-POINT" fmt) (setq found-sync-at-point t))
               (when (string-match-p "Stage 2" fmt) (setq found-stage-2 t))
               (when (string-match-p "Stage 3" fmt) (setq found-stage-3 t))
               (when (string-match-p "Stage 4" fmt) (setq found-stage-4 t))))))
       (expect found-sync-at-point :to-be-truthy)
       (expect found-stage-2 :to-be-truthy)
       (expect found-stage-3 :to-be-truthy)
       (expect found-stage-4 :to-be-truthy))))

  (it "logs skip without push/finalize stages when unchanged"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:CANVAS_URL: existing-page
:PAYLOAD_HASH: placeholder
:END:

Content here.
"
     (org-back-to-heading)
     (let* ((data (list :title "Test Page" :canvas-url "existing-page" :pom (point-marker)))
            (payload '((title . "Test Page")))
            (hash (md5 (json-encode payload))))
       ;; Set the stored hash to match
       (org-entry-put (point) "PAYLOAD_HASH" hash)
       (save-buffer)
       (spy-on 'org-canvas--log-info)
       (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
		(org-canvas--push-at-point-runtime (list
						    :feature "pages"
						    :parse (lambda () data)
						    :build (lambda (_) payload)
						    :push (lambda (_data _payload &optional _ctx) (error "Should not be called"))
						    :finalize (lambda (_data _response &optional _ctx) (error "Should not be called"))
						    :title-key :title)))
       (let ((found-skip nil)
             (found-stage-3 nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-info))
           (when (>= (length call) 2)
             (let ((fmt (nth 1 call)))
               (when (stringp fmt)
                 (when (string-match-p "Skip" fmt) (setq found-skip t))
                 (when (string-match-p "Stage 3" fmt) (setq found-stage-3 t))))))
         (expect found-skip :to-be-truthy)
         (expect found-stage-3 :to-be nil))))))

(describe "org-canvas--sync-warn-duplicate-titles"
  (it "warns when duplicate titles exist"
    (with-temp-org-buffer
     "* Same Title
:PROPERTIES:
:END:

* Same Title
:PROPERTIES:
:END:

* Different Title
:PROPERTIES:
:END:
"
     (let ((markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))
       (spy-on 'org-canvas--log-warning)
       (org-canvas--sync-warn-duplicate-titles markers (buffer-file-name))
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
           (when (and (>= (length call) 3)
                      (stringp (nth 1 call))
                      (string-match-p "Duplicate Title" (nth 1 call)))
             (let ((formatted (apply #'format (cdr call))))
               (when (string-match-p "Same Title" formatted)
                 (setq found t)))))
         (expect found :to-be-truthy)))))

  (it "does not warn when all titles are distinct"
    (with-temp-org-buffer
     "* Title A
:PROPERTIES:
:END:

* Title B
:PROPERTIES:
:END:
"
     (let ((markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))
       (spy-on 'org-canvas--log-warning)
       (org-canvas--sync-warn-duplicate-titles markers (buffer-file-name))
       (expect 'org-canvas--log-warning :not :to-have-been-called)))))

(describe "org-canvas--sync-execute-pipeline dry-run counter"
  (it "increments dry-run counter instead of success"
    (with-temp-org-buffer
     "* Dry Run Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((org-canvas--dry-run t)
            (counters (list :success 0 :skip 0 :fail 0 :dry-run 0))
            (ctx (list :push-fn #'ignore
                       :feature-name "pages"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil)))
            (data (list :title "Dry Run Item" :canvas-id nil))
            (payload '((title . "Dry Run Item"))))
       (org-canvas--sync-execute-pipeline data payload ctx)
       (expect (plist-get counters :dry-run) :to-equal 1)
       (expect (plist-get counters :success) :to-equal 0)))))

(describe "org-canvas--sync-log-summary dry-run format"
  (it "shows dry-run format when dry-run count > 0"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (spy-on 'message)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 0 :skip 2 :fail 0 :dry-run 3))
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'org-canvas--log-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Would sync" (nth 1 call)))
                  (setq found t)))
              (expect found :to-be-truthy))
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'message))
                (when (and (stringp (car call))
                           (string-match-p "dry-run" (car call)))
                  (setq found t)))
              (expect found :to-be-truthy)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "accumulates dry-run count into global counters"
    (let ((temp-file (make-temp-file "summary-test" nil ".org"))
          (org-canvas--sync-global-counters (list :success 0 :skip 0 :fail 0 :dry-run 0)))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (spy-on 'message)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 0 :skip 1 :fail 0 :dry-run 5))
            (expect (plist-get org-canvas--sync-global-counters :dry-run)
                    :to-equal 5))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--sync-warn-stale-headings"
  (it "warns and prompts when heading has LAST_SYNCED but no CANVAS_ID"
    (with-temp-org-buffer
     "* Stale Item
:PROPERTIES:
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (spy-on 'y-or-n-p :and-return-value t)
     (let ((noninteractive nil)
           (markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       (expect 'message :to-have-been-called)
       (expect 'y-or-n-p :to-have-been-called))))

  (it "aborts when user declines stale heading prompt"
    (with-temp-org-buffer
     "* Stale Item
:PROPERTIES:
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'y-or-n-p :and-return-value nil)
     (let ((noninteractive nil)
           (markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (expect (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
               :to-throw 'user-error))))

  (it "does not warn for normal heading with CANVAS_ID"
    (with-temp-org-buffer
     "* Normal Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (let ((markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       (expect 'message :not :to-have-been-called))))

  (it "does not warn for new heading without LAST_SYNCED"
    (with-temp-org-buffer
     "* New Item
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (let ((markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       (expect 'message :not :to-have-been-called))))

  (it "lists all stale headings in single warning when multiple exist"
    (with-temp-org-buffer
     "* Stale A
:PROPERTIES:
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
* Stale B
:PROPERTIES:
:LAST_SYNCED: [2025-02-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (spy-on 'y-or-n-p :and-return-value t)
     (let ((noninteractive nil)
           (markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       ;; One consolidated message + one prompt
       (expect 'message :to-have-been-called-times 1)
       (expect 'y-or-n-p :to-have-been-called-times 1)
       ;; Both titles logged individually
       (expect 'org-canvas--log-warning :to-have-been-called-times 2)))))

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

(describe "org-canvas--sync-process-entry deferred counter"
  (it "counts deferred drop-rule rejections separately from failures"
    (with-temp-org-buffer
     "* Group A
:PROPERTIES:
:CANVAS_ID: 5
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :parse-fn
                       (lambda ()
                         (error "Drop rules cannot be higher than the number of assignments (HTTP 400)"))
                       :build-fn #'ignore
                       :push-fn #'ignore
                       :finalize-fn #'ignore
                       :feature-name "assignment-groups"
                       :feature-upper "ASSIGNMENT-GROUPS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (spy-on 'org-canvas--log-error)
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :deferred) :to-equal 1)
       (expect (plist-get counters :fail) :to-equal 0)
       (expect 'org-canvas--log-error :not :to-have-been-called)))))

(describe "org-canvas--sync-log-summary deferred count"
  (it "logs a Deferred line when counters include :deferred"
    (let ((temp-file (make-temp-file "sync-summary-" nil ".org"))
          (logged nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged))))
            (org-canvas--sync-log-summary "groups" temp-file
                                          (list :success 1 :skip 0 :fail 0
                                                :deferred 2))
            (expect (cl-find-if (lambda (l) (string-match-p "Deferred: 2" l))
                                logged)
                    :to-be-truthy))
        (delete-file temp-file))))

  (it "omits the Deferred line when nothing was deferred"
    (let ((temp-file (make-temp-file "sync-summary-" nil ".org"))
          (logged nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged))))
            (org-canvas--sync-log-summary "groups" temp-file
                                          (list :success 1 :skip 0 :fail 0))
            (expect (cl-find-if (lambda (l) (string-match-p "Deferred:" l))
                                logged)
                    :to-be nil))
        (delete-file temp-file)))))

(describe "org-canvas--sync-record-feature-stats"
  (it "is a no-op when no global sync is active"
    (let ((org-canvas--sync-global-counters nil)
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats "Pages" '(:success 3))
      (expect org-canvas--sync-global-feature-stats :to-be nil)))

  (it "accumulates aggregate counters including deferred"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats
       "Groups" '(:success 2 :skip 1 :fail 1 :deferred 1))
      (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 2)
      (expect (plist-get org-canvas--sync-global-counters :skip) :to-equal 1)
      (expect (plist-get org-canvas--sync-global-counters :fail) :to-equal 1)
      (expect (plist-get org-canvas--sync-global-counters :deferred) :to-equal 1)))

  (it "merges repeated records for the same label"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats
       "Module Items" '(:success 3 :skip 1 :skipped-titles ("A (no linked content)")))
      (org-canvas--sync-record-feature-stats
       "Module Items" '(:success 2 :fail 1 :failed-titles ("B")))
      (expect (length org-canvas--sync-global-feature-stats) :to-equal 1)
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :success) :to-equal 5)
        (expect (plist-get entry :skip) :to-equal 1)
        (expect (plist-get entry :fail) :to-equal 1)
        (expect (plist-get entry :skipped-titles)
                :to-equal '("A (no linked content)"))
        (expect (plist-get entry :failed-titles) :to-equal '("B")))))

  ;; Issue #66: a counter the run populated but the entry dropped never
  ;; reaches the table, and the table is what people read.
  (it "carries the dry-run, conflict and pulled counters into the entry"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats
       "Assignments" '(:dry-run 31 :skip 30 :conflict 2 :pulled 1))
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :dry-run) :to-equal 31)
        (expect (plist-get entry :conflict) :to-equal 2)
        (expect (plist-get entry :pulled) :to-equal 1))))

  (it "merges those counters across repeated records for one label"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats "Groups" '(:dry-run 2 :conflict 1))
      (org-canvas--sync-record-feature-stats "Groups" '(:dry-run 3 :pulled 4))
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :dry-run) :to-equal 5)
        (expect (plist-get entry :conflict) :to-equal 1)
        (expect (plist-get entry :pulled) :to-equal 4))))

  (it "accumulates conflicts and pulls into the aggregate counters"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats "Pages" '(:conflict 3 :pulled 2))
      (expect (plist-get org-canvas--sync-global-counters :conflict) :to-equal 3)
      (expect (plist-get org-canvas--sync-global-counters :pulled) :to-equal 2))))

(describe "org-canvas--sync-summary-columns"
  (it "replaces Success with Would sync when the run was a dry run"
    ;; The reported symptom: a preview printing 0 success reads as clean.
    (expect (org-canvas--sync-summary-columns '((:dry-run 31 :skip 30)))
            :to-equal '(("Would sync" . :dry-run) ("Skipped" . :skip)
                        ("Failed" . :fail) ("Deferred" . :deferred))))

  (it "keeps the narrow table for an ordinary clean sync"
    (expect (org-canvas--sync-summary-columns '((:success 8 :skip 1)))
            :to-equal '(("Success" . :success) ("Skipped" . :skip)
                        ("Failed" . :fail) ("Deferred" . :deferred))))

  (it "adds a Conflicts column only when the run hit conflicts"
    (expect (mapcar #'car (org-canvas--sync-summary-columns
                           '((:success 8 :conflict 5))))
            :to-equal '("Success" "Skipped" "Failed" "Deferred" "Conflicts")))

  (it "adds a Pulled column only when something was pulled"
    (expect (mapcar #'car (org-canvas--sync-summary-columns
                           '((:success 8 :pulled 2))))
            :to-equal '("Success" "Skipped" "Failed" "Deferred" "Pulled")))

  (it "shows both when both happened, across different features"
    (expect (mapcar #'car (org-canvas--sync-summary-columns
                           '((:success 8 :conflict 1) (:success 2 :pulled 1))))
            :to-equal '("Success" "Skipped" "Failed" "Deferred"
                        "Conflicts" "Pulled"))))

(describe "org-canvas--sync-stat-total"
  (it "sums a counter across features, treating an absent one as zero"
    (expect (org-canvas--sync-stat-total
             '((:dry-run 31) (:skip 2) (:dry-run 16)) :dry-run)
            :to-equal 47)))

(describe "org-canvas--sync-log-summary feature stats recording"
  (it "records the feature's counters under its capitalized label"
    (let ((temp-file (make-temp-file "sync-summary-" nil ".org"))
          (org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (unwind-protect
          (progn
            (org-canvas--sync-log-summary "pages" temp-file
                                          (list :success 2 :skip 1 :fail 1
                                                :failed-titles '("Course Home")))
            (let ((entry (car org-canvas--sync-global-feature-stats)))
              (expect (plist-get entry :label) :to-equal "Pages")
              (expect (plist-get entry :success) :to-equal 2)
              (expect (plist-get entry :failed-titles)
                      :to-equal '("Course Home")))
            (expect (plist-get org-canvas--sync-global-counters :success)
                    :to-equal 2))
        (delete-file temp-file)))))

(describe "org-canvas--sync-reclassify-skip-as-success"
  (it "moves one skip to success in aggregates and the labeled entry"
    (let ((org-canvas--sync-global-counters
           (list :success 5 :skip 2 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats
           (list (list :label "Module Items" :success 3 :skip 2 :fail 0
                       :deferred 0 :failed-titles nil
                       :skipped-titles '("A (no linked content)" "B (no linked content)")))))
      (org-canvas--sync-reclassify-skip-as-success "Module Items" "A")
      (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 6)
      (expect (plist-get org-canvas--sync-global-counters :skip) :to-equal 1)
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :success) :to-equal 4)
        (expect (plist-get entry :skip) :to-equal 1)
        (expect (plist-get entry :skipped-titles)
                :to-equal '("B (no linked content)")))))

  (it "is a no-op when no global sync is active"
    (let ((org-canvas--sync-global-counters nil)
          (org-canvas--sync-global-feature-stats
           (list (list :label "Module Items" :success 0 :skip 1 :fail 0
                       :deferred 0 :failed-titles nil :skipped-titles '("A")))))
      (org-canvas--sync-reclassify-skip-as-success "Module Items" "A")
      (expect (plist-get (car org-canvas--sync-global-feature-stats) :skip)
              :to-equal 1)))

  (it "tolerates a label with no recorded entry"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 1 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-reclassify-skip-as-success "Module Items" "A")
      (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 1))))

(describe "org-canvas--sync-log-global-summary"
  (it "renders the per-type table and named failed/skipped items"
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Module Items" :success 15 :skip 1 :fail 0
                       :deferred 0 :failed-titles nil
                       :skipped-titles '("Course Home (no linked content synced)"))
                 (list :label "Pages" :success 8 :skip 0 :fail 1 :deferred 0
                       :failed-titles '("Course Home") :skipped-titles nil)))
          (logged-info nil)
          (logged-warn nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged-info)))
                ((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged-warn))))
        (org-canvas--sync-log-global-summary))
      ;; Stats were pushed in reverse sync order; table renders Pages first
      (let ((lines (nreverse logged-info)))
        (expect (nth 0 lines) :to-match "Type.*Success.*Skipped.*Failed.*Deferred")
        (expect (nth 1 lines) :to-match "Pages +8 +0 +1 +0")
        (expect (nth 2 lines) :to-match "Module Items +15 +1 +0 +0"))
      (expect (cl-find-if (lambda (l) (string-match-p "Failed Pages: 'Course Home'" l))
                          logged-warn)
              :to-be-truthy)
      (expect (cl-find-if (lambda (l)
                            (string-match-p "Skipped Module Items: 'Course Home (no linked content synced)'" l))
                          logged-warn)
              :to-be-truthy)))

  (it "logs nothing when no stats were recorded"
    (let ((org-canvas--sync-global-feature-stats nil)
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged))))
        (org-canvas--sync-log-global-summary))
      (expect logged :to-be nil)))

  ;; Issue #66, the whole point: the table has to describe the run that
  ;; happened.  The reported case was 31 assignments and 16 modules pending
  ;; a push, printed as 0 success.
  (it "reports what a dry run would do, under a header that says so"
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Modules" :success 0 :skip 0 :fail 0 :deferred 0
                       :dry-run 16 :conflict 0 :pulled 0)
                 (list :label "Assignments" :success 0 :skip 30 :fail 0
                       :deferred 0 :dry-run 31 :conflict 0 :pulled 0)))
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (let ((lines (nreverse logged)))
        (expect (nth 0 lines) :to-match "DRY RUN")
        (expect (nth 1 lines) :to-match "Type.*Would sync.*Skipped")
        (expect (nth 1 lines) :not :to-match "Success")
        (expect (nth 2 lines) :to-match "Assignments +31 +30 +0 +0")
        (expect (nth 3 lines) :to-match "Modules +16 +0 +0 +0"))))

  (it "shows conflicts and pulls a real sync hit"
    ;; The worse half of #66: a run with five conflicts printed 0 failed
    ;; and nothing else, so the table said the course was clean.
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Pages" :success 8 :skip 0 :fail 0 :deferred 0
                       :dry-run 0 :conflict 5 :pulled 2)))
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (let ((lines (nreverse logged)))
        (expect (nth 0 lines) :to-match "Conflicts.*Pulled")
        (expect (nth 1 lines) :to-match "Pages +8 +0 +0 +0 +5 +2"))))

  (it "says nothing about a dry run when the sync was real"
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Pages" :success 8 :skip 0 :fail 0 :deferred 0
                       :dry-run 0 :conflict 0 :pulled 0)))
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (expect (cl-find-if (lambda (l) (string-match-p "DRY RUN" l)) logged)
              :to-be nil))))

(describe "org-canvas--sync-run-pipeline :after-sync"
  (it "runs the hook after entries, before the summary"
    ;; Ordering matters: the hook reports on state the sync just produced,
    ;; and its output should read above the SYNC COMPLETE banner.
    (let ((order nil))
      (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
                ((symbol-function 'org-canvas--sync-collect-entries)
                 (lambda (&rest _) (list :targets nil :all-ids-before nil)))
                ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                ((symbol-function 'org-canvas--sync-log-summary)
                 (lambda (&rest _) (push 'summary order))))
               (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
						    :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore
						    :after-sync (lambda (ctx) (push (if (plist-get ctx :feature-name) 'after-sync 'no-ctx) order))))
        (expect (nreverse order) :to-equal '(after-sync summary)))))

  (it "is optional"
    (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
              ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
              ((symbol-function 'org-canvas--sync-collect-entries)
               (lambda (&rest _) (list :targets nil :all-ids-before nil)))
              ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
              ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
	     (expect (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
							  :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore))
              :not :to-throw)))

  (it "is wired through org-canvas-define-sync for assignment groups"
    ;; Guards the macro plumbing, not just the runtime argument: a
    ;; :after-sync that never reaches the pipeline would fail silently.
    (let ((hook-ran nil))
      (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
                ((symbol-function 'org-canvas--sync-collect-entries)
                 (lambda (&rest _) (list :targets nil :all-ids-before nil)))
                ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                ((symbol-function 'org-canvas--sync-log-summary) #'ignore)
                ((symbol-function 'org-canvas--assignment-group-reconcile-unmanaged)
                 (lambda (&rest _) (setq hook-ran t))))
        (org-canvas-sync-assignment-groups)
        (expect hook-ran :to-be t)))))

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

(describe "org-canvas--dry-run-decision-note (issue #84)"
  (it "names the standing answer when the strategy is set"
    (let ((org-canvas-conflict-strategy 'push))
      (expect (org-canvas--dry-run-decision-note 'org-canvas-conflict-strategy)
              :to-equal "; org-canvas-conflict-strategy is push")))

  (it "says a batch sync would skip"
    (let ((org-canvas-conflict-strategy nil) (noninteractive t))
      (expect (org-canvas--dry-run-decision-note 'org-canvas-conflict-strategy)
              :to-equal "; a batch sync would skip it")))

  (it "says a real sync would ask"
    (let ((org-canvas-duplicate-title-strategy nil) (noninteractive nil))
      (expect (org-canvas--dry-run-decision-note 'org-canvas-duplicate-title-strategy)
              :to-equal "; a real sync would ask"))))

(describe "org-canvas--sync-dry-run-entry (issue #84)"
  (defun test-dry-run-84--run (heading data remote-updated titles)
    "Run the dry-run branch for HEADING with DATA against a snapshot.
REMOTE-UPDATED and TITLES are the two halves of the snapshot.
Returns (COUNTERS . LOG-LINES)."
    (let ((logged nil) (counters nil))
      (with-temp-org-buffer heading
        (org-back-to-heading)
        (let* ((org-canvas--dry-run t)
               (ctx (list :push-fn (lambda (&rest _) (error "Must not push"))
                          :feature-name "assignments" :total-count 1
                          :counters (list :success 0 :skip 0 :fail 0
                                          :dry-run 0 :dry-run-conflict 0)
                          :synced-ids (list nil)
                          :baseline nil
                          :remote-updated remote-updated :remote-titles titles)))
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged)))
                    ((symbol-function 'message) #'ignore))
            (org-canvas--sync-execute-pipeline data '((name . "x")) ctx))
          (setq counters (plist-get ctx :counters))))
      (cons counters (nreverse logged))))

  (defconst test-dry-run-84--stamped
    "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
")

  (it "counts a remotely newer entry as a conflict, not a push"
    (let ((map (make-hash-table :test 'equal))
          (org-canvas-conflict-strategy nil) (noninteractive nil))
      (puthash "61" "2026-08-25T00:00:00Z" map)
      (let ((run (test-dry-run-84--run test-dry-run-84--stamped
                                        (list :title "Lab 1" :canvas-id "61") map nil)))
        (expect (plist-get (car run) :dry-run-conflict) :to-equal 1)
        (expect (plist-get (car run) :dry-run) :to-equal 0)
        (expect (car (cdr run))
                :to-equal "[DRY-RUN] Would CONFLICT 'Lab 1' (remote updated at 2026-08-25T00:00:00Z; a real sync would ask)"))))

  (it "names the standing conflict strategy"
    (let ((map (make-hash-table :test 'equal))
          (org-canvas-conflict-strategy 'push))
      (puthash "61" "2026-08-25T00:00:00Z" map)
      (let ((run (test-dry-run-84--run test-dry-run-84--stamped
                                        (list :title "Lab 1" :canvas-id "61") map nil)))
        (expect (car (cdr run)) :to-match "org-canvas-conflict-strategy is push"))))

  (it "still counts an entry Canvas has not touched as an update"
    (let ((map (make-hash-table :test 'equal)))
      (puthash "61" "2026-08-01T00:00:00Z" map)
      (let ((run (test-dry-run-84--run test-dry-run-84--stamped
                                        (list :title "Lab 1" :canvas-id "61") map nil)))
        (expect (plist-get (car run) :dry-run) :to-equal 1)
        (expect (plist-get (car run) :dry-run-conflict) :to-equal 0)
        (expect (car (cdr run)) :to-equal "[DRY-RUN] Would UPDATE 'Lab 1'"))))

  (it "counts a create whose title Canvas already holds as a conflict (issue #85)"
    (let ((titles (make-hash-table :test 'equal))
          (org-canvas-duplicate-title-strategy nil) (noninteractive nil))
      (puthash "R11" '(((id . 2563810) (name . "R11"))) titles)
      (let ((run (test-dry-run-84--run "* R11\n" (list :title "R11" :canvas-id nil)
                                        nil titles)))
        (expect (plist-get (car run) :dry-run-conflict) :to-equal 1)
        (expect (car (cdr run))
                :to-equal "[DRY-RUN] Would CONFLICT 'R11' (title already on Canvas as id 2563810; a real sync would ask)"))))

  (it "names a page holder by url and lists several holders"
    (let ((titles (make-hash-table :test 'equal))
          (org-canvas-duplicate-title-strategy 'adopt))
      (puthash "Welcome" '(((url . "welcome")) ((url . "welcome-2"))) titles)
      (let ((run (test-dry-run-84--run "* Welcome\n" (list :title "Welcome" :canvas-id nil)
                                        nil titles)))
        (expect (car (cdr run))
                :to-match "as id welcome, welcome-2; org-canvas-duplicate-title-strategy is adopt"))))

  (it "reports a plain create as a create"
    (let ((run (test-dry-run-84--run "* R11\n" (list :title "R11" :canvas-id nil)
                                      nil (make-hash-table :test 'equal))))
      (expect (plist-get (car run) :dry-run) :to-equal 1)
      (expect (car (cdr run)) :to-equal "[DRY-RUN] Would CREATE 'R11'"))))

(describe "org-canvas--sync-log-summary dry-run conflicts (issue #84)"
  (defun test-summary-84--run (counters)
    "Log a summary for COUNTERS.  Returns (LOG-LINES . MESSAGES)."
    (let ((temp-file (make-temp-file "summary-test" nil ".org"))
          (logged nil) (msgs nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (cl-letf (((symbol-function 'org-canvas--log-info)
                       (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
              (org-canvas--sync-log-summary "assignments" temp-file counters))
            (cons (nreverse logged) (nreverse msgs)))
        (let ((buf (find-buffer-visiting temp-file))) (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "reports would-conflict apart from would-sync"
    (let ((run (test-summary-84--run
                '(:success 0 :skip 39 :fail 0 :dry-run 27 :dry-run-conflict 10))))
      (expect (car run) :to-contain "Would sync: 27 | Would conflict: 10 | Skipped: 39")
      (expect (cdr run)
              :to-equal '("Assignments dry-run: 27 would sync, 10 would conflict, 39 skipped."))))

  (it "stays in dry-run form when every pending entry would conflict"
    (let ((run (test-summary-84--run
                '(:success 0 :skip 5 :fail 0 :dry-run 0 :dry-run-conflict 3))))
      (expect (car (cdr run)) :to-equal "Assignments dry-run: 0 would sync, 3 would conflict, 5 skipped."))))

(describe "org-canvas--sync-summary-columns dry-run conflicts (issue #84)"
  (it "adds a Conflicts column to the dry-run table when some entries would"
    (let ((org-canvas--dry-run nil))
      (expect (org-canvas--sync-summary-columns
               '((:dry-run 27 :dry-run-conflict 10 :skip 39)))
              :to-equal '(("Would sync" . :dry-run) ("Conflicts" . :dry-run-conflict)
                          ("Skipped" . :skip) ("Failed" . :fail)
                          ("Deferred" . :deferred)))))

  (it "treats stats with only would-conflict entries as a dry run"
    (let ((org-canvas--dry-run nil))
      (expect (org-canvas--sync-stats-dry-run-p '((:dry-run 0 :dry-run-conflict 2)))
              :to-be-truthy)
      (expect (org-canvas--sync-stats-dry-run-p '((:success 2))) :to-be nil)))

  (it "treats any stats as a dry run while one is running"
    (let ((org-canvas--dry-run t))
      (expect (org-canvas--sync-stats-dry-run-p '((:success 0))) :to-be-truthy)))

  (it "carries the count into the global table"
    (let ((org-canvas--dry-run nil)
          (org-canvas--sync-global-feature-stats nil)
          (org-canvas--sync-global-counters (list :success 0))
          (logged nil))
      (org-canvas--sync-record-feature-stats "Assignments"
                                             '(:dry-run 27 :dry-run-conflict 10 :skip 39))
      (expect (plist-get org-canvas--sync-global-counters :dry-run-conflict) :to-equal 10)
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (let ((lines (nreverse logged)))
        (expect (nth 0 lines) :to-match "DRY RUN")
        (expect (nth 1 lines) :to-match "Would sync +Conflicts +Skipped")
        (expect (nth 2 lines) :to-match "Assignments +27 +10 +39")))))

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

(describe "org-canvas--sync-execute-pipeline duplicate outcome (issue #85)"
  (it "counts a duplicate-title skip among the skipped and names it"
    (with-temp-org-buffer "* R11\n"
      (org-back-to-heading)
      (let* ((counters (list :success 0 :skip 0 :fail 0 :conflict 0))
             (ctx (list :push-fn (lambda (_d _p &optional _ctx) 'duplicate)
                        :feature-name "assignments" :total-count 1
                        :counters counters :synced-ids (list nil)))
             (msgs nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
          (org-canvas--sync-execute-pipeline
           (list :title "R11" :canvas-id nil) '((name . "R11")) ctx))
        (expect (plist-get counters :skip) :to-equal 1)
        (expect (plist-get counters :success) :to-equal 0)
        (expect (car (plist-get counters :skipped-titles))
                :to-equal "R11 (already on Canvas; adopt it with org-canvas-adopt-at-point or rename)")
        (expect (car msgs) :to-equal "Assignments [1/1] SKIPPED: 'R11' (title already on Canvas)")))))

(describe "org-canvas--sync-run-pipeline title index (issue #85)"
  (defun test-titles-85--run (snapshot)
    "Push one unstamped heading through the pipeline with SNAPSHOT.
Returns the :remote-titles of the run context the push received."
    (let ((file (make-temp-file "titles-" nil ".org"))
          (seen 'unset))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* R11\n"))
            (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                      ((symbol-function 'org-canvas--sync-log-summary) #'ignore)
                      ((symbol-function 'org-canvas--sync-fetch-remote-snapshot)
                       (lambda (&rest _) snapshot))
                      ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                      ((symbol-function 'org-canvas--save-buffer) #'ignore))
		     (org-canvas--sync-run-pipeline (list
						     :feature "assignments" :file file :query "LEVEL=1"
						     :parse (lambda () (list :title "R11" :canvas-id nil :pom (point-marker)))
						     :build (lambda (_data) '((name . "R11")))
						     :push (lambda (_data _payload &optional ctx)
							     (setq seen (plist-get ctx :remote-titles))
							     '((id . 1)))
						     :finalize #'ignore)))
            seen)
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "binds the snapshot's titles while entries push"
    (let ((titles (make-hash-table :test 'equal)))
      (expect (test-titles-85--run (list :updated (make-hash-table :test 'equal)
                                         :titles titles))
              :to-be titles)))

  (it "marks the run as unchecked when there is no snapshot"
    (expect (test-titles-85--run nil) :to-equal 'none)))

(describe "org-canvas--push-at-point-runtime stopped push (issues #85, #86)"
  (it "reports a duplicate instead of finalizing a symbol"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* R11\n"
        (org-back-to-heading)
        (let ((finalized nil) (msgs nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
		   (org-canvas--push-at-point-runtime (list
						       :feature "assignment"
						       :parse (lambda () (list :title "R11" :canvas-id nil :pom (point)))
						       :build (lambda (_data) '((name . "R11")))
						       :push (lambda (_data _payload &optional _ctx) 'duplicate)
						       :finalize (lambda (_data _response &optional _ctx) (setq finalized t))
						       :title-key :title)))
          (expect finalized :to-be nil)
          (expect (car msgs)
                  :to-equal "Assignment 'R11' not pushed — Canvas already holds this title; adopt it with M-x org-canvas-adopt-at-point or rename.")))))

  (it "words a conflict and a pull"
    (let ((msgs nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) msgs)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--push-at-point-report-stop "page" "Welcome" 'conflict)
        (org-canvas--push-at-point-report-stop "page" "Welcome" 'pulled))
      (expect (nth 1 msgs) :to-match "remote item was modified")
      (expect (nth 0 msgs) :to-match "remote version was pulled"))))

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

(describe "org-canvas--push-at-point-runtime refreshes #+LAST_SYNCED (issue #104)"
  (it "writes the file header from the stamp finalize recorded"
    ;; assignments.org read [2026-08-19] after sixty at-point pushes:
    ;; only the full-sync path ever touched the header.
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (search-forward "* Lab 1")
       (org-back-to-heading t)
       (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                 ((symbol-function 'display-buffer) #'ignore))
		(org-canvas--push-at-point-runtime (list
						    :feature "assignment"
						    :parse (lambda () (list :title "Lab 1" :canvas-id "61" :pom (point-marker)))
						    :build (lambda (_data) '((name . "Lab 1")))
						    :push (lambda (_data _payload &optional _ctx)
							    '((id . 61) (updated_at . "2026-09-01T14:10:30Z")))
						    :finalize (lambda (data response &optional ctx) (org-canvas--finalize-item data response :ctx ctx))
						    :title-key :title)))
       (expect (org-canvas--pull-read-file-header)
               :to-equal (format-time-string
                          "[%Y-%m-%d %a %H:%M]"
                          (time-add (date-to-time "2026-09-01T14:10:30Z") 60))))))

  (it "leaves the header alone when the push stops at a conflict"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (search-forward "* Lab 1")
       (org-back-to-heading t)
       (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                 ((symbol-function 'display-buffer) #'ignore))
		(org-canvas--push-at-point-runtime (list
						    :feature "assignment"
						    :parse (lambda () (list :title "Lab 1" :canvas-id "61" :pom (point-marker)))
						    :build (lambda (_data) '((name . "Lab 1")))
						    :push (lambda (_data _payload &optional _ctx) 'conflict)
						    :finalize (lambda (data response &optional ctx) (org-canvas--finalize-item data response :ctx ctx))
						    :title-key :title)))
       (expect (org-canvas--pull-read-file-header)
               :to-equal "[2026-08-19 Wed 09:59]")))))

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

(describe "org-canvas--sync-execute-pipeline skip path backfills the baseline (issue #104)"
  (it "records CANVAS_UPDATED_AT for an unchanged legacy entry"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading t)
     (let* ((payload '((name . "Lab 1")))
            (map (make-hash-table :test 'equal))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :push-fn (lambda (&rest _) (error "Must not push"))
                       :feature-name "assignments"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil)
                       :remote-updated map
                       :baseline (encode-time
                                  (org-parse-time-string "[2026-08-19 Wed 12:00]")))))
       (puthash "61" "2026-08-01T00:00:00Z" map)
       (org-entry-put (point) "PAYLOAD_HASH" (md5 (json-encode payload)))
       (org-canvas--sync-execute-pipeline
        (list :title "Lab 1" :canvas-id "61" :pom (point-marker)) payload ctx)
       (expect (plist-get counters :skip) :to-equal 1)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-08-01T00:00:00Z")))))

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

(describe "org-canvas--sync-check-spec"
  (it "accepts a spec made only of known keys holding the required ones"
    (expect (org-canvas--sync-check-spec
             (list :feature "pages" :parse #'ignore :hash-extra nil)
             '(:feature :parse))
            :not :to-throw))

  (it "names an unknown key, so a misspelt option cannot vanish"
    (expect (org-canvas--sync-check-spec (list :feature "pages" :parser #'ignore)
                                         '(:feature))
            :to-throw 'error))

  (it "names a missing required key"
    (expect (org-canvas--sync-check-spec (list :feature "pages") '(:feature :parse))
            :to-throw 'error)))

(describe "org-canvas-define-sync builds the spec its runners read"
  (it "runs the pipeline with the module's options under their own names"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas--sync-run-pipeline)
                 (lambda (spec) (setq seen spec) nil)))
        (org-canvas-sync-announcements))
      (expect (plist-get seen :feature) :to-equal "announcements")
      (expect (plist-get seen :query) :to-equal "LEVEL=1")
      (expect (plist-get seen :parse) :to-be #'org-canvas--announcement-parse-entry)
      (expect (plist-get seen :pull-item-fn) :to-be #'org-canvas--announcement-pull-item)
      (expect (functionp (plist-get seen :push)) :to-be t)
      (expect (functionp (plist-get seen :finalize)) :to-be t)
      (expect (org-canvas--sync-check-spec seen nil) :not :to-throw))))


(describe "sync spec :prepare and :first"
  (it "runs :prepare once before the first entry and keeps its result in the context"
    (let ((file (make-temp-file "prepare-" nil ".org"))
          (seen nil) (prepared 0))
      (unwind-protect
          (with-org-canvas-test-config
            (with-temp-file file
              (insert "* One\n:PROPERTIES:\n:END:\n* Two\n:PROPERTIES:\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore))
              (org-canvas--sync-run-pipeline
               (list :feature "things" :file file :query "LEVEL=1"
                     :parse (lambda () (list :title (org-get-heading t t t t)
                                             :canvas-id "1" :pom (point-marker)))
                     :build (lambda (_data) '((name . "x")))
                     :push (lambda (_data _payload &optional ctx)
                             (push (plist-get ctx :prepared) seen)
                             '((id . 1)))
                     :finalize (lambda (&rest _) nil)
                     :prepare (lambda (ctx)
                                (cl-incf prepared)
                                (format "root-of-%s" (plist-get ctx :feature-name))))))
            (expect prepared :to-equal 1)
            (expect seen :to-equal '("root-of-things" "root-of-things")))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "stops the run before any push when :prepare signals"
    (let ((file (make-temp-file "prepare-" nil ".org"))
          (pushed nil))
      (unwind-protect
          (with-org-canvas-test-config
            (with-temp-file file (insert "* One\n"))
            (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore))
              (expect (org-canvas--sync-run-pipeline
                       (list :feature "things" :file file :query "LEVEL=1"
                             :parse (lambda () (list :title "One" :pom (point-marker)))
                             :build (lambda (_data) nil)
                             :push (lambda (&rest _) (setq pushed t))
                             :finalize #'ignore
                             :prepare (lambda (_ctx) (error "No root"))))
                      :to-throw 'error))
            (expect pushed :to-be nil))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "runs :prepare before the parse of a push at point"
    (with-temp-org-buffer "* One\n"
      (org-back-to-heading)
      (let ((order nil))
        (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
          (org-canvas--push-at-point-runtime
           (list :feature "thing"
                 :parse (lambda () (push 'parse order) (list :title "One" :canvas-id "1" :pom (point)))
                 :build (lambda (_data) '((name . "x")))
                 :push (lambda (_data _payload &optional ctx)
                         (push (plist-get ctx :prepared) order) '((id . 1)))
                 :finalize (lambda (&rest _) nil)
                 :prepare (lambda (_ctx) (push 'prepare order) 'root))))
        (expect (nreverse order) :to-equal '(prepare parse root)))))

  (it "makes the generated sync run its :first command beforehand, keeping the log"
    (let ((order nil))
      (cl-letf (((symbol-function 'org-canvas--sync-run-pipeline)
                 (lambda (spec)
                   (push (list (plist-get spec :feature) org-canvas--inhibit-log-clear) order)
                   nil))
                ((symbol-function 'org-canvas-sync-outcome-groups)
                 (lambda () (push (list "outcome-groups" org-canvas--inhibit-log-clear) order))))
        (org-canvas-sync-outcomes))
      (expect (nreverse order)
              :to-equal '(("outcome-groups" t) ("outcomes" t))))))


(describe "sync spec :hash and :dry-run, and the push results they allow"
  (defmacro test-sync-hash--with-file (content &rest body)
    "Run BODY with FILE bound to a temp Org file holding CONTENT, in test config."
    (declare (indent 1))
    `(let ((file (make-temp-file "hash-" nil ".org")))
       (unwind-protect
           (with-org-canvas-test-config
             (with-temp-file file (insert ,content))
             (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore))
               ,@body))
         (let ((buf (find-buffer-visiting file)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
         (delete-file file))))

  (defun test-sync-hash--spec (file &rest keys)
    "A minimal sync spec over FILE whose push records a call, plus KEYS."
    (append (list :feature "things" :file file :query "LEVEL=1"
                  :parse (lambda () (list :title (org-get-heading t t t t)
                                          :canvas-id (org-entry-get (point) "CANVAS_ID")
                                          :pom (point-marker)))
                  :build (lambda (_data) '((name . "x")))
                  :finalize (lambda (&rest _) nil))
            keys))

  (it "skips on a stored hash the :hash function reproduces, and stamps its value"
    (test-sync-hash--with-file "* Same\n:PROPERTIES:\n:CANVAS_ID: 1\n:PAYLOAD_HASH: content-1\n:END:\n* Fresh\n:PROPERTIES:\n:CANVAS_ID: 2\n:END:\n"
      (let ((pushed nil))
        (org-canvas--sync-run-pipeline
         (test-sync-hash--spec file
                               :push (lambda (data _payload &optional _ctx)
                                       (push (plist-get data :title) pushed) '((id . 2)))
                               :hash (lambda (_payload data)
                                       (format "content-%s" (plist-get data :canvas-id)))))
        (expect pushed :to-equal '("Fresh"))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-max))
          (org-back-to-heading t)
          (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal "content-2")))))

  (it "never skips and never stamps when the push owns the hash"
    (test-sync-hash--with-file "* Same\n:PROPERTIES:\n:CANVAS_ID: 1\n:PAYLOAD_HASH: mine\n:END:\n"
      (let ((pushed 0))
        (org-canvas--sync-run-pipeline
         (test-sync-hash--spec file
                               :push (lambda (&rest _) (cl-incf pushed) t)
                               :hash 'push))
        (expect pushed :to-equal 1)
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (org-back-to-heading t)
          (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal "mine")))))

  (it "counts a push that answers skip as a skip, without finalizing"
    (test-sync-hash--with-file "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let ((ctx (org-canvas--sync-run-pipeline
                  (test-sync-hash--spec file
                                        :push (lambda (&rest _) 'skip)
                                        :finalize (lambda (&rest _) (error "must not finalize"))
                                        :hash 'push))))
        (expect (plist-get (plist-get ctx :counters) :skip) :to-equal 1)
        (expect (plist-get (plist-get ctx :counters) :success) :to-equal 0))))

  (it "lets a push preview a dry run itself when :dry-run is push, and counts the sentinel"
    (test-sync-hash--with-file "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let* ((org-canvas--dry-run t)
             (called nil)
             (ctx (org-canvas--sync-run-pipeline
                   (test-sync-hash--spec file
                                         :push (lambda (&rest _) (setq called t)
                                                 org-canvas--dry-run-response)
                                         :dry-run 'push))))
        (expect called :to-be t)
        (expect (plist-get (plist-get ctx :counters) :dry-run) :to-equal 1))))

  (it "reports from the snapshot under a dry run when the push does not preview"
    (test-sync-hash--with-file "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let* ((org-canvas--dry-run t)
             (called nil)
             (ctx (org-canvas--sync-run-pipeline
                   (test-sync-hash--spec file
                                         :push (lambda (&rest _) (setq called t) t)))))
        (expect called :to-be nil)
        (expect (plist-get (plist-get ctx :counters) :dry-run) :to-equal 1))))

  (it "counts a heading the parser declines as a skip, not a failure"
    (test-sync-hash--with-file "* Folder\n* File\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let* ((pushed nil)
             (spec (test-sync-hash--spec file :push (lambda (data &rest _)
                                                      (push (plist-get data :title) pushed) t)
                                         :hash 'push))
             (ctx (progn
                    (plist-put spec :parse
                               (lambda () (unless (string= (org-get-heading t t t t) "Folder")
                                            (list :title (org-get-heading t t t t)
                                                  :canvas-id "1" :pom (point-marker)))))
                    (org-canvas--sync-run-pipeline spec))))
        (expect pushed :to-equal '("File"))
        (expect (plist-get (plist-get ctx :counters) :skip) :to-equal 1)
        (expect (plist-get (plist-get ctx :counters) :fail) :to-equal 0)
        (expect (plist-get (plist-get ctx :counters) :success) :to-equal 1))))

  (it "rejects a hash mode the spec keys do not know"
    (expect (org-canvas--sync-check-spec (list :feature "x" :hashing 'push) '(:feature))
            :to-throw 'error)))

(describe "push at point with a push-owned hash"
  (it "calls the push despite a matching stored hash, stamps nothing, and returns the context"
    (with-temp-org-buffer "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:PAYLOAD_HASH: mine\n:END:\n"
      (org-back-to-heading)
      (let ((pushed nil) ctx)
        (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
          (setq ctx (org-canvas--push-at-point-runtime
                     (list :feature "thing"
                           :parse (lambda () (list :title "One" :canvas-id "1" :pom (point)))
                           :build (lambda (_data) '((name . "x")))
                           :push (lambda (_data _payload &optional ctx)
                                   (setq pushed t)
                                   (org-canvas--ctx-push ctx :file-changed-ids "One")
                                   t)
                           :finalize (lambda (&rest _) nil)
                           :hash 'push))))
        (expect pushed :to-be t)
        (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal "mine")
        (expect (plist-get ctx :file-changed-ids) :to-equal '("One")))))

  (it "reports a push that answers skip as unchanged"
    (with-temp-org-buffer "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (org-back-to-heading)
      (let ((said nil))
        (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (org-canvas--push-at-point-runtime
           (list :feature "thing"
                 :parse (lambda () (list :title "One" :canvas-id "1" :pom (point)))
                 :build (lambda (_data) nil)
                 :push (lambda (&rest _) 'skip)
                 :finalize (lambda (&rest _) (error "must not finalize"))
                 :hash 'push)))
        (expect said :to-match "unchanged")))))


(describe "org-canvas--sync-collect-entries scopes ids to the query (issue #196)"
  (defmacro test-196--with-file (content &rest body)
    "Run BODY with FILE bound to a temp Org file holding CONTENT."
    (declare (indent 1))
    `(let ((file (make-temp-file "orphan-" nil ".org")))
       (unwind-protect
           (progn (with-temp-file file (insert ,content))
                  (cl-letf (((symbol-function 'org-canvas--log-info) #'ignore))
                    ,@body))
         (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
         (delete-file file))))

  (it "collects only the ids of the headings the query selects"
    (test-196--with-file "* Module\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** Item\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n* Stale\n:PROPERTIES:\n:CANVAS_ID: 2\n:END:\n"
      (let ((entries (org-canvas--sync-collect-entries file "LEVEL=1" "modules")))
        (expect (plist-get entries :all-ids-before) :to-equal '("1" "2"))
        (expect (length (plist-get entries :targets)) :to-equal 2)
        (dolist (m (plist-get entries :targets)) (set-marker m nil)))))

  (it "reads the other level when the query names it"
    (test-196--with-file "* Group\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** Outcome\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
      (let ((entries (org-canvas--sync-collect-entries file "LEVEL=2" "outcomes")))
        (expect (plist-get entries :all-ids-before) :to-equal '("55"))
        (dolist (m (plist-get entries :targets)) (set-marker m nil)))))

  (it "keeps a heading without an id out of the list but among the targets"
    (test-196--with-file "* New\n* Stamped\n:PROPERTIES:\n:CANVAS_ID: 9\n:END:\n"
      (let ((entries (org-canvas--sync-collect-entries file "LEVEL=1" "pages")))
        (expect (plist-get entries :all-ids-before) :to-equal '("9"))
        (expect (length (plist-get entries :targets)) :to-equal 2)
        (dolist (m (plist-get entries :targets)) (set-marker m nil)))))

  (it "does not warn about a stamped child of a synced parent, but still about an unreached parent"
    (test-196--with-file "* Module\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** Item\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
      (let ((warned nil))
        (with-org-canvas-test-config
          (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore)
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) warned))))
            (org-canvas--sync-run-pipeline
             (list :feature "modules" :file file :query "LEVEL=1"
                   :parse (lambda () (list :title (org-get-heading t t t t)
                                           :canvas-id (org-entry-get (point) "CANVAS_ID")
                                           :pom (point-marker)))
                   :build (lambda (_data) '((name . "x")))
                   :push (lambda (&rest _) '((id . 1)))
                   :finalize (lambda (&rest _) nil)))))
        (expect (cl-some (lambda (w) (string-match-p "\\[Orphan\\]" w)) warned) :to-be nil)))))


(provide 'org-canvas-core-sync-test)
;;; org-canvas-core-sync-test.el ends here
