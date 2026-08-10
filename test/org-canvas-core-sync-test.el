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

;;;; 8. Mock API recording helpers

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

;;;; 9. Search Item Helper

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

;;;; 10. Push to API Helper

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

;;;; 11. Finalize Item Helper

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
                                  :post-fn (lambda (_d _r) (setq post-called t)))
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

;;;; 12. Delete All Items Helper

(describe "org-canvas--delete-all-items (mocked)"
  (it "deletes items from Canvas via queued helper"
    (with-org-canvas-test-config
      (let ((queued-args nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((id . 1) (title . "Item 1"))
                       ((id . 2) (title . "Item 2")))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (items endpoint-fn id-field title-field &optional skip-fn _delete-data)
                     (setq queued-args (list items endpoint-fn id-field title-field skip-fn))
                     (cons 2 '("1" "2")))))
          (let ((deleted (org-canvas--delete-all-items "items"
                           :endpoint "items"
                           :file nil)))
            (expect deleted :to-equal 2)
            ;; Verify queued helper received correct args
            (expect (length (nth 0 queued-args)) :to-equal 2)
            (expect (nth 2 queued-args) :to-equal 'id)
            (expect (nth 3 queued-args) :to-equal 'title))))))

  (it "passes skip-fn to queued helper"
    (with-org-canvas-test-config
      (let ((queued-skip-fn nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((id . 1) (title . "Keep") (front_page . t))
                       ((id . 2) (title . "Delete") (front_page . :json-false)))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (_items _endpoint-fn _id-field _title-field &optional skip-fn _delete-data)
                     (setq queued-skip-fn skip-fn)
                     (cons 1 '("2")))))
          (org-canvas--delete-all-items "pages"
            :endpoint "pages"
            :file nil
            :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
          (expect queued-skip-fn :not :to-be nil)))))

  (it "uses custom id-field and title-field"
    (with-org-canvas-test-config
      (let ((queued-args nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((url . "my-page") (name . "My Page")))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (items endpoint-fn id-field title-field &optional _skip-fn _delete-data)
                     (setq queued-args (list items endpoint-fn id-field title-field))
                     (cons 1 '("my-page")))))
          (let ((deleted (org-canvas--delete-all-items "pages"
                           :endpoint "pages"
                           :file nil
                           :id-field 'url
                           :title-field 'name)))
            (expect deleted :to-equal 1)
            (expect (nth 2 queued-args) :to-equal 'url)
            (expect (nth 3 queued-args) :to-equal 'name))))))

  (it "constructs correct endpoint-fn"
    (with-org-canvas-test-config
      (let ((captured-endpoint-fn nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((id . 42) (title . "Test")))))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (_items endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                     (setq captured-endpoint-fn endpoint-fn)
                     (cons 1 '("42")))))
          (org-canvas--delete-all-items "items"
            :endpoint "things"
            :file nil)
          ;; Verify endpoint-fn produces correct URL
          (let ((url (funcall captured-endpoint-fn 42)))
            (expect url :to-match "things/42$"))))))

  (it "cleans local properties from org file"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item 1
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:

* Item 2
:PROPERTIES:
:CANVAS_ID: 2
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 1) (title . "Item 1")))))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                           (cons 1 '("1")))))
                (org-canvas--delete-all-items "items"
                  :endpoint "items"
                  :file temp-file)))
            ;; Check that both items' properties were cleared
            (with-current-buffer (find-file-noselect temp-file)
              (goto-char (point-min))
              (org-back-to-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
              ;; Item 2 should also be cleared (delete-all cleans all properties)
              (outline-next-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))
        (delete-file temp-file)))))

;;;; 12b. Queued Delete Helper

(describe "org-canvas--delete-items-queued"
  (it "returns (0 . nil) for empty items list"
    (let ((result (org-canvas--delete-items-queued
                   nil
                   (lambda (id) (format "http://example.com/%s" id))
                   'id 'title)))
      (expect (car result) :to-equal 0)
      (expect (cdr result) :to-be nil)))

  (it "returns (0 . nil) when all items are skipped"
    (let ((result (org-canvas--delete-items-queued
                   '(((id . 1) (title . "A")) ((id . 2) (title . "B")))
                   (lambda (id) (format "http://example.com/%s" id))
                   'id 'title
                   (lambda (_item) t))))
      (expect (car result) :to-equal 0)
      (expect (cdr result) :to-be nil)))

  (it "deletes items and collects results"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args) nil)))
        (let ((result (org-canvas--delete-items-queued
                       '(((id . 1) (title . "First"))
                         ((id . 2) (title . "Second")))
                       (lambda (id) (format "http://example.com/%s" id))
                       'id 'title)))
          (expect (car result) :to-equal 2)
          (expect (member "1" (cdr result)) :to-be-truthy)
          (expect (member "2" (cdr result)) :to-be-truthy)))))

  (it "continues on error and only counts successes"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (when (= call-count 1)
                       (error "Delete failed")))))
          (let ((result (org-canvas--delete-items-queued
                         '(((id . 1) (title . "Fail"))
                           ((id . 2) (title . "Succeed")))
                         (lambda (id) (format "http://example.com/%s" id))
                         'id 'title)))
            (expect (car result) :to-equal 1)
            (expect (cdr result) :to-equal '("2")))))))

  (it "converts numeric IDs to strings in deleted-ids"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args) nil)))
        (let ((result (org-canvas--delete-items-queued
                       '(((id . 42) (title . "Numeric")))
                       (lambda (id) (format "http://example.com/%s" id))
                       'id 'title)))
          (expect (car result) :to-equal 1)
          (expect (car (cdr result)) :to-equal "42")))))

  (it "keeps string IDs as-is in deleted-ids"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args) nil)))
        (let ((result (org-canvas--delete-items-queued
                       '(((id . "my-page") (title . "String")))
                       (lambda (id) (format "http://example.com/%s" id))
                       'id 'title)))
          (expect (car result) :to-equal 1)
          (expect (car (cdr result)) :to-equal "my-page")))))

  (it "passes correct URL from endpoint-fn"
    (with-org-canvas-test-config
      (let ((captured-url nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method url &rest _args)
                     (setq captured-url url)
                     nil)))
          (org-canvas--delete-items-queued
           '(((id . 5) (title . "Test")))
           (lambda (id) (format "http://canvas.example.com/items/%s" id))
           'id 'title)
          (expect captured-url :to-equal "http://canvas.example.com/items/5")))))

  (it "skips items with skip-fn and deletes the rest"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args) nil)))
        (let ((result (org-canvas--delete-items-queued
                       '(((id . 1) (title . "Keep") (front_page . t))
                         ((id . 2) (title . "Delete") (front_page . :json-false)))
                       (lambda (id) (format "http://example.com/%s" id))
                       'id 'title
                       (lambda (item) (eq (alist-get 'front_page item) t)))))
          (expect (car result) :to-equal 1))))))

;;;; 13. Delete Item at Point Helper

(describe "org-canvas--delete-item-at-point (mocked)"
  (it "deletes item and clears properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test Item
:PROPERTIES:
:CANVAS_ID: 555
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas--delete-item-at-point "item"
             :endpoint "items/%s")
           (expect-api-called 'DELETE "items/555")
           (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
           (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))

  (it "errors when no CANVAS_ID present"
    (with-temp-org-buffer
     "* New Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--delete-item-at-point "item" :endpoint "items/%s")
             :to-throw 'user-error)))

  (it "uses custom id-property"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* My Page
:PROPERTIES:
:CANVAS_URL: my-page-slug
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas--delete-item-at-point "page"
             :endpoint "pages/%s"
             :id-property "CANVAS_URL")
           (expect-api-called 'DELETE "pages/my-page-slug")))))))

;;;; 17. Push to API Edge Cases

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
                (find-fn (lambda (_title) '((id . 789)))))
            (let ((result (org-canvas--push-to-api data payload
                                                   :endpoint "items"
                                                   :find-fn find-fn)))
              (expect (alist-get 'id result) :to-equal 789))))))))

;;;; 18. Delete All Items Edge Cases

(describe "org-canvas--delete-all-items edge cases (mocked)"
  (it "passes list-params to GET request"
    (with-org-canvas-test-config
      (let ((captured-params nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional params)
                     (setq captured-params params)
                     nil))
                  ((symbol-function 'org-canvas--delete-items-queued)
                   (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                     (cons 0 nil))))
          (org-canvas--delete-all-items "items"
            :endpoint "items"
            :file nil
            :list-params '(("filter" . "active")))
          (expect captured-params :to-equal '(("filter" . "active")))))))

  (it "returns 0 for empty remote items"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params) nil))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                   (cons 0 nil))))
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 0)))))

  (it "does not clean properties when file is nil"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 1) (title . "Item")))))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                   (cons 1 '("1")))))
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 1))))))

;;;; 19. Delete Item at Point Edge Cases

(describe "org-canvas--delete-item-at-point edge cases (mocked)"
  (it "handles delete failure gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Cannot delete"))))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (with-temp-org-buffer
         "* Item to Delete
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
         (org-back-to-heading)
         ;; Should return nil on failure, not throw
         (let ((result (org-canvas--delete-item-at-point "item" :endpoint "items/%s")))
           (expect result :to-be nil))))))

  (it "returns nil when user cancels"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (with-temp-org-buffer
         "* Item
:PROPERTIES:
:CANVAS_ID: 456
:END:
"
         (org-back-to-heading)
         ;; Should return nil when user cancels
         (let ((result (org-canvas--delete-item-at-point "item" :endpoint "items/%s")))
           (expect result :to-be nil)))))))

;;;; 22. Search Item Edge Cases

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

;;;; 23. Finalize Item Edge Cases

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
                                  :post-fn (lambda (d r)
                                             (setq post-fn-called-with (list d r))))
       (expect post-fn-called-with :to-be-truthy)
       (expect (car post-fn-called-with) :to-equal data)
       (expect (cadr post-fn-called-with) :to-equal response)))))

;;;; 27. org-canvas--push-to-api nested recovery

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

;;;; 30. org-canvas-define-sync macro generated functions

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

;;;; 31. org-canvas--delete-all-items edge paths

(describe "org-canvas--delete-all-items edge paths"
  (it "handles empty remote items list"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params) nil))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                   (cons 0 nil))))
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 0)))))

  (it "cleans all properties even when queued helper reports partial success"
    (let ((temp-file (make-temp-file "test-canvas" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item A
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:

* Item B
:PROPERTIES:
:CANVAS_ID: 2
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 1) (title . "Item A"))
                             ((id . 2) (title . "Item B")))))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                           ;; Simulate: item 1 deleted, item 2 failed
                           (cons 1 '("1")))))
                (org-canvas--delete-all-items "items"
                  :endpoint "items"
                  :file temp-file)))
            ;; Both items should be cleaned (delete-all cleans all properties)
            (with-current-buffer (find-file-noselect temp-file)
              (goto-char (point-min))
              (org-back-to-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
              (outline-next-heading)
              (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))
        (delete-file temp-file))))

  (it "does not clean properties when file is nil"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 1) (title . "Item")))))
                ((symbol-function 'org-canvas--delete-items-queued)
                 (lambda (_items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                   (cons 1 '("1")))))
        ;; Should not error even with nil file
        (let ((deleted (org-canvas--delete-all-items "items"
                         :endpoint "items"
                         :file nil)))
          (expect deleted :to-equal 1))))))

;;;; 32. org-canvas--delete-item-at-point edge paths

(describe "org-canvas--delete-item-at-point additional tests"
  (it "preserves properties on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Server error"))))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (with-temp-org-buffer
         "* Test Item
:PROPERTIES:
:CANVAS_ID: 999
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (org-canvas--delete-item-at-point "item" :endpoint "items/%s")
         ;; Properties should be preserved on failure
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "999")
         (expect (org-entry-get (point) "LAST_SYNCED") :to-equal "[2024-01-01 Mon]")))))

  (it "succeeds and clears all sync properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Item
:PROPERTIES:
:CANVAS_ID: 555
:CANVAS_URL: my-url
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (let ((result (org-canvas--delete-item-at-point "item" :endpoint "items/%s")))
             (expect result :to-be t)
             (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
             (expect (org-entry-get (point) "CANVAS_URL") :to-be nil)
             (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))))

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

(describe "org-canvas-define-delete-all macro validation"
  (it "errors when :endpoint is missing"
    (expect (macroexpand '(org-canvas-define-delete-all test-bad
                            :file some-file))
            :to-throw 'error '("org-canvas-define-delete-all: :endpoint is required")))

  (it "errors when :file is missing"
    (expect (macroexpand '(org-canvas-define-delete-all test-bad
                            :endpoint "items"))
            :to-throw 'error '("org-canvas-define-delete-all: :file is required"))))

(describe "org-canvas-define-delete-at-point macro validation"
  (it "errors when neither :endpoint nor :delete-url-fn provided"
    (expect (macroexpand '(org-canvas-define-delete-at-point test-bad))
            :to-throw 'error '("org-canvas-define-delete-at-point: :endpoint or :delete-url-fn required"))))

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
            ;; Even with find-fn, non-timeout errors should re-throw
            (expect (org-canvas--push-to-api data payload
                                             :endpoint "items"
                                             :find-fn (lambda (_) '((id . 1))))
                    :to-throw 'error)))))))

;;;; 37. finalize-item pom nil-guard

(describe "org-canvas--finalize-item pom nil-guard"
  (it "errors when :pom is missing from data"
    (let ((data (list :title "No POM"))
          (response '((id . 123))))
      (expect (org-canvas--finalize-item data response)
              :to-throw 'error))))

;;;; Payload Hashing / Skip Logic (via sync macro)

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

;;;; Metamorphic relations

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

;;;; Push-at-Point Macro

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

;;;; Conflict Detection

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
                    (lambda (_data _remote) 'skip)))
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
                       :push-fn (lambda (_data _payload) 'conflict)
                       :finalize-fn (lambda (_data _response) nil)
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

;;;; Interactive Conflict Resolution

(describe "org-canvas--conflict-format-diff"
  (it "creates a buffer with conflict details"
    (let ((data (list :title "My Page" :description "local body text"
                      :pom nil))
          (remote '((title . "My Page Remote")
                    (updated_at . "2026-02-01T10:00:00Z")
                    (body . "remote body text")))
          (org-canvas--current-pull-item-fn #'ignore))
      (let ((buf (org-canvas--conflict-format-diff data remote)))
        (unwind-protect
            (with-current-buffer buf
              (expect (buffer-string) :to-match "Conflict: My Page")
              (expect (buffer-string) :to-match "Remote updated_at:")
              (expect (buffer-string) :to-match "Title")
              (expect (buffer-string) :to-match "My Page Remote"))
          (when (buffer-live-p buf) (kill-buffer buf))))))

  (it "handles nil body gracefully"
    (let ((data (list :title "No Body" :pom nil))
          (remote '((title . "No Body") (updated_at . "2026-02-01T10:00:00Z")))
          (org-canvas--current-pull-item-fn nil))
      (let ((buf (org-canvas--conflict-format-diff data remote)))
        (unwind-protect
            (with-current-buffer buf
              (expect (buffer-string) :to-match "Conflict: No Body"))
          (when (buffer-live-p buf) (kill-buffer buf))))))

  (it "shows pull option only when pull-item-fn is set"
    (let ((data (list :title "Item" :pom nil))
          (remote '((title . "Item") (updated_at . "2026-02-01T10:00:00Z"))))
      ;; With pull-item-fn
      (let ((org-canvas--current-pull-item-fn #'ignore))
        (let ((buf (org-canvas--conflict-format-diff data remote)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :to-match "l = Pull"))
            (when (buffer-live-p buf) (kill-buffer buf)))))
      ;; Without pull-item-fn
      (let ((org-canvas--current-pull-item-fn nil))
        (let ((buf (org-canvas--conflict-format-diff data remote)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :not :to-match "l = Pull"))
            (when (buffer-live-p buf) (kill-buffer buf)))))))

  (it "shows P/L/S when pull-item-fn is set, P/S when nil"
    (let ((data (list :title "Item" :pom nil))
          (remote '((title . "Item") (updated_at . "2026-02-01T10:00:00Z"))))
      ;; With pull-item-fn: should show P/L/S
      (let ((org-canvas--current-pull-item-fn #'ignore))
        (let ((buf (org-canvas--conflict-format-diff data remote)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :to-match "P/L/S"))
            (when (buffer-live-p buf) (kill-buffer buf)))))
      ;; Without pull-item-fn: should show P/S
      (let ((org-canvas--current-pull-item-fn nil))
        (let ((buf (org-canvas--conflict-format-diff data remote)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :to-match "P/S")
                (expect (buffer-string) :not :to-match "P/L/S"))
            (when (buffer-live-p buf) (kill-buffer buf))))))))

(describe "org-canvas--conflict-prompt"
  (it "returns push for p"
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?p)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'push)))

  (it "returns pull for l"
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?l)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'pull)))

  (it "returns skip for s"
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?s)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'skip)))

  (it "returns push-all for P"
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?P)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'push-all)))

  (it "returns pull-all for L"
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?L)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'pull-all)))

  (it "returns skip-all for S"
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?S)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'skip-all))))

(describe "org-canvas--resolve-conflict"
  (it "returns apply-all value immediately when set"
    (let ((org-canvas--conflict-apply-all 'push)
          (org-canvas--current-pull-item-fn nil))
      (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")))
              :to-equal 'push)))

  (it "returns skip when apply-all is skip"
    (let ((org-canvas--conflict-apply-all 'skip)
          (org-canvas--current-pull-item-fn nil))
      (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")))
              :to-equal 'skip)))

  (it "sets apply-all on push-all choice"
    (let ((org-canvas--conflict-apply-all nil)
          (org-canvas--current-pull-item-fn nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'push-all)))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")))
                :to-equal 'push)
        (expect org-canvas--conflict-apply-all :to-equal 'push))))

  (it "sets apply-all on skip-all choice"
    (let ((org-canvas--conflict-apply-all nil)
          (org-canvas--current-pull-item-fn nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'skip-all)))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")))
                :to-equal 'skip)
        (expect org-canvas--conflict-apply-all :to-equal 'skip))))

  (it "sets apply-all on pull-all choice"
    (let ((org-canvas--conflict-apply-all nil)
          (org-canvas--current-pull-item-fn #'ignore))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'pull-all)))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")))
                :to-equal 'pull)
        (expect org-canvas--conflict-apply-all :to-equal 'pull))))

  (it "kills the diff buffer after prompting"
    (let ((org-canvas--conflict-apply-all nil)
          (org-canvas--current-pull-item-fn nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'push)))
        (org-canvas--resolve-conflict '(:title "X") '((title . "X")))
        (expect (get-buffer org-canvas--conflict-buffer-name) :to-be nil)))))

(describe "org-canvas--conflict-pull-local"
  (it "calls pull-item-fn and refreshes file-level LAST_SYNCED header"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Old Title
:PROPERTIES:
:CANVAS_ID: 100
:PAYLOAD_HASH: abc123
:END:
"
     (re-search-forward "^\\* ")
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Old Title" :pom pom))
            (remote '((title . "New Title")
                      (updated_at . "2026-02-01T12:00:00Z")
                      (message . "new body")))
            (pull-called nil))
       (org-canvas--conflict-pull-local data remote
         (lambda (_item _pos) (setq pull-called t)))
       (expect pull-called :to-be-truthy)
       ;; PAYLOAD_HASH should be deleted
       (expect (org-entry-get pom "PAYLOAD_HASH") :to-be nil)
       ;; CANVAS_UPDATED_AT should be set on the heading
       (expect (org-entry-get pom "CANVAS_UPDATED_AT")
               :to-equal "2026-02-01T12:00:00Z")
       ;; Per-entry LAST_SYNCED should not be written
       (expect (org-entry-get pom "LAST_SYNCED") :to-be nil)
       ;; File-level header should have been refreshed
       (let ((new-synced (org-canvas--pull-read-file-header)))
         (expect new-synced :to-be-truthy)
         (expect new-synced :not :to-equal "[2026-01-01 Thu 10:00]")))))

  (it "updates heading title when different"
    (with-temp-org-buffer
     "* Original Name
:PROPERTIES:
:CANVAS_ID: 200
:END:
"
     (org-back-to-heading)
     (let* ((pom (let ((m (point-marker)))
                   (set-marker-insertion-type m t)
                   m))
            (data (list :title "Original Name" :pom pom))
            (remote '((title . "Updated Name")
                      (updated_at . "2026-02-01T12:00:00Z"))))
       (org-canvas--conflict-pull-local data remote
         (lambda (_item _pos) nil))
       ;; pull-write-file-header may have inserted text at the top —
       ;; navigate by structure rather than by stale position
       (goto-char (point-min))
       (re-search-forward "^\\* " nil t)
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "Updated Name"))))

  (it "handles integer pom (non-marker)"
    (with-temp-org-buffer
     "* Integer POM Test
:PROPERTIES:
:CANVAS_ID: 300
:PAYLOAD_HASH: oldhash
:END:
"
     (org-back-to-heading)
     (let* ((pom (point))
            (data (list :title "Integer POM Test" :pom pom))
            (remote '((title . "Renamed via Integer")
                      (updated_at . "2026-03-01T09:00:00Z")))
            (pull-called nil))
       (org-canvas--conflict-pull-local data remote
         (lambda (_item _pos) (setq pull-called t)))
       (expect pull-called :to-be-truthy)
       ;; Re-locate the heading because pull-write-file-header may have
       ;; shifted positions when inserting the file-level header
       (goto-char (point-min))
       (re-search-forward "^\\* " nil t)
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "Renamed via Integer")
       (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-03-01T09:00:00Z")
       ;; File-level header gets refreshed
       (expect (org-canvas--pull-read-file-header) :to-be-truthy)))))

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
                    (lambda (_data _remote) 'push)))
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
             (org-canvas--current-pull-item-fn (lambda (_item _pos) nil)))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (when (eq method 'GET)
                        '((id . 111) (updated_at . "2026-02-01T10:00:00Z")))))
                   ((symbol-function 'org-canvas--resolve-conflict)
                    (lambda (_data _remote) 'pull)))
           (let ((data (list :title "Pull Item" :canvas-id "111"
                             :pom (point-marker)))
                 (payload '((title . "Pull Item"))))
             (expect (org-canvas--push-to-api data payload :endpoint "items")
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
       (let ((org-canvas-detect-conflicts t)
             (org-canvas--current-pull-item-fn nil))
         (cl-letf (((symbol-function 'org-canvas-api-request)
                    (lambda (method _url &rest _args)
                      (when (eq method 'GET)
                        '((id . 222) (updated_at . "2026-02-01T10:00:00Z")))))
                   ((symbol-function 'org-canvas--resolve-conflict)
                    (lambda (_data _remote) 'pull)))
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
                       :push-fn (lambda (_data _payload) 'pulled)
                       :finalize-fn (lambda (_data _response) nil)
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

;;;; Pull Helpers

(describe "org-canvas--html-to-org"
  (it "converts simple HTML to Org"
    (let ((result (org-canvas--html-to-org "<p>Hello <strong>world</strong></p>")))
      (expect result :to-match "Hello")
      (expect result :to-match "world")))

  (it "returns raw HTML with warning if pandoc is absent"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (let ((result (org-canvas--html-to-org "<p>Test</p>")))
        (expect result :to-match "WARNING")
        (expect result :to-match "<p>Test</p>")))))

(describe "org-canvas--pull-insert-body"
  (it "inserts converted HTML as Org text"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 1
:END:
Old body text
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (org-canvas--pull-insert-body "<p>New content</p>")
     (goto-char (point-min))
     (expect (buffer-string) :to-match "New content")
     (expect (buffer-string) :not :to-match "Old body text")))

  (it "does nothing when body is nil"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 1
:END:
Keep this
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (org-canvas--pull-insert-body nil)
     (expect (buffer-string) :to-match "Keep this")))

  (it "does nothing when body is empty string"
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:CANVAS_ID: 1
:END:
Keep this too
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (org-canvas--pull-insert-body "")
     (expect (buffer-string) :to-match "Keep this too"))))

(describe "org-canvas--pull-upsert-heading"
  (it "creates new heading when no match exists"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert ""))
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file 999 "New Item")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                (expect (org-get-heading t t t t) :to-equal "New Item"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "finds existing heading by CANVAS_ID"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Existing\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file 42 "Updated")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                ;; Should find existing, not create new
                (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "uses custom id-property"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Page\n:PROPERTIES:\n:CANVAS_URL: my-page\n:END:\n"))
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file "my-page" "Updated Page" "CANVAS_URL")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                (expect (org-entry-get (point) "CANVAS_URL")
                        :to-equal "my-page"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--iso8601-to-org-timestamp"
  (it "converts ISO8601 to active timestamp"
    (let ((result (org-canvas--iso8601-to-org-timestamp "2026-01-15T10:00:00Z")))
      (expect result :to-match "<2026-01-15")))

  (it "returns nil for nil active-timestamp input"
    (expect (org-canvas--iso8601-to-org-timestamp nil) :to-be nil))

  (it "returns nil for :null"
    (expect (org-canvas--iso8601-to-org-timestamp :null) :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--iso8601-to-org-timestamp "") :to-be nil)))

(describe "org-canvas--iso8601-to-org-inactive-timestamp"
  (it "converts ISO8601 to inactive timestamp"
    (let ((result (org-canvas--iso8601-to-org-inactive-timestamp
                   "2026-01-15T10:00:00Z")))
      (expect result :to-match "\\[2026-01-15")))

  (it "returns nil for nil inactive-timestamp input"
    (expect (org-canvas--iso8601-to-org-inactive-timestamp nil) :to-be nil)))

(describe "org-canvas-define-pull"
  (it "signals error when :file is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :endpoint "test"
                            :pull-item-fn #'ignore))
            :to-throw 'error))

  (it "signals error when :endpoint is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :file test-file
                            :pull-item-fn #'ignore))
            :to-throw 'error))

  (it "signals error when :pull-item-fn is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :file test-file
                            :endpoint "test"))
            :to-throw 'error))

  (it "generates a pull function with correct name"
    (let ((expansion (macroexpand '(org-canvas-define-pull test-widgets
                                    :file test-file
                                    :endpoint "widgets"
                                    :pull-item-fn #'ignore))))
      (expect expansion :to-be-truthy)
      ;; Check that the expansion contains a defun with the right name
      (expect (format "%S" expansion) :to-match "org-canvas-pull-test-widgets")))

  (it "aborts when user declines overwrite of existing file"
    (let* ((temp-dir (make-temp-file "pull-confirm-test" t))
           (test-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Existing Group\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (let ((org-canvas-assignment-groups-file test-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                          ((symbol-function 'display-buffer) (lambda (_) nil))
                          ((symbol-function 'y-or-n-p) (lambda (_) nil)))
                  (expect (org-canvas-pull-assignment-groups)
                          :to-throw 'user-error)))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "proceeds without prompting when file does not exist"
    (let* ((temp-dir (make-temp-file "pull-confirm-test" t))
           (test-file (expand-file-name "assignment-groups.org" temp-dir))
           (prompted nil))
      (unwind-protect
          (let ((org-canvas-assignment-groups-file test-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params) '()))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil))
                        ((symbol-function 'y-or-n-p)
                         (lambda (_) (setq prompted t) t)))
                (org-canvas-pull-assignment-groups)
                (expect prompted :to-be nil))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--pull-upsert-heading at EOF"
  (it "creates heading when file has no newline at end"
    (let* ((temp-dir (make-temp-file "upsert-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file (insert "* Existing"))  ;; no trailing newline
            (let ((pos (org-canvas--pull-upsert-heading
                        test-file 999 "New Item")))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char pos)
                (expect (org-get-heading t t t t) :to-equal "New Item"))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

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
                                      :push-fn (lambda (_data _payload)
                                                 (setq api-called t)
                                                 '((id . 1)))
                                      :finalize-fn (lambda (_data _response) nil)
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
  (it "binds org-canvas--current-pull-item-fn from pull-item-fn arg"
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
             (org-canvas--push-at-point-runtime
              "test"
              (lambda () (list :title "Test" :canvas-id "99" :pom (point)))
              (lambda (_data) '((title . "Test")))
              (lambda (_data _payload)
                (setq captured-pull-fn org-canvas--current-pull-item-fn)
                '((id . 99)))
              (lambda (_data _response) nil)
              :title
              #'my-pull-fn))
           (expect captured-pull-fn :to-equal #'my-pull-fn)))))))

;;;; Compile-time form builders

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
      (expect (format "%S" form) :to-match "my-post"))))

;;;; Runtime sync pipeline

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
                  (org-canvas--sync-run-pipeline
                   "test" org-file "LEVEL=1"
                   #'org-canvas--announcement-parse-entry
                   #'org-canvas--announcement-build-payload
                   (lambda (data payload)
                     (org-canvas--push-to-api data payload :endpoint "test"))
                   (lambda (data response)
                     (org-canvas--finalize-item data response))
                   nil nil)
                  (expect parse-count :to-be-truthy)))))
        (delete-directory temp-dir t))))

  (it "binds conflict-apply-all to nil"
    (let ((org-canvas--conflict-apply-all 'push)
          (captured-val 'not-set))
      (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                ((symbol-function 'org-canvas--sync-validate-file)
                 (lambda (_upper _file)
                   (setq captured-val org-canvas--conflict-apply-all)))
                ((symbol-function 'org-canvas--sync-collect-entries)
                 (lambda (&rest _) (list :targets nil :all-ids-before nil)))
                ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
        (org-canvas--sync-run-pipeline "test" "/tmp/test.org" "LEVEL=1"
                                       #'ignore #'ignore #'ignore #'ignore)
        (expect captured-val :to-be nil))))

  (it "binds current-pull-item-fn from argument"
    (let ((captured-fn nil))
      (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                ((symbol-function 'org-canvas--sync-validate-file)
                 (lambda (&rest _)
                   (setq captured-fn org-canvas--current-pull-item-fn)))
                ((symbol-function 'org-canvas--sync-collect-entries)
                 (lambda (&rest _) (list :targets nil :all-ids-before nil)))
                ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
        (org-canvas--sync-run-pipeline "test" "/tmp/test.org" "LEVEL=1"
                                       #'ignore #'ignore #'ignore #'ignore
                                       #'my-pull-fn)
        (expect captured-fn :to-equal #'my-pull-fn)))))

;;;; Pull Helper Tests

(describe "org-canvas--pull-set-boolean-property"
  (it "sets true when value is t"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--pull-set-boolean-property (point) "ALLOW_RATING" t)
     (expect (org-entry-get (point) "ALLOW_RATING") :to-equal "true")))

  (it "sets false when value is nil and org-canvas-emit-defaults is t"
    (let ((org-canvas-emit-defaults t))
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (org-canvas--pull-set-boolean-property (point) "ALLOW_RATING" nil)
       (expect (org-entry-get (point) "ALLOW_RATING") :to-equal "false"))))

  (it "sets false when value is :json-false and org-canvas-emit-defaults is t"
    (let ((org-canvas-emit-defaults t))
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:END:
"
       (org-back-to-heading)
       (org-canvas--pull-set-boolean-property (point) "PINNED" :json-false)
       (expect (org-entry-get (point) "PINNED") :to-equal "false")))))

(describe "org-canvas--pull-set-timestamp-property"
  (it "sets Org timestamp from ISO8601"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--pull-set-timestamp-property (point) "START_AT" "2026-09-01T14:00:00Z")
     (expect (org-entry-get (point) "START_AT") :to-match "<2026-09-01")))

  (it "does nothing when iso8601 is nil"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--pull-set-timestamp-property (point) "START_AT" nil)
     (expect (org-entry-get (point) "START_AT") :to-be nil)))

  (it "does nothing when iso8601 is empty string"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--pull-set-timestamp-property (point) "START_AT" "")
     (expect (org-entry-get (point) "START_AT") :to-be nil))))

(describe "org-canvas--pull-process-item"
  (it "upserts heading and saves sync state"
    (let* ((temp-dir (make-temp-file "pull-proc" t))
           (org-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file org-file (insert "#+TITLE: Test\n"))
            (let ((item '((id . 42) (title . "My Item")))
                  (item-fn-called nil))
              (with-current-buffer (find-file-noselect org-file)
                (org-canvas--pull-process-item
                 item org-file
                 (list :id-field 'id :title-field 'title
                       :id-property "CANVAS_ID"
                       :pull-item-fn (lambda (_item _pos) (setq item-fn-called t))))
                (goto-char (point-min))
                (re-search-forward "^\\* " nil t)
                (org-back-to-heading)
                (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42")
                ;; Per-entry LAST_SYNCED is no longer written
                (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))
              (expect item-fn-called :to-be t)))
        (let ((buf (find-buffer-visiting org-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "updates existing heading title"
    (let* ((temp-dir (make-temp-file "pull-proc2" t))
           (org-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file org-file
              (insert "#+TITLE: Test\n* Old Title\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((item '((id . 42) (title . "New Title"))))
              (with-current-buffer (find-file-noselect org-file)
                (org-canvas--pull-process-item
                 item org-file
                 (list :id-field 'id :title-field 'title
                       :id-property "CANVAS_ID"
                       :pull-item-fn (lambda (_item _pos) nil)))
                (goto-char (point-min))
                (re-search-forward "^\\* " nil t)
                (expect (org-get-heading t t t t) :to-equal "New Title"))))
        (let ((buf (find-buffer-visiting org-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Upload File Tests

(describe "org-canvas--upload-file"
  (it "performs 3-step upload and returns file alist"
    (let* ((temp-file (make-temp-file "upload-test" nil ".png"))
           (step1-called nil)
           (step2-called nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PNGDATA"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (method _url &rest _args)
                           (cond
                            ((eq method 'POST)
                             (setq step1-called t)
                             '((upload_url . "https://upload.example.com/upload")
                               (upload_params . ((key . "val")))))
                            (t '((id . 777) (display_name . "test.png"))))))
                        ((symbol-function 'url-retrieve-synchronously)
                         (lambda (_url &rest _args)
                           (setq step2-called t)
                           (let ((buf (generate-new-buffer " *upload-test*")))
                             (with-current-buffer buf
                               (insert "HTTP/1.1 200 OK\r\n\r\n")
                               (insert (json-encode '((id . 777)
                                                      (display_name . "test.png")))))
                             buf))))
                (let ((result (org-canvas--upload-file temp-file)))
                  (expect step1-called :to-be t)
                  (expect step2-called :to-be t)
                  (expect (alist-get 'id result) :to-equal 777)))))
        (delete-file temp-file))))

  (it "follows Location header when no JSON id in step 2"
    (let* ((temp-file (make-temp-file "upload-loc" nil ".jpg")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "JPGDATA"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (method _url &rest _args)
                           (cond
                            ((eq method 'POST)
                             '((upload_url . "https://upload.example.com/x")
                               (upload_params . nil)))
                            ((eq method 'GET)
                             '((id . 888) (display_name . "photo.jpg"))))))
                        ((symbol-function 'url-retrieve-synchronously)
                         (lambda (_url &rest _args)
                           (let ((buf (generate-new-buffer " *upload-loc*")))
                             (with-current-buffer buf
                               (insert "HTTP/1.1 301 Redirect\r\n")
                               (insert "Location: https://canvas.test/files/888/confirm\r\n")
                               (insert "\r\n{\"status\":\"pending\"}"))
                             buf))))
                (let ((result (org-canvas--upload-file temp-file)))
                  (expect (alist-get 'id result) :to-equal 888)))))
        (delete-file temp-file))))

  (it "uses custom notify-url and display-name"
    (let* ((temp-file (make-temp-file "upload-custom" nil ".gif"))
           (notify-url-used nil)
           (name-used nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "GIFDATA"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method url &rest args)
                           (setq notify-url-used url)
                           (let ((payload (plist-get args :data)))
                             (setq name-used (alist-get 'name payload)))
                           '((upload_url . "https://up.test/x")
                             (upload_params . nil))))
                        ((symbol-function 'url-retrieve-synchronously)
                         (lambda (_url &rest _args)
                           (let ((buf (generate-new-buffer " *upload-custom*")))
                             (with-current-buffer buf
                               (insert "HTTP/1.1 200 OK\r\n\r\n")
                               (insert (json-encode '((id . 999)))))
                             buf))))
                (org-canvas--upload-file temp-file
                                         "https://custom.api/files"
                                         "renamed.gif")
                (expect notify-url-used :to-equal "https://custom.api/files")
                (expect name-used :to-equal "renamed.gif"))))
        (delete-file temp-file)))))

;;;; conflict-format-diff with LAST_SYNCED and name-based remote

(describe "org-canvas--conflict-format-diff"
  (it "reads LAST_SYNCED from file-level header and uses name field for title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-15 Thu 14:30]
* My Item
:PROPERTIES:
:CANVAS_ID: 123
:END:

Local body content.
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let ((org-canvas--current-pull-item-fn nil)
             (data (list :title "My Item"
                         :description "Local body content."
                         :pom (point-marker)))
             (remote '((name . "Remote Item Name")
                       (updated_at . "2026-02-01T10:00:00Z")
                       (description . "Remote body."))))
         (let ((buf (org-canvas--conflict-format-diff data remote)))
           (unwind-protect
               (with-current-buffer buf
                 (let ((content (buffer-string)))
                   ;; Should contain the remote title from 'name field
                   (expect content :to-match "Remote Item Name")
                   ;; Should contain timestamps
                   (expect content :to-match "2026-01-15")
                   (expect content :to-match "2026-02-01")))
             (when (buffer-live-p buf)
               (kill-buffer buf)))))))))

;;;; conflict-pull-local overwrites heading and calls pull-item-fn

(describe "org-canvas--conflict-pull-local"
  (it "renames heading and invokes pull-item-fn"
    (with-temp-org-buffer
     "* Old Title
:PROPERTIES:
:CANVAS_ID: 456
:END:

Old body.
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (pull-called nil)
            (data (list :title "Old Title" :pom pom))
            (remote '((title . "New Remote Title")
                      (updated_at . "2026-02-10T08:00:00Z")
                      (body . "New body."))))
       (org-canvas--conflict-pull-local
        data remote
        (lambda (_response _pos)
          (setq pull-called t)))
       ;; Re-navigate by structure: pull-write-file-header may have
       ;; inserted text at the top, shifting positions
       (goto-char (point-min))
       (re-search-forward "^\\* " nil t)
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "New Remote Title")
       ;; pull-item-fn should have been called
       (expect pull-called :to-be-truthy)
       ;; Per-entry LAST_SYNCED should not be written
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
       ;; File-level header should have been written
       (expect (org-canvas--pull-read-file-header) :to-match "^\\[20")
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-02-10T08:00:00Z")
       ;; PAYLOAD_HASH should be deleted
       (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)))))

;;;; demo-conflict via mocked prompt

(describe "org-canvas-demo-conflict"
  (it "runs the demo and returns a resolution choice"
    (let ((demo-message nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'push))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq demo-message (apply #'format fmt args)))))
        (org-canvas-demo-conflict)
        (expect demo-message :to-match "push")))))

;;;; push-at-point payload hash skip

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
           (org-canvas--push-at-point-runtime
            "page"
            (lambda () (list :title "Test Page" :canvas-id "100" :pom (point)))
            (lambda (_data) payload)
            (lambda (_data _payload) (setq api-called t) '((id . 100)))
            (lambda (_data _response) nil)
            :title
            nil))
         ;; API should NOT have been called (skipped)
         (expect api-called :to-be nil))))))

;;;; push-at-point-runtime canvas-url fallback

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
           (org-canvas--push-at-point-runtime
            "page"
            (lambda () (list :title "Test Page" :canvas-url "my-page-slug" :pom (point)))
            (lambda (_data) payload)
            (lambda (_data _payload) (setq api-called t) '((url . "my-page-slug")))
            (lambda (_data _response) nil)
            :title
            nil))
         ;; Should be skipped because hash matches AND canvas-url is truthy
         (expect api-called :to-be nil))))))

;;;; upload-file step-2 fallback paths

(describe "org-canvas--upload-file step-2 fallbacks"
  (it "returns JSON when step-2 has JSON without id but no Location"
    (let* ((temp-file (make-temp-file "upload-json-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "filedata"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (method url &rest _args)
                           (cond
                            ;; Step 1: notify
                            ((eq method 'POST)
                             '((upload_url . "https://up.test/x")
                               (upload_params . nil)))
                            ;; Step 3: confirm via GET on location
                            ((eq method 'GET)
                             '((id . 777))))))
                        ((symbol-function 'url-retrieve-synchronously)
                         (lambda (_url &rest _args)
                           (let ((buf (generate-new-buffer " *upload-json*")))
                             (with-current-buffer buf
                               ;; JSON response without 'id, no Location header
                               (insert "HTTP/1.1 200 OK\r\n\r\n")
                               (insert (json-encode '((status . "pending")
                                                      (location . "/files/777/confirm")))))
                             buf))))
                (let ((result (org-canvas--upload-file temp-file)))
                  ;; Should follow the location from the JSON body
                  (expect (alist-get 'id result) :to-equal 777)))))
        (delete-file temp-file))))

  (it "errors when step-2 has no JSON and no Location header"
    (let* ((temp-file (make-temp-file "upload-empty-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "filedata"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           '((upload_url . "https://up.test/x")
                             (upload_params . nil))))
                        ((symbol-function 'url-retrieve-synchronously)
                         (lambda (_url &rest _args)
                           (let ((buf (generate-new-buffer " *upload-empty*")))
                             (with-current-buffer buf
                               ;; No valid JSON, no Location
                               (insert "HTTP/1.1 200 OK\r\n\r\n")
                               (insert "not json"))
                             buf))))
                (expect (org-canvas--upload-file temp-file)
                        :to-throw 'error))))
        (delete-file temp-file))))

  (it "prepends base-url to relative location"
    (let* ((temp-file (make-temp-file "upload-rel-" nil ".txt"))
           (get-url nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "filedata"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (method url &rest _args)
                           (cond
                            ((eq method 'POST)
                             '((upload_url . "https://up.test/x")
                               (upload_params . nil)))
                            ((eq method 'GET)
                             (setq get-url url)
                             '((id . 555))))))
                        ((symbol-function 'url-retrieve-synchronously)
                         (lambda (_url &rest _args)
                           (let ((buf (generate-new-buffer " *upload-rel*")))
                             (with-current-buffer buf
                               (insert "HTTP/1.1 301 Redirect\r\n")
                               (insert "Location: /api/v1/files/555/confirm\r\n")
                               (insert "\r\n"))
                             buf))))
                (let ((result (org-canvas--upload-file temp-file)))
                  (expect (alist-get 'id result) :to-equal 555)
                  ;; Should have prepended base-url
                  (expect get-url :to-match "^https://test.canvas.example.com/api/v1/files/555/confirm")))))
        (delete-file temp-file)))))

;;;; Shared Rubric Association

(describe "org-canvas--associate-rubric"
  (it "sends correct payload for Assignment type"
    (with-org-canvas-test-config
      (let (sent-method sent-url sent-data)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (setq sent-method method
                           sent-url url
                           sent-data (plist-get args :data))
                     '((id . 1)))))
          (org-canvas--associate-rubric 42 "99" "Assignment")
          (expect sent-method :to-equal 'POST)
          (expect sent-url :to-match "rubric_associations")
          (let ((assoc (gethash "rubric_association" sent-data)))
            (expect (gethash "rubric_id" assoc) :to-equal 99)
            (expect (gethash "association_id" assoc) :to-equal 42)
            (expect (gethash "association_type" assoc) :to-equal "Assignment")
            (expect (gethash "purpose" assoc) :to-equal "grading"))))))

  (it "sends correct payload for Discussion type"
    (with-org-canvas-test-config
      (let (sent-data)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq sent-data (plist-get args :data))
                     '((id . 1)))))
          (org-canvas--associate-rubric 55 "77" "Discussion")
          (let ((assoc (gethash "rubric_association" sent-data)))
            (expect (gethash "association_type" assoc) :to-equal "Discussion")
            (expect (gethash "association_id" assoc) :to-equal 55)
            (expect (gethash "rubric_id" assoc) :to-equal 77))))))

  (it "handles API errors gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (error "API failure"))))
        ;; Should not signal an error
        (expect (org-canvas--associate-rubric 1 "2" "Assignment")
                :not :to-throw)))))

;;; org-canvas-core-test.el ends here

;;;; Rate-limit retry through push pipeline

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

;;;; Conflict batch flow integration

(describe "conflict batch flow"
  (it "auto-pushes second conflict after user chooses Push All on first"
    (with-org-canvas-test-config
      (let ((org-canvas-detect-conflicts t)
            (org-canvas--conflict-apply-all nil)
            (org-canvas--current-pull-item-fn nil)
            (prompt-count 0)
            (put-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (pcase method
                       ;; GET always returns a "newer" item (conflict)
                       ('GET '((id . 1) (updated_at . "2026-03-01T10:00:00Z")))
                       ('PUT (setq put-count (1+ put-count))
                             '((id . 1))))))
                  ((symbol-function 'org-canvas--conflict-prompt)
                   (lambda (_has-pull)
                     (setq prompt-count (1+ prompt-count))
                     'push-all)))
          ;; First push: conflict detected, user prompted, chooses "Push All"
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item 1\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data1 (list :title "Item 1" :canvas-id "1" :pom (point-marker)))
                 (payload1 '((title . "Item 1"))))
             (org-canvas--push-to-api data1 payload1 :endpoint "items")
             ;; User was prompted once
             (expect prompt-count :to-equal 1)
             ;; PUT was called (force push)
             (expect put-count :to-equal 1)))
          ;; Second push: conflict detected, but apply-all is already 'push
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item 2\n:PROPERTIES:\n:CANVAS_ID: 2\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data2 (list :title "Item 2" :canvas-id "2" :pom (point-marker)))
                 (payload2 '((title . "Item 2"))))
             (org-canvas--push-to-api data2 payload2 :endpoint "items")
             ;; No additional prompt (apply-all active)
             (expect prompt-count :to-equal 1)
             ;; PUT was called again
             (expect put-count :to-equal 2)))))))

  (it "auto-skips second conflict after user chooses Skip All on first"
    (with-org-canvas-test-config
      (let ((org-canvas-detect-conflicts t)
            (org-canvas--conflict-apply-all nil)
            (org-canvas--current-pull-item-fn nil)
            (prompt-count 0)
            (put-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (pcase method
                       ('GET '((id . 1) (updated_at . "2026-03-01T10:00:00Z")))
                       ('PUT (setq put-count (1+ put-count))
                             '((id . 1))))))
                  ((symbol-function 'org-canvas--conflict-prompt)
                   (lambda (_has-pull)
                     (setq prompt-count (1+ prompt-count))
                     'skip-all)))
          ;; First push: conflict, user chooses "Skip All"
          ;; push-to-api returns 'conflict for both skip and skip-all
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item A\n:PROPERTIES:\n:CANVAS_ID: 10\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data (list :title "Item A" :canvas-id "10" :pom (point-marker)))
                 (payload '((title . "Item A"))))
             (let ((result (org-canvas--push-to-api data payload :endpoint "items")))
               (expect result :to-equal 'conflict))))
          ;; Second push: auto-skipped without prompt
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item B\n:PROPERTIES:\n:CANVAS_ID: 20\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data (list :title "Item B" :canvas-id "20" :pom (point-marker)))
                 (payload '((title . "Item B"))))
             (let ((result (org-canvas--push-to-api data payload :endpoint "items")))
               (expect result :to-equal 'conflict))))
          ;; Prompt was only shown once
          (expect prompt-count :to-equal 1)
          ;; No PUTs were sent
          (expect put-count :to-equal 0))))))

;;;; Macro helper coverage

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

;;;; Global counter accumulation

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

;;;; Duplicate CANVAS_ID warning

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

;;;; Heading title in error messages

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

;;;; Sync-at-point stage logging

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
       (org-canvas--push-at-point-runtime
        "pages"
        (lambda () (list :title "Test Page" :canvas-id nil :pom (point-marker)))
        (lambda (_) '((title . "Test Page")))
        (lambda (_data _payload) '((url . "test-page")))
        (lambda (_data _response) nil)
        :title nil))
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
         (org-canvas--push-at-point-runtime
          "pages"
          (lambda () data)
          (lambda (_) payload)
          (lambda (_data _payload) (error "Should not be called"))
          (lambda (_data _response) (error "Should not be called"))
          :title nil))
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

;;;; Duplicate heading title warnings

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

;;;; Dry-run counter

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

(describe "org-canvas--resolve-conflict unexpected choice"
  (it "returns skip for unexpected choice symbol"
    (spy-on 'org-canvas--log-warning)
    (let ((org-canvas--conflict-apply-all nil)
          (org-canvas--current-pull-item-fn nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-format-diff)
                 (lambda (_data _remote) (get-buffer-create "*test-diff*")))
                ((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'unexpected-value)))
        (let ((result (org-canvas--resolve-conflict '(:title "Test") '((title . "Test")))))
          (expect result :to-equal 'skip)
          (expect 'org-canvas--log-warning :to-have-been-called))))))

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

;;;; Pull Sort Helper

(describe "org-canvas--pull-sort-items"
  (it "sorts items by position ascending"
    (let ((items '(((id . 3) (position . 30) (title . "C"))
                   ((id . 1) (position . 10) (title . "A"))
                   ((id . 2) (position . 20) (title . "B")))))
      (let ((sorted (org-canvas--pull-sort-items items)))
        (expect (mapcar (lambda (x) (alist-get 'title x)) sorted)
                :to-equal '("A" "B" "C")))))

  (it "uses secondary key when positions tie or are absent"
    (let ((items '(((id . 1) (position . 1) (assignment_group_id . 200) (title . "B"))
                   ((id . 2) (position . 1) (assignment_group_id . 100) (title . "A")))))
      (let ((sorted (org-canvas--pull-sort-items items 'assignment_group_id)))
        (expect (mapcar (lambda (x) (alist-get 'title x)) sorted)
                :to-equal '("A" "B")))))

  (it "groups by secondary key first, then sorts by position within group"
    (let ((items '(((id . 1) (name . "B-2") (position . 2) (assignment_group_id . 100))
                   ((id . 2) (name . "A-3") (position . 3) (assignment_group_id . 50))
                   ((id . 3) (name . "B-1") (position . 1) (assignment_group_id . 100))
                   ((id . 4) (name . "A-1") (position . 1) (assignment_group_id . 50)))))
      (let ((sorted (org-canvas--pull-sort-items items 'assignment_group_id)))
        (expect (mapcar (lambda (x) (alist-get 'name x)) sorted)
                :to-equal '("A-1" "A-3" "B-1" "B-2")))))

  (it "falls back to name when position is missing"
    (let ((items '(((id . 1) (name . "Beta"))
                   ((id . 2) (name . "Alpha")))))
      (let ((sorted (org-canvas--pull-sort-items items)))
        (expect (mapcar (lambda (x) (alist-get 'name x)) sorted)
                :to-equal '("Alpha" "Beta")))))

  (it "falls back to title when position and name are missing"
    (let ((items '(((id . 1) (title . "Zebra"))
                   ((id . 2) (title . "Apple")))))
      (let ((sorted (org-canvas--pull-sort-items items)))
        (expect (mapcar (lambda (x) (alist-get 'title x)) sorted)
                :to-equal '("Apple" "Zebra")))))

  (it "falls back to id when position and name are absent"
    (let ((items '(((id . 5)) ((id . 3)) ((id . 1)))))
      (let ((sorted (org-canvas--pull-sort-items items)))
        (expect (mapcar (lambda (x) (alist-get 'id x)) sorted)
                :to-equal '(1 3 5)))))

  (it "is stable: items with same key preserve input order"
    (let ((items '(((id . 1) (position . 1) (title . "first"))
                   ((id . 2) (position . 1) (title . "second"))
                   ((id . 3) (position . 1) (title . "third")))))
      (let ((sorted (org-canvas--pull-sort-items items)))
        (expect (mapcar (lambda (x) (alist-get 'title x)) sorted)
                :to-equal '("first" "second" "third")))))

  (it "handles empty list"
    (expect (org-canvas--pull-sort-items '()) :to-equal nil))

  (it "handles single-item list"
    (let ((items '(((id . 1) (position . 5) (title . "Only")))))
      (expect (org-canvas--pull-sort-items items) :to-equal items)))

  (it "treats missing position as greater than any present position"
    ;; An item without `position' should sort after items with a position,
    ;; so the explicit ordering wins over the missing-data fallback.
    (let ((items '(((id . 1) (title . "no-pos"))
                   ((id . 2) (position . 5) (title . "has-pos")))))
      (let ((sorted (org-canvas--pull-sort-items items)))
        (expect (mapcar (lambda (x) (alist-get 'title x)) sorted)
                :to-equal '("has-pos" "no-pos")))))

  (it "uses tertiary key (string) between secondary and position"
    ;; Two items in the same assignment_group_id, different due_at.
    ;; With tertiary-key 'due_at, the earlier-due item sorts first
    ;; even when Canvas returns them in reverse position order.
    (let ((items '(((id . 1) (name . "Late") (position . 1)
                    (assignment_group_id . 100)
                    (due_at . "2026-03-27T23:59:00Z"))
                   ((id . 2) (name . "NN")   (position . 2)
                    (assignment_group_id . 100)
                    (due_at . "2026-03-06T23:59:00Z"))
                   ((id . 3) (name . "Early") (position . 3)
                    (assignment_group_id . 100)
                    (due_at . "2026-02-06T23:59:00Z")))))
      (let ((sorted (org-canvas--pull-sort-items
                     items 'assignment_group_id 'due_at)))
        (expect (mapcar (lambda (x) (alist-get 'name x)) sorted)
                :to-equal '("Early" "NN" "Late")))))

  (it "items missing tertiary key sort after those with one"
    (let ((items '(((id . 1) (name . "B") (position . 1)
                    (assignment_group_id . 100))
                   ((id . 2) (name . "A") (position . 2)
                    (assignment_group_id . 100)
                    (due_at . "2026-02-06T23:59:00Z")))))
      (let ((sorted (org-canvas--pull-sort-items
                     items 'assignment_group_id 'due_at)))
        (expect (mapcar (lambda (x) (alist-get 'name x)) sorted)
                :to-equal '("A" "B")))))

  (it "tertiary tier still respects secondary grouping"
    ;; Group A items (due 2026-04-01, 2026-03-01) and Group B items
    ;; (due 2026-02-01) — all of group A's items must precede group B's
    ;; even though group B has the earliest due date.
    (let ((items '(((id . 1) (name . "A-late") (assignment_group_id . 100)
                    (due_at . "2026-04-01T23:59:00Z"))
                   ((id . 2) (name . "B-early") (assignment_group_id . 200)
                    (due_at . "2026-02-01T23:59:00Z"))
                   ((id . 3) (name . "A-early") (assignment_group_id . 100)
                    (due_at . "2026-03-01T23:59:00Z")))))
      (let ((sorted (org-canvas--pull-sort-items
                     items 'assignment_group_id 'due_at)))
        (expect (mapcar (lambda (x) (alist-get 'name x)) sorted)
                :to-equal '("A-early" "A-late" "B-early"))))))

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
        (expect (plist-get entry :failed-titles) :to-equal '("B"))))))

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
      (expect logged :to-be nil))))

(provide 'org-canvas-core-sync-test)
;;; org-canvas-core-sync-test.el ends here
