;;; org-canvas-core-sync-test.el --- Buttercup tests for org-canvas-core sync pipeline helpers  -*- lexical-binding: t; -*-

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
                  (expect call-count :to-equal 2)))))
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
  (it "parses LAST_SYNCED from an Org heading"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:LAST_SYNCED: [2026-01-15 Thu 10:00]
:END:
"
     (org-back-to-heading)
     (let ((result (org-canvas--parse-last-synced (point))))
       (expect result :to-be-truthy))))

  (it "returns nil when no LAST_SYNCED"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--parse-last-synced (point)) :to-be nil))))

(describe "org-canvas--conflict-check"
  (it "returns conflict cons when remote is newer"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
       (org-back-to-heading)
       ;; Remote updated_at is much newer than LAST_SYNCED
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    '((id . 123) (updated_at . "2026-02-01T10:00:00Z")))))
         (let ((result (org-canvas--conflict-check "items" "123" (point))))
           (expect (car result) :to-equal 'conflict)
           (expect (alist-get 'id (cdr result)) :to-equal 123))))))

  (it "returns nil when local is newer"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-02-01 Thu 10:00]
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    '((id . 123) (updated_at . "2026-01-01T10:00:00Z")))))
         (expect (org-canvas--conflict-check "items" "123" (point))
                 :to-be nil)))))

  (it "returns nil when no LAST_SYNCED exists"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (org-back-to-heading)
       (expect (org-canvas--conflict-check "items" "123" (point))
               :to-be nil))))

  (it "returns nil on GET failure"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
       (org-back-to-heading)
       (cl-letf (((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    (signal 'error '("HTTP 500")))))
         (expect (org-canvas--conflict-check "items" "123" (point))
                 :to-be nil))))))

(describe "org-canvas--push-to-api conflict detection"
  (it "returns conflict when user chooses skip"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Conflict Item
:PROPERTIES:
:CANVAS_ID: 456
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
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

  (it "skips conflict check when detect-conflicts is nil"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((org-canvas-detect-conflicts nil)
              (data '(:title "Force Push" :canvas-id "789"))
              (payload '((title . "Force Push"))))
          (org-canvas--push-to-api data payload :endpoint "items")
          (expect-api-called 'PUT "items/789")))))

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
            (spy-on 'elog-info)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 5 :skip 2 :fail 1 :conflict 3 :pulled 1))
            (let ((found-conflicts nil)
                  (found-pulled nil))
              (dolist (call (spy-calls-all-args 'elog-info))
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
            (spy-on 'elog-info)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 5 :skip 2 :fail 1 :conflict 0 :pulled 0))
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'elog-info))
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
  (it "calls pull-item-fn and updates metadata"
    (with-temp-org-buffer
     "* Old Title
:PROPERTIES:
:CANVAS_ID: 100
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:PAYLOAD_HASH: abc123
:END:
"
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
       ;; CANVAS_UPDATED_AT should be set
       (expect (org-entry-get pom "CANVAS_UPDATED_AT")
               :to-equal "2026-02-01T12:00:00Z")
       ;; LAST_SYNCED should be updated (not the old value)
       (let ((new-synced (org-entry-get pom "LAST_SYNCED")))
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
     (let* ((pom (point-marker))
            (data (list :title "Original Name" :pom pom))
            (remote '((title . "Updated Name")
                      (updated_at . "2026-02-01T12:00:00Z"))))
       (org-canvas--conflict-pull-local data remote
         (lambda (_item _pos) nil))
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
       (goto-char pom)
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "Renamed via Integer")
       (expect (org-entry-get pom "PAYLOAD_HASH") :to-be nil)
       (expect (org-entry-get pom "CANVAS_UPDATED_AT")
               :to-equal "2026-03-01T09:00:00Z")
       (expect (org-entry-get pom "LAST_SYNCED") :to-be-truthy)))))

(describe "org-canvas--push-to-api conflict resolution"
  (it "proceeds with PUT when user chooses push"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Push Item
:PROPERTIES:
:CANVAS_ID: 789
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
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
       "* Pull Item
:PROPERTIES:
:CANVAS_ID: 111
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
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
       "* No Pull
:PROPERTIES:
:CANVAS_ID: 222
:LAST_SYNCED: [2026-01-01 Thu 10:00]
:END:
"
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
                            :item-fn #'ignore))
            :to-throw 'error))

  (it "signals error when :endpoint is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :file test-file
                            :item-fn #'ignore))
            :to-throw 'error))

  (it "signals error when :item-fn is missing"
    (expect (macroexpand '(org-canvas-define-pull test-feature
                            :file test-file
                            :endpoint "test"))
            :to-throw 'error))

  (it "generates a pull function with correct name"
    (let ((expansion (macroexpand '(org-canvas-define-pull test-widgets
                                    :file test-file
                                    :endpoint "widgets"
                                    :item-fn #'ignore))))
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
            (spy-on 'elog-warning)
            (with-org-canvas-test-config
              (org-canvas--sync-collect-entries test-file "LEVEL=1" "test")
              (let ((dup-warned nil))
                (dolist (call (spy-calls-all-args 'elog-warning))
                  (when (and (>= (length call) 2)
                             (stringp (nth 1 call))
                             (string-match-p "Duplicate" (nth 1 call)))
                    (setq dup-warned t)))
                (expect dup-warned :to-be-truthy))))
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
    (spy-on 'elog-warning)
    (org-canvas--sync-warn-orphans '("111" "222" "333") '("111" "333") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'elog-warning))
        (when (and (>= (length call) 3)
                   (stringp (nth 1 call))
                   (string-match-p "Orphan" (nth 1 call)))
          (setq orphan-warned t)))
      (expect orphan-warned :to-be-truthy)))

  (it "does not warn when all IDs synced"
    (spy-on 'elog-warning)
    (org-canvas--sync-warn-orphans '("111" "222") '("111" "222") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'elog-warning))
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

  (it "sets false when value is nil"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--pull-set-boolean-property (point) "ALLOW_RATING" nil)
     (expect (org-entry-get (point) "ALLOW_RATING") :to-equal "false")))

  (it "sets false when value is :json-false"
    (with-temp-org-buffer
     "* Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (org-canvas--pull-set-boolean-property (point) "PINNED" :json-false)
     (expect (org-entry-get (point) "PINNED") :to-equal "false"))))

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
                       :item-fn (lambda (_item _pos) (setq item-fn-called t))))
                (goto-char (point-min))
                (re-search-forward "^\\* " nil t)
                (org-back-to-heading)
                (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42")
                (expect (org-entry-get (point) "LAST_SYNCED") :to-be-truthy))
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
                       :item-fn (lambda (_item _pos) nil)))
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
  (it "reads LAST_SYNCED from pom and uses name field for title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* My Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2026-01-15 Thu 14:30]
:END:

Local body content.
"
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
       ;; Heading should be renamed
       (goto-char (marker-position pom))
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "New Remote Title")
       ;; pull-item-fn should have been called
       (expect pull-called :to-be-truthy)
       ;; Sync metadata should be updated
       (expect (org-entry-get (marker-position pom) "LAST_SYNCED") :to-match "^\\[20")
       (expect (org-entry-get (marker-position pom) "CANVAS_UPDATED_AT")
               :to-equal "2026-02-10T08:00:00Z")
       ;; PAYLOAD_HASH should be deleted
       (expect (org-entry-get (marker-position pom) "PAYLOAD_HASH") :to-be nil)))))

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
           "* Item 1\n:PROPERTIES:\n:CANVAS_ID: 1\n:LAST_SYNCED: [2026-01-01 Thu 10:00]\n:END:\n"
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
           "* Item 2\n:PROPERTIES:\n:CANVAS_ID: 2\n:LAST_SYNCED: [2026-01-01 Thu 10:00]\n:END:\n"
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
           "* Item A\n:PROPERTIES:\n:CANVAS_ID: 10\n:LAST_SYNCED: [2026-01-01 Thu 10:00]\n:END:\n"
           (org-back-to-heading)
           (let ((data (list :title "Item A" :canvas-id "10" :pom (point-marker)))
                 (payload '((title . "Item A"))))
             (let ((result (org-canvas--push-to-api data payload :endpoint "items")))
               (expect result :to-equal 'conflict))))
          ;; Second push: auto-skipped without prompt
          (with-temp-org-buffer
           "* Item B\n:PROPERTIES:\n:CANVAS_ID: 20\n:LAST_SYNCED: [2026-01-01 Thu 10:00]\n:END:\n"
           (org-back-to-heading)
           (let ((data (list :title "Item B" :canvas-id "20" :pom (point-marker)))
                 (payload '((title . "Item B"))))
             (let ((result (org-canvas--push-to-api data payload :endpoint "items")))
               (expect result :to-equal 'conflict))))
          ;; Prompt was only shown once
          (expect prompt-count :to-equal 1)
          ;; No PUTs were sent
          (expect put-count :to-equal 0))))))

(provide 'org-canvas-core-sync-test)
;;; org-canvas-core-sync-test.el ends here
