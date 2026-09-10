;;; org-canvas-core-delete-test.el --- Tests for org-canvas-core-delete -*- lexical-binding: t; -*-

;;; Commentary:

;; Specs for `org-canvas-core-delete'.

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

(describe "org-canvas--prune-collect-local-ids"
  (it "collects ids from headings at all levels"
    (let ((temp-file (make-temp-file "prune-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* A\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** B\n:PROPERTIES:\n:CANVAS_ID: 2\n:END:\n* C\n"))
            (expect (org-canvas--prune-collect-local-ids temp-file "CANVAS_ID")
                    :to-equal '("1" "2")))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "signals user-error when the file is missing"
    (expect (org-canvas--prune-collect-local-ids "/nonexistent/x.org" "CANVAS_ID")
            :to-throw 'user-error)))

(describe "org-canvas--prune-runtime (mocked)"
  (it "deletes only remote items absent from the org file"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "prune-" nil ".org"))
            (pruned-items nil))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "* Kept\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (&rest _)
                           '(((id . 1) (title . "Kept"))
                             ((id . 2) (title . "Orphan A"))
                             ((id . 3) (title . "Orphan B")))))
                        ((symbol-function 'y-or-n-p) (lambda (_) t))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (items &rest _)
                           (setq pruned-items items)
                           (cons (length items) nil))))
                (expect (org-canvas--prune-runtime "pages"
                          :endpoint "pages" :file temp-file)
                        :to-equal 2)
                (expect (mapcar (lambda (i) (alist-get 'id i)) pruned-items)
                        :to-equal '(2 3))))
          (let ((buf (find-buffer-visiting temp-file)))
            (when buf (kill-buffer buf)))
          (delete-file temp-file)))))

  (it "respects skip-fn (protected items are not orphans)"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "prune-" nil ".org"))
        (pruned-items nil))
        (unwind-protect
            (progn
              (with-temp-file temp-file (insert "* Empty\n"))
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (&rest _)
                           '(((id . 1) (title . "Front") (front_page . t))
                             ((id . 2) (title . "Orphan")))))
                        ((symbol-function 'y-or-n-p) (lambda (_) t))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (items &rest _)
                           (setq pruned-items items)
                           (cons (length items) nil))))
                (org-canvas--prune-runtime "pages"
                  :endpoint "pages" :file temp-file
                  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
                (expect (length pruned-items) :to-equal 1)
                (expect (alist-get 'id (car pruned-items)) :to-equal 2)))
          (let ((buf (find-buffer-visiting temp-file)))
            (when buf (kill-buffer buf)))
          (delete-file temp-file)))))

  (it "counts protected items in the prune tally (issue #81)"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "prune-" nil ".org"))
            (logged nil))
        (unwind-protect
            (progn
              (with-temp-file temp-file (insert "* Empty\n"))
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (&rest _)
                           '(((id . 1) (title . "Front") (front_page . t))
                             ((id . 2) (title . "Orphan")))))
                        ((symbol-function 'y-or-n-p) (lambda (_) nil))
                        ((symbol-function 'org-canvas--log-info)
                         (lambda (_l fmt &rest args)
                           (push (apply #'format fmt args) logged))))
                (org-canvas--prune-runtime "pages"
                  :endpoint "pages" :file temp-file
                  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
                ;; Without this the front page is simply missing from the
                ;; tally, which reads as "there was nothing else there".
                (expect (car (last logged)) :to-match ", 1 protected")))
          (let ((buf (find-buffer-visiting temp-file)))
            (when buf (kill-buffer buf)))
          (delete-file temp-file)))))

  (it "omits the protected count when no skip-fn applies"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "prune-" nil ".org"))
            (logged nil))
        (unwind-protect
            (progn
              (with-temp-file temp-file (insert "* Empty\n"))
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (&rest _) '(((id . 2) (title . "Orphan")))))
                        ((symbol-function 'y-or-n-p) (lambda (_) nil))
                        ((symbol-function 'org-canvas--log-info)
                         (lambda (_l fmt &rest args)
                           (push (apply #'format fmt args) logged))))
                (org-canvas--prune-runtime "pages"
                  :endpoint "pages" :file temp-file)
                (expect (car (last logged)) :not :to-match "protected")))
          (let ((buf (find-buffer-visiting temp-file)))
            (when buf (kill-buffer buf)))
          (delete-file temp-file)))))

  (it "deletes nothing when the user declines"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "prune-" nil ".org"))
            (delete-called nil))
        (unwind-protect
            (progn
              (with-temp-file temp-file (insert "* Empty\n"))
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (&rest _) '(((id . 2) (title . "Orphan")))))
                        ((symbol-function 'y-or-n-p) (lambda (_) nil))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (&rest _) (setq delete-called t) (cons 0 nil))))
                (expect (org-canvas--prune-runtime "pages"
                          :endpoint "pages" :file temp-file)
                        :to-equal 0)
                (expect delete-called :to-be nil)))
          (let ((buf (find-buffer-visiting temp-file)))
            (when buf (kill-buffer buf)))
          (delete-file temp-file)))))

  (it "does not prompt when there are no orphans"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "prune-" nil ".org")))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "* Kept\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (&rest _) '(((id . 1) (title . "Kept")))))
                        ((symbol-function 'y-or-n-p)
                         (lambda (_) (error "Must not prompt"))))
                (expect (org-canvas--prune-runtime "pages"
                          :endpoint "pages" :file temp-file)
                        :to-equal 0)))
          (let ((buf (find-buffer-visiting temp-file)))
            (when buf (kill-buffer buf)))
          (delete-file temp-file)))))

  (it "generates prune commands for every delete-all feature"
    (expect (fboundp 'org-canvas-prune-pages) :to-be-truthy)
    (expect (fboundp 'org-canvas-prune-assignments) :to-be-truthy)
    (expect (fboundp 'org-canvas-prune-quizzes) :to-be-truthy)
    (expect (fboundp 'org-canvas-prune-modules) :to-be-truthy)
    (expect (fboundp 'org-canvas-prune-calendar-events) :to-be-truthy)
    (expect (fboundp 'org-canvas-prune-group-categories) :to-be-truthy)))

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

(describe "org-canvas--prune-runtime URL resolution"
  (it "lists through :list-url-fn and deletes through the default item URL"
    (with-org-canvas-test-config
      (let ((temp-file (make-temp-file "prune-" nil ".org"))
            (listed-url nil)
            (delete-url nil))
        (unwind-protect
            (progn
              (with-temp-file temp-file (insert "* Empty\n"))
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method url &rest _)
                           (setq listed-url url)
                           '(((id . 2) (title . "Orphan")))))
                        ((symbol-function 'y-or-n-p) (lambda (_) t))
                        ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                        ((symbol-function 'org-canvas--delete-items-queued)
                         (lambda (items del-fn &rest _)
                           (setq delete-url (funcall del-fn (alist-get 'id (car items))))
                           (cons (length items) nil))))
                (expect (org-canvas--prune-runtime "pages"
                          :endpoint "pages" :file temp-file
                          :list-url-fn (lambda () "https://canvas.test/api/v1/custom/pages"))
                        :to-equal 1))
              (expect listed-url :to-equal "https://canvas.test/api/v1/custom/pages")
              (expect delete-url :to-match "/pages/2\\'"))
          (let ((buf (find-buffer-visiting temp-file)))
            (when buf (kill-buffer buf)))
          (delete-file temp-file))))))

(provide 'org-canvas-core-delete-test)
;;; org-canvas-core-delete-test.el ends here
