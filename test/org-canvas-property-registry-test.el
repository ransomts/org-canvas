;;; org-canvas-property-registry-test.el --- Tests for property registry  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the centralized property registry in org-canvas-core-config.el.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)

(describe "Property Registry"
  :var (saved-registry)
  (before-each
    (setq saved-registry org-canvas--property-registry)
    (setq org-canvas--property-registry (make-hash-table :test 'equal)))
  (after-each
    (setq org-canvas--property-registry saved-registry))

  (describe "org-canvas-register-properties"
    (it "stores a spec in the registry"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)
          (:org-prop "FRONT_PAGE" :data-key :front_page :type boolean)))
      (expect (gethash "pages" org-canvas--property-registry) :not :to-be nil))

    (it "does not overwrite an existing entry"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)))
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "EDITED" :data-key :edited :type boolean)))
      ;; Should still have the original property, not the overwritten one
      (let* ((spec (gethash "pages" org-canvas--property-registry))
             (props (plist-get spec :properties))
             (first-prop (car props)))
        (expect (plist-get first-prop :org-prop) :to-equal "PUBLISHED")))

    (it "stores multiple features independently"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)))
      (org-canvas-register-properties "assignments"
        :file-var 'org-canvas-assignments-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "POINTS" :data-key :points :type number)))
      (expect (hash-table-count org-canvas--property-registry) :to-equal 2)
      (expect (gethash "pages" org-canvas--property-registry) :not :to-be nil)
      (expect (gethash "assignments" org-canvas--property-registry) :not :to-be nil)))

  (describe "org-canvas--property-to-validate-prop"
    (it "converts a basic property spec"
      ;; Pre-bind `:type' outside `expect': the buttercup `expect' macro
      ;; expands to an `oclosure-lambda' whose `type' slot shadows the
      ;; `:type' keyword on Emacs 29.x, silently returning nil.
      (let* ((result (org-canvas--property-to-validate-prop
                      '(:org-prop "PUBLISHED" :data-key :published :type boolean)))
             (result-type (plist-get result :type)))
        (expect (plist-get result :name) :to-equal "PUBLISHED")
        (expect result-type :to-equal 'boolean)))

    (it "converts an enum property with :values"
      (let* ((result (org-canvas--property-to-validate-prop
                      '(:org-prop "GRADING_TYPE" :data-key :grading_type
                        :type enum :values ("points" "percent"))))
             (result-type (plist-get result :type)))
        (expect (plist-get result :name) :to-equal "GRADING_TYPE")
        (expect result-type :to-equal 'enum)
        (expect (plist-get result :values) :to-equal '("points" "percent"))))

    (it "converts a link property correctly"
      (let* ((result (org-canvas--property-to-validate-prop
                      '(:org-prop "GROUP" :data-key :group
                        :type link
                        :target-file org-canvas-assignment-groups-file
                        :link-id-property "CANVAS_ID")))
             (result-type (plist-get result :type)))
        (expect (plist-get result :name) :to-equal "GROUP")
        (expect result-type :to-equal 'link)
        (expect (plist-get result :target-file) :to-equal 'org-canvas-assignment-groups-file)
        (expect (plist-get result :id-property) :to-equal "CANVAS_ID"))))

  (describe "org-canvas--get-validate-specs-from-registry"
    (it "returns correct structure from registry"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)))
      (let* ((specs (org-canvas--get-validate-specs-from-registry))
             (spec (car specs)))
        (expect (length specs) :to-equal 1)
        (expect (plist-get spec :label) :to-equal "pages")
        (expect (plist-get spec :file) :to-equal 'org-canvas-pages-file)
        (expect (plist-get spec :query) :to-equal "LEVEL=1")
        (let* ((props (plist-get spec :properties))
               (prop (car props))
               (prop-type (plist-get prop :type)))
          (expect (plist-get prop :name) :to-equal "PUBLISHED")
          (expect prop-type :to-equal 'boolean))))

    (it "includes :date-order in output"
      (org-canvas-register-properties "assignments"
        :file-var 'org-canvas-assignments-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "POINTS" :data-key :points :type number))
        :date-order '(("UNLOCK_AT" "DUE_AT" "LOCK_AT")))
      (let* ((specs (org-canvas--get-validate-specs-from-registry))
             (spec (car specs)))
        (expect (plist-get spec :date-order)
                :to-equal '(("UNLOCK_AT" "DUE_AT" "LOCK_AT")))))

    (it "includes :structural-fn in output"
      (org-canvas-register-properties "assignments"
        :file-var 'org-canvas-assignments-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "POINTS" :data-key :points :type number))
        :structural-fn #'org-canvas--validate-assignment-structure)
      (let* ((specs (org-canvas--get-validate-specs-from-registry))
             (spec (car specs)))
        (expect (plist-get spec :structural-fn)
                :to-equal #'org-canvas--validate-assignment-structure))))

  (describe "org-canvas-define-payload"
    :var (test-data)

    (before-each
      (spy-on 'org-canvas--log-info)
      (spy-on 'org-canvas--log-debug)
      (org-canvas-register-properties "test-payload"
        :label "Test"
        :file-var 'org-canvas-test-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
           :api-key "published" :boolean-json t)
          (:org-prop "DUE_AT" :data-key :due_at :type timestamp
           :api-key "due_at")
          (:org-prop "NOTE" :data-key :note :type string
           :api-key "note")
          (:org-prop "NO_API" :data-key :no_api :type string)))
      (setq test-data '(:title "Test Item" :published t :due_at "2026-04-01T00:00:00Z" :note "Hello")))

    (describe "alist format"
      (before-each
        (eval '(org-canvas-define-payload test-alist
                 :registry-key "test-payload"
                 :format alist
                 :title-api-key title)))

      (it "generates a function that exists"
        (expect (fboundp 'org-canvas--test-alist-build-payload) :to-be t))

      (it "includes title in payload"
        (let ((result (org-canvas--test-alist-build-payload test-data)))
          (expect (alist-get 'title result) :to-equal "Test Item")))

      (it "converts boolean-json fields via org-canvas--to-json-boolean"
        (let ((result (org-canvas--test-alist-build-payload test-data)))
          (expect (alist-get 'published result) :to-equal t))
        (let ((result (org-canvas--test-alist-build-payload
                       (plist-put (copy-sequence test-data) :published nil))))
          (expect (alist-get 'published result) :to-equal :json-false)))

      (it "skips nil optional fields"
        (let ((result (org-canvas--test-alist-build-payload
                       '(:title "Test" :published t))))
          (expect (assq 'due_at result) :to-be nil)
          (expect (assq 'note result) :to-be nil)))

      (it "includes non-nil optional fields"
        (let ((result (org-canvas--test-alist-build-payload test-data)))
          (expect (alist-get 'due_at result) :to-equal "2026-04-01T00:00:00Z")
          (expect (alist-get 'note result) :to-equal "Hello")))

      (it "skips properties without :api-key"
        (let ((result (org-canvas--test-alist-build-payload
                       (append test-data '(:no_api "secret")))))
          (expect (assq 'no_api result) :to-be nil)))

      (it "includes static fields"
        (eval '(org-canvas-define-payload test-alist-static
                 :registry-key "test-payload"
                 :format alist
                 :title-api-key title
                 :static-fields ((is_announcement . t) (discussion_type . "side_comment"))))
        (let ((result (org-canvas--test-alist-static-build-payload test-data)))
          (expect (alist-get 'is_announcement result) :to-equal t)
          (expect (alist-get 'discussion_type result) :to-equal "side_comment")))

      (it "includes body when body-key and body-api-key provided"
        (eval '(org-canvas-define-payload test-alist-body
                 :registry-key "test-payload"
                 :format alist
                 :title-api-key title
                 :body-key :message
                 :body-api-key message))
        (let ((result (org-canvas--test-alist-body-build-payload
                       (append test-data '(:message "<p>Hi</p>")))))
          (expect (alist-get 'message result) :to-equal "<p>Hi</p>")))

      (it "calls post-build-fn"
        (eval '(org-canvas-define-payload test-alist-post
                 :registry-key "test-payload"
                 :format alist
                 :title-api-key title
                 :post-build-fn (lambda (data payload)
                                  (push '(extra . "added") payload)
                                  payload)))
        (let ((result (org-canvas--test-alist-post-build-payload test-data)))
          (expect (alist-get 'extra result) :to-equal "added"))))

    (describe "hash-table format"
      (before-each
        (eval '(org-canvas-define-payload test-hash
                 :registry-key "test-payload"
                 :format hash-table
                 :title-api-key "title"
                 :body-key :body
                 :body-api-key "body"
                 :wrapper-key "wiki_page")))

      (it "generates a function that exists"
        (expect (fboundp 'org-canvas--test-hash-build-payload) :to-be t))

      (it "wraps in wrapper key"
        (let* ((result (org-canvas--test-hash-build-payload
                        (append test-data '(:body "<p>Content</p>"))))
               (inner (gethash "wiki_page" result)))
          (expect inner :not :to-be nil)
          (expect (hash-table-p inner) :to-be t)))

      (it "includes title and body"
        (let* ((result (org-canvas--test-hash-build-payload
                        (append test-data '(:body "<p>Content</p>"))))
               (inner (gethash "wiki_page" result)))
          (expect (gethash "title" inner) :to-equal "Test Item")
          (expect (gethash "body" inner) :to-equal "<p>Content</p>")))

      (it "converts booleans"
        (let* ((result (org-canvas--test-hash-build-payload
                        (append test-data '(:body "<p>X</p>"))))
               (inner (gethash "wiki_page" result)))
          (expect (gethash "published" inner) :to-equal t))
        (let* ((result (org-canvas--test-hash-build-payload
                        '(:title "T" :published nil :body "<p>X</p>")))
               (inner (gethash "wiki_page" result)))
          (expect (gethash "published" inner) :to-equal :json-false)))

      (it "skips nil optional fields in hash-table"
        (let* ((result (org-canvas--test-hash-build-payload
                        '(:title "T" :published t :body "<p>X</p>")))
               (inner (gethash "wiki_page" result)))
          (expect (gethash "due_at" inner) :to-equal nil)))

      (it "calls extra-required-fn"
        (eval '(org-canvas-define-payload test-hash-extra
                 :registry-key "test-payload"
                 :format hash-table
                 :title-api-key "title"
                 :extra-required-fn (lambda (data inner)
                                      (puthash "custom" "value" inner))))
        (let ((result (org-canvas--test-hash-extra-build-payload test-data)))
          (expect (gethash "custom" result) :to-equal "value")))

      (it "returns inner hash when no wrapper-key"
        (eval '(org-canvas-define-payload test-hash-no-wrap
                 :registry-key "test-payload"
                 :format hash-table
                 :title-api-key "title"))
        (let ((result (org-canvas--test-hash-no-wrap-build-payload test-data)))
          (expect (hash-table-p result) :to-be t)
          (expect (gethash "title" result) :to-equal "Test Item"))))

    (describe "macro validation"
      (it "errors on missing :format"
        (expect (macroexpand '(org-canvas-define-payload bad
                                :registry-key "x"
                                :title-api-key title))
                :to-throw 'error))

      (it "errors on missing :registry-key"
        (expect (macroexpand '(org-canvas-define-payload bad
                                :format alist
                                :title-api-key title))
                :to-throw 'error))

      (it "errors on missing :title-api-key"
        (expect (macroexpand '(org-canvas-define-payload bad
                                :format alist
                                :registry-key "x"))
                :to-throw 'error)))))

(provide 'org-canvas-property-registry-test)
;;; org-canvas-property-registry-test.el ends here
