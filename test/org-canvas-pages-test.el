;;; org-canvas-pages-test.el --- Buttercup tests for pages  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-pages)

;;;; Transform (pure, no buffer)

(describe "org-canvas--page-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--page-transform-props
                   '(:title-raw "Page Title [1/3]" :canvas-url nil
                     :published-raw nil :front-page-raw nil
                     :editing-roles-raw nil :todo-date-raw nil
                     :notify-of-update-raw nil))))
      (expect (plist-get result :title) :to-equal "Page Title")))

  (it "defaults published to true"
    (let ((result (org-canvas--page-transform-props
                   '(:title-raw "Test" :canvas-url nil
                     :published-raw nil :front-page-raw nil
                     :editing-roles-raw nil :todo-date-raw nil
                     :notify-of-update-raw nil))))
      (expect (plist-get result :published) :to-be t)))

  (it "interprets front_page boolean"
    (let ((result (org-canvas--page-transform-props
                   '(:title-raw "Home" :canvas-url nil
                     :published-raw nil :front-page-raw "true"
                     :editing-roles-raw nil :todo-date-raw nil
                     :notify-of-update-raw nil))))
      (expect (plist-get result :front_page) :to-be t)))

  (it "passes through editing_roles"
    (let ((result (org-canvas--page-transform-props
                   '(:title-raw "Test" :canvas-url nil
                     :published-raw nil :front-page-raw nil
                     :editing-roles-raw "teachers,students" :todo-date-raw nil
                     :notify-of-update-raw nil))))
      (expect (plist-get result :editing_roles) :to-equal "teachers,students")))

  (it "parses TODO_DATE to ISO8601"
    (let ((result (org-canvas--page-transform-props
                   '(:title-raw "Test" :canvas-url nil
                     :published-raw nil :front-page-raw nil
                     :editing-roles-raw nil :todo-date-raw "<2026-03-01 Sun 10:00>"
                     :notify-of-update-raw nil))))
      (expect (plist-get result :student_todo_at) :to-match "2026-03-01T"))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--page-parse-entry"
  (it "extracts page title from heading"
    (with-temp-org-buffer
     "* Course Syllabus
:PROPERTIES:
:PUBLISHED: true
:END:

Welcome to the course.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :title) :to-equal "Course Syllabus"))))

  (test-org-canvas-define-common-parse-tests
   #'org-canvas--page-parse-entry
   :id-property "CANVAS_URL" :id-key :canvas-url)

  (it "parses published property (default true)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:

Body.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :published) :to-be t))))

  (it "parses front_page property"
    (with-temp-org-buffer
     "* Home Page
:PROPERTIES:
:PUBLISHED: true
:FRONT_PAGE: true
:END:

Welcome!
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :front_page) :to-be t))))

  (it "parses EDITING_ROLES property"
    (with-temp-org-buffer
     "* Editable Page
:PROPERTIES:
:PUBLISHED: true
:EDITING_ROLES: teachers,students
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :editing_roles) :to-equal "teachers,students"))))

  (it "returns nil for absent EDITING_ROLES"
    (with-temp-org-buffer
     "* Normal Page
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :editing_roles) :to-be nil))))

  (it "parses TODO_DATE as student_todo_at"
    (with-temp-org-buffer
     "* Todo Page
:PROPERTIES:
:PUBLISHED: true
:TODO_DATE: <2026-01-12 Mon 09:00>
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :student_todo_at) :to-match "^2026-01-12T"))))

  (it "returns nil for absent TODO_DATE"
    (with-temp-org-buffer
     "* Normal Page
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :student_todo_at) :to-be nil)))))

;;;; Stage 2: Build Payload

(describe "org-canvas--page-build-payload"
  (it "wraps page data in wiki_page key"
    (let* ((data '(:title "My Page" :body "<p>Content</p>" :published t))
           (payload (org-canvas--page-build-payload data)))
      (expect (gethash "wiki_page" payload) :to-be-truthy)))

  (it "includes title in payload"
    (let* ((data '(:title "Test Page" :body "" :published t))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "title" wiki-page) :to-equal "Test Page")))

  (it "includes body in payload"
    (let* ((data '(:title "Test" :body "<p>Body text</p>" :published t))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "body" wiki-page) :to-equal "<p>Body text</p>")))

  (it "sets published to t when true"
    (let* ((data '(:title "Test" :body "" :published t))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "published" wiki-page) :to-be t)))

  (it "sets published to :json-false when false"
    (let* ((data '(:title "Draft" :body "" :published nil))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "published" wiki-page) :to-equal :json-false)))

  (it "includes front_page when true"
    (let* ((data '(:title "Home" :body "" :published t :front_page t))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "front_page" wiki-page) :to-be t)))

  (it "errors on empty title"
    (let ((data '(:title "" :body "" :published t)))
      (expect (org-canvas--page-build-payload data)
              :to-throw 'error)))

  (it "includes editing_roles in payload"
    (let* ((data '(:title "Page" :body "" :published t
                   :editing_roles "teachers,students"))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "editing_roles" wiki-page)
              :to-equal "teachers,students")))

  (it "does not include editing_roles when nil"
    (let* ((data '(:title "Page" :body "" :published t
                   :editing_roles nil))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "editing_roles" wiki-page) :to-be nil)))

  (it "includes student_todo_at in payload"
    (let* ((data '(:title "Page" :body "" :published t
                   :student_todo_at "2026-01-12T09:00:00Z"))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "student_todo_at" wiki-page)
              :to-equal "2026-01-12T09:00:00Z"))))

;;;; Stage 3: Push to API (mocked)

(describe "page push-to-api (mocked)"
  (it "uses POST for new pages"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-url nil))
              (payload (make-hash-table)))
          (org-canvas--push-to-api data payload
            :endpoint "pages" :id-key :canvas-url
            :find-fn #'org-canvas--page-search-by-title)
          (expect-api-called 'POST "pages$")))))

  (it "uses PUT for existing pages"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-url "existing-page"))
              (payload (make-hash-table)))
          (org-canvas--push-to-api data payload
            :endpoint "pages" :id-key :canvas-url
            :find-fn #'org-canvas--page-search-by-title)
          (expect-api-called 'PUT "pages/existing-page"))))))

(describe "org-canvas--page-search-by-title (mocked)"
  (it "searches pages endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("pages" . [((url . "test-page") (title . "Test Page"))])))
        (org-canvas--page-search-by-title "Test Page")
        (expect-api-called 'GET "pages"))))

  (it "returns matching page"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("pages" . [((url . "syllabus") (title . "Syllabus"))
                            ((url . "other") (title . "Other"))])))
        (let ((result (org-canvas--page-search-by-title "Syllabus")))
          (expect (alist-get 'url result) :to-equal "syllabus"))))))

;;;; Stage 4: Finalize

(describe "page finalize"
  (it "saves CANVAS_URL from response"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Page" :pom (point-marker)))
           (response '((url . "test-page-url") (title . "Test Page"))))
       (org-canvas--finalize-item data response
         :id-field 'url :id-property "CANVAS_URL")
       (expect (org-entry-get (point) "CANVAS_URL") :to-equal "test-page-url"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((url . "test"))))
       (org-canvas--finalize-item data response
         :id-field 'url :id-property "CANVAS_URL")
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-")))))

;;;; Additional Parse Tests

(describe "org-canvas--page-parse-entry"
  (it "parses published=false"
    (with-temp-org-buffer
     "* Draft Page
:PROPERTIES:
:PUBLISHED: false
:END:

Draft content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :published) :to-be nil))))

  (it "defaults front_page to nil"
    (with-temp-org-buffer
     "* Normal Page
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :front_page) :to-be nil))))

  (it "exports body content to HTML"
    (with-temp-org-buffer
     "* Page with Content
:PROPERTIES:
:END:

This is *bold* text.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       ;; Body should contain HTML
       (expect (plist-get data :body) :to-match "<")))))

;;;; Additional Build Payload Tests

(describe "org-canvas--page-build-payload"
  (it "does not include front_page when false"
    (let* ((data '(:title "Normal" :body "" :published t :front_page nil))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      ;; front_page should not be in hash table when nil
      (expect (gethash "front_page" wiki-page) :to-be nil))))

;;;; Additional Push to API Tests

(describe "page push-to-api error recovery (mocked)"
  (it "retries as POST on 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (and (eq method 'PUT) (= call-count 1))
                         (signal 'error '("API Request Failed (HTTP 404)" nil nil))
                       '((url . "new-page-url"))))))
          (let ((data '(:title "Stale Page" :canvas-url "old-url"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--push-to-api data payload
                            :endpoint "pages" :id-key :canvas-url
                            :find-fn #'org-canvas--page-search-by-title)))
              (expect (alist-get 'url result) :to-equal "new-page-url")
              (expect call-count :to-equal 2)))))))

  (it "recovers from timeout"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that times out
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error (list "Request failed" "Timeout")))
                      ;; Second call: search finds the page
                      ((eq method 'GET)
                       [((url . "recovered-page") (title . "Timeout Page"))])
                      (t nil)))))
          (let ((data '(:title "Timeout Page" :canvas-url nil))
                (payload (make-hash-table)))
            (let ((result (org-canvas--push-to-api data payload
                            :endpoint "pages" :id-key :canvas-url
                            :find-fn #'org-canvas--page-search-by-title)))
              (expect (alist-get 'url result) :to-equal "recovered-page")))))))

  (it "signals error on non-recoverable failure"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request" nil nil)))))
        (let ((data '(:title "Bad Page" :canvas-url nil))
              (payload (make-hash-table)))
          (expect (org-canvas--push-to-api data payload
                    :endpoint "pages" :id-key :canvas-url
                    :find-fn #'org-canvas--page-search-by-title)
                  :to-throw 'error))))))

;;;; Additional Find by Title Tests

(describe "org-canvas--page-search-by-title (mocked)"
  (it "returns nil when no match"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("pages" . [((url . "other") (title . "Other Page"))])))
        (let ((result (org-canvas--page-search-by-title "Nonexistent")))
          (expect result :to-be nil)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("API error")))))
        (let ((result (org-canvas--page-search-by-title "Any")))
          (expect result :to-be nil))))))

;;;; Additional Finalize Tests

(describe "page finalize error"
  (it "signals error when no URL in response"
    (with-temp-org-buffer
     "* No URL Page
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No URL Page" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (expect (org-canvas--finalize-item data response
                 :id-field 'url :id-property "CANVAS_URL")
               :to-throw 'error)))))

;;;; Delete Page at Point Tests

(describe "org-canvas-delete-page-at-point (mocked)"
  (it "deletes page and clears properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test Page
:PROPERTIES:
:CANVAS_URL: test-page-url
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas-delete-page-at-point)
           (expect-api-called 'DELETE "pages/test-page-url")
           (expect (org-entry-get (point) "CANVAS_URL") :to-be nil)
           (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))

  (it "errors when no CANVAS_URL present"
    (with-temp-org-buffer
     "* New Page
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-page-at-point) :to-throw 'user-error)))

  (it "handles delete API error gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("Delete failed"))))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (with-temp-org-buffer
         "* Page To Delete
:PROPERTIES:
:CANVAS_URL: some-page
:END:
"
         (org-back-to-heading)
         ;; Should not throw, just message
         (org-canvas-delete-page-at-point)
         ;; Properties should still exist since delete failed
         (expect (org-entry-get (point) "CANVAS_URL") :to-equal "some-page")))))

  (it "aborts when user says no"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Page
:PROPERTIES:
:CANVAS_URL: my-page
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
           (org-canvas-delete-page-at-point)
           ;; Should NOT have called DELETE
           (expect (test-org-canvas-api-called-p 'DELETE "pages/my-page")
                   :to-be nil)
           ;; Properties should still exist
           (expect (org-entry-get (point) "CANVAS_URL") :to-equal "my-page")))))))

;;;; Sync Pages Pipeline

(describe "org-canvas-sync-pages (mocked)"
  (it "errors when pages file not found"
    (let ((org-canvas-pages-file "/nonexistent/pages.org"))
      (expect (org-canvas-sync-pages) :to-throw 'error)))

  (it "syncs pages from file"
    (let ((temp-dir (make-temp-file "pages-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (synced-count 0))
            (with-temp-file org-file
              (insert "* Test Page
:PROPERTIES:
:END:

Page content.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq synced-count (1+ synced-count)))
                             '((url . "test-page-url")))))
                  (org-canvas-sync-pages)
                  (expect synced-count :to-equal 1)
                  ;; Check CANVAS_URL was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_URL") :to-equal "test-page-url"))))))
        (delete-directory temp-dir t)))))

;;;; Delete All Pages

(describe "org-canvas-delete-all-pages (mocked)"
  (it "skips front page during deletion"
    (let ((temp-dir (make-temp-file "pages-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (deleted-count 0))
            (with-temp-file org-file
              (insert "* Home
:PROPERTIES:
:CANVAS_URL: home
:END:
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((url . "home") (title . "Home") (front_page . t))
                               ((url . "other") (title . "Other") (front_page . nil)))))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _endpoint-fn _id-field _title-field &optional skip-fn)
                             (let ((to-delete (if skip-fn (cl-remove-if skip-fn items) items)))
                               (setq deleted-count (length to-delete))
                               (cons (length to-delete) nil)))))
                  (org-canvas-delete-all-pages)
                  ;; Only the non-front page should be deleted
                  (expect deleted-count :to-equal 1)))))
        (delete-directory temp-dir t)))))

;;;; Export Edge Cases

(describe "org-canvas--page-parse-entry export edge cases"
  (it "resolves cross-file links via export-subtree-body-to-html"
    (with-temp-org-buffer
     "* Buffer Page
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-export-as)
                (lambda (&rest _args)
                  "<p>Resolved content</p>")))
       (let ((data (org-canvas--page-parse-entry)))
         (expect (plist-get data :body) :to-equal "<p>Resolved content</p>")))))

  (it "handles export signaling error"
    (with-temp-org-buffer
     "* Error Page
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-export-as)
                (lambda (&rest _args)
                  (error "Export failed"))))
       (expect (org-canvas--page-parse-entry) :to-throw 'error))))

  (it "handles export returning unexpected type"
    (with-temp-org-buffer
     "* Bad Page
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-export-as)
                (lambda (&rest _args) 42)))
       (expect (org-canvas--page-parse-entry) :to-throw 'error)))))

;;;; Additional Coverage Tests

(describe "org-canvas--page-validate-editing-roles"
  (it "warns on invalid role"
    (spy-on 'message)
    (org-canvas--page-validate-editing-roles "teachers,invalid_role")
    (expect 'message :to-have-been-called))

  (it "accepts valid roles without warning"
    (spy-on 'message)
    (org-canvas--page-validate-editing-roles "teachers,students")
    (expect 'message :not :to-have-been-called)))

;;;; NOTIFY_OF_UPDATE Property Tests

(describe "org-canvas--page-parse-entry NOTIFY_OF_UPDATE"
  (it "parses NOTIFY_OF_UPDATE true"
    (with-temp-org-buffer
     "* Updated Page
:PROPERTIES:
:PUBLISHED: true
:NOTIFY_OF_UPDATE: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :notify_of_update) :to-be t))))

  (it "defaults NOTIFY_OF_UPDATE to nil"
    (with-temp-org-buffer
     "* Normal Page
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :notify_of_update) :to-be nil))))

  (it "parses NOTIFY_OF_UPDATE false"
    (with-temp-org-buffer
     "* Page
:PROPERTIES:
:PUBLISHED: true
:NOTIFY_OF_UPDATE: false
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--page-parse-entry)))
       (expect (plist-get data :notify_of_update) :to-be nil)))))

(describe "org-canvas--page-build-payload NOTIFY_OF_UPDATE"
  (it "includes notify_of_update when true"
    (let* ((data '(:title "Page" :body "<p>Body</p>" :published t
                   :notify_of_update t))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "notify_of_update" wiki-page) :to-be t)))

  (it "does not include notify_of_update when nil"
    (let* ((data '(:title "Page" :body "<p>Body</p>" :published t
                   :notify_of_update nil))
           (payload (org-canvas--page-build-payload data))
           (wiki-page (gethash "wiki_page" payload)))
      (expect (gethash "notify_of_update" wiki-page) :to-be nil))))

;;; org-canvas-pages-test.el ends here
