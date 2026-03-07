;;; org-canvas-group-categories-test.el --- Buttercup tests for group categories  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-group-categories)

;;;; Transform (pure, no buffer)

(describe "org-canvas--group-category-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--group-category-transform-props
                   '(:title-raw "Project Groups [2/3]" :canvas-id nil
                     :self-signup-raw nil :group-limit-raw nil
                     :auto-leader-raw nil :create-group-count-raw nil))))
      (expect (plist-get result :title) :to-equal "Project Groups")))

  (it "validates self-signup values"
    (let ((result (org-canvas--group-category-transform-props
                   '(:title-raw "Groups" :canvas-id nil
                     :self-signup-raw "enabled" :group-limit-raw nil
                     :auto-leader-raw nil :create-group-count-raw nil))))
      (expect (plist-get result :self_signup) :to-equal "enabled")))

  (it "converts group-limit to number"
    (let ((result (org-canvas--group-category-transform-props
                   '(:title-raw "Groups" :canvas-id nil
                     :self-signup-raw nil :group-limit-raw "5"
                     :auto-leader-raw nil :create-group-count-raw nil))))
      (expect (plist-get result :group_limit) :to-equal 5)))

  (it "returns nil for absent optional fields"
    (let ((result (org-canvas--group-category-transform-props
                   '(:title-raw "Groups" :canvas-id nil
                     :self-signup-raw nil :group-limit-raw nil
                     :auto-leader-raw nil :create-group-count-raw nil))))
      (expect (plist-get result :self_signup) :to-be nil)
      (expect (plist-get result :group_limit) :to-be nil)
      (expect (plist-get result :auto_leader) :to-be nil)
      (expect (plist-get result :create_group_count) :to-be nil))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--group-category-parse-entry"
  (it "extracts group category title from heading"
    (with-temp-org-buffer
     "* Project Groups
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :title) :to-equal "Project Groups"))))

  (test-org-canvas-define-common-parse-tests
   #'org-canvas--group-category-parse-entry)

  (it "parses SELF_SIGNUP=enabled"
    (with-temp-org-buffer
     "* Sign Up Groups
:PROPERTIES:
:SELF_SIGNUP: enabled
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :self_signup) :to-equal "enabled"))))

  (it "parses SELF_SIGNUP=restricted"
    (with-temp-org-buffer
     "* Restricted Groups
:PROPERTIES:
:SELF_SIGNUP: restricted
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :self_signup) :to-equal "restricted"))))

  (it "validates SELF_SIGNUP with warning for invalid values"
    (with-temp-org-buffer
     "* Bad Groups
:PROPERTIES:
:SELF_SIGNUP: invalid_value
:END:
"
     (org-back-to-heading)
     (spy-on 'elog-warning)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect 'elog-warning :to-have-been-called))))

  (it "parses GROUP_LIMIT as number"
    (with-temp-org-buffer
     "* Limited Groups
:PROPERTIES:
:GROUP_LIMIT: 4
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :group_limit) :to-equal 4))))

  (it "parses AUTO_LEADER=first"
    (with-temp-org-buffer
     "* Leader Groups
:PROPERTIES:
:AUTO_LEADER: first
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :auto_leader) :to-equal "first"))))

  (it "parses AUTO_LEADER=random"
    (with-temp-org-buffer
     "* Random Leader Groups
:PROPERTIES:
:AUTO_LEADER: random
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :auto_leader) :to-equal "random"))))

  (it "parses CREATE_GROUP_COUNT as number"
    (with-temp-org-buffer
     "* Auto Groups
:PROPERTIES:
:CREATE_GROUP_COUNT: 10
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :create_group_count) :to-equal 10))))

  (it "returns nil for optional absent properties"
    (with-temp-org-buffer
     "* Minimal Groups
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--group-category-parse-entry)))
       (expect (plist-get data :self_signup) :to-be nil)
       (expect (plist-get data :group_limit) :to-be nil)
       (expect (plist-get data :auto_leader) :to-be nil)
       (expect (plist-get data :create_group_count) :to-be nil))))

)

;;;; Stage 2: Build Payload

(describe "org-canvas--group-category-build-payload"
  (it "includes name in payload"
    (let* ((data '(:title "Project Groups"))
           (payload (org-canvas--group-category-build-payload data)))
      (expect (alist-get 'name payload) :to-equal "Project Groups")))

  (it "includes self_signup when set"
    (let* ((data '(:title "Groups" :self_signup "enabled"))
           (payload (org-canvas--group-category-build-payload data)))
      (expect (alist-get 'self_signup payload) :to-equal "enabled")))

  (it "includes group_limit when set"
    (let* ((data '(:title "Groups" :group_limit 4))
           (payload (org-canvas--group-category-build-payload data)))
      (expect (alist-get 'group_limit payload) :to-equal 4)))

  (it "includes auto_leader when set"
    (let* ((data '(:title "Groups" :auto_leader "random"))
           (payload (org-canvas--group-category-build-payload data)))
      (expect (alist-get 'auto_leader payload) :to-equal "random")))

  (it "includes create_group_count when set"
    (let* ((data '(:title "Groups" :create_group_count 10))
           (payload (org-canvas--group-category-build-payload data)))
      (expect (alist-get 'create_group_count payload) :to-equal 10)))

  (it "omits nil optional fields"
    (let* ((data '(:title "Minimal Groups"))
           (payload (org-canvas--group-category-build-payload data)))
      (expect (assq 'self_signup payload) :to-be nil)
      (expect (assq 'group_limit payload) :to-be nil)
      (expect (assq 'auto_leader payload) :to-be nil)
      (expect (assq 'create_group_count payload) :to-be nil))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--group-category-push-to-api (mocked)"
  (it "uses POST with course endpoint for new group categories"
    (with-org-canvas-test-config
      (let ((api-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (push (list method url) api-calls)
                     '((id . 42) (name . "New Groups")))))
          (let ((data '(:title "New Groups" :canvas-id nil))
                (payload '((name . "New Groups"))))
            (org-canvas--group-category-push-to-api data payload)
            (let ((call (car api-calls)))
              (expect (car call) :to-equal 'POST)
              (expect (cadr call) :to-match "courses/.*/group_categories")))))))

  (it "uses PUT with global endpoint for existing group categories"
    (with-org-canvas-test-config
      (let ((api-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (push (list method url) api-calls)
                     '((id . 42) (name . "Existing Groups")))))
          (let ((data '(:title "Existing Groups" :canvas-id "42"))
                (payload '((name . "Existing Groups"))))
            (org-canvas--group-category-push-to-api data payload)
            (let ((call (car api-calls)))
              (expect (car call) :to-equal 'PUT)
              (expect (cadr call) :to-match "api/v1/group_categories/42")))))))

  (it "retries as POST on 404 for stale canvas-id"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         (signal 'error '("HTTP 404 Not Found"))
                       '((id . 99) (name . "Recovered"))))))
          (let ((data '(:title "Stale" :canvas-id "999"))
                (payload '((name . "Stale"))))
            (let ((result (org-canvas--group-category-push-to-api data payload)))
              (expect call-count :to-equal 2)
              (expect (alist-get 'id result) :to-equal 99)))))))

  (it "searches Canvas on timeout"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Request timed out"))))
                ((symbol-function 'org-canvas--search-item)
                 (lambda (_endpoint _title &rest _args)
                   '((id . 77) (name . "Found After Timeout")))))
        (let ((data '(:title "Timeout Test" :canvas-id nil))
              (payload '((name . "Timeout Test"))))
          (let ((result (org-canvas--group-category-push-to-api data payload)))
            (expect (alist-get 'id result) :to-equal 77))))))

  (it "returns dry-run response when dry-run is active"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t))
        (let ((data '(:title "Dry" :canvas-id nil))
              (payload '((name . "Dry"))))
          (let ((result (org-canvas--group-category-push-to-api data payload)))
            (expect (alist-get 'id result) :to-equal "dry-run"))))))

  (it "returns dry-run response for existing items"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t))
        (let ((data '(:title "Dry" :canvas-id "42"))
              (payload '((name . "Dry"))))
          (let ((result (org-canvas--group-category-push-to-api data payload)))
            (expect (alist-get 'id result) :to-equal "dry-run")))))))

;;;; Stage 4: Finalize

(describe "group-category finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Groups
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Groups" :pom (point-marker)))
           (response '((id . 42) (name . "Test Groups"))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test Groups
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Groups" :pom (point-marker)))
           (response '((id . 42))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-"))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* Test Groups
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Groups" :pom (point-marker)))
           (response '((error . "bad"))))
       (expect (org-canvas--finalize-item data response) :to-throw 'error)))))

;;;; Sync Pipeline Tests

(describe "org-canvas-sync-group-categories (mocked)"
  (it "errors when file not found"
    (let ((org-canvas-group-categories-file "/nonexistent/group-categories.org"))
      (expect (org-canvas-sync-group-categories) :to-throw 'error)))

  (it "syncs group categories from file"
    (let ((temp-dir (make-temp-file "gc-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "group-categories.org" temp-dir))
                 (synced-count 0))
            (with-temp-file org-file
              (insert "* Project Groups
:PROPERTIES:
:SELF_SIGNUP: enabled
:GROUP_LIMIT: 4
:END:
"))
            (let ((org-canvas-group-categories-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq synced-count (1+ synced-count)))
                             '((id . 42)))))
                  (org-canvas-sync-group-categories)
                  (expect synced-count :to-equal 1)
                  ;; Check CANVAS_ID was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))))
        (delete-directory temp-dir t)))))

;;;; Delete All Tests

(describe "org-canvas-delete-all-group-categories (mocked)"
  (it "deletes group categories using global endpoint"
    (let ((temp-dir (make-temp-file "gc-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "group-categories.org" temp-dir))
                 (deleted-urls nil))
            (with-temp-file org-file
              (insert "* Groups
:PROPERTIES:
:CANVAS_ID: 42
:END:
"))
            (let ((org-canvas-group-categories-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 42) (name . "Groups")))))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method url &rest _args)
                             (when (eq method 'DELETE)
                               (push url deleted-urls))
                             nil)))
                  (org-canvas-delete-all-group-categories)
                  (expect (length deleted-urls) :to-equal 1)
                  (expect (car deleted-urls) :to-match "api/v1/group_categories/42")))))
        (delete-directory temp-dir t))))

  (it "logs warning when DELETE fails for a group category"
    (let ((temp-dir (make-temp-file "gc-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "group-categories.org" temp-dir))
                 (warning-logged nil))
            (with-temp-file org-file
              (insert "* Failing Groups
:PROPERTIES:
:CANVAS_ID: 55
:END:
"))
            (let ((org-canvas-group-categories-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 55) (name . "Failing Groups")))))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'DELETE)
                               (signal 'error '("Connection refused")))))
                          ((symbol-function 'elog-error)
                           (lambda (_logger fmt &rest _args)
                             (when (string-match-p "Delete failed" fmt)
                               (setq warning-logged t)))))
                  (org-canvas-delete-all-group-categories)
                  (expect warning-logged :to-be t)))))
        (delete-directory temp-dir t)))))

;;;; Pull Function Tests

(describe "org-canvas--group-category-pull-item"
  (it "sets SELF_SIGNUP property"
    (with-temp-org-buffer
     "* Groups
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--group-category-pull-item
      '((id . 1) (name . "Groups") (self_signup . "enabled")
        (group_limit . 4) (auto_leader . "first"))
      (point))
     (expect (org-entry-get (point) "SELF_SIGNUP") :to-equal "enabled")))

  (it "sets GROUP_LIMIT property"
    (with-temp-org-buffer
     "* Groups
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--group-category-pull-item
      '((id . 1) (name . "Groups") (group_limit . 5))
      (point))
     (expect (org-entry-get (point) "GROUP_LIMIT") :to-equal "5")))

  (it "sets AUTO_LEADER property"
    (with-temp-org-buffer
     "* Groups
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--group-category-pull-item
      '((id . 1) (name . "Groups") (auto_leader . "random"))
      (point))
     (expect (org-entry-get (point) "AUTO_LEADER") :to-equal "random")))

  (it "skips nil properties"
    (with-temp-org-buffer
     "* Groups
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--group-category-pull-item
      '((id . 1) (name . "Groups") (self_signup . nil) (group_limit . 0))
      (point))
     (expect (org-entry-get (point) "SELF_SIGNUP") :to-be nil)
     (expect (org-entry-get (point) "GROUP_LIMIT") :to-be nil))))

;;;; Delete at Point Tests

(describe "org-canvas-delete-group-category-at-point"
  (it "errors when no CANVAS_ID present"
    (with-temp-org-buffer
     "* Unsynced Groups
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-group-category-at-point)
             :to-throw 'user-error)))

  (it "deletes via global endpoint when confirmed"
    (with-temp-org-buffer
     "* Synced Groups
:PROPERTIES:
:CANVAS_ID: 42
:LAST_SYNCED: [2026-01-01 Thu]
:END:
"
     (org-back-to-heading)
     (let ((deleted-url nil)
           (org-canvas-base-url "https://test.canvas.example.com"))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                 ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                 ((symbol-function 'display-buffer) (lambda (_) nil))
                 ((symbol-function 'org-canvas-api-request)
                  (lambda (_method url &rest _args)
                    (setq deleted-url url)
                    nil)))
         (org-canvas-delete-group-category-at-point)
         (expect deleted-url :to-match "api/v1/group_categories/42")
         (expect (org-entry-get (point) "CANVAS_ID") :to-be nil))))))

;;;; Push Error Path Tests

(describe "org-canvas--group-category-push-to-api (error paths)"
  (it "re-signals non-timeout non-404 errors"
    (with-org-canvas-test-config
      (let ((data '(:title "Groups" :canvas-id nil)))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _args)
                     (signal 'error '("500 Internal Server Error"))))
                  ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                  ((symbol-function 'display-buffer) (lambda (_) nil)))
          (expect (org-canvas--group-category-push-to-api data '((name . "Groups")))
                  :to-throw 'error)))))

  (it "re-signals when timeout recovery finds nothing"
    (with-org-canvas-test-config
      (let ((data '(:title "Groups" :canvas-id nil)))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _args)
                     (signal 'error '("request timed out"))))
                  ((symbol-function 'org-canvas--search-item)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                  ((symbol-function 'display-buffer) (lambda (_) nil)))
          (expect (org-canvas--group-category-push-to-api data '((name . "Groups")))
                  :to-throw 'error)))))

  (it "retries as POST on 404 for stale CANVAS_ID"
    (with-org-canvas-test-config
      (let ((data '(:title "Groups" :canvas-id "99"))
            (call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (cl-incf call-count)
                     (if (= call-count 1)
                         (signal 'error '("404 Not Found"))
                       '((id . 200) (name . "Groups")))))
                  ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                  ((symbol-function 'display-buffer) (lambda (_) nil)))
          (let ((result (org-canvas--group-category-push-to-api data '((name . "Groups")))))
            (expect (alist-get 'id result) :to-equal 200)
            (expect call-count :to-equal 2)))))))

;;;; Delete at Point Error Tests

(describe "org-canvas-delete-group-category-at-point (error handling)"
  (it "handles API error gracefully"
    (with-temp-org-buffer
     "* Groups
:PROPERTIES:
:CANVAS_ID: 42
:END:
"
     (org-back-to-heading)
     (with-org-canvas-test-config
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                 ((symbol-function 'org-canvas-api-request)
                  (lambda (_method _url &rest _args)
                    (signal 'error '("403 Forbidden"))))
                 ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                 ((symbol-function 'display-buffer) (lambda (_) nil)))
         ;; Should not throw — error is caught
         (org-canvas-delete-group-category-at-point)
         ;; CANVAS_ID should still be present (delete failed)
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42"))))))

;;;; Pull Pipeline Test

(describe "org-canvas-pull-group-categories"
  (it "imports categories into new file"
    (let* ((temp-dir (make-temp-file "gc-pull" t))
           (gc-file (expand-file-name "group-categories.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-group-categories-file gc-file))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional _params)
                           '(((id . 10) (name . "Lab Groups")
                              (self_signup . "enabled")
                              (group_limit . 4)))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil))
                        ((symbol-function 'org-canvas--pull-confirm-overwrite)
                         (lambda (&rest _) nil)))
                (org-canvas-pull-group-categories)
                (with-current-buffer (find-file-noselect gc-file)
                  (expect (buffer-string) :to-match "Lab Groups")
                  (expect (buffer-string) :to-match "CANVAS_ID")))))
        (let ((buf (find-buffer-visiting gc-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;; org-canvas-group-categories-test.el ends here
