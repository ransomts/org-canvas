;;; org-canvas-announcements-test.el --- Buttercup tests for announcements  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-announcements)

;;;; Stage 1: Parse Entry

(describe "org-canvas--announcement-parse-entry"
  (it "extracts title from heading"
    (with-temp-org-buffer
     "* Important Announcement
:PROPERTIES:
:PUBLISHED: true
:END:

Body text here.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :title) :to-equal "Important Announcement"))))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Test Announcement
:PROPERTIES:
:CANVAS_ID: 12345
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "12345"))))

  (it "returns nil canvas-id for new announcements"
    (with-temp-org-buffer
     "* New Announcement
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :canvas-id) :to-be nil))))

  (it "parses published property (default true)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:

Body.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :published) :to-be t))))

  (it "parses published=false"
    (with-temp-org-buffer
     "* Draft Announcement
:PROPERTIES:
:PUBLISHED: false
:END:

Draft content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :published) :to-be nil))))

  (it "sets is_announcement to t"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:PUBLISHED: true
:END:

Body.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :is_announcement) :to-be t))))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:

Body.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:END:\n\nBody.\n")
     (org-back-to-heading)
     (expect (org-canvas--announcement-parse-entry) :to-throw 'error)))

  (it "parses POST_AT timestamp"
    (with-temp-org-buffer
     "* Scheduled Announcement
:PROPERTIES:
:POST_AT: <2024-06-15 Sat 10:00>
:END:

Scheduled content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       ;; POST_AT should be parsed into delayed_post_at as ISO8601
       (expect (plist-get data :delayed_post_at) :to-be-truthy)
       (expect (plist-get data :delayed_post_at) :to-match "T.*Z$"))))

  (it "parses ALLOW_COMMENTS=true"
    (with-temp-org-buffer
     "* Announcement with Comments
:PROPERTIES:
:ALLOW_COMMENTS: true
:END:

Discussion welcome.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :allow_discussion_comments) :to-be t))))

  (it "parses ALLOW_COMMENTS=false"
    (with-temp-org-buffer
     "* Locked Announcement
:PROPERTIES:
:ALLOW_COMMENTS: false
:END:

No comments allowed.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :allow_discussion_comments) :to-be nil)))))

;;;; Stage 2: Build Payload

(describe "org-canvas--announcement-build-payload"
  (it "includes title in payload"
    (let* ((data '(:title "My Announcement" :message "<p>Hello</p>" :published t))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (alist-get 'title payload) :to-equal "My Announcement")))

  (it "includes message in payload"
    (let* ((data '(:title "Test" :message "<p>Body content</p>" :published t))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (alist-get 'message payload) :to-equal "<p>Body content</p>")))

  (it "sets is_announcement to t"
    (let* ((data '(:title "Test" :message "" :published t))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (alist-get 'is_announcement payload) :to-be t)))

  (it "sets discussion_type to side_comment"
    (let* ((data '(:title "Test" :message "" :published t))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (alist-get 'discussion_type payload) :to-equal "side_comment")))

  (it "includes delayed_post_at when specified"
    (let* ((data '(:title "Test" :message "" :published t :delayed_post_at "2024-01-15T09:00:00Z"))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (alist-get 'delayed_post_at payload) :to-equal "2024-01-15T09:00:00Z")))

  (it "adds lock_at when allow_discussion_comments is nil"
    (let* ((data '(:title "Locked" :message "" :published t :allow_discussion_comments nil))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (alist-get 'lock_at payload) :to-match "^20[0-9][0-9]-")))

  (it "does not add lock_at when allow_discussion_comments is t"
    (let* ((data '(:title "Open" :message "" :published t :allow_discussion_comments t))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (alist-get 'lock_at payload) :to-be nil))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--announcement-push-to-api (mocked)"
  (it "uses POST for new announcements"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-id nil))
              (payload '((title . "New"))))
          (org-canvas--announcement-push-to-api data payload)
          (expect-api-called 'POST "discussion_topics")))))

  (it "uses PUT for existing announcements"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "123"))
              (payload '((title . "Existing"))))
          (org-canvas--announcement-push-to-api data payload)
          (expect-api-called 'PUT "discussion_topics/123"))))))

;;;; Stage 4: Finalize

(describe "org-canvas--announcement-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Announcement
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Announcement" :pom (point-marker)))
           (response '((id . 99999) (title . "Test Announcement"))))
       (org-canvas--announcement-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "99999"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 88888))))
       (org-canvas--announcement-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-"))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((error . "something failed"))))
       (expect (org-canvas--announcement-finalize data response) :to-throw 'error)))))

;;;; Sync Pipeline Tests

(describe "org-canvas-sync-announcements (mocked)"
  (it "errors when announcements file not found"
    (let ((org-canvas-announcements-file "/nonexistent/announcements.org"))
      (expect (org-canvas-sync-announcements) :to-throw 'error)))

  (it "syncs announcements from file"
    (let ((temp-dir (make-temp-file "announcements-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "announcements.org" temp-dir))
                 (synced-count 0))
            (with-temp-file org-file
              (insert "* Test Announcement
:PROPERTIES:
:END:

Body content.
"))
            (let ((org-canvas-announcements-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq synced-count (1+ synced-count)))
                             '((id . 12345)))))
                  (org-canvas-sync-announcements)
                  (expect synced-count :to-equal 1)
                  ;; Check CANVAS_ID was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345"))))))
        (delete-directory temp-dir t)))))

;;;; Delete All Announcements Tests

(describe "org-canvas-delete-all-announcements (mocked)"
  (it "deletes all announcements"
    (let ((temp-dir (make-temp-file "announcements-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "announcements.org" temp-dir))
                 (deleted-count 0))
            (with-temp-file org-file
              (insert "* Announcement
:PROPERTIES:
:CANVAS_ID: 111
:END:
"))
            (let ((org-canvas-announcements-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 111) (title . "Announcement")))))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn)
                             (setq deleted-count (length items))
                             (cons (length items) (mapcar (lambda (i) (number-to-string (alist-get 'id i))) items)))))
                  (org-canvas-delete-all-announcements)
                  (expect deleted-count :to-equal 1)))))
        (delete-directory temp-dir t)))))

;;;; Push to API Error Path

(describe "org-canvas--announcement-push-to-api error path"
  (it "re-signals API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error: 500 Internal Server Error")))))
        (let ((data '(:title "Bad" :canvas-id nil))
              (payload '((title . "Bad"))))
          (expect (org-canvas--announcement-push-to-api data payload)
                  :to-throw 'error)))))

  (it "re-signals error for existing announcement update"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error: 403 Forbidden")))))
        (let ((data '(:title "Forbidden" :canvas-id "123"))
              (payload '((title . "Forbidden"))))
          (expect (org-canvas--announcement-push-to-api data payload)
                  :to-throw 'error))))))

;;;; Pull Function Tests

(describe "org-canvas--announcement-pull-item"
  (it "sets DELAYED_POST_AT from ISO timestamp"
    (with-temp-org-buffer
     "* Announcement
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas--html-to-org)
                (lambda (html) html)))
       (org-canvas--announcement-pull-item
        '((id . 1) (title . "Announcement")
          (delayed_post_at . "2026-06-15T09:00:00Z")
          (message . "<p>Hello</p>"))
        (point))
       (expect (org-entry-get (point) "DELAYED_POST_AT") :to-match "<2026-06-15"))))

  (it "skips DELAYED_POST_AT when nil"
    (with-temp-org-buffer
     "* Announcement
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas--html-to-org)
                (lambda (html) html)))
       (org-canvas--announcement-pull-item
        '((id . 1) (title . "Announcement")
          (delayed_post_at . nil)
          (message . "<p>Hello</p>"))
        (point))
       (expect (org-entry-get (point) "DELAYED_POST_AT") :to-be nil))))

  (it "inserts body text"
    (with-temp-org-buffer
     "* Announcement
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas--html-to-org)
                (lambda (html) (replace-regexp-in-string "<[^>]+>" "" html))))
       (org-canvas--announcement-pull-item
        '((id . 1) (title . "Ann") (message . "<p>Important info</p>"))
        (point))
       (expect (buffer-string) :to-match "Important info")))))

;;;; SPECIFIC_SECTIONS Property Tests

(describe "org-canvas--announcement-parse-entry SPECIFIC_SECTIONS"
  (it "parses SPECIFIC_SECTIONS string"
    (with-temp-org-buffer
     "* Section Announcement
:PROPERTIES:
:PUBLISHED: true
:SPECIFIC_SECTIONS: section_1,section_2
:END:

Content for specific sections.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :specific_sections) :to-equal "section_1,section_2"))))

  (it "returns nil for absent SPECIFIC_SECTIONS"
    (with-temp-org-buffer
     "* General Announcement
:PROPERTIES:
:PUBLISHED: true
:END:

Content for everyone.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       (expect (plist-get data :specific_sections) :to-be nil)))))

(describe "org-canvas--announcement-build-payload SPECIFIC_SECTIONS"
  (it "resolves section names to IDs via sections file"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n\n* Section B\n:PROPERTIES:\n:CANVAS_ID: 200\n:END:\n"))
            (let* ((org-canvas-sections-file sections-file)
                   (data '(:title "Test" :message "<p>Body</p>" :published t
                           :specific_sections "Section A,Section B"))
                   (payload (org-canvas--announcement-build-payload data)))
              (expect (alist-get 'specific_sections payload)
                      :to-equal "100,200")))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "passes through numeric IDs unchanged"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "#+TITLE: Sections\n"))
            (let* ((org-canvas-sections-file sections-file)
                   (data '(:title "Test" :message "<p>Body</p>" :published t
                           :specific_sections "123,456"))
                   (payload (org-canvas--announcement-build-payload data)))
              (expect (alist-get 'specific_sections payload)
                      :to-equal "123,456")))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "excludes specific_sections when nil"
    (let* ((data '(:title "Test" :message "<p>Body</p>" :published t
                   :specific_sections nil))
           (payload (org-canvas--announcement-build-payload data)))
      (expect (assq 'specific_sections payload) :to-be nil))))

;;; org-canvas-announcements-test.el ends here
