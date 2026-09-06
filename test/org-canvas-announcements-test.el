;;; org-canvas-announcements-test.el --- Buttercup tests for announcements  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-announcements)

;;;; Transform (pure, no buffer)

(describe "org-canvas--announcement-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--announcement-transform-props
                   '(:title-raw "My Announcement [1/3]" :canvas-id nil
                     :published-raw nil :posted-at-raw nil
                     :delayed-post-at-raw nil :author-raw nil
                     :allow-comments-raw nil :specific-sections-raw nil))))
      (expect (plist-get result :title) :to-equal "My Announcement")))

  (it "defaults published to true when nil"
    (let ((result (org-canvas--announcement-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw nil :posted-at-raw nil
                     :delayed-post-at-raw nil :author-raw nil
                     :allow-comments-raw nil :specific-sections-raw nil))))
      (expect (plist-get result :published) :to-be t)))

  (it "interprets published=false"
    (let ((result (org-canvas--announcement-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw "false" :posted-at-raw nil
                     :delayed-post-at-raw nil :author-raw nil
                     :allow-comments-raw nil :specific-sections-raw nil))))
      (expect (plist-get result :published) :to-be nil)))

  (it "parses DELAYED_POST_AT to ISO8601"
    (let ((result (org-canvas--announcement-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw nil :posted-at-raw nil
                     :delayed-post-at-raw "<2024-06-15 Sat 10:00>"
                     :author-raw nil
                     :allow-comments-raw nil :specific-sections-raw nil))))
      (expect (plist-get result :delayed_post_at) :to-match "2024-06-15T")))

  (it "returns nil for absent DELAYED_POST_AT"
    (let ((result (org-canvas--announcement-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw nil :posted-at-raw nil
                     :delayed-post-at-raw nil :author-raw nil
                     :allow-comments-raw nil :specific-sections-raw nil))))
      (expect (plist-get result :delayed_post_at) :to-be nil)))

  (it "interprets ALLOW_COMMENTS boolean"
    (let ((result (org-canvas--announcement-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw nil :posted-at-raw nil
                     :delayed-post-at-raw nil :author-raw nil
                     :allow-comments-raw "true" :specific-sections-raw nil))))
      (expect (plist-get result :allow_discussion_comments) :to-be t)))

  (it "passes through canvas-id and specific-sections"
    (let ((result (org-canvas--announcement-transform-props
                   '(:title-raw "Test" :canvas-id "42"
                     :published-raw nil :posted-at-raw nil
                     :delayed-post-at-raw nil :author-raw nil
                     :allow-comments-raw nil :specific-sections-raw "sec1,sec2"))))
      (expect (plist-get result :canvas-id) :to-equal "42")
      (expect (plist-get result :specific_sections) :to-equal "sec1,sec2"))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--announcement-parse-entry"
  (it "extracts announcement title from heading"
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

  (test-org-canvas-define-common-parse-tests #'org-canvas--announcement-parse-entry)

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

  (it "parses DELAYED_POST_AT timestamp"
    (with-temp-org-buffer
     "* Scheduled Announcement
:PROPERTIES:
:DELAYED_POST_AT: <2024-06-15 Sat 10:00>
:END:

Scheduled content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--announcement-parse-entry)))
       ;; DELAYED_POST_AT should be parsed into delayed_post_at as ISO8601
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

(describe "announcement push-to-api (mocked)"
  (it "uses POST for new announcements"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-id nil))
              (payload '((title . "New"))))
          (org-canvas--push-to-api data payload :endpoint "discussion_topics")
          (expect-api-called 'POST "discussion_topics")))))

  (it "uses PUT for existing announcements"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "123"))
              (payload '((title . "Existing"))))
          (org-canvas--push-to-api data payload :endpoint "discussion_topics")
          (expect-api-called 'PUT "discussion_topics/123"))))))

;;;; Stage 4: Finalize

(describe "announcement finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Announcement
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Announcement" :pom (point-marker)))
           (response '((id . 99999) (title . "Test Announcement"))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "99999"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 88888))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((error . "something failed"))))
       (expect (org-canvas--finalize-item data response) :to-throw 'error)))))

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
        (delete-directory temp-dir t))))

  (it "sends a payload whose body reflects the org input"
    ;; End-to-end: real parse -> build -> push.  Asserts the actual request
    ;; body that reaches the wire, not merely that a POST happened, so a
    ;; broken property->API-key mapping or dropped static field is caught.
    (let ((temp-dir (make-temp-file "announcements-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "announcements.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Welcome
:PROPERTIES:
:PUBLISHED: false
:END:

Hello class.
"))
            (let ((org-canvas-announcements-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-announcements)
                  (let ((body (test-org-canvas-api-call-data
                               'POST "discussion_topics")))
                    (expect (alist-get 'title body) :to-equal "Welcome")
                    (expect (alist-get 'is_announcement body) :to-be t)
                    (expect (alist-get 'discussion_type body)
                            :to-equal "side_comment")
                    ;; PUBLISHED: false must map to the JSON-false sentinel.
                    (expect (alist-get 'published body) :to-be :json-false))))))
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
                           (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                             (setq deleted-count (length items))
                             (cons (length items) (mapcar (lambda (i) (number-to-string (alist-get 'id i))) items)))))
                  (org-canvas-delete-all-announcements)
                  (expect deleted-count :to-equal 1)))))
        (delete-directory temp-dir t)))))

;;;; Push to API Error Path

(describe "announcement push-to-api error path"
  (it "re-signals API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error: 500 Internal Server Error")))))
        (let ((data '(:title "Bad" :canvas-id nil))
              (payload '((title . "Bad"))))
          (expect (org-canvas--push-to-api data payload :endpoint "discussion_topics")
                  :to-throw 'error)))))

  (it "re-signals error for existing announcement update"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error: 403 Forbidden")))))
        (let ((data '(:title "Forbidden" :canvas-id "123"))
              (payload '((title . "Forbidden"))))
          (expect (org-canvas--push-to-api data payload :endpoint "discussion_topics")
                  :to-throw 'error))))))

;;;; Pull Function Tests

(describe "org-canvas--announcement-pull-item"
  (it "sets DELAYED_POST_AT from ISO timestamp"
    (with-pull-property-test #'org-canvas--announcement-pull-item
      '((id . 1) (title . "Announcement") (delayed_post_at . "2026-06-15T09:00:00Z")
        (message . "<p>Hello</p>"))
      "DELAYED_POST_AT" :to-match "<2026-06-15"))

  (it "skips DELAYED_POST_AT when nil"
    (with-pull-property-test #'org-canvas--announcement-pull-item
      '((id . 1) (title . "Announcement") (delayed_post_at . nil)
        (message . "<p>Hello</p>"))
      "DELAYED_POST_AT" :to-be nil))

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

;;;; Delete at Point

(describe "org-canvas-delete-announcement-at-point"
  (it "deletes announcement and clears properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test
:PROPERTIES:
:CANVAS_ID: 42
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas-delete-announcement-at-point)
           (expect-api-called 'DELETE "discussion_topics/42")
           (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
           (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))

  (it "errors when no CANVAS_ID"
    (with-temp-org-buffer
     "* New
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-announcement-at-point) :to-throw 'user-error)))

  (it "aborts when user says no"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test
:PROPERTIES:
:CANVAS_ID: 42
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
           (org-canvas-delete-announcement-at-point)
           (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42")))))))

;;;; Pull Metadata (POSTED_AT, AUTHOR, DELAYED_POST_AT)

(describe "announcement pull metadata"
  (it "emits :POSTED_AT: when posted_at is present"
    (let ((temp (make-temp-file "ann-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert ""))
            (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                       (lambda (&rest _)
                         '(((id . 100) (title . "Hello")
                            (message . "<p>x</p>")
                            (posted_at . "2026-04-01T15:00:00Z")
                            (delayed_post_at . :null)
                            (user . ((display_name . "Tim Ransom"))))))))
              (let ((org-canvas-announcements-file temp))
                (org-canvas-pull-announcements)))
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((s (buffer-string)))
                (expect s :to-match ":POSTED_AT:")
                (expect s :to-match ":AUTHOR:[ \t]+Tim Ransom"))))
        (delete-file temp))))

  (it "emits :DELAYED_POST_AT: only when delayed_post_at is non-null"
    (let ((temp (make-temp-file "ann-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert ""))
            (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                       (lambda (&rest _)
                         '(((id . 200) (title . "Scheduled")
                            (message . "<p>x</p>")
                            (delayed_post_at . "2026-05-01T08:00:00Z")
                            (posted_at . :null)
                            (user . ((display_name . "Tim Ransom"))))))))
              (let ((org-canvas-announcements-file temp))
                (org-canvas-pull-announcements)))
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :to-match ":DELAYED_POST_AT:")))
        (delete-file temp))))

  (it "omits :AUTHOR: when the user field is absent"
    (let ((temp (make-temp-file "ann-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert ""))
            (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                       (lambda (&rest _)
                         '(((id . 300) (title . "Anon")
                            (message . "<p>x</p>")
                            (posted_at . "2026-04-01T15:00:00Z"))))))
              (let ((org-canvas-announcements-file temp))
                (org-canvas-pull-announcements)))
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :not :to-match ":AUTHOR:")))
        (delete-file temp)))))


(describe "announcement pull reads nested fields through the registry (issue #135)"
  (it "writes AUTHOR from user.display_name"
    (with-pull-property-test #'org-canvas--announcement-pull-item
      '((id . 1) (title . "Hi") (message . "<p>x</p>")
        (user . ((display_name . "Prof. Ada"))))
      "AUTHOR" :to-equal "Prof. Ada"))

  (it "leaves ALLOW_COMMENTS implicit when Canvas reports the topic locked"
    (with-pull-property-test #'org-canvas--announcement-pull-item
      '((id . 1) (title . "Hi") (message . "<p>x</p>") (locked . t))
      "ALLOW_COMMENTS" :to-be nil))

  (it "writes ALLOW_COMMENTS true when replies are open"
    (with-pull-property-test #'org-canvas--announcement-pull-item
      '((id . 1) (title . "Hi") (message . "<p>x</p>") (locked . :json-false))
      "ALLOW_COMMENTS" :to-equal "true")))

;;; org-canvas-announcements-test.el ends here
