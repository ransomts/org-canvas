;;; org-canvas-discussions-test.el --- Buttercup tests for discussions  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-discussions)
;; The drift report and the validator read the checkpoint registry entries.
(require 'org-canvas-diff)
(require 'org-canvas-validate)

;;;; Transform (pure, no buffer)

(describe "org-canvas--discussion-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--discussion-transform-props
                   '(:title-raw "Discussion [2/3]" :canvas-id nil
                     :published-raw nil :discussion-type-raw nil
                     :post-first-raw nil :pinned-raw nil :available-from-raw nil
                     :allow-rating-raw nil :only-graders-can-rate-raw nil
                     :sort-by-rating-raw nil :specific-sections nil
                     :grading-type-raw nil :points-raw nil :due-at-raw nil
                     :lock-at-raw nil :assignment-group-id-raw nil
                     :group-category-id-raw nil :rubric-id nil))))
      (expect (plist-get result :title) :to-equal "Discussion")))

  (it "defaults discussion_type to side_comment"
    (let ((result (org-canvas--discussion-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw nil :discussion-type-raw nil
                     :post-first-raw nil :pinned-raw nil :available-from-raw nil
                     :allow-rating-raw nil :only-graders-can-rate-raw nil
                     :sort-by-rating-raw nil :specific-sections nil
                     :grading-type-raw nil :points-raw nil :due-at-raw nil
                     :lock-at-raw nil :assignment-group-id-raw nil
                     :group-category-id-raw nil :rubric-id nil))))
      (expect (plist-get result :discussion_type) :to-equal "side_comment")))

  (it "converts points to number"
    (let ((result (org-canvas--discussion-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw nil :discussion-type-raw nil
                     :post-first-raw nil :pinned-raw nil :available-from-raw nil
                     :allow-rating-raw nil :only-graders-can-rate-raw nil
                     :sort-by-rating-raw nil :specific-sections nil
                     :grading-type-raw "points" :points-raw "10" :due-at-raw nil
                     :lock-at-raw nil :assignment-group-id-raw nil
                     :group-category-id-raw nil :rubric-id nil))))
      (expect (plist-get result :points_possible) :to-equal 10)))

  (it "interprets boolean properties"
    (let ((result (org-canvas--discussion-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :published-raw "false" :discussion-type-raw nil
                     :post-first-raw "true" :pinned-raw "true" :available-from-raw nil
                     :allow-rating-raw "true" :only-graders-can-rate-raw nil
                     :sort-by-rating-raw nil :specific-sections nil
                     :grading-type-raw nil :points-raw nil :due-at-raw nil
                     :lock-at-raw nil :assignment-group-id-raw nil
                     :group-category-id-raw nil :rubric-id nil))))
      (expect (plist-get result :published) :to-be nil)
      (expect (plist-get result :require_initial_post) :to-be t)
      (expect (plist-get result :pinned) :to-be t)
      (expect (plist-get result :allow_rating) :to-be t))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--discussion-parse-entry"
  (describe "common fields"
    (it "extracts discussion title from heading"
      (with-temp-org-buffer
       "* Week 1 Discussion
:PROPERTIES:
:PUBLISHED: true
:END:

Share your thoughts.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :title) :to-equal "Week 1 Discussion"))))

    (test-org-canvas-define-common-parse-tests #'org-canvas--discussion-parse-entry))

  (describe "discussion-specific fields"
    (it "parses discussion_type property"
      (with-temp-org-buffer
       "* Threaded Discussion
:PROPERTIES:
:DISCUSSION_TYPE: threaded
:PUBLISHED: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :discussion_type) :to-equal "threaded"))))

    (it "defaults discussion_type to side_comment"
      (with-temp-org-buffer
       "* Discussion
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :discussion_type) :to-equal "side_comment"))))

    (it "parses post_first property"
      (with-temp-org-buffer
       "* Discussion
:PROPERTIES:
:PUBLISHED: true
:POST_FIRST: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :require_initial_post) :to-be t))))

    (it "parses PINNED property"
      (with-temp-org-buffer
       "* Pinned Discussion
:PROPERTIES:
:PINNED: true
:PUBLISHED: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :pinned) :to-be t))))

    (it "defaults PINNED to nil"
      (with-temp-org-buffer
       "* Normal Discussion
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :pinned) :to-be nil))))

    (it "parses AVAILABLE_FROM as delayed_post_at"
      (with-temp-org-buffer
       "* Delayed Discussion
:PROPERTIES:
:PUBLISHED: true
:AVAILABLE_FROM: <2026-02-01 Sun 08:00>
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :delayed_post_at) :to-match "^2026-02-01T"))))

    (it "parses DUE_AT timestamp"
      (let ((old-tz (getenv "TZ")))
        (unwind-protect
            (progn
              (set-time-zone-rule "UTC")
              (with-temp-org-buffer
               "* Graded Discussion
:PROPERTIES:
:PUBLISHED: true
:GRADING_TYPE: points
:POINTS: 10
:DUE_AT: <2026-03-15 Sun 23:59>
:END:

Content.
"
               (org-back-to-heading)
               (let ((data (org-canvas--discussion-parse-entry)))
                 (expect (plist-get data :due_at) :to-match "^2026-03-15T"))))
          (set-time-zone-rule old-tz))))

    (it "parses LOCK_AT timestamp"
      (let ((old-tz (getenv "TZ")))
        (unwind-protect
            (progn
              (set-time-zone-rule "UTC")
              (with-temp-org-buffer
               "* Discussion
:PROPERTIES:
:PUBLISHED: true
:LOCK_AT: <2026-04-01 Wed 23:59>
:END:

Content.
"
               (org-back-to-heading)
               (let ((data (org-canvas--discussion-parse-entry)))
                 (expect (plist-get data :lock_at) :to-match "^2026-04-01T"))))
          (set-time-zone-rule old-tz))))

    (it "returns nil for absent date properties"
      (with-temp-org-buffer
       "* Simple Discussion
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :delayed_post_at) :to-be nil)
         (expect (plist-get data :due_at) :to-be nil)
         (expect (plist-get data :lock_at) :to-be nil))))

    (it "parses ALLOW_RATING property"
      (with-temp-org-buffer
       "* Rated Discussion
:PROPERTIES:
:PUBLISHED: true
:ALLOW_RATING: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :allow_rating) :to-be t))))

    (it "parses ONLY_GRADERS_CAN_RATE property"
      (with-temp-org-buffer
       "* Grader Rated Discussion
:PROPERTIES:
:PUBLISHED: true
:ONLY_GRADERS_CAN_RATE: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :only_graders_can_rate) :to-be t))))

    (it "parses SORT_BY_RATING property"
      (with-temp-org-buffer
       "* Sorted Discussion
:PROPERTIES:
:PUBLISHED: true
:SORT_BY_RATING: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :sort_by_rating) :to-be t)))))

  (describe "graded discussion fields"
    (it "parses grading properties"
      (with-temp-org-buffer
       "* Graded Discussion
:PROPERTIES:
:PUBLISHED: true
:GRADING_TYPE: points
:POINTS: 25
:END:

Participate for points.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :grading_type) :to-equal "points")
         (expect (plist-get data :points_possible) :to-equal 25))))

    (it "parses GROUP_CATEGORY property"
      (with-temp-org-buffer
       "* Group Discussion
:PROPERTIES:
:PUBLISHED: true
:GROUP_CATEGORY: 42
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :group_category_id) :to-equal 42))))

    (it "parses SPECIFIC_SECTIONS property"
      (with-temp-org-buffer
       "* Sectioned Discussion
:PROPERTIES:
:PUBLISHED: true
:SPECIFIC_SECTIONS: 1,2,3
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--discussion-parse-entry)))
         (expect (plist-get data :specific_sections) :to-equal "1,2,3"))))))

;;;; Stage 2: Build Payload

(describe "org-canvas--discussion-build-payload"
  (describe "required fields"
    (it "includes title in payload"
      (let* ((data '(:title "My Discussion" :message "<p>Topic</p>" :published t
                     :discussion_type "side_comment" :require_initial_post nil))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'title payload) :to-equal "My Discussion")))

    (it "includes message in payload"
      (let* ((data '(:title "Test" :message "<p>Discuss this</p>" :published t
                     :discussion_type "side_comment" :require_initial_post nil))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'message payload) :to-equal "<p>Discuss this</p>"))))

  (describe "discussion options"
    (it "includes discussion_type"
      (let* ((data '(:title "Test" :message "" :published t
                     :discussion_type "threaded" :require_initial_post nil))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'discussion_type payload) :to-equal "threaded")))

    (it "includes require_initial_post"
      (let* ((data '(:title "Test" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post t))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'require_initial_post payload) :to-be t)))

    (it "includes pinned when true"
      (let* ((data '(:title "Pinned" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :pinned t))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'pinned payload) :to-be t)))

    (it "does not include pinned when nil"
      (let* ((data '(:title "Normal" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :pinned nil))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'pinned payload) :to-be nil))))

  (describe "scheduling"
    (it "includes delayed_post_at at top level"
      (let* ((data '(:title "Delayed" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :delayed_post_at "2026-02-01T08:00:00Z"))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'delayed_post_at payload) :to-equal "2026-02-01T08:00:00Z")))

    (it "includes lock_at for non-graded discussions"
      (let* ((data '(:title "Locking" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :lock_at "2026-04-01T23:59:00Z"))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'lock_at payload) :to-equal "2026-04-01T23:59:00Z")))

    (it "puts dates in assignment sub-object for graded discussions"
      (let* ((data '(:title "Graded" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :points_possible 20 :grading_type "points"
                     :due_at "2026-03-15T23:59:00Z"
                     :lock_at "2026-04-01T23:59:00Z"
                     :delayed_post_at "2026-02-01T08:00:00Z"))
             (payload (org-canvas--discussion-build-payload data))
             (assignment (alist-get 'assignment payload)))
        (expect (alist-get 'due_at assignment) :to-equal "2026-03-15T23:59:00Z")
        (expect (alist-get 'lock_at assignment) :to-equal "2026-04-01T23:59:00Z")
        (expect (alist-get 'unlock_at assignment) :to-equal "2026-02-01T08:00:00Z"))))

  (describe "grading"
    (it "includes assignment for graded discussions"
      (let* ((data '(:title "Graded" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :points_possible 20 :grading_type "points"))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'assignment payload) :to-be-truthy)
        (expect (alist-get 'points_possible (alist-get 'assignment payload)) :to-equal 20)))

    (it "includes assignment_group_id for graded discussions"
      (let* ((data '(:title "Grouped" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :points_possible 10 :grading_type "points"
                     :assignment_group_id 42))
             (payload (org-canvas--discussion-build-payload data))
             (assignment (alist-get 'assignment payload)))
        (expect (alist-get 'assignment_group_id assignment) :to-equal 42)))

    (it "includes group_category_id when present"
      (let* ((data '(:title "Grouped" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :group_category_id 99))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'group_category_id payload) :to-equal 99)))

    (it "resolves section names to IDs"
      (let ((sections-file (make-temp-file "test-sections" nil ".org")))
        (unwind-protect
            (progn
              (with-temp-file sections-file
                (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
              (let* ((org-canvas-sections-file sections-file)
                     (data '(:title "Sectioned" :message "" :published t
                             :discussion_type "side_comment" :require_initial_post nil
                             :specific_sections "Section A"))
                     (payload (org-canvas--discussion-build-payload data)))
                (expect (alist-get 'specific_sections payload) :to-equal "100")))
          (let ((buf (find-buffer-visiting sections-file)))
            (when buf (kill-buffer buf)))
          (delete-file sections-file))))

    (it "passes through numeric IDs in specific_sections"
      (let ((sections-file (make-temp-file "test-sections" nil ".org")))
        (unwind-protect
            (progn
              (with-temp-file sections-file
                (insert "#+TITLE: Sections\n"))
              (let* ((org-canvas-sections-file sections-file)
                     (data '(:title "Sectioned" :message "" :published t
                             :discussion_type "side_comment" :require_initial_post nil
                             :specific_sections "1,2,3"))
                     (payload (org-canvas--discussion-build-payload data)))
                (expect (alist-get 'specific_sections payload) :to-equal "1,2,3")))
          (let ((buf (find-buffer-visiting sections-file)))
            (when buf (kill-buffer buf)))
          (delete-file sections-file)))))

  (describe "rating options"
    (it "includes allow_rating when true"
      (let* ((data '(:title "Rated" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :allow_rating t))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'allow_rating payload) :to-be t)))

    (it "does not include allow_rating when nil"
      (let* ((data '(:title "Unrated" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :allow_rating nil))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'allow_rating payload) :to-be nil)))

    (it "includes only_graders_can_rate when true"
      (let* ((data '(:title "Grader Rated" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :only_graders_can_rate t))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'only_graders_can_rate payload) :to-be t)))

    (it "includes sort_by_rating when true"
      (let* ((data '(:title "Sorted" :message "" :published t
                     :discussion_type "side_comment" :require_initial_post nil
                     :sort_by_rating t))
             (payload (org-canvas--discussion-build-payload data)))
        (expect (alist-get 'sort_by_rating payload) :to-be t)))))

;;;; Stage 3: Push to API (mocked)

(describe "discussion push-to-api (mocked)"
  (it "uses POST for new discussions"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-id nil))
              (payload '((title . "New"))))
          (org-canvas--push-to-api data payload :endpoint "discussion_topics")
          (expect-api-called 'POST "discussion_topics")))))

  (it "uses PUT for existing discussions"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "456"))
              (payload '((title . "Existing"))))
          (org-canvas--push-to-api data payload :endpoint "discussion_topics")
          (expect-api-called 'PUT "discussion_topics/456")))))

  (it "sends a payload whose body reflects the org input"
    (let ((temp-dir (make-temp-file "discussions-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "discussions.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Topic\n:PROPERTIES:\n:DISCUSSION_TYPE: threaded\n:POST_FIRST: true\n:END:\n\nDiscuss this.\n"))
            (let ((org-canvas-discussions-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-discussions)
                  (let ((body (test-org-canvas-api-call-data 'POST "discussion_topics")))
                    (expect (alist-get 'title body) :to-equal "Topic")
                    (expect (alist-get 'discussion_type body) :to-equal "threaded")
                    (expect (alist-get 'require_initial_post body) :to-be t))))))
        (delete-directory temp-dir t)))))

;;;; Stage 4: Finalize

(describe "discussion finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Discussion
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Discussion" :pom (point-marker)))
           (response '((id . 77777) (title . "Test Discussion"))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "77777"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 66666))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)))))

;;;; Validation Tests

(describe "DISCUSSION_TYPE validation"
  (it "falls back to default for invalid discussion type"
    (with-temp-org-buffer
     "* Discussion
:PROPERTIES:
:DISCUSSION_TYPE: invalid_type
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--discussion-parse-entry)))
       (expect (plist-get data :discussion_type) :to-equal "side_comment")))))

;;;; Pull Function Tests

(describe "org-canvas--discussion-pull-item"
  (it "sets DISCUSSION_TYPE property"
    (with-pull-property-test #'org-canvas--discussion-pull-item
      '((id . 1) (title . "Discussion") (discussion_type . "threaded")
        (message . "<p>Discuss</p>"))
      "DISCUSSION_TYPE" :to-equal "threaded"))

  (it "writes delayed_post_at to AVAILABLE_FROM, the property push reads (issue #135)"
    (with-pull-property-test #'org-canvas--discussion-pull-item
      '((id . 1) (title . "Discussion") (delayed_post_at . "2026-06-15T09:00:00Z")
        (message . "<p>Later</p>"))
      "AVAILABLE_FROM" :to-match "<2026-06-15"))

  (it "never writes DELAYED_POST_AT, which no discussion push reads"
    (with-pull-property-test #'org-canvas--discussion-pull-item
      '((id . 1) (title . "Discussion") (delayed_post_at . "2026-06-15T09:00:00Z")
        (message . "<p>Later</p>"))
      "DELAYED_POST_AT" :to-be nil))

  (it "inserts body text"
    (with-temp-org-buffer
     "* Discussion
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas--html-to-org)
                (lambda (html) (replace-regexp-in-string "<[^>]+>" "" html))))
       (org-canvas--discussion-pull-item
        '((id . 1) (title . "Disc")
          (message . "<p>Talk about this</p>"))
        (point))
       (expect (buffer-string) :to-match "Talk about this"))))

  (it "sets ALLOW_RATING property"
    (with-pull-property-test #'org-canvas--discussion-pull-item
      '((id . 1) (title . "Discussion") (allow_rating . t)
        (message . "<p>Content</p>"))
      "ALLOW_RATING" :to-equal "true"))

  (it "sets ONLY_GRADERS_CAN_RATE property"
    (with-pull-property-test #'org-canvas--discussion-pull-item
      '((id . 1) (title . "Discussion") (only_graders_can_rate . t)
        (message . "<p>Content</p>"))
      "ONLY_GRADERS_CAN_RATE" :to-equal "true"))

  (it "sets SORT_BY_RATING property"
    (with-pull-property-test #'org-canvas--discussion-pull-item
      '((id . 1) (title . "Discussion") (sort_by_rating . t)
        (message . "<p>Content</p>"))
      "SORT_BY_RATING" :to-equal "true")))

;;;; Parse Entry GROUP Link Resolution

(describe "org-canvas--discussion-parse-entry GROUP link"
  (it "resolves GROUP link to assignment_group_id"
    (let ((groups-file (make-temp-file "test-groups" nil ".org"))
          (disc-file (make-temp-file "test-disc" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Discussions
:PROPERTIES:
:CANVAS_ID: 888
:END:
"))
            (with-temp-file disc-file
              (insert (format "* Graded Discussion
:PROPERTIES:
:PUBLISHED: true
:GRADING_TYPE: points
:POINTS: 10
:GROUP: [[file:%s::*Discussions]]
:END:

Content.
" groups-file)))
            (let ((org-canvas-discussions-file disc-file))
              (with-current-buffer (find-file-noselect disc-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--discussion-parse-entry)))
                  (expect (plist-get data :assignment_group_id) :to-equal 888)))))
        (let ((buf (find-buffer-visiting groups-file)))
          (when buf (kill-buffer buf)))
        (let ((buf (find-buffer-visiting disc-file)))
          (when buf (kill-buffer buf)))
        (delete-file groups-file)
        (delete-file disc-file)))))

;;;; RUBRIC_LINK Tests

(describe "org-canvas--discussion-parse-entry RUBRIC_LINK"
  (it "resolves RUBRIC_LINK to rubric-id"
    (let ((rubrics-file (make-temp-file "test-rubrics" nil ".org"))
          (disc-file (make-temp-file "test-disc" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file rubrics-file
              (insert "* Essay Rubric\n:PROPERTIES:\n:CANVAS_ID: 555\n:END:\n"))
            (with-temp-file disc-file
              (insert (format "* Graded Discussion\n:PROPERTIES:\n:PUBLISHED: true\n:RUBRIC_LINK: [[file:%s::*Essay Rubric][Essay Rubric]]\n:END:\n\nContent.\n"
                              rubrics-file)))
            (let ((org-canvas-discussions-file disc-file))
              (with-current-buffer (find-file-noselect disc-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--discussion-parse-entry)))
                  (expect (plist-get data :rubric-id) :to-equal "555")))))
        (let ((buf (find-buffer-visiting rubrics-file)))
          (when buf (kill-buffer buf)))
        (let ((buf (find-buffer-visiting disc-file)))
          (when buf (kill-buffer buf)))
        (delete-file rubrics-file)
        (delete-file disc-file))))

  (it "returns nil rubric-id when no RUBRIC_LINK"
    (with-temp-org-buffer
     "* Plain Discussion
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--discussion-parse-entry)))
       (expect (plist-get data :rubric-id) :to-be nil)))))

(describe "org-canvas--discussion-post-finalize"
  (it "calls associate-rubric when rubric-id present"
    (let (called-args)
      (cl-letf (((symbol-function 'org-canvas--associate-rubric)
                 (lambda (item-id rubric-id assoc-type)
                   (setq called-args (list item-id rubric-id assoc-type)))))
        (org-canvas--discussion-post-finalize
         '(:title "Test" :rubric-id "99")
         '((id . 42)))
        (expect called-args :to-equal '(42 "99" "Discussion")))))

  (it "does not call associate-rubric when no rubric-id"
    (let ((called nil))
      (cl-letf (((symbol-function 'org-canvas--associate-rubric)
                 (lambda (&rest _) (setq called t))))
        (org-canvas--discussion-post-finalize
         '(:title "Test" :rubric-id nil)
         '((id . 42)))
        (expect called :to-be nil))))

  (it "reports the association so the baseline is re-read (issue #124)"
    (cl-letf (((symbol-function 'org-canvas--associate-rubric)
               (lambda (&rest _) t)))
      (let ((ctx (org-canvas--sync-make-ctx)))
        (org-canvas--discussion-post-finalize
         '(:title "Test" :rubric-id "99") '((id . 42)) ctx)
        (expect (plist-get ctx :remote-touched) :to-be t))))

  (it "reports nothing when the association failed"
    (cl-letf (((symbol-function 'org-canvas--associate-rubric)
               (lambda (&rest _) nil)))
      (let ((ctx (org-canvas--sync-make-ctx)))
        (org-canvas--discussion-post-finalize
         '(:title "Test" :rubric-id "99") '((id . 42)) ctx)
        (expect (plist-get ctx :remote-touched) :to-be nil)))))

;;;; Delete at Point

(describe "org-canvas-delete-discussion-at-point"
  (it "deletes discussion and clears properties"
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
           (org-canvas-delete-discussion-at-point)
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
     (expect (org-canvas-delete-discussion-at-point) :to-throw 'user-error)))

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
           (org-canvas-delete-discussion-at-point)
           (expect (org-entry-get (point) "CANVAS_ID") :to-equal "42")))))))

;;;; The at-point push consults the search (issue #179)

(describe "org-canvas-sync-discussion-at-point duplicate guard (issue #179)"
  (it "adopts the item Canvas holds under the title instead of creating a second"
    (with-org-canvas-test-config
      (let ((requests nil)
            (errors nil)
            (org-canvas-duplicate-title-strategy 'adopt)
            (org-canvas-detect-conflicts nil))
        (cl-letf (((symbol-function 'org-canvas--log-error)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) errors)))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _)
                     (push (list method url) requests)
                     (pcase method
                       ('GET [((id . 77) (title . "Debate") (name . "Debate")
                               (updated_at . "2026-01-01T00:00:00Z"))])
                       ('PUT '((id . 77) (title . "Debate") (name . "Debate")
                               (updated_at . "2026-01-02T00:00:00Z")))
                       (_ '((id . 900) (title . "Debate") (name . "Debate"))))))
                  ((symbol-function 'display-buffer) #'ignore))
          (with-temp-org-buffer "* Debate\nTake a side.\n"
            (goto-char (point-min))
            (search-forward "Debate")
            (org-back-to-heading t)
            (org-canvas-sync-discussion-at-point)
            (expect errors :to-be nil)
            (expect (cl-some (lambda (r) (eq (car r) 'POST)) requests) :to-be nil)
            (expect (cl-some (lambda (r) (and (eq (car r) 'PUT)
                                              (string-match-p "discussion_topics/77$" (cadr r))))
                             requests)
                    :to-be-truthy)
            (goto-char (point-min))
            (search-forward "Debate")
            (org-back-to-heading t)
            (expect (org-entry-get (point) "CANVAS_ID") :to-equal "77")))))))


;;;; Checkpoints (issue #216)

(defun test-discussions--checkpoint-node (id topic-points topic-due-at reply-points reply-due-at required)
  "A GraphQL discussion node ID with the two checkpoints.
TOPIC-POINTS and TOPIC-DUE-AT describe reply_to_topic, REPLY-POINTS
and REPLY-DUE-AT reply_to_entry, REQUIRED the replies required."
  `((_id . ,id) (replyToEntryRequiredCount . ,required)
    (checkpoints . [((tag . "reply_to_topic") (pointsPossible . ,topic-points) (dueAt . ,topic-due-at))
                    ((tag . "reply_to_entry") (pointsPossible . ,reply-points) (dueAt . ,reply-due-at))])))

(defun test-discussions--checkpoint-page (nodes &optional next-cursor)
  "A GraphQL reply page holding NODES, pointing at NEXT-CURSOR when given."
  `((course . ((discussionsConnection
                . ((pageInfo . ((hasNextPage . ,(if next-cursor t :json-false))
                                (endCursor . ,(or next-cursor :null))))
                   (nodes . ,(vconcat nodes))))))))

(defmacro test-discussions--with-checkpoint-pages (pages &rest body)
  "Run BODY with `org-canvas--graphql-query' answering PAGES in turn.
The variables of each request are collected in `asked', newest first,
and the cache is empty before and after."
  (declare (indent 1))
  `(let ((remaining ,pages) (asked nil))
     (ignore asked)
     (org-canvas--discussion-checkpoints-forget)
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas--graphql-query)
                      (lambda (_document variables)
                        (push variables asked)
                        (or (pop remaining) (error "No more pages")))))
             ,@body))
       (org-canvas--discussion-checkpoints-forget))))

(describe "discussion checkpoints: parse and payload (issue #216)"
  (it "reads nothing when no CHECKPOINT_ property is written"
    (expect (org-canvas--discussion-transform-checkpoints '(:points-raw "10")) :to-be nil))

  (it "reads the five properties, the replies required defaulting to 1"
    (let ((checkpoints (org-canvas--discussion-transform-checkpoints
                        '(:checkpoint-topic-points-raw "6"
                          :checkpoint-topic-due-at-raw "<2026-10-01 Thu 23:59>"
                          :checkpoint-reply-points-raw "4"
                          :checkpoint-reply-due-at-raw "<2026-10-03 Sat 23:59>"))))
      (expect (plist-get checkpoints :topic-points) :to-equal 6)
      (expect (plist-get checkpoints :reply-points) :to-equal 4)
      (expect (plist-get checkpoints :topic-due-at) :to-match "\\`2026-10-0[12]T")
      (expect (plist-get checkpoints :reply-due-at) :to-match "\\`2026-10-0[34]T")
      (expect (plist-get checkpoints :replies-required) :to-equal 1))
    (expect (plist-get (org-canvas--discussion-transform-checkpoints
                        '(:checkpoint-replies-required-raw "3"))
                       :replies-required)
            :to-equal 3))

  (it "grades a checkpointed discussion out of the sum unless POINTS says otherwise"
    (let ((summed (org-canvas--discussion-transform-props
                   '(:title-raw "T" :checkpoint-topic-points-raw "6"
                     :checkpoint-reply-points-raw "4")))
          (explicit (org-canvas--discussion-transform-props
                     '(:title-raw "T" :points-raw "12" :checkpoint-topic-points-raw "6"
                       :checkpoint-reply-points-raw "4")))
          (plain (org-canvas--discussion-transform-props '(:title-raw "T"))))
      (expect (plist-get summed :points_possible) :to-equal 10)
      (expect (plist-get (plist-get summed :checkpoints) :topic-points) :to-equal 6)
      (expect (plist-get explicit :points_possible) :to-equal 12)
      (expect (plist-get plain :checkpoints) :to-be nil)
      (expect (plist-get plain :points_possible) :to-be nil)))

  (it "parses the properties from a heading"
    (with-temp-org-buffer
     "* Debate
:PROPERTIES:
:CHECKPOINT_TOPIC_POINTS: 6
:CHECKPOINT_REPLY_POINTS: 4
:CHECKPOINT_REPLIES_REQUIRED: 2
:END:
Take a side.
"
     (org-back-to-heading)
     (let ((data (org-canvas--discussion-parse-entry)))
       (expect (plist-get (plist-get data :checkpoints) :replies-required) :to-equal 2)
       (expect (plist-get data :points_possible) :to-equal 10))))

  (it "never carries the checkpoints in the REST payload, but does carry the assignment"
    (let* ((data (org-canvas--discussion-transform-props
                  '(:title-raw "T" :checkpoint-topic-points-raw "6"
                    :checkpoint-reply-points-raw "4")))
           (json (json-encode (org-canvas--discussion-build-payload
                               (append data '(:message "<p>x</p>"))))))
      (expect json :not :to-match "heckpoint")
      (expect json :to-match "\"assignment\"")
      (expect json :to-match "\"points_possible\":10")))

  (it "folds the checkpoints into the hash, and only them"
    (let ((a (org-canvas--discussion-hash-extra
              '(:checkpoints (:topic-points 6 :topic-due-at "2026-10-01T23:59:00Z"
                              :reply-points 4 :reply-due-at nil :replies-required 1))))
          (b (org-canvas--discussion-hash-extra
              '(:checkpoints (:topic-points 6 :topic-due-at "2026-10-02T23:59:00Z"
                              :reply-points 4 :reply-due-at nil :replies-required 1)))))
      (expect a :not :to-equal b)
      (expect (org-canvas--discussion-hash-extra '(:title "T")) :to-equal ""))))

(describe "discussion checkpoints: the course-wide read (issue #216)"
  (it "turns a node into a plist and answers nil for a node without checkpoints"
    (let ((cp (org-canvas--discussion-checkpoints-from-node
               (test-discussions--checkpoint-node 7 6 "2026-10-01T23:59:00Z" 4 :null 2))))
      (expect (plist-get cp :topic-points) :to-equal 6)
      (expect (plist-get cp :topic-due-at) :to-equal "2026-10-01T23:59:00Z")
      (expect (plist-get cp :reply-points) :to-equal 4)
      (expect (plist-get cp :reply-due-at) :to-be nil)
      (expect (plist-get cp :replies-required) :to-equal 2))
    (expect (org-canvas--discussion-checkpoints-from-node
             '((_id . 8) (replyToEntryRequiredCount . 0) (checkpoints . [])))
            :to-be nil))

  (it "follows the cursor across pages, keys the map by topic id, and reads once"
    (test-discussions--with-checkpoint-pages
        (list (test-discussions--checkpoint-page
               (list (test-discussions--checkpoint-node 7 6 nil 4 nil 1)
                     '((_id . 8) (replyToEntryRequiredCount . 0) (checkpoints . [])))
               "cursor-1")
              (test-discussions--checkpoint-page
               (list (test-discussions--checkpoint-node 9 3 "2026-10-01T23:59:00Z" 2 nil 1))))
      (let ((item-7 '((id . 7) (assignment_id . 70)))
            (item-8 '((id . 8) (assignment_id . 80)))
            (item-9 '((id . 9) (assignment_id . 90))))
        (expect (org-canvas--discussion-remote-checkpoint-topic-points item-7) :to-equal 6)
        (expect (org-canvas--discussion-remote-checkpoint-reply-points item-9) :to-equal 2)
        (expect (org-canvas--discussion-remote-checkpoint-topic-due-at item-9)
                :to-equal "2026-10-01T23:59:00Z")
        (expect (org-canvas--discussion-remote-checkpoint-reply-due-at item-9) :to-be nil)
        (expect (org-canvas--discussion-remote-checkpoint-replies-required item-7) :to-equal 1)
        (expect (org-canvas--discussion-remote-checkpoint-topic-points item-8) :to-be nil)
        (expect (org-canvas--discussion-checkpoints-known-p item-8) :to-be-truthy)
        (expect (length asked) :to-equal 2)
        (expect (alist-get 'cursor (car asked)) :to-equal "cursor-1")
        (expect (alist-get 'cursor (cadr asked)) :to-be nil))))

  (it "never asks Canvas for an ungraded topic"
    (test-discussions--with-checkpoint-pages nil
      (expect (org-canvas--discussion-checkpoints-known-p '((id . 7))) :to-be nil)
      (expect (org-canvas--discussion-remote-checkpoint-topic-points '((id . 7))) :to-be nil)
      (expect asked :to-be nil)))

  (it "answers refused after one warning when the read fails, and leaves nothing known"
    (let ((warned nil))
      (org-canvas--discussion-checkpoints-forget)
      (unwind-protect
          (with-org-canvas-test-config
            (cl-letf (((symbol-function 'org-canvas--graphql-query)
                       (lambda (&rest _) (signal 'org-canvas-permission-error (list "403"))))
                      ((symbol-function 'org-canvas--log-warning)
                       (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
              (expect (org-canvas--discussion-checkpoints-known-p '((id . 7) (assignment_id . 70)))
                      :to-be nil)
              (expect (org-canvas--discussion-checkpoints-comparable-p nil '((id . 7) (assignment_id . 70)))
                      :to-be nil)
              (expect (org-canvas--discussion-checkpoints-known-p '((id . 8) (assignment_id . 80)))
                      :to-be nil)
              (expect (length warned) :to-equal 1)
              (expect (car warned) :to-match "Could not read the discussion checkpoints")))
        (org-canvas--discussion-checkpoints-forget))))

  (it "is forgotten when a command starts, log cleared or not"
    (setq org-canvas--discussion-checkpoints-cache (cons "x" (make-hash-table)))
    (let ((org-canvas--inhibit-log-clear t))
      (org-canvas-clear-log))
    (expect org-canvas--discussion-checkpoints-cache :to-be nil)))

(describe "discussion checkpoints: pull and drift (issue #216)"
  (it "writes the five properties from the map, the default replies required left implicit"
    (test-discussions--with-checkpoint-pages
        (list (test-discussions--checkpoint-page
               (list (test-discussions--checkpoint-node
                      7 6 "2026-10-01T23:59:00Z" 4 "2026-10-03T23:59:00Z" 1))))
      (with-temp-org-buffer "* Debate\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n"
        (org-back-to-heading)
        (with-html-to-org-identity
          (org-canvas--discussion-pull-item
           '((id . 7) (assignment_id . 70) (title . "Debate") (message . "<p>x</p>"))
           (point)))
        (expect (org-entry-get (point) "CHECKPOINT_TOPIC_POINTS") :to-equal "6")
        (expect (org-entry-get (point) "CHECKPOINT_TOPIC_DUE_AT") :to-match "<2026-10-0[12]")
        (expect (org-entry-get (point) "CHECKPOINT_REPLY_POINTS") :to-equal "4")
        (expect (org-entry-get (point) "CHECKPOINT_REPLY_DUE_AT") :to-match "<2026-10-0[34]")
        (expect (org-entry-get (point) "CHECKPOINT_REPLIES_REQUIRED") :to-be nil))))

  (it "removes the properties of a topic whose checkpoints are gone"
    (test-discussions--with-checkpoint-pages
        (list (test-discussions--checkpoint-page
               (list '((_id . 7) (replyToEntryRequiredCount . 0) (checkpoints . [])))))
      (with-temp-org-buffer "* Debate\n:PROPERTIES:\n:CANVAS_ID: 7\n:CHECKPOINT_TOPIC_POINTS: 6\n:CHECKPOINT_REPLY_POINTS: 4\n:END:\n"
        (org-back-to-heading)
        (with-html-to-org-identity
          (org-canvas--discussion-pull-item
           '((id . 7) (assignment_id . 70) (title . "Debate") (message . "<p>x</p>"))
           (point)))
        (expect (org-entry-get (point) "CHECKPOINT_TOPIC_POINTS") :to-be nil)
        (expect (org-entry-get (point) "CHECKPOINT_REPLY_POINTS") :to-be nil))))

  (it "leaves the properties alone when the read was refused"
    (org-canvas--discussion-checkpoints-forget)
    (unwind-protect
        (with-org-canvas-test-config
          (cl-letf (((symbol-function 'org-canvas--graphql-query)
                     (lambda (&rest _) (signal 'org-canvas-api-error (list "500"))))
                    ((symbol-function 'org-canvas--log-warning) #'ignore))
            (with-temp-org-buffer "* Debate\n:PROPERTIES:\n:CANVAS_ID: 7\n:CHECKPOINT_TOPIC_POINTS: 6\n:END:\n"
              (org-back-to-heading)
              (with-html-to-org-identity
                (org-canvas--discussion-pull-item
                 '((id . 7) (assignment_id . 70) (title . "Debate") (message . "<p>x</p>"))
                 (point)))
              (expect (org-entry-get (point) "CHECKPOINT_TOPIC_POINTS") :to-equal "6"))))
      (org-canvas--discussion-checkpoints-forget)))

  (it "reports drift on a declared checkpoint property and none on a refused read"
    (let ((specs (cl-remove-if-not
                  (lambda (spec) (string-prefix-p "CHECKPOINT_" (plist-get spec :org-prop)))
                  (plist-get (gethash "discussions" org-canvas--property-registry) :properties))))
      (test-discussions--with-checkpoint-pages
          (list (test-discussions--checkpoint-page
                 (list (test-discussions--checkpoint-node 7 10 nil 4 nil 1))))
        (with-temp-org-buffer "* Debate\n:PROPERTIES:\n:CANVAS_ID: 7\n:CHECKPOINT_TOPIC_POINTS: 6\n:CHECKPOINT_REPLY_POINTS: 4\n:END:\n"
          (org-back-to-heading)
          (expect (org-canvas--diff-compare-fields specs (point) '((id . 7) (assignment_id . 70)))
                  :to-equal '(("CHECKPOINT_TOPIC_POINTS" "6" "10")))))
      (org-canvas--discussion-checkpoints-forget)
      (unwind-protect
          (with-org-canvas-test-config
            (cl-letf (((symbol-function 'org-canvas--graphql-query)
                       (lambda (&rest _) (signal 'org-canvas-api-error (list "500"))))
                      ((symbol-function 'org-canvas--log-warning) #'ignore))
              (with-temp-org-buffer "* Debate\n:PROPERTIES:\n:CANVAS_ID: 7\n:CHECKPOINT_TOPIC_POINTS: 6\n:END:\n"
                (org-back-to-heading)
                (expect (org-canvas--diff-compare-fields specs (point) '((id . 7) (assignment_id . 70)))
                        :to-be nil))))
        (org-canvas--discussion-checkpoints-forget)))))

(describe "discussion checkpoints: push (issue #216)"
  (it "sets the checkpoints through the mutation from finalize and notes the remote write"
    (with-org-canvas-test-config
      (let ((seen nil) (ctx (org-canvas--sync-make-ctx)))
        (setq org-canvas--discussion-checkpoints-cache (cons "x" (make-hash-table)))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (what doc vars) (setq seen (list what doc vars)) nil))
                  ((symbol-function 'org-canvas--associate-rubric) #'ignore))
          (org-canvas--discussion-post-finalize
           '(:title "Debate"
             :checkpoints (:topic-points 6 :topic-due-at "2026-10-01T23:59:00Z"
                           :reply-points 4 :reply-due-at nil :replies-required 2))
           '((id . 7)) ctx))
        (expect (nth 0 seen) :to-match "checkpoints of 'Debate'")
        (expect (nth 1 seen) :to-match "updateDiscussionTopic")
        (expect (nth 1 seen) :to-match "setCheckpoints: true")
        (let* ((vars (nth 2 seen))
               (inputs (append (alist-get 'checkpoints vars) nil))
               (topic (nth 0 inputs))
               (reply (nth 1 inputs)))
          (expect (alist-get 'topicId vars) :to-equal "7")
          (expect (alist-get 'checkpointLabel topic) :to-equal "reply_to_topic")
          (expect (alist-get 'pointsPossible topic) :to-equal 6)
          (expect (append (alist-get 'dates topic) nil)
                  :to-equal '(((type . "everyone") (dueAt . "2026-10-01T23:59:00Z"))))
          (expect (assq 'repliesRequired topic) :to-be nil)
          (expect (alist-get 'checkpointLabel reply) :to-equal "reply_to_entry")
          (expect (alist-get 'pointsPossible reply) :to-equal 4)
          (expect (append (alist-get 'dates reply) nil) :to-equal '(((type . "everyone"))))
          (expect (alist-get 'repliesRequired reply) :to-equal 2)
          (expect (json-encode (alist-get 'checkpoints vars)) :to-match "\\`\\["))
        (expect (plist-get ctx :remote-touched) :to-be t)
        (expect org-canvas--discussion-checkpoints-cache :to-be nil))))

  (it "sends nothing and notes nothing without checkpoints"
    (let ((sent nil) (ctx (org-canvas--sync-make-ctx)))
      (cl-letf (((symbol-function 'org-canvas--graphql-mutate) (lambda (&rest _) (setq sent t)))
                ((symbol-function 'org-canvas--associate-rubric) #'ignore))
        (org-canvas--discussion-post-finalize '(:title "Debate") '((id . 7)) ctx))
      (expect sent :to-be nil)
      (expect (plist-get ctx :remote-touched) :to-be nil)))

  (it "fails the entry with Canvas's words when the mutation answers validation errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                 (lambda (&rest _)
                   '((updateDiscussionTopic
                      . ((discussionTopic . :null)
                         (errors . [((message . "Checkpoints are not enabled"))])))))))
        (expect (condition-case err
                    (org-canvas--discussion-push-checkpoints
                     '(:title "Debate" :checkpoints (:topic-points 6 :reply-points 4)) 7)
                  (org-canvas-api-error (error-message-string err)))
                :to-match "Checkpoints of 'Debate': Checkpoints are not enabled"))))

  (it "previews the mutation under a dry run and sends nothing"
    (with-org-canvas-test-config
      (let ((org-canvas--dry-run t) (sent nil) (lines nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-send) (lambda (&rest _) (setq sent t)))
                  ((symbol-function 'org-canvas--log-info)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) lines))))
          (expect (org-canvas--discussion-push-checkpoints
                   '(:title "Debate" :checkpoints (:topic-points 6 :reply-points 4)) 7)
                  :to-be t))
        (expect sent :to-be nil)
        (expect (cl-some (lambda (l) (string-match-p "\\[DRY-RUN\\] Would set the checkpoints of 'Debate'" l)) lines)
                :to-be-truthy)))))

(defun test-discussions--checkpoint-issues (drawer)
  "Return the validator's issues for a discussion heading with DRAWER."
  (with-temp-org-buffer (format "* Debate\n:PROPERTIES:\n%s:END:\n" drawer)
    (org-back-to-heading)
    (org-canvas--validate-discussion-checkpoints
     '(:file "discussions.org" :line 1 :heading "Debate"))))

(describe "discussion checkpoints: validation (issue #216)"
  (it "says nothing about a discussion without checkpoints"
    (expect (test-discussions--checkpoint-issues ":POINTS: 10\n:DUE_AT: <2026-10-01 Thu>\n") :to-be nil))

  (it "requires both point values"
    (let ((issues (test-discussions--checkpoint-issues ":CHECKPOINT_TOPIC_DUE_AT: <2026-10-01 Thu>\n")))
      (expect (mapcar (lambda (i) (plist-get i :property)) issues)
              :to-equal '("CHECKPOINT_TOPIC_POINTS" "CHECKPOINT_REPLY_POINTS"))
      (expect (plist-get (car issues) :severity) :to-be 'error)))

  (it "rejects DUE_AT beside checkpoints and warns when POINTS is not the sum"
    (let ((issues (test-discussions--checkpoint-issues
                   ":CHECKPOINT_TOPIC_POINTS: 6\n:CHECKPOINT_REPLY_POINTS: 4\n:POINTS: 12\n:DUE_AT: <2026-10-01 Thu>\n")))
      (expect (mapcar (lambda (i) (list (plist-get i :severity) (plist-get i :property))) issues)
              :to-equal '((error "DUE_AT") (warning "POINTS")))
      (expect (plist-get (cadr issues) :message) :to-match "add up to 10")))

  (it "accepts a well-formed checkpointed discussion"
    (expect (test-discussions--checkpoint-issues
             ":CHECKPOINT_TOPIC_POINTS: 6\n:CHECKPOINT_REPLY_POINTS: 4\n:POINTS: 10\n:CHECKPOINT_REPLIES_REQUIRED: 2\n")
            :to-be nil))

  (it "warns when the reply checkpoint is due before the topic one"
    (with-temp-org-buffer "* Debate\n:PROPERTIES:\n:CHECKPOINT_TOPIC_DUE_AT: <2026-10-05 Mon>\n:CHECKPOINT_REPLY_DUE_AT: <2026-10-01 Thu>\n:END:\n"
      (org-back-to-heading)
      (let ((issues (org-canvas--validate-check-date-order
                     (plist-get (gethash "discussions" org-canvas--property-registry) :date-order)
                     '(:file "discussions.org" :line 1 :heading "Debate"))))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :property) :to-equal "CHECKPOINT_TOPIC_DUE_AT"))))

  (it "is wired as the discussions registry's structural check"
    (expect (plist-get (gethash "discussions" org-canvas--property-registry) :structural-fn)
            :to-be #'org-canvas--validate-discussion-checkpoints)))

;;; org-canvas-discussions-test.el ends here
