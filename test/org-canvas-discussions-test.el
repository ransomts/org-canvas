;;; org-canvas-discussions-test.el --- Buttercup tests for discussions  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-discussions)

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
          (expect-api-called 'PUT "discussion_topics/456"))))))

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

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 66666))))
       (org-canvas--finalize-item data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-")))))

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

  (it "sets DELAYED_POST_AT when present"
    (with-pull-property-test #'org-canvas--discussion-pull-item
      '((id . 1) (title . "Discussion") (delayed_post_at . "2026-06-15T09:00:00Z")
        (message . "<p>Later</p>"))
      "DELAYED_POST_AT" :to-match "<2026-06-15"))

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
        (expect called :to-be nil)))))

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

;;; org-canvas-discussions-test.el ends here
