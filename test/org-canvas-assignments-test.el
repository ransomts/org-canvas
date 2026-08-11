;;; org-canvas-assignments-test.el --- Buttercup tests for assignments  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-assignments)

;;;; Helper Functions

(describe "org-canvas--assignment-parse-extensions"
  (it "parses comma-separated extensions"
    (expect (org-canvas--assignment-parse-extensions "py, txt, pdf")
            :to-equal '("py" "txt" "pdf")))

  (it "parses space-separated extensions"
    (expect (org-canvas--assignment-parse-extensions "py txt pdf")
            :to-equal '("py" "txt" "pdf")))

  (it "returns nil for nil extensions input"
    (expect (org-canvas--assignment-parse-extensions nil) :to-be nil)))

(describe "org-canvas--assignment-parse-submission-types"
  (it "parses single submission type"
    (expect (org-canvas--assignment-parse-submission-types "online_upload")
            :to-equal '("online_upload")))

  (it "parses multiple submission types"
    (expect (org-canvas--assignment-parse-submission-types "online_upload, online_text_entry")
            :to-equal '("online_upload" "online_text_entry")))

  (it "returns none for nil input"
    (expect (org-canvas--assignment-parse-submission-types nil)
            :to-equal '("none"))))

;;;; Transform (pure, no buffer)

(describe "org-canvas--assignment-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--assignment-transform-props
                   '(:title-raw "Homework 1 [1/3]" :canvas-id nil
                     :points-raw nil :grading-type-raw nil :published-raw nil
                     :due-at-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :submission-raw nil :allowed-extensions-raw nil
                     :max-attempts-raw nil :peer-reviews-raw nil
                     :peer-review-count-raw nil :peer-review-due-at-raw nil
                     :automatic-peer-reviews-raw nil :omit-from-grades-raw nil
                     :anonymous-grading-raw nil :notify-of-update-raw nil
                     :grade-individually-raw nil :only-visible-to-overrides-raw nil
                     :moderated-grading-raw nil :grader-count-raw nil
                     :muted-raw nil :turnitin-enabled-raw nil
                     :grading-standard-id-raw nil :position-raw nil
                     :assignment-group-id-raw nil :rubric-id nil
                     :group-category-id-raw nil))))
      (expect (plist-get result :title) :to-equal "Homework 1")))

  (it "converts points to number"
    (let ((result (org-canvas--assignment-transform-props
                   '(:title-raw "HW" :canvas-id nil
                     :points-raw "100" :grading-type-raw nil :published-raw nil
                     :due-at-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :submission-raw nil :allowed-extensions-raw nil
                     :max-attempts-raw nil :peer-reviews-raw nil
                     :peer-review-count-raw nil :peer-review-due-at-raw nil
                     :automatic-peer-reviews-raw nil :omit-from-grades-raw nil
                     :anonymous-grading-raw nil :notify-of-update-raw nil
                     :grade-individually-raw nil :only-visible-to-overrides-raw nil
                     :moderated-grading-raw nil :grader-count-raw nil
                     :muted-raw nil :turnitin-enabled-raw nil
                     :grading-standard-id-raw nil :position-raw nil
                     :assignment-group-id-raw nil :rubric-id nil
                     :group-category-id-raw nil))))
      (expect (plist-get result :points_possible) :to-equal 100)))

  (it "defaults published to true"
    (let ((result (org-canvas--assignment-transform-props
                   '(:title-raw "HW" :canvas-id nil
                     :points-raw nil :grading-type-raw nil :published-raw nil
                     :due-at-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :submission-raw nil :allowed-extensions-raw nil
                     :max-attempts-raw nil :peer-reviews-raw nil
                     :peer-review-count-raw nil :peer-review-due-at-raw nil
                     :automatic-peer-reviews-raw nil :omit-from-grades-raw nil
                     :anonymous-grading-raw nil :notify-of-update-raw nil
                     :grade-individually-raw nil :only-visible-to-overrides-raw nil
                     :moderated-grading-raw nil :grader-count-raw nil
                     :muted-raw nil :turnitin-enabled-raw nil
                     :grading-standard-id-raw nil :position-raw nil
                     :assignment-group-id-raw nil :rubric-id nil
                     :group-category-id-raw nil))))
      (expect (plist-get result :published) :to-be t)))

  (it "parses due_at timestamp"
    (let ((result (org-canvas--assignment-transform-props
                   '(:title-raw "HW" :canvas-id nil
                     :points-raw nil :grading-type-raw nil :published-raw nil
                     :due-at-raw "<2026-09-15 Mon 10:00>" :unlock-at-raw nil
                     :lock-at-raw nil :submission-raw nil
                     :allowed-extensions-raw nil :max-attempts-raw nil
                     :peer-reviews-raw nil :peer-review-count-raw nil
                     :peer-review-due-at-raw nil :automatic-peer-reviews-raw nil
                     :omit-from-grades-raw nil :anonymous-grading-raw nil
                     :notify-of-update-raw nil :grade-individually-raw nil
                     :only-visible-to-overrides-raw nil :moderated-grading-raw nil
                     :grader-count-raw nil :muted-raw nil
                     :turnitin-enabled-raw nil :grading-standard-id-raw nil
                     :position-raw nil :assignment-group-id-raw nil
                     :rubric-id nil :group-category-id-raw nil))))
      (expect (plist-get result :due_at) :to-match "2026-09-15T")))

  (it "defaults grading_type to points"
    (let ((result (org-canvas--assignment-transform-props
                   '(:title-raw "HW" :canvas-id nil
                     :points-raw nil :grading-type-raw nil :published-raw nil
                     :due-at-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :submission-raw nil :allowed-extensions-raw nil
                     :max-attempts-raw nil :peer-reviews-raw nil
                     :peer-review-count-raw nil :peer-review-due-at-raw nil
                     :automatic-peer-reviews-raw nil :omit-from-grades-raw nil
                     :anonymous-grading-raw nil :notify-of-update-raw nil
                     :grade-individually-raw nil :only-visible-to-overrides-raw nil
                     :moderated-grading-raw nil :grader-count-raw nil
                     :muted-raw nil :turnitin-enabled-raw nil
                     :grading-standard-id-raw nil :position-raw nil
                     :assignment-group-id-raw nil :rubric-id nil
                     :group-category-id-raw nil))))
      (expect (plist-get result :grading_type) :to-equal "points")))

  (it "parses submission types"
    (let ((result (org-canvas--assignment-transform-props
                   '(:title-raw "HW" :canvas-id nil
                     :points-raw nil :grading-type-raw nil :published-raw nil
                     :due-at-raw nil :unlock-at-raw nil :lock-at-raw nil
                     :submission-raw "online_upload,online_text_entry"
                     :allowed-extensions-raw nil :max-attempts-raw nil
                     :peer-reviews-raw nil :peer-review-count-raw nil
                     :peer-review-due-at-raw nil :automatic-peer-reviews-raw nil
                     :omit-from-grades-raw nil :anonymous-grading-raw nil
                     :notify-of-update-raw nil :grade-individually-raw nil
                     :only-visible-to-overrides-raw nil :moderated-grading-raw nil
                     :grader-count-raw nil :muted-raw nil
                     :turnitin-enabled-raw nil :grading-standard-id-raw nil
                     :position-raw nil :assignment-group-id-raw nil
                     :rubric-id nil :group-category-id-raw nil))))
      (expect (plist-get result :submission_types)
              :to-equal '("online_upload" "online_text_entry")))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--assignment-parse-entry"
  (describe "common fields"
    (it "extracts assignment title from heading"
      (with-temp-org-buffer
       "* Homework 1
:PROPERTIES:
:PUBLISHED: true
:POINTS: 100
:END:

Complete the exercises.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :title) :to-equal "Homework 1"))))

    (test-org-canvas-define-common-parse-tests
     #'org-canvas--assignment-parse-entry)

    (it "parses points_possible"
      (with-temp-org-buffer
       "* Assignment
:PROPERTIES:
:PUBLISHED: true
:POINTS: 50
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :points_possible) :to-equal 50)))))

  (describe "submission types"
    (it "parses grading_type (default points)"
      (with-temp-org-buffer
       "* Assignment
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :grading_type) :to-equal "points"))))

    (it "parses custom grading_type"
      (with-temp-org-buffer
       "* Pass/Fail Assignment
:PROPERTIES:
:PUBLISHED: true
:GRADING_TYPE: pass_fail
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :grading_type) :to-equal "pass_fail"))))

    (it "parses submission_types"
      (with-temp-org-buffer
       "* Upload Assignment
:PROPERTIES:
:PUBLISHED: true
:SUBMISSION: online_upload
:END:

Upload your work.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :submission_types) :to-equal '("online_upload"))))))

  (describe "optional fields"
    (it "parses allowed_extensions"
      (with-temp-org-buffer
       "* Code Assignment
:PROPERTIES:
:PUBLISHED: true
:SUBMISSION: online_upload
:ALLOWED_EXTENSIONS: py, txt
:END:

Submit code.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :allowed_extensions) :to-equal '("py" "txt")))))

    (it "parses max_attempts"
      (with-temp-org-buffer
       "* Assignment
:PROPERTIES:
:PUBLISHED: true
:MAX_ATTEMPTS: 3
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :allowed_attempts) :to-equal 3))))

    (it "parses peer_reviews"
      (with-temp-org-buffer
       "* Peer Review Assignment
:PROPERTIES:
:PUBLISHED: true
:PEER_REVIEWS: true
:PEER_REVIEW_COUNT: 2
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :peer_reviews) :to-be t)
         (expect (plist-get data :peer_review_count) :to-equal 2))))))

;;;; Stage 2: Build Payload

(describe "org-canvas--assignment-build-payload"
  (describe "required fields"
    (it "wraps in assignment key"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")))
             (payload (org-canvas--assignment-build-payload data)))
        (expect (gethash "assignment" payload) :to-be-truthy)))

    (it "includes name in payload"
      (let* ((data '(:title "My Assignment" :description "" :published t
                     :grading_type "points" :submission_types ("none")))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "name" assignment) :to-equal "My Assignment")))

    (it "includes points_possible"
      (let* ((data '(:title "Test" :description "" :published t :points_possible 75
                     :grading_type "points" :submission_types ("none")))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "points_possible" assignment) :to-equal 75))))

  (describe "submission fields"
    (it "includes submission_types"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("online_upload" "online_text_entry")))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "submission_types" assignment)
                :to-equal '("online_upload" "online_text_entry"))))

    (it "includes allowed_extensions"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("online_upload")
                     :allowed_extensions ("py" "txt")))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "allowed_extensions" assignment) :to-equal '("py" "txt")))))

  (describe "optional fields"
    (it "includes peer review settings"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :peer_reviews t :peer_review_count 3))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "peer_reviews" assignment) :to-be t)
        (expect (gethash "peer_review_count" assignment) :to-equal 3)))))

;;;; Stage 3: Push to API (mocked)

(describe "assignment push-to-api (mocked)"
  (it "uses POST for new assignments"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-id nil))
              (payload (make-hash-table)))
          (org-canvas--push-to-api data payload
            :endpoint "assignments"
            :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name)))
          (expect-api-called 'POST "assignments$")))))

  (it "uses PUT for existing assignments"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "789"))
              (payload (make-hash-table)))
          (org-canvas--push-to-api data payload
            :endpoint "assignments"
            :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name)))
          (expect-api-called 'PUT "assignments/789")))))

  (it "sends a payload whose body reflects the org input"
    (let ((temp-dir (make-temp-file "assignments-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "assignments.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Essay\n:PROPERTIES:\n:POINTS: 50\n:GRADING_TYPE: points\n:PUBLISHED: true\n:SUBMISSION: online_upload\n:END:\n\nWrite an essay.\n"))
            (let ((org-canvas-assignments-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-assignments)
                  (let* ((body (test-org-canvas-api-call-data 'POST "assignments"))
                         (assignment (gethash "assignment" body)))
                    (expect (gethash "name" assignment) :to-equal "Essay")
                    (expect (gethash "points_possible" assignment) :to-equal 50)
                    (expect (gethash "grading_type" assignment) :to-equal "points")
                    (expect (gethash "published" assignment) :to-be t)
                    (expect (gethash "submission_types" assignment)
                            :to-equal '("online_upload")))))))
        (delete-directory temp-dir t)))))

(describe "assignment search (mocked)"
  (it "searches assignments endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (name . "Homework 1"))])))
        (org-canvas--search-item "assignments" "Homework 1" :match-field 'name)
        (expect-api-called 'GET "assignments"))))

  (it "returns matching assignment"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (name . "Lab 1"))
                                  ((id . 101) (name . "Lab 2"))])))
        (let ((result (org-canvas--search-item "assignments" "Lab 1" :match-field 'name)))
          (expect (alist-get 'id result) :to-equal 100))))))

;;;; Stage 4: Finalize

(describe "assignment finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Assignment" :pom (point-marker) :rubric-id nil))
           (response '((id . 44444) (name . "Test Assignment"))))
       (org-canvas--finalize-item data response
         :post-fn #'org-canvas--assignment-post-finalize)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "44444"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker) :rubric-id nil))
           (response '((id . 33333))))
       (org-canvas--finalize-item data response
         :post-fn #'org-canvas--assignment-post-finalize)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* No ID Assignment
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No ID Assignment" :pom (point-marker) :rubric-id nil))
           (response '((error . "failed"))))
       (expect (org-canvas--finalize-item data response
                 :post-fn #'org-canvas--assignment-post-finalize)
               :to-throw 'error)))))

;;;; Resolve Link ID Tests

(describe "org-canvas--assignment-resolve-link-id"
  (it "returns nil for nil link input"
    (expect (org-canvas--assignment-resolve-link-id nil "CANVAS_ID") :to-be nil))

  (it "returns nil for invalid link format"
    (expect (org-canvas--assignment-resolve-link-id "not a link" "CANVAS_ID") :to-be nil))

  (it "resolves link to canvas ID"
    (let ((temp-file (make-temp-file "test-groups" nil ".org"))
          (org-canvas-assignments-file (make-temp-file "test-assign" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* My Group
:PROPERTIES:
:CANVAS_ID: 12345
:END:
"))
            (let ((link (format "[[file:%s::*My Group]]" temp-file)))
              (expect (org-canvas--assignment-resolve-link-id link "CANVAS_ID")
                      :to-equal "12345")))
        (delete-file temp-file)
        (delete-file org-canvas-assignments-file))))

  (it "returns nil when target heading not found"
    (let ((temp-file (make-temp-file "test-groups" nil ".org"))
          (org-canvas-assignments-file (make-temp-file "test-assign" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Other Group\n"))
            (let ((link (format "[[file:%s::*Missing Group]]" temp-file)))
              (expect (org-canvas--assignment-resolve-link-id link "CANVAS_ID")
                      :to-be nil)))
        (delete-file temp-file)
        (delete-file org-canvas-assignments-file)))))

;;;; Additional Parse Tests

(describe "org-canvas--assignment-parse-entry"
  (describe "timestamps"
    (it "parses due_at timestamp"
      (with-temp-org-buffer
       "* Timed Assignment
:PROPERTIES:
:PUBLISHED: true
:DUE_AT: <2025-06-15 Sun 23:59>
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :due_at) :to-be-truthy)
         (expect (plist-get data :due_at) :to-match "T.*Z"))))

    (it "parses unlock_at and lock_at timestamps"
      (with-temp-org-buffer
       "* Locked Assignment
:PROPERTIES:
:PUBLISHED: true
:UNLOCK_AT: <2025-06-01 Sun 00:00>
:LOCK_AT: <2025-06-30 Mon 23:59>
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :unlock_at) :to-be-truthy)
         (expect (plist-get data :lock_at) :to-be-truthy)))))

  (describe "boolean fields"
    (it "parses published=false"
      (with-temp-org-buffer
       "* Draft Assignment
:PROPERTIES:
:PUBLISHED: false
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :published) :to-be nil))))

    (it "parses omit_from_final_grade"
      (with-temp-org-buffer
       "* Extra Credit
:PROPERTIES:
:PUBLISHED: true
:OMIT_FROM_GRADES: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :omit_from_final_grade) :to-be t))))

    (it "parses anonymous_grading"
      (with-temp-org-buffer
       "* Anonymous Exam
:PROPERTIES:
:PUBLISHED: true
:ANONYMOUS_GRADING: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :anonymous_grading) :to-be t))))

    (it "parses notify_of_update"
      (with-temp-org-buffer
       "* Updated Assignment
:PROPERTIES:
:PUBLISHED: true
:NOTIFY_OF_UPDATE: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :notify_of_update) :to-be t))))

    (it "parses automatic_peer_reviews"
      (with-temp-org-buffer
       "* Peer Assignment
:PROPERTIES:
:PUBLISHED: true
:PEER_REVIEWS: true
:AUTOMATIC_PEER_REVIEWS: false
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :automatic_peer_reviews) :to-be nil))))

    (it "parses only_visible_to_overrides"
      (with-temp-org-buffer
       "* Section-Only Assignment
:PROPERTIES:
:PUBLISHED: true
:ONLY_VISIBLE_TO_OVERRIDES: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :only_visible_to_overrides) :to-be t))))

    (it "parses moderated_grading"
      (with-temp-org-buffer
       "* Moderated Exam
:PROPERTIES:
:PUBLISHED: true
:MODERATED_GRADING: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :moderated_grading) :to-be t))))

    (it "parses grade_group_students_individually"
      (with-temp-org-buffer
       "* Group Assignment
:PROPERTIES:
:PUBLISHED: true
:GRADE_INDIVIDUALLY: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :grade_group_students_individually) :to-be t)))))

  (describe "optional fields"
    (it "exports description to HTML"
      (with-temp-org-buffer
       "* Assignment
:PROPERTIES:
:END:

This is *bold* text.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :description) :to-match "<"))))

    (it "parses group_category_id"
      (with-temp-org-buffer
       "* Group Project
:PROPERTIES:
:PUBLISHED: true
:GROUP_CATEGORY_ID: 42
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :group_category_id) :to-equal 42))))

    (it "parses grader_count"
      (with-temp-org-buffer
       "* Moderated Exam
:PROPERTIES:
:PUBLISHED: true
:MODERATED_GRADING: true
:GRADER_COUNT: 2
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :grader_count) :to-equal 2))))

    (it "returns nil grader_count when absent"
      (with-temp-org-buffer
       "* Assignment
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :grader_count) :to-be nil))))

    (it "parses position"
      (with-temp-org-buffer
       "* Ordered Assignment
:PROPERTIES:
:PUBLISHED: true
:POSITION: 5
:END:

Content.
"
       (org-back-to-heading)
       (let ((data (org-canvas--assignment-parse-entry)))
         (expect (plist-get data :position) :to-equal 5))))))

;;;; Additional Build Payload Tests

(describe "org-canvas--assignment-build-payload"
  (describe "required fields"
    (it "includes due_at when present"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :due_at "2025-06-15T23:59:00Z"))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "due_at" assignment) :to-equal "2025-06-15T23:59:00Z")))

    (it "includes unlock_at and lock_at when present"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :unlock_at "2025-06-01T00:00:00Z"
                     :lock_at "2025-06-30T23:59:00Z"))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "unlock_at" assignment) :to-equal "2025-06-01T00:00:00Z")
        (expect (gethash "lock_at" assignment) :to-equal "2025-06-30T23:59:00Z")))

    (it "includes assignment_group_id when present"
      (let* ((data '(:title "Grouped" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :assignment_group_id 999))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "assignment_group_id" assignment) :to-equal 999)))

    (it "sets published to :json-false when false"
      (let* ((data '(:title "Draft" :description "" :published nil
                     :grading_type "points" :submission_types ("none")))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "published" assignment) :to-equal :json-false))))

  (describe "submission fields"
    (it "includes allowed_attempts when present"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :allowed_attempts 3))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "allowed_attempts" assignment) :to-equal 3))))

  (describe "peer review fields"
    (it "includes peer_reviews_due_at when peer reviews enabled"
      (let* ((data '(:title "Peer" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :peer_reviews t :peer_reviews_due_at "2025-07-01T00:00:00Z"))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "peer_reviews_due_at" assignment) :to-equal "2025-07-01T00:00:00Z")))

    (it "defaults automatic_peer_reviews to t when peer_reviews enabled but automatic not set"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :peer_reviews t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "automatic_peer_reviews" assignment) :to-be t)))

    (it "defaults automatic_peer_reviews to t when AUTOMATIC_PEER_REVIEWS is explicitly false"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :peer_reviews t :automatic_peer_reviews nil))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        ;; org-canvas-org-get-boolean-property returns nil for "false",
        ;; which the build function treats the same as "not set" (defaults to t)
        (expect (gethash "automatic_peer_reviews" assignment) :to-be t)))

    (it "converts explicit automatic_peer_reviews true to json boolean"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :peer_reviews t :automatic_peer_reviews t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "automatic_peer_reviews" assignment) :to-be t))))

  (describe "optional fields"
    (it "includes omit_from_final_grade when true"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :omit_from_final_grade t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "omit_from_final_grade" assignment) :to-be t)))

    (it "excludes omit_from_final_grade when nil"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :omit_from_final_grade nil))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "omit_from_final_grade" assignment) :to-be nil)))

    (it "includes anonymous_grading when true"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :anonymous_grading t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "anonymous_grading" assignment) :to-be t)))

    (it "includes notify_of_update when true"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :notify_of_update t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "notify_of_update" assignment) :to-be t)))

    (it "includes group_category_id when present"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :group_category_id 42))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "group_category_id" assignment) :to-equal 42)))

    (it "includes grade_group_students_individually when true"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :grade_group_students_individually t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "grade_group_students_individually" assignment) :to-be t)))

    (it "includes only_visible_to_overrides when true"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :only_visible_to_overrides t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "only_visible_to_overrides" assignment) :to-be t)))

    (it "includes moderated_grading when true"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :moderated_grading t))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "moderated_grading" assignment) :to-be t)))

    (it "includes grader_count when moderated_grading is true"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :moderated_grading t :grader_count 2))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "grader_count" assignment) :to-equal 2)))

    (it "excludes grader_count when moderated_grading is nil"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :grader_count 2))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "grader_count" assignment) :to-be nil)))

    (it "includes position when present"
      (let* ((data '(:title "Test" :description "" :published t
                     :grading_type "points" :submission_types ("none")
                     :position 5))
             (payload (org-canvas--assignment-build-payload data))
             (assignment (gethash "assignment" payload)))
        (expect (gethash "position" assignment) :to-equal 5)))))

;;;; Additional Push to API Tests

(describe "assignment push-to-api error recovery (mocked)"
  (it "retries as POST on 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (and (eq method 'PUT) (= call-count 1))
                         (signal 'error '("API Request Failed (HTTP 404)" nil nil))
                       '((id . 999))))))
          (let ((data '(:title "Stale" :canvas-id "old-id"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--push-to-api data payload
                            :endpoint "assignments"
                            :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name)))))
              (expect (alist-get 'id result) :to-equal 999)
              (expect call-count :to-equal 2)))))))

  (it "recovers from timeout"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error (list "Request failed" "Timeout")))
                      ((eq method 'GET)
                       [((id . 888) (name . "Timeout Assignment"))])
                      (t nil)))))
          (let ((data '(:title "Timeout Assignment" :canvas-id nil))
                (payload (make-hash-table)))
            (let ((result (org-canvas--push-to-api data payload
                            :endpoint "assignments"
                            :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name)))))
              (expect (alist-get 'id result) :to-equal 888)))))))

  (it "signals error on non-recoverable failure"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request" nil nil)))))
        (let ((data '(:title "Bad" :canvas-id nil))
              (payload (make-hash-table)))
          (expect (org-canvas--push-to-api data payload
                    :endpoint "assignments"
                    :find-fn (lambda (name) (org-canvas--search-item "assignments" name :match-field 'name)))
                  :to-throw 'error))))))

;;;; Additional Find by Name Tests

(describe "assignment search (mocked)"
  (it "returns nil when no match"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (name . "Other"))])))
        (let ((result (org-canvas--search-item "assignments" "Nonexistent"
                       :match-field 'name)))
          (expect result :to-be nil)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("API error")))))
        (let ((result (org-canvas--search-item "assignments" "Any"
                       :match-field 'name)))
          (expect result :to-be nil))))))

;;;; Associate Rubric Tests

(describe "org-canvas--assignment-associate-rubric (mocked)"
  (it "creates rubric association"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--assignment-associate-rubric 123 "456")
        (expect-api-called 'POST "rubric_associations"))))

  (it "handles association error gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("Association failed")))))
        ;; Should not throw, just log warning
        (org-canvas--assignment-associate-rubric 123 "456")))))

;;;; Post Finalize Tests

(describe "org-canvas--assignment-post-finalize"
  (it "associates rubric when rubric-id present"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:rubric-id "789"))
              (response '((id . 123))))
          (org-canvas--assignment-post-finalize data response)
          (expect-api-called 'POST "rubric_associations")))))

  (it "skips rubric association when no rubric-id"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:rubric-id nil))
              (response '((id . 123))))
          (org-canvas--assignment-post-finalize data response)
          (expect (test-org-canvas-api-called-p 'POST "rubric_associations") :to-be nil))))))

;;;; Parse Entry with Linked Properties

(describe "org-canvas--assignment-parse-entry with cross-file links"
  (it "resolves GROUP link to assignment_group_id"
    (let ((groups-file (make-temp-file "test-groups" nil ".org"))
          (assignments-file (make-temp-file "test-assign" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Homework
:PROPERTIES:
:CANVAS_ID: 777
:END:
"))
            (with-temp-file assignments-file
              (insert (format "* Assignment 1
:PROPERTIES:
:PUBLISHED: true
:GROUP: [[file:%s::*Homework]]
:END:

Content.
" groups-file)))
            (let ((org-canvas-assignments-file assignments-file))
              (with-current-buffer (find-file-noselect assignments-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--assignment-parse-entry)))
                  (expect (plist-get data :assignment_group_id) :to-equal 777)))))
        (delete-file groups-file)
        (delete-file assignments-file))))

  (it "resolves RUBRIC_LINK to rubric-id"
    (let ((rubrics-file (make-temp-file "test-rubrics" nil ".org"))
          (assignments-file (make-temp-file "test-assign" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file rubrics-file
              (insert "* Standard Rubric
:PROPERTIES:
:CANVAS_ID: 888
:END:
"))
            (with-temp-file assignments-file
              (insert (format "* Assignment 2
:PROPERTIES:
:PUBLISHED: true
:RUBRIC_LINK: [[file:%s::*Standard Rubric]]
:END:

Content.
" rubrics-file)))
            (let ((org-canvas-assignments-file assignments-file))
              (with-current-buffer (find-file-noselect assignments-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--assignment-parse-entry)))
                  (expect (plist-get data :rubric-id) :to-equal "888")))))
        (delete-file rubrics-file)
        (delete-file assignments-file))))

  (it "returns number for assignment_group_id"
    (let ((groups-file (make-temp-file "test-groups" nil ".org"))
          (assignments-file (make-temp-file "test-assign" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Labs
:PROPERTIES:
:CANVAS_ID: 42
:END:
"))
            (with-temp-file assignments-file
              (insert (format "* Lab 1
:PROPERTIES:
:PUBLISHED: true
:GROUP: [[file:%s::*Labs]]
:END:

Content.
" groups-file)))
            (let ((org-canvas-assignments-file assignments-file))
              (with-current-buffer (find-file-noselect assignments-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let ((data (org-canvas--assignment-parse-entry)))
                  (expect (numberp (plist-get data :assignment_group_id)) :to-be t)))))
        (delete-file groups-file)
        (delete-file assignments-file)))))

;;;; Validation Tests

(describe "GRADING_TYPE validation"
  (it "accepts valid grading types"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:GRADING_TYPE: letter_grade
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :grading_type) :to-equal "letter_grade"))))

  (it "falls back to default for invalid grading type"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:GRADING_TYPE: invalid_type
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :grading_type) :to-equal "points")))))

(describe "SUBMISSION validation"
  (it "warns for invalid submission type"
    (spy-on 'message)
    (org-canvas--assignment-parse-submission-types "invalid_type")
    (expect 'message :to-have-been-called-with
            "Warning: SUBMISSION '%s' is not valid" "invalid_type")))

;;;; Post-Finalize Verification

(describe "org-canvas--assignment-post-finalize"
  (it "warns when assignment_group_id mismatches"
    (spy-on 'org-canvas--log-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id 200 :rubric-id nil))
          (response '((id . 1001) (assignment_group_id . 999))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'org-canvas--log-warning :to-have-been-called)))

  (it "does not warn when assignment_group_id matches"
    (spy-on 'org-canvas--log-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id 200 :rubric-id nil))
          (response '((id . 1001) (assignment_group_id . 200))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'org-canvas--log-warning :not :to-have-been-called)))

  (it "does not warn when no expected group"
    (spy-on 'org-canvas--log-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id nil :rubric-id nil))
          (response '((id . 1001) (assignment_group_id . 999))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'org-canvas--log-warning :not :to-have-been-called)))

  (it "still associates rubric after verification"
    (spy-on 'org-canvas--log-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id 200 :rubric-id "55"))
          (response '((id . 1001) (assignment_group_id . 200))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'org-canvas--assignment-associate-rubric
              :to-have-been-called-with 1001 "55"))))

;;;; Pull Function Tests

(describe "org-canvas--assignment-resolve-group-link"
  (it "resolves group ID to Org link"
    (let ((groups-file (make-temp-file "test-groups" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Homework
:PROPERTIES:
:CANVAS_ID: 777
:END:
"))
            (let ((org-canvas-assignment-groups-file groups-file))
              (let ((result (org-canvas--assignment-resolve-group-link 777)))
                (expect result :to-match "Homework"))))
        (let ((buf (find-buffer-visiting groups-file)))
          (when buf (kill-buffer buf)))
        (delete-file groups-file))))

  (it "returns nil when file does not exist"
    (let ((org-canvas-assignment-groups-file "/nonexistent-groups.org"))
      (expect (org-canvas--assignment-resolve-group-link 777) :to-be nil)))

  (it "returns nil when no matching group"
    (let ((groups-file (make-temp-file "test-groups" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Other Group
:PROPERTIES:
:CANVAS_ID: 999
:END:
"))
            (let ((org-canvas-assignment-groups-file groups-file))
              (expect (org-canvas--assignment-resolve-group-link 777) :to-be nil)))
        (let ((buf (find-buffer-visiting groups-file)))
          (when buf (kill-buffer buf)))
        (delete-file groups-file))))

  (it "returns nil for nil group-id"
    (expect (org-canvas--assignment-resolve-group-link nil) :to-be nil)))

(describe "org-canvas--assignment-pull-item"
  (it "sets PEER_REVIEWS and GROUP properties"
    (let ((groups-file (make-temp-file "test-groups" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file groups-file
              (insert "* Labs
:PROPERTIES:
:CANVAS_ID: 555
:END:
"))
            (let ((org-canvas-assignment-groups-file groups-file))
              (with-temp-org-buffer
               "* Assignment
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
               (org-back-to-heading)
               (with-html-to-org-identity
                 ;; overrides sub-fetch mocked: network guard must stay quiet
                 (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                            (lambda (&rest _) nil)))
                   (org-canvas--assignment-pull-item
                    '((id . 1) (name . "Assignment")
                      (points_possible . 100)
                      (submission_types . ["online_upload"])
                      (peer_reviews . t)
                      (assignment_group_id . 555)
                      (description . "<p>Do this</p>"))
                    (point)))
                 (expect (org-entry-get (point) "PEER_REVIEWS") :to-equal "true")
                 (expect (org-entry-get (point) "GROUP") :to-match "Labs")))))
        (let ((buf (find-buffer-visiting groups-file)))
          (when buf (kill-buffer buf)))
        (delete-file groups-file))))

  (it "sets OMIT_FROM_GRADES property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (omit_from_final_grade . t)
        (description . ""))
      "OMIT_FROM_GRADES" :to-equal "true"))

  (it "sets ANONYMOUS_GRADING property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (anonymous_grading . t)
        (description . ""))
      "ANONYMOUS_GRADING" :to-equal "true"))

  (it "sets ONLY_VISIBLE_TO_OVERRIDES property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (only_visible_to_overrides . t)
        (description . ""))
      "ONLY_VISIBLE_TO_OVERRIDES" :to-equal "true"))

  (it "sets MODERATED_GRADING property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (moderated_grading . t)
        (description . ""))
      "MODERATED_GRADING" :to-equal "true"))

  (it "sets GRADER_COUNT property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (grader_count . 3)
        (description . ""))
      "GRADER_COUNT" :to-equal "3"))

  (it "skips GRADER_COUNT when zero"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (grader_count . 0)
        (description . ""))
      "GRADER_COUNT" :to-be nil))

  (it "sets GRADE_INDIVIDUALLY property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (grade_group_students_individually . t)
        (description . ""))
      "GRADE_INDIVIDUALLY" :to-equal "true"))

  (it "sets GROUP_CATEGORY_ID property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (group_category_id . 42)
        (description . ""))
      "GROUP_CATEGORY_ID" :to-equal "42"))

  (it "sets POSITION property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (position . 5)
        (description . ""))
      "POSITION" :to-equal "5")))

;;;; New Property Tests: MUTED and GRADING_STANDARD_ID

(describe "org-canvas--assignment-parse-entry (muted + grading_standard_id)"
  (it "parses MUTED property"
    (with-temp-org-buffer
     "* Muted Assignment
:PROPERTIES:
:PUBLISHED: true
:MUTED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :muted) :to-be t))))

  (it "returns nil for missing MUTED"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :muted) :to-be nil))))

  (it "parses GRADING_STANDARD_ID"
    (with-temp-org-buffer
     "* Graded Assignment
:PROPERTIES:
:PUBLISHED: true
:GRADING_STANDARD_ID: 42
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :grading_standard_id) :to-equal 42))))

  (it "returns nil for missing GRADING_STANDARD_ID"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :grading_standard_id) :to-be nil)))))

(describe "org-canvas--assignment-build-payload (muted + grading_standard_id)"
  (it "includes muted when true"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")
                   :muted t))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "muted" assignment) :to-be t)))

  (it "excludes muted when nil"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "muted" assignment) :to-be nil)))

  (it "includes grading_standard_id when present"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")
                   :grading_standard_id 42))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "grading_standard_id" assignment) :to-equal 42)))

  (it "excludes grading_standard_id when nil"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "grading_standard_id" assignment) :to-be nil))))

(describe "org-canvas--assignment-pull-item (muted + grading_standard_id)"
  (it "sets MUTED property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (muted . t)
        (description . ""))
      "MUTED" :to-equal "true"))

  (it "sets GRADING_STANDARD_ID property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (grading_standard_id . 42)
        (description . ""))
      "GRADING_STANDARD_ID" :to-equal "42")))

;;;; TURNITIN_ENABLED Property Tests

(describe "org-canvas--assignment-parse-entry (turnitin_enabled)"
  (it "parses TURNITIN_ENABLED property"
    (with-temp-org-buffer
     "* Essay Assignment
:PROPERTIES:
:PUBLISHED: true
:TURNITIN_ENABLED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :turnitin_enabled) :to-be t))))

  (it "returns nil for missing TURNITIN_ENABLED"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :turnitin_enabled) :to-be nil)))))

(describe "org-canvas--assignment-build-payload (turnitin_enabled)"
  (it "includes turnitin_enabled when true"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")
                   :turnitin_enabled t))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "turnitin_enabled" assignment) :to-be t)))

  (it "excludes turnitin_enabled when nil"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "turnitin_enabled" assignment) :to-be nil))))

(describe "org-canvas--assignment-pull-item (turnitin_enabled)"
  (it "sets TURNITIN_ENABLED property"
    (with-pull-property-test #'org-canvas--assignment-pull-item
      '((id . 1) (name . "Assignment")
        (turnitin_enabled . t)
        (description . ""))
      "TURNITIN_ENABLED" :to-equal "true")))

;;;; Section override pull (Task 16)

(describe "org-canvas--section-link-by-id"
  (it "returns a file link to the section heading by CANVAS_ID"
    (let ((sections-temp (make-temp-file "sections-link-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-temp
              (insert "#+TITLE: Sections\n* S2601-CPSC-6300-001-15179\n:PROPERTIES:\n:CANVAS_ID: 296338\n:END:\n"))
            (let ((org-canvas-sections-file sections-temp))
              (let ((link (org-canvas--section-link-by-id 296338)))
                (expect link :to-match
                        "\\[\\[file:.*::\\*S2601-CPSC-6300-001-15179\\]\\[S2601-CPSC-6300-001-15179\\]\\]"))))
        (let ((buf (find-buffer-visiting sections-temp)))
          (when buf (kill-buffer buf)))
        (delete-file sections-temp))))

  (it "returns the raw id when sections file is missing"
    (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
      (expect (org-canvas--section-link-by-id 296338) :to-equal "296338")))

  (it "returns the raw id when no matching CANVAS_ID is found"
    (let ((sections-temp (make-temp-file "sections-link-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-temp
              (insert "#+TITLE: Sections\n* Other\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (let ((org-canvas-sections-file sections-temp))
              (expect (org-canvas--section-link-by-id 296338) :to-equal "296338")))
        (let ((buf (find-buffer-visiting sections-temp)))
          (when buf (kill-buffer buf)))
        (delete-file sections-temp)))))

(describe "assignment override pull"
  (it "emits #+NAME: overrides table when assignment has overrides"
    (let* ((temp-dir (make-temp-file "assign-ov-test" t))
           (test-file (expand-file-name "assignments.org" temp-dir))
           (sections-file (expand-file-name "sections.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-assignments-file test-file)
                (org-canvas-sections-file sections-file)
                (org-canvas-assignment-groups-file "/tmp/nonexistent-ag.org"))
            (with-temp-file sections-file
              (insert "#+TITLE: Sections\n* S2601-CPSC-6300-001-15179\n:PROPERTIES:\n:CANVAS_ID: 296338\n:END:\n"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method url &optional _params)
                           (cond
                            ((string-match-p "/overrides" url)
                             '(((id . 5001)
                                (course_section_id . 296338)
                                (due_at . "2026-02-22T23:59:00Z"))))
                            (t
                             '(((id . 678) (name . "Lab 1")
                                (due_at . "2026-02-15T23:59:00Z")
                                (assignment_group_id . 100)))))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-assignments)
                (let ((s (with-temp-buffer
                           (insert-file-contents test-file)
                           (buffer-string))))
                  (expect s :to-match "^#\\+NAME: overrides$")
                  (expect s :to-match "S2601-CPSC-6300-001-15179")
                  (expect s :to-match "<2026-02-22")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "emits no override table when assignment has no overrides"
    (let* ((temp-dir (make-temp-file "assign-noov-test" t))
           (test-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-assignments-file test-file)
                (org-canvas-assignment-groups-file "/tmp/nonexistent-ag.org"))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method url &optional _params)
                           (cond
                            ((string-match-p "/overrides" url) '())
                            (t
                             '(((id . 700) (name . "Solo")
                                (due_at . "2026-02-15T23:59:00Z")
                                (assignment_group_id . 100)))))))
                        ((symbol-function 'org-canvas-clear-log) (lambda () nil))
                        ((symbol-function 'display-buffer) (lambda (_) nil)))
                (org-canvas-pull-assignments)
                (let ((s (with-temp-buffer
                           (insert-file-contents test-file)
                           (buffer-string))))
                  (expect s :not :to-match "^#\\+NAME: overrides$")))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--override-emit-table"
  (it "renders \"All Sections\" when course_section_id is nil"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . nil)
            (due_at . "2026-03-28T23:59:00Z")))
         "2026-02-15T23:59:00Z" nil nil))
      (expect (buffer-string) :to-match "All Sections")
      (expect (buffer-string) :not :to-match "| nil ")))

  (it "drops rows whose every populated date matches the parent"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . 1)
            (due_at . "2026-02-15T23:59:00Z")))
         "2026-02-15T23:59:00Z" nil nil))
      ;; The override matches parent due_at and has no unlock/lock — nothing to show.
      (expect (buffer-string) :to-equal "")))

  (it "drops columns where every kept row's cell is empty"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . 1)
            (due_at . "2026-03-28T23:59:00Z")))
         "2026-02-15T23:59:00Z" nil nil))
      (let ((s (buffer-string)))
        (expect s :to-match "Due At")
        (expect s :not :to-match "Unlock At")
        (expect s :not :to-match "Lock At"))))

  (it "keeps columns that have data in any kept row"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . 1)
            (due_at . "2026-03-28T23:59:00Z")
            (unlock_at . "2026-03-20T00:00:00Z")))
         "2026-02-15T23:59:00Z" nil nil))
      (let ((s (buffer-string)))
        (expect s :to-match "Due At")
        (expect s :to-match "Unlock At")
        ;; "Lock At" appears as a substring of "Unlock At" — anchor to a
        ;; pipe so we only match it when it's a real column header.
        (expect s :not :to-match "| Lock At "))))

  (it "skips the entire table when every override is redundant"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . nil)
            (due_at . "2026-02-15T23:59:00Z"))
           ((id . 2) (course_section_id . 99)
            (due_at . "2026-02-15T23:59:00Z")))
         "2026-02-15T23:59:00Z" nil nil))
      (expect (buffer-string) :to-equal "")))

  (it "still emits the table when overrides differ on at least one field"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . 1)
            (due_at . "2026-02-15T23:59:00Z"))   ; redundant
           ((id . 2) (course_section_id . 2)
            (due_at . "2026-03-28T23:59:00Z")))  ; differs
         "2026-02-15T23:59:00Z" nil nil))
      (let ((s (buffer-string)))
        (expect s :to-match "^#\\+NAME: overrides$")
        ;; Only the differing row appears.
        (expect s :to-match "<2026-03-28")
        (expect s :not :to-match "<2026-02-15")))))

;;; org-canvas-assignments-test.el ends here
