;;; org-canvas-assignments-test.el --- Buttercup tests for assignments  -*- lexical-binding: t; -*-

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

  (it "returns nil for nil input"
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

;;;; Stage 1: Parse Entry

(describe "org-canvas--assignment-parse-entry"
  (it "extracts title from heading"
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

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:POINTS: 100\n:END:\n\nBody.\n")
     (org-back-to-heading)
     (expect (org-canvas--assignment-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:CANVAS_ID: 11111
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "11111"))))

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
       (expect (plist-get data :points_possible) :to-equal 50))))

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
       (expect (plist-get data :submission_types) :to-equal '("online_upload")))))

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
       (expect (plist-get data :peer_review_count) :to-equal 2)))))

;;;; Stage 2: Build Payload

(describe "org-canvas--assignment-build-payload"
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
      (expect (gethash "points_possible" assignment) :to-equal 75)))

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
      (expect (gethash "allowed_extensions" assignment) :to-equal '("py" "txt"))))

  (it "includes peer review settings"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")
                   :peer_reviews t :peer_review_count 3))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "peer_reviews" assignment) :to-be t)
      (expect (gethash "peer_review_count" assignment) :to-equal 3))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--assignment-push-to-api (mocked)"
  (it "uses POST for new assignments"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-id nil))
              (payload (make-hash-table)))
          (org-canvas--assignment-push-to-api data payload)
          (expect-api-called 'POST "assignments$")))))

  (it "uses PUT for existing assignments"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "789"))
              (payload (make-hash-table)))
          (org-canvas--assignment-push-to-api data payload)
          (expect-api-called 'PUT "assignments/789"))))))

(describe "org-canvas--assignment-find-by-name (mocked)"
  (it "searches assignments endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (name . "Homework 1"))])))
        (org-canvas--assignment-find-by-name "Homework 1")
        (expect-api-called 'GET "assignments"))))

  (it "returns matching assignment"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (name . "Lab 1"))
                                  ((id . 101) (name . "Lab 2"))])))
        (let ((result (org-canvas--assignment-find-by-name "Lab 1")))
          (expect (alist-get 'id result) :to-equal 100))))))

;;;; Stage 4: Finalize

(describe "org-canvas--assignment-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Assignment
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Assignment" :pom (point-marker) :rubric-id nil))
           (response '((id . 44444) (name . "Test Assignment"))))
       (org-canvas--assignment-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "44444"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker) :rubric-id nil))
           (response '((id . 33333))))
       (org-canvas--assignment-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-"))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* No ID Assignment
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No ID Assignment" :pom (point-marker) :rubric-id nil))
           (response '((error . "failed"))))
       (expect (org-canvas--assignment-finalize data response) :to-throw 'error)))))

;;;; Resolve Link ID Tests

(describe "org-canvas--assignment-resolve-link-id"
  (it "returns nil for nil input"
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
  (it "returns nil canvas-id for new assignments"
    (with-temp-org-buffer
     "* New Assignment
:PROPERTIES:
:PUBLISHED: true
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :canvas-id) :to-be nil))))

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
       (expect (plist-get data :lock_at) :to-be-truthy))))

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

  (it "includes pom marker"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:END:

Content.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy))))

  (it "exports description to HTML"
    (with-temp-org-buffer
     "* Assignment
:PROPERTIES:
:END:

This is *bold* text.
"
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-parse-entry)))
       (expect (plist-get data :description) :to-match "<")))))

;;;; Additional Build Payload Tests

(describe "org-canvas--assignment-build-payload"
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
      (expect (gethash "published" assignment) :to-equal :json-false)))

  (it "includes allowed_attempts when present"
    (let* ((data '(:title "Test" :description "" :published t
                   :grading_type "points" :submission_types ("none")
                   :allowed_attempts 3))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "allowed_attempts" assignment) :to-equal 3)))

  (it "includes peer_reviews_due_at when peer reviews enabled"
    (let* ((data '(:title "Peer" :description "" :published t
                   :grading_type "points" :submission_types ("none")
                   :peer_reviews t :peer_reviews_due_at "2025-07-01T00:00:00Z"))
           (payload (org-canvas--assignment-build-payload data))
           (assignment (gethash "assignment" payload)))
      (expect (gethash "peer_reviews_due_at" assignment) :to-equal "2025-07-01T00:00:00Z"))))

;;;; Additional Push to API Tests

(describe "org-canvas--assignment-push-to-api (mocked)"
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
            (let ((result (org-canvas--assignment-push-to-api data payload)))
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
            (let ((result (org-canvas--assignment-push-to-api data payload)))
              (expect (alist-get 'id result) :to-equal 888)))))))

  (it "signals error on non-recoverable failure"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request" nil nil)))))
        (let ((data '(:title "Bad" :canvas-id nil))
              (payload (make-hash-table)))
          (expect (org-canvas--assignment-push-to-api data payload)
                  :to-throw 'error))))))

;;;; Additional Find by Name Tests

(describe "org-canvas--assignment-find-by-name (mocked)"
  (it "returns nil when no match"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments" . [((id . 100) (name . "Other"))])))
        (let ((result (org-canvas--assignment-find-by-name "Nonexistent")))
          (expect result :to-be nil)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'error '("API error")))))
        (let ((result (org-canvas--assignment-find-by-name "Any")))
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
    (spy-on 'elog-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id 200 :rubric-id nil))
          (response '((id . 1001) (assignment_group_id . 999))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'elog-warning :to-have-been-called)))

  (it "does not warn when assignment_group_id matches"
    (spy-on 'elog-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id 200 :rubric-id nil))
          (response '((id . 1001) (assignment_group_id . 200))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'elog-warning :not :to-have-been-called)))

  (it "does not warn when no expected group"
    (spy-on 'elog-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id nil :rubric-id nil))
          (response '((id . 1001) (assignment_group_id . 999))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'elog-warning :not :to-have-been-called)))

  (it "still associates rubric after verification"
    (spy-on 'elog-warning)
    (spy-on 'org-canvas--assignment-associate-rubric)
    (let ((data (list :title "Lab 1" :assignment_group_id 200 :rubric-id "55"))
          (response '((id . 1001) (assignment_group_id . 200))))
      (org-canvas--assignment-post-finalize data response)
      (expect 'org-canvas--assignment-associate-rubric
              :to-have-been-called-with 1001 "55"))))

;;; org-canvas-assignments-test.el ends here
