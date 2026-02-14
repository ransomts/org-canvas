;;; org-canvas-assignment-groups-test.el --- Buttercup tests for assignment groups  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-assignment-groups)

;;;; Stage 1: Parse Entry

(describe "org-canvas--assignment-group-parse-entry"
  (it "extracts name from heading"
    (with-temp-org-buffer
     "* Config
** Homework
:PROPERTIES:
:WEIGHT: 30
:END:
"
     (search-forward "Homework")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :name) :to-equal "Homework"))))

  (it "errors on empty name"
    (with-temp-org-buffer
     (concat "* Config\n** " "\n:PROPERTIES:\n:WEIGHT: 30\n:END:\n")
     (search-forward "** ")
     (org-back-to-heading)
     (expect (org-canvas--assignment-group-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Config
** Exams
:PROPERTIES:
:WEIGHT: 40
:CANVAS_ID: 22222
:END:
"
     (search-forward "Exams")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "22222"))))

  (it "parses group_weight"
    (with-temp-org-buffer
     "* Config
** Projects
:PROPERTIES:
:WEIGHT: 25
:END:
"
     (search-forward "Projects")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       ;; string-to-number returns integer for "25"
       (expect (plist-get data :group_weight) :to-equal 25))))

  (it "defaults group_weight to 0"
    (with-temp-org-buffer
     "* Config
** Group
:PROPERTIES:
:END:
"
     (search-forward "Group")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :group_weight) :to-equal 0.0))))

  (it "parses drop_lowest rule"
    (with-temp-org-buffer
     "* Config
** Quizzes
:PROPERTIES:
:WEIGHT: 20
:DROP_LOWEST: 2
:END:
"
     (search-forward "Quizzes")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :rules) :to-be-truthy)
       (expect (alist-get 'drop_lowest (plist-get data :rules)) :to-equal 2))))

  (it "parses drop_highest rule"
    (with-temp-org-buffer
     "* Config
** Labs
:PROPERTIES:
:WEIGHT: 15
:DROP_HIGHEST: 1
:END:
"
     (search-forward "Labs")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (alist-get 'drop_highest (plist-get data :rules)) :to-equal 1))))

  (it "parses POSITION as number"
    (with-temp-org-buffer
     "* Config
** Homework
:PROPERTIES:
:WEIGHT: 30
:POSITION: 2
:END:
"
     (search-forward "Homework")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :position) :to-equal 2))))

  (it "returns nil for position when not set"
    (with-temp-org-buffer
     "* Config
** Group
:PROPERTIES:
:WEIGHT: 10
:END:
"
     (search-forward "Group")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :position) :to-be nil))))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Config
** Test
:PROPERTIES:
:WEIGHT: 10
:END:
"
     (search-forward "Test")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy)))))

;;;; Stage 2: Build Payload

(describe "org-canvas--assignment-group-build-payload"
  (it "includes name in payload"
    (let* ((data '(:name "Homework" :group_weight 30.0 :rules nil :canvas-id nil))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'name payload) :to-equal "Homework")))

  (it "includes group_weight"
    (let* ((data '(:name "Test" :group_weight 25.0 :rules nil :canvas-id nil))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'group_weight payload) :to-equal 25.0)))

  (it "excludes rules for new groups (POST)"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules ((drop_lowest . 1))))
           (payload (org-canvas--assignment-group-build-payload data)))
      ;; Rules should NOT be included for new groups (canvas-id is nil)
      (expect (alist-get 'rules payload) :to-be nil)))

  (it "includes rules for existing groups (PUT)"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id "123"
                   :rules ((drop_lowest . 2))))
           (payload (org-canvas--assignment-group-build-payload data)))
      ;; Rules should be included for updates (canvas-id is present)
      (expect (alist-get 'rules payload) :to-be-truthy)
      (expect (alist-get 'drop_lowest (alist-get 'rules payload)) :to-equal 2)))

  (it "includes position in payload when set"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules nil :position 3))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'position payload) :to-equal 3)))

  (it "excludes position from payload when nil"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules nil :position nil))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'position payload) :to-be nil))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--assignment-group-push-to-api (mocked)"
  (it "uses POST for new groups"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "New" :canvas-id nil))
              (payload '((name . "New"))))
          (org-canvas--assignment-group-push-to-api data payload)
          (expect-api-called 'POST "assignment_groups$")))))

  (it "uses PUT for existing groups"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "Existing" :canvas-id "321"))
              (payload '((name . "Existing"))))
          (org-canvas--assignment-group-push-to-api data payload)
          (expect-api-called 'PUT "assignment_groups/321"))))))

;;;; Stage 4: Finalize

(describe "org-canvas--assignment-group-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Config
** Test Group
:PROPERTIES:
:WEIGHT: 20
:END:
"
     (search-forward "Test Group")
     (org-back-to-heading)
     (let ((data (list :name "Test Group" :pom (point-marker)))
           (response '((id . 55555) (name . "Test Group"))))
       (org-canvas--assignment-group-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "55555"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Config
** Test
:PROPERTIES:
:WEIGHT: 15
:END:
"
     (search-forward "Test")
     (org-back-to-heading)
     (let ((data (list :name "Test" :pom (point-marker)))
           (response '((id . 44444))))
       (org-canvas--assignment-group-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-")))))

;;;; Pull Function Tests

(describe "org-canvas--assignment-group-pull-item"
  (it "sets WEIGHT property"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 25))
      (point))
     (expect (org-entry-get (point) "WEIGHT") :to-equal "25")))

  (it "sets DROP_LOWEST and DROP_HIGHEST from rules"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 10)
        (rules . ((drop_lowest . 2) (drop_highest . 1))))
      (point))
     (expect (org-entry-get (point) "DROP_LOWEST") :to-equal "2")
     (expect (org-entry-get (point) "DROP_HIGHEST") :to-equal "1")))

  (it "skips zero drop rules"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 50)
        (rules . ((drop_lowest . 0) (drop_highest . 0))))
      (point))
     (expect (org-entry-get (point) "DROP_LOWEST") :to-be nil)
     (expect (org-entry-get (point) "DROP_HIGHEST") :to-be nil)))

  (it "sets POSITION property on pull"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30) (position . 2))
      (point))
     (expect (org-entry-get (point) "POSITION") :to-equal "2")))

  (it "does not set POSITION when nil in response"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30))
      (point))
     (expect (org-entry-get (point) "POSITION") :to-be nil))))

;;;; New Property Tests: NEVER_DROP

(describe "org-canvas--assignment-group-parse-entry (never_drop)"
  (it "parses NEVER_DROP into rules"
    (with-temp-org-buffer
     "* Config
** Protected
:PROPERTIES:
:WEIGHT: 30
:NEVER_DROP: 123,456,789
:END:
"
     (search-forward "Protected")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (alist-get 'never_drop (plist-get data :rules))
               :to-equal '(123 456 789)))))

  (it "does not include never_drop when absent"
    (with-temp-org-buffer
     "* Config
** Simple
:PROPERTIES:
:WEIGHT: 20
:END:
"
     (search-forward "Simple")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (alist-get 'never_drop (plist-get data :rules)) :to-be nil)))))

(describe "org-canvas--assignment-group-build-payload (never_drop)"
  (it "includes never_drop in rules for updates"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id "123"
                   :rules ((drop_lowest . 1) (never_drop . (100 200)))))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'never_drop (alist-get 'rules payload))
              :to-equal '(100 200))))

  (it "excludes never_drop rules for new groups"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules ((never_drop . (100 200)))))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'rules payload) :to-be nil))))

(describe "org-canvas--assignment-group-pull-item (never_drop)"
  (it "sets NEVER_DROP property from rules"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30)
        (rules . ((never_drop . [100 200 300]))))
      (point))
     (expect (org-entry-get (point) "NEVER_DROP") :to-equal "100,200,300")))

  (it "does not set NEVER_DROP when empty"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30)
        (rules . ((never_drop . []))))
      (point))
     (expect (org-entry-get (point) "NEVER_DROP") :to-be nil))))

;;; org-canvas-assignment-groups-test.el ends here
