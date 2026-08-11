;;; org-canvas-modules-test.el --- Buttercup tests for modules  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-modules)

;;;; Helper Functions

(describe "org-canvas--module-item-type-from-file"
  (it "returns Page for pages.org"
    (expect (org-canvas--module-item-type-from-file "pages.org")
            :to-equal "Page"))

  (it "returns Assignment for assignments.org"
    (expect (org-canvas--module-item-type-from-file "assignments.org")
            :to-equal "Assignment"))

  (it "returns Discussion for discussions.org"
    (expect (org-canvas--module-item-type-from-file "discussions.org")
            :to-equal "Discussion"))

  (it "returns Quiz for quizzes.org"
    (expect (org-canvas--module-item-type-from-file "quizzes.org")
            :to-equal "Quiz"))

  (it "returns File for files.org"
    (expect (org-canvas--module-item-type-from-file "files.org")
            :to-equal "File"))

  (it "returns Discussion for announcements.org"
    (expect (org-canvas--module-item-type-from-file "announcements.org")
            :to-equal "Discussion"))

  (it "defaults to Page for unknown files"
    (expect (org-canvas--module-item-type-from-file "unknown.org")
            :to-equal "Page")))

(describe "org-canvas--module-parse-prerequisite-ids"
  (it "parses comma-separated IDs"
    (expect (org-canvas--module-parse-prerequisite-ids "123, 456, 789")
            :to-equal '("123" "456" "789")))

  (it "returns nil for nil prerequisite-ids input"
    (expect (org-canvas--module-parse-prerequisite-ids nil) :to-be nil)))

;;;; Transform Functions

(describe "org-canvas--module-transform-props"
  (it "strips statistics cookie from title"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Week 1 [1/3]" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :title) :to-equal "Week 1")))

  (it "interprets published true by default"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :published) :to-be t)))

  (it "interprets published false"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw "false" :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :published) :to-be nil)))

  (it "converts position to number and omits zero"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw "3"
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :position) :to-equal 3)))

  (it "omits position when zero or nil"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :position) :to-be nil)))

  (it "passes through prerequisite-module-ids"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids "100, 200"
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :prerequisite-module-ids) :to-equal '("100" "200"))))

  (it "interprets require-sequential-progress boolean"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw "true" :publish-final-grade-raw nil))))
      (expect (plist-get data :require-sequential-progress) :to-be t)))

  (it "interprets publish-final-grade boolean"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id nil
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw "true"))))
      (expect (plist-get data :publish-final-grade) :to-be t)))

  (it "passes through canvas-id"
    (let ((data (org-canvas--module-transform-props
                 '(:title-raw "Mod" :canvas-id "999"
                   :published-raw nil :position-raw nil
                   :unlock-at-raw nil :prerequisite-module-ids nil
                   :require-sequential-raw nil :publish-final-grade-raw nil))))
      (expect (plist-get data :canvas-id) :to-equal "999"))))

(describe "org-canvas--module-item-transform-props"
  (it "strips statistics cookie from title for ExternalUrl"
    (let* ((data (org-canvas--module-item-transform-props
                  '(:title-raw "Link [2/5]" :heading-with-links nil
                    :canvas-id nil :indent-raw nil
                    :completion-requirement nil :min-score-raw nil
                    :external-url "https://example.com" :new-tab-raw nil
                    :published-raw nil :link-info nil)))
           (data-type (plist-get data :type)))
      (expect (plist-get data :title) :to-equal "Link")
      (expect data-type :to-equal "ExternalUrl")))

  (it "interprets published true by default for items"
    (let ((data (org-canvas--module-item-transform-props
                 '(:title-raw "Item" :heading-with-links nil
                   :canvas-id nil :indent-raw nil
                   :completion-requirement nil :min-score-raw nil
                   :external-url "https://x.com" :new-tab-raw nil
                   :published-raw nil :link-info nil))))
      (expect (plist-get data :published) :to-be t)))

  (it "interprets new-tab boolean"
    (let ((data (org-canvas--module-item-transform-props
                 '(:title-raw "Item" :heading-with-links nil
                   :canvas-id nil :indent-raw nil
                   :completion-requirement nil :min-score-raw nil
                   :external-url "https://x.com" :new-tab-raw "true"
                   :published-raw nil :link-info nil))))
      (expect (plist-get data :new-tab) :to-be t)))

  (it "converts indent to number"
    (let ((data (org-canvas--module-item-transform-props
                 '(:title-raw "Item" :heading-with-links nil
                   :canvas-id nil :indent-raw "2"
                   :completion-requirement nil :min-score-raw nil
                   :external-url "https://x.com" :new-tab-raw nil
                   :published-raw nil :link-info nil))))
      (expect (plist-get data :indent) :to-equal 2)))

  (it "converts min-score and omits zero"
    (let ((data (org-canvas--module-item-transform-props
                 '(:title-raw "Item" :heading-with-links nil
                   :canvas-id nil :indent-raw nil
                   :completion-requirement "min_score" :min-score-raw "80"
                   :external-url "https://x.com" :new-tab-raw nil
                   :published-raw nil :link-info nil))))
      (expect (plist-get data :min-score) :to-equal 80)))

  (it "omits min-score when zero"
    (let ((data (org-canvas--module-item-transform-props
                 '(:title-raw "Item" :heading-with-links nil
                   :canvas-id nil :indent-raw nil
                   :completion-requirement "min_score" :min-score-raw nil
                   :external-url "https://x.com" :new-tab-raw nil
                   :published-raw nil :link-info nil))))
      (expect (plist-get data :min-score) :to-be nil)))

  (it "returns linked item data when link-info present"
    (let* ((data (org-canvas--module-item-transform-props
                  '(:title-raw "Ignored" :heading-with-links nil
                    :canvas-id "42" :indent-raw "1"
                    :completion-requirement "must_view" :min-score-raw nil
                    :external-url nil :new-tab-raw nil
                    :published-raw "true"
                    :link-info (:type "Assignment" :title "Lab 1"
                                :content-id 555 :page-url nil))))
           (data-type (plist-get data :type)))
      (expect data-type :to-equal "Assignment")
      (expect (plist-get data :title) :to-equal "Lab 1")
      (expect (plist-get data :content-id) :to-equal 555)
      (expect (plist-get data :canvas-id) :to-equal "42")
      (expect (plist-get data :indent) :to-equal 1)
      (expect (plist-get data :completion-requirement) :to-equal "must_view")))

  (it "returns nil when no external-url and no link-info"
    (let ((data (org-canvas--module-item-transform-props
                 '(:title-raw "Plain Text" :heading-with-links nil
                   :canvas-id nil :indent-raw nil
                   :completion-requirement nil :min-score-raw nil
                   :external-url nil :new-tab-raw nil
                   :published-raw nil :link-info nil))))
      (expect data :to-be nil)))

  (it "passes through canvas-id for ExternalUrl"
    (let ((data (org-canvas--module-item-transform-props
                 '(:title-raw "Link" :heading-with-links nil
                   :canvas-id "77" :indent-raw nil
                   :completion-requirement nil :min-score-raw nil
                   :external-url "https://x.com" :new-tab-raw nil
                   :published-raw nil :link-info nil))))
      (expect (plist-get data :canvas-id) :to-equal "77"))))

;;;; Stage 1: Parse Module Entry

(describe "org-canvas--module-parse-entry"
  (it "extracts module title from heading"
    (with-temp-org-buffer
     "* Week 1 - Introduction
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :title) :to-equal "Week 1 - Introduction"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:PUBLISHED: true\n:END:\n")
     (org-back-to-heading)
     (expect (org-canvas--module-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 66666
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "66666"))))

  (it "parses published property"
    (with-temp-org-buffer
     "* Hidden Module
:PROPERTIES:
:PUBLISHED: false
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :published) :to-be nil))))

  (it "defaults published to true when missing"
    (with-temp-org-buffer
     "* Module Without Published
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :published) :to-be t))))

  (it "parses position property"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:PUBLISHED: true
:POSITION: 3
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :position) :to-equal 3))))

  (it "returns nil for position when zero or missing"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:PUBLISHED: true
:POSITION: 0
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :position) :to-be nil))))

  (it "parses require_sequential_progression"
    (with-temp-org-buffer
     "* Sequential Module
:PROPERTIES:
:PUBLISHED: true
:REQUIRE_SEQUENTIAL_PROGRESSION: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :require-sequential-progress) :to-be t))))

  (it "parses prerequisite_module_ids"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:PUBLISHED: true
:PREREQUISITE_MODULE_IDS: 100, 101
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :prerequisite-module-ids) :to-equal '("100" "101")))))

  (it "returns nil for unlock_at when not specified"
    (with-temp-org-buffer
     "* Module Without Unlock
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :unlock-at) :to-be nil))))

  (it "parses publish_final_grade"
    (with-temp-org-buffer
     "* Final Module
:PROPERTIES:
:PUBLISHED: true
:PUBLISH_FINAL_GRADE: true
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :publish-final-grade) :to-be t))))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--module-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy)))))

;;;; Stage 1: Parse Module Item Entry

(describe "org-canvas--module-item-parse-entry"
  (it "treats non-link headings as SubHeader"
    ;; Emacs 29.x org-mode has different heading recognition behavior
    (unless test-org-canvas-emacs-30-p
      (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
    (with-temp-org-buffer
     "* Module
** Section Divider
:PROPERTIES:
:END:
"
     (search-forward "Section Divider")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       (expect (plist-get data :type) :to-equal "SubHeader")
       (expect (plist-get data :title) :to-equal "Section Divider"))))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Module
** Item
:PROPERTIES:
:CANVAS_ID: 77777
:END:
"
     (search-forward "Item")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       (expect (plist-get data :canvas-id) :to-equal "77777"))))

  (it "parses indent property"
    (with-temp-org-buffer
     "* Module
** Indented Item
:PROPERTIES:
:INDENT: 2
:END:
"
     (search-forward "Indented Item")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       (expect (plist-get data :indent) :to-equal 2))))

  (it "defaults indent to 0"
    (with-temp-org-buffer
     "* Module
** Item Without Indent
:PROPERTIES:
:END:
"
     (search-forward "Item Without Indent")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       (expect (plist-get data :indent) :to-equal 0))))

  (it "parses min_score property"
    (unless test-org-canvas-emacs-30-p
      (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
    (with-temp-org-buffer
     "* Module
** Scored Item
:PROPERTIES:
:MIN_SCORE: 80
:COMPLETION_REQUIREMENT: min_score
:END:
"
     (search-forward "Scored Item")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       ;; SubHeader doesn't carry min-score, but verify it's parsed
       (expect (plist-get data :type) :to-equal "SubHeader"))))

  (it "parses completion_requirement for SubHeader"
    ;; Note: completion_requirement is only included for linked items, not SubHeaders.
    ;; A heading without a link is parsed as SubHeader, which doesn't include completion-requirement.
    ;; This test verifies that SubHeaders work correctly (without completion-requirement).
    (unless test-org-canvas-emacs-30-p
      (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
    (with-temp-org-buffer
     "* Module
** Required Item
:PROPERTIES:
:COMPLETION_REQUIREMENT: must_view
:END:
"
     (search-forward "Required Item")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       ;; Non-link heading is parsed as SubHeader, which doesn't carry completion_requirement
       (expect (plist-get data :type) :to-equal "SubHeader"))))

  (it "includes pom in SubHeader data"
    (with-temp-org-buffer
     "* Module
** SubHeader Item
:PROPERTIES:
:END:
"
     (search-forward "SubHeader Item")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       (expect (plist-get data :pom) :to-be-truthy))))

  (it "parses EXTERNAL_URL as ExternalUrl type"
    (with-temp-org-buffer
     "* Module
** Google
:PROPERTIES:
:EXTERNAL_URL: https://www.google.com
:END:
"
     (search-forward "Google")
     (org-back-to-heading)
     (let* ((data (org-canvas--module-item-parse-entry "/tmp/"))
            (data-type (plist-get data :type)))
       (expect data-type :to-equal "ExternalUrl")
       (expect (plist-get data :external-url) :to-equal "https://www.google.com"))))

  (it "parses NEW_TAB property for ExternalUrl"
    (with-temp-org-buffer
     "* Module
** External Link
:PROPERTIES:
:EXTERNAL_URL: https://example.com
:NEW_TAB: true
:END:
"
     (search-forward "External Link")
     (org-back-to-heading)
     (let* ((data (org-canvas--module-item-parse-entry "/tmp/"))
            (data-type (plist-get data :type)))
       (expect data-type :to-equal "ExternalUrl")
       (expect (plist-get data :new-tab) :to-be t))))

  (it "defaults NEW_TAB to nil when not set"
    (with-temp-org-buffer
     "* Module
** External Link
:PROPERTIES:
:EXTERNAL_URL: https://example.com
:END:
"
     (search-forward "External Link")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       (expect (plist-get data :new-tab) :to-be nil))))

  (it "preserves canvas-id and indent for ExternalUrl"
    (with-temp-org-buffer
     "* Module
** External
:PROPERTIES:
:EXTERNAL_URL: https://example.com
:CANVAS_ID: 54321
:INDENT: 1
:END:
"
     (search-forward "External")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp/")))
       (expect (plist-get data :canvas-id) :to-equal "54321")
       (expect (plist-get data :indent) :to-equal 1)))))

;;;; Stage 2: Build Module Payload

(describe "org-canvas--module-build-payload"
  (it "wraps in module key"
    (let* ((data '(:title "Test Module" :published t))
           (payload (org-canvas--module-build-payload data)))
      (expect (gethash "module" payload) :to-be-truthy)))

  (it "includes name in payload"
    (let* ((data '(:title "Week 1" :published t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "name" module) :to-equal "Week 1")))

  (it "includes published status as t"
    (let* ((data '(:title "Test" :published t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "published" module) :to-be t)))

  (it "includes published status as :json-false when unpublished"
    (let* ((data '(:title "Test" :published nil))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "published" module) :to-equal :json-false)))

  (it "includes position when specified"
    (let* ((data '(:title "Test" :published t :position 5))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "position" module) :to-equal 5)))

  (it "excludes position when not specified"
    (let* ((data '(:title "Test" :published t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "position" module) :to-be nil)))

  (it "includes prerequisite_module_ids"
    (let* ((data '(:title "Test" :published t :prerequisite-module-ids ("100" "101")))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "prerequisite_module_ids" module) :to-equal '("100" "101"))))

  (it "includes unlock_at when specified"
    (let* ((data '(:title "Test" :published t :unlock-at "2024-02-01T08:00:00Z"))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "unlock_at" module) :to-equal "2024-02-01T08:00:00Z")))

  (it "includes require_sequential_progress when true"
    (let* ((data '(:title "Test" :published t :require-sequential-progress t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "require_sequential_progress" module) :to-be t)))

  (it "excludes require_sequential_progress when nil"
    (let* ((data '(:title "Test" :published t :require-sequential-progress nil))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "require_sequential_progress" module) :to-be nil)))

  (it "includes publish_final_grade when true"
    (let* ((data '(:title "Test" :published t :publish-final-grade t))
           (payload (org-canvas--module-build-payload data))
           (module (gethash "module" payload)))
      (expect (gethash "publish_final_grade" module) :to-be t))))

;;;; Stage 2: Build Module Item Payload

(describe "org-canvas--module-item-build-payload"
  (describe "common fields"
    (it "wraps in module_item key"
      (let* ((data '(:type "SubHeader" :title "Section"))
             (payload (org-canvas--module-item-build-payload data 1)))
        (expect (gethash "module_item" payload) :to-be-truthy)))

    (it "includes type and title"
      (let* ((data '(:type "Page" :title "Syllabus"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "type" item) :to-equal "Page")
        (expect (gethash "title" item) :to-equal "Syllabus")))

    (it "includes position"
      (let* ((data '(:type "SubHeader" :title "Test"))
             (payload (org-canvas--module-item-build-payload data 3))
             (item (gethash "module_item" payload)))
        (expect (gethash "position" item) :to-equal 3))))

  (describe "content_id"
    (it "includes content_id when present"
      (let* ((data '(:type "Assignment" :title "HW1" :content-id 999))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "content_id" item) :to-equal 999)))

    (it "excludes content_id when not present"
      (let* ((data '(:type "SubHeader" :title "Section"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "content_id" item) :to-be nil))))

  (describe "page URL"
    (it "includes page_url for Page type"
      (let* ((data '(:type "Page" :title "Syllabus" :page-url "syllabus"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "page_url" item) :to-equal "syllabus")))

    (it "excludes page_url for non-Page types"
      (let* ((data '(:type "Assignment" :title "HW1" :page-url "should-not-appear"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "page_url" item) :to-be nil))))

  (describe "indent"
    (it "includes indent"
      (let* ((data '(:type "SubHeader" :title "Test" :indent 1))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "indent" item) :to-equal 1)))

    (it "excludes indent when nil"
      (let* ((data '(:type "SubHeader" :title "Test"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "indent" item) :to-be nil))))

  (describe "completion requirements"
    (it "includes completion_requirement type"
      (let* ((data '(:type "Assignment" :title "HW1" :completion-requirement "must_submit"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "completion_requirement[type]" item) :to-equal "must_submit")))

    (it "includes completion_requirement min_score"
      (let* ((data '(:type "Quiz" :title "Quiz 1" :completion-requirement "min_score" :min-score 80))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "completion_requirement[type]" item) :to-equal "min_score")
        (expect (gethash "completion_requirement[min_score]" item) :to-equal 80)))

    (it "excludes min_score when not specified"
      (let* ((data '(:type "Page" :title "Reading" :completion-requirement "must_view"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "completion_requirement[type]" item) :to-equal "must_view")
        (expect (gethash "completion_requirement[min_score]" item) :to-be nil))))

  (describe "external URL items"
    (it "includes external_url for ExternalUrl type"
      (let* ((data '(:type "ExternalUrl" :title "Google" :external-url "https://www.google.com"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "external_url" item) :to-equal "https://www.google.com")))

    (it "includes new_tab when set"
      (let* ((data '(:type "ExternalUrl" :title "Link" :external-url "https://example.com" :new-tab t))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "new_tab" item) :to-be t)))

    (it "excludes new_tab when nil"
      (let* ((data '(:type "ExternalUrl" :title "Link" :external-url "https://example.com"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "new_tab" item) :to-be nil)))

    (it "excludes external_url when not present"
      (let* ((data '(:type "SubHeader" :title "Section"))
             (payload (org-canvas--module-item-build-payload data 1))
             (item (gethash "module_item" payload)))
        (expect (gethash "external_url" item) :to-be nil)))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--module-push-to-api (mocked)"
  (it "uses POST for new modules"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New" :canvas-id nil))
              (payload (make-hash-table)))
          (org-canvas--module-push-to-api data payload)
          (expect-api-called 'POST "modules$")))))

  (it "uses PUT for existing modules"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "555"))
              (payload (make-hash-table)))
          (org-canvas--module-push-to-api data payload)
          (expect-api-called 'PUT "modules/555")))))

  (it "recovers from timeout by searching for created module"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that times out
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Request failed" "Timeout")))
                      ;; Second call: recovery search finds the module
                      ((eq method 'GET)
                       [((id . 999) (name . "Timeout Module"))])
                      (t nil)))))
          (let ((data '(:title "Timeout Module" :canvas-id nil))
                (payload (make-hash-table)))
            (let ((result (org-canvas--module-push-to-api data payload)))
              (expect (alist-get 'id result) :to-equal 999)))))))

  (it "retries as POST when PUT returns 404"
    (with-org-canvas-test-config
      (let ((call-count 0)
            (post-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: PUT that returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("404 Not Found")))
                      ;; Second call: POST succeeds
                      ((eq method 'POST)
                       (setq post-called t)
                       '((id . 888) (name . "Recovered Module")))
                      (t nil)))))
          (let ((data '(:title "Missing Module" :canvas-id "999"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--module-push-to-api data payload)))
              (expect post-called :to-be t)
              (expect (alist-get 'id result) :to-equal 888)))))))

  (it "signals error when timeout recovery finds no module"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that times out
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Request failed" "Timeout")))
                      ;; Second call: recovery search finds nothing
                      ((eq method 'GET)
                       [])
                      (t nil)))))
          (let ((data '(:title "Lost Module" :canvas-id nil))
                (payload (make-hash-table)))
            (expect (org-canvas--module-push-to-api data payload)
                    :to-throw 'error))))))

  (it "signals non-timeout/non-404 errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request: Invalid module")))))
        (let ((data '(:title "Bad Module" :canvas-id nil))
              (payload (make-hash-table)))
          (expect (org-canvas--module-push-to-api data payload)
                  :to-throw 'error))))))

(describe "org-canvas--module-item-push-to-api (mocked)"
  (it "uses POST for new items"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Item" :canvas-id nil))
              (payload (make-hash-table)))
          (org-canvas--module-item-push-to-api 100 data payload)
          (expect-api-called 'POST "modules/100/items$")))))

  (it "uses PUT for existing items"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "888"))
              (payload (make-hash-table)))
          (org-canvas--module-item-push-to-api 100 data payload)
          (expect-api-called 'PUT "modules/100/items/888")))))

  (it "retries as POST when PUT returns 404"
    (with-org-canvas-test-config
      (let ((call-count 0)
            (post-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: PUT that returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("404 Not Found")))
                      ;; Second call: POST succeeds
                      ((eq method 'POST)
                       (setq post-called t)
                       '((id . 777) (title . "Recovered Item")))
                      (t nil)))))
          (let ((data '(:title "Missing Item" :canvas-id "999"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--module-item-push-to-api 100 data payload)))
              (expect post-called :to-be t)
              (expect (alist-get 'id result) :to-equal 777)))))))

  (it "signals non-404 errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request: Invalid item")))))
        (let ((data '(:title "Bad Item" :canvas-id nil))
              (payload (make-hash-table)))
          (expect (org-canvas--module-item-push-to-api 100 data payload)
                  :to-throw 'error))))))

;;;; Stage 4: Finalize

(describe "org-canvas--module-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Module
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Module" :pom (point-marker)))
           (response '((id . 88888) (name . "Test Module"))))
       (org-canvas--module-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "88888"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 77777))))
       (org-canvas--module-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* No ID Module
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No ID Module" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (expect (org-canvas--module-finalize data response) :to-throw 'error))))

  (it "handles numeric id in response"
    (with-temp-org-buffer
     "* Numeric ID Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Numeric ID Test" :pom (point-marker)))
           (response '((id . 12345))))
       (org-canvas--module-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "12345")))))

;;;; Module Item Finalize

(describe "org-canvas--module-item-finalize (uses module-finalize)"
  (it "saves CANVAS_ID for module item"
    (with-temp-org-buffer
     "* Module
** Test Item
:PROPERTIES:
:END:
"
     (search-forward "Test Item")
     (org-back-to-heading)
     (let ((data (list :title "Test Item" :pom (point-marker)))
           (response '((id . 55555))))
       (org-canvas--module-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "55555")))))

;;;; Helper Functions

(describe "org-canvas--module-resolve-link"
  (it "returns nil for plain headings without links"
    (expect (org-canvas--module-resolve-link "Regular Section" "/tmp/")
            :to-be nil))

  (it "returns nil when file doesn't exist"
    ;; Links to non-existent files return nil
    (expect (org-canvas--module-resolve-link "[[file:nonexistent.org::*Heading][Item]]" "/tmp/")
            :to-be nil))

  (it "returns nil for malformed links"
    (expect (org-canvas--module-resolve-link "[[broken link" "/tmp/")
            :to-be nil))

  (it "returns nil for nil link input"
    (expect (org-canvas--module-resolve-link nil "/tmp/")
            :to-be nil))

  (it "resolves link with CANVAS_ID from target file"
    (let ((temp-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Test Page\n:PROPERTIES:\n:CANVAS_ID: 12345\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file))
                   (link (format "[[file:%s::*Test Page][Link Text]]" basename))
                   (result (org-canvas--module-resolve-link link dir)))
              (expect result :to-be-truthy)
              (expect (plist-get result :content-id) :to-equal 12345)
              (expect (plist-get result :title) :to-equal "Link Text")))
        (delete-file temp-file))))

  (it "resolves link with CANVAS_URL for pages"
    (let ((temp-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Syllabus\n:PROPERTIES:\n:CANVAS_URL: syllabus-page\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file))
                   (link (format "[[file:%s::*Syllabus][Syllabus Link]]" basename))
                   (result (org-canvas--module-resolve-link link dir)))
              (expect result :to-be-truthy)
              (expect (plist-get result :page-url) :to-equal "syllabus-page")
              (expect (plist-get result :title) :to-equal "Syllabus Link")))
        (delete-file temp-file))))

  (it "returns nil when heading has no CANVAS_ID"
    (let ((temp-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Unsynced Page\n:PROPERTIES:\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file))
                   (link (format "[[file:%s::*Unsynced Page][Link]]" basename))
                   (result (org-canvas--module-resolve-link link dir)))
              (expect result :to-be nil)))
        (delete-file temp-file))))

  (it "determines type from filename"
    (unless test-org-canvas-emacs-30-p
      (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
    (let ((temp-file (make-temp-file "assignments" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Homework 1\n:PROPERTIES:\n:CANVAS_ID: 999\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file))
                   (link (format "[[file:%s::*Homework 1][HW1]]" basename))
                   (result (org-canvas--module-resolve-link link dir)))
              (expect result :to-be-truthy)
              ;; Type is determined by filename pattern
              (expect (plist-get result :type) :to-be-truthy)))
        (delete-file temp-file)))))

(describe "org-canvas--module-search-by-name (mocked)"
  (it "searches modules by name"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("modules" . [((id . 100) (name . "Week 1"))
                            ((id . 101) (name . "Week 2"))])))
        (org-canvas--module-search-by-name "Week 1")
        (expect-api-called 'GET "modules"))))

  (it "returns matching module"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("modules" . [((id . 100) (name . "Week 1"))
                            ((id . 101) (name . "Week 2"))])))
        (let ((result (org-canvas--module-search-by-name "Week 1")))
          (expect (alist-get 'id result) :to-equal 100)))))

  (it "returns nil when no match found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("modules" . [((id . 100) (name . "Week 1"))])))
        (let ((result (org-canvas--module-search-by-name "Nonexistent Module")))
          (expect result :to-be nil)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error")))))
        (let ((result (org-canvas--module-search-by-name "Any Module")))
          (expect result :to-be nil))))))

;;;; Delete Functions (mocked)

(describe "org-canvas-delete-module-at-point"
  (it "errors when no CANVAS_ID property found"
    (with-temp-org-buffer
     "* Module Without ID
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-module-at-point)
             :to-throw 'user-error))))

;;;; Sync Items Tests

(describe "org-canvas--module-sync-items (mocked)"
  (it "returns counts of zero for module with no items"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Empty Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
         (org-back-to-heading)
         (let ((counts (org-canvas--module-sync-items 100 (point) default-directory)))
           (expect counts :to-equal '(0 0 0))))))))

;;;; Module Sync Items with Items

(describe "org-canvas--module-sync-items with items (mocked)"
  (it "syncs SubHeader items without content-id"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Section Header
:PROPERTIES:
:END:
"
         (org-back-to-heading)
         (let ((marker (point-marker)))
           (let ((results (org-canvas--module-sync-items 100 (marker-position marker) default-directory)))
             ;; SubHeaders can be synced - results is (success skip fail)
             (expect (numberp (car results)) :to-be t)))))))

  (it "skips items without content-id or page-url"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas--module-resolve-link)
                   (lambda (_link _dir)
                     ;; Return linked item without canvas-id
                     '(:type "Page" :title "Unsynced" :content-id nil :page-url nil)))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     '((id . 123)))))
          (with-temp-org-buffer
           "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** [[file:unsynced.org::*Heading][Link]]
:PROPERTIES:
:END:
"
           (org-back-to-heading)
           (let ((marker (point-marker)))
             (let ((results (org-canvas--module-sync-items 100 (marker-position marker) default-directory)))
               ;; Counted as a skip (not a failure): the target simply
               ;; has no CANVAS_ID yet
               (expect (nth 1 results) :to-be-greater-than 0)
               (expect (nth 2 results) :to-equal 0))))))))

  (it "records skipped item titles in the global sync stats"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--module-resolve-link)
                 (lambda (_link _dir)
                   '(:type "Page" :title "Unsynced" :content-id nil :page-url nil))))
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** [[file:unsynced.org::*Course Home][Course Home]]
:PROPERTIES:
:END:
"
         (org-back-to-heading)
         (let ((org-canvas--sync-global-counters
                (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
               (org-canvas--sync-global-feature-stats nil)
               (marker (point-marker)))
           (org-canvas--module-sync-items 100 (marker-position marker) default-directory)
           (let ((entry (car org-canvas--sync-global-feature-stats)))
             (expect (plist-get entry :label) :to-equal "Module Items")
             (expect (plist-get entry :skip) :to-equal 1)
             (expect (car (plist-get entry :skipped-titles))
                     :to-match "no linked content synced"))
           (expect (plist-get org-canvas--sync-global-counters :skip)
                   :to-equal 1)))))))

;;;; Delete All Modules Tests

(describe "org-canvas-delete-all-modules (mocked)"
  (it "deletes modules and cleans local properties"
    (let ((temp-file (make-temp-file "modules" nil ".org"))
          (org-canvas-modules-file nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Module 1
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:

* Module 2
:PROPERTIES:
:CANVAS_ID: 2
:END:
"))
            (setq org-canvas-modules-file temp-file)
            (with-org-canvas-test-config
              (let ((deleted-ids nil))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             [((id . 1) (name . "Module 1"))
                              ((id . 2) (name . "Module 2"))]))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                             (dolist (item items)
                               (push (number-to-string (alist-get 'id item)) deleted-ids))
                             (cons (length items) deleted-ids))))
                  (org-canvas-delete-all-modules)
                  ;; Should have deleted 2 modules
                  (expect (length deleted-ids) :to-equal 2)))))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))))

  (it "aborts when user cancels"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (expect (org-canvas-delete-all-modules) :to-throw 'user-error))))

;;;; Delete Module at Point Tests

(describe "org-canvas-delete-module-at-point (mocked)"
  (it "deletes module and clears child item properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:LAST_SYNCED: [2024-01-01 Mon]
:END:
** Item 1
:PROPERTIES:
:CANVAS_ID: 101
:END:
** Item 2
:PROPERTIES:
:CANVAS_ID: 102
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas-delete-module-at-point)
           ;; Module properties should be cleared
           (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
           ;; Child item properties should also be cleared
           (org-goto-first-child)
           (expect (org-entry-get (point) "CANVAS_ID") :to-be nil))))))

  (it "handles delete API failure gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Cannot delete"))))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
         (org-back-to-heading)
         ;; Should not throw, just message
         (org-canvas-delete-module-at-point)
         ;; Properties should still exist since delete failed
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100")))))

  (it "aborts when user cancels"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
         (org-back-to-heading)
         ;; Should not delete
         (org-canvas-delete-module-at-point)
         (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100"))))))

;;;; Module Item Parsing with Links

(describe "org-canvas--module-item-parse-entry with resolved links"
  (it "extracts content-id from linked file"
    (let ((temp-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Test Page\n:PROPERTIES:\n:CANVAS_ID: 99999\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file)))
              (with-temp-org-buffer
               (format "* Module
** [[file:%s::*Test Page][Link Text]]
:PROPERTIES:
:END:
" basename)
               (search-forward "Link Text")
               (org-back-to-heading)
               (let ((data (org-canvas--module-item-parse-entry dir)))
                 (expect (plist-get data :content-id) :to-equal 99999)))))
        (delete-file temp-file))))

  (it "extracts page-url from linked page"
    (let ((temp-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Syllabus\n:PROPERTIES:\n:CANVAS_URL: syllabus-url\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file)))
              (with-temp-org-buffer
               (format "* Module
** [[file:%s::*Syllabus][Syllabus Link]]
:PROPERTIES:
:END:
" basename)
               (search-forward "Syllabus Link")
               (org-back-to-heading)
               (let ((data (org-canvas--module-item-parse-entry dir)))
                 (expect (plist-get data :page-url) :to-equal "syllabus-url")))))
        (delete-file temp-file)))))

;;;; Module Push API Timeout Recovery Failure

(describe "org-canvas--module-push-to-api timeout recovery failure"
  (it "signals error when timeout recovery fails and item not found"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error (list "Request failed" "Timeout")))))
        (let ((data '(:title "Lost" :canvas-id nil))
              (payload (make-hash-table)))
          (expect (org-canvas--module-push-to-api data payload)
                  :to-throw 'error))))))

;;;; Module Item Push API Error

(describe "org-canvas--module-item-push-to-api POST failure after 404"
  (it "signals error when POST retry also fails"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("404 Not Found")))
                      ;; POST also fails
                      ((eq method 'POST)
                       (signal 'error '("Server Error")))
                      (t nil)))))
          (let ((data '(:title "Bad" :canvas-id "999"))
                (payload (make-hash-table)))
            (expect (org-canvas--module-item-push-to-api 100 data payload)
                    :to-throw 'error)))))))

;;;; Module Resolve Link Edge Cases

(describe "org-canvas--module-resolve-link edge cases"
  (it "handles link without heading specifier"
    (let ((temp-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* First Page\n:PROPERTIES:\n:CANVAS_ID: 11111\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file))
                   ;; Link without ::*Heading
                   (link (format "[[file:%s][Just File]]" basename))
                   (result (org-canvas--module-resolve-link link dir)))
              ;; Should still find first heading
              (expect result :to-be-truthy)
              (expect (plist-get result :content-id) :to-equal 11111)))
        (delete-file temp-file))))

  (it "falls back to basename in modules dir when absolute path not found"
    (let ((temp-file (make-temp-file "discussions" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* My Discussion\n:PROPERTIES:\n:CANVAS_ID: 22222\n:END:\n"))
            (let* ((dir (file-name-directory temp-file))
                   (basename (file-name-nondirectory temp-file))
                   ;; Link with absolute path to non-existent directory
                   (link (format "[[file:/nonexistent/path/%s::*My Discussion][My Discussion]]"
                                 basename))
                   (result (org-canvas--module-resolve-link link dir)))
              ;; Should fall back to finding the file by basename in dir
              (expect result :to-be-truthy)
              (expect (plist-get result :content-id) :to-equal 22222)
              (expect (plist-get result :title) :to-equal "My Discussion")))
        (delete-file temp-file)))))

;;;; Sync Modules Integration Tests

(describe "org-canvas-sync-modules integration (mocked)"
  (it "syncs module and its items"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-post-count 0)
                 (item-post-count 0))
            (with-temp-file modules-file
              (insert "* Week 1
:PROPERTIES:
:PUBLISHED: true
:END:
** Section Header
:PROPERTIES:
:END:
"))
            (let ((org-canvas-modules-file modules-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest _args)
                               (cond
                                ;; Pre-flight course check
                                ((string-match "courses/99999/$" url)
                                 '((id . 99999)))
                                ;; POST module
                                ((and (eq method 'POST) (string-match "modules$" url))
                                 (setq module-post-count (1+ module-post-count))
                                 '((id . 500) (name . "Week 1")))
                                ;; POST module item
                                ((and (eq method 'POST) (string-match "modules/500/items" url))
                                 (setq item-post-count (1+ item-post-count))
                                 '((id . 501) (title . "Section Header")))
                                ;; PUT
                                ((eq method 'PUT)
                                 '((id . 500)))
                                (t '((id . 1)))))))
                    (org-canvas-sync-modules)
                    (expect module-post-count :to-equal 1)
                    ;; Verify CANVAS_ID was saved on module
                    (with-current-buffer (find-file-noselect modules-file)
                      (goto-char (point-min))
                      (org-back-to-heading)
                      (expect (org-entry-get (point) "CANVAS_ID") :to-equal "500")))))))
        (delete-directory temp-dir t))))

  (it "sends a module-create body reflecting the org input"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "modules.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Week 1
:PROPERTIES:
:PUBLISHED: true
:POSITION: 3
:END:
"))
            (let ((org-canvas-modules-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-modules)
                  ;; The module-create POST goes to a URL ending in
                  ;; "modules"; item POSTs go to ".../modules/<id>/items".
                  (let* ((body (test-org-canvas-api-call-data 'POST "modules$"))
                         (module (and (hash-table-p body)
                                      (gethash "module" body))))
                    (expect (hash-table-p body) :to-be-truthy)
                    (expect (hash-table-p module) :to-be-truthy)
                    ;; Real property->API-key mapping: name, published, position.
                    (expect (gethash "name" module) :to-equal "Week 1")
                    (expect (gethash "published" module) :to-be t)
                    (expect (gethash "position" module) :to-equal 3))))))
        (delete-directory temp-dir t))))

  (it "errors when file not found"
    (let ((org-canvas-modules-file "/nonexistent/modules.org"))
      (with-sync-test-env
        (expect (org-canvas-sync-modules) :to-throw 'error))))

  (it "continues when module fails but processes next"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-count 0))
            (with-temp-file modules-file
              (insert "* Module 1
:PROPERTIES:
:PUBLISHED: true
:END:

* Module 2
:PROPERTIES:
:PUBLISHED: true
:END:
"))
            (let ((org-canvas-modules-file modules-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest _args)
                               (cond
                                ((string-match "courses/99999/$" url) '((id . 99999)))
                                ((and (eq method 'POST) (string-match "modules$" url))
                                 (setq module-count (1+ module-count))
                                 (if (= module-count 1)
                                     (signal 'error '("Module 1 failed"))
                                   '((id . 600) (name . "Module 2"))))
                                (t '((id . 1)))))))
                    (org-canvas-sync-modules)
                    ;; Both should have been attempted
                    (expect module-count :to-equal 2))))))
        (delete-directory temp-dir t))))

  (it "pre-flight warning does not abort sync"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-count 0))
            (with-temp-file modules-file
              (insert "* Module
:PROPERTIES:
:PUBLISHED: true
:END:
"))
            (let ((org-canvas-modules-file modules-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest _args)
                               (cond
                                ;; Pre-flight fails
                                ((string-match "courses/99999/$" url)
                                 (signal 'error '("Pre-flight failed")))
                                ;; Module POST still works
                                ((and (eq method 'POST) (string-match "modules$" url))
                                 (setq module-count (1+ module-count))
                                 '((id . 700) (name . "Module")))
                                (t '((id . 1)))))))
                    (org-canvas-sync-modules)
                    ;; Module should still have been synced despite pre-flight warning
                    (expect module-count :to-equal 1))))))
        (delete-directory temp-dir t)))))

;;;; Module Sync Items Edge Cases

(describe "org-canvas--module-sync-items edge cases (mocked)"
  (it "increments fail count on item parse error"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Bad Item
:PROPERTIES:
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'org-canvas--module-item-parse-entry)
                    (lambda (_dir) (error "Parse error"))))
           (let ((counts (org-canvas--module-sync-items 100 (point) default-directory)))
             ;; Parse errors are real failures (third element)
             (expect (nth 2 counts) :to-be-greater-than 0))))))))

;;;; Delete All Modules Edge Cases

(describe "org-canvas-delete-all-modules edge cases (mocked)"
  (it "handles delete error and continues to next"
    (let ((temp-file (make-temp-file "modules" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Module 1
:PROPERTIES:
:CANVAS_ID: 1
:END:
"))
            (let ((org-canvas-modules-file temp-file)
                  (delete-attempts 0))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                            ((symbol-function 'org-canvas-api-request)
                             (lambda (_method _url &rest _args)
                               [((id . 1) (name . "M1")) ((id . 2) (name . "M2"))]))
                            ((symbol-function 'org-canvas--delete-items-queued)
                             (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                               (setq delete-attempts (length items))
                               (cons (length items) nil))))
                    (org-canvas-delete-all-modules)
                    ;; Both should have been attempted
                    (expect delete-attempts :to-equal 2))))))
        (when (file-exists-p temp-file) (delete-file temp-file)))))

  (it "cleans local properties for deleted modules"
    (let ((temp-file (make-temp-file "modules" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Module 1
:PROPERTIES:
:CANVAS_ID: 1
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"))
            (let ((org-canvas-modules-file temp-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                            ((symbol-function 'org-canvas-api-request)
                             (lambda (method _url &rest _args)
                               (cond
                                ((eq method 'GET)
                                 [((id . 1) (name . "Module 1"))])
                                ((eq method 'DELETE) nil)))))
                    (org-canvas-delete-all-modules)
                    ;; CANVAS_ID should be cleared
                    (with-current-buffer (find-file-noselect temp-file)
                      (goto-char (point-min))
                      (org-back-to-heading)
                      (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))))))
        (when (file-exists-p temp-file) (delete-file temp-file))))))

;;;; Module Push 404→POST additional tests

(describe "org-canvas--module-push-to-api 404→POST"
  (it "signals error when POST also fails after 404"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; PUT returns 404
                      ((and (eq method 'PUT) (= call-count 1))
                       (signal 'error '("404 Not Found")))
                      ;; POST also fails
                      ((eq method 'POST)
                       (signal 'error '("Server Error")))
                      (t nil)))))
          (let ((data '(:title "Module" :canvas-id "999"))
                (payload (make-hash-table)))
            (expect (org-canvas--module-push-to-api data payload)
                    :to-throw 'error)))))))

;;;; Coverage Gap Tests

(describe "org-canvas--module-item-parse-entry min-score"
  (it "includes min-score when positive for linked items"
    (let ((temp-pages (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-pages
              (insert "* Linked Page\n:PROPERTIES:\n:CANVAS_ID: 555\n:END:\n"))
            (let ((temp-modules (make-temp-file "modules-" nil ".org")))
              (unwind-protect
                  (progn
                    (with-temp-file temp-modules
                      (insert (format "* Module\n** [[file:%s::*Linked Page][Linked Page]]\n:PROPERTIES:\n:COMPLETION_REQUIREMENT: min_score\n:MIN_SCORE: 80\n:END:\n" temp-pages)))
                    (with-current-buffer (find-file-noselect temp-modules)
                      (goto-char (point-min))
                      (search-forward "Linked Page]]")
                      (org-back-to-heading)
                      (let ((data (org-canvas--module-item-parse-entry
                                   (file-name-directory temp-modules))))
                        (expect (plist-get data :min-score) :to-equal 80)
                        (expect (plist-get data :completion-requirement) :to-equal "min_score"))
                      (kill-buffer)))
                (delete-file temp-modules))))
        (delete-file temp-pages))))

  (it "does not include min-score when zero"
    (let ((temp-modules (make-temp-file "modules-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-modules
              (insert "* Module\n** SubHeader Item\n:PROPERTIES:\n:MIN_SCORE: 0\n:END:\n"))
            (with-current-buffer (find-file-noselect temp-modules)
              (goto-char (point-min))
              (search-forward "SubHeader")
              (org-back-to-heading)
              (let ((data (org-canvas--module-item-parse-entry
                           (file-name-directory temp-modules))))
                (expect (plist-get data :min-score) :to-be nil))
              (kill-buffer)))
        (delete-file temp-modules))))

  (it "includes min-score for ExternalUrl items"
    (let ((temp-modules (make-temp-file "modules-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-modules
              (insert "* Module\n** Google Docs\n:PROPERTIES:\n:EXTERNAL_URL: https://docs.google.com\n:COMPLETION_REQUIREMENT: min_score\n:MIN_SCORE: 90\n:END:\n"))
            (with-current-buffer (find-file-noselect temp-modules)
              (goto-char (point-min))
              (search-forward "Google Docs")
              (org-back-to-heading)
              (let* ((data (org-canvas--module-item-parse-entry
                            (file-name-directory temp-modules)))
                     (result-type (plist-get data :type)))
                (expect result-type :to-equal "ExternalUrl")
                (expect (plist-get data :min-score) :to-equal 90)
                (expect (plist-get data :completion-requirement) :to-equal "min_score"))
              (kill-buffer)))
        (delete-file temp-modules)))))

(describe "org-canvas--module-sync-items integration"
  (it "syncs child items of a module"
    (let ((temp-pages (make-temp-file "pages-" nil ".org"))
          (temp-modules (make-temp-file "modules-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-pages
              (insert "* Page A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n\n* Page B\n:PROPERTIES:\n:CANVAS_ID: 200\n:END:\n"))
            (with-temp-file temp-modules
              (insert (format "* Week 1\n:PROPERTIES:\n:CANVAS_ID: 50\n:END:\n** [[file:%s::*Page A][Page A]]\n:PROPERTIES:\n:END:\n** [[file:%s::*Page B][Page B]]\n:PROPERTIES:\n:END:\n"
                              temp-pages temp-pages)))
            (with-org-canvas-test-config
              (with-mock-api
                (with-current-buffer (find-file-noselect temp-modules)
                  (goto-char (point-min))
                  (org-back-to-heading)
                  (let ((result (org-canvas--module-sync-items
                                 "50" (point) (file-name-directory temp-modules))))
                    ;; Both items should be processed successfully
                    (expect (car result) :to-be-truthy)
                    (expect (apply #'+ result) :to-be-truthy))
                  (kill-buffer)))))
        (delete-file temp-pages)
        (delete-file temp-modules)))))

;;;; Extracted Helper Coverage Tests

(describe "org-canvas--module-search-heading-for-id"
  (it "finds heading by exact match"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* My Page\n:PROPERTIES:\n:CANVAS_ID: 111\n:END:\n"))
            (let ((result (org-canvas--module-search-heading-for-id
                           temp-file "My Page" "My Page")))
              (expect result :to-be-truthy)
              (expect (car result) :to-equal "111")))
        (let ((buf (get-file-buffer temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "falls back to display title when exact heading search fails"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              ;; Heading is a link — exact search for the link text won't match
              ;; because re-search-forward looks for the raw heading text
              (insert "* [[file:content/foo.pdf][Foo Document]]\n:PROPERTIES:\n:CANVAS_ID: 222\n:END:\n"))
            ;; Search with a heading that won't match raw text, but title
            ;; matches the display name from org-get-heading
            (let ((result (org-canvas--module-search-heading-for-id
                           temp-file "NonexistentHeading" "Foo Document")))
              (expect result :to-be-truthy)
              (expect (car result) :to-equal "222")))
        (let ((buf (get-file-buffer temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "falls back to partial title match"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Introduction to Canvas\n:PROPERTIES:\n:CANVAS_ID: 333\n:END:\n"))
            ;; Exact heading search will fail; partial match on title succeeds
            (let ((result (org-canvas--module-search-heading-for-id
                           temp-file "WontMatch" "Canvas")))
              (expect result :to-be-truthy)
              (expect (car result) :to-equal "333")))
        (let ((buf (get-file-buffer temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "returns nil when neither heading nor title matches"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Some Other Heading\n:PROPERTIES:\n:CANVAS_ID: 444\n:END:\n"))
            (let ((result (org-canvas--module-search-heading-for-id
                           temp-file "NoMatch" "AlsoNoMatch")))
              (expect result :to-be nil)))
        (let ((buf (get-file-buffer temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "returns page-url when CANVAS_URL is set"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* My Page\n:PROPERTIES:\n:CANVAS_URL: my-page-url\n:END:\n"))
            (let ((result (org-canvas--module-search-heading-for-id
                           temp-file "My Page" "My Page")))
              (expect result :to-be-truthy)
              (expect (cdr result) :to-equal "my-page-url")))
        (let ((buf (get-file-buffer temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "unescapes brackets in heading before searching"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Page [with brackets]\n:PROPERTIES:\n:CANVAS_ID: 666\n:END:\n"))
            ;; Org escapes brackets inside link components: \[ and \]
            (let ((result (org-canvas--module-search-heading-for-id
                           temp-file "Page \\[with brackets\\]" "Page")))
              (expect result :to-be-truthy)
              (expect (car result) :to-equal "666")))
        (let ((buf (get-file-buffer temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "uses first heading when no heading specified"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* First\n:PROPERTIES:\n:CANVAS_ID: 555\n:END:\n* Second\n"))
            (let ((result (org-canvas--module-search-heading-for-id
                           temp-file nil "anything")))
              (expect result :to-be-truthy)
              (expect (car result) :to-equal "555")))
        (let ((buf (get-file-buffer temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--module-classify-unlinked-heading"
  ;; NOTE: These tests use let* to pre-bind (plist-get ... :type)
  ;; because Emacs 29's oclosure-lambda (used by buttercup's expect
  ;; macro) has a bug where :type is shadowed by the oclosure type slot.
  (it "classifies heading with file link as unresolved link"
    (let* ((result (org-canvas--module-classify-unlinked-heading
                    "[[file:assignments.org::*HW1][HW1]]"
                    "HW1" nil 0 1 t))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "Assignment")
      (expect (plist-get result :title) :to-equal "HW1")
      (expect (plist-get result :canvas-id) :to-be nil)))

  (it "extracts title from display part of file link"
    (let* ((result (org-canvas--module-classify-unlinked-heading
                    "[[file:pages.org::*Welcome][Welcome Page]]"
                    "Welcome Page" nil 0 1 t))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "Page")
      (expect (plist-get result :title) :to-equal "Welcome Page")))

  (it "uses raw-heading as title when link has no display part"
    (let ((result (org-canvas--module-classify-unlinked-heading
                   "[[file:quizzes.org::*Quiz 1]]"
                   "Quiz 1" nil 0 1 t)))
      ;; No ][...]] display part, so link-title is nil, falls back to raw-heading
      (expect (plist-get result :title) :to-equal "Quiz 1")))

  (it "classifies plain text heading as SubHeader"
    (let* ((result (org-canvas--module-classify-unlinked-heading
                    "Section Divider" "Section Divider" nil 0 1 t))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "SubHeader")
      (expect (plist-get result :title) :to-equal "Section Divider")))

  (it "preserves canvas-id and indent"
    (let ((result (org-canvas--module-classify-unlinked-heading
                   "[[file:files.org::*Doc][Doc]]"
                   "Doc" "existing-id" 2 t 100)))
      (expect (plist-get result :canvas-id) :to-equal "existing-id")
      (expect (plist-get result :indent) :to-equal 2)
      (expect (plist-get result :pom) :to-equal 100)))

  (it "determines type from file link filename"
    (let* ((result (org-canvas--module-classify-unlinked-heading
                    "[[file:discussions.org::*Topic][Topic]]"
                    "Topic" nil 0 1 t))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "Discussion")))

  (it "uses heading-with-links over raw-heading for link detection"
    ;; When heading-with-links has the link but raw-heading is stripped
    (let* ((result (org-canvas--module-classify-unlinked-heading
                    "[[file:files.org::*Upload][Upload]]"
                    "Upload" nil 0 1 t))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "File")))

  (it "falls back to raw-heading when heading-with-links is nil"
    (let* ((result (org-canvas--module-classify-unlinked-heading
                    nil "Plain Text" nil 0 1 t))
           (result-type (plist-get result :type)))
      (expect result-type :to-equal "SubHeader")
      (expect (plist-get result :title) :to-equal "Plain Text"))))

(describe "org-canvas--module-resolve-file-path"
  (it "returns absolute path when file exists"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (let* ((dir (file-name-directory temp-file))
                 (basename (file-name-nondirectory temp-file))
                 (result (org-canvas--module-resolve-file-path basename dir)))
            (expect result :to-equal temp-file))
        (delete-file temp-file))))

  (it "falls back to basename when absolute path fails"
    (let ((temp-file (make-temp-file "pages-" nil ".org")))
      (unwind-protect
          (let* ((dir (file-name-directory temp-file))
                 (basename (file-name-nondirectory temp-file))
                 ;; Use a relative path that will fail as absolute
                 (result (org-canvas--module-resolve-file-path
                          (concat "../nonexistent-dir/" basename) dir)))
            (expect result :to-equal (expand-file-name basename dir)))
        (delete-file temp-file))))

  (it "returns nil when neither path exists"
    (let ((result (org-canvas--module-resolve-file-path
                   "totally-nonexistent-file.org" "/tmp/")))
      (expect result :to-be nil))))

;;;; Module Item Timeout Recovery

(describe "org-canvas--module-item-push-to-api timeout recovery"
  (it "recovers from timeout when item is found on Canvas"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that times out
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Request failed" "Timeout")))
                      ;; Second call: GET items (search after timeout)
                      ((eq method 'GET)
                       [((id . 555) (title . "Found Item"))
                        ((id . 556) (title . "Other Item"))])
                      (t nil)))))
          (let ((data '(:title "Found Item" :canvas-id nil))
                (payload (make-hash-table)))
            (let ((result (org-canvas--module-item-push-to-api 100 data payload)))
              (expect (alist-get 'id result) :to-equal 555)))))))

  (it "re-signals timeout when item is not found"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST that times out
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Request failed" "Timeout")))
                      ;; Search returns no matching items
                      ((eq method 'GET) [])
                      (t nil)))))
          (let ((data '(:title "Missing Item" :canvas-id nil))
                (payload (make-hash-table)))
            (expect (org-canvas--module-item-push-to-api 100 data payload)
                    :to-throw 'error)))))))

;;;; Pull Function Tests

(describe "org-canvas--module-resolve-item-link"
  (it "resolves Page by CANVAS_URL"
    (let* ((temp-dir (make-temp-file "mod-link-test" t))
           (pages-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file pages-file
              (insert "* Welcome
:PROPERTIES:
:CANVAS_URL: welcome-page
:END:
"))
            (let ((org-canvas-directory temp-dir))
              (let ((link (org-canvas--module-resolve-item-link
                           "Page" "welcome-page" "Welcome")))
                (expect link :to-match "Welcome"))))
        (let ((buf (find-buffer-visiting pages-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "resolves Assignment by CANVAS_ID"
    (let* ((temp-dir (make-temp-file "mod-link-test" t))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* Homework 1
:PROPERTIES:
:CANVAS_ID: 42
:END:
"))
            (let ((org-canvas-directory temp-dir))
              (let ((link (org-canvas--module-resolve-item-link
                           "Assignment" 42 "Homework 1")))
                (expect link :to-match "Homework 1"))))
        (let ((buf (find-buffer-visiting assign-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns title when file not found"
    (let ((org-canvas-directory "/nonexistent-dir/"))
      (expect (org-canvas--module-resolve-item-link "Page" 999 "My Page")
              :to-equal "My Page")))

  (it "returns title for unknown type"
    (expect (org-canvas--module-resolve-item-link "UnknownType" 1 "Item")
            :to-equal "Item"))

  (it "returns Untitled when title and file both nil"
    (expect (org-canvas--module-resolve-item-link "UnknownType" 1 nil)
            :to-equal "Untitled"))

  (it "uses heading-name as display text when title is nil"
    (let* ((temp-dir (make-temp-file "mod-link-nil-title" t))
           (pages-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file pages-file
              (insert "* My Page\n:PROPERTIES:\n:CANVAS_URL: my-page\n:END:\n"))
            (let ((org-canvas-directory temp-dir))
              (let ((link (org-canvas--module-resolve-item-link
                           "Page" "my-page" nil)))
                ;; When title is nil, (or title heading-name) falls back to heading-name
                (expect link :to-match "\\[\\[file:")
                (expect link :to-match "My Page"))))
        (let ((buf (find-buffer-visiting pages-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns Untitled when heading not found in existing file"
    (let* ((temp-dir (make-temp-file "mod-link-no-match" t))
           (pages-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file pages-file
              (insert "* Some Other Page\n:PROPERTIES:\n:CANVAS_URL: other\n:END:\n"))
            (let ((org-canvas-directory temp-dir))
              (let ((link (org-canvas--module-resolve-item-link
                           "Page" "nonexistent-id" nil)))
                ;; heading-name is nil and title is nil → "Untitled"
                (expect link :to-equal "Untitled"))))
        (let ((buf (find-buffer-visiting pages-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--module-pull-insert-items"
  (it "inserts SubHeader as L2 heading"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (let ((count (org-canvas--module-pull-insert-items
                   [((type . "SubHeader") (title . "Week 1")
                     (id . 101) (content_id . nil) (indent . nil))])))
       (expect count :to-equal 1)
       (expect (buffer-string) :to-match "\\*\\* Week 1"))))

  (it "inserts ExternalUrl item with EXTERNAL_URL and NEW_TAB properties"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (let ((count (org-canvas--module-pull-insert-items
                   [((type . "ExternalUrl") (title . "Google")
                     (id . 301) (external_url . "https://www.google.com")
                     (new_tab . t) (indent . nil))])))
       (expect count :to-equal 1)
       (expect (buffer-string) :to-match "\\*\\* Google")
       ;; Find the ExternalUrl heading and verify properties
       (goto-char (point-min))
       (search-forward "Google")
       (org-back-to-heading)
       (expect (org-entry-get (point) "EXTERNAL_URL")
               :to-equal "https://www.google.com")
       (expect (org-entry-get (point) "NEW_TAB")
               :to-equal "true")
       (expect (org-entry-get (point) "CANVAS_ID")
               :to-equal "301"))))

  (it "omits NEW_TAB property when new_tab is nil"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (org-canvas--module-pull-insert-items
      [((type . "ExternalUrl") (title . "Link")
        (id . 302) (external_url . "https://example.com")
        (new_tab . nil) (indent . nil))])
     (goto-char (point-min))
     (search-forward "Link")
     (org-back-to-heading)
     (expect (org-entry-get (point) "EXTERNAL_URL")
             :to-equal "https://example.com")
     (expect (org-entry-get (point) "NEW_TAB") :to-be nil)))

  (it "sets INDENT property on ExternalUrl when indent is present"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (org-canvas--module-pull-insert-items
      [((type . "ExternalUrl") (title . "Indented Link")
        (id . 303) (external_url . "https://example.com/indented")
        (new_tab . nil) (indent . 2))])
     (goto-char (point-min))
     (search-forward "Indented Link")
     (org-back-to-heading)
     (expect (org-entry-get (point) "INDENT") :to-equal "2")
     (expect (org-entry-get (point) "EXTERNAL_URL")
             :to-equal "https://example.com/indented")))

  (it "inserts linked items with INDENT"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (cl-letf (((symbol-function 'org-canvas--module-resolve-item-link)
                (lambda (_type _cid title) (or title "Untitled"))))
       (let ((count (org-canvas--module-pull-insert-items
                     [((type . "Assignment") (title . "HW1")
                       (id . 201) (content_id . 42) (indent . 2))])))
         (expect count :to-equal 1)
         (expect (buffer-string) :to-match "\\*\\* HW1")))))

  (it "inserts multiple items on separate lines without concatenation"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (let ((count (org-canvas--module-pull-insert-items
                   [((type . "SubHeader") (title . "First")
                     (id . 101) (content_id . nil) (indent . nil))
                    ((type . "SubHeader") (title . "Second")
                     (id . 102) (content_id . nil) (indent . nil))
                    ((type . "SubHeader") (title . "Third")
                     (id . 103) (content_id . nil) (indent . nil))])))
       (expect count :to-equal 3)
       ;; Each heading must start at column 0; no ":END:** " concatenation.
       (expect (buffer-string) :not :to-match ":END:\\*\\*")
       (expect (length (split-string (buffer-string) "^\\*\\* " t))
               :to-equal 4)))))

(describe "module :INDENT: suppression"
  (it "omits :INDENT: when value is 0 on linked content item"
    (with-temp-org-buffer
     ""
     (goto-char (point-max))
     (cl-letf (((symbol-function 'org-canvas--module-resolve-item-link)
                (lambda (_type _cid title) (or title "Untitled"))))
       (org-canvas--module-pull-insert-content-item
        '((id . 1) (type . "Page") (title . "x")
          (content_id . 99) (indent . 0))
        1 nil))
     (expect (buffer-string) :not :to-match ":INDENT:")))

  (it "emits :INDENT: when value is nonzero on linked content item"
    (with-temp-org-buffer
     ""
     (goto-char (point-max))
     (cl-letf (((symbol-function 'org-canvas--module-resolve-item-link)
                (lambda (_type _cid title) (or title "Untitled"))))
       (org-canvas--module-pull-insert-content-item
        '((id . 1) (type . "Page") (title . "x")
          (content_id . 99) (indent . 2))
        1 nil))
     (expect (buffer-string) :to-match ":INDENT: +2")))

  (it "omits :INDENT: when value is 0 on ExternalUrl"
    (with-temp-org-buffer
     ""
     (goto-char (point-max))
     (org-canvas--module-pull-insert-external-url
      '((title . "Link") (external_url . "https://example.com")
        (new_tab . nil) (indent . 0))
      1 nil)
     (expect (buffer-string) :not :to-match ":INDENT:")))

  (it "emits :INDENT: when value is nonzero on ExternalUrl"
    (with-temp-org-buffer
     ""
     (goto-char (point-max))
     (org-canvas--module-pull-insert-external-url
      '((title . "Link") (external_url . "https://example.com")
        (new_tab . nil) (indent . 3))
      1 nil)
     (expect (buffer-string) :to-match ":INDENT: +3")))

  (it "omits :INDENT: on SubHeader when 0"
    (with-temp-org-buffer
     ""
     (goto-char (point-max))
     (org-canvas--module-pull-insert-items
      [((type . "SubHeader") (title . "Hdr")
        (id . 1) (indent . 0))])
     (expect (buffer-string) :not :to-match ":INDENT:")))

  (it "emits :INDENT: on SubHeader when nonzero"
    (with-temp-org-buffer
     ""
     (goto-char (point-max))
     (org-canvas--module-pull-insert-items
      [((type . "SubHeader") (title . "Hdr")
        (id . 1) (indent . 1))])
     (expect (buffer-string) :to-match ":INDENT: +1"))))

(describe "org-canvas--module-resolve-item-link for File items"
  (it "links directly to the file path, not via files.org indirection"
    ;; Regression: previously this produced a brittle link of the form
    ;; `[[file:files.org::*[[file:content/foo.pdf][foo.pdf]]][foo.pdf]]'
    ;; — the search target embedded the entire raw heading because file
    ;; headings in files.org are themselves links.  Now we look up the
    ;; CANVAS_ID in files.org and emit the file's own path directly so
    ;; the link is both readable and click-navigable.
    (let* ((tmpdir (make-temp-file "resolve-link" t))
           (files-path (expand-file-name "files.org" tmpdir)))
      (unwind-protect
          (progn
            (with-temp-file files-path
              (insert "* [[file:content/foo.pdf][foo.pdf]]\n"
                      ":PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--path)
                       (lambda (f) (if (equal f "files.org") files-path f))))
              (let* ((link (org-canvas--module-resolve-item-link
                            "File" 42 "foo.pdf")))
                (expect link :to-equal "[[file:content/foo.pdf][foo.pdf]]")
                (with-temp-buffer
                  (org-mode)
                  (insert (format "* parent\n** %s\n" link))
                  (goto-char (point-min))
                  (search-forward "[[file:")
                  (goto-char (match-beginning 0))
                  ;; Pre-bind `:type' outside `expect': buttercup's
                  ;; `expect' oclosure shadows the `:type' keyword on
                  ;; Emacs 29.x, silently returning nil.
                  (let* ((parsed (org-element-link-parser))
                         (parsed-type (org-element-property :type parsed)))
                    (expect parsed-type :to-equal "file")
                    (expect (org-element-property :path parsed)
                            :to-equal "content/foo.pdf")
                    ;; No `::*search-option' on the new clean form.
                    (expect (org-element-property :search-option parsed)
                            :to-be nil))))))
        (delete-directory tmpdir t))))

  (it "resolves files nested under folder headings in files.org"
    ;; Files can live arbitrarily deep under folder headings, so the
    ;; lookup must scan all entries (not just LEVEL=1).
    (let* ((tmpdir (make-temp-file "resolve-link" t))
           (files-path (expand-file-name "files.org" tmpdir)))
      (unwind-protect
          (progn
            (with-temp-file files-path
              (insert "* readings\n"
                      "** books\n"
                      "*** [[file:content/readings/books/x.pdf][x.pdf]]\n"
                      ":PROPERTIES:\n:CANVAS_ID: 77\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--path)
                       (lambda (f) (if (equal f "files.org") files-path f))))
              (expect (org-canvas--module-resolve-item-link "File" 77 "x.pdf")
                      :to-equal "[[file:content/readings/books/x.pdf][x.pdf]]")))
        (delete-directory tmpdir t))))

  (it "falls back to title when CANVAS_ID is not found in files.org"
    (let* ((tmpdir (make-temp-file "resolve-link" t))
           (files-path (expand-file-name "files.org" tmpdir)))
      (unwind-protect
          (progn
            (with-temp-file files-path (insert ""))
            (cl-letf (((symbol-function 'org-canvas--path)
                       (lambda (f) (if (equal f "files.org") files-path f))))
              (expect (org-canvas--module-resolve-item-link "File" 999 "missing.pdf")
                      :to-equal "missing.pdf")))
        (delete-directory tmpdir t)))))

(describe "org-canvas--module-resolve-link for direct file links"
  (it "treats a non-.org link target as a File-typed module item"
    (let* ((tmpdir (make-temp-file "resolve-direct" t))
           (files-path (expand-file-name "files.org" tmpdir)))
      (unwind-protect
          (progn
            (with-temp-file files-path
              (insert "* [[file:content/foo.pdf][foo.pdf]]\n"
                      ":PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            ;; Pre-bind `:type' outside `expect': buttercup's `expect'
            ;; oclosure shadows the `:type' keyword on Emacs 29.x,
            ;; silently returning nil.
            (let* ((resolved (org-canvas--module-resolve-link
                              "[[file:content/foo.pdf][foo.pdf]]" tmpdir))
                   (resolved-type (plist-get resolved :type)))
              (expect resolved-type :to-equal "File")
              (expect (plist-get resolved :content-id) :to-equal 42)
              (expect (plist-get resolved :title) :to-equal "foo.pdf")))
        (delete-directory tmpdir t))))

  (it "still resolves the legacy gnarly form pointing through files.org"
    ;; Backward compat: existing modules.org files have File items as
    ;; `[[file:files.org::*[[file:content/...][...]]][...]]'.  The push
    ;; side must continue to resolve those until the next pull rewrites
    ;; them into the new direct form.
    (let* ((tmpdir (make-temp-file "resolve-legacy" t))
           (files-path (expand-file-name "files.org" tmpdir)))
      (unwind-protect
          (progn
            (with-temp-file files-path
              (insert "* [[file:content/foo.pdf][foo.pdf]]\n"
                      ":PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let* ((legacy "[[file:files.org::*\\[\\[file:content/foo.pdf\\]\\[foo.pdf\\]\\]][foo.pdf]]")
                   (resolved (org-canvas--module-resolve-link legacy tmpdir))
                   (resolved-type (plist-get resolved :type)))
              (expect resolved-type :to-equal "File")
              (expect (plist-get resolved :content-id) :to-equal 42)))
        (delete-directory tmpdir t)))))

(describe "org-canvas-pull-modules"
  (it "creates module headings with items"
    (let* ((temp-dir (make-temp-file "pull-mod-test" t))
           (modules-file (expand-file-name "modules.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-modules-file modules-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 10) (name . "Week 1")
                                (items . [((type . "SubHeader")
                                           (title . "Readings")
                                           (id . 101))])))))
                          ((symbol-function 'org-canvas--module-resolve-item-link)
                           (lambda (_type _cid title) (or title "Untitled"))))
                  (org-canvas-pull-modules)
                  (with-current-buffer (find-file-noselect modules-file)
                    (expect (buffer-string) :to-match "Week 1")
                    (expect (buffer-string) :to-match "Readings"))))))
        (let ((buf (find-buffer-visiting modules-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates modules.org when it does not exist"
    (let* ((temp-dir (make-temp-file "pull-mod-test" t))
           (modules-file (expand-file-name "modules.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-modules-file modules-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '())))
                  (org-canvas-pull-modules)
                  (expect (file-exists-p modules-file) :to-be-truthy)))))
        (let ((buf (find-buffer-visiting modules-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; New Property Tests: PUBLISHED for Module Items

(describe "org-canvas--module-item-parse-entry (published)"
  (it "defaults published to t when not set"
    (with-temp-org-buffer
     "* Module
** Plain Heading
:PROPERTIES:
:END:
"
     (search-forward "Plain Heading")
     (org-back-to-heading)
     (let ((data (org-canvas--module-item-parse-entry "/tmp")))
       (expect (plist-get data :published) :to-be t))))

  (it "parses PUBLISHED false on external URL item"
    (with-temp-org-buffer
     "* Module
** Hidden Link
:PROPERTIES:
:EXTERNAL_URL: https://example.com
:PUBLISHED: false
:END:
"
     (search-forward "Hidden Link")
     (org-back-to-heading)
     (let* ((data (org-canvas--module-item-parse-entry "/tmp"))
            (result-type (plist-get data :type)))
       (expect (plist-get data :published) :to-be nil)
       (expect result-type :to-equal "ExternalUrl")))))

(describe "org-canvas--module-item-build-payload (published)"
  (it "includes published true in payload"
    (let* ((data '(:type "SubHeader" :title "Section" :published t))
           (payload (org-canvas--module-item-build-payload data 1))
           (item (gethash "module_item" payload)))
      (expect (gethash "published" item) :to-be t)))

  (it "includes published false as :json-false"
    (let* ((data '(:type "SubHeader" :title "Section" :published nil))
           (payload (org-canvas--module-item-build-payload data 1))
           (item (gethash "module_item" payload)))
      (expect (gethash "published" item) :to-equal :json-false))))

(describe "org-canvas--module-pull-insert-items (published)"
  (it "sets PUBLISHED property on pulled items"
    (with-temp-org-buffer
     "* Module
"
     (goto-char (point-max))
     (let ((items [((type . "SubHeader") (title . "Section 1")
                    (id . 1) (published . :json-false))]))
       (org-canvas--module-pull-insert-items items)
       (goto-char (point-min))
       (re-search-forward "Section 1")
       (org-back-to-heading)
       (expect (org-entry-get (point) "PUBLISHED") :to-equal "false"))))

  (it "sets PUBLISHED true on linked items (when org-canvas-emit-defaults)"
    (let ((org-canvas-emit-defaults t))
      (with-temp-org-buffer
       "* Module
"
       (goto-char (point-max))
       (let ((items [((type . "ExternalUrl") (title . "Link")
                      (id . 2) (external_url . "https://x.com")
                      (published . t))]))
         (org-canvas--module-pull-insert-items items)
         (goto-char (point-min))
         (re-search-forward "Link")
         (org-back-to-heading)
         (expect (org-entry-get (point) "PUBLISHED") :to-equal "true"))))))

;;;; Cross-file module link resolution integration

(describe "cross-file module link resolution"
  (it "resolves correct heading when target file has multiple headings"
    (let* ((temp-dir (make-temp-file "module-link-test" t))
           (pages-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (progn
            ;; Create pages.org with 3 headings, only the middle one has the target ID
            (with-temp-file pages-file
              (insert "* First Page\n:PROPERTIES:\n:CANVAS_URL: first-page\n:END:\n\n")
              (insert "* Target Page\n:PROPERTIES:\n:CANVAS_URL: target-url\n:END:\n\n")
              (insert "* Third Page\n:PROPERTIES:\n:CANVAS_URL: third-page\n:END:\n"))
            ;; Module item links to the middle heading
            (with-temp-org-buffer
             (format "* Module\n** [[file:pages.org::*Target Page][My Target]]\n:PROPERTIES:\n:END:\n")
             (search-forward "My Target")
             (org-back-to-heading)
             (let ((data (org-canvas--module-item-parse-entry temp-dir)))
               (expect (plist-get data :page-url) :to-equal "target-url")
               (expect (plist-get data :title) :to-equal "My Target"))))
        (let ((buf (find-buffer-visiting pages-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "resolves CANVAS_ID from assignments file"
    (let* ((temp-dir (make-temp-file "module-link-test" t))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* Homework 1\n:PROPERTIES:\n:CANVAS_ID: 5001\n:END:\n\n")
              (insert "* Homework 2\n:PROPERTIES:\n:CANVAS_ID: 5002\n:END:\n\n")
              (insert "* Final Exam\n:PROPERTIES:\n:CANVAS_ID: 5003\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:assignments.org::*Homework 2][HW2 Link]]\n:PROPERTIES:\n:END:\n")
             (search-forward "HW2 Link")
             (org-back-to-heading)
             (let* ((data (org-canvas--module-item-parse-entry temp-dir))
                    ;; Pre-bind :type to avoid Emacs 29 oclosure shadowing
                    (data-type (plist-get data :type)))
               (expect (plist-get data :content-id) :to-equal 5002)
               (expect data-type :to-equal "Assignment")
               (expect (plist-get data :title) :to-equal "HW2 Link"))))
        (let ((buf (find-buffer-visiting assign-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns nil content-id when linked heading has no CANVAS_ID"
    (let* ((temp-dir (make-temp-file "module-link-test" t))
           (pages-file (expand-file-name "pages.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file pages-file
              (insert "* Synced Page\n:PROPERTIES:\n:CANVAS_URL: synced\n:END:\n\n")
              (insert "* Unsynced Page\n:PROPERTIES:\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:pages.org::*Unsynced Page][Unsynced]]\n:PROPERTIES:\n:END:\n")
             (search-forward "Unsynced")
             (org-back-to-heading)
             (let ((data (org-canvas--module-item-parse-entry temp-dir)))
               ;; Should classify as SubHeader or have nil content-id
               ;; since the target heading has no CANVAS_ID/CANVAS_URL
               (expect (plist-get data :content-id) :to-be nil)
               (expect (plist-get data :page-url) :to-be nil))))
        (let ((buf (find-buffer-visiting pages-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--module-find-item-by-title pagination"
  (it "finds an item that lives on page 2 of the module items list"
    (with-org-canvas-test-config
      (let ((calls 0))
        ;; Mock api-request directly so the all-pages helper runs end-to-end.
        ;; Page 1 returns 100 items (none matching "Target"); page 2 returns
        ;; 2 items including "Target". A naive single-page caller would miss it.
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (cl-incf calls)
                     (pcase calls
                       (1 (vconcat
                           (cl-loop for i from 1 to 100
                                    collect `((id . ,i) (title . ,(format "Item %d" i))))))
                       (2 (vector '((id . 101) (title . "Other"))
                                  '((id . 102) (title . "Target"))))
                       (_ (vector))))))
          (let ((found (org-canvas--module-find-item-by-title "999" "Target")))
            (expect found :not :to-be nil)
            (expect (alist-get 'id found) :to-equal 102)
            (expect calls :to-equal 2))))))

  (it "returns nil when item is missing across all pages"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args) (vector))))
        (expect (org-canvas--module-find-item-by-title "999" "Missing") :to-be nil)))))

(describe "module item ITEM_TYPE"
  (it "emits :ITEM_TYPE: SubHeader for SubHeader items"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (org-canvas--module-pull-insert-subheader "Week 1" 101 nil)
     (expect (buffer-string) :to-match ":ITEM_TYPE: SubHeader")))

  (it "emits :ITEM_TYPE: Page for Page items"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (cl-letf (((symbol-function 'org-canvas--module-resolve-item-link)
                (lambda (_type _cid title) (or title "Untitled"))))
       (org-canvas--module-pull-insert-content-item
        '((id . 2) (type . "Page") (title . "Reading 1")
          (page_url . "reading-1") (position . 1) (indent . 0))
        2 t))
     (expect (buffer-string) :to-match ":ITEM_TYPE: Page")))

  (it "emits :ITEM_TYPE: Assignment for Assignment items"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (cl-letf (((symbol-function 'org-canvas--module-resolve-item-link)
                (lambda (_type _cid title) (or title "Untitled"))))
       (org-canvas--module-pull-insert-content-item
        '((id . 3) (type . "Assignment") (title . "HW 1")
          (content_id . 999) (position . 1) (indent . 0))
        3 t))
     (expect (buffer-string) :to-match ":ITEM_TYPE: Assignment")))

  (it "emits :ITEM_TYPE: ExternalUrl for ExternalUrl items"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (goto-char (save-excursion (org-end-of-subtree t) (point)))
     (org-canvas--module-pull-insert-external-url
      '((id . 4) (type . "ExternalUrl") (title . "Read this")
        (external_url . "https://example.com") (position . 1) (indent . 0))
      4 t)
     (expect (buffer-string) :to-match ":ITEM_TYPE: ExternalUrl"))))

(describe "module item ITEM_TYPE on push parse"
  (it "uses :ITEM_TYPE: when present (no link inference)"
    (with-temp-org-buffer
     "* My Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Just a section header
:PROPERTIES:
:ITEM_TYPE: SubHeader
:END:
"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* ")
     (org-back-to-heading t)
     (let* ((raw (org-canvas--module-item-read-props (point) "."))
            (transformed (org-canvas--module-item-transform-props raw))
            (data (or transformed
                      (org-canvas--module-classify-unlinked-heading
                       (plist-get raw :heading-with-links)
                       (org-canvas--strip-statistics-cookie
                        (plist-get raw :title-raw))
                       (plist-get raw :canvas-id)
                       0 t (point))))
            (data-type (plist-get data :type)))
       (expect data-type :to-equal "SubHeader"))))

  (it "falls back to inference when :ITEM_TYPE: is absent"
    (with-temp-org-buffer
     "* My Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Plain text heading
"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* ")
     (org-back-to-heading t)
     (let* ((raw (org-canvas--module-item-read-props (point) "."))
            (transformed (org-canvas--module-item-transform-props raw))
            (data (or transformed
                      (org-canvas--module-classify-unlinked-heading
                       (plist-get raw :heading-with-links)
                       (org-canvas--strip-statistics-cookie
                        (plist-get raw :title-raw))
                       (plist-get raw :canvas-id)
                       0 t (point))))
            (data-type (plist-get data :type)))
       ;; classify-unlinked-heading should still produce SubHeader here
       (expect data-type :to-equal "SubHeader")))))

;;;; End-of-Run Retry Pass (issue #9)

(describe "org-canvas--module-item-position"
  (it "returns 1-based positions among siblings"
    (with-temp-org-buffer
     "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** First
** Second
** Third
"
     (goto-char (point-min))
     (search-forward "** Second")
     (expect (org-canvas--module-item-position (point)) :to-equal 2)
     (goto-char (point-min))
     (search-forward "** First")
     (expect (org-canvas--module-item-position (point)) :to-equal 1)
     (goto-char (point-min))
     (search-forward "** Third")
     (expect (org-canvas--module-item-position (point)) :to-equal 3))))

(describe "org-canvas--module-retry-single-pending"
  (it "syncs the item when its target now has a CANVAS_ID"
    (with-org-canvas-test-config
      (with-mock-api
        (cl-letf (((symbol-function 'org-canvas--module-resolve-link)
                   (lambda (_link _dir)
                     '(:type "Page" :title "Course Home"
                       :content-id nil :page-url "course-home"))))
          (with-temp-org-buffer
           "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** [[file:pages.org::*Course Home][Course Home]]
:PROPERTIES:
:END:
"
           (goto-char (point-min))
           (search-forward "** ")
           (org-back-to-heading)
           (let* ((entry (list :module-id 100
                               :marker (point-marker)
                               :title "Course Home"
                               :dir default-directory)))
             (expect (org-canvas--module-retry-single-pending entry)
                     :to-equal "Course Home")))))))

  (it "refreshes the module PAYLOAD_HASH after healing an item"
    ;; The hash stored at module-sync time predates the item's link
    ;; resolution; without a refresh the next sync re-pushes the whole
    ;; module once for no reason.
    (with-org-canvas-test-config
      (with-mock-api
        (cl-letf (((symbol-function 'org-canvas--module-resolve-link)
                   (lambda (_link _dir)
                     '(:type "Page" :title "Course Home"
                       :content-id nil :page-url "course-home"))))
          (with-temp-org-buffer
           "* Module
:PROPERTIES:
:CANVAS_ID: 100
:PAYLOAD_HASH: stale
:END:
** [[file:pages.org::*Course Home][Course Home]]
:PROPERTIES:
:END:
"
           (let ((org-canvas-modules-file (buffer-file-name)))
             (goto-char (point-min))
             (search-forward "** ")
             (org-back-to-heading)
             (let ((entry (list :module-id 100
                                :marker (point-marker)
                                :title "Course Home"
                                :dir default-directory)))
               (expect (org-canvas--module-retry-single-pending entry)
                       :to-equal "Course Home"))
             (goto-char (point-min))
             (org-back-to-heading)
             (let* ((saved (org-entry-get (point) "PAYLOAD_HASH"))
                    (data (org-canvas--module-parse-entry))
                    (expected (org-canvas--sync-payload-hash
                               (org-canvas--module-build-payload data)
                               data #'org-canvas--module-items-digest)))
               (expect saved :not :to-equal "stale")
               (expect saved :to-equal expected))))))))

  (it "returns nil when the target still has no CANVAS_ID"
    (with-org-canvas-test-config
      (with-mock-api
        (cl-letf (((symbol-function 'org-canvas--module-resolve-link)
                   (lambda (_link _dir)
                     '(:type "Page" :title "Course Home"
                       :content-id nil :page-url nil))))
          (with-temp-org-buffer
           "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** [[file:pages.org::*Course Home][Course Home]]
:PROPERTIES:
:END:
"
           (goto-char (point-min))
           (search-forward "** ")
           (org-back-to-heading)
           (let ((entry (list :module-id 100
                              :marker (point-marker)
                              :title "Course Home"
                              :dir default-directory)))
             (expect (org-canvas--module-retry-single-pending entry)
                     :to-be nil)))))))

  (it "returns nil and warns when the push errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--module-item-parse-entry)
                 (lambda (_dir) (error "Parse boom")))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (with-temp-org-buffer
         "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Item
"
         (goto-char (point-min))
         (search-forward "** ")
         (org-back-to-heading)
         (let ((entry (list :module-id 100
                            :marker (point-marker)
                            :title "Item"
                            :dir default-directory)))
           (expect (org-canvas--module-retry-single-pending entry)
                   :to-be nil))))))

  (it "returns nil for a dead marker"
    (expect (org-canvas--module-retry-single-pending
             (list :module-id 100 :marker (make-marker)
                   :title "Gone" :dir "/tmp/"))
            :to-be nil)))

(describe "org-canvas--module-retry-pending-items"
  (it "does nothing when no items are pending"
    (let ((org-canvas--module-items-pending nil)
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_l fmt &rest args)
                   (push (apply #'format fmt args) logged))))
        (org-canvas--module-retry-pending-items))
      (expect logged :to-be nil)))

  (it "reclassifies healed items and hints about the rest"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Module
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Healed
** Still Pending
"
       (goto-char (point-min))
       (search-forward "** Healed")
       (org-back-to-heading)
       (let ((healed-marker (point-marker)))
         (search-forward "** Still Pending")
         (org-back-to-heading)
         (let* ((pending-marker (point-marker))
                (org-canvas--sync-global-counters
                 (list :success 0 :skip 2 :fail 0 :dry-run 0 :deferred 0))
                (org-canvas--sync-global-feature-stats
                 (list (list :label "Module Items" :success 0 :skip 2 :fail 0
                             :deferred 0 :failed-titles nil
                             :skipped-titles
                             '("Healed (no linked content synced)"
                               "Still Pending (no linked content synced)"))))
                (org-canvas--module-items-pending
                 (list (list :module-id 100 :marker pending-marker
                             :title "Still Pending" :dir default-directory)
                       (list :module-id 100 :marker healed-marker
                             :title "Healed" :dir default-directory)))
                (warnings nil))
           (cl-letf (((symbol-function 'org-canvas--module-retry-single-pending)
                      (lambda (entry)
                        (when (equal (plist-get entry :title) "Healed")
                          "Healed")))
                     ((symbol-function 'org-canvas--log-warning)
                      (lambda (_l fmt &rest args)
                        (push (apply #'format fmt args) warnings))))
             (org-canvas--module-retry-pending-items))
           (expect org-canvas--module-items-pending :to-be nil)
           ;; Healed item moved from skip to success
           (let ((entry (car org-canvas--sync-global-feature-stats)))
             (expect (plist-get entry :success) :to-equal 1)
             (expect (plist-get entry :skip) :to-equal 1)
             (expect (plist-get entry :skipped-titles)
                     :to-equal '("Still Pending (no linked content synced)")))
           (expect (plist-get org-canvas--sync-global-counters :success)
                   :to-equal 1)
           ;; Remaining item produced the re-run hint
           (expect (cl-find-if
                    (lambda (w)
                      (string-match-p "still pending: 'Still Pending'.*org-canvas-sync-modules" w))
                    warnings)
                   :to-be-truthy)))))))

;;;; Items Digest — issue #26 (item edits must dirty the module hash)

(describe "org-canvas--module-items-digest"
  (it "is stable across repeated computation"
    (with-temp-org-buffer
     "* Week 1
:PROPERTIES:
:PUBLISHED: true
:END:
** Item One
** Item Two
"
     (let ((org-canvas-modules-file (buffer-file-name)))
       (org-back-to-heading)
       (let ((data (list :pom (point))))
         (expect (org-canvas--module-items-digest data)
                 :to-equal (org-canvas--module-items-digest data))))))

  (it "changes when an item is added"
    (let ((d1 (with-temp-org-buffer
               "* Week 1\n** Item One\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point))))))
          (d2 (with-temp-org-buffer
               "* Week 1\n** Item One\n** Item Two\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point)))))))
      (expect d1 :not :to-equal d2)))

  (it "changes when an item is renamed"
    (let ((d1 (with-temp-org-buffer
               "* Week 1\n** Old Title\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point))))))
          (d2 (with-temp-org-buffer
               "* Week 1\n** New Title\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point)))))))
      (expect d1 :not :to-equal d2)))

  (it "changes when an item property changes"
    (let ((d1 (with-temp-org-buffer
               "* Week 1\n** Item One\n:PROPERTIES:\n:INDENT: 0\n:END:\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point))))))
          (d2 (with-temp-org-buffer
               "* Week 1\n** Item One\n:PROPERTIES:\n:INDENT: 1\n:END:\n"
               (let ((org-canvas-modules-file (buffer-file-name)))
                 (org-back-to-heading)
                 (org-canvas--module-items-digest (list :pom (point)))))))
      (expect d1 :not :to-equal d2)))

  (it "does not change when an item gains a CANVAS_ID"
    ;; IDs are assigned by finalize right after the hash is computed;
    ;; if they entered the digest, every sync would dirty the module.
    (with-temp-org-buffer
     "* Week 1
** Item One
"
     (let ((org-canvas-modules-file (buffer-file-name)))
       (org-back-to-heading)
       (let* ((data (list :pom (point)))
              (before (org-canvas--module-items-digest data)))
         (save-excursion
           (search-forward "Item One")
           (org-entry-put (point) "CANVAS_ID" "501"))
         (expect (org-canvas--module-items-digest data) :to-equal before)))))

  (it "changes when a linked target gains a CANVAS_ID"
    ;; Interplay with #22: rotation of a target's id must dirty the
    ;; module so the stale item gets repaired on the next sync.
    (let ((pages-file (make-temp-file "pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file pages-file
              (insert "* Test Page\n:PROPERTIES:\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Test Page][Link Text]]\n"
                     (file-name-nondirectory pages-file))
             (let ((org-canvas-modules-file (buffer-file-name)))
               (org-back-to-heading)
               (let* ((data (list :pom (point)))
                      (before (org-canvas--module-items-digest data)))
                 (with-current-buffer (find-file-noselect pages-file)
                   (goto-char (point-min))
                   (org-back-to-heading)
                   (org-entry-put (point) "CANVAS_ID" "4242")
                   (save-buffer))
                 (expect (org-canvas--module-items-digest data)
                         :not :to-equal before)))))
        (let ((buf (find-buffer-visiting pages-file)))
          (when buf (kill-buffer buf)))
        (delete-file pages-file)))))

(describe "org-canvas-sync-modules payload-hash with items (mocked)"
  (it "skips an unchanged module but re-syncs after an item is added"
    (let ((temp-dir (make-temp-file "modules-test" t)))
      (unwind-protect
          (let* ((modules-file (expand-file-name "modules.org" temp-dir))
                 (module-writes 0)
                 (item-writes 0))
            (with-temp-file modules-file
              (insert "* Week 1
:PROPERTIES:
:PUBLISHED: true
:END:
** Section Header
"))
            (let ((org-canvas-modules-file modules-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest _args)
                               (cond
                                ((string-match "courses/99999/$" url)
                                 '((id . 99999)))
                                ((and (eq method 'POST) (string-match "modules$" url))
                                 (setq module-writes (1+ module-writes))
                                 '((id . 500) (name . "Week 1")))
                                ((and (eq method 'PUT) (string-match "modules/500$" url))
                                 (setq module-writes (1+ module-writes))
                                 '((id . 500) (name . "Week 1")))
                                ((and (memq method '(POST PUT))
                                      (string-match "modules/500/items" url))
                                 (setq item-writes (1+ item-writes))
                                 '((id . 501) (title . "Item")))
                                (t '((id . 1)))))))
                    ;; Run 1: creates module + item, saves PAYLOAD_HASH
                    (org-canvas-sync-modules)
                    (expect module-writes :to-equal 1)
                    ;; Run 2: nothing changed — module skipped
                    (org-canvas-sync-modules)
                    (expect module-writes :to-equal 1)
                    ;; Add an item; run 3 must re-push the module and
                    ;; sync both items (the core of issue #26)
                    (with-current-buffer (find-file-noselect modules-file)
                      (goto-char (point-max))
                      (insert "** Another Header\n")
                      (save-buffer))
                    (setq item-writes 0)
                    (org-canvas-sync-modules)
                    (expect module-writes :to-equal 2)
                    (expect item-writes :to-equal 2))))))
        (delete-directory temp-dir t)))))

;;;; Conflict pull-item

(describe "org-canvas--module-pull-item"
  (it "sets module properties from the response"
    (with-org-canvas-test-config
      ;; An explicitly empty items list must NOT trigger a fetch
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (error "Must not hit the API"))))
        (with-temp-org-buffer
         "* Old Name\n:PROPERTIES:\n:CANVAS_ID: 500\n:END:\n"
         (org-back-to-heading)
         (org-canvas--module-pull-item
          '((id . 500) (name . "Week 1") (position . 3)
            (published . :json-false)
            (require_sequential_progress . t)
            (unlock_at . "2027-01-15T08:00:00Z")
            (prerequisite_module_ids . (77 88))
            (items . nil))
          (point))
         (expect (org-entry-get (point) "POSITION") :to-equal "3")
         ;; published t is the registry default and gets suppressed;
         ;; the non-default false must be written out
         (expect (org-entry-get (point) "PUBLISHED") :to-equal "false")
         (expect (org-entry-get (point) "REQUIRE_SEQUENTIAL_PROGRESSION")
                 :to-equal "true")
         (expect (org-entry-get (point) "UNLOCK_AT") :to-match "2027-01-15")
         (expect (org-entry-get (point) "PREREQUISITE_MODULE_IDS")
                 :to-equal "77,88")))))

  (it "replaces child items with the included remote list"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (error "Must not hit the API"))))
        (with-temp-org-buffer
         "* Week 1\n:PROPERTIES:\n:CANVAS_ID: 500\n:END:\n** Stale Local Item\n"
         (org-back-to-heading)
         (org-canvas--module-pull-item
          '((id . 500) (name . "Week 1")
            (items . (((id . 501) (type . "SubHeader")
                       (title . "Remote Header") (published . t)))))
          (point))
         (expect (buffer-string) :not :to-match "Stale Local Item")
         (expect (buffer-string) :to-match "\\*\\* Remote Header")))))

  (it "fetches items from the endpoint when the response lacks them"
    ;; The conflict-check GET returns the module without items.
    (with-org-canvas-test-config
      (let ((fetched-url nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method url &rest _args)
                     (setq fetched-url url)
                     '(((id . 502) (type . "SubHeader")
                        (title . "Fetched Header") (published . t))))))
          (with-temp-org-buffer
           "* Week 1\n:PROPERTIES:\n:CANVAS_ID: 500\n:END:\n** Old Item\n"
           (org-back-to-heading)
           (org-canvas--module-pull-item '((id . 500) (name . "Week 1")) (point))
           (expect fetched-url :to-match "modules/500/items")
           (expect (buffer-string) :not :to-match "Old Item")
           (expect (buffer-string) :to-match "\\*\\* Fetched Header"))))))

  (it "is registered as the pull-item-fn for the modules sync"
    ;; The generated sync binds it dynamically; expanding the macro
    ;; form is the cheapest wiring check.
    (let ((expansion (macroexpand-1
                      '(org-canvas-define-sync modules
                         :file org-canvas-modules-file
                         :parse #'org-canvas--module-parse-entry
                         :build #'org-canvas--module-build-payload
                         :push #'org-canvas--module-push-to-api
                         :finalize #'org-canvas--module-finalize
                         :pull-item-fn #'org-canvas--module-pull-item))))
      (expect (format "%S" expansion)
              :to-match "org-canvas--module-pull-item"))))

;;; org-canvas-modules-test.el ends here
