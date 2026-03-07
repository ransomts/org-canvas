;;; org-canvas-outcomes-test.el --- Buttercup tests for outcomes  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-outcomes)

;;;; Helper Functions

(describe "org-canvas--outcome-get-root-group-id (mocked)"
  (it "returns root group id from API"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("root_outcome_group" . ((id . 12345) (title . "Root")))))
        (let ((result (org-canvas--outcome-get-root-group-id)))
          (expect result :to-equal 12345)))))

  (it "calls the correct endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("root_outcome_group" . ((id . 99)))))
        (org-canvas--outcome-get-root-group-id)
        (expect-api-called 'GET "root_outcome_group")))))

(describe "org-canvas--outcome-parse-ratings"
  (it "parses rating list items"
    (with-temp-org-buffer
     "* Group
** Outcome
:PROPERTIES:
:END:

Description text.

- [4] Exceeds Expectations
- [3] Meets Expectations
- [2] Approaching
- [1] Does Not Meet
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((ratings (org-canvas--outcome-parse-ratings)))
       (expect (length ratings) :to-equal 4)
       (expect (alist-get 'points (nth 0 ratings)) :to-equal 4)
       (expect (alist-get 'description (nth 0 ratings)) :to-equal "Exceeds Expectations")
       (expect (alist-get 'points (nth 3 ratings)) :to-equal 1))))

  (it "returns empty list when no ratings"
    (with-temp-org-buffer
     "* Group
** Outcome
:PROPERTIES:
:END:

Just description, no ratings.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((ratings (org-canvas--outcome-parse-ratings)))
       (expect ratings :to-equal nil)))))

(describe "org-canvas--outcome-get-description"
  (it "extracts description text"
    (with-temp-org-buffer
     "* Group
** Outcome
:PROPERTIES:
:END:

Students will be able to demonstrate understanding.

- [4] Excellent
- [3] Good
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((desc (org-canvas--outcome-get-description)))
       (expect desc :to-match "demonstrate understanding"))))

  (it "excludes property drawer"
    (with-temp-org-buffer
     "* Group
** Outcome
:PROPERTIES:
:CALCULATION_METHOD: highest
:END:

Description text.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((desc (org-canvas--outcome-get-description)))
       (expect desc :not :to-match "CALCULATION_METHOD")
       (expect desc :to-match "Description text")))))

;;;; Transform Props

(describe "org-canvas--outcome-group-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--outcome-group-transform-props
                   '(:title-raw "Communication Skills [2/3]" :canvas-id nil))))
      (expect (plist-get result :title) :to-equal "Communication Skills")))

  (it "passes through plain title unchanged"
    (let ((result (org-canvas--outcome-group-transform-props
                   '(:title-raw "Writing" :canvas-id nil))))
      (expect (plist-get result :title) :to-equal "Writing")))

  (it "passes through canvas-id"
    (let ((result (org-canvas--outcome-group-transform-props
                   '(:title-raw "Group" :canvas-id "11111"))))
      (expect (plist-get result :canvas-id) :to-equal "11111")))

  (it "passes through nil canvas-id for new groups"
    (let ((result (org-canvas--outcome-group-transform-props
                   '(:title-raw "Group" :canvas-id nil))))
      (expect (plist-get result :canvas-id) :to-be nil))))

(describe "org-canvas--outcome-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome [1/2]" :canvas-id nil
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :title) :to-equal "Outcome")))

  (it "passes through canvas-id"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id "22222"
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :canvas-id) :to-equal "22222")))

  (it "converts calculation-int-raw to number"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "decaying_average"
                     :calculation-int-raw "65"
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :calculation_int) :to-equal 65)))

  (it "leaves calculation_int nil when raw is nil"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :calculation_int) :to-be nil)))

  (it "converts mastery-points-raw to number"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw "3"
                     :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :mastery_points) :to-equal 3)))

  (it "leaves mastery_points nil when raw is nil"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :mastery_points) :to-be nil)))

  (it "passes through ratings unchanged"
    (let* ((ratings '(((points . 4) (description . "Exceeds"))
                      ((points . 3) (description . "Meets"))))
           (result (org-canvas--outcome-transform-props
                    (list :title-raw "Outcome" :canvas-id nil
                          :calculation-method "highest" :calculation-int-raw nil
                          :mastery-points-raw nil :description ""
                          :ratings ratings :parent-group-id-raw nil))))
      (expect (plist-get result :ratings) :to-equal ratings)))

  (it "converts parent-group-id-raw to number"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw "100"))))
      (expect (plist-get result :parent-group-id) :to-equal 100)))

  (it "leaves parent-group-id nil when raw is nil"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :parent-group-id) :to-be nil)))

  (it "passes through description unchanged"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "highest" :calculation-int-raw nil
                     :mastery-points-raw nil
                     :description "Students will write clearly."
                     :ratings nil :parent-group-id-raw nil))))
      (expect (plist-get result :description) :to-equal "Students will write clearly.")))

  (it "passes through calculation_method as :calculation_method"
    (let ((result (org-canvas--outcome-transform-props
                   '(:title-raw "Outcome" :canvas-id nil
                     :calculation-method "n_mastery" :calculation-int-raw "5"
                     :mastery-points-raw nil :description "" :ratings nil
                     :parent-group-id-raw nil))))
      (expect (plist-get result :calculation_method) :to-equal "n_mastery"))))

;;;; Stage 1: Parse Outcome Group Entry

(describe "org-canvas--outcome-group-parse-entry"
  (it "extracts outcome group title from heading"
    (with-temp-org-buffer
     "* Communication Skills
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-group-parse-entry)))
       (expect (plist-get data :title) :to-equal "Communication Skills"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:END:\n")
     (org-back-to-heading)
     (expect (org-canvas--outcome-group-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 11111
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-group-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "11111"))))

  (it "returns nil canvas-id for new groups"
    (with-temp-org-buffer
     "* New Group
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-group-parse-entry)))
       (expect (plist-get data :canvas-id) :to-be nil))))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-group-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy)))))

;;;; Stage 1: Parse Outcome Entry

(describe "org-canvas--outcome-parse-entry"
  (it "extracts outcome title from heading"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Written Communication
:PROPERTIES:
:END:

Students will write clearly.

- [4] Excellent
- [3] Good
"
     (search-forward "Written Communication")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :title) :to-equal "Written Communication"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* Group\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"
             "** " "\n:PROPERTIES:\n:END:\n\nDescription.\n\n- [4] Good\n")
     (search-forward "** ")
     (org-back-to-heading)
     (expect (org-canvas--outcome-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:CANVAS_ID: 22222
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "22222"))))

  (it "parses calculation_method (default highest)"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :calculation_method) :to-equal "highest"))))

  (it "parses custom calculation_method"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:CALCULATION_METHOD: decaying_average
:CALCULATION_INT: 65
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :calculation_method) :to-equal "decaying_average")
       (expect (plist-get data :calculation_int) :to-equal 65))))

  (it "parses mastery_points"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:MASTERY_POINTS: 3
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :mastery_points) :to-equal 3))))

  (it "extracts ratings"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Description.

- [4] Exceeds
- [3] Meets
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :ratings) :to-be-truthy)
       (expect (length (plist-get data :ratings)) :to-equal 2))))

  (it "gets parent-group-id from parent heading"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 99999
:END:
** Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :parent-group-id) :to-equal 99999))))

  (it "returns nil canvas-id for new outcomes"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** New Outcome
:PROPERTIES:
:END:

Description.
"
     (search-forward "New Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :canvas-id) :to-be nil))))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy))))

  (it "extracts description text"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Students will demonstrate critical thinking skills.

- [4] Excellent
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :description) :to-match "critical thinking"))))

  (it "returns nil parent-group-id when parent has no canvas-id"
    (with-temp-org-buffer
     "* Unsynced Group
:PROPERTIES:
:END:
** Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :parent-group-id) :to-be nil))))

  (it "defaults mastery_points to nil when not specified"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :mastery_points) :to-be nil)))))

;;;; Stage 2: Build Outcome Group Payload

(describe "org-canvas--outcome-group-build-payload"
  (it "includes title"
    (let* ((data '(:title "My Group"))
           (payload (org-canvas--outcome-group-build-payload data)))
      (expect (alist-get 'title payload) :to-equal "My Group")))

  (it "returns alist format"
    (let* ((data '(:title "Test Group"))
           (payload (org-canvas--outcome-group-build-payload data)))
      (expect (listp payload) :to-be t)
      (expect (consp (car payload)) :to-be t))))

;;;; Stage 2: Build Outcome Payload

(describe "org-canvas--outcome-build-payload"
  (it "includes title"
    (let* ((data '(:title "My Outcome" :description "" :calculation_method "highest"))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'title payload) :to-equal "My Outcome")))

  (it "includes description"
    (let* ((data '(:title "Test" :description "Students will learn." :calculation_method "highest"))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'description payload) :to-equal "Students will learn.")))

  (it "includes calculation_method"
    (let* ((data '(:title "Test" :description "" :calculation_method "decaying_average"))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'calculation_method payload) :to-equal "decaying_average")))

  (it "includes calculation_int when appropriate"
    (let* ((data '(:title "Test" :description "" :calculation_method "decaying_average" :calculation_int 65))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'calculation_int payload) :to-equal 65)))

  (it "includes mastery_points"
    (let* ((data '(:title "Test" :description "" :calculation_method "highest" :mastery_points 3))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'mastery_points payload) :to-equal 3)))

  (it "includes ratings"
    (let* ((data '(:title "Test" :description "" :calculation_method "highest"
                   :ratings (((points . 4) (description . "Excellent"))
                             ((points . 3) (description . "Good")))))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'ratings payload) :to-be-truthy)
      (expect (length (alist-get 'ratings payload)) :to-equal 2)))

  (it "excludes calculation_int when nil"
    (let* ((data '(:title "Test" :description "" :calculation_method "highest" :calculation_int nil))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (assoc 'calculation_int payload) :to-be nil)))

  (it "excludes ratings when empty"
    (let* ((data '(:title "Test" :description "" :calculation_method "highest" :ratings nil))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (assoc 'ratings payload) :to-be nil)))

  (it "excludes mastery_points when nil"
    (let* ((data '(:title "Test" :description "" :calculation_method "highest" :mastery_points nil))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (assoc 'mastery_points payload) :to-be nil)))

  (it "defaults description to empty string"
    (let* ((data '(:title "Test" :description nil :calculation_method "highest"))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'description payload) :to-equal ""))))

;;;; Stage 3: Search Functions (mocked)

(describe "org-canvas--outcome-group-search-by-title (mocked)"
  (it "searches subgroups endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups/100/subgroups" . [((id . 200) (title . "Skills"))
                                                   ((id . 201) (title . "Knowledge"))])))
        (org-canvas--outcome-group-search-by-title "Skills" 100)
        (expect-api-called 'GET "outcome_groups/100/subgroups"))))

  (it "returns matching group"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups" . [((id . 200) (title . "Skills"))
                                     ((id . 201) (title . "Knowledge"))])))
        (let ((result (org-canvas--outcome-group-search-by-title "Skills" 100)))
          (expect (alist-get 'id result) :to-equal 200)))))

  (it "returns nil when not found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups" . [((id . 200) (title . "Other"))])))
        (let ((result (org-canvas--outcome-group-search-by-title "Missing" 100)))
          (expect result :to-be nil)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error")))))
        (let ((result (org-canvas--outcome-group-search-by-title "Any" 100)))
          (expect result :to-be nil))))))

(describe "org-canvas--outcome-search-by-title (mocked)"
  (it "searches outcomes in group"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups/100/outcomes" . [((outcome . ((id . 300) (title . "Writing"))))])))
        (org-canvas--outcome-search-by-title "Writing" 100)
        (expect-api-called 'GET "outcome_groups/100/outcomes"))))

  (it "returns matching outcome"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups" . [((outcome . ((id . 300) (title . "Writing"))))
                                     ((outcome . ((id . 301) (title . "Reading"))))])))
        (let ((result (org-canvas--outcome-search-by-title "Writing" 100)))
          (expect (alist-get 'id result) :to-equal 300)))))

  (it "returns nil when not found"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups" . [])))
        (let ((result (org-canvas--outcome-search-by-title "Missing" 100)))
          (expect result :to-be nil)))))

  (it "returns nil on API error"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("API Error")))))
        (let ((result (org-canvas--outcome-search-by-title "Any" 100)))
          (expect result :to-be nil))))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--outcome-group-push-to-api (mocked)"
  (it "uses PUT for existing groups"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "555")))
          (org-canvas--outcome-group-push-to-api data 1)
          (expect-api-called 'PUT "outcome_groups/555")))))

  (it "uses POST for new groups"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Group" :canvas-id nil)))
          (org-canvas--outcome-group-push-to-api data 100)
          (expect-api-called 'POST "outcome_groups/100/subgroups")))))

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
                       '((id . 888) (title . "Recovered Group")))
                      (t nil)))))
          (let ((data '(:title "Missing Group" :canvas-id "999")))
            (let ((result (org-canvas--outcome-group-push-to-api data 100)))
              (expect post-called :to-be t)
              (expect (alist-get 'id result) :to-equal 888)))))))

  (it "signals non-404 errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request")))))
        (let ((data '(:title "Bad Group" :canvas-id "123")))
          (expect (org-canvas--outcome-group-push-to-api data 100)
                  :to-throw 'error))))))

(describe "org-canvas--outcome-push-to-api (mocked)"
  (it "uses PUT for existing outcomes"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "Existing" :canvas-id "666" :parent-group-id 100)))
          (org-canvas--outcome-push-to-api data)
          (expect-api-called 'PUT "outcomes/666")))))

  (it "uses POST for new outcomes"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Outcome" :canvas-id nil :parent-group-id 100
                      :description "Desc" :calculation_method "highest")))
          (org-canvas--outcome-push-to-api data)
          (expect-api-called 'POST "outcome_groups/100/outcomes")))))

  (it "errors when no parent-group-id"
    (let ((data '(:title "Orphan" :canvas-id nil :parent-group-id nil)))
      (expect (org-canvas--outcome-push-to-api data) :to-throw 'error)))

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
                       '((outcome . ((id . 777) (title . "Recovered")))))
                      (t nil)))))
          (let ((data '(:title "Missing" :canvas-id "999" :parent-group-id 100
                        :description "" :calculation_method "highest")))
            (let ((result (org-canvas--outcome-push-to-api data)))
              (expect post-called :to-be t)))))))

  (it "signals non-404 errors"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Bad Request")))))
        (let ((data '(:title "Bad" :canvas-id "123" :parent-group-id 100
                      :description "" :calculation_method "highest")))
          (expect (org-canvas--outcome-push-to-api data)
                  :to-throw 'error))))))

(describe "org-canvas--outcome-group-create (mocked)"
  (it "posts to subgroups endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Group")))
          (org-canvas--outcome-group-create data 100)
          (expect-api-called 'POST "outcome_groups/100/subgroups")))))

  (it "returns created group"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups" . ((id . 555) (title . "Created Group")))))
        (let ((data '(:title "Created Group")))
          (let ((result (org-canvas--outcome-group-create data 100)))
            (expect (alist-get 'id result) :to-equal 555))))))

  (it "searches for group on error"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST fails
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Already exists")))
                      ;; Second call: GET to find existing
                      ((eq method 'GET)
                       [((id . 444) (title . "Existing"))])
                      (t nil)))))
          (let ((data '(:title "Existing")))
            (let ((result (org-canvas--outcome-group-create data 100)))
              (expect (alist-get 'id result) :to-equal 444))))))))

(describe "org-canvas--outcome-create (mocked)"
  (it "posts to outcomes endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Outcome" :description "Desc" :calculation_method "highest")))
          (org-canvas--outcome-create data 100)
          (expect-api-called 'POST "outcome_groups/100/outcomes")))))

  (it "extracts outcome from response"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups" . ((outcome . ((id . 666) (title . "Created")))))))
        (let ((data '(:title "Created" :description "" :calculation_method "highest")))
          (let ((result (org-canvas--outcome-create data 100)))
            (expect (alist-get 'id result) :to-equal 666))))))

  (it "returns response directly if no outcome key"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups" . ((id . 777) (title . "Direct")))))
        (let ((data '(:title "Direct" :description "" :calculation_method "highest")))
          (let ((result (org-canvas--outcome-create data 100)))
            (expect (alist-get 'id result) :to-equal 777))))))

  (it "searches for outcome on error"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: POST fails
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Already exists")))
                      ;; Second call: GET to find existing
                      ((eq method 'GET)
                       [((outcome . ((id . 333) (title . "Found"))))])
                      (t nil)))))
          (let ((data '(:title "Found" :description "" :calculation_method "highest")))
            (let ((result (org-canvas--outcome-create data 100)))
              (expect (alist-get 'id result) :to-equal 333))))))))

;;;; Stage 4: Finalize

(describe "org-canvas--outcome-group-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Group
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Test Group" :pom (point-marker)))
           (response '((id . 77777) (title . "Test Group"))))
       (org-canvas--outcome-group-finalize data response)
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
       (org-canvas--outcome-group-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-"))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* No ID Group
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "No ID Group" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (expect (org-canvas--outcome-group-finalize data response) :to-throw 'error)))))

(describe "org-canvas--outcome-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Test Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "Test Outcome")
     (org-back-to-heading)
     (let ((data (list :title "Test Outcome" :pom (point-marker)))
           (response '((id . 88888) (title . "Test Outcome"))))
       (org-canvas--outcome-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "88888"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Test
:PROPERTIES:
:END:

Content.
"
     (search-forward "Test")
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 55555))))
       (org-canvas--outcome-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-"))))

  (it "signals error when no ID in response"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** No ID Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "No ID Outcome")
     (org-back-to-heading)
     (let ((data (list :title "No ID Outcome" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (expect (org-canvas--outcome-finalize data response) :to-throw 'error)))))

;;;; Delete All Outcomes Tests

(describe "org-canvas-delete-all-outcomes (mocked)"
  (it "deletes all outcome groups"
    (let ((temp-file (make-temp-file "outcomes" nil ".org"))
          (org-canvas-outcomes-file nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Group 1
:PROPERTIES:
:CANVAS_ID: 1
:END:
** Outcome 1
:PROPERTIES:
:CANVAS_ID: 101
:END:
"))
            (setq org-canvas-outcomes-file temp-file)
            (with-org-canvas-test-config
              (let ((deleted-count 0))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_method url &rest _args)
                             (cond
                              ((string-match "root_outcome_group" url)
                               '((id . 1000)))
                              ((string-match "subgroups" url)
                               [((id . 1) (title . "Group 1"))]))))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                             (setq deleted-count (length items))
                             (cons (length items) nil))))
                  (org-canvas-delete-all-outcomes)
                  (expect deleted-count :to-equal 1)))))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))))

  (it "aborts when user cancels"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (expect (org-canvas-delete-all-outcomes) :to-throw 'user-error))))

  (it "continues after delete error"
    (let ((temp-file (make-temp-file "outcomes" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert ""))
            (with-org-canvas-test-config
              (let ((org-canvas-outcomes-file temp-file)
                    (delete-attempts 0))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (_method url &rest _args)
                             (cond
                              ((string-match "root_outcome_group" url)
                               '((id . 1000)))
                              ((string-match "subgroups" url)
                               [((id . 1) (title . "G1")) ((id . 2) (title . "G2"))]))))
                          ((symbol-function 'org-canvas--delete-items-queued)
                           (lambda (items _endpoint-fn _id-field _title-field &optional _skip-fn _delete-data)
                             (setq delete-attempts (length items))
                             (cons (length items) nil))))
                  (org-canvas-delete-all-outcomes)
                  ;; Both should have been attempted
                  (expect delete-attempts :to-equal 2)))))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

;;;; Outcome Parse Entry Edge Cases

(describe "org-canvas--outcome-parse-entry edge cases"
  (it "returns nil calculation_int when not specified"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Content.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :calculation_int) :to-be nil))))

  (it "handles empty ratings list"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Just description, no ratings.
"
     (search-forward "Outcome")
     (org-back-to-heading)
     (let ((data (org-canvas--outcome-parse-entry)))
       (expect (plist-get data :ratings) :to-equal nil)))))

;;;; Outcome Group Push with Existing ID

(describe "org-canvas--outcome-group-push-to-api update path"
  (it "updates existing group with PUT"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups/555" . ((id . 555) (title . "Updated")))))
        (let ((data '(:title "Existing" :canvas-id "555")))
          (let ((result (org-canvas--outcome-group-push-to-api data 1000)))
            (expect-api-called 'PUT "outcome_groups/555")))))))

;;;; Outcome Push with Recovery

(describe "org-canvas--outcome-push-to-api recovery paths"
  (it "searches for outcome on creation error"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; POST fails
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Already exists")))
                      ;; GET finds existing
                      ((eq method 'GET)
                       [((outcome . ((id . 888) (title . "Found"))))])
                      (t nil)))))
          (let ((data '(:title "Found" :canvas-id nil :parent-group-id 100
                        :description "" :calculation_method "highest")))
            (let ((result (org-canvas--outcome-push-to-api data)))
              (expect (alist-get 'id result) :to-equal 888))))))))

;;;; Outcome Build Payload - calculation methods

(describe "org-canvas--outcome-build-payload calculation methods"
  (it "includes calculation_int for n_mastery"
    (let* ((data '(:title "Test" :description "" :calculation_method "n_mastery" :calculation_int 3))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'calculation_int payload) :to-equal 3)))

  (it "includes calculation_int for latest"
    (let* ((data '(:title "Test" :description "" :calculation_method "latest" :calculation_int 1))
           (payload (org-canvas--outcome-build-payload data)))
      (expect (alist-get 'calculation_int payload) :to-equal 1)))

  (it "excludes calculation_int for unsupported methods"
    (let* ((data '(:title "Test" :description "" :calculation_method "average" :calculation_int 5))
           (payload (org-canvas--outcome-build-payload data)))
      ;; "average" not in supported list
      (expect (assoc 'calculation_int payload) :to-be nil))))

;;;; Outcome Group Finalize Edge Case

(describe "org-canvas--outcome-group-finalize without ID"
  (it "signals error when response lacks ID"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((data (list :title "Group" :pom (point-marker)))
           (response '((error . "failed"))))
       (expect (org-canvas--outcome-group-finalize data response)
               :to-throw 'error)))))

;;;; Outcome Create - Response Handling

(describe "org-canvas--outcome-create response handling"
  (it "extracts outcome from nested response"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("outcome_groups/100/outcomes" . ((outcome . ((id . 123) (title . "Nested")))))))
        (let ((data '(:title "Nested" :description "" :calculation_method "highest")))
          (let ((result (org-canvas--outcome-create data 100)))
            (expect (alist-get 'id result) :to-equal 123)))))))

;;;; Sync Outcomes Integration Tests

(describe "org-canvas-sync-outcomes integration (mocked)"
  (it "syncs groups and outcomes successfully"
    (let ((temp-file (make-temp-file "outcomes" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Communication Skills
:PROPERTIES:
:END:
** Written Communication
:PROPERTIES:
:END:

Students will write clearly.

- [4] Exceeds
- [3] Meets
"))
            (let ((org-canvas-outcomes-file temp-file)
                  (group-post-count 0)
                  (outcome-post-count 0))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest _args)
                               (cond
                                ;; Root group
                                ((string-match "root_outcome_group" url)
                                 '((id . 1000)))
                                ;; POST group
                                ((and (eq method 'POST) (string-match "subgroups" url))
                                 (setq group-post-count (1+ group-post-count))
                                 '((id . 2000) (title . "Communication Skills")))
                                ;; POST outcome
                                ((and (eq method 'POST) (string-match "outcomes" url))
                                 (setq outcome-post-count (1+ outcome-post-count))
                                 '((outcome . ((id . 3000) (title . "Written Communication")))))
                                ;; PUT for existing
                                ((eq method 'PUT)
                                 '((id . 2000)))
                                (t '((id . 1)))))))
                    (org-canvas-sync-outcomes)
                    (expect group-post-count :to-equal 1)
                    (expect outcome-post-count :to-equal 1)
                    ;; Verify CANVAS_IDs were saved
                    (with-current-buffer (find-file-noselect temp-file)
                      (goto-char (point-min))
                      (org-back-to-heading)
                      (expect (org-entry-get (point) "CANVAS_ID") :to-equal "2000")))))))
        (when (file-exists-p temp-file) (delete-file temp-file)))))

  (it "errors when file not found"
    (let ((org-canvas-outcomes-file "/nonexistent/outcomes.org"))
      (with-sync-test-env
        (expect (org-canvas-sync-outcomes) :to-throw 'error))))

  (it "errors when root group not found"
    (let ((temp-file (make-temp-file "outcomes" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Group\n"))
            (let ((org-canvas-outcomes-file temp-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (_method url &rest _args)
                               (when (string-match "root_outcome_group" url)
                                 '((name . "Root") (id . nil))))))
                    (expect (org-canvas-sync-outcomes) :to-throw 'error))))))
        (when (file-exists-p temp-file) (delete-file temp-file)))))

  (it "continues when group sync fails but processes outcomes"
    (let ((temp-file (make-temp-file "outcomes" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Outcome
:PROPERTIES:
:END:

Content.

- [4] Good
"))
            (let ((org-canvas-outcomes-file temp-file)
                  (outcome-count 0))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest _args)
                               (cond
                                ((string-match "root_outcome_group" url) '((id . 1000)))
                                ;; Group PUT fails
                                ((and (eq method 'PUT) (string-match "outcome_groups/100$" url))
                                 (signal 'error '("Group update failed")))
                                ;; Outcome POST succeeds
                                ((and (eq method 'POST) (string-match "outcomes" url))
                                 (setq outcome-count (1+ outcome-count))
                                 '((outcome . ((id . 500)))))
                                (t '((id . 1)))))))
                    (org-canvas-sync-outcomes)
                    (expect outcome-count :to-equal 1))))))
        (when (file-exists-p temp-file) (delete-file temp-file))))))

;;;; Outcome Group Create Recovery

(describe "org-canvas--outcome-group-create recovery"
  (it "signals error when POST fails and search fails"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _args)
                   (cond
                    ((eq method 'POST)
                     (signal 'error '("Server error")))
                    ((eq method 'GET)
                     ;; Return empty results - no match
                     [((id . 1) (title . "Other Group"))])
                    (t nil)))))
        (let ((data '(:title "Lost Group")))
          (expect (org-canvas--outcome-group-create data 100)
                  :to-throw 'error))))))

;;;; Outcome Create Recovery

(describe "org-canvas--outcome-create recovery"
  (it "signals error when POST and search both fail"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _args)
                   (cond
                    ((eq method 'POST)
                     (signal 'error '("Server error")))
                    ((eq method 'GET)
                     ;; Return empty results
                     [])
                    (t nil)))))
        (let ((data '(:title "Lost Outcome" :description "" :calculation_method "highest")))
          (expect (org-canvas--outcome-create data 100)
                  :to-throw 'error))))))

;;;; Delete All Outcomes Cleanup

(describe "org-canvas-delete-all-outcomes cleanup paths"
  (it "cleans all CANVAS_ID properties from outcomes file"
    (let ((temp-file (make-temp-file "outcomes" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Group 1
:PROPERTIES:
:CANVAS_ID: 100
:LAST_SYNCED: [2024-01-01 Mon]
:END:
** Outcome 1
:PROPERTIES:
:CANVAS_ID: 200
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"))
            (let ((org-canvas-outcomes-file temp-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                            ((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest _args)
                               (cond
                                ((string-match "root_outcome_group" url) '((id . 1000)))
                                ((and (eq method 'GET) (string-match "subgroups" url))
                                 [((id . 100) (title . "Group 1"))])
                                ((eq method 'DELETE) nil)
                                (t nil)))))
                    (org-canvas-delete-all-outcomes)
                    ;; Both group and outcome CANVAS_IDs should be cleared
                    (with-current-buffer (find-file-noselect temp-file)
                      (goto-char (point-min))
                      (org-back-to-heading)
                      (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))))))
        (when (file-exists-p temp-file) (delete-file temp-file)))))

  (it "handles no subgroups"
    (with-org-canvas-test-config
      (with-sync-test-env
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (_method url &rest _args)
                     (cond
                      ((string-match "root_outcome_group" url) '((id . 1000)))
                      (t [])))))
          ;; Should not error with empty subgroups
          (let ((org-canvas-outcomes-file "/nonexistent.org"))
            (org-canvas-delete-all-outcomes))
          (expect t :to-be t))))))

;;;; Outcome Group Push Timeout Recovery

(describe "org-canvas--outcome-group-push-to-api timeout recovery"
  (it "recovers from POST timeout when group exists"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; POST fails with generic error
                      ((and (eq method 'POST) (= call-count 1))
                       (signal 'error '("Request timed out")))
                      ;; GET search finds the group
                      ((eq method 'GET)
                       [((id . 444) (title . "Recovered Group"))])
                      (t nil)))))
          (let ((data '(:title "Recovered Group" :canvas-id nil)))
            (let ((result (org-canvas--outcome-group-create data 100)))
              (expect (alist-get 'id result) :to-equal 444))))))))

;;;; Coverage Gap Tests

(describe "org-canvas-sync-outcomes Phase 2 failure"
  (it "handles individual outcome push failure and continues"
    (let ((temp-file (make-temp-file "outcomes-" nil ".org"))
          (push-count 0))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Outcome Group
:PROPERTIES:
:CANVAS_ID: 100
:END:

** Outcome A
:PROPERTIES:
:MASTERY_POINTS: 3
:CALCULATION_METHOD: highest
:END:

Description A.

- [4] Excellent
- [3] Good
- [2] Fair
- [1] Poor

** Outcome B
:PROPERTIES:
:MASTERY_POINTS: 3
:CALCULATION_METHOD: highest
:END:

Description B.

- [4] Excellent
- [3] Good
- [2] Fair
- [1] Poor
"))
            (let ((org-canvas-outcomes-file temp-file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             '((id . 1000) (title . "Root"))))
                          ((symbol-function 'org-canvas--outcome-get-root-group-id)
                           (lambda () 1000))
                          ((symbol-function 'org-canvas--outcome-group-parse-entry)
                           (lambda () '(:title "Group" :canvas-id "100" :pom 1)))
                          ((symbol-function 'org-canvas--outcome-group-push-to-api)
                           (lambda (_d _r) '((id . 100))))
                          ((symbol-function 'org-canvas--outcome-group-finalize)
                           (lambda (&rest _) nil))
                          ((symbol-function 'org-canvas--outcome-push-to-api)
                           (lambda (_data)
                             (setq push-count (1+ push-count))
                             (when (= push-count 1)
                               (signal 'error '("API error for outcome A")))
                             '((id . 200))))
                          ((symbol-function 'org-canvas--outcome-parse-entry)
                           (lambda () `(:title ,(format "Outcome %d" push-count) :pom ,(point-marker))))
                          ((symbol-function 'org-canvas--outcome-finalize)
                           (lambda (&rest _) nil))
                          ((symbol-function 'display-buffer)
                           (lambda (&rest _) nil)))
                  (org-canvas-sync-outcomes)
                  ;; Both outcomes should be attempted
                  (expect push-count :to-equal 2)))))
        (delete-file temp-file)))))

;;;; Pull Function Tests

(describe "org-canvas--outcome-pull-find-or-create-l2"
  (it "finds existing L2 by CANVAS_ID"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Existing Outcome
:PROPERTIES:
:CANVAS_ID: 200
:END:
"
     (org-back-to-heading)
     (let ((pos (org-canvas--outcome-pull-find-or-create-l2 200 "Existing Outcome")))
       (goto-char pos)
       (expect (org-entry-get pos "CANVAS_ID") :to-equal "200"))))

  (it "creates new L2 when not found"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
     (org-back-to-heading)
     (let ((pos (org-canvas--outcome-pull-find-or-create-l2 999 "New Outcome")))
       (goto-char pos)
       (expect (org-get-heading t t t t) :to-equal "New Outcome")))))

(describe "org-canvas--outcome-pull-insert-ratings"
  (it "inserts formatted ratings"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:END:
"
     (goto-char (point-max))
     (org-canvas--outcome-pull-insert-ratings
      "Has description"
      [((description . "Excellent") (points . 4))
       ((description . "Good") (points . 3))])
     (expect (buffer-string) :to-match "Ratings:")
     (expect (buffer-string) :to-match "Excellent (4 pts)")
     (expect (buffer-string) :to-match "Good (3 pts)")))

  (it "does nothing for nil description"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:END:
"
     (let ((before (buffer-string)))
       (goto-char (point-max))
       (org-canvas--outcome-pull-insert-ratings
        nil [((description . "Good") (points . 3))])
       (expect (buffer-string) :to-equal before))))

  (it "does nothing for empty description"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:END:
"
     (let ((before (buffer-string)))
       (goto-char (point-max))
       (org-canvas--outcome-pull-insert-ratings
        "" [((description . "Good") (points . 3))])
       (expect (buffer-string) :to-equal before)))))

(describe "org-canvas--outcome-pull-process-group"
  (it "fetches and inserts outcomes"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                (lambda (_method _url &optional _params)
                  '(((outcome . ((id . 201) (title . "Outcome A")
                                 (description . "Desc A")
                                 (ratings . [((description . "Good") (points . 3))])))))))
               ((symbol-function 'org-canvas--html-to-org)
                (lambda (html) html)))
       (let ((count (org-canvas--outcome-pull-process-group 100)))
         (expect count :to-equal 1)
         (expect (buffer-string) :to-match "Outcome A")))))

  (it "returns 0 on API error"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
     (org-back-to-heading)
     (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                (lambda (_method _url &optional _params)
                  (signal 'error '("API error")))))
       (expect (org-canvas--outcome-pull-process-group 100) :to-equal 0)))))

(describe "org-canvas-pull-outcomes"
  (it "creates groups and outcomes"
    (let* ((temp-dir (make-temp-file "pull-outcomes-test" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-outcomes-file outcomes-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method url &optional _params)
                             (cond
                              ((string-match "outcome_groups$" url)
                               '(((id . 10) (title . "Critical Thinking"))))
                              ((string-match "outcomes$" url)
                               '(((outcome . ((id . 201) (title . "Analyze")
                                              (description . "<p>Desc</p>")
                                              (ratings . []))))))
                              (t '()))))
                          ((symbol-function 'org-canvas--html-to-org)
                           (lambda (html) (replace-regexp-in-string "<[^>]+>" "" html))))
                  (org-canvas-pull-outcomes)
                  (with-current-buffer (find-file-noselect outcomes-file)
                    (expect (buffer-string) :to-match "Critical Thinking")
                    (expect (buffer-string) :to-match "Analyze"))))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates file when it does not exist"
    (let* ((temp-dir (make-temp-file "pull-outcomes-test" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-outcomes-file outcomes-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params) '()))
                          ((symbol-function 'org-canvas--html-to-org)
                           (lambda (html) html)))
                  (org-canvas-pull-outcomes)
                  (expect (file-exists-p outcomes-file) :to-be-truthy)))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-outcomes progress"
  (it "shows per-group progress in echo area"
    (let* ((temp-dir (make-temp-file "pull-outcomes-progress" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-outcomes-file outcomes-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (with-temp-file outcomes-file (insert ""))
                (spy-on 'message)
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 10) (title . "Group A"))
                               ((id . 20) (title . "Group B")))))
                          ((symbol-function 'org-canvas--outcome-pull-process-group)
                           (lambda (_gid) 0))
                          ((symbol-function 'org-canvas--html-to-org)
                           (lambda (html) html)))
                  (org-canvas-pull-outcomes)
                  (expect 'message :to-have-been-called-with
                          "Outcomes [%d/%d] Pulling group '%s'..." 1 2 "Group A")
                  (expect 'message :to-have-been-called-with
                          "Outcomes [%d/%d] Pulling group '%s'..." 2 2 "Group B")))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;; org-canvas-outcomes-test.el ends here
