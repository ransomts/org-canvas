;;; org-canvas-rubrics-test.el --- Buttercup tests for rubrics  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-rubrics)

;;;; Helpers

(defun test-rubric--criterion (description points &rest rest)
  "Build a criterion plist from DESCRIPTION, POINTS, and REST plist keys.
REST may include :long-description, :outcome-link, :ratings."
  (let ((plist (list :description description :points points
                     :long-description (or (plist-get rest :long-description) "")
                     :outcome-link (plist-get rest :outcome-link)
                     :ratings (plist-get rest :ratings))))
    plist))

;;;; Transform (pure, no buffer)

(describe "org-canvas--rubric-transform-props"
  (it "strips statistics cookie from title"
    (let ((result (org-canvas--rubric-transform-props
                   '(:title-raw "Rubric [1/2]" :canvas-id nil
                     :free-form-raw nil :criteria nil))))
      (expect (plist-get result :title) :to-equal "Rubric")))

  (it "interprets free-form boolean"
    (let ((result (org-canvas--rubric-transform-props
                   '(:title-raw "Test" :canvas-id nil
                     :free-form-raw "true" :criteria nil))))
      (expect (plist-get result :free-form) :to-be t)))

  (it "passes through criteria plist data"
    (let* ((criteria (list (list :description "Analysis" :points 5)))
           (result (org-canvas--rubric-transform-props
                    (list :title-raw "Test" :canvas-id "42"
                          :free-form-raw nil :criteria criteria))))
      (expect (plist-get result :criteria) :to-equal criteria)
      (expect (plist-get result :canvas-id) :to-equal "42"))))

;;;; Points-tag encoding

(describe "org-canvas--rubric-points-tag"
  (it "encodes integer points as Npt"
    (expect (org-canvas--rubric-points-tag 5) :to-equal "5pt")
    (expect (org-canvas--rubric-points-tag 10) :to-equal "10pt"))

  (it "encodes whole-valued floats without decimal"
    (expect (org-canvas--rubric-points-tag 5.0) :to-equal "5pt"))

  (it "encodes fractional points using underscore"
    (expect (org-canvas--rubric-points-tag 3.5) :to-equal "3_5pt")))

(describe "org-canvas--rubric-decode-points-tag"
  (it "decodes integer tag"
    (expect (org-canvas--rubric-decode-points-tag "5pt") :to-equal 5))

  (it "decodes underscore-fractional tag"
    (expect (org-canvas--rubric-decode-points-tag "3_5pt") :to-equal 3.5))

  (it "returns nil for non-pt tags"
    (expect (org-canvas--rubric-decode-points-tag "regular") :to-be nil))

  (it "returns nil for nil tag"
    (expect (org-canvas--rubric-decode-points-tag nil) :to-be nil)))

;;;; Stage 1: Parse Entry (new format)

(describe "org-canvas--rubric-parse-entry"
  (it "extracts rubric title from heading"
    (with-temp-org-buffer
     "* Essay Rubric
:PROPERTIES:
:END:
** Thesis :20pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 20 | |
| No Marks | 0 | |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :title) :to-equal "Essay Rubric"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:END:\n"
             "** Thesis :20pt:\n"
             "| Rating | Points | Description |\n"
             "|--------+--------+-------------|\n"
             "| Full Marks | 20 | |\n")
     (org-back-to-heading)
     (expect (org-canvas--rubric-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:CANVAS_ID: 33333
:END:
** Quality :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "33333"))))

  (it "returns nil canvas-id for new rubrics"
    (with-temp-org-buffer
     "* New Rubric
:PROPERTIES:
:END:
** Quality :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :canvas-id) :to-be nil))))

  (it "parses free_form property"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:FREE_FORM_CRITERION_COMMENTS: true
:END:
** Item :5pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 5 | |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :free-form) :to-be t))))

  (it "extracts criterion description and points from heading"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:END:
** Clarity :25pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 25 | |
| No Marks | 0 | |
** Originality :15pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 15 | |
"
     (org-back-to-heading)
     (let* ((data (org-canvas--rubric-parse-entry))
            (criteria (plist-get data :criteria)))
       (expect (length criteria) :to-equal 2)
       (let ((c0 (nth 0 criteria))
             (c1 (nth 1 criteria)))
         (expect (plist-get c0 :description) :to-equal "Clarity")
         (expect (plist-get c0 :points) :to-equal 25)
         (expect (plist-get c1 :description) :to-equal "Originality")
         (expect (plist-get c1 :points) :to-equal 15)))))

  (it "errors when no criteria found"
    (with-temp-org-buffer
     "* Rubric Without Criteria
:PROPERTIES:
:END:

Just some text, no level-2 children.
"
     (org-back-to-heading)
     (expect (org-canvas--rubric-parse-entry) :to-throw 'error)))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
** Item :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy))))

  (it "extracts ratings from criterion sub-table"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:END:
** Code Quality :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Excellent | 10 | |
| Good | 7 | |
| Poor | 0 | |
"
     (org-back-to-heading)
     (let* ((data (org-canvas--rubric-parse-entry))
            (criteria (plist-get data :criteria))
            (c0 (nth 0 criteria))
            (ratings (plist-get c0 :ratings)))
       (expect (length ratings) :to-equal 3)
       (expect (nth 0 (nth 0 ratings)) :to-equal "Excellent")
       (expect (nth 1 (nth 0 ratings)) :to-equal 10)
       (expect (nth 0 (nth 2 ratings)) :to-equal "Poor"))))

  (it "extracts long description from body before sub-table"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:END:
** Quality :10pt:
This is a longer description of the criterion.
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading)
     (let* ((data (org-canvas--rubric-parse-entry))
            (criteria (plist-get data :criteria))
            (c0 (nth 0 criteria)))
       (expect (plist-get c0 :long-description)
               :to-match "longer description"))))

  (it "extracts OUTCOME property as :outcome-link"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:END:
** Quality :10pt:
:PROPERTIES:
:OUTCOME: [[file:outcomes.org::*Python][Python]]
:END:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading)
     (let* ((data (org-canvas--rubric-parse-entry))
            (criteria (plist-get data :criteria))
            (c0 (nth 0 criteria)))
       (expect (plist-get c0 :outcome-link)
               :to-match "Python")))))

;;;; Stage 2: Build Payload

(describe "org-canvas--rubric-build-payload"
  (it "wraps in rubric key"
    (let* ((data (list :title "Test" :free-form nil
                       :criteria (list (test-rubric--criterion "Crit" 10))))
           (payload (org-canvas--rubric-build-payload data)))
      (expect (gethash "rubric" payload) :to-be-truthy)))

  (it "includes title in payload"
    (let* ((data (list :title "Grading Rubric" :free-form nil
                       :criteria (list (test-rubric--criterion "Item" 5))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload)))
      (expect (gethash "title" rubric) :to-equal "Grading Rubric")))

  (it "sets free_form_criterion_comments"
    (let* ((data (list :title "Test" :free-form t
                       :criteria (list (test-rubric--criterion "Item" 5))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload)))
      (expect (gethash "free_form_criterion_comments" rubric) :to-be t)))

  (it "builds criteria hash"
    (let* ((data (list :title "Test" :free-form nil
                       :criteria (list (test-rubric--criterion "Clarity" 20)
                                       (test-rubric--criterion "Structure" 15))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      (expect criteria :to-be-truthy)
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be-truthy)))

  (it "includes rubric_association"
    (with-org-canvas-test-config
      (let* ((data (list :title "Test" :free-form nil
                         :criteria (list (test-rubric--criterion "Item" 5))))
             (payload (org-canvas--rubric-build-payload data)))
        (expect (gethash "rubric_association" payload) :to-be-truthy))))

  (it "sets free_form_criterion_comments to :json-false when nil"
    (let* ((data (list :title "Test" :free-form nil
                       :criteria (list (test-rubric--criterion "Item" 5))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload)))
      (expect (gethash "free_form_criterion_comments" rubric) :to-equal :json-false)))

  (it "preserves criterion order"
    (let* ((data (list :title "Test" :free-form nil
                       :criteria (list (test-rubric--criterion "First" 10)
                                       (test-rubric--criterion "Second" 20)
                                       (test-rubric--criterion "Third" 30))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      (expect (gethash "description" (gethash "0" criteria)) :to-equal "First")
      (expect (gethash "description" (gethash "1" criteria)) :to-equal "Second")
      (expect (gethash "description" (gethash "2" criteria)) :to-equal "Third")
      (expect (gethash "3" criteria) :to-be nil)))

  (it "builds default Full Marks/No Marks ratings when no ratings list"
    (let* ((data (list :title "Test" :free-form nil
                       :criteria (list (test-rubric--criterion "Quality" 25))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric))
           (crit0 (gethash "0" criteria))
           (ratings (gethash "ratings" crit0)))
      (expect ratings :to-be-truthy)
      (let ((r1 (gethash "0" ratings))
            (r2 (gethash "1" ratings)))
        (expect (gethash "description" r1) :to-equal "Full Marks")
        (expect (gethash "points" r1) :to-equal 25)
        (expect (gethash "description" r2) :to-equal "No Marks")
        (expect (gethash "points" r2) :to-equal 0)))))

;;;; Stage 3: Push to API (mocked)

(describe "org-canvas--rubric-delete-by-id (mocked)"
  (it "calls DELETE on rubrics endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--rubric-delete-by-id "12345")
        (expect-api-called 'DELETE "rubrics/12345")))))

(describe "org-canvas--rubric-push-to-api (mocked)"
  (it "uses POST to create rubric"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:title "New Rubric"))
              (payload (make-hash-table)))
          (org-canvas--rubric-push-to-api data payload)
          (expect-api-called 'POST "rubrics")))))

  (it "deletes existing rubric with same title before creating"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("rubrics$" . [((id . 555) (title . "Duplicate Rubric"))])))
        (let ((data '(:title "Duplicate Rubric"))
              (payload (make-hash-table)))
          (org-canvas--rubric-push-to-api data payload)
          (expect-api-called 'DELETE "rubrics/555")
          (expect-api-called 'POST "rubrics")))))

  (it "recovers from timeout by searching for created rubric"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ((and (eq method 'GET) (= call-count 1))
                       [])
                      ((eq method 'POST)
                       (signal 'error '("Timeout waiting for response")))
                      ((and (eq method 'GET) (>= call-count 3))
                       [((id . 999) (title . "Timeout Rubric"))])
                      (t nil)))))
          (let ((data '(:title "Timeout Rubric"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--rubric-push-to-api data payload)))
              (expect (alist-get 'id result) :to-equal 999)))))))

  (it "signals non-timeout errors"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ((and (eq method 'GET) (= call-count 1))
                       [])
                      ((eq method 'POST)
                       (signal 'error '("Bad Request: Invalid rubric")))
                      (t nil)))))
          (let ((data '(:title "Bad Rubric"))
                (payload (make-hash-table)))
            (expect (org-canvas--rubric-push-to-api data payload)
                    :to-throw 'error))))))

  (it "continues after failing to delete existing rubric"
    (with-org-canvas-test-config
      (let ((call-count 0)
            (delete-called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ((and (eq method 'GET) (= call-count 1))
                       [((id . 111) (title . "In Use Rubric"))])
                      ((eq method 'DELETE)
                       (setq delete-called t)
                       (signal 'error '("Rubric is in use and cannot be deleted")))
                      ((eq method 'POST)
                       '((id . 222) (title . "In Use Rubric")))
                      (t nil)))))
          (let ((data '(:title "In Use Rubric"))
                (payload (make-hash-table)))
            (let ((result (org-canvas--rubric-push-to-api data payload)))
              (expect delete-called :to-be t)
              (expect (alist-get 'id result) :to-equal 222)))))))

  (it "signals error when timeout recovery finds no rubric"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ((and (eq method 'GET) (= call-count 1))
                       [])
                      ((eq method 'POST)
                       (signal 'error '("Timeout waiting for response")))
                      ((and (eq method 'GET) (>= call-count 3))
                       nil)
                      (t nil)))))
          (let ((data '(:title "Lost Rubric"))
                (payload (make-hash-table)))
            (expect (org-canvas--rubric-push-to-api data payload)
                    :to-throw 'error)))))))

(describe "rubric search (mocked)"
  (it "searches rubrics endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("rubrics" . [((id . 100) (title . "Essay Rubric"))])))
        (org-canvas--search-item "rubrics" "Essay Rubric" :params nil)
        (expect-api-called 'GET "rubrics"))))

  (it "returns matching rubric"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("rubrics" . [((id . 100) (title . "Rubric A"))
                              ((id . 101) (title . "Rubric B"))])))
        (let ((result (org-canvas--search-item "rubrics" "Rubric A" :params nil)))
          (expect (alist-get 'id result) :to-equal 100))))))

;;;; Rubric Dissociation

(describe "org-canvas--rubric-dissociate-all (mocked)"
  (it "dissociates rubrics from assignments with rubric_settings"
    (with-org-canvas-test-config
      (let ((delete-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((eq method 'GET)
                       [((id . 1) (name . "Assignment A")
                         (rubric_settings . ((id . 501))))
                        ((id . 2) (name . "Assignment B")
                         (rubric_settings . ((id . 502))))
                        ((id . 3) (name . "No Rubric"))])
                      ((eq method 'DELETE)
                       (push url delete-calls)
                       nil)))))
          (let ((count (org-canvas--rubric-dissociate-all)))
            (expect count :to-equal 2)
            (expect (length delete-calls) :to-equal 2))))))

  (it "skips assignments without rubric_settings"
    (with-org-canvas-test-config
      (let ((delete-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (cond
                      ((eq method 'GET)
                       [((id . 1) (name . "Plain Assignment"))])
                      ((eq method 'DELETE)
                       (push t delete-calls)
                       nil)))))
          (let ((count (org-canvas--rubric-dissociate-all)))
            (expect count :to-equal 0)
            (expect delete-calls :to-be nil))))))

  (it "continues when dissociation fails for one assignment"
    (with-org-canvas-test-config
      (let ((delete-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((eq method 'GET)
                       [((id . 1) (name . "Fail") (rubric_settings . ((id . 601))))
                        ((id . 2) (name . "Succeed") (rubric_settings . ((id . 602))))])
                      ((eq method 'DELETE)
                       (if (string-match "601" url)
                           (signal 'error '("500 Internal Server Error"))
                         (setq delete-count (1+ delete-count))
                         nil))))))
          (let ((count (org-canvas--rubric-dissociate-all)))
            (expect count :to-equal 1)
            (expect delete-count :to-equal 1))))))

  (it "handles empty assignment list"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args) [])))
        (expect (org-canvas--rubric-dissociate-all) :to-equal 0))))

  (it "dissociates rubrics from assignments across multiple pages"
    (with-org-canvas-test-config
      (let ((get-calls 0)
            (delete-urls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (cond
                      ((eq method 'GET)
                       (cl-incf get-calls)
                       (pcase get-calls
                         (1 (vconcat
                             (cl-loop for i from 1 to 100
                                      collect (if (zerop (mod i 2))
                                                  `((id . ,i)
                                                    (name . ,(format "Assn %d" i))
                                                    (rubric_settings . ((id . ,(+ 500 i)))))
                                                `((id . ,i)
                                                  (name . ,(format "Assn %d" i)))))))
                         (2 (vector '((id . 101) (name . "Late Assn"))
                                    '((id . 102) (name . "Final")
                                      (rubric_settings . ((id . 999))))))
                         (_ (vector))))
                      ((eq method 'DELETE)
                       (push url delete-urls)
                       nil)))))
          (let ((count (org-canvas--rubric-dissociate-all)))
            (expect count :to-equal 51)
            (expect (length delete-urls) :to-equal 51)
            (expect (cl-some (lambda (u) (string-match-p "999" u)) delete-urls)
                    :to-be-truthy)
            (expect get-calls :to-equal 2)))))))

(describe "org-canvas--rubric-log-diagnostics (mocked)"
  (it "fetches rubric detail and assignments on failure"
    (with-org-canvas-test-config
      (let ((get-urls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (when (eq method 'GET)
                       (push url get-urls))
                     (cond
                      ((string-match "rubrics/555" url)
                       '((id . 555) (title . "Stuck Rubric")
                         (context_type . "Course") (context_id . 99999)
                         (reusable . t) (read_only . :json-false)
                         (assessments . [((id . 1) (assessment_type . "grading")
                                          (artifact_type . "Submission")
                                          (artifact_id . 42) (score . 85))])))
                      ((string-match "assignments" url)
                       [((id . 10) (name . "HW1") (rubric_id . 555)
                         (rubric_settings . ((id . 801))))])
                      (t nil)))))
          (org-canvas--rubric-log-diagnostics
           555 '((id . 555) (title . "Stuck Rubric") (points_possible . 100)))
          (expect (cl-some (lambda (u) (string-match "rubrics/555" u)) get-urls)
                  :to-be-truthy)
          (expect (cl-some (lambda (u) (string-match "assignments" u)) get-urls)
                  :to-be-truthy)))))

  (it "handles errors in diagnostic fetch gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Network error")))))
        (org-canvas--rubric-log-diagnostics 999 '((id . 999) (title . "Bad")))))))

(describe "org-canvas-delete-all-rubrics (mocked)"
  (it "dissociates rubrics before deleting"
    (let ((temp-dir (make-temp-file "rubrics-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "rubrics.org" temp-dir))
                 (dissociate-called nil)
                 (deleted-count 0))
            (with-temp-file org-file
              (insert "* Rubric
:PROPERTIES:
:CANVAS_ID: 111
:END:
** Item :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"))
            (let ((org-canvas-rubrics-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                          ((symbol-function 'org-canvas--rubric-dissociate-all)
                           (lambda () (setq dissociate-called t) 0))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (cond
                              ((eq method 'GET)
                               [((id . 111) (title . "Rubric"))])
                              ((eq method 'DELETE)
                               (setq deleted-count (1+ deleted-count))
                               nil)))))
                  (org-canvas-delete-all-rubrics)
                  (expect dissociate-called :to-be t)
                  (expect deleted-count :to-equal 1)))))
        (delete-directory temp-dir t))))

  (it "calls diagnostics when rubric still exists after failed deletion"
    (with-org-canvas-test-config
      (let ((diagnostics-called nil))
        (with-sync-test-env
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                    ((symbol-function 'org-canvas--rubric-dissociate-all)
                     (lambda () 0))
                    ((symbol-function 'org-canvas--rubric-log-diagnostics)
                     (lambda (id _data)
                       (push id diagnostics-called)))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _args)
                       (cond
                        ((and (eq method 'GET) (string-match-p "rubrics$" url))
                         [((id . 200) (title . "OK Rubric"))
                          ((id . 201) (title . "Bad Rubric"))])
                        ((and (eq method 'GET) (string-match "rubrics/201" url))
                         '((id . 201) (title . "Bad Rubric")))
                        ((and (eq method 'DELETE) (string-match "200" url))
                         nil)
                        ((and (eq method 'DELETE) (string-match "201" url))
                         (signal 'error '("500 Internal Server Error")))))))
            (let ((org-canvas-rubrics-file "/nonexistent"))
              (org-canvas-delete-all-rubrics)
              (expect (length diagnostics-called) :to-equal 1)
              (expect (car diagnostics-called) :to-equal 201)))))))

  (it "counts rubric as deleted when DELETE returns 500 but GET returns 404"
    (with-org-canvas-test-config
      (let ((diagnostics-called nil))
        (with-sync-test-env
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                    ((symbol-function 'org-canvas--rubric-dissociate-all)
                     (lambda () 0))
                    ((symbol-function 'org-canvas--rubric-log-diagnostics)
                     (lambda (id _data)
                       (push id diagnostics-called)))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _args)
                       (cond
                        ((and (eq method 'GET) (string-match-p "rubrics$" url))
                         [((id . 300) (title . "Ghost Rubric"))])
                        ((and (eq method 'GET) (string-match "rubrics/300" url))
                         (signal 'error '("HTTP error 404")))
                        ((and (eq method 'DELETE) (string-match "300" url))
                         (signal 'error '("500 Internal Server Error")))))))
            (let ((org-canvas-rubrics-file "/nonexistent"))
              (org-canvas-delete-all-rubrics)
              (expect diagnostics-called :to-be nil))))))))

;;;; Stage 4: Finalize

(describe "org-canvas--rubric-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Item :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading)
     (let ((data (list :title "Test Rubric" :pom (point-marker)))
           (response '((rubric . ((id . 99999))))))
       (org-canvas--rubric-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "99999"))))

  (it "handles response with id at top level"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
** Item :5pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 5 | |
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 88888))))
       (org-canvas--rubric-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "88888"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:
** Item :5pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 5 | |
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 77777))))
       (org-canvas--rubric-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))

  (it "does not save CANVAS_ID when response has no id"
    (with-temp-org-buffer
     "* No ID Rubric
:PROPERTIES:
:END:
** Item :5pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 5 | |
"
     (org-back-to-heading)
     (let ((data (list :title "No ID Rubric" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (org-canvas--rubric-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))))

;;;; Pull Function Tests (new format)

(describe "org-canvas--rubric-pull-item"
  (it "emits a child heading per criterion with sub-table"
    (with-temp-org-buffer
     "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((id . 1) (title . "My Rubric")
                   (data . [((description . "Quality")
                             (points . 10)
                             (long_description . "Well written")
                             (ratings . [((description . "Full Marks") (points . 10))
                                         ((description . "No Marks") (points . 0))]))]))))
       (org-canvas--rubric-pull-item item (point))
       (let ((output (buffer-string)))
         (expect output :to-match "^\\*\\* Quality[ \t]+:10pt:$")
         (expect output :to-match "Well written")
         (expect output :to-match "| Rating | Points | Description |")
         (expect output :to-match "Full Marks")
         (expect output :to-match "No Marks")))))

  (it "always emits sub-table even for default Full/No Marks ratings"
    (with-temp-org-buffer
     "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((id . 1) (title . "My Rubric")
                   (data . [((description . "Quality")
                             (points . 10)
                             (long_description . "")
                             (ratings . [((description . "Full Marks") (points . 10))
                                         ((description . "No Marks") (points . 0))]))]))))
       (org-canvas--rubric-pull-item item (point))
       (let ((output (buffer-string)))
         (expect output :to-match "| Rating | Points | Description |")
         (expect output :to-match "Full Marks")
         (expect output :to-match "No Marks")))))

  (it "emits OUTCOME property when criterion has learning_outcome_id"
    (cl-letf (((symbol-function 'org-canvas--rubric-outcome-title)
               (lambda (_id) "Communication")))
      (with-temp-org-buffer
       "* Outcomey Rubric
:PROPERTIES:
:CANVAS_ID: 200
:END:
"
       (org-back-to-heading)
       (let ((item '((id . 200) (title . "Outcomey Rubric")
                     (data . [((description . "Articulates clearly")
                               (points . 4)
                               (long_description . "")
                               (learning_outcome_id . 999)
                               (ratings . [((description . "Full Marks") (points . 4))
                                           ((description . "No Marks") (points . 0))]))]))))
         (org-canvas--rubric-pull-item item (point))
         (let ((output (buffer-string)))
           (expect output :to-match
                   ":OUTCOME:[ ]+\\[\\[file:outcomes\\.org::\\*Communication"))))))

  (it "escapes pipe characters in criterion descriptions"
    (with-temp-org-buffer
     "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((id . 1) (title . "My Rubric")
                   (data . [((description . "Good|Bad")
                             (points . 5)
                             (long_description . "")
                             (ratings . [((description . "Full Marks") (points . 5))]))]))))
       (org-canvas--rubric-pull-item item (point))
       (expect (buffer-string) :to-match "Good/Bad"))))

  (it "does nothing for nil criteria"
    (with-temp-org-buffer
     "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
Keep this body
"
     (org-back-to-heading)
     (let ((item '((id . 1) (title . "My Rubric"))))
       (org-canvas--rubric-pull-item item (point))
       (expect (buffer-string) :to-match "Keep this body"))))

  (it "sorts rating rows by points descending"
    (with-temp-org-buffer
     "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((id . 1) (title . "My Rubric")
                   (data . [((description . "Quality")
                             (points . 10)
                             (long_description . "")
                             (ratings . [((description . "Poor") (points . 0))
                                         ((description . "Excellent") (points . 10))
                                         ((description . "Good") (points . 7))]))]))))
       (org-canvas--rubric-pull-item item (point))
       (let* ((content (buffer-string))
              (exc-pos (string-match "Excellent" content))
              (good-pos (string-match "Good" content))
              (poor-pos (string-match "Poor" content)))
         (expect exc-pos :to-be-truthy)
         (expect (< exc-pos good-pos) :to-be t)
         (expect (< good-pos poor-pos) :to-be t))))))

(describe "org-canvas-pull-rubrics"
  (it "pulls rubrics from Canvas"
    (let* ((temp-dir (make-temp-file "pull-rubric-test" t))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-rubrics-file rubrics-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 1) (title . "Essay Rubric")
                                (data . [((description . "Thesis")
                                          (points . 20)
                                          (long_description . "")
                                          (ratings . [((description . "Full Marks") (points . 20))
                                                      ((description . "No Marks") (points . 0))]))])))))
                          ((symbol-function 'y-or-n-p) (lambda (_) t)))
                  (org-canvas-pull-rubrics)
                  (with-current-buffer (find-file-noselect rubrics-file)
                    (expect (buffer-string) :to-match "Essay Rubric")
                    (expect (buffer-string) :to-match "Thesis"))))))
        (let ((buf (find-buffer-visiting rubrics-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Rubric Diagnostics Tests

(describe "org-canvas--rubric-log-detail"
  (it "logs assessment info when present"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   '((id . 1)
                     (context_type . "Course")
                     (context_id . 99)
                     (reusable . t)
                     (read_only . nil)
                     (assessments . [((id . 5) (assessment_type . "grading")
                                      (artifact_type . "Submission")
                                      (artifact_id . 10) (score . 85))])))))
        (org-canvas--rubric-log-detail 1)
        (expect 'org-canvas--log-warning :to-have-been-called))))

  (it "logs when no assessments"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   '((id . 1) (context_type . "Course") (context_id . 99)
                     (reusable . nil) (read_only . nil) (assessments . nil)))))
        (org-canvas--rubric-log-detail 1)
        (expect 'org-canvas--log-warning :to-have-been-called))))

  (it "handles API error"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("404 Not Found")))))
        (org-canvas--rubric-log-detail 1)
        (expect 'org-canvas--log-warning :to-have-been-called)))))

(describe "org-canvas--rubric-find-linked-assignments"
  (it "returns empty list when no assignments match"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   [((id . 1) (name . "HW") (rubric_id . 999))])))
        (let ((result (org-canvas--rubric-find-linked-assignments "123")))
          (expect result :to-equal nil))))))

(describe "org-canvas--rubric-log-linked-assignments"
  (it "logs warning when no assignments reference the rubric"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   [((id . 1) (name . "HW1") (rubric_id . 999))
                    ((id . 2) (name . "HW2") (rubric_id . 888))])))
        (org-canvas--rubric-log-linked-assignments "123")
        (expect 'org-canvas--log-warning :to-have-been-called-with
                org-canvas--logger
                "  [Assignments] No assignments reference this rubric"))))

  (it "handles API error gracefully"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Server error")))))
        (org-canvas--rubric-log-linked-assignments "123")
        (expect 'org-canvas--log-warning :to-have-been-called)))))

(describe "org-canvas-delete-all-rubrics confirmation"
  (it "aborts when user declines"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (expect (org-canvas-delete-all-rubrics) :to-throw 'user-error))))

;;;; Custom-rating Builds (new format: ratings come from criterion plist)

(describe "org-canvas--rubric-build-criterion with rating list"
  (it "builds default 2-level ratings when ratings list is nil"
    (let ((crit (org-canvas--rubric-build-criterion
                 (test-rubric--criterion "Quality" 10) 0)))
      (let* ((obj (plist-get crit :obj))
             (ratings (gethash "ratings" obj)))
        (expect (gethash "0" ratings) :to-be-truthy)
        (expect (gethash "1" ratings) :to-be-truthy)
        (expect (gethash "2" ratings) :to-be nil)
        (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full Marks")
        (expect (gethash "description" (gethash "1" ratings)) :to-equal "No Marks"))))

  (it "builds custom ratings from criterion :ratings"
    (let* ((cp (list :description "Quality" :points 10 :long-description ""
                     :outcome-link nil
                     :ratings '(("Excellent" 10 "") ("Good" 7 "") ("Poor" 0 ""))))
           (crit (org-canvas--rubric-build-criterion cp 0))
           (obj (plist-get crit :obj))
           (ratings (gethash "ratings" obj)))
      (expect (gethash "0" ratings) :to-be-truthy)
      (expect (gethash "1" ratings) :to-be-truthy)
      (expect (gethash "2" ratings) :to-be-truthy)
      (expect (gethash "description" (gethash "0" ratings)) :to-equal "Excellent")
      (expect (gethash "points" (gethash "0" ratings)) :to-equal 10)
      (expect (gethash "description" (gethash "1" ratings)) :to-equal "Good")
      (expect (gethash "points" (gethash "1" ratings)) :to-equal 7)
      (expect (gethash "description" (gethash "2" ratings)) :to-equal "Poor")
      (expect (gethash "points" (gethash "2" ratings)) :to-equal 0))))

(describe "org-canvas--rubric-build-payload with multi-level ratings"
  (it "builds criteria with default ratings when no ratings list"
    (let* ((data (list :title "Test" :free-form nil
                       :criteria (list (test-rubric--criterion "Quality" 10))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric))
           (crit0 (gethash "0" criteria))
           (ratings (gethash "ratings" crit0)))
      (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full Marks")))

  (it "builds criteria with custom ratings from plist :ratings"
    (let* ((cp (list :description "Quality" :points 10 :long-description ""
                     :outcome-link nil
                     :ratings '(("Excellent" 10 "") ("Good" 7 "") ("Poor" 0 ""))))
           (data (list :title "Test" :free-form nil :criteria (list cp)))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be nil)
      (let ((ratings (gethash "ratings" (gethash "0" criteria))))
        (expect (gethash "0" ratings) :to-be-truthy)
        (expect (gethash "1" ratings) :to-be-truthy)
        (expect (gethash "2" ratings) :to-be-truthy)
        (expect (gethash "description" (gethash "0" ratings)) :to-equal "Excellent"))))

  (it "round-trips parsed criteria through build-payload"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Code Quality :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Excellent | 10 | |
| Good | 7 | |
| Fair | 3 | |
| Poor | 0 | |
** Correctness :15pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 15 | |
| No Marks | 0 | |
"
     (org-back-to-heading)
     (let* ((data (org-canvas--rubric-parse-entry))
            (payload (org-canvas--rubric-build-payload data))
            (rubric (gethash "rubric" payload))
            (criteria (gethash "criteria" rubric)))
       (let* ((code-quality nil)
              (correctness nil))
         (maphash (lambda (_k v)
                    (cond
                     ((string= (gethash "description" v) "Code Quality")
                      (setq code-quality v))
                     ((string= (gethash "description" v) "Correctness")
                      (setq correctness v))))
                  criteria)
         (expect code-quality :to-be-truthy)
         (let ((ratings (gethash "ratings" code-quality)))
           (expect (gethash "0" ratings) :to-be-truthy)
           (expect (gethash "3" ratings) :to-be-truthy)
           (expect (gethash "description" (gethash "0" ratings)) :to-equal "Excellent"))
         (expect correctness :to-be-truthy)
         (let ((ratings (gethash "ratings" correctness)))
           (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full Marks")))))))

(describe "org-canvas--rubric-sort-ratings"
  (it "sorts ratings by points descending"
    (let ((sorted (org-canvas--rubric-sort-ratings
                   '(((description . "Poor") (points . 0))
                     ((description . "Good") (points . 7))
                     ((description . "Excellent") (points . 10))))))
      (expect (alist-get 'description (nth 0 sorted)) :to-equal "Excellent")
      (expect (alist-get 'description (nth 1 sorted)) :to-equal "Good")
      (expect (alist-get 'description (nth 2 sorted)) :to-equal "Poor"))))

;;;; Outcome Links

(describe "org-canvas--rubric-build-criterion with outcome-link"
  (it "resolves outcome-link to learning_outcome_id"
    (let* ((temp-dir (make-temp-file "rubric-outcome-build" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Skills\n** Python\n:PROPERTIES:\n:CANVAS_ID: 51479\n:END:\n"))
            (let ((org-canvas-rubrics-file rubrics-file)
                  (org-canvas-outcomes-file outcomes-file))
              (let* ((link (format "[[file:%s::*Python][Python]]" outcomes-file))
                     (cp (list :description "Quality" :points 10
                               :long-description "" :outcome-link link
                               :ratings nil))
                     (crit (org-canvas--rubric-build-criterion cp 0))
                     (obj (plist-get crit :obj)))
                (expect (gethash "learning_outcome_id" obj) :to-equal "51479"))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "omits learning_outcome_id when outcome-link is nil"
    (let ((crit (org-canvas--rubric-build-criterion
                 (test-rubric--criterion "Quality" 10) 0)))
      (let ((obj (plist-get crit :obj)))
        (expect (gethash "learning_outcome_id" obj) :to-be nil)))))

(describe "org-canvas--rubric-build-payload with outcome links"
  (it "resolves :outcome-link to learning_outcome_id"
    (let* ((temp-dir (make-temp-file "rubric-outcome-payload" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Programming\n** Python Proficiency\n:PROPERTIES:\n:CANVAS_ID: 51479\n:END:\n"))
            (let ((org-canvas-rubrics-file rubrics-file)
                  (org-canvas-outcomes-file outcomes-file)
                  (org-canvas-course-id "99999"))
              (let* ((link (format "[[file:%s::*Python Proficiency][Python Proficiency]]" outcomes-file))
                     (data (list :title "Test" :free-form nil
                                 :criteria (list (list :description "Code Quality"
                                                       :points 10
                                                       :long-description ""
                                                       :outcome-link link
                                                       :ratings nil)
                                                 (test-rubric--criterion "Correctness" 10))))
                     (payload (org-canvas--rubric-build-payload data))
                     (rubric (gethash "rubric" payload))
                     (criteria (gethash "criteria" rubric))
                     (c0 (gethash "0" criteria))
                     (c1 (gethash "1" criteria)))
                (expect (gethash "learning_outcome_id" c0) :to-equal "51479")
                (expect (gethash "learning_outcome_id" c1) :to-be nil))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "handles criteria without outcome-link without errors"
    (let* ((data (list :title "Test" :free-form nil
                       :criteria (list (test-rubric--criterion "Quality" 10)
                                       (test-rubric--criterion "Format" 5))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric))
           (c0 (gethash "0" criteria))
           (c1 (gethash "1" criteria)))
      (expect (gethash "learning_outcome_id" c0) :to-be nil)
      (expect (gethash "learning_outcome_id" c1) :to-be nil))))

;;;; Pull-side outcome handling

(describe "org-canvas--rubric-pull-item with outcome links"
  (it "emits OUTCOME property for criteria with learning_outcome_id"
    (let* ((temp-dir (make-temp-file "rubric-pull-outcome" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Skills\n** Python Proficiency\n:PROPERTIES:\n:CANVAS_ID: 51479\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (with-temp-org-buffer
               "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
               (org-back-to-heading)
               (let ((item '((id . 1) (title . "My Rubric")
                             (data . [((description . "Code Quality")
                                       (points . 10)
                                       (long_description . "")
                                       (learning_outcome_id . 51479)
                                       (ratings . [((description . "Full Marks") (points . 10))
                                                   ((description . "No Marks") (points . 0))]))
                                      ((description . "Correctness")
                                       (points . 10)
                                       (long_description . "")
                                       (ratings . [((description . "Full Marks") (points . 10))
                                                   ((description . "No Marks") (points . 0))]))]))))
                 (org-canvas--rubric-pull-item item (point))
                 (let ((content (buffer-string)))
                   (expect content :to-match ":OUTCOME:")
                   (expect content :to-match "Python Proficiency")
                   (expect content :to-match "^\\*\\* Code Quality[ \t]+:10pt:$")
                   (expect content :to-match "^\\*\\* Correctness[ \t]+:10pt:$"))))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "omits :OUTCOME: property when no learning_outcome_id"
    (with-temp-org-buffer
     "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((id . 1) (title . "My Rubric")
                   (data . [((description . "Quality")
                             (points . 10)
                             (long_description . "Well written")
                             (ratings . [((description . "Full Marks") (points . 10))
                                         ((description . "No Marks") (points . 0))]))]))))
       (org-canvas--rubric-pull-item item (point))
       (let ((content (buffer-string)))
         (expect content :not :to-match ":OUTCOME:")))))

  (it "falls back to outcome ID string when title not found"
    (let* ((temp-dir (make-temp-file "rubric-pull-nolookup" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Skills\n** Other Outcome\n:PROPERTIES:\n:CANVAS_ID: 999\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (with-temp-org-buffer
               "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
               (org-back-to-heading)
               (let ((item '((id . 1) (title . "My Rubric")
                             (data . [((description . "Quality")
                                       (points . 10)
                                       (long_description . "")
                                       (learning_outcome_id . 12345)
                                       (ratings . [((description . "Full Marks") (points . 10))]))]))))
                 (org-canvas--rubric-pull-item item (point))
                 (expect (buffer-string) :to-match "12345")))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--rubric-outcome-title"
  (it "returns heading title for matching CANVAS_ID"
    (let* ((temp-dir (make-temp-file "outcome-title-test" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Group\n** My Outcome\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (expect (org-canvas--rubric-outcome-title 42) :to-equal "My Outcome")))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns nil when CANVAS_ID not found"
    (let* ((temp-dir (make-temp-file "outcome-title-test" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Group\n** Other\n:PROPERTIES:\n:CANVAS_ID: 99\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (expect (org-canvas--rubric-outcome-title 42) :to-be nil)))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns nil when outcomes file does not exist"
    (let ((org-canvas-outcomes-file "/nonexistent/outcomes.org"))
      (expect (org-canvas--rubric-outcome-title 42) :to-be nil)))

  (it "handles string outcome-id"
    (let* ((temp-dir (make-temp-file "outcome-title-str" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Group\n** My Outcome\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (expect (org-canvas--rubric-outcome-title "42") :to-equal "My Outcome")))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--rubric-pull-emit-criterion HTML conversion"
  (it "converts HTML descriptions via html-to-org-inline"
    (cl-letf (((symbol-function 'org-canvas--html-to-org-inline)
               (lambda (html) (concat "C:" html))))
      (with-temp-buffer
        (let ((c '((description . "<b>Bold</b>") (points . 5)
                   (long_description . "<em>Italic</em>")
                   (ratings . [])))) ; no ratings list
          (org-canvas--rubric-pull-emit-criterion c)
          (expect (buffer-string) :to-match "C:<b>Bold</b>")
          (expect (buffer-string) :to-match "C:<em>Italic</em>")))))

  (it "converts HTML rating descriptions via html-to-org-inline"
    (cl-letf (((symbol-function 'org-canvas--html-to-org-inline)
               (lambda (html) (concat "R:" html)))
              ((symbol-function 'org-canvas--rubric-sort-ratings)
               (lambda (r) (append r nil))))
      (with-temp-buffer
        (let ((c '((description . "D") (points . 5) (long_description . "")
                   (ratings . [((description . "<i>Good</i>") (points . 5))]))))
          (org-canvas--rubric-pull-emit-criterion c)
          (expect (buffer-string) :to-match "R:<i>Good</i>"))))))

;;;; Delete at Point

(describe "org-canvas-delete-rubric-at-point"
  (it "deletes rubric and clears properties"
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
           (org-canvas-delete-rubric-at-point)
           (expect-api-called 'DELETE "rubrics/42")
           (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
           (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))

  (it "errors when no CANVAS_ID"
    (with-temp-org-buffer
     "* New
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-rubric-at-point) :to-throw 'user-error))))

;;; org-canvas-rubrics-test.el ends here
