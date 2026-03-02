;;; org-canvas-rubrics-test.el --- Buttercup tests for rubrics  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-rubrics)

;;;; Stage 1: Parse Entry

(describe "org-canvas--rubric-parse-entry"
  (it "extracts title from heading"
    (with-temp-org-buffer
     "* Essay Rubric
:PROPERTIES:
:END:

| Criterion | Points | Description |
|-----------+--------+-------------|
| Thesis    |     20 | Clear thesis |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :title) :to-equal "Essay Rubric"))))

  (it "errors on empty title"
    (with-temp-org-buffer
     (concat "* " "\n:PROPERTIES:\n:END:\n\n"
             "| Criterion | Points | Description |\n"
             "|-----------+--------+-------------|\n"
             "| Thesis    |     20 | Clear thesis |\n")
     (org-back-to-heading)
     (expect (org-canvas--rubric-parse-entry) :to-throw 'error)))

  (it "extracts canvas-id when present"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:CANVAS_ID: 33333
:END:

| Criterion | Points |
|-----------+--------|
| Quality   |     10 |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :canvas-id) :to-equal "33333"))))

  (it "returns nil canvas-id for new rubrics"
    (with-temp-org-buffer
     "* New Rubric
:PROPERTIES:
:END:

| Criterion | Points |
|-----------+--------|
| Quality   |     10 |
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

| Criterion | Points |
|-----------+--------|
| Item      |      5 |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :free-form) :to-be t))))

  (it "extracts criteria from table"
    (with-temp-org-buffer
     "* Rubric
:PROPERTIES:
:END:

| Criterion   | Points | Long Description |
|-------------+--------+------------------|
| Clarity     |     25 | Writing clarity  |
| Originality |     25 | Original ideas   |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :criteria) :to-be-truthy)
       (expect (length (plist-get data :criteria)) :to-be-greater-than 0))))

  (it "errors when no table found"
    (with-temp-org-buffer
     "* Rubric Without Table
:PROPERTIES:
:END:

Just some text, no table.
"
     (org-back-to-heading)
     (expect (org-canvas--rubric-parse-entry) :to-throw 'error)))

  (it "includes pom in data"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:

| Criterion | Points |
|-----------+--------|
| Item      |     10 |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (expect (plist-get data :pom) :to-be-truthy)))))

;;;; Stage 2: Build Payload

(describe "org-canvas--rubric-build-payload"
  (it "wraps in rubric key"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Criterion" "10" "Description"))))
           (payload (org-canvas--rubric-build-payload data)))
      (expect (gethash "rubric" payload) :to-be-truthy)))

  (it "includes title in payload"
    (let* ((data '(:title "Grading Rubric" :free-form nil
                   :criteria (("Item" "5" ""))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload)))
      (expect (gethash "title" rubric) :to-equal "Grading Rubric")))

  (it "sets free_form_criterion_comments"
    (let* ((data '(:title "Test" :free-form t
                   :criteria (("Item" "5" ""))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload)))
      (expect (gethash "free_form_criterion_comments" rubric) :to-be t)))

  (it "builds criteria hash"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Clarity" "20" "Clear writing")
                              ("Structure" "15" "Good organization"))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      (expect criteria :to-be-truthy)
      ;; Should have two criteria (keys "0" and "1")
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be-truthy)))

  (it "includes rubric_association"
    (with-org-canvas-test-config
      (let* ((data '(:title "Test" :free-form nil
                     :criteria (("Item" "5" ""))))
             (payload (org-canvas--rubric-build-payload data)))
        (expect (gethash "rubric_association" payload) :to-be-truthy))))

  (it "sets free_form_criterion_comments to :json-false when nil"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Item" "5" ""))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload)))
      (expect (gethash "free_form_criterion_comments" rubric) :to-equal :json-false)))

  (it "skips hlines in criteria table"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Header" "Points" "Description")
                              hline
                              ("Criterion1" "10" "Desc1")
                              ("Criterion2" "20" "Desc2"))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      ;; Should have 3 criteria (header + 2 rows, hline skipped)
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be-truthy)
      (expect (gethash "2" criteria) :to-be-truthy)
      (expect (gethash "3" criteria) :to-be nil)))

  (it "includes ratings with Full Marks and No Marks"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Quality" "25" "Description"))))
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
          ;; Should have called DELETE first, then POST
          (expect-api-called 'DELETE "rubrics/555")
          (expect-api-called 'POST "rubrics")))))

  (it "recovers from timeout by searching for created rubric"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (setq call-count (1+ call-count))
                     (cond
                      ;; First call: search for existing (none found)
                      ((and (eq method 'GET) (= call-count 1))
                       [])
                      ;; Second call: POST that times out
                      ((eq method 'POST)
                       (signal 'error '("Timeout waiting for response")))
                      ;; Third call: recovery search finds the rubric
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
                      ;; First call: search for existing (none found)
                      ((and (eq method 'GET) (= call-count 1))
                       [])
                      ;; Second call: POST that fails with non-timeout error
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
                      ;; First call: search finds existing rubric
                      ((and (eq method 'GET) (= call-count 1))
                       [((id . 111) (title . "In Use Rubric"))])
                      ;; Second call: DELETE fails (rubric in use)
                      ((eq method 'DELETE)
                       (setq delete-called t)
                       (signal 'error '("Rubric is in use and cannot be deleted")))
                      ;; Third call: POST succeeds anyway
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
                      ;; First call: search for existing (none found)
                      ((and (eq method 'GET) (= call-count 1))
                       [])
                      ;; Second call: POST that times out
                      ((eq method 'POST)
                       (signal 'error '("Timeout waiting for response")))
                      ;; Third call: recovery search finds nothing
                      ((and (eq method 'GET) (>= call-count 3))
                       nil)
                      (t nil)))))
          (let ((data '(:title "Lost Rubric"))
                (payload (make-hash-table)))
            (expect (org-canvas--rubric-push-to-api data payload)
                    :to-throw 'error)))))))

(describe "org-canvas--rubric-search-by-title (mocked)"
  (it "searches rubrics endpoint"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("rubrics" . [((id . 100) (title . "Essay Rubric"))])))
        (org-canvas--rubric-search-by-title "Essay Rubric")
        (expect-api-called 'GET "rubrics"))))

  (it "returns matching rubric"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("rubrics" . [((id . 100) (title . "Rubric A"))
                              ((id . 101) (title . "Rubric B"))])))
        (let ((result (org-canvas--rubric-search-by-title "Rubric A")))
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
        (expect (org-canvas--rubric-dissociate-all) :to-equal 0)))))

(describe "org-canvas--rubric-log-diagnostics (mocked)"
  (it "fetches rubric detail and assignments on failure"
    (with-org-canvas-test-config
      (let ((get-urls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (when (eq method 'GET)
                       (push url get-urls))
                     (cond
                      ;; Individual rubric detail
                      ((string-match "rubrics/555" url)
                       '((id . 555) (title . "Stuck Rubric")
                         (context_type . "Course") (context_id . 99999)
                         (reusable . t) (read_only . :json-false)
                         (assessments . [((id . 1) (assessment_type . "grading")
                                          (artifact_type . "Submission")
                                          (artifact_id . 42) (score . 85))])))
                      ;; Assignments list
                      ((string-match "assignments" url)
                       [((id . 10) (name . "HW1") (rubric_id . 555)
                         (rubric_settings . ((id . 801))))])
                      (t nil)))))
          (org-canvas--rubric-log-diagnostics
           555 '((id . 555) (title . "Stuck Rubric") (points_possible . 100)))
          ;; Should have fetched rubric detail and assignments
          (expect (cl-some (lambda (u) (string-match "rubrics/555" u)) get-urls)
                  :to-be-truthy)
          (expect (cl-some (lambda (u) (string-match "assignments" u)) get-urls)
                  :to-be-truthy)))))

  (it "handles errors in diagnostic fetch gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Network error")))))
        ;; Should not throw - errors are caught and logged
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

| Criterion | Points |
|-----------+--------|
| Item      |     10 |
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
                        ;; List all rubrics
                        ((and (eq method 'GET) (string-match-p "rubrics$" url))
                         [((id . 200) (title . "OK Rubric"))
                          ((id . 201) (title . "Bad Rubric"))])
                        ;; Verify GET for rubric 201 succeeds = still exists
                        ((and (eq method 'GET) (string-match "rubrics/201" url))
                         '((id . 201) (title . "Bad Rubric")))
                        ((and (eq method 'DELETE) (string-match "200" url))
                         nil)
                        ((and (eq method 'DELETE) (string-match "201" url))
                         (signal 'error '("500 Internal Server Error")))))))
            (let ((org-canvas-rubrics-file "/nonexistent"))
              (org-canvas-delete-all-rubrics)
              ;; Diagnostics should be called only for the failed rubric
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
                        ;; List all rubrics
                        ((and (eq method 'GET) (string-match-p "rubrics$" url))
                         [((id . 300) (title . "Ghost Rubric"))])
                        ;; Verify GET returns 404 = rubric was deleted
                        ((and (eq method 'GET) (string-match "rubrics/300" url))
                         (signal 'error '("HTTP error 404")))
                        ;; DELETE returns 500
                        ((and (eq method 'DELETE) (string-match "300" url))
                         (signal 'error '("500 Internal Server Error")))))))
            (let ((org-canvas-rubrics-file "/nonexistent"))
              (org-canvas-delete-all-rubrics)
              ;; Diagnostics should NOT be called - rubric was confirmed deleted
              (expect diagnostics-called :to-be nil))))))))

;;;; Stage 4: Finalize

(describe "org-canvas--rubric-finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:

| Criterion | Points |
|-----------+--------|
| Item      |     10 |
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

| Criterion | Points |
|-----------+--------|
| Item      |      5 |
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 88888))))
       (org-canvas--rubric-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "88888"))))

  (it "saves LAST_SYNCED timestamp"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:END:

| Criterion | Points |
|-----------+--------|
| Item      |      5 |
"
     (org-back-to-heading)
     (let ((data (list :title "Test" :pom (point-marker)))
           (response '((id . 77777))))
       (org-canvas--rubric-finalize data response)
       (expect (org-entry-get (point) "LAST_SYNCED")
               :to-match "^\\[20[0-9][0-9]-"))))

  (it "does not save CANVAS_ID when response has no id"
    (with-temp-org-buffer
     "* No ID Rubric
:PROPERTIES:
:END:

| Criterion | Points |
|-----------+--------|
| Item      |      5 |
"
     (org-back-to-heading)
     (let ((data (list :title "No ID Rubric" :pom (point-marker)))
           (response '((error . "something went wrong"))))
       (org-canvas--rubric-finalize data response)
       (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)))))

;;;; Pull Function Tests

(describe "org-canvas--rubric-pull-item"
  (it "replaces body with criteria table"
    (with-temp-org-buffer
     "* My Rubric
:PROPERTIES:
:CANVAS_ID: 1
:END:
Old body text
"
     (org-back-to-heading)
     (let ((item '((id . 1) (title . "My Rubric")
                   (data . [((description . "Quality")
                             (points . 10)
                             (long_description . "Well written"))]))))
       (org-canvas--rubric-pull-item item (point))
       (expect (buffer-string) :to-match "| Quality | 10 | Well written |")
       (expect (buffer-string) :not :to-match "Old body text"))))

  (it "escapes pipe characters in criteria"
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
                             (long_description . "Either|Or"))]))))
       (org-canvas--rubric-pull-item item (point))
       (expect (buffer-string) :to-match "Good/Bad")
       (expect (buffer-string) :to-match "Either/Or"))))

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
       (expect (buffer-string) :to-match "Keep this body")))))

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
                                          (long_description . ""))])))))
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
      (spy-on 'elog-warning)
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
        (expect 'elog-warning :to-have-been-called))))

  (it "logs when no assessments"
    (with-org-canvas-test-config
      (spy-on 'elog-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   '((id . 1) (context_type . "Course") (context_id . 99)
                     (reusable . nil) (read_only . nil) (assessments . nil)))))
        (org-canvas--rubric-log-detail 1)
        (expect 'elog-warning :to-have-been-called))))

  (it "handles API error"
    (with-org-canvas-test-config
      (spy-on 'elog-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("404 Not Found")))))
        (org-canvas--rubric-log-detail 1)
        (expect 'elog-warning :to-have-been-called)))))

(describe "org-canvas--rubric-find-linked-assignments"
  (it "returns empty list when no assignments match"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   [((id . 1) (name . "HW") (rubric_id . 999))])))
        (let ((result (org-canvas--rubric-find-linked-assignments "123")))
          (expect result :to-equal nil))))))

(describe "org-canvas--rubric-log-linked-assignments"
  (it "handles API error gracefully"
    (with-org-canvas-test-config
      (spy-on 'elog-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Server error")))))
        (org-canvas--rubric-log-linked-assignments "123")
        (expect 'elog-warning :to-have-been-called)))))

(describe "org-canvas-delete-all-rubrics confirmation"
  (it "aborts when user declines"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (expect (org-canvas-delete-all-rubrics) :to-throw 'user-error))))

;;;; Multi-Level Ratings Tests

(describe "org-canvas--rubric-rating-row-p"
  (it "returns non-nil for > prefixed rows"
    (expect (org-canvas--rubric-rating-row-p '("> Excellent" "10" ""))
            :to-be-truthy))

  (it "returns nil for normal criterion rows"
    (expect (org-canvas--rubric-rating-row-p '("Quality" "10" ""))
            :to-be nil))

  (it "returns nil for hline"
    (expect (org-canvas--rubric-rating-row-p 'hline) :to-be nil)))

(describe "org-canvas--rubric-build-criterion with rating-rows"
  (it "builds default 2-level ratings when no rating-rows"
    (let ((crit (org-canvas--rubric-build-criterion '("Quality" "10" "Desc") 0)))
      (let* ((obj (plist-get crit :obj))
             (ratings (gethash "ratings" obj)))
        (expect (gethash "0" ratings) :to-be-truthy)
        (expect (gethash "1" ratings) :to-be-truthy)
        (expect (gethash "2" ratings) :to-be nil)
        (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full Marks")
        (expect (gethash "description" (gethash "1" ratings)) :to-equal "No Marks"))))

  (it "builds custom ratings from rating-rows"
    (let ((crit (org-canvas--rubric-build-criterion
                 '("Quality" "10" "Desc") 0
                 '(("> Excellent" "10" "") ("> Good" "7" "") ("> Poor" "0" "")))))
      (let* ((obj (plist-get crit :obj))
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

  (it "strips > prefix from rating descriptions"
    (let ((crit (org-canvas--rubric-build-criterion
                 '("Quality" "10" "") 0
                 '(("> Full" "10" "")))))
      (let* ((obj (plist-get crit :obj))
             (ratings (gethash "ratings" obj)))
        (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full"))))

  (it "uses nil rating-rows as default 2-level"
    (let ((crit (org-canvas--rubric-build-criterion '("Quality" "10" "") 0 nil)))
      (let* ((obj (plist-get crit :obj))
             (ratings (gethash "ratings" obj)))
        (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full Marks")))))

(describe "org-canvas--rubric-build-payload with multi-level ratings"
  (it "builds criteria with default ratings when no > rows"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Quality" "10" "Desc"))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric))
           (crit0 (gethash "0" criteria))
           (ratings (gethash "ratings" crit0)))
      (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full Marks")))

  (it "builds criteria with custom ratings from > rows"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Quality" "10" "Desc")
                              ("> Excellent" "10" "")
                              ("> Good" "7" "")
                              ("> Poor" "0" ""))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      ;; Should have 1 criterion (not 4)
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be nil)
      (let ((ratings (gethash "ratings" (gethash "0" criteria))))
        (expect (gethash "0" ratings) :to-be-truthy)
        (expect (gethash "1" ratings) :to-be-truthy)
        (expect (gethash "2" ratings) :to-be-truthy)
        (expect (gethash "description" (gethash "0" ratings)) :to-equal "Excellent"))))

  (it "mixes criteria with and without custom ratings"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Code Quality" "10" "")
                              ("> Excellent" "10" "")
                              ("> Good" "7" "")
                              ("Correctness" "15" ""))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      ;; Should have 2 criteria
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be-truthy)
      (expect (gethash "2" criteria) :to-be nil)
      ;; First criterion should have custom ratings
      (let ((ratings0 (gethash "ratings" (gethash "0" criteria))))
        (expect (gethash "description" (gethash "0" ratings0)) :to-equal "Excellent"))
      ;; Second criterion should have default ratings
      (let ((ratings1 (gethash "ratings" (gethash "1" criteria))))
        (expect (gethash "description" (gethash "0" ratings1)) :to-equal "Full Marks"))))

  (it "handles hlines mixed with rating rows"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Header" "Points" "Desc")
                              hline
                              ("Quality" "10" "Desc")
                              ("> Great" "10" "")
                              ("> OK" "5" ""))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      ;; Header + Quality = 2 criteria (hline skipped)
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be-truthy)
      (expect (gethash "2" criteria) :to-be nil)))

  (it "calculates total points correctly with rating rows"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Quality" "10" "")
                              ("> Excellent" "10" "")
                              ("> Poor" "0" "")
                              ("Format" "5" ""))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric))
           (c0 (gethash "0" criteria))
           (c1 (gethash "1" criteria)))
      (expect (gethash "points" c0) :to-equal 10)
      (expect (gethash "points" c1) :to-equal 5))))

(describe "org-canvas--rubric-has-custom-ratings"
  (it "returns nil for default 2-level ratings"
    (expect (org-canvas--rubric-has-custom-ratings
             '(((description . "Full Marks") (points . 10))
               ((description . "No Marks") (points . 0))))
            :to-be nil))

  (it "returns non-nil for 3+ level ratings"
    (expect (org-canvas--rubric-has-custom-ratings
             '(((description . "Excellent") (points . 10))
               ((description . "Good") (points . 7))
               ((description . "Poor") (points . 0))))
            :to-be-truthy))

  (it "returns non-nil for custom 2-level names"
    (expect (org-canvas--rubric-has-custom-ratings
             '(((description . "Pass") (points . 10))
               ((description . "Fail") (points . 0))))
            :to-be-truthy))

  (it "returns nil for nil ratings"
    (expect (org-canvas--rubric-has-custom-ratings nil) :to-be nil)))

(describe "org-canvas--rubric-sort-ratings"
  (it "sorts ratings by points descending"
    (let ((sorted (org-canvas--rubric-sort-ratings
                   '(((description . "Poor") (points . 0))
                     ((description . "Good") (points . 7))
                     ((description . "Excellent") (points . 10))))))
      (expect (alist-get 'description (nth 0 sorted)) :to-equal "Excellent")
      (expect (alist-get 'description (nth 1 sorted)) :to-equal "Good")
      (expect (alist-get 'description (nth 2 sorted)) :to-equal "Poor"))))

(describe "org-canvas--rubric-pull-item with multi-level ratings"
  (it "inserts > rows for custom ratings"
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
                             (ratings . [((description . "Excellent") (points . 10))
                                         ((description . "Good") (points . 7))
                                         ((description . "Poor") (points . 0))]))]))))
       (org-canvas--rubric-pull-item item (point))
       (expect (buffer-string) :to-match "| Quality | 10 |")
       (expect (buffer-string) :to-match "| > Excellent | 10 |")
       (expect (buffer-string) :to-match "| > Good | 7 |")
       (expect (buffer-string) :to-match "| > Poor | 0 |"))))

  (it "does not insert > rows for default 2-level ratings"
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
       (expect (buffer-string) :to-match "| Quality | 10 |")
       (expect (buffer-string) :not :to-match "> "))))

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
       ;; Verify order: Excellent (10) before Good (7) before Poor (0)
       (let ((content (buffer-string)))
         (expect (string-match "> Excellent" content) :to-be-truthy)
         (let ((exc-pos (string-match "> Excellent" content))
               (good-pos (string-match "> Good" content))
               (poor-pos (string-match "> Poor" content)))
           (expect (< exc-pos good-pos) :to-be t)
           (expect (< good-pos poor-pos) :to-be t))))))

  (it "round-trips multi-level ratings through pull and build"
    ;; Parse the pulled format and verify the build produces correct structure
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:

| Criterion | Points | Description |
|---|---|---|
| Code Quality | 10 | Well-written |
| > Excellent | 10 | |
| > Good | 7 | |
| > Fair | 3 | |
| > Poor | 0 | |
| Correctness | 15 | Correct output |
"
     (org-back-to-heading)
     (let ((data (org-canvas--rubric-parse-entry)))
       (let* ((payload (org-canvas--rubric-build-payload data))
              (rubric (gethash "rubric" payload))
              (criteria (gethash "criteria" rubric)))
         ;; Should have 3 criteria: header, Code Quality, Correctness
         ;; (Header row is criterion 0, Code Quality is 1, Correctness is 2)
         (let* ((code-quality nil)
                (correctness nil))
           ;; Find the Code Quality criterion by checking descriptions
           (maphash (lambda (_k v)
                      (cond
                       ((string= (gethash "description" v) "Code Quality")
                        (setq code-quality v))
                       ((string= (gethash "description" v) "Correctness")
                        (setq correctness v))))
                    criteria)
           ;; Code Quality should have 4 custom ratings
           (expect code-quality :to-be-truthy)
           (let ((ratings (gethash "ratings" code-quality)))
             (expect (gethash "0" ratings) :to-be-truthy)
             (expect (gethash "3" ratings) :to-be-truthy)
             (expect (gethash "description" (gethash "0" ratings)) :to-equal "Excellent"))
           ;; Correctness should have default 2-level ratings
           (expect correctness :to-be-truthy)
           (let ((ratings (gethash "ratings" correctness)))
             (expect (gethash "description" (gethash "0" ratings)) :to-equal "Full Marks"))))))))

;;;; Outcome Links in 4th Column

(describe "org-canvas--rubric-build-criterion with outcome-id"
  (it "adds learning_outcome_id when outcome-id is provided"
    (let ((crit (org-canvas--rubric-build-criterion
                 '("Quality" "10" "Desc") 0 nil "51479")))
      (let ((obj (plist-get crit :obj)))
        (expect (gethash "learning_outcome_id" obj) :to-equal "51479"))))

  (it "omits learning_outcome_id when outcome-id is nil"
    (let ((crit (org-canvas--rubric-build-criterion
                 '("Quality" "10" "Desc") 0 nil nil)))
      (let ((obj (plist-get crit :obj)))
        (expect (gethash "learning_outcome_id" obj) :to-be nil))))

  (it "includes learning_outcome_id with custom ratings"
    (let ((crit (org-canvas--rubric-build-criterion
                 '("Quality" "10" "Desc") 0
                 '(("> Good" "10" "") ("> Poor" "0" ""))
                 "99999")))
      (let ((obj (plist-get crit :obj)))
        (expect (gethash "learning_outcome_id" obj) :to-equal "99999")
        (expect (gethash "ratings" obj) :to-be-truthy)))))

(describe "org-canvas--rubric-build-payload with outcome links"
  (it "resolves 4th column outcome link to learning_outcome_id"
    (let* ((temp-dir (make-temp-file "rubric-outcome-test" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Programming Skills\n** Python Proficiency\n:PROPERTIES:\n:CANVAS_ID: 51479\n:END:\n"))
            (let ((org-canvas-rubrics-file rubrics-file)
                  (org-canvas-outcomes-file outcomes-file)
                  (org-canvas-course-id "99999"))
              (let* ((link (format "[[file:%s::*Python Proficiency][Python Proficiency]]" outcomes-file))
                     (data (list :title "Test" :free-form nil
                                 :criteria (list (list "Code Quality" "10" "Clean" link)
                                                 (list "Correctness" "10" "Correct" ""))))
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

  (it "handles 3-column tables without errors (backward compat)"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Quality" "10" "Desc")
                              ("Format" "5" "Clean"))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric))
           (c0 (gethash "0" criteria))
           (c1 (gethash "1" criteria)))
      (expect (gethash "learning_outcome_id" c0) :to-be nil)
      (expect (gethash "learning_outcome_id" c1) :to-be nil)))

  (it "ignores 4th column on rating rows"
    (let* ((data '(:title "Test" :free-form nil
                   :criteria (("Quality" "10" "Desc" "some-link")
                              ("> Good" "10" "" "ignored")
                              ("> Poor" "0" "" "also-ignored"))))
           (payload (org-canvas--rubric-build-payload data))
           (rubric (gethash "rubric" payload))
           (criteria (gethash "criteria" rubric)))
      ;; Should have 1 criterion (rating rows are grouped, not separate criteria)
      (expect (gethash "0" criteria) :to-be-truthy)
      (expect (gethash "1" criteria) :to-be nil)))

  (it "handles mixed criteria with and without outcome links"
    (let* ((temp-dir (make-temp-file "rubric-outcome-mix" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Skills\n** Python\n:PROPERTIES:\n:CANVAS_ID: 111\n:END:\n** Data\n:PROPERTIES:\n:CANVAS_ID: 222\n:END:\n"))
            (let ((org-canvas-rubrics-file rubrics-file)
                  (org-canvas-outcomes-file outcomes-file)
                  (org-canvas-course-id "99999"))
              (let* ((link1 (format "[[file:%s::*Python][Python]]" outcomes-file))
                     (link2 (format "[[file:%s::*Data][Data]]" outcomes-file))
                     (data (list :title "Test" :free-form nil
                                 :criteria (list (list "Quality" "10" "" link1)
                                                 (list "Middle" "5" "" "")
                                                 (list "End" "10" "" link2))))
                     (payload (org-canvas--rubric-build-payload data))
                     (rubric (gethash "rubric" payload))
                     (criteria (gethash "criteria" rubric)))
                (expect (gethash "learning_outcome_id" (gethash "0" criteria)) :to-equal "111")
                (expect (gethash "learning_outcome_id" (gethash "1" criteria)) :to-be nil)
                (expect (gethash "learning_outcome_id" (gethash "2" criteria)) :to-equal "222"))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--rubric-pull-item with outcome links"
  (it "includes 4th column with outcome link when learning_outcome_id present"
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
                                       (learning_outcome_id . 51479))
                                      ((description . "Correctness")
                                       (points . 10)
                                       (long_description . ""))]))))
                 (org-canvas--rubric-pull-item item (point))
                 (let ((content (buffer-string)))
                   ;; Should have 4-column header
                   (expect content :to-match "| Criterion | Points | Description | Outcome |")
                   ;; Code Quality should have outcome link
                   (expect content :to-match "Python Proficiency")
                   ;; Correctness should have empty outcome column
                   (expect content :to-match "| Correctness | 10 |  |  |"))))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "uses 3-column format when no criteria have outcomes"
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
                             (long_description . "Well written"))]))))
       (org-canvas--rubric-pull-item item (point))
       (let ((content (buffer-string)))
         (expect content :to-match "| Criterion | Points | Description |")
         (expect content :not :to-match "Outcome")))))

  (it "falls back to outcome ID when title not found in outcomes.org"
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
                                       (learning_outcome_id . 12345))]))))
                 (org-canvas--rubric-pull-item item (point))
                 ;; Should fall back to just the numeric ID
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
      (expect (org-canvas--rubric-outcome-title 42) :to-be nil))))

(describe "org-canvas--rubric-pull-has-outcomes"
  (it "returns non-nil when any criterion has learning_outcome_id"
    (expect (org-canvas--rubric-pull-has-outcomes
             [((learning_outcome_id . 42)) ((description . "Foo"))])
            :to-be-truthy))

  (it "returns nil when no criteria have learning_outcome_id"
    (expect (org-canvas--rubric-pull-has-outcomes
             [((description . "Foo")) ((description . "Bar"))])
            :to-be nil)))

;;; org-canvas-rubrics-test.el ends here
