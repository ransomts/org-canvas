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

;;; org-canvas-rubrics-test.el ends here
