;;; org-canvas-core-api-test.el --- Buttercup tests for org-canvas-core API layer  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)

;;;; 3. API Layer

(describe "org-canvas-api-course-endpoint"
  (it "constructs basic course endpoint"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "pages")
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/pages")))

  (it "constructs endpoint with format args"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "assignments/%s" 12345)
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/assignments/12345")))

  (it "handles multiple format args"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "quizzes/%s/questions/%s" 100 200)
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/quizzes/100/questions/200")))

  (it "handles empty suffix"
    (with-org-canvas-test-config
      (expect (org-canvas-api-course-endpoint "")
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/")))

  (it "strips trailing slash from base-url before joining"
    (let ((org-canvas-base-url "https://clemson.instructure.com/")
          (org-canvas-course-id "281704"))
      (expect (org-canvas-api-course-endpoint "pages")
              :to-equal "https://clemson.instructure.com/api/v1/courses/281704/pages")))

  (it "leaves base-url alone when there is no trailing slash"
    (let ((org-canvas-base-url "https://clemson.instructure.com")
          (org-canvas-course-id "281704"))
      (expect (org-canvas-api-course-endpoint "pages")
              :to-equal "https://clemson.instructure.com/api/v1/courses/281704/pages"))))

(describe "org-canvas--build-curl-command"
  (it "builds GET request without -X flag"
    (let ((cmd (org-canvas--build-curl-command 'GET "https://example.com/api" nil)))
      (expect cmd :not :to-match "-X GET")
      (expect cmd :to-match "Authorization: Bearer \\$CANVAS_TOKEN")
      (expect cmd :to-match "\"https://example.com/api\"")))

  (it "builds POST request with -X POST"
    (let ((cmd (org-canvas--build-curl-command 'POST "https://example.com/api" nil)))
      (expect cmd :to-match "-X POST")))

  (it "builds PUT request with -X PUT"
    (let ((cmd (org-canvas--build-curl-command 'PUT "https://example.com/api" nil)))
      (expect cmd :to-match "-X PUT")))

  (it "builds DELETE request with -X DELETE"
    (let ((cmd (org-canvas--build-curl-command 'DELETE "https://example.com/api" nil)))
      (expect cmd :to-match "-X DELETE")))

  (it "includes JSON payload in -d flag"
    (let ((cmd (org-canvas--build-curl-command 'POST "https://example.com/api"
                                               "{\"key\":\"value\"}")))
      (expect cmd :to-match "-d '{\"key\":\"value\"}'")))

  (it "includes Content-Type header"
    (let ((cmd (org-canvas--build-curl-command 'GET "https://example.com/api" nil)))
      (expect cmd :to-match "Content-Type: application/json"))))

(describe "org-canvas-api-request (mocked)"
  (it "records API calls"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/api")
        (expect (test-org-canvas-api-call-count) :to-equal 1))))

  (it "returns mock response"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((response (org-canvas-api-request 'GET "https://example.com/api")))
          (expect (alist-get 'id response) :to-equal 12345)))))

  (it "tracks POST calls correctly"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'POST "https://example.com/pages"
                                :data '((title . "Test")))
        (expect-api-called 'POST "pages"))))

  (it "handles multiple API calls"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/pages")
        (org-canvas-api-request 'POST "https://example.com/assignments")
        (org-canvas-api-request 'PUT "https://example.com/quizzes")
        (expect (test-org-canvas-api-call-count) :to-equal 3)))))

;;;; 14. API Request Error Handling

(describe "org-canvas-api-request error handling (mocked)"
  (it "handles GET request with params"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/api"
                                :params '(("per_page" . "50")))
        (expect (test-org-canvas-api-call-count) :to-equal 1))))

  (it "encodes data as JSON for POST"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'POST "https://example.com/api"
                                :data '((name . "Test") (value . 42)))
        (expect-api-called 'POST "api")))))

;;;; 15. Additional API Request Tests

(describe "org-canvas-api-request edge cases (mocked)"
  (it "handles timeout parameter"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'GET "https://example.com/api" :timeout 30)
        (expect (test-org-canvas-api-call-count) :to-equal 1))))

  (it "encodes hash-table data as JSON"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data (make-hash-table)))
          (puthash 'name "Test" data)
          (org-canvas-api-request 'POST "https://example.com/api" :data data)
          (expect-api-called 'POST "api")))))

  (it "passes string data as-is"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas-api-request 'POST "https://example.com/api"
                                :data "{\"already\":\"json\"}")
        (expect-api-called 'POST "api")))))

;;;; 24. org-canvas-api-request internals (mock plz directly)

(describe "org-canvas-api-request internals"
  (it "builds query string from params"
    (with-org-canvas-test-config
      (let ((captured-url nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method url &rest _args)
                     (setq captured-url url)
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api"
                                  :params '(("per_page" . "50") ("page" . "2")))
          (expect captured-url :to-match "\\?per_page=50")
          (expect captured-url :to-match "page=2")))))

  (it "encodes alist data as JSON body"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (org-canvas-api-request 'POST "https://example.com/api"
                                  :data '((name . "Test") (value . 42)))
          (expect captured-body :to-be-truthy)
          ;; Should be valid JSON
          (let ((parsed (json-read-from-string captured-body)))
            (expect (alist-get 'name parsed) :to-equal "Test")
            (expect (alist-get 'value parsed) :to-equal 42))))))

  (it "encodes hash-table data as JSON body"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (let ((data (make-hash-table)))
            (puthash 'key "value" data)
            (org-canvas-api-request 'POST "https://example.com/api" :data data)
            (expect captured-body :to-be-truthy)
            (expect captured-body :to-match "\"key\""))))))

  (it "passes string data as-is without re-encoding"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (org-canvas-api-request 'POST "https://example.com/api"
                                  :data "{\"already\":\"json\"}")
          (expect captured-body :to-equal "{\"already\":\"json\"}")))))

  (it "downcases method symbol for plz"
    (with-org-canvas-test-config
      (let ((captured-method nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (method _url &rest _args)
                     (setq captured-method method)
                     '((id . 1)))))
          (org-canvas-api-request 'POST "https://example.com/api")
          (expect captured-method :to-equal 'post)))))

  (it "passes timeout to plz"
    (with-org-canvas-test-config
      (let ((captured-timeout nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-timeout (plist-get args :timeout))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api" :timeout 30)
          (expect captured-timeout :to-equal 30)))))

  (it "uses default timeout when none specified"
    (with-org-canvas-test-config
      (let ((captured-timeout nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-timeout (plist-get args :timeout))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api")
          (expect captured-timeout :to-equal test-org-canvas-request-timeout)))))

  (it "sends Authorization header with token"
    (with-org-canvas-test-config
      (let ((captured-headers nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-headers (plist-get args :headers))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api")
          (expect (cdr (assoc "Authorization" captured-headers))
                  :to-equal (concat "Bearer " test-org-canvas-api-token))))))

  (it "converts plz-error to standard error signal"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (_method _url &rest _args)
                   (signal 'plz-error (list (make-plz-error
                                             :response (make-plz-response
                                                        :status 500
                                                        :body "Internal Error")))))))
        (expect (org-canvas-api-request 'GET "https://example.com/api")
                :to-throw 'error))))

  (it "includes HTTP status in error message"
    (with-org-canvas-test-config
      (let ((err-msg nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest _args)
                     (signal 'plz-error (list (make-plz-error
                                               :response (make-plz-response
                                                          :status 403
                                                          :body "Forbidden")))))))
          (condition-case err
              (org-canvas-api-request 'GET "https://example.com/api")
            (error (setq err-msg (cadr err))))
          (expect err-msg :to-match "403")))))

  (it "returns nil for no-body responses"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (_method _url &rest _args)
                   nil)))
        (let ((result (org-canvas-api-request 'DELETE "https://example.com/api")))
          (expect result :to-be nil)))))

  (it "sends no body for nil data"
    (with-org-canvas-test-config
      (let ((captured-body nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest args)
                     (setq captured-body (plist-get args :body))
                     '((id . 1)))))
          (org-canvas-api-request 'GET "https://example.com/api")
          (expect captured-body :to-be nil))))))

;;;; 26. org-canvas-test-connection error path

(describe "org-canvas-test-connection error path"
  (it "handles API error gracefully"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Connection refused")))))
        ;; Should not throw, just message
        (org-canvas-test-connection)
        ;; If we get here, it handled the error
        (expect t :to-be t))))

  (it "logs error on failure"
    (with-org-canvas-test-config
      (let ((error-logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (signal 'error '("Connection refused"))))
                  ((symbol-function 'org-canvas--log-error)
                   (lambda (&rest _args)
                     (setq error-logged t)
                     nil)))
          (org-canvas-test-connection)
          (expect error-logged :to-be t)))))

  (it "shows 401 specific message"
    (with-org-canvas-test-config
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("HTTP 401 Unauthorized")))))
        (org-canvas-test-connection)
        (expect 'message :to-have-been-called-with
                "Connection failed: authentication error (HTTP 401). Regenerate your API token."))))

  (it "shows 403 specific message"
    (with-org-canvas-test-config
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("HTTP 403 Forbidden")))))
        (org-canvas-test-connection)
        (expect 'message :to-have-been-called-with
                "Connection failed: permission denied (HTTP 403). Check your token scope."))))

  (it "shows 404 specific message"
    (with-org-canvas-test-config
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("HTTP 404 Not Found")))))
        (org-canvas-test-connection)
        (expect 'message :to-have-been-called-with
                "Connection failed: course not found (HTTP 404). Check your course ID."))))

  (it "shows network error specific message"
    (with-org-canvas-test-config
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   (signal 'error '("Could not resolve host: canvas.example.com")))))
        (org-canvas-test-connection)
        (expect 'message :to-have-been-called-with
                "Connection failed: network error. Check your URL and internet connection.")))))

(describe "org-canvas-api-request body logging"
  (it "logs request body when org-canvas-log-request-bodies is t"
    (let ((logged-messages nil))
      (with-org-canvas-test-config
        (let ((org-canvas-log-request-bodies t))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest _args) '((id . 1))))
                    ((symbol-function 'org-canvas--log-debug)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged-messages))))
            (org-canvas-api-request 'POST "https://example.com/api"
                                    :data '((title . "Test")))
            (expect (cl-some (lambda (m) (string-match-p "Body:" m)) logged-messages)
                    :to-be-truthy))))))

  (it "logs response body when org-canvas-log-request-bodies is t"
    (let ((logged-messages nil))
      (with-org-canvas-test-config
        (let ((org-canvas-log-request-bodies t))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest _args) '((id . 1) (name . "Test"))))
                    ((symbol-function 'org-canvas--log-debug)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged-messages))))
            (org-canvas-api-request 'GET "https://example.com/api")
            (expect (cl-some (lambda (m) (string-match-p "Response Body:" m)) logged-messages)
                    :to-be-truthy)))))))

(describe "org-canvas-api-request plz-error handling"
  (it "signals a concise message with the HTTP status (no body/struct in data)"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (let ((resp (make-plz-response :status 422 :body "Validation failed")))
                     (signal 'plz-error (make-plz-error :response resp))))))
        (condition-case err
            (org-canvas-api-request 'POST "https://example.com/api"
                                    :data '((bad . "data")))
          (error
           (expect (cadr err) :to-match "HTTP 422")
           (expect (cddr err) :to-be nil))))))

  (it "surfaces the Canvas JSON error message in the signaled message"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (let ((resp (make-plz-response
                                :status 400
                                :body "{\"errors\":{\"published\":[{\"attribute\":\"published\",\"type\":\"invalid\",\"message\":\"The front page cannot be unpublished\"}]}}")))
                     (signal 'plz-error (make-plz-error :response resp))))))
        (condition-case err
            (org-canvas-api-request 'PUT "https://example.com/api")
          (error
           (expect (cadr err)
                   :to-equal "The front page cannot be unpublished (HTTP 400)"))))))

  (it "handles plz-error without response"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error (make-plz-error :response nil)))))
        (condition-case err
            (org-canvas-api-request 'GET "https://example.com/api")
          (error
           (expect (cadr err) :to-match "API Request Failed:")))))))

;;;; Pagination Helper

(describe "org-canvas-api-request-all-pages"
  (it "returns all items from a single page"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   ;; Return fewer than 100 items -> single page
                   [((id . 1)) ((id . 2)) ((id . 3))])))
        (let ((result (org-canvas-api-request-all-pages 'GET "https://example.com/api")))
          (expect (length result) :to-equal 3)))))

  (it "aggregates items across multiple pages"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq call-count (1+ call-count))
                     (let* ((params (plist-get args :params))
                            (page (cdr (assoc "page" params))))
                       (cond
                        ;; First page: return exactly 100 items
                        ((string= page "1")
                         (make-vector 100 '((id . 1))))
                        ;; Second page: return fewer (end of data)
                        ((string= page "2")
                         [((id . 101)) ((id . 102))]))))))
          (let ((result (org-canvas-api-request-all-pages 'GET "https://example.com/api")))
            (expect (length result) :to-equal 102)
            (expect call-count :to-equal 2))))))

  (it "passes additional params along with pagination"
    (with-org-canvas-test-config
      (let ((received-params nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq received-params (plist-get args :params))
                     [])))
          (org-canvas-api-request-all-pages 'GET "https://example.com/api"
                                            '(("only_announcements" . "true")))
          (expect (assoc "only_announcements" received-params) :to-be-truthy)
          (expect (assoc "per_page" received-params) :to-be-truthy)
          (expect (assoc "page" received-params) :to-be-truthy)))))

  (it "fetches all pages until a short page is returned"
    (with-org-canvas-test-config
      (let* ((page-1 (vconcat (cl-loop for i from 1 to 100 collect `((id . ,i)))))
             (page-2 (vector '((id . 101)) '((id . 102))))
             (calls 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (cl-incf calls)
                     (pcase calls
                       (1 page-1)
                       (2 page-2)
                       (_ (vector))))))
          (let ((result (org-canvas-api-request-all-pages 'GET "https://example.invalid/api/v1/items")))
            (expect (length result) :to-equal 102)
            (expect calls :to-equal 2)))))))

;;;; 36. Credential validation in org-canvas-api-request

(describe "org-canvas-api-request credential validation"
  (it "errors when api-token is empty"
    (let ((org-canvas-api-token "")
          (org-canvas-course-id "12345"))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error)))

  (it "errors when api-token is nil"
    (let ((org-canvas-api-token nil)
          (org-canvas-course-id "12345"))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error)))

  (it "errors when course-id is empty"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id ""))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error)))

  (it "errors when course-id is nil"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id nil))
      (expect (org-canvas-api-request 'GET "https://example.com/api")
              :to-throw 'error))))

;;;; Preflight Check

(describe "org-canvas--preflight-check"
  (it "errors when API token is empty"
    (let ((org-canvas-api-token ""))
      (expect (org-canvas--preflight-check) :to-throw 'error
              '("API token not configured.  Set org-canvas-api-token in org-canvas-credentials.el\nRun M-x org-canvas-init for guided setup"))))

  (it "errors when API token is nil"
    (let ((org-canvas-api-token nil))
      (expect (org-canvas--preflight-check) :to-throw 'error
              '("API token not configured.  Set org-canvas-api-token in org-canvas-credentials.el\nRun M-x org-canvas-init for guided setup"))))

  (it "errors when course ID is empty"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id ""))
      (expect (org-canvas--preflight-check) :to-throw 'error
              '("Course ID not configured.  Set org-canvas-course-id in org-canvas-credentials.el\nRun M-x org-canvas-init for guided setup"))))

  (it "signals connection error with actionable message"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (error "API Request Failed (HTTP 401)"))))
        (expect (org-canvas--preflight-check) :to-throw 'error))))

  (it "succeeds when connection works"
    (let ((org-canvas-api-token "valid-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) '((name . "Test Course")))))
        (expect (org-canvas--preflight-check) :not :to-throw)))))

;;;; Rate Limit Handling

(describe "org-canvas-api-request rate limit handling"
  (it "retries on 429 response"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345")
          (org-canvas-rate-limit-retries 2)
          (org-canvas-rate-limit-wait 1)
          (call-count 0))
      (spy-on 'message)
      (spy-on 'sleep-for)
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (setq call-count (1+ call-count))
                   (if (= call-count 1)
                       (signal 'plz-error
                               (make-plz-error
                                :response (make-plz-response :status 429 :body "rate limit")))
                     '((id . 1))))))
        (let ((result (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")))
          (expect call-count :to-equal 2)
          (expect (alist-get 'id result) :to-equal 1)
          (expect 'message :to-have-been-called-with
                  "Rate limited (HTTP %d). Retrying in %ds..." 429 1)))))

  (it "fails after exhausting retries"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345")
          (org-canvas-rate-limit-retries 1)
          (org-canvas-rate-limit-wait 1))
      (spy-on 'sleep-for)
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error
                            :response (make-plz-response :status 429 :body "rate limit"))))))
        (expect (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")
                :to-throw 'error)))))

;;;; HTTP 401 Specific Message

(describe "org-canvas-api-request 401 handling"
  (it "provides actionable error message for 401"
    (let ((org-canvas-api-token "expired-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error
                            :response (make-plz-response :status 401 :body "Unauthorized"))))))
        (condition-case err
            (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")
          (error
           (expect (cadr err) :to-match "Authentication failed.*401")))))))

;;;; HTTP 403 Handling

(describe "org-canvas-api-request 403 handling"
  (it "provides permission denied message for 403"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error
                            :response (make-plz-response :status 403 :body "forbidden"))))))
        (condition-case err
            (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")
          (error
           (expect (cadr err) :to-match "Permission denied.*403"))))))

  (it "retries 403 with rate limit indication"
    (let ((org-canvas-api-token "test-token")
          (org-canvas-course-id "12345")
          (org-canvas-rate-limit-retries 1)
          (org-canvas-rate-limit-wait 1)
          (call-count 0))
      (spy-on 'sleep-for)
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (setq call-count (1+ call-count))
                   (if (= call-count 1)
                       (signal 'plz-error
                               (make-plz-error
                                :response (make-plz-response :status 403 :body "rate limit exceeded")))
                     '((id . 1))))))
        (let ((result (org-canvas-api-request 'GET "https://test.example.com/api/v1/test")))
          (expect call-count :to-equal 2)
          (expect (alist-get 'id result) :to-equal 1))))))

(describe "org-canvas-api-request-all-pages pagination progress"
  (it "shows progress for each page fetched"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (spy-on 'message)
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         ;; First page: 100 items (full page triggers next)
                         (make-list 100 '((id . 1)))
                       ;; Second page: fewer than 100 (done)
                       (make-list 5 '((id . 2)))))))
          (org-canvas-api-request-all-pages 'GET "https://test.example.com/api/v1/pages")
          (expect 'message :to-have-been-called-with
                  "Fetching page %d (%d items so far)..." 1 0)
          (expect 'message :to-have-been-called-with
                  "Fetching page %d (%d items so far)..." 2 100)))))

  (it "shows progress for single page"
    (with-org-canvas-test-config
      (spy-on 'message)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args) '((id . 1)))))
        (org-canvas-api-request-all-pages 'GET "https://test.example.com/api/v1/pages")
        (expect 'message :to-have-been-called-with
                "Fetching page %d (%d items so far)..." 1 0)))))

(describe "org-canvas--api-handle-plz-error rate-limit countdown"
  (it "calls sleep-for 1 second at a time instead of full duration"
    (with-org-canvas-test-config
      (let ((org-canvas-rate-limit-wait 3)
            (org-canvas-rate-limit-retries 1))
        (spy-on 'sleep-for)
        (spy-on 'message)
        (let ((result (org-canvas--api-handle-plz-error
                       (cons 'plz-error
                             (make-plz-error
                              :response (make-plz-response :status 429 :body "rate limit")))
                       "https://test.example.com/api")))
          (expect result :to-equal :retry)
          (expect 'sleep-for :to-have-been-called-times 3)
          (expect 'sleep-for :to-have-been-called-with 1)))))

  (it "shows descending countdown messages"
    (with-org-canvas-test-config
      (let ((org-canvas-rate-limit-wait 3)
            (org-canvas-rate-limit-retries 1)
            (messages nil))
        (spy-on 'sleep-for)
        (spy-on 'message :and-call-fake
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages)))
        (org-canvas--api-handle-plz-error
         (cons 'plz-error
               (make-plz-error
                :response (make-plz-response :status 429 :body "rate limit")))
         "https://test.example.com/api")
        (setq messages (nreverse messages))
        (expect (nth 0 messages) :to-match "Retrying in 3s")
        (expect (nth 1 messages) :to-match "Retrying in 2s")
        (expect (nth 2 messages) :to-match "Retrying in 1s")))))

(describe "transient retry"
  (it "retries plz curl-28 timeouts up to retry-delays length"
    (let* ((calls 0)
           (org-canvas-transient-retry-delays '(0 0))
           (org-canvas-api-token "test-token")
           (org-canvas-course-id "281704")
           (org-canvas-base-url "https://example.invalid"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (cl-incf calls)
                   (if (< calls 3)
                       (signal 'plz-error
                               (make-plz-error :curl-error '(28 . "Operation timeout.")))
                     '((id . 42)))))
                ((symbol-function 'sleep-for) (lambda (_) nil)))
        (let ((result (org-canvas-api-request 'GET "https://example.invalid/api/v1/x")))
          (expect calls :to-equal 3)
          (expect (alist-get 'id result) :to-equal 42)))))

  (it "retries HTTP 503 then succeeds"
    (let* ((calls 0)
           (org-canvas-transient-retry-delays '(0))
           (org-canvas-api-token "test-token")
           (org-canvas-course-id "281704")
           (org-canvas-base-url "https://example.invalid"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (cl-incf calls)
                   (if (= calls 1)
                       (signal 'plz-error
                               (make-plz-error
                                :response (make-plz-response :status 503 :body "")))
                     '((id . 7)))))
                ((symbol-function 'sleep-for) (lambda (_) nil)))
        (let ((result (org-canvas-api-request 'GET "https://example.invalid/api/v1/x")))
          (expect calls :to-equal 2)
          (expect (alist-get 'id result) :to-equal 7)))))

  (it "raises after exhausting retry-delays"
    (let* ((org-canvas-transient-retry-delays '(0))
           (org-canvas-api-token "test-token")
           (org-canvas-course-id "281704")
           (org-canvas-base-url "https://example.invalid"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error :curl-error '(28 . "Operation timeout.")))))
                ((symbol-function 'sleep-for) (lambda (_) nil)))
        (expect (org-canvas-api-request 'GET "https://example.invalid/api/v1/x")
                :to-throw 'org-canvas-api-error))))

  (it "scrubs cookies from the error signaled after exhausting retries on 503"
    (let* ((org-canvas-transient-retry-delays '(0))
           (org-canvas-api-token "test-token")
           (org-canvas-course-id "281704")
           (org-canvas-base-url "https://example.invalid"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error
                            :response (make-plz-response
                                       :status 503
                                       :headers '((set-cookie . "canvas_session=SECRET"))
                                       :body "unavailable")))))
                ((symbol-function 'sleep-for) (lambda (_) nil)))
        (let ((caught (condition-case e
                          (org-canvas-api-request 'GET "https://example.invalid/api/v1/x")
                        (org-canvas-api-error e))))
          (expect (car caught) :to-be 'org-canvas-api-error)
          (expect (format "%S" caught) :not :to-match "SECRET"))))))

(describe "org-canvas--associate-rubric failure message"
  (it "shows warning in echo area on failure"
    (with-org-canvas-test-config
      (spy-on 'message)
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Network error"))))
        (org-canvas--associate-rubric "42" "99" "Assignment")
        (expect 'message :to-have-been-called-with
                "WARNING: Rubric association failed for %s: %s" "42" "Network error")))))

(describe "org-canvas--upload-file"
  (it "errors when Canvas returns no upload_url"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   ;; Canvas returns response without upload_url
                   '((upload_params . ((key . "abc")))))))
        (let ((temp-file (make-temp-file "upload-test" nil ".pdf")))
          (unwind-protect
              (progn
                (with-temp-file temp-file (insert "content"))
                (expect (org-canvas--upload-file temp-file)
                        :to-throw 'error))
            (delete-file temp-file)))))))

;;;; Fault-injection matrix

(defun org-canvas-fault--status-err (status &optional body)
  "Build a condition-case value for a plz-error with HTTP STATUS and BODY."
  (cons 'plz-error
        (make-plz-error :response (make-plz-response :status status
                                                     :body (or body "")))))

(defun org-canvas-fault--curl-err (code)
  "Build a condition-case value for a plz curl error CODE."
  (cons 'plz-error (make-plz-error :curl-error (cons code "curl error"))))

(describe "org-canvas--api-handle-plz-error fault matrix"
  ;; Each Canvas failure mode must map to its documented outcome: retry
  ;; (rate-limit), retry-transient (5xx / curl), or a signaled error.
  (it "429 rate limit -> :retry"
    (let ((org-canvas-rate-limit-wait 0))
      (expect (org-canvas--api-handle-plz-error
               (org-canvas-fault--status-err 429 "rate limited") "u")
              :to-equal :retry)))

  (it "403 with a rate-limit body -> :retry"
    (let ((org-canvas-rate-limit-wait 0))
      (expect (org-canvas--api-handle-plz-error
               (org-canvas-fault--status-err 403 "rate limit exceeded") "u")
              :to-equal :retry)))

  (it "401 -> credentials error (expired token)"
    (expect (org-canvas--api-handle-plz-error
             (org-canvas-fault--status-err 401 "unauthorized") "u")
            :to-throw 'org-canvas-credentials-error))

  (it "403 (non-rate-limit) -> credentials error (scope)"
    (expect (org-canvas--api-handle-plz-error
             (org-canvas-fault--status-err 403 "forbidden") "u")
            :to-throw 'org-canvas-credentials-error))

  (it "502/503/504 -> :retry-transient"
    (dolist (status '(502 503 504))
      (expect (org-canvas--api-handle-plz-error
               (org-canvas-fault--status-err status "gateway") "u")
              :to-equal :retry-transient)))

  (it "500 -> generic api error"
    (expect (org-canvas--api-handle-plz-error
             (org-canvas-fault--status-err 500 "server error") "u")
            :to-throw 'org-canvas-api-error))

  (it "curl connect/timeout/recv errors -> :retry-transient"
    (dolist (code '(7 28 56))
      (expect (org-canvas--api-handle-plz-error
               (org-canvas-fault--curl-err code) "u")
              :to-equal :retry-transient))))

(describe "org-canvas-api-request method guard"
  (it "rejects unsupported methods with a clear error before any network activity"
    (with-org-canvas-test-config
      (let ((plz-called nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _args) (setq plz-called t))))
          (expect (org-canvas-api-request 'OPTIONS "https://example.invalid/api/v1/x")
                  :to-throw 'org-canvas-api-error)
          (expect plz-called :to-be nil)))))

  (it "names the offending method in the error message"
    (with-org-canvas-test-config
      (let ((caught (condition-case e
                        (org-canvas-api-request 'OPTIONS "https://example.invalid/api/v1/x")
                      (org-canvas-api-error e))))
        (expect (error-message-string caught) :to-match "OPTIONS"))))

  (it "dispatches PATCH to the curl fallback, not plz"
    (with-org-canvas-test-config
      (let ((plz-called nil)
            (curl-args nil))
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _args) (setq plz-called t)))
                  ((symbol-function 'org-canvas--api-curl-patch)
                   (lambda (url _headers payload _timeout)
                     (setq curl-args (list url payload))
                     '((id . 9)))))
          (let ((result (org-canvas-api-request 'PATCH "https://example.invalid/api/v1/x"
                                                :data '((title . "T")))))
            (expect result :to-equal '((id . 9)))
            (expect plz-called :to-be nil)
            (expect (car curl-args) :to-equal "https://example.invalid/api/v1/x")
            (expect (cadr curl-args) :to-equal "{\"title\":\"T\"}")))))))

(describe "org-canvas--api-curl-patch-config"
  (it "includes the method, headers, url, and trailing data-binary directive"
    (let ((config (org-canvas--api-curl-patch-config
                   "https://x.test/api" '(("Authorization" . "Bearer tok")
                                          ("Content-Type" . "application/json"))
                   30 "{\"a\":1}")))
      (expect config :to-match "request = \"PATCH\"")
      (expect config :to-match "header = \"Authorization: Bearer tok\"")
      (expect config :to-match "url = \"https://x.test/api\"")
      (expect config :to-match "max-time = 30")
      ;; data-binary must be the final directive so curl reads the
      ;; remaining stdin as the body
      (expect config :to-match "data-binary = \"@-\"\n\\'")))

  (it "omits data-binary when there is no body"
    (expect (org-canvas--api-curl-patch-config "https://x.test/api" nil 30 nil)
            :not :to-match "data-binary")))

(describe "org-canvas--api-curl-patch-parse"
  (it "returns parsed JSON on 2xx"
    (expect (org-canvas--api-curl-patch-parse 0 "{\"id\": 555}\n200")
            :to-equal '((id . 555))))

  (it "returns nil for an empty 2xx body"
    (expect (org-canvas--api-curl-patch-parse 0 "\n204") :to-be nil))

  (it "signals plz-error with a response struct on HTTP error status"
    (let ((err (condition-case e
                   (org-canvas--api-curl-patch-parse
                    0 "{\"errors\":[{\"message\":\"nope\"}]}\n400")
                 (plz-error (cdr e)))))
      (expect (plz-response-status (plz-error-response err)) :to-equal 400)
      (expect (plz-response-body (plz-error-response err)) :to-match "nope")))

  (it "signals plz-error with a curl-error on non-zero exit"
    (let ((err (condition-case e
                   (org-canvas--api-curl-patch-parse 7 "connection refused")
                 (plz-error (cdr e)))))
      (expect (car (plz-error-curl-error err)) :to-equal 7)))

  (it "signals plz-error when output has no status code"
    (expect (org-canvas--api-curl-patch-parse 0 "garbage with no status")
            :to-throw 'plz-error)))

(describe "org-canvas--api-curl-patch"
  (it "writes config and body to stdin and parses curl output"
    (let ((seen-stdin nil))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (_program infile _dest _display &rest _args)
                   (setq seen-stdin (with-temp-buffer
                                      (insert-file-contents infile)
                                      (buffer-string)))
                   (insert "{\"id\": 42}\n200")
                   0)))
        (let ((result (org-canvas--api-curl-patch
                       "https://x.test/api"
                       '(("Authorization" . "Bearer tok"))
                       "{\"quiz\":{\"title\":\"T\"}}" 30)))
          (expect result :to-equal '((id . 42)))
          (expect seen-stdin :to-match "request = \"PATCH\"")
          (expect seen-stdin :to-match "Bearer tok")
          ;; Body follows the config in the same stdin stream
          (expect seen-stdin :to-match "data-binary = \"@-\"\n{\"quiz\":{\"title\":\"T\"}}\\'"))))))

(describe "org-canvas--api-error-message"
  (it "extracts per-attribute error messages"
    (expect (org-canvas--api-error-message
             "{\"errors\":{\"published\":[{\"attribute\":\"published\",\"type\":\"invalid\",\"message\":\"The front page cannot be unpublished\"}]}}")
            :to-equal "The front page cannot be unpublished"))

  (it "extracts messages from arrays of message objects"
    (expect (org-canvas--api-error-message
             "{\"errors\":[{\"message\":\"only one late policy per course is allowed\"}]}")
            :to-equal "only one late policy per course is allowed"))

  (it "extracts bare-string error arrays"
    (expect (org-canvas--api-error-message
             "{\"errors\":[\"something went wrong\"]}")
            :to-equal "something went wrong"))

  (it "uses a top-level message field"
    (expect (org-canvas--api-error-message "{\"message\":\"invalid request\"}")
            :to-equal "invalid request"))

  (it "joins distinct messages and dedupes repeats"
    (expect (org-canvas--api-error-message
             "{\"errors\":{\"name\":[{\"message\":\"too long\"},{\"message\":\"too long\"}],\"weight\":[{\"message\":\"not a number\"}]}}")
            :to-equal "too long; not a number"))

  (it "returns nil for non-JSON bodies"
    (expect (org-canvas--api-error-message "Internal Server Error") :to-be nil)
    (expect (org-canvas--api-error-message nil) :to-be nil)
    (expect (org-canvas--api-error-message "{not valid json") :to-be nil))

  (it "returns nil for JSON without any message"
    (expect (org-canvas--api-error-message "{\"ok\":true}") :to-be nil)))

(describe "org-canvas--scrub-plz-error"
  (it "masks set-cookie headers in the response"
    (let* ((err (make-plz-error
                 :response (make-plz-response
                            :status 422
                            :headers '((set-cookie . "canvas_session=SECRET; path=/")
                                       (content-type . "application/json"))
                            :body "{\"errors\":[]}")))
           (clean (org-canvas--scrub-plz-error err))
           (headers (plz-response-headers (plz-error-response clean))))
      (expect (cdr (assq 'set-cookie headers)) :to-equal "***MASKED***")
      (expect (cdr (assq 'content-type headers)) :to-equal "application/json")
      (expect (plz-response-status (plz-error-response clean)) :to-equal 422)
      (expect (plz-response-body (plz-error-response clean))
              :to-equal "{\"errors\":[]}")))

  (it "does not mutate the original error"
    (let* ((err (make-plz-error
                 :response (make-plz-response
                            :status 500
                            :headers '((set-cookie . "canvas_session=SECRET")))))
           (_ (org-canvas--scrub-plz-error err)))
      (expect (cdr (assq 'set-cookie
                         (plz-response-headers (plz-error-response err))))
              :to-equal "canvas_session=SECRET")))

  (it "passes through a plz-error without a response"
    (let ((err (make-plz-error :curl-error '(28 . "Operation timeout."))))
      (expect (org-canvas--scrub-plz-error err) :to-be err)))

  (it "passes through non-plz-error values"
    (expect (org-canvas--scrub-plz-error nil) :to-be nil)
    (expect (org-canvas--scrub-plz-error "boom") :to-equal "boom")))

(describe "org-canvas--api-handle-plz-error cookie scrubbing"
  (it "signals errors whose data carries no live cookie values"
    (let* ((err (cons 'plz-error
                      (make-plz-error
                       :response (make-plz-response
                                  :status 500
                                  :headers '((set-cookie . "canvas_session=SECRET; path=/")
                                             (set-cookie . "_csrf_token=ALSOSECRET"))
                                  :body "server error"))))
           (caught (condition-case e
                       (org-canvas--api-handle-plz-error err "http://x")
                     (org-canvas-api-error e))))
      (expect (error-message-string caught) :not :to-match "SECRET")
      (expect (format "%S" caught) :not :to-match "SECRET"))))

;;;; Network guard (test-helper)

(describe "network guard"
  (it "refuses unmocked plz calls"
    (expect (plz 'get "https://example.com/") :to-throw 'error))

  (it "refuses unmocked url-retrieve-synchronously calls"
    (expect (url-retrieve-synchronously "https://example.com/")
            :to-throw 'error))

  (it "refuses spawning the real curl binary"
    (expect (call-process plz-curl-program nil nil nil "--version")
            :to-throw 'error))

  (it "does not name credentials in the refusal"
    (let ((err (condition-case e (plz 'get "https://example.com/") (error e))))
      (expect (format "%S" err) :not :to-match "Bearer")
      (expect (format "%S" err) :to-match "Unmocked network call")))

  (it "lets non-curl subprocesses through"
    ;; `true' exits 0 — proves the call-process guard is curl-specific
    (expect (call-process "true") :to-equal 0))

  (it "is bypassed by cl-letf mocks like real tests use"
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest _) "mocked")))
      (expect (plz 'get "https://example.com/") :to-equal "mocked"))))

(describe "feature URL resolution (issue #87)"
  (it "lists under the course by default"
    (with-org-canvas-test-config
      (expect (org-canvas--feature-list-url '(:endpoint "assignments"))
              :to-equal (org-canvas-api-course-endpoint "assignments"))
      (expect (org-canvas--feature-item-url '(:endpoint "assignments") 42)
              :to-equal (org-canvas-api-course-endpoint "assignments/%s" 42))))

  (it "lets a feature name its own list and item URLs"
    (let ((feature (list :endpoint "calendar_events"
                         :list-url-fn (lambda () "https://x/api/v1/calendar_events")
                         :item-url-fn (lambda (id)
                                        (format "https://x/api/v1/calendar_events/%s" id)))))
      (expect (org-canvas--feature-list-url feature)
              :to-equal "https://x/api/v1/calendar_events")
      (expect (org-canvas--feature-item-url feature 42)
              :to-equal "https://x/api/v1/calendar_events/42")))

  (it "calls a function-valued list-params each time, and passes a list through"
    (let ((n 0))
      (expect (org-canvas--feature-list-params
               (list :list-params (lambda () (setq n (1+ n)) `(("n" . ,n)))))
              :to-equal '(("n" . 1)))
      (expect (org-canvas--feature-list-params '(:list-params (("type" . "event"))))
              :to-equal '(("type" . "event")))
      (expect (org-canvas--feature-list-params '(:endpoint "pages")) :to-be nil))))

(describe "org-canvas--feature-modified-field (issue #94)"
  (it "defaults to updated_at and honours a declaration"
    (expect (org-canvas--feature-modified-field '(:endpoint "assignments"))
            :to-be 'updated_at)
    (expect (org-canvas--feature-modified-field '(:modified-field modified_at))
            :to-be 'modified_at))

  (it "is declared by files, whose updated_at moves on metadata touches"
    (expect (org-canvas--feature-modified-field
             (org-canvas--registry-find-feature "files"))
            :to-be 'modified_at)))

(provide 'org-canvas-core-api-test)
;;; org-canvas-core-api-test.el ends here
