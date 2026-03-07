;;; org-canvas-core-api-test.el --- Buttercup tests for org-canvas-core API layer  -*- lexical-binding: t; -*-

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
              :to-equal "https://test.canvas.example.com/api/v1/courses/99999/"))))

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
                  ((symbol-function 'elog-error)
                   (lambda (&rest _args)
                     (setq error-logged t)
                     nil)))
          (org-canvas-test-connection)
          (expect error-logged :to-be t))))))

(describe "org-canvas-api-request body logging"
  (it "logs request body when org-canvas-log-request-bodies is t"
    (let ((logged-messages nil))
      (with-org-canvas-test-config
        (let ((org-canvas-log-request-bodies t))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest _args) '((id . 1))))
                    ((symbol-function 'elog-debug)
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
                    ((symbol-function 'elog-debug)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged-messages))))
            (org-canvas-api-request 'GET "https://example.com/api")
            (expect (cl-some (lambda (m) (string-match-p "Response Body:" m)) logged-messages)
                    :to-be-truthy)))))))

(describe "org-canvas-api-request plz-error handling"
  (it "extracts HTTP status and body from plz-error with response"
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
           (expect (caddr err) :to-equal "Validation failed"))))))

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
          (expect (assoc "page" received-params) :to-be-truthy))))))

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

(describe "org-canvas--associate-rubric failure message"
  (it "shows warning in echo area on failure"
    (with-org-canvas-test-config
      (spy-on 'message)
      (spy-on 'elog-warning)
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

(provide 'org-canvas-core-api-test)
;;; org-canvas-core-api-test.el ends here
