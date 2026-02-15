;;; org-canvas-core-api.el --- Canvas API communication layer -*- lexical-binding: t; -*-

;;; Commentary:

;; HTTP communication with the Canvas REST API via plz.
;; Handles JSON encoding/decoding, rate-limit retry, timeout,
;; curl command generation for debugging, and pagination.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'plz)
(require 'url-util)
(require 'org-canvas-core-config)

;;;; 3. API Layer
;;
;; The API layer handles all HTTP communication with Canvas.
;; Key features:
;;   - Automatic JSON encoding/decoding
;;   - Request timeout handling (configurable via org-canvas-request-timeout)
;;   - Debug logging with curl command generation for troubleshooting
;;   - Error normalization (plz-error -> standard error signal)

(defun org-canvas-api-course-endpoint (suffix &rest args)
  "Construct a course-specific endpoint URL.
SUFFIX is the path after /courses/:id/.  ARGS are format arguments."
  (format "%s/api/v1/courses/%s/%s"
	  org-canvas-base-url
	  org-canvas-course-id
	  (apply #'format suffix args)))

(defun org-canvas--build-curl-command (method full-url json-payload)
  "Build a curl command string for debugging.
METHOD is the HTTP method, FULL-URL is the complete URL with query
params, JSON-PAYLOAD is the JSON body (or nil).  Uses $CANVAS_TOKEN
placeholder for the API token (requires double quotes for expansion)."
  (let ((parts (list "curl")))
    ;; Method (skip for GET as it's default)
    (unless (eq method 'GET)
      (push (format "-X %s" method) parts))
    ;; Headers - use double quotes so $CANVAS_TOKEN expands
    (push "-H \"Authorization: Bearer $CANVAS_TOKEN\"" parts)
    (push "-H \"Content-Type: application/json\"" parts)
    ;; Data payload - escape double quotes in JSON
    (when json-payload
      (push (format "-d '%s'" json-payload) parts))
    ;; URL (double quoted in case of special chars)
    (push (format "\"%s\"" full-url) parts)
    ;; Join in reverse order (we pushed, so it's backwards)
    (mapconcat #'identity (nreverse parts) " \\\n     ")))

(defun org-canvas--ensure-credentials ()
  "Signal error if API token or course ID are not configured."
  (when (or (null org-canvas-api-token) (string-empty-p org-canvas-api-token))
    (error "API token not configured.  Set org-canvas-api-token in org-canvas-credentials.el"))
  (when (or (null org-canvas-course-id) (string-empty-p org-canvas-course-id))
    (error "Course ID not configured.  Set org-canvas-course-id in org-canvas-credentials.el")))

(defun org-canvas--api-handle-plz-error (err full-url)
  "Handle a plz-error ERR from a request to FULL-URL.
Return `:retry' if the request should be retried (after sleeping),
or signal an error for terminal failures."
  (let* ((plz-err (cdr err))
         (response (and (plz-error-p plz-err)
                        (plz-error-response plz-err)))
         (status (and response (plz-response-status response)))
         (body (and response (plz-response-body response)))
         (err-msg (if status
                      (format "API Request Failed (HTTP %s)" status)
                    (format "API Request Failed: %S" plz-err))))
    (elog-debug org-canvas--logger "[API] <<< RESPONSE: %s" (or status "error"))
    (cond
     ;; Rate limited (429 or 403 with rate limit indication)
     ((and status (or (= status 429)
                      (and (= status 403)
                           body (string-match-p "rate" (format "%s" body)))))
      (elog-warning org-canvas--logger
        "[API] Rate limited (HTTP %d). Waiting %ds..."
        status org-canvas-rate-limit-wait)
      (sleep-for org-canvas-rate-limit-wait)
      :retry)

     ;; Authentication failure (401)
     ((and status (= status 401))
      (signal 'error
        (list "Authentication failed (HTTP 401). Your API token may have expired.\nGenerate a new one at Canvas > Account > Settings > Approved Integrations."
              body plz-err)))

     ;; Forbidden (403, non-rate-limit)
     ((and status (= status 403))
      (signal 'error
        (list "Permission denied (HTTP 403). Your token may lack the required scope for this operation."
              body plz-err)))

     ;; Generic error
     (t
      (elog-error org-canvas--logger "%s\n  URL: %s\n  Body: %S"
        err-msg full-url body)
      (signal 'error (list err-msg body plz-err))))))

(defun org-canvas--api-build-query-string (params)
  "Build a URL query string from PARAMS alist.
Returns a string like \"?key=value&...\" or \"\" if PARAMS is nil."
  (if params
      (concat "?" (url-build-query-string
                   (cl-loop for (k . v) in params
                            collect (list (format "%s" k)
                                          (format "%s" v)))))
    ""))

(defun org-canvas--api-retries-exhausted (retry-count err)
  "Signal an error after RETRY-COUNT rate-limit retries.
ERR is the last plz-error condition."
  (let* ((plz-err (cdr err))
         (response (and (plz-error-p plz-err)
                        (plz-error-response plz-err)))
         (body (and response (plz-response-body response))))
    (signal 'error (list (format "Rate limited after %d retries" retry-count)
                         body plz-err))))

(defun org-canvas--api-log-request (request)
  "Log debug info for an API REQUEST plist.
REQUEST has keys :method :url :params :body :timeout :headers."
  (let ((method (plist-get request :method))
        (full-url (plist-get request :url))
        (params (plist-get request :params))
        (json-payload (plist-get request :body))
        (timeout (plist-get request :timeout))
        (headers (plist-get request :headers)))
    (elog-debug org-canvas--logger "[API] >>> REQUEST: %s %s" method full-url)
    (elog-debug org-canvas--logger "[API] Timeout: %ds | Headers: %S"
      timeout (org-canvas--mask-token headers))
    (when params
      (elog-debug org-canvas--logger "[API] Params: %S" params))
    (when (and json-payload org-canvas-log-request-bodies)
      (elog-debug org-canvas--logger "[API] Body:\n%s"
        (org-canvas--pretty-json json-payload)))
    (elog-debug org-canvas--logger "[API] curl:\n%s"
      (org-canvas--build-curl-command method full-url
                                      (when org-canvas-log-request-bodies json-payload)))))

(defun org-canvas--api-log-response (result)
  "Log debug info for an API response RESULT."
  (elog-debug org-canvas--logger "[API] <<< RESPONSE: success")
  (when (and result org-canvas-log-request-bodies)
    (elog-debug org-canvas--logger "[API] Response Body:\n%s"
      (org-canvas--pretty-json result))))

(defun org-canvas--api-execute-with-retry (plz-method full-url headers json-payload actual-timeout)
  "Execute PLZ-METHOD request to FULL-URL with retry on rate-limit.
HEADERS, JSON-PAYLOAD, and ACTUAL-TIMEOUT configure the request."
  (let ((retry-count 0)
        (done nil)
        result)
    (while (not done)
      (condition-case err
          (progn
            (setq result
                  (plz plz-method full-url
                    :headers headers
                    :body json-payload
                    :as #'json-read
                    :timeout actual-timeout))
            (org-canvas--api-log-response result)
            (setq done t))
        (plz-error
         (when (eq (org-canvas--api-handle-plz-error err full-url) :retry)
           (if (< retry-count org-canvas-rate-limit-retries)
               (progn
                 (elog-debug org-canvas--logger
                   "[API] Retry %d/%d" (1+ retry-count) org-canvas-rate-limit-retries)
                 (setq retry-count (1+ retry-count)))
             (setq done t)
             (org-canvas--api-retries-exhausted retry-count err))))))
    result))

(cl-defun org-canvas-api-request (method url &key params data timeout)
  "Perform an HTTP request to the Canvas API synchronously using `plz'.
METHOD is \\='GET, \\='POST, \\='PUT, or \\='DELETE.
URL is the full endpoint.
PARAMS is an alist of query parameters.
DATA is an alist or hash-table to be sent as JSON body (for POST/PUT).
TIMEOUT is the request timeout in seconds."
  (org-canvas--ensure-credentials)
  (let* ((full-url (concat url (org-canvas--api-build-query-string params)))
	 (json-payload (when data
			 (if (stringp data) data (json-encode data))))
	 (headers `(("Authorization" . ,(concat "Bearer " org-canvas-api-token))
		    ("Content-Type" . "application/json")))
	 (actual-timeout (or timeout org-canvas-request-timeout))
	 ;; IMPORTANT: plz requires lowercase method symbols ('post not 'POST)
         ;; Our codebase uses uppercase by convention, so convert here
	 (plz-method (intern (downcase (symbol-name method)))))

    (org-canvas--api-log-request
     (list :method method :url full-url :params params
           :body json-payload :timeout actual-timeout :headers headers))
    (org-canvas--api-execute-with-retry plz-method full-url headers json-payload actual-timeout)))

(defun org-canvas-api-request-all-pages (method url &optional params)
  "Fetch all pages of results from a paginated Canvas API endpoint.
METHOD is the HTTP method (usually \\='GET).
URL is the full endpoint URL.
PARAMS is an alist of additional query parameters.
Automatically adds per_page=100 and loops until a page returns
fewer results than per_page.
Returns a flat list of all items across all pages."
  (let ((page 1)
        (per-page 100)
        (all-items nil)
        (done nil))
    (while (not done)
      (let* ((page-params (append (or params '())
                                  `(("per_page" . ,(number-to-string per-page))
                                    ("page" . ,(number-to-string page)))))
             (page-items (append (org-canvas-api-request method url :params page-params) nil)))
        (setq all-items (append all-items page-items))
        (if (< (length page-items) per-page)
            (setq done t)
          (setq page (1+ page)))))
    all-items))

(provide 'org-canvas-core-api)
;;; org-canvas-core-api.el ends here
