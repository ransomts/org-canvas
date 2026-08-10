;;; org-canvas-core-api.el --- Canvas API communication layer -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
SUFFIX is the path after /courses/:id/.  ARGS are format arguments.
A trailing slash on `org-canvas-base-url' is trimmed so the joined
URL never contains a double slash."
  (let ((base (replace-regexp-in-string "/+\\'" "" org-canvas-base-url)))
    (format "%s/api/v1/courses/%s/%s"
	    base
	    org-canvas-course-id
	    (apply #'format suffix args))))

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
    (org-canvas--signal 'org-canvas-credentials-error
      "API token not configured.  Set org-canvas-api-token in org-canvas-credentials.el\nRun M-x org-canvas-init for guided setup"))
  (when (or (null org-canvas-course-id) (string-empty-p org-canvas-course-id))
    (org-canvas--signal 'org-canvas-credentials-error
      "Course ID not configured.  Set org-canvas-course-id in org-canvas-credentials.el\nRun M-x org-canvas-init for guided setup")))

(defconst org-canvas--api-transient-curl-errors '(7 28 56)
  "Curl error codes treated as transient (connect, timeout, recv).")

(defconst org-canvas--api-transient-http-statuses '(502 503 504)
  "HTTP status codes treated as transient (gateway/service errors).")

(defun org-canvas--api-transient-error-p (plz-err)
  "Return non-nil when PLZ-ERR represents a transient failure."
  (let* ((curl-err (and (plz-error-p plz-err) (plz-error-curl-error plz-err)))
         (response (and (plz-error-p plz-err) (plz-error-response plz-err)))
         (status (and response (plz-response-status response))))
    (or (and curl-err
             (memq (car curl-err) org-canvas--api-transient-curl-errors))
        (and status
             (memq status org-canvas--api-transient-http-statuses)))))

(defun org-canvas--scrub-plz-error (plz-err)
  "Return a copy of PLZ-ERR with sensitive response headers masked.
PLZ-ERR structs embed the full `plz-response', whose headers include
live session cookies (set-cookie: canvas_session=...).  Scrubbing here
keeps them out of signal data and any `%S'/`error-message-string'
output.  Non-struct or response-less values pass through unchanged."
  (if (and (plz-error-p plz-err) (plz-error-response plz-err))
      (let ((clean (copy-plz-error plz-err))
            (clean-resp (copy-plz-response (plz-error-response plz-err))))
        (setf (plz-response-headers clean-resp)
              (org-canvas--mask-headers (plz-response-headers clean-resp)))
        (setf (plz-error-response clean) clean-resp)
        clean)
    plz-err))

(defun org-canvas--api-handle-plz-error (err full-url)
  "Handle a plz-error ERR from a request to FULL-URL.
Return `:retry' for rate-limited (sleep already done),
`:retry-transient' for transient errors (caller must sleep),
or signal an error for terminal failures."
  (let* ((plz-err (org-canvas--scrub-plz-error (cdr err)))
         (response (and (plz-error-p plz-err)
                        (plz-error-response plz-err)))
         (status (and response (plz-response-status response)))
         (body (and response (plz-response-body response)))
         (err-msg (if status
                      (format "API Request Failed (HTTP %s)" status)
                    (format "API Request Failed: %S" plz-err))))
    (org-canvas--log-debug org-canvas--logger "[API] <<< RESPONSE: %s" (or status "error"))
    (cond
     ;; Rate limited (429 or 403 with rate limit indication)
     ((and status (or (= status 429)
                      (and (= status 403)
                           body (string-match-p "rate" (format "%s" body)))))
      (org-canvas--log-warning org-canvas--logger
        "[API] Rate limited (HTTP %d). Waiting %ds..."
        status org-canvas-rate-limit-wait)
      (dotimes (i org-canvas-rate-limit-wait)
        (message "Rate limited (HTTP %d). Retrying in %ds..."
                 status (- org-canvas-rate-limit-wait i))
        (sleep-for 1))
      :retry)

     ;; Authentication failure (401)
     ((and status (= status 401))
      (signal 'org-canvas-credentials-error
        (list "Authentication failed (HTTP 401). Your API token may have expired.\nGenerate a new one at Canvas > Account > Settings > Approved Integrations."
              body plz-err)))

     ;; Forbidden (403, non-rate-limit)
     ((and status (= status 403))
      (signal 'org-canvas-credentials-error
        (list "Permission denied (HTTP 403). Your token may lack the required scope for this operation."
              body plz-err)))

     ;; Transient (curl timeout / 5xx); caller will sleep based on retry index
     ((org-canvas--api-transient-error-p plz-err)
      :retry-transient)

     ;; Generic error
     (t
      (org-canvas--log-error org-canvas--logger "%s\n  URL: %s\n  Body: %S"
        err-msg full-url body)
      (signal 'org-canvas-api-error (list err-msg body plz-err))))))

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
  (let* ((plz-err (org-canvas--scrub-plz-error (cdr err)))
         (response (and (plz-error-p plz-err)
                        (plz-error-response plz-err)))
         (body (and response (plz-response-body response))))
    (signal 'org-canvas-api-error
            (list (format "Rate limited after %d retries" retry-count)
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
    (org-canvas--log-debug org-canvas--logger "[API] >>> REQUEST: %s %s" method full-url)
    (org-canvas--log-debug org-canvas--logger "[API] Timeout: %ds | Headers: %S"
      timeout (org-canvas--mask-token headers))
    (when params
      (org-canvas--log-debug org-canvas--logger "[API] Params: %S" params))
    (when (and json-payload org-canvas-log-request-bodies)
      (org-canvas--log-debug org-canvas--logger "[API] Body:\n%s"
        (org-canvas--pretty-json json-payload)))
    (org-canvas--log-debug org-canvas--logger "[API] curl:\n%s"
      (org-canvas--build-curl-command method full-url
                                      (when org-canvas-log-request-bodies json-payload)))))

(defun org-canvas--api-log-response (result)
  "Log debug info for an API response RESULT."
  (org-canvas--log-debug org-canvas--logger "[API] <<< RESPONSE: success")
  (when (and result org-canvas-log-request-bodies)
    (org-canvas--log-debug org-canvas--logger "[API] Response Body:\n%s"
      (org-canvas--pretty-json result))))

(defun org-canvas--api-handle-rate-retry (err rate-retry-count)
  "Advance the rate-limit retry counter or signal exhaustion.
ERR is the original plz-error condition.  RATE-RETRY-COUNT is the
current count (pre-increment).  Returns the new count, or signals
`org-canvas-api-error' once `org-canvas-rate-limit-retries' is reached."
  (if (< rate-retry-count org-canvas-rate-limit-retries)
      (progn
        (org-canvas--log-debug org-canvas--logger
          "[API] Rate retry %d/%d"
          (1+ rate-retry-count) org-canvas-rate-limit-retries)
        (1+ rate-retry-count))
    (org-canvas--api-retries-exhausted rate-retry-count err)))

(defun org-canvas--api-handle-transient-retry (err transient-retry-index)
  "Sleep and advance the transient retry index, or signal exhaustion.
ERR is the original plz-error condition.  TRANSIENT-RETRY-INDEX is the
0-based position into `org-canvas-transient-retry-delays' for the
upcoming retry.  Returns the new index, or signals `org-canvas-api-error'
once the delay list is exhausted."
  (let ((delay (nth transient-retry-index org-canvas-transient-retry-delays)))
    (if delay
        (progn
          (org-canvas--log-warning org-canvas--logger
            "[API] Transient error, retrying in %ds (%d/%d)"
            delay (1+ transient-retry-index)
            (length org-canvas-transient-retry-delays))
          (sleep-for delay)
          (1+ transient-retry-index))
      (let* ((plz-err (org-canvas--scrub-plz-error (cdr err)))
             (response (and (plz-error-p plz-err) (plz-error-response plz-err)))
             (body (and response (plz-response-body response))))
        (signal 'org-canvas-api-error
                (list (format "Transient error after %d retries"
                              (length org-canvas-transient-retry-delays))
                      body plz-err))))))

(defun org-canvas--api-execute-with-retry (plz-method full-url headers json-payload actual-timeout)
  "Execute PLZ-METHOD request to FULL-URL with retry on rate-limit or transient.
HEADERS, JSON-PAYLOAD, and ACTUAL-TIMEOUT configure the request."
  (let ((rate-retry-count 0)
        (transient-retry-index 0)
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
         (pcase (org-canvas--api-handle-plz-error err full-url)
           (:retry
            (setq rate-retry-count
                  (org-canvas--api-handle-rate-retry err rate-retry-count)))
           (:retry-transient
            (setq transient-retry-index
                  (org-canvas--api-handle-transient-retry err transient-retry-index)))))))
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
      (message "Fetching page %d (%d items so far)..." page (length all-items))
      (let* ((page-params (append (or params '())
                                  `(("per_page" . ,(number-to-string per-page))
                                    ("page" . ,(number-to-string page)))))
             (page-items (append (org-canvas-api-request method url :params page-params) nil)))
        (dolist (item page-items)
          (push item all-items))
        (if (< (length page-items) per-page)
            (setq done t)
          (setq page (1+ page)))))
    (nreverse all-items)))

;;;; 3b. Rubric Association

(defun org-canvas--associate-rubric (item-id rubric-id association-type)
  "Associate RUBRIC-ID with ITEM-ID on Canvas.
ASSOCIATION-TYPE is \"Assignment\" or \"Discussion\"."
  (org-canvas--log-info org-canvas--logger "[Rubric] Associating rubric %s with %s %s"
             rubric-id (downcase association-type) item-id)
  (let* ((endpoint (org-canvas-api-course-endpoint "rubric_associations"))
         (payload (make-hash-table :test 'equal))
         (assoc (make-hash-table :test 'equal)))
    (puthash "rubric_id" (string-to-number rubric-id) assoc)
    (puthash "association_id" item-id assoc)
    (puthash "association_type" association-type assoc)
    (puthash "purpose" "grading" assoc)
    (puthash "rubric_association" assoc payload)
    (condition-case err
        (progn
          (org-canvas-api-request 'POST endpoint :data payload)
          (org-canvas--log-info org-canvas--logger "[Rubric] Association created"))
      (error
       (org-canvas--log-warning org-canvas--logger "[Rubric] Association failed: %s" (error-message-string err))
       (message "WARNING: Rubric association failed for %s: %s" item-id (error-message-string err))))))

;;;; 3c. File Upload Infrastructure
;;
;; Self-contained 3-step Canvas file upload, independent of the
;; files.el module so any feature module can upload files.

(defconst org-canvas--mime-type-alist
  '(;; Documents
    ("pdf" . "application/pdf")
    ("doc" . "application/msword") ("docx" . "application/msword")
    ("xls" . "application/vnd.ms-excel") ("xlsx" . "application/vnd.ms-excel")
    ("ppt" . "application/vnd.ms-powerpoint") ("pptx" . "application/vnd.ms-powerpoint")
    ;; Code
    ("py" . "text/x-python") ("python" . "text/x-python")
    ("js" . "application/javascript") ("javascript" . "application/javascript")
    ("html" . "text/html") ("htm" . "text/html")
    ("css" . "text/css") ("json" . "application/json") ("xml" . "application/xml")
    ("txt" . "text/plain") ("text" . "text/plain")
    ("md" . "text/markdown") ("markdown" . "text/markdown")
    ("csv" . "text/csv")
    ;; Images
    ("png" . "image/png")
    ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("svg" . "image/svg+xml")
    ("webp" . "image/webp") ("bmp" . "image/bmp")
    ;; Archives
    ("zip" . "application/zip")
    ("gz" . "application/gzip") ("gzip" . "application/gzip")
    ("tar" . "application/x-tar"))
  "Alist mapping file extensions to MIME content types.")

(defun org-canvas--guess-content-type (filename)
  "Guess MIME type for FILENAME based on extension."
  (let ((ext (downcase (or (file-name-extension filename) ""))))
    (or (alist-get ext org-canvas--mime-type-alist nil nil #'equal)
        "application/octet-stream")))

(defun org-canvas--upload-build-multipart (upload-params local-path boundary)
  "Build multipart/form-data body for file upload.
UPLOAD-PARAMS is the alist from Canvas step 1 response.
LOCAL-PATH is the local file path.
BOUNDARY is the multipart boundary string.
Returns a unibyte string."
  (let ((body-parts nil)
        (file-content (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally local-path)
                        (buffer-string)))
        (actual-filename (file-name-nondirectory local-path))
        (actual-content-type (org-canvas--guess-content-type local-path)))
    ;; Build form fields from upload_params
    (dolist (param (append upload-params nil))
      (let* ((key (car param))
             (raw-value (cdr param))
             ;; Fix Canvas nulls/unknowns
             (value (cond
                     ((and (eq key 'filename) (null raw-value))
                      actual-filename)
                     ((and (eq key 'content_type)
                           (or (null raw-value)
                               (equal raw-value "unknown/unknown")))
                      actual-content-type)
                     (t raw-value))))
        (when value
          (push (format "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s"
                        boundary key value)
                body-parts))))
    ;; File parameter must be LAST
    (push (format "--%s\r\nContent-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\nContent-Type: %s\r\n\r\n"
                  boundary actual-filename actual-content-type)
          body-parts)
    ;; Encode form parts as unibyte before concatenating with binary
    (let* ((body-prefix (encode-coding-string
                         (concat (mapconcat #'identity (nreverse body-parts) "\r\n") "\r\n")
                         'raw-text))
           (body-suffix (encode-coding-string
                         (format "\r\n--%s--\r\n" boundary)
                         'raw-text)))
      (concat body-prefix file-content body-suffix))))

(defun org-canvas--upload-parse-step2-response (buf)
  "Parse the step 2 upload response from BUF.
Returns an alist with either an \\='id key (direct completion) or
a \\='location key (needs step 3 confirmation).  Kills BUF when done."
  (unwind-protect
      (with-current-buffer buf
        (goto-char (point-min))
        (let (location-header json-response)
          (save-excursion
            (when (re-search-forward "^[Ll]ocation: \\(.*\\)\r?$" nil t)
              (setq location-header (string-trim (match-string 1)))))
          (when (re-search-forward "\r?\n\r?\n" nil t)
            (setq json-response
                  (condition-case err
                      (json-read-from-string
                       (buffer-substring-no-properties (point) (point-max)))
                    (error
                     (org-canvas--log-debug org-canvas--logger
                       "[Upload] Step 2 response body was not valid JSON (%s); falling back to Location header"
                       (error-message-string err))
                     nil))))
          (cond
           ((and json-response (alist-get 'id json-response))
            json-response)
           (location-header
            `((location . ,location-header)))
           (json-response json-response)
           (t (org-canvas--signal 'org-canvas-api-error
                "Upload failed: no JSON or Location header")))))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun org-canvas--upload-confirm (step2-response)
  "Confirm a Canvas upload given STEP2-RESPONSE from step 2.
If STEP2-RESPONSE already contains an \\='id, returns it directly.
Otherwise follows the \\='location header for step 3 confirmation."
  (org-canvas--log-info org-canvas--logger "[Upload Step 3] Confirming upload...")
  (if (alist-get 'id step2-response)
      (progn
        (org-canvas--log-info org-canvas--logger "[Upload] Complete: file ID %s"
                   (alist-get 'id step2-response))
        step2-response)
    (let* ((location (alist-get 'location step2-response))
           (full-url (if (string-prefix-p "http" location)
                         location
                       (concat org-canvas-base-url location)))
           (response (org-canvas-api-request 'GET full-url)))
      (org-canvas--log-info org-canvas--logger "[Upload] Complete: file ID %s"
                 (alist-get 'id response))
      response)))

(defun org-canvas--upload-file (local-path &optional notify-url display-name)
  "Upload LOCAL-PATH to Canvas via the 3-step upload API.
NOTIFY-URL is the step 1 endpoint (defaults to course files).
DISPLAY-NAME overrides the filename shown in Canvas.
Returns the Canvas file object alist (with \\='id key)."
  (let* ((filename (or display-name (file-name-nondirectory local-path)))
         (size (file-attribute-size (file-attributes local-path)))
         (content-type (org-canvas--guess-content-type local-path))
         (url (or notify-url
                  (format "%s/api/v1/courses/%s/files"
                          org-canvas-base-url org-canvas-course-id)))
         (payload `((name . ,filename)
                    (size . ,size)
                    (content_type . ,content-type))))
    (org-canvas--log-info org-canvas--logger "[Upload Step 1] Notifying Canvas for '%s'..." filename)
    ;; Step 1: Notify Canvas
    (let ((upload-info (org-canvas-api-request 'POST url :data payload)))
      (let* ((upload-url (alist-get 'upload_url upload-info))
             (upload-params (alist-get 'upload_params upload-info))
             (boundary (format "----FormBoundary%s"
                               (md5 (format "%s%s" (current-time) (random))))))
        (unless upload-url
          (org-canvas--signal 'org-canvas-api-error
            "Canvas API returned no upload_url in step 1 response: %S" upload-info))
        (org-canvas--log-info org-canvas--logger "[Upload Step 2] Sending file to %s..." upload-url)
        ;; Step 2: Upload the file
        (let* ((full-body (org-canvas--upload-build-multipart
                           upload-params local-path boundary))
               (url-request-method "POST")
               (url-request-extra-headers
                `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
               (url-request-data full-body)
               (step2-buf (url-retrieve-synchronously
                           upload-url nil nil org-canvas-upload-timeout))
               (step2-response (org-canvas--upload-parse-step2-response step2-buf)))
          (org-canvas--upload-confirm step2-response))))))

(provide 'org-canvas-core-api)
;;; org-canvas-core-api.el ends here
