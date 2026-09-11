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
(require 'auth-source)
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


;;;; Feature URL Resolution
;;
;; Every consumer of the feature registry — the sync's remote snapshot,
;; the drift report, the orphan scan, pull-at-point — used to spell its
;; own `org-canvas-api-course-endpoint' call, which quietly assumed that
;; every feature lists under the course.  Calendar events do not: they
;; live at the global /api/v1/calendar_events, filtered by context code,
;; and could not be registered without every consumer 404ing (issue
;; #87).  These resolvers are the one place that assumption is made, and
;; the registry's `:list-url-fn' / `:item-url-fn' the one place a feature
;; overrides it — the escape hatches `org-canvas-define-delete-all' has
;; always had.

(defun org-canvas--feature-list-url (feature)
  "Return the URL that lists FEATURE's items.
FEATURE is a feature registry entry.  Its `:list-url-fn' wins when
present; otherwise the course-scoped `:endpoint'."
  (let ((fn (plist-get feature :list-url-fn)))
    (if fn
        (funcall fn)
      (org-canvas-api-course-endpoint (plist-get feature :endpoint)))))

(defun org-canvas--feature-item-url (feature id)
  "Return the URL of FEATURE's item ID.
FEATURE is a feature registry entry.  Its `:item-url-fn' wins when
present; otherwise ID is appended to the course-scoped `:endpoint'."
  (let ((fn (plist-get feature :item-url-fn)))
    (if fn
        (funcall fn id)
      (org-canvas-api-course-endpoint
       (format "%s/%%s" (plist-get feature :endpoint)) id))))

(defun org-canvas--feature-list-params (feature)
  "Return the query parameters that list FEATURE's items, or nil.
A `:list-params' value may be a function of no arguments, called each
time: calendar events filter by a context code that embeds
`org-canvas-course-id', which is not known when the module loads."
  (let ((params (plist-get feature :list-params)))
    (if (functionp params) (funcall params) params)))

(defun org-canvas--feature-modified-field (feature)
  "Return the field that tracks FEATURE's remote content modification.
`:modified-field' when the feature declares one, otherwise
`updated_at'.  Files declare `modified_at': Canvas bumps a file's
`updated_at' on metadata-only touches — a lock, a usage-rights edit, a
module-item relink — none of which change what a student downloads, so
drift decided from it re-flagged unchanged files forever (issue #94)."
  (or (plist-get feature :modified-field) 'updated_at))

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

;;;; Token Resolution
;;
;; Every read of the API token goes through `org-canvas--api-token'
;; (issue #138).  `org-canvas-api-token' is what the per-course
;; credentials file and `org-canvas-init' set, and it still wins when
;; it is non-empty.  Otherwise the token comes from `auth-source', so
;; it can live in ~/.authinfo.gpg — encrypted, outside the course
;; directory, and not inside a file that `load' evaluates — as
;;
;;   machine HOST login COURSE-ID password TOKEN
;;
;; where HOST is `org-canvas-base-url' with its scheme stripped.  An
;; entry whose login is the course id is preferred; failing that, any
;; entry for the host serves every course there.

(defun org-canvas--nonempty-string-p (value)
  "Return non-nil when VALUE is a string with at least one character."
  (and (stringp value) (not (string-empty-p value))))

(defvar org-canvas--api-token-cache nil
  "Cache of the auth-source token as ((HOST . COURSE-ID) . TOKEN), or nil.
Only a hit is kept, so an entry added mid-session is found on the next
request.  `org-canvas--api-token-forget' drops it, as does
`org-canvas--api-token-cache-watcher' whenever `org-canvas-base-url'
or `org-canvas-course-id' changes; the key guards against a change
neither saw.")

(defun org-canvas--api-token-forget ()
  "Drop the cached auth-source token so the next request resolves it again.
`org-canvas-activate-course' calls this after loading a course: that
is how a token rotated in ~/.authinfo.gpg for the same host and course
takes effect without restarting Emacs."
  (setq org-canvas--api-token-cache nil))

(defun org-canvas--api-token-cache-watcher (_symbol _newval _operation _where)
  "Drop the cached token after a change of host or course.
Installed on `org-canvas-base-url' and `org-canvas-course-id'.  Unlike
`org-canvas--directory-watcher' it reacts to every operation, not only
`set': a `let' binding of either variable changes which token applies
for as long as it lasts, and forgetting a cache costs nothing."
  (setq org-canvas--api-token-cache nil))

(add-variable-watcher 'org-canvas-base-url #'org-canvas--api-token-cache-watcher)
(add-variable-watcher 'org-canvas-course-id #'org-canvas--api-token-cache-watcher)

(defun org-canvas--api-host ()
  "Return the host `org-canvas-base-url' names, or nil when it names none.
The scheme, any port and any path are stripped: the result is the
`machine' an authinfo entry for the instance names."
  (when (stringp org-canvas-base-url)
    (let ((host (car (split-string
                      (replace-regexp-in-string
                       "\\`[A-Za-z][A-Za-z0-9+.-]*://" "" org-canvas-base-url)
                      "[/:]"))))
      (unless (string-empty-p host) host))))

(defun org-canvas--api-token-from-auth-source (host course-id)
  "Return the auth-source secret for HOST, or nil when it has none.
An entry whose login is COURSE-ID is tried first, then any entry for
HOST at all.  A secret auth-source hands back as a function (it does
so for encrypted backends) is called, so the result is always the
token string."
  (cl-some
   (lambda (user)
     (let* ((entry (car (apply #'auth-source-search
                               :host host :max 1 :require '(:secret)
                               (and user (list :user user)))))
            (secret (plist-get entry :secret)))
       (if (functionp secret) (funcall secret) secret)))
   (if (org-canvas--nonempty-string-p course-id)
       (list course-id nil)
     (list nil))))

(defun org-canvas--api-token ()
  "Return the Canvas API token, or nil when none is configured.
A non-empty `org-canvas-api-token' wins.  Otherwise the token is looked
up in `auth-source' under the host of `org-canvas-base-url', preferring
an entry whose login is `org-canvas-course-id' (see
`org-canvas--api-token-from-auth-source'), and a hit is kept in
`org-canvas--api-token-cache' for that host and course."
  (let* ((host (org-canvas--api-host))
         (key (cons host org-canvas-course-id)))
    (cond
     ((org-canvas--nonempty-string-p org-canvas-api-token)
      org-canvas-api-token)
     ((null host) nil)
     ((equal (car org-canvas--api-token-cache) key)
      (cdr org-canvas--api-token-cache))
     (t
      (let ((token (org-canvas--api-token-from-auth-source
                    host org-canvas-course-id)))
        (when token
          (setq org-canvas--api-token-cache (cons key token)))
        token)))))

(defun org-canvas--api-authinfo-line ()
  "Return the authinfo line naming the token for this host and course.
A host or course id that is not configured is shown as a placeholder.
Spelled once here for the preflight message and the setup wizard."
  (format "machine %s login %s password <token>"
          (or (org-canvas--api-host) "<canvas-host>")
          (if (org-canvas--nonempty-string-p org-canvas-course-id)
              org-canvas-course-id
            "<course-id>")))

(defun org-canvas--ensure-credentials ()
  "Signal an error unless an API token and a course ID are configured.
The token is whatever `org-canvas--api-token' resolves; the message
names both places it may come from, with the authinfo line spelled
out for this host and course."
  (unless (org-canvas--api-token)
    (org-canvas--signal 'org-canvas-credentials-error
      "API token not configured.  Add a line to ~/.authinfo.gpg:\n  %s\nor set org-canvas-api-token in org-canvas-credentials.el\nRun M-x org-canvas-init for guided setup"
      (org-canvas--api-authinfo-line)))
  (unless (org-canvas--nonempty-string-p org-canvas-course-id)
    (org-canvas--signal 'org-canvas-credentials-error
      "Course ID not configured.  Set org-canvas-course-id in org-canvas-credentials.el\nRun M-x org-canvas-init for guided setup")))

(defconst org-canvas--api-supported-methods '(GET HEAD POST PUT DELETE PATCH)
  "HTTP methods `org-canvas-api-request' can send.
plz itself has no PATCH support (an unrecognized method symbol falls
through its curl-argument builder and the request silently degrades to
a bodyless GET), so PATCH is sent through the direct curl fallback
`org-canvas--api-curl-patch' instead.  Anything not in this list is
rejected up front.")

(defun org-canvas--api-request-headers (&optional masked)
  "Return the request headers for a Canvas API call, as an alist.
The Authorization header carries the token `org-canvas--api-token'
resolves.  With MASKED non-nil it carries the log placeholder instead
and the token is never read, which is what request logging wants.

Call this inside the function that hands the request to the
transport, never earlier.  A backtrace prints function arguments
verbatim, so a headers alist threaded down the call chain put the
bearer token on stderr the first time an api-error escaped a batch
script (issue #178)."
  `(("Authorization" . ,(if masked
                            "Bearer ***MASKED***"
                          (concat "Bearer " (org-canvas--api-token))))
    ("Content-Type" . "application/json")))

(defun org-canvas--api-curl-patch-config (full-url timeout body-p)
  "Build a curl config string for a PATCH request to FULL-URL.
TIMEOUT is the max time in seconds.  When BODY-P is non-nil, ends with
a data-binary directive that makes curl read the request body from the
remainder of stdin (the same trick plz uses, so the Authorization
header never appears on the command line).  The headers come from
`org-canvas--api-request-headers', resolved here rather than passed
in, so no caller's frame carries the token (issue #178)."
  (concat
   "request = \"PATCH\"\n"
   (mapconcat (lambda (h) (format "header = \"%s: %s\"" (car h) (cdr h)))
              (org-canvas--api-request-headers) "\n")
   "\n"
   (format "url = \"%s\"\n" full-url)
   (format "max-time = %d\n" timeout)
   "silent\n"
   "show-error\n"
   "write-out = \"\\n%{http_code}\"\n"
   ;; Must be the last directive: curl reads the rest of stdin as the body
   (when body-p "data-binary = \"@-\"\n")))

(defun org-canvas--api-curl-patch-parse (exit-code output)
  "Parse curl PATCH OUTPUT given EXIT-CODE, mimicking plz's contract.
Returns the parsed JSON body on 2xx (nil when empty).  Signals
`plz-error' with a curl-error struct on transport failure or with a
response struct on HTTP error status, so the shared retry and error
machinery treats the fallback exactly like a plz request."
  (unless (eq exit-code 0)
    (signal 'plz-error
            (make-plz-error :curl-error
                            (cons (if (numberp exit-code) exit-code 2)
                                  (format "curl failed: %s" output)))))
  ;; write-out appended "\n%{http_code}" — split it off the body
  (unless (string-match "\\(?:\\`\\|\n\\)\\([0-9]\\{3\\}\\)[ \t\n]*\\'" output)
    (signal 'plz-error
            (make-plz-error :message (format "No HTTP status in curl output: %s"
                                             output))))
  (let* ((status (string-to-number (match-string 1 output)))
         (body (string-trim (substring output 0 (match-beginning 0)))))
    (if (and (>= status 200) (< status 300))
        (unless (string-empty-p body)
          (json-read-from-string body))
      (signal 'plz-error
              (make-plz-error :response (make-plz-response :status status
                                                           :body body))))))

(defun org-canvas--api-curl-patch (full-url json-payload timeout)
  "Send a PATCH request to FULL-URL via curl directly.
plz cannot send PATCH, so this fallback mirrors its behavior: the
headers and the method go in a curl config read from stdin (keeping
the token off the command line), JSON-PAYLOAD follows as the body,
TIMEOUT caps the request.  Returns parsed JSON on success; signals
`plz-error' structs on failure so shared error handling applies.

The config is written from a buffer rather than handed to
`write-region' as a string, so the token is an argument to nothing
that can fail (issue #178)."
  (let ((stdin-file (make-temp-file "org-canvas-patch-")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert (org-canvas--api-curl-patch-config
                     full-url timeout json-payload))
            (when json-payload (insert json-payload))
            (let ((coding-system-for-write 'utf-8))
              (write-region nil nil stdin-file nil 'silent)))
          (with-temp-buffer
            (let ((exit-code (call-process plz-curl-program stdin-file t nil
                                           "--config" "-")))
              (org-canvas--api-curl-patch-parse exit-code (buffer-string)))))
      (delete-file stdin-file))))

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

(defun org-canvas--api-unwrap-plz-error (datum)
  "Return the `plz-error' struct DATUM carries, whatever shape it arrived in.
A `condition-case' on `plz-error' catches errors signalled three ways.
Our own curl fallback signals the struct as the datum.  plz itself
signals `plz-http-error' and `plz-curl-error', whose datum is a list
of a label and the struct, and a handful of its parse failures carry
no struct at all, only strings.

Reading `(cdr err)' as a struct is therefore wrong for every real HTTP
error plz raises: `plz-error-p' fails, the response inside is never
found, and with it go the status, the body and the cookies that need
masking (issue #152).  A datum holding no struct is returned as it
came, for the caller to describe."
  (cond
   ((plz-error-p datum) datum)
   ((listp datum) (or (cl-find-if #'plz-error-p datum) datum))
   (t datum)))

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

(defun org-canvas--api-error-datum (err)
  "Return the scrubbed `plz-error' struct behind the error condition ERR.
Unwraps plz's list datum (`org-canvas--api-unwrap-plz-error') before
masking it (`org-canvas--scrub-plz-error'), so every reader of an error
gets a struct whose headers are safe to print.  Both steps belong to
every caller: skipping the unwrap loses the response, skipping the
scrub leaks the session cookies in it."
  (org-canvas--scrub-plz-error
   (org-canvas--api-unwrap-plz-error (cdr err))))

(defun org-canvas--api-describe-datum (datum)
  "Return a readable description of a DATUM carrying no `plz-error'.
plz signals a few parse failures with a list of strings and no struct.
Printing that with `%S' gives the user a quoted list; joining the
strings gives them the sentence plz wrote."
  (if (and (listp datum) (cl-every #'stringp datum))
      (mapconcat #'identity datum ": ")
    (format "%S" datum)))

(defun org-canvas--api-collect-error-messages (node)
  "Collect message strings from a Canvas `errors' JSON subtree NODE.
Handles the shapes Canvas uses: per-attribute maps of message objects
\(objects with a `message' key), arrays of message objects, and arrays
of bare strings.  Other string fields (attribute names, error types)
are ignored."
  (cond
   ((stringp node) (list node))
   ((vectorp node)
    (cl-mapcan #'org-canvas--api-collect-error-messages (append node nil)))
   ((and (consp node) (consp (car node)))
    (cl-mapcan (lambda (pair)
                 (cond
                  ((eq (car pair) 'message)
                   (and (stringp (cdr pair)) (list (cdr pair))))
                  ((stringp (cdr pair)) nil)
                  (t (org-canvas--api-collect-error-messages (cdr pair)))))
               node))))

(defun org-canvas--api-error-message (body)
  "Extract Canvas's human-readable error message from BODY, or nil.
BODY is the raw response body string.  Canvas 4xx bodies are JSON with
an `errors' structure and/or a top-level `message'."
  (when (and (stringp body)
             (string-prefix-p "{" (string-trim-left body)))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (json (json-read-from-string body))
               (msgs (append
                      (org-canvas--api-collect-error-messages
                       (alist-get 'errors json))
                      (let ((m (alist-get 'message json)))
                        (and (stringp m) (list m))))))
          (when msgs
            (mapconcat #'identity (delete-dups msgs) "; ")))
      (error nil))))

;;;; Waiting
;;
;; Every pause a sync takes — a rate-limit back-off, a transient retry,
;; the request pacing interval, the folder-creation settle — used to be
;; a bare `sleep-for', which freezes the display for its whole length:
;; no countdown, no redisplay, timers stalled (issue #142).  One helper
;; owns the pause instead.

(defun org-canvas--wait (seconds &optional reason)
  "Pause for SECONDS, keeping the display alive when someone is watching.
REASON, when non-nil, is a short phrase — \"Rate limited (HTTP 429).
Retrying\" — shown in the echo area with the seconds left, so a wait
reads as a countdown rather than a hang.  In batch this is a single
`sleep-for'.  Interactively the wait is taken in one-second slices
through `sit-for', which redisplays and lets timers run; a slice during
which input arrives falls back to `sleep-for', so the wait never spins
and never ends early.  Fractions are fine."
  (cond
   (noninteractive
    (when reason
      (message "%s in %ds..." reason (ceiling seconds)))
    (sleep-for seconds))
   (t
    (let ((left seconds))
      (while (> left 0)
        (let ((slice (min 1 left)))
          (when reason
            (message "%s in %ds..." reason (ceiling left)))
          (unless (sit-for slice)
            (sleep-for slice))
          (setq left (- left slice))))))))

(defun org-canvas--api-handle-plz-error (err full-url)
  "Handle a plz-error ERR from a request to FULL-URL.
Return `:retry' for rate-limited (sleep already done),
`:retry-transient' for transient errors (caller must sleep),
or signal an error for terminal failures.

Signaled errors carry a single concise, human-readable message
\(Canvas's own error text when the body provides one); the full
response detail is logged at DEBUG so `error-message-string' output
in [FAILED] lines stays readable."
  (let* ((plz-err (org-canvas--api-error-datum err))
         (response (and (plz-error-p plz-err)
                        (plz-error-response plz-err)))
         (status (and response (plz-response-status response)))
         (body (and response (plz-response-body response)))
         (curl-err (and (plz-error-p plz-err) (plz-error-curl-error plz-err)))
         (canvas-msg (org-canvas--api-error-message body))
         (err-msg (cond
                   ((and status canvas-msg)
                    (format "%s (HTTP %s)" canvas-msg status))
                   (status (format "API Request Failed (HTTP %s)" status))
                   (curl-err (format "API Request Failed: %s" (cdr curl-err)))
                   (t (format "API Request Failed: %s"
                              (org-canvas--api-describe-datum plz-err))))))
    (org-canvas--log-debug org-canvas--logger "[API] <<< RESPONSE: %s" (or status "error"))
    (cond
     ;; Rate limited (429 or 403 with rate limit indication)
     ((and status (or (= status 429)
                      (and (= status 403)
                           body (string-match-p "rate" (format "%s" body)))))
      (org-canvas--log-warning org-canvas--logger
        "[API] Rate limited (HTTP %d). Waiting %ds..."
        status org-canvas-rate-limit-wait)
      (org-canvas--wait org-canvas-rate-limit-wait
                        (format "Rate limited (HTTP %d). Retrying" status))
      :retry)

     ;; Authentication failure (401)
     ((and status (= status 401))
      (org-canvas--log-debug org-canvas--logger "[API] 401 body: %S" body)
      (signal 'org-canvas-credentials-error
        (list "Authentication failed (HTTP 401). Your API token may have expired.\nGenerate a new one at Canvas > Account > Settings > Approved Integrations.")))

     ;; Forbidden (403, non-rate-limit)
     ((and status (= status 403))
      (org-canvas--log-debug org-canvas--logger "[API] 403 body: %S" body)
      (signal 'org-canvas-permission-error
              (list (format "Permission denied (HTTP 403) reading %s%s.  Your Canvas role, or your token scopes, cannot see it"
                            (or (org-canvas--api-resource-name full-url) "this resource")
                            (if canvas-msg (format ": %s" canvas-msg) "")))))

     ;; Transient (curl timeout / 5xx); caller will sleep based on retry index
     ((org-canvas--api-transient-error-p plz-err)
      :retry-transient)

     ;; Generic error: concise message in the signal, full detail at DEBUG
     (t
      (org-canvas--log-debug org-canvas--logger "%s\n  URL: %s\n  Body: %S"
        err-msg full-url body)
      (signal 'org-canvas-api-error (list err-msg))))))

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
  (let* ((plz-err (org-canvas--api-error-datum err))
         (response (and (plz-error-p plz-err)
                        (plz-error-response plz-err)))
         (body (and response (plz-response-body response))))
    (org-canvas--log-debug org-canvas--logger
      "[API] Rate-limit retries exhausted. Body: %S" body)
    (signal 'org-canvas-api-error
            (list (format "Rate limited after %d retries" retry-count)))))

(defun org-canvas--api-log-request (request)
  "Log debug info for an API REQUEST plist.
REQUEST has keys :method :url :params :body :timeout :headers.  The
caller passes headers already masked (`org-canvas--api-request-headers'
with MASKED); the token itself never enters this function."
  (let ((method (plist-get request :method))
        (full-url (plist-get request :url))
        (params (plist-get request :params))
        (json-payload (plist-get request :body))
        (timeout (plist-get request :timeout))
        (headers (plist-get request :headers)))
    (org-canvas--log-debug org-canvas--logger "[API] >>> REQUEST: %s %s" method full-url)
    (org-canvas--log-debug org-canvas--logger "[API] Timeout: %ds | Headers: %S"
      timeout headers)
    (when params
      (org-canvas--log-debug org-canvas--logger "[API] Params: %S" params))
    (when (and json-payload org-canvas-log-request-bodies)
      (org-canvas--log-debug org-canvas--logger "[API] Body:\n%s"
        (org-canvas--pretty-json json-payload)))
    (org-canvas--log-debug org-canvas--logger "[API] curl:\n%s"
      (org-canvas--build-curl-command method full-url
                                      (when org-canvas-log-request-bodies json-payload)))))

(defun org-canvas--api-log-response (result)
  "Log debug info for an API response RESULT.
A `plz-response' (a request made with AS `response') logs its status;
its body is logged decoded only when bodies are logged at all."
  (org-canvas--log-debug org-canvas--logger "[API] <<< RESPONSE: success%s"
    (if (plz-response-p result)
        (format " (HTTP %s)" (plz-response-status result))
      ""))
  (when (and result org-canvas-log-request-bodies (plz-response-p result))
    (setq result (org-canvas--api-decode-response result)))
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
          (org-canvas--wait delay
                            (format "Transient error, retry %d/%d"
                                    (1+ transient-retry-index)
                                    (length org-canvas-transient-retry-delays)))
          (1+ transient-retry-index))
      (let* ((plz-err (org-canvas--api-error-datum err))
             (response (and (plz-error-p plz-err) (plz-error-response plz-err)))
             (status (and response (plz-response-status response)))
             (body (and response (plz-response-body response)))
             (curl-err (and (plz-error-p plz-err) (plz-error-curl-error plz-err))))
        (org-canvas--log-debug org-canvas--logger
          "[API] Transient retries exhausted. Body: %S" body)
        ;; The curl detail (e.g. "Operation timeout.") must stay in the
        ;; message: org-canvas--timeout-error-p keys off it for recovery.
        (signal 'org-canvas-api-error
                (list (format "Transient error after %d retries: %s"
                              (length org-canvas-transient-retry-delays)
                              (cond (curl-err (cdr curl-err))
                                    (status (format "HTTP %s" status))
                                    (t "unknown error")))))))))

(defun org-canvas--api-execute-request (plz-method full-url json-payload actual-timeout
                                                   &optional as)
  "Send one PLZ-METHOD request to FULL-URL and return the parsed JSON.
Dispatches PATCH to the direct curl fallback (plz cannot send it);
everything else goes through plz.  JSON-PAYLOAD and ACTUAL-TIMEOUT
configure the request.  AS `response' returns the `plz-response'
itself, headers and undecoded body, which the paginator needs for the
Link header; anything else returns the decoded JSON.

This is the transport boundary for the token (issue #178): the
headers are resolved by `org-canvas--api-request-headers' in the call
to plz itself, and any error plz raises is re-signalled from here, so
a backtrace of whatever escapes the retry loop starts at this frame
and never shows plz's own, whose arguments hold the header."
  (if (eq plz-method 'patch)
      (org-canvas--api-curl-patch full-url json-payload actual-timeout)
    (condition-case err
        (plz plz-method full-url
          :headers (org-canvas--api-request-headers)
          :body json-payload
          :as (if (eq as 'response) 'response #'json-read)
          :timeout actual-timeout)
      (error (signal (car err) (cdr err))))))

(defun org-canvas--api-execute-with-retry (plz-method full-url json-payload actual-timeout
                                                      &optional as)
  "Execute PLZ-METHOD request to FULL-URL with retry on rate-limit or transient.
JSON-PAYLOAD and ACTUAL-TIMEOUT configure the request; AS is passed to
`org-canvas--api-execute-request'.  The headers are not an argument by
design: this frame is on the backtrace of every error that escapes,
and the token must not be (issue #178)."
  (let ((rate-retry-count 0)
        (transient-retry-index 0)
        (done nil)
        result)
    (while (not done)
      (condition-case err
          (progn
            (setq result
                  (org-canvas--api-execute-request plz-method full-url
                                                   json-payload actual-timeout as))
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

(defvar org-canvas--last-request-time nil
  "Time the previous Canvas API request went out, for pacing.
Consulted by `org-canvas--api-pace' when
`org-canvas-request-min-interval' is positive.")

(defun org-canvas--api-pace ()
  "Wait out `org-canvas-request-min-interval' since the previous request.
Sleeps only for whatever part of the interval has not already passed,
so a sync that spends its time parsing and exporting pays nothing
extra.  Records the time this request goes out."
  (when (and (numberp org-canvas-request-min-interval)
             (> org-canvas-request-min-interval 0)
             org-canvas--last-request-time)
    (let ((wait (- org-canvas-request-min-interval
                   (float-time (time-subtract nil org-canvas--last-request-time)))))
      (when (> wait 0)
        (org-canvas--log-debug org-canvas--logger
          "[Pace] Waiting %.1fs before the next request (org-canvas-request-min-interval)"
          wait)
        (org-canvas--wait wait))))
  (setq org-canvas--last-request-time (current-time)))

(defun org-canvas--check-writable (method &optional what)
  "Refuse METHOD on a course flagged with `org-canvas-read-only'.
WHAT names the operation for the message, defaulting to the method.
Only GET is allowed through; everything else signals
`org-canvas-read-only-error' before anything is sent."
  (when (and org-canvas-read-only (not (eq method 'GET)))
    (signal 'org-canvas-read-only-error
            (list (format "This course is marked read-only (org-canvas-read-only is t), so %s was refused.  Pull, status and diff still work; set org-canvas-read-only to nil in org-canvas-credentials.el to allow writes"
                          (or what (format "a %s request" method)))))))

(cl-defun org-canvas-api-request (method url &key params data timeout as)
  "Perform an HTTP request to the Canvas API synchronously using `plz'.
METHOD is \\='GET, \\='POST, \\='PUT, or \\='DELETE.
URL is the full endpoint.
PARAMS is an alist of query parameters.
DATA is an alist or hash-table to be sent as JSON body (for POST/PUT).
TIMEOUT is the request timeout in seconds.
AS `response' returns the `plz-response' with its headers instead of
the decoded body; `org-canvas-api-request-all-pages' asks for it to
follow the Link header.
A course marked read-only with `org-canvas-read-only' refuses anything
but GET, before the request is built (issue #163).

METHOD must be one of `org-canvas--api-supported-methods'.  PATCH is
sent via a direct curl fallback (plz cannot send it); anything not in
the list signals `org-canvas-api-error' immediately, since plz
silently corrupts unrecognized methods into bodyless GETs."
  (unless (memq method org-canvas--api-supported-methods)
    (org-canvas--signal 'org-canvas-api-error
      "Unsupported HTTP method %s: the plz transport can only send %s"
      method org-canvas--api-supported-methods))
  (org-canvas--check-writable method)
  (org-canvas--ensure-credentials)
  (let* ((full-url (concat url (org-canvas--api-build-query-string params)))
	 (json-payload (when data
			 (if (stringp data) data (json-encode data))))
	 (actual-timeout (or timeout org-canvas-request-timeout))
	 ;; IMPORTANT: plz requires lowercase method symbols ('post not 'POST)
         ;; Our codebase uses uppercase by convention, so convert here
	 (plz-method (intern (downcase (symbol-name method)))))

    (org-canvas--api-log-request
     (list :method method :url full-url :params params
           :body json-payload :timeout actual-timeout
           :headers (org-canvas--api-request-headers 'masked)))
    (org-canvas--api-pace)
    (org-canvas--api-execute-with-retry plz-method full-url json-payload actual-timeout as)))

(defun org-canvas--api-decode-response (response)
  "Return the JSON in RESPONSE's body, a `plz-response', or nil when empty."
  (let ((body (plz-response-body response)))
    (when (and (stringp body) (not (string-blank-p body)))
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (json-read)))))

(defun org-canvas--api-next-page-url (response)
  "Return the URL RESPONSE's Link header names as `next', or nil.
Canvas paginates some endpoints by bookmark, and answers a numbered
page beyond the first with 400 \"Invalid page; please restart
iteration and follow next links\" (the enrollments API does); the Link
header is the only way through those, and works for the numbered ones
too."
  (let ((link (and (plz-response-p response)
                   (alist-get 'link (plz-response-headers response)))))
    (when (stringp link)
      (cl-loop for part in (split-string link ",")
               when (string-match "<\\([^>]+\\)>[^,]*rel=\"next\"" part)
               return (match-string 1 part)))))

(defun org-canvas--api-resource-name (url)
  "Return the Canvas resource URL addresses, for a progress message.
The last path segment that is not an id, with underscores read as
spaces: a modules-items URL reads \"items\", an outcome-groups one
reads \"subgroups\".  Nil when nothing usable is left."
  (when (stringp url)
    (let* ((path (car (split-string url "[?#]")))
           (segments (nreverse (split-string path "/" t)))
           (name (cl-find-if-not (lambda (s) (string-match-p "\\`[0-9]+\\'" s))
                                 segments)))
      (when (and name (not (string-match-p "\\`[0-9.]*\\'" name)))
        (replace-regexp-in-string "_" " " name)))))

(defvar org-canvas--api-unwrap-key nil
  "Key under which the reply being paged wraps its rows, or nil.
Most list endpoints answer a bare array; quiz submissions answer
{quiz_submissions: [...]}.  Bound around a call to
`org-canvas-api-request-all-pages' by a caller that knows the key, so
the helper's signature, and every test stub of it, stays as it is.")

(defun org-canvas--api-page-items (reply)
  "Return the rows of one page REPLY as a list.
REPLY is a `plz-response' (decoded here) or an already-decoded body
from a stub; a body wrapped under `org-canvas--api-unwrap-key' is
unwrapped."
  (let ((body (if (plz-response-p reply) (org-canvas--api-decode-response reply) reply)))
    (append (if org-canvas--api-unwrap-key
                (alist-get org-canvas--api-unwrap-key body)
              body)
            nil)))

(defvar org-canvas--api-max-pages 1000
  "Pages `org-canvas-api-request-all-pages' walks before it gives up.
A hundred thousand items is more than any course endpoint answers; a
walk that long is a reply that never comes back short, and the walk
would otherwise grow without bound.")

(defun org-canvas--api-page-repeats-p (page-items previous)
  "Return non-nil when PAGE-ITEMS is the same page as PREVIOUS.
A reply without headers that answers the same full page to every
page number (a test stub, or an endpoint that ignores `page') would
otherwise be walked forever, one copy of the page per turn."
  (and previous (equal page-items previous)))

(defun org-canvas--api-check-page-cap (resource page count)
  "Signal `org-canvas-api-error' when PAGE of RESOURCE is past the cap.
COUNT is how many items the walk has gathered so far, for the message.
The cap is `org-canvas--api-max-pages'."
  (when (> page org-canvas--api-max-pages)
    (signal 'org-canvas-api-error
            (list (format "Pagination of %s did not end after %d pages (%d items); giving up"
                          resource org-canvas--api-max-pages count)))))

(defun org-canvas-api-request-all-pages (method url &optional params)
  "Fetch all pages of results from a paginated Canvas API endpoint.
METHOD is the HTTP method (usually \\='GET).
URL is the full endpoint URL.
PARAMS is an alist of additional query parameters.
A reply that wraps its rows is unwrapped when
`org-canvas--api-unwrap-key' is bound to the key.
Asks for per_page=100 and follows the Link header's `next' URL until
there is none, which is how Canvas asks to be paged: an endpoint
paginated by bookmark (enrollments) answers a numbered second page
with a 400.  A reply without headers — a stubbed request in the tests —
falls back to numbered pages until one comes back short, or repeats
the page before it; a walk past `org-canvas--api-max-pages' signals
`org-canvas-api-error' rather than growing without bound.
Returns a flat list of all items across all pages."
  (let ((page 1)
        (per-page 100)
        (all-items nil)
        (next-url nil)
        (previous nil)
        (done nil)
        (resource (or (org-canvas--api-resource-name url) "results")))
    (while (not done)
      ;; Page 1 is the whole story for most fetches, and announcing it
      ;; said neither what was being fetched nor anything that changes:
      ;; ninety-nine identical "Fetching page 1 (0 items so far)" lines
      ;; were a quarter of one pull\='s output (issue #156).  The echo
      ;; area now hears only about a fetch that really is paging; the
      ;; log hears about every page.
      (org-canvas--log-debug org-canvas--logger
        "[API] Fetching %s page %d (%d so far)" resource page (length all-items))
      (when (> page 1)
        (message "Fetching %s, page %d (%d so far)..."
                 resource page (length all-items)))
      (let* ((page-params (append (or params '())
                                  `(("per_page" . ,(number-to-string per-page))
                                    ("page" . ,(number-to-string page)))))
             ;; A `next' URL already carries the query, bookmark included.
             (reply (if next-url
                        (org-canvas-api-request method next-url :as 'response)
                      (org-canvas-api-request method url :params page-params :as 'response)))
             (with-headers (plz-response-p reply))
             (page-items (org-canvas--api-page-items reply)))
        (when (org-canvas--api-page-repeats-p page-items previous)
          (org-canvas--log-warning org-canvas--logger
            "[API] %s page %d repeats page %d; the endpoint ignores paging, stopping here"
            resource page (1- page))
          (setq page-items nil))
        (dolist (item page-items)
          (push item all-items))
        (setq next-url (and with-headers (org-canvas--api-next-page-url reply))
              previous page-items
              done (if with-headers (null next-url) (< (length page-items) per-page)))
        (unless done
          (setq page (1+ page))
          (org-canvas--api-check-page-cap resource page (length all-items)))))
    (nreverse all-items)))


;;;; GraphQL

;; Canvas keeps a few course-level operations out of its REST API: the
;; grade post policies and posting grades are GraphQL mutations only
;; (issue #202).  The endpoint takes the same bearer token, so the
;; request travels through `org-canvas-api-request' and inherits its
;; redaction, rate-limit handling and the read-only guard.

(defun org-canvas--graphql-url ()
  "Return the course instance's GraphQL endpoint."
  (format "%s/api/graphql" org-canvas-base-url))

(defun org-canvas--graphql-mutation-p (document)
  "Return non-nil when the GraphQL DOCUMENT is a mutation."
  (string-match-p "\\`[[:space:]]*mutation\\b" document))

(defun org-canvas--graphql-errors-message (errors)
  "Join the GraphQL ERRORS (a vector or list of alists) into one line."
  (mapconcat (lambda (e) (or (alist-get 'message e) (format "%s" e)))
             (append errors nil) "; "))

(defun org-canvas--graphql-send (document &optional variables)
  "POST the GraphQL DOCUMENT with VARIABLES and return its `data' alist.
GraphQL answers 200 with an `errors' array when a field fails, so a
reply carrying errors signals `org-canvas-api-error' with their
messages; the transport's own errors pass through as they are."
  (let* ((body (append (list (cons 'query document))
                       (when variables (list (cons 'variables variables)))))
         (reply (org-canvas-api-request 'POST (org-canvas--graphql-url) :data body))
         (errors (and (listp reply) (alist-get 'errors reply))))
    (when (and errors (not (eq errors :null)) (> (length errors) 0))
      (org-canvas--signal 'org-canvas-api-error
        "GraphQL: %s" (org-canvas--graphql-errors-message errors)))
    (and (listp reply) (alist-get 'data reply))))

(defun org-canvas--graphql-query (document &optional variables)
  "Run the GraphQL query DOCUMENT with VARIABLES; return its `data' alist.
A query is a read, so a course marked `org-canvas-read-only' allows it
although it travels as a POST: the guard is lifted for this one request
only, and never for a document that is a mutation, which signals here
before anything is sent.  Mutations go through
`org-canvas--graphql-mutate', which keeps the guard and the dry run."
  (when (org-canvas--graphql-mutation-p document)
    (org-canvas--signal 'org-canvas-api-error
      "org-canvas--graphql-query was handed a mutation; use org-canvas--graphql-mutate"))
  (let ((org-canvas-read-only nil))
    (org-canvas--graphql-send document variables)))

(defvar org-canvas--course-post-policy-cache nil
  "Cons of (COURSE-ID . POLICY) from the last course post-policy read.
POLICY is \"manual\" or \"automatic\".  The assignments pull and the
drift report ask once per run whether each assignment differs from the
course; this keeps that to one GET.  Forgotten by
`org-canvas--course-post-policy-forget' when the settings push changes
the policy.")

(defun org-canvas--post-manually-to-policy (post-manually)
  "Return \"manual\" for a true POST-MANUALLY, \"automatic\" for false, else nil."
  (cond ((eq post-manually t) "manual")
        ((eq post-manually :json-false) "automatic")
        (t nil)))

(defun org-canvas--post-policy-from-property (raw property-name)
  "Return RAW when it names a post policy, else warn and return nil.
Unlike `org-canvas--validate-property', an unrecognised value falls
back to nothing rather than to the first allowed value: a typo must not
quietly flip a gradebook to manual or automatic posting.  PROPERTY-NAME
is for the warning."
  (cond ((null raw) nil)
        ((member raw org-canvas--valid-post-policies) raw)
        (t (org-canvas--log-warning org-canvas--logger
             "[Validate] %s: '%s' is not valid (expected: %s); the policy is left as it is"
             property-name raw (string-join org-canvas--valid-post-policies ", "))
           nil)))

(defun org-canvas--course-post-policy ()
  "Return the course's grade post policy, \"manual\" or \"automatic\".
Read once per course through GET /courses/:id?include[]=post_manually
and cached; nil when Canvas does not report it."
  (unless (equal (car org-canvas--course-post-policy-cache) org-canvas-course-id)
    (let ((course (org-canvas-api-request
                   'GET (org-canvas-api-course-endpoint "")
                   :params '(("include[]" . "post_manually")))))
      (setq org-canvas--course-post-policy-cache
            (cons org-canvas-course-id
                  (org-canvas--post-manually-to-policy
                   (alist-get 'post_manually course))))))
  (cdr org-canvas--course-post-policy-cache))

(defun org-canvas--course-post-policy-forget ()
  "Drop the cached course post policy, so the next read asks Canvas."
  (setq org-canvas--course-post-policy-cache nil))

;;;; 3b. Rubric Association

(defun org-canvas--associate-rubric (item-id rubric-id association-type &optional flags)
  "Associate RUBRIC-ID with ITEM-ID on Canvas.
ASSOCIATION-TYPE is \"Assignment\" or \"Discussion\".  FLAGS is a plist
whose :use-for-grading and :hide-score-total, when non-nil, travel as
the association's `use_for_grading' and `hide_score_total' (t or
`:json-false'); a nil flag is not sent, so Canvas keeps its value.
Canvas updates the existing association for the pair, so sending the
flags on every push keeps Org the source of truth (issue #118).
Returns t when the association was written, nil when it failed —
the caller reports the write so the item's baseline can be re-read
from Canvas afterwards (issue #124)."
  (org-canvas--log-info org-canvas--logger "[Rubric] Associating rubric %s with %s %s"
             rubric-id (downcase association-type) item-id)
  (let* ((endpoint (org-canvas-api-course-endpoint "rubric_associations"))
         (payload (make-hash-table :test 'equal))
         (assoc (make-hash-table :test 'equal))
         (use-for-grading (plist-get flags :use-for-grading))
         (hide-score-total (plist-get flags :hide-score-total)))
    (puthash "rubric_id" (string-to-number rubric-id) assoc)
    (puthash "association_id" item-id assoc)
    (puthash "association_type" association-type assoc)
    (puthash "purpose" "grading" assoc)
    (when use-for-grading
      (puthash "use_for_grading" use-for-grading assoc))
    (when hide-score-total
      (puthash "hide_score_total" hide-score-total assoc))
    (puthash "rubric_association" assoc payload)
    (condition-case err
        (progn
          (org-canvas-api-request 'POST endpoint :data payload)
          (org-canvas--log-info org-canvas--logger "[Rubric] Association created")
          t)
      (error
       (org-canvas--log-warning org-canvas--logger "[Rubric] Association failed: %s" (error-message-string err))
       (org-canvas--user-message "WARNING: Rubric association failed for %s: %s"
         item-id (error-message-string err))
       nil))))

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
    ;; Encode form parts as unibyte before concatenating with binary.
    ;; No terminator is appended after the join: `mapconcat' already puts
    ;; the CRLF that ends each field's value before the next boundary, and
    ;; the file part — always last — ends with its own CRLFCRLF, the blank
    ;; line closing a MIME header block.  Appending one more put two bytes
    ;; in front of every file Canvas stored, so no upload was ever
    ;; byte-faithful to its source (issue #70).
    (let* ((body-prefix (encode-coding-string
                         (mapconcat #'identity (nreverse body-parts) "\r\n")
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
        (org-canvas--check-writable 'POST "a file upload")
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
