# Pull Pipeline Improvements Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land all 20 items from `canvas-structure/improvements.md` plus the latent pagination bug found in the pull log, per the design at `docs/superpowers/specs/2026-04-26-pull-improvements-design.md`.

**Architecture:** Five waves, executed direct-to-main with one commit per task. Wave 1 builds reliability primitives (pagination migration, transient-error retry, end-of-pull summary, double-slash fix). Wave 2 fixes embedded-image rot via on-demand metadata fetch. Wave 3 is the hard schema cutover (file-level `LAST_SYNCED`, default suppression, rubric/description format, module item `:ITEM_TYPE:`, sort by position, `:INDENT:` suppression). Wave 4 is content fidelity (course image, TZ localization, override pull, announcement metadata, quiz-question bug). Wave 5 is empty-file headers and cosmetic polish.

**Tech Stack:** Emacs Lisp, `plz` HTTP, `buttercup` testing via `eldev test`, `undercover` coverage, `ert` for legacy tests.

---

## Files & Helpers Map

This is the canonical reference for which files/helpers each task adds or touches. Tasks reference these by name without re-explaining them.

**New helpers:**
- `org-canvas--api-paginated` — replaces hard-coded `?per_page=100&page=1` callers; thin wrapper around existing `org-canvas-api-request-all-pages`.
- `org-canvas--api-transient-error-p` — predicate for "should retry": curl errors 28/56/7, HTTP 502/503/504. (429 stays in the existing rate-limit path.)
- `org-canvas--pull-summary` — defvar holding alist of `(file . list-of-error-plists)`; helpers `summary-record`, `summary-print`, `summary-reset`.
- `org-canvas--pull-tz-cache` — defvar with the resolved course TZ for the current session.
- `org-canvas--pull-localize-timestamp` — UTC ISO-8601 → Org active timestamp in cached TZ.
- `org-canvas--pull-emit-empty-file` — write the "Canvas returned 0 items" header when an endpoint is empty.
- `org-canvas--rewrite-fetch-unknown-file` — Wave 2: GET `/api/v1/files/:id`, download, append to files.org cache.

**New defcustoms:**
- `org-canvas-emit-defaults` (default `nil`) — when non-nil, suppress nothing.
- `org-canvas-transient-retry-delays` (default `'(5 10 20)`) — sleeps between transient-error retries.

**Touched files** (one or more tasks each): `lisp/org-canvas-core-api.el`, `lisp/org-canvas-core-org.el`, `lisp/org-canvas-core-sync.el`, `lisp/org-canvas-core-config.el`, `lisp/org-canvas-pages.el`, `lisp/org-canvas-announcements.el`, `lisp/org-canvas-assignments.el`, `lisp/org-canvas-quizzes.el`, `lisp/org-canvas-rubrics.el`, `lisp/org-canvas-modules.el`, `lisp/org-canvas-files.el`, `lisp/org-canvas-settings.el`, `lisp/org-canvas-outcomes.el`, `lisp/org-canvas-discussions.el`, `lisp/org-canvas-calendar.el`, `lisp/org-canvas-group-categories.el`, `lisp/org-canvas-new-quizzes.el`, `lisp/org-canvas-validate.el`, `lisp/org-canvas.el`.

**Tests:** Per-module test files under `test/` with the same basename. Each task adds tests to the corresponding test file.

**Conventions** (from `CLAUDE.md`):
- Run `eldev lint && eldev complexity && eldev test` after each commit; all must be green.
- TDD: failing test first, minimal impl, verify pass, commit.
- Use `with-mock-api` and `with-temp-org-buffer` from `test/test-helper.el`.
- Stage markers in logs: `[Stage N: StageName]`.

---

# Wave 1: Reliability Primitives

## Task 1: Fix double-slash in API URLs (#16)

**Files:**
- Modify: `lisp/org-canvas-core-config.el` (defcustom watcher) or `lisp/org-canvas-core-api.el` (URL builder)
- Test: `test/org-canvas-core-api-test.el`

The pattern `clemson.instructure.com//api/v1/...` comes from concatenating `org-canvas-base-url` (with trailing `/`) directly with paths starting with `/`. Strip the trailing slash at point-of-use.

- [ ] **Step 1: Write the failing test**

Add to `test/org-canvas-core-api-test.el` inside the existing `describe "API URL builder"` block (or create one):

```elisp
(it "strips trailing slash from base-url before joining"
  (let ((org-canvas-base-url "https://clemson.instructure.com/")
        (org-canvas-course-id "281704"))
    (expect (org-canvas-api-course-endpoint "pages")
            :to-equal "https://clemson.instructure.com/api/v1/courses/281704/pages")))

(it "leaves base-url alone when there is no trailing slash"
  (let ((org-canvas-base-url "https://clemson.instructure.com")
        (org-canvas-course-id "281704"))
    (expect (org-canvas-api-course-endpoint "pages")
            :to-equal "https://clemson.instructure.com/api/v1/courses/281704/pages")))
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cd /mnt/b410f78a-9927-453b-b3b6-4b3910634741/Code/org-canvas && eldev test "strips trailing slash"
```

Expected: FAIL with `https://clemson.instructure.com//api/v1/...` mismatch.

- [ ] **Step 3: Write minimal implementation**

In `lisp/org-canvas-core-api.el`, modify `org-canvas-api-course-endpoint`:

```elisp
(defun org-canvas-api-course-endpoint (suffix &rest args)
  "Build a course-scoped API endpoint URL.
SUFFIX is appended to the base URL after `/api/v1/courses/COURSE-ID/'.
ARGS are format args applied to SUFFIX."
  (let ((base (replace-regexp-in-string "/+\\'" "" org-canvas-base-url)))
    (apply #'format
           (concat base "/api/v1/courses/" org-canvas-course-id "/" suffix)
           args)))
```

If a non-course endpoint helper exists (e.g., for `/api/v1/calendar_events`), apply the same `replace-regexp-in-string` pattern. Audit with `grep -n "org-canvas-base-url" lisp/`.

- [ ] **Step 4: Run test to verify pass**

```bash
eldev test "trailing slash"
```

Expected: PASS.

- [ ] **Step 5: Run full lint+test gate**

```bash
eldev lint && eldev complexity && eldev test
```

Expected: all green.

- [ ] **Step 6: Commit**

```bash
git add lisp/org-canvas-core-api.el test/org-canvas-core-api-test.el
git commit -m "$(cat <<'EOF'
fix(core-api): strip trailing slash from base-url

Improvements item #16: every Canvas API URL was emitted as
clemson.instructure.com//api/v1/... because the base URL kept its
trailing slash. Trim it at the join site so URLs are clean.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: Migrate single-page list callers to `api-request-all-pages` (latent pagination bug)

**Files:**
- Modify: every caller currently using `org-canvas-api-request 'GET ... :params ((per_page . 100) (page . 1))` for list endpoints
- Test: per-module test files; mock multi-page responses

Audit reveals these single-page list call sites (use `grep -n "page=1\|page-params\|(\"page\"" lisp/`):
- `lisp/org-canvas-modules.el:553` — module list
- `lisp/org-canvas-modules.el:578` — module items list
- `lisp/org-canvas-core-sync.el:568` — generic search
- Possibly `lisp/org-canvas-settings.el` tab/section calls

Each caller becomes a one-liner.

- [ ] **Step 1: Audit list-endpoint callers**

```bash
grep -n "page-params\|\"page\".*\"1\"" lisp/*.el | grep -v -E "^lisp/(org-canvas-core-api|.+\.elc):"
```

For each match, decide: is this a list endpoint that could exceed 100 items? If yes, plan a migration. Single-result `?page=1` calls (e.g., "first folder by name") stay.

- [ ] **Step 2: Write a failing pagination test**

In `test/org-canvas-core-api-test.el`:

```elisp
(it "fetches all pages until a short page is returned"
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
        (expect calls :to-equal 2)))))
```

(The existing all-pages helper already does this; the test locks in the contract for callers that migrate to it.)

- [ ] **Step 3: Run the test (it should already pass against existing all-pages helper)**

```bash
eldev test "fetches all pages"
```

Expected: PASS — this test is regression protection for the helper.

- [ ] **Step 4: Migrate `org-canvas-modules.el` module list**

Replace at line ~553:

```elisp
;; BEFORE:
(let* ((endpoint (org-canvas-api-course-endpoint "modules"))
       (params '(("include[]" . "items") ("per_page" . "100") ("page" . "1")))
       (results (append (org-canvas-api-request 'GET endpoint :params params) nil)))
  ...)

;; AFTER:
(let* ((endpoint (org-canvas-api-course-endpoint "modules"))
       (results (org-canvas-api-request-all-pages
                 'GET endpoint '(("include[]" . "items")))))
  ...)
```

(The helper adds `per_page` and `page` itself.)

- [ ] **Step 5: Migrate the rest**

Apply the same shape to each caller from Step 1. Add a per-caller test in the corresponding module test file mocking a 2-page response and verifying both pages' items appear in the output. For example, in `test/org-canvas-modules-test.el`, add a test that mocks two pages of modules and verifies both page-2 modules end up in `modules.org`.

- [ ] **Step 6: Run full gate**

```bash
eldev lint && eldev complexity && eldev test
```

Expected: green. If any module's existing tests broke (e.g., because they mocked the old single-page call shape), update the mock to use `org-canvas-api-request-all-pages` instead.

- [ ] **Step 7: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
fix(api): migrate list-endpoint callers to api-request-all-pages

Latent pagination bug: every list endpoint hit ?per_page=100&page=1
with no follow-up, silently truncating courses with more than 100
modules/assignments/etc. Migrate the remaining single-page callers
to the existing all-pages helper.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 3: Retry on transient errors (timeout, 5xx) — fixes #1 part A

**Files:**
- Modify: `lisp/org-canvas-core-api.el` — extend `org-canvas--api-handle-plz-error` and add `org-canvas--api-transient-error-p`
- Modify: `lisp/org-canvas-core-config.el` — add `org-canvas-transient-retry-delays`
- Test: `test/org-canvas-core-api-test.el`

Existing retry only fires on 429/403-rate. Improvements #1 needs retry on plz curl error 28 (timeout) and HTTP 502/503/504. Keep rate-limit retry separate (Canvas dictates that wait); add a new retry path with exponential backoff for transient errors.

- [ ] **Step 1: Add the defcustom**

In `lisp/org-canvas-core-config.el` near `org-canvas-rate-limit-wait`:

```elisp
(defcustom org-canvas-transient-retry-delays '(5 10 20)
  "Sleep durations (seconds) between retries of transient API errors.
List length determines the maximum number of retries. Transient errors
include curl timeouts (errors 28, 56, 7) and HTTP 502/503/504. Rate
limit retries (HTTP 429) use `org-canvas-rate-limit-wait' separately."
  :type '(repeat integer)
  :group 'org-canvas)
```

- [ ] **Step 2: Write the failing test**

In `test/org-canvas-core-api-test.el`:

```elisp
(describe "transient retry"
  (it "retries plz curl-28 timeouts up to retry-delays length"
    (let* ((calls 0)
           (org-canvas-transient-retry-delays '(0 0))  ; suppress sleeps in tests
           (org-canvas-api-token "test-token")
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
           (org-canvas-base-url "https://example.invalid"))
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _args)
                   (signal 'plz-error
                           (make-plz-error :curl-error '(28 . "Operation timeout.")))))
                ((symbol-function 'sleep-for) (lambda (_) nil)))
        (expect (org-canvas-api-request 'GET "https://example.invalid/api/v1/x")
                :to-throw 'org-canvas-api-error)))))
```

- [ ] **Step 3: Run tests; expect FAIL**

```bash
eldev test "transient retry"
```

Expected: FAIL — current code re-raises immediately on curl-28.

- [ ] **Step 4: Implement transient detection**

Add to `lisp/org-canvas-core-api.el`:

```elisp
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
```

- [ ] **Step 5: Wire transient retry into the request loop**

Modify `org-canvas--api-execute-with-retry` to handle transients. The cleanest approach is to extend `org-canvas--api-handle-plz-error` to return `:retry-transient` and have the loop branch on it:

```elisp
(defun org-canvas--api-handle-plz-error (err full-url)
  "Handle a plz-error ERR from a request to FULL-URL.
Return `:retry' for rate-limited (sleep already done), `:retry-transient'
for transient errors (caller must sleep), or signal an error for
terminal failures."
  (let* ((plz-err (cdr err))
         (response (and (plz-error-p plz-err) (plz-error-response plz-err)))
         (status (and response (plz-response-status response)))
         (body (and response (plz-response-body response)))
         (err-msg (if status
                      (format "API Request Failed (HTTP %s)" status)
                    (format "API Request Failed: %S" plz-err))))
    (org-canvas--log-debug org-canvas--logger "[API] <<< RESPONSE: %s" (or status "error"))
    (cond
     ;; Rate limited (sleep here, return :retry)
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
     ;; 401, 403 non-rate-limit
     ((and status (= status 401))
      (signal 'org-canvas-credentials-error
        (list "Authentication failed (HTTP 401). Your API token may have expired.\nGenerate a new one at Canvas > Account > Settings > Approved Integrations."
              body plz-err)))
     ((and status (= status 403))
      (signal 'org-canvas-credentials-error
        (list "Permission denied (HTTP 403). Your token may lack the required scope for this operation."
              body plz-err)))
     ;; Transient — caller will sleep based on retry index
     ((org-canvas--api-transient-error-p plz-err)
      :retry-transient)
     ;; Terminal
     (t
      (org-canvas--log-error org-canvas--logger "%s\n  URL: %s\n  Body: %S"
        err-msg full-url body)
      (signal 'org-canvas-api-error (list err-msg body plz-err))))))
```

Then update `org-canvas--api-execute-with-retry` to track a transient-retry counter:

```elisp
(defun org-canvas--api-execute-with-retry (plz-method full-url headers json-payload actual-timeout)
  "Execute PLZ-METHOD request to FULL-URL with retry on rate-limit or transient errors.
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
            (if (< rate-retry-count org-canvas-rate-limit-retries)
                (progn
                  (org-canvas--log-debug org-canvas--logger
                    "[API] Rate retry %d/%d" (1+ rate-retry-count) org-canvas-rate-limit-retries)
                  (setq rate-retry-count (1+ rate-retry-count)))
              (org-canvas--api-retries-exhausted rate-retry-count err)))
           (:retry-transient
            (let ((delay (nth transient-retry-index org-canvas-transient-retry-delays)))
              (if delay
                  (progn
                    (org-canvas--log-warning org-canvas--logger
                      "[API] Transient error, retrying in %ds (%d/%d)"
                      delay (1+ transient-retry-index)
                      (length org-canvas-transient-retry-delays))
                    (sleep-for delay)
                    (setq transient-retry-index (1+ transient-retry-index)))
                ;; Exhausted: signal the original error
                (let* ((plz-err (cdr err))
                       (response (and (plz-error-p plz-err) (plz-error-response plz-err)))
                       (body (and response (plz-response-body response))))
                  (signal 'org-canvas-api-error
                          (list (format "Transient error after %d retries"
                                        (length org-canvas-transient-retry-delays))
                                body plz-err))))))))))
    result))
```

- [ ] **Step 6: Run tests; expect PASS**

```bash
eldev test "transient retry"
```

Expected: all three new tests pass; existing rate-limit tests still pass.

- [ ] **Step 7: Run full gate**

```bash
eldev lint && eldev complexity && eldev test
```

Expected: green. If `complexity` flags `org-canvas--api-execute-with-retry` over threshold, extract the two retry branches into helpers `org-canvas--api-handle-rate-retry` and `org-canvas--api-handle-transient-retry`.

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-core-api.el lisp/org-canvas-core-config.el test/org-canvas-core-api-test.el
git commit -m "$(cat <<'EOF'
feat(core-api): retry transient errors with exp backoff

Improvements item #1 part A. Curl timeouts (errors 28/56/7) and HTTP
502/503/504 are now retried with delays from
org-canvas-transient-retry-delays (default 5/10/20s). Rate-limit
retries (429) keep their existing path. The page-detail timeout that
shipped a corrupted pages.org will now retry.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: End-of-pull error summary (#1 part B)

**Files:**
- Create accumulator in: `lisp/org-canvas-core-org.el` (or new file `lisp/org-canvas-core-summary.el` if core-org grows past ~1100 lines after this task)
- Modify: `lisp/org-canvas.el` `org-canvas-pull-all` to call summary print at the end
- Modify: each pull function to record errors when one item fails inside a multi-item pull
- Test: `test/org-canvas-core-org-test.el`

The accumulator is a defvar, not a global. It's bound during `pull-all` and printed to `*Messages*` plus a `*org-canvas-pull-summary*` buffer at end-of-pull.

- [ ] **Step 1: Write failing test**

In `test/org-canvas-core-org-test.el`:

```elisp
(describe "pull summary accumulator"
  (it "starts empty after reset"
    (org-canvas--pull-summary-reset)
    (expect (org-canvas--pull-summary-empty-p) :to-be t))

  (it "records errors with file, message, and log line"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record
     :file "pages.org"
     :item "connecting-to-the-palmetto-jupyter-image"
     :error "Operation timeout"
     :log-line 154)
    (expect (org-canvas--pull-summary-empty-p) :to-be nil)
    (let ((records (org-canvas--pull-summary-records)))
      (expect (length records) :to-equal 1)
      (expect (plist-get (car records) :file) :to-equal "pages.org")))

  (it "prints a summary block when non-empty"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record
     :file "pages.org" :item "x" :error "Operation timeout" :log-line 154)
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "Pull complete with 1 non-fatal error")
      (expect output :to-match "pages.org"))))
```

- [ ] **Step 2: Run; expect FAIL** — `org-canvas--pull-summary-*` undefined.

```bash
eldev test "pull summary accumulator"
```

- [ ] **Step 3: Implement**

Append to `lisp/org-canvas-core-org.el`:

```elisp
(defvar org-canvas--pull-summary nil
  "Accumulator for non-fatal errors during a pull.
Each element is a plist with :file, :item, :error, :log-line.")

(defun org-canvas--pull-summary-reset ()
  "Clear the pull summary accumulator."
  (setq org-canvas--pull-summary nil))

(defun org-canvas--pull-summary-empty-p ()
  "Non-nil when no errors have been recorded this pull."
  (null org-canvas--pull-summary))

(defun org-canvas--pull-summary-records ()
  "Return the list of recorded summary entries (oldest first)."
  (reverse org-canvas--pull-summary))

(cl-defun org-canvas--pull-summary-record (&key file item error log-line)
  "Record a non-fatal pull failure for FILE/ITEM with ERROR text and LOG-LINE."
  (push (list :file file :item item :error error :log-line log-line)
        org-canvas--pull-summary))

(defun org-canvas--pull-summary-print ()
  "Print the pull summary to standard output (and *Messages*)."
  (let ((records (org-canvas--pull-summary-records)))
    (when records
      (princ (format "Pull complete with %d non-fatal error%s:\n"
                     (length records)
                     (if (= (length records) 1) "" "s")))
      (dolist (rec records)
        (princ (format "  %s%s: %s%s\n"
                       (or (plist-get rec :file) "(unknown)")
                       (if (plist-get rec :item)
                           (format " [%s]" (plist-get rec :item))
                         "")
                       (plist-get rec :error)
                       (if (plist-get rec :log-line)
                           (format " (log line %d)" (plist-get rec :log-line))
                         "")))))))
```

- [ ] **Step 4: Run tests; expect PASS**

```bash
eldev test "pull summary accumulator"
```

- [ ] **Step 5: Wire into `org-canvas-pull-all`**

In `lisp/org-canvas.el`, find `org-canvas-pull-all` (or whatever the orchestration entry is) and wrap:

```elisp
(defun org-canvas-pull-all ()
  "Pull all course data from Canvas."
  (interactive)
  (org-canvas--pull-summary-reset)
  (unwind-protect
      (progn
        ;; ...existing pull pipeline...
        )
    (unless (org-canvas--pull-summary-empty-p)
      (with-output-to-temp-buffer "*org-canvas-pull-summary*"
        (org-canvas--pull-summary-print))
      (message "Pull complete with errors — see *org-canvas-pull-summary*."))))
```

(Read the actual function shape first — it may already use `condition-case` per phase. Insert the reset at the top and the print in the unwind-protect cleanup.)

- [ ] **Step 6: Wire pages-detail timeout to call `summary-record`**

In `lisp/org-canvas-pages.el` around the detail call (line ~156), wrap the per-page detail fetch in a `condition-case` that records to summary on error:

```elisp
(condition-case err
    (org-canvas-api-request 'GET detail-url)
  (org-canvas-api-error
   (org-canvas--pull-summary-record
    :file (file-name-nondirectory org-canvas-pages-file)
    :item slug
    :error (error-message-string err))
   nil))  ; return nil so the caller skips this page
```

The caller must handle nil-detail — leave the heading as-is (or write it without body) instead of corrupting `:CANVAS_URL:`. **This is the original symptom from improvements.md #1.**

Adjust the caller to set `:CANVAS_ID:` correctly even on detail failure: pull the `id` from the list-page response (which already has it) instead of relying on the detail response.

- [ ] **Step 7: Add a test for pages-detail-timeout summary recording**

In `test/org-canvas-pages-test.el`, mock the list call to return one page and the detail call to throw `org-canvas-api-error`. Assert: (a) summary records exactly one entry, (b) `:CANVAS_ID` is set on the heading from the list response, not `:CANVAS_URL`.

- [ ] **Step 8: Run full gate**

```bash
eldev lint && eldev complexity && eldev test
```

- [ ] **Step 9: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
feat(pull): record non-fatal errors and print summary at end

Improvements item #1 part B. Pull functions catch per-item failures,
record them in org-canvas--pull-summary, and at the end of pull-all the
summary is printed to *org-canvas-pull-summary*. The page-detail
timeout no longer corrupts pages.org with :CANVAS_URL: — the heading
gets :CANVAS_ID: from the list-page response, the detail body is
skipped, and the failure surfaces in the summary buffer.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

# Wave 2: Embedded Files

## Task 5: Expand the embedded-URL regex (#2 part A)

**Files:**
- Modify: `lisp/org-canvas-core-org.el` — extend the regex used by `org-canvas--rewrite-canvas-file-urls` (or whatever the existing helper is called; check after `b8838c7`)
- Test: `test/org-canvas-core-org-test.el`

Existing rewriter handles `files/NNNN/preview?verifier=...`. Need to also handle `files/NNNN?verifier=...&wrap=1` (no `/preview`) — that's the `[Clemson Home Data Analyst Student Position-1.pdf]` link in `announcements.org` line 132 that wasn't rewritten despite file 29871332 being in files.org.

- [ ] **Step 1: Locate the existing regex**

```bash
grep -n "files/\\\\\\|preview\\\\\\|verifier\\\\" lisp/org-canvas-core-org.el
```

Find the `defconst` or `defun` containing the rewriter regex. Read 30 lines around it to understand the rewrite shape.

- [ ] **Step 2: Write a failing test**

In `test/org-canvas-core-org-test.el`:

```elisp
(describe "Canvas file URL rewriting"
  (it "rewrites /preview?verifier= form (existing)"
    (let ((file-map (list (cons "29871332" "content/Uploaded Media/file.pdf"))))
      (expect
       (org-canvas--rewrite-canvas-file-urls
        "before [[https://x.com/courses/281704/files/29871332/preview?verifier=ABC]] after"
        file-map)
       :to-equal
       "before [[file:content/Uploaded Media/file.pdf]] after")))

  (it "rewrites bare ?verifier=&wrap=1 form (new)"
    (let ((file-map (list (cons "29871332" "content/Uploaded Media/file.pdf"))))
      (expect
       (org-canvas--rewrite-canvas-file-urls
        "x [[https://x.com/courses/281704/files/29871332?verifier=ABC&wrap=1]] y"
        file-map)
       :to-equal
       "x [[file:content/Uploaded Media/file.pdf]] y")))

  (it "preserves the link description when present"
    (let ((file-map (list (cons "29871332" "content/Uploaded Media/file.pdf"))))
      (expect
       (org-canvas--rewrite-canvas-file-urls
        "[[https://x.com/courses/281704/files/29871332?verifier=ABC&wrap=1][display.pdf]]"
        file-map)
       :to-equal
       "[[file:content/Uploaded Media/file.pdf][display.pdf]]"))))
```

- [ ] **Step 3: Run; expect FAIL on the bare-?verifier and description tests**

```bash
eldev test "Canvas file URL rewriting"
```

- [ ] **Step 4: Update the regex**

The new regex should match `files/(\d+)(/preview)?(\?[^]\s]*)?` — capture file ID, allow optional `/preview`, capture optional query string. Update the rewriter to:

```elisp
(defconst org-canvas--canvas-file-url-regex
  ;; courses/COURSE/files/ID with optional /preview and optional query string
  (concat "https://[^/]+/courses/[0-9]+/files/"
          "\\([0-9]+\\)"
          "\\(?:/preview\\)?"
          "\\(?:\\?[^]\n[:space:]]*\\)?"))
```

And in the rewrite function, look up `(match-string 1 url)` in the file-map; if found, replace; if not, leave intact for now (Task 6 fills the gap).

- [ ] **Step 5: Run; expect PASS for all three new tests**

- [ ] **Step 6: Run full gate**

- [ ] **Step 7: Commit**

```bash
git add lisp/org-canvas-core-org.el test/org-canvas-core-org-test.el
git commit -m "$(cat <<'EOF'
fix(rewriter): match bare ?verifier= file URL form

Improvements item #2 part A. The existing rewriter only handled
files/NNNN/preview?verifier=...; URLs without /preview (the form used
for non-image attachments) were left as expiring Canvas URLs even when
the file was already downloaded. Expand the regex to match both forms.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 6: On-demand metadata fetch for unknown file IDs (#2 part B, #3 indirectly)

**Files:**
- Modify: `lisp/org-canvas-core-org.el` — add `org-canvas--rewrite-fetch-unknown-file`
- Modify: `lisp/org-canvas-files.el` — expose helpers to append a single entry to files.org and download a single file given metadata
- Test: `test/org-canvas-core-org-test.el`, `test/org-canvas-files-test.el`

When the rewriter encounters a file ID with no entry in the cache, GET `/api/v1/files/:id`, derive the canonical folder path, download the file, append a heading to files.org, update the cache. Subsequent rewrites in the same session reuse the cache.

- [ ] **Step 1: Audit existing files.org append helpers**

```bash
grep -n "defun org-canvas--file-emit\\|append-to-files\\|emit-fresh" lisp/org-canvas-files.el
```

If a single-entry append helper doesn't exist, plan to extract one from the existing fresh-tree emitter.

- [ ] **Step 2: Write a failing test**

In `test/org-canvas-core-org-test.el`:

```elisp
(describe "fetch unknown file on rewrite"
  (it "fetches metadata, downloads, registers, and rewrites"
    (let* ((file-map (list))
           (api-calls 0)
           (downloads 0)
           (file-id "30061566"))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method url &rest _args)
                   (cl-incf api-calls)
                   (when (string-match-p "/api/v1/files/30061566" url)
                     `((id . 30061566)
                       (display_name . "screenshot.png")
                       (folder_id . 999)
                       (url . "https://x.com/files/30061566/download?verifier=Z")
                       (content-type . "image/png")
                       (size . 12345)))))
                ((symbol-function 'org-canvas--folder-relative-path)
                 (lambda (_id) "Uploaded Media"))
                ((symbol-function 'org-canvas--file-download-to)
                 (lambda (_url path)
                   (cl-incf downloads)
                   (make-directory (file-name-directory path) t)
                   (with-temp-file path (insert "fake bytes")))))
        (let* ((cache (make-hash-table :test 'equal))
               (rewritten
                (org-canvas--rewrite-canvas-file-urls
                 "see [[https://x.com/courses/281704/files/30061566/preview?verifier=A]]"
                 file-map :resolve-unknown t :cache cache)))
          (expect rewritten :to-match "\\[\\[file:content/Uploaded Media/screenshot\\.png\\]\\]")
          (expect api-calls :to-equal 1)
          (expect downloads :to-equal 1)
          (expect (gethash "30061566" cache) :to-equal "content/Uploaded Media/screenshot.png"))))))
```

- [ ] **Step 3: Run; expect FAIL** — helper params don't exist yet.

- [ ] **Step 4: Implement `org-canvas--rewrite-fetch-unknown-file`**

```elisp
(defun org-canvas--rewrite-fetch-unknown-file (file-id session-cache)
  "Fetch metadata for FILE-ID, download it, return its local relpath.
Cache the result in SESSION-CACHE (a hash table) for reuse across calls.
Returns nil if the file cannot be fetched (records to pull-summary)."
  (or (gethash file-id session-cache)
      (condition-case err
          (let* ((url (format "%s/api/v1/files/%s"
                              (replace-regexp-in-string "/+\\'" "" org-canvas-base-url)
                              file-id))
                 (meta (org-canvas-api-request 'GET url))
                 (display-name (alist-get 'display_name meta))
                 (folder-id (alist-get 'folder_id meta))
                 (download-url (alist-get 'url meta))
                 (folder-rel (or (org-canvas--folder-relative-path folder-id)
                                 "Uploaded Media"))
                 (rel-path (concat "content/" folder-rel "/" display-name))
                 (abs-path (expand-file-name rel-path org-canvas-directory)))
            (org-canvas--file-download-to download-url abs-path)
            (org-canvas--files-org-append-entry
             :id file-id :display-name display-name
             :folder-rel folder-rel :rel-path rel-path)
            (puthash file-id rel-path session-cache)
            rel-path)
        (error
         (org-canvas--pull-summary-record
          :file "embedded-file-fetch"
          :item file-id
          :error (error-message-string err))
         nil))))
```

- [ ] **Step 5: Extend `org-canvas--rewrite-canvas-file-urls` signature**

```elisp
(cl-defun org-canvas--rewrite-canvas-file-urls (text file-map &key resolve-unknown cache)
  "Rewrite Canvas file URLs in TEXT to local file: links.
FILE-MAP is an alist of (file-id . relpath) for known files.
When RESOLVE-UNKNOWN is non-nil and a file ID is not in FILE-MAP,
fetch metadata from Canvas, download the file, and rewrite. CACHE is a
session hash-table used to dedupe lookups."
  (let ((cache (or cache (make-hash-table :test 'equal))))
    (replace-regexp-in-string
     org-canvas--canvas-file-url-regex
     (lambda (m)
       (let* ((file-id (match-string 1 m))
              (rel-path (or (cdr (assoc file-id file-map))
                            (gethash file-id cache)
                            (and resolve-unknown
                                 (org-canvas--rewrite-fetch-unknown-file file-id cache)))))
         (if rel-path
             (format "[[file:%s]]" rel-path)
           m)))  ; leave as-is on failure
     text t t)))
```

(The bracket-link variant — `[[URL][desc]]` — needs a separate substitution that preserves `desc`. Add a sibling regex `\\[\\[org-canvas--canvas-file-url-regex\\]\\[\\([^]]*\\)\\]\\]` and rewrite to `[[file:PATH][DESC]]`.)

- [ ] **Step 6: Implement helper `org-canvas--file-download-to`**

In `lisp/org-canvas-files.el`:

```elisp
(defun org-canvas--file-download-to (url abs-path)
  "Download URL to ABS-PATH using `plz' (binary mode)."
  (make-directory (file-name-directory abs-path) t)
  (let ((data (plz 'get url :as 'binary
                   :timeout org-canvas-upload-timeout)))
    (with-temp-file abs-path
      (set-buffer-multibyte nil)
      (insert data))
    (org-canvas--log-info org-canvas--logger "[Download] %s (%d bytes)"
      (file-name-nondirectory abs-path)
      (length data))
    abs-path))
```

- [ ] **Step 7: Implement `org-canvas--files-org-append-entry`**

This appends a heading under the right folder ancestor in `files.org`. Quickest acceptable path: if the folder-rel ancestor heading exists, insert under it; else create the folder heading at the right nesting level. For implementation simplicity in this task, append at end-of-file under a flat "Uploaded Media" heading and accept that fresh-tree pull will reorganize on the next `rm + re-pull`.

```elisp
(defun org-canvas--files-org-append-entry (&rest props)
  "Append a single file entry to files.org.
PROPS keys: :id :display-name :folder-rel :rel-path."
  (let ((id (plist-get props :id))
        (display-name (plist-get props :display-name))
        (folder-rel (plist-get props :folder-rel))
        (rel-path (plist-get props :rel-path))
        (file org-canvas-files-file))
    (unless (file-exists-p file)
      (with-temp-file file (insert "#+TITLE: Files\n")))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        ;; Ensure folder-rel ancestor heading exists
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\* %s$" (regexp-quote folder-rel)) nil t)
          (goto-char (point-max))
          (insert (format "\n* %s\n" folder-rel))
          (forward-char -1))
        (org-end-of-subtree t t)
        (insert (format "** [[file:%s][%s]]\n:PROPERTIES:\n:CANVAS_ID: %s\n:END:\n"
                        rel-path display-name id)))
      (save-buffer))))
```

- [ ] **Step 8: Run tests; expect PASS for the unknown-file fetch test**

- [ ] **Step 9: Wire `:resolve-unknown t` into all body-rewrite call sites**

Modify the existing pull pipeline so each module's body rewrite calls `(org-canvas--rewrite-canvas-file-urls ... :resolve-unknown t :cache org-canvas--pull-rewrite-cache)`. Add `defvar org-canvas--pull-rewrite-cache` reset alongside the summary in `pull-all`.

Modules to update: announcements, assignments, pages, quizzes, discussions (whichever currently call the rewriter).

- [ ] **Step 10: Run full gate**

If `complexity` flags `org-canvas--rewrite-fetch-unknown-file`, split into smaller helpers (`build-url`, `derive-rel-path`, `record-failure`).

- [ ] **Step 11: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
feat(rewriter): on-demand fetch+download for unknown file IDs

Improvements item #2. When the body rewriter encounters a Canvas
files/NNNN URL whose ID is not already in files.org, GET
/api/v1/files/:id, derive the canonical folder path, download into
content/<folder>/<display_name>, append an entry to files.org, and
rewrite the link. A session cache dedupes lookups for IDs that appear
in multiple bodies. Failure to fetch records to the pull summary.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

# Wave 3: Schema Cutover

## Task 7: File-level `#+LAST_SYNCED:` header (#7)

**Files:**
- Modify: `lisp/org-canvas-core-org.el` — `org-canvas-org-save-sync-state` no longer sets per-entry; new function writes file-level header
- Modify: every pull function — call `org-canvas--pull-write-file-header` after writing items
- Modify: push side — read file-level header for conflict timestamps
- Test: per-module pull tests; `test/org-canvas-core-org-test.el`

- [ ] **Step 1: Audit all `LAST_SYNCED` reads/writes**

```bash
grep -n "LAST_SYNCED" lisp/*.el | grep -v ".elc"
```

Catalog: which functions read `:LAST_SYNCED:` (push conflict logic) and which write it (`org-canvas-org-save-sync-state`).

- [ ] **Step 2: Write failing tests**

In `test/org-canvas-core-org-test.el`:

```elisp
(describe "file-level LAST_SYNCED"
  (it "writes #+LAST_SYNCED to the buffer header"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp (insert "#+TITLE: Pages\n* Page 1\n"))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :to-match
                      "^#\\+LAST_SYNCED: \\[[0-9-]+ \\w+ [0-9:]+\\]")))
        (delete-file temp))))

  (it "replaces an existing header instead of duplicating"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp
        (insert "#+TITLE: Pages\n#+LAST_SYNCED: [2025-01-01 Wed 00:00]\n* x\n"))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((count 0))
                (goto-char (point-min))
                (while (re-search-forward "^#\\+LAST_SYNCED:" nil t)
                  (cl-incf count))
                (expect count :to-equal 1))))
        (delete-file temp)))))
```

- [ ] **Step 3: Run; expect FAIL**

- [ ] **Step 4: Implement**

In `lisp/org-canvas-core-org.el`:

```elisp
(defun org-canvas--pull-write-file-header ()
  "Write or replace the #+LAST_SYNCED: header in the current buffer."
  (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+LAST_SYNCED:.*$" nil t)
          (replace-match (format "#+LAST_SYNCED: %s" timestamp) t t)
        ;; No existing header — insert after #+TITLE if present, else at top
        (goto-char (point-min))
        (if (re-search-forward "^#\\+TITLE:.*$" nil t)
            (progn (end-of-line) (insert "\n#+LAST_SYNCED: " timestamp))
          (goto-char (point-min))
          (insert "#+LAST_SYNCED: " timestamp "\n"))))))
```

- [ ] **Step 5: Strip per-entry write from `org-canvas-org-save-sync-state`**

```elisp
(defun org-canvas-org-save-sync-state (pom id &optional id-prop)
  "Standardize saving CANVAS_ID (or ID-PROP) to the heading at POM.
File-level LAST_SYNCED is handled by `org-canvas--pull-write-file-header'."
  (let ((prop (or id-prop "CANVAS_ID"))
        (id-str (org-canvas--normalize-id id)))
    (org-canvas-org-set-property pom prop id-str)))
```

- [ ] **Step 6: Add file-header write to every pull function**

For each module's pull entry point (ending with `save-buffer`), add `(org-canvas--pull-write-file-header)` immediately before the save:

```elisp
(org-canvas--pull-write-file-header)
(save-buffer)
```

Modules: pages, assignments, quizzes, modules, rubrics, files, discussions, announcements, calendar, group-categories, outcomes, sections, settings, new-quizzes.

- [ ] **Step 7: Update push-side conflict reader**

Find where push reads `:LAST_SYNCED:` for conflict diffs:

```bash
grep -n '"LAST_SYNCED"' lisp/*.el | grep -v test
```

Replace per-entry reads with a file-header reader. Add helper:

```elisp
(defun org-canvas--pull-read-file-header ()
  "Return the #+LAST_SYNCED timestamp string from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+LAST_SYNCED: \\(.+\\)$" nil t)
      (match-string-no-properties 1))))
```

- [ ] **Step 8: Update tests across modules that asserted per-entry `:LAST_SYNCED:`**

```bash
grep -n "LAST_SYNCED" test/*.el | grep -v ".elc"
```

For each test that asserted a per-entry property, change the assertion to assert (a) the file-header is present and (b) the per-entry property is **absent**.

- [ ] **Step 9: Run full gate**

Many tests will need updates here. Work through them module by module.

- [ ] **Step 10: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
feat(schema): file-level #+LAST_SYNCED header

Improvements item #7. Drop per-entry :LAST_SYNCED: properties; emit one
#+LAST_SYNCED file header per pulled org file. Push side reads the
header instead. This is a hard schema cutover — existing files become
invalid until re-pulled.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 8: Suppress registry-default booleans, gated by toggle (#6)

**Files:**
- Modify: `lisp/org-canvas-core-config.el` — add `org-canvas-emit-defaults`
- Modify: `lisp/org-canvas-core-sync.el` (or wherever pull emits properties from the registry)
- Test: per-module tests

- [ ] **Step 1: Add the defcustom**

In `lisp/org-canvas-core-config.el`:

```elisp
(defcustom org-canvas-emit-defaults nil
  "When non-nil, emit Org properties whose values match the registry default.
Default behavior (nil) suppresses these to keep drawers terse."
  :type 'boolean
  :group 'org-canvas)
```

- [ ] **Step 2: Locate the property emission code**

Find where pull functions iterate the registry to write properties. Likely in the macro `org-canvas-define-pull` or in a helper called from each pull module.

```bash
grep -n "org-canvas-register-properties\\|org-canvas--registry-get\\|pull-set-property" lisp/*.el | head -20
```

- [ ] **Step 3: Write a failing test**

In `test/org-canvas-property-registry-test.el` (or the central registry test file):

```elisp
(describe "default suppression"
  (it "omits a boolean property when its value equals the registry default"
    (let ((org-canvas-emit-defaults nil))
      ;; Assume registry has PEER_REVIEWS :type boolean :default nil
      (with-temp-org-buffer
       "* Heading\n"
       (org-canvas--pull-set-boolean-property
        (point) "PEER_REVIEWS" :json-false)  ; matches default
       (expect (org-entry-get (point) "PEER_REVIEWS") :to-be nil))))

  (it "emits when value differs from registry default"
    (let ((org-canvas-emit-defaults nil))
      (with-temp-org-buffer
       "* Heading\n"
       (org-canvas--pull-set-boolean-property
        (point) "PEER_REVIEWS" t)  ; differs from default nil
       (expect (org-entry-get (point) "PEER_REVIEWS") :to-equal "true"))))

  (it "emits everything when org-canvas-emit-defaults is non-nil"
    (let ((org-canvas-emit-defaults t))
      (with-temp-org-buffer
       "* Heading\n"
       (org-canvas--pull-set-boolean-property
        (point) "PEER_REVIEWS" :json-false)
       (expect (org-entry-get (point) "PEER_REVIEWS") :to-equal "false")))))
```

- [ ] **Step 4: Run; expect FAIL**

- [ ] **Step 5: Implement**

The registry lives in `org-canvas--property-registry` (hash table keyed by feature-name). Add an accessor that scans across features for a property name, and update the boolean setter to consult it:

```elisp
(defun org-canvas--registry-find-property (org-prop)
  "Scan `org-canvas--property-registry' for a spec with :org-prop = ORG-PROP.
Returns the property spec plist, or nil. First match wins."
  (catch 'found
    (maphash
     (lambda (_feature feature-plist)
       (dolist (spec (plist-get feature-plist :properties))
         (when (string= (plist-get spec :org-prop) org-prop)
           (throw 'found spec))))
     org-canvas--property-registry)
    nil))

(defun org-canvas--pull-set-boolean-property (pos prop value)
  "Set PROP to VALUE at POS, suppressing default values per registry.
VALUE is t, :json-false, nil, or a string."
  (let* ((spec (org-canvas--registry-find-property prop))
         (default (plist-get spec :default))
         (normalized (cond ((eq value :json-false) nil)
                           ((eq value t) t)
                           ((stringp value)
                            (cond ((string= value "true") t)
                                  ((string= value "false") nil)
                                  (t value)))
                           (t value))))
    (when (or org-canvas-emit-defaults
              (not (eq (and normalized t) (and default t))))
      (org-canvas-org-set-property pos prop
                                   (if normalized "true" "false")))))
```

(The `(and normalized t)` / `(and default t)` coercion turns nil and any non-t non-nil into a clean nil/t for `eq` comparison so :json-false vs nil in the registry default doesn't cause a false mismatch.)

- [ ] **Step 6: Run tests; expect PASS for all three**

- [ ] **Step 7: Audit module tests for affected assertions**

Many existing tests assert `:PEER_REVIEWS: false` in pulled output. Update them: when the value matches default, the property should be **absent**.

- [ ] **Step 8: Run full gate**

- [ ] **Step 9: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
feat(schema): suppress boolean properties matching registry default

Improvements item #6. Properties declared with :type boolean and
:default in the property registry are now omitted on pull when the
pulled value matches the registered default. Set
org-canvas-emit-defaults=t to dump everything (debug aid). Push reads
absent props as the registry default already, so this is a pull-side
change with no push update required.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 9: Rubric child-heading-per-criterion format (#12)

**Files:**
- Modify: `lisp/org-canvas-rubrics.el` — pull emit and push parse both
- Test: `test/org-canvas-rubrics-test.el`

The current flat-table-with-`>`-prefix format is replaced with: one child heading per criterion, points in the trailing tag, ratings as a sub-table.

- [ ] **Step 1: Read the existing rubric pull/push**

```bash
grep -n "defun org-canvas--rubric" lisp/org-canvas-rubrics.el
```

Identify:
- pull emit function (writes the flat table)
- parse function (reads it for push)
- the data shape: criteria array, each with ratings array

- [ ] **Step 2: Write failing tests for pull**

In `test/org-canvas-rubrics-test.el`:

```elisp
(describe "rubric pull (new format)"
  (it "emits a child heading per criterion with a sub-table of ratings"
    (let ((rubric '((id . 100)
                    (title . "Test Rubric")
                    (data . [((id . "c1")
                              (description . "Critical thinking")
                              (points . 5.0)
                              (ratings . [((description . "Full Marks") (points . 5.0))
                                          ((description . "Partial") (points . 3.0))
                                          ((description . "No Marks") (points . 0.0))]))]))))
      (with-temp-org-buffer
       ""
       (org-canvas--rubric-pull-item rubric)
       (let ((output (buffer-string)))
         (expect output :to-match "^\\* Test Rubric")
         (expect output :to-match "^\\*\\* Critical thinking[ \t]+:5pt:")
         (expect output :to-match "| Rating[ ]+| Points[ ]+| Description |")
         (expect output :to-match "| Full Marks[ ]+|[ ]+5\\.0[ ]+|")
         (expect output :to-match "| Partial[ ]+|[ ]+3\\.0[ ]+|")
         (expect output :to-match "| No Marks[ ]+|[ ]+0\\.0[ ]+|"))))))
```

- [ ] **Step 3: Write failing tests for push parse round-trip**

```elisp
(describe "rubric push parse (new format)"
  (it "parses criterion heading + sub-table into the data shape"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:CANVAS_ID: 100
:END:
** Critical thinking                                                  :5pt:
| Rating     | Points | Description |
|------------+--------+-------------|
| Full Marks |    5.0 |             |
| Partial    |    3.0 |             |
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((data (org-canvas--rubric-parse-entry)))
       (let ((criteria (plist-get data :criteria)))
         (expect (length criteria) :to-equal 1)
         (let ((c (car criteria)))
           (expect (plist-get c :description) :to-equal "Critical thinking")
           (expect (plist-get c :points) :to-equal 5.0)
           (expect (length (plist-get c :ratings)) :to-equal 2)))))))
```

- [ ] **Step 4: Run tests; expect FAIL**

- [ ] **Step 5: Implement pull emit**

Replace the flat-table emitter with:

```elisp
(defun org-canvas--rubric-emit-criterion (criterion)
  "Emit one CRITERION as a child heading + ratings sub-table."
  (let* ((desc (alist-get 'description criterion))
         (pts (alist-get 'points criterion))
         (ratings (alist-get 'ratings criterion)))
    (insert (format "** %s :%spt:\n" desc (org-canvas--format-number pts)))
    (insert "| Rating | Points | Description |\n")
    (insert "|--------+--------+-------------|\n")
    (dolist (r (append ratings nil))
      (insert (format "| %s | %s | %s |\n"
                      (alist-get 'description r)
                      (org-canvas--format-number (alist-get 'points r))
                      (or (alist-get 'long_description r) ""))))
    (insert "\n")))

(defun org-canvas--rubric-pull-item (rubric)
  "Emit one RUBRIC at point."
  (insert (format "* %s\n" (alist-get 'title rubric)))
  (insert ":PROPERTIES:\n")
  (insert (format ":CANVAS_ID: %s\n" (alist-get 'id rubric)))
  (insert ":END:\n")
  (let ((criteria (alist-get 'data rubric)))
    (dolist (c (append criteria nil))
      (org-canvas--rubric-emit-criterion c))))
```

- [ ] **Step 6: Implement push parse for the new format**

Replace the flat-table parser with: walk child headings (level=2 under the rubric heading), for each, read the sub-table.

```elisp
(defun org-canvas--rubric-parse-criterion-at-point ()
  "Parse a level-2 criterion heading + its sub-table.
Returns a plist (:description :points :ratings)."
  (org-back-to-heading t)
  (let* ((heading (org-get-heading t t t t))
         (tags (org-get-tags))
         (points-tag (cl-find-if (lambda (tg) (string-match "\\([0-9.]+\\)pt" tg)) tags))
         (points (and points-tag
                      (string-to-number (replace-regexp-in-string "pt$" "" points-tag))))
         (ratings nil)
         (subtree-end (save-excursion (org-end-of-subtree t t))))
    (save-excursion
      (forward-line 1)
      (when (re-search-forward "^|" subtree-end t)
        (org-table-goto-line 2)  ; skip header row
        (forward-line 1)         ; skip separator
        (while (and (looking-at "^|") (< (point) subtree-end))
          (let* ((row (org-table-current-line))
                 (rating (org-trim (org-table-get nil 1)))
                 (rating-points (string-to-number (org-table-get nil 2)))
                 (rating-desc (org-trim (org-table-get nil 3))))
            (push (list :description rating
                        :points rating-points
                        :long_description rating-desc)
                  ratings))
          (forward-line 1))))
    (list :description heading :points points :ratings (nreverse ratings))))

(defun org-canvas--rubric-parse-entry ()
  "Parse the rubric subtree at point into the API payload shape."
  (org-back-to-heading t)
  (let* ((title (org-get-heading t t t t))
         (criteria nil)
         (rubric-end (save-excursion (org-end-of-subtree t t))))
    (save-excursion
      (forward-line 1)
      (while (re-search-forward "^\\*\\* " rubric-end t)
        (push (org-canvas--rubric-parse-criterion-at-point) criteria)
        (org-end-of-subtree t t)))
    (list :title title
          :criteria (nreverse criteria))))
```

- [ ] **Step 7: Run all rubric tests**

```bash
eldev test rubrics
```

Expected: all PASS. Update any existing tests that asserted the flat-table format.

- [ ] **Step 8: Run full gate**

- [ ] **Step 9: Commit**

```bash
git add lisp/org-canvas-rubrics.el test/org-canvas-rubrics-test.el
git commit -m "$(cat <<'EOF'
feat(rubrics): child heading per criterion + ratings sub-table

Improvements item #12. Replace the flat table with `>`-prefixed rows
with a structured layout: one level-2 heading per criterion (points in
the trailing tag), ratings as a per-criterion sub-table. Both pull
emit and push parse updated.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 10: `** Description` subheading where descriptions coexist with structured children (#13)

**Files:**
- Modify: `lisp/org-canvas-quizzes.el` (description + questions)
- Modify: `lisp/org-canvas-rubrics.el` (description + criteria — only if Canvas rubrics carry a description; otherwise skip)
- Modify: any other module emitting body+children
- Test: per-module tests

- [ ] **Step 1: Identify modules with body+structured-children**

Quizzes are the explicit case. Check rubrics, modules (description on the module itself with items below), assignments-with-overrides.

- [ ] **Step 2: Write failing test for quizzes**

In `test/org-canvas-quizzes-test.el`:

```elisp
(describe "quiz description boundary"
  (it "wraps body in ** Description when questions follow"
    (let ((quiz '((id . 100)
                  (title . "Midterm")
                  (description . "<p>Read carefully.</p>"))))
      (with-temp-org-buffer
       ""
       (org-canvas--quiz-pull-item quiz '(((id . 1) (question_text . "Q1"))))
       (let ((output (buffer-string)))
         (expect output :to-match "^\\* Midterm")
         (expect output :to-match "^\\*\\* Description")
         (expect output :to-match "Read carefully")
         (expect output :to-match "^\\*\\* Question")))))

  (it "emits body inline when no children follow"
    (let ((quiz '((id . 101)
                  (title . "Easy quiz")
                  (description . "<p>Just go.</p>"))))
      (with-temp-org-buffer
       ""
       (org-canvas--quiz-pull-item quiz nil)
       (let ((output (buffer-string)))
         (expect output :not :to-match "^\\*\\* Description")
         (expect output :to-match "Just go"))))))
```

- [ ] **Step 3: Run; expect FAIL**

- [ ] **Step 4: Implement**

In the quiz pull-item emitter:

```elisp
(defun org-canvas--quiz-pull-item (quiz questions)
  "Emit QUIZ heading + body + question subheadings."
  (insert (format "* %s\n" (alist-get 'title quiz)))
  (insert ":PROPERTIES:\n")
  (insert (format ":CANVAS_ID: %s\n" (alist-get 'id quiz)))
  (insert ":END:\n")
  (let ((body-html (alist-get 'description quiz))
        (have-children (and questions (not (= 0 (length questions))))))
    (when (and body-html (not (string-empty-p body-html)))
      (when have-children
        (insert "** Description\n"))
      (let ((body-text (org-canvas--html-to-org body-html)))
        (insert body-text)
        (unless (string-suffix-p "\n" body-text) (insert "\n"))))
    (when have-children
      (dolist (q (append questions nil))
        (org-canvas--quiz-emit-question q)))))
```

- [ ] **Step 5: Update push parse to skip the `** Description` heading**

In the quiz parse function, when reading body content, check whether the first child heading is `** Description` — if so, the body is its content; if not, the body is the parent's content.

```elisp
(defun org-canvas--quiz-parse-body ()
  "Return the description body for the quiz heading at point."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
      (forward-line 1)
      ;; Skip property drawer
      (when (looking-at "^:PROPERTIES:")
        (re-search-forward "^:END:$" nil t)
        (forward-line 1))
      (cond
       ;; Has ** Description child
       ((re-search-forward "^\\*\\* Description$" subtree-end t)
        (forward-line 1)
        (let ((desc-end (save-excursion (org-end-of-subtree t t))))
          (org-canvas--html-from-region (point) desc-end)))
       ;; No structured children: read until next heading
       (t
        (let ((body-end (save-excursion
                          (if (re-search-forward "^\\*+ " subtree-end t)
                              (line-beginning-position)
                            subtree-end))))
          (org-canvas--html-from-region (point) body-end)))))))
```

- [ ] **Step 6: Run; expect PASS for both new quiz tests**

- [ ] **Step 7: Run full gate**

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-quizzes.el test/org-canvas-quizzes-test.el
git commit -m "$(cat <<'EOF'
feat(quizzes): wrap description in ** Description when questions follow

Improvements item #13. Quizzes that have both a description body and
questions emit a level-2 ** Description heading wrapping the body so
the boundary is unambiguous. Quizzes with no questions keep the
inline body. Push parse handles both shapes.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 11: Module item `:ITEM_TYPE:` property (#10)

**Files:**
- Modify: `lisp/org-canvas-modules.el` — pull emits `:ITEM_TYPE:`; push reads it for type dispatch instead of guessing from link target
- Test: `test/org-canvas-modules-test.el`

- [ ] **Step 1: Locate module item emit and parse**

```bash
grep -n "module-item\\|module-pull-insert\\|SubHeader\\|ItemType" lisp/org-canvas-modules.el
```

- [ ] **Step 2: Failing test — pull emits `:ITEM_TYPE:`**

```elisp
(describe "module item ITEM_TYPE"
  (it "emits :ITEM_TYPE: SubHeader for SubHeader items"
    (with-temp-org-buffer
     ""
     (org-canvas--module-pull-insert-subheader
      '((id . 1) (type . "SubHeader") (title . "Week 1")))
     (expect (buffer-string) :to-match ":ITEM_TYPE: SubHeader")))

  (it "emits :ITEM_TYPE: Page for Page items"
    (with-temp-org-buffer
     ""
     (org-canvas--module-pull-insert-content-item
      '((id . 2) (type . "Page") (title . "Reading 1") (page_url . "reading-1")))
     (expect (buffer-string) :to-match ":ITEM_TYPE: Page"))))
```

- [ ] **Step 3: Run; expect FAIL**

- [ ] **Step 4: Implement**

In each emit helper, after the property drawer's `:END:`, insert:

```elisp
(insert (format ":ITEM_TYPE: %s\n" (alist-get 'type item)))
```

In a single helper called by both, where `item` is the API alist.

- [ ] **Step 5: Update push parse**

The current parse infers type from the heading body (`[[file:...]]` → File, `[[...assignments.org::...]]` → Assignment, etc.). Replace with a direct read of `:ITEM_TYPE:`, falling back to inference only when the property is absent (back-compat for hand-edited files).

- [ ] **Step 6: Run; expect PASS**

- [ ] **Step 7: Run full gate**

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-modules.el test/org-canvas-modules-test.el
git commit -m "$(cat <<'EOF'
feat(modules): emit :ITEM_TYPE: on module item drawers

Improvements item #10. Canvas distinguishes SubHeader from regular
items, but the previous heuristic-based inference (link target shape)
collapsed them. Emit :ITEM_TYPE: on every module item; push reads it
directly with the inference path retained as fallback.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 12: Suppress `:INDENT:` when zero (#8)

**Files:**
- Modify: `lisp/org-canvas-modules.el`
- Test: `test/org-canvas-modules-test.el`

- [ ] **Step 1: Failing test**

```elisp
(it "omits :INDENT: when value is zero"
  (with-temp-org-buffer
   ""
   (org-canvas--module-pull-insert-content-item
    '((id . 1) (type . "Page") (title . "x") (page_url . "x") (indent . 0)))
   (expect (buffer-string) :not :to-match ":INDENT:")))

(it "emits :INDENT: when value is nonzero"
  (with-temp-org-buffer
   ""
   (org-canvas--module-pull-insert-content-item
    '((id . 1) (type . "Page") (title . "x") (page_url . "x") (indent . 2)))
   (expect (buffer-string) :to-match ":INDENT: 2")))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Implement**

Find where `:INDENT:` is emitted; wrap in `(when (and indent (> indent 0)) ...)`.

- [ ] **Step 4: Run; PASS**

- [ ] **Step 5: Run full gate**

- [ ] **Step 6: Commit**

```bash
git add lisp/org-canvas-modules.el test/org-canvas-modules-test.el
git commit -m "$(cat <<'EOF'
feat(modules): omit :INDENT: when zero

Improvements item #8. Stop emitting :INDENT: 0 on every flat module
item. Indent only appears when nonzero.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 13: Sort by `position` on pull (#9)

**Files:**
- Modify: `lisp/org-canvas-core-sync.el` (or per-module pull functions) — add a sort step before emit
- Test: per-module tests

- [ ] **Step 1: Add a sorting helper**

In `lisp/org-canvas-core-sync.el`:

```elisp
(defun org-canvas--pull-sort-items (items &optional secondary-key)
  "Return ITEMS sorted by position (then SECONDARY-KEY if provided), then name, then id.
ITEMS is a list of alists from a Canvas API response."
  (sort (copy-sequence items)
        (lambda (a b)
          (let ((apos (alist-get 'position a))
                (bpos (alist-get 'position b)))
            (cond
             ((and apos bpos (/= apos bpos)) (< apos bpos))
             ;; Secondary key (e.g. assignment_group_id for assignments)
             ((and secondary-key
                   (let ((av (alist-get secondary-key a))
                         (bv (alist-get secondary-key b)))
                     (and av bv (/= av bv))))
              (< (alist-get secondary-key a) (alist-get secondary-key b)))
             ((and (alist-get 'name a) (alist-get 'name b))
              (string< (alist-get 'name a) (alist-get 'name b)))
             (t (< (or (alist-get 'id a) 0) (or (alist-get 'id b) 0))))))))
```

- [ ] **Step 2: Write failing test**

```elisp
(it "sorts items by position before emitting"
  (let ((items '(((id . 3) (position . 30) (title . "C"))
                 ((id . 1) (position . 10) (title . "A"))
                 ((id . 2) (position . 20) (title . "B")))))
    (let ((sorted (org-canvas--pull-sort-items items)))
      (expect (mapcar (lambda (x) (alist-get 'title x)) sorted)
              :to-equal '("A" "B" "C")))))

(it "uses secondary key when positions tie"
  (let ((items '(((id . 1) (position . 1) (assignment_group_id . 200) (title . "B"))
                 ((id . 2) (position . 1) (assignment_group_id . 100) (title . "A")))))
    (let ((sorted (org-canvas--pull-sort-items items 'assignment_group_id)))
      (expect (mapcar (lambda (x) (alist-get 'title x)) sorted)
              :to-equal '("A" "B")))))
```

- [ ] **Step 3: Run; FAIL**

- [ ] **Step 4: Implement (already shown in Step 1)**

- [ ] **Step 5: Wire into each pull function**

In each module's pull, replace the iterate-as-fetched pattern with:

```elisp
(let ((sorted-items (org-canvas--pull-sort-items raw-items 'assignment_group_id)))
  (dolist (item sorted-items)
    ...))
```

For assignments: pass `'assignment_group_id` so groups stay together. Other modules: just `position`.

- [ ] **Step 6: Run; PASS**

- [ ] **Step 7: Run full gate**

- [ ] **Step 8: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
feat(pull): sort items by position before emit

Improvements item #9. Pull functions now sort fetched items by
position (with assignment_group_id as secondary key for assignments)
before emitting them. This puts the org file in pedagogical order
instead of database-id order.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

# Wave 4: Content Fidelity

## Task 14: Course image download (#3)

**Files:**
- Modify: `lisp/org-canvas-settings.el` — pull downloads the image, stores local path
- Test: `test/org-canvas-settings-test.el`

- [ ] **Step 1: Failing test**

```elisp
(it "downloads course image into content/course_image and stores local path"
  (cl-letf (((symbol-function 'org-canvas--file-download-to)
             (lambda (_url path)
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert "image bytes")))))
    (let* ((response '((id . 1) (name . "Test")
                       (image_url . "https://x.com/files/img.jpg?token=ABC&exp=999")))
           (org-canvas-directory (make-temp-file "test-" t)))
      (unwind-protect
          (let ((local-path (org-canvas--settings-pull-course-image response)))
            (expect local-path :to-match "content/course_image/img\\.jpg")
            (expect (file-exists-p (expand-file-name local-path org-canvas-directory))
                    :to-be t))
        (delete-directory org-canvas-directory t)))))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Implement**

```elisp
(defun org-canvas--settings-pull-course-image (course-response)
  "Download the course image referenced in COURSE-RESPONSE.
Return the relative path under `org-canvas-directory', or nil if no image."
  (when-let* ((url (alist-get 'image_url course-response))
              (basename (org-canvas--url-basename url))
              (rel-path (concat "content/course_image/" basename))
              (abs-path (expand-file-name rel-path org-canvas-directory)))
    (org-canvas--file-download-to url abs-path)
    rel-path))

(defun org-canvas--url-basename (url)
  "Extract a clean basename from URL (drop query string)."
  (let ((path (car (split-string url "[?#]"))))
    (file-name-nondirectory path)))
```

Wire into the settings emit so `:COURSE_IMAGE:` writes the local path, not the signed URL.

- [ ] **Step 4: Run; PASS**

- [ ] **Step 5: Run full gate**

- [ ] **Step 6: Commit**

```bash
git add lisp/org-canvas-settings.el test/org-canvas-settings-test.el
git commit -m "$(cat <<'EOF'
feat(settings): download course image, store local path

Improvements item #3. Stop committing the signed Canvas course image
URL to settings.org. Download the image into
content/course_image/<basename>, store the relative path.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 15: Localize timestamps to course TZ (#11)

**Files:**
- Modify: `lisp/org-canvas-core-org.el` — add TZ resolver and timestamp formatter
- Modify: pull functions that emit timestamps
- Test: `test/org-canvas-core-org-test.el`

- [ ] **Step 1: Failing test**

```elisp
(describe "TZ-aware pull timestamp"
  (it "converts UTC ISO-8601 to America/New_York Org timestamp"
    (let ((org-canvas--pull-tz-cache "America/New_York"))
      (expect (org-canvas--pull-localize-timestamp "2026-04-04T03:59:00Z")
              :to-equal "<2026-04-03 Fri 23:59>")))

  (it "falls back to system TZ when cache is nil"
    (let ((org-canvas--pull-tz-cache nil))
      ;; Just assert it returns a non-nil Org timestamp; exact value depends on host
      (expect (org-canvas--pull-localize-timestamp "2026-04-04T03:59:00Z")
              :to-match "^<[0-9]"))))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Implement**

```elisp
(defvar org-canvas--pull-tz-cache nil
  "Resolved course timezone for the current pull session.
nil means fall back to system TZ.")

(defun org-canvas--pull-resolve-tz ()
  "Resolve the course TZ from settings.org and cache it.
Sets `org-canvas--pull-tz-cache' to the IANA TZ string or nil."
  (setq org-canvas--pull-tz-cache
        (let ((settings-file org-canvas-settings-file))
          (when (file-exists-p settings-file)
            (with-current-buffer (find-file-noselect settings-file)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^:TIME_ZONE: \\(.+\\)$" nil t)
                  (string-trim (match-string-no-properties 1)))))))))

(defun org-canvas--pull-localize-timestamp (utc-iso8601)
  "Convert UTC-ISO8601 to an Org active timestamp in the cached course TZ.
Falls back to system TZ when cache is nil."
  (when (and utc-iso8601 (stringp utc-iso8601) (not (string-empty-p utc-iso8601)))
    (let* ((parsed (date-to-time utc-iso8601))
           (target-tz (or org-canvas--pull-tz-cache nil)))
      (format-time-string "<%Y-%m-%d %a %H:%M>" parsed target-tz))))
```

- [ ] **Step 4: Wire into pull-all**

Call `(org-canvas--pull-resolve-tz)` after settings.org is pulled (or at the very start, reading existing settings.org if present).

- [ ] **Step 5: Migrate existing timestamp emitters**

Find every `format-time-string` call that emits an Org timestamp from a Canvas response. Replace with `org-canvas--pull-localize-timestamp`.

```bash
grep -n "format-time-string.*<%Y\\|encode-time.*org-parse" lisp/*.el | grep -v test
```

- [ ] **Step 6: Run; PASS for new tests; existing tests may need update**

For the timestamp tests that were timezone-independent (using `set-time-zone-rule "UTC"`), update them to set `org-canvas--pull-tz-cache` to the expected TZ.

- [ ] **Step 7: Run full gate**

- [ ] **Step 8: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
feat(pull): localize timestamps to course timezone

Improvements item #11. UTC ISO-8601 timestamps from Canvas now convert
to the course timezone (read from settings.org :TIME_ZONE:) before
emitting Org active timestamps. Falls back to system TZ when settings
hasn't been pulled. Push converts back to UTC on send.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 16: Section overrides pull (#14)

**Files:**
- Modify: `lisp/org-canvas-assignments.el` — add per-assignment overrides fetch + emit
- Test: `test/org-canvas-assignments-test.el`

The push side already reads `#+NAME: overrides` tables. Pull adds the symmetric emit.

- [ ] **Step 1: Failing test**

```elisp
(describe "assignment override pull"
  (it "emits #+NAME: overrides table when assignment has overrides"
    (let* ((assignment '((id . 678) (name . "Lab 1") (due_at . "2026-02-15T23:59:00Z")))
           (overrides '(((id . 5001)
                         (course_section_id . 296338)
                         (due_at . "2026-02-22T23:59:00Z")))))
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method url &rest _)
                   (when (string-match-p "/overrides" url)
                     overrides)))
                ((symbol-function 'org-canvas--section-link-by-id)
                 (lambda (_id) "[[file:sections.org::*S2601-CPSC-6300-001-15179][6300]]")))
        (with-temp-org-buffer
         ""
         (org-canvas--assignment-pull-item assignment)
         (let ((output (buffer-string)))
           (expect output :to-match "#\\+NAME: overrides")
           (expect output :to-match "S2601-CPSC-6300-001-15179")
           (expect output :to-match "<2026-02-22"))))))

  (it "emits no table when assignment has no overrides"
    (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
               (lambda (&rest _) (vector))))
      (with-temp-org-buffer
       ""
       (org-canvas--assignment-pull-item
        '((id . 700) (name . "Solo") (due_at . "2026-02-15T23:59:00Z")))
       (expect (buffer-string) :not :to-match "#\\+NAME: overrides")))))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Implement section ID → link helper**

```elisp
(defun org-canvas--section-link-by-id (section-id)
  "Return an Org file link to the section heading with CANVAS_ID = SECTION-ID.
Returns the section name as plain text if no link can be resolved."
  (let ((sections-file org-canvas-sections-file))
    (if (and sections-file (file-exists-p sections-file))
        (with-current-buffer (find-file-noselect sections-file)
          (save-excursion
            (goto-char (point-min))
            (let ((target (number-to-string section-id))
                  found name)
              (while (and (not found)
                          (re-search-forward "^:CANVAS_ID: \\(.+\\)$" nil t))
                (when (string= (string-trim (match-string-no-properties 1)) target)
                  (save-excursion
                    (org-back-to-heading t)
                    (setq name (org-get-heading t t t t))
                    (setq found t))))
              (if found
                  (format "[[file:%s::*%s][%s]]"
                          (file-name-nondirectory sections-file) name name)
                target))))
      (number-to-string section-id))))
```

- [ ] **Step 4: Implement the override fetch + emit**

```elisp
(defun org-canvas--assignment-fetch-overrides (assignment-id)
  "GET overrides for ASSIGNMENT-ID; return the list (vector or nil)."
  (let ((url (org-canvas-api-course-endpoint
              (format "assignments/%s/overrides" assignment-id))))
    (condition-case err
        (org-canvas-api-request-all-pages 'GET url)
      (org-canvas-api-error
       (org-canvas--pull-summary-record
        :file (file-name-nondirectory org-canvas-assignments-file)
        :item (format "assignment %s overrides" assignment-id)
        :error (error-message-string err))
       nil))))

(defun org-canvas--assignment-emit-overrides (overrides)
  "Emit a #+NAME: overrides table for OVERRIDES (alist list)."
  (when (and overrides (> (length overrides) 0))
    (insert "#+NAME: overrides\n")
    (insert "| Section | DUE_AT | UNLOCK_AT | LOCK_AT |\n")
    (insert "|---------+--------+-----------+---------|\n")
    (dolist (ov (append overrides nil))
      (let ((sec-link (org-canvas--section-link-by-id
                       (alist-get 'course_section_id ov)))
            (due (or (org-canvas--pull-localize-timestamp (alist-get 'due_at ov)) ""))
            (unlock (or (org-canvas--pull-localize-timestamp (alist-get 'unlock_at ov)) ""))
            (lock (or (org-canvas--pull-localize-timestamp (alist-get 'lock_at ov)) "")))
        (insert (format "| %s | %s | %s | %s |\n" sec-link due unlock lock))))
    (insert "\n")))
```

Call `(org-canvas--assignment-emit-overrides (org-canvas--assignment-fetch-overrides id))` from the assignment pull-item, after the property drawer and before the body.

- [ ] **Step 5: Run; PASS**

- [ ] **Step 6: Run full gate**

- [ ] **Step 7: Commit**

```bash
git add lisp/org-canvas-assignments.el test/org-canvas-assignments-test.el
git commit -m "$(cat <<'EOF'
feat(assignments): pull section overrides as #+NAME: overrides table

Improvements item #14. Assignments with section-specific date
overrides (the cross-listed-course case) now emit the
#+NAME: overrides table that the existing push pipeline already
reads. Section column resolves to a file: link into sections.org;
empty override sets emit no table.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 17: Announcement metadata (#15)

**Files:**
- Modify: `lisp/org-canvas-announcements.el` — emit POSTED_AT, AUTHOR, DELAYED_POST_AT
- Modify: `lisp/org-canvas-core-config.el` if registry needs new property declarations
- Test: `test/org-canvas-announcements-test.el`

- [ ] **Step 1: Failing test**

```elisp
(it "emits POSTED_AT, AUTHOR, DELAYED_POST_AT properties"
  (let ((announcement '((id . 100)
                        (title . "Heads up")
                        (message . "<p>x</p>")
                        (posted_at . "2026-04-01T15:00:00Z")
                        (delayed_post_at . "2026-04-02T08:00:00Z")
                        (user . ((display_name . "Tim Ransom"))))))
    (with-temp-org-buffer
     ""
     (org-canvas--announcement-pull-item announcement)
     (let ((output (buffer-string)))
       (expect output :to-match ":POSTED_AT:")
       (expect output :to-match ":AUTHOR: Tim Ransom")
       (expect output :to-match ":DELAYED_POST_AT:")))))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Implement**

In the announcement emit function, after CANVAS_ID, set:

```elisp
(when-let ((posted (alist-get 'posted_at item)))
  (org-canvas-org-set-property pos "POSTED_AT"
                                (org-canvas--pull-localize-timestamp posted)))
(when-let ((author (alist-get 'display_name (alist-get 'user item))))
  (org-canvas-org-set-property pos "AUTHOR" author))
(when-let ((delayed (alist-get 'delayed_post_at item)))
  (org-canvas-org-set-property pos "DELAYED_POST_AT"
                                (org-canvas--pull-localize-timestamp delayed)))
```

Add registry entries (no `:default`, since these are timestamps/strings).

- [ ] **Step 4: Run; PASS**

- [ ] **Step 5: Run full gate**

- [ ] **Step 6: Commit**

```bash
git add lisp/org-canvas-announcements.el test/org-canvas-announcements-test.el
git commit -m "$(cat <<'EOF'
feat(announcements): emit POSTED_AT, AUTHOR, DELAYED_POST_AT

Improvements item #15. Announcement headings carry the date posted,
author, and scheduled post date so the org file is the authoritative
record without a Canvas trip.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 18: Quiz-question emission bug (#5)

**Files:**
- Investigate: `lisp/org-canvas-quizzes.el` — log shows all 15 question endpoints succeeded but only one quiz emitted Question subheadings
- Test: regression test

- [ ] **Step 1: Reproduce locally**

Run a single-quiz pull on the course; check that questions are emitted. If only one quiz emits, the bug is reproducible. If all emit, the issue may be data-shape (e.g., the live course has only one quiz with questions).

```bash
cd /mnt/b410f78a-9927-453b-b3b6-4b3910634741/Code/org-canvas
emacs --batch -L lisp -l org-canvas \
  --eval "(setq org-canvas-directory \"/tmp/quiz-test\")" \
  --eval "(org-canvas-pull-quizzes)"
```

Compare output to `canvas-structure/quizzes.org`.

- [ ] **Step 2: Trace the question-emission path**

```bash
grep -n "questions\\|--quiz-emit-question\\|sync-quiz-questions" lisp/org-canvas-quizzes.el
```

Likely candidates for the bug:
- A guard that checks `(> (length questions) 0)` failing because `questions` is `nil` instead of `[]`.
- A buffer-modification side-effect from emitting one quiz's questions that prevents subsequent emits (mark-shifting; see CLAUDE.md "Collect Markers Before Iterating").
- A condition-case swallowing an error after the first quiz emit.

Read `lisp/org-canvas-quizzes.el` lines 800–870 carefully.

- [ ] **Step 3: Write a regression test for the suspected cause**

Once the cause is identified, add a buttercup `it` block that exercises the path with multiple quizzes-with-questions and asserts all emit.

- [ ] **Step 4: Implement the fix**

If it's the marker-shifting issue, follow the CLAUDE.md pattern: collect markers before mutating.

- [ ] **Step 5: Run; PASS**

- [ ] **Step 6: Run full gate**

- [ ] **Step 7: Commit**

```bash
git add lisp/org-canvas-quizzes.el test/org-canvas-quizzes-test.el
git commit -m "$(cat <<'EOF'
fix(quizzes): emit questions for every quiz, not just the first

Improvements item #5. The pull log showed all 15 quiz /questions
endpoints succeeded, but only one quiz emitted Question subheadings.
[Fill in root cause once identified during investigation.]

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

(Update the commit message body once the root cause is identified.)

---

# Wave 5: Polish

## Task 19: Empty-file self-doc headers (#4 + #18)

**Files:**
- Modify: `lisp/org-canvas-core-org.el` — add `org-canvas--pull-emit-empty-file`
- Modify: outcomes, calendar, discussions, group-categories, new-quizzes pull functions
- Test: per-module tests

- [ ] **Step 1: Failing test**

```elisp
(describe "empty file pull header"
  (it "writes #+TITLE and a 0-items comment"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (unwind-protect
          (progn
            (org-canvas--pull-emit-empty-file temp "Discussions")
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((s (buffer-string)))
                (expect s :to-match "^#\\+TITLE: Discussions$")
                (expect s :to-match "^#\\+LAST_SYNCED: \\[")
                (expect s :to-match "^# Canvas returned 0 items"))))
        (delete-file temp)))))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Implement**

```elisp
(defun org-canvas--pull-emit-empty-file (path label)
  "Write an empty-file self-documenting header to PATH for LABEL."
  (with-temp-file path
    (insert (format "#+TITLE: %s\n" label))
    (insert (format "#+LAST_SYNCED: %s\n"
                    (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (insert "# Canvas returned 0 items at this sync.\n")))
```

- [ ] **Step 4: Wire into each module**

In each pull function, after fetching the list, add an early return:

```elisp
(if (zerop (length items))
    (org-canvas--pull-emit-empty-file org-canvas-discussions-file "Discussions")
  ;; ... existing emit path
  )
```

- [ ] **Step 5: Run; PASS**

- [ ] **Step 6: Run full gate**

- [ ] **Step 7: Commit**

```bash
git add lisp/ test/
git commit -m "$(cat <<'EOF'
feat(pull): self-doc header for empty-list endpoints

Improvements items #4 and #18. Endpoints that return zero items now
write a self-documenting header (#+TITLE, #+LAST_SYNCED, '# Canvas
returned 0 items') instead of a blank file. Affects new-quizzes,
outcomes, calendar, discussions, group-categories.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 20: Files duplicate detection (#19)

**Files:**
- Modify: `lisp/org-canvas-files.el` — log a warning when (size, content_type, base-name-stripped) collide
- Test: `test/org-canvas-files-test.el`

- [ ] **Step 1: Failing test**

```elisp
(it "logs a warning for two files with same size+content_type"
  (let* ((files '(((id . 1) (display_name . "foo.pdf")
                   (size . 100) (content-type . "application/pdf"))
                  ((id . 2) (display_name . "foo-1.pdf")
                   (size . 100) (content-type . "application/pdf"))))
         (warnings nil))
    (cl-letf (((symbol-function 'org-canvas--log-warning)
               (lambda (_logger fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      (org-canvas--file-detect-duplicates files)
      (expect (cl-some (lambda (w) (string-match-p "duplicate" w)) warnings)
              :to-be t))))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Implement**

```elisp
(defun org-canvas--file-detect-duplicates (items)
  "Warn for ITEMS sharing (size, content-type)."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (item (append items nil))
      (let ((key (list (alist-get 'size item) (alist-get 'content-type item))))
        (push item (gethash key groups))))
    (maphash
     (lambda (key entries)
       (when (> (length entries) 1)
         (org-canvas--log-warning org-canvas--logger
           "[Files] Possible duplicate group (size=%s type=%s): %s"
           (car key) (cadr key)
           (mapconcat (lambda (e) (alist-get 'display_name e)) entries ", "))))
     groups)))
```

Call it from `org-canvas-pull-files` after the file list is fetched.

- [ ] **Step 4: Run; PASS**

- [ ] **Step 5: Run full gate**

- [ ] **Step 6: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): warn on (size, content_type) duplicate groups

Improvements item #19. When pull-files fetches the file list, log a
warning for each group of >1 file sharing size and content-type so
the user can clean them up in Canvas.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 21: `:LICENSE:` double-space fix (#17)

**Files:**
- Modify: wherever `LICENSE` is emitted in `lisp/org-canvas-settings.el`
- Test: `test/org-canvas-settings-test.el`

- [ ] **Step 1: Failing test**

```elisp
(it "emits :LICENSE: with single space"
  (with-temp-org-buffer
   ""
   (org-canvas--settings-emit-license "private")
   (expect (buffer-string) :to-match "^:LICENSE: private$")
   (expect (buffer-string) :not :to-match ":LICENSE:  ")))
```

- [ ] **Step 2: Run; FAIL**

- [ ] **Step 3: Find the formatter**

```bash
grep -n "LICENSE" lisp/org-canvas-settings.el
```

Replace any `format ":LICENSE:  %s"` with `format ":LICENSE: %s"` (single space) — or use `org-canvas-org-set-property` if it isn't already.

- [ ] **Step 4: Run; PASS**

- [ ] **Step 5: Run full gate**

- [ ] **Step 6: Commit**

```bash
git add lisp/org-canvas-settings.el test/org-canvas-settings-test.el
git commit -m "$(cat <<'EOF'
fix(settings): single space in :LICENSE: emitter

Improvements item #17. Cosmetic.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 22: Headline sanitization for special chars (#20)

**Files:**
- Modify: `lisp/org-canvas-files.el` — escape Org-breaking chars in display names used as headlines
- Test: `test/org-canvas-files-test.el`

- [ ] **Step 1: Inventory chars that break Org parsing**

In Org headlines, the only structurally-breaking chars are those in `[`-link syntax when the display name is the link description. `[` and `]` need escaping with `\[` and `\]`. Everything else (parens, underscores) is cosmetic and stays.

- [ ] **Step 2: Failing test**

```elisp
(it "escapes brackets in file display names used as link descriptions"
  (expect (org-canvas--file-sanitize-headline "[bracketed].pdf")
          :to-equal "\\[bracketed\\].pdf"))
```

- [ ] **Step 3: Run; FAIL**

- [ ] **Step 4: Implement**

```elisp
(defun org-canvas--file-sanitize-headline (display-name)
  "Escape Org-breaking chars in DISPLAY-NAME for safe use in a headline."
  (replace-regexp-in-string
   "\\[\\|\\]"
   (lambda (m) (concat "\\\\" m))
   display-name))
```

Apply at the headline-emit site only (don't escape filesystem paths).

- [ ] **Step 5: Run; PASS**

- [ ] **Step 6: Run full gate**

- [ ] **Step 7: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
fix(files): escape brackets in file display-name headlines

Improvements item #20. Filenames containing [ or ] broke Org link
parsing when used as the description of a [[file:...]] link. Escape
them at the headline emit site.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

# Final Verification

## Task 23: End-to-end manual smoke test

After all 22 tasks land:

- [ ] **Step 1: Re-pull the data science course**

```bash
mv "/home/tsranso/Documents/20-teaching/4300 6300 Applied Data Science/2026 Spring/canvas-structure" /tmp/canvas-structure-pre-improvements
```

In Emacs:
```
M-x org-canvas-pull-all
```

- [ ] **Step 2: Verify expectations against improvements.md**

Spot-check each of the 20 items from `improvements.md` against the new output:

1. `pages.org` shows `:CANVAS_ID:` (not `:CANVAS_URL:`) for the Palmetto Jupyter page; if the timeout still happens, summary buffer pops up with the error.
2. Embedded image links in announcements/assignments/quizzes are `[[file:content/...]]`.
3. `settings.org` `:COURSE_IMAGE:` is a local path; `content/course_image/` has the image.
4. `new-quizzes.org` has a 0-items header.
5. `quizzes.org` has Question subheadings under every quiz that has questions.
6. Property drawers no longer carry `:PEER_REVIEWS: false` etc.
7. `#+LAST_SYNCED:` appears once per file at the top; no per-entry copies.
8. `:INDENT: 0` is gone.
9. `assignments.org` is sorted by group then position.
10. Module items have `:ITEM_TYPE:`.
11. `DUE_AT` timestamps reflect course TZ (`23:59` EDT, not `03:59` UTC the next day).
12. `rubrics.org` has child criterion headings + sub-tables.
13. Quizzes with both description and questions have `** Description`.
14. Assignments with section overrides have `#+NAME: overrides` tables.
15. Announcements have `:POSTED_AT:`, `:AUTHOR:`, `:DELAYED_POST_AT:`.
16. URLs in `org-canvas.log` are `clemson.instructure.com/api/v1/...` (single slash).
17. `:LICENSE: private` (single space).
18. Empty pull files have self-doc headers.
19. Log shows duplicate-file warnings.
20. File headlines with brackets are escaped.

- [ ] **Step 3: Verify push round-trip on a single item**

Edit one announcement's body, run `M-x org-canvas-sync-announcements`, confirm the change lands in Canvas.

- [ ] **Step 4: Verify a full sync still works**

`M-x org-canvas-sync` should complete without errors.

- [ ] **Step 5: If issues found, file follow-up commits per task pattern**

---

## Self-Review Checklist (run after writing each commit)

After every commit, before the next task:

1. `eldev lint && eldev complexity && eldev test` is green.
2. New code paths have test coverage at the line level (run `eldev test -u "on,text,dontsend"` and check the diff against the previous run).
3. CLAUDE.md "Lessons Learned" doesn't list a pattern this commit violates (e.g., marker-shifting in dolist + buffer-modify; see Task 18 link to that section).
4. The commit message accurately describes why, not just what.
