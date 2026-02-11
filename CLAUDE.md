# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

org-canvas is an Emacs Lisp package that synchronizes course content from Org Mode files to Canvas LMS via its REST API. The workflow treats Org files as the "source of truth" - instructors design courses in Org Mode and push changes to Canvas.

### Supported Content Types (12)

Assignments, quizzes, pages, modules, rubrics, outcomes, discussions, announcements, files, assignment groups, sections, and per-section date overrides.

## Build Commands

This project uses Eldev (Elisp Development Tool):

```bash
eldev test           # Run tests
eldev compile        # Compile Elisp files
eldev lint           # Run linter
eldev package        # Create distributable package
```

**Always run `eldev lint` before committing or pushing.** Fix any warnings before proceeding.

## Architecture

### Module Structure

```
lisp/
├── org-canvas.el           # Main entry point, orchestrates all modules
├── org-canvas-core.el      # Shared infrastructure (config, logging, API, Org utilities)
├── org-canvas-credentials.el  # Secrets (API token, course ID) - not in git
├── org-canvas-validate.el  # Offline validation engine (no API contact)
└── org-canvas-{feature}.el # Feature modules (pages, rubrics, announcements, etc.)
```

### Dependency Rules

- All feature modules require `org-canvas-core`
- Feature modules must NOT depend on each other
- `org-canvas-core` must NOT import any feature modules (prevents circular deps)
- `org-canvas.el` orchestrates by requiring all modules

### 4-Stage Pipeline Pattern

Every feature module follows this consistent pattern:

1. **Parse** (`org-canvas--{feature}-parse-entry`) - Extract data from Org heading properties
2. **Build Payload** (`org-canvas--{feature}-build-payload`) - Convert to Canvas API format (hash-tables)
3. **Execute** (`org-canvas--{feature}-push-to-api`) - Call API with timeout recovery
4. **Finalize** (`org-canvas--{feature}-finalize`) - Save CANVAS_ID and LAST_SYNCED to Org file

### Shared Infrastructure (org-canvas-core.el)

The core module provides macros and helpers to eliminate boilerplate:

**Sync Pipeline Macro:**
```elisp
(org-canvas-define-sync announcements
  :file org-canvas-announcements-file
  :parse #'org-canvas--announcement-parse-entry
  :build #'org-canvas--announcement-build-payload
  :push #'org-canvas--announcement-push-to-api
  :finalize #'org-canvas--announcement-finalize)
```
Generates `org-canvas-sync-announcements` with logging, error handling, and buffer saving.

**Delete Macros:**
```elisp
(org-canvas-define-delete-all pages
  :endpoint "pages"
  :file org-canvas-pages-file
  :id-field 'url              ; optional, default 'id
  :id-property "CANVAS_URL"   ; optional, default "CANVAS_ID"
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))

(org-canvas-define-delete-at-point assignment
  :endpoint "assignments/%s")
```

**Push/Search/Finalize Helpers:**
```elisp
;; Generic search - replaces duplicated find-by-title/name functions
(org-canvas--search-item "assignments" title :match-field 'name)

;; Generic push with 404 retry and timeout recovery
(org-canvas--push-to-api data payload
  :endpoint "pages"
  :id-key :canvas-url           ; for pages (default :canvas-id)
  :find-fn #'org-canvas--page-search-by-title)  ; for timeout recovery

;; Generic finalize with optional post-processing
(org-canvas--finalize-item data response
  :id-field 'url                ; default 'id
  :id-property "CANVAS_URL"     ; default "CANVAS_ID"
  :post-fn #'associate-rubric)  ; optional callback
```

**Modules using shared infrastructure:** announcements, pages, discussions, assignments, assignment-groups, rubrics

**Modules with custom sync logic (not macro-based):** files (3-step upload), outcomes (hierarchical), quizzes (nested questions), modules (parent-child items), overrides (reconcile-based)

**Pull-only modules:** sections (pulled from Canvas via `org-canvas-pull-sections`, not pushed)

### Sync State Property

All items track Canvas state via `CANVAS_ID`:
- Present: UPDATE (PUT) operation
- Absent: CREATE (POST) operation

Use `org-canvas-org-save-sync-state` to standardize saving.

## Code Conventions

### Naming
- Private functions: `org-canvas--function-name` (double dash)
- Public functions: `org-canvas-function-name` (single dash)
- Entry points: `org-canvas-sync-{feature}` (push) or `org-canvas-pull-{feature}` (pull-only)
- Sync single: `org-canvas-sync-{feature}-at-point`
- Delete all: `org-canvas-delete-all-{feature}`
- Delete single: `org-canvas-delete-{feature}-at-point`

### Logging
- Use `elog` library with `org-canvas--logger`
- Stage markers: `[Stage N: StageName]` prefix
- Levels: debug, info, warning, error

### JSON/API
- Modules with nested payloads (assignments, pages, modules, rubrics, files) use hash-tables: `(let ((payload (make-hash-table))) (puthash 'key val payload))`
- Flat-payload modules (discussions, announcements, quizzes, outcomes) use alists: `'((key . val))`
- Both formats serialize correctly via `json-encode`
- Boolean handling: `t` for true, `:json-false` for false
- Org properties are strings - compare with `"true"`/`"false"`

### Error Handling
- Wrap API calls in `condition-case`
- On timeout: search Canvas for item, retry if needed
- On 404 for PUT: retry as POST
- On 429 or rate-limit 403: retry with configurable delay (`org-canvas-rate-limit-retries`, `org-canvas-rate-limit-wait`)
- On 401: actionable message about expired API token
- On 403 (non-rate-limit): message about insufficient token scope
- Continue processing other items if one fails
- Master sync wraps each feature in `org-canvas--safe-sync` so missing `.org` files are skipped
- `org-canvas--preflight-check` validates credentials and connection before any sync begins

### Org Interaction
- Always `org-back-to-heading t` before property access
- Use markers for safe position tracking
- Save buffer after modifications

## Key Files

- `lisp/org-canvas-core.el` - All shared utilities (read this first)
- `readme.org` - Project overview and quick start
- `documentation/manual.org` - Full manual with file formats, properties, and commands
- `example-course/` - Working example course structure (12 .org files)
- `documentation/architecture/canvas-openapi3.yaml` - Canvas API spec

## Dependencies

External: `plz`, `elog`, `compat`, `org` (9.6+), `ox-html`

The `elog` package is fetched from GitHub via `eldev-use-vc-repository` (not vendored). The `compat` library provides forward compatibility for core Emacs functions across versions.

## Testing

### Running Tests

```bash
eldev test              # Run all 979 tests
eldev test "core"       # Run tests matching pattern
```

### Code Coverage

Code coverage is provided by the `undercover` library via Eldev's undercover plugin.

```bash
# Text report (prints summary to console)
eldev test -u "on,text,dontsend"

# Codecov JSON report (for Codecov integration)
eldev test -u "on,codecov,dontsend" -U coverage/coverage.json

# Coveralls format (for CI integration)
eldev test -u "on,coveralls"
```

Coverage reports are saved to `coverage/` (gitignored).

### Test Structure

```
test/
├── test-helper.el                    # Common fixtures, mocks, macros
├── org-canvas-core-test.el           # Core utilities (259 tests)
├── org-canvas-test.el               # Orchestration/integration (8 tests)
├── org-canvas-{feature}-test.el      # Tests for each feature module (12 modules)
```

### Test Utilities

**`with-temp-org-buffer`** - Create temp Org buffer for testing:
```elisp
(with-temp-org-buffer
 "* Heading
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
 (org-back-to-heading)
 (should (equal (org-entry-get (point) "CANVAS_ID") "123")))
```

**`with-mock-api`** - Mock API calls without network:
```elisp
(with-mock-api
 (org-canvas--announcement-push-to-api data payload)
 (should (test-org-canvas-api-called-p 'POST "discussion_topics")))
```

### Test Coverage Summary

**1264 tests total** covering core utilities, all feature modules, and validation (6 tests skip on Emacs 29.x due to org-mode differences). Coverage is ~99% (17 uncovered lines across 15 files).

| Module            | Tests |
|-------------------|-------|
| **core**          | 353   |
| files             | 136   |
| modules           | 133   |
| quizzes           | 123   |
| outcomes          | 97    |
| **validate**      | 72    |
| assignments       | 69    |
| pages             | 48    |
| rubrics           | 48    |
| sections          | 43    |
| orchestration     | 38    |
| discussions       | 34    |
| announcements     | 31    |
| settings          | 20    |
| assignment-groups | 19    |

**Core tests cover:**
- Path utilities (`org-canvas--path`)
- API endpoint construction and curl command generation
- Org property getters/setters (string, boolean, number)
- Sync state persistence (`org-canvas-org-save-sync-state`)
- Timestamp parsing (Org → ISO8601)
- Safe entry iteration (`org-canvas--for-each-entry`)
- Generic push/search/finalize helpers
- Error handling (timeout recovery, 404 retry, cascading errors)
- Link resolution between files
- Rate limit retry (429/403), 401/403 actionable error messages
- Preflight credential and connection validation
- Payload hashing, date validation, title stripping

**Feature module tests cover all 4 stages:**
1. **Parse**: Property extraction, defaults, CANVAS_ID detection, timestamp conversion
2. **Build**: Payload construction, boolean handling, optional fields
3. **Push**: POST/PUT selection, timeout recovery, 404 retry logic
4. **Finalize**: CANVAS_ID saving, LAST_SYNCED timestamps, error handling

**Pull function tests cover:**
- Canvas-to-Org import for all 12 feature modules
- `pull-upsert-heading`, `pull-insert-body`, property/timestamp setting
- Item link resolution across files (modules → pages/assignments/etc.)

**Integration tests cover:**
- Full sync pipelines (`org-canvas-sync-{feature}`)
- Full pull pipelines (`org-canvas-pull-{feature}`)
- Delete-all commands (`org-canvas-delete-all-{feature}`)
- Delete-at-point commands
- User confirmation prompts
- Cross-file link resolution

## Eldev Configuration

### Project Structure Quirk

The main file `org-canvas.el` lives in `lisp/` but the package linter expects it at the project root. A **symlink** at the root resolves this:

```
org-canvas.el -> lisp/org-canvas.el
```

This allows `eldev lint` to find the `Package-Requires` header while keeping the actual file in `lisp/` where Eldev's load path expects it.

### Key Eldev Settings

```elisp
(setf eldev-project-source-dirs '("lisp"))
(setf eldev-project-main-file "org-canvas.el")
(eldev-use-vc-repository 'elog :github "Kinneyzhang/elog")
(eldev-add-extra-dependencies 'runtime '(elog))
(push "test" eldev-project-source-dirs)  ; For test discovery
(setf eldev-lint-ignored-fileset '("vendor/" "test/"))
```

### Cleaning Build Artifacts

If builds fail mysteriously, clean the Eldev cache:
```bash
eldev clean all
```

## Lessons Learned

### HTTP Library: Use `plz` instead of `request.el`

The `request.el` library with `:sync t` has a known race condition where synchronous requests can freeze indefinitely, even when the underlying curl process completes successfully. The `plz` library handles synchronous requests more reliably.

**plz method format**: Pass HTTP methods as lowercase symbols:
```elisp
;; Correct
(plz 'get url ...)
(plz 'post url ...)

;; Wrong - will cause 400 Bad Request
(plz 'GET url ...)   ; uppercase symbol
(plz :get url ...)   ; keyword
```

The conversion from the codebase convention (`'GET`, `'POST`) to plz format:
```elisp
(intern (downcase (symbol-name method)))  ; 'POST -> 'post
```

**Note**: The `request.el` library is NOT a dependency. Some files had leftover `(require 'request)` statements that were removed - these were dead code from the migration to `plz`.

### Canvas File Upload API Quirks

Canvas's 3-step file upload process returns `upload_params` with null/unknown values:
```json
{
  "upload_params": {
    "filename": null,
    "content_type": "unknown/unknown"
  }
}
```

The multipart form upload code must override these with actual values:
```elisp
(cond
 ((and (eq key 'filename) (null raw-value))
  actual-filename)
 ((and (eq key 'content_type)
       (or (null raw-value) (string= raw-value "unknown/unknown")))
  actual-content-type)
 (t raw-value))
```

Without this fix, files get uploaded with `display_name: "nil"`.

### Debugging API Issues

The `org-canvas--build-curl-command` function logs curl commands that can be copy/pasted to terminal for testing. This is invaluable for isolating whether issues are in the Elisp HTTP library or the API call itself.

### Canvas Quiz Questions API

Quizzes and questions are **separate API resources**. The quiz description field is just introductory text - actual questions must be synced via the Quiz Questions API:

```
POST /api/v1/courses/:course_id/quizzes/:quiz_id/questions
```

The question payload includes `question_name`, `question_text`, `question_type`, `points_possible`, and an `answers` array. Different question types have different answer formats (see `org-canvas--question-build-answers`).

### Collect Markers Before Iterating with Buffer Modifications

When iterating over Org headings and modifying the buffer (e.g., saving CANVAS_ID), **never use `outline-next-heading` in a while loop**. Buffer modifications shift point positions, causing infinite loops:

```elisp
;; WRONG - infinite loop after buffer modification
(while (outline-next-heading)
  (do-something-that-modifies-buffer))

;; CORRECT - collect stable markers first, then iterate
(let ((markers nil))
  (save-excursion
    (while (outline-next-heading)
      (push (point-marker) markers)))
  (dolist (m (nreverse markers))
    (goto-char (marker-position m))
    (do-something-that-modifies-buffer)))
```

This pattern is used in `org-canvas--sync-quiz-questions` and throughout the codebase.

### Extracting Body Text Without Lists

When parsing question headings, the prompt text should exclude answer lists. Stop at the first list item:

```elisp
(when (re-search-forward "^[ \t]*[-*+] " end t)
  (setq end (match-beginning 0)))
```

This ensures `"What is 2+2?\n- [X] 4\n- [ ] 5"` extracts only `"What is 2+2?"` as the question text, with answers parsed separately.

### Assignment Group Drop Rules Require Two-Phase Sync

Canvas rejects drop rules (e.g., `drop_lowest`) when creating a new assignment group that has no assignments yet:

```
"Drop rules cannot be higher than the number of assignments"
```

**Solution**: Two-phase sync in `org-canvas-sync`:

1. **Phase 1**: Create assignment groups without drop rules (skip `rules` in payload for POST)
2. **Sync assignments**: Places them in their groups
3. **Phase 2**: Re-sync assignment groups with drop rules (include `rules` in payload for PUT)

In `org-canvas--assignment-group-build-payload`, only include rules when updating:

```elisp
(if (and rules is-update)
    `((name . ,name) (group_weight . ,weight) (rules . ,rules))
  `((name . ,name) (group_weight . ,weight)))
```

### Emacs Version Compatibility and the `compat` Library

The `compat` library provides forward compatibility shims for **core Emacs Lisp functions**, allowing newer function signatures and behaviors to work on older Emacs versions. It's useful for:
- `string-empty-p`, `string-join`, etc. on older Emacs
- Newer `seq-*` functions
- Other core Emacs additions

**However**, `compat` does NOT help with **org-mode internal behavior differences**. Org-mode ships with Emacs but has its own version (e.g., org 9.6 in Emacs 29.x, org 9.7 in Emacs 30.x). Functions like `org-back-to-heading`, `org-end-of-subtree`, and `org-get-heading` may behave differently between org-mode versions, especially in programmatically created buffers.

**Testing across Emacs versions:**

```yaml
# CI matrix tests on multiple Emacs versions
strategy:
  matrix:
    emacs-version: ['29.3', '30.1']
```

**File-backed buffers for org-mode tests:**

Org-mode functions work more reliably in file-backed buffers than `with-temp-buffer`. The `with-temp-org-buffer` macro creates real temp files:

```elisp
(defmacro with-temp-org-buffer (content &rest body)
  `(let ((temp-file (make-temp-file "org-test-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file temp-file (insert ,content))
           (with-current-buffer (find-file-noselect temp-file)
             (unwind-protect
                 (progn (goto-char (point-min)) ,@body)
               (kill-buffer))))
       (delete-file temp-file))))
```

**Skip tests for version-specific org-mode behavior:**

Some tests depend on org-mode internals that differ between versions. Use skip conditions:

```elisp
(defvar test-org-canvas-emacs-30-p (>= emacs-major-version 30))

(it "test requiring newer org-mode"
  (unless test-org-canvas-emacs-30-p
    (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
  ;; test body
  )
```

**Current skipped tests (6 total):**
- `org-canvas--module-item-parse-entry` tests for SubHeader parsing
- `org-canvas--module-resolve-link` type detection from filename
- `org-canvas--quiz-parse-numerical-answer` exact and range parsing

These involve `org-back-to-heading` and `org-end-of-subtree` which don't recognize heading structure consistently in Emacs 29.x's org-mode when using `search-forward` positioning.

### JUnit XML Test Output with buttercup-junit

Buttercup doesn't have built-in JUnit XML output, but the `buttercup-junit` package (available on MELPA) provides a JUnit reporter. This is required for Codecov Test Analytics and other CI tools that consume JUnit XML.

**Eldev configuration** (in `Eldev` file):

```elisp
(eldev-add-extra-dependencies 'test 'buttercup-junit)

;; Enable JUnit output via environment variable
(when (getenv "ELDEV_JUNIT")
  (with-eval-after-load 'buttercup
    (require 'buttercup-junit)
    (setq buttercup-reporter #'buttercup-junit-reporter)
    (setq buttercup-junit-result-file
          (or (getenv "JUNIT_REPORT_FILE") "test-results.xml"))))
```

**Key insight**: The JUnit reporter must be configured *after* buttercup is loaded. Using `with-eval-after-load 'buttercup` ensures the reporter is set at the right time. Eldev's `--setup` option runs too early (before dependencies are installed).

**Running locally with JUnit output:**

```bash
ELDEV_JUNIT=1 JUNIT_REPORT_FILE=test-results.xml eldev test
```

**buttercup-junit variables:**
- `buttercup-junit-result-file` - Output file path (default: "results.xml")
- `buttercup-junit-inner-reporter` - Secondary reporter for console output (default: `buttercup-reporter-adaptive`)
- `buttercup-junit--to-stdout` - Whether to also print XML to stdout

### Codecov Integration

Codecov provides two types of reports: **coverage** (line coverage) and **test analytics** (test results, timing, flaky test detection).

**GitHub Actions setup** (`.github/workflows/ci.yml`):

```yaml
- name: Run tests with coverage
  run: |
    mkdir -p coverage
    eldev test -u "on,codecov,dontsend" -U coverage/coverage.json
  env:
    ELDEV_JUNIT: 1
    JUNIT_REPORT_FILE: test-results.xml

# Upload coverage report
- name: Upload coverage to Codecov
  uses: codecov/codecov-action@v5
  with:
    token: ${{ secrets.CODECOV_TOKEN }}
    files: coverage/coverage.json

# Upload test results (separate upload with report_type)
- name: Upload test results to Codecov
  uses: codecov/codecov-action@v5
  if: ${{ !cancelled() }}  # Upload even if tests fail
  with:
    token: ${{ secrets.CODECOV_TOKEN }}
    files: test-results.xml
    report_type: test_results
```

**Key points:**
- Use `codecov-action@v5` (v4 works but v5 has better test analytics support)
- Coverage and test results require **separate upload steps** with different `report_type` values
- Use `if: ${{ !cancelled() }}` for test results to ensure upload even when tests fail
- The `CODECOV_TOKEN` secret must be configured in GitHub repository settings
- Test Analytics may need to be enabled in Codecov settings (beta feature)

### `save-excursion` Discards Return Values from Inner Point Searches

When a helper function finds a position inside `save-excursion` and returns it, the caller **must capture** that return value. After `save-excursion` completes, the buffer's point is restored — so calling `(point)` yields the *old* position, not the one found by the search:

```elisp
;; WRONG — (point) is restored by save-excursion inside find-heading-in-file
(when (org-canvas--find-heading-in-file abs-file heading)
  (with-current-buffer (find-file-noselect abs-file)
    (org-entry-get (point) "CANVAS_ID")))  ;; reads wrong heading!

;; CORRECT — capture the returned point
(let ((heading-point (org-canvas--find-heading-in-file abs-file heading)))
  (when heading-point
    (with-current-buffer (find-file-noselect abs-file)
      (org-entry-get heading-point "CANVAS_ID"))))
```

This bug silently returns nil (or the wrong property) because `org-entry-get` succeeds but reads from whichever position point happened to be at. Tests with a single heading won't catch it — **always test with 3+ headings** to verify the correct one is resolved.

### Diagnosing Paren Imbalances in Elisp

The byte compiler's `Invalid read syntax: ")"` error reports a byte offset, not a line number, making it hard to locate. Use Emacs batch mode with `emacs-lisp-mode` (required for correct string/comment handling):

```bash
emacs --batch -Q --eval '
(with-temp-buffer
  (insert-file-contents "lisp/org-canvas-core.el")
  (emacs-lisp-mode)
  (goto-char (point-min))
  (let ((last-good 1))
    (condition-case err
        (while (< (point) (point-max))
          (setq last-good (line-number-at-pos))
          (forward-sexp))
      (error
       (message "Last good sexp at line %d, error at line %d: %s"
                last-good (line-number-at-pos) err)))))
'
```

To check a single function:

```bash
emacs --batch -Q --eval '
(with-temp-buffer
  (insert-file-contents "lisp/org-canvas-core.el")
  (emacs-lisp-mode)
  (goto-char (point-min))
  (search-forward "(defun org-canvas--my-function")
  (beginning-of-line)
  (condition-case err
      (progn (forward-sexp)
             (message "Balanced, ends at line %d" (line-number-at-pos)))
    (error (message "Error at line %d: %s" (line-number-at-pos) err))))
'
```

**Important:** Without `emacs-lisp-mode`, `forward-sexp` in `fundamental-mode` misparses strings containing brackets (e.g., regex literals like `"\\[\\["`) and reports false positives.
