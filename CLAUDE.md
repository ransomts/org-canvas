# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

org-canvas is an Emacs Lisp package that synchronizes course content from Org Mode files to Canvas LMS via its REST API. The workflow treats Org files as the "source of truth" - instructors design courses in Org Mode and push changes to Canvas.

### Supported Content Types (15)

Assignments, quizzes (classic), new quizzes, pages, modules, rubrics, outcomes, discussions, announcements, files, assignment groups, group categories, calendar events, sections, and per-section date overrides.

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
├── org-canvas.el              # Main entry point, orchestrates all modules
├── org-canvas-core.el         # Meta-require for all core-* files
├── org-canvas-core-config.el  # Config, constants, shared enum values
├── org-canvas-core-api.el     # API request helpers, curl, rate limiting
├── org-canvas-core-org.el     # Org property/buffer helpers, HTML export
├── org-canvas-core-sync.el    # Sync pipeline macros, push/pull/delete infra
├── org-canvas-credentials.el  # Secrets (API token, course ID) - not in git
├── org-canvas-validate.el     # Offline validation engine (no API contact)
└── org-canvas-{feature}.el    # Feature modules (pages, rubrics, announcements, etc.)
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
  :finalize #'org-canvas--announcement-finalize
  :pull-item-fn #'org-canvas--announcement-pull-item)
```
Generates `org-canvas-sync-announcements` with logging, error handling, conflict resolution, and buffer saving. The optional `:pull-item-fn` enables the "pull" option during interactive conflict resolution. The optional `:hash-extra` is a function of the parsed data whose string result is folded into the PAYLOAD_HASH — required for any module whose children sync inside finalize, because the unchanged-skip bypasses finalize and child edits would otherwise never propagate. Users: modules (`org-canvas--module-items-digest`, resolves link targets so content-id rotation also dirties), quizzes (`org-canvas--quiz-questions-digest`), new-quizzes (`org-canvas--new-quiz-items-digest`, also folds the rubric link). The latter two build on `org-canvas--org-children-digest` (core-org), which digests raw child-subtree text with sync-state property lines stripped — digests deliberately exclude CANVAS_ID/CANVAS_ITEM_ID (assigned by finalize right after hashing) so a successful sync doesn't dirty the next run. Outcomes need no digest: their two levels sync as independent pipeline passes.

**Declarative Parse Macro:**
```elisp
(org-canvas-define-parse announcement
  :body :message              ;; export subtree HTML into this key
  :properties
  (("PUBLISHED"      :published                :type boolean :default t)
   ("POST_AT"        :delayed_post_at          :type timestamp)
   ("ALLOW_COMMENTS" :allow_discussion_comments :type boolean)
   ("SPECIFIC_SECTIONS" :specific_sections      :type string)))
```
Generates three functions from a declarative spec:
- `org-canvas--{feature}-read-props (pom)` — raw `org-entry-get` calls
- `org-canvas--{feature}-transform-props (raw)` — pure typed conversion
- `org-canvas--{feature}-parse-entry ()` — full parse pipeline

Type dispatch: `boolean` (with optional `:default`), `timestamp`, `number`, `enum` (with `:values`), `string` (pass-through). Hooks: `:after-read`, `:after-transform` for non-standard modules.

**Modules using `org-canvas-define-parse`:** announcements, pages, calendar, group-categories, assignment-groups

**Delete Macros:**
```elisp
(org-canvas-define-delete-all pages
  :endpoint "pages"
  :file org-canvas-pages-file
  :id-field 'url              ; optional, default 'id
  :id-property "CANVAS_URL"   ; optional, default "CANVAS_ID"
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t))
  ;; For non-course-scoped endpoints:
  :list-url-fn (lambda () (format "%s/api/v1/calendar_events" org-canvas-base-url))
  :delete-url-fn (lambda (id) (format "%s/api/v1/group_categories/%s" org-canvas-base-url id))
  :delete-data '((cancel_reason . "Deleted by org-canvas")))

(org-canvas-define-delete-at-point assignment
  :endpoint "assignments/%s")
```

**Push/Search/Finalize Helpers:**
```elisp
;; Generic search - replaces duplicated find-by-title/name functions
(org-canvas--search-item "assignments" title :match-field 'name)

;; Generic push with 404 retry, timeout recovery, and conflict detection
;; Conflict check is delegated to org-canvas--push-check-and-resolve-conflict
;; which returns 'push, 'skip, or 'pulled
(org-canvas--push-to-api data payload
  :endpoint "pages"
  :id-key :canvas-url           ; for pages (default :canvas-id)
  :find-fn (lambda (title) (org-canvas--search-item "pages" title)))

;; Generic finalize with optional post-processing
(org-canvas--finalize-item data response
  :id-field 'url                ; default 'id
  :id-property "CANVAS_URL"     ; default "CANVAS_ID"
  :post-fn #'associate-rubric)  ; optional callback
```

**Centralized Property Registry:**
```elisp
(org-canvas-register-properties "announcements"
  :label "Announcements"
  :file-var 'org-canvas-announcements-file
  :query "LEVEL=1"
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :api-key "published" :boolean-json t)
    (:org-prop "POST_AT" :data-key :delayed_post_at :type timestamp
     :api-key "delayed_post_at")))
```
All 19 validation targets self-register their property definitions at load time. The validation engine (`org-canvas-validate`) queries this registry at runtime instead of maintaining a separate spec constant. Property specs declare: Org property name, output plist key, type, API field name, and validation constraints (enum values, link targets, date ordering).

**Modules registering properties:** All feature modules register via `org-canvas-register-properties`. Modules with multiple heading levels register multiple entries (e.g., quizzes registers "quizzes" for LEVEL=1 and "quiz-questions" for LEVEL=2).

**The registry `:query` must match the sync query's heading level.** Validation selects headings with it — assignment-groups use `"LEVEL=2+WEIGHT={.}"` because groups are level-2 headings under a container; a `LEVEL=1` query would validate only the container and silently skip every group (this bug hid the drop-rules check for months). Structural checks wire in via `:structural-fn` (e.g. `org-canvas--validate-drop-rules`, `org-canvas--validate-page-structure`).

**Pending-first-sync collapse:** validation warnings for link targets that merely lack a CANVAS_ID carry a `:pending-sync` flag and collapse into one summary line in the report; `C-u M-x org-canvas-validate` lists them individually.

**Declarative Payload Builder:**
```elisp
(org-canvas-define-payload group-category
  :registry-key "group-categories"
  :format alist
  :title-key :title
  :title-api-key name)
```
Generates `org-canvas--{feature}-build-payload` from registry property specs. Properties with `:api-key` are included in the payload; `:boolean-json t` properties are converted via `org-canvas--to-json-boolean`. Supports alist and hash-table formats, wrapper keys, static fields, and escape hatches (`:post-build-fn`, `:extra-required-fn`).

**Modules using `org-canvas-define-payload`:** group-categories, calendar, pages, announcements

**Modules using shared infrastructure:** announcements, pages, discussions, assignments, assignment-groups, rubrics

**Modules with custom push logic (use `org-canvas-define-sync` but have custom push functions due to non-standard API endpoints):** group-categories (split POST/PUT URLs), calendar (global endpoint, not course-scoped)

**Modules with custom sync logic (not macro-based):** files (3-step upload), outcomes (hierarchical), quizzes (nested questions), new-quizzes (nested items, `/api/quiz/v1/` API), modules (parent-child items), overrides (reconcile-based)

Files sync in three tiers rather than always re-uploading: `PAYLOAD_HASH` stores `BYTES:METADATA` (`org-canvas--file-hash-parts` splits it), so a whole-hash match skips, a bytes match goes through `org-canvas--file-update-metadata` (one `PUT /api/v1/files/:id` — name, parent_folder_id, locked/hidden/dates — keeping the file id), and only a content change falls back to delete-and-re-upload, which rotates the id and breaks external links. Legacy single-md5 hashes re-upload once, then carry both halves. Before either write path, `org-canvas--file-check-conflict` runs the shared conflict resolution with `org-canvas--file-pull-item` as the pull option (issue #49).

**Pull-only modules:** sections (pulled from Canvas via `org-canvas-pull-sections`, not pushed)

### Complexity Management

Functions are kept below a cognitive complexity threshold of 15. Common extraction patterns used throughout the codebase:

- **Single-item helpers**: Extract the body of a `dolist` into a named helper (e.g., `org-canvas--settings-sync-single-tab`, `org-canvas--file-sync-single-entry`, `org-canvas--resolve-single-image`)
- **Data-driven loops**: Replace repetitive `when`-blocks with a field-spec constant and `dolist` + `pcase` (e.g., `org-canvas--late-policy-field-specs` in settings.el)
- **Conflict extraction**: `org-canvas--push-check-and-resolve-conflict` handles the entire conflict detection block, returning a `push`/`skip`/`pulled` symbol
- **Timeout detection**: `org-canvas--timeout-error-p` is the shared predicate for timeout errors — use it instead of inline string matching
- **Payload wrapping**: Extract nested payload restructuring into helpers (e.g., `org-canvas--new-quiz-item-wrap-payload`)
- **Pull property setters**: Extract property-setting blocks from pull functions into dedicated helpers (e.g., `org-canvas--file-pull-set-properties`, `org-canvas--settings-pull-late-policy-properties`)

Run `eldev complexity` to check current metrics. Target: 0 functions above threshold (>15).

### Sync State Property

All items track Canvas state via `CANVAS_ID`:
- Present: UPDATE (PUT) operation
- Absent: CREATE (POST) operation

Use `org-canvas-org-save-sync-state` to standardize saving.

### Global Sync Summary

`org-canvas-sync` ends with a per-type table (Success/Skipped/Failed/Deferred) plus named failed/skipped items, rendered by `org-canvas--sync-log-global-summary`. Feature syncs record stats via `org-canvas--sync-record-feature-stats` — automatic in the macro pipeline (`org-canvas--sync-log-summary`), explicit in the custom syncs (settings, files, overrides). Recording is a no-op outside a global sync (gated on `org-canvas--sync-global-counters`).

Module items skipped because their target lacked a CANVAS_ID are tracked in `org-canvas--module-items-pending` and retried after the final tier (`org-canvas--module-retry-pending-items`): healed items are reclassified skip→success in the summary; the rest produce a named "re-run org-canvas-sync-modules" hint.

## Code Conventions

### Naming
- Private functions: `org-canvas--function-name` (double dash)
- Public functions: `org-canvas-function-name` (single dash)
- Entry points: `org-canvas-sync-{feature}` (push) or `org-canvas-pull-{feature}` (pull-only)
- Sync single: `org-canvas-sync-{feature}-at-point`
- Delete all: `org-canvas-delete-all-{feature}`
- Delete single: `org-canvas-delete-{feature}-at-point`
- Prune orphans: `org-canvas-prune-{feature}` (generated by `org-canvas-define-delete-all` from the same spec; deletes Canvas items whose ID is absent from the org file, after confirmation)

### Logging
- Use the in-tree logger in `lisp/org-canvas-core-log.el` via `org-canvas--logger`
- Public API: `org-canvas--log-{trace,debug,info,warning,error}` and `org-canvas--logger-{set-level,set-file,set-handlers}`
- Stage markers: `[Stage N: StageName]` prefix
- Levels: trace, debug, info, warning, error, fatal (priority order; threshold gates emission)
- Secrets never reach logs: every line passes through `org-canvas--log-redact` (masks Bearer tokens and session/csrf/token cookie or query values), and plz-error structs get their response headers scrubbed via `org-canvas--scrub-plz-error` before entering signal data
- `org-canvas--save-buffer` is a no-op on unmodified buffers — completion-time safety saves don't log duplicate `[Saved]` lines

### JSON/API
- Modules with nested payloads (assignments, pages, modules, rubrics, files) use hash-tables: `(let ((payload (make-hash-table))) (puthash 'key val payload))`
- Flat-payload modules (discussions, announcements, quizzes, outcomes) use alists: `'((key . val))`
- Both formats serialize correctly via `json-encode`
- Boolean handling: `t` for true, `:json-false` for false
- Org properties are strings - compare with `"true"`/`"false"`

### Error Handling
- Wrap API calls in `condition-case`
- API-layer signals carry a single concise message: Canvas 4xx JSON bodies are parsed into it via `org-canvas--api-error-message` (e.g. `"The front page cannot be unpublished (HTTP 400)"`); full URL/body detail logs at DEBUG. Each item failure produces exactly one `[ERROR]` line — the `[FAILED]` summary
- plz supports only GET/HEAD/POST/PUT/DELETE; PATCH goes through the direct curl fallback `org-canvas--api-curl-patch` (mimics plz's contract — parsed JSON on success, `plz-error` structs on failure — so shared retry/error machinery applies). Any other method is rejected up front (it would silently degrade to a bodyless GET inside plz). Late-policy updates use PATCH via this fallback — Canvas routes that update as PATCH only, and PUT 404s even when a policy exists (issue #13; do not "simplify" it back to PUT)
- On timeout: search Canvas for item, retry if needed — use `org-canvas--timeout-error-p` to detect timeout errors (matches both `"Timeout"` and `"timed out"` case-insensitively)
- On 404 for PUT: retry as POST
- On 429 or rate-limit 403: retry with configurable delay (`org-canvas-rate-limit-retries`, `org-canvas-rate-limit-wait`)
- On 401: actionable message about expired API token
- On 403 (non-rate-limit): message about insufficient token scope
- Continue processing other items if one fails
- Deferrable rejections (drop rules exceeding the group's assignment count) count as `:deferred`, not failures — `org-canvas--sync-deferred-error-p` classifies them; they self-resolve on a later sync
- Master sync wraps each feature in `org-canvas--safe-sync` so missing `.org` files are skipped
- `org-canvas--preflight-check` validates credentials and connection before any sync begins

### Conflict Resolution
- Baseline is `org-canvas--conflict-baseline`: the entry's own `CANVAS_UPDATED_AT` (the remote `updated_at` finalize recorded on the last push — Canvas's clock, so no skew allowance, and present on push-only courses), falling back to the file-level `#+LAST_SYNCED` header. Pushes now write that header too, stamped from the newest remote `updated_at` of the run and rounded up a minute (issue #48)
- The payload-hash skip is drift-aware: `org-canvas--sync-fetch-remote-updated` takes one list snapshot per feature and `org-canvas--sync-remote-drifted-p` pulls any remotely-modified entry off the skip path so it goes through the normal conflict comparison. A matching hash only proves the *local* file is unchanged — without this, an item edited only in the web UI was skipped forever. When the snapshot is unavailable, `org-canvas--sync-warn-unverified-skips` says how many entries went unchecked
- `org-canvas--conflict-check` returns `(cons 'conflict REMOTE-RESPONSE)` (not bare `'conflict`)
- `org-canvas--resolve-conflict` shows a diff buffer and prompts: push/pull/skip (capitals = apply to all)
- `org-canvas--conflict-apply-all` defvar: batch decision bound per-sync by `org-canvas-define-sync`
- `org-canvas--current-pull-item-fn` defvar: dynamically bound per-sync so `push-to-api` can access it
- `org-canvas--conflict-pull-local` overwrites local heading via the module's pull-item function
- Push-to-api returns `'pulled` (not `'conflict`) when user chooses pull — tracked by `:pulled` counter in sync pipeline
- Modules with `:pull-item-fn` in their `org-canvas-define-sync`: announcements, pages, discussions, assignments, assignment-groups, rubrics, group-categories, calendar-events, modules (whose pull-item also replaces child item headings from the remote list, fetching them when the conflict-check GET response lacks an `items` key)

### Org Interaction
- Always `org-back-to-heading t` before property access
- Use markers for safe position tracking
- Save buffer after modifications

## Key Files

- `lisp/org-canvas-core.el` - All shared utilities (read this first)
- `readme.org` - Project overview and quick start
- `documentation/manual.org` - Full manual with file formats, properties, and commands
- `demo-course/` - Working example course (DS 101); 16 .org files covering all content types
- `documentation/architecture/canvas-openapi3.yaml` - Canvas API spec
- `test/contract/` - OpenAPI contract fixture + generator (payload conformance)
- `test/mutation/` - Mutation-testing harness (assertion-depth measurement)
- `test/docgen/` - Generates the manual's Property Reference from the registry

## Dependencies

External: `plz`, `transient` (0.4+), `org` (9.6+), `ox-html`

Logging is handled by the in-tree `lisp/org-canvas-core-log.el` module (no external dependency).

## Testing

### Running Tests

```bash
eldev test              # Run all ~2800 tests (about 30s)
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
├── test-helper.el                       # Common fixtures, mocks, macros
├── org-canvas-core-{area}-test.el       # Core, split: config, api, org, sync, usability
├── org-canvas-test.el                   # Orchestration/integration
├── org-canvas-{feature}-test.el         # One per feature module
├── org-canvas-validate-test.el          # Offline validation engine
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

**~2800 tests total** (2791 as of 2026-08-10) covering core utilities, all feature modules, and validation (9 tests skip on Emacs 29.x due to org-mode differences). Line coverage is ~99.5% — the pre-push hook blocks below 99%. Run `eldev test` for the exact current count and `eldev test -u "on,codecov,dontsend" -U coverage/coverage.json` for per-file coverage.

Largest suites: core-sync, core-org, quizzes, modules, files, new-quizzes, validate (one `org-canvas-{name}-test.el` per module; exact sizes drift, don't track them here).

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
- Canvas-to-Org import for all 15 feature modules
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

**plz has no PATCH support** (verified empirically against a local echo server): an unrecognized method symbol falls through plz's curl-argument builder, the request goes out as a **plain GET with no body**, and plz then crashes in its process sentinel. This is why New Quizzes updates (a PATCH-only API) and late-policy updates (also PATCH-only — PUT is not routed and 404s, issue #13) never worked until PATCH got its own transport: `org-canvas--api-curl-patch` invokes curl directly, passing method/headers/url via a config stream on stdin (keeping the token off the command line, same trick as plz) with the body appended after the trailing `data-binary = "@-"` directive. It returns parsed JSON and signals `plz-error` structs, so the shared retry and error machinery treats it exactly like a plz request. Methods outside `org-canvas--api-supported-methods` are rejected with a clear error.

### Property Drawers Must Not Reach the Body Link Resolver

`org-canvas--export-subtree-body-to-html` strips property drawers from its temp buffer before `org-canvas--resolve-body-links` runs. Drawer links (`GROUP:`, `RUBRIC_LINK:`) can't resolve to Canvas URLs (assignment groups and rubrics have no user-facing page) and would emit false "Unresolved" warnings; the HTML exporter drops drawers anyway, so output is unchanged.

### Dry Run Is a Per-Module Obligation, Not a Shared Guarantee

`org-canvas--dry-run` is checked by the shared machinery — `org-canvas--sync-execute-pipeline` (which skips push *and* finalize) and `org-canvas--push-to-api` — so every macro-based module is covered for free. **Modules with custom sync loops are not.** Issue #34: `org-canvas-sync-dry-run` issued `DELETE /api/v1/files/:id` against a live course and re-uploaded five files, because `org-canvas-sync-files` runs its own loop into `org-canvas--file-push-to-api`, which talks to `org-canvas-api-request` directly. Overrides (PUT/POST/DELETE reconcile) and the settings late-policy PATCH had the same hole.

When adding or editing a module that does not go through `org-canvas-define-sync`, guard **every** write path yourself, and put the check at the point where one guard covers the whole sequence (`org-canvas--file-push-to-api` covers the DELETE *and* the 3-step upload; `org-canvas--settings-push-late-policy` covers the PATCH *and* its POST fallback). Return `org-canvas--dry-run-response` and have the caller recognize it with `org-canvas--dry-run-response-p`, so finalize, `PAYLOAD_HASH` writes, and other bookkeeping are skipped — a preview must never leave an entry looking synced. Side effects that are not requests need guarding too (folder pre-creation in files).

`test/org-canvas-dry-run-test.el` enforces this for every sync entry point against a throwaway copy of `demo-course/`: zero non-GET requests, and the .org files byte-identical afterward. Add new sync commands to `org-canvas-dry-run--sync-commands` (a spec asserts each is `fboundp`, so a rename surfaces there).

### Test Isolation: the Network Guard Blocks Unmocked HTTP

test-helper.el advises the network primitives — `plz`, `url-retrieve-synchronously`, and `call-process` when spawning `plz-curl-program` — to signal "Unmocked network call during tests" instead of performing real I/O. The test environment loads the user's real `org-canvas-credentials.el`, so before the guard an unmocked code path (e.g. the assignment overrides sub-fetch) silently attempted live API calls with real credentials. Tests that exercise API internals are unaffected: `cl-letf` on those same symbols replaces the whole function cell, advice included. If a new test trips the guard, mock `org-canvas-api-request`/`org-canvas-api-request-all-pages` (or the primitive itself) — never weaken the guard.

### Test Isolation: Don't Assert on the Shared Log Buffer

Integration tests must not assert on `*org-canvas-log*` buffer contents — earlier tests mutate logger state (handlers, buffer names, leftover content), so such assertions pass in isolation but fail in the full run. Instead capture log lines by `cl-letf`-ing `org-canvas--log-info`/`org-canvas--log-warning` into a list and match on that.

### gh CLI Quirk

`gh issue view N` exits non-zero due to a GraphQL Projects-classic deprecation warning even on success — use `gh issue view N --json body --jq .body` instead.

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

### Emacs Version Compatibility

Org-mode ships with Emacs but has its own version (e.g., org 9.6 in Emacs 29.x, org 9.7 in Emacs 30.x). Functions like `org-back-to-heading`, `org-end-of-subtree`, and `org-get-heading` may behave differently between org-mode versions, especially in programmatically created buffers.

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

**Current skipped tests (9 total):**
- `test/org-canvas-modules-test.el` — 4 (SubHeader parsing, link type detection)
- `test/org-canvas-new-quizzes-test.el` — 3 (nested item parsing)
- `test/org-canvas-quizzes-test.el` — 2 (numerical answer exact/range parsing)

These involve `org-back-to-heading` and `org-end-of-subtree` which don't recognize heading structure consistently in Emacs 29.x's org-mode when using `search-forward` positioning. Grep for `buttercup-pending` to enumerate.

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
