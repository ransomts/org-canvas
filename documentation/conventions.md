# Code Conventions

Naming, logging, JSON/API, error handling, conflict resolution, Org interaction and complexity. Imported by [CLAUDE.md](../CLAUDE.md), so every session loads it.

## Naming
- Private functions: `org-canvas--function-name` (double dash); public: `org-canvas-function-name`
- Entry points: `org-canvas-sync-{feature}` (push), `org-canvas-pull-{feature}` (pull), `org-canvas-sync-{singular}-at-point`, `org-canvas-sync-{singular}` (one heading by exact title or, with `'canvas-id`, by stamp — the at-point runtime at that position; `org-canvas-sync-headings` takes a list, #287), `org-canvas-pull-{singular}` (its pull twin: Canvas's version of one named heading, no prompt, file saved; `org-canvas-pull-headings` takes a list, #346), `org-canvas-delete-all-{feature}`, `org-canvas-delete-{feature}-at-point`, `org-canvas-prune-{feature}` (generated with delete-all: deletes Canvas items whose ID is absent from the org file, after confirmation)

## Logging
- Use the in-tree logger in `lisp/org-canvas-core-log.el`: `org-canvas--log-{trace,debug,info,warning,error}`, `org-canvas--logger-{set-level,set-file,set-handlers}`; levels trace, debug, info, warning, error, fatal
- Stage markers: `[Stage N: StageName]` prefix
- Secrets never reach logs: every line passes through `org-canvas--log-redact` (Bearer tokens, session/csrf/token cookie or query values); plz-error structs are scrubbed by `org-canvas--scrub-plz-error` before entering signal data
- Secrets never reach the *user* either: a message carrying text the package did not write itself (`error-message-string` above all) goes through `org-canvas--user-message`, never a bare `message`, and `org-canvas--pull-summary-record` masks its `:error` on the way in (#154). The echo area, `*Messages*` and batch stderr are shared sinks too
- Secrets never reach *backtraces* either: a backtrace prints function arguments verbatim, so the token is never one. `org-canvas--api-request-headers` resolves the Authorization header inside the transport function (`org-canvas--api-execute-request`, `org-canvas--api-curl-patch-config`), which also re-signals whatever plz raises so plz's own frames are gone before an error escapes (#178) — api-interaction.org, "Redaction Cannot Reach a Backtrace"
- A log handler never signals: the file is appended unvisited and unlocked, a failure noted once (#443)
- `org-canvas--save-buffer` is a no-op on unmodified buffers; each sync command clears the log unless `org-canvas--inhibit-log-clear` is bound (the master sync binds it)

## JSON/API
- Nested payloads (assignments, pages, modules, rubrics, files) use hash-tables; flat payloads (discussions, announcements, quizzes, outcomes) use alists; both serialize via `json-encode`
- `org-canvas-api-request` decodes with `json-read`, so a JSON array arrives as a **vector**: walk a reply with `cl-find-if`/`seq-*`, or coerce with `(append reply nil)` first. An empty vector is also non-nil, so it passes a `when` guard (#153). A list-returning test stub hides both
- Booleans: `t` for true, `:json-false` for false (nil is JSON null, which Canvas reads differently). Org properties are strings — compare with `"true"`/`"false"`
- The codebase passes `'POST`; `org-canvas-api-request` lowercases it for plz (`'post`). Uppercase or keyword methods to plz are a 400
- GraphQL (post policies, posting grades; #202): a read goes through `org-canvas--graphql-query`, which lifts the read-only guard for that one request and refuses a mutation; a write goes through `org-canvas--graphql-mutate`, which keeps the guard and the dry run. A 200 reply carrying `errors` is an `org-canvas-api-error` — decisions.org, "Post Policies Are GraphQL"

## Error Handling
- Wrap API calls in `condition-case`; continue processing other items if one fails
- One concise message per failure: 4xx bodies parse through `org-canvas--api-error-message`, detail logs at DEBUG, exactly one `[ERROR]` line per item
- A course can be marked read-only (`org-canvas-read-only`, set in the credentials file): `org-canvas--check-writable` refuses any non-GET at the transport, before the request is built, so every present and future writer is covered (#163). Reads, status and diff are untouched
- Read an error datum with `org-canvas--api-error-datum`, never `(cdr err)`: plz signals `plz-http-error` with a *list* of a label and the struct, so a bare `(cdr err)` fails every `plz-error-p` guard and loses the status, body and cookies (#152)
- Timeout → search Canvas for the item, retry if needed (`org-canvas--timeout-error-p` is the predicate); 404 on PUT → retry as POST; 429 or rate-limit 403 → retry (`org-canvas-rate-limit-retries`, `org-canvas-rate-limit-wait`); 401 → expired-token message; other 403 → `org-canvas-permission-error`, which `org-canvas--safe-pull` counts as a skip and names in the closing line (#155); a 404 "disabled for this course" (a tab the course switched off) → `org-canvas-feature-disabled-error`, a skip too, named apart in the closing line (#486); a pull's per-item and sub-read records do the same through `org-canvas--api-skip-error-p`, a refusal or disabled tab a `skip` and anything else an `error` (`org-canvas--rewrite-record-failure` #390, `org-canvas--settings-pull-optional-record` #397)
- `org-canvas-permission-error` has **two parents**: the credentials error (#155's skip handling) *and* `org-canvas-api-error`, so per-item handlers meaning "this request failed" still cover 403. With the credentials parent alone they silently stopped, and one cross-course file link in one page body aborted a 46-page pull (#171). When adding an error symbol, check `grep -rn "(org-canvas-api-error" lisp/` for handlers that should still catch it
- Deferrable rejections (drop rules exceeding the group's assignment count) count as `:deferred` (`org-canvas--sync-deferred-error-p`), not failures
- `org-canvas--safe-sync` skips missing `.org` files; `org-canvas--preflight-check` runs before any sync
- A property only Canvas may set at all (an assignment's document processor) declares `:canvas-owned t` beside its `:remote-fn`: pull writes it, diff compares it even on a silent heading, validate warns when it is typed before the first sync, and the payload never carries it (#184)
- What the course *should* hold of such a property is a second property declaring `:intent-of` the observed one (`WANT_DOCUMENT_PROCESSOR` → `DOCUMENT_PROCESSOR`): never pulled, parsed or sent; validate warns on a stamped heading the last pull left unmet, diff counts it as a CHANGED field row and shows the reverse as an uncounted NOTE row (#293)
- A property a pull writes and no push reads (when an announcement or a reply was posted, who wrote it) declares `:pull-only t`: validate keeps the parse check but drops the past-timestamp warning, since such a timestamp is in the past by nature, and the manual marks it as Canvas's to set
- A property whose Canvas field can come back holding a value only Canvas may set declares those as `:read-only-values` beside `:values` (assignments' SUBMISSION, for the `online_quiz` a quiz's shadow assignment carries): the validator accepts them, since a pull wrote them, and the module refuses them only where a push is genuinely wrong — a create, not an update (#167)
- On a course marked `org-canvas-read-only`, validate holds back findings that only protect a push (past timestamps, pre-first-sync links), marked with `org-canvas--validate-push-only`, and names the count; `org-canvas-validate-all` keeps them (#168). A finding true of the course itself is never marked
- A validate finding picks its severity by what it is about: `error` when the push fails or sends something wrong, `warning` when something breaks (a past date, a stale id, a link that 404s for students), `note` when nothing breaks but the course may not be laid out as a student needs (an item in no module). Only errors fail `org-canvas-validate-batch`; notes are listed last and counted apart (#413)
- A report-producing command renders through `org-canvas--report-display`, which prints to stdout under `noninteractive` instead of displaying a buffer a batch Emacs cannot show (#155, #169); `org-canvas-validate-batch` exits non-zero on errors, as `org-canvas-diff-batch` does on drift
- A content type that accumulates course copies declares `:duplicate-titles t` on its property registration; the offline validator then compares its titles with `(OLD)`, semester stamps and Canvas's `(n)` suffixes stripped, and calls a scheduled twin an error (#164). Child levels stay out, since their titles repeat by design
- Validation reports links into another course on the same instance, and top-level `/users/` and `/accounts/` routes: warnings (a shared department page is occasionally meant), never push-only, listed individually and grouped by target. The scan runs once per distinct file in `org-canvas--validate-run-all-specs`, not per spec — four files are registered by two features each and would report twice (#172)
- Files pull has three modes (`org-canvas--file-pull-mode`): `fresh` builds the folder tree, `flat` upserts top-level headings, `hierarchical` upserts folder by folder, matching by Canvas id wherever a heading sits (#158). Nothing local is deleted for being absent from Canvas — that is `org-canvas-cleanup-orphans`
- An optional dependency loads through `org-canvas--require-optional`, never `(require 'x nil t)`: NOERROR covers a missing file only, so a feature that raises while loading takes org-canvas down with it (#157)
- Full list: api-interaction.org, "Error Handling Conventions"

## Conflict Resolution
- Baseline is `org-canvas--conflict-baseline`: the entry's `CANVAS_UPDATED_AT`, falling back to the file's `#+LAST_SYNCED` header, which pushes advance forward-only (#48, #104)
- The payload-hash skip is drift-aware: a matching hash proves only the local side unchanged, so a remotely-modified entry leaves the skip path (`org-canvas--sync-remote-drifted-p`)
- `org-canvas--conflict-check` returns `(cons 'conflict REMOTE-RESPONSE)`; `org-canvas--resolve-conflict` prompts push/pull/skip (capitals apply to all, remembered in the run context); push returns `'pulled` when the user pulls
- `org-canvas-conflict-strategy` is the caller's seam and is never rebound by the pipeline; under `noninteractive` the fallback is `skip` (#72). Tests that exercise the prompt must bind `noninteractive` to nil
- A `:post-fn` that writes to Canvas again must say so with `org-canvas--finalize-note-remote-write`, on the context it is handed (#124)
- Full rules: api-interaction.org, "Conflict Detection"

## Org Interaction
- Always `org-back-to-heading t` before property access; use markers for position tracking; save after modifying
- Open course files only with `org-canvas--find-file-noselect`; `org-canvas--path` returns truenames; `org-canvas--ensure-buffer-fresh` guards property writes and saves in batch — decisions.org, "Org Files in Batch Mode"
- Property drawers are stripped before the body link resolver runs — design.org, "Key Lessons"

## Complexity
Keep functions below cognitive complexity 15 (`eldev complexity`; the pre-push hook budgets two pre-existing macros). Patterns: single-item helpers, data-driven loops (`pcase` over a field-spec constant), conflict extraction, `org-canvas--timeout-error-p`, payload-wrapping helpers, pull property setters — design.org, "Complexity Management".
