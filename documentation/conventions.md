# Code Conventions

Naming, logging, JSON/API, error handling, conflict resolution, Org interaction and complexity. Imported by [CLAUDE.md](../CLAUDE.md), so every session loads it.

Each rule is the sentence a contributor needs before writing code; the pointer names the heading in [architecture/](architecture/) that holds the reasoning.

## Naming
- Private functions: `org-canvas--function-name` (double dash); public: `org-canvas-function-name`
- Entry points: `org-canvas-sync-{feature}` (push), `org-canvas-pull-{feature}` (pull), `org-canvas-sync-{singular}-at-point`, `org-canvas-sync-{singular}` (one heading by exact title or, with `'canvas-id`, by stamp — the at-point runtime at that position; `org-canvas-sync-headings` takes a list, #287), `org-canvas-pull-{singular}` (its pull twin, no prompt, file saved; `org-canvas-pull-headings` takes a list, #346), `org-canvas-delete-all-{feature}`, `org-canvas-delete-{feature}-at-point`, `org-canvas-prune-{feature}` (generated with delete-all: deletes Canvas items whose ID is absent from the org file, after confirmation) — module-developer-guide.org, "Naming Conventions"

## Logging
- Use the in-tree logger in `lisp/org-canvas-core-log.el`: `org-canvas--log-{trace,debug,info,warning,error}`, `org-canvas--logger-{set-level,set-file,set-handlers}`; stage markers take a `[Stage N: StageName]` prefix
- Secrets never reach the log: every line passes through `org-canvas--log-redact`, and plz-error structs are scrubbed by `org-canvas--scrub-plz-error` before entering signal data — api-interaction.org, "The Token's Path"
- Secrets never reach the *user* either: a message carrying text the package did not write itself (`error-message-string` above all) goes through `org-canvas--user-message`, never a bare `message`, and `org-canvas--pull-summary-record` masks its `:error` on the way in (#154) — api-interaction.org, "Redaction Reaches the User, Not Only the Log"
- Secrets never reach *backtraces* either: the token is never a function argument, so `org-canvas--api-request-headers` resolves the Authorization header inside the transport function, which also re-signals whatever plz raises (#178) — api-interaction.org, "Redaction Cannot Reach a Backtrace"
- A log handler never signals: the file is appended unvisited and unlocked, a failure noted once (#443)
- `org-canvas--save-buffer` is a no-op on unmodified buffers; each sync command clears the log unless `org-canvas--inhibit-log-clear` is bound (the master sync binds it)

## JSON/API
- Nested payloads (assignments, pages, modules, rubrics, files) use hash-tables; flat payloads (discussions, announcements, quizzes, outcomes) use alists; both serialize via `json-encode` — design.org, "Payload Format: Hash-Tables vs Alists"
- `org-canvas-api-request` decodes with `json-read`, so a JSON array arrives as a **vector**: walk a reply with `cl-find-if`/`seq-*`, or coerce with `(append reply nil)` first. An empty vector is also non-nil, so it passes a `when` guard (#153). A list-returning test stub hides both — api-interaction.org, "A Reply Is Parsed JSON: Arrays Are Vectors"
- Booleans: `t` for true, `:json-false` for false (nil is JSON null, which Canvas reads differently). Org properties are strings — compare with `"true"`/`"false"` — design.org, "Boolean Handling"
- The codebase passes `'POST`; `org-canvas-api-request` lowercases it for plz (`'post`). Uppercase or keyword methods to plz are a 400 — api-interaction.org, "Method Symbol Convention"
- GraphQL (post policies, posting grades; #202): a read goes through `org-canvas--graphql-query`, which lifts the read-only guard for that one request and refuses a mutation; a write goes through `org-canvas--graphql-mutate`, which keeps the guard and the dry run. A 200 reply carrying `errors` is an `org-canvas-api-error` — decisions.org, "Post Policies Are GraphQL"

## Error Handling
- Wrap API calls in `condition-case`; continue processing other items if one fails. One concise message per failure: 4xx bodies parse through `org-canvas--api-error-message`, detail logs at DEBUG, exactly one `[ERROR]` line per item — api-interaction.org, "Error Handling Conventions"
- Read an error datum with `org-canvas--api-error-datum`, never `(cdr err)`: plz signals a list of a label and the struct, so a bare `(cdr err)` loses the status, body and cookies (#152) — api-interaction.org, "The Shape of an Error Datum"
- The transport owns the retries: timeout → search Canvas and retry (`org-canvas--timeout-error-p`); 404 on PUT → retry as POST; 429 or rate-limit 403 → retry (`org-canvas-rate-limit-retries`, `org-canvas-rate-limit-wait`); 401 → expired-token message — api-interaction.org, "Rate Limiting and Retries"
- A non-rate-limit 403 is `org-canvas-permission-error`, a skip named in the closing line (#155); a 404 "disabled for this course" is `org-canvas-feature-disabled-error`, a skip too (#486). A pull's per-item and sub-read records use `org-canvas--api-skip-error-p` the same way (#390, #397) — api-interaction.org, "A Role Refusal Is a Skip"
- `org-canvas-permission-error` has **two parents**: the credentials error (so safe-pull counts it a skip) *and* `org-canvas-api-error`, so per-item handlers meaning "this request failed" still cover 403. Check `grep -rn "(org-canvas-api-error" lisp/` when adding an error symbol (#171) — api-interaction.org, "Error Handling Conventions"
- A course can be marked read-only (`org-canvas-read-only`, set in the credentials file): `org-canvas--check-writable` refuses any non-GET at the transport, before the request is built, so every present and future writer is covered (#163). Reads, status and diff are untouched — api-interaction.org, "Read-Only Courses". `org-canvas-write-allowlist` scopes one identity to files and features (#550) — api-interaction.org, "The Write Allowlist"
- Deferrable rejections (drop rules exceeding the group's assignment count) count as `:deferred` (`org-canvas--sync-deferred-error-p`), not failures
- `org-canvas--safe-sync` skips missing `.org` files; `org-canvas--preflight-check` runs before any sync
- Property declarations that change what a push sends: `:canvas-owned t` (#184) — decisions.org, "External-Tool (LTI) Assignments — and No Turnitin"; `:intent-of` (#293) — decisions.org, "A Wanted Document Processor Is Declared, Compared, Never Sent"; `:pull-only t` — design.org, "One Registry Entry per Feature"; `:read-only-values` (#167) — decisions.org, "Canvas-Owned Submission Types"
- Validate holds back push-only findings on a read-only course (#168) and picks severity by what a finding is about: `error` when the push fails, `warning` when something breaks, `note` when the layout is merely questionable (#413) — decisions.org, "Validation Is Read-Only Aware" and "A Published Heading No Module Holds Is a Validate Note". A finding a course may keep carries a check id (`org-canvas--validate-checks`, `org-canvas--validate-checked`, `VALIDATE_ACCEPT` with a reason; #538) — decisions.org, "Validate Accepts One Check With a Reason"
- A report-producing command renders through `org-canvas--report-display`, which prints to stdout under `noninteractive`; `org-canvas-validate-batch` exits non-zero on errors, as `org-canvas-diff-batch` does on drift (#155, #169) — decisions.org, "A Report Command Describes Itself in Batch"
- A content type that accumulates course copies declares `:duplicate-titles t`; validation compares titles with `(OLD)`, semester stamps and Canvas's `(n)` suffixes stripped and calls a scheduled twin an error (#164); child levels stay out — design.org, "Duplicate and Superseded Titles"
- Validation reports links into another course on the same instance, and top-level `/users/` and `/accounts/` routes, as warnings, listed individually and grouped by target; the scan runs once per distinct file (#172) — decisions.org, "Cross-Course Links Are the Validator's Business"
- Files pull has three modes (`org-canvas--file-pull-mode`): `fresh`, `flat`, `hierarchical`; nothing local is deleted for being absent from Canvas (#158) — pull-system.org, "Files: Three Pull Modes"
- An optional dependency loads through `org-canvas--require-optional`, never `(require 'x nil t)`: NOERROR covers a missing file only (#157)

## Conflict Resolution
- Baseline is `org-canvas--conflict-baseline`: the entry's `CANVAS_UPDATED_AT`, falling back to the file's `#+LAST_SYNCED` header, which pushes advance forward-only (#48, #104)
- The payload-hash skip is drift-aware: a matching hash proves only the local side unchanged, so a remotely-modified entry leaves the skip path (`org-canvas--sync-remote-drifted-p`)
- `org-canvas--conflict-check` returns `(cons 'conflict REMOTE-RESPONSE)`; `org-canvas--resolve-conflict` prompts push/pull/skip (capitals apply to all, remembered in the run context); push returns `'pulled` when the user pulls
- `org-canvas-conflict-strategy` is the caller's seam and is never rebound by the pipeline; under `noninteractive` the fallback is `skip` (#72). Tests that exercise the prompt must bind `noninteractive` to nil
- A `:post-fn` that writes to Canvas again must say so with `org-canvas--finalize-note-remote-write`, on the context it is handed (#124) — api-interaction.org, "Conflict Detection"

## Org Interaction
- Always `org-back-to-heading t` before property access; use markers for position tracking; save after modifying
- Open course files only with `org-canvas--find-file-noselect`; `org-canvas--path` returns truenames; `org-canvas--ensure-buffer-fresh` guards property writes and saves in batch — decisions.org, "Org Files in Batch Mode: Truenames and Fresh Buffers"
- Property drawers are stripped before the body link resolver runs — design.org, "Property Drawers Must Not Reach the Body Link Resolver"

## Complexity
Keep functions below cognitive complexity 15 (`eldev complexity`; the pre-push hook and CI allow no exceptions). Patterns: single-item helpers, data-driven loops (`pcase` over a field-spec constant), conflict extraction, `org-canvas--timeout-error-p`, payload-wrapping helpers, pull property setters — design.org, "Complexity Management".
