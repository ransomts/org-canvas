# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

org-canvas is an Emacs Lisp package that synchronizes course content from Org Mode files to Canvas LMS via its REST API. Org files are the "source of truth": instructors design courses in Org Mode and push changes to Canvas.

Content types, one module each: assignments (including LTI/external-tool assignments such as Gradescope), classic quizzes, new quizzes, pages, modules, rubrics, outcomes, discussions, announcements, files, assignment groups, group categories, calendar events, sections (pull-only), grading periods (pull-only), grading schemes, per-section date overrides, per-student quiz accommodations, and course settings. Submission grading (`org-canvas-submissions.el`: view, comment, download attachments, push grades and rubric assessments) is a separate feature, not a content type.

This file is the short guide: conventions, commands, hard rules, pointers. The reasoning behind every rule — the issue post-mortems that used to be inline here — lives in `documentation/architecture/` (see "Where the Narratives Live"). When a rule cites a heading, read it before touching that code.

## Build Commands

This project uses Eldev (Elisp Development Tool):

```bash
eldev prepare        # Install dependencies (first run, or after Eldev changes)
eldev test           # Run tests; the last line prints the spec count (about 40 s)
eldev compile        # Compile Elisp files
eldev lint           # Run linter
eldev complexity     # Cognitive complexity report (target: 0 functions above 15)
eldev package        # Create distributable package
eldev clean all      # Clear the Eldev cache — mandatory after editing a macro
```

**Always run `eldev lint` before committing or pushing.** Fix any warnings before proceeding. CI lints on Emacs 30.1, whose checkdoc rejects a third-person verb such as "holds" anywhere in a docstring's first line; a newer local Emacs accepts it, so keep first lines imperative throughout (PRs #89/#90/#189/#258/#259 went red on exactly this). The pre-push hook lints under the pinned Emacs 30 for that reason; a bare `eldev lint` on a newer Emacs is not the CI check.

`org-canvas.el` at the repository root is a symlink to `lisp/org-canvas.el` so the package linter finds `Package-Requires` while the load path stays in `lisp/` (testing.org, "Eldev Test Configuration"). CI runs the tests on Emacs 29.3, 29.4 and 30.1; the pre-push hook runs 29 and 30 through nix-shell. CI runs on every pull request whatever its base, and `main` requires the checks on an up-to-date head, so a PR stacked on another branch still needs a rebase once the lower one merges — prefer independent PRs and merge them one at a time.

CI (`.github/workflows/ci.yml`) runs Emacs 30.1's suite in one job, with coverage for Codecov; Emacs 29.3 and 29.4 each run in three shards (`.github/workflows/test-shards.yml`, files split by `scripts/test-shard.sh` using `test/shard-weights.txt`), and the isolation check runs in three shards too. An aggregator job per version reports under the check names branch protection requires (`test (29.3)`, `test (29.4)`, `isolation`), failing unless every shard passed; rename a job only together with the protection rule. testing.org, "CI Pipeline".

**Changelog: one fragment per PR, never CHANGELOG.org.** A PR adds `changelog.d/<issue>.org` (a `* Added`/`* Changed`/`* Fixed` heading, then `- #<issue> ...` items; `changelog.d/README.org`), which CI validates with `scripts/changelog-collect.py --check`; at release time `scripts/changelog-collect.py` folds them into CHANGELOG.org and deletes them. When every PR wrote the top of the same section, each merge conflicted with the next.

## Architecture

### Module Structure

```
lisp/
├── org-canvas.el                # Entry point: requires everything, sync/pull/delete tiers, at-point dispatch
├── org-canvas-status.el         # org-canvas-status (local overview) and the content-type table
├── org-canvas-publish.el        # Bulk publish/unpublish and PUBLISH_AT releases (requires modules)
├── org-canvas-adopt.el          # org-canvas-adopt-at-point (requires diff)
├── org-canvas-orphans.el        # org-canvas-cleanup-orphans
├── org-canvas-browse.el         # org-canvas-browse-at-point: open the heading's Canvas web page (no API)
├── org-canvas-core.el           # Meta-require for all core-* files
├── org-canvas-core-config.el    # Config, constants, enum values, property and feature registries
├── org-canvas-core-log.el       # In-tree logger (org-canvas--log-*), secret redaction
├── org-canvas-core-api.el       # API requests, curl PATCH fallback, rate limiting, pacing, uploads
├── org-canvas-core-org.el       # Org property/buffer helpers, batch freshness, timestamps, link resolution
├── org-canvas-core-html.el      # HTML export (links, images) and HTML→Org conversion on pull
├── org-canvas-core-pull.el      # Pull helpers, file-URL rewriter, pull macros, pull summary
├── org-canvas-core-macros.el    # Declarative parse and payload DSL (define-parse, define-payload)
├── org-canvas-core-sync.el      # Sync pipeline macro, push/finalize infra, snapshots, duplicate guard
├── org-canvas-core-conflict.el  # Interactive conflict resolution UI
├── org-canvas-core-delete.el    # Delete/prune infrastructure and macros
├── org-canvas-credentials.el    # Secrets (API token, course ID) — gitignored
├── org-canvas-validate.el       # Offline validation engine (no API contact)
├── org-canvas-diff.el           # Read-only drift report (org-canvas-diff, org-canvas-diff-mode)
├── org-canvas-setup.el          # Setup wizard (org-canvas-init)
├── org-canvas-transient.el      # Transient command menu
├── org-canvas-submissions.el    # Submission viewer and grading
├── org-canvas-quiz-submissions.el # Read-only table of a classic quiz's attempts (requires submissions)
├── org-canvas-peer-reviews.el   # Read-only tables of who reviews whom on an assignment (requires submissions)
├── org-canvas-submissions-status.el # Grading queue: per-column submitted/graded/posted counts and the next action (requires submissions)
├── org-canvas-messages.el       # Send Canvas conversations from messages.org; never part of org-canvas-sync
├── org-canvas-new-quiz-items.el # New Quizzes item/question pipeline (sub-module of new-quizzes)
├── org-canvas-{feature}.el      # Feature modules: announcements, assignment-groups, assignments,
│                                #   calendar, discussion-replies (pull-only), discussions, files,
│                                #   grading-periods (pull-only), grading-schemes, group-categories, groups (pull-only),
│                                #   gradebook (pull-only), people (pull-only), rubric-results (pull-only),
│                                #   quiz-results (pull-only), quiz-accommodations,
│                                #   modules, new-quizzes, outcomes, pages, quizzes, rubrics, sections, settings
└── org-canvas-autoloads.el      # Generated by Eldev's autoloads plugin; gitignored (a copy is
                                 #   checked in at test/org-canvas-autoloads.el)
```

`ls lisp/` is the authority; update this tree when a file is added.

### Dependency Rules

- All feature modules require `org-canvas-core`
- Feature modules must NOT depend on each other. The one sanctioned exception is a sub-module: `org-canvas-new-quizzes` requires `org-canvas-new-quiz-items`, which itself requires only core
- `org-canvas-core` must NOT import any feature modules (prevents circular deps)
- `org-canvas.el` orchestrates by requiring all modules
- Command files (status, publish, adopt, orphans, browse, diff, validate, submissions, quiz-submissions, peer-reviews, submissions-status, messages) sit above the feature modules: they require core and may require the feature module they drive (publish requires modules, adopt requires diff, quiz-submissions, peer-reviews and submissions-status require submissions); no feature module may require a command file
- diff.el declares, never requires, the two modules.el functions the sync adopts module items with (`org-canvas--module-item-parse-entry`, `org-canvas--module-item-same-content-p`), so its pairing agrees with the sync by construction (#299)
- A feature module may name a validate.el function by symbol — `:structural-fn #'org-canvas--validate-drop-rules` on its property registration, resolved when validation runs — and may `declare-function` a function it must call from another module (assignments does this for `org-canvas--override-fetch` in sections.el). Declare; never require another feature

### 4-Stage Pipeline Pattern

Every feature module follows this consistent pattern:

1. **Parse** (`org-canvas--{feature}-parse-entry`) - Extract data from Org heading properties
2. **Build Payload** (`org-canvas--{feature}-build-payload`) - Convert to Canvas API format
3. **Execute** (`org-canvas--{feature}-push-to-api`) - Call API with timeout recovery
4. **Finalize** (`org-canvas--{feature}-finalize`) - Save CANVAS_ID and LAST_SYNCED to Org file

### Shared Infrastructure (one example each)

Option lists, generated names and the reasoning are in `documentation/architecture/module-developer-guide.org` (step by step) and `design.org` (why). These are the real calls from announcements.el and pages.el.

```elisp
;; Feature registry (core-config): drift check, duplicate guard, diff, prune, pull-at-point.
;; Must precede the module's org-canvas-define-sync (api-interaction.org, "Feature Registry URL Resolution").
(org-canvas-register-feature
 :name "Announcements" :endpoint "discussion_topics"
 :file-var 'org-canvas-announcements-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title
 :list-params '(("only_announcements" . "true")))

;; Property registry (core-config): one declaration feeds parse, validate, diff and the manual.
(org-canvas-register-properties "announcements"
  :label "Announcements" :file-var 'org-canvas-announcements-file
  :query "LEVEL=1"                       ; MUST match the sync's heading level
  :body-api-key "message"                ; lets org-canvas-diff compare bodies
  :properties
  `((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
     :api-key "published" :boolean-json t)))

;; Declarative parse (core-macros): generates read-props / transform-props / parse-entry
(org-canvas-define-parse announcement
  :body :message
  :properties (("PUBLISHED" :published :type boolean :default t)
               ("POST_AT" :delayed_post_at :type timestamp)))

;; Declarative payload from the registry (core-macros)
(org-canvas-define-payload group-category
  :registry-key "group-categories" :format alist :title-key :title :title-api-key name)

;; Sync pipeline (core-sync): generates org-canvas-sync-announcements and -at-point
(org-canvas-define-sync announcements
  :file org-canvas-announcements-file
  :parse #'org-canvas--announcement-parse-entry
  :build #'org-canvas--announcement-build-payload
  :endpoint "discussion_topics"          ; auto push/finalize; :push/:finalize override
  :pull-item-fn #'org-canvas--announcement-pull-item)   ; enables "pull" in the conflict prompt
;; :hash-extra is REQUIRED for a module whose children sync inside finalize (modules, quizzes,
;; new-quizzes) — design.org, "Macro Options for Child State".

;; Delete-all + prune, and delete-at-point (core-delete)
(org-canvas-define-delete-all pages
  :endpoint "pages" :file org-canvas-pages-file
  :id-field 'url :id-property "CANVAS_URL"
  :skip-fn (lambda (item) (eq (alist-get 'front_page item) t)))
(org-canvas-define-delete-at-point page :endpoint "pages/%s")
```

Registry keys that bite: `:remote-fn` when Canvas returns a value nested or renamed (without it every item reports drift), `:compare-p` to exclude a property for an entry, `:query` matching the heading level, `:modified-field` for features whose `updated_at` moves on metadata touches (files use `modified_at`), `:web-pages` naming where a heading lives in the Canvas web interface (`org-canvas-register-web-pages` for a file with no feature entry, #292; `org-canvas-register-pull-feature` likewise gives such a file pull-at-point and adopt-at-point, #297, and with `:drift-report` a section of the drift report, #313), `:list-params` and `:item-params` when a read needs a query parameter to answer what Org holds (assignments read with `override_assignment_dates=false`, or one student's extension comes back as the assignment's own dates, #273) — design.org, "The Property Registry"; api-interaction.org, "Feature Registry URL Resolution"; decisions.org, "Assignment Reads Ask for the Assignment's Own Dates".

Modules using `org-canvas-define-parse`: announcements, pages, calendar, group-categories, assignment-groups. Using `org-canvas-define-payload`: group-categories, calendar, pages, announcements, grading-schemes. Custom push (non-standard URLs): group-categories, calendar; custom push because Canvas has no update (grading-schemes: a stamped scheme is compared and skipped, never PUT). Custom sync loops (not macro-based): overrides, quiz accommodations (a table under each quiz reconciled against the quiz's submissions; values are absolute, zeros clear), and the child reconcilers inside quizzes, new-quizzes and modules — see Hard Rule 1. Two levels of one file (outcomes): one `org-canvas-define-sync` per level, the second naming the first with `:first`, and `:prepare` for a value the run fetches once (the root group). A push that owns its change detection (files, whose stored hash is bytes and metadata in two halves): `:hash push` keeps the runner from skipping on or stamping PAYLOAD_HASH, the push answers `skip` when nothing is to send, and `:dry-run push` lets it preview its own tiers under a dry run. Pull-only: sections, grading-periods.

### The Run Context

One sync run owns one context plist (`org-canvas--sync-make-ctx`, keys in `org-canvas--sync-ctx-keys`): the remote snapshot, the pipeline functions, the counters, and every flag or list the run sets for itself — the capital answers at conflict and duplicate prompts, the module's pull function, the ids modules moved and files changed. It is threaded, never global, so nothing a run decides can reach the next one (#72, #141). Push, finalize and `:post-fn` take it as an optional last argument; `org-canvas--sync-run-pipeline` returns it, which is how the master sync gets the module items left pending. Add a key to `org-canvas--sync-ctx-keys` before using it, or `plist-put` has nothing to mutate in place.

What stays dynamically bound is the *caller's* seam, set around a command by whoever invokes it: `org-canvas--dry-run`, `org-canvas-conflict-strategy`, `org-canvas--file-force-upload`, `org-canvas--inhibit-log-clear`, and the master sync's aggregates.

### Sync State

`CANVAS_ID` present → UPDATE (PUT); absent → CREATE (POST). Finalize also stamps `LAST_SYNCED`, `CANVAS_UPDATED_AT` (the conflict baseline, Canvas's clock) and the sync runner stores `PAYLOAD_HASH` (skip optimization). Write them with `org-canvas-org-save-sync-state`; pull paths drop `PAYLOAD_HASH` and restamp `CANVAS_UPDATED_AT` via `org-canvas--conflict-pull-local` — by hand, the entry re-flags as drifted forever while looking clean.

### Where the Narratives Live

Each document under `documentation/architecture/`, then the topics it holds, one per line and in alphabetical order, so a pull request that adds a topic touches a line of its own. Issue numbers are in parentheses.

**`design.org`**

- complexity patterns
- drop-rules two-phase sync
- global sync summary (#66)
- `:hash-extra`
- key lessons (markers, `save-excursion`, upload nulls, unibyte, property drawers, question body text)
- payload builder
- Pipeline macros
- property registry (`:remote-fn` #61/#62, `:compare-p` #93)

**`api-interaction.org`**

- conflict baseline and strategy rules (#48, #72, #86, #104, #124)
- copy-pasteable curl commands for debugging
- dry-run rules (#34, #84)
- duplicate-title guard and adopt-at-point (#85, #101)
- error-handling rules
- feature registry URLs and `:modified-field` (#87, #94)
- file re-upload tiers (#49, #70, #71, #77)
- plz and the PATCH fallback (#13)
- quiz questions API
- upload quirks

**`decisions.org`**

- a classic quiz pulls whole into one heading — pull-at-point, the diff's `p` on an EXTRA row, adopt filling a stub, `:pull-whole-entry` — and numbers a question name it cannot tell apart (#295)
- a heading opens its Canvas page from `:web-pages` rules its module declares (#292)
- a MOVED row stamps the item id on the heading it paired (`s`, `org-canvas-diff-stamp-moves`), and a stamped move is an uncounted RELOCATE row saying the sync recreates the item with a new id (#342, #343)
- a New Quiz item is pushed only as a type it can build: fill-in-the-blank invalid, hot-spot pull-only, an unknown TYPE a failed item (#337, #340)
- a New Quiz pull writes its instructions where the push reads them, and adoption stamps `assignment_id` first (#309)
- a New Quiz pulls whole through a pull-only entry (`org-canvas-register-pull-feature`) kept out of the feature registry (#297)
- a pull takes its heading by name too, with no prompt, through the at-point pull's own pieces (#346)
- a push compares what Canvas stored with what it sent, and a new survey says whether it is anonymous (#349)
- a push takes its heading by name and the at-point runtime records its outcome (#287)
- a refresh reports what changed and keeps a departed student's heading for the work under it (#282)
- a re-pull keeps typed scores and the summary table alone still asks (#281)
- a survey's results are its answers by text, a graded survey's included, and the quiz-results pull counts what it skipped (#347)
- a wanted document processor declared, compared and never sent (#293)
- assignment reads ask for their own dates and quizzes do not need to (#273)
- batch reports (#155, #169)
- batch-mode file handling (#97, #121, #188, #249)
- body headings and the pull entry-count guard (#175)
- bulk publish (#47) and a new header's flag PUT after its POST (#279)
- by-design extras (#98, #102, #111)
- Canvas-owned submission types (#167)
- comments and drafts as paragraphs and short CONFLICT values (#264)
- document processors read by GraphQL once per command, REST `asset_processors` the fallback (#350)
- drift report bodies (#83)
- drift report rows deleted in batch, EXTRA only, after safety checks and a JSON snapshot (#345)
- drift report verbs (#103)
- every browse address checked against canvas-lms's routes, and a module SubHeader opening its module (#300)
- EXTRA assignments and groups described from the lists already read and a group weight total line that is not drift (#296)
- hard-break markup dropped from bodies (#266)
- LTI assignments and no Turnitin, the Turnitin document processor as a Canvas-owned property (#184)
- module items adopt a twin before POST and the drift report lists item twins (#177, #179), the end-of-run retry pass included (#308)
- module items reconcile moves (#105)
- New Quiz items join the drift report as a child pass paired within their quiz as the sync adopts them, with rows whose verbs know the quiz (#322)
- New Quiz items pull into a heading (the body's first paragraph) and the prompt under it, rewritten in place with their answer list kept (#333)
- New Quiz settings live under `quiz_settings`, found by a live probe: minutes as seconds, attempts and scoring as `multiple_attempts`, sent only when set and compared wherever the reply carries the object (#321)
- New Quizzes join the drift report through their pull-only entry's `:drift-report`, compare a setting only where the reply carries it, and leave the Assignments extras (#313)
- pending creates counted apart from drift and moved module items paired on the link's description (#294)
- quiz publish sequencing (#59)
- read-only validation (#168)
- rubric assessments from the grading file (#250) and their comments as items under the table (#263)
- rubrics update in place (#123) and keep their criterion ids and assignments (#255, #256)
- similarity reports read per column from GraphQL's `ltiAssetReportsConnection`, never `hasPlagiarismTool` or `turnitinData`, and counted per row (#351)
- stamp adoption for a CHANGED row with nothing to compare (#257)
- submission commands take their target as an argument and prompt only on nil (#280)
- the accommodations table sits below a quiz's own text and never reaches the description from any layout (#298)
- the drift report pairs module items by content as the sync adopts them, declaring the modules.el parser rather than requiring it (#299)
- the grading queue counts rows, never columns (#283)
- the roster marks who left on Canvas's word and never on a misread list (#290)
- the rubric in full in the grading file (#265)
- the syllabus is the text above the first sub-heading and `** Navigation` never reaches it (#275)
- timestamp precision (#176)

**`pull-system.org`**

- Pull macros and helpers
- pull-at-point (#67)
- single-item pull registration
- `:skip-fn` reporting and the front page (#81, #82)
- the settings pull keeps the tab list a failed read cannot refresh (#277)

**`testing.org`**

- Test helpers and generators, test isolation guards, Emacs 29/30 matrix and skipped tests, JUnit and Codecov, Eldev layout, paren-imbalance debugging

**`module-developer-guide.org`**

- Adding a module, step by step

**`coverage.org`**

- how to refresh the map
- the ranked list of gaps deliberately left undone (2026-09-11)
- what org-canvas covers, touches one way, or never touches in Canvas
- the write paths to watch on first live use

**`plans/`, `specs/`**

- Historical planning documents (2026-03/04)

## Code Conventions

### Naming
- Private functions: `org-canvas--function-name` (double dash); public: `org-canvas-function-name`
- Entry points: `org-canvas-sync-{feature}` (push), `org-canvas-pull-{feature}` (pull), `org-canvas-sync-{singular}-at-point`, `org-canvas-sync-{singular}` (one heading by exact title or, with `'canvas-id`, by stamp — the at-point runtime at that position; `org-canvas-sync-headings` takes a list, #287), `org-canvas-pull-{singular}` (its pull twin: Canvas's version of one named heading, no prompt, file saved; `org-canvas-pull-headings` takes a list, #346), `org-canvas-delete-all-{feature}`, `org-canvas-delete-{feature}-at-point`, `org-canvas-prune-{feature}` (generated with delete-all: deletes Canvas items whose ID is absent from the org file, after confirmation)

### Logging
- Use the in-tree logger in `lisp/org-canvas-core-log.el`: `org-canvas--log-{trace,debug,info,warning,error}`, `org-canvas--logger-{set-level,set-file,set-handlers}`; levels trace, debug, info, warning, error, fatal
- Stage markers: `[Stage N: StageName]` prefix
- Secrets never reach logs: every line passes through `org-canvas--log-redact` (Bearer tokens, session/csrf/token cookie or query values); plz-error structs are scrubbed by `org-canvas--scrub-plz-error` before entering signal data
- Secrets never reach the *user* either: a message carrying text the package did not write itself (`error-message-string` above all) goes through `org-canvas--user-message`, never a bare `message`, and `org-canvas--pull-summary-record` masks its `:error` on the way in (#154). The echo area, `*Messages*` and batch stderr are shared sinks too
- Secrets never reach *backtraces* either: a backtrace prints function arguments verbatim, so the token is never one. `org-canvas--api-request-headers` resolves the Authorization header inside the transport function (`org-canvas--api-execute-request`, `org-canvas--api-curl-patch-config`), which also re-signals whatever plz raises so plz's own frames are gone before an error escapes (#178) — api-interaction.org, "Redaction Cannot Reach a Backtrace"
- `org-canvas--save-buffer` is a no-op on unmodified buffers; each sync command clears the log unless `org-canvas--inhibit-log-clear` is bound (the master sync binds it)

### JSON/API
- Nested payloads (assignments, pages, modules, rubrics, files) use hash-tables; flat payloads (discussions, announcements, quizzes, outcomes) use alists; both serialize via `json-encode`
- `org-canvas-api-request` decodes with `json-read`, so a JSON array arrives as a **vector**: walk a reply with `cl-find-if`/`seq-*`, or coerce with `(append reply nil)` first. An empty vector is also non-nil, so it passes a `when` guard (#153). A list-returning test stub hides both
- Booleans: `t` for true, `:json-false` for false (nil is JSON null, which Canvas reads differently). Org properties are strings — compare with `"true"`/`"false"`
- The codebase passes `'POST`; `org-canvas-api-request` lowercases it for plz (`'post`). Uppercase or keyword methods to plz are a 400
- GraphQL (post policies, posting grades; #202): a read goes through `org-canvas--graphql-query`, which lifts the read-only guard for that one request and refuses a mutation; a write goes through `org-canvas--graphql-mutate`, which keeps the guard and the dry run. A 200 reply carrying `errors` is an `org-canvas-api-error` — decisions.org, "Post Policies Are GraphQL"

### Error Handling
- Wrap API calls in `condition-case`; continue processing other items if one fails
- One concise message per failure: 4xx bodies parse through `org-canvas--api-error-message`, detail logs at DEBUG, exactly one `[ERROR]` line per item
- A course can be marked read-only (`org-canvas-read-only`, set in the credentials file): `org-canvas--check-writable` refuses any non-GET at the transport, before the request is built, so every present and future writer is covered (#163). Reads, status and diff are untouched
- Read an error datum with `org-canvas--api-error-datum`, never `(cdr err)`: plz signals `plz-http-error` with a *list* of a label and the struct, so a bare `(cdr err)` fails every `plz-error-p` guard and loses the status, body and cookies (#152)
- Timeout → search Canvas for the item, retry if needed (`org-canvas--timeout-error-p` is the predicate); 404 on PUT → retry as POST; 429 or rate-limit 403 → retry (`org-canvas-rate-limit-retries`, `org-canvas-rate-limit-wait`); 401 → expired-token message; other 403 → `org-canvas-permission-error`, which `org-canvas--safe-pull` counts as a skip and names in the closing line (#155)
- `org-canvas-permission-error` has **two parents**: the credentials error (#155's skip handling) *and* `org-canvas-api-error`, so per-item handlers meaning "this request failed" still cover 403. With the credentials parent alone they silently stopped, and one cross-course file link in one page body aborted a 46-page pull (#171). When adding an error symbol, check `grep -rn "(org-canvas-api-error" lisp/` for handlers that should still catch it
- Deferrable rejections (drop rules exceeding the group's assignment count) count as `:deferred` (`org-canvas--sync-deferred-error-p`), not failures
- `org-canvas--safe-sync` skips missing `.org` files; `org-canvas--preflight-check` runs before any sync
- A property only Canvas may set at all (an assignment's document processor) declares `:canvas-owned t` beside its `:remote-fn`: pull writes it, diff compares it even on a silent heading, validate warns when it is typed before the first sync, and the payload never carries it (#184)
- What the course *should* hold of such a property is a second property declaring `:intent-of` the observed one (`WANT_DOCUMENT_PROCESSOR` → `DOCUMENT_PROCESSOR`): never pulled, parsed or sent; validate warns on a stamped heading the last pull left unmet, diff counts it as a CHANGED field row and shows the reverse as an uncounted NOTE row (#293)
- A property a pull writes and no push reads (when an announcement or a reply was posted, who wrote it) declares `:pull-only t`: validate keeps the parse check but drops the past-timestamp warning, since such a timestamp is in the past by nature, and the manual marks it as Canvas's to set
- A property whose Canvas field can come back holding a value only Canvas may set declares those as `:read-only-values` beside `:values` (assignments' SUBMISSION, for the `online_quiz` a quiz's shadow assignment carries): the validator accepts them, since a pull wrote them, and the module refuses them only where a push is genuinely wrong — a create, not an update (#167)
- On a course marked `org-canvas-read-only`, validate holds back findings that only protect a push (past timestamps, pre-first-sync links), marked with `org-canvas--validate-push-only`, and names the count; `org-canvas-validate-all` keeps them (#168). A finding true of the course itself is never marked
- A report-producing command renders through `org-canvas--report-display`, which prints to stdout under `noninteractive` instead of displaying a buffer a batch Emacs cannot show (#155, #169); `org-canvas-validate-batch` exits non-zero on errors, as `org-canvas-diff-batch` does on drift
- A content type that accumulates course copies declares `:duplicate-titles t` on its property registration; the offline validator then compares its titles with `(OLD)`, semester stamps and Canvas's `(n)` suffixes stripped, and calls a scheduled twin an error (#164). Child levels stay out, since their titles repeat by design
- Validation reports links into another course on the same instance, and top-level `/users/` and `/accounts/` routes: warnings (a shared department page is occasionally meant), never push-only, listed individually and grouped by target. The scan runs once per distinct file in `org-canvas--validate-run-all-specs`, not per spec — four files are registered by two features each and would report twice (#172)
- Files pull has three modes (`org-canvas--file-pull-mode`): `fresh` builds the folder tree, `flat` upserts top-level headings, `hierarchical` upserts folder by folder, matching by Canvas id wherever a heading sits (#158). Nothing local is deleted for being absent from Canvas — that is `org-canvas-cleanup-orphans`
- An optional dependency loads through `org-canvas--require-optional`, never `(require 'x nil t)`: NOERROR covers a missing file only, so a feature that raises while loading takes org-canvas down with it (#157)
- Full list: api-interaction.org, "Error Handling Conventions"

### Conflict Resolution
- Baseline is `org-canvas--conflict-baseline`: the entry's `CANVAS_UPDATED_AT`, falling back to the file's `#+LAST_SYNCED` header, which pushes advance forward-only (#48, #104)
- The payload-hash skip is drift-aware: a matching hash proves only the local side unchanged, so a remotely-modified entry leaves the skip path (`org-canvas--sync-remote-drifted-p`)
- `org-canvas--conflict-check` returns `(cons 'conflict REMOTE-RESPONSE)`; `org-canvas--resolve-conflict` prompts push/pull/skip (capitals apply to all, remembered in the run context); push returns `'pulled` when the user pulls
- `org-canvas-conflict-strategy` is the caller's seam and is never rebound by the pipeline; under `noninteractive` the fallback is `skip` (#72). Tests that exercise the prompt must bind `noninteractive` to nil
- A `:post-fn` that writes to Canvas again must say so with `org-canvas--finalize-note-remote-write`, on the context it is handed (#124)
- Full rules: api-interaction.org, "Conflict Detection"

### Org Interaction
- Always `org-back-to-heading t` before property access; use markers for position tracking; save after modifying
- Open course files only with `org-canvas--find-file-noselect`; `org-canvas--path` returns truenames; `org-canvas--ensure-buffer-fresh` guards property writes and saves in batch — decisions.org, "Org Files in Batch Mode"
- Property drawers are stripped before the body link resolver runs — design.org, "Key Lessons"

### Complexity
Keep functions below cognitive complexity 15 (`eldev complexity`; the pre-push hook budgets two pre-existing macros). Patterns: single-item helpers, data-driven loops (`pcase` over a field-spec constant), conflict extraction, `org-canvas--timeout-error-p`, payload-wrapping helpers, pull property setters — design.org, "Complexity Management".

## Hard Rules

Non-negotiable. Each was learned on a live course; the pointer holds the full story.

1. **Dry run is a per-module obligation, not a shared guarantee** (#34). A custom sync loop guards every write path itself and returns `org-canvas--dry-run-response`; `test/org-canvas-dry-run-test.el` enforces it, so add new sync commands to `org-canvas-dry-run--sync-commands`. — api-interaction.org, "Dry-Run Mode"
2. **Never weaken the test network guard.** test-helper.el refuses unmocked `plz`/`url-retrieve-synchronously`/curl; mock `org-canvas-api-request` instead. — testing.org, "Test Isolation"
3. **Never assert on the shared `*org-canvas-log*` buffer**; `cl-letf` the log functions into a list. — testing.org, "Test Isolation"
4. **PATCH goes through `org-canvas--api-curl-patch`**, never plz (plz sends it as a bodiless GET). Late-policy updates must stay PATCH; PUT 404s (#13). — api-interaction.org, "HTTP Library"
5. **Turnitin was removed deliberately; do not add it back without re-probing.** Its document-processor mode is observed through `DOCUMENT_PROCESSOR`, a `:canvas-owned` property, and never set: a push must never emit `asset_processors`, since an empty array strips one attached by hand (#184). — decisions.org, "External-Tool (LTI) Assignments — and No Turnitin"
6. **A replacement file upload must not delete first** (#77); deleting first made Canvas drop the module items (#71). Never set `org-canvas--file-force-upload` globally. — api-interaction.org, "When a File Is Re-uploaded"
7. **Rubrics update in place** — PUT to `rubrics/ID`, never delete-and-recreate; associations and assessments belong to the id (#123). An update sends every criterion and rating back under its live id (`CANVAS_CRITERION_ID` on the heading; Canvas mints a new id for anything unnamed, orphaning assessments and grading-file rows, #256) and takes all but one assignment association off around the PUT, since Canvas copies a rubric several assignments grade with instead of updating it (#255). — decisions.org, "Rubrics Update In Place" and "Rubric Updates Keep Their Ids and Their Assignments"
8. **`org-canvas--find-file-noselect`, never `find-file-noselect`**, for course files (#121); **`org-canvas--path` returns truenames** and comparisons use them (#97). — decisions.org, "Org Files in Batch Mode"
9. **Collect markers before iterating with buffer modifications**; never `outline-next-heading` in a `while` loop that modifies the buffer. — design.org, "Key Lessons"
10. **Capture the return value of a `save-excursion` helper**; `(point)` afterwards is the old position. Test with 3+ headings. — design.org, "Key Lessons"
11. **`eldev clean all` after editing a macro**; callers keep stale expansions silently. — testing.org, "Common Pitfalls"
12. **A `:skip-fn` must say so**: declare `:skip-reason` beside it; only push and delete-all skip the front page, pull does not (#81, #82). — pull-system.org, "A :skip-fn Must Say So"
13. **Register the feature before `org-canvas-define-sync`** and resolve URLs through `org-canvas--feature-list-url` / `-item-url` / `-list-params` / `-item-params`, never by spelling the course endpoint (#87). A read parameter a feature declares is checked against the spec by the read-parameter contract test (#273). — api-interaction.org, "Feature Registry URL Resolution"
14. **Classic quizzes keep `published` out of the payload**; it is applied in finalize after the questions (#59). — decisions.org, "Quiz Publish Sequencing"
15. **Every counter a run populates must be in `org-canvas--sync-stat-keys`** or it is silently dropped from the summary; custom syncs record a dry run as `:dry-run`, not `:success` (#66). — design.org, "Global Sync Summary"
16. **Children are never pruned unasked** (module items, quiz questions); a misread remote list is `unknown`, never empty (#105). — decisions.org, "Module Items Reconcile Cross-Module Moves"
17. **Publish state belongs to the object**, written where it is declared, never inferred from a module item (#47). — decisions.org, "Bulk Publish"
18. **A property Canvas returns nested or renamed needs a `:remote-fn`**, or the whole feature reports drift forever (#61, #62). — design.org, "The Property Registry"
19. **`org-canvas-diff` must never write**: local bodies export with the OFFLINE flag (#83). Stamp adoption (`a` on a CHANGED row with no compared property differing, `org-canvas-diff-adopt-stamps` for all of them) is a separate verb that writes `CANVAS_UPDATED_AT` only, keeps `PAYLOAD_HASH` and sends nothing; a row where a property differs is never adopted (#257). Stamping a MOVED row (`s`, `org-canvas-diff-stamp-moves`) is likewise a separate verb that writes `CANVAS_ID` only on the heading the report paired, and sends nothing (#342). — decisions.org, "The Drift Report Compares Bodies", "A CHANGED Row With Nothing to Compare Adopts the Stamp", "A MOVED Row Stamps Its Paired Heading"
20. **Before any POST the duplicate-title guard runs** (#85); recovery from a lost stamp is `org-canvas-adopt-at-point` (#101), not a second create. A module item without CANVAS_ID adopts the same-content item its module already holds (`org-canvas--module-item-adopt-twin`, PUT not POST) before any POST; twins are named, never deleted (#179). The same holds for every other create path: quiz questions and New Quiz items adopt through `org-canvas--adopt-child-twin`, New Quizzes through `org-canvas--new-quiz-guard-duplicate`, outcomes through `org-canvas--outcome-update-or-create`, and the generic 404 recovery updates the title's twin before it re-POSTs. A new feature on the generic path declares a `:find-fn`; a new custom push looks its title up before its POST. — api-interaction.org, "Duplicate-Title Guard"; decisions.org, "Module Items Adopt a Twin" and "Every Create Path Adopts a Twin"
21. **Counts in documentation are commands, not numbers** — spec totals, coverage and skipped-test counts drift with every PR (this file was 823 lines of them; #139).
22. **A body never introduces a heading at or above its parent's level** — a pulled HTML heading becomes a `#+begin_hN` block at the `org-canvas--html-to-org` chokepoint and exports back as `<hN>`; never let converted text reach an entry as an Org headline, and never demote instead (quiz and new-quiz extractors stop at any heading; #175). — decisions.org, "A Body Never Introduces a Headline"

## Testing

```bash
eldev test                                                    # All tests; the last line is the spec count
eldev test "core"                                             # Tests matching a pattern (no -p flag)
eldev test -u "on,codecov,dontsend" -U coverage/coverage.json # Per-file coverage; pre-push gate is 99%
python3 scripts/patch-coverage.py                            # After the line above: lisp/ lines added since origin/main that no test runs (codecov/patch)
eldev test -u "on,text,dontsend"                              # Text summary (may end in overflow-error; prefer JSON)
ELDEV_JUNIT=1 JUNIT_REPORT_FILE=test-results.xml eldev test   # JUnit XML for Codecov
grep -rn buttercup-pending test/                              # Specs skipped on Emacs 29.x
scripts/test-each-file.sh                                     # Every test file alone (~2.5 min); --shard K/N for one CI shard
scripts/test-shard.sh 2/3                                     # The files of CI shard 2 of 3 (eldev test $(scripts/test-shard.sh 2/3))
scripts/test-each-file.sh --timings > test/shard-weights.txt  # Refresh the shard weights when shards drift apart
eldev exec -f scripts/graphql-introspect.el                   # Refresh the GraphQL fixture from the live instance, once per semester (test/contract/README.md)
```

Every test file must pass on its own (`scripts/test-each-file.sh`, CI's sharded `isolation` job; #260): test-helper loads the whole package, and a spec that sets global state — the log level above all — restores it.

Layout: `test/test-helper.el` (fixtures, mocks, macros, network guard); `test/org-canvas-core-{config,api,org,html,pull,sync,conflict,delete,usability}-test.el`; `test/org-canvas-test.el` (orchestration); one `test/org-canvas-{feature}-test.el` per module; `test/org-canvas-validate-test.el`; `test/org-canvas-dry-run-test.el`; `test/org-canvas-doc-reference-test.el` (the manual's generated Property Reference); `test/org-canvas-contract-test.el` and `test/org-canvas-graphql-contract-test.el` (REST payloads and every registered read's query parameters against the OpenAPI spec, #273; the GraphQL documents and their variables against the Canvas GraphQL schema, #269); `test/contract/` (both fixtures and their generators; the GraphQL one regenerates from the instance's introspection or the canvas-lms SDL, see its README); `test/mutation/` (mutation-testing harness); `test/docgen/` (generates the Property Reference from the registry).

Utilities (test-helper.el; full reference in testing.org):
- `with-temp-org-buffer` — file-backed temp Org buffer; org functions misbehave in `with-temp-buffer`
- `with-mock-api` — records calls; assert with `test-org-canvas-api-called-p`, `test-org-canvas-api-call-count`
- `with-sync-test-env`, `with-org-canvas-test-config`, `with-html-to-org-identity`, `with-nonexistent-canvas-files` (binds every `org-canvas-*-file`, since defcustom defaults are evaluated at load)
- Generators: `test-org-canvas-define-common-{parse,transform,push}-tests`, `with-pull-property-test`
- Emacs 29: pre-bind `:type` in `let*` before `expect` (oclosure shadowing); skip with `(signal 'buttercup-pending ...)` under `test-org-canvas-emacs-30-p`
- Mock `plz` directly (`cl-letf`) to test `org-canvas-api-request` internals; `plz-error` is signalled as the struct, not a list

Adding a module: also add it to `eldev-undercover-fileset` in `Eldev` and mock its sync/delete functions in `test/org-canvas-test.el` (module-developer-guide.org, Steps 10–12).

## Key Files

- `lisp/org-canvas-core-sync.el` and `lisp/org-canvas-core-config.el` — pipeline, push infrastructure, registries (read these first)
- `readme.org`, `CONTRIBUTING.md` — overview, setup, PR process
- `documentation/manual.org` — full manual; its Property Reference is generated by `test/docgen/` from the registry and checked by `test/org-canvas-doc-reference-test.el`, so never hand-edit it
- `documentation/architecture/` — design.org, api-interaction.org, pull-system.org, testing.org, module-developer-guide.org, decisions.org, coverage.org (the Canvas feature map), `canvas-openapi3.yaml` (Canvas API spec), `plans/`, `specs/`
- `demo-course/` — working example course (DS 101) covering every content type

## Dependencies

External: `plz`, `transient` (0.4+), `org` (9.6+), `ox-html`; `pandoc` is optional (HTML→Org on pull). Emacs floor: see `Package-Requires` in `lisp/org-canvas.el`. Logging is in-tree (`org-canvas-core-log.el`); `request.el` is not a dependency (its leftover requires were dead code from the plz migration).

## gh CLI Quirk

`gh issue view N` exits non-zero due to a GraphQL Projects-classic deprecation warning even on success — use `gh issue view N --json body --jq .body` instead.
