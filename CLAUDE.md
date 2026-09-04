# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

org-canvas is an Emacs Lisp package that synchronizes course content from Org Mode files to Canvas LMS via its REST API. The workflow treats Org files as the "source of truth" - instructors design courses in Org Mode and push changes to Canvas.

### Supported Content Types (16)

Assignments (including LTI/external-tool assignments such as Gradescope), quizzes (classic), new quizzes, pages, modules, rubrics, outcomes, discussions, announcements, files, assignment groups, group categories, calendar events, sections, per-section date overrides, and course settings.

## Build Commands

This project uses Eldev (Elisp Development Tool):

```bash
eldev test           # Run tests
eldev compile        # Compile Elisp files
eldev lint           # Run linter
eldev package        # Create distributable package
```

**Always run `eldev lint` before committing or pushing.** Fix any warnings before proceeding. CI lints on Emacs 30.1, whose checkdoc rejects a third-person verb such as "holds" anywhere in a docstring's first line; a newer local Emacs may accept it, so keep first lines imperative throughout (PRs #89/#90 went red on exactly this).

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
├── org-canvas-diff.el         # Read-only drift report (org-canvas-diff)
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

**`:remote-fn` — how the drift report reads a property back off Canvas.** `org-canvas--diff-remote-field` otherwise assumes the payload is a flat alist keyed by `:api-key`, falling back to `:data-key`. That assumption is wrong wherever Canvas nests or renames a field, and the failure is silent: `alist-get` returns nil, the formatter prints `false`/`(unset)`, and *every* item with that property is reported as drifted. Two specs declare a `:remote-fn` (a symbol naming a one-argument function of the Canvas item) for exactly that reason — files' `PUBLISHED` reads the inverse of `locked`, which is the field Canvas actually returns (issue #61, matching the pull mapping from #50), and assignment-groups' `DROP_LOWEST`/`DROP_HIGHEST` read the nested `rules` object the push side already builds (issue #62). When adding a property whose Canvas spelling is not a flat key of the same name, give it a `:remote-fn`; the give-away is a whole feature reporting the same field as changed on an untouched course.

**`:compare-p` — when a property is no one's opinion to compare.** A spec may name a predicate of `(POM ITEM)`; nil means the drift report skips the property for that entry. Calendar `ALL_DAY` declares `org-canvas--calendar-all-day-comparable-p`: Canvas never stores the flag on an event spanning days — it keeps the times, sets `all_day` false, fills `all_day_date` — so a span's `ALL_DAY: true` could never round-trip and was a permanent diff row (issue #93). Spans are judged on the Org timestamps' *written* dates (`org-canvas--org-timestamps-span-days-p`, core-org): comparing converted UTC dates would misclassify a single local day that crosses midnight in UTC. `org-canvas--validate-all-day-span` (the calendar registry's `:structural-fn`) warns about the same state offline.

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

Files sync in three tiers rather than always re-uploading: `PAYLOAD_HASH` stores `BYTES:METADATA` (`org-canvas--file-hash-parts` splits it), so a whole-hash match skips, a bytes match goes through `org-canvas--file-update-metadata` (one `PUT /api/v1/files/:id` — name, parent_folder_id, locked/hidden/dates — keeping the file id), and only a content change re-uploads. A legacy single-md5 hash is no longer taken as evidence of a content change: `org-canvas--file-migrate-legacy-hash` fetches the Canvas copy once and adopts a split hash when the bytes agree (tolerating the leading CRLF that issue #70 put on every pre-fix upload), so the id survives. Only a genuine difference re-uploads, and `org-canvas--file-announce-legacy-hashes` says up front how many entries face that check — issue #71: a re-upload rotates the file id, and deleting the old object first made Canvas *silently drop the module items pointing at it*.

**A replacement upload no longer deletes first** (issue #77). `org-canvas--file-clear-way-for-upload` deletes only when `org-canvas--file-replace-in-place-p` says the file moved folders; otherwise the upload's `on_duplicate=overwrite` (already in the preflight payload) replaces it where it stands. Measured on a live course: the id still rotates, but Canvas repoints the module items itself and keeps the old id resolving as an alias, so the collateral is gone. The delete survives for a folder move because overwrite matches on name *within a folder* — with nothing to overwrite at the destination the old object would linger as an orphan still holding the items. That case records into `org-canvas--file-recreated-ids` and warns; ordinary id changes now log at info via `org-canvas--file-warn-changed-ids`.

**`org-canvas--file-force-upload`** overrides every skip in `org-canvas--file-sync-parsed-entry`, which all reason from a hash — and a hash describes the local file and the last upload, never what Canvas actually holds. Built for the files uploaded before issue #70 was fixed, whose remote copies carry a stray CRLF that the issue #71 migration deliberately tolerates. Bound for one run by `org-canvas-files-force-reupload` / `org-canvas-force-reupload-file-at-point`, both of which confirm first; never set it globally, or every sync re-uploads everything, which is what #71 was about. Before either write path, `org-canvas--file-check-conflict` runs the shared conflict resolution with `org-canvas--file-pull-item` as the pull option (issue #49).

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

### External-Tool (LTI) Assignments — and No Turnitin

An assignment whose submissions an LTI tool takes (Gradescope is the case this was built for) carries `EXTERNAL_TOOL_URL` and/or `EXTERNAL_TOOL_ID` beside `SUBMISSION: external_tool`. `org-canvas--assignment-add-external-tool` nests them into `external_tool_tag_attributes` — a URL, or `content_id` plus the `content_type` Canvas requires — and always sends `new_tab`, since an explicit false is meaningful. Because the field is nested, all three properties declare a `:remote-fn` (`org-canvas--assignment-remote-tool-*`); read flat, `alist-get` returns nil and *every* external-tool assignment reports drift forever. `org-canvas--validate-external-tool` warns both ways: `external_tool` naming no tool (Canvas creates an assignment that launches nothing and fails only when a student opens it) and a tool named without `external_tool` (silently ignored). Launch URLs are not guessable, so `org-canvas-list-external-tools` lists them — it passes `include_parents=true`, without which Canvas returns only course-installed tools and institutional courses come back empty.

The boundary: org-canvas owns the Canvas side only. Gradescope has no supported public API, so the tool-side assignment and its grade push stay in Gradescope's hands; grades arrive in Canvas over LTI and the submissions module reads them from there.

**Turnitin support was removed, deliberately — do not add it back without re-probing.** `TURNITIN_ENABLED` drove the legacy account-level plugin. Probed live on 2026-08-25 against clemson.instructure.com: zero of 62 assignments carried a `turnitin_enabled` key, so the plugin is off; Turnitin is installed as an LTI 1.3 tool (`assignment_selection: no`), the plagiarism-review association, which the Assignments REST API does not expose. The property was inert and `turnitin_settings` would have been dead code. The fields remain in `test/contract/canvas-contract.json` because that fixture describes Canvas's API, not ours.

### Quiz Publish Sequencing

Classic quizzes keep `published` **out** of the quiz payload entirely; `org-canvas--quiz-settle-publish-state` applies it in finalize *after* `org-canvas--quiz-sync-children` writes the questions. Canvas computes `points_possible`/`question_count` (and the backing assignment's points) on a publish transition, not on question insert, so publishing on the create POST froze an empty quiz's totals at zero (issue #59). Omitting the key also leaves an existing quiz's state untouched on a PUT. For a quiz that was *already* published, `org-canvas--quiz-refresh-totals` GETs it and republishes only when `org-canvas--quiz-totals-stale-p` sees the remote count *behind* what was written — a higher remote count means orphan questions, not staleness — and only when `unpublishable` is t; with submissions it warns instead.

### Rubrics Update In Place (issue #123)

A rubric push used to *delete* whatever carried the title and POST a fresh one. Associations belong to the old rubric id, so every assignment silently lost its rubric — and the rubric assessments students were graded with would have gone with it; on the course this was found on, only the essay's lack of submissions kept it from being data loss. `org-canvas--rubric-push-to-api` is now the generic `org-canvas--push-to-api` on `rubrics`: a CANVAS_ID is PUT to `rubrics/ID` (Canvas updates the criteria in place, keeping the id, its associations and its assessments), a heading without one creates, a title Canvas already holds goes through the duplicate-title guard (#85) instead of a DELETE, and a stale id 404s into a POST. `org-canvas--rubric-finalize` unwraps the `rubric` key and delegates to `org-canvas--finalize-item`, so rubrics finally stamp like everything else; `org-canvas--rubric-warn-recreated` says so loudly if the id comes back different, the one case where linked assignments still need a re-push. **The Rubric API object carries no `updated_at`** (checked against `documentation/architecture/canvas-openapi3.yaml`), so the conflict check can never fire for a rubric — it costs one GET and is left in for uniformity, not for detection.

### Bulk Publish

`org-canvas-publish-module` / `-unpublish-module` set `PUBLISHED` on the module *and on the heading that owns each linked object* — resolved by `org-canvas--module-item-target`, which returns `(FILE . POSITION)` rather than a Canvas id. Publish state belongs to the object (issue #47), so it is written where the object is declared, never inferred from a module item at push time; SubHeaders and external URLs have no object and are set in place. `PUBLISH_AT` on a module is org-canvas's own release schedule (never sent to Canvas): `org-canvas-apply-scheduled-releases` runs at the start of `org-canvas-sync`, before tier -1, so objects it publishes are pushed by the same run. A module *item* may carry its own `PUBLISH_AT`, which outranks its module's: `org-canvas--module-item-held-p` holds such an item back from any publish — scheduled or by hand — until its date arrives, and `org-canvas--module-release-items` releases it then. Holding applies only to publishing; unpublishing a module takes scheduled items down with it. An item released inside an unpublished module is warned about, never fixed by publishing the module (issue #47 again). Mechanics live in modules.el, commands in org-canvas.el (they offer to sync, which modules.el must not require).

### Module Items Reconcile Cross-Module Moves (issue #105)

Canvas has no cross-module move (the module id is in the item URL), so a block moved under another module heading in modules.org used to work only by accident: a PUT against the wrong module 404s and is retried as a POST, and the old copy stayed behind either way. `org-canvas--module-sync-items` now lists the module's items once (`org-canvas--module-remote-items`, which returns `unknown` on a failed request *or a malformed list* — deleting on a misread would be the worst outcome, and generic test mocks return junk for every GET) and: `org-canvas--module-item-disown-foreign-id` clears the id of a heading the module does not hold so it is created here, remembering the old id in `org-canvas--module-items-moved` (cleared by the `:after-sync`); `org-canvas--module-reconcile-departed` deletes an unclaimed remote item only when it has demonstrably moved — another module's heading claims it (`org-canvas--module-item-claimed-elsewhere`) or it is in the moved list — and warns about any other unlisted item rather than pruning it (children are never pruned unasked; quiz questions follow the same rule). Items get their own `org-canvas--module-item-finalize`: they used to go through the module finalize, whose post-fn re-ran the child sync with the *item's* id as a module id — a no-op then, a 404 per item once the child sync fetches. `org-canvas--validate-module-item-ids` (the module-items `:file-fn`) warns when two item headings claim one id.

### Single-Item Pull

`org-canvas-pull-at-point` refreshes the heading at point from Canvas by handing one fetched item to `org-canvas--conflict-pull-local` — the same function the conflict prompt's pull option uses, which stamps `CANVAS_UPDATED_AT` and drops the stale `PAYLOAD_HASH` (get those wrong by hand and the entry re-triggers drift forever while looking clean). Dispatch is generic: the feature registry carries `:pull-item-fn`, written by `org-canvas-register-pull-item-fn` from whichever of `org-canvas-define-sync` or `org-canvas-define-pull` the module uses, and `org-canvas--registry-feature-for-file` maps the current buffer to its feature. **A module with a custom sync *and* a custom pull registers its own** (files.el does; quizzes and outcomes have no pull-item fn at all and the command says which whole-file command to use instead). Generated `org-canvas-pull-<feature>` commands also take a prefix arg that restricts the pull to ids the file already claims (issue #67).

### A `:skip-fn` Must Say So

A skipped item used to leave no trace: no log line, no counter, nothing in the
`.org` file. The number a pull prints is the number *written*, which reads as the
number *available* — so a local copy missing content looked exactly like a clean
one (issue #81). Every path that consults a `:skip-fn` now accounts for it.
`org-canvas--pull-handle-item` (the runtime dispatcher the generated pull loop
calls — it exists so the macro stays a two-branch `pcase`) returns `processed` /
`skipped` / nil; a skip is logged by name, counted into the completion line via
`org-canvas--pull-skip-suffix`, and recorded through `org-canvas--pull-record-skip`
as a `:kind 'skip` entry in `org-canvas--pull-summary`, which
`org-canvas--pull-summary-print` renders as its own section. `org-canvas-diff`
counts what it never compared (`org-canvas--diff-unclaimed`) and names it in the
footer, and the orphan scan and prune tally count protected items. Declare
`:skip-reason` — a short phrase — wherever a `:skip-fn` is declared, in both
`org-canvas-define-pull` and `org-canvas-register-feature`; without one the
reports fall back to a generic "excluded by this module".

**Only push and delete-all skip the front page.** Pull deliberately does not
(issue #82). The guard is about destruction — one page per course, and Canvas
refuses to unpublish it — while pull writes local files only, so excluding it
there left the one page students land on as the one page missing from the source
of truth: absent from a migration, and invisible to `org-canvas-diff`, which has
nothing local to compare. `org-canvas--page-pull-item` records `FRONT_PAGE: true`
so the round-trip survives, and *removes* the property when Canvas no longer
serves that page as the home page — two headings claiming it is not a state
Canvas can be in.

### Duplicate-Title Guard (issue #85)

`org-canvas--push-to-api` chooses POST purely on the absence of an id, and every recovery from a partial create (POST succeeded, stamp never written) left the course one sync away from a second copy students could submit to. Before any POST, `org-canvas--push-guard-duplicate` looks the title up and asks adopt/skip/create — `org-canvas--resolve-duplicate` mirrors the conflict machinery: `org-canvas--duplicate-apply-all` (capitals, per run), `org-canvas-duplicate-title-strategy` (the caller's seam; `create` also disables the lookup), skip under `noninteractive`. Adopt stamps the id *and* `CANVAS_UPDATED_AT` from the item before the PUT so the conflict check that follows compares against the item's own clock. The lookup source is `org-canvas--current-remote-titles`: the drift snapshot's title index (now built by `org-canvas--sync-fetch-remote-snapshot` from the same one list request) inside a sync, the symbol `none` when a sync had no snapshot (an unregistered feature such as new quizzes, a failed fetch, detection off — check nothing rather than spend a GET per entry), and nil outside a sync, where a single-entry push asks the module's `:find-fn`. The pipeline counts a `duplicate` return as a skip with a named `:skipped-titles` entry; `org-canvas--push-at-point-runtime` reports `conflict`/`pulled`/`duplicate` instead of finalizing a symbol. `org-canvas-diff` reports the same situation as `UNCLAIMED` via `org-canvas--diff-pair-unclaimed`.

**`org-canvas-adopt-at-point` is the same act after the fact (issue #101).** The guard only fires when a create is attempted; a stamp that died after the POST (#99's `[Stamp]` line) leaves no local record of the id, so adoption by title is the recovery. The command lists the feature's items through the registry URL helpers, matches the heading title (via `org-link-display-format`, since a files.org heading is a link), and on exactly one match calls `org-canvas--adopt-stamp` (core-sync) — the one spelling of adoption, which `org-canvas--push-adopt-item` also uses: id property, `CANVAS_UPDATED_AT` from the feature's `:modified-field`, and `PAYLOAD_HASH` dropped so the next sync verifies content. Matches a `:skip-fn` disowns or another heading already claims are held back and named; zero or several refuse. `org-canvas-request-min-interval` (core-api `org-canvas--api-pace`, called by every `org-canvas-api-request`) replaces the `sleep-for` batch scripts wrapped around at-point calls.

### Dry Run Reports Conflicts (issue #84)

The dry-run branch of `org-canvas--sync-execute-pipeline` returns before the push, where the conflict check lives, so it used to count every pending entry as a push. `org-canvas--sync-dry-run-entry` answers from the snapshot in ctx (`:remote-updated` via `org-canvas--sync-remote-newer-at`, `:remote-titles` via `org-canvas--sync-remote-items-titled`) and counts a would-stop entry as `:dry-run-conflict`, which is in `org-canvas--sync-stat-keys` (a key left out is silently dropped — issue #66). Dry-run mode detection is `org-canvas--sync-stats-dry-run-p`, which also consults `org-canvas--dry-run` itself: a run where every pending entry would conflict has a `:dry-run` total of zero.

### The Drift Report Compares Bodies (issue #83)

The description is set directly on the payload, outside the property specs, so `org-canvas-diff` could not see it. Modules that push a body declare `:body-api-key` on `org-canvas-register-properties` (assignments/quizzes `description`, pages `body`, announcements/discussions `message`); pages add `:body-list-params '(("include[]" . "body"))` because the pages index omits bodies unless asked, and quizzes a `:body-fn` because their description is only the text before the first question. Local bodies come from `(org-canvas--export-subtree-body-to-html t)` — the OFFLINE flag skips link *and image* resolution, and image resolution uploads; a report must never write. Both sides go through `org-canvas--diff-html-to-text` (tags dropped, entities decoded, whitespace collapsed) so markup-only differences stay invisible. Its whitespace class is explicit, not `[[:space:]]`: that class goes by the current buffer's syntax table, and in a Lisp buffer newline is comment-end, so the same string normalized differently depending on which buffer was current. A remote item lacking the field, or a failed export, means "not compared", never drift. A CHANGED row with no field differences now carries a line saying the change is in something uncompared.

### Conflict Log Line Names Its Baseline (issue #86)

`org-canvas--conflict-baseline-source` returns `(TIME . LABEL)`; `org-canvas--conflict-baseline` is its car. The conflict warning and the `*canvas-conflict*` buffer print the LABEL (`CANVAS_UPDATED_AT ...` or `#+LAST_SYNCED [...] (entry has no CANVAS_UPDATED_AT)`) rather than re-reading the file header, which was wrong whenever the entry's own stamp was compared and nil whenever POM was a bare position.

### By-Design Extras (issue #98)

A synced course never read zero: Canvas scaffolding and deliberate web-UI objects sat as permanent EXTRA rows, so `org-canvas-diff-batch` could not do cron monitoring. Three valves, broad to narrow: structural `:skip-fn`s on the feature registry suppress scaffolding — the root outcome group (no `parent_outcome_group`), the stock "Assignments" group (skip-fns only ever hold back *unclaimed* items, so a managed group of that name is still compared), and classic-quiz shadow assignments (`quiz_id` present) — counted in the footer the #81 way, and protecting the same objects from orphan cleanup; `org-canvas-diff-excluded-features` skips a feature wholesale with a visible "not checked" line; `org-canvas-diff-known-extras` (`(FEATURE ID &optional NOTE)`) acknowledges individual ids, counted once in the footer, with a `STALE-ACK` divergence — which counts, deliberately — when an acknowledged id stops existing so the list cannot rot. Both defcustoms affect the diff only; prune still lists acknowledged items. A fourth valve is automatic (issue #102): announcement and page attachments land in Uploaded Media as unclaimed files, and the known-extras list grew by hand forever. `org-canvas--diff-feature` records `:referenced-files` — the `/files/<id>` ids the body field of every item embeds — for each feature with a `:body-api-key` (the bodies are already fetched for the #83 comparison), `org-canvas--diff-syllabus-references` adds the syllabus (the one extra GET), and `org-canvas--diff-apply-references` moves matching Files extras into a `:referenced` count rendered as a "Referenced media" footer line. Runs after every feature is diffed, since registry order puts Files before the body features. `org-canvas-diff-scan-references` (default t) turns it off.

### The Drift Report Has Verbs (issue #103)

`org-canvas--diff-render` still returns a string, and batch output is unchanged, but each row is inserted by `org-canvas--diff-insert-row`, which stamps the text with an `org-canvas-diff-row` property `(:feature NAME :entry ENTRY)`; text properties survive `buffer-string` and `insert`, and `princ` drops them. The interactive buffer is in `org-canvas-diff-mode` (derived from `special-mode`): `RET` `org-canvas-diff-visit` (heading by id property for MISSING/CHANGED, by title for UNCLAIMED, `browse-url` of the `:html-url` that extras now carry), `a` `org-canvas-diff-acknowledge` (edits `org-canvas-diff-known-extras` in memory, then hands the *whole new list* to `org-canvas-diff-acknowledge-function`, default `customize-save-variable`; on a STALE-ACK row it drops the entry), `k` `org-canvas-diff-delete` (confirms, then the feature's item URL and `:delete-data`, as orphan cleanup does), `p` `org-canvas-diff-pull` (runs `org-canvas-pull-at-point` in the heading's buffer), `g` re-runs, `TAB`/`S-TAB` walk rows by stepping over property runs (a CHANGED row spans its field lines). An action rewrites its row in place via `org-canvas--diff-rewrite-row`, keeping the property, rather than re-fetching everything. No magit-section dependency: the rows are plain text properties.

### Feature Registry URL Resolution (issue #87)

Every consumer of `org-canvas--feature-registry` — the sync snapshot, `org-canvas-diff`, the orphan scan and delete, `org-canvas-pull-at-point` — resolves URLs through `org-canvas--feature-list-url` / `org-canvas--feature-item-url` / `org-canvas--feature-list-params` (core-api), never by spelling `org-canvas-api-course-endpoint` itself. A feature that does not list under the course registers `:list-url-fn`, `:item-url-fn`, a function-valued `:list-params` (called per use — the calendar context code embeds `org-canvas-course-id`, unknown at load) and `:delete-data`. Calendar events were the first: unregistered, they had no drift check, no duplicate guard, no diff, and `pull-at-point` refused calendar.org because `org-canvas-register-pull-item-fn` is a no-op for an unregistered feature — so `org-canvas-register-feature` must precede the module's `org-canvas-define-sync`. **`GET /api/v1/calendar_events` returns only today's events unless `all_events=true`** (probed 2026-09-01: 0 vs 6 on course 297530); `org-canvas--calendar-event-list-params` is the one spelling of the parameters, shared by sync, search, pull, delete-all and the registry. New quizzes stay unregistered: they key on `CANVAS_ASSIGNMENT_ID`, no course with New Quizzes was available to confirm the list's id field, and a wrong `:id-field` would make the orphan prune see every real quiz as an orphan.

`:modified-field` names the remote timestamp drift is decided from (default `updated_at`), resolved by `org-canvas--feature-modified-field`. Files declare `modified_at`: Canvas bumps a file's `updated_at` on metadata-only touches (a lock, usage rights, a module-item relink), which re-flagged unchanged files forever and, under `org-canvas-conflict-strategy` `push`, re-uploaded identical bytes and rotated ids (issue #94). The field feeds the snapshot index, `org-canvas--conflict-check` (files pass it through `org-canvas--file-check-conflict`) and `org-canvas--diff-modified-p`, and `org-canvas--finalize-item` stamps `CANVAS_UPDATED_AT` from the same field via `:updated-field` so baseline and comparison agree.

### Global Sync Summary

`org-canvas-sync` ends with a per-type table plus named failed/skipped items, rendered by `org-canvas--sync-log-global-summary`. Feature syncs record stats via `org-canvas--sync-record-feature-stats` — automatic in the macro pipeline (`org-canvas--sync-log-summary`), explicit in the custom syncs (settings, files, overrides). Recording is a no-op outside a global sync (gated on `org-canvas--sync-global-counters`).

**Its columns follow the run** (`org-canvas--sync-summary-columns`): a dry run swaps Success for **Would sync** under a `DRY RUN` header, and Conflicts/Pulled columns appear only when the run had some. Every counter a run populates must be listed in `org-canvas--sync-stat-keys` — both the aggregate and the per-feature entry are built from it, and a key left out is silently dropped on the way to the table. That is issue #66: `:dry-run`, `:conflict` and `:pulled` were populated but never carried, so a dry run with 31 pending assignment updates printed `0 success` and a sync that hit five conflicts printed `0 failed` and nothing else. A custom sync must record its dry-run outcome as `:dry-run`, not `:success` (settings did the latter), or the mode detection never sees it.

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
- Baseline is `org-canvas--conflict-baseline`: the entry's own `CANVAS_UPDATED_AT` (the remote `updated_at` finalize recorded on the last push — Canvas's clock, so no skew allowance, and present on push-only courses), falling back to the file-level `#+LAST_SYNCED` header. Pushes now write that header too, stamped from the newest remote `updated_at` of the run and rounded up a minute (issue #48). The write is forward-only (`org-canvas--sync-advance-file-header`) and every single-entry push advances it from the stamp finalize just wrote (`org-canvas--sync-advance-header-from-entry`, issue #104: a file maintained at point read 13 days stale). Since that advance never compared the file's other entries, the verified-skip path backfills `CANVAS_UPDATED_AT` on a legacy entry from the snapshot (`org-canvas--sync-backfill-baseline`) so the header stops being anyone's baseline after one full sync
- The payload-hash skip is drift-aware: `org-canvas--sync-fetch-remote-updated` takes one list snapshot per feature and `org-canvas--sync-remote-drifted-p` pulls any remotely-modified entry off the skip path so it goes through the normal conflict comparison. A matching hash only proves the *local* file is unchanged — without this, an item edited only in the web UI was skipped forever. When the snapshot is unavailable, `org-canvas--sync-warn-unverified-skips` says how many entries went unchecked
- `org-canvas--conflict-check` returns `(cons 'conflict REMOTE-RESPONSE)` (not bare `'conflict`)
- `org-canvas--resolve-conflict` shows a diff buffer and prompts: push/pull/skip (capitals = apply to all)
- `org-canvas--conflict-apply-all` defvar: batch decision bound per-sync by `org-canvas-define-sync`
- `org-canvas-conflict-strategy` defcustom (nil/`push`/`pull`/`skip`) is the *caller's* seam and is never rebound by the pipeline — `--conflict-apply-all` looks like it would serve but every entry point rebinds it to nil, so a caller's `let` loses (issue #72). `org-canvas--conflict-unattended-action` consults it before prompting, and falls back to `skip` under `noninteractive`: `read-char-choice` in a batch Emacs signals end-of-file rather than taking a default, which killed the whole sync. Tests that exercise the interactive prompt must bind `noninteractive` to nil
- `org-canvas--current-pull-item-fn` defvar: dynamically bound per-sync so `push-to-api` can access it
- `org-canvas--conflict-pull-local` overwrites local heading via the module's pull-item function
- Push-to-api returns `'pulled` (not `'conflict`) when user chooses pull — tracked by `:pulled` counter in sync pipeline
- A `:post-fn` that writes to Canvas again must say so with `org-canvas--finalize-note-remote-write`. `org-canvas--finalize-item` stamps `CANVAS_UPDATED_AT` from the push response and *then* runs the post-fn, so the rubric association — a further POST that Canvas counts as touching the assignment — left the baseline seconds behind: `org-canvas-diff` called every rubric-bearing assignment CHANGED with no compared property differing, and the next push walked into the conflict check, both over the sync's own writes (issue #124). `org-canvas--finalize-run-post-fn` binds the flag around the call and `org-canvas--finalize-restamp-updated` re-reads the item once and stamps what Canvas holds, honouring `:updated-field` so files still compare `modified_at`. The endpoint reaches the generated finalize from `org-canvas-define-sync`'s `:endpoint` and only alongside a `:post-fn`; a module with a custom finalize must pass `:endpoint` itself. `org-canvas--associate-rubric` returns t/nil so assignments and discussions declare only a write that landed
- Modules with `:pull-item-fn` in their `org-canvas-define-sync`: announcements, pages, discussions, assignments, assignment-groups, rubrics, group-categories, calendar-events, modules (whose pull-item also replaces child item headings from the remote list, fetching them when the conflict-check GET response lacks an `items` key)

### Org Interaction
- Always `org-back-to-heading t` before property access
- Use markers for safe position tracking
- Save buffer after modifications
- `org-canvas--path` returns truenames — one inode, one buffer. A symlinked `org-canvas-directory` spelling used to give a batch caller a dual buffer: every PUT landed, only the stamps died on the unanswerable changed-on-disk prompt, and the caller's final save clobbered stamps the sync had written (issue #97). `org-canvas--registry-feature-for-file` compares truenames for the same reason
- `org-canvas--ensure-buffer-fresh` guards `org-canvas-org-set-property` and `org-canvas--save-buffer` in batch: an unmodified stale buffer is reverted (`[Fresh]` log), a modified stale one signals a clear error instead of the prompt. A finalize failure after a successful API call logs `[Stamp]` naming the phantom-drift consequence — the [FAILED] line alone reads as a push that never happened

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

**~3400 tests total** (3397 as of 2026-09-01) covering core utilities, all feature modules, and validation (9 tests skip on Emacs 29.x due to org-mode differences). Line coverage is ~99.5% — the pre-push hook blocks below 99%. Run `eldev test` for the exact current count and `eldev test -u "on,codecov,dontsend" -U coverage/coverage.json` for per-file coverage.

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
