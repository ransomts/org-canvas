# Pull Pipeline Improvements (improvements.md follow-up)

**Date:** 2026-04-26
**Status:** Design
**Scope:** Cross-cutting; touches `org-canvas-core-api.el`, `org-canvas-core-org.el`, `org-canvas-core-sync.el`, every feature module's pull path, and the property registry.

## Problem

A full course pull on 2026-04-26 (CPSC 4300/6300, course 281704, log: 495 lines, output: `canvas-structure/`) surfaced 20 issues in `improvements.md` plus one latent bug found while reading the log. Categorized:

- **Reliability (silent data loss).** A page-detail timeout shipped a partially-written `pages.org` with `:CANVAS_URL:` instead of `:CANVAS_ID:`. No retry, no end-of-pull error surfaced. Embedded image URLs in announcements/assignments/quizzes are signed Canvas URLs that expire. The course image is stored in `settings.org` as a signed URL committed to git. New-quizzes and several other empty endpoints produce zero-byte files indistinguishable from errors.
- **Schema noise.** ~13 lines of mostly-`false` boolean properties on every assignment/page; per-entry `:LAST_SYNCED:` repeated dozens of times when one file-level header would do; `:INDENT: 0` always emitted; sort order by `CANVAS_ID` instead of `:POSITION:`; module item `:ITEM_TYPE:` lost; UTC times not localized; rubric ratings hacked into a flat table with `>` prefix; quiz/page descriptions have no separator from the body.
- **Missing exports.** Section overrides (the cross-listed-course case) are not pulled at all even though push-side support exists.
- **Cosmetic.** `clemson.instructure.com//api/v1/...` double-slash; `:LICENSE:  private` double-space; "Uploaded Media" duplicate detection; special-char filenames; missing announcement metadata (POSTED_AT, AUTHOR, DELAYED_POST_AT).
- **Latent (log-only).** Every list endpoint hits `?per_page=100&page=1` with no follow-up — silently truncates beyond 100 items.

## Goals

- A pull that hits a transient API error retries and ends with a user-visible summary instead of a "successful" sync that quietly omitted data.
- Embedded file URLs in pulled HTML round-trip to local `[[file:…]]` links, including hidden RCE uploads not exposed in the user-visible Files area.
- The pulled org files are short and scannable: no boolean defaults, no per-entry timestamps, no `:INDENT: 0`, sorted by position, with section overrides expressed as the existing `#+NAME: overrides` table.
- Empty endpoints produce a self-documenting file ("Canvas returned 0 items at <ts>") rather than a blank file.
- All UTC ISO-8601 timestamps are emitted in the course's configured timezone.
- Lists with more than 100 items pull in full.

## Non-Goals

- Backward compatibility with the current schema. Format changes are breaking; users re-pull. (See "Decision: hard cutover" below.)
- Generalized "default-value suppression" beyond booleans declared in the property registry. Strings/enums/numbers stay even when matching Canvas defaults.
- A migration command for existing pulled directories. `rm` and re-pull is the supported path.
- Reorganizing the duplicate-pdfs-in-Uploaded-Media data. Detection and warning only; the user resolves manually in Canvas.
- Fixing push-side issues that improvements.md surfaced about pull output. The push path is read in places (e.g., conflict diffs); where it consumes properties whose pull format is changing, push gets minimal updates to read the new format only — not both.

## Decisions

### 1. Hard schema cutover, no compat reads

The push path reads `:LAST_SYNCED:` for conflict timestamps and `:PEER_REVIEWS: false`-style booleans for payload construction. The cutover changes these:

- **`:LAST_SYNCED:`** moves from per-entry to a single `#+LAST_SYNCED:` file header. Push reads only the file-level header.
- **Boolean defaults** (e.g., `:PEER_REVIEWS:`) are absent on pull. Push treats absent as the registered `:default`.

Existing org files with the old format become invalid until re-pulled. The user (single course in active use) re-pulls once after the changes land.

### 2. Embedded file rewriting via on-demand metadata fetch

The recent commit `b8838c7` rewrites Canvas file URLs in pulled HTML to local `[[file:…]]` links — but only for file IDs already present in `files.org`'s CANVAS_ID-to-relpath map, and only for the URL form `files/NNNN/preview?verifier=…`.

Two gaps:

- **Different URL form** (`files/NNNN?verifier=…&wrap=1`, no `/preview`) — unhandled by the existing regex.
- **Unknown file IDs** — RCE uploads and other hidden-folder files reference IDs that aren't in the per-course Files listing because they live in Canvas's "Uploaded Media" auto-folder. The existing rewriter has no entry for them.

The fix is one mechanism that handles both gaps:

1. Expand the URL-extraction regex to match both forms (with or without `/preview`, with or without `&wrap=1`).
2. When the rewriter finds a file ID with no entry in the local map, GET `/api/v1/files/:id`. From the metadata, derive the canonical Canvas folder path (joining `folder_id` to the folder map already loaded by `org-canvas-pull-files`), download the file to `content/<folder-path>/<display_name>`, and append a heading to `files.org` so subsequent body rewrites and round-trip pushes find it.
3. Cache resolved file IDs for the rest of the session to avoid duplicate metadata calls when the same image appears in multiple bodies.

This keeps the file model uniform — every embedded file is a tracked entry in `files.org`, downloaded under `content/`, linked the same way as user-uploaded attachments.

### 3. Default-suppression: registry-driven, gated by toggle

Property modules already register `:default` values for booleans via `org-canvas-register-properties` (e.g., `:type boolean :default t` or `:default nil`). Pull functions consult the registry: when a property's pulled value equals its registered default, the property is omitted.

This is gated by a new defcustom:

```elisp
(defcustom org-canvas-emit-defaults nil
  "When non-nil, emit Org properties whose values match the registry default.
Default behavior (nil) suppresses these to keep drawers terse.")
```

Set to `t` for verbose pulls when debugging. Push is unchanged: missing properties already mean "use the documented default" in the existing code path; this just stops writing them out on pull.

Scope is intentionally narrow: only `:type boolean` properties whose `:default` is set in the registry. Strings, enums, numbers, and timestamps continue to emit unconditionally — generalized default detection adds complexity for diminishing returns.

### 4. Format changes

**Rubric ratings.** Replace the `>`-prefixed flat table with a child heading per criterion containing a sub-table:

```org
* Critical Thinking
  :PROPERTIES:
  :CANVAS_ID: 12345
  :END:
** Presents the information in sufficient depth                          [5pt]
   | Rating       | Points | Description |
   |--------------+--------+-------------|
   | Full Marks   |    5.0 |             |
   | Partial      |    3.0 |             |
   | No Marks     |    0.0 |             |
** Demonstrates understanding of source material                         [3pt]
   | Rating | Points | Description |
   ...
```

The criterion heading shows points in the trailing tag-style bracket so it's visible without expanding the table. Push parses the sub-table per criterion heading (not the previous flat layout).

**Description boundary.** When a heading has a body (HTML description from Canvas) AND structured children (e.g., quiz questions, rubric criteria, override tables), wrap the description in a `** Description` subheading. When there are no structured children, the body is emitted directly under the parent heading as today (no spurious `** Description` wrapper).

This applies to: quizzes (when questions are emitted), rubrics (always — criteria follow), and any future content type with a body + nested structure.

**Module item `:ITEM_TYPE:`.** Add `:ITEM_TYPE:` as a property on every module item heading. Values: `Page`, `Assignment`, `Quiz`, `Discussion`, `File`, `ExternalUrl`, `ExternalTool`, `SubHeader`. SubHeader rows currently look identical to plain headings; this property disambiguates.

**`:INDENT:`.** Emit only when nonzero.

**Sort order.** Lists sort by `position` when the API returns it, otherwise by `name`, with `id` as last resort. Assignments sort by `assignment_group_id` then `position` so the org file groups them as the user expects (intra-group order preserved).

**Section overrides (pull symmetry).** The existing `#+NAME: overrides` table format used by push is the target. Pull queries `/api/v1/courses/:id/assignments/:id/overrides` for each assignment, and emits a table under the assignment heading when the response is non-empty:

```org
* Lab 1
  :PROPERTIES:
  :CANVAS_ID: 678
  :DUE_AT: <2026-02-15 Sun 23:59>
  :END:
  #+NAME: overrides
  | Section                                            | DUE_AT                  | UNLOCK_AT |
  |----------------------------------------------------+-------------------------+-----------|
  | [[file:sections.org::*S2601-CPSC-6300-001-15179][6300]] | <2026-02-22 Sun 23:59>  |           |
```

Empty override sets emit no table. Section column resolves the section's `course_section_id` to the file link in `sections.org` (using the heading-by-CANVAS_ID lookup the validator already uses).

**Course image.** Download the asset behind `:COURSE_IMAGE:` into `content/course_image/<basename-from-url>` and store the local relative path. The signed URL never lands in the org file.

### 5. Timezone localization

Settings pull already captures `:TIME_ZONE:` (e.g., `America/New_York`). Every other pull module that emits a timestamp:

1. Reads the timezone from `settings.org` `:TIME_ZONE:` at the start of the pull (cached for the session).
2. Falls back to the system TZ if `:TIME_ZONE:` is missing or settings.org hasn't been pulled this session.
3. Converts the Canvas UTC ISO-8601 string to that TZ before emitting an Org active timestamp `<2026-02-15 Sun 23:59>`.

Push converts back to UTC on send (already the contract).

### 6. Reliability primitives

**Pagination.** `org-canvas--api-paginated` (new helper) walks the RFC 5988 `Link: <…>; rel="next"` header until exhausted, falling back to `?page=N` until an empty array on responses lacking the header. Replaces the hard-coded `?per_page=100&page=1` pattern in every list call.

**Retry with backoff.** `org-canvas--api-with-retry` wraps API calls with three attempts (5s, 10s, 20s sleeps) when the response is a transient error: timeout (curl errors 28/56/7), 502/503/504, or 429. 4xx other than 429 errors fail immediately. The new pages-detail call is the highest-value caller.

**End-of-pull summary.** A new `org-canvas--pull-summary` accumulator collects non-fatal errors during the pull (each pull function calls `(org-canvas--pull-summary-record :file FILE :error MSG :log-line N)` on caught errors). At the end of `org-canvas-pull-all`, if the accumulator is non-empty, the package prints a block to `*Messages*`:

```
Pull complete with 1 non-fatal error:
  pages.org: timeout fetching connecting-to-the-palmetto-jupyter-image (log line 154)
  Re-run `org-canvas-pull-pages` to retry.
```

### 7. Cosmetic fixes

- **Double-slash URLs.** `org-canvas-base-url`'s join helper strips trailing slashes from the base before concatenating the API path.
- **`:LICENSE:  private` double-space.** Settings emitter uses `format "%s: %s"`, single space.
- **Empty file headers.** When a pull function completes successfully but the API returned zero items, write:
  ```
  #+TITLE: <Module label>
  #+LAST_SYNCED: [2026-04-26 Sun 07:32]
  # Canvas returned 0 items at this sync.
  ```
  When the pull call itself failed, the existing org file (if any) is left untouched and the failure is recorded in the pull summary instead.
- **Files duplicate detection.** During `org-canvas-pull-files`, track `(size, content_type, display_name-stripped)` tuples; emit one warning per detected dup group via `org-canvas--log-warning`.
- **Headline sanitization.** When a Canvas filename contains `:` (becomes literal `_` in display), an opening parenthesis followed by `(elongated)`-style suffixes, or other punctuation that breaks Org parsing — escape only the cases that actually break Org (currently: nothing strict, but `[`/`]` inside link descriptions need backslash-escape).
- **Announcement metadata.** Add `:POSTED_AT:`, `:AUTHOR:` (user.display_name), `:DELAYED_POST_AT:` properties.

### 8. Quiz-question emission bug (#5)

The log shows all 15 quiz `/questions` endpoints were called and returned success, but only one quiz emitted Question subheadings. This is a bug in the emit path, not network. Investigation deferred to implementation; no design choice. Likely candidates: a guard checking response shape that fails for the common case, or a side-effect on the first emit that prevents subsequent ones.

## Architecture

### Module impact

| Module | Pull change | Push change |
|--------|-------------|-------------|
| core-api | Pagination, retry, base-URL fix | none |
| core-org | URL rewriter regex expansion, on-demand file fetch + register, summary accumulator, file-level `#+LAST_SYNCED:` reader | reads `#+LAST_SYNCED:` from header instead of `:LAST_SYNCED:` per entry |
| core-sync | Default-suppression helper, sort-by-position helper | reads absent properties as registry default (already does) |
| settings | Course-image download, TZ caching, double-space fix | reads local `:COURSE_IMAGE:` path |
| pages | retry on detail call | none |
| announcements | metadata properties, embedded URL rewrite | none |
| assignments | overrides pull, default suppression | overrides push already exists |
| quizzes | question emission fix, `** Description`, embedded URL rewrite | none |
| rubrics | child-heading-per-criterion format | reads new format |
| modules | `:ITEM_TYPE:` property, suppress `:INDENT: 0` | reads `:ITEM_TYPE:` for type dispatch |
| files | duplicate detection warning, headline sanitization | none |
| outcomes / calendar / discussions / new-quizzes / group-categories | empty-file self-doc header | none |
| validate | offline checker recognizes new schema (no LAST_SYNCED per entry, optional booleans) | n/a |

### Sequencing

Direct-to-main, one commit per logical unit. Five waves, each ending with `eldev lint && eldev complexity && eldev test` green:

1. **Reliability primitives** — pagination, retry, summary accumulator, double-slash fix.
2. **Embedded files** — URL regex expansion, on-demand metadata fetch, files.org session register.
3. **Schema cutover** — file-level `#+LAST_SYNCED:`, default suppression with toggle, rubric format, `** Description`, `:ITEM_TYPE:`, `:INDENT:` suppression, sort-by-position.
4. **Content fidelity** — course image download, TZ localization, override pull, announcement metadata, quiz-question bug investigation.
5. **Polish** — empty-file headers, duplicate detection, `:LICENSE:` double-space, headline cosmetic fixes.

## Testing

Each commit includes tests. Patterns the repo already uses:

- `with-mock-api` for response shaping (404, 5xx, paginated Link headers).
- `with-temp-org-buffer` for emitter tests; round-trip through parse where the format changes.
- Integration-level tests for whole-module pulls under `with-nonexistent-canvas-files` so the emitter writes through to disk.
- Coverage gate in the pre-push hook is 99%; new uncovered lines must be either covered or explicitly justified in the test file.

The single biggest test risk is the embedded-file fetch path: it requires mocking sequential API calls (body rewrite triggers metadata GET triggers download). The existing `test-org-canvas-api-called-p` infrastructure handles per-call assertions; the new helper just needs a multi-call recorder.

## Open Issues / Investigation

- **Quiz-question emission (#5)** — root cause unknown; trace during Wave 4. May yield a small commit in Wave 1 if it's a logging/error-swallowing problem upstream.
- **Files API edge case for embedded fetch.** Some Canvas instances may not expose `/api/v1/files/:id` for files the user can preview but not list. If a metadata GET 401s/403s, the rewriter falls back to downloading via the embedded URL directly (its verifier is valid at fetch time) and synthesizes a minimal files.org entry with `display_name = <basename of Content-Disposition or last URL segment>` and `folder_path = "Uploaded Media"`.
