# Pull Files: Preserve Canvas Folder Hierarchy

**Date:** 2026-04-25
**Status:** Design
**Scope:** `org-canvas-pull-files` in `lisp/org-canvas-files.el`

## Problem

`org-canvas-pull-files` flattens the Canvas folder hierarchy. Every file becomes a top-level heading in `files.org`, and every download lands in a single `content/` directory regardless of its Canvas folder. This is asymmetric with push: push reconstructs Canvas folders from heading nesting (ancestor headings without file links become folder path components), so a round trip — pull then push — collapses the user's Canvas Files area into the root folder. It also creates filename-collision risk on disk when Canvas allows two files with the same `display_name` in different folders.

## Goals

- Fresh pull produces a `files.org` whose heading nesting and on-disk paths mirror the Canvas folder structure.
- Push of a freshly pulled `files.org` recreates the exact Canvas folder hierarchy that was pulled (round-trip preservation for the fresh-pull case).
- Re-pull onto an existing flat `files.org` does not break.
- No new dependencies; no schema changes to existing properties.

## Non-Goals

- Reorganizing an already-flat `files.org` into a hierarchy on re-pull. Conversion path: delete `files.org` and re-pull.
- Storing folder Canvas IDs on folder headings. Push resolves folders by path, not ID, so persisting the ID is unnecessary.
- Migrating an existing flat `content/` directory to the new nested layout.

## Design Choices

Two design choices were made before writing this spec:

**1. On-disk layout: mirror Canvas folders under `content/`.** A file at Canvas folder `Labs/Week 1` downloads to `content/Labs/Week 1/<display_name>`, and its `[[file:…]]` link in `files.org` points to that same path. Push reads the Canvas target folder from heading nesting (independent of link path), so the on-disk layout becomes free documentation and avoids cross-folder filename collisions.

**2. Re-pull semantics: fresh-pull only.** When `files.org` already contains entries with `CANVAS_ID`s, the existing in-place upsert logic runs unchanged and the flat structure persists. Hierarchy is only built when emitting a fresh tree. Conversion = `rm files.org && org-canvas-pull-files` (the `content/` directory can stay; new downloads will land in nested paths and old flat copies become orphaned but harmless).

**3. Hierarchical re-pull is rejected.** After a successful fresh-tree pull, files.org contains folder-only headings (no `CANVAS_ID`, no file link). The next re-pull detects this and refuses with a clear message: existing nested file headings would not be found by the level-1-only upsert, producing duplicate level-1 entries. Users wanting refreshed Canvas state on a hierarchical layout must `rm files.org && org-canvas-pull-files`.

## Architecture

`org-canvas-pull-files` becomes a 3-step pipeline:

```
GET /api/v1/courses/:id/folders (paginated)  ──► folder-id → relative-path map
GET /api/v1/courses/:id/files   (paginated)  ──┐
                                                ├──► group by relative-path, sort
                                                └──► emit nested tree (folders + files)
```

The dispatcher consults `org-canvas--file-pull-mode` (see Components) to choose between three branches: `'fresh` (run emit-fresh-tree), `'flat` (run the existing upsert path), or `'hierarchical` (refuse with `user-error`).

## Components

### `org-canvas--file-pull-fetch-folders`
Fetches all folders via `GET /api/v1/courses/:id/folders` (paginated using the existing `org-canvas-api-request-all-pages` helper). Returns a hash table mapping `folder_id` (number) to relative path (string). The Canvas root folder maps to `""`.

### `org-canvas--file-pull-folder-relative-path`
Pure helper. Takes a Canvas folder's `full_name` (e.g., `"course files"`, `"course files/Labs"`, `"course files/Labs/Week 1"`) and strips the leading `"course files"` segment, returning the relative path. The leading segment is *only* stripped when it equals `"course files"` exactly — if a course has been renamed or the API returns a different prefix, the original `full_name` is returned unchanged so we don't silently lose path components.

### `org-canvas--file-pull-mode`
Returns one of three symbols by inspecting `files.org`:
- `'fresh` — no entries with `CANVAS_ID` and no folder-only headings. Run the fresh-tree emitter.
- `'flat` — at least one `CANVAS_ID` exists and every heading either has a `CANVAS_ID` or contains a file link (`[[file:…]]` or `[[pdf:…]]`). Run the existing flat upsert.
- `'hierarchical` — at least one heading exists with neither `CANVAS_ID` nor a file link (i.e., a folder heading from a previous fresh-tree pull). Refuse with a clear user-error.

### `org-canvas--file-pull-emit-fresh-tree`
Given the folder map and the file list, emits the nested heading structure into the `files.org` buffer:
1. Group files by their relative folder path (looked up via `folder_id`).
2. Sort paths lexically; within each path, sort files by `display_name`.
3. Walk sorted paths, emitting folder ancestor headings as needed (`* Labs`, `** Week 1`) once each — not once per file.
4. For each file: emit a heading at the appropriate level with link `[[file:content/<rel-path>/<display_name>][<display_name>]]` (or `[[file:content/<display_name>]]` at root), set `CANVAS_ID` and `LAST_SYNCED`, populate properties via `org-canvas--file-pull-set-properties`, and download via `org-canvas--file-pull-download` to the local nested path.

The emitter appends headings to the existing buffer; any leading content (e.g., `#+TITLE: Files`, comment lines) is preserved. Fresh mode is detected by the absence of `CANVAS_ID` properties, not by buffer emptiness.

### `org-canvas--file-pull-download` (existing, modified)
Existing helper at `lisp/org-canvas-files.el:881`. Modify to ensure the parent directory exists before `url-copy-file` (it currently assumes flat `content/`).

### `org-canvas-pull-files` (existing, restructured)
The top-level entry point becomes a dispatcher:
1. Clear caches; fetch folders → folder map (failure → empty map).
2. Fetch files (paginated).
3. Open `files.org`; check `org-canvas--file-pull-mode`:
   - `'fresh` → call `emit-fresh-tree`.
   - `'flat` → loop over files using current upsert behavior; log a one-line info note that hierarchy is unavailable in flat mode.
   - `'hierarchical` → `user-error` with message: `"Hierarchical files.org detected; re-pull is not yet supported on a nested layout. Delete files.org and re-run org-canvas-pull-files."`
4. Save buffer (skipped in hierarchical-refusal case).

## Data Flow

**Fresh pull:**
1. `pull-files` clears `org-canvas--file-folder-cache` and `org-canvas--file-root-folder-cache`.
2. Fetches folders, builds `folder-id → relative-path` map.
3. Fetches files (paginated).
4. Detects empty/fresh state in `files.org`.
5. Groups files by relative path; sorts.
6. Walks the sorted tree, emitting folder ancestor headings on first visit, then file headings under them.
7. For each file: `mkdir -p content/<rel-path>/`, download via `url-copy-file` to nested path, set properties.

**Re-pull on flat file:**
1. Same fetches, but `fresh-mode-p` returns `nil`.
2. Logs warning: `"Existing flat files.org detected; new files will land at root. Delete files.org and re-pull to rebuild hierarchy."`
3. Iterates through files using existing `pull-upsert-heading` logic, no path changes, downloads to flat `content/`.

## Error Handling

- **Folder-list fetch fails** (network, 404, 401): catch the error, log a warning, and proceed with an empty folder map. In fresh mode this means every file maps to the root path and the emitter produces flat output — equivalent to today's behavior. The user still gets files; a warning tells them hierarchy was unavailable this run.
- **File's `folder_id` not in map** (folder hidden, deleted between the two API calls): log a warning identifying the file; place at root in the emitted tree.
- **Cross-folder same-name collision** (Canvas allows two files with same `display_name` in different folders): handled naturally — different folder paths produce different download paths and different headings.
- **Within-folder same-name collision** (rare; Canvas typically renames on upload): the second file's heading overwrites the first via `pull-upsert-heading` matching on `CANVAS_ID`. Document as a known edge case in the function docstring.
- **`url-copy-file` fails partway** (e.g., one file's URL 403s): existing behavior — log warning, continue. Empty `mkdir -p` directories are left in place; harmless.

## Testing

New `describe` blocks in `test/org-canvas-files-test.el` mirror the existing fresh-tree spec structure (currently around line 1878):

- **Fresh pull, all files at root** → top-level headings, no folder headings, downloads to `content/<name>` (regression test for existing behavior).
- **Fresh pull, mixed root + one-deep folder** → root files at top level, folder file under `* FolderName`, downloads to `content/FolderName/<name>`.
- **Fresh pull, two-deep folder** → `* Labs` / `** Week 1` / `*** [[file:content/Labs/Week 1/foo.pdf]]`, download path matches.
- **Fresh pull, same `display_name` in two folders** → two distinct headings under different folder ancestors, two distinct download paths, no upsert collision.
- **Fresh pull, multiple files share a folder** → exactly one folder heading emitted (not one per file).
- **Re-pull on flat files.org** → flat structure preserved, no folder headings inserted, warning message logged, existing CANVAS_IDs respected.
- **Re-pull on hierarchical files.org** → `user-error` raised with the documented message; buffer not modified; no API calls beyond the initial folder/files fetches that already happened (acceptable — they're idempotent).
- **Folder-list fetch raises an error** → falls back to flat emission, no exception escapes.
- **Folder `full_name` lacks `course files` prefix** → `folder-relative-path` returns the input unchanged (no silent path loss).

All tests use the existing `with-mock-api` and temp-directory patterns from the current pull-files specs.

## Out of Scope (deferred)

- Reorganizing existing flat `files.org` into hierarchy on re-pull (option 2c from brainstorm).
- Refreshing Canvas state on a hierarchical `files.org` without deleting it first (would require generalizing `pull-upsert-heading` to scan all levels and handle Canvas-side folder moves).
- Persisting `folder_id` on folder headings.
- Cleaning up orphan flat downloads in `content/` after a fresh re-pull migration.
- Symmetric "file move" detection between pulls (Canvas folder change for an existing CANVAS_ID).
- The same kind of folder/structure preservation for other content types.
