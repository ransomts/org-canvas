# Pull-Files Folder Hierarchy Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `org-canvas-pull-files` reconstruct the Canvas folder hierarchy as nested Org headings on a fresh pull, mirroring the same hierarchy under `content/` on disk, while keeping the existing flat upsert behavior on re-pull.

**Architecture:** Add a folder-fetch step ahead of the file-fetch, build a `folder-id → relative-path` map, and dispatch on a 3-state mode predicate (`fresh`/`flat`/`hierarchical`). Fresh mode emits a nested heading tree directly into `files.org` and downloads to nested paths under `content/`. Flat mode keeps current upsert behavior. Hierarchical re-pull is rejected with `user-error`.

**Tech Stack:** Emacs Lisp; `plz` (HTTP); `buttercup` (tests); `eldev` (build/test); `undercover` (coverage). Spec: `documentation/architecture/specs/2026-04-25-pull-files-folder-hierarchy-design.md`.

---

## File Structure

**Modified:**
- `lisp/org-canvas-files.el` — add 5 new helpers, modify 1 existing helper (`org-canvas--file-pull-download`), restructure `org-canvas-pull-files` entry point.
- `test/org-canvas-files-test.el` — add new `describe` blocks for each helper and integration scenarios. Update existing `org-canvas-pull-files` tests to mock both folders and files endpoints.

**Not changed:** all other modules; `org-canvas--pull-upsert-heading` keeps its `LEVEL=1` scope; no new files; no schema changes to existing properties.

---

### Task 1: Add `org-canvas--file-pull-folder-relative-path` (pure helper)

**Files:**
- Modify: `lisp/org-canvas-files.el` — append new helper near other pull helpers (after the existing `org-canvas--file-pull-download` block around line 894).
- Test: `test/org-canvas-files-test.el` — add new `describe` block at end of file.

- [ ] **Step 1: Write the failing tests**

Append to `test/org-canvas-files-test.el`:

```elisp
(describe "org-canvas--file-pull-folder-relative-path"
  (it "returns empty string for the Canvas root folder"
    (expect (org-canvas--file-pull-folder-relative-path "course files")
            :to-equal ""))
  (it "strips the prefix from a one-deep folder"
    (expect (org-canvas--file-pull-folder-relative-path "course files/Labs")
            :to-equal "Labs"))
  (it "strips the prefix from a deeply nested folder"
    (expect (org-canvas--file-pull-folder-relative-path "course files/Labs/Week 1")
            :to-equal "Labs/Week 1"))
  (it "leaves an unrecognized prefix unchanged"
    (expect (org-canvas--file-pull-folder-relative-path "other root/x")
            :to-equal "other root/x"))
  (it "returns empty string for nil"
    (expect (org-canvas--file-pull-folder-relative-path nil)
            :to-equal "")))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `eldev test "org-canvas--file-pull-folder-relative-path"`
Expected: 5 failures, "Symbol's function definition is void: org-canvas--file-pull-folder-relative-path".

- [ ] **Step 3: Write minimal implementation**

Append to `lisp/org-canvas-files.el` near the other pull helpers (after `org-canvas--file-pull-download` on line 893):

```elisp
(defun org-canvas--file-pull-folder-relative-path (full-name)
  "Return FULL-NAME with the Canvas \"course files\" prefix stripped.
Canvas folder full_names start with \"course files\" by convention.
Return \"\" for the root folder, the suffix when the prefix matches,
or FULL-NAME unchanged otherwise (so unrecognized layouts don't
silently lose path components)."
  (cond
   ((null full-name) "")
   ((string= full-name "course files") "")
   ((string-prefix-p "course files/" full-name)
    (substring full-name (length "course files/")))
   (t full-name)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `eldev test "org-canvas--file-pull-folder-relative-path"`
Expected: 5 passing, 0 failing.

- [ ] **Step 5: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): add folder-relative-path helper for pull hierarchy

Pure helper that strips the Canvas "course files" prefix from a
folder's full_name. Handles root, nested folders, unrecognized
prefixes, and nil. First building block for folder-aware pull-files.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Add `org-canvas--file-pull-fetch-folders` helper

**Files:**
- Modify: `lisp/org-canvas-files.el` — add helper after Task 1's helper.
- Test: `test/org-canvas-files-test.el` — append new `describe` block.

- [ ] **Step 1: Write the failing tests**

Append to `test/org-canvas-files-test.el`:

```elisp
(describe "org-canvas--file-pull-fetch-folders"
  (it "builds id-to-relative-path hash from folders API response"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((id . 100) (full_name . "course files"))
                     ((id . 101) (full_name . "course files/Labs"))
                     ((id . 102) (full_name . "course files/Labs/Week 1"))))))
        (let ((map (org-canvas--file-pull-fetch-folders)))
          (expect (gethash 100 map) :to-equal "")
          (expect (gethash 101 map) :to-equal "Labs")
          (expect (gethash 102 map) :to-equal "Labs/Week 1")
          (expect (hash-table-count map) :to-equal 3)))))

  (it "skips folders that have no id"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   '(((full_name . "no id"))
                     ((id . 5) (full_name . "course files/Ok"))))))
        (let ((map (org-canvas--file-pull-fetch-folders)))
          (expect (gethash 5 map) :to-equal "Ok")
          (expect (hash-table-count map) :to-equal 1)))))

  (it "returns an empty hash when API returns no folders"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params) '())))
        (let ((map (org-canvas--file-pull-fetch-folders)))
          (expect (hash-table-count map) :to-equal 0))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `eldev test "org-canvas--file-pull-fetch-folders"`
Expected: 3 failures, "Symbol's function definition is void: org-canvas--file-pull-fetch-folders".

- [ ] **Step 3: Write minimal implementation**

Append to `lisp/org-canvas-files.el` after Task 1's helper:

```elisp
(defun org-canvas--file-pull-fetch-folders ()
  "Fetch all course folders from Canvas.
Return a hash table mapping folder id (number, `eql' test) to its
path relative to the Canvas root (string; `\"\"' for the root)."
  (let ((map (make-hash-table :test 'eql))
        (endpoint (org-canvas-api-course-endpoint "folders")))
    (dolist (folder (org-canvas-api-request-all-pages 'GET endpoint))
      (let ((id (alist-get 'id folder)))
        (when id
          (puthash id
                   (org-canvas--file-pull-folder-relative-path
                    (alist-get 'full_name folder))
                   map))))
    map))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `eldev test "org-canvas--file-pull-fetch-folders"`
Expected: 3 passing.

- [ ] **Step 5: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): fetch Canvas folders into id-to-path hash

GET /api/v1/courses/:id/folders (paginated), build a hash mapping
folder id to relative path. Skips entries lacking an id. Used by the
pull-files dispatcher to associate each file with its Canvas folder.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Add `org-canvas--file-pull-mode` 3-state predicate

**Files:**
- Modify: `lisp/org-canvas-files.el` — add helper.
- Test: `test/org-canvas-files-test.el` — append new `describe`.

- [ ] **Step 1: Write the failing tests**

Append to `test/org-canvas-files-test.el`:

```elisp
(describe "org-canvas--file-pull-mode"
  (it "returns 'fresh for an empty buffer"
    (with-temp-org-buffer ""
      (expect (org-canvas--file-pull-mode) :to-equal 'fresh)))

  (it "returns 'fresh for a buffer with only #+TITLE: header"
    (with-temp-org-buffer "#+TITLE: Files\n# comment line\n"
      (expect (org-canvas--file-pull-mode) :to-equal 'fresh)))

  (it "returns 'flat when every heading has CANVAS_ID and a file link"
    (with-temp-org-buffer
        "* [[file:content/a.pdf][a.pdf]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
* [[file:content/b.pdf][b.pdf]]
:PROPERTIES:
:CANVAS_ID: 2
:END:
"
      (expect (org-canvas--file-pull-mode) :to-equal 'flat)))

  (it "returns 'hierarchical when a folder-only heading is present"
    (with-temp-org-buffer
        "* Labs
** [[file:content/Labs/a.pdf][a.pdf]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
      (expect (org-canvas--file-pull-mode) :to-equal 'hierarchical)))

  (it "returns 'hierarchical when one heading lacks both CANVAS_ID and a link"
    (with-temp-org-buffer
        "* Some folder name
* [[file:content/a.pdf][a.pdf]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
      (expect (org-canvas--file-pull-mode) :to-equal 'hierarchical))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `eldev test "org-canvas--file-pull-mode"`
Expected: 5 failures.

- [ ] **Step 3: Write minimal implementation**

Append to `lisp/org-canvas-files.el`:

```elisp
(defun org-canvas--file-pull-mode ()
  "Return the pull mode for the current `files.org' buffer.

Returns one of three symbols:
- `fresh' if no entry has a CANVAS_ID property and no folder-only
  heading is present (initial pull, file empty or header-only).
- `flat' if at least one CANVAS_ID exists and every heading either
  has a CANVAS_ID or contains a file link (legacy layout from before
  folder hierarchy support).
- `hierarchical' if any heading lacks both a CANVAS_ID and a file link
  (a folder heading from a previous fresh-tree pull).  The dispatcher
  uses this to refuse re-pull on a nested layout."
  (let ((has-cid nil)
        (folder-only nil))
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-map-entries
      (lambda ()
        (let* ((heading (org-canvas--strip-statistics-cookie
                         (org-get-heading t t t t)))
               (link-path (org-canvas--file-extract-link-path heading))
               (cid (org-entry-get (point) "CANVAS_ID")))
          (when cid (setq has-cid t))
          (unless (or cid link-path)
            (setq folder-only t))))
      t 'file))
    (cond (folder-only 'hierarchical)
          (has-cid 'flat)
          (t 'fresh))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `eldev test "org-canvas--file-pull-mode"`
Expected: 5 passing.

- [ ] **Step 5: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): add 3-state pull-mode predicate (fresh/flat/hierarchical)

Inspects files.org for CANVAS_ID properties and file links to choose
between the fresh-tree emitter, the existing flat upsert path, and a
refusal for re-pull on a nested layout.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Make `org-canvas--file-pull-download` create parent directories

**Files:**
- Modify: `lisp/org-canvas-files.el:881-893` — `org-canvas--file-pull-download`.
- Test: `test/org-canvas-files-test.el` — extend existing `describe "org-canvas--file-pull-download"` block.

- [ ] **Step 1: Write the failing test**

Append a new `it` block inside the existing `describe "org-canvas--file-pull-download"` block (currently around line 1829):

```elisp
  (it "creates the parent directory tree before downloading"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (nested-path (expand-file-name "Labs/Week 1/lab.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url path &rest _args)
                       (setq downloaded path)
                       (with-temp-file path (insert "x")))))
            (org-canvas--file-pull-download
             "lab.pdf" "https://example.com/lab.pdf" nested-path 1)
            (expect downloaded :to-equal nested-path)
            (expect (file-directory-p
                     (expand-file-name "Labs/Week 1" temp-dir))
                    :to-be-truthy))
        (delete-directory temp-dir t))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `eldev test "org-canvas--file-pull-download"`
Expected: the new test fails because `url-copy-file` mock would succeed but the directory doesn't get created — actually `make-directory` is missing entirely, the assertion fails on `file-directory-p`.

- [ ] **Step 3: Make the minimal change**

Edit `lisp/org-canvas-files.el` at the `org-canvas--file-pull-download` body. Find:

```elisp
  (when (and download-url (not (file-exists-p local-path)))
    (condition-case err
        (progn
          (org-canvas--log-info org-canvas--logger
            "[Download] %s (%s bytes)" display-name (or size "?"))
          (url-copy-file download-url local-path t))
```

Replace with:

```elisp
  (when (and download-url (not (file-exists-p local-path)))
    (condition-case err
        (progn
          (make-directory (file-name-directory local-path) t)
          (org-canvas--log-info org-canvas--logger
            "[Download] %s (%s bytes)" display-name (or size "?"))
          (url-copy-file download-url local-path t))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `eldev test "org-canvas--file-pull-download"`
Expected: all 5 tests pass (4 original + 1 new).

- [ ] **Step 5: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): mkdir -p the parent before downloading a pulled file

Prepares pull-files to download into nested content/<rel-path>/ paths.
Existing flat downloads are unaffected (mkdir of an existing dir is a
no-op when CREATE-PARENTS is t).

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Add `org-canvas--file-pull-emit-fresh-tree` — root-only files

**Files:**
- Modify: `lisp/org-canvas-files.el` — add new helper.
- Test: `test/org-canvas-files-test.el` — add new `describe` block.

This task implements the simplest case: every file is at the Canvas root. Tasks 6 and 7 layer in nested folders and edge cases.

- [ ] **Step 1: Write the failing test**

Append to `test/org-canvas-files-test.el`:

```elisp
(describe "org-canvas--file-pull-emit-fresh-tree"
  (it "emits one top-level heading per file when all files are at root"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql))
           (downloads nil))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url path &rest _args)
                         (push path downloads))))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "syllabus.pdf")
                    (folder_id . 100) (url . "https://example.com/syllabus.pdf")
                    (content-type . "application/pdf") (size . 2048))
                   ((id . 2) (display_name . "schedule.pdf")
                    (folder_id . 100) (url . "https://example.com/schedule.pdf")
                    (content-type . "application/pdf") (size . 1024)))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect body :to-match "^\\* \\[\\[file:content/schedule\\.pdf\\]\\[schedule\\.pdf\\]\\]$")
                  (expect body :to-match "^\\* \\[\\[file:content/syllabus\\.pdf\\]\\[syllabus\\.pdf\\]\\]$")
                  (expect body :to-match ":CANVAS_ID: 1")
                  (expect body :to-match ":CANVAS_ID: 2")
                  (expect body :to-match ":CONTENT_TYPE: application/pdf")
                  (expect body :to-match ":SIZE: 2048")))
              (expect (length downloads) :to-equal 2)
              (expect (cl-some (lambda (p)
                                 (string-suffix-p "content/syllabus.pdf" p))
                               downloads)
                      :to-be-truthy)))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `eldev test "org-canvas--file-pull-emit-fresh-tree"`
Expected: failure, "Symbol's function definition is void".

- [ ] **Step 3: Write minimal implementation**

Append to `lisp/org-canvas-files.el`:

```elisp
(defun org-canvas--file-pull-group-by-folder (folder-map remote-items)
  "Group REMOTE-ITEMS by their folder-relative path.
FOLDER-MAP is the hash from `org-canvas--file-pull-fetch-folders'.
Returns an alist ((REL-PATH . ITEMS) ...).  Items whose `folder_id'
is missing from FOLDER-MAP fall under \"\" (root)."
  (let ((groups nil))
    (dolist (item remote-items)
      (let* ((folder-id (alist-get 'folder_id item))
             (rel-path (or (and folder-id (gethash folder-id folder-map)) ""))
             (cell (assoc rel-path groups)))
        (if cell
            (setcdr cell (cons item (cdr cell)))
          (push (cons rel-path (list item)) groups))))
    groups))

(defun org-canvas--file-pull-emit-file-heading (item depth rel-path content-dir total counter)
  "Emit a heading + properties for ITEM at DEPTH under REL-PATH.
COUNTER is a cons cell whose car is the running file count (mutated).
TOTAL is the count of all files for the progress message.
Downloads to CONTENT-DIR/REL-PATH/DISPLAY_NAME."
  (let* ((id (alist-get 'id item))
         (display-name (alist-get 'display_name item))
         (download-url (alist-get 'url item))
         (local-rel (if (string-empty-p rel-path)
                        display-name
                      (concat rel-path "/" display-name)))
         (link-target (concat "content/" local-rel))
         (heading-text (org-link-make-string
                        (concat "file:" link-target)
                        display-name))
         (local-path (expand-file-name local-rel content-dir)))
    (insert (make-string depth ?*) " " heading-text "\n")
    (let ((pos (save-excursion (forward-line -1) (point))))
      (org-canvas-org-save-sync-state pos id)
      (org-canvas--file-pull-set-properties pos item))
    (setcar counter (1+ (car counter)))
    (message "Files [%d/%d] Pulling '%s'..." (car counter) total display-name)
    (org-canvas--file-pull-download
     display-name download-url local-path (alist-get 'size item))))

(defun org-canvas--file-pull-emit-fresh-tree (folder-map remote-items content-dir)
  "Emit a folder-aware heading tree for REMOTE-ITEMS into the current buffer.
FOLDER-MAP maps folder id to relative path; empty path is root.
CONTENT-DIR is the local directory under which files are downloaded.

Files at root become level-1 headings.  Files in a folder get
ancestor folder headings emitted once each before the file headings.
Properties are written via `org-canvas-org-save-sync-state' and
`org-canvas--file-pull-set-properties' for symmetry with flat mode."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let* ((groups (org-canvas--file-pull-group-by-folder folder-map remote-items))
         (sorted-paths (sort (mapcar #'car groups) #'string<))
         (total (length remote-items))
         (counter (list 0)))
    (dolist (rel-path sorted-paths)
      (let* ((items (sort (cdr (assoc rel-path groups))
                          (lambda (a b)
                            (string< (alist-get 'display_name a)
                                     (alist-get 'display_name b)))))
             (file-depth 1))
        (dolist (item items)
          (org-canvas--file-pull-emit-file-heading
           item file-depth rel-path content-dir total counter))))
    (car counter)))
```

This intentionally ignores nested folders (Task 6 layers them in). At Task 5 every file goes to depth 1 regardless of `rel-path`. The test only exercises root files so this passes.

- [ ] **Step 4: Run test to verify it passes**

Run: `eldev test "org-canvas--file-pull-emit-fresh-tree"`
Expected: pass.

- [ ] **Step 5: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): emit-fresh-tree for root-only files

First slice of the folder-aware emitter: emits a top-level heading
for each file, sets CANVAS_ID + LAST_SYNCED + content-type + size,
downloads to content/<display_name>. Folder ancestors come next.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: Extend `emit-fresh-tree` for nested folders (with dedup)

**Files:**
- Modify: `lisp/org-canvas-files.el` — extend the emit function and add a path-walk helper.
- Test: `test/org-canvas-files-test.el` — add nested-folder cases to the existing `describe`.

- [ ] **Step 1: Write the failing tests**

Append inside the `describe "org-canvas--file-pull-emit-fresh-tree"` block:

```elisp
  (it "emits a folder heading once for multiple files in the same folder"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (puthash 200 "Labs" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "lab1.pdf") (folder_id . 200)
                    (url . "https://example.com/lab1.pdf"))
                   ((id . 2) (display_name . "lab2.pdf") (folder_id . 200)
                    (url . "https://example.com/lab2.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect (cl-count ?\n
                                    (replace-regexp-in-string
                                     "[^\n]" ""
                                     (replace-regexp-in-string
                                      "\\`.*?\\(^\\* Labs\\)" ""
                                      body)))
                          :to-be-greater-than 0)
                  (expect body :to-match "^\\* Labs$")
                  ;; Exactly one "* Labs" heading
                  (expect (with-temp-buffer
                            (insert body)
                            (goto-char (point-min))
                            (let ((n 0))
                              (while (re-search-forward "^\\* Labs$" nil t)
                                (cl-incf n))
                              n))
                          :to-equal 1)
                  (expect body :to-match "^\\*\\* \\[\\[file:content/Labs/lab1\\.pdf\\]\\[lab1\\.pdf\\]\\]$")
                  (expect body :to-match "^\\*\\* \\[\\[file:content/Labs/lab2\\.pdf\\]\\[lab2\\.pdf\\]\\]$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "emits ancestor folder headings for a deeply nested folder"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (puthash 300 "Labs/Week 1" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "intro.pdf") (folder_id . 300)
                    (url . "https://example.com/intro.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect body :to-match "^\\* Labs$")
                  (expect body :to-match "^\\*\\* Week 1$")
                  (expect body :to-match
                          "^\\*\\*\\* \\[\\[file:content/Labs/Week 1/intro\\.pdf\\]\\[intro\\.pdf\\]\\]$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "does not duplicate shared ancestor headings between sibling subfolders"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            (puthash 301 "Labs/Week 1" folder-map)
            (puthash 302 "Labs/Week 2" folder-map)
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "w1.pdf") (folder_id . 301)
                    (url . "https://example.com/w1.pdf"))
                   ((id . 2) (display_name . "w2.pdf") (folder_id . 302)
                    (url . "https://example.com/w2.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect (with-temp-buffer
                            (insert body)
                            (goto-char (point-min))
                            (let ((n 0))
                              (while (re-search-forward "^\\* Labs$" nil t)
                                (cl-incf n))
                              n))
                          :to-equal 1)
                  (expect body :to-match "^\\*\\* Week 1$")
                  (expect body :to-match "^\\*\\* Week 2$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `eldev test "org-canvas--file-pull-emit-fresh-tree"`
Expected: 3 new failures (root-only test still passes).

- [ ] **Step 3: Replace the emit-fresh-tree body to handle ancestor headings**

In `lisp/org-canvas-files.el`, replace the Task 5 implementation of `org-canvas--file-pull-emit-fresh-tree` with:

```elisp
(defun org-canvas--file-pull-common-prefix-len (a b)
  "Return the count of leading elements shared between lists A and B."
  (let ((i 0))
    (while (and (nth i a) (nth i b)
                (string= (nth i a) (nth i b)))
      (setq i (1+ i)))
    i))

(defun org-canvas--file-pull-emit-folder-ancestors (current-parts new-parts)
  "Insert folder headings to descend from CURRENT-PARTS to NEW-PARTS.
Both are lists of path components.  Headings are emitted only for
the suffix of NEW-PARTS that's not already shared with CURRENT-PARTS."
  (let ((common (org-canvas--file-pull-common-prefix-len current-parts new-parts))
        (i 0))
    (setq i common)
    (while (< i (length new-parts))
      (insert (make-string (1+ i) ?*) " " (nth i new-parts) "\n")
      (setq i (1+ i)))))

(defun org-canvas--file-pull-emit-fresh-tree (folder-map remote-items content-dir)
  "Emit a folder-aware heading tree for REMOTE-ITEMS into the current buffer.
FOLDER-MAP maps folder id to relative path; empty path is root.
CONTENT-DIR is the local directory under which files are downloaded.

Sorts folder paths lexically and files within each folder by
`display_name'.  Emits each ancestor folder heading exactly once.
Files at the root land at level 1; files at depth N land at
level N+1.  Properties are written via `org-canvas-org-save-sync-state'
and `org-canvas--file-pull-set-properties' for parity with flat mode."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let* ((groups (org-canvas--file-pull-group-by-folder folder-map remote-items))
         (sorted-paths (sort (mapcar #'car groups) #'string<))
         (total (length remote-items))
         (counter (list 0))
         (current-parts nil))
    (dolist (rel-path sorted-paths)
      (let* ((parts (if (string-empty-p rel-path) nil
                      (split-string rel-path "/" t)))
             (file-depth (1+ (length parts)))
             (items (sort (cdr (assoc rel-path groups))
                          (lambda (a b)
                            (string< (alist-get 'display_name a)
                                     (alist-get 'display_name b))))))
        (org-canvas--file-pull-emit-folder-ancestors current-parts parts)
        (setq current-parts parts)
        (dolist (item items)
          (org-canvas--file-pull-emit-file-heading
           item file-depth rel-path content-dir total counter))))
    (car counter)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `eldev test "org-canvas--file-pull-emit-fresh-tree"`
Expected: 4 passing (1 root-only + 3 new).

- [ ] **Step 5: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): emit-fresh-tree handles nested folders with dedup

Lexically sort folder paths, walk shared ancestor prefixes, and emit
each ancestor folder heading exactly once. Files land at depth N+1
where N is their folder depth.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Handle missing `folder_id` (fallback to root)

**Files:**
- Test: `test/org-canvas-files-test.el` — add one case to `describe "org-canvas--file-pull-emit-fresh-tree"`.

This case is already handled by `org-canvas--file-pull-group-by-folder` (the `or … ""` fallback), but we add an explicit test to lock the behavior in.

- [ ] **Step 1: Write the failing-or-passing test**

Append inside the same `describe` block:

```elisp
  (it "places a file with unknown folder_id at the root"
    (let* ((temp-dir (make-temp-file "emit-tree-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files\n"))
            (puthash 100 "" folder-map)
            ;; Note: folder_id 999 is NOT in folder-map.
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "orphan.pdf") (folder_id . 999)
                    (url . "https://example.com/orphan.pdf")))
                 content-dir)
                (save-buffer))
              (with-temp-buffer
                (insert-file-contents files-file)
                (let ((body (buffer-string)))
                  (expect body :to-match "^\\* \\[\\[file:content/orphan\\.pdf\\]\\[orphan\\.pdf\\]\\]$")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))
```

- [ ] **Step 2: Run test to verify it passes (no impl change needed)**

Run: `eldev test "org-canvas--file-pull-emit-fresh-tree"`
Expected: 5 passing. The fallback already works because `(or (and folder-id (gethash folder-id folder-map)) "")` returns `""` for unknown ids.

- [ ] **Step 3: Commit**

```bash
git add test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
test(files): lock in root-fallback for unknown folder_id

Documents that emit-fresh-tree places orphan files (folder_id absent
from the folder map) at the root rather than dropping them.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 8: Extract `org-canvas--file-pull-emit-flat` from existing pull-files

**Files:**
- Modify: `lisp/org-canvas-files.el:916-951` — split the loop body of `org-canvas-pull-files` into a separate helper.
- Test: existing `org-canvas-pull-files` tests still pass (no behavior change).

This is a pure refactor — same code, same behavior, new boundary so the dispatcher in Task 9 can call either path.

- [ ] **Step 1: Move the existing per-item loop into a new function**

In `lisp/org-canvas-files.el`, find the current body of `org-canvas-pull-files` (the loop starting `(dolist (item remote) …)`) and extract it. Replace the relevant section with a call to the new helper.

Before (existing code, around line 932-948):

```elisp
    (with-current-buffer (find-file-noselect file)
      (dolist (item remote)
        (cl-incf count)
        (let* ((id (alist-get 'id item))
               (display-name (alist-get 'display_name item))
               (download-url (alist-get 'url item))
               (local-path (expand-file-name display-name content-dir))
               (heading-text (org-link-make-string
                              (format "file:content/%s" display-name)
                              display-name))
               (pos (org-canvas--pull-upsert-heading file id heading-text)))
          (message "Files [%d/%d] Pulling '%s'..." count total display-name)
          (goto-char pos)
          (org-canvas-org-save-sync-state pos id)
          (org-canvas--file-pull-set-properties pos item)
          (org-canvas--file-pull-download
           display-name download-url local-path (alist-get 'size item))))
      (org-canvas--save-buffer))
```

After (replace with a call to the new helper):

```elisp
    (with-current-buffer (find-file-noselect file)
      (org-canvas--file-pull-emit-flat file remote content-dir)
      (org-canvas--save-buffer))
```

Add the new helper above `org-canvas-pull-files`:

```elisp
(defun org-canvas--file-pull-emit-flat (file remote content-dir)
  "Upsert REMOTE files as flat top-level headings in FILE.
FILE is the path to files.org, REMOTE is the list of file alists from
the Canvas API, CONTENT-DIR is the local download directory.
Preserves existing CANVAS_ID matches in place; new files are appended."
  (let ((total (length remote))
        (count 0))
    (dolist (item remote)
      (cl-incf count)
      (let* ((id (alist-get 'id item))
             (display-name (alist-get 'display_name item))
             (download-url (alist-get 'url item))
             (local-path (expand-file-name display-name content-dir))
             (heading-text (org-link-make-string
                            (format "file:content/%s" display-name)
                            display-name))
             (pos (org-canvas--pull-upsert-heading file id heading-text)))
        (message "Files [%d/%d] Pulling '%s'..." count total display-name)
        (goto-char pos)
        (org-canvas-org-save-sync-state pos id)
        (org-canvas--file-pull-set-properties pos item)
        (org-canvas--file-pull-download
         display-name download-url local-path (alist-get 'size item))))
    count))
```

- [ ] **Step 2: Run existing pull-files tests to verify behavior is unchanged**

Run: `eldev test "org-canvas-pull-files"`
Expected: 4 passing (existing tests untouched).

- [ ] **Step 3: Commit**

```bash
git add lisp/org-canvas-files.el
git commit -m "$(cat <<'EOF'
refactor(files): extract pull-emit-flat from pull-files body

Pure refactor: same loop, same behavior, separate function so the
dispatcher can route flat vs fresh-tree branches.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 9: Update `org-canvas-pull-files` to dispatch on mode

**Files:**
- Modify: `lisp/org-canvas-files.el:916-951` — `org-canvas-pull-files`.
- Test: `test/org-canvas-files-test.el` — update existing tests to mock both endpoints, add new dispatcher tests.

- [ ] **Step 1: Write the failing tests**

Replace the existing `describe "org-canvas-pull-files"` block in `test/org-canvas-files-test.el` (currently around line 1878). The existing tests need URL-aware mocks now. Add new dispatcher cases. Final block:

```elisp
(describe "org-canvas-pull-files"
  (defun test-files--mock-pages (folders files)
    "Return a lambda that dispatches on URL to FOLDERS or FILES."
    (lambda (_method url &optional _params)
      (cond
       ((string-match-p "/folders" url) folders)
       ((string-match-p "/files" url) files)
       (t '()))))

  (it "creates flat headings on a fresh empty files.org with all-root files"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files")))
                            '(((id . 1) (display_name . "syllabus.pdf")
                               (folder_id . 100)
                               (url . "https://example.com/syllabus.pdf")
                               (content-type . "application/pdf")
                               (size . 2048)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (expect (buffer-string) :to-match "syllabus.pdf")
                    (expect (buffer-string) :to-match "CANVAS_ID.*1")
                    (expect (buffer-string) :to-match "CONTENT_TYPE.*application/pdf")
                    (expect (buffer-string) :to-match "SIZE.*2048"))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates files.org when it does not exist"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect (file-exists-p files-file) :to-be-truthy)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "creates content directory"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect (file-directory-p
                           (expand-file-name "content" temp-dir))
                          :to-be-truthy)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "fresh pull builds folder hierarchy from non-root files"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files"))
                              ((id . 200) (full_name . "course files/Labs")))
                            '(((id . 1) (display_name . "lab1.pdf")
                               (folder_id . 200)
                               (url . "https://example.com/lab1.pdf")
                               (content-type . "application/pdf")
                               (size . 100)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (let ((body (buffer-string)))
                      (expect body :to-match "^\\* Labs$")
                      (expect body :to-match "^\\*\\* \\[\\[file:content/Labs/lab1\\.pdf\\]\\[lab1\\.pdf\\]\\]$")))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "re-pull on an existing flat files.org keeps flat structure"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file
              (insert "#+TITLE: Files\n* [[file:content/syllabus.pdf][syllabus.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files"))
                              ((id . 200) (full_name . "course files/Labs")))
                            '(((id . 1) (display_name . "syllabus.pdf")
                               (folder_id . 200)
                               (url . "https://example.com/syllabus.pdf")
                               (content-type . "application/pdf")
                               (size . 100)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (let ((body (buffer-string)))
                      ;; flat layout preserved — no folder heading inserted
                      (expect body :not :to-match "^\\* Labs$")
                      ;; existing heading still found and updated
                      (expect body :to-match "syllabus.pdf")
                      (expect body :to-match ":CANVAS_ID: 1")))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "re-pull on a hierarchical files.org refuses with user-error"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file
              (insert "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (expect (org-canvas-pull-files) :to-throw 'user-error)))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "falls back to flat emission when folder fetch fails"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method url &optional _params)
                             (cond
                              ((string-match-p "/folders" url)
                               (signal 'plz-error '("simulated folder fetch failure")))
                              ((string-match-p "/files" url)
                               '(((id . 1) (display_name . "fallback.pdf")
                                  (folder_id . 100)
                                  (url . "https://example.com/fallback.pdf")
                                  (content-type . "application/pdf")
                                  (size . 100)))))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  ;; Should not throw — falls back to empty folder map → flat emission.
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (let ((body (buffer-string)))
                      ;; File is emitted at root since folder map is empty.
                      (expect body :to-match "^\\* \\[\\[file:content/fallback\\.pdf\\]\\[fallback\\.pdf\\]\\]$")))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "escapes brackets in display_name to produce a parseable Org heading"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (bracketed "IML [Molnar] 2ed.pdf"))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages
                            '(((id . 100) (full_name . "course files")))
                            `(((id . 7) (display_name . ,bracketed)
                               (folder_id . 100)
                               (url . "https://example.com/x.pdf")
                               (content-type . "application/pdf")
                               (size . 100)))))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (goto-char (point-min))
                    (search-forward "[[file:")
                    (goto-char (match-beginning 0))
                    (let* ((link (org-element-link-parser))
                           (link-type (org-element-property :type link)))
                      (expect link-type :to-equal "file"))
                    (expect (buffer-string) :to-match "CANVAS_ID.*7"))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `eldev test "org-canvas-pull-files"`
Expected: at least the new fresh-hierarchy, hierarchical-refusal, and folder-fetch-fallback cases fail; existing cases may still pass since the pull function hasn't been wired through the dispatcher.

- [ ] **Step 3: Update `org-canvas-pull-files` to dispatch**

Replace the body of `org-canvas-pull-files` (around line 916) with:

```elisp
;;;###autoload
(defun org-canvas-pull-files ()
  "Pull file metadata from Canvas into files.org.
Downloads file contents to the content/ directory.

On a fresh pull (no CANVAS_IDs in files.org and no folder-only
headings), reconstructs the Canvas folder hierarchy as nested Org
headings and downloads under content/<rel-path>/.

On a re-pull of an existing flat files.org, updates entries in place
without restructuring.  Refuses to re-pull a hierarchical files.org
with `user-error' (delete files.org and re-run to refresh)."
  (interactive)
  (org-canvas--start-operation "PULLING FILES")
  (let* ((file (expand-file-name org-canvas-files-file))
         (content-dir (expand-file-name
                       "content" (file-name-directory file)))
         (folder-map (condition-case err
                         (org-canvas--file-pull-fetch-folders)
                       (error
                        (org-canvas--log-warning org-canvas--logger
                          "[Files] Folder fetch failed (%s); falling back to flat layout."
                          (error-message-string err))
                        (make-hash-table :test 'eql))))
         (endpoint (org-canvas-api-course-endpoint "files"))
         (remote (org-canvas-api-request-all-pages 'GET endpoint)))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (unless (file-directory-p content-dir)
      (make-directory content-dir t))
    (with-current-buffer (find-file-noselect file)
      (let ((mode (org-canvas--file-pull-mode)))
        (pcase mode
          ('hierarchical
           (user-error
            "Hierarchical files.org detected; re-pull is not yet supported on a nested layout. Delete files.org and re-run org-canvas-pull-files"))
          ('fresh
           (let ((emitted
                  (org-canvas--file-pull-emit-fresh-tree
                   folder-map remote content-dir)))
             (org-canvas--save-buffer)
             (org-canvas--log-info org-canvas--logger
               "Files pull complete (fresh tree): %d files" emitted)
             (message "Files pull complete: %d files." emitted)))
          ('flat
           (org-canvas--log-info org-canvas--logger
             "[Files] Existing flat files.org detected; running flat upsert. Delete files.org and re-pull to rebuild folder hierarchy.")
           (let ((emitted
                  (org-canvas--file-pull-emit-flat file remote content-dir)))
             (org-canvas--save-buffer)
             (org-canvas--log-info org-canvas--logger
               "Files pull complete (flat): %d files" emitted)
             (message "Files pull complete: %d files." emitted))))))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `eldev test "org-canvas-pull-files"`
Expected: all 8 tests in the block pass.

- [ ] **Step 5: Commit**

```bash
git add lisp/org-canvas-files.el test/org-canvas-files-test.el
git commit -m "$(cat <<'EOF'
feat(files): dispatch pull-files on fresh/flat/hierarchical mode

Reads files.org via the new pull-mode predicate and routes to the
fresh-tree emitter, the existing flat upsert, or a user-error refusal
for hierarchical re-pull. Folder fetch failures degrade to flat.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 10: Final verification — lint, full tests, coverage

**Files:** none modified (verification only).

- [ ] **Step 1: Run the linter**

Run: `eldev lint`
Expected: 0 warnings related to `org-canvas-files.el`. Fix any introduced issues (typically docstring formatting, byte-compiler warnings) before proceeding. If warnings appear, address them and re-run.

- [ ] **Step 2: Run the full test suite**

Run: `eldev test`
Expected: full suite green. Test count should be 2413 + new tests added in Tasks 1-9 (roughly 2429-2435).

- [ ] **Step 3: Check coverage threshold**

Run: `eldev test -u "on,text,dontsend" 2>&1 | tail -20`
Expected: overall coverage ≥99% (current baseline is 99.99%). If the new code drops coverage below threshold, identify uncovered branches and add targeted tests.

- [ ] **Step 4: Run the pre-push gauntlet locally before pushing**

Run: `.githooks/pre-push origin main` (or skip if not pushing yet).
Expected: lint + complexity + Emacs 29 tests + Emacs 30 tests all pass.

- [ ] **Step 5: Final smoke test against the user's pulled course**

Manual verification only:

```
1. Back up the existing canvas-structure/files.org and content/.
2. cd "~/Documents/20-teaching/4300 6300 Applied Data Science/2026 Spring/canvas-structure"
3. rm files.org
4. M-x org-canvas-pull-files
5. Inspect files.org — should show * <Folder> headings with ** <file> children mirroring Canvas.
6. Inspect content/ — files in folders should be at content/<Folder>/<name>.
```

Then confirm with the user before committing the smoke test as done.

---

## Self-Review

**Spec coverage check:**
- "Fresh pull produces nested headings + nested on-disk paths" → Tasks 5-7 + Task 9 fresh-hierarchy test.
- "Push of fresh-pulled files.org recreates Canvas hierarchy" → covered by existing push semantics + the new emitter producing folder ancestor headings (no separate test needed; push tests already verify the contract).
- "Re-pull on flat files.org doesn't break" → Task 9 re-pull-on-flat test.
- "Re-pull on hierarchical refuses" → Task 9 hierarchical-refusal test.
- "Folder-list fetch failure degrades gracefully" → Task 9 fallback test.
- "Strip 'course files' prefix only when exact match" → Task 1 unchanged-prefix test.
- "mkdir -p before download" → Task 4.
- "Folder heading emitted once per folder" → Task 6 sibling-subfolder dedup test.
- "Same display_name in two folders → no collision" → implicit in nested test paths; the fresh-hierarchy and ancestors tests both produce distinct paths. *(If desired, add an explicit collision test in Task 6 or 9 — left out as redundant since paths are deterministic from rel-path.)*

**Placeholder scan:** none — every step has runnable code or commands.

**Type consistency:** `folder-map` is consistently a hash with `eql` test, integer keys, string values; `rel-path` is consistently a string with empty `""` for root; `parts` is consistently a list of strings; `counter` is a single-element list (mutable).

**Scope:** single feature, single file mostly modified, one plan.
