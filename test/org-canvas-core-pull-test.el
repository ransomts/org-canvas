;;; org-canvas-core-pull-test.el --- Tests for org-canvas-core-pull -*- lexical-binding: t; -*-

;;; Commentary:

;; Pull helpers, the file-URL rewriter, the pull macros and the pull summary.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)
(require 'org-canvas-pages)
(require 'org-canvas-assignments)
(require 'org-canvas-sections)
(require 'org-canvas-files)

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
                      "^#\\+LAST_SYNCED: \\[[0-9-]+ \\w+ [0-9:]+\\]"))
            (kill-buffer))
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
                (expect count :to-equal 1)))
            (kill-buffer))
        (delete-file temp))))

  (it "inserts header at top when no #+TITLE present"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp (insert "* Heading\n"))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (goto-char (point-min))
              (expect (buffer-substring (point-min) (line-end-position))
                      :to-match "^#\\+LAST_SYNCED:"))
            (kill-buffer))
        (delete-file temp))))

  (it "inserts header into a brand-new empty buffer"
    (let ((temp (make-temp-file "org-test-" nil ".org")))
      (with-temp-file temp (insert ""))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp)
            (org-canvas--pull-write-file-header)
            (save-buffer)
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :to-match
                      "^#\\+LAST_SYNCED: \\[[0-9-]+ \\w+ [0-9:]+\\]"))
            (kill-buffer))
        (delete-file temp)))))

(describe "org-canvas--pull-read-file-header"
  (it "reads #+LAST_SYNCED from the current buffer"
    (with-temp-buffer
      (insert "#+TITLE: Pages\n#+LAST_SYNCED: [2026-04-26 Sun 12:00]\n* x\n")
      (expect (org-canvas--pull-read-file-header)
              :to-equal "[2026-04-26 Sun 12:00]")))

  (it "returns nil when no #+LAST_SYNCED"
    (with-temp-buffer
      (insert "#+TITLE: Pages\n* x\n")
      (expect (org-canvas--pull-read-file-header) :to-be nil))))

;;;; File-URL → Local-Link Rewriting (pull-side inverse of image resolver)

(describe "org-canvas--build-file-id-cache"
  (it "returns an empty hash for an empty files.org"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (let ((cache (org-canvas--build-file-id-cache temp-file)))
            (expect (hash-table-count cache) :to-equal 0))
        (delete-file temp-file))))

  (it "collects top-level file headings"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* [[file:content/foo.pdf][foo.pdf]]
:PROPERTIES:
:CANVAS_ID: 1001
:END:
* [[file:content/bar.png][bar.png]]
:PROPERTIES:
:CANVAS_ID: 1002
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (gethash "1001" cache) :to-equal "content/foo.pdf")
              (expect (gethash "1002" cache) :to-equal "content/bar.png")
              (expect (hash-table-count cache) :to-equal 2)))
        (delete-file temp-file))))

  (it "collects nested file headings under folder headings"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Uploaded Media
** [[file:content/Uploaded Media/img.png][img.png]]
:PROPERTIES:
:CANVAS_ID: 2001
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (gethash "2001" cache)
                      :to-equal "content/Uploaded Media/img.png")
              (expect (hash-table-count cache) :to-equal 1)))
        (delete-file temp-file))))

  (it "skips folder headings (no file: link, no CANVAS_ID)"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Uploaded Media
** [[file:content/Uploaded Media/img.png][img.png]]
:PROPERTIES:
:CANVAS_ID: 3001
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (hash-table-count cache) :to-equal 1)))
        (delete-file temp-file))))

  (it "skips entries missing CANVAS_ID"
    (let ((temp-file (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* [[file:content/foo.pdf][foo.pdf]]
:PROPERTIES:
:LAST_SYNCED: [2026-01-01 Thu 00:00]
:END:
"))
            (let ((cache (org-canvas--build-file-id-cache temp-file)))
              (expect (hash-table-count cache) :to-equal 0)))
        (delete-file temp-file))))

  (it "returns an empty hash for a nonexistent file"
    (let ((cache (org-canvas--build-file-id-cache "/nonexistent/files.org")))
      (expect (hash-table-count cache) :to-equal 0))))

(describe "org-canvas--rewrite-canvas-file-urls"
  ;; These tests focus on cache-hit behavior.  The cache-miss path now
  ;; triggers an on-demand API fetch (Task 6), so we stub the API to
  ;; signal an error — that exercises the pass-through-on-failure branch
  ;; without making real network calls.
  (let (cache)
    (before-each
      (setq cache (make-hash-table :test 'equal))
      (puthash "29257665" "content/Uploaded Media/img.png" cache)
      (puthash "28960058" "content/readings/ethics.pdf" cache)
      (org-canvas--pull-summary-reset)
      (spy-on 'org-canvas-api-request
              :and-call-fake
              (lambda (&rest _)
                (signal 'org-canvas-api-error '("Forbidden" nil nil)))))

    (it "rewrites a bare URL link with no description to file link with filename"
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/29257665/preview?verifier=abc]]"
               cache)
              :to-equal "[[file:content/Uploaded Media/img.png][img.png]]"))

    (it "preserves the description when present"
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/28960058?verifier=abc&wrap=1][What is data ethics.pdf]]"
               cache)
              :to-equal "[[file:content/readings/ethics.pdf][What is data ethics.pdf]]"))

    (it "leaves URLs alone whose IDs are not in the cache"
      (let ((input "[[https://x.instructure.com/courses/1/files/9999999/preview?verifier=z]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input cache)
                :to-equal input)))

    (it "rewrites multiple URLs in the same string"
      (let ((result (org-canvas--rewrite-canvas-file-urls
                     "before [[https://x.instructure.com/courses/1/files/29257665/preview?verifier=a]] middle [[https://x.instructure.com/courses/1/files/28960058?verifier=b][label]] after"
                     cache)))
        (expect result :to-match "\\[\\[file:content/Uploaded Media/img.png\\]\\[img.png\\]\\]")
        (expect result :to-match "\\[\\[file:content/readings/ethics.pdf\\]\\[label\\]\\]")
        (expect result :to-match "before ")
        (expect result :to-match " middle ")
        (expect result :to-match " after")))

    (it "leaves non-Canvas URLs alone"
      (let ((input "[[https://example.com/page][example]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input cache)
                :to-equal input)))

    (it "handles bare URL with no /preview suffix"
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/29257665]]"
               cache)
              :to-equal "[[file:content/Uploaded Media/img.png][img.png]]"))

    (it "rewrites bare ?verifier=&wrap=1 form without /preview and without description"
      ;; Regression: lock in that `files/NNNN?verifier=…&wrap=1' (no `/preview',
      ;; no `][desc]') is matched and rewritten just like the /preview form.
      (expect (org-canvas--rewrite-canvas-file-urls
               "[[https://x.instructure.com/courses/1/files/29257665?verifier=ABC&wrap=1]]"
               cache)
              :to-equal "[[file:content/Uploaded Media/img.png][img.png]]"))

    (it "leaves a bare ?verifier= URL unchanged when its ID is not in cache"
      ;; Regression: cache miss must pass through verbatim, not partially mangle
      ;; the URL (Task 6 will add an on-demand fetch for unknown IDs).
      (let ((input "[[https://x.instructure.com/courses/1/files/99999999?verifier=Z&wrap=1]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input cache)
                :to-equal input)))

    (it "returns nil unchanged"
      (expect (org-canvas--rewrite-canvas-file-urls nil cache) :to-be nil))

    (it "returns empty string unchanged"
      (expect (org-canvas--rewrite-canvas-file-urls "" cache) :to-equal ""))

    (it "is a no-op when cache is empty"
      (let ((empty (make-hash-table :test 'equal))
            (input "[[https://x.instructure.com/courses/1/files/29257665/preview?verifier=a]]"))
        (expect (org-canvas--rewrite-canvas-file-urls input empty)
                :to-equal input)))))

(describe "org-canvas--html-to-org-with-rewrite"
  (it "returns empty string for nil"
    (expect (org-canvas--html-to-org-with-rewrite nil) :to-equal ""))

  (it "returns empty string for empty input"
    (expect (org-canvas--html-to-org-with-rewrite "") :to-equal ""))

  (it "rewrites a Canvas file URL using the cache"
    (let ((org-canvas--file-id-cache (make-hash-table :test 'equal)))
      (puthash "30061566" "content/Uploaded Media/screenshot.png"
               org-canvas--file-id-cache)
      (with-html-to-org-identity
        (let ((result (org-canvas--html-to-org-with-rewrite
                       "[[https://x.com/courses/1/files/30061566/preview]]")))
          (expect result :to-match
                  "\\[\\[file:content/Uploaded Media/screenshot\\.png\\]"))))))

(describe "org-canvas--html-to-org-inline-with-rewrite"
  (it "returns empty string for nil"
    (expect (org-canvas--html-to-org-inline-with-rewrite nil) :to-equal ""))

  (it "returns empty string for empty input"
    (expect (org-canvas--html-to-org-inline-with-rewrite "") :to-equal ""))

  (it "collapses newlines and rewrites file URLs"
    (let ((org-canvas--file-id-cache (make-hash-table :test 'equal)))
      (puthash "42" "content/foo.png" org-canvas--file-id-cache)
      (with-html-to-org-identity
        (let ((result (org-canvas--html-to-org-inline-with-rewrite
                       "see\n[[https://x.com/courses/1/files/42/preview]]")))
          (expect result :not :to-match "\n")
          (expect result :to-match "\\[\\[file:content/foo\\.png\\]"))))))

(describe "fetch unknown file on rewrite"
  (before-each
    (setq org-canvas--rewrite-folder-cache nil)
    (org-canvas--pull-summary-reset))

  (it "fetches metadata, downloads, registers, and rewrites"
    (let ((cache (make-hash-table :test 'equal))
          (api-calls 0)
          (downloads 0)
          (org-canvas-directory (make-temp-file "test-rewrite-" t))
          (org-canvas-files-file nil))
      (unwind-protect
          (progn
            (setq org-canvas-files-file
                  (expand-file-name "files.org" org-canvas-directory))
            (with-temp-file org-canvas-files-file
              (insert "#+TITLE: Files\n"))
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (_method url &rest _args)
                         (cl-incf api-calls)
                         (cond
                          ((string-match-p "/api/v1/files/30061566\\'" url)
                           '((id . 30061566)
                             (display_name . "screenshot.png")
                             (folder_id . 999)
                             (url . "https://x.com/files/30061566/download?verifier=Z")
                             (content-type . "image/png")
                             (size . 12345)))
                          ((string-match-p "/api/v1/folders/999\\'" url)
                           '((id . 999)
                             (full_name . "course files/Uploaded Media"))))))
                      ((symbol-function 'org-canvas--file-pull-download)
                       (lambda (_dn _url path _size)
                         (cl-incf downloads)
                         (make-directory (file-name-directory path) t)
                         (with-temp-file path (insert "fake bytes")))))
              (let* ((input "see [[https://x.com/courses/281704/files/30061566/preview?verifier=A]]")
                     (rewritten (org-canvas--rewrite-canvas-file-urls input cache)))
                (expect rewritten :to-match
                        "\\[\\[file:content/Uploaded Media/screenshot\\.png\\]\\[screenshot\\.png\\]\\]")
                (expect downloads :to-equal 1)
                (expect (gethash "30061566" cache)
                        :to-equal "content/Uploaded Media/screenshot.png"))))
        (delete-directory org-canvas-directory t))))

  (it "falls back to the placeholder folder when the folder 403s (issue #171)"
    (let ((org-canvas--rewrite-folder-cache nil))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (signal 'org-canvas-permission-error
                           '("Permission denied (HTTP 403) reading folders")))))
        (expect (org-canvas--rewrite-fetch-folder-relpath 999) :to-be nil))))

  (it "survives a 403 on one file, passing the URL through (issue #171)"
    ;; A page body embedding a file from another course 403s for a
    ;; Designer.  Before #171 the permission error escaped this handler
    ;; and took the whole content type with it.
    (let ((cache (make-hash-table :test 'equal))
          (org-canvas-directory (make-temp-file "test-rewrite-403-" t))
          (org-canvas-files-file nil))
      (unwind-protect
          (progn
            (setq org-canvas-files-file
                  (expand-file-name "files.org" org-canvas-directory))
            (with-temp-file org-canvas-files-file (insert ""))
            (org-canvas--pull-summary-reset)
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (&rest _)
                         (signal 'org-canvas-permission-error
                                 '("Permission denied (HTTP 403) reading files")))))
              (let* ((input "x [[https://x.com/courses/208463/files/21157335/preview]] y")
                     (rewritten (org-canvas--rewrite-canvas-file-urls input cache)))
                (expect rewritten :to-equal input)
                (expect (length (org-canvas--pull-summary-records)) :to-equal 1))))
        (delete-directory org-canvas-directory t))))

  (it "returns nil and records to summary when metadata GET fails"
    (let ((cache (make-hash-table :test 'equal))
          (org-canvas-directory (make-temp-file "test-rewrite-fail-" t))
          (org-canvas-files-file nil))
      (unwind-protect
          (progn
            (setq org-canvas-files-file
                  (expand-file-name "files.org" org-canvas-directory))
            (with-temp-file org-canvas-files-file (insert ""))
            (org-canvas--pull-summary-reset)
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (&rest _)
                         (signal 'org-canvas-api-error '("Forbidden" nil nil)))))
              (let* ((input "x [[https://x.com/courses/281704/files/99999999?verifier=Z]] y")
                     (rewritten (org-canvas--rewrite-canvas-file-urls input cache)))
                ;; URL passes through unchanged
                (expect rewritten :to-equal input)
                ;; Failure recorded
                (expect (org-canvas--pull-summary-empty-p) :to-be nil))))
        (delete-directory org-canvas-directory t))))

  (it "caches resolved IDs for the rest of the session"
    (let ((cache (make-hash-table :test 'equal))
          (api-calls 0)
          (org-canvas-directory (make-temp-file "test-rewrite-cache-" t))
          (org-canvas-files-file nil))
      (unwind-protect
          (progn
            (setq org-canvas-files-file
                  (expand-file-name "files.org" org-canvas-directory))
            (with-temp-file org-canvas-files-file (insert ""))
            (cl-letf (((symbol-function 'org-canvas-api-request)
                       (lambda (_method url &rest _args)
                         (cl-incf api-calls)
                         (cond
                          ((string-match-p "/api/v1/files/30061566\\'" url)
                           '((id . 30061566)
                             (display_name . "screenshot.png")
                             (folder_id . 999)
                             (url . "https://x.com/files/30061566/download?verifier=Z")))
                          ((string-match-p "/api/v1/folders/999\\'" url)
                           '((full_name . "course files/Uploaded Media"))))))
                      ((symbol-function 'org-canvas--file-pull-download)
                       (lambda (_dn _url path _size)
                         (make-directory (file-name-directory path) t)
                         (with-temp-file path (insert "x")))))
              (let ((url1 "[[https://x.com/courses/281704/files/30061566/preview?verifier=A]]")
                    (url2 "[[https://x.com/courses/281704/files/30061566?verifier=B]]"))
                (org-canvas--rewrite-canvas-file-urls (concat url1 "\n" url2) cache)
                ;; Two URLs, but only one metadata fetch + one folder fetch
                (expect api-calls :to-equal 2))))
        (delete-directory org-canvas-directory t)))))

;;;; Pull-side buffer lifecycle helpers

(describe "org-canvas--pull-was-fresh-p"
  (it "returns t when file does not exist and no buffer visits it"
    (let ((temp (make-temp-file "fresh-" nil ".org")))
      (delete-file temp)
      (unwind-protect
          (expect (org-canvas--pull-was-fresh-p temp) :to-be-truthy)
        (when (file-exists-p temp) (delete-file temp)))))

  (it "returns nil when the file already exists on disk"
    (let ((temp (make-temp-file "exists-" nil ".org")))
      (unwind-protect
          (expect (org-canvas--pull-was-fresh-p temp) :to-be nil)
        (delete-file temp))))

  (it "returns nil when a buffer already visits the file"
    (let* ((temp (make-temp-file "visited-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (expect (org-canvas--pull-was-fresh-p temp) :to-be nil)
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file temp)))))

(describe "org-canvas--pull-kill-fresh-buffer"
  (it "kills the buffer when WAS-FRESH and buffer is unmodified"
    (let* ((temp (make-temp-file "kill-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (org-canvas--pull-kill-fresh-buffer temp t)
            (expect (find-buffer-visiting temp) :to-be nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (file-exists-p temp) (delete-file temp)))))

  (it "leaves the buffer alone when WAS-FRESH is nil"
    (let* ((temp (make-temp-file "keep-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (org-canvas--pull-kill-fresh-buffer temp nil)
            (expect (buffer-live-p buf) :to-be-truthy))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file temp))))

  (it "does not kill a modified buffer even when WAS-FRESH"
    (let* ((temp (make-temp-file "dirty-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edit") (set-buffer-modified-p t))
            (org-canvas--pull-kill-fresh-buffer temp t)
            (expect (buffer-live-p buf) :to-be-truthy))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp))))

  (it "is a no-op when no buffer visits the file"
    (let ((temp (make-temp-file "nobuf-" nil ".org")))
      (unwind-protect
          (progn
            (org-canvas--pull-kill-fresh-buffer temp t)
            (expect (find-buffer-visiting temp) :to-be nil))
        (delete-file temp)))))

(describe "org-canvas--pull-confirm-unsaved"
  (it "is a no-op when no buffer visits the file"
    (let ((temp (make-temp-file "noopen-" nil ".org")))
      (unwind-protect
          (expect (org-canvas--pull-confirm-unsaved temp "feature")
                  :not :to-throw)
        (delete-file temp))))

  (it "is a no-op when the visiting buffer is unmodified"
    (let* ((temp (make-temp-file "clean-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (expect (org-canvas--pull-confirm-unsaved temp "feature")
                  :not :to-throw)
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file temp))))

  (it "saves the buffer when the user answers yes"
    (let* ((temp (make-temp-file "yes-" nil ".org"))
           (buf (find-file-noselect temp))
           (saved nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edits") (set-buffer-modified-p t))
            ;; `noninteractive' is t under the test runner, which would
            ;; short-circuit the prompt; bind it off to exercise the answer.
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                        ((symbol-function 'org-canvas--save-buffer)
                         (lambda () (setq saved t) (set-buffer-modified-p nil))))
                (org-canvas--pull-confirm-unsaved temp "feature")))
            (expect saved :to-be-truthy))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp))))

  (it "signals user-error when the user answers no"
    (let* ((temp (make-temp-file "no-" nil ".org"))
           (buf (find-file-noselect temp)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edits") (set-buffer-modified-p t))
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                (expect (org-canvas--pull-confirm-unsaved temp "feature")
                        :to-throw 'user-error))))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp))))

  ;; Issue #34: under `emacs --batch' this prompt consumed stdin and
  ;; silently swallowed a step of a full `org-canvas-sync'.
  (it "saves without prompting in batch mode"
    (let* ((temp (make-temp-file "batch-" nil ".org"))
           (buf (find-file-noselect temp))
           (saved nil)
           (prompted nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "edits") (set-buffer-modified-p t))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) (setq prompted t) nil))
                      ((symbol-function 'org-canvas--save-buffer)
                       (lambda () (setq saved t) (set-buffer-modified-p nil))))
              (org-canvas--pull-confirm-unsaved temp "feature"))
            (expect prompted :to-be nil)
            (expect saved :to-be-truthy))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-file temp)))))

(describe "org-canvas--pull-confirm-overwrite"
  (it "aborts when the user declines"
    (let ((temp (make-temp-file "existing-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "* Existing heading\n"))
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                (expect (org-canvas--pull-confirm-overwrite temp "sections")
                        :to-throw 'user-error))))
        (delete-file temp))))

  (it "proceeds when the user accepts"
    (let ((temp (make-temp-file "existing-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "* Existing heading\n"))
            (let ((noninteractive nil))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                (expect (org-canvas--pull-confirm-overwrite temp "sections")
                        :not :to-throw))))
        (delete-file temp))))

  ;; This is the prompt that blocked batch runs: it fires for every pull
  ;; whose target file already exists, which is the normal case.
  (it "does not prompt in batch mode"
    (let ((temp (make-temp-file "existing-" nil ".org"))
          (prompted nil))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "* Existing heading\n"))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) (setq prompted t) nil)))
              (expect (org-canvas--pull-confirm-overwrite temp "sections")
                      :not :to-throw))
            (expect prompted :to-be nil))
        (delete-file temp))))

  (it "is a no-op for an empty file"
    (let ((temp (make-temp-file "empty-" nil ".org")))
      (unwind-protect
          (let ((noninteractive nil))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) (error "Should not prompt for an empty file"))))
              (expect (org-canvas--pull-confirm-overwrite temp "sections")
                      :not :to-throw)))
        (delete-file temp)))))

(describe "org-canvas--pull-insert-body file-URL rewriting"
  (it "rewrites Canvas file URLs in the converted org body"
    (let ((temp-files (make-temp-file "files-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-files
              (insert "* [[file:content/foo.pdf][foo.pdf]]
:PROPERTIES:
:CANVAS_ID: 7777
:END:
"))
            (let ((org-canvas-files-file temp-files)
                  (org-canvas--file-id-cache nil))
              (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
                        ((symbol-function 'call-process-region)
                         (lambda (_start _end _program &optional _delete buffer &rest _args)
                           (when buffer
                             (erase-buffer)
                             (insert "[[https://x.instructure.com/courses/1/files/7777/preview?verifier=z]]"))
                           0)))
                (with-temp-buffer
                  (org-mode)
                  (insert "* Heading\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
                  (goto-char (point-min))
                  (org-back-to-heading)
                  (org-canvas--pull-insert-body "<p>anything</p>")
                  (let ((body (buffer-substring-no-properties (point-min) (point-max))))
                    (expect body :to-match "\\[\\[file:content/foo.pdf\\]\\[foo.pdf\\]\\]")
                    (expect body :not :to-match "instructure.com"))))))
        (delete-file temp-files)))))

;;;; Pull-item macro :after-pull coverage

(defvar test--after-pull-cov-called nil)

(describe "org-canvas-define-pull-item :after-pull hook"
  (it "calls after-pull function with item and pos"
    (setq test--after-pull-cov-called nil)
    (eval
     '(org-canvas-define-pull-item test--after-pull-cov
        :after-pull (lambda (item _pos)
                      (setq test--after-pull-cov-called item))
        :properties
        ((some_field "SOME_FIELD" :type string)))
     t)
    (with-temp-org-buffer
     "* Test Heading
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let ((item '((some_field . "value1"))))
       (org-canvas--test--after-pull-cov-pull-item item (point))
       (expect test--after-pull-cov-called :to-equal item)))))

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
      (expect (plist-get (car records) :file) :to-equal "pages.org")
      (expect (plist-get (car records) :item)
              :to-equal "connecting-to-the-palmetto-jupyter-image")
      (expect (plist-get (car records) :error) :to-equal "Operation timeout")
      (expect (plist-get (car records) :log-line) :to-equal 154)))

  (it "preserves insertion order across multiple records"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :item "first" :error "e1")
    (org-canvas--pull-summary-record :file "b.org" :item "second" :error "e2")
    (let ((records (org-canvas--pull-summary-records)))
      (expect (length records) :to-equal 2)
      (expect (plist-get (nth 0 records) :item) :to-equal "first")
      (expect (plist-get (nth 1 records) :item) :to-equal "second")))

  (it "prints nothing when empty"
    (org-canvas--pull-summary-reset)
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-equal "")))

  (it "prints a summary block when non-empty"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record
     :file "pages.org" :item "x" :error "Operation timeout" :log-line 154)
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "Pull complete with 1 non-fatal error")
      (expect output :to-match "pages.org")
      (expect output :to-match "Operation timeout")
      (expect output :to-match "log line 154")))

  (it "pluralizes errors correctly with multiple records"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "e1")
    (org-canvas--pull-summary-record :file "b.org" :error "e2")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "2 non-fatal errors")))

  (it "omits the item suffix when item is nil"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "settings.org" :error "boom")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :not :to-match "\\[")))

  (it "omits the log line suffix when log-line is nil"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "settings.org" :error "boom")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :not :to-match "log line"))))

(describe "pull summary skip records (issue #81)"
  (it "defaults a record's kind to error"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (expect (plist-get (car (org-canvas--pull-summary-records)) :kind)
            :to-equal 'error))

  (it "separates skips from errors by kind"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (org-canvas--pull-summary-record :kind 'skip :file "pages.org"
                                     :item "home" :error "front page")
    (expect (length (org-canvas--pull-summary-records-of-kind 'error))
            :to-equal 1)
    (let ((skips (org-canvas--pull-summary-records-of-kind 'skip)))
      (expect (length skips) :to-equal 1)
      (expect (plist-get (car skips) :item) :to-equal "home")))

  (it "counts a record written without a kind as an error"
    (org-canvas--pull-summary-reset)
    (push (list :file "a.org" :error "boom") org-canvas--pull-summary)
    (expect (length (org-canvas--pull-summary-records-of-kind 'error))
            :to-equal 1)
    (expect (org-canvas--pull-summary-records-of-kind 'skip) :to-be nil))

  (it "prints skips in their own section"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :kind 'skip :file "pages.org"
                                     :item "home" :error "front page")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "1 item skipped")
      (expect output :to-match "pages.org \\[home\\]: front page")
      (expect output :not :to-match "non-fatal error")))

  (it "pluralizes and separates the two sections when both are present"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (org-canvas--pull-summary-record :kind 'skip :file "pages.org"
                                     :item "home" :error "front page")
    (org-canvas--pull-summary-record :kind 'skip :file "d.org"
                                     :item "news" :error "announcement")
    (let ((output (with-output-to-string
                    (org-canvas--pull-summary-print))))
      (expect output :to-match "1 non-fatal error")
      (expect output :to-match "2 items skipped")))

  (it "tallies errors alone"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (expect (org-canvas--pull-summary-tally) :to-equal "1 non-fatal error(s)"))

  (it "tallies skips alone"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :kind 'skip :file "a.org" :error "why")
    (expect (org-canvas--pull-summary-tally) :to-equal "1 item(s) skipped"))

  (it "tallies both kinds together"
    (org-canvas--pull-summary-reset)
    (org-canvas--pull-summary-record :file "a.org" :error "boom")
    (org-canvas--pull-summary-record :kind 'skip :file "b.org" :error "why")
    (expect (org-canvas--pull-summary-tally)
            :to-equal "1 non-fatal error(s), 1 item(s) skipped"))

  (it "tallies an empty accumulator as an empty string"
    (org-canvas--pull-summary-reset)
    (expect (org-canvas--pull-summary-tally) :to-equal "")))

(describe "pull skip helpers (issue #81)"
  (it "labels an item by its title field"
    (expect (org-canvas--pull-item-label
             '((url . "home") (title . "Home")) 'url 'title)
            :to-equal "Home"))

  (it "falls back to the id field when there is no title"
    (expect (org-canvas--pull-item-label '((url . "home")) 'url 'title)
            :to-equal "home"))

  (it "labels an item carrying neither field"
    (expect (org-canvas--pull-item-label '((foo . 1)) 'url 'title)
            :to-equal "(unnamed)"))

  (it "returns an empty suffix when nothing was skipped"
    (expect (org-canvas--pull-skip-suffix 0 "front page") :to-equal ""))

  (it "names the reason in the suffix"
    (expect (org-canvas--pull-skip-suffix 2 "front page")
            :to-equal " (2 skipped: front page)"))

  (it "omits the reason when the module declares none"
    (expect (org-canvas--pull-skip-suffix 1 nil) :to-equal " (1 skipped)"))

  (it "logs and records a skipped item"
    (org-canvas--pull-summary-reset)
    (let ((logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
        (org-canvas--pull-record-skip
         "/tmp/course/pages.org" '((url . "home") (title . "Home"))
         'url 'title "front page"))
      (expect (car logged) :to-equal "[Pull] Skipped 'Home': front page")
      (let ((rec (car (org-canvas--pull-summary-records-of-kind 'skip))))
        (expect (plist-get rec :file) :to-equal "pages.org")
        (expect (plist-get rec :item) :to-equal "Home")
        (expect (plist-get rec :error) :to-equal "front page"))))

  (it "records a reasonless skip with a generic explanation"
    (org-canvas--pull-summary-reset)
    (cl-letf (((symbol-function 'org-canvas--log-info) #'ignore))
      (org-canvas--pull-record-skip
       "/tmp/course/pages.org" '((url . "home")) 'url 'title nil))
    (expect (plist-get (car (org-canvas--pull-summary-records-of-kind 'skip))
                       :error)
            :to-match "skip rule")))

(describe "empty file pull header"
  (it "writes #+TITLE, #+LAST_SYNCED, and 0-items comment"
    (let ((temp (make-temp-file "empty-test-" nil ".org")))
      (unwind-protect
          (progn
            (org-canvas--pull-emit-empty-file temp "Discussions")
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((s (buffer-string)))
                (expect s :to-match "^#\\+TITLE: Discussions$")
                (expect s :to-match "^#\\+LAST_SYNCED: \\[")
                (expect s :to-match "^# Canvas returned 0 items at this sync\\.$"))))
        (delete-file temp))))

  (it "overwrites existing content"
    (let ((temp (make-temp-file "empty-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp (insert "stale content here\n* Old heading\n"))
            (org-canvas--pull-emit-empty-file temp "Calendar")
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :not :to-match "stale content")
              (expect (buffer-string) :to-match "Canvas returned 0 items")))
        (delete-file temp)))))

(describe "org-canvas--pull-known-ids"
  (it "returns the Canvas ids the file already claims"
    (let ((temp (make-temp-file "known-ids-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"
                      "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 62\n:END:\n"
                      "* Draft\n"))
            (expect (sort (org-canvas--pull-known-ids temp "CANVAS_ID") #'string<)
                    :to-equal '("61" "62")))
        (let ((buf (find-buffer-visiting temp)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))
        (delete-file temp))))

  (it "returns nothing for a file that does not exist yet"
    (expect (org-canvas--pull-known-ids "/tmp/no-such-pull-file.org" "CANVAS_ID")
            :to-be nil)))

(describe "org-canvas--pull-item-managed-p"
  (it "recognizes an item the file already claims, comparing as strings"
    (expect (org-canvas--pull-item-managed-p '((id . 61)) 'id '("61" "62"))
            :to-be t))

  (it "rejects an item no heading claims"
    (expect (org-canvas--pull-item-managed-p '((id . 99)) 'id '("61" "62"))
            :to-be nil))

  (it "rejects an item with no id at all"
    (expect (org-canvas--pull-item-managed-p '((title . "x")) 'id '("61"))
            :to-be nil)))

;;;; Registry-driven pull (issue #135)

(describe "org-canvas--registry-remote-field"
  (it "prefers :api-key over :data-key"
    (expect (org-canvas--registry-remote-field
             '(:org-prop "X" :data-key :other :api-key "wanted" :type string)
             '((wanted . "yes") (other . "no")))
            :to-equal "yes"))

  (it "falls back to the :data-key name"
    (expect (org-canvas--registry-remote-field
             '(:org-prop "X" :data-key :points_possible :type number)
             '((points_possible . 10)))
            :to-equal 10))

  (it "calls a :remote-fn with the item"
    (expect (org-canvas--registry-remote-field
             (list :org-prop "X" :data-key :nested :type (quote string)
                   :remote-fn (lambda (item)
                                (alist-get 'deep (alist-get 'outer item))))
             '((outer . ((deep . "found")))))
            :to-equal "found")))

(describe "org-canvas--registry-remote-present-p"
  (it "is true for a flat key the item carries, even when null"
    (expect (org-canvas--registry-remote-present-p
             '(:data-key :due_at) '((due_at . :null)))
            :to-be t))

  (it "is nil for a flat key the item lacks"
    (expect (org-canvas--registry-remote-present-p
             '(:data-key :due_at) '((id . 1)))
            :to-be nil))

  (it "trusts a :remote-fn to answer for its field"
    (expect (org-canvas--registry-remote-present-p
             (list :data-key :x :remote-fn #'ignore) '((id . 1)))
            :to-be t)))

(describe "org-canvas--registry-remote-as-org"
  (it "spells booleans true and false, folding :json-false"
    (expect (org-canvas--registry-remote-as-org '(:type boolean) t)
            :to-equal "true")
    (expect (org-canvas--registry-remote-as-org '(:type boolean) :json-false)
            :to-equal "false")
    (expect (org-canvas--registry-remote-as-org '(:type boolean) nil)
            :to-equal "false"))

  (it "converts a timestamp to an Org timestamp and rejects garbage"
    (let ((org-canvas--pull-tz-cache nil))
      (expect (org-canvas--registry-remote-as-org
               '(:type timestamp) "2026-06-15T09:00:00Z")
              :to-match "<2026-06-15 [A-Za-z]+ 09:00>")
      (expect (org-canvas--registry-remote-as-org '(:type timestamp) "soon")
              :to-be nil)
      (expect (org-canvas--registry-remote-as-org '(:type timestamp) :null)
              :to-be nil)))

  (it "joins a csv-enum from an array or a comma string"
    (expect (org-canvas--registry-remote-as-org
             '(:type csv-enum) ["online_upload" "online_url"])
            :to-equal "online_upload,online_url")
    (expect (org-canvas--registry-remote-as-org
             '(:type csv-enum) "teachers, students")
            :to-equal "teachers,students")
    (expect (org-canvas--registry-remote-as-org '(:type csv-enum) [])
            :to-be nil))

  (it "formats numbers and strings, treating the empty string as unset"
    (expect (org-canvas--registry-remote-as-org '(:type number) 12.5)
            :to-equal "12.5")
    (expect (org-canvas--registry-remote-as-org '(:type enum) "points")
            :to-equal "points")
    (expect (org-canvas--registry-remote-as-org '(:type string) "")
            :to-be nil)
    (expect (org-canvas--registry-remote-as-org '(:type string) :null)
            :to-be nil)))

(describe "org-canvas--registry-value-default-p"
  (it "compares a boolean against its registered default"
    (expect (org-canvas--registry-value-default-p '(:type boolean :default t) t)
            :to-be t)
    (expect (org-canvas--registry-value-default-p
             '(:type boolean :default t) :json-false)
            :to-be nil)
    (expect (org-canvas--registry-value-default-p '(:type boolean) :json-false)
            :to-be t)
    (expect (org-canvas--registry-value-default-p '(:type boolean) t)
            :to-be nil))

  (it "treats zero and null as a number's default"
    (expect (org-canvas--registry-value-default-p '(:type number) 0) :to-be t)
    (expect (org-canvas--registry-value-default-p '(:type number) :null)
            :to-be t)
    (expect (org-canvas--registry-value-default-p '(:type number) 3)
            :to-be nil))

  (it "treats nothing at all as the default for the other types"
    (expect (org-canvas--registry-value-default-p '(:type string) "")
            :to-be t)
    (expect (org-canvas--registry-value-default-p '(:type csv-enum) [])
            :to-be t)
    (expect (org-canvas--registry-value-default-p
             '(:type timestamp) "2026-01-01T00:00:00Z")
            :to-be nil)
    (expect (org-canvas--registry-value-default-p
             '(:type enum :default "points") "points")
            :to-be t)))

(describe "org-canvas--pull-apply-spec"
  (it "leaves a property alone when the item does not carry its field"
    (with-temp-org-buffer
     "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:DUE_AT: <2026-01-01 Thu 09:00>\n:END:\n"
     (org-back-to-heading)
     (org-canvas--pull-apply-spec
      '(:org-prop "DUE_AT" :data-key :due_at :type timestamp)
      '((id . 1)) (point))
     (expect (org-entry-get (point) "DUE_AT")
             :to-equal "<2026-01-01 Thu 09:00>")))

  (it "writes a non-default value"
    (with-temp-org-buffer
     "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     (org-back-to-heading)
     (org-canvas--pull-apply-spec
      '(:org-prop "POINTS" :data-key :points_possible :type number)
      '((points_possible . 10)) (point))
     (expect (org-entry-get (point) "POINTS") :to-equal "10")))

  (it "deletes a property whose remote value is the default"
    (with-temp-org-buffer
     "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:PUBLISHED: false\n:END:\n"
     (org-back-to-heading)
     (org-canvas--pull-apply-spec
      '(:org-prop "PUBLISHED" :data-key :published :type boolean :default t)
      '((published . t)) (point))
     (expect (org-entry-get (point) "PUBLISHED") :to-be nil)))

  (it "writes the default when org-canvas-emit-defaults is set"
    (with-temp-org-buffer
     "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     (org-back-to-heading)
     (let ((org-canvas-emit-defaults t))
       (org-canvas--pull-apply-spec
        '(:org-prop "PUBLISHED" :data-key :published :type boolean :default t)
        '((published . t)) (point)))
     (expect (org-entry-get (point) "PUBLISHED") :to-equal "true")))

  (it "writes false over a stale true when the default is true (issue #134)"
    (with-temp-org-buffer
     "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:PUBLISHED: true\n:END:\n"
     (org-back-to-heading)
     (org-canvas--pull-apply-spec
      '(:org-prop "PUBLISHED" :data-key :published :type boolean :default t)
      '((published . :json-false)) (point))
     (expect (org-entry-get (point) "PUBLISHED") :to-equal "false")))

  (it "skips a :local-only spec entirely"
    (with-temp-org-buffer
     "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:PUBLISH_AT: <2026-09-01 Tue 08:00>\n:END:\n"
     (org-back-to-heading)
     (org-canvas--pull-apply-spec
      '(:org-prop "PUBLISH_AT" :data-key :publish_at :type timestamp
        :local-only t)
      '((publish_at . "2030-01-01T00:00:00Z")) (point))
     (expect (org-entry-get (point) "PUBLISH_AT")
             :to-equal "<2026-09-01 Tue 08:00>")))

  (it "leaves a value it cannot spell unchanged and says so"
    (with-temp-org-buffer
     "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n"
     (org-back-to-heading)
     (let ((warnings nil))
       (cl-letf (((symbol-function 'org-canvas--log-warning)
                  (lambda (_logger fmt &rest args)
                    (push (apply #'format fmt args) warnings))))
         (let ((org-canvas-assignment-groups-file
                "/nonexistent/assignment-groups.org"))
           (org-canvas--pull-apply-spec
            '(:org-prop "GROUP" :data-key :assignment_group_id :type link
              :target-file org-canvas-assignment-groups-file)
            '((assignment_group_id . 42)) (point))))
       (expect (org-entry-get (point) "GROUP")
               :to-equal "[[file:assignment-groups.org::*Homework][Homework]]")
       (expect (car warnings) :to-match "GROUP")))))

(describe "org-canvas--pull-resolve-link"
  (it "links to the heading the target file keeps for the id, and tracks edits"
    (let ((target (make-temp-file "org-canvas-groups-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target
              (insert "* Homework\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let* ((org-canvas-assignment-groups-file target)
                   (spec '(:type link
                           :target-file org-canvas-assignment-groups-file))
                   (base (file-name-nondirectory target)))
              (expect (org-canvas--pull-resolve-link spec 42)
                      :to-equal (format "[[file:%s::*Homework][Homework]]" base))
              (expect (org-canvas--pull-resolve-link spec 99) :to-be nil)
              (expect (org-canvas--pull-resolve-link spec nil) :to-be nil)
              ;; An edit to the target file invalidates the cached index.
              (with-current-buffer (org-canvas--find-file-noselect target)
                (goto-char (point-max))
                (insert "* Labs\n:PROPERTIES:\n:CANVAS_ID: 99\n:END:\n"))
              (expect (org-canvas--pull-resolve-link spec 99)
                      :to-equal (format "[[file:%s::*Labs][Labs]]" base))))
        (let ((buf (find-buffer-visiting target)))
          (when buf
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (delete-file target)))))

(describe "org-canvas--pull-item-from-registry"
  (it "signals on an unknown registry key"
    (expect (org-canvas--pull-item-from-registry "no-such-feature" nil 1)
            :to-throw 'error))

  (it "writes every registered property from the item"
    (let ((org-canvas--property-registry (make-hash-table :test 'equal)))
      (puthash "widgets"
               '(:properties
                 ((:org-prop "PUBLISHED" :data-key :published :type boolean
                   :default t)
                  (:org-prop "POINTS" :data-key :points_possible :type number)
                  (:org-prop "DUE_AT" :data-key :due_at :type timestamp)))
               org-canvas--property-registry)
      (with-temp-org-buffer
       "* T\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
       (org-back-to-heading)
       (let ((org-canvas--pull-tz-cache nil))
         (org-canvas--pull-item-from-registry
          "widgets"
          '((published . :json-false) (points_possible . 5)
            (due_at . "2026-06-15T09:00:00Z"))
          (point)))
       (expect (org-entry-get (point) "PUBLISHED") :to-equal "false")
       (expect (org-entry-get (point) "POINTS") :to-equal "5")
       (expect (org-entry-get (point) "DUE_AT") :to-match "<2026-06-15")))))

(describe "org-canvas-define-pull-item :registry-key"
  (it "expands to a registry-driven pull followed by the body and after-pull"
    (let ((form (format "%S" (macroexpand
                              '(org-canvas-define-pull-item test--rk
                                 :registry-key "widgets"
                                 :body-field message
                                 :after-pull #'ignore)))))
      (expect form :to-match "org-canvas--pull-item-from-registry \"widgets\" item pos")
      (expect form :to-match "org-canvas--pull-insert-body")
      (expect form :to-match "funcall"))))

;;;; The pull summary is a sink users share (issue #154)

(describe "org-canvas--pull-summary-record redaction"
  (before-each (org-canvas--pull-summary-reset))
  (after-each (org-canvas--pull-summary-reset))

  (it "masks a credential in the error text it stores"
    ;; The summary is rendered into a buffer the user is invited to read
    ;; and share, and its :error is raw error-message-string text.
    (org-canvas--pull-summary-record
     :file "settings.org" :item "late policy"
     :error "not pulled: set-cookie: canvas_session=HIJACKME; secure")
    (let ((rec (car (org-canvas--pull-summary-records))))
      (expect (plist-get rec :error) :to-match "canvas_session=\\*\\*\\*MASKED\\*\\*\\*")
      (expect (plist-get rec :error) :not :to-match "HIJACKME")
      (expect (plist-get rec :item) :to-equal "late policy")))

  (it "keeps the printed summary clean"
    (org-canvas--pull-summary-record
     :file "settings.org" :item "tabs"
     :error "Bearer 7~SECRETTOKEN")
    (expect (org-canvas--pull-summary-format-record
             (car (org-canvas--pull-summary-records)))
            :not :to-match "SECRETTOKEN"))

  (it "records a nil error without failing"
    (org-canvas--pull-summary-record :file "pages.org" :kind 'skip)
    (expect (plist-get (car (org-canvas--pull-summary-records)) :error)
            :to-be nil)))

(describe "org-canvas--pull-check-entry-count"
  (it "warns and records when an id-less level-1 entry appeared"
    (let ((warnings nil))
      (org-canvas--pull-summary-reset)
      (unwind-protect
          (progn
            (with-temp-org-buffer
             "* Real
:PROPERTIES:
:CANVAS_ID: 7
:END:
* Phantom
lost text
"
             (cl-letf (((symbol-function 'org-canvas--log-warning)
                        (lambda (_logger fmt &rest args)
                          (push (apply #'format fmt args) warnings))))
               (org-canvas--pull-check-entry-count
                "quizzes" "/x/quizzes.org" "CANVAS_ID" 0 1)))
            (expect (length warnings) :to-equal 1)
            (expect (car warnings) :to-match
                    (concat "quizzes.org: 1 level-1 entry without CANVAS_ID"
                            " appeared while writing 1 quizzes"))
            (let ((recs (org-canvas--pull-summary-records-of-kind 'error)))
              (expect (length recs) :to-equal 1)
              (expect (plist-get (car recs) :file) :to-equal "quizzes.org")
              (expect (plist-get (car recs) :error) :to-match "split an entry")))
        (org-canvas--pull-summary-reset))))

  (it "says nothing when the id-less entries were there before"
    (let ((warnings nil))
      (org-canvas--pull-summary-reset)
      (unwind-protect
          (progn
            (with-temp-org-buffer
             "* Draft
text
* Real
:PROPERTIES:
:CANVAS_ID: 7
:END:
"
             (cl-letf (((symbol-function 'org-canvas--log-warning)
                        (lambda (_logger fmt &rest args)
                          (push (apply #'format fmt args) warnings))))
               (org-canvas--pull-check-entry-count
                "pages" "/x/pages.org" "CANVAS_ID" 1 1)))
            (expect warnings :to-equal nil)
            (expect (org-canvas--pull-summary-empty-p) :to-be-truthy))
        (org-canvas--pull-summary-reset)))))

;;;; Branches of the pull helpers no module reaches

(describe "org-canvas--pull-set-boolean-property with string values"
  (it "writes true for the string \"true\""
    (with-temp-org-buffer "* Item\n"
      (org-back-to-heading)
      (org-canvas--pull-set-boolean-property (point) "COVERAGE_FLAG" "true")
      (expect (org-entry-get (point) "COVERAGE_FLAG") :to-equal "true")))

  (it "writes false for the string \"false\""
    (let ((org-canvas-emit-defaults t))
      (with-temp-org-buffer "* Item\n"
        (org-back-to-heading)
        (org-canvas--pull-set-boolean-property (point) "COVERAGE_FLAG" "false")
        (expect (org-entry-get (point) "COVERAGE_FLAG") :to-equal "false"))))

  (it "treats any other string as true"
    (with-temp-org-buffer "* Item\n"
      (org-back-to-heading)
      (org-canvas--pull-set-boolean-property (point) "COVERAGE_FLAG" "yes")
      (expect (org-entry-get (point) "COVERAGE_FLAG") :to-equal "true")))

  (it "treats a non-string, non-boolean value as true"
    (with-temp-org-buffer "* Item\n"
      (org-back-to-heading)
      (org-canvas--pull-set-boolean-property (point) "COVERAGE_FLAG" 1)
      (expect (org-entry-get (point) "COVERAGE_FLAG") :to-equal "true"))))

(describe "org-canvas--pull-label-for"
  (it "returns the registered label"
    (expect (org-canvas--pull-label-for "pages") :to-equal "Pages"))

  (it "capitalizes an unregistered feature name"
    (expect (org-canvas--pull-label-for "study-guides") :to-equal "Study Guides")))

(describe "org-canvas--strip-course-files-prefix"
  (it "returns a name without the course files prefix unchanged"
    (expect (org-canvas--strip-course-files-prefix "Uploaded Media")
            :to-equal "Uploaded Media"))

  (it "strips the prefix and empties the root"
    (expect (org-canvas--strip-course-files-prefix "course files/Week 1")
            :to-equal "Week 1")
    (expect (org-canvas--strip-course-files-prefix "course files") :to-equal "")
    (expect (org-canvas--strip-course-files-prefix nil) :to-equal "")))

(describe "org-canvas--rewrite-fetch-folder-relpath cache"
  (it "answers from the cache without a request"
    (let ((org-canvas--rewrite-folder-cache (make-hash-table :test 'eql))
          (called nil))
      (puthash 12 "Week 1" org-canvas--rewrite-folder-cache)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (setq called t) nil)))
        (expect (org-canvas--rewrite-fetch-folder-relpath 12) :to-equal "Week 1"))
      (expect called :to-be nil))))

(describe "org-canvas--files-org-append-fetched-entry"
  (defmacro test-org-canvas--with-files-org (initial &rest body)
    "Run BODY with `org-canvas-files-file' holding INITIAL (nil: absent)."
    (declare (indent 1))
    `(let* ((dir (make-temp-file "files-append-" t))
            (org-canvas-files-file (expand-file-name "files.org" dir)))
       (unwind-protect
           (progn
             (when ,initial
               (with-temp-file org-canvas-files-file (insert ,initial)))
             ,@body)
         (let ((buf (find-buffer-visiting org-canvas-files-file)))
           (when buf
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf)))
         (delete-directory dir t))))

  (defun test-org-canvas--files-org-text ()
    "Return the text of `org-canvas-files-file' as saved on disk."
    (with-temp-buffer
      (insert-file-contents org-canvas-files-file)
      (buffer-string)))

  (it "signals when no files file is configured"
    (let ((org-canvas-files-file nil))
      (expect (org-canvas--files-org-append-fetched-entry "a.png" "a.png" 1 nil nil)
              :to-throw 'error)))

  (it "creates the file and the parent heading, stamping a string id"
    (test-org-canvas--with-files-org nil
      (org-canvas--files-org-append-fetched-entry
       "Uploaded Media/shot.png" "shot.png" "abc-1" "image/png" 10)
      (let ((text (test-org-canvas--files-org-text)))
        (expect text :to-match "^\\* Uploaded Media\n")
        (expect text :to-match
                "^\\*\\* \\[\\[file:content/Uploaded Media/shot\\.png\\]\\[shot\\.png\\]\\]$")
        (expect text :to-match ":CANVAS_ID: +abc-1")
        (expect text :to-match ":CONTENT_TYPE: +image/png")
        (expect text :to-match ":SIZE: +10"))))

  (it "starts a new parent heading on its own line"
    (test-org-canvas--with-files-org "* Other\nsome text"
      (org-canvas--files-org-append-fetched-entry "shot.png" "shot.png" 7 nil nil)
      (expect (test-org-canvas--files-org-text)
              :to-match "some text\n\\* Uploaded Media\n\\*\\* \\[\\[file:content/shot\\.png\\]")))

  (it "starts the child on its own line after a parent body"
    (test-org-canvas--with-files-org "* Uploaded Media\nnotes"
      (org-canvas--files-org-append-fetched-entry "shot.png" "shot.png" 7 nil nil)
      (expect (test-org-canvas--files-org-text)
              :to-match "notes\n\\*\\* \\[\\[file:content/shot\\.png\\]"))))

(describe "org-canvas--rewrite-fetch-unknown-file at the course root"
  (it "places a root file directly under content/"
    (with-org-canvas-test-config
      (let ((cache (make-hash-table :test 'equal))
            (org-canvas-directory (make-temp-file "rewrite-root-" t))
            (org-canvas--rewrite-folder-cache (make-hash-table :test 'eql))
            (appended nil)
            (downloaded nil))
        (unwind-protect
            (progn
              (puthash 3 "" org-canvas--rewrite-folder-cache)
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (&rest _)
                           '((id . 55) (display_name . "root.pdf") (folder_id . 3)
                             (url . "https://example.com/f")
                             (content-type . "application/pdf") (size . 9))))
                        ((symbol-function 'org-canvas--file-pull-download)
                         (lambda (&rest args) (setq downloaded args)))
                        ((symbol-function 'org-canvas--files-org-append-fetched-entry)
                         (lambda (&rest args) (setq appended args))))
                (expect (org-canvas--rewrite-fetch-unknown-file 55 cache)
                        :to-equal "content/root.pdf"))
              (expect (car appended) :to-equal "root.pdf")
              (expect (nth 2 downloaded)
                      :to-equal (expand-file-name "content/root.pdf" org-canvas-directory))
              (expect (gethash 55 cache) :to-equal "content/root.pdf"))
          (delete-directory org-canvas-directory t))))))

(describe "org-canvas--pull-item-set-property"
  (it "converts each declared type"
    (with-temp-org-buffer "* Item\n"
      (org-back-to-heading)
      (let ((pos (point))
            (item '((flag . t) (when . "2026-03-01T12:00:00Z") (count . 4)
                    (zero . 0) (label . "x") (missing . :null))))
        (org-canvas--pull-item-set-property pos 'flag "COV_FLAG" 'boolean item)
        (org-canvas--pull-item-set-property pos 'when "COV_WHEN" 'timestamp item)
        (org-canvas--pull-item-set-property pos 'count "COV_COUNT" 'number item)
        (org-canvas--pull-item-set-property pos 'zero "COV_ZERO" 'number item)
        (org-canvas--pull-item-set-property pos 'label "COV_LABEL" 'non-null item)
        (org-canvas--pull-item-set-property pos 'missing "COV_MISSING" 'non-null item)
        (org-canvas--pull-item-set-property pos 'label "COV_STRING" 'string item)
        (expect (org-entry-get pos "COV_FLAG") :to-equal "true")
        (expect (org-entry-get pos "COV_WHEN") :to-match "2026-03-0[12]")
        (expect (org-entry-get pos "COV_COUNT") :to-equal "4")
        (expect (org-entry-get pos "COV_ZERO") :to-be nil)
        (expect (org-entry-get pos "COV_LABEL") :to-equal "x")
        (expect (org-entry-get pos "COV_MISSING") :to-be nil)
        (expect (org-entry-get pos "COV_STRING") :to-equal "x")))))

(describe "org-canvas--pull-sort-items ties"
  (it "keeps input order for items equal on every key"
    (let ((items '(((id . 1) (name . "Same") (position . 1) (tag . "first"))
                   ((id . 1) (name . "Same") (position . 1) (tag . "second")))))
      (expect (mapcar (lambda (x) (alist-get 'tag x))
                      (org-canvas--pull-sort-items items))
              :to-equal '("first" "second")))))

(describe "a generated pull with a prefix argument"
  ;; Issue #67: pulling a whole endpoint to reconcile two items writes a
  ;; heading for every item the course holds, managed or not.
  (it "refreshes only the items the file already claims"
    (let ((temp (make-temp-file "managed-pull-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp
              (insert "* Old title\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                       (lambda (&rest _)
                         '(((id . 100) (title . "Mine, renamed")
                            (message . "<p>x</p>"))
                           ((id . 999) (title . "Someone else's")
                            (message . "<p>y</p>"))))))
              (let ((org-canvas-announcements-file temp))
                (org-canvas-pull-announcements t)))
            (with-temp-buffer
              (insert-file-contents temp)
              (let ((s (buffer-string)))
                (expect s :to-match "Mine, renamed")
                (expect s :not :to-match "Someone else's"))))
        (let ((buf (find-buffer-visiting temp)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))
        (delete-file temp))))

  (it "imports everything when called without the prefix argument"
    (let ((temp (make-temp-file "unmanaged-pull-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp
              (insert "* Old title\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                       (lambda (&rest _)
                         '(((id . 100) (title . "Mine, renamed")
                            (message . "<p>x</p>"))
                           ((id . 999) (title . "Someone else's")
                            (message . "<p>y</p>"))))))
              (let ((org-canvas-announcements-file temp))
                (org-canvas-pull-announcements)))
            (with-temp-buffer
              (insert-file-contents temp)
              (expect (buffer-string) :to-match "Someone else's")))
        (let ((buf (find-buffer-visiting temp)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))
        (delete-file temp)))))

(provide 'org-canvas-core-pull-test)
;;; org-canvas-core-pull-test.el ends here
