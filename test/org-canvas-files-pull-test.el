;;; org-canvas-files-pull-test.el --- Buttercup tests for the files pull  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-files-pull', which rebuilds files.org from
;; Canvas: the three layouts it writes, the staleness check that keeps a
;; re-pull from downloading a course again (issue #158), the forced
;; download a conflict's resolution makes, and the properties it reads
;; back.  The base file's specs are in org-canvas-files-test.el, the
;; upload's in org-canvas-files-upload-test.el; the three were one file
;; until #593.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-files)
(require 'org-canvas-files-pull)

;;;; Pull Function Tests

(describe "org-canvas--file-pull-download"
  (before-each (test-org-canvas-reset-file-caches))

  (it "downloads when file is absent"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "test.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url path &rest _args)
                       (setq downloaded path))))
            (org-canvas--file-pull-download "test.pdf" "https://example.com/test.pdf" local-path 1024)
            (expect downloaded :to-equal local-path))
        (delete-directory temp-dir t))))

  (it "skips when the local copy is already the right size (issue #158)"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "existing.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (progn
            (with-temp-file local-path (insert "existing"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args)
                         (setq downloaded t))))
              (org-canvas--file-pull-download
               "existing.pdf" "https://example.com/f" local-path
               (file-attribute-size (file-attributes local-path)))
              (expect downloaded :to-be nil)))
        (delete-directory temp-dir t))))

  (it "skips when the size is unknown and something is already there"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "existing.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (progn
            (with-temp-file local-path (insert "existing"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) (setq downloaded t))))
              (org-canvas--file-pull-download
               "existing.pdf" "https://example.com/f" local-path nil)
              (expect downloaded :to-be nil)))
        (delete-directory temp-dir t))))

  (it "fetches again when the local copy is a different length (issue #158)"
    ;; A refresh that skipped every file that merely existed left a
    ;; Canvas-side edit invisible for ever.
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "stale.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (progn
            (with-temp-file local-path (insert "short"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (_url _path &rest _args) (setq downloaded t))))
              (org-canvas--file-pull-download
               "stale.pdf" "https://example.com/f" local-path 999)
              (expect downloaded :to-be t)))
        (delete-directory temp-dir t))))

  (it "handles download errors gracefully"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "fail.pdf" temp-dir)))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url _path &rest _args)
                       (signal 'error '("Network error")))))
            ;; Should not throw
            (org-canvas--file-pull-download "fail.pdf" "https://example.com/f" local-path nil))
        (delete-directory temp-dir t))))

  (it "skips when download-url is nil"
    (let* ((temp-dir (make-temp-file "file-pull-test" t))
           (local-path (expand-file-name "nourl.pdf" temp-dir))
           (downloaded nil))
      (unwind-protect
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url _path &rest _args) (setq downloaded t))))
            (org-canvas--file-pull-download "nourl.pdf" nil local-path 100)
            (expect downloaded :to-be nil))
        (delete-directory temp-dir t))))

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
        (delete-directory temp-dir t)))))

(describe "org-canvas-pull-files"
  (before-each (test-org-canvas-reset-file-caches))

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
                    (expect (buffer-string) :to-match "CANVAS_ID: +1")
                    (expect (buffer-string) :to-match "CONTENT_TYPE: +application/pdf")
                    (expect (buffer-string) :to-match "SIZE: +2048"))))))
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

  (it "invalidates org-canvas--file-id-cache after rewriting files.org"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file)
                (org-canvas--file-id-cache
                 (let ((h (make-hash-table :test 'equal)))
                   (puthash "stale" "stale/path.pdf" h)
                   h)))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages '() '()))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (expect org-canvas--file-id-cache :to-be nil)))))
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
                      (expect body :to-match "CANVAS_ID: +1")))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (defun test-files-158--headings (body regexp)
    "Return the heading lines in BODY whose text matches REGEXP."
    (seq-filter (lambda (line)
                  (and (string-match-p "\\`\\*+ " line)
                       (string-match-p regexp line)))
                (split-string body "\n")))

  (defun test-files-158--repull (initial folders remote check)
    "Re-pull INITIAL files.org against FOLDERS and REMOTE, then CHECK the body."
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert initial))
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (test-files--mock-pages folders remote))
                          ((symbol-function 'url-copy-file)
                           (lambda (_url _path &rest _args) nil)))
                  (org-canvas-pull-files)
                  (with-current-buffer (find-file-noselect files-file)
                    (funcall check (buffer-string)))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "re-pulls a hierarchical files.org in place (issue #158)"
    ;; It used to refuse, so refreshing files meant deleting the file
    ;; holding every CANVAS_ID mapping and re-downloading the course.
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     '(((id . 100) (full_name . "course files/Labs")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 100)
        (url . "https://example.com/a.pdf") (size . 10)))
     (lambda (body)
       (expect body :to-match "^\\* Labs$")
       ;; One heading for the file, not a second copy of it.
       (expect (length (test-files-158--headings body "a\\.pdf")) :to-equal 1)
       (expect body :to-match "CANVAS_ID: +1"))))

  (it "adds a new file under its folder, creating the folder heading (issue #158)"
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     '(((id . 100) (full_name . "course files/Labs"))
       ((id . 200) (full_name . "course files/Lectures/Week 1")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 100)
        (url . "https://example.com/a.pdf") (size . 10))
       ((id . 2) (display_name . "slides.pdf") (folder_id . 200)
        (url . "https://example.com/slides.pdf") (size . 20)))
     (lambda (body)
       ;; The whole folder path is built, not just the leaf.
       (expect body :to-match "^\\* Lectures$")
       (expect body :to-match "^\\*\\* Week 1$")
       (expect body :to-match "^\\*\\*\\* \\[\\[file:content/Lectures/Week 1/slides\\.pdf\\]")
       (expect body :to-match "CANVAS_ID: +2")
       ;; The file that was already there is untouched.
       (expect (length (test-files-158--headings body "a\\.pdf")) :to-equal 1))))

  (it "files a new file under a folder heading that already exists (issue #158)"
    ;; The folder is reused, not written a second time beside itself.
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     '(((id . 100) (full_name . "course files/Labs")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 100)
        (url . "https://example.com/a.pdf") (size . 10))
       ((id . 2) (display_name . "b.pdf") (folder_id . 100)
        (url . "https://example.com/b.pdf") (size . 20)))
     (lambda (body)
       (expect (length (test-files-158--headings body "^\\* Labs$")) :to-equal 1)
       (expect (length (test-files-158--headings body "b\\.pdf")) :to-equal 1)
       (expect body :to-match "^\\*\\* \\[\\[file:content/Labs/b\\.pdf\\]"))))

  (it "reuses a deep folder path that already exists (issue #158)"
    (test-files-158--repull
     "#+TITLE: Files\n* Lectures\n** Week 1\n*** [[file:content/Lectures/Week 1/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     '(((id . 200) (full_name . "course files/Lectures/Week 1")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 200)
        (url . "https://example.com/a.pdf") (size . 10))
       ((id . 2) (display_name . "b.pdf") (folder_id . 200)
        (url . "https://example.com/b.pdf") (size . 20)))
     (lambda (body)
       (expect (length (test-files-158--headings body "^\\* Lectures$")) :to-equal 1)
       (expect (length (test-files-158--headings body "^\\*\\* Week 1$")) :to-equal 1)
       (expect body :to-match "^\\*\\*\\* \\[\\[file:content/Lectures/Week 1/b\\.pdf\\]"))))

  (it "appends a root-level file to a buffer with no trailing newline (issue #158)"
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:"
     '(((id . 100) (full_name . "course files/Labs")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 100)
        (url . "https://example.com/a.pdf") (size . 10))
       ((id . 8) (display_name . "syllabus.pdf") (folder_id . nil)
        (url . "https://example.com/s.pdf") (size . 5)))
     (lambda (body)
       (expect body :to-match "^\\* \\[\\[file:content/syllabus\\.pdf\\]")
       (expect body :not :to-match ":END:\\*")))) 

  (it "appends cleanly to a files.org with no trailing newline (issue #158)"
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:"
     '(((id . 100) (full_name . "course files/Labs"))
       ((id . 300) (full_name . "course files/Archive")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 100)
        (url . "https://example.com/a.pdf") (size . 10))
       ((id . 2) (display_name . "c.pdf") (folder_id . 300)
        (url . "https://example.com/c.pdf") (size . 30)))
     (lambda (body)
       (expect body :to-match "^\\* Archive$")
       (expect body :to-match "^\\*\\* \\[\\[file:content/Archive/c\\.pdf\\]")
       ;; No heading ran into the line above it.
       (expect body :not :to-match ":END:\\*"))))

  (it "moves a file whose folder changed, rather than duplicating it (issue #158)"
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     '(((id . 100) (full_name . "course files/Labs"))
       ((id . 300) (full_name . "course files/Archive")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 300)
        (url . "https://example.com/a.pdf") (size . 10)))
     (lambda (body)
       (expect (length (test-files-158--headings body "a\\.pdf")) :to-equal 1)
       (expect body :to-match "^\\* Archive$")
       (expect body :to-match "\\[\\[file:content/Archive/a\\.pdf\\]")
       (expect body :not :to-match "content/Labs/a\\.pdf"))))

  (it "keeps a file at the course root at level 1 (issue #158)"
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n"
     '(((id . 100) (full_name . "course files/Labs")))
     '(((id . 7) (display_name . "syllabus.pdf") (folder_id . nil)
        (url . "https://example.com/s.pdf") (size . 5)))
     (lambda (body)
       (expect body :to-match "^\\* \\[\\[file:content/syllabus\\.pdf\\]"))))

  (it "leaves hand-written content and files Canvas no longer lists alone (issue #158)"
    ;; Deletion stays with org-canvas-cleanup-orphans, and the reason the
    ;; old advice was bad is that it threw away notes like this one.
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\nA note I wrote by hand.\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** [[file:content/Labs/gone.pdf][gone.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 9\n:END:\n"
     '(((id . 100) (full_name . "course files/Labs")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 100)
        (url . "https://example.com/a.pdf") (size . 10)))
     (lambda (body)
       (expect body :to-match "A note I wrote by hand\\.")
       (expect body :to-match "CANVAS_ID: +9"))))

  (it "finds a file the operator moved by hand, and files it where Canvas says (issue #158)"
    ;; Matching is by Canvas id wherever the heading sits, so a hand-moved
    ;; file is found rather than created a second time.  Canvas is the
    ;; source of truth for which folder it belongs in, so it is filed
    ;; there.
    (test-files-158--repull
     "#+TITLE: Files\n* Mine\n** [[file:content/Labs/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     '(((id . 100) (full_name . "course files/Labs")))
     '(((id . 1) (display_name . "a.pdf") (folder_id . 100)
        (url . "https://example.com/a.pdf") (size . 10)))
     (lambda (body)
       (expect (length (test-files-158--headings body "a\\.pdf")) :to-equal 1)
       ;; Canvas says Labs, the heading sits under Mine: it is moved to
       ;; the folder Canvas names, which is the one source of truth here.
       (expect body :to-match "^\\* Labs$"))))

  (it "renames a heading when Canvas renamed the file (issue #158)"
    (test-files-158--repull
     "#+TITLE: Files\n* Labs\n** [[file:content/Labs/old.pdf][old.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     '(((id . 100) (full_name . "course files/Labs")))
     '(((id . 1) (display_name . "new.pdf") (folder_id . 100)
        (url . "https://example.com/new.pdf") (size . 10)))
     (lambda (body)
       (expect body :to-match "new\\.pdf")
       (expect body :not :to-match "old\\.pdf"))))

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
                    (expect (buffer-string) :to-match "CANVAS_ID: +7"))))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Properties, layout and progress
;;
;; What a pull writes back: the visibility and usage-rights
;; properties, the three layouts (fresh tree, flat, hierarchical),
;; duplicate detection, and the progress it reports.

(describe "org-canvas--file-pull-set-properties visibility"
  (before-each (test-org-canvas-reset-file-caches))

  (it "writes PUBLISHED: false for a locked file"
    ;; Issue #50: `locked' is the Publish control, so it is what PUBLISHED
    ;; must round-trip against.
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point) '((locked . t) (hidden . :json-false)))
     (expect (org-entry-get (point) "PUBLISHED") :to-equal "false")
     (expect (org-entry-get (point) "HIDDEN") :to-be nil)))

  (it "writes HIDDEN: true for an unlisted file and leaves it published"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point) '((locked . :json-false) (hidden . t)))
     (expect (org-entry-get (point) "HIDDEN") :to-equal "true")
     (expect (org-entry-get (point) "PUBLISHED") :to-be nil)))

  (it "leaves both properties off for a plain published file"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point) '((locked . :json-false) (hidden . :json-false)))
     (expect (org-entry-get (point) "PUBLISHED") :to-be nil)
     (expect (org-entry-get (point) "HIDDEN") :to-be nil))))

(describe "org-canvas--file-pull-set-properties usage rights"
  (before-each (test-org-canvas-reset-file-caches))

  (it "sets USE_JUSTIFICATION, USAGE_LICENSE, and COPYRIGHT from usage_rights"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((content-type . "application/pdf")
                   (size . 2048)
                   (usage_rights . ((use_justification . "own_copyright")
                                    (license . "cc_by_sa")
                                    (legal_copyright . "2026 Author"))))))
       (org-canvas--file-pull-set-properties (point) item)
       (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-equal "own_copyright")
       (expect (org-entry-get (point) "USAGE_LICENSE") :to-equal "cc_by_sa")
       (expect (org-entry-get (point) "COPYRIGHT") :to-equal "2026 Author"))))

  (it "sets partial usage rights when only justification is present"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((content-type . "application/pdf")
                   (size . 512)
                   (usage_rights . ((use_justification . "fair_use"))))))
       (org-canvas--file-pull-set-properties (point) item)
       (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-equal "fair_use")
       (expect (org-entry-get (point) "USAGE_LICENSE") :to-be nil)
       (expect (org-entry-get (point) "COPYRIGHT") :to-be nil))))

  (it "does not set usage properties when usage_rights is absent"
    (with-temp-org-buffer
     "* [[file:test.pdf][Test PDF]]
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (let ((item '((content-type . "application/pdf")
                   (size . 1024))))
       (org-canvas--file-pull-set-properties (point) item)
       (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-be nil)
       (expect (org-entry-get (point) "USAGE_LICENSE") :to-be nil)
       (expect (org-entry-get (point) "COPYRIGHT") :to-be nil)))))

(describe "org-canvas-pull-files progress"
  (before-each (test-org-canvas-reset-file-caches))

  (it "shows per-file progress in echo area"
    (let* ((temp-dir (make-temp-file "pull-files-test" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-org-canvas-test-config
              (with-sync-test-env
                (with-temp-file files-file (insert ""))
                (make-directory content-dir t)
                (spy-on 'message)
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional _params)
                             '(((id . 1) (display_name . "file1.pdf") (url . "http://x") (size . 100))
                               ((id . 2) (display_name . "file2.pdf") (url . "http://y") (size . 200)))))
                          ((symbol-function 'org-canvas--file-pull-download)
                           (lambda (&rest _) nil))
                          ((symbol-function 'org-canvas--html-to-org)
                           (lambda (html) html)))
                  (org-canvas-pull-files)
                  (expect 'message :to-have-been-called-with
                          "Files [%d/%d] Pulling '%s'..." 1 2 "file1.pdf")
                  (expect 'message :to-have-been-called-with
                          "Files [%d/%d] Pulling '%s'..." 2 2 "file2.pdf")))))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--file-pull-folder-relative-path"
  (before-each (test-org-canvas-reset-file-caches))

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

(describe "org-canvas--file-pull-fetch-folders"
  (before-each (test-org-canvas-reset-file-caches))

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

(describe "org-canvas--file-pull-mode"
  (before-each (test-org-canvas-reset-file-caches))

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

(describe "org-canvas--file-pull-emit-fresh-tree"
  (before-each (test-org-canvas-reset-file-caches))

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
                  (expect body :to-match ":CONTENT_TYPE: +application/pdf")
                  (expect body :to-match ":SIZE: +2048")))
              (expect (length downloads) :to-equal 2)
              (expect (cl-some (lambda (p)
                                 (string-suffix-p "content/syllabus.pdf" p))
                               downloads)
                      :to-be-truthy)))
        (let ((buf (find-buffer-visiting files-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

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
        (delete-directory temp-dir t))))

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
        (delete-directory temp-dir t)))))

(describe "file duplicate detection"
  (before-each (test-org-canvas-reset-file-caches))

  (it "logs a warning for files sharing (size, content-type)"
    (let* ((files '(((id . 1) (display_name . "foo.pdf")
                     (size . 100) (content-type . "application/pdf"))
                    ((id . 2) (display_name . "foo-1.pdf")
                     (size . 100) (content-type . "application/pdf"))
                    ((id . 3) (display_name . "bar.pdf")
                     (size . 200) (content-type . "application/pdf"))))
           (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--file-detect-duplicates files)
        (expect (and (cl-some (lambda (w) (string-match-p "duplicate" w))
                              warnings)
                     t)
                :to-be t))))

  (it "does not warn when all files have distinct (size, content-type)"
    (let* ((files '(((id . 1) (display_name . "a.pdf") (size . 100)
                     (content-type . "application/pdf"))
                    ((id . 2) (display_name . "b.pdf") (size . 200)
                     (content-type . "application/pdf"))))
           (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--file-detect-duplicates files)
        (expect warnings :to-be nil))))

  (it "lists the duplicate group's display names in the warning"
    (let* ((files '(((id . 1) (display_name . "Position.pdf") (size . 2644805)
                     (content-type . "application/pdf"))
                    ((id . 2) (display_name . "Position-1.pdf") (size . 2644805)
                     (content-type . "application/pdf"))))
           (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--file-detect-duplicates files)
        (let ((joined (mapconcat #'identity warnings " ")))
          (expect joined :to-match "Position\\.pdf")
          (expect joined :to-match "Position-1\\.pdf"))))))

;;;; Forced download

(describe "org-canvas--file-pull-download force"
  (it "leaves an existing file alone by default"
    (let ((local (make-temp-file "dl-" nil ".pdf")))
      (unwind-protect
          (progn
            (with-temp-file local (insert "local"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (&rest _) (error "Must not download"))))
              (expect (org-canvas--file-pull-download "Doc" "https://x/y" local 5)
                      :not :to-throw)))
        (delete-file local))))

  (it "overwrites when forced"
    (let ((local (make-temp-file "dl-" nil ".pdf"))
          (called nil))
      (unwind-protect
          (progn
            (with-temp-file local (insert "local"))
            (cl-letf (((symbol-function 'url-copy-file)
                       (lambda (&rest _) (setq called t))))
              (org-canvas--file-pull-download "Doc" "https://x/y" local 5 t)
              (expect called :to-be t)))
        (delete-file local)))))

;;;; Coverage: pull-mode and the usage-rights registry

(describe "org-canvas--file-pull-mode partial heading"
  (before-each (test-org-canvas-reset-file-caches))

  ;; `(unless (or cid link-path) ...)' marks a heading as a folder
  ;; container only when BOTH are absent.  Existing tests covered
  ;; neither-present and both-present, where `or' and `and' agree, so
  ;; the mutation survived.  The discriminating case is a heading with
  ;; exactly one of the two: under `and' it would be mistaken for a
  ;; folder and the pull would reshape the whole file hierarchically.
  (it "is fresh for a link that has not been synced yet"
    (with-temp-org-buffer
     "* [[file:a.pdf][a.pdf]]
"
     (expect (org-canvas--file-pull-mode) :to-equal 'fresh))))

(describe "file pull reads usage rights through the registry (issue #135)"
  (it "writes USE_JUSTIFICATION, USAGE_LICENSE and COPYRIGHT from usage_rights"
    (with-temp-org-buffer
     "* [[file:content/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties
      (point)
      '((id . 1) (locked . :json-false)
        (usage_rights . ((use_justification . "own_copyright")
                         (license . "cc_by")
                         (legal_copyright . "(C) 2026 Ada")))))
     (expect (org-entry-get (point) "USE_JUSTIFICATION") :to-equal "own_copyright")
     (expect (org-entry-get (point) "USAGE_LICENSE") :to-equal "cc_by")
     (expect (org-entry-get (point) "COPYRIGHT") :to-equal "(C) 2026 Ada")
     (expect (org-entry-get (point) "PUBLISHED") :to-be nil)))

  (it "writes PUBLISHED false for a locked file"
    (with-temp-org-buffer
     "* [[file:content/a.pdf][a.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     (org-back-to-heading)
     (org-canvas--file-pull-set-properties (point) '((id . 1) (locked . t)))
     (expect (org-entry-get (point) "PUBLISHED") :to-equal "false"))))

(describe "org-canvas--file-pull-emit-fresh-tree without a trailing newline"
  (before-each (test-org-canvas-reset-file-caches))

  (it "starts the tree on a fresh line"
    (let* ((temp-dir (make-temp-file "emit-tree-nl-" t))
           (files-file (expand-file-name "files.org" temp-dir))
           (content-dir (expand-file-name "content" temp-dir))
           (folder-map (make-hash-table :test 'eql)))
      (unwind-protect
          (let ((org-canvas-files-file files-file))
            (with-temp-file files-file (insert "#+TITLE: Files"))
            (puthash 100 "" folder-map)
            (cl-letf (((symbol-function 'url-copy-file) (lambda (&rest _) nil)))
              (with-current-buffer (find-file-noselect files-file)
                (org-canvas--file-pull-emit-fresh-tree
                 folder-map
                 '(((id . 1) (display_name . "syllabus.pdf") (folder_id . 100)
                    (url . "https://example.com/syllabus.pdf")))
                 content-dir)
                (expect (buffer-string)
                        :to-match "#\\+TITLE: Files\n\\* \\[\\[file:content/syllabus\\.pdf\\]")
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (delete-directory temp-dir t)))))

;;;; A Re-pull Writes What the First Pull Wrote (issue #403)

(describe "A files re-pull leaves the file as it was (issue #403)"
  (it "writes the folder tree the first pull built again on every re-pull"
    (let ((texts (cl-letf (((symbol-function 'url-copy-file) #'ignore))
                   (test-org-canvas-pull-texts
                    'org-canvas-files-file #'org-canvas-pull-files
                    (lambda (_method url &rest _)
                      (cond
                       ((string-match-p "/folders" url)
                        '(((id . 100) (full_name . "course files"))
                          ((id . 200) (full_name . "course files/Labs"))
                          ((id . 300) (full_name . "course files/Labs/Sub"))))
                       ((string-match-p "/files" url)
                        '(((id . 1) (display_name . "a.pdf") (folder_id . 200)
                           (url . "https://example.com/a") (size . 1))
                          ((id . 2) (display_name . "b.pdf") (folder_id . 300)
                           (url . "https://example.com/b") (size . 1))
                          ((id . 3) (display_name . "c.pdf") (folder_id . 100)
                           (url . "https://example.com/c") (size . 1))))))
                    nil 3))))
      (expect (car texts) :to-match "^\\*\\*\\* \\[\\[file:content/Labs/Sub/b\\.pdf\\]")
      (expect (nth 1 texts) :to-equal (car texts))
      (expect (nth 2 texts) :to-equal (car texts)))))

(provide 'org-canvas-files-pull-test)
;;; org-canvas-files-pull-test.el ends here
