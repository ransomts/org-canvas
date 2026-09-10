;;; org-canvas-core-html-test.el --- Tests for org-canvas-core-html -*- lexical-binding: t; -*-

;;; Commentary:

;; HTML export (links, images) and HTML-to-Org conversion.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)
(require 'org-canvas-pages)
(require 'org-canvas-assignments)
(require 'org-canvas-sections)
(require 'org-canvas-files)

(describe "org-canvas--resolve-to-canvas-url"
  (it "resolves a simple heading in pages.org"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* My Page\n:PROPERTIES:\n:CANVAS_URL: my-page-1\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "My Page" dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/pages/my-page-1"))
          (delete-directory dir t)))))

  (it "resolves a file link heading by display name"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (files-file (expand-file-name "files.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file files-file
                (insert "* [[file:content/data.csv][data.csv]]\n:PROPERTIES:\n:CANVAS_ID: 99999\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "files.org"
                       "[[file:../course-content/data.csv][data.csv]]"
                       dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/files/99999"))
          (delete-directory dir t)))))

  (it "returns nil when heading not found"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Other Page\n:PROPERTIES:\n:CANVAS_URL: other\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "Nonexistent" dir)
                      :to-be nil))
          (delete-directory dir t)))))

  (it "returns nil when CANVAS_ID not set"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* My Page\n:PROPERTIES:\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "My Page" dir)
                      :to-be nil))
          (delete-directory dir t)))))

  (it "returns nil for unknown file type"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (file (expand-file-name "unknown.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file file
                (insert "* Heading\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "unknown.org" "Heading" dir)
                      :to-be nil))
          (delete-directory dir t)))))

  (it "resolves correct heading among multiple headings"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* First Page\n:PROPERTIES:\n:CANVAS_URL: first-page\n:END:\n\n* Second Page\n:PROPERTIES:\n:CANVAS_URL: second-page\n:END:\n\n* Third Page\n:PROPERTIES:\n:CANVAS_URL: third-page\n:END:\n"))
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "Second Page" dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/pages/second-page")
              (expect (org-canvas--resolve-to-canvas-url
                       "pages.org" "Third Page" dir)
                      :to-equal
                      "https://test.canvas.example.com/courses/99999/pages/third-page"))
          (delete-directory dir t))))))

(describe "org-canvas--resolve-body-links"
  (it "replaces a cross-file link with Canvas URL"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Lecture 01\n:PROPERTIES:\n:CANVAS_URL: lecture-01\n:END:\n"))
              (with-temp-buffer
                (insert "See [[file:pages.org::*Lecture 01][Lecture 01]].")
                (org-canvas--resolve-body-links dir)
                (expect (buffer-string) :to-match
                        "https://test.canvas.example.com/courses/99999/pages/lecture-01")))
          (delete-directory dir t)))))

  (it "replaces unresolvable links with display text"
    (with-org-canvas-test-config
      (with-temp-buffer
        (insert "See [[file:pages.org::*Missing][My Link]].")
        (org-canvas--resolve-body-links "/nonexistent/dir/")
        (expect (buffer-string) :to-equal "See My Link."))))

  (it "handles escaped brackets in heading"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (files-file (expand-file-name "files.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file files-file
                (insert "* [[file:content/labs/code.py][code.py]]\n:PROPERTIES:\n:CANVAS_ID: 55555\n:END:\n"))
              (with-temp-buffer
                (insert "Get [[file:files.org::*\\[\\[file:../content/labs/code.py\\]\\[code.py\\]\\]][code.py]].")
                (org-canvas--resolve-body-links dir)
                (expect (buffer-string) :to-match "files/55555")))
          (delete-directory dir t))))))

(describe "org-canvas--export-subtree-body-to-html"
  (it "exports subtree with resolved links"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-test-" t))
             (pages-file (expand-file-name "pages.org" dir))
             (assign-file (expand-file-name "test.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Intro Page\n:PROPERTIES:\n:CANVAS_URL: intro-page-1\n:END:\n"))
              (with-temp-file assign-file
                (insert "* Assignment\n:PROPERTIES:\n:END:\n\nSee [[file:pages.org::*Intro Page][Intro Page]].\n"))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (let ((html (org-canvas--export-subtree-body-to-html)))
                  (expect html :to-match "intro-page-1")
                  (expect html :to-match "Intro Page"))
                (kill-buffer)))
          (delete-directory dir t)))))

  (it "excludes override tables from exported HTML"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-override-" t))
             (test-file (expand-file-name "test.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "* Assignment\n:PROPERTIES:\n:END:\n\nComplete the lab.\n\n#+NAME: overrides\n| Section   | Due At              | Unlock At | Lock At |\n|-----------+---------------------+-----------+---------|\n| Section A | <2026-03-01 Sun>    |           |         |\n"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (let ((html (org-canvas--export-subtree-body-to-html)))
                  (expect html :to-match "Complete the lab")
                  (expect html :not :to-match "overrides")
                  (expect html :not :to-match "Section A"))
                (kill-buffer)))
          (delete-directory dir t)))))

  (it "does not warn about links in property drawers (GROUP/RUBRIC_LINK)"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-propdrawer-" t))
             (test-file (expand-file-name "assignments.org" dir))
             (warnings nil))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "* Global Challenge Essay
:PROPERTIES:
:GROUP: [[file:assignment-groups.org::*Essays][Essays]]
:RUBRIC_LINK: [[file:rubrics.org::*Essay Rubric][Essay Rubric]]
:END:

Write the essay.

** Details
:PROPERTIES:
:GROUP: [[file:assignment-groups.org::*Essays][Essays]]
:END:

More text.
"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (cl-letf (((symbol-function 'org-canvas--log-warning)
                           (lambda (_logger fmt &rest args)
                             (push (apply #'format fmt args) warnings))))
                  (let ((html (org-canvas--export-subtree-body-to-html)))
                    (expect html :to-match "Write the essay")
                    (expect html :to-match "More text")
                    (expect html :not :to-match "Essay Rubric")))
                (kill-buffer))
              (expect warnings :to-equal nil))
          (delete-directory dir t)))))

  (it "still warns about genuinely unresolvable body links"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-unresolved-" t))
             (pages-file (expand-file-name "pages.org" dir))
             (test-file (expand-file-name "test.org" dir))
             (warnings nil))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Some Other Page\n"))
              (with-temp-file test-file
                (insert "* Assignment\n:PROPERTIES:\n:END:\n\nSee [[file:pages.org::*Missing Page][Missing Page]].\n"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (cl-letf (((symbol-function 'org-canvas--log-warning)
                           (lambda (_logger fmt &rest args)
                             (push (apply #'format fmt args) warnings))))
                  (let ((html (org-canvas--export-subtree-body-to-html)))
                    (expect html :to-match "Missing Page")))
                (kill-buffer))
              (expect (cl-find-if (lambda (w) (string-match-p "Unresolved" w))
                                  warnings)
                      :to-be-truthy))
          (delete-directory dir t)))))

  (it "succeeds with source blocks without requiring a kernel"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "export-babel-" t))
             (test-file (expand-file-name "test.org" dir))
             (org-html-htmlize-output-type nil))
        (unwind-protect
            (progn
              (with-temp-file test-file
                (insert "* Lecture 01: Intro to Python\n:PROPERTIES:\n:END:\n\nHere is some code:\n\n#+begin_src python\nprint(\"hello world\")\n#+end_src\n"))
              (with-current-buffer (find-file-noselect test-file)
                (goto-char (point-min))
                (let ((html (org-canvas--export-subtree-body-to-html)))
                  (expect html :to-be-truthy)
                  (expect html :to-match "hello world"))
                (kill-buffer)))
          (delete-directory dir t))))))

(describe "org-canvas--export-subtree-body-to-html"
  (it "succeeds with source blocks without requiring a kernel"
    (with-temp-org-buffer
     "* Page Title
:PROPERTIES:
:END:

#+begin_src python
x = 42
#+end_src
"
     (goto-char (point-min))
     (let* ((org-html-htmlize-output-type nil)
            (html (org-canvas--export-subtree-body-to-html)))
       (expect html :to-be-truthy)
       (expect html :to-match "42"))))

  (it "falls back to default-directory when buffer has no file"
    (with-temp-buffer
      (insert "* Heading\n:PROPERTIES:\n:END:\n\nSome content here.\n")
      (org-mode)
      (goto-char (point-min))
      (let ((html (org-canvas--export-subtree-body-to-html)))
        (expect html :to-be-truthy)
        (expect html :to-match "Some content here")))))

;;;; Body Link Resolution Warnings

(describe "org-canvas--resolve-body-links warnings"
  (it "warns on unresolved body links"
    (spy-on 'org-canvas--log-warning)
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Lecture 01\n"))
              (with-temp-buffer
                (insert "See [[file:pages.org::*Missing Page][Missing Page]].")
                (org-canvas--resolve-body-links dir)
                ;; Link should be replaced with display text
                (expect (buffer-string) :to-match "Missing Page")
                (expect (buffer-string) :not :to-match "\\[\\[file:")
                ;; Warning should have been logged
                (expect 'org-canvas--log-warning :to-have-been-called)))
          (delete-directory dir t)))))

  (it "does not warn on resolved body links"
    (spy-on 'org-canvas--log-warning)
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (pages-file (expand-file-name "pages.org" dir)))
        (unwind-protect
            (progn
              (with-temp-file pages-file
                (insert "* Lecture 01\n:PROPERTIES:\n:CANVAS_URL: lecture-01\n:END:\n"))
              (with-temp-buffer
                (insert "See [[file:pages.org::*Lecture 01][Lecture 01]].")
                (org-canvas--resolve-body-links dir)
                (expect (buffer-string) :to-match "lecture-01")
                (expect 'org-canvas--log-warning :not :to-have-been-called)))
          (delete-directory dir t))))))

;;;; Body Link Regex (widened character class)

(describe "org-canvas--resolve-body-links file path support"
  (it "handles paths with spaces"
    (with-org-canvas-test-config
      (let* ((dir (make-temp-file "resolve-test-" t))
             (subdir (expand-file-name "my dir" dir))
             (pages-file (expand-file-name "pages.org" subdir)))
        (unwind-protect
            (progn
              (make-directory subdir t)
              (with-temp-file pages-file
                (insert "* A Page\n:PROPERTIES:\n:CANVAS_URL: a-page\n:END:\n"))
              ;; Note: Org link with space in path won't match the regex
              ;; since [^]:] won't match inside [[file:...]] properly for spaces
              ;; Test that the regex at least doesn't crash on weird content
              (with-temp-buffer
                (insert "See [[file:pages.org::*A Page][A Page]].")
                (org-canvas--resolve-body-links subdir)
                (expect (buffer-string) :to-match "a-page")))
          (delete-directory dir t))))))

(describe "org-canvas--html-to-org pandoc failure"
  (it "returns warning with raw HTML when pandoc exits non-zero"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete _buffer &rest _args)
                 1)))  ;; exit code 1
      (let ((result (org-canvas--html-to-org "<p>Test</p>")))
        (expect result :to-match "WARNING.*pandoc conversion failed")
        (expect result :to-match "<p>Test</p>")))))

(describe "org-canvas--html-strip-ids"
  (it "removes a single id attribute"
    (expect (org-canvas--html-strip-ids "<div id=\"content\">Hi</div>")
            :to-equal "<div>Hi</div>"))

  (it "removes multiple id attributes"
    (expect (org-canvas--html-strip-ids
             "<div id=\"a\"><span id=\"b\">x</span></div>")
            :to-equal "<div><span>x</span></div>"))

  (it "leaves other attributes intact"
    (expect (org-canvas--html-strip-ids
             "<a href=\"x\" id=\"foo\" class=\"bar\">link</a>")
            :to-equal "<a href=\"x\" class=\"bar\">link</a>"))

  (it "is a no-op when there are no id attributes"
    (expect (org-canvas--html-strip-ids "<p>plain</p>")
            :to-equal "<p>plain</p>"))

  (it "handles empty string"
    (expect (org-canvas--html-strip-ids "") :to-equal ""))

  (it "handles nil"
    (expect (org-canvas--html-strip-ids nil) :to-be nil))

  (it "handles single-quoted id values"
    (expect (org-canvas--html-strip-ids "<div id='content'>Hi</div>")
            :to-equal "<div>Hi</div>"))

  (it "handles ids containing dashes and digits"
    (expect (org-canvas--html-strip-ids
             "<div id=\"masthead-container-1\">x</div>")
            :to-equal "<div>x</div>")))

(describe "org-canvas--html-to-org strips id attributes before pandoc"
  (it "passes id-stripped HTML to pandoc"
    (let (captured)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
                ((symbol-function 'call-process-region)
                 (lambda (start end _program &optional _delete _buffer &rest _args)
                   (setq captured (buffer-substring-no-properties start end))
                   0)))
        (org-canvas--html-to-org "<div id=\"content\"><p>x</p></div>")
        (expect captured :not :to-match " id=")))))

(describe "org-canvas--html-to-org-post-process"
  (it "returns nil unchanged"
    (expect (org-canvas--html-to-org-post-process nil) :to-be nil))

  (it "decodes a non-breaking space to a regular space"
    (let* ((nbsp (string ? ))
           (input (concat "Foo" nbsp "Bar")))
      (expect (org-canvas--html-to-org-post-process input)
              :to-equal "Foo Bar")))

  (it "collapses a line containing only NBSP into an empty line"
    ;; Mirrors `<p>&nbsp;</p>' spacers in Canvas's WYSIWYG output:
    ;; the NBSP becomes a regular space, then the whitespace-only
    ;; line is flattened.
    (let* ((nbsp (string ? ))
           (input (concat "before\n" nbsp "\nafter")))
      (expect (org-canvas--html-to-org-post-process input)
              :to-equal "before\n\nafter")))

  (it "collapses a line containing only spaces and tabs into an empty line"
    (expect (org-canvas--html-to-org-post-process "before\n  \t  \nafter")
            :to-equal "before\n\nafter"))

  (it "inserts a space before <YYYY-MM-DD when preceded by non-whitespace"
    (expect (org-canvas--html-to-org-post-process "Due:<2026-04-22>")
            :to-equal "Due: <2026-04-22>"))

  (it "leaves a properly-spaced timestamp alone"
    (expect (org-canvas--html-to-org-post-process "Due: <2026-04-22>")
            :to-equal "Due: <2026-04-22>"))

  (it "does not insert a space at the start of a line"
    ;; A timestamp at line start has no preceding non-whitespace char to
    ;; key off of, so the post-pass leaves it alone.
    (expect (org-canvas--html-to-org-post-process "<2026-04-22>")
            :to-equal "<2026-04-22>"))

  (it "does not insert a space before `<YYYY-MM-DD' inside link display text"
    ;; Org link openers `[[' should not be treated as a label that
    ;; needs a space before a date — the `[' is in the exclusion set
    ;; for the timestamp-spacing regex.  Use a context where the link
    ;; resolves (heading has CUSTOM_ID `tag') so the TOC-repair pass
    ;; doesn't drop the link wrapper.
    (let* ((input (concat "* heading\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: tag\n"
                          ":END:\n"
                          "\n"
                          "see [[#tag][<2026-04-22>]]"))
           (out (org-canvas--html-to-org-post-process input)))
      (expect out :to-match "\\[\\[#tag\\]\\[<2026-04-22>\\]\\]")
      (expect out :not :to-match "< 2026")))

  (it "does not insert a space before a non-date `<' (e.g., `<em>')"
    (expect (org-canvas--html-to-org-post-process "say <em>x</em>")
            :to-equal "say <em>x</em>")))

(describe "org-canvas--normalize-toc-target"
  (it "lowercases and trims"
    (expect (org-canvas--normalize-toc-target "  Overview  ")
            :to-equal "overview"))

  (it "strips a leading numeric prefix `N. '"
    (expect (org-canvas--normalize-toc-target "1. Overview")
            :to-equal "overview"))

  (it "strips a leading numeric prefix `N) '"
    (expect (org-canvas--normalize-toc-target "2) Methods")
            :to-equal "methods"))

  (it "leaves text without a numeric prefix alone"
    (expect (org-canvas--normalize-toc-target "Conclusion")
            :to-equal "conclusion"))

  (it "returns empty for nil"
    (expect (org-canvas--normalize-toc-target nil) :to-equal "")))

(describe "org-canvas--repair-toc-and-prune-customids"
  (it "is a no-op for nil and empty input"
    (expect (org-canvas--repair-toc-and-prune-customids nil) :to-be nil)
    (expect (org-canvas--repair-toc-and-prune-customids "") :to-equal ""))

  (it "rewrites a dangling TOC link to the matching heading's CUSTOM_ID"
    (let* ((input (concat "[[#orga904f23][1. Overview]]\n"
                          "\n"
                          "* 1. Overview\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: overview\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "\\[\\[#overview\\]\\[1\\. Overview\\]\\]")
      (expect out :not :to-match "orga904f23")))

  (it "matches by display text after stripping numeric prefix"
    ;; Heading has no numeric prefix; TOC link does.  Normalization
    ;; strips the prefix from the link text so the lookup succeeds.
    (let* ((input (concat "[[#orgaaa][1. Methods]]\n"
                          "\n"
                          "* Methods\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: methods\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "\\[\\[#methods\\]\\[1\\. Methods\\]\\]")))

  (it "drops the link wrapper when no heading matches"
    (let* ((input "see [[#orphan][Mystery Section]]\n")
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "Mystery Section")
      (expect out :not :to-match "\\[\\[")
      (expect out :not :to-match "orphan")))

  (it "preserves a link whose target is already a known CUSTOM_ID"
    (let* ((input (concat "see [[#methods][Methods]]\n"
                          "\n"
                          "* Methods\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: methods\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match "\\[\\[#methods\\]\\[Methods\\]\\]")))

  (it "prunes a CUSTOM_ID that no remaining link references"
    (let* ((input (concat "[[#kept][Kept]]\n"
                          "\n"
                          "* Kept\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: kept\n"
                          ":END:\n"
                          "\n"
                          "* Orphan\n"
                          ":PROPERTIES:\n"
                          ":CUSTOM_ID: orphan\n"
                          ":END:\n"))
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-match ":CUSTOM_ID: kept")
      (expect out :not :to-match ":CUSTOM_ID: orphan")
      ;; The Orphan heading's now-empty PROPERTIES drawer is collapsed.
      (expect out :not :to-match "^\\* Orphan\n:PROPERTIES:")))

  (it "leaves headings with no CUSTOM_ID drawer alone"
    (let* ((input "* Plain\n\nbody\n")
           (out (org-canvas--repair-toc-and-prune-customids input)))
      (expect out :to-equal input)))

  (it "is a no-op for body without headings or anchor links"
    (let ((input "Just a paragraph.\n\nAnother paragraph."))
      (expect (org-canvas--repair-toc-and-prune-customids input)
              :to-equal input))))

(describe "org-canvas--html-to-org-inline"
  (it "collapses multi-line pandoc output to single line"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete buffer &rest _args)
                 (when buffer
                   (erase-buffer)
                   (insert "Line one\nLine two\nLine three"))
                 0)))
      (expect (org-canvas--html-to-org-inline "<p>Line one</p><p>Line two</p>")
              :to-equal "Line one Line two Line three")))

  (it "returns empty string for nil input"
    (expect (org-canvas--html-to-org-inline nil) :to-equal ""))

  (it "returns empty string for empty input"
    (expect (org-canvas--html-to-org-inline "") :to-equal ""))

  (it "trims surrounding whitespace"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _program &optional _delete buffer &rest _args)
                 (when buffer
                   (erase-buffer)
                   (insert "  trimmed  "))
                 0)))
      (expect (org-canvas--html-to-org-inline "<p> trimmed </p>")
              :to-equal "trimmed"))))

;;;; Inline Image Resolution Tests

(describe "org-canvas--resolve-image-links"
  (it "replaces image link with cached URL"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (puthash "diagram.png" "https://canvas.example.com/files/42/preview"
               org-canvas--image-cache)
      (with-temp-buffer
        (insert "* Heading\nSee [[file:diagram.png]] for details.\n")
        (org-canvas--resolve-image-links "/tmp/")
        (expect (buffer-string) :to-match "canvas\\.example\\.com/files/42/preview"))))

  (it "preserves display text in image links"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (puthash "chart.jpg" "https://canvas.example.com/files/10/preview"
               org-canvas--image-cache)
      (with-temp-buffer
        (insert "* Heading\n[[file:chart.jpg][Sales Chart]]\n")
        (org-canvas--resolve-image-links "/tmp/")
        (expect (buffer-string) :to-match "\\[Sales Chart\\]"))))

  (it "handles multiple image links"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (puthash "a.png" "https://canvas.example.com/a" org-canvas--image-cache)
      (puthash "b.gif" "https://canvas.example.com/b" org-canvas--image-cache)
      (with-temp-buffer
        (insert "* H\n[[file:a.png]] and [[file:b.gif]]\n")
        (org-canvas--resolve-image-links "/tmp/")
        (expect (buffer-string) :to-match "canvas\\.example\\.com/a")
        (expect (buffer-string) :to-match "canvas\\.example\\.com/b"))))

  (it "leaves non-image file links untouched"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (with-temp-buffer
        (insert "* H\n[[file:handout.pdf]]\n")
        (let ((original (buffer-string)))
          (org-canvas--resolve-image-links "/tmp/")
          (expect (buffer-string) :to-equal original)))))

  (it "warns on missing local file"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (spy-on 'org-canvas--log-warning)
      (with-temp-buffer
        (insert "* H\n[[file:missing.png]]\n")
        (org-canvas--resolve-image-links "/tmp/nonexistent/")
        (expect 'org-canvas--log-warning :to-have-been-called))))

  (it "uploads image on cache miss when file exists"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal))
          (temp-file (make-temp-file "img-test" nil ".png")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "PNG"))
            (cl-letf (((symbol-function 'org-canvas--image-ensure-folder)
                       (lambda () 123))
                      ((symbol-function 'org-canvas--upload-file)
                       (lambda (_path &optional _url _name)
                         '((id . 555)))))
              (with-org-canvas-test-config
                (with-temp-buffer
                  (insert (format "* H\n[[file:%s]]\n" temp-file))
                  (org-canvas--resolve-image-links "/")
                  (expect (buffer-string) :to-match "files/555/preview")
                  ;; Verify cache was updated
                  (expect (gethash (file-name-nondirectory temp-file)
                                   org-canvas--image-cache)
                          :to-be-truthy)))))
        (delete-file temp-file))))

  (it "recognizes all image extensions"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
      (dolist (ext org-canvas--image-extensions)
        (puthash (format "test.%s" ext) (format "https://url/%s" ext)
                 org-canvas--image-cache))
      (with-temp-buffer
        (insert "* H\n")
        (dolist (ext org-canvas--image-extensions)
          (insert (format "[[file:test.%s]]\n" ext)))
        (org-canvas--resolve-image-links "/tmp/")
        (dolist (ext org-canvas--image-extensions)
          (expect (buffer-string) :to-match (format "url/%s" ext)))))))

(describe "org-canvas--image-cache-init"
  (it "populates cache from API response"
    (with-org-canvas-test-config
      (let ((org-canvas--image-cache nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     ;; folders/by_path returns a list
                     '(((id . 42)))))
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &rest _args)
                     '(((display_name . "logo.png") (url . "https://files.canvas/logo.png"))
                       ((display_name . "banner.jpg") (url . "https://files.canvas/banner.jpg"))))))
          (org-canvas--image-cache-init)
          (expect (hash-table-count org-canvas--image-cache) :to-equal 2)
          (expect (gethash "logo.png" org-canvas--image-cache)
                  :to-equal "https://files.canvas/logo.png")))))

  (it "handles missing folder gracefully"
    (with-org-canvas-test-config
      (let ((org-canvas--image-cache nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest _args)
                     (signal 'error '("404")))))
          (org-canvas--image-cache-init)
          ;; Cache should exist but be empty
          (expect org-canvas--image-cache :to-be-truthy)
          (expect (hash-table-count org-canvas--image-cache) :to-equal 0)))))

  (it "does not re-initialize if already set"
    (let ((org-canvas--image-cache (make-hash-table :test 'equal))
          (api-called nil))
      (puthash "existing.png" "url" org-canvas--image-cache)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (setq api-called t) nil)))
        (org-canvas--image-cache-init)
        (expect api-called :to-be nil)
        (expect (hash-table-count org-canvas--image-cache) :to-equal 1)))))

(describe "org-canvas--image-ensure-folder"
  (it "returns existing folder ID"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method _url &rest _args)
                   '(((id . 77))))))
        (expect (org-canvas--image-ensure-folder) :to-equal 77))))

  (it "creates folder when not found"
    (with-org-canvas-test-config
      (let ((call-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (setq call-count (1+ call-count))
                     (if (eq method 'GET)
                         (signal 'error '("404"))
                       '((id . 88))))))
          (expect (org-canvas--image-ensure-folder) :to-equal 88)
          (expect call-count :to-equal 2))))))

;;;; Image upload failure in resolve-single-image

(describe "org-canvas--resolve-single-image"
  (it "handles upload failure gracefully"
    (with-org-canvas-test-config
      (let* ((temp-dir (make-temp-file "img-test-" t))
             (img-file (expand-file-name "test.png" temp-dir))
             (org-canvas--image-cache (make-hash-table :test 'equal))
             (org-canvas-image-folder "course-images")
             (folder-id-ref (list 99))
             (replaced nil))
        (unwind-protect
            (progn
              (with-temp-file img-file (insert "PNGDATA"))
              (cl-letf (((symbol-function 'org-canvas--upload-file)
                         (lambda (&rest _) (error "Network error")))
                        ((symbol-function 'org-canvas--image-replace-link)
                         (lambda (&rest _) (setq replaced t))))
                (let ((rep (list :path "test.png" :start 1 :end 20 :display nil)))
                  ;; Should not error, just warn
                  (org-canvas--resolve-single-image rep temp-dir folder-id-ref 1 1)
                  ;; Image should NOT have been replaced (upload failed)
                  (expect replaced :to-be nil))))
          (delete-directory temp-dir t))))))

(describe "org-canvas--resolve-single-image echo area warnings"
  (it "shows warning when image upload fails"
    (with-org-canvas-test-config
      (let ((temp-dir (make-temp-file "img-test-" t)))
        (unwind-protect
            (let ((img-file (expand-file-name "test.png" temp-dir)))
              (with-temp-file img-file (insert "fake-png"))
              (spy-on 'message)
              (spy-on 'org-canvas--log-warning)
              (spy-on 'org-canvas--log-info)
              (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
                (cl-letf (((symbol-function 'org-canvas--image-ensure-folder)
                           (lambda () 1))
                          ((symbol-function 'org-canvas--upload-file)
                           (lambda (&rest _) (error "Upload failed"))))
                  (org-canvas--resolve-single-image
                   (list :start 0 :end 10 :path "test.png" :display nil)
                   temp-dir (list nil) 1 1)
                  (expect 'message :to-have-been-called-with
                          "WARNING: Image upload failed: %s" "test.png"))))
          (delete-directory temp-dir t)))))

  (it "shows warning when image file not found"
    (with-org-canvas-test-config
      (spy-on 'message)
      (spy-on 'org-canvas--log-warning)
      (let ((org-canvas--image-cache (make-hash-table :test 'equal)))
        (org-canvas--resolve-single-image
         (list :start 0 :end 10 :path "nonexistent.png" :display nil)
         "/tmp/no-such-dir" (list nil) 1 1)
        (expect 'message :to-have-been-called-with
                "WARNING: Image not found: %s"
                (expand-file-name "nonexistent.png" "/tmp/no-such-dir"))))))

(describe "org-canvas--resolve-image-links progress"
  (it "shows per-image progress messages"
    (with-org-canvas-test-config
      (spy-on 'message)
      (spy-on 'org-canvas--resolve-single-image)
      (spy-on 'org-canvas--image-cache-init)
      (with-temp-buffer
        (insert "[[file:img1.png]] and [[file:img2.png]]")
        (org-canvas--resolve-image-links "/tmp/")
        (expect 'message :to-have-been-called-with "Images [%d/%d] Processing..." 1 2)
        (expect 'message :to-have-been-called-with "Images [%d/%d] Processing..." 2 2)))))

(describe "org-canvas--export-subtree-body-to-html offline (issue #83)"
  (it "skips link and image resolution so a read-only caller never uploads"
    (with-temp-org-buffer
     "* Page\n\nSee [[file:pages.org::*Other][Other]] and [[file:img.png]].\n"
     (org-back-to-heading)
     (let ((links nil) (images nil))
       (cl-letf (((symbol-function 'org-canvas--resolve-body-links)
                  (lambda (_) (setq links t)))
                 ((symbol-function 'org-canvas--resolve-image-links)
                  (lambda (_) (setq images t))))
         (let ((html (org-canvas--export-subtree-body-to-html t)))
           (expect html :to-match "Other")
           (expect links :to-be nil)
           (expect images :to-be nil))
         (org-canvas--export-subtree-body-to-html)
         (expect links :to-be t)
         (expect images :to-be t))))))

;;;; Body Headings Never Become Headlines (issue #175)

(defun test-org-canvas-pandoc-emitting (org-text)
  "Return a `call-process-region' stand-in that replaces the region with ORG-TEXT."
  (lambda (start end _program &optional _delete _buffer &rest _args)
    (delete-region start end)
    (insert org-text)
    0))

(describe "org-canvas--org-body-neutralize-headlines"
  (it "returns nil for nil and text without a headline as it is"
    (expect (org-canvas--org-body-neutralize-headlines nil) :to-be nil)
    (expect (org-canvas--org-body-neutralize-headlines "plain\n*bold* text")
            :to-equal "plain\n*bold* text"))

  (it "turns a headline and its pandoc drawer into a heading block"
    (expect (org-canvas--org-body-neutralize-headlines
             "** Read this\n:PROPERTIES:\n:CLASS: article-title\n:END:\nafter")
            :to-equal "#+begin_h2\nRead this\n#+end_h2\nafter"))

  (it "keeps the level of a bare headline"
    (expect (org-canvas--org-body-neutralize-headlines "* Top\nbody")
            :to-equal "#+begin_h1\nTop\n#+end_h1\nbody"))

  (it "never emits a headline, whatever the depth"
    (let ((out (org-canvas--org-body-neutralize-headlines
                "intro\n******** Deep\nmid\n* Top\nend")))
      (expect out :not :to-match "^\\*+ ")
      (expect out :to-match "^#\\+begin_h6\nDeep\n#\\+end_h6$")
      (expect out :to-match "^#\\+begin_h1\nTop\n#\\+end_h1$")
      (expect out :to-match "^mid$")
      (expect out :to-match "^end$")))

  (it "carries a referenced CUSTOM_ID as a target and retargets its link"
    (expect (org-canvas--org-body-neutralize-headlines
             (concat "see [[#overview][1. Overview]]\n"
                     "** Overview\n:PROPERTIES:\n:CUSTOM_ID: overview\n:END:\ntext"))
            :to-equal (concat "see [[overview][1. Overview]]\n"
                              "#+begin_h2\n<<overview>> Overview\n#+end_h2\ntext")))

  (it "drops a headline with no title"
    (expect (org-canvas--org-body-neutralize-headlines "before\n** \nafter")
            :to-equal "before\n\nafter"))

  (it "escapes a title that is itself shaped like a headline"
    (let ((out (org-canvas--org-body-neutralize-headlines "** * starry\ntail")))
      (expect out :to-equal "#+begin_h2\n\\ast{} starry\n#+end_h2\ntail")
      (expect out :not :to-match "^\\*+ "))))

(describe "org-canvas--html-to-org keeps a body free of headlines"
  (it "turns the headline pandoc emits for <h1> into a block"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (test-org-canvas-pandoc-emitting
                "* Heading\n:PROPERTIES:\n:CUSTOM_ID: heading\n:END:\n\nafter")))
      (let ((out (org-canvas--html-to-org "<h1>Heading</h1><p>after</p>")))
        (expect out :not :to-match "^\\* ")
        (expect out :to-match "^#\\+begin_h1\nHeading\n#\\+end_h1")
        (expect out :to-match "^after$"))))

  (it "guards the raw-HTML fallback as well"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (let ((out (org-canvas--html-to-org "<pre>\n* not a heading\n</pre>")))
        (expect out :to-match "WARNING: pandoc not found")
        (expect out :not :to-match "^\\* ")
        (expect out :to-match "not a heading"))))

  (it "collapses a heading block to its text inline"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "pandoc"))
              ((symbol-function 'call-process-region)
               (test-org-canvas-pandoc-emitting "** Title\nrest")))
      (expect (org-canvas--html-to-org-inline "<h2>Title</h2>rest")
              :to-equal "Title rest"))))

(describe "org-canvas--org-to-html-string heading blocks"
  (it "exports a one-paragraph heading block as the heading it came from"
    (let ((html (org-canvas--org-to-html-string
                 (concat "intro\n\n#+begin_h2\n*Read:* [[https://x.example/a][Art]]"
                         " and answer.\n#+end_h2\n\nafter"))))
      (expect html :to-match
              (concat "<h2><b>Read:</b> <a href=\"https://x.example/a\">Art</a>"
                      " and answer.</h2>"))
      (expect html :not :to-match "class=\"h2\"")
      (expect html :to-match "after")))

  (it "keeps a block of several paragraphs a div"
    (let ((html (org-canvas--org-to-html-string "#+begin_h2\none\n\ntwo\n#+end_h2")))
      (expect html :to-match "<div class=\"h2\"")
      (expect html :not :to-match "<h2>")))

  (it "leaves another backend's output alone"
    (expect (org-canvas--html-heading-block-filter "\\begin{h2}x\\end{h2}" 'latex nil)
            :to-equal "\\begin{h2}x\\end{h2}"))

  (it "reaches the subtree exporter every registry body goes through"
    (with-temp-org-buffer
     "* Page
:PROPERTIES:
:CANVAS_ID: 1
:END:
text

#+begin_h3
Sub
#+end_h3

more
"
     (org-back-to-heading t)
     (let ((html (org-canvas--export-subtree-body-to-html t)))
       (expect html :to-match "<h3>Sub</h3>")
       (expect html :to-match "more")))))

(provide 'org-canvas-core-html-test)
;;; org-canvas-core-html-test.el ends here
