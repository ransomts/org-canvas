;;; org-canvas-core-html.el --- HTML export and conversion for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Both directions of the HTML boundary: resolving cross-file links and
;; inline images for export, exporting a subtree body to HTML, and
;; turning Canvas HTML back into Org on pull (pandoc when available),
;; including the heading-block and custom-id repairs that keep a body
;; from introducing a headline (issue #175).

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox)
(require 'subr-x)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)

;;;; Cross-file Link Resolution for HTML Export

(defun org-canvas--resolve-to-canvas-url (file heading source-dir)
  "Resolve a cross-file link to a Canvas URL.
FILE is the relative path to a .org file.
HEADING is the heading search text (may contain escaped brackets).
SOURCE-DIR is the directory of the source file containing the link."
  (let* ((abs-file (expand-file-name file source-dir))
         (basename (file-name-nondirectory file))
         (url-info (assoc basename org-canvas--file-to-endpoint-map))
         (url-info (when url-info (cdr url-info))))
    (when (and url-info (file-exists-p abs-file))
      (let ((endpoint (car url-info))
            (id-prop (cadr url-info))
            (clean-heading (org-canvas--unescape-org-brackets heading)))
        (let ((heading-point (org-canvas--find-heading-in-file abs-file clean-heading)))
          (when heading-point
            (with-current-buffer (org-canvas--find-file-noselect abs-file)
              (let ((id (org-entry-get heading-point id-prop)))
                (when id
                  (format "%s/courses/%s/%s/%s"
                          org-canvas-base-url org-canvas-course-id
                          endpoint id))))))))))

(defun org-canvas--replace-link-with-canvas-url (link-info source-dir)
  "Replace Org link described by LINK-INFO with a Canvas URL link.
LINK-INFO is a plist with :start :end :file :heading :display.
SOURCE-DIR is the directory of the source .org file.
If resolution fails, replaces with plain display text."
  (let ((link-start (plist-get link-info :start))
        (link-end (plist-get link-info :end))
        (file (plist-get link-info :file))
        (heading (plist-get link-info :heading))
        (display (plist-get link-info :display)))
    (let ((canvas-url (org-canvas--resolve-to-canvas-url
                       file heading source-dir)))
      (delete-region link-start link-end)
      (goto-char link-start)
      (if canvas-url
          (insert (format "[[%s][%s]]" canvas-url display))
        (org-canvas--log-warning org-canvas--logger
          "[Links] Unresolved: [[file:%s::*%s][%s]] → plain text"
          file heading display)
        (insert display)))))

(defun org-canvas--resolve-body-links (source-dir)
  "Resolve cross-file Org links in current buffer to Canvas URLs.
Replaces [[file:*.org::*HEADING][DISPLAY]] links with Canvas URL links.
Unresolvable links are replaced with their display text.
SOURCE-DIR is the directory of the source .org file."
  (goto-char (point-min))
  (while (re-search-forward
          "\\[\\[file:\\([^]:]+\\.org\\)::\\*" nil t)
    (let ((link-start (match-beginning 0))
          (file (match-string 1))
          (heading-start (point)))
      (when (search-forward "][" nil t)
        (let* ((heading (buffer-substring-no-properties
                         heading-start (- (point) 2)))
               (display-start (point)))
          (when (search-forward "]]" nil t)
            (let* ((link-end (point))
                   (display (buffer-substring-no-properties
                             display-start (- link-end 2))))
              (org-canvas--replace-link-with-canvas-url
               (list :start link-start :end link-end :file file
                     :heading heading :display display)
               source-dir))))))))

;;;; Inline Image Resolution

(defvar org-canvas--image-cache nil
  "Hash-table mapping local filename to Canvas preview URL.
Session-scoped; cleared at end of master sync.")

(defcustom org-canvas-image-folder "org-canvas-images"
  "Canvas folder name for inline images uploaded by org-canvas."
  :type 'string
  :group 'org-canvas)

(defconst org-canvas--image-extensions
  '("png" "jpg" "jpeg" "gif" "svg" "webp" "bmp")
  "File extensions recognized as inline images.")

(defun org-canvas--image-cache-load-folder ()
  "Fetch image folder contents and populate `org-canvas--image-cache'."
  (let* ((folders-url (org-canvas-api-course-endpoint
                       (format "folders/by_path/%s" org-canvas-image-folder)))
         (folder (car (last (org-canvas-api-request 'GET folders-url))))
         (folder-id (alist-get 'id folder)))
    (when folder-id
      (let ((files (org-canvas-api-request-all-pages
                    'GET (format "%s/api/v1/folders/%s/files"
                                 org-canvas-base-url folder-id))))
        (dolist (file files)
          (let ((name (alist-get 'display_name file))
                (url (alist-get 'url file)))
            (when (and name url)
              (puthash name url org-canvas--image-cache))))
        (org-canvas--log-debug org-canvas--logger
          "[Images] Cache initialized: %d files in %s/"
          (hash-table-count org-canvas--image-cache)
          org-canvas-image-folder)))))

(defun org-canvas--image-cache-init ()
  "Initialize the image cache from Canvas folder listing.
Fetches the image folder contents and builds a filename->URL map."
  (unless org-canvas--image-cache
    (setq org-canvas--image-cache (make-hash-table :test 'equal))
    (condition-case nil
        (org-canvas--image-cache-load-folder)
      (error
       (org-canvas--log-debug org-canvas--logger
         "[Images] Folder '%s' not found, will create on first upload"
         org-canvas-image-folder)))))

(defun org-canvas--image-ensure-folder ()
  "Ensure the image upload folder exists on Canvas.
Returns the folder ID."
  (condition-case nil
      (let* ((url (org-canvas-api-course-endpoint
                   (format "folders/by_path/%s" org-canvas-image-folder)))
             (folder (car (last (org-canvas-api-request 'GET url)))))
        (alist-get 'id folder))
    (error
     ;; Create the folder
     (org-canvas--log-info org-canvas--logger "[Images] Creating folder: %s" org-canvas-image-folder)
     (let ((response (org-canvas-api-request
                      'POST (org-canvas-api-course-endpoint "folders")
                      :data `((name . ,org-canvas-image-folder)
                              (parent_folder_path . "/")))))
       (alist-get 'id response)))))

(defun org-canvas--image-replace-link (rep url)
  "Replace image link described by REP plist with Canvas URL link."
  (let ((display (plist-get rep :display))
        (filename (file-name-nondirectory (plist-get rep :path))))
    (goto-char (plist-get rep :start))
    (delete-region (plist-get rep :start) (plist-get rep :end))
    (insert (format "[[%s][%s]]" url (or display filename)))))

(defun org-canvas--resolve-single-image (rep source-dir folder-id-ref count total)
  "Process a single image link REP in SOURCE-DIR.
FOLDER-ID-REF is a cons cell whose car is the folder ID (lazily initialized).
COUNT and TOTAL are for progress logging.
Checks cache first, then uploads if file exists."
  (let* ((rel-path (plist-get rep :path))
         (abs-path (expand-file-name rel-path source-dir))
         (filename (file-name-nondirectory rel-path))
         (cached-url (gethash filename org-canvas--image-cache)))
    (cond
     (cached-url
      (org-canvas--log-debug org-canvas--logger "[Images] Cache hit: %s" filename)
      (org-canvas--image-replace-link rep cached-url))
     ((file-exists-p abs-path)
      (condition-case err
          (progn
            (org-canvas--log-info org-canvas--logger
              "[Images] Uploading %s (%d/%d)..." filename count total)
            (unless (car folder-id-ref)
              (setcar folder-id-ref (org-canvas--image-ensure-folder)))
            (let* ((notify-url (format "%s/api/v1/folders/%s/files"
                                       org-canvas-base-url (car folder-id-ref)))
                   (file-obj (org-canvas--upload-file abs-path notify-url))
                   (preview-url (format "%s/courses/%s/files/%s/preview"
                                        org-canvas-base-url
                                        org-canvas-course-id
                                        (alist-get 'id file-obj))))
              (puthash filename preview-url org-canvas--image-cache)
              (org-canvas--image-replace-link rep preview-url)
              t))
        (error
         (org-canvas--log-warning org-canvas--logger
           "[Images] Failed to upload %s: %s"
           filename (error-message-string err))
         (message "WARNING: Image upload failed: %s" filename)
         nil)))
     (t
      (org-canvas--log-warning org-canvas--logger
        "[Images] File not found: %s" abs-path)
      (message "WARNING: Image not found: %s" abs-path)
      nil))))

(defun org-canvas--resolve-image-links (source-dir)
  "Resolve inline image links in current buffer to Canvas URLs.
Replaces [[file:IMAGE]] links with Canvas preview URLs.
SOURCE-DIR is the directory of the source .org file.
Images are uploaded to the `org-canvas-image-folder' on Canvas."
  (goto-char (point-min))
  (let ((image-re (format "\\[\\[file:\\([^]]+\\.\\(%s\\)\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
                          (regexp-opt org-canvas--image-extensions)))
        (replacements nil))
    ;; Collect all matches first (avoid modifying buffer during search)
    (while (re-search-forward image-re nil t)
      (let ((link-start (match-beginning 0))
            (link-end (match-end 0))
            (rel-path (match-string 1))
            (display (match-string 3)))
        (push (list :start link-start :end link-end
                    :path rel-path :display display)
              replacements)))
    ;; Process in reverse order (last match first) to preserve positions
    (when replacements
      (org-canvas--image-cache-init)
      (let ((folder-id-ref (list nil))
            (count 0)
            (failed 0)
            (total (length replacements)))
        (dolist (rep replacements)
          (setq count (1+ count))
          (message "Images [%d/%d] Processing..." count total)
          (unless (org-canvas--resolve-single-image
                   rep source-dir folder-id-ref count total)
            (setq failed (1+ failed))))
        (when (> failed 0)
          (org-canvas--log-warning org-canvas--logger
            "[Images] %d of %d images failed to process" failed total)
          (message "WARNING: %d of %d images failed. See *canvas-log*."
                   failed total))))))

;;;; HTML Export

(defvar org-export-with-broken-links)

(defvar org-export-with-sub-superscripts)

(defvar org-export-use-babel)

(defconst org-canvas--html-heading-div-re
  (concat "\\`<div class=\"h\\([1-6]\\)\"[^>]*>[ \t\n]*<p>"
          "\\(\\(?:.\\|\n\\)*?\\)</p>[ \t\n]*</div>[ \t\n]*\\'")
  "Match the HTML ox-html emits for a one-paragraph `#+begin_hN' block.
G1 = the level, G2 = the paragraph's inner HTML.")

(defun org-canvas--html-heading-block-filter (text backend _info)
  "Render a `#+begin_hN' special block as an HTML `<hN>' heading.
An export filter for `org-export-filter-special-block-functions'.
TEXT is the block as ox-html rendered it, a `<div class=\"hN\">'
around one paragraph; BACKEND must derive from `html'.  The block is
what a pulled body keeps an HTML heading as (issue #175), so a push
returns the heading Canvas had.  A block of more than one paragraph
stays a div."
  (let* ((level (and (org-export-derived-backend-p backend 'html)
                     (string-match org-canvas--html-heading-div-re text)
                     (match-string 1 text)))
         (inner (and level (match-string 2 text))))
    (if (and inner (not (string-match-p "</?p>" inner)))
        (format "<h%s>%s</h%s>\n" level (string-trim inner) level)
      text)))

(defmacro org-canvas--with-body-export-settings (&rest body)
  "Run BODY, an Org to HTML export, the way a pushed body is exported.
Sub- and superscripts stay literal and a heading block comes back as
the `<hN>' it was pulled from (issue #175)."
  (declare (indent 0))
  `(let ((org-export-with-sub-superscripts nil)
         (org-export-filter-special-block-functions
          (cons #'org-canvas--html-heading-block-filter
                org-export-filter-special-block-functions)))
     ,@body))

(defun org-canvas--org-to-html-string (text)
  "Export Org TEXT to an HTML fragment the way a body is pushed.
The exporter behind every module's own body text — a quiz
description, a question — so a heading block in any of them reaches
Canvas as a heading (issue #175)."
  (org-canvas--with-body-export-settings
    (org-export-string-as text 'html t)))

(defun org-canvas--export-subtree-body-to-html (&optional offline)
  "Export current Org subtree to HTML, resolving cross-file links.
Returns the HTML string.  Cross-file links [[file:*.org::*...][...]]
are resolved to Canvas URLs when the target has a CANVAS_ID.

When OFFLINE is non-nil, neither links nor inline images are
resolved.  Image resolution uploads the images Canvas lacks, so a
read-only caller — the drift report comparing bodies (issue #83) —
must ask for this; it gets the same text with local link markup."
  (save-excursion
    (org-back-to-heading t)
    (let* ((beg (point))
           (end (save-excursion (org-end-of-subtree t) (point)))
           (content (buffer-substring beg end))
           (source-dir (file-name-directory
                        (or (buffer-file-name) default-directory))))
      (with-temp-buffer
        (let ((default-directory source-dir))
          (insert content)
          (org-mode)
          ;; Strip the override and accommodation tables (a `#+NAME:'
          ;; line and the table rows after it): they feed a sync, not
          ;; the description.
          (goto-char (point-min))
          (while (re-search-forward "^#\\+NAME: \\(overrides\\|accommodations\\)\n" nil t)
            (let ((start (match-beginning 0)))
              (while (looking-at "^|")
                (forward-line 1))
              (delete-region start (point))))
          ;; Strip property drawers: the HTML exporter drops them anyway,
          ;; and property links (GROUP:, RUBRIC_LINK:) must not reach the
          ;; body link resolver, which cannot resolve them to Canvas URLs
          ;; and would emit false "Unresolved" warnings
          (goto-char (point-min))
          (while (re-search-forward org-property-drawer-re nil t)
            (delete-region (match-beginning 0)
                           (min (1+ (match-end 0)) (point-max))))
          (unless offline
            ;; Resolve cross-file links to Canvas URLs
            (org-canvas--resolve-body-links source-dir)
            ;; Resolve inline image links to Canvas URLs
            (org-canvas--resolve-image-links source-dir))
          ;; Export the subtree to HTML (body only)
          (goto-char (point-min))
          (let ((org-export-with-broken-links 'mark)
                (org-export-use-babel nil))
            (org-canvas--with-body-export-settings
              (org-export-as 'html t nil t nil))))))))

;;;; HTML to Org Conversion

(defun org-canvas--html-strip-ids (html)
  "Remove all `id=\"...\"' attributes from HTML.
Pandoc renders HTML id attributes as Org radio targets (\"<<foo>>\"),
which leak structural anchors from Canvas's page templates into pulled
bodies.  Stripping ids before conversion suppresses the artifact."
  (when html
    (replace-regexp-in-string
     " id=\\(\"[^\"]*\"\\|'[^']*'\\)" "" html)))

(defconst org-canvas--customid-line-re
  "^[ \t]*:CUSTOM_ID:[ \t]+\\(.+?\\)[ \t]*$"
  "Match a `:CUSTOM_ID:' property drawer entry.  G1 = id value.")

(defconst org-canvas--anchor-link-re
  "\\[\\[#\\([^]\n]+\\)\\]\\[\\([^]\n]+\\)\\]\\]"
  "Match an Org link to a CUSTOM_ID anchor.  G1 = id, G2 = display text.")

(defun org-canvas--normalize-toc-target (s)
  "Return a fuzzy-match key for TOC display text S.
Lowercases, trims, and strips a leading `N.' or `N) ' numeric prefix
so a heading titled \"Overview\" matches a TOC link reading
\"1. Overview\".  Returns the empty string for nil."
  (if (or (null s) (string-empty-p s))
      ""
    (let ((s (downcase (string-trim s))))
      (replace-regexp-in-string "\\`[0-9]+[.)][ \t]*" "" s))))

(defun org-canvas--collect-customids (text)
  "Return an alist of (NORMALIZED-HEADING . CUSTOM-ID) for TEXT.
Walks Org headings in TEXT and pairs each one with its CUSTOM_ID
property drawer entry (if any).  Headings without a CUSTOM_ID are
not in the result."
  (let ((result nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ +\\(.+\\)$" nil t)
        (let ((heading (match-string-no-properties 1))
              (heading-end (match-end 0))
              (custom-id nil))
          (save-excursion
            (goto-char heading-end)
            (forward-line)
            ;; A pandoc-generated PROPERTIES drawer sits right after the
            ;; heading line.  Bound the search to the next ~10 lines AND
            ;; refuse to cross another heading.
            (let* ((bound-pos (save-excursion (forward-line 10) (point)))
                   (next-heading (save-excursion
                                   (and (re-search-forward "^\\*+ "
                                                           bound-pos t)
                                        (match-beginning 0))))
                   (bound (or next-heading bound-pos)))
              (when (re-search-forward
                     org-canvas--customid-line-re bound t)
                (setq custom-id
                      (string-trim (match-string-no-properties 1))))))
          (when custom-id
            (push (cons (org-canvas--normalize-toc-target heading)
                        custom-id)
                  result)))))
    (nreverse result)))

(defun org-canvas--rewrite-toc-links (text customid-by-heading)
  "Rewrite dangling `[[#X][Y]]' anchor links in TEXT.
CUSTOMID-BY-HEADING is the alist returned by
`org-canvas--collect-customids'.

For each link:
- If X is a known CUSTOM_ID (a value in CUSTOMID-BY-HEADING), the
  link already resolves and is kept unchanged.
- Else the link's display text Y is normalized and looked up
  against the heading map.  If a heading matches, the link target
  is rewritten to that heading's CUSTOM_ID.
- Else the link wrapper is dropped and only Y survives — better a
  plain phrase than a dangling anchor."
  (let ((known-ids (mapcar #'cdr customid-by-heading)))
    (replace-regexp-in-string
     org-canvas--anchor-link-re
     (lambda (match)
       (save-match-data
         (string-match org-canvas--anchor-link-re match)
         (let ((id (match-string 1 match))
               (display (match-string 2 match)))
           (cond
            ((member id known-ids) match)
            ((let ((target
                    (cdr (assoc (org-canvas--normalize-toc-target display)
                                customid-by-heading))))
               (and target (format "[[#%s][%s]]" target display))))
            (t display)))))
     text t t)))

(defun org-canvas--collect-referenced-customids (text)
  "Return de-duplicated CUSTOM_IDs referenced by `[[#X][Y]]' links in TEXT."
  (let ((ids nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward org-canvas--anchor-link-re nil t)
        (push (match-string-no-properties 1) ids)))
    (delete-dups ids)))

(defun org-canvas--prune-unreferenced-customids (text referenced-ids)
  "Strip `:CUSTOM_ID:' drawer lines from TEXT not in REFERENCED-IDS.
Collapses now-empty `:PROPERTIES:'/`:END:' blocks.  Returns the
processed text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward org-canvas--customid-line-re nil t)
      (let ((id (string-trim (match-string-no-properties 1))))
        (unless (member id referenced-ids)
          (delete-region (line-beginning-position)
                         (min (point-max) (1+ (line-end-position)))))))
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*:PROPERTIES:[ \t]*\n[ \t]*:END:[ \t]*\n?"
            nil t)
      (replace-match ""))
    (buffer-string)))

(defun org-canvas--repair-toc-and-prune-customids (text)
  "Repair dangling TOC links and prune orphan CUSTOM_IDs in TEXT.

Pandoc's HTML→Org conversion produces two correlated artifacts when
a Canvas page contains a table of contents:

1. Anchor links like `[[#orga904f23][1. Overview]]' that target the
   *original* HTML `id' attributes (which we strip before pandoc),
   so every TOC link points at a non-existent anchor.
2. A `:CUSTOM_ID:' drawer entry on every heading, pandoc-derived
   from the heading text — most are inert, since nothing in the
   converted body links to them.

This pass first re-targets dangling TOC links to the matching
heading (by display text), then drops any CUSTOM_ID that no
remaining link references.  Returns TEXT unchanged when nil or
empty."
  (if (or (null text) (string-empty-p text))
      text
    (let* ((map (org-canvas--collect-customids text))
           (rewritten (org-canvas--rewrite-toc-links text map))
           (used (org-canvas--collect-referenced-customids rewritten)))
      (org-canvas--prune-unreferenced-customids rewritten used))))

(defun org-canvas--html-to-org-post-process (text)
  "Clean up pandoc's Org output before insertion.

- Decode U+00A0 (non-breaking space, from `&nbsp;') to a regular
  space.  Without this, lines that originated as `<p>&nbsp;</p>'
  spacers in Canvas's WYSIWYG output stay as literal NBSP-only
  lines that look blank but aren't (`cat -A' shows `M-BM-').
- Collapse lines containing only whitespace into empty lines so
  spacer paragraphs don't survive as decorated blanks.
- Insert a space before an Org timestamp `<YYYY-MM-DD' when the
  preceding character is non-whitespace and not `<' or `[' —
  pandoc occasionally emits `Due:<2026-04-22>' with no separator
  when the source HTML had a narrow space or NBSP between label
  and date.
- Repair dangling TOC links and prune orphan `:CUSTOM_ID:' drawer
  entries (see `org-canvas--repair-toc-and-prune-customids').

Returns TEXT unchanged when nil."
  (when text
    (let* ((s (replace-regexp-in-string " " " " text))
           (s (replace-regexp-in-string "^[ \t]+$" "" s))
           (s (replace-regexp-in-string
               "\\([^][ \t<\n]\\)\\(<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
               "\\1 \\2"
               s))
           (s (org-canvas--repair-toc-and-prune-customids s)))
      s)))

(defconst org-canvas--body-headline-re "^\\(\\*+\\)[ \t]+\\(.*\\)$"
  "Match an Org headline line.  G1 = stars, G2 = title.")

(defun org-canvas--body-drawer-end ()
  "Return the end of the property drawer under the headline at point.
Point is at the end of a headline line.  The drawer is the one
pandoc writes under a converted heading; nil when the next line is
not `:PROPERTIES:' or the drawer never closes."
  (save-excursion
    (forward-line 1)
    (when (and (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
               (re-search-forward "^[ \t]*:END:[ \t]*$" nil t))
      (point))))

(defun org-canvas--body-drawer-custom-id (start end)
  "Return the CUSTOM_ID named in the drawer between START and END, or nil."
  (save-excursion
    (goto-char start)
    (when (re-search-forward org-canvas--customid-line-re end t)
      (string-trim (match-string-no-properties 1)))))

(defun org-canvas--body-replace-headline ()
  "Replace the headline `org-canvas--body-headline-re' just matched.
Point is at the end of the match with the match data live.  The
headline line and the drawer under it go; a heading block of the
same level takes their place, carrying the drawer's CUSTOM_ID as a
`<<target>>' so a link to it still resolves.  Returns that id, or
nil."
  (let* ((level (min 6 (length (match-string 1))))
         (title (string-trim (match-string-no-properties 2)))
         (start (match-beginning 0))
         (line-end (match-end 0))
         (drawer-end (org-canvas--body-drawer-end))
         (id (and drawer-end
                  (org-canvas--body-drawer-custom-id line-end drawer-end))))
    (delete-region start (or drawer-end line-end))
    (goto-char start)
    (unless (string-empty-p title)
      ;; A title shaped like a headline (`* starry') would be one
      ;; again, and indented it would be a list item; the entity
      ;; keeps the star as text.
      (when (string-match-p "\\`\\*+[ \t]" title)
        (setq title (concat "\\ast{}" (substring title 1))))
      (insert (format "#+begin_h%d\n%s%s\n#+end_h%d"
                      level (if id (format "<<%s>> " id) "") title level)))
    id))

(defun org-canvas--body-retarget-anchor-links (ids)
  "Point `[[#ID]' links in the current buffer at `<<ID>>' targets.
IDS are the CUSTOM_IDs whose headings became heading blocks: a block
cannot carry a CUSTOM_ID, so the anchor form of the link would dangle
where the fuzzy form resolves."
  (dolist (id ids)
    (goto-char (point-min))
    (let ((from (format "[[#%s]" id))
          (to (format "[[%s]" id)))
      (while (search-forward from nil t)
        (replace-match to t t)))))

(defun org-canvas--org-body-neutralize-headlines (text)
  "Turn every Org headline in TEXT into a `#+begin_hN' heading block.
TEXT is a converted body about to be inserted under an entry.  An
HTML heading that pandoc renders as an Org headline ends that entry
and begins another — the quiz in issue #175 lost five of its six
questions to one — so a body never carries a headline at any level:
every module's body extractor stops at the next heading, and quizzes,
new quizzes and outcomes read child headings as questions and
outcomes.  The block keeps the heading's level and text and exports
back to `<hN>' through `org-canvas--html-heading-block-filter'.
Returns TEXT itself when it carries no headline."
  (if (or (null text)
          (not (string-match-p org-canvas--body-headline-re text)))
      text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((ids nil))
        (while (re-search-forward org-canvas--body-headline-re nil t)
          (let ((id (org-canvas--body-replace-headline)))
            (when id (push id ids))))
        (org-canvas--body-retarget-anchor-links ids))
      (buffer-string))))

(defun org-canvas--strip-heading-block-markers (text)
  "Drop the `#+begin_hN' and `#+end_hN' lines of TEXT, keeping their text."
  (replace-regexp-in-string
   "^#\\+\\(?:begin\\|end\\)_h[1-6][ \t]*\\(?:\n\\|\\'\\)" "" text))

(defun org-canvas--html-to-org-pandoc (html)
  "Convert HTML to Org text through pandoc.
Returns the post-processed Org text, or the raw HTML behind a
warning line when pandoc exits non-zero."
  (with-temp-buffer
    (insert (org-canvas--html-strip-ids html))
    (let ((exit-code (call-process-region
                      (point-min) (point-max) "pandoc"
                      t t nil
                      "-f" "html" "-t" "org" "--wrap=none")))
      (if (= exit-code 0)
          (org-canvas--html-to-org-post-process
           (string-trim (buffer-string)))
        (concat "# WARNING: pandoc conversion failed\n" html)))))

(defun org-canvas--html-to-org (html)
  "Convert HTML string to Org format using pandoc.
Returns the Org-mode text, or the raw HTML prefixed with a warning
if pandoc is not available.  Output passes through
`org-canvas--html-to-org-post-process' to clean up NBSP characters,
whitespace-only lines, and missing spacing before inline timestamps,
and then, whichever path produced it, through
`org-canvas--org-body-neutralize-headlines': a body never carries an
Org headline (issue #175)."
  (org-canvas--org-body-neutralize-headlines
   (if (executable-find "pandoc")
       (org-canvas--html-to-org-pandoc html)
     (concat "# WARNING: pandoc not found, raw HTML below\n" html))))

(defun org-canvas--html-to-org-inline (html)
  "Convert HTML to Org and collapse to a single line.
Delegates to `org-canvas--html-to-org', then replaces newlines with spaces
and trims whitespace.  Suitable for table cells, list items, and heading
titles where multi-line output would break formatting.  A heading
block's marker lines are dropped first; its text stays.
Returns empty string for nil or empty HTML."
  (if (or (null html) (string-empty-p html))
      ""
    (string-trim
     (replace-regexp-in-string "[\n\r]+" " "
                               (org-canvas--strip-heading-block-markers
                                (org-canvas--html-to-org html))))))

(provide 'org-canvas-core-html)
;;; org-canvas-core-html.el ends here
