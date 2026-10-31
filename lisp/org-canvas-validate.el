;;; org-canvas-validate.el --- Validation engine for org-canvas  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Validates all course org files for property errors *before* syncing.
;; No Canvas API contact is required.
;;
;; Usage: M-x org-canvas-validate
;;
;; Output is displayed in a `*canvas-validate*' buffer using
;; `compilation-mode' so M-g n / M-g p jump to issues in source files.
;;
;; This file runs the checks and reports them.  The checks themselves
;; live beside it: `org-canvas-validate-issue' for the shared
;; vocabulary, `org-canvas-validate-content' and
;; `org-canvas-validate-grading' for the checking families.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-validate-issue)
(require 'org-canvas-validate-content)
(require 'org-canvas-validate-grading)
(require 'cl-lib)
(require 'compile)

;;;; 1. Duplicate and Superseded Titles (issue #164)
;;
;; Years of course copies leave a course full of near-twins.  On one
;; real course: 46 pages under 29 distinct titles, 131 rubrics under
;; about 40, `Homework Rubric' fifteen times over.  Duplicate detection
;; existed only for files, keyed on size and content-type, which is
;; where duplication matters least.
;;
;; The offline validator is the right place for the rest: it costs no
;; API call and runs on an inherited course before anything is touched.

(defconst org-canvas--validate-supersede-markers
  '(" *([^)]*\\(?:OLD\\|old\\|Old\\)[^)]*) *"
    "\\`\\(?:OLD DON'T USE\\|OLD DONT USE\\|DO NOT USE\\) *"
    "\\`[SsFf][0-9][0-9] +"
    " *([SsFf][0-9][0-9][^)]*) *"
    " *([0-9]+) *")
  "Regexps stripped from a title before two titles are compared.
Between them they cover what a course copy leaves behind: a
parenthetical carrying OLD, such as \"(OLD)\" or \"(s25 - old)\"; a
leading \"OLD DON'T USE\"; a semester stamp, leading \"S24 \" or
parenthesised \"(F23)\"; and Canvas's own \"(2)\" disambiguators, which
stack into \"Some Rubric (2) (3)\".")

(defun org-canvas--validate-normalize-title (title)
  "Return TITLE with course-copy debris stripped, for comparison.
Case and surrounding whitespace are dropped too, so \\\"Sprint 2 (S25 -
OLD)\\\" and \\\"Sprint 2\\\" compare equal.  Returns nil for a title that
normalizes to nothing."
  (when title
    (let ((s title))
      (dolist (re org-canvas--validate-supersede-markers)
        (setq s (replace-regexp-in-string re " " s)))
      (setq s (string-trim (replace-regexp-in-string " +" " " s)))
      (unless (string-empty-p s) (downcase s)))))

(defun org-canvas--validate-collect-titles (query)
  "Return the current buffer's entries grouped by normalized title.
QUERY is the spec's `org-map-entries' match, so only the level the
feature declares is read and a module item never joins the modules it
sits under.  A heading Canvas deleted is left out, by the rule the
push skips it with: it has its own warning, and pairing it with the
live twin it left behind reported an error Canvas no longer has (issue
#402).  Each group is (NORMALIZED . ENTRIES), where an entry is a
plist (:title :line :due) in document order."
  (let ((groups nil))
    (dolist (marker (org-map-entries (lambda () (point-marker)) query 'file))
      (goto-char (marker-position marker))
      (let* ((title (org-get-heading t t t t))
             (norm (org-canvas--validate-normalize-title
                    (org-link-display-format (or title "")))))
        (when (and norm (not (org-canvas--sync-deleted-stamp)))
          (let ((cell (assoc norm groups))
                (entry (list :title title
                             :line (line-number-at-pos)
                             :due (org-entry-get (point) "DUE_AT"))))
            (if cell
                (setcdr cell (append (cdr cell) (list entry)))
              (push (cons norm (list entry)) groups)))))
      (set-marker marker nil))
    (nreverse groups)))

(defun org-canvas--validate-duplicate-issue (file entries)
  "Return the issue for ENTRIES in FILE sharing a title, or nil.
Two headings that differ only in course-copy debris are a warning.
Two that also carry a `DUE_AT' are an error: a superseded twin left
scheduled is a gradebook hazard, and it is invisible in the Canvas UI
when the two sit in different assignment groups."
  (let* ((first (car entries))
         (loc (list :file file :line (plist-get first :line)
                    :heading (plist-get first :title)))
         (named (mapconcat (lambda (e)
                             (format "'%s' (line %d%s)"
                                     (plist-get e :title) (plist-get e :line)
                                     (if (plist-get e :due)
                                         (format ", due %s" (plist-get e :due))
                                       "")))
                           entries ", "))
         (scheduled (seq-filter (lambda (e) (plist-get e :due)) entries)))
    (if (> (length scheduled) 1)
        (org-canvas--validate-make-issue
         'error loc nil
         (format "%d entries share this title once OLD/semester/(n) markers are stripped, and %d of them carry a DUE_AT: %s — students see both; decide which is canonical"
                 (length entries) (length scheduled) named))
      (org-canvas--validate-make-issue
       'warning loc nil
       (format "%d entries share this title once OLD/semester/(n) markers are stripped: %s"
               (length entries) named)))))

(defun org-canvas--validate-duplicate-titles (file query)
  "Return issues for entries whose titles collide in the current buffer.
FILE names it for the report; QUERY selects the headings to compare.
Called after the per-entry checks, with the buffer current."
  (delq nil
        (mapcar (lambda (group)
                  (when (cdr (cdr group))
                    (org-canvas--validate-duplicate-issue file (cdr group))))
                (org-canvas--validate-collect-titles query))))

;;;; 2. Cross-Course Links (issue #172)

(defun org-canvas--validate-canvas-host ()
  "Return the host of `org-canvas-base-url', or nil when it is unset.
The cross-course scan anchors on the configured instance, so a link to
some other site is nobody's business here."
  (when (and (stringp org-canvas-base-url)
             (string-match "\\`https?://\\([^/]+\\)" org-canvas-base-url))
    (match-string 1 org-canvas-base-url)))

(defun org-canvas--validate-foreign-url-re (host)
  "Return a regexp matching a Canvas URL on HOST worth reporting.
Group 1, when set, is a course id — compared with `org-canvas-course-id'
by the caller.  Group 2, when set, names a top-level `users' or
`accounts' route, which no student can follow whichever course it
sits in.  A course-scoped route (=/courses/ID/users/...=) is group 1's
business, not group 2's, which is why the alternation anchors both
directly after the host."
  (format "https?://%s/\\(?:courses/\\([0-9]+\\)\\|\\(users\\|accounts\\)/[0-9]+\\)"
          (regexp-quote host)))

(defun org-canvas--validate-cross-course-issue (course-id route url file)
  "Build the warning for one foreign link, or nil when it points here.
COURSE-ID is the course the link names (nil for a ROUTE match), ROUTE
is \"users\" or \"accounts\", URL is the text that matched, and FILE
is the file being scanned.  Point is on the match."
  (let* ((heading (ignore-errors
                    (save-excursion (org-back-to-heading t)
                                    (org-get-heading t t t t))))
         (loc (list :file file :line (line-number-at-pos) :heading heading)))
    (cond
     ((and course-id (not (equal course-id (format "%s" org-canvas-course-id))))
      (append (org-canvas--validate-make-issue
               'warning loc nil
               (format "link into course %s, not this one: %s (students not enrolled there get a 404)"
                       course-id url))
              (list :cross-course course-id)))
     (route
      (append (org-canvas--validate-make-issue
               'warning loc nil
               (format "link to a Canvas %s route: %s (student-visible and broken)"
                       route url))
              (list :cross-course route))))))

(defun org-canvas--validate-cross-course-links (file)
  "Report links in FILE that point at another course on this instance.
A course copied forward carries links to the shell it came from.  They
resolve for the instructor, who is usually enrolled in both, and 404
for every student, and only the ones that happened to name a file id
were ever noticed — a link to another shell's *page* passed in silence
\(issue #172).  Pure string work over the Org file: no request is
made, so this runs on a read-only course and in batch like the rest.

Warnings rather than errors: a link to a shared department page, or to
a prerequisite course, is occasionally meant."
  (let ((host (org-canvas--validate-canvas-host))
        (issues nil))
    (when (and host org-canvas-course-id
               (not (string-empty-p (format "%s" org-canvas-course-id))))
      (let ((re (org-canvas--validate-foreign-url-re host)))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (when-let* ((issue (org-canvas--validate-cross-course-issue
                                 (match-string-no-properties 1)
                                 (match-string-no-properties 2)
                                 (match-string-no-properties 0)
                                 file)))
               (push issue issues)))))))
    (nreverse issues)))

(defun org-canvas--validate-cross-course-summary (issues)
  "Return a line grouping cross-course ISSUES by target, or nil.
The useful question about course-copy residue is which shell it came
from, not which link came first, so the targets are named once each."
  (let ((targets nil)
        (count 0))
    (dolist (issue issues)
      (when-let* ((target (plist-get issue :cross-course)))
        (setq count (1+ count))
        (unless (member target targets) (push target targets))))
    (when (> count 0)
      (setq targets (sort (nreverse targets) #'string<))
      (format "%d link(s) into %d other course(s) or account route(s): %s"
              count (length targets) (string-join targets ", ")))))

;;;; 3. Body Fragments From an Older Pull (issue #391)

(defun org-canvas--validate-body-fragments (file)
  "Report headings in FILE that look like body text an older pull stranded.
Before issue #175 a heading in a pulled body became an Org headline,
and the rest of that body a heading with no CANVAS_ID after the item.
A re-pull rewrites the item and leaves the fragment, which a push would
create as a new Canvas item: `org-canvas--body-fragment-reason' names
the mark that gave it away.  Every level is read, once per file, as the
cross-course scan is.  A warning, never push-only: the mirror is wrong
whatever happens next.  Nothing is changed."
  (let ((issues nil))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (when-let* ((reason (org-canvas--body-fragment-reason)))
            (push (org-canvas--validate-make-issue
                   'warning
                   (list :file file :line (line-number-at-pos)
                         :heading (org-get-heading t t t t))
                   nil
                   (format "no CANVAS_ID and %s: probably a body fragment from an older pull; a push will not create it without asking (fold it into the item above)"
                           reason))
                  issues)))
        t 'file)))
    (nreverse issues)))

;;;; 4. Body Links to Local Files a Push Cannot Resolve (issue #468)

(defun org-canvas--validate-body-line-p ()
  "Return non-nil when point is on a line a push exports as body text.
A heading line is not: a files.org or modules.org heading is itself a
file link, and is read as one by its own module.  Nor is a property
line."
  (save-excursion
    (beginning-of-line)
    (not (or (looking-at-p org-outline-regexp-bol)
             (looking-at-p org-property-re)))))

(defun org-canvas--validate-local-file-link-issue (target dir file)
  "Return the issue for the link to TARGET on this line of FILE, or nil.
DIR is FILE's directory.  Nil for a link another pass resolves and for
one the push sends as a Canvas URL."
  (when-let* ((path (org-canvas--local-file-link-path target))
              ((org-canvas--validate-body-line-p)))
    (let ((id (org-canvas--local-file-canvas-id path dir)))
      (unless (stringp id)
        (org-canvas--validate-push-only
         (org-canvas--validate-make-issue
          'warning
          (list :file file :line (line-number-at-pos)
                :heading (ignore-errors
                           (save-excursion (org-back-to-heading t)
                                           (org-get-heading t t t t))))
          nil
          (format "link to local file %s: %s; a push sends its text, not a link"
                  path (org-canvas--local-file-unresolved-reason id path dir))
          (eq id 'unsynced)))))))

(defun org-canvas--validate-local-file-links (file)
  "Report body links in FILE to local files a push cannot resolve.
A push sends a link to a local file other than an image or an Org file
as the Canvas URL of the files.org heading whose link target is the
same file (`org-canvas--resolve-file-links'), and one with no such
heading, or none with a CANVAS_ID, as plain text: a relative href
would 404 on Canvas.  A file not uploaded yet is pending-sync.  Both
only protect a push, so a read-only course does not hear of them."
  (let ((dir (file-name-directory (expand-file-name file)))
        (issues nil))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-canvas--local-file-link-re nil t)
         (when-let* ((issue (org-canvas--validate-local-file-link-issue
                             (match-string-no-properties 1) dir file)))
           (push issue issues)))))
    (nreverse issues)))

;;;; 5. Items No Module Holds (issue #413)

(defvar org-canvas-modules-file)

(defcustom org-canvas-validate-module-coverage
  '(org-canvas-assignments-file org-canvas-pages-file
    org-canvas-quizzes-file org-canvas-discussions-file)
  "Course files whose published headings validate expects a module to hold.
Students reach a course through its modules, so a published heading
no `org-canvas-modules-file' item links to is a note.  Unpublished
headings, the front page, hidden files and headings marked NO_MODULE
are left out.  Add `org-canvas-files-file' to check files too: left
out by default, since most files are images a page embeds.  Nil turns
the check off; it never runs when the modules file holds no item."
  :type '(repeat variable)
  :group 'org-canvas)

(defun org-canvas--validate-module-link-target (raw dir)
  "Return what the module item link in heading text RAW names, or nil.
DIR is the modules file's directory.  The answer is (heading FILE
HEADING) for a link to an Org heading, or (path FILE) for a link
straight at a file, FILE absolute.  An Org file that is missing falls
back to its basename in DIR, as the sync's resolver does."
  (let* ((org-link (string-match "\\[\\[file:\\([^]]+\\)::\\*\\(.+\\)\\]\\[" raw))
         (path (and (or org-link (string-match "\\[\\[file:\\([^]]+\\)\\]\\[" raw))
                    (match-string 1 raw)))
         (heading (and org-link (match-string 2 raw))))
    (cond
     (heading
      (let ((file (expand-file-name path dir)))
        (unless (file-exists-p file)
          (setq file (expand-file-name (file-name-nondirectory path) dir)))
        (list 'heading file
              (replace-regexp-in-string
               "\\\\[][]" (lambda (m) (substring m 1)) heading))))
     (path (list 'path (expand-file-name path dir))))))

(defun org-canvas--validate-heading-key (file pos)
  "Return the key naming the heading at POS in FILE."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (org-back-to-heading t)
      (format "%s:%d" (file-truename file) (point)))))

(defun org-canvas--validate-module-link-key (target)
  "Return the key of what TARGET, from the link reader, names, or nil."
  (let ((file (nth 1 target)))
    (if (eq (car target) 'path)
        (file-truename file)
      (when-let* ((pos (org-canvas--find-heading-in-file file (nth 2 target))))
        (org-canvas--validate-heading-key file pos)))))

(defun org-canvas--validate-module-linked-keys (modules-file)
  "Return a hash of what MODULES-FILE's items link to, or nil if none do.
Keys are `org-canvas--validate-heading-key' strings for a heading and
truenames for a file linked straight at.  An item Canvas deleted, or
one under a module it deleted, holds nothing (issue #402)."
  (let ((dir (file-name-directory modules-file))
        (raws nil)
        (keys (make-hash-table :test 'equal)))
    (with-current-buffer (org-canvas--find-file-noselect modules-file)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (when (and (not (org-canvas--sync-deleted-stamp))
                     (looking-at org-complex-heading-regexp)
                     (match-string-no-properties 4))
            (push (match-string-no-properties 4) raws)))
        "LEVEL=2" 'file)))
    (dolist (raw raws)
      (when-let* ((target (org-canvas--validate-module-link-target raw dir))
                  (key (org-canvas--validate-module-link-key target)))
        (puthash key t keys)))
    (and (> (hash-table-count keys) 0) keys)))

(defun org-canvas--validate-module-exempt-p ()
  "Return non-nil when the heading at point needs no module.
It is unpublished, the front page, a hidden file, marked NO_MODULE,
or deleted on Canvas."
  (or (equal (org-entry-get (point) "PUBLISHED") "false")
      (equal (org-entry-get (point) "FRONT_PAGE") "true")
      (equal (org-entry-get (point) "HIDDEN") "true")
      (equal (org-entry-get (point) "NO_MODULE") "true")
      (org-canvas--sync-deleted-stamp)))

(defun org-canvas--validate-module-entry-key (file files-p)
  "Return the key a module link to the heading at point in FILE would have.
FILES-P says FILE is the files file, whose file headings are links to
the file itself, which a module item may name directly; a folder
heading, with no link, answers nil."
  (if files-p
      (when (looking-at org-complex-heading-regexp)
        (when-let* ((heading (match-string-no-properties 4))
                    ((string-match "\\[\\[file:\\([^]]+\\)\\]" heading)))
          (file-truename (expand-file-name (match-string 1 heading)
                                           (file-name-directory file)))))
    (format "%s:%d" (file-truename file) (point))))

(defun org-canvas--validate-module-query (file-var)
  "Return the heading query of the validate spec for FILE-VAR's file."
  (or (plist-get (cl-find file-var (org-canvas--validate-specs)
                          :key (lambda (spec) (plist-get spec :file)))
                 :query)
      "LEVEL=1"))

(defun org-canvas--validate-module-orphans (file-var keys)
  "Return a note per published heading in FILE-VAR's file KEYS lacks.
For a file heading, a module link to the heading or to the file it
names both count."
  (let* ((file (expand-file-name (symbol-value file-var)))
         (files-p (eq file-var 'org-canvas-files-file))
         (issues nil))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (let ((key (org-canvas--validate-module-entry-key file files-p)))
            (unless (or (null key)
                        (org-canvas--validate-module-exempt-p)
                        (gethash key keys)
                        (and files-p (gethash (org-canvas--validate-heading-key
                                               file (point))
                                              keys)))
              (let ((heading (org-get-heading t t t t)))
                (push (org-canvas--validate-make-issue
                       'note
                       (list :file file :line (line-number-at-pos)
                             :heading heading)
                       nil
                       (format "'%s' is published but in no module: no modules.org item links to it (add one, or set NO_MODULE: t if it is kept out on purpose)"
                               heading))
                      issues)))))
        (org-canvas--validate-module-query file-var) 'file)))
    (nreverse issues)))

(defun org-canvas--validate-module-coverage ()
  "Return a note per published heading that no module item links to.
The files `org-canvas-validate-module-coverage' names are read against
the links in `org-canvas-modules-file'.  A published column that no
module holds is dated and graded and still missing from the Modules
page, and nothing else reports it (issue #413).  A note, not a
warning: nothing breaks and no value is wrong.  Nothing is returned
when the course keeps no modules file or its items link to nothing.
A finding about the course, never push-only; no request is made."
  (let* ((modules-file (and (boundp 'org-canvas-modules-file)
                            org-canvas-modules-file
                            (expand-file-name org-canvas-modules-file)))
         (keys (and org-canvas-validate-module-coverage
                    modules-file (file-exists-p modules-file)
                    (org-canvas--validate-module-linked-keys modules-file)))
         (issues nil))
    (when keys
      (dolist (var org-canvas-validate-module-coverage)
        (when (and (boundp var) (symbol-value var)
                   (file-exists-p (expand-file-name (symbol-value var))))
          (setq issues (nconc issues
                              (org-canvas--validate-module-orphans var keys))))))
    issues))

;;;; 6. Validation Engine

(defun org-canvas--validate-spec (spec)
  "Run validation for a single SPEC.
Returns a list of issues.

`:structural-fn' runs once per matched heading, with point on it.
`:file-fn' runs once for the whole file and receives its path — for
rules that only make sense across every entry at once, such as whether
the assignment-group weights sum to 100.

`:duplicate-titles' runs the course-copy twin check over the same
headings the query selected (issue #164)."
  (let* ((file-var (plist-get spec :file))
         (query (plist-get spec :query))
         (props (plist-get spec :properties))
         (date-order (plist-get spec :date-order))
         (structural-fn (plist-get spec :structural-fn))
         (file-fn (plist-get spec :file-fn))
         (duplicate-titles (plist-get spec :duplicate-titles))
         (org-canvas--validate-always-on-canvas (plist-get spec :always-on-canvas))
         (file (and (boundp file-var)
                    (expand-file-name (symbol-value file-var))))
         (issues nil))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (let ((markers (org-map-entries (lambda () (point-marker)) query 'file)))
            (dolist (marker markers)
              (goto-char (marker-position marker))
              (setq issues (nconc issues
                                  (org-canvas--validate-entry-at-marker
                                   props date-order structural-fn file))))
            (dolist (m markers) (set-marker m nil)))
          (when duplicate-titles
            (goto-char (point-min))
            (setq issues
                  (nconc issues
                         (org-canvas--validate-duplicate-titles file query))))))
      ;; File-level hooks take the path and read it themselves.
      (when file-fn
        (setq issues (nconc issues (funcall file-fn file)))))
    issues))

;;;; 7. Report Buffer and Mode

(defvar org-canvas-validate-mode-font-lock-keywords
  '(("^\\(.+\\):\\([0-9]+\\): \\(error\\): " (3 'compilation-error))
    ("^\\(.+\\):\\([0-9]+\\): \\(warning\\): " (3 'compilation-warning))
    ("^\\(.+\\):\\([0-9]+\\): \\(note\\): " (3 'compilation-info))
    ("^Validation complete:" . 'compilation-info)
    ("^=+$" . 'shadow)
    ("^Validating " . 'font-lock-function-name-face))
  "Font lock keywords for validation report buffer.")

(define-derived-mode org-canvas-validate-mode compilation-mode "Canvas-Validate"
  "Mode for org-canvas validation results.
\\<org-canvas-validate-mode-map>
Use \\[next-error] and \\[previous-error] to navigate issues."
  (setq-local compilation-error-regexp-alist
              '((org-canvas-validate
                 "^\\(.+\\):\\([0-9]+\\): error: " 1 2 nil 2)
                (org-canvas-validate-warn
                 "^\\(.+\\):\\([0-9]+\\): warning: " 1 2 nil 1)
                (org-canvas-validate-note
                 "^\\(.+\\):\\([0-9]+\\): note: " 1 2 nil 0)))
  (setq-local font-lock-defaults
              '(org-canvas-validate-mode-font-lock-keywords t)))

;;;; 8. Main Command

(defun org-canvas--validate-run-all-specs ()
  "Run all validation specs and collect issues.
The cross-course link scan runs once per distinct file rather than
once per spec: two features can register the same file (modules and
module items both name modules.org), and the scan reads the whole file
either way, so a per-spec call would report every foreign link twice
\(issue #172).  So do the body-fragment and local-file-link scans.
The module check reads the modules file against the content files
once, after the specs (issue #413).
Returns a plist (:issues ISSUES :checked N :skipped N)."
  (let ((all-issues nil)
        (files-checked 0)
        (files-skipped 0)
        (scanned nil))
    (dolist (spec (org-canvas--validate-specs))
      (let* ((file-var (plist-get spec :file))
             (file (and (boundp file-var)
                        (expand-file-name (symbol-value file-var)))))
        (if (and file (file-exists-p file))
            (progn
              (setq files-checked (1+ files-checked))
              (let ((issues (org-canvas--validate-spec spec)))
                (setq all-issues (nconc all-issues issues)))
              (unless (member file scanned)
                (push file scanned)
                (setq all-issues
                      (nconc all-issues
                             (org-canvas--validate-cross-course-links file)
                             (org-canvas--validate-body-fragments file)
                             (org-canvas--validate-local-file-links file)))))
          (setq files-skipped (1+ files-skipped)))))
    (setq all-issues (nconc all-issues (org-canvas--validate-module-coverage)))
    (list :issues all-issues :checked files-checked :skipped files-skipped)))

(defun org-canvas--validate-format-notes (note-count)
  "Return \", N note(s)\" for a positive NOTE-COUNT, else the empty string."
  (if (and note-count (> note-count 0))
      (format ", %d note(s)" note-count)
    ""))

(defun org-canvas--validate-format-summary (error-count warning-count
                                                        &optional note-count)
  "Return a summary message string for ERROR-COUNT and WARNING-COUNT.
NOTE-COUNT, when positive, is named after them."
  (let ((notes (org-canvas--validate-format-notes note-count)))
    (cond
     ((> error-count 0)
      (format "Validation: %d error(s), %d warning(s)%s"
              error-count warning-count notes))
     ((> warning-count 0)
      (format "Validation: %d warning(s), no errors%s" warning-count notes))
     ((> (or note-count 0) 0)
      (format "Validation: no errors or warnings%s" notes))
     (t
      "Validation passed: no issues found"))))

(defun org-canvas--validate-notes-last (issues)
  "Return ISSUES with the notes moved after everything else.
Each group keeps the order its findings were made in."
  (let ((note-p (lambda (i) (eq (org-canvas--validate-severity i) 'note))))
    (append (cl-remove-if note-p issues) (cl-remove-if-not note-p issues))))

(defun org-canvas--validate-insert-accepted (accepted count)
  "Insert the accepted findings: ACCEPTED listed with reasons, else COUNT.
ACCEPTED is nil unless the report shows everything; then each is
listed with its reason under a heading of its own.  Otherwise a
positive COUNT is named in one line (issue #538).  An accepted line
does not read as a `compilation-mode' error, warning or note."
  (cond
   (accepted
    (insert (format "\nAccepted (%d), each with the reason it is kept:\n"
                    (length accepted)))
    (dolist (issue accepted)
      (insert (format "%s:%d: accepted %s: %s\n    reason: %s\n"
                      (plist-get issue :file) (plist-get issue :line)
                      (plist-get issue :severity) (plist-get issue :message)
                      (plist-get issue :accepted)))))
   ((> count 0)
    (insert (format "%d finding(s) accepted with a reason (VALIDATE_ACCEPT, Accept cells); M-x org-canvas-validate-all lists them\n"
                    count)))))

(defun org-canvas--validate-insert-report (listed pending verbose stats
                                                  &optional accepted)
  "Insert the validation report into the current buffer.
LISTED are the issues printed individually, PENDING the pre-first-sync
link warnings, collapsed into one line unless VERBOSE.  Notes are
listed after the errors and warnings.  STATS is a plist (:errors
:warnings :notes :checked :skipped :suppressed :accepted); a non-zero
:suppressed count is named in a line of its own, so that holding push-only
findings back on a read-only course is visible rather than silent.
ACCEPTED, given when the report shows everything, lists the accepted
findings; otherwise only their count is named (issue #538).
Cross-course links are listed individually and then grouped by the
shell they point at, which is the question worth answering about
course-copy residue (issue #172)."
  (insert "org-canvas validation report\n")
  (insert (make-string 60 ?=))
  (insert "\n\n")
  (if (or listed pending)
      (progn
        ;; Notes after errors and warnings, each group in the order found.
        (dolist (issue (org-canvas--validate-notes-last listed))
          (insert (org-canvas--validate-format-issue issue))
          (insert "\n"))
        (when pending
          (if verbose
              (dolist (issue pending)
                (insert (org-canvas--validate-format-issue issue))
                (insert "\n"))
            (insert (format "%d link(s) pending first sync (targets have no CANVAS_ID yet); C-u M-x org-canvas-validate lists them\n"
                            (length pending))))))
    (insert "No issues found.\n"))
  (when-let* ((cross (org-canvas--validate-cross-course-summary
                      (append listed pending))))
    (insert cross)
    (insert "\n"))
  (when (> (plist-get stats :suppressed) 0)
    (insert (format "%d push-only finding(s) suppressed (org-canvas-read-only is set); M-x org-canvas-validate-all shows them\n"
                    (plist-get stats :suppressed))))
  (org-canvas--validate-insert-accepted accepted (or (plist-get stats :accepted) 0))
  (insert "\n")
  (insert (make-string 60 ?=))
  (insert "\n")
  (insert (format "Validation complete: %d error(s), %d warning(s)%s across %d file(s)"
                  (plist-get stats :errors) (plist-get stats :warnings)
                  (org-canvas--validate-format-notes (plist-get stats :notes))
                  (plist-get stats :checked)))
  (when pending
    (insert (format " (%d pending first sync)" (length pending))))
  (when (> (plist-get stats :skipped) 0)
    (insert (format " (%d file(s) not found, skipped)" (plist-get stats :skipped))))
  (insert "\n"))

;;;; 9. Findings as Data (issue #531)

(defun org-canvas--validate-collect (&optional all)
  "Run every check and return the findings of a validation report, as data.
A plist (:issues ISSUES :accepted ACCEPTED :stats STATS).  ISSUES are
the findings the report lists, pending-first-sync links among them; on
a course marked `org-canvas-read-only' the push-only ones are held
back unless ALL.  ACCEPTED are the findings a VALIDATE_ACCEPT or an
Accept cell took out of ISSUES (issue #538), each with its `:accepted'
reason.  STATS is (:errors :warnings :notes :checked :skipped
:suppressed :accepted), the severity counts taken over ISSUES.  The
read behind `org-canvas-validate' and `org-canvas-validate-json-data'."
  (let* ((result (org-canvas--validate-run-all-specs))
         (found (plist-get result :issues))
         (kept (if (and org-canvas-read-only (not all))
                   (cl-remove-if (lambda (i) (plist-get i :push-only)) found)
                 found))
         (accepted-p (lambda (i) (plist-get i :accepted)))
         (accepted (cl-remove-if-not accepted-p kept))
         (issues (cl-remove-if accepted-p kept)))
    (list :issues issues
          :accepted accepted
          :stats (list :errors (cl-count 'error issues
                                         :key #'org-canvas--validate-severity)
                       :warnings (cl-count 'warning issues
                                           :key #'org-canvas--validate-severity)
                       :notes (cl-count 'note issues
                                        :key #'org-canvas--validate-severity)
                       :checked (plist-get result :checked)
                       :skipped (plist-get result :skipped)
                       :suppressed (- (length found) (length kept))
                       :accepted (length accepted)))))

(defun org-canvas--validate-json-bool (value)
  "Return VALUE as `json-encode' writes a boolean."
  (if value t :json-false))

(defun org-canvas--validate-json-finding (issue)
  "Return ISSUE, a finding plist, as a JSON object alist."
  (list (cons 'severity (symbol-name (plist-get issue :severity)))
        (cons 'file (plist-get issue :file))
        (cons 'line (plist-get issue :line))
        (cons 'heading (plist-get issue :heading))
        (cons 'property (plist-get issue :property))
        (cons 'message (org-canvas--log-redact
                        (format "%s" (plist-get issue :message))))
        (cons 'pending_sync (org-canvas--validate-json-bool
                             (plist-get issue :pending-sync)))
        (cons 'push_only (org-canvas--validate-json-bool
                          (plist-get issue :push-only)))
        (cons 'check (and (plist-get issue :check)
                          (symbol-name (plist-get issue :check))))))

(defun org-canvas--validate-json-accepted (issue)
  "Return ISSUE, an accepted finding, as a JSON object alist with its reason."
  (append (org-canvas--validate-json-finding issue)
          (list (cons 'reason (plist-get issue :accepted)))))

(defun org-canvas-validate-json-data (&optional all)
  "Return the validation report as an alist for `json-encode'.
The findings `org-canvas-validate' would report, ALL as there, notes
last, every pending-first-sync link listed and marked; the counts are
the report's.  The accepted findings are counted, and listed under
`accepted_findings' only with ALL, an empty list otherwise (issue
#538).  Nothing is displayed.  The schema is the manual's
\"Machine-Readable Output\" (issue #531)."
  (let* ((data (org-canvas--validate-collect all))
         (stats (plist-get data :stats)))
    (list (cons 'command "validate")
          (cons 'course org-canvas-directory)
          (cons 'all (org-canvas--validate-json-bool all))
          (cons 'counts (list (cons 'error (plist-get stats :errors))
                              (cons 'warning (plist-get stats :warnings))
                              (cons 'note (plist-get stats :notes))))
          (cons 'files_checked (plist-get stats :checked))
          (cons 'files_skipped (plist-get stats :skipped))
          (cons 'suppressed (plist-get stats :suppressed))
          (cons 'accepted (plist-get stats :accepted))
          (cons 'findings
                (vconcat (mapcar #'org-canvas--validate-json-finding
                                 (org-canvas--validate-notes-last
                                  (plist-get data :issues)))))
          (cons 'accepted_findings
                (vconcat (and all
                              (mapcar #'org-canvas--validate-json-accepted
                                      (plist-get data :accepted))))))))

;;;###autoload
(defun org-canvas-validate (&optional verbose all)
  "Validate all course org files without contacting the Canvas API.
Checks property types, enum values, date ordering, and structural
requirements across all 12 content types.

Results are displayed in a `*canvas-validate*' buffer with
`compilation-mode' navigation (\\[next-error] / \\[previous-error]),
and printed to standard output under `noninteractive' — validate makes
no API calls, so a batch Emacs is the natural place to run it, and a
batch run used to print a tally naming no file (issue #169).

Warnings about link targets that merely lack a CANVAS_ID (expected
state before the first sync) are collapsed into a single summary
line.  With a prefix argument VERBOSE, list them individually.

On a course marked `org-canvas-read-only', findings that exist only to
protect a push are held back and counted in one line; non-nil ALL
keeps them, which is what `org-canvas-validate-all' passes (issue
#168).

Returns the number of errors reported, so a batch caller can act on
it; see `org-canvas-validate-batch'."
  (interactive "P")
  (let* ((data (org-canvas--validate-collect all))
         (all-issues (plist-get data :issues))
         (pending-issues (cl-remove-if-not
                          (lambda (i) (plist-get i :pending-sync)) all-issues))
         (listed-issues (cl-remove-if
                         (lambda (i) (plist-get i :pending-sync)) all-issues))
         (stats (plist-get data :stats))
         (error-count (plist-get stats :errors))
         (warning-count (plist-get stats :warnings))
         (note-count (plist-get stats :notes)))
    (org-canvas--report-display
     "*canvas-validate*"
     (lambda ()
       (org-canvas--validate-insert-report
        listed-issues pending-issues verbose stats
        (and all (plist-get data :accepted))))
     #'org-canvas-validate-mode)
    (message "%s" (org-canvas--validate-format-summary
                   error-count warning-count note-count))
    error-count))

;;;###autoload
(defun org-canvas-validate-all (&optional verbose)
  "Validate every course org file, holding nothing back.
Same as `org-canvas-validate' except that findings which only protect
a push are reported even on a course marked `org-canvas-read-only' —
the list to read when you are about to clear that flag and adopt the
course for real (issue #168).  VERBOSE lists the pre-first-sync link
warnings individually, as it does there."
  (interactive "P")
  (org-canvas-validate verbose t))

;;;###autoload
(defun org-canvas-validate-batch ()
  "Run `org-canvas-validate', then exit non-zero if it found any error.
Intended for a pre-commit hook or a CI step over a course directory,
invoked from a batch-mode Emacs with -f org-canvas-validate-batch.
Validation contacts no API, so this needs no token.  Mirrors
`org-canvas-diff-batch'."
  (kill-emacs (if (> (org-canvas-validate) 0) 1 0)))

(provide 'org-canvas-validate)
;;; org-canvas-validate.el ends here
