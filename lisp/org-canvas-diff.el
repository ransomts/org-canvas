;;; org-canvas-diff.el --- Read-only drift report against Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Answers the question `org-canvas-status' sounds like it answers but
;; does not: does Canvas currently match my Org files?
;;
;; `org-canvas-status' is entirely local — it reports what the Org files
;; say about themselves.  `org-canvas-sync-dry-run' answers a third
;; question, which entries have a locally changed PAYLOAD_HASH.  Neither
;; can see a change made only in the Canvas web UI, so before this the
;; only ways to find remote drift were to run a real sync and read the
;; conflict prompts, or to query the API by hand and diff (issue #51).
;;
;; WHAT IT REPORTS
;; ===============
;; Per feature, four kinds of divergence:
;;
;;   Modified remotely  the item's `updated_at' is newer than the
;;                      baseline recorded at the last push or pull
;;   Missing on Canvas  a heading carries a Canvas id that the course
;;                      no longer has
;;   Only on Canvas     an item in the course that no Org heading claims
;;   Unclaimed          such an item whose title an unstamped heading
;;                      shares — a create that lost its stamp (#85)
;;
;; plus, for every property the module registers, the differing values
;; themselves — published, points, due dates, group weights and so on —
;; and, for modules that declare a `:body-api-key', the body, compared
;; as text (#83).
;;
;; SCOPE OF THE FIELD COMPARISON
;; =============================
;; Only properties actually written in the Org file are compared.  An
;; absent property means the file expresses no opinion, and each module
;; applies its own default at parse time, so treating absence as a value
;; would invent differences that no sync would act on.  A spec may also
;; name a `:compare-p' predicate of (POM ITEM) saying whether the
;; property is a comparable opinion at all: Canvas drops ALL_DAY on a
;; calendar event spanning days — the times are kept, the flag is not —
;; so comparing it would flag the entry on every run, forever (#93).
;;
;; ACKNOWLEDGED AND EXCLUDED (#98)
;; ===============================
;; A synced course can still hold remote objects that are unclaimed by
;; design — Canvas scaffolding, features authored in the web UI on
;; purpose, deliberate one-offs — and a report that flags them forever
;; buries the one extra that matters and pins `org-canvas-diff-batch'
;; at a non-zero exit.  Three valves, from broad to narrow: a feature's
;; `:skip-fn' suppresses scaffolding structurally (counted in the
;; footer, the #81 way); `org-canvas-diff-excluded-features' skips a
;; feature wholesale, printing one line so the exclusion stays visible
;; (its bodies are still read for referenced media, since excluding a
;; feature from the rows should not blind that scan — #111);
;; `org-canvas-diff-known-extras' acknowledges individual ids, counted
;; once in the footer — and flagged loudly, and counted as drift, when
;; an acknowledged id stops existing, so the list cannot rot.  A fourth
;; valve is automatic (#102): a file that course content embeds is
;; referenced media, counted in the footer, so an attachment uploaded
;; through the rich-content editor does not need acknowledging by hand.
;;
;; COST AND SAFETY
;; ===============
;; One list request per feature.  Nothing is written, locally or
;; remotely, so this is safe to run at any time — from a hook, or from a
;; scheduled job via `org-canvas-diff-batch', which exits non-zero when
;; it finds drift.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(declare-function org-canvas-pull-at-point "org-canvas")

(defconst org-canvas--diff-buffer-name "*canvas-diff*"
  "Name of the buffer holding the drift report.")

(defcustom org-canvas-diff-excluded-features nil
  "Feature names `org-canvas-diff' does not check at all.
For content that is ad hoc by design — a course whose announcements
are authored in the web UI on purpose will never have Org headings for
them, and reporting each one as EXTRA forever buries real drift
\(issue #98).  Names match the way the registry matches them, so
\"announcements\" and \"Announcements\" both work.  The report prints
one line per excluded feature, so the exclusion stays visible.
An excluded feature that has a body is still listed once, without
producing rows, so the media its content embeds is recognized rather
than reported as unclaimed files (issue #111); the line says so.
Exclusion affects the drift report only."
  :type '(repeat string)
  :group 'org-canvas)

(defcustom org-canvas-diff-known-extras nil
  "Remote items acknowledged as deliberately unclaimed.
Each entry is (FEATURE-NAME ID) or (FEATURE-NAME ID NOTE) — e.g.
\(\"files\" \"31505574\" \"embedded announcement media\").  Acknowledged
ids are excluded from the EXTRA rows and from the divergence total
that drives `org-canvas-diff-batch''s exit code, and reported once as
a counted footer line.  An acknowledged id that no longer exists on
Canvas is flagged as STALE-ACK — and does count as drift — so the
list cannot rot (issue #98).  Acknowledgment affects the drift report
only: orphan cleanup and prune still list these items, which is where
to look if one should be deleted after all."
  :type '(repeat (list (string :tag "Feature")
                       (string :tag "Canvas id")
                       (choice (const :tag "No note" nil)
                               (string :tag "Note"))))
  :group 'org-canvas)

(defcustom org-canvas-diff-scan-references t
  "Whether the drift report treats media embedded in course content as claimed.
Every image or PDF the rich-content editor uploads lands in Uploaded
Media as a file no Org heading claims, and telling a *referenced*
attachment apart from a stray upload used to mean fetching content
bodies by hand and searching them for the file id (issue #102).  When
non-nil, the report scans the bodies it already has — assignments,
pages, announcements, discussions and quizzes are listed with their
bodies for the body comparison — plus the syllabus, one extra request,
for `/files/<id>' references, and reports unclaimed files that are
referenced as a counted footer line instead of EXTRA rows.  An
unreferenced unclaimed file keeps alarming, which is exactly right; a
file whose referent disappears reverts to EXTRA on its own.  Affects
the drift report only."
  :type 'boolean
  :group 'org-canvas)

(defconst org-canvas--diff-comparable-types
  '(boolean number timestamp enum csv-enum string)
  "Property types the field comparison understands.
`link' is excluded: the Org side is a heading link and the Canvas side
an id, so comparing them would need the same resolution a sync does.")

;;;; Registry Lookup

(defun org-canvas--diff-normalize-name (name)
  "Return NAME lowercased with spaces, underscores and dashes removed.
The same normalization `org-canvas--registry-find-feature' applies, so
every spelling of a feature name means the same feature."
  (downcase (replace-regexp-in-string "[ _-]" "" name)))

(defun org-canvas--diff-find-properties (feature-name)
  "Return the property registry entry for FEATURE-NAME, or nil.
Matches the way `org-canvas--registry-find-feature' does, so the
feature registry's \"Assignment Groups\" finds the property registry's
\"assignment-groups\"."
  (let ((norm (org-canvas--diff-normalize-name feature-name))
        found)
    (maphash (lambda (key plist)
               (when (and (not found)
                          (string= (org-canvas--diff-normalize-name key) norm))
                 (setq found plist)))
             org-canvas--property-registry)
    found))

(defun org-canvas--diff-feature-excluded-p (name)
  "Return non-nil when feature NAME is excluded from the drift report.
Consults `org-canvas-diff-excluded-features', matching names the way
the registry does (issue #98)."
  (let ((norm (org-canvas--diff-normalize-name name)))
    (cl-some (lambda (excluded)
               (string= norm (org-canvas--diff-normalize-name excluded)))
             org-canvas-diff-excluded-features)))

(defun org-canvas--diff-known-extras-for (name)
  "Return feature NAME's entries of `org-canvas-diff-known-extras'.
Each as (ID . NOTE), the id as a string (issue #98)."
  (let ((norm (org-canvas--diff-normalize-name name)))
    (delq nil
          (mapcar (lambda (entry)
                    (when (string= norm (org-canvas--diff-normalize-name
                                         (nth 0 entry)))
                      (cons (format "%s" (nth 1 entry)) (nth 2 entry))))
                  org-canvas-diff-known-extras))))

;;;; Value Comparison

(defalias 'org-canvas--diff-remote-field #'org-canvas--registry-remote-field
  "Return ITEM's value for the Canvas field described by SPEC.
The registry-driven pull reads the same function (core-org), so the
report and the pull can never disagree about which field a property
means (issue #135).")

(defalias 'org-canvas--diff-normalize-remote
  #'org-canvas--registry-normalize-remote
  "Return VALUE with Canvas's JSON null and false spellings folded to nil.")

(defalias 'org-canvas--diff-remote-list #'org-canvas--registry-remote-list
  "Return REMOTE as a list of strings, however Canvas spelled it (issue #63).")

(defun org-canvas--diff-values-equal-p (type local remote)
  "Return non-nil when LOCAL (an Org string) and REMOTE agree, given TYPE."
  (let ((remote (org-canvas--diff-normalize-remote remote)))
    (pcase type
      ('boolean (eq (string= local "true") (and remote t)))
      ('number (and remote (= (string-to-number local)
                              (if (stringp remote) (string-to-number remote) remote))))
      ('timestamp
       (let ((local-iso (org-canvas-org-parse-timestamp local)))
         (and local-iso remote
              (equal (org-canvas--parse-iso8601-time local-iso)
                     (org-canvas--parse-iso8601-time remote)))))
      ('csv-enum
       (equal (sort (split-string (or local "") "," t "[ \t]+") #'string<)
              (sort (org-canvas--diff-remote-list remote) #'string<)))
      (_ (equal local (and remote (format "%s" remote)))))))

(defun org-canvas--diff-format-remote (type remote)
  "Return a printable form of REMOTE for TYPE."
  (let ((remote (org-canvas--diff-normalize-remote remote)))
    (pcase type
      ('boolean (if remote "true" "false"))
      ('csv-enum (mapconcat #'identity (org-canvas--diff-remote-list remote) ","))
      (_ (if remote (format "%s" remote) "(unset)")))))

(defun org-canvas--diff-compare-fields (specs pom item)
  "Compare the properties at POM against Canvas ITEM using SPECS.
Returns a list of (ORG-PROP LOCAL REMOTE) for the ones that differ.
Only properties actually present in the Org file are considered — see
the commentary on why absence is not a value — and a spec's
`:compare-p' predicate, when it declares one, can rule the property
out for this entry: ALL_DAY on a multi-day calendar event cannot
round-trip, so it is no one's opinion to compare (issue #93)."
  (let (diffs)
    (dolist (spec specs)
      (let ((type (plist-get spec :type))
            (compare-p (plist-get spec :compare-p))
            (org-prop (plist-get spec :org-prop)))
        (when (and (memq type org-canvas--diff-comparable-types)
                   (not (plist-get spec :local-only))
                   (or (null compare-p) (funcall compare-p pom item)))
          (let ((local (org-entry-get pom org-prop)))
            (when (and local (not (string-empty-p local)))
              (let ((remote (org-canvas--diff-remote-field spec item)))
                (unless (org-canvas--diff-values-equal-p type local remote)
                  (push (list org-prop local
                              (org-canvas--diff-format-remote type remote))
                        diffs))))))))
    (nreverse diffs)))

;;;; Body Comparison
;;
;; The description is the one field a student actually reads, and the
;; one field the property specs never mention: modules set it directly
;; on the payload, outside the registry, so the report gave a rewritten
;; body a clean bill of health (issue #83).  Canvas does not hand back
;; the HTML it was given — it prepends a stylesheet link, rewrites
;; attributes, normalizes whitespace and entities — so the two sides
;; are compared as the text a reader sees.  A difference in markup
;; alone is invisible here, which is the price of never reporting one
;; a student cannot see.

(defconst org-canvas--diff-named-entities
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"")
    ("apos" . "'") ("nbsp" . " "))
  "Named HTML entities decoded when comparing bodies as text.")

(defun org-canvas--diff-decode-entity (entity)
  "Return the text ENTITY (\"&amp;\", \"&#39;\", \"&#x2019;\") stands for.
An entity this does not know is kept as written, so both sides still
agree when they spell it the same way."
  (let ((name (substring entity 1 -1)))
    (cond
     ((assoc name org-canvas--diff-named-entities)
      (cdr (assoc name org-canvas--diff-named-entities)))
     ((string-match "\\`#[xX]\\([0-9a-fA-F]+\\)\\'" name)
      (string (string-to-number (match-string 1 name) 16)))
     ((string-match "\\`#\\([0-9]+\\)\\'" name)
      (string (string-to-number (match-string 1 name))))
     (t entity))))

(defun org-canvas--diff-html-to-text (html)
  "Return HTML as normalized plain text.
Tags are dropped, entities decoded and whitespace collapsed, so two
renderings of the same text compare equal however they were marked
up.  Anything that is not a string reads as \"\"."
  (if (not (stringp html))
      ""
    (let* ((s (replace-regexp-in-string
               "<\\(script\\|style\\)\\(?:.\\|\n\\)*?</\\1>" " " html))
           (s (replace-regexp-in-string "<[^>]*>" " " s))
           (s (replace-regexp-in-string
               "&#?[[:alnum:]]+;"
               (lambda (m) (save-match-data (org-canvas--diff-decode-entity m)))
               s))
           ;; An explicit class, not [:space:]: that one goes by the current
           ;; buffer's syntax table, and in a Lisp buffer newline is not space.
           (s (replace-regexp-in-string "[ \t\n\r\f\u00a0]+" " " s)))
      (string-trim s))))

(defun org-canvas--diff-text-excerpts (local remote &optional width)
  "Return excerpts of LOCAL and REMOTE around their first difference.
Each is WIDTH characters (default 40), starting a little before the
point where the two stop agreeing; an empty side reads \"(empty)\"."
  (let* ((width (or width 40))
         (n (min (length local) (length remote)))
         (i 0))
    (while (and (< i n) (eq (aref local i) (aref remote i)))
      (setq i (1+ i)))
    (let ((start (max 0 (- i 10))))
      (mapcar (lambda (s)
                (if (string-empty-p s)
                    "(empty)"
                  (let ((end (min (length s) (+ start width))))
                    (concat (if (> start 0) "…" "")
                            (substring s start end)
                            (if (< end (length s)) "…" "")))))
              (list local remote)))))

(defun org-canvas--diff-local-body-html (pom body-fn)
  "Return the HTML the entry at POM would push as its body, or nil.
BODY-FN, when non-nil, is the module's own extractor (a quiz's
description is only the text before its first question); otherwise
the shared subtree exporter runs offline.  Offline matters: the full
exporter resolves inline images by uploading the ones Canvas lacks,
and a report must not write.  Links change only markup, which the
text comparison ignores anyway.  Nil, with a warning, when the export
fails, so a broken body is reported as unchecked rather than changed."
  (condition-case err
      (with-current-buffer (marker-buffer pom)
        (save-excursion
          (goto-char pom)
          (if body-fn
              (funcall body-fn)
            (org-canvas--export-subtree-body-to-html t))))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Diff] Could not export the body at %s: %s" pom (error-message-string err))
     nil)))

(defun org-canvas--diff-compare-body (props pom item)
  "Compare the body at POM against Canvas ITEM, as text.
PROPS is the feature's property registry entry: `:body-api-key' names
the Canvas field (description, body, message) and the optional
`:body-fn' the local extractor.  Returns nil when the feature has no
body, ITEM does not carry the field, or the local export failed —
none of which is drift — and otherwise (t . ROW), where ROW is nil
when the two agree and (LABEL LOCAL REMOTE), with excerpts around the
first difference, when they do not."
  (let* ((api-key (plist-get props :body-api-key))
         (cell (and api-key (assq (intern api-key) item)))
         (html (and cell (org-canvas--diff-local-body-html
                          pom (plist-get props :body-fn)))))
    (when html
      (let ((local (org-canvas--diff-html-to-text html))
            (remote (org-canvas--diff-html-to-text
                     (org-canvas--diff-normalize-remote (cdr cell)))))
        (cons t (unless (string= local remote)
                  (cons (upcase api-key)
                        (org-canvas--diff-text-excerpts local remote))))))))

;;;; Referenced Media
;;
;; A file id appears in course content as `/files/<id>' — in a download
;; link, a preview `src', a `data-api-endpoint' — whichever host and
;; course path precede it.  The bodies are already in hand: the body
;; comparison lists every body-bearing feature with its body (#83), so
;; only the syllabus costs a request.

(defun org-canvas--diff-file-references (html)
  "Return the file ids `/files/<id>' references in HTML, as strings, deduplicated.
Anything that is not a string yields nil."
  (when (stringp html)
    (let ((ids nil) (start 0))
      (while (string-match "/files/\\([0-9]+\\)" html start)
        (cl-pushnew (match-string 1 html) ids :test #'string=)
        (setq start (match-end 0)))
      (nreverse ids))))

(defun org-canvas--diff-items-references (items api-key)
  "Return the file ids the API-KEY bodies of ITEMS reference, deduplicated."
  (let ((ids nil))
    (dolist (item items)
      (dolist (id (org-canvas--diff-file-references
                   (org-canvas--diff-normalize-remote
                    (alist-get (intern api-key) item))))
        (cl-pushnew id ids :test #'string=)))
    (nreverse ids)))

(defun org-canvas--diff-syllabus-references ()
  "Return the file ids the course syllabus references, or nil.
One request; a failure is logged and reads as no references, so the
report still comes out."
  (condition-case err
      (org-canvas--diff-file-references
       (org-canvas--diff-normalize-remote
        (alist-get 'syllabus_body
                   (org-canvas-api-request
                    'GET (org-canvas-api-course-endpoint "")
                    :params '(("include[]" . "syllabus_body"))))))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Diff] Could not read the syllabus for file references: %s"
       (error-message-string err))
     nil)))

(defun org-canvas--diff-scan-excluded-references (feature props body-api-key)
  "Return the file ids the BODY-API-KEY bodies of excluded FEATURE reference.
PROPS is FEATURE's registered property spec.  Excluding a feature from
the rows must not blind the reference scan (issue #111).  Announcements
are the case it was written for: the body feature most often authored
in the web UI and kept out of the report, whose every attached flyer
lands in Uploaded Media, where it reads as a Files EXTRA that only a
hand-written `org-canvas-diff-known-extras' entry could quiet.  Costs
one read-only list request; a failed request is logged and reads as no
references, so the report still comes out."
  (condition-case err
      (org-canvas--diff-items-references
       (append (org-canvas-api-request-all-pages
                'GET (org-canvas--feature-list-url feature)
                (append (org-canvas--feature-list-params feature)
                        (plist-get props :body-list-params)))
               nil)
       body-api-key)
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Diff] Could not read %s for file references: %s"
       (plist-get feature :name) (error-message-string err))
     nil)))

(defun org-canvas--diff-excluded-result (feature)
  "Return the report entry for excluded FEATURE.
A feature with a body still has its bodies read, so
`org-canvas--diff-apply-references' can acknowledge the media they
embed (issue #111); `:references-scanned' records that it happened, so
the report can say so beside the exclusion line."
  (let* ((name (plist-get feature :name))
         (props (org-canvas--diff-find-properties name))
         (body-api-key (and org-canvas-diff-scan-references
                            (plist-get props :body-api-key))))
    (list :name name :excluded t
          :references-scanned (and body-api-key t)
          :referenced-files
          (and body-api-key
               (org-canvas--diff-scan-excluded-references
                feature props body-api-key)))))

(defun org-canvas--diff-apply-references (results)
  "Move referenced files out of the Files result's extras in RESULTS.
Collects every `:referenced-files' the body features recorded, plus
the syllabus's, and turns each Files extra whose id is among them into
a `:referenced' count rather than a row (issue #102).  Does nothing
when `org-canvas-diff-scan-references' is off, or when Files was not
checked.  Returns RESULTS, mutated in place."
  (let ((files (cl-find-if (lambda (r)
                             (string= (org-canvas--diff-normalize-name
                                       (plist-get r :name))
                                      "files"))
                           results)))
    (when (and org-canvas-diff-scan-references files
               (not (plist-get files :excluded))
               (not (plist-get files :error)))
      (let ((refs (org-canvas--diff-syllabus-references)))
        (dolist (r results)
          (dolist (id (plist-get r :referenced-files))
            (cl-pushnew id refs :test #'string=)))
        (let* ((extra (plist-get files :extra))
               (referenced (cl-remove-if-not
                            (lambda (e) (member (plist-get e :id) refs)) extra)))
          (plist-put files :extra (cl-set-difference extra referenced))
          (plist-put files :referenced (length referenced)))))
    results))

;;;; Per-Feature Comparison
;;;; Per-Feature Comparison

(defun org-canvas--diff-collect-local (file query id-property)
  "Collect entries from FILE matching QUERY, keyed by ID-PROPERTY.
Returns a list of plists (:id ID :title TITLE :pom MARKER).  The
position is a marker, not an offset: the comparison reads properties
long after this buffer stops being current, and a bare offset would
silently be read against whatever buffer happened to be."
  (when (and file (file-exists-p file))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-map-entries
       (lambda ()
         (list :id (org-entry-get (point) id-property)
               :title (org-get-heading t t t t)
               :pom (point-marker)))
       (or query "LEVEL=1") 'file))))

(defun org-canvas--diff-remote-index (items id-field)
  "Return a hash of ITEMS keyed by their ID-FIELD, as strings."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((id (alist-get id-field item)))
        (when id (puthash (format "%s" id) item index))))
    index))

(defun org-canvas--diff-entry (entry index specs &optional props modified-field)
  "Compare local ENTRY against the remote INDEX using property SPECS.
PROPS, the feature's property registry entry, enables the body
comparison when it names a `:body-api-key' (issue #83).
MODIFIED-FIELD names the remote timestamp \"modified remotely\" is
decided from, default `updated_at'; files pass `modified_at' (issue
#94).  Returns a plist describing the divergence, or nil when they
agree."
  (let* ((id (plist-get entry :id))
         (pom (plist-get entry :pom))
         (item (and id (gethash id index))))
    (cond
     ((null id) nil)
     ((null item)
      (list :kind 'missing :title (plist-get entry :title) :id id))
     (t
      (let* ((body (org-canvas--diff-compare-body props pom item))
             (fields (append (org-canvas--diff-compare-fields specs pom item)
                             (and (cdr body) (list (cdr body)))))
             (drifted (org-canvas--diff-modified-p pom item modified-field)))
        (when (or fields drifted)
          (list :kind 'modified :title (plist-get entry :title) :id id
                :updated (alist-get (or modified-field 'updated_at) item)
                :remote-newer drifted :fields fields
                :body-compared (and body t))))))))

(defun org-canvas--diff-modified-p (pom item &optional modified-field)
  "Return non-nil when ITEM was modified after POM's baseline.
MODIFIED-FIELD names the timestamp compared, default `updated_at'.
Files compare `modified_at', the content timestamp — Canvas bumps
their `updated_at' on metadata-only touches, which is not drift
\(issue #94)."
  (let ((baseline (org-canvas--conflict-baseline pom))
        (remote (org-canvas--parse-iso8601-time
                 (alist-get (or modified-field 'updated_at) item))))
    (and baseline remote (time-less-p baseline remote))))

(defun org-canvas--diff-unclaimed (items claimed id-field title-field skip-fn)
  "Sort remote ITEMS no local heading CLAIMED into extras and suppressions.
ID-FIELD and TITLE-FIELD name the item alist keys.  SKIP-FN, when
non-nil, marks an item the feature never compares.  Returns a cons of
the extra entry list and the count SKIP-FN held back — a skipped item
is neither drift nor extra, but it is also not checked, and saying so
is what keeps the report from reading as full coverage (issue #81)."
  (let ((extra nil)
        (suppressed 0))
    (dolist (item items)
      (unless (member (format "%s" (alist-get id-field item)) claimed)
        (if (and skip-fn (funcall skip-fn item))
            (cl-incf suppressed)
          (push (list :kind 'extra
                      :title (format "%s" (or (alist-get title-field item) "?"))
                      :id (format "%s" (alist-get id-field item))
                      ;; Where RET on the row can take a reader (#103).
                      :html-url (org-canvas--diff-normalize-remote
                                 (alist-get 'html_url item)))
                extra))))
    (cons (nreverse extra) suppressed)))

(defun org-canvas--diff-pair-unclaimed (extra local id-property)
  "Pair each EXTRA remote item with an unstamped LOCAL heading of its title.
A partial create — the POST succeeded, the stamp was never written —
leaves exactly this pair: a remote item nothing claims and, a few
lines away, a heading of the same name with no ID-PROPERTY.  Reported
apart they are a two-line mystery, and the next sync creates a second
item (issue #85).  Such an entry becomes kind `unclaimed' and names
the property to stamp; the rest of EXTRA is returned as it was."
  (let ((unstamped (delq nil (mapcar (lambda (e)
                                       (and (null (plist-get e :id))
                                            (plist-get e :title)))
                                     local))))
    (mapcar (lambda (e)
              (if (member (plist-get e :title) unstamped)
                  (list :kind 'unclaimed :title (plist-get e :title)
                        :id (plist-get e :id) :property id-property
                        :html-url (plist-get e :html-url))
                e))
            extra)))

(defun org-canvas--diff-feature (feature)
  "Compare one FEATURE registry entry against Canvas.
Returns a plist (:name :divergences :extra :acknowledged :error):
:extra holds remote items no Org heading claims — minus the ones
`org-canvas-diff-known-extras' acknowledges, whose count is
:acknowledged — and :error a message when the list request failed.
An acknowledged id Canvas no longer holds joins :divergences as a
`stale-ack' entry, so the acknowledgment list cannot rot (issue #98)."
  (let* ((name (plist-get feature :name))
         (file-var (plist-get feature :file-var))
         (file (and (boundp file-var) (symbol-value file-var)))
         (id-property (plist-get feature :id-property))
         (id-field (plist-get feature :id-field))
         (title-field (plist-get feature :title-field))
         (props (org-canvas--diff-find-properties name))
         (specs (plist-get props :properties))
         (modified-field (org-canvas--feature-modified-field feature))
         (skip-fn (plist-get feature :skip-fn)))
    (condition-case err
        (let* ((items (append (org-canvas-api-request-all-pages
                               'GET (org-canvas--feature-list-url feature)
                               ;; e.g. include[]=body — the pages list
                               ;; omits bodies unless asked (issue #83)
                               (append (org-canvas--feature-list-params feature)
                                       (plist-get props :body-list-params)))
                              nil))
               (index (org-canvas--diff-remote-index items id-field))
               (local (org-canvas--diff-collect-local
                       file (plist-get props :query) id-property))
               (claimed (delq nil (mapcar (lambda (e) (plist-get e :id)) local)))
               divergences)
          (dolist (entry local)
            (let ((d (org-canvas--diff-entry entry index specs props
                                             modified-field)))
              (when d (push d divergences)))
            (let ((m (plist-get entry :pom)))
              (when (markerp m) (set-marker m nil))))
          (let* ((unclaimed (org-canvas--diff-unclaimed
                             items claimed id-field title-field skip-fn))
                 (paired (org-canvas--diff-pair-unclaimed
                          (car unclaimed) local id-property))
                 (known (org-canvas--diff-known-extras-for name))
                 (acked (cl-remove-if-not
                         (lambda (e) (assoc (plist-get e :id) known))
                         paired))
                 (stale (cl-remove-if (lambda (k) (gethash (car k) index))
                                      known)))
            (list :name name
                  :divergences (append (nreverse divergences)
                                       (mapcar (lambda (k)
                                                 (list :kind 'stale-ack
                                                       :id (car k)
                                                       :note (cdr k)))
                                               stale))
                  :extra (cl-remove-if (lambda (e) (memq e acked)) paired)
                  :acknowledged (length acked)
                  :suppressed (cdr unclaimed)
                  :skip-reason (plist-get feature :skip-reason)
                  ;; The bodies are in hand; note what media they embed
                  ;; for the files result to consult (issue #102).
                  :referenced-files
                  (and org-canvas-diff-scan-references
                       (plist-get props :body-api-key)
                       (org-canvas--diff-items-references
                        items (plist-get props :body-api-key))))))
      (error
       (list :name name :error (error-message-string err))))))

;;;; Report

(defun org-canvas--diff-insert-entry (entry)
  "Insert one ENTRY of a drift report at point."
  (pcase (plist-get entry :kind)
    ('missing
     (insert (format "  MISSING   %s (id %s is not in this course)\n"
                     (plist-get entry :title) (plist-get entry :id))))
    ('extra
     (insert (format "  EXTRA     %s (id %s, no Org heading claims it)\n"
                     (plist-get entry :title) (plist-get entry :id))))
    ('stale-ack
     (insert (format "  STALE-ACK id %s is acknowledged in org-canvas-diff-known-extras but no longer exists on Canvas — remove the entry%s\n"
                     (plist-get entry :id)
                     (let ((note (plist-get entry :note)))
                       (if note (format " (%s)" note) "")))))
    ('unclaimed
     (insert (format "  UNCLAIMED %s (Canvas id %s has this title and no heading claims it; adopt it with org-canvas-adopt-at-point, which stamps %s, or rename)\n"
                     (plist-get entry :title) (plist-get entry :id)
                     (or (plist-get entry :property) "CANVAS_ID"))))
    ('modified
     (insert (format "  CHANGED   %s%s\n"
                     (plist-get entry :title)
                     (if (plist-get entry :remote-newer)
                         (format " (Canvas updated %s)" (plist-get entry :updated))
                       "")))
     (dolist (field (plist-get entry :fields))
       (insert (format "              %-18s org: %-24s canvas: %s\n"
                       (nth 0 field) (nth 1 field) (nth 2 field))))
     ;; A timestamp-only change used to be a puzzle: every compared
     ;; field matched, so the row could not say what had changed.
     (when (and (plist-get entry :remote-newer) (null (plist-get entry :fields)))
       (insert (format "              (no compared property differs; the change is in a field this report does not compare — e.g. %s)\n"
                       (if (plist-get entry :body-compared)
                           "overrides or a rubric association"
                         "the description or overrides")))))))

(defun org-canvas--diff-insert-row (feature-name entry)
  "Insert ENTRY of FEATURE-NAME's report at point, as an actionable row.
The text carries an `org-canvas-diff-row' property naming the feature
and the entry, which is what the report's keys act on (issue #103).
Batch output prints the same text, properties and all invisible."
  (let ((start (point)))
    (org-canvas--diff-insert-entry entry)
    (put-text-property start (point) 'org-canvas-diff-row
                       (list :feature feature-name :entry entry))))

(defun org-canvas--diff-count (results)
  "Return the total number of divergences across RESULTS."
  (apply #'+ (mapcar (lambda (r) (+ (length (plist-get r :divergences))
                                    (length (plist-get r :extra))))
                     results)))

(defun org-canvas--diff-suppressed-note (results)
  "Return a line naming remote items no check in RESULTS covered, or nil.
Items a `:skip-fn' holds back are not compared and cannot
show up as extra, so without this the report reads as full coverage."
  (let ((parts (delq nil
                     (mapcar
                      (lambda (r)
                        (let ((n (plist-get r :suppressed)))
                          (when (and n (> n 0))
                            (format "%d %s (%s)" n (plist-get r :name)
                                    (or (plist-get r :skip-reason)
                                        "excluded by this module")))))
                      results))))
    (when parts
      (format "Not checked: %s.\n" (mapconcat #'identity parts ", ")))))

(defun org-canvas--diff-render (results)
  "Render RESULTS into a report string."
  (with-temp-buffer
    (insert "org-canvas Drift Report\n")
    (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
    (insert (make-string 60 ?=) "\n\n")
    (dolist (result results)
      (let ((divergences (plist-get result :divergences))
            (extra (plist-get result :extra))
            (err (plist-get result :error)))
        (cond
         ((plist-get result :excluded)
          (insert (format "%s: not checked (org-canvas-diff-excluded-features)%s\n\n"
                          (plist-get result :name)
                          (if (plist-get result :references-scanned)
                              "; bodies read for referenced media only"
                            ""))))
         (err
          (insert (format "%s: could not check (%s)\n\n"
                          (plist-get result :name) err)))
         ((and (null divergences) (null extra)))
         (t
          (insert (format "%s: %d divergence(s)\n"
                          (plist-get result :name)
                          (+ (length divergences) (length extra))))
          (dolist (entry (append divergences extra))
            (org-canvas--diff-insert-row (plist-get result :name) entry))
          (insert "\n")))))
    (let ((total (org-canvas--diff-count results)))
      (insert (make-string 60 ?=) "\n")
      (insert (if (zerop total)
                  "No drift: Canvas matches the Org files.\n"
                (format "%d divergence(s) found.\n" total)))
      (when-let* ((note (org-canvas--diff-suppressed-note results)))
        (insert note))
      (let ((acked (apply #'+ (mapcar (lambda (r)
                                        (or (plist-get r :acknowledged) 0))
                                      results))))
        (when (> acked 0)
          (insert (format "Acknowledged extras: %d (org-canvas-diff-known-extras).\n"
                          acked))))
      (let ((referenced (apply #'+ (mapcar (lambda (r)
                                             (or (plist-get r :referenced) 0))
                                           results))))
        (when (> referenced 0)
          (insert (format "Referenced media: %d unclaimed file%s embedded in course content (org-canvas-diff-scan-references).\n"
                          referenced (if (= referenced 1) "" "s"))))))
    (buffer-string)))

;;;; Acting on the Report
;;
;; The report named problems and offered no verbs: visiting the heading,
;; acknowledging an extra, deleting a stray remote object, pulling one
;; item all happened somewhere else, by hand — a one-off elisp script
;; around a DELETE, a drawer edit, a line appended to a config file, a
;; search for the heading (issue #103).  Each action's machinery already
;; existed; only the rows could not reach it.  Rows carry their entry as
;; a text property, and these commands act on the row at point.

(defcustom org-canvas-diff-acknowledge-function #'org-canvas--diff-acknowledge-save
  "Function that persists `org-canvas-diff-known-extras' after the report edits it.
Called with the new value of the whole list, after the variable itself
has been set, when `a' on a report row acknowledges an extra or drops
a stale acknowledgment.  The default saves through `customize-save-variable';
a course that keeps its configuration in a file of its own can name a
function that writes the list there instead."
  :type 'function
  :group 'org-canvas)

(defun org-canvas--diff-acknowledge-save (extras)
  "Save EXTRAS as `org-canvas-diff-known-extras' with Customize."
  (customize-save-variable 'org-canvas-diff-known-extras extras))

(defvar org-canvas-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-canvas-diff-visit)
    (define-key map (kbd "a") #'org-canvas-diff-acknowledge)
    (define-key map (kbd "k") #'org-canvas-diff-delete)
    (define-key map (kbd "p") #'org-canvas-diff-pull)
    (define-key map (kbd "g") #'org-canvas-diff-refresh)
    (define-key map (kbd "TAB") #'org-canvas-diff-next-row)
    (define-key map (kbd "<backtab>") #'org-canvas-diff-previous-row)
    map)
  "Keymap for `org-canvas-diff-mode'.")

(define-derived-mode org-canvas-diff-mode special-mode "Canvas-Diff"
  "Major mode for the drift report, with actions on its rows.
\\<org-canvas-diff-mode-map>
\\[org-canvas-diff-visit] visits the Org heading of the row at point, or
opens the Canvas object of an EXTRA row in a browser;
\\[org-canvas-diff-acknowledge] acknowledges the EXTRA at point (or drops a
STALE-ACK); \\[org-canvas-diff-delete] deletes the remote object of an EXTRA
or UNCLAIMED row, after confirming; \\[org-canvas-diff-pull] pulls a CHANGED
row's item over the heading; \\[org-canvas-diff-refresh] runs the report
again; \\[org-canvas-diff-next-row] and \\[org-canvas-diff-previous-row]
move between rows."
  (setq-local revert-buffer-function (lambda (&rest _) (org-canvas-diff))))

(defconst org-canvas--diff-key-legend
  "RET visit   a acknowledge   k delete   p pull   g refresh   TAB next row\n"
  "One line naming the report's keys, shown in the interactive buffer only.")

(defun org-canvas--diff-row-at-point ()
  "Return the (:feature NAME :entry ENTRY) plist of the row at point.
Signals a `user-error' off any row."
  (or (get-text-property (point) 'org-canvas-diff-row)
      (user-error "No report row at point")))

(defun org-canvas--diff-row-feature (row)
  "Return the feature registry entry for ROW, or signal."
  (or (org-canvas--registry-find-feature (plist-get row :feature))
      (user-error "%s is not a registered feature" (plist-get row :feature))))

(defun org-canvas--diff-rewrite-row (text)
  "Replace the row at point with TEXT, keeping its row property.
TEXT is one line without a newline.  How an action leaves its mark
without re-running every list request."
  (let* ((row (org-canvas--diff-row-at-point))
         (inhibit-read-only t)
         (start (line-beginning-position))
         (end (line-end-position)))
    (delete-region start end)
    (goto-char start)
    (insert text)
    (put-text-property start (point) 'org-canvas-diff-row row)
    (goto-char start)))

(defun org-canvas--diff-find-heading-by-id (file id-property id)
  "Return the position of the heading in FILE whose ID-PROPERTY is ID, or nil."
  (when (and file (file-exists-p file))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (catch 'found
          (org-map-entries
           (lambda ()
             (when (equal (org-entry-get (point) id-property) id)
               (throw 'found (point))))
           nil 'file)
          nil)))))

(defun org-canvas--diff-heading-position (feature entry)
  "Return (FILE . POSITION) of the heading ENTRY of FEATURE describes, or nil.
MISSING and CHANGED rows are found by their id property; an UNCLAIMED
row names an unstamped heading, found by title."
  (let* ((file-var (plist-get feature :file-var))
         (file (and file-var (boundp file-var) (symbol-value file-var)))
         (pos (pcase (plist-get entry :kind)
                ('unclaimed
                 (org-canvas--find-heading-in-file file (plist-get entry :title)))
                ((or 'missing 'modified)
                 (org-canvas--diff-find-heading-by-id
                  file (or (plist-get feature :id-property) "CANVAS_ID")
                  (plist-get entry :id))))))
    (and pos (cons file pos))))

(defun org-canvas--diff-goto-heading (feature entry)
  "Make the buffer of ENTRY's heading current, with point on it, or signal.
FEATURE is the registry entry that says which file to look in.
Returns the buffer."
  (let ((where (org-canvas--diff-heading-position feature entry)))
    (unless where
      (user-error "Cannot find the heading for '%s' in %s"
                  (plist-get entry :title) (plist-get feature :name)))
    (set-buffer (org-canvas--find-file-noselect (car where)))
    (goto-char (cdr where))
    (org-back-to-heading t)
    (current-buffer)))

(defun org-canvas-diff-visit ()
  "Visit what the row at point is about.
A row with an Org heading (MISSING, CHANGED, UNCLAIMED) opens the
course file with point on that heading.  An EXTRA row has no heading,
so its Canvas object is opened in a browser when the API said where
it lives."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (feature (org-canvas--diff-row-feature row)))
    (pcase (plist-get entry :kind)
      ((or 'missing 'modified 'unclaimed)
       (let ((buf (save-excursion (org-canvas--diff-goto-heading feature entry)))
             (pos nil))
         (with-current-buffer buf (setq pos (point)))
         (pop-to-buffer buf)
         (goto-char pos)
         (when (fboundp 'org-fold-show-context) (org-fold-show-context))))
      (_
       (let ((url (plist-get entry :html-url)))
         (unless url
           (user-error "Canvas did not say where %s %s lives; look it up by id"
                       (plist-get feature :name) (plist-get entry :id)))
         (browse-url url))))))

(defun org-canvas-diff-acknowledge ()
  "Acknowledge the EXTRA or UNCLAIMED row at point as deliberately unclaimed.
Adds it to `org-canvas-diff-known-extras', with a note if you give one,
and persists the list through `org-canvas-diff-acknowledge-function'.
On a STALE-ACK row, drops the acknowledgment instead."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (feature (plist-get row :feature))
         (id (plist-get entry :id)))
    (pcase (plist-get entry :kind)
      ('stale-ack
       (setq org-canvas-diff-known-extras
             (cl-remove-if (lambda (k)
                             (and (string= (org-canvas--diff-normalize-name (nth 0 k))
                                           (org-canvas--diff-normalize-name feature))
                                  (string= (format "%s" (nth 1 k)) id)))
                           org-canvas-diff-known-extras))
       (funcall org-canvas-diff-acknowledge-function org-canvas-diff-known-extras)
       (org-canvas--diff-rewrite-row
        (format "  DROPPED   acknowledgment of id %s (%s)" id feature))
       (message "Dropped the acknowledgment of %s %s." feature id))
      ((or 'extra 'unclaimed)
       (let* ((note (read-string (format "Note for %s '%s' (optional): "
                                         feature (plist-get entry :title))))
              (note (and (not (string-empty-p note)) note)))
         (setq org-canvas-diff-known-extras
               (append org-canvas-diff-known-extras (list (list feature id note))))
         (funcall org-canvas-diff-acknowledge-function org-canvas-diff-known-extras)
         (org-canvas--diff-rewrite-row
          (format "  ACK       %s (id %s acknowledged%s)"
                  (plist-get entry :title) id (if note (format ": %s" note) "")))
         (message "Acknowledged %s %s." feature id)))
      (kind (user-error "A %s row is not something to acknowledge" (upcase (symbol-name kind)))))))

(defun org-canvas-diff-delete ()
  "Delete the EXTRA or UNCLAIMED row's Canvas object, after confirming.
Uses the feature's item URL and delete body, as orphan cleanup does."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (feature (org-canvas--diff-row-feature row))
         (id (plist-get entry :id))
         (name (plist-get feature :name)))
    (unless (memq (plist-get entry :kind) '(extra unclaimed))
      (user-error "Only an EXTRA or UNCLAIMED row names a remote object to delete"))
    (when (y-or-n-p (format "Delete %s '%s' (id %s) from Canvas? "
                            name (plist-get entry :title) id))
      (let ((delete-data (plist-get feature :delete-data)))
        (apply #'org-canvas-api-request 'DELETE
               (org-canvas--feature-item-url feature id)
               (and delete-data (list :data delete-data))))
      (org-canvas--log-info org-canvas--logger
        "[Diff] Deleted %s #%s '%s' from the report" name id (plist-get entry :title))
      (org-canvas--diff-rewrite-row
       (format "  DELETED   %s (id %s)" (plist-get entry :title) id))
      (message "Deleted %s %s." name id))))

(defun org-canvas-diff-pull ()
  "Pull the item of the CHANGED row at point over its Org heading.
Runs `org-canvas-pull-at-point' on the heading, which asks first."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (feature (org-canvas--diff-row-feature row)))
    (unless (eq (plist-get entry :kind) 'modified)
      (user-error "Only a CHANGED row has a Canvas version to pull"))
    (let ((pulled (save-excursion
                    (with-current-buffer (org-canvas--diff-goto-heading feature entry)
                      (org-canvas-pull-at-point)
                      t))))
      (when pulled
        (org-canvas--diff-rewrite-row
         (format "  PULLED    %s (id %s)" (plist-get entry :title) (plist-get entry :id)))))))

(defun org-canvas-diff-refresh ()
  "Run the drift report again."
  (interactive)
  (org-canvas-diff))

(defun org-canvas--diff-row-start (pos)
  "Return where the row run containing POS begins.
A CHANGED row spans several lines, all carrying the same property
value; its start is the line the verb is on."
  (let ((row (get-text-property pos 'org-canvas-diff-row)))
    (while (and (> pos (point-min))
                (eq (get-text-property (1- pos) 'org-canvas-diff-row) row))
      (setq pos (1- pos)))
    pos))

(defun org-canvas-diff-next-row ()
  "Move point to the next report row."
  (interactive)
  (let* ((row (get-text-property (point) 'org-canvas-diff-row))
         (pos (point))
         (found nil))
    (while (and row (< pos (point-max))
                (eq (get-text-property pos 'org-canvas-diff-row) row))
      (setq pos (1+ pos)))
    (while (and (not found) (< pos (point-max)))
      (if (get-text-property pos 'org-canvas-diff-row)
          (setq found pos)
        (setq pos (1+ pos))))
    (if found (goto-char found) (user-error "No more rows"))))

(defun org-canvas-diff-previous-row ()
  "Move point to the previous report row."
  (interactive)
  (let ((pos (if (get-text-property (point) 'org-canvas-diff-row)
                 (org-canvas--diff-row-start (point))
               (point)))
        (found nil))
    (while (and (not found) (> pos (point-min)))
      (setq pos (1- pos))
      (when (get-text-property pos 'org-canvas-diff-row)
        (setq found pos)))
    (if found
        (goto-char (org-canvas--diff-row-start found))
      (user-error "No previous row"))))

;;;; Commands
;;;; Commands

;;;###autoload
(defun org-canvas-diff ()
  "Report how Canvas differs from the Org files, without changing anything.
Makes one read-only list request per feature and compares ids, remote
modification times and the properties each module registers.  Features
in `org-canvas-diff-excluded-features' are skipped with a visible
line — their bodies are still read for the media they embed (issue
#111) — and ids in `org-canvas-diff-known-extras' are counted as
acknowledged rather than reported (issue #98); with
`org-canvas-diff-scan-references' on, unclaimed files that course
content embeds are counted rather than reported too (issue #102).
Returns the number of
divergences found, so a batch caller can act on it; see
`org-canvas-diff-batch'."
  (interactive)
  (org-canvas--preflight-check)
  (let (results)
    (dolist (feature org-canvas--feature-registry)
      (let ((name (plist-get feature :name)))
        (if (org-canvas--diff-feature-excluded-p name)
            (push (org-canvas--diff-excluded-result feature) results)
          (message "Drift: checking %s..." name)
          (push (org-canvas--diff-feature feature) results))))
    (setq results (org-canvas--diff-apply-references (nreverse results)))
    (let ((report (org-canvas--diff-render results))
          (total (org-canvas--diff-count results)))
      (if noninteractive
          (princ report)
        (with-current-buffer (get-buffer-create org-canvas--diff-buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert report)
            (insert "\n" org-canvas--diff-key-legend)
            (goto-char (point-min)))
          (org-canvas-diff-mode)
          (display-buffer (current-buffer)))
        (message "Drift: %d divergence(s)" total))
      total)))

;;;###autoload
(defun org-canvas-diff-batch ()
  "Run `org-canvas-diff', then exit non-zero if there was any drift.
Intended for scheduled jobs, invoked from a batch-mode Emacs with
-f org-canvas-diff-batch."
  (kill-emacs (if (> (org-canvas-diff) 0) 1 0)))

(provide 'org-canvas-diff)
;;; org-canvas-diff.el ends here
