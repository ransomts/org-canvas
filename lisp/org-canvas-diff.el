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
;; Per feature, five kinds of divergence:
;;
;;   Modified remotely  the item's `updated_at' is newer than the
;;                      baseline recorded at the last push or pull
;;   Missing on Canvas  a heading carries a Canvas id that the course
;;                      no longer has
;;   Only on Canvas     an item in the course that no Org heading claims
;;   Unclaimed          such an item whose title an unstamped heading
;;                      shares — a create that lost its stamp (#85)
;;   Moved              a module item whose title an unstamped heading
;;                      under another module shares (#294)
;;
;; plus, for every property the module registers, the differing values
;; themselves — published, points, due dates, group weights and so on —
;; and, for modules that declare a `:body-api-key', the body, compared
;; as text (#83).  A declared intent (`:intent-of', e.g.
;; WANT_DOCUMENT_PROCESSOR) is held against what Canvas holds, and the
;; reverse — Canvas holds one, the file declares none — is a NOTE row,
;; shown but not counted as drift (#293).
;;
;; And, counted apart because it is not drift, every heading with no
;; Canvas id that the next sync would create: a PENDING row (#294);
;; and every stamped module item heading whose id Canvas holds in
;; another module, which the next sync recreates under a new id and
;; deletes from the old one: a RELOCATE row (#343).
;; New Quizzes, which list from the quiz service and so stay out of the
;; feature registry, take part through their pull-only entry (#313),
;; and their items in a section of their own (#322).
;; An EXTRA assignment or group says what it is, from the list replies
;; already held, and a WEIGHTS line under the groups sets Org's weight
;; total against Canvas's when they differ, uncounted (#296).
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
;; it finds drift (pending creates are not drift).  The two local writes
;; in this file, stamp adoption (#257) and stamping a MOVED row's item
;; id on its heading (#342), are separate keys and commands the report
;; never runs itself, and neither sends anything to Canvas.
;; So is deleting a row's Canvas object: `k' one row at a time, and
;; `org-canvas-diff-delete-rows' for a batch (#345), both after safety
;; checks and a JSON snapshot of the object.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(declare-function org-canvas-pull-at-point "org-canvas")
(declare-function org-canvas--pull-new-heading "org-canvas" (feature id title))
;; The Module Items pass asks the sync's own parser what an unstamped
;; item heading links, and the sync's own predicate whether a Canvas
;; item holds it (issue #299).  Declared, never required: diff is a
;; command file above the feature modules, and `org-canvas' loads both.
(declare-function org-canvas--module-item-parse-entry "org-canvas-modules"
                  (modules-file-dir))
(declare-function org-canvas--module-item-same-content-p "org-canvas-modules"
                  (data item))
;; The New Quiz Items pass lists a quiz's items at the quiz service, and
;; pairs an unstamped item heading as the sync's twin adoption does
;; (issue #322).  Declared for the same reason: the sub-module is New
;; Quizzes' own, and `org-canvas' loads it.
(declare-function org-canvas--new-quiz-api-endpoint "org-canvas-new-quiz-items"
                  (suffix &rest args))
(declare-function org-canvas--new-quiz-item-twin-p "org-canvas-new-quiz-items"
                  (data item))
(declare-function org-canvas--new-quiz-item-remote-title "org-canvas-new-quiz-items"
                  (item))

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
list cannot rot (issue #98).  A module item is acknowledged under
\"module-items\", the section its rows report in — an item id and a
module id are different things (issue #177).  Acknowledgment affects
the drift report only: orphan cleanup and prune still list these
items, which is where to look if one should be deleted after all."
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

(defun org-canvas--diff-features ()
  "Return the entries the drift report compares, in report order.
Every feature registry entry, then each pull-only entry that declares
`:drift-report' (`org-canvas-register-pull-feature'): New Quizzes list
from the quiz service, so they stay out of the registry the orphan
scan and prune read, and join the report here instead (issue #313)."
  (append org-canvas--feature-registry
          (cl-remove-if-not (lambda (f) (plist-get f :drift-report))
                            org-canvas--pull-feature-registry)))

(defun org-canvas--diff-find-feature (name)
  "Return the entry of `org-canvas--diff-features' named NAME, or nil.
Names match the way the registry matches them."
  (let ((norm (org-canvas--diff-normalize-name name)))
    (cl-find-if (lambda (f)
                  (string= norm (org-canvas--diff-normalize-name
                                 (plist-get f :name))))
                (org-canvas--diff-features))))

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

(defun org-canvas--diff-time-minute (time)
  "Return TIME as a count of whole minutes since the epoch.
An Org timestamp names a minute and nothing finer, so a comparison
against one can claim no more precision than this.  The seconds are
dropped, not tolerated: xx:59 and xx+1:00 are different minutes in the
file even though they are a second apart (issue #176)."
  (floor (time-convert time 'integer) 60))

(defun org-canvas--diff-values-equal-p (type local remote)
  "Return non-nil when LOCAL (an Org string) and REMOTE agree, given TYPE.
Timestamps agree when they fall in the same minute: a deadline set in
the Canvas web UI carries :59 seconds, which the Org side can neither
store nor pull, so an exact comparison was a DUE_AT row that nothing
but a push could clear (issue #176)."
  (let ((remote (org-canvas--diff-normalize-remote remote)))
    (pcase type
      ('boolean (eq (string= local "true") (and remote t)))
      ('number (and remote (= (string-to-number local)
                              (if (stringp remote) (string-to-number remote) remote))))
      ('timestamp
       (let ((local-time (org-canvas--parse-iso8601-time
                          (org-canvas-org-parse-timestamp local)))
             (remote-time (org-canvas--parse-iso8601-time remote)))
         (and local-time remote-time
              (= (org-canvas--diff-time-minute local-time)
                 (org-canvas--diff-time-minute remote-time)))))
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

(defun org-canvas--diff-observed-spec (spec specs)
  "Return the spec in SPECS that intent SPEC's `:intent-of' names, or nil."
  (let ((observed (plist-get spec :intent-of)))
    (seq-find (lambda (s) (equal (plist-get s :org-prop) observed)) specs)))

(defun org-canvas--diff-intent-remote-text (observed-spec item)
  "Return how the drift report spells the value Canvas ITEM gives OBSERVED-SPEC.
The observed value when there is one; otherwise whether Canvas said
\"none\" or said nothing at all.  An instance can omit the field from
every read — one course's assignment GET carried no `asset_processors'
key with or without an `include[]' (issue #293) — and a row reading
\"(none)\" there would claim more than the API told us.  Whether
Canvas answered is the spec's own say
\(`org-canvas--registry-remote-present-p'): the processors come by
GraphQL first, and a course-wide read that answered is a \"none\"
even with no REST key (issue #350)."
  (let ((remote (org-canvas--diff-normalize-remote
                 (org-canvas--diff-remote-field observed-spec item))))
    (cond (remote (format "%s" remote))
          ((org-canvas--registry-remote-present-p observed-spec item)
           "(none; attach it in the web UI, B opens the edit page, then pull)")
          (t "(not reported by Canvas)"))))

(defun org-canvas--diff-compare-intent (spec specs pom item)
  "Compare the intent SPEC declares at POM with the value in Canvas ITEM.
SPECS is the feature's spec list, where the observed property named
by SPEC's `:intent-of' is found.  Returns (ORG-PROP LOCAL REMOTE) when
the heading declares an intent Canvas does not satisfy, else nil:
silence declares nothing, and Canvas holding a processor nobody asked
for is a note, not drift (`org-canvas--diff-entry-notes')."
  (let* ((local (org-entry-get pom (plist-get spec :org-prop)))
         (observed-spec (org-canvas--diff-observed-spec spec specs)))
    (when (and observed-spec local (not (string-empty-p (string-trim local))))
      (let ((remote (org-canvas--diff-normalize-remote
                     (org-canvas--diff-remote-field observed-spec item))))
        (unless (org-canvas--intent-satisfied-p local remote)
          (list (plist-get spec :org-prop) (string-trim local)
                (org-canvas--diff-intent-remote-text observed-spec item)))))))

(defun org-canvas--diff-compare-field (spec pom item)
  "Compare one ordinary property SPEC at POM against Canvas ITEM.
Returns (ORG-PROP LOCAL REMOTE) when they differ, else nil.  See
`org-canvas--diff-compare-fields' for which specs take part."
  (let ((type (plist-get spec :type))
        (compare-p (plist-get spec :compare-p))
        (org-prop (plist-get spec :org-prop)))
    (when (and (memq type org-canvas--diff-comparable-types)
               (not (plist-get spec :local-only))
               (or (null compare-p) (funcall compare-p pom item)))
      (let* ((written (org-entry-get pom org-prop))
             (local (and written (not (string-empty-p written)) written)))
        (when (or local (plist-get spec :canvas-owned))
          (let ((remote (org-canvas--diff-remote-field spec item)))
            (unless (if local
                        (org-canvas--diff-values-equal-p type local remote)
                      (null (org-canvas--diff-normalize-remote remote)))
              (list org-prop (or local "(unset)")
                    (org-canvas--diff-format-remote type remote)))))))))

(defun org-canvas--diff-compare-fields (specs pom item)
  "Compare the properties at POM against Canvas ITEM using SPECS.
Returns a list of (ORG-PROP LOCAL REMOTE) for the ones that differ.
Only properties actually present in the Org file are considered — see
the commentary on why absence is not a value — and a spec's
`:compare-p' predicate, when it declares one, can rule the property
out for this entry: ALL_DAY on a multi-day calendar event cannot
round-trip, so it is no one's opinion to compare (issue #93).

The exception is a `:canvas-owned' spec, compared whether or not the
heading carries it: the file cannot hold an opinion on a field only
Canvas may set, so its silence is not one, and a document processor
attached in the web UI after the last pull is exactly what the report
exists to show (issue #184).  Such a row reads \"(unset)\" on the
local side.

An `:intent-of' spec is compared against the observed property it
names, not against a field of its own: WANT_DOCUMENT_PROCESSOR differs
when Canvas holds no processor its text matches (issue #293)."
  (delq nil
        (mapcar (lambda (spec)
                  (if (plist-get spec :intent-of)
                      (org-canvas--diff-compare-intent spec specs pom item)
                    (org-canvas--diff-compare-field spec pom item)))
                specs)))

(defun org-canvas--diff-entry-notes (entry index specs)
  "Return the uncounted NOTE rows for local ENTRY against the remote INDEX.
SPECS is the feature's spec list.  A note is an `:intent-of' property
the heading leaves unset while Canvas holds the observation it would
declare — Turnitin attached to a column no WANT_DOCUMENT_PROCESSOR
asks for.  That is a legitimate state the file has simply not adopted,
so it is shown and not counted as drift (issue #293)."
  (let* ((id (plist-get entry :id))
         (pom (plist-get entry :pom))
         (item (and id (gethash id index)))
         notes)
    (when item
      (dolist (spec specs)
        (when-let* ((observed-spec (and (plist-get spec :intent-of)
                                        (org-canvas--diff-observed-spec spec specs)))
                    ((not (org-entry-get pom (plist-get spec :org-prop))))
                    (remote (org-canvas--diff-normalize-remote
                             (org-canvas--diff-remote-field observed-spec item))))
          (push (list :kind 'note :title (plist-get entry :title) :id id
                      :property (plist-get spec :org-prop)
                      :observed (plist-get observed-spec :org-prop)
                      :remote (format "%s" remote))
                notes))))
    (nreverse notes)))

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

(defun org-canvas--diff-remote-body (props item)
  "Return (FIELD . HTML), the body of Canvas ITEM, or nil.
PROPS is the property registry entry: its `:body-remote-fn', when it
names one, reads a body the reply nests (a New Quiz item's, issue
#322); otherwise the `:body-api-key' field sits at the top level."
  (let ((key (intern (plist-get props :body-api-key)))
        (fn (plist-get props :body-remote-fn)))
    (if fn
        (let ((html (funcall fn item)))
          (and html (cons key html)))
      (assq key item))))

(defun org-canvas--diff-compare-body (props pom item)
  "Compare the body at POM against Canvas ITEM, as text.
PROPS is the feature's property registry entry: `:body-api-key' names
the Canvas field (description, body, message), the optional
`:body-fn' the local extractor and `:body-remote-fn' the remote one.
Returns nil when the feature has no body, ITEM does not carry the
field, or the local export failed — none of which is drift — and
otherwise (t . ROW), where ROW is nil when the two agree and (LABEL
LOCAL REMOTE), with excerpts around the first difference, when they
do not."
  (let* ((api-key (plist-get props :body-api-key))
         (cell (and api-key (org-canvas--diff-remote-body props item)))
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

;;;; Extra Details (issue #296)
;;
;; An EXTRA row named a title and an id, and saying what the object was
;; took a hand GET per row.  For the two features where that matters
;; most — an assignment made in the web UI, a group added to fix the
;; weights — the list replies the report already holds say enough: an
;; assignment's group, points, due date, publish state and ungraded
;; count, a group's weight and how many assignments it holds.  The group
;; name and the assignment count each need the other feature's list, so
;; the details are added after every feature is diffed, as the
;; referenced-media pass is.  No request is added.  The same pass sets
;; the groups' weight totals, a line under their section.

(defun org-canvas--diff-result-named (results name)
  "Return the result in RESULTS for feature NAME, when it was read.
An excluded or failed feature has no list reply, and so no result
here."
  (cl-find-if (lambda (r)
                (and (string= (org-canvas--diff-normalize-name
                               (plist-get r :name))
                              (org-canvas--diff-normalize-name name))
                     (plist-member r :remote-items)))
              results))

(defun org-canvas--diff-number (n)
  "Return number N printed without a trailing \".0\"."
  (if (= n (truncate n)) (format "%d" (truncate n)) (format "%s" n)))

(defun org-canvas--diff-assignment-details (item group-names)
  "Return the identifying fields of assignment ITEM as one string.
GROUP-NAMES maps a group id string to its name; a group it lacks is
named by id."
  (let ((group (org-canvas--diff-normalize-remote
                (alist-get 'assignment_group_id item)))
        (points (org-canvas--diff-normalize-remote
                 (alist-get 'points_possible item)))
        (due (let ((iso (org-canvas--diff-normalize-remote
                         (alist-get 'due_at item))))
               (and iso (or (org-canvas--iso8601-to-org-timestamp iso) iso))))
        (grading (alist-get 'needs_grading_count item)))
    (mapconcat
     #'identity
     (delq nil
           (list
            (when group
              (let ((name (gethash (format "%s" group) group-names)))
                (if name (format "group '%s'" name) (format "group id %s" group))))
            (when (numberp points)
              (format "%s pts" (org-canvas--diff-number points)))
            (if due (format "due %s" due) "no due date")
            (when (assq 'published item)
              (if (eq (alist-get 'published item) t) "published" "unpublished"))
            (when (numberp grading) (format "%d to grade" grading))))
     ", ")))

(defun org-canvas--diff-group-details (item counts)
  "Return the identifying fields of assignment group ITEM as one string.
COUNTS maps a group id string to its assignment count, or is nil when
the assignments were not read."
  (let ((weight (org-canvas--diff-normalize-remote
                 (alist-get 'group_weight item))))
    (mapconcat
     #'identity
     (delq nil
           (list (format "weight %s%%"
                         (org-canvas--diff-number (if (numberp weight) weight 0)))
                 (when counts
                   (let ((n (gethash (format "%s" (alist-get 'id item)) counts 0)))
                     (format "%d assignment%s" n (if (= n 1) "" "s"))))))
     ", ")))

(defun org-canvas--diff-detail-extras (result fn)
  "Give each EXTRA entry of RESULT a :details line from FN.
FN takes the entry's remote item and returns a string.  An UNCLAIMED
entry, also among the result's extras, names its heading instead."
  (let ((index (org-canvas--diff-remote-index
                (plist-get result :remote-items) 'id)))
    (dolist (entry (plist-get result :extra))
      (when-let* (((eq (plist-get entry :kind) 'extra))
                  (item (gethash (plist-get entry :id) index)))
        (plist-put entry :details (funcall fn item))))))

(defun org-canvas--diff-org-weight-total (props)
  "Return the WEIGHT total of the group headings that PROPS names.
Nil when the groups file does not exist."
  (let* ((var (plist-get props :file-var))
         (file (and var (boundp var) (symbol-value var))))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (apply #'+ (org-map-entries
                    (lambda ()
                      (string-to-number (or (org-entry-get (point) "WEIGHT") "0")))
                    (plist-get props :query) 'file))))))

(defun org-canvas--diff-weight-totals (groups)
  "Return (ORG . CANVAS) group weight totals for GROUPS when they differ.
CANVAS sums every group Canvas holds, the stock group a skip-fn hides
included, since its weight counts toward the grade all the same."
  (let ((org (org-canvas--diff-org-weight-total
              (org-canvas--diff-find-properties (plist-get groups :name))))
        (canvas (apply #'+ (mapcar
                            (lambda (item)
                              (let ((w (org-canvas--diff-normalize-remote
                                        (alist-get 'group_weight item))))
                                (if (numberp w) w 0)))
                            (plist-get groups :remote-items)))))
    (when (and org (> (abs (- org canvas)) 0.01))
      (cons org canvas))))

(defun org-canvas--diff-apply-extra-details (results)
  "Add identifying fields to the assignment and group extras in RESULTS.
Also sets the groups result's :weight-totals when Org and Canvas
disagree.  Reads only the list replies already held (issue #296).
Returns RESULTS, mutated in place."
  (let* ((assignments (org-canvas--diff-result-named results "Assignments"))
         (groups (org-canvas--diff-result-named results "Assignment Groups"))
         (names (org-canvas--diff-remote-index
                 (plist-get groups :remote-items) 'id))
         (counts (and assignments (make-hash-table :test 'equal))))
    (maphash (lambda (id item) (puthash id (alist-get 'name item) names)) names)
    (dolist (item (plist-get assignments :remote-items))
      (let ((key (format "%s" (alist-get 'assignment_group_id item))))
        (puthash key (1+ (gethash key counts 0)) counts)))
    (when assignments
      (org-canvas--diff-detail-extras
       assignments (lambda (item) (org-canvas--diff-assignment-details item names))))
    (when groups
      (org-canvas--diff-detail-extras
       groups (lambda (item) (org-canvas--diff-group-details item counts)))
      (plist-put groups :weight-totals (org-canvas--diff-weight-totals groups)))
    results))

;;;; New Quiz Assignments (issue #313)
;;
;; A New Quiz is an assignment, so the Assignments list holds every one,
;; and assignments.org claims none of them: each was an Assignments EXTRA
;; row, a web-UI quiz and a synced one alike.  With a New Quizzes
;; section of its own, that row reports the same object twice, the
;; second time wrongly.  When the New Quizzes list was read, an
;; Assignments extra whose id it holds is taken out and counted in the
;; footer's "Not checked" line, as a `:skip-fn' is.  Nothing is fetched,
;; and the orphan scan and prune are not touched.

(defconst org-canvas--diff-new-quiz-covered-reason
  "a New Quiz's assignment, checked under New Quizzes"
  "The reason for leaving a New Quiz's assignment out of Assignments.")

(defun org-canvas--diff-apply-new-quiz-assignments (results)
  "Take the New Quizzes' own assignments out of the Assignments extras.
RESULTS is the whole report.  Only when both lists were read; the
count goes to the Assignments result as :covered.  Returns RESULTS,
mutated in place."
  (let ((assignments (org-canvas--diff-result-named results "Assignments"))
        (quizzes (org-canvas--diff-result-named results "New Quizzes")))
    (when (and assignments quizzes)
      (let* ((ids (org-canvas--diff-remote-index
                   (plist-get quizzes :remote-items) '(assignment_id id)))
             (extra (plist-get assignments :extra))
             (covered (cl-remove-if-not
                       (lambda (e) (gethash (plist-get e :id) ids)) extra)))
        (plist-put assignments :extra (cl-set-difference extra covered))
        (plist-put assignments :covered (length covered))))
    results))

;;;; Per-Feature Comparison

(defun org-canvas--diff-display-title (heading)
  "Return the visible title of HEADING, the name of its Canvas item.
A heading that is a link — a module item's
`[[file:assignments.org::*Closer][Closer]]', a files.org heading — is
named on Canvas by its description, never by the markup; a bare link
by the heading or file it targets.  Statistics cookies are dropped, as
the sync drops them.  Comparing raw heading text against a Canvas
title is why a moved module item never paired (issue #294)."
  (let ((h (string-trim (or heading ""))))
    (org-canvas--strip-statistics-cookie
     (if (string-match "\\`\\[\\[\\([^]]+\\)\\]\\]\\'" h)
         (let ((target (match-string 1 h)))
           (cond ((string-match "::\\*\\(.+\\)\\'" target) (match-string 1 target))
                 ((string-match "\\`file:\\(.+\\)\\'" target)
                  (file-name-nondirectory (match-string 1 target)))
                 (t target)))
       (org-link-display-format h)))))

(defun org-canvas--diff-local-entry (id-property)
  "Return the drift report's plist for the heading at point.
ID-PROPERTY names the property holding its Canvas id; an empty value
is no id, as the sync reads it.  :title is the heading as
`org-get-heading' gives it, :heading the raw text with any link markup,
:match-title what Canvas would call it (`org-canvas--diff-display-title')
and :line where it sits, which is how a PENDING row finds it again."
  (let* ((id (org-entry-get (point) id-property))
         (raw (save-excursion
                (beginning-of-line)
                (when (looking-at org-complex-heading-regexp)
                  (match-string-no-properties 4)))))
    (list :id (and id (not (string-empty-p id)) id)
          :title (org-get-heading t t t t)
          :heading raw
          :match-title (org-canvas--diff-display-title raw)
          :line (line-number-at-pos))))

(defun org-canvas--diff-collect-local (file query id-property)
  "Collect entries from FILE matching QUERY, keyed by ID-PROPERTY.
Returns a list of `org-canvas--diff-local-entry' plists, each with a
:pom marker added.  The position is a marker, not an offset: the
comparison reads properties long after this buffer stops being
current, and a bare offset would silently be read against whatever
buffer happened to be."
  (when (and file (file-exists-p file))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-map-entries
       (lambda ()
         (append (org-canvas--diff-local-entry id-property)
                 (list :pom (point-marker))))
       (or query "LEVEL=1") 'file))))

(defun org-canvas--diff-remote-index (items id-field)
  "Return a hash of ITEMS keyed by their ID-FIELD, as strings.
ID-FIELD may be a list of fields tried in order, as a New Quiz's
`(assignment_id id)' is (`org-canvas--item-id-value', issue #313)."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((id (org-canvas--item-id-value item id-field)))
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
                :body-compared (and body t)
                ;; Where `b' on the row can take a reader (#292).
                :html-url (org-canvas--diff-normalize-remote
                           (alist-get 'html_url item)))))))))

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
ID-FIELD and TITLE-FIELD name the item alist keys; ID-FIELD may be a
list, as `org-canvas--item-id-value' reads it.  SKIP-FN, when
non-nil, marks an item the feature never compares.  Returns a cons of
the extra entry list and the count SKIP-FN held back — a skipped item
is neither drift nor extra, but it is also not checked, and saying so
is what keeps the report from reading as full coverage (issue #81)."
  (let ((extra nil)
        (suppressed 0))
    (dolist (item items)
      (unless (member (format "%s" (org-canvas--item-id-value item id-field))
                      claimed)
        (if (and skip-fn (funcall skip-fn item))
            (cl-incf suppressed)
          (push (list :kind 'extra
                      :title (format "%s" (or (alist-get title-field item) "?"))
                      :id (format "%s" (org-canvas--item-id-value item id-field))
                      ;; Where RET on the row can take a reader (#103).
                      :html-url (org-canvas--diff-normalize-remote
                                 (alist-get 'html_url item)))
                extra))))
    (cons (nreverse extra) suppressed)))

(defun org-canvas--diff-match-title (entry)
  "Return the title local ENTRY is paired on: what Canvas would call it.
Falls back to ENTRY's :title when it carries no :match-title."
  (string-trim (or (plist-get entry :match-title) (plist-get entry :title) "")))

(defun org-canvas--diff-pair-unclaimed (extra local id-property)
  "Pair each EXTRA remote item with an unstamped LOCAL heading of its title.
A partial create — the POST succeeded, the stamp was never written —
leaves exactly this pair: a remote item nothing claims and, a few
lines away, a heading of the same name with no ID-PROPERTY.  Reported
apart they are a two-line mystery, and the next sync creates a second
item (issue #85).  Such an entry becomes kind `unclaimed' and names
the property to stamp; the rest of EXTRA is returned as it was.
A heading is matched by the title it shows, not its markup
\(`org-canvas--diff-match-title', issue #294)."
  (let ((unstamped (delq nil (mapcar (lambda (e)
                                       (and (null (plist-get e :id))
                                            (org-canvas--diff-match-title e)))
                                     local))))
    (mapcar (lambda (e)
              (if (member (string-trim (plist-get e :title)) unstamped)
                  (list :kind 'unclaimed :title (plist-get e :title)
                        :id (plist-get e :id) :property id-property
                        :html-url (plist-get e :html-url))
                e))
            extra)))

(defun org-canvas--diff-split-acknowledged (name extra index)
  "Sort feature NAME's EXTRA entries by `org-canvas-diff-known-extras'.
INDEX is the hash of the remote ids seen.  Returns a plist: :extra,
the entries left unacknowledged; :acknowledged, how many were; and
:stale, a `stale-ack' divergence for each acknowledged id INDEX no
longer holds, so the acknowledgment list cannot rot (issue #98)."
  (let* ((known (org-canvas--diff-known-extras-for name))
         (acked (cl-remove-if-not
                 (lambda (e) (assoc (plist-get e :id) known))
                 extra))
         (stale (cl-remove-if (lambda (k) (gethash (car k) index)) known)))
    (list :extra (cl-remove-if (lambda (e) (memq e acked)) extra)
          :acknowledged (length acked)
          :stale (mapcar (lambda (k)
                           (list :kind 'stale-ack :id (car k) :note (cdr k)))
                         stale))))

;;;; Pending Creates (issue #294)
;;
;; The report listed only what Canvas has that Org lacks and what both
;; hold differently, so a heading with no Canvas id — which the next
;; sync POSTs — got no row at all.  After a local restructure the report
;; showed the Canvas half of the change (56 EXTRA module items) and said
;; nothing of the 57 items, three assignments, two rubrics and one group
;; the next sync would create.  Each unstamped heading a feature would
;; push is now a PENDING row.  They are kept apart from the divergences
;; — `:pending' on the result, never in `org-canvas--diff-count' — since
;; they are the Org side's unpushed work, not Canvas drifting from it: a
;; course whose instructor is drafting next week still reports zero, and
;; `org-canvas-diff-batch' exits zero on it.  A heading an UNCLAIMED or
;; MOVED row already names is not listed twice.

(defconst org-canvas--diff-pending-p-fns
  '(("files" . org-canvas--diff-file-heading-p))
  "Predicates over an unstamped local entry, by normalized feature name.
A feature named here reports a PENDING row only for an entry its
predicate accepts: a files.org folder heading carries no id and is
never uploaded, so it is not a create.")

(defun org-canvas--diff-file-heading-p (entry)
  "Return non-nil when files.org ENTRY is a file heading, not a folder.
The sync uploads only a heading that links a file."
  (string-match-p "\\[\\[file:" (or (plist-get entry :heading) "")))

(defun org-canvas--diff-pending (name local paired file property)
  "Return a `pending' entry for each LOCAL entry of feature NAME with no id.
PAIRED lists the match titles an UNCLAIMED or MOVED row already names,
which are left out; FILE is where the headings live and PROPERTY the id
property the next sync stamps.  A feature in
`org-canvas--diff-pending-p-fns' reports only what its predicate
accepts."
  (let ((pred (alist-get (org-canvas--diff-normalize-name name)
                         org-canvas--diff-pending-p-fns nil nil #'string=)))
    (delq nil
          (mapcar (lambda (e)
                    (when (and (null (plist-get e :id))
                               (not (member (org-canvas--diff-match-title e) paired))
                               (or (null pred) (funcall pred e)))
                      (list :kind 'pending
                            :title (org-canvas--diff-match-title e)
                            :heading (plist-get e :title)
                            :file file :line (plist-get e :line)
                            :property property
                            :where (plist-get e :where)
                            :container (plist-get e :container))))
                  local))))

(defun org-canvas--diff-paired-titles (entries)
  "Return the trimmed titles of the UNCLAIMED and MOVED rows among ENTRIES."
  (delq nil (mapcar (lambda (e)
                      (and (memq (plist-get e :kind) '(unclaimed moved))
                           (string-trim (plist-get e :title))))
                    entries)))

;;;; Module Items (issue #177)
;;
;; Modules are the one feature with children the report skipped.  A
;; sync that had lost an item's stamp created a second copy of the same
;; assignment in the module (issue #179); students saw it twice, and
;; the report called Modules clean, since it compared module ids and
;; never looked inside one.  The child pass below lists the items of
;; every module the file claims — one request per module, the same
;; shape as the sync's own reconcile — and reports an item no level-2
;; heading anywhere in modules.org claims as EXTRA, in a section of its
;; own named "Module Items": an item id and a module id are different
;; things, so an acknowledgment in `org-canvas-diff-known-extras' is
;; filed under "module-items", and the row's delete goes to the item's
;; own URL.  An item another module's heading claims is a pending move
;; the next sync settles (issue #105), not an extra.
;;
;; An unclaimed item is then paired with an unstamped item heading,
;; one to one (issue #294), the way the sync would adopt it (issue
;; #299): the heading is read by the sync's own parser, which resolves
;; its link offline to a type and a content id — a lookup in the linked
;; Org file, no request, nothing stamped — and the sync's own predicate
;; says whether the Canvas item holds that content.  A same-titled item
;; linking different content is therefore left an EXTRA, and the
;; heading a PENDING create, which is what the sync does with them.
;; Only a heading whose link target has no Canvas id yet falls back to
;; the title — the sync's predicate does too — and its row says it was
;; paired by title only.  That title is the link's description, what
;; Canvas calls the item, never the markup.  A heading in the item's
;; own module makes
;; the pair UNCLAIMED (the next sync adopts the twin, #179); one under
;; another module makes it MOVED — the item was carried to a new week
;; in Org without its stamp, and the next sync creates it there and
;; leaves this copy.  A SubHeader is paired only within its module: its
;; title is all it has, and "Readings" recurs in every week by design.
;; The unstamped headings left over are PENDING creates.
;;
;; A MOVED row names the heading it paired with, and `s' stamps the
;; item's id there (issue #342).  The sync cannot move an item: once
;; stamped, the heading's id is not in its module, so the next sync
;; creates the item there with a new id and deletes the old copy (issue
;; #105).  That stamped heading is a RELOCATE row, counted apart like a
;; PENDING one, since the report is the only warning that the id
;; changes (issue #343).

(defconst org-canvas--diff-children-fns
  '(("modules" . org-canvas--diff-module-items)
    ("newquizzes" . org-canvas--diff-new-quiz-items))
  "Child checks, by normalized feature name.
Each a function of (ITEMS LOCAL FILE) — the feature's remote list,
the local entries `org-canvas--diff-feature' collected, and the file
they came from — returning a result plist of its own, reported right
after the feature's.")

(defun org-canvas--diff-children (name items local file)
  "Run feature NAME's child check, if any, over ITEMS, LOCAL and FILE."
  (when-let* ((fn (alist-get (org-canvas--diff-normalize-name name)
                             org-canvas--diff-children-fns
                             nil nil #'string=)))
    (funcall fn items local file)))

(defun org-canvas--diff-module-item-lists (modules)
  "Return (MODULE . ITEMS) for each of MODULES, (ID . NAME) pairs.
One list request per module."
  (mapcar (lambda (module)
            (cons module
                  (append (org-canvas-api-request-all-pages
                           'GET (org-canvas-api-course-endpoint
                                 "modules/%s/items" (car module)))
                          nil)))
          modules))

(defun org-canvas--diff-module-item-extras (lists claimed)
  "Return the items in LISTS no heading CLAIMED, as `extra' entries.
LISTS is what `org-canvas--diff-module-item-lists' returned; each
entry names the module it sits in and carries the module id, which is
what deleting it needs, and the item's type and content, which pairing
needs."
  (let ((extra nil))
    (dolist (pair lists)
      (dolist (item (cdr pair))
        (unless (member (format "%s" (alist-get 'id item)) claimed)
          (push (list :kind 'extra
                      :title (format "%s" (or (alist-get 'title item) "?"))
                      :id (format "%s" (alist-get 'id item))
                      :html-url (org-canvas--diff-normalize-remote
                                 (alist-get 'html_url item))
                      :module-id (car (car pair))
                      :where (cdr (car pair))
                      :type (alist-get 'type item)
                      :content-id (alist-get 'content_id item)
                      :page-url (alist-get 'page_url item))
                extra))))
    (nreverse extra)))

(defun org-canvas--diff-module-item-content (dir)
  "Return the sync's reading of the item heading at point, or nil.
DIR is the directory of modules.org, which the heading's link is
relative to.  The heading is read by the sync's own parser,
`org-canvas--module-item-parse-entry': a :type, and a :content-id or
:page-url when the link resolves — looked up in the linked Org file,
offline — or neither when its target has no Canvas id yet (issue
#299).  The parser logs as a sync stage would, so the logger is off
while it runs; a report is not a sync.  Nil when the parser is not
loaded or fails, and the pairing then goes by title alone."
  (let ((org-canvas--logger nil))
    (save-excursion
      (ignore-errors (org-canvas--module-item-parse-entry dir)))))

(defun org-canvas--diff-module-item-headings (file)
  "Return the level-2 headings of modules FILE as local entries.
Each is an `org-canvas--diff-local-entry' plist with the module it sits
under added: :module-id, that heading's CANVAS_ID, :where, its title,
and :content, what it links (`org-canvas--diff-module-item-content').
No markers: nothing here is read again."
  (when (and file (file-exists-p file))
    (let ((dir (file-name-directory (expand-file-name file))))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((parent (save-excursion
                           (when (org-up-heading-safe)
                             (cons (org-entry-get (point) "CANVAS_ID")
                                   (org-get-heading t t t t))))))
             (append (org-canvas--diff-local-entry "CANVAS_ID")
                     (list :module-id (car parent) :where (cdr parent)
                           :content (org-canvas--diff-module-item-content dir)))))
         "LEVEL=2" 'file)))))

(defun org-canvas--diff-module-item-remote (extra)
  "Return EXTRA, a remote module item entry, as the alist Canvas sent."
  (list (cons 'type (plist-get extra :type))
        (cons 'title (plist-get extra :title))
        (cons 'content_id (plist-get extra :content-id))
        (cons 'page_url (plist-get extra :page-url))))

(defun org-canvas--diff-module-item-match (extra entry)
  "Return how the unstamped item heading ENTRY matches EXTRA, or nil.
`content' when the sync would adopt EXTRA for ENTRY on what it links:
the same type and content id or page url, or for a SubHeader or an
external URL, whose title is its content, the same title
\(`org-canvas--module-item-same-content-p', issue #299).  `title' when
the two share only a title because ENTRY's link target has no Canvas
id yet, or ENTRY could not be parsed: the sync's predicate falls back
to the title there too, but what the target will be is not known."
  (let ((content (plist-get entry :content)))
    (cond
     ((null content)
      (and (string= (org-canvas--diff-match-title entry)
                    (string-trim (plist-get extra :title)))
           'title))
     ((not (org-canvas--module-item-same-content-p
            content (org-canvas--diff-module-item-remote extra)))
      nil)
     ((or (plist-get content :content-id) (plist-get content :page-url)
          (member (plist-get content :type) '("SubHeader" "ExternalUrl")))
      'content)
     (t 'title))))

(defun org-canvas--diff-module-item-partner (extra unstamped)
  "Return (ENTRY . MATCH), the entry of UNSTAMPED to pair with EXTRA.
EXTRA is a remote module item; MATCH is how they matched
\(`org-canvas--diff-module-item-match').  A heading in the item's own
module first; failing that, one under any other module, unless the
item is a SubHeader.  Nil when no heading matches."
  (let ((matches (delq nil (mapcar (lambda (e)
                                     (let ((m (org-canvas--diff-module-item-match
                                               extra e)))
                                       (and m (cons e m))))
                                   unstamped))))
    (or (cl-find-if (lambda (pair) (equal (plist-get (car pair) :module-id)
                                          (plist-get extra :module-id)))
                    matches)
        (and (not (equal (plist-get extra :type) "SubHeader"))
             (car matches)))))

(defun org-canvas--diff-module-item-paired (extra found file)
  "Return EXTRA re-kinded for FOUND, its (PARTNER . MATCH), from FILE.
`unclaimed' when PARTNER sits in the item's module, `moved' when it
does not.  :to names PARTNER's module and :by-title says only the
title paired them (issue #299).  :file, :line, :partner-heading (the
raw heading text) and :partner-title (what it shows) name the heading
itself, which is how a MOVED row says which heading it paired with,
a renamed one included, and how `s' finds it to stamp (issue #342)."
  (let ((partner (car found)))
    (append (list :kind (if (equal (plist-get partner :module-id)
                                   (plist-get extra :module-id))
                            'unclaimed
                          'moved)
                  :to (plist-get partner :where)
                  :by-title (eq (cdr found) 'title)
                  :file file :line (plist-get partner :line)
                  :partner-heading (plist-get partner :heading)
                  :partner-title (org-canvas--diff-match-title partner))
            (cddr extra))))

(defun org-canvas--diff-module-item-pair (extra headings &optional file)
  "Pair the EXTRA module items with the unstamped entries of HEADINGS.
Returns (ENTRIES . LEFT): ENTRIES is EXTRA with each paired item
re-kinded by `org-canvas--diff-module-item-paired', FILE being the
modules file HEADINGS came from, and LEFT the unstamped headings
nothing paired, the pending creates.  Each heading pairs at most once
\(issue #294)."
  (let ((unstamped (cl-remove-if (lambda (e) (plist-get e :id)) headings)))
    (cons (mapcar
           (lambda (e)
             (let ((found (org-canvas--diff-module-item-partner e unstamped)))
               (if (not found)
                   e
                 (setq unstamped (delq (car found) unstamped))
                 (org-canvas--diff-module-item-paired e found file))))
           extra)
          unstamped)))

(defun org-canvas--diff-module-item-synced (modules local)
  "Return (ID . TITLE) for each LOCAL module entry with an id in MODULES."
  (let ((index (org-canvas--diff-remote-index modules 'id)))
    (delq nil (mapcar (lambda (e)
                        (and (plist-get e :id)
                             (gethash (plist-get e :id) index)
                             (cons (plist-get e :id) (plist-get e :title))))
                      local))))

(defun org-canvas--diff-module-item-homes (lists)
  "Return a hash of the item ids in LISTS to (MODULE . ITEM).
MODULE is the (ID . NAME) pair of the module Canvas holds the item in."
  (let ((homes (make-hash-table :test 'equal)))
    (dolist (pair lists)
      (dolist (item (cdr pair))
        (puthash (format "%s" (alist-get 'id item))
                 (cons (car pair) item) homes)))
    homes))

(defun org-canvas--diff-module-item-relocation (entry home file)
  "Return the `relocate' entry of heading ENTRY, whose item is at HOME.
HOME is the (MODULE . ITEM) the item sits at, FILE the modules file."
  (list :kind 'relocate :title (org-canvas--diff-match-title entry)
        :id (plist-get entry :id) :where (cdr (car home))
        :to (plist-get entry :where)
        :file file :line (plist-get entry :line)
        :html-url (org-canvas--diff-normalize-remote
                   (alist-get 'html_url (cdr home)))))

(defun org-canvas--diff-module-item-relocations (lists headings file)
  "Return a `relocate' entry per stamped heading of HEADINGS in another module.
LISTS is what `org-canvas--diff-module-item-lists' returned and FILE
the modules file.  A heading stamped with an item id that Canvas holds
under a module other than the heading's own is not drift, and no row
of the report named it: the next sync finds the id missing from the
heading's module (`org-canvas--module-item-disown-foreign-id'),
creates the item there with a new id and deletes the old copy, since
Canvas cannot move an item between modules (issue #343)."
  (let ((homes (org-canvas--diff-module-item-homes lists)))
    (delq nil
          (mapcar
           (lambda (e)
             (let ((home (and (plist-get e :id)
                              (gethash (plist-get e :id) homes))))
               (when (and home (not (equal (car (car home))
                                           (plist-get e :module-id))))
                 (org-canvas--diff-module-item-relocation e home file))))
           headings))))

(defun org-canvas--diff-module-items-result (modules local file)
  "Compare the items of the modules in LOCAL against Canvas; see the caller.
MODULES, LOCAL and FILE are `org-canvas--diff-module-items''s."
  (let* ((headings (org-canvas--diff-module-item-headings file))
         (lists (org-canvas--diff-module-item-lists
                 (org-canvas--diff-module-item-synced modules local)))
         (seen (org-canvas--diff-remote-index
                (apply #'append (mapcar #'cdr lists)) 'id))
         (split (org-canvas--diff-split-acknowledged
                 "Module Items"
                 (org-canvas--diff-module-item-extras
                  lists (delq nil (mapcar (lambda (e) (plist-get e :id)) headings)))
                 seen))
         (paired (org-canvas--diff-module-item-pair (plist-get split :extra)
                                                    headings file)))
    (list :name "Module Items"
          :divergences (plist-get split :stale)
          :extra (car paired)
          :acknowledged (plist-get split :acknowledged)
          :pending (org-canvas--diff-pending "Module Items" (cdr paired) nil
                                             file "CANVAS_ID")
          ;; Stamped headings whose item sits in another module (#343).
          :relocate (org-canvas--diff-module-item-relocations
                     lists headings file))))

(defun org-canvas--diff-module-items (modules local file)
  "Compare the items of the modules in LOCAL against Canvas.
MODULES is the course's module list; only a LOCAL entry whose id it
holds is looked into, one request each.  FILE is modules.org, whose
level-2 headings are what claims an item.  Returns a result plist named
\"Module Items\", shaped like `org-canvas--diff-feature''s: an item no
level-2 heading in modules.org claims is an extra (issue #177), minus
the ones `org-canvas-diff-known-extras' acknowledges under
\"module-items\", with a `stale-ack' divergence for an acknowledged id
no module holds any more.  An extra that shares its title with an
unstamped item heading is UNCLAIMED or MOVED, and the unstamped
headings left are :pending (issue #294).  A stamped heading whose item
Canvas holds in another module is :relocate (issue #343)."
  (if (org-canvas--diff-feature-excluded-p "Module Items")
      (list :name "Module Items" :excluded t)
    (condition-case err
        (org-canvas--diff-module-items-result modules local file)
      (error
       (list :name "Module Items" :error (error-message-string err))))))

;;;; New Quiz Items (issue #322)
;;
;; New Quizzes joined the report in #313 with their own settings and
;; instructions compared, and nothing below them: an item edited or added
;; in the web UI, or deleted there, left the quiz reading clean.  The
;; pass below is the module items' shape (#177) under a quiz.  For every
;; quiz heading whose CANVAS_ASSIGNMENT_ID the quiz service still lists,
;; one request lists its items (`quizzes/:id/items'), and the level-2
;; headings under that quiz are held against them: a heading whose
;; CANVAS_ITEM_ID the quiz no longer holds is MISSING; one whose
;; registered properties (POINTS, TYPE) or body differ is CHANGED; an
;; item no heading of its quiz claims is EXTRA.  Items belong to one
;; quiz, so a claim counts only within it, and an unstamped heading is
;; paired only with an item of its own quiz, by the predicate the sync
;; adopts a twin with (`org-canvas--new-quiz-item-twin-p', #179): the
;; item's first paragraph is the heading.  Such a pair is UNCLAIMED, and
;; the unstamped headings left are PENDING.  The body is the heading and
;; the prompt above the answer list, exported offline as a push would
;; send it, compared as text (#83); the answers themselves live in
;; `interaction_data', whose ids are minted afresh by every push, and
;; are not compared.  No item carries a conflict baseline of its own,
;; so an item row is never "modified remotely" and never adoptable.
;;
;; A list that cannot be read is not an empty quiz (Hard Rule 16): the
;; section reads "could not check" rather than reporting every heading
;; MISSING.  Every row names its quiz (:quiz-id, :where), which is what
;; its verbs need: `k' deletes the item at the quiz service, `b' opens
;; the quiz's page (an item has none), and `p' pulls the quiz, which
;; writes its items whole.  Acknowledgments are filed under
;; "new-quiz-items".  Read-only, as the rest of the report.

(defconst org-canvas--diff-new-quiz-items-name "New Quiz Items"
  "The section New Quiz item rows report in (issue #322).")

(defun org-canvas--diff-new-quiz-item-list (quiz-id)
  "Return the items of the New Quiz QUIZ-ID as a list, or signal.
One request to the quiz service.  A reply that is not a list of item
objects is a misread, never an empty quiz, so it signals (Hard Rule
16)."
  (let ((reply (org-canvas-api-request-all-pages
                'GET (org-canvas--new-quiz-api-endpoint
                      "quizzes/%s/items" quiz-id))))
    (unless (and (or (vectorp reply) (listp reply))
                 (seq-every-p (lambda (i) (and (consp i) (consp (car i)))) reply))
      (error "Could not read the item list of quiz %s" quiz-id))
    (append reply nil)))

(defun org-canvas--diff-new-quiz-item-lists (quizzes local)
  "Return (QUIZ-ID TITLE . ITEMS) for each LOCAL quiz found in QUIZZES.
QUIZZES is the quiz service's reply, LOCAL the quiz headings
`org-canvas--diff-feature' collected; one request per stamped quiz."
  (let ((index (org-canvas--diff-remote-index quizzes '(assignment_id id))))
    (delq nil
          (mapcar (lambda (e)
                    (let ((id (plist-get e :id)))
                      (when (and id (gethash id index))
                        (cons id (cons (plist-get e :title)
                                       (org-canvas--diff-new-quiz-item-list id))))))
                  local))))

(defun org-canvas--diff-new-quiz-item-headings (file)
  "Return the item headings of New Quizzes FILE as local entries.
Each is an `org-canvas--diff-local-entry' plist keyed by CANVAS_ITEM_ID,
with the quiz it sits under added: :quiz-id, that heading's
CANVAS_ASSIGNMENT_ID, and :where, its title; and :pom, a marker the
caller releases."
  (when (and file (file-exists-p file))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-map-entries
       (lambda ()
         (let ((quiz (save-excursion
                       (when (org-up-heading-safe)
                         (cons (org-entry-get (point) "CANVAS_ASSIGNMENT_ID")
                               (org-get-heading t t t t))))))
           (append (org-canvas--diff-local-entry "CANVAS_ITEM_ID")
                   (list :quiz-id (car quiz) :where (cdr quiz)
                         :container "quiz" :pom (point-marker)))))
       "LEVEL=2" 'file))))

(defun org-canvas--diff-new-quiz-item-base (heading file)
  "Return the fields common to every item row about HEADING in FILE.
The quiz it sits in, for the verbs, and where the heading is, for RET."
  (list :title (plist-get heading :title) :id (plist-get heading :id)
        :quiz-id (plist-get heading :quiz-id) :where (plist-get heading :where)
        :container "quiz" :file file :line (plist-get heading :line)
        :heading (plist-get heading :title) :property "CANVAS_ITEM_ID"))

(defun org-canvas--diff-new-quiz-item-entry (heading items props file)
  "Compare the stamped item HEADING with ITEMS, its quiz's list.
PROPS is the \"new-quiz-items\" property registration; FILE is where
HEADING lives.  Returns a `missing' entry when the quiz holds no item
of its id, a `modified' one when a property or the body differs, and
nil when they agree."
  (let* ((id (plist-get heading :id))
         (pom (plist-get heading :pom))
         (item (cl-find id items :test #'equal
                        :key (lambda (i) (format "%s" (alist-get 'id i)))))
         (base (org-canvas--diff-new-quiz-item-base heading file)))
    (if (null item)
        (cons :kind (cons 'missing base))
      (let* ((body (org-canvas--diff-compare-body props pom item))
             (fields (append (org-canvas--diff-compare-fields
                              (plist-get props :properties) pom item)
                             (and (cdr body) (list (cdr body))))))
        (when fields
          (append (list :kind 'modified :fields fields
                        :body-compared (and body t))
                  base))))))

(defun org-canvas--diff-new-quiz-item-title (item)
  "Return the report's name for remote ITEM: its first paragraph, or ?."
  (let ((title (org-canvas--new-quiz-item-remote-title item)))
    (if (string-empty-p title) "?" title)))

(defun org-canvas--diff-new-quiz-item-extras (lists claimed)
  "Return the items in LISTS no heading of their quiz CLAIMED, as extras.
LISTS is what `org-canvas--diff-new-quiz-item-lists' returned; CLAIMED
holds a (QUIZ-ID . ITEM-ID) pair per stamped item heading.  Each entry
keeps the item, which pairing reads."
  (let (extra)
    (dolist (quiz lists)
      (dolist (item (cddr quiz))
        (let ((id (format "%s" (alist-get 'id item))))
          (unless (member (cons (car quiz) id) claimed)
            (push (list :kind 'extra
                        :title (org-canvas--diff-new-quiz-item-title item)
                        :id id :quiz-id (car quiz) :where (cadr quiz)
                        :container "quiz" :item item)
                  extra)))))
    (nreverse extra)))

(defun org-canvas--diff-new-quiz-item-twin (heading extra taken)
  "Return the entry of EXTRA the sync would adopt for unstamped HEADING.
One in HEADING's quiz, not in TAKEN, whose item opens with HEADING's
title (`org-canvas--new-quiz-item-twin-p'); nil when there is none."
  (let ((data (list :title (org-canvas--strip-statistics-cookie
                            (plist-get heading :title)))))
    (cl-find-if (lambda (e)
                  (and (equal (plist-get e :quiz-id) (plist-get heading :quiz-id))
                       (not (memq e taken))
                       (org-canvas--new-quiz-item-twin-p data (plist-get e :item))))
                extra)))

(defun org-canvas--diff-new-quiz-item-pair (extra headings file)
  "Pair the EXTRA items with the unstamped entries of HEADINGS in FILE.
Returns (ENTRIES . LEFT): ENTRIES is EXTRA with each paired item made
`unclaimed', naming its heading's place, and LEFT the unstamped
headings nothing paired, the pending creates.  Headings are taken in
file order, each pairing at most once, as the sync adopts them."
  (let (pairs left)
    (dolist (h headings)
      (unless (plist-get h :id)
        (let ((twin (org-canvas--diff-new-quiz-item-twin
                     h extra (mapcar #'car pairs))))
          (if twin (push (cons twin h) pairs) (push h left)))))
    (cons (mapcar (lambda (e)
                    (let ((h (cdr (assq e pairs))))
                      (if h
                          (append (list :kind 'unclaimed :file file
                                        :line (plist-get h :line)
                                        :heading (plist-get h :title))
                                  (cddr e))
                        e)))
                  extra)
          (nreverse left))))

(defun org-canvas--diff-new-quiz-item-claims (headings)
  "Return a (QUIZ-ID . ITEM-ID) pair for each stamped entry of HEADINGS."
  (delq nil (mapcar (lambda (h)
                      (and (plist-get h :id)
                           (cons (plist-get h :quiz-id) (plist-get h :id))))
                    headings)))

(defun org-canvas--diff-new-quiz-item-divergences (headings lists file)
  "Return the MISSING and CHANGED entries of the stamped HEADINGS.
Only a heading under a quiz LISTS holds is compared; FILE is theirs."
  (let ((props (org-canvas--diff-find-properties "new-quiz-items")))
    (delq nil (mapcar (lambda (h)
                        (let ((quiz (assoc (plist-get h :quiz-id) lists)))
                          (and quiz (plist-get h :id)
                               (org-canvas--diff-new-quiz-item-entry
                                h (cddr quiz) props file))))
                      headings))))

(defun org-canvas--diff-new-quiz-items-result (quizzes local file)
  "Compare the items of the quizzes in LOCAL against Canvas; see the caller.
QUIZZES, LOCAL and FILE are `org-canvas--diff-new-quiz-items''s."
  (let ((name org-canvas--diff-new-quiz-items-name)
        (headings (org-canvas--diff-new-quiz-item-headings file)))
    (unwind-protect
        (let* ((lists (org-canvas--diff-new-quiz-item-lists quizzes local))
               (split (org-canvas--diff-split-acknowledged
                       name
                       (org-canvas--diff-new-quiz-item-extras
                        lists (org-canvas--diff-new-quiz-item-claims headings))
                       (org-canvas--diff-remote-index
                        (apply #'append (mapcar #'cddr lists)) 'id)))
               (paired (org-canvas--diff-new-quiz-item-pair
                        (plist-get split :extra) headings file)))
          (list :name name
                :divergences (append (org-canvas--diff-new-quiz-item-divergences
                                      headings lists file)
                                     (plist-get split :stale))
                :extra (car paired)
                :acknowledged (plist-get split :acknowledged)
                :pending (org-canvas--diff-pending name (cdr paired) nil
                                                   file "CANVAS_ITEM_ID")))
      (dolist (h headings) (set-marker (plist-get h :pom) nil)))))

(defun org-canvas--diff-new-quiz-items (quizzes local file)
  "Compare the items of the New Quizzes in LOCAL against Canvas.
QUIZZES is the quiz service's list; only a LOCAL quiz whose id it holds
is looked into, one request each.  FILE is new-quizzes.org, whose
level-2 headings are the items.  Returns a result plist named \"New
Quiz Items\", shaped like `org-canvas--diff-feature''s (issue #322);
a list that cannot be read makes it an :error, never an empty quiz."
  (let ((name org-canvas--diff-new-quiz-items-name))
    (if (org-canvas--diff-feature-excluded-p name)
        (list :name name :excluded t)
      (condition-case err
          (org-canvas--diff-new-quiz-items-result quizzes local file)
        (error
         (list :name name :error (error-message-string err)))))))

(defun org-canvas--diff-feature (feature)
  "Compare one FEATURE registry entry against Canvas.
Returns a plist (:name :divergences :extra :notes :acknowledged
:pending :error): :extra holds remote items no Org heading claims —
minus the ones `org-canvas-diff-known-extras' acknowledges, whose count
is :acknowledged — :notes informational rows that are not drift (issue
#293), :pending the unstamped headings the next sync would create
\(issue #294), and :error a message when the list request failed.
An acknowledged id Canvas no longer holds joins :divergences as a
`stale-ack' entry, so the acknowledgment list cannot rot (issue #98).
A feature with a child check (`org-canvas--diff-children-fns') carries
its child's result as :children, which `org-canvas-diff' reports right
after this one (issue #177)."
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
               divergences notes)
          (dolist (entry local)
            (let ((d (org-canvas--diff-entry entry index specs props
                                             modified-field)))
              (when d (push d divergences)))
            (setq notes (nconc notes (org-canvas--diff-entry-notes
                                      entry index specs)))
            (let ((m (plist-get entry :pom)))
              (when (markerp m) (set-marker m nil))))
          (let* ((unclaimed (org-canvas--diff-unclaimed
                             items claimed id-field title-field skip-fn))
                 (paired (org-canvas--diff-pair-unclaimed
                          (car unclaimed) local id-property))
                 (split (org-canvas--diff-split-acknowledged name paired index)))
            (list :name name
                  :divergences (append (nreverse divergences)
                                       (plist-get split :stale))
                  :extra (plist-get split :extra)
                  :notes notes
                  :acknowledged (plist-get split :acknowledged)
                  ;; Unstamped headings the next sync creates (#294).
                  :pending (org-canvas--diff-pending
                            name local (org-canvas--diff-paired-titles paired)
                            file id-property)
                  :children (org-canvas--diff-children name items local file)
                  ;; The list reply, for the EXTRA details pass (#296).
                  :remote-items items
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

(defconst org-canvas--diff-by-title-note
  "paired by title only, since the heading's link target has no Canvas id yet"
  "What a module item row adds when only the title paired it (issue #299).")

(defun org-canvas--diff-unclaimed-line (entry)
  "Return the report line of UNCLAIMED ENTRY.
A module item's twin is adopted by the sync itself (issue #179), so
its line says that instead of naming `org-canvas-adopt-at-point'.  The
pairing went by content, as the sync's adoption does, unless ENTRY
says :by-title (issue #299)."
  (cond
   ((plist-get entry :by-title)
    (format "  UNCLAIMED %s (item id %s in module '%s' has this title and no heading claims it; %s — the next sync adopts it only if that target turns out to be this item's content)\n"
            (plist-get entry :title) (plist-get entry :id) (plist-get entry :where)
            org-canvas--diff-by-title-note))
   ((plist-get entry :module-id)
    (format "  UNCLAIMED %s (item id %s in module '%s' has the type and content of an unstamped heading there, and no heading claims it; the next sync adopts it)\n"
            (plist-get entry :title) (plist-get entry :id) (plist-get entry :where)))
   ((plist-get entry :quiz-id)
    (format "  UNCLAIMED %s (item id %s in quiz '%s' opens with the title of an unstamped heading there, and no heading claims it; the next sync adopts it)\n"
            (plist-get entry :title) (plist-get entry :id) (plist-get entry :where)))
   (t
    (format "  UNCLAIMED %s (Canvas id %s has this title and no heading claims it; adopt it with org-canvas-adopt-at-point, which stamps %s, or rename)\n"
            (plist-get entry :title) (plist-get entry :id)
            (or (plist-get entry :property) "CANVAS_ID")))))

(defun org-canvas--diff-partner-text (entry)
  "Return how a MOVED row names ENTRY's paired heading (issue #342).
The heading's shown title and line, so a pair whose titles differ —
a column renumbered in Org, an item renamed on Canvas — is legible."
  (let ((title (plist-get entry :partner-title))
        (line (plist-get entry :line)))
    (cond ((and title line)
           (format "the unstamped heading '%s' (line %d)" title line))
          (title (format "the unstamped heading '%s'" title))
          (t "an unstamped heading"))))

(defun org-canvas--diff-moved-line (entry)
  "Return the report line of MOVED ENTRY, a module item (issue #294).
Stamping the item's id on the heading in its new module, which `s'
does (issue #342), makes the next sync recreate the item there with a
new id and delete this copy, rather than leave it behind: Canvas
cannot move an item between modules (issues #105, #343).  An entry
paired by title only says so (issue #299)."
  (format "  MOVED     %s (item id %s sits in module '%s'; %s places it in '%s', so the next sync creates it there and leaves this copy — stamp CANVAS_ID %s on that heading and the next sync instead recreates it there with a new id and deletes this copy%s)\n"
          (plist-get entry :title) (plist-get entry :id)
          (plist-get entry :where) (org-canvas--diff-partner-text entry)
          (plist-get entry :to) (plist-get entry :id)
          (if (plist-get entry :by-title)
              (concat "; " org-canvas--diff-by-title-note)
            "")))

(defun org-canvas--diff-relocate-line (entry)
  "Return the report line of RELOCATE ENTRY, a module item (issue #343)."
  (format "  RELOCATE  %s (item id %s sits in module '%s'; its heading, stamped with that id, is under '%s', so the next sync recreates it there with a new id and deletes this copy)\n"
          (plist-get entry :title) (plist-get entry :id)
          (plist-get entry :where) (plist-get entry :to)))

(defun org-canvas--diff-container (entry)
  "Return the container of ENTRY, for the text of its row, or nil.
A module item's row names its module (issue #177), a New Quiz item's
its quiz (issue #322), each by kind and title; an entry of its own is
in no container."
  (when-let* ((where (plist-get entry :where)))
    (format "%s '%s'" (or (plist-get entry :container) "module") where)))

(defun org-canvas--diff-where (entry prefix)
  "Return PREFIX, a space and ENTRY's container, or \"\" for none."
  (let ((container (org-canvas--diff-container entry)))
    (if container (concat prefix " " container) "")))

(defun org-canvas--diff-pending-line (entry)
  "Return the report line of PENDING ENTRY, a create for the next sync."
  (format "  PENDING   %s (no %s%s; the next sync creates it)\n"
          (plist-get entry :title)
          (or (plist-get entry :property) "CANVAS_ID")
          (org-canvas--diff-where entry ", in")))

(defun org-canvas--diff-insert-entry (entry)
  "Insert one ENTRY of a drift report at point."
  (pcase (plist-get entry :kind)
    ('missing
     (insert (format "  MISSING   %s (id %s is not in %s)\n"
                     (plist-get entry :title) (plist-get entry :id)
                     ;; An item is missing from its quiz (#322).
                     (or (org-canvas--diff-container entry) "this course"))))
    ('extra
     (insert (format "  EXTRA     %s (id %s%s, no Org heading claims it)\n"
                     (plist-get entry :title) (plist-get entry :id)
                     ;; A module item says which module it sits in (#177).
                     (org-canvas--diff-where entry " in")))
     ;; What the object is, from the list reply already read (#296).
     (when-let* ((details (plist-get entry :details)))
       (insert (format "              %s\n" details))))
    ('stale-ack
     (insert (format "  STALE-ACK id %s is acknowledged in org-canvas-diff-known-extras but no longer exists on Canvas — remove the entry%s\n"
                     (plist-get entry :id)
                     (let ((note (plist-get entry :note)))
                       (if note (format " (%s)" note) "")))))
    ('unclaimed (insert (org-canvas--diff-unclaimed-line entry)))
    ('moved (insert (org-canvas--diff-moved-line entry)))
    ('relocate (insert (org-canvas--diff-relocate-line entry)))
    ('pending (insert (org-canvas--diff-pending-line entry)))
    ('note
     (insert (format "  NOTE      %s (%s on Canvas is %s; no %s declares it — informational, not drift)\n"
                     (plist-get entry :title) (plist-get entry :observed)
                     (plist-get entry :remote) (plist-get entry :property))))
    ('modified
     (insert (format "  CHANGED   %s%s%s\n"
                     (plist-get entry :title)
                     (org-canvas--diff-where entry " in")
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

(defun org-canvas--diff-suppressed-parts (result)
  "Return the \"Not checked\" phrases of one feature RESULT, nils included.
What its `:skip-fn' held back, and the New Quizzes' assignments taken
out of the Assignments extras (:covered, issue #313)."
  (let ((name (plist-get result :name)))
    (mapcar (lambda (pair)
              (let ((n (car pair)))
                (when (and n (> n 0))
                  (format "%d %s (%s)" n name (cdr pair)))))
            (list (cons (plist-get result :suppressed)
                        (or (plist-get result :skip-reason)
                            "excluded by this module"))
                  (cons (plist-get result :covered)
                        org-canvas--diff-new-quiz-covered-reason)))))

(defun org-canvas--diff-suppressed-note (results)
  "Return a line naming remote items no check in RESULTS covered, or nil.
Items a `:skip-fn' holds back are not compared and cannot
show up as extra, so without this the report reads as full coverage."
  (let ((parts (delq nil (mapcan #'org-canvas--diff-suppressed-parts results))))
    (when parts
      (format "Not checked: %s.\n" (mapconcat #'identity parts ", ")))))

(defun org-canvas--diff-pending-count (results)
  "Return the count of PENDING rows across RESULTS (issue #294)."
  (apply #'+ (mapcar (lambda (r) (length (plist-get r :pending))) results)))

(defun org-canvas--diff-relocate-count (results)
  "Return the count of RELOCATE rows across RESULTS (issue #343)."
  (apply #'+ (mapcar (lambda (r) (length (plist-get r :relocate))) results)))

(defun org-canvas--diff-apart-text (notes pending relocate)
  "Return the section header's counts of rows that are not drift.
NOTES, PENDING and RELOCATE are the section's lists of each."
  (concat (if notes (format ", %d note(s)" (length notes)) "")
          (if pending (format ", %d pending create(s)" (length pending)) "")
          (if relocate (format ", %d relocation(s)" (length relocate)) "")))

(defun org-canvas--diff-weight-totals-line (totals)
  "Return the report line of the group weight TOTALS, (ORG . CANVAS).
Printed under the Assignment Groups section and not counted as drift
\(issue #296): each cause of the difference is a row of its own, or a
pending create, or the stock group the footer names."
  (format "  WEIGHTS   Org's groups sum to %s%%, Canvas's to %s%% (a group only on Canvas, a weight not yet pushed, or a group not yet created; matters when the course weights final grades by group — not counted as drift)\n"
          (org-canvas--diff-number (car totals))
          (org-canvas--diff-number (cdr totals))))

(defun org-canvas--diff-render-section (result)
  "Insert the section of one feature RESULT with rows.
Divergences and extras come first, under a count of them; the notes
\(issue #293), pending creates (issue #294) and relocations (issue
#343) follow, counted apart, since they are not drift."
  (let* ((rows (append (plist-get result :divergences) (plist-get result :extra)))
         (notes (plist-get result :notes))
         (pending (plist-get result :pending))
         (relocate (plist-get result :relocate)))
    (insert (format "%s: %d divergence(s)%s\n"
                    (plist-get result :name) (length rows)
                    (org-canvas--diff-apart-text notes pending relocate)))
    (dolist (entry (append rows notes pending relocate))
      (org-canvas--diff-insert-row (plist-get result :name) entry))
    (when-let* ((totals (plist-get result :weight-totals)))
      (insert (org-canvas--diff-weight-totals-line totals)))
    (insert "\n")))

(defun org-canvas--diff-insert-relocate-footer (results)
  "Insert the footer line counting the RELOCATE rows of RESULTS, if any."
  (let ((n (org-canvas--diff-relocate-count results)))
    (when (> n 0)
      (insert (format "Relocations: %d module item%s the next sync recreates in %s heading's module with a new id, deleting the old copy (not counted as drift).\n"
                      n (if (= n 1) "" "s") (if (= n 1) "its" "their"))))))

(defun org-canvas--diff-render (results)
  "Render RESULTS into a report string."
  (with-temp-buffer
    (insert "org-canvas Drift Report\n")
    (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
    (insert (make-string 60 ?=) "\n\n")
    (dolist (result results)
      (let ((err (plist-get result :error)))
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
         ((or (plist-get result :divergences) (plist-get result :extra)
              (plist-get result :notes) (plist-get result :pending)
              (plist-get result :relocate) (plist-get result :weight-totals))
          (org-canvas--diff-render-section result)))))
    (let ((total (org-canvas--diff-count results))
          (pending (org-canvas--diff-pending-count results)))
      (insert (make-string 60 ?=) "\n")
      (insert (if (zerop total)
                  "No drift: Canvas matches the Org files.\n"
                (format "%d divergence(s) found.\n" total)))
      (when (> pending 0)
        (insert (format "Pending creates: %d heading%s with no Canvas id, which the next sync creates (not counted as drift).\n"
                        pending (if (= pending 1) "" "s"))))
      (org-canvas--diff-insert-relocate-footer results)
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
    (define-key map (kbd "b") #'org-canvas-diff-browse)
    (define-key map (kbd "B") #'org-canvas-diff-browse-edit)
    (define-key map (kbd "a") #'org-canvas-diff-acknowledge)
    (define-key map (kbd "k") #'org-canvas-diff-delete)
    (define-key map (kbd "p") #'org-canvas-diff-pull)
    (define-key map (kbd "s") #'org-canvas-diff-stamp-move)
    (define-key map (kbd "S") #'org-canvas-diff-stamp-moves)
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
\\[org-canvas-diff-browse] opens the Canvas page of any row whose object
Canvas still holds, and \\[org-canvas-diff-browse-edit] its edit page;
\\[org-canvas-diff-acknowledge] acknowledges the EXTRA at point (or drops a
STALE-ACK), or on a CHANGED row with no compared property differing
adopts the Canvas timestamp as the heading's baseline
\(`org-canvas-diff-adopt-stamp'); \\[org-canvas-diff-delete] deletes the
remote object of an EXTRA, UNCLAIMED or MOVED row, after confirming;
\\[org-canvas-diff-pull] pulls a CHANGED row's item over the heading, or
an EXTRA quiz or New Quiz into a new heading;
\\[org-canvas-diff-stamp-move] stamps a MOVED row's item id on the heading it
was paired with, and \\[org-canvas-diff-stamp-moves] every MOVED row's,
sending nothing;
\\[org-canvas-diff-refresh] runs the report again;
\\[org-canvas-diff-next-row] and \\[org-canvas-diff-previous-row] move
between rows."
  (setq-local revert-buffer-function (lambda (&rest _) (org-canvas-diff))))

(defconst org-canvas--diff-key-legend
  "RET visit   b/B browse/edit on Canvas   a acknowledge/adopt   k delete   p pull   s/S stamp moved   g refresh   TAB next row\n"
  "One line naming the report's keys, shown in the interactive buffer only.")

(defun org-canvas--diff-row-at-point ()
  "Return the (:feature NAME :entry ENTRY) plist of the row at point.
Signals a `user-error' off any row."
  (or (get-text-property (point) 'org-canvas-diff-row)
      (user-error "No report row at point")))

(defun org-canvas--diff-row-feature (row)
  "Return the feature registry entry for ROW, or signal.
A New Quiz row's entry is its pull-only one (issue #313)."
  (or (org-canvas--diff-find-feature (plist-get row :feature))
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

(defun org-canvas--diff-pending-position (entry)
  "Return (FILE . POSITION) of the heading of PENDING row ENTRY, or nil.
The row carries the file and line the heading was read from; when the
heading there is no longer the one named, it is looked up by title."
  (let ((file (plist-get entry :file))
        (heading (plist-get entry :heading)))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (or (plist-get entry :line) 1)))
          (let ((pos (if (and (org-at-heading-p)
                              (equal (org-get-heading t t t t) heading))
                         (point)
                       (org-canvas--find-heading-in-file file heading))))
            (and pos (cons file pos))))))))

(defun org-canvas--diff-relocate-position (entry)
  "Return (FILE . POSITION) of the heading of RELOCATE row ENTRY, or nil.
The heading carries the row's item id (issue #343)."
  (let* ((file (plist-get entry :file))
         (pos (org-canvas--diff-find-heading-by-id file "CANVAS_ID"
                                                   (plist-get entry :id))))
    (and pos (cons file pos))))

(defun org-canvas--diff-heading-position (feature entry)
  "Return (FILE . POSITION) of the heading ENTRY of FEATURE describes, or nil.
MISSING, CHANGED and NOTE rows are found by their id property; an UNCLAIMED
row names an unstamped heading, found by title; a PENDING row names
its own file and line (issue #294), as every New Quiz item row does
\(issue #322): an item heading is no feature's own; a RELOCATE row
names its own file and id (issue #343)."
  (cond ((eq (plist-get entry :kind) 'relocate)
         (org-canvas--diff-relocate-position entry))
        ((or (eq (plist-get entry :kind) 'pending) (plist-get entry :file))
         (org-canvas--diff-pending-position entry))
        (t (org-canvas--diff-feature-heading-position feature entry))))

(defun org-canvas--diff-feature-heading-position (feature entry)
  "Return (FILE . POSITION) of ENTRY's heading in FEATURE's file, or nil."
  (let* ((file-var (plist-get feature :file-var))
         (file (and file-var (boundp file-var) (symbol-value file-var)))
         (pos (pcase (plist-get entry :kind)
                ('unclaimed
                 (org-canvas--find-heading-in-file file (plist-get entry :title)))
                ((or 'missing 'modified 'note)
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
                  (plist-get entry :title)
                  (or (plist-get feature :name) (plist-get entry :file))))
    (set-buffer (org-canvas--find-file-noselect (car where)))
    (goto-char (cdr where))
    (org-back-to-heading t)
    (current-buffer)))

(defun org-canvas--diff-visits-heading-p (entry)
  "Return non-nil for a report ENTRY with an Org heading to visit.
A module item's UNCLAIMED row names its Canvas item, whose heading
has no title of its own to find it by."
  (pcase (plist-get entry :kind)
    ((or 'missing 'modified 'note 'pending 'relocate) t)
    ('unclaimed (not (plist-get entry :module-id)))))

(defun org-canvas-diff-visit ()
  "Visit what the row at point is about.
A row with an Org heading (MISSING, CHANGED, UNCLAIMED, NOTE, PENDING,
RELOCATE) opens the course file with point on that heading.  An EXTRA or MOVED
row, or a module item's UNCLAIMED row, names a Canvas object, which is
opened in a browser: where the API said it lives, or else the page
the feature's registered rules name (issue #313)."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry)))
    (if (org-canvas--diff-visits-heading-p entry)
        ;; A PENDING or RELOCATE row carries its own file (#294, #343),
        ;; as a New Quiz item's row does (issue #322).
        (let* ((feature (unless (or (memq (plist-get entry :kind)
                                          '(pending relocate))
                                    (plist-get entry :file))
                          (org-canvas--diff-row-feature row)))
               (buf (save-excursion (org-canvas--diff-goto-heading feature entry)))
               (pos (with-current-buffer buf (point))))
          (pop-to-buffer buf)
          (goto-char pos)
          (when (fboundp 'org-fold-show-context) (org-fold-show-context)))
      ;; The quiz service gives a New Quiz no `html_url'; the page its
      ;; registered rules name serves instead (issue #313).
      (browse-url (org-canvas--diff-row-web-url row nil)))))

;; RET on a row with a heading visits the heading, so a row that exists
;; on both sides had no way to the Canvas page at all (issue #292).

(defun org-canvas--diff-web-entry (feature)
  "Return the entry naming where FEATURE's items live on the Canvas site.
FEATURE itself when it declares `:web-pages'; otherwise what its file
registered through `org-canvas-register-web-pages', as a New Quiz's
pull-only entry does (issues #292, #313), or FEATURE when there is
nothing; nil for no FEATURE."
  (when feature
    (if (plist-get feature :web-pages)
        feature
      (let* ((var (plist-get feature :file-var))
             (file (and var (boundp var) (symbol-value var))))
        (or (org-canvas--web-pages-for-file file) feature)))))

(defun org-canvas--diff-row-web-url (row edit)
  "Return the Canvas web address of ROW's object, or signal.
With EDIT, the edit page the feature's `:web-pages' names, falling
back to the object's page; otherwise the `html_url' Canvas gave, or
the page assembled from the registry when it gave none.  A New Quiz
item has no page of its own, so its row opens its quiz's (issue #322)."
  (let* ((entry (plist-get row :entry))
         (quiz-id (plist-get entry :quiz-id))
         (feature (org-canvas--diff-web-entry
                   (org-canvas--diff-find-feature
                    (if quiz-id "New Quizzes" (plist-get row :feature)))))
         (id (or quiz-id (plist-get entry :id)))
         (html (plist-get entry :html-url))
         (assembled (and feature id
                         (org-canvas--feature-web-url feature id edit))))
    (or (if edit (or assembled html) (or html assembled))
        (user-error "No Canvas page is known for %s '%s'"
                    (plist-get row :feature)
                    (or (plist-get entry :title) id)))))

(defun org-canvas-diff-browse (&optional edit)
  "Open the Canvas page of the row at point in a browser.
With a prefix argument EDIT, open its edit page where Canvas has one.
A MISSING or STALE-ACK row names an object Canvas no longer holds,
and a PENDING row one it does not hold yet, so there is nothing to
open.  Sends nothing to Canvas.  Returns the address opened."
  (interactive "P")
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry)))
    (pcase (plist-get entry :kind)
      ((or 'missing 'stale-ack)
       (user-error "'%s' is not on Canvas any more; nothing to open"
                   (or (plist-get entry :title) (plist-get entry :id))))
      ('pending
       (user-error "'%s' is not on Canvas yet; the next sync creates it"
                   (plist-get entry :title))))
    (let ((url (org-canvas--diff-row-web-url row edit)))
      (browse-url url)
      url)))

(defun org-canvas-diff-browse-edit ()
  "Open the Canvas edit page of the row at point in a browser."
  (interactive)
  (org-canvas-diff-browse t))

(defun org-canvas-diff-acknowledge ()
  "Acknowledge the EXTRA, UNCLAIMED or MOVED row at point as deliberate.
Adds it to `org-canvas-diff-known-extras', with a note if you give one,
and persists the list through `org-canvas-diff-acknowledge-function'.
On a STALE-ACK row, drops the acknowledgment instead.  On a CHANGED
row with no compared property differing, adopts the Canvas timestamp
as the heading's baseline (`org-canvas-diff-adopt-stamp', issue #257)."
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
      ((or 'extra 'unclaimed 'moved)
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
      ;; On a CHANGED row, accepting what Canvas holds means adopting
      ;; its timestamp — when nothing compared differs (issue #257).
      ('modified (org-canvas-diff-adopt-stamp))
      (kind (user-error "A %s row is not something to acknowledge" (upcase (symbol-name kind)))))))

;;;; Deleting Remote Objects (issues #103, #345)
;;
;; `k' deleted a row's Canvas object behind one `y-or-n-p', and nothing
;; else could: a batch caller cleaning up after a calendar reshuffle
;; (nine EXTRA rows, issue #345) rebuilt the delete target by hand and
;; wrote its own safety checks and snapshots.  Both paths now share
;; them.  Before any DELETE the object is read — at the URL the delete
;; goes to, with `include[]=associations' for a rubric — and two kinds
;; of object are held back, naming why: an assignment with a submission
;; or a score (its gradebook column goes with it), and a rubric an
;; assignment grades with (its assessments go with it).  `k' shows the
;; reason in its question; `org-canvas-diff-delete-rows' refuses unless
;; asked with :force.  The object's JSON is then written to a snapshot
;; file, so a mistaken delete can be rebuilt, and only then is the
;; DELETE sent.  The report itself still never writes (Hard Rule 19):
;; these are separate, explicit verbs.

(defcustom org-canvas-diff-delete-snapshot-directory nil
  "Directory for the JSON of each object a drift-report delete removes.
Nil means canvas-snapshots/ under `org-canvas-directory', resolved
when used rather than when the package loads.  Each file is named
<time>-<feature>-<id>.json and holds the object as Canvas returned it
just before the DELETE (issue #345)."
  :type '(choice (const :tag "canvas-snapshots/ under org-canvas-directory" nil)
                 directory)
  :group 'org-canvas)

(defun org-canvas--diff-snapshot-dir ()
  "Return the snapshot directory as an absolute path."
  (expand-file-name (or org-canvas-diff-delete-snapshot-directory
                        (org-canvas--path "canvas-snapshots/"))))

(defun org-canvas--diff-snapshot-write (feature-name id object)
  "Write OBJECT, FEATURE-NAME's item ID, to a snapshot file; return its path."
  (let* ((dir (org-canvas--diff-snapshot-dir))
         (file (expand-file-name
                (format "%s-%s-%s.json"
                        (format-time-string "%Y%m%dT%H%M%S")
                        (replace-regexp-in-string
                         "[^[:alnum:]]+" "-" (downcase feature-name))
                        (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" id))
                dir)))
    (make-directory dir t)
    (let ((coding-system-for-write 'utf-8)
          (json-encoding-pretty-print t))
      (write-region (concat (json-encode object) "\n") nil file nil 'silent))
    file))

(defun org-canvas--diff-delete-target (row entry)
  "Return (URL . DELETE-DATA) for the remote object ENTRY of ROW names.
A module item lives under its module, not at a feature URL (issue
#177), and a New Quiz item under its quiz at the quiz service (issue
#322); anything else uses the feature's item URL and delete body, as
orphan cleanup does."
  (let ((id (plist-get entry :id)))
    (cond
     ((plist-get entry :module-id)
      (cons (org-canvas-api-course-endpoint "modules/%s/items/%s"
                                            (plist-get entry :module-id) id)
            nil))
     ((plist-get entry :quiz-id)
      (cons (org-canvas--new-quiz-api-endpoint "quizzes/%s/items/%s"
                                               (plist-get entry :quiz-id) id)
            nil))
     (t
      (let ((feature (org-canvas--diff-row-feature row)))
        (cons (org-canvas--feature-item-url feature id)
              (plist-get feature :delete-data)))))))

(defun org-canvas--diff-delete-read-params (row entry)
  "Return the query parameters that read ENTRY of ROW before its delete.
A rubric is read with its associations, which its guard counts; any
other feature with the parameters it reads one item with.  A module
item or a New Quiz item (issue #322) is read with none: neither is a
feature's own."
  (unless (or (plist-get entry :module-id) (plist-get entry :quiz-id))
    (if (string= (org-canvas--diff-normalize-name (plist-get row :feature))
                 "rubrics")
        '(("include[]" . "associations"))
      (org-canvas--feature-item-params (org-canvas--diff-row-feature row)))))

(defun org-canvas--diff-submitted-p (submission)
  "Return non-nil when a student turned in work on SUBMISSION."
  (or (alist-get 'submitted_at submission)
      (member (alist-get 'workflow_state submission)
              '("submitted" "pending_review"))))

(defun org-canvas--diff-scored-p (submission)
  "Return non-nil when SUBMISSION carries a score or a grade."
  (or (alist-get 'score submission) (alist-get 'grade submission)))

(defun org-canvas--diff-delete-assignment-guard (entry object)
  "Return why assignment ENTRY must not be deleted, or nil.
OBJECT is the assignment as Canvas holds it.  Lists the assignment's
submissions — one request, paged — and counts those a student turned
in and those carrying a score or grade; Canvas's own
`has_submitted_submissions' flag counts too."
  (let* ((subs (cl-remove-if-not
                #'consp
                (append (org-canvas-api-request-all-pages
                         'GET (org-canvas-api-course-endpoint
                               "assignments/%s/submissions"
                               (plist-get entry :id)))
                        nil)))
         (submitted (cl-count-if #'org-canvas--diff-submitted-p subs))
         (scored (cl-count-if #'org-canvas--diff-scored-p subs)))
    (cond ((or (> submitted 0) (> scored 0))
           (format "the assignment has %d submission(s) and %d score(s)"
                   submitted scored))
          ((eq (alist-get 'has_submitted_submissions object) t)
           "Canvas says the assignment has submissions"))))

(defun org-canvas--diff-delete-rubric-guard (_entry object)
  "Return why the rubric OBJECT must not be deleted, or nil.
OBJECT was read with its associations; an Assignment association means
an assignment grades with the rubric, and its assessments would go."
  (let ((ids (delq nil
                   (mapcar (lambda (a)
                             (and (consp a)
                                  (equal (alist-get 'association_type a)
                                         "Assignment")
                                  (format "%s" (alist-get 'association_id a))))
                           (append (alist-get 'associations object) nil)))))
    (when ids
      (format "the rubric grades assignment(s) %s"
              (mapconcat #'identity ids ", ")))))

(defconst org-canvas--diff-delete-guards
  '(("assignments" . org-canvas--diff-delete-assignment-guard)
    ("newquizzes" . org-canvas--diff-delete-assignment-guard)
    ("rubrics" . org-canvas--diff-delete-rubric-guard))
  "Safety checks before a delete, by normalized feature name.
Each a function of (ENTRY OBJECT) returning why the object must stay,
or nil (issue #345).  A New Quiz row's id is its assignment's (issue
#313), so the assignment's check counts the quiz's submissions.")

(defun org-canvas--diff-delete-inspect (row entry)
  "Read the object ENTRY of ROW names; return (OBJECT . REASON).
REASON says why the object should not be deleted, or is nil: the
feature's guard (`org-canvas--diff-delete-guards'), or an empty reply,
which leaves nothing to snapshot.  A module item has no guard."
  (let* ((params (org-canvas--diff-delete-read-params row entry))
         (object (apply #'org-canvas-api-request 'GET
                        (car (org-canvas--diff-delete-target row entry))
                        (and params (list :params params))))
         (guard (and (not (plist-get entry :module-id))
                     (alist-get (org-canvas--diff-normalize-name
                                 (plist-get row :feature))
                                org-canvas--diff-delete-guards
                                nil nil #'string=))))
    (cons object
          (if (null object)
              "Canvas returned nothing to snapshot"
            (and guard (funcall guard entry object))))))

(defun org-canvas--diff-delete-execute (row entry object)
  "Snapshot OBJECT, then delete the object ENTRY of ROW names.
Returns the snapshot's path.  A course marked read-only is refused
before the snapshot is written, as the transport would refuse the
DELETE (issue #163)."
  (org-canvas--check-writable 'DELETE "a drift-report delete")
  (let* ((target (org-canvas--diff-delete-target row entry))
         (delete-data (cdr target))
         (file (org-canvas--diff-snapshot-write
                (plist-get row :feature) (plist-get entry :id) object)))
    (apply #'org-canvas-api-request 'DELETE (car target)
           (and delete-data (list :data delete-data)))
    (org-canvas--log-info org-canvas--logger
      "[Diff] Deleted %s #%s '%s'; its JSON is in %s"
      (plist-get row :feature) (plist-get entry :id)
      (plist-get entry :title) file)
    file))

(defun org-canvas--diff-delete-question (name entry reason)
  "Return the question `k' asks before deleting ENTRY of feature NAME.
REASON, when non-nil, is what the safety check found."
  (format "Delete %s '%s' (id %s) from Canvas%s? "
          name (plist-get entry :title) (plist-get entry :id)
          (if reason (format ", although %s" reason) "")))

(defun org-canvas-diff-delete ()
  "Delete the EXTRA, UNCLAIMED or MOVED row's Canvas object, after confirming.
Uses the feature's item URL and delete body, as orphan cleanup does;
a module item's row deletes the item from its module (issue #177) —
for a MOVED row, the copy left in the old module (issue #294).  The
object is read first: the question names an assignment's submissions
and scores or a rubric's assignments, and the object's JSON is saved
under `org-canvas-diff-delete-snapshot-directory' before the DELETE
\(issue #345)."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (id (plist-get entry :id))
         (name (plist-get row :feature)))
    (unless (memq (plist-get entry :kind) '(extra unclaimed moved))
      (user-error "Only an EXTRA, UNCLAIMED or MOVED row names a remote object to delete"))
    (let ((inspect (org-canvas--diff-delete-inspect row entry)))
      (when (y-or-n-p (org-canvas--diff-delete-question
                       name entry (cdr inspect)))
        (let ((file (org-canvas--diff-delete-execute row entry (car inspect))))
          (org-canvas--diff-rewrite-row
           (format "  DELETED   %s (id %s)" (plist-get entry :title) id))
          (message "Deleted %s %s; its JSON is in %s." name id file))))))

;;;;; Deleting Rows Without Asking (issue #345)

(defun org-canvas--diff-result-entries (result)
  "Return every entry RESULT reports: rows, notes, pending and relocations.
A RELOCATE row (issue #343) is among them so a selector naming one is
told it is not an EXTRA row, rather than that the report has no row."
  (append (plist-get result :divergences) (plist-get result :extra)
          (plist-get result :notes) (plist-get result :pending)
          (plist-get result :relocate)))

(defun org-canvas--diff-selector-outcome (feature id outcome reason)
  "Return the outcome plist of a selector that named no deletable row.
FEATURE and ID are what the selector named, OUTCOME the symbol and
REASON the sentence."
  (list :feature feature :id id :outcome outcome :reason reason))

(defun org-canvas--diff-select-by-id (result feature id)
  "Return the row of RESULT whose entry carries ID, or an outcome plist.
FEATURE is the selector's feature name.  Only an EXTRA row is
returned as a row, (:feature NAME :entry ENTRY); any other kind comes
back as a `not-extra' outcome, no row at all as `not-found'."
  (let ((entry (cl-find-if (lambda (e) (equal (plist-get e :id) id))
                           (org-canvas--diff-result-entries result))))
    (cond ((null entry)
           (org-canvas--diff-selector-outcome
            feature id 'not-found "the report has no row with this id"))
          ((eq (plist-get entry :kind) 'extra)
           (list :feature (plist-get result :name) :entry entry))
          (t
           (org-canvas--diff-selector-outcome
            feature id 'not-extra
            (format "a %s row, and only an EXTRA row is deleted"
                    (upcase (symbol-name (plist-get entry :kind)))))))))

(defun org-canvas--diff-select (results selector)
  "Return what SELECTOR names in RESULTS, a list of rows and outcomes.
SELECTOR is (:feature NAME :id ID), one row, or (:feature NAME :all t),
every EXTRA row of the feature.  A row is (:feature NAME :entry ENTRY);
a selector that names no deletable row yields an outcome plist."
  (let* ((feature (plist-get selector :feature))
         (id (and (plist-get selector :id)
                  (format "%s" (plist-get selector :id))))
         (result (and feature
                      (cl-find-if (lambda (r)
                                    (string= (org-canvas--diff-normalize-name
                                              (plist-get r :name))
                                             (org-canvas--diff-normalize-name
                                              feature)))
                                  results))))
    (cond ((null result)
           (list (org-canvas--diff-selector-outcome
                  feature id 'not-found "the report has no such feature")))
          ((plist-get selector :all)
           (mapcar (lambda (e)
                     (list :feature (plist-get result :name) :entry e))
                   (cl-remove-if-not
                    (lambda (e) (eq (plist-get e :kind) 'extra))
                    (plist-get result :extra))))
          (t (list (org-canvas--diff-select-by-id result feature id))))))

(defun org-canvas--diff-delete-row (row force)
  "Delete ROW's Canvas object without asking; return its outcome plist.
The outcome is `deleted' (with :snapshot, the JSON's path), `refused'
when a safety check held it back and FORCE is nil, `dry-run' under
`org-canvas--dry-run', which sends and writes nothing, or `failed'
when a request did.  :reason names what the check found, forced or
not."
  (let* ((entry (plist-get row :entry))
         (out (list :feature (plist-get row :feature) :id (plist-get entry :id)
                    :title (plist-get entry :title))))
    (condition-case err
        (let* ((inspect (org-canvas--diff-delete-inspect row entry))
               (reason (cdr inspect)))
          (cond
           ((and reason (not force))
            (append out (list :outcome 'refused :reason reason)))
           (org-canvas--dry-run
            (org-canvas--log-info org-canvas--logger
              "[DRY-RUN] Would delete %s #%s '%s'" (plist-get row :feature)
              (plist-get entry :id) (plist-get entry :title))
            (append out (list :outcome 'dry-run :reason reason)))
           (t
            (append out (list :outcome 'deleted :reason reason
                              :snapshot (org-canvas--diff-delete-execute
                                         row entry (car inspect)))))))
      (error
       (org-canvas--log-error org-canvas--logger
         "[Diff] Could not delete %s #%s: %s" (plist-get row :feature)
         (plist-get entry :id) (error-message-string err))
       (append out (list :outcome 'failed
                         :reason (error-message-string err)))))))

(defun org-canvas--diff-delete-log-outcome (outcome)
  "Log one OUTCOME plist of `org-canvas-diff-delete-rows'."
  (org-canvas--log-info org-canvas--logger
    "[Diff] %s %s #%s%s%s"
    (upcase (symbol-name (plist-get outcome :outcome)))
    (plist-get outcome :feature) (plist-get outcome :id)
    (if (plist-get outcome :title)
        (format " '%s'" (plist-get outcome :title))
      "")
    (if (plist-get outcome :reason)
        (format ": %s" (plist-get outcome :reason))
      "")))

(defun org-canvas--diff-outcome-count (outcomes &rest kinds)
  "Return how many of OUTCOMES have an :outcome among KINDS."
  (cl-count-if (lambda (o) (memq (plist-get o :outcome) kinds)) outcomes))

;;;###autoload
(cl-defun org-canvas-diff-delete-rows (selectors &key force results)
  "Delete the Canvas objects of the drift report's EXTRA rows SELECTORS names.
Never asks.  Each selector is (:feature NAME :id ID), one row, or
\(:feature NAME :all t), every EXTRA row of the feature; NAME matches
the way the report's sections do (\"Module Items\", \"assignments\").
The rows come from the report's own comparison — RESULTS when given,
as `org-canvas--diff-collect-results' returns them, else read afresh —
and each object is deleted at the URL `k' would use.

Only an EXTRA row is deleted: an UNCLAIMED or MOVED row has a heading
the next sync would pair with it.  Before each DELETE the object is
read, and an assignment with a submission or score, or a rubric an
assignment grades with, is refused, naming why, unless FORCE is
non-nil; the object's JSON is then written under
`org-canvas-diff-delete-snapshot-directory'.  A dry run
\(`org-canvas--dry-run') reads and checks but writes and sends
nothing; a course marked read-only refuses every delete.

Returns one plist per row or unmatched selector: :feature, :id,
:title, :outcome — `deleted', `refused', `dry-run', `failed',
`not-extra' or `not-found' — :reason and, once deleted, :snapshot
\(issue #345)."
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let ((results (or results (org-canvas--diff-collect-results)))
        (seen nil)
        (outcomes nil))
    (dolist (selector selectors)
      (dolist (row (org-canvas--diff-select results selector))
        (let ((key (cons (plist-get row :feature)
                         (plist-get (plist-get row :entry) :id))))
          (cond ((not (plist-get row :entry)) (push row outcomes))
                ((member key seen))
                (t (push key seen)
                   (push (org-canvas--diff-delete-row row force) outcomes))))))
    (setq outcomes (nreverse outcomes))
    (mapc #'org-canvas--diff-delete-log-outcome outcomes)
    (message "Drift delete: %d deleted, %d refused, %d left for another reason"
             (org-canvas--diff-outcome-count outcomes 'deleted)
             (org-canvas--diff-outcome-count outcomes 'refused)
             (org-canvas--diff-outcome-count
              outcomes 'dry-run 'failed 'not-extra 'not-found))
    outcomes))

(defun org-canvas--diff-pull-extra (feature entry)
  "Pull the item of EXTRA row ENTRY into a new heading of FEATURE's file.
Only a feature that pulls whole entries (`:pull-whole-entry', classic
quizzes) can write one from nothing; a module item belongs under its
module heading, which a module pull writes.  Returns non-nil when the
item was pulled."
  (unless (and (plist-get feature :pull-whole-entry)
               (not (plist-get entry :module-id)))
    (user-error "%s cannot pull '%s' into a new heading; use M-x org-canvas-pull-%s"
                (plist-get feature :name) (plist-get entry :title)
                (downcase (replace-regexp-in-string
                           " " "-" (plist-get feature :name)))))
  (save-excursion
    (org-canvas--pull-new-heading feature (plist-get entry :id)
                                  (plist-get entry :title))))

(defun org-canvas--diff-pull-quiz-of (entry)
  "Pull the New Quiz of item row ENTRY over its quiz heading.
An item is part of its quiz, and the quiz pulls whole, its items
included (issue #322), so the quiz heading of CANVAS_ASSIGNMENT_ID
:quiz-id is what `org-canvas-pull-at-point' runs on.  Asks first."
  (let ((feature (org-canvas--diff-find-feature "New Quizzes")))
    (save-excursion
      (with-current-buffer (org-canvas--diff-goto-heading
                            feature (list :kind 'modified
                                          :id (plist-get entry :quiz-id)
                                          :title (plist-get entry :where)))
        (org-canvas-pull-at-point)
        t))))

(defun org-canvas--diff-pulled-line (entry)
  "Return the text to put in place of ENTRY's row once it is pulled."
  (format "  PULLED    %s (id %s%s)" (plist-get entry :title) (plist-get entry :id)
          (cond ((plist-get entry :quiz-id)
                 (format ", with %s" (org-canvas--diff-container entry)))
                ((eq (plist-get entry :kind) 'extra) ", new heading")
                (t ""))))

(defun org-canvas-diff-pull ()
  "Pull the item of the row at point from Canvas into Org.
On a CHANGED row, runs `org-canvas-pull-at-point' on the heading.  On
an EXTRA row of a feature that pulls whole entries (classic and New
Quizzes), writes the item as a new heading at the end of its file
\(issues #295, #313).  On a New Quiz item's CHANGED or EXTRA row,
pulls the quiz it sits in (issue #322).  Each asks first."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (kind (plist-get entry :kind))
         (pulled
          (cond
           ((not (memq kind '(modified extra)))
            (user-error "Only a CHANGED or EXTRA row has a Canvas version to pull"))
           ((plist-get entry :quiz-id) (org-canvas--diff-pull-quiz-of entry))
           ((eq kind 'modified)
            (save-excursion
              (with-current-buffer (org-canvas--diff-goto-heading
                                    (org-canvas--diff-row-feature row) entry)
                (org-canvas-pull-at-point)
                t)))
           (t (org-canvas--diff-pull-extra (org-canvas--diff-row-feature row)
                                           entry)))))
    (when pulled
      (org-canvas--diff-rewrite-row (org-canvas--diff-pulled-line entry)))))

;;;; Adopting a Stamp
;;
;; A change Canvas makes on its own moves `updated_at' on entries whose
;; content did not change: a metadata-only file touch (before #94), the
;; rubric-association bump (before #131), and a course-wide post policy
;; change, which rewrote the policy of all 77 assignments in one course
;; (issue #257).  The report is right to list each as CHANGED with no
;; compared property differing, and there is nothing in Org to change;
;; a push would send the same content again.  The only fix is to copy
;; the live timestamp into CANVAS_UPDATED_AT, which was a one-off script
;; three times over.  Adoption is that script: the stamp moves,
;; PAYLOAD_HASH stays (the local side did not change, so the skip must
;; survive), and nothing is sent.  It is a separate key and command,
;; never something the report does by itself (issue #83).

(defun org-canvas--diff-adoptable-p (entry)
  "Return non-nil when ENTRY is a CHANGED row whose stamp can be adopted.
That is one Canvas holds newer than the baseline with no compared
property differing: the change is in a field the report does not
compare, so the heading has nothing to push or pull."
  (and (eq (plist-get entry :kind) 'modified)
       (plist-get entry :remote-newer)
       (null (plist-get entry :fields))
       (stringp (plist-get entry :updated))))

(defun org-canvas--diff-adopt-entry (feature entry)
  "Stamp ENTRY's heading with the timestamp Canvas reports, and save the file.
FEATURE is the registry entry that says which file to look in.  Writes
CANVAS_UPDATED_AT only: PAYLOAD_HASH is left as it is, since the local
content did not change.  Returns the timestamp written, or nil when
the heading could not be found."
  (let ((where (org-canvas--diff-heading-position feature entry))
        (updated (plist-get entry :updated)))
    (when where
      (with-current-buffer (org-canvas--find-file-noselect (car where))
        (save-excursion
          (goto-char (cdr where))
          (org-back-to-heading t)
          (org-canvas-org-set-property (point) "CANVAS_UPDATED_AT" updated)
          (org-canvas--save-buffer)))
      (org-canvas--log-info org-canvas--logger
        "[Diff] Adopted the stamp of %s '%s': CANVAS_UPDATED_AT is now %s, PAYLOAD_HASH kept, nothing sent"
        (plist-get feature :name) (plist-get entry :title) updated)
      updated)))

(defun org-canvas-diff-adopt-stamp ()
  "Adopt the Canvas timestamp of the CHANGED row at point as its baseline.
Only a row with no compared property differing qualifies: Canvas is
newer, but the change is in a field the report does not compare, so
there is nothing to push or pull.  Writes CANVAS_UPDATED_AT on the
heading, keeps PAYLOAD_HASH and sends nothing (issue #257).  A row
where a property differs is refused; push or pull that one instead.
`org-canvas-diff-adopt-stamps' does the same for every such row."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (title (plist-get entry :title)))
    (unless (eq (plist-get entry :kind) 'modified)
      (user-error "Only a CHANGED row has a Canvas timestamp to adopt"))
    ;; Refused before the feature is looked up: a New Quiz item's row
    ;; is never adoptable and names no feature of its own (#322).
    (unless (org-canvas--diff-adoptable-p entry)
      (user-error "A compared property of '%s' differs; push or pull it rather than adopting its stamp"
                  title))
    (let* ((feature (org-canvas--diff-row-feature row))
           (updated (org-canvas--diff-adopt-entry feature entry)))
      (unless updated
        (user-error "Cannot find the heading for '%s' in %s"
                    title (plist-get feature :name)))
      (org-canvas--diff-rewrite-row
       (format "  ADOPTED   %s (CANVAS_UPDATED_AT set to %s, nothing sent)"
               title updated))
      (message "Adopted the stamp of %s '%s'." (plist-get feature :name) title))))

(defconst org-canvas--diff-adopt-buffer-name "*canvas-diff-adopt*"
  "Name of the buffer holding the stamp adoption report.")

(defun org-canvas--diff-adopt-candidates (results)
  "Sort the CHANGED rows of RESULTS into the adoptable and the held back.
Returns (ADOPTABLE . HELD): ADOPTABLE lists (FEATURE-NAME . ENTRY) for
every row with no compared property differing, HELD counts the CHANGED
rows a differing property keeps out."
  (let ((adoptable nil) (held 0))
    (dolist (result results)
      (dolist (entry (plist-get result :divergences))
        (cond ((org-canvas--diff-adoptable-p entry)
               (push (cons (plist-get result :name) entry) adoptable))
              ((eq (plist-get entry :kind) 'modified)
               (setq held (1+ held))))))
    (cons (nreverse adoptable) held)))

(defun org-canvas--diff-adopt-all (candidates)
  "Adopt the stamp of every (FEATURE-NAME . ENTRY) in CANDIDATES.
Returns (ADOPTED . MISSED): ADOPTED lists (FEATURE-NAME TITLE
TIMESTAMP) in order, MISSED lists (FEATURE-NAME . TITLE) for the
headings that could not be found."
  (let ((adopted nil) (missed nil))
    (dolist (candidate candidates)
      (let* ((name (car candidate))
             (entry (cdr candidate))
             (feature (org-canvas--diff-find-feature name))
             (updated (and feature (org-canvas--diff-adopt-entry feature entry))))
        (if updated
            (push (list name (plist-get entry :title) updated) adopted)
          (push (cons name (plist-get entry :title)) missed))))
    (cons (nreverse adopted) (nreverse missed))))

(defun org-canvas--diff-adopt-render (adopted missed held)
  "Insert the stamp adoption report at point.
ADOPTED and MISSED are the halves `org-canvas--diff-adopt-all'
returns, HELD the count of CHANGED rows left alone because a compared
property differs."
  (insert "org-canvas Stamp Adoption\n")
  (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
  (insert (make-string 60 ?=) "\n\n")
  (dolist (a adopted)
    (insert (format "  ADOPTED   %s (%s; CANVAS_UPDATED_AT set to %s)\n"
                    (nth 1 a) (nth 0 a) (nth 2 a))))
  (dolist (m missed)
    (insert (format "  NOT FOUND %s (%s; no heading carries its id)\n" (cdr m) (car m))))
  (when (or adopted missed) (insert "\n"))
  (insert (make-string 60 ?=) "\n")
  (insert (if adopted
              (format "%d stamp(s) adopted; PAYLOAD_HASH kept, nothing sent to Canvas.\n"
                      (length adopted))
            "No stamp to adopt: no CHANGED row has every compared property matching.\n"))
  (when (> held 0)
    (insert (format "%d CHANGED row(s) left alone: a compared property differs — push or pull those.\n"
                    held))))

(defun org-canvas--diff-adopt-confirm (count)
  "Ask whether to adopt COUNT stamps, or signal a `user-error'.
Under `noninteractive' there is nobody to ask, and running the command
was the answer."
  (unless (or noninteractive
              (y-or-n-p (format "Adopt the Canvas timestamp of %d CHANGED entr%s with every compared property matching? "
                                count (if (= count 1) "y" "ies"))))
    (user-error "Aborted; no stamp adopted")))

;;;###autoload
(defun org-canvas-diff-adopt-stamps ()
  "Adopt the Canvas timestamp of every CHANGED entry with nothing to compare.
Runs the drift comparison (`org-canvas-diff' reads, one list request
per feature), then, after confirming, copies the live timestamp into
CANVAS_UPDATED_AT on each heading Canvas holds newer whose compared
properties all match — the rows the report says have their change in a
field it does not compare.  PAYLOAD_HASH is kept and nothing is sent.
A row where a property differs is left alone and counted (issue #257).
The course-wide case is a post policy change, which rewrites every
assignment.  Returns the number of stamps adopted."
  (interactive)
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let* ((results (org-canvas--diff-collect-results))
         (candidates (org-canvas--diff-adopt-candidates results))
         (held (cdr candidates))
         (outcome (cons nil nil)))
    (when (car candidates)
      (org-canvas--diff-adopt-confirm (length (car candidates)))
      (setq outcome (org-canvas--diff-adopt-all (car candidates))))
    (org-canvas--report-display
     org-canvas--diff-adopt-buffer-name
     (lambda () (org-canvas--diff-adopt-render (car outcome) (cdr outcome) held)))
    (message "Drift: %d stamp(s) adopted, %d CHANGED row(s) left alone"
             (length (car outcome)) held)
    (length (car outcome))))

;;;; Stamping a Moved Item (issue #342)
;;
;; A MOVED row named the id to stamp and the heading to stamp it on,
;; and nothing did the stamping: after a course-wide reshuffle that
;; was 48 hand edits, done by a script parsing the report's text, and
;; the renamed pairs — the ones the report had paired by content, not
;; by title — were the ones a title search missed.  The report knows
;; the heading it paired, so `s' stamps the item's id there and `S'
;; (`org-canvas-diff-stamp-moves') every MOVED row's.  CANVAS_ID is the
;; only property written, and nothing is sent: the next sync is what
;; recreates the item in its new module and deletes the old copy, and
;; the report shows each stamped heading as a RELOCATE row until then
;; (issue #343).  Like stamp adoption, it is a verb the report never
;; runs itself (Hard Rule 19).

(defun org-canvas--diff-moved-heading-p (entry)
  "Return non-nil when the heading at point is MOVED ENTRY's partner.
That is an item heading with ENTRY's :partner-heading text and no
CANVAS_ID: a heading stamped since the report is no longer the one it
paired."
  (and (org-at-heading-p)
       (let ((local (org-canvas--diff-local-entry "CANVAS_ID")))
         (and (null (plist-get local :id))
              (equal (plist-get local :heading)
                     (plist-get entry :partner-heading))))))

(defun org-canvas--diff-moved-search (entry)
  "Return the position of MOVED ENTRY's partner under its module, or nil.
The fallback when the heading is no longer on the line the report
read it from: an unstamped level-2 heading of the same text under the
module ENTRY's :to names."
  (catch 'found
    (org-map-entries
     (lambda ()
       (when (and (org-canvas--diff-moved-heading-p entry)
                  (equal (save-excursion
                           (and (org-up-heading-safe)
                                (org-get-heading t t t t)))
                         (plist-get entry :to)))
         (throw 'found (point))))
     "LEVEL=2" 'file)
    nil))

(defun org-canvas--diff-moved-position (entry)
  "Return (FILE . POSITION) of the heading MOVED ENTRY was paired with, or nil.
The row carries the file and line it was read from; when the heading
there is not the one paired, it is looked up under its module
\(`org-canvas--diff-moved-search')."
  (let ((file (plist-get entry :file)))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (or (plist-get entry :line) 1)))
          (let ((pos (if (org-canvas--diff-moved-heading-p entry)
                         (point)
                       (org-canvas--diff-moved-search entry))))
            (and pos (cons file pos))))))))

(defun org-canvas--diff-stamp-at (where entry)
  "Write MOVED ENTRY's item id as CANVAS_ID on the heading at WHERE.
WHERE is (FILE . POSITION).  Saves the file and sends nothing."
  (with-current-buffer (org-canvas--find-file-noselect (car where))
    (save-excursion
      (goto-char (cdr where))
      (org-back-to-heading t)
      (org-canvas-org-set-property (point) "CANVAS_ID" (plist-get entry :id))
      (org-canvas--save-buffer)))
  (org-canvas--log-info org-canvas--logger
    "[Diff] Stamped item id %s on '%s' in '%s'; nothing sent — the next sync recreates it there with a new id and deletes the copy in '%s'"
    (plist-get entry :id) (plist-get entry :partner-title)
    (plist-get entry :to) (plist-get entry :where)))

(defun org-canvas--diff-stamp-entry (entry)
  "Stamp MOVED ENTRY's item id on the heading it was paired with, and save.
Writes CANVAS_ID only and sends nothing.  Returns `stamped'; `claimed'
when a heading of the file already carries the id, so a second stamp
would make two headings claim one item; or `not-found' when the paired
heading cannot be found unstamped."
  (if (org-canvas--diff-find-heading-by-id
       (plist-get entry :file) "CANVAS_ID" (plist-get entry :id))
      'claimed
    (let ((where (org-canvas--diff-moved-position entry)))
      (if (null where)
          'not-found
        (org-canvas--diff-stamp-at where entry)
        'stamped))))

(defun org-canvas--diff-stamp-refusal (entry outcome)
  "Signal a `user-error' naming why MOVED ENTRY's OUTCOME is no stamp."
  (if (eq outcome 'claimed)
      (user-error "Item id %s is already on a heading; run the report again (g)"
                  (plist-get entry :id))
    (user-error "Cannot find the unstamped heading '%s' under '%s'; run the report again (g)"
                (or (plist-get entry :partner-title) (plist-get entry :title))
                (plist-get entry :to))))

(defun org-canvas-diff-stamp-move ()
  "Stamp the item id of the MOVED row at point on the heading it paired.
The heading is the one the row names, found by line and text, however
its title differs from the item's (issue #342).  Writes CANVAS_ID and
sends nothing; the next sync then recreates the item in the heading's
module with a new id and deletes the copy in the old one, which the
report shows as a RELOCATE row until it does (issue #343).  A row
paired by title only asks first, since the heading's link target has
no Canvas id to prove the pair.  `org-canvas-diff-stamp-moves' does
the same for every MOVED row."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry)))
    (unless (eq (plist-get entry :kind) 'moved)
      (user-error "Only a MOVED row names a heading to stamp"))
    (when (and (plist-get entry :by-title)
               (not (y-or-n-p
                     (format "'%s' was paired by title only; stamp item id %s on it anyway? "
                             (plist-get entry :title) (plist-get entry :id)))))
      (user-error "Aborted; nothing stamped"))
    (let ((outcome (org-canvas--diff-stamp-entry entry)))
      (unless (eq outcome 'stamped)
        (org-canvas--diff-stamp-refusal entry outcome))
      (org-canvas--diff-rewrite-row
       (format "  STAMPED   %s (CANVAS_ID %s on '%s' in '%s', nothing sent; the next sync recreates it there with a new id and deletes the copy in '%s')"
               (plist-get entry :title) (plist-get entry :id)
               (plist-get entry :partner-title) (plist-get entry :to)
               (plist-get entry :where)))
      (message "Stamped item id %s; the next sync recreates it in '%s'."
               (plist-get entry :id) (plist-get entry :to)))))

(defconst org-canvas--diff-stamp-buffer-name "*canvas-diff-stamp*"
  "Name of the buffer holding the report of stamped MOVED rows.")

(defun org-canvas--diff-stamp-candidates (results)
  "Sort the MOVED rows of RESULTS into the stampable and the held back.
Returns (STAMPABLE . HELD): STAMPABLE lists the entries paired by
content, HELD those paired by title only, which only `s' on the row
stamps, after asking."
  (let ((stampable nil) (held nil))
    (dolist (result results)
      (dolist (entry (plist-get result :extra))
        (when (eq (plist-get entry :kind) 'moved)
          (if (plist-get entry :by-title)
              (push entry held)
            (push entry stampable)))))
    (cons (nreverse stampable) (nreverse held))))

(defun org-canvas--diff-stamp-outcome-line (entry outcome)
  "Return the stamp report's line for ENTRY, stamped with OUTCOME."
  (pcase outcome
    ('stamped (format "  STAMPED   %s (CANVAS_ID %s on '%s' in '%s')\n"
                      (plist-get entry :title) (plist-get entry :id)
                      (plist-get entry :partner-title) (plist-get entry :to)))
    ('claimed (format "  CLAIMED   %s (item id %s is already on a heading)\n"
                      (plist-get entry :title) (plist-get entry :id)))
    (_ (format "  NOT FOUND %s (no unstamped heading '%s' under '%s')\n"
               (plist-get entry :title) (plist-get entry :partner-title)
               (plist-get entry :to)))))

(defun org-canvas--diff-stamp-render (outcomes held)
  "Insert the report of stamped MOVED rows at point.
OUTCOMES lists (ENTRY . OUTCOME) from `org-canvas--diff-stamp-entry';
HELD the rows left alone because only their title paired them."
  (insert "org-canvas Moved Item Stamps\n")
  (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
  (insert (make-string 60 ?=) "\n\n")
  (dolist (o outcomes)
    (insert (org-canvas--diff-stamp-outcome-line (car o) (cdr o))))
  (dolist (e held)
    (insert (format "  HELD      %s (item id %s; %s — stamp it with s on its row)\n"
                    (plist-get e :title) (plist-get e :id)
                    org-canvas--diff-by-title-note)))
  (when (or outcomes held) (insert "\n"))
  (insert (make-string 60 ?=) "\n")
  (let ((n (cl-count 'stamped outcomes :key #'cdr)))
    (insert (if (> n 0)
                (format "%d item id(s) stamped, nothing sent to Canvas; the next sync recreates each in its heading's module with a new id and deletes the old copy.\n" n)
              "No item id stamped.\n"))))

(defun org-canvas--diff-stamp-confirm (count)
  "Ask whether to stamp COUNT MOVED rows, or signal a `user-error'.
Under `noninteractive' there is nobody to ask, and running the command
was the answer."
  (unless (or noninteractive
              (let ((s (if (= count 1) "" "s")))
                (y-or-n-p
                 (format "Stamp the item id of %d MOVED row%s on the heading%s it was paired with? "
                         count s s))))
    (user-error "Aborted; nothing stamped")))

;;;###autoload
(defun org-canvas-diff-stamp-moves ()
  "Stamp the item id of every MOVED row on the heading it was paired with.
Runs the drift comparison (`org-canvas-diff' reads, one list request
per feature), then, after confirming, writes each MOVED item's id as
CANVAS_ID on the heading the report paired it with by content (issue
#342).  Nothing is sent: the next sync recreates each item in its
heading's module with a new id and deletes the old copy (issue #343).
A row paired by title only is held back and listed; `s' on its row
stamps it after asking.  Works from a batch Emacs, without asking.
Returns the number of ids stamped."
  (interactive)
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let* ((candidates (org-canvas--diff-stamp-candidates
                      (org-canvas--diff-collect-results)))
         (outcomes nil))
    (when (car candidates)
      (org-canvas--diff-stamp-confirm (length (car candidates)))
      (setq outcomes
            (mapcar (lambda (e) (cons e (org-canvas--diff-stamp-entry e)))
                    (car candidates))))
    (org-canvas--report-display
     org-canvas--diff-stamp-buffer-name
     (lambda () (org-canvas--diff-stamp-render outcomes (cdr candidates))))
    (let ((n (cl-count 'stamped outcomes :key #'cdr)))
      (message "Drift: %d item id(s) stamped, %d MOVED row(s) held back"
               n (length (cdr candidates)))
      n)))

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

(defun org-canvas--diff-collect-results ()
  "Compare every registered feature against Canvas and return the results.
One list request per feature (`org-canvas--diff-feature'), New Quizzes
included (`org-canvas--diff-features', issue #313); an excluded
feature contributes its visible line, a module's item check follows it
\(issue #177), and the referenced-media scan is applied at the end
\(issue #102), as is the move of the New Quizzes' assignments out of
the Assignments extras.  The read behind `org-canvas-diff',
`org-canvas-diff-adopt-stamps' and `org-canvas-diff-stamp-moves'."
  (let (results)
    (dolist (feature (org-canvas--diff-features))
      (let ((name (plist-get feature :name)))
        (if (org-canvas--diff-feature-excluded-p name)
            (push (org-canvas--diff-excluded-result feature) results)
          (message "Drift: checking %s..." name)
          (let ((result (org-canvas--diff-feature feature)))
            (push result results)
            ;; Module items report right after their modules (#177).
            (when-let* ((child (plist-get result :children)))
              (push child results))))))
    (org-canvas--diff-apply-new-quiz-assignments
     (org-canvas--diff-apply-extra-details
      (org-canvas--diff-apply-references (nreverse results))))))

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
Headings with no Canvas id are listed as pending creates, counted
apart (issue #294).  Returns the number of divergences found, pending
creates not included, so a batch caller can act on it; see
`org-canvas-diff-batch'."
  (interactive)
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let ((results (org-canvas--diff-collect-results)))
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
        (message "Drift: %d divergence(s), %d pending create(s)"
                 total (org-canvas--diff-pending-count results)))
      total)))

;;;###autoload
(defun org-canvas-diff-batch ()
  "Run `org-canvas-diff', then exit non-zero if there was any drift.
Intended for scheduled jobs, invoked from a batch-mode Emacs with
-f org-canvas-diff-batch.  Pending creates — headings the next sync
would create — are printed but are not drift, so they alone exit zero
\(issue #294)."
  (kill-emacs (if (> (org-canvas-diff) 0) 1 0)))

(provide 'org-canvas-diff)
;;; org-canvas-diff.el ends here
