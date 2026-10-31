;;; org-canvas-diff.el --- Read-only drift report against Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Answers the question `org-canvas-status' sounds like it answers but
;; does not: does Canvas currently match my Org files?
;;
;; `org-canvas-status' is entirely local -- it reports what the Org files
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
;;                      shares -- a create that lost its stamp (#85)
;;   Moved              a module item whose title an unstamped heading
;;                      under another module shares (#294)
;;
;; plus, for every property the module registers, the differing values
;; themselves -- published, points, due dates, group weights and so on --
;; and, for modules that declare a `:body-api-key', the body, compared
;; as text (#83).  A declared intent (`:intent-of', e.g.
;; WANT_DOCUMENT_PROCESSOR) is held against what Canvas holds, and the
;; reverse -- Canvas holds one, the file declares none -- is a NOTE row,
;; shown but not counted as drift (#293).
;;
;; And, counted apart because it is not drift, every heading with no
;; Canvas id that the next sync would create: a PENDING row (#294);
;; and every stamped module item heading whose id Canvas holds in
;; another module, which the next sync moves there keeping its id
;; (#352), recreating it only when Canvas refuses: a RELOCATE row (#343).
;; New Quizzes, which list from the quiz service and so stay out of the
;; feature registry, take part through their pull-only entry (#313),
;; and their items in a section of their own (#322).
;; An EXTRA assignment or group says what it is, from the list replies
;; already held, and a WEIGHTS line under the groups sets Org's weight
;; total against Canvas's when they differ, uncounted (#296).
;;
;; WHERE THE PARTS LIVE
;; ====================
;; This file is the comparison engine: the registry lookup, the
;; per-feature pass, and the entry points `org-canvas-diff',
;; `org-canvas-diff-batch' and `org-canvas-diff-json-data'.  Three
;; siblings hold the rest, and none of them is required from here --
;; each requires this file, so the command files above load all four:
;;
;;   org-canvas-diff-fields.el  what value Canvas holds for a property,
;;                              the body comparison, referenced media,
;;                              the EXTRA details pass and the New Quiz
;;                              assignments pass
;;   org-canvas-diff-items.el   the module items, New Quiz items and
;;                              assignment overrides child passes
;;   org-canvas-diff-report.el  the rows, their rendering, the report
;;                              mode and its row-navigation commands
;;   org-canvas-diff-verbs.el   the verbs that act on a row: delete,
;;                              pull, adopt a stamp, stamp a moved item
;;
;; SCOPE OF THE FIELD COMPARISON
;; =============================
;; Only properties actually written in the Org file are compared.  An
;; absent property means the file expresses no opinion, and each module
;; applies its own default at parse time, so treating absence as a value
;; would invent differences that no sync would act on.  A spec may also
;; name a `:compare-p' predicate of (POM ITEM) saying whether the
;; property is a comparable opinion at all: Canvas drops ALL_DAY on a
;; calendar event spanning days -- the times are kept, the flag is not --
;; so comparing it would flag the entry on every run, forever (#93).
;; The exception is a boolean that declares a `:default': the parse
;; applies it, so the push acts on silence, and a pull deletes
;; PUBLISHED: true as the default.  Such a property is compared against
;; its default when the heading is silent (issue #498).
;;
;; ACKNOWLEDGED AND EXCLUDED (#98)
;; ===============================
;; A synced course can still hold remote objects that are unclaimed by
;; design -- Canvas scaffolding, features authored in the web UI on
;; purpose, deliberate one-offs -- and a report that flags them forever
;; buries the one extra that matters and pins `org-canvas-diff-batch'
;; at a non-zero exit.  Three valves, from broad to narrow: a feature's
;; `:skip-fn' suppresses scaffolding structurally (counted in the
;; footer, the #81 way); `org-canvas-diff-excluded-features' skips a
;; feature wholesale, printing one line so the exclusion stays visible
;; (its bodies are still read for referenced media, since excluding a
;; feature from the rows should not blind that scan -- #111);
;; `org-canvas-diff-known-extras' acknowledges individual ids, counted
;; once in the footer -- and flagged loudly, and counted as drift, when
;; an acknowledged id stops existing, so the list cannot rot.  A fourth
;; valve is automatic (#102): a file that course content embeds is
;; referenced media, counted in the footer, so an attachment uploaded
;; through the rich-content editor does not need acknowledging by hand.
;;
;; COST AND SAFETY
;; ===============
;; One list request per feature.  Nothing is written, locally or
;; remotely, so this is safe to run at any time -- from a hook, or from a
;; scheduled job via `org-canvas-diff-batch', which exits non-zero when
;; it finds drift (pending creates are not drift).  The two local writes
;; in the report, stamp adoption (#257) and stamping a MOVED row's item
;; id on its heading (#342), are separate keys and commands the report
;; never runs itself, and neither sends anything to Canvas.
;; So is deleting a row's Canvas object: `k' one row at a time, and
;; `org-canvas-diff-delete-rows' for a batch (#345), both after safety
;; checks and a JSON snapshot of the object.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;; The engine calls into its siblings, which each require this file
;; back; requiring them here would make a loop of it.  The command
;; files above load all four.
(declare-function org-canvas--diff-compare-fields "org-canvas-diff-fields"
                  (specs pom item))
(declare-function org-canvas--diff-compare-body "org-canvas-diff-fields"
                  (props pom item))
(declare-function org-canvas--diff-normalize-remote "org-canvas-diff-fields"
                  (value))
(declare-function org-canvas--diff-entry-notes "org-canvas-diff-fields"
                  (entry index specs))
(declare-function org-canvas--diff-items-references "org-canvas-diff-fields"
                  (items api-key))
(declare-function org-canvas--diff-excluded-result "org-canvas-diff-fields"
                  (feature))
(declare-function org-canvas--diff-apply-references "org-canvas-diff-fields"
                  (results))
(declare-function org-canvas--diff-apply-extra-details "org-canvas-diff-fields"
                  (results))
(declare-function org-canvas--diff-apply-new-quiz-assignments
                  "org-canvas-diff-fields" (results))
(declare-function org-canvas--diff-children "org-canvas-diff-items"
                  (name items local file))
(defvar org-canvas--diff-children-fns)
(declare-function org-canvas--diff-render "org-canvas-diff-report"
                  (results))
(declare-function org-canvas--diff-insert-entry "org-canvas-diff-report"
                  (entry))
(declare-function org-canvas--diff-count "org-canvas-diff-report" (results))
(declare-function org-canvas--diff-pending-count "org-canvas-diff-report"
                  (results))
(declare-function org-canvas-diff-mode "org-canvas-diff-report" ())
(defvar org-canvas--diff-key-legend)
(declare-function org-canvas-diff-adopt-stamps "org-canvas-diff-verbs" ())
(declare-function org-canvas-diff-stamp-moves "org-canvas-diff-verbs" ())

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
  "Return NAME in the spelling the drift report compares names by.
`org-canvas--feature-id-string' folds spaces, underscores and dashes to
one lower-case spelling, the registry's id; the report drops the dashes
too, the way it matched names before the id existed, so
\"assignmentgroups\" still means \"Assignment Groups\" (issue #572)."
  (replace-regexp-in-string "-" "" (org-canvas--feature-id-string name)))

(defun org-canvas--diff-find-properties (feature-name)
  "Return FEATURE-NAME's registry entry when property specs are registered.
The drift report compares a heading's properties, so an entry with no
`:properties' — a feature whose file the report reads another way — is
not one.  The lookup is on the id, so the feature registry's
\"Assignment Groups\" finds the specs its property registration added to
the same entry (issue #572)."
  (let ((entry (org-canvas--registry-entry feature-name)))
    (and (plist-member entry :properties) entry)))

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
  (append (org-canvas--feature-entries)
          (cl-remove-if-not (lambda (f) (plist-get f :drift-report))
                            org-canvas--pull-feature-registry)))

(defun org-canvas--diff-find-feature (name)
  "Return the entry of `org-canvas--diff-features' named NAME, or nil.
A registered feature answers on its id, one hash read; a pull-only
entry has no id in the registry, and any spelling of a registered
name that the id fold does not cover is matched by name as before
\(issue #572)."
  (or (org-canvas--registry-find-feature name)
      (let ((norm (org-canvas--diff-normalize-name name)))
        (cl-find-if (lambda (f)
                      (string= norm (org-canvas--diff-normalize-name
                                     (plist-get f :name))))
                    (org-canvas--diff-features)))))


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
          :deleted (org-entry-get (point) org-canvas--prop-canvas-deleted)
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

(defconst org-canvas--diff-title-label "TITLE"
  "The field name a CHANGED row gives a title Canvas holds otherwise.")

(defun org-canvas--diff-squeeze (text)
  "Return TEXT trimmed, with each stretch of whitespace one space, or nil."
  (and (stringp text)
       (string-trim (replace-regexp-in-string "[ \t\n\r ]+" " " text))))

(defun org-canvas--diff-compare-title (props entry item title-field)
  "Return (TITLE ORG CANVAS) when ITEM's title is not ENTRY's heading, or nil.
PROPS is the feature's registry entry, and only one that
declares `:compare-title' takes part; TITLE-FIELD names the Canvas
field.  The push sends the heading as the title, so a rename in the
web UI is undone by the next push, and the report used to say only
that nothing compared differed (issue #498).  A heading that is a link
is left out: its title is the target's, read by its own module."
  (let ((heading (plist-get entry :heading)))
    (when (and (plist-get props :compare-title) title-field heading
               (not (string-match-p "\\[\\[" heading)))
      (let ((local (org-canvas--diff-squeeze (plist-get entry :match-title)))
            (remote (org-canvas--diff-squeeze
                     (org-canvas--diff-normalize-remote
                      (alist-get title-field item)))))
        (when (and local remote (not (string= local remote)))
          (list org-canvas--diff-title-label local remote))))))

(defun org-canvas--diff-entry (entry index specs &optional props modified-field
                                     title-field)
  "Compare local ENTRY against the remote INDEX using property SPECS.
PROPS, the feature's registry entry, enables the body
comparison when it names a `:body-api-key' (issue #83), and the title
comparison when it declares `:compare-title' with TITLE-FIELD the
Canvas field (issue #498).
MODIFIED-FIELD names the remote timestamp \"modified remotely\" is
decided from, default `updated_at'; files pass `modified_at' (issue
#94).  Returns a plist describing the divergence, or nil when they
agree.  A CHANGED row keeps the Canvas item as :item, which the
row's verbs read rather than asking Canvas again (issues #498, #500)."
  (let* ((id (plist-get entry :id))
         (pom (plist-get entry :pom))
         (item (and id (gethash id index))))
    (cond
     ((null id) nil)
     ((null item)
      (list :kind 'missing :title (plist-get entry :title) :id id
            :deleted (plist-get entry :deleted)))
     (t
      (let* ((body (org-canvas--diff-compare-body props pom item))
             (title (org-canvas--diff-compare-title props entry item title-field))
             (fields (append (and title (list title))
                             (org-canvas--diff-compare-fields specs pom item)
                             (and (cdr body) (list (cdr body)))))
             (drifted (org-canvas--diff-modified-p pom item modified-field)))
        (when (or fields drifted)
          (list :kind 'modified :title (plist-get entry :title) :id id
                :updated (alist-get (or modified-field 'updated_at) item)
                :remote-newer drifted :fields fields
                :body-compared (and body t)
                :item item
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


;;;; Feature Check

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
                                             modified-field title-field)))
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

(defun org-canvas--diff-read ()
  "Check the course is ready, then return the drift results, writing nothing.
The preflight check and the operation-start hook, then
`org-canvas--diff-collect-results'.  The read behind `org-canvas-diff'
and `org-canvas-diff-json-data'."
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (org-canvas--diff-collect-results))

;;;;; Rows as Data (issue #531)

(defconst org-canvas--diff-json-statuses
  '((modified . "CHANGED") (missing . "MISSING") (extra . "EXTRA")
    (stale-ack . "STALE-ACK") (unclaimed . "UNCLAIMED") (moved . "MOVED")
    (relocate . "RELOCATE") (pending . "PENDING") (note . "NOTE"))
  "The status of each row kind, as the report labels it.
An override row takes its status from its :state instead.")

(defun org-canvas--diff-json-status (entry)
  "Return the status of ENTRY, the label its report row opens with."
  (if (eq (plist-get entry :kind) 'override)
      (pcase (plist-get entry :state)
        ('extra "EXTRA")
        ('missing "MISSING")
        (_ "CHANGED"))
    (alist-get (plist-get entry :kind) org-canvas--diff-json-statuses)))

(defun org-canvas--diff-json-count-key (status)
  "Return the counts key of STATUS: \"STALE-ACK\" becomes `stale_ack'."
  (intern (replace-regexp-in-string "-" "_" (downcase status))))

(defun org-canvas--diff-json-text (value)
  "Return VALUE as a string, or nil for nil."
  (and value (format "%s" value)))

(defun org-canvas--diff-json-message (entry)
  "Return the first line of ENTRY's report row, trimmed and redacted."
  (let ((text (with-temp-buffer
                (org-canvas--diff-insert-entry entry)
                (buffer-string))))
    (org-canvas--log-redact
     (string-trim (car (split-string text "\n"))))))

(defun org-canvas--diff-json-row (feature entry drift)
  "Return ENTRY of FEATURE's section as a JSON object alist.
DRIFT is non-nil when the row counts as drift."
  (list (cons 'status (org-canvas--diff-json-status entry))
        (cons 'kind (symbol-name (plist-get entry :kind)))
        (cons 'drift (if drift t :json-false))
        (cons 'feature feature)
        (cons 'title (org-canvas--diff-json-text (plist-get entry :title)))
        (cons 'canvas_id (org-canvas--diff-json-text (plist-get entry :id)))
        (cons 'container (plist-get entry :where))
        (cons 'container_kind (and (plist-get entry :where)
                                   (or (plist-get entry :container) "module")))
        (cons 'file (plist-get entry :file))
        (cons 'line (plist-get entry :line))
        (cons 'remote_newer (if (plist-get entry :remote-newer) t :json-false))
        (cons 'updated_at (org-canvas--diff-json-text (plist-get entry :updated)))
        (cons 'fields
              (vconcat (mapcar (lambda (f)
                                 (list (cons 'name (org-canvas--diff-json-text (nth 0 f)))
                                       (cons 'org (org-canvas--diff-json-text (nth 1 f)))
                                       (cons 'canvas (org-canvas--diff-json-text (nth 2 f)))))
                               (plist-get entry :fields))))
        (cons 'message (org-canvas--diff-json-message entry))))

(defun org-canvas--diff-json-rows (result)
  "Return the rows of one feature RESULT as JSON object alists.
In the report's order: divergences and extras, which are drift, then
notes, pending creates and relocations, which are not."
  (let ((name (plist-get result :name)))
    (append
     (mapcar (lambda (e) (org-canvas--diff-json-row name e t))
             (append (plist-get result :divergences) (plist-get result :extra)))
     (mapcar (lambda (e) (org-canvas--diff-json-row name e nil))
             (append (plist-get result :notes) (plist-get result :pending)
                     (plist-get result :relocate))))))

(defun org-canvas--diff-json-feature (result)
  "Return how one feature RESULT was checked, as a JSON object alist."
  (let ((err (plist-get result :error)))
    (list (cons 'name (plist-get result :name))
          (cons 'state (cond ((plist-get result :excluded) "excluded")
                             (err "error")
                             (t "checked")))
          (cons 'error (and err (org-canvas--log-redact (format "%s" err)))))))

(defun org-canvas--diff-json-counts (rows)
  "Return the count of ROWS of each status, every status named."
  (mapcar (lambda (pair)
            (let ((status (cdr pair)))
              (cons (org-canvas--diff-json-count-key status)
                    (cl-count status rows
                              :key (lambda (r) (alist-get 'status r))
                              :test #'equal))))
          org-canvas--diff-json-statuses))

(defun org-canvas--diff-json-total (results key)
  "Return the sum of KEY, a count, over RESULTS."
  (apply #'+ (mapcar (lambda (r) (or (plist-get r key) 0)) results)))

(defun org-canvas-diff-json-data ()
  "Return the drift report as an alist for `json-encode', writing nothing.
The rows `org-canvas-diff' would print, each with its status and
whether it counts as drift, the per-status counts and the drift total
`org-canvas-diff' returns.  Nothing is displayed or written (hard
rule 19).  The schema is the manual's \"Machine-Readable Output\"
\(issue #531)."
  (let* ((results (org-canvas--diff-read))
         (rows (mapcan #'org-canvas--diff-json-rows results)))
    (list (cons 'command "diff")
          (cons 'course org-canvas-directory)
          (cons 'course_id (org-canvas--diff-json-text org-canvas-course-id))
          (cons 'drift (org-canvas--diff-count results))
          (cons 'counts (org-canvas--diff-json-counts rows))
          (cons 'acknowledged (org-canvas--diff-json-total results :acknowledged))
          (cons 'referenced (org-canvas--diff-json-total results :referenced))
          (cons 'features (vconcat (mapcar #'org-canvas--diff-json-feature results)))
          (cons 'rows (vconcat rows)))))

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
  (let ((results (org-canvas--diff-read)))
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
