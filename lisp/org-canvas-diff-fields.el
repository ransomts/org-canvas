;;; org-canvas-diff-fields.el --- Drift report's field and body comparison  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The half of the drift report that answers "what value does Canvas
;; hold, and does it differ from the value the Org heading declares?":
;; the property comparison each module's registry specs drive, the body
;; comparison for features with a `:body-api-key' (issue #83), and the
;; three passes that read Canvas's own answers back -- referenced media
;; (#102), what an EXTRA assignment or group is (#296), and the
;; assignments Canvas attaches to a New Quiz (#313).
;;
;; Nothing here reads a file or sends a request beyond the syllabus,
;; whose body the reference scan asks for; every other pass works from
;; the list replies `org-canvas--diff-feature' already holds.
;;
;; The report's rows and verbs live in `org-canvas-diff-report.el' and
;; `org-canvas-diff-verbs.el'; the engine that calls these passes is
;; `org-canvas-diff.el'.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-diff)
(require 'cl-lib)
(require 'org-entities)

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

(defun org-canvas--diff-declared-default (spec)
  "Return the Org spelling of the default SPEC declares, or nil.
Only a boolean with an explicit `:default' has one: the registry's
default must match the parse side's fallback (pull-system.org), so a
heading silent on PUBLISHED is pushed as published, and a pull deletes
PUBLISHED: true as the default it is.  An `:absent-inherits' spec has
no default; its silence means \"inherit\" (issue #323)."
  (and (eq (plist-get spec :type) 'boolean)
       (plist-member spec :default)
       (not (plist-get spec :absent-inherits))
       (if (plist-get spec :default) "true" "false")))

(defun org-canvas--diff-compare-default (spec item)
  "Compare the default SPEC declares with Canvas ITEM, for a silent heading.
Returns (ORG-PROP LOCAL REMOTE) when Canvas holds the other value, else
nil.  A heading the pull left without PUBLISHED, because Canvas said
true, is pushed as published; when Canvas unpublishes it later, the
next push publishes it again, and a report blind to silence called
the row one with nothing compared, which `a' would have adopted
\(issue #498).  A field Canvas did not send is not compared."
  (let ((default (org-canvas--diff-declared-default spec)))
    (when (and default (org-canvas--registry-remote-present-p spec item))
      (let ((remote (org-canvas--diff-remote-field spec item)))
        (unless (org-canvas--diff-values-equal-p 'boolean default remote)
          (list (plist-get spec :org-prop)
                (format "(unset: %s)" default)
                (org-canvas--diff-format-remote 'boolean remote)))))))

(defun org-canvas--diff-compare-written (spec local item)
  "Compare LOCAL, SPEC's written value or nil, with Canvas ITEM.
Returns (ORG-PROP LOCAL REMOTE) when they differ, else nil.  A nil
LOCAL is compared only for a `:canvas-owned' spec."
  (when (or local (plist-get spec :canvas-owned))
    (let ((type (plist-get spec :type))
          (remote (org-canvas--diff-remote-field spec item)))
      (unless (if local
                  (org-canvas--diff-values-equal-p type local remote)
                (null (org-canvas--diff-normalize-remote remote)))
        (list (plist-get spec :org-prop) (or local "(unset)")
              (org-canvas--diff-format-remote type remote))))))

(defun org-canvas--diff-compare-field (spec pom item)
  "Compare one ordinary property SPEC at POM against Canvas ITEM.
Returns (ORG-PROP LOCAL REMOTE) when they differ, else nil.  See
`org-canvas--diff-compare-fields' for which specs take part."
  (let ((compare-p (plist-get spec :compare-p)))
    (when (and (memq (plist-get spec :type) org-canvas--diff-comparable-types)
               (not (plist-get spec :local-only))
               (or (null compare-p) (funcall compare-p pom item)))
      (let* ((written (org-entry-get pom (plist-get spec :org-prop)))
             (local (and written (not (string-empty-p written)) written)))
        (if (or local (plist-get spec :canvas-owned))
            (org-canvas--diff-compare-written spec local item)
          (org-canvas--diff-compare-default spec item))))))

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
local side.  A boolean declaring a `:default' is compared too, its
default standing in for the silent heading, since the push sends it
\(`org-canvas--diff-compare-default', issue #498).

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
  (append
   (delq nil
         (mapcar (lambda (entry)
                   ;; org-entities holds section-header strings too.
                   (when (listp entry)
                     (let ((name (nth 0 entry))
                           (html (nth 3 entry))
                           (utf8 (nth 6 entry)))
                       (and (stringp name) (stringp utf8)
                            (string-match-p "\\`&[[:alnum:]]+;\\'" html)
                            (cons name utf8)))))
                 org-entities))
   ;; The XML predefine org-entities does not carry.
   '(("apos" . "'")))
  "Named HTML entities decoded when comparing bodies as text.
Built from `org-entities', whose HTML spellings are exactly what
ox-html emits.  The six-entry hand list this replaces covered only
what Canvas was known to escape, so an `&hellip;' ox-html emits for
`...' stayed encoded on the local side while Canvas held the
character — a permanent CHANGED row over text nobody changed (issue
#510).  An entity neither this table nor the numeric forms know is
kept as written, so both sides still agree when they spell it the
same way.")

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


(provide 'org-canvas-diff-fields)
;;; org-canvas-diff-fields.el ends here
