;;; org-canvas-validate-issue.el --- Validation vocabulary for org-canvas  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The words the course validator's checks are written in: the issue
;; plist and its formatting, the registry of check ids, accepts (issue
;; #538), the type-specific checks, the date-ordering check, the spec
;; table, and the roster lookups every other family of checks shares.
;;
;; The checking families require this file; it requires none of them.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; 1. Issue Structure
;;
;; Each issue is a plist:
;; (:severity error|warning|note :file PATH :line NUM :heading TITLE
;;  :property PROP :message MSG)
;;
;; The severity says what the finding is about (issue #413):
;;   error   - the push fails or sends something wrong
;;   warning - the file or the course is out of step in a way that
;;             breaks something (a past date, a stale id, a dead link)
;;   note    - nothing breaks and no value is wrong, but the course may
;;             not be laid out as a student needs (an item in no module)
;; Only errors fail `org-canvas-validate-batch'.
;;
;; Optional keys: :pending-sync, :push-only, :check (the id of the
;; check that made it, issue #538) and :accepted (the reason an accept
;; gave for keeping it).

(defun org-canvas--validate-format-issue (issue)
  "Format ISSUE plist as a `compilation-mode' compatible string."
  (format "%s:%d: %s: %s"
          (plist-get issue :file)
          (plist-get issue :line)
          (symbol-name (plist-get issue :severity))
          (plist-get issue :message)))

(defun org-canvas--validate-severity (issue)
  "Return the severity of ISSUE: `error', `warning' or `note'."
  (plist-get issue :severity))

(defun org-canvas--validate-make-issue (severity loc property message
                                                 &optional pending-sync)
  "Create a validation issue plist.
SEVERITY is `error', `warning' or `note'.  LOC is a plist
\(:file F :line N :heading H).
PROPERTY and MESSAGE describe the problem.  PENDING-SYNC marks the
issue as expected pre-first-sync state (a link target that simply has
no CANVAS_ID yet); the report collapses these into a summary line."
  (append (list :severity severity
                :file (plist-get loc :file)
                :line (plist-get loc :line)
                :heading (plist-get loc :heading)
                :property property :message message)
          (when pending-sync '(:pending-sync t))))

(defun org-canvas--validate-push-only (issue)
  "Mark ISSUE as advice that exists only to protect a push, and return it.
A course marked `org-canvas-read-only' will never make that push, and
such findings were half of what validation had to say about a mirror
of somebody else's course — every historical due date reported as
being in the past, faithfully and uselessly (issue #168).  A finding
that describes the course itself is never marked: a title collision, a
broken link or a malformed drawer is true whatever you intend to do
next.  Nil ISSUE passes through, so a check can wrap its result."
  (and issue (append issue (list :push-only t))))

;;;; 2. Check Ids and Accepts (issue #538)
;;
;; A finding a course may decide to keep carries the id of the check
;; that made it (`:check').  A heading names one in VALIDATE_ACCEPT,
;; an override row in its Accept cell, each with the reason it is
;; kept, and the finding leaves the report for a count and, with
;; --all, a list of what was accepted and why.  A check opts in by
;; joining `org-canvas--validate-checks' and marking its findings with
;; `org-canvas--validate-checked'; a finding without an id cannot be
;; accepted.

(defconst org-canvas--validate-checks
  '((date-order . "dates out of order (unlock, due, lock)")
    (lock-window . "a lock date missing or off the course's late window")
    (require-unlock . "an unlock date missing where the course requires one"))
  "Checks a course may accept the findings of, by id, each with its subject.")

(defconst org-canvas--validate-accept-property "VALIDATE_ACCEPT"
  "Heading property naming a check to accept there, and why.")

(defun org-canvas--validate-checked (check issue)
  "Return ISSUE marked as found by CHECK, an `org-canvas--validate-checks' id.
Nil ISSUE passes through, so a check can wrap its result."
  (and issue (append issue (list :check check))))

(defun org-canvas--validate-parse-accept (text)
  "Split accept TEXT into (NAME . REASON), REASON nil when absent.
NAME is the check id as written, the first word; REASON the rest."
  (let ((text (string-trim text)))
    (string-match "\\`\\(\\S-+\\)\\(?:[ \t]+\\(.+\\)\\)?\\'" text)
    (cons (match-string 1 text) (match-string 2 text))))

(defun org-canvas--validate-accept-issue (severity loc where format-string
                                                   &rest args)
  "Return a finding about an accept, of SEVERITY at LOC.
WHERE is (PROPERTY . PREFIX): the property the accept is written in,
nil on an override row, and the text starting the message.  The
message is FORMAT-STRING applied to ARGS."
  (org-canvas--validate-make-issue
   severity loc (car where)
   (concat (cdr where) (apply #'format format-string args))))

(defun org-canvas--validate-accept-matches (issues check line)
  "Return the findings of ISSUES that CHECK made at LINE."
  (cl-remove-if-not (lambda (issue)
                      (and (eq (plist-get issue :check) check)
                           (eql (plist-get issue :line) line)))
                    issues))

(defun org-canvas--validate-accept-refusal (parsed matched loc where)
  "Return why accept PARSED may take none of MATCHED, as a finding, or nil.
PARSED is (NAME . REASON).  LOC and WHERE are as for
`org-canvas--validate-accept-issue'."
  (let ((label (if (car where) (car where) "Accept")))
    (cond
     ((not (assq (intern (car parsed)) org-canvas--validate-checks))
      (org-canvas--validate-accept-issue
       'warning loc where "%s names '%s', which is not a check validate knows (%s)"
       label (car parsed)
       (mapconcat (lambda (c) (symbol-name (car c))) org-canvas--validate-checks ", ")))
     ((null (cdr parsed))
      (org-canvas--validate-accept-issue
       'warning loc where "%s %s gives no reason, so it accepts nothing: write why after the check id"
       label (car parsed)))
     ((cl-find 'error matched :key #'org-canvas--validate-severity)
      (org-canvas--validate-accept-issue
       'warning loc where "%s %s accepts nothing: the finding is an error, which has to be fixed"
       label (car parsed)))
     ((null matched)
      (org-canvas--validate-accept-issue
       'note loc where "Stale accept: %s %s matched no finding here; remove it (%s)"
       label (car parsed) (cdr parsed))))))

(defun org-canvas--validate-apply-accepts (issues accepts loc where)
  "Mark the findings of ISSUES that ACCEPTS take, and return the list.
ACCEPTS are texts, each a check id and the reason it is kept.  An
accept covers the findings its check made at LOC's line, never one
on another line: a heading's accept leaves its override rows alone.
It marks each with `:accepted' and the reason.  An accept that takes
nothing (an unknown id, no reason, an error, nothing found) adds a
finding saying so, after ISSUES.  WHERE is as for
`org-canvas--validate-accept-issue'."
  (let ((own nil))
    (dolist (text accepts)
      (let* ((parsed (org-canvas--validate-parse-accept text))
             (matched (org-canvas--validate-accept-matches
                       issues (intern (car parsed)) (plist-get loc :line)))
             (refusal (org-canvas--validate-accept-refusal
                       parsed matched loc where)))
        (if refusal
            (push refusal own)
          (setq issues (mapcar (lambda (issue)
                                 (if (and (memq issue matched)
                                          (not (plist-get issue :accepted)))
                                     (append issue (list :accepted (cdr parsed)))
                                   issue))
                               issues)))))
    (nconc issues (nreverse own))))

(defun org-canvas--validate-heading-accepts ()
  "Return the VALIDATE_ACCEPT values of the heading at point, each whole.
Org joins a property's `+' lines with spaces and splits them at every
space (`org-entry-get', `org-entry-get-multivalued-property'), where
a reason has spaces in it; so each drawer line is read as one value."
  (when-let* ((block (org-get-property-block)))
    (save-excursion
      (goto-char (car block))
      (let ((case-fold-search t)
            (re (format "^[ \t]*:%s\\+?:[ \t]*\\(.*?\\)[ \t]*$"
                        org-canvas--validate-accept-property))
            (values nil))
        (while (re-search-forward re (cdr block) t)
          (unless (string-empty-p (match-string-no-properties 1))
            (push (match-string-no-properties 1) values)))
        (nreverse values)))))

;;;; 3. Type-Specific Validators
;;
;; Each returns nil (valid) or an issue plist.

(defun org-canvas--validate-check-boolean (value property loc)
  "Check that VALUE is \"true\", \"false\", or absent.
PROPERTY names the property.  LOC is a (:file :line :heading) plist."
  (when (and value (not (member (downcase value) '("true" "false"))))
    (org-canvas--validate-make-issue
     'error loc property
     (format "%s: '%s' is not a valid boolean (expected true/false)" property value))))

(defun org-canvas--validate-check-number (value property loc)
  "Check that VALUE is a valid number.
PROPERTY names the property.  LOC is a (:file :line :heading) plist."
  (when (and value (not (string-empty-p value))
            (not (string-match-p "\\`-?[0-9]*\\.?[0-9]+\\'" value)))
    (org-canvas--validate-make-issue
     'error loc property
     (format "%s: '%s' is not a valid number" property value))))

(defun org-canvas--validate-check-enum (value property valid-values loc)
  "Check that VALUE is in VALID-VALUES.
PROPERTY names the property.  LOC is a (:file :line :heading) plist.
VALID-VALUES already carries the property's `:read-only-values', so a
value only Canvas may set passes here and is refused at the push."
  (when (and value (not (member value valid-values)))
    (org-canvas--validate-make-issue
     'error loc property
     (format "%s: '%s' is not valid (expected: %s)"
             property value (string-join valid-values ", ")))))

(defun org-canvas--validate-check-csv-enum (value property valid-values loc)
  "Check that each comma-separated part of VALUE is in VALID-VALUES.
PROPERTY names the property.  LOC is a (:file :line :heading) plist.
A `csv-enum' spec without `:values' is a free-form list (file
extensions, Canvas ids) and has nothing to check against.
VALID-VALUES already carries the property's `:read-only-values': a
pulled quiz-backed assignment says SUBMISSION: online_quiz, and every
such heading was an error until the value was let through (issue
#167)."
  (when (and value valid-values)
    (let ((parts (split-string value "," t "[ \t]+")))
      (let ((bad (cl-remove-if (lambda (p) (member p valid-values)) parts)))
        (when bad
          (org-canvas--validate-make-issue
           'error loc property
           (format "%s: invalid value(s) '%s' (expected: %s)"
                   property (string-join bad ", ")
                   (string-join valid-values ", "))))))))

(defcustom org-canvas-validate-past-timestamps 'unsynced-only
  "When `org-canvas-validate' warns that a timestamp is in the past.
`unsynced-only' warns on a heading no push has created yet, where a
past date is most likely a typo about to be sent; on a heading that
is on Canvas a past date is the record of something that happened,
and four weeks into a term every assignment that has run would warn
\(issue #208).  `always' warns on every heading, as it did before;
`never' drops the warning.  The course itself, in settings.org, is
always on Canvas: its start date is past for the whole term."
  :type '(choice (const :tag "Headings not yet on Canvas" unsynced-only)
                 (const :tag "Every heading" always)
                 (const :tag "Never" never))
  :group 'org-canvas)

(defvar org-canvas--validate-always-on-canvas nil
  "Non-nil while validating a spec whose headings always exist on Canvas.
Bound by `org-canvas--validate-spec' from the registration's
`:always-on-canvas' (settings.org: the course is never created by a
push), so the past-timestamp rule treats every heading as synced.")

(defun org-canvas--validate-entry-on-canvas-p ()
  "Return non-nil when the entry at point already exists on Canvas.
True for a heading stamped with CANVAS_ID or CANVAS_URL, and for every
heading of a spec registered `:always-on-canvas'.  Nil outside an Org
entry, so a check run on a bare value behaves as it would for a
heading no push has created."
  (or org-canvas--validate-always-on-canvas
      (and (derived-mode-p 'org-mode)
           (not (org-before-first-heading-p))
           (or (org-entry-get (point) "CANVAS_ID")
               (org-entry-get (point) "CANVAS_URL"))
           t)))

(defun org-canvas--validate-past-timestamp-matters-p (pull-only)
  "Return non-nil when a past timestamp on the entry at point deserves a warning.
Never for a PULL-ONLY property, which is in the past by nature; then
by `org-canvas-validate-past-timestamps', which under `unsynced-only'
asks `org-canvas--validate-entry-on-canvas-p'."
  (and (not pull-only)
       (pcase org-canvas-validate-past-timestamps
         ('never nil)
         ('always t)
         (_ (not (org-canvas--validate-entry-on-canvas-p))))))

(defun org-canvas--validate-check-timestamp (value property loc &optional pull-only)
  "Check that VALUE is parseable as an Org timestamp.
Warns if the timestamp is in the past and that matters here (see
`org-canvas--validate-past-timestamp-matters-p'): never for a
PULL-ONLY property, which a pull wrote and is in the past by nature,
and by default only on a heading no push has created (issue #208).
PROPERTY names the property.  LOC is a (:file :line :heading) plist."
  (when value
    (condition-case nil
        (let* ((parsed (org-parse-time-string value))
               (encoded (encode-time parsed))
               (iso (format-time-string "%Y-%m-%dT%H:%M:%SZ" encoded t))
               (now (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
          (when (and (string< iso now)
                     (org-canvas--validate-past-timestamp-matters-p pull-only))
            (org-canvas--validate-push-only
             (org-canvas--validate-make-issue
              'warning loc property
              (format "%s: timestamp %s is in the past" property value)))))
      (error
       (org-canvas--validate-make-issue
        'error loc property
        (format "%s: '%s' is not a valid Org timestamp" property value))))))

(defun org-canvas--validate-resolve-file-link (value id-property source-file loc property
                                                     not-link-msg unresolved-msg
                                                     &optional not-link-severity)
  "Check VALUE as a file link, verifying ID-PROPERTY resolves.
SOURCE-FILE is the file containing the link (for relative path resolution).
LOC is a (:file :line :heading) plist.  PROPERTY is the property name (or nil).
NOT-LINK-MSG is the message when VALUE is not a file link.
UNRESOLVED-MSG is the warning message when the link target has no ID-PROPERTY.
NOT-LINK-SEVERITY is the severity for non-link values (default: \\='error).
Returns nil (valid) or an issue plist."
  (cond
   ((not (string-match "\\[\\[file:" value))
    (org-canvas--validate-make-issue (or not-link-severity 'error)
                                     loc property not-link-msg))
   ((not (org-canvas--resolve-link-property value id-property source-file))
    (org-canvas--validate-push-only
     (org-canvas--validate-make-issue 'warning loc property unresolved-msg t)))))

(defun org-canvas--validate-check-link (value property _target-file-var id-property loc)
  "Check that VALUE is a valid Org file link and resolves.
PROPERTY is the property name for error messages.
TARGET-FILE-VAR is the symbol of the target file variable.
ID-PROPERTY is the Canvas ID property expected on the target heading.
LOC is a (:file :line :heading) plist."
  (when value
    (org-canvas--validate-resolve-file-link
     value id-property (plist-get loc :file) loc property
     (format "%s: '%s' is not a file link (expected [[file:...::*heading][...]])"
             property value)
     (format "%s: link target has no %s (sync target first)"
             property id-property))))

;;;; 4. Date Ordering Check

(defun org-canvas--validate-safe-parse-timestamp (value)
  "Parse VALUE as an Org timestamp, returning ISO string or nil on error."
  (and value (condition-case nil
                 (org-canvas-org-parse-timestamp value)
               (error nil))))

(defun org-canvas--validate-date-sequence (dates loc &optional prefix)
  "Warn where DATES run out of order; return the list of issues.
DATES is a list of (NAME VALUE ISO INHERITED) in the order they must
run, ISO nil for a date that is absent or does not parse.  Those are
passed over, so each date that is there is compared with the next one
that is: an unlock after the lock warns when there is no due date
between them.  A pair of two INHERITED dates is not reported: an
override row inherits the assignment's dates, and their order is the
heading's to report (issue #538).  LOC is a (:file :line :heading)
plist and PREFIX, when given, starts each message.  Shared by headings
and override rows (issue #534), which is what keeps the two in
agreement.  The findings are check `date-order'."
  (let ((present (cl-remove-if-not (lambda (date) (nth 2 date)) dates))
        (issues nil))
    (cl-loop for (a b) on present
             while b
             when (and (string> (nth 2 a) (nth 2 b))
                       (not (and (nth 3 a) (nth 3 b))))
             do (push (org-canvas--validate-checked
                       'date-order
                       (org-canvas--validate-make-issue
                        'warning loc (car a)
                        (format "%s%s is after %s (%s > %s)"
                                (or prefix "") (car a) (car b)
                                (nth 1 a) (nth 1 b))))
                      issues))
    (nreverse issues)))

(defun org-canvas--validate-check-date-order (date-triples loc)
  "Check chronological ordering for DATE-TRIPLES.
Each triple is (PROP1 PROP2 PROP3) where values should be ordered.
LOC is a (:file :line :heading) plist.  Two triples sharing their
ends can find the same pair out of order; it is reported once.
Returns a list of warning issues."
  (cl-remove-duplicates
   (mapcan (lambda (triple)
             (org-canvas--validate-date-sequence
              (mapcar (lambda (prop)
                        (let ((value (org-entry-get (point) prop)))
                          (list prop value
                                (org-canvas--validate-safe-parse-timestamp value))))
                      triple)
              loc))
           date-triples)
   :test #'equal :from-end t))

;;;; 5. Validation Specs
;;
;; Enum constants are defined in org-canvas-core-config.el and inherited
;; via (require 'org-canvas-core).

(defun org-canvas--validate-specs ()
  "Return validation specs from the property registry.
This replaces the former `org-canvas--validate-specs' defconst."
  (org-canvas--get-validate-specs-from-registry))

;;;; 6. Roster Lookups

(defun org-canvas--validate-override-lookup (var title property)
  "Return PROPERTY of the level-2 heading TITLE in the file VAR names, or nil.
Nil as well when VAR is unbound or the file is missing: the groups
and people files are pulled, and may not exist yet."
  (let ((file (and (boundp var) (symbol-value var))))
    (org-canvas--heading-property-by-title file title property "LEVEL=2")))

(defun org-canvas--validate-departed-people (names)
  "Return those of NAMES marked in people.org as having left the course.
A people pull marks a heading Canvas no longer lists with the state
Canvas gives, or absent (issue #290); a name people.org does not hold
is not departed, only unresolved, and is reported as that."
  (cl-remove-if-not
   (lambda (name)
     (org-canvas--people-departed-state-p
      (org-canvas--validate-override-lookup 'org-canvas-people-file name "ENROLLMENT_STATE")))
   names))

(defun org-canvas--validate-name-list (names)
  "Return NAMES quoted and comma separated, for a warning."
  (mapconcat (lambda (n) (format "'%s'" n)) names ", "))

;;;; 7. Structural Checks on an Entry

(defun org-canvas--validate-check-canvas-owned (value property loc)
  "Warn when PROPERTY, which only Canvas may set, is typed before a sync.
VALUE is the property's text.  A `:canvas-owned' property is written
by a pull and never sent by a push, so on a heading that has no
CANVAS_ID yet it can only be a hope: the object it describes has to
be attached in the web UI once the assignment exists, and then
pulled.  On a stamped heading the value is what a pull wrote and
there is nothing to say; any id stamp counts, a New Quiz item's
CANVAS_ITEM_ID included (issue #365).  Push-only, since it protects a
create.  LOC is a (:file :line :heading) plist (issue #184)."
  (when (and value (not (string-empty-p value))
             (not (cl-some (lambda (id-prop) (org-entry-get (point) id-prop))
                           (org-canvas--delete-id-property-names))))
    (org-canvas--validate-push-only
     (org-canvas--validate-make-issue
      'warning loc property
      (format (concat "%s is set by Canvas, never by a push: sync the "
                      "heading first, attach it in the web UI "
                      "(M-x org-canvas-browse-edit-at-point), then pull")
              property)))))

(defun org-canvas--validate-check-intent (value property observed loc)
  "Warn when the intent VALUE of PROPERTY is not met by OBSERVED.
PROPERTY declares `:intent-of' OBSERVED, the `:canvas-owned' property
a pull writes: WANT_DOCUMENT_PROCESSOR against DOCUMENT_PROCESSOR.  On
a stamped heading whose last pull recorded nothing that satisfies the
declaration (`org-canvas--intent-satisfied-p'), the column exists and
lacks what the file says it should carry — a syllabus promise resting
on a browser step nobody took, or one a course copy dropped.  Not
push-only: it is true of the course whatever you do next.  Silent on
an unstamped heading, where there is no column yet to attach to.  LOC
is a (:file :line :heading) plist (issue #293)."
  (when (and value (not (string-empty-p (string-trim value)))
             (or (org-entry-get (point) "CANVAS_ID")
                 (org-entry-get (point) "CANVAS_URL")))
    (let ((have (org-entry-get (point) observed)))
      (unless (org-canvas--intent-satisfied-p value have)
        (org-canvas--validate-make-issue
         'warning loc property
         (format (concat "%s wanted but %s: attach it on the item's edit "
                         "page in the web UI (M-x org-canvas-browse-edit-at-point; "
                         "Canvas allows no other way), then pull")
                 (string-trim value)
                 (if (and have (not (string-empty-p have)))
                     (format "the last pull recorded %s: %s" observed have)
                   (format "the last pull recorded no %s" observed))))))))

(defun org-canvas--validate-entry-properties (props loc)
  "Validate PROPS list for the heading at point.
LOC is a (:file :line :heading) plist.
A property's `:read-only-values' join its `:values' as accepted input,
because they are values a pull wrote down (issue #167).  A
`:canvas-owned' property is checked for being typed ahead of the
sync that could give it meaning (issue #184), and an `:intent-of'
property against the observation it names (issue #293).
Returns a list of issues."
  (let ((issues nil))
    (dolist (prop props)
      (let* ((name (plist-get prop :name))
             (type (plist-get prop :type))
             (values (append (plist-get prop :values)
                             (plist-get prop :read-only-values)))
             (target-file (plist-get prop :target-file))
             (id-prop (plist-get prop :id-property))
             (value (org-entry-get (point) name))
             (issue
              (pcase type
                ('boolean
                 (org-canvas--validate-check-boolean value name loc))
                ('number
                 (org-canvas--validate-check-number value name loc))
                ('enum
                 (org-canvas--validate-check-enum value name values loc))
                ('csv-enum
                 (org-canvas--validate-check-csv-enum value name values loc))
                ('timestamp
                 (org-canvas--validate-check-timestamp
                  value name loc (plist-get prop :pull-only)))
                ('link
                 (org-canvas--validate-check-link value name target-file id-prop loc)))))
        (when issue
          (push issue issues))
        (when (plist-get prop :canvas-owned)
          (when-let* ((owned (org-canvas--validate-check-canvas-owned value name loc)))
            (push owned issues)))
        (when-let* ((observed (plist-get prop :intent-of))
                    (unmet (org-canvas--validate-check-intent
                            value name observed loc)))
          (push unmet issues))))
    (nreverse issues)))

(defun org-canvas--validate-canvas-deleted (loc)
  "Warn when the heading at point, at LOC, was deleted on Canvas.
A pull marks such a heading `org-canvas--prop-canvas-deleted' and a
push skips it (issue #392).  A finding about the course, not a push,
so a read-only course hears it too."
  (let ((stamp (org-entry-get (point) org-canvas--prop-canvas-deleted)))
    (when stamp
      (org-canvas--validate-make-issue
       'warning loc org-canvas--prop-canvas-deleted
       (format "'%s' was deleted on Canvas (%s); a push skips it.  Delete the heading, or remove %s and its id to create it again"
               (plist-get loc :heading) stamp
               org-canvas--prop-canvas-deleted)))))

(defun org-canvas--validate-entry-at-marker (props date-order structural-fn file)
  "Validate the entry at point using PROPS, DATE-ORDER, and STRUCTURAL-FN.
FILE identifies the source file.  The heading's VALIDATE_ACCEPT
values apply to the findings on its own line (issue #538).  Returns a
list of issues."
  (let* ((line (line-number-at-pos))
         (heading (org-get-heading t t t t))
         (loc (list :file file :line line :heading heading))
         (accepts (org-canvas--validate-heading-accepts))
         (issues (delq nil (list (org-canvas--validate-canvas-deleted loc)))))
    (when props
      (setq issues (nconc issues
                          (org-canvas--validate-entry-properties props loc))))
    (when date-order
      (setq issues (nconc issues
                          (org-canvas--validate-check-date-order
                           date-order loc))))
    (when structural-fn
      (let ((structural-issues (funcall structural-fn loc)))
        (when structural-issues
          (setq issues (nconc issues structural-issues)))))
    (if accepts
        (org-canvas--validate-apply-accepts
         issues accepts loc (cons org-canvas--validate-accept-property ""))
      issues)))

(provide 'org-canvas-validate-issue)
;;; org-canvas-validate-issue.el ends here
