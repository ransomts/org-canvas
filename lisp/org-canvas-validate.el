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

;;; Code:

(require 'org-canvas-core)
(require 'org-table)
(require 'cl-lib)
(require 'compile)

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

;;;;; Check Ids and Accepts (issue #538)
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

;;;; 2. Type-Specific Validators
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

(defvar org-canvas-grading-periods-file
  "Declared here so the grading-period date check compiles; the variable
is defined in org-canvas-grading-periods.el, which validate must not
require (issue #245).")

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

;;;; 3. Date Ordering Check

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

;;;; 4. Validation Specs
;;
;; Enum constants are defined in org-canvas-core-config.el and inherited
;; via (require 'org-canvas-core).

(defun org-canvas--validate-specs ()
  "Return validation specs from the property registry.
This replaces the former `org-canvas--validate-specs' defconst."
  (org-canvas--get-validate-specs-from-registry))

;;;; 5. Structural Validators

(defconst org-canvas--validate-checkpoint-properties
  '("CHECKPOINT_TOPIC_POINTS" "CHECKPOINT_TOPIC_DUE_AT"
    "CHECKPOINT_REPLY_POINTS" "CHECKPOINT_REPLY_DUE_AT"
    "CHECKPOINT_REPLIES_REQUIRED")
  "The properties any one of which makes a discussion checkpointed.")

(defun org-canvas--validate-discussion-checkpoints (loc)
  "Check the CHECKPOINT_ properties of the discussion heading at point.
LOC is a (:file :line :heading) plist.  Any of the five makes the
discussion checkpointed (issue #216): both point values are then
required, DUE_AT does not apply since each checkpoint carries its own
date, and POINTS, when written, must be their sum, which is what
Canvas grades the discussion out of."
  (when (cl-some (lambda (name) (org-entry-get (point) name))
                 org-canvas--validate-checkpoint-properties)
    (let ((topic (org-entry-get (point) "CHECKPOINT_TOPIC_POINTS"))
          (reply (org-entry-get (point) "CHECKPOINT_REPLY_POINTS"))
          (points (org-entry-get (point) "POINTS"))
          (issues nil))
      (dolist (name '("CHECKPOINT_TOPIC_POINTS" "CHECKPOINT_REPLY_POINTS"))
        (unless (org-entry-get (point) name)
          (push (org-canvas--validate-make-issue
                 'error loc name
                 (format "%s is required on a checkpointed discussion" name))
                issues)))
      (when (org-entry-get (point) "DUE_AT")
        (push (org-canvas--validate-make-issue
               'error loc "DUE_AT"
               "DUE_AT does not apply to a checkpointed discussion; each checkpoint carries its own due date")
              issues))
      (when (and points topic reply)
        (let ((sum (+ (string-to-number topic) (string-to-number reply))))
          (unless (= (string-to-number points) sum)
            (push (org-canvas--validate-make-issue
                   'warning loc "POINTS"
                   (format "POINTS is %s but the checkpoints add up to %s, which is what Canvas grades out of"
                           points sum))
                  issues))))
      (nreverse issues))))

(defun org-canvas--validate-single-outcome-link (row file loc)
  "Validate the outcome link in ROW's 4th column.
FILE is the rubrics file path.  LOC is a (:file :line :heading) plist.
Returns an issue or nil."
  (let ((criterion (nth 0 row))
        (outcome-cell (nth 3 row)))
    (when (and outcome-cell
               (not (string-empty-p (string-trim outcome-cell))))
      (org-canvas--validate-resolve-file-link
       outcome-cell "CANVAS_ID" file loc nil
       (format "Rubric criterion '%s' outcome is not a file link" criterion)
       (format "Rubric criterion '%s' outcome link has no CANVAS_ID (sync outcomes first)"
               criterion)
       'warning))))

(defun org-canvas--validate-rubric-outcome-links (table-data file loc)
  "Validate outcome links in 4th column of rubric TABLE-DATA.
FILE is the rubrics file path.  LOC is a (:file :line :heading) plist.
Returns a list of issues.
Retained for backward compatibility with any caller passing legacy table data."
  (let ((issues nil))
    (dolist (row table-data)
      (unless (or (eq row 'hline)
                  (and (listp row) (stringp (nth 0 row))
                       (string-match-p "\\`> " (string-trim-left (nth 0 row)))))
        (let ((issue (org-canvas--validate-single-outcome-link row file loc)))
          (when issue (push issue issues)))))
    (nreverse issues)))

(defun org-canvas--validate-rubric-criterion-outcome (file loc)
  "Validate the :OUTCOME: property on the criterion at point.
FILE is the rubrics file; LOC is a (:file :line :heading) plist describing
the parent rubric.  Returns a list of issues (typically zero or one)."
  (let* ((outcome (org-entry-get (point) "OUTCOME"))
         (issues nil))
    (when (and outcome
               (not (string-empty-p (string-trim outcome)))
               (string-match-p "\\[\\[file:" outcome))
      (let ((row (list (org-get-heading t t t t) "" "" outcome)))
        (let ((issue (org-canvas--validate-single-outcome-link row file loc)))
          (when issue (push issue issues)))))
    (nreverse issues)))

(defun org-canvas--validate-rubric-structure (loc)
  "Check that the rubric heading at point has level-2 criterion children.
Also validates :OUTCOME: properties on each criterion heading.
LOC is a (:file :line :heading) plist."
  (let* ((rubric-pom (point))
         (end (save-excursion (org-end-of-subtree t) (point)))
         (issues nil)
         (file (plist-get loc :file))
         (criterion-count 0))
    (save-excursion
      (goto-char rubric-pom)
      (when (< (point) end)
        (forward-line 1))
      (while (and (< (point) end)
                  (re-search-forward "^\\*\\* " end t))
        (setq criterion-count (1+ criterion-count))
        (save-excursion
          (org-back-to-heading t)
          (let ((outcome-issues
                 (org-canvas--validate-rubric-criterion-outcome file loc)))
            (setq issues (nconc issues outcome-issues))))
        (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point))))
          (goto-char (min subtree-end end)))))
    (when (zerop criterion-count)
      (push (org-canvas--validate-make-issue
             'error loc nil
             "Rubric has no criteria (expected level-2 child headings)")
            issues))
    issues))

(defun org-canvas--validate-file-structure (loc)
  "Check file heading structure: link existence, file on disk, size.
LOC is a (:file :line :heading) plist."
  (let ((issues nil)
        (file (plist-get loc :file))
        (raw-heading (save-excursion
                       (org-back-to-heading t)
                       (looking-at org-complex-heading-regexp)
                       (match-string-no-properties 4))))
    (when (and raw-heading (string-match "\\[\\[file:\\([^]]+\\)" raw-heading))
      (let* ((link-path (match-string 1 raw-heading))
             (abs-path (expand-file-name link-path
                                         (file-name-directory file))))
        (cond
         ((not (file-exists-p abs-path))
          (push (org-canvas--validate-make-issue
                 'error loc nil
                 (format "Linked file does not exist: %s" link-path))
                issues))
         (t
          (let ((size-mb (/ (float (file-attribute-size (file-attributes abs-path)))
                            (* 1024 1024))))
            (when (and (boundp 'org-canvas-max-file-size-mb)
                       (> size-mb org-canvas-max-file-size-mb))
              (push (org-canvas--validate-make-issue
                     'warning loc nil
                     (format "File is %.1f MB (limit: %d MB)"
                             size-mb org-canvas-max-file-size-mb))
                    issues)))))))
    (nreverse issues)))

(declare-function org-canvas--section-meets-parse "org-canvas-sections" (text))

(defun org-canvas--validate-section-structure (loc)
  "Warn if section has no CANVAS_ID (not yet pulled) or an unreadable MEETS.
LOC is a (:file :line :heading) plist.  MEETS is typed by hand and read
only by section-window scoring, which ignores one it cannot parse, so a
typo is reported here rather than scoring a column against no window
\(issue #383)."
  (let ((meets (org-entry-get (point) "MEETS")))
    (delq nil
          (list
           (unless (org-entry-get (point) "CANVAS_ID")
             (org-canvas--validate-make-issue
              'warning loc "CANVAS_ID"
              "Section has no CANVAS_ID (run org-canvas-pull-sections first)"))
           (when (and meets (not (org-canvas--section-meets-parse meets)))
             (org-canvas--validate-make-issue
              'warning loc "MEETS"
              (format "MEETS '%s' is not days and a time range, like MWF 10:10-11:00"
                      meets)))))))

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

(defun org-canvas--validate-override-students (text row-loc)
  "Check TEXT, the names in a `Students:' override row; return an issue or nil.
A name people.org does not hold is reported first; otherwise a name
whose heading says the student left the course, since Canvas refuses
or ignores an override for someone no longer enrolled.  Both
push-only.  ROW-LOC locates the row."
  (let* ((names (split-string text ";" t "[ \t]+"))
         (missing (cl-remove-if
                   (lambda (name)
                     (or (string-match-p "\\`#[0-9]+\\'" name)
                         (org-canvas--validate-override-lookup
                          'org-canvas-people-file name "USER_ID")))
                   names))
         (departed (and (null missing) (org-canvas--validate-departed-people names))))
    (cond
     (missing
      (org-canvas--validate-push-only
       (org-canvas--validate-make-issue
        'warning row-loc nil
        (format "Override student(s) %s not in people.org (pull people first, or write #id)"
                (org-canvas--validate-name-list missing)))))
     (departed
      (org-canvas--validate-push-only
       (org-canvas--validate-make-issue
        'warning row-loc nil
        (format "Override student(s) %s no longer enrolled according to people.org"
                (org-canvas--validate-name-list departed))))))))

(defun org-canvas--validate-override-target (target row-loc)
  "Check TARGET, the first cell of an override row, and return an issue or nil.
A section link is checked elsewhere; `Group: <title>' must name a
group heading in groups.org and `Students: <name>; <name>' person
headings in people.org, unless written as a literal #id (issue #224).
Push-only: a row that resolves to nothing is skipped by the sync and
touches nothing on Canvas.  ROW-LOC locates the row."
  (cond
   ((string-match "\\`Group:[ \t]*\\(.+?\\)[ \t]*\\'" target)
    (let ((title (match-string 1 target)))
      (unless (or (string-match-p "\\`#[0-9]+\\'" title)
                  (org-canvas--validate-override-lookup
                   'org-canvas-groups-file title "CANVAS_ID"))
        (org-canvas--validate-push-only
         (org-canvas--validate-make-issue
          'warning row-loc nil
          (format "Override group '%s' is not in groups.org (pull groups first, or write #id)"
                  title))))))
   ((string-match "\\`Students?:[ \t]*\\(.+?\\)[ \t]*\\'" target)
    (org-canvas--validate-override-students (match-string 1 target) row-loc))
   ((not (string-match "\\[\\[file:" target))
    (org-canvas--validate-make-issue
     'warning row-loc nil
     (format "Override row '%s' is not a section link, Group: or Students:" target)))))

(defun org-canvas--validate-override-rows (file heading)
  "Check each data row in an override table for who it is for.
The first cell must be a section link, `Group: <title>' or
`Students: <name>; <name>' (see `org-canvas--validate-override-target').
FILE and HEADING identify the location.  Point must be at the first data row."
  (let ((issues nil))
    (while (looking-at "^|\\([^-]\\)")
      (let* ((row-line (line-number-at-pos))
             (row-loc (list :file file :line row-line :heading heading))
             (line-text (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
             (fields (split-string line-text "|" t "[ \t]+"))
             (target (and fields (car fields) (string-trim (car fields)))))
        (when (and target (not (string-empty-p target)))
          (when-let* ((issue (org-canvas--validate-override-target target row-loc)))
            (push issue issues))))
      (forward-line 1))
    (nreverse issues)))

(defun org-canvas--validate-external-tool (loc)
  "Check that an LTI-backed assignment at point names a tool, and vice versa.
An `external_tool' submission type with no EXTERNAL_TOOL_URL and no
EXTERNAL_TOOL_ID creates a Canvas assignment that launches nothing —
it looks fine in the assignment list and fails only when a student
opens it.  The mirror case (a tool declared but never selected as the
submission type) is the more common typo: Canvas silently ignores the
attributes and takes submissions itself.  LOC is a
\(:file :line :heading) plist."
  (let* ((submission (or (org-entry-get (point) "SUBMISSION") ""))
         (external-p (member "external_tool" (split-string submission "," t "[ \t]+")))
         (url (org-entry-get (point) "EXTERNAL_TOOL_URL"))
         (id (org-entry-get (point) "EXTERNAL_TOOL_ID"))
         (named (or url id)))
    (cond
     ((and external-p (not named))
      (list (org-canvas--validate-make-issue
             'warning loc "EXTERNAL_TOOL_URL"
             (concat "SUBMISSION: external_tool without EXTERNAL_TOOL_URL or "
                     "EXTERNAL_TOOL_ID — Canvas will create an assignment that "
                     "launches nothing.  M-x org-canvas-list-external-tools "
                     "shows the launch URL of every tool installed in the course"))))
     ((and named (not external-p))
      (list (org-canvas--validate-make-issue
             'warning loc "EXTERNAL_TOOL_URL"
             (concat "EXTERNAL_TOOL_URL/EXTERNAL_TOOL_ID is ignored unless "
                     "SUBMISSION includes external_tool")))))))

(defun org-canvas--validate-grading-period-bounds ()
  "Return the (START . END) ISO pairs of the pulled grading periods, or nil.
Read from `org-canvas-grading-periods-file' when the variable is bound
and the file exists with content; a period whose dates do not parse is
left out."
  (let ((file (and (boundp 'org-canvas-grading-periods-file)
                   org-canvas-grading-periods-file)))
    (when (and file (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 0))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (delq nil
              (org-map-entries
               (lambda ()
                 (let ((start (org-canvas--validate-safe-parse-timestamp
                               (org-entry-get (point) "START_DATE")))
                       (end (org-canvas--validate-safe-parse-timestamp
                             (org-entry-get (point) "END_DATE"))))
                   (and start end (cons start end))))
               "LEVEL=1" 'file))))))

(defun org-canvas--validate-due-in-grading-period (loc)
  "Warn when the DUE_AT at point falls outside every pulled grading period.
LOC is a (:file :line :heading) plist.  Says nothing when the heading
has no DUE_AT, when no grading periods have been pulled, or when the
date sits inside a period, bounds included."
  (let* ((raw (org-entry-get (point) "DUE_AT"))
         (due (org-canvas--validate-safe-parse-timestamp raw))
         (periods (and due (org-canvas--validate-grading-period-bounds))))
    (when (and periods
               (not (cl-some (lambda (period)
                               (and (not (string< due (car period)))
                                    (not (string< (cdr period) due))))
                             periods)))
      (list (org-canvas--validate-make-issue
             'warning loc "DUE_AT"
             (format "DUE_AT %s falls outside every grading period in %s"
                     raw (file-name-nondirectory org-canvas-grading-periods-file)))))))

;;;; 5a-1b. Scheduled Posting (issue #352)

(defun org-canvas--validate-post-schedule-problem (grades comments policy)
  "Return what is wrong with a posting schedule, or nil.
GRADES and COMMENTS are the raw POST_GRADES_AT and POST_COMMENTS_AT,
POLICY the raw POST_POLICY.  Canvas keeps a schedule only with both
times, grades no earlier than comments, under manual posting."
  (let ((grades-iso (org-canvas--validate-safe-parse-timestamp grades))
        (comments-iso (org-canvas--validate-safe-parse-timestamp comments)))
    (cond
     ((not (and grades comments))
      (format "%s without %s: Canvas schedules a posting only with both times"
              (if grades "POST_GRADES_AT" "POST_COMMENTS_AT")
              (if grades "POST_COMMENTS_AT" "POST_GRADES_AT")))
     ((and grades-iso comments-iso
           (time-less-p (date-to-time grades-iso) (date-to-time comments-iso)))
      (format "POST_GRADES_AT %s is before POST_COMMENTS_AT %s: Canvas posts grades no earlier than comments"
              grades comments))
     ((equal policy "automatic")
      "a posting schedule needs manual posting, and POST_POLICY is automatic"))))

(defun org-canvas--validate-post-schedule (loc)
  "Check the posting schedule of the assignment heading at point.
LOC is a (:file :line :heading) plist.  An error when Canvas would
refuse or drop the schedule; nothing when the heading has none.  A
time that does not parse is reported by the timestamp check."
  (let ((grades (org-entry-get (point) "POST_GRADES_AT"))
        (comments (org-entry-get (point) "POST_COMMENTS_AT")))
    (when-let* (((or grades comments))
                (problem (org-canvas--validate-post-schedule-problem
                          grades comments
                          (org-entry-get (point) "POST_POLICY"))))
      (list (org-canvas--validate-make-issue
             'error loc (if grades "POST_GRADES_AT" "POST_COMMENTS_AT")
             problem)))))

;;;; 5a-2. Grading Schemes
;;
;; The table helpers live in the grading-schemes module, which is always
;; loaded with org-canvas; validate names them rather than requiring a
;; feature module (the dependency rule in
;; documentation/architecture-guide.md).

(declare-function org-canvas--grading-scheme-find-table "org-canvas-grading-schemes" (end))
(declare-function org-canvas--grading-scheme-table-rows "org-canvas-grading-schemes" (table))
(declare-function org-canvas--grading-scheme-cutoff-number "org-canvas-grading-schemes" (cell))

(defun org-canvas--validate-grading-scheme-structure (loc)
  "Check the scheme table under the heading at point.
LOC is a (:file :line :heading) plist.  The table must exist, every
cutoff must be a number no higher than 100, the rows must descend, and
the last row must be 0, or Canvas has no grade for the lowest scores."
  (let* ((end (save-excursion (org-end-of-subtree t) (point)))
         (table (org-canvas--grading-scheme-find-table end))
         (rows (and table (org-canvas--grading-scheme-table-rows table)))
         (issue (lambda (message)
                  (list (org-canvas--validate-make-issue 'error loc "scheme" message)))))
    (cond
     ((null table)
      (funcall issue "Grading scheme has no #+NAME: scheme table (| Grade | Cutoff |)"))
     ((null rows)
      (funcall issue "Grading scheme table has no grade rows"))
     (t
      (let ((cutoffs (mapcar (lambda (row)
                               (cons (car row)
                                     (org-canvas--grading-scheme-cutoff-number (cdr row))))
                             rows))
            (issues nil))
        (dolist (row cutoffs)
          (cond
           ((null (cdr row))
            (push (format "Cutoff for '%s' is not a number" (car row)) issues))
           ((> (cdr row) 100)
            (push (format "Cutoff for '%s' is above 100" (car row)) issues))))
        (unless issues
          (cl-loop for (a b) on cutoffs
                   while b
                   unless (> (cdr a) (cdr b))
                   do (push (format "Cutoffs must descend: '%s' (%s) is not above '%s' (%s)"
                                    (car a) (cdr a) (car b) (cdr b))
                            issues)
                   and return nil)
          (unless (zerop (cdr (car (last cutoffs))))
            (push (format "The last row, '%s', should have a cutoff of 0 so every score has a grade"
                          (car (car (last cutoffs))))
                  issues)))
        (mapcan (lambda (message) (funcall issue message)) (nreverse issues)))))))

(defun org-canvas--validate-grading-standard-ids (schemes-file)
  "Warn where settings.org or assignments.org name a scheme SCHEMES-FILE lacks.
A GRADING_STANDARD_ID that matches no CANVAS_ID in SCHEMES-FILE is
either stale or never pulled; the warning sits on the heading that
names it.  An id of 0 names no scheme and is passed over.  A scheme
Canvas deleted is not known, and a heading Canvas deleted is not
checked (issue #402)."
  (let ((known (with-current-buffer (org-canvas--find-file-noselect schemes-file)
                 (save-excursion
                   (delq nil (org-map-entries
                              (lambda ()
                                (unless (org-canvas--sync-deleted-stamp)
                                  (org-entry-get (point) "CANVAS_ID")))
                              "LEVEL=1" 'file)))))
        (issues nil))
    (dolist (var '(org-canvas-settings-file org-canvas-assignments-file))
      (let ((file (and (boundp var) (symbol-value var))))
        (when (and file (file-exists-p file))
          (with-current-buffer (org-canvas--find-file-noselect file)
            (save-excursion
              (org-map-entries
               (lambda ()
                 (let ((id (org-entry-get (point) "GRADING_STANDARD_ID")))
                   (when (and id (not (member id known))
                              (not (org-canvas--sync-deleted-stamp))
                              (not (member id '("0" ""))))
                     (push (org-canvas--validate-make-issue
                            'warning
                            (list :file file :line (line-number-at-pos)
                                  :heading (org-get-heading t t t t))
                            "GRADING_STANDARD_ID"
                            (format "GRADING_STANDARD_ID %s matches no CANVAS_ID in %s; pull the schemes, or check the id"
                                    id (file-name-nondirectory schemes-file)))
                           issues))))
               "LEVEL=1" 'file))))))
    (nreverse issues)))

(defun org-canvas--validate-assignment-structure (loc)
  "Check the LTI tool, grading period, posting schedule and override table.
LOC is a (:file :line :heading) plist."
  (let ((late-window (org-canvas--validate-late-window-defaults)))
    (nconc (org-canvas--validate-external-tool loc)
           (org-canvas--validate-due-in-grading-period loc)
           (org-canvas--validate-post-schedule loc)
           (org-canvas--validate-heading-late-window loc late-window)
           (org-canvas--validate-override-table loc late-window))))

(cl-defun org-canvas--validate-override-table (loc &optional late-window)
  "Check the `#+NAME: overrides' table under the assignment at point.
Each row is checked for who it is for, for a section link's id, for
the order of its dates and, given LATE-WINDOW (the assignment's dates
when the course's late-window rule applies), against that rule.  LOC
is a (:file :line :heading) plist.  Nil without a table."
  (let ((end (save-excursion (org-end-of-subtree t) (point)))
        (dates (org-canvas--validate-heading-dates)))
    (save-excursion
      (unless (re-search-forward "^#\\+NAME: overrides" end t)
        (cl-return-from org-canvas--validate-override-table nil))
      (forward-line 1)
      (unless (looking-at "^|")
        (cl-return-from org-canvas--validate-override-table nil))
      (let ((header-pos (point))
            (file (plist-get loc :file))
            (heading (plist-get loc :heading)))
        (forward-line 1)
        (when (looking-at "^|-")
          (forward-line 1))
        (let* ((rows-pos (point))
               (row-issues (org-canvas--validate-override-rows file heading))
               (id-issues (progn (goto-char rows-pos)
                                 (org-canvas--validate-override-section-ids
                                  file heading))))
          (goto-char header-pos)
          (nconc row-issues id-issues
                 (org-canvas--validate-override-row-dates
                  loc late-window dates)))))))

(defun org-canvas--validate-heading-dates ()
  "Return the assignment's (:due :unlock :lock) at point, raw Org timestamps."
  (list :due (org-entry-get (point) "DUE_AT")
        :unlock (org-entry-get (point) "UNLOCK_AT")
        :lock (org-entry-get (point) "LOCK_AT")))

(declare-function org-canvas--override-date-columns "org-canvas-sections" (header))
(declare-function org-canvas--override-parse-timestamp-cell "org-canvas-sections" (cell))
(declare-function org-canvas--override-accept-column "org-canvas-sections" (header))

(defun org-canvas--validate-override-cell (row column)
  "Return (VALUE . ISO) for the date cell at COLUMN of override ROW.
VALUE is the trimmed text, nil when the cell is empty or absent; ISO
is what the override push reads it as
\(`org-canvas--override-parse-timestamp-cell', in the course zone), nil
when it reads nothing."
  (let ((text (string-trim (or (and column (nth column row)) ""))))
    (unless (string-empty-p text)
      (cons text (ignore-errors
                   (org-canvas--override-parse-timestamp-cell text))))))

(defun org-canvas--validate-override-row-label (row)
  "Return the first cell of override ROW, a link shown as its description."
  (org-link-display-format (string-trim (or (nth 0 row) ""))))

(defun org-canvas--validate-override-row-date (row column inherited)
  "Return the (NAME VALUE ISO INHERITED) date of override ROW at COLUMN.
The row's own date when the push reads one there, else INHERITED, the
assignment's raw timestamp, marked as inherited: Canvas reads an
override that omits a date as not overriding it, so the assignment's
applies (issue #538)."
  (let ((cell (org-canvas--validate-override-cell row column)))
    (if (cdr cell)
        (list (car cell) (cdr cell) nil)
      (list (and inherited (format "%s (the assignment's)" inherited))
            (org-canvas--validate-safe-parse-timestamp inherited)
            t))))

(defun org-canvas--validate-override-row-date-order (row columns row-loc
                                                         &optional dates)
  "Check that ROW's Unlock At, Due At and Lock At run in that order.
COLUMNS are the due, unlock and lock indexes
\(`org-canvas--override-date-columns').  A date the row leaves empty,
or the push cannot read, is the assignment's, from DATES
\(:due :unlock :lock); a pair is reported only when the row sets at
least one of its two dates, since the assignment's own order is its
heading's to report (issues #534, #538).  ROW-LOC locates the row."
  (org-canvas--validate-date-sequence
   (cl-mapcar (lambda (name column key)
                (cons name (org-canvas--validate-override-row-date
                            row column (plist-get dates key))))
              '("Unlock At" "Due At" "Lock At")
              (list (nth 1 columns) (nth 0 columns) (nth 2 columns))
              '(:unlock :due :lock))
   row-loc
   (format "Override row '%s': "
           (org-canvas--validate-override-row-label row))))

(defun org-canvas--validate-override-row-late-window (row columns row-loc defaults)
  "Apply the course's late-window rule to override ROW, or return nil.
COLUMNS are the due, unlock and lock indexes.  DEFAULTS is the
assignment's own (:due :unlock :lock), from
`org-canvas--validate-late-window-defaults', nil when the rule does
not apply.  A row is checked only when it sets a due or a lock date of
its own; a date it leaves empty, or one the push cannot read, is the
assignment's, as on Canvas, where an override not setting a date
keeps the assignment's (issue #536).  ROW-LOC locates the row."
  (when defaults
    (let* ((dates (mapcar (lambda (column)
                            (let ((cell (org-canvas--validate-override-cell row column)))
                              (and (cdr cell) (car cell))))
                          columns))
           (due (nth 0 dates))
           (lock (nth 2 dates)))
      (when (or due lock)
        (org-canvas--validate-late-window
         (or due (plist-get defaults :due))
         (or lock (plist-get defaults :lock))
         (or (nth 1 dates) (plist-get defaults :unlock))
         row-loc
         (format "Override row '%s': "
                 (org-canvas--validate-override-row-label row)))))))

(defun org-canvas--validate-override-row-accepts (issues row column row-loc)
  "Apply the Accept cell of override ROW, at COLUMN, to ISSUES.
The cell holds accepts separated by `;', each a check id and the
reason (issue #538).  ROW-LOC locates the row."
  (let ((accepts (and column
                      (split-string (or (nth column row) "") ";" t "[ \t]+"))))
    (if accepts
        (org-canvas--validate-apply-accepts
         issues accepts row-loc
         (cons nil (format "Override row '%s': "
                           (org-canvas--validate-override-row-label row))))
      issues)))

(defun org-canvas--validate-override-row-dates (loc &optional defaults dates)
  "Check the dates of each row of the override table at point.
Point is on the table's first line, its header, which names the date
columns as the push reads them.  LOC is the assignment's
\(:file :line :heading) plist; each issue carries its row's line.
DEFAULTS, when the course's late-window rule applies to the
assignment, are its own dates (see
`org-canvas--validate-override-row-late-window'); DATES are its dates
whether or not it does, which a row's date order inherits.  A row's
Accept cell applies to its findings (issue #538)."
  (let* ((start (line-number-at-pos))
         (table (org-table-to-lisp))
         (columns (org-canvas--override-date-columns (car table)))
         (accept (org-canvas--override-accept-column (car table))))
    (cl-loop for row in (cdr table)
             for line from (1+ start)
             for row-loc = (list :file (plist-get loc :file) :line line
                                 :heading (plist-get loc :heading))
             unless (eq row 'hline)
             nconc (org-canvas--validate-override-row-accepts
                    (nconc (org-canvas--validate-override-row-date-order
                            row columns row-loc dates)
                           (org-canvas--validate-override-row-late-window
                            row columns row-loc defaults))
                    row accept row-loc))))

;;;; 5a-1c. A Course's Late Window (issue #536)

(defcustom org-canvas-validate-lock-after-due-hours nil
  "Hours after its due date an online column must lock, or nil for no rule.
A number, or a list of the offsets allowed, 0 meaning a lock at the
due date: \\='(0 48) accepts either.  `org-canvas-validate' warns
about a dated online column (SUBMISSION names an online type) with no
lock date, or one locking at another offset, on the assignment heading
and on each override row.  Hours are counted by the clock, as the
dates are written.  A course rule: set it in the course's
`org-canvas-course-config-file', beside its syllabus late policy."
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Hours")
                 (repeat :tag "Any of these hours" number))
  :group 'org-canvas)

(defcustom org-canvas-validate-require-unlock nil
  "Non-nil when every dated online column must carry an unlock date.
`org-canvas-validate' warns about an assignment heading or override
row with a due date and no unlock date.  A course rule: set it in the
course's `org-canvas-course-config-file'."
  :type 'boolean
  :group 'org-canvas)

(defconst org-canvas--validate-online-submission-types
  '("online_upload" "online_url" "online_text_entry" "media_recording")
  "Submission types Canvas groups as Online, which the late window covers.")

(defun org-canvas--validate-late-window-defaults ()
  "Return the assignment's dates for the late-window rule, or nil.
Nil unless a rule is set and the assignment at point takes an online
submission; otherwise its (:due :unlock :lock), raw Org timestamps."
  (when (and (or org-canvas-validate-lock-after-due-hours
                 org-canvas-validate-require-unlock)
             (cl-intersection
              (split-string (or (org-entry-get (point) "SUBMISSION") "")
                            "," t "[ \t]+")
              org-canvas--validate-online-submission-types
              :test #'string=))
    (org-canvas--validate-heading-dates)))

(defun org-canvas--validate-wall-seconds (timestamp)
  "Return Org TIMESTAMP as seconds, read as written in no zone."
  (let ((decoded (org-parse-time-string timestamp)))
    (setf (nth 8 decoded) 0)
    (float-time (encode-time decoded))))

(defun org-canvas--validate-wall-hours (from to)
  "Return the hours from Org timestamp FROM to TO by the clock, or nil.
Both are read as written, in no zone, so a lock two days after a due
date is 48 hours across a daylight-saving change too.  Nil when either
does not parse."
  (ignore-errors
    (/ (- (org-canvas--validate-wall-seconds to)
          (org-canvas--validate-wall-seconds from))
       3600.0)))

(defun org-canvas--validate-format-hours (hours)
  "Return HOURS, a list of numbers, as text for a message.
Each is rounded to a tenth of an hour."
  (concat (mapconcat (lambda (h)
                       (org-canvas--validate-format-weight
                        (/ (round (* 10 h)) 10.0)))
                     hours " or ")
          " hour(s)"))

(defun org-canvas--validate-lock-offset-text (hours)
  "Return where a lock HOURS after the due date falls, for a message."
  (if (< hours 0)
      (format "locks %s before the due date"
              (org-canvas--validate-format-hours (list (- hours))))
    (format "locks %s after the due date"
            (org-canvas--validate-format-hours (list hours)))))

(defun org-canvas--validate-lock-window (due lock loc prefix)
  "Check LOCK against DUE for `org-canvas-validate-lock-after-due-hours'.
DUE and LOCK are raw Org timestamps, LOCK nil when the column has
none.  LOC locates the column and PREFIX starts the message.  Returns
an issue or nil, check `lock-window'; a lock date that does not parse
is the timestamp check's to report."
  (when-let* ((allowed (if (numberp org-canvas-validate-lock-after-due-hours)
                           (list org-canvas-validate-lock-after-due-hours)
                         org-canvas-validate-lock-after-due-hours)))
    (let ((hours (and lock (org-canvas--validate-wall-hours due lock))))
      (org-canvas--validate-checked
       'lock-window
       (cond
        ((null lock)
         (org-canvas--validate-make-issue
          'warning loc "LOCK_AT"
          (format "%sLock window missing: due %s with no lock date; the course locks online columns %s after the due date"
                  prefix due (org-canvas--validate-format-hours allowed))))
        ((and hours
              (not (cl-some (lambda (h) (= (round (* 60 h)) (round (* 60 hours))))
                            allowed)))
         (org-canvas--validate-make-issue
          'warning loc "LOCK_AT"
          (format "%sLock window mismatch: %s (due %s, lock %s); the course allows %s"
                  prefix (org-canvas--validate-lock-offset-text hours)
                  due lock (org-canvas--validate-format-hours allowed)))))))))

(defun org-canvas--validate-late-window (due lock unlock loc &optional prefix)
  "Check one online column against the course's late-window rule.
DUE, LOCK and UNLOCK are its raw Org timestamps, an absent one nil.
A column with no due date that parses has no window and is passed
over.  The findings are warnings (`org-canvas-validate-lock-after-due-hours',
`org-canvas-validate-require-unlock'), the lock window's push-only
once the column is past due, since a column that has closed is a
record and not a push to come; a closed column is not asked for an
unlock date at all, since it opened as it ran.  LOC locates the
column and PREFIX starts each message."
  (when-let* ((due-iso (org-canvas--validate-safe-parse-timestamp due)))
    (let* ((prefix (or prefix ""))
           (past (string< due-iso (org-canvas-current-iso8601-timestamp)))
           (issues (delq nil
                         (list
                          (org-canvas--validate-lock-window due lock loc prefix)
                          (when (and org-canvas-validate-require-unlock (null unlock)
                                     (not past))
                            (org-canvas--validate-checked
                             'require-unlock
                             (org-canvas--validate-make-issue
                              'warning loc "UNLOCK_AT"
                              (format "%sUnlock date missing: the course requires one on every online column"
                                      prefix))))))))
      (if past
          (mapcar #'org-canvas--validate-push-only issues)
        issues))))

(defun org-canvas--validate-heading-late-window (loc defaults)
  "Check the assignment heading at LOC against the course's late window.
DEFAULTS are its dates from `org-canvas--validate-late-window-defaults',
nil when the rule does not apply."
  (when defaults
    (org-canvas--validate-late-window
     (plist-get defaults :due) (plist-get defaults :lock)
     (plist-get defaults :unlock) loc)))

;; Messages (issue #232): the resolver lives in the messages module,
;; named here rather than required, as the grading-scheme helpers are.

(declare-function org-canvas--message-resolve-target "org-canvas-messages" (to &optional offline))
(declare-function org-canvas--message-unresolved-advice "org-canvas-messages" (kind))
(declare-function org-canvas--message-body "org-canvas-messages" ())
(declare-function org-canvas--message-target-kind "org-canvas-messages" (to))

(defun org-canvas--validate-message-departed (to)
  "Return the students named in TO with a departed people.org heading."
  (pcase (org-canvas--message-target-kind to)
    (`(students . ,rest)
     (org-canvas--validate-departed-people (split-string rest ";" t "[ \t]+")))))

(defun org-canvas--validate-message-to (to loc)
  "Check the TO value of the message heading at point.
LOC is a (:file :line :heading) plist.  TO is required; a value that is
none of the kinds is an error; a name that resolves to nothing pulled
is a warning naming what to pull.  Self resolves offline."
  (let ((target (and to (not (string-empty-p (string-trim to)))
                     (org-canvas--message-resolve-target to t))))
    (cond
     ((null target)
      (org-canvas--validate-make-issue
       'error loc "TO" "TO is required: Self, Course, Section: name, Group: name or Students: names"))
     ((plist-get target :invalid)
      (org-canvas--validate-make-issue
       'error loc "TO"
       (format "TO: '%s' is none of Self, Course, Section: name, Group: name or Students: names" to)))
     ((plist-get target :unresolved)
      (org-canvas--validate-make-issue
       'warning loc "TO"
       (format "TO: could not resolve %s (%s)"
               (mapconcat (lambda (n) (format "'%s'" n)) (plist-get target :unresolved) ", ")
               (org-canvas--message-unresolved-advice (plist-get target :kind)))))
     ((org-canvas--validate-message-departed to)
      (org-canvas--validate-make-issue
       'warning loc "TO"
       (format "TO: %s no longer enrolled according to people.org"
               (org-canvas--validate-name-list (org-canvas--validate-message-departed to))))))))

(defun org-canvas--validate-message-structure (loc)
  "Check the message heading at point: its TO, its body and its stamp.
LOC is a (:file :line :heading) plist.  An empty body is an error; a
heading already sent whose body no longer matches the PAYLOAD_HASH
stamped at the send warns that editing it sends nothing (issue #232)."
  (let* ((body (org-canvas--message-body))
         (sent (org-entry-get (point) "SENT_AT"))
         (hash (org-entry-get (point) "PAYLOAD_HASH"))
         (issues (list (org-canvas--validate-message-to (org-entry-get (point) "TO") loc))))
    (when (string-empty-p body)
      (push (org-canvas--validate-make-issue
             'error loc "body" "The message has no body; nothing to send")
            issues))
    (when (and sent hash (not (equal hash (md5 body))))
      (push (org-canvas--validate-make-issue
             'warning loc "SENT_AT"
             (format "Already sent %s; the edited text will not go out (add a new heading to send again)" sent))
            issues))
    (delq nil (nreverse issues))))

;;;; 5b. Cross-Module Structural Validators

(defun org-canvas--validate-module-item-target (abs-path clean-heading file-path loc)
  "Validate that CLEAN-HEADING exists in ABS-PATH and has a CANVAS_ID.
FILE-PATH is the relative path for error messages.  LOC is the location plist.
Returns a list of issues."
  (let ((heading-point (org-canvas--find-heading-in-file abs-path clean-heading)))
    (cond
     ((not heading-point)
      (list (org-canvas--validate-make-issue
             'error loc nil
             (format "Module item links to missing heading: '%s' in %s"
                     clean-heading file-path))))
     (t
      (with-current-buffer (org-canvas--find-file-noselect abs-path)
        (unless (or (org-entry-get heading-point "CANVAS_ID")
                    (org-entry-get heading-point "CANVAS_URL"))
          (list (org-canvas--validate-make-issue
                 'warning loc nil
                 (format "Module item target '%s' has no CANVAS_ID (sync it first)"
                         clean-heading)
                 t))))))))

(defun org-canvas--validate-module-heading-link (raw-heading loc)
  "Check the `file:X.org::*Heading' link in RAW-HEADING, or return nil.
Nil too when RAW-HEADING holds no such link.  LOC is a
\(:file :line :heading) plist."
  ;; Greedy `.+' for the heading group so a target heading that is
  ;; itself a file link (nested brackets) is captured whole;
  ;; backtracking lands on the last `][' boundary.  A `[^]]'-based
  ;; group truncates at the first nested `]' (false "missing
  ;; heading" on links to files.org headings).
  (when (string-match "\\[\\[file:\\([^]]+\\)::\\*\\(.+\\)\\]\\[" raw-heading)
    (let* ((file-path (match-string 1 raw-heading))
           (source-dir (file-name-directory (plist-get loc :file)))
           (abs-path (expand-file-name file-path source-dir)))
      (if (not (file-exists-p abs-path))
          (list (org-canvas--validate-make-issue
                 'error loc nil
                 (format "Module item links to missing file: %s" file-path)))
        (let ((clean-heading (replace-regexp-in-string
                              "\\\\[][]"
                              (lambda (m) (substring m 1))
                              (match-string 2 raw-heading))))
          (org-canvas--validate-module-item-target
           abs-path clean-heading file-path loc))))))

(declare-function org-canvas--module-resolve-file-path "org-canvas-modules"
                  (file modules-file-dir))
(declare-function org-canvas--module-content-target "org-canvas-modules"
                  (rel-path modules-file-dir))

(defun org-canvas--validate-module-file-link (raw-heading loc)
  "Check the bare `file:PATH' link in RAW-HEADING, or return nil.
A module item linking straight at a file is resolved by the push
against the `files.org' heading whose link target is the same path
\(`org-canvas--module-content-target'), and an Org file is found on
disk relative to the modules file, by its basename as a fallback
\(`org-canvas--module-resolve-file-path').  A path neither finds is an
error, as a missing heading is: the push skips the item (issue #535).
LOC is a (:file :line :heading) plist."
  (when (string-match "\\[\\[file:\\([^]]+\\)\\]\\[" raw-heading)
    (let ((path (match-string 1 raw-heading))
          (dir (file-name-directory (plist-get loc :file))))
      (unless (or (string-match-p "::" path)
                  (org-canvas--module-resolve-file-path path dir)
                  (org-canvas--module-content-target path dir))
        (list (org-canvas--validate-make-issue
               'error loc nil
               (format "Module item links to missing file: %s (not on disk, and no files.org heading links to it)"
                       path)))))))

(defun org-canvas--validate-module-item-link (loc)
  "Check that module item link at point resolves to a synced heading.
LOC is a (:file :line :heading) plist.
Skips items with EXTERNAL_URL property or plain-text SubHeaders."
  (unless (org-entry-get (point) "EXTERNAL_URL")
    (when-let* ((raw-heading (save-excursion
                               (org-back-to-heading t)
                               (looking-at org-complex-heading-regexp)
                               (match-string-no-properties 4))))
      (if (string-match-p "\\[\\[file:[^]]+::\\*" raw-heading)
          (org-canvas--validate-module-heading-link raw-heading loc)
        (org-canvas--validate-module-file-link raw-heading loc)))))

(defun org-canvas--validate-quiz-question-points (pos)
  "Return the point value for the question at POS.
For question groups, returns PICK_COUNT * QUESTION_POINTS."
  (save-excursion
    (goto-char pos)
    (org-back-to-heading t)
    (let ((qtype (org-entry-get (point) "TYPE"))
          (pick-count (org-entry-get (point) "PICK_COUNT"))
          (question-points (org-entry-get (point) "QUESTION_POINTS"))
          (points (org-entry-get (point) "POINTS")))
      (cond
       ((and (equal qtype "group") pick-count question-points)
        (* (string-to-number pick-count) (string-to-number question-points)))
       (points (string-to-number points))
       (t 0)))))

(defun org-canvas--validate-quiz-sum-question-points ()
  "Sum POINTS across level-2 subheadings of the current quiz subtree.
Return nil when there are no question subheadings."
  (let ((end (save-excursion (org-end-of-subtree t) (point)))
        (markers nil))
    (save-excursion
      (while (re-search-forward "^\\*\\* " end t)
        (push (point-marker) markers)))
    (when markers
      (let* ((ordered (nreverse markers))
             (sum (cl-loop for m in ordered
                           sum (org-canvas--validate-quiz-question-points
                                (marker-position m)))))
        (dolist (m ordered) (set-marker m nil))
        sum))))

(defconst org-canvas--validate-survey-quiz-types '("survey" "graded_survey")
  "QUIZ_TYPE values whose responses can be collected anonymously.")

(defun org-canvas--validate-quiz-anonymity (loc)
  "Warn when the unstamped survey at point does not say who responds.
LOC is a (:file :line :heading) plist.  A push creates the survey,
and Canvas creates it named unless it is told otherwise; a response
cannot be made anonymous after it is submitted, so a survey copied
from an anonymous one must say so (issue #349).  A stamped survey is
left alone: its push sends nothing about anonymity and changes
nothing."
  (when (and (member (org-entry-get (point) "QUIZ_TYPE")
                     org-canvas--validate-survey-quiz-types)
             (not (org-entry-get (point) "CANVAS_ID"))
             (not (org-entry-get (point) "ANONYMOUS_SUBMISSIONS")))
    (list (org-canvas--validate-push-only
           (org-canvas--validate-make-issue
            'warning loc "ANONYMOUS_SUBMISSIONS"
            (concat "Survey sets no ANONYMOUS_SUBMISSIONS; a push creates"
                    " it named (set true or false)"))))))

(defun org-canvas--validate-quiz-point-total (loc)
  "Check that quiz POINTS matches sum of question points.
LOC is a (:file :line :heading) plist.  The accommodations table
under the quiz, when there is one, is checked as well (issue #230),
and so is whether a new survey says it is anonymous (issue #349)."
  (nconc
   (org-canvas--validate-quiz-anonymity loc)
   (when-let* ((declared-points (org-entry-get (point) "POINTS"))
               (sum (org-canvas--validate-quiz-sum-question-points))
               (declared (string-to-number declared-points)))
     (unless (= declared sum)
       (list (org-canvas--validate-make-issue
              'warning loc "POINTS"
              (format "Quiz POINTS is %s but question total is %s"
                      declared-points (number-to-string sum))))))
   (org-canvas--validate-quiz-accommodations loc)))

(defun org-canvas--validate-accommodation-cells (line)
  "Split the table LINE into trimmed cells."
  (mapcar #'string-trim (split-string (string-trim line "|" "|") "|")))

(defun org-canvas--validate-accommodation-number-columns (header)
  "Return the indexes of the Extra attempts and Extra time cells in HEADER.
HEADER is the table's first row, as cells.  A header naming neither
falls back to positions 1 and 2, as the sync does."
  (let* ((titles (mapcar #'downcase header))
         (attempts (cl-position "extra attempts" titles :test #'string=))
         (time (cl-position "extra time" titles :test #'string=)))
    (if (or attempts time)
        (delq nil (list attempts time))
      (list 1 2))))

(defun org-canvas--validate-accommodation-row (cells columns row-loc)
  "Check one accommodations row, CELLS, and return its issues.
The student must be a person heading in people.org or a literal #id
\(push-only: the sync skips a row it cannot resolve); the cells at
COLUMNS must be whole numbers (an error, since the sync refuses the
table).  ROW-LOC locates the row."
  (let ((student (or (nth 0 cells) ""))
        (issues nil))
    (unless (or (string-empty-p student)
                (string-match-p "\\`#[0-9]+\\'" student)
                (org-canvas--validate-override-lookup
                 'org-canvas-people-file student "USER_ID"))
      (push (org-canvas--validate-push-only
             (org-canvas--validate-make-issue
              'warning row-loc nil
              (format "Accommodation student '%s' is not in people.org (pull people first, or write #id)"
                      student)))
            issues))
    (when (org-canvas--validate-departed-people (list student))
      (push (org-canvas--validate-push-only
             (org-canvas--validate-make-issue
              'warning row-loc nil
              (format "Accommodation student '%s' is no longer enrolled according to people.org" student)))
            issues))
    (dolist (col columns)
      (let ((cell (or (nth col cells) "")))
        (unless (string-match-p "\\`[0-9]*\\'" cell)
          (push (org-canvas--validate-make-issue
                 'error row-loc nil
                 (format "Accommodation cell '%s' is not a whole number" cell))
                issues))))
    (nreverse issues)))

(cl-defun org-canvas--validate-quiz-accommodations (loc)
  "Check the `#+NAME: accommodations' table under the quiz at point.
LOC is a (:file :line :heading) plist.  Nil without a table."
  (let ((end (save-excursion (org-end-of-subtree t) (point)))
        (issues nil))
    (save-excursion
      (unless (re-search-forward "^#\\+NAME:[ \t]+accommodations[ \t]*$" end t)
        (cl-return-from org-canvas--validate-quiz-accommodations nil))
      (forward-line 1)
      (unless (looking-at "^|")
        (cl-return-from org-canvas--validate-quiz-accommodations nil))
      (let ((columns (org-canvas--validate-accommodation-number-columns
                      (org-canvas--validate-accommodation-cells
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))))
        (forward-line 1)
        (when (looking-at "^|-")
          (forward-line 1))
        (while (looking-at "^|\\([^-]\\)")
          (let ((row-loc (list :file (plist-get loc :file)
                               :line (line-number-at-pos)
                               :heading (plist-get loc :heading)))
                (cells (org-canvas--validate-accommodation-cells
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
            (setq issues (nconc issues (org-canvas--validate-accommodation-row
                                        cells columns row-loc))))
          (forward-line 1))))
    issues))

(defun org-canvas--count-assignments-in-group (group-name assignments-file)
  "Count assignments in GROUP-NAME by scanning ASSIGNMENTS-FILE.
A heading Canvas deleted is not counted (issue #402)."
  (if (and assignments-file (file-exists-p assignments-file))
      (let ((count 0))
        (with-current-buffer (org-canvas--find-file-noselect assignments-file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (let ((group-prop (org-entry-get (point) "GROUP")))
                 (when (and group-prop
                            (not (org-canvas--sync-deleted-stamp))
                            (string-match (regexp-quote group-name) group-prop))
                   (setq count (1+ count)))))
             "LEVEL=1" 'file)))
        count)
    0))

(defun org-canvas--validate-all-day-span (loc)
  "Warn when ALL_DAY: true is set on a multi-day calendar event.
Canvas does not store the flag for a span: it keeps the times, sets
`all_day' to false and fills `all_day_date', so the property has no
effect and can never round-trip (issue #93).  LOC is a
\(:file :line :heading) plist."
  (let ((all-day (org-entry-get (point) "ALL_DAY")))
    (when (and all-day (string= (downcase all-day) "true")
               (org-canvas--org-timestamps-span-days-p
                (org-entry-get (point) "START_AT")
                (org-entry-get (point) "END_AT")))
      (list (org-canvas--validate-make-issue
             'warning loc "ALL_DAY"
             "ALL_DAY: true has no effect on a multi-day event — Canvas keeps the times and stores all_day as false; the flag applies to single-day events only")))))

(defun org-canvas--validate-page-structure (loc)
  "Check page-level structural constraints at point.
Canvas rejects FRONT_PAGE: true combined with PUBLISHED: false with
HTTP 400 \"The front page cannot be unpublished\", which also cascades
into skipped module items linking the page.  LOC is a
\(:file :line :heading) plist."
  (let ((front-page (org-entry-get (point) "FRONT_PAGE"))
        (published (org-entry-get (point) "PUBLISHED")))
    (append
     (when (and front-page (string= (downcase front-page) "true")
                published (string= (downcase published) "false"))
       (list (org-canvas--validate-make-issue
              'error loc "FRONT_PAGE"
              "FRONT_PAGE: true requires PUBLISHED: true — Canvas rejects an unpublished front page (a published page in an unpublished course is still invisible to students)")))
     (org-canvas--validate-page-schedule loc front-page published))))

(defun org-canvas--validate-page-schedule (loc front-page published)
  "Warn about what a page's PUBLISH_AT overrides at point (issue #379).
Canvas publishes a scheduled page at PUBLISH_AT whatever PUBLISHED
says, and the push never sends PUBLISHED beside it, so PUBLISHED:
false there keeps nothing hidden.  A future PUBLISH_AT unpublishes the
page until then, which the course home page cannot be.  FRONT-PAGE and
PUBLISHED are the heading's values; LOC is a \(:file :line :heading)
plist.  Push-only: both protect what the next push sends."
  (when (org-canvas--validate-page-scheduled-p)
    (delq nil
          (list
           (when (and published (string= (downcase published) "false"))
             (org-canvas--validate-push-only
              (org-canvas--validate-make-issue
               'warning loc "PUBLISHED"
               "PUBLISHED: false is not sent beside PUBLISH_AT — Canvas publishes the page at PUBLISH_AT; remove PUBLISH_AT to keep it unpublished")))
           (when (and front-page (string= (downcase front-page) "true"))
             (org-canvas--validate-push-only
              (org-canvas--validate-make-issue
               'warning loc "PUBLISH_AT"
               "PUBLISH_AT on the front page — a future time unpublishes the course home page until then")))))))

(defun org-canvas--validate-page-scheduled-p ()
  "Return non-nil when the heading at point carries a PUBLISH_AT."
  (let ((publish-at (org-entry-get (point) "PUBLISH_AT")))
    (and publish-at (not (string-empty-p (string-trim publish-at))))))

(defun org-canvas--validate-new-quiz-item-type (loc)
  "Warn when the New Quiz item at point has a TYPE a push refuses.
Such a type is in `org-canvas--new-quiz-pull-only-types': a pull
writes it for an item made in Canvas, so the enum check accepts it,
but the push cannot build the item and refuses it (issue #340).
Push-only, since a read-only course never makes that push.  LOC is a
\(:file :line :heading) plist."
  (let ((q-type (org-entry-get (point) "TYPE")))
    (when (member q-type org-canvas--new-quiz-pull-only-types)
      (list (org-canvas--validate-push-only
             (org-canvas--validate-make-issue
              'warning loc "TYPE"
              (format (concat "TYPE %s is not pushed: its image regions "
                              "cannot be built from Org, so a sync refuses "
                              "the item; edit it in Canvas")
                      q-type)))))))

(defconst org-canvas--validate-raw-html-tag-re
  (concat "</?"
          (regexp-opt '("p" "span" "div" "ul" "ol" "li" "br" "a" "strong"
                        "em" "b" "i" "u" "h1" "h2" "h3" "h4" "h5" "h6"
                        "table" "thead" "tbody" "tr" "td" "th" "img" "link"
                        "style" "font" "hr" "blockquote" "pre" "code"
                        "iframe" "sup" "sub")
                      t)
          "\\(?:[ \t][^>\n]*\\)?/?>")
  "Regexp matching an HTML tag written into Org text.
Element names are listed so an Org radio target or timestamp, which
also sit in angle brackets, is never taken for one.")

(defun org-canvas--validate-settings-raw-html (loc)
  "Warn when the syllabus above the first sub-heading is raw HTML.
A settings pull before issue #459 wrote the syllabus as Canvas sent
it, tags and all.  The push exports the syllabus as Org, where a tag
is plain text, so it would reach Canvas escaped and the syllabus page
would show its own markup.  A tag inside an export block or snippet
is meant to be HTML and is left alone.  Push-only.  LOC is a
\(:file :line :heading) plist.  Returns an issue or nil."
  (let* ((bounds (org-canvas--pull-entry-text-bounds))
         (case-fold-search t)
         (line nil))
    (save-excursion
      (goto-char (car bounds))
      (while (and (not line)
                  (re-search-forward org-canvas--validate-raw-html-tag-re
                                     (cdr bounds) t))
        ;; Read the start before parsing: the parser moves the match data.
        (let ((start (match-beginning 0)))
          (when (memq (org-element-type
                       (save-excursion (goto-char start) (org-element-context)))
                      '(paragraph table-cell))
            (setq line (line-number-at-pos start))))))
    (when line
      (org-canvas--validate-push-only
       (org-canvas--validate-make-issue
        'warning (plist-put (copy-sequence loc) :line line) "syllabus"
        "The syllabus holds raw HTML tags, which the push would send as escaped text; pull settings again to convert it to Org, or wrap the HTML in #+begin_export html")))))

(defun org-canvas--validate-settings-structure (loc)
  "Warn about a sub-heading of the course heading that is not `Navigation'.
The syllabus is the text above the first sub-heading and `** Navigation'
is the only sub-heading settings.org reads, so anything else under
the course heading reaches Canvas nowhere (issue #275).  Push-only:
it is text the push leaves behind, not a fault of the course.  A
syllabus holding raw HTML is flagged too
\(`org-canvas--validate-settings-raw-html').  LOC is a
\(:file :line :heading) plist."
  (let ((end (save-excursion (org-end-of-subtree t) (point)))
        (issues (delq nil (list (org-canvas--validate-settings-raw-html loc)))))
    (save-excursion
      (while (and (outline-next-heading) (< (point) end))
        (let ((heading (org-get-heading t t t t)))
          (unless (string= (string-trim (or heading "")) "Navigation")
            (push (org-canvas--validate-push-only
                   (org-canvas--validate-make-issue
                    'warning
                    (list :file (plist-get loc :file)
                          :line (line-number-at-pos)
                          :heading heading)
                    "heading"
                    (format "`%s' is not syllabus text and is not pushed: the syllabus is the text above the first sub-heading, and Navigation is the only sub-heading settings.org reads" heading)))
                  issues)))))
    (nreverse issues)))

(defun org-canvas--validate-drop-rules (loc)
  "Check that drop rules don't exceed assignment count for this group.
LOC is a (:file :line :heading) plist."
  (let ((drop-lowest (org-entry-get (point) "DROP_LOWEST"))
        (drop-highest (org-entry-get (point) "DROP_HIGHEST")))
    (when (or drop-lowest drop-highest)
      (let* ((group-name (plist-get loc :heading))
             (drop-low (if drop-lowest (string-to-number drop-lowest) 0))
             (drop-high (if drop-highest (string-to-number drop-highest) 0))
             (total-drops (+ drop-low drop-high))
             (assignments-file (and (boundp 'org-canvas-assignments-file)
                                    (expand-file-name
                                     (symbol-value 'org-canvas-assignments-file))))
             (assignment-count (org-canvas--count-assignments-in-group
                                group-name assignments-file)))
        (when (and (> total-drops 0) (>= total-drops assignment-count))
          (list (org-canvas--validate-make-issue
                 'warning loc "DROP_LOWEST"
                 (format "Drop rules (%d) >= assignment count (%d) for group '%s'"
                         total-drops assignment-count group-name))))))))

(defun org-canvas--validate-module-item-ids (file)
  "Warn when two module item headings in FILE claim the same CANVAS_ID.
An item's id belongs to one module.  A block copied under a second
module with its drawer intact claims an item the first module holds;
the sync copes — the copy is created fresh where it sits (issue #105)
— but two headings naming one id is never what was meant, and the
extra copy is what a move that should have been a cut looks like.
A heading Canvas deleted, or one under a module it deleted, claims
nothing: a push skips it (issue #402).
Returns a list of issues, one per duplicated id."
  (let ((seen (make-hash-table :test 'equal))
        (order nil)
        (issues nil))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let ((id (org-entry-get (point) "CANVAS_ID")))
             (when (and id (not (org-canvas--sync-deleted-stamp)))
               (unless (gethash id seen) (push id order))
               (push (cons (line-number-at-pos) (org-get-heading t t t t))
                     (gethash id seen)))))
         "LEVEL=2" 'file)))
    (dolist (id (nreverse order))
      (let ((places (reverse (gethash id seen))))
        (when (cdr places)
          (push (org-canvas--validate-make-issue
                 'warning
                 (list :file file :line (car (car places)) :heading (cdr (car places)))
                 "CANVAS_ID"
                 (format "Module item CANVAS_ID %s is claimed by %d headings (lines %s) — an item belongs to one module; drop the id on the copy so it is created fresh"
                         id (length places)
                         (mapconcat (lambda (p) (format "%d" (car p))) places ", ")))
                issues))))
    (nreverse issues)))

(defconst org-canvas--validate-weight-sum-tolerance 0.01
  "Slack allowed when checking that group weights sum to 100.
Weights are read as floats, so an exact comparison would reject
values like 33.33 + 33.33 + 33.34 that Canvas itself accepts.")

(defun org-canvas--validate-weight-sum (file)
  "Warn when the WEIGHT properties in FILE do not sum to 100.
Only meaningful when the course applies group weights, but the check is
offline by design (see issue #37): weights that sum to less than 100
silently inflate every grade, and more than 100 deflates them, with no
symptom until final grades come out.  Returns a list of issues.

Groups with no WEIGHT are not counted, nor are groups Canvas deleted
\(issue #402); a file with no weighted groups
at all produces no issue, since an unweighted course is a normal
configuration rather than a mistake."
  (let ((total 0)
        (count 0)
        (first-line nil))
    ;; Read FILE rather than whatever buffer happens to be current: the
    ;; hook is handed a path, so it should not depend on its caller
    ;; having visited it.
    (with-current-buffer (org-canvas--find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let ((weight (org-entry-get (point) "WEIGHT")))
             (when (and weight (not (org-canvas--sync-deleted-stamp)))
               (unless first-line (setq first-line (line-number-at-pos)))
               (setq count (1+ count))
               (setq total (+ total (string-to-number weight))))))
         "LEVEL=2+WEIGHT={.}" 'file)))
    (when (and (> count 0)
               (> (abs (- total 100)) org-canvas--validate-weight-sum-tolerance))
      (list (org-canvas--validate-make-issue
             'warning
             (list :file file :line (or first-line 1) :heading "Assignment Groups")
             "WEIGHT"
             (format "Group weights sum to %s, not 100 — every grade is %s if the course applies group weights"
                     (org-canvas--validate-format-weight total)
                     (if (< total 100) "inflated" "deflated")))))))

(defun org-canvas--validate-format-weight (weight)
  "Format WEIGHT without a trailing .0 for whole numbers."
  (if (= weight (floor weight))
      (format "%d" (floor weight))
    (format "%s" weight)))

(defun org-canvas--validate-override-section-ids (file heading)
  "Check that override table section links have CANVAS_IDs.
FILE and HEADING identify the location.  Point must be at the first data row."
  (let ((issues nil))
    (while (looking-at "^|\\([^-]\\)")
      (let* ((row-line (line-number-at-pos))
             (row-loc (list :file file :line row-line :heading heading))
             (line-text (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
             (fields (split-string line-text "|" t "[ \t]+")))
        (when (and fields (car fields))
          (let ((section-ref (string-trim (car fields))))
            (when (string-match "\\[\\[file:" section-ref)
              (let ((resolved (org-canvas--resolve-link-property
                               section-ref "CANVAS_ID" file)))
                (unless resolved
                  (push (org-canvas--validate-make-issue
                         'warning row-loc nil
                         (format "Override section link target has no CANVAS_ID (pull sections first)")
                         t)
                        issues)))))))
      (forward-line 1))
    (nreverse issues)))

;;;; 5c. Duplicate and Superseded Titles (issue #164)
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

;;;; 5d. Cross-Course Links (issue #172)

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

;;;; 5e. Body Fragments From an Older Pull (issue #391)

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

;;;; 5f. Body Links to Local Files a Push Cannot Resolve (issue #468)

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

;;;; 5g. Items No Module Holds (issue #413)

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

;;;;; Findings as Data (issue #531)

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
