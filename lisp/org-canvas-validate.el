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
;; (:severity error|warning :file PATH :line NUM :heading TITLE
;;  :property PROP :message MSG)

(defun org-canvas--validate-format-issue (issue)
  "Format ISSUE plist as a `compilation-mode' compatible string."
  (format "%s:%d: %s: %s"
          (plist-get issue :file)
          (plist-get issue :line)
          (symbol-name (plist-get issue :severity))
          (plist-get issue :message)))

(defun org-canvas--validate-make-issue (severity loc property message
                                                 &optional pending-sync)
  "Create a validation issue plist.
SEVERITY is `error' or `warning'.  LOC is a plist (:file F :line N :heading H).
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

(defun org-canvas--validate-check-timestamp (value property loc &optional pull-only)
  "Check that VALUE is parseable as an Org timestamp.
Warns if the timestamp is in the past, unless PULL-ONLY: a property
only a pull writes (when a reply was posted) is in the past by nature,
and one warning per reply told nobody anything.
PROPERTY names the property.  LOC is a (:file :line :heading) plist."
  (when value
    (condition-case nil
        (let* ((parsed (org-parse-time-string value))
               (encoded (encode-time parsed))
               (iso (format-time-string "%Y-%m-%dT%H:%M:%SZ" encoded t))
               (now (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
          (when (and (not pull-only) (string< iso now))
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

(defun org-canvas--validate-check-date-order (date-triples loc)
  "Check chronological ordering for DATE-TRIPLES.
Each triple is (PROP1 PROP2 PROP3) where values should be ordered.
LOC is a (:file :line :heading) plist.
Returns a list of warning issues."
  (let ((issues nil))
    (dolist (triple date-triples)
      (let* ((p1 (nth 0 triple))
             (p2 (nth 1 triple))
             (p3 (nth 2 triple))
             (v1 (org-entry-get (point) p1))
             (v2 (org-entry-get (point) p2))
             (v3 (org-entry-get (point) p3))
             (iso1 (org-canvas--validate-safe-parse-timestamp v1))
             (iso2 (org-canvas--validate-safe-parse-timestamp v2))
             (iso3 (org-canvas--validate-safe-parse-timestamp v3)))
        (when (and iso1 iso2 (string> iso1 iso2))
          (push (org-canvas--validate-make-issue
                 'warning loc p1
                 (format "%s is after %s (%s > %s)" p1 p2 v1 v2))
                issues))
        (when (and iso2 iso3 (string> iso2 iso3))
          (push (org-canvas--validate-make-issue
                 'warning loc p2
                 (format "%s is after %s (%s > %s)" p2 p3 v2 v3))
                issues))))
    (nreverse issues)))

;;;; 4. Validation Specs
;;
;; Enum constants are defined in org-canvas-core-config.el and inherited
;; via (require 'org-canvas-core).

(defun org-canvas--validate-specs ()
  "Return validation specs from the property registry.
This replaces the former `org-canvas--validate-specs' defconst."
  (org-canvas--get-validate-specs-from-registry))

;;;; 5. Structural Validators

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

(defun org-canvas--validate-section-structure (loc)
  "Warn if section has no CANVAS_ID (not yet pulled).
LOC is a (:file :line :heading) plist."
  (unless (org-entry-get (point) "CANVAS_ID")
    (list (org-canvas--validate-make-issue
           'warning loc "CANVAS_ID"
           "Section has no CANVAS_ID (run org-canvas-pull-sections first)"))))

(defun org-canvas--validate-override-rows (file heading)
  "Check each data row in an override table for valid section links.
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
            (when (and (not (string-empty-p section-ref))
                       (not (string-match "\\[\\[file:" section-ref)))
              (push (org-canvas--validate-make-issue
                     'warning row-loc nil
                     (format "Override row section '%s' is not a file link"
                             section-ref))
                    issues)))))
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

;;;; 5a-2. Grading Schemes
;;
;; The table helpers live in the grading-schemes module, which is always
;; loaded with org-canvas; validate names them rather than requiring a
;; feature module (the dependency rule in CLAUDE.md).

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
names it.  An id of 0 names no scheme and is passed over."
  (let ((known (with-current-buffer (org-canvas--find-file-noselect schemes-file)
                 (save-excursion
                   (delq nil (org-map-entries
                              (lambda () (org-entry-get (point) "CANVAS_ID"))
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

(cl-defun org-canvas--validate-assignment-structure (loc)
  "Check the LTI tool declaration, the grading period, and the override table.
LOC is a (:file :line :heading) plist."
  (let ((issues (nconc (org-canvas--validate-external-tool loc)
                       (org-canvas--validate-due-in-grading-period loc)))
        (end (save-excursion (org-end-of-subtree t) (point))))
    (save-excursion
      (unless (re-search-forward "^#\\+NAME: overrides" end t)
        (cl-return-from org-canvas--validate-assignment-structure issues))
      (forward-line 1)
      (unless (looking-at "^|")
        (cl-return-from org-canvas--validate-assignment-structure issues))
      (forward-line 1)
      (when (looking-at "^|-")
        (forward-line 1))
      (let* ((save-pos (point))
             (file (plist-get loc :file))
             (heading (plist-get loc :heading))
             (row-issues (org-canvas--validate-override-rows file heading)))
        (goto-char save-pos)
        (let ((id-issues (org-canvas--validate-override-section-ids file heading)))
          (nconc issues row-issues id-issues))))))

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

(cl-defun org-canvas--validate-module-item-link (loc)
  "Check that module item link at point resolves to a synced heading.
LOC is a (:file :line :heading) plist.
Skips items with EXTERNAL_URL property or plain-text SubHeaders."
  (when (org-entry-get (point) "EXTERNAL_URL")
    (cl-return-from org-canvas--validate-module-item-link nil))
  (let ((raw-heading (save-excursion
                       (org-back-to-heading t)
                       (looking-at org-complex-heading-regexp)
                       (match-string-no-properties 4))))
    (when (and raw-heading
               ;; Greedy `.+' for the heading group so a target heading that is
               ;; itself a file link (nested brackets) is captured whole;
               ;; backtracking lands on the last `][' boundary.  A `[^]]'-based
               ;; group truncates at the first nested `]' (false "missing
               ;; heading" on links to files.org headings).
               (string-match
                "\\[\\[file:\\([^]]+\\)::\\*\\(.+\\)\\]\\["
                raw-heading))
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
             abs-path clean-heading file-path loc)))))))

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

(defun org-canvas--validate-quiz-point-total (loc)
  "Check that quiz POINTS matches sum of question points.
LOC is a (:file :line :heading) plist."
  (when-let* ((declared-points (org-entry-get (point) "POINTS"))
              (sum (org-canvas--validate-quiz-sum-question-points))
              (declared (string-to-number declared-points)))
    (unless (= declared sum)
      (list (org-canvas--validate-make-issue
             'warning loc "POINTS"
             (format "Quiz POINTS is %s but question total is %s"
                     declared-points (number-to-string sum)))))))

(defun org-canvas--count-assignments-in-group (group-name assignments-file)
  "Count assignments in GROUP-NAME by scanning ASSIGNMENTS-FILE."
  (if (and assignments-file (file-exists-p assignments-file))
      (let ((count 0))
        (with-current-buffer (org-canvas--find-file-noselect assignments-file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (let ((group-prop (org-entry-get (point) "GROUP")))
                 (when (and group-prop
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
    (when (and front-page (string= (downcase front-page) "true")
               published (string= (downcase published) "false"))
      (list (org-canvas--validate-make-issue
             'error loc "FRONT_PAGE"
             "FRONT_PAGE: true requires PUBLISHED: true — Canvas rejects an unpublished front page (a published page in an unpublished course is still invisible to students)")))))

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
             (when id
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

Groups with no WEIGHT are not counted; a file with no weighted groups
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
             (when weight
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
sits under.  Each group is (NORMALIZED . ENTRIES), where an entry is a
plist (:title :line :due) in document order."
  (let ((groups nil))
    (dolist (marker (org-map-entries (lambda () (point-marker)) query 'file))
      (goto-char (marker-position marker))
      (let* ((title (org-get-heading t t t t))
             (norm (org-canvas--validate-normalize-title
                    (org-link-display-format (or title "")))))
        (when norm
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

;;;; 6. Validation Engine

(defun org-canvas--validate-check-canvas-owned (value property loc)
  "Warn when PROPERTY, which only Canvas may set, is typed before a sync.
VALUE is the property's text.  A `:canvas-owned' property is written
by a pull and never sent by a push, so on a heading that has no
CANVAS_ID yet it can only be a hope: the object it describes has to
be attached in the web UI once the assignment exists, and then
pulled.  On a stamped heading the value is what a pull wrote and
there is nothing to say.  Push-only, since it protects a create.  LOC
is a (:file :line :heading) plist (issue #184)."
  (when (and value (not (string-empty-p value))
             (not (org-entry-get (point) "CANVAS_ID"))
             (not (org-entry-get (point) "CANVAS_URL")))
    (org-canvas--validate-push-only
     (org-canvas--validate-make-issue
      'warning loc property
      (format (concat "%s is set by Canvas, never by a push: sync the "
                      "heading first, attach it in the web UI, then pull")
              property)))))

(defun org-canvas--validate-entry-properties (props loc)
  "Validate PROPS list for the heading at point.
LOC is a (:file :line :heading) plist.
A property's `:read-only-values' join its `:values' as accepted input,
because they are values a pull wrote down (issue #167).  A
`:canvas-owned' property is checked for being typed ahead of the
sync that could give it meaning (issue #184).
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
            (push owned issues)))))
    (nreverse issues)))

(defun org-canvas--validate-entry-at-marker (props date-order structural-fn file)
  "Validate the entry at point using PROPS, DATE-ORDER, and STRUCTURAL-FN.
FILE identifies the source file.  Returns a list of issues."
  (let* ((line (line-number-at-pos))
         (heading (org-get-heading t t t t))
         (loc (list :file file :line line :heading heading))
         (issues nil))
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
    issues))

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
                 "^\\(.+\\):\\([0-9]+\\): warning: " 1 2 nil 1)))
  (setq-local font-lock-defaults
              '(org-canvas-validate-mode-font-lock-keywords t)))

;;;; 8. Main Command

(defun org-canvas--validate-run-all-specs ()
  "Run all validation specs and collect issues.
The cross-course link scan runs once per distinct file rather than
once per spec: two features can register the same file (modules and
module items both name modules.org), and the scan reads the whole file
either way, so a per-spec call would report every foreign link twice
\(issue #172).
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
                             (org-canvas--validate-cross-course-links file)))))
          (setq files-skipped (1+ files-skipped)))))
    (list :issues all-issues :checked files-checked :skipped files-skipped)))

(defun org-canvas--validate-format-summary (error-count warning-count)
  "Return a summary message string for ERROR-COUNT and WARNING-COUNT."
  (cond
   ((> error-count 0)
    (format "Validation: %d error(s), %d warning(s)" error-count warning-count))
   ((> warning-count 0)
    (format "Validation: %d warning(s), no errors" warning-count))
   (t
    "Validation passed: no issues found")))

(defun org-canvas--validate-insert-report (listed pending verbose stats)
  "Insert the validation report into the current buffer.
LISTED are the issues printed individually, PENDING the pre-first-sync
link warnings, collapsed into one line unless VERBOSE.  STATS is a
plist (:errors :warnings :checked :skipped :suppressed); a non-zero
:suppressed count is named in a line of its own, so that holding push-only
findings back on a read-only course is visible rather than silent.
Cross-course links are listed individually and then grouped by the
shell they point at, which is the question worth answering about
course-copy residue (issue #172)."
  (insert "org-canvas validation report\n")
  (insert (make-string 60 ?=))
  (insert "\n\n")
  (if (or listed pending)
      (progn
        (dolist (issue listed)
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
  (insert "\n")
  (insert (make-string 60 ?=))
  (insert "\n")
  (insert (format "Validation complete: %d error(s), %d warning(s) across %d file(s)"
                  (plist-get stats :errors) (plist-get stats :warnings)
                  (plist-get stats :checked)))
  (when pending
    (insert (format " (%d pending first sync)" (length pending))))
  (when (> (plist-get stats :skipped) 0)
    (insert (format " (%d file(s) not found, skipped)" (plist-get stats :skipped))))
  (insert "\n"))

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
  (let* ((result (org-canvas--validate-run-all-specs))
         (found (plist-get result :issues))
         (suppress (and org-canvas-read-only (not all)))
         (all-issues (if suppress
                         (cl-remove-if (lambda (i) (plist-get i :push-only)) found)
                       found))
         (suppressed-count (- (length found) (length all-issues)))
         (pending-issues (cl-remove-if-not
                          (lambda (i) (plist-get i :pending-sync)) all-issues))
         (listed-issues (cl-remove-if
                         (lambda (i) (plist-get i :pending-sync)) all-issues))
         (error-count (cl-count 'error all-issues :key (lambda (i) (plist-get i :severity))))
         (warning-count (cl-count 'warning all-issues :key (lambda (i) (plist-get i :severity))))
         (stats (list :errors error-count :warnings warning-count
                      :checked (plist-get result :checked)
                      :skipped (plist-get result :skipped)
                      :suppressed suppressed-count)))
    (org-canvas--report-display
     "*canvas-validate*"
     (lambda ()
       (org-canvas--validate-insert-report
        listed-issues pending-issues verbose stats))
     #'org-canvas-validate-mode)
    (message "%s" (org-canvas--validate-format-summary error-count warning-count))
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
