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
PROPERTY names the property.  LOC is a (:file :line :heading) plist."
  (when (and value (not (member value valid-values)))
    (org-canvas--validate-make-issue
     'error loc property
     (format "%s: '%s' is not valid (expected: %s)"
             property value (string-join valid-values ", ")))))

(defun org-canvas--validate-check-csv-enum (value property valid-values loc)
  "Check that each comma-separated part of VALUE is in VALID-VALUES.
PROPERTY names the property.  LOC is a (:file :line :heading) plist."
  (when value
    (let ((parts (split-string value "," t "[ \t]+")))
      (let ((bad (cl-remove-if (lambda (p) (member p valid-values)) parts)))
        (when bad
          (org-canvas--validate-make-issue
           'error loc property
           (format "%s: invalid value(s) '%s' (expected: %s)"
                   property (string-join bad ", ")
                   (string-join valid-values ", "))))))))

(defun org-canvas--validate-check-timestamp (value property loc)
  "Check that VALUE is parseable as an Org timestamp.
Warns if the timestamp is in the past.
PROPERTY names the property.  LOC is a (:file :line :heading) plist."
  (when value
    (condition-case nil
        (let* ((parsed (org-parse-time-string value))
               (encoded (encode-time parsed))
               (iso (format-time-string "%Y-%m-%dT%H:%M:%SZ" encoded t))
               (now (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
          (when (string< iso now)
            (org-canvas--validate-make-issue
             'warning loc property
             (format "%s: timestamp %s is in the past" property value))))
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
    (org-canvas--validate-make-issue 'warning loc property unresolved-msg t))))

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
      (let ((row (list "" "" "" outcome)))
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

(cl-defun org-canvas--validate-assignment-structure (loc)
  "Check override table in assignment if present.
LOC is a (:file :line :heading) plist."
  (let ((end (save-excursion (org-end-of-subtree t) (point))))
    (save-excursion
      (unless (re-search-forward "^#\\+NAME: overrides" end t)
        (cl-return-from org-canvas--validate-assignment-structure nil))
      (forward-line 1)
      (unless (looking-at "^|")
        (cl-return-from org-canvas--validate-assignment-structure nil))
      (forward-line 1)
      (when (looking-at "^|-")
        (forward-line 1))
      (let* ((save-pos (point))
             (file (plist-get loc :file))
             (heading (plist-get loc :heading))
             (row-issues (org-canvas--validate-override-rows file heading)))
        (goto-char save-pos)
        (let ((id-issues (org-canvas--validate-override-section-ids file heading)))
          (nconc row-issues id-issues))))))

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
      (with-current-buffer (find-file-noselect abs-path)
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
        (with-current-buffer (find-file-noselect assignments-file)
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
    (with-current-buffer (find-file-noselect file)
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

;;;; 6. Validation Engine

(defun org-canvas--validate-entry-properties (props loc)
  "Validate PROPS list for the heading at point.
LOC is a (:file :line :heading) plist.
Returns a list of issues."
  (let ((issues nil))
    (dolist (prop props)
      (let* ((name (plist-get prop :name))
             (type (plist-get prop :type))
             (values (plist-get prop :values))
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
                 (org-canvas--validate-check-timestamp value name loc))
                ('link
                 (org-canvas--validate-check-link value name target-file id-prop loc)))))
        (when issue
          (push issue issues))))
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
the assignment-group weights sum to 100."
  (let* ((file-var (plist-get spec :file))
         (query (plist-get spec :query))
         (props (plist-get spec :properties))
         (date-order (plist-get spec :date-order))
         (structural-fn (plist-get spec :structural-fn))
         (file-fn (plist-get spec :file-fn))
         (file (and (boundp file-var)
                    (expand-file-name (symbol-value file-var))))
         (issues nil))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (let ((markers (org-map-entries (lambda () (point-marker)) query 'file)))
            (dolist (marker markers)
              (goto-char (marker-position marker))
              (setq issues (nconc issues
                                  (org-canvas--validate-entry-at-marker
                                   props date-order structural-fn file))))
            (dolist (m markers) (set-marker m nil)))))
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
Returns a plist (:issues ISSUES :checked N :skipped N)."
  (let ((all-issues nil)
        (files-checked 0)
        (files-skipped 0))
    (dolist (spec (org-canvas--validate-specs))
      (let* ((file-var (plist-get spec :file))
             (file (and (boundp file-var)
                        (expand-file-name (symbol-value file-var)))))
        (if (and file (file-exists-p file))
            (progn
              (setq files-checked (1+ files-checked))
              (let ((issues (org-canvas--validate-spec spec)))
                (setq all-issues (nconc all-issues issues))))
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

;;;###autoload
(defun org-canvas-validate (&optional verbose)
  "Validate all course org files without contacting the Canvas API.
Checks property types, enum values, date ordering, and structural
requirements across all 12 content types.

Results are displayed in a `*canvas-validate*' buffer with
`compilation-mode' navigation (\\[next-error] / \\[previous-error]).

Warnings about link targets that merely lack a CANVAS_ID (expected
state before the first sync) are collapsed into a single summary
line.  With a prefix argument VERBOSE, list them individually."
  (interactive "P")
  (let* ((result (org-canvas--validate-run-all-specs))
         (all-issues (plist-get result :issues))
         (pending-issues (cl-remove-if-not
                          (lambda (i) (plist-get i :pending-sync)) all-issues))
         (listed-issues (cl-remove-if
                         (lambda (i) (plist-get i :pending-sync)) all-issues))
         (pending-count (length pending-issues))
         (files-checked (plist-get result :checked))
         (files-skipped (plist-get result :skipped))
         (buf (get-buffer-create "*canvas-validate*"))
         (error-count (cl-count 'error all-issues :key (lambda (i) (plist-get i :severity))))
         (warning-count (cl-count 'warning all-issues :key (lambda (i) (plist-get i :severity)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "org-canvas validation report\n"))
        (insert (make-string 60 ?=))
        (insert "\n\n")
        (if all-issues
            (progn
              (dolist (issue listed-issues)
                (insert (org-canvas--validate-format-issue issue))
                (insert "\n"))
              (when (> pending-count 0)
                (if verbose
                    (dolist (issue pending-issues)
                      (insert (org-canvas--validate-format-issue issue))
                      (insert "\n"))
                  (insert (format "%d link(s) pending first sync (targets have no CANVAS_ID yet); C-u M-x org-canvas-validate lists them\n"
                                  pending-count)))))
          (insert "No issues found.\n"))
        (insert "\n")
        (insert (make-string 60 ?=))
        (insert "\n")
        (insert (format "Validation complete: %d error(s), %d warning(s) across %d file(s)"
                        error-count warning-count files-checked))
        (when (> pending-count 0)
          (insert (format " (%d pending first sync)" pending-count)))
        (when (> files-skipped 0)
          (insert (format " (%d file(s) not found, skipped)" files-skipped)))
        (insert "\n"))
      (org-canvas-validate-mode)
      (goto-char (point-min)))
    (display-buffer buf)
    (message "%s" (org-canvas--validate-format-summary error-count warning-count))))

(provide 'org-canvas-validate)
;;; org-canvas-validate.el ends here
