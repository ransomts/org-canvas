;;; org-canvas-validate.el --- Validation engine for org-canvas  -*- lexical-binding: t; -*-

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

(defun org-canvas--validate-make-issue (severity loc property message)
  "Create a validation issue plist.
SEVERITY is `error' or `warning'.  LOC is a plist (:file F :line N :heading H).
PROPERTY and MESSAGE describe the problem."
  (list :severity severity
        :file (plist-get loc :file)
        :line (plist-get loc :line)
        :heading (plist-get loc :heading)
        :property property :message message))

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
    (org-canvas--validate-make-issue 'warning loc property unresolved-msg))))

(defun org-canvas--validate-check-link (value property target-file-var id-property loc)
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

(defconst org-canvas--validate-specs
  `((:label "Assignments"
     :file org-canvas-assignments-file
     :query "LEVEL=1"
     :properties
     ((:name "POINTS" :type number)
      (:name "GRADING_TYPE" :type enum :values ,org-canvas--valid-grading-types)
      (:name "PUBLISHED" :type boolean)
      (:name "SUBMISSION" :type csv-enum :values ,org-canvas--valid-submission-types)
      (:name "MAX_ATTEMPTS" :type number)
      (:name "DUE_AT" :type timestamp)
      (:name "UNLOCK_AT" :type timestamp)
      (:name "LOCK_AT" :type timestamp)
      (:name "PEER_REVIEWS" :type boolean)
      (:name "PEER_REVIEW_COUNT" :type number)
      (:name "PEER_REVIEW_DUE_AT" :type timestamp)
      (:name "GROUP" :type link :target-file org-canvas-assignment-groups-file :id-property "CANVAS_ID")
      (:name "RUBRIC_LINK" :type link :target-file org-canvas-rubrics-file :id-property "CANVAS_ID")
      (:name "OMIT_FROM_GRADES" :type boolean)
      (:name "ANONYMOUS_GRADING" :type boolean)
      (:name "NOTIFY_OF_UPDATE" :type boolean)
      (:name "AUTOMATIC_PEER_REVIEWS" :type boolean)
      (:name "GRADE_INDIVIDUALLY" :type boolean)
      (:name "ONLY_VISIBLE_TO_OVERRIDES" :type boolean)
      (:name "MODERATED_GRADING" :type boolean)
      (:name "GRADER_COUNT" :type number)
      (:name "MUTED" :type boolean)
      (:name "TURNITIN_ENABLED" :type boolean)
      (:name "GRADING_STANDARD_ID" :type number)
      (:name "GROUP_CATEGORY_ID" :type number)
      (:name "POSITION" :type number))
     :date-order (("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
     :structural-fn org-canvas--validate-assignment-structure)

    (:label "Pages"
     :file org-canvas-pages-file
     :query "LEVEL=1"
     :properties
     ((:name "PUBLISHED" :type boolean)
      (:name "FRONT_PAGE" :type boolean)
      (:name "EDITING_ROLES" :type csv-enum :values ,org-canvas--valid-editing-roles)
      (:name "TODO_DATE" :type timestamp)
      (:name "NOTIFY_OF_UPDATE" :type boolean)))

    (:label "Quizzes"
     :file org-canvas-quizzes-file
     :query "LEVEL=1"
     :properties
     ((:name "QUIZ_TYPE" :type enum :values ,org-canvas--valid-quiz-types)
      (:name "PUBLISHED" :type boolean)
      (:name "SHUFFLE_ANSWERS" :type boolean)
      (:name "TIME_LIMIT" :type number)
      (:name "ALLOWED_ATTEMPTS" :type number)
      (:name "DUE_AT" :type timestamp)
      (:name "UNLOCK_AT" :type timestamp)
      (:name "LOCK_AT" :type timestamp)
      (:name "SHOW_CORRECT_ANSWERS" :type boolean)
      (:name "SHOW_CORRECT_ANSWERS_AT" :type timestamp)
      (:name "HIDE_CORRECT_ANSWERS_AT" :type timestamp)
      (:name "HIDE_RESULTS" :type enum :values ,org-canvas--valid-hide-results)
      (:name "SCORING_POLICY" :type enum :values ,org-canvas--valid-scoring-policies)
      (:name "ONE_QUESTION_AT_A_TIME" :type boolean)
      (:name "CANT_GO_BACK" :type boolean)
      (:name "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT" :type boolean)
      (:name "ONE_TIME_RESULTS" :type boolean)
      (:name "ONLY_VISIBLE_TO_OVERRIDES" :type boolean)
      (:name "GROUP" :type link :target-file org-canvas-assignment-groups-file :id-property "CANVAS_ID"))
     :date-order (("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
     :structural-fn org-canvas--validate-quiz-point-total)

    (:label "Quiz Questions"
     :file org-canvas-quizzes-file
     :query "LEVEL=2"
     :properties
     ((:name "TYPE" :type enum :values ,org-canvas--valid-question-types)
      (:name "POINTS" :type number)
      (:name "PICK_COUNT" :type number)
      (:name "QUESTION_POINTS" :type number)
      (:name "QUESTION_BANK_ID" :type number)))

    (:label "Modules"
     :file org-canvas-modules-file
     :query "LEVEL=1"
     :properties
     ((:name "PUBLISHED" :type boolean)
      (:name "UNLOCK_AT" :type timestamp)
      (:name "REQUIRE_SEQUENTIAL_PROGRESSION" :type boolean)
      (:name "PUBLISH_FINAL_GRADE" :type boolean)))

    (:label "Module Items"
     :file org-canvas-modules-file
     :query "LEVEL=2"
     :properties
     ((:name "INDENT" :type number)
      (:name "COMPLETION_REQUIREMENT" :type enum :values ,org-canvas--valid-completion-requirements)
      (:name "MIN_SCORE" :type number)
      (:name "NEW_TAB" :type boolean)
      (:name "PUBLISHED" :type boolean))
     :structural-fn org-canvas--validate-module-item-link)

    (:label "Rubrics"
     :file org-canvas-rubrics-file
     :query "LEVEL=1"
     :properties
     ((:name "FREE_FORM_CRITERION_COMMENTS" :type boolean))
     :structural-fn org-canvas--validate-rubric-structure)

    (:label "Outcome Groups"
     :file org-canvas-outcomes-file
     :query "LEVEL=1"
     :properties ())

    (:label "Outcomes"
     :file org-canvas-outcomes-file
     :query "LEVEL=2"
     :properties
     ((:name "CALCULATION_METHOD" :type enum :values ,org-canvas--valid-calculation-methods)
      (:name "CALCULATION_INT" :type number)
      (:name "MASTERY_POINTS" :type number)))

    (:label "Discussions"
     :file org-canvas-discussions-file
     :query "LEVEL=1"
     :properties
     ((:name "PUBLISHED" :type boolean)
      (:name "DISCUSSION_TYPE" :type enum :values ,org-canvas--valid-discussion-types)
      (:name "GRADING_TYPE" :type enum :values ,org-canvas--valid-grading-types)
      (:name "POINTS" :type number)
      (:name "POST_FIRST" :type boolean)
      (:name "PINNED" :type boolean)
      (:name "AVAILABLE_FROM" :type timestamp)
      (:name "DUE_AT" :type timestamp)
      (:name "LOCK_AT" :type timestamp)
      (:name "ALLOW_RATING" :type boolean)
      (:name "ONLY_GRADERS_CAN_RATE" :type boolean)
      (:name "SORT_BY_RATING" :type boolean)
      (:name "GROUP_CATEGORY" :type number)
      (:name "GROUP" :type link :target-file org-canvas-assignment-groups-file :id-property "CANVAS_ID")
      (:name "RUBRIC_LINK" :type link :target-file org-canvas-rubrics-file :id-property "CANVAS_ID"))
     :date-order (("AVAILABLE_FROM" "DUE_AT" "LOCK_AT")))

    (:label "Announcements"
     :file org-canvas-announcements-file
     :query "LEVEL=1"
     :properties
     ((:name "PUBLISHED" :type boolean)
      (:name "DELAYED_POST_AT" :type timestamp)
      (:name "ALLOW_COMMENTS" :type boolean)))

    (:label "Files"
     :file org-canvas-files-file
     :query "LEVEL>0"
     :properties
     ((:name "PUBLISHED" :type boolean)
      (:name "UNLOCK_AT" :type timestamp)
      (:name "LOCK_AT" :type timestamp)
      (:name "USE_JUSTIFICATION" :type enum :values ,org-canvas--valid-use-justifications))
     :structural-fn org-canvas--validate-file-structure)

    (:label "Assignment Groups"
     :file org-canvas-assignment-groups-file
     :query "LEVEL=1"
     :properties
     ((:name "WEIGHT" :type number)
      (:name "DROP_LOWEST" :type number)
      (:name "DROP_HIGHEST" :type number)
      (:name "POSITION" :type number))
     :structural-fn org-canvas--validate-drop-rules)

    (:label "New Quizzes"
     :file org-canvas-new-quizzes-file
     :query "LEVEL=1"
     :properties
     ((:name "TIME_LIMIT" :type number)
      (:name "SHUFFLE_ANSWERS" :type boolean)
      (:name "ONE_AT_A_TIME" :type boolean)
      (:name "ALLOWED_ATTEMPTS" :type number)
      (:name "SCORING_POLICY" :type enum :values ,org-canvas--valid-new-quiz-scoring-policies)
      (:name "GROUP" :type link :target-file org-canvas-assignment-groups-file :id-property "CANVAS_ID")
      (:name "RUBRIC_LINK" :type link :target-file org-canvas-rubrics-file :id-property "CANVAS_ID")))

    (:label "New Quiz Items"
     :file org-canvas-new-quizzes-file
     :query "LEVEL=2"
     :properties
     ((:name "POINTS" :type number)
      (:name "TYPE" :type enum :values ,org-canvas--valid-new-quiz-types)
      (:name "OUTCOME" :type link :target-file org-canvas-outcomes-file :id-property "CANVAS_ID")))

    (:label "Settings"
     :file org-canvas-settings-file
     :query "LEVEL=1"
     :properties
     ((:name "APPLY_WEIGHTS" :type boolean)
      (:name "HIDE_FINAL_GRADES" :type boolean)
      (:name "PUBLIC_SYLLABUS" :type boolean)
      (:name "IS_PUBLIC" :type boolean)
      (:name "DEFAULT_VIEW" :type enum :values ,org-canvas--valid-views)
      (:name "LICENSE" :type enum :values ,org-canvas--valid-licenses)
      (:name "START_AT" :type timestamp)
      (:name "END_AT" :type timestamp)
      (:name "ALLOW_STUDENT_DISCUSSION_TOPICS" :type boolean)
      (:name "ALLOW_STUDENT_DISCUSSION_EDITING" :type boolean)
      (:name "ALLOW_STUDENT_FORUM_ATTACHMENTS" :type boolean)
      (:name "LOCK_ALL_ANNOUNCEMENTS" :type boolean)
      (:name "RESTRICT_STUDENT_FUTURE_VIEW" :type boolean)
      (:name "RESTRICT_STUDENT_PAST_VIEW" :type boolean)
      (:name "SHOW_ANNOUNCEMENTS_ON_HOME_PAGE" :type boolean)
      (:name "HOME_PAGE_ANNOUNCEMENT_LIMIT" :type number)
      (:name "HIDE_DISTRIBUTION_GRAPHS" :type boolean)
      (:name "GRADING_STANDARD_ID" :type number)
      (:name "LATE_SUBMISSION_DEDUCTION" :type number)
      (:name "LATE_SUBMISSION_DEDUCTION_ENABLED" :type boolean)
      (:name "LATE_SUBMISSION_INTERVAL" :type enum :values ,org-canvas--valid-late-intervals)
      (:name "LATE_SUBMISSION_MINIMUM_PERCENT" :type number)
      (:name "LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED" :type boolean)
      (:name "MISSING_SUBMISSION_DEDUCTION" :type number)
      (:name "MISSING_SUBMISSION_DEDUCTION_ENABLED" :type boolean)))

    (:label "Sections"
     :file org-canvas-sections-file
     :query "LEVEL=1"
     :properties ()
     :structural-fn org-canvas--validate-section-structure)

    (:label "Group Categories"
     :file org-canvas-group-categories-file
     :query "LEVEL=1"
     :properties
     ((:name "SELF_SIGNUP" :type enum :values ,org-canvas--valid-self-signup-values)
      (:name "GROUP_LIMIT" :type number)
      (:name "AUTO_LEADER" :type enum :values ,org-canvas--valid-auto-leader-values)
      (:name "CREATE_GROUP_COUNT" :type number)))

    (:label "Calendar Events"
     :file org-canvas-calendar-events-file
     :query "LEVEL=1"
     :properties
     ((:name "START_AT" :type timestamp)
      (:name "END_AT" :type timestamp)
      (:name "ALL_DAY" :type boolean))))
  "Validation specs for all org-canvas content types.")

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
Returns a list of issues."
  (let ((issues nil))
    (dolist (row table-data)
      (unless (or (eq row 'hline)
                  (and (listp row) (stringp (nth 0 row))
                       (string-match-p "\\`> " (string-trim-left (nth 0 row)))))
        (let ((issue (org-canvas--validate-single-outcome-link row file loc)))
          (when issue (push issue issues)))))
    (nreverse issues)))

(defun org-canvas--validate-rubric-structure (loc)
  "Check that the rubric heading at point has an org-table.
Also validates outcome links in the optional 4th column.
LOC is a (:file :line :heading) plist."
  (let ((end (save-excursion (org-end-of-subtree t) (point)))
        (issues nil))
    (save-excursion
      (if (not (re-search-forward "^|" end t))
          (push (org-canvas--validate-make-issue
                 'error loc nil
                 "Rubric has no criteria table (expected an org-table under heading)")
                issues)
        ;; Table exists — validate outcome links in 4th column
        (beginning-of-line)
        (let ((table-data (org-table-to-lisp)))
          (when table-data
            (let ((outcome-issues (org-canvas--validate-rubric-outcome-links
                                   table-data (plist-get loc :file) loc)))
              (setq issues (nconc issues outcome-issues)))))))
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
                         clean-heading)))))))))

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
    (when (and raw-heading (string-match "\\[\\[file:\\([^]]+\\)::\\*\\(.+?\\)\\]" raw-heading))
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

(defun org-canvas--validate-quiz-point-total (loc)
  "Check that quiz POINTS matches sum of question points.
LOC is a (:file :line :heading) plist."
  (let ((declared-points (org-entry-get (point) "POINTS"))
        (end (save-excursion (org-end-of-subtree t) (point)))
        (markers nil))
    (when declared-points
      (save-excursion
        (while (re-search-forward "^\\*\\* " end t)
          (push (point-marker) markers)))
      (when markers
        (let ((sum (cl-loop for m in (nreverse markers)
                            sum (org-canvas--validate-quiz-question-points
                                 (marker-position m))))
              (declared (string-to-number declared-points)))
          (unless (= declared sum)
            (list (org-canvas--validate-make-issue
                   'warning loc "POINTS"
                   (format "Quiz POINTS is %s but question total is %s"
                           declared-points (number-to-string sum))))))))))

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
                         (format "Override section link target has no CANVAS_ID (pull sections first)"))
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
Returns a list of issues."
  (let* ((file-var (plist-get spec :file))
         (query (plist-get spec :query))
         (props (plist-get spec :properties))
         (date-order (plist-get spec :date-order))
         (structural-fn (plist-get spec :structural-fn))
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
                                   props date-order structural-fn file))))))))
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
    (dolist (spec org-canvas--validate-specs)
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
(defun org-canvas-validate ()
  "Validate all course org files without contacting the Canvas API.
Checks property types, enum values, date ordering, and structural
requirements across all 12 content types.

Results are displayed in a `*canvas-validate*' buffer with
`compilation-mode' navigation (\\[next-error] / \\[previous-error])."
  (interactive)
  (let* ((result (org-canvas--validate-run-all-specs))
         (all-issues (plist-get result :issues))
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
            (dolist (issue all-issues)
              (insert (org-canvas--validate-format-issue issue))
              (insert "\n"))
          (insert "No issues found.\n"))
        (insert "\n")
        (insert (make-string 60 ?=))
        (insert "\n")
        (insert (format "Validation complete: %d error(s), %d warning(s) across %d file(s)"
                        error-count warning-count files-checked))
        (when (> files-skipped 0)
          (insert (format " (%d file(s) not found, skipped)" files-skipped)))
        (insert "\n"))
      (org-canvas-validate-mode)
      (goto-char (point-min)))
    (display-buffer buf)
    (message "%s" (org-canvas--validate-format-summary error-count warning-count))))

(provide 'org-canvas-validate)
;;; org-canvas-validate.el ends here
