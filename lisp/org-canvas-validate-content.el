;;; org-canvas-validate-content.el --- Content checks for org-canvas  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Structural checks on what a course puts on Canvas: discussions,
;; rubrics, files, sections, pages, quizzes, settings, modules and
;; messages.  Each returns issue plists built by
;; `org-canvas-validate-issue'.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-validate-issue)
(require 'cl-lib)

;;;; 1. Structural Validators

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
       outcome-cell
       :id-property "CANVAS_ID"
       :source-file file
       :loc loc
       :property nil
       :not-link-msg
       (format "Rubric criterion '%s' outcome is not a file link" criterion)
       :unresolved-msg
       (format "Rubric criterion '%s' outcome link has no CANVAS_ID (sync outcomes first)"
               criterion)
       :not-link-severity 'warning))))

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

;;;; 2. Messages (issue #232)

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

;;;; 3. Module Structure

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

;;;; 4. Quizzes and Accommodations

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

;;;; 5. Pages and Schedules

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

;;;; 6. Settings

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

;;;; 7. Module Item Ids

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

(provide 'org-canvas-validate-content)
;;; org-canvas-validate-content.el ends here
