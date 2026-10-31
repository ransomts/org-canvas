;;; org-canvas-heading-verify.el --- Read pushed assignments, rubrics, modules and files back -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-sync-headings' pushes an assignment, a module and files
;; by heading in one call and reports each `synced', which is what
;; Canvas answered, not what it now holds.  A module sync creates,
;; updates and reorders children, and a module item can publish the
;; file it points at; a re-upload gives a file a new id, so a check by
;; the old id checks nothing.  A rubric heading, and the rubric an
;; assignment links, had no read-back either (issue #567).
;; Confirming such a push took a script of raw GETs read by eye
;; (issue #524).
;;
;; `org-canvas-verify-headings' (`scripts/org-canvas verify
;; modules:TITLE files:TITLE assignments:TITLE rubrics:TITLE') is that
;; read-back, in the shape of the announcement verify (#502).  Each
;; named heading's object is read on its own, by the CANVAS_ID the
;; heading carries now, through the feature registry's item URL and
;; parameters (Hard Rule 13), and compared:
;;
;;   - assignment: the title, PUBLISHED, POINTS (when written), and
;;     DUE_AT, UNLOCK_AT and LOCK_AT.  The read carries the feature's
;;     `override_assignment_dates=false', so the base dates are what is
;;     compared, never a student's extension (#273); the rubric the
;;     heading's RUBRIC_LINK names is compared with the one Canvas
;;     attaches, criterion by criterion (issue #567);
;;   - module: the title, PUBLISHED, and its items: the module's item
;;     list is read once and each child heading, parsed as the module
;;     sync parses it, is checked for its position among the items the
;;     push sends, its title, and its publish state when the push sends
;;     one.  An item Canvas holds that no child names is a difference;
;;   - file: PUBLISHED (against `locked'), HIDDEN (false when absent, as
;;     the push sends it), UNLOCK_AT and LOCK_AT;
;;   - rubric: the title, CANVAS_ID, criterion count and total points,
;;     then each criterion's description and points.
;;
;; A date the heading leaves out wants none on Canvas: that is what a
;; student would not expect to see.  Nothing is written, locally or on
;; Canvas.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org-canvas-core)
;; A command file above the feature modules: it reads their registry
;; entries, compares fields the drift report's way, parses module items
;; as the module sync does, and reports in the announcement verify's
;; rows.
(require 'org-canvas-diff)
(require 'org-canvas-diff-fields)
(require 'org-canvas-announcements-verify)
(require 'org-canvas-assignments)
(require 'org-canvas-modules)
(require 'org-canvas-files)
;; The rubric a heading links, for the rubric rows (issue #567).
(require 'org-canvas-rubrics)

(defconst org-canvas--heading-verify-buffer "*canvas-verify-headings*"
  "Buffer the heading verification report is rendered into.")

(defconst org-canvas--heading-verify-features
  '(("assignment" "Assignments" org-canvas--heading-verify-assignment-rows)
    ("module" "Modules" org-canvas--heading-verify-module-rows)
    ("file" "Files" org-canvas--heading-verify-file-rows)
    ("rubric" "Rubrics" org-canvas--heading-verify-rubric-rows))
  "The features this command can verify, as (SINGULAR NAME ROWS-FN).
NAME is the registry name; ROWS-FN is called with the feature entry,
its property registration, the heading's entry and the Canvas item,
and returns the report rows.")

(defconst org-canvas--heading-verify-assignment-props
  '(("PUBLISHED" . default) ("POINTS" . written)
    ("DUE_AT" . none) ("UNLOCK_AT" . none) ("LOCK_AT" . none))
  "The assignment properties compared, as (PROP . RULE).
See `org-canvas--heading-verify-property' for the rules.")

(defconst org-canvas--heading-verify-file-props
  '(("PUBLISHED" . default) ("HIDDEN" . "false")
    ("UNLOCK_AT" . none) ("LOCK_AT" . none))
  "The file properties compared, as (PROP . RULE).
HIDDEN absent is sent as false, so false is what it stands for.")

;;;; Properties

(defun org-canvas--heading-verify-kind (name)
  "Return the verified feature NAME means, or nil.
The entry of `org-canvas--heading-verify-features'."
  (assoc (org-canvas--singularize (org-canvas--diff-normalize-name (format "%s" name)))
         org-canvas--heading-verify-features))

(defun org-canvas--heading-verify-spec (props prop)
  "Return the spec of PROP in PROPS, a property registration."
  (seq-find (lambda (s) (equal (plist-get s :org-prop) prop))
            (plist-get props :properties)))

(defun org-canvas--heading-verify-written (pom prop)
  "Return PROP as written at POM, trimmed, or nil when absent or empty."
  (let ((value (org-entry-get pom prop)))
    (and value (not (string-empty-p (string-trim value))) (string-trim value))))

(defun org-canvas--heading-verify-expected (spec written rule)
  "Return the Org value SPEC's property stands for: WRITTEN, or RULE's.
RULE `default' is the spec's declared default, a string is itself,
and anything else is nil, no value."
  (cond (written written)
        ((eq rule 'default) (org-canvas--diff-declared-default spec))
        ((stringp rule) rule)))

(defun org-canvas--heading-verify-property (props pom item prop rule)
  "Return the row of PROP at POM against Canvas ITEM, or nil when skipped.
PROPS is the feature's property registration.  RULE says what an
absent PROP stands for: `default' its declared default, a string that
value, `none' no value on Canvas either, and `written' that it is not
compared at all."
  (let* ((spec (org-canvas--heading-verify-spec props prop))
         (written (org-canvas--heading-verify-written pom prop))
         (local (org-canvas--heading-verify-expected spec written rule))
         (type (plist-get spec :type))
         (remote (org-canvas--diff-remote-field spec item)))
    (unless (and (eq rule 'written) (null written))
      (org-canvas--announcement-verify-row
       (downcase prop)
       (if local
           (org-canvas--diff-values-equal-p type local remote)
         (null (org-canvas--diff-normalize-remote remote)))
       (cond (written written)
             (local (format "(unset: %s)" local))
             (t "(unset)"))
       (org-canvas--diff-format-remote type remote)))))

(defun org-canvas--heading-verify-properties (props pom item rules)
  "Return the rows of RULES, (PROP . RULE) pairs, at POM against ITEM.
PROPS is the feature's property registration."
  (delq nil (mapcar (lambda (rule)
                      (org-canvas--heading-verify-property
                       props pom item (car rule) (cdr rule)))
                    rules)))

(defun org-canvas--heading-verify-title (entry item field)
  "Return the title row of heading ENTRY against ITEM's FIELD."
  (let ((org (org-canvas--diff-squeeze (plist-get entry :match-title)))
        (canvas (org-canvas--diff-squeeze
                 (org-canvas--diff-normalize-remote (alist-get field item)))))
    (org-canvas--announcement-verify-row
     "title" (equal org canvas) (format "%S" org) (format "%S" (or canvas "")))))

;;;; Assignments and Files

(defun org-canvas--heading-verify-assignment-rows (feature props entry item)
  "Return the rows of assignment heading ENTRY against Canvas ITEM.
FEATURE and PROPS are the assignment registry entries.  ITEM was read
with `override_assignment_dates' false, so its dates are the base ones.
The rubric the heading links is compared too (issue #567)."
  (append
   (list (org-canvas--heading-verify-title entry item (plist-get feature :title-field)))
   (org-canvas--heading-verify-properties
    props (plist-get entry :pom) item org-canvas--heading-verify-assignment-props)
   (org-canvas--heading-verify-assignment-rubric-rows entry item)))

;;;; Rubrics

(defun org-canvas--heading-verify-rubric-criterion-rows (local remote)
  "Return one row per criterion of LOCAL against REMOTE, by position.
Only when both sides carry the rubric: a missing one is the summary
row's to report.  Criteria are compared position by position, not by
their Canvas ids, since a push may mint fresh ones (issue #256)."
  (when (and local remote)
    (let ((org-criteria (plist-get local :criteria))
          (canvas-criteria (plist-get remote :criteria)))
      (cl-loop for index below (max (length org-criteria) (length canvas-criteria))
               for a = (nth index org-criteria)
               for b = (nth index canvas-criteria)
               collect (org-canvas--announcement-verify-row
                        (format "criterion %d" (1+ index))
                        (org-canvas--rubric-criteria-agree-p a b)
                        (org-canvas--rubric-criterion-text a)
                        (org-canvas--rubric-criterion-text b))))))

(defun org-canvas--heading-verify-rubric-report (local remote)
  "Return the rubric rows of LOCAL against REMOTE, or nil for neither.
One row compares the rubric itself (its id, title, criterion count and
points); the rest compare its criteria, position by position."
  (when (or local remote)
    (append
     (list (org-canvas--announcement-verify-row
            "rubric"
            (org-canvas--rubric-summary-agrees-p local remote)
            (org-canvas--rubric-summary-text local)
            (org-canvas--rubric-summary-text remote)))
     (org-canvas--heading-verify-rubric-criterion-rows local remote))))

(defun org-canvas--heading-verify-assignment-rubric-rows (entry item)
  "Return the rubric rows of assignment heading ENTRY against Canvas ITEM.
The heading's RUBRIC_LINK names its rubric; Canvas's copy comes with
the assignment read, so no second request is made (issue #567)."
  (org-canvas--heading-verify-rubric-report
   (org-canvas--rubric-summary-at-property (plist-get entry :pom) "RUBRIC_LINK")
   (org-canvas--rubric-assignment-summary item)))

(defun org-canvas--heading-verify-rubric-rows (_feature _props entry item)
  "Return the rows of rubric heading ENTRY against Canvas ITEM."
  (org-canvas--heading-verify-rubric-report
   (org-canvas--rubric-heading-summary (plist-get entry :pom))
   (org-canvas--rubric-live-summary item)))

(defun org-canvas--heading-verify-file-rows (_feature props entry item)
  "Return the rows of file heading ENTRY against Canvas ITEM.
PROPS is the files property registration.  The heading is a link, so
its title is not compared."
  (org-canvas--heading-verify-properties
   props (plist-get entry :pom) item org-canvas--heading-verify-file-props))

;;;; Module Items

(defun org-canvas--heading-verify-position (item)
  "Return ITEM's `position', or 0 when Canvas gave none."
  (let ((position (alist-get 'position item)))
    (if (numberp position) position 0)))

(defun org-canvas--heading-verify-remote-items (feature module-id)
  "Return the items of FEATURE's module MODULE-ID on Canvas, in position order."
  (sort (append (org-canvas-api-request-all-pages
                 'GET (concat (org-canvas--feature-item-url feature module-id) "/items"))
                nil)
        (lambda (a b) (< (org-canvas--heading-verify-position a)
                         (org-canvas--heading-verify-position b)))))

(defun org-canvas--heading-verify-remote-index (remote)
  "Return REMOTE's items by id as a string, each as (RANK . ITEM).
RANK is the item's 1-based place in REMOTE."
  (let ((index (make-hash-table :test 'equal))
        (rank 0))
    (dolist (item remote)
      (puthash (format "%s" (alist-get 'id item)) (cons (cl-incf rank) item) index))
    index))

(defun org-canvas--heading-verify-local-items (module-pom)
  "Return the item headings under MODULE-POM, parsed the way the sync does."
  (let ((dir (file-name-directory (expand-file-name org-canvas-modules-file))))
    (mapcar (lambda (marker)
              (prog1 (save-excursion
                       (goto-char marker)
                       (org-canvas--module-item-parse-entry dir))
                (set-marker marker nil)))
            (org-canvas--module-collect-item-markers module-pom))))

(defun org-canvas--heading-verify-item-pushed-p (data)
  "Return non-nil when item DATA would go out with the module sync.
An item linking content that has no CANVAS_ID yet is skipped."
  (or (member (plist-get data :type) org-canvas--module-item-self-owned-types)
      (plist-get data :content-id)
      (plist-get data :page-url)))

(defun org-canvas--heading-verify-item-wanted (data)
  "Return the publish state item DATA's push would send, or nil.
Nil when the push sends no state: the item keeps its content's own."
  (when (or (plist-get data :published-specified)
            (member (plist-get data :type) org-canvas--module-item-self-owned-types))
    (if (plist-get data :published) "published" "unpublished")))

(defun org-canvas--heading-verify-canvas-item (rank item)
  "Return Canvas ITEM, at RANK in its module, as report text."
  (format "%S at %d, %s"
          (or (org-canvas--diff-normalize-remote (alist-get 'title item)) "")
          rank
          (if (org-canvas--diff-normalize-remote (alist-get 'published item))
              "published" "unpublished")))

(defun org-canvas--heading-verify-org-item (data position)
  "Return item heading DATA, sent at POSITION, as report text."
  (let ((wanted (org-canvas--heading-verify-item-wanted data)))
    (format "%S at %d%s" (plist-get data :title) position
            (if wanted (format ", %s" wanted) ""))))

(defun org-canvas--heading-verify-item-agrees-p (data position found)
  "Return non-nil when FOUND, (RANK . ITEM), is item DATA sent at POSITION.
The rank, the title and, when the push sends one, the publish state
must agree."
  (let ((item (cdr found))
        (wanted (org-canvas--heading-verify-item-wanted data)))
    (and (= position (car found))
         (equal (org-canvas--diff-squeeze (plist-get data :title))
                (org-canvas--diff-squeeze
                 (org-canvas--diff-normalize-remote (alist-get 'title item))))
         (or (null wanted)
             (eq (equal wanted "published")
                 (and (org-canvas--diff-normalize-remote (alist-get 'published item)) t))))))

(defun org-canvas--heading-verify-item-row (data position index)
  "Return the row of item heading DATA against INDEX, Canvas's items.
POSITION is where the push sends it, nil for an item the push skips."
  (let* ((field (format "item %s" (or position "-")))
         (id (plist-get data :canvas-id))
         (id (and (org-canvas--nonempty-string-p id) id))
         (found (and id (gethash id index))))
    (cond ((null position)
           (org-canvas--announcement-verify-row
            field nil (format "%S, not sent: its content has no CANVAS_ID"
                              (plist-get data :title))
            "none"))
          ((null id)
           (org-canvas--announcement-verify-row
            field nil (org-canvas--heading-verify-org-item data position)
            "none (no CANVAS_ID; never pushed)"))
          ((null found)
           (org-canvas--announcement-verify-row
            field nil (org-canvas--heading-verify-org-item data position)
            (format "no item %s in this module" id)))
          (t (org-canvas--announcement-verify-row
              field (org-canvas--heading-verify-item-agrees-p data position found)
              (org-canvas--heading-verify-org-item data position)
              (org-canvas--heading-verify-canvas-item (car found) (cdr found)))))))

(defun org-canvas--heading-verify-extra-rows (remote index claimed)
  "Return a row for each REMOTE item no id in CLAIMED names.
INDEX holds REMOTE's ranks."
  (mapcar (lambda (item)
            (let ((found (gethash (format "%s" (alist-get 'id item)) index)))
              (org-canvas--announcement-verify-row
               "extra" nil "no heading"
               (org-canvas--heading-verify-canvas-item (car found) item))))
          (seq-remove (lambda (item) (member (format "%s" (alist-get 'id item)) claimed))
                      remote)))

(defun org-canvas--heading-verify-items (feature module-pom module-id)
  "Return the item rows of the module heading at MODULE-POM, Canvas's MODULE-ID.
FEATURE is the modules registry entry."
  (let* ((remote (org-canvas--heading-verify-remote-items feature module-id))
         (index (org-canvas--heading-verify-remote-index remote))
         (local (org-canvas--heading-verify-local-items module-pom))
         (position 0)
         (rows (mapcar (lambda (data)
                         (org-canvas--heading-verify-item-row
                          data (and (org-canvas--heading-verify-item-pushed-p data)
                                    (cl-incf position))
                          index))
                       local)))
    (append rows
            (org-canvas--heading-verify-extra-rows
             remote index (delq nil (mapcar (lambda (d) (plist-get d :canvas-id)) local))))))

(defun org-canvas--heading-verify-module-rows (feature props entry item)
  "Return the rows of module heading ENTRY against Canvas ITEM.
FEATURE and PROPS are the modules registry entries."
  (let ((pom (plist-get entry :pom)))
    (append (list (org-canvas--heading-verify-title entry item (plist-get feature :title-field)))
            (org-canvas--heading-verify-properties props pom item '(("PUBLISHED" . default)))
            (org-canvas--heading-verify-items feature pom (plist-get entry :id)))))

;;;; One Heading

(defun org-canvas--heading-verify-here (kind feature props)
  "Verify the heading at point, of KIND, against FEATURE's Canvas item.
PROPS is the feature's property registration.  Return the result plist."
  (let* ((entry (append (org-canvas--diff-local-entry "CANVAS_ID")
                        (list :pom (point-marker))))
         (entry (plist-put entry :title (plist-get entry :match-title)))
         (id (plist-get entry :id)))
    (unwind-protect
        (if (null id)
            (org-canvas--announcement-verify-result
             entry nil "no CANVAS_ID; the heading was never pushed")
          (let ((item (org-canvas--pull-item-read feature id)))
            (if (eq item 'gone)
                (org-canvas--announcement-verify-result entry nil "not on Canvas any more")
              (org-canvas--announcement-verify-result
               entry (funcall (nth 2 kind) feature props entry item)))))
      (set-marker (plist-get entry :pom) nil))))

(defun org-canvas--heading-verify-find (feature props target by)
  "Return a marker at FEATURE's heading TARGET names, BY title or id.
PROPS is the feature's property registration, whose query selects the
headings."
  (org-canvas--sync-find-heading
   (symbol-value (plist-get feature :file-var)) (plist-get props :query)
   target by "CANVAS_ID" (plist-get feature :name) "verify"))

(defun org-canvas--heading-verify-one (entry)
  "Verify heading ENTRY, (KIND TARGET BY), and return its result plist.
A heading that cannot be found or read is a result naming the error,
so the next heading is still verified."
  (pcase-let* ((`(,kind ,target ,by) entry)
               (name (nth 1 kind))
               (feature (org-canvas--diff-find-feature name))
               (props (org-canvas--diff-find-properties name)))
    (cons :feature
          (cons name
                (condition-case err
                    (let ((marker (org-canvas--heading-verify-find feature props target by)))
                      (with-current-buffer (marker-buffer marker)
                        (save-excursion
                          (goto-char marker)
                          (set-marker marker nil)
                          (org-canvas--heading-verify-here kind feature props))))
                  (error (org-canvas--announcement-verify-result
                          (list :target target) nil
                          (org-canvas--log-redact (error-message-string err)))))))))

(defun org-canvas--heading-verify-entries (entries)
  "Return ENTRIES as (KIND TARGET BY) lists, KIND a verified feature.
An entry naming a feature this command does not verify is a
`user-error', before anything is read."
  (mapcar (lambda (entry)
            (let* ((parsed (org-canvas--sync-headings-entry entry))
                   (kind (org-canvas--heading-verify-kind (car parsed))))
              (unless kind
                (user-error "Verify reads assignments:, modules:, files: or rubrics:TITLE, not %s:%s"
                            (car parsed) (nth 1 parsed)))
              (cons kind (cdr parsed))))
          entries))

;;;; Report

(defun org-canvas--heading-verify-render (results)
  "Insert the verification report on RESULTS, one section per heading."
  (insert "#+TITLE: Heading verification\n")
  (dolist (result results)
    (insert (format "\n* %s: %s\n\n" (plist-get result :feature)
                    (org-canvas--announcement-verify-summary result)))
    (dolist (row (plist-get result :rows))
      (insert (format "- %-7s %-9s :: Org %s; Canvas %s\n"
                      (if (plist-get row :ok) "ok" "DIFFERS")
                      (plist-get row :field) (plist-get row :org)
                      (plist-get row :canvas))))))

(defun org-canvas--heading-verify-ask ()
  "Read a feature and the title of one of its headings, as a heading entry."
  (when noninteractive
    (user-error "No heading named, and nothing to ask in batch (org-canvas-verify-headings)"))
  (let* ((kind (assoc (completing-read "Verify feature: "
                                       (mapcar #'car org-canvas--heading-verify-features)
                                       nil t)
                      org-canvas--heading-verify-features))
         (feature (org-canvas--diff-find-feature (nth 1 kind)))
         (props (org-canvas--diff-find-properties (nth 1 kind))))
    (list (list (car kind)
                (org-canvas--sync-heading-ask
                 (car kind) (symbol-value (plist-get feature :file-var))
                 (plist-get props :query) "verify")
                'title))))

;;;###autoload
(defun org-canvas-verify-headings (&optional entries)
  "Read pushed assignments, modules, files and rubrics back; return what differs.
ENTRIES name the headings as `org-canvas-show-headings' takes them:
\(FEATURE . TITLE) or (FEATURE TARGET BY), FEATURE assignments,
modules, files or rubrics and BY `title' or `canvas-id'.
Interactively, a feature and a title are asked for.

Each object is read on its own by the CANVAS_ID its heading carries
now (issue #524).  An assignment is compared on its title, PUBLISHED,
POINTS and its base dates, read with `override_assignment_dates'
false, and on the rubric its RUBRIC_LINK names: the title, id,
criterion count and total points, then each criterion's description
and points, against the one Canvas attaches (issue #567); a module on
its title, PUBLISHED and its items, each child heading's position
among the items the push sends, title and publish state, and an item
no child names; a file on PUBLISHED against `locked', HIDDEN,
UNLOCK_AT and LOCK_AT; a rubric on its title, id, criterion count and
points, then each criterion.  Nothing is written, locally or on
Canvas.

Returns one plist per heading: :feature, :title, :id, :rows (each with
:field, :ok, :org and :canvas), :error (a heading not found, never
pushed or gone from Canvas) and :differences, the rows that disagree,
or 1 on an error.  The report is shown, or printed under
`noninteractive', and a line per heading is echoed."
  (interactive)
  (org-canvas--preflight-check)
  (let* ((parsed (org-canvas--heading-verify-entries
                  (or entries (org-canvas--heading-verify-ask))))
         (results (mapcar #'org-canvas--heading-verify-one parsed)))
    (org-canvas--report-display
     org-canvas--heading-verify-buffer
     (lambda () (org-canvas--heading-verify-render results))
     #'org-mode)
    (dolist (result results)
      (org-canvas--user-message "Verify %s: %s" (plist-get result :feature)
                                (org-canvas--announcement-verify-summary result)))
    results))

(provide 'org-canvas-heading-verify)
;;; org-canvas-heading-verify.el ends here
