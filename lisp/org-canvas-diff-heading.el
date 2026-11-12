;;; org-canvas-diff-heading.el --- Drift check for one heading or a named list -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A course's standing rule is to diff before a push, since a push can
;; silently overwrite an edit made in the Canvas web UI.  Before this,
;; the only drift check was `org-canvas-diff', the whole course: one
;; list request per feature and every row.  A push of two assignments
;; and a file by heading either paid for that report or skipped the
;; check, and a script of raw GETs compared `updated_at' with each
;; drawer by eye (issue #670).
;;
;; `org-canvas-diff-at-point' and its by-name twin
;; `org-canvas-diff-headings' (`scripts/org-canvas diff FEATURE:TITLE...'),
;; in the shape of `org-canvas-sync-headings' (#287) and
;; `org-canvas-pull-headings' (#346), check only the headings named.
;; Each heading's object is read on its own (`org-canvas--pull-item-read'),
;; by the id its drawer carries now -- a re-uploaded file's id changes,
;; so it is never cached -- and compared by the very function the
;; course-wide pass compares an entry with (`org-canvas--diff-entry'),
;; under the same options (`org-canvas--diff-feature-options').  So the
;; row a heading gets is the row the full report would give it:
;;
;;   CHANGED   Canvas's stamp is newer than the heading's baseline, or a
;;             compared property, the title or the body differs
;;   MISSING   Canvas no longer holds the heading's id
;;   NOTE      a declared intent Canvas holds and the heading does not
;;   PENDING   the heading has no id, so the next push creates it
;;
;; What a by-heading check cannot say is what only a list says: EXTRA,
;; UNCLAIMED and MOVED rows, and the child passes (module items, New
;; Quiz items, assignment overrides).  Those stay the full report's.
;;
;; The count returned is the drift, CHANGED and MISSING rows, plus one
;; for each heading that could not be checked (not found, or the read
;; failed), so a batch caller's non-zero means "do not push yet".
;; PENDING and NOTE rows are shown and not counted, as in the full
;; report.  Nothing is written, locally or on Canvas (Hard Rule 19):
;; the rows land in a buffer in `org-canvas-diff-mode', whose verbs are
;; separate commands, and `g' checks the same headings again.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)
(require 'org-canvas-diff)
;; The NOTE rows' comparison, and the rows themselves with the report
;; mode they are acted on in.
(require 'org-canvas-diff-fields)
(require 'org-canvas-diff-report)
;; A feature named in any spelling, as `org-canvas-show-headings' reads it.
(require 'org-canvas-drift)

(defconst org-canvas--diff-heading-buffer-name "*canvas-diff-headings*"
  "Name of the buffer holding a by-heading drift check.")

;;;; One Heading

(defun org-canvas--diff-heading-id-property (feature)
  "Return the property FEATURE's headings carry their Canvas id in."
  (or (plist-get feature :id-property) "CANVAS_ID"))

(defun org-canvas--diff-heading-index (id item)
  "Return a remote index holding ITEM under the heading's ID.
ITEM `gone' leaves it empty, which the comparison reports MISSING.
The key is the heading's id, not the item's: a re-uploaded file's old
id still resolves, to the new file, and is not missing."
  (let ((index (make-hash-table :test 'equal)))
    (unless (eq item 'gone)
      (puthash id item index))
    index))

(defun org-canvas--diff-heading-rows (feature props entry)
  "Return the result keys of local ENTRY, a FEATURE heading, against Canvas.
PROPS is FEATURE's property registration.  An entry with no id is a
PENDING row; otherwise its item is read on its own and compared as
the course-wide pass compares it, giving :divergences and :notes."
  (let ((id (plist-get entry :id))
        (name (plist-get feature :name)))
    (if (null id)
        (list :pending (org-canvas--diff-pending
                        name (list entry) nil (buffer-file-name)
                        (org-canvas--diff-heading-id-property feature)))
      (let* ((index (org-canvas--diff-heading-index
                     id (org-canvas--pull-item-read feature id)))
             (row (org-canvas--diff-entry
                   entry index (org-canvas--diff-feature-options feature props))))
        (list :divergences (and row (list row))
              :notes (org-canvas--diff-entry-notes
                      entry index (plist-get props :properties)))))))

(defun org-canvas--diff-heading-here (feature)
  "Check the heading at point, a FEATURE heading, and return its result.
A feature that pushes whole entries (`:pull-whole-entry', a quiz and
its questions) is checked at its level-1 heading.  The result is
shaped like `org-canvas--diff-feature''s, with :heading and :id beside
:name; a failed read is its :error."
  (org-back-to-heading t)
  (when (plist-get feature :pull-whole-entry)
    (while (org-up-heading-safe)))
  (let* ((name (plist-get feature :name))
         (entry (append (org-canvas--diff-local-entry
                         (org-canvas--diff-heading-id-property feature))
                        (list :pom (point-marker))))
         (head (list :name name :heading (plist-get entry :match-title)
                     :id (plist-get entry :id))))
    (unwind-protect
        (condition-case err
            (append head (org-canvas--diff-heading-rows
                          feature (org-canvas--diff-find-properties name) entry))
          (error (append head (list :error (org-canvas--log-redact
                                            (error-message-string err))))))
      (set-marker (plist-get entry :pom) nil))))

(defun org-canvas--diff-heading-feature-here ()
  "Return the feature entry of the current buffer's course file, or signal."
  (or (org-canvas--pull-feature-for-file (buffer-file-name))
      (user-error "%s is not one of this course's Canvas files"
                  (buffer-name))))

;;;; Named Headings

(defun org-canvas--diff-heading-named (feature target by)
  "Check FEATURE's heading TARGET names, BY title or `canvas-id'.
Returns its result; a heading not found, or found twice, is a result
naming the error, so the next heading is still checked."
  (condition-case err
      (let* ((var (plist-get feature :file-var))
             (marker (org-canvas--sync-find-heading
                      (org-canvas--heading-query
                       :file (and var (boundp var) (symbol-value var))
                       :query (or (plist-get (org-canvas--diff-find-properties
                                              (plist-get feature :name))
                                             :query)
                                  "LEVEL=1")
                       :target target :by by
                       :id-property (org-canvas--diff-heading-id-property feature)
                       :feature (plist-get feature :name) :verb "diff"))))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (set-marker marker nil)
            (org-canvas--diff-heading-here feature))))
    (error (list :name (plist-get feature :name) :heading target
                 :error (org-canvas--log-redact (error-message-string err))))))

(defun org-canvas--diff-heading-results (entries)
  "Check each heading ENTRIES names and return the results, writing nothing.
ENTRIES are as `org-canvas-pull-headings' takes them.  Every feature
is resolved first, so a misspelt one stops before anything is read."
  (let* ((parsed (mapcar #'org-canvas--sync-headings-entry entries))
         (features (mapcar (lambda (e) (org-canvas--drift-feature-named (car e)))
                           parsed)))
    (org-canvas--preflight-check)
    (run-hooks 'org-canvas--operation-start-hook)
    (cl-mapcar (lambda (feature e)
                 (org-canvas--diff-heading-named feature (nth 1 e) (nth 2 e)))
               features parsed)))

;;;; Report

(defun org-canvas--diff-heading-unchecked (results)
  "Return the number of RESULTS whose heading could not be checked."
  (cl-count-if (lambda (r) (plist-get r :error)) results))

(defun org-canvas--diff-heading-total (results)
  "Return the drift across RESULTS plus one per heading not checked."
  (+ (org-canvas--diff-count results)
     (org-canvas--diff-heading-unchecked results)))

(defun org-canvas--diff-heading-state (result)
  "Return what one heading RESULT found, in the words of its line."
  (let ((rows (length (plist-get result :divergences))))
    (cond ((plist-get result :error)
           (format "could not check (%s)" (plist-get result :error)))
          ((> rows 0) (format "%d divergence(s)" rows))
          ((plist-get result :pending) "not on Canvas yet")
          (t "no drift"))))

(defun org-canvas--diff-heading-insert (result)
  "Insert the section of one heading RESULT: its line, then its rows."
  (let ((name (plist-get result :name))
        (id (plist-get result :id)))
    (insert (format "%s '%s'%s: %s\n" name (plist-get result :heading)
                    (if id (format " (id %s)" id) "")
                    (org-canvas--diff-heading-state result)))
    (dolist (entry (append (plist-get result :divergences)
                           (plist-get result :notes)
                           (plist-get result :pending)))
      (org-canvas--diff-insert-row name entry))
    (insert "\n")))

(defun org-canvas--diff-heading-render (results)
  "Insert the by-heading drift report on RESULTS at point."
  (insert "org-canvas Drift Check by Heading\n")
  (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
  (insert (make-string 60 ?=) "\n\n")
  (mapc #'org-canvas--diff-heading-insert results)
  (let ((drift (org-canvas--diff-count results))
        (unchecked (org-canvas--diff-heading-unchecked results)))
    (insert (make-string 60 ?=) "\n")
    (insert (if (zerop drift)
                (format "No drift in %d heading(s) checked.\n"
                        (- (length results) unchecked))
              (format "%d divergence(s) found.\n" drift)))
    (when (> unchecked 0)
      (insert (format "%d heading(s) could not be checked.\n" unchecked))))
  (unless noninteractive
    (insert "\n" org-canvas--diff-key-legend)))

(defun org-canvas--diff-heading-show (results refresh)
  "Show the report on RESULTS and return the count a batch caller acts on.
REFRESH is the function `g' in the report buffer calls."
  (org-canvas--report-display
   org-canvas--diff-heading-buffer-name
   (lambda () (org-canvas--diff-heading-render results))
   (lambda ()
     (org-canvas-diff-mode)
     (setq-local org-canvas--diff-refresh-function refresh)))
  (let ((total (org-canvas--diff-heading-total results)))
    (org-canvas--user-message "Drift: %d heading(s) checked, %d divergence(s) or unchecked"
                              (length results) total)
    total))

;;;; Commands

;;;###autoload
(defun org-canvas-diff-at-point ()
  "Report how Canvas differs from the heading at point, writing nothing.
Reads the heading's item on its own, by the id its drawer carries now,
and compares it as `org-canvas-diff' would: the row is the one the full
report gives the heading, CHANGED, MISSING, a NOTE, or PENDING when it
has no id (issue #670).  Returns the number of divergences, plus one
when the heading could not be checked, so a script can refuse to push
on non-zero.  Asks nothing, so it runs in batch too."
  (interactive)
  (let ((feature (org-canvas--diff-heading-feature-here))
        (marker (point-marker)))
    (org-canvas--preflight-check)
    (run-hooks 'org-canvas--operation-start-hook)
    (let ((result (save-excursion (org-canvas--diff-heading-here feature))))
      (org-canvas--diff-heading-show
       (list result)
       (lambda ()
         (with-current-buffer (marker-buffer marker)
           (save-excursion (goto-char marker) (org-canvas-diff-at-point))))))))

;;;###autoload
(defun org-canvas-diff-headings (entries)
  "Report how Canvas differs from each heading ENTRIES names, writing nothing.
ENTRIES are as `org-canvas-pull-headings' takes them: (FEATURE . TITLE)
or (FEATURE TARGET BY), BY `title' or `canvas-id'.  Each heading is
checked as `org-canvas-diff-at-point' checks it, one read per heading
rather than the whole report (issue #670); a heading not found or not
read is a line of its own, and the next is still checked.  The report
is shown, or printed in batch (`scripts/org-canvas diff FEATURE:TITLE').
Returns the number of divergences plus the headings not checked."
  (org-canvas--diff-heading-show
   (org-canvas--diff-heading-results entries)
   (lambda () (org-canvas-diff-headings entries))))

(defun org-canvas-diff-headings-json-data (entries)
  "Return the check of headings ENTRIES names, as an alist for `json-encode'.
The schema of `org-canvas-diff-json-data', with `headings' in place
of `features': each heading's feature, title, id, and state, checked
or error.  `drift' is the count `org-canvas-diff-headings' returns.
Nothing is displayed or written (Hard Rule 19)."
  (let* ((results (org-canvas--diff-heading-results entries))
         (rows (mapcan #'org-canvas--diff-json-rows results)))
    (list (cons 'command "diff")
          (cons 'course org-canvas-directory)
          (cons 'course_id (org-canvas--diff-json-text org-canvas-course-id))
          (cons 'drift (org-canvas--diff-heading-total results))
          (cons 'counts (org-canvas--diff-json-counts rows))
          (cons 'headings (vconcat (mapcar #'org-canvas--diff-heading-json results)))
          (cons 'rows (vconcat rows)))))

(defun org-canvas--diff-heading-json (result)
  "Return how one heading RESULT was checked, as a JSON object alist."
  (let ((err (plist-get result :error)))
    (list (cons 'feature (plist-get result :name))
          (cons 'title (plist-get result :heading))
          (cons 'canvas_id (org-canvas--diff-json-text (plist-get result :id)))
          (cons 'state (if err "error" "checked"))
          (cons 'error err))))

(provide 'org-canvas-diff-heading)
;;; org-canvas-diff-heading.el ends here
