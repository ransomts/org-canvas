;;; org-canvas-publish.el --- Publish a module and everything it lists -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The bulk-publish commands: publish or unpublish a module together with
;; every object its items point at, and apply the PUBLISH_AT schedule.
;; The mechanics live in org-canvas-modules.el (`org-canvas--module-publish-apply',
;; `org-canvas--module-release-items'); the commands live here because
;; they offer to run a full sync afterwards, and the master sync applies
;; the schedule before its first tier.  Publish state belongs to the
;; object, so each command edits the file that declares it (issue #47).

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-modules)

(declare-function org-canvas-sync "org-canvas")

;;;; Bulk Publish
;;
;; "Publish week 3" used to mean six to ten hand edits across three or
;; four files, every week for sixteen weeks (issue #52).  The mechanics
;; live in org-canvas-modules.el; the commands live here because they
;; offer to run a full sync afterwards.

(defun org-canvas--publish-read-module ()
  "Prompt for a module title from modules.org."
  (let ((modules (org-canvas--module-positions)))
    (unless modules
      (user-error "No modules found in %s" org-canvas-modules-file))
    (completing-read "Module: " (mapcar #'car modules) nil t)))

(defun org-canvas--publish-quote-titles (titles)
  "Return TITLES as a readable comma-separated, quoted list."
  (mapconcat (lambda (x) (format "'%s'" x)) titles ", "))

(defun org-canvas--publish-report (title state result)
  "Report RESULT of setting publish STATE on module TITLE."
  (let ((changed (plist-get result :changed))
        (unresolved (plist-get result :unresolved))
        (held (plist-get result :held))
        (verb (if state "Published" "Unpublished")))
    (org-canvas--log-info org-canvas--logger
      "[Publish] %s '%s': %d heading(s) changed" verb title changed)
    (when unresolved
      (org-canvas--log-warning org-canvas--logger
        "[Publish] %d item(s) in '%s' could not be resolved to an object: %s"
        (length unresolved) title
        (org-canvas--publish-quote-titles unresolved)))
    (when held
      (org-canvas--log-info org-canvas--logger
        "[Publish] %d item(s) in '%s' held for their own PUBLISH_AT: %s"
        (length held) title (org-canvas--publish-quote-titles held)))
    (message "%s '%s': %d heading(s) changed%s%s"
             verb title changed
             (if unresolved
                 (format ", %d unresolved" (length unresolved))
               "")
             (if held (format ", %d held" (length held)) ""))))

(defun org-canvas--publish-module-1 (title state)
  "Set publish STATE on module TITLE and everything it lists.
Returns the result plist from `org-canvas--module-publish-apply'."
  (let* ((modules (org-canvas--module-positions))
         (pom (cdr (assoc title modules))))
    (unless pom
      (user-error "No module named '%s' in %s" title org-canvas-modules-file))
    (let ((result (with-current-buffer
                      (org-canvas--find-file-noselect
                       (expand-file-name org-canvas-modules-file))
                    (org-canvas--module-publish-apply pom state))))
      (org-canvas--publish-report title state result)
      result)))

(defun org-canvas--publish-offer-sync ()
  "Offer to sync now, unless running non-interactively."
  (when (and (not noninteractive)
             (y-or-n-p "Push the change to Canvas now? "))
    (org-canvas-sync)))

;;;###autoload
(defun org-canvas-publish-module ()
  "Publish a module and every object its items point at.
Sets PUBLISHED: true on the module in modules.org and on the heading
that owns each linked object, in whichever file that is — the publish
state belongs to the object, so this edits the file that declares it
rather than relying on a module item to carry it (see issue #47).
SubHeaders and external URLs, which have no object of their own, are
set in place.

An item carrying its own PUBLISH_AT is held back until that date and
named in the report, so a week can go live with pieces of it still
scheduled ahead.  Offers to sync afterwards."
  (interactive)
  (let ((title (org-canvas--publish-read-module)))
    (org-canvas--publish-module-1 title t)
    (org-canvas--publish-offer-sync)))

;;;###autoload
(defun org-canvas-unpublish-module ()
  "Unpublish a module and every object its items point at.
The reverse of `org-canvas-publish-module', except that an item's own
PUBLISH_AT never holds it back here: a week comes down whole, rather
than leaving scheduled content live behind an unpublished module."
  (interactive)
  (let ((title (org-canvas--publish-read-module)))
    (org-canvas--publish-module-1 title nil)
    (org-canvas--publish-offer-sync)))

(defun org-canvas--release-due-modules ()
  "Publish every module whose PUBLISH_AT has passed.  Return their titles."
  (let ((released nil))
    (dolist (entry (org-canvas--module-positions))
      (let ((title (car entry))
            (pom (cdr entry)))
        (when (with-current-buffer
                  (org-canvas--find-file-noselect (expand-file-name org-canvas-modules-file))
                (and (org-canvas--module-due-for-release-p pom)
                     (not (equal (org-entry-get pom "PUBLISHED") "true"))))
          (org-canvas--publish-module-1 title t)
          (push title released))))
    (nreverse released)))

(defun org-canvas--release-report-items (result)
  "Log what the item release pass in RESULT did.
Names every item released, every link it could not resolve, and every
item now live inside a module that is still unpublished."
  (let ((released (plist-get result :released))
        (unresolved (plist-get result :unresolved)))
    (when released
      (org-canvas--log-info org-canvas--logger
        "[Release] %d item(s) reached their PUBLISH_AT: %s"
        (length released) (org-canvas--publish-quote-titles released)))
    (when unresolved
      (org-canvas--log-warning org-canvas--logger
        "[Release] %d scheduled item(s) could not be resolved to an object: %s"
        (length unresolved) (org-canvas--publish-quote-titles unresolved)))
    (dolist (entry (plist-get result :hidden))
      (org-canvas--log-warning org-canvas--logger
        (concat "[Release] released inside unpublished module '%s': %s"
                " — students cannot see them until the module is published")
        (car entry) (org-canvas--publish-quote-titles (cdr entry))))))

;;;###autoload
(defun org-canvas-apply-scheduled-releases ()
  "Publish every module and module item whose PUBLISH_AT has passed.
Lets a whole semester's release plan be declared once, in the Org
files, instead of being applied by hand each week.  Idempotent: a
module already carrying PUBLISHED: true is left alone, and the
publish is written into the Org files as explicit properties.

A module item may carry its own PUBLISH_AT, which outranks its
module's: publishing the module leaves that item alone until its own
date arrives, so a week can go live with pieces of it still scheduled
ahead.  An item released inside a module that is still unpublished is
reported as such — the module is left alone, since publish state
belongs to the object (issue #47).

Called at the start of `org-canvas-sync', before any feature syncs, so
the objects it publishes are pushed by the same run.  Returns the list
of everything released, modules first."
  (interactive)
  (let* ((modules (org-canvas--release-due-modules))
         (items (org-canvas--module-release-items)))
    (when modules
      (org-canvas--log-info org-canvas--logger
        "[Release] %d module(s) reached their PUBLISH_AT: %s"
        (length modules) (org-canvas--publish-quote-titles modules)))
    (org-canvas--release-report-items items)
    (append modules (plist-get items :released))))

(provide 'org-canvas-publish)
;;; org-canvas-publish.el ends here
