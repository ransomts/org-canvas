;;; org-canvas-adopt.el --- Claim the Canvas item a heading's title names -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-adopt-at-point' is the recovery `org-canvas-diff' asks for
;; on an UNCLAIMED row: a heading with no Canvas id whose title Canvas
;; already holds.  It stamps the id and CANVAS_UPDATED_AT from the remote
;; item through `org-canvas--adopt-stamp', the same act the duplicate-title
;; guard performs at the moment it blocks a create (issues #85, #101).

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-diff)

;;;; Adopt One Heading
;;
;; The drift report's UNCLAIMED row names a remote item whose title an
;; unstamped heading shares — what a create that lost its stamp looks
;; like — and used to end "stamp CANVAS_ID or rename", leaving the
;; stamping to hand: look the id up, edit the drawer, get
;; CANVAS_UPDATED_AT right or the next sync walks into the conflict
;; prompt (issue #101).  Nothing local records the id a POST returned,
;; so adoption by title is the one recovery from the whole
;; create-succeeded-then-died class that #99 made loud.  The duplicate
;; guard already adopts at the moment it blocks a create; this is the
;; same act, offered after the fact.

(defun org-canvas--adopt-at-point-feature ()
  "Return the feature entry for the current buffer's file.
Signals a `user-error' when the buffer is not a course file."
  (let* ((file (buffer-file-name))
         (feature (org-canvas--registry-feature-for-file file)))
    (unless feature
      (user-error "%s is not one of this course's Canvas files"
                  (if file (file-name-nondirectory file) "This buffer")))
    feature))

(defun org-canvas--adopt-claimed-ids (feature)
  "Return the ids headings in the current buffer already claim for FEATURE.
Reads the feature's id property off every heading its registry query
selects, so an id another heading owns is never adopted twice."
  (let ((id-property (or (plist-get feature :id-property) "CANVAS_ID"))
        (query (or (plist-get (org-canvas--diff-find-properties
                               (plist-get feature :name))
                              :query)
                   "LEVEL=1")))
    (save-excursion
      (delq nil (org-map-entries
                 (lambda () (org-entry-get (point) id-property))
                 query 'file)))))

(defun org-canvas--adopt-candidates (feature title)
  "Return FEATURE's remote items titled TITLE, as (ITEMS . NOTES).
ITEMS are the adoptable ones.  NOTES are strings, one per match held
back: an item the feature's `:skip-fn' says is not its to manage (a
classic quiz's shadow assignment, the front page) and an id some other
heading in the file already claims.  One list request."
  (let* ((title-field (or (plist-get feature :title-field) 'title))
         (id-field (or (plist-get feature :id-field) 'id))
         (skip-fn (plist-get feature :skip-fn))
         (claimed (org-canvas--adopt-claimed-ids feature))
         (items (append (org-canvas-api-request-all-pages
                         'GET (org-canvas--feature-list-url feature)
                         (org-canvas--feature-list-params feature))
                        nil))
         (adoptable nil)
         (notes nil))
    (dolist (item items)
      (when (equal (alist-get title-field item) title)
        (let ((id (format "%s" (alist-get id-field item))))
          (cond
           ((and skip-fn (funcall skip-fn item))
            (push (format "id %s is %s" id
                          (or (plist-get feature :skip-reason)
                              "excluded by this module"))
                  notes))
           ((member id claimed)
            (push (format "id %s is already claimed by another heading" id) notes))
           (t (push item adoptable))))))
    (cons (nreverse adoptable) (nreverse notes))))

(defun org-canvas--adopt-notes-suffix (notes)
  "Return NOTES as a parenthesized suffix for a message, or \"\"."
  (if notes
      (format " (%s)" (mapconcat #'identity notes "; "))
    ""))

(defun org-canvas--adopt-at-point-1 (feature item title)
  "Stamp the heading at point as the owner of FEATURE's ITEM.
TITLE names the heading in the log.  Writes the id property and
CANVAS_UPDATED_AT and drops PAYLOAD_HASH — see `org-canvas--adopt-stamp'
— then saves.  Nothing is sent to Canvas."
  (let* ((id-property (or (plist-get feature :id-property) "CANVAS_ID"))
         (modified-field (org-canvas--feature-modified-field feature))
         (id (org-canvas--adopt-stamp (point) id-property item modified-field)))
    (org-canvas--save-buffer)
    (org-canvas--log-info org-canvas--logger
      "[Adopt] '%s' now claims %s %s (%s %s %s); PAYLOAD_HASH dropped so the next sync verifies its content"
      title (plist-get feature :name) id id-property
      modified-field (or (alist-get modified-field item) "unknown"))
    (message "'%s' now claims %s %s. Sync it to verify its content."
             title (plist-get feature :name) id)
    id))

;;;###autoload
(defun org-canvas-adopt-at-point ()
  "Claim the Canvas item that shares the title of the heading at point.
For a heading with no Canvas id — a create whose stamp never landed,
or a heading written for an item that already exists — this looks the
title up in the feature's remote list and, on exactly one match,
stamps the id and CANVAS_UPDATED_AT from the item and drops any
PAYLOAD_HASH, so the next sync updates that item in place and verifies
its content instead of creating a second copy.  It is the recovery
`org-canvas-diff' asks for on an UNCLAIMED row.

Zero or several matches refuse and say so; a match the feature holds
back (a quiz's shadow assignment, the front page) or one another
heading already claims is named too.  Nothing is sent to Canvas."
  (interactive)
  (org-back-to-heading t)
  (let* ((feature (org-canvas--adopt-at-point-feature))
         (id-property (or (plist-get feature :id-property) "CANVAS_ID"))
         ;; A files.org heading is a link; its description is the display
         ;; name Canvas knows the file by.
         (title (org-link-display-format (org-get-heading t t t t)))
         (existing (org-entry-get (point) id-property)))
    (when existing
      (user-error "'%s' already claims %s %s — nothing to adopt"
                  title id-property existing))
    (let* ((found (org-canvas--adopt-candidates feature title))
           (items (car found))
           (notes (cdr found))
           (id-field (or (plist-get feature :id-field) 'id)))
      (pcase (length items)
        (0 (user-error "Nothing on Canvas is titled '%s'%s"
                       title (org-canvas--adopt-notes-suffix notes)))
        (1 (org-canvas--adopt-at-point-1 feature (car items) title))
        (_ (user-error "'%s' is held by %d Canvas items (%s); there is no one item to adopt — stamp %s by hand with the one you mean%s"
                       title (length items)
                       (mapconcat (lambda (item)
                                    (format "%s" (alist-get id-field item)))
                                  items ", ")
                       id-property
                       (org-canvas--adopt-notes-suffix notes)))))))

(provide 'org-canvas-adopt)
;;; org-canvas-adopt.el ends here
