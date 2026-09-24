;;; org-canvas-browse.el --- Open the Canvas page of the heading at point -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-browse-at-point' opens the heading at point in the
;; Canvas web interface, and `org-canvas-browse-edit-at-point' its edit
;; page.  Some settings exist only there: a document processor is
;; attached through a browser-only LTI deep-linking flow (issue #184),
;; and before this command getting to it meant assembling
;; https://BASE/courses/COURSE/assignments/ID/edit by hand (issue #292).
;;
;; Where a heading lives is declared by its module, beside its feature
;; registration, as `:web-pages' rules (or through
;; `org-canvas-register-web-pages' for a file the feature registry does
;; not hold); this file only reads them.  A heading whose level no rule
;; names — a quiz question, a rubric criterion — opens its parent.
;; Nothing is sent to Canvas.

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'org)
(require 'org-canvas-core)

(defun org-canvas--browse-entry-here ()
  "Return the web-page entry of the current buffer's course file, or signal."
  (let ((file (buffer-file-name)))
    (or (org-canvas--web-pages-for-file file)
        (user-error "%s is not a course file with pages on Canvas"
                    (if file (file-name-nondirectory file) "This buffer")))))

(defun org-canvas--browse-rules-at-level (rules level)
  "Return those of RULES that apply to a heading at LEVEL."
  (cl-remove-if-not (lambda (rule)
                      (memq (plist-get rule :level) (list nil level)))
                    rules))

(defun org-canvas--browse-apply-rule (rule edit)
  "Return the page RULE gives the heading at point, or nil.
The value is a plist: :path, relative to the course, and :edit,
non-nil when EDIT asked for the edit page and RULE has one.  Nil
when the rule does not apply: its id property is absent, or its
`:path-fn' declines the heading."
  (let ((fn (plist-get rule :path-fn))
        (prop (plist-get rule :id-property))
        (edit (and edit (plist-get rule :edit) t)))
    (cond
     (fn (let ((path (funcall fn)))
           (and path (list :path path))))
     (prop (let ((id (org-entry-get (point) prop)))
             (and id (not (string-empty-p id))
                  (list :path (org-canvas--web-rule-path rule id edit)
                        :edit edit))))
     (t (list :path (org-canvas--web-rule-path rule nil edit)
              :edit edit)))))

(defun org-canvas--browse-unstamped (rules)
  "Signal that the heading at point is not on Canvas yet.
RULES are the ones that named its level; the first id property among
them is the stamp it lacks."
  (user-error "'%s' has no %s yet; sync it to Canvas first"
              (org-get-heading t t t t)
              (or (cl-some (lambda (r) (plist-get r :id-property)) rules)
                  "Canvas id")))

(defun org-canvas--browse-heading-target (entry edit)
  "Return the Canvas page of the heading at point, per ENTRY's rules.
EDIT asks for the edit page.  A heading whose level no rule names
takes its parent's page, so point may move up the outline; the caller
wraps this in `save-excursion'.  The value is the plist of
`org-canvas--browse-apply-rule' with :title, the heading whose page it
is, and :climbed, non-nil when that is an ancestor."
  (let ((rules (plist-get entry :web-pages))
        (climbed nil)
        (target nil))
    (while (not target)
      (let ((here (org-canvas--browse-rules-at-level rules (org-current-level))))
        (cond
         (here
          (setq target (or (cl-some (lambda (r) (org-canvas--browse-apply-rule r edit))
                                    here)
                           (org-canvas--browse-unstamped here))))
         ((org-up-heading-safe) (setq climbed t))
         (t (user-error "%s has no Canvas page for '%s'"
                        (plist-get entry :name) (org-get-heading t t t t))))))
    (append (list :title (org-get-heading t t t t) :climbed climbed) target)))

(defun org-canvas--browse-message (target from edit)
  "Return the echo-area line for opening TARGET.
FROM is the title of the heading the command started on; EDIT whether
the edit page was asked for."
  (concat (format "Opened '%s' on Canvas" (plist-get target :title))
          (if (plist-get target :climbed)
              (format " ('%s' has no page of its own)" from)
            "")
          (if (and edit (not (plist-get target :edit)))
              " (Canvas has no separate edit page for it)"
            "")))

;;;###autoload
(defun org-canvas-browse-at-point (&optional edit)
  "Open the Canvas web page of the heading at point in a browser.
With a prefix argument EDIT, open its edit page where Canvas has one.

The address is built from `org-canvas-base-url', `org-canvas-course-id'
and the heading's id (CANVAS_ID, a page's CANVAS_URL, a New Quiz's
CANVAS_ASSIGNMENT_ID), by the rules the heading's module declares.  A
heading with no page of its own — a quiz question, a rubric criterion,
a heading inside a body — opens its parent's.  An unstamped heading is
not on Canvas yet, so it signals rather than guessing.

For the settings only the web interface can change: a document
processor, anything behind LTI deep linking (issue #292).  Sends
nothing to Canvas.  Returns the address opened."
  (interactive "P")
  (let ((entry (org-canvas--browse-entry-here)))
    (when (org-before-first-heading-p)
      (user-error "Move point to a heading first"))
    (let* ((from (save-excursion (org-back-to-heading t)
                                 (org-get-heading t t t t)))
           (target (save-excursion
                     (org-back-to-heading t)
                     (org-canvas--browse-heading-target entry edit)))
           (url (org-canvas--web-url (plist-get target :path))))
      (browse-url url)
      (message "%s" (org-canvas--browse-message target from edit))
      url)))

;;;###autoload
(defun org-canvas-browse-edit-at-point ()
  "Open the Canvas edit page of the heading at point in a browser.
The same as `org-canvas-browse-at-point' with a prefix argument."
  (interactive)
  (org-canvas-browse-at-point t))

(provide 'org-canvas-browse)
;;; org-canvas-browse.el ends here
