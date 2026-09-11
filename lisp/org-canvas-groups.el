;;; org-canvas-groups.el --- Pull groups and their members from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module pulls the groups inside each group category, and the
;; students in each group, into groups.org.  It is pull-only: groups are
;; created by Canvas (from a category's CREATE_GROUP_COUNT, by student
;; self-signup, or by hand in the web UI) and membership is decided
;; there too, so org-canvas reads them for reference and never writes
;; them back.  Nothing local is deleted for being absent on Canvas.
;;
;; The module reads the group-category API itself and does not depend
;; on `org-canvas-group-categories'; when group-categories.org exists
;; and holds a category's CANVAS_ID, the category heading links to it.
;;
;; FILE STRUCTURE
;; ==============
;; In groups.org:
;;   - Level 1 headings = Group Categories (linked to group-categories.org)
;;   - Level 2 headings = Groups
;;   - Group body       = one "- Name" line per member, in Canvas's order
;;
;; PROPERTIES (set by pull, read-only)
;; ====================================
;; CANVAS_ID       - Canvas category id (L1) or group id (L2)
;; MAX_MEMBERSHIP  - Member limit, when the group has one
;; JOIN_LEVEL      - Who may join: parent_context_auto_join,
;;                   parent_context_request, invitation_only
;; MEMBERS_COUNT   - Number of members Canvas reports
;; LAST_SYNCED     - Timestamp of last pull
;;
;; PERSONAL DATA
;; =============
;; The member lists are student names.  Only the display name is
;; written -- no email, SIS id or login id -- but a name is still
;; personal data, so treat groups.org the way the submissions
;; directory is treated: keep it out of a course repository (add it
;; to .gitignore) and out of anything shared.  The module writes only
;; `org-canvas-groups-file' itself.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/group_categories        - the course's categories
;;   GET /group_categories/:id/groups         - a category's groups
;;   GET /groups/:id/users                    - a group's members

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-groups-file (org-canvas--path "groups.org")
  "Path to the groups.org file.
It holds student names once pulled; keep it out of a course repository."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-groups-file "groups.org")
(org-canvas-register-properties "groups"
  :label "Groups"
  :file-var 'org-canvas-groups-file
  :query "LEVEL=2"
  :properties
  `((:org-prop "MAX_MEMBERSHIP" :data-key :max_membership :type number
     :doc "Member limit for the group, when Canvas sets one (pull-only)")
    (:org-prop "JOIN_LEVEL" :data-key :join_level :type enum
     :values ,org-canvas--valid-group-join-levels
     :doc "Who may join the group (pull-only)")
    (:org-prop "MEMBERS_COUNT" :data-key :members_count :type number
     :doc "Number of members Canvas reports for the group (pull-only)")))

;;;; Category Link Resolution

(defun org-canvas--group-category-heading (category-id)
  "Return the group-categories.org heading carrying CATEGORY-ID, or nil.
Nil as well when `org-canvas-group-categories-file' is unset or the
file does not exist; the pull then writes the category's name plainly."
  (let ((file (and (boundp 'org-canvas-group-categories-file)
                   org-canvas-group-categories-file)))
    (when (and file (file-exists-p file))
      (let ((target (format "%s" category-id))
            (heading nil))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (when (and (not heading)
                          (equal (org-entry-get (point) "CANVAS_ID") target))
                 (setq heading (org-get-heading t t t t))))
             "LEVEL=1" 'file)))
        heading))))

(defun org-canvas--group-category-title (category-id name)
  "Return the heading text for the category CATEGORY-ID named NAME.
A link to the category's heading in group-categories.org when that
file holds the id, as the modules pull links its items; NAME alone
otherwise."
  (let ((heading (org-canvas--group-category-heading category-id)))
    (if heading
        (let ((unescaped (replace-regexp-in-string
                          "\\\\\\([][]\\)" "\\1" heading)))
          (org-link-make-string
           (format "file:%s::*%s"
                   (file-name-nondirectory org-canvas-group-categories-file)
                   unescaped)
           name))
      name)))

;;;; Heading Upsert

(defun org-canvas--group-find-or-create-l2 (group-id title)
  "Find or create the level-2 heading with CANVAS_ID GROUP-ID under point.
TITLE names a heading that has to be created.  Point must be on the
parent category heading.  Returns the position of the group heading."
  (let ((target (format "%s" group-id))
        (pos nil))
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when (and (not pos)
                    (equal (org-entry-get (point) "CANVAS_ID") target))
           (setq pos (point))))
       "LEVEL=2" 'tree)
      (widen))
    (unless pos
      ;; Append at the end of the category's subtree, after the last
      ;; non-blank line, with one blank line before the new heading;
      ;; the blank lines that preceded the next heading stay after it.
      (goto-char (save-excursion (org-end-of-subtree t) (point)))
      (skip-chars-backward " \t\n")
      (insert (format "\n\n** %s\n" title))
      (forward-line -1)
      (org-back-to-heading t)
      (setq pos (point)))
    pos))

;;;; Members

(defun org-canvas--group-fetch-members (group-id name)
  "Return the users of group GROUP-ID as a list, or the symbol `failed'.
NAME is the group's name, for the log.  A request the enrolment cannot
make (a 403 on one group) is logged, recorded in the pull summary as a
skip, and answered with `failed' so the caller leaves that group's
member list as it was; it does not stop the pull (issue #171)."
  (condition-case err
      (append (org-canvas-api-request-all-pages
               'GET (format "%s/api/v1/groups/%s/users" org-canvas-base-url group-id))
              nil)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Groups] Could not list the members of '%s' (%s); its member list is left as it was"
       name (error-message-string err))
     (org-canvas--pull-summary-record
      :file (file-name-nondirectory org-canvas-groups-file)
      :item name :kind 'skip
      :error (error-message-string err)
      :log-line (org-canvas--pull-summary-current-log-line))
     'failed)))

(defun org-canvas--group-write-members (users)
  "Replace the body of the group heading at point with USERS' names.
One \"- Name\" line per user, in the order Canvas returned them; the
display name only, never an email, SIS id or login id.  Point must be
on the group heading."
  ;; `org-end-of-meta-data' without FULL stops right after the property
  ;; drawer; with it, a blank line after the drawer would be skipped and
  ;; survive the rewrite as a gap between :END: and the first name.
  (let ((body-start (save-excursion (org-end-of-meta-data) (point)))
        (body-end (save-excursion
                    (if (outline-next-heading) (point) (point-max)))))
    (delete-region body-start body-end)
    (goto-char body-start)
    (dolist (user users)
      (insert (format "- %s\n" (or (alist-get 'name user) "Unnamed"))))
    (insert "\n")))

;;;; Pull

(defun org-canvas--group-set-properties (pos group)
  "Write GROUP's pulled properties on the heading at POS."
  (let ((max (alist-get 'max_membership group))
        (join (alist-get 'join_level group))
        (count (alist-get 'members_count group)))
    (when (numberp max)
      (org-canvas-org-set-property pos "MAX_MEMBERSHIP" (format "%s" max)))
    (when (stringp join)
      (org-canvas-org-set-property pos "JOIN_LEVEL" join))
    (when (numberp count)
      (org-canvas-org-set-property pos "MEMBERS_COUNT" (format "%s" count)))))

(defun org-canvas--group-pull-one (group)
  "Upsert GROUP under the category heading at point; return its member count.
Members are rewritten from Canvas unless listing them failed, in
which case the existing list stays and the count is 0."
  (let* ((gid (alist-get 'id group))
         (name (or (alist-get 'name group) "Unnamed group"))
         (pos (org-canvas--group-find-or-create-l2 gid name)))
    (goto-char pos)
    (org-edit-headline name)
    (org-canvas-org-save-sync-state pos gid)
    (org-canvas--group-set-properties pos group)
    (let ((users (org-canvas--group-fetch-members gid name)))
      (if (eq users 'failed)
          0
        (goto-char pos)
        (org-canvas--group-write-members users)
        (length users)))))

(defun org-canvas--group-pull-category (file category)
  "Upsert CATEGORY and its groups into FILE; return (GROUPS . MEMBERS) counts.
The current buffer must visit FILE."
  (let* ((cid (alist-get 'id category))
         (name (or (alist-get 'name category) "Unnamed category"))
         (title (org-canvas--group-category-title cid name))
         (pos (org-canvas--pull-upsert-heading file cid title))
         (groups (append (org-canvas-api-request-all-pages
                          'GET (format "%s/api/v1/group_categories/%s/groups"
                                       org-canvas-base-url cid))
                         nil))
         (members 0))
    (goto-char pos)
    (org-edit-headline title)
    (org-canvas-org-save-sync-state pos cid)
    (dolist (group (org-canvas--pull-sort-items groups))
      (goto-char pos)
      (cl-incf members (org-canvas--group-pull-one group)))
    (cons (length groups) members)))

;;;###autoload
(defun org-canvas-pull-groups ()
  "Pull every group category's groups and members into groups.org.
Categories become level-1 headings, groups level-2, and each group's
members a list of names in its body.  Read-only: nothing is pushed,
and nothing local is deleted for being absent on Canvas.  The file
holds student names afterwards; keep it out of a course repository."
  (interactive)
  (org-canvas--start-operation "PULLING GROUPS")
  (let* ((file (expand-file-name org-canvas-groups-file))
         (categories (append (org-canvas-api-request-all-pages
                              'GET (org-canvas-api-course-endpoint "group_categories"))
                             nil))
         (total (length categories))
         (category-count 0) (group-count 0) (member-count 0)
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "groups")
    (if (zerop total)
        (org-canvas--pull-emit-empty-file file (org-canvas--pull-label-for "groups"))
      (unless (file-exists-p file)
        (with-temp-file file (insert "")))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (dolist (category (org-canvas--pull-sort-items categories))
          (cl-incf category-count)
          (message "Groups [%d/%d] Pulling category '%s'..."
                   category-count total (or (alist-get 'name category) ""))
          (let ((counts (org-canvas--group-pull-category file category)))
            (cl-incf group-count (car counts))
            (cl-incf member-count (cdr counts))))
        (org-canvas--pull-write-file-header)
        (org-canvas--save-buffer)))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger
      "Groups pull complete: %d categories, %d groups, %d members"
      category-count group-count member-count)
    (message "Groups pull complete: %d categories, %d groups, %d members."
             category-count group-count member-count)))

(provide 'org-canvas-groups)
;;; org-canvas-groups.el ends here
