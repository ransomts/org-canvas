;;; org-canvas-core-conflict.el --- Interactive conflict resolution UI -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; When a push would overwrite remote changes made since the last sync,
;; the conflict pipeline shows a diff buffer and prompts the user to
;; push (overwrite remote), pull (overwrite local), or skip.
;;
;; Uppercase answers apply to all remaining conflicts in the current sync
;; via `org-canvas--conflict-apply-all'.

;;; Code:

(require 'diff)
(require 'org-canvas-core-config)

(defvar org-canvas--conflict-apply-all nil
  "When non-nil, auto-resolve all conflicts with this action.
Valid values: nil, \\='push, \\='pull, \\='skip.
Bound per-sync by `org-canvas-define-sync'.")

(defvar org-canvas--current-pull-item-fn nil
  "Pull-item function for the module currently being synced.
Dynamically bound by the sync pipeline so `org-canvas--push-to-api'
can access it without changing per-module push function signatures.")

(defconst org-canvas--conflict-buffer-name "*canvas-conflict*"
  "Buffer name for the conflict resolution diff display.")

;;;###autoload
(defun org-canvas-demo-conflict ()
  "Display a sample conflict diff buffer for evaluating the UI.
Shows the `diff-mode' conflict resolution interface with mock data,
then prompts for an action.  No API calls are made."
  (interactive)
  (let* ((org-canvas--current-pull-item-fn #'ignore)
         (data (list :title "Software Setup Guide"
                     :description (concat
                                   "<p>Follow these steps to set up your "
                                   "development environment for DS 101.</p>\n"
                                   "<p><strong>Step 1: Install Python 3.10+</strong></p>\n"
                                   "<p>Download from python.org</p>\n"
                                   "<p><strong>Step 2: Install VS Code</strong></p>\n"
                                   "<p>Download from code.visualstudio.com</p>")
                     :pom nil))
         (remote `((title . "Software Setup Guide")
                   (updated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ"))
                   (body . ,(concat
                             "<p>Follow these steps to set up your "
                             "development environment for DS 101.</p>\n"
                             "<p><strong>Step 1: Install Python 3.12+</strong></p>\n"
                             "<p>Download from python.org/downloads</p>\n"
                             "<p><strong>Step 2: Install VS Code</strong></p>\n"
                             "<p>Download from code.visualstudio.com</p>\n"
                             "<p><strong>Step 3: Install Git</strong></p>\n"
                             "<p>Download from git-scm.com</p>"))))
         (buf (org-canvas--conflict-format-diff data remote))
         (choice (unwind-protect
                     (org-canvas--conflict-prompt t)
                   (when (buffer-live-p buf)
                     (kill-buffer buf)))))
    (message "Demo conflict resolved with: %s" choice)))

(defun org-canvas--conflict-format-diff (data remote-response)
  "Create a diff buffer comparing local DATA with REMOTE-RESPONSE.
Returns the buffer.  The caller should kill it after resolution."
  (let* ((title (plist-get data :title))
         (pom (plist-get data :pom))
         (last-synced (when (and pom (markerp pom))
                        (with-current-buffer (marker-buffer pom)
                          (org-canvas--pull-read-file-header))))
         (remote-updated (alist-get 'updated_at remote-response))
         (remote-title (or (alist-get 'title remote-response)
                           (alist-get 'name remote-response)))
         (remote-body (or (alist-get 'body remote-response)
                          (alist-get 'message remote-response)
                          (alist-get 'description remote-response)))
         (local-body (plist-get data :description))
         (has-pull-fn (not (null org-canvas--current-pull-item-fn)))
         (buf (get-buffer-create org-canvas--conflict-buffer-name))
         (local-text (or local-body "(none)"))
         (remote-text (or remote-body "(none)"))
         (local-file (make-temp-file "canvas-local-"))
         (remote-file (make-temp-file "canvas-remote-")))
    (unwind-protect
        (progn
          (with-temp-file local-file (insert local-text))
          (with-temp-file remote-file (insert remote-text))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "=== Conflict: %s ===\n\n" (or title "untitled")))
              (insert (format "  Local LAST_SYNCED:   %s\n" (or last-synced "unknown")))
              (insert (format "  Remote updated_at:   %s\n\n" (or remote-updated "unknown")))
              (when (and remote-title title
                         (not (string= title remote-title)))
                (insert (format "--- Title ---\n  Local:  %s\n  Remote: %s\n\n"
                                title remote-title)))
              ;; Insert unified diff with diff-mode coloring
              (insert "--- Body Diff ---\n")
              (insert "  Lines starting with - (red)  = YOUR local Org file\n")
              (insert "  Lines starting with + (green) = Canvas (remote)\n\n")
              (let ((diff-buf (diff-no-select local-file remote-file
                                             '("-u" "--label=Local"
                                               "--label=Remote")
                                             'noasync)))
                (insert-buffer-substring diff-buf)
                (kill-buffer diff-buf))
              (insert "\n--- Actions ---\n")
              (insert "  p = Push (overwrite Canvas)\n")
              (when has-pull-fn
                (insert "  l = Pull (overwrite local)\n"))
              (insert "  s = Skip\n")
              (insert (if has-pull-fn
                          "  P/L/S = Apply to all remaining conflicts\n"
                        "  P/S = Apply to all remaining conflicts\n")))
            (diff-mode)
            (view-mode 1)
            (goto-char (point-min))))
      (delete-file local-file)
      (delete-file remote-file))
    (display-buffer buf)
    buf))

(defun org-canvas--conflict-prompt (has-pull-fn)
  "Prompt user for conflict resolution action.
HAS-PULL-FN controls whether pull options are shown.
Returns one of: push, pull, skip, push-all, pull-all, skip-all."
  (let* ((pull-keys (when has-pull-fn '(?l ?L)))
         (all-keys (append '(?p ?P) pull-keys '(?s ?S)))
         (prompt (if has-pull-fn
                     "[p]ush [l]pull [s]kip (capitals = all): "
                   "[p]ush [s]kip (capitals = all): "))
         (choice (read-char-choice prompt all-keys)))
    (pcase choice
      (?p 'push) (?P 'push-all)
      (?l 'pull) (?L 'pull-all)
      (?s 'skip) (?S 'skip-all))))

(cl-defun org-canvas--resolve-conflict (data remote-response)
  "Resolve a conflict for DATA given REMOTE-RESPONSE.
Checks `org-canvas--conflict-apply-all' for a batch decision.
Otherwise shows a diff buffer and prompts the user.
Returns \\='push, \\='pull, or \\='skip."
  ;; Fast path: apply-all already set by a previous choice
  (when org-canvas--conflict-apply-all
    (cl-return-from org-canvas--resolve-conflict
      org-canvas--conflict-apply-all))
  ;; Show diff and prompt
  (let* ((has-pull-fn (not (null org-canvas--current-pull-item-fn)))
         (buf (org-canvas--conflict-format-diff data remote-response))
         (choice (unwind-protect
                     (org-canvas--conflict-prompt has-pull-fn)
                   (when (buffer-live-p buf)
                     (kill-buffer buf)))))
    (pcase choice
      ('push-all (setq org-canvas--conflict-apply-all 'push) 'push)
      ('pull-all (setq org-canvas--conflict-apply-all 'pull) 'pull)
      ('skip-all (setq org-canvas--conflict-apply-all 'skip) 'skip)
      (_ (progn
           (org-canvas--log-warning org-canvas--logger
             "[Conflict] Unexpected choice %S, defaulting to skip" choice)
           'skip)))))

(defun org-canvas--conflict-pull-local (data remote-response pull-item-fn)
  "Overwrite local heading with REMOTE-RESPONSE data.
DATA is the parsed entry plist.  PULL-ITEM-FN is the module-specific
function that sets properties and body from a remote item.
Updates title, refreshes the file-level #+LAST_SYNCED header, and
deletes stale PAYLOAD_HASH."
  (let ((pom (plist-get data :pom))
        (remote-title (or (alist-get 'title remote-response)
                          (alist-get 'name remote-response))))
    (when (and remote-title pom)
      (save-excursion
        (goto-char (if (markerp pom) (marker-position pom) pom))
        (org-back-to-heading t)
        ;; Update heading title
        (let ((old-heading (org-get-heading t t t t)))
          (when (and old-heading (not (string= old-heading remote-title)))
            (org-edit-headline remote-title)))))
    ;; Call module-specific pull-item to update properties/body
    (let ((pos (if (markerp pom) (marker-position pom) pom)))
      (funcall pull-item-fn remote-response pos)
      (let ((updated-at (alist-get 'updated_at remote-response)))
        (when updated-at
          (org-canvas-org-set-property pos "CANVAS_UPDATED_AT" updated-at)))
      (org-entry-delete pos org-canvas--prop-payload-hash)
      (org-canvas--pull-write-file-header)
      (org-canvas--save-buffer))))

(provide 'org-canvas-core-conflict)
;;; org-canvas-core-conflict.el ends here
