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

(declare-function org-canvas--conflict-baseline-source "org-canvas-core-sync")

(defcustom org-canvas-conflict-strategy nil
  "How to resolve a remote-modified entry without asking.
nil, the default, shows the diff and prompts.  `push' overwrites
Canvas, `pull' overwrites the local heading, `skip' leaves the entry
alone and names it in the log.

This is the seam a scheduled sync needs.  `org-canvas--conflict-apply-all'
looks like it would serve, but every sync entry point rebinds it to nil
so one run's \"apply to all\" answer cannot leak into the next, which
leaves a caller nothing to set (issue #72).  This variable is never
rebound by the pipeline.

Under `noninteractive' a nil setting behaves as `skip': a batch Emacs
has no one to answer `read-char-choice', and a scheduled sync that
leaves remote edits alone and names them beats one that dies reading a
keystroke that cannot arrive."
  :type '(choice (const :tag "Ask" nil)
                 (const :tag "Always push (overwrite Canvas)" push)
                 (const :tag "Always pull (overwrite local)" pull)
                 (const :tag "Always skip" skip))
  :group 'org-canvas)

(defcustom org-canvas-duplicate-title-strategy nil
  "What to do when a heading with no Canvas id names a title Canvas holds.
Every recovery from a partial create — a timeout, an error after the
POST, a killed Emacs — leaves a heading without its CANVAS_ID next to
the item it created, and the next sync would create a second one that
students can see and submit to (issue #85).  So before any POST the
title is looked up in the feature's remote list, and on a hit:

nil, the default, asks — adopt the existing item (stamp its id and
update it), skip the heading, or create anyway.  `adopt', `skip' and
`create' answer without asking; `create' also skips the lookup.
Under `noninteractive' a nil setting behaves as `skip', for the same
reason `org-canvas-conflict-strategy' does.

Titles are not unique on Canvas, so `create' stays available, and a
title several remote items carry is never adopted: there is no one
item to adopt, so it is skipped with a warning instead."
  :type '(choice (const :tag "Ask" nil)
                 (const :tag "Adopt the existing item" adopt)
                 (const :tag "Skip the heading" skip)
                 (const :tag "Create anyway" create))
  :group 'org-canvas)

(defvar org-canvas--conflict-apply-all nil
  "When non-nil, auto-resolve all conflicts with this action.
Valid values: nil, \\='push, \\='pull, \\='skip.
Bound per-sync by `org-canvas-define-sync'.  For a decision that
outlives one run, see `org-canvas-conflict-strategy'.")

(defvar org-canvas--duplicate-apply-all nil
  "When non-nil, answer every duplicate-title prompt this way.
Valid values: nil, \\='adopt, \\='skip, \\='create.  Bound per-sync by
`org-canvas--sync-run-pipeline', like `org-canvas--conflict-apply-all'.")

(defvar org-canvas--current-remote-titles nil
  "Title index of the feature being synced, `none', or nil.
A hash of remote title to the items carrying it, taken from the same
list request as the drift snapshot and bound per-sync by
`org-canvas--sync-run-pipeline' — to `none' when the run has no
snapshot, so the create guard checks nothing rather than spending a
GET per entry.  Nil outside a sync, where `org-canvas--push-to-api'
asks its FIND-FN instead.")

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
         ;; The baseline the check compared, labelled by source (issue #86)
         (baseline (when pom
                     (condition-case nil
                         (cdr (org-canvas--conflict-baseline-source pom))
                       (error nil))))
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
              (insert (format "  Local baseline:      %s\n" (or baseline "none (first sync)")))
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

(cl-defun org-canvas--conflict-prompt (has-pull-fn)
  "Prompt user for conflict resolution action.
HAS-PULL-FN controls whether pull options are shown.
Returns one of: push, pull, skip, push-all, pull-all, skip-all.

Returns `skip' without prompting under `noninteractive'.  Callers
normally settle this earlier, in `org-canvas--conflict-unattended-action';
the guard is repeated here because `read-char-choice' in a batch Emacs
does not fall back to a default, it signals end-of-file and takes the
whole sync with it (issue #72)."
  (when noninteractive
    (cl-return-from org-canvas--conflict-prompt 'skip))
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

(defun org-canvas--conflict-unattended-action (data)
  "Return the action to take for DATA without prompting, or nil to ask.
`org-canvas-conflict-strategy' wins when set.  Failing that, a batch
Emacs takes `skip': `read-char-choice' there reads a keystroke that
cannot arrive and kills the sync (issue #72)."
  (let ((action (or org-canvas-conflict-strategy
                    (and noninteractive 'skip))))
    (when action
      (org-canvas--log-warning org-canvas--logger
        "[Conflict] '%s' resolved as %s without prompting%s"
        (or (plist-get data :title) (plist-get data :display-name) "entry")
        action
        (if org-canvas-conflict-strategy
            " (org-canvas-conflict-strategy)"
          " (batch mode; set org-canvas-conflict-strategy to choose)"))
      action)))

(cl-defun org-canvas--resolve-conflict (data remote-response)
  "Resolve a conflict for DATA given REMOTE-RESPONSE.
Checks `org-canvas--conflict-apply-all' for a batch decision, then
`org-canvas--conflict-unattended-action' for a configured or batch-mode
one.  Otherwise shows a diff buffer and prompts the user.
Returns \\='push, \\='pull, or \\='skip."
  ;; Fast path: apply-all already set by a previous choice
  (when org-canvas--conflict-apply-all
    (cl-return-from org-canvas--resolve-conflict
      org-canvas--conflict-apply-all))
  ;; No one to ask, or a standing instruction not to
  (let ((unattended (org-canvas--conflict-unattended-action data)))
    (when unattended
      (cl-return-from org-canvas--resolve-conflict unattended)))
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

;;;; Duplicate Titles
;;
;; The same shape as conflict resolution, for the other way a push can
;; do damage: creating an item Canvas already holds (issue #85).

(cl-defun org-canvas--duplicate-prompt (title ids)
  "Ask what to do about TITLE, already on Canvas under IDS.
Returns one of: adopt, skip, create, adopt-all, skip-all, create-all.
Adopting is offered only when IDS names exactly one item.  Returns
`skip' without prompting under `noninteractive', for the reason
`org-canvas--conflict-prompt' gives."
  (when noninteractive
    (cl-return-from org-canvas--duplicate-prompt 'skip))
  (let* ((single (null (cdr ids)))
         (keys (append (when single '(?a ?A)) '(?s ?S ?c ?C)))
         (prompt (format "'%s' already exists on Canvas (id %s). %s[s]kip [c]reate anyway (capitals = all): "
                         title (mapconcat #'identity ids ", ")
                         (if single "[a]dopt " "")))
         (choice (read-char-choice prompt keys)))
    (pcase choice
      (?a 'adopt) (?A 'adopt-all)
      (?s 'skip) (?S 'skip-all)
      (?c 'create) (?C 'create-all))))

(defun org-canvas--duplicate-unattended-action (title)
  "Return the action to take for TITLE without prompting, or nil to ask.
`org-canvas-duplicate-title-strategy' wins when set.  Failing that, a
batch Emacs takes `skip'."
  (let ((action (or org-canvas-duplicate-title-strategy
                    (and noninteractive 'skip))))
    (when action
      (org-canvas--log-warning org-canvas--logger
        "[Duplicate] '%s' resolved as %s without prompting%s"
        title action
        (if org-canvas-duplicate-title-strategy
            " (org-canvas-duplicate-title-strategy)"
          " (batch mode; set org-canvas-duplicate-title-strategy to choose)"))
      action)))

(cl-defun org-canvas--resolve-duplicate (title ids)
  "Decide what to do about TITLE, already on Canvas under IDS.
Checks `org-canvas--duplicate-apply-all' for a batch decision, then
`org-canvas--duplicate-unattended-action' for a configured or
batch-mode one, and otherwise prompts.  Returns `adopt', `skip' or
`create'."
  (when org-canvas--duplicate-apply-all
    (cl-return-from org-canvas--resolve-duplicate org-canvas--duplicate-apply-all))
  (let ((unattended (org-canvas--duplicate-unattended-action title)))
    (when unattended
      (cl-return-from org-canvas--resolve-duplicate unattended)))
  (pcase (org-canvas--duplicate-prompt title ids)
    ('adopt-all (setq org-canvas--duplicate-apply-all 'adopt) 'adopt)
    ('skip-all (setq org-canvas--duplicate-apply-all 'skip) 'skip)
    ('create-all (setq org-canvas--duplicate-apply-all 'create) 'create)
    (choice choice)))

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
