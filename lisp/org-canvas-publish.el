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

;;;; Publish One Heading
;;
;; The only way to take one column down used to be PUBLISHED: false in
;; its drawer and a push of the heading, which sends the whole payload:
;; a description rewritten locally and not ready for students went out
;; with the flag, the opposite of what was wanted (issue #466).  These
;; commands send `published' alone, refuse what Canvas would refuse,
;; and then adopt the new state in the drawer: PUBLISHED, Canvas's new
;; timestamp, and a PAYLOAD_HASH that is restamped when the rest of the
;; heading is what was last pushed and dropped when it is not, so the
;; next push neither skips a real edit nor reports its own unpublish as
;; a conflict.

(defconst org-canvas--publish-at-point-features
  '(("Assignments" :wrapper assignment :can-unpublish unpublishable
     :refusal "it has student submissions")
    ("Quizzes" :wrapper quiz :can-unpublish unpublishable
     :refusal "it has student submissions")
    ("Discussions" :can-unpublish can_unpublish
     :refusal "it has replies")
    ("Pages" :wrapper wiki_page :front-page t :scheduled t))
  "The features a heading is published one at a time in, by registry name.
Each entry's plist says how: `:wrapper' is the key the PUT nests
`published' under (none for a discussion, whose payload is flat);
`:can-unpublish' the field of the item that is false when Canvas will
not unpublish it, and `:refusal' why; `:front-page' that the course's
front page cannot be unpublished; `:scheduled' that a heading carrying
PUBLISH_AT is refused, since Canvas cancels the schedule of a page whose
published state a request changes (issue #379).")

(defun org-canvas--publish-feature-rules (feature)
  "Return the rules plist for FEATURE, a feature registry entry, or nil."
  (cdr (assoc (plist-get feature :name) org-canvas--publish-at-point-features)))

(defun org-canvas--publish-singular (name)
  "Return the singular of the registry feature NAME (\"Quizzes\" is quiz)."
  (org-canvas--singularize (downcase name)))

(defun org-canvas--publish-at-point-target ()
  "Return a plist naming the item the heading at point publishes.
Point moves to the level-1 heading first: a quiz's question is part of
its quiz.  The plist holds :feature, the registry entry; :rules, its
entry in `org-canvas--publish-at-point-features'; :spec, its push at
point (`org-canvas--sync-entry-specs'); :id and :title.  A buffer of
another file, a heading Canvas deleted, a page scheduled by PUBLISH_AT
and a heading never pushed are each a `user-error'."
  (org-back-to-heading t)
  (while (org-up-heading-safe))
  (let* ((file (buffer-file-name))
         (feature (org-canvas--registry-feature-for-file file))
         (rules (org-canvas--publish-feature-rules feature))
         (id-property (or (plist-get feature :id-property) "CANVAS_ID"))
         (title (org-get-heading t t t t)))
    (unless rules
      (user-error "%s is not a file whose headings publish one at a time (%s)"
                  (if file (file-name-nondirectory file) "This buffer")
                  "assignments, quizzes, discussions and pages"))
    (org-canvas--sync-refuse-deleted)
    (when (and (plist-get rules :scheduled) (org-entry-get (point) "PUBLISH_AT"))
      (user-error "'%s' is scheduled by PUBLISH_AT, which a change of its published state would cancel on Canvas; remove PUBLISH_AT first"
                  title))
    (unless (org-entry-get (point) id-property)
      (user-error "'%s' has no %s; push it first, since Canvas has nothing to publish"
                  title id-property))
    (list :feature feature :rules rules :title title
          :id (org-entry-get (point) id-property)
          :spec (cdr (assoc (org-canvas--publish-singular (plist-get feature :name))
                            org-canvas--sync-entry-specs)))))

(defun org-canvas--publish-refusal (rules remote published title)
  "Return why Canvas would refuse to set REMOTE's published state, or nil.
RULES is the feature's entry in `org-canvas--publish-at-point-features',
REMOTE the item as Canvas holds it, PUBLISHED the state asked for and
TITLE the heading's.  Only an unpublish is ever refused."
  (let ((field (plist-get rules :can-unpublish)))
    (cond
     (published nil)
     ((and field (assq field remote) (not (eq (alist-get field remote) t)))
      (format "Canvas will not unpublish '%s': %s" title (plist-get rules :refusal)))
     ((and (plist-get rules :front-page) (eq (alist-get 'front_page remote) t))
      (format "Canvas will not unpublish '%s': it is the course front page" title)))))

(defun org-canvas--publish-remote-drifted-p (feature remote title)
  "Return non-nil when REMOTE changed on Canvas after the heading's baseline.
FEATURE is the registry entry, whose modified field and scheduled
dates are compared as a push's conflict check compares them; TITLE
names the heading in the log.  Never true with
`org-canvas-detect-conflicts' off."
  (when org-canvas-detect-conflicts
    (let ((local (org-canvas--conflict-baseline (point)))
          (remote-time (org-canvas--parse-iso8601-time
                        (alist-get (org-canvas--feature-modified-field feature)
                                   remote))))
      (and local remote-time (time-less-p local remote-time)
           (not (org-canvas--conflict-scheduled-bump
                 local remote-time
                 (org-canvas--conflict-scheduled-dates
                  (org-canvas--feature-scheduled-dates-fn feature)
                  remote title)))))))

(defun org-canvas--publish-heading-clean-p (spec)
  "Return non-nil when the heading at point is what its last push sent.
That is, its stored PAYLOAD_HASH is the hash SPEC's push would compare
now (`org-canvas--sync-heading-hash')."
  (let ((stored (org-entry-get (point) org-canvas--prop-payload-hash)))
    (and stored (equal stored (org-canvas--sync-heading-hash spec)))))

(defun org-canvas--publish-payload (rules published)
  "Return the PUT body setting only `published' to PUBLISHED, by RULES."
  (let ((flag `((published . ,(org-canvas--to-json-boolean published))))
        (wrapper (plist-get rules :wrapper)))
    (if wrapper `((,wrapper . ,flag)) flag)))

(defun org-canvas--publish-stamp (target published response keep-hash drifted)
  "Write PUBLISHED into the heading at point, TARGET's, and save.
RESPONSE is Canvas's reply to the PUT, or nil when nothing was sent;
its modified field becomes CANVAS_UPDATED_AT, unless DRIFTED says
Canvas had changed the item since the baseline, which is kept so the
next push still reports that change.  With KEEP-HASH and not DRIFTED,
PAYLOAD_HASH is restamped for the new state; otherwise it is dropped,
so the next push sends the whole heading.  Return the new hash, or nil."
  (let* ((field (org-canvas--feature-modified-field (plist-get target :feature)))
         (updated (and response (not drifted) (alist-get field response))))
    (org-canvas-org-set-property (point) "PUBLISHED" (if published "true" "false"))
    (when (stringp updated)
      (org-canvas-org-set-property (point) "CANVAS_UPDATED_AT" updated))
    (let ((hash (and keep-hash (not drifted)
                     (org-canvas--sync-heading-hash (plist-get target :spec)))))
      (if hash
          (org-canvas-org-set-property (point) org-canvas--prop-payload-hash hash)
        (org-entry-delete (point) org-canvas--prop-payload-hash))
      (org-canvas--save-buffer)
      hash)))

(defun org-canvas--publish-at-point-report (target verb outcome hash drifted)
  "Log and show how setting TARGET's published state ended.
VERB is \"publish\" or \"unpublish\"; OUTCOME is `sent' or `unchanged'
\(Canvas already held that state); HASH the PAYLOAD_HASH kept, or nil
when it was dropped; DRIFTED non-nil when Canvas had changed the item
since the baseline."
  (let* ((title (plist-get target :title))
         (what (format "%s '%s'" (org-canvas--publish-singular
                                  (plist-get (plist-get target :feature) :name))
                       title))
         (head (if (eq outcome 'sent)
                   (format "%sed %s on Canvas; only the flag was sent" (capitalize verb) what)
                 (format "%s is already %sed on Canvas; nothing sent" what verb)))
         (tail (cond (drifted "; Canvas changed it since the last sync, so the next push will ask about that")
                     (hash "")
                     (t "; PAYLOAD_HASH dropped, so the next push sends the whole heading"))))
    (org-canvas--log-info org-canvas--logger "[Publish] %s%s" head tail)
    (message "%s%s." head tail)))

;;;###autoload
(defun org-canvas-set-published-at-point (published)
  "Publish the heading at point on Canvas when PUBLISHED, else unpublish it.
Only `published' is sent, so the heading's other local edits stay
local.  Assignments, classic quizzes, discussions and pages; a quiz's
question means its quiz.  An unpublish Canvas would refuse (a column
with submissions, a discussion with replies, the front page) is
refused before anything is sent.  The drawer then records PUBLISHED and
Canvas's new CANVAS_UPDATED_AT, and PAYLOAD_HASH is restamped when the
rest of the heading is what was last pushed, and dropped when it is
not.  A course marked read-only refuses; a dry run reads and writes
nothing.  Return `sent', `unchanged' (Canvas already held the state)
or `dry-run' (issue #466)."
  (let* ((target (org-canvas--publish-at-point-target))
         (feature (plist-get target :feature))
         (title (plist-get target :title))
         (verb (if published "publish" "unpublish")))
    (unless org-canvas--dry-run
      (org-canvas--check-writable 'PUT (format "%sing '%s'" verb title)))
    (let ((remote (org-canvas--pull-item-read feature (plist-get target :id))))
      (when (eq remote 'gone)
        (user-error "'%s' is no longer on Canvas; pull it to mark the heading" title))
      (let ((refusal (org-canvas--publish-refusal
                      (plist-get target :rules) remote published title)))
        (when refusal (user-error "%s" refusal)))
      (if org-canvas--dry-run
          (progn
            (org-canvas--log-info org-canvas--logger
              "[DRY-RUN] Would %s '%s' (only the flag)" verb title)
            (message "Would %s '%s' (dry run); nothing sent or written." verb title)
            'dry-run)
        (org-canvas--publish-apply target published remote verb)))))

(defun org-canvas--publish-apply (target published remote verb)
  "Send PUBLISHED for TARGET unless REMOTE is in that state; stamp, report.
VERB is \"publish\" or \"unpublish\".  Return `sent' or `unchanged'."
  (let* ((drifted (org-canvas--publish-remote-drifted-p
                   (plist-get target :feature) remote (plist-get target :title)))
         (clean (org-canvas--publish-heading-clean-p (plist-get target :spec)))
         (outcome (if (eq (eq (alist-get 'published remote) t) (and published t))
                      'unchanged
                    'sent))
         (response (when (eq outcome 'sent)
                     (org-canvas-api-request
                      'PUT (org-canvas--feature-item-url (plist-get target :feature)
                                                        (plist-get target :id))
                      :data (org-canvas--publish-payload (plist-get target :rules)
                                                         published))))
         (hash (org-canvas--publish-stamp target published response clean drifted)))
    (org-canvas--publish-at-point-report target verb outcome hash drifted)
    outcome))

;;;###autoload
(defun org-canvas-publish-at-point ()
  "Publish the heading at point on Canvas, sending nothing else.
See `org-canvas-set-published-at-point'."
  (interactive)
  (org-canvas-set-published-at-point t))

;;;###autoload
(defun org-canvas-unpublish-at-point ()
  "Unpublish the heading at point on Canvas, sending nothing else.
See `org-canvas-set-published-at-point'."
  (interactive)
  (org-canvas-set-published-at-point nil))

(defun org-canvas--publish-feature-named (name)
  "Return the registry entry of the feature NAME publishes in.
NAME is the feature's name, singular or plural, as a string or symbol;
one outside `org-canvas--publish-at-point-features' is a `user-error'."
  (let* ((name (org-canvas--singularize
                (downcase (if (symbolp name) (symbol-name name) name))))
         (entry (cl-find name org-canvas--publish-at-point-features
                         :key (lambda (e) (org-canvas--publish-singular (car e)))
                         :test #'equal)))
    (unless entry
      (user-error "No publish by heading for %s; the features are: %s" name
                  (mapconcat (lambda (e) (org-canvas--publish-singular (car e)))
                             org-canvas--publish-at-point-features ", ")))
    (org-canvas--registry-find-feature (car entry))))

;;;###autoload
(defun org-canvas-set-published (feature target published &optional by)
  "Set PUBLISHED on the FEATURE heading TARGET names, sending nothing else.
FEATURE is assignment, quiz, discussion or page, singular or plural.
TARGET is the heading's exact title or, with BY `canvas-id', its
Canvas id, found as `org-canvas-sync-headings' finds it.  The
heading's file is saved.  Return what
`org-canvas-set-published-at-point' returns (issue #466)."
  (let* ((feature (org-canvas--publish-feature-named feature))
         (marker (org-canvas--sync-find-heading
                  (expand-file-name (symbol-value (plist-get feature :file-var)))
                  "LEVEL=1" target by
                  (or (plist-get feature :id-property) "CANVAS_ID")
                  (org-canvas--publish-singular (plist-get feature :name))
                  (if published "publish" "unpublish"))))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-canvas-set-published-at-point published)))))

(provide 'org-canvas-publish)
;;; org-canvas-publish.el ends here
