;;; org-canvas-core-sync.el --- Sync pipeline core and the define-sync DSL  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The sync engine's core: the run context the generated functions carry,
;; the per-entry pipeline (skip, dry run, fragment and deleted-heading
;; guards, finalize), the counters and the global summary, and the
;; `org-canvas-define-sync' DSL that turns a feature declaration into its
;; sync, pull and at-point commands.
;;
;; The pieces that surround it live in files of their own: the push guards
;; (remote drift, conflicts, error recovery, the duplicate-title guard and
;; the echo check) in org-canvas-core-push.el, and the at-point and
;; by-heading commands in org-canvas-core-heading.el.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)
(require 'org-canvas-core-pull)
(require 'org-canvas-core-macros)
(require 'org-canvas-core-conflict)
(require 'org-canvas-core-delete)

;; The pipeline below calls the push guards and the by-heading runtimes
;; this module's macro generates commands over.  They live in the files
;; layered above this one, which require it in turn, so they are
;; declared here and resolved when a sync actually runs (issue #570).
(declare-function org-canvas--sync-file-baseline "org-canvas-core-push" (sync-file))
(declare-function org-canvas--sync-fetch-remote-snapshot "org-canvas-core-push" (feature-name))
(declare-function org-canvas--sync-write-push-header "org-canvas-core-push" (sync-file ctx))
(declare-function org-canvas--sync-warn-unverified-skips "org-canvas-core-push" (feature-name counters ctx))
(declare-function org-canvas--sync-note-remote-time "org-canvas-core-push" (response ctx))
(declare-function org-canvas--sync-remote-drifted-p "org-canvas-core-push" (canvas-id ctx title))
(declare-function org-canvas--sync-remote-newer-at "org-canvas-core-push" (canvas-id ctx))
(declare-function org-canvas--sync-remote-items-titled "org-canvas-core-push" (title ctx))
(declare-function org-canvas--sync-backfill-baseline "org-canvas-core-push" (canvas-id title ctx))
(declare-function org-canvas--sync-deferred-error-p "org-canvas-core-push" (err))


(defvar org-canvas--sync-in-progress nil
  "Non-nil when a sync is running.  Prevents concurrent syncs.")

(defvar org-canvas--dry-run nil
  "When non-nil, sync shows what would happen without making API calls.
Every code path that issues a non-GET request must check this before
contacting Canvas.  The macro pipeline checks it once in
`org-canvas--sync-execute-pipeline' (which skips push *and* finalize)
and again in `org-canvas--push-to-api'; modules with custom sync loops
\(files, overrides, settings) check it themselves.")

(defconst org-canvas--dry-run-response '((id . "dry-run"))
  "Sentinel response returned by push helpers during a dry run.
Recognized by `org-canvas--dry-run-response-p' so callers can skip
finalization, hash writes, and other bookkeeping that would otherwise
mark an item as synced without anything having been pushed.")

(defun org-canvas--dry-run-response-p (response)
  "Return non-nil if RESPONSE is the dry-run sentinel.
Compares by value, not identity, so a copied or re-consed sentinel
still reads as a dry run."
  (equal response org-canvas--dry-run-response))

(defun org-canvas--graphql-mutate (what document &optional variables)
  "Run the GraphQL mutation DOCUMENT with VARIABLES, described by WHAT.
Under `org-canvas--dry-run' nothing is sent: a [DRY-RUN] line names
WHAT and the sentinel comes back (Hard Rule 1).  Otherwise the reply's
`data' alist is returned; a course marked read-only refuses the POST
at the transport, as it refuses every write (issue #163)."
  (if org-canvas--dry-run
      (progn
        (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s" what)
        org-canvas--dry-run-response)
    (org-canvas--log-info org-canvas--logger "[Execute] GraphQL: %s" what)
    (org-canvas--graphql-send document variables)))

(defvar org-canvas--sync-global-feature-stats nil
  "Per-feature stat entries accumulated during `org-canvas-sync'.
Each entry is a plist (:label STRING :success N :skip N :fail N
:deferred N :failed-titles LIST :skipped-titles LIST).  Only
populated while `org-canvas--sync-global-counters' is non-nil
\(i.e., inside a global sync); standalone sync commands leave it
untouched.  Rendered by `org-canvas--sync-log-global-summary'.")

(defvar org-canvas--sync-global-counters nil
  "When non-nil, a plist accumulating counts across sync modules.
Bound by `org-canvas-sync' to aggregate success/skip/fail totals.")

;;;; 6. Sync Pipeline Infrastructure
;;
;; Runtime helpers called by the generated sync functions.
;; Extracted from `org-canvas-define-sync' for readability.

(defun org-canvas--sync-validate-file (feature-upper sync-file)
  "Validate SYNC-FILE exists and prompt to save unsaved buffers.
FEATURE-UPPER is the uppercased feature name for log messages.
Opens the log buffer and logs a sync header."
  ;; When org-canvas--inhibit-log-clear is non-nil, we are inside
  ;; org-canvas-sync which already checked this guard at entry.
  (when (and org-canvas--sync-in-progress
             (not org-canvas--inhibit-log-clear))
    (user-error "A sync is already in progress.  Please wait for it to finish"))
  (unless (and sync-file (file-exists-p sync-file))
    (org-canvas--signal 'org-canvas-config-error
      "%s file not found: %s" feature-upper sync-file))
  (let ((buf (org-canvas--find-file-noselect sync-file)))
    (when (buffer-modified-p buf)
      (if (org-canvas--confirm
           (format "%s has unsaved changes.  Save before syncing? "
                   (file-name-nondirectory sync-file)))
          (with-current-buffer buf (org-canvas--save-buffer))
        (user-error "Aborted: unsaved changes in %s" sync-file))))
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (org-canvas--log-info org-canvas--logger "========================================")
  (org-canvas--log-info org-canvas--logger ">>> STARTING %s SYNC" feature-upper)
  (org-canvas--log-info org-canvas--logger "File: %s" sync-file)
  (org-canvas--log-info org-canvas--logger "Course: %s | URL: %s"
    org-canvas-course-id org-canvas-base-url)
  (org-canvas--log-info org-canvas--logger "========================================"))

(defun org-canvas--sync-collect-entries (sync-file query feature-name)
  "Collect entry markers and existing CANVAS_IDs from SYNC-FILE.
QUERY is the `org-map-entries' match string.
FEATURE-NAME is used for log messages.
Returns a plist (:targets MARKERS :all-ids-before IDS).

The ids come from the same headings QUERY selects, not from every
level of the file: a module item, a quiz question or an outcome under
its group is synced by the pass that owns that level, and reporting
it as orphaned because this pass did not walk it was one warning per
child on every run (issue #196)."
  (let (targets all-ids-before)
    (with-current-buffer (org-canvas--find-file-noselect sync-file)
      (org-map-entries
       (lambda ()
         (push (point-marker) targets)
         (let ((id (org-entry-get (point) "CANVAS_ID")))
           (when (and id (not (string-empty-p id)))
             (push id all-ids-before))))
       query 'file)
      (setq targets (nreverse targets)
            all-ids-before (nreverse all-ids-before)))
    (org-canvas--log-info org-canvas--logger "Found %d %s to sync"
      (length targets) feature-name)
    ;; Warn about duplicate CANVAS_IDs
    (let ((id-counts (make-hash-table :test 'equal)))
      (dolist (id all-ids-before)
        (puthash id (1+ (gethash id id-counts 0)) id-counts))
      (maphash (lambda (id count)
                 (when (> count 1)
                   (org-canvas--log-warning org-canvas--logger
                     "[Duplicate] CANVAS_ID %s appears %d times in %s"
                     id count sync-file)
                   (message "WARNING: CANVAS_ID %s appears %d times in %s"
                            id count sync-file)))
               id-counts))
    (org-canvas--sync-warn-duplicate-titles targets sync-file)
    (org-canvas--sync-warn-stale-headings targets sync-file)
    (list :targets targets :all-ids-before all-ids-before)))

(defun org-canvas--sync-warn-duplicate-titles (targets sync-file)
  "Warn about duplicate heading titles among TARGETS markers in SYNC-FILE."
  (let ((title-counts (make-hash-table :test 'equal)))
    (dolist (m targets)
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char (marker-position m))
          (let ((title (org-get-heading t t t t)))
            (when title
              (puthash title (1+ (gethash title title-counts 0)) title-counts))))))
    (maphash (lambda (title count)
               (when (> count 1)
                 (org-canvas--log-warning org-canvas--logger
                   "[Duplicate Title] '%s' appears %d times in %s — timeout recovery may match wrong item"
                   title count sync-file)))
             title-counts)))

(defun org-canvas--sync-warn-stale-headings (targets _sync-file)
  "Warn about headings in TARGETS that were previously synced.
Detects headings with LAST_SYNCED but no CANVAS_ID/CANVAS_URL,
which would create duplicates on Canvas.  _SYNC-FILE is unused.
Prompts user to continue if stale headings are found."
  (let ((stale-titles nil))
    (dolist (m targets)
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char (marker-position m))
          (let ((title (org-get-heading t t t t)))
            (when (and (org-entry-get (point) org-canvas--prop-last-synced)
                       (not (or (org-entry-get (point) "CANVAS_ID")
                                (org-entry-get (point) "CANVAS_URL"))))
              (push title stale-titles)
              (org-canvas--log-warning org-canvas--logger
                "[Stale] '%s' was previously synced but has no CANVAS_ID" title))))))
    (when stale-titles
      (message "WARNING: %d heading(s) will create duplicates on Canvas: %s"
               (length stale-titles)
               (mapconcat (lambda (s) (format "'%s'" s)) (nreverse stale-titles) ", "))
      (unless (or noninteractive
                  (y-or-n-p
                   (format "%d heading(s) have LAST_SYNCED but no CANVAS_ID — sync will create duplicates.  Continue? "
                           (length stale-titles))))
        (user-error "Aborted — restore CANVAS_ID properties or delete LAST_SYNCED to fix")))))

(defun org-canvas--sync-finalize-push (response data payload-hash ctx)
  "Finalize a successful push RESPONSE for DATA.
PAYLOAD-HASH is saved to the heading, unless it is nil because the push
owns the stamp (spec :hash `push').  CTX is the sync context plist."
  (let ((finalize-fn (org-canvas--ctx-get ctx :finalize-fn))
        (counters (org-canvas--ctx-get ctx :counters))
        (synced-ids (org-canvas--ctx-get ctx :synced-ids))
        (canvas-id (or (plist-get data :canvas-id)
                       (plist-get data :canvas-url)))
        (cap-feature (capitalize (org-canvas--ctx-get ctx :feature-name)))
        (title (plist-get data (or (org-canvas--ctx-get ctx :title-key) :title)))
        (total-count (org-canvas--ctx-get ctx :total-count))
        (progress (+ (plist-get (org-canvas--ctx-get ctx :counters) :success)
                     (plist-get (org-canvas--ctx-get ctx :counters) :skip)
                     (plist-get (org-canvas--ctx-get ctx :counters) :fail)
                     1)))
    (funcall finalize-fn data response ctx)
    (org-canvas--sync-note-remote-time response ctx)
    (when payload-hash
      (org-canvas-org-set-property (point) org-canvas--prop-payload-hash payload-hash))
    (org-canvas--save-buffer)
    (plist-put counters :success (1+ (plist-get counters :success)))
    (message "%s [%d/%d] Synced '%s'"
      cap-feature progress total-count title)
    (unless canvas-id
      (let ((new-id (or (org-entry-get (point) "CANVAS_ID")
                        (org-entry-get (point) "CANVAS_URL"))))
        (when new-id
          (push new-id (car synced-ids)))))))

(defun org-canvas--sync-payload-hash (payload data hash-extra-fn)
  "Compute the change-detection hash for PAYLOAD.
When HASH-EXTRA-FN is non-nil it is called with the parsed DATA and
its string result is folded into the hash.  This lets a module include
state that lives outside its own payload in change detection — e.g.
modules fold in a digest of their child items so item-level edits
dirty the module."
  (md5 (concat (json-encode payload)
               (when hash-extra-fn (funcall hash-extra-fn data)))))

(defun org-canvas--sync-execute-pipeline (data payload ctx)
  "Execute the skip/dry-run/push pipeline for a single entry.
DATA is the parsed entry, PAYLOAD is the built API payload.
CTX is the sync context plist (see `org-canvas--sync-process-entry')."
  (let* ((push-fn (org-canvas--ctx-get ctx :push-fn))
         (feature-name (org-canvas--ctx-get ctx :feature-name))
         (total-count (org-canvas--ctx-get ctx :total-count))
         (counters (org-canvas--ctx-get ctx :counters))
         (synced-ids (org-canvas--ctx-get ctx :synced-ids))
         (payload-hash (org-canvas--sync-entry-hash payload data ctx))
         (stored-hash (org-entry-get (point) org-canvas--prop-payload-hash))
         (canvas-id (or (plist-get data :canvas-id)
                        (plist-get data :canvas-url)))
         (title (plist-get data (or (org-canvas--ctx-get ctx :title-key) :title)))
         (cap-feature (capitalize feature-name))
         (progress (+ (plist-get counters :success)
                      (plist-get counters :skip)
                      (plist-get counters :fail)
                      1)))
    (when canvas-id
      (push canvas-id (car synced-ids)))
    (cond
     ((and payload-hash stored-hash (string= payload-hash stored-hash) canvas-id
           (not (org-canvas--sync-remote-drifted-p canvas-id ctx title)))
      (plist-put counters :skip (1+ (plist-get counters :skip)))
      (org-canvas--log-info org-canvas--logger "[Skip] '%s' unchanged" title)
      (org-canvas--sync-backfill-baseline canvas-id title ctx)
      (message "%s [%d/%d] Skipping '%s' (unchanged)"
        cap-feature progress total-count title))
     ((org-canvas--sync-refuse-fragment-p canvas-id title)
      (org-canvas--sync-count-fragment title ctx progress))
     ((and org-canvas--dry-run (not (org-canvas--ctx-get ctx :dry-run-in-push)))
      (org-canvas--sync-dry-run-entry canvas-id title ctx))
     (t
      (org-canvas--sync-handle-push-response
       (funcall push-fn data payload ctx) data payload-hash ctx progress)))))

(defun org-canvas--sync-refuse-fragment-p (canvas-id title)
  "Return non-nil when the heading at point, TITLE, is not to be created.
Only an entry without CANVAS-ID can be refused, and only one that
`org-canvas--body-fragment-reason' calls a body fragment a pull before
issue #175 stranded: created, it would be a new Canvas item titled
with a line of another item's text (issue #391).  A dry run and a
batch Emacs refuse it without asking; interactively the question is
asked with `y-or-n-p' itself, since a create students may see is not
the kind of question `org-canvas-assume-yes' answers.  The heading
is never touched."
  (when-let* (((not canvas-id))
              (reason (org-canvas--body-fragment-reason)))
    (org-canvas--log-warning org-canvas--logger
      "[Fragment] '%s' has no CANVAS_ID and %s: probably a body fragment left by an older pull"
      title reason)
    (or org-canvas--dry-run
        noninteractive
        (not (y-or-n-p
              (format "'%s' has no CANVAS_ID and %s, so it is probably a body fragment from an older pull.  Create it on Canvas anyway? "
                      title reason))))))

(defun org-canvas--sync-count-fragment (title ctx progress)
  "Count the refused create of TITLE as a skip in CTX, and say why.
PROGRESS is the 1-based position for the echo-area line."
  (let ((counters (org-canvas--ctx-get ctx :counters)))
    (plist-put counters :skip (1+ (plist-get counters :skip)))
    (plist-put counters :skipped-titles
               (cons (format "%s (probably a body fragment from an older pull; fold it into the item above, or confirm the create interactively)" title)
                     (plist-get counters :skipped-titles)))
    (message "%s [%d/%d] SKIPPED: '%s' (probably a body fragment; not created)"
             (capitalize (org-canvas--ctx-get ctx :feature-name)) progress
             (org-canvas--ctx-get ctx :total-count) title)))

(defun org-canvas--sync-handle-push-response (response data payload-hash ctx progress)
  "Count RESPONSE from the push of DATA, or finalize it.
A symbol means the push stopped short: `conflict' and `pulled' come
from the conflict prompt, `duplicate' from the create guard (issue
#85), which names the entry among the skipped so the summary says
where it went, and `skip' from a push that owns its change detection
and found nothing to send.  The dry-run sentinel comes from a push that
previews its own writes (spec :dry-run `push').  Anything else is the
API response, finalized with PAYLOAD-HASH (nil when the push owns the
stamp).  CTX is the sync context, PROGRESS the 1-based position for
the echo-area line."
  (let* ((counters (org-canvas--ctx-get ctx :counters))
         (cap-feature (capitalize (org-canvas--ctx-get ctx :feature-name)))
         (total-count (org-canvas--ctx-get ctx :total-count))
         (title (plist-get data (or (org-canvas--ctx-get ctx :title-key) :title))))
    (pcase response
      ('skip
       (plist-put counters :skip (1+ (plist-get counters :skip)))
       (message "%s [%d/%d] Skipping '%s'" cap-feature progress total-count title))
      ((pred org-canvas--dry-run-response-p)
       (plist-put counters :dry-run (1+ (or (plist-get counters :dry-run) 0))))
      ('conflict
       (plist-put counters :conflict (1+ (plist-get counters :conflict)))
       (message "%s [%d/%d] CONFLICT: '%s' (remote modified)"
         cap-feature progress total-count title))
      ('pulled
       (plist-put counters :pulled (1+ (or (plist-get counters :pulled) 0)))
       (message "%s [%d/%d] PULLED: '%s' (local updated)"
         cap-feature progress total-count title))
      ('duplicate
       (plist-put counters :skip (1+ (plist-get counters :skip)))
       (plist-put counters :skipped-titles
                  (cons (format "%s (already on Canvas; adopt it with org-canvas-adopt-at-point or rename)" title)
                        (plist-get counters :skipped-titles)))
       (message "%s [%d/%d] SKIPPED: '%s' (title already on Canvas)"
         cap-feature progress total-count title))
      (_
       (condition-case err
           (org-canvas--sync-finalize-push response data payload-hash ctx)
         (error
          ;; The API call landed; only the local bookkeeping failed.  Say
          ;; exactly that, or the operator reads the [FAILED] line as a
          ;; push that never happened and a re-run walks into phantom
          ;; drift (issue #97).
          (org-canvas--log-error org-canvas--logger
            "[Stamp] The push of '%s' landed on Canvas, but stamping the file failed: %s — the entry still carries its old CANVAS_UPDATED_AT and PAYLOAD_HASH, so the next sync will report drift that is not real"
            title (error-message-string err))
          (signal (car err) (cdr err))))))))

(defun org-canvas--dry-run-decision-note (variable)
  "Return a note on how a real sync would settle a stop, from VARIABLE.
VARIABLE is `org-canvas-conflict-strategy' or
`org-canvas-duplicate-title-strategy'.  When it is set, a real sync
would not stop at all, and the note names the standing answer; under
`noninteractive' a nil setting means skip."
  (let ((value (symbol-value variable)))
    (cond (value (format "; %s is %s" variable value))
          (noninteractive "; a batch sync would skip it")
          (t "; a real sync would ask"))))

(defun org-canvas--sync-dry-run-entry (canvas-id title ctx)
  "Report what a real sync would do with the entry at point, and count it.
CANVAS-ID is the entry's id (nil for a create), TITLE its display
name, CTX the sync context.

The per-item conflict check lives inside the push, which a dry run
never reaches, so a dry run used to count every pending entry as a
push — including the ones a real sync would stop at the conflict
prompt, or skip outright in batch (issue #84).  The remote snapshot
already holds every `updated_at' and every title, so the two questions
a real sync would ask are answered here without a request: has Canvas
touched this item since the baseline, and, for a create, does Canvas
already hold this title (issue #85)?  Either way the entry counts as
`:dry-run-conflict', not `:dry-run'."
  (let* ((counters (org-canvas--ctx-get ctx :counters))
         (cap-feature (capitalize (org-canvas--ctx-get ctx :feature-name)))
         (remote-newer (and canvas-id (org-canvas--sync-remote-newer-at canvas-id ctx)))
         (existing (and (not canvas-id)
                        (org-canvas--sync-remote-items-titled title ctx)))
         (verb (cond ((or remote-newer existing) "CONFLICT")
                     (canvas-id "UPDATE")
                     (t "CREATE")))
         (detail (cond
                  (remote-newer
                   (format " (remote updated at %s%s)" remote-newer
                           (org-canvas--dry-run-decision-note
                            'org-canvas-conflict-strategy)))
                  (existing
                   (format " (title already on Canvas as id %s%s)"
                           (mapconcat (lambda (item)
                                        (format "%s" (or (alist-get 'id item)
                                                         (alist-get 'url item))))
                                      existing ", ")
                           (org-canvas--dry-run-decision-note
                            'org-canvas-duplicate-title-strategy)))
                  (t "")))
         (key (if (string= verb "CONFLICT") :dry-run-conflict :dry-run)))
    (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s '%s'%s"
      verb title detail)
    (message "%s [DRY-RUN] Would %s '%s'%s" cap-feature (downcase verb) title detail)
    (plist-put counters key (1+ (or (plist-get counters key) 0)))))

(defun org-canvas--sync-deleted-stamp ()
  "Return the deletion mark ruling the heading at point, or nil.
Its own `org-canvas--prop-canvas-deleted' stamp, else the nearest
ancestor's: a heading under one Canvas deleted is gone with it (issue
#399).  The one definition of a deleted heading, which the push skips
\(issue #392) and validate's cross-heading checks leave out (issue
#402)."
  (org-entry-get (point) org-canvas--prop-canvas-deleted t))

(defun org-canvas--sync-deleted-note (title)
  "Return why the heading at point, TITLE, is not pushed, or nil.
Non-nil when a pull marked it `org-canvas--prop-canvas-deleted': its
Canvas item was deleted, so a PUT would 404 and the recovery would
create the item again under a new id, silently undoing the deletion
\(issue #392).  Non-nil too when a heading above it carries the mark:
an outcome under a group Canvas deleted, whose create would name a
group that is gone (issue #399)."
  (let ((stamp (org-entry-get (point) org-canvas--prop-canvas-deleted)))
    (if stamp
        (format "'%s' was deleted on Canvas (%s %s); not pushed.  Delete the heading, or remove %s and its CANVAS_ID to create it again"
                title org-canvas--prop-canvas-deleted stamp
                org-canvas--prop-canvas-deleted)
      (setq stamp (org-canvas--sync-deleted-stamp))
      (when stamp
        (format "'%s' sits under a heading deleted on Canvas (%s %s); not pushed.  Settle that heading first"
                title org-canvas--prop-canvas-deleted stamp)))))

(defun org-canvas--sync-refuse-deleted ()
  "Signal a `user-error' when the heading at point may not be pushed.
The at-point push's guard (issue #392), shared with a module whose
push at point is written by hand, New Quizzes' (issue #399)."
  (let ((note (org-canvas--sync-deleted-note (org-get-heading t t t t))))
    (when note (user-error "%s" note))))

(defun org-canvas--sync-skip-deleted (title ctx)
  "Count the heading at point, TITLE, as skipped if Canvas deleted it.
Returns non-nil when it did, after logging why and naming it among
CTX's skipped titles; nil otherwise."
  (let ((note (org-canvas--sync-deleted-note title))
        (counters (org-canvas--ctx-get ctx :counters)))
    (when note
      (plist-put counters :skip (1+ (plist-get counters :skip)))
      (plist-put counters :skipped-titles
                 (cons (format "%s (deleted on Canvas)" title)
                       (plist-get counters :skipped-titles)))
      (org-canvas--log-warning org-canvas--logger "[Skip] %s" note)
      (message "%s" note)
      t)))

(defun org-canvas--sync-process-entry (marker ctx)
  "Process one entry through the 4-stage pipeline.
MARKER is the position of the entry.  CTX is the run context from
`org-canvas--sync-make-ctx' (see `org-canvas--sync-ctx-keys'): the
pipeline functions, :feature-name and :feature-upper for the log,
:total-count, :counters and :synced-ids mutated in place, and
:title-key, the plist key of the display name (default :title)."
  (let ((parse-fn (org-canvas--ctx-get ctx :parse-fn))
        (build-fn (org-canvas--ctx-get ctx :build-fn))
        (feature-upper (org-canvas--ctx-get ctx :feature-upper))
        (feature-name (org-canvas--ctx-get ctx :feature-name))
        (total-count (org-canvas--ctx-get ctx :total-count))
        (counters (org-canvas--ctx-get ctx :counters)))
    (org-canvas--log-info org-canvas--logger "----------------------------------------")
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (let ((heading-title (org-get-heading t t t t)))
          (condition-case err
              (unless (org-canvas--sync-skip-deleted heading-title ctx)
                (let ((data (funcall parse-fn)))
                  (if (null data)
                      ;; A heading the parser declines is not an entry: a
                      ;; folder heading in files.org.  Counted as a skip so
                      ;; the tally still adds up to the headings walked.
                      (progn
                        (plist-put counters :skip (1+ (plist-get counters :skip)))
                        (org-canvas--log-info org-canvas--logger
                          "[Skip] '%s' is not a %s entry" heading-title feature-name))
                    (org-canvas--sync-execute-pipeline
                     data (funcall build-fn data) ctx))))
            (error
             (if (org-canvas--sync-deferred-error-p err)
                 (progn
                   (plist-put counters :deferred
                              (1+ (or (plist-get counters :deferred) 0)))
                   (org-canvas--log-warning org-canvas--logger
                     "[Deferred] %s '%s': %s — will apply on a future sync"
                     feature-upper heading-title (error-message-string err))
                   (message "%s '%s' deferred: %s"
                     (capitalize feature-name) heading-title
                     (error-message-string err)))
               (plist-put counters :fail (1+ (plist-get counters :fail)))
               (plist-put counters :failed-titles
                          (cons heading-title
                                (or (plist-get counters :failed-titles) nil)))
               (org-canvas--log-error org-canvas--logger "[FAILED] %s '%s': %s"
                 feature-upper heading-title (error-message-string err))
               (message "%s [%d/%d] FAILED '%s': %s"
                 (capitalize feature-name)
                 (+ (plist-get counters :success) (plist-get counters :skip)
                    (plist-get counters :fail))
                 total-count heading-title (error-message-string err))))))))))

(defun org-canvas--sync-warn-orphans (all-ids-before synced-ids feature-name)
  "Warn about CANVAS_IDs in ALL-IDS-BEFORE not present in SYNCED-IDS.
FEATURE-NAME is used in the log message."
  (dolist (old-id all-ids-before)
    (unless (member old-id synced-ids)
      (org-canvas--log-warning org-canvas--logger
        "[Orphan] CANVAS_ID %s in file was not synced — may be orphaned on Canvas.\n  To clean up: delete the heading's CANVAS_ID property, or use M-x org-canvas-delete-%s-at-point"
        old-id feature-name))))

(defun org-canvas--sync-log-summary (feature-name sync-file counters)
  "Save SYNC-FILE and log completion summary.
FEATURE-NAME is the module name.  COUNTERS is a plist with
:success, :skip, :fail, and optionally :conflict, :pulled, :deferred,
and :dry-run counts."
  (let ((feature-upper (upcase feature-name))
        (success-count (plist-get counters :success))
        (skip-count (plist-get counters :skip))
        (fail-count (plist-get counters :fail))
        (conflict-count (or (plist-get counters :conflict) 0))
        (pulled-count (or (plist-get counters :pulled) 0))
        (deferred-count (or (plist-get counters :deferred) 0))
        (dry-run-count (or (plist-get counters :dry-run) 0))
        (dry-run-conflicts (or (plist-get counters :dry-run-conflict) 0))
        (extra-counts 0))
    (with-current-buffer (org-canvas--find-file-noselect sync-file)
      (org-canvas--save-buffer))
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> %s SYNC COMPLETE" feature-upper)
    (cond
     ((or org-canvas--dry-run (> dry-run-count 0) (> dry-run-conflicts 0))
      ;; A pending entry a real sync would stop at is a conflict, not a
      ;; push, and is counted apart (issue #84).
      (org-canvas--log-info org-canvas--logger
        "Would sync: %d | Would conflict: %d | Skipped: %d"
        dry-run-count dry-run-conflicts skip-count)
      (message "%s dry-run: %d would sync, %d would conflict, %d skipped."
               (capitalize feature-name) dry-run-count dry-run-conflicts
               skip-count))
     (t
      (setq extra-counts (+ conflict-count pulled-count))
      (if (> extra-counts 0)
          (org-canvas--log-info org-canvas--logger
            "Success: %d | Skipped: %d | Failed: %d | Conflicts: %d | Pulled: %d"
            success-count skip-count fail-count conflict-count pulled-count)
        (org-canvas--log-info org-canvas--logger "Success: %d | Skipped: %d | Failed: %d"
          success-count skip-count fail-count))
      (if (> extra-counts 0)
          (message "%s sync: %d success, %d skipped, %d failed, %d conflicts, %d pulled."
                   (capitalize feature-name) success-count skip-count
                   fail-count conflict-count pulled-count)
        (message "%s sync: %d success, %d skipped, %d failed."
                 (capitalize feature-name) success-count skip-count fail-count))
      (when (> deferred-count 0)
        (org-canvas--log-info org-canvas--logger
          "Deferred: %d (will apply on a future sync)" deferred-count))
      (let ((failed-titles (plist-get counters :failed-titles)))
        (when failed-titles
          (org-canvas--log-warning org-canvas--logger "Failed items: %s"
            (mapconcat (lambda (title) (format "'%s'" title))
                       (reverse failed-titles) ", "))))))
    (org-canvas--log-info org-canvas--logger "========================================")
    ;; Accumulate into the global summary when running inside org-canvas-sync
    (org-canvas--sync-record-feature-stats (capitalize feature-name) counters)))

(defconst org-canvas--sync-stat-keys
  '(:success :skip :fail :deferred :dry-run :dry-run-conflict :conflict :pulled)
  "Counter keys carried from a feature's sync into the global summary.
Every counter a run populates belongs here.  A key left out is
silently dropped on the way to the table, which is how a dry run with
31 pending updates came to print 0 success (issue #66).")

(defun org-canvas--sync-record-feature-stats (label counters)
  "Record LABEL's COUNTERS into the global sync summary.
No-op unless a global sync is active (`org-canvas--sync-global-counters'
non-nil).  Accumulates the aggregate counters and merges a per-feature
entry (matched by LABEL) in `org-canvas--sync-global-feature-stats',
including :failed-titles and :skipped-titles name lists."
  (when org-canvas--sync-global-counters
    (dolist (key org-canvas--sync-stat-keys)
      (plist-put org-canvas--sync-global-counters key
                 (+ (or (plist-get org-canvas--sync-global-counters key) 0)
                    (or (plist-get counters key) 0))))
    (let ((entry (cl-find label org-canvas--sync-global-feature-stats
                          :key (lambda (e) (plist-get e :label))
                          :test #'equal)))
      (if entry
          (progn
            (dolist (key org-canvas--sync-stat-keys)
              (plist-put entry key (+ (plist-get entry key)
                                      (or (plist-get counters key) 0))))
            (plist-put entry :failed-titles
                       (append (plist-get entry :failed-titles)
                               (reverse (plist-get counters :failed-titles))))
            (plist-put entry :skipped-titles
                       (append (plist-get entry :skipped-titles)
                               (reverse (plist-get counters :skipped-titles)))))
        (push (append
               (list :label label)
               (mapcan (lambda (key)
                         (list key (or (plist-get counters key) 0)))
                       org-canvas--sync-stat-keys)
               (list :failed-titles (reverse (plist-get counters :failed-titles))
                     :skipped-titles (reverse (plist-get counters :skipped-titles))))
              org-canvas--sync-global-feature-stats)))))

(defun org-canvas--sync-reclassify-skip-as-success (label title-match
                                                          &optional key)
  "Move one skip to success in LABEL's stats entry and the aggregates.
Removes the first :skipped-titles entry containing TITLE-MATCH.  Used
by the end-of-run retry pass when a previously skipped item syncs
after all.  KEY is the counter credited, `:success' when nil; a dry
run passes `:dry-run' (Hard Rule 15).  No-op unless a global sync is
active."
  (let ((key (or key :success)))
    (when org-canvas--sync-global-counters
      (plist-put org-canvas--sync-global-counters :skip
                 (max 0 (1- (or (plist-get org-canvas--sync-global-counters :skip) 0))))
      (setq org-canvas--sync-global-counters
            (plist-put org-canvas--sync-global-counters key
                       (1+ (or (plist-get org-canvas--sync-global-counters key) 0))))
      (let ((entry (cl-find label org-canvas--sync-global-feature-stats
                            :key (lambda (e) (plist-get e :label))
                            :test #'equal)))
        (when entry
          (plist-put entry :skip (max 0 (1- (plist-get entry :skip))))
          (plist-put entry key (1+ (or (plist-get entry key) 0)))
          (plist-put entry :skipped-titles
                     (cl-remove-if (lambda (x)
                                     (string-match-p (regexp-quote title-match) x))
                                   (plist-get entry :skipped-titles)
                                   :count 1)))))))

(defun org-canvas--sync-stat-total (stats key)
  "Return the sum of KEY across STATS."
  (apply #'+ (mapcar (lambda (s) (or (plist-get s key) 0)) stats)))

(defun org-canvas--sync-stats-dry-run-p (stats)
  "Return non-nil when STATS describe a dry run.
True inside a dry run, and for stats recorded by one — a run where
every pending entry would conflict has a `:dry-run' total of zero and
must still be reported as the preview it was (issue #84)."
  (or org-canvas--dry-run
      (> (org-canvas--sync-stat-total stats :dry-run) 0)
      (> (org-canvas--sync-stat-total stats :dry-run-conflict) 0)))

(defun org-canvas--sync-summary-columns (stats)
  "Return the columns the summary table should show for STATS.
Each entry is (HEADER . KEY).

A dry run reports what it *would* do, so `Would sync' replaces
`Success': a preview whose table reads 0 success looks like a course
with nothing pending, which is how a dry run with 31 pending
assignment updates got taken for a clean one (issue #66).  Under the
DRY RUN header its `Conflicts' column counts the entries a real sync
would stop at (issue #84).

Conflicts and pulls earn a column only when the run had some, so an
ordinary clean sync keeps the narrow table it has always printed."
  (if (org-canvas--sync-stats-dry-run-p stats)
      (append '(("Would sync" . :dry-run))
              (when (> (org-canvas--sync-stat-total stats :dry-run-conflict) 0)
                '(("Conflicts" . :dry-run-conflict)))
              '(("Skipped" . :skip) ("Failed" . :fail) ("Deferred" . :deferred)))
    (append '(("Success" . :success) ("Skipped" . :skip)
              ("Failed" . :fail) ("Deferred" . :deferred))
            (when (> (org-canvas--sync-stat-total stats :conflict) 0)
              '(("Conflicts" . :conflict)))
            (when (> (org-canvas--sync-stat-total stats :pulled) 0)
              '(("Pulled" . :pulled))))))

(defun org-canvas--sync-summary-render-table (stats columns)
  "Log STATS as an aligned table with COLUMNS, one row per feature."
  (org-canvas--log-info org-canvas--logger "%-20s%s" "Type"
    (mapconcat (lambda (col) (format "%10s" (car col))) columns ""))
  (dolist (s stats)
    (org-canvas--log-info org-canvas--logger "%-20s%s"
      (plist-get s :label)
      (mapconcat (lambda (col)
                   (format "%10d" (or (plist-get s (cdr col)) 0)))
                 columns ""))))

(defun org-canvas--sync-log-global-summary ()
  "Log the aggregated per-type table and named failed/skipped items.
Renders `org-canvas--sync-global-feature-stats' (in sync order) as an
aligned table whose columns follow what the run actually did — see
`org-canvas--sync-summary-columns' — followed by one warning line per
feature naming failed items and noteworthy skipped items.  No-op when
no stats were recorded."
  (let ((stats (reverse org-canvas--sync-global-feature-stats)))
    (when stats
      (when (org-canvas--sync-stats-dry-run-p stats)
        (org-canvas--log-info org-canvas--logger
          "DRY RUN — nothing was written; the counts below are what a real sync would do"))
      (org-canvas--sync-summary-render-table
       stats (org-canvas--sync-summary-columns stats))
      (dolist (s stats)
        (let ((failed (plist-get s :failed-titles))
              (skipped (plist-get s :skipped-titles)))
          (when failed
            (org-canvas--log-warning org-canvas--logger "Failed %s: %s"
              (plist-get s :label)
              (mapconcat (lambda (x) (format "'%s'" x)) failed ", ")))
          (when skipped
            (org-canvas--log-warning org-canvas--logger "Skipped %s: %s"
              (plist-get s :label)
              (mapconcat (lambda (x) (format "'%s'" x)) skipped ", "))))))))

;; Plist key convention in parse-entry return values:
;; - kebab-case (:canvas-id, :pom, :local-path) for internal pipeline fields
;; - snake_case (:points_possible, :due_at) for fields mapping 1:1 to Canvas API params
;; - Universal fields (:title, :published, :description) use kebab-case regardless
;; Modules with hash-table payloads (rubrics, modules, files) use all kebab-case
;; since they remap everything manually in build-payload.

(defun org-canvas--make-push-fn-form (endpoint id-key title-key find-fn)
  "Build a push lambda form for endpoint-based sync macros.
Returns a quoted lambda of (DATA PAYLOAD &optional CTX) that calls
`org-canvas--push-to-api' with ENDPOINT and optional ID-KEY, TITLE-KEY,
FIND-FN, handing it the run context."
  `(lambda (data payload &optional ctx)
     (org-canvas--push-to-api data payload
                              (org-canvas--push-options
                               :ctx ctx
                               :endpoint ,endpoint
                               ,@(when id-key `(:id-key ,id-key))
                               ,@(when title-key `(:title-key ,title-key))
                               ,@(when find-fn `(:find-fn ,find-fn))))))

(defun org-canvas--make-finalize-fn-form (id-field id-property title-key post-fn
                                                   &optional endpoint)
  "Build a finalize lambda form for endpoint-based sync macros.
Returns a quoted lambda of (DATA RESPONSE &optional CTX) that calls
`org-canvas--finalize-item' with optional ID-FIELD, ID-PROPERTY,
TITLE-KEY, POST-FN, handing it the run context.  ENDPOINT
travels with POST-FN so a post-fn that writes to Canvas again can have
the baseline re-read from it (issue #124)."
  `(lambda (data response &optional ctx)
     (org-canvas--finalize-item data response
                                (org-canvas--finalize-options
                                 :ctx ctx
                                 ,@(when id-field `(:id-field ,id-field))
                                 ,@(when id-property `(:id-property ,id-property))
                                 ,@(when title-key `(:title-key ,title-key))
                                 ,@(when (and post-fn endpoint) `(:endpoint ,endpoint))
                                 ,@(when post-fn `(:post-fn ,post-fn))))))

(defconst org-canvas--sync-spec-keys
  '(:feature :file :query :parse :build :push :finalize
    :pull-item-fn :title-key :hash-extra :hash :dry-run :prepare :after-sync
    :id-property :after-heading)
  "Keys a sync spec may carry.
A sync spec is the plist `org-canvas-define-sync' builds from its
options and hands to `org-canvas--sync-run-pipeline' and
`org-canvas--push-at-point-runtime': one value, named the way the
module wrote it, instead of eleven positional arguments.  :feature is
the module name (the singular for a push at point), :file the expanded
Org file, :query the Org match, :parse, :build, :push and :finalize
the four stage functions, :pull-item-fn the function that enables the
pull answer at a conflict prompt, :title-key the plist key of the
display name, :hash-extra the function whose result is folded into the
payload hash, :hash the change-detection hash's owner (a function of
PAYLOAD and DATA replacing the payload's md5, or the symbol `push' when
the push decides for itself and the runner neither skips before it nor
stamps after it), :dry-run the symbol `push' when the push guards its
own writes and previews them (the runner then does not intercept a dry
run ahead of it), :prepare a function of the context run once before
the first entry, whose result the context keeps as :prepared,
:after-sync the hook run on the context before the summary, and
:id-property the Org property the stamp lives in (CANVAS_ID unless the
module says otherwise), which a sync by heading matches an id against
\(issue #287), and :after-heading the function a push of one heading
runs after it (see `org-canvas--push-at-point-after-heading').")

(defun org-canvas--sync-entry-hash (payload data ctx)
  "Return the change-detection hash for PAYLOAD and DATA under CTX, or nil.
The md5 of the encoded payload (plus :hash-extra-fn) unless the spec
declared :hash: a function is called with PAYLOAD and DATA; `push'
means the push owns change detection, and nil is returned so the
runner neither skips on a stored hash nor stamps one."
  (let ((hash-fn (org-canvas--ctx-get ctx :hash-fn)))
    (cond ((eq hash-fn 'push) nil)
          (hash-fn (funcall hash-fn payload data))
          (t (org-canvas--sync-payload-hash
              payload data (org-canvas--ctx-get ctx :hash-extra-fn))))))

(defun org-canvas--sync-check-spec (spec required)
  "Signal on a key of SPEC outside the sync spec keys, or a REQUIRED key absent.
A plist accepts any key silently, so a misspelt option in a module would
otherwise vanish; this names it instead."
  (let ((rest spec))
    (while rest
      (unless (memq (car rest) org-canvas--sync-spec-keys)
        (error "Sync spec: unknown key %S (see `org-canvas--sync-spec-keys')"
               (car rest)))
      (setq rest (cddr rest))))
  (dolist (key required)
    (unless (plist-get spec key)
      (error "Sync spec: %s is required" key))))

(defun org-canvas--sync-run-pipeline (spec)
  "Run the full sync pipeline described by SPEC.
SPEC is a sync spec (`org-canvas--sync-spec-keys'): :feature, :file,
:query, :parse, :build, :push and :finalize are required.
:pull-item-fn enables the interactive conflict pull; :title-key is the
plist key of the display name in logs; :hash-extra, when non-nil, is
called with the parsed data and its string result is folded into the
payload hash (see `org-canvas--sync-payload-hash').

:prepare, when non-nil, is called with the run context after the
snapshot and before the first entry; its result is stored in the
context as :prepared for the push and finalize functions to read.  It
may signal, which stops the run before anything is sent: outcomes fetch
the course root group here.

:after-sync, when non-nil, is called with the run context once every
entry has been processed, just before the summary.  It is the place for
checks that need remote state and so cannot live in the offline
validator — it must not signal, since a reconciliation problem should
never fail a sync that otherwise succeeded.

Returns the run context (`org-canvas--sync-ctx-keys'), so an
orchestrator can read what the run accumulated — the master sync
collects the module items left pending — without a global."
  (org-canvas--sync-check-spec
   spec '(:feature :file :query :parse :build :push :finalize))
  ;; Before the log header and before anything is read or sent: a file
  ;; outside `org-canvas-write-allowlist' is refused at the start, so a
  ;; scoped identity's run never previews or pushes it (issue #550).
  (org-canvas--check-write-scope (plist-get spec :file) "a sync")
  (org-canvas-clear-log)
  (let* ((feature-name (plist-get spec :feature))
         (sync-file (plist-get spec :file))
         (query (plist-get spec :query))
         (parse-fn (plist-get spec :parse))
         (build-fn (plist-get spec :build))
         (push-fn (plist-get spec :push))
         (finalize-fn (plist-get spec :finalize))
         (pull-item-fn (plist-get spec :pull-item-fn))
         (title-key (plist-get spec :title-key))
         (hash-extra-fn (plist-get spec :hash-extra))
         (hash-fn (plist-get spec :hash))
         (dry-run-in-push (eq (plist-get spec :dry-run) 'push))
         (prepare-fn (plist-get spec :prepare))
         (after-sync-fn (plist-get spec :after-sync))
         (feature-upper (upcase feature-name)))
    (org-canvas--sync-validate-file feature-upper sync-file)
    (let* ((entries (org-canvas--sync-collect-entries sync-file query feature-name))
           (targets (plist-get entries :targets))
           (all-ids-before (plist-get entries :all-ids-before))
           (counters (list :success 0 :skip 0 :fail 0 :pulled 0 :dry-run 0
                           :dry-run-conflict 0 :conflict 0))
           (synced-ids (list nil))
           (baseline (org-canvas--sync-file-baseline sync-file))
           ;; One list request per feature, and only when there is something
           ;; to compare.  Not gated on BASELINE: an entry carries its own
           ;; CANVAS_UPDATED_AT, which is what makes this work on a course
           ;; that has only ever been pushed.  The same request indexes the
           ;; remote titles, which the create path consults (issue #85).
           (snapshot (when (and org-canvas-detect-conflicts targets)
                       (org-canvas--sync-fetch-remote-snapshot feature-name)))
           ;; Everything this run sets for itself lives here and dies
           ;; with it (issue #141).  :remote-titles is `none' rather
           ;; than nil without a snapshot: inside a sync the create
           ;; guard reads this or nothing, and must not fall back to a
           ;; GET per entry.
           (ctx (org-canvas--sync-make-ctx
                 :baseline baseline
                 :remote-updated (plist-get snapshot :updated)
                 :remote-titles (or (plist-get snapshot :titles) 'none)
                 :remote-times (list nil)
                 :parse-fn parse-fn
                 :build-fn build-fn
                 :push-fn push-fn
                 :finalize-fn finalize-fn
                 :feature-name feature-name
                 :feature-upper feature-upper
                 :total-count (length targets)
                 :counters counters
                 :synced-ids synced-ids
                 :title-key title-key
                 :hash-extra-fn hash-extra-fn
                 :hash-fn hash-fn
                 :dry-run-in-push dry-run-in-push
                 :pull-item-fn pull-item-fn)))
      (when prepare-fn
        (plist-put ctx :prepared (funcall prepare-fn ctx)))
      (dolist (marker targets)
        (org-canvas--sync-process-entry marker ctx))
      (dolist (m targets) (set-marker m nil))
      (unless org-canvas--dry-run
        (org-canvas--sync-write-push-header sync-file ctx))
      (org-canvas--sync-warn-unverified-skips feature-name counters ctx)
      (org-canvas--sync-warn-orphans all-ids-before (car synced-ids) feature-name)
      (when after-sync-fn
        (funcall after-sync-fn ctx))
      (org-canvas--sync-log-summary feature-name sync-file counters)
      ctx)))

(defconst org-canvas--singular-overrides
  '(("group-categories" . "group-category")
    ("new-quizzes" . "new-quiz")
    ("quizzes" . "quiz"))
  "Irregular plural-to-singular mappings for module names.")

(defun org-canvas--singularize (name)
  "Singularize module NAME for function naming.
Uses `org-canvas--singular-overrides' for irregular forms,
otherwise strips trailing \"s\"."
  (or (cdr (assoc name org-canvas--singular-overrides))
      (if (string-suffix-p "s" name)
          (substring name 0 -1)
        name)))

(defun org-canvas--pull-heading-fn-forms (feature-name singular file-expr
                                                       query id-property)
  "Return the forms defining and registering org-canvas-pull-SINGULAR.
SINGULAR is the module's singular name, and FEATURE-NAME the plural
the whole-file pull is named after;
FILE-EXPR, QUERY and ID-PROPERTY are the module's, as
`org-canvas-define-sync' received them.  A singular that equals
FEATURE-NAME would name the whole-file pull org-canvas-pull-FEATURE
and silently replace it, so that is an error at expansion (issue
#346)."
  (when (string= singular feature-name)
    (error "org-canvas-define-sync: %s has no singular; org-canvas-pull-%s would replace its whole-file pull"
           feature-name singular))
  (let ((fn-name (intern (format "org-canvas-pull-%s" singular))))
    `(;;;###autoload
      (defun ,fn-name (&optional target by)
        ,(format "Replace the %s heading TARGET names with Canvas's version.
The pull twin of `org-canvas-sync-%s', from
issue #346.  TARGET is the heading's exact title in %s,
or, with BY `canvas-id', its %s; an error names a target that
matches no heading or more than one.  Nil asks for a title (never
under `noninteractive').  Never asks to confirm: naming the heading
is the confirmation.  Saves the file and returns a plist whose
:outcome is `pulled', `dry-run' or `deleted' (marked CANVAS_DELETED,
Canvas having deleted the item).  The whole-file pull is
`org-canvas-pull-%s';
`org-canvas-pull-headings' takes several."
                 singular singular
                 (if (symbolp file-expr)
                     (format "the file\n`%s' names" file-expr)
                   "the module's file")
                 (or id-property "CANVAS_ID")
                 feature-name)
        (interactive)
        (org-canvas--pull-heading-runtime
         (org-canvas--heading-query
          :file (expand-file-name ,file-expr) :query ,query
          :target target :by by
          :id-property ,(or id-property "CANVAS_ID")
          :feature ,singular :verb "pull")))
      (org-canvas--pull-register-heading-fn ,singular #',fn-name))))

(defmacro org-canvas-define-sync (feature &rest args)
  "Define a sync function for FEATURE using the 4-stage pipeline pattern.

FEATURE is a symbol like \\='pages or \\='announcements.

ARGS is a plist with the following keys:
  :file - Expression that evaluates to the org file path (required)
  :query - Org match query for entries (default: \"LEVEL=1\")
  :parse - Function to parse entry at point (required)
  :build - Function to build payload from parsed data (required)
  :push - Push function (DATA PAYLOAD &optional CTX), or auto-generated
          from :endpoint
  :finalize - Finalize function (DATA RESPONSE &optional CTX), or
              auto-generated from :endpoint
  :endpoint - API endpoint suffix; auto-generates :push/:finalize
  :id-key - Plist key for Canvas ID in data (default: :canvas-id)
  :id-field - Alist key for ID in API response (default: \\='id)
  :id-property - Org property name for ID (default: \"CANVAS_ID\")
  :find-fn - Search function for timeout recovery (used by auto-generated :push)
  :post-fn - Callback (DATA RESPONSE &optional CTX) after finalize (used by
             auto-generated :finalize); one that writes to Canvas again
             says so with `org-canvas--finalize-note-remote-write'
  :title-key - Plist key for display name in logs (default: :title)
  :pull-item-fn - Optional function to pull remote data into local heading
                  for interactive conflict resolution; also generates the
                  pull by heading below
  :pull-heading - Non-nil generates the pull by heading for a module that
                  registers its pull-item function apart from the sync
                  (quizzes), so it passes no :pull-item-fn here
  :hash-extra - Optional function called with the parsed data; its string
                result is folded into the payload hash so state outside the
                payload (e.g. module items) participates in change detection
  :hash - Optional owner of change detection.  A function of (PAYLOAD DATA)
          returns the hash stored as PAYLOAD_HASH and compared before a push,
          in place of the payload's md5.  The symbol `push' means the push
          decides for itself: the runner never skips on a stored hash and
          never stamps one, and the push returns `skip' when there is
          nothing to send (files, whose tiers compare bytes and metadata)
  :dry-run - The symbol `push' when the push guards every write itself and
             previews them: the runner then calls it under a dry run instead
             of reporting from the snapshot, and counts the sentinel it
             returns
  :prepare - Optional function of the run context, run once after the
             snapshot and before the first entry (and before the parse of a
             push at point); its result is kept in the context as :prepared
             for :push and :finalize to read.  May signal, which stops the
             run before anything is sent
  :after-heading - Optional function of the run context, run by the push at
                  point and the push by heading only (never by the whole-file
                  sync), after the heading synced, was unchanged or was
                  previewed, with point on it; for state that lives under the
                  heading and is pushed apart from its payload (an
                  assignment's overrides table, issue #380).  Its plist result
                  is merged into the `org-canvas-sync-headings' result, whose
                  :note the closing lines print
  :after-sync - Optional function of the run context, run once after every
                entry is processed, for reconciliation that needs remote
                state and so cannot live in the offline validator.  Must
                not signal.
  :first - Optional sync command the generated sync runs before its own
           pipeline, with the log kept between the two; for a content type
           whose level-1 headings must exist on Canvas before its level-2
           headings can be filed under them (outcomes)
  :no-at-point - When non-nil, suppress generating the sync-at-point function,
                 the sync-by-heading function and the pull-by-heading
                 function (the module writes them)

When :endpoint is provided but :push is not, a push function is auto-generated
that calls `org-canvas--push-to-api' with the given endpoint and options.
When :endpoint is provided but :finalize is not, a finalize function is
auto-generated that calls `org-canvas--finalize-item'.

Three commands come out: org-canvas-sync-FEATURE (every entry),
org-canvas-sync-SINGULAR-at-point (the entry at point) and
org-canvas-sync-SINGULAR (TARGET &optional BY), the entry a title or
a stamp names, for a caller without a point (issue #287); the last is
registered for `org-canvas-sync-headings'.  A feature with a
single-item pull (:pull-item-fn or :pull-heading) also gets
org-canvas-pull-SINGULAR (TARGET &optional BY), which replaces that
heading with Canvas's version and is registered for
`org-canvas-pull-headings' (issue #346).

Example usage:
  (org-canvas-define-sync announcements
    :file org-canvas-announcements-file
    :parse #\\='org-canvas--announcement-parse-entry
    :build #\\='org-canvas--announcement-build-payload
    :endpoint \"discussion_topics\")"
  (declare (indent 1))
  (org-canvas--sync-gen-forms feature args))

(defun org-canvas--sync-gen-spec (feature args)
  "Return the generation spec `org-canvas-define-sync' works from.
FEATURE and ARGS are the macro's arguments.  The spec is a plist of the
resolved names, options and stage functions, with the auto-generated push
and finalize forms already built; the emitters below read it instead of
ARGS, so the macro itself only destructures and splices."
  (let* ((feature-name (symbol-name feature))
         (singular (org-canvas--singularize feature-name))
         (endpoint (plist-get args :endpoint))
         (title-key (plist-get args :title-key))
         (parse-fn (plist-get args :parse))
         (build-fn (plist-get args :build))
         (pull-item-fn (plist-get args :pull-item-fn))
         (hash-extra-fn (plist-get args :hash-extra))
         (hash-fn (plist-get args :hash))
         (prepare-fn (plist-get args :prepare))
         (after-heading-fn (plist-get args :after-heading))
         (push-fn (or (plist-get args :push)
                      (when endpoint
                        (org-canvas--make-push-fn-form
                         endpoint (plist-get args :id-key)
                         title-key (plist-get args :find-fn)))))
         (finalize-fn (or (plist-get args :finalize)
                          (when endpoint
                            (org-canvas--make-finalize-fn-form
                             (plist-get args :id-field)
                             (plist-get args :id-property)
                             title-key (plist-get args :post-fn)
                             endpoint)))))
    (list :feature-name feature-name
          :singular singular
          :sync-fn (intern (format "org-canvas-sync-%s" feature-name))
          :at-point-fn (intern (format "org-canvas-sync-%s-at-point" singular))
          :heading-fn (intern (format "org-canvas-sync-%s" singular))
          :file-expr (plist-get args :file)
          :query (or (plist-get args :query) "LEVEL=1")
          :parse-fn parse-fn :build-fn build-fn
          :push-fn push-fn :finalize-fn finalize-fn
          :pull-item-fn pull-item-fn :pull-heading (plist-get args :pull-heading)
          :title-key title-key
          :hash-extra-fn hash-extra-fn :hash-fn hash-fn
          :dry-run-mode (plist-get args :dry-run)
          :prepare-fn prepare-fn
          :after-sync-fn (plist-get args :after-sync)
          :after-heading-fn after-heading-fn
          :first-fn (plist-get args :first)
          :no-at-point (plist-get args :no-at-point)
          :id-property (plist-get args :id-property)
          ;; The spec a push at point and a push by heading share (issue #287).
          :entry-spec `(list :feature ,singular
                             :parse ,parse-fn :build ,build-fn
                             :push ,push-fn :finalize ,finalize-fn
                             :title-key ,(or title-key :title)
                             :pull-item-fn ,pull-item-fn
                             :hash-extra ,hash-extra-fn :hash ,hash-fn
                             :prepare ,prepare-fn
                             :after-heading ,after-heading-fn))))

(defun org-canvas--sync-gen-check (spec)
  "Check SPEC for an option `org-canvas-define-sync' requires.
The check runs at expansion, so a module that forgets one hears about it
when it loads rather than when its command is first typed."
  (unless (plist-get spec :file-expr)
    (error "org-canvas-define-sync: :file is required"))
  (unless (plist-get spec :parse-fn)
    (error "org-canvas-define-sync: :parse is required"))
  (unless (plist-get spec :build-fn)
    (error "org-canvas-define-sync: :build is required"))
  (unless (plist-get spec :push-fn)
    (error "org-canvas-define-sync: :push or :endpoint is required"))
  (unless (plist-get spec :finalize-fn)
    (error "org-canvas-define-sync: :finalize or :endpoint is required")))

(defun org-canvas--sync-gen-registration-forms (spec)
  "Return the forms registering SPEC's pull-item function, or nil.
The module already names that function for conflict resolution;
recording it on the feature entry is what gives
`org-canvas-pull-at-point' something to dispatch on (issue #67)."
  (let ((pull-item-fn (plist-get spec :pull-item-fn)))
    (when pull-item-fn
      `((org-canvas-register-pull-item-fn ,(plist-get spec :feature-name)
                                          ,pull-item-fn)))))

(defun org-canvas--sync-gen-run-form (spec)
  "Return the whole-file sync form SPEC describes.
That is the pipeline call, wrapped in the log-keeping prefix when the
spec declares a :first command to run ahead of it (outcomes)."
  (let ((run `(org-canvas--sync-run-pipeline
               (list :feature ,(plist-get spec :feature-name)
                     :file (expand-file-name ,(plist-get spec :file-expr))
                     :query ,(plist-get spec :query)
                     :parse ,(plist-get spec :parse-fn)
                     :build ,(plist-get spec :build-fn)
                     :push ,(plist-get spec :push-fn)
                     :finalize ,(plist-get spec :finalize-fn)
                     :pull-item-fn ,(plist-get spec :pull-item-fn)
                     :title-key ,(plist-get spec :title-key)
                     :hash-extra ,(plist-get spec :hash-extra-fn)
                     :hash ,(plist-get spec :hash-fn)
                     :dry-run ,(plist-get spec :dry-run-mode)
                     :prepare ,(plist-get spec :prepare-fn)
                     :after-sync ,(plist-get spec :after-sync-fn))))
        (first-fn (plist-get spec :first-fn)))
    (if first-fn
        `(progn
           (org-canvas-clear-log)
           (let ((org-canvas--inhibit-log-clear t))
             (funcall ,first-fn)
             ,run))
      run)))

(defun org-canvas--sync-gen-sync-fn (spec)
  "Return the forms defining SPEC's whole-file sync command."
  (let ((feature-name (plist-get spec :feature-name))
        (first-fn (plist-get spec :first-fn)))
    `(;;;###autoload
      (defun ,(plist-get spec :sync-fn) ()
        ,(format "Synchronize %s to Canvas using the 4-stage pipeline.%s" feature-name
                 (if first-fn (format "\nRuns `%s' first." (cadr first-fn)) ""))
        (interactive)
        ,(org-canvas--sync-gen-run-form spec)))))

(defun org-canvas--sync-gen-heading-docstring (spec)
  "Return the docstring SPEC's push-by-heading command carries."
  (let ((file-expr (plist-get spec :file-expr)))
    (format "Sync the %s heading TARGET names to Canvas (issue #287).
TARGET is the heading's exact title in %s, or, with BY
`canvas-id', its %s; an error names a target that matches no
heading or more than one.  Nil asks for a title (never under
`noninteractive').  Return the run context; its :outcome is `synced',
`unchanged', `conflict', `pulled', `duplicate', `fragment' or
`dry-run'.  The at-point command does the same at point; this one finds the heading,
so a script need not (`org-canvas-sync-headings' takes several)."
            (plist-get spec :singular)
            (if (symbolp file-expr)
                (format "the file `%s' names" file-expr)
              "the module's file")
            (or (plist-get spec :id-property) "CANVAS_ID"))))

(defun org-canvas--sync-gen-at-point-forms (spec)
  "Return the forms defining SPEC's at-point and by-heading commands.
Nil when the module writes those commands itself (:no-at-point)."
  (unless (plist-get spec :no-at-point)
    (let ((singular (plist-get spec :singular))
          (heading-fn (plist-get spec :heading-fn))
          (entry-spec (plist-get spec :entry-spec))
          (file-expr (plist-get spec :file-expr)))
      `(;;;###autoload
        (defun ,(plist-get spec :at-point-fn) ()
          ,(format "Sync the %s at point to Canvas." singular)
          (interactive)
          (org-canvas--push-at-point-runtime ,entry-spec))
        ;;;###autoload
        (defun ,heading-fn (&optional target by)
          ,(org-canvas--sync-gen-heading-docstring spec)
          (interactive)
          (org-canvas--sync-heading-runtime
           (append ,entry-spec
                   (list :file (expand-file-name ,file-expr)
                         :query ,(plist-get spec :query)
                         :id-property ,(plist-get spec :id-property)))
           target by))
        (org-canvas--sync-register-heading-fn ,singular #',heading-fn)
        (org-canvas--sync-register-entry-spec ,singular ,entry-spec)))))

(defun org-canvas--sync-gen-pull-forms (spec)
  "Return the pull-by-heading forms SPEC asks for, or nil.
A feature with a single-item pull (:pull-item-fn or :pull-heading) gets
one; :no-at-point means the module writes that too."
  (when (and (or (plist-get spec :pull-item-fn) (plist-get spec :pull-heading))
             (not (plist-get spec :no-at-point)))
    (org-canvas--pull-heading-fn-forms
     (plist-get spec :feature-name) (plist-get spec :singular)
     (plist-get spec :file-expr) (plist-get spec :query)
     (plist-get spec :id-property))))

(defun org-canvas--sync-gen-forms (feature args)
  "Return the forms `org-canvas-define-sync' emits for FEATURE and ARGS.
This is the sync DSL's expander proper; the macro is a thin expander
over it, so a module's expansion can be stepped through here in the
debugger instead of being buried inside one macro body."
  (let ((spec (org-canvas--sync-gen-spec feature args)))
    (org-canvas--sync-gen-check spec)
    `(progn
       ,@(org-canvas--sync-gen-registration-forms spec)
       ,@(org-canvas--sync-gen-sync-fn spec)
       ,@(org-canvas--sync-gen-at-point-forms spec)
       ,@(org-canvas--sync-gen-pull-forms spec))))

(provide 'org-canvas-core-sync)
;;; org-canvas-core-sync.el ends here
