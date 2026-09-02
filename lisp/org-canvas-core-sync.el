;;; org-canvas-core-sync.el --- Sync pipeline, conflict, push/delete infrastructure -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Sync pipeline macros (`org-canvas-define-sync', `org-canvas-define-push-at-point'),
;; conflict detection/resolution, generic push-to-API with error recovery,
;; delete infrastructure, setup wizard, and file upload.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)
(require 'org-canvas-core-macros)
(require 'org-canvas-core-conflict)
(require 'org-canvas-core-delete)

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
  (let ((buf (find-file-noselect sync-file)))
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
Returns a plist (:targets MARKERS :all-ids-before IDS)."
  (let (targets all-ids-before)
    (with-current-buffer (find-file-noselect sync-file)
      (setq targets (org-map-entries (lambda () (point-marker)) query 'file))
      (setq all-ids-before
            (org-map-entries
             (lambda () (org-entry-get (point) "CANVAS_ID"))
             "CANVAS_ID={.}" 'file)))
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
PAYLOAD-HASH is saved to the heading.  CTX is the sync context plist."
  (let ((finalize-fn (plist-get ctx :finalize-fn))
        (counters (plist-get ctx :counters))
        (synced-ids (plist-get ctx :synced-ids))
        (canvas-id (or (plist-get data :canvas-id)
                       (plist-get data :canvas-url)))
        (cap-feature (capitalize (plist-get ctx :feature-name)))
        (title (plist-get data (or (plist-get ctx :title-key) :title)))
        (total-count (plist-get ctx :total-count))
        (progress (+ (plist-get (plist-get ctx :counters) :success)
                     (plist-get (plist-get ctx :counters) :skip)
                     (plist-get (plist-get ctx :counters) :fail)
                     1)))
    (funcall finalize-fn data response)
    (org-canvas--sync-note-remote-time response ctx)
    (org-canvas-org-set-property (point) org-canvas--prop-payload-hash payload-hash)
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
  (let* ((push-fn (plist-get ctx :push-fn))
         (feature-name (plist-get ctx :feature-name))
         (total-count (plist-get ctx :total-count))
         (counters (plist-get ctx :counters))
         (synced-ids (plist-get ctx :synced-ids))
         (payload-hash (org-canvas--sync-payload-hash
                        payload data (plist-get ctx :hash-extra-fn)))
         (stored-hash (org-entry-get (point) org-canvas--prop-payload-hash))
         (canvas-id (or (plist-get data :canvas-id)
                        (plist-get data :canvas-url)))
         (title (plist-get data (or (plist-get ctx :title-key) :title)))
         (cap-feature (capitalize feature-name))
         (progress (+ (plist-get counters :success)
                      (plist-get counters :skip)
                      (plist-get counters :fail)
                      1)))
    (when canvas-id
      (push canvas-id (car synced-ids)))
    (cond
     ((and stored-hash (string= payload-hash stored-hash) canvas-id
           (not (org-canvas--sync-remote-drifted-p canvas-id ctx title)))
      (plist-put counters :skip (1+ (plist-get counters :skip)))
      (org-canvas--log-info org-canvas--logger "[Skip] '%s' unchanged" title)
      (org-canvas--sync-backfill-baseline canvas-id title ctx)
      (message "%s [%d/%d] Skipping '%s' (unchanged)"
        cap-feature progress total-count title))
     (org-canvas--dry-run
      (org-canvas--sync-dry-run-entry canvas-id title ctx))
     (t
      (org-canvas--sync-handle-push-response
       (funcall push-fn data payload) data payload-hash ctx progress)))))

(defun org-canvas--sync-handle-push-response (response data payload-hash ctx progress)
  "Count RESPONSE from the push of DATA, or finalize it.
A symbol means the push stopped short: `conflict' and `pulled' come
from the conflict prompt, `duplicate' from the create guard (issue
#85), which names the entry among the skipped so the summary says
where it went.  Anything else is the API response, finalized with
PAYLOAD-HASH.  CTX is the sync context, PROGRESS the 1-based position
for the echo-area line."
  (let* ((counters (plist-get ctx :counters))
         (cap-feature (capitalize (plist-get ctx :feature-name)))
         (total-count (plist-get ctx :total-count))
         (title (plist-get data (or (plist-get ctx :title-key) :title))))
    (pcase response
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
  (let* ((counters (plist-get ctx :counters))
         (cap-feature (capitalize (plist-get ctx :feature-name)))
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

(defun org-canvas--sync-process-entry (marker ctx)
  "Process one entry through the 4-stage pipeline.
MARKER is the position of the entry.
CTX is a plist with keys:
  :parse-fn, :build-fn, :push-fn, :finalize-fn - pipeline stage functions
  :feature-name, :feature-upper - for log messages
  :total-count - total entries being processed
  :counters - plist (:success N :skip N :fail N) mutated in place
  :synced-ids - list (mutated via push) of processed CANVAS_IDs
  :title-key - plist key for display name (default :title)"
  (let ((parse-fn (plist-get ctx :parse-fn))
        (build-fn (plist-get ctx :build-fn))
        (feature-upper (plist-get ctx :feature-upper))
        (feature-name (plist-get ctx :feature-name))
        (total-count (plist-get ctx :total-count))
        (counters (plist-get ctx :counters)))
    (org-canvas--log-info org-canvas--logger "----------------------------------------")
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (let ((heading-title (org-get-heading t t t t)))
          (condition-case err
              (let* ((data (funcall parse-fn))
                     (payload (funcall build-fn data)))
                (org-canvas--sync-execute-pipeline data payload ctx))
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
    (with-current-buffer (find-file-noselect sync-file)
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

(defun org-canvas--sync-reclassify-skip-as-success (label title-match)
  "Move one skip to success in LABEL's stats entry and the aggregates.
Removes the first :skipped-titles entry containing TITLE-MATCH.  Used
by the end-of-run retry pass when a previously skipped item syncs
after all.  No-op unless a global sync is active."
  (when org-canvas--sync-global-counters
    (plist-put org-canvas--sync-global-counters :skip
               (max 0 (1- (or (plist-get org-canvas--sync-global-counters :skip) 0))))
    (plist-put org-canvas--sync-global-counters :success
               (1+ (or (plist-get org-canvas--sync-global-counters :success) 0)))
    (let ((entry (cl-find label org-canvas--sync-global-feature-stats
                          :key (lambda (e) (plist-get e :label))
                          :test #'equal)))
      (when entry
        (plist-put entry :skip (max 0 (1- (plist-get entry :skip))))
        (plist-put entry :success (1+ (plist-get entry :success)))
        (plist-put entry :skipped-titles
                   (cl-remove-if (lambda (x)
                                   (string-match-p (regexp-quote title-match) x))
                                 (plist-get entry :skipped-titles)
                                 :count 1))))))

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
Returns a quoted lambda that calls `org-canvas--push-to-api'
with ENDPOINT and optional ID-KEY, TITLE-KEY, FIND-FN."
  `(lambda (data payload)
     (org-canvas--push-to-api data payload
       :endpoint ,endpoint
       ,@(when id-key `(:id-key ,id-key))
       ,@(when title-key `(:title-key ,title-key))
       ,@(when find-fn `(:find-fn ,find-fn)))))

(defun org-canvas--make-finalize-fn-form (id-field id-property title-key post-fn)
  "Build a finalize lambda form for endpoint-based sync macros.
Returns a quoted lambda that calls `org-canvas--finalize-item'
with optional ID-FIELD, ID-PROPERTY, TITLE-KEY, POST-FN."
  `(lambda (data response)
     (org-canvas--finalize-item data response
       ,@(when id-field `(:id-field ,id-field))
       ,@(when id-property `(:id-property ,id-property))
       ,@(when title-key `(:title-key ,title-key))
       ,@(when post-fn `(:post-fn ,post-fn)))))

(defun org-canvas--sync-run-pipeline (feature-name sync-file query
                                                   parse-fn build-fn push-fn
                                                   finalize-fn
                                                   &optional pull-item-fn title-key
                                                   hash-extra-fn after-sync-fn)
  "Run the full sync pipeline for FEATURE-NAME.
SYNC-FILE is the expanded org file path.  QUERY is the org match query.
PARSE-FN, BUILD-FN, PUSH-FN, FINALIZE-FN are the 4-stage pipeline functions.
PULL-ITEM-FN enables interactive conflict pull.  TITLE-KEY is the plist key
for the display name in logs.  HASH-EXTRA-FN, when non-nil, is called with
the parsed data and its string result is folded into the payload hash
\(see `org-canvas--sync-payload-hash').

AFTER-SYNC-FN, when non-nil, is called with no arguments once every entry
has been processed, just before the summary.  It is the place for checks
that need remote state and so cannot live in the offline validator — it
must not signal, since a reconciliation problem should never fail a sync
that otherwise succeeded."
  (org-canvas-clear-log)
  (let ((org-canvas--conflict-apply-all nil)
        (org-canvas--duplicate-apply-all nil)
        (org-canvas--current-pull-item-fn pull-item-fn)
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
           ;; `none' rather than nil: inside a sync the create guard reads
           ;; this or nothing, and must not fall back to a GET per entry.
           (org-canvas--current-remote-titles (or (plist-get snapshot :titles) 'none))
           (ctx (list :baseline baseline
                      :remote-updated (plist-get snapshot :updated)
                      :remote-titles (plist-get snapshot :titles)
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
                      :hash-extra-fn hash-extra-fn)))
      (dolist (marker targets)
        (org-canvas--sync-process-entry marker ctx))
      (dolist (m targets) (set-marker m nil))
      (unless org-canvas--dry-run
        (org-canvas--sync-write-push-header sync-file ctx))
      (org-canvas--sync-warn-unverified-skips feature-name counters ctx)
      (org-canvas--sync-warn-orphans all-ids-before (car synced-ids) feature-name)
      (when after-sync-fn
        (funcall after-sync-fn))
      (org-canvas--sync-log-summary feature-name sync-file counters))))

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

(defmacro org-canvas-define-sync (feature &rest args)
  "Define a sync function for FEATURE using the 4-stage pipeline pattern.

FEATURE is a symbol like \\='pages or \\='announcements.

ARGS is a plist with the following keys:
  :file - Expression that evaluates to the org file path (required)
  :query - Org match query for entries (default: \"LEVEL=1\")
  :parse - Function to parse entry at point (required)
  :build - Function to build payload from parsed data (required)
  :push - Push function (or auto-generated from :endpoint)
  :finalize - Finalize function (or auto-generated from :endpoint)
  :endpoint - API endpoint suffix; auto-generates :push/:finalize
  :id-key - Plist key for Canvas ID in data (default: :canvas-id)
  :id-field - Alist key for ID in API response (default: \\='id)
  :id-property - Org property name for ID (default: \"CANVAS_ID\")
  :find-fn - Search function for timeout recovery (used by auto-generated :push)
  :post-fn - Callback after finalize (used by auto-generated :finalize)
  :title-key - Plist key for display name in logs (default: :title)
  :pull-item-fn - Optional function to pull remote data into local heading
                  for interactive conflict resolution
  :hash-extra - Optional function called with the parsed data; its string
                result is folded into the payload hash so state outside the
                payload (e.g. module items) participates in change detection
  :after-sync - Optional function of no arguments run once after every entry
                is processed, for reconciliation that needs remote state and
                so cannot live in the offline validator.  Must not signal.
  :no-at-point - When non-nil, suppress generating the sync-at-point function

When :endpoint is provided but :push is not, a push function is auto-generated
that calls `org-canvas--push-to-api' with the given endpoint and options.
When :endpoint is provided but :finalize is not, a finalize function is
auto-generated that calls `org-canvas--finalize-item'.

Example usage:
  (org-canvas-define-sync announcements
    :file org-canvas-announcements-file
    :parse #\\='org-canvas--announcement-parse-entry
    :build #\\='org-canvas--announcement-build-payload
    :endpoint \"discussion_topics\")"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (sync-fn-name (intern (format "org-canvas-sync-%s" feature-name)))
         (file-expr (plist-get args :file))
         (query (or (plist-get args :query) "LEVEL=1"))
         (parse-fn (plist-get args :parse))
         (build-fn (plist-get args :build))
         (endpoint (plist-get args :endpoint))
         (title-key (plist-get args :title-key))
         (pull-item-fn (plist-get args :pull-item-fn))
         (hash-extra-fn (plist-get args :hash-extra))
         (after-sync-fn (plist-get args :after-sync))
         (no-at-point (plist-get args :no-at-point))
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
                             title-key (plist-get args :post-fn)))))
         (singular (org-canvas--singularize feature-name))
         (at-point-fn-name (intern (format "org-canvas-sync-%s-at-point" singular))))
    (unless file-expr (error "org-canvas-define-sync: :file is required"))
    (unless parse-fn (error "org-canvas-define-sync: :parse is required"))
    (unless build-fn (error "org-canvas-define-sync: :build is required"))
    (unless push-fn (error "org-canvas-define-sync: :push or :endpoint is required"))
    (unless finalize-fn (error "org-canvas-define-sync: :finalize or :endpoint is required"))
    `(progn
       ;; The module already names its pull-item function for conflict
       ;; resolution; recording it on the feature entry is what gives
       ;; `org-canvas-pull-at-point' something to dispatch on (issue #67).
       ,@(when pull-item-fn
           `((org-canvas-register-pull-item-fn ,feature-name ,pull-item-fn)))
       ;;;###autoload
       (defun ,sync-fn-name ()
         ,(format "Synchronize %s to Canvas using the 4-stage pipeline." feature-name)
         (interactive)
         (org-canvas--sync-run-pipeline
          ,feature-name (expand-file-name ,file-expr)
          ,query ,parse-fn ,build-fn ,push-fn ,finalize-fn
          ,pull-item-fn ,title-key ,hash-extra-fn ,after-sync-fn))
       ,@(unless no-at-point
           `(;;;###autoload
             (defun ,at-point-fn-name ()
               ,(format "Sync the %s at point to Canvas." singular)
               (interactive)
               (org-canvas--push-at-point-runtime
                ,singular
                ,parse-fn ,build-fn ,push-fn ,finalize-fn
                ,(or title-key :title) ,pull-item-fn ,hash-extra-fn)))))))

;;;; 6a. Remote Drift Detection
;;
;; A payload-hash match proves the local file has not changed since the
;; last push.  It says nothing about Canvas.  Before this, an item edited
;; only in the web UI was skipped before any remote comparison happened —
;; the per-item conflict check lives inside the push, which the skip
;; branch never reaches — so the divergence was permanent and silent
;; (issue #48).  One list request per feature closes that hole.

(defun org-canvas--sync-note-remote-time (response ctx)
  "Record RESPONSE's `updated_at' in CTX's remote-time accumulator.
The accumulator is absent for single-entry pushes, where there is no
file-level header to write afterwards."
  (let ((times-ref (plist-get ctx :remote-times))
        (updated (and (listp response) (alist-get 'updated_at response))))
    (when (and times-ref (stringp updated))
      (push updated (car times-ref)))))

(defun org-canvas--sync-file-baseline (sync-file)
  "Return SYNC-FILE\\='s #+LAST_SYNCED header as an Emacs time, or nil."
  (when (file-exists-p sync-file)
    (with-current-buffer (find-file-noselect sync-file)
      (let ((ts (org-canvas--pull-read-file-header)))
        (when ts (encode-time (org-parse-time-string ts)))))))

(defun org-canvas--sync-remote-updated-index (items id-field &optional modified-field)
  "Return a hash of each of ITEMS' ID-FIELD, as a string, to its modification time.
MODIFIED-FIELD names the alist key that holds it, default `updated_at'.
Files read `modified_at', the content timestamp — Canvas bumps their
`updated_at' on metadata-only touches, which must not count as drift
\(issue #94)."
  (let ((field (or modified-field 'updated_at))
        (map (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((id (alist-get id-field item))
            (updated (alist-get field item)))
        (when (and id (stringp updated))
          (puthash (format "%s" id) updated map))))
    map))

(defun org-canvas--sync-remote-title-index (items title-field)
  "Return a hash of each of ITEMS' TITLE-FIELD to the items carrying it.
Titles are not unique on Canvas, so a value is a list, in list order;
a caller that wants to adopt an id must check there is exactly one."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((title (alist-get title-field item)))
        (when (stringp title)
          (puthash title (append (gethash title map) (list item)) map))))
    map))

(defun org-canvas--sync-fetch-remote-snapshot (feature-name)
  "Return FEATURE-NAME's remote items, indexed two ways, or nil.
One list request per feature.  The result is a plist:

  :updated  remote id to `updated_at', consulted before a payload-hash
            skip so an item changed only on Canvas is noticed instead
            of skipped (issue #48)
  :titles   remote title to the items carrying it, consulted before a
            POST so a heading that lost its stamp is not created a
            second time (issue #85)

Both key by the feature's registered fields, so pages key on `url' to
match their CANVAS_URL and assignments on `name'.

Returns nil when the feature is not in the registry or the request
fails; the caller then falls back to skipping unverified and says so."
  (let ((feature (org-canvas--registry-find-feature feature-name)))
    (when feature
      (condition-case err
          (let ((items (append (org-canvas-api-request-all-pages
                                'GET (org-canvas--feature-list-url feature)
                                (org-canvas--feature-list-params feature))
                               nil)))
            (list :updated (org-canvas--sync-remote-updated-index
                            items (or (plist-get feature :id-field) 'id)
                            (org-canvas--feature-modified-field feature))
                  :titles (org-canvas--sync-remote-title-index
                           items (or (plist-get feature :title-field) 'title))))
        (error
         (org-canvas--log-warning org-canvas--logger
           "[Conflict] Could not fetch the remote %s snapshot (%s); unchanged entries will be skipped without checking Canvas, and creates will not be checked for a title Canvas already holds"
           feature-name (error-message-string err))
         nil)))))

(defun org-canvas--sync-fetch-remote-updated (feature-name)
  "Return a hash of remote id to `updated_at' for FEATURE-NAME, or nil.
The `:updated' half of `org-canvas--sync-fetch-remote-snapshot'."
  (plist-get (org-canvas--sync-fetch-remote-snapshot feature-name) :updated))

(defun org-canvas--sync-remote-newer-at (canvas-id ctx)
  "Return the remote `updated_at' of CANVAS-ID if it postdates the baseline.
Called with point on the entry's heading.  CTX carries the remote
snapshot and the file-level fallback baseline.  Nil when there is no
snapshot, the snapshot has no entry for CANVAS-ID, the entry has no
baseline, or Canvas has not touched the item since."
  (let* ((remote-updated (plist-get ctx :remote-updated))
         (updated (and remote-updated canvas-id
                       (gethash (format "%s" canvas-id) remote-updated)))
         (baseline (and updated
                        (org-canvas--conflict-baseline
                         (point) (plist-get ctx :baseline))))
         (remote-time (and baseline (org-canvas--parse-iso8601-time updated))))
    (when (and remote-time (time-less-p baseline remote-time))
      updated)))

(defun org-canvas--sync-remote-drifted-p (canvas-id ctx title)
  "Return non-nil when CANVAS-ID was modified on Canvas after the baseline.
Called with point on the entry's heading.  CTX carries the remote
snapshot and the file-level fallback baseline; TITLE is used for the
log line.  A drifted entry must not take the
unchanged-skip: letting it fall through to the push puts it back on the
normal path, where `org-canvas--push-check-and-resolve-conflict\' shows
the diff and offers push/pull/skip."
  (let ((updated (org-canvas--sync-remote-newer-at canvas-id ctx)))
    (when updated
      (org-canvas--log-warning org-canvas--logger
        "[Drift] '%s' is unchanged locally but Canvas has it as updated at %s — comparing"
        title updated)
      t)))

(defun org-canvas--sync-remote-items-titled (title ctx)
  "Return the remote items in CTX's snapshot that carry TITLE, or nil."
  (let ((titles (plist-get ctx :remote-titles)))
    (and titles title (gethash title titles))))

(defun org-canvas--sync-warn-unverified-skips (feature-name counters ctx)
  "Warn when FEATURE-NAME entries were skipped without checking Canvas.
COUNTERS supplies the skip tally and CTX the remote snapshot that
should have verified them.  A payload-hash skip only proves the local
file is unchanged.  When the
remote snapshot is missing — no #+LAST_SYNCED baseline yet, or the list
request failed — say so, rather than let a \"0 failed\" summary imply a
comparison that never happened (issue #48)."
  (let ((skips (plist-get counters :skip))
        (previews (if org-canvas--dry-run (or (plist-get counters :dry-run) 0) 0))
        (why (if (plist-get ctx :baseline)
                 "the remote snapshot was unavailable"
               "this file has no #+LAST_SYNCED baseline yet (one is written after this run)")))
    (when (and org-canvas-detect-conflicts
               (not (plist-get ctx :remote-updated)))
      (when (> skips 0)
        (org-canvas--log-warning org-canvas--logger
          "[Conflict] %d %s entr%s skipped as unchanged were not verified against Canvas — %s"
          skips feature-name (if (= skips 1) "y" "ies") why))
      ;; A dry run answers "would this conflict?" from the same snapshot
      ;; (issue #84), so without one its would-sync lines are unverified too.
      (when (> previews 0)
        (org-canvas--log-warning org-canvas--logger
          "[DRY-RUN] %d %s entr%s reported as would-sync were not checked for conflicts — %s"
          previews feature-name (if (= previews 1) "y" "ies") why)))))

(defun org-canvas--sync-write-push-header (sync-file ctx)
  "Record a sync baseline in SYNC-FILE\\='s #+LAST_SYNCED header.
CTX carries the remote timestamps collected during the run.
Only pull used to write this header, so a course authored in Org and
pushed never acquired one and conflict detection stayed permanently
inert (issue #48).

The stamp comes from the newest `updated_at\\=' Canvas returned during
this run, not from the local clock: the header is only ever compared
against remote timestamps, and a client running behind the server would
otherwise make every item we just pushed look remotely modified on the
next run.  It is rounded up to the next whole minute because Org
timestamps carry no seconds — rounding down would place the header
before our own pushes and produce exactly those false conflicts.  The
cost is that a remote edit in the same minute as a push is not seen.

The write is forward-only (`org-canvas--sync-advance-file-header'),
and single-entry pushes advance the same header (issue #104)."
  (let* ((times (car (plist-get ctx :remote-times)))
         (newest (car (sort (copy-sequence times) #'string>)))
         (time (and newest (org-canvas--parse-iso8601-time newest))))
    (when time
      (with-current-buffer (find-file-noselect sync-file)
        (org-canvas--sync-advance-file-header time)
        (org-canvas--save-buffer)))))

(defun org-canvas--sync-advance-file-header (time)
  "Move the current buffer's #+LAST_SYNCED header forward to TIME.
TIME is an Emacs time, normally a remote `updated_at' Canvas has just
returned; it is rounded up to the next whole minute for the reason
`org-canvas--sync-write-push-header' gives.  Forward-only: a header
already at or past that minute is left alone, since a baseline that
moves backward can only manufacture conflicts.  Returns the stamp
written, or nil when nothing changed.  Does not save."
  (let* ((stamp-time (time-add time 60))
         (stamp (format-time-string "[%Y-%m-%d %a %H:%M]" stamp-time))
         (new-time (encode-time (org-parse-time-string stamp)))
         (current (org-canvas--pull-read-file-header))
         (current-time (and current
                            (ignore-errors
                              (encode-time (org-parse-time-string current))))))
    (when (or (null current-time) (time-less-p current-time new-time))
      (org-canvas--pull-write-file-header stamp-time)
      stamp)))

(defun org-canvas--sync-advance-header-from-entry ()
  "Advance the file header from the CANVAS_UPDATED_AT of the entry at point.
Called once a single-entry push has been finalized.  A full sync
writes #+LAST_SYNCED at the end of the run and nothing else did, so a
file maintained by at-point syncs carried a header weeks behind the
entries in it — every heading pushed and re-stamped, the first line
still naming a date 13 days gone (issue #104).  The entry's
CANVAS_UPDATED_AT is whatever finalize just stamped, so this needs no
knowledge of which response field the module reads.  Returns the
stamp written, or nil."
  (let ((updated (org-canvas--parse-iso8601-time
                  (org-entry-get (point) "CANVAS_UPDATED_AT"))))
    (when updated
      (let ((stamp (org-canvas--sync-advance-file-header updated)))
        (when stamp
          (org-canvas--log-info org-canvas--logger
            "[Finalize] #+LAST_SYNCED advanced to %s" stamp))
        stamp))))

(defun org-canvas--sync-backfill-baseline (canvas-id title ctx)
  "Give the unchanged entry at point its own CANVAS_UPDATED_AT, if it has none.
Called on the skip path once `org-canvas--sync-remote-drifted-p' has
found CANVAS-ID untouched since the baseline in CTX; TITLE is for the
log.  An entry synced before CANVAS_UPDATED_AT existed leans on the
file-level #+LAST_SYNCED header for its baseline, and that header now
advances on any successful push in the file (issue #104), including
one that never compared this entry.  The snapshot holds the remote
`updated_at' this run has just checked against a real baseline, so
recording it is exactly as sound as the skip itself, and from then on
the entry judges drift by its own clock.  Nothing is written when the
entry is already stamped, the snapshot has no entry for it, there was
no baseline (nothing was proven), or this is a dry run."
  (let ((remote-updated (plist-get ctx :remote-updated)))
    (when (and remote-updated
               (plist-get ctx :baseline)
               (not org-canvas--dry-run)
               (null (org-entry-get (point) "CANVAS_UPDATED_AT")))
      (let ((updated (gethash (format "%s" canvas-id) remote-updated)))
        (when updated
          (org-canvas-org-set-property (point) "CANVAS_UPDATED_AT" updated)
          (org-canvas--log-info org-canvas--logger
            "[Skip] Recorded CANVAS_UPDATED_AT %s for '%s' from the remote snapshot"
            updated title))))))

;;;; 6b. Conflict Detection
;;
;; Before overwriting a Canvas item (PUT), check if someone edited it
;; remotely.  Compare the item's `updated_at' from a fresh GET with the
;; local `LAST_SYNCED' timestamp.  If Canvas is newer, warn and skip.

(defun org-canvas--parse-iso8601-time (iso8601)
  "Parse ISO8601 timestamp string to an Emacs time value.
Returns nil if ISO8601 is nil or :null."
  (when (and iso8601 (not (eq iso8601 :null)) (stringp iso8601))
    (date-to-time iso8601)))

(defun org-canvas--last-synced-header (pom)
  "Return the file-level #+LAST_SYNCED header of POM's buffer, or nil.
POM is a marker, or a position in the current buffer."
  (let ((buf (cond ((markerp pom) (marker-buffer pom))
                   ((and (numberp pom) (buffer-live-p (current-buffer)))
                    (current-buffer)))))
    (when buf
      (with-current-buffer buf
        (org-canvas--pull-read-file-header)))))

(defun org-canvas--parse-last-synced (pom)
  "Parse the file-level #+LAST_SYNCED header to an Emacs time value.
POM is a marker or position in the buffer to query.  Returns nil
if no #+LAST_SYNCED header exists in that buffer."
  (let ((ts (org-canvas--last-synced-header pom)))
    (when ts
      (encode-time (org-parse-time-string ts)))))

(defun org-canvas--conflict-baseline (pom &optional fallback)
  "Return the time the entry at POM was last known to agree with Canvas.
Prefers the entry's own CANVAS_UPDATED_AT — the remote `updated_at'
recorded when it was last pushed or pulled.  That is Canvas's own
clock, so it needs no allowance for skew, and crucially it exists on
courses that are only ever pushed, which never acquire the file-level
#+LAST_SYNCED header the check used to depend on (issue #48).

Falls back to FALLBACK when given, otherwise to that header, for
entries synced before CANVAS_UPDATED_AT was recorded."
  (car (org-canvas--conflict-baseline-source pom fallback)))

(defun org-canvas--conflict-baseline-source (pom &optional fallback)
  "Return (TIME . LABEL) for the baseline of the entry at POM, or nil.
TIME is what `org-canvas--conflict-baseline' returns and LABEL says
where it came from — the entry's own CANVAS_UPDATED_AT, or the
file-level #+LAST_SYNCED header (FALLBACK, already parsed, when the
caller supplies it) for an entry that has none.  The conflict line
used to print the header no matter which was compared, and nil for
any POM that was not a marker, which sent a reader checking a header
that was fine (issue #86)."
  (let* ((own (org-entry-get pom "CANVAS_UPDATED_AT"))
         (own-time (org-canvas--parse-iso8601-time own))
         (header (unless (or own-time fallback)
                   (org-canvas--last-synced-header pom))))
    (cond
     (own-time (cons own-time (format "CANVAS_UPDATED_AT %s" own)))
     (fallback
      (cons fallback
            (format "#+LAST_SYNCED %s (entry has no CANVAS_UPDATED_AT)"
                    (format-time-string "[%Y-%m-%d %a %H:%M]" fallback))))
     (header
      (cons (encode-time (org-parse-time-string header))
            (format "#+LAST_SYNCED %s (entry has no CANVAS_UPDATED_AT)" header))))))

(cl-defun org-canvas--conflict-check (endpoint id pom &optional title modified-field)
  "Check if the remote item at ENDPOINT/ID was modified after the baseline at POM.
TITLE names the entry in the log line; without it ENDPOINT/ID does.
MODIFIED-FIELD names the response field that tracks content
modification, default `updated_at'; files pass `modified_at', because
Canvas bumps their `updated_at' on metadata-only touches and comparing
it re-flagged files whose bytes never changed (issue #94).
Returns (cons \\='conflict REMOTE-RESPONSE) if the remote item is newer,
nil otherwise.  Returns nil on GET failure (allows push to proceed) or
when there is no baseline at all (first sync).

The log line names the baseline actually compared — the entry's
CANVAS_UPDATED_AT, or the file header when it has none — rather than
the header regardless (issue #86)."
  (let* ((source (org-canvas--conflict-baseline-source pom))
         (local-time (car source)))
    (unless local-time
      (cl-return-from org-canvas--conflict-check nil))
    (condition-case err
        (let* ((field (or modified-field 'updated_at))
               (full-url (org-canvas-api-course-endpoint
                          (format "%s/%%s" endpoint) id))
               (response (org-canvas-api-request 'GET full-url))
               (updated-at (alist-get field response))
               (remote-time (org-canvas--parse-iso8601-time updated-at)))
          (if (and remote-time (time-less-p local-time remote-time))
              (progn
                (org-canvas--log-warning org-canvas--logger
                  "[Conflict] '%s': remote %s %s is newer than %s"
                  (or title (format "%s/%s" endpoint id)) field updated-at
                  (cdr source))
                (cons 'conflict response))
            nil))
      (error
       ;; A failed remote check must not be silent: proceeding with the push
       ;; could overwrite remote changes the user never saw.
       (org-canvas--log-warning org-canvas--logger
         "[Conflict] Remote check for %s/%s failed (%s); proceeding with push without conflict detection"
         endpoint id (error-message-string err))
       nil))))

;;;; 7. Push-to-API Infrastructure
;;
;; These helpers standardize the "Execute" stage of the pipeline.
;; They handle common error recovery scenarios:
;;
;;   - Timeout: The request timed out but may have succeeded on the server.
;;     Uses find-fn to search for the item by title.
;;
;;   - 404 on PUT: The CANVAS_ID in our Org file is stale (item was deleted
;;     from Canvas). Automatically retries as POST to create a new item.
;;
;; Use `org-canvas--push-to-api' for most feature modules.
;; Use `org-canvas--search-item' when you need custom search logic.

(defun org-canvas--timeout-error-p (err)
  "Return non-nil if ERR represents a timeout.
ERR is a `condition-case' error value.  Check both the message and
error-thrown fields since different callers place the timeout
indicator in different positions."
  (or (let ((error-thrown (caddr err)))
        (and error-thrown (string-match-p "[Tt]imeout\\|timed out"
                                         (format "%s" error-thrown))))
      (let ((err-msg (error-message-string err)))
        (and err-msg (string-match-p "[Tt]imeout\\|timed out" err-msg)))))

(defun org-canvas--404-error-p (err)
  "Return non-nil if ERR represents an HTTP 404 response.
ERR is a `condition-case' error value."
  (string-match-p "404" (error-message-string err)))

(defun org-canvas--sync-deferred-error-p (err)
  "Return non-nil when ERR is a Canvas rejection that self-resolves later.
Currently matches drop rules exceeding the group's assignment count
\(\"Drop rules cannot be higher than the number of assignments\"),
which succeeds on a future sync once the group has enough assignments.
ERR is a `condition-case' error value."
  (string-match-p "drop rules cannot be higher"
                  (downcase (error-message-string err))))

(defun org-canvas--404-on-put-p (err method)
  "Return non-nil if ERR is a 404 and METHOD is PUT or PATCH.
ERR is a `condition-case' error value."
  (and (memq method '(PUT PATCH))
       (org-canvas--404-error-p err)))

(cl-defun org-canvas--search-item (endpoint title &key params match-field)
  "Search for an item on Canvas by title.

ENDPOINT is the API endpoint suffix (e.g., \"pages\" or \"assignments\").
TITLE is the value to search for.

Keyword arguments:
  PARAMS - Additional params for GET request (default adds search_term).
  MATCH-FIELD - Alist key to match against TITLE (default: \\='title).

Return the matching item alist, or nil if not found."
  (let ((match-field (or match-field 'title)))
    (org-canvas--log-info org-canvas--logger "[Search] Looking for item with %s='%s'..." match-field title)
    (condition-case err
        (let* ((full-endpoint (org-canvas-api-course-endpoint endpoint))
               (search-params (append (or params `(("search_term" . ,title))) nil))
               (results (append (org-canvas-api-request 'GET full-endpoint :params search-params) nil))
               (count (length results)))
          (org-canvas--log-debug org-canvas--logger "[Search] Found %d results" count)
          (let ((found (cl-loop for item in results
                                when (string-equal (alist-get match-field item) title)
                                return item)))
            (if found
                (org-canvas--log-info org-canvas--logger "[Search] Found exact match: ID=%s"
                  (or (alist-get 'id found) (alist-get 'url found)))
              (org-canvas--log-debug org-canvas--logger "[Search] No exact match found"))
            found))
      (error
       (org-canvas--log-warning org-canvas--logger "[Search] Failed: %s" (error-message-string err))
       nil))))

(defun org-canvas--handle-timeout-recovery (find-fn title err)
  "Search for item by TITLE after a timeout using FIND-FN.
Re-signal ERR if item not found."
  (org-canvas--log-warning org-canvas--logger "[Timeout] Checking if item was created...")
  (let ((found (funcall find-fn title)))
    (if found
        (progn
          (org-canvas--log-info org-canvas--logger "[Recovery] Found item after timeout!")
          found)
      (org-canvas--log-error org-canvas--logger "[Recovery] Item not found after timeout")
      (signal (car err) (cdr err)))))

(defun org-canvas--handle-404-retry (endpoint payload find-fn title _err &optional post-url)
  "Retry as POST after 404 on PUT.
ENDPOINT is the base endpoint, PAYLOAD the data to send.
FIND-FN and TITLE are used for timeout recovery on the retry.
ERR is the original error for re-signaling.
POST-URL, when non-nil, overrides the default course-scoped POST URL."
  (org-canvas--log-warning org-canvas--logger "[Recovery] Item not found (404). Retrying as POST...")
  (let ((new-endpoint (or post-url (org-canvas-api-course-endpoint endpoint))))
    (condition-case post-err
        (let ((response (org-canvas-api-request 'POST new-endpoint :data payload)))
          (org-canvas--log-info org-canvas--logger "[Recovery] POST successful")
          response)
      (error
       (if (and find-fn (org-canvas--timeout-error-p post-err))
           (org-canvas--handle-timeout-recovery find-fn title post-err)
         (signal (car post-err) (cdr post-err)))))))

(defun org-canvas--push-check-and-resolve-conflict (endpoint id data title
                                                             &optional modified-field)
  "Check for conflicts on ENDPOINT/ID using DATA.
TITLE is for logging.  MODIFIED-FIELD is passed to
`org-canvas--conflict-check' — files compare `modified_at' (issue #94).
Returns `push', `skip', or `pulled'."
  (let ((conflict-result (org-canvas--conflict-check
                          endpoint id (plist-get data :pom) title
                          modified-field)))
    (if (not (and conflict-result (eq (car conflict-result) 'conflict)))
        'push
      (let* ((remote-response (cdr conflict-result))
             (effective-pull-fn org-canvas--current-pull-item-fn)
             (resolution (org-canvas--resolve-conflict data remote-response)))
        (pcase resolution
          ('skip
           (org-canvas--log-warning org-canvas--logger
             "[Conflict] Skipping '%s' — remote item was modified since last sync" title)
           'skip)
          ('pull
           (if effective-pull-fn
               (progn
                 (org-canvas--conflict-pull-local data remote-response effective-pull-fn)
                 (org-canvas--log-info org-canvas--logger
                   "[Conflict] Pulled remote version of '%s'" title)
                 'pulled)
             (org-canvas--log-warning org-canvas--logger
               "[Conflict] No pull function available, skipping '%s'" title)
             'skip))
          ('push
           (org-canvas--log-info org-canvas--logger
             "[Conflict] Force-pushing '%s' (user chose overwrite)" title)
           'push))))))

;;;; 7a. Duplicate-Title Guard
;;
;; A create is chosen purely by the absence of an id.  Every recovery
;; from a partial create — a timeout, an error after the POST, a killed
;; Emacs — leaves a heading without its stamp next to the item it made,
;; and the next sync would create a second one that students can see
;; and submit to (issue #85).  So before any POST the title is looked
;; up: in the drift snapshot when a sync bound one (free), otherwise
;; through the module's FIND-FN (one GET).

(defun org-canvas--push-remote-items-titled (title find-fn)
  "Return the remote items carrying TITLE, or nil.
Inside a sync `org-canvas--current-remote-titles' is the snapshot's
title index, read for free, or `none' when the run had no snapshot —
an unregistered feature, a failed fetch, conflict detection off — in
which case nothing is checked rather than a GET spent per entry.
Outside a sync (a single-entry push) FIND-FN, the module's search
function, is asked; a module without one is not checked."
  (cond
   ((hash-table-p org-canvas--current-remote-titles)
    (gethash title org-canvas--current-remote-titles))
   (org-canvas--current-remote-titles nil)
   (find-fn
    (let ((found (funcall find-fn title)))
      (and found (list found))))))

(defun org-canvas--push-item-id (item id-key)
  "Return ITEM's id as a string, from the field ID-KEY implies.
`:canvas-url' reads `url' (pages); anything else reads `id'."
  (let ((id (alist-get (if (eq id-key :canvas-url) 'url 'id) item)))
    (and id (format "%s" id))))

(defun org-canvas--adopt-stamp (pom id-property item &optional modified-field)
  "Stamp the heading at POM as the owner of Canvas ITEM.
Writes ID-PROPERTY from ITEM's `url' (pages) or `id', CANVAS_UPDATED_AT
from MODIFIED-FIELD (default `updated_at'; files pass `modified_at')
so the next comparison is against the item's own clock rather than
reporting the adoption itself as a conflict, and removes PAYLOAD_HASH
so the next sync verifies content it has never compared instead of
taking the unchanged-skip.  This is the one spelling of adoption,
shared by the duplicate-title guard and `org-canvas-adopt-at-point'
\(issue #101).  Returns the id as a string, or nil when ITEM has none."
  (let* ((field (if (equal id-property "CANVAS_URL") 'url 'id))
         (id (alist-get field item))
         (updated (alist-get (or modified-field 'updated_at) item)))
    (when id
      (setq id (format "%s" id))
      (org-canvas-org-set-property pom id-property id)
      (when (stringp updated)
        (org-canvas-org-set-property pom "CANVAS_UPDATED_AT" updated))
      (org-entry-delete pom org-canvas--prop-payload-hash)
      id)))

(defun org-canvas--push-adopt-item (data id-key title item)
  "Stamp DATA's heading with ITEM's id and record it under ID-KEY.
Writes the id property and CANVAS_UPDATED_AT from ITEM through
`org-canvas--adopt-stamp', so the update that follows compares against
the item's own clock.  TITLE is for the log.  Returns the id."
  (let ((id (org-canvas--adopt-stamp
             (plist-get data :pom)
             (if (eq id-key :canvas-url) "CANVAS_URL" "CANVAS_ID")
             item)))
    (plist-put data id-key id)
    (org-canvas--log-info org-canvas--logger
      "[Duplicate] Adopted Canvas id %s for '%s' — updating it instead of creating a second"
      id title)
    id))

(defun org-canvas--push-guard-duplicate (data id-key title find-fn)
  "Before creating TITLE, look it up on Canvas and decide what to do.
Returns nil to go ahead and create, `skip' to leave the heading
alone, or the adopted id — DATA and its heading updated — to PUT
instead.  Not consulted when `org-canvas-duplicate-title-strategy' is
`create', when DATA has no :pom to stamp, or when nothing on Canvas
carries TITLE.  ID-KEY and FIND-FN are as for `org-canvas--push-to-api'."
  (unless (or (eq org-canvas-duplicate-title-strategy 'create)
              (null (plist-get data :pom)))
    (let ((items (org-canvas--push-remote-items-titled title find-fn)))
      (when items
        (let* ((ids (mapcar (lambda (item) (org-canvas--push-item-id item id-key))
                            items))
               (id-list (mapconcat #'identity ids ", "))
               (action (org-canvas--resolve-duplicate title ids)))
          ;; Titles are not unique on Canvas; with several holders there
          ;; is no one item to adopt.
          (when (and (eq action 'adopt) (cdr ids))
            (org-canvas--log-warning org-canvas--logger
              "[Duplicate] '%s' is held by %d Canvas items (%s); cannot adopt one, skipping"
              title (length ids) id-list)
            (setq action 'skip))
          (pcase action
            ('adopt (org-canvas--push-adopt-item data id-key title (car items)))
            ('skip
             (org-canvas--log-warning org-canvas--logger
               "[Duplicate] Skipping '%s' — Canvas already holds it as id %s; adopt it with M-x org-canvas-adopt-at-point (which stamps %s), or rename the heading"
               title id-list (if (eq id-key :canvas-url) "CANVAS_URL" "CANVAS_ID"))
             'skip)
            (_
             (org-canvas--log-warning org-canvas--logger
               "[Duplicate] Creating '%s' although Canvas already holds it as id %s"
               title id-list)
             nil)))))))

(cl-defun org-canvas--push-to-api (data payload
					&key
					endpoint
					id-key
					title-key
					find-fn
					post-url-fn
					put-url-fn)
  "Generic push-to-API with 404 retry and optional timeout recovery.

DATA is the parsed entry plist (must contain :canvas-id or :canvas-url).
PAYLOAD is the API payload to send.

Keyword arguments:
  ENDPOINT - API endpoint suffix (e.g., \"assignments\" or \"pages\").
  ID-KEY - Key in DATA for Canvas ID (default: :canvas-id).
  TITLE-KEY - Key in DATA for title (default: :title).
  FIND-FN - Optional function (TITLE) to search for item after timeout.
  POST-URL-FN - Optional () -> URL for POST (overrides course-scoped default).
  PUT-URL-FN - Optional (ID) -> URL for PUT (overrides course-scoped default).

Handle:
  - POST for new items (no ID), PUT for existing items
  - A title Canvas already holds: asks, or follows
    `org-canvas-duplicate-title-strategy' — adopting the id turns the
    POST into a PUT (issue #85)
  - 404 on PUT: retries as POST (stale ID recovery)
  - Timeout: calls FIND-FN to check if item was created

Returns the API response alist, or one of the symbols `conflict',
`pulled' and `duplicate' when the push stopped short."
  (let* ((id-key (or id-key :canvas-id))
         (title-key (or title-key :title))
         (title (plist-get data title-key))
         (guard (and (not (plist-get data id-key))
                     (not org-canvas--dry-run)
                     (org-canvas--push-guard-duplicate data id-key title find-fn)))
         (id (plist-get data id-key))
         (method (if id 'PUT 'POST))
         (full-endpoint (cond
                         ((and id put-url-fn) (funcall put-url-fn id))
                         ((and (not id) post-url-fn) (funcall post-url-fn))
                         (id (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) id))
                         (t (org-canvas-api-course-endpoint endpoint))))
         (post-url (when post-url-fn (funcall post-url-fn))))

    (when (eq guard 'skip)
      (cl-return-from org-canvas--push-to-api 'duplicate))

    ;; Dry-run: skip API call and return a mock response
    (when org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s '%s' to %s" method title full-endpoint)
      (cl-return-from org-canvas--push-to-api org-canvas--dry-run-response))

    ;; Conflict detection: for PUT only, check if remote was modified
    (when (and org-canvas-detect-conflicts
               (eq method 'PUT)
               (plist-get data :pom))
      (let ((decision (org-canvas--push-check-and-resolve-conflict
                       endpoint id data title)))
        (unless (eq decision 'push)
          (cl-return-from org-canvas--push-to-api
            (if (eq decision 'pulled) 'pulled 'conflict)))))

    (org-canvas--log-info org-canvas--logger "[Execute] %s '%s' to %s" method title full-endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request method full-endpoint :data payload)))
          (org-canvas--log-info org-canvas--logger "[Execute] %s successful for '%s'" method title)
          response)
      (error
       ;; Warning, not error: a recovery path may follow, and on terminal
       ;; failure the sync pipeline's [FAILED] line is the single ERROR.
       (org-canvas--log-warning org-canvas--logger "[Execute] Failed: %s" (error-message-string err))

       (cond
        ;; CASE 1: Timeout -> Check if item exists via find-fn
        ((and find-fn (org-canvas--timeout-error-p err))
         (org-canvas--handle-timeout-recovery find-fn title err))

        ;; CASE 2: 404 on PUT -> Retry as POST (stale ID)
        ((org-canvas--404-on-put-p err method)
         (org-canvas--handle-404-retry endpoint payload find-fn title err post-url))

        ;; Default: Re-throw
        (t (signal (car err) (cdr err))))))))

(cl-defun org-canvas--finalize-item (data response
					  &key
					  id-field
					  id-property
					  title-key
					  updated-field
					  post-fn)
  "Finalize sync by saving Canvas ID and LAST_SYNCED.

DATA is the parsed entry plist (must contain :pom).
RESPONSE is the API response alist.

Keyword arguments:
  ID-FIELD - Alist key for ID in response (default: \\='id).
  ID-PROPERTY - Org property name to save (default: \"CANVAS_ID\").
  TITLE-KEY - Key in DATA for title (default: :title).
  UPDATED-FIELD - Response field stamped into CANVAS_UPDATED_AT
    (default: \\='updated_at).  Files use \\='modified_at so the baseline
    agrees with what drift detection compares (issue #94).
  POST-FN - Optional function (DATA RESPONSE) for additional finalization.

Save the Canvas ID and LAST_SYNCED timestamp to the Org entry."
  (let* ((id-field (or id-field 'id))
         (id-property (or id-property "CANVAS_ID"))
         (title-key (or title-key :title))
         (id (alist-get id-field response))
         (pom (plist-get data :pom))
         (title (plist-get data title-key)))

    (unless pom
      (error "Finalize-item: missing :pom in data for '%s'" title))

    (org-canvas--log-debug org-canvas--logger "[Finalize] Processing response for '%s'" title)

    (if id
        (progn
          (org-canvas--log-info org-canvas--logger "[Finalize] Saving %s=%s for '%s'" id-property id title)
          (org-canvas-org-save-sync-state pom id id-property)
          ;; Save CANVAS_UPDATED_AT for conflict detection
          (let ((updated-at (alist-get (or updated-field 'updated_at) response)))
            (when updated-at
              (org-canvas-org-set-property pom "CANVAS_UPDATED_AT"
                                           (format "%s" updated-at))))
          (when post-fn
            (funcall post-fn data response))
          (org-canvas--log-info org-canvas--logger "[Finalize] Complete for '%s'" title))
      (org-canvas--log-error org-canvas--logger "[Finalize] No ID in response for '%s'!" title)
      (org-canvas--signal 'org-canvas-api-error
        "No %s in API response for '%s'" id-field title))))

;;;; 9. Push-at-Point Infrastructure

(defun org-canvas--push-at-point-runtime (feature-name parse-fn build-fn
                                                       push-fn finalize-fn
                                                       title-key pull-item-fn
                                                       &optional hash-extra-fn)
  "Runtime body for generated push-at-point functions.
FEATURE-NAME is the module name string.  PARSE-FN, BUILD-FN,
PUSH-FN, FINALIZE-FN are the 4-stage pipeline functions.
TITLE-KEY is the plist key for the display name.
PULL-ITEM-FN, when non-nil, enables the pull option during conflict resolution.
HASH-EXTRA-FN, when non-nil, is folded into the payload hash
\(see `org-canvas--sync-payload-hash')."
  (org-back-to-heading t)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (org-canvas--log-info org-canvas--logger ">>> SYNC-AT-POINT: %s" feature-name)
  (let* ((org-canvas--current-pull-item-fn pull-item-fn)
         (data (funcall parse-fn))
         (title (plist-get data title-key))
         (payload (funcall build-fn data))
         (payload-hash (org-canvas--sync-payload-hash payload data hash-extra-fn))
         (stored-hash (org-entry-get (point) org-canvas--prop-payload-hash))
         (canvas-id (or (plist-get data :canvas-id)
                        (plist-get data :canvas-url))))
    (org-canvas--log-info org-canvas--logger "[Stage 2: Build] '%s'" title)
    (if (and stored-hash
             (string= payload-hash stored-hash)
             canvas-id)
        (progn
          (org-canvas--log-info org-canvas--logger "[Skip] '%s' unchanged" title)
          (message "%s '%s' unchanged — skipped." (capitalize feature-name) title))
      (org-canvas--log-info org-canvas--logger "[Stage 3: Push] '%s' (%s)"
        title (if canvas-id "UPDATE" "CREATE"))
      (let ((response (funcall push-fn data payload)))
        (if (memq response '(conflict pulled duplicate))
            (org-canvas--push-at-point-report-stop feature-name title response)
          (org-canvas--log-info org-canvas--logger "[Stage 4: Finalize] '%s'" title)
          (condition-case err
              (progn
                (funcall finalize-fn data response)
                (org-canvas-org-set-property (point) org-canvas--prop-payload-hash payload-hash)
                (org-canvas--sync-advance-header-from-entry)
                (org-canvas--save-buffer))
            (error
             ;; The push landed; only the stamp died (issue #97).
             (org-canvas--log-error org-canvas--logger
               "[Stamp] The push of '%s' landed on Canvas, but stamping the file failed: %s — the entry still carries its old CANVAS_UPDATED_AT and PAYLOAD_HASH, so the next sync will report drift that is not real"
               title (error-message-string err))
             (signal (car err) (cdr err))))
          (org-canvas--log-info org-canvas--logger "[Sync] '%s' synced successfully" title)
          (message "%s '%s' synced." (capitalize feature-name) title))))))

(defun org-canvas--push-at-point-report-stop (feature-name title outcome)
  "Say why the single-entry push of TITLE stopped with OUTCOME.
FEATURE-NAME names the module.  OUTCOME is `conflict', `pulled' or
`duplicate', the symbols `org-canvas--push-to-api' returns in place
of a response; finalizing one as if it were a response is a type
error, which is how a conflict at point used to end in a backtrace."
  (let ((why (pcase outcome
               ('conflict "not pushed — the remote item was modified since the last sync")
               ('pulled "not pushed — the remote version was pulled instead")
               (_ "not pushed — Canvas already holds this title; adopt it with M-x org-canvas-adopt-at-point or rename"))))
    (org-canvas--log-warning org-canvas--logger "[Sync] '%s' %s" title why)
    (message "%s '%s' %s." (capitalize feature-name) title why)))


(provide 'org-canvas-core-sync)
;;; org-canvas-core-sync.el ends here
