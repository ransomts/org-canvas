;;; org-canvas-core-sync.el --- Sync pipeline, conflict, push/delete infrastructure -*- lexical-binding: t; -*-

;;; Commentary:

;; Sync pipeline macros (`org-canvas-define-sync', `org-canvas-define-push-at-point'),
;; conflict detection/resolution, generic push-to-API with error recovery,
;; delete infrastructure, setup wizard, and file upload.

;;; Code:

(require 'cl-lib)
(require 'diff)
(require 'json)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)

(defvar org-canvas--sync-in-progress nil
  "Non-nil when a sync is running.  Prevents concurrent syncs.")

(defvar org-canvas--dry-run nil
  "When non-nil, sync shows what would happen without making API calls.")

(defvar org-canvas--conflict-apply-all nil
  "When non-nil, auto-resolve all conflicts with this action.
Valid values: nil, \\='push, \\='pull, \\='skip.
Bound per-sync by `org-canvas-define-sync'.")

(defvar org-canvas--current-pull-item-fn nil
  "Pull-item function for the module currently being synced.
Dynamically bound by the sync pipeline so `org-canvas--push-to-api'
can access it without changing per-module push function signatures.")

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
    (error "%s file not found: %s" feature-upper sync-file))
  (let ((buf (find-file-noselect sync-file)))
    (when (buffer-modified-p buf)
      (if (y-or-n-p (format "%s has unsaved changes.  Save before syncing? "
                            (file-name-nondirectory sync-file)))
          (with-current-buffer buf (save-buffer))
        (user-error "Aborted: unsaved changes in %s" sync-file))))
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (elog-info org-canvas--logger "========================================")
  (elog-info org-canvas--logger ">>> STARTING %s SYNC" feature-upper)
  (elog-info org-canvas--logger "File: %s" sync-file)
  (elog-info org-canvas--logger "Course: %s | URL: %s"
    org-canvas-course-id org-canvas-base-url)
  (elog-info org-canvas--logger "========================================"))

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
    (elog-info org-canvas--logger "Found %d %s to sync"
      (length targets) feature-name)
    ;; Warn about duplicate CANVAS_IDs
    (let ((id-counts (make-hash-table :test 'equal)))
      (dolist (id all-ids-before)
        (puthash id (1+ (gethash id id-counts 0)) id-counts))
      (maphash (lambda (id count)
                 (when (> count 1)
                   (elog-warning org-canvas--logger
                     "[Duplicate] CANVAS_ID %s appears %d times in %s"
                     id count sync-file)))
               id-counts))
    (list :targets targets :all-ids-before all-ids-before)))

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
    (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
    (save-buffer)
    (plist-put counters :success (1+ (plist-get counters :success)))
    (message "%s [%d/%d] Synced '%s'"
      cap-feature progress total-count title)
    (unless canvas-id
      (let ((new-id (or (org-entry-get (point) "CANVAS_ID")
                        (org-entry-get (point) "CANVAS_URL"))))
        (when new-id
          (push new-id (car synced-ids)))))))

(defun org-canvas--sync-execute-pipeline (data payload ctx)
  "Execute the skip/dry-run/push pipeline for a single entry.
DATA is the parsed entry, PAYLOAD is the built API payload.
CTX is the sync context plist (see `org-canvas--sync-process-entry')."
  (let* ((push-fn (plist-get ctx :push-fn))
         (feature-name (plist-get ctx :feature-name))
         (total-count (plist-get ctx :total-count))
         (counters (plist-get ctx :counters))
         (synced-ids (plist-get ctx :synced-ids))
         (payload-hash (md5 (json-encode payload)))
         (stored-hash (org-entry-get (point) "PAYLOAD_HASH"))
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
     ((and stored-hash (string= payload-hash stored-hash) canvas-id)
      (plist-put counters :skip (1+ (plist-get counters :skip)))
      (elog-info org-canvas--logger "[Skip] '%s' unchanged" title)
      (message "%s [%d/%d] Skipping '%s' (unchanged)"
        cap-feature progress total-count title))
     (org-canvas--dry-run
      (elog-info org-canvas--logger "[DRY-RUN] Would %s '%s'"
        (if canvas-id "UPDATE" "CREATE") title)
      (message "%s [DRY-RUN] Would %s '%s'"
        cap-feature (if canvas-id "update" "create") title)
      (plist-put counters :success (1+ (plist-get counters :success))))
     (t
      (let ((response (funcall push-fn data payload)))
        (cond
         ((eq response 'conflict)
          (plist-put counters :conflict
                     (1+ (plist-get counters :conflict)))
          (message "%s [%d/%d] CONFLICT: '%s' (remote modified)"
            cap-feature progress total-count title))
         ((eq response 'pulled)
          (plist-put counters :pulled
                     (1+ (or (plist-get counters :pulled) 0)))
          (message "%s [%d/%d] PULLED: '%s' (local updated)"
            cap-feature progress total-count title))
         (t
          (org-canvas--sync-finalize-push response data payload-hash ctx))))))))

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
    (elog-info org-canvas--logger "----------------------------------------")
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (condition-case err
            (let* ((data (funcall parse-fn))
                   (payload (funcall build-fn data)))
              (org-canvas--sync-execute-pipeline data payload ctx))
          (error
           (plist-put counters :fail (1+ (plist-get counters :fail)))
           (elog-error org-canvas--logger "[FAILED] %s at point %d: %s"
             feature-upper (marker-position marker) (error-message-string err))
           (message "%s [%d/%d] FAILED: %s"
             (capitalize feature-name)
             (+ (plist-get counters :success) (plist-get counters :skip)
                (plist-get counters :fail))
             total-count (error-message-string err))))))))

(defun org-canvas--sync-warn-orphans (all-ids-before synced-ids feature-name)
  "Warn about CANVAS_IDs in ALL-IDS-BEFORE not present in SYNCED-IDS.
FEATURE-NAME is used in the log message."
  (dolist (old-id all-ids-before)
    (unless (member old-id synced-ids)
      (elog-warning org-canvas--logger
        "[Orphan] CANVAS_ID %s in file was not synced — may be orphaned on Canvas.\n  To clean up: delete the heading's CANVAS_ID property, or use M-x org-canvas-delete-%s-at-point"
        old-id feature-name))))

(defun org-canvas--sync-log-summary (feature-name sync-file counters)
  "Save SYNC-FILE and log completion summary.
FEATURE-NAME is the module name.  COUNTERS is a plist with
:success, :skip, :fail, and optionally :conflict and :pulled counts."
  (let ((feature-upper (upcase feature-name))
        (success-count (plist-get counters :success))
        (skip-count (plist-get counters :skip))
        (fail-count (plist-get counters :fail))
        (conflict-count (or (plist-get counters :conflict) 0))
        (pulled-count (or (plist-get counters :pulled) 0))
        (extra-counts 0))
    (with-current-buffer (find-file-noselect sync-file)
      (save-buffer)
      (elog-info org-canvas--logger "Saved %s" sync-file))
    (elog-info org-canvas--logger "========================================")
    (elog-info org-canvas--logger ">>> %s SYNC COMPLETE" feature-upper)
    (setq extra-counts (+ conflict-count pulled-count))
    (if (> extra-counts 0)
        (elog-info org-canvas--logger
          "Success: %d | Skipped: %d | Failed: %d | Conflicts: %d | Pulled: %d"
          success-count skip-count fail-count conflict-count pulled-count)
      (elog-info org-canvas--logger "Success: %d | Skipped: %d | Failed: %d"
        success-count skip-count fail-count))
    (elog-info org-canvas--logger "========================================")
    (if (> extra-counts 0)
        (message "%s sync: %d success, %d skipped, %d failed, %d conflicts, %d pulled."
                 (capitalize feature-name) success-count skip-count
                 fail-count conflict-count pulled-count)
      (message "%s sync: %d success, %d skipped, %d failed."
               (capitalize feature-name) success-count skip-count fail-count))))

;; Plist key convention in parse-entry return values:
;; - kebab-case (:canvas-id, :pom, :local-path) for internal pipeline fields
;; - snake_case (:points_possible, :due_at) for fields mapping 1:1 to Canvas API params
;; - Universal fields (:title, :published, :description) use kebab-case regardless
;; Modules with hash-table payloads (rubrics, modules, files) use all kebab-case
;; since they remap everything manually in build-payload.

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
         (feature-upper (upcase feature-name))
         (sync-fn-name (intern (format "org-canvas-sync-%s" feature-name)))
         (file-expr (plist-get args :file))
         (query (or (plist-get args :query) "LEVEL=1"))
         (parse-fn (plist-get args :parse))
         (build-fn (plist-get args :build))
         (endpoint (plist-get args :endpoint))
         (id-key (plist-get args :id-key))
         (id-field (plist-get args :id-field))
         (id-property (plist-get args :id-property))
         (find-fn (plist-get args :find-fn))
         (post-fn (plist-get args :post-fn))
         (title-key (plist-get args :title-key))
         (pull-item-fn (plist-get args :pull-item-fn))
         ;; Auto-generate push-fn from :endpoint when :push not provided
         (push-fn (or (plist-get args :push)
                      (when endpoint
                        `(lambda (data payload)
                           (org-canvas--push-to-api data payload
                             :endpoint ,endpoint
                             ,@(when id-key `(:id-key ,id-key))
                             ,@(when title-key `(:title-key ,title-key))
                             ,@(when find-fn `(:find-fn ,find-fn)))))))
         ;; Auto-generate finalize-fn from :endpoint when :finalize not provided
         (finalize-fn (or (plist-get args :finalize)
                          (when endpoint
                            `(lambda (data response)
                               (org-canvas--finalize-item data response
                                 ,@(when id-field `(:id-field ,id-field))
                                 ,@(when id-property `(:id-property ,id-property))
                                 ,@(when title-key `(:title-key ,title-key))
                                 ,@(when post-fn `(:post-fn ,post-fn))))))))
    ;; Validate required args
    (unless file-expr (error "org-canvas-define-sync: :file is required"))
    (unless parse-fn (error "org-canvas-define-sync: :parse is required"))
    (unless build-fn (error "org-canvas-define-sync: :build is required"))
    (unless push-fn (error "org-canvas-define-sync: :push or :endpoint is required"))
    (unless finalize-fn (error "org-canvas-define-sync: :finalize or :endpoint is required"))
    `(progn
       ;;;###autoload
       (defun ,sync-fn-name ()
         ,(format "Synchronize %s to Canvas using the 4-stage pipeline." feature-name)
         (interactive)
         (org-canvas-clear-log)
         (let ((org-canvas--conflict-apply-all nil)
               (org-canvas--current-pull-item-fn ,pull-item-fn)
               (sync-file (expand-file-name ,file-expr)))
           (org-canvas--sync-validate-file ,feature-upper sync-file)
           (let* ((entries (org-canvas--sync-collect-entries
                            sync-file ,query ,feature-name))
                  (targets (plist-get entries :targets))
                  (all-ids-before (plist-get entries :all-ids-before))
                  (counters (list :success 0 :skip 0 :fail 0 :pulled 0))
                  (synced-ids (list nil))
                  (ctx (list :parse-fn ,parse-fn
                             :build-fn ,build-fn
                             :push-fn ,push-fn
                             :finalize-fn ,finalize-fn
                             :feature-name ,feature-name
                             :feature-upper ,feature-upper
                             :total-count (length targets)
                             :counters counters
                             :synced-ids synced-ids
                             :title-key ,title-key)))
             ;; Process each entry through the pipeline
             (dolist (marker targets)
               (org-canvas--sync-process-entry marker ctx))
             (org-canvas--sync-warn-orphans all-ids-before (car synced-ids)
                                            ,feature-name)
             (org-canvas--sync-log-summary
              ,feature-name sync-file counters)))))))

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

(defun org-canvas--parse-last-synced (pom)
  "Parse the LAST_SYNCED Org timestamp at POM to an Emacs time value.
Returns nil if no LAST_SYNCED property exists."
  (let ((ts (org-entry-get pom "LAST_SYNCED")))
    (when ts
      (encode-time (org-parse-time-string ts)))))

(cl-defun org-canvas--conflict-check (endpoint id pom)
  "Check if the remote item at ENDPOINT/ID was modified after LAST_SYNCED at POM.
Returns (cons \\='conflict REMOTE-RESPONSE) if the remote item is newer,
nil otherwise.  Returns nil on GET failure (allows push to proceed) or
when no LAST_SYNCED exists (legacy item, first sync)."
  (let ((local-time (org-canvas--parse-last-synced pom)))
    (unless local-time
      (cl-return-from org-canvas--conflict-check nil))
    (condition-case _err
        (let* ((full-url (org-canvas-api-course-endpoint
                          (format "%s/%%s" endpoint) id))
               (response (org-canvas-api-request 'GET full-url))
               (updated-at (alist-get 'updated_at response))
               (remote-time (org-canvas--parse-iso8601-time updated-at)))
          (if (and remote-time (time-less-p local-time remote-time))
              (progn
                (elog-warning org-canvas--logger
                  "[Conflict] Remote item updated at %s, local LAST_SYNCED is %s"
                  updated-at (org-entry-get pom "LAST_SYNCED"))
                (cons 'conflict response))
            nil))
      (error nil))))

;;;; 6c. Interactive Conflict Resolution
;;
;; When a conflict is detected, show a diff buffer and let the user
;; choose: push (overwrite remote), pull (overwrite local), or skip.

(defconst org-canvas--conflict-buffer-name "*canvas-conflict*"
  "Buffer name for the conflict resolution diff display.")

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
         (last-synced (when (plist-get data :pom)
                        (org-entry-get (plist-get data :pom) "LAST_SYNCED")))
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
      (_ choice))))

(defun org-canvas--conflict-pull-local (data remote-response pull-item-fn)
  "Overwrite local heading with REMOTE-RESPONSE data.
DATA is the parsed entry plist.  PULL-ITEM-FN is the module-specific
function that sets properties and body from a remote item.
Updates title, LAST_SYNCED, and deletes stale PAYLOAD_HASH."
  (let ((pom (plist-get data :pom))
        (remote-title (or (alist-get 'title remote-response)
                          (alist-get 'name remote-response))))
    (when (and remote-title pom)
      (save-excursion
        (goto-char (if (markerp pom) (marker-position pom) pom))
        (org-back-to-heading t)
        ;; Update heading title
        (let* ((old-heading (org-get-heading t t t t))
               (new-heading remote-title))
          (when (and old-heading (not (string= old-heading new-heading)))
            (re-search-forward org-complex-heading-regexp (line-end-position) t)
            (replace-match new-heading t t nil 4)))))
    ;; Call module-specific pull-item to update properties/body
    (let ((pos (if (markerp pom) (marker-position pom) pom)))
      (funcall pull-item-fn remote-response pos)
      ;; Update sync metadata
      (org-entry-put pos "LAST_SYNCED"
                     (format-time-string "[%Y-%m-%d %a %H:%M]"))
      (let ((updated-at (alist-get 'updated_at remote-response)))
        (when updated-at
          (org-entry-put pos "CANVAS_UPDATED_AT" updated-at)))
      (org-entry-delete pos "PAYLOAD_HASH")
      (save-buffer))))

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

(defun org-canvas--404-on-put-p (err method)
  "Return non-nil if ERR is a 404 and METHOD is PUT or PATCH.
ERR is a `condition-case' error value."
  (and (memq method '(PUT PATCH))
       (string-match-p "404" (error-message-string err))))

(cl-defun org-canvas--search-item (endpoint title &key params match-field)
  "Search for an item on Canvas by title.

ENDPOINT is the API endpoint suffix (e.g., \"pages\" or \"assignments\").
TITLE is the value to search for.

Keyword arguments:
  PARAMS - Additional params for GET request (default adds search_term).
  MATCH-FIELD - Alist key to match against TITLE (default: \\='title).

Return the matching item alist, or nil if not found."
  (let ((match-field (or match-field 'title)))
    (elog-info org-canvas--logger "[Search] Looking for item with %s='%s'..." match-field title)
    (condition-case err
        (let* ((full-endpoint (org-canvas-api-course-endpoint endpoint))
               (search-params (append (or params `(("search_term" . ,title))) nil))
               (results (append (org-canvas-api-request 'GET full-endpoint :params search-params) nil))
               (count (length results)))
          (elog-debug org-canvas--logger "[Search] Found %d results" count)
          (let ((found (cl-loop for item in results
                                when (string-equal (alist-get match-field item) title)
                                return item)))
            (if found
                (elog-info org-canvas--logger "[Search] Found exact match: ID=%s"
                  (or (alist-get 'id found) (alist-get 'url found)))
              (elog-debug org-canvas--logger "[Search] No exact match found"))
            found))
      (error
       (elog-warning org-canvas--logger "[Search] Failed: %s" (error-message-string err))
       nil))))

(defun org-canvas--handle-timeout-recovery (find-fn title err)
  "Search for item by TITLE after a timeout using FIND-FN.
Re-signal ERR if item not found."
  (elog-warning org-canvas--logger "[Timeout] Checking if item was created...")
  (let ((found (funcall find-fn title)))
    (if found
        (progn
          (elog-info org-canvas--logger "[Recovery] Found item after timeout!")
          found)
      (elog-error org-canvas--logger "[Recovery] Item not found after timeout")
      (signal (car err) (cdr err)))))

(defun org-canvas--handle-404-retry (endpoint payload find-fn title _err)
  "Retry as POST after 404 on PUT.
ENDPOINT is the base endpoint, PAYLOAD the data to send.
FIND-FN and TITLE are used for timeout recovery on the retry.
ERR is the original error for re-signaling."
  (elog-warning org-canvas--logger "[Recovery] Item not found (404). Retrying as POST...")
  (let ((new-endpoint (org-canvas-api-course-endpoint endpoint)))
    (condition-case post-err
        (let ((response (org-canvas-api-request 'POST new-endpoint :data payload)))
          (elog-info org-canvas--logger "[Recovery] POST successful")
          response)
      (error
       (if (and find-fn (caddr post-err)
                (string-match "Timeout" (format "%s" (caddr post-err))))
           (org-canvas--handle-timeout-recovery find-fn title post-err)
         (signal (car post-err) (cdr post-err)))))))

(defun org-canvas--push-check-and-resolve-conflict (endpoint id data title)
  "Check for conflicts on ENDPOINT/ID using DATA.
TITLE is for logging.  Returns `push', `skip', or `pulled'."
  (let ((conflict-result (org-canvas--conflict-check endpoint id (plist-get data :pom))))
    (if (not (and conflict-result (eq (car conflict-result) 'conflict)))
        'push
      (let* ((remote-response (cdr conflict-result))
             (effective-pull-fn org-canvas--current-pull-item-fn)
             (resolution (org-canvas--resolve-conflict data remote-response)))
        (pcase resolution
          ('skip
           (elog-warning org-canvas--logger
             "[Conflict] Skipping '%s' — remote item was modified since last sync" title)
           'skip)
          ('pull
           (if effective-pull-fn
               (progn
                 (org-canvas--conflict-pull-local data remote-response effective-pull-fn)
                 (elog-info org-canvas--logger
                   "[Conflict] Pulled remote version of '%s'" title)
                 'pulled)
             (elog-warning org-canvas--logger
               "[Conflict] No pull function available, skipping '%s'" title)
             'skip))
          ('push
           (elog-info org-canvas--logger
             "[Conflict] Force-pushing '%s' (user chose overwrite)" title)
           'push))))))

(cl-defun org-canvas--push-to-api (data payload
					&key
					endpoint
					id-key
					title-key
					find-fn)
  "Generic push-to-API with 404 retry and optional timeout recovery.

DATA is the parsed entry plist (must contain :canvas-id or :canvas-url).
PAYLOAD is the API payload to send.

Keyword arguments:
  ENDPOINT - API endpoint suffix (e.g., \"assignments\" or \"pages\").
  ID-KEY - Key in DATA for Canvas ID (default: :canvas-id).
  TITLE-KEY - Key in DATA for title (default: :title).
  FIND-FN - Optional function (TITLE) to search for item after timeout.

Handle:
  - POST for new items (no ID), PUT for existing items
  - 404 on PUT: retries as POST (stale ID recovery)
  - Timeout: calls FIND-FN to check if item was created

Returns the API response alist."
  (let* ((id-key (or id-key :canvas-id))
         (title-key (or title-key :title))
         (id (plist-get data id-key))
         (title (plist-get data title-key))
         (method (if id 'PUT 'POST))
         (full-endpoint (if id
                            (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) id)
                          (org-canvas-api-course-endpoint endpoint))))

    ;; Dry-run: skip API call and return a mock response
    (when org-canvas--dry-run
      (elog-info org-canvas--logger "[DRY-RUN] Would %s '%s' to %s" method title full-endpoint)
      (cl-return-from org-canvas--push-to-api '((id . "dry-run"))))

    ;; Conflict detection: for PUT only, check if remote was modified
    (when (and org-canvas-detect-conflicts
               (eq method 'PUT)
               (plist-get data :pom))
      (let ((decision (org-canvas--push-check-and-resolve-conflict
                       endpoint id data title)))
        (unless (eq decision 'push)
          (cl-return-from org-canvas--push-to-api
            (if (eq decision 'pulled) 'pulled 'conflict)))))

    (elog-info org-canvas--logger "[Execute] %s '%s' to %s" method title full-endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request method full-endpoint :data payload)))
          (elog-info org-canvas--logger "[Execute] %s successful for '%s'" method title)
          response)
      (error
       (elog-error org-canvas--logger "[Execute] Failed: %s" (error-message-string err))

       (cond
        ;; CASE 1: Timeout -> Check if item exists via find-fn
        ((and find-fn (org-canvas--timeout-error-p err))
         (org-canvas--handle-timeout-recovery find-fn title err))

        ;; CASE 2: 404 on PUT -> Retry as POST (stale ID)
        ((org-canvas--404-on-put-p err method)
         (org-canvas--handle-404-retry endpoint payload find-fn title err))

        ;; Default: Re-throw
        (t (signal (car err) (cdr err))))))))

(cl-defun org-canvas--finalize-item (data response
					  &key
					  id-field
					  id-property
					  title-key
					  post-fn)
  "Finalize sync by saving Canvas ID and LAST_SYNCED.

DATA is the parsed entry plist (must contain :pom).
RESPONSE is the API response alist.

Keyword arguments:
  ID-FIELD - Alist key for ID in response (default: \\='id).
  ID-PROPERTY - Org property name to save (default: \"CANVAS_ID\").
  TITLE-KEY - Key in DATA for title (default: :title).
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

    (elog-debug org-canvas--logger "[Finalize] Processing response for '%s'" title)

    (if id
        (progn
          (elog-info org-canvas--logger "[Finalize] Saving %s=%s for '%s'" id-property id title)
          (org-canvas-org-save-sync-state pom id id-property)
          ;; Save CANVAS_UPDATED_AT for conflict detection
          (let ((updated-at (alist-get 'updated_at response)))
            (when updated-at
              (org-canvas-org-set-property pom "CANVAS_UPDATED_AT"
                                           (format "%s" updated-at))))
          (when post-fn
            (funcall post-fn data response))
          (elog-info org-canvas--logger "[Finalize] Complete for '%s'" title))
      (elog-error org-canvas--logger "[Finalize] No ID in response for '%s'!" title)
      (error "No %s in API response for '%s'" id-field title))))

;;;; 8. Delete Infrastructure
;;
;; Two types of delete operations:
;;
;;   - delete-all: Fetches all items from Canvas and deletes them,
;;     then cleans up local CANVAS_ID properties from the Org file.
;;
;;   - delete-at-point: Deletes the single item at the current cursor
;;     position (requires confirmation).
;;
;; Use the macros `org-canvas-define-delete-all' and
;; `org-canvas-define-delete-at-point' to generate these functions.

(defun org-canvas--delete-log-skipped (items skip-fn title-field)
  "Log skipped items from ITEMS using SKIP-FN.
TITLE-FIELD is the alist key for item display names."
  (dolist (item items)
    (when (funcall skip-fn item)
      (elog-info org-canvas--logger "Skipping: '%s'"
        (alist-get title-field item)))))

(defun org-canvas--delete-items-queued (items endpoint-fn id-field title-field &optional skip-fn)
  "Delete ITEMS from Canvas using synchronous requests.
ENDPOINT-FN is a function taking an item ID and returning the DELETE URL.
ID-FIELD and TITLE-FIELD are alist keys for extracting ID/title from each item.
SKIP-FN, if non-nil, is called with each item; non-nil return skips that item.
Returns (DELETED-COUNT . DELETED-IDS)."
  (let* ((to-delete (if skip-fn (cl-remove-if skip-fn items) items))
         (skipped (- (length items) (length to-delete))))
    (when (and (> skipped 0) skip-fn)
      (org-canvas--delete-log-skipped items skip-fn title-field))
    ;; Short-circuit if nothing to delete
    (if (null to-delete)
        (cons 0 nil)
      (let ((deleted-count 0)
            (deleted-ids nil))
        (dolist (item to-delete)
          (let ((item-id (alist-get id-field item))
                (item-title (alist-get title-field item)))
            (elog-info org-canvas--logger "Deleting: '%s' (ID: %s)" item-title item-id)
            (condition-case err
                (progn
                  (org-canvas-api-request 'DELETE (funcall endpoint-fn item-id))
                  (push (org-canvas--normalize-id item-id) deleted-ids)
                  (setq deleted-count (1+ deleted-count))
                  (elog-info org-canvas--logger "  -> Deleted '%s' successfully" item-title))
              (error
               (elog-error org-canvas--logger "  -> Delete failed for '%s': %s"
                 item-title (error-message-string err))))))
        (cons deleted-count deleted-ids)))))

(cl-defun org-canvas--delete-all-items (feature-name
                                        &key
                                        endpoint
                                        file
                                        id-field
                                        title-field
                                        id-property
                                        list-params
                                        skip-fn)
  "Generic implementation for deleting all items of a feature type.

FEATURE-NAME is a string like \"announcements\" or \"pages\".

Keyword arguments:
  ENDPOINT - API endpoint suffix (e.g., \"assignments\").
  FILE - Path to the org file for cleaning local properties.
  ID-FIELD - Alist key for item ID in API response (default: \\='id).
  TITLE-FIELD - Alist key for item title in API response (default: \\='title).
  ID-PROPERTY - Org property name for ID (default: \"CANVAS_ID\").
  LIST-PARAMS - Extra params for GET request.
  SKIP-FN - Optional function taking an item, returns non-nil to skip.

Returns the count of successfully deleted items."
  (let* ((id-field (or id-field 'id))
         (title-field (or title-field 'title))
         (id-property (or id-property "CANVAS_ID"))
         (full-endpoint (org-canvas-api-course-endpoint endpoint))
         (remote-items (org-canvas-api-request-all-pages 'GET full-endpoint list-params)))

    (elog-info org-canvas--logger "Found %d %s on Canvas" (length remote-items) feature-name)

    (let* ((result (org-canvas--delete-items-queued
                    remote-items
                    (lambda (item-id)
                      (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) item-id))
                    id-field title-field skip-fn))
           (deleted-count (car result)))

      ;; Cleanup local properties
      (org-canvas--clean-local-sync-properties file id-property)

      (elog-info org-canvas--logger "========================================")
      (elog-info org-canvas--logger ">>> MASS DELETION COMPLETE: %d removed" deleted-count)
      (elog-info org-canvas--logger "========================================")

      deleted-count)))

(cl-defun org-canvas--delete-item-at-point (feature-name
                                            &key
                                            endpoint
                                            id-property)
  "Generic implementation for deleting the item at the current Org heading.

FEATURE-NAME is a string like \"assignment\" or \"page\".

Keyword arguments:
  ENDPOINT - API endpoint pattern with %s for ID (e.g., \"assignments/%s\").
  ID-PROPERTY - Org property name for ID (default: \"CANVAS_ID\").

Return non-nil if deletion succeeded."
  (org-back-to-heading t)
  (let* ((id-property (or id-property "CANVAS_ID"))
         (pom (point))
         (canvas-id (org-canvas-org-get-property pom id-property))
         (title (org-get-heading t t t t)))

    (unless canvas-id
      (user-error "No %s property found for this heading" id-property))

    (when (y-or-n-p (format "Delete '%s' from Canvas? " title))
      (org-canvas-clear-log)
      (display-buffer (get-buffer-create org-canvas--log-buffer-name))
      (elog-info org-canvas--logger "Deleting %s '%s' (ID: %s)..." feature-name title canvas-id)

      (condition-case err
          (progn
            (org-canvas-api-request 'DELETE
				    (org-canvas-api-course-endpoint endpoint canvas-id))
            (elog-info org-canvas--logger "Successfully deleted from Canvas")
            (org-canvas-clear-sync-properties pom)
            (elog-info org-canvas--logger "Cleaned local properties")
            (message "%s '%s' deleted." (capitalize feature-name) title)
            t)
        (error
         (elog-error org-canvas--logger "Failed to delete: %s" (error-message-string err))
         (message "Failed to delete %s. Check logs." feature-name)
         nil)))))

(cl-defun org-canvas--delete-all-runtime (feature-name &key endpoint file
                                                        id-field title-field
                                                        id-property list-params
                                                        skip-fn)
  "Runtime body for generated delete-all functions.
FEATURE-NAME is the module name string.  ENDPOINT, FILE, ID-FIELD,
TITLE-FIELD, ID-PROPERTY, LIST-PARAMS, and SKIP-FN are passed
through to `org-canvas--delete-all-items'."
  (org-canvas-clear-log)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let ((feature-upper (upcase feature-name)))
    (elog-warning org-canvas--logger "========================================")
    (elog-warning org-canvas--logger ">>> STARTING MASS DELETION OF %s" feature-upper)
    (elog-warning org-canvas--logger "========================================"))
  (let ((deleted-count (org-canvas--delete-all-items feature-name
                          :endpoint endpoint :file file
                          :id-field id-field :title-field title-field
                          :id-property id-property :list-params list-params
                          :skip-fn skip-fn)))
    (message "%s deletion complete. %d removed." (capitalize feature-name) deleted-count)))

(defmacro org-canvas-define-delete-all (feature &rest args)
  "Define a delete-all function for FEATURE.
FEATURE is a symbol like \\='pages.  ARGS is a plist with keys:
  :endpoint, :file (required), :id-field, :title-field,
  :id-property, :list-params, :skip-fn."
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas-delete-all-%s" feature-name)))
         (endpoint (plist-get args :endpoint))
         (file-expr (plist-get args :file))
         (id-field (or (plist-get args :id-field) ''id))
         (title-field (or (plist-get args :title-field) ''title))
         (id-property (or (plist-get args :id-property) "CANVAS_ID"))
         (list-params (plist-get args :list-params))
         (skip-fn (plist-get args :skip-fn)))
    (unless endpoint (error "org-canvas-define-delete-all: :endpoint is required"))
    (unless file-expr (error "org-canvas-define-delete-all: :file is required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Delete ALL %s in the configured course." feature-name)
         (interactive)
         (unless org-canvas--inhibit-log-clear
           (unless (y-or-n-p ,(format "Delete ALL %s in this course? " feature-name))
             (user-error "Aborted")))
         (org-canvas--delete-all-runtime ,feature-name
           :endpoint ,endpoint :file ,file-expr
           :id-field ,id-field :title-field ,title-field
           :id-property ,id-property :list-params ,list-params
           :skip-fn ,skip-fn)))))

(defmacro org-canvas-define-delete-at-point (feature &rest args)
  "Define a delete-at-point function for FEATURE.

FEATURE is a symbol like \\='page or \\='assignment.

ARGS is a plist with the following keys:
  :endpoint - API endpoint pattern with %s (required)
  :id-property - Org property name (default: \"CANVAS_ID\")

Example:
  (org-canvas-define-delete-at-point assignment
    :endpoint \"assignments/%s\")"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas-delete-%s-at-point" feature-name)))
         (endpoint (plist-get args :endpoint))
         (id-property (or (plist-get args :id-property) "CANVAS_ID")))
    (unless endpoint (error "org-canvas-define-delete-at-point: :endpoint is required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Delete the Canvas %s associated with the current Org heading." feature-name)
         (interactive)
         (org-canvas--delete-item-at-point ,feature-name
                                           :endpoint ,endpoint
                                           :id-property ,id-property)))))

;;;; 9. Push-at-Point Infrastructure

(defun org-canvas--push-at-point-runtime (feature-name parse-fn build-fn
                                                       push-fn finalize-fn
                                                       title-key pull-item-fn)
  "Runtime body for generated push-at-point functions.
FEATURE-NAME is the module name string.  PARSE-FN, BUILD-FN,
PUSH-FN, FINALIZE-FN are the 4-stage pipeline functions.
TITLE-KEY is the plist key for the display name.
PULL-ITEM-FN, when non-nil, enables the pull option during conflict resolution."
  (org-back-to-heading t)
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let* ((org-canvas--current-pull-item-fn pull-item-fn)
         (data (funcall parse-fn))
         (title (plist-get data title-key))
         (payload (funcall build-fn data))
         (payload-hash (md5 (json-encode payload)))
         (stored-hash (org-entry-get (point) "PAYLOAD_HASH"))
         (canvas-id (or (plist-get data :canvas-id)
                        (plist-get data :canvas-url))))
    (if (and stored-hash
             (string= payload-hash stored-hash)
             canvas-id)
        (progn
          (elog-info org-canvas--logger "[Skip] '%s' unchanged" title)
          (message "%s '%s' unchanged — skipped." (capitalize feature-name) title))
      (let ((response (funcall push-fn data payload)))
        (funcall finalize-fn data response)
        (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
        (save-buffer)
        (elog-info org-canvas--logger "[Sync] '%s' synced successfully" title)
        (message "%s '%s' synced." (capitalize feature-name) title)))))

(defmacro org-canvas-define-push-at-point (feature &rest args)
  "Define a sync-at-point function for FEATURE.
FEATURE is a symbol like \\='page.  ARGS is a plist with keys:
  :parse, :build (required), :push, :finalize, :endpoint,
  :id-key, :id-field, :id-property, :find-fn, :post-fn,
  :title-key, :pull-item-fn.
When :endpoint is provided, :push and :finalize are auto-generated
if not explicitly given."
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas-sync-%s-at-point" feature-name)))
         (parse-fn (plist-get args :parse))
         (build-fn (plist-get args :build))
         (endpoint (plist-get args :endpoint))
         (id-key (plist-get args :id-key))
         (id-field (plist-get args :id-field))
         (id-property (plist-get args :id-property))
         (find-fn (plist-get args :find-fn))
         (post-fn (plist-get args :post-fn))
         (title-key (or (plist-get args :title-key) :title))
         (pull-item-fn (plist-get args :pull-item-fn))
         ;; Auto-generate push-fn from :endpoint when :push not provided
         (push-fn (or (plist-get args :push)
                      (when endpoint
                        `(lambda (data payload)
                           (org-canvas--push-to-api data payload
                             :endpoint ,endpoint
                             ,@(when id-key `(:id-key ,id-key))
                             ,@(when (not (eq title-key :title)) `(:title-key ,title-key))
                             ,@(when find-fn `(:find-fn ,find-fn)))))))
         ;; Auto-generate finalize-fn from :endpoint when :finalize not provided
         (finalize-fn (or (plist-get args :finalize)
                          (when endpoint
                            `(lambda (data response)
                               (org-canvas--finalize-item data response
                                 ,@(when id-field `(:id-field ,id-field))
                                 ,@(when id-property `(:id-property ,id-property))
                                 ,@(when (not (eq title-key :title)) `(:title-key ,title-key))
                                 ,@(when post-fn `(:post-fn ,post-fn))))))))
    (unless parse-fn (error "org-canvas-define-push-at-point: :parse is required"))
    (unless build-fn (error "org-canvas-define-push-at-point: :build is required"))
    (unless push-fn (error "org-canvas-define-push-at-point: :push or :endpoint is required"))
    (unless finalize-fn (error "org-canvas-define-push-at-point: :finalize or :endpoint is required"))
    `(progn
       ;;;###autoload
       (defun ,fn-name ()
         ,(format "Sync the %s at point to Canvas." feature-name)
         (interactive)
         (org-canvas--push-at-point-runtime
          ,feature-name ,parse-fn ,build-fn ,push-fn ,finalize-fn
          ,title-key ,pull-item-fn)))))

;;;; 10. Setup Wizard

(defconst org-canvas--skeleton-files
  '("assignments.org" "pages.org" "quizzes.org" "modules.org"
    "files.org" "outcomes.org" "rubrics.org" "discussions.org"
    "announcements.org" "assignment-groups.org" "sections.org"
    "settings.org")
  "List of org files to create in a new course skeleton.")

(defun org-canvas--write-credentials-file (dir url token course-id)
  "Write org-canvas-credentials.el in DIR with URL, TOKEN, COURSE-ID."
  (let ((file (expand-file-name "org-canvas-credentials.el" dir)))
    (with-temp-file file
      (insert ";;; org-canvas-credentials.el --- Course credentials  -*- lexical-binding: t; -*-\n\n")
      (insert ";; This file is NOT checked into version control.\n")
      (insert ";; It contains sensitive API credentials.\n\n")
      (insert (format "(setq org-canvas-directory %S)\n" dir))
      (insert (format "(setq org-canvas-base-url %S)\n" url))
      (insert (format "(setq org-canvas-api-token %S)\n" token))
      (insert (format "(setq org-canvas-course-id %S)\n" course-id))
      (insert "\n(provide 'org-canvas-credentials)\n")
      (insert ";;; org-canvas-credentials.el ends here\n"))
    file))

(defun org-canvas--create-skeleton-files (dir)
  "Create minimal skeleton .org files in DIR."
  (dolist (filename org-canvas--skeleton-files)
    (let ((file (expand-file-name filename dir)))
      (unless (file-exists-p file)
        (with-temp-file file
          (insert (format "#+TITLE: %s\n"
                          (capitalize (file-name-sans-extension filename))))
          (insert "# See documentation/manual.org for property reference\n"))))))

;;;###autoload
(defun org-canvas-init ()
  "Set up org-canvas for a new course.
Prompts for required configuration, tests the connection,
and writes org-canvas-credentials.el."
  (interactive)
  (let* ((dir (read-directory-name "Course directory: " nil nil t))
         (url (read-string "Canvas base URL: " "https://canvas.instructure.com"))
         (token (read-string "API token: "))
         (course-id (read-string "Course ID: ")))
    ;; Validate inputs
    (when (string-empty-p token)
      (user-error "API token cannot be empty"))
    (when (string-empty-p course-id)
      (user-error "Course ID cannot be empty"))
    ;; Test connection before writing config
    (message "Testing connection...")
    (let ((org-canvas-base-url url)
          (org-canvas-api-token token)
          (org-canvas-course-id course-id))
      (condition-case err
          (let ((course (org-canvas-api-request 'GET
                          (org-canvas-api-course-endpoint ""))))
            (message "Connected to: %s" (alist-get 'name course)))
        (error
         (if (y-or-n-p (format "Connection failed: %s\nSave credentials anyway? "
                               (error-message-string err)))
             (message "Saving credentials without connection verification...")
           (user-error "Aborted")))))
    ;; Write credentials
    (let ((cred-file (org-canvas--write-credentials-file dir url token course-id)))
      (message "Credentials saved to %s" cred-file))
    ;; Optionally create skeleton files
    (when (y-or-n-p "Create skeleton .org files for all content types? ")
      (org-canvas--create-skeleton-files dir)
      (message "Created skeleton files in %s" dir))
    ;; Load the new credentials
    (setq org-canvas-directory dir)
    (setq org-canvas-base-url url)
    (setq org-canvas-api-token token)
    (setq org-canvas-course-id course-id)
    (message "org-canvas initialized!  Use M-x org-canvas-status to see sync state.")))

;;;; 11. File Upload Infrastructure
;;
;; Self-contained 3-step Canvas file upload, independent of the
;; files.el module so any feature module can upload files.

(defun org-canvas--guess-content-type (filename)
  "Guess MIME type for FILENAME based on extension."
  (let ((ext (downcase (or (file-name-extension filename) ""))))
    (cond
     ((string= ext "png")  "image/png")
     ((member ext '("jpg" "jpeg")) "image/jpeg")
     ((string= ext "gif")  "image/gif")
     ((string= ext "svg")  "image/svg+xml")
     ((string= ext "webp") "image/webp")
     ((string= ext "bmp")  "image/bmp")
     ((string= ext "pdf")  "application/pdf")
     (t "application/octet-stream"))))

(defun org-canvas--upload-build-multipart (upload-params local-path boundary)
  "Build multipart/form-data body for file upload.
UPLOAD-PARAMS is the alist from Canvas step 1 response.
LOCAL-PATH is the local file path.
BOUNDARY is the multipart boundary string.
Returns a unibyte string."
  (let ((body-parts nil)
        (file-content (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally local-path)
                        (buffer-string)))
        (actual-filename (file-name-nondirectory local-path))
        (actual-content-type (org-canvas--guess-content-type local-path)))
    ;; Build form fields from upload_params
    (dolist (param (append upload-params nil))
      (let* ((key (car param))
             (raw-value (cdr param))
             ;; Fix Canvas nulls/unknowns
             (value (cond
                     ((and (eq key 'filename) (null raw-value))
                      actual-filename)
                     ((and (eq key 'content_type)
                           (or (null raw-value)
                               (equal raw-value "unknown/unknown")))
                      actual-content-type)
                     (t raw-value))))
        (when value
          (push (format "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s"
                        boundary key value)
                body-parts))))
    ;; File parameter must be LAST
    (push (format "--%s\r\nContent-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\nContent-Type: %s\r\n\r\n"
                  boundary actual-filename actual-content-type)
          body-parts)
    ;; Encode form parts as unibyte before concatenating with binary
    (let* ((body-prefix (encode-coding-string
                         (concat (mapconcat #'identity (nreverse body-parts) "\r\n") "\r\n")
                         'raw-text))
           (body-suffix (encode-coding-string
                         (format "\r\n--%s--\r\n" boundary)
                         'raw-text)))
      (concat body-prefix file-content body-suffix))))

(defun org-canvas--upload-file (local-path &optional notify-url display-name)
  "Upload LOCAL-PATH to Canvas via the 3-step upload API.
NOTIFY-URL is the step 1 endpoint (defaults to course files).
DISPLAY-NAME overrides the filename shown in Canvas.
Returns the Canvas file object alist (with \\='id key)."
  (let* ((filename (or display-name (file-name-nondirectory local-path)))
         (size (file-attribute-size (file-attributes local-path)))
         (content-type (org-canvas--guess-content-type local-path))
         (url (or notify-url
                  (format "%s/api/v1/courses/%s/files"
                          org-canvas-base-url org-canvas-course-id)))
         (payload `((name . ,filename)
                    (size . ,size)
                    (content_type . ,content-type))))
    (elog-info org-canvas--logger "[Upload Step 1] Notifying Canvas for '%s'..." filename)
    ;; Step 1: Notify Canvas
    (let ((upload-info (org-canvas-api-request 'POST url :data payload)))
      (let* ((upload-url (alist-get 'upload_url upload-info))
             (upload-params (alist-get 'upload_params upload-info))
             (boundary (format "----FormBoundary%s"
                               (md5 (format "%s%s" (current-time) (random))))))
        (elog-info org-canvas--logger "[Upload Step 2] Sending file to %s..." upload-url)
        ;; Step 2: Upload the file
        (let* ((full-body (org-canvas--upload-build-multipart
                           upload-params local-path boundary))
               (url-request-method "POST")
               (url-request-extra-headers
                `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
               (url-request-data full-body)
               (step2-response
                (with-current-buffer (url-retrieve-synchronously upload-url nil nil 120)
                  (goto-char (point-min))
                  (let (location-header json-response)
                    (save-excursion
                      (when (re-search-forward "^[Ll]ocation: \\(.*\\)\r?$" nil t)
                        (setq location-header (string-trim (match-string 1)))))
                    (when (re-search-forward "\r?\n\r?\n" nil t)
                      (setq json-response
                            (condition-case nil
                                (json-read-from-string
                                 (buffer-substring-no-properties (point) (point-max)))
                              (error nil))))
                    (kill-buffer)
                    (cond
                     ((and json-response (alist-get 'id json-response))
                      json-response)
                     (location-header
                      `((location . ,location-header)))
                     (json-response json-response)
                     (t (error "Upload failed: no JSON or Location header")))))))
          ;; Step 3: Confirm upload
          (elog-info org-canvas--logger "[Upload Step 3] Confirming upload...")
          (if (alist-get 'id step2-response)
              (progn
                (elog-info org-canvas--logger "[Upload] Complete: file ID %s"
                           (alist-get 'id step2-response))
                step2-response)
            (let* ((location (alist-get 'location step2-response))
                   (full-url (if (string-prefix-p "http" location)
                                 location
                               (concat org-canvas-base-url location)))
                   (response (org-canvas-api-request 'GET full-url)))
              (elog-info org-canvas--logger "[Upload] Complete: file ID %s"
                         (alist-get 'id response))
              response)))))))

(provide 'org-canvas-core-sync)
;;; org-canvas-core-sync.el ends here
