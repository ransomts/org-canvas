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
  "When non-nil, sync shows what would happen without making API calls.")

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
    (error "%s file not found: %s" feature-upper sync-file))
  (let ((buf (find-file-noselect sync-file)))
    (when (buffer-modified-p buf)
      (if (y-or-n-p (format "%s has unsaved changes.  Save before syncing? "
                            (file-name-nondirectory sync-file)))
          (with-current-buffer buf (save-buffer))
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
            (when (and (org-entry-get (point) "LAST_SYNCED")
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
      (org-canvas--log-info org-canvas--logger "[Skip] '%s' unchanged" title)
      (message "%s [%d/%d] Skipping '%s' (unchanged)"
        cap-feature progress total-count title))
     (org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s '%s'"
        (if canvas-id "UPDATE" "CREATE") title)
      (message "%s [DRY-RUN] Would %s '%s'"
        cap-feature (if canvas-id "update" "create") title)
      (plist-put counters :dry-run (1+ (or (plist-get counters :dry-run) 0))))
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
               total-count heading-title (error-message-string err)))))))))

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
:success, :skip, :fail, and optionally :conflict, :pulled, and :dry-run counts."
  (let ((feature-upper (upcase feature-name))
        (success-count (plist-get counters :success))
        (skip-count (plist-get counters :skip))
        (fail-count (plist-get counters :fail))
        (conflict-count (or (plist-get counters :conflict) 0))
        (pulled-count (or (plist-get counters :pulled) 0))
        (dry-run-count (or (plist-get counters :dry-run) 0))
        (extra-counts 0))
    (with-current-buffer (find-file-noselect sync-file)
      (save-buffer)
      (org-canvas--log-info org-canvas--logger "Saved %s" sync-file))
    (org-canvas--log-info org-canvas--logger "========================================")
    (org-canvas--log-info org-canvas--logger ">>> %s SYNC COMPLETE" feature-upper)
    (cond
     ((> dry-run-count 0)
      (org-canvas--log-info org-canvas--logger "Would sync: %d | Skipped: %d"
        dry-run-count skip-count)
      (message "%s dry-run: %d would sync, %d skipped."
               (capitalize feature-name) dry-run-count skip-count))
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
      (let ((failed-titles (plist-get counters :failed-titles)))
        (when failed-titles
          (org-canvas--log-warning org-canvas--logger "Failed items: %s"
            (mapconcat (lambda (title) (format "'%s'" title))
                       (nreverse failed-titles) ", "))))))
    (org-canvas--log-info org-canvas--logger "========================================")
    ;; Accumulate into global counters when running inside org-canvas-sync
    (when org-canvas--sync-global-counters
      (plist-put org-canvas--sync-global-counters :success
                 (+ (plist-get org-canvas--sync-global-counters :success) success-count))
      (plist-put org-canvas--sync-global-counters :skip
                 (+ (plist-get org-canvas--sync-global-counters :skip) skip-count))
      (plist-put org-canvas--sync-global-counters :fail
                 (+ (plist-get org-canvas--sync-global-counters :fail) fail-count))
      (plist-put org-canvas--sync-global-counters :dry-run
                 (+ (or (plist-get org-canvas--sync-global-counters :dry-run) 0)
                    dry-run-count)))))

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
                                                   &optional pull-item-fn title-key)
  "Run the full sync pipeline for FEATURE-NAME.
SYNC-FILE is the expanded org file path.  QUERY is the org match query.
PARSE-FN, BUILD-FN, PUSH-FN, FINALIZE-FN are the 4-stage pipeline functions.
PULL-ITEM-FN enables interactive conflict pull.  TITLE-KEY is the plist key
for the display name in logs."
  (org-canvas-clear-log)
  (let ((org-canvas--conflict-apply-all nil)
        (org-canvas--current-pull-item-fn pull-item-fn)
        (feature-upper (upcase feature-name)))
    (org-canvas--sync-validate-file feature-upper sync-file)
    (let* ((entries (org-canvas--sync-collect-entries sync-file query feature-name))
           (targets (plist-get entries :targets))
           (all-ids-before (plist-get entries :all-ids-before))
           (counters (list :success 0 :skip 0 :fail 0 :pulled 0 :dry-run 0 :conflict 0))
           (synced-ids (list nil))
           (ctx (list :parse-fn parse-fn
                      :build-fn build-fn
                      :push-fn push-fn
                      :finalize-fn finalize-fn
                      :feature-name feature-name
                      :feature-upper feature-upper
                      :total-count (length targets)
                      :counters counters
                      :synced-ids synced-ids
                      :title-key title-key)))
      (dolist (marker targets)
        (org-canvas--sync-process-entry marker ctx))
      (dolist (m targets) (set-marker m nil))
      (org-canvas--sync-warn-orphans all-ids-before (car synced-ids) feature-name)
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
       ;;;###autoload
       (defun ,sync-fn-name ()
         ,(format "Synchronize %s to Canvas using the 4-stage pipeline." feature-name)
         (interactive)
         (org-canvas--sync-run-pipeline
          ,feature-name (expand-file-name ,file-expr)
          ,query ,parse-fn ,build-fn ,push-fn ,finalize-fn
          ,pull-item-fn ,title-key))
       ,@(unless no-at-point
           `(;;;###autoload
             (defun ,at-point-fn-name ()
               ,(format "Sync the %s at point to Canvas." singular)
               (interactive)
               (org-canvas--push-at-point-runtime
                ,singular
                ,parse-fn ,build-fn ,push-fn ,finalize-fn
                ,(or title-key :title) ,pull-item-fn)))))))

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
                (org-canvas--log-warning org-canvas--logger
                  "[Conflict] Remote item updated at %s, local LAST_SYNCED is %s"
                  updated-at (org-entry-get pom "LAST_SYNCED"))
                (cons 'conflict response))
            nil))
      (error nil))))

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
  - 404 on PUT: retries as POST (stale ID recovery)
  - Timeout: calls FIND-FN to check if item was created

Returns the API response alist."
  (let* ((id-key (or id-key :canvas-id))
         (title-key (or title-key :title))
         (id (plist-get data id-key))
         (title (plist-get data title-key))
         (method (if id 'PUT 'POST))
         (full-endpoint (cond
                         ((and id put-url-fn) (funcall put-url-fn id))
                         ((and (not id) post-url-fn) (funcall post-url-fn))
                         (id (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) id))
                         (t (org-canvas-api-course-endpoint endpoint))))
         (post-url (when post-url-fn (funcall post-url-fn))))

    ;; Dry-run: skip API call and return a mock response
    (when org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would %s '%s' to %s" method title full-endpoint)
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

    (org-canvas--log-info org-canvas--logger "[Execute] %s '%s' to %s" method title full-endpoint)

    (condition-case err
        (let ((response (org-canvas-api-request method full-endpoint :data payload)))
          (org-canvas--log-info org-canvas--logger "[Execute] %s successful for '%s'" method title)
          response)
      (error
       (org-canvas--log-error org-canvas--logger "[Execute] Failed: %s" (error-message-string err))

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

    (org-canvas--log-debug org-canvas--logger "[Finalize] Processing response for '%s'" title)

    (if id
        (progn
          (org-canvas--log-info org-canvas--logger "[Finalize] Saving %s=%s for '%s'" id-property id title)
          (org-canvas-org-save-sync-state pom id id-property)
          ;; Save CANVAS_UPDATED_AT for conflict detection
          (let ((updated-at (alist-get 'updated_at response)))
            (when updated-at
              (org-canvas-org-set-property pom "CANVAS_UPDATED_AT"
                                           (format "%s" updated-at))))
          (when post-fn
            (funcall post-fn data response))
          (org-canvas--log-info org-canvas--logger "[Finalize] Complete for '%s'" title))
      (org-canvas--log-error org-canvas--logger "[Finalize] No ID in response for '%s'!" title)
      (error "No %s in API response for '%s'" id-field title))))

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
  (org-canvas--log-info org-canvas--logger ">>> SYNC-AT-POINT: %s" feature-name)
  (let* ((org-canvas--current-pull-item-fn pull-item-fn)
         (data (funcall parse-fn))
         (title (plist-get data title-key))
         (payload (funcall build-fn data))
         (payload-hash (md5 (json-encode payload)))
         (stored-hash (org-entry-get (point) "PAYLOAD_HASH"))
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
        (org-canvas--log-info org-canvas--logger "[Stage 4: Finalize] '%s'" title)
        (funcall finalize-fn data response)
        (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
        (save-buffer)
        (org-canvas--log-info org-canvas--logger "[Sync] '%s' synced successfully" title)
        (message "%s '%s' synced." (capitalize feature-name) title)))))


(provide 'org-canvas-core-sync)
;;; org-canvas-core-sync.el ends here
