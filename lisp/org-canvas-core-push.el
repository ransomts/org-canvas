;;; org-canvas-core-push.el --- Push guards: drift, conflict, recovery, echo  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; What stands between a payload and Canvas's copy of it.  The pipeline in
;; org-canvas-core-sync.el decides *whether* an entry is pushed; this file
;; owns the guards a push passes through: the remote snapshot that proves
;; the file's payload hash still describes Canvas, the per-item conflict
;; check and its diff prompt, the timeout/404 recovery that adopts a twin
;; instead of creating a duplicate, the duplicate-title guard that runs
;; before every POST, the children reconcilers a parent's finalize uses,
;; and the echo check that compares what Canvas stored with what it was
;; sent.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)
(require 'org-canvas-core-pull)
(require 'org-canvas-core-conflict)
(require 'org-canvas-core-sync)

;;;; 0. Option Bundles
;;
;; The runtimes on the push path took a tail of arguments that only ever
;; travelled together: `org-canvas--push-to-api' seven keyword arguments,
;; `org-canvas--finalize-item' seven more, `org-canvas--conflict-check'
;; eight positional ones.  Every call site named them all, and the
;; signature said nothing about which ones belonged together.  Each is
;; now one bundle, built where the module knows its wiring (issue #592).
;; `org-canvas--conflict-request' keeps a second positional constructor
;; for the call sites that were already positional.

(cl-defstruct (org-canvas--push-options (:copier nil)
               (:constructor org-canvas--push-options))
  "Where and how one item is pushed to Canvas.
ENDPOINT is the API suffix.  ID-KEY and TITLE-KEY are the plist keys
the parsed data spells the item's Canvas id and display name under.
FIND-FN looks the item up by title after a timeout.  POST-URL-FN and
PUT-URL-FN build the create and update URLs where the course-scoped
default is not where the feature lives.  UPDATED-FIELD names the
response field that tracks modification: `updated_at' unless a module
passes `modified_at', as files do.  CTX is the run context."
  endpoint id-key title-key find-fn post-url-fn put-url-fn
  updated-field ctx)

(cl-defstruct (org-canvas--conflict-request
               (:copier nil)
               (:constructor org-canvas--conflict-request)
               (:constructor org-canvas--conflict-request-from
                             (endpoint id pom
                                       &optional title modified-field
                                       params url scheduled-fn)))
  "One item's conflict check.
ENDPOINT and ID name the item, POM the heading whose baseline is
compared, TITLE the entry in the log line.  MODIFIED-FIELD names the
response field that tracks content modification, PARAMS the query the
read carries, URL the item's address where the course-scoped default
404s, and SCHEDULED-FN the feature's answer to whether a newer remote
timestamp is a bump Canvas makes itself."
  endpoint id pom title modified-field params url scheduled-fn)

(cl-defstruct (org-canvas--restamp-options (:copier nil)
               (:constructor org-canvas--restamp-options))
  "What `org-canvas--finalize-restamp-updated' re-reads.
POM is the heading to stamp, ENDPOINT and ID the item to read, FIELD
the response field holding its timestamp, TITLE the entry in the log,
and CTX the run context whose feature says where the item lives."
  pom endpoint id field title ctx)

(cl-defstruct (org-canvas--finalize-options (:copier nil)
               (:constructor org-canvas--finalize-options))
  "How `org-canvas--finalize-item' stamps one pushed entry.
ID-FIELD and ID-PROPERTY say where the id is read from the response and
which Org property holds it.  TITLE-KEY is the plist key of the display
name.  UPDATED-FIELD names the response field stamped into
CANVAS_UPDATED_AT.  POST-FN, when non-nil, runs after the stamp, and
ENDPOINT with CTX is what lets a post-fn that writes to Canvas again
have its baseline re-read (issue #124)."
  id-field id-property title-key updated-field post-fn endpoint ctx)

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
  (let ((times-ref (org-canvas--ctx-get ctx :remote-times))
        (updated (and (listp response) (alist-get 'updated_at response))))
    (when (and times-ref (stringp updated))
      (push updated (car times-ref)))))

(defun org-canvas--sync-file-baseline (sync-file)
  "Return SYNC-FILE\\='s #+LAST_SYNCED header as an Emacs time, or nil."
  (when (file-exists-p sync-file)
    (with-current-buffer (org-canvas--find-file-noselect sync-file)
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
  (let* ((remote-updated (org-canvas--ctx-get ctx :remote-updated))
         (updated (and remote-updated canvas-id
                       (gethash (format "%s" canvas-id) remote-updated)))
         (baseline (and updated
                        (org-canvas--conflict-baseline
                         (point) (org-canvas--ctx-get ctx :baseline))))
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
  "Return the remote items in CTX's snapshot that carry TITLE, or nil.
Nil too when the run has no snapshot (:remote-titles is `none')."
  (let ((titles (org-canvas--ctx-get ctx :remote-titles)))
    (and (hash-table-p titles) title (gethash title titles))))

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
        (why (if (org-canvas--ctx-get ctx :baseline)
                 "the remote snapshot was unavailable"
               "this file has no #+LAST_SYNCED baseline yet (one is written after this run)")))
    (when (and org-canvas-detect-conflicts
               (not (org-canvas--ctx-get ctx :remote-updated)))
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
  (let* ((times (car (org-canvas--ctx-get ctx :remote-times)))
         (newest (car (sort (copy-sequence times) #'string>)))
         (time (and newest (org-canvas--parse-iso8601-time newest))))
    (when time
      (with-current-buffer (org-canvas--find-file-noselect sync-file)
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
  (let ((remote-updated (org-canvas--ctx-get ctx :remote-updated)))
    (when (and remote-updated
               (org-canvas--ctx-get ctx :baseline)
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

(defconst org-canvas--scheduled-bump-window 60
  "Seconds after a scheduled date within which a timestamp bump is Canvas's.
Canvas moves an assignment's `updated_at' when its `unlock_at' passes,
with nothing edited: five seconds after it, on the course that
reported it (issue #453).  The allowance covers a busy job queue; a
web edit made inside it is taken for the bump.")

(defun org-canvas--conflict-scheduled-bump (local-time remote-time dates)
  "Return the entry of DATES that explains REMOTE-TIME, or nil.
DATES is a list of (LABEL . ISO8601), the dates at which Canvas
touches the item on its own.  One explains the remote timestamp when
it passed after LOCAL-TIME, the baseline, and REMOTE-TIME sits no more
than `org-canvas--scheduled-bump-window' seconds after it (issue #453)."
  (cl-find-if
   (lambda (entry)
     (let ((time (org-canvas--parse-iso8601-time (cdr entry))))
       (and time
            (time-less-p local-time time)
            (not (time-less-p remote-time time))
            (<= (float-time (time-subtract remote-time time))
                org-canvas--scheduled-bump-window))))
   dates))

(defun org-canvas--conflict-scheduled-dates (scheduled-fn response title)
  "Return what SCHEDULED-FN answers for RESPONSE, or nil when it fails.
A failure is logged under TITLE and answers nil, so the newer
timestamp stays a conflict: a read that could not explain the bump
must not let the push past it (issue #453)."
  (when scheduled-fn
    (condition-case err
        (funcall scheduled-fn response)
      (error
       (org-canvas--log-warning org-canvas--logger
         "[Conflict] Could not read the scheduled dates of '%s' (%s); a bump they would explain stays a conflict"
         title (error-message-string err))
       nil))))

(cl-defun org-canvas--conflict-check (request)
  "Compare REQUEST's item with the baseline its heading recorded.
REQUEST is an `org-canvas--conflict-request'.  Its ENDPOINT and ID name
the item, its POM the heading whose baseline is compared, and its TITLE
the entry in the log line; without a TITLE, ENDPOINT/ID does.
Its MODIFIED-FIELD names the response field that tracks content
modification, default `updated_at'; files pass `modified_at', because
Canvas bumps their `updated_at' on metadata-only touches and comparing
it re-flagged files whose bytes never changed (issue #94).
Its PARAMS are query parameters the read carries — the feature's
`:item-params', since the response is what the conflict prompt's pull
writes into the heading, and an assignment read without them carries a
student's extension as its due date (issue #273).
Its URL, when non-nil, is the item's address, as
`org-canvas--sync-item-url' resolves it; without it ENDPOINT/ID is read
under the course, which 404s for a feature that does not live there —
calendar events are global, and every one of them used to push
unguarded (issue #344).
Its SCHEDULED-FN, when non-nil, is the feature's `:scheduled-dates-fn':
a remote timestamp that one of the dates it answers explains is not an
edit, and the push goes ahead (`org-canvas--conflict-scheduled-bump',
issue #453).
Returns (cons \\='conflict REMOTE-RESPONSE) if the remote item is newer,
nil otherwise.  Returns nil on GET failure (allows push to proceed) or
when there is no baseline at all (first sync).

The log line names the baseline actually compared — the entry's
CANVAS_UPDATED_AT, or the file header when it has none — rather than
the header regardless (issue #86)."
  (let* ((endpoint (org-canvas--conflict-request-endpoint request))
         (id (org-canvas--conflict-request-id request))
         (pom (org-canvas--conflict-request-pom request))
         (title (org-canvas--conflict-request-title request))
         (modified-field (org-canvas--conflict-request-modified-field request))
         (params (org-canvas--conflict-request-params request))
         (url (org-canvas--conflict-request-url request))
         (scheduled-fn (org-canvas--conflict-request-scheduled-fn request))
         (source (org-canvas--conflict-baseline-source pom))
         (local-time (car source)))
    (unless local-time
      (cl-return-from org-canvas--conflict-check nil))
    (condition-case err
        (let* ((field (or modified-field 'updated_at))
               (full-url (or url (org-canvas-api-course-endpoint
                                  (format "%s/%%s" endpoint) id)))
               (response (org-canvas-api-request 'GET full-url :params params))
               (updated-at (alist-get field response))
               (remote-time (org-canvas--parse-iso8601-time updated-at)))
          (when (and remote-time (time-less-p local-time remote-time))
            (let* ((label (or title (format "%s/%s" endpoint id)))
                   (bump (org-canvas--conflict-scheduled-bump
                          local-time remote-time
                          (org-canvas--conflict-scheduled-dates
                           scheduled-fn response label))))
              (if bump
                  (progn
                    (org-canvas--log-info org-canvas--logger
                      "[Conflict] '%s': remote %s %s is newer than %s, but it follows %s %s, a bump Canvas makes itself; not a conflict"
                      label field updated-at (cdr source) (car bump) (cdr bump))
                    nil)
                (org-canvas--log-warning org-canvas--logger
                  "[Conflict] '%s': remote %s %s is newer than %s"
                  label field updated-at (cdr source))
                (cons 'conflict response)))))
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

(defun org-canvas--recovery-find-twin (find-fn title id-key)
  "Return the Canvas item FIND-FN reports under TITLE, or nil.
Asked before the POST a 404 recovery would otherwise send: the stamped
id is gone, but the title may live on under another id — a course copy,
or a create whose stamp was lost — and a second copy is the last thing
a recovery should make (issue #179).  ID-KEY says which field names
the item (`url' for pages).  Nothing is looked up without a FIND-FN,
or when `org-canvas-duplicate-title-strategy' is `create'."
  (when (and find-fn (not (eq org-canvas-duplicate-title-strategy 'create)))
    (condition-case err
        (let ((found (funcall find-fn title)))
          (and found (org-canvas--push-item-id found id-key) found))
      (error
       (org-canvas--log-warning org-canvas--logger
         "[Recovery] Could not look '%s' up by title (%s); creating it again"
         title (error-message-string err))
       nil))))

(defun org-canvas--recovery-update-twin (payload options twin title)
  "Update TWIN, the item on Canvas under TITLE, with PAYLOAD.
OPTIONS is the push's `org-canvas--push-options': its ENDPOINT is the
base endpoint, its PUT-URL-FN, when non-nil, builds the item URL from
an id, and its ID-KEY names the field the id is read from.
Returns the API response, whose id finalize stamps over the stale
one."
  (let* ((endpoint (org-canvas--push-options-endpoint options))
         (put-url-fn (org-canvas--push-options-put-url-fn options))
         (id-key (or (org-canvas--push-options-id-key options) :canvas-id))
         (id (org-canvas--push-item-id twin id-key))
         (url (if put-url-fn
                  (funcall put-url-fn id)
                (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) id))))
    (org-canvas--log-warning org-canvas--logger
      "[Recovery] '%s' is gone under its stamped id, but Canvas holds the title as id %s — updating that one instead of creating a second copy"
      title id)
    (let ((response (org-canvas-api-request 'PUT url :data payload)))
      (org-canvas--log-info org-canvas--logger "[Recovery] PUT successful for '%s'" title)
      response)))

(defun org-canvas--handle-404-retry (payload options title)
  "Recover from a 404 on PUT: update the item's twin, or create it again.
PAYLOAD is the data to send and TITLE names the item in the log.
OPTIONS is the push's `org-canvas--push-options': its FIND-FN, with
TITLE, first looks for an item Canvas still holds under the title
\(issue #179), and serves timeout recovery on the retry; its
POST-URL-FN, when non-nil, overrides the default course-scoped POST
URL; its PUT-URL-FN builds the URL of an adopted twin from its id,
which its ID-KEY (default `:canvas-id'; pages pass `:canvas-url')
reads from the item."
  (let* ((endpoint (org-canvas--push-options-endpoint options))
         (find-fn (org-canvas--push-options-find-fn options))
         (post-url-fn (org-canvas--push-options-post-url-fn options))
         (put-url-fn (org-canvas--push-options-put-url-fn options))
         (id-key (or (org-canvas--push-options-id-key options) :canvas-id))
         (twin (org-canvas--recovery-find-twin find-fn title id-key)))
    (if twin
        (org-canvas--recovery-update-twin payload options twin title)
      (org-canvas--log-warning org-canvas--logger "[Recovery] Item not found (404). Retrying as POST...")
      (let ((new-endpoint (or (and post-url-fn (funcall post-url-fn))
                              (org-canvas-api-course-endpoint endpoint))))
        (condition-case post-err
            (let ((response (org-canvas-api-request 'POST new-endpoint :data payload)))
              (org-canvas--log-info org-canvas--logger "[Recovery] POST successful")
              response)
          (error
           (if (and find-fn (org-canvas--timeout-error-p post-err))
               (org-canvas--handle-timeout-recovery find-fn title post-err)
             (signal (car post-err) (cdr post-err)))))))))

(defun org-canvas--sync-item-scheduled-fn (ctx)
  "Return the `:scheduled-dates-fn' of the feature CTX is running, or nil.
CTX is the run context; its :feature-name finds the registry entry
\(issue #453)."
  (let ((name (org-canvas--ctx-get ctx :feature-name)))
    (when name
      (org-canvas--feature-scheduled-dates-fn
       (org-canvas--registry-find-feature name)))))

(defun org-canvas--sync-item-params (ctx)
  "Return the query parameters for a single-item read in this run.
CTX is the run context; its :feature-name finds the registry entry,
whose `:item-params' answer.  Nil for a custom push that passes no
context, or a feature outside the registry."
  (let ((name (org-canvas--ctx-get ctx :feature-name)))
    (when name
      (org-canvas--feature-item-params
       (org-canvas--registry-find-feature name)))))

(defun org-canvas--sync-item-url (endpoint id &optional ctx put-url-fn)
  "Return the URL of item ID under ENDPOINT for a read in this run.
PUT-URL-FN, the push's own item-URL function, wins when given.
Otherwise CTX's :feature-name finds the registry entry, and its
`:item-url-fn' answers through `org-canvas--feature-item-url'.  Only
then is ID appended to ENDPOINT under the course: calendar events live
at the global /calendar_events/:id, and a course-scoped read of one
404s (issue #344)."
  (let ((feature (and (org-canvas--ctx-get ctx :feature-name)
                      (org-canvas--registry-find-feature
                       (org-canvas--ctx-get ctx :feature-name)))))
    (cond
     (put-url-fn (funcall put-url-fn id))
     ((plist-get feature :item-url-fn)
      (org-canvas--feature-item-url feature id))
     (t (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) id)))))

(defun org-canvas--push-check-and-resolve-conflict (data id options)
  "Check for conflicts on the item DATA names, under ID, using OPTIONS.
OPTIONS is the push's `org-canvas--push-options': its ENDPOINT names
the item, its TITLE-KEY is where DATA spells the title for the log, its
UPDATED-FIELD is what `org-canvas--conflict-check' compares (files
pass `modified_at', issue #94), its CTX is the run context and its
PUT-URL-FN the item's address.
CTX's :pull-item-fn makes the pull option available, its
:conflict-apply-all remembers a capital answer, and its :feature-name
finds the `:item-params' the check reads with, so what the pull option
writes is the item's own dates (issue #273).  PUT-URL-FN and CTX
resolve the address read, through `org-canvas--sync-item-url'
\(issue #344).  CTX's feature also says which dates explain a newer
remote timestamp that nobody edited
\(`org-canvas--sync-item-scheduled-fn', issue #453).
Returns `push', `skip', or `pulled'."
  (let* ((endpoint (org-canvas--push-options-endpoint options))
         (title-key (or (org-canvas--push-options-title-key options) :title))
         (title (plist-get data title-key))
         (ctx (org-canvas--push-options-ctx options))
         (put-url-fn (org-canvas--push-options-put-url-fn options))
         (request (org-canvas--conflict-request
                   :endpoint endpoint
                   :id id
                   :pom (plist-get data :pom)
                   :title title
                   :modified-field (org-canvas--push-options-updated-field options)
                   :params (org-canvas--sync-item-params ctx)
                   :url (org-canvas--sync-item-url endpoint id ctx put-url-fn)
                   :scheduled-fn (org-canvas--sync-item-scheduled-fn ctx)))
         (conflict-result (org-canvas--conflict-check request)))
    (if (not (and conflict-result (eq (car conflict-result) 'conflict)))
        'push
      (let* ((remote-response (cdr conflict-result))
             (effective-pull-fn (org-canvas--ctx-get ctx :pull-item-fn))
             (resolution (org-canvas--resolve-conflict data remote-response ctx)))
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
;;
;; Child levels — quiz questions, New Quiz items, module items — push
;; outside `org-canvas--push-to-api', so the guard never sees them, and
;; they are where a lost stamp is likeliest: a git checkout that
;; reverts a stamped file drops every child's id at once.  Their parent
;; already lists its children in one request; an unstamped child that
;; matches a listed item no sibling heading claims adopts it instead
;; (issue #179).  Module items keep their own spelling in modules.el,
;; which also feeds the run context; the helpers below serve the rest.

(defun org-canvas--child-twin-before-p (a b)
  "Return non-nil when remote item A is ordered before B: position, then id."
  (let ((pa (or (alist-get 'position a) 0))
        (pb (or (alist-get 'position b) 0))
        (ia (alist-get 'id a))
        (ib (alist-get 'id b)))
    (cond ((/= pa pb) (< pa pb))
          ((and (numberp ia) (numberp ib)) (< ia ib))
          (t (string< (format "%s" ia) (format "%s" ib))))))

(defun org-canvas--child-twins (remote match-p claimed)
  "Return the items of REMOTE that MATCH-P accepts and CLAIMED does not name.
REMOTE is a list or vector of item alists, or the symbol `unknown'
\(a list that could not be read), which yields nil.  CLAIMED is the
list of id strings the sibling headings carry.  Ordered by position,
then id, so the earliest twin is adopted and the rest are named."
  (unless (eq remote 'unknown)
    (sort (cl-remove-if-not
           (lambda (item)
             ;; An item is an alist; anything else in the list (a bare
             ;; cons from a reply that was not a list) is not a twin.
             (and (consp item) (consp (car item))
                  (alist-get 'id item)
                  (not (member (format "%s" (alist-get 'id item)) claimed))
                  (funcall match-p item)))
           (append remote nil))
          #'org-canvas--child-twin-before-p)))

(defun org-canvas--adopt-child-twin (data remote match-p claimed label)
  "Give unstamped DATA the id of the REMOTE item it matches, if any.
MATCH-P is called with each remote item alist; CLAIMED names the ids
sibling headings already carry, so two headings with the same title
take two twins rather than one.  LABEL prefixes the log lines.  On a
match DATA's :canvas-id is set in place, so the push that follows is
an update, and finalize writes the stamp; further twins are named and
left alone (children are never pruned unasked).  Nothing is adopted
when DATA carries an id, when REMOTE is `unknown', or when
`org-canvas-duplicate-title-strategy' is `create'.  Returns the
adopted id as a string, or nil."
  (when (and (not (plist-get data :canvas-id))
             (not (eq org-canvas-duplicate-title-strategy 'create)))
    (let ((twins (org-canvas--child-twins remote match-p claimed))
          (title (or (plist-get data :title) (plist-get data :name))))
      (when twins
        (let ((id (format "%s" (alist-get 'id (car twins)))))
          (org-canvas--log-info org-canvas--logger
            "%s '%s' has no id yet, but Canvas already holds it as %s — adopting it; updating in place instead of creating a second copy"
            label title id)
          (when (cdr twins)
            (org-canvas--log-warning org-canvas--logger
              "%s Canvas holds %d more item(s) matching '%s' (%s) — left in place; delete them in Canvas"
              label (length (cdr twins)) title
              (mapconcat (lambda (item) (format "%s" (alist-get 'id item)))
                         (cdr twins) ", ")))
          (plist-put data :canvas-id id)
          id)))))

(defun org-canvas--sync-children (markers label item-fn)
  "Run ITEM-FN at each heading at MARKERS and count the outcomes.
ITEM-FN is called with point on the heading, in the heading's own
buffer, for the parse/adopt/build/push/finalize of one child; the
symbol `skip' counts the child as skipped rather than synced, and
anything else as a success.  An error ITEM-FN signals is logged as one
LABEL line — LABEL is the bracketed tag the module's other child log
lines carry, as `org-canvas--adopt-child-twin' takes it — and counted
as a failure, and the loop moves on.  MARKERS are released afterwards.
Returns a plist (:success N :skip N :fail N :total N); the caller
owns how the counts are reported, since the modules disagree about
denominator and wording."
  (let ((success 0) (skip 0) (fail 0))
    (dolist (m markers)
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char (marker-position m))
          (condition-case err
              (if (eq (funcall item-fn) 'skip)
                  (setq skip (1+ skip))
                (setq success (1+ success)))
            (error
             (setq fail (1+ fail))
             (org-canvas--log-error org-canvas--logger
               "%s Failed: %s" label (error-message-string err)))))))
    (dolist (m markers) (set-marker m nil))
    (list :success success :skip skip :fail fail :total (length markers))))

(defun org-canvas--push-remote-items-titled (title find-fn &optional ctx)
  "Return the remote items carrying TITLE, or nil.
Inside a sync CTX's :remote-titles is the snapshot's title index, read
for free, or `none' when the run had no snapshot — an unregistered
feature, a failed fetch, conflict detection off — in which case nothing
is checked rather than a GET spent per entry.  Outside a sync (a
single-entry push, whose context carries no titles) FIND-FN, the
module's search function, is asked; a module without one is not
checked."
  (let ((titles (org-canvas--ctx-get ctx :remote-titles)))
    (cond
     ((hash-table-p titles)
      (gethash title titles))
     (titles nil)
     (find-fn
      (let ((found (funcall find-fn title)))
        (and found (list found)))))))

(defun org-canvas--push-item-id (item id-key)
  "Return ITEM's id as a string, from the field ID-KEY implies.
`:canvas-url' reads `url' (pages); anything else reads `id'."
  (let ((id (alist-get (if (eq id-key :canvas-url) 'url 'id) item)))
    (and id (format "%s" id))))

(defun org-canvas--item-id-value (item id-field)
  "Return ITEM's id from ID-FIELD, a key or a list of keys tried in order.
A list names the fields a reply may carry the id under, the preferred
one first: a New Quiz's is `(assignment_id id)' (issue #309)."
  (if (consp id-field)
      (cl-some (lambda (field) (alist-get field item)) id-field)
    (alist-get id-field item)))

(defun org-canvas--adopt-stamp (pom id-property item &optional modified-field
                                    id-field)
  "Stamp the heading at POM as the owner of Canvas ITEM.
Writes ID-PROPERTY from ITEM's ID-FIELD (see
`org-canvas--item-id-value'), by default its `url' for pages and its
`id' otherwise, CANVAS_UPDATED_AT
from MODIFIED-FIELD (default `updated_at'; files pass `modified_at')
so the next comparison is against the item's own clock rather than
reporting the adoption itself as a conflict, and removes PAYLOAD_HASH
so the next sync verifies content it has never compared instead of
taking the unchanged-skip.  This is the one spelling of adoption,
shared by the duplicate-title guard and `org-canvas-adopt-at-point'
\(issue #101).  Returns the id as a string, or nil when ITEM has none."
  (let* ((field (or id-field
                    (if (equal id-property "CANVAS_URL") 'url 'id)))
         (id (org-canvas--item-id-value item field))
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

(defun org-canvas--push-guard-duplicate (data id-key title find-fn &optional ctx)
  "Before creating TITLE, look it up on Canvas and decide what to do.
Returns nil to go ahead and create, `skip' to leave the heading
alone, or the adopted id — DATA and its heading updated — to PUT
instead.  Not consulted when `org-canvas-duplicate-title-strategy' is
`create', when DATA has no :pom to stamp, or when nothing on Canvas
carries TITLE.  ID-KEY, FIND-FN and CTX are as for
`org-canvas--push-to-api'."
  (unless (or (eq org-canvas-duplicate-title-strategy 'create)
              (null (plist-get data :pom)))
    (let ((items (org-canvas--push-remote-items-titled title find-fn ctx)))
      (when items
        (let* ((ids (mapcar (lambda (item) (org-canvas--push-item-id item id-key))
                            items))
               (id-list (mapconcat #'identity ids ", "))
               (action (org-canvas--resolve-duplicate title ids ctx)))
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

(cl-defun org-canvas--push-to-api (data payload options)
  "Generic push-to-API with 404 retry and optional timeout recovery.

DATA is the parsed entry plist (must contain :canvas-id or :canvas-url).
PAYLOAD is the API payload to send.
OPTIONS is an `org-canvas--push-options' saying where and how the item
is pushed:
  ENDPOINT names the API endpoint suffix (\"assignments\", \"pages\").
  ID-KEY and TITLE-KEY name the keys in DATA that spell the item's
    Canvas ID (default :canvas-id) and title (default :title).
  FIND-FN is an optional function (TITLE) searching for the item after
    a timeout.
  POST-URL-FN and PUT-URL-FN are optional functions building the POST
    and PUT URLs where the course-scoped default is not where the
    feature lives.
  UPDATED-FIELD names the response field the conflict check compares:
    default `updated_at'.
  CTX is the run context (`org-canvas--sync-make-ctx'): the snapshot's
    title index for the create guard, the module's pull function for
    the conflict prompt, and the run's remembered capital answers.  Nil
    from a caller outside any run, which behaves as a single push: the
    guard asks FIND-FN, and no answer is remembered.

Handle:
  - POST for new items (no ID), PUT for existing items
  - A title Canvas already holds: asks, or follows
    `org-canvas-duplicate-title-strategy' — adopting the id turns the
    POST into a PUT (issue #85)
  - 404 on PUT: retries as POST (stale ID recovery)
  - Timeout: calls FIND-FN to check if item was created

Returns the API response alist, or one of the symbols `conflict',
`pulled' and `duplicate' when the push stopped short."
  (let* ((endpoint (org-canvas--push-options-endpoint options))
         (id-key (or (org-canvas--push-options-id-key options) :canvas-id))
         (title-key (or (org-canvas--push-options-title-key options) :title))
         (find-fn (org-canvas--push-options-find-fn options))
         (post-url-fn (org-canvas--push-options-post-url-fn options))
         (put-url-fn (org-canvas--push-options-put-url-fn options))
         (ctx (org-canvas--push-options-ctx options))
         (title (plist-get data title-key))
         (guard (and (not (plist-get data id-key))
                     (not org-canvas--dry-run)
                     (org-canvas--push-guard-duplicate data id-key title find-fn ctx)))
         (id (plist-get data id-key))
         (method (if id 'PUT 'POST))
         (full-endpoint (cond
                         ((and id put-url-fn) (funcall put-url-fn id))
                         ((and (not id) post-url-fn) (funcall post-url-fn))
                         (id (org-canvas-api-course-endpoint (format "%s/%%s" endpoint) id))
                         (t (org-canvas-api-course-endpoint endpoint)))))

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
                       data id options)))
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

        ;; CASE 2: 404 on PUT -> update the title's twin, else retry as POST
        ((org-canvas--404-on-put-p err method)
         (org-canvas--handle-404-retry payload options title))

        ;; Default: Re-throw
        (t (signal (car err) (cdr err))))))))

(defun org-canvas--finalize-note-remote-write (ctx)
  "Declare that the running finalize post-fn has written to Canvas again.
CTX is the run context the post-fn received as its third argument.
The push response's timestamp is no longer what Canvas holds, so
`org-canvas--finalize-run-post-fn' re-reads the item and restamps the
baseline from it (issue #124)."
  (plist-put ctx :remote-touched t))

(defun org-canvas--finalize-restamp-updated (restamp)
  "Re-read RESTAMP's item and stamp its field into CANVAS_UPDATED_AT.
RESTAMP is an `org-canvas--restamp-options': its POM is the heading to
stamp, its ENDPOINT and ID the item to read, its FIELD the response
field holding the timestamp, its TITLE the entry in the log, and its
CTX the run context, whose feature says where the item lives
\(`org-canvas--sync-item-url', issue #344).
A post-fn that writes to Canvas again — the rubric association is the
one that started this — moves the item's timestamp past the stamp
finalize just wrote from the push response.  The baseline is then
permanently behind by the length of that write: `org-canvas-diff' calls
the entry CHANGED with no compared property differing, and the next
push walks into the conflict check, both over the sync's own doing
\(issue #124).  One GET, spent only when a post-fn reported a write,
leaves the heading's baseline equal to what Canvas holds.  A failed
read is logged and leaves the stamp alone."
  (let ((pom (org-canvas--restamp-options-pom restamp))
        (endpoint (org-canvas--restamp-options-endpoint restamp))
        (id (org-canvas--restamp-options-id restamp))
        (field (org-canvas--restamp-options-field restamp))
        (title (org-canvas--restamp-options-title restamp))
        (ctx (org-canvas--restamp-options-ctx restamp)))
    (when (and endpoint id (not org-canvas--dry-run))
      (condition-case err
          (let* ((url (org-canvas--sync-item-url endpoint id ctx))
                 (updated (alist-get (or field 'updated_at)
                                     (org-canvas-api-request 'GET url))))
            (if (not (stringp updated))
                (org-canvas--log-debug org-canvas--logger
                  "[Finalize] '%s' reports no %s to restamp after its post-sync write"
                  title (or field 'updated_at))
              (org-canvas-org-set-property pom "CANVAS_UPDATED_AT" updated)
              (org-canvas--log-info org-canvas--logger
                "[Finalize] Baseline for '%s' restamped to %s after its post-sync write"
                title updated)))
        (error
         (org-canvas--log-warning org-canvas--logger
           "[Finalize] Could not re-read '%s' after its post-sync write (%s); the next drift report may report this push as a change"
           title (error-message-string err)))))))

(defun org-canvas--finalize-run-post-fn (post-fn data response &optional restamp)
  "Call POST-FN with DATA, RESPONSE and the context, then restamp if it wrote.
RESTAMP is an `org-canvas--restamp-options' naming what
`org-canvas--finalize-restamp-updated' re-reads; its CTX, handed to
POST-FN, says whether the write happened.  The restamp runs only when
POST-FN called `org-canvas--finalize-note-remote-write' on that CTX.
The flag is cleared before and after the call, so it means this post-fn
and no other."
  (let ((ctx (org-canvas--restamp-options-ctx restamp)))
    (plist-put ctx :remote-touched nil)
    (funcall post-fn data response ctx)
    (when (org-canvas--ctx-get ctx :remote-touched)
      (org-canvas--finalize-restamp-updated restamp))
    (plist-put ctx :remote-touched nil)))

;;;; Push Echo Check
;;
;; Canvas answers a create or update with the object as it stored it,
;; and it does not always store what it was sent: a classic quiz sent
;; `scoring_policy: keep_latest' came back `keep_highest', and the push
;; reported it synced; only the next drift report showed the difference
;; (issue #349).  The check below reads the registry the way the drift
;; report does — a spec's `:remote-fn' answers for the stored side, so a
;; nested or renamed field is no false alarm — and compares each field
;; the payload carried.  It warns and never fails the push.

(defconst org-canvas--registry-echo-types
  '(boolean number timestamp enum csv-enum string)
  "Property types the push echo check compares.
`link' is left out: the payload holds an id the module resolves and
checks itself, as quizzes do for their assignment group.")

(defun org-canvas--registry-echo-checked-p (spec)
  "Return non-nil when the push echo check compares property SPEC.
A property the push never sends — Canvas's own, a declared intent,
org-canvas's bookkeeping, a pull-only record — has nothing to echo."
  (and (memq (plist-get spec :type) org-canvas--registry-echo-types)
       (not (or (plist-get spec :canvas-owned) (plist-get spec :intent-of)
                (plist-get spec :local-only) (plist-get spec :pull-only)))))

(defun org-canvas--registry-sent-cell (sent key)
  "Return (KEY . VALUE) when the payload object SENT carries KEY, else nil.
SENT is an alist or a hash table keyed by symbols or strings, the two
shapes payload builders produce."
  (if (hash-table-p sent)
      (let* ((missing (make-symbol "missing"))
             (value (gethash key sent missing)))
        (when (eq value missing)
          (setq value (gethash (symbol-name key) sent missing)))
        (unless (eq value missing) (cons key value)))
    (assq key sent)))

(defun org-canvas--registry-echo-number (value)
  "Return VALUE, a number or its string spelling, as a float, or nil."
  (cond ((numberp value) (float value))
        ((stringp value) (float (string-to-number value)))))

(defun org-canvas--registry-echo-equal-p (spec sent stored)
  "Return non-nil when STORED is the value SENT for property SPEC.
Numbers compare by value (10 and 10.0 agree), CSV lists as sets, and
everything else in its Org spelling, so a boolean's false and null
agree and two timestamps agree when they name the same minute."
  (let ((sent (org-canvas--registry-normalize-remote sent))
        (stored (org-canvas--registry-normalize-remote stored)))
    (pcase (plist-get spec :type)
      ('number (equal (org-canvas--registry-echo-number sent)
                      (org-canvas--registry-echo-number stored)))
      ('csv-enum (equal (sort (org-canvas--registry-remote-list sent) #'string<)
                        (sort (org-canvas--registry-remote-list stored)
                              #'string<)))
      (_ (equal (org-canvas--registry-remote-as-org spec sent)
                (org-canvas--registry-remote-as-org spec stored))))))

(defun org-canvas--registry-echo-field (spec sent response)
  "Compare property SPEC as the payload object SENT had it with RESPONSE.
Returns (ORG-PROP SENT STORED), the values in their Org spelling, when
Canvas stored something else; nil when they agree, when the payload
did not carry the field, or when RESPONSE does not report it."
  (when (org-canvas--registry-echo-checked-p spec)
    (let ((cell (org-canvas--registry-sent-cell
                 sent (org-canvas--registry-remote-key spec))))
      (when (and cell (org-canvas--registry-remote-present-p spec response))
        (let ((stored (org-canvas--registry-remote-field spec response)))
          (unless (org-canvas--registry-echo-equal-p spec (cdr cell) stored)
            (list (plist-get spec :org-prop)
                  (or (org-canvas--registry-remote-as-org spec (cdr cell))
                      "(unset)")
                  (or (org-canvas--registry-remote-as-org spec stored)
                      "(unset)"))))))))

(defun org-canvas--registry-echo-mismatches (feature sent response)
  "Return FEATURE's registered properties Canvas stored otherwise than SENT.
FEATURE names a registry entry (\"quizzes\"); SENT is the
payload's object, inside any wrapper key; RESPONSE is Canvas's reply
to the create or update.  Each element is (ORG-PROP SENT STORED)."
  (let ((props (org-canvas--registry-entry feature)))
    (delq nil (mapcar (lambda (spec)
                        (org-canvas--registry-echo-field spec sent response))
                      (plist-get props :properties)))))

(defun org-canvas--registry-warn-echo (feature title sent response)
  "Warn about each property of FEATURE Canvas did not store as SENT.
TITLE names the item in the log.  SENT and RESPONSE are as for
`org-canvas--registry-echo-mismatches', whose list is returned.  A
module's finalize calls this so a value Canvas quietly replaced shows
at push time, not at the next drift report (issue #349)."
  (let ((mismatches (org-canvas--registry-echo-mismatches
                     feature sent response)))
    (dolist (m mismatches)
      (org-canvas--log-warning org-canvas--logger
        "[Verify] '%s': sent %s %s, Canvas stored %s"
        title (nth 0 m) (nth 1 m) (nth 2 m)))
    mismatches))

(defun org-canvas--finalize-item (data response &optional options)
  "Finalize sync by saving Canvas ID and LAST_SYNCED.

DATA is the parsed entry plist (must contain :pom).
RESPONSE is the API response alist.
OPTIONS is an `org-canvas--finalize-options' saying how the entry is
stamped:
  ID-FIELD is the alist key for the ID in RESPONSE (default `id').
  ID-PROPERTY is the Org property to save it in (default \"CANVAS_ID\").
  TITLE-KEY is the key in DATA for the title (default :title).
  UPDATED-FIELD is the response field stamped into CANVAS_UPDATED_AT
    (default \\='updated_at).  Files use \\='modified_at so the baseline
    agrees with what drift detection compares (issue #94).
  POST-FN is an optional function (DATA RESPONSE CTX) for additional
    finalization.
  ENDPOINT is the API endpoint suffix the item lives under.  Only
    consulted when POST-FN calls
    `org-canvas--finalize-note-remote-write': the item is then re-read
    from it and CANVAS_UPDATED_AT restamped, so a push that associates
    a rubric does not report itself as drift afterwards (issue #124).
  CTX is the run context, handed to POST-FN as its third argument.  A
    caller outside any run may omit it; a fresh one is made so the
    post-fn still has somewhere to note a remote write.

Save the Canvas ID and LAST_SYNCED timestamp to the Org entry."
  (let* ((options (or options (org-canvas--finalize-options)))
         (id-field (or (org-canvas--finalize-options-id-field options) 'id))
         (id-property (or (org-canvas--finalize-options-id-property options)
                          "CANVAS_ID"))
         (title-key (or (org-canvas--finalize-options-title-key options) :title))
         (updated-field (org-canvas--finalize-options-updated-field options))
         (post-fn (org-canvas--finalize-options-post-fn options))
         (endpoint (org-canvas--finalize-options-endpoint options))
         (ctx (or (org-canvas--finalize-options-ctx options)
                  (org-canvas--sync-make-ctx)))
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
            (org-canvas--finalize-run-post-fn
             post-fn data response
             (org-canvas--restamp-options
              :pom pom :endpoint endpoint :id id :field updated-field
              :title title :ctx ctx)))
          (org-canvas--log-info org-canvas--logger "[Finalize] Complete for '%s'" title))
      (org-canvas--log-error org-canvas--logger "[Finalize] No ID in response for '%s'!" title)
      (org-canvas--signal 'org-canvas-api-error
        "No %s in API response for '%s'" id-field title))))

(provide 'org-canvas-core-push)
;;; org-canvas-core-push.el ends here
