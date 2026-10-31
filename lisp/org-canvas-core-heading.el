;;; org-canvas-core-heading.el --- At-point and by-heading commands  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The per-entry commands a feature module's `org-canvas-define-sync'
;; generates share a runtime here, and the batch verbs that name one
;; heading instead of walking a file are built on it: push at point
;; (`org-canvas-define-push-at-point'), sync by heading, pull by heading
;; and delete by heading.  Each finds the heading, runs the feature's own
;; at-point function there, and reports one line per heading.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core-config)
(require 'org-canvas-core-org)
(require 'org-canvas-core-pull)
(require 'org-canvas-core-conflict)
(require 'org-canvas-core-delete)
(require 'org-canvas-core-sync)
(require 'org-canvas-core-push)

;;;; 9. Push-at-Point Infrastructure

(defun org-canvas--push-at-point-finalize
    (finalize-fn data response ctx payload-hash title)
  "Finalize the push at point of TITLE and stamp its heading.
FINALIZE-FN is called with DATA, RESPONSE and CTX; PAYLOAD-HASH, when
non-nil, is stored on the heading and the buffer saved.  An error in
stamping is logged as a push that landed, then re-signalled."
  (org-canvas--log-info org-canvas--logger "[Stage 4: Finalize] '%s'" title)
  (condition-case err
      (progn
        (funcall finalize-fn data response ctx)
        (when payload-hash
          (org-canvas-org-set-property (point) org-canvas--prop-payload-hash payload-hash))
        (org-canvas--sync-advance-header-from-entry)
        (org-canvas--save-buffer))
    (error
     ;; The push landed; only the stamp died (issue #97).
     (org-canvas--log-error org-canvas--logger
       "[Stamp] The push of '%s' landed on Canvas, but stamping the file failed: %s — the entry still carries its old CANVAS_UPDATED_AT and PAYLOAD_HASH, so the next sync will report drift that is not real"
       title (error-message-string err))
       (signal (car err) (cdr err)))))

(defun org-canvas--push-at-point-runtime (spec)
  "Runtime body for generated push-at-point functions.
SPEC is a sync spec (`org-canvas--sync-spec-keys'): :feature is the
singular module name, :parse, :build, :push and :finalize the four
stage functions, all required.  :title-key is the plist key of the
display name (default :title); :pull-item-fn, when non-nil, enables
the pull option during conflict resolution; :hash-extra, when
non-nil, is folded into the payload hash (see
`org-canvas--sync-payload-hash'); :hash and :prepare mean what they
mean in a full run.  Returns the context, so a caller can read what
the push recorded in it — its :outcome above all: `synced',
`unchanged', `conflict', `pulled', `duplicate', `fragment' (a create
refused as a stranded body fragment, issue #391) or `dry-run' (issue
#287).  The push runs in a context of its own, so a capital answer at
its conflict prompt is forgotten when it returns rather than applied
to every later push at point (issue #141)."
  (org-canvas--sync-check-spec spec '(:feature :parse :build :push :finalize))
  (org-back-to-heading t)
  (org-canvas--sync-refuse-deleted)
  ;; The heading's file is the buffer's, so the scope check needs no
  ;; spec key here; a sync by heading funnels through this runtime and
  ;; is covered with it (issue #550).
  (org-canvas--check-write-scope (buffer-file-name) "a push at point")
  (display-buffer (get-buffer-create org-canvas--log-buffer-name))
  (let* ((feature-name (plist-get spec :feature))
         ;; Advances past the #+LAST_SYNCED header finalize may insert
         ;; above a file's first heading, where point would stay behind.
         (heading (copy-marker (point) t))
         (parse-fn (plist-get spec :parse))
         (build-fn (plist-get spec :build))
         (push-fn (plist-get spec :push))
         (finalize-fn (plist-get spec :finalize))
         (title-key (or (plist-get spec :title-key) :title))
         (pull-item-fn (plist-get spec :pull-item-fn))
         (hash-extra-fn (plist-get spec :hash-extra))
         (prepare-fn (plist-get spec :prepare))
         (ctx (progn
                (org-canvas--log-info org-canvas--logger
                                      ">>> SYNC-AT-POINT: %s" feature-name)
                (org-canvas--sync-make-ctx :feature-name feature-name
                                           :pull-item-fn pull-item-fn
                                           :hash-extra-fn hash-extra-fn
                                           :hash-fn (plist-get spec :hash))))
         (data (progn
                 (when prepare-fn
                   (plist-put ctx :prepared (funcall prepare-fn ctx)))
                 (funcall parse-fn)))
         (title (plist-get data title-key))
         (payload (funcall build-fn data))
         (payload-hash (org-canvas--sync-entry-hash payload data ctx))
         (stored-hash (org-entry-get (point) org-canvas--prop-payload-hash))
         (canvas-id (or (plist-get data :canvas-id)
                        (plist-get data :canvas-url))))
    (org-canvas--log-info org-canvas--logger "[Stage 2: Build] '%s'" title)
    ;; What :after-heading may hold against Canvas's copy (issue #410).
    (plist-put ctx :heading-payload payload)
    (if (and payload-hash
             stored-hash
             (string= payload-hash stored-hash)
             canvas-id)
        (progn
          (plist-put ctx :outcome 'unchanged)
          (org-canvas--log-info org-canvas--logger "[Skip] '%s' unchanged" title)
          (message "%s '%s' unchanged — skipped." (capitalize feature-name) title))
      (org-canvas--log-info org-canvas--logger "[Stage 3: Push] '%s' (%s)"
        title (if canvas-id "UPDATE" "CREATE"))
      (let ((response (if (org-canvas--sync-refuse-fragment-p canvas-id title)
                          'fragment
                        (funcall push-fn data payload ctx))))
        (cond
         ((memq response '(conflict pulled duplicate fragment))
          (plist-put ctx :outcome response)
          (org-canvas--push-at-point-report-stop feature-name title response))
         ((eq response 'skip)
          (plist-put ctx :outcome 'unchanged)
          (org-canvas--log-info org-canvas--logger "[Skip] '%s' nothing to send" title)
          (message "%s '%s' unchanged — skipped." (capitalize feature-name) title))
         ;; A dry run's sentinel is not a response: finalizing it would
         ;; stamp CANVAS_ID "dry-run" into the heading (Hard Rule 1).
         ((org-canvas--dry-run-response-p response)
          (plist-put ctx :outcome 'dry-run)
          (org-canvas--log-info org-canvas--logger "[DRY-RUN] '%s' not stamped" title)
          (message "%s '%s' would be pushed (dry run)." (capitalize feature-name) title))
         (t
          (org-canvas--push-at-point-finalize
           finalize-fn data response ctx payload-hash title)
          (plist-put ctx :outcome 'synced)
          (org-canvas--log-info org-canvas--logger "[Sync] '%s' synced successfully" title)
          (message "%s '%s' synced." (capitalize feature-name) title)))))
    (org-canvas--push-at-point-after-heading spec ctx heading)
    (set-marker heading nil)
    ctx))

(defun org-canvas--push-at-point-after-heading (spec ctx heading)
  "Run SPEC's :after-heading on CTX when the heading's push went through.
The push went through when CTX's :outcome is `synced', `unchanged' or
`dry-run': a heading stopped at a conflict or a duplicate sends nothing
more.  The function is called with point on HEADING, a marker, and
CTX; its result is kept as CTX's :heading-report and the buffer is
saved, since it may have restamped the heading (issue #380).  CTX's
:heading-payload is the payload the push built, whether or not it
was sent (issue #410)."
  (let ((fn (plist-get spec :after-heading)))
    (when (and fn (memq (org-canvas--ctx-get ctx :outcome) '(synced unchanged dry-run)))
      (goto-char heading)
      (org-back-to-heading t)
      (plist-put ctx :heading-report (funcall fn ctx))
      (org-canvas--save-buffer))))

(defun org-canvas--push-at-point-report-stop (feature-name title outcome)
  "Say why the single-entry push of TITLE stopped with OUTCOME.
FEATURE-NAME names the module.  OUTCOME is `conflict', `pulled' or
`duplicate', the symbols `org-canvas--push-to-api' returns in place
of a response, or `fragment', a create the runtime refused before the
push (issue #391); finalizing one as if it were a response is a type
error, which is how a conflict at point used to end in a backtrace."
  (let ((why (pcase outcome
               ('conflict "not pushed — the remote item was modified since the last sync")
               ('pulled "not pushed — the remote version was pulled instead")
               ('fragment "not created — it has no CANVAS_ID and looks like a body fragment an older pull left behind; fold it into the item above, or confirm the create")
               (_ "not pushed — Canvas already holds this title; adopt it with M-x org-canvas-adopt-at-point or rename"))))
    (org-canvas--log-warning org-canvas--logger "[Sync] '%s' %s" title why)
    (message "%s '%s' %s." (capitalize feature-name) title why)))


;;;; 10. Sync by Heading (issue #287)
;;
;; The at-point commands are the only per-entry push, and they are
;; interactive by design: point, current buffer.  A batch script that
;; wanted to push two named headings opened the course file itself,
;; searched for the heading by regexp, called the at-point command and
;; saved — twelve lines rewritten in every push script, each a place to
;; get it wrong (a regexp that matches a longer title, a heading found
;; in the wrong file, a save skipped after the stamp).  The same gap on
;; the submissions side was issue #280.
;;
;; `org-canvas-define-sync' now also generates org-canvas-sync-<singular>
;; \(TARGET &optional BY): the file comes from the module's :file, the
;; heading is found by exact title or, with BY `canvas-id', by its stamp
;; — an error on zero or several matches — and the existing at-point
;; runtime runs at that position and saves.  `org-canvas-sync-headings'
;; takes a list of them.

(defvar org-canvas--sync-heading-fns nil
  "Alist of (SINGULAR . FUNCTION): every feature's sync by heading.
FUNCTION takes (TARGET &optional BY).  `org-canvas-define-sync' adds an
entry for each feature it defines; a module with a hand-written
command adds its own with `org-canvas--sync-register-heading-fn'.
`org-canvas-sync-headings' resolves its feature names here, plural or
singular, so a misspelt feature is named before anything is sent.")

(defun org-canvas--sync-register-heading-fn (singular fn)
  "Record FN as the sync by heading of the feature called SINGULAR.
A second registration for the same name replaces the first, so a
reloaded module does not leave a stale function behind."
  (setf (alist-get singular org-canvas--sync-heading-fns nil nil #'equal) fn))

(defvar org-canvas--sync-entry-specs nil
  "Alist of (SINGULAR . SPEC): the sync spec of every push at point.
SPEC is the plist `org-canvas-define-sync' hands
`org-canvas--push-at-point-runtime' (`org-canvas--sync-spec-keys').  A
command that must hash a heading as its push would, without pushing
it, reads the stage functions here (`org-canvas--sync-heading-hash',
issue #466).")

(defun org-canvas--sync-register-entry-spec (singular spec)
  "Record SPEC as the push at point of the feature called SINGULAR.
A second registration for the same name replaces the first."
  (setf (alist-get singular org-canvas--sync-entry-specs nil nil #'equal) spec))

(defun org-canvas--sync-heading-hash (spec)
  "Return the payload hash SPEC's push would compare at the heading at point.
SPEC is a sync spec (`org-canvas--sync-entry-specs').  The heading is
parsed and its payload built exactly as a push at point would, but
with `org-canvas-read-only' bound, so an inline image Canvas lacks is
not uploaded on the way (it fails, the link stays local, and the hash
then differs from any a push stored).  Nil when the spec owns no
md5 (`:hash' `push'), or the parse or build fails."
  (condition-case err
      (save-excursion
        (org-back-to-heading t)
        (let* ((org-canvas-read-only t)
               (data (funcall (plist-get spec :parse)))
               (payload (funcall (plist-get spec :build) data)))
          (org-canvas--sync-entry-hash
           payload data
           (org-canvas--sync-make-ctx
            :hash-extra-fn (plist-get spec :hash-extra)
            :hash-fn (plist-get spec :hash)))))
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Hash] Could not hash the heading as a push would (%s)"
       (error-message-string err))
     nil)))

(defun org-canvas--heading-fn-lookup (feature fns verb)
  "Return the function for FEATURE in FNS, an alist (SINGULAR . FUNCTION).
FEATURE is the module's name as a symbol or string, plural or singular
\(`assignments' or `assignment'); a name FNS lacks is a `user-error'
naming VERB and listing the ones that exist."
  (let* ((name (if (symbolp feature) (symbol-name feature) feature))
         (fn (cdr (assoc (org-canvas--singularize name) fns))))
    (or fn
        (user-error "No %s by heading for %s; the features are: %s" verb name
                    (mapconcat #'car (reverse fns) ", ")))))

(defun org-canvas--sync-heading-fn-for (feature)
  "Return the sync by heading of FEATURE, a symbol or string.
FEATURE is the module's name, plural or singular (`assignments' or
`assignment'); a name no module registered is a `user-error' listing
the ones that exist."
  (org-canvas--heading-fn-lookup feature org-canvas--sync-heading-fns "sync"))

(defun org-canvas--sync-heading-text ()
  "Return the text of the heading at point as written, or nil.
The title alone: no stars, TODO keyword, priority, tags or trailing
blanks, but with its link markup intact — `org-complex-heading-regexp'
group 4, the way the modules whose headings are links read it, since
`org-get-heading' strips link syntax on Org 9.7 and keeps it before."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (let ((text (match-string-no-properties 4)))
        (and text (string-trim text))))))

(defun org-canvas--sync-heading-matches-p (target by id-property)
  "Return non-nil when the heading at point is the one TARGET names.
With BY nil or `title', TARGET is the heading's exact text: as
written, as `org-get-heading' reads it, or with each link reduced to
its description, so the files.org heading
[[file:../syllabus.pdf][syllabus.pdf]] also answers to \"syllabus.pdf\".
`org-get-heading' keeps link markup before Org 9.7 and strips it after,
so only the reduced form names a link heading by its description on
every Emacs (issue #446).  With BY `canvas-id', TARGET is the value of
ID-PROPERTY, a string or an integer.  Any other BY is a `user-error'."
  (pcase by
    ((or 'nil 'title)
     (let ((text (org-canvas--sync-heading-text)))
       (member target (list text
                            (and text (org-link-display-format text))
                            (string-trim (or (org-get-heading t t t t) ""))))))
    ('canvas-id
     (equal (format "%s" target) (org-entry-get (point) id-property)))
    (_ (user-error "Sync by heading: BY must be nil, `title' or `canvas-id', not %S" by))))

(defun org-canvas--sync-heading-markers (file query pred)
  "Return a marker at each heading of FILE selected by QUERY that PRED accepts.
QUERY is the module's `org-map-entries' match; PRED is called with
point at each heading.  FILE is visited through
`org-canvas--find-file-noselect' (Hard Rule 8)."
  (let ((found nil))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-map-entries (lambda () (when (funcall pred) (push (point-marker) found)))
                       query 'file))
    (nreverse found)))

(cl-defun org-canvas--sync-find-heading (&key file query target by
                                              id-property feature verb)
  "Return a marker at the one heading of FILE that TARGET names.
QUERY, BY and ID-PROPERTY are as in `org-canvas--sync-heading-matches-p';
FEATURE names the module in the error, and VERB (default \"sync\") what
to do at point instead.  Every argument is a keyword.  No match and
more than one match are both a `user-error': a script must never push
the wrong heading, and two headings with one title or one stamp are a
file to fix (or a job for the at-point command) rather than a guess."
  (let ((markers (org-canvas--sync-heading-markers
                  file query
                  (lambda () (org-canvas--sync-heading-matches-p target by id-property))))
        (what (if (eq by 'canvas-id) (format "%s %s" id-property target)
                (format "titled %S" target))))
    (pcase (length markers)
      (0 (user-error "No %s heading %s in %s" feature what (file-name-nondirectory file)))
      (1 (car markers))
      (n (user-error "%d %s headings %s in %s; %s one at point instead"
                     n feature what (file-name-nondirectory file)
                     (or verb "sync"))))))

(defun org-canvas--sync-heading-titles (file query)
  "Return the text of every heading of FILE under QUERY, in file order."
  (mapcar (lambda (m) (with-current-buffer (marker-buffer m)
                        (save-excursion (goto-char m) (org-canvas--sync-heading-text))))
          (org-canvas--sync-heading-markers file query (lambda () t))))

(defun org-canvas--sync-heading-ask (feature file query &optional verb)
  "Read the title of a FEATURE heading of FILE, among those under QUERY.
Under `noninteractive' there is nobody to ask, and a script that
passed no target is told so rather than left reading standard input.
VERB (default \"sync\") names the command in that message."
  (when noninteractive
    (user-error "No %s heading named, and nothing to ask in batch (org-canvas-%s-%s)"
                feature (or verb "sync") feature))
  (completing-read (format "%s heading: " (capitalize feature))
                   (org-canvas--sync-heading-titles file query) nil t))

(defun org-canvas--sync-heading-runtime (spec target &optional by)
  "Sync the heading TARGET names to Canvas and return the run context.
SPEC is a sync spec (`org-canvas--sync-spec-keys') with :file and
:query beside the four stage functions; :id-property is the property
BY `canvas-id' matches TARGET against (default CANVAS_ID).  TARGET nil
asks for a title.  The heading is found with
`org-canvas--sync-find-heading' and pushed by
`org-canvas--push-at-point-runtime' at that position, which stamps
and saves; the context's :outcome says how it ended."
  (org-canvas--sync-check-spec spec '(:feature :file :query :parse :build :push :finalize))
  (let* ((feature (plist-get spec :feature))
         (file (plist-get spec :file))
         (query (plist-get spec :query))
         (target (or target (org-canvas--sync-heading-ask feature file query)))
         (marker (org-canvas--sync-find-heading
                  :file file :query query :target target :by by
                  :id-property (or (plist-get spec :id-property) "CANVAS_ID")
                  :feature feature)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-canvas--push-at-point-runtime spec)))))

(defun org-canvas--sync-headings-entry (entry)
  "Return ENTRY of `org-canvas-sync-headings' as a list (FEATURE TARGET BY).
ENTRY is (FEATURE . TARGET) with a string TARGET, or (FEATURE TARGET)
or (FEATURE TARGET BY); anything else is a `user-error'.
`org-canvas-pull-headings' reads its entries here too."
  (cond ((and (consp entry) (stringp (cdr entry)))
         (list (car entry) (cdr entry) nil))
        ((and (consp entry) (listp (cdr entry)) (nth 1 entry))
         (list (car entry) (nth 1 entry) (nth 2 entry)))
        (t (user-error "Headings: %S is not (FEATURE . TITLE) or (FEATURE TARGET BY)"
                       entry))))

(defun org-canvas--sync-headings-run-one (fn feature target by &optional label)
  "Call FN, the sync by heading of FEATURE, on TARGET and BY; return a result.
The result is a plist (:feature :target :outcome), :outcome the
context's, followed by its :heading-report (an assignment's override
counts, issue #380), or `failed' with :error naming what went wrong: one
heading's failure is reported and the next one is pushed, as in a full
run.  LABEL (default \"Sync headings\") tags the log line; the pull by
heading runs its entries here too."
  (condition-case err
      (let ((ctx (funcall fn target by)))
        (append (list :feature feature :target target
                      :outcome (org-canvas--ctx-get ctx :outcome))
                (org-canvas--ctx-get ctx :heading-report)))
    (error
     (org-canvas--log-error org-canvas--logger "[%s] %s '%s' failed: %s"
       (or label "Sync headings") feature target (error-message-string err))
     (list :feature feature :target target :outcome 'failed
           :error (error-message-string err)))))

(defun org-canvas--sync-headings-report (results)
  "Say how RESULTS, the list `org-canvas-sync-headings' built, came out.
One line per heading, redacted since an error's text is not the
package's own, then one line of counts."
  (dolist (r results)
    (org-canvas--user-message "%s '%s': %s%s%s"
      (plist-get r :feature) (plist-get r :target) (plist-get r :outcome)
      (if (plist-get r :note) (format "; %s" (plist-get r :note)) "")
      (if (plist-get r :error) (format " — %s" (plist-get r :error)) "")))
  (let ((count (lambda (o) (cl-count o results :key (lambda (r) (plist-get r :outcome))))))
    (message "Synced %d heading(s): %d synced, %d unchanged, %d stopped, %d failed"
             (length results)
             (funcall count 'synced)
             (funcall count 'unchanged)
             (+ (funcall count 'conflict) (funcall count 'pulled)
                (funcall count 'duplicate) (funcall count 'fragment)
                (funcall count 'dry-run))
             (funcall count 'failed))))

;;;###autoload
(defun org-canvas-sync-headings (entries)
  "Sync ENTRIES, named headings of several features, one after another.
Each entry is (FEATURE . TITLE), or (FEATURE TARGET BY) where BY is
nil, `title' or `canvas-id' (see `org-canvas--sync-heading-matches-p');
FEATURE is a module's name, plural or singular.  Every feature is
resolved before the first push, so a misspelt one stops the batch
with nothing sent; after that a failed heading is reported and the
next is pushed.  Requests are paced as any sync's are
\\(`org-canvas-request-min-interval').  Return one plist per entry,
\\(:feature :target :outcome), with :error on a failure — the same
words the closing lines print.  An assignment with a `#+NAME:
overrides' table also reconciles that table and adds :overrides, its
\(CREATED UPDATED DELETED) counts or `failed', and a :note saying so
\(issue #380).  For example:

  (org-canvas-sync-headings
   \\='((assignment . \"R5: Framework Strengths\")
     (page . \"Week 05 objectives\")
     (assignment \"2563805\" canvas-id)))"
  (let ((results (org-canvas--headings-run
                  entries #'org-canvas--sync-heading-fn-for "Sync headings")))
    (org-canvas--sync-headings-report results)
    results))

(defun org-canvas--headings-run (entries fn-for label)
  "Run each of ENTRIES through its by-heading function, as FN-FOR resolves it.
ENTRIES are read by `org-canvas--sync-headings-entry'; FN-FOR maps a
feature name to its function, and every one is resolved before the
first runs, so a misspelt feature stops the batch with nothing done.
LABEL tags a failure's log line.  Return the results of
`org-canvas--sync-headings-run-one', one per entry."
  (let* ((parsed (mapcar #'org-canvas--sync-headings-entry entries))
         (fns (mapcar (lambda (e) (funcall fn-for (car e))) parsed)))
    (cl-mapcar (lambda (fn e)
                 (org-canvas--sync-headings-run-one
                  fn (car e) (nth 1 e) (nth 2 e) label))
               fns parsed)))


;;;; 11. Pull by Heading (issue #346)
;;
;; `org-canvas-pull-at-point' accepts one Canvas-side edit, but only at
;; point and after a y-or-n-p.  A batch script taking Canvas's version of
;; one named heading had to find it by regexp, redefine the private
;; `org-canvas--confirm' for the rest of the session, pull and save —
;; the pull side of the gap #287 closed for pushes.
;;
;; `org-canvas-define-sync' now also generates org-canvas-pull-<singular>
;; \(TARGET &optional BY) for every feature with a single-item pull: the
;; heading is found as the sync by heading finds it, the at-point pull's
;; target and fetch run there without a prompt (naming the heading is the
;; answer), and the file is saved.  `org-canvas-pull-headings' takes a
;; list.  The names cannot meet the whole-file pulls: those are plural
;; \(org-canvas-pull-assignments), these singular
;; \(org-canvas-pull-assignment), and a feature whose singular is its
;; plural is refused at expansion by `org-canvas--pull-heading-fn-forms'.

(defun org-canvas--pull-at-point-feature ()
  "Return the feature entry for the current buffer's file.
Signals a `user-error' when the buffer is not a course file, or when
its feature has no pull-item function to refresh a heading with.  A
file outside the feature registry may still have a pull entry (New
Quizzes, issue #297); see `org-canvas--pull-feature-for-file'."
  (let* ((file (buffer-file-name))
         (feature (org-canvas--pull-feature-for-file file)))
    (unless feature
      (user-error "%s is not one of this course's Canvas files"
                  (if file (file-name-nondirectory file) "This buffer")))
    (unless (plist-get feature :pull-item-fn)
      (user-error "%s has no single-item pull; use M-x org-canvas-pull-%s"
                  (plist-get feature :name)
                  (downcase (replace-regexp-in-string
                             " " "-" (plist-get feature :name)))))
    feature))

(defun org-canvas--pull-at-point-target ()
  "Return (FEATURE ID TITLE) for the single-item pull of the heading at point.
FEATURE is the file's entry (`org-canvas--pull-at-point-feature'), ID
the heading's stamp and TITLE its text.  A feature that pulls whole
entries (`:pull-whole-entry') climbs to the level-1 heading first, and
point is left there: a quiz's question is part of the quiz.  A heading
with no stamp is a `user-error', since nothing on Canvas answers it."
  (org-back-to-heading t)
  (let ((feature (org-canvas--pull-at-point-feature)))
    (when (plist-get feature :pull-whole-entry)
      (while (org-up-heading-safe)))
    (let* ((id-property (or (plist-get feature :id-property) "CANVAS_ID"))
           (id (org-entry-get (point) id-property))
           (title (org-get-heading t t t t)))
      (unless id
        (user-error "'%s' has no %s — nothing on Canvas to pull from" title
                    id-property))
      (list feature id title))))

(defun org-canvas--pull-at-point-1 (feature id title)
  "Overwrite the heading at point with FEATURE's Canvas item ID.
TITLE names the heading in the log.  The read carries FEATURE's
`:item-params', so an assignment comes back with its own dates rather
than a student's extension (issue #273).  The write is
`org-canvas--conflict-pull-local', which restamps CANVAS_UPDATED_AT
and drops PAYLOAD_HASH.  Returns `pulled', or `gone' when Canvas
answers that the item was deleted: the heading is then marked
CANVAS_DELETED, as a list pull marks it, rather than the 404 failing
the command (issue #399)."
  (let ((remote (org-canvas--pull-item-read feature id)))
    (if (eq remote 'gone)
        (org-canvas--pull-at-point-mark-gone feature id)
      (org-canvas--conflict-pull-local
       (list :pom (point-marker)) remote (plist-get feature :pull-item-fn))
      (org-canvas--log-info org-canvas--logger
        "[Pull] Refreshed '%s' from Canvas (%s %s)"
        title (plist-get feature :name) id)
      (message "Pulled '%s' from Canvas." title)
      'pulled)))

(defun org-canvas--pull-at-point-mark-gone (feature id)
  "Mark the heading at point deleted on Canvas, FEATURE's item ID; return `gone'.
The heading and its text stay, and the buffer is saved."
  (let* ((marker (point-marker))
         (mark (org-canvas--pull-mark-gone marker))
         (note (format "'%s' is no longer on Canvas (%s %s); heading kept, marked %s %s"
                       (car mark) (plist-get feature :name) id
                       org-canvas--prop-canvas-deleted (cdr mark))))
    (set-marker marker nil)
    (org-canvas--save-buffer)
    (org-canvas--log-warning org-canvas--logger "[Pull] %s" note)
    (message "%s" note)
    'gone))

(defvar org-canvas--pull-heading-fns nil
  "Alist of (SINGULAR . FUNCTION): every feature's pull by heading.
FUNCTION takes (TARGET &optional BY).  `org-canvas-define-sync' adds an
entry for each feature with a single-item pull; a module with a
hand-written command adds its own with
`org-canvas--pull-register-heading-fn'.  `org-canvas-pull-headings'
resolves its feature names here.")

(defun org-canvas--pull-register-heading-fn (singular fn)
  "Record FN as the pull by heading of the feature called SINGULAR.
A second registration for the same name replaces the first."
  (setf (alist-get singular org-canvas--pull-heading-fns nil nil #'equal) fn))

(defun org-canvas--pull-heading-fn-for (feature)
  "Return the pull by heading of FEATURE, a symbol or string.
FEATURE is plural or singular; a name no module registered is a
`user-error' listing the ones that exist."
  (org-canvas--heading-fn-lookup feature org-canvas--pull-heading-fns "pull"))

(defun org-canvas--pull-heading-here (singular)
  "Pull the heading at point for the feature SINGULAR names; return a result.
No prompt: the caller named the heading.  Under a dry run nothing is
read or written.  Otherwise the at-point pull runs and the buffer is
saved.  The result is a plist (:outcome :title :id), :outcome `pulled',
`dry-run' or `deleted' when Canvas answered that the item was deleted
and the heading was marked instead (issue #399)."
  (run-hooks 'org-canvas--operation-start-hook)
  (pcase-let ((`(,feature ,id ,title) (org-canvas--pull-at-point-target)))
    (if org-canvas--dry-run
        (progn
          (message "%s '%s' would be pulled (dry run)."
                   (capitalize singular) title)
          (list :outcome 'dry-run :title title :id id))
      (let ((outcome (org-canvas--pull-at-point-1 feature id title)))
        (org-canvas--save-buffer)
        ;; A heading Canvas deleted is marked, not pulled (issue #399).
        (list :outcome (if (eq outcome 'gone) 'deleted 'pulled)
              :title title :id id)))))

(defun org-canvas--pull-heading-runtime (singular file query id-property
                                                  target &optional by)
  "Pull the SINGULAR heading of FILE that TARGET names from Canvas.
QUERY and ID-PROPERTY are the module's, as its sync by heading uses
them; TARGET and BY mean what they mean there
\(`org-canvas--sync-find-heading'), and TARGET nil asks for a title.
Return the plist `org-canvas--pull-heading-here' builds, with
:feature and :target."
  (let* ((target (or target
                     (org-canvas--sync-heading-ask singular file query "pull")))
         (marker (org-canvas--sync-find-heading
                  :file file :query query :target target :by by
                  :id-property id-property :feature singular :verb "pull")))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (append (list :feature singular :target target)
                (org-canvas--pull-heading-here singular))))))

(defun org-canvas--pull-headings-report (results)
  "Say how RESULTS, the list `org-canvas-pull-headings' built, came out.
One redacted line per heading, then one line of counts."
  (dolist (r results)
    (org-canvas--user-message "%s '%s': %s%s"
      (plist-get r :feature) (plist-get r :target) (plist-get r :outcome)
      (if (plist-get r :error) (format " — %s" (plist-get r :error)) "")))
  (let ((count (lambda (o) (cl-count o results
                                     :key (lambda (r) (plist-get r :outcome))))))
    (message "Pulled %d heading(s): %d pulled, %d dry run, %d failed%s"
             (length results) (funcall count 'pulled)
             (funcall count 'dry-run) (funcall count 'failed)
             (let ((deleted (funcall count 'deleted)))
               (if (zerop deleted) ""
                 (format ", %d deleted on Canvas" deleted))))))

;;;###autoload
(defun org-canvas-pull-headings (entries)
  "Replace ENTRIES, named headings of several features, with Canvas's versions.
The pull twin of `org-canvas-sync-headings', and its ENTRIES are
written the same way: (FEATURE . TITLE), or (FEATURE TARGET BY) where
BY is nil, `title' or `canvas-id'.  Every feature is resolved before
the first pull; after that a failed heading is reported and the next
is pulled.  Nothing asks to confirm.  Return one plist per entry,
\(:feature :target :outcome), :outcome `pulled', `dry-run', `deleted'
\(the heading marked CANVAS_DELETED) or `failed' with :error.  For example:

  (org-canvas-pull-headings
   \\='((assignment . \"Attendance 01\")
     (page \"week-05-objectives\" canvas-id)))"
  (let ((results (org-canvas--headings-run
                  entries #'org-canvas--pull-heading-fn-for "Pull headings")))
    (org-canvas--pull-headings-report results)
    results))
;;;; 12. Delete by Heading (issue #525)
;;
;; Deleting one Canvas item by its heading from a script meant finding
;; the heading by regexp and stubbing `org-canvas--confirm' and
;; `y-or-n-p' for the rest of the run, which silenced every other
;; question with the one meant.  `org-canvas-delete-headings' finds each
;; heading as the sync by heading does and runs the feature's own
;; `org-canvas-delete-FEATURE-at-point' there, registered by
;; `org-canvas-define-delete-at-point'; the confirmation is answered in
;; advance by binding `org-canvas-delete-confirmed', which answers that
;; question alone.

(defun org-canvas--delete-heading-feature (singular)
  "Return the feature registry entry whose singular name is SINGULAR, or nil."
  (cl-find singular org-canvas--feature-registry
           :key (lambda (f) (org-canvas--singularize
                             (org-canvas--delete-heading-key f)))
           :test #'equal))

(defun org-canvas--delete-heading-key (feature)
  "Return FEATURE's registry name as the property registry keys it."
  (downcase (replace-regexp-in-string " " "-" (plist-get feature :name))))

(defun org-canvas--delete-heading-fn-for (feature)
  "Return the delete by heading of FEATURE, a function of TARGET and BY.
FEATURE is the module's name, plural or singular, as a symbol or
string; one with no delete at point is a `user-error' listing those
that have one.  The heading is looked for in the feature's file under
its property registration's query, as the drift report finds it."
  (let* ((name (if (symbolp feature) (symbol-name feature) feature))
         (deleter (org-canvas--heading-fn-lookup
                   name org-canvas--delete-heading-fns "delete"))
         (singular (org-canvas--singularize name))
         (entry (or (org-canvas--delete-heading-feature singular) (error "No feature %s" singular)))
         (props (gethash (org-canvas--delete-heading-key entry)
                         org-canvas--property-registry))
         (query (or (plist-get props :query) "LEVEL=1")))
    (lambda (target by)
      (org-canvas--delete-heading-runtime deleter entry singular query target by))))

(defun org-canvas--delete-heading-runtime (deleter feature singular query
                                                   target by)
  "Run DELETER at the heading of FEATURE that TARGET names; return a context.
FEATURE is the feature registry entry, SINGULAR its name and QUERY
the heading match; TARGET and BY are as in
`org-canvas--sync-find-heading'.  The heading's file is saved.  The
context's :outcome is DELETER's answer, `failed' for nil, and its
:heading-report names the id property and the id deleted."
  (let* ((id-property (or (plist-get feature :id-property) "CANVAS_ID"))
         (marker (org-canvas--sync-find-heading
                  :file (expand-file-name
                         (symbol-value (plist-get feature :file-var)))
                  :query query :target target :by by
                  :id-property id-property :feature singular :verb "delete")))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((id (org-entry-get (point) id-property))
               (outcome (funcall deleter)))
          (org-canvas--save-buffer)
          (org-canvas--sync-make-ctx
           :outcome (or outcome 'failed)
           :heading-report (list :id-property id-property :id id)))))))

(defun org-canvas--delete-headings-report (results)
  "Say how RESULTS, the list `org-canvas-delete-headings' built, came out.
One redacted line per heading, then one line of counts."
  (dolist (r results)
    (org-canvas--user-message "%s '%s': %s%s"
      (plist-get r :feature) (plist-get r :target) (plist-get r :outcome)
      (if (plist-get r :error) (format " — %s" (plist-get r :error)) "")))
  (let ((count (lambda (o) (cl-count o results
                                     :key (lambda (r) (plist-get r :outcome))))))
    (message "Deleted %d heading(s): %d deleted, %d dry run, %d declined, %d failed"
             (length results) (funcall count 'deleted) (funcall count 'dry-run)
             (funcall count 'declined) (funcall count 'failed))))

;;;###autoload
(defun org-canvas-delete-headings (entries)
  "Delete ENTRIES, named headings of several features, from Canvas.
ENTRIES are written as `org-canvas-sync-headings' takes them:
\(FEATURE . TITLE), or (FEATURE TARGET BY) where BY is nil, `title' or
`canvas-id'.  Each heading's `org-canvas-delete-FEATURE-at-point' runs
there and its file is saved; the heading stays, its sync properties
cleared.  Every feature is resolved before the first delete; after
that a failed heading is reported and the next one runs.

Each delete asks first unless `org-canvas-delete-confirmed' is bound
non-nil, which answers that question and no other.  In batch, where
there is nobody to ask, an unconfirmed run is refused before anything
is read.  A dry run (`org-canvas--dry-run') asks nothing and sends
nothing.  Return one plist per entry, (:feature :target :outcome),
:outcome `deleted', `dry-run', `declined' or `failed' with :error,
and :id-property and :id naming what went (issue #525)."
  (when (and noninteractive (not org-canvas-delete-confirmed)
             (not org-canvas--dry-run))
    (user-error "Nothing deleted: confirm with org-canvas-delete-confirmed bound to t (scripts/org-canvas delete --yes)"))
  (org-canvas-clear-log)
  (let* ((org-canvas--inhibit-log-clear t)
         (results (org-canvas--headings-run
                   entries #'org-canvas--delete-heading-fn-for "Delete headings")))
    (org-canvas--delete-headings-report results)
    results))

(provide 'org-canvas-core-heading)
;;; org-canvas-core-heading.el ends here
