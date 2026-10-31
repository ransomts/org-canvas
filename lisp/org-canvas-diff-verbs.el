;;; org-canvas-diff-verbs.el --- Drift report's verbs  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The verbs the drift report's rows offer, each a separate command the
;; report never runs itself (Hard Rule 19): delete a row's Canvas object
;; after safety checks and a JSON snapshot of it (#103, #345), pull a
;; row's item over its heading (#103), adopt a CHANGED row's stamp
;; (#257), and stamp a MOVED row's item id on the heading it paired
;; with (#342).
;;
;; Deleting is the only verb that sends anything.  Adopting a stamp and
;; stamping a moved item write the Org file and nothing else.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-diff)
(require 'org-canvas-diff-report)
(require 'cl-lib)

;; The pull verbs the report's rows reach for are declared, never
;; required: this is a command file above the feature modules, and
;; `org-canvas' loads them.  A delete names the object that owns an
;; assignment the way prune and the orphan scan find it owned (#366).
(declare-function org-canvas-pull-at-point "org-canvas")
(declare-function org-canvas--pull-new-heading "org-canvas" (feature id title))
(declare-function org-canvas--new-quiz-api-endpoint "org-canvas-new-quiz-items"
                  (suffix &rest args))
(declare-function org-canvas--assignment-owner "org-canvas-assignments"
                  (item))

;;;; Deleting Remote Objects (issues #103, #345)
;;
;; `k' deleted a row's Canvas object behind one `y-or-n-p', and nothing
;; else could: a batch caller cleaning up after a calendar reshuffle
;; (nine EXTRA rows, issue #345) rebuilt the delete target by hand and
;; wrote its own safety checks and snapshots.  Both paths now share
;; them.  Before any DELETE the object is read — at the URL the delete
;; goes to, with `include[]=associations' for a rubric — and two kinds
;; of object are held back, naming why: an assignment with a submission
;; or a score (its gradebook column goes with it), and a rubric an
;; assignment grades with (its assessments go with it).  `k' shows the
;; reason in its question; `org-canvas-diff-delete-rows' refuses unless
;; asked with :force.  The object's JSON is then written to a snapshot
;; file, so a mistaken delete can be rebuilt, and only then is the
;; DELETE sent.  The report itself still never writes (Hard Rule 19):
;; these are separate, explicit verbs.

(defcustom org-canvas-diff-delete-snapshot-directory nil
  "Directory for the JSON of each object a drift-report delete removes.
Nil means canvas-snapshots/ under `org-canvas-directory', resolved
when used rather than when the package loads.  Each file is named
<time>-<feature>-<id>.json and holds the object as Canvas returned it
just before the DELETE (issue #345)."
  :type '(choice (const :tag "canvas-snapshots/ under org-canvas-directory" nil)
                 directory)
  :group 'org-canvas)

(defun org-canvas--diff-snapshot-dir ()
  "Return the snapshot directory as an absolute path."
  (expand-file-name (or org-canvas-diff-delete-snapshot-directory
                        (org-canvas--path "canvas-snapshots/"))))

(defun org-canvas--diff-snapshot-write (feature-name id object)
  "Write OBJECT, FEATURE-NAME's item ID, to a snapshot file; return its path."
  (let* ((dir (org-canvas--diff-snapshot-dir))
         (file (expand-file-name
                (format "%s-%s-%s.json"
                        (format-time-string "%Y%m%dT%H%M%S")
                        (replace-regexp-in-string
                         "[^[:alnum:]]+" "-" (downcase feature-name))
                        (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" id))
                dir)))
    (make-directory dir t)
    (let ((coding-system-for-write 'utf-8)
          (json-encoding-pretty-print t))
      (write-region (concat (json-encode object) "\n") nil file nil 'silent))
    file))

(defun org-canvas--diff-delete-target (row entry)
  "Return (URL . DELETE-DATA) for the remote object ENTRY of ROW names.
A module item lives under its module, not at a feature URL (issue
#177), and a New Quiz item under its quiz at the quiz service (issue
#322); anything else uses the feature's item URL and delete body, as
orphan cleanup does."
  (let ((id (plist-get entry :id)))
    (cond
     ((plist-get entry :module-id)
      (cons (org-canvas-api-course-endpoint "modules/%s/items/%s"
                                            (plist-get entry :module-id) id)
            nil))
     ((plist-get entry :quiz-id)
      (cons (org-canvas--new-quiz-api-endpoint "quizzes/%s/items/%s"
                                               (plist-get entry :quiz-id) id)
            nil))
     (t
      (let ((feature (org-canvas--diff-row-feature row)))
        (cons (org-canvas--feature-item-url feature id)
              (plist-get feature :delete-data)))))))

(defun org-canvas--diff-delete-read-params (row entry)
  "Return the query parameters that read ENTRY of ROW before its delete.
A rubric is read with its associations, which its guard counts; any
other feature with the parameters it reads one item with.  A module
item or a New Quiz item (issue #322) is read with none: neither is a
feature's own."
  (unless (or (plist-get entry :module-id) (plist-get entry :quiz-id))
    (if (string= (org-canvas--diff-normalize-name (plist-get row :feature))
                 "rubrics")
        '(("include[]" . "associations"))
      (org-canvas--feature-item-params (org-canvas--diff-row-feature row)))))

(defun org-canvas--diff-submitted-p (submission)
  "Return non-nil when a student turned in work on SUBMISSION."
  (or (alist-get 'submitted_at submission)
      (member (alist-get 'workflow_state submission)
              '("submitted" "pending_review"))))

(defun org-canvas--diff-scored-p (submission)
  "Return non-nil when SUBMISSION carries a score or a grade."
  (or (alist-get 'score submission) (alist-get 'grade submission)))

(defun org-canvas--diff-delete-assignment-guard (entry object)
  "Return why assignment ENTRY must not be deleted, or nil.
OBJECT is the assignment as Canvas holds it.  Lists the assignment's
submissions — one request, paged — and counts those a student turned
in and those carrying a score or grade; Canvas's own
`has_submitted_submissions' flag counts too."
  (let* ((subs (cl-remove-if-not
                #'consp
                (append (org-canvas-api-request-all-pages
                         'GET (org-canvas-api-course-endpoint
                               "assignments/%s/submissions"
                               (plist-get entry :id)))
                        nil)))
         (submitted (cl-count-if #'org-canvas--diff-submitted-p subs))
         (scored (cl-count-if #'org-canvas--diff-scored-p subs)))
    (cond ((or (> submitted 0) (> scored 0))
           (format "the assignment has %d submission(s) and %d score(s)"
                   submitted scored))
          ((eq (alist-get 'has_submitted_submissions object) t)
           "Canvas says the assignment has submissions"))))

(defun org-canvas--diff-assignment-owner-label (object)
  "Return a name for the object that owns the assignment OBJECT, or nil.
A graded discussion by its title, a classic quiz by its id; a New
Quiz's assignment is the quiz, so it has no separate name."
  (let ((topic (alist-get 'discussion_topic object))
        (quiz-id (alist-get 'quiz_id object)))
    (cond ((and (consp topic) (alist-get 'title topic))
           (format "discussion '%s'" (alist-get 'title topic)))
          ((and quiz-id (not (eq quiz-id :null)))
           (format "quiz %s" quiz-id)))))

(defun org-canvas--diff-delete-owner-guard (object)
  "Return why the assignment OBJECT belongs to another object, or nil.
Asks the assignments feature's `:delete-skip-fn', the predicate prune
and the orphan scan leave an owned assignment by (issue #319): deleting
a quiz's or a graded discussion's assignment deletes or breaks its
owner (issue #366)."
  (let* ((skip (org-canvas--feature-delete-skip
                (org-canvas--registry-find-feature "Assignments")))
         (owned (and (car skip) (funcall (car skip) object))))
    (when owned
      (let ((label (org-canvas--diff-assignment-owner-label object))
            (owner (or (org-canvas--assignment-owner object) (cdr skip))))
        (if label
            (format "the assignment belongs to %s (%s)" label owner)
          (format "the assignment belongs to %s" owner))))))

(defun org-canvas--diff-delete-assignments-guard (entry object)
  "Return why the Assignments row ENTRY must not be deleted, or nil.
OBJECT is the assignment as Canvas holds it.  An assignment another
object owns is named first (issue #366), without reading its
submissions; otherwise `org-canvas--diff-delete-assignment-guard'
counts them."
  (or (org-canvas--diff-delete-owner-guard object)
      (org-canvas--diff-delete-assignment-guard entry object)))

(defun org-canvas--diff-delete-rubric-guard (_entry object)
  "Return why the rubric OBJECT must not be deleted, or nil.
OBJECT was read with its associations; an Assignment association means
an assignment grades with the rubric, and its assessments would go."
  (let ((ids (delq nil
                   (mapcar (lambda (a)
                             (and (consp a)
                                  (equal (alist-get 'association_type a)
                                         "Assignment")
                                  (format "%s" (alist-get 'association_id a))))
                           (append (alist-get 'associations object) nil)))))
    (when ids
      (format "the rubric grades assignment(s) %s"
              (mapconcat #'identity ids ", ")))))

(defconst org-canvas--diff-delete-guards
  '(("assignments" . org-canvas--diff-delete-assignments-guard)
    ("newquizzes" . org-canvas--diff-delete-assignment-guard)
    ("rubrics" . org-canvas--diff-delete-rubric-guard))
  "Safety checks before a delete, by normalized feature name.
Each a function of (ENTRY OBJECT) returning why the object must stay,
or nil (issue #345).  An Assignments row is also held back when a quiz
or a graded discussion owns the assignment (issue #366).  A New Quiz
row's id is its assignment's (issue #313), so the assignment's check
counts the quiz's submissions; its object is the quiz itself, which
the ownership check does not apply to.")

(defun org-canvas--diff-delete-inspect (row entry)
  "Read the object ENTRY of ROW names; return (OBJECT . REASON).
REASON says why the object should not be deleted, or is nil: the
feature's guard (`org-canvas--diff-delete-guards'), or an empty reply,
which leaves nothing to snapshot.  A module item has no guard."
  (let* ((params (org-canvas--diff-delete-read-params row entry))
         (object (apply #'org-canvas-api-request 'GET
                        (car (org-canvas--diff-delete-target row entry))
                        (and params (list :params params))))
         (guard (and (not (plist-get entry :module-id))
                     (alist-get (org-canvas--diff-normalize-name
                                 (plist-get row :feature))
                                org-canvas--diff-delete-guards
                                nil nil #'string=))))
    (cons object
          (if (null object)
              "Canvas returned nothing to snapshot"
            (and guard (funcall guard entry object))))))

(defun org-canvas--diff-delete-scope-file (row entry)
  "Return the Org file whose allowlist entry governs deleting ROW's ENTRY.
A module item is claimed by a heading in modules.org, though its drift
feature \"Module Items\" is no registry entry; anything else is its
feature's own file, or the feature's name when no file resolves, so a
refusal still names what to grant (issue #550)."
  (if (plist-get entry :module-id)
      ;; The Modules entry is registered by the module's own load; without
      ;; it, the nil file is refused fail-closed by the check.
      (org-canvas--feature-file (org-canvas--registry-find-feature "Modules"))
    (or (org-canvas--feature-file
         (org-canvas--diff-find-feature (plist-get row :feature)))
        (plist-get row :feature))))

(defun org-canvas--diff-delete-execute (row entry object)
  "Snapshot OBJECT, then delete the object ENTRY of ROW names.
Returns the snapshot's path.  A course marked read-only is refused
before the snapshot is written, as the transport would refuse the
DELETE (issue #163); so is an identity whose allowlist does not name
the row's feature file (issue #550)."
  (org-canvas--check-writable 'DELETE "a drift-report delete")
  (org-canvas--check-write-scope
   (org-canvas--diff-delete-scope-file row entry)
   "a drift-report delete")
  (let* ((target (org-canvas--diff-delete-target row entry))
         (delete-data (cdr target))
         (file (org-canvas--diff-snapshot-write
                (plist-get row :feature) (plist-get entry :id) object)))
    (apply #'org-canvas-api-request 'DELETE (car target)
           (and delete-data (list :data delete-data)))
    (org-canvas--log-info org-canvas--logger
      "[Diff] Deleted %s #%s '%s'; its JSON is in %s"
      (plist-get row :feature) (plist-get entry :id)
      (plist-get entry :title) file)
    file))

(defun org-canvas--diff-delete-question (name entry reason)
  "Return the question `k' asks before deleting ENTRY of feature NAME.
REASON, when non-nil, is what the safety check found."
  (format "Delete %s '%s' (id %s) from Canvas%s? "
          name (plist-get entry :title) (plist-get entry :id)
          (if reason (format ", although %s" reason) "")))

(defun org-canvas-diff-delete ()
  "Delete the EXTRA, UNCLAIMED or MOVED row's Canvas object, after confirming.
Uses the feature's item URL and delete body, as orphan cleanup does;
a module item's row deletes the item from its module (issue #177) —
for a MOVED row, the copy left in the old module (issue #294).  The
object is read first: the question names an assignment's submissions
and scores or a rubric's assignments, and the object's JSON is saved
under `org-canvas-diff-delete-snapshot-directory' before the DELETE
\(issue #345)."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (id (plist-get entry :id))
         (name (plist-get row :feature)))
    (unless (memq (plist-get entry :kind) '(extra unclaimed moved))
      (user-error "Only an EXTRA, UNCLAIMED or MOVED row names a remote object to delete"))
    (let ((inspect (org-canvas--diff-delete-inspect row entry)))
      (when (y-or-n-p (org-canvas--diff-delete-question
                       name entry (cdr inspect)))
        (let ((file (org-canvas--diff-delete-execute row entry (car inspect))))
          (org-canvas--diff-rewrite-row
           (format "  DELETED   %s (id %s)" (plist-get entry :title) id))
          (message "Deleted %s %s; its JSON is in %s." name id file))))))

;;;;; Deleting Rows Without Asking (issue #345)

(defun org-canvas--diff-result-entries (result)
  "Return every entry RESULT reports: rows, notes, pending and relocations.
A RELOCATE row (issue #343) is among them so a selector naming one is
told it is not an EXTRA row, rather than that the report has no row."
  (append (plist-get result :divergences) (plist-get result :extra)
          (plist-get result :notes) (plist-get result :pending)
          (plist-get result :relocate)))

(defun org-canvas--diff-selector-outcome (feature id outcome reason)
  "Return the outcome plist of a selector that named no deletable row.
FEATURE and ID are what the selector named, OUTCOME the symbol and
REASON the sentence."
  (list :feature feature :id id :outcome outcome :reason reason))

(defun org-canvas--diff-select-by-id (result feature id)
  "Return the row of RESULT whose entry carries ID, or an outcome plist.
FEATURE is the selector's feature name.  Only an EXTRA row is
returned as a row, (:feature NAME :entry ENTRY); any other kind comes
back as a `not-extra' outcome, no row at all as `not-found'."
  (let ((entry (cl-find-if (lambda (e) (equal (plist-get e :id) id))
                           (org-canvas--diff-result-entries result))))
    (cond ((null entry)
           (org-canvas--diff-selector-outcome
            feature id 'not-found "the report has no row with this id"))
          ((eq (plist-get entry :kind) 'extra)
           (list :feature (plist-get result :name) :entry entry))
          (t
           (org-canvas--diff-selector-outcome
            feature id 'not-extra
            (format "a %s row, and only an EXTRA row is deleted"
                    (upcase (symbol-name (plist-get entry :kind)))))))))

(defun org-canvas--diff-select (results selector)
  "Return what SELECTOR names in RESULTS, a list of rows and outcomes.
SELECTOR is (:feature NAME :id ID), one row, or (:feature NAME :all t),
every EXTRA row of the feature.  A row is (:feature NAME :entry ENTRY);
a selector that names no deletable row yields an outcome plist."
  (let* ((feature (plist-get selector :feature))
         (id (and (plist-get selector :id)
                  (format "%s" (plist-get selector :id))))
         (result (and feature
                      (cl-find-if (lambda (r)
                                    (string= (org-canvas--diff-normalize-name
                                              (plist-get r :name))
                                             (org-canvas--diff-normalize-name
                                              feature)))
                                  results))))
    (cond ((null result)
           (list (org-canvas--diff-selector-outcome
                  feature id 'not-found "the report has no such feature")))
          ((plist-get selector :all)
           (mapcar (lambda (e)
                     (list :feature (plist-get result :name) :entry e))
                   (cl-remove-if-not
                    (lambda (e) (eq (plist-get e :kind) 'extra))
                    (plist-get result :extra))))
          (t (list (org-canvas--diff-select-by-id result feature id))))))

(defun org-canvas--diff-delete-row (row force)
  "Delete ROW's Canvas object without asking; return its outcome plist.
The outcome is `deleted' (with :snapshot, the JSON's path), `refused'
when a safety check held it back and FORCE is nil, `dry-run' under
`org-canvas--dry-run', which sends and writes nothing, or `failed'
when a request did.  :reason names what the check found, forced or
not."
  (let* ((entry (plist-get row :entry))
         (out (list :feature (plist-get row :feature) :id (plist-get entry :id)
                    :title (plist-get entry :title))))
    (condition-case err
        (let* ((inspect (org-canvas--diff-delete-inspect row entry))
               (reason (cdr inspect)))
          (cond
           ((and reason (not force))
            (append out (list :outcome 'refused :reason reason)))
           (org-canvas--dry-run
            (org-canvas--log-info org-canvas--logger
              "[DRY-RUN] Would delete %s #%s '%s'" (plist-get row :feature)
              (plist-get entry :id) (plist-get entry :title))
            (append out (list :outcome 'dry-run :reason reason)))
           (t
            (append out (list :outcome 'deleted :reason reason
                              :snapshot (org-canvas--diff-delete-execute
                                         row entry (car inspect)))))))
      (error
       (org-canvas--log-error org-canvas--logger
         "[Diff] Could not delete %s #%s: %s" (plist-get row :feature)
         (plist-get entry :id) (error-message-string err))
       (append out (list :outcome 'failed
                         :reason (error-message-string err)))))))

(defun org-canvas--diff-delete-log-outcome (outcome)
  "Log one OUTCOME plist of `org-canvas-diff-delete-rows'."
  (org-canvas--log-info org-canvas--logger
    "[Diff] %s %s #%s%s%s"
    (upcase (symbol-name (plist-get outcome :outcome)))
    (plist-get outcome :feature) (plist-get outcome :id)
    (if (plist-get outcome :title)
        (format " '%s'" (plist-get outcome :title))
      "")
    (if (plist-get outcome :reason)
        (format ": %s" (plist-get outcome :reason))
      "")))

(defun org-canvas--diff-outcome-count (outcomes &rest kinds)
  "Return how many of OUTCOMES have an :outcome among KINDS."
  (cl-count-if (lambda (o) (memq (plist-get o :outcome) kinds)) outcomes))

;;;###autoload
(cl-defun org-canvas-diff-delete-rows (selectors &key force results)
  "Delete the Canvas objects of the drift report's EXTRA rows SELECTORS names.
Never asks.  Each selector is (:feature NAME :id ID), one row, or
\(:feature NAME :all t), every EXTRA row of the feature; NAME matches
the way the report's sections do (\"Module Items\", \"assignments\").
The rows come from the report's own comparison — RESULTS when given,
as `org-canvas--diff-collect-results' returns them, else read afresh —
and each object is deleted at the URL `k' would use.

Only an EXTRA row is deleted: an UNCLAIMED or MOVED row has a heading
the next sync would pair with it.  Before each DELETE the object is
read, and an assignment with a submission or score, or a rubric an
assignment grades with, is refused, naming why, unless FORCE is
non-nil; the object's JSON is then written under
`org-canvas-diff-delete-snapshot-directory'.  A dry run
\(`org-canvas--dry-run') reads and checks but writes and sends
nothing; a course marked read-only refuses every delete.

Returns one plist per row or unmatched selector: :feature, :id,
:title, :outcome — `deleted', `refused', `dry-run', `failed',
`not-extra' or `not-found' — :reason and, once deleted, :snapshot
\(issue #345)."
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let ((results (or results (org-canvas--diff-collect-results)))
        (seen nil)
        (outcomes nil))
    (dolist (selector selectors)
      (dolist (row (org-canvas--diff-select results selector))
        (let ((key (cons (plist-get row :feature)
                         (plist-get (plist-get row :entry) :id))))
          (cond ((not (plist-get row :entry)) (push row outcomes))
                ((member key seen))
                (t (push key seen)
                   (push (org-canvas--diff-delete-row row force) outcomes))))))
    (setq outcomes (nreverse outcomes))
    (mapc #'org-canvas--diff-delete-log-outcome outcomes)
    (message "Drift delete: %d deleted, %d refused, %d left for another reason"
             (org-canvas--diff-outcome-count outcomes 'deleted)
             (org-canvas--diff-outcome-count outcomes 'refused)
             (org-canvas--diff-outcome-count
              outcomes 'dry-run 'failed 'not-extra 'not-found))
    outcomes))

(defun org-canvas--diff-pull-extra (feature entry)
  "Pull the item of EXTRA row ENTRY into a new heading of FEATURE's file.
Only a feature that pulls whole entries (`:pull-whole-entry', classic
quizzes) can write one from nothing; a module item belongs under its
module heading, which a module pull writes.  Returns non-nil when the
item was pulled."
  (unless (and (plist-get feature :pull-whole-entry)
               (not (plist-get entry :module-id)))
    (user-error "%s cannot pull '%s' into a new heading; use M-x org-canvas-pull-%s"
                (plist-get feature :name) (plist-get entry :title)
                (downcase (replace-regexp-in-string
                           " " "-" (plist-get feature :name)))))
  (save-excursion
    (org-canvas--pull-new-heading feature (plist-get entry :id)
                                  (plist-get entry :title))))

(defun org-canvas--diff-pull-quiz-of (entry)
  "Pull the New Quiz of item row ENTRY over its quiz heading.
An item is part of its quiz, and the quiz pulls whole, its items
included (issue #322), so the quiz heading of CANVAS_ASSIGNMENT_ID
:quiz-id is what `org-canvas-pull-at-point' runs on.  Asks first."
  (let ((feature (org-canvas--diff-find-feature "New Quizzes")))
    (save-excursion
      (with-current-buffer (org-canvas--diff-goto-heading
                            feature (list :kind 'modified
                                          :id (plist-get entry :quiz-id)
                                          :title (plist-get entry :where)))
        (org-canvas-pull-at-point)
        t))))

(defun org-canvas--diff-pull-heading-of (entry)
  "Pull the assignment heading of override row ENTRY over itself.
A table is part of its heading, and the heading's pull writes the
table Canvas holds whole (issue #411), so the heading the row names
by file and line is what `org-canvas-pull-at-point' runs on.  Asks
first."
  (save-excursion
    (with-current-buffer (org-canvas--diff-goto-heading nil entry)
      (org-canvas-pull-at-point)
      t)))

(defun org-canvas--diff-pulled-line (entry)
  "Return the text to put in place of ENTRY's row once it is pulled."
  (if (eq (plist-get entry :kind) 'override)
      (format "  PULLED    %s (with the table of %s)" (plist-get entry :title)
              (org-canvas--diff-container entry))
    (format "  PULLED    %s (id %s%s)" (plist-get entry :title) (plist-get entry :id)
            (cond ((plist-get entry :quiz-id)
                   (format ", with %s" (org-canvas--diff-container entry)))
                  ((eq (plist-get entry :kind) 'extra) ", new heading")
                  (t "")))))

(defun org-canvas-diff-pull ()
  "Pull the item of the row at point from Canvas into Org.
On a CHANGED row, runs `org-canvas-pull-at-point' on the heading.  On
an EXTRA row of a feature that pulls whole entries (classic and New
Quizzes), writes the item as a new heading at the end of its file
\(issues #295, #313).  On a New Quiz item's CHANGED or EXTRA row,
pulls the quiz it sits in (issue #322); on an override row, the
assignment heading whose table it is (issue #411).  Each asks first."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (kind (plist-get entry :kind))
         (pulled
          (cond
           ((eq kind 'override) (org-canvas--diff-pull-heading-of entry))
           ((not (memq kind '(modified extra)))
            (user-error "Only a CHANGED or EXTRA row has a Canvas version to pull"))
           ((plist-get entry :quiz-id) (org-canvas--diff-pull-quiz-of entry))
           ((eq kind 'modified)
            (save-excursion
              (with-current-buffer (org-canvas--diff-goto-heading
                                    (org-canvas--diff-row-feature row) entry)
                (org-canvas-pull-at-point)
                t)))
           (t (org-canvas--diff-pull-extra (org-canvas--diff-row-feature row)
                                           entry)))))
    (when pulled
      (org-canvas--diff-rewrite-row (org-canvas--diff-pulled-line entry)))))

;;;; Adopting a Stamp
;;
;; A change Canvas makes on its own moves `updated_at' on entries whose
;; content did not change: a metadata-only file touch (before #94), the
;; rubric-association bump (before #131), and a course-wide post policy
;; change, which rewrote the policy of all 77 assignments in one course
;; (issue #257).  The report is right to list each as CHANGED with no
;; compared property differing, and there is nothing in Org to change;
;; a push would send the same content again.  The only fix is to copy
;; the live timestamp into CANVAS_UPDATED_AT, which was a one-off script
;; three times over.  Adoption is that script: the stamp moves,
;; PAYLOAD_HASH stays (the local side did not change, so the skip must
;; survive), and nothing is sent.  It is a separate key and command,
;; never something the report does by itself (issue #83).

(defun org-canvas--diff-adoptable-p (entry)
  "Return non-nil when ENTRY is a CHANGED row whose stamp can be adopted.
That is one Canvas holds newer than the baseline with no compared
property differing: the change is in a field the report does not
compare, so the heading has nothing to push or pull."
  (and (eq (plist-get entry :kind) 'modified)
       (plist-get entry :remote-newer)
       (null (plist-get entry :fields))
       (stringp (plist-get entry :updated))))

(defun org-canvas--diff-adopt-entry (feature entry)
  "Stamp ENTRY's heading with the timestamp Canvas reports, and save the file.
FEATURE is the registry entry that says which file to look in.  Writes
CANVAS_UPDATED_AT only: PAYLOAD_HASH is left as it is, since the local
content did not change.  Returns the timestamp written, or nil when
the heading could not be found."
  (let ((where (org-canvas--diff-heading-position feature entry))
        (updated (plist-get entry :updated)))
    (when where
      (with-current-buffer (org-canvas--find-file-noselect (car where))
        (save-excursion
          (goto-char (cdr where))
          (org-back-to-heading t)
          (org-canvas-org-set-property (point) "CANVAS_UPDATED_AT" updated)
          (org-canvas--save-buffer)))
      (org-canvas--log-info org-canvas--logger
        "[Diff] Adopted the stamp of %s '%s': CANVAS_UPDATED_AT is now %s, PAYLOAD_HASH kept, nothing sent"
        (plist-get feature :name) (plist-get entry :title) updated)
      updated)))

(defun org-canvas-diff-adopt-stamp ()
  "Adopt the Canvas timestamp of the CHANGED row at point as its baseline.
Only a row with no compared property differing qualifies: Canvas is
newer, but the change is in a field the report does not compare, so
there is nothing to push or pull.  Writes CANVAS_UPDATED_AT on the
heading, keeps PAYLOAD_HASH and sends nothing (issue #257).  A row
where a property differs is refused; push or pull that one instead.
`org-canvas-diff-adopt-stamps' does the same for every such row."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (title (plist-get entry :title)))
    (unless (eq (plist-get entry :kind) 'modified)
      (user-error "Only a CHANGED row has a Canvas timestamp to adopt"))
    ;; Refused before the feature is looked up: a New Quiz item's row
    ;; is never adoptable and names no feature of its own (#322).
    (unless (org-canvas--diff-adoptable-p entry)
      (user-error "A compared property of '%s' differs; push or pull it rather than adopting its stamp"
                  title))
    (let* ((feature (org-canvas--diff-row-feature row))
           (updated (org-canvas--diff-adopt-entry feature entry)))
      (unless updated
        (user-error "Cannot find the heading for '%s' in %s"
                    title (plist-get feature :name)))
      (org-canvas--diff-rewrite-row
       (format "  ADOPTED   %s (CANVAS_UPDATED_AT set to %s, nothing sent)"
               title updated))
      (message "Adopted the stamp of %s '%s'." (plist-get feature :name) title))))

(defconst org-canvas--diff-adopt-buffer-name "*canvas-diff-adopt*"
  "Name of the buffer holding the stamp adoption report.")

(defun org-canvas--diff-adopt-candidates (results)
  "Sort the CHANGED rows of RESULTS into the adoptable and the held back.
Returns (ADOPTABLE . HELD): ADOPTABLE lists (FEATURE-NAME . ENTRY) for
every row with no compared property differing, HELD counts the CHANGED
rows a differing property keeps out."
  (let ((adoptable nil) (held 0))
    (dolist (result results)
      (dolist (entry (plist-get result :divergences))
        (cond ((org-canvas--diff-adoptable-p entry)
               (push (cons (plist-get result :name) entry) adoptable))
              ((eq (plist-get entry :kind) 'modified)
               (setq held (1+ held))))))
    (cons (nreverse adoptable) held)))

(defun org-canvas--diff-adopt-all (candidates)
  "Adopt the stamp of every (FEATURE-NAME . ENTRY) in CANDIDATES.
Returns (ADOPTED . MISSED): ADOPTED lists (FEATURE-NAME TITLE
TIMESTAMP) in order, MISSED lists (FEATURE-NAME . TITLE) for the
headings that could not be found."
  (let ((adopted nil) (missed nil))
    (dolist (candidate candidates)
      (let* ((name (car candidate))
             (entry (cdr candidate))
             (feature (org-canvas--diff-find-feature name))
             (updated (and feature (org-canvas--diff-adopt-entry feature entry))))
        (if updated
            (push (list name (plist-get entry :title) updated) adopted)
          (push (cons name (plist-get entry :title)) missed))))
    (cons (nreverse adopted) (nreverse missed))))

(defun org-canvas--diff-adopt-render (adopted missed held)
  "Insert the stamp adoption report at point.
ADOPTED and MISSED are the halves `org-canvas--diff-adopt-all'
returns, HELD the count of CHANGED rows left alone because a compared
property differs."
  (insert "org-canvas Stamp Adoption\n")
  (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
  (insert (make-string 60 ?=) "\n\n")
  (dolist (a adopted)
    (insert (format "  ADOPTED   %s (%s; CANVAS_UPDATED_AT set to %s)\n"
                    (nth 1 a) (nth 0 a) (nth 2 a))))
  (dolist (m missed)
    (insert (format "  NOT FOUND %s (%s; no heading carries its id)\n" (cdr m) (car m))))
  (when (or adopted missed) (insert "\n"))
  (insert (make-string 60 ?=) "\n")
  (insert (if adopted
              (format "%d stamp(s) adopted; PAYLOAD_HASH kept, nothing sent to Canvas.\n"
                      (length adopted))
            "No stamp to adopt: no CHANGED row has every compared property matching.\n"))
  (when (> held 0)
    (insert (format "%d CHANGED row(s) left alone: a compared property differs — push or pull those.\n"
                    held))))

(defun org-canvas--diff-adopt-confirm (count)
  "Ask whether to adopt COUNT stamps, or signal a `user-error'.
Under `noninteractive' there is nobody to ask, and running the command
was the answer."
  (unless (or noninteractive
              (y-or-n-p (format "Adopt the Canvas timestamp of %d CHANGED entr%s with every compared property matching? "
                                count (if (= count 1) "y" "ies"))))
    (user-error "Aborted; no stamp adopted")))

;;;###autoload
(defun org-canvas-diff-adopt-stamps ()
  "Adopt the Canvas timestamp of every CHANGED entry with nothing to compare.
Runs the drift comparison (`org-canvas-diff' reads, one list request
per feature), then, after confirming, copies the live timestamp into
CANVAS_UPDATED_AT on each heading Canvas holds newer whose compared
properties all match — the rows the report says have their change in a
field it does not compare.  PAYLOAD_HASH is kept and nothing is sent.
A row where a property differs is left alone and counted (issue #257).
The course-wide case is a post policy change, which rewrites every
assignment.  Returns the number of stamps adopted."
  (interactive)
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let* ((results (org-canvas--diff-collect-results))
         (candidates (org-canvas--diff-adopt-candidates results))
         (held (cdr candidates))
         (outcome (cons nil nil)))
    (when (car candidates)
      (org-canvas--diff-adopt-confirm (length (car candidates)))
      (setq outcome (org-canvas--diff-adopt-all (car candidates))))
    (org-canvas--report-display
     org-canvas--diff-adopt-buffer-name
     (lambda () (org-canvas--diff-adopt-render (car outcome) (cdr outcome) held)))
    (message "Drift: %d stamp(s) adopted, %d CHANGED row(s) left alone"
             (length (car outcome)) held)
    (length (car outcome))))

;;;; Stamping a Moved Item (issue #342)
;;
;; A MOVED row named the id to stamp and the heading to stamp it on,
;; and nothing did the stamping: after a course-wide reshuffle that
;; was 48 hand edits, done by a script parsing the report's text, and
;; the renamed pairs — the ones the report had paired by content, not
;; by title — were the ones a title search missed.  The report knows
;; the heading it paired, so `s' stamps the item's id there and `S'
;; (`org-canvas-diff-stamp-moves') every MOVED row's.  CANVAS_ID is the
;; only property written, and nothing is sent: the next sync is what
;; moves the item into its new module, keeping its id (#352), and
;; the report shows each stamped heading as a RELOCATE row until then
;; (issue #343).  Like stamp adoption, it is a verb the report never
;; runs itself (Hard Rule 19).

(defun org-canvas--diff-moved-heading-p (entry)
  "Return non-nil when the heading at point is MOVED ENTRY's partner.
That is an item heading with ENTRY's :partner-heading text and no
CANVAS_ID: a heading stamped since the report is no longer the one it
paired."
  (and (org-at-heading-p)
       (let ((local (org-canvas--diff-local-entry "CANVAS_ID")))
         (and (null (plist-get local :id))
              (equal (plist-get local :heading)
                     (plist-get entry :partner-heading))))))

(defun org-canvas--diff-moved-search (entry)
  "Return the position of MOVED ENTRY's partner under its module, or nil.
The fallback when the heading is no longer on the line the report
read it from: an unstamped level-2 heading of the same text under the
module ENTRY's :to names."
  (catch 'found
    (org-map-entries
     (lambda ()
       (when (and (org-canvas--diff-moved-heading-p entry)
                  (equal (save-excursion
                           (and (org-up-heading-safe)
                                (org-get-heading t t t t)))
                         (plist-get entry :to)))
         (throw 'found (point))))
     "LEVEL=2" 'file)
    nil))

(defun org-canvas--diff-moved-position (entry)
  "Return (FILE . POSITION) of the heading MOVED ENTRY was paired with, or nil.
The row carries the file and line it was read from; when the heading
there is not the one paired, it is looked up under its module
\(`org-canvas--diff-moved-search')."
  (let ((file (plist-get entry :file)))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (or (plist-get entry :line) 1)))
          (let ((pos (if (org-canvas--diff-moved-heading-p entry)
                         (point)
                       (org-canvas--diff-moved-search entry))))
            (and pos (cons file pos))))))))

(defun org-canvas--diff-stamp-at (where entry)
  "Write MOVED ENTRY's item id as CANVAS_ID on the heading at WHERE.
WHERE is (FILE . POSITION).  Saves the file and sends nothing."
  (with-current-buffer (org-canvas--find-file-noselect (car where))
    (save-excursion
      (goto-char (cdr where))
      (org-back-to-heading t)
      (org-canvas-org-set-property (point) "CANVAS_ID" (plist-get entry :id))
      (org-canvas--save-buffer)))
  (org-canvas--log-info org-canvas--logger
    "[Diff] Stamped item id %s on '%s' in '%s'; nothing sent — the next sync moves it there from '%s', keeping its id"
    (plist-get entry :id) (plist-get entry :partner-title)
    (plist-get entry :to) (plist-get entry :where)))

(defun org-canvas--diff-stamp-entry (entry)
  "Stamp MOVED ENTRY's item id on the heading it was paired with, and save.
Writes CANVAS_ID only and sends nothing.  Returns `stamped'; `claimed'
when a heading of the file already carries the id, so a second stamp
would make two headings claim one item; or `not-found' when the paired
heading cannot be found unstamped."
  (if (org-canvas--diff-find-heading-by-id
       (plist-get entry :file) "CANVAS_ID" (plist-get entry :id))
      'claimed
    (let ((where (org-canvas--diff-moved-position entry)))
      (if (null where)
          'not-found
        (org-canvas--diff-stamp-at where entry)
        'stamped))))

(defun org-canvas--diff-stamp-refusal (entry outcome)
  "Signal a `user-error' naming why MOVED ENTRY's OUTCOME is no stamp."
  (if (eq outcome 'claimed)
      (user-error "Item id %s is already on a heading; run the report again (g)"
                  (plist-get entry :id))
    (user-error "Cannot find the unstamped heading '%s' under '%s'; run the report again (g)"
                (or (plist-get entry :partner-title) (plist-get entry :title))
                (plist-get entry :to))))

(defun org-canvas-diff-stamp-move ()
  "Stamp the item id of the MOVED row at point on the heading it paired.
The heading is the one the row names, found by line and text, however
its title differs from the item's (issue #342).  Writes CANVAS_ID and
sends nothing; the next sync then moves the item into the heading's
module, keeping its id (issue #352), which the report shows as a
RELOCATE row until it does (issue #343).  A row
paired by title only asks first, since the heading's link target has
no Canvas id to prove the pair.  `org-canvas-diff-stamp-moves' does
the same for every MOVED row."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry)))
    (unless (eq (plist-get entry :kind) 'moved)
      (user-error "Only a MOVED row names a heading to stamp"))
    (when (and (plist-get entry :by-title)
               (not (y-or-n-p
                     (format "'%s' was paired by title only; stamp item id %s on it anyway? "
                             (plist-get entry :title) (plist-get entry :id)))))
      (user-error "Aborted; nothing stamped"))
    (let ((outcome (org-canvas--diff-stamp-entry entry)))
      (unless (eq outcome 'stamped)
        (org-canvas--diff-stamp-refusal entry outcome))
      (org-canvas--diff-rewrite-row
       (format "  STAMPED   %s (CANVAS_ID %s on '%s' in '%s', nothing sent; the next sync moves it there from '%s', keeping its id)"
               (plist-get entry :title) (plist-get entry :id)
               (plist-get entry :partner-title) (plist-get entry :to)
               (plist-get entry :where)))
      (message "Stamped item id %s; the next sync moves it into '%s'."
               (plist-get entry :id) (plist-get entry :to)))))

(defconst org-canvas--diff-stamp-buffer-name "*canvas-diff-stamp*"
  "Name of the buffer holding the report of stamped MOVED rows.")

(defun org-canvas--diff-stamp-candidates (results)
  "Sort the MOVED rows of RESULTS into the stampable and the held back.
Returns (STAMPABLE . HELD): STAMPABLE lists the entries paired by
content, HELD those paired by title only, which only `s' on the row
stamps, after asking."
  (let ((stampable nil) (held nil))
    (dolist (result results)
      (dolist (entry (plist-get result :extra))
        (when (eq (plist-get entry :kind) 'moved)
          (if (plist-get entry :by-title)
              (push entry held)
            (push entry stampable)))))
    (cons (nreverse stampable) (nreverse held))))

(defun org-canvas--diff-stamp-outcome-line (entry outcome)
  "Return the stamp report's line for ENTRY, stamped with OUTCOME."
  (pcase outcome
    ('stamped (format "  STAMPED   %s (CANVAS_ID %s on '%s' in '%s')\n"
                      (plist-get entry :title) (plist-get entry :id)
                      (plist-get entry :partner-title) (plist-get entry :to)))
    ('claimed (format "  CLAIMED   %s (item id %s is already on a heading)\n"
                      (plist-get entry :title) (plist-get entry :id)))
    (_ (format "  NOT FOUND %s (no unstamped heading '%s' under '%s')\n"
               (plist-get entry :title) (plist-get entry :partner-title)
               (plist-get entry :to)))))

(defun org-canvas--diff-stamp-render (outcomes held)
  "Insert the report of stamped MOVED rows at point.
OUTCOMES lists (ENTRY . OUTCOME) from `org-canvas--diff-stamp-entry';
HELD the rows left alone because only their title paired them."
  (insert "org-canvas Moved Item Stamps\n")
  (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
  (insert (make-string 60 ?=) "\n\n")
  (dolist (o outcomes)
    (insert (org-canvas--diff-stamp-outcome-line (car o) (cdr o))))
  (dolist (e held)
    (insert (format "  HELD      %s (item id %s; %s — stamp it with s on its row)\n"
                    (plist-get e :title) (plist-get e :id)
                    org-canvas--diff-by-title-note)))
  (when (or outcomes held) (insert "\n"))
  (insert (make-string 60 ?=) "\n")
  (let ((n (cl-count 'stamped outcomes :key #'cdr)))
    (insert (if (> n 0)
                (format "%d item id(s) stamped, nothing sent to Canvas; the next sync moves each into its heading's module, keeping its id.\n" n)
              "No item id stamped.\n"))))

(defun org-canvas--diff-stamp-confirm (count)
  "Ask whether to stamp COUNT MOVED rows, or signal a `user-error'.
Under `noninteractive' there is nobody to ask, and running the command
was the answer."
  (unless (or noninteractive
              (let ((s (if (= count 1) "" "s")))
                (y-or-n-p
                 (format "Stamp the item id of %d MOVED row%s on the heading%s it was paired with? "
                         count s s))))
    (user-error "Aborted; nothing stamped")))

;;;###autoload
(defun org-canvas-diff-stamp-moves ()
  "Stamp the item id of every MOVED row on the heading it was paired with.
Runs the drift comparison (`org-canvas-diff' reads, one list request
per feature), then, after confirming, writes each MOVED item's id as
CANVAS_ID on the heading the report paired it with by content (issue
#342).  Nothing is sent: the next sync moves each item into its
heading's module, keeping its id (issues #343, #352).
A row paired by title only is held back and listed; `s' on its row
stamps it after asking.  Works from a batch Emacs, without asking.
Returns the number of ids stamped."
  (interactive)
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let* ((candidates (org-canvas--diff-stamp-candidates
                      (org-canvas--diff-collect-results)))
         (outcomes nil))
    (when (car candidates)
      (org-canvas--diff-stamp-confirm (length (car candidates)))
      (setq outcomes
            (mapcar (lambda (e) (cons e (org-canvas--diff-stamp-entry e)))
                    (car candidates))))
    (org-canvas--report-display
     org-canvas--diff-stamp-buffer-name
     (lambda () (org-canvas--diff-stamp-render outcomes (cdr candidates))))
    (let ((n (cl-count 'stamped outcomes :key #'cdr)))
      (message "Drift: %d item id(s) stamped, %d MOVED row(s) held back"
               n (length (cdr candidates)))
      n)))


(provide 'org-canvas-diff-verbs)
;;; org-canvas-diff-verbs.el ends here
