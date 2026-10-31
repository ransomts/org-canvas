;;; org-canvas-diff-report.el --- Drift report's rows, rendering and mode  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The drift report's rendering, and the mode its buffer opens in: one
;; row per divergence (a CHANGED row spanning as many lines as it has
;; differing fields), the footer that counts what the rows leave out,
;; the row accessors the verbs act through, and the commands that visit,
;; browse and acknowledge a row or move between them.
;;
;; The verbs themselves live in `org-canvas-diff-verbs.el', which
;; requires this file for those accessors; the mode's keymap only names
;; them.  Nothing here writes: the report describes, and the verbs act.
;;
;; The engine that produces the results rendered here is
;; `org-canvas-diff.el'.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-diff)
(require 'org-canvas-diff-fields)
(require 'cl-lib)

;; The verbs require this file for the row accessors below, so the
;; keymap can only name them.
(declare-function org-canvas-diff-delete "org-canvas-diff-verbs" ())
(declare-function org-canvas-diff-pull "org-canvas-diff-verbs" ())
(declare-function org-canvas-diff-stamp-move "org-canvas-diff-verbs" ())
(declare-function org-canvas-diff-stamp-moves "org-canvas-diff-verbs" ())
(declare-function org-canvas-diff-adopt-stamp "org-canvas-diff-verbs" ())

;;;; Report

(defconst org-canvas--diff-by-title-note
  "paired by title only, since the heading's link target has no Canvas id yet"
  "What a module item row adds when only the title paired it (issue #299).")

(defun org-canvas--diff-unclaimed-line (entry)
  "Return the report line of UNCLAIMED ENTRY.
A module item's twin is adopted by the sync itself (issue #179), so
its line says that instead of naming `org-canvas-adopt-at-point'.  The
pairing went by content, as the sync's adoption does, unless ENTRY
says :by-title (issue #299)."
  (cond
   ((plist-get entry :by-title)
    (format "  UNCLAIMED %s (item id %s in module '%s' has this title and no heading claims it; %s — the next sync adopts it only if that target turns out to be this item's content)\n"
            (plist-get entry :title) (plist-get entry :id) (plist-get entry :where)
            org-canvas--diff-by-title-note))
   ((plist-get entry :module-id)
    (format "  UNCLAIMED %s (item id %s in module '%s' has the type and content of an unstamped heading there, and no heading claims it; the next sync adopts it)\n"
            (plist-get entry :title) (plist-get entry :id) (plist-get entry :where)))
   ((plist-get entry :quiz-id)
    (format "  UNCLAIMED %s (item id %s in quiz '%s' opens with the title of an unstamped heading there, and no heading claims it; the next sync adopts it)\n"
            (plist-get entry :title) (plist-get entry :id) (plist-get entry :where)))
   (t
    (format "  UNCLAIMED %s (Canvas id %s has this title and no heading claims it; adopt it with org-canvas-adopt-at-point, which stamps %s, or rename)\n"
            (plist-get entry :title) (plist-get entry :id)
            (or (plist-get entry :property) "CANVAS_ID")))))

(defun org-canvas--diff-partner-text (entry)
  "Return how a MOVED row names ENTRY's paired heading (issue #342).
The heading's shown title and line, so a pair whose titles differ —
a column renumbered in Org, an item renamed on Canvas — is legible."
  (let ((title (plist-get entry :partner-title))
        (line (plist-get entry :line)))
    (cond ((and title line)
           (format "the unstamped heading '%s' (line %d)" title line))
          (title (format "the unstamped heading '%s'" title))
          (t "an unstamped heading"))))

(defconst org-canvas--diff-move-outcome
  "moves it there, keeping its id (if Canvas refuses the move, it recreates it there with a new id and deletes this copy)"
  "What the next sync does with a stamped item another module holds.
The module sync moves it by GraphQL and falls back to the recreate
issue #105 settled on (issue #352).")

(defun org-canvas--diff-moved-line (entry)
  "Return the report line of MOVED ENTRY, a module item (issue #294).
Stamping the item's id on the heading in its new module, which `s'
does (issue #342), makes the next sync move this item there, keeping
its id, rather than create a second one (issues #343, #352).  An
entry paired by title only says so (issue #299)."
  (format "  MOVED     %s (item id %s sits in module '%s'; %s places it in '%s', so the next sync creates it there and leaves this copy — stamp CANVAS_ID %s on that heading and the next sync instead %s%s)\n"
          (plist-get entry :title) (plist-get entry :id)
          (plist-get entry :where) (org-canvas--diff-partner-text entry)
          (plist-get entry :to) (plist-get entry :id)
          org-canvas--diff-move-outcome
          (if (plist-get entry :by-title)
              (concat "; " org-canvas--diff-by-title-note)
            "")))

(defun org-canvas--diff-relocate-line (entry)
  "Return the report line of RELOCATE ENTRY, a module item (issue #343)."
  (format "  RELOCATE  %s (item id %s sits in module '%s'; its heading, stamped with that id, is under '%s', so the next sync %s)\n"
          (plist-get entry :title) (plist-get entry :id)
          (plist-get entry :where) (plist-get entry :to)
          org-canvas--diff-move-outcome))

(defun org-canvas--diff-container (entry)
  "Return the container of ENTRY, for the text of its row, or nil.
A module item's row names its module (issue #177), a New Quiz item's
its quiz (issue #322), each by kind and title; an entry of its own is
in no container."
  (when-let* ((where (plist-get entry :where)))
    (format "%s '%s'" (or (plist-get entry :container) "module") where)))

(defun org-canvas--diff-where (entry prefix)
  "Return PREFIX, a space and ENTRY's container, or \"\" for none."
  (let ((container (org-canvas--diff-container entry)))
    (if container (concat prefix " " container) "")))

(defun org-canvas--diff-pending-line (entry)
  "Return the report line of PENDING ENTRY, a create for the next sync."
  (format "  PENDING   %s (no %s%s; the next sync creates it)\n"
          (plist-get entry :title)
          (or (plist-get entry :property) "CANVAS_ID")
          (org-canvas--diff-where entry ", in")))

(defun org-canvas--diff-insert-fields (fields)
  "Insert FIELDS, (NAME ORG CANVAS) triples, under a CHANGED row."
  (dolist (field fields)
    (insert (format "              %-18s org: %-24s canvas: %s\n"
                    (nth 0 field) (nth 1 field) (nth 2 field)))))

(defun org-canvas--diff-override-line (entry)
  "Return the report line of override row ENTRY (issue #411)."
  (let ((who (plist-get entry :title))
        (where (org-canvas--diff-container entry))
        (dates (plist-get entry :dates)))
    (pcase (plist-get entry :state)
      ('extra
       (format "  EXTRA     %s (override id %s of %s%s; no row of the heading's overrides table claims it, so a push keeps it — push --prune-overrides drops it, or p pulls the table%s)\n"
               who (plist-get entry :id) where
               (if (string-empty-p dates) "" (concat ", " dates))
               (if (plist-get entry :redundant)
                   "; its dates are the assignment's own, which a pull writes no row for"
                 "")))
      ('missing
       (format "  MISSING   %s (row of %s%s; Canvas holds no such override, so the next push of that heading creates it)\n"
               who where (if (string-empty-p dates) "" (concat ", " dates))))
      (_
       (format "  CHANGED   %s (override id %s of %s)\n"
               who (plist-get entry :id) where)))))

(defun org-canvas--diff-insert-entry (entry)
  "Insert one ENTRY of a drift report at point."
  (pcase (plist-get entry :kind)
    ('override
     (insert (org-canvas--diff-override-line entry))
     (org-canvas--diff-insert-fields (plist-get entry :fields)))
    ('missing
     (insert (format "  MISSING   %s (id %s is not in %s%s)\n"
                     (plist-get entry :title) (plist-get entry :id)
                     ;; An item is missing from its quiz (#322).
                     (or (org-canvas--diff-container entry) "this course")
                     ;; A pull already found it deleted (#392).
                     (if (plist-get entry :deleted)
                         (format "; a pull marked it %s %s"
                                 org-canvas--prop-canvas-deleted
                                 (plist-get entry :deleted))
                       ""))))
    ('extra
     (insert (format "  EXTRA     %s (id %s%s, no Org heading claims it)\n"
                     (plist-get entry :title) (plist-get entry :id)
                     ;; A module item says which module it sits in (#177).
                     (org-canvas--diff-where entry " in")))
     ;; What the object is, from the list reply already read (#296).
     (when-let* ((details (plist-get entry :details)))
       (insert (format "              %s\n" details))))
    ('stale-ack
     (insert (format "  STALE-ACK id %s is acknowledged in org-canvas-diff-known-extras but no longer exists on Canvas — remove the entry%s\n"
                     (plist-get entry :id)
                     (let ((note (plist-get entry :note)))
                       (if note (format " (%s)" note) "")))))
    ('unclaimed (insert (org-canvas--diff-unclaimed-line entry)))
    ('moved (insert (org-canvas--diff-moved-line entry)))
    ('relocate (insert (org-canvas--diff-relocate-line entry)))
    ('pending (insert (org-canvas--diff-pending-line entry)))
    ('note
     (insert (format "  NOTE      %s (%s on Canvas is %s; no %s declares it — informational, not drift)\n"
                     (plist-get entry :title) (plist-get entry :observed)
                     (plist-get entry :remote) (plist-get entry :property))))
    ('modified
     (insert (format "  CHANGED   %s%s%s\n"
                     (plist-get entry :title)
                     (org-canvas--diff-where entry " in")
                     (if (plist-get entry :remote-newer)
                         (format " (Canvas updated %s)" (plist-get entry :updated))
                       "")))
     (org-canvas--diff-insert-fields (plist-get entry :fields))
     ;; A timestamp-only change used to be a puzzle: every compared
     ;; field matched, so the row could not say what had changed.
     (when (and (plist-get entry :remote-newer) (null (plist-get entry :fields)))
       (insert (format "              (no compared property differs; the change is in a field this report does not compare — e.g. %s)\n"
                       (if (plist-get entry :body-compared)
                           "overrides or a rubric association"
                         "the description or overrides")))))))

(defun org-canvas--diff-insert-row (feature-name entry)
  "Insert ENTRY of FEATURE-NAME's report at point, as an actionable row.
The text carries an `org-canvas-diff-row' property naming the feature
and the entry, which is what the report's keys act on (issue #103).
Batch output prints the same text, properties and all invisible."
  (let ((start (point)))
    (org-canvas--diff-insert-entry entry)
    (put-text-property start (point) 'org-canvas-diff-row
                       (list :feature feature-name :entry entry))))

(defun org-canvas--diff-count (results)
  "Return the total number of divergences across RESULTS."
  (apply #'+ (mapcar (lambda (r) (+ (length (plist-get r :divergences))
                                    (length (plist-get r :extra))))
                     results)))

(defun org-canvas--diff-suppressed-parts (result)
  "Return the \"Not checked\" phrases of one feature RESULT, nils included.
What its `:skip-fn' held back, and the New Quizzes' assignments taken
out of the Assignments extras (:covered, issue #313)."
  (let ((name (plist-get result :name)))
    (mapcar (lambda (pair)
              (let ((n (car pair)))
                (when (and n (> n 0))
                  (format "%d %s (%s)" n name (cdr pair)))))
            (list (cons (plist-get result :suppressed)
                        (or (plist-get result :skip-reason)
                            "excluded by this module"))
                  (cons (plist-get result :covered)
                        org-canvas--diff-new-quiz-covered-reason)))))

(defun org-canvas--diff-suppressed-note (results)
  "Return a line naming remote items no check in RESULTS covered, or nil.
Items a `:skip-fn' holds back are not compared and cannot
show up as extra, so without this the report reads as full coverage."
  (let ((parts (delq nil (mapcan #'org-canvas--diff-suppressed-parts results))))
    (when parts
      (format "Not checked: %s.\n" (mapconcat #'identity parts ", ")))))

(defun org-canvas--diff-pending-count (results)
  "Return the count of PENDING rows across RESULTS (issue #294)."
  (apply #'+ (mapcar (lambda (r) (length (plist-get r :pending))) results)))

(defun org-canvas--diff-relocate-count (results)
  "Return the count of RELOCATE rows across RESULTS (issue #343)."
  (apply #'+ (mapcar (lambda (r) (length (plist-get r :relocate))) results)))

(defun org-canvas--diff-apart-text (notes pending relocate)
  "Return the section header's counts of rows that are not drift.
NOTES, PENDING and RELOCATE are the section's lists of each."
  (concat (if notes (format ", %d note(s)" (length notes)) "")
          (if pending (format ", %d pending create(s)" (length pending)) "")
          (if relocate (format ", %d relocation(s)" (length relocate)) "")))

(defun org-canvas--diff-weight-totals-line (totals)
  "Return the report line of the group weight TOTALS, (ORG . CANVAS).
Printed under the Assignment Groups section and not counted as drift
\(issue #296): each cause of the difference is a row of its own, or a
pending create, or the stock group the footer names."
  (format "  WEIGHTS   Org's groups sum to %s%%, Canvas's to %s%% (a group only on Canvas, a weight not yet pushed, or a group not yet created; matters when the course weights final grades by group — not counted as drift)\n"
          (org-canvas--diff-number (car totals))
          (org-canvas--diff-number (cdr totals))))

(defun org-canvas--diff-render-section (result)
  "Insert the section of one feature RESULT with rows.
Divergences and extras come first, under a count of them; the notes
\(issue #293), pending creates (issue #294) and relocations (issue
#343) follow, counted apart, since they are not drift."
  (let* ((rows (append (plist-get result :divergences) (plist-get result :extra)))
         (notes (plist-get result :notes))
         (pending (plist-get result :pending))
         (relocate (plist-get result :relocate)))
    (insert (format "%s: %d divergence(s)%s\n"
                    (plist-get result :name) (length rows)
                    (org-canvas--diff-apart-text notes pending relocate)))
    (dolist (entry (append rows notes pending relocate))
      (org-canvas--diff-insert-row (plist-get result :name) entry))
    (when-let* ((totals (plist-get result :weight-totals)))
      (insert (org-canvas--diff-weight-totals-line totals)))
    (insert "\n")))

(defun org-canvas--diff-insert-relocate-footer (results)
  "Insert the footer line counting the RELOCATE rows of RESULTS, if any."
  (let ((n (org-canvas--diff-relocate-count results)))
    (when (> n 0)
      (insert (format "Relocations: %d module item%s the next sync moves into %s heading's module, keeping the id (not counted as drift).\n"
                      n (if (= n 1) "" "s") (if (= n 1) "its" "their"))))))

(defun org-canvas--diff-render (results)
  "Render RESULTS into a report string."
  (with-temp-buffer
    (insert "org-canvas Drift Report\n")
    (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
    (insert (make-string 60 ?=) "\n\n")
    (dolist (result results)
      (let ((err (plist-get result :error)))
        (cond
         ((plist-get result :excluded)
          (insert (format "%s: not checked (org-canvas-diff-excluded-features)%s\n\n"
                          (plist-get result :name)
                          (if (plist-get result :references-scanned)
                              "; bodies read for referenced media only"
                            ""))))
         (err
          (insert (format "%s: could not check (%s)\n\n"
                          (plist-get result :name) err)))
         ((or (plist-get result :divergences) (plist-get result :extra)
              (plist-get result :notes) (plist-get result :pending)
              (plist-get result :relocate) (plist-get result :weight-totals))
          (org-canvas--diff-render-section result)))))
    (let ((total (org-canvas--diff-count results))
          (pending (org-canvas--diff-pending-count results)))
      (insert (make-string 60 ?=) "\n")
      (insert (if (zerop total)
                  "No drift: Canvas matches the Org files.\n"
                (format "%d divergence(s) found.\n" total)))
      (when (> pending 0)
        (insert (format "Pending creates: %d heading%s with no Canvas id, which the next sync creates (not counted as drift).\n"
                        pending (if (= pending 1) "" "s"))))
      (org-canvas--diff-insert-relocate-footer results)
      (when-let* ((note (org-canvas--diff-suppressed-note results)))
        (insert note))
      (let ((acked (apply #'+ (mapcar (lambda (r)
                                        (or (plist-get r :acknowledged) 0))
                                      results))))
        (when (> acked 0)
          (insert (format "Acknowledged extras: %d (org-canvas-diff-known-extras).\n"
                          acked))))
      (let ((referenced (apply #'+ (mapcar (lambda (r)
                                             (or (plist-get r :referenced) 0))
                                           results))))
        (when (> referenced 0)
          (insert (format "Referenced media: %d unclaimed file%s embedded in course content (org-canvas-diff-scan-references).\n"
                          referenced (if (= referenced 1) "" "s"))))))
    (buffer-string)))

;;;; Acting on the Report
;;
;; The report named problems and offered no verbs: visiting the heading,
;; acknowledging an extra, deleting a stray remote object, pulling one
;; item all happened somewhere else, by hand — a one-off elisp script
;; around a DELETE, a drawer edit, a line appended to a config file, a
;; search for the heading (issue #103).  Each action's machinery already
;; existed; only the rows could not reach it.  Rows carry their entry as
;; a text property, and these commands act on the row at point.

(defcustom org-canvas-diff-acknowledge-function #'org-canvas--diff-acknowledge-save
  "Function that persists `org-canvas-diff-known-extras' after the report edits it.
Called with the new value of the whole list, after the variable itself
has been set, when `a' on a report row acknowledges an extra or drops
a stale acknowledgment.  The default saves through `customize-save-variable';
a course that keeps its configuration in a file of its own can name a
function that writes the list there instead."
  :type 'function
  :group 'org-canvas)

(defun org-canvas--diff-acknowledge-save (extras)
  "Save EXTRAS as `org-canvas-diff-known-extras' with Customize."
  (customize-save-variable 'org-canvas-diff-known-extras extras))

(defvar org-canvas-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-canvas-diff-visit)
    (define-key map (kbd "b") #'org-canvas-diff-browse)
    (define-key map (kbd "B") #'org-canvas-diff-browse-edit)
    (define-key map (kbd "a") #'org-canvas-diff-acknowledge)
    (define-key map (kbd "k") #'org-canvas-diff-delete)
    (define-key map (kbd "p") #'org-canvas-diff-pull)
    (define-key map (kbd "s") #'org-canvas-diff-stamp-move)
    (define-key map (kbd "S") #'org-canvas-diff-stamp-moves)
    (define-key map (kbd "g") #'org-canvas-diff-refresh)
    (define-key map (kbd "TAB") #'org-canvas-diff-next-row)
    (define-key map (kbd "<backtab>") #'org-canvas-diff-previous-row)
    map)
  "Keymap for `org-canvas-diff-mode'.")

(define-derived-mode org-canvas-diff-mode special-mode "Canvas-Diff"
  "Major mode for the drift report, with actions on its rows.
\\<org-canvas-diff-mode-map>
\\[org-canvas-diff-visit] visits the Org heading of the row at point, or
opens the Canvas object of an EXTRA row in a browser;
\\[org-canvas-diff-browse] opens the Canvas page of any row whose object
Canvas still holds, and \\[org-canvas-diff-browse-edit] its edit page;
\\[org-canvas-diff-acknowledge] acknowledges the EXTRA at point (or drops a
STALE-ACK), or on a CHANGED row with no compared property differing
adopts the Canvas timestamp as the heading's baseline
\(`org-canvas-diff-adopt-stamp'); \\[org-canvas-diff-delete] deletes the
remote object of an EXTRA, UNCLAIMED or MOVED row, after confirming;
\\[org-canvas-diff-pull] pulls a CHANGED row's item over the heading, or
an EXTRA quiz or New Quiz into a new heading;
\\[org-canvas-diff-stamp-move] stamps a MOVED row's item id on the heading it
was paired with, and \\[org-canvas-diff-stamp-moves] every MOVED row's,
sending nothing;
\\[org-canvas-diff-adopt-drift] writes what Canvas holds of a CHANGED row's
fields, and nothing else, into its heading, or appends an EXTRA
override row to its table (`org-canvas-drift'), sending nothing;
\\[org-canvas-diff-show-row] shows a CHANGED row's Canvas version in full;
\\[org-canvas-diff-refresh] runs the report again;
\\[org-canvas-diff-next-row] and \\[org-canvas-diff-previous-row] move
between rows."
  (setq-local revert-buffer-function (lambda (&rest _) (org-canvas-diff))))

(defconst org-canvas--diff-key-legend
  "RET visit   b/B browse/edit on Canvas   a acknowledge/adopt   k delete   p pull   s/S stamp moved   A adopt drift   = show in full   g refresh   TAB next row\n"
  "One line naming the report's keys, shown in the interactive buffer only.")

(defun org-canvas--diff-row-at-point ()
  "Return the (:feature NAME :entry ENTRY) plist of the row at point.
Signals a `user-error' off any row."
  (or (get-text-property (point) 'org-canvas-diff-row)
      (user-error "No report row at point")))

(defun org-canvas--diff-row-feature (row)
  "Return the feature registry entry for ROW, or signal.
A New Quiz row's entry is its pull-only one (issue #313)."
  (or (org-canvas--diff-find-feature (plist-get row :feature))
      (user-error "%s is not a registered feature" (plist-get row :feature))))

(defun org-canvas--diff-rewrite-row (text)
  "Replace the row at point with TEXT, keeping its row property.
TEXT is one line without a newline.  How an action leaves its mark
without re-running every list request."
  (let* ((row (org-canvas--diff-row-at-point))
         (inhibit-read-only t)
         (start (line-beginning-position))
         (end (line-end-position)))
    (delete-region start end)
    (goto-char start)
    (insert text)
    (put-text-property start (point) 'org-canvas-diff-row row)
    (goto-char start)))

(defun org-canvas--diff-find-heading-by-id (file id-property id)
  "Return the position of the heading in FILE whose ID-PROPERTY is ID, or nil."
  (when (and file (file-exists-p file))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (catch 'found
          (org-map-entries
           (lambda ()
             (when (equal (org-entry-get (point) id-property) id)
               (throw 'found (point))))
           nil 'file)
          nil)))))

(defun org-canvas--diff-pending-position (entry)
  "Return (FILE . POSITION) of the heading of PENDING row ENTRY, or nil.
The row carries the file and line the heading was read from; when the
heading there is no longer the one named, it is looked up by title."
  (let ((file (plist-get entry :file))
        (heading (plist-get entry :heading)))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (or (plist-get entry :line) 1)))
          (let ((pos (if (and (org-at-heading-p)
                              (equal (org-get-heading t t t t) heading))
                         (point)
                       (org-canvas--find-heading-in-file file heading))))
            (and pos (cons file pos))))))))

(defun org-canvas--diff-relocate-position (entry)
  "Return (FILE . POSITION) of the heading of RELOCATE row ENTRY, or nil.
The heading carries the row's item id (issue #343)."
  (let* ((file (plist-get entry :file))
         (pos (org-canvas--diff-find-heading-by-id file "CANVAS_ID"
                                                   (plist-get entry :id))))
    (and pos (cons file pos))))

(defun org-canvas--diff-heading-position (feature entry)
  "Return (FILE . POSITION) of the heading ENTRY of FEATURE describes, or nil.
MISSING, CHANGED and NOTE rows are found by their id property; an UNCLAIMED
row names an unstamped heading, found by title; a PENDING row names
its own file and line (issue #294), as every New Quiz item row does
\(issue #322): an item heading is no feature's own; a RELOCATE row
names its own file and id (issue #343)."
  (cond ((eq (plist-get entry :kind) 'relocate)
         (org-canvas--diff-relocate-position entry))
        ((or (eq (plist-get entry :kind) 'pending) (plist-get entry :file))
         (org-canvas--diff-pending-position entry))
        (t (org-canvas--diff-feature-heading-position feature entry))))

(defun org-canvas--diff-feature-heading-position (feature entry)
  "Return (FILE . POSITION) of ENTRY's heading in FEATURE's file, or nil."
  (let* ((file-var (plist-get feature :file-var))
         (file (and file-var (boundp file-var) (symbol-value file-var)))
         (pos (pcase (plist-get entry :kind)
                ('unclaimed
                 (org-canvas--find-heading-in-file file (plist-get entry :title)))
                ((or 'missing 'modified 'note)
                 (org-canvas--diff-find-heading-by-id
                  file (or (plist-get feature :id-property) "CANVAS_ID")
                  (plist-get entry :id))))))
    (and pos (cons file pos))))

(defun org-canvas--diff-goto-heading (feature entry)
  "Make the buffer of ENTRY's heading current, with point on it, or signal.
FEATURE is the registry entry that says which file to look in.
Returns the buffer."
  (let ((where (org-canvas--diff-heading-position feature entry)))
    (unless where
      (user-error "Cannot find the heading for '%s' in %s"
                  (plist-get entry :title)
                  (or (plist-get feature :name) (plist-get entry :file))))
    (set-buffer (org-canvas--find-file-noselect (car where)))
    (goto-char (cdr where))
    (org-back-to-heading t)
    (current-buffer)))

(defun org-canvas--diff-visits-heading-p (entry)
  "Return non-nil for a report ENTRY with an Org heading to visit.
A module item's UNCLAIMED row names its Canvas item, whose heading
has no title of its own to find it by."
  (pcase (plist-get entry :kind)
    ((or 'missing 'modified 'note 'pending 'relocate 'override) t)
    ('unclaimed (not (plist-get entry :module-id)))))

(defun org-canvas-diff-visit ()
  "Visit what the row at point is about.
A row with an Org heading (MISSING, CHANGED, UNCLAIMED, NOTE, PENDING,
RELOCATE) opens the course file with point on that heading.  An EXTRA or MOVED
row, or a module item's UNCLAIMED row, names a Canvas object, which is
opened in a browser: where the API said it lives, or else the page
the feature's registered rules name (issue #313)."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry)))
    (if (org-canvas--diff-visits-heading-p entry)
        ;; A PENDING or RELOCATE row carries its own file (#294, #343),
        ;; as a New Quiz item's row does (issue #322).
        (let* ((feature (unless (or (memq (plist-get entry :kind)
                                          '(pending relocate))
                                    (plist-get entry :file))
                          (org-canvas--diff-row-feature row)))
               (buf (save-excursion (org-canvas--diff-goto-heading feature entry)))
               (pos (with-current-buffer buf (point))))
          (pop-to-buffer buf)
          (goto-char pos)
          (when (fboundp 'org-fold-show-context) (org-fold-show-context)))
      ;; The quiz service gives a New Quiz no `html_url'; the page its
      ;; registered rules name serves instead (issue #313).
      (browse-url (org-canvas--diff-row-web-url row nil)))))

;; RET on a row with a heading visits the heading, so a row that exists
;; on both sides had no way to the Canvas page at all (issue #292).

(defun org-canvas--diff-web-entry (feature)
  "Return the entry naming where FEATURE's items live on the Canvas site.
FEATURE itself when it declares `:web-pages'; otherwise what its file
registered through `org-canvas-register-web-pages', as a New Quiz's
pull-only entry does (issues #292, #313), or FEATURE when there is
nothing; nil for no FEATURE."
  (when feature
    (if (plist-get feature :web-pages)
        feature
      (let* ((var (plist-get feature :file-var))
             (file (and var (boundp var) (symbol-value var))))
        (or (org-canvas--web-pages-for-file file) feature)))))

(defun org-canvas--diff-row-web-url (row edit)
  "Return the Canvas web address of ROW's object, or signal.
With EDIT, the edit page the feature's `:web-pages' names, falling
back to the object's page; otherwise the `html_url' Canvas gave, or
the page assembled from the registry when it gave none.  A New Quiz
item has no page of its own, so its row opens its quiz's (issue #322)."
  (let* ((entry (plist-get row :entry))
         (quiz-id (plist-get entry :quiz-id))
         (feature (org-canvas--diff-web-entry
                   (org-canvas--diff-find-feature
                    (if quiz-id "New Quizzes" (plist-get row :feature)))))
         (id (or quiz-id (plist-get entry :id)))
         (html (plist-get entry :html-url))
         (assembled (and feature id
                         (org-canvas--feature-web-url feature id edit))))
    (or (if edit (or assembled html) (or html assembled))
        (user-error "No Canvas page is known for %s '%s'"
                    (plist-get row :feature)
                    (or (plist-get entry :title) id)))))

(defun org-canvas-diff-browse (&optional edit)
  "Open the Canvas page of the row at point in a browser.
With a prefix argument EDIT, open its edit page where Canvas has one.
A MISSING or STALE-ACK row names an object Canvas no longer holds,
and a PENDING row one it does not hold yet, so there is nothing to
open.  Sends nothing to Canvas.  Returns the address opened."
  (interactive "P")
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry)))
    (pcase (plist-get entry :kind)
      ((or 'missing 'stale-ack)
       (user-error "'%s' is not on Canvas any more; nothing to open"
                   (or (plist-get entry :title) (plist-get entry :id))))
      ('pending
       (user-error "'%s' is not on Canvas yet; the next sync creates it"
                   (plist-get entry :title))))
    (let ((url (org-canvas--diff-row-web-url row edit)))
      (browse-url url)
      url)))

(defun org-canvas-diff-browse-edit ()
  "Open the Canvas edit page of the row at point in a browser."
  (interactive)
  (org-canvas-diff-browse t))

(defun org-canvas-diff-acknowledge ()
  "Acknowledge the EXTRA, UNCLAIMED or MOVED row at point as deliberate.
Adds it to `org-canvas-diff-known-extras', with a note if you give one,
and persists the list through `org-canvas-diff-acknowledge-function'.
On a STALE-ACK row, drops the acknowledgment instead.  On a CHANGED
row with no compared property differing, adopts the Canvas timestamp
as the heading's baseline (`org-canvas-diff-adopt-stamp', issue #257)."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry))
         (feature (plist-get row :feature))
         (id (plist-get entry :id)))
    (pcase (plist-get entry :kind)
      ('stale-ack
       (setq org-canvas-diff-known-extras
             (cl-remove-if (lambda (k)
                             (and (string= (org-canvas--diff-normalize-name (nth 0 k))
                                           (org-canvas--diff-normalize-name feature))
                                  (string= (format "%s" (nth 1 k)) id)))
                           org-canvas-diff-known-extras))
       (funcall org-canvas-diff-acknowledge-function org-canvas-diff-known-extras)
       (org-canvas--diff-rewrite-row
        (format "  DROPPED   acknowledgment of id %s (%s)" id feature))
       (message "Dropped the acknowledgment of %s %s." feature id))
      ((or 'extra 'unclaimed 'moved)
       (let* ((note (read-string (format "Note for %s '%s' (optional): "
                                         feature (plist-get entry :title))))
              (note (and (not (string-empty-p note)) note)))
         (setq org-canvas-diff-known-extras
               (append org-canvas-diff-known-extras (list (list feature id note))))
         (funcall org-canvas-diff-acknowledge-function org-canvas-diff-known-extras)
         (org-canvas--diff-rewrite-row
          (format "  ACK       %s (id %s acknowledged%s)"
                  (plist-get entry :title) id (if note (format ": %s" note) "")))
         (message "Acknowledged %s %s." feature id)))
      ;; On a CHANGED row, accepting what Canvas holds means adopting
      ;; its timestamp — when nothing compared differs (issue #257).
      ('modified (org-canvas-diff-adopt-stamp))
      ('override
       (user-error "An override belongs in its heading's table: p pulls the table, or edit the row and push the heading"))
      (kind (user-error "A %s row is not something to acknowledge" (upcase (symbol-name kind)))))))


(defun org-canvas-diff-refresh ()
  "Run the drift report again."
  (interactive)
  (org-canvas-diff))

(defun org-canvas--diff-row-start (pos)
  "Return where the row run containing POS begins.
A CHANGED row spans several lines, all carrying the same property
value; its start is the line the verb is on."
  (let ((row (get-text-property pos 'org-canvas-diff-row)))
    (while (and (> pos (point-min))
                (eq (get-text-property (1- pos) 'org-canvas-diff-row) row))
      (setq pos (1- pos)))
    pos))

(defun org-canvas-diff-next-row ()
  "Move point to the next report row."
  (interactive)
  (let* ((row (get-text-property (point) 'org-canvas-diff-row))
         (pos (point))
         (found nil))
    (while (and row (< pos (point-max))
                (eq (get-text-property pos 'org-canvas-diff-row) row))
      (setq pos (1+ pos)))
    (while (and (not found) (< pos (point-max)))
      (if (get-text-property pos 'org-canvas-diff-row)
          (setq found pos)
        (setq pos (1+ pos))))
    (if found (goto-char found) (user-error "No more rows"))))

(defun org-canvas-diff-previous-row ()
  "Move point to the previous report row."
  (interactive)
  (let ((pos (if (get-text-property (point) 'org-canvas-diff-row)
                 (org-canvas--diff-row-start (point))
               (point)))
        (found nil))
    (while (and (not found) (> pos (point-min)))
      (setq pos (1- pos))
      (when (get-text-property pos 'org-canvas-diff-row)
        (setq found pos)))
    (if found
        (goto-char (org-canvas--diff-row-start found))
      (user-error "No previous row"))))


(provide 'org-canvas-diff-report)
;;; org-canvas-diff-report.el ends here
