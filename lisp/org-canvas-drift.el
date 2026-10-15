;;; org-canvas-drift.el --- Show and adopt what a drift row names -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Two verbs the drift report lacked, each a separate command it never
;; runs itself (Hard Rule 19).
;;
;; ADOPTING THE DRIFT (issue #498)
;; ===============================
;; A CHANGED row names the fields where Canvas differs, and the natural
;; fix was `p', a pull, which rewrites the whole heading: it deleted
;; PUBLISHED: true as the default it is, wrote MUTED where Canvas said
;; muted, dropped PAYLOAD_HASH and the drawer's alignment, and ran the
;; description through pandoc, losing local-only export attributes and
;; table rules Canvas never held.  On a live course the pull of twenty
;; headings could not be kept, and a script copied the stamps and the
;; few real changes across instead.
;;
;; `A' on the row (`org-canvas-diff-adopt-drift') and
;; `org-canvas-diff-adopt-drifts' write what the row names and nothing
;; else: each differing property in its Org spelling, the heading's
;; title when Canvas renamed it, and, only when asked, the body.  When
;; every field was written, CANVAS_UPDATED_AT takes Canvas's stamp; a
;; field held back keeps the old stamp, so the next push still meets
;; the conflict check.  A row with no compared property differing is
;; stamp adoption (issue #257), unchanged.  On an EXTRA override row,
;; `A' appends the override to the heading's table as a new row,
;; leaving the existing rows as they are.  Nothing is sent to Canvas.
;;
;; SHOWING THE CANVAS SIDE (issue #500)
;; ====================================
;; The report shows a changed body as two excerpts around the first
;; difference, enough to know that it changed but not what it says.
;; `=' on a CHANGED row (`org-canvas-diff-show-row') and
;; `org-canvas-show-at-point' (`org-canvas-show-headings' by name, and
;; `scripts/org-canvas show') print the Canvas version of one heading:
;; every compared property beside the Org value, the title, and the
;; body as Canvas holds it, as the Org a pull would convert it to, and
;; as a unified diff against the heading's own body.  They read and
;; never write.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)
(require 'org-canvas-diff)

;; Appending an override row reads and writes the table the way the
;; sections module does; `org-canvas' loads it.
(declare-function org-canvas--override-date-columns "org-canvas-sections" (header))
(declare-function org-canvas--override-target-cell "org-canvas-sections" (ov))
(declare-function org-canvas--override-format-cell "org-canvas-sections"
                  (iso8601))
(declare-function org-canvas--override-emit-table "org-canvas-sections"
                  (overrides &optional parent-due parent-unlock parent-lock))
;; Whether a heading is what its last push sent, as setting the
;; published flag alone decides it (issue #466); `org-canvas' loads it.
(declare-function org-canvas--publish-heading-clean-p "org-canvas-publish"
                  (spec))

;;;; Fields

(defun org-canvas--drift-props (feature)
  "Return the property registry entry of FEATURE, a feature entry, or nil."
  (org-canvas--diff-find-properties (plist-get feature :name)))

(defun org-canvas--drift-body-label (props)
  "Return the field name a CHANGED row gives the body PROPS declares, or nil."
  (let ((key (plist-get props :body-api-key)))
    (and key (upcase key))))

(defun org-canvas--drift-spec (props name)
  "Return the spec of PROPS whose Org property is NAME, or nil."
  (cl-find name (plist-get props :properties)
           :key (lambda (s) (plist-get s :org-prop)) :test #'equal))

(defun org-canvas--drift-field-kind (props name)
  "Return how the field NAME of a CHANGED row is adopted, given PROPS.
`title', `body', `property', `intent' (a declaration, never adopted)
or `unknown'."
  (let ((spec (org-canvas--drift-spec props name)))
    (cond ((equal name org-canvas--diff-title-label) 'title)
          ((equal name (org-canvas--drift-body-label props)) 'body)
          ((null spec) 'unknown)
          ((plist-get spec :intent-of) 'intent)
          (t 'property))))

(defun org-canvas--drift-adopt-property (spec item)
  "Write SPEC's property at the heading at point from Canvas ITEM.
The value goes in its Org spelling, as a pull spells it, and is
written even when it is the default: the heading named the property,
and adopting must not turn an explicit PUBLISHED into silence.  A
field Canvas holds nothing in deletes the property.  Returns the text
written, or nil."
  (let* ((prop (plist-get spec :org-prop))
         (text (org-canvas--registry-remote-as-org
                spec (org-canvas--registry-remote-field spec item))))
    (if text
        (org-canvas-org-set-property (point) prop text)
      (org-entry-delete (point) prop))
    text))

(defun org-canvas--drift-adopt-title (feature item)
  "Give the heading at point the title in Canvas ITEM; return it.
FEATURE's `:title-field' names the field.  The TODO keyword, priority
and tags stay."
  (let ((title (org-canvas--diff-squeeze
                (alist-get (plist-get feature :title-field) item))))
    (org-edit-headline title)
    title))

;;;; The Body

(defconst org-canvas--drift-kept-block-re
  "^#\\+NAME:[ \t]+\\(?:overrides\\|accommodations\\)[ \t]*$"
  "A table a heading's body holds that its push reads apart from the body.")

(defun org-canvas--drift-body-adoptable-p (props)
  "Return non-nil when the body PROPS declares is the heading's own text.
A module with a `:body-fn' (a classic quiz's description stops at its
first question) or a `:body-remote-fn' (a New Quiz item) reads its body
in a way of its own, which only its pull writes back."
  (and (plist-get props :body-api-key)
       (not (plist-get props :body-fn))
       (not (plist-get props :body-remote-fn))))

(defun org-canvas--drift-kept-blocks (start end)
  "Return the overrides and accommodations tables between START and END.
Each is its `#+NAME:' line and its table rows, as text."
  (let ((blocks nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward org-canvas--drift-kept-block-re end t)
        (let ((begin (match-beginning 0)))
          (forward-line 1)
          (while (and (< (point) end) (looking-at-p "^[ \t]*|"))
            (forward-line 1))
          (push (buffer-substring-no-properties begin (point)) blocks))))
    (nreverse blocks)))

(defun org-canvas--drift-replace-body (html)
  "Replace the text of the heading at point with HTML converted to Org.
The text is what lies between the drawer and the first child heading.
The overrides and accommodations tables there are kept, verbatim, and
written first, where a pull writes them; the rest is Canvas's.  Canvas
file links become local ones, as a pull rewrites them."
  (let* ((bounds (org-canvas--pull-entry-text-bounds))
         (kept (org-canvas--drift-kept-blocks (car bounds) (cdr bounds)))
         (org (org-canvas--html-to-org-with-rewrite html)))
    (delete-region (car bounds) (cdr bounds))
    (goto-char (car bounds))
    (insert "\n"
            (mapconcat (lambda (b) (concat (string-trim-right b) "\n\n")) kept "")
            org "\n")))

(defun org-canvas--drift-has-children-p ()
  "Return non-nil when the heading at point has a child heading."
  (save-excursion
    (org-back-to-heading t)
    (org-goto-first-child)))

(defun org-canvas--drift-adopt-body (props item body)
  "Replace the body of the heading at point with Canvas ITEM's, if BODY.
PROPS is the feature's property registry entry.  Returns nil when the
body was written, else the reason it was held back: BODY nil (the
caller did not ask), a body only the module's own pull writes, or a
heading whose sub-headings are part of the body its push sends."
  (cond
   ((not (org-canvas--drift-body-adoptable-p props))
    "this module reads its body its own way; copy it by hand (= shows it) or pull with p")
   ((org-canvas--drift-has-children-p)
    "the heading's sub-headings are part of its body; copy it by hand (= shows it)")
   ((not body)
    "not asked; adopt it with --body (A asks), or copy it by hand (= shows it)")
   (t
    (save-excursion
      (org-canvas--drift-replace-body
       (or (org-canvas--diff-normalize-remote
            (cdr (org-canvas--diff-remote-body props item)))
           "")))
    nil)))

;;;; Adopting a CHANGED Row

(defun org-canvas--drift-adopt-field (feature props name item body)
  "Write field NAME of the heading at point from Canvas ITEM.
FEATURE and PROPS are the feature's entries; BODY says whether the
body may be replaced.  Returns nil when written, else why it was not."
  (pcase (org-canvas--drift-field-kind props name)
    ('title (org-canvas--drift-adopt-title feature item) nil)
    ('property (org-canvas--drift-adopt-property
                (org-canvas--drift-spec props name) item)
               nil)
    ('body (org-canvas--drift-adopt-body props item body))
    ('intent "a declaration of what Canvas should hold, never adopted from it")
    (_ "not a property this module registers")))

(defun org-canvas--drift-adopt-fields (feature entry body)
  "Write the Canvas side of each field CHANGED ENTRY names, at point.
FEATURE is the registry entry and BODY whether the body may be
replaced.  Returns (ADOPTED . HELD): the names written, and (NAME .
REASON) for each one held back."
  (let ((props (org-canvas--drift-props feature))
        (item (plist-get entry :item))
        (adopted nil) (held nil))
    (dolist (field (plist-get entry :fields))
      (save-excursion
        (org-back-to-heading t)
        (let ((reason (org-canvas--drift-adopt-field
                       feature props (car field) item body)))
          (if reason
              (push (cons (car field) reason) held)
            (push (car field) adopted)))))
    (cons (nreverse adopted) (nreverse held))))

(defun org-canvas--drift-entry-spec (feature)
  "Return the push-at-point spec of FEATURE, or nil (issue #466)."
  (cdr (assoc (org-canvas--singularize
               (downcase (replace-regexp-in-string
                          " " "-" (plist-get feature :name))))
              org-canvas--sync-entry-specs)))

(defun org-canvas--drift-restamp-hash (spec clean)
  "Hash the heading at point again when CLEAN, by SPEC's push.
A heading that was what its last push sent is, once adopted, what
Canvas holds, so its next sync has nothing to send.  One with local
edits of its own keeps its old hash, so the next sync sends them."
  (when clean
    (let ((hash (org-canvas--sync-heading-hash spec)))
      (if hash
          (org-canvas-org-set-property (point) org-canvas--prop-payload-hash hash)
        (org-entry-delete (point) org-canvas--prop-payload-hash)))))

(defun org-canvas--drift-adopt-here (feature entry body)
  "Adopt CHANGED ENTRY of FEATURE at the heading at point, and save.
BODY says whether the body may be replaced.  Returns the outcome's
plist tail: :outcome `adopted' or `partial', :adopted, :held and
:updated, the stamp written or nil.  The stamp moves only when every
field was written."
  (org-back-to-heading t)
  (let* ((spec (org-canvas--drift-entry-spec feature))
         (clean (and spec (org-canvas--publish-heading-clean-p spec)))
         (result (org-canvas--drift-adopt-fields feature entry body))
         (updated (plist-get entry :updated))
         (stamp (and (null (cdr result)) (stringp updated) updated)))
    (org-back-to-heading t)
    (when stamp
      (org-canvas-org-set-property (point) "CANVAS_UPDATED_AT" stamp))
    (org-canvas--drift-restamp-hash spec clean)
    (org-canvas--save-buffer)
    (list :outcome (if (cdr result) 'partial 'adopted)
          :adopted (car result) :held (cdr result) :updated stamp)))

(defun org-canvas--drift-at (where fn)
  "Call FN with point on the heading at WHERE, (FILE . POSITION).
Returns FN's value; point in the course buffer is restored."
  (with-current-buffer (org-canvas--find-file-noselect (car where))
    (save-excursion
      (goto-char (cdr where))
      (org-back-to-heading t)
      (funcall fn))))

(defun org-canvas--drift-log-outcome (outcome)
  "Log OUTCOME, an adoption's plist."
  (org-canvas--log-info org-canvas--logger "[Drift] %s %s '%s'%s%s; nothing sent"
    (upcase (symbol-name (plist-get outcome :outcome)))
    (plist-get outcome :feature) (plist-get outcome :title)
    (if (plist-get outcome :adopted)
        (format ": wrote %s" (mapconcat #'identity (plist-get outcome :adopted) ", "))
      "")
    (if (plist-get outcome :held)
        (format "; held %s" (mapconcat #'car (plist-get outcome :held) ", "))
      "")))

(defun org-canvas--drift-adopt-changed (feature entry body)
  "Adopt CHANGED ENTRY of FEATURE into its heading; return an outcome plist.
BODY says whether the body may be replaced.  A row with no compared
property differing adopts its stamp (`org-canvas--diff-adopt-entry',
issue #257), :outcome `stamped'.  Otherwise the fields are written
\(`adopted', or `partial' when one was held back), or, under a dry
run, only named (`dry-run').  `not-found' when no heading carries the
id, `refused' when the row holds no copy of the Canvas item."
  (let ((out (list :feature (plist-get feature :name)
                   :title (plist-get entry :title)))
        (where (org-canvas--diff-heading-position feature entry)))
    (cond
     ((null where) (append out (list :outcome 'not-found)))
     (org-canvas--dry-run
      (append out (list :outcome 'dry-run
                        :adopted (or (mapcar #'car (plist-get entry :fields))
                                     (list "CANVAS_UPDATED_AT")))))
     ((org-canvas--diff-adoptable-p entry)
      (append out (list :outcome 'stamped
                        :updated (org-canvas--diff-adopt-entry feature entry))))
     ((null (plist-get entry :item))
      (append out (list :outcome 'refused
                        :reason "the row holds no copy of the Canvas item; run the report again")))
     (t (let ((result (append out (org-canvas--drift-at
                                   where (lambda () (org-canvas--drift-adopt-here
                                                     feature entry body))))))
          (org-canvas--drift-log-outcome result)
          result)))))

;;;; Appending an Override Row

(defun org-canvas--drift-header (table)
  "Return the first row of TABLE, an `org-table-to-lisp' list, that is not a rule."
  (cl-find-if #'listp table))

(defun org-canvas--drift-override-cells (ov header)
  "Return the cells of a table row for override OV under HEADER.
HEADER is the table's first row; each date goes in the column its
title names (`org-canvas--override-date-columns').  Returns (missing
TITLE...) instead when OV carries a date the table has no column for:
adding the column would rewrite every row."
  (let ((cells (make-list (max 1 (length header)) ""))
        (missing nil))
    (setf (nth 0 cells) (org-canvas--override-target-cell ov))
    (cl-mapc (lambda (field col title)
               (let ((value (org-canvas--diff-normalize-remote (alist-get field ov))))
                 (when value
                   (if (and col (< col (length cells)))
                       (setf (nth col cells) (org-canvas--override-format-cell value))
                     (push title missing)))))
             '(due_at unlock_at lock_at)
             (org-canvas--override-date-columns header)
             '("Due At" "Unlock At" "Lock At"))
    (if missing (cons 'missing (nreverse missing)) cells)))

(defun org-canvas--drift-append-row (ov)
  "Append override OV to the table at point, after its last row.
Point is on the table.  The existing rows are left exactly as they are,
alignment included.  Returns nil, or why nothing was appended."
  (let ((cells (org-canvas--drift-override-cells
                ov (org-canvas--drift-header (org-table-to-lisp)))))
    (if (eq (car cells) 'missing)
        (format "the table has no %s column; add it, or pull the heading with p"
                (mapconcat #'identity (cdr cells) " or "))
      (goto-char (org-table-end))
      (insert "| " (mapconcat #'identity cells " | ") " |\n")
      nil)))

(defun org-canvas--drift-insert-table (ov)
  "Write a new overrides table holding OV under the drawer at point.
Returns nil, or why nothing was written."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let ((start (point)))
      (org-canvas--override-emit-table (list ov))
      (and (= start (point)) "the override carries no date to write"))))

(defun org-canvas--drift-append-override-here (ov)
  "Add override OV to the table of the heading at point, and save.
Returns nil, or why nothing was written."
  (let* ((end (save-excursion (org-end-of-subtree t) (point)))
         (table (save-excursion
                  (when (re-search-forward "^#\\+NAME:[ \t]+overrides[ \t]*$" end t)
                    (forward-line 1)
                    (and (org-at-table-p) (point)))))
         (reason (if table
                     (save-excursion (goto-char table) (org-canvas--drift-append-row ov))
                   (org-canvas--drift-insert-table ov))))
    (unless reason (org-canvas--save-buffer))
    reason))

(defun org-canvas--drift-append-override (entry)
  "Add EXTRA override row ENTRY to its heading's table; return an outcome.
The heading is the one carrying the row's assignment id in the row's
file.  :outcome is `appended', `dry-run', `refused' with :reason, or
`not-found'.  Nothing is sent: the next push of the heading claims
the override instead of deleting it (issue #411)."
  (let* ((out (list :feature org-canvas--diff-overrides-name
                    :title (format "%s (%s)" (plist-get entry :title)
                                   (plist-get entry :where))))
         (file (plist-get entry :file))
         (pos (org-canvas--diff-find-heading-by-id
               file "CANVAS_ID" (plist-get entry :assignment-id)))
         (ov (plist-get entry :override)))
    (cond
     ((or (null pos) (null ov)) (append out (list :outcome 'not-found)))
     (org-canvas--dry-run (append out (list :outcome 'dry-run)))
     (t (let ((reason (org-canvas--drift-at
                       (cons file pos)
                       (lambda () (org-canvas--drift-append-override-here ov)))))
          (if reason
              (append out (list :outcome 'refused :reason reason))
            (org-canvas--log-info org-canvas--logger
              "[Drift] Appended override %s to the table of '%s'; nothing sent"
              (plist-get entry :id) (plist-get entry :where))
            (append out (list :outcome 'appended))))))))

;;;; The Row Verb

(defun org-canvas--drift-extra-override-p (entry)
  "Return non-nil when ENTRY is an EXTRA override row (issue #411)."
  (and (eq (plist-get entry :kind) 'override)
       (eq (plist-get entry :state) 'extra)))

(defun org-canvas--drift-adopt-row (row body)
  "Adopt the Canvas side of ROW, a report row; return an outcome plist.
BODY says whether a CHANGED row's body may be replaced.  Anything but
a CHANGED row of a feature or an EXTRA override row is a `user-error'."
  (let ((entry (plist-get row :entry)))
    (cond
     ((org-canvas--drift-extra-override-p entry)
      (org-canvas--drift-append-override entry))
     ((not (eq (plist-get entry :kind) 'modified))
      (user-error "Only a CHANGED row or an EXTRA override row has a Canvas side to adopt"))
     ((plist-get entry :quiz-id)
      (user-error "A New Quiz item is adopted with its quiz: p pulls the quiz"))
     (t (org-canvas--drift-adopt-changed
         (org-canvas--diff-row-feature row) entry body)))))

(defun org-canvas--drift-row-body-p (row)
  "Return non-nil when the CHANGED ROW names its feature's body."
  (let ((label (org-canvas--drift-body-label
                (org-canvas--diff-find-properties (plist-get row :feature)))))
    (and label (assoc label (plist-get (plist-get row :entry) :fields)) t)))

(defun org-canvas--drift-ask-body (row)
  "Ask whether to replace ROW's body with Canvas's, when it names one."
  (and (not noninteractive)
       (org-canvas--drift-row-body-p row)
       (y-or-n-p (format "Replace the body of '%s' with Canvas's, converted from HTML (local-only markup in it is lost)? "
                         (plist-get (plist-get row :entry) :title)))))

(defun org-canvas--drift-held-text (held)
  "Return how an outcome line names the HELD fields, (NAME . REASON)."
  (mapconcat (lambda (h) (format "%s held: %s" (car h) (cdr h))) held "; "))

(defun org-canvas--drift-outcome-line (outcome)
  "Return the report line, without a newline, for adoption OUTCOME."
  (let ((title (plist-get outcome :title))
        (written (mapconcat #'identity (plist-get outcome :adopted) ", ")))
    (pcase (plist-get outcome :outcome)
      ('stamped (format "  ADOPTED   %s (CANVAS_UPDATED_AT set to %s, nothing sent)"
                        title (plist-get outcome :updated)))
      ('adopted (format "  ADOPTED   %s (%s written from Canvas; CANVAS_UPDATED_AT set to %s, nothing sent)"
                        title written (or (plist-get outcome :updated) "(none)")))
      ('partial (format "  PARTIAL   %s (%s; %s; CANVAS_UPDATED_AT kept, nothing sent)"
                        title (if (string-empty-p written) "nothing written"
                                (concat written " written"))
                        (org-canvas--drift-held-text (plist-get outcome :held))))
      ('appended (format "  ADOPTED   %s (row appended to the overrides table, nothing sent)"
                         title))
      ('dry-run (format "  DRY-RUN   %s (would write %s)"
                        title (if (string-empty-p written) "the table row" written)))
      ('not-found (format "  NOT FOUND %s (no heading carries its id)" title))
      (_ (format "  HELD      %s (%s)" title (plist-get outcome :reason))))))

(defun org-canvas--drift-rewrite-whole-row (text)
  "Replace every line of the row at point with TEXT, keeping its property.
A CHANGED row spans its field lines, which an adoption leaves stale."
  (let* ((row (org-canvas--diff-row-at-point))
         (inhibit-read-only t)
         (start (org-canvas--diff-row-start (point)))
         (end (next-single-property-change start 'org-canvas-diff-row nil
                                           (point-max))))
    (delete-region start end)
    (goto-char start)
    (insert text "\n")
    (put-text-property start (1- (point)) 'org-canvas-diff-row row)
    (goto-char start)))

(defun org-canvas-diff-adopt-drift ()
  "Write the Canvas side of the row at point into Org, and nothing else.
On a CHANGED row, each field the row names takes the Canvas value:
a property in its Org spelling (explicit, even the default), the
heading's title when Canvas renamed it, and the body, when the row
names it, only if you say so (it is converted from HTML, and
local-only markup in it is lost).  When every field was written,
CANVAS_UPDATED_AT takes Canvas's stamp and PAYLOAD_HASH is hashed
again if the heading was clean; a field held back keeps the stamp.
A row with no compared property differing adopts its stamp, as `a'
does.  On an EXTRA override row, the override is appended to the
heading's table, the other rows untouched.  Sends nothing (issue
#498).  Returns the outcome plist."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (outcome (org-canvas--drift-adopt-row row (org-canvas--drift-ask-body row))))
    (org-canvas--drift-rewrite-whole-row (org-canvas--drift-outcome-line outcome))
    (message "%s" (string-trim (org-canvas--drift-outcome-line outcome)))
    outcome))

;;;; Adopting Rows in Batch

(defconst org-canvas--drift-adopt-buffer-name "*canvas-diff-adopt-drift*"
  "Name of the buffer holding the report of adopted drift.")

(defun org-canvas--drift-singular (name)
  "Return feature NAME normalized and singular, for matching any spelling."
  (org-canvas--singularize (org-canvas--diff-normalize-name (format "%s" name))))

(defun org-canvas--drift-named-p (entry target by)
  "Return non-nil when ENTRY is the heading TARGET names, BY title or id."
  (if (eq by 'canvas-id)
      (equal (plist-get entry :id) (format "%s" target))
    (equal (plist-get entry :title) target)))

(defun org-canvas--drift-override-of-p (entry target by id)
  "Return non-nil when override ENTRY belongs to the heading TARGET names.
BY is as for `org-canvas--drift-named-p'; ID is the heading's Canvas
id when its CHANGED row gave one."
  (and (org-canvas--drift-extra-override-p entry)
       (or (and id (equal (plist-get entry :assignment-id) id))
           (if (eq by 'canvas-id)
               (equal (plist-get entry :assignment-id) (format "%s" target))
             (equal (plist-get entry :where) target)))))

(defun org-canvas--drift-result (results name)
  "Return the result in RESULTS whose feature is NAME, in any spelling."
  (cl-find-if (lambda (r) (equal (org-canvas--drift-singular (plist-get r :name))
                                 (org-canvas--drift-singular name)))
              results))

(defun org-canvas--drift-override-rows (results pred)
  "Return the EXTRA override rows of RESULTS that PRED accepts, as rows."
  (mapcar (lambda (e) (list :feature org-canvas--diff-overrides-name :entry e))
          (cl-remove-if-not
           pred (plist-get (org-canvas--drift-result
                            results org-canvas--diff-overrides-name)
                           :extra))))

(defun org-canvas--drift-select-heading (results selector overrides)
  "Return the rows SELECTOR names in RESULTS, or a `not-found' outcome.
SELECTOR is (FEATURE TARGET BY); the row is the heading's CHANGED
row, and with OVERRIDES each EXTRA override row of its table."
  (pcase-let* ((`(,feature ,target ,by) selector)
               (result (org-canvas--drift-result results feature))
               (entry (cl-find-if (lambda (e) (and (eq (plist-get e :kind) 'modified)
                                                   (org-canvas--drift-named-p e target by)))
                                  (plist-get result :divergences)))
               (rows (append (and entry (list (list :feature (plist-get result :name)
                                                    :entry entry)))
                             (and overrides
                                  (org-canvas--drift-override-rows
                                   results (lambda (e) (org-canvas--drift-override-of-p
                                                        e target by (plist-get entry :id))))))))
    (or rows
        (list (list :feature (format "%s" feature) :title target :outcome 'not-found
                    :reason "the report has no CHANGED row for it")))))

(defun org-canvas--drift-select-all (results overrides)
  "Return every CHANGED row of RESULTS that a feature owns.
With OVERRIDES, every EXTRA override row as well."
  (append
   (cl-mapcan (lambda (r)
                (when (org-canvas--diff-find-feature (plist-get r :name))
                  (mapcar (lambda (e) (list :feature (plist-get r :name) :entry e))
                          (cl-remove-if-not
                           (lambda (e) (and (eq (plist-get e :kind) 'modified)
                                            (not (plist-get e :quiz-id))))
                           (plist-get r :divergences)))))
              results)
   (and overrides
        (org-canvas--drift-override-rows results #'org-canvas--drift-extra-override-p))))

(defun org-canvas--drift-adopt-one (row body)
  "Adopt ROW, or pass an outcome plist through; never signal.
BODY is as for `org-canvas--drift-adopt-row'."
  (if (not (plist-get row :entry))
      row
    (condition-case err
        (org-canvas--drift-adopt-row row body)
      (error
       (list :feature (plist-get row :feature)
             :title (plist-get (plist-get row :entry) :title)
             :outcome 'failed :reason (error-message-string err))))))

(defconst org-canvas--drift-done-outcomes '(adopted stamped appended dry-run)
  "The outcomes after which nothing the row named is left to do.")

(defun org-canvas--drift-adopt-render (outcomes)
  "Insert the report of adoption OUTCOMES at point."
  (insert "org-canvas Drift Adoption\n")
  (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
  (insert (make-string 60 ?=) "\n\n")
  (dolist (o outcomes)
    (insert (org-canvas--drift-outcome-line o) "\n"))
  (when outcomes (insert "\n"))
  (insert (make-string 60 ?=) "\n")
  (let ((done (cl-count-if (lambda (o) (memq (plist-get o :outcome)
                                             org-canvas--drift-done-outcomes))
                           outcomes)))
    (insert (format "%d row(s) adopted, %d left; nothing sent to Canvas.\n"
                    done (- (length outcomes) done)))))

;;;###autoload
(cl-defun org-canvas-diff-adopt-drifts (selectors &key body overrides results)
  "Write the Canvas side of the drift report rows in SELECTORS into Org.
SELECTORS is `all', every CHANGED row a feature owns, or a list of
headings, each (FEATURE . TITLE) or (FEATURE TARGET BY) as
`org-canvas-sync-headings' reads them, naming the heading's CHANGED
row.  With OVERRIDES, each EXTRA override row of the headings named
\(every one, with `all') is appended to its table too.  Each row is
adopted as `A' adopts it (`org-canvas-diff-adopt-drift'); a body is
replaced only with BODY non-nil.  Never asks.  The rows come from the
report's own comparison, RESULTS when given, else read afresh.  A dry
run writes nothing and names what would be written.  Nothing is sent
to Canvas.

Returns one plist per row or unmatched heading: :feature, :title,
:outcome (`adopted', `stamped', `partial', `appended', `dry-run',
`not-found', `refused' or `failed'), :adopted, :held, :updated and
:reason (issue #498)."
  (org-canvas--preflight-check)
  (run-hooks 'org-canvas--operation-start-hook)
  (let* ((results (or results (org-canvas--diff-collect-results)))
         (rows (if (eq selectors 'all)
                   (org-canvas--drift-select-all results overrides)
                 (cl-mapcan (lambda (s) (org-canvas--drift-select-heading
                                         results (org-canvas--sync-headings-entry s)
                                         overrides))
                            selectors)))
         (outcomes (mapcar (lambda (row) (org-canvas--drift-adopt-one row body)) rows)))
    (org-canvas--report-display org-canvas--drift-adopt-buffer-name
                                (lambda () (org-canvas--drift-adopt-render outcomes)))
    outcomes))

;;;###autoload
(defun org-canvas-diff-adopt-all-drifts ()
  "Write the Canvas side of every CHANGED row into Org, after confirming.
Runs the drift comparison, then adopts each CHANGED row a feature owns
the way `A' does (`org-canvas-diff-adopt-drifts'), bodies held back,
and reports what it wrote and what it held.  Nothing is sent to
Canvas.  Returns the outcomes."
  (interactive)
  (unless (or noninteractive
              (y-or-n-p "Write Canvas's side of every CHANGED row into Org (bodies held back)? "))
    (user-error "Aborted; nothing adopted"))
  (org-canvas-diff-adopt-drifts 'all))

;;;; Showing the Canvas Side (issue #500)

(defconst org-canvas--drift-show-buffer-name "*canvas-show*"
  "Name of the buffer holding the Canvas version of a heading.")

(defun org-canvas--drift-show-pair (name local remote differs)
  "Return the two lines naming field NAME: LOCAL in Org, REMOTE on Canvas.
DIFFERS marks the pair with a star."
  (format "%s %-20s org:    %s\n  %-20s canvas: %s\n"
          (if differs "*" " ") name (or local "(unset)") "" remote))

(defun org-canvas--drift-show-comparable-p (spec pom item)
  "Return non-nil when the report compares SPEC at POM against ITEM.
Its type is one the comparison understands, it is not org-canvas's
own bookkeeping, and its `:compare-p', when it has one, agrees."
  (let ((compare-p (plist-get spec :compare-p)))
    (and (memq (plist-get spec :type) org-canvas--diff-comparable-types)
         (not (plist-get spec :local-only))
         (or (null compare-p) (funcall compare-p pom item)))))

(defun org-canvas--drift-show-property (spec specs pom item)
  "Return the lines for SPEC at POM against Canvas ITEM, or nil.
SPECS is the feature's spec list.  A property the report does not
compare, or one silent on both sides, is left out."
  (when (org-canvas--drift-show-comparable-p spec pom item)
    (let* ((local (org-entry-get pom (plist-get spec :org-prop)))
           (intent (plist-get spec :intent-of))
           (observed (and intent (org-canvas--diff-observed-spec spec specs)))
           (remote-value (and (or observed (not intent))
                              (org-canvas--diff-remote-field (or observed spec) item)))
           (differs (if intent
                        (org-canvas--diff-compare-intent spec specs pom item)
                      (org-canvas--diff-compare-field spec pom item))))
      (when (and (or observed (not intent))
                 (or local differs (org-canvas--diff-normalize-remote remote-value)))
        (org-canvas--drift-show-pair
         (plist-get spec :org-prop) local
         (if intent
             (org-canvas--diff-intent-remote-text observed item)
           (org-canvas--diff-format-remote (plist-get spec :type) remote-value))
         differs)))))

(defun org-canvas--drift-show-properties (props pom item)
  "Return the lines for every property PROPS registers, at POM against ITEM."
  (let ((specs (plist-get props :properties)))
    (mapconcat #'identity
               (delq nil (mapcar (lambda (s) (org-canvas--drift-show-property
                                              s specs pom item))
                                 specs))
               "")))

(defun org-canvas--drift-show-title (feature entry item)
  "Return the lines naming the heading ENTRY and FEATURE's title on ITEM."
  (let ((local (org-canvas--diff-squeeze (plist-get entry :match-title)))
        (remote (org-canvas--diff-squeeze
                 (org-canvas--diff-normalize-remote
                  (alist-get (plist-get feature :title-field) item)))))
    (org-canvas--drift-show-pair "TITLE" local (or remote "(not reported)")
                                 (and remote (not (equal local remote))))))

(defun org-canvas--drift-local-body-org (props)
  "Return the Org text the heading at point sends as its body, given PROPS.
The text below the drawer, sub-headings included, unless the module
reads its body by a `:body-fn', which stops at the first child; its
overrides and accommodations tables are left out, as the push leaves
them out."
  (save-excursion
    (org-back-to-heading t)
    (let* ((bounds (org-canvas--pull-entry-text-bounds))
           (end (if (plist-get props :body-fn)
                    (cdr bounds)
                  (save-excursion (org-end-of-subtree t t) (point))))
           (text (buffer-substring-no-properties (car bounds) end)))
      (string-trim
       (replace-regexp-in-string
        "^#\\+NAME:[ \t]+\\(?:overrides\\|accommodations\\)[ \t]*\n\\(?:[ \t]*|.*\n?\\)*"
        "" text)))))

(defun org-canvas--drift-unified-diff (old new)
  "Return a unified diff of OLD, labelled org, against NEW, labelled canvas.
The texts go to temporary files outside the course for the diff
program; \"(no difference)\" when they agree."
  (cond
   ((string= old new) "(no difference)\n")
   ((not (executable-find "diff"))
    "(no diff program found; both texts are above)\n")
   (t
    (let ((a (make-temp-file "org-canvas-org-" nil ".org" (concat old "\n")))
          (b (make-temp-file "org-canvas-canvas-" nil ".org" (concat new "\n"))))
      (unwind-protect
          (with-temp-buffer
            (call-process "diff" nil t nil "-u" "--label" "org" "--label" "canvas" a b)
            (buffer-string))
        (delete-file a)
        (delete-file b))))))

(defun org-canvas--drift-show-body (props pom item)
  "Return the body section for the heading at POM against Canvas ITEM, or \"\".
PROPS names the body field; a feature without one, or an item that
does not carry it, has no section.  Canvas's HTML, the Org a pull
would convert it to (file links not rewritten, since that downloads),
and a unified diff of the heading's body against that Org."
  (let ((cell (and (plist-get props :body-api-key)
                   (org-canvas--diff-remote-body props item))))
    (if (null cell)
        ""
      (let* ((html (or (org-canvas--diff-normalize-remote (cdr cell)) ""))
             (compared (org-canvas--diff-compare-body props pom item))
             (canvas-org (if (string-empty-p html) "" (org-canvas--html-to-org html)))
             (local-org (org-canvas--drift-local-body-org props)))
        (concat
         (format "\n%s %s (%s)\n" (if (cdr compared) "*" " ")
                 (org-canvas--drift-body-label props)
                 (cond ((null compared) "not compared: the local body did not export")
                       ((cdr compared) "the text differs")
                       (t "the same text; any difference is markup")))
         "--- Canvas HTML ---\n" html "\n"
         "--- Canvas as Org (what a pull converts it to; file links not rewritten) ---\n"
         canvas-org "\n"
         "--- This heading's body against Canvas's, as Org ---\n"
         (org-canvas--drift-unified-diff local-org canvas-org))))))

(defun org-canvas--drift-show-head (feature entry item)
  "Return the first line, naming FEATURE's heading ENTRY and Canvas ITEM."
  (let ((baseline (org-canvas--conflict-baseline (plist-get entry :pom)))
        (updated (alist-get (org-canvas--feature-modified-field feature) item)))
    (format "%s '%s' (%s %s): the Canvas version, nothing written\n  Canvas updated %s; the heading's baseline is %s\n\n"
            (plist-get feature :name) (plist-get entry :title)
            (or (plist-get feature :id-property) "CANVAS_ID") (plist-get entry :id)
            (or (org-canvas--diff-normalize-remote updated) "(not reported)")
            (if baseline (format-time-string "%FT%TZ" baseline t) "(none)"))))

(defun org-canvas--drift-show-text (feature item)
  "Return the Canvas version of the heading at point as text, ITEM read.
FEATURE is its registry entry.  Reads and writes nothing more: the
local body is exported offline (Hard Rule 19).  Fields that differ
are starred."
  (let* ((entry (append (org-canvas--diff-local-entry
                         (or (plist-get feature :id-property) "CANVAS_ID"))
                        (list :pom (point-marker))))
         (props (org-canvas--drift-props feature))
         (pom (plist-get entry :pom)))
    (unwind-protect
        (concat (org-canvas--drift-show-head feature entry item)
                (org-canvas--drift-show-title feature entry item)
                (org-canvas--drift-show-properties props pom item)
                (org-canvas--drift-show-body props pom item))
      (set-marker pom nil))))

(defun org-canvas--drift-show-display (text)
  "Display TEXT in the show buffer, or print it in batch; return TEXT."
  (org-canvas--report-display org-canvas--drift-show-buffer-name
                              (lambda () (insert text)))
  text)

(defun org-canvas-diff-show-row ()
  "Show the Canvas version of the CHANGED row at point, in full.
Every compared property beside the Org value, the title, and the body
as Canvas's HTML, as Org, and as a unified diff against the heading's
own, from the copy of the item the report already read (issue #500).
Writes nothing and sends nothing.  Returns the text shown."
  (interactive)
  (let* ((row (org-canvas--diff-row-at-point))
         (entry (plist-get row :entry)))
    (unless (and (eq (plist-get entry :kind) 'modified) (plist-get entry :item))
      (user-error "Only a CHANGED row of a feature carries a Canvas version to show"))
    (let* ((feature (org-canvas--diff-row-feature row))
           (where (or (org-canvas--diff-heading-position feature entry)
                      (user-error "Cannot find the heading for '%s'"
                                  (plist-get entry :title)))))
      (org-canvas--drift-show-display
       (org-canvas--drift-at where (lambda () (org-canvas--drift-show-text
                                               feature (plist-get entry :item))))))))

(defun org-canvas--drift-show-here ()
  "Return the Canvas version of the heading at point as text.
The item is read on its own (`org-canvas--pull-item-read'); nothing is
written.  A heading Canvas deleted says so."
  (save-excursion
    (pcase-let ((`(,feature ,id ,title) (org-canvas--pull-at-point-target)))
      (let ((item (org-canvas--pull-item-read feature id)))
        (if (eq item 'gone)
            (format "%s '%s' (id %s) is not on Canvas any more; nothing to show.\n"
                    (plist-get feature :name) title id)
          (org-canvas--drift-show-text feature item))))))

;;;###autoload
(defun org-canvas-show-at-point ()
  "Show the Canvas version of the heading at point, writing nothing.
Reads the item from Canvas and shows every compared property beside
the Org value, the title, and the body as Canvas's HTML, as the Org a
pull would convert it to, and as a unified diff against the heading's
own (issue #500).  Unlike `org-canvas-pull-at-point' it changes
nothing, so the Canvas text can be read, and copied, before deciding.
Returns the text shown."
  (interactive)
  (org-canvas--drift-show-display (org-canvas--drift-show-here)))

(defun org-canvas--drift-feature-named (name)
  "Return the drift report's feature entry NAME means, in any spelling, or signal."
  (or (cl-find-if (lambda (f) (equal (org-canvas--drift-singular (plist-get f :name))
                                     (org-canvas--drift-singular name)))
                  (org-canvas--diff-features))
      (user-error "No feature %s; the features are: %s" name
                  (mapconcat (lambda (f) (plist-get f :name))
                             (org-canvas--diff-features) ", "))))

(defun org-canvas--drift-show-heading (feature target by)
  "Return the Canvas version of FEATURE's heading TARGET names, BY title or id."
  (let* ((var (plist-get feature :file-var))
         (file (and var (boundp var) (symbol-value var)))
         (query (or (plist-get (org-canvas--drift-props feature) :query) "LEVEL=1"))
         (marker (org-canvas--sync-find-heading
                  file query target by
                  (or (plist-get feature :id-property) "CANVAS_ID")
                  (plist-get feature :name) "show")))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (set-marker marker nil)
        (org-canvas--drift-show-here)))))

(defun org-canvas--drift-show-one (feature entry)
  "Return the text for heading ENTRY, (FEATURE-NAME TARGET BY), of FEATURE.
A failure is its own line, so the next heading is still shown."
  (condition-case err
      (org-canvas--drift-show-heading feature (nth 1 entry) (nth 2 entry))
    (error (org-canvas--log-redact
            (format "%s '%s': %s\n" (nth 0 entry) (nth 1 entry)
                    (error-message-string err))))))

;;;###autoload
(defun org-canvas-show-headings (entries)
  "Show the Canvas version of each heading ENTRIES names, writing nothing.
ENTRIES are as `org-canvas-pull-headings' takes them: (FEATURE . TITLE)
or (FEATURE TARGET BY).  Every feature is resolved first, so a
misspelt one stops before anything is read.  The text is displayed,
or printed in batch (`scripts/org-canvas show').  Returns it."
  (org-canvas--preflight-check)
  (let* ((parsed (mapcar #'org-canvas--sync-headings-entry entries))
         (features (mapcar (lambda (e) (org-canvas--drift-feature-named (car e))) parsed)))
    (org-canvas--drift-show-display
     (mapconcat #'identity (cl-mapcar #'org-canvas--drift-show-one features parsed)
                (concat "\n" (make-string 60 ?=) "\n\n")))))

;;;; Keys

(define-key org-canvas-diff-mode-map (kbd "A") #'org-canvas-diff-adopt-drift)
(define-key org-canvas-diff-mode-map (kbd "=") #'org-canvas-diff-show-row)

(provide 'org-canvas-drift)
;;; org-canvas-drift.el ends here
