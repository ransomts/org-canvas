;;; org-canvas-grading-schemes.el --- Grading schemes: pull, create, validate -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module syncs grading schemes (Canvas calls them grading
;; standards): the letter-grade cutoffs a course, or an assignment,
;; names by GRADING_STANDARD_ID.  A course copied to a new term keeps
;; its cutoffs only if someone rebuilds them, which is what this file
;; is for: the scheme lives in Org, is created on Canvas from there,
;; and the id it gets is what settings.org and assignments.org name.
;;
;; FILE STRUCTURE
;; ==============
;; In grading-schemes.org:
;;   - Level 1 headings = grading schemes, titled as on Canvas
;;   - Body = one `#+NAME: scheme' table, | Grade | Cutoff |, the
;;     cutoff a percent (94, not 0.94) that is the minimum for that
;;     grade, rows descending, the last row 0
;;
;; PROPERTIES
;; ==========
;; CANVAS_ID      - Canvas grading standard id (set by sync or pull)
;; CONTEXT        - course or account (set by pull; an account scheme
;;                  is listed for reference and never pushed)
;; POINTS_BASED   - "true" for a points-based scheme (default false)
;; SCALING_FACTOR - what a points-based scheme is out of (4.0, say);
;;                  the cutoffs stay percents of it
;;
;; API NOTES
;; =========
;;   GET    /courses/:id/grading_standards       available in the course:
;;                                              course-level and account-level
;;   POST   /courses/:id/grading_standards       title, grading_scheme_entry
;;                                              (name and value, the value a
;;                                              percent), points_based,
;;                                              scaling_factor
;;   GET    /courses/:id/grading_standards/:id
;;   DELETE /courses/:id/grading_standards/:id   course-level only
;; There is no update: a scheme with a CANVAS_ID whose table differs
;; from Canvas's is skipped with a warning that says what to do (delete
;; it, clear CANVAS_ID, sync again, re-point GRADING_STANDARD_ID).
;; Canvas answers `grading_scheme' values as fractions (0.94); the
;; create request takes percents (94), which is also what the table
;; holds.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(declare-function org-canvas--validate-grading-scheme-structure "org-canvas-validate")
(declare-function org-canvas--validate-grading-standard-ids "org-canvas-validate")

;;;; Configuration

(defcustom org-canvas-grading-schemes-file (org-canvas--path "grading-schemes.org")
  "Path to the grading-schemes.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-grading-schemes-file "grading-schemes.org")
(org-canvas-register-feature
 :name "Grading Schemes" :endpoint "grading_standards"
 :file-var 'org-canvas-grading-schemes-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'title)

(defun org-canvas--grading-scheme-remote-context (item)
  "Return where the grading standard ITEM lives: \"course\" or \"account\".
Canvas spells it `context_type', capitalised."
  (let ((type (alist-get 'context_type item)))
    (and (stringp type) (downcase type))))

(defun org-canvas--grading-scheme-remote-scaling-factor (item)
  "Return ITEM's scaling factor when it is points-based, else nil.
A percentage-based scheme carries a factor of 1 that means nothing,
so it is not written."
  (and (eq (alist-get 'points_based item) t)
       (alist-get 'scaling_factor item)))

(org-canvas-register-properties "grading-schemes"
  :label "Grading Schemes"
  :file-var 'org-canvas-grading-schemes-file
  :query "LEVEL=1"
  :structural-fn #'org-canvas--validate-grading-scheme-structure
  :file-fn #'org-canvas--validate-grading-standard-ids
  :properties
  '((:org-prop "CONTEXT" :data-key :context :type enum
     :values ("course" "account") :pull-only t
     :remote-fn org-canvas--grading-scheme-remote-context
     :doc "Where the scheme lives: course, or account (never pushed)")
    (:org-prop "POINTS_BASED" :data-key :points_based :type boolean
     :api-key "points_based" :boolean-json t
     :doc "Points-based scheme, out of SCALING_FACTOR (default: percentage-based)")
    (:org-prop "SCALING_FACTOR" :data-key :scaling_factor :type number
     :api-key "scaling_factor"
     :remote-fn org-canvas--grading-scheme-remote-scaling-factor
     :doc "What a points-based scheme is out of, e.g. 4.0; only with POINTS_BASED")))

;;;; The Scheme Table

(defconst org-canvas--grading-scheme-table-name "scheme"
  "The `#+NAME:' of the cutoff table under a grading scheme heading.")

(defun org-canvas--grading-scheme-find-table (end)
  "Return the `#+NAME: scheme' table between point and END, or nil.
The table comes back as `org-table-to-lisp' gives it."
  (save-excursion
    (when (re-search-forward
           (format "^#\\+NAME:[ \t]+%s[ \t]*$" org-canvas--grading-scheme-table-name)
           end t)
      (forward-line 1)
      (when (looking-at-p org-table-line-regexp)
        (org-table-to-lisp)))))

(defun org-canvas--grading-scheme-cutoff-number (cell)
  "Return the cutoff in the table CELL as a number, or nil.
A trailing percent sign is allowed; anything else that is not a
number is nil."
  (let ((text (string-trim (replace-regexp-in-string "%\\'" "" (string-trim cell)))))
    (and (string-match-p "\\`-?[0-9]*\\.?[0-9]+\\'" text)
         (string-to-number text))))

(defun org-canvas--grading-scheme-table-rows (table)
  "Return the grade rows of TABLE as (GRADE . CUTOFF-CELL) pairs.
Horizontal lines are dropped, and so is a first row whose cutoff cell
is not a number: the header.  A row with one cell is skipped."
  (let ((rows (cl-remove-if (lambda (row) (or (eq row 'hline) (null (cdr row)))) table)))
    (when (and rows (not (org-canvas--grading-scheme-cutoff-number (nth 1 (car rows)))))
      (setq rows (cdr rows)))
    (mapcar (lambda (row) (cons (string-trim (nth 0 row)) (nth 1 row))) rows)))

(defun org-canvas--grading-scheme-parse-table (table title)
  "Return TABLE's grades as a list of (NAME . PERCENT), or signal.
TITLE names the scheme in the error.  Nil TABLE means the heading has
no `#+NAME: scheme' table, which is an error too: a scheme is its
cutoffs."
  (unless table
    (org-canvas--signal 'org-canvas-validation-error
      "Grading scheme '%s' has no #+NAME: scheme table" title))
  (let ((entries
         (mapcar (lambda (row)
                   (let ((cutoff (org-canvas--grading-scheme-cutoff-number (cdr row))))
                     (unless cutoff
                       (org-canvas--signal 'org-canvas-validation-error
                         "Grading scheme '%s': cutoff for '%s' is not a number: %S"
                         title (car row) (string-trim (cdr row))))
                     (cons (car row) cutoff)))
                 (org-canvas--grading-scheme-table-rows table))))
    (unless entries
      (org-canvas--signal 'org-canvas-validation-error
        "Grading scheme '%s' has an empty scheme table" title))
    entries))

(defun org-canvas--grading-scheme-format-cutoff (fraction)
  "Return the Canvas FRACTION (0.94) as the percent of the table (94).
Whole percents are written without a decimal point."
  (let* ((percent (* (float fraction) 100))
         (rounded (/ (fround (* percent 100)) 100)))
    (if (= rounded (ftruncate rounded))
        (format "%d" (truncate rounded))
      (format "%s" rounded))))

(defun org-canvas--grading-scheme-remote-entries (item)
  "Return the grading standard ITEM's entries as (NAME . PERCENT)."
  (mapcar (lambda (entry)
            (cons (alist-get 'name entry)
                  (* (float (alist-get 'value entry)) 100)))
          (append (alist-get 'grading_scheme item) nil)))

(defun org-canvas--grading-scheme-insert-table (entries)
  "Insert the `#+NAME: scheme' table for ENTRIES ((NAME . PERCENT)...) at point."
  (insert (format "#+NAME: %s\n" org-canvas--grading-scheme-table-name))
  (insert "| Grade | Cutoff |\n|-------+--------|\n")
  (dolist (entry entries)
    (insert (format "| %s | %s |\n" (car entry)
                    (org-canvas--grading-scheme-format-cutoff (/ (cdr entry) 100.0)))))
  (save-excursion
    (forward-line -1)
    (org-table-align)))

;;;; 1. Stage: Extraction

(defun org-canvas--grading-scheme-parse-entry ()
  "Extract the grading scheme at point: its properties and its table."
  (org-back-to-heading t)
  (let* ((pom (point))
         (title (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
         (end (save-excursion (org-end-of-subtree t) (point)))
         (factor (org-entry-get pom "SCALING_FACTOR")))
    (org-canvas--require-title title pom "Grading scheme")
    (org-canvas--log-info org-canvas--logger
      "[Stage 1: Parse] Processing grading scheme: '%s' (ID: %s)"
      title (or (org-entry-get pom "CANVAS_ID") "NEW"))
    (list :title title
          :canvas-id (org-entry-get pom "CANVAS_ID")
          :context (org-entry-get pom "CONTEXT")
          :points_based (org-canvas--interpret-boolean (org-entry-get pom "POINTS_BASED"))
          :scaling_factor (and factor (org-canvas--safe-string-to-number factor "SCALING_FACTOR"))
          :scheme (org-canvas--grading-scheme-parse-table
                   (org-canvas--grading-scheme-find-table end) title)
          :pom pom)))

;;;; 2. Stage: Transformation

(defun org-canvas--grading-scheme-post-build (data payload)
  "Add the cutoffs to PAYLOAD from DATA, and drop a factor with no scheme to scale.
The entries travel as `grading_scheme_entry', a list of name and
value, the value a percent."
  (unless (plist-get data :points_based)
    (setq payload (assq-delete-all 'scaling_factor payload)))
  (append payload
          (list (cons 'grading_scheme_entry
                      (vconcat (mapcar (lambda (entry)
                                         (list (cons 'name (car entry))
                                               (cons 'value (cdr entry))))
                                       (plist-get data :scheme)))))))

(org-canvas-define-payload grading-scheme
  :registry-key "grading-schemes"
  :format alist
  :title-key :title
  :title-api-key title
  :post-build-fn #'org-canvas--grading-scheme-post-build)

;;;; 3. Stage: Execution

(defun org-canvas--grading-scheme-find (title)
  "Return the grading standard titled TITLE available in the course, or nil."
  (org-canvas--search-item "grading_standards" title
                           :params '(("per_page" . "100"))))

(defun org-canvas--grading-scheme-remote (canvas-id ctx)
  "Return the grading standard CANVAS-ID from Canvas, or nil when it has none.
Inside a run CTX's snapshot answers without a request; a push at
point asks with one GET, where a 404 is nil."
  (let ((titles (plist-get ctx :remote-titles))
        (found nil))
    (if (hash-table-p titles)
        (progn
          (maphash (lambda (_title items)
                     (dolist (item items)
                       (when (equal (format "%s" (alist-get 'id item))
                                    (format "%s" canvas-id))
                         (setq found item))))
                   titles)
          found)
      (condition-case err
          (org-canvas-api-request
           'GET (org-canvas-api-course-endpoint "grading_standards/%s" canvas-id))
        (error (if (org-canvas--404-error-p err)
                   nil
                 (signal (car err) (cdr err))))))))

(defun org-canvas--grading-scheme-same-p (data remote)
  "Return non-nil when the parsed scheme DATA matches the Canvas standard REMOTE.
Title, the cutoffs (to a hundredth of a percent), the points-based
flag and, for a points-based scheme, the scaling factor."
  (let ((local (plist-get data :scheme))
        (theirs (org-canvas--grading-scheme-remote-entries remote))
        (points (and (plist-get data :points_based) t)))
    (and (equal (plist-get data :title) (alist-get 'title remote))
         (eq points (eq (alist-get 'points_based remote) t))
         (or (not points)
             (equal (float (or (plist-get data :scaling_factor) 1))
                    (float (or (alist-get 'scaling_factor remote) 1))))
         (= (length local) (length theirs))
         (cl-every (lambda (mine yours)
                     (and (equal (car mine) (car yours))
                          (< (abs (- (float (cdr mine)) (cdr yours))) 0.005)))
                   local theirs))))

(defun org-canvas--grading-scheme-stamp-hash (data payload ctx)
  "Record PAYLOAD's hash on DATA's heading so the next sync skips it unasked.
CTX supplies the hash owner.  Nothing is written under a dry run."
  (unless org-canvas--dry-run
    (let ((hash (org-canvas--sync-entry-hash payload data ctx)))
      (when hash
        (org-canvas-org-set-property (plist-get data :pom)
                                     org-canvas--prop-payload-hash hash)
        (org-canvas--save-buffer)))))

(defun org-canvas--grading-scheme-note-skip (ctx title why)
  "Name TITLE among the run's skipped entries in CTX, with WHY."
  (let ((counters (plist-get ctx :counters)))
    (when counters
      (plist-put counters :skipped-titles
                 (cons (format "%s (%s)" title why)
                       (plist-get counters :skipped-titles))))))

(defun org-canvas--grading-scheme-push-existing (data payload remote ctx)
  "Settle the push of DATA, stamped with an id, against Canvas's REMOTE.
The same scheme is a skip, with PAYLOAD's hash stamped so the next run
does not ask again.  A different one is a skip with a warning: Canvas
cannot edit a scheme in place.  CTX is the run context."
  (let ((title (plist-get data :title)))
    (if (org-canvas--grading-scheme-same-p data remote)
        (progn
          (org-canvas--log-info org-canvas--logger
            "[Skip] '%s' matches Canvas (id %s)" title (alist-get 'id remote))
          (org-canvas--grading-scheme-stamp-hash data payload ctx))
      (org-canvas--log-warning org-canvas--logger
        "[Skip] '%s' differs from Canvas (id %s), and Canvas cannot edit a grading scheme in place: delete it there (org-canvas-delete-grading-scheme-at-point, or the web UI), clear CANVAS_ID, sync again to create it afresh, then point GRADING_STANDARD_ID in settings.org or assignments.org at the new id"
        title (alist-get 'id remote))
      (org-canvas--grading-scheme-note-skip
       ctx title "differs from Canvas, which cannot edit a scheme in place"))
    'skip))

(defun org-canvas--grading-scheme-push-create (data payload ctx)
  "Create DATA's scheme on Canvas from PAYLOAD, unless it is already there.
A scheme of the same title and cutoffs is adopted: its item is
returned as the response, so finalize stamps its id (Hard Rule 20).
One of the same title and other cutoffs is a `duplicate'.  Neither is
looked for when `org-canvas-duplicate-title-strategy' is `create'.  A
dry run answers the dry-run sentinel.  CTX is the run context."
  (let* ((title (plist-get data :title))
         (twins (unless (eq org-canvas-duplicate-title-strategy 'create)
                  (org-canvas--push-remote-items-titled
                   title #'org-canvas--grading-scheme-find ctx)))
         (same (cl-find-if (lambda (item) (org-canvas--grading-scheme-same-p data item))
                           twins))
         (url (org-canvas-api-course-endpoint "grading_standards")))
    (cond
     (same
      (org-canvas--log-info org-canvas--logger
        "[Duplicate] Canvas already holds '%s' as id %s with the same cutoffs; adopting it instead of creating a second"
        title (alist-get 'id same))
      (if org-canvas--dry-run org-canvas--dry-run-response same))
     (twins
      (org-canvas--log-warning org-canvas--logger
        "[Duplicate] Skipping '%s' — Canvas already holds a scheme of that title as id %s with other cutoffs; rename the heading, or adopt it with M-x org-canvas-adopt-at-point and take its cutoffs on the next pull"
        title (mapconcat (lambda (item) (format "%s" (alist-get 'id item))) twins ", "))
      'duplicate)
     (org-canvas--dry-run
      (org-canvas--log-info org-canvas--logger "[DRY-RUN] Would POST '%s' to %s" title url)
      org-canvas--dry-run-response)
     (t
      (org-canvas--log-info org-canvas--logger "[Execute] POST '%s' to %s" title url)
      (org-canvas-api-request 'POST url :data payload)))))

(defun org-canvas--grading-scheme-push (data payload &optional ctx)
  "Push the grading scheme DATA as PAYLOAD; CTX is the run context.
An account scheme is never pushed.  A scheme with a CANVAS_ID is
compared with Canvas's and skipped either way, since there is no
update; one whose id Canvas no longer has is created afresh.  A new
scheme is created, unless Canvas already holds one of its title."
  (let ((title (plist-get data :title))
        (id (plist-get data :canvas-id)))
    (cond
     ((equal (plist-get data :context) "account")
      (org-canvas--log-info org-canvas--logger
        "[Skip] '%s' belongs to the account; only course schemes are pushed" title)
      'skip)
     (id
      (let ((remote (org-canvas--grading-scheme-remote id ctx)))
        (if remote
            (org-canvas--grading-scheme-push-existing data payload remote ctx)
          (org-canvas--log-warning org-canvas--logger
            "[Execute] '%s' carries CANVAS_ID %s but Canvas has no such scheme; creating it afresh"
            title id)
          (org-canvas--grading-scheme-push-create data payload ctx))))
     (t
      (org-canvas--grading-scheme-push-create data payload ctx)))))

;;;; Main Sync Function

(org-canvas-define-sync grading-schemes
  :file org-canvas-grading-schemes-file
  :parse #'org-canvas--grading-scheme-parse-entry
  :build #'org-canvas--grading-scheme-build-payload
  :push #'org-canvas--grading-scheme-push
  :endpoint "grading_standards"
  :dry-run 'push
  :pull-item-fn #'org-canvas--grading-scheme-pull-item)

(org-canvas-define-delete-at-point grading-scheme
  :endpoint "grading_standards/%s")

;;;; Pull

(defun org-canvas--grading-scheme-write-table (item pos)
  "Replace the body of the heading at POS with ITEM's scheme table."
  (org-with-point-at pos
    (org-back-to-heading t)
    (let* ((meta-end (save-excursion (org-end-of-meta-data t) (point)))
           (body-end (save-excursion (org-end-of-subtree t t) (point)))
           (body-start (save-excursion
                         (goto-char (min meta-end body-end))
                         (skip-chars-backward " \t\n")
                         (point))))
      (delete-region body-start body-end)
      (goto-char body-start)
      (insert "\n\n")
      (org-canvas--grading-scheme-insert-table
       (org-canvas--grading-scheme-remote-entries item))
      (insert "\n"))))

(org-canvas-define-pull-item grading-scheme
  :registry-key "grading-schemes"
  :after-pull #'org-canvas--grading-scheme-write-table)

(org-canvas-define-pull grading-schemes
  :file org-canvas-grading-schemes-file
  :endpoint "grading_standards"
  :pull-item-fn #'org-canvas--grading-scheme-pull-item)

(provide 'org-canvas-grading-schemes)
;;; org-canvas-grading-schemes.el ends here
