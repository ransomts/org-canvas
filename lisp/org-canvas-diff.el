;;; org-canvas-diff.el --- Read-only drift report against Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Answers the question `org-canvas-status' sounds like it answers but
;; does not: does Canvas currently match my Org files?
;;
;; `org-canvas-status' is entirely local — it reports what the Org files
;; say about themselves.  `org-canvas-sync-dry-run' answers a third
;; question, which entries have a locally changed PAYLOAD_HASH.  Neither
;; can see a change made only in the Canvas web UI, so before this the
;; only ways to find remote drift were to run a real sync and read the
;; conflict prompts, or to query the API by hand and diff (issue #51).
;;
;; WHAT IT REPORTS
;; ===============
;; Per feature, three kinds of divergence:
;;
;;   Modified remotely  the item's `updated_at' is newer than the
;;                      baseline recorded at the last push or pull
;;   Missing on Canvas  a heading carries a Canvas id that the course
;;                      no longer has
;;   Only on Canvas     an item in the course that no Org heading claims
;;
;; plus, for every property the module registers, the differing values
;; themselves — published, points, due dates, group weights and so on.
;;
;; SCOPE OF THE FIELD COMPARISON
;; =============================
;; Only properties actually written in the Org file are compared.  An
;; absent property means the file expresses no opinion, and each module
;; applies its own default at parse time, so treating absence as a value
;; would invent differences that no sync would act on.
;;
;; COST AND SAFETY
;; ===============
;; One list request per feature.  Nothing is written, locally or
;; remotely, so this is safe to run at any time — from a hook, or from a
;; scheduled job via `org-canvas-diff-batch', which exits non-zero when
;; it finds drift.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

(defconst org-canvas--diff-buffer-name "*canvas-diff*"
  "Name of the buffer holding the drift report.")

(defconst org-canvas--diff-comparable-types
  '(boolean number timestamp enum csv-enum string)
  "Property types the field comparison understands.
`link' is excluded: the Org side is a heading link and the Canvas side
an id, so comparing them would need the same resolution a sync does.")

;;;; Registry Lookup

(defun org-canvas--diff-find-properties (feature-name)
  "Return the property registry entry for FEATURE-NAME, or nil.
Matches the way `org-canvas--registry-find-feature' does, so the
feature registry's \"Assignment Groups\" finds the property registry's
\"assignment-groups\"."
  (let ((norm (lambda (s) (downcase (replace-regexp-in-string "[ _-]" "" s))))
        found)
    (maphash (lambda (key plist)
               (when (and (not found)
                          (string= (funcall norm key)
                                   (funcall norm feature-name)))
                 (setq found plist)))
             org-canvas--property-registry)
    found))

;;;; Value Comparison

(defun org-canvas--diff-remote-field (spec item)
  "Return ITEM's value for the Canvas field described by SPEC.
A spec may name a `:remote-fn' of one argument, the Canvas item, for a
property the payload does not hold under a flat key of its own: a
file's publish state lives in `locked' (issue #61) and a group's drop
rules under `rules' (issue #62), so a flat lookup silently returned
nil and reported every such item as drifted.  Otherwise uses the
spec's `:api-key' when it declares one, and failing that the
`:data-key', which every module already names after the Canvas field."
  (let ((remote-fn (plist-get spec :remote-fn)))
    (if remote-fn
        (funcall remote-fn item)
      (let ((key (or (plist-get spec :api-key)
                     (substring (symbol-name (plist-get spec :data-key)) 1))))
        (alist-get (intern key) item)))))

(defun org-canvas--diff-normalize-remote (value)
  "Return VALUE with Canvas's JSON null and false spellings folded to nil."
  (if (memq value '(:json-false :null)) nil value))

(defun org-canvas--diff-remote-list (remote)
  "Return REMOTE as a list of strings, however Canvas spelled it.
A `csv-enum' field arrives either as a JSON array or as one comma
separated string — pages return `editing_roles' as \"teachers\" —
and `append' on a string yields character codes, which is how
\"teachers\" came to be reported as 116,101,97,... (issue #63)."
  (cond ((null remote) nil)
        ((stringp remote) (split-string remote "," t "[ \t]+"))
        (t (mapcar (lambda (v) (format "%s" v)) (append remote nil)))))

(defun org-canvas--diff-values-equal-p (type local remote)
  "Return non-nil when LOCAL (an Org string) and REMOTE agree, given TYPE."
  (let ((remote (org-canvas--diff-normalize-remote remote)))
    (pcase type
      ('boolean (eq (string= local "true") (and remote t)))
      ('number (and remote (= (string-to-number local)
                              (if (stringp remote) (string-to-number remote) remote))))
      ('timestamp
       (let ((local-iso (org-canvas-org-parse-timestamp local)))
         (and local-iso remote
              (equal (org-canvas--parse-iso8601-time local-iso)
                     (org-canvas--parse-iso8601-time remote)))))
      ('csv-enum
       (equal (sort (split-string (or local "") "," t "[ \t]+") #'string<)
              (sort (org-canvas--diff-remote-list remote) #'string<)))
      (_ (equal local (and remote (format "%s" remote)))))))

(defun org-canvas--diff-format-remote (type remote)
  "Return a printable form of REMOTE for TYPE."
  (let ((remote (org-canvas--diff-normalize-remote remote)))
    (pcase type
      ('boolean (if remote "true" "false"))
      ('csv-enum (mapconcat #'identity (org-canvas--diff-remote-list remote) ","))
      (_ (if remote (format "%s" remote) "(unset)")))))

(defun org-canvas--diff-compare-fields (specs pom item)
  "Compare the properties at POM against Canvas ITEM using SPECS.
Returns a list of (ORG-PROP LOCAL REMOTE) for the ones that differ.
Only properties actually present in the Org file are considered — see
the commentary on why absence is not a value."
  (let (diffs)
    (dolist (spec specs)
      (let ((type (plist-get spec :type))
            (org-prop (plist-get spec :org-prop)))
        (when (memq type org-canvas--diff-comparable-types)
          (let ((local (org-entry-get pom org-prop)))
            (when (and local (not (string-empty-p local)))
              (let ((remote (org-canvas--diff-remote-field spec item)))
                (unless (org-canvas--diff-values-equal-p type local remote)
                  (push (list org-prop local
                              (org-canvas--diff-format-remote type remote))
                        diffs))))))))
    (nreverse diffs)))

;;;; Per-Feature Comparison

(defun org-canvas--diff-collect-local (file query id-property)
  "Collect entries from FILE matching QUERY, keyed by ID-PROPERTY.
Returns a list of plists (:id ID :title TITLE :pom MARKER).  The
position is a marker, not an offset: the comparison reads properties
long after this buffer stops being current, and a bare offset would
silently be read against whatever buffer happened to be."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
         (list :id (org-entry-get (point) id-property)
               :title (org-get-heading t t t t)
               :pom (point-marker)))
       (or query "LEVEL=1") 'file))))

(defun org-canvas--diff-remote-index (items id-field)
  "Return a hash of ITEMS keyed by their ID-FIELD, as strings."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((id (alist-get id-field item)))
        (when id (puthash (format "%s" id) item index))))
    index))

(defun org-canvas--diff-entry (entry index specs)
  "Compare local ENTRY against the remote INDEX using property SPECS.
Returns a plist describing the divergence, or nil when they agree."
  (let* ((id (plist-get entry :id))
         (pom (plist-get entry :pom))
         (item (and id (gethash id index))))
    (cond
     ((null id) nil)
     ((null item)
      (list :kind 'missing :title (plist-get entry :title) :id id))
     (t
      (let ((fields (org-canvas--diff-compare-fields specs pom item))
            (drifted (org-canvas--diff-modified-p pom item)))
        (when (or fields drifted)
          (list :kind 'modified :title (plist-get entry :title) :id id
                :updated (alist-get 'updated_at item)
                :remote-newer drifted :fields fields)))))))

(defun org-canvas--diff-modified-p (pom item)
  "Return non-nil when ITEM's `updated_at' is newer than POM's baseline."
  (let ((baseline (org-canvas--conflict-baseline pom))
        (remote (org-canvas--parse-iso8601-time (alist-get 'updated_at item))))
    (and baseline remote (time-less-p baseline remote))))

(defun org-canvas--diff-feature (feature)
  "Compare one FEATURE registry entry against Canvas.
Returns a plist (:name :divergences :extra :error), where :extra holds
remote items no Org heading claims and :error a message when the list
request failed."
  (let* ((name (plist-get feature :name))
         (file-var (plist-get feature :file-var))
         (file (and (boundp file-var) (symbol-value file-var)))
         (id-property (plist-get feature :id-property))
         (id-field (plist-get feature :id-field))
         (title-field (plist-get feature :title-field))
         (props (org-canvas--diff-find-properties name))
         (specs (plist-get props :properties))
         (skip-fn (plist-get feature :skip-fn)))
    (condition-case err
        (let* ((items (append (org-canvas-api-request-all-pages
                               'GET (org-canvas-api-course-endpoint
                                     (plist-get feature :endpoint))
                               (plist-get feature :list-params))
                              nil))
               (index (org-canvas--diff-remote-index items id-field))
               (local (org-canvas--diff-collect-local
                       file (plist-get props :query) id-property))
               (claimed (delq nil (mapcar (lambda (e) (plist-get e :id)) local)))
               divergences extra)
          (dolist (entry local)
            (let ((d (org-canvas--diff-entry entry index specs)))
              (when d (push d divergences)))
            (let ((m (plist-get entry :pom)))
              (when (markerp m) (set-marker m nil))))
          (dolist (item items)
            (unless (or (and skip-fn (funcall skip-fn item))
                        (member (format "%s" (alist-get id-field item)) claimed))
              (push (list :kind 'extra
                          :title (format "%s" (or (alist-get title-field item) "?"))
                          :id (format "%s" (alist-get id-field item)))
                    extra)))
          (list :name name
                :divergences (nreverse divergences)
                :extra (nreverse extra)))
      (error
       (list :name name :error (error-message-string err))))))

;;;; Report

(defun org-canvas--diff-insert-entry (entry)
  "Insert one ENTRY of a drift report at point."
  (pcase (plist-get entry :kind)
    ('missing
     (insert (format "  MISSING   %s (id %s is not in this course)\n"
                     (plist-get entry :title) (plist-get entry :id))))
    ('extra
     (insert (format "  EXTRA     %s (id %s, no Org heading claims it)\n"
                     (plist-get entry :title) (plist-get entry :id))))
    ('modified
     (insert (format "  CHANGED   %s%s\n"
                     (plist-get entry :title)
                     (if (plist-get entry :remote-newer)
                         (format " (Canvas updated %s)" (plist-get entry :updated))
                       "")))
     (dolist (field (plist-get entry :fields))
       (insert (format "              %-18s org: %-24s canvas: %s\n"
                       (nth 0 field) (nth 1 field) (nth 2 field)))))))

(defun org-canvas--diff-count (results)
  "Return the total number of divergences across RESULTS."
  (apply #'+ (mapcar (lambda (r) (+ (length (plist-get r :divergences))
                                    (length (plist-get r :extra))))
                     results)))

(defun org-canvas--diff-render (results)
  "Render RESULTS into a report string."
  (with-temp-buffer
    (insert "org-canvas Drift Report\n")
    (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
    (insert (make-string 60 ?=) "\n\n")
    (dolist (result results)
      (let ((divergences (plist-get result :divergences))
            (extra (plist-get result :extra))
            (err (plist-get result :error)))
        (cond
         (err
          (insert (format "%s: could not check (%s)\n\n"
                          (plist-get result :name) err)))
         ((and (null divergences) (null extra)))
         (t
          (insert (format "%s: %d divergence(s)\n"
                          (plist-get result :name)
                          (+ (length divergences) (length extra))))
          (dolist (entry (append divergences extra))
            (org-canvas--diff-insert-entry entry))
          (insert "\n")))))
    (let ((total (org-canvas--diff-count results)))
      (insert (make-string 60 ?=) "\n")
      (insert (if (zerop total)
                  "No drift: Canvas matches the Org files.\n"
                (format "%d divergence(s) found.\n" total))))
    (buffer-string)))

;;;; Commands

;;;###autoload
(defun org-canvas-diff ()
  "Report how Canvas differs from the Org files, without changing anything.
Makes one read-only list request per feature and compares ids, remote
modification times and the properties each module registers.  Returns
the number of divergences found, so a batch caller can act on it; see
`org-canvas-diff-batch'."
  (interactive)
  (org-canvas--preflight-check)
  (let (results)
    (dolist (feature org-canvas--feature-registry)
      (message "Drift: checking %s..." (plist-get feature :name))
      (push (org-canvas--diff-feature feature) results))
    (setq results (nreverse results))
    (let ((report (org-canvas--diff-render results))
          (total (org-canvas--diff-count results)))
      (if noninteractive
          (princ report)
        (with-current-buffer (get-buffer-create org-canvas--diff-buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert report)
            (goto-char (point-min)))
          (special-mode)
          (display-buffer (current-buffer)))
        (message "Drift: %d divergence(s)" total))
      total)))

;;;###autoload
(defun org-canvas-diff-batch ()
  "Run `org-canvas-diff', then exit non-zero if there was any drift.
Intended for scheduled jobs, invoked from a batch-mode Emacs with
-f org-canvas-diff-batch."
  (kill-emacs (if (> (org-canvas-diff) 0) 1 0)))

(provide 'org-canvas-diff)
;;; org-canvas-diff.el ends here
