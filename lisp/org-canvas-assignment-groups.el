;;; org-canvas-assignment-groups.el --- Pipeline-based Assignment Group Sync -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module implements the sync pipeline for Canvas Assignment Groups.
;;
;; FILE STRUCTURE
;; ==============
;; In assignment-groups.org:
;;   - Level 1 heading = Container (title only, ignored)
;;   - Level 2 headings = Assignment Groups
;;
;; PROPERTIES
;; ==========
;; WEIGHT       - Percentage weight (0-100) for weighted grading
;; DROP_LOWEST  - Number of lowest scores to drop
;; DROP_HIGHEST - Number of highest scores to drop
;;
;; WEIGHTED GRADING
;; ================
;; For weighted grades, the total WEIGHT across all groups should be 100.
;; Example:
;;   * Assignment Groups
;;   ** Labs          :WEIGHT: 40
;;   ** Exams         :WEIGHT: 30
;;   ** Participation :WEIGHT: 30
;;
;; DROP RULES
;; ==========
;; Canvas rejects drop rules for groups with no assignments.
;; The org-canvas-sync function handles this by:
;;   1. First sync: Create groups without drop rules
;;   2. Sync assignments (places them in groups)
;;   3. Second sync: Update groups with drop rules
;;
;; LINKING
;; =======
;; Assignments link to groups via the GROUP property:
;;   :GROUP: [[file:assignment-groups.org::*Labs][Labs]]

;;; Code:
(require 'org-canvas-core)

;;;; Configuration

(defcustom org-canvas-assignment-groups-file (org-canvas--path "assignment-groups.org")
  "Path to the assignment-groups.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-assignment-groups-file "assignment-groups.org")
(org-canvas-register-feature
 :name "Assignment Groups" :endpoint "assignment_groups"
 :file-var 'org-canvas-assignment-groups-file
 :id-field 'id :id-property "CANVAS_ID" :title-field 'name)
(org-canvas-register-properties "assignment-groups"
  :label "Assignment Groups"
  :file-var 'org-canvas-assignment-groups-file
  ;; Groups are level-2 headings with a WEIGHT property (same query the
  ;; sync uses).  LEVEL=1 would validate only the container heading and
  ;; never reach the groups (or their drop rules).
  :query "LEVEL=2+WEIGHT={.}"
  :properties
  '((:org-prop "WEIGHT" :data-key :group_weight :type number
     :doc "Group weight percentage (0-100)")
    (:org-prop "DROP_LOWEST" :data-key :drop_lowest :type number
     :doc "Number of lowest scores to drop")
    (:org-prop "DROP_HIGHEST" :data-key :drop_highest :type number
     :doc "Number of highest scores to drop")
    (:org-prop "POSITION" :data-key :position :type number
     :doc "Position in group ordering"))
  :structural-fn #'org-canvas--validate-drop-rules
  ;; File-level: the weights only mean anything summed across every group.
  :file-fn #'org-canvas--validate-weight-sum)

;;;; 1. Stage: Extraction

(defun org-canvas--assignment-group-compute-rules (data)
  "Compute drop rules from raw properties in DATA.
Returns DATA with :rules added and raw drop fields removed."
  (let* ((dl (plist-get data :drop_lowest))
         (dh (plist-get data :drop_highest))
         (nd (plist-get data :never_drop))
         (rules (delq nil
                  (list (when dl `(drop_lowest . ,dl))
                        (when dh `(drop_highest . ,dh))
                        (when nd `(never_drop . ,(mapcar #'string-to-number
                                                         (split-string nd "," t " "))))))))
    (plist-put data :rules rules)
    ;; Default weight to 0.0 if absent
    (unless (plist-get data :group_weight)
      (plist-put data :group_weight 0.0))
    data))

(org-canvas-define-parse assignment-group
  :title-key :name
  :entity-name "Assignment group"
  :after-transform #'org-canvas--assignment-group-compute-rules
  :properties
  (("WEIGHT"       :group_weight  :type number)
   ("DROP_LOWEST"  :drop_lowest   :type number)
   ("DROP_HIGHEST" :drop_highest  :type number)
   ("NEVER_DROP"   :never_drop    :type string)
   ("POSITION"     :position      :type number)))

;;;; 2. Stage: Transformation

(defun org-canvas--assignment-group-build-payload (data)
  "Convert DATA to Canvas payload.
Drop rules are only included for updates (PUT), not creates (POST),
because Canvas rejects drop rules when no assignments exist yet."
  (let ((name (plist-get data :name))
        (weight (plist-get data :group_weight))
        (rules (plist-get data :rules))
        (is-update (plist-get data :canvas-id)))

    (org-canvas--log-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" name)
    (org-canvas--log-info org-canvas--logger "[Stage 2: Transform]   Weight: %s" weight)

    ;; Only include rules on update - Canvas rejects drop rules on new groups with no assignments
    (let* ((pos (plist-get data :position))
           (payload (if (and rules is-update)
                        (progn
                          (org-canvas--log-info org-canvas--logger "[Stage 2: Transform]   Rules: %S (included for update)" rules)
                          `((name . ,name)
                            (group_weight . ,weight)
                            (rules . ,rules)))
                      (progn
                        (when rules
                          (org-canvas--log-info org-canvas--logger "[Stage 2: Transform]   Rules: %S (skipped for new group)" rules))
                        `((name . ,name)
                          (group_weight . ,weight))))))
      (when pos
        (push `(position . ,pos) payload))

      (org-canvas--log-debug org-canvas--logger "[Stage 2: Transform] Payload: %S" payload)
      payload)))

;;;; Unmanaged Group Reconciliation

(defun org-canvas--assignment-group-local-ids ()
  "Return the CANVAS_IDs recorded in `org-canvas-assignment-groups-file'."
  (let ((file (expand-file-name org-canvas-assignment-groups-file)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (org-map-entries
           (lambda () (org-entry-get (point) "CANVAS_ID"))
           "CANVAS_ID={.}" 'file))))))

(defun org-canvas--assignment-group-report-unmanaged (group)
  "Log GROUP as present on Canvas but absent from the org file.
Severity follows the consequence: a group holding assignments is an
active problem, an empty one is a trap that has not sprung yet."
  (let* ((name (alist-get 'name group))
         (id (alist-get 'id group))
         (count (length (append (alist-get 'assignments group) nil))))
    (if (> count 0)
        (org-canvas--log-error org-canvas--logger
          "[Unmanaged] Canvas group '%s' (ID %s) holds %d assignment(s) but is not in %s — if the course applies group weights those assignments count toward nothing.  Add the group to the file, or move the assignments into a managed group."
          name id count
          (file-name-nondirectory (expand-file-name org-canvas-assignment-groups-file)))
      (org-canvas--log-warning org-canvas--logger
        "[Unmanaged] Canvas group '%s' (ID %s) is not in %s.  It is empty now, so nothing is mis-weighted yet — but an assignment created in the Canvas web UI can default into it."
        name id
        (file-name-nondirectory (expand-file-name org-canvas-assignment-groups-file))))
    count))

(defun org-canvas--assignment-group-reconcile-unmanaged ()
  "Report Canvas assignment groups that `assignment-groups.org' does not manage.

An assignment landing in an unmanaged group is silently worth nothing
when weighted grading is on and the managed groups already sum to 100:
Canvas raises no error, the assignment looks normally graded, and the
weighting just omits it.  The usual instance is the stock `Assignments'
group every new course ships with.

This cannot live in `org-canvas-validate', which makes no API calls by
design — knowing a group exists remotely requires asking Canvas.  Runs
as the `:after-sync' hook of the group sync, costing one GET.

Nothing is ever deleted: Canvas requires at least one group, removing
one with assignments forces a move, and silently discarding something a
user made in the web UI would be worse than the trap.  Returns the list
of unmanaged groups.  Never signals — a failed check must not fail an
otherwise good sync."
  (condition-case err
      (let* ((remote (org-canvas-api-request-all-pages
                      'GET (org-canvas-api-course-endpoint "assignment_groups")
                      '(("include[]" . "assignments"))))
             (local-ids (org-canvas--assignment-group-local-ids))
             (unmanaged
              (seq-filter (lambda (group)
                            (not (member (format "%s" (alist-get 'id group))
                                         local-ids)))
                          (append remote nil))))
        (when unmanaged
          (let ((with-assignments 0))
            (dolist (group unmanaged)
              (when (> (org-canvas--assignment-group-report-unmanaged group) 0)
                (setq with-assignments (1+ with-assignments))))
            (message "Assignment groups: %d unmanaged group(s) on Canvas%s"
                     (length unmanaged)
                     (if (> with-assignments 0)
                         (format ", %d holding assignments" with-assignments)
                       ""))))
        unmanaged)
    (error
     (org-canvas--log-warning org-canvas--logger
       "[Unmanaged] Could not check for unmanaged groups: %s"
       (error-message-string err))
     nil)))

;;;; Main Sync Function

;; Generate org-canvas-sync-assignment-groups using the pipeline macro
;; Assignment groups are level-2 headings with a WEIGHT property
(org-canvas-define-sync assignment-groups
  :file org-canvas-assignment-groups-file
  :query "LEVEL=2+WEIGHT={.}"
  :parse #'org-canvas--assignment-group-parse-entry
  :build #'org-canvas--assignment-group-build-payload
  :endpoint "assignment_groups"
  :title-key :name
  :pull-item-fn #'org-canvas--assignment-group-pull-item
  :after-sync #'org-canvas--assignment-group-reconcile-unmanaged)

;; Generate org-canvas-delete-all-assignment-groups using the delete macro
;; Note: Canvas requires at least one assignment group, so the default
;; 'Assignments' group cannot be deleted (will error on last group)
;; Assignment groups use 'name instead of 'title for the title field
(org-canvas-define-delete-all assignment-groups
  :endpoint "assignment_groups"
  :file org-canvas-assignment-groups-file
  :title-field 'name)

(org-canvas-define-delete-at-point assignment-group
  :endpoint "assignment_groups/%s")

;;;; Pull

(defun org-canvas--assignment-group-pull-item (item pos)
  "Set per-item properties for a pulled assignment group.
ITEM is the API response alist, POS is the heading position."
  (let ((weight (alist-get 'group_weight item))
        (rules (alist-get 'rules item))
        (position (alist-get 'position item)))
    (when weight
      (org-canvas-org-set-property pos "WEIGHT" (format "%s" weight)))
    (when position
      (org-canvas-org-set-property pos "POSITION" (format "%s" position)))
    (when rules
      (let ((drop-lowest (alist-get 'drop_lowest rules))
            (drop-highest (alist-get 'drop_highest rules))
            (never-drop (alist-get 'never_drop rules)))
        (when (and drop-lowest (> drop-lowest 0))
          (org-canvas-org-set-property pos "DROP_LOWEST"
                                       (format "%s" drop-lowest)))
        (when (and drop-highest (> drop-highest 0))
          (org-canvas-org-set-property pos "DROP_HIGHEST"
                                       (format "%s" drop-highest)))
        (when (and never-drop (> (length never-drop) 0))
          (org-canvas-org-set-property pos "NEVER_DROP"
                                       (mapconcat #'number-to-string (append never-drop nil) ",")))))))

(org-canvas-define-pull assignment-groups
  :file org-canvas-assignment-groups-file
  :endpoint "assignment_groups"
  :title-field 'name
  :pull-item-fn #'org-canvas--assignment-group-pull-item)

(provide 'org-canvas-assignment-groups)
;;; org-canvas-assignment-groups.el ends here
