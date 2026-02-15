;;; org-canvas-assignment-groups.el --- Pipeline-based Assignment Group Sync -*- lexical-binding: t; -*-

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
(require 'elog)

;;;; Configuration

(defcustom org-canvas-assignment-groups-file (org-canvas--path "assignment-groups.org")
  "Path to the assignment-groups.org file."
  :type 'file
  :group 'org-canvas)

;;;; 1. Stage: Extraction

(defun org-canvas--assignment-group-parse-entry ()
  "Extract assignment group data from the Org heading at point."
  (org-back-to-heading t)
  (let ((level (org-outline-level))
        (pom (point)))
    (elog-debug org-canvas--logger "[Stage 1: Parse] At point %d, level %d" pom level)

    (let* ((name (org-canvas--strip-statistics-cookie (org-get-heading t t t t)))
           (canvas-id (org-canvas-org-get-property pom "CANVAS_ID"))
           (group-weight (org-canvas-org-get-property pom "WEIGHT"))
           (drop-lowest (org-canvas-org-get-property pom "DROP_LOWEST"))
           (drop-highest (org-canvas-org-get-property pom "DROP_HIGHEST"))
           (never-drop (org-canvas-org-get-property pom "NEVER_DROP"))
           (position (org-canvas-org-get-property pom "POSITION")))

      (when (or (null name) (string-empty-p name))
        (error "Assignment group name cannot be empty at point %d" pom))

      (elog-info org-canvas--logger "[Stage 1: Parse] Assignment Group: '%s'" name)
      (elog-info org-canvas--logger "[Stage 1: Parse]   ID: %s" (or canvas-id "NEW"))
      (elog-info org-canvas--logger "[Stage 1: Parse]   Weight: %s%%" (or group-weight "0"))
      (elog-info org-canvas--logger "[Stage 1: Parse]   Drop Lowest: %s" (or drop-lowest "0"))
      (elog-info org-canvas--logger "[Stage 1: Parse]   Drop Highest: %s" (or drop-highest "0"))

      ;; Build rules as a flat alist for Canvas API
      (let ((rules (delq nil (list
                              (when drop-lowest
                                `(drop_lowest . ,(org-canvas--safe-string-to-number drop-lowest "DROP_LOWEST")))
                              (when drop-highest
                                `(drop_highest . ,(org-canvas--safe-string-to-number drop-highest "DROP_HIGHEST")))
                              (when never-drop
                                `(never_drop . ,(mapcar #'string-to-number (split-string never-drop "," t " "))))))))
        (when rules
          (elog-info org-canvas--logger "[Stage 1: Parse]   Rules: %S" rules))

        (list :name name
              :canvas-id canvas-id
              :group_weight (if group-weight (org-canvas--safe-string-to-number group-weight "WEIGHT") 0.0)
              :rules rules
              :position (when position (org-canvas--safe-string-to-number position "POSITION"))
              :pom pom)))))

;;;; 2. Stage: Transformation

(defun org-canvas--assignment-group-build-payload (data)
  "Convert DATA to Canvas payload.
Drop rules are only included for updates (PUT), not creates (POST),
because Canvas rejects drop rules when no assignments exist yet."
  (let ((name (plist-get data :name))
        (weight (plist-get data :group_weight))
        (rules (plist-get data :rules))
        (is-update (plist-get data :canvas-id)))

    (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%s'" name)
    (elog-info org-canvas--logger "[Stage 2: Transform]   Weight: %s" weight)

    ;; Only include rules on update - Canvas rejects drop rules on new groups with no assignments
    (let* ((pos (plist-get data :position))
           (payload (if (and rules is-update)
                        (progn
                          (elog-info org-canvas--logger "[Stage 2: Transform]   Rules: %S (included for update)" rules)
                          `((name . ,name)
                            (group_weight . ,weight)
                            (rules . ,rules)))
                      (progn
                        (when rules
                          (elog-info org-canvas--logger "[Stage 2: Transform]   Rules: %S (skipped for new group)" rules))
                        `((name . ,name)
                          (group_weight . ,weight))))))
      (when pos
        (push `(position . ,pos) payload))

      (elog-debug org-canvas--logger "[Stage 2: Transform] Payload: %S" payload)
      payload)))

;;;; 3. Stage: Execution

(defun org-canvas--assignment-group-push-to-api (data payload)
  "Send PAYLOAD with DATA to Canvas API.
Handles 404 on PUT by retrying as POST (stale CANVAS_ID recovery)."
  (org-canvas--push-to-api data payload
    :endpoint "assignment_groups"
    :title-key :name))

;;;; 4. Stage: Finalization

(defun org-canvas--assignment-group-finalize (data response)
  "Update local Org file with DATA based on the RESPONSE."
  (org-canvas--finalize-item data response :title-key :name))

;;;; Main Sync Function

;; Generate org-canvas-sync-assignment-groups using the pipeline macro
;; Assignment groups are level-2 headings with a WEIGHT property
(org-canvas-define-sync assignment-groups
  :file org-canvas-assignment-groups-file
  :query "LEVEL=2+WEIGHT={.}"
  :parse #'org-canvas--assignment-group-parse-entry
  :build #'org-canvas--assignment-group-build-payload
  :push #'org-canvas--assignment-group-push-to-api
  :finalize #'org-canvas--assignment-group-finalize
  :title-key :name
  :pull-item-fn #'org-canvas--assignment-group-pull-item)

(org-canvas-define-push-at-point assignment-group
  :parse #'org-canvas--assignment-group-parse-entry
  :build #'org-canvas--assignment-group-build-payload
  :push #'org-canvas--assignment-group-push-to-api
  :finalize #'org-canvas--assignment-group-finalize
  :pull-item-fn #'org-canvas--assignment-group-pull-item)

;; Generate org-canvas-delete-all-assignment-groups using the delete macro
;; Note: Canvas requires at least one assignment group, so the default
;; 'Assignments' group cannot be deleted (will error on last group)
;; Assignment groups use 'name instead of 'title for the title field
(org-canvas-define-delete-all assignment-groups
  :endpoint "assignment_groups"
  :file org-canvas-assignment-groups-file
  :title-field 'name)

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
  :item-fn #'org-canvas--assignment-group-pull-item)

(provide 'org-canvas-assignment-groups)
;;; org-canvas-assignment-groups.el ends here
