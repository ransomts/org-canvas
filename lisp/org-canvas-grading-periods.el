;;; org-canvas-grading-periods.el --- Pull grading periods from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module pulls a course's grading periods into grading-periods.org
;; (read-only).  Grading periods are set on the term or the account, not
;; by an instructor, so nothing here pushes; the pull exists so the
;; periods are visible beside the course and so the offline validator
;; can say when an assignment's DUE_AT falls outside every one of them.
;;
;; FILE STRUCTURE
;; ==============
;; In grading-periods.org:
;;   - Level 1 headings = grading periods (pulled from Canvas)
;;
;; PROPERTIES (set by pull, read-only)
;; ====================================
;; CANVAS_ID     - Canvas grading period ID
;; START_DATE    - Period start (Org timestamp, course zone)
;; END_DATE      - Period end
;; CLOSE_DATE    - Date after which grades in the period are locked
;; WEIGHT        - Weight of the period, when the term weights periods
;; IS_CLOSED     - "true" when the period is closed; omitted while open
;; The file's #+LAST_SYNCED header records the last pull.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/grading_periods
;; The reply wraps the list: {"grading_periods": [...]}, so the paging
;; helper cannot be used as is; a course has a handful at most.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-grading-periods-file (org-canvas--path "grading-periods.org")
  "Path to the grading-periods.org file."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-grading-periods-file "grading-periods.org")
(org-canvas-register-properties "grading-periods"
  :label "Grading Periods"
  :file-var 'org-canvas-grading-periods-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "START_DATE" :data-key :start_date :type timestamp
     :api-key "start_date"
     :doc "Period start (set by Canvas; pull-only)")
    (:org-prop "END_DATE" :data-key :end_date :type timestamp
     :api-key "end_date"
     :doc "Period end (set by Canvas; pull-only)")
    (:org-prop "CLOSE_DATE" :data-key :close_date :type timestamp
     :api-key "close_date"
     :doc "Date after which the period's grades are locked (pull-only)")
    (:org-prop "WEIGHT" :data-key :weight :type number
     :api-key "weight"
     :doc "Weight of the period when the term weights its periods (pull-only)")
    (:org-prop "IS_CLOSED" :data-key :is_closed :type boolean
     :api-key "is_closed"
     :doc "Whether the period is closed (pull-only; absent while open)")))

;;;; Pull

(defun org-canvas--grading-periods-fetch ()
  "Return the course's grading periods as a list of alists.
The endpoint wraps its list under `grading_periods', which is why this
does not go through `org-canvas-api-request-all-pages'."
  (let* ((endpoint (org-canvas-api-course-endpoint "grading_periods"))
         (response (org-canvas-api-request
                    'GET endpoint :params '(("per_page" . "100")))))
    (append (alist-get 'grading_periods response) nil)))

(defun org-canvas--grading-period-pull-item (item pos)
  "Set the grading period properties at POS from Canvas ITEM.
The title and the id are written by the caller; this writes the
dates, the weight when Canvas reports one, and the closed flag."
  (org-canvas--pull-set-timestamp-property pos "START_DATE" (alist-get 'start_date item))
  (org-canvas--pull-set-timestamp-property pos "END_DATE" (alist-get 'end_date item))
  (org-canvas--pull-set-timestamp-property pos "CLOSE_DATE" (alist-get 'close_date item))
  (let ((weight (alist-get 'weight item)))
    (when (numberp weight)
      (org-canvas-org-set-property pos "WEIGHT" (format "%s" weight))))
  (org-canvas--pull-set-boolean-property pos "IS_CLOSED" (alist-get 'is_closed item)))

(defconst org-canvas--grading-periods-pull-config
  (list :id-field 'id :title-field 'title :id-property "CANVAS_ID"
        :pull-item-fn #'org-canvas--grading-period-pull-item)
  "The pull plist `org-canvas--pull-process-item' reads for a period.")

;;;###autoload
(defun org-canvas-pull-grading-periods ()
  "Pull the course's grading periods from Canvas into grading-periods.org.
Headings are matched by CANVAS_ID and upserted in start-date order;
nothing local is deleted for being absent on Canvas.  A course without
grading periods gets the empty-file note every pull writes."
  (interactive)
  (org-canvas--start-operation "PULLING GRADING PERIODS")
  (let* ((file (expand-file-name org-canvas-grading-periods-file))
         (remote (org-canvas--grading-periods-fetch))
         (count 0)
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-overwrite file "grading-periods")
    (org-canvas--pull-confirm-unsaved file "grading-periods")
    (if (null remote)
        (org-canvas--pull-emit-empty-file
         file (org-canvas--pull-label-for "grading-periods"))
      (unless (file-exists-p file)
        (with-temp-file file (insert "")))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (dolist (item (org-canvas--pull-sort-items remote nil 'start_date))
          (org-canvas--pull-process-item
           item file org-canvas--grading-periods-pull-config)
          (cl-incf count))
        (org-canvas--pull-write-file-header)
        (org-canvas--save-buffer)))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (org-canvas--log-info org-canvas--logger
      "Grading periods pull complete: %d period%s%s"
      count (if (= count 1) "" "s")
      (if (zerop count) " (the course has none)" ""))
    (message "Grading periods pull complete: %d period%s%s."
             count (if (= count 1) "" "s")
             (if (zerop count) " (the course has none)" ""))))

(provide 'org-canvas-grading-periods)
;;; org-canvas-grading-periods.el ends here
