;;; org-canvas-transient.el --- Transient command menu for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides a transient command menu (`org-canvas-dispatch') for
;; discoverability of org-canvas commands.  Requires transient.el
;; (built into Emacs 29+).

;;; Code:

(require 'transient)

;;;###autoload
(transient-define-prefix org-canvas-dispatch-sync-at-point ()
  "Sync a single item at point."
  ["Sync at point"
   ("p" "Page" org-canvas-sync-page-at-point)
   ("a" "Assignment" org-canvas-sync-assignment-at-point)
   ("d" "Discussion" org-canvas-sync-discussion-at-point)
   ("n" "Announcement" org-canvas-sync-announcement-at-point)
   ("m" "Module" org-canvas-sync-module-at-point)
   ("r" "Rubric" org-canvas-sync-rubric-at-point)
   ("g" "Assignment Group" org-canvas-sync-assignment-group-at-point)
   ("c" "Calendar Event" org-canvas-sync-calendar-event-at-point)
   ("q" "Quiz" org-canvas-sync-quiz-at-point)
   ("Q" "New Quiz" org-canvas-sync-new-quiz-at-point)
   ("G" "Group Category" org-canvas-sync-group-category-at-point)])

;;;###autoload
(transient-define-prefix org-canvas-dispatch-pull-single ()
  "Pull a single content type from Canvas."
  ["Pull single"
   ("p" "Pages" org-canvas-pull-pages)
   ("a" "Assignments" org-canvas-pull-assignments)
   ("d" "Discussions" org-canvas-pull-discussions)
   ("n" "Announcements" org-canvas-pull-announcements)
   ("m" "Modules" org-canvas-pull-modules)
   ("r" "Rubrics" org-canvas-pull-rubrics)
   ("g" "Assignment Groups" org-canvas-pull-assignment-groups)
   ("c" "Calendar Events" org-canvas-pull-calendar-events)
   ("q" "Quizzes" org-canvas-pull-quizzes)
   ("Q" "New Quizzes" org-canvas-pull-new-quizzes)
   ("G" "Group Categories" org-canvas-pull-group-categories)
   ("f" "Files" org-canvas-pull-files)
   ("o" "Outcomes" org-canvas-pull-outcomes)
   ("e" "Sections" org-canvas-pull-sections)
   ("s" "Settings" org-canvas-pull-settings)
   ("S" "Submissions" org-canvas-pull-submissions)])

;;;###autoload
(transient-define-prefix org-canvas-dispatch-delete-at-point ()
  "Delete a single item at point from Canvas."
  ["Delete at point"
   ("p" "Page" org-canvas-delete-page-at-point)
   ("a" "Assignment" org-canvas-delete-assignment-at-point)
   ("d" "Discussion" org-canvas-delete-discussion-at-point)
   ("n" "Announcement" org-canvas-delete-announcement-at-point)
   ("m" "Module" org-canvas-delete-module-at-point)
   ("r" "Rubric" org-canvas-delete-rubric-at-point)
   ("g" "Assignment Group" org-canvas-delete-assignment-group-at-point)
   ("c" "Calendar Event" org-canvas-delete-calendar-event-at-point)
   ("f" "File" org-canvas-delete-file-at-point)
   ("G" "Group Category" org-canvas-delete-group-category-at-point)])

;;;###autoload
(transient-define-prefix org-canvas-dispatch ()
  "Dispatch menu for org-canvas commands."
  ["Sync"
   ("s" "Sync all" org-canvas-sync)
   ("d" "Dry-run preview" org-canvas-sync-dry-run)
   ("f" "Force push (skip conflicts)" org-canvas-force-push)
   ("@" "Sync at point..." org-canvas-dispatch-sync-at-point)]
  ["Files"
   ("F" "Force re-upload (all files)" org-canvas-files-force-reupload)
   ("z" "Force re-upload file at point" org-canvas-force-reupload-file-at-point)]
  ["Pull"
   ("p" "Pull all from Canvas" org-canvas-pull-all)
   ("P" "Pull single..." org-canvas-dispatch-pull-single)
   ("u" "Pull heading at point" org-canvas-pull-at-point)
   ("A" "Adopt Canvas item for heading at point" org-canvas-adopt-at-point)]
  ["Delete"
   ("D" "Delete all from Canvas" org-canvas-delete-all)
   ("O" "Cleanup orphans" org-canvas-cleanup-orphans)
   ("X" "Delete at point..." org-canvas-dispatch-delete-at-point)]
  ["Publish"
   ("m" "Publish module and its contents..." org-canvas-publish-module)
   ("M" "Unpublish module and its contents..." org-canvas-unpublish-module)
   ("R" "Apply scheduled releases (PUBLISH_AT)" org-canvas-apply-scheduled-releases)]
  ["Submissions"
   ("g" "Pull submissions (grading file)" org-canvas-pull-submissions)
   ("o" "Open a saved grading file" org-canvas-open-submissions)
   ("a" "Apply the completion rule to this file" org-canvas-submissions-apply-completion-rule)
   ("G" "Push grades" org-canvas-submissions-push-grades)]
  ["Tools"
   ("i" "Init (setup wizard)" org-canvas-init)
   ("c" "Switch course" org-canvas-activate-course)
   ("t" "Test connection" org-canvas-test-connection)
   ("v" "Validate files" org-canvas-validate)
   ("S" "Status overview (local)" org-canvas-status)
   ("r" "Drift report (compare with Canvas)" org-canvas-diff)
   ("x" "List external tools (LTI)" org-canvas-list-external-tools)]
  ["Log"
   ("ll" "Set log level" org-canvas-set-log-level)
   ("ld" "Set log destination" org-canvas-set-log-destination)
   ("lc" "Clear log" org-canvas-clear-log)]
  ["Learn"
   ("?" "Demo conflict UI" org-canvas-demo-conflict)])

(provide 'org-canvas-transient)
;;; org-canvas-transient.el ends here
