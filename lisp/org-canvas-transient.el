;;; org-canvas-transient.el --- Transient command menu for org-canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a transient command menu (`org-canvas-dispatch') for
;; discoverability of org-canvas commands.  Requires transient.el
;; (built into Emacs 29+).

;;; Code:

(require 'transient)

;;;###autoload
(transient-define-prefix org-canvas-dispatch ()
  "Dispatch menu for org-canvas commands."
  ["Sync"
   ("s" "Sync all" org-canvas-sync)
   ("d" "Dry-run preview" org-canvas-sync-dry-run)
   ("f" "Force push (skip conflicts)" org-canvas-force-push)]
  ["Pull"
   ("p" "Pull all from Canvas" org-canvas-pull-all)]
  ["Delete"
   ("D" "Delete all from Canvas" org-canvas-delete-all)
   ("O" "Cleanup orphans" org-canvas-cleanup-orphans)]
  ["Tools"
   ("i" "Init (setup wizard)" org-canvas-init)
   ("t" "Test connection" org-canvas-test-connection)
   ("v" "Validate files" org-canvas-validate)
   ("S" "Status overview" org-canvas-status)]
  ["Learn"
   ("?" "Demo conflict UI" org-canvas-demo-conflict)])

(provide 'org-canvas-transient)
;;; org-canvas-transient.el ends here
