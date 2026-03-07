;;; org-canvas-transient-test.el --- Buttercup tests for org-canvas-transient  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-transient)

(describe "org-canvas-dispatch"
  (it "is an interactive command"
    (expect (commandp 'org-canvas-dispatch) :to-be-truthy))

  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch 'transient--prefix) :to-be-truthy))

  (it "includes demo-conflict in menu"
    (let* ((layout (get 'org-canvas-dispatch 'transient--layout))
           (found nil))
      (dolist (group (append layout nil))
        (when (vectorp group)
          (let ((suffixes (aref group 3)))
            (dolist (suffix suffixes)
              (when (listp suffix)
                (let ((plist (nth 2 suffix)))
                  (when (eq (plist-get plist :command) 'org-canvas-demo-conflict)
                    (setq found t))))))))
      (expect found :to-be-truthy)))

  (it "includes sync-at-point sub-prefix"
    (let* ((layout (get 'org-canvas-dispatch 'transient--layout))
           (found nil))
      (dolist (group (append layout nil))
        (when (vectorp group)
          (let ((suffixes (aref group 3)))
            (dolist (suffix suffixes)
              (when (listp suffix)
                (let ((plist (nth 2 suffix)))
                  (when (eq (plist-get plist :command) 'org-canvas-dispatch-sync-at-point)
                    (setq found t))))))))
      (expect found :to-be-truthy)))

  (it "includes pull-single sub-prefix"
    (let* ((layout (get 'org-canvas-dispatch 'transient--layout))
           (found nil))
      (dolist (group (append layout nil))
        (when (vectorp group)
          (let ((suffixes (aref group 3)))
            (dolist (suffix suffixes)
              (when (listp suffix)
                (let ((plist (nth 2 suffix)))
                  (when (eq (plist-get plist :command) 'org-canvas-dispatch-pull-single)
                    (setq found t))))))))
      (expect found :to-be-truthy)))

  (it "includes delete-at-point sub-prefix"
    (let* ((layout (get 'org-canvas-dispatch 'transient--layout))
           (found nil))
      (dolist (group (append layout nil))
        (when (vectorp group)
          (let ((suffixes (aref group 3)))
            (dolist (suffix suffixes)
              (when (listp suffix)
                (let ((plist (nth 2 suffix)))
                  (when (eq (plist-get plist :command) 'org-canvas-dispatch-delete-at-point)
                    (setq found t))))))))
      (expect found :to-be-truthy))))

(describe "org-canvas-dispatch-sync-at-point"
  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch-sync-at-point 'transient--prefix) :to-be-truthy))

  (it "includes page sync command"
    (let* ((layout (get 'org-canvas-dispatch-sync-at-point 'transient--layout))
           (found nil))
      (dolist (group (append layout nil))
        (when (vectorp group)
          (let ((suffixes (aref group 3)))
            (dolist (suffix suffixes)
              (when (listp suffix)
                (let ((plist (nth 2 suffix)))
                  (when (eq (plist-get plist :command) 'org-canvas-sync-page-at-point)
                    (setq found t))))))))
      (expect found :to-be-truthy))))

(describe "org-canvas-dispatch-pull-single"
  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch-pull-single 'transient--prefix) :to-be-truthy))

  (it "includes pull-pages command"
    (let* ((layout (get 'org-canvas-dispatch-pull-single 'transient--layout))
           (found nil))
      (dolist (group (append layout nil))
        (when (vectorp group)
          (let ((suffixes (aref group 3)))
            (dolist (suffix suffixes)
              (when (listp suffix)
                (let ((plist (nth 2 suffix)))
                  (when (eq (plist-get plist :command) 'org-canvas-pull-pages)
                    (setq found t))))))))
      (expect found :to-be-truthy))))

(describe "org-canvas-dispatch-delete-at-point"
  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch-delete-at-point 'transient--prefix) :to-be-truthy))

  (it "includes delete-page-at-point command"
    (let* ((layout (get 'org-canvas-dispatch-delete-at-point 'transient--layout))
           (found nil))
      (dolist (group (append layout nil))
        (when (vectorp group)
          (let ((suffixes (aref group 3)))
            (dolist (suffix suffixes)
              (when (listp suffix)
                (let ((plist (nth 2 suffix)))
                  (when (eq (plist-get plist :command) 'org-canvas-delete-page-at-point)
                    (setq found t))))))))
      (expect found :to-be-truthy))))

(provide 'org-canvas-transient-test)
;;; org-canvas-transient-test.el ends here
