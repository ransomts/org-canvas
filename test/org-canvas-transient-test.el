;;; org-canvas-transient-test.el --- Buttercup tests for org-canvas-transient  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

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
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-demo-conflict)
            :to-be-truthy))

  (it "includes sync-at-point sub-prefix"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-dispatch-sync-at-point)
            :to-be-truthy))

  (it "includes pull-single sub-prefix"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-dispatch-pull-single)
            :to-be-truthy))

  (it "includes delete-at-point sub-prefix"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-dispatch-delete-at-point)
            :to-be-truthy))

  (it "offers the unsuppressed validation beside the ordinary one (issue #168)"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-validate-all)
            :to-be-truthy))

  (it "offers stamp adoption beside the drift report (issue #257)"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-diff-adopt-stamps)
            :to-be-truthy)))

(describe "org-canvas-dispatch-sync-at-point"
  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch-sync-at-point 'transient--prefix) :to-be-truthy))

  (it "includes page sync command"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch-sync-at-point 'org-canvas-sync-page-at-point)
            :to-be-truthy)))

(describe "org-canvas-dispatch-pull-single"
  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch-pull-single 'transient--prefix) :to-be-truthy))

  (it "includes pull-pages command"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch-pull-single 'org-canvas-pull-pages)
            :to-be-truthy)))

(describe "org-canvas-dispatch-delete-at-point"
  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch-delete-at-point 'transient--prefix) :to-be-truthy))

  (it "includes delete-page-at-point command"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch-delete-at-point 'org-canvas-delete-page-at-point)
            :to-be-truthy)))

;;;; Read-only courses grey out the writing half (issue #163)

(describe "org-canvas--transient-writable-p"
  (it "is true for a course you own"
    (let ((org-canvas-read-only nil))
      (expect (org-canvas--transient-writable-p) :to-be-truthy)))

  (it "is false for a course marked read-only"
    (let ((org-canvas-read-only t))
      (expect (org-canvas--transient-writable-p) :to-be nil)))

  (it "gates the writing groups and leaves reading alone"
    ;; The menu should say what mode the course is in, not only error
    ;; when a push command is chosen.  A prefix's layout is walked
    ;; structurally rather than by index: transient has moved the groups
    ;; and their plists between versions, and the suite runs on three.
    (cl-labels
        ((group-plist (node name)
           (cond
            ((and (listp node) (plist-member node :description)
                  (equal (plist-get node :description) name))
             node)
            ((or (vectorp node) (listp node))
             (cl-some (lambda (child) (group-plist child name))
                      (append node nil)))))
         (plist-of (name)
           (group-plist (get 'org-canvas-dispatch 'transient--layout) name))
         (gated-p (name)
           (eq (plist-get (plist-of name) :inapt-if-not)
               'org-canvas--transient-writable-p)))
      (dolist (writing '("Sync" "Files" "Delete" "Publish"))
        (expect (plist-of writing) :to-be-truthy)
        (expect (gated-p writing) :to-be-truthy))
      (dolist (reading '("Pull" "Tools" "Log"))
        (expect (plist-of reading) :to-be-truthy)
        (expect (gated-p reading) :to-be nil)))))

(provide 'org-canvas-transient-test)
;;; org-canvas-transient-test.el ends here
