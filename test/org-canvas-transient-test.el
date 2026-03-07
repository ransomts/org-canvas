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

(provide 'org-canvas-transient-test)
;;; org-canvas-transient-test.el ends here
