;;; org-canvas-transient-test.el --- Buttercup tests for org-canvas-transient  -*- lexical-binding: t; -*-

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-transient)

(describe "org-canvas-dispatch"
  (it "is an interactive command"
    (expect (commandp 'org-canvas-dispatch) :to-be-truthy))

  (it "is defined as a transient prefix"
    (expect (get 'org-canvas-dispatch 'transient--prefix) :to-be-truthy)))

(provide 'org-canvas-transient-test)
;;; org-canvas-transient-test.el ends here
