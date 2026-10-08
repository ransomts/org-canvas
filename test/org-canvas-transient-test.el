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

  (it "offers the heading's Canvas page and its edit page (issue #292)"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-browse-at-point)
            :to-be-truthy)
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-browse-edit-at-point)
            :to-be-truthy))

  (it "offers stamp adoption beside the drift report (issue #257)"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-diff-adopt-stamps)
            :to-be-truthy)))

;; A key bound twice in one prefix reaches only one of its commands; the
;; dispatch menu once bound z and P twice each.
(defun test-org-canvas-transient--keys (prefix-sym)
  "Return every suffix key in PREFIX-SYM's layout, duplicates kept."
  (let* ((layout (get prefix-sym 'transient--layout))
         (columns (if (vectorp layout) (aref layout 2) layout))
         keys)
    (dolist (col columns)
      (when (vectorp col)
        (let ((suffixes (aref col (if (numberp (aref col 0)) 3 2))))
          (dolist (suffix suffixes)
            (when (listp suffix)
              (let ((plist (if (numberp (car suffix)) (nth 2 suffix) (cdr suffix))))
                (push (plist-get plist :key) keys)))))))
    keys))

(describe "org-canvas transient keys"
  (dolist (prefix '(org-canvas-dispatch
                    org-canvas-dispatch-sync-at-point
                    org-canvas-dispatch-pull-single
                    org-canvas-dispatch-delete-at-point))
    (it (format "binds no key twice in %s" prefix)
      (let* ((keys (test-org-canvas-transient--keys prefix))
             (dupes (seq-uniq (seq-filter
                               (lambda (k) (> (seq-count (lambda (x) (equal x k)) keys) 1))
                               keys))))
        (expect keys :not :to-be nil)
        (expect dupes :to-equal nil)))))

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

;;;; Autoloads

(describe "the generated autoloads (issue #469)"
  ;; A bare cookie on `transient-define-prefix' made `loaddefs-generate'
  ;; copy each whole form into the autoloads file, so loading it before
  ;; transient failed with void-function.  The file is generated and
  ;; loaded in a fresh `emacs -Q', as a package manager does: generated
  ;; here, where transient is loaded, the macro would be expanded and
  ;; the copied forms would never show.
  (it "declare the menus without loading transient"
    (let* ((lisp-dir (file-name-directory (locate-library "org-canvas-transient")))
           (tmp (make-temp-file "org-canvas-autoloads-" t))
           (out (expand-file-name "org-canvas-autoloads.el" tmp))
           (emacs (expand-file-name invocation-name invocation-directory))
           (menus '(org-canvas-dispatch org-canvas-dispatch-sync-at-point
                    org-canvas-dispatch-pull-single
                    org-canvas-dispatch-delete-at-point))
           (script
            `(progn
               (loaddefs-generate ,lisp-dir ,out)
               (load ,out nil t)
               (prin1 (list (featurep 'transient)
                            (mapcar (lambda (s)
                                      (and (autoloadp (symbol-function s))
                                           (commandp s)))
                                    ',menus)))))
           (status nil)
           (result nil))
      (unwind-protect
          (setq result
                (with-temp-buffer
                  (setq status (call-process emacs nil (list t nil) nil
                                             "-Q" "--batch" "--eval"
                                             (prin1-to-string script)))
                  (buffer-string)))
        (delete-directory tmp t))
      (expect status :to-equal 0)
      (expect (car (read-from-string result))
              :to-equal (list nil (make-list (length menus) t))))))

(provide 'org-canvas-transient-test)
;;; org-canvas-transient-test.el ends here
