;;; org-canvas-complexity-test.el --- Arity counting in the complexity report  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Guards the arity column of `eldev complexity' (issue #606).  The
;; report is the project's own gate, so its numbers have to mean what
;; the gate says they mean: `&key' is the language's bundle, not a
;; positional tail, and the name after `&rest' is that tail itself.
;;
;; scripts/ is outside `eldev-standard-excludes'' coverage fileset and
;; nothing requires the analyzer, so this file loads it by path.  The
;; `eldev-defcommand' at its end needs eldev, which the test runner
;; always has.

;;; Code:

(require 'buttercup)
(require 'eldev nil t)

(defconst org-canvas-complexity-test--script
  (let ((rel "scripts/complexity.el"))
    (cond ((file-exists-p rel) (expand-file-name rel))
          ((file-exists-p (expand-file-name rel default-directory))
           (expand-file-name rel default-directory))
          (t rel)))
  "Path to the analyzer that `Eldev' loads for `eldev complexity'.")

(load org-canvas-complexity-test--script nil t)

(describe "org-canvas--complexity-count-params"
  (it "counts a &key bundle as no parameters at all"
    (expect (org-canvas--complexity-count-params
             '(method url &key params data timeout as))
            :to-equal 2))

  (it "counts a &key entry written as a defaulted pair"
    (expect (org-canvas--complexity-count-params
             '(a &key (b 1) ((:c d) 2) &allow-other-keys))
            :to-equal 1))

  (it "counts optional parameters but not the &rest tail"
    (expect (org-canvas--complexity-count-params
             '(start end program &optional delete buffer &rest args))
            :to-equal 5))

  (it "stops at &aux"
    (expect (org-canvas--complexity-count-params '(a b &aux c d))
            :to-equal 2))

  (it "counts a plain positional arglist in full"
    (expect (org-canvas--complexity-count-params '(a b c d e f))
            :to-equal 6))

  (it "answers zero for an empty or non-list arglist"
    (expect (org-canvas--complexity-count-params nil) :to-equal 0)
    (expect (org-canvas--complexity-count-params '(&optional)) :to-equal 0)))
