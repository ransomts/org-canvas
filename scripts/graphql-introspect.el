;;; graphql-introspect.el --- Refresh the GraphQL fixture from the live instance  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Regenerates test/contract/canvas-graphql-contract.json from the Canvas
;; instance the course credentials point at, the standing procedure at
;; each semester boundary (test/contract/README.md, "GraphQL contract").
;;
;;     eldev exec -f scripts/graphql-introspect.el
;;
;; The credentials are read the way the package reads them: the file
;; ORG_CANVAS_CREDENTIALS names, else lisp/org-canvas-credentials.el,
;; else nothing loaded and `auth-source' asked for the host of
;; `org-canvas-base-url'.  The token goes to the extractor through the
;; child's process environment only — never a command line, so never
;; the shell history, and never this script's output.  ORG_CANVAS_PYTHON
;; names the interpreter that has graphql-core (default python3).
;;
;; Kept out of every Eldev fileset like the other scripts here: it is a
;; batch program, not part of the package.

;;; Code:

(require 'org-canvas)

;; `eldev exec' evaluates the forms rather than loading the file, so
;; `load-file-name' is nil there and the project root is the directory
;; Eldev runs in.
(let* ((root (if load-file-name
                 (file-name-directory (directory-file-name (file-name-directory load-file-name)))
               default-directory))
       (credentials (or (getenv "ORG_CANVAS_CREDENTIALS")
                        (let ((default (expand-file-name "lisp/org-canvas-credentials.el" root)))
                          (and (file-exists-p default) default))))
       (python (or (getenv "ORG_CANVAS_PYTHON") "python3"))
       (extractor (expand-file-name "test/contract/extract-canvas-graphql-contract.py" root)))
  (when credentials
    (load credentials nil t))
  (let ((token (org-canvas--api-token)))
    (unless (and (stringp token) (not (string-empty-p token)))
      (error "No Canvas API token: set ORG_CANVAS_CREDENTIALS to a credentials file, or configure auth-source"))
    (princ (format "Introspecting %s with the credentials of course %s (token: %d characters, not shown)\n"
                   org-canvas-base-url org-canvas-course-id (length token)))
    (let ((process-environment (cons (concat "CANVAS_API_TOKEN=" token) process-environment)))
      (with-temp-buffer
        (let ((status (call-process python nil t nil extractor "--introspect" org-canvas-base-url)))
          (princ (buffer-string))
          (unless (eq status 0)
            (kill-emacs 1)))))))

;;; graphql-introspect.el ends here
