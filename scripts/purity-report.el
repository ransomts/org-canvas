;;; purity-report.el --- Static purity analysis for org-canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; Heuristic static analysis that classifies org-canvas functions as
;; pure or impure by scanning their bodies for known side-effecting forms.
;;
;; Usage:
;;   emacs --batch -Q -l scripts/purity-report.el
;;
;; Optional environment variables:
;;   PURITY_VERBOSE=1              Show impurity reasons for each function
;;   PURITY_MODULE=pages           Only analyze a specific module (substring match)
;;   PURITY_PURE_ONLY=1            Only list pure functions
;;   PURITY_IGNORE=logging,filesystem  Comma-separated categories to ignore

;;; Code:

(require 'cl-lib)

;; --------------------------------------------------------------------------
;; Impurity markers: each entry is (CATEGORY DESCRIPTION SYMBOLS...)
;; --------------------------------------------------------------------------

(defconst purity--markers
  '(;; Buffer reading
    (buffer-read "reads buffer state"
                 org-entry-get org-entry-get-multivalued-property
                 org-get-heading org-get-todo-state org-get-tags
                 org-element-at-point org-element-parse-buffer
                 buffer-string buffer-substring buffer-substring-no-properties
                 buffer-name buffer-file-name buffer-modified-p
                 point point-min point-max point-marker
                 line-number-at-pos current-column
                 looking-at looking-at-p looking-back
                 re-search-forward re-search-backward
                 search-forward search-backward
                 match-string match-beginning match-end
                 org-complex-heading-regexp
                 thing-at-point char-after char-before
                 get-text-property text-properties-at
                 org-up-heading-safe org-back-to-heading
                 org-end-of-subtree org-end-of-meta-data
                 org-at-heading-p outline-next-heading
                 org-get-next-sibling org-goto-first-child
                 org-narrow-to-subtree org-table-to-lisp)

    ;; Buffer writing
    (buffer-write "modifies buffer"
                  insert insert-before-markers delete-region
                  delete-char erase-buffer
                  org-entry-put org-entry-delete
                  org-set-property org-delete-property
                  org-edit-headline org-toggle-tag
                  org-set-tags replace-match
                  save-buffer write-file write-region
                  org-id-get-create rename-buffer
                  set-buffer-modified-p indent-region
                  org-indent-region)

    ;; Point/position mutation
    (point-move "moves point or changes buffer context"
                goto-char forward-char backward-char
                forward-line forward-word
                beginning-of-line end-of-line
                widen narrow-to-region
                set-marker)

    ;; Buffer/window management
    (buffer-mgmt "manages buffers/windows"
                 find-file find-file-noselect kill-buffer
                 with-current-buffer set-buffer
                 switch-to-buffer pop-to-buffer
                 get-buffer-create generate-new-buffer
                 display-buffer delete-window
                 with-temp-buffer with-temp-file
                 insert-file-contents insert-file-contents-literally)

    ;; Network I/O
    (network "performs network I/O"
             plz url-retrieve url-retrieve-synchronously
             org-canvas-api-request org-canvas--api-request-raw
             org-canvas--api-get-paginated)

    ;; File system I/O
    (filesystem "accesses filesystem"
                file-exists-p file-readable-p file-directory-p
                file-name-directory file-name-nondirectory
                file-name-extension file-name-sans-extension
                expand-file-name file-relative-name
                directory-files make-directory delete-file
                copy-file rename-file file-attributes
                file-name-as-directory)

    ;; Logging / output
    (logging "produces output"
             message elog-info elog-debug elog-warning elog-error
             org-canvas-clear-log
             princ print pp prin1)

    ;; User interaction
    (interactive "prompts user"
                 read-string read-from-minibuffer
                 completing-read read-directory-name
                 read-file-name y-or-n-p yes-or-no-p
                 read-char read-key)

    ;; Process / shell
    (process "runs external processes"
             call-process start-process shell-command
             process-send-string make-process
             async-shell-command)

    ;; Global state mutation
    ;; NOTE: puthash/remhash/clrhash/gethash excluded — they typically operate
    ;; on locally-created hash-tables (e.g., build-payload functions).
    ;; The scanner cannot distinguish local vs global hash-table mutation.
    (global-state "mutates global state"
                  setq-default defvar-local
                  put
                  add-to-list add-hook remove-hook
                  run-hooks run-hook-with-args)

    ;; Save-excursion (implies buffer interaction)
    (excursion "uses save-excursion context"
               save-excursion save-restriction save-match-data)

    ;; Org export (runs full export engine, touches temp buffers)
    (org-export "runs org export machinery"
                org-export-as org-export-string-as
                org-html-export-as-html
                org-canvas--export-subtree-to-html))
  "Alist of impurity categories with their marker symbols.")

;; --------------------------------------------------------------------------
;; Build a fast lookup hash: symbol -> list of (CATEGORY DESCRIPTION)
;; --------------------------------------------------------------------------

(defvar purity--symbol-table (make-hash-table :test 'eq :size 200))

(dolist (entry purity--markers)
  (let ((category (car entry))
        (description (cadr entry))
        (symbols (cddr entry)))
    (dolist (sym symbols)
      (let ((existing (gethash sym purity--symbol-table)))
        (puthash sym (cons (list category description) existing)
                 purity--symbol-table)))))

;; --------------------------------------------------------------------------
;; Recursive body scanner
;; --------------------------------------------------------------------------

(defun purity--scan-form (form found-set depth)
  "Recursively scan FORM for impure symbols, adding to FOUND-SET.
DEPTH limits recursion to avoid pathological cases."
  (when (> depth 100) (cl-return-from purity--scan-form))
  (cond
   ;; Symbol: check against marker table
   ((symbolp form)
    (let ((hits (gethash form purity--symbol-table)))
      (dolist (hit hits)
        (puthash (car hit) (cadr hit) found-set))))
   ;; List: check car (function position) and recurse
   ((consp form)
    ;; Check the function symbol
    (when (symbolp (car form))
      (let ((hits (gethash (car form) purity--symbol-table)))
        (dolist (hit hits)
          (puthash (car hit) (cadr hit) found-set)))
      ;; Special: setq on known global prefixes
      (when (eq (car form) 'setq)
        (let ((var (cadr form)))
          (when (and (symbolp var)
                     (let ((name (symbol-name var)))
                       (or (string-prefix-p "org-canvas-" name)
                           (string-prefix-p "org-canvas--" name))))
            (puthash 'global-state "mutates package globals" found-set)))))
    ;; Recurse into sub-forms, handling dotted pairs safely
    (let ((tail (cdr form)))
      (while (consp tail)
        (purity--scan-form (car tail) found-set (1+ depth))
        (setq tail (cdr tail)))
      ;; Handle dotted pair cdr (e.g., (a . b))
      (when (and tail (symbolp tail))
        (let ((hits (gethash tail purity--symbol-table)))
          (dolist (hit hits)
            (puthash (car hit) (cadr hit) found-set))))))))

;; --------------------------------------------------------------------------
;; Analyze a single function
;; --------------------------------------------------------------------------

(defun purity--analyze-function (sym)
  "Analyze function SYM. Return (SYM PURE-P . REASONS-ALIST) or nil."
  (let ((func (symbol-function sym)))
    (when func
      (let* ((body (cond
                    ;; Compiled function — we can't inspect bytecode,
                    ;; so we check if we have the source form cached
                    ((byte-code-function-p func) nil)
                    ;; Lambda / closure
                    ((and (consp func) (memq (car func) '(lambda closure)))
                     (cddr func))
                    ;; Autoload — skip
                    ((autoloadp func) nil)
                    (t nil)))
             (found (make-hash-table :test 'eq :size 16)))
        (when body
          (dolist (form body)
            (purity--scan-form form found 0))
          (let ((reasons nil))
            (maphash (lambda (cat desc) (push (cons cat desc) reasons)) found)
            (list sym (null reasons) reasons)))))))

;; --------------------------------------------------------------------------
;; Source-level fallback for byte-compiled functions
;; --------------------------------------------------------------------------

(defvar purity--source-forms (make-hash-table :test 'eq :size 500)
  "Map from symbol to source body for functions we loaded from source.")

(defun purity--load-source-forms (file)
  "Read FILE and extract all defun/cl-defun forms into `purity--source-forms'."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (consp form)
                       (memq (car form) '(defun cl-defun defsubst)))
              (let ((name (cadr form))
                    ;; body is everything after the arglist (skip docstring)
                    (body (cdddr form)))
                ;; Skip docstring if present
                (when (stringp (car body))
                  (setq body (cdr body)))
                ;; Skip (declare ...) and (interactive ...) forms
                (while (and body (consp (car body))
                            (memq (caar body) '(declare interactive)))
                  (setq body (cdr body)))
                (puthash name body purity--source-forms)))))
      (end-of-file nil))))

(defun purity--analyze-from-source (sym)
  "Analyze function SYM from cached source forms."
  (let ((body (gethash sym purity--source-forms)))
    (when body
      (let ((found (make-hash-table :test 'eq :size 16)))
        (dolist (form body)
          (purity--scan-form form found 0))
        (let ((reasons nil))
          (maphash (lambda (cat desc) (push (cons cat desc) reasons)) found)
          (list sym (null reasons) reasons))))))

;; --------------------------------------------------------------------------
;; Main entry point
;; --------------------------------------------------------------------------

(defun purity--report ()
  "Generate purity report for all org-canvas functions."
  (let* ((verbose (getenv "PURITY_VERBOSE"))
         (module-filter (getenv "PURITY_MODULE"))
         (pure-only (getenv "PURITY_PURE_ONLY"))
         (ignore-cats
          (when (getenv "PURITY_IGNORE")
            (mapcar #'intern (split-string (getenv "PURITY_IGNORE") ","))))
         (lisp-dir (expand-file-name "lisp" default-directory))
         (source-files
          (cl-remove-if
           (lambda (f)
             (or (string-match-p "autoloads\\|credentials" f)
                 (and module-filter
                      (not (string-match-p module-filter
                                           (file-name-nondirectory f))))))
           (directory-files lisp-dir t "^org-canvas.*\\.el$")))
         (pure-fns nil)
         (impure-fns nil)
         (skipped 0))

    ;; Phase 1: Load source forms from all files
    (dolist (file source-files)
      (purity--load-source-forms file))

    ;; Phase 2: Analyze all functions from source forms (no loading needed)
    (maphash
     (lambda (sym body)
       (ignore body)
       (when (string-prefix-p "org-canvas" (symbol-name sym))
         (let ((result (purity--analyze-from-source sym)))
           (if result
               (let ((reasons (caddr result)))
                 ;; Filter out ignored categories
                 (when ignore-cats
                   (setq reasons (cl-remove-if
                                  (lambda (r) (memq (car r) ignore-cats))
                                  reasons)))
                 (if (null reasons)
                     (push (car result) pure-fns)
                   (push (cons (car result) reasons) impure-fns)))
             (cl-incf skipped)))))
     purity--source-forms)

    ;; Sort alphabetically
    (setq pure-fns (sort pure-fns (lambda (a b)
                                     (string< (symbol-name a)
                                              (symbol-name b)))))
    (setq impure-fns (sort impure-fns (lambda (a b)
                                         (string< (symbol-name (car a))
                                                  (symbol-name (car b))))))

    ;; ---------- Output ----------
    (princ "═══════════════════════════════════════════════════════════\n")
    (princ "  org-canvas Purity Report\n")
    (princ "═══════════════════════════════════════════════════════════\n\n")

    ;; Summary
    (let ((total (+ (length pure-fns) (length impure-fns))))
      (princ (format "  Total functions analyzed: %d\n" total))
      (princ (format "  Pure:   %3d  (%d%%)\n"
                     (length pure-fns)
                     (if (zerop total) 0
                       (round (* 100.0 (/ (float (length pure-fns)) total))))))
      (princ (format "  Impure: %3d  (%d%%)\n"
                     (length impure-fns)
                     (if (zerop total) 0
                       (round (* 100.0 (/ (float (length impure-fns)) total))))))
      (when (> skipped 0)
        (princ (format "  Skipped (byte-compiled, no source): %d\n" skipped))))
    (princ "\n")

    ;; Impurity category breakdown
    (unless pure-only
      (let ((cat-counts (make-hash-table :test 'eq)))
        (dolist (entry impure-fns)
          (dolist (reason (cdr entry))
            (let ((cat (car reason)))
              (puthash cat (1+ (or (gethash cat cat-counts) 0)) cat-counts))))
        (princ "  Impurity breakdown:\n")
        (let ((sorted-cats nil))
          (maphash (lambda (k v) (push (cons k v) sorted-cats)) cat-counts)
          (setq sorted-cats (sort sorted-cats (lambda (a b) (> (cdr a) (cdr b)))))
          (dolist (entry sorted-cats)
            (princ (format "    %-15s %3d functions\n"
                           (symbol-name (car entry)) (cdr entry)))))
        (princ "\n")))

    ;; Pure functions
    (princ "───────────────────────────────────────────────────────────\n")
    (princ (format "  PURE FUNCTIONS (%d)\n" (length pure-fns)))
    (princ "───────────────────────────────────────────────────────────\n")
    (dolist (sym pure-fns)
      (princ (format "  %s\n" (symbol-name sym))))
    (princ "\n")

    ;; Impure functions
    (unless pure-only
      (princ "───────────────────────────────────────────────────────────\n")
      (princ (format "  IMPURE FUNCTIONS (%d)\n" (length impure-fns)))
      (princ "───────────────────────────────────────────────────────────\n")
      (dolist (entry impure-fns)
        (let ((sym (car entry))
              (reasons (cdr entry)))
          (if verbose
              (progn
                (princ (format "  %s\n" (symbol-name sym)))
                (dolist (r reasons)
                  (princ (format "    [%s] %s\n"
                                 (symbol-name (car r)) (cdr r)))))
            ;; Compact: function name + category tags
            (princ (format "  %-55s  %s\n"
                           (symbol-name sym)
                           (mapconcat (lambda (r) (format "[%s]" (car r)))
                                      reasons " ")))))))

    (princ "\n")))

;; Run it
(purity--report)

;;; purity-report.el ends here
