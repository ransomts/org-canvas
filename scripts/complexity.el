;;; complexity.el --- Code complexity analyzer for `eldev complexity'  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `eldev complexity' reports, for every function of the project, its
;; cognitive and cyclomatic complexity, its length and its arity.  The
;; command and the walks it scores with live here rather than in `Eldev',
;; which says how the project builds and loads this file to add the
;; command.  scripts/ is in `eldev-standard-excludes', so this file stays
;; out of the package and out of `eldev compile'; being loaded from
;; `Eldev' is what keeps `eldev complexity' working.
;;
;; Cognitive complexity follows SonarSource (2016): each scored form adds
;; 1 + the current nesting depth.  Cyclomatic complexity follows McCabe
;; (1976): 1 + one per decision point.  CODEMETRICS_THRESHOLD (default 15)
;; is the cognitive score above which the command exits 1, and the CI job
;; reads that count from the summary line.

;;; Code:

;; --- Code Complexity Analyzer ---

;; --- Cognitive complexity (SonarSource, 2016) ---
;; Penalizes nested control flow: each scored form adds +1 + current nesting depth.

(defvar org-canvas--cognitive-scored-forms
  '(if when unless cond pcase
    condition-case condition-case-unless-debug
    while dolist dotimes
    cl-loop catch unwind-protect)
  "Forms that increment cognitive complexity (+1 + nesting depth).")

(defun org-canvas--cognitive-walk (sexp depth)
  "Walk SEXP tree at nesting DEPTH, returning cognitive complexity score."
  (cond
   ((not (consp sexp)) 0)
   ((not (listp (cdr sexp))) 0)                ; dotted pair
   ((memq (car sexp) org-canvas--cognitive-scored-forms)
    (let ((increment (+ 1 depth))
          (child-score 0))
      (dolist (child (cdr sexp))
        (setq child-score (+ child-score (org-canvas--cognitive-walk child (1+ depth)))))
      (+ increment child-score)))
   (t
    (let ((total 0))
      (dolist (child sexp)
        (setq total (+ total (org-canvas--cognitive-walk child depth))))
      total))))

;; --- Cyclomatic complexity (McCabe, 1976) ---
;; Counts linearly independent paths: base 1 + one per decision point.
;; Nesting is irrelevant — only the count of branches matters.

(defvar org-canvas--cyclomatic-simple-forms
  '(if when unless while dolist dotimes cl-loop catch)
  "Forms that add +1 to cyclomatic complexity (single decision point).")

(defun org-canvas--cyclomatic-walk (sexp)
  "Walk SEXP tree, returning cyclomatic decision-point count (caller adds base 1)."
  (cond
   ((not (consp sexp)) 0)
   ((not (listp (cdr sexp))) 0)                ; dotted pair
   ;; Simple decision forms: +1, then recurse into children
   ((memq (car sexp) org-canvas--cyclomatic-simple-forms)
    (let ((total 1))
      (dolist (child (cdr sexp))
        (setq total (+ total (org-canvas--cyclomatic-walk child))))
      total))
   ;; cond/pcase: +1 per clause (each is a decision point)
   ((memq (car sexp) '(cond pcase))
    (let ((total 0))
      (dolist (clause (cdr sexp))
        (when (consp clause)
          (setq total (1+ total))
          (dolist (child clause)
            (setq total (+ total (org-canvas--cyclomatic-walk child))))))
      total))
   ;; condition-case: +1 per handler clause
   ((memq (car sexp) '(condition-case condition-case-unless-debug))
    (let ((total 0)
          ;; (condition-case VAR BODYFORM HANDLERS...)
          (handlers (cdddr sexp)))
      ;; Recurse into bodyform
      (setq total (+ total (org-canvas--cyclomatic-walk (nth 2 sexp))))
      (dolist (handler handlers)
        (when (consp handler)
          (setq total (1+ total))
          (dolist (child (cdr handler))
            (setq total (+ total (org-canvas--cyclomatic-walk child))))))
      total))
   ;; unwind-protect: no decision point, just recurse
   (t
    (let ((total 0))
      (dolist (child sexp)
        (setq total (+ total (org-canvas--cyclomatic-walk child))))
      total))))

;; --- Shared helpers ---

(defun org-canvas--complexity-skip-preamble (body)
  "Skip docstring, (declare ...), and (interactive ...) from BODY."
  (when (and body (stringp (car body)))
    (setq body (cdr body)))
  (while (and body (consp (car body))
              (memq (caar body) '(declare interactive)))
    (setq body (cdr body)))
  body)

(defun org-canvas--complexity-peek-form ()
  "Read the next form in the buffer without moving point.
Answer nil when no form follows, so a defun with an empty body asks
rather than errors."
  (ignore-errors
    (save-excursion
      (read (current-buffer)))))

(defun org-canvas--complexity-body-start ()
  "Return where the defun just read starts its body forms.
Point is at the defun's end.  The docstring and any `declare' or
`interactive' forms are preamble, skipped here exactly as
`org-canvas--complexity-skip-preamble' skips them in the parsed body,
so the length metric counts the code and not the prose that
documents it (issue #589)."
  (save-excursion
    (backward-sexp 1)                  ; back to the form's opening paren
    (down-list 1)                      ; inside it: the reads below must
                                        ; never leave the form, or they
                                        ; walk into the next one
    (read (current-buffer))            ; the head: defun, cl-defun, ...
    (read (current-buffer))            ; the name
    (read (current-buffer))            ; the argument list
    (when (stringp (org-canvas--complexity-peek-form))
      (read (current-buffer)))         ; the docstring
    (while (let ((form (org-canvas--complexity-peek-form)))
             (and (consp form)
                  (memq (car form) '(declare interactive))))
      (read (current-buffer)))         ; a declare or interactive form
    ;; Land on the first body form rather than at the preamble's closing
    ;; delimiter, so the docstring's last line is not counted as code.
    (skip-chars-forward " \t\n")
    (point)))

(defun org-canvas--complexity-count-params (arglist)
  "Count the number of required + optional parameters in ARGLIST.
Excludes &rest, &optional, &key keywords themselves but counts their arguments."
  (let ((count 0))
    (dolist (arg (if (listp arglist) arglist nil))
      (unless (memq arg '(&optional &rest &key &allow-other-keys &body))
        (setq count (1+ count))))
    count))

(defun org-canvas--complexity-analyze-file (file)
  "Analyze FILE, returning list of (name cog cyc loc params) per defun."
  (let ((results nil))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((sexp (read (current-buffer))))
              (when (and (consp sexp)
                         (memq (car sexp) '(defun cl-defun defmacro defsubst)))
                (let* ((name (nth 1 sexp))
                       (arglist (nth 2 sexp))
                       (params (org-canvas--complexity-count-params arglist))
                       ;; From the first body form, not from before the
                       ;; defun: a `loc' taken from before the form charged
                       ;; the docstring and the `;;;;' commentary above it
                       ;; to the function below (issue #589).
                       (loc (count-lines (org-canvas--complexity-body-start)
                                         (point)))
                       (body (org-canvas--complexity-skip-preamble (cdddr sexp)))
                       (cog 0) (cyc 0))
                  (dolist (form body)
                    (setq cog (+ cog (org-canvas--cognitive-walk form 0)))
                    (setq cyc (+ cyc (org-canvas--cyclomatic-walk form))))
                  (push (list name cog (1+ cyc) loc params) results)))))
        (end-of-file nil)))
    (nreverse results)))

(defun org-canvas--complexity-label (score)
  "Return severity label for SCORE."
  (cond
   ((> score 15) "very high")
   ((> score 10) "high")
   ((> score 5)  "moderate")
   (t nil)))

(eldev-defcommand org-canvas-complexity (&rest parameters)
  "Report code metrics for all project functions.

Scans all source files and scores each defun/defmacro using:
  - Cognitive complexity (SonarSource, 2016): penalizes nested control flow
  - Cyclomatic complexity (McCabe, 1976): counts linearly independent paths
  - Function length (LOC): lines of code per function
  - Parameter count: number of formal parameters

Set CODEMETRICS_THRESHOLD env var to change the cognitive fail threshold
\(default 15)."
  :aliases (complexity cc)
  :briefdoc "Report code complexity metrics"
  (let* ((threshold (string-to-number (or (getenv "CODEMETRICS_THRESHOLD") "15")))
         (source-files (mapcan (lambda (dir)
                                 (when (file-directory-p dir)
                                   (directory-files dir t "\\.el\\'" t)))
                               eldev-project-source-dirs))
         (total-fns 0)
         (above-threshold 0)
         (cog-bins (list 0 0 0 0))     ; low, moderate, high, very-high
         (max-cyc 0)
         (max-loc 0) (max-loc-fn nil)
         (long-fns 0)                   ; LOC > 50
         (high-arity 0)                 ; params > 5
         (total-loc 0))
    (eldev-output "=== Code Complexity Report ===")
    (eldev-output "    cog = cognitive  cyc = cyclomatic  loc = lines  par = params\n")
    (dolist (file (sort source-files #'string<))
      (let ((results (org-canvas--complexity-analyze-file file))
            (rel-path (file-relative-name file eldev-project-dir)))
        (when results
          (let ((sorted (sort (copy-sequence results)
                              (lambda (a b) (> (nth 1 a) (nth 1 b)))))
                (file-printed nil))
            (dolist (entry sorted)
              (let* ((name (car entry))
                     (cog (nth 1 entry))
                     (cyc (nth 2 entry))
                     (loc (nth 3 entry))
                     (params (nth 4 entry))
                     (label (org-canvas--complexity-label cog))
                     (name-str (symbol-name name))
                     (dots (make-string (max 1 (- 42 (length name-str))) ?.)))
                (setq total-fns (1+ total-fns))
                (setq total-loc (+ total-loc loc))
                (when (> cyc max-cyc) (setq max-cyc cyc))
                (when (> loc max-loc) (setq max-loc loc max-loc-fn name))
                (when (> loc 50) (setq long-fns (1+ long-fns)))
                (when (> params 5) (setq high-arity (1+ high-arity)))
                (cond
                 ((> cog 15) (setf (nth 3 cog-bins) (1+ (nth 3 cog-bins))))
                 ((> cog 10) (setf (nth 2 cog-bins) (1+ (nth 2 cog-bins))))
                 ((> cog 5)  (setf (nth 1 cog-bins) (1+ (nth 1 cog-bins))))
                 (t          (setf (nth 0 cog-bins) (1+ (nth 0 cog-bins)))))
                (when (> cog threshold)
                  (setq above-threshold (1+ above-threshold)))
                (when (or (and label (> cog 5)) (> loc 50) (> params 5))
                  (unless file-printed
                    (eldev-output "\n%s" rel-path)
                    (setq file-printed t))
                  (eldev-output "  %s %s cog %3d  cyc %3d  loc %3d  par %d%s"
                                name-str dots cog cyc loc params
                                (cond
                                 (label (format "  [%s]" label))
                                 ((> loc 50) "  [long]")
                                 ((> params 5) "  [arity]")
                                 (t ""))))))))))
    (eldev-output "\nSummary: %d functions, %d total LOC, %d above cognitive threshold (%d)"
                  total-fns total-loc above-threshold threshold)
    (eldev-output "  Cognitive  — Low (0-5): %d | Moderate (6-10): %d | High (11-15): %d | Very High (>15): %d"
                  (nth 0 cog-bins) (nth 1 cog-bins) (nth 2 cog-bins) (nth 3 cog-bins))
    (eldev-output "  Cyclomatic — max: %d" max-cyc)
    (eldev-output "  Length     — avg: %d LOC | max: %d (%s) | >50 LOC: %d"
                  (if (> total-fns 0) (/ total-loc total-fns) 0)
                  max-loc (if max-loc-fn (symbol-name max-loc-fn) "n/a") long-fns)
    (eldev-output "  Arity      — >5 params: %d" high-arity)
    (when (> above-threshold 0)
      (eldev-warn "\n%d function(s) exceed cognitive complexity threshold of %d"
                  above-threshold threshold)
      (signal 'eldev-quit 1))))

;;; complexity.el ends here
