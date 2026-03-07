;;; mutation-test.el --- Mutation testing for org-canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; Mutation testing: deliberately introduce bugs into source code and
;; verify that the test suite catches them.  A mutation that passes all
;; tests ("survives") indicates a gap in test coverage.
;;
;; Usage:
;;   emacs --batch -Q -l scripts/mutation-test.el
;;
;; Environment variables:
;;   MUTANT_MODULE=pages     Only mutate a specific module
;;   MUTANT_LIMIT=20         Max mutations to try (default: all found)
;;   MUTANT_VERBOSE=1        Show details for each mutation
;;   MUTANT_PATTERN=build    Only run tests matching pattern (default: module name)
;;
;; Exit code: 0 if all mutations killed, 1 if any survived.

;;; Code:

(require 'cl-lib)

;; --------------------------------------------------------------------------
;; Mutation operators
;; --------------------------------------------------------------------------

(defconst mutant--operators
  '(;; Boolean flips
    (:name "flip t→nil"
     :find "\\bt\\b"
     :replace "nil"
     :context defun)

    (:name "flip :json-false→t"
     :find ":json-false"
     :replace "t"
     :context defun)

    (:name "flip t→:json-false"
     :find "\\bt\\b"
     :replace ":json-false"
     :context defun)

    ;; Method swaps
    (:name "swap POST→PUT"
     :find "'POST"
     :replace "'PUT"
     :context defun)

    (:name "swap PUT→POST"
     :find "'PUT"
     :replace "'POST"
     :context defun)

    ;; Comparison operator swaps
    (:name "swap equal→string="
     :find "(equal "
     :replace "(string= "
     :context defun)

    (:name "swap string=→equal"
     :find "(string= "
     :replace "(equal "
     :context defun)

    ;; Negate condition
    (:name "when→unless"
     :find "(when "
     :replace "(unless "
     :context defun)

    (:name "unless→when"
     :find "(unless "
     :replace "(when "
     :context defun)

    ;; Swap numeric defaults
    (:name "default 1→0"
     :find "\\b1)"
     :replace "0)"
     :context defun)

    ;; Remove string-equal check
    (:name "string-equal→eq"
     :find "(string-equal "
     :replace "(eq "
     :context defun)

    ;; Swap cons/push direction
    (:name "push→ignore"
     :find "(push "
     :replace "(ignore "
     :context defun))
  "List of mutation operators as plists.")

;; --------------------------------------------------------------------------
;; Source file scanning
;; --------------------------------------------------------------------------

(defun mutant--find-source-files (lisp-dir &optional module-filter)
  "Find org-canvas source files in LISP-DIR, optionally filtered by MODULE-FILTER."
  (let ((files (directory-files lisp-dir t "\\`org-canvas.*\\.el\\'")))
    (cl-remove-if
     (lambda (f)
       (let ((name (file-name-nondirectory f)))
         (or (string-match-p "autoloads\\|credentials" name)
             (and module-filter
                  (not (string-match-p module-filter name))))))
     files)))

(defun mutant--find-mutations-in-file (file)
  "Scan FILE for applicable mutations.
Returns list of (FILE LINE OPERATOR ORIGINAL-LINE)."
  (let ((mutations nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((in-defun nil)
            (line-num 0))
        (while (not (eobp))
          (setq line-num (1+ line-num))
          (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
            ;; Track defun boundaries
            (when (string-match-p "^(\\(cl-\\)?defun " line)
              (setq in-defun t))
            ;; Skip comment lines and docstrings
            (unless (or (string-match-p "^\\s-*;" line)
                        (string-match-p "^\\s-*\"" line))
              (when in-defun
                (dolist (op mutant--operators)
                  (let ((find-pat (plist-get op :find)))
                    (when (string-match-p find-pat line)
                      ;; Avoid self-mutations (don't mutate the find pattern itself)
                      (let* ((replaced (replace-regexp-in-string
                                        find-pat (plist-get op :replace) line nil nil nil 1))
                             ;; Only add if the replacement actually changed something
                             (changed (not (string= line replaced))))
                        (when changed
                          (push (list file line-num op line) mutations)))))))))
          (forward-line 1))))
    (nreverse mutations)))

;; --------------------------------------------------------------------------
;; Mutation application and testing
;; --------------------------------------------------------------------------

(defun mutant--apply-mutation (file line-num original-line new-line)
  "In FILE, replace line LINE-NUM content from ORIGINAL-LINE to NEW-LINE.
Returns the original file content for restoration."
  (let ((content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    (with-temp-file file
      (insert content)
      (goto-char (point-min))
      (forward-line (1- line-num))
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (delete-region beg end)
        (insert new-line)))
    content))

(defun mutant--restore-file (file original-content)
  "Restore FILE to ORIGINAL-CONTENT."
  (with-temp-file file
    (insert original-content)))

(defun mutant--run-tests (test-pattern)
  "Run eldev test with TEST-PATTERN. Return t if tests pass, nil if they fail."
  (let ((exit-code
         (call-process "eldev" nil nil nil "test" test-pattern)))
    (= exit-code 0)))

(defun mutant--module-from-file (file)
  "Extract module name from FILE for test pattern matching."
  (let ((name (file-name-sans-extension (file-name-nondirectory file))))
    (if (string-match "org-canvas-\\(.+\\)" name)
        (match-string 1 name)
      name)))

;; --------------------------------------------------------------------------
;; Main entry point
;; --------------------------------------------------------------------------

(defun mutant--report ()
  "Run mutation testing and report results."
  (let* ((module-filter (getenv "MUTANT_MODULE"))
         (limit (when (getenv "MUTANT_LIMIT")
                  (string-to-number (getenv "MUTANT_LIMIT"))))
         (verbose (getenv "MUTANT_VERBOSE"))
         (test-pattern-override (getenv "MUTANT_PATTERN"))
         (lisp-dir (expand-file-name "lisp" default-directory))
         (source-files (mutant--find-source-files lisp-dir module-filter))
         (all-mutations nil)
         (killed 0)
         (survived 0)
         (errors 0)
         (survivors nil))

    ;; Phase 1: Collect all possible mutations
    (dolist (file source-files)
      (let ((mutations (mutant--find-mutations-in-file file)))
        (setq all-mutations (append all-mutations mutations))))

    ;; Apply limit
    (when (and limit (> (length all-mutations) limit))
      ;; Sample evenly across the list
      (let ((step (/ (float (length all-mutations)) limit))
            (sampled nil))
        (dotimes (i limit)
          (push (nth (floor (* i step)) all-mutations) sampled))
        (setq all-mutations (nreverse sampled))))

    (princ "═══════════════════════════════════════════════════════════\n")
    (princ "  org-canvas Mutation Testing\n")
    (princ "═══════════════════════════════════════════════════════════\n\n")
    (princ (format "  Mutations found: %d\n" (length all-mutations)))
    (when limit
      (princ (format "  Limited to: %d\n" limit)))
    (princ "\n")

    ;; Phase 2: Apply each mutation, run tests, check if killed
    (let ((total (length all-mutations))
          (current 0))
      (dolist (mutation all-mutations)
        (cl-incf current)
        (let* ((file (nth 0 mutation))
               (line-num (nth 1 mutation))
               (op (nth 2 mutation))
               (original-line (nth 3 mutation))
               (find-pat (plist-get op :find))
               (replace-str (plist-get op :replace))
               (new-line (replace-regexp-in-string
                          find-pat replace-str original-line nil nil nil 1))
               (op-name (plist-get op :name))
               (module (mutant--module-from-file file))
               (test-pattern (or test-pattern-override module))
               (short-file (file-name-nondirectory file)))

          (princ (format "  [%d/%d] %s:%d %s..."
                         current total short-file line-num op-name))

          (let ((original-content (mutant--apply-mutation file line-num original-line new-line)))
            (unwind-protect
                (condition-case err
                    (if (mutant--run-tests test-pattern)
                        ;; Tests passed → mutation survived (BAD)
                        (progn
                          (cl-incf survived)
                          (push mutation survivors)
                          (princ " SURVIVED ✗\n")
                          (when verbose
                            (princ (format "    - %s\n    + %s\n" original-line new-line))))
                      ;; Tests failed → mutation killed (GOOD)
                      (progn
                        (cl-incf killed)
                        (princ " killed ✓\n")))
                  (error
                   (cl-incf errors)
                   (princ (format " ERROR: %s\n" (error-message-string err)))))
              (mutant--restore-file file original-content))))))

    ;; Phase 3: Summary
    (princ "\n")
    (princ "═══════════════════════════════════════════════════════════\n")
    (princ "  Results\n")
    (princ "═══════════════════════════════════════════════════════════\n\n")
    (let ((total (+ killed survived errors)))
      (princ (format "  Total mutations:  %d\n" total))
      (princ (format "  Killed:           %d ✓\n" killed))
      (princ (format "  Survived:         %d ✗\n" survived))
      (when (> errors 0)
        (princ (format "  Errors:           %d\n" errors)))
      (princ (format "  Kill rate:        %d%%\n"
                     (if (zerop total) 100
                       (round (* 100.0 (/ (float killed) total))))))
      (princ "\n"))

    ;; List survivors
    (when survivors
      (princ "───────────────────────────────────────────────────────────\n")
      (princ "  SURVIVING MUTATIONS (test gaps)\n")
      (princ "───────────────────────────────────────────────────────────\n")
      (dolist (mutation (nreverse survivors))
        (let* ((file (nth 0 mutation))
               (line-num (nth 1 mutation))
               (op (nth 2 mutation))
               (original-line (nth 3 mutation))
               (find-pat (plist-get op :find))
               (replace-str (plist-get op :replace))
               (new-line (replace-regexp-in-string
                          find-pat replace-str original-line nil nil nil 1)))
          (princ (format "\n  %s:%d [%s]\n"
                         (file-name-nondirectory file) line-num (plist-get op :name)))
          (princ (format "    - %s\n    + %s\n"
                         (string-trim original-line)
                         (string-trim new-line)))))
      (princ "\n"))

    ;; Exit code
    (kill-emacs (if (zerop survived) 0 1))))

;; Run it
(mutant--report)

;;; mutation-test.el ends here
