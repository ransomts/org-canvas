;;; org-canvas-batch.el --- One-line batch entry point for org-canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A course that runs its Canvas work from `emacs --batch' used to open
;; every script with the same bootstrap: put each straight build
;; directory on `load-path', require org-canvas, load the course's
;; credentials file and set the variables a batch Emacs needs.  Of an
;; eleven-line script printing the grading queue, ten lines were that
;; header (issue #416).  This file does the bootstrap once.
;;
;; `org-canvas-batch-setup' is the half a script still wants: given a
;; course directory, it loads the credentials file found there and sets
;; the batch-safe variables.  `org-canvas-batch' is the other half, a
;; command line with subcommands (status, grades, pull, push, diff,
;; validate, sync ...), which `scripts/org-canvas' wraps:
;;
;;   $ scripts/org-canvas -C ~/courses/ethics status
;;   $ "$EMACS" --batch -l org-canvas-batch -f org-canvas-batch -- diff
;;
;; Loading this file first makes org-canvas's dependencies loadable in a
;; bare batch Emacs (`org-canvas-batch-ensure-load-path'): directories
;; named in ORG_CANVAS_LOAD_PATH, then package.el's packages, then the
;; straight and elpa build directories under `user-emacs-directory'.
;; The directory this file sits in always comes first, so the checkout
;; the script was run from is the code that runs, and
;; `load-prefer-newer' is set, so a stale byte-compiled file left in that
;; checkout never shadows the source beside it (issue #424).
;;
;; This is a command file: it sits above every feature module and no
;; module requires it.

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'loadhist)

;; Set before anything below loads, for the rest of this process: a
;; checkout compiled in place keeps .elc files that `git pull' and
;; `straight-rebuild-package' never refresh, and Emacs would otherwise
;; load them over newer sources ("Source file ... newer than
;; byte-compiled file; using older file"), running old code against
;; this entry point (issue #424).  A `let' around the require below
;; would not do: org-canvas loads libraries later, through autoloads and
;; optional requires, and those must prefer the newer file too.
;; `scripts/org-canvas' sets it on the command line as well, so this
;; file itself is never read from a stale .elc.
(setq load-prefer-newer t)

;;;; Load Path

(defconst org-canvas-batch-load-path-variable "ORG_CANVAS_LOAD_PATH"
  "Environment variable naming extra `load-path' directories.
Its value is a list of directories separated by the variable `path-separator',
added before anything else is searched.")

(defconst org-canvas-batch--own-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "The directory this file was loaded from.")

(defun org-canvas-batch--subdirectories (root)
  "Return the package directories directly under ROOT, or nil."
  (when (file-directory-p root)
    (cl-remove-if-not #'file-directory-p
                      (directory-files root t "\\`[^.]"))))

(defun org-canvas-batch--scan-directories ()
  "Return the build directories straight and package.el keep, if any."
  (append (org-canvas-batch--subdirectories
           (expand-file-name "straight/build" user-emacs-directory))
          (org-canvas-batch--subdirectories
           (expand-file-name "elpa" user-emacs-directory))))

(defun org-canvas-batch--env-directories ()
  "Return the directories `org-canvas-batch-load-path-variable' names."
  (let ((value (getenv org-canvas-batch-load-path-variable)))
    (and value (not (string-empty-p value))
         (mapcar #'expand-file-name (split-string value path-separator t)))))

(defun org-canvas-batch--try-package-initialize ()
  "Activate package.el's installed packages, ignoring any failure."
  (condition-case nil
      (progn (require 'package)
             (package-initialize)
             t)
    (error nil)))

(defun org-canvas-batch-ensure-load-path ()
  "Make org-canvas and its dependencies loadable in a bare batch Emacs.
Add this file's directory and the directories the environment
variable ORG_CANVAS_LOAD_PATH names to `load-path'.  Then, if the
dependency plz still cannot be found, activate package.el's
packages, and failing that add every build directory under
straight/build and elpa in `user-emacs-directory'.  Return the
directories added.
This file's directory, then the named ones, end up first, ahead of
any directory package.el put in front, so an org-canvas that
package.el or straight installed never shadows the copy asked
for (issue #424)."
  (let ((before load-path)
        (first (cons org-canvas-batch--own-directory
                     (org-canvas-batch--env-directories))))
    (dolist (dir (reverse first))
      (add-to-list 'load-path dir))
    (unless (locate-library "plz")
      (org-canvas-batch--try-package-initialize))
    (unless (locate-library "plz")
      (dolist (dir (org-canvas-batch--scan-directories))
        (add-to-list 'load-path dir t)))
    (setq load-path (append first (cl-set-difference load-path first
                                                     :test #'equal)))
    (cl-set-difference load-path before :test #'equal)))

(org-canvas-batch-ensure-load-path)

(require 'org-canvas)

;;;; Setup

(defconst org-canvas-batch-credentials-file "org-canvas-credentials.el"
  "The name of a course directory's credentials file.")

(defun org-canvas-batch-setup (&optional directory)
  "Prepare this Emacs to run org-canvas on the course in DIRECTORY.
DIRECTORY defaults to `default-directory'.  Load the credentials
file found there, if any, without printing its name or contents,
after setting `org-canvas-directory' to DIRECTORY, which the file may
override.
Set `find-file-visit-truename', so a script that opens a course file
itself gets the buffer org-canvas uses (issue #97).  The
changed-on-disk question is not answered here: org-canvas opens
course files through its own guard, which rereads or restores a
stale file in batch rather than asking (issues #121, #188, #249),
where a blanket `revert-without-query' would reread a rolled-back
copy silently.  Return the credentials file loaded, or nil."
  (let* ((dir (file-name-as-directory
               (expand-file-name (or directory default-directory))))
         (credentials (expand-file-name org-canvas-batch-credentials-file dir)))
    (unless (file-directory-p dir)
      (user-error "Course directory does not exist: %s" dir))
    (setq find-file-visit-truename t)
    (setq default-directory dir)
    (setq org-canvas-directory dir)
    (when (file-exists-p credentials)
      (load credentials nil t t))
    (org-canvas--api-token-forget)
    (org-canvas--time-zone-reset)
    (and (file-exists-p credentials) credentials)))

;;;; Command Line

(define-error 'org-canvas-batch-usage-error "Usage error")

(defun org-canvas-batch--usage (format-string &rest args)
  "Signal a usage error whose message is FORMAT-STRING applied to ARGS."
  (signal 'org-canvas-batch-usage-error
          (list (apply #'format format-string args))))

(defconst org-canvas-batch--commands
  '(("status" org-canvas-batch--cmd-status 0 0 ""
     "Print the grading queue (org-canvas-submissions-status).")
    ("overview" org-canvas-batch--cmd-overview 0 0 ""
     "Print the local sync overview (org-canvas-status).")
    ("grades" org-canvas-batch--cmd-grades 1 nil "[--download] NAME..."
     "Pull each assignment's submissions into its grading file.")
    ("pull-queue" org-canvas-batch--cmd-pull-queue 0 0 ""
     "Pull every column the grading queue says needs it (#415).")
    ("push-comments" org-canvas-batch--cmd-push-comments 1 nil "ASSIGNMENT..."
     "Push only the sent comments edited or marked DELETE (#425).")
    ("pull" org-canvas-batch--cmd-pull 1 nil "FEATURE:TITLE..."
     "Replace named headings with Canvas's versions.")
    ("push" org-canvas-batch--cmd-push 1 nil "FEATURE:TITLE..."
     "Push named headings (org-canvas-sync-headings).")
    ("diff" org-canvas-batch--cmd-diff 0 0 ""
     "Print the drift report; exit 1 on drift.")
    ("validate" org-canvas-batch--cmd-validate 0 1 "[--all]"
     "Validate the Org files offline; exit 1 on errors.")
    ("sync" org-canvas-batch--cmd-sync 0 0 ""
     "Push every enabled feature; exit 1 if any item failed.")
    ("version" org-canvas-batch--cmd-version 0 0 ""
     "Print the version, commit and directory loaded (also --version)."
     no-course))
  "The subcommands: (NAME FUNCTION MIN-ARGS MAX-ARGS SYNOPSIS DOC [NO-COURSE]).
MAX-ARGS nil means any number.  FUNCTION takes the parsed command
line and returns the exit status.  NO-COURSE non-nil means the
command runs without a course directory, its credentials unread.")

(defun org-canvas-batch--help-text ()
  "Return the usage text, one line per subcommand."
  (concat
   "Usage: org-canvas [-C DIR] [--dry-run] COMMAND [ARGS...]\n\n"
   "  -C, --course DIR  course directory (default: the current directory)\n"
   "  -n, --dry-run     push and sync send nothing\n"
   "  -h, --help        print this text\n"
   "  -V, --version     print the version and where it was loaded from\n\nCommands:\n"
   (mapconcat (lambda (c)
                (format "  %-30s %s" (string-trim (format "%s %s" (nth 0 c) (nth 4 c)))
                        (nth 5 c)))
              org-canvas-batch--commands "\n")
   "\n\nFEATURE:TITLE names a heading by its exact title (assignment:Essay 1);\n"
   "FEATURE#ID names it by its Canvas id (assignment#2563805).\n"
   "Exit status: 0 done, 1 drift, errors or a failed item, 2 usage, 3 error.\n"))

(defun org-canvas-batch--parse-option (arg rest parsed)
  "Read the global option ARG, REST the arguments after it, into PARSED.
Return (PARSED . REST) with any value ARG took consumed from REST."
  (cond
   ((member arg '("-C" "--course"))
    (unless rest (org-canvas-batch--usage "%s needs a directory" arg))
    (cons (plist-put parsed :directory (car rest)) (cdr rest)))
   ((string-prefix-p "--course=" arg)
    (cons (plist-put parsed :directory (substring arg 9)) rest))
   ((member arg '("-n" "--dry-run"))
    (cons (plist-put parsed :dry-run t) rest))
   ((member arg '("-h" "--help"))
    (cons (plist-put parsed :help t) rest))
   ((member arg '("-V" "--version"))
    (cons (plist-put parsed :version t) rest))
   (t (org-canvas-batch--usage "Unknown option %s" arg))))

(defun org-canvas-batch-parse-args (args)
  "Parse ARGS, the words after the program name, into a plist.
The plist holds :directory, :dry-run, :help, :command (the entry of
`org-canvas-batch--commands') and :args.  A leading \"--\" is
dropped.  --version stands for the version command, whatever
follows it; --help, or no command at all, asks for the help.
Anything malformed signals `org-canvas-batch-usage-error'."
  (let ((parsed (list :directory nil :dry-run nil :help nil :version nil))
        (rest (if (equal (car args) "--") (cdr args) args)))
    (while (and rest (string-prefix-p "-" (car rest)))
      (let ((step (org-canvas-batch--parse-option (car rest) (cdr rest) parsed)))
        (setq parsed (car step) rest (cdr step))))
    (when (plist-get parsed :version)
      (setq rest (list "version")))
    (if (or (plist-get parsed :help) (null rest))
        (plist-put parsed :help t)
      (let ((command (assoc (car rest) org-canvas-batch--commands)))
        (unless command
          (org-canvas-batch--usage "Unknown command %s" (car rest)))
        (org-canvas-batch--check-arity command (cdr rest))
        (plist-put (plist-put parsed :command command) :args (cdr rest))))))

(defun org-canvas-batch--check-arity (command args)
  "Signal a usage error unless ARGS suit COMMAND's argument counts."
  (let ((n (length args)) (min (nth 2 command)) (max (nth 3 command)))
    (when (or (< n min) (and max (> n max)))
      (org-canvas-batch--usage "%s takes %s" (car command)
                               (if (string-empty-p (nth 4 command))
                                   "no arguments"
                                 (nth 4 command))))))

(defun org-canvas-batch-heading-entry (arg)
  "Return the heading entry ARG names, for `org-canvas-sync-headings'.
ARG is FEATURE:TITLE, naming a heading by its exact title, or
FEATURE#ID, naming it by its Canvas id.  FEATURE is a module's name,
singular or plural, and ends at the first colon or hash sign, so a
title may hold either."
  (unless (string-match "\\`\\([a-z][a-z-]*\\)\\([:#]\\)\\(.+\\)\\'" arg)
    (org-canvas-batch--usage
     "%s is not FEATURE:TITLE or FEATURE#ID (to pull a grading file, use grades)"
     arg))
  (list (match-string 1 arg) (match-string 3 arg)
        (if (equal (match-string 2 arg) "#") 'canvas-id 'title)))

;;;; Subcommands

(defun org-canvas-batch--failed-count (results)
  "Return how many of RESULTS, heading result plists, failed."
  (cl-count 'failed results :key (lambda (r) (plist-get r :outcome))))

(defun org-canvas-batch--cmd-status (_parsed)
  "Print the grading queue; return 0."
  (org-canvas-submissions-status)
  0)

(defun org-canvas-batch--cmd-overview (_parsed)
  "Print the local sync overview; return 0."
  (org-canvas-status)
  (princ (with-current-buffer "*canvas-status*" (buffer-string)))
  0)

(defun org-canvas-batch--pull-grades (name download)
  "Pull NAME's submissions into its grading file, attachments if DOWNLOAD.
Return t, or nil after reporting a failure."
  (condition-case err
      (let* ((org-canvas-submissions-default-view 'detail)
             (buf (org-canvas-pull-submissions name download)))
        (message "Grading file: %s" (buffer-file-name buf))
        t)
    (error
     (org-canvas--user-message "grades '%s' failed: %s"
                               name (error-message-string err))
     nil)))

(defun org-canvas-batch--cmd-grades (parsed)
  "Pull the grading file of each assignment PARSED names; return the status."
  (let* ((args (plist-get parsed :args))
         (download (member "--download" args))
         (names (remove "--download" args)))
    (unless names
      (org-canvas-batch--usage "grades takes [--download] NAME..."))
    (if (cl-every #'identity
                  (mapcar (lambda (name) (org-canvas-batch--pull-grades name download))
                          names))
        0 1)))

(defun org-canvas-batch--cmd-pull-queue (_parsed)
  "Pull each column in need of it, by the grading queue; return 0."
  (unless (fboundp 'org-canvas-submissions-pull-queue)
    (org-canvas-batch--usage
     "pull-queue is not available: this org-canvas has no org-canvas-submissions-pull-queue (issue #415)"))
  (funcall 'org-canvas-submissions-pull-queue)
  0)

(defun org-canvas-batch--cmd-push-comments (parsed)
  "Push the sent comments changed in each grading file PARSED names.
A --dry-run sends nothing.  Return 1 if any change was not sent."
  (let* ((org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run)))
         (results (mapcar #'org-canvas-push-submission-comment-edits
                          (plist-get parsed :args))))
    (if (cl-every (lambda (r) (and r (zerop (+ (plist-get r :refused)
                                               (plist-get r :failed)
                                               (or (plist-get r :errored) 0)))))
                  results)
        0 1)))

(defun org-canvas-batch--cmd-pull (parsed)
  "Pull the headings PARSED names; return 1 if any failed."
  (let ((entries (mapcar #'org-canvas-batch-heading-entry
                         (plist-get parsed :args))))
    (if (zerop (org-canvas-batch--failed-count
                (org-canvas-pull-headings entries)))
        0 1)))

(defun org-canvas-batch--cmd-push (parsed)
  "Push the headings PARSED names; return 1 if any failed."
  (let ((entries (mapcar #'org-canvas-batch-heading-entry
                         (plist-get parsed :args)))
        (org-canvas--dry-run (or org-canvas--dry-run
                                 (plist-get parsed :dry-run))))
    (if (zerop (org-canvas-batch--failed-count
                (org-canvas-sync-headings entries)))
        0 1)))

(defun org-canvas-batch--cmd-diff (_parsed)
  "Print the drift report; return 1 if anything drifted."
  (if (> (org-canvas-diff) 0) 1 0))

(defun org-canvas-batch--cmd-validate (parsed)
  "Validate the Org files, all findings under --all in PARSED.
Return 1 if validation found an error."
  (let ((args (plist-get parsed :args)))
    (when (and args (not (equal args '("--all"))))
      (org-canvas-batch--usage "validate takes [--all]"))
    (if (> (org-canvas-validate nil (and args t)) 0) 1 0)))

(defun org-canvas-batch--cmd-sync (parsed)
  "Push every enabled feature, a dry run if PARSED asked for --dry-run.
Return 1 if any item failed."
  (let* ((org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run)))
         (counters (org-canvas-sync)))
    (if (> (or (plist-get counters :fail) 0) 0) 1 0)))

;;;; Version

(defun org-canvas-batch--loaded-file ()
  "Return the file org-canvas was loaded from, or nil if it is not loaded."
  (and (featurep 'org-canvas) (feature-file 'org-canvas)))

(defun org-canvas-batch--header-version (file)
  "Return the Version header of FILE's source, or \"unknown\"."
  (let ((source (replace-regexp-in-string "\\.elc\\'" ".el" file)))
    (or (and (file-readable-p source)
             (with-temp-buffer
               (insert-file-contents source)
               (lm-version)))
        "unknown")))

(defun org-canvas-batch--git-commit (directory)
  "Return the short commit of the git checkout holding DIRECTORY, or nil.
Nil when git is missing, DIRECTORY is not in a checkout, or git
fails in any other way."
  (condition-case nil
      (let ((default-directory (file-name-as-directory directory)))
        (car (process-lines "git" "rev-parse" "--short" "HEAD")))
    (error nil)))

(defun org-canvas-batch-version-text ()
  "Return the version text: version, commit and the directory loaded.
The commit is that of the checkout holding the loaded file, symbolic
links followed, so a straight build directory names the commit of
the repository it links into."
  (let* ((file (or (org-canvas-batch--loaded-file)
                   (error "Org-canvas is not loaded")))
         (commit (org-canvas-batch--git-commit
                  (file-name-directory (file-truename file)))))
    (format "org-canvas %s%s\nLoaded from %s (%s)\n"
            (org-canvas-batch--header-version file)
            (if commit (format " (commit %s)" commit) "")
            (file-name-directory file)
            (if (string-suffix-p ".elc" file) "byte-compiled" "source"))))

(defun org-canvas-batch--cmd-version (_parsed)
  "Print the version text; return 0."
  (princ (org-canvas-batch-version-text))
  0)

;;;; Entry Points

(defun org-canvas-batch--run (parsed)
  "Set up the course PARSED names and run its command; return the status."
  (let ((command (plist-get parsed :command)))
    (if (plist-get parsed :help)
        (progn (princ (org-canvas-batch--help-text)) 0)
      (unless (nth 6 command)
        (org-canvas-batch-setup (plist-get parsed :directory)))
      (funcall (nth 1 command) parsed))))

(defun org-canvas-batch-main (args)
  "Run the org-canvas command line ARGS and return its exit status.
ARGS are the words after the program name; see
`org-canvas-batch--help-text'.  The status is 0 when the command
succeeded, 1 when it reported drift, validation errors or a failed
item, 2 for a usage error and 3 when an error stopped it.  Error text
is redacted before it is printed."
  (condition-case err
      (org-canvas-batch--run (org-canvas-batch-parse-args args))
    (org-canvas-batch-usage-error
     (message "org-canvas: %s\nRun with --help for usage." (cadr err))
     2)
    (error
     (org-canvas--user-message "org-canvas: %s" (error-message-string err))
     3)))

;;;###autoload
(defun org-canvas-batch ()
  "Run the command line left after -f org-canvas-batch, then exit.
Consume `command-line-args-left', so Emacs does not visit the
arguments as files, and exit with `org-canvas-batch-main''s status."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (kill-emacs (org-canvas-batch-main args))))

(provide 'org-canvas-batch)
;;; org-canvas-batch.el ends here
