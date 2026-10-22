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
;; command line with subcommands (status, grades, gradebook, pull,
;; pull-all, push, publish, delete, diff, adopt, show, validate,
;; sync ...), which `scripts/org-canvas' wraps:
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
override; then the course settings file there, if any
\(`org-canvas-load-course-config', issue #499).
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
    (org-canvas-load-course-config dir)
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
    ("reports" org-canvas-batch--cmd-reports 0 1 "[--save]"
     "Print every column's Similarity and AI Writing reports (#437).")
    ("pull-queue" org-canvas-batch--cmd-pull-queue 0 0 ""
     "Pull every column the grading queue says needs it (#415).")
    ("push-comments" org-canvas-batch--cmd-push-comments 1 nil "ASSIGNMENT..."
     "Push only the sent comments edited or marked DELETE (#425).")
    ("push-grades" org-canvas-batch--cmd-push-grades 1 4
     "ASSIGNMENT [--only ID,ID] [--dry-run]"
     "Push a grading file, or only the named students' rows (#441).")
    ("post" org-canvas-batch--cmd-post 1 nil "--queue | ASSIGNMENT..."
     "Post columns' grades by id or name, or every post row (#495).")
    ("take" org-canvas-batch--cmd-take 2 nil "FILE USER-ID..."
     "Resolve the students' CONFLICTs by taking Canvas's grade (#496).")
    ("keep" org-canvas-batch--cmd-keep 2 nil "FILE USER-ID..."
     "Resolve the students' CONFLICTs by keeping the typed grade (#496).")
    ("export-comments" org-canvas-batch--cmd-export-comments 1 2 "FILE [OUT.json]"
     "Write a grading file's comments as JSON, to OUT or stdout (#438).")
    ("import-comments" org-canvas-batch--cmd-import-comments 2 3
     "[--posted] FILE IN.json"
     "Write IN.json's comment text into a grading file; exit 1 on a miss.")
    ("check-comments" org-canvas-batch--cmd-check-comments 1 nil "FILE..."
     "Check grading files' comments; exit 1 on errors (#438).")
    ("draft-late-notes" org-canvas-batch--cmd-draft-late-notes 1 nil
     "[--hold ID,ID] FILE..."
     "Draft late rows' late-policy comments; exit 1 on a skip (#484).")
    ("pull-all" org-canvas-batch--cmd-pull-all 0 0 ""
     "Import the whole course; exit 1 if anything failed (#458).")
    ("gradebook" org-canvas-batch--cmd-gradebook 0 0 ""
     "Pull gradebook.org; print its Sections table and the course's scores.")
    ("verify" org-canvas-batch--cmd-verify 1 nil
     "[--hidden] ASSIGNMENT... | announcements:TITLE..."
     "Read pushed columns or announcements back; exit 1 on a difference.")
    ("apply-ruling" org-canvas-batch--cmd-apply-ruling 2 2 "FILE RULING.json"
     "Apply RULING.json's rulings to listed students; exit 1 on a miss (#448).")
    ("excuse" org-canvas-batch--cmd-excuse 1 3 "[--all] [--overwrite] FILE"
     "Excuse the students with no submission, or --all of them (#501).")
    ("import-grades" org-canvas-batch--cmd-import-grades 2 3
     "[--posted] FILE IN.json"
     "Write IN.json's scores, notes and comments; exit 1 on a miss (#483).")
    ("pull" org-canvas-batch--cmd-pull 1 nil "FEATURE or FEATURE:TITLE..."
     "Pull a whole feature, or named headings, from Canvas.")
    ("push" org-canvas-batch--cmd-push 1 nil
     "[--conflict push|pull|skip] FEATURE:TITLE..."
     "Push named headings; --conflict resolves conflicts (#470).")
    ("publish" org-canvas-batch--cmd-publish 1 nil "FEATURE:TITLE..."
     "Publish named headings, sending only the flag (#466).")
    ("unpublish" org-canvas-batch--cmd-unpublish 1 nil "FEATURE:TITLE..."
     "Unpublish named headings, sending only the flag (#466).")
    ("delete" org-canvas-batch--cmd-delete 1 nil "[--yes] FEATURE:TITLE..."
     "Delete named headings from Canvas; --yes confirms (#525).")
    ("diff" org-canvas-batch--cmd-diff 0 0 ""
     "Print the drift report; exit 1 on drift.")
    ("adopt" org-canvas-batch--cmd-adopt 1 nil
     "[--body] [--overrides] --all | FEATURE:TITLE..."
     "Write what CHANGED rows name into Org, nothing sent (#498).")
    ("show" org-canvas-batch--cmd-show 1 nil "FEATURE:TITLE..."
     "Print Canvas's version of named headings; writes nothing (#500).")
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
   "\n\nA bare feature name (pull announcements) pulls that whole feature,\n"
   "adopting items made in the web UI; FEATURE:TITLE names a heading by\n"
   "its exact title (assignment:Essay 1);\n"
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

(defun org-canvas-batch--feature-pull-fn (name)
  "Return the whole-feature pull function NAME names, or signal usage.
NAME is a module's name as the pull commands spell it, singular or
plural (`announcements', `assignment-groups', `new-quiz'), matched
against the whole-feature pulls the pull tiers hold.  The usage error
names every feature."
  (let ((want (org-canvas--singularize (downcase name))))
    (or (cl-loop for entry in (apply #'append org-canvas--pull-tiers)
                 for fn = (car entry)
                 for suffix = (replace-regexp-in-string
                               "\\`org-canvas-pull-" "" (symbol-name fn))
                 when (string= want (org-canvas--singularize suffix))
                 return fn)
        (org-canvas-batch--usage
         "%s is not a feature with a pull; the features are: %s"
         name
         (mapconcat (lambda (entry)
                     (replace-regexp-in-string
                      "\\`org-canvas-pull-" "" (symbol-name (car entry))))
                    (apply #'append org-canvas--pull-tiers) ", ")))))

;;;; Subcommands

(defun org-canvas-batch--failed-count (results)
  "Return how many of RESULTS, heading result plists, failed."
  (cl-count 'failed results :key (lambda (r) (plist-get r :outcome))))

(defun org-canvas-batch--cmd-status (_parsed)
  "Print the grading queue; return 0."
  (org-canvas-submissions-status)
  0)

(defun org-canvas-batch--cmd-reports (parsed)
  "Print the document processor reports; return 0.
They are also saved when PARSED's arguments are --save."
  (let ((args (plist-get parsed :args)))
    (unless (member args '(nil ("--save")))
      (org-canvas-batch--usage "reports takes [--save]"))
    (org-canvas-submissions-reports (and args t))
    0))

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

(defun org-canvas-batch--cmd-gradebook (_parsed)
  "Pull gradebook.org and print the class standing; return 0.
The Sections table, then the course's mean and median scores
\(`org-canvas-gradebook-summary-text', issue #451)."
  (princ (org-canvas-gradebook-summary-text (org-canvas-pull-gradebook)))
  0)

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

(defun org-canvas-batch--push-grades-args (args)
  "Read push-grades ARGS into (ASSIGNMENT ONLY DRY-RUN).
ONLY is the list of ids after --only, split at commas; DRY-RUN is
non-nil for --dry-run or -n.  Anything else is a usage error."
  (let ((assignment nil) (only nil) (dry-run nil))
    (while args
      (let ((arg (pop args)))
        (cond ((member arg '("-n" "--dry-run")) (setq dry-run t))
              ((equal arg "--only")
               (setq only (split-string (or (pop args) "") "," t "[ \t]+"))
               (unless (and only (cl-every (lambda (id) (string-match-p "\\`[0-9]+\\'" id))
                                           only))
                 (org-canvas-batch--usage "--only takes Canvas user ids, ID,ID")))
              ((or assignment (string-prefix-p "-" arg))
               (org-canvas-batch--usage "push-grades takes ASSIGNMENT [--only ID,ID] [--dry-run]"))
              (t (setq assignment arg)))))
    (unless assignment
      (org-canvas-batch--usage "push-grades takes ASSIGNMENT [--only ID,ID] [--dry-run]"))
    (list assignment only dry-run)))

(defun org-canvas-batch--push-grades-ok-p (result)
  "Return non-nil when RESULT, a grade push's plist, left nothing behind.
Grades that did not land, conflicts, ids with no row or nothing to
send, and sent-comment changes not sent all count against it."
  (and result
       (not (memq (plist-get result :state) '(failed unconfirmed)))
       (zerop (+ (plist-get result :conflicts) (plist-get result :refused)
                 (plist-get result :failed)
                 (or (plist-get result :errored) 0)))
       (null (plist-get result :missing))
       (null (plist-get result :unchanged))))

(defun org-canvas-batch--cmd-push-grades (parsed)
  "Push the grading file PARSED names, or only its --only rows.
Grades are never posted.  A --dry-run, before the command or after
it, sends nothing and prints the fields each student's row would
send.  Return 1 if anything was left behind
\(`org-canvas-batch--push-grades-ok-p')."
  (let* ((args (org-canvas-batch--push-grades-args (plist-get parsed :args)))
         (org-canvas--dry-run (or org-canvas--dry-run (nth 2 args)
                                  (plist-get parsed :dry-run)))
         (result (org-canvas-push-submission-grades (nth 0 args) nil (nth 1 args))))
    (dolist (row (plist-get result :would-send))
      (princ (format "%s\n" (plist-get row :line))))
    (if (org-canvas-batch--push-grades-ok-p result) 0 1)))

(defun org-canvas-batch--cmd-post (parsed)
  "Post the grades of the columns PARSED names, or with --queue every post row.
Grades are posted only for the columns named, by Canvas id or exact
name, or under --queue for those the grading queue marks post.  A
--dry-run sends nothing.  Return 1 if any column was not posted."
  (let* ((args (plist-get parsed :args))
         (org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run)))
         (outcomes (cond ((equal args '("--queue"))
                          (org-canvas-submissions-post-queue))
                         ((member "--queue" args)
                          (org-canvas-batch--usage "post takes --queue alone, or ASSIGNMENT..."))
                         (t (org-canvas-post-grades args)))))
    (dolist (outcome outcomes)
      (princ (format "%s %s: %s\n" (plist-get outcome :id)
                     (or (plist-get outcome :name) "")
                     (or (plist-get outcome :message) (plist-get outcome :state)))))
    (if (cl-some (lambda (o) (eq (plist-get o :state) 'failed)) outcomes) 1 0)))

(defun org-canvas-batch--resolve-conflicts (parsed take)
  "Resolve the CONFLICTs PARSED names, taking Canvas's grade when TAKE.
The first argument is the grading file, the rest Canvas user ids.
Return 1 if an id had no heading or no CONFLICT."
  (let* ((args (plist-get parsed :args))
         (result (org-canvas-submissions-resolve-conflicts
                  (car args) (cdr args) take)))
    (dolist (id (plist-get result :missing))
      (message "No heading for user %s" id))
    (dolist (id (plist-get result :unmarked))
      (message "No CONFLICT on user %s" id))
    (if (or (plist-get result :missing) (plist-get result :unmarked)) 1 0)))

(defun org-canvas-batch--cmd-take (parsed)
  "Take Canvas's grade on the CONFLICT rows PARSED names; return the status."
  (org-canvas-batch--resolve-conflicts parsed t))

(defun org-canvas-batch--cmd-keep (parsed)
  "Keep the typed grade on the CONFLICT rows PARSED names; return the status."
  (org-canvas-batch--resolve-conflicts parsed nil))

(defun org-canvas-batch--cmd-export-comments (parsed)
  "Write the comments of the grading file PARSED names as JSON; return 0.
The second argument names the output file; without it the JSON is
printed."
  (let ((args (plist-get parsed :args)))
    (org-canvas-submissions-export-comments (nth 0 args) (or (nth 1 args) "-"))
    0))

(defun org-canvas-batch--cmd-import-comments (parsed)
  "Import the JSON PARSED names into its grading file, a dry run on --dry-run.
--posted lets changes under a posted grade through.  Return 1 if a key
matched nothing or a change was kept back."
  (let* ((args (plist-get parsed :args))
         (allow-posted (member "--posted" args))
         (rest (remove "--posted" args))
         (org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run))))
    (unless (= (length rest) 2)
      (org-canvas-batch--usage "import-comments takes [--posted] FILE IN.json"))
    (let ((result (org-canvas-submissions-import-comments
                   (nth 0 rest) (expand-file-name (nth 1 rest)) (and allow-posted t))))
      (if (or (plist-get result :unmatched) (plist-get result :skipped)) 1 0))))

(defun org-canvas-batch--cmd-check-comments (parsed)
  "Check the comments of each grading file PARSED names.
Return 1 if any check found an error."
  (let ((errors (mapcar (lambda (file)
                          (org-canvas-submissions-comment-check-errors
                           (org-canvas-submissions-check-comments file)))
                        (plist-get parsed :args))))
    (if (cl-every #'zerop errors) 0 1)))

(defun org-canvas-batch--draft-late-notes-args (args)
  "Read draft-late-notes ARGS into (HOLD . FILES).
HOLD is the list after --hold, split at commas; FILES the rest."
  (let ((hold nil) (files nil))
    (while args
      (let ((arg (pop args)))
        (cond ((equal arg "--hold")
               (setq hold (split-string (or (pop args) "") "," t "[ \t]+"))
               (unless hold
                 (org-canvas-batch--usage "--hold takes user ids or names, ID,ID")))
              (t (push arg files)))))
    (unless files
      (org-canvas-batch--usage "draft-late-notes takes [--hold ID,ID] FILE..."))
    (cons hold (nreverse files))))

(defun org-canvas-batch--cmd-draft-late-notes (parsed)
  "Draft the late-policy comments of each grading file PARSED names.
A --dry-run writes nothing and shows each note.  Return 1 if a
student was skipped or carries a late note with other numbers."
  (let* ((args (org-canvas-batch--draft-late-notes-args (plist-get parsed :args)))
         (org-canvas--dry-run (or org-canvas--dry-run (plist-get parsed :dry-run)))
         (results (mapcar (lambda (file)
                            (org-canvas-submissions-draft-late-notes file (car args)))
                          (cdr args))))
    (if (cl-some (lambda (r) (or (plist-get r :skipped) (plist-get r :changed)))
                 results)
        1 0)))

(defun org-canvas-batch--announcement-arg-p (arg)
  "Return non-nil when ARG names an announcement heading, FEATURE:TITLE."
  (string-match-p "\\`announcements?[:#]" arg))

(defun org-canvas-batch--verify-announcements (args)
  "Verify the announcement headings ARGS name; return non-nil if all agree.
Nil ARGS verify nothing and agree."
  (or (null args)
      (cl-every (lambda (r) (zerop (plist-get r :differences)))
                (org-canvas-verify-announcements
                 (mapcar #'org-canvas-batch-heading-entry args)))))

(defun org-canvas-batch--verify-columns (targets hidden)
  "Verify the grading files TARGETS name, HIDDEN as for --hidden.
Return non-nil if every column agrees; nil TARGETS agree."
  (or (null targets)
      (cl-every (lambda (r) (zerop (plist-get r :differences)))
                (org-canvas-submissions-verify targets hidden))))

(defun org-canvas-batch--cmd-verify (parsed)
  "Read back each column or announcement PARSED names and compare it.
A column is compared with its grading file; --hidden counts a posted
submission as a difference (`org-canvas-submissions-verify').  An
announcements:TITLE or announcements#ID argument is compared with its
heading (`org-canvas-verify-announcements', #502).  Return 1 if any
differs."
  (let* ((args (plist-get parsed :args))
         (hidden (and (member "--hidden" args) t))
         (rest (remove "--hidden" args))
         (announcements (seq-filter #'org-canvas-batch--announcement-arg-p rest))
         (targets (seq-remove #'org-canvas-batch--announcement-arg-p rest)))
    (unless rest
      (org-canvas-batch--usage "verify takes %s"
                               (nth 4 (assoc "verify" org-canvas-batch--commands))))
    ;; Both run, so one report does not hide the other.
    (let ((ok-columns (org-canvas-batch--verify-columns targets hidden))
          (ok-announcements (org-canvas-batch--verify-announcements announcements)))
      (if (and ok-columns ok-announcements) 0 1))))

(defun org-canvas-batch--cmd-apply-ruling (parsed)
  "Apply the JSON rulings PARSED names to its grading file.
A dry run on --dry-run.  Return 1 if a user id matched nothing or a
student was kept back."
  (let* ((args (plist-get parsed :args))
         (org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run)))
         (results (org-canvas-submissions-apply-ruling-json
                   (nth 0 args) (expand-file-name (nth 1 args)))))
    (if (cl-some (lambda (r) (or (plist-get r :unmatched) (plist-get r :skipped)))
                 results)
        1 0)))

(defun org-canvas-batch--cmd-excuse (parsed)
  "Excuse students of the grading file PARSED names, a dry run on --dry-run.
Only those with no submission, unless --all; a graded student only
with --overwrite.  Return 0."
  (let* ((args (plist-get parsed :args))
         (rest (cl-remove-if (lambda (a) (member a '("--all" "--overwrite"))) args))
         (org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run))))
    (unless (and (= (length rest) 1) (not (string-prefix-p "-" (car rest))))
      (org-canvas-batch--usage "excuse takes [--all] [--overwrite] FILE"))
    (org-canvas-submissions-excuse (car rest)
                                   (if (member "--all" args) 'all 'unsubmitted)
                                   (and (member "--overwrite" args) t))
    0))

(defun org-canvas-batch--cmd-import-grades (parsed)
  "Import the JSON grades PARSED names into its grading file.
A dry run on --dry-run; --posted lets changes under a posted grade
through.  Return 1 if a key matched nothing or a change was kept back."
  (let* ((args (plist-get parsed :args))
         (rest (remove "--posted" args))
         (org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run))))
    (unless (= (length rest) 2)
      (org-canvas-batch--usage "import-grades takes [--posted] FILE IN.json"))
    (let ((result (org-canvas-submissions-import-grades
                   (nth 0 rest) (expand-file-name (nth 1 rest))
                   (and (member "--posted" args) t))))
      (if (or (plist-get result :unmatched) (plist-get result :skipped)) 1 0))))

(defun org-canvas-batch--cmd-pull (parsed)
  "Pull what PARSED names; return 1 if any pull failed.
An argument holding a colon or hash names headings — FEATURE:TITLE,
FEATURE#ID — pulled one by one.  A bare feature name pulls the whole
feature, upserting the Canvas-made items it holds (issue #511): the
way an announcement written in the web UI is adopted from batch
without pulling the whole course.  Features run first, so a heading
named after one is found once its items exist."
  (let ((entries nil) (features nil) (failed 0))
    (dolist (arg (plist-get parsed :args))
      (if (string-match-p "[:#]" arg)
          (push (org-canvas-batch-heading-entry arg) entries)
        (push arg features)))
    (dolist (name (nreverse features))
      (let ((fn (org-canvas-batch--feature-pull-fn name)))
        (condition-case err
            (funcall fn)
          (error
           (cl-incf failed)
           (message "Pulling %s failed: %s" name (error-message-string err))))))
    (when entries
      (setq failed (+ failed
                      (org-canvas-batch--failed-count
                       (org-canvas-pull-headings (nreverse entries))))))
    (if (zerop failed) 0 1)))

(defun org-canvas-batch--cmd-pull-all (_parsed)
  "Import the whole course and print how the pull went.
The pull summary is printed by `org-canvas-pull-all' itself; the
closing line follows it.  Return 1 if a content type failed or an
item was lost to an error.  A type the enrolment may not read is a
skip, expected of a Designer or TA, and leaves the status 0."
  (let ((counters (org-canvas-pull-all)))
    (princ (format "%s\n" (org-canvas--pull-completion-line counters)))
    (if (or (> (plist-get counters :fail) 0)
            (org-canvas--pull-summary-records-of-kind 'error))
        1 0)))

(defun org-canvas-batch--conflict-strategy (value)
  "Return the strategy symbol for VALUE, the word after --conflict.
VALUE is \"push\", \"pull\" or \"skip\"; anything else, or nil when
--conflict ended the line, is a usage error."
  (cond
   ((member value '("push" "pull" "skip")) (intern value))
   (value (org-canvas-batch--usage
           "--conflict takes push, pull or skip, not %s" value))
   (t (org-canvas-batch--usage "--conflict needs push, pull or skip"))))

(defun org-canvas-batch--push-args (args)
  "Read push ARGS into (STRATEGY . HEADINGS).
STRATEGY is the symbol --conflict STRATEGY or --conflict=STRATEGY
named, anywhere among ARGS, or nil without one; HEADINGS are the
other arguments, in order.  No heading is a usage error."
  (let ((strategy nil) (headings nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((equal arg "--conflict")
          (setq strategy (org-canvas-batch--conflict-strategy (pop args))))
         ((string-prefix-p "--conflict=" arg)
          (setq strategy (org-canvas-batch--conflict-strategy
                          (substring arg (length "--conflict=")))))
         (t (push arg headings)))))
    (unless headings
      (org-canvas-batch--usage "push takes %s"
                               (nth 4 (assoc "push" org-canvas-batch--commands))))
    (cons strategy (nreverse headings))))

(defun org-canvas-batch--cmd-push (parsed)
  "Push the headings PARSED names; return 1 if any failed.
A --conflict among the arguments binds `org-canvas-conflict-strategy'
for this run: push overwrites Canvas, pull the heading, and skip
leaves the entry alone, as a batch Emacs does without it (#470)."
  (let* ((args (org-canvas-batch--push-args (plist-get parsed :args)))
         (entries (mapcar #'org-canvas-batch-heading-entry (cdr args)))
         (org-canvas-conflict-strategy (or (car args)
                                           org-canvas-conflict-strategy))
         (org-canvas--dry-run (or org-canvas--dry-run
                                  (plist-get parsed :dry-run))))
    (if (zerop (org-canvas-batch--failed-count
                (org-canvas-sync-headings entries)))
        0 1)))

(defun org-canvas-batch--set-published-one (entry published)
  "Set PUBLISHED on the heading ENTRY names; return t, or nil on failure.
ENTRY is what `org-canvas-batch-heading-entry' returns.  A refusal or an
error is printed, redacted, and the next heading still runs."
  (condition-case err
      (progn (org-canvas-set-published (nth 0 entry) (nth 1 entry)
                                       published (nth 2 entry))
             t)
    (error
     (org-canvas--user-message "%s %s failed: %s"
                               (if published "publish" "unpublish")
                               (nth 1 entry) (error-message-string err))
     nil)))

(defun org-canvas-batch--set-published (parsed published)
  "Set PUBLISHED on each heading PARSED names; return 1 if any failed.
A --dry-run sends nothing and writes nothing."
  (let ((entries (mapcar #'org-canvas-batch-heading-entry
                         (plist-get parsed :args)))
        (org-canvas--dry-run (or org-canvas--dry-run
                                 (plist-get parsed :dry-run))))
    (if (cl-every #'identity
                  (mapcar (lambda (entry)
                            (org-canvas-batch--set-published-one entry published))
                          entries))
        0 1)))

(defun org-canvas-batch--cmd-publish (parsed)
  "Publish the headings PARSED names; return 1 if any failed."
  (org-canvas-batch--set-published parsed t))

(defun org-canvas-batch--cmd-unpublish (parsed)
  "Unpublish the headings PARSED names; return 1 if any failed."
  (org-canvas-batch--set-published parsed nil))

(defun org-canvas-batch--delete-args (args)
  "Read delete ARGS into (CONFIRMED DRY-RUN . HEADINGS).
CONFIRMED is non-nil for --yes or -y and DRY-RUN for --dry-run or -n,
anywhere among ARGS; HEADINGS are the other arguments, in order.  No
heading is a usage error."
  (let ((confirmed nil) (dry-run nil) (headings nil))
    (dolist (arg args)
      (cond ((member arg '("-y" "--yes")) (setq confirmed t))
            ((member arg '("-n" "--dry-run")) (setq dry-run t))
            (t (push arg headings))))
    (unless headings
      (org-canvas-batch--usage "delete takes %s"
                               (nth 4 (assoc "delete" org-canvas-batch--commands))))
    (cons confirmed (cons dry-run (nreverse headings)))))

(defun org-canvas-batch--cmd-delete (parsed)
  "Delete the headings PARSED names from Canvas; return 1 if any failed.
--yes answers the delete confirmation, binding
`org-canvas-delete-confirmed' and nothing else, so every other
question still stands; without it, and without --dry-run, nothing is
deleted and the command is a usage error.  A --dry-run, before the
command or after it, sends nothing and prints what would go."
  (let* ((args (org-canvas-batch--delete-args (plist-get parsed :args)))
         (entries (mapcar #'org-canvas-batch-heading-entry (cddr args)))
         (org-canvas--dry-run (or org-canvas--dry-run (nth 1 args)
                                  (plist-get parsed :dry-run)))
         (org-canvas-delete-confirmed (car args)))
    (unless (or org-canvas-delete-confirmed org-canvas--dry-run)
      (org-canvas-batch--usage
       "delete needs --yes to confirm, or --dry-run to see what would go"))
    (let ((results (org-canvas-delete-headings entries)))
      (dolist (r results)
        (when (eq (plist-get r :outcome) 'dry-run)
          (princ (format "Would delete %s '%s' (%s %s)\n"
                         (plist-get r :feature) (plist-get r :target)
                         (plist-get r :id-property) (plist-get r :id)))))
      (if (cl-every (lambda (r) (memq (plist-get r :outcome) '(deleted dry-run)))
                    results)
          0 1))))

(defun org-canvas-batch--cmd-diff (_parsed)
  "Print the drift report; return 1 if anything drifted."
  (if (> (org-canvas-diff) 0) 1 0))

(defun org-canvas-batch--adopt-args (args)
  "Read adopt ARGS into (SELECTORS BODY OVERRIDES).
SELECTORS is `all' for --all, else the headings the other arguments
name; BODY and OVERRIDES are non-nil for --body and --overrides.
--all beside a heading, or neither, is a usage error."
  (let* ((body (and (member "--body" args) t))
         (overrides (and (member "--overrides" args) t))
         (all (member "--all" args))
         (headings (cl-set-difference args '("--body" "--overrides" "--all")
                                      :test #'equal)))
    (when (or (and all headings) (not (or all headings)))
      (org-canvas-batch--usage "adopt takes %s"
                               (nth 4 (assoc "adopt" org-canvas-batch--commands))))
    (list (if all 'all (mapcar #'org-canvas-batch-heading-entry headings))
          body overrides)))

(defun org-canvas-batch--cmd-adopt (parsed)
  "Adopt the drift of the rows PARSED names; return 1 if any is left.
A --dry-run writes nothing and names what would be written; nothing
is ever sent (`org-canvas-diff-adopt-drifts')."
  (let* ((args (org-canvas-batch--adopt-args (plist-get parsed :args)))
         (org-canvas--dry-run (or org-canvas--dry-run (plist-get parsed :dry-run)))
         (outcomes (org-canvas-diff-adopt-drifts (nth 0 args)
                                                 :body (nth 1 args)
                                                 :overrides (nth 2 args))))
    (if (cl-every (lambda (o) (memq (plist-get o :outcome)
                                    org-canvas--drift-done-outcomes))
                  outcomes)
        0 1)))

(defun org-canvas-batch--cmd-show (parsed)
  "Print the Canvas version of each heading PARSED names; return 0."
  (org-canvas-show-headings (mapcar #'org-canvas-batch-heading-entry
                                    (plist-get parsed :args)))
  0)

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
