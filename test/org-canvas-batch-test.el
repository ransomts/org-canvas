;;; org-canvas-batch-test.el --- Tests for the batch entry point  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-batch' is the one-line command line a batch Emacs runs
;; (issue #416).  Every command it dispatches to is mocked with
;; `cl-letf', so no spec reaches the network, and `kill-emacs' is
;; mocked wherever the entry point itself runs.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-batch)

(defmacro test-batch--with-temp-dir (var &rest body)
  "Bind VAR to a fresh temporary directory around BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "org-canvas-batch" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defmacro test-batch--quietly (messages &rest body)
  "Run BODY collecting each `message' into the list MESSAGES names."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message)
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) ,messages))))
     ,@body))

(defvar test-batch--status nil
  "The status `test-batch--run' last saw.")

(defun test-batch--run (args)
  "Return the exit status of ARGS, the course setup mocked away."
  (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore))
    (with-output-to-string
      (setq test-batch--status (org-canvas-batch-main args)))
    test-batch--status))

;;;; Load path

(describe "org-canvas-batch-ensure-load-path"
  (it "adds the directories ORG_CANVAS_LOAD_PATH names first"
    (let ((load-path '("/existing"))
          (process-environment
           (cons (concat "ORG_CANVAS_LOAD_PATH=/a" path-separator "/b")
                 process-environment)))
      (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "/x/plz.el")))
        (let ((added (org-canvas-batch-ensure-load-path)))
          (expect (member "/a" load-path) :to-be-truthy)
          (expect (member "/b" load-path) :to-be-truthy)
          (expect (member "/a" added) :to-be-truthy)
          (expect (car (last load-path)) :to-equal "/existing")))))

  (it "leaves package.el and the build directories alone when plz is found"
    (let ((load-path nil) (tried nil))
      (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "/x/plz.el"))
                ((symbol-function 'org-canvas-batch--try-package-initialize)
                 (lambda () (setq tried t))))
        (org-canvas-batch-ensure-load-path)
        (expect tried :not :to-be-truthy))))

  (it "tries package.el, then scans straight and elpa, when plz is missing"
    (test-batch--with-temp-dir home
      (make-directory (expand-file-name "straight/build/plz" home) t)
      (make-directory (expand-file-name "elpa/transient-0.7" home) t)
      (make-directory (expand-file-name "elpa/.hidden" home) t)
      (let ((load-path nil) (tried nil) (user-emacs-directory home))
        (cl-letf (((symbol-function 'locate-library) #'ignore)
                  ((symbol-function 'org-canvas-batch--try-package-initialize)
                   (lambda () (setq tried t))))
          (org-canvas-batch-ensure-load-path)
          (expect tried :to-be-truthy)
          (expect (member (expand-file-name "straight/build/plz" home) load-path)
                  :to-be-truthy)
          (expect (member (expand-file-name "elpa/transient-0.7" home) load-path)
                  :to-be-truthy)
          (expect (cl-some (lambda (d) (string-match-p "\\.hidden" d)) load-path)
                  :not :to-be-truthy)))))

  (it "reads no directory that does not exist"
    (expect (org-canvas-batch--subdirectories "/no/such/dir/anywhere") :to-be nil))

  (it "reports package.el's activation, or nil when it fails"
    (cl-letf (((symbol-function 'package-initialize) #'ignore))
      (expect (org-canvas-batch--try-package-initialize) :to-be t))
    (cl-letf (((symbol-function 'package-initialize)
               (lambda (&rest _) (error "Broken archive"))))
      (expect (org-canvas-batch--try-package-initialize) :to-be nil)))

  (it "ignores an empty ORG_CANVAS_LOAD_PATH"
    (let ((process-environment (cons "ORG_CANVAS_LOAD_PATH=" process-environment)))
      (expect (org-canvas-batch--env-directories) :to-be nil))))

;;;; Setup

(describe "org-canvas-batch-setup"
  (it "loads the course's credentials and sets the batch variables"
    (test-batch--with-temp-dir dir
      (with-temp-file (expand-file-name "org-canvas-credentials.el" dir)
        (insert ";;; -*- lexical-binding: t; -*-\n"
                "(setq org-canvas-course-id \"4242\" org-canvas-read-only t)\n"))
      (with-org-canvas-course-globals
        (let ((find-file-visit-truename nil)
              (default-directory "/"))
          (let ((loaded (org-canvas-batch-setup dir)))
            (expect loaded :to-equal
                    (expand-file-name "org-canvas-credentials.el" dir))
            (expect org-canvas-course-id :to-equal "4242")
            (expect org-canvas-read-only :to-be t)
            (expect org-canvas-directory :to-equal dir)
            (expect default-directory :to-equal dir)
            (expect find-file-visit-truename :to-be t))))))

  (it "keeps an org-canvas-directory the credentials file sets"
    (test-batch--with-temp-dir dir
      (with-temp-file (expand-file-name "org-canvas-credentials.el" dir)
        (insert ";;; -*- lexical-binding: t; -*-\n"
                "(setq org-canvas-directory \"/elsewhere/\")\n"))
      (with-org-canvas-course-globals
        (let ((find-file-visit-truename nil) (default-directory "/"))
          (org-canvas-batch-setup dir)
          (expect org-canvas-directory :to-equal "/elsewhere/")))))

  (it "uses the current directory and returns nil without a credentials file"
    (test-batch--with-temp-dir dir
      (with-org-canvas-course-globals
        (let ((find-file-visit-truename nil) (default-directory dir))
          (expect (org-canvas-batch-setup) :to-be nil)
          (expect org-canvas-directory :to-equal dir)))))

  (it "refuses a directory that does not exist"
    (with-org-canvas-course-globals
      (let ((find-file-visit-truename nil) (default-directory "/"))
        (expect (org-canvas-batch-setup "/no/such/course/") :to-throw 'user-error)))))

;;;; Parsing

(describe "org-canvas-batch-parse-args"
  (it "reads the course directory, dry run and command"
    (let* ((parsed (org-canvas-batch-parse-args
                    '("--" "-C" "/c" "-n" "push" "page:Week 1")))
           (command (car (plist-get parsed :command))))
      (expect (plist-get parsed :directory) :to-equal "/c")
      (expect (plist-get parsed :dry-run) :to-be t)
      (expect command :to-equal "push")
      (expect (plist-get parsed :args) :to-equal '("page:Week 1"))))

  (it "reads --course=DIR and --course DIR"
    (expect (plist-get (org-canvas-batch-parse-args '("--course=/d" "diff")) :directory)
            :to-equal "/d")
    (expect (plist-get (org-canvas-batch-parse-args '("--course" "/e" "diff")) :directory)
            :to-equal "/e"))

  (it "asks for help on --help or no command"
    (expect (plist-get (org-canvas-batch-parse-args '("-h")) :help) :to-be t)
    (expect (plist-get (org-canvas-batch-parse-args '("--help" "diff")) :help) :to-be t)
    (expect (plist-get (org-canvas-batch-parse-args nil) :help) :to-be t))

  (it "leaves options after the command to the command"
    (expect (plist-get (org-canvas-batch-parse-args '("grades" "--download" "Essay"))
                       :args)
            :to-equal '("--download" "Essay")))

  (it "refuses malformed command lines"
    (dolist (args '(("-x" "diff") ("-C") ("frobnicate") ("diff" "extra")
                    ("push") ("validate" "--all" "more")))
      (expect (org-canvas-batch-parse-args args)
              :to-throw 'org-canvas-batch-usage-error)))

  (it "names what a command takes when its arguments are wrong"
    (expect (condition-case err (org-canvas-batch-parse-args '("diff" "x"))
              (org-canvas-batch-usage-error (cadr err)))
            :to-equal "diff takes no arguments")
    (expect (condition-case err (org-canvas-batch-parse-args '("push"))
              (org-canvas-batch-usage-error (cadr err)))
            :to-equal "push takes FEATURE:TITLE...")))

(describe "org-canvas-batch-heading-entry"
  (it "reads a heading by title, a colon in the title kept"
    (expect (org-canvas-batch-heading-entry "assignment:R5: Framework Strengths")
            :to-equal '("assignment" "R5: Framework Strengths" title)))

  (it "reads a heading by Canvas id"
    (expect (org-canvas-batch-heading-entry "assignment#2563805")
            :to-equal '("assignment" "2563805" canvas-id)))

  (it "reads a hyphenated feature and a hash sign in a title"
    (expect (org-canvas-batch-heading-entry "new-quiz:Quiz #3")
            :to-equal '("new-quiz" "Quiz #3" title)))

  (it "refuses an argument naming no feature, pointing at grades"
    (expect (condition-case err (org-canvas-batch-heading-entry "Essay 1")
              (org-canvas-batch-usage-error (cadr err)))
            :to-match "use grades")))

(describe "org-canvas-batch--help-text"
  (it "lists every command and the exit statuses"
    (let ((text (org-canvas-batch--help-text)))
      (dolist (c org-canvas-batch--commands)
        (expect text :to-match (regexp-quote (car c))))
      (expect text :to-match "Exit status"))))

;;;; Dispatch

(describe "org-canvas-batch-main"
  (it "prints the help and exits 0 without touching the course"
    (let ((setup nil))
      (cl-letf (((symbol-function 'org-canvas-batch-setup)
                 (lambda (&rest _) (setq setup t))))
        (let ((out (with-output-to-string
                     (expect (org-canvas-batch-main '("--help")) :to-equal 0))))
          (expect out :to-match "Usage: org-canvas")
          (expect setup :not :to-be-truthy)))))

  (it "sets up the named course before running the command"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas-batch-setup)
                 (lambda (dir) (setq seen dir)))
                ((symbol-function 'org-canvas-diff) (lambda () 0)))
        (expect (org-canvas-batch-main '("-C" "/course" "diff")) :to-equal 0)
        (expect seen :to-equal "/course"))))

  (it "exits 2 on a usage error"
    (let (messages)
      (test-batch--quietly messages
        (expect (test-batch--run '("frobnicate")) :to-equal 2))
      (expect (car messages) :to-match "Unknown command frobnicate")))

  (it "exits 3 on an error, with the text redacted"
    (let (messages)
      (test-batch--quietly messages
        (cl-letf (((symbol-function 'org-canvas-diff)
                   (lambda () (error "Failed: Authorization: Bearer sekrit123456"))))
          (expect (test-batch--run '("diff")) :to-equal 3)))
      (expect (car messages) :to-match "^org-canvas: Failed")
      (expect (car messages) :not :to-match "sekrit123456")))

  (it "prints the grading queue for status"
    (let ((called nil))
      (cl-letf (((symbol-function 'org-canvas-submissions-status)
                 (lambda () (setq called t))))
        (expect (test-batch--run '("status")) :to-equal 0)
        (expect called :to-be t))))

  (it "prints the local overview for overview"
    (cl-letf (((symbol-function 'org-canvas-status)
               (lambda ()
                 (with-current-buffer (get-buffer-create "*canvas-status*")
                   (erase-buffer)
                   (insert "org-canvas Sync Status\n")))))
      (let ((out (with-output-to-string
                   (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore))
                     (expect (org-canvas-batch-main '("overview")) :to-equal 0)))))
        (kill-buffer "*canvas-status*")
        (expect out :to-match "Sync Status"))))

  (it "pulls each grading file, the detail view forced, and exits 1 on a failure"
    (let (calls messages)
      (test-batch--quietly messages
        (cl-letf (((symbol-function 'org-canvas-pull-submissions)
                   (lambda (name download)
                     (push (list name download org-canvas-submissions-default-view)
                           calls)
                     (if (equal name "Bad")
                         (error "No assignment named Bad")
                       (with-current-buffer (get-buffer-create " *grading*")
                         (setq buffer-file-name "/g/Essay.org")
                         (current-buffer))))))
          (expect (test-batch--run '("grades" "Essay")) :to-equal 0)
          (expect (test-batch--run '("grades" "--download" "Essay" "Bad"))
                  :to-equal 1)
          (with-current-buffer " *grading*" (setq buffer-file-name nil))
          (kill-buffer " *grading*")))
      (expect (reverse (mapcar (lambda (c) (list (car c) (and (cadr c) t) (nth 2 c)))
                               calls))
              :to-equal '(("Essay" nil detail) ("Essay" t detail) ("Bad" t detail)))
      (expect (cl-some (lambda (m) (string-match-p "grades 'Bad' failed" m)) messages)
              :to-be-truthy)
      (expect (cl-some (lambda (m) (string-match-p "Grading file: /g/Essay.org" m))
                       messages)
              :to-be-truthy)))

  (it "refuses grades with nothing but --download"
    (let (messages)
      (test-batch--quietly messages
        (expect (test-batch--run '("grades" "--download")) :to-equal 2))))

  (it "says pull-queue is not available until #415 defines it"
    (let (messages)
      (cl-letf (((symbol-function 'org-canvas-submissions-pull-queue) nil))
        (test-batch--quietly messages
          (expect (test-batch--run '("pull-queue")) :to-equal 2)))
      (expect (car messages) :to-match "pull-queue is not available")))

  (it "runs pull-queue once it is defined"
    (let ((called nil))
      (cl-letf (((symbol-function 'org-canvas-submissions-pull-queue)
                 (lambda () (setq called t))))
        (expect (test-batch--run '("pull-queue")) :to-equal 0)
        (expect called :to-be t))))

  (it "pulls named headings and exits 1 when one failed"
    (let ((entries nil) (outcome 'pulled))
      (cl-letf (((symbol-function 'org-canvas-pull-headings)
                 (lambda (e) (setq entries e)
                   (list (list :outcome 'pulled) (list :outcome outcome)))))
        (expect (test-batch--run '("pull" "page:Week 1" "assignment#77")) :to-equal 0)
        (expect entries :to-equal '(("page" "Week 1" title)
                                    ("assignment" "77" canvas-id)))
        (setq outcome 'failed)
        (expect (test-batch--run '("pull" "page:Week 1" "assignment#77")) :to-equal 1))))

  (it "pushes named headings, as a dry run under --dry-run"
    (let ((dry nil) (outcome 'synced))
      (cl-letf (((symbol-function 'org-canvas-sync-headings)
                 (lambda (_e) (setq dry org-canvas--dry-run)
                   (list (list :outcome outcome)))))
        (expect (test-batch--run '("-n" "push" "page:Week 1")) :to-equal 0)
        (expect dry :to-be t)
        (setq outcome 'failed)
        (expect (test-batch--run '("push" "page:Week 1")) :to-equal 1)
        (expect dry :to-be nil))))

  (it "pushes each grading file's sent comment changes, exiting 1 on one unsent (#425)"
    (let ((calls nil) (refused 0))
      (cl-letf (((symbol-function 'org-canvas-push-submission-comment-edits)
                 (lambda (name) (push (list name org-canvas--dry-run) calls)
                   (list :edited 1 :deleted 0 :refused refused :failed 0 :dry-run 0))))
        (expect (test-batch--run '("-n" "push-comments" "Essay" "2573836")) :to-equal 0)
        (expect (reverse calls) :to-equal '(("Essay" t) ("2573836" t)))
        (setq refused 1 calls nil)
        (expect (test-batch--run '("push-comments" "Essay")) :to-equal 1)
        (expect calls :to-equal '(("Essay" nil))))))

  (it "exits with the drift report's verdict"
    (let ((total 0))
      (cl-letf (((symbol-function 'org-canvas-diff) (lambda () total)))
        (expect (test-batch--run '("diff")) :to-equal 0)
        (setq total 3)
        (expect (test-batch--run '("diff")) :to-equal 1))))

  (it "exits with validation's verdict, holding nothing back under --all"
    (let ((errors 0) (all-seen 'unset))
      (cl-letf (((symbol-function 'org-canvas-validate)
                 (lambda (_verbose all) (setq all-seen all) errors)))
        (expect (test-batch--run '("validate")) :to-equal 0)
        (expect all-seen :to-be nil)
        (setq errors 2)
        (expect (test-batch--run '("validate" "--all")) :to-equal 1)
        (expect all-seen :to-be t))))

  (it "refuses validate with another argument"
    (let (messages)
      (test-batch--quietly messages
        (expect (test-batch--run '("validate" "--verbose")) :to-equal 2))))

  (it "exits 1 when a full sync failed an item, and dry-runs under -n"
    (let ((fail 0) (dry nil))
      (cl-letf (((symbol-function 'org-canvas-sync)
                 (lambda () (setq dry org-canvas--dry-run)
                   (list :success 1 :fail fail))))
        (expect (test-batch--run '("-n" "sync")) :to-equal 0)
        (expect dry :to-be t)
        (setq fail 1)
        (expect (test-batch--run '("sync")) :to-equal 1)
        (expect dry :to-be nil)))))

;;;; Entry point

(describe "org-canvas-batch"
  (it "consumes the command line and exits with its status"
    (let ((command-line-args-left '("--" "diff"))
          (status nil))
      (cl-letf (((symbol-function 'kill-emacs) (lambda (s) (setq status s)))
                ((symbol-function 'org-canvas-batch-setup) #'ignore)
                ((symbol-function 'org-canvas-diff) (lambda () 2)))
        (org-canvas-batch)
        (expect status :to-equal 1)
        (expect command-line-args-left :to-be nil)))))

(provide 'org-canvas-batch-test)
;;; org-canvas-batch-test.el ends here
