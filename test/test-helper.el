;;; test-helper.el --- Test helper for org-canvas tests  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Common fixtures, macros, and utilities for org-canvas Buttercup tests.
;;
;; Code coverage is handled by Eldev's undercover plugin.
;; Run with: eldev test --undercover
;; See coverage report: coverage/lcov.info

;;; Code:

(require 'cl-lib)

;;;; Credentials Isolation
;;
;; `org-canvas-core-config' ends with `(require 'org-canvas-credentials
;; nil t)', which loads the developer's real credentials file whenever
;; one sits on the load path — and lisp/ is on it, gitignored file and
;; all.  Tests used to run with those values, which is how an unmocked
;; fallback branch once made a live call with real credentials (issue
;; #138; the network guard below was the reaction).  Providing the
;; feature first turns that `require' into a no-op, so the file is never
;; read, and the dummies below are what every defcustom then keeps:
;; `defcustom' leaves an already-bound variable alone, and they are set
;; before `org-canvas-core' is required so no load-time code sees
;; anything else.
;;
;; `org-canvas-directory' gets a per-process temp directory rather than
;; a fixed dummy.  Logging lands under it: with a shared real directory
;; tests wrote one log file and created `.#org-canvas.log' locks, which
;; collide when the pre-push hook runs the Emacs 29 and Emacs 30 jobs in
;; parallel and one job aborts.  The directory itself is rebound rather
;; than `org-canvas-log-destination' because a test asserts that the
;; defcustom default remains `both'.
;;
;; `org-canvas--api-token' falls through to `auth-source' when the token
;; is empty, so `auth-sources' is emptied too: a spec that binds the
;; token to "" must never reach the developer's ~/.authinfo.  Specs
;; that exercise the resolver mock `auth-source-search' with `cl-letf'.

(defvar test-org-canvas-base-url "https://test.canvas.example.com"
  "Test Canvas base URL.")

(defvar test-org-canvas-api-token "test-token-12345"
  "Test API token.")

(defvar test-org-canvas-course-id "99999"
  "Test course ID.")

(defvar org-canvas-directory)
(defvar org-canvas-base-url)
(defvar org-canvas-api-token)
(defvar org-canvas-course-id)

(provide 'org-canvas-credentials)
(setq org-canvas-directory (file-name-as-directory
                            (make-temp-file "org-canvas-test-" t))
      org-canvas-base-url test-org-canvas-base-url
      org-canvas-api-token test-org-canvas-api-token
      org-canvas-course-id test-org-canvas-course-id)

(require 'auth-source)
(setq auth-sources nil)

;; The whole package, not core alone: a test file run by itself
;; (`eldev test "pattern"') must see what the full suite sees.  Every
;; module registers with the feature and property registries, the
;; sync-at-point and prune commands are generated per module, and
;; assignments call into sections through a `declare-function'; with
;; core alone, 18 of the test files failed in isolation and passed in
;; the suite only because some earlier file had loaded the rest
;; (issue #260).
(require 'org-canvas)

;; Suppress "Non-existent agenda file" prompt in batch mode.
;; Org checks agenda files when opening .org buffers; temp files deleted
;; during test cleanup trigger an interactive prompt that hangs batch runs.
(when noninteractive
  (defun org-check-agenda-file (_file) nil))

;; Lockfiles have no value in a batch suite and cause real harm: a
;; `.#name' symlink appearing while some spec has a buffer dirty can
;; break anything that enumerates the directory (issue #44 — a fixture
;; copy of demo-course/ died on one).  This stops the suite creating
;; its own; a concurrent interactive Emacs can still create them, which
;; is why the dry-run fixture copy also skips them explicitly.
(setq create-lockfiles nil)

;;;; Network Guard
;;
;; The test environment used to load the user's real
;; `org-canvas-credentials.el', so any code path that reached a network
;; primitive without a mock hit the LIVE Canvas API with real
;; credentials (observed 2026-08-10: an unmocked fallback branch made a
;; live call from a test).  Credentials Isolation above now keeps the
;; real values out, but an unmocked call would still leave the process,
;; and the isolation could regress: refuse at the bottom of the stack
;; instead of hoping every test mocks high enough.  Tests that
;; exercise the API internals mock these same
;; symbols with `cl-letf', which replaces the whole function cell —
;; advice included — so they are unaffected.

(defun test-org-canvas--refuse-network (kind &rest detail)
  "Signal an error refusing a real network call from a test.
KIND names the blocked primitive; DETAIL is safe-to-print context
\(never headers, which may carry credentials)."
  (error "Unmocked network call during tests (%s): %S — mock org-canvas-api-request or the primitive with cl-letf"
         kind detail))

(advice-add 'plz :override
            (lambda (method url &rest _)
              (test-org-canvas--refuse-network 'plz method url))
            '((name . test-org-canvas-network-guard)))

(advice-add 'url-retrieve-synchronously :override
            (lambda (url &rest _)
              (test-org-canvas--refuse-network 'url-retrieve-synchronously url))
            '((name . test-org-canvas-network-guard)))

;; Only curl spawns aimed at the network (the PATCH fallback uses
;; `plz-curl-program') are refused; other subprocesses (pandoc, git)
;; pass through untouched.
(advice-add 'call-process :around
            (lambda (orig program &rest args)
              (if (and (boundp 'plz-curl-program)
                       (equal program plz-curl-program))
                  (test-org-canvas--refuse-network 'curl program)
                (apply orig program args)))
            '((name . test-org-canvas-network-guard)))

;;;; Course Globals Guard
;;
;; `org-canvas-init' and `org-canvas-activate-course' `setq' the course
;; settings, and the directory watcher then `set's every registered
;; file-path variable from the new directory.  The `org-canvas-init'
;; specs used to leave all of it behind: two answered every
;; `y-or-n-p' with yes, "adopted course?" included, so every spec that
;; ran after them started read-only and a write outside
;; `with-org-canvas-test-config' was refused only in the full suite
;; (issue #353).  A spec that runs such a command wraps it in
;; `with-org-canvas-course-globals'; the check below fails any spec
;; that leaves one of these variables changed, and restores it so the
;; next spec starts clean.

(require 'buttercup)

(defconst test-org-canvas-course-globals
  '(org-canvas-directory org-canvas-base-url org-canvas-api-token
    org-canvas-course-id org-canvas-read-only
    org-canvas-write-allowlist
    org-canvas--active-course-name)
  "Course settings a spec must leave as it found them.")

(defun test-org-canvas-course-global-vars ()
  "Return the course globals and every registered file-path variable."
  (append test-org-canvas-course-globals
          (mapcar #'car org-canvas--file-var-registry)))

(defmacro with-org-canvas-course-globals (&rest body)
  "Run BODY with the course globals bound to their current values.
A `setq' in BODY, such as `org-canvas-init' makes, then changes only
this binding, and the file-path variables the directory watcher
recomputes are bound too, so nothing survives BODY."
  (declare (indent 0))
  (let ((vars (make-symbol "vars")))
    `(let ((,vars (test-org-canvas-course-global-vars)))
       (cl-progv ,vars (mapcar #'symbol-value ,vars)
         ,@body))))

(defun test-org-canvas-make-registry (&rest entries)
  "Return a fresh registry holding ENTRIES, in the order given.
Each entry is a plist as `org-canvas-register-feature' would store it:
it is given its registry id, the `:feature' mark and a registration
order, unless it names one of those itself.  Pass `:feature nil' for a
file section that only registered property specs (issue #572)."
  (let ((registry (make-hash-table :test 'equal))
        (order (length entries)))
    (dolist (entry entries)
      (let ((entry (copy-sequence entry)))
        (unless (plist-member entry :id)
          (setq entry (plist-put entry :id (org-canvas--feature-id entry))))
        (unless (plist-member entry :feature)
          (setq entry (plist-put entry :feature t)))
        (setq entry (plist-put entry :order order)
              order (1- order))
        (puthash (symbol-name (org-canvas--feature-id entry))
                 entry registry)))
    registry))

(defvar test-org-canvas--course-globals-baseline
  (mapcar (lambda (var) (cons var (default-value var)))
          (test-org-canvas-course-global-vars))
  "Each course global's value when the suite started, as (VAR . VALUE).")

(defun test-org-canvas--check-course-globals ()
  "Fail the current spec when it left a course global changed.
The value is put back first, so one leaking spec fails alone rather
than every spec that runs after it."
  (let (changed)
    (pcase-dolist (`(,var . ,value) test-org-canvas--course-globals-baseline)
      (unless (equal (default-value var) value)
        (push (list var (default-value var)) changed)
        (set-default var value)))
    (when changed
      (buttercup-fail "Spec left course globals changed: %S (issue #353)"
                      (nreverse changed)))))

;; Buttercup has no suite-wide `after-each'; its runner appends each
;; suite's own to this variable's top-level value, so a function here
;; runs after every spec's own cleanup.
(unless (boundp 'buttercup--after-each)
  (error "Buttercup no longer has `buttercup--after-each'; move the guard"))
(add-to-list 'buttercup--after-each #'test-org-canvas--check-course-globals
             t)

;;;; Test Configuration

(defvar test-org-canvas-emacs-30-p (>= emacs-major-version 30)
  "Non-nil if running on Emacs 30 or later.
Some org-mode functions behave differently in Emacs 29.x regarding
heading structure recognition in programmatic buffers.")

(defvar test-org-canvas-request-timeout 5
  "Test timeout in seconds.")

;;;; API Mocking

(defvar test-org-canvas-api-calls nil
  "List of recorded API calls: ((method url data args-plist) ...).
The first three elements (method, url, :data) are preserved for backward
compatibility; the fourth element is the full keyword-argument plist, so
:params and :timeout can be asserted too.")

(defvar test-org-canvas-api-responses nil
  "Alist of (url-pattern . response) for mock responses.")

(defun test-org-canvas-mock-api-request (method url &rest args)
  "Mock API request function that records calls and returns mock responses.
METHOD, URL, and :data occupy the first three slots (backward compatible);
the full ARGS plist is recorded as a fourth slot so :params and :timeout
can be asserted via `test-org-canvas-call-arg'."
  (let ((call (list method url (plist-get args :data) args)))
    (push call test-org-canvas-api-calls)
    ;; Find matching response
    (let ((response nil))
      (cl-loop for (pattern . resp) in test-org-canvas-api-responses
               when (string-match-p pattern url)
               do (setq response resp)
               and return nil)
      (or response '((id . 12345) (name . "Mock Response"))))))

(defun test-org-canvas-api-called-p (method url-pattern)
  "Check if API was called with METHOD and URL matching URL-PATTERN."
  (cl-some (lambda (call)
             (and (eq (car call) method)
                  (string-match-p url-pattern (cadr call))))
           test-org-canvas-api-calls))

(defun test-org-canvas-find-api-call (method url-pattern)
  "Return the most recent recorded call matching METHOD and URL-PATTERN, or nil."
  (cl-find-if (lambda (call)
                (and (eq (car call) method)
                     (string-match-p url-pattern (cadr call))))
              test-org-canvas-api-calls))

(defun test-org-canvas-api-call-data (method url-pattern)
  "Return the :data payload of the matching METHOD/URL-PATTERN call, or nil.
Use to assert request body structure, not merely that a call happened."
  (nth 2 (test-org-canvas-find-api-call method url-pattern)))

(defun test-org-canvas-call-arg (call key)
  "Return keyword KEY (e.g. :params, :timeout) from a recorded CALL."
  (plist-get (nth 3 call) key))

(defun test-org-canvas-count-matches (regexp text)
  "Return how many times REGEXP matches in TEXT."
  (let ((n 0) (start 0))
    (while (string-match regexp text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defun test-org-canvas-api-call-count ()
  "Return the number of API calls made."
  (length test-org-canvas-api-calls))

(defun test-org-canvas-last-api-call ()
  "Return the most recent API call."
  (car test-org-canvas-api-calls))

;;;; Macros

(defmacro with-org-canvas-test-config (&rest body)
  "Execute BODY with test Canvas configuration.
The course is writable (`org-canvas-read-only' nil) whatever the
caller has bound, since the specs written under this macro exercise
pushes (issue #163)."
  (declare (indent 0))
  `(let ((org-canvas-base-url test-org-canvas-base-url)
         (org-canvas-api-token test-org-canvas-api-token)
         (org-canvas-course-id test-org-canvas-course-id)
         (org-canvas-read-only nil)
         (org-canvas-request-timeout test-org-canvas-request-timeout))
     ,@body))

(defmacro with-mock-api (&rest body)
  "Execute BODY with mocked API calls."
  (declare (indent 0))
  `(let ((test-org-canvas-api-calls nil)
         (test-org-canvas-api-responses nil))
     (cl-letf (((symbol-function 'org-canvas-api-request)
                #'test-org-canvas-mock-api-request))
       ,@body)))

(defmacro with-delete-snapshot-dir (&rest body)
  "Run BODY with delete snapshots going to a fresh temporary directory.
The directory is bound to `snapshot-dir' and removed afterwards, so a
spec never depends on, or leaves files in, the course directory the
rest of the suite shares (issue #555)."
  (declare (indent 0) (debug let))
  `(let* ((snapshot-dir (make-temp-file "delete-snapshots-" t))
          (org-canvas-diff-delete-snapshot-directory snapshot-dir))
     (unwind-protect
         (progn ,@body)
       (delete-directory snapshot-dir t))))

(defun test-org-canvas-delete--snapshot-files (dir)
  "Return DIR's delete-all snapshots, the <time>-<feature>-all.json files."
  (directory-files dir t "-all\\.json\\'"))

(defun test-org-canvas-delete--snapshot-titles (file)
  "Return the title of every item the snapshot FILE holds."
  (mapcar (lambda (item) (alist-get 'title item))
          (append (json-read-file file) nil)))

(defconst test-org-canvas--sync-fn-labels
  '((settings . org-canvas-sync-settings)
    (outcomes . org-canvas-sync-outcomes)
    (rubrics . org-canvas-sync-rubrics)
    (assignment-groups . org-canvas-sync-assignment-groups)
    (group-categories . org-canvas-sync-group-categories)
    (sections . org-canvas-pull-sections)
    (grading-periods . org-canvas-pull-grading-periods)
    (grading-schemes . org-canvas-pull-grading-schemes)
    (people . org-canvas-pull-people)
    (gradebook . org-canvas-pull-gradebook)
    (rubric-results . org-canvas-pull-rubric-results)
    (quiz-results . org-canvas-pull-quiz-results)
    (files . org-canvas-sync-files)
    (pages . org-canvas-sync-pages)
    (discussions . org-canvas-sync-discussions)
    (announcements . org-canvas-sync-announcements)
    (sync-grading-schemes . org-canvas-sync-grading-schemes)
    (calendar-events . org-canvas-sync-calendar-events)
    (quizzes . org-canvas-sync-quizzes)
    (new-quizzes . org-canvas-sync-new-quizzes)
    (assignments . org-canvas-sync-assignments)
    (overrides . org-canvas-sync-overrides)
    (quiz-accommodations . org-canvas-sync-quiz-accommodations)
    (modules . org-canvas-sync-modules))
  "The tier functions `org-canvas-sync' drives, one (LABEL . FN) each.
LABEL is the short name the orchestration specs record and assert on;
the list is the sync order, so a spec checking it reads the macro's
own order (#433).")

(defun test-org-canvas--mocked-sync-binding (pair record error overrides)
  "Return one cl-letf binding for the sync fn PAIR.
RECORD is the caller's list variable or nil; ERROR a LABEL whose
function signals; OVERRIDES an alist of (LABEL . LAMBDA) escapes."
  (let* ((label (car pair))
         (fn (cdr pair))
         (override (cdr (assq label overrides))))
    `((symbol-function ',fn)
      ,(cond (override)
             ((memq label (if (listp error) error (list error)))
              '(lambda () (error "Mocked sync failure")))
             (record `(lambda () (push ',label ,record)))
             (t '(lambda () nil))))))

(cl-defmacro with-mocked-sync-fns ((&key record error overrides) &rest body)
  "Run BODY with every tier sync function `org-canvas-sync' drives mocked.
Each one is a no-op by default, and `org-canvas--preflight-check'
answers nil.  RECORD, a list variable the caller binds, makes every
function push its LABEL there in the order it ran instead.  ERROR, a
LABEL or list of them from `test-org-canvas--sync-fn-labels', makes
those functions signal — the cascading-failure shape, where the sync
must carry on.  OVERRIDES, an alist of (LABEL . LAMBDA), replaces that
LABEL's mock outright: a spec making one tier record feature stats, or
watch a preflight, says so there.  The mock blocks the orchestration
specs hand-wrote were this macro, verbatim, eight times over (#433)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
             ,@(mapcar (lambda (pair)
                         (test-org-canvas--mocked-sync-binding
                          pair record error overrides))
                       test-org-canvas--sync-fn-labels))
     ,@body))

(defconst test-org-canvas--pull-fn-labels
  '((settings . org-canvas-pull-settings)
    (sections . org-canvas-pull-sections)
    (grading-periods . org-canvas-pull-grading-periods)
    (grading-schemes . org-canvas-pull-grading-schemes)
    (people . org-canvas-pull-people)
    (gradebook . org-canvas-pull-gradebook)
    (rubric-results . org-canvas-pull-rubric-results)
    (quiz-results . org-canvas-pull-quiz-results)
    (assignment-groups . org-canvas-pull-assignment-groups)
    (group-categories . org-canvas-pull-group-categories)
    (groups . org-canvas-pull-groups)
    (outcomes . org-canvas-pull-outcomes)
    (rubrics . org-canvas-pull-rubrics)
    (pages . org-canvas-pull-pages)
    (files . org-canvas-pull-files)
    (discussions . org-canvas-pull-discussions)
    (discussion-replies . org-canvas-pull-discussion-replies)
    (announcements . org-canvas-pull-announcements)
    (calendar-events . org-canvas-pull-calendar-events)
    (assignments . org-canvas-pull-assignments)
    (quizzes . org-canvas-pull-quizzes)
    (question-banks . org-canvas-pull-question-banks)
    (new-quizzes . org-canvas-pull-new-quizzes)
    (modules . org-canvas-pull-modules))
  "The pull functions `org-canvas-pull-all' drives, one (LABEL . FN) each.
In pull order; a spec checking the order reads the macro's own (#433).")

(cl-defmacro with-mocked-pull-fns ((&key record error overrides) &rest body)
  "Run BODY with every pull function `org-canvas-pull-all' drives mocked.
RECORD, ERROR and OVERRIDES are as `with-mocked-sync-fns' names, over
`test-org-canvas--pull-fn-labels'.  `org-canvas--preflight-check'
answers nil; `executable-find' and `yes-or-no-p' stay the spec's own,
since the pandoc and abort specs answer them differently."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-canvas--preflight-check) (lambda () nil))
             ,@(mapcar (lambda (pair)
                         (test-org-canvas--mocked-sync-binding
                          pair record error overrides))
                       test-org-canvas--pull-fn-labels))
     ,@body))

(defmacro with-temp-org-buffer (content &rest body)
  "Create a temp buffer with CONTENT in Org mode, then execute BODY.
Ensures proper org-mode initialization for consistent behavior
across Emacs versions."
  (declare (indent 1))
  `(let ((temp-file (make-temp-file "org-test-" nil ".org"))
         (org-agenda-files nil))
     (unwind-protect
         (progn
           (with-temp-file temp-file
             (insert ,content))
           (with-current-buffer (find-file-noselect temp-file)
             (unwind-protect
                 (progn
                   (goto-char (point-min))
                   ,@body)
               (kill-buffer))))
       (delete-file temp-file))))

(defun test-org-canvas-reset-file-caches ()
  "Reset the files module's session caches to their defvar defaults.

These are module-level globals that a sync populates and never clears on
the way out, so whatever the previous spec left behind is what the next
one starts from.  Six describes used to reset them ad hoc and disagreed
about how — some bound the folder cache to a fresh hash-table, one to
nil — which left the outcome of specs that never reset at all depending
on which describe ran before them (issue #43).

nil is the documented default and means \"no caching\"; the sync entry
points install a hash-table themselves.  Specs that want caching
behavior still set up their own table after this runs."
  (setq org-canvas--file-root-folder-cache nil)
  (setq org-canvas--file-folder-cache nil))

(defmacro with-sync-test-env (&rest body)
  "Execute BODY with sync infrastructure mocked.
Suppresses `org-canvas-clear-log' and `display-buffer' side effects."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-canvas-clear-log) (lambda () nil))
             ((symbol-function 'display-buffer) (lambda (_) nil)))
     ,@body))

;; A classic quiz pull lists each quiz's question groups (issue #492)
(defun test-org-canvas-quiz-no-groups (_method url &rest _)
  "Stand in for `org-canvas-api-request' in a quiz pull with no groups.
Answer a quiz's groups list (URL ending in /groups) with no groups and
refuse any other URL as an unmocked call, so the stub hides nothing."
  (if (string-match-p "/quizzes/[0-9]+/groups\\'" url)
      '((quiz_groups . []))
    (error "Unmocked org-canvas-api-request in a quiz pull test: %s" url)))

(defun test-org-canvas-pull-texts (file-var pull-fn api-fn &optional initial times)
  "Run PULL-FN TIMES times (default 2) and return the file text after each.
FILE-VAR names the `org-canvas-*-file' variable, bound to a fresh
file in a temporary directory holding INITIAL (default empty).
`org-canvas-api-request-all-pages' answers through API-FN on every
pull, so each one reads the same course.  The file's #+LAST_SYNCED
line is dropped from each text, since two pulls a minute apart stamp
it differently and a spec comparing them would flake."
  (let* ((dir (make-temp-file "pull-texts-" t))
         (file (expand-file-name "course.org" dir))
         (texts nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert (or initial "")))
          (cl-progv (list file-var) (list file)
            (with-org-canvas-test-config
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) api-fn)
                          ((symbol-function 'message) #'ignore))
                  (dotimes (_ (or times 2))
                    (funcall pull-fn)
                    (push (replace-regexp-in-string
                           "^#\\+LAST_SYNCED: .*\n" ""
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string)))
                          texts))))))
          (nreverse texts))
      (let ((buf (find-buffer-visiting file)))
        (when buf
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))
      (delete-directory dir t))))

(defmacro with-html-to-org-identity (&rest body)
  "Execute BODY with `org-canvas--html-to-org' as identity function."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-canvas--html-to-org) (lambda (html) html)))
     ,@body))

(defmacro with-nonexistent-canvas-files (&rest body)
  "Execute BODY with all `org-canvas-*-file' vars set to nonexistent paths."
  (declare (indent 0))
  `(let ((org-canvas-assignments-file "/tmp/nonexistent/assignments.org")
         (org-canvas-pages-file "/tmp/nonexistent/pages.org")
         (org-canvas-quizzes-file "/tmp/nonexistent/quizzes.org")
         (org-canvas-modules-file "/tmp/nonexistent/modules.org")
         (org-canvas-files-file "/tmp/nonexistent/files.org")
         (org-canvas-outcomes-file "/tmp/nonexistent/outcomes.org")
         (org-canvas-rubrics-file "/tmp/nonexistent/rubrics.org")
         (org-canvas-discussions-file "/tmp/nonexistent/discussions.org")
         (org-canvas-announcements-file "/tmp/nonexistent/announcements.org")
         (org-canvas-assignment-groups-file "/tmp/nonexistent/assignment-groups.org")
         (org-canvas-people-file "/tmp/nonexistent/people.org")
         (org-canvas-gradebook-file "/tmp/nonexistent/gradebook.org")
         (org-canvas-rubric-results-file "/tmp/nonexistent/rubric-results.org")
         (org-canvas-quiz-results-file "/tmp/nonexistent/quiz-results.org")
         (org-canvas-question-banks-file "/tmp/nonexistent/question-banks.org")
         (org-canvas-messages-file "/tmp/nonexistent/messages.org")
         (org-canvas-sections-file "/tmp/nonexistent/sections.org")
         (org-canvas-grading-periods-file "/tmp/nonexistent/grading-periods.org")
         (org-canvas-grading-schemes-file "/tmp/nonexistent/grading-schemes.org")
         (org-canvas-new-quizzes-file "/tmp/nonexistent/new-quizzes.org")
         (org-canvas-group-categories-file "/tmp/nonexistent/group-categories.org")
         (org-canvas-calendar-events-file "/tmp/nonexistent/calendar.org"))
     ,@body))

;;;; Test Generators

(defun test-org-canvas-define-common-parse-tests (parse-fn &rest opts)
  "Generate 4 standard parse-entry tests for PARSE-FN.
Call inside a `describe' block.  OPTS is a plist:
  :id-property  Org property name (default \"CANVAS_ID\")
  :id-key       Plist key in parsed data (default :canvas-id)
  :extra-props  Additional property lines to insert (e.g. \":START_AT: ...\n\")
  :body         Body text after properties (default \"\nBody.\n\")"
  (let ((id-property (or (plist-get opts :id-property) "CANVAS_ID"))
        (id-key (or (plist-get opts :id-key) :canvas-id))
        (extra-props (or (plist-get opts :extra-props) ""))
        (body (or (plist-get opts :body) "\nBody.\n")))

    (it "includes pom in data"
      (with-temp-org-buffer
       (concat "* Test\n:PROPERTIES:\n" extra-props ":END:\n" body)
       (org-back-to-heading)
       (let ((data (funcall parse-fn)))
         (expect (plist-get data :pom) :to-be-truthy))))

    (it "extracts canvas-id when present"
      (with-temp-org-buffer
       (concat "* Test\n:PROPERTIES:\n:" id-property ": 12345\n"
               extra-props ":END:\n" body)
       (org-back-to-heading)
       (let ((data (funcall parse-fn)))
         (expect (plist-get data id-key) :to-equal "12345"))))

    (it "returns nil canvas-id for new items"
      (with-temp-org-buffer
       (concat "* Test\n:PROPERTIES:\n" extra-props ":END:\n" body)
       (org-back-to-heading)
       (let ((data (funcall parse-fn)))
         (expect (plist-get data id-key) :to-be nil))))

    (it "errors on empty title"
      (with-temp-org-buffer
       (concat "* \n:PROPERTIES:\n" extra-props ":END:\n" body)
       (org-back-to-heading)
       (expect (funcall parse-fn) :to-throw 'error)))))

(defun test-org-canvas-define-common-transform-tests (transform-fn &rest opts)
  "Generate standard transform-props tests for TRANSFORM-FN.
Call inside a `describe' block.  OPTS is a plist:
  :defaults - Default raw plist for inputs (required)
  :title-key - Output plist key for title (default :title)
  :tests - List of test specs, each a plist:
    :raw-key     Raw input plist key
    :data-key    Output plist key to check
    :type        One of: boolean, timestamp, string
    :default     For boolean: default value when absent"
  (let ((defaults (plist-get opts :defaults))
        (title-key (or (plist-get opts :title-key) :title))
        (tests (plist-get opts :tests)))

    (it "strips statistics cookie from title"
      (let* ((input (plist-put (copy-sequence defaults) :title-raw "My Title [1/3]"))
             (result (funcall transform-fn input)))
        (expect (plist-get result title-key) :to-equal "My Title")))

    (dolist (spec tests)
      (let ((raw-key (plist-get spec :raw-key))
            (data-key (plist-get spec :data-key))
            (type (plist-get spec :type))
            (default (plist-get spec :default)))
        (pcase type
          ('boolean
           (if default
               (progn
                 (it (format "defaults %s to %s" data-key default)
                   (let* ((input (plist-put (copy-sequence defaults) raw-key nil))
                          (result (funcall transform-fn input)))
                     (expect (plist-get result data-key) :to-be default)))
                 (it (format "interprets %s=false" data-key)
                   (let* ((input (plist-put (copy-sequence defaults) raw-key "false"))
                          (result (funcall transform-fn input)))
                     (expect (plist-get result data-key) :to-be nil))))
             (it (format "interprets %s=true" data-key)
               (let* ((input (plist-put (copy-sequence defaults) raw-key "true"))
                      (result (funcall transform-fn input)))
                 (expect (plist-get result data-key) :to-be t)))
             (it (format "returns nil for absent %s" data-key)
               (let* ((input (plist-put (copy-sequence defaults) raw-key nil))
                      (result (funcall transform-fn input)))
                 (expect (plist-get result data-key) :to-be nil)))))
          ('timestamp
           (it (format "parses %s to ISO8601" data-key)
             (let* ((input (plist-put (copy-sequence defaults)
                                      raw-key "<2026-06-15 Mon 10:00>"))
                    (result (funcall transform-fn input)))
               (expect (plist-get result data-key) :to-match "2026-06-15T")))
           (it (format "returns nil for absent %s" data-key)
             (let* ((input (plist-put (copy-sequence defaults) raw-key nil))
                    (result (funcall transform-fn input)))
               (expect (plist-get result data-key) :to-be nil)))))))))

(defun test-org-canvas-define-common-push-tests (push-fn &rest opts)
  "Generate POST/PUT selection tests for PUSH-FN.
Call inside a `describe' block.  OPTS is a plist:
  :endpoint - API endpoint string (required)
  :id-key - Plist key for Canvas ID (default :canvas-id)"
  (let ((endpoint (plist-get opts :endpoint))
        (id-key (or (plist-get opts :id-key) :canvas-id)))

    (it "uses POST for new items"
      (with-org-canvas-test-config
        (with-mock-api
          (funcall push-fn (list id-key nil :title "Test" :pom 1) '((a . 1)))
          (expect-api-called 'POST endpoint))))

    (it "uses PUT for existing items"
      (with-org-canvas-test-config
        (with-mock-api
          (funcall push-fn (list id-key "42" :title "Test" :pom 1) '((a . 1)))
          (expect-api-called 'PUT endpoint))))))

(defmacro with-pull-property-test (pull-fn response property matcher value)
  "Test that PULL-FN with RESPONSE sets Org PROPERTY as expected.
Creates a temp buffer with CANVAS_ID: 1, mocks html-to-org as identity,
calls PULL-FN, and asserts (expect (org-entry-get (point) PROPERTY) MATCHER VALUE).
Paged list fetches (e.g. the assignment overrides sub-fetch) are mocked
to return nil, and the course-wide document-processor and posting
schedule reads answer as refused, so no pull-item can reach the network
guard."
  (declare (indent 2))
  `(with-temp-org-buffer
    "* Test\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
    (org-back-to-heading)
    (with-html-to-org-identity
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas--assignment-processors-fetch)
                 (lambda () 'refused))
                ((symbol-function 'org-canvas--assignment-schedules-fetch)
                 (lambda () 'refused)))
        (let ((org-canvas--assignment-processors-cache nil)
              (org-canvas--assignment-schedules-cache nil))
          (funcall ,pull-fn ,response (point)))
        (expect (org-entry-get (point) ,property) ,matcher ,value)))))

(defun test-org-canvas-stub-processors (&optional map schedules)
  "Answer the course-wide GraphQL reads of assignments for one spec.
MAP is a hash of assignment id (a string) to its processor nodes; nil
answers as a refused read, so every assignment falls back to its REST
`asset_processors'.  SCHEDULES is the same for the posting schedules
\(issue #352), a hash of assignment id to its `scheduledPost'; nil
answers as refused, so no POST_GRADES_AT or POST_COMMENTS_AT is
touched.  Call from a `before-each', beside an `after-each' of
`org-canvas--assignment-processors-forget': the spies end with the
spec, and forgetting here means no earlier spec's answer is reused
\(issue #350)."
  (org-canvas--assignment-processors-forget)
  (org-canvas--assignment-schedules-forget)
  (spy-on 'org-canvas--assignment-processors-fetch
          :and-return-value (or map 'refused))
  (spy-on 'org-canvas--assignment-schedules-fetch
          :and-return-value (or schedules 'refused)))

;;;; Assertion Helpers

(defun expect-api-called (method endpoint)
  "Assert that the mock API was called with METHOD matching ENDPOINT."
  (expect (test-org-canvas-api-called-p method endpoint) :to-be-truthy))

(defun expect-synced-timestamp (pom)
  "Assert that no per-entry LAST_SYNCED is written at POM.
After the file-level #+LAST_SYNCED header migration, finalize/pull
no longer writes a per-entry LAST_SYNCED — this helper enforces that
by asserting the property is absent."
  (expect (org-entry-get pom "LAST_SYNCED") :to-be nil))

;;;; Mock Data Builders

(defun test-org-canvas-make-response (defaults &optional overrides)
  "Merge OVERRIDES into DEFAULTS alist, returning a new alist.
Keys in OVERRIDES win over keys in DEFAULTS."
  (let ((result (copy-alist defaults)))
    (dolist (pair overrides)
      (setf (alist-get (car pair) result nil nil #'equal) (cdr pair)))
    result))

(defun test-org-canvas-make-page (&optional overrides)
  "Build a mock Canvas page API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((url . "test-page")
     (title . "Test Page")
     (body . "<p>Page body</p>")
     (published . t)
     (front_page . :json-false)
     (editing_roles . "teachers")
     (created_at . "2026-01-01T00:00:00Z")
     (updated_at . "2026-01-01T00:00:00Z"))
   overrides))

(defun test-org-canvas-make-assignment (&optional overrides)
  "Build a mock Canvas assignment API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 1001)
     (name . "Test Assignment")
     (description . "<p>Do the work</p>")
     (points_possible . 100)
     (grading_type . "points")
     (published . t)
     (submission_types . ("online_upload"))
     (due_at . "2026-06-01T23:59:00Z")
     (created_at . "2026-01-01T00:00:00Z")
     (updated_at . "2026-01-01T00:00:00Z"))
   overrides))

(defun test-org-canvas-make-discussion (&optional overrides)
  "Build a mock Canvas discussion API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 2001)
     (title . "Test Discussion")
     (message . "<p>Discuss this</p>")
     (discussion_type . "side_comment")
     (published . t)
     (pinned . :json-false)
     (require_initial_post . :json-false))
   overrides))

(defun test-org-canvas-make-announcement (&optional overrides)
  "Build a mock Canvas announcement API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 3001)
     (title . "Test Announcement")
     (message . "<p>Attention</p>")
     (is_announcement . t)
     (published . t))
   overrides))

(defun test-org-canvas-make-module (&optional overrides)
  "Build a mock Canvas module API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 4001)
     (name . "Test Module")
     (position . 1)
     (published . t)
     (items_count . 0)
     (items_url . "https://test.canvas.example.com/api/v1/courses/99999/modules/4001/items"))
   overrides))

(defun test-org-canvas-make-rubric (&optional overrides)
  "Build a mock Canvas rubric API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 5001)
     (title . "Test Rubric")
     (points_possible . 20)
     (free_form_criterion_comments . :json-false))
   overrides))

(defun test-org-canvas-make-quiz (&optional overrides)
  "Build a mock Canvas quiz API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 6001)
     (title . "Test Quiz")
     (quiz_type . "assignment")
     (published . t)
     (time_limit . 30)
     (points_possible . 50))
   overrides))

(defun test-org-canvas-make-calendar-event (&optional overrides)
  "Build a mock Canvas calendar event API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 7001)
     (title . "Test Event")
     (start_at . "2026-06-01T14:00:00Z")
     (end_at . "2026-06-01T15:00:00Z")
     (all_day . :json-false)
     (description . "<p>Event description</p>"))
   overrides))

(defun test-org-canvas-make-outcome (&optional overrides)
  "Build a mock Canvas outcome API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 8001)
     (title . "Test Outcome")
     (calculation_method . "highest")
     (mastery_points . 3)
     (points_possible . 5))
   overrides))

(defun test-org-canvas-make-group-category (&optional overrides)
  "Build a mock Canvas group category API response alist.
OVERRIDES is an alist of keys to override."
  (test-org-canvas-make-response
   '((id . 9001)
     (name . "Test Group Category")
     (self_signup . nil)
     (group_limit . nil)
     (auto_leader . nil))
   overrides))

(defun test-org-canvas-transient--suffix-command (suffix)
  "Extract the :command from a transient SUFFIX entry, or nil."
  (when (listp suffix)
    (let ((plist (if (numberp (car suffix))
                     (nth 2 suffix)
                   (cdr suffix))))
      (plist-get plist :command))))

(defun test-org-canvas-transient--column-has-command-p (col command)
  "Return non-nil if transient column vector COL contains COMMAND."
  (when (vectorp col)
    (let* ((has-level (numberp (aref col 0)))
           (suffixes (aref col (if has-level 3 2))))
      (cl-some (lambda (s) (eq (test-org-canvas-transient--suffix-command s) command))
               suffixes))))

(defun test-org-canvas-transient-has-command-p (prefix-sym command)
  "Return non-nil if PREFIX-SYM's transient layout contains COMMAND.
Handles both Emacs 29 (transient 0.4) and Emacs 30 (transient 0.7+) layout formats."
  (let* ((layout (get prefix-sym 'transient--layout))
         (columns (cond
                   ((listp layout) layout)
                   ((vectorp layout) (aref layout 2)))))
    (cl-some (lambda (col) (test-org-canvas-transient--column-has-command-p col command))
             columns)))

;;;; Drift Report Fixtures

;; Helpers more than one of the drift report's spec files needs: the
;; report buffer and its row navigation (`org-canvas-diff-test.el' and
;; the report, verbs and items files), the delete and batch-delete
;; stubs, the New Quiz and moved-item fixtures.

(defvar test-org-canvas--diff-delete-requests nil
  "Requests the delete stub recorded, newest first, as (METHOD URL ARGS).")

(defun test-org-canvas--diff-delete-stub (method url &rest args)
  "Record METHOD, URL and ARGS; answer a GET with a small object."
  (push (list method url args) test-org-canvas--diff-delete-requests)
  (when (eq method 'GET) '((id . 1) (title . "Snapshot me"))))

(defun test-org-canvas--diff-deletes ()
  "Return the DELETE requests the stub recorded, oldest first."
  (reverse (cl-remove-if-not (lambda (r) (eq (car r) 'DELETE))
                             test-org-canvas--diff-delete-requests)))

(defmacro test-org-canvas--with-snapshot-dir (&rest body)
  "Run BODY with the delete snapshots going to a fresh temporary directory.
The directory is bound to `snapshot-dir' and removed afterwards."
  (declare (indent 0))
  `(let* ((snapshot-dir (make-temp-file "diff-snapshots-" t))
          (org-canvas-diff-delete-snapshot-directory snapshot-dir)
          (test-org-canvas--diff-delete-requests nil))
     (ignore snapshot-dir)
     (unwind-protect (progn ,@body)
       (delete-directory snapshot-dir t))))

;;;; Deleting Rows Without Asking (issue #345)

(defconst test-org-canvas--diff-reshuffle-results
  '((:name "Assignments"
     :extra ((:kind extra :title "Session 87" :id "701")
             (:kind unclaimed :title "Session 88" :id "702" :property "CANVAS_ID")))
    (:name "Rubrics"
     :extra ((:kind extra :title "Old rubric" :id "801")))
    (:name "Module Items"
     :divergences ((:kind stale-ack :id "999"))
     :extra ((:kind extra :title "87-surveillance-capitalism" :id "5787192"
              :module-id "781715" :where "Week 9")
             (:kind extra :title "88-privacy" :id "5787193"
              :module-id "781715" :where "Week 9")
             (:kind moved :title "Closer" :id "5787194"
              :module-id "781715" :where "Week 9" :to "Week 10"))))
  "What the report found after the calendar reshuffle of issue #345.")

(defmacro test-org-canvas--with-batch-delete (&rest body)
  "Run BODY against the mock API with snapshots in a temporary directory.
Preflight is stubbed; `snapshot-dir' names the directory."
  (declare (indent 0))
  `(with-org-canvas-test-config
     (test-org-canvas--with-snapshot-dir
       (with-mock-api
         (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore))
           ,@body)))))

(defun test-org-canvas--diff-outcome (outcomes id)
  "Return the outcome symbol OUTCOMES record for ID."
  (plist-get (cl-find id outcomes :key (lambda (o) (plist-get o :id))
                      :test #'equal)
             :outcome))

(defun test-org-canvas--diff-deleted-urls ()
  "Return the URLs the mock API received a DELETE for, oldest first."
  (reverse (delq nil (mapcar (lambda (c) (and (eq (car c) 'DELETE) (nth 1 c)))
                             test-org-canvas-api-calls))))

;;;; Stamping a MOVED Row and the RELOCATE Row (issues #342, #343)

(defconst test-org-canvas-342--modules-org
  "* Week 5
:PROPERTIES:
:CANVAS_ID: 781703
:END:
** Readings
* Week 6
:PROPERTIES:
:CANVAS_ID: 781704
:END:
** [[file:assignments.org::*R7: Arguing Both Sides][R7: Arguing Both Sides]]
** Readings
"
  "Week 6 holds a renumbered item heading and a Readings header, both unstamped.")

(defconst test-org-canvas-342--r7
  "[[file:assignments.org::*R7: Arguing Both Sides][R7: Arguing Both Sides]]"
  "The raw text of the renumbered heading, as the report records it.")

(defmacro test-org-canvas-342--with-modules-file (var &rest body)
  "Bind VAR to a temporary modules file holding the #342 fixture, run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "diff-342-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert test-org-canvas-342--modules-org))
           ,@body)
       (let ((buf (find-buffer-visiting ,var))) (when buf (kill-buffer buf)))
       (delete-file ,var))))

(defun test-org-canvas-342--ids (file)
  "Return (HEADING CANVAS_ID) for each level-2 heading of FILE.
HEADING is the shown text, the same on Emacs 29 and 30."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-map-entries
     (lambda ()
       (list (org-link-display-format (org-get-heading t t t t))
             (org-entry-get (point) "CANVAS_ID")))
     "LEVEL=2")))

(defun test-org-canvas-342--moved (file &rest overrides)
  "Return the MOVED entry naming the R7 heading of FILE, with OVERRIDES."
  (let ((entry (list :kind 'moved :title "R6: Arguing Both Sides" :id "5707155"
                     :module-id "781703" :where "Week 5" :to "Week 6"
                     :file file :line 10
                     :partner-heading test-org-canvas-342--r7
                     :partner-title "R7: Arguing Both Sides")))
    (while overrides
      (setq entry (plist-put entry (pop overrides) (pop overrides))))
    entry))

(defun test-org-canvas-342--row-buffer (entry)
  "Return a report buffer whose Module Items section holds ENTRY."
  (test-org-canvas--diff-report-buffer
   `((:name "Modules" :children (:name "Module Items" :extra (,entry))))))

;;;; New Quizzes in the drift report (issue #313)

(defconst test-nq-313--file
  (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n:TIME_LIMIT: 30\n:SCORING_POLICY: keep_highest\n:END:\n"
          "Read carefully.\n\n"
          "** Q1\n:PROPERTIES:\n:CANVAS_ITEM_ID: 9\n:TYPE: essay\n:END:\n"
          "* Final\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 42\n:END:\n"
          "* Draft\n")
  "A new-quizzes.org with a drifted quiz, a deleted one and an unpushed one.")

(defconst test-nq-313--remote
  '(((id . "41") (assignment_id . "41") (title . "Midterm")
     (instructions . "<p>Read slowly.</p>")
     (quiz_settings (has_time_limit . t)
                    (session_time_limit_in_seconds . 2700)
                    (multiple_attempts (multiple_attempts_enabled . :json-false)
                                       (score_to_keep . "highest"))))
    ((id . "43") (assignment_id . "43") (title . "Web Quiz")))
  "The quiz service's list for `test-nq-313--file'.
Midterm's settings sit under `quiz_settings' as Canvas keeps them
\(issue #321): its time limit differs, its SCORING_POLICY agrees.")

(defmacro test-nq-313--with-file (content &rest body)
  "Run BODY with `org-canvas-new-quizzes-file' holding CONTENT."
  (declare (indent 1))
  `(let ((file (make-temp-file "diff-nq-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (let ((org-canvas-new-quizzes-file file))
             ,@body))
       (let ((buf (find-buffer-visiting file)))
         (when buf
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-file file))))

;;;; Assignment Overrides (issue #411)

(defconst test-diff-411--assignments
  (concat "* R6: The Verdict\n:PROPERTIES:\n:CANVAS_ID: 456\n"
          ":CANVAS_UPDATED_AT: 2026-09-28T16:32:20Z\n:END:\n\nBody.\n\n"
          "#+NAME: overrides\n"
          "| Section | Due At | Lock At |\n"
          "|---------+--------+---------|\n"
          "| [[file:sections.org::*Section A][Section A]] | <2026-09-28 Mon 14:15> | |\n"
          "| [[file:sections.org::*Section C][Section C]] | <2026-09-30 Wed 14:15> | |\n"
          "| Students: #279871 | <2026-09-29 Tue 14:00> | <2026-10-01 Thu 14:00> |\n"
          "* No overrides\n:PROPERTIES:\n:CANVAS_ID: 457\n:END:\n"
          "* Gone\n:PROPERTIES:\n:CANVAS_ID: 458\n:CANVAS_DELETED: [2026-09-28 Mon]\n:END:\n"
          "* Draft\n")
  "One heading with a table, one without, one a pull marked deleted, one unstamped.")

(defun test-diff-411--iso (org-timestamp)
  "Return ORG-TIMESTAMP as the ISO 8601 string the table's parse gives it."
  (org-canvas-org-parse-timestamp org-timestamp))

(defun test-diff-411--items ()
  "Return the Assignments list reply, overrides included, as Canvas sends it.
Section A's override agrees with its row; the student's lacks the
row's lock; Section B's no row claims; id 14 no row claims and its
due date is the assignment's own."
  (let ((due (test-diff-411--iso "<2026-09-28 Mon 14:15>")))
    (vector
     `((id . 456) (name . "R6: The Verdict") (due_at . ,due)
       (updated_at . "2026-09-28T16:32:20Z")
       (html_url . "https://canvas.example/courses/1/assignments/456")
       (overrides . [((id . 10) (course_section_id . 777) (due_at . ,due))
                     ((id . 11) (student_ids . [279871])
                      (due_at . ,(test-diff-411--iso "<2026-09-29 Tue 14:00>")))
                     ((id . 12) (course_section_id . 779)
                      (due_at . ,(test-diff-411--iso "<2026-10-02 Fri 14:15>")))
                     ((id . 14) (student_ids . [300]) (due_at . ,due))]))
     '((id . 457) (name . "No overrides") (updated_at . "2026-09-28T16:32:20Z")
       (overrides . [])))))

(defun test-diff-411--result (items &optional excluded fn)
  "Run the Assignments diff over the scratch course with ITEMS as the list.
EXCLUDED binds `org-canvas-diff-excluded-features'.  Returns (RESULT
FILE PARAMS): the Assignments result, the assignments file, and the
parameters the list was read with.  FN, when given, is called with
RESULT and FILE while the scratch course still exists."
  (let ((dir (make-temp-file "diff-411-" t))
        (params nil))
    (unwind-protect
        (let ((file (expand-file-name "assignments.org" dir)))
          (with-temp-file (expand-file-name "sections.org" dir)
            (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 777\n:END:\n"
                    "* Section B\n:PROPERTIES:\n:CANVAS_ID: 779\n:END:\n"
                    "* Section C\n:PROPERTIES:\n:CANVAS_ID: 778\n:END:\n"))
          (with-temp-file file (insert test-diff-411--assignments))
          (let ((org-canvas-assignments-file file)
                (org-canvas-sections-file (expand-file-name "sections.org" dir))
                (org-canvas-diff-excluded-features excluded))
            (with-org-canvas-test-config
              (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                         (lambda (_method _url &optional p &rest _)
                           (setq params p)
                           items))
                        ((symbol-function 'org-canvas-api-request)
                         (lambda (&rest _) (error "The report must not write"))))
                (let ((result (org-canvas--diff-feature
                               (org-canvas--registry-find-feature "assignments"))))
                  (when fn (funcall fn result file))
                  (list result file params))))))
      (dolist (name '("assignments.org" "sections.org"))
        (let ((buf (find-buffer-visiting (expand-file-name name dir))))
          (when buf
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf))))
      (delete-directory dir t))))

(defun test-diff-411--by-state (entries state)
  "Return the entries of ENTRIES whose :state is STATE."
  (cl-remove-if-not (lambda (e) (eq (plist-get e :state) state)) entries))

(defun test-org-canvas--diff-report-buffer (results)
  "Render RESULTS the way `org-canvas-diff' does and return the buffer."
  (with-org-canvas-test-config
    (let ((noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                ;; The referenced-media scan (#102) would otherwise
                ;; read the syllabus, one request these specs count.
                ((symbol-function 'org-canvas--diff-syllabus-references)
                 (lambda () nil))
                ((symbol-function 'org-canvas--diff-feature)
                 (lambda (feature)
                   (or (cl-find (plist-get feature :name) results
                                :key (lambda (r) (plist-get r :name))
                                :test #'string=)
                       (list :name (plist-get feature :name))))))
        (org-canvas-diff)
        (get-buffer org-canvas--diff-buffer-name)))))

(defun test-org-canvas--diff-goto-row (kind)
  "Put point on the first row of KIND in the current buffer."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((row (get-text-property (point) 'org-canvas-diff-row)))
        (if (and row (eq (plist-get (plist-get row :entry) :kind) kind))
            (setq found t)
          (forward-line 1))))
    (unless found (error "No %s row" kind))))

(defun test-org-canvas--diff-browse (results kind &optional edit)
  "Browse the first KIND row of a report of RESULTS; return the URL opened.
EDIT is passed on.  A `user-error' comes back as (error MESSAGE)."
  (let ((opened nil)
        (org-canvas-base-url "https://canvas.test")
        (org-canvas-course-id "42"))
    (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
      (with-current-buffer (test-org-canvas--diff-report-buffer results)
        (test-org-canvas--diff-goto-row kind)
        (condition-case e
            (progn (org-canvas-diff-browse edit) opened)
          (user-error (list 'error (error-message-string e))))))))

;; Pin the zone Org timestamps are read and written in (issue #136).  With
;; nil the converters would use this machine's local zone whenever no
;; settings.org names a course zone, and every exact timestamp expectation
;; in the suite would depend on where the tests run.  Specs about the zone
;; policy itself bind `org-canvas-time-zone' to nil.
(setq org-canvas-time-zone "UTC")

;;;; Submissions Fixtures
;;
;; Shared by the submissions test files, which were split along the
;; module lines in issue #570.  A fixture used by more than one of
;; them lives here.

(defun test-org-canvas-make-user (&optional overrides)
  "Build a mock Canvas user alist."
  (test-org-canvas-make-response
   '((id . 5001)
     (name . "Alice Adams")
     (sortable_name . "Adams, Alice"))
   overrides))

(defun test-org-canvas-make-submission (&optional overrides)
  "Build a mock Canvas submission alist."
  (test-org-canvas-make-response
   `((id . 50001)
     (user_id . 5001)
     (assignment_id . 1001)
     (workflow_state . "submitted")
     (submitted_at . "2026-02-15T23:45:00Z")
     (late . :json-false)
     (missing . :json-false)
     (score . 92)
     (body . "Here is my solution.")
     (user . ,(test-org-canvas-make-user))
     (assignment . ((points_possible . 100)))
     (submission_comments . [])
     (rubric_assessment . nil)
     (attachments . []))
   overrides))

(defun test-org-canvas-make-submission-with-comment (&optional overrides)
  "Build a mock submission with a comment."
  (test-org-canvas-make-submission
   (append
    `((submission_comments
       . [((author_name . "Prof. Smith")
           (comment . "Good work!")
           (created_at . "2026-02-16T10:00:00Z"))]))
    overrides)))

(defun test-org-canvas-make-submission-with-attachment (&optional overrides)
  "Build a mock submission with an attachment."
  (test-org-canvas-make-submission
   (append
    `((attachments
       . [((display_name . "homework1.pdf")
           (url . "https://canvas.example.com/files/999/download"))]))
    overrides)))

(defun test-org-canvas-make-submission-with-rubric (&optional overrides)
  "Build a mock submission with rubric assessment."
  (test-org-canvas-make-submission
   (append
    `((rubric_assessment
       . ((crit_1 . ((points . 18)
                     (rating_description . "Excellent")))
          (crit_2 . ((points . 15)
                     (rating_description . "Good"))))))
    overrides)))

;;;; Status Normalization

(defconst test-rubric-criteria
  '(((id . "_7104") (description . "Thesis") (points . 2.0)
     (ratings . [((id . "r1") (description . "Excellent") (points . 2.0))
                 ((id . "r2") (description . "Weak") (points . 1.0))]))
    ((id . "_7105") (description . "Evidence | support") (points . 3))
    ((id . "_7106") (description . "Style") (points . 1)))
  "Three rubric criteria as the assignment object lists them.")

(defmacro with-grading-file (content &rest body)
  "Visit a scratch grading file holding CONTENT, in submissions mode, run BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-canvas-subs-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (setq buf (find-file-noselect file))
           (with-current-buffer buf
             (org-mode)
             (org-canvas-submissions-mode 1)
             ;; A refresh reads the column's reports over GraphQL
             ;; (#351); here the column has none.
             (cl-letf (((symbol-function 'org-canvas--graphql-query) #'ignore))
               ,@body)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

(defconst test-grading-file-header
  "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n\n")

(defun test-refresh--from-canvas (overrides)
  "Refresh the current grading file as if Canvas held one submission.
OVERRIDES adjust `test-org-canvas-make-submission'; a prompt fails."
  (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
             (lambda (_id) (list (test-org-canvas-make-submission overrides))))
            ((symbol-function 'org-canvas--submissions-fetch-assignment)
             (lambda (_id) '((id . 1001))))
            ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
            ((symbol-function 'switch-to-buffer) (lambda (b) b))
            ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask"))))
    (org-canvas-submissions-refresh)))

(defvar test-refresh--messages nil "What the last `test-refresh--run' said.")

(defvar test-refresh--log nil "What the last `test-refresh--run' logged at INFO.")

(defun test-refresh--run (subs)
  "Refresh the current grading file as if Canvas held SUBS.
Messages and INFO log lines are collected in `test-refresh--messages'
and `test-refresh--log'; a prompt fails."
  (setq test-refresh--messages nil
        test-refresh--log nil)
  (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment) (lambda (_id) subs))
            ((symbol-function 'org-canvas--submissions-fetch-assignment) (lambda (_id) '((id . 1001))))
            ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
            ((symbol-function 'switch-to-buffer) (lambda (b) b))
            ((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask")))
            ((symbol-function 'message)
             (lambda (fmt &rest args) (push (apply #'format fmt args) test-refresh--messages)))
            ((symbol-function 'org-canvas--log-info)
             (lambda (_logger fmt &rest args) (push (apply #'format fmt args) test-refresh--log))))
    (org-canvas-submissions-refresh)))

(defun test-refresh--summary ()
  "Return the Refreshed line of the last `test-refresh--run'."
  (cl-find-if (lambda (m) (string-prefix-p "Refreshed " m)) test-refresh--messages))

(defun test-refresh--student (name id &rest props)
  "Return a grading-file heading for NAME with USER_ID ID and PROPS."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n" name id)
          (apply #'concat props)
          ":END:\n"))

(defun test-refresh--bob (&optional overrides)
  "Return a submission for Beta, Bob (5002), adjusted by OVERRIDES."
  (test-org-canvas-make-submission
   (append overrides
           '((id . 50002) (user_id . 5002)
             (user . ((id . 5002) (name . "Bob Beta") (sortable_name . "Beta, Bob")))))))

(defmacro with-submissions-dir (&rest body)
  "Run BODY with `org-canvas-submissions-directory' bound to a fresh DIR.
Every buffer visiting a file under it is killed afterwards, and no
prompt may be reached: `completing-read' and `y-or-n-p' both signal."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "org-canvas-subs-" t))
          (org-canvas-submissions-directory dir)
          (noninteractive t))
     (unwind-protect
         (cl-letf (((symbol-function 'completing-read)
                    (lambda (&rest _) (error "must not prompt")))
                   ((symbol-function 'y-or-n-p)
                    (lambda (&rest _) (error "must not ask")))
                   ((symbol-function 'switch-to-buffer) (lambda (b) b))
                   ((symbol-function 'org-canvas--submissions-heading-for-assignment)
                    (lambda (_id) nil))
                   ((symbol-function 'org-canvas--graphql-query) #'ignore))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p (file-truename dir) (file-truename (buffer-file-name b))))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun test-entry--canvas (assignment-object &optional listed)
  "Return an `org-canvas-api-request' stub answering for ASSIGNMENT-OBJECT.
A single-assignment read answers ASSIGNMENT-OBJECT, or signals when it
is nil; the list read answers LISTED; a submissions read answers one
submission.  Every call is pushed onto `test-entry--calls'."
  (lambda (method url &rest _)
    (push (cons method url) test-entry--calls)
    (cond ((string-match-p "/submissions$" url)
           (vector (test-org-canvas-make-submission)))
          ((string-match-p "/assignments/[0-9]+$" url)
           (or assignment-object (signal 'org-canvas-api-error (list "404"))))
          ((string-match-p "/assignments$" url)
           (vconcat listed))
          (t (error "Unexpected request %s" url)))))

(defvar test-entry--calls nil "The requests a stub of `test-entry--canvas' saw.")

(defconst test-rubric-assignment
  '((id . 1001)
    (name . "Global Challenge Essay")
    (use_rubric_for_grading . t)
    (rubric_settings . ((id . 133477) (title . "Essay Rubric") (points_possible . 100)
                        (free_form_criterion_comments . :json-false)
                        (hide_score_total . :json-false) (hide_points . :json-false)))
    (rubric . [((id . "_1") (description . "Thesis") (points . 20.0)
                (ratings . [((description . "Excellent") (points . 20.0))
                            ((description . "Weak | vague") (points . 5.0))]))
               ((id . "_2") (description . "Evidence") (points . 30)
                (ratings . [((description . "Strong") (points . 30))]))])))

(defconst test-rubric-assignment-described
  '((id . 1001)
    (name . "Closer")
    (use_rubric_for_grading . t)
    (rubric_settings . ((id . 138462) (title . "Closer Rubric") (points_possible . 6)))
    (rubric . [((id . "_1") (description . "The Day&#39;s Idea Does the Work") (points . 2)
                (long_description . "<p>The answer is built from the session&#39;s material.</p><p>Feed-forward: journal row 2.</p>")
                (ratings . [((description . "Working") (points . 2)
                             (long_description . "The day&#39;s idea carries the answer"))
                            ((description . "Named") (points . 1)
                             (long_description . "Named, but the answer reads the same without it"))
                            ((description . "Absent") (points . 0) (long_description . :null))]))
               ((id . "_2") (description . "Stakes | Named") (points . 4) (long_description . :null)
                (ratings . [((description . "Yes") (points . 4))]))]))
  "An assignment whose rubric carries the descriptions Canvas escapes (issue #265).")

(defconst test-rubric-file-header
  (concat test-grading-file-header
          "#+PROPERTY: POINTS_POSSIBLE 6\n"
          "#+PROPERTY: CANVAS_RUBRIC_ID 133477\n"
          "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING true\n\n")
  "Grading-file header of an assignment whose rubric sets the grade.")

(defun test-rubric-entry (name user-id props rows)
  "Return a student entry for NAME and USER-ID with PROPS and rubric ROWS.
PROPS is a string of property lines; ROWS a list of (ID CRITERION MAX
SCORE COMMENT), written as the table and the comment items under it."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n%s:END:\n\n** Rubric\n" name user-id props)
          "| Id | Criterion | Max | Score |\n|---+---+---+---|\n"
          (mapconcat (lambda (r) (apply #'format "| %s | %s | %s | %s |\n"
                                        (mapcar (lambda (c) (or c "")) (cl-subseq r 0 4))))
                     rows "")
          (mapconcat (lambda (r)
                       (format "- %s ::%s\n" (nth 0 r)
                               (if (nth 4 r)
                                   (concat " " (replace-regexp-in-string "\n" "\n  " (nth 4 r)))
                                 "")))
                     rows "")
          "\n"))

(defmacro with-rubric-file (content &rest body)
  "Visit CONTENT as a grading file with its context recovered, then run BODY."
  (declare (indent 1))
  `(with-grading-file ,content
     (org-canvas--submissions-ensure-context)
     ,@body))

(defconst test-rubric-blank-rows
  '(("_7104" "Thesis" 2 nil nil) ("_7105" "Evidence" 3 nil nil) ("_7106" "Style" 1 nil nil))
  "An unassessed rubric table's rows.")

(defconst test-grade-after--rows
  '(("_7104" "Thesis" 2 nil "Read it again.") ("_7105" "Evidence" 3 nil nil)
    ("_7106" "Style" 1 nil "Tidy."))
  "Comment-only rubric rows: feedback on two criteria and no points anywhere.")

(defun test-rubric--display (subs shown-cell)
  "Render SUBS as HW's grading file, storing the buffer in SHOWN-CELL's car."
  (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b) (setcar shown-cell b) b))
            ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
            ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) nil)))
    (org-canvas--submissions-display
     "HW" "1001" subs 'detail
     `((id . 1001) (use_rubric_for_grading . t) (rubric_settings . ((id . 7) (title . "R")))
       (rubric . ,(vconcat test-rubric-criteria))))))

(defmacro test-progress--with-replies (replies &rest body)
  "Run BODY with the API answering REPLIES in turn, waits recorded, not slept.
Each call is pushed onto `calls' as (METHOD URL); each wait onto
`waits'.  A reply that is the symbol `error' signals instead."
  (declare (indent 1))
  `(let ((queue ,replies) (calls nil) (waits nil))
     (cl-letf (((symbol-function 'org-canvas-api-request)
                (lambda (method url &rest _)
                  (push (list method url) calls)
                  (let ((reply (pop queue)))
                    (if (eq reply 'error)
                        (signal 'org-canvas-api-error '("Connection failed"))
                      reply))))
               ((symbol-function 'org-canvas--wait)
                (lambda (seconds &rest _) (push seconds waits)))
               ((symbol-function 'org-canvas--log-warning) #'ignore))
       ,@body)))

(defmacro test-batch-push--with-dir (content &rest body)
  "Run BODY with a submissions directory holding HW.org, CONTENT, unvisited.
`file' is bound to its path; any buffer visiting it is killed after."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-canvas-batch-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           ;; The grading file reads the column's reports over GraphQL
           ;; on a refresh only; a push asks nothing of it.
           ,@body)
       (let ((buf (find-buffer-visiting file)))
         (when (buffer-live-p buf)
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun test-sent--file (baseline items &optional more)
  "Return a grading file whose student carries BASELINE and ITEMS under Comments.
BASELINE is a list of (ID . TEXT), written as CANVAS_COMMENTS digests;
ITEMS is the text of the Comments section.  MORE follows the student."
  (concat test-grading-file-header
          "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n"
          (if baseline
              (format ":CANVAS_COMMENTS: %s\n"
                      (mapconcat (lambda (p)
                                   (format "%s=%s" (car p)
                                           (org-canvas--submissions-comment-digest (cdr p))))
                                 baseline " "))
            "")
          ":END:\n\n** Comments\n" items "\n** Notes\nMine.\n" (or more "")))

(defun test-sent--comment (id author text)
  "Return a Canvas submission comment ID by the user AUTHOR holding TEXT."
  `((id . ,id) (author_id . ,author) (author_name . "Prof") (comment . ,text)
    (created_at . "2026-09-28T14:02:00Z")))

(defmacro test-sent--with-canvas (comments &rest body)
  "Run BODY with Canvas holding COMMENTS on Alice's submission, the grader 77."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-canvas--submissions-self-id) (lambda () "77"))
             ((symbol-function 'org-canvas--submissions-fetch-for-assignment)
              (lambda (_id)
                (list (test-org-canvas-make-submission
                       (list (cons 'submission_comments (vconcat ,comments)))))))
             ((symbol-function 'org-canvas--submissions-fetch-assignment) #'ignore)
             ((symbol-function 'switch-to-buffer) #'identity))
     (with-html-to-org-identity ,@body)))

(defmacro test-sent--collecting-warnings (&rest body)
  "Run BODY and return the warnings it logged, in order."
  `(let ((warnings nil))
     (cl-letf (((symbol-function 'org-canvas--log-warning)
                (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warnings))))
       ,@body)
     (nreverse warnings)))

(defun test-sent-only--everything-file ()
  "Return a grading file where a sent comment, scores, rows, drafts and the bank differ."
  (concat test-rubric-file-header
          "* Comment Bank\n- Show your units.\n\n"
          (test-rubric-entry "Adams, Alice" 5001
                             (format ":SCORE: 4\n:CANVAS_SCORE: 3\n:SUBMISSION_ID: 9001\n:LATE_STATUS: late\n:CANVAS_COMMENTS: 22=%s\n"
                                     (org-canvas--submissions-comment-digest "Mine."))
                             '(("_7104" "Thesis" 2 2 "Sharp") ("_7105" "Evidence" 3 2 nil)
                               ("_7106" "Style" 1 nil nil)))
          "** Comments\n- *Prof* [22] :: Mine, rewritten.\n\n"
          "** Comment to post\nSee me.\n"))

;;;; Validation Test Helpers

(defmacro with-validate-test-dir (temp-dir-var &rest body)
  "Create a temp dir, bind all org-canvas file vars to it, run BODY.
TEMP-DIR-VAR is bound to the temp directory path.
Cleans up on exit."
  (declare (indent 1))
  `(let* ((,temp-dir-var (make-temp-file "org-val-int-" t))
          (org-canvas-directory ,temp-dir-var)
          (org-canvas-assignments-file (expand-file-name "assignments.org" ,temp-dir-var))
          (org-canvas-pages-file (expand-file-name "pages.org" ,temp-dir-var))
          (org-canvas-quizzes-file (expand-file-name "quizzes.org" ,temp-dir-var))
          (org-canvas-modules-file (expand-file-name "modules.org" ,temp-dir-var))
          (org-canvas-files-file (expand-file-name "files.org" ,temp-dir-var))
          (org-canvas-outcomes-file (expand-file-name "outcomes.org" ,temp-dir-var))
          (org-canvas-rubrics-file (expand-file-name "rubrics.org" ,temp-dir-var))
          (org-canvas-discussions-file (expand-file-name "discussions.org" ,temp-dir-var))
          (org-canvas-announcements-file (expand-file-name "announcements.org" ,temp-dir-var))
          (org-canvas-assignment-groups-file (expand-file-name "assignment-groups.org" ,temp-dir-var))
          (org-canvas-sections-file (expand-file-name "sections.org" ,temp-dir-var))
          (org-canvas-settings-file (expand-file-name "settings.org" ,temp-dir-var))
          (org-canvas-new-quizzes-file (expand-file-name "new-quizzes.org" ,temp-dir-var))
          (org-canvas-group-categories-file (expand-file-name "group-categories.org" ,temp-dir-var))
          (org-canvas-calendar-events-file (expand-file-name "calendar.org" ,temp-dir-var)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,temp-dir-var t))))

(defun test-validate-create-empty-files (dir &optional except)
  "Create empty org files for all content types in DIR.
EXCEPT is a list of filenames to skip."
  (dolist (f '("assignments.org" "pages.org" "quizzes.org"
               "modules.org" "files.org" "outcomes.org"
               "rubrics.org" "discussions.org" "announcements.org"
               "assignment-groups.org" "sections.org" "settings.org"
               "new-quizzes.org" "group-categories.org" "calendar.org"))
    (unless (member f except)
      (with-temp-file (expand-file-name f dir)
        (insert "#+TITLE: Test\n")))))

(defun test-validate-538--entry (props &optional table)
  "Return the findings of an assignment heading, whole plists.
PROPS is its drawer text and TABLE an override table under it; the
heading is checked as the assignment spec checks it."
  (with-temp-org-buffer
      (format "* HW\n:PROPERTIES:\n%s:END:\n%s"
              props (if table (concat "\n#+NAME: overrides\n" table) ""))
    (org-back-to-heading t)
    (org-canvas--validate-entry-at-marker
     nil '(("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
     #'org-canvas--validate-assignment-structure "/c/assignments.org")))

(defun test-validate-538--brief (issues)
  "Return ISSUES as (SEVERITY LINE CHECK ACCEPTED MESSAGE) lists."
  (mapcar (lambda (i)
            (list (plist-get i :severity) (plist-get i :line)
                  (plist-get i :check) (plist-get i :accepted)
                  (plist-get i :message)))
          issues))

(defconst test-validate-538--online
  ":SUBMISSION: online_upload\n:UNLOCK_AT: <2099-03-01 Sun 00:00>\n:DUE_AT: <2099-03-10 Tue 23:59>\n"
  "An online heading, unlocked and due, with no lock date.")

(defun test-validate--weights-file (dir &rest weights)
  "Write assignment-groups.org in DIR with one group per entry in WEIGHTS."
  (with-temp-file (expand-file-name "assignment-groups.org" dir)
    (insert "* Assignment Groups\n")
    (let ((n 0))
      (dolist (w weights)
        (setq n (1+ n))
        (insert (format "** Group %d\n:PROPERTIES:\n:WEIGHT: %s\n:END:\n" n w))))))

(provide 'test-helper)
;;; test-helper.el ends here
