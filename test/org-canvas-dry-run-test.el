;;; org-canvas-dry-run-test.el --- A dry run must not write to Canvas  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Issue #34: `org-canvas-sync-dry-run' deleted and re-uploaded five files on a
;; live course.  The macro pipeline did guard dry runs, but the files module
;; runs its own loop straight into `org-canvas-api-request', so nothing checked
;; `org-canvas--dry-run' before the DELETE.  Overrides had the same hole.
;;
;; These tests run EVERY feature sync -- macro-generated and hand-written --
;; over a throwaway copy of the bundled demo course, which ships CANVAS_IDs so
;; the update/replace paths are the ones exercised, and assert:
;;
;;   1. no request other than GET reaches the API layer, and
;;   2. the .org files come back byte-for-byte identical, because a preview
;;      must not record CANVAS_ID, LAST_SYNCED or PAYLOAD_HASH either.
;;
;; The command list is deliberately exhaustive rather than sampled: the bug was
;; a module that opted out of the shared infrastructure, so a test covering
;; only the shared path would have missed it.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'cl-lib)
(require 'seq)

(defconst org-canvas-dry-run--demo-dir
  (let ((rel "demo-course"))
    (cond ((file-directory-p rel) (expand-file-name rel))
          ((file-directory-p (expand-file-name rel default-directory))
           (expand-file-name rel default-directory))
          (t rel)))
  "Absolute path to the bundled demo course used as a fixture.")

(defconst org-canvas-dry-run--sync-commands
  '(org-canvas-sync-assignments
    org-canvas-sync-assignment-groups
    org-canvas-sync-announcements
    org-canvas-sync-pages
    org-canvas-sync-discussions
    org-canvas-sync-quizzes
    org-canvas-sync-new-quizzes
    org-canvas-sync-modules
    org-canvas-sync-rubrics
    org-canvas-sync-outcomes
    org-canvas-sync-group-categories
    org-canvas-sync-calendar-events
    org-canvas-sync-settings
    org-canvas-sync-files
    org-canvas-sync-overrides)
  "Every feature sync entry point, macro-generated or hand-written.")

(defvar org-canvas-dry-run--calls nil
  "Recorded (METHOD . URL) pairs made during the sync under test.")

(defvar org-canvas-dry-run--log nil
  "Log lines emitted during the sync under test.")

(defun org-canvas-dry-run--record (method url &rest _args)
  "Record METHOD and URL; return nil as the response body.
Returning nil rather than a canned object keeps every caller on a
well-defined path (an empty result list) instead of tripping over a
stub shaped wrong for that endpoint."
  (push (cons method url) org-canvas-dry-run--calls)
  nil)

(defun org-canvas-dry-run--capture-log (_logger format-string &rest args)
  "Record FORMAT-STRING applied to ARGS.
Log lines are captured into a list rather than read back out of
*org-canvas-log*, whose contents leak between tests."
  (push (apply #'format format-string args) org-canvas-dry-run--log))

(defun org-canvas-dry-run--org-checksums (dir)
  "Return an alist of (NAME . MD5) for every .org file in DIR."
  (mapcar (lambda (f)
            (cons (file-name-nondirectory f)
                  (with-temp-buffer
                    (insert-file-contents-literally f)
                    (md5 (current-buffer)))))
          (sort (directory-files dir t "\\.org\\'") #'string<)))

(defun org-canvas-dry-run--kill-buffers-under (dir)
  "Kill buffers visiting files under DIR without prompting."
  (let ((kill-buffer-query-functions nil))
    (dolist (buf (buffer-list))
      (let ((bf (buffer-file-name buf)))
        (when (and bf (string-prefix-p dir bf))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))))

(defmacro org-canvas-dry-run--with-course (dir-var &rest body)
  "Copy the demo course to a temp dir, bind DIR-VAR to it, and run BODY.
Every `org-canvas-*-file' variable is pointed at the copy, so a sync
that writes despite the dry run damages the throwaway tree and is
caught by the checksum comparison instead of dirtying the repo."
  (declare (indent 1))
  `(let* ((tmp-root (make-temp-file "org-canvas-dry-run-" t))
          (,dir-var (expand-file-name "demo-course" tmp-root)))
     (unwind-protect
         (progn
           (copy-directory org-canvas-dry-run--demo-dir ,dir-var t t t)
           (let ((org-canvas-directory ,dir-var)
                 (org-canvas-assignments-file (expand-file-name "assignments.org" ,dir-var))
                 (org-canvas-quizzes-file (expand-file-name "quizzes.org" ,dir-var))
                 (org-canvas-new-quizzes-file (expand-file-name "new-quizzes.org" ,dir-var))
                 (org-canvas-pages-file (expand-file-name "pages.org" ,dir-var))
                 (org-canvas-modules-file (expand-file-name "modules.org" ,dir-var))
                 (org-canvas-rubrics-file (expand-file-name "rubrics.org" ,dir-var))
                 (org-canvas-outcomes-file (expand-file-name "outcomes.org" ,dir-var))
                 (org-canvas-discussions-file (expand-file-name "discussions.org" ,dir-var))
                 (org-canvas-announcements-file (expand-file-name "announcements.org" ,dir-var))
                 (org-canvas-assignment-groups-file (expand-file-name "assignment-groups.org" ,dir-var))
                 (org-canvas-files-file (expand-file-name "files.org" ,dir-var))
                 (org-canvas-group-categories-file (expand-file-name "group-categories.org" ,dir-var))
                 (org-canvas-calendar-events-file (expand-file-name "calendar.org" ,dir-var))
                 (org-canvas-sections-file (expand-file-name "sections.org" ,dir-var))
                 (org-canvas-settings-file (expand-file-name "settings.org" ,dir-var)))
             ,@body))
       (org-canvas-dry-run--kill-buffers-under tmp-root)
       (delete-directory tmp-root t))))

(defun org-canvas-dry-run--exercise (command dir)
  "Run COMMAND as a dry run against the course in DIR.
Returns a plist of (:writes :log :before :after).  COMMAND is allowed to
error -- the stubbed API returns nothing, so some preflights give up --
because the assertions are about what it did on the way, not whether it
finished."
  (let ((org-canvas-dry-run--calls nil)
        (org-canvas-dry-run--log nil)
        (before (org-canvas-dry-run--org-checksums dir)))
    (with-org-canvas-test-config
      (with-sync-test-env
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   #'org-canvas-dry-run--record)
                  ((symbol-function 'org-canvas--log-info)
                   #'org-canvas-dry-run--capture-log)
                  ((symbol-function 'message) (lambda (&rest _) nil))
                  ((symbol-function 'sleep-for) (lambda (&rest _) nil)))
          (let ((org-canvas--dry-run t))
            (ignore-errors (funcall command))))))
    ;; Flush anything a sync left in a buffer but had no business writing:
    ;; the checksums must reflect the files as a later Emacs would find them.
    (dolist (buf (buffer-list))
      (let ((bf (buffer-file-name buf)))
        (when (and bf (string-prefix-p dir bf) (buffer-modified-p buf))
          (with-current-buffer buf (save-buffer)))))
    (list :writes (seq-remove (lambda (call) (eq (car call) 'GET))
                              org-canvas-dry-run--calls)
          :log org-canvas-dry-run--log
          :before before
          :after (org-canvas-dry-run--org-checksums dir))))

(describe "dry run (issue #34)"

  (it "covers every sync entry point"
    ;; Guards against a feature being renamed or added without being
    ;; listed here, which would silently shrink this suite's reach.
    (dolist (cmd org-canvas-dry-run--sync-commands)
      (expect (list cmd (fboundp cmd)) :to-equal (list cmd t))))

  (dolist (command org-canvas-dry-run--sync-commands)
    (it (format "%s issues no non-GET request and writes no org file" command)
      (org-canvas-dry-run--with-course dir
        (let ((result (org-canvas-dry-run--exercise command dir)))
          ;; The bug: DELETE /api/v1/files/:id during a preview.
          (expect (plist-get result :writes) :to-equal nil)
          ;; And no CANVAS_ID / LAST_SYNCED / PAYLOAD_HASH left behind.
          (expect (plist-get result :after)
                  :to-equal (plist-get result :before))))))

  ;; The checks above pass trivially for a sync that dies before reaching its
  ;; push, so pin down that the two hand-written loops -- the ones that
  ;; bypassed the shared guard -- really did walk their entries.
  (it "reaches the push path in the files module"
    (org-canvas-dry-run--with-course dir
      (let* ((result (org-canvas-dry-run--exercise 'org-canvas-sync-files dir))
             (dry-lines (seq-filter (lambda (l) (string-match-p "\\[DRY-RUN\\]" l))
                                    (plist-get result :log))))
        (expect (seq-filter (lambda (l) (string-match-p "Would REPLACE" l)) dry-lines)
                :not :to-equal nil))))

  (it "reaches the push path in the overrides sync"
    (org-canvas-dry-run--with-course dir
      (let* ((result (org-canvas-dry-run--exercise 'org-canvas-sync-overrides dir))
             (dry-lines (seq-filter (lambda (l) (string-match-p "\\[DRY-RUN\\]" l))
                                    (plist-get result :log))))
        (expect (seq-filter (lambda (l) (string-match-p "Would CREATE override" l)) dry-lines)
                :not :to-equal nil)))))

(provide 'org-canvas-dry-run-test)
;;; org-canvas-dry-run-test.el ends here
