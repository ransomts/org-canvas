;;; test-fixtures.el --- Shared course and API fixtures for org-canvas tests  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The two things almost every spec needs, and that each feature test
;; file used to build for itself (issue #574):
;;
;; - `with-test-course' makes a temporary course directory from an alist
;;   of file names and contents, and binds every `org-canvas-*-file'
;;   variable into it from `org-canvas--file-var-registry' -- the same
;;   set `with-validate-test-dir' binds -- so a spec names only the
;;   files it cares about and never reads the developer's own course.
;; - `with-test-api' and its variants are the one mocked-Canvas harness.
;;   They compose `with-org-canvas-test-config' and `with-mock-api'
;;   instead of every file repeating the pair.
;;
;; File contents are built with `test-org-canvas-heading' and
;; `test-org-canvas-headings', for the "* HW" over ":PROPERTIES:" shape
;; most fixtures hand-wrote, and read back with
;; `test-org-canvas-course-prop'.
;;
;; This lives beside test-helper.el rather than inside it: the helper is
;; the suite's machinery (guards, network refusal, generators) and
;; already long, while this is one layer of fixtures that more files
;; will move onto a few at a time.  test-helper.el loads it after the
;; package, so the registry is complete; test-fixtures.el requires
;; nothing of test-helper at load time, which is what keeps the two from
;; loading each other recursively.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Org content builders

(defun test-org-canvas-heading (title &rest spec)
  "Return TITLE as one Org entry, with the drawer and body SPEC gives.
SPEC is a plist:
  :props  alist of (NAME . VALUE); a nil VALUE leaves the property out,
          and nil writes no drawer at all
  :body   text inserted after the drawer (default none)
  :level  headline depth (default 1)

The shape most specs need -- \"* HW\" over a \":PROPERTIES:\" drawer --
is (test-org-canvas-heading \"HW\" :props \\='((\"CANVAS_ID\" . \"61\")))."
  (let ((props (plist-get spec :props))
        (body (or (plist-get spec :body) ""))
        (level (or (plist-get spec :level) 1)))
    (concat (make-string level ?*)
            " " title "\n"
            (when props
              (concat ":PROPERTIES:\n"
                      (mapconcat #'identity
                                 (delq nil
                                       (mapcar (lambda (pair)
                                                 (when (cdr pair)
                                                   (format ":%s: %s\n"
                                                           (car pair) (cdr pair))))
                                               props))
                                 "")
                      ":END:\n"))
            body)))

(defun test-org-canvas-headings (entries)
  "Return ENTRIES, a list of `test-org-canvas-heading' arguments, as text.
Each entry is (TITLE :props ALIST :body TEXT :level N), so
\(test-org-canvas-headings
 \\='((\"Lab 1\" :props ((\"CANVAS_ID\" . \"61\")))
   (\"Lab 2\" :props ((\"CANVAS_ID\" . \"62\")))))
is a two-assignment file."
  (mapconcat (lambda (entry) (apply #'test-org-canvas-heading entry))
             entries ""))

(defun test-org-canvas-course-contents (value)
  "Return VALUE, a `with-test-course' file's content, as text to write.
VALUE is a string, nil for an empty file, or a plist of :prefix,
:headings and :suffix, where :headings is passed to
`test-org-canvas-headings'."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((eq (car-safe value) :headings)
    (concat (or (plist-get value :prefix) "")
            (test-org-canvas-headings (plist-get value :headings))
            (or (plist-get value :suffix) "")))
   (t (error "Cannot write a course file from %S" value))))

;;;; Course directory

(defun test-org-canvas-course-write (dir files)
  "Write FILES into DIR, creating parent directories as needed.
FILES is an alist of (RELATIVE-NAME . CONTENT).  A name ending in a
slash is made a directory; otherwise CONTENT goes through
`test-org-canvas-course-contents' into the file."
  (dolist (entry files)
    (let* ((name (car entry))
           (path (expand-file-name name dir)))
      (if (string-suffix-p "/" name)
          (make-directory path t)
        (make-directory (file-name-directory path) t)
        (with-temp-file path
          (insert (test-org-canvas-course-contents (cdr entry))))))))

(defun test-org-canvas-course-cleanup (dir)
  "Kill the buffers visiting files under DIR, then delete DIR.
A buffer left visiting a deleted file keeps one spec's course for the
next one and holds a lock; `with-temp-org-buffer' kills its buffer for
the same reason."
  (let ((root (directory-file-name (file-truename dir))))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-file-name buffer)))
        (when (and file
                   (string-prefix-p (concat root "/")
                                    (directory-file-name (file-truename file))))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))))
  (when (file-exists-p dir)
    (delete-directory dir t)))

(defun test-org-canvas-course-bindings (dir)
  "Return the course bindings for DIR as (VARIABLES . VALUES).
The variables are `org-canvas-directory' and every file-path variable
in `org-canvas--file-var-registry', each pointed at its own file under
DIR whether or not that file exists -- which is what keeps a spec from
reaching the developer's course through a variable it did not name."
  (cons (cons 'org-canvas-directory
              (mapcar #'car org-canvas--file-var-registry))
        (cons (file-name-as-directory dir)
              (mapcar (lambda (entry)
                        (expand-file-name (cdr entry) (directory-file-name dir)))
                      org-canvas--file-var-registry))))

(defmacro with-test-course (spec &rest body)
  "Run BODY with a temporary course directory built from SPEC.
SPEC is (DIR-VAR :files FILES), where DIR-VAR is bound to the
directory and FILES is any expression that evaluates to an alist of
\(RELATIVE-NAME . CONTENT) as `test-org-canvas-course-write' takes.
FILES may be nil, and may mention DIR-VAR -- the files are written
after it is bound.

Every `org-canvas-*-file' variable is bound to its own path under the
directory, so a spec lists only the files it cares about.  The
directory, and the buffers visiting its files, are removed after BODY.

  (with-test-course (dir :files \\='((\"assignments.org\"
                              :headings ((\"HW\" :props ((\"CANVAS_ID\" . \"61\")))))))
    (org-canvas-sync-assignments))"
  (declare (indent 1) (debug (sexp body)))
  (let ((dir-var (car spec))
        (files (plist-get (cdr spec) :files)))
    (unless (and (symbolp dir-var) dir-var)
      (error "with-test-course: SPEC must start with the directory variable"))
    `(let ((,dir-var (make-temp-file "org-canvas-course-" t)))
       (unwind-protect
           (progn
             (test-org-canvas-course-write ,dir-var ,files)
             (let ((test-org-canvas--course-bindings
                    (test-org-canvas-course-bindings ,dir-var)))
               (cl-progv (car test-org-canvas--course-bindings)
                         (cdr test-org-canvas--course-bindings)
                 ,@body)))
         (test-org-canvas-course-cleanup ,dir-var)))))

(defun test-org-canvas-course-prop (dir file heading property)
  "Return PROPERTY of HEADING in DIR/FILE, or nil.
HEADING is matched anywhere in a headline's text, so both a link's
description and the title it names find the entry."
  (with-current-buffer (find-file-noselect (expand-file-name file dir))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^\\*+ .*%s" (regexp-quote heading)) nil t)
        (org-back-to-heading t)
        (org-entry-get (point) property)))))

(defun test-org-canvas-course-heading-pos (dir file title)
  "Return the position of the headline TITLE in DIR/FILE.
TITLE is matched anywhere in a headline's text, as in
`test-org-canvas-course-prop'."
  (with-current-buffer (find-file-noselect (expand-file-name file dir))
    (goto-char (point-min))
    (re-search-forward (format "^\\*+ .*%s" (regexp-quote title)))
    (org-back-to-heading t)
    (point)))

;;;; API mocking

;; `with-mock-api' (test-helper.el) answers from
;; `test-org-canvas-api-responses' unless `test-org-canvas-api-handler'
;; is non-nil, in which case that function answers the request.  The
;; recording is the same either way, so `test-org-canvas-api-called-p',
;; `test-org-canvas-api-call-count' and `expect-api-called' work for
;; every variant below.

(defmacro with-test-api (&rest body)
  "Run BODY with the test Canvas configuration and a mocked API.
`with-org-canvas-test-config' and `with-mock-api' are the two things a
spec that talks to Canvas always wants together; this is the one place
they are paired."
  (declare (indent 0))
  `(with-org-canvas-test-config
     (with-mock-api
       ,@body)))

(defmacro with-test-api-responses (responses &rest body)
  "Run BODY like `with-test-api', answering from the RESPONSES table.
RESPONSES is an alist of (URL-PATTERN . RESPONSE) as
`test-org-canvas-api-responses' takes; the first pattern that matches
the URL answers, and anything unmatched gets the canned default."
  (declare (indent 1))
  `(with-test-api
     (let ((test-org-canvas-api-responses ,responses))
       ,@body)))

(defmacro with-test-api-handler (handler &rest body)
  "Run BODY like `with-test-api', answering each request by calling HANDLER.
HANDLER receives METHOD, URL and the keyword arguments
`org-canvas-api-request' was called with, the same convention the
`cl-letf' forms this replaces use, and its return value is the response
-- nil included, so a hand-written dispatcher keeps answering for the
URLs it does not know."
  (declare (indent 1))
  `(with-test-api
     (let ((test-org-canvas-api-handler ,handler))
       ,@body)))

(defmacro with-test-sync (&rest body)
  "Run BODY like `with-test-api', with sync's own side effects silenced.
Composes `with-sync-test-env' with `with-test-api'."
  (declare (indent 0))
  `(with-sync-test-env
     (with-test-api
       ,@body)))

(defmacro with-test-sync-handler (handler &rest body)
  "Run BODY like `with-test-api-handler', with sync's side effects silenced.
Composes `with-sync-test-env' with `with-test-api-handler'."
  (declare (indent 1))
  `(with-sync-test-env
     (with-test-api-handler ,handler
       ,@body)))

(provide 'test-fixtures)
;;; test-fixtures.el ends here
