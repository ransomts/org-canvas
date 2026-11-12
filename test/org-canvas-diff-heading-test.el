;;; org-canvas-diff-heading-test.el --- Tests for the drift check by heading -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Covers `org-canvas-diff-heading' (issue #670): the drift check of the
;; heading at point and of a named list, which reads each heading's item
;; on its own and gives it the row the full report would, counts a
;; heading it could not check, writes nothing, and answers the command
;; line's `diff FEATURE:TITLE' and `--json diff FEATURE:TITLE'.  No spec
;; reaches the network: the item read is stubbed, and `with-mock-api'
;; refuses anything else.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-batch)

;;;; Fixtures

(defconst test-diff-heading--assignments
  (concat "#+TITLE: Assignments\n"
          "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:POINTS: 10\n"
          ":CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:END:\nLab one.\n"
          "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 61\n"
          ":CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:END:\nLab two.\n"
          "* Lab 3\nNot pushed yet.\n")
  "Two stamped assignment headings and one the next push creates.")

(defconst test-diff-heading--quizzes
  (concat "#+TITLE: Quizzes\n"
          "* Quiz 1\n:PROPERTIES:\n:CANVAS_ID: 70\n"
          ":CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:END:\n"
          "** Question one\nWhat?\n")
  "One stamped quiz with a question under it.")

(defconst test-diff-heading--lab-1
  '((id . 60) (name . "Lab 1") (points_possible . 12)
    (updated_at . "2026-10-14T15:04:51Z"))
  "Lab 1 as Canvas holds it: worth 12, and edited after the stamp.")

(defmacro test-diff-heading--with-course (&rest body)
  "Run BODY in a course holding the fixtures, the preflight check stubbed.
FILE is bound to assignments.org's path.  The API is mocked, since the
assignment comparison reads its document processors over GraphQL."
  (declare (indent 0))
  `(with-test-course (dir :files (list (cons "assignments.org"
                                             test-diff-heading--assignments)
                                       (cons "quizzes.org"
                                             test-diff-heading--quizzes)))
     (let ((file (expand-file-name "assignments.org" dir)))
       (ignore file)
       (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore))
         (with-mock-api ,@body)))))

(defun test-diff-heading--items (alist)
  "Return a stub for `org-canvas--pull-item-read' answering from ALIST.
ALIST maps an id string to the item, `gone', or `boom' (signal); an
id it lacks is the item with only that id and the name Lab 2.  Each
id read is pushed onto `test-diff-heading--read'."
  (lambda (_feature id)
    (push id test-diff-heading--read)
    (let ((item (alist-get id alist nil nil #'equal)))
      (pcase item
        ('boom (error "Failed: Authorization: Bearer sekrit123456"))
        ('nil `((id . ,(string-to-number id)) (name . "Lab 2")
                (updated_at . "2026-08-01T00:00:00Z")))
        (_ item)))))

(defvar test-diff-heading--read nil
  "The ids the stubbed item read was asked for, newest first.")

(defun test-diff-heading--at (file title)
  "Return the drift check's text and count for heading TITLE of FILE.
As (TEXT . COUNT), the report printed as under batch."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (goto-char (point-min))
    (re-search-forward (concat "^\\*+ " (regexp-quote title) "$"))
    (let (count)
      (cons (with-output-to-string (setq count (org-canvas-diff-at-point)))
            count))))

(defun test-diff-heading--kill-report ()
  "Kill the report buffer a spec left, so the next starts without one."
  (when-let* ((buffer (get-buffer org-canvas--diff-heading-buffer-name)))
    (kill-buffer buffer)))

(defun test-diff-heading--disk (file)
  "Return FILE's text as it is on disk."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;;;; At Point

(describe "org-canvas-diff-at-point (issue #670)"
  (before-each (setq test-diff-heading--read nil))
  (after-each (test-diff-heading--kill-report))

  (it "gives a drifted heading its CHANGED row and counts it, writing nothing"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items `(("60" . ,test-diff-heading--lab-1)))))
        (let* ((before (test-diff-heading--disk file))
               (result (test-diff-heading--at file "Lab 1")))
          (expect (cdr result) :to-equal 1)
          (expect (car result) :to-match "Assignments 'Lab 1' (id 60): 1 divergence(s)")
          (expect (car result) :to-match "CHANGED   Lab 1 (Canvas updated 2026-10-14T15:04:51Z)")
          (expect (car result) :to-match "POINTS +org: 10 +canvas: 12")
          (expect (car result) :to-match "1 divergence(s) found")
          (expect test-diff-heading--read :to-equal '("60"))
          (expect (test-diff-heading--disk file) :to-equal before)
          (expect (buffer-modified-p (org-canvas--find-file-noselect file)) :to-be nil)))))

  (it "gives the row the full report gives the same heading"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items `(("60" . ,test-diff-heading--lab-1))))
                ((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (vector test-diff-heading--lab-1))))
        (let* ((feature (org-canvas--diff-find-feature "Assignments"))
               (full (car (plist-get (org-canvas--diff-feature feature) :divergences)))
               (one (with-current-buffer (org-canvas--find-file-noselect file)
                      (goto-char (point-min))
                      (re-search-forward "^\\* Lab 1$")
                      (car (plist-get (org-canvas--diff-heading-here feature)
                                      :divergences)))))
          (expect (plist-get one :title) :to-equal (plist-get full :title))
          (expect (plist-get one :fields) :to-equal (plist-get full :fields))
          (expect (plist-get one :remote-newer) :to-equal (plist-get full :remote-newer))))))

  (it "says no drift and returns zero for a heading Canvas agrees with"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items nil)))
        (let ((result (test-diff-heading--at file "Lab 2")))
          (expect (cdr result) :to-equal 0)
          (expect (car result) :to-match "Assignments 'Lab 2' (id 61): no drift")
          (expect (car result) :to-match "No drift in 1 heading(s) checked")))))

  (it "names a heading Canvas no longer holds MISSING"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items '(("61" . gone)))))
        (let ((result (test-diff-heading--at file "Lab 2")))
          (expect (cdr result) :to-equal 1)
          (expect (car result) :to-match "MISSING   Lab 2 (id 61 is not in this course)")))))

  (it "lists an unstamped heading PENDING without reading or counting it"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items nil)))
        (let ((result (test-diff-heading--at file "Lab 3")))
          (expect (cdr result) :to-equal 0)
          (expect (car result) :to-match "Assignments 'Lab 3': not on Canvas yet")
          (expect (car result) :to-match "PENDING   Lab 3 (no CANVAS_ID; the next sync creates it)")
          (expect test-diff-heading--read :to-be nil)))))

  (it "counts a heading whose read failed, the error redacted"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items '(("60" . boom)))))
        (let ((result (test-diff-heading--at file "Lab 1")))
          (expect (cdr result) :to-equal 1)
          (expect (car result) :to-match "Lab 1' (id 60): could not check")
          (expect (car result) :not :to-match "sekrit123456")
          (expect (car result) :to-match "1 heading(s) could not be checked")))))

  (it "checks a quiz question's quiz, which pushes whole"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items
                  '(("70" . ((id . 70) (title . "Quiz 1")
                             (updated_at . "2026-08-01T00:00:00Z")))))))
        (let ((result (test-diff-heading--at org-canvas-quizzes-file "Question one")))
          (expect (car result) :to-match "Quizzes 'Quiz 1' (id 70): no drift")
          (expect test-diff-heading--read :to-equal '("70"))))))

  (it "refuses a buffer that is not a course file"
    (with-temp-buffer
      (org-mode)
      (insert "* Heading\n")
      (expect (org-canvas-diff-at-point) :to-throw 'user-error))))

;;;; Named Headings

(describe "org-canvas-diff-headings (issue #670)"
  (before-each (setq test-diff-heading--read nil))
  (after-each (test-diff-heading--kill-report))

  (it "checks each named heading by title or id, one not found counted"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items `(("60" . ,test-diff-heading--lab-1)))))
        (let (count)
          (let ((text (with-output-to-string
                        (setq count (org-canvas-diff-headings
                                     '(("assignment" . "Lab 1")
                                       ("assignments" "61" canvas-id)
                                       ("assignment" . "Nope")))))))
            (expect count :to-equal 2)
            (expect text :to-match "'Lab 1' (id 60): 1 divergence(s)")
            (expect text :to-match "'Lab 2' (id 61): no drift")
            (expect text :to-match "Assignments 'Nope': could not check (No Assignments heading titled \"Nope\"")
            (expect (reverse test-diff-heading--read) :to-equal '("60" "61")))))))

  (it "names the features when one is misspelt, before reading anything"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items nil)))
        (expect (org-canvas-diff-headings '(("assignment" . "Lab 1")
                                            ("frobnicate" . "X")))
                :to-throw 'user-error)
        (expect test-diff-heading--read :to-be nil))))

  (it "returns the documented JSON object, writing nothing"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items `(("60" . ,test-diff-heading--lab-1)
                                             ("61" . boom)))))
        (let* ((org-canvas-course-id 42)
               (data (org-canvas-diff-headings-json-data
                      '(("assignment" . "Lab 1") ("assignment" . "Lab 2")
                        ("assignment" . "Lab 3"))))
               (headings (alist-get 'headings data))
               (rows (alist-get 'rows data)))
          (expect (mapcar #'car data)
                  :to-equal '(command course course_id drift counts headings rows))
          (expect (alist-get 'course_id data) :to-equal "42")
          (expect (alist-get 'drift data) :to-equal 2)
          (expect (alist-get 'changed (alist-get 'counts data)) :to-equal 1)
          (expect (alist-get 'pending (alist-get 'counts data)) :to-equal 1)
          (expect (mapcar (lambda (h) (alist-get 'state h)) headings)
                  :to-equal '("checked" "error" "checked"))
          (expect (alist-get 'canvas_id (aref headings 0)) :to-equal "60")
          (expect (alist-get 'error (aref headings 1)) :not :to-match "sekrit")
          (expect (alist-get 'status (aref rows 0)) :to-equal "CHANGED")
          (expect (get-buffer org-canvas--diff-heading-buffer-name) :to-be nil)))))

  (it "shows the rows in the report mode, and g checks the same headings again"
    (test-diff-heading--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (test-diff-heading--items nil))
                ((symbol-function 'display-buffer) #'ignore)
                ((symbol-function 'message) #'ignore))
        (let ((noninteractive nil))
          (org-canvas-diff-headings '(("assignment" . "Lab 2")))
          (with-current-buffer org-canvas--diff-heading-buffer-name
            (expect major-mode :to-be 'org-canvas-diff-mode)
            (expect (buffer-string) :to-match "RET visit")
            (setq test-diff-heading--read nil)
            (org-canvas-diff-refresh)
            (expect test-diff-heading--read :to-equal '("61")))
          (with-current-buffer (org-canvas--find-file-noselect file)
            (goto-char (point-min))
            (re-search-forward "^\\* Lab 2$")
            (org-canvas-diff-at-point))
          (with-current-buffer org-canvas--diff-heading-buffer-name
            (setq test-diff-heading--read nil)
            (revert-buffer)
            (expect test-diff-heading--read :to-equal '("61"))))))))

;;;; The Command Line

(describe "scripts/org-canvas diff FEATURE:TITLE (issue #670)"
  (defun test-diff-heading--batch (args)
    "Return (STATUS . STDOUT) of the command line ARGS, the setup mocked."
    (let (status)
      (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
                ((symbol-function 'message) #'ignore))
        (let ((out (with-output-to-string (setq status (org-canvas-batch-main args)))))
          (cons status out)))))

  (it "checks the named headings and exits with their verdict"
    (let ((count 0) (seen nil))
      (cl-letf (((symbol-function 'org-canvas-diff-headings)
                 (lambda (entries) (setq seen entries) count))
                ((symbol-function 'org-canvas-diff) (lambda () (error "Not the full report"))))
        (expect (car (test-diff-heading--batch '("diff" "assignment:Lab 1" "file#99")))
                :to-equal 0)
        (expect seen :to-equal '(("assignment" "Lab 1" title) ("file" "99" canvas-id)))
        (setq count 2)
        (expect (car (test-diff-heading--batch '("diff" "assignment:Lab 1"))) :to-equal 1))))

  (it "prints the headings' JSON object under --json"
    (cl-letf (((symbol-function 'org-canvas-diff-headings-json-data)
               (lambda (_entries) '((command . "diff") (drift . 1))))
              ((symbol-function 'org-canvas-diff-json-data)
               (lambda () (error "Not the full report"))))
      (let ((result (test-diff-heading--batch '("--json" "diff" "assignment:Lab 1"))))
        (expect (car result) :to-equal 1)
        (expect (cdr result) :to-match "\"drift\":1"))))

  (it "refuses an argument that names no heading"
    (expect (car (test-diff-heading--batch '("diff" "extra"))) :to-equal 2)))

;;; org-canvas-diff-heading-test.el ends here
