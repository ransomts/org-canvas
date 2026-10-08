;;; org-canvas-local-file-links-test.el --- Tests for body links to local files  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Issue #468: a body link to a local non-image file was exported by
;; ox-html as a relative href that 404s on Canvas.  A push now sends
;; the Canvas URL of the files.org heading whose link target is the
;; same file, warns and sends plain text when there is none, and
;; validate flags the link beforehand.  The URL is the shape the pull
;; rewrites back to the same content/ link, so pull and push round-trip.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defconst test-local-files-org
  "#+TITLE: Files
* Handouts
** [[file:content/handout.pdf][handout.pdf]]
:PROPERTIES:
:CANVAS_ID: 777
:END:
** [[file:content/notes \\[v2\\].txt][notes.txt]]
:PROPERTIES:
:CANVAS_ID: 778
:END:
** [[file:content/draft.docx][draft.docx]]
"
  "A files.org with an uploaded file, a bracketed name and an unsynced file.")

(defmacro test-local-files-with-course (&rest body)
  "Run BODY in a temporary course, with DIR bound to its directory.
files.org holds `test-local-files-org'; pages live one level down in
DIR/pages, so a body's relative paths differ from files.org's."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory
                (file-truename (make-temp-file "org-local-files-" t))))
          (org-canvas-files-file (expand-file-name "files.org" dir))
          (org-canvas--local-file-id-cache nil))
     (unwind-protect
         (with-org-canvas-test-config
           (make-directory (expand-file-name "pages" dir))
           (make-directory (expand-file-name "content" dir))
           (with-temp-file org-canvas-files-file
             (insert test-local-files-org))
           ,@body)
       (dolist (buf (buffer-list))
         (let ((file (buffer-file-name buf)))
           (when (and file (string-prefix-p dir (file-truename file)))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-local-files-resolve (text dir)
  "Return TEXT after `org-canvas--resolve-file-links' from DIR.
The warnings logged are returned too, as (TEXT . WARNINGS)."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'org-canvas--log-warning)
               (lambda (_logger fmt &rest args)
                 (push (apply #'format fmt args) warnings)))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (insert text)
        (org-canvas--resolve-file-links dir)
        (cons (buffer-string) (nreverse warnings))))))

(describe "org-canvas--resolve-file-links (issue #468)"
  (it "sends the Canvas URL of the files.org heading for the same file"
    (test-local-files-with-course
      (let ((result (test-local-files-resolve
                     "See [[file:../content/handout.pdf][the handout]]."
                     (expand-file-name "pages" dir))))
        (expect (car result)
                :to-equal
                "See [[https://test.canvas.example.com/courses/99999/files/777][the handout]].")
        (expect (cdr result) :to-be nil))))

  (it "uses the file name when the link has no description"
    (test-local-files-with-course
      (expect (car (test-local-files-resolve "[[file:content/handout.pdf]]" dir))
              :to-equal
              "[[https://test.canvas.example.com/courses/99999/files/777][handout.pdf]]")))

  (it "drops a search option and undoes bracket escapes before matching"
    (test-local-files-with-course
      (expect (car (test-local-files-resolve
                    "[[file:content/handout.pdf::3][p3]] [[file:content/notes \\[v2\\].txt][n]]"
                    dir))
              :to-equal
              (concat "[[https://test.canvas.example.com/courses/99999/files/777][p3]] "
                      "[[https://test.canvas.example.com/courses/99999/files/778][n]]"))))

  (it "sends plain text and warns for a file files.org has not uploaded"
    (test-local-files-with-course
      (let ((result (test-local-files-resolve
                     "Read [[file:../content/draft.docx][the draft]]."
                     (expand-file-name "pages" dir))))
        (expect (car result) :to-equal "Read the draft.")
        (expect (car (cdr result)) :to-match "sync files first"))))

  (it "sends plain text and warns for a file no files.org heading links to"
    (test-local-files-with-course
      (with-temp-file (expand-file-name "content/extra.pdf" dir) (insert "x"))
      (let ((result (test-local-files-resolve
                     "[[file:content/extra.pdf][extra]]" dir)))
        (expect (car result) :to-equal "extra")
        (expect (car (cdr result)) :to-match "no files.org heading links to it"))))

  (it "names a missing file as missing"
    (test-local-files-with-course
      (let ((result (test-local-files-resolve "[[file:content/gone.pdf]]" dir)))
        (expect (car result) :to-equal "gone.pdf")
        (expect (car (cdr result)) :to-match "no such file"))))

  (it "warns for every link when there is no files.org"
    (let ((org-canvas-files-file "/tmp/nonexistent/files.org"))
      (let ((result (test-local-files-resolve "[[file:a.pdf][A]]" "/tmp/")))
        (expect (car result) :to-equal "A")
        (expect (length (cdr result)) :to-equal 1))))

  (it "leaves image and Org file links to their own passes"
    (test-local-files-with-course
      (let ((text "[[file:img.png]] [[file:pages.org][P]] [[https://x.example/a.pdf][web]]"))
        (expect (test-local-files-resolve text dir)
                :to-equal (cons text nil)))))

  (it "builds the map again once a sync stamps a CANVAS_ID"
    (test-local-files-with-course
      (expect (car (test-local-files-resolve "[[file:content/draft.docx][d]]" dir))
              :to-equal "d")
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-files-file)
        (goto-char (point-min))
        (re-search-forward "draft.docx")
        (org-entry-put (point) "CANVAS_ID" "779"))
      (expect (car (test-local-files-resolve "[[file:content/draft.docx][d]]" dir))
              :to-match "/files/779\\]"))))

(describe "org-canvas--export-subtree-body-to-html and local files (issue #468)"
  (it "exports an href Canvas can open, not a relative path"
    (test-local-files-with-course
      (let ((page (expand-file-name "pages/pages.org" dir)))
        (with-temp-file page
          (insert "* Week 1\n:PROPERTIES:\n:END:\n\nGet [[file:../content/handout.pdf][the handout]].\n"))
        (with-current-buffer (org-canvas--find-file-noselect page)
          (goto-char (point-min))
          (let ((html (org-canvas--export-subtree-body-to-html)))
            (expect html :to-match
                    "href=\"https://test.canvas.example.com/courses/99999/files/777\"")
            (expect html :not :to-match "content/handout.pdf"))))))

  (it "round-trips with the pull's rewrite to the same content/ link"
    (test-local-files-with-course
      (let* ((pushed (car (test-local-files-resolve
                           "[[file:content/handout.pdf][the handout]]" dir)))
             (cache (org-canvas--build-file-id-cache org-canvas-files-file)))
        (expect (org-canvas--rewrite-canvas-file-urls pushed cache)
                :to-equal "[[file:content/handout.pdf][the handout]]")))))

(describe "org-canvas--validate-local-file-links (issue #468)"
  (it "flags the links a push cannot resolve, and only in body text"
    (test-local-files-with-course
      (with-temp-file (expand-file-name "content/extra.pdf" dir) (insert "x"))
      (let ((page (expand-file-name "pages/pages.org" dir)))
        (with-temp-file page
          (insert "* Week 1\n"
                  ":PROPERTIES:\n:NOTE: [[file:../content/extra.pdf][x]]\n:END:\n"
                  "[[file:../content/handout.pdf][ok]]\n"
                  "[[file:../content/extra.pdf][extra]]\n"
                  "[[file:../content/draft.docx][draft]]\n"
                  "** [[file:../content/extra.pdf][a heading link]]\n"))
        (let* ((issues (org-canvas--validate-local-file-links page))
               (extra (nth 0 issues))
               (draft (nth 1 issues)))
          (expect (length issues) :to-equal 2)
          (expect (plist-get extra :severity) :to-equal 'warning)
          (expect (plist-get extra :line) :to-equal 6)
          (expect (plist-get extra :heading) :to-equal "Week 1")
          (expect (plist-get extra :message)
                  :to-match "link to local file ../content/extra.pdf: no files.org heading")
          (expect (plist-get extra :push-only) :to-be t)
          (expect (plist-get extra :pending-sync) :to-be nil)
          (expect (plist-get draft :message) :to-match "sync files first")
          (expect (plist-get draft :pending-sync) :to-be t)))))

  (it "joins the validation run"
    (test-local-files-with-course
      (let ((page (expand-file-name "pages.org" dir)))
        (with-temp-file page
          (insert "* Week 1\n\n[[file:content/gone.pdf][gone]]\n"))
        (with-nonexistent-canvas-files
          (let* ((org-canvas-pages-file page)
                 (issues (plist-get (org-canvas--validate-run-all-specs) :issues)))
            (expect (cl-count-if (lambda (i)
                                   (string-match-p "local file content/gone.pdf"
                                                   (plist-get i :message)))
                                 issues)
                    :to-equal 1)))))))

;;; org-canvas-local-file-links-test.el ends here
