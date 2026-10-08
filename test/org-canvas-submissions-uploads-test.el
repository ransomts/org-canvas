;;; org-canvas-submissions-uploads-test.el --- Tests for upload text and pairing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-upload-text' (issue #457): the
;; text of a column's downloaded uploads, students grouped by shared
;; upload with SAME_AS, and the refresh line naming attachments that
;; are not on disk.  pdftotext is never run: `executable-find' and
;; `call-process' are stubbed.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-uploads)

(defconst test-uploads-doc "1AbCdEfGhIjKlMnOpQrStUvWxYz0123456789"
  "A Google Doc id the pair shares.")

(defconst test-uploads-template "1TemplateTemplateTemplateTemplate00"
  "A Google Doc id every student's upload links: the instructions.")

(defun test-uploads-student (name uid &rest files)
  "Return a grading-file heading NAME (USER_ID UID) linking FILES.
Each of FILES is (FILENAME . LOCAL-P); a local one links its copy."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n:ATTEMPT: 1\n:END:\n" name uid)
          (if files "\n** Attachments\n" "")
          (mapconcat
           (lambda (f)
             (let ((url (format "https://canvas.example.com/files/%s/download" (car f))))
               (if (cdr f)
                   (format "- [[file:files/HW/%s/%s][%s]] ([[%s][Canvas]])\n"
                           (org-canvas--submissions-sanitize-filename name)
                           (car f) (car f) url)
                 (format "- [[%s][%s]]\n" url (car f)))))
           files "")
          "\n"))

(defconst test-uploads-grading
  (concat "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
          "#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n\n"
          (test-uploads-student "Adams, Alice" 5001 '("a.pdf" . t))
          (test-uploads-student "Beta, Bob" 5002 '("b.pdf" . t))
          (test-uploads-student "Cruz, Cal" 5003 '("c.pdf" . t))
          (test-uploads-student "Diaz, Dee" 5004 '("d.pdf" . t))
          (test-uploads-student "Eve, Ed" 5005 '("e.txt" . t))
          (test-uploads-student "Fox, Fay" 5006 '("f.png" . t) '("g.pdf"))
          (test-uploads-student "Gil, Gus" 5007)
          "* Left, Lee\n:PROPERTIES:\n:USER_ID: 5008\n:STATUS: left\n:END:\n\n"
          "** Attachments\n- [[https://canvas.example.com/files/9/download][l.pdf]]\n")
  "A grading file with a doc-id pair, a same-file pair and the odd cases.")

(defconst test-uploads-files
  `(("Adams, Alice" "a.pdf" "%PDF-1.4\0alice")
    ("Beta, Bob" "b.pdf" "%PDF-1.4\0bob")
    ("Cruz, Cal" "c.pdf" "%PDF-1.4\0same bytes")
    ("Diaz, Dee" "d.pdf" "%PDF-1.4\0same bytes")
    ("Eve, Ed" "e.txt" "* not a heading\nplain words\n")
    ("Fox, Fay" "f.png" "\211PNG\0\0"))
  "The files on disk: (STUDENT FILENAME BYTES).")

(defconst test-uploads-pdf-text
  `(("a.pdf" . ,(format "Alice and Bob\nhttps://docs.google.com/document/d/%s/edit\n" test-uploads-doc))
    ("b.pdf" . ,(format "Bob and Alice (export 2)\ndocs.google.com/document/d/%s\n" test-uploads-doc))
    ("c.pdf" . "Cal's essay\n")
    ("d.pdf" . "Cal's essay\n"))
  "What the stubbed pdftotext prints for each PDF.")

(defun test-uploads-write-files (dir)
  "Write `test-uploads-files' under DIR's files/HW/."
  (dolist (f test-uploads-files)
    (let ((folder (expand-file-name
                   (format "files/HW/%s/" (org-canvas--submissions-sanitize-filename (nth 0 f)))
                   dir)))
      (make-directory folder t)
      (let ((coding-system-for-write 'no-conversion))
        (with-temp-file (expand-file-name (nth 1 f) folder)
          (set-buffer-multibyte nil)
          (insert (nth 2 f)))))))

(defvar test-uploads-calls nil "The programs the stubbed `call-process' ran.")

(defun test-uploads-call-process (program _infile _destination _display &rest args)
  "Stand in for `call-process': print PROGRAM's text for ARGS, return 0.
pdftotext prints from `test-uploads-pdf-text'; a file named there
with no text fails with 1."
  (push (cons program args) test-uploads-calls)
  (let ((text (cdr (assoc (file-name-nondirectory (cl-find-if (lambda (a) (string-match-p "\\." a)) args)) test-uploads-pdf-text))))
    (if text (progn (insert text) 0) 1)))

(defmacro with-uploads-file (content &rest body)
  "Visit a scratch grading file HW.org holding CONTENT, files on disk, run BODY.
`dir' is the submissions directory.  pdftotext is found and stubbed;
the report comes back as text rather than displayed."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "org-canvas-uploads-" t)))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (test-uploads-calls nil)
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (test-uploads-write-files dir)
           (setq buf (find-file-noselect file))
           (with-current-buffer buf
             (org-mode)
             (org-canvas-submissions-mode 1)
             (cl-letf (((symbol-function 'executable-find)
                        (lambda (program &rest _) (and (equal program "pdftotext") "/usr/bin/pdftotext")))
                       ((symbol-function 'call-process) #'test-uploads-call-process)
                       ((symbol-function 'org-canvas--report-display)
                        (lambda (_name render &optional _mode)
                          (with-temp-buffer (funcall render) (buffer-string)))))
               ,@body)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory dir t))))

(defun test-uploads-prop (name property)
  "Return PROPERTY of the heading NAME in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s$" (regexp-quote name)))
    (org-entry-get (point) property)))

;;;; Reading One Upload

(describe "org-canvas--submissions-upload-read"
  (let ((path "/tmp/x/hw.pdf"))
    (it "reads a PDF through pdftotext -layout"
      (let ((test-uploads-calls nil)
            (test-uploads-pdf-text '(("hw.pdf" . "the text\n"))))
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/pdftotext"))
                  ((symbol-function 'call-process) #'test-uploads-call-process))
          (expect (org-canvas--submissions-upload-read path "%PDF")
                  :to-equal '(:text "the text\n"))
          (expect test-uploads-calls :to-equal `(("pdftotext" "-layout" ,path "-"))))))
    (it "says pdftotext is missing, and runs nothing"
      (cl-letf (((symbol-function 'executable-find) #'ignore)
                ((symbol-function 'call-process) (lambda (&rest _) (error "must not run"))))
        (expect (plist-get (org-canvas--submissions-upload-read path "%PDF") :note)
                :to-equal "pdftotext not found: install poppler-utils to read PDFs")))
    (it "says so when pdftotext fails on the file"
      (let ((test-uploads-pdf-text nil))
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/pdftotext"))
                  ((symbol-function 'call-process) #'test-uploads-call-process))
          (expect (org-canvas--submissions-upload-read path "%PDF")
                  :to-equal '(:note "pdftotext could not read this file"))))))
  (it "reads a .docx through pandoc, or says pandoc is missing"
    (let ((test-uploads-calls nil)
          (test-uploads-pdf-text '(("hw.docx" . "docx words"))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/pandoc"))
                ((symbol-function 'call-process) #'test-uploads-call-process))
        (expect (org-canvas--submissions-upload-read "/tmp/hw.docx" "PK\0")
                :to-equal '(:text "docx words"))
        (expect (car test-uploads-calls) :to-equal '("pandoc" "-t" "plain" "/tmp/hw.docx"))))
    (cl-letf (((symbol-function 'executable-find) #'ignore))
      (expect (org-canvas--submissions-upload-read "/tmp/hw.DOCX" "PK\0")
              :to-equal '(:note "pandoc not found: open the .docx to read it"))))
  (it "reads a text file as it is and declines a binary one"
    (expect (org-canvas--submissions-upload-read "/tmp/notes.md" "caf\303\251\n")
            :to-equal '(:text "café\n"))
    (expect (org-canvas--submissions-upload-read "/tmp/scan.png" "\211PNG\0")
            :to-equal '(:note "not a text file (.png): open it to read it"))))

(describe "org-canvas--submissions-doc-ids"
  (it "finds document, sheet, slide and published ids once each, in order"
    (expect (org-canvas--submissions-doc-ids
             (format "see https://docs.google.com/document/d/%s/edit and again docs.google.com/document/d/%s"
                     test-uploads-doc test-uploads-doc)
             nil
             "docs.google.com/spreadsheets/d/1SheetSheetSheetSheetSheet00/x docs.google.com/document/d/e/2PACX-1vPublishedPublished00/pub docs.google.com/document/d/short")
            :to-equal (list test-uploads-doc "1SheetSheetSheetSheetSheet00"
                            "2PACX-1vPublishedPublished00"))))

;;;; Grouping

(defun test-uploads-fake (name &rest keys)
  "Return a collected student NAME whose one upload carries KEYS.
KEYS are doc ids, and a symbol `hash:X' for the file hash X."
  (list :name name :user-id name
        :uploads (list (list :name "f.pdf"
                             :hash (or (cl-find-if #'symbolp keys) name)
                             :doc-ids (cl-remove-if #'symbolp keys)))))

(describe "org-canvas--submissions-upload-groups"
  (it "joins by doc id, by file, and through a member; leaves the rest out"
    (let* ((a (test-uploads-fake "A" "doc1"))
           (b (test-uploads-fake "B" "doc1" 'h))
           (c (test-uploads-fake "C" 'h))
           (d (test-uploads-fake "D" "doc2"))
           (e (test-uploads-fake "E" 'g))
           (f (test-uploads-fake "F" 'g))
           (groups (org-canvas--submissions-upload-groups (list a b c d e f))))
      (expect (mapcar (lambda (g) (mapcar (lambda (m) (plist-get m :name)) (plist-get g :members)))
                      groups)
              :to-equal '(("A" "B" "C") ("E" "F")))
      (expect (org-canvas--submissions-upload-reason (car groups)) :to-equal "Google Doc doc1")
      (expect (org-canvas--submissions-upload-reason (cadr groups)) :to-equal "the same file")
      (expect (org-canvas--submissions-upload-others b (car groups)) :to-equal "A; C")))
  (it "ignores a doc id more students carry than the limit"
    (let* ((org-canvas-submissions-same-as-max-group 2)
           (students (list (test-uploads-fake "A" "tpl" "pair")
                           (test-uploads-fake "B" "tpl" "pair")
                           (test-uploads-fake "C" "tpl"))))
      (expect (mapcar (lambda (g) (length (plist-get g :members)))
                      (org-canvas--submissions-upload-groups students))
              :to-equal '(2))
      (expect (org-canvas--submissions-upload-groups (list (test-uploads-fake "C" "tpl")))
              :to-be nil))))

;;;; The Command

(describe "org-canvas-submissions-upload-text"
  (it "shows each student's text, pairs read once, and writes SAME_AS"
    (with-uploads-file test-uploads-grading
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* Eve, Ed$")
        (org-entry-put (point) "SAME_AS" "Stale, Pair"))
      (let ((text (org-canvas-submissions-upload-text)))
        (expect (test-uploads-prop "Adams, Alice" "SAME_AS") :to-equal "Beta, Bob")
        (expect (test-uploads-prop "Beta, Bob" "SAME_AS") :to-equal "Adams, Alice")
        (expect (test-uploads-prop "Cruz, Cal" "SAME_AS") :to-equal "Diaz, Dee")
        (expect (test-uploads-prop "Eve, Ed" "SAME_AS") :to-be nil)
        (expect (test-uploads-prop "Gil, Gus" "SAME_AS") :to-be nil)
        (expect (buffer-modified-p) :to-be nil)
        (expect text :to-match "^6 student(s) with downloaded uploads, 4 distinct document(s); 2 group(s) share one$")
        (expect text :to-match (regexp-quote (format "- Same upload: Adams, Alice; Beta, Bob (Google Doc %s)" test-uploads-doc)))
        (expect text :to-match (regexp-quote "- Same upload: Cruz, Cal; Diaz, Dee (the same file)"))
        (expect text :to-match (regexp-quote "Not on disk, so not read (D in the grading file downloads them): Fox, Fay (g.pdf)"))
        (expect text :to-match "\\* Adams, Alice\nSame upload as Beta, Bob\\.\n\\*\\* a\\.pdf\n#\\+begin_example\nAlice and Bob")
        (expect text :to-match "\\* Beta, Bob\nSame upload as Adams, Alice\\.\nRead under Adams, Alice above\\.")
        (expect text :not :to-match "Bob and Alice (export 2)")
        (expect text :to-match "#\\+begin_example\n,\\* not a heading\nplain words\n#\\+end_example")
        (expect text :to-match "\\*\\* f\\.png\nnot a text file (\\.png): open it to read it\n\\*\\* g\\.pdf\nNot downloaded\\.")
        (expect text :not :to-match "Gil, Gus\\|Left, Lee"))))
  (it "says pdftotext is missing in each PDF's block and still groups by file"
    (with-uploads-file test-uploads-grading
      (cl-letf (((symbol-function 'executable-find) #'ignore))
        (let ((text (org-canvas-submissions-upload-text)))
          (expect test-uploads-calls :to-be nil)
          (expect (test-uploads-prop "Cruz, Cal" "SAME_AS") :to-equal "Diaz, Dee")
          (expect (test-uploads-prop "Adams, Alice" "SAME_AS") :to-be nil)
          (expect text :to-match "\\*\\* a\\.pdf\npdftotext not found: install poppler-utils to read PDFs")))))
  (it "writes the report beside the downloads with SAVE"
    (with-uploads-file test-uploads-grading
      (let ((text (org-canvas-submissions-upload-text t))
            (saved (expand-file-name "files/HW/uploads.org" dir)))
        (expect (file-exists-p saved) :to-be-truthy)
        (expect (with-temp-buffer (insert-file-contents saved) (buffer-string))
                :to-equal text))))
  (it "refuses outside a grading file, in the summary, and with no attachments"
    (with-temp-buffer
      (expect (org-canvas-submissions-upload-text) :to-throw 'user-error))
    (with-uploads-file test-uploads-grading
      (setq-local org-canvas-submissions--current-view 'summary)
      (expect (org-canvas-submissions-upload-text) :to-throw 'user-error))
    (with-uploads-file (concat "#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
                               (test-uploads-student "Gil, Gus" 5007))
      (expect (org-canvas-submissions-upload-text)
              :to-throw 'user-error '("No attachments in HW"))))
  (it "is on u in the grading file"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "u"))
            :to-be 'org-canvas-submissions-upload-text)))

;;;; A Copy That Is Not the Attachment (refresh side)

(describe "org-canvas--submissions-stale-copy-p"
  (it "tells an earlier attempt's copy by its size or its age"
    (let ((path (make-temp-file "org-canvas-stale-")))
      (unwind-protect
          (progn
            (with-temp-file path (insert "12345"))
            (set-file-times path (date-to-time "2026-10-01T12:00:00Z"))
            (expect (org-canvas--submissions-stale-copy-p path nil) :to-be nil)
            (expect (org-canvas--submissions-stale-copy-p path '((size . 5))) :to-be nil)
            (expect (org-canvas--submissions-stale-copy-p path '((size . 9))) :to-be-truthy)
            (expect (org-canvas--submissions-stale-copy-p
                     path '((created_at . "2026-10-02T12:00:00Z")))
                    :to-be-truthy)
            (expect (org-canvas--submissions-stale-copy-p
                     path '((size . 5) (created_at . "2026-09-30T12:00:00Z")))
                    :to-be nil))
        (delete-file path)))))

(describe "a refresh and the attachments on disk (issue #457)"
  (let ((messages nil))
    (cl-flet ((refresh (subs)
                (setq messages nil)
                (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                           (lambda (_id) subs))
                          ((symbol-function 'org-canvas--submissions-fetch-assignment)
                           (lambda (_id) '((id . 1001))))
                          ((symbol-function 'org-canvas--submissions-heading-for-assignment) #'ignore)
                          ((symbol-function 'org-canvas--submissions-fetch-history)
                           (lambda (&rest _) nil))
                          ((symbol-function 'switch-to-buffer) (lambda (b) b))
                          ((symbol-function 'message)
                           (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
                  (org-canvas-submissions-refresh))
                (cl-find-if (lambda (m) (string-prefix-p "Refreshed " m)) messages)))
      (it "names a listed attachment not on disk, and an older copy under its name"
        (with-org-canvas-test-config
          (with-uploads-file test-uploads-grading
            (set-file-times (expand-file-name "files/HW/Adams__Alice/a.pdf" dir)
                            (date-to-time "2026-10-01T12:00:00Z"))
            (let ((line (refresh
                         (list (test-uploads-submission 5001 "Adams, Alice" "a.pdf" 2
                                                        "2026-10-05T12:00:00Z")
                               (test-uploads-submission 5002 "Beta, Bob" "b.pdf" 1)
                               (test-uploads-submission 5003 "Cruz, Cal" "new.pdf" 1)))))
              (expect line :to-match
                      (regexp-quote "; 2 attachment(s) not on disk, D downloads them: Adams, Alice (a.pdf, an older copy is there); Cruz, Cal (new.pdf)"))
              (goto-char (point-min))
              (expect (buffer-string) :to-match "^- \\[\\[https://canvas.example.com/files/a.pdf/download\\]\\[a.pdf\\]\\]$")
              (expect (buffer-string) :to-match "^- \\[\\[file:files/HW/Beta__Bob/b.pdf\\]\\[b.pdf\\]\\]")))))
      (it "says nothing of a column nobody downloaded"
        (with-org-canvas-test-config
          (with-uploads-file test-uploads-grading
            (delete-directory (expand-file-name "files" dir) t)
            (expect (refresh (list (test-uploads-submission 5003 "Cruz, Cal" "new.pdf" 1)))
                    :not :to-match "not on disk"))))
      (it "keeps SAME_AS while the attempt is the one it was worked out from"
        (with-org-canvas-test-config
          (with-uploads-file test-uploads-grading
            (org-canvas-submissions-upload-text)
            (refresh (list (test-uploads-submission 5001 "Adams, Alice" "a.pdf" 1)
                           (test-uploads-submission 5003 "Cruz, Cal" "c.pdf" 2)
                           (test-uploads-submission 5004 "Diaz, Dee" "d.pdf" 1)))
            (expect (test-uploads-prop "Adams, Alice" "SAME_AS") :to-equal "Beta, Bob")
            (expect (test-uploads-prop "Cruz, Cal" "SAME_AS") :to-be nil)
            (expect (test-uploads-prop "Diaz, Dee" "SAME_AS") :to-equal "Cruz, Cal")))))))

(defun test-uploads-submission (uid name filename attempt &optional created-at)
  "Return a Canvas submission for NAME (UID) on ATTEMPT listing FILENAME.
CREATED-AT, when given, is when Canvas received the attachment."
  `((id . ,(* 10 uid)) (user_id . ,uid) (assignment_id . 1001)
    (workflow_state . "submitted") (submitted_at . "2026-10-05T12:00:00Z")
    (attempt . ,attempt) (late . :json-false) (missing . :json-false)
    (score . nil) (user . ((id . ,uid) (name . ,name) (sortable_name . ,name)))
    (submission_comments . [])
    (attachments . [((display_name . ,filename)
                     (url . ,(format "https://canvas.example.com/files/%s/download" filename))
                     ,@(and created-at `((created_at . ,created-at))))])))

;;; org-canvas-submissions-uploads-test.el ends here
