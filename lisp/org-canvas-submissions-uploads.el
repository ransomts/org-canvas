;;; org-canvas-submissions-uploads.el --- Read a column's uploads and pair them -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A grading file links each student's uploads, and once D has
;; downloaded them they sit in files/<assignment>/<student>/.  Reading
;; them still meant opening a PDF per heading, and a pair assignment
;; (one Google Doc, each partner uploading the same PDF) meant reading
;; every document twice, or grouping them first with a script that ran
;; pdftotext over the folder and hashed the text (issue #457).
;;
;; `org-canvas-submissions-upload-text' is that script as a command on
;; a grading file.  For every student with attachments it reads the
;; downloaded copies:
;;
;;   - a PDF through pdftotext (`org-canvas-submissions-pdftotext-program'),
;;     a .docx, .odt, .rtf or .epub through pandoc, a text file as it is;
;;     when the program is missing the block says so and the rest goes on;
;;   - an attachment not downloaded is named as such, never fetched.
;;
;; Students are then grouped by shared upload: the same Google Doc id
;; (found in the text, or in the PDF's own link annotations), or the
;; same file, byte for byte.  The doc id comes first because two
;; exports of one document differ by a header line, which defeats a
;; hash of the text; a doc id that more than
;; `org-canvas-submissions-same-as-max-group' students carry is a link
;; everyone copied (the template, the instructions) and is ignored.
;; Each grouped heading gets SAME_AS, the other members' names, and an
;; ungrouped one loses it; a refresh keeps SAME_AS while the attempt is
;; the one it was worked out from.
;;
;; The report shows one block per student, the text in an example
;; block, and the second member of a group points to the first instead
;; of repeating the text, so a pair is read once.  The groups are
;; listed at the top.  Nothing is read from Canvas and nothing is sent;
;; the only write is SAME_AS in the grading file (and, with SAVE, the
;; report beside the downloads).  Under `noninteractive' the report is
;; printed to standard output.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules: it reads the grading
;; files and downloads the submissions module writes.
(require 'org-canvas-submissions)

(defcustom org-canvas-submissions-pdftotext-program "pdftotext"
  "Program to turn a downloaded PDF upload into text.
Poppler's pdftotext, run as PROGRAM -layout FILE -.  When it cannot be
found, `org-canvas-submissions-upload-text' says so in the block of
each PDF and goes on with the other uploads."
  :type 'string
  :group 'org-canvas)

(defcustom org-canvas-submissions-same-as-max-group 4
  "Most students one Google Doc id may join into a group.
A doc id found in more students' uploads than this is a link everyone
copied, such as the assignment's template, not a shared write-up, and
`org-canvas-submissions-upload-text' ignores it for grouping."
  :type 'integer
  :group 'org-canvas)

(define-key org-canvas-submissions-mode-map (kbd "u") #'org-canvas-submissions-upload-text)

(defconst org-canvas--submissions-uploads-buffer "*canvas-uploads*"
  "Buffer the upload text report is rendered into.")

(defconst org-canvas--submissions-doc-id-regexp
  (concat "docs\\.google\\.com/"
          "\\(?:document\\|spreadsheets\\|presentation\\)/d/\\(?:e/\\)?"
          "\\([-_A-Za-z0-9]\\{20,\\}\\)")
  "A Google Docs link; group 1 is the document id.")

(defconst org-canvas--submissions-pandoc-extensions '("docx" "odt" "rtf" "epub")
  "Extensions of uploads read through pandoc.")

;;;; The Text of One Upload

(defun org-canvas--submissions-upload-run (program &rest args)
  "Run PROGRAM with ARGS and return its standard output, or nil on failure.
Standard error is discarded."
  (with-temp-buffer
    (and (eql (apply #'call-process program nil '(t nil) nil args) 0)
         (buffer-string))))

(defun org-canvas--submissions-upload-convert (program args absent)
  "Return the text PROGRAM prints for ARGS, as a plist.
The value is (:text TEXT), or (:note WHY): ABSENT when PROGRAM cannot
be found, or a line saying it failed."
  (if (not (executable-find program))
      (list :note absent)
    (let ((out (apply #'org-canvas--submissions-upload-run program args)))
      (if out
          (list :text out)
        (list :note (format "%s could not read this file" program))))))

(defun org-canvas--submissions-upload-raw (path)
  "Return the bytes of the file at PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun org-canvas--submissions-upload-read (path raw)
  "Return the text of the upload at PATH, whose bytes are RAW, as a plist.
\(:text TEXT) when it could be read, else (:note WHY) saying why not."
  (let ((ext (downcase (or (file-name-extension path) "")))
        (pdftotext org-canvas-submissions-pdftotext-program))
    (cond ((equal ext "pdf")
           (org-canvas--submissions-upload-convert
            pdftotext (list "-layout" path "-")
            (format "%s not found: install poppler-utils to read PDFs" pdftotext)))
          ((member ext org-canvas--submissions-pandoc-extensions)
           (org-canvas--submissions-upload-convert
            "pandoc" (list "-t" "plain" path)
            (format "pandoc not found: open the .%s to read it" ext)))
          ((string-match-p "\0" raw)
           (list :note (format "not a text file (.%s): open it to read it" ext)))
          (t (list :text (decode-coding-string raw 'utf-8))))))

(defun org-canvas--submissions-doc-ids (&rest texts)
  "Return the Google Doc ids named in TEXTS, first seen first."
  (let ((ids nil))
    (dolist (text texts)
      (let ((start 0))
        (while (and text
                    (string-match org-canvas--submissions-doc-id-regexp text start))
          (cl-pushnew (match-string 1 text) ids :test #'equal)
          (setq start (match-end 0)))))
    (nreverse ids)))

(defun org-canvas--submissions-upload (name path)
  "Return the upload NAME, downloaded at PATH, as a plist.
The keys are :name, :hash (SHA-256 of the file), :doc-ids (found in
the text and in the raw file, where a PDF keeps its link targets),
and :text or :note from `org-canvas--submissions-upload-read'."
  (let* ((raw (org-canvas--submissions-upload-raw path))
         (read (org-canvas--submissions-upload-read path raw)))
    (append (list :name name
                  :hash (secure-hash 'sha256 raw)
                  :doc-ids (org-canvas--submissions-doc-ids (plist-get read :text) raw))
            read)))

;;;; A Grading File's Uploads

(defun org-canvas--submissions-uploads-at-point ()
  "Return the student at point and their uploads, as a plist.
The keys are :user-id, :name, :uploads (downloaded ones, each an
`org-canvas--submissions-upload' plist) and :missing (the names of
attachments with no copy on disk)."
  (let ((uploads nil)
        (missing nil))
    (dolist (entry (org-canvas--submissions-attachment-entries))
      (let* ((local (plist-get entry :local))
             (path (and local (expand-file-name local (org-canvas--submissions-dir)))))
        (if (and path (file-exists-p path))
            (push (org-canvas--submissions-upload (plist-get entry :name) path) uploads)
          (push (plist-get entry :name) missing))))
    (list :user-id (org-entry-get (point) "USER_ID")
          :name (org-get-heading t t t t)
          :uploads (nreverse uploads)
          :missing (nreverse missing))))

(defun org-canvas--submissions-uploads-collect ()
  "Return `org-canvas--submissions-uploads-at-point' for each student.
Only the students who have attachments, in file order; a student who
left the course is skipped."
  (let ((students nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (when (and (org-entry-get (point) "USER_ID")
                   (not (org-canvas--submissions-left-p)))
          (let ((student (org-canvas--submissions-uploads-at-point)))
            (when (or (plist-get student :uploads) (plist-get student :missing))
              (push student students))))
        (forward-line 1)))
    (nreverse students)))

;;;; Grouping by Shared Upload

(defun org-canvas--submissions-student-keys (student)
  "Return STUDENT's grouping keys: (doc . ID) and (file . HASH) conses."
  (let ((keys nil))
    (dolist (upload (plist-get student :uploads))
      (dolist (id (plist-get upload :doc-ids))
        (cl-pushnew (cons 'doc id) keys :test #'equal))
      (cl-pushnew (cons 'file (plist-get upload :hash)) keys :test #'equal))
    (nreverse keys)))

(defun org-canvas--submissions-upload-keys (students)
  "Return the keys STUDENTS share, as (KEY . INDICES) in first-seen order.
KEY is from `org-canvas--submissions-student-keys'; INDICES are the
positions in STUDENTS of the two or more holding it.  A doc id held by
more than `org-canvas-submissions-same-as-max-group' is dropped."
  (let ((table nil)
        (index 0))
    (dolist (student students)
      (dolist (key (org-canvas--submissions-student-keys student))
        (let ((cell (assoc key table)))
          (if cell
              (setcdr cell (append (cdr cell) (list index)))
            (push (list key index) table))))
      (cl-incf index))
    (cl-remove-if (lambda (cell)
                    (let ((count (length (cdr cell))))
                      (or (< count 2)
                          (and (eq (caar cell) 'doc)
                               (> count org-canvas-submissions-same-as-max-group)))))
                  (nreverse table))))

(defun org-canvas--submissions-upload-join (labels a b)
  "Give every index labelled like A in the vector LABELS the label of B."
  (let ((from (aref labels a))
        (to (aref labels b)))
    (dotimes (i (length labels))
      (when (eql (aref labels i) from)
        (aset labels i to)))))

(defun org-canvas--submissions-upload-groups (students)
  "Return STUDENTS grouped by shared upload, in file order.
Each group is (:members STUDENTS :keys KEYS): the members joined by a
key they share, directly or through another member, and the keys that
joined them.  Students sharing nothing are in no group."
  (let* ((keys (org-canvas--submissions-upload-keys students))
         (labels (vconcat (number-sequence 0 (1- (length students)))))
         (by-label nil)
         (groups nil))
    (dolist (cell keys)
      (dolist (index (cddr cell))
        (org-canvas--submissions-upload-join labels index (cadr cell))))
    (dolist (cell keys)
      (push (car cell) (alist-get (aref labels (cadr cell)) by-label)))
    (dotimes (i (length students))
      (let ((label (aref labels i)))
        (when-let* ((cell (assq label by-label)))
          (setq by-label (delq cell by-label))
          (push (list :members (cl-loop for j below (length students)
                                        when (eql (aref labels j) label)
                                        collect (nth j students))
                      :keys (reverse (cdr cell)))
                groups))))
    (nreverse groups)))

(defun org-canvas--submissions-upload-group-of (student groups)
  "Return the group of GROUPS that STUDENT is a member of, or nil."
  (cl-find-if (lambda (group) (memq student (plist-get group :members))) groups))

(defun org-canvas--submissions-upload-others (student group)
  "Return the names of GROUP's members other than STUDENT, joined by \"; \"."
  (mapconcat (lambda (member) (plist-get member :name))
             (remq student (plist-get group :members)) "; "))

(defun org-canvas--submissions-upload-reason (group)
  "Return what joined GROUP: its Google Doc ids, else the same file."
  (let ((ids (cl-loop for key in (plist-get group :keys)
                      when (eq (car key) 'doc) collect (cdr key))))
    (if ids
        (format "Google Doc %s" (string-join ids ", "))
      "the same file")))

(defun org-canvas--submissions-mark-same-as (students groups)
  "Write SAME_AS on the headings of STUDENTS from GROUPS.
A grouped student gets the other members' names, an ungrouped one
loses the property.  Only a student with a downloaded upload is
touched: one whose files are not on disk was not compared."
  (save-excursion
    (dolist (student students)
      (when (and (plist-get student :uploads)
                 (org-canvas--submissions-goto-user (plist-get student :user-id)))
        (if-let* ((group (org-canvas--submissions-upload-group-of student groups)))
            (org-entry-put (point) "SAME_AS"
                           (org-canvas--submissions-upload-others student group))
          (org-entry-delete (point) "SAME_AS"))))))

;;;; The Report

(defun org-canvas--submissions-uploads-counts (students groups)
  "Return the report's count line for STUDENTS and GROUPS."
  (let* ((read (cl-count-if (lambda (s) (plist-get s :uploads)) students))
         (shared (apply #'+ (mapcar (lambda (g) (length (plist-get g :members))) groups))))
    (format "%d student(s) with downloaded uploads, %d distinct document(s); %d group(s) share one\n"
            read (+ (- read shared) (length groups)) (length groups))))

(defun org-canvas--submissions-uploads-insert-header (name students groups)
  "Insert the report's title, counts and groups for column NAME.
STUDENTS and GROUPS are what the report is built from."
  (insert (format "#+TITLE: Uploads: %s\n\n" name)
          (org-canvas--submissions-uploads-counts students groups))
  (dolist (group groups)
    (insert (format "- Same upload: %s (%s)\n"
                    (mapconcat (lambda (m) (plist-get m :name)) (plist-get group :members) "; ")
                    (org-canvas--submissions-upload-reason group))))
  (when-let* ((missing (cl-remove-if-not (lambda (s) (plist-get s :missing)) students)))
    (insert (format "- Not on disk, so not read (D in the grading file downloads them): %s\n"
                    (mapconcat (lambda (s)
                                 (format "%s (%s)" (plist-get s :name)
                                         (string-join (plist-get s :missing) ", ")))
                               missing "; ")))))

(defun org-canvas--submissions-uploads-insert-upload (upload)
  "Insert UPLOAD's sub-heading and its text, or the note saying why not."
  (insert (format "** %s\n" (plist-get upload :name)))
  (if-let* ((text (plist-get upload :text)))
      (insert "#+begin_example\n"
              (org-escape-code-in-string (string-trim-right text))
              "\n#+end_example\n")
    (insert (plist-get upload :note) "\n")))

(defun org-canvas--submissions-uploads-insert-student (student groups)
  "Insert STUDENT's block of the report.
A member of one of GROUPS who is not its first points to the first
rather than repeating the text."
  (let* ((group (org-canvas--submissions-upload-group-of student groups))
         (first (car (plist-get group :members))))
    (insert (format "\n* %s\n" (plist-get student :name)))
    (when group
      (insert (format "Same upload as %s.\n"
                      (org-canvas--submissions-upload-others student group))))
    (if (and group (not (eq student first)))
        (insert (format "Read under %s above.\n" (plist-get first :name)))
      (mapc #'org-canvas--submissions-uploads-insert-upload (plist-get student :uploads)))
    (dolist (name (plist-get student :missing))
      (insert (format "** %s\nNot downloaded.\n" name)))))

(defun org-canvas--submissions-uploads-render (name students groups)
  "Insert the upload report for column NAME, from STUDENTS and GROUPS."
  (org-canvas--submissions-uploads-insert-header name students groups)
  (dolist (student students)
    (org-canvas--submissions-uploads-insert-student student groups)))

(defun org-canvas--submissions-uploads-save (text)
  "Write TEXT beside this column's downloads; return the file written."
  (let ((file (expand-file-name
               (format "files/%s/uploads.org"
                       (org-canvas--submissions-sanitize-filename
                        org-canvas-submissions--assignment-name))
               (org-canvas--submissions-dir))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))
    file))

;;;###autoload
(defun org-canvas-submissions-upload-text (&optional save)
  "Show the text of every downloaded upload in this grading file.
One block per student; a PDF is read through pdftotext, a .docx and
its kin through pandoc, a text file as it is, and an attachment not
downloaded is named (D downloads them).  Students who uploaded the
same document — the same Google Doc id, or the same file — are
grouped: each gets SAME_AS on its heading, naming the others, and the
report shows the text once (issue #457).  With SAVE (the prefix
argument) the report is also written as uploads.org beside the
column's downloads.  Return the report text."
  (interactive "P")
  (unless org-canvas-submissions-mode
    (user-error "Not in a submissions buffer"))
  (org-canvas--submissions-ensure-context)
  (unless (eq org-canvas-submissions--current-view 'detail)
    (user-error "Switch to detail view first (press v)"))
  (let* ((name org-canvas-submissions--assignment-name)
         (students (org-canvas--submissions-uploads-collect))
         (groups (org-canvas--submissions-upload-groups students)))
    (unless students
      (user-error "No attachments in %s" name))
    (let ((inhibit-read-only t))
      (org-canvas--submissions-mark-same-as students groups))
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (let ((text (org-canvas--report-display
                 org-canvas--submissions-uploads-buffer
                 (lambda () (org-canvas--submissions-uploads-render name students groups))
                 #'org-mode)))
      (when save
        (message "Wrote %s" (org-canvas--submissions-uploads-save text)))
      text)))

(provide 'org-canvas-submissions-uploads)
;;; org-canvas-submissions-uploads.el ends here
