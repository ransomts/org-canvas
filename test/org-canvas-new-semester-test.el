;;; org-canvas-new-semester-test.el --- Specs for the semester rollover  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-new-semester' (issue #547) carries a copied course
;; directory into a new semester: it retargets the credentials file at
;; the new course id and strips every sync stamp the registries name.
;; These specs pin the stamp set to the registries rather than to a
;; hand-enumerated list, check that content survives untouched, that
;; the dry run writes nothing, and that the credentials retarget keeps
;; what the user wrote beside the lines it rewrites.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'test-helper)
(require 'org-canvas-new-semester)

;;;; Fixtures

(defconst test-new-semester--assignments-before
  "#+TITLE: Assignments
#+LAST_SYNCED: [2026-05-15 Fri 10:00]

* Homework 1
:PROPERTIES:
:POINTS: 50
:CANVAS_ID: 101
:CANVAS_UPDATED_AT: 2026-05-01T10:00:00Z
:PAYLOAD_HASH: abc123
:LAST_SYNCED: [2026-05-01 Fri 10:00]
:END:
Do the reading.

** Homework 1 quiz item
:PROPERTIES:
:TYPE: essay
:CANVAS_ITEM_ID: 55
:END:
Answer here.

* Homework 2
:PROPERTIES:
:POINTS: 100
:CANVAS_ASSIGNMENT_ID: 201
:CANVAS_DELETED: 2026-05-02
:END:
Second homework.
"
  "A copied assignments.org: stamps the sed knew, the deleted mark it
did not (#392), and content properties that must survive.")

(defconst test-new-semester--assignments-after
  "#+TITLE: Assignments

* Homework 1
:PROPERTIES:
:POINTS: 50
:END:
Do the reading.

** Homework 1 quiz item
:PROPERTIES:
:TYPE: essay
:END:
Answer here.

* Homework 2
:PROPERTIES:
:POINTS: 100
:END:
Second homework.
"
  "assignments.org after the rollover: every stamp gone, content kept.")

(defconst test-new-semester--pages-before
  "#+TITLE: Pages
#+LAST_SYNCED: [2026-05-15 Fri 10:00]

* Syllabus
:PROPERTIES:
:CANVAS_URL: syllabus
:END:
The syllabus page.
"
  "A copied pages.org whose only property is a stamp, so the whole
drawer goes.")

(defconst test-new-semester--pages-after
  "#+TITLE: Pages

* Syllabus
The syllabus page.
"
  "pages.org after the rollover: an emptied drawer disappears.")

(defconst test-new-semester--replies-before
  "#+TITLE: Discussion Replies
#+LAST_SYNCED: [2026-05-15 Fri 10:00]

* Welcome thread
:PROPERTIES:
:CANVAS_ID: 300
:END:
Welcome!

** A student's reply
:PROPERTIES:
:CANVAS_ENTRY_ID: 99001
:AUTHOR: A Student
:END:
Thanks!
"
  "A copied discussion-replies.org.  CANVAS_ENTRY_ID is a pull-only
reader's record of history, never a push pointer, so it is content and
stays.")

(defconst test-new-semester--replies-after
  "#+TITLE: Discussion Replies

* Welcome thread
Welcome!

** A student's reply
:PROPERTIES:
:CANVAS_ENTRY_ID: 99001
:AUTHOR: A Student
:END:
Thanks!
"
  "discussion-replies.org after the rollover: the push-side stamp gone,
the pull-only record kept.")

(defun test-new-semester--file-text (directory name)
  "Return the text of DIRECTORY's file NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name directory))
    (buffer-string)))

(defun test-new-semester--checksums (directory)
  "Return an alist of (RELATIVE-NAME . HASH) for every file under DIRECTORY."
  (mapcar (lambda (file)
            (cons (file-relative-name file directory)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-hash))))
          (sort (directory-files-recursively directory ".*" nil)
                #'string<)))

(defmacro test-new-semester--with-course (dir-var &rest body)
  "Create a copied course in a temp directory, bind DIR-VAR, run BODY.
The course holds the three fixture files plus an
org-canvas-credentials.el written for the semester the directory was
copied from: course 77777 in the old directory, the way a copy leaves
it.  Buffers visiting the tree are killed and the tree deleted
afterwards."
  (declare (indent 1))
  `(let* ((tmp (make-temp-file "new-semester-" t))
          (old-dir (expand-file-name "spring-2026" tmp))
          (,dir-var (expand-file-name "fall-2027" tmp)))
     (unwind-protect
         (progn
           (make-directory ,dir-var)
           (with-temp-file (expand-file-name "assignments.org" ,dir-var)
             (insert test-new-semester--assignments-before))
           (with-temp-file (expand-file-name "pages.org" ,dir-var)
             (insert test-new-semester--pages-before))
           (with-temp-file (expand-file-name "discussion-replies.org" ,dir-var)
             (insert test-new-semester--replies-before))
           (make-directory old-dir)
           (org-canvas--write-credentials-file
            old-dir "https://canvas.example.com" "token-1" "77777" nil)
           (copy-file (expand-file-name "org-canvas-credentials.el" old-dir)
                      (expand-file-name "org-canvas-credentials.el" ,dir-var))
           ,@body)
       (dolist (buf (buffer-list))
         (let ((bf (buffer-file-name buf)))
           (when (and bf (string-prefix-p tmp bf))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory tmp t))))

(defun test-new-semester--run (directory course-id)
  "Run `org-canvas-new-semester' on DIRECTORY with COURSE-ID, return its report.
The prompts answer themselves; under `noninteractive' the confirmation
is assumed yes, which is what applies the rollover."
  (cl-letf (((symbol-function 'read-directory-name)
             (lambda (&rest _) directory))
            ((symbol-function 'read-string)
             (lambda (&rest _) course-id))
            ((symbol-function 'message) #'ignore))
    (org-canvas-new-semester)))

;;;; The Stamp Set

(describe "org-canvas--semester-strip-property-names"
  (it "covers every stamp the sed named plus the deleted mark"
    (dolist (prop '("CANVAS_ID" "CANVAS_URL" "CANVAS_UPDATED_AT" "LAST_SYNCED"
                    "PAYLOAD_HASH" "CANVAS_ASSIGNMENT_ID" "CANVAS_ITEM_ID"
                    "CANVAS_CRITERION_ID" "CANVAS_DELETED"))
      (expect (member prop (org-canvas--semester-strip-property-names))
              :to-be-truthy)))

  (it "covers a stamp registered after the sed was written"
    ;; The point of the registry: a stamp a later module registers joins
    ;; the strip set the moment it loads, instead of surviving the way
    ;; CANVAS_DELETED survived the sed (issue #547).
    (let ((org-canvas--id-property-registry
           (cons "CANVAS_FUTURE_ID" org-canvas--id-property-registry)))
      (expect (member "CANVAS_FUTURE_ID"
                      (org-canvas--semester-strip-property-names))
              :to-be-truthy))))

(describe "org-canvas--semester-org-files"
  (it "takes the .org files of the directory, no dotfiles, no recursion"
    (let* ((tmp (make-temp-file "semester-files-" t))
           (sub (expand-file-name "week1" tmp)))
      (unwind-protect
          (progn
            (make-directory sub)
            (with-temp-file (expand-file-name "assignments.org" tmp)
              (insert "* A\n"))
            (with-temp-file (expand-file-name "nested.org" sub)
              (insert "* B\n"))
            (with-temp-file (expand-file-name ".#assignments.org" tmp)
              (insert "* C\n"))
            (with-temp-file (expand-file-name "notes.txt" tmp)
              (insert "not org\n"))
            (expect (org-canvas--semester-org-files tmp)
                    :to-equal (list (expand-file-name "assignments.org" tmp))))
        (delete-directory tmp t)))))

;;;; The Command

(describe "org-canvas-new-semester"
  (it "refuses an empty course id"
    (let ((tmp (make-temp-file "semester-empty-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _) tmp))
                    ((symbol-function 'read-string) (lambda (&rest _) "")))
            (expect (org-canvas-new-semester) :to-throw 'user-error))
        (delete-directory tmp t))))

  (it "dry run names every heading and the new course ID, writing nothing"
    (test-new-semester--with-course dir
      (let ((before (test-new-semester--checksums dir))
            (report nil))
        (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil)))
          (setq report (test-new-semester--run dir "88888")))
        (expect report :to-match "88888")
        (dolist (title '("Homework 1" "Homework 1 quiz item" "Homework 2"
                         "Syllabus" "Welcome thread"))
          (expect report :to-match (regexp-quote title)))
        (expect report :to-match "org-canvas-credentials\\.el")
        (expect (test-new-semester--checksums dir) :to-equal before))))

  (it "strips exactly the registry-known stamps, leaving content"
    (test-new-semester--with-course dir
      (with-org-canvas-course-globals
        (test-new-semester--run dir "88888")
        ;; The session now points at the new course (issue #547: the
        ;; next sync must create, not update the old course's objects).
        (expect org-canvas-course-id :to-equal "88888")
        (expect org-canvas-directory
                :to-equal (file-name-as-directory dir)))
      (expect (test-new-semester--file-text dir "assignments.org")
              :to-equal test-new-semester--assignments-after)
      (expect (test-new-semester--file-text dir "pages.org")
              :to-equal test-new-semester--pages-after)
      (expect (test-new-semester--file-text dir "discussion-replies.org")
              :to-equal test-new-semester--replies-after)))

  (it "retargets the credentials file, keeping token and hand edits"
    (test-new-semester--with-course dir
      (with-org-canvas-course-globals
        (test-new-semester--run dir "88888"))
      (let ((text (test-new-semester--file-text
                   dir "org-canvas-credentials.el")))
        (expect text :to-match
                (regexp-quote "(setq org-canvas-course-id \"88888\")"))
        (expect text :to-match
                (regexp-quote
                 (format "(setq org-canvas-directory %S)"
                         (file-name-as-directory dir))))
        (expect text :to-match
                (regexp-quote "(setq org-canvas-api-token \"token-1\")"))
        (expect text :to-match
                (regexp-quote
                 "(setq org-canvas-base-url \"https://canvas.example.com\")")))))

  (it "appends the setq lines a hand-minimized credentials file lacks"
    (test-new-semester--with-course dir
      ;; No directory line, no course-id line, and no final newline:
      ;; both appends run, the second on a buffer that now ends in one.
      (with-temp-file (expand-file-name "org-canvas-credentials.el" dir)
        (insert "(setq org-canvas-api-token \"token-1\")"))
      (with-org-canvas-course-globals
        (test-new-semester--run dir "88888"))
      (let ((text (test-new-semester--file-text
                   dir "org-canvas-credentials.el")))
        (expect text :to-match
                (regexp-quote
                 (format "(setq org-canvas-directory %S)"
                         (file-name-as-directory dir))))
        (expect text :to-match
                (regexp-quote "(setq org-canvas-course-id \"88888\")"))
        (expect text :to-match
                (regexp-quote "(setq org-canvas-api-token \"token-1\")")))))

  (it "keeps a comment beside the setq line it rewrites"
    (test-new-semester--with-course dir
      (with-temp-file (expand-file-name "org-canvas-credentials.el" dir)
        (insert ";;; org-canvas-credentials.el --- hand kept  -*- lexical-binding: t; -*-\n")
        (insert "(setq org-canvas-api-token \"token-1\")\n")
        (insert "(setq org-canvas-course-id \"77777\") ;; the semester before\n"))
      (with-org-canvas-course-globals
        (test-new-semester--run dir "88888"))
      (expect (test-new-semester--file-text
               dir "org-canvas-credentials.el")
              :to-match
              (regexp-quote
               "(setq org-canvas-course-id \"88888\") ;; the semester before"))))

  (it "retargets only the session when the directory has no credentials file"
    (test-new-semester--with-course dir
      (delete-file (expand-file-name "org-canvas-credentials.el" dir))
      (with-org-canvas-course-globals
        (test-new-semester--run dir "88888")
        (expect org-canvas-course-id :to-equal "88888"))
      (expect (file-exists-p
               (expand-file-name "org-canvas-credentials.el" dir))
              :to-be nil)))

  (it "sections a stamped heading in a file with no #+LAST_SYNCED header"
    (let* ((tmp (make-temp-file "semester-noheader-" t))
           (dir (expand-file-name "course" tmp)))
      (unwind-protect
          (progn
            (make-directory dir)
            (with-temp-file (expand-file-name "midterm.org" dir)
              (insert "#+TITLE: Midterm\n\n* Midterm item\n:PROPERTIES:\n:TYPE: essay\n:CANVAS_ITEM_ID: 55\n:END:\n"))
            (let ((report nil))
              (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil)))
                (setq report (test-new-semester--run dir "88888")))
              (expect report :to-match (regexp-quote "Midterm item: CANVAS_ITEM_ID"))
              (expect report :to-match "drop 0 #\\+LAST_SYNCED header")))
        (delete-directory tmp t))))

  (it "reports nothing to strip in a course with no sync state anywhere"
    (let* ((tmp (make-temp-file "semester-clean-" t))
           (dir (expand-file-name "course" tmp)))
      (unwind-protect
          (progn
            (make-directory dir)
            (with-temp-file (expand-file-name "fresh.org" dir)
              (insert "#+TITLE: Fresh\n\n* New item\n:PROPERTIES:\n:POINTS: 10\n:END:\n"))
            (let ((report nil))
              (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil)))
                ;; An empty `org-canvas-directory' takes the prompt's
                ;; default from `default-directory' instead.
                (let ((org-canvas-directory ""))
                  (setq report (test-new-semester--run dir "88888"))))
              (expect report :to-match "Nothing to strip")
              (expect report :to-match "only the credentials retarget")))
        (delete-directory tmp t)))))

(provide 'org-canvas-new-semester-test)
;;; org-canvas-new-semester-test.el ends here
