;;; org-canvas-body-fragment-test.el --- Tests for stranded body fragments  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Issue #391: before #175 a heading in a pulled body became an Org
;; headline, stranding the rest of the body as an ID-less heading that
;; a push would create.  These specs cover the detection, the
;; validator's warning and the push's refusal.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defconst test-fragment-class-course
  "* Microservices HW Questions
:PROPERTIES:
:CANVAS_ID: 2539983
:END:
#+begin_h1
*Read the following article: * [[https://example.com/a][Microservices 101]]
#+end_h1
The article.
* *Read the following article: * [[https://example.com/a][Microservices 101]]
:PROPERTIES:
:CLASS: article-title
:END:
The questions are open note.
"
  "An item and the CLASS-marked fragment an older pull left after it.")

(defconst test-fragment-block-course
  "* Lab 2
:PROPERTIES:
:CANVAS_ID: 77
:END:
Intro.
#+begin_h1
<<shot>> Screen shot   1:
#+end_h1
** Notes
Sub-heading text.
* Screen shot 1:
The rest of the body.
"
  "An item whose heading block the drawer-less fragment after it repeats.")

(defun test-fragment-reason-at (content heading)
  "Return the fragment reason for HEADING in CONTENT."
  (with-temp-org-buffer content
    (re-search-forward (concat "^\\*+ " (regexp-quote heading)))
    (org-canvas--body-fragment-reason)))

(defun test-fragment-ctx (push-fn)
  "Return a sync context whose push is PUSH-FN, for the fragment course."
  (list :parse-fn (lambda () (list :title (org-get-heading t t t t)
                                   :canvas-id (org-entry-get (point) "CANVAS_ID")
                                   :pom (point-marker)))
        :build-fn (lambda (_data) '((title . "x")))
        :push-fn push-fn
        :finalize-fn (lambda (_data _response &optional _ctx) nil)
        :feature-name "assignments" :feature-upper "ASSIGNMENTS"
        :total-count 2
        :counters (list :success 0 :skip 0 :fail 0 :conflict 0 :pulled 0)
        :synced-ids (list nil)))

(defun test-fragment-run-entries (push-fn)
  "Run the fragment course's two headings through the pipeline with PUSH-FN.
Return the counters."
  (with-temp-org-buffer test-fragment-class-course
    (let ((ctx (test-fragment-ctx push-fn))
          (markers (org-map-entries #'point-marker "LEVEL=1" 'file)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (dolist (m markers)
          (org-canvas--sync-process-entry m ctx)))
      (plist-get ctx :counters))))

;;;; Detection

(describe "org-canvas--body-fragment-reason"
  (it "names a pandoc CLASS drawer on an ID-less heading"
    (expect (test-fragment-reason-at test-fragment-class-course
                                     "*Read the following")
            :to-equal "carries the pandoc attribute CLASS"))

  (it "names a pandoc data attribute, whatever its case"
    (expect (test-fragment-reason-at
             "* Note\n:PROPERTIES:\n:data-role: aside\n:END:\n" "Note")
            :to-equal "carries the pandoc attribute DATA-ROLE"))

  (it "finds a heading block of the item above, past its sub-headings"
    (expect (test-fragment-reason-at test-fragment-block-course
                                     "Screen shot 1:")
            :to-equal "repeats a heading block of the item above"))

  (it "reads a star entity in the block as the star it stands for"
    (expect (test-fragment-reason-at
             "* A\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n#+begin_h2\n\\ast{} starry\n#+end_h2\n* * starry\n"
             "* starry")
            :to-equal "repeats a heading block of the item above"))

  (it "leaves a stamped heading alone, by CANVAS_ID or CANVAS_URL"
    (expect (test-fragment-reason-at
             "* P\n:PROPERTIES:\n:CANVAS_ID: 5\n:CLASS: x\n:END:\n" "P")
            :to-be nil)
    (expect (test-fragment-reason-at
             "* P\n:PROPERTIES:\n:CANVAS_URL: p\n:CLASS: x\n:END:\n" "P")
            :to-be nil))

  (it "leaves a new item alone: ordinary properties, CUSTOM_ID, another block"
    (expect (test-fragment-reason-at
             "* HW 3\n:PROPERTIES:\n:POINTS: 10\n:CUSTOM_ID: hw3\n:END:\n" "HW 3")
            :to-be nil)
    (expect (test-fragment-reason-at
             "* A\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n#+begin_h1\nOther\n#+end_h1\n* HW 4\n"
             "HW 4")
            :to-be nil)
    (expect (test-fragment-reason-at "* First\nText.\n" "First") :to-be nil))

  (it "never calls an empty headline a repeat"
    (expect (test-fragment-reason-at
             "* A\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n#+begin_h1\n\n#+end_h1\n* \n"
             "")
            :to-be nil)))

;;;; Validation

(describe "org-canvas--validate-body-fragments"
  (it "warns on each fragment, at its line, and changes nothing"
    (let* ((dir (make-temp-file "org-frag-" t))
           (file (expand-file-name "assignments.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert test-fragment-class-course test-fragment-block-course))
            (let* ((issues (org-canvas--validate-body-fragments file))
                   (first (car issues)))
              (expect (length issues) :to-equal 2)
              (expect (plist-get first :severity) :to-equal 'warning)
              (expect (plist-get first :line) :to-equal 9)
              (expect (plist-get first :heading)
                      :to-match "Read the following article")
              (expect (plist-get first :message)
                      :to-match "probably a body fragment from an older pull")
              (expect (plist-get first :push-only) :to-be nil)
              (expect (plist-get (cadr issues) :heading)
                      :to-equal "Screen shot 1:"))
            (expect (with-temp-buffer (insert-file-contents file)
                                      (buffer-string))
                    :to-equal (concat test-fragment-class-course
                                      test-fragment-block-course)))
        (delete-directory dir t))))

  (it "joins the validation run once per file"
    (let* ((dir (make-temp-file "org-frag-" t))
           (file (expand-file-name "assignments.org" dir)))
      (unwind-protect
          (with-nonexistent-canvas-files
            (with-temp-file file (insert test-fragment-class-course))
            (let* ((org-canvas-assignments-file file)
                   (issues (plist-get (org-canvas--validate-run-all-specs)
                                      :issues))
                   (fragments (cl-remove-if-not
                               (lambda (i)
                                 (string-match-p "body fragment"
                                                 (plist-get i :message)))
                               issues)))
              (expect (length fragments) :to-equal 1)))
        (delete-directory dir t)))))

;;;; The Push

(describe "org-canvas--sync-execute-pipeline and a body fragment"
  (it "skips the fragment under noninteractive and says so, pushing the item"
    (let* ((pushed nil)
           (counters (test-fragment-run-entries
                      (lambda (data _payload &optional _ctx)
                        (push (plist-get data :title) pushed)
                        '((id . 1))))))
      (expect pushed :to-equal '("Microservices HW Questions"))
      (expect (plist-get counters :skip) :to-equal 1)
      (expect (car (plist-get counters :skipped-titles))
              :to-match "probably a body fragment")))

  (it "asks interactively, even under org-canvas-assume-yes, and creates on yes"
    (let* ((asked nil)
           (pushed nil)
           (noninteractive nil)
           (org-canvas-assume-yes t))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (push prompt asked) t)))
        (test-fragment-run-entries
         (lambda (data _payload &optional _ctx)
           (push (plist-get data :title) pushed)
           '((id . 1)))))
      (expect (length asked) :to-equal 1)
      (expect (car asked) :to-match "Create it on Canvas anyway")
      (expect (length pushed) :to-equal 2)))

  (it "creates nothing when the answer is no"
    (let* ((pushed nil)
           (noninteractive nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
        (let ((counters (test-fragment-run-entries
                         (lambda (data _payload &optional _ctx)
                           (push (plist-get data :title) pushed)
                           '((id . 1))))))
          (expect (plist-get counters :skip) :to-equal 1)))
      (expect pushed :to-equal '("Microservices HW Questions"))))

  (it "never asks in a dry run, and counts the fragment as skipped"
    (let* ((noninteractive nil)
           (org-canvas--dry-run t)
           (asked nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq asked t) t)))
        (let ((counters (test-fragment-run-entries
                         (lambda (&rest _) (error "No push in a dry run")))))
          (expect asked :to-be nil)
          (expect (plist-get counters :skip) :to-equal 1)
          (expect (plist-get counters :dry-run) :to-equal 1)))))

  (it "sends no POST for the fragment from a real sync command"
    (let* ((dir (make-temp-file "org-frag-" t))
           (file (expand-file-name "announcements.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Welcome\n:PROPERTIES:\n:ALLOW_COMMENTS: true\n:END:\nHello.\n"
                      "* Tail of a body\n:PROPERTIES:\n:CLASS: lead\n:END:\nMore.\n"))
            (with-org-canvas-test-config
              (let ((org-canvas-announcements-file file))
                (with-sync-test-env
                  (with-mock-api
                    (org-canvas-sync-announcements)
                    (let ((posts (cl-remove-if-not
                                  (lambda (c) (eq (car c) 'POST))
                                  test-org-canvas-api-calls)))
                      (expect (length posts) :to-equal 1)
                      (expect (alist-get 'title (nth 2 (car posts)))
                              :to-equal "Welcome")))))))
        (delete-directory dir t)))))

(describe "org-canvas--push-at-point-runtime and a body fragment"
  (defun test-fragment-at-point (push-fn)
    "Push the fragment heading at point through PUSH-FN; return the context."
    (with-temp-org-buffer test-fragment-class-course
      (re-search-forward "^\\* \\*Read")
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (org-canvas--push-at-point-runtime
         (list :feature "assignment"
               :parse (lambda () (list :title "Fragment" :pom (point)))
               :build (lambda (_data) '((name . "Fragment")))
               :push push-fn
               :finalize (lambda (_data _response &optional _ctx) nil))))))

  (it "refuses the create under noninteractive and reports the outcome"
    (let* ((pushed nil)
           (said nil)
           (ctx (cl-letf (((symbol-function 'message)
                           (lambda (fmt &rest args)
                             (push (apply #'format fmt args) said))))
                  (test-fragment-at-point
                   (lambda (&rest _) (setq pushed t) '((id . 1)))))))
      (expect pushed :to-be nil)
      (expect (plist-get ctx :outcome) :to-equal 'fragment)
      (expect (car said) :to-match "not created")))

  (it "creates it when the interactive answer is yes"
    (let* ((pushed nil)
           (noninteractive nil)
           (ctx (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t))
                          ((symbol-function 'message) #'ignore))
                  (test-fragment-at-point
                   (lambda (&rest _) (setq pushed t) '((id . 1)))))))
      (expect pushed :to-be t)
      (expect (plist-get ctx :outcome) :to-equal 'synced))))

(describe "org-canvas--sync-headings-report and a refused fragment"
  (it "counts the refusal among the stopped headings"
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) said))))
        (org-canvas--sync-headings-report
         (list (list :feature "assignments" :target "Tail"
                     :outcome 'fragment))))
      (expect (car said) :to-match "1 stopped"))))

(provide 'org-canvas-body-fragment-test)
;;; org-canvas-body-fragment-test.el ends here
