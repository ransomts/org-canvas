;;; org-canvas-grading-periods-test.el --- Buttercup tests for grading periods -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-grading-periods': the read-only pull and the
;; validator that reads what it wrote.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-grading-periods)
(require 'org-canvas-validate)
(require 'org-canvas)

(defconst test-gp--fall
  '((id . 501) (title . "Fall 2026 Q1")
    (start_date . "2026-08-19T00:00:00Z")
    (end_date . "2026-10-09T23:59:59Z")
    (close_date . "2026-10-16T23:59:59Z")
    (weight . 50) (is_closed . :json-false))
  "A period with every field set.")

(defconst test-gp--winter
  '((id . 502) (title . "Fall 2026 Q2")
    (start_date . "2026-10-10T00:00:00Z")
    (end_date . "2026-12-04T23:59:59Z")
    (close_date . nil)
    (weight . nil) (is_closed . t))
  "A period with no weight and no close date, already closed.")

(defmacro test-gp--with-file (&rest body)
  "Run BODY with `org-canvas-grading-periods-file' bound to a fresh temp path.
FILE is bound to that path; the buffer visiting it is killed afterwards."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "gp-" t))
          (file (expand-file-name "grading-periods.org" dir))
          (org-canvas-grading-periods-file file))
     (unwind-protect
         (with-org-canvas-test-config
           (with-sync-test-env
             ,@body))
       (let ((buf (find-buffer-visiting file)))
         (when buf
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun test-gp--mock-canvas (periods)
  "Return a stand-in for `org-canvas-api-request' answering PERIODS.
PERIODS is a list; the reply wraps it under `grading_periods' as a
vector, the way `json-read' delivers it."
  (lambda (_method _url &rest _args)
    (list (cons 'grading_periods (vconcat periods)))))

(defun test-gp--heading-props (file title)
  "Return the property alist of the heading TITLE in FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s$" (regexp-quote title)))
    (org-back-to-heading t)
    (org-entry-properties)))

(describe "org-canvas--grading-periods-fetch"
  (it "unwraps the grading_periods list and returns a list, not a vector"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-gp--mock-canvas (list test-gp--fall))))
        (let ((periods (org-canvas--grading-periods-fetch)))
          (expect (listp periods) :to-be t)
          (expect (alist-get 'id (car periods)) :to-equal 501)))))

  (it "asks the course's grading_periods endpoint"
    (with-org-canvas-test-config
      (let ((asked nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _) (setq asked (list method url)) nil)))
          (org-canvas--grading-periods-fetch))
        (expect (car asked) :to-be 'GET)
        (expect (cadr asked) :to-match "/courses/[0-9]+/grading_periods\\'")))))

(describe "org-canvas-pull-grading-periods"
  (it "writes one heading per period with every property"
    (test-gp--with-file
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-gp--mock-canvas (list test-gp--winter test-gp--fall))))
        (org-canvas-pull-grading-periods))
      (let ((fall (test-gp--heading-props file "Fall 2026 Q1")))
        (expect (alist-get "CANVAS_ID" fall nil nil #'equal) :to-equal "501")
        (expect (alist-get "START_DATE" fall nil nil #'equal) :to-match "2026-08-1[89]")
        (expect (alist-get "END_DATE" fall nil nil #'equal) :to-match "2026-10-\\(09\\|10\\)")
        (expect (alist-get "CLOSE_DATE" fall nil nil #'equal) :to-match "2026-10-1[67]")
        (expect (alist-get "WEIGHT" fall nil nil #'equal) :to-equal "50")
        ;; A boolean at its default is omitted, as every pull does.
        (expect (alist-get "IS_CLOSED" fall nil nil #'equal) :to-be nil)
        (with-temp-buffer
          (insert-file-contents file)
          (expect (buffer-string) :to-match "^#\\+LAST_SYNCED:")))))

  (it "omits a nil weight and a nil close date, and writes the closed flag"
    (test-gp--with-file
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-gp--mock-canvas (list test-gp--winter))))
        (org-canvas-pull-grading-periods))
      (let ((winter (test-gp--heading-props file "Fall 2026 Q2")))
        (expect (alist-get "WEIGHT" winter nil nil #'equal) :to-be nil)
        (expect (alist-get "CLOSE_DATE" winter nil nil #'equal) :to-be nil)
        (expect (alist-get "IS_CLOSED" winter nil nil #'equal) :to-equal "true"))))

  (it "orders headings by start date whatever order Canvas answers in"
    (test-gp--with-file
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-gp--mock-canvas (list test-gp--winter test-gp--fall))))
        (org-canvas-pull-grading-periods))
      (with-current-buffer (find-file-noselect file)
        (expect (org-map-entries (lambda () (org-get-heading t t t t)) "LEVEL=1" 'file)
                :to-equal '("Fall 2026 Q1" "Fall 2026 Q2")))))

  (it "upserts by CANVAS_ID on a second pull instead of duplicating"
    (test-gp--with-file
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-gp--mock-canvas (list test-gp--fall))))
        (org-canvas-pull-grading-periods))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-gp--mock-canvas
                  (list (append '((weight . 60)) (assq-delete-all 'weight (copy-alist test-gp--fall)))))))
        (org-canvas-pull-grading-periods))
      (with-current-buffer (find-file-noselect file)
        (expect (length (org-map-entries (lambda () t) "LEVEL=1" 'file)) :to-equal 1))
      (expect (alist-get "WEIGHT" (test-gp--heading-props file "Fall 2026 Q1") nil nil #'equal)
              :to-equal "60")))

  (it "writes the empty-file note and says the course has none"
    (test-gp--with-file
      (let ((said nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-gp--mock-canvas nil))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (org-canvas-pull-grading-periods))
        (expect said :to-match "0 periods (the course has none)")
        (with-temp-buffer
          (insert-file-contents file)
          (expect (buffer-string) :to-match "#\\+TITLE: Grading Periods")
          (expect (buffer-string) :to-match "Canvas returned 0 items")))))

  (it "reports one period in the singular"
    (test-gp--with-file
      (let ((said nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-gp--mock-canvas (list test-gp--fall)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (org-canvas-pull-grading-periods))
        (expect said :to-equal "Grading periods pull complete: 1 period.")))))

(describe "org-canvas--validate-due-in-grading-period"
  (defun test-gp--write-periods (file)
    "Write two periods covering Aug 19 to Dec 4, 2026, into FILE."
    (with-temp-file file
      (insert "* Q1\n:PROPERTIES:\n:CANVAS_ID: 501\n:START_DATE: <2026-08-19 Wed>\n:END_DATE: <2026-10-09 Fri>\n:END:\n"
              "* Q2\n:PROPERTIES:\n:CANVAS_ID: 502\n:START_DATE: <2026-10-10 Sat>\n:END_DATE: <2026-12-04 Fri>\n:END:\n"
              "* Broken\n:PROPERTIES:\n:CANVAS_ID: 503\n:START_DATE: not a date\n:END:\n")))

  (defmacro test-gp--validate-due (due &rest body)
    "Run BODY with point on an assignment heading whose DUE_AT is DUE.
ISSUES is bound to what the validator returned."
    (declare (indent 1))
    `(with-temp-org-buffer (format "* HW\n:PROPERTIES:\n%s:END:\n"
                                   (if ,due (format ":DUE_AT: %s\n" ,due) ""))
       (org-back-to-heading t)
       (let ((issues (org-canvas--validate-due-in-grading-period
                      (list :file "assignments.org" :line 1 :heading "HW"))))
         ,@body)))

  (it "is quiet for a due date inside a period"
    (test-gp--with-file
      (test-gp--write-periods file)
      (test-gp--validate-due "<2026-09-15 Tue 23:59>"
        (expect issues :to-be nil))))

  (it "warns for a due date outside every period, naming the file"
    (test-gp--with-file
      (test-gp--write-periods file)
      (test-gp--validate-due "<2026-12-20 Sun 23:59>"
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :severity) :to-be 'warning)
        (expect (plist-get (car issues) :property) :to-equal "DUE_AT")
        (expect (plist-get (car issues) :message)
                :to-match "falls outside every grading period in grading-periods.org"))))

  (it "treats a period's bounds as inside"
    (test-gp--with-file
      (test-gp--write-periods file)
      (test-gp--validate-due "<2026-08-19 Wed>"
        (expect issues :to-be nil))))

  (it "is quiet when the heading has no DUE_AT"
    (test-gp--with-file
      (test-gp--write-periods file)
      (test-gp--validate-due nil
        (expect issues :to-be nil))))

  (it "is quiet when no grading periods file exists, or it is empty"
    (test-gp--with-file
      (test-gp--validate-due "<2026-12-20 Sun 23:59>"
        (expect issues :to-be nil))
      (with-temp-file file (insert ""))
      (test-gp--validate-due "<2026-12-20 Sun 23:59>"
        (expect issues :to-be nil))))

  (it "leaves a period whose dates do not parse out of the bounds"
    (test-gp--with-file
      (test-gp--write-periods file)
      (expect (length (org-canvas--validate-grading-period-bounds)) :to-equal 2)))

  (it "runs from the assignment structural validator"
    (test-gp--with-file
      (test-gp--write-periods file)
      (with-temp-org-buffer "* HW\n:PROPERTIES:\n:DUE_AT: <2027-01-05 Tue>\n:END:\n"
        (org-back-to-heading t)
        (let ((issues (org-canvas--validate-assignment-structure
                       (list :file "assignments.org" :line 1 :heading "HW"))))
          (expect (cl-some (lambda (i) (equal (plist-get i :property) "DUE_AT")) issues)
                  :to-be-truthy))))))

(describe "grading periods wiring"
  (it "registers the file variable and the properties"
    (expect (org-canvas--registry-find-property "CLOSE_DATE") :not :to-be nil)
    (expect (assq 'org-canvas-grading-periods-file org-canvas--file-var-registry)
            :not :to-be nil))

  (it "is a pull in the pull tiers and in the sync tiers, with no sync command"
    (expect (cl-some (lambda (tier) (assq 'org-canvas-pull-grading-periods tier))
                     org-canvas--pull-tiers)
            :to-be-truthy)
    (expect (cl-some (lambda (tier) (assq 'org-canvas-pull-grading-periods tier))
                     org-canvas--sync-tiers)
            :to-be-truthy)
    (expect (fboundp 'org-canvas-sync-grading-periods) :to-be nil)))

(provide 'org-canvas-grading-periods-test)
;;; org-canvas-grading-periods-test.el ends here
