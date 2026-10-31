;;; org-canvas-heading-verify-test.el --- Tests for reading assignments, modules and files back -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-verify-headings' and the assignment, module
;; and file arguments of the `verify' batch command (issue #524).
;; Canvas is `org-canvas-api-request', mocked by `with-mock-api'; no
;; spec reaches the network, and none asserts on the log buffer.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-heading-verify)
(require 'org-canvas-batch)

;;;; Fixtures

(defconst test-hver--assignments
  (concat "#+TITLE: Assignments\n"
          "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1001\n:POINTS: 10\n"
          ":DUE_AT: <2026-11-06 Fri 23:59>\n:END:\nWrite it.\n"
          "* Draft\n:PROPERTIES:\n:CANVAS_ID: 1002\n:PUBLISHED: false\n:END:\n"
          "* Rubricated\n:PROPERTIES:\n:CANVAS_ID: 1003\n:POINTS: 10\n"
          ":RUBRIC_LINK: [[file:rubrics.org::*Python Basics][Python Basics]]\n:END:\n"
          "* Unpushed\nNot yet.\n")
  "Three stamped assignment headings and one never pushed.")

(defconst test-hver--rubrics
  (concat "#+TITLE: Rubrics\n"
          "* Python Basics\n:PROPERTIES:\n:CANVAS_ID: 900\n:END:\n"
          "** Sources :5pt:\nEvaluate sources.\n"
          "| Rating | Points |\n|--------+--------|\n| Full   | 5 |\n| None   | 0 |\n"
          "** Argument :5pt:\n"
          "| Rating | Points |\n|--------+--------|\n| Full   | 5 |\n| None   | 0 |\n")
  "One two-criterion rubric worth ten points.")

(defconst test-hver--modules
  (concat "#+TITLE: Modules\n"
          "* Week 1\n:PROPERTIES:\n:CANVAS_ID: 501\n:END:\n"
          "** Friday\n:PROPERTIES:\n:CANVAS_ID: 9001\n:END:\n"
          "** [[file:assignments.org::*Essay][Essay]]\n:PROPERTIES:\n:CANVAS_ID: 9003\n:END:\n"
          "** Reading\n:PROPERTIES:\n:CANVAS_ID: 9002\n"
          ":EXTERNAL_URL: https://example.com/reading\n:PUBLISHED: false\n:END:\n"
          "** [[file:assignments.org::*Unpushed][Unpushed]]\n")
  "A module whose items are a SubHeader, an assignment, a URL and an unsent link.")

(defconst test-hver--files
  (concat "#+TITLE: Files\n"
          "* [[file:deck.pdf][deck.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 7001\n"
          ":UNLOCK_AT: <2026-11-01 Sun 08:00>\n:END:\n")
  "One file heading with an unlock date.")

(defun test-hver--alist (base overrides)
  "Return a fresh copy of BASE with OVERRIDES, KEY VALUE pairs, applied."
  ;; `copy-tree': a quoted constant must not be changed in place.
  (let ((alist (copy-tree base)))
    (while overrides
      (setf (alist-get (pop overrides) alist) (pop overrides)))
    alist))

(defun test-hver--responses ()
  "Return the mock responses for every stamped heading, all agreeing."
  ;; `copy-tree': the specs change the cells in place.
  (copy-tree
   `(("assignments/1001\\'" . ((id . 1001) (name . "Essay") (published . t)
                               (points_possible . 10.0)
                               (due_at . "2026-11-06T23:59:00Z")
                               (unlock_at) (lock_at)))
     ("assignments/1002\\'" . ((id . 1002) (name . "Draft") (published . :json-false)
                               (due_at) (unlock_at) (lock_at)))
     ("assignments/1003\\'" . ((id . 1003) (name . "Rubricated") (published . t)
                               (points_possible . 10.0)
                               (due_at) (unlock_at) (lock_at)
                               (rubric_settings . ((id . 900) (title . "Python Basics")
                                                   (points_possible . 10)))
                               (rubric . [((id . 11) (description . "Sources")
                                           (points . 5))
                                          ((id . 12) (description . "Argument")
                                           (points . 5))])))
     ("rubrics/900\\'" . ((id . 900) (title . "Python Basics")
                          (points_possible . 10.0)
                          (data . [((id . 11) (description . "Sources") (points . 5)
                                    (long_description . "Evaluate sources."))
                                   ((id . 12) (description . "Argument")
                                    (points . 5))])))
     ("modules/501/items" . [((id . 9001) (title . "Friday") (position . 1)
                              (published . t))
                             ((id . 9002) (title . "Reading") (position . 3)
                              (published . :json-false))
                             ((id . 9003) (title . "Essay") (position . 2)
                              (published . t))])
     ("modules/501\\'" . ((id . 501) (name . "Week 1") (published . t)))
     ("files/7001\\'" . ((id . 7001) (display_name . "deck.pdf")
                         (locked . :json-false) (hidden . :json-false)
                         (unlock_at . "2026-11-01T08:00:00Z") (lock_at))))
   t))

(defun test-hver--respond (pattern &rest overrides)
  "Make PATTERN's mock response its default with OVERRIDES applied."
  (let ((cell (assoc pattern test-org-canvas-api-responses)))
    (setcdr cell (test-hver--alist (cdr cell) overrides))))

(defun test-hver--respond-items (items)
  "Make the module's mock item list ITEMS."
  (setcdr (assoc "modules/501/items" test-org-canvas-api-responses) items))

(defun test-hver--respond-rubric (pattern &rest overrides)
  "Make the mock response PATTERN carry OVERRIDES, (KEY VALUE) pairs."
  (let ((cell (assoc pattern test-org-canvas-api-responses)))
    (setcdr cell (test-hver--alist (cdr cell) overrides))))

(defmacro test-hver--with-course (&rest body)
  "Run BODY in a course whose files hold the fixtures.
DIR is bound to the course directory, the API mocked with
`test-hver--responses', the zone UTC and the preflight check and echo
stubbed.  The buffers are killed and the directory deleted afterwards."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "hver-" t)))
          (org-canvas-time-zone "UTC")
          (org-canvas-assignments-file (expand-file-name "assignments.org" dir))
          (org-canvas-rubrics-file (expand-file-name "rubrics.org" dir))
          (org-canvas-modules-file (expand-file-name "modules.org" dir))
          (org-canvas-files-file (expand-file-name "files.org" dir)))
     (unwind-protect
         (progn
           (with-temp-file org-canvas-assignments-file (insert test-hver--assignments))
           (with-temp-file org-canvas-rubrics-file (insert test-hver--rubrics))
           (with-temp-file org-canvas-modules-file (insert test-hver--modules))
           (with-temp-file org-canvas-files-file (insert test-hver--files))
           (with-org-canvas-test-config
             (with-mock-api
               (setq test-org-canvas-api-responses (test-hver--responses))
               (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                         ((symbol-function 'org-canvas--user-message) #'ignore))
                 ,@body))))
       (dolist (f (list org-canvas-assignments-file org-canvas-rubrics-file
                        org-canvas-modules-file org-canvas-files-file))
         (let ((buf (find-buffer-visiting f))) (when buf (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-hver--verify (&rest entries)
  "Verify ENTRIES, (FEATURE . TITLE) pairs; return (RESULTS . PRINTED-TEXT)."
  (let (results)
    (let ((text (with-output-to-string
                  (setq results (org-canvas-verify-headings entries)))))
      (cons results text))))

(defun test-hver--row (result field)
  "Return the row of RESULT named FIELD."
  (seq-find (lambda (r) (equal (plist-get r :field) field)) (plist-get result :rows)))

(defun test-hver--differences (results)
  "Return the :differences of each of RESULTS."
  (mapcar (lambda (r) (plist-get r :differences)) results))

;;;; Agreement

(describe "org-canvas-verify-headings (issue #524)"
  (it "finds nothing to report when every heading landed as written"
    (test-hver--with-course
      (pcase-let ((`(,results . ,text)
                   (test-hver--verify '("assignments" . "Essay") '("assignment" . "Draft")
                                      '("modules" . "Week 1") '("files" . "deck.pdf"))))
        (expect (test-hver--differences results) :to-equal '(0 0 1 0))
        (expect text :to-match "^#\\+TITLE: Heading verification")
        (expect text :to-match "^\\* Assignments: Essay (CANVAS_ID 1001): 0 difference(s)")
        (expect text :to-match "ok +due_at +:: Org <2026-11-06 Fri 23:59>; Canvas 2026-11-06T23:59:00Z")
        (expect text :to-match "ok +points +:: Org 10; Canvas 10.0")
        (expect text :to-match "ok +published +:: Org (unset: true); Canvas true")
        (expect text :to-match "ok +item 1 +:: Org \"Friday\" at 1, published; Canvas \"Friday\" at 1, published")
        (expect text :to-match "ok +item 2 +:: Org \"Essay\" at 2; Canvas \"Essay\" at 2, published")
        (expect text :to-match "ok +item 3 +:: Org \"Reading\" at 3, unpublished; Canvas \"Reading\" at 3, unpublished")
        (expect text :to-match "^\\* Files: deck.pdf (CANVAS_ID 7001): 0 difference(s)")
        (expect text :to-match "ok +hidden +:: Org (unset: false); Canvas false")
        (expect text :to-match "ok +unlock_at +:: Org <2026-11-01 Sun 08:00>"))))

  (it "calls a child the push never sends a difference"
    (test-hver--with-course
      (let* ((result (car (car (test-hver--verify '("modules" . "Week 1")))))
             (row (test-hver--row result "item -")))
        (expect (plist-get result :differences) :to-equal 1)
        (expect (plist-get row :org) :to-match "\"Unpushed\", not sent: its content has no CANVAS_ID"))))

  (it "reads each object by id through the registry, and writes nothing"
    (test-hver--with-course
      (test-hver--verify '("assignments" . "Essay") '("modules" . "Week 1")
                         '("files" . "deck.pdf"))
      (expect (test-org-canvas-api-called-p 'GET "/courses/[0-9]+/assignments/1001\\'")
              :to-be-truthy)
      (expect (test-org-canvas-api-called-p 'GET "/courses/[0-9]+/modules/501/items\\'")
              :to-be-truthy)
      (expect (test-org-canvas-api-called-p 'GET "/courses/[0-9]+/files/7001\\'")
              :to-be-truthy)
      (expect (cl-every (lambda (call) (eq (car call) 'GET)) test-org-canvas-api-calls)
              :to-be-truthy)
      (dolist (pair (list (cons org-canvas-assignments-file test-hver--assignments)
                          (cons org-canvas-modules-file test-hver--modules)
                          (cons org-canvas-files-file test-hver--files)))
        (expect (buffer-modified-p (find-buffer-visiting (car pair))) :to-be nil)
        (expect (with-temp-buffer (insert-file-contents (car pair)) (buffer-string))
                :to-equal (cdr pair)))))

  (it "asks Canvas for an assignment's base dates, not an override's"
    (test-hver--with-course
      (test-hver--verify '("assignments" . "Essay"))
      (expect (test-org-canvas-call-arg
               (test-org-canvas-find-api-call 'GET "assignments/1001\\'") :params)
              :to-equal '(("override_assignment_dates" . "false")))))

  (it "names a heading by its Canvas id"
    (test-hver--with-course
      (let ((result (car (car (test-hver--verify '("files" "7001" canvas-id))))))
        (expect (plist-get result :title) :to-equal "deck.pdf")
        (expect (plist-get result :differences) :to-equal 0)))))

;;;; Rubrics (issue #567)

(describe "org-canvas-verify-headings and rubrics (issue #567)"
  (it "compares the rubric an assignment carries with the one its heading links"
    (test-hver--with-course
      (pcase-let ((`(,results . ,text) (test-hver--verify '("assignments" . "Rubricated"))))
        (expect (plist-get (car results) :differences) :to-equal 0)
        (expect text :to-match "ok +rubric +:: Org \"Python Basics\" (id 900), 2 criteria, 10 points; Canvas \"Python Basics\" (id 900), 2 criteria, 10 points")
        (expect text :to-match "ok +criterion 1 +:: Org \"Sources\" (5 points); Canvas \"Sources\" (5 points)")
        (expect text :to-match "ok +criterion 2 +:: Org \"Argument\" (5 points); Canvas \"Argument\" (5 points)"))))

  (it "calls a renamed rubric and a criterion whose points moved a difference"
    (test-hver--with-course
      (test-hver--respond-rubric
       "assignments/1003\\'"
       'rubric_settings '((id . 900) (title . "Python Basics v2") (points_possible . 12))
       'rubric [((id . 11) (description . "Sources") (points . 7))
                ((id . 12) (description . "Argument") (points . 5))])
      (pcase-let ((`(,results . ,text) (test-hver--verify '("assignments" . "Rubricated"))))
        (expect (plist-get (car results) :differences) :to-equal 2)
        (expect text :to-match "DIFFERS rubric +:: Org \"Python Basics\" (id 900), 2 criteria, 10 points; Canvas \"Python Basics v2\" (id 900), 2 criteria, 12 points")
        (expect text :to-match "DIFFERS criterion 1 +:: Org \"Sources\" (5 points); Canvas \"Sources\" (7 points)"))))

  (it "reads a rubric heading back through the registry, writing nothing"
    (test-hver--with-course
      (pcase-let ((`(,results . ,text) (test-hver--verify '("rubrics" . "Python Basics"))))
        (expect (plist-get (car results) :differences) :to-equal 0)
        (expect (test-org-canvas-api-called-p 'GET "/courses/[0-9]+/rubrics/900\\'")
                :to-be-truthy)
        (expect (cl-every (lambda (call) (eq (car call) 'GET)) test-org-canvas-api-calls)
                :to-be-truthy)
        (expect text :to-match "ok +rubric +:: Org \"Python Basics\" (id 900), 2 criteria, 10 points")
        (expect text :to-match "ok +criterion 1 +:: Org \"Sources\" (5 points); Canvas \"Sources\" (5 points)"))))

  (it "calls a dropped criterion a difference, and goes on"
    (test-hver--with-course
      (test-hver--respond-rubric
       "rubrics/900\\'"
       'data [((id . 11) (description . "Sources") (points . 5))])
      (pcase-let ((`(,results . ,text) (test-hver--verify '("rubrics" . "Python Basics"))))
        (expect (plist-get (car results) :differences) :to-equal 2)
        (expect text :to-match "DIFFERS rubric +:: Org \"Python Basics\" (id 900), 2 criteria, 10 points; Canvas \"Python Basics\" (id 900), 1 criterion, 10 points")
        (expect text :to-match "DIFFERS criterion 2 +:: Org \"Argument\" (5 points); Canvas (none)")))))

;;;; Differences

(describe "org-canvas-verify-headings differences"
  (it "compares an assignment's title, PUBLISHED, POINTS and dates"
    (test-hver--with-course
      (test-hver--respond "assignments/1001\\'" 'name "Essay (edited)"
                          'published :json-false 'points_possible 12
                          'due_at "2026-11-07T23:59:00Z"
                          'lock_at "2026-11-10T00:00:00Z")
      (pcase-let ((`(,results . ,text) (test-hver--verify '("assignments" . "Essay"))))
        (expect (plist-get (car results) :differences) :to-equal 5)
        (expect text :to-match "DIFFERS title +:: Org \"Essay\"; Canvas \"Essay (edited)\"")
        (expect text :to-match "DIFFERS published +:: Org (unset: true); Canvas false")
        (expect text :to-match "DIFFERS points +:: Org 10; Canvas 12")
        (expect text :to-match "DIFFERS due_at +:: Org <2026-11-06 Fri 23:59>; Canvas 2026-11-07")
        (expect text :to-match "DIFFERS lock_at +:: Org (unset); Canvas 2026-11-10"))))

  (it "leaves POINTS out when the heading does not write it"
    (test-hver--with-course
      (let ((result (car (car (test-hver--verify '("assignments" . "Draft"))))))
        (expect (test-hver--row result "points") :to-be nil)
        (expect (plist-get (test-hver--row result "published") :org) :to-equal "false"))))

  (it "catches a re-upload that dropped a file's unlock date or published it"
    (test-hver--with-course
      (test-hver--respond "files/7001\\'" 'unlock_at nil 'locked t 'hidden t)
      (pcase-let ((`(,results . ,text) (test-hver--verify '("files" . "deck.pdf"))))
        (expect (plist-get (car results) :differences) :to-equal 3)
        (expect text :to-match "DIFFERS unlock_at +:: Org <2026-11-01 Sun 08:00>; Canvas (unset)")
        (expect text :to-match "DIFFERS published +:: Org (unset: true); Canvas false")
        (expect text :to-match "DIFFERS hidden +:: Org (unset: false); Canvas true"))))

  (it "calls a moved, renamed or republished item a difference"
    (test-hver--with-course
      (test-hver--respond-items
       [((id . 9003) (title . "Essay") (position . 1) (published . t))
        ((id . 9001) (title . "Friday!") (position . 2) (published . t))
        ((id . 9002) (title . "Reading") (position . 3) (published . t))])
      (pcase-let* ((`(,results . ,text) (test-hver--verify '("modules" . "Week 1")))
                   (result (car results)))
        (expect (plist-get result :differences) :to-equal 4)
        (expect text :to-match "DIFFERS item 1 +:: Org \"Friday\" at 1, published; Canvas \"Friday!\" at 2, published")
        (expect text :to-match "DIFFERS item 2 +:: Org \"Essay\" at 2; Canvas \"Essay\" at 1")
        (expect text :to-match "DIFFERS item 3 +:: Org \"Reading\" at 3, unpublished; Canvas \"Reading\" at 3, published"))))

  (it "calls an item no child names, or one Canvas lacks, a difference"
    (test-hver--with-course
      (test-hver--respond-items
       [((id . 9001) (title . "Friday") (position . 1) (published . t))
        ((id . 9003) (title . "Essay") (position . 2) (published . t))
        ((id . 9999) (title . "Stray") (position . 3) (published . t))])
      (pcase-let ((`(,results . ,text) (test-hver--verify '("modules" . "Week 1"))))
        (expect (plist-get (car results) :differences) :to-equal 3)
        (expect text :to-match "DIFFERS item 3 +:: Org \"Reading\" at 3, unpublished; Canvas no item 9002 in this module")
        (expect text :to-match "DIFFERS extra +:: Org no heading; Canvas \"Stray\" at 3, published"))))

  (it "calls a child with no CANVAS_ID never pushed"
    (test-hver--with-course
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-modules-file)
        (goto-char (point-min))
        (re-search-forward "^:CANVAS_ID: 9001\n")
        (replace-match "")
        (save-buffer))
      (let* ((result (car (car (test-hver--verify '("modules" . "Week 1")))))
             (row (test-hver--row result "item 1")))
        (expect (plist-get row :ok) :to-be nil)
        (expect (plist-get row :canvas) :to-equal "none (no CANVAS_ID; never pushed)")
        (expect (test-hver--row result "extra") :to-be-truthy))))

  (it "reports a heading never pushed, gone, or missing, and goes on"
    (test-hver--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (lambda (_feature id)
                   (if (equal id "1002") 'gone
                     (cdr (assoc "assignments/1001\\'" test-org-canvas-api-responses))))))
        (pcase-let ((`(,results . ,text)
                     (test-hver--verify '("assignments" . "Unpushed") '("assignments" . "Draft")
                                        '("modules" . "Nope") '("assignments" . "Essay"))))
          (expect (test-hver--differences results) :to-equal '(1 1 1 0))
          (expect text :to-match "^\\* Assignments: Unpushed: no CANVAS_ID; the heading was never pushed")
          (expect text :to-match "^\\* Assignments: Draft (CANVAS_ID 1002): not on Canvas any more")
          (expect text :to-match "^\\* Modules: Nope: No Modules heading titled \"Nope\"")))))

  (it "refuses a heading of another feature before reading anything"
    (test-hver--with-course
      (expect (org-canvas-verify-headings '(("pages" . "Syllabus"))) :to-throw 'user-error)
      (expect test-org-canvas-api-calls :to-be nil)))

  (it "asks for a feature and a title when called interactively"
    (test-hver--with-course
      (let ((noninteractive nil) (shown nil) (answers '("file" "deck.pdf")))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop answers)))
                  ((symbol-function 'display-buffer) (lambda (buf &rest _) (setq shown buf))))
          (let ((results (call-interactively #'org-canvas-verify-headings)))
            (expect (plist-get (car results) :title) :to-equal "deck.pdf")
            (expect (buffer-name shown) :to-equal "*canvas-verify-headings*"))))))

  (it "refuses to ask in batch"
    (test-hver--with-course
      (let ((noninteractive t))
        (expect (org-canvas-verify-headings) :to-throw 'user-error)))))

;;;; The command line

(describe "scripts/org-canvas verify FEATURE:TITLE (issue #524)"
  (it "tells a heading argument from a column or an announcement"
    (expect (org-canvas-batch--heading-verify-arg-p "modules:Week 1") :to-be-truthy)
    (expect (org-canvas-batch--heading-verify-arg-p "file#7001") :to-be-truthy)
    (expect (org-canvas-batch--heading-verify-arg-p "assignments:Essay") :to-be-truthy)
    (expect (org-canvas-batch--heading-verify-arg-p "rubrics:Python Basics") :to-be-truthy)
    (expect (org-canvas-batch--heading-verify-arg-p "Assignments: Essay") :to-be nil)
    (expect (org-canvas-batch--heading-verify-arg-p "announcements:Reminder") :to-be nil)
    (expect (org-canvas-batch--heading-verify-arg-p "1001") :to-be nil))

  (it "verifies columns, announcements and headings named together, all"
    (let ((columns nil) (announcements nil) (headings nil) (differences 0))
      (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
                ((symbol-function 'org-canvas-verify-headings)
                 (lambda (entries) (setq headings entries)
                   (list (list :differences differences))))
                ((symbol-function 'org-canvas-verify-announcements)
                 (lambda (entries) (setq announcements entries)
                   (list (list :differences 0))))
                ((symbol-function 'org-canvas-submissions-verify)
                 (lambda (targets hidden) (setq columns (list targets hidden))
                   (list (list :differences 0)))))
        (expect (org-canvas-batch-main '("verify" "HW" "announcements:Reminder"
                                         "modules:Week 1" "file#7001"
                                         "rubrics:Python Basics"))
                :to-equal 0)
        (expect columns :to-equal '(("HW") nil))
        (expect announcements :to-equal '(("announcements" "Reminder" title)))
        (expect headings :to-equal '(("modules" "Week 1" title) ("file" "7001" canvas-id)
                                     ("rubrics" "Python Basics" title)))
        (setq differences 2 columns nil)
        (expect (org-canvas-batch-main '("verify" "assignments:Essay")) :to-equal 1)
        (expect columns :to-be nil)))))

(provide 'org-canvas-heading-verify-test)
;;; org-canvas-heading-verify-test.el ends here
