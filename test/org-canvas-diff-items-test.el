;;; org-canvas-diff-items-test.el --- Drift report child pass specs  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; lisp/org-canvas-diff-items.el: the module items, New Quiz items and
;; assignment overrides children, and the rows they produce.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

;;;; Issue #177: module items are compared too

(describe "org-canvas--diff-module-items (issue #177)"
  (defconst test-org-canvas-177--modules-org
    "* Week 3
:PROPERTIES:
:CANVAS_ID: 781703
:END:
** [[file:assignments.org::*R3][R3: Stakeholders]]
:PROPERTIES:
:CANVAS_ID: 5864670
:END:
* Week 4
:PROPERTIES:
:CANVAS_ID: 781704
:END:
** Moved here
:PROPERTIES:
:CANVAS_ID: 77
:END:
"
    "Two synced modules; Week 4 claims an item that still sits in Week 3.")

  (defun test-org-canvas-177--modules-diff (content remote &optional known excluded)
    "Run the Modules diff over CONTENT with the item lists in REMOTE.
REMOTE maps a module id (string) to its item vector, or to `fail'.
KNOWN binds `org-canvas-diff-known-extras'; EXCLUDED binds
`org-canvas-diff-excluded-features'.  Returns the Modules result;
its :children is the Module Items result."
    (let ((file (make-temp-file "diff-177-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert content))
            (let ((org-canvas-modules-file file)
                  (org-canvas-diff-known-extras known)
                  (org-canvas-diff-excluded-features excluded))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method url &rest _)
                             (if (string-match "modules/\\([0-9]+\\)/items" url)
                                 (let ((items (cdr (assoc (match-string 1 url) remote))))
                                   (if (eq items 'fail) (error "items failed") items))
                               ;; The module list itself, as Canvas sends it.
                               [((id . 781703) (name . "Week 3"))
                                ((id . 781704) (name . "Week 4"))]))))
                  (org-canvas--diff-feature
                   (org-canvas--registry-find-feature "modules"))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (defconst test-org-canvas-177--remote
    '(("781703" . [((id . 5864670) (type . "Assignment") (title . "R3: Stakeholders")
                    (content_id . 2563803) (position . 10)
                    (html_url . "https://canvas.example/courses/1/modules/items/5864670"))
                   ((id . 5864661) (type . "Assignment") (title . "R3: Stakeholders")
                    (content_id . 2563803) (position . 10)
                    (html_url . "https://canvas.example/courses/1/modules/items/5864661"))
                   ((id . 77) (type . "SubHeader") (title . "Moved here"))])
      ("781704" . []))
    "Week 3 holds the claimed item, its twin, and an item Week 4's heading claims.")

  (it "reports the twin no heading claims as an EXTRA of Module Items, and counts it"
    (let* ((result (test-org-canvas-177--modules-diff
                    test-org-canvas-177--modules-org test-org-canvas-177--remote))
           (child (plist-get result :children))
           (extra (plist-get child :extra)))
      ;; The modules themselves agree.
      (expect (plist-get result :extra) :to-be nil)
      (expect (plist-get result :divergences) :to-be nil)
      (expect (plist-get child :name) :to-equal "Module Items")
      (expect (length extra) :to-equal 1)
      (expect (plist-get (car extra) :kind) :to-equal 'extra)
      (expect (plist-get (car extra) :id) :to-equal "5864661")
      (expect (plist-get (car extra) :title) :to-equal "R3: Stakeholders")
      (expect (plist-get (car extra) :module-id) :to-equal "781703")
      (expect (plist-get (car extra) :where) :to-equal "Week 3")
      (expect (plist-get (car extra) :html-url) :to-match "items/5864661$")
      (expect (org-canvas--diff-count (list result child)) :to-equal 1)))

  (it "silences an acknowledged item and counts it, filed under module-items"
    (let* ((result (test-org-canvas-177--modules-diff
                    test-org-canvas-177--modules-org test-org-canvas-177--remote
                    '(("module-items" "5864661" "twin, see #179"))))
           (child (plist-get result :children)))
      (expect (plist-get child :extra) :to-be nil)
      (expect (plist-get child :acknowledged) :to-equal 1)
      (expect (plist-get child :divergences) :to-be nil)
      (expect (org-canvas--diff-count (list result child)) :to-equal 0)))

  (it "flags an acknowledged item id no module holds any more"
    (let* ((result (test-org-canvas-177--modules-diff
                    test-org-canvas-177--modules-org
                    '(("781703" . [((id . 5864670) (type . "Assignment")
                                    (title . "R3: Stakeholders"))
                                   ((id . 77) (type . "SubHeader") (title . "Moved here"))])
                      ("781704" . []))
                    '(("module-items" "5864661" "twin"))))
           (child (plist-get result :children))
           (d (car (plist-get child :divergences))))
      (expect (plist-get d :kind) :to-equal 'stale-ack)
      (expect (plist-get d :id) :to-equal "5864661")
      (expect (plist-get child :acknowledged) :to-equal 0)))

  (it "looks only into modules the file claims, and records a failed item list"
    (let* ((result (test-org-canvas-177--modules-diff
                    "* Week 3\n:PROPERTIES:\n:CANVAS_ID: 781703\n:END:\n"
                    '(("781703" . fail) ("781704" . [((id . 1) (title . "x"))]))))
           (child (plist-get result :children)))
      (expect (plist-get child :error) :to-match "items failed")
      ;; The unclaimed module is the parent's business.
      (expect (mapcar (lambda (e) (plist-get e :id)) (plist-get result :extra))
              :to-equal '("781704"))))

  (it "skips the pass when module-items is excluded, visibly"
    (let* ((result (test-org-canvas-177--modules-diff
                    test-org-canvas-177--modules-org test-org-canvas-177--remote
                    nil '("module-items")))
           (child (plist-get result :children)))
      (expect (plist-get child :excluded) :to-be t)
      (expect (plist-get child :extra) :to-be nil)))

  (it "gives no other feature a child result"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) [])))
        (expect (plist-get (org-canvas--diff-feature
                            (org-canvas--registry-find-feature "pages"))
                           :children)
                :to-be nil))))

  (it "renders the row under Module Items, naming the module, and counts it in the total"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Modules")
                       (:name "Module Items"
                        :extra ((:kind extra :title "R3: Stakeholders" :id "5864661"
                                 :module-id "781703" :where "Week 3")))))))
        (expect report :to-match "Module Items: 1 divergence")
        (expect report :to-match "EXTRA     R3: Stakeholders (id 5864661 in module 'Week 3', no Org heading claims it)")
        (expect report :to-match "1 divergence(s) found"))))

  (it "reports the child right after its feature, in the count org-canvas-diff returns"
    (with-org-canvas-test-config
      (let ((names nil))
        (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                  ((symbol-function 'org-canvas--diff-render)
                   (lambda (results)
                     (setq names (mapcar (lambda (r) (plist-get r :name)) results))
                     ""))
                  ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                  ((symbol-function 'org-canvas--diff-feature)
                   (lambda (feature)
                     (let ((name (plist-get feature :name)))
                       (if (string= name "Modules")
                           (list :name name
                                 :children (list :name "Module Items"
                                                 :extra '((:kind extra :title "Twin" :id "5864661"
                                                           :module-id "781703"))))
                         (list :name name))))))
          (expect (org-canvas-diff) :to-equal 1)
          (let ((at (cl-position "Modules" names :test #'string=)))
            (expect at :not :to-be nil)
            (expect (nth (1+ at) names) :to-equal "Module Items"))))))

  (it "deletes a module item row from its module, not from a feature URL"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        ;; The report buffer helper answers by registered feature, so
        ;; the item result rides along as the Modules result's child.
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Modules"
                                 :children (:name "Module Items"
                                            :extra ((:kind extra :title "R3: Stakeholders" :id "5864661"
                                                     :module-id "781703" :where "Week 3"))))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-delete)
          (expect (thing-at-point 'line t) :to-match "DELETED   R3: Stakeholders (id 5864661)")))
      (let ((deletes (test-org-canvas--diff-deletes)))
        (expect (length deletes) :to-equal 1)
        (expect (nth 1 (car deletes)) :to-match "modules/781703/items/5864661$")
        (expect (plist-get (nth 2 (car deletes)) :data) :to-be nil))
      ;; The object was read at the same URL and snapshotted first (#345).
      (expect (cl-find-if (lambda (r) (and (eq (car r) 'GET)
                                           (string-match-p "modules/781703/items/5864661$"
                                                           (nth 1 r))))
                          test-org-canvas--diff-delete-requests)
              :to-be-truthy)
      (expect (length (directory-files snapshot-dir nil "\\.json\\'")) :to-equal 1)))

  (it "acknowledges a module item row under Module Items"
    (let* ((org-canvas-diff-known-extras nil)
           (saved nil)
           (org-canvas-diff-acknowledge-function
            (lambda (extras) (setq saved extras))))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "twin")))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Modules"
                                 :children (:name "Module Items"
                                            :extra ((:kind extra :title "R3: Stakeholders" :id "5864661"
                                                     :module-id "781703"))))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-acknowledge)))
      (expect saved :to-equal '(("Module Items" "5864661" "twin")))
      (let ((org-canvas-diff-known-extras saved))
        (expect (org-canvas--diff-known-extras-for "module-items")
                :to-equal '(("5864661" . "twin")))))))

(describe "org-canvas--diff-module-items pairs moves and lists pending items (issue #294)"
  (defconst test-org-canvas-294--modules-org
    "* Week 5
:PROPERTIES:
:CANVAS_ID: 781703
:END:
** Readings
* Week 6
:PROPERTIES:
:CANVAS_ID: 781704
:END:
** [[file:assignments.org::*Closer: Privacy Pros and Cons][Closer: Privacy Pros and Cons]]
** Readings
** [[file:assignments.org::*Why Ethics Part 1][Why Ethics Part 1]]
* Week 7
** Intro
"
    "Week 6 carries an item Canvas holds in Week 5, and some new ones.")

  (defconst test-org-canvas-294--remote
    '(("781703" . [((id . 501) (type . "Assignment")
                    (title . "Closer: Privacy Pros and Cons")
                    (html_url . "https://canvas.example/courses/1/modules/items/501"))
                   ((id . 502) (type . "SubHeader") (title . "Readings"))])
      ("781704" . [((id . 503) (type . "SubHeader") (title . "Readings"))]))
    "Week 5 holds the moved assignment and a Readings header; Week 6 its own.")

  (defun test-org-canvas-294--child (&optional remote)
    "Return the Module Items result for the #294 fixture against REMOTE."
    (plist-get (test-org-canvas-177--modules-diff
                test-org-canvas-294--modules-org
                (or remote test-org-canvas-294--remote))
               :children))

  (it "reports an item carried to another module as MOVED, not as a deletion"
    (let* ((extra (plist-get (test-org-canvas-294--child) :extra))
           (moved (cl-find "501" extra :key (lambda (e) (plist-get e :id)) :test #'equal)))
      (expect (plist-get moved :kind) :to-equal 'moved)
      (expect (plist-get moved :where) :to-equal "Week 5")
      (expect (plist-get moved :to) :to-equal "Week 6")
      ;; What k deletes: the copy in the old module.
      (expect (plist-get moved :module-id) :to-equal "781703")
      (expect (plist-get moved :html-url) :to-match "items/501$")))

  (it "pairs a heading in the item's own module as UNCLAIMED"
    (let* ((extra (plist-get (test-org-canvas-294--child) :extra))
           (week5 (cl-find "502" extra :key (lambda (e) (plist-get e :id)) :test #'equal))
           (week6 (cl-find "503" extra :key (lambda (e) (plist-get e :id)) :test #'equal)))
      ;; Each Readings header pairs with its own module's heading.
      (expect (plist-get week5 :kind) :to-equal 'unclaimed)
      (expect (plist-get week6 :kind) :to-equal 'unclaimed)))

  (it "never pairs a SubHeader across modules"
    (let* ((extra (plist-get (test-org-canvas-294--child
                              '(("781703" . [((id . 502) (type . "SubHeader") (title . "Intro"))])
                                ("781704" . [])))
                             :extra)))
      (expect (plist-get (car extra) :kind) :to-equal 'extra)))

  (it "lists the unstamped item headings nothing paired as pending, with their module"
    (let* ((child (test-org-canvas-294--child))
           (pending (plist-get child :pending)))
      (expect (mapcar (lambda (e) (list (plist-get e :title) (plist-get e :where)))
                      pending)
              :to-equal '(("Why Ethics Part 1" "Week 6") ("Intro" "Week 7")))
      ;; The MOVED row and two UNCLAIMED rows count; pending does not.
      (expect (org-canvas--diff-count (list child)) :to-equal 3)))

  (it "lists an unsynced module as a pending create of Modules"
    (let* ((result (test-org-canvas-177--modules-diff
                    test-org-canvas-294--modules-org test-org-canvas-294--remote)))
      (expect (mapcar (lambda (e) (plist-get e :title)) (plist-get result :pending))
              :to-equal '("Week 7")))))

;;;; Module Items Pair by Content, as the Sync Adopts (issue #299)

(describe "org-canvas--diff-module-items pairs by content, as the sync adopts (issue #299)"
  (defconst test-org-canvas-299--assignments-org
    "* Closer: Privacy Pros and Cons
:PROPERTIES:
:CANVAS_ID: 2563803
:END:
* Why Ethics Part 1
:PROPERTIES:
:CANVAS_ID: 2563900
:END:
* Draft Essay
"
    "Two synced assignments and one the next sync creates.")

  (defconst test-org-canvas-299--modules-org
    "* Week 5
:PROPERTIES:
:CANVAS_ID: 781703
:END:
** [[file:assignments.org::*Why Ethics Part 1][Why Ethics Part 1]]
** [[file:assignments.org::*Draft Essay][Draft Essay]]
* Week 6
:PROPERTIES:
:CANVAS_ID: 781704
:END:
** [[file:assignments.org::*Closer: Privacy Pros and Cons][Closer]]
"
    "Three unstamped item headings, each linking an assignment.")

  (defconst test-org-canvas-299--remote
    '(("781703" . [((id . 601) (type . "Assignment") (title . "Why Ethics Part 1")
                    (content_id . 1111))
                   ((id . 602) (type . "Assignment") (title . "Draft Essay")
                    (content_id . 42))
                   ((id . 603) (type . "Assignment")
                    (title . "Closer: Privacy Pros and Cons (old)")
                    (content_id . 2563803))])
      ("781704" . [((id . 604) (type . "Assignment") (title . "Closer")
                    (content_id . 9999))]))
    "Week 5: a same-titled item linking other content, an item whose
heading's target has no id yet, and the Closer under an old title;
Week 6: an item titled Closer that links other content.")

  (defun test-org-canvas-299--run ()
    "Run the Modules diff over the #299 fixture in a directory of its own.
Returns (CHILD ASSIGNMENTS-MODIFIED ASSIGNMENTS-TEXT)."
    (let* ((dir (make-temp-file "diff-299-" t))
           (modules (expand-file-name "modules.org" dir))
           (assignments (expand-file-name "assignments.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file assignments (insert test-org-canvas-299--assignments-org))
            (with-temp-file modules (insert test-org-canvas-299--modules-org))
            (let ((org-canvas-modules-file modules)
                  (org-canvas-diff-known-extras nil)
                  (org-canvas-diff-excluded-features nil))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method url &rest _)
                             (if (string-match "modules/\\([0-9]+\\)/items" url)
                                 (cdr (assoc (match-string 1 url)
                                             test-org-canvas-299--remote))
                               [((id . 781703) (name . "Week 5"))
                                ((id . 781704) (name . "Week 6"))])))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (&rest _) (error "The report must not send"))))
                  (let ((result (org-canvas--diff-feature
                                 (org-canvas--registry-find-feature "modules")))
                        (buf (find-buffer-visiting assignments)))
                    (list (plist-get result :children)
                          (and buf (buffer-modified-p buf))
                          (with-temp-buffer
                            (insert-file-contents assignments)
                            (buffer-string))))))))
        (dolist (f (list modules assignments))
          (let ((buf (find-buffer-visiting f))) (when buf (kill-buffer buf))))
        (delete-directory dir t))))

  (defun test-org-canvas-299--by-id (child id)
    "Return the entry of CHILD's :extra whose id is ID."
    (cl-find id (plist-get child :extra)
             :key (lambda (e) (plist-get e :id)) :test #'equal))

  (it "leaves a same-titled item that links other content an EXTRA, and the heading pending"
    (let* ((child (car (test-org-canvas-299--run)))
           (e (test-org-canvas-299--by-id child "601")))
      (expect (plist-get e :kind) :to-equal 'extra)
      (expect (mapcar (lambda (p) (plist-get p :title)) (plist-get child :pending))
              :to-equal '("Why Ethics Part 1"))))

  (it "pairs a moved item by its content, whatever Canvas now calls it"
    (let* ((child (car (test-org-canvas-299--run)))
           (moved (test-org-canvas-299--by-id child "603"))
           (other (test-org-canvas-299--by-id child "604")))
      (expect (plist-get moved :kind) :to-equal 'moved)
      (expect (plist-get moved :to) :to-equal "Week 6")
      (expect (plist-get moved :by-title) :to-be nil)
      ;; The same-titled item in the heading's own module links other
      ;; content, so the sync would not adopt it.
      (expect (plist-get other :kind) :to-equal 'extra)))

  (it "falls back to the title when the linked target has no Canvas id yet, and says so"
    (let* ((child (car (test-org-canvas-299--run)))
           (e (test-org-canvas-299--by-id child "602")))
      (expect (plist-get e :kind) :to-equal 'unclaimed)
      (expect (plist-get e :by-title) :to-be t)
      (with-temp-buffer
        (org-canvas--diff-insert-entry e)
        (expect (buffer-string) :to-match "paired by title only"))))

  (it "resolves offline and writes nothing (Hard Rule 19)"
    (let ((out (test-org-canvas-299--run)))
      (expect (cadr out) :to-be nil)
      (expect (nth 2 out) :to-equal test-org-canvas-299--assignments-org)))

  (it "keeps the sync's parser from logging during the report"
    (let ((logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-dispatch)
                 (lambda (logger &rest _) (when logger (push t logged)))))
        (test-org-canvas-299--run))
      (expect logged :to-be nil)))

  (it "pairs by title alone when the heading could not be parsed"
    (let ((extra '(:kind extra :title "Closer " :id "1" :type "Assignment")))
      (expect (org-canvas--diff-module-item-match
               extra '(:match-title "Closer" :content nil))
              :to-equal 'title)
      (expect (org-canvas--diff-module-item-match
               extra '(:match-title "Opener" :content nil))
              :to-be nil)))

  (it "pairs a page by its url"
    (let ((extra '(:kind extra :title "Old Name" :id "1" :type "Page"
                   :page-url "office-hours")))
      (expect (org-canvas--diff-module-item-match
               extra '(:match-title "Office Hours"
                       :content (:type "Page" :title "Office Hours"
                                 :page-url "office-hours")))
              :to-equal 'content)))

  (it "says so on a MOVED row paired by title only, and not on one paired by content"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind moved :title "Draft" :id "9" :module-id "1" :where "Week 5"
         :to "Week 6" :by-title t))
      (org-canvas--diff-insert-entry
       '(:kind moved :title "Closer" :id "8" :module-id "1" :where "Week 5"
         :to "Week 6"))
      (let ((lines (split-string (buffer-string) "\n" t)))
        (expect (car lines) :to-match "paired by title only")
        (expect (cadr lines) :not :to-match "paired by title only")))))

(describe "a MOVED row names the heading it paired (issue #342)"
  (it "carries the partner's file, line and title from the pairing"
    (let* ((child (car (test-org-canvas-299--run)))
           (moved (test-org-canvas-299--by-id child "603")))
      (expect (plist-get moved :partner-title) :to-equal "Closer")
      (expect (plist-get moved :partner-heading)
              :to-equal "[[file:assignments.org::*Closer: Privacy Pros and Cons][Closer]]")
      (expect (plist-get moved :line) :to-equal 11)
      (expect (file-name-nondirectory (plist-get moved :file)) :to-equal "modules.org")))

  (it "prints the paired heading and says the sync moves the item, keeping its id (issue #352)"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind moved :title "R6: Arguing Both Sides" :id "5707155" :module-id "1"
         :where "Week 5" :to "Week 6" :line 10
         :partner-title "R7: Arguing Both Sides"))
      (expect (buffer-string)
              :to-match "the unstamped heading 'R7: Arguing Both Sides' (line 10) places it in 'Week 6'")
      (expect (buffer-string)
              :to-match "stamp CANVAS_ID 5707155 on that heading and the next sync instead moves it there, keeping its id (if Canvas refuses the move, it recreates it there with a new id and deletes this copy)")
      (expect (buffer-string) :not :to-match "to move it instead")))

  (it "names the heading without a line, or says an unstamped heading, when that is all it has"
    (expect (org-canvas--diff-partner-text '(:partner-title "Closer"))
            :to-equal "the unstamped heading 'Closer'")
    (expect (org-canvas--diff-partner-text '(:line 4))
            :to-equal "an unstamped heading")))

(describe "the RELOCATE row (issue #343)"
  (it "reports a stamped heading whose item sits in another module, counted apart"
    (let* ((result (test-org-canvas-177--modules-diff
                    test-org-canvas-177--modules-org test-org-canvas-177--remote))
           (child (plist-get result :children))
           (relocate (plist-get child :relocate)))
      (expect (length relocate) :to-equal 1)
      (expect (plist-get (car relocate) :kind) :to-equal 'relocate)
      (expect (plist-get (car relocate) :title) :to-equal "Moved here")
      (expect (plist-get (car relocate) :id) :to-equal "77")
      (expect (plist-get (car relocate) :where) :to-equal "Week 3")
      (expect (plist-get (car relocate) :to) :to-equal "Week 4")
      (expect (plist-get (car relocate) :line) :to-equal 13)
      ;; Only the twin counts; the relocation does not.
      (expect (org-canvas--diff-count (list child)) :to-equal 1)))

  (it "reports an item whose heading sits under a module not yet on Canvas"
    (let* ((result (test-org-canvas-177--modules-diff
                    (concat test-org-canvas-177--modules-org
                            "* Week 9\n** Carried\n:PROPERTIES:\n:CANVAS_ID: 5864661\n:END:\n")
                    test-org-canvas-177--remote))
           (relocate (plist-get (plist-get result :children) :relocate)))
      (expect (mapcar (lambda (e) (list (plist-get e :id) (plist-get e :where)
                                        (plist-get e :to)))
                      relocate)
              :to-equal '(("77" "Week 3" "Week 4")
                          ("5864661" "Week 3" "Week 9")))
      (expect (plist-get (cadr relocate) :html-url) :to-match "items/5864661$")))

  (it "follows a stamped MOVED row: the next report names it RELOCATE, not MOVED"
    (let* ((dir (make-temp-file "diff-343-" t))
           (modules (expand-file-name "modules.org" dir))
           (assignments (expand-file-name "assignments.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file assignments (insert test-org-canvas-299--assignments-org))
            (with-temp-file modules (insert test-org-canvas-299--modules-org))
            (let ((org-canvas-modules-file modules)
                  (org-canvas-diff-known-extras nil)
                  (org-canvas-diff-excluded-features nil))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method url &rest _)
                             (if (string-match "modules/\\([0-9]+\\)/items" url)
                                 (cdr (assoc (match-string 1 url)
                                             test-org-canvas-299--remote))
                               [((id . 781703) (name . "Week 5"))
                                ((id . 781704) (name . "Week 6"))])))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (&rest _) (error "Stamping must not send"))))
                  (let* ((run (lambda ()
                                (plist-get (org-canvas--diff-feature
                                            (org-canvas--registry-find-feature "modules"))
                                           :children)))
                         (moved (test-org-canvas-299--by-id (funcall run) "603")))
                    (expect (org-canvas--diff-stamp-entry moved) :to-equal 'stamped)
                    (let* ((after (funcall run))
                           (relocate (plist-get after :relocate)))
                      (expect (test-org-canvas-299--by-id after "603") :to-be nil)
                      (expect (mapcar (lambda (e) (list (plist-get e :id) (plist-get e :title)
                                                        (plist-get e :where) (plist-get e :to)))
                                      relocate)
                              :to-equal '(("603" "Closer" "Week 5" "Week 6")))))))))
        (dolist (f (list modules assignments))
          (let ((buf (find-buffer-visiting f))) (when buf (kill-buffer buf))))
        (delete-directory dir t))))

  (it "renders the row, the section count and a footer, and still says no drift"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Module Items"
                        :relocate ((:kind relocate :title "Journal 05" :id "5707155"
                                    :where "Week 06" :to "Week 07")))))))
        (expect report :to-match "Module Items: 0 divergence(s), 1 relocation(s)")
        (expect report :to-match "  RELOCATE  Journal 05 (item id 5707155 sits in module 'Week 06'; its heading, stamped with that id, is under 'Week 07', so the next sync moves it there, keeping its id (if Canvas refuses the move, it recreates it there with a new id and deletes this copy))")
        (expect report :to-match "No drift")
        (expect report :to-match "Relocations: 1 module item the next sync moves into its heading's module, keeping the id"))
      (expect (org-canvas--diff-render
               '((:name "Module Items"
                  :relocate ((:kind relocate :title "a" :id "1" :where "x" :to "y")
                             (:kind relocate :title "b" :id "2" :where "x" :to "y")))))
              :to-match "Relocations: 2 module items the next sync moves into their heading's module")
      (expect (org-canvas--diff-render '((:name "A"))) :not :to-match "Relocations")))

  (it "visits the stamped heading, opens the Canvas item, and refuses to acknowledge, delete or stamp it"
    (test-org-canvas-342--with-modules-file file
      (org-canvas--diff-stamp-entry (test-org-canvas-342--moved file))
      (let ((shown nil) (opened nil)
            (entry `(:kind relocate :title "R6: Arguing Both Sides" :id "5707155"
                     :where "Week 5" :to "Week 6" :file ,file :line 10
                     :html-url "https://x.test/items/5707155")))
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buf &rest _) (setq shown buf) (set-buffer buf)))
                  ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
          (with-current-buffer (test-org-canvas--diff-report-buffer
                                `((:name "Modules"
                                   :children (:name "Module Items" :relocate (,entry)))))
            (test-org-canvas--diff-goto-row 'relocate)
            (save-excursion (org-canvas-diff-visit))
            (org-canvas-diff-browse)
            (expect (org-canvas-diff-acknowledge) :to-throw 'user-error)
            (expect (org-canvas-diff-delete) :to-throw 'user-error)
            (expect (org-canvas-diff-stamp-move) :to-throw 'user-error)))
        (with-current-buffer shown
          (expect (line-number-at-pos) :to-equal 10))
        (expect opened :to-equal "https://x.test/items/5707155")))))

(describe "org-canvas--new-quiz-body-html (issue #313)"
  (it "exports the text above the first item, as the push does"
    (test-nq-313--with-file test-nq-313--file
      (with-current-buffer (org-canvas--find-file-noselect file)
        (goto-char (point-min))
        (let ((html (org-canvas--new-quiz-body-html)))
          (expect html :to-match "Read carefully")
          (expect html :not :to-match "Q1")))))

  (it "answers the empty string for a quiz with no text"
    (test-nq-313--with-file "* Empty\n** Q1\n"
      (with-current-buffer (org-canvas--find-file-noselect file)
        (goto-char (point-min))
        (expect (org-canvas--new-quiz-body-html) :to-equal "")))))

(describe "org-canvas--new-quiz-remote-carries (issue #313)"
  (it "compares a setting only when the reply carries its key"
    (let ((pred (org-canvas--new-quiz-remote-carries 'time_limit)))
      (expect (funcall pred nil '((time_limit . 30))) :to-be-truthy)
      (expect (funcall pred nil '((time_limit . :null))) :to-be-truthy)
      (expect (funcall pred nil '((title . "Q"))) :to-be nil))))

;;;; New Quiz items in the drift report (issue #322)

(defconst test-nq-322--file
  (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n:END:\n"
          "Read carefully.\n\n"
          "** Q1\n:PROPERTIES:\n:CANVAS_ITEM_ID: 9\n:TYPE: essay\n:POINTS: 2\n:END:\n"
          "Explain.\n\n"
          "** Q2\n:PROPERTIES:\n:CANVAS_ITEM_ID: 10\n:END:\n"
          "** Q3\n"
          "** Q4\n"
          "* Final\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 42\n:END:\n"
          "* Draft\n"
          "** D1\n")
  "A new-quizzes.org whose items drifted every way an item can.
Q1 differs in points and prompt, Q2 was deleted on Canvas, Q3 lost its
stamp, Q4 and D1 are not pushed yet.")

(defconst test-nq-322--quizzes
  '(((id . "41") (assignment_id . "41") (title . "Midterm")
     (instructions . "<p>Read carefully.</p>"))
    ((id . "42") (assignment_id . "42") (title . "Final")))
  "The quiz service's quiz list for `test-nq-322--file'.")

(defconst test-nq-322--items
  '(("41" . [((id . "9") (points_possible . 5) (position . 1)
              (entry (item_body . "<p>Q1</p>\n<p>Explain more.</p>")
                     (interaction_type_slug . "essay")))
             ((id . "11") (points_possible . 1) (position . 2)
              (entry (item_body . "<p>Web item</p>")))
             ((id . "12") (position . 3)
              (entry (item_body . "<p>Q3</p>")))])
    ("42" . [((id . "20") (entry (item_body . "<p>Q4</p>")))]))
  "Each quiz's item list, by quiz id.
Final holds an item titled Q4, which Midterm's unstamped Q4 does not
claim: items pair only within their quiz.")

(defun test-nq-322--child (items &optional known excluded)
  "Run the New Quizzes diff over `file', with ITEMS as the item lists.
ITEMS maps a quiz id to its reply, or to `fail'.  KNOWN and EXCLUDED
bind the acknowledgment and exclusion lists.  Returns the New Quiz
Items result.  Must run inside `test-nq-313--with-file'."
  (let ((org-canvas-diff-known-extras known)
        (org-canvas-diff-excluded-features excluded))
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method url &rest _)
                   (if (string-match "quizzes/\\([0-9]+\\)/items" url)
                       (let ((reply (cdr (assoc (match-string 1 url) items))))
                         (if (eq reply 'fail) (error "Items failed") reply))
                     test-nq-322--quizzes)))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "The report must not write"))))
        (plist-get (org-canvas--diff-feature
                    (org-canvas--diff-find-feature "New Quizzes"))
                   :children)))))

(defun test-nq-322--by-id (entries id)
  "Return the entry of ENTRIES whose :id is ID."
  (cl-find id entries :key (lambda (e) (plist-get e :id)) :test #'equal))

(describe "org-canvas--diff-new-quiz-items (issue #322)"
  (it "reports CHANGED, MISSING, EXTRA, UNCLAIMED and PENDING items per quiz"
    (test-nq-313--with-file test-nq-322--file
      (let* ((child (test-nq-322--child test-nq-322--items))
             (divergences (plist-get child :divergences))
             (q1 (test-nq-322--by-id divergences "9"))
             (q2 (test-nq-322--by-id divergences "10"))
             (q1-kind (plist-get q1 :kind))
             (q2-kind (plist-get q2 :kind))
             (extra (plist-get child :extra))
             (web (test-nq-322--by-id extra "11"))
             (web-kind (plist-get web :kind))
             (q3 (test-nq-322--by-id extra "12"))
             (q3-kind (plist-get q3 :kind))
             (final (test-nq-322--by-id extra "20"))
             (final-kind (plist-get final :kind))
             (pending (plist-get child :pending)))
        (expect (plist-get child :name) :to-equal "New Quiz Items")
        (expect (plist-get child :error) :to-be nil)
        (expect (length divergences) :to-equal 2)
        (expect q1-kind :to-be 'modified)
        (expect (plist-get q1 :fields)
                :to-equal '(("POINTS" "2" "5")
                            ("ITEM_BODY" "Q1 Explain." "Q1 Explain more.")))
        (expect (plist-get q1 :quiz-id) :to-equal "41")
        (expect (plist-get q1 :where) :to-equal "Midterm")
        (expect (plist-get q1 :remote-newer) :to-be nil)
        (expect q2-kind :to-be 'missing)
        (expect (plist-get q2 :title) :to-equal "Q2")
        (expect web-kind :to-be 'extra)
        (expect (plist-get web :title) :to-equal "Web item")
        (expect (plist-get web :quiz-id) :to-equal "41")
        (expect q3-kind :to-be 'unclaimed)
        (expect (plist-get q3 :heading) :to-equal "Q3")
        (expect (plist-get q3 :file) :to-equal file)
        (expect final-kind :to-be 'extra)
        (expect (plist-get final :where) :to-equal "Final")
        (expect (mapcar (lambda (e) (list (plist-get e :title) (plist-get e :where)))
                        pending)
                :to-equal '(("Q4" "Midterm") ("D1" "Draft")))
        (expect (plist-get (car pending) :property) :to-equal "CANVAS_ITEM_ID")
        (expect (org-canvas--diff-count (list child)) :to-equal 5)
        (expect (buffer-modified-p (find-buffer-visiting file)) :to-be nil))))

  (it "agrees when points, type and body match, reading the slug as Org spells it"
    (test-nq-313--with-file
        (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n:END:\n"
                "** Blank [1/2]\n:PROPERTIES:\n:CANVAS_ITEM_ID: 9\n"
                ":TYPE: short-answer\n:POINTS: 3\n:END:\nFill it in.\n"
                "- [X] yes\n"
                "** Bare\n:PROPERTIES:\n:CANVAS_ITEM_ID: 10\n:TYPE: essay\n"
                ":POINTS: 7\n:END:\n")
      (let ((child (test-nq-322--child
                    ;; Older replies carry the body and slug at the top;
                    ;; Bare's reply holds neither points nor a slug.
                    '(("41" . [((id . "9") (points_possible . 3)
                                (item_body . "<p>Blank</p><p>Fill it in.</p>")
                                (interaction_type_slug . "rich-fill-blank"))
                               ((id . "10")
                                (entry (item_body . "<p>Bare</p>")))])))))
        (expect (plist-get child :divergences) :to-be nil)
        (expect (plist-get child :extra) :to-be nil)
        (expect (plist-get child :pending) :to-be nil))))

  (it "reads a failed item list as unchecked, never as an empty quiz"
    (test-nq-313--with-file test-nq-322--file
      (let ((child (test-nq-322--child '(("41" . fail) ("42" . [])))))
        (expect (plist-get child :error) :to-match "Items failed")
        (expect (plist-get child :divergences) :to-be nil))))

  (it "refuses a reply that is not a list of items"
    (test-nq-313--with-file test-nq-322--file
      (let ((child (test-nq-322--child
                    '(("41" . ((errors . [((message . "no"))]))) ("42" . [])))))
        (expect (plist-get child :error) :to-match "item list of quiz 41"))))

  (it "files acknowledgments under new-quiz-items and flags a stale one"
    (test-nq-313--with-file test-nq-322--file
      (let* ((child (test-nq-322--child
                     test-nq-322--items
                     '(("new-quiz-items" "11" "web-built on purpose")
                       ("new-quiz-items" "99" "gone"))))
             (stale (test-nq-322--by-id (plist-get child :divergences) "99"))
             (stale-kind (plist-get stale :kind)))
        (expect (test-nq-322--by-id (plist-get child :extra) "11") :to-be nil)
        (expect (plist-get child :acknowledged) :to-equal 1)
        (expect stale-kind :to-be 'stale-ack))))

  (it "skips the pass when new-quiz-items is excluded, visibly"
    (test-nq-313--with-file test-nq-322--file
      (let ((child (test-nq-322--child test-nq-322--items nil '("new-quiz-items"))))
        (expect (plist-get child :excluded) :to-be t))))

  (it "has nothing to report without the file"
    (let ((org-canvas-new-quizzes-file "/nonexistent/new-quizzes.org"))
      (expect (org-canvas--diff-new-quiz-item-headings org-canvas-new-quizzes-file)
              :to-be nil))))

(describe "New Quiz item rows (issue #322)"
  (defun test-nq-322--report (child)
    "Return the report buffer of a New Quizzes result carrying CHILD."
    (test-org-canvas--diff-report-buffer
     (list (list :name "New Quizzes" :children child))))

  (it "names each item's quiz in its row"
    (test-nq-313--with-file test-nq-322--file
      (let ((child (test-nq-322--child test-nq-322--items)))
        (with-current-buffer (test-nq-322--report child)
          (let ((text (buffer-string)))
            (expect text :to-match "New Quiz Items: 5 divergence(s), 2 pending create(s)")
            (expect text :to-match "CHANGED   Q1 in quiz 'Midterm'")
            (expect text :to-match "ITEM_BODY")
            (expect text :to-match "MISSING   Q2 (id 10 is not in quiz 'Midterm')")
            (expect text :to-match "EXTRA     Web item (id 11 in quiz 'Midterm', no Org heading")
            (expect text :to-match "EXTRA     Q4 (id 20 in quiz 'Final'")
            (expect text :to-match "UNCLAIMED Q3 (item id 12 in quiz 'Midterm' opens with the title")
            (expect text :to-match "PENDING   Q4 (no CANVAS_ITEM_ID, in quiz 'Midterm'; ")
            (expect text :to-match "PENDING   D1 (no CANVAS_ITEM_ID, in quiz 'Draft'; "))))))

  (it "visits the heading of a CHANGED, MISSING or UNCLAIMED item row"
    (test-nq-313--with-file test-nq-322--file
      (let ((child (test-nq-322--child test-nq-322--items))
            (shown nil))
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buf &rest _) (setq shown buf) (set-buffer buf))))
          (dolist (pair '((modified . "Q1") (missing . "Q2") (unclaimed . "Q3")))
            (with-current-buffer (test-nq-322--report child)
              (test-org-canvas--diff-goto-row (car pair))
              (org-canvas-diff-visit))
            (with-current-buffer shown
              (expect (org-get-heading t t t t) :to-equal (cdr pair))))))))

  (it "opens the quiz's page for an item row, which has none of its own"
    (test-nq-313--with-file test-nq-322--file
      (let ((results (list (list :name "New Quizzes"
                                 :children (test-nq-322--child test-nq-322--items)))))
        (expect (test-org-canvas--diff-browse results 'extra)
                :to-equal "https://canvas.test/courses/42/assignments/41")
        (expect (test-org-canvas--diff-browse results 'modified t)
                :to-equal "https://canvas.test/courses/42/assignments/41/edit"))))

  (it "reads, snapshots and deletes an EXTRA item at the quiz service, under its quiz"
    (test-nq-313--with-file test-nq-322--file
      (test-org-canvas--with-snapshot-dir
        (let ((child (test-nq-322--child test-nq-322--items))
              (requests nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest args)
                       (push (list method url (plist-get args :params)) requests)
                       (and (eq method 'GET) '((id . "11"))))))
            (with-current-buffer (test-nq-322--report child)
              (test-org-canvas--diff-goto-row 'extra)
              (org-canvas-diff-delete)))
          (setq requests (nreverse requests))
          (expect (mapcar #'car requests) :to-equal '(GET DELETE))
          ;; An item is no feature's own: read with no parameters (#345).
          (expect (nth 2 (car requests)) :to-be nil)
          (dolist (r requests)
            (expect (nth 1 r)
                    :to-match "/api/quiz/v1/courses/[^/]+/quizzes/41/items/11$"))
          (expect (length (directory-files snapshot-dir nil "\\.json\\'"))
                  :to-equal 1)))))

  (it "deletes an EXTRA item through org-canvas-diff-delete-rows (#345)"
    (test-org-canvas--with-batch-delete
      (let* ((results '((:name "New Quiz Items"
                         :extra ((:kind extra :title "Web question" :id "11"
                                  :quiz-id "41" :where "Midterm" :container "quiz")))))
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "New Quiz Items" :all t)) :results results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(deleted))
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas--new-quiz-api-endpoint "quizzes/41/items/11"))))))

  (it "pulls the quiz an item row sits in, on CHANGED and EXTRA rows alike"
    (test-nq-313--with-file test-nq-322--file
      (let ((child (test-nq-322--child test-nq-322--items))
            (pulled nil))
        (cl-letf (((symbol-function 'org-canvas-pull-at-point)
                   (lambda ()
                     (push (list (org-get-heading t t t t)
                                 (org-entry-get (point) "CANVAS_ASSIGNMENT_ID"))
                           pulled))))
          (dolist (kind '(modified extra))
            (with-current-buffer (test-nq-322--report child)
              (test-org-canvas--diff-goto-row kind)
              (org-canvas-diff-pull)
              (expect (thing-at-point 'line t)
                      :to-match "PULLED    .* (id [0-9]+, with quiz 'Midterm')")))
          (expect pulled :to-equal '(("Midterm" "41") ("Midterm" "41")))))))

  (it "refuses to pull a MISSING item row, and to adopt an item's stamp"
    (test-nq-313--with-file test-nq-322--file
      (let ((child (test-nq-322--child test-nq-322--items)))
        (with-current-buffer (test-nq-322--report child)
          (test-org-canvas--diff-goto-row 'missing)
          (expect (org-canvas-diff-pull) :to-throw 'user-error)
          (test-org-canvas--diff-goto-row 'modified)
          (expect (org-canvas-diff-adopt-stamp) :to-throw 'user-error))))))

(describe "New Quiz item readers (issue #322)"
  (it "reads an item's body under entry, at the top level, or not at all"
    (expect (org-canvas--new-quiz-item-remote-body
             '((entry (item_body . "<p>A</p>")) (item_body . "<p>B</p>")))
            :to-equal "<p>A</p>")
    (expect (org-canvas--new-quiz-item-remote-body '((item_body . "<p>B</p>")))
            :to-equal "<p>B</p>")
    (expect (org-canvas--new-quiz-item-remote-body '((entry . "x") (id . 1)))
            :to-be nil))

  (it "reads an item's type the way the Org TYPE spells it"
    (expect (org-canvas--new-quiz-item-remote-type
             '((entry (interaction_type_slug . "numeric"))))
            :to-equal "numerical")
    (expect (org-canvas--new-quiz-item-remote-type
             '((interaction_type_slug . "choice")))
            :to-equal "choice")
    (expect (org-canvas--new-quiz-item-remote-type '((id . 1))) :to-be nil))

  (it "exports the heading and prompt an item pushes, without the answers"
    (test-nq-313--with-file "* Quiz\n** Pick one [0/1]\nWhich?\n- [X] This\n- [ ] That\n"
      (with-current-buffer (org-canvas--find-file-noselect file)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* ")
        (let ((html (org-canvas--new-quiz-item-body-html)))
          (expect html :to-match "Pick one")
          (expect html :to-match "Which\\?")
          (expect html :not :to-match "\\[")
          (expect html :not :to-match "This")))))

  (it "ends the prompt where the item's TYPE says its answers begin (#335, #337)"
    (test-nq-313--with-file
        (concat "* Quiz\n"
                "** Put in order\n:PROPERTIES:\n:TYPE: ordering\n:END:\nOldest first.\n1. Alpha\n2. Beta\n"
                "** Discuss\n:PROPERTIES:\n:TYPE: essay\n:END:\nConsider:\n- privacy\n- consent\n"
                "** Hot\n:PROPERTIES:\n:TYPE: hot-spot\n:END:\nClick the heart.\n")
      (with-current-buffer (org-canvas--find-file-noselect file)
        (let ((html (lambda (title)
                      (goto-char (point-min))
                      (re-search-forward (concat "^\\*\\* " title))
                      (org-canvas--new-quiz-item-body-html))))
          (let ((ordering (funcall html "Put in order"))
                (essay (funcall html "Discuss"))
                (hot (funcall html "Hot")))
            (expect ordering :to-match "Oldest first")
            (expect ordering :not :to-match "Alpha")
            (expect essay :to-match "privacy")
            ;; A pull-only type is compared all the same (#340).
            (expect hot :to-match "Click the heart"))))))

  (it "answers no remote body when the reader finds none"
    (expect (org-canvas--diff-remote-body
             '(:body-api-key "item_body" :body-remote-fn ignore) '((id . 1)))
            :to-be nil)
    (expect (org-canvas--diff-remote-body
             '(:body-api-key "item_body"
               :body-remote-fn org-canvas--new-quiz-item-remote-body)
             '((entry (item_body . "<p>A</p>"))))
            :to-equal '(item_body . "<p>A</p>"))))

;;;; Hot-spot regions in the item drift report (issue #365)

(defun test-nq-365-diff--item (regions &optional count)
  "Return a hot-spot item 9 reply holding REGIONS, COUNT of them.
Each of REGIONS is (SHAPE X1 Y1 X2 Y2)."
  `((id . "9") (points_possible . 1)
    (entry (item_body . "<p>Click the heart.</p>")
           (interaction_type_slug . "hot-spot")
           (interaction_data (image_url . "https://s3.test/item_media/a/b")
                             (hotspots_count . ,(or count (length regions))))
           (scoring_algorithm . "HotSpot")
           (scoring_data
            (value . ,(vconcat
                       (cl-loop for (shape x1 y1 x2 y2) in regions
                                for id from 1
                                collect `((id . ,id) (type . ,shape)
                                          (coordinates
                                           . [((x . ,x1) (y . ,y1))
                                              ((x . ,x2) (y . ,y2))])))))))))

(defconst test-nq-365-diff--file
  (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n:END:\n"
          "** Click the heart.\n:PROPERTIES:\n:CANVAS_ITEM_ID: 9\n"
          ":TYPE: hot-spot\n:POINTS: 1\n:HOTSPOTS_COUNT: 1\n"
          ":HOTSPOTS: square 0.3156,0.1956 0.7407,0.8362\n:END:\n")
  "A new-quizzes.org holding one pulled hot-spot item.")

(defun test-nq-365-diff--fields (item &optional content)
  "Return the CHANGED fields of item 9 in CONTENT against remote ITEM.
CONTENT defaults to `test-nq-365-diff--file'."
  (test-nq-313--with-file (or content test-nq-365-diff--file)
    (let ((child (test-nq-322--child (list (cons "41" (vector item))))))
      (plist-get (test-nq-322--by-id (plist-get child :divergences) "9")
                 :fields))))

(describe "hot-spot regions in the drift report (issue #365)"
  (it "agrees when Canvas holds the regions the file records"
    (expect (test-nq-365-diff--fields
             (test-nq-365-diff--item '(("square" 0.3156 0.1956 0.7407 0.8362))))
            :to-be nil))

  (it "reads float noise in a coordinate as no change"
    (expect (test-nq-365-diff--fields
             (test-nq-365-diff--item
              '(("square" 0.31560000000001 0.19559999999 0.7407 0.8362))))
            :to-be nil))

  (it "reports a region moved in the web UI as a CHANGED field"
    (expect (test-nq-365-diff--fields
             (test-nq-365-diff--item '(("square" 0.25 0.1956 0.7407 0.8362))))
            :to-equal '(("HOTSPOTS" "square 0.3156,0.1956 0.7407,0.8362"
                         "square 0.25,0.1956 0.7407,0.8362"))))

  (it "reports a region added in the web UI in the count and the regions"
    (expect (test-nq-365-diff--fields
             (test-nq-365-diff--item '(("square" 0.3156 0.1956 0.7407 0.8362)
                                       ("oval" 0.1 0.1 0.2 0.2))))
            :to-equal '(("HOTSPOTS_COUNT" "1" "2")
                        ("HOTSPOTS" "square 0.3156,0.1956 0.7407,0.8362"
                         "square 0.3156,0.1956 0.7407,0.8362; oval 0.1,0.1 0.2,0.2"))))

  (it "reports regions a heading pulled before #365 does not record yet"
    (expect (test-nq-365-diff--fields
             (test-nq-365-diff--item '(("square" 0.3156 0.1956 0.7407 0.8362)))
             (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n:END:\n"
                     "** Click the heart.\n:PROPERTIES:\n:CANVAS_ITEM_ID: 9\n"
                     ":TYPE: hot-spot\n:POINTS: 1\n:END:\n"))
            :to-equal '(("HOTSPOTS_COUNT" "(unset)" "1")
                        ("HOTSPOTS" "(unset)"
                         "square 0.3156,0.1956 0.7407,0.8362"))))

  (it "compares nothing a reply without the regions cannot say"
    (let ((item (test-nq-365-diff--item '(("square" 0.25 0.25 0.5 0.5)))))
      (setf (alist-get 'entry item)
            (assq-delete-all 'interaction_data
                             (assq-delete-all 'scoring_data
                                              (copy-alist (alist-get 'entry item)))))
      (expect (test-nq-365-diff--fields item) :to-be nil)))

  (it "compares no regions on an item of another type"
    (expect (test-nq-365-diff--fields
             '((id . "9") (points_possible . 1)
               (entry (item_body . "<p>Click the heart.</p>")
                      (interaction_type_slug . "choice")
                      (scoring_data (value . "x")))))
            :to-equal '(("TYPE" "hot-spot" "choice")))))

(describe "org-canvas--diff-assignment-overrides (issue #411)"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (it "asks the list for overrides and compares each heading's table with them"
    (let* ((run (test-diff-411--result (test-diff-411--items)))
           (result (nth 0 run))
           (child (plist-get result :children))
           (divergences (plist-get child :divergences))
           (extra (plist-get child :extra))
           (missing (car (test-diff-411--by-state divergences 'missing)))
           (changed (car (test-diff-411--by-state divergences 'changed)))
           (missing-kind (plist-get missing :kind))
           (changed-kind (plist-get changed :kind)))
      (expect (assoc "include[]" (nth 2 run)) :to-equal '("include[]" . "overrides"))
      (expect (assoc "override_assignment_dates" (nth 2 run)) :to-be-truthy)
      ;; The assignment itself agrees; only its overrides differ.  The
      ;; heading a pull marked deleted is the Assignments section's
      ;; MISSING row (issue #392), not this pass's business.
      (expect (mapcar (lambda (d) (plist-get d :title))
                      (plist-get result :divergences))
              :to-equal '("Gone"))
      (expect (plist-get child :name) :to-equal "Assignment Overrides")
      (expect (plist-get child :error) :to-be nil)
      (expect (length divergences) :to-equal 2)
      (expect missing-kind :to-be 'override)
      (expect (plist-get missing :title) :to-equal "Section C")
      (expect (plist-get missing :dates) :to-equal "due <2026-09-30 Wed 14:15>")
      (expect (plist-get missing :where) :to-equal "R6: The Verdict")
      (expect (plist-get missing :assignment-id) :to-equal "456")
      (expect (plist-get missing :file) :to-equal (nth 1 run))
      (expect (plist-get missing :line) :to-equal 1)
      (expect changed-kind :to-be 'override)
      (expect (plist-get changed :title) :to-equal "Students: #279871")
      (expect (plist-get changed :id) :to-equal "11")
      ;; The Canvas override itself, for `A' to rewrite the row from (#528).
      (expect (alist-get 'id (plist-get changed :override)) :to-equal 11)
      (expect (plist-get changed :fields)
              :to-equal '(("Lock At" "<2026-10-01 Thu 14:00>" "(unset)")))
      (expect (mapcar (lambda (e) (list (plist-get e :id) (plist-get e :title)
                                        (plist-get e :redundant)))
                      extra)
              :to-equal '(("12" "Section B" nil) ("14" "Students: #300" t)))
      (expect (plist-get (car extra) :dates) :to-equal "due <2026-10-02 Fri 14:15>")
      (expect (plist-get (car extra) :html-url) :to-match "assignments/456$")
      (expect (org-canvas--diff-count (list child)) :to-equal 4)))

  (it "reads a list without overrides as unchecked, never as none"
    (let* ((items (vector '((id . 456) (name . "R6: The Verdict")
                            (updated_at . "2026-09-28T16:32:20Z"))))
           (child (plist-get (nth 0 (test-diff-411--result items)) :children)))
      (expect (plist-get child :error) :to-match "no overrides for id 456")
      (expect (plist-get child :divergences) :to-be nil)))

  (it "has nothing to say when every table agrees and the rest hold none"
    (let* ((due (test-diff-411--iso "<2026-09-28 Mon 14:15>"))
           (items (vector
                   `((id . 456) (name . "R6: The Verdict") (due_at . ,due)
                     (updated_at . "2026-09-28T16:32:20Z")
                     (overrides . [((id . 10) (course_section_id . 777) (due_at . ,due))
                                   ((id . 11) (student_ids . [279871])
                                    (due_at . ,(test-diff-411--iso "<2026-09-29 Tue 14:00>"))
                                    (lock_at . ,(test-diff-411--iso "<2026-10-01 Thu 14:00>")))
                                   ((id . 13) (course_section_id . 778)
                                    (due_at . ,(test-diff-411--iso "<2026-09-30 Wed 14:15>")))]))
                   '((id . 457) (name . "No overrides") (overrides . []))))
           (child (plist-get (nth 0 (test-diff-411--result items)) :children)))
      (expect (plist-get child :error) :to-be nil)
      (expect (plist-get child :divergences) :to-be nil)
      (expect (plist-get child :extra) :to-be nil)))

  (it "skips the pass when assignment-overrides is excluded, visibly"
    (let ((child (plist-get (nth 0 (test-diff-411--result (test-diff-411--items)
                                                          '("assignment-overrides")))
                            :children)))
      (expect (plist-get child :excluded) :to-be t)))

  (it "has nothing to report without the file"
    (expect (org-canvas--diff-override-headings "/nonexistent/assignments.org" "LEVEL=1")
            :to-be nil))

  (it "spells both sides of a differing date as the table would, and an absent one as unset"
    (let ((due (test-diff-411--iso "<2026-09-28 Mon 14:15>"))
          (other (test-diff-411--iso "<2026-09-29 Tue 14:15>")))
      (expect (org-canvas--diff-override-fields
               (list :section-id "777" :due-at due :lock-at other)
               `((id . 10) (due_at . ,other)))
              :to-equal '(("Due At" "<2026-09-28 Mon 14:15>" "<2026-09-29 Tue 14:15>")
                          ("Lock At" "<2026-09-29 Tue 14:15>" "(unset)")))
      (expect (org-canvas--diff-override-fields
               (list :section-id "777" :due-at due)
               `((id . 10) (due_at . ,due) (unlock_at . ,other)))
              :to-equal '(("Unlock At" "(unset)" "<2026-09-29 Tue 14:15>"))))))

;;; org-canvas-diff-items-test.el ends here
