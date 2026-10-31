;;; org-canvas-diff-report-test.el --- Drift report rendering specs  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; lisp/org-canvas-diff-report.el: the rows, their rendering, the report
;; mode and its row navigation.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(describe "org-canvas--diff-render"
  (it "says so plainly when nothing has drifted"
    (with-org-canvas-test-config
      (expect (org-canvas--diff-render '((:name "Assignments")))
              :to-match "No drift")))

  (it "lists each divergence under its feature"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Assignments"
                        :divergences ((:kind modified :title "Lab 1" :id "61"
                                       :remote-newer t
                                       :updated "2026-08-25T00:00:00Z"
                                       :fields (("POINTS" "10" "25"))))
                        :extra ((:kind extra :title "Surprise" :id "99")))))))
        (expect report :to-match "Assignments: 2 divergence")
        (expect report :to-match "CHANGED   Lab 1")
        (expect report :to-match "Canvas updated 2026-08-25")
        (expect report :to-match "POINTS.*org: 10 *canvas: 25")
        (expect report :to-match "EXTRA     Surprise")
        (expect report :to-match "2 divergence(s) found"))))

  (it "reports a feature it could not check"
    (with-org-canvas-test-config
      (expect (org-canvas--diff-render
               '((:name "Assignments" :error "Connection refused")))
              :to-match "could not check"))))

(describe "org-canvas--diff-suppressed-note (issue #81)"
  (it "says nothing when every remote item was checked"
    (expect (org-canvas--diff-suppressed-note
             '((:name "Pages" :suppressed 0) (:name "Files")))
            :to-be nil))

  (it "names the count, the feature and the reason"
    (expect (org-canvas--diff-suppressed-note
             '((:name "Pages" :suppressed 1 :skip-reason "front page")))
            :to-equal "Not checked: 1 Pages (front page).\n"))

  (it "falls back when the feature declares no reason"
    (expect (org-canvas--diff-suppressed-note
             '((:name "Pages" :suppressed 2)))
            :to-equal "Not checked: 2 Pages (excluded by this module).\n"))

  (it "joins several features into one line"
    (expect (org-canvas--diff-suppressed-note
             '((:name "Pages" :suppressed 1 :skip-reason "front page")
               (:name "Discussions" :suppressed 3 :skip-reason "announcement")))
            :to-equal
            "Not checked: 1 Pages (front page), 3 Discussions (announcement).\n"))

  (it "appears in a clean report so full coverage is not implied"
    (let ((report (org-canvas--diff-render
                   '((:name "Pages" :divergences nil :extra nil
                      :suppressed 1 :skip-reason "front page")))))
      (expect report :to-match "No drift")
      (expect report :to-match "Not checked: 1 Pages (front page)"))))

(describe "org-canvas--diff-insert-entry"
  (it "names a heading whose Canvas id is gone"
    (with-temp-buffer
      (org-canvas--diff-insert-entry '(:kind missing :title "Lab 2" :id "62"))
      (expect (buffer-string) :to-match "MISSING   Lab 2")
      (expect (buffer-string) :to-match "62 is not in this course"))))

(describe "org-canvas--diff-insert-entry notes (issues #83, #85)"
  (it "explains a change no compared field shows, naming the description when it was not compared"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind modified :title "Midterm" :id "1" :remote-newer t
         :updated "2026-08-20T19:03:10Z" :fields nil))
      (expect (buffer-string) :to-match "CHANGED   Midterm (Canvas updated 2026-08-20T19:03:10Z)")
      (expect (buffer-string) :to-match "no compared property differs")
      (expect (buffer-string) :to-match "e\\.g\\. the description or overrides")))

  (it "points past the description when it was compared"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind modified :title "Midterm" :id "1" :remote-newer t :updated "x"
         :fields nil :body-compared t))
      (expect (buffer-string) :to-match "e\\.g\\. overrides or a rubric association")
      (expect (buffer-string) :not :to-match "the description")))

  (it "adds no note when a field row explains the change"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind modified :title "Lab" :id "1" :remote-newer t :updated "x"
         :fields (("POINTS" "10" "25"))))
      (expect (buffer-string) :not :to-match "no compared property")))

  (it "renders an unclaimed pair with the property to stamp"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind unclaimed :title "R11: The Ethics Email" :id "2563810" :property "CANVAS_ID"))
      (expect (buffer-string)
              :to-equal "  UNCLAIMED R11: The Ethics Email (Canvas id 2563810 has this title and no heading claims it; adopt it with org-canvas-adopt-at-point, which stamps CANVAS_ID, or rename)\n"))))

;;;; Acting on the report (issue #103)

(describe "org-canvas-diff-mode (issue #103)"
  (it "puts the report in its own mode with the verbs bound and a legend"
    (with-current-buffer (test-org-canvas--diff-report-buffer nil)
      (expect major-mode :to-be 'org-canvas-diff-mode)
      (expect (derived-mode-p 'special-mode) :to-be-truthy)
      (expect (lookup-key org-canvas-diff-mode-map (kbd "RET")) :to-be #'org-canvas-diff-visit)
      (expect (lookup-key org-canvas-diff-mode-map (kbd "a")) :to-be #'org-canvas-diff-acknowledge)
      (expect (lookup-key org-canvas-diff-mode-map (kbd "k")) :to-be #'org-canvas-diff-delete)
      (expect (lookup-key org-canvas-diff-mode-map (kbd "p")) :to-be #'org-canvas-diff-pull)
      (expect (lookup-key org-canvas-diff-mode-map (kbd "g")) :to-be #'org-canvas-diff-refresh)
      (expect (buffer-string) :to-match "RET visit   b/B browse/edit on Canvas   a acknowledge/adopt")))

  (it "carries each row's feature and entry as a text property, and none off a row"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments"
                             :extra ((:kind extra :title "Surprise" :id "99")))))
      (test-org-canvas--diff-goto-row 'extra)
      (let ((row (get-text-property (point) 'org-canvas-diff-row)))
        (expect (plist-get row :feature) :to-equal "Assignments")
        (expect (plist-get (plist-get row :entry) :id) :to-equal "99"))
      (goto-char (point-min))
      (expect (org-canvas-diff-visit) :to-throw 'user-error)))

  (it "moves between rows with TAB and back"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments"
                             :divergences ((:kind missing :title "Lab 2" :id "62"))
                             :extra ((:kind extra :title "Surprise" :id "99")))))
      (goto-char (point-min))
      (org-canvas-diff-next-row)
      (expect (thing-at-point 'line t) :to-match "MISSING   Lab 2")
      (org-canvas-diff-next-row)
      (expect (thing-at-point 'line t) :to-match "EXTRA     Surprise")
      (expect (org-canvas-diff-next-row) :to-throw 'user-error)
      (org-canvas-diff-previous-row)
      (expect (thing-at-point 'line t) :to-match "MISSING   Lab 2")
      (expect (org-canvas-diff-previous-row) :to-throw 'user-error)))

  (it "reruns the report on g"
    (let ((ran nil))
      (cl-letf (((symbol-function 'org-canvas-diff) (lambda () (setq ran t))))
        (org-canvas-diff-refresh))
      (expect ran :to-be t))))

(describe "org-canvas-diff-visit (issue #103)"
  (it "opens the course file on the heading a CHANGED row names"
    (let ((file (make-temp-file "diff-visit-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:END:\n"
                      "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"
                      "* Lab 3\n:PROPERTIES:\n:CANVAS_ID: 62\n:END:\n"))
            (let ((org-canvas-assignments-file file)
                  (shown nil))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buf &rest _) (setq shown buf) (set-buffer buf))))
                (with-current-buffer (test-org-canvas--diff-report-buffer
                                      '((:name "Assignments"
                                         :divergences ((:kind modified :title "Lab 2" :id "61"
                                                        :fields (("POINTS" "10" "25")))))))
                  (test-org-canvas--diff-goto-row 'modified)
                  (org-canvas-diff-visit)))
              (expect (buffer-file-name shown) :to-equal (file-truename file))
              (with-current-buffer shown
                (expect (org-get-heading t t t t) :to-equal "Lab 2"))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "finds the unstamped heading an UNCLAIMED row names by title"
    (let ((file (make-temp-file "diff-visit-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:END:\n* R11: The Ethics Email\n"))
            (let ((org-canvas-assignments-file file)
                  (shown nil))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buf &rest _) (setq shown buf) (set-buffer buf))))
                (with-current-buffer (test-org-canvas--diff-report-buffer
                                      '((:name "Assignments"
                                         :extra ((:kind unclaimed :title "R11: The Ethics Email"
                                                  :id "2563810" :property "CANVAS_ID")))))
                  (test-org-canvas--diff-goto-row 'unclaimed)
                  (org-canvas-diff-visit)))
              (with-current-buffer shown
                (expect (org-get-heading t t t t) :to-equal "R11: The Ethics Email"))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "opens an EXTRA row's Canvas object in a browser, when Canvas said where"
    (let ((opened nil))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Assignments"
                                 :extra ((:kind extra :title "Surprise" :id "99"
                                          :html-url "https://x.test/courses/1/assignments/99")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-visit)))
      (expect opened :to-equal "https://x.test/courses/1/assignments/99")))

  (it "opens the registered page of an EXTRA row Canvas gave no address (issue #313)"
    (let ((opened nil)
          (org-canvas-base-url "https://canvas.test")
          (org-canvas-course-id "42"))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Modules" :extra ((:kind extra :title "Week 9" :id "7")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-visit)))
      (expect opened :to-match "/courses/42/modules")))

  (it "says so when an EXTRA row has no web address"
    (cl-letf (((symbol-function 'browse-url)
               (lambda (&rest _) (error "Nothing to open"))))
      (with-temp-buffer
        (org-canvas--diff-insert-row
         "Module Items" '(:kind extra :title "Link" :id "5"))
        (goto-char (point-min))
        (expect (org-canvas-diff-visit) :to-throw 'user-error))))

  (it "signals when the heading cannot be found"
    (let ((file (make-temp-file "diff-visit-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Lab 1\n"))
            (let ((org-canvas-assignments-file file))
              (with-current-buffer (test-org-canvas--diff-report-buffer
                                    '((:name "Assignments"
                                       :divergences ((:kind missing :title "Gone" :id "61")))))
                (test-org-canvas--diff-goto-row 'missing)
                (expect (org-canvas-diff-visit) :to-throw 'user-error))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas-diff-browse (issue #292)"
  (it "binds b and B in the report"
    (expect (lookup-key org-canvas-diff-mode-map (kbd "b")) :to-be #'org-canvas-diff-browse)
    (expect (lookup-key org-canvas-diff-mode-map (kbd "B")) :to-be #'org-canvas-diff-browse-edit))

  (it "opens a CHANGED row's html_url, and assembles its edit page"
    (let ((results '((:name "Assignments"
                      :divergences ((:kind modified :title "Essay" :id "61"
                                     :html-url "https://canvas.test/courses/42/assignments/61"
                                     :fields (("POINTS" "10" "25"))))))))
      (expect (test-org-canvas--diff-browse results 'modified)
              :to-equal "https://canvas.test/courses/42/assignments/61")
      (expect (test-org-canvas--diff-browse results 'modified t)
              :to-equal "https://canvas.test/courses/42/assignments/61/edit")))

  (it "assembles a CHANGED row's page when Canvas gave no address"
    (expect (test-org-canvas--diff-browse
             '((:name "Pages"
                :divergences ((:kind modified :title "Home" :id "home" :fields nil))))
             'modified)
            :to-equal "https://canvas.test/courses/42/pages/home"))

  (it "opens an EXTRA row's address, even for the edit page, when nothing else is known"
    (expect (org-canvas--diff-row-web-url
             '(:feature "Module Items"
               :entry (:kind extra :title "Link" :id "5"
                       :html-url "https://canvas.test/courses/42/modules/items/5"))
             t)
            :to-equal "https://canvas.test/courses/42/modules/items/5"))

  (it "refuses a MISSING row, and a row with no known page"
    (let ((r (test-org-canvas--diff-browse
              '((:name "Assignments" :divergences ((:kind missing :title "Gone" :id "61"))))
              'missing)))
      (expect (car r) :to-be 'error)
      (expect (cadr r) :to-match "not on Canvas any more"))
    (expect (org-canvas--diff-row-web-url
             '(:feature "Module Items" :entry (:kind extra :title "Link" :id "5"))
             nil)
            :to-throw 'user-error))

  (it "names an untitled row by its id when refusing"
    (cl-letf (((symbol-function 'org-canvas--diff-row-at-point)
               (lambda () '(:feature "Assignments" :entry (:kind stale-ack :id "77")))))
      (expect (condition-case e (org-canvas-diff-browse)
                (user-error (error-message-string e)))
              :to-match "77. is not on Canvas any more"))
    (expect (condition-case e
                (org-canvas--diff-row-web-url
                 '(:feature "Module Items" :entry (:kind extra :id "5")) nil)
              (user-error (error-message-string e)))
            :to-match "Module Items .5."))

  (it "refuses a PENDING row, which Canvas does not hold yet"
    (let (opened)
      (cl-letf (((symbol-function 'org-canvas--diff-row-at-point)
                 (lambda () '(:feature "Assignments"
                              :entry (:kind pending :title "Why Ethics Part 1"))))
                ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (expect (condition-case e (org-canvas-diff-browse)
                  (user-error (error-message-string e)))
                :to-match "Why Ethics Part 1. is not on Canvas yet")
        (expect opened :to-be nil))))

  (it "opens the edit page through its own command"
    (let (asked)
      (cl-letf (((symbol-function 'org-canvas-diff-browse)
                 (lambda (&optional edit) (setq asked edit))))
        (org-canvas-diff-browse-edit))
      (expect asked :to-be t))))

(describe "org-canvas-diff-acknowledge (issue #103)"
  (it "adds the row's id to the known extras with a note, persists, and marks the row"
    (let ((org-canvas-diff-known-extras nil)
          (saved nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "embedded media"))
                ((symbol-function 'org-canvas--diff-acknowledge-save)
                 (lambda (extras) (setq saved extras))))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Files" :extra ((:kind extra :title "a.png" :id "31452881")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-acknowledge)
          (expect (thing-at-point 'line t)
                  :to-match "ACK       a.png (id 31452881 acknowledged: embedded media)")
          ;; The row keeps its property, so a second key still knows it.
          (expect (get-text-property (point) 'org-canvas-diff-row) :to-be-truthy)))
      (expect org-canvas-diff-known-extras
              :to-equal '(("Files" "31452881" "embedded media")))
      (expect saved :to-equal org-canvas-diff-known-extras)))

  (it "records no note when none is given"
    (let ((org-canvas-diff-known-extras nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'org-canvas--diff-acknowledge-save) #'ignore))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Files" :extra ((:kind extra :title "a.png" :id "1")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-acknowledge)))
      (expect org-canvas-diff-known-extras :to-equal '(("Files" "1" nil)))))

  (it "drops the acknowledgment on a STALE-ACK row"
    (let ((org-canvas-diff-known-extras '(("files" "1" "old") ("Pages" "2" nil)))
          (saved nil))
      (cl-letf (((symbol-function 'org-canvas--diff-acknowledge-save)
                 (lambda (extras) (setq saved extras))))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Files" :divergences ((:kind stale-ack :id "1" :note "old")))))
          (test-org-canvas--diff-goto-row 'stale-ack)
          (org-canvas-diff-acknowledge)
          (expect (thing-at-point 'line t) :to-match "DROPPED   acknowledgment of id 1")))
      (expect org-canvas-diff-known-extras :to-equal '(("Pages" "2" nil)))
      (expect saved :to-equal '(("Pages" "2" nil)))))

  (it "refuses a row that is not an extra"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :divergences ((:kind missing :title "Gone" :id "61")))))
      (test-org-canvas--diff-goto-row 'missing)
      (expect (org-canvas-diff-acknowledge) :to-throw 'user-error)))

  (it "saves through Customize by default"
    (let ((saved nil))
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (sym val) (setq saved (list sym val)))))
        (org-canvas--diff-acknowledge-save '(("Files" "1" nil))))
      (expect saved :to-equal '(org-canvas-diff-known-extras (("Files" "1" nil)))))))

(describe "org-canvas--diff-render extra details and weight totals (issue #296)"
  (it "prints an EXTRA row's details on the line under it, inside the row"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Assignments"
                        :extra ((:kind extra :title "Pop Quiz" :id "501"
                                 :details "group 'Labs', 20 pts")))))))
        (expect report :to-match
                "EXTRA     Pop Quiz (id 501, no Org heading claims it)\n +group 'Labs', 20 pts\n")
        (expect (get-text-property (string-match "group 'Labs'" report)
                                   'org-canvas-diff-row report)
                :to-be-truthy))))

  (it "prints the weight totals under the groups, without counting them as drift"
    (with-org-canvas-test-config
      (let* ((results '((:name "Assignment Groups" :weight-totals (100 . 110.0))))
             (report (org-canvas--diff-render results)))
        (expect report :to-match "Assignment Groups: 0 divergence")
        (expect report :to-match
                "WEIGHTS   Org's groups sum to 100%, Canvas's to 110%")
        (expect report :to-match "No drift")
        (expect (org-canvas--diff-count results) :to-equal 0)))))

(describe "org-canvas--diff-render referenced media footer (issue #102)"
  (it "counts referenced files in the footer"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Files" :referenced 2)))))
        (expect report :to-match "No drift")
        (expect report :to-match
                "Referenced media: 2 unclaimed files embedded in course content (org-canvas-diff-scan-references)"))))

  (it "uses the singular for one"
    (with-org-canvas-test-config
      (expect (org-canvas--diff-render '((:name "Files" :referenced 1)))
              :to-match "Referenced media: 1 unclaimed file embedded"))))

;;;; Report-buffer commands off a row

(describe "org-canvas-diff-mode revert and row lookups"
  (it "reruns the report on revert-buffer"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments"
                             :divergences ((:kind missing :title "Lab 2" :id "62")))))
      (let ((ran nil))
        (cl-letf (((symbol-function 'org-canvas-diff) (lambda (&rest _) (setq ran t))))
          (revert-buffer))
        (expect ran :to-be t))))

  (it "signals for a row naming an unregistered feature"
    (expect (org-canvas--diff-row-feature '(:feature "Nonesuch"))
            :to-throw 'user-error))

  (it "moves to the last row from the end of the buffer"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments"
                             :divergences ((:kind missing :title "Lab 2" :id "62")))))
      (goto-char (point-max))
      (expect (get-text-property (point) 'org-canvas-diff-row) :to-be nil)
      (org-canvas-diff-previous-row)
      (expect (thing-at-point 'line t) :to-match "MISSING   Lab 2"))))

(describe "org-canvas--diff-insert-entry new kinds (issue #294)"
  (it "renders a moved item with both modules and the id to stamp"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind moved :title "Closer" :id "501" :module-id "781703"
         :where "Week 5" :to "Week 6"))
      (expect (buffer-string) :to-match "^  MOVED     Closer (item id 501 sits in module 'Week 5'; an unstamped heading places it in 'Week 6'")
      (expect (buffer-string) :to-match "stamp CANVAS_ID 501 on that heading")))

  (it "renders a pending create with its property and, for an item, its module"
    (with-temp-buffer
      (org-canvas--diff-insert-entry '(:kind pending :title "Why Ethics Part 1" :property "CANVAS_ID"))
      (org-canvas--diff-insert-entry '(:kind pending :title "Intro" :property "CANVAS_ID" :where "Week 7"))
      (expect (buffer-string)
              :to-equal (concat "  PENDING   Why Ethics Part 1 (no CANVAS_ID; the next sync creates it)\n"
                                "  PENDING   Intro (no CANVAS_ID, in module 'Week 7'; the next sync creates it)\n"))))

  (it "says a module item's twin is adopted by the sync, not by adopt-at-point"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind unclaimed :title "Readings" :id "502" :module-id "781703" :where "Week 5"))
      (expect (buffer-string) :to-match "item id 502 in module 'Week 5'.*the next sync adopts it")
      (expect (buffer-string) :not :to-match "adopt-at-point"))))

(describe "org-canvas--diff-render pending creates (issue #294)"
  (it "lists pending rows under their feature, counted apart, and says no drift"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Assignments"
                        :pending ((:kind pending :title "Why Ethics Part 1"
                                   :property "CANVAS_ID")))))))
        (expect report :to-match "Assignments: 0 divergence(s), 1 pending create(s)")
        (expect report :to-match "PENDING   Why Ethics Part 1")
        (expect report :to-match "No drift")
        (expect report :to-match "Pending creates: 1 heading with no Canvas id"))))

  (it "uses the plural and prints no pending line without pending creates"
    (with-org-canvas-test-config
      (expect (org-canvas--diff-render
               '((:name "A" :pending ((:kind pending :title "x") (:kind pending :title "y")))))
              :to-match "Pending creates: 2 headings")
      (expect (org-canvas--diff-render '((:name "A")))
              :not :to-match "Pending creates"))))

(describe "report verbs on the new rows (issue #294)"
  (it "visits the heading a PENDING row names, by its line"
    (let ((file (make-temp-file "diff-294-visit-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Readings\n* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:END:\n* Readings\n"))
            (let ((shown nil))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buf &rest _) (setq shown buf) (set-buffer buf))))
                (with-current-buffer (test-org-canvas--diff-report-buffer
                                      `((:name "Modules"
                                         :children (:name "Module Items"
                                                    :pending ((:kind pending :title "Readings"
                                                               :heading "Readings"
                                                               :file ,file :line 6))))))
                  (test-org-canvas--diff-goto-row 'pending)
                  (org-canvas-diff-visit)))
              ;; The second Readings, not the first of that title.
              (with-current-buffer shown
                (expect (line-number-at-pos) :to-equal 6))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "falls back to the title when the heading has moved since the report"
    (let ((file (make-temp-file "diff-294-visit-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Lab 1\n* Why Ethics Part 1\n"))
            (let ((where (org-canvas--diff-pending-position
                          (list :kind 'pending :heading "Why Ethics Part 1"
                                :file file :line 1))))
              (expect (cdr where) :to-be-greater-than 1)))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "names the file when a PENDING row's heading is gone"
    (let ((file (make-temp-file "diff-294-gone-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Lab 1\n"))
            (expect (condition-case e
                        (org-canvas--diff-goto-heading
                         nil (list :kind 'pending :title "Why Ethics Part 1"
                                   :heading "Why Ethics Part 1" :file file :line 1))
                      (user-error (error-message-string e)))
                    :to-match (concat "Cannot find the heading for .Why Ethics Part 1. in "
                                      (regexp-quote file))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "opens a module item's UNCLAIMED row in a browser"
    (let ((opened nil))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Modules"
                                 :children (:name "Module Items"
                                            :extra ((:kind unclaimed :title "Readings" :id "502"
                                                     :module-id "781703" :where "Week 5"
                                                     :html-url "https://x.test/items/502"))))))
          (test-org-canvas--diff-goto-row 'unclaimed)
          (org-canvas-diff-visit)))
      (expect opened :to-equal "https://x.test/items/502")))

  (it "deletes the copy a MOVED row leaves in the old module"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Modules"
                                 :children (:name "Module Items"
                                            :extra ((:kind moved :title "Closer" :id "501"
                                                     :module-id "781703" :where "Week 5"
                                                     :to "Week 6"))))))
          (test-org-canvas--diff-goto-row 'moved)
          (org-canvas-diff-delete)))
      (expect (cadr (car (test-org-canvas--diff-deletes)))
              :to-match "modules/781703/items/501$")))

  (it "acknowledges a MOVED row, and refuses to acknowledge or delete a PENDING one"
    (let* ((org-canvas-diff-known-extras nil)
           (saved nil)
           (org-canvas-diff-acknowledge-function (lambda (extras) (setq saved extras))))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Modules"
                                 :children (:name "Module Items"
                                            :extra ((:kind moved :title "Closer" :id "501"
                                                     :module-id "781703" :where "Week 5"
                                                     :to "Week 6"))
                                            :pending ((:kind pending :title "New"))))))
          (test-org-canvas--diff-goto-row 'moved)
          (org-canvas-diff-acknowledge)
          (test-org-canvas--diff-goto-row 'pending)
          (expect (org-canvas-diff-acknowledge) :to-throw 'user-error)
          (expect (org-canvas-diff-delete) :to-throw 'user-error)))
      (expect saved :to-equal '(("Module Items" "501" nil))))))

(describe "report verbs on New Quiz rows (issue #313)"
  (it "pulls an EXTRA New Quiz into a new heading through its pull-only entry"
    (test-nq-313--with-file "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n:END:\n"
      (let ((pulled nil))
        (cl-letf (((symbol-function 'org-canvas--pull-at-point-1)
                   (lambda (feature id title)
                     (setq pulled (list (plist-get feature :name) id title
                                        (org-entry-get (point) "CANVAS_ASSIGNMENT_ID"))))))
          (with-current-buffer (test-org-canvas--diff-report-buffer
                                '((:name "New Quizzes"
                                   :extra ((:kind extra :title "Web Quiz" :id "43")))))
            (test-org-canvas--diff-goto-row 'extra)
            (org-canvas-diff-pull)
            (expect (thing-at-point 'line t)
                    :to-match "PULLED    Web Quiz (id 43, new heading)")))
        (expect pulled :to-equal '("New Quizzes" "43" "Web Quiz" "43")))))

  (it "browses an EXTRA New Quiz at its assignment page, which the quiz service does not give"
    (let ((results '((:name "New Quizzes"
                      :extra ((:kind extra :title "Web Quiz" :id "43"))))))
      (expect (test-org-canvas--diff-browse results 'extra)
              :to-equal "https://canvas.test/courses/42/assignments/43")
      (expect (test-org-canvas--diff-browse results 'extra t)
              :to-equal "https://canvas.test/courses/42/assignments/43/edit")))

  (it "visits an EXTRA New Quiz at its assignment page"
    (let ((opened nil)
          (org-canvas-base-url "https://canvas.test")
          (org-canvas-course-id "42"))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "New Quizzes"
                                 :extra ((:kind extra :title "Web Quiz" :id "43")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-visit)))
      (expect opened :to-equal "https://canvas.test/courses/42/assignments/43"))))

(describe "org-canvas--diff-web-entry (issue #313)"
  (it "keeps a feature that declares its own pages"
    (let ((feature (org-canvas--registry-find-feature "assignments")))
      (expect (org-canvas--diff-web-entry feature) :to-be feature)))

  (it "falls back to the feature itself when its file registered no pages"
    (let ((feature (list :name "Side" :file-var 'test-nq-313--no-such-var)))
      (expect (org-canvas--diff-web-entry feature) :to-be feature)))

  (it "answers nil for no feature"
    (expect (org-canvas--diff-web-entry nil) :to-be nil)))

(describe "Assignment Overrides rows (issue #411)"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (defun test-diff-411--report (child)
    "Return the report buffer of an Assignments result carrying CHILD."
    (test-org-canvas--diff-report-buffer
     (list (list :name "Assignments" :children child))))

  (it "renders each row naming who, the assignment and the next push's effect"
    (let ((child (plist-get (nth 0 (test-diff-411--result (test-diff-411--items)))
                            :children)))
      (with-current-buffer (test-diff-411--report child)
        (let ((text (buffer-string)))
          (expect text :to-match "Assignment Overrides: 4 divergence(s)")
          (expect text :to-match "MISSING   Section C (row of assignment 'R6: The Verdict', due <2026-09-30 Wed 14:15>; Canvas holds no such override, so the next push of that heading creates it)")
          (expect text :to-match "CHANGED   Students: #279871 (override id 11 of assignment 'R6: The Verdict')")
          (expect text :to-match "Lock At            org: <2026-10-01 Thu 14:00>   canvas: (unset)")
          (expect text :to-match "EXTRA     Section B (override id 12 of assignment 'R6: The Verdict', due <2026-10-02 Fri 14:15>; no row of the heading's overrides table claims it, so a push keeps it — push --prune-overrides drops it, or p pulls the table)")
          (expect text :to-match "EXTRA     Students: #300 (override id 14 of assignment 'R6: The Verdict', due <2026-09-28 Mon 14:15>; no row .* keeps it — push --prune-overrides drops it, or p pulls the table; its dates are the assignment's own, which a pull writes no row for)")
          (expect text :to-match "4 divergence(s) found")))))

  (it "visits the assignment heading, browses its page, pulls it, and refuses the rest"
    (let ((pulled nil) (opened nil))
      (test-diff-411--result
       (test-diff-411--items) nil
       (lambda (result file)
         (with-current-buffer (test-diff-411--report (plist-get result :children))
           (goto-char (point-min))
           (re-search-forward "^  EXTRA     Section B")
           (let ((entry (plist-get (org-canvas--diff-row-at-point) :entry)))
             (expect (org-canvas--diff-visits-heading-p entry) :to-be t)
             (expect (cdr (org-canvas--diff-heading-position nil entry))
                     :to-equal 1))
           (cl-letf (((symbol-function 'browse-url) (lambda (url) (setq opened url)))
                     ((symbol-function 'org-canvas-pull-at-point)
                      (lambda () (setq pulled (org-get-heading t t t t))))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (buf &rest _) (set-buffer buf))))
             (expect (org-canvas-diff-browse) :to-match "assignments/456$")
             (org-canvas-diff-pull)
             (expect pulled :to-equal "R6: The Verdict")
             (expect (buffer-substring (line-beginning-position) (line-end-position))
                     :to-equal "  PULLED    Section B (with the table of assignment 'R6: The Verdict')")
             (goto-char (point-min))
             (re-search-forward "^  MISSING   Section C")
             (expect (org-canvas-diff-acknowledge) :to-throw 'user-error)
             (expect (org-canvas-diff-delete) :to-throw 'user-error)
             (expect (org-canvas-diff-stamp-move) :to-throw 'user-error)
             (org-canvas-diff-visit)
             (expect (buffer-file-name) :to-equal file)
             (expect (org-get-heading t t t t) :to-equal "R6: The Verdict")))))
      (expect opened :to-match "assignments/456$"))))

;;; org-canvas-diff-report-test.el ends here
