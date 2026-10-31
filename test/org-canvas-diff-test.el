;;; org-canvas-diff-test.el --- Drift report engine specs  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The comparison engine in lisp/org-canvas-diff.el: registry lookup, the
;; per-feature pass, and the entry points.  Each part of the report moved
;; to a file of its own has its specs beside it: fields, items, report
;; and verbs.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(describe "org-canvas--diff-find-properties"
  (it "matches the feature registry's label against the property registry key"
    ;; "Assignment Groups" in one, "assignment-groups" in the other.
    (expect (plist-get (org-canvas--diff-find-properties "Assignment Groups")
                       :query)
            :to-equal "LEVEL=2+WEIGHT={.}"))

  (it "matches a single-word label"
    (expect (org-canvas--diff-find-properties "Assignments") :to-be-truthy))

  (it "returns nil for an unknown label"
    (expect (org-canvas--diff-find-properties "Nonexistent") :to-be nil)))

(describe "org-canvas--diff-modified-p"
  (it "flags a remote item newer than the recorded baseline"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-modified-p
              (point) '((updated_at . "2026-08-25T00:00:00Z")))
             :to-be-truthy)))

  (it "leaves an item Canvas has not touched since the baseline alone"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-modified-p
              (point) '((updated_at . "2026-08-19T13:19:49Z")))
             :to-be nil)))

  (it "is inert without a baseline"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-modified-p
              (point) '((updated_at . "2026-08-25T00:00:00Z")))
             :to-be nil))))

(describe "org-canvas--diff-entry"
  (it "reports a heading whose Canvas id no longer exists"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading)
     (let ((result (org-canvas--diff-entry
                    (list :id "61" :title "Lab 1" :pom (point))
                    (make-hash-table :test 'equal) nil)))
       (expect (plist-get result :kind) :to-equal 'missing))))

  (it "ignores a heading that has never been synced"
    (with-temp-org-buffer
     "* Lab 1\n"
     (org-back-to-heading)
     (expect (org-canvas--diff-entry
              (list :id nil :title "Lab 1" :pom (point))
              (make-hash-table :test 'equal) nil)
             :to-be nil)))

  (it "reports a field difference"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:POINTS: 10
:END:
"
     (org-back-to-heading)
     (let ((index (make-hash-table :test 'equal)))
       (puthash "61" '((id . 61) (points_possible . 25)) index)
       (let ((result (org-canvas--diff-entry
                      (list :id "61" :title "Lab 1" :pom (point))
                      index
                      '((:org-prop "POINTS" :data-key :points_possible
                         :type number)))))
         (expect (plist-get result :kind) :to-equal 'modified)
         (expect (plist-get result :fields) :to-equal '(("POINTS" "10" "25")))))))

  (it "says nothing when the entry and Canvas agree"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:POINTS: 10
:END:
"
     (org-back-to-heading)
     (let ((index (make-hash-table :test 'equal)))
       (puthash "61" '((id . 61) (points_possible . 10)) index)
       (expect (org-canvas--diff-entry
                (list :id "61" :title "Lab 1" :pom (point))
                index
                '((:org-prop "POINTS" :data-key :points_possible :type number)))
               :to-be nil)))))

(describe "org-canvas--diff-feature"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (it "finds field drift, missing items and orphans in one pass"
    (let ((file (make-temp-file "diff-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:POINTS: 10\n:END:\n"
                      "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 62\n:END:\n"))
            (let ((org-canvas-assignments-file file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _)
                             '(((id . 61) (name . "Lab 1") (points_possible . 25))
                               ((id . 99) (name . "Surprise Quiz"))))))
                  (let* ((result (org-canvas--diff-feature
                                  (org-canvas--registry-find-feature "assignments")))
                         (divergences (plist-get result :divergences))
                         (extra (plist-get result :extra)))
                    ;; Lab 1 differs on POINTS, Lab 2 is gone from Canvas,
                    ;; and id 99 is on Canvas with nothing claiming it.
                    (expect (length divergences) :to-equal 2)
                    (expect (plist-get (nth 0 divergences) :fields)
                            :to-equal '(("POINTS" "10" "25")))
                    (expect (plist-get (nth 1 divergences) :kind) :to-equal 'missing)
                    (expect (length extra) :to-equal 1)
                    (expect (plist-get (car extra) :title) :to-equal "Surprise Quiz"))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "records the error instead of failing the whole report"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (error "Connection refused"))))
        (let ((result (org-canvas--diff-feature
                       (org-canvas--registry-find-feature "assignments"))))
          (expect (plist-get result :error) :to-match "Connection refused"))))))

(describe "org-canvas-diff"
  (it "returns the divergence count and writes nothing"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                ((symbol-function 'org-canvas--diff-feature)
                 (lambda (_feature)
                   (list :name "Test"
                         :extra '((:kind extra :title "Surprise" :id "99")))))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "The report must not write")))
                ((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (error "The report must not write"))))
        (expect (org-canvas-diff)
                :to-equal (length (org-canvas--diff-features)))))))

(describe "org-canvas--diff-feature skip-fn"
  (it "does not report an item the feature declares uninteresting"
    ;; Pages skip the front page, which cannot be managed as an ordinary page.
    (let ((file (make-temp-file "diff-skip-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Welcome\n"))
            (let ((org-canvas-pages-file file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _)
                             '(((url . "front") (title . "Front") (front_page . t))))))
                  (let ((result (org-canvas--diff-feature
                                 (org-canvas--registry-find-feature "pages"))))
                    (expect (plist-get result :extra) :to-be nil)
                    ;; ...but it is counted, so the report can say the check
                    ;; did not cover it (issue #81).
                    (expect (plist-get result :suppressed) :to-equal 1)
                    (expect (plist-get result :skip-reason)
                            :to-equal "front page"))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "does not count a skipped item the Org file already claims"
    ;; Once the front page is pulled (issue #82) it is compared like any
    ;; other heading, so counting it as unchecked would be a lie.
    (let ((file (make-temp-file "diff-skip-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Front\n:PROPERTIES:\n:CANVAS_URL: front\n:END:\n"))
            (let ((org-canvas-pages-file file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _)
                             '(((url . "front") (title . "Front") (front_page . t))))))
                  (expect (plist-get (org-canvas--diff-feature
                                      (org-canvas--registry-find-feature "pages"))
                                     :suppressed)
                          :to-equal 0)))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas-diff interactive output"
  (it "renders into a buffer when not running in batch"
    (with-org-canvas-test-config
      (let ((shown nil)
            (noninteractive nil))
        (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                  ((symbol-function 'display-buffer)
                   (lambda (buf &rest _) (setq shown buf)))
                  ((symbol-function 'org-canvas--diff-feature)
                   (lambda (_feature) (list :name "Test"))))
          (org-canvas-diff)
          (expect (buffer-name shown) :to-equal org-canvas--diff-buffer-name)
          (with-current-buffer shown
            (expect (buffer-string) :to-match "No drift")))))))

(describe "org-canvas-diff-batch"
  (it "exits non-zero when there is drift"
    (let (code)
      (cl-letf (((symbol-function 'org-canvas-diff) (lambda () 3))
                ((symbol-function 'kill-emacs) (lambda (c) (setq code c))))
        (org-canvas-diff-batch)
        (expect code :to-equal 1))))

  (it "exits zero when Canvas matches"
    (let (code)
      (cl-letf (((symbol-function 'org-canvas-diff) (lambda () 0))
                ((symbol-function 'kill-emacs) (lambda (c) (setq code c))))
        (org-canvas-diff-batch)
        (expect code :to-equal 0)))))

(describe "org-canvas--diff-entry with a body (issue #83)"
  (it "adds the body row to the fields and marks the body as compared"
    (with-temp-org-buffer "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n\nOld prompt.\n"
      (org-back-to-heading)
      (let ((index (make-hash-table :test 'equal)))
        (puthash "61" '((id . 61) (description . "<p>New prompt typed in the web UI.</p>"))
                 index)
        (let ((d (org-canvas--diff-entry (list :id "61" :title "Lab 1" :pom (point-marker))
                                         index nil '(:body-api-key "description"))))
          (expect (plist-get d :kind) :to-equal 'modified)
          (expect (plist-get d :body-compared) :to-be t)
          (expect (car (car (plist-get d :fields))) :to-equal "DESCRIPTION")))))

  (it "says nothing when only markup differs"
    (with-temp-org-buffer "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n\nSame.\n"
      (org-back-to-heading)
      (let ((index (make-hash-table :test 'equal)))
        (puthash "61" '((id . 61) (description . "<div><p>Same.</p></div>")) index)
        (expect (org-canvas--diff-entry (list :id "61" :title "Lab 1" :pom (point-marker))
                                        index nil '(:body-api-key "description"))
                :to-be nil)))))

(describe "org-canvas--diff-pair-unclaimed (issue #85)"
  (it "re-kinds an extra whose title an unstamped heading shares"
    (let ((paired (org-canvas--diff-pair-unclaimed
                   '((:kind extra :title "R11" :id "2563810")
                     (:kind extra :title "Surprise" :id "99"))
                   '((:id nil :title "R11") (:id "5" :title "Surprise"))
                   "CANVAS_ID")))
      (expect (plist-get (nth 0 paired) :kind) :to-equal 'unclaimed)
      (expect (plist-get (nth 0 paired) :property) :to-equal "CANVAS_ID")
      (expect (plist-get (nth 0 paired) :id) :to-equal "2563810")
      ;; Surprise is claimed by another heading, so its namesake stays extra.
      (expect (plist-get (nth 1 paired) :kind) :to-equal 'extra)))

  (it "is the identity with no unstamped headings"
    (expect (org-canvas--diff-pair-unclaimed '((:kind extra :title "R11" :id "1")) nil "CANVAS_ID")
            :to-equal '((:kind extra :title "R11" :id "1")))))

(describe "org-canvas--diff-feature unclaimed and body (issues #83, #85)"
  (it "pairs an extra with the unstamped heading of its name and asks for page bodies"
    (let ((file (make-temp-file "diff-" nil ".org"))
          (seen-params nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* R11: The Ethics Email\n\nPrompt.\n"
                      "* Welcome\n:PROPERTIES:\n:CANVAS_URL: welcome\n"
                      ":CANVAS_UPDATED_AT: 2026-08-01T00:00:00Z\n:END:\n\nHello class.\n"))
            (let ((org-canvas-pages-file file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_method _url &optional params)
                             (setq seen-params params)
                             '(((url . "r11-the-ethics-email")
                                (title . "R11: The Ethics Email")
                                (body . "<p>Prompt.</p>"))
                               ((url . "welcome") (title . "Welcome")
                                (body . "<p>Hello everyone.</p>")
                                (updated_at . "2026-08-25T00:00:00Z"))))))
                  (let* ((result (org-canvas--diff-feature
                                  (org-canvas--registry-find-feature "pages")))
                         (extra (plist-get result :extra))
                         (divergences (plist-get result :divergences)))
                    (expect (assoc "include[]" seen-params) :to-equal '("include[]" . "body"))
                    (expect (length extra) :to-equal 1)
                    (expect (plist-get (car extra) :kind) :to-equal 'unclaimed)
                    (expect (plist-get (car extra) :property) :to-equal "CANVAS_URL")
                    (expect (length divergences) :to-equal 1)
                    (expect (plist-get (car divergences) :body-compared) :to-be t)
                    (expect (car (car (plist-get (car divergences) :fields)))
                            :to-equal "BODY"))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas--diff-feature for a global endpoint (issue #87)"
  (it "lists calendar events at their own URL and compares them"
    (let ((file (make-temp-file "diff-87-" nil ".org"))
          (seen nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Office Hours\n:PROPERTIES:\n:CANVAS_ID: 42\n:LOCATION_NAME: Room 1\n:END:\n"))
            (with-org-canvas-test-config
              (let ((org-canvas-calendar-events-file file))
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (_m url &optional params)
                             (setq seen (cons url params))
                             '(((id . 42) (title . "Office Hours")
                                (location_name . "Room 2"))))))
                  (let* ((result (org-canvas--diff-feature
                                  (org-canvas--registry-find-feature "calendar-events")))
                         (d (car (plist-get result :divergences))))
                    (expect (plist-get result :error) :to-be nil)
                    (expect (plist-get d :fields)
                            :to-equal '(("LOCATION_NAME" "Room 1" "Room 2")))))
                (expect (car seen)
                        :to-equal (concat test-org-canvas-base-url "/api/v1/calendar_events"))
                (expect (assoc "all_events" (cdr seen)) :to-be-truthy))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas--diff-modified-p modified field (issue #94)"
  (it "ignores an updated_at bump when told the content field"
    (with-temp-org-buffer "* f\n:PROPERTIES:\n:CANVAS_UPDATED_AT: 2026-08-31T18:34:37Z\n:END:\n"
      (org-back-to-heading)
      (let ((item '((updated_at . "2026-09-01T12:12:24Z")
                    (modified_at . "2026-08-31T18:34:37Z"))))
        (expect (org-canvas--diff-modified-p (point) item 'modified_at) :to-be nil)
        (expect (org-canvas--diff-modified-p (point) item) :to-be-truthy)))))

(describe "org-canvas--diff-feature for files (issue #94)"
  (defun test-diff-94--run (modified-at)
    "Diff one stamped file heading against a remote with MODIFIED-AT.
The remote updated_at is always newer than the baseline."
    (let ((file (make-temp-file "diff-94-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* syllabus.pdf\n:PROPERTIES:\n:CANVAS_ID: 31495932\n"
                      ":CANVAS_UPDATED_AT: 2026-08-31T18:34:37Z\n:END:\n"))
            (let ((org-canvas-files-file file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _)
                             `(((id . 31495932) (display_name . "syllabus.pdf")
                                (updated_at . "2026-09-01T12:12:24Z")
                                (modified_at . ,modified-at))))))
                  (org-canvas--diff-feature
                   (org-canvas--registry-find-feature "files"))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "does not report a file whose bytes are unchanged"
    (expect (plist-get (test-diff-94--run "2026-08-31T18:34:37Z") :divergences)
            :to-be nil))

  (it "reports a replaced file, showing the content timestamp"
    (let ((d (car (plist-get (test-diff-94--run "2026-09-01T09:00:00Z")
                             :divergences))))
      (expect (plist-get d :remote-newer) :to-be-truthy)
      (expect (plist-get d :updated) :to-equal "2026-09-01T09:00:00Z"))))

;;;; Issue #98: a synced course can read zero

(describe "scaffolding skip-fns (issue #98)"
  (it "suppresses the course root outcome group, but not a child group"
    (let ((skip (plist-get (org-canvas--registry-find-feature "outcomes") :skip-fn)))
      (expect (funcall skip '((id . 127270) (title . "Course root"))) :to-be-truthy)
      (expect (funcall skip '((id . 2) (parent_outcome_group . :null))) :to-be-truthy)
      (expect (funcall skip '((id . 3) (parent_outcome_group . ((id . 127270)))))
              :to-be nil)))

  (it "suppresses the stock Assignments group"
    (let ((skip (plist-get (org-canvas--registry-find-feature "assignment-groups")
                           :skip-fn)))
      (expect (funcall skip '((id . 677142) (name . "Assignments"))) :to-be-truthy)
      (expect (funcall skip '((id . 2) (name . "Homework"))) :to-be nil)))

  (it "suppresses a classic quiz's shadow assignment"
    (let ((skip (plist-get (org-canvas--registry-find-feature "assignments")
                           :skip-fn)))
      (expect (funcall skip '((id . 2555670) (quiz_id . 5))) :to-be-truthy)
      (expect (funcall skip '((id . 2) (quiz_id . :null))) :to-be nil)
      (expect (funcall skip '((id . 3) (name . "Essay"))) :to-be nil)))

  (it "names a reason for each"
    (dolist (f '("outcomes" "assignment-groups" "assignments"))
      (expect (plist-get (org-canvas--registry-find-feature f) :skip-reason)
              :to-be-truthy))))

(describe "org-canvas-diff-excluded-features (issue #98)"
  (it "does not diff the feature and keeps the count at zero"
    (with-org-canvas-test-config
      (let ((org-canvas-diff-excluded-features '("announcements"))
            ;; The bodies of an excluded feature are still read for the
            ;; media they embed (issue #111); nothing else is requested.
            (org-canvas-diff-scan-references nil)
            (checked nil))
        (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                  ((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'org-canvas--diff-feature)
                   (lambda (feature)
                     (push (plist-get feature :name) checked)
                     (list :name (plist-get feature :name)))))
          (expect (org-canvas-diff) :to-equal 0)
          (expect checked :not :to-contain "Announcements")
          (expect (length checked)
                  :to-equal (1- (length (org-canvas--diff-features))))))))

  (it "renders the exclusion as its own visible line"
    (with-org-canvas-test-config
      (expect (org-canvas--diff-render '((:name "Announcements" :excluded t)))
              :to-match "Announcements: not checked (org-canvas-diff-excluded-features)"))))

(describe "org-canvas-diff-known-extras (issue #98)"
  (defun test-ack-98--files (known remote)
    "Run the files diff with KNOWN acknowledged against REMOTE items."
    (let ((file (make-temp-file "ack-98-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Real\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (let ((org-canvas-files-file file)
                  (org-canvas-diff-known-extras known))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _) remote)))
                  (org-canvas--diff-feature
                   (org-canvas--registry-find-feature "files"))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "keeps an acknowledged extra out of the report and counts it"
    (let ((result (test-ack-98--files
                   '(("files" "31505574" "embedded media"))
                   '(((id . 1) (display_name . "Real"))
                     ((id . 31505574) (display_name . "media.mp4"))
                     ((id . 31452881) (display_name . "other.mp4"))))))
      (expect (plist-get result :acknowledged) :to-equal 1)
      (expect (mapcar (lambda (e) (plist-get e :id)) (plist-get result :extra))
              :to-equal '("31452881"))
      (expect (plist-get result :divergences) :to-be nil)))

  (it "flags an acknowledged id Canvas no longer holds, and counts it"
    (let ((result (test-ack-98--files
                   '(("files" "99" "old duplicate"))
                   '(((id . 1) (display_name . "Real"))))))
      (expect (plist-get result :acknowledged) :to-equal 0)
      (let ((d (car (plist-get result :divergences))))
        (expect (plist-get d :kind) :to-equal 'stale-ack)
        (expect (plist-get d :id) :to-equal "99")
        (expect (plist-get d :note) :to-equal "old duplicate"))
      (expect (org-canvas--diff-count (list result)) :to-equal 1)))

  (it "does not flag an acknowledged id that a heading now claims"
    (let ((result (test-ack-98--files
                   '(("files" "1" nil))
                   '(((id . 1) (display_name . "Real"))))))
      (expect (plist-get result :divergences) :to-be nil)
      (expect (plist-get result :acknowledged) :to-equal 0)))

  (it "renders the stale entry and the acknowledged footer count"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Files"
                        :divergences ((:kind stale-ack :id "99" :note "old dup"))
                        :extra nil :acknowledged 2)))))
        (expect report :to-match "STALE-ACK id 99")
        (expect report :to-match "old dup")
        (expect report :to-match "Acknowledged extras: 2 (org-canvas-diff-known-extras)")
        (expect report :to-match "1 divergence"))))

  (it "matches feature names however they are spelled"
    (expect (org-canvas--diff-known-extras-for "Assignment Groups")
            :to-equal nil)
    (let ((org-canvas-diff-known-extras '(("assignment_groups" 677142 nil))))
      (expect (org-canvas--diff-known-extras-for "Assignment Groups")
              :to-equal '(("677142" . nil))))))

(describe "org-canvas--diff-entry carries the web address (issue #292)"
  (it "keeps html_url on a CHANGED entry"
    (with-temp-org-buffer "* Essay\n:PROPERTIES:\n:CANVAS_ID: 61\n:CANVAS_UPDATED_AT: 2026-01-01T00:00:00Z\n:END:\n"
      (let* ((index (org-canvas--diff-remote-index
                     '(((id . 61) (updated_at . "2026-02-01T00:00:00Z")
                        (html_url . "https://canvas.test/courses/42/assignments/61")))
                     'id))
             (entry (org-canvas--diff-entry
                     (list :id "61" :title "Essay" :pom (point-marker)) index nil)))
        (expect (plist-get entry :kind) :to-be 'modified)
        (expect (plist-get entry :html-url)
                :to-equal "https://canvas.test/courses/42/assignments/61")))))

(describe "org-canvas-diff-unclaimed carries the web address (issue #103)"
  (it "keeps html_url on extra and unclaimed entries"
    (let* ((unclaimed (org-canvas--diff-unclaimed
                       '(((id . 99) (name . "Surprise") (html_url . "https://x.test/a/99")))
                       nil 'id 'name nil))
           (paired (org-canvas--diff-pair-unclaimed
                    (car unclaimed) '((:id nil :title "Surprise")) "CANVAS_ID")))
      (expect (plist-get (car (car unclaimed)) :html-url) :to-equal "https://x.test/a/99")
      (expect (plist-get (car paired) :kind) :to-be 'unclaimed)
      (expect (plist-get (car paired) :html-url) :to-equal "https://x.test/a/99"))))

(describe "org-canvas--diff-feature records referenced media (issue #102)"
  (it "notes the file ids a body feature's items embed"
    (let ((file (make-temp-file "diff-refs-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Welcome\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (let ((org-canvas-announcements-file file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _)
                             '(((id . 1) (title . "Welcome")
                                (message . "<img src=\"/courses/1/files/31452881/preview\">"))))))
                  (let ((result (org-canvas--diff-feature
                                 (org-canvas--registry-find-feature "announcements"))))
                    (expect (plist-get result :referenced-files)
                            :to-equal '("31452881")))
                  (let ((org-canvas-diff-scan-references nil))
                    (expect (plist-get (org-canvas--diff-feature
                                        (org-canvas--registry-find-feature "announcements"))
                                       :referenced-files)
                            :to-be nil))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas--diff-feature keeps its list reply (issue #296)"
  (it "records the items it read for the details pass"
    (with-org-canvas-test-config
      (let ((org-canvas-assignments-file "/nonexistent/assignments.org"))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (vector '((id . 99) (name . "Surprise"))))))
          (let ((result (org-canvas--diff-feature
                         (org-canvas--registry-find-feature "assignments"))))
            (expect (plist-get result :remote-items)
                    :to-equal '(((id . 99) (name . "Surprise"))))))))))

(describe "org-canvas-diff applies the reference scan (issue #102)"
  (it "excludes referenced files from the divergence count"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                ((symbol-function 'org-canvas--diff-feature)
                 (lambda (feature)
                   (pcase (plist-get feature :name)
                     ("Files" (list :name "Files"
                                    :extra '((:kind extra :title "a.png" :id "31452881")
                                             (:kind extra :title "dup.png" :id "31505574"))))
                     ("Announcements" (list :name "Announcements"
                                            :referenced-files '("31452881")))
                     (name (list :name name))))))
        (expect (org-canvas-diff) :to-equal 1)))))

;;;; Pending Creates and Moved Module Items (issue #294)

(describe "org-canvas--diff-display-title (issue #294)"
  (it "reads a link heading by its description, as Canvas names the item"
    (expect (org-canvas--diff-display-title
             "[[file:assignments.org::*Closer: Privacy Pros and Cons][Closer: Privacy Pros and Cons]]")
            :to-equal "Closer: Privacy Pros and Cons"))

  (it "reads a bare heading link by the heading it targets"
    (expect (org-canvas--diff-display-title "[[file:assignments.org::*R3: Stakeholders]]")
            :to-equal "R3: Stakeholders"))

  (it "reads a bare file link by the file's name"
    (expect (org-canvas--diff-display-title "[[file:content/syllabus.pdf]]")
            :to-equal "syllabus.pdf"))

  (it "reads any other bare link as its target"
    (expect (org-canvas--diff-display-title "[[https://x.test/a]]")
            :to-equal "https://x.test/a"))

  (it "leaves plain text alone, less its statistics cookie and whitespace"
    (expect (org-canvas--diff-display-title "  Week 6 [2/3] ") :to-equal "Week 6")
    (expect (org-canvas--diff-display-title nil) :to-equal ""))

  (it "keeps the text around a link inside a heading"
    (expect (org-canvas--diff-display-title "Read [[https://x.test][the brief]] first")
            :to-equal "Read the brief first")))

(describe "org-canvas--diff-pair-unclaimed on the shown title (issue #294)"
  (it "pairs an extra with an unstamped heading that is a link"
    (let ((paired (org-canvas--diff-pair-unclaimed
                   '((:kind extra :title "syllabus.pdf" :id "31"))
                   '((:id nil :title "[[file:content/syllabus.pdf][syllabus.pdf]]"
                      :match-title "syllabus.pdf"))
                   "CANVAS_ID")))
      (expect (plist-get (car paired) :kind) :to-equal 'unclaimed))))

(describe "org-canvas--diff-feature pending creates (issue #294)"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (defun test-org-canvas-294--feature-diff (feature-name var content items)
    "Diff FEATURE-NAME with its file VAR holding CONTENT against ITEMS.
Returns the result and whether the file's buffer was left modified."
    (let ((file (make-temp-file "diff-294-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file (insert content))
            (cl-progv (list var) (list file)
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _) items))
                          ((symbol-function 'org-canvas-api-request)
                           (lambda (&rest _) (error "The report must not send"))))
                  (let ((result (org-canvas--diff-feature
                                 (org-canvas--registry-find-feature feature-name))))
                    (list result
                          (buffer-modified-p (find-buffer-visiting file))))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "lists an unstamped heading as pending, apart from the divergences"
    (let* ((out (test-org-canvas-294--feature-diff
                 "assignments" 'org-canvas-assignments-file
                 (concat "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"
                         "* Why Ethics Part 1\n:PROPERTIES:\n:POINTS: 20\n:END:\n"
                         "* R11: The Ethics Email\n")
                 [((id . 61) (name . "Lab 1"))
                  ((id . 99) (name . "R11: The Ethics Email"))]))
           (result (car out))
           (pending (plist-get result :pending)))
      (expect (length pending) :to-equal 1)
      (expect (plist-get (car pending) :kind) :to-equal 'pending)
      (expect (plist-get (car pending) :title) :to-equal "Why Ethics Part 1")
      (expect (plist-get (car pending) :property) :to-equal "CANVAS_ID")
      (expect (plist-get (car pending) :line) :to-equal 5)
      ;; The heading the UNCLAIMED row already names is not listed twice.
      (expect (plist-get (car (plist-get result :extra)) :kind) :to-equal 'unclaimed)
      (expect (org-canvas--diff-count (list result)) :to-equal 1)
      (expect (org-canvas--diff-pending-count (list result)) :to-equal 1)
      ;; Nothing was written (Hard Rule 19).
      (expect (cadr out) :to-be nil)))

  (it "names the id property the feature stamps"
    (let* ((result (car (test-org-canvas-294--feature-diff
                         "pages" 'org-canvas-pages-file "* Office Hours\n\nText.\n" []))))
      (expect (plist-get (car (plist-get result :pending)) :property)
              :to-equal "CANVAS_URL")))

  (it "reads an empty id property as no id, as the sync does"
    (let* ((result (car (test-org-canvas-294--feature-diff
                         "rubrics" 'org-canvas-rubrics-file
                         "* R6 Rubric\n:PROPERTIES:\n:CANVAS_ID:\n:END:\n" []))))
      (expect (plist-get result :divergences) :to-be nil)
      (expect (length (plist-get result :pending)) :to-equal 1)))

  (it "does not call a files.org folder heading a create, and pairs a file heading's link"
    (let* ((result (car (test-org-canvas-294--feature-diff
                         "files" 'org-canvas-files-file
                         (concat "* Readings\n"
                                 "** [[file:content/week6.pdf][week6.pdf]]\n"
                                 "** [[file:content/syllabus.pdf][syllabus.pdf]]\n")
                         [((id . 31) (display_name . "syllabus.pdf"))])))
           (pending (plist-get result :pending)))
      (expect (mapcar (lambda (e) (plist-get e :title)) pending)
              :to-equal '("week6.pdf"))
      (expect (plist-get (car (plist-get result :extra)) :kind) :to-equal 'unclaimed))))

(describe "org-canvas-diff and pending creates (issue #294)"
  (it "leaves pending creates out of the count the batch exit reads"
    (let (code)
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                  ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                  ((symbol-function 'org-canvas--diff-feature)
                   (lambda (feature)
                     (list :name (plist-get feature :name)
                           :pending '((:kind pending :title "New" :property "CANVAS_ID")))))
                  ((symbol-function 'kill-emacs) (lambda (c) (setq code c))))
          (with-output-to-string
            (expect (org-canvas-diff) :to-equal 0)
            (org-canvas-diff-batch))))
      (expect code :to-equal 0))))

(describe "org-canvas--diff-feature with a declared document processor"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (it "counts an unmet declaration as drift and a surplus processor as a note"
    (let ((file (make-temp-file "diff-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Global Challenge Essay\n:PROPERTIES:\n:CANVAS_ID: 2497349\n"
                      ":WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
                      "* Reflection\n:PROPERTIES:\n:CANVAS_ID: 7\n"
                      ":DOCUMENT_PROCESSOR: Turnitin (asset processor 5)\n:END:\n"))
            (let ((org-canvas-assignments-file file))
              (with-org-canvas-test-config
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _)
                             '(((id . 2497349) (name . "Global Challenge Essay")
                                (asset_processors . []))
                               ((id . 7) (name . "Reflection")
                                (asset_processors . [((id . 5) (title . "Turnitin"))]))))))
                  (let* ((result (org-canvas--diff-feature
                                  (org-canvas--registry-find-feature "assignments")))
                         (divergences (plist-get result :divergences))
                         (notes (plist-get result :notes))
                         (report (org-canvas--diff-render (list result))))
                    (expect (length divergences) :to-equal 1)
                    (expect (car (car (plist-get (car divergences) :fields)))
                            :to-equal "WANT_DOCUMENT_PROCESSOR")
                    (expect (length notes) :to-equal 1)
                    (expect (plist-get (car notes) :title) :to-equal "Reflection")
                    (expect (org-canvas--diff-count (list result)) :to-equal 1)
                    (expect report :to-match "Assignments: 1 divergence(s), 1 note(s)")
                    (expect report :to-match "CHANGED   Global Challenge Essay")
                    (expect report :to-match
                            "WANT_DOCUMENT_PROCESSOR org: Turnitin")
                    (expect report :to-match
                            "NOTE      Reflection (DOCUMENT_PROCESSOR on Canvas is Turnitin (asset processor 5); no WANT_DOCUMENT_PROCESSOR declares it")
                    (expect report :to-match "1 divergence(s) found"))))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "renders a feature holding only notes and still reports no drift"
    (with-org-canvas-test-config
      (let ((report (org-canvas--diff-render
                     '((:name "Assignments"
                        :notes ((:kind note :title "Reflection" :id "7"
                                 :property "WANT_DOCUMENT_PROCESSOR"
                                 :observed "DOCUMENT_PROCESSOR"
                                 :remote "Turnitin")))))))
        (expect report :to-match "Assignments: 0 divergence(s), 1 note(s)")
        (expect report :to-match "NOTE      Reflection")
        (expect report :to-match "No drift"))))

  (it "finds a NOTE row's heading by its id"
    (let ((file (make-temp-file "diff-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Other\n* Reflection\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n"))
            (let ((org-canvas-assignments-file file))
              (let ((where (org-canvas--diff-heading-position
                            (org-canvas--registry-find-feature "assignments")
                            '(:kind note :title "Reflection" :id "7"))))
                (expect (car where) :to-equal file)
                (expect (cdr where) :to-be-greater-than 1))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

(describe "org-canvas--diff-features (issue #313)"
  (it "adds the New Quizzes pull entry after the feature registry"
    (let ((features (org-canvas--diff-features)))
      (expect (length features)
              :to-equal (1+ (length org-canvas--feature-registry)))
      (expect (plist-get (car (last features)) :name) :to-equal "New Quizzes")))

  (it "leaves out a pull-only entry that does not ask for the report"
    (let ((org-canvas--pull-feature-registry
           (list (list :name "Side" :file-var 'x))))
      (expect (length (org-canvas--diff-features))
              :to-equal (length org-canvas--feature-registry))))

  (it "keeps New Quizzes out of the registry the orphan scan and prune read"
    (expect (org-canvas--registry-find-feature "New Quizzes") :to-be nil))

  (it "finds either kind of entry by any spelling of its name"
    (expect (plist-get (org-canvas--diff-find-feature "new-quizzes") :id-property)
            :to-equal "CANVAS_ASSIGNMENT_ID")
    (expect (plist-get (org-canvas--diff-find-feature "assignments") :name)
            :to-equal "Assignments")
    (expect (org-canvas--diff-find-feature "Nonexistent") :to-be nil)))

(describe "org-canvas--diff-feature on New Quizzes (issue #313)"
  (it "lists the quiz service and reports CHANGED, MISSING, EXTRA and PENDING"
    (test-nq-313--with-file test-nq-313--file
      (with-org-canvas-test-config
        (let ((urls nil))
          (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                     (lambda (_method url &rest _)
                       (push url urls)
                       test-nq-313--remote))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (&rest _) (error "The report must not write"))))
            (let* ((result (org-canvas--diff-feature
                            (org-canvas--diff-find-feature "New Quizzes")))
                   (divergences (plist-get result :divergences))
                   (changed (nth 0 divergences))
                   (missing (nth 1 divergences))
                   (changed-kind (plist-get changed :kind))
                   (missing-kind (plist-get missing :kind))
                   (extra (plist-get result :extra))
                   (pending (plist-get result :pending)))
              ;; The quiz list, then Midterm's items (issue #322).
              (expect (reverse urls) :to-equal
                      (list (org-canvas--new-quiz-api-endpoint "quizzes")
                            (org-canvas--new-quiz-api-endpoint "quizzes/41/items")))
              (expect (plist-get result :error) :to-be nil)
              (expect changed-kind :to-be 'modified)
              (expect (plist-get changed :title) :to-equal "Midterm")
              (expect (plist-get changed :fields)
                      :to-equal '(("TIME_LIMIT" "30" "45")
                                  ("INSTRUCTIONS" "Read carefully." "Read slowly.")))
              (expect missing-kind :to-be 'missing)
              (expect (plist-get missing :id) :to-equal "42")
              (expect (mapcar (lambda (e) (plist-get e :id)) extra)
                      :to-equal '("43"))
              (expect (mapcar (lambda (e) (plist-get e :title)) pending)
                      :to-equal '("Draft"))
              (expect (plist-get (car pending) :property)
                      :to-equal "CANVAS_ASSIGNMENT_ID")))))))

  (it "agrees when the instructions and every reported setting match"
    (test-nq-313--with-file test-nq-313--file
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _)
                     ;; Only `id', as older replies carry it.
                     '(((id . "41") (title . "Midterm")
                        (instructions . "<p>Read carefully.</p>")
                        (quiz_settings
                         (has_time_limit . t)
                         (session_time_limit_in_seconds . 1800)
                         (multiple_attempts (score_to_keep . "highest"))))
                       ((id . "42") (title . "Final") (instructions . :null))))))
          (let ((result (org-canvas--diff-feature
                         (org-canvas--diff-find-feature "New Quizzes"))))
            (expect (plist-get result :divergences) :to-be nil)
            (expect (plist-get result :extra) :to-be nil))))))

  (it "pairs a web-UI quiz with an unstamped heading of its title"
    (test-nq-313--with-file "* Web Quiz\n"
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) test-nq-313--remote)))
          (let* ((result (org-canvas--diff-feature
                          (org-canvas--diff-find-feature "New Quizzes")))
                 (web (cl-find "43" (plist-get result :extra)
                               :key (lambda (e) (plist-get e :id)) :test #'equal))
                 (web-kind (plist-get web :kind)))
            (expect web-kind :to-be 'unclaimed)
            (expect (plist-get web :property) :to-equal "CANVAS_ASSIGNMENT_ID")
            (expect (plist-get result :pending) :to-be nil)))))))

(describe "New Quiz settings in the drift report (issue #321)"
  (it "compares every setting a heading sets against quiz_settings"
    (test-nq-313--with-file
        (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n"
                ":TIME_LIMIT: 30\n:SHUFFLE_ANSWERS: true\n"
                ":ONE_AT_A_TIME: false\n"
                ":ALLOWED_ATTEMPTS: 2\n:SCORING_POLICY: keep_latest\n:END:\n")
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method url &rest _)
                     (unless (string-match-p "items" url)
                       '(((id . "41") (title . "Midterm")
                          (quiz_settings
                           (has_time_limit . :json-false)
                           (session_time_limit_in_seconds . 0)
                           (shuffle_answers . :json-false)
                           (one_at_a_time_type . "question")
                           (multiple_attempts
                            (multiple_attempts_enabled . t)
                            (attempt_limit . :json-false)
                            (score_to_keep . "highest")))))))))
          (let* ((result (org-canvas--diff-feature
                          (org-canvas--diff-find-feature "New Quizzes")))
                 (changed (car (plist-get result :divergences))))
            (expect (plist-get changed :fields)
                    :to-equal '(("TIME_LIMIT" "30" "0")
                                ("SHUFFLE_ANSWERS" "true" "false")
                                ("ONE_AT_A_TIME" "false" "true")
                                ("ALLOWED_ATTEMPTS" "2" "-1")
                                ("SCORING_POLICY" "keep_latest"
                                 "keep_highest"))))))))

  (it "compares no setting when the reply has no quiz_settings"
    (test-nq-313--with-file
        (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n"
                ":TIME_LIMIT: 30\n:ALLOWED_ATTEMPTS: 2\n:END:\n")
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method url &rest _)
                     (unless (string-match-p "items" url)
                       '(((id . "41") (title . "Midterm")
                          (time_limit . 45) (allowed_attempts . 5)))))))
          (let ((result (org-canvas--diff-feature
                         (org-canvas--diff-find-feature "New Quizzes"))))
            (expect (plist-get result :divergences) :to-be nil))))))

  (it "reads a limit in seconds back as minutes"
    (expect (org-canvas--new-quiz-remote-time-limit
             '((quiz_settings (has_time_limit . t)
                              (session_time_limit_in_seconds . 1380))))
            :to-equal 23)
    (expect (org-canvas--new-quiz-remote-time-limit
             '((quiz_settings (has_time_limit . t)
                              (session_time_limit_in_seconds . 90))))
            :to-equal 1.5)))

(describe "a MISSING row a pull already marked (issue #392)"
  (it "carries the mark from the heading to the row it prints"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_DELETED: [2026-09-28 Mon 10:00]
:END:
"
     (org-back-to-heading)
     (let* ((entry (append (org-canvas--diff-local-entry "CANVAS_ID")
                           (list :pom (point-marker))))
            (row (org-canvas--diff-entry entry (make-hash-table :test 'equal) nil)))
       (with-temp-buffer
         (org-canvas--diff-insert-entry row)
         (expect (buffer-string) :to-match
                 "is not in this course; a pull marked it CANVAS_DELETED \\[2026-09-28 Mon 10:00\\])"))))))

;;; org-canvas-diff-test.el ends here
