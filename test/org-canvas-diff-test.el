;;; org-canvas-diff-test.el --- Tests for the drift report -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Covers `org-canvas-diff' (issue #51): the read-only comparison of a
;; course against the Org files that describe it.

;;; Code:

(require 'buttercup)
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

(describe "org-canvas--diff-remote-field"
  (it "prefers an explicit api-key"
    (expect (org-canvas--diff-remote-field
             '(:data-key :published :api-key "workflow_state")
             '((workflow_state . "active") (published . t)))
            :to-equal "active"))

  (it "falls back to the data-key, which names the Canvas field"
    (expect (org-canvas--diff-remote-field
             '(:data-key :points_possible)
             '((points_possible . 10)))
            :to-equal 10))

  (it "asks the spec's remote-fn when the value is not under a flat key"
    ;; Issues #61 and #62: a file's publish state lives in `locked', a
    ;; group's drop rules under `rules'.
    (expect (org-canvas--diff-remote-field
             '(:data-key :drop_lowest
               :remote-fn org-canvas--assignment-group-remote-drop-lowest)
             '((group_weight . 15.0) (rules (drop_lowest . 1))))
            :to-equal 1))

  (it "prefers the remote-fn over a flat key of the same name"
    (expect (org-canvas--diff-remote-field
             '(:data-key :published :remote-fn org-canvas--file-remote-published)
             '((published . :json-false) (locked . :json-false)))
            :to-be t)))

(describe "org-canvas--diff-remote-list"
  (it "splits a comma-separated string Canvas sent instead of an array"
    ;; Issue #63: `append' on a string yields character codes, so
    ;; "teachers" was reported as 116,101,97,...
    (expect (org-canvas--diff-remote-list "teachers") :to-equal '("teachers"))
    (expect (org-canvas--diff-remote-list "teachers,students")
            :to-equal '("teachers" "students")))

  (it "trims whitespace around the separators"
    (expect (org-canvas--diff-remote-list "teachers, students")
            :to-equal '("teachers" "students")))

  (it "passes an array through as strings"
    (expect (org-canvas--diff-remote-list ["teachers" "students"])
            :to-equal '("teachers" "students"))
    (expect (org-canvas--diff-remote-list '(online_upload on_paper))
            :to-equal '("online_upload" "on_paper")))

  (it "returns nothing for an absent value"
    (expect (org-canvas--diff-remote-list nil) :to-be nil)))

(describe "org-canvas--diff-values-equal-p"
  (it "compares booleans across Org strings and JSON false"
    (expect (org-canvas--diff-values-equal-p 'boolean "true" t) :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p 'boolean "false" :json-false)
            :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p 'boolean "false" t) :to-be nil)
    (expect (org-canvas--diff-values-equal-p 'boolean "true" :json-false)
            :to-be nil))

  (it "compares numbers regardless of integer or float spelling"
    (expect (org-canvas--diff-values-equal-p 'number "10" 10) :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p 'number "10" 10.0) :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p 'number "10" 5) :to-be nil))

  (it "reports a number against a null remote as different"
    (expect (org-canvas--diff-values-equal-p 'number "10" :null) :to-be nil))

  (it "compares timestamps as instants, not strings"
    (let ((org-ts "<2026-08-20 Thu 23:59>"))
      (expect (org-canvas--diff-values-equal-p
               'timestamp org-ts (org-canvas-org-parse-timestamp org-ts))
              :to-be-truthy)
      (expect (org-canvas--diff-values-equal-p
               'timestamp org-ts "2026-01-01T00:00:00Z")
              :to-be nil)))

  (it "compares csv enums as sets"
    (expect (org-canvas--diff-values-equal-p
             'csv-enum "online_upload,online_text_entry"
             ["online_text_entry" "online_upload"])
            :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p
             'csv-enum "online_upload" ["on_paper"])
            :to-be nil))

  (it "compares a csv enum Canvas sent as one string, not an array"
    ;; Issue #63: pages return editing_roles as "teachers".
    (expect (org-canvas--diff-values-equal-p 'csv-enum "teachers" "teachers")
            :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p
             'csv-enum "teachers,students" "students,teachers")
            :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p 'csv-enum "teachers" "students")
            :to-be nil))

  (it "compares plain strings"
    (expect (org-canvas--diff-values-equal-p 'string "letter_grade" "letter_grade")
            :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p 'string "letter_grade" "points")
            :to-be nil)))

(describe "org-canvas--diff-compare-fields"
  (let ((specs '((:org-prop "POINTS" :data-key :points_possible :type number)
                 (:org-prop "PUBLISHED" :data-key :published :type boolean)
                 (:org-prop "GROUP" :data-key :assignment_group_id :type link))))

    (it "reports the differing values"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:POINTS: 10
:PUBLISHED: false
:END:
"
       (org-back-to-heading)
       (let ((diffs (org-canvas--diff-compare-fields
                     specs (point)
                     '((points_possible . 25) (published . t)))))
         (expect diffs :to-equal '(("POINTS" "10" "25")
                                   ("PUBLISHED" "false" "true"))))))

    (it "says nothing when the declared properties agree"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:POINTS: 10
:PUBLISHED: false
:END:
"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point)
                '((points_possible . 10) (published . :json-false)))
               :to-be nil)))

    (it "ignores properties the Org file does not declare"
      ;; Absence is not a value: each module applies its own parse default,
      ;; so treating it as one would invent differences no sync would act on.
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:POINTS: 10
:END:
"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point) '((points_possible . 10) (published . t)))
               :to-be nil)))

    (it "skips link properties, whose two sides are not comparable"
      (with-temp-org-buffer
       "* Lab 1
:PROPERTIES:
:GROUP: [[file:assignment-groups.org::*Labs][Labs]]
:END:
"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point) '((assignment_group_id . 77)))
               :to-be nil)))))

;;; The three false positives of issues #61, #62 and #63, each driven
;;; through the module's own registered specs rather than a hand-written
;;; one — the bugs were in what the specs said, not in the comparison.

(defun org-canvas-diff-test--specs (feature)
  "Return the registered property specs for FEATURE."
  (plist-get (org-canvas--diff-find-properties feature) :properties))

(describe "org-canvas--diff-compare-fields against real registry specs"
  (it "does not call a published file unpublished (issue #61)"
    (with-temp-org-buffer
     "* syllabus.pdf
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-compare-fields
              (org-canvas-diff-test--specs "Files") (point)
              '((id . 42) (display_name . "syllabus.pdf")
                (locked . :json-false) (hidden . :json-false)))
             :to-be nil)))

  (it "still reports a file that really is locked on Canvas"
    (with-temp-org-buffer
     "* syllabus.pdf
:PROPERTIES:
:PUBLISHED: true
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-compare-fields
              (org-canvas-diff-test--specs "Files") (point)
              '((id . 42) (locked . t)))
             :to-equal '(("PUBLISHED" "true" "false")))))

  (it "reads drop rules out of the nested rules object (issue #62)"
    (with-temp-org-buffer
     "* Quizzes
:PROPERTIES:
:WEIGHT: 15
:DROP_LOWEST: 1
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-compare-fields
              (org-canvas-diff-test--specs "Assignment Groups") (point)
              '((id . 697530) (name . "Quizzes") (group_weight . 15.0)
                (rules (drop_lowest . 1))))
             :to-be nil)))

  (it "still reports a drop rule Canvas does not hold"
    (with-temp-org-buffer
     "* Quizzes
:PROPERTIES:
:WEIGHT: 15
:DROP_LOWEST: 1
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-compare-fields
              (org-canvas-diff-test--specs "Assignment Groups") (point)
              '((id . 697530) (group_weight . 15.0) (rules)))
             :to-equal '(("DROP_LOWEST" "1" "(unset)")))))

  (it "compares editing roles Canvas sent as a string (issue #63)"
    (with-temp-org-buffer
     "* Course Home
:PROPERTIES:
:EDITING_ROLES: teachers
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-compare-fields
              (org-canvas-diff-test--specs "Pages") (point)
              '((url . "course-home") (editing_roles . "teachers")))
             :to-be nil)))

  (it "prints readable roles when they really differ"
    (with-temp-org-buffer
     "* Course Home
:PROPERTIES:
:EDITING_ROLES: teachers
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--diff-compare-fields
              (org-canvas-diff-test--specs "Pages") (point)
              '((url . "course-home") (editing_roles . "students")))
             :to-equal '(("EDITING_ROLES" "teachers" "students"))))))

(describe "org-canvas--file-remote-published"
  (it "reads a file's publish state from locked, the field Canvas returns"
    (expect (org-canvas--file-remote-published '((locked . t))) :to-be nil)
    (expect (org-canvas--file-remote-published '((locked . :json-false)))
            :to-be t))

  (it "treats a file with no locked field as published"
    (expect (org-canvas--file-remote-published '((id . 42))) :to-be t)))

(describe "org-canvas--assignment-group-remote-rule"
  (it "reaches into the nested rules object"
    (expect (org-canvas--assignment-group-remote-drop-lowest
             '((rules (drop_lowest . 1))))
            :to-equal 1)
    (expect (org-canvas--assignment-group-remote-drop-highest
             '((rules (drop_highest . 2))))
            :to-equal 2))

  (it "returns nil for a group Canvas holds no rules for"
    (expect (org-canvas--assignment-group-remote-drop-lowest
             '((group_weight . 15.0)))
            :to-be nil)
    (expect (org-canvas--assignment-group-remote-drop-lowest '((rules)))
            :to-be nil))

  (it "returns nil for a rule that is not the one asked for"
    (expect (org-canvas--assignment-group-remote-drop-highest
             '((rules (drop_lowest . 1))))
            :to-be nil)))

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
                :to-equal (length org-canvas--feature-registry))))))

(describe "org-canvas--diff-values-equal-p string-shaped numbers"
  (it "compares a Canvas number returned as a string"
    (expect (org-canvas--diff-values-equal-p 'number "10" "10") :to-be-truthy)
    (expect (org-canvas--diff-values-equal-p 'number "10" "5") :to-be nil)))

(describe "org-canvas--diff-format-remote"
  (it "prints a csv enum as the comma-separated form the Org file uses"
    (expect (org-canvas--diff-format-remote 'csv-enum ["online_upload" "on_paper"])
            :to-equal "online_upload,on_paper"))

  (it "prints a string-shaped csv enum as itself, not as character codes"
    ;; Issue #63's visible symptom: canvas: 116,101,97,99,104,101,114,115
    (expect (org-canvas--diff-format-remote 'csv-enum "teachers")
            :to-equal "teachers")
    (expect (org-canvas--diff-format-remote 'csv-enum "teachers,students")
            :to-equal "teachers,students"))

  (it "prints an unset value plainly"
    (expect (org-canvas--diff-format-remote 'string :null) :to-equal "(unset)"))

  (it "prints booleans as the Org file spells them"
    (expect (org-canvas--diff-format-remote 'boolean :json-false) :to-equal "false")
    (expect (org-canvas--diff-format-remote 'boolean t) :to-equal "true")))

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

;;;; Issue #83: the body is compared, as text

(describe "org-canvas--diff-html-to-text (issue #83)"
  (it "drops tags, decodes entities and collapses whitespace"
    (expect (org-canvas--diff-html-to-text
             "<link rel=\"stylesheet\" href=\"x.css\"><p>Read&nbsp;the   <b>text</b> &amp;\n\nreply&#39;s &#x2019;quote&#8217;</p>")
            :to-equal "Read the text & reply's ’quote’"))

  (it "drops script and style bodies"
    (expect (org-canvas--diff-html-to-text
             "<style>p{color:red}</style><p>Hi</p><script>x()</script>")
            :to-equal "Hi"))

  (it "reads a non-string as empty"
    (expect (org-canvas--diff-html-to-text nil) :to-equal "")
    (expect (org-canvas--diff-html-to-text :null) :to-equal ""))

  (it "keeps an unknown entity as written"
    (expect (org-canvas--diff-html-to-text "a &bogus; b") :to-equal "a &bogus; b")))

(describe "org-canvas--diff-text-excerpts"
  (it "cuts both sides around the first difference"
    (let* ((a (concat (make-string 30 ?x) "same then LOCAL words follow here"))
           (b (concat (make-string 30 ?x) "same then REMOTE words follow here"))
           (ex (org-canvas--diff-text-excerpts a b 20)))
      (expect (nth 0 ex) :to-equal "…same then LOCAL word…")
      (expect (nth 1 ex) :to-equal "…same then REMOTE wor…")))

  (it "marks an empty side"
    (expect (org-canvas--diff-text-excerpts "" "text") :to-equal '("(empty)" "text")))

  (it "shows a short pair whole"
    (expect (org-canvas--diff-text-excerpts "abc" "abd") :to-equal '("abc" "abd"))))

(describe "org-canvas--diff-local-body-html"
  (it "exports the heading offline, without resolving images"
    (with-temp-org-buffer "* Lab 1\n\nRead the *book*.\n"
      (org-back-to-heading)
      (let ((m (point-marker)) (resolved nil))
        (cl-letf (((symbol-function 'org-canvas--resolve-image-links)
                   (lambda (_) (setq resolved t))))
          ;; From another buffer: the marker must carry its own.
          (with-temp-buffer
            (expect (org-canvas--diff-local-body-html m nil) :to-match "<b>book</b>")))
        (expect resolved :to-be nil))))

  (it "uses the module's extractor when given"
    (with-temp-org-buffer "* Lab 1\n"
      (org-back-to-heading)
      (expect (org-canvas--diff-local-body-html (point-marker) (lambda () "<p>custom</p>"))
              :to-equal "<p>custom</p>")))

  (it "warns and returns nil when the export fails"
    (let ((warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
        (with-temp-org-buffer "* Lab 1\n"
          (org-back-to-heading)
          (expect (org-canvas--diff-local-body-html
                   (point-marker) (lambda () (error "boom")))
                  :to-be nil)))
      (expect (car warnings) :to-match "Could not export the body.*boom"))))

(describe "org-canvas--diff-compare-body (issue #83)"
  (it "is nil for a feature that declares no body"
    (expect (org-canvas--diff-compare-body '(:properties ()) nil '((description . "x")))
            :to-be nil))

  (it "is nil when the item does not carry the field"
    (with-temp-org-buffer "* Lab 1\n\nText.\n"
      (org-back-to-heading)
      (expect (org-canvas--diff-compare-body
               '(:body-api-key "description") (point-marker) '((id . 1)))
              :to-be nil)))

  (it "agrees across markup Canvas rewrote"
    (with-temp-org-buffer "* Lab 1\n\nRead the *book* & reply.\n"
      (org-back-to-heading)
      (expect (org-canvas--diff-compare-body
               '(:body-api-key "description") (point-marker)
               '((description . "<link rel=\"stylesheet\" href=\"x\"><p>Read the <strong>book</strong> &amp;\nreply.</p>")))
              :to-equal '(t))))

  (it "reports a rewritten body with excerpts"
    (with-temp-org-buffer "* Lab 1\n\nWrite about chapter one.\n"
      (org-back-to-heading)
      (let ((result (org-canvas--diff-compare-body
                     '(:body-api-key "description") (point-marker)
                     '((description . "<p>Write about chapter two.</p>")))))
        (expect (car result) :to-be t)
        (expect (nth 0 (cdr result)) :to-equal "DESCRIPTION")
        (expect (nth 1 (cdr result)) :to-match "chapter one")
        (expect (nth 2 (cdr result)) :to-match "chapter two"))))

  (it "treats a null remote body as empty"
    (with-temp-org-buffer "* Lab 1\n"
      (org-back-to-heading)
      (expect (org-canvas--diff-compare-body
               '(:body-api-key "body") (point-marker) '((body . :null)))
              :to-equal '(t))))

  (it "is nil, not drift, when the local export fails"
    (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
      (with-temp-org-buffer "* Lab 1\n"
        (org-back-to-heading)
        (expect (org-canvas--diff-compare-body
                 (list :body-api-key "body" :body-fn (lambda () (error "boom")))
                 (point-marker) '((body . "x")))
                :to-be nil)))))

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
              :to-equal "  UNCLAIMED R11: The Ethics Email (Canvas id 2563810 has this title and no heading claims it; stamp CANVAS_ID or rename)\n"))))

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

(describe "body registry declarations (issue #83)"
  (it "name the Canvas field for every module that pushes a body"
    (dolist (pair '(("assignments" . "description") ("pages" . "body")
                    ("announcements" . "message") ("discussions" . "message")
                    ("quizzes" . "description")))
      (expect (plist-get (gethash (car pair) org-canvas--property-registry) :body-api-key)
              :to-equal (cdr pair))))

  (it "leave modules without a body alone"
    (expect (plist-get (gethash "modules" org-canvas--property-registry) :body-api-key)
            :to-be nil)))

(provide 'org-canvas-diff-test)
;;; org-canvas-diff-test.el ends here
