;;; org-canvas-diff-test.el --- Tests for the drift report -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Covers `org-canvas-diff' (issue #51): the read-only comparison of a
;; course against the Org files that describe it.

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

  (it "compares timestamps at minute precision, since Org has no seconds (issue #176)"
    ;; The issue's exact pair: a web-UI end-of-day deadline stored as
    ;; 03:59:59Z, pulled as <2026-09-09 Wed 23:59> in the course's
    ;; Eastern zone.  The POSIX rule stands in for America/New_York so
    ;; the test does not depend on tzdata being installed.
    (let ((org-canvas-time-zone "EST5EDT,M3.2.0,M11.1.0"))
      (expect (org-canvas--diff-values-equal-p
               'timestamp "<2026-09-09 Wed 23:59>" "2026-09-10T03:59:59Z")
              :to-be-truthy))
    (let ((org-canvas-time-zone "UTC"))
      (expect (org-canvas--diff-values-equal-p
               'timestamp "<2026-09-09 Wed 23:59>" "2026-09-09T23:59:59Z")
              :to-be-truthy)
      (expect (org-canvas--diff-values-equal-p
               'timestamp "<2026-09-09 Wed 23:59>" "2026-09-09T23:59:01Z")
              :to-be-truthy)))

  (it "still reports a timestamp in a different minute (issue #176)"
    ;; Truncation, not a 60-second tolerance: one second before the
    ;; minute and the first second of the next are both drift.
    (let ((org-canvas-time-zone "UTC"))
      (expect (org-canvas--diff-values-equal-p
               'timestamp "<2026-09-09 Wed 23:59>" "2026-09-09T23:58:59Z")
              :to-be nil)
      (expect (org-canvas--diff-values-equal-p
               'timestamp "<2026-09-09 Wed 23:59>" "2026-09-10T00:00:00Z")
              :to-be nil)))

  (it "reports a timestamp against an absent or unparsable remote as different"
    (let ((org-canvas-time-zone "UTC"))
      (expect (org-canvas--diff-values-equal-p
               'timestamp "<2026-09-09 Wed 23:59>" :null)
              :to-be nil)
      (expect (org-canvas--diff-values-equal-p
               'timestamp "<2026-09-09 Wed 23:59>" 12345)
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
                :to-equal (length (org-canvas--diff-features)))))))

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
              :to-equal "  UNCLAIMED R11: The Ethics Email (Canvas id 2563810 has this title and no heading claims it; adopt it with org-canvas-adopt-at-point, which stamps CANVAS_ID, or rename)\n"))))

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

(describe "org-canvas--diff-compare-fields :compare-p (issue #93)"
  (it "skips a property whose predicate says there is no opinion to compare"
    (with-temp-org-buffer "* E\n:PROPERTIES:\n:ALL_DAY: true\n:END:\n"
      (org-back-to-heading)
      (let ((specs `((:org-prop "ALL_DAY" :data-key :all_day :type boolean
                      :compare-p ,(lambda (_pom _item) nil)))))
        (expect (org-canvas--diff-compare-fields specs (point)
                                                 '((all_day . :json-false)))
                :to-be nil))))

  (it "compares as before when the predicate agrees"
    (with-temp-org-buffer "* E\n:PROPERTIES:\n:ALL_DAY: true\n:END:\n"
      (org-back-to-heading)
      (let ((specs `((:org-prop "ALL_DAY" :data-key :all_day :type boolean
                      :compare-p ,(lambda (_pom _item) t)))))
        (expect (org-canvas--diff-compare-fields specs (point)
                                                 '((all_day . :json-false)))
                :to-equal '(("ALL_DAY" "true" "false")))))))

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

(describe "an excluded feature still feeds the reference scan (issue #111)"
  (it "reads the bodies of an excluded body feature for the media they embed"
    (with-org-canvas-test-config
      (let ((requested nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method url &rest _args)
                     (push url requested)
                     '(((id . 1812713) (title . "Week 1")
                        (message . "<a class=\"instructure_file_link\" href=\"/courses/1/files/31505590/download\">flyer</a>"))))))
          (let ((result (org-canvas--diff-excluded-result
                         (org-canvas--registry-find-feature "announcements"))))
            (expect (plist-get result :excluded) :to-be t)
            (expect (plist-get result :referenced-files) :to-equal '("31505590"))
            (expect (plist-get result :references-scanned) :to-be t)
            (expect (length requested) :to-equal 1))))))

  (it "spends no request on an excluded feature with no body to read"
    (with-org-canvas-test-config
      (let ((called nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (setq called t) nil)))
          (let ((result (org-canvas--diff-excluded-result
                         (org-canvas--registry-find-feature "files"))))
            (expect called :to-be nil)
            (expect (plist-get result :references-scanned) :to-be nil)
            (expect (plist-get result :referenced-files) :to-be nil))))))

  (it "spends no request when the reference scan is off"
    (with-org-canvas-test-config
      (let ((called nil)
            (org-canvas-diff-scan-references nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (setq called t) nil)))
          (expect (plist-get (org-canvas--diff-excluded-result
                              (org-canvas--registry-find-feature "announcements"))
                             :references-scanned)
                  :to-be nil)
          (expect called :to-be nil)))))

  (it "reads as no references when the list request fails"
    (with-org-canvas-test-config
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (signal 'error '("HTTP 500"))))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) warned))))
          (let ((result (org-canvas--diff-excluded-result
                         (org-canvas--registry-find-feature "announcements"))))
            (expect (plist-get result :referenced-files) :to-be nil)
            (expect (plist-get result :references-scanned) :to-be t)
            (expect (car warned) :to-match "for file references"))))))

  (it "acknowledges a file only an excluded feature's body embeds"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas--diff-syllabus-references)
                 (lambda () nil)))
        (let* ((results (list (list :name "Files"
                                    :extra (list (list :kind 'extra :id "31505590")
                                                 (list :kind 'extra :id "999")))
                              (list :name "Announcements" :excluded t
                                    :references-scanned t
                                    :referenced-files '("31505590"))))
               (files (car (org-canvas--diff-apply-references results))))
          (expect (plist-get files :referenced) :to-equal 1)
          (expect (mapcar (lambda (e) (plist-get e :id))
                          (plist-get files :extra))
                  :to-equal '("999"))))))

  (it "says on the exclusion line that the bodies were read anyway"
    (with-org-canvas-test-config
      (expect (org-canvas--diff-render
               '((:name "Announcements" :excluded t :references-scanned t)))
              :to-match "not checked (org-canvas-diff-excluded-features); bodies read for referenced media only"))))

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

;;;; Acting on the report (issue #103)

(describe "org-canvas-diff-mode (issue #103)"
  (defun test-org-canvas--diff-report-buffer (results)
    "Render RESULTS the way `org-canvas-diff' does and return the buffer."
    (with-org-canvas-test-config
      (let ((noninteractive nil))
        (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                  ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                  ;; The referenced-media scan (#102) would otherwise
                  ;; read the syllabus, one request these specs count.
                  ((symbol-function 'org-canvas--diff-syllabus-references)
                   (lambda () nil))
                  ((symbol-function 'org-canvas--diff-feature)
                   (lambda (feature)
                     (or (cl-find (plist-get feature :name) results
                                  :key (lambda (r) (plist-get r :name))
                                  :test #'string=)
                         (list :name (plist-get feature :name))))))
          (org-canvas-diff)
          (get-buffer org-canvas--diff-buffer-name)))))

  (defun test-org-canvas--diff-goto-row (kind)
    "Put point on the first row of KIND in the current buffer."
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (let ((row (get-text-property (point) 'org-canvas-diff-row)))
          (if (and row (eq (plist-get (plist-get row :entry) :kind) kind))
              (setq found t)
            (forward-line 1))))
      (unless found (error "No %s row" kind))))

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
  (defun test-org-canvas--diff-browse (results kind &optional edit)
    "Browse the first KIND row of a report of RESULTS; return the URL opened.
EDIT is passed on.  A `user-error' comes back as (error MESSAGE)."
    (let ((opened nil)
          (org-canvas-base-url "https://canvas.test")
          (org-canvas-course-id "42"))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (with-current-buffer (test-org-canvas--diff-report-buffer results)
          (test-org-canvas--diff-goto-row kind)
          (condition-case e
              (progn (org-canvas-diff-browse edit) opened)
            (user-error (list 'error (error-message-string e))))))))

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

(defvar test-org-canvas--diff-delete-requests nil
  "Requests the delete stub recorded, newest first, as (METHOD URL ARGS).")

(defun test-org-canvas--diff-delete-stub (method url &rest args)
  "Record METHOD, URL and ARGS; answer a GET with a small object."
  (push (list method url args) test-org-canvas--diff-delete-requests)
  (when (eq method 'GET) '((id . 1) (title . "Snapshot me"))))

(defun test-org-canvas--diff-deletes ()
  "Return the DELETE requests the stub recorded, oldest first."
  (reverse (cl-remove-if-not (lambda (r) (eq (car r) 'DELETE))
                             test-org-canvas--diff-delete-requests)))

(defmacro test-org-canvas--with-snapshot-dir (&rest body)
  "Run BODY with the delete snapshots going to a fresh temporary directory.
The directory is bound to `snapshot-dir' and removed afterwards."
  (declare (indent 0))
  `(let* ((snapshot-dir (make-temp-file "diff-snapshots-" t))
          (org-canvas-diff-delete-snapshot-directory snapshot-dir)
          (test-org-canvas--diff-delete-requests nil))
     (ignore snapshot-dir)
     (unwind-protect (progn ,@body)
       (delete-directory snapshot-dir t))))

(describe "org-canvas-diff-delete (issue #103)"
  (it "deletes the remote object through the feature's URL after confirming, and marks the row"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Files" :extra ((:kind extra :title "dup.png" :id "31505574")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-delete)
          (expect (thing-at-point 'line t) :to-match "DELETED   dup.png (id 31505574)")))
      (let ((deletes (test-org-canvas--diff-deletes)))
        (expect (length deletes) :to-equal 1)
        (expect (nth 1 (car deletes)) :to-match "files/31505574$"))))

  (it "sends the feature's delete body when it declares one"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Calendar Events" :extra ((:kind extra :title "Old" :id "5")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-delete)))
      (let ((delete (car (test-org-canvas--diff-deletes))))
        (expect (nth 1 delete) :to-match "calendar_events/5$")
        (expect (plist-get (nth 2 delete) :data)
                :to-equal '((cancel_reason . "Deleted by org-canvas"))))))

  (it "sends no DELETE and writes no snapshot when the confirmation is declined"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Files" :extra ((:kind extra :title "dup.png" :id "1")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-delete)
          (expect (thing-at-point 'line t) :to-match "EXTRA     dup.png")))
      (expect (test-org-canvas--diff-deletes) :to-be nil)
      (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil)))

  (it "refuses a row with no remote object of its own to delete"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :divergences ((:kind missing :title "Gone" :id "61")))))
      (test-org-canvas--diff-goto-row 'missing)
      (expect (org-canvas-diff-delete) :to-throw 'user-error))))

(describe "org-canvas-diff-pull (issue #103)"
  (it "runs the single-heading pull on the CHANGED row's heading and marks the row"
    (let ((file (make-temp-file "diff-pull-" nil ".org"))
          (pulled-in nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:END:\n"
                      "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"))
            (let ((org-canvas-assignments-file file))
              (cl-letf (((symbol-function 'org-canvas-pull-at-point)
                         (lambda () (setq pulled-in (cons (buffer-file-name)
                                                          (org-get-heading t t t t))))))
                (with-current-buffer (test-org-canvas--diff-report-buffer
                                      '((:name "Assignments"
                                         :divergences ((:kind modified :title "Lab 2" :id "61"
                                                        :remote-newer t
                                                        :updated "2026-08-25T00:00:00Z")))))
                  (test-org-canvas--diff-goto-row 'modified)
                  (org-canvas-diff-pull)
                  (expect (thing-at-point 'line t) :to-match "PULLED    Lab 2 (id 61)")))
              (expect (car pulled-in) :to-equal (file-truename file))
              (expect (cdr pulled-in) :to-equal "Lab 2")))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "refuses an EXTRA row of a feature that cannot pull into a new heading"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :extra ((:kind extra :title "Surprise" :id "99")))))
      (test-org-canvas--diff-goto-row 'extra)
      (expect (org-canvas-diff-pull) :to-throw 'user-error)))

  (it "refuses a row with no Canvas version to pull"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :divergences ((:kind missing :title "Gone" :id "61")))))
      (test-org-canvas--diff-goto-row 'missing)
      (expect (org-canvas-diff-pull) :to-throw 'user-error)))

  (it "pulls an EXTRA quiz into a new heading at the end of quizzes.org (issue #295)"
    (let ((file (make-temp-file "diff-pull-extra-" nil ".org"))
          (pulled nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Quiz 1\n:PROPERTIES:\n:CANVAS_ID: 5\n:END:\n"))
            (let ((org-canvas-quizzes-file file))
              (cl-letf (((symbol-function 'org-canvas--pull-at-point-1)
                         (lambda (feature id title)
                           (setq pulled (list (plist-get feature :name) id title
                                              (org-entry-get (point) "CANVAS_ID")
                                              (org-get-heading t t t t))))))
                (with-current-buffer (test-org-canvas--diff-report-buffer
                                      '((:name "Quizzes"
                                         :extra ((:kind extra :title "Survey" :id "77")))))
                  (test-org-canvas--diff-goto-row 'extra)
                  (org-canvas-diff-pull)
                  (expect (thing-at-point 'line t)
                          :to-match "PULLED    Survey (id 77, new heading)")))
              (expect pulled :to-equal '("Quizzes" "77" "Survey" "77" "Survey"))
              (with-current-buffer (find-buffer-visiting file)
                (expect (buffer-string) :to-match "^\\* Quiz 1\n\\(?:.*\n\\)*\\* Survey\n"))))
        (let ((buf (find-buffer-visiting file)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
        (delete-file file))))

  (it "sends nothing when the EXTRA pull is declined"
    (let ((file (make-temp-file "diff-pull-extra-no-" nil ".org")))
      (unwind-protect
          (let ((org-canvas-quizzes-file file))
            (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil))
                      ((symbol-function 'org-canvas--pull-at-point-1)
                       (lambda (&rest _) (error "Declined pull must not fetch"))))
              (with-current-buffer (test-org-canvas--diff-report-buffer
                                    '((:name "Quizzes"
                                       :extra ((:kind extra :title "Survey" :id "77")))))
                (test-org-canvas--diff-goto-row 'extra)
                (org-canvas-diff-pull)
                (expect (thing-at-point 'line t) :to-match "EXTRA     Survey"))))
        (let ((buf (find-buffer-visiting file)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
        (delete-file file)))))

;;;; Stamp adoption (issue #257)

(defmacro test-org-canvas-257--with-assignments-file (content &rest body)
  "Run BODY with `org-canvas-assignments-file' bound to a file holding CONTENT.
The file is deleted afterwards, its buffer killed."
  (declare (indent 1))
  `(let ((file (make-temp-file "diff-adopt-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (let ((org-canvas-assignments-file file))
             ,@body))
       (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
       (delete-file file))))

(defun test-org-canvas-257--property (file title property)
  "Return PROPERTY of the heading TITLE in FILE, read from disk."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (concat "^\\* " (regexp-quote title) "$"))
    (org-entry-get (point) property)))

(defconst test-org-canvas-257--file
  (concat "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:PAYLOAD_HASH: aaa\n:END:\n"
          "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 61\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:PAYLOAD_HASH: bbb\n:END:\n"
          "* Lab 3\n:PROPERTIES:\n:CANVAS_ID: 62\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:PAYLOAD_HASH: ccc\n:END:\n")
  "Three stamped assignment headings for the adoption specs.")

(describe "org-canvas--diff-adoptable-p (issue #257)"
  (it "accepts a CHANGED row Canvas holds newer with no compared property differing"
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer t :fields nil
               :updated "2026-09-11T20:00:00Z"))
            :to-be-truthy))

  (it "refuses a row where a compared property differs, one Canvas is not newer on, and other kinds"
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer t
               :fields (("POINTS" "10" "12")) :updated "2026-09-11T20:00:00Z"))
            :to-be nil)
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer nil :fields nil
               :updated "2026-09-11T20:00:00Z"))
            :to-be nil)
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer t :fields nil))
            :to-be nil)
    (expect (org-canvas--diff-adoptable-p '(:kind missing :title "Lab 2" :id "61"))
            :to-be nil)))

(describe "org-canvas-diff-adopt-stamp (issue #257)"
  (it "writes the Canvas timestamp into CANVAS_UPDATED_AT, keeps PAYLOAD_HASH, sends nothing, and marks the row"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-mock-api
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Assignments"
                                 :divergences ((:kind modified :title "Lab 2" :id "61"
                                                :remote-newer t :fields nil
                                                :updated "2026-09-11T20:00:00Z")))))
          (test-org-canvas--diff-goto-row 'modified)
          (org-canvas-diff-adopt-stamp)
          (expect (thing-at-point 'line t)
                  :to-match "ADOPTED   Lab 2 (CANVAS_UPDATED_AT set to 2026-09-11T20:00:00Z, nothing sent)")
          (expect (get-text-property (point) 'org-canvas-diff-row) :to-be-truthy))
        (expect (test-org-canvas-api-call-count) :to-equal 0))
      (expect (test-org-canvas-257--property file "Lab 2" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T20:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 2" "PAYLOAD_HASH") :to-equal "bbb")
      ;; The neighbours are untouched.
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 3" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")))

  (it "is what a on a CHANGED row does"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-current-buffer (test-org-canvas--diff-report-buffer
                            '((:name "Assignments"
                               :divergences ((:kind modified :title "Lab 3" :id "62"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T21:00:00Z")))))
        (test-org-canvas--diff-goto-row 'modified)
        (org-canvas-diff-acknowledge)
        (expect (thing-at-point 'line t) :to-match "ADOPTED   Lab 3"))
      (expect (test-org-canvas-257--property file "Lab 3" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T21:00:00Z")))

  (it "refuses a CHANGED row where a compared property differs and leaves the file alone"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-current-buffer (test-org-canvas--diff-report-buffer
                            '((:name "Assignments"
                               :divergences ((:kind modified :title "Lab 2" :id "61"
                                              :remote-newer t
                                              :fields (("POINTS" "10" "12"))
                                              :updated "2026-09-11T20:00:00Z")))))
        (test-org-canvas--diff-goto-row 'modified)
        (expect (org-canvas-diff-adopt-stamp) :to-throw 'user-error)
        (expect (org-canvas-diff-acknowledge) :to-throw 'user-error)
        (expect (thing-at-point 'line t) :to-match "CHANGED   Lab 2"))
      (expect (test-org-canvas-257--property file "Lab 2" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")))

  (it "refuses a row that is not CHANGED"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :extra ((:kind extra :title "Surprise" :id "99")))))
      (test-org-canvas--diff-goto-row 'extra)
      (expect (org-canvas-diff-adopt-stamp) :to-throw 'user-error)))

  (it "signals when no heading carries the row's id"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-current-buffer (test-org-canvas--diff-report-buffer
                            '((:name "Assignments"
                               :divergences ((:kind modified :title "Ghost" :id "999"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z")))))
        (test-org-canvas--diff-goto-row 'modified)
        (expect (org-canvas-diff-adopt-stamp) :to-throw 'user-error)))))

(describe "org-canvas-diff-adopt-stamps (issue #257)"
  (defun test-org-canvas-257--run (results &optional answer)
    "Run the batch adoption over RESULTS as if every feature reported them.
ANSWER `no' declines the confirmation, which otherwise says yes; the
prompt text and the report text come back as (PROMPT . REPORT)."
    (let ((prompt nil))
      (with-org-canvas-test-config
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                    ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                    ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                    ((symbol-function 'y-or-n-p)
                     (lambda (q) (setq prompt q) (not (eq answer 'no))))
                    ((symbol-function 'org-canvas--diff-feature)
                     (lambda (feature)
                       (or (cl-find (plist-get feature :name) results
                                    :key (lambda (r) (plist-get r :name))
                                    :test #'string=)
                           (list :name (plist-get feature :name))))))
            (org-canvas-diff-adopt-stamps))))
      (cons prompt (with-current-buffer org-canvas--diff-adopt-buffer-name (buffer-string)))))

  (it "adopts every qualifying row after confirming, counts the held-back ones, and sends nothing"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (let ((out nil))
        (with-mock-api
          (setq out (test-org-canvas-257--run
                     '((:name "Assignments"
                        :divergences ((:kind modified :title "Lab 1" :id "60"
                                       :remote-newer t :fields nil
                                       :updated "2026-09-11T20:00:00Z")
                                      (:kind modified :title "Lab 2" :id "61"
                                       :remote-newer t
                                       :fields (("POINTS" "10" "12"))
                                       :updated "2026-09-11T20:00:00Z")
                                      (:kind modified :title "Lab 3" :id "62"
                                       :remote-newer t :fields nil
                                       :updated "2026-09-11T22:00:00Z")
                                      (:kind missing :title "Lab 4" :id "63"))
                        :extra ((:kind extra :title "Surprise" :id "99"))))))
          (expect (test-org-canvas-api-call-count) :to-equal 0))
        (expect (car out) :to-match "Adopt the Canvas timestamp of 2 CHANGED entries")
        (expect (cdr out) :to-match "ADOPTED   Lab 1 (Assignments; CANVAS_UPDATED_AT set to 2026-09-11T20:00:00Z)")
        (expect (cdr out) :to-match "ADOPTED   Lab 3 (Assignments; CANVAS_UPDATED_AT set to 2026-09-11T22:00:00Z)")
        (expect (cdr out) :to-match "2 stamp(s) adopted; PAYLOAD_HASH kept, nothing sent to Canvas")
        (expect (cdr out) :to-match "1 CHANGED row(s) left alone: a compared property differs")
        (expect (cdr out) :not :to-match "Lab 2 (Assignments"))
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T20:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 1" "PAYLOAD_HASH") :to-equal "aaa")
      (expect (test-org-canvas-257--property file "Lab 2" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 3" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T22:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 3" "PAYLOAD_HASH") :to-equal "ccc")))

  (it "returns the count adopted and names a heading it could not find"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (let ((count nil))
        (cl-letf (((symbol-function 'org-canvas--report-display)
                   (lambda (_name render) (with-temp-buffer (funcall render) (buffer-string)))))
          (with-org-canvas-test-config
            (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                      ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                      ((symbol-function 'org-canvas--diff-feature)
                       (lambda (feature)
                         (if (string= (plist-get feature :name) "Assignments")
                             '(:name "Assignments"
                               :divergences ((:kind modified :title "Lab 1" :id "60"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z")
                                             (:kind modified :title "Ghost" :id "999"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z")))
                           (list :name (plist-get feature :name))))))
              ;; Batch: no prompt, `noninteractive' stays t under eldev.
              (setq count (org-canvas-diff-adopt-stamps)))))
        (expect count :to-equal 1))
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T20:00:00Z")))

  (it "renders the heading it could not find"
    (with-org-canvas-test-config
      (with-temp-buffer
        (org-canvas--diff-adopt-render
         '(("Assignments" "Lab 1" "2026-09-11T20:00:00Z"))
         '(("Assignments" . "Ghost")) 0)
        (expect (buffer-string) :to-match "NOT FOUND Ghost (Assignments; no heading carries its id)")
        (expect (buffer-string) :to-match "1 stamp(s) adopted")
        (expect (buffer-string) :not :to-match "left alone"))))

  (it "writes nothing and says so when the confirmation is declined"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (expect (test-org-canvas-257--run
               '((:name "Assignments"
                  :divergences ((:kind modified :title "Lab 1" :id "60"
                                 :remote-newer t :fields nil
                                 :updated "2026-09-11T20:00:00Z"))))
               'no)
              :to-throw 'user-error)
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")))

  (it "asks nothing and reports so when no row qualifies"
    (let ((out (test-org-canvas-257--run
                '((:name "Assignments"
                   :divergences ((:kind modified :title "Lab 2" :id "61"
                                  :remote-newer t :fields (("POINTS" "10" "12"))
                                  :updated "2026-09-11T20:00:00Z")))
                  (:name "Pages" :error "boom")))))
      (expect (car out) :to-be nil)
      (expect (cdr out) :to-match "No stamp to adopt")
      (expect (cdr out) :to-match "1 CHANGED row(s) left alone")))

  (it "skips a row whose feature is not registered, without stopping the rest"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (let ((outcome (org-canvas--diff-adopt-all
                      '(("No Such Feature" . (:kind modified :title "X" :id "1"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z"))
                        ("Assignments" . (:kind modified :title "Lab 1" :id "60"
                                          :remote-newer t :fields nil
                                          :updated "2026-09-11T20:00:00Z"))))))
        (expect (car outcome) :to-equal '(("Assignments" "Lab 1" "2026-09-11T20:00:00Z")))
        (expect (cdr outcome) :to-equal '(("No Such Feature" . "X")))))))

;;;; Referenced media (issue #102)

(describe "org-canvas--diff-file-references (issue #102)"
  (it "finds file ids in download links, previews and API endpoints, once each"
    (expect (org-canvas--diff-file-references
             (concat "<p><a href=\"https://x.instructure.com/courses/297530/files/31452881/download?wrap=1\""
                     " data-api-endpoint=\"https://x.instructure.com/api/v1/courses/297530/files/31452881\">a</a>"
                     "<img src=\"/courses/297530/files/31505590/preview\"></p>"))
            :to-equal '("31452881" "31505590")))

  (it "ignores paths that are not files"
    (expect (org-canvas--diff-file-references
             "<a href=\"/courses/297530/pages/welcome\">p</a> /files/ /filesystem/12")
            :to-be nil))

  (it "yields nothing for a non-string"
    (expect (org-canvas--diff-file-references nil) :to-be nil)
    (expect (org-canvas--diff-file-references :null) :to-be nil)))

(describe "org-canvas--diff-items-references (issue #102)"
  (it "collects the ids every item's body references, deduplicated"
    (expect (org-canvas--diff-items-references
             '(((id . 1) (message . "<img src=\"/files/10/preview\">"))
               ((id . 2) (message . :null))
               ((id . 3) (message . "/files/10/download and /files/11/download")))
             "message")
            :to-equal '("10" "11"))))

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

(describe "org-canvas--diff-syllabus-references (issue #102)"
  (it "asks for the syllabus body and scans it"
    (with-org-canvas-test-config
      (let ((params nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_method _url &rest args)
                     (setq params (plist-get args :params))
                     '((id . 99999)
                       (syllabus_body . "<a href=\"/courses/99999/files/42/download\">s</a>")))))
          (expect (org-canvas--diff-syllabus-references) :to-equal '("42")))
        (expect params :to-equal '(("include[]" . "syllabus_body"))))))

  (it "reads a failure as no references, and says so"
    (with-org-canvas-test-config
      (let ((warnings nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) (error "HTTP 500")))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) warnings))))
          (expect (org-canvas--diff-syllabus-references) :to-be nil))
        (expect (car warnings) :to-match "Could not read the syllabus")))))

(describe "org-canvas--diff-apply-references (issue #102)"
  (let ((files-result (lambda ()
                        (list :name "Files"
                              :extra (list (list :kind 'extra :title "a.png" :id "31452881")
                                           (list :kind 'extra :title "b.png" :id "31505590")
                                           (list :kind 'extra :title "dup.png" :id "31505574"))))))

    (it "moves referenced files out of the extras and counts them"
      ;; Course 297530: two attachments embedded by announcements, one
      ;; double upload nothing referenced.  Only the stray should remain.
      (cl-letf (((symbol-function 'org-canvas--diff-syllabus-references)
                 (lambda () '("31505590"))))
        (let* ((results (list (list :name "Announcements"
                                    :referenced-files '("31452881"))
                              (funcall files-result)))
               (files (nth 1 (org-canvas--diff-apply-references results))))
          (expect (mapcar (lambda (e) (plist-get e :id)) (plist-get files :extra))
                  :to-equal '("31505574"))
          (expect (plist-get files :referenced) :to-equal 2)
          (expect (org-canvas--diff-count results) :to-equal 1))))

    (it "does nothing when the scan is off"
      (let ((org-canvas-diff-scan-references nil))
        (cl-letf (((symbol-function 'org-canvas--diff-syllabus-references)
                   (lambda () (error "Must not be asked"))))
          (let* ((results (list (list :name "Announcements"
                                      :referenced-files '("31452881"))
                                (funcall files-result)))
                 (files (nth 1 (org-canvas--diff-apply-references results))))
            (expect (length (plist-get files :extra)) :to-equal 3)
            (expect (plist-get files :referenced) :to-be nil)))))

    (it "does nothing when Files was excluded or could not be checked"
      (cl-letf (((symbol-function 'org-canvas--diff-syllabus-references)
                 (lambda () (error "Must not be asked"))))
        (expect (org-canvas--diff-apply-references
                 (list (list :name "Files" :excluded t)))
                :not :to-throw)
        (expect (org-canvas--diff-apply-references
                 (list (list :name "Files" :error "boom")))
                :not :to-throw)
        (expect (org-canvas--diff-apply-references
                 (list (list :name "Pages" :referenced-files '("1"))))
                :not :to-throw)))))

(describe "org-canvas--diff-apply-extra-details (issue #296)"
  (let ((assignments
         (lambda ()
           (list :name "Assignments"
                 :remote-items
                 (list '((id . 501) (name . "Pop Quiz") (assignment_group_id . 7)
                         (points_possible . 20.0) (due_at . "2026-09-25T03:59:00Z")
                         (published . t) (needs_grading_count . 89))
                       '((id . 502) (name . "Draft") (assignment_group_id . 7)
                         (points_possible . 12.5) (due_at . :null)
                         (published . :json-false))
                       '((id . 61) (name . "Lab 1") (assignment_group_id . 8)))
                 :extra (list (list :kind 'extra :title "Pop Quiz" :id "501")
                              (list :kind 'extra :title "Draft" :id "502")
                              (list :kind 'unclaimed :title "Lab 1" :id "61")))))
        (groups
         (lambda ()
           (list :name "Assignment Groups"
                 :remote-items (list '((id . 7) (name . "Extra Credit")
                                       (group_weight . 10.0))
                                     '((id . 8) (name . "Labs")
                                       (group_weight . 100)))
                 :extra (list (list :kind 'extra :title "Extra Credit" :id "7"))))))

    (it "names what each Canvas-only assignment and group is, from the lists held"
      (let ((org-canvas-assignment-groups-file "/nonexistent/groups.org"))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) (error "No request may be made")))
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (error "No request may be made"))))
          (let* ((results (org-canvas--diff-apply-extra-details
                           (list (funcall assignments) (funcall groups))))
                 (extra (plist-get (nth 0 results) :extra))
                 (group (car (plist-get (nth 1 results) :extra))))
            (expect (plist-get (nth 0 extra) :details)
                    :to-equal "group 'Extra Credit', 20 pts, due <2026-09-25 Fri 03:59>, published, 89 to grade")
            (expect (plist-get (nth 1 extra) :details)
                    :to-equal "group 'Extra Credit', 12.5 pts, no due date, unpublished")
            ;; An UNCLAIMED row names its heading; it needs no details.
            (expect (plist-get (nth 2 extra) :details) :to-be nil)
            (expect (plist-get group :details)
                    :to-equal "weight 10%, 2 assignments")
            ;; No groups file, no Org total to set against Canvas's.
            (expect (plist-get (nth 1 results) :weight-totals) :to-be nil)))))

    (it "names the group by id and leaves the count out when the other list was not read"
      (let* ((results (org-canvas--diff-apply-extra-details
                       (list (funcall assignments)
                             (list :name "Assignment Groups" :excluded t)))))
        (expect (plist-get (car (plist-get (nth 0 results) :extra)) :details)
                :to-match "^group id 7, 20 pts"))
      (let ((org-canvas-assignment-groups-file "/nonexistent/groups.org"))
        (let* ((results (org-canvas--diff-apply-extra-details
                         (list (list :name "Assignments" :error "boom")
                               (funcall groups)))))
          (expect (plist-get (car (plist-get (nth 1 results) :extra)) :details)
                  :to-equal "weight 10%"))))

    (it "prints a due date it cannot parse as Canvas sent it"
      (expect (org-canvas--diff-assignment-details
               '((due_at . "tomorrow")) (make-hash-table :test 'equal))
              :to-equal "due tomorrow"))

    (it "gives an assignment in a group of one the singular"
      (expect (org-canvas--diff-group-details
               '((id . 8) (group_weight . :null))
               (let ((h (make-hash-table :test 'equal))) (puthash "8" 1 h) h))
              :to-equal "weight 0%, 1 assignment"))

    (it "sets the weight totals when Org's groups and Canvas's disagree"
      (let ((file (make-temp-file "groups-" nil ".org")))
        (unwind-protect
            (progn
              (with-temp-file file
                (insert "* Groups\n"
                        "** Labs\n:PROPERTIES:\n:CANVAS_ID: 8\n:WEIGHT: 60\n:END:\n"
                        "** Exams\n:PROPERTIES:\n:WEIGHT: 40\n:END:\n"
                        "** Unweighted\n"))
              (let ((org-canvas-assignment-groups-file file))
                (let ((results (org-canvas--diff-apply-extra-details
                                (list (funcall groups)))))
                  ;; Org says 100; Canvas holds 110 with the extra group.
                  (expect (plist-get (car results) :weight-totals)
                          :to-equal '(100 . 110.0)))
                (let ((agreeing (list :name "Assignment Groups"
                                      :remote-items
                                      (list '((id . 8) (group_weight . 60))
                                            '((id . 9) (group_weight . 40.0))))))
                  (org-canvas--diff-apply-extra-details (list agreeing))
                  (expect (plist-get agreeing :weight-totals) :to-be nil))))
          (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
          (delete-file file))))))

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
                            (org-canvas--registry-find-feature "assignments"))
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

;;;; A Canvas-Owned Property Is Compared Even When the Heading Is Silent (issue #184)

(describe "org-canvas--diff-compare-fields with a :canvas-owned spec"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (let ((specs '((:org-prop "DOCUMENT_PROCESSOR" :data-key :asset_processors
                  :type string :canvas-owned t
                  :remote-fn org-canvas--assignment-remote-document-processor
                  :remote-known-p org-canvas--assignment-document-processor-known-p))))
    (it "reports a processor attached in the web UI after the last pull"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point)
                '((asset_processors . [((id . 5) (title . "Turnitin"))])))
               :to-equal '(("DOCUMENT_PROCESSOR" "(unset)"
                            "Turnitin (asset processor 5)")))))

    (it "says nothing when neither side has one"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields specs (point) '((id . 1)))
               :to-be nil)
       (expect (org-canvas--diff-compare-fields
                specs (point) '((asset_processors . [])))
               :to-be nil)))

    (it "reports a processor detached since the pull"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:DOCUMENT_PROCESSOR: Turnitin (asset processor 5)\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point) '((asset_processors . [])))
               :to-equal '(("DOCUMENT_PROCESSOR" "Turnitin (asset processor 5)"
                            "(unset)")))))

    (it "agrees when the pulled value still matches"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:DOCUMENT_PROCESSOR: Turnitin (asset processor 5)\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point)
                '((asset_processors . [((id . 5) (title . "Turnitin"))])))
               :to-be nil)))

    (it "is how the assignments registry declares DOCUMENT_PROCESSOR"
      (let ((spec (seq-find (lambda (s)
                              (equal (plist-get s :org-prop) "DOCUMENT_PROCESSOR"))
                            (org-canvas-diff-test--specs "assignments"))))
        (expect (plist-get spec :canvas-owned) :to-be t)
        (expect (plist-get spec :remote-fn)
                :to-equal 'org-canvas--assignment-remote-document-processor)))))

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

;;;; A Declared Document Processor Held Against Canvas (issue #293)

(describe "org-canvas--diff-compare-fields with an :intent-of spec"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (let ((specs '((:org-prop "DOCUMENT_PROCESSOR" :data-key :asset_processors
                  :type string :canvas-owned t
                  :remote-fn org-canvas--assignment-remote-document-processor
                  :remote-known-p org-canvas--assignment-document-processor-known-p)
                 (:org-prop "WANT_DOCUMENT_PROCESSOR" :data-key :want_document_processor
                  :type string :intent-of "DOCUMENT_PROCESSOR"))))
    (it "reports a column Canvas says carries no processor"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (let ((diffs (org-canvas--diff-compare-fields
                     specs (point) '((id . 1) (asset_processors . [])))))
         (expect (length diffs) :to-equal 1)
         (expect (nth 0 (car diffs)) :to-equal "WANT_DOCUMENT_PROCESSOR")
         (expect (nth 1 (car diffs)) :to-equal "Turnitin")
         (expect (nth 2 (car diffs)) :to-match "(none; attach it in the web UI"))))

    (it "says Canvas did not report the field when the key is absent"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields specs (point) '((id . 1)))
               :to-equal '(("WANT_DOCUMENT_PROCESSOR" "Turnitin"
                            "(not reported by Canvas)")))))

    (it "names the processor Canvas holds when it is another tool"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:DOCUMENT_PROCESSOR: Copyleaks (asset processor 9)\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point)
                '((asset_processors . [((id . 9) (title . "Copyleaks"))])))
               :to-equal '(("WANT_DOCUMENT_PROCESSOR" "Turnitin"
                            "Copyleaks (asset processor 9)")))))

    (it "agrees when Canvas holds the declared processor"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: turnitin\n:DOCUMENT_PROCESSOR: Turnitin (asset processor 5)\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point)
                '((asset_processors . [((id . 5) (title . "Turnitin"))])))
               :to-be nil)))

    (it "leaves a processor attached since the pull to the observed row"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point)
                '((asset_processors . [((id . 5) (title . "Turnitin"))])))
               :to-equal '(("DOCUMENT_PROCESSOR" "(unset)"
                            "Turnitin (asset processor 5)")))))

    (it "says nothing for a heading that declares nothing"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                specs (point) '((asset_processors . [])))
               :to-be nil)))

    (it "ignores an intent whose observed property is not registered"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-compare-fields
                (last specs) (point) '((asset_processors . [])))
               :to-be nil)))))

(describe "org-canvas--diff-entry-notes"
  (before-each (test-org-canvas-stub-processors))
  (after-each (org-canvas--assignment-processors-forget))
  (let ((specs (org-canvas-diff-test--specs "assignments"))
        (index (make-hash-table :test 'equal)))
    (puthash "1" '((id . 1) (asset_processors . [((id . 5) (title . "Turnitin"))]))
             index)
    (puthash "2" '((id . 2) (asset_processors . [])) index)
    (it "notes a processor Canvas holds that no declaration asks for"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
       (org-back-to-heading)
       (let ((notes (org-canvas--diff-entry-notes
                     (list :id "1" :title "Essay" :pom (point)) index specs)))
         (expect (length notes) :to-equal 1)
         (expect (plist-get (car notes) :kind) :to-equal 'note)
         (expect (plist-get (car notes) :property) :to-equal "WANT_DOCUMENT_PROCESSOR")
         (expect (plist-get (car notes) :observed) :to-equal "DOCUMENT_PROCESSOR")
         (expect (plist-get (car notes) :remote)
                 :to-equal "Turnitin (asset processor 5)"))))

    (it "has nothing to note once declared, without a processor, or without an item"
      (with-temp-org-buffer
       "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n:END:\n"
       (org-back-to-heading)
       (expect (org-canvas--diff-entry-notes
                (list :id "1" :title "Essay" :pom (point)) index specs)
               :to-be nil)
       (expect (org-canvas--diff-entry-notes
                (list :id "2" :title "Essay" :pom (point)) index specs)
               :to-be nil)
       (expect (org-canvas--diff-entry-notes
                (list :id "3" :title "Essay" :pom (point)) index specs)
               :to-be nil)
       (expect (org-canvas--diff-entry-notes
                (list :id nil :title "Essay" :pom (point)) index specs)
               :to-be nil)))))

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

;;;; Deleting Rows Without Asking (issue #345)

(defconst test-org-canvas--diff-reshuffle-results
  '((:name "Assignments"
     :extra ((:kind extra :title "Session 87" :id "701")
             (:kind unclaimed :title "Session 88" :id "702" :property "CANVAS_ID")))
    (:name "Rubrics"
     :extra ((:kind extra :title "Old rubric" :id "801")))
    (:name "Module Items"
     :divergences ((:kind stale-ack :id "999"))
     :extra ((:kind extra :title "87-surveillance-capitalism" :id "5787192"
              :module-id "781715" :where "Week 9")
             (:kind extra :title "88-privacy" :id "5787193"
              :module-id "781715" :where "Week 9")
             (:kind moved :title "Closer" :id "5787194"
              :module-id "781715" :where "Week 9" :to "Week 10"))))
  "What the report found after the calendar reshuffle of issue #345.")

(defmacro test-org-canvas--with-batch-delete (&rest body)
  "Run BODY against the mock API with snapshots in a temporary directory.
Preflight is stubbed; `snapshot-dir' names the directory."
  (declare (indent 0))
  `(with-org-canvas-test-config
     (test-org-canvas--with-snapshot-dir
       (with-mock-api
         (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore))
           ,@body)))))

(defun test-org-canvas--diff-outcome (outcomes id)
  "Return the outcome symbol OUTCOMES record for ID."
  (plist-get (cl-find id outcomes :key (lambda (o) (plist-get o :id))
                      :test #'equal)
             :outcome))

(defun test-org-canvas--diff-deleted-urls ()
  "Return the URLs the mock API received a DELETE for, oldest first."
  (reverse (delq nil (mapcar (lambda (c) (and (eq (car c) 'DELETE) (nth 1 c)))
                             test-org-canvas-api-calls))))

(describe "org-canvas-diff-delete-rows (issue #345)"
  (it "deletes module items and a clean assignment, snapshotting each first"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions" . [((user_id . 1) (workflow_state . "unsubmitted"))])
              ("assignments/701" . ((id . 701) (name . "Session 87")))
              ("modules/781715/items/5787192" . ((id . 5787192) (title . "87-surveillance-capitalism")))))
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Module Items" :id "5787192")
                         (:feature "assignments" :id 701))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(deleted deleted))
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas-api-course-endpoint "modules/781715/items/5787192")
                                (org-canvas-api-course-endpoint "assignments/701")))
        ;; Each snapshot holds the object as Canvas returned it.
        (let* ((file (plist-get (car outcomes) :snapshot))
               (json (json-read-file file)))
          (expect (file-name-directory file) :to-equal (file-name-as-directory snapshot-dir))
          (expect (file-name-nondirectory file) :to-match "-module-items-5787192\\.json\\'")
          (expect (alist-get 'title json) :to-equal "87-surveillance-capitalism"))
        (expect (length (directory-files snapshot-dir nil "\\.json\\'")) :to-equal 2))))

  (it "reads an assignment with its own dates, the parameters its item read carries"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions" . [])
              ("assignments/701" . ((id . 701)))))
      (org-canvas-diff-delete-rows '((:feature "Assignments" :id "701"))
                                   :results test-org-canvas--diff-reshuffle-results)
      (let ((read (cl-find-if (lambda (c) (and (eq (car c) 'GET)
                                               (string-match-p "assignments/701\\'" (nth 1 c))))
                              test-org-canvas-api-calls)))
        (expect (plist-get (nth 3 read) :params)
                :to-equal (org-canvas--feature-item-params
                           (org-canvas--registry-find-feature "Assignments"))))))

  (it "refuses an assignment with a submission or a score, naming why"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions"
               . [((user_id . 1) (submitted_at . "2026-09-01T10:00:00Z") (workflow_state . "submitted"))
                  ((user_id . 2) (score . 8.0) (grade . "8") (workflow_state . "graded"))
                  ((user_id . 3) (workflow_state . "unsubmitted"))])
              ("assignments/701" . ((id . 701)))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Assignments" :id "701"))
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason)
                :to-equal "the assignment has 1 submission(s) and 1 score(s)")
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil)
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil))))

  (it "refuses an assignment Canvas flags as submitted to even when the list shows none"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions" . [])
              ("assignments/701" . ((id . 701) (has_submitted_submissions . t)))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Assignments" :id "701"))
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason) :to-match "Canvas says"))))

  (it "checks a New Quiz's submissions as its assignment's, deleting at the quiz service (#313)"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/41/submissions"
               . [((user_id . 1) (submitted_at . "2026-09-01T10:00:00Z") (workflow_state . "submitted"))])
              ("assignments/42/submissions" . [])
              ("quizzes/4[12]" . ((id . "41") (assignment_id . "41") (title . "Midterm")))))
      (let* ((results '((:name "New Quizzes"
                         :extra ((:kind extra :title "Midterm" :id "41")
                                 (:kind extra :title "Final" :id "42")))))
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "New Quizzes" :all t)) :results results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(refused deleted))
        (expect (plist-get (car outcomes) :reason)
                :to-equal "the assignment has 1 submission(s) and 0 score(s)")
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas--new-quiz-api-endpoint "quizzes/42"))))))

  (it "refuses a rubric an assignment grades with, read with its associations"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("rubrics/801" . ((id . 801)
                                (associations . [((association_type . "Course") (association_id . 1))
                                                 ((association_type . "Assignment") (association_id . 701))])))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Rubrics" :id "801"))
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason) :to-equal "the rubric grades assignment(s) 701")
        (expect (test-org-canvas-call-arg
                 (test-org-canvas-find-api-call 'GET "rubrics/801") :params)
                :to-equal '(("include[]" . "associations")))
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil))))

  (it "deletes a rubric only a course association holds"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("rubrics/801" . ((id . 801)
                                (associations . [((association_type . "Course") (association_id . 1))])))))
      (expect (test-org-canvas--diff-outcome
               (org-canvas-diff-delete-rows '((:feature "Rubrics" :id "801"))
                                            :results test-org-canvas--diff-reshuffle-results)
               "801")
              :to-be 'deleted)))

  (it "deletes a refused object under :force, keeping the reason"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("rubrics/801" . ((id . 801)
                                (associations . [((association_type . "Assignment") (association_id . 701))])))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Rubrics" :id "801"))
                           :force t
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'deleted)
        (expect (plist-get outcome :reason) :to-match "701")
        (expect (file-exists-p (plist-get outcome :snapshot)) :to-be-truthy)
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas-api-course-endpoint "rubrics/801"))))))

  (it "deletes every EXTRA row of a feature under :all, and nothing UNCLAIMED or MOVED"
    (test-org-canvas--with-batch-delete
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Module Items" :all t))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :id)) outcomes)
                :to-equal '("5787192" "5787193"))
        (expect (length (test-org-canvas--diff-deleted-urls)) :to-equal 2))))

  (it "deletes a row two selectors name only once"
    (test-org-canvas--with-batch-delete
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Module Items" :id "5787192")
                         (:feature "module-items" :all t))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :id)) outcomes)
                :to-equal '("5787192" "5787193"))
        (expect (length (test-org-canvas--diff-deleted-urls)) :to-equal 2))))

  (it "names a selector that matches no EXTRA row, and sends nothing for it"
    (test-org-canvas--with-batch-delete
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Assignments" :id "702")
                         (:feature "Module Items" :id "5787194")
                         (:feature "Module Items" :id "999")
                         (:feature "Assignments" :id "42")
                         (:feature "Nonexistent" :id "1"))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(not-extra not-extra not-extra not-found not-found))
        (expect (plist-get (car outcomes) :reason) :to-match "UNCLAIMED row")
        (expect (plist-get (nth 3 outcomes) :reason) :to-match "no row with this id")
        (expect (plist-get (nth 4 outcomes) :reason) :to-match "no such feature")
        (expect test-org-canvas-api-calls :to-be nil))))

  (it "calls a RELOCATE row not EXTRA, and :all leaves it alone (issue #343)"
    (test-org-canvas--with-batch-delete
      (let* ((results '((:name "Module Items"
                         :relocate ((:kind relocate :title "Journal 05"
                                     :id "5707155" :where "Week 06"
                                     :to "Week 07")))))
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "Module Items" :id "5707155")
                          (:feature "Module Items" :all t))
                        :results results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(not-extra))
        (expect (plist-get (car outcomes) :reason) :to-match "RELOCATE row")
        (expect test-org-canvas-api-calls :to-be nil))))

  (it "refuses an object Canvas returned nothing for, with nothing to snapshot"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses '(("modules/781715/items/5787192" . nil)))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest args)
                   (push (list method url nil args) test-org-canvas-api-calls)
                   nil)))
        (let ((outcome (car (org-canvas-diff-delete-rows
                             '((:feature "Module Items" :id "5787192"))
                             :results test-org-canvas--diff-reshuffle-results))))
          (expect (plist-get outcome :outcome) :to-be 'refused)
          (expect (plist-get outcome :reason) :to-match "nothing to snapshot")))))

  (it "reads and checks under a dry run, but writes no snapshot and sends no DELETE"
    (test-org-canvas--with-batch-delete
      (let* ((org-canvas--dry-run t)
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "Module Items" :all t))
                        :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(dry-run dry-run))
        (expect (cl-remove-if (lambda (c) (eq (car c) 'GET)) test-org-canvas-api-calls)
                :to-be nil)
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil))))

  (it "fails every row on a read-only course, before a snapshot or a DELETE"
    (test-org-canvas--with-batch-delete
      (let* ((org-canvas-read-only t)
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "Module Items" :id "5787192"))
                        :results test-org-canvas--diff-reshuffle-results)))
        (expect (plist-get (car outcomes) :outcome) :to-be 'failed)
        (expect (plist-get (car outcomes) :reason) :to-match "read-only")
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil)
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil))))

  (it "records a failed read and goes on to the next row"
    (test-org-canvas--with-batch-delete
      (let ((mock (symbol-function 'org-canvas-api-request)))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (if (string-match-p "5787192\\'" url)
                         (signal 'org-canvas-api-error '("404 Not Found"))
                       (apply mock method url args))))
                  ((symbol-function 'org-canvas--log-error) #'ignore))
          (let ((outcomes (org-canvas-diff-delete-rows
                           '((:feature "Module Items" :all t))
                           :results test-org-canvas--diff-reshuffle-results)))
            (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                    :to-equal '(failed deleted))
            (expect (plist-get (car outcomes) :reason) :to-match "404")
            (expect (length (test-org-canvas--diff-deleted-urls)) :to-equal 1))))))

  (it "runs the report's own comparison when no results are given"
    (test-org-canvas--with-batch-delete
      (let ((collected 0))
        (cl-letf (((symbol-function 'org-canvas--diff-collect-results)
                   (lambda () (setq collected (1+ collected))
                     test-org-canvas--diff-reshuffle-results)))
          (expect (test-org-canvas--diff-outcome
                   (org-canvas-diff-delete-rows '((:feature "Module Items" :id "5787193")))
                   "5787193")
                  :to-be 'deleted)
          (expect collected :to-equal 1))))))

(describe "org-canvas-diff-delete's safety check (issue #345)"
  (it "names an assignment's submissions in its question"
    (test-org-canvas--with-snapshot-dir
      (let ((asked nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (q) (setq asked q) nil))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _)
                     (push (list method url) test-org-canvas--diff-delete-requests)
                     (if (string-match-p "submissions" url)
                         [((submitted_at . "2026-09-01T10:00:00Z"))]
                       '((id . 701))))))
          (with-current-buffer (test-org-canvas--diff-report-buffer
                                '((:name "Assignments" :extra ((:kind extra :title "Session 87" :id "701")))))
            (test-org-canvas--diff-goto-row 'extra)
            (org-canvas-diff-delete)))
        (expect asked :to-equal
                "Delete Assignments 'Session 87' (id 701) from Canvas, although the assignment has 1 submission(s) and 0 score(s)? ")
        (expect (test-org-canvas--diff-deletes) :to-be nil)))))

(describe "org-canvas--diff-snapshot-dir (issue #345)"
  (it "defaults to canvas-snapshots/ under the course directory, resolved when used"
    (let* ((dir (make-temp-file "diff-course-" t))
           (org-canvas-directory dir)
           (org-canvas-diff-delete-snapshot-directory nil))
      (unwind-protect
          (expect (org-canvas--diff-snapshot-dir)
                  :to-equal (expand-file-name "canvas-snapshots/" (file-truename dir)))
        (delete-directory dir t)))))

;;;; Stamping a MOVED Row and the RELOCATE Row (issues #342, #343)

(defconst test-org-canvas-342--modules-org
  "* Week 5
:PROPERTIES:
:CANVAS_ID: 781703
:END:
** Readings
* Week 6
:PROPERTIES:
:CANVAS_ID: 781704
:END:
** [[file:assignments.org::*R7: Arguing Both Sides][R7: Arguing Both Sides]]
** Readings
"
  "Week 6 holds a renumbered item heading and a Readings header, both unstamped.")

(defconst test-org-canvas-342--r7
  "[[file:assignments.org::*R7: Arguing Both Sides][R7: Arguing Both Sides]]"
  "The raw text of the renumbered heading, as the report records it.")

(defmacro test-org-canvas-342--with-modules-file (var &rest body)
  "Bind VAR to a temporary modules file holding the #342 fixture, run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "diff-342-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert test-org-canvas-342--modules-org))
           ,@body)
       (let ((buf (find-buffer-visiting ,var))) (when buf (kill-buffer buf)))
       (delete-file ,var))))

(defun test-org-canvas-342--ids (file)
  "Return (HEADING CANVAS_ID) for each level-2 heading of FILE.
HEADING is the shown text, the same on Emacs 29 and 30."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-map-entries
     (lambda ()
       (list (org-link-display-format (org-get-heading t t t t))
             (org-entry-get (point) "CANVAS_ID")))
     "LEVEL=2")))

(defun test-org-canvas-342--moved (file &rest overrides)
  "Return the MOVED entry naming the R7 heading of FILE, with OVERRIDES."
  (let ((entry (list :kind 'moved :title "R6: Arguing Both Sides" :id "5707155"
                     :module-id "781703" :where "Week 5" :to "Week 6"
                     :file file :line 10
                     :partner-heading test-org-canvas-342--r7
                     :partner-title "R7: Arguing Both Sides")))
    (while overrides
      (setq entry (plist-put entry (pop overrides) (pop overrides))))
    entry))

(defun test-org-canvas-342--row-buffer (entry)
  "Return a report buffer whose Module Items section holds ENTRY."
  (test-org-canvas--diff-report-buffer
   `((:name "Modules" :children (:name "Module Items" :extra (,entry))))))

(describe "a MOVED row names the heading it paired (issue #342)"
  (it "carries the partner's file, line and title from the pairing"
    (let* ((child (car (test-org-canvas-299--run)))
           (moved (test-org-canvas-299--by-id child "603")))
      (expect (plist-get moved :partner-title) :to-equal "Closer")
      (expect (plist-get moved :partner-heading)
              :to-equal "[[file:assignments.org::*Closer: Privacy Pros and Cons][Closer]]")
      (expect (plist-get moved :line) :to-equal 11)
      (expect (file-name-nondirectory (plist-get moved :file)) :to-equal "modules.org")))

  (it "prints the paired heading and says the sync recreates the item with a new id"
    (with-temp-buffer
      (org-canvas--diff-insert-entry
       '(:kind moved :title "R6: Arguing Both Sides" :id "5707155" :module-id "1"
         :where "Week 5" :to "Week 6" :line 10
         :partner-title "R7: Arguing Both Sides"))
      (expect (buffer-string)
              :to-match "the unstamped heading 'R7: Arguing Both Sides' (line 10) places it in 'Week 6'")
      (expect (buffer-string)
              :to-match "stamp CANVAS_ID 5707155 on that heading and the next sync instead recreates it there with a new id and deletes this copy")
      (expect (buffer-string) :not :to-match "to move it instead")))

  (it "names the heading without a line, or says an unstamped heading, when that is all it has"
    (expect (org-canvas--diff-partner-text '(:partner-title "Closer"))
            :to-equal "the unstamped heading 'Closer'")
    (expect (org-canvas--diff-partner-text '(:line 4))
            :to-equal "an unstamped heading")))

(describe "org-canvas-diff-stamp-move (issue #342)"
  (it "stamps the item id on the paired heading, sends nothing, and marks the row"
    (test-org-canvas-342--with-modules-file file
      (with-mock-api
        (with-current-buffer (test-org-canvas-342--row-buffer
                              (test-org-canvas-342--moved file))
          (test-org-canvas--diff-goto-row 'moved)
          (org-canvas-diff-stamp-move)
          (expect (thing-at-point 'line t)
                  :to-match "STAMPED   R6: Arguing Both Sides (CANVAS_ID 5707155 on 'R7: Arguing Both Sides' in 'Week 6', nothing sent; the next sync recreates it there with a new id and deletes the copy in 'Week 5')")
          (expect (get-text-property (point) 'org-canvas-diff-row) :to-be-truthy))
        (expect (test-org-canvas-api-call-count) :to-equal 0))
      (expect (test-org-canvas-342--ids file)
              :to-equal '(("Readings" nil)
                          ("R7: Arguing Both Sides" "5707155")
                          ("Readings" nil)))))

  (it "is what s does, and S stamps them all"
    (expect (lookup-key org-canvas-diff-mode-map (kbd "s"))
            :to-be #'org-canvas-diff-stamp-move)
    (expect (lookup-key org-canvas-diff-mode-map (kbd "S"))
            :to-be #'org-canvas-diff-stamp-moves)
    (with-current-buffer (test-org-canvas--diff-report-buffer nil)
      (expect (buffer-string) :to-match "s/S stamp moved")))

  (it "finds the heading under its module when it is no longer on the recorded line"
    (test-org-canvas-342--with-modules-file file
      (with-current-buffer (test-org-canvas-342--row-buffer
                            (test-org-canvas-342--moved
                             file :title "Readings" :partner-heading "Readings"
                             :partner-title "Readings" :line 1))
        (test-org-canvas--diff-goto-row 'moved)
        (org-canvas-diff-stamp-move))
      ;; The Readings under Week 6, not the one under Week 5.
      (expect (test-org-canvas-342--ids file)
              :to-equal '(("Readings" nil)
                          ("R7: Arguing Both Sides" nil)
                          ("Readings" "5707155")))))

  (it "refuses when a heading already carries the id, and writes nothing"
    (test-org-canvas-342--with-modules-file file
      (let ((entry (test-org-canvas-342--moved file :id "781703")))
        (with-current-buffer (test-org-canvas-342--row-buffer entry)
          (test-org-canvas--diff-goto-row 'moved)
          (expect (condition-case e (org-canvas-diff-stamp-move)
                    (user-error (error-message-string e)))
                  :to-match "Item id 781703 is already on a heading")
          (expect (thing-at-point 'line t) :to-match "MOVED")))
      (expect (mapcar #'cadr (test-org-canvas-342--ids file)) :to-equal '(nil nil nil))))

  (it "refuses when the paired heading is gone or already stamped"
    (test-org-canvas-342--with-modules-file file
      (with-current-buffer (test-org-canvas-342--row-buffer
                            (test-org-canvas-342--moved
                             file :partner-heading "Nowhere" :partner-title "Nowhere"))
        (test-org-canvas--diff-goto-row 'moved)
        (expect (condition-case e (org-canvas-diff-stamp-move)
                  (user-error (error-message-string e)))
                :to-match "Cannot find the unstamped heading .Nowhere. under .Week 6."))
      (expect (org-canvas--diff-stamp-entry
               (test-org-canvas-342--moved (concat file ".missing")))
              :to-equal 'not-found)
      ;; A row with no partner title names the item instead.
      (expect (condition-case e
                  (org-canvas--diff-stamp-refusal
                   '(:title "Closer" :to "Week 6") 'not-found)
                (user-error (error-message-string e)))
              :to-match "unstamped heading .Closer. under .Week 6.")))

  (it "asks first on a row paired by title only, and writes nothing when declined"
    (test-org-canvas-342--with-modules-file file
      (let ((answer nil) (asked nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (q) (setq asked q) answer)))
          (with-current-buffer (test-org-canvas-342--row-buffer
                                (test-org-canvas-342--moved file :by-title t))
            (test-org-canvas--diff-goto-row 'moved)
            (expect (org-canvas-diff-stamp-move) :to-throw 'user-error)
            (expect asked :to-match "paired by title only")
            (expect (cadr (nth 1 (test-org-canvas-342--ids file))) :to-be nil)
            (setq answer t)
            (org-canvas-diff-stamp-move)))
        (expect (cadr (nth 1 (test-org-canvas-342--ids file))) :to-equal "5707155"))))

  (it "refuses a row that is not MOVED"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :extra ((:kind extra :title "Surprise" :id "99")))))
      (test-org-canvas--diff-goto-row 'extra)
      (expect (org-canvas-diff-stamp-move) :to-throw 'user-error))))

(describe "org-canvas-diff-stamp-moves (issue #342)"
  (defun test-org-canvas-342--run (results &optional answer)
    "Run the bulk stamp over RESULTS as if every feature reported them.
ANSWER `no' declines the confirmation.  Returns (PROMPT REPORT COUNT)."
    (let ((prompt nil) (count nil))
      (with-org-canvas-test-config
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                    ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                    ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                    ((symbol-function 'y-or-n-p)
                     (lambda (q) (setq prompt q) (not (eq answer 'no))))
                    ((symbol-function 'org-canvas--diff-feature)
                     (lambda (feature)
                       (or (cl-find (plist-get feature :name) results
                                    :key (lambda (r) (plist-get r :name))
                                    :test #'string=)
                           (list :name (plist-get feature :name))))))
            (setq count (org-canvas-diff-stamp-moves)))))
      (list prompt
            (with-current-buffer org-canvas--diff-stamp-buffer-name (buffer-string))
            count)))

  (it "stamps every MOVED row paired by content after confirming, lists the rest, and sends nothing"
    (test-org-canvas-342--with-modules-file file
      (let ((out nil))
        (with-mock-api
          (setq out (test-org-canvas-342--run
                     `((:name "Modules"
                        :children
                        (:name "Module Items"
                         :extra (,(test-org-canvas-342--moved file)
                                 ,(test-org-canvas-342--moved
                                   file :title "Readings" :id "502"
                                   :partner-heading "Readings" :partner-title "Readings"
                                   :line 11 :by-title t)
                                 ,(test-org-canvas-342--moved
                                   file :title "Gone" :id "503"
                                   :partner-heading "Gone" :partner-title "Gone")
                                 ,(test-org-canvas-342--moved
                                   file :title "Twice" :id "781704")
                                 (:kind extra :title "Stray" :id "504")))))))
          (expect (test-org-canvas-api-call-count) :to-equal 0))
        (expect (nth 0 out) :to-match "Stamp the item id of 3 MOVED rows")
        (expect (nth 1 out) :to-match "STAMPED   R6: Arguing Both Sides (CANVAS_ID 5707155 on 'R7: Arguing Both Sides' in 'Week 6')")
        (expect (nth 1 out) :to-match "NOT FOUND Gone (no unstamped heading 'Gone' under 'Week 6')")
        (expect (nth 1 out) :to-match "CLAIMED   Twice (item id 781704 is already on a heading)")
        (expect (nth 1 out) :to-match "HELD      Readings (item id 502; paired by title only")
        (expect (nth 1 out) :to-match "1 item id(s) stamped, nothing sent to Canvas")
        (expect (nth 1 out) :not :to-match "Stray")
        (expect (nth 2 out) :to-equal 1))
      (expect (test-org-canvas-342--ids file)
              :to-equal '(("Readings" nil)
                          ("R7: Arguing Both Sides" "5707155")
                          ("Readings" nil)))))

  (it "writes nothing when the confirmation is declined"
    (test-org-canvas-342--with-modules-file file
      (expect (test-org-canvas-342--run
               `((:name "Modules"
                  :children (:name "Module Items"
                             :extra (,(test-org-canvas-342--moved file)))))
               'no)
              :to-throw 'user-error)
      (expect (mapcar #'cadr (test-org-canvas-342--ids file)) :to-equal '(nil nil nil))))

  (it "asks nothing and says so when no row qualifies"
    (let ((out (test-org-canvas-342--run nil)))
      (expect (nth 0 out) :to-be nil)
      (expect (nth 1 out) :to-match "No item id stamped")
      (expect (nth 2 out) :to-equal 0)))

  (it "runs from a batch Emacs without asking"
    (test-org-canvas-342--with-modules-file file
      (let ((count nil))
        (cl-letf (((symbol-function 'org-canvas--report-display)
                   (lambda (_name render) (with-temp-buffer (funcall render))))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
          (with-org-canvas-test-config
            (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                      ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                      ((symbol-function 'org-canvas--diff-feature)
                       (lambda (feature)
                         (list :name (plist-get feature :name)
                               :extra (and (string= (plist-get feature :name) "Modules")
                                           (list (test-org-canvas-342--moved file)))))))
              (setq count (org-canvas-diff-stamp-moves)))))
        (expect count :to-equal 1))
      (expect (cadr (nth 1 (test-org-canvas-342--ids file))) :to-equal "5707155"))))

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
        (expect report :to-match "  RELOCATE  Journal 05 (item id 5707155 sits in module 'Week 06'; its heading, stamped with that id, is under 'Week 07', so the next sync recreates it there with a new id and deletes this copy)")
        (expect report :to-match "No drift")
        (expect report :to-match "Relocations: 1 module item the next sync recreates in its heading's module with a new id"))
      (expect (org-canvas--diff-render
               '((:name "Module Items"
                  :relocate ((:kind relocate :title "a" :id "1" :where "x" :to "y")
                             (:kind relocate :title "b" :id "2" :where "x" :to "y")))))
              :to-match "Relocations: 2 module items the next sync recreates in their heading's module")
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

;;;; New Quizzes in the drift report (issue #313)

(defconst test-nq-313--file
  (concat "* Midterm\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 41\n:TIME_LIMIT: 30\n:SCORING_POLICY: keep_highest\n:END:\n"
          "Read carefully.\n\n"
          "** Q1\n:PROPERTIES:\n:CANVAS_ITEM_ID: 9\n:TYPE: essay\n:END:\n"
          "* Final\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 42\n:END:\n"
          "* Draft\n")
  "A new-quizzes.org with a drifted quiz, a deleted one and an unpushed one.")

(defconst test-nq-313--remote
  '(((id . "41") (assignment_id . "41") (title . "Midterm")
     (instructions . "<p>Read slowly.</p>")
     (quiz_settings (has_time_limit . t)
                    (session_time_limit_in_seconds . 2700)
                    (multiple_attempts (multiple_attempts_enabled . :json-false)
                                       (score_to_keep . "highest"))))
    ((id . "43") (assignment_id . "43") (title . "Web Quiz")))
  "The quiz service's list for `test-nq-313--file'.
Midterm's settings sit under `quiz_settings' as Canvas keeps them
\(issue #321): its time limit differs, its SCORING_POLICY agrees.")

(defmacro test-nq-313--with-file (content &rest body)
  "Run BODY with `org-canvas-new-quizzes-file' holding CONTENT."
  (declare (indent 1))
  `(let ((file (make-temp-file "diff-nq-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (let ((org-canvas-new-quizzes-file file))
             ,@body))
       (let ((buf (find-buffer-visiting file)))
         (when buf
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-file file))))

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

(describe "org-canvas--new-quiz-remote-carries (issue #313)"
  (it "compares a setting only when the reply carries its key"
    (let ((pred (org-canvas--new-quiz-remote-carries 'time_limit)))
      (expect (funcall pred nil '((time_limit . 30))) :to-be-truthy)
      (expect (funcall pred nil '((time_limit . :null))) :to-be-truthy)
      (expect (funcall pred nil '((title . "Q"))) :to-be nil))))

(describe "org-canvas--diff-apply-new-quiz-assignments (issue #313)"
  (it "takes a New Quiz's assignment out of the Assignments extras and counts it"
    (let* ((assignments (list :name "Assignments" :remote-items nil
                              :extra (list '(:kind extra :title "Web Quiz" :id "43")
                                           '(:kind extra :title "Essay" :id "99"))))
           (quizzes (list :name "New Quizzes"
                          :remote-items '(((assignment_id . "43") (id . "7")))))
           (results (list assignments quizzes)))
      (org-canvas--diff-apply-new-quiz-assignments results)
      (expect (mapcar (lambda (e) (plist-get e :id)) (plist-get assignments :extra))
              :to-equal '("99"))
      (expect (plist-get assignments :covered) :to-equal 1)
      (expect (org-canvas--diff-suppressed-note results)
              :to-equal "Not checked: 1 Assignments (a New Quiz's assignment, checked under New Quizzes).\n")))

  (it "leaves the Assignments extras alone when New Quizzes were not read"
    (let* ((assignments (list :name "Assignments" :remote-items nil
                              :extra (list '(:kind extra :title "Web Quiz" :id "43"))))
           (results (list assignments
                          (list :name "New Quizzes" :error "Connection refused"))))
      (org-canvas--diff-apply-new-quiz-assignments results)
      (expect (length (plist-get assignments :extra)) :to-equal 1)
      (expect (plist-get assignments :covered) :to-be nil)))

  (it "runs as part of the report"
    (with-org-canvas-test-config
      (let ((org-canvas-diff-scan-references nil))
        (cl-letf (((symbol-function 'org-canvas--diff-feature)
                   (lambda (feature)
                     (pcase (plist-get feature :name)
                       ("Assignments"
                        (list :name "Assignments" :remote-items nil
                              :extra (list (list :kind 'extra :title "Web Quiz"
                                                 :id "43"))))
                       ("New Quizzes"
                        (list :name "New Quizzes"
                              :remote-items '(((assignment_id . "43")))
                              :extra (list (list :kind 'extra :title "Web Quiz"
                                                 :id "43"))))
                       (name (list :name name))))))
          (let* ((results (org-canvas--diff-collect-results))
                 (assignments (org-canvas--diff-result-named results "Assignments")))
            (expect (plist-get assignments :extra) :to-be nil)
            (expect (org-canvas--diff-count results) :to-equal 1)))))))

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

(provide 'org-canvas-diff-test)
;;; org-canvas-diff-test.el ends here
