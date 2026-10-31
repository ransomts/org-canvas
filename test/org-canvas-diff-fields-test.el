;;; org-canvas-diff-fields-test.el --- Drift report field comparison specs  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; lisp/org-canvas-diff-fields.el: the property and body comparison, the
;; referenced-media scan, the EXTRA details pass and the New Quiz
;; assignments pass.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

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

(describe "org-canvas--diff-html-to-text (issue #510)"
  (it "decodes every named entity ox-html emits, not six of them"
    (expect (org-canvas--diff-decode-entity "&hellip;") :to-equal "…")
    (expect (org-canvas--diff-decode-entity "&rsquo;") :to-equal "’")
    (expect (org-canvas--diff-decode-entity "&mdash;") :to-equal "—")
    (expect (org-canvas--diff-decode-entity "&Aacute;") :to-equal "Á"))

  (it "compares an exported ellipsis against Canvas's character"
    (expect (org-canvas--diff-html-to-text "a &hellip; b &rsquo; c &mdash; d")
            :to-equal (org-canvas--diff-html-to-text "a … b ’ c — d"))))

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

(describe "body registry declarations (issue #83)"
  (it "name the Canvas field for every module that pushes a body"
    (dolist (pair '(("assignments" . "description") ("pages" . "body")
                    ("announcements" . "message") ("discussions" . "message")
                    ("quizzes" . "description")))
      (expect (plist-get (gethash (car pair) org-canvas--registry) :body-api-key)
              :to-equal (cdr pair))))

  (it "leave modules without a body alone"
    (expect (plist-get (gethash "modules" org-canvas--registry) :body-api-key)
            :to-be nil)))

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

;;; org-canvas-diff-fields-test.el ends here
