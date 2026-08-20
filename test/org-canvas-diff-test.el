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
            :to-equal 10)))

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
                  (expect (plist-get (org-canvas--diff-feature
                                      (org-canvas--registry-find-feature "pages"))
                                     :extra)
                          :to-be nil)))))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file)))))

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

(provide 'org-canvas-diff-test)
;;; org-canvas-diff-test.el ends here
