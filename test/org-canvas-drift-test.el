;;; org-canvas-drift-test.el --- Tests for showing and adopting drift -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Covers `org-canvas-drift' (issues #498, #500): adopting what a drift
;; row names into its heading and nothing else, appending an EXTRA
;; override row to its table, and showing the Canvas version of a
;; heading in full without writing.  Also the drift report's own
;; additions those verbs read: the title comparison, a silent boolean
;; compared against its declared default, and the Canvas item a CHANGED
;; row now carries.  No spec reaches the network: the comparison is
;; stubbed, the item read is mocked, and `with-mock-api' counts calls.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

;;;; Fixtures

(defconst test-drift--file
  (concat "#+TITLE: Assignments\n"
          "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:POINTS:     10\n:PUBLISHED: false\n"
          ":DUE_AT: <2026-10-01 Thu 23:59>\n:WANT_DOCUMENT_PROCESSOR: Turnitin\n"
          ":CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:PAYLOAD_HASH: aaa\n:END:\n"
          "#+NAME: overrides\n| Section | Due At |\n|---------+--------|\n"
          "| [[file:sections.org::*S1][S1]] |   <2026-10-02 Fri 23:59> |\n\n"
          "#+ATTR_LATEX: :align lr\n| a | 1 |\n|---+---|\n| b | 2 |\n\n"
          "Description text,\nhard wrapped.\n"
          "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 61\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n"
          ":PAYLOAD_HASH: bbb\n:END:\nLab two.\n** A part\nMore.\n"
          "* Lab 3\n:PROPERTIES:\n:CANVAS_ID: 62\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n"
          ":END:\nNo table here.\n"
          "* Lab 4\n:PROPERTIES:\n:CANVAS_ID: 63\n"
          ":RUBRIC_LINK: [[file:rubrics.org::*Python Basics][Python Basics]]\n:END:\n"
          "Do it.\n")
  "Four stamped assignment headings, the last with a rubric attached.")

(defconst test-drift--rubrics
  (concat "#+TITLE: Rubrics\n"
          "* Python Basics\n:PROPERTIES:\n:CANVAS_ID: 900\n:END:\n"
          "** Sources :5pt:\nEvaluate sources.\n"
          "| Rating | Points |\n|--------+--------|\n| Full   | 5 |\n| None   | 0 |\n"
          "** Argument :5pt:\n"
          "| Rating | Points |\n|--------+--------|\n| Full   | 5 |\n| None   | 0 |\n")
  "One two-criterion rubric worth ten points.")

(defmacro test-drift--with-file (&rest body)
  "Run BODY with the drift fixtures as the course's Org files.
DIR is the course directory; `org-canvas-assignments-file' holds
`test-drift--file' as assignments.org and `org-canvas-rubrics-file'
`test-drift--rubrics' as rubrics.org, so a link between them resolves.
FILE and RUBRICS-FILE are bound to their paths.  The clean-heading
check is stubbed to say no, so no spec hashes a heading unless it asks
to."
  (declare (indent 0))
  `(with-test-course (dir :files (list (cons "assignments.org" test-drift--file)
                                      (cons "rubrics.org" test-drift--rubrics)))
     (let ((file (expand-file-name "assignments.org" dir))
           (rubrics-file (expand-file-name "rubrics.org" dir)))
       (cl-letf (((symbol-function 'org-canvas--publish-heading-clean-p)
                  (lambda (_spec) nil)))
         ,@body))))

(defun test-drift--text (file)
  "Return FILE's text, read from disk."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun test-drift--prop (file title property)
  "Return PROPERTY of heading TITLE in FILE, read from disk."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (concat "^\\* " (regexp-quote title) "$"))
    (org-entry-get (point) property)))

(defun test-drift--show (title item)
  "Return the show text of assignment heading TITLE against Canvas ITEM."
  (with-current-buffer (org-canvas--find-file-noselect org-canvas-assignments-file)
    (goto-char (point-min))
    (re-search-forward (concat "^\\* " (regexp-quote title) "$"))
    (org-canvas--drift-show-text (org-canvas--diff-find-feature "Assignments") item)))

(defconst test-drift--item
  '((id . 60) (name . "Lab 1 renamed") (points_possible . 12) (published . t)
    (due_at) (description . "<p>Canvas wrote this.</p>")
    (updated_at . "2026-10-14T15:04:51Z"))
  "Lab 1 as Canvas holds it.")

(defconst test-drift--rubric-item
  '((id . 63) (name . "Lab 4") (points_possible . 10) (published . t)
    (due_at) (description . "")
    (rubric_settings . ((id . 900) (title . "Python Basics") (points_possible . 10)))
    (rubric . [((id . 11) (description . "Sources") (points . 5))
               ((id . 12) (description . "Argument") (points . 5))]))
  "Lab 4 as Canvas holds it, with `test-drift--rubrics' attached.")

(defun test-drift--rubric-item-with (&rest overrides)
  "Return `test-drift--rubric-item' with OVERRIDES, (KEY VALUE) pairs, set.
A `title' or `points_possible' lands in its `rubric_settings'."
  (let ((item (copy-tree test-drift--rubric-item)))
    (while overrides
      (let ((key (pop overrides)) (value (pop overrides)))
        (if (memq key '(title points_possible))
            (setf (alist-get key (alist-get 'rubric_settings item)) value)
          (setf (alist-get key item) value))))
    item))

(defun test-drift--entry (&rest fields)
  "Return Lab 1's CHANGED entry naming FIELDS, its item `test-drift--item'."
  (list :kind 'modified :title "Lab 1" :id "60" :remote-newer t
        :updated "2026-10-14T15:04:51Z" :fields fields :item test-drift--item))

(defun test-drift--row (entry &optional feature)
  "Return a report row for ENTRY of FEATURE (default Assignments)."
  (list :feature (or feature "Assignments") :entry entry))

(defun test-drift--report-buffer (results)
  "Render RESULTS the way `org-canvas-diff' does and return the buffer."
  (with-org-canvas-test-config
    (let ((noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                ((symbol-function 'org-canvas--diff-feature)
                 (lambda (feature)
                   (or (cl-find (plist-get feature :name) results
                                :key (lambda (r) (plist-get r :name)) :test #'string=)
                       (list :name (plist-get feature :name))))))
        (org-canvas-diff)
        (get-buffer org-canvas--diff-buffer-name)))))

(defun test-drift--goto-row (kind)
  "Put point on the first row of KIND in the current buffer."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((row (get-text-property (point) 'org-canvas-diff-row)))
        (if (and row (eq (plist-get (plist-get row :entry) :kind) kind))
            (setq found t)
          (forward-line 1))))
    (unless found (error "No %s row" kind))))

(defun test-drift--expect-no-writes ()
  "Expect no recorded call to have been a write.
A read may still be a GraphQL query, sent as a POST to /api/graphql."
  (dolist (call test-org-canvas-api-calls)
    (expect (or (eq (car call) 'GET)
                (string-match-p "/api/graphql\\'" (cadr call)))
            :to-be-truthy)))

;;;; The report's additions

(describe "org-canvas--diff-compare-default (issue #498)"
  (let ((spec '(:org-prop "PUBLISHED" :data-key :published :type boolean :default t)))
    (it "holds a silent heading's declared default against Canvas"
      (expect (org-canvas--diff-compare-default spec '((published . :json-false)))
              :to-equal '("PUBLISHED" "(unset: true)" "false")))

    (it "is silent when Canvas holds the default, or did not send the field"
      (expect (org-canvas--diff-compare-default spec '((published . t))) :to-be nil)
      (expect (org-canvas--diff-compare-default spec '((id . 1))) :to-be nil))

    (it "leaves out a boolean with no declared default, or one that inherits"
      (expect (org-canvas--diff-compare-default
               '(:org-prop "MUTED" :data-key :muted :type boolean)
               '((muted . t)))
              :to-be nil)
      (expect (org-canvas--diff-compare-default
               '(:org-prop "PUBLISHED" :data-key :published :type boolean
                 :default t :absent-inherits t)
               '((published . :json-false)))
              :to-be nil)
      (expect (org-canvas--diff-declared-default
               '(:org-prop "X" :data-key :x :type boolean :default nil))
              :to-equal "false")))

  (it "reaches the field comparison of a heading that does not write PUBLISHED"
    (with-temp-org-buffer "* Lab\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (with-mock-api
       (let ((specs (plist-get (org-canvas--diff-find-properties "Assignments")
                              :properties)))
        (expect (assoc "PUBLISHED" (org-canvas--diff-compare-fields
                                    specs (point) '((id . 1) (published . :json-false))))
                :to-equal '("PUBLISHED" "(unset: true)" "false")))))))

(describe "org-canvas--diff-compare-title (issue #498)"
  (let ((props '(:compare-title t)))
    (it "names a title Canvas holds otherwise"
      (expect (org-canvas--diff-compare-title
               props '(:heading "Midterm Exam" :match-title "Midterm Exam")
               '((name . "Midterm Exam MCQ")) 'name)
              :to-equal '("TITLE" "Midterm Exam" "Midterm Exam MCQ")))

    (it "agrees across whitespace, and leaves a link heading out"
      (expect (org-canvas--diff-compare-title
               props '(:heading "Midterm  Exam" :match-title "Midterm  Exam")
               '((name . " Midterm Exam ")) 'name)
              :to-be nil)
      (expect (org-canvas--diff-compare-title
               props '(:heading "[[file:a.org::*X][X]]" :match-title "X")
               '((name . "Y")) 'name)
              :to-be nil)))

  (it "compares nothing for a module that does not declare it"
    (expect (org-canvas--diff-compare-title
             nil '(:heading "A" :match-title "A") '((name . "B")) 'name)
            :to-be nil))

  (it "is declared by the modules whose push sends the heading as the title"
    (dolist (name '("Assignments" "Pages" "Quizzes" "Discussions" "Announcements"))
      (expect (plist-get (org-canvas--diff-find-properties name) :compare-title)
              :to-be t))))

(describe "org-canvas--diff-entry (issues #498, #500)"
  (it "keeps the Canvas item on a CHANGED row and puts a rename first"
    (with-temp-org-buffer "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 61\n:POINTS: 10\n:END:\n"
      (with-mock-api
       (let* ((index (org-canvas--diff-remote-index
                     '(((id . 61) (name . "Lab One") (points_possible . 12))) 'id))
             (props (org-canvas--diff-find-properties "Assignments"))
             (entry (append (org-canvas--diff-local-entry "CANVAS_ID")
                            (list :pom (point-marker))))
             (d (org-canvas--diff-entry
                 entry index
                 (org-canvas--diff-options
                  :specs (plist-get props :properties)
                  :props (plist-put (copy-sequence props) :body-api-key nil)
                  :title-field 'name))))
        (expect (car (plist-get d :fields)) :to-equal '("TITLE" "Lab 1" "Lab One"))
        (expect (assoc "POINTS" (plist-get d :fields)) :to-equal '("POINTS" "10" "12"))
        (expect (alist-get 'name (plist-get d :item)) :to-equal "Lab One"))))))

;;;; Adopting a CHANGED row

(describe "org-canvas--drift-adopt-changed (issue #498)"
  (it "writes exactly the fields the row names, the stamp, and nothing else"
    (test-drift--with-file
      (let* ((before (test-drift--text file))
             (outcome (with-mock-api
                        (prog1 (org-canvas--drift-adopt-changed
                                (org-canvas--diff-find-feature "Assignments")
                                (test-drift--entry '("TITLE" "Lab 1" "Lab 1 renamed")
                                                   '("POINTS" "10" "12")
                                                   '("PUBLISHED" "false" "true")
                                                   '("DUE_AT" "<2026-10-01 Thu 23:59>" "(unset)"))
                                nil)
                          (expect (test-org-canvas-api-call-count) :to-equal 0))))
             (after (test-drift--text file)))
        (expect (plist-get outcome :outcome) :to-be 'adopted)
        (expect (plist-get outcome :adopted) :to-equal '("TITLE" "POINTS" "PUBLISHED" "DUE_AT"))
        (expect (test-drift--prop file "Lab 1 renamed" "POINTS") :to-equal "12")
        ;; Written out, though true is the default a pull would delete.
        (expect (test-drift--prop file "Lab 1 renamed" "PUBLISHED") :to-equal "true")
        (expect (test-drift--prop file "Lab 1 renamed" "DUE_AT") :to-be nil)
        (expect (test-drift--prop file "Lab 1 renamed" "CANVAS_UPDATED_AT")
                :to-equal "2026-10-14T15:04:51Z")
        (expect (test-drift--prop file "Lab 1 renamed" "PAYLOAD_HASH") :to-equal "aaa")
        (expect (test-drift--prop file "Lab 1 renamed" "WANT_DOCUMENT_PROCESSOR")
                :to-equal "Turnitin")
        ;; The body, its tables and the other headings are byte for byte.
        (let ((tail (substring before (string-match "#\\+NAME: overrides" before))))
          (expect (string-suffix-p tail after) :to-be t)))))

  (it "rehashes a heading that was what its last push sent"
    (test-drift--with-file
      (cl-letf (((symbol-function 'org-canvas--publish-heading-clean-p) (lambda (_) t))
                ((symbol-function 'org-canvas--sync-heading-hash) (lambda (_) "new")))
        (org-canvas--drift-adopt-changed
         (org-canvas--diff-find-feature "Assignments")
         (test-drift--entry '("POINTS" "10" "12")) nil))
      (expect (test-drift--prop file "Lab 1" "PAYLOAD_HASH") :to-equal "new")))

  (it "drops the hash of a clean heading the push cannot hash any more"
    (test-drift--with-file
      (cl-letf (((symbol-function 'org-canvas--publish-heading-clean-p) (lambda (_) t))
                ((symbol-function 'org-canvas--sync-heading-hash) (lambda (_) nil)))
        (org-canvas--drift-adopt-changed
         (org-canvas--diff-find-feature "Assignments")
         (test-drift--entry '("POINTS" "10" "12")) nil))
      (expect (test-drift--prop file "Lab 1" "PAYLOAD_HASH") :to-be nil)))

  (it "holds the body back unless asked, and then keeps the old stamp"
    (test-drift--with-file
      (let ((outcome (org-canvas--drift-adopt-changed
                      (org-canvas--diff-find-feature "Assignments")
                      (test-drift--entry '("POINTS" "10" "12")
                                         '("DESCRIPTION" "Description…" "Canvas…"))
                      nil)))
        (expect (plist-get outcome :outcome) :to-be 'partial)
        (expect (car (car (plist-get outcome :held))) :to-equal "DESCRIPTION")
        (expect (cdr (car (plist-get outcome :held))) :to-match "not asked")
        (expect (test-drift--prop file "Lab 1" "POINTS") :to-equal "12")
        (expect (test-drift--prop file "Lab 1" "CANVAS_UPDATED_AT")
                :to-equal "2026-09-01T00:00:00Z")
        (expect (test-drift--text file) :to-match "hard wrapped"))))

  (it "replaces only the body when asked, keeping the overrides table"
    (test-drift--with-file
      (cl-letf (((symbol-function 'org-canvas--html-to-org-with-rewrite)
                 (lambda (html) (expect html :to-equal "<p>Canvas wrote this.</p>")
                   "Canvas wrote this.")))
        (let ((outcome (org-canvas--drift-adopt-changed
                        (org-canvas--diff-find-feature "Assignments")
                        (test-drift--entry '("DESCRIPTION" "Description…" "Canvas…"))
                        t)))
          (expect (plist-get outcome :outcome) :to-be 'adopted)))
      (let ((text (test-drift--text file)))
        (expect text :to-match "#\\+NAME: overrides\n| Section | Due At |")
        (expect text :to-match "|   <2026-10-02 Fri 23:59> |\n\nCanvas wrote this.\n\\* Lab 2")
        (expect text :not :to-match "hard wrapped")
        (expect text :to-match "Lab two.\n\\*\\* A part"))
      (expect (test-drift--prop file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-10-14T15:04:51Z")))

  (it "refuses a body whose heading has sub-headings, or a module reading it its own way"
    (test-drift--with-file
      (let* ((feature (org-canvas--diff-find-feature "Assignments"))
             (entry (list :kind 'modified :title "Lab 2" :id "61" :remote-newer t
                          :updated "2026-10-14T15:04:51Z"
                          :fields '(("DESCRIPTION" "Lab two." "x"))
                          :item '((id . 61) (description . "<p>x</p>"))))
             (outcome (org-canvas--drift-adopt-changed feature entry t)))
        (expect (cdr (car (plist-get outcome :held))) :to-match "sub-headings")
        (expect (test-drift--text file) :to-match "Lab two.")))
    (with-temp-buffer
      (expect (org-canvas--drift-adopt-body '(:body-api-key "description" :body-fn ignore)
                                            nil t)
              :to-match "its own way")))

  (it "never adopts a declared intent or a field no spec registers"
    (test-drift--with-file
      (let ((outcome (org-canvas--drift-adopt-changed
                      (org-canvas--diff-find-feature "Assignments")
                      (test-drift--entry '("WANT_DOCUMENT_PROCESSOR" "Turnitin" "(none)")
                                         '("BOGUS" "1" "2"))
                      nil)))
        (expect (plist-get outcome :outcome) :to-be 'partial)
        (expect (mapcar #'car (plist-get outcome :held))
                :to-equal '("WANT_DOCUMENT_PROCESSOR" "BOGUS"))
        (expect (cdr (car (plist-get outcome :held))) :to-match "declaration")
        (expect (test-drift--prop file "Lab 1" "WANT_DOCUMENT_PROCESSOR")
                :to-equal "Turnitin"))))

  (it "adopts only the stamp of a row with nothing compared differing"
    (test-drift--with-file
      (let ((outcome (org-canvas--drift-adopt-changed
                      (org-canvas--diff-find-feature "Assignments")
                      (list :kind 'modified :title "Lab 2" :id "61" :remote-newer t
                            :updated "2026-10-14T15:04:51Z" :fields nil)
                      nil)))
        (expect (plist-get outcome :outcome) :to-be 'stamped)
        (expect (test-drift--prop file "Lab 2" "CANVAS_UPDATED_AT")
                :to-equal "2026-10-14T15:04:51Z")
        (expect (test-drift--prop file "Lab 2" "PAYLOAD_HASH") :to-equal "bbb"))))

  (it "writes nothing under a dry run, nor without a heading or a Canvas copy"
    (test-drift--with-file
      (let* ((feature (org-canvas--diff-find-feature "Assignments"))
             (before (test-drift--text file))
             (dry (let ((org-canvas--dry-run t))
                    (org-canvas--drift-adopt-changed
                     feature (test-drift--entry '("POINTS" "10" "12")) nil)))
             (ghost (org-canvas--drift-adopt-changed
                     feature (list :kind 'modified :title "Ghost" :id "999"
                                   :fields '(("POINTS" "1" "2")))
                     nil))
             (bare (org-canvas--drift-adopt-changed
                    feature (list :kind 'modified :title "Lab 1" :id "60"
                                  :fields '(("POINTS" "10" "12")))
                    nil)))
        (expect (plist-get dry :outcome) :to-be 'dry-run)
        (expect (plist-get dry :adopted) :to-equal '("POINTS"))
        (expect (plist-get (let ((org-canvas--dry-run t))
                             (org-canvas--drift-adopt-changed
                              feature (list :kind 'modified :title "Lab 2" :id "61"
                                            :remote-newer t :fields nil
                                            :updated "2026-10-14T15:04:51Z")
                              nil))
                           :adopted)
                :to-equal '("CANVAS_UPDATED_AT"))
        (expect (plist-get ghost :outcome) :to-be 'not-found)
        (expect (plist-get bare :outcome) :to-be 'refused)
        (expect (test-drift--text file) :to-equal before)))))

;;;; Appending an override row

(defmacro test-drift--with-override-cells (&rest body)
  "Run BODY with the override cell writers stubbed to fixed text."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-canvas--override-target-cell)
              (lambda (_ov) "[[file:sections.org::*S2][S2]]"))
             ((symbol-function 'org-canvas--override-format-cell)
              (lambda (iso) (if iso "<2026-10-20 Tue 23:59>" ""))))
     ,@body))

(defun test-drift--override-entry (assignment-id where ov)
  "Return an EXTRA override entry of ASSIGNMENT-ID's heading WHERE, for OV."
  (list :kind 'override :state 'extra :assignment-id assignment-id
        :where where :container "assignment" :file org-canvas-assignments-file
        :title "S2" :id "900" :override ov))

(describe "org-canvas--drift-append-override (issue #498)"
  (it "appends the row after the table's last, leaving the others as they are"
    (test-drift--with-file
      (test-drift--with-override-cells
        (let ((outcome (org-canvas--drift-append-override
                        (test-drift--override-entry
                         "60" "Lab 1" '((id . 900) (course_section_id . 2)
                                        (due_at . "2026-10-21T03:59:00Z"))))))
          (expect (plist-get outcome :outcome) :to-be 'appended)
          (expect (test-drift--text file)
                  :to-match (regexp-quote "| [[file:sections.org::*S1][S1]] |   <2026-10-02 Fri 23:59> |\n| [[file:sections.org::*S2][S2]] | <2026-10-20 Tue 23:59> |\n\n#+ATTR_LATEX"))))))

  (it "refuses a date the table has no column for"
    (test-drift--with-file
      (test-drift--with-override-cells
        (let* ((before (test-drift--text file))
               (outcome (org-canvas--drift-append-override
                         (test-drift--override-entry
                          "60" "Lab 1" '((id . 900) (course_section_id . 2)
                                         (lock_at . "2026-10-21T03:59:00Z"))))))
          (expect (plist-get outcome :outcome) :to-be 'refused)
          (expect (plist-get outcome :reason) :to-match "no Lock At column")
          (expect (test-drift--text file) :to-equal before)))))

  (it "writes a table under the drawer of a heading that has none"
    (test-drift--with-file
      (test-drift--with-override-cells
        (expect (plist-get (org-canvas--drift-append-override
                            (test-drift--override-entry
                             "62" "Lab 3" '((id . 900) (course_section_id . 2)
                                            (due_at . "2026-10-21T03:59:00Z"))))
                           :outcome)
                :to-be 'appended)
        (expect (test-drift--text file)
                :to-match (regexp-quote ":END:\n#+NAME: overrides\n| Section | Due At |")))))

  (it "refuses to start a table for an override that carries no date"
    (test-drift--with-file
      (test-drift--with-override-cells
        (expect (plist-get (org-canvas--drift-append-override
                            (test-drift--override-entry "62" "Lab 3" '((id . 901))))
                           :outcome)
                :to-be 'refused))))

  (it "writes nothing under a dry run, nor for a heading it cannot find"
    (test-drift--with-file
      (let ((before (test-drift--text file)))
        (expect (plist-get (let ((org-canvas--dry-run t))
                             (org-canvas--drift-append-override
                              (test-drift--override-entry "60" "Lab 1" '((id . 900)))))
                           :outcome)
                :to-be 'dry-run)
        (expect (plist-get (org-canvas--drift-append-override
                            (test-drift--override-entry "999" "Ghost" '((id . 900))))
                           :outcome)
                :to-be 'not-found)
        (expect (test-drift--text file) :to-equal before))))

  (it "places a date in the column its title names, whatever the order"
    (test-drift--with-override-cells
      (expect (org-canvas--drift-override-cells
               '((lock_at . "x") (due_at . "y")) '("Section" "Lock At" "Due At"))
              :to-equal '("[[file:sections.org::*S2][S2]]" "<2026-10-20 Tue 23:59>"
                          "<2026-10-20 Tue 23:59>"))
      (expect (org-canvas--drift-header '(hline ("Section" "Due At")))
              :to-equal '("Section" "Due At")))))

;;;; Rewriting a CHANGED override row

(defmacro test-drift--with-section-rows (&rest body)
  "Run BODY with a table cell naming Sn read as section n."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-canvas--override-parse-target)
              (lambda (cell _dir)
                (and (string-match "\\*S\\([0-9]+\\)\\]" cell)
                     (list :section-id (match-string 1 cell))))))
     (test-drift--with-override-cells ,@body)))

(defun test-drift--changed-override (assignment-id where ov &rest names)
  "Return a CHANGED override entry of ASSIGNMENT-ID's heading WHERE, for OV.
NAMES are the columns the row names as differing."
  (list :kind 'override :state 'changed :assignment-id assignment-id
        :where where :container "assignment" :file org-canvas-assignments-file
        :title "S1" :id "800" :override ov
        :fields (mapcar (lambda (n) (list n "<old>" "<new>")) names)))

(describe "org-canvas--drift-rewrite-override (issue #528)"
  (it "rewrites the dates the row names in the row claiming the override"
    (test-drift--with-file
      (test-drift--with-section-rows
        (let ((outcome (org-canvas--drift-rewrite-override
                        (test-drift--changed-override
                         "60" "Lab 1" '((id . 800) (course_section_id . 1)
                                        (due_at . "2026-10-21T03:59:00Z"))
                         "Due At"))))
          (expect (plist-get outcome :outcome) :to-be 'rewritten)
          (expect (plist-get outcome :adopted) :to-equal '("Due At"))
          (expect (org-canvas--drift-outcome-line outcome)
                  :to-match "ADOPTED   S1 (Lab 1) (Due At rewritten in the overrides table, nothing sent)")
          (expect (test-drift--text file)
                  :to-match (regexp-quote "|---------+--------|\n| [[file:sections.org::*S1][S1]] | <2026-10-20 Tue 23:59> |\n\n#+ATTR_LATEX"))))))

  (it "writes only the named dates, and empties one Canvas cleared"
    (test-drift--with-override-cells
      (let ((header '("Section" "Due At" "Lock At")))
        (expect (org-canvas--drift-date-cells
                 '("S1" "a" "b") '((due_at . "x") (lock_at . "y")) header '("Lock At"))
                :to-equal '("S1" "a" "<2026-10-20 Tue 23:59>"))
        (expect (org-canvas--drift-date-cells '("S1" "a" "b") '((lock_at . "y")) header '("Due At"))
                :to-equal '("S1" "" "b")))))

  (it "refuses a row no table row claims, a missing column, and a heading without a table"
    (test-drift--with-file
      (test-drift--with-section-rows
        (let ((before (test-drift--text file)))
          (expect (plist-get (org-canvas--drift-rewrite-override
                              (test-drift--changed-override
                               "60" "Lab 1" '((id . 800) (course_section_id . 9)) "Due At"))
                             :reason)
                  :to-match "no row of the table claims the override")
          (expect (plist-get (org-canvas--drift-rewrite-override
                              (test-drift--changed-override
                               "60" "Lab 1" '((id . 800) (course_section_id . 1)
                                              (lock_at . "2026-10-21T03:59:00Z"))
                               "Lock At"))
                             :reason)
                  :to-match "no Lock At column")
          (expect (plist-get (org-canvas--drift-rewrite-override
                              (test-drift--changed-override
                               "62" "Lab 3" '((id . 800) (course_section_id . 1)) "Due At"))
                             :reason)
                  :to-match "has no overrides table")
          (expect (test-drift--text file) :to-equal before)))))

  (it "names what it would write under a dry run, and writes nothing"
    (test-drift--with-file
      (let* ((before (test-drift--text file))
             (outcome (let ((org-canvas--dry-run t))
                        (org-canvas--drift-rewrite-override
                         (test-drift--changed-override
                          "60" "Lab 1" '((id . 800)) "Due At" "Lock At")))))
        (expect (org-canvas--drift-outcome-line outcome)
                :to-match "DRY-RUN   S1 (Lab 1) (would write Due At, Lock At)")
        (expect (test-drift--text file) :to-equal before))))

  (it "adopts CHANGED override rows in batch and holds a MISSING one, counting it left"
    (test-drift--with-file
      (test-drift--with-section-rows
        (let (report outcomes)
          (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                    ((symbol-function 'org-canvas--report-display)
                     (lambda (_name render) (setq report (with-temp-buffer
                                                           (funcall render)
                                                           (buffer-string))))))
            (setq outcomes
                  (org-canvas-diff-adopt-drifts
                   'all :overrides t
                   :results `((:name "Assignment Overrides"
                               :divergences
                               (,(test-drift--changed-override
                                  "60" "Lab 1" '((id . 800) (course_section_id . 1)
                                                 (due_at . "2026-10-21T03:59:00Z"))
                                  "Due At")
                                (:kind override :state missing :title "S3" :where "Lab 2"
                                 :assignment-id "61")))))))
          (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                  :to-equal '(rewritten refused))
          (expect report :to-match "HELD      S3 (Lab 2) (Canvas holds no such override")
          (expect report :to-match "1 row(s) adopted, 1 left"))))))

;;;; The row verb

(describe "org-canvas-diff-adopt-drift (issue #498)"
  (it "is bound to A and = in the report, and the legend names them"
    (expect (lookup-key org-canvas-diff-mode-map (kbd "A")) :to-be #'org-canvas-diff-adopt-drift)
    (expect (lookup-key org-canvas-diff-mode-map (kbd "=")) :to-be #'org-canvas-diff-show-row)
    (expect org-canvas--diff-key-legend :to-match "A adopt drift   = show in full"))

  (it "adopts the CHANGED row at point and rewrites the whole row"
    (test-drift--with-file
      (with-mock-api
        (with-current-buffer (test-drift--report-buffer
                              `((:name "Assignments"
                                 :divergences (,(test-drift--entry '("POINTS" "10" "12")
                                                                   '("PUBLISHED" "false" "true"))))))
          (test-drift--goto-row 'modified)
          (let ((noninteractive nil))
            (org-canvas-diff-adopt-drift))
          (expect (thing-at-point 'line t)
                  :to-match "ADOPTED   Lab 1 (POINTS, PUBLISHED written from Canvas; CANVAS_UPDATED_AT set to 2026-10-14T15:04:51Z, nothing sent)")
          (expect (get-text-property (point) 'org-canvas-diff-row) :to-be-truthy)
          (forward-line 1)
          ;; The field lines went with the row.
          (expect (thing-at-point 'line t) :not :to-match "POINTS"))
        (expect (test-org-canvas-api-call-count) :to-equal 0))
      (expect (test-drift--prop file "Lab 1" "POINTS") :to-equal "12")))

  (it "asks before replacing a body, interactively"
    (test-drift--with-file
      (let ((asked nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (q) (setq asked q) t))
                  ((symbol-function 'org-canvas--html-to-org-with-rewrite)
                   (lambda (_) "Canvas wrote this.")))
          (with-current-buffer (test-drift--report-buffer
                                `((:name "Assignments"
                                   :divergences (,(test-drift--entry
                                                   '("DESCRIPTION" "Description…" "Canvas…"))))))
            (test-drift--goto-row 'modified)
            (let ((noninteractive nil))
              (org-canvas-diff-adopt-drift))))
        (expect asked :to-match "Replace the body of 'Lab 1'")
        (expect (test-drift--text file) :to-match "Canvas wrote this."))))

  (it "appends an EXTRA override row from the report"
    (test-drift--with-file
      (test-drift--with-override-cells
        (with-current-buffer (test-drift--report-buffer
                              `((:name "Assignments"
                                 :children
                                 (:name "Assignment Overrides"
                                  :extra (,(test-drift--override-entry
                                           "60" "Lab 1" '((id . 900) (course_section_id . 2)
                                                          (due_at . "2026-10-21T03:59:00Z"))))))))
          (test-drift--goto-row 'override)
          (org-canvas-diff-adopt-drift)
          (expect (thing-at-point 'line t)
                  :to-match "ADOPTED   S2 (Lab 1) (row appended to the overrides table")))))

  (it "refuses a row with no Canvas side to adopt, and a New Quiz item"
    (with-current-buffer (test-drift--report-buffer
                          '((:name "Assignments" :extra ((:kind extra :title "Stray" :id "99")))))
      (test-drift--goto-row 'extra)
      (expect (org-canvas-diff-adopt-drift) :to-throw 'user-error))
    (expect (org-canvas--drift-adopt-row
             (test-drift--row '(:kind modified :title "Q1" :id "5" :quiz-id "7")) nil)
            :to-throw 'user-error)
    (expect (org-canvas--drift-adopt-row
             (test-drift--row '(:kind override :state missing :title "S1")) nil)
            :to-throw 'user-error)))

(describe "org-canvas--drift-outcome-line (issue #498)"
  (it "names each outcome"
    (expect (org-canvas--drift-outcome-line
             '(:title "T" :outcome stamped :updated "U"))
            :to-match "ADOPTED   T (CANVAS_UPDATED_AT set to U")
    (expect (org-canvas--drift-outcome-line
             '(:title "T" :outcome partial :adopted ("POINTS")
               :held (("DESCRIPTION" . "not asked"))))
            :to-match "PARTIAL   T (POINTS written; DESCRIPTION held: not asked; CANVAS_UPDATED_AT kept")
    (expect (org-canvas--drift-outcome-line
             '(:title "T" :outcome partial :held (("X" . "why"))))
            :to-match "(nothing written; X held: why")
    (expect (org-canvas--drift-outcome-line '(:title "T" :outcome dry-run :adopted ("A")))
            :to-match "DRY-RUN   T (would write A)")
    (expect (org-canvas--drift-outcome-line '(:title "T" :outcome dry-run))
            :to-match "would write the table row")
    (expect (org-canvas--drift-outcome-line '(:title "T" :outcome not-found))
            :to-match "NOT FOUND T")
    (expect (org-canvas--drift-outcome-line '(:title "T" :outcome failed :reason "boom"))
            :to-match "HELD      T (boom)")
    (expect (org-canvas--drift-outcome-line '(:title "T" :outcome adopted :adopted ("A")))
            :to-match "CANVAS_UPDATED_AT set to (none)")))

;;;; Adopting in batch

(describe "org-canvas-diff-adopt-drifts (issue #498)"
  (defun test-drift--results ()
    "Return a report holding Lab 1's CHANGED row and Lab 1's EXTRA override."
    `((:name "Assignments"
       :divergences (,(test-drift--entry '("POINTS" "10" "12"))
                     (:kind missing :title "Gone" :id "70")))
      (:name "Assignment Overrides"
       :extra (,(test-drift--override-entry
                 "60" "Lab 1" '((id . 900) (course_section_id . 2)
                                (due_at . "2026-10-21T03:59:00Z")))
               ,(test-drift--override-entry "62" "Lab 3" '((id . 901)))))
      (:name "Module Items"
       :divergences ((:kind modified :title "Item" :id "5")))))

  (defun test-drift--adopt (selectors &rest keys)
    "Run the batch adoption of SELECTORS with KEYS; return (OUTCOMES . REPORT)."
    (let (report)
      (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                ((symbol-function 'org-canvas--report-display)
                 (lambda (_name render) (setq report (with-temp-buffer
                                                       (funcall render)
                                                       (buffer-string))))))
        (cons (apply #'org-canvas-diff-adopt-drifts selectors
                     :results (test-drift--results) keys)
              report))))

  (it "adopts a heading named by title, with its override rows when asked"
    (test-drift--with-file
      (test-drift--with-override-cells
        (let* ((out (test-drift--adopt '(("assignment" . "Lab 1")) :overrides t))
               (outcomes (car out)))
          (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                  :to-equal '(adopted appended))
          (expect (cdr out) :to-match "2 row(s) adopted, 0 left; nothing sent to Canvas")))
      (expect (test-drift--prop file "Lab 1" "POINTS") :to-equal "12")
      (expect (test-drift--text file) :to-match "S2\\]\\] | <2026-10-20")))

  (it "names a heading by id, and one the report has no row for"
    (test-drift--with-file
      (let ((outcomes (car (test-drift--adopt '(("assignments" "60" canvas-id)
                                                ("assignment" "Nope" title))))))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(adopted not-found))
        (expect (plist-get (nth 1 outcomes) :reason) :to-match "no CHANGED row"))))

  (it "takes an override row of a heading by its id when the heading has no CHANGED row"
    (test-drift--with-file
      (test-drift--with-override-cells
        (let ((outcomes (car (test-drift--adopt '(("assignment" "62" canvas-id))
                                                :overrides t))))
          (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                  :to-equal '(refused))))))

  (it "adopts every CHANGED row a feature owns with all, never a module item"
    (test-drift--with-file
      (test-drift--with-override-cells
        (let ((outcomes (car (test-drift--adopt 'all :overrides t))))
          (expect (mapcar (lambda (o) (plist-get o :title)) outcomes)
                  :to-equal '("Lab 1" "S2 (Lab 1)" "S2 (Lab 3)"))))))

  (it "reads the report itself when given none"
    (test-drift--with-file
      (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                ((symbol-function 'org-canvas--report-display) #'ignore)
                ((symbol-function 'org-canvas--diff-collect-results) #'test-drift--results))
        (expect (mapcar (lambda (o) (plist-get o :outcome))
                        (org-canvas-diff-adopt-drifts '(("assignment" . "Lab 1"))))
                :to-equal '(adopted)))))

  (it "turns a failure into an outcome and goes on"
    (cl-letf (((symbol-function 'org-canvas--drift-adopt-row)
               (lambda (&rest _) (error "Boom"))))
      (let ((outcome (org-canvas--drift-adopt-one (test-drift--row '(:title "T")) nil)))
        (expect (plist-get outcome :outcome) :to-be 'failed)
        (expect (plist-get outcome :reason) :to-equal "Boom")))
    (expect (org-canvas--drift-adopt-one '(:title "T" :outcome not-found) nil)
            :to-equal '(:title "T" :outcome not-found)))

  (it "asks before adopting every row interactively, and not in batch"
    (let ((called nil))
      (cl-letf (((symbol-function 'org-canvas-diff-adopt-drifts)
                 (lambda (selectors &rest _) (setq called selectors) nil)))
        (org-canvas-diff-adopt-all-drifts)
        (expect called :to-be 'all)
        (setq called nil)
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
            (expect (org-canvas-diff-adopt-all-drifts) :to-throw 'user-error)))
        (expect called :to-be nil)))))

;;;; Showing the Canvas side

(describe "org-canvas--drift-show-text (issue #500)"
  (it "shows every compared property, the title and the body in full, starring what differs"
    (test-drift--with-file
      (cl-letf (((symbol-function 'org-canvas--html-to-org)
                 (lambda (_html) "Canvas wrote this.")))
        (let ((text (with-mock-api
                      (prog1 (with-current-buffer (org-canvas--find-file-noselect file)
                               (goto-char (point-min))
                               (re-search-forward "^\\* Lab 1$")
                               (org-canvas--drift-show-text
                                (org-canvas--diff-find-feature "Assignments")
                                test-drift--item))
                        (test-drift--expect-no-writes)))))
          (expect text :to-match "Assignments 'Lab 1' (CANVAS_ID 60): the Canvas version, nothing written")
          (expect text :to-match "Canvas updated 2026-10-14T15:04:51Z; the heading's baseline is 2026-09-01T00:00:00Z")
          (expect text :to-match "\\* TITLE +org: +Lab 1\n +canvas: Lab 1 renamed")
          (expect text :to-match "\\* POINTS +org: +10\n +canvas: 12")
          (expect text :to-match "\\* PUBLISHED +org: +false\n +canvas: true")
          (expect text :to-match "\\* WANT_DOCUMENT_PROCESSOR +org: +Turnitin")
          (expect text :to-match "\\* DESCRIPTION (the text differs)")
          (expect text :to-match "--- Canvas HTML ---\n<p>Canvas wrote this.</p>")
          (expect text :to-match "--- Canvas as Org [^\n]*\nCanvas wrote this.")
          (when (executable-find "diff")
            (expect text :to-match "^-hard wrapped.$")
            (expect text :to-match "^\\+Canvas wrote this.$"))
          (expect text :not :to-match "S1\\]\\] |")))
      ;; Nothing was written.
      (expect (test-drift--prop file "Lab 1" "POINTS") :to-equal "10")))

  (it "has no body section without a body, and says when the text agrees"
    (with-temp-org-buffer "* X\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\nSame words.\n"
      (expect (org-canvas--drift-show-body nil (point) '((id . 1))) :to-equal "")
      (cl-letf (((symbol-function 'org-canvas--html-to-org) (lambda (_) "Same words.")))
        (expect (org-canvas--drift-show-body
                 '(:body-api-key "description") (point-marker)
                 '((id . 1) (description . "<p>Same words.</p>")))
                :to-match "the same text; any difference is markup[^z]*(no difference)"))
      (cl-letf (((symbol-function 'org-canvas--diff-compare-body) (lambda (&rest _) nil)))
        (expect (org-canvas--drift-show-body
                 '(:body-api-key "description") (point-marker)
                 '((id . 1) (description . "")))
                :to-match "not compared"))))

  (it "says so when there is no diff program"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (expect (org-canvas--drift-unified-diff "a" "b") :to-match "no diff program")))

  (it "reads a quiz's body only up to its first question, tables left out"
    (with-temp-org-buffer "* Q\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n#+NAME: accommodations\n| a |\n\nIntro.\n** Question\nText.\n"
      (expect (org-canvas--drift-local-body-org '(:body-fn ignore)) :to-equal "Intro.")
      (expect (org-canvas--drift-local-body-org nil) :to-match "Intro.\n\\*\\* Question"))))

(describe "org-canvas--drift-show-rubric (issue #567)"
  (it "shows the rubric the heading links beside the one Canvas attaches"
    (test-drift--with-file
      (let ((text (test-drift--show "Lab 4" test-drift--rubric-item)))
        (expect text :to-match "RUBRIC_LINK +org: +\"Python Basics\" (id 900), 2 criteria, 10 points")
        (expect text :to-match "canvas: \"Python Basics\" (id 900), 2 criteria, 10 points")
        (expect text :not :to-match "\\* RUBRIC_LINK"))))

  (it "stars a renamed rubric, or one worth other points"
    (test-drift--with-file
      (let ((text (test-drift--show
                   "Lab 4" (test-drift--rubric-item-with
                            'title "Python Basics v2" 'points_possible 12))))
        (expect text :to-match "\\* RUBRIC_LINK +org: +\"Python Basics\" (id 900), 2 criteria, 10 points")
        (expect text :to-match "canvas: \"Python Basics v2\" (id 900), 2 criteria, 12 points"))))

  (it "shows a rubric Canvas attaches to a heading that links none"
    (test-drift--with-file
      (let ((text (test-drift--show "Lab 1" test-drift--rubric-item)))
        (expect text :to-match "\\* RUBRIC_LINK +org: +(none)")
        (expect text :to-match "canvas: \"Python Basics\" (id 900), 2 criteria, 10 points"))))

  (it "has no rubric block when neither side has one"
    (test-drift--with-file
      (let ((text (test-drift--show "Lab 1" test-drift--item)))
        (expect text :not :to-match "RUBRIC_LINK"))))

  (it "shows no block on a feature whose read carries no rubric"
    (test-drift--with-file
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-assignments-file)
        (goto-char (point-min))
        (re-search-forward "^\\* Lab 4$")
        ;; A discussion's read returns no rubric, so its RUBRIC_LINK is
        ;; not starred against one (issue #567).
        (expect (org-canvas--drift-show-rubric
                 (org-canvas--diff-find-feature "Discussions")
                 (point) test-drift--item)
                :to-equal ""))))

  (it "says when the link resolves to nothing, and when the rubric was never pushed"
    (test-drift--with-file
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-assignments-file)
        (goto-char (point-min))
        (re-search-forward "^\\* Lab 4$")
        (org-entry-put (point) "RUBRIC_LINK" "[[file:rubrics.org::*Nope][Nope]]"))
      (expect (test-drift--show "Lab 4" test-drift--rubric-item)
              :to-match "(unresolved link)")
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-assignments-file)
        (goto-char (point-min))
        (re-search-forward "^\\* Lab 4$")
        (org-entry-put (point) "RUBRIC_LINK"
                       "[[file:rubrics.org::*Python Basics][Python Basics]]"))
      (with-current-buffer (org-canvas--find-file-noselect org-canvas-rubrics-file)
        (goto-char (point-min))
        (re-search-forward "^:CANVAS_ID: 900\n")
        (replace-match ""))
      (expect (test-drift--show "Lab 4" test-drift--rubric-item)
              :to-match "org: +\"Python Basics\" (no CANVAS_ID), 2 criteria, 10 points"))))

(describe "org-canvas-diff-show-row (issue #500)"
  (it "shows the row's Canvas version from the item the report read"
    (test-drift--with-file
      (let (shown)
        (cl-letf (((symbol-function 'org-canvas--html-to-org) (lambda (_) "C."))
                  ((symbol-function 'org-canvas--report-display)
                   (lambda (_name render) (setq shown (with-temp-buffer (funcall render)
                                                                         (buffer-string))))))
          (with-mock-api
            (with-current-buffer (test-drift--report-buffer
                                  `((:name "Assignments"
                                     :divergences (,(test-drift--entry '("POINTS" "10" "12"))))))
              (test-drift--goto-row 'modified)
              (org-canvas-diff-show-row))
            (test-drift--expect-no-writes)))
        (expect shown :to-match "\\* POINTS +org: +10"))))

  (it "refuses a row with no Canvas copy, and one whose heading is gone"
    (test-drift--with-file
      (with-current-buffer (test-drift--report-buffer
                            '((:name "Assignments" :divergences ((:kind missing :title "G" :id "9")))))
        (test-drift--goto-row 'missing)
        (expect (org-canvas-diff-show-row) :to-throw 'user-error))
      (with-current-buffer (test-drift--report-buffer
                            `((:name "Assignments"
                               :divergences ((:kind modified :title "Ghost" :id "999"
                                              :fields (("POINTS" "1" "2"))
                                              :item ((id . 999)))))))
        (test-drift--goto-row 'modified)
        (expect (org-canvas-diff-show-row) :to-throw 'user-error)))))

(describe "org-canvas-show-at-point and org-canvas-show-headings (issue #500)"
  (it "reads the heading's item and shows it, writing nothing"
    (test-drift--with-file
      (let ((read nil))
        (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                   (lambda (_feature id) (setq read id) test-drift--item))
                  ((symbol-function 'org-canvas--html-to-org) (lambda (_) "C.")))
          (with-current-buffer (org-canvas--find-file-noselect file)
            (goto-char (point-min))
            (re-search-forward "^\\* Lab 1$")
            (let ((text (with-mock-api
                          (with-output-to-string (org-canvas-show-at-point)))))
              (expect text :to-match "\\* POINTS +org: +10"))))
        (expect read :to-equal "60"))
      (expect (test-drift--prop file "Lab 1" "POINTS") :to-equal "10")))

  (it "lists an assignment's overrides as Canvas holds them (issue #528)"
    (let ((assignments (org-canvas--diff-find-feature "Assignments")))
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method url &rest _)
                   (expect url :to-match "assignments/60/overrides")
                   [((id . 800) (student_ids . [5]) (due_at . "2026-10-23T14:15:00Z"))
                    ((id . 801) (student_ids . [6]))])))
        (let ((text (org-canvas--drift-show-overrides assignments "60")))
          (expect text :to-match "Overrides on Canvas (2)")
          (expect text :to-match "Students: #5 +due <2026-10-23")
          (expect text :to-match "Students: #6 +(no dates)")))
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (error "Boom"))))
        (expect (org-canvas--drift-show-overrides assignments "60")
                :to-match "Overrides on Canvas: not read (Boom)"))
      (expect (org-canvas--drift-show-overrides (org-canvas--diff-find-feature "Pages") "x")
              :to-equal "")))

  (it "says when Canvas no longer holds the item"
    (test-drift--with-file
      (cl-letf (((symbol-function 'org-canvas--pull-item-read) (lambda (&rest _) 'gone)))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (goto-char (point-min))
          (re-search-forward "^\\* Lab 2$")
          (expect (with-output-to-string (org-canvas-show-at-point))
                  :to-match "'Lab 2' (id 61) is not on Canvas any more")))))

  (it "shows named headings in turn, a failure as its own line"
    (test-drift--with-file
      (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                ((symbol-function 'org-canvas--pull-item-read)
                 (lambda (_feature id) `((id . ,id) (name . "Lab 2"))))
                ((symbol-function 'org-canvas--html-to-org) (lambda (_) "C.")))
        (let ((text (with-mock-api
                      (with-output-to-string
                        (org-canvas-show-headings '(("assignment" "61" canvas-id)
                                                    ("assignments" . "Nope")))))))
          (expect text :to-match "Assignments 'Lab 2' (CANVAS_ID 61)")
          (expect text :to-match "assignments 'Nope': No Assignments heading titled")))))

  (it "names the features when one is misspelt, before reading anything"
    (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore))
      (expect (org-canvas-show-headings '(("frobnicate" . "X"))) :to-throw 'user-error))))

;;;; The command line

(describe "scripts/org-canvas adopt and show (issues #498, #500)"
  (defun test-drift--batch (args)
    "Return the exit status of the command line ARGS, the course setup mocked."
    (let (status)
      (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore))
        (with-output-to-string (setq status (org-canvas-batch-main args))))
      status))

  (it "reads --all, headings, --body and --overrides"
    (require 'org-canvas-batch)
    (expect (org-canvas-batch--adopt-args '("--all" "--body"))
            :to-equal '(all t nil))
    (expect (org-canvas-batch--adopt-args '("assignment:Lab 1" "--overrides"))
            :to-equal '((("assignment" "Lab 1" title)) nil t))
    (dolist (bad '(("--body") ("--all" "assignment:Lab 1")))
      (expect (org-canvas-batch--adopt-args bad) :to-throw 'org-canvas-batch-usage-error)))

  (it "exits 0 when every row was adopted and 1 when one was left"
    (require 'org-canvas-batch)
    (let ((outcomes '((:outcome adopted) (:outcome appended)))
          (seen nil))
      (cl-letf (((symbol-function 'org-canvas-diff-adopt-drifts)
                 (lambda (selectors &rest keys)
                   (setq seen (list selectors keys org-canvas--dry-run))
                   outcomes)))
        (expect (test-drift--batch '("-n" "adopt" "--all" "--overrides")) :to-equal 0)
        (expect seen :to-equal '(all (:body nil :overrides t) t))
        (setq outcomes '((:outcome adopted) (:outcome partial)))
        (expect (test-drift--batch '("adopt" "assignment:Lab 1")) :to-equal 1))))

  (it "shows the named headings and exits 0"
    (require 'org-canvas-batch)
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas-show-headings)
                 (lambda (entries) (setq seen entries) "")))
        (expect (test-drift--batch '("show" "page:Week 1" "assignment#60")) :to-equal 0)
        (expect seen :to-equal '(("page" "Week 1" title) ("assignment" "60" canvas-id)))))))

;;; org-canvas-drift-test.el ends here
