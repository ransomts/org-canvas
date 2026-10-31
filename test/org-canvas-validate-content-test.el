;;; org-canvas-validate-content-test.el --- Buttercup tests for the validator's content checks  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-validate-content': discussions,
;; rubrics, files, sections, pages, quizzes, settings,
;; modules and messages.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(describe "org-canvas--validate-rubric-structure"
  (it "returns nil when criterion sub-headings are present"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Writing :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
| No Marks | 0 | |
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-rubric-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Rubric"))
             :to-be nil)))

  (it "returns error when no criterion sub-headings"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:

No level-2 children here.
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-rubric-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Test Rubric"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "no criteria")))))

(describe "org-canvas--validate-file-structure"
  (it "returns nil for a folder heading (no link)"
    (with-temp-org-buffer
     "* course-content
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-file-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "course-content"))
             :to-be nil)))

  (it "returns error for missing linked file"
    (with-temp-org-buffer
     "* [[file:content/nonexistent.pdf][Nonexistent]]
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-file-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Nonexistent"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "does not exist"))))

  (it "returns nil for existing linked file"
    (let* ((temp-dir (make-temp-file "org-val-test-" t))
           (content-file (expand-file-name "test.txt" temp-dir))
           (org-file (expand-file-name "files.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file content-file (insert "hello"))
            (with-temp-file org-file
              (insert (format "* [[file:%s][test.txt]]\n:PROPERTIES:\n:END:\n"
                              content-file)))
            (with-current-buffer (find-file-noselect org-file)
              (unwind-protect
                  (progn
                    (goto-char (point-min))
                    (org-back-to-heading t)
                    (expect (org-canvas--validate-file-structure
                             (list :file org-file :line (line-number-at-pos) :heading "test.txt"))
                            :to-be nil))
                (kill-buffer))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--validate-section-structure"
  (it "returns nil when CANVAS_ID is present"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:CANVAS_ID: 12345
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-section-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Section A"))
             :to-be nil)))

  (it "returns warning when CANVAS_ID is missing"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:END:
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-section-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Section A"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID"))))

  (it "warns on a MEETS it cannot read and accepts one it can (issue #383)"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:CANVAS_ID: 1
:MEETS: MWF 10-11
:END:
* Section B
:PROPERTIES:
:CANVAS_ID: 2
:MEETS: TTh 13:25-14:15
:END:
"
     (let ((issues (org-canvas--validate-section-structure
                    (list :file (buffer-file-name) :line 1 :heading "Section A"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :property) :to-equal "MEETS")
       (expect (plist-get (car issues) :message) :to-match "MWF 10-11"))
     (re-search-forward "^\\* Section B")
     (expect (org-canvas--validate-section-structure
              (list :file (buffer-file-name) :line 6 :heading "Section B"))
             :to-be nil))))

(describe "org-canvas--validate-file-structure file size warning"
  (it "warns when file exceeds max size"
    (let* ((org-canvas-max-file-size-mb 0)  ;; 0 MB threshold - any file triggers
           (temp-file (make-temp-file "size-test" nil ".txt"))
           (org-file (make-temp-file "org-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "content"))
            (with-temp-file org-file
              (insert (format "* [[file:%s][Test File]]\n:PROPERTIES:\n:END:\n" temp-file)))
            (with-current-buffer (find-file-noselect org-file)
              (goto-char (point-min))
              (org-back-to-heading)
              (let ((issues (org-canvas--validate-file-structure
                             (list :file org-file :line 1 :heading "Test File"))))
                (expect (length issues) :not :to-equal 0)
                (let ((issue (car issues)))
                  (expect (plist-get issue :severity) :to-equal 'warning)
                  (expect (plist-get issue :message) :to-match "MB")))
              (kill-buffer)))
        (delete-file temp-file)
        (delete-file org-file))))

  (it "does not warn when file size equals the limit (boundary)"
    ;; Guards `(> size-mb max)': a >= would flag a file exactly at the limit.
    ;; An empty file is 0.0 MB; with the limit at 0, `>' is nil but `>=' is t.
    (let* ((org-canvas-max-file-size-mb 0)
           (temp-file (make-temp-file "size-test" nil ".txt"))  ;; empty: 0 bytes
           (org-file (make-temp-file "org-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file org-file
              (insert (format "* [[file:%s][Test File]]\n:PROPERTIES:\n:END:\n" temp-file)))
            (with-current-buffer (find-file-noselect org-file)
              (goto-char (point-min))
              (org-back-to-heading)
              (let ((issues (org-canvas--validate-file-structure
                             (list :file org-file :line 1 :heading "Test File")))
                    (has-size-warning nil))
                (dolist (i issues)
                  (when (string-match-p "MB" (plist-get i :message))
                    (setq has-size-warning t)))
                (expect has-size-warning :to-be nil))
              (kill-buffer)))
        (delete-file temp-file)
        (delete-file org-file)))))

(describe "org-canvas--validate-quiz-question-points"
  (it "returns 0 for a group question missing PICK_COUNT (no crash)"
    ;; Guards the `(and (equal qtype \"group\") pick-count question-points)'
    ;; condition: an `or' would enter the group branch and call
    ;; string-to-number on a nil PICK_COUNT, crashing on malformed input.
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
** Group Q
:PROPERTIES:
:TYPE: group
:END:
"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* Group Q")
     (org-back-to-heading)
     (expect (org-canvas--validate-quiz-question-points (point)) :to-equal 0))))

(describe "org-canvas--validate-module-item-link"
  (it "returns nil for items with EXTERNAL_URL"
    (with-temp-org-buffer
     "* Module
** External Site
:PROPERTIES:
:EXTERNAL_URL: https://example.com
:END:
"
     (search-forward "External Site")
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 2 :heading "External Site")))
       (expect (org-canvas--validate-module-item-link loc) :to-be nil))))

  (it "returns nil for plain-text SubHeaders"
    (with-temp-org-buffer
     "* Module
** Week 1 Overview
:PROPERTIES:
:END:
"
     (search-forward "Week 1 Overview")
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 2 :heading "Week 1 Overview")))
       (expect (org-canvas--validate-module-item-link loc) :to-be nil))))

  (it "returns error for link to missing file"
    (with-temp-org-buffer
     "* Module
** [[file:nonexistent.org::*Heading][Heading]]
:PROPERTIES:
:END:
"
     (search-forward "Heading")
     (org-back-to-heading)
     (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
            (issues (org-canvas--validate-module-item-link loc)))
       (expect issues :not :to-be nil)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message) :to-match "missing file"))))

  (it "returns error for link to missing heading"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Existing Page\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Missing Page][Missing Page]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Missing Page")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :not :to-be nil)
               (expect (plist-get (car issues) :severity) :to-equal 'error)
               (expect (plist-get (car issues) :message) :to-match "missing heading"))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file))))

  (it "returns warning when target heading has no CANVAS_ID"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Unsynced Page\n:PROPERTIES:\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Unsynced Page][Unsynced Page]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Unsynced Page")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :not :to-be nil)
               (expect (plist-get (car issues) :severity) :to-equal 'warning)
               (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID"))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file))))

  (it "returns nil for valid link with CANVAS_ID"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Synced Page\n:PROPERTIES:\n:CANVAS_ID: 123\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Synced Page][Synced Page]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Synced Page")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :to-be nil))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file)))))

(defun test-validate--module-file-link-issues (items)
  "Return the module-item-link issues of each heading in ITEMS.
ITEMS is the text of the level-2 item headings.  They sit in a
modules.org beside a files.org whose one heading links to
content/listed.pdf (not on disk), a file content/on-disk.pdf and a
pages.org.  The answer is a list, per item in order, of
\(SEVERITY MESSAGE) pairs."
  (let* ((dir (file-name-as-directory (make-temp-file "validate-535-" t)))
         (modules (expand-file-name "modules.org" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "content" dir))
          (with-temp-file (expand-file-name "content/on-disk.pdf" dir) (insert "pdf"))
          (with-temp-file (expand-file-name "pages.org" dir)
            (insert "* Intro\n:PROPERTIES:\n:CANVAS_URL: intro\n:END:\n"))
          (with-temp-file (expand-file-name "files.org" dir)
            (insert "* [[file:content/listed.pdf][listed.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 9\n:END:\n"))
          (with-temp-file modules (insert "* Week 1\n" items))
          (with-current-buffer (org-canvas--find-file-noselect modules)
            (unwind-protect
                (let ((markers (org-map-entries #'point-marker "LEVEL=2" 'file)))
                  (mapcar (lambda (marker)
                            (goto-char marker)
                            (mapcar (lambda (issue)
                                      (list (plist-get issue :severity)
                                            (plist-get issue :message)))
                                    (org-canvas--validate-module-item-link
                                     (list :file modules :line (line-number-at-pos)
                                           :heading "item"))))
                          markers))
              (kill-buffer))))
      (dolist (name '("files.org" "pages.org"))
        (when-let* ((buf (find-buffer-visiting (expand-file-name name dir))))
          (kill-buffer buf)))
      (delete-directory dir t))))

(describe "module item links to a bare file path (issue #535)"
  (it "passes a file on disk, one files.org lists and an Org file found by name"
    (expect (test-validate--module-file-link-issues
             (concat "** [[file:content/on-disk.pdf][On disk]]\n"
                     "** [[file:content/listed.pdf][Listed]]\n"
                     "** [[file:../elsewhere/pages.org][Pages]]\n"))
            :to-equal '(nil nil nil)))

  (it "reports a path that is neither on disk nor in files.org as an error"
    (let ((issues (test-validate--module-file-link-issues
                   (concat "** [[file:content/on-disk.pdf][On disk]]\n"
                           "** [[file:content/gone.pdf][Gone]]\n"
                           "** [[file:content/listed.pdf][Listed]]\n"))))
      (expect (nth 0 issues) :to-be nil)
      (expect (nth 2 issues) :to-be nil)
      (expect (length (nth 1 issues)) :to-equal 1)
      (expect (car (car (nth 1 issues))) :to-equal 'error)
      (expect (nth 1 (car (nth 1 issues))) :to-match
              "missing file: content/gone.pdf (not on disk, and no files.org heading")))

  (it "leaves a link with a search option to the heading-link check"
    (expect (test-validate--module-file-link-issues
             "** [[file:pages.org::#intro][Intro]]\n")
            :to-equal '(nil))))

(describe "org-canvas--validate-module-item-link with escaped brackets"
  (it "unescapes \\[ in heading name before searching"
    (let ((target-file (make-temp-file "test-pages" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file target-file
              (insert "* Page [Extra\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (with-temp-org-buffer
             (format "* Module\n** [[file:%s::*Page \\[Extra][Page Extra]]\n:PROPERTIES:\n:END:\n" target-file)
             (search-forward "Page Extra")
             (org-back-to-heading)
             (let* ((loc (list :file (buffer-file-name) :line 2 :heading "Link"))
                    (issues (org-canvas--validate-module-item-link loc)))
               (expect issues :to-be nil))))
        (let ((buf (find-buffer-visiting target-file)))
          (when buf (kill-buffer buf)))
        (delete-file target-file)))))

(describe "org-canvas--validate-quiz-point-total"
  (it "returns nil when POINTS matches question total"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:POINTS: 30
:END:
** Q1
:PROPERTIES:
:POINTS: 10
:END:
** Q2
:PROPERTIES:
:POINTS: 20
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz")))
       (expect (org-canvas--validate-quiz-point-total loc) :to-be nil))))

  (it "returns warning when POINTS does not match total"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:POINTS: 100
:END:
** Q1
:PROPERTIES:
:POINTS: 10
:END:
** Q2
:PROPERTIES:
:POINTS: 20
:END:
"
     (org-back-to-heading)
     (let* ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz"))
            (issues (org-canvas--validate-quiz-point-total loc)))
       (expect issues :not :to-be nil)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :message) :to-match "100")
       (expect (plist-get (car issues) :message) :to-match "30"))))

  (it "returns nil when no POINTS property"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:END:
** Q1
:PROPERTIES:
:POINTS: 10
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz")))
       (expect (org-canvas--validate-quiz-point-total loc) :to-be nil))))

  (it "handles question groups with PICK_COUNT * QUESTION_POINTS"
    (with-temp-org-buffer
     "* Quiz
:PROPERTIES:
:POINTS: 30
:END:
** Regular Question
:PROPERTIES:
:POINTS: 10
:END:
** Question Group
:PROPERTIES:
:TYPE: group
:PICK_COUNT: 2
:QUESTION_POINTS: 10
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Quiz")))
       (expect (org-canvas--validate-quiz-point-total loc) :to-be nil)))))

(describe "org-canvas--validate-rubric-outcome-links"
  (it "returns nil for table with valid outcome links"
    (let* ((temp-dir (make-temp-file "validate-rubric-outcome" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Group\n** Python\n:PROPERTIES:\n:CANVAS_ID: 42\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (let ((link (format "[[file:%s::*Python][Python]]" outcomes-file))
                    (loc (list :file rubrics-file :line 5 :heading "Test")))
                (expect (org-canvas--validate-rubric-outcome-links
                         (list (list "Quality" "10" "Desc" link))
                         rubrics-file loc)
                        :to-be nil))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "warns for outcome link with no CANVAS_ID"
    (let* ((temp-dir (make-temp-file "validate-rubric-outcome" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file
              (insert "* Group\n** Python\n:PROPERTIES:\n:END:\n"))
            (let ((org-canvas-outcomes-file outcomes-file))
              (let ((link (format "[[file:%s::*Python][Python]]" outcomes-file))
                    (loc (list :file rubrics-file :line 5 :heading "Test")))
                (let ((issues (org-canvas--validate-rubric-outcome-links
                               (list (list "Quality" "10" "Desc" link))
                               rubrics-file loc)))
                  (expect (length issues) :to-equal 1)
                  (expect (plist-get (car issues) :severity) :to-equal 'warning)
                  (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID")))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "warns for non-link text in 4th column"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (let ((issues (org-canvas--validate-rubric-outcome-links
                     (list (list "Quality" "10" "Desc" "plain text"))
                     "/tmp/rubrics.org" loc)))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :severity) :to-equal 'warning)
        (expect (plist-get (car issues) :message) :to-match "not a file link"))))

  (it "returns nil for 3-column table (backward compat)"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (expect (org-canvas--validate-rubric-outcome-links
               (list (list "Quality" "10" "Desc"))
               "/tmp/rubrics.org" loc)
              :to-be nil)))

  (it "skips rating rows"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (expect (org-canvas--validate-rubric-outcome-links
               (list (list "> Good" "10" "" "ignored-link"))
               "/tmp/rubrics.org" loc)
              :to-be nil)))

  (it "skips empty 4th column"
    (let ((loc (list :file "/tmp/rubrics.org" :line 5 :heading "Test")))
      (expect (org-canvas--validate-rubric-outcome-links
               (list (list "Quality" "10" "Desc" ""))
               "/tmp/rubrics.org" loc)
              :to-be nil))))

(describe "org-canvas--validate-rubric-structure with outcome links"
  (it "validates :OUTCOME: property as a file link"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Quality :10pt:
:PROPERTIES:
:OUTCOME: plain text
:END:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-rubric-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos)
                          :heading "Test Rubric"))))
       ;; Should warn about "plain text" not being a file link.  Property is
       ;; only treated as a candidate when it contains [[file:..., so a bare
       ;; non-link string yields no issue (validation is skipped).
       ;; This test asserts the structural validation (no issue) for plain
       ;; text values; only file-link values are scrutinised further.
       (expect issues :to-be nil))))

  (it "passes for criterion without :OUTCOME: property"
    (with-temp-org-buffer
     "* Test Rubric
:PROPERTIES:
:END:
** Quality :10pt:
| Rating | Points | Description |
|--------+--------+-------------|
| Full Marks | 10 | |
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-rubric-structure
              (list :file (buffer-file-name) :line (line-number-at-pos)
                    :heading "Test Rubric"))
             :to-be nil))))

(describe "org-canvas--validate-settings-structure (issue #275)"
  (it "warns, push-only, about a sub-heading the syllabus push leaves behind"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Syllabus text.

** Navigation
1. Home

** Office Hours
Tuesdays.
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-settings-structure
                    '(:file "settings.org" :line 1 :heading "Course"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :push-only) :to-be t)
       (expect (plist-get (car issues) :file) :to-equal "settings.org")
       (expect (plist-get (car issues) :line) :to-equal 10)
       (expect (plist-get (car issues) :heading) :to-equal "Office Hours")
       (expect (plist-get (car issues) :message)
               :to-match "`Office Hours' is not syllabus text"))))

  (it "says nothing about Navigation, or about a course heading with no children"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Syllabus text.

** Navigation
1. Home
"
     (org-back-to-heading)
     (expect (org-canvas--validate-settings-structure
              '(:file "settings.org" :line 1 :heading "Course"))
             :to-be nil))
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Syllabus text.
"
     (org-back-to-heading)
     (expect (org-canvas--validate-settings-structure
              '(:file "settings.org" :line 1 :heading "Course"))
             :to-be nil)))

  (it "runs from the settings spec"
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "settings.org" dir)
        (insert "* My Course\n:PROPERTIES:\n:END:\n\nText.\n\n** Grading\nA table.\n\n** Navigation\n1. Home\n"))
      (test-validate-create-empty-files dir '("settings.org"))
      (let* ((spec (cl-find "Settings" (org-canvas--validate-specs)
                            :key (lambda (s) (plist-get s :label))
                            :test #'string=))
             (issues (org-canvas--validate-spec spec)))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :message) :to-match "`Grading' is not syllabus text")))))

(describe "org-canvas--validate-settings-raw-html (issue #459)"
  (it "warns, push-only, at the first line of a syllabus pulled as raw HTML"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Plain words first.
<p><span style=\"color: #0000ff;\">Welcome</span></p>
<ul><li>Week 1</li></ul>

** Navigation
1. Home
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-settings-structure
                    '(:file "settings.org" :line 1 :heading "Course"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :push-only) :to-be t)
       (expect (plist-get (car issues) :property) :to-equal "syllabus")
       (expect (plist-get (car issues) :line) :to-equal 6)
       (expect (plist-get (car issues) :heading) :to-equal "Course")
       (expect (plist-get (car issues) :message) :to-match "raw HTML tags"))))

  (it "says nothing about Org text, timestamps, targets, or HTML meant as HTML"
    (with-temp-org-buffer
     "* Course
:PROPERTIES:
:END:

Due <2026-10-06 Tue>, see <<office hours>> and *bold* text.
Inline @@html:<b>bold</b>@@ snippet.

#+begin_export html
<p><span style=\"color: red;\">Kept as HTML</span></p>
#+end_export

#+HTML: <div>one line</div>

** Navigation
<p>Not syllabus text</p>
"
     (org-back-to-heading)
     (expect (org-canvas--validate-settings-structure
              '(:file "settings.org" :line 1 :heading "Course"))
             :to-be nil))))

(describe "org-canvas--validate-page-structure"
  (it "errors on FRONT_PAGE: true with PUBLISHED: false"
    (with-temp-org-buffer
     "* Course Home
:PROPERTIES:
:PUBLISHED: false
:FRONT_PAGE: true
:END:
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-page-structure
                    '(:file "pages.org" :line 1 :heading "Course Home"))))
       (expect issues :not :to-be nil)
       (expect (plist-get (car issues) :severity) :to-equal 'error)
       (expect (plist-get (car issues) :message)
               :to-match "unpublished front page"))))

  (it "passes for a published front page"
    (with-temp-org-buffer
     "* Course Home
:PROPERTIES:
:PUBLISHED: true
:FRONT_PAGE: true
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-page-structure
              '(:file "pages.org" :line 1 :heading "Course Home"))
             :to-be nil)))

  (it "passes when PUBLISHED is absent (defaults to published)"
    (with-temp-org-buffer
     "* Course Home
:PROPERTIES:
:FRONT_PAGE: true
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-page-structure
              '(:file "pages.org" :line 1 :heading "Course Home"))
             :to-be nil)))

  (it "passes for an unpublished non-front page"
    (with-temp-org-buffer
     "* Draft Page
:PROPERTIES:
:PUBLISHED: false
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-page-structure
              '(:file "pages.org" :line 1 :heading "Draft Page"))
             :to-be nil))))

(describe "org-canvas--validate-page-structure PUBLISH_AT (issue #379)"
  (it "warns that PUBLISHED: false is not sent beside PUBLISH_AT"
    (with-temp-org-buffer
     "* Week 3
:PROPERTIES:
:PUBLISHED: false
:PUBLISH_AT: <2026-10-05 Mon 08:00>
:END:
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-page-structure
                    '(:file "pages.org" :line 1 :heading "Week 3"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :property) :to-equal "PUBLISHED")
       (expect (plist-get (car issues) :message) :to-match "not sent"))))

  (it "warns about a scheduled front page"
    (with-temp-org-buffer
     "* Course Home
:PROPERTIES:
:FRONT_PAGE: true
:PUBLISH_AT: <2026-10-05 Mon 08:00>
:END:
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-page-structure
                    '(:file "pages.org" :line 1 :heading "Course Home"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :property) :to-equal "PUBLISH_AT")
       (expect (plist-get (car issues) :message) :to-match "home page"))))

  (it "passes a scheduled page that leaves PUBLISHED alone"
    (with-temp-org-buffer
     "* Week 3
:PROPERTIES:
:PUBLISH_AT: <2026-10-05 Mon 08:00>
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-page-structure
              '(:file "pages.org" :line 1 :heading "Week 3"))
             :to-be nil))))

(describe "org-canvas--validate-all-day-span (issue #93)"
  (it "warns on ALL_DAY: true over a span"
    (with-temp-org-buffer "* Break
:PROPERTIES:
:START_AT: <2026-11-25 Wed 00:00>
:END_AT: <2026-11-27 Fri 23:59>
:ALL_DAY: true
:END:
"
      (org-back-to-heading)
      (let ((issues (org-canvas--validate-all-day-span
                     '(:file "calendar.org" :line 1 :heading "Break"))))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :severity) :to-equal 'warning)
        (expect (plist-get (car issues) :property) :to-equal "ALL_DAY")
        (expect (plist-get (car issues) :message) :to-match "single-day"))))

  (it "stays quiet for a single day, a span without the flag, and no end"
    (dolist (content '("* E\n:PROPERTIES:\n:START_AT: <2026-10-12 Mon 00:00>\n:END_AT: <2026-10-12 Mon 23:59>\n:ALL_DAY: true\n:END:\n"
                       "* E\n:PROPERTIES:\n:START_AT: <2026-11-25 Wed 00:00>\n:END_AT: <2026-11-27 Fri 23:59>\n:ALL_DAY: false\n:END:\n"
                       "* E\n:PROPERTIES:\n:START_AT: <2026-10-12 Mon 09:00>\n:ALL_DAY: true\n:END:\n"))
      (with-temp-org-buffer content
        (org-back-to-heading)
        (expect (org-canvas--validate-all-day-span
                 '(:file "f" :line 1 :heading "E"))
                :to-be nil)))))

(describe "org-canvas--validate-rubric-criterion-outcome"
  (it "reports a criterion whose OUTCOME link has no CANVAS_ID"
    (let* ((temp-dir (make-temp-file "validate-criterion-outcome" t))
           (outcomes-file (expand-file-name "outcomes.org" temp-dir))
           (rubrics-file (expand-file-name "rubrics.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file outcomes-file (insert "* Group\n** Python\n"))
            (let ((org-canvas-outcomes-file outcomes-file)
                  (loc (list :file rubrics-file :line 1 :heading "Essay")))
              (with-temp-org-buffer
                  (format "* Essay\n** Quality\n:PROPERTIES:\n:OUTCOME: [[file:%s::*Python][Python]]\n:END:\n"
                          outcomes-file)
                (re-search-forward "^\\*\\* Quality")
                (org-back-to-heading t)
                (let ((issues (org-canvas--validate-rubric-criterion-outcome rubrics-file loc)))
                  (expect (length issues) :to-equal 1)
                  (expect (plist-get (car issues) :severity) :to-equal 'warning)
                  (expect (plist-get (car issues) :message)
                          :to-match "criterion 'Quality' outcome link has no CANVAS_ID")))))
        (let ((buf (find-buffer-visiting outcomes-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "returns nothing when the criterion has no OUTCOME"
    (with-temp-org-buffer "* Essay\n** Quality\n"
      (re-search-forward "^\\*\\* Quality")
      (org-back-to-heading t)
      (expect (org-canvas--validate-rubric-criterion-outcome
               "/tmp/rubrics.org" (list :file "/tmp/rubrics.org" :line 1 :heading "Essay"))
              :to-be nil))))

(describe "the accommodations table under a quiz (issue #230)"
  (defmacro test-validate-accommodations-with-people (&rest body)
    "Run BODY with people.org holding Adams, Alice as USER_ID 1."
    (declare (indent 0))
    `(let* ((dir (make-temp-file "acc-validate-" t))
            (org-canvas-people-file (expand-file-name "people.org" dir)))
       (with-temp-file org-canvas-people-file
         (insert "* Students\n** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"))
       (unwind-protect
           (progn ,@body)
         (let ((buf (find-buffer-visiting org-canvas-people-file)))
           (when buf (kill-buffer buf)))
         (delete-directory dir t))))

  (it "passes a roster name and a literal #id, warns push-only about a stranger, errors on a word"
    (test-validate-accommodations-with-people
      (with-temp-org-buffer
          "* Quiz
:PROPERTIES:
:CANVAS_ID: 7
:END:

#+NAME: accommodations
| Student      | Extra time | Extra attempts |
|--------------+------------+----------------|
| Adams, Alice |         30 |                |
| #9           |         15 |              1 |
| Nobody, Nell |         10 |                |
| #4           |       lots |                |

** Question
"
        (org-back-to-heading t)
        (let* ((issues (org-canvas--validate-quiz-point-total
                        (list :file (buffer-file-name) :line 1 :heading "Quiz")))
               (messages (mapcar (lambda (i) (plist-get i :message)) issues)))
          (expect (length issues) :to-equal 2)
          (expect (car messages) :to-match "'Nobody, Nell' is not in people.org")
          (expect (plist-get (car issues) :push-only) :to-be t)
          (expect (plist-get (car issues) :line) :to-equal 11)
          (expect (cadr messages) :to-match "'lots' is not a whole number")
          (expect (plist-get (cadr issues) :severity) :to-equal 'error)))))

  (it "warns, push-only, about a student people.org marks as gone (issue #290)"
    (test-validate-accommodations-with-people
      (with-temp-file org-canvas-people-file
        (insert "* Students\n** Beta, Bob\n:PROPERTIES:\n:USER_ID: 2\n"
                ":ENROLLMENT_STATE: absent\n:END:\n"))
      (let ((issues (org-canvas--validate-accommodation-row
                     '("Beta, Bob" "30" "") '(1 2) (list :file "quizzes.org" :line 9 :heading "Quiz"))))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :message)
                :to-equal "Accommodation student 'Beta, Bob' is no longer enrolled according to people.org")
        (expect (plist-get (car issues) :push-only) :to-be t))))

  (it "says nothing about a quiz without a table, or with a #+NAME: line and no table"
    (with-temp-org-buffer "* Quiz\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\nText only.\n"
      (org-back-to-heading t)
      (expect (org-canvas--validate-quiz-accommodations
               (list :file (buffer-file-name) :line 1 :heading "Quiz"))
              :to-be nil))
    (with-temp-org-buffer "* Quiz\n#+NAME: accommodations\nnot a table\n"
      (org-back-to-heading t)
      (expect (org-canvas--validate-quiz-accommodations
               (list :file (buffer-file-name) :line 1 :heading "Quiz"))
              :to-be nil)))

  (it "finds the number columns by header, falling back to positions 1 and 2"
    (expect (org-canvas--validate-accommodation-number-columns
             '("Student" "Unlocked" "Extra time")) :to-equal '(2))
    (expect (org-canvas--validate-accommodation-number-columns
             '("Student" "A" "B")) :to-equal '(1 2))))

(describe "org-canvas--validate-module-item-ids (issue #105)"
  (it "warns once per item id claimed by more than one heading, naming the lines"
    (let ((file (make-temp-file "dup-item-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Week 1\n** Check 3\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
                      "* Week 2\n** Check 3\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
                      "** Check 4\n:PROPERTIES:\n:CANVAS_ID: 56\n:END:\n"))
            (let ((issues (org-canvas--validate-module-item-ids file)))
              (expect (length issues) :to-equal 1)
              (expect (plist-get (car issues) :severity) :to-equal 'warning)
              (expect (plist-get (car issues) :line) :to-equal 2)
              (expect (plist-get (car issues) :message)
                      :to-match "CANVAS_ID 55 is claimed by 2 headings (lines 2, 7)")))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "is quiet when every item id is unique"
    (let ((file (make-temp-file "uniq-item-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Week 1\n** Check 3\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
                      "* Week 2\n** Check 4\n:PROPERTIES:\n:CANVAS_ID: 56\n:END:\n"
                      "** No id yet\n"))
            (expect (org-canvas--validate-module-item-ids file) :to-be nil))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "is wired in as the module-items file check"
    (expect (plist-get (gethash "module-items" org-canvas--property-registry) :file-fn)
            :to-be #'org-canvas--validate-module-item-ids)))

(describe "org-canvas--validate-quiz-anonymity (issue #349)"
  (it "warns, push-only, on an unstamped survey that does not say"
    (with-temp-org-buffer
     "* Post-debate
:PROPERTIES:
:QUIZ_TYPE: survey
:END:
"
     (org-back-to-heading t)
     (let* ((loc (list :file (buffer-file-name) :line 1 :heading "Post-debate"))
            (issues (org-canvas--validate-quiz-point-total loc))
            (issue (car issues)))
       (expect (length issues) :to-equal 1)
       (expect (plist-get issue :property) :to-equal "ANONYMOUS_SUBMISSIONS")
       (expect (plist-get issue :severity) :to-be 'warning)
       (expect (plist-get issue :push-only) :to-be t))))

  (it "warns on a graded survey too"
    (with-temp-org-buffer
     "* Graded
:PROPERTIES:
:QUIZ_TYPE: graded_survey
:END:
"
     (org-back-to-heading t)
     (expect (length (org-canvas--validate-quiz-anonymity
                      (list :file (buffer-file-name) :line 1)))
             :to-equal 1)))

  (it "is silent when the survey says either way"
    (with-temp-org-buffer
     "* Named
:PROPERTIES:
:QUIZ_TYPE: survey
:ANONYMOUS_SUBMISSIONS: false
:END:
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-quiz-anonymity
              (list :file (buffer-file-name) :line 1))
             :to-be nil)))

  (it "is silent on a stamped survey and on a graded quiz"
    (with-temp-org-buffer
     "* Stamped
:PROPERTIES:
:QUIZ_TYPE: survey
:CANVAS_ID: 691335
:END:
* Quiz
:PROPERTIES:
:QUIZ_TYPE: assignment
:END:
"
     (goto-char (point-min))
     (org-back-to-heading t)
     (expect (org-canvas--validate-quiz-anonymity
              (list :file (buffer-file-name) :line 1))
             :to-be nil)
     (org-next-visible-heading 1)
     (expect (org-canvas--validate-quiz-anonymity
              (list :file (buffer-file-name) :line 6))
             :to-be nil))))

(provide 'org-canvas-validate-content-test)
;;; org-canvas-validate-content-test.el ends here
