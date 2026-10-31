;;; org-canvas-submissions-test.el --- Tests for org-canvas-submissions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for the submissions viewer core.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions)

(describe "org-canvas--submissions-sanitize-filename"
  (it "replaces spaces and special chars"
    (expect (org-canvas--submissions-sanitize-filename "Homework 1: Test")
            :to-equal "Homework_1__Test"))

  (it "preserves alphanumeric and dots"
    (expect (org-canvas--submissions-sanitize-filename "file.txt")
            :to-equal "file.txt")))

;;;; Summary View Rendering

(describe "org-canvas--submissions-comment-text"
  (it "normalizes line ends, surrounding whitespace and blank runs, and is nil when blank"
    (expect (org-canvas--submissions-comment-text nil) :to-be nil)
    (expect (org-canvas--submissions-comment-text :null) :to-be nil)
    (expect (org-canvas--submissions-comment-text "  \n \t\n") :to-be nil)
    (expect (org-canvas--submissions-comment-text " Sharp ") :to-equal "Sharp")
    (expect (org-canvas--submissions-comment-text "a | b") :to-equal "a | b")
    (expect (org-canvas--submissions-comment-text "\n\na\r\n  b  \n\n\n\nc\n\n")
            :to-equal "a\nb\n\nc"))
  (it "agrees between a comment as Canvas returns it and as its item reads back"
    (let ((canvas "First point.\r\n\r\nSecond point, indented on Canvas.\r\n  - a sub-point"))
      (with-temp-buffer
        (org-mode)
        (insert "* Adams, Alice\n")
        (org-canvas--submissions-render-rubric
         test-rubric-criteria `((_7104 . ((points . 2) (comments . ,canvas)))))
        (goto-char (point-min))
        (expect (nth 4 (car (org-canvas--submissions-rubric-rows)))
                :to-equal (org-canvas--submissions-comment-text canvas))))))

(describe "org-canvas--submissions-fetch-for-assignment"
  (it "sends each include as its own include[] key, never comma-joined (#112)"
    (with-org-canvas-test-config
      (let ((captured nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional params)
                     (setq captured params)
                     nil)))
          (org-canvas--submissions-fetch-for-assignment "1001")
          (let ((includes (mapcar #'cdr
                                  (cl-remove-if-not
                                   (lambda (p) (equal (car p) "include[]"))
                                   captured))))
            (expect includes :to-have-same-items-as
                    '("submission_comments" "rubric_assessment" "user"))
            (expect (cl-some (lambda (v) (string-match-p "," v)) includes)
                    :to-be nil)))))))

;;;; Entry Point (org-canvas-pull-submissions)

(describe "org-canvas-submissions-mode"
  (it "does not set buffer-read-only (writable for grade editing)"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (expect buffer-read-only :to-be nil)))

  (it "defines expected keybindings"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "g"))
              :to-equal #'org-canvas-submissions-refresh)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "v"))
              :to-equal #'org-canvas-submissions-toggle-view)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "d"))
              :to-equal #'org-canvas-submissions-download-attachments)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "c"))
              :to-equal #'org-canvas-submissions-add-comment)
      (expect (lookup-key org-canvas-submissions-mode-map (kbd "D"))
              :to-equal #'org-canvas-submissions-download-all-attachments))))

;;;; Full Detail Rendering Integration

(describe "submissions grade keybinding"
  (it "binds S to push-grades"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "S"))
            :to-equal #'org-canvas-submissions-push-grades)))

;;;; Minor Mode — Writable Buffer

(describe "submissions buffer writability"
  (it "does not set buffer-read-only"
    (with-temp-buffer
      (org-canvas-submissions-mode 1)
      (expect buffer-read-only :to-be nil))))

;;;; Integration — Detail Edit → Push Flow

(describe "org-canvas--submissions-entered-score"
  (it "prefers entered_score over the late-adjusted score"
    (expect (org-canvas--submissions-entered-score
             '((entered_score . 5.0) (score . 4.0)))
            :to-equal 5.0))
  (it "falls back to score"
    (expect (org-canvas--submissions-entered-score '((score . 92))) :to-equal 92))
  (it "is nil when ungraded"
    (expect (org-canvas--submissions-entered-score '((score . nil))) :to-be nil)))

(describe "org-canvas--submissions-dir"
  (it "resolves submissions/ under org-canvas-directory when unset"
    (let ((org-canvas-submissions-directory nil)
          (org-canvas-directory "/tmp/course/"))
      (expect (org-canvas--submissions-dir) :to-match "/tmp/course/submissions/?$")))
  (it "honors an explicit directory"
    (let ((org-canvas-submissions-directory "/tmp/elsewhere/"))
      (expect (org-canvas--submissions-dir) :to-equal "/tmp/elsewhere/"))))

(describe "org-canvas--submissions-file-property"
  (it "reads a #+PROPERTY keyword"
    (with-temp-buffer
      (insert test-grading-file-header "* Adams, Alice\n")
      (expect (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID") :to-equal "1001")
      (expect (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_NAME") :to-equal "HW")))
  (it "is nil when absent"
    (with-temp-buffer
      (insert "* Adams, Alice\n")
      (expect (org-canvas--submissions-file-property "CANVAS_ASSIGNMENT_ID") :to-be nil))))

(describe "org-canvas--submissions-ensure-context"
  (it "recovers id, name, and view from a reopened grading file"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n")
      (org-canvas--submissions-ensure-context)
      (expect org-canvas-submissions--assignment-id :to-equal "1001")
      (expect org-canvas-submissions--assignment-name :to-equal "HW")
      (expect org-canvas-submissions--current-view :to-equal 'detail)))
  (it "leaves values a pull already set alone"
    (with-temp-buffer
      (org-mode)
      (setq-local org-canvas-submissions--assignment-id "7")
      (setq-local org-canvas-submissions--current-view 'summary)
      (org-canvas--submissions-ensure-context)
      (expect org-canvas-submissions--assignment-id :to-equal "7")
      (expect org-canvas-submissions--current-view :to-equal 'summary))))

(describe "submission link helpers"
  (it "build course URLs without a doubled slash"
    (let ((org-canvas-base-url "https://canvas.example.edu/")
          (org-canvas-course-id "42"))
      (expect (org-canvas--submissions-assignment-url "7")
              :to-equal "https://canvas.example.edu/courses/42/assignments/7")
      (expect (org-canvas--submissions-speedgrader-url "7")
              :to-equal "https://canvas.example.edu/courses/42/gradebook/speed_grader?assignment_id=7")
      (expect (org-canvas--submissions-speedgrader-url "7" 5001)
              :to-equal "https://canvas.example.edu/courses/42/gradebook/speed_grader?assignment_id=7&student_id=5001"))))

(describe "org-canvas--submissions-heading-for-assignment"
  (it "finds the assignments heading by CANVAS_ID"
    (let ((file (make-temp-file "org-canvas-assignments-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Other\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n* Closer: First Stakes\n:PROPERTIES:\n:CANVAS_ID: 1001\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--submissions-assignments-file) (lambda () file)))
              (expect (org-canvas--submissions-heading-for-assignment "1001")
                      :to-equal "Closer: First Stakes")
              (expect (org-canvas--submissions-heading-for-assignment "999") :to-be nil)))
        (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
        (delete-file file))))
  (it "is nil when the assignments file does not exist"
    (cl-letf (((symbol-function 'org-canvas--submissions-assignments-file)
               (lambda () "/nonexistent/assignments.org")))
      (expect (org-canvas--submissions-heading-for-assignment "1001") :to-be nil))))

(describe "grading file links line"
  (it "links the Org heading, the Canvas page, and SpeedGrader"
    (let ((org-canvas-base-url "https://canvas.example.edu")
          (org-canvas-course-id "42")
          (org-canvas-submissions-directory "/course/submissions/"))
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment)
                 (lambda (_id) "Closer: First Stakes"))
                ((symbol-function 'org-canvas--submissions-assignments-file)
                 (lambda () "/course/assignments.org")))
        (with-temp-buffer
          (org-mode)
          (org-canvas--submissions-render-detail "HW" "1001" nil)
          (let ((content (buffer-string)))
            (expect content :to-match "Assignment: \\[\\[file:\\.\\./assignments\\.org::\\*Closer: First Stakes\\]\\[in Org\\]\\]")
            (expect content :to-match "\\[\\[https://canvas\\.example\\.edu/courses/42/assignments/1001\\]\\[on Canvas\\]\\]")
            (expect content :to-match "speed_grader\\?assignment_id=1001\\]\\[SpeedGrader\\]\\]"))))))
  (it "omits the Org link when no heading carries the id"
    (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil)))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail "HW" "1001" nil)
        (expect (buffer-string) :to-match "^Assignment: \\[\\[https://")
        (expect (buffer-string) :not :to-match "in Org")))))

(describe "per-student SpeedGrader link"
  (it "follows the property drawer when the assignment id is known"
    (let ((org-canvas-base-url "https://canvas.example.edu")
          (org-canvas-course-id "42"))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry (test-org-canvas-make-submission) "HW" "1001")
        (expect (buffer-string)
                :to-match ":END:\n\\[\\[https://canvas\\.example\\.edu/courses/42/gradebook/speed_grader\\?assignment_id=1001&student_id=5001\\]\\[Open in SpeedGrader\\]\\]"))))
  (it "is absent without an assignment id"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry (test-org-canvas-make-submission))
      (expect (buffer-string) :not :to-match "SpeedGrader"))))

(describe "attachment entries"
  (it "parses remote and local forms"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n\n** Attachments\n- [[https://example.com/files/1/download][hw.pdf]]\n- [[file:files/HW/Adams__Alice/notes.pdf][notes.pdf]] ([[https://example.com/files/2/download][Canvas]])\n\n** Comments\n- [[https://example.com/x][not an attachment]]\n"
     (org-back-to-heading)
     (let ((entries (org-canvas--submissions-attachment-entries)))
       (expect (length entries) :to-equal 2)
       (expect (plist-get (nth 0 entries) :name) :to-equal "hw.pdf")
       (expect (plist-get (nth 0 entries) :local) :to-be nil)
       (expect (plist-get (nth 1 entries) :name) :to-equal "notes.pdf")
       (expect (plist-get (nth 1 entries) :url) :to-equal "https://example.com/files/2/download")
       (expect (plist-get (nth 1 entries) :local) :to-equal "files/HW/Adams__Alice/notes.pdf"))))
  (it "renders a downloaded attachment with the local link first"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (local-dir (org-canvas--submissions-attachment-dir "HW" "Adams, Alice")))
      (unwind-protect
          (progn
            (make-directory local-dir t)
            (with-temp-file (expand-file-name "hw.pdf" local-dir) (insert "pdf"))
            (with-temp-buffer
              (org-canvas--submissions-render-attachments
               [((display_name . "hw.pdf") (url . "https://example.com/files/1/download"))
                ((display_name . "late.pdf") (url . "https://example.com/files/2/download"))]
               "HW" "Adams, Alice")
              (expect (buffer-string)
                      :to-match "- \\[\\[file:files/HW/Adams__Alice/hw\\.pdf\\]\\[hw\\.pdf\\]\\] (\\[\\[https://example\\.com/files/1/download\\]\\[Canvas\\]\\])")
              (expect (buffer-string)
                      :to-match "- \\[\\[https://example\\.com/files/2/download\\]\\[late\\.pdf\\]\\]")))
        (delete-directory dir t)))))

(describe "org-canvas--submissions-refresh-links"
  (it "rewrites the Assignment line when the heading was renamed, once"
    (with-grading-file (concat test-grading-file-header
                               "Assignment: [[file:../assignments.org::*Old Title][in Org]], [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/courses/1/gradebook/speed_grader?assignment_id=1001][SpeedGrader]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment)
                 (lambda (_id) "New Title"))
                ((symbol-function 'org-canvas--submissions-assignments-file)
                 (lambda () (expand-file-name "../assignments.org" dir))))
        (expect (org-canvas--submissions-refresh-links) :to-be-truthy)
        (expect (buffer-string) :to-match "^Assignment: \\[\\[file:\\.\\./assignments\\.org::\\*New Title\\]\\[in Org\\]\\]")
        (expect (buffer-string) :not :to-match "Old Title")
        (expect (count-matches "^Assignment: " (point-min) (point-max)) :to-equal 1)
        (expect (org-canvas--submissions-refresh-links) :to-be nil))))
  (it "leaves a file without an Assignment line alone"
    (with-grading-file (concat test-grading-file-header "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (expect (org-canvas--submissions-refresh-links) :to-be nil)
      (expect (buffer-string) :not :to-match "Assignment:")))
  (it "runs when a grading file is opened, and saves the result"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (file (expand-file-name "HW.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert test-grading-file-header
                      "Assignment: [[file:../assignments.org::*Old Title][in Org]], [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/courses/1/gradebook/speed_grader?assignment_id=1001][SpeedGrader]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"))
            (cl-letf (((symbol-function 'completing-read) (lambda (_p files &rest _) (car files)))
                      ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) "Fresh Title"))
                      ((symbol-function 'org-canvas--submissions-assignments-file)
                       (lambda () (expand-file-name "../assignments.org" dir))))
              (org-canvas-open-submissions))
            (expect (buffer-string) :to-match "::\\*Fresh Title\\]")
            (expect (buffer-modified-p) :to-be nil)
            (expect (with-temp-buffer (insert-file-contents file) (buffer-string)) :to-match "Fresh Title"))
        (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
        (delete-directory dir t))))
  (it "runs after a successful push"
    (with-org-canvas-test-config
      (with-mock-api
        (with-grading-file (concat test-grading-file-header
                                   "Assignment: [[file:../assignments.org::*Old Title][in Org]], [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/courses/1/gradebook/speed_grader?assignment_id=1001][SpeedGrader]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n")
          (let ((org-canvas-submissions-check-conflicts nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                      ((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) "Renamed"))
                      ((symbol-function 'org-canvas--submissions-assignments-file)
                       (lambda () (expand-file-name "../assignments.org" dir))))
              (org-canvas-submissions-push-grades)))
          (expect-api-called 'PUT "assignments/1001/submissions/5001")
          (expect (buffer-string) :to-match "::\\*Renamed\\]")
          (expect (buffer-string) :to-match ":CANVAS_SCORE: 95")
          (expect (buffer-modified-p) :to-be nil))))))

;;;; Rubric Link and Criteria

(describe "org-canvas--submissions-rubric-settings"
  (it "returns the rubric id and title as strings"
    (expect (org-canvas--submissions-rubric-settings test-rubric-assignment)
            :to-equal '("133477" . "Essay Rubric")))
  (it "is nil without an attached rubric"
    (expect (org-canvas--submissions-rubric-settings '((id . 1))) :to-be nil)
    (expect (org-canvas--submissions-rubric-settings nil) :to-be nil)))

(describe "org-canvas--submissions-heading-for-rubric"
  (it "looks the rubric up in the registered rubrics file"
    (let ((file (make-temp-file "org-canvas-rubrics-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Essay Rubric\n:PROPERTIES:\n:CANVAS_ID: 133477\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--submissions-rubrics-file) (lambda () file)))
              (expect (org-canvas--submissions-heading-for-rubric "133477") :to-equal "Essay Rubric")
              (expect (org-canvas--submissions-heading-for-rubric "1") :to-be nil)))
        (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
        (delete-file file)))))

(describe "refresh-links also rebuilds the Rubric line"
  (it "recomputes it from the CANVAS_RUBRIC_ID property"
    (with-grading-file (concat test-grading-file-header
                               "#+PROPERTY: CANVAS_RUBRIC_ID 133477\n#+PROPERTY: CANVAS_RUBRIC_TITLE Old Rubric\nAssignment: [[https://x/courses/1/assignments/1001][on Canvas]], [[https://x/speed][SpeedGrader]]\nRubric: [[file:../rubrics.org::*Old Rubric][in Org]], [[https://x/courses/1/rubrics/133477][on Canvas]]\n\n* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (cl-letf (((symbol-function 'org-canvas--submissions-heading-for-assignment) (lambda (_id) nil))
                ((symbol-function 'org-canvas--submissions-heading-for-rubric) (lambda (_id) "Renamed Rubric"))
                ((symbol-function 'org-canvas--submissions-rubrics-file)
                 (lambda () (expand-file-name "../rubrics.org" dir))))
        (expect (org-canvas--submissions-refresh-links) :to-be-truthy)
        (expect (buffer-string) :to-match "^Rubric: \\[\\[file:\\.\\./rubrics\\.org::\\*Renamed Rubric\\]\\[in Org\\]\\]")
        (expect (buffer-string) :not :to-match "Old Rubric\\]")
        (expect (count-matches "^Rubric: " (point-min) (point-max)) :to-equal 1)))))

(describe "org-canvas--submissions-fetch-assignment"
  (it "GETs the assignment"
    (with-org-canvas-test-config
      (with-mock-api
        (org-canvas--submissions-fetch-assignment "1001")
        (expect-api-called 'GET "assignments/1001"))))
  (it "is nil when the request fails"
    (cl-letf (((symbol-function 'org-canvas-api-request) (lambda (&rest _) (error "down"))))
      (expect (org-canvas--submissions-fetch-assignment "1001") :to-be nil)))

  (it "says so with one warning when the request fails (issue #435)"
    (let ((logged nil))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "down")))
                ((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
        (expect (org-canvas--submissions-fetch-assignment "1001") :to-be nil)
        (expect (length logged) :to-equal 1)
        (expect (car logged) :to-match "Could not read assignment 1001")))))

;;;; Drafted Comments

(describe "DAYS_LATE"
  (it "is the lateness rounded up to whole days, only when late"
    (expect (org-canvas--submissions-days-late '((submitted_at . "2026-02-15T23:45:00Z") (seconds_late . 133200))) :to-equal 2)
    (expect (org-canvas--submissions-days-late '((submitted_at . "2026-02-15T23:45:00Z") (seconds_late . 86400))) :to-equal 1)
    (expect (org-canvas--submissions-days-late '((submitted_at . "2026-02-15T23:45:00Z") (seconds_late . 0))) :to-be nil)
    ;; a missing submission carries seconds_late as time since due; that is not lateness
    (expect (org-canvas--submissions-days-late '((submitted_at . nil) (seconds_late . 1000000))) :to-be nil)
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((late . t) (seconds_late . 400000))))
      (expect (buffer-string) :to-match ":DAYS_LATE: 5\n"))))

(defvar test-org-canvas-subs--rubrics-file "/tmp/org-canvas-test-rubrics.org"
  "File variable a fake rubrics feature registration points at.")

(describe "org-canvas--submissions-rubrics-file"
  (it "returns the file the rubrics feature registers"
    (let ((org-canvas--feature-registry
           (list (list :name "Rubrics" :endpoint "rubrics"
                       :file-var 'test-org-canvas-subs--rubrics-file))))
      (expect (org-canvas--submissions-rubrics-file)
              :to-equal test-org-canvas-subs--rubrics-file)))

  (it "is nil when no rubrics feature is registered"
    (let ((org-canvas--feature-registry nil))
      (expect (org-canvas--submissions-rubrics-file) :to-be nil))))

(describe "org-canvas--submissions-render-rubric-properties"
  (it "writes the points and no rubric keywords when none is attached"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties '((points_possible . 10)))
      (expect (buffer-string) :to-equal "#+PROPERTY: POINTS_POSSIBLE 10\n")))

  (it "writes the rubric id and title beside the points"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties
       '((points_possible . 12.5)
         (rubric_settings . ((id . 7) (title . "Essay")))))
      (expect (buffer-string) :to-match "POINTS_POSSIBLE 12\\.5\n")
      (expect (buffer-string) :to-match "CANVAS_RUBRIC_ID 7\n")
      (expect (buffer-string) :to-match "CANVAS_RUBRIC_TITLE Essay\n"))))

(provide 'org-canvas-submissions-test)
;;; org-canvas-submissions-test.el ends here
