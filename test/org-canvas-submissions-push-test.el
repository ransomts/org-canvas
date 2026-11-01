;;; org-canvas-submissions-push-test.el --- Tests for org-canvas-submissions-push -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buttercup tests for writing and posting grades.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-push)

(describe "org-canvas--submissions-parse-score"
  (it "parses integer string"
    (expect (org-canvas--submissions-parse-score "92") :to-equal "92"))

  (it "parses decimal string"
    (expect (org-canvas--submissions-parse-score "85.5") :to-equal "85.5"))

  (it "extracts numerator from score/points format"
    (expect (org-canvas--submissions-parse-score "95/100") :to-equal "95"))

  (it "trims whitespace"
    (expect (org-canvas--submissions-parse-score " 92 ") :to-equal "92"))

  (it "returns nil for non-numeric input"
    (expect (org-canvas--submissions-parse-score "abc") :to-be nil))

  (it "returns nil for empty string"
    (expect (org-canvas--submissions-parse-score "") :to-be nil))

  (it "returns nil for nil"
    (expect (org-canvas--submissions-parse-score nil) :to-be nil)))

;;;; Snapshot Scores

(describe "org-canvas--submissions-snapshot-scores"
  (it "builds alist from submissions"
    (let* ((subs (list (test-org-canvas-make-submission '((score . 92)))
                       (test-org-canvas-make-submission
                        '((score . nil)
                          (user . ((id . 5002) (sortable_name . "Beta, Bob")))))))
           (snapshot (org-canvas--submissions-snapshot-scores subs)))
      (expect (alist-get 5001 snapshot) :to-equal "92")
      (expect (alist-get 5002 snapshot) :to-be nil)))

  (it "formats decimal scores"
    (let* ((subs (list (test-org-canvas-make-submission '((score . 85.5)))))
           (snapshot (org-canvas--submissions-snapshot-scores subs)))
      (expect (alist-get 5001 snapshot) :to-equal "85.5"))))

;;;; Change Detection — Detail View

(describe "org-canvas--submissions-collect-detail-changes"
  (it "detects changed score"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (setq-local org-canvas-submissions--data nil)
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :user-id) :to-equal 5001)
       (expect (plist-get (car changes) :new-score) :to-equal "95")
       (expect (plist-get (car changes) :old-score) :to-equal "92"))))

  (it "ignores unchanged scores"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 0))))

  (it "detects new score where none existed"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 88\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . nil)))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :new-score) :to-equal "88"))))

  (it "detects multiple changes"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n\n* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores
                 '((5001 . "92") (5002 . "75")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 2))))

  (it "handles score/points format in SCORE property"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95/100\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (plist-get (car changes) :new-score) :to-equal "95")))))

;;;; Change Detection — Dispatcher

(describe "org-canvas--submissions-collect-grade-changes"
  (it "dispatches to detail collector in detail view"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
     (let ((changes (org-canvas--submissions-collect-grade-changes)))
       (expect (length changes) :to-equal 1))))

  (it "dispatches to summary collector in summary view"
    (with-temp-buffer
      (org-mode)
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 99 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-grade-changes)))
        (expect (length changes) :to-equal 1)))))

;;;; Change Detection — Summary View

(describe "org-canvas--submissions-collect-summary-changes"
  (it "detects changed score in table"
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Submissions: HW\n\n")
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 95/100 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-summary-changes)))
        (expect (length changes) :to-equal 1)
        (expect (plist-get (car changes) :new-score) :to-equal "95"))))

  (it "ignores unchanged scores in table"
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Submissions: HW\n\n")
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 92/100 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-summary-changes)))
        (expect (length changes) :to-equal 0))))

  (it "maps student name to user ID via cached data"
    (with-temp-buffer
      (org-mode)
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | submitted | <2026-02-15> | 99 |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)))
      (let ((changes (org-canvas--submissions-collect-summary-changes)))
        (expect (plist-get (car changes) :user-id) :to-equal 5001)))))

;;;; Push Functions

(describe "org-canvas--submissions-push-single-grade"
  (it "sends PUT with correct payload"
    (with-test-api
      (org-canvas--submissions-push-single-grade
       "1001" (list :user-id 5001 :name "A" :old-score "90" :new-score "95"))
      (expect-api-called 'PUT "assignments/1001/submissions/5001")
      (let* ((call (test-org-canvas-last-api-call))
             (data (nth 2 call)))
        (expect (alist-get 'posted_grade (alist-get 'submission data))
                :to-equal "95")
        (expect (assq 'rubric_assessment data) :to-be nil))))
  (it "sends the rubric assessment beside the grade, and alone when only it moved"
    (with-test-api
      (org-canvas--submissions-push-single-grade
       "1001" (list :user-id 5001 :name "A" :old-score "90" :new-score "4"
                    :score-derived t
                    :triples '(("_7104" "2" "Sharp") ("_7105" "2" nil) ("_7106" nil nil))))
      (let ((data (nth 2 (test-org-canvas-last-api-call))))
        (expect (alist-get 'posted_grade (alist-get 'submission data)) :to-equal "4")
        (expect (alist-get 'rubric_assessment data)
                :to-equal '((_7104 . ((points . 2) (comments . "Sharp")))
                            (_7105 . ((points . 2))))))
      (org-canvas--submissions-push-single-grade
       "1001" (list :user-id 5001 :name "A" :old-score "90" :new-score "90"
                    :triples '(("_7104" nil "Late but fine"))))
      (let ((data (nth 2 (test-org-canvas-last-api-call))))
        (expect (assq 'submission data) :to-be nil)
        (expect (alist-get 'rubric_assessment data)
                :to-equal '((_7104 . ((comments . "Late but fine")))))))))

(describe "org-canvas--submissions-push-bulk-grades"
  (it "sends POST with grade_data payload"
    (with-test-api
      (let ((changes (list (list :user-id 5001 :name "A" :old-score "90" :new-score "95")
                           (list :user-id 5002 :name "B" :old-score "80" :new-score "85"))))
        (org-canvas--submissions-push-bulk-grades "1001" changes)
        (expect-api-called 'POST "assignments/1001/submissions/update_grades")
        (let* ((call (test-org-canvas-last-api-call))
               (data (nth 2 call))
               (grade-data (alist-get 'grade_data data)))
          (expect (alist-get 'posted_grade (alist-get "5001" grade-data nil nil #'equal))
                  :to-equal "95")
          (expect (alist-get 'posted_grade (alist-get "5002" grade-data nil nil #'equal))
                  :to-equal "85"))))))

;;;; Push Command

(describe "unsubmitted student grading"
  (it "detects nil→score transition"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 75\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . nil)))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :old-score) :to-be nil)
       (expect (plist-get (car changes) :new-score) :to-equal "75")))))

;;;; Grading Files (file-backed round trip)

(describe "CANVAS_SCORE baseline in change detection"
  (it "compares SCORE against CANVAS_SCORE, ignoring a stale snapshot"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:ATTEMPT: 1\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores '((5001 . "95")))
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (length changes) :to-equal 1)
       (expect (plist-get (car changes) :old-score) :to-equal "92")
       (expect (plist-get (car changes) :new-score) :to-equal "95")
       (expect (plist-get (car changes) :attempt) :to-equal 1))))
  (it "sees no change when SCORE equals CANVAS_SCORE"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores nil)
     (expect (org-canvas--submissions-collect-detail-changes) :to-equal nil)))
  (it "treats a first grade on an ungraded heading as a change"
    (with-temp-org-buffer
     "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 5\n:END:\n"
     (setq-local org-canvas-submissions--current-view 'detail)
     (setq-local org-canvas-submissions--original-scores nil)
     (let ((changes (org-canvas--submissions-collect-detail-changes)))
       (expect (plist-get (car changes) :old-score) :to-be nil)
       (expect (plist-get (car changes) :new-score) :to-equal "5")))))

(describe "org-canvas--submissions-live-baselines"
  (it "maps each student to the entered score string and attempt"
    (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
               (lambda (_id)
                 (list (test-org-canvas-make-submission
                        '((entered_score . 5.0) (score . 4.0) (attempt . 2)))
                       (test-org-canvas-make-submission
                        '((score . nil) (attempt . nil)
                          (user . ((id . 5002) (sortable_name . "Beta, Bob")))))))))
      (let ((live (org-canvas--submissions-live-baselines "1001")))
        (expect (alist-get 5001 live) :to-equal '("5" 2 nil nil))
        (expect (alist-get 5002 live) :to-equal '(nil nil nil nil)))))
  (it "carries the digest of each student's assessment"
    (let ((sub (test-org-canvas-make-submission
                '((rubric_assessment . ((_7104 . ((points . 2)))))))))
      (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                 (lambda (_id) (list sub))))
        (expect (nth 2 (alist-get 5001 (org-canvas--submissions-live-baselines "1001")))
                :to-equal (org-canvas--submissions-rubric-digest '(("_7104" "2" nil))))))))

(describe "clearing a grade (issue #417)"
  (defun test-clear--entry (name user-id props)
    "Return a student heading for NAME and USER-ID with PROPS lines."
    (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n%s:END:\n" name user-id props))

  (it "reads a typed SCORE: blank keeps the baseline, a clear word is no grade"
    (expect (org-canvas--submissions-typed-score nil "92" "A") :to-equal "92")
    (expect (org-canvas--submissions-typed-score "  " "92" "A") :to-equal "92")
    (expect (org-canvas--submissions-typed-score "none" "92" "A") :to-be nil)
    (expect (org-canvas--submissions-typed-score " None " "92" "A") :to-be nil)
    (expect (org-canvas--submissions-typed-score "-" "92" "A") :to-be nil)
    (expect (org-canvas--submissions-typed-score "95" "92" "A") :to-equal "95")
    (expect (org-canvas--submissions-typed-score "excused" nil "A") :to-equal "EX")
    (expect (org-canvas--submissions-typed-score "9O" "92" "A") :to-throw 'user-error))

  (it "pushes the empty grade and takes SCORE and CANVAS_SCORE away"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
        (let ((org-canvas-submissions-check-conflicts nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades)))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
        (let ((data (nth 2 (test-org-canvas-last-api-call))))
          (expect (alist-get 'submission data) :to-equal '((posted_grade . ""))))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-be nil)
        (expect (org-entry-get (point) "SCORE") :to-be nil)
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil))))

  (it "leaves a grade alone when SCORE is absent or blank"
    (with-grading-file (concat test-grading-file-header
                               (test-clear--entry "Adams, Alice" 5001 ":CANVAS_SCORE: 92\n")
                               (test-clear--entry "Beta, Bob" 5002 ":SCORE:\n:CANVAS_SCORE: 80\n"))
      (org-canvas--submissions-ensure-context)
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
      (expect (org-canvas--submissions-collect-carryover) :to-be nil)))

  (it "sees no change in a clear where Canvas holds no grade"
    (with-grading-file (concat test-grading-file-header
                               (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n"))
      (org-canvas--submissions-ensure-context)
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
      (expect (org-canvas--submissions-collect-carryover) :to-be nil)))

  (it "refuses a SCORE it cannot read before anything is sent"
    (with-grading-file (concat test-grading-file-header
                               (test-clear--entry "Adams, Alice" 5001 ":SCORE: 9O\n:CANVAS_SCORE: 92\n"))
      (org-canvas--submissions-ensure-context)
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))

  (it "sends a clear through update_grades beside a score, and names it"
    (let ((asked nil))
      (with-test-api
        (setq test-org-canvas-api-responses
              '(("update_grades" . ((id . 77) (workflow_state . "completed")))))
        (with-grading-file (concat test-grading-file-header
                                   (test-clear--entry "Adams, Alice" 5001 ":SCORE: -\n:CANVAS_SCORE: 92\n")
                                   (test-clear--entry "Beta, Bob" 5002 ":SCORE: 85\n:CANVAS_SCORE: 80\n"))
          (let ((org-canvas-submissions-check-conflicts nil)
                (org-canvas-assume-yes nil)
                (noninteractive nil))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (q) (if asked (error "Asked twice: %s" q) (setq asked q)) t))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (q) (if asked (error "Asked twice: %s" q) (setq asked q)) t)))
              (org-canvas--submissions-push-current t)))
          (let* ((call (test-org-canvas-find-api-call 'POST "update_grades"))
                 (grade-data (alist-get 'grade_data (nth 2 call))))
            (expect (alist-get "5001" grade-data nil nil #'equal)
                    :to-equal '((posted_grade . "")))
            (expect (alist-get "5002" grade-data nil nil #'equal)
                    :to-equal '((posted_grade . "85"))))
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "SCORE") :to-be nil)
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-be nil)
          (org-canvas--submissions-goto-user 5002)
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "85")))
      (expect asked :to-match "2 grade change(s) (1 clearing a grade)")))

  (it "shows a clear as clear in the list of changes"
    (expect (org-canvas--submissions-describe-changes
             (list (list :name "Adams, Alice" :old-score "92" :new-score nil :clear t)))
            :to-equal "  Adams, Alice: 92 → clear"))

  (it "sends nothing under a dry run and records nothing"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
        (let ((org-canvas-submissions-check-conflicts nil)
              (org-canvas--dry-run t))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades)))
        (expect (test-org-canvas-api-call-count) :to-equal 0)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "none")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92"))))

  (it "keeps a typed clear over a re-pull until Canvas holds no grade"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
        (test-refresh--from-canvas '((score . 92) (entered_score . 92)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "none")
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "92")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (plist-get (car (org-canvas--submissions-collect-grade-changes)) :clear)
                :to-be t)
        (test-refresh--from-canvas '((score . nil) (entered_score . nil)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-be nil)
        (expect (org-entry-get (point) "CONFLICT") :to-be nil))))

  (it "marks a typed clear when Canvas's grade moved under it"
    (with-org-canvas-test-config
      (with-grading-file (concat test-grading-file-header
                                 (test-clear--entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 92\n"))
        (test-refresh--from-canvas '((score . 93) (entered_score . 93)))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "SCORE") :to-equal "none")
        (expect (org-entry-get (point) "CONFLICT") :to-equal "score: Canvas has 93"))))

  (it "reads a clear word and a blank cell in the summary table"
    (with-temp-buffer
      (org-mode)
      (insert "| Student | Status | Submitted At | Score |\n")
      (insert "|---------+--------+--------------+-------|\n")
      (insert "| Adams, Alice | graded | <2026-02-15> | none |\n")
      (insert "| Beta, Bob | graded | <2026-02-15> |  |\n")
      (org-table-align)
      (setq-local org-canvas-submissions--current-view 'summary)
      (setq-local org-canvas-submissions--original-scores '((5001 . "92") (5002 . "80")))
      (setq-local org-canvas-submissions--data
                  (list (test-org-canvas-make-submission)
                        (test-org-canvas-make-submission
                         '((user . ((id . 5002) (sortable_name . "Beta, Bob")))))))
      (let ((changes (org-canvas--submissions-collect-grade-changes)))
        (expect (length changes) :to-equal 1)
        (expect (plist-get (car changes) :user-id) :to-equal 5001)
        (expect (plist-get (car changes) :clear) :to-be t)
        (expect (org-canvas--submissions-grade-fields (car changes))
                :to-equal '((posted_grade . ""))))))

  (it "refuses a clear beside changed Rubric rows that set the grade"
    (with-rubric-file (concat test-rubric-file-header
                              (test-rubric-entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 3\n"
                                                 '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 nil nil)
                                                   ("_7106" "Style" 1 nil nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))

  (it "clears the grade alone when the Rubric rows did not change"
    (with-rubric-file (concat test-rubric-file-header
                              (test-rubric-entry "Adams, Alice" 5001 ":SCORE: none\n:CANVAS_SCORE: 3\n"
                                                 test-rubric-blank-rows))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :clear) :to-be t)
        (expect (plist-get change :triples) :to-be nil)
        (expect (org-canvas--submissions-grade-fields change)
                :to-equal '((posted_grade . "")))))))

;;;; Links and Local Attachments

(describe "excused submissions"
  (it "parse EX in any spelling"
    (expect (org-canvas--submissions-parse-score "EX") :to-equal "EX")
    (expect (org-canvas--submissions-parse-score " excused ") :to-equal "EX")
    (expect (org-canvas--submissions-parse-score "extra") :to-be nil))
  (it "render as SCORE EX with no FINAL_SCORE and snapshot as EX"
    (let ((sub (test-org-canvas-make-submission '((excused . t) (score . nil)))))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry sub)
        (expect (buffer-string) :to-match ":SCORE: EX\n:CANVAS_SCORE: EX\n")
        (expect (buffer-string) :not :to-match "FINAL_SCORE"))
      (expect (alist-get 5001 (org-canvas--submissions-snapshot-scores (list sub))) :to-equal "EX")))
  (it "push EX as the posted grade"
    (with-test-api
      (with-grading-file (concat test-grading-file-header
                                 "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: ex\n:CANVAS_SCORE: 92\n:END:\n")
        (let ((org-canvas-submissions-check-conflicts nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (org-canvas-submissions-push-grades)))
        (let ((data (nth 2 (test-org-canvas-last-api-call))))
          (expect (alist-get 'posted_grade (alist-get 'submission data)) :to-equal "EX"))
        (expect (buffer-string) :to-match ":CANVAS_SCORE: EX")))))

(describe "posting grades (issue #202)"
  (it "records the assignment's effective policy in the file header"
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties '((points_possible . 10) (post_manually . t)))
      (expect (buffer-string) :to-match "#\\+PROPERTY: POST_POLICY manual\n"))
    (with-temp-buffer
      (org-canvas--submissions-render-rubric-properties '((points_possible . 10)))
      (expect (buffer-string) :not :to-match "POST_POLICY")))

  (it "renders POSTED_AT for a posted submission and nothing for an unposted one"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((score . 5) (posted_at . "2026-09-10T15:00:00Z"))) "1001")
      (expect (buffer-string) :to-match ":POSTED_AT: <2026-09-10"))
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((score . 5) (posted_at . :null))) "1001")
      (expect (buffer-string) :not :to-match "POSTED_AT"))
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((score . 5) (posted_at . "not a date"))) "HW" "1001")
      (expect (buffer-string) :to-match ":POSTED_AT: not a date")))

  (it "reads the policy from the file, so no request is needed at push time"
    (with-grading-file (concat test-grading-file-header "#+PROPERTY: POST_POLICY manual\n* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (expect (org-canvas--submissions-post-manually-p) :to-be t))
    (with-grading-file (concat test-grading-file-header "* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (expect (org-canvas--submissions-post-manually-p) :to-be nil)))

  (it "offers to post after a push only under a manual policy, and posts on yes"
    (with-grading-file (concat test-grading-file-header "#+PROPERTY: POST_POLICY manual\n* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((asked nil) (posted nil) (noninteractive nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) t))
                  ((symbol-function 'org-canvas-submissions-post-grades) (lambda () (setq posted t))))
          (org-canvas--submissions-offer-to-post 1)
          (expect asked :to-be t)
          (expect posted :to-be t)
          (setq asked nil posted nil)
          (org-canvas--submissions-offer-to-post 0)
          (expect asked :to-be nil))))
    (with-grading-file (concat test-grading-file-header "* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((asked nil) (noninteractive nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) t)))
          (org-canvas--submissions-offer-to-post 1))
        (expect asked :to-be nil))))

  (it "never posts from a batch Emacs, even with every prompt assumed yes (issue #381)"
    (with-grading-file (concat test-grading-file-header "#+PROPERTY: POST_POLICY manual\n* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((asked nil) (posted nil) (said nil)
            (noninteractive t) (org-canvas-assume-yes t))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) t))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args))))
                  ((symbol-function 'org-canvas-submissions-post-grades) (lambda () (setq posted t))))
          (org-canvas--submissions-offer-to-post 3))
        (expect asked :to-be nil)
        (expect posted :to-be nil)
        (expect said :to-match "Grades held under the manual post policy"))))

  (it "posts the assignment's grades through the mutation"
    (with-grading-file (concat test-grading-file-header "* A\n:PROPERTIES:\n:USER_ID: 1\n:END:\n")
      (let ((seen nil))
        (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                   (lambda (what doc vars) (setq seen (list what doc vars)) nil)))
          (org-canvas-submissions-post-grades))
        (expect (nth 1 seen) :to-match "postAssignmentGrades")
        (expect (alist-get 'assignmentId (nth 2 seen)) :to-equal "1001"))))

  (it "refuses outside a grading buffer and without an assignment id"
    (with-temp-buffer
      (expect (org-canvas-submissions-post-grades) :to-throw 'user-error))
    (with-temp-buffer
      (org-mode)
      (insert "* A\n")
      (setq-local org-canvas-submissions--assignment-id nil)
      (org-canvas-submissions-mode 1)
      (expect (org-canvas--submissions-push-current nil) :to-throw 'user-error)
      (expect (org-canvas-submissions-post-grades) :to-throw 'user-error)))

  (it "binds P in the grading file and lists it in the menu"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "P")) :to-be #'org-canvas-submissions-post-grades)))


;;;; Rubric Assessments From the Grading File (issue #250)

(describe "rubric change detection"
  (it "sees no change while the rows match the baseline"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001
                                                  (format ":SCORE: 4\n:CANVAS_SCORE: 4\n:CANVAS_RUBRIC: %s\n"
                                                          (org-canvas--submissions-rubric-digest
                                                           '(("_7104" "2" "Sharp") ("_7105" "2" nil))))
                                                  '(("_7104" "Thesis" 2 "2.0" "Sharp") ("_7105" "Evidence" 3 2 nil)
                                                    ("_7106" "Style" 1 nil nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
      (expect (org-canvas--submissions-pending-count) :to-equal 0)))
  (it "reports edited rows with their digests, total, and how many are scored"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" "Thesis" 2 2 "Sharp") ("_7105" "Evidence" 3 1.5 nil)
                                                    ("_7106" "Style" 1 nil nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :user-id) :to-equal 5001)
        (expect (plist-get change :triples)
                :to-equal '(("_7104" "2" "Sharp") ("_7105" "1.5" nil) ("_7106" nil nil)))
        (expect (plist-get change :old-rubric) :to-be nil)
        (expect (plist-get change :new-rubric)
                :to-equal (org-canvas--submissions-rubric-digest '(("_7104" "2" "Sharp") ("_7105" "1.5" nil))))
        (expect (plist-get change :total) :to-equal "3.5")
        (expect (plist-get change :filled) :to-equal 2)
        (expect (plist-get change :of) :to-equal 3))))
  (it "derives the score from the rows when the rubric is used for grading"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 3\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil)
                                                    ("_7106" "Style" 1 1 nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :new-score) :to-equal "6")
        (expect (plist-get change :old-score) :to-equal "3")
        (expect (plist-get change :score-derived) :to-be t)
        (expect (org-canvas--submissions-describe-changes (list change))
                :to-equal "  Adams, Alice: 3 → 6 (rubric 3/3)"))))
  (it "keeps a score edited to the rows' total, and refuses one that disagrees"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 5.0\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil)
                                                    ("_7106" "Style" 1 nil nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :new-score) :to-equal "5.0")
        (expect (plist-get change :score-derived) :to-be nil))
      (org-canvas--submissions-goto-user 5001)
      (org-entry-put (point) "SCORE" "4")
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))
  (it "leaves the score alone when the rubric is not used for grading"
    (with-rubric-file (concat test-grading-file-header
                               "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING false\n\n"
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 3\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :new-score) :to-equal "3")
        (expect (plist-get change :score-derived) :to-be nil)
        (expect (org-canvas--submissions-change-sends-grade-p change) :to-be nil)
        (expect (plist-get change :triples) :to-be-truthy))))
  (it "refuses a score that is not a number or exceeds the criterion's points"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" nil 2 "two" nil))))
      (expect (org-canvas--submissions-collect-grade-changes)
              :to-throw 'user-error '("Rubric score \"two\" for Adams, Alice (_7104) is not a number")))
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" "Thesis" 2 3 nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error))
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" "Thesis" 2 "EX" nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))
  (it "does not count rubric edits, or the score they derive, as edits a re-pull loses"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 3\n:CANVAS_SCORE: 3\n"
                                                  '(("_7104" "Thesis" 2 2 nil) ("_7105" "Evidence" 3 3 nil)))
                               "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 4\n:CANVAS_SCORE: 2\n:END:\n")
      (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 2)
      (expect (org-canvas--submissions-pending-count) :to-equal 1)))
  (it "refuses a table emptied by hand over an assessment, naming none"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001
                                                  ":SCORE: 2\n:CANVAS_SCORE: 2\n:CANVAS_RUBRIC: abcdef012345\n"
                                                  test-rubric-blank-rows))
      (expect (org-canvas--submissions-collect-grade-changes)
              :to-throw 'user-error
              '("Adams, Alice: the Rubric rows are empty but Canvas holds an assessment; write none in their Score cells to clear it"))))
  (it "sees no change in an empty table where Canvas holds no assessment"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 2\n:CANVAS_SCORE: 2\n"
                                                  test-rubric-blank-rows))
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil))))

(defun test-grade-after--push (content &optional replies)
  "Push the grading file CONTENT with conflicts unchecked; return the calls, oldest first.
REPLIES is the mock API's response alist."
  (let ((calls nil))
    (with-test-api
      (setq test-org-canvas-api-responses replies)
      (with-rubric-file content
        (let ((org-canvas-submissions-check-conflicts nil))
          (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) t)))
            (org-canvas--submissions-push-current t)))
        (setq calls (reverse test-org-canvas-api-calls))
        (org-canvas--submissions-goto-user 5001)
        (setq calls (list :calls calls
                          :score (org-entry-get (point) "SCORE")
                          :baseline (org-entry-get (point) "CANVAS_SCORE")
                          :rubric (org-entry-get (point) "CANVAS_RUBRIC")))))
    calls))

(describe "a grade beside comment-only rubric rows (issue #444)"
  (it "keeps an unedited score, derives nothing, and sends it after the rows"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 5\n:CANVAS_SCORE: 5\n"
                                                  test-grade-after--rows))
      (let* ((change (car (org-canvas--submissions-collect-grade-changes)))
             (fields (org-canvas--submissions-grade-fields change)))
        (expect (plist-get change :new-score) :to-equal "5")
        (expect (plist-get change :score-derived) :to-be nil)
        (expect (plist-get change :grade-after) :to-be t)
        (expect (assq 'posted_grade fields) :to-be nil)
        (expect (assq 'rubric_assessment fields) :to-be-truthy)
        (expect (org-canvas--submissions-trailing-grade-fields change)
                :to-equal '((posted_grade . "5")))
        (expect (org-canvas--submissions-pending-count) :to-equal 0))))

  (it "sends a typed score after the rows, and nothing after them on an ungraded row"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 0\n" test-grade-after--rows)
                               (test-rubric-entry "Beta, Bob" 5002 "" test-grade-after--rows))
      (let ((changes (org-canvas--submissions-collect-grade-changes)))
        (expect (plist-get (nth 0 changes) :grade-after) :to-be t)
        (expect (plist-get (nth 0 changes) :new-score) :to-equal "0")
        (expect (plist-get (nth 1 changes) :grade-after) :to-be nil)
        (expect (plist-get (nth 1 changes) :new-score) :to-be nil)
        (expect (org-canvas--submissions-grade-fields (nth 1 changes))
                :to-equal (list (assq 'rubric_assessment
                                      (org-canvas--submissions-grade-fields (nth 0 changes))))))))

  (it "sends the grade beside the rows when they carry points, or the rubric is not for grading"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 2\n:CANVAS_SCORE: 0\n"
                                                  '(("_7104" "Thesis" 2 2 "Good"))))
      (expect (plist-get (car (org-canvas--submissions-collect-grade-changes)) :grade-after)
              :to-be nil))
    (with-rubric-file (concat test-grading-file-header
                               "#+PROPERTY: CANVAS_RUBRIC_USE_FOR_GRADING false\n\n"
                               (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 5\n:CANVAS_SCORE: 5\n"
                                                  test-grade-after--rows))
      (expect (plist-get (car (org-canvas--submissions-collect-grade-changes)) :grade-after)
              :to-be nil)))

  (it "PUTs the rows first and the grade second, then records the baselines"
    (let* ((r (test-grade-after--push
               (concat test-rubric-file-header
                       (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 5\n:CANVAS_SCORE: 5\n"
                                          test-grade-after--rows))))
           (calls (plist-get r :calls)))
      (expect (mapcar #'car calls) :to-equal '(PUT PUT))
      (expect (assq 'submission (nth 2 (nth 0 calls))) :to-be nil)
      (expect (assq 'rubric_assessment (nth 2 (nth 0 calls))) :to-be-truthy)
      (expect (nth 2 (nth 1 calls)) :to-equal '((submission . ((posted_grade . "5")))))
      (expect (plist-get r :score) :to-equal "5")
      (expect (plist-get r :baseline) :to-equal "5")
      (expect (plist-get r :rubric)
              :to-equal (org-canvas--submissions-rubric-digest
                         '(("_7104" nil "Read it again.") ("_7106" nil "Tidy."))))))

  (it "sends the trailing grades once the bulk job carrying the rows completed"
    (let* ((r (test-grade-after--push
               (concat test-rubric-file-header
                       (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 10\n" test-grade-after--rows)
                       (test-rubric-entry "Beta, Bob" 5002 ":SCORE: 4\n:CANVAS_SCORE: 2\n"
                                          test-grade-after--rows)
                       "* Cole, Cy\n:PROPERTIES:\n:USER_ID: 5003\n:SCORE: 4\n:CANVAS_SCORE: 2\n:END:\n")
               '(("update_grades" . ((id . 77) (workflow_state . "completed"))))))
           (posts (seq-filter (lambda (c) (eq (car c) 'POST)) (plist-get r :calls)))
           (first (alist-get 'grade_data (nth 2 (nth 0 posts))))
           (second (alist-get 'grade_data (nth 2 (nth 1 posts)))))
      (expect (length posts) :to-equal 2)
      (expect (assq 'posted_grade (alist-get "5001" first nil nil #'equal)) :to-be nil)
      (expect (assq 'rubric_assessment (alist-get "5001" first nil nil #'equal)) :to-be-truthy)
      (expect (alist-get 'posted_grade (alist-get "5003" first nil nil #'equal)) :to-equal "4")
      (expect second :to-equal '(("5001" . ((posted_grade . "10")))
                                 ("5002" . ((posted_grade . "4")))))
      (expect (plist-get r :baseline) :to-equal "10")))

  (it "sends no trailing grade when the first job failed, and records nothing"
    (let* ((r (test-grade-after--push
               (concat test-rubric-file-header
                       (test-rubric-entry "Adams, Alice" 5001 ":SCORE: 10\n" test-grade-after--rows)
                       "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 4\n:CANVAS_SCORE: 2\n:END:\n")
               '(("update_grades" . ((id . 77) (workflow_state . "failed"))))))
           (posts (seq-filter (lambda (c) (eq (car c) 'POST)) (plist-get r :calls))))
      (expect (length posts) :to-equal 1)
      (expect (plist-get r :baseline) :to-be nil)
      (expect (plist-get r :rubric) :to-be nil)))

  (it "names the trailing grade in a dry run"
    (let ((change (list :old-score "5" :new-score "5" :grade-after t
                        :triples '(("_7104" nil "Tidy.")))))
      (expect (org-canvas--submissions-would-send-line "Adams, Alice" change nil)
              :to-equal (concat "Adams, Alice: {\"rubric_assessment\":{\"_7104\":"
                                "{\"comments\":\"Tidy.\"}}}; then {\"posted_grade\":\"5\"}"))
      (let ((org-canvas--dry-run t) (logged nil))
        (cl-letf (((symbol-function 'org-canvas--log-info)
                   (lambda (_logger fmt &rest args) (setq logged (apply #'format fmt args)))))
          (expect (org-canvas--submissions-send-grades "1001" (list change))
                  :to-equal '(:state dry-run)))
        (expect logged :to-match "then 1 grade(s) after their rubric assessments")))))

(defconst test-clear-rows--digest
  (org-canvas--submissions-rubric-digest '(("_7104" "12" "Strong") ("_7105" "4" nil)))
  "The CANVAS_RUBRIC of an assessment the grading file clears.")

(describe "clearing rubric rows with none (issue #449)"
  (it "reads none as a cleared row, sent with empty points and its comment"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001
                                                  (format ":SCORE: 16\n:CANVAS_SCORE: 16\n:CANVAS_RUBRIC: %s\n"
                                                          test-clear-rows--digest)
                                                  '(("_7104" "Thesis" 2 "none" nil) ("_7105" "Evidence" 3 " - " "Kept.")
                                                    ("_7106" "Style" 1 nil nil))))
      (let* ((change (car (org-canvas--submissions-collect-grade-changes)))
             (fields (org-canvas--submissions-grade-fields change)))
        (expect (plist-get change :triples)
                :to-equal '(("_7104" nil nil t) ("_7105" nil "Kept." t) ("_7106" nil nil)))
        (expect (plist-get change :cleared) :to-equal 2)
        (expect (plist-get change :new-rubric)
                :to-equal (org-canvas--submissions-rubric-digest '(("_7105" nil "Kept."))))
        (expect (alist-get 'rubric_assessment fields)
                :to-equal '((_7104 . ((points . "") (comments . "")))
                            (_7105 . ((points . "") (comments . "Kept.")))))
        (expect (plist-get change :grade-after) :to-be t)
        (expect (org-canvas--submissions-describe-changes (list change))
                :to-equal "  Adams, Alice: 16 → 16 (rubric 0/3, 2 cleared)"))))

  (it "sees no change in rows cleared where Canvas holds no assessment"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001 ""
                                                  '(("_7104" "Thesis" 2 "none" nil))))
      (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)))

  (it "lets SCORE none stand beside rows that carry no points, and not beside points"
    (with-rubric-file (concat test-rubric-file-header
                               (test-rubric-entry "Adams, Alice" 5001
                                                  (format ":SCORE: none\n:CANVAS_SCORE: 16\n:CANVAS_RUBRIC: %s\n"
                                                          test-clear-rows--digest)
                                                  '(("_7104" "Thesis" 2 "none" nil) ("_7105" "Evidence" 3 "none" nil))))
      (let ((change (car (org-canvas--submissions-collect-grade-changes))))
        (expect (plist-get change :clear) :to-be t)
        (expect (plist-get change :grade-after) :to-be nil)
        (expect (alist-get 'posted_grade (org-canvas--submissions-grade-fields change))
                :to-equal ""))
      (org-canvas--submissions-goto-user 5001)
      (org-canvas--submissions-rubric-set-row "_7104" "1" nil)
      (expect (org-canvas--submissions-collect-grade-changes) :to-throw 'user-error)))

  (it "clears the rows, then sends the ruled score, and records both"
    (with-test-api
      (with-rubric-file (concat test-rubric-file-header
                                 (test-rubric-entry "Adams, Alice" 5001
                                                    (format ":SCORE: 0\n:CANVAS_SCORE: 16\n:CANVAS_RUBRIC: %s\n"
                                                            test-clear-rows--digest)
                                                    '(("_7104" "Thesis" 2 "none" nil)
                                                      ("_7105" "Evidence" 3 "none" nil)
                                                      ("_7106" "Style" 1 nil nil))))
        (let ((org-canvas-submissions-check-conflicts nil))
          (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) t)))
            (org-canvas--submissions-push-current t)))
        (let ((calls (reverse test-org-canvas-api-calls)))
          (expect (mapcar #'car calls) :to-equal '(PUT PUT))
          (expect (nth 2 (nth 0 calls))
                  :to-equal '((rubric_assessment . ((_7104 . ((points . "") (comments . "")))
                                                    (_7105 . ((points . "") (comments . "")))))))
          (expect (nth 2 (nth 1 calls)) :to-equal '((submission . ((posted_grade . "0"))))))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CANVAS_RUBRIC") :to-be nil)
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "0")
        (expect (mapcar (lambda (r) (nth 3 r)) (org-canvas--submissions-rubric-rows))
                :to-equal '(nil nil nil))
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil))))

  (it "keeps rows emptied or cleared by hand across a refresh"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (shown (list nil))
           (assessed (test-org-canvas-make-submission
                      '((score . 3) (rubric_assessment . ((_7104 . ((points . 2) (comments . "Sharp")))
                                                          (_7105 . ((points . 1)))))))))
      (unwind-protect
          (progn
            (test-rubric--display (list assessed) shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (dolist (id '("_7104" "_7105" "_7106"))
                (org-canvas--submissions-rubric-set-row id nil nil))
              (save-buffer))
            (test-rubric--display (list assessed) shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (expect (mapcar (lambda (r) (list (nth 3 r) (nth 4 r)))
                              (org-canvas--submissions-rubric-rows))
                      :to-equal '((nil nil) (nil nil) (nil nil)))
              (expect (org-entry-get (point) "CONFLICT") :to-be nil)
              (org-canvas--submissions-rubric-set-row "_7104" "none" nil)
              (save-buffer))
            (test-rubric--display (list assessed) shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (expect (mapcar (lambda (r) (nth 3 r)) (org-canvas--submissions-rubric-rows))
                      :to-equal '("none" nil nil))))
        (when (buffer-live-p (car shown)) (kill-buffer (car shown)))
        (delete-directory dir t)))))

;;;; Document processor reports (issue #351)

(defvar test-late--mutations nil "What `test-late--push' sent through the mutation.")

(defvar test-late--warnings nil "What `test-late--push' logged at WARNING.")

(defun test-late--heading (&rest props)
  "Return Alice's grading-file heading with PROPS and a SUBMISSION_ID."
  (concat test-grading-file-header
          "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SUBMISSION_ID: 50001\n"
          (apply #'concat props) ":END:\n"))

(defun test-late--push (reply &optional live)
  "Push the current grading file; the mutation answers REPLY.
REPLY is a function of the variables, or a value.  LIVE is the
student's live baseline for the conflict check, nil to skip it."
  (setq test-late--mutations nil
        test-late--warnings nil)
  (let ((org-canvas-submissions-check-conflicts (and live t)))
    (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
               (lambda (_what document variables)
                 (push (cons document variables) test-late--mutations)
                 (if (functionp reply) (funcall reply variables) reply)))
              ((symbol-function 'org-canvas--submissions-live-baselines)
               (lambda (_id) (list (cons 5001 live))))
              ((symbol-function 'org-canvas--log-warning)
               (lambda (_logger fmt &rest args)
                 (push (apply #'format fmt args) test-late--warnings)))
              ((symbol-function 'y-or-n-p) (lambda (_) t)))
      (org-canvas-submissions-push-grades))))

(defun test-late--stored (status &optional seconds)
  "Return a mutation reply saying Canvas stored STATUS, SECONDS late."
  `((updateSubmissionGradeStatus
     . ((submission . ((_id . "50001") (latePolicyStatus . ,(or status :null))
                       (secondsLate . ,(or seconds 0))))
        (errors . :null)))))

(describe "the late status in a grading file (issue #352)"
  (it "is written with its baseline when Canvas holds one, and not otherwise"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((late_policy_status . "extended"))))
      (expect (buffer-string) :to-match ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: extended\n"))
    (dolist (status '(nil :null ""))
      (with-temp-buffer
        (org-mode)
        (org-canvas--submissions-render-detail-entry
         (test-org-canvas-make-submission `((late_policy_status . ,status))))
        (expect (buffer-string) :not :to-match "LATE_STATUS"))))
  (it "writes a status Canvas returns that the push does not know, as it comes"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((late_policy_status . "excused_by_policy"))))
      (expect (buffer-string) :to-match ":LATE_STATUS: excused_by_policy\n")
      (goto-char (point-min))
      (expect (org-canvas--submissions-late-status-change-at-point "Adams") :to-be nil))))

(describe "org-canvas--submissions-late-status-change-at-point"
  (it "is a change when the typed status differs from its baseline, in any case"
    (with-grading-file (test-late--heading ":LATE_STATUS:  Extended \n:CANVAS_LATE_STATUS: late\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (org-canvas--submissions-late-status-change-at-point "Adams")
              :to-equal '(:late-status "extended" :old-late-status "late"
                          :submission-id "50001"))))
  (it "is a change on a row with no status yet"
    (with-grading-file (test-late--heading ":LATE_STATUS: missing\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (plist-get (org-canvas--submissions-late-status-change-at-point "Adams")
                         :old-late-status)
              :to-be nil)))
  (it "is no change when it matches, is blank, or is absent"
    (dolist (props '(":LATE_STATUS: late\n:CANVAS_LATE_STATUS: late\n"
                     ":LATE_STATUS:\n:CANVAS_LATE_STATUS: late\n"
                     ":CANVAS_LATE_STATUS: late\n"
                     ""))
      (with-grading-file (test-late--heading props)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-canvas--submissions-late-status-change-at-point "Adams") :to-be nil))))
  (it "refuses a value Canvas does not take, naming the ones it does"
    (with-grading-file (test-late--heading ":LATE_STATUS: tardy\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (org-canvas--submissions-late-status-change-at-point "Adams")
              :to-throw 'user-error
              '("Adams: LATE_STATUS tardy is not one of late, missing, extended, none"))))
  (it "refuses a heading with no submission id to address"
    (with-grading-file (concat test-grading-file-header
                               "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:LATE_STATUS: late\n:END:\n")
      (org-canvas--submissions-goto-user 5001)
      (expect (org-canvas--submissions-late-status-change-at-point "Adams")
              :to-throw 'user-error))))

(describe "pushing a late status with S"
  (it "sends a status-only change through the mutation and nothing over REST"
    (with-test-api
      (with-grading-file (test-late--heading ":SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"
                                             ":DAYS_LATE: 3\n:SCORE: 92\n:CANVAS_SCORE: 92\n"
                                             ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
        (test-late--push (test-late--stored "extended" 0))
        (expect (test-org-canvas-api-call-count) :to-equal 0)
        (expect (length test-late--mutations) :to-equal 1)
        (expect (caar test-late--mutations) :to-be org-canvas--submissions-late-status-mutation)
        (expect (cdar test-late--mutations)
                :to-equal '((submissionId . "50001") (status . "extended")))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "extended")
        (expect (org-entry-get (point) "DAYS_LATE") :to-be nil)
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)
        (expect (buffer-modified-p) :to-be nil))))
  (it "sends a score and a status together, the grade over REST"
    (with-test-api
      (with-grading-file (test-late--heading ":SUBMITTED_AT: <2026-02-15 Sun 23:45>\n"
                                             ":SCORE: 80\n:CANVAS_SCORE: 92\n:LATE_STATUS: late\n")
        (let ((prompt nil) (messages nil))
          (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                    ((symbol-function 'org-canvas--graphql-mutate)
                     (lambda (&rest _) (test-late--stored "late" 90000))))
            (let ((org-canvas-submissions-check-conflicts nil))
              (org-canvas-submissions-push-grades)))
          (expect prompt :to-match "1 grade change(s) (1 setting a late status)")
          (expect (cl-some (lambda (m) (string-match-p "92 → 80 (late status: no status → late)" m))
                           messages)
                  :to-be-truthy)
          (expect (car messages) :to-match "Pushed 1 grade(s), 1 late status(es) and 0 comment(s)"))
        (expect-api-called 'PUT "assignments/1001/submissions/5001")
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "80")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "late")
        (expect (org-entry-get (point) "DAYS_LATE") :to-equal "2"))))
  (it "records what Canvas stored when it is not what was sent, and says so"
    (with-test-api
      (with-grading-file (test-late--heading ":LATE_STATUS: none\n:CANVAS_LATE_STATUS: missing\n")
        (test-late--push (test-late--stored nil))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "LATE_STATUS") :to-be nil)
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-be nil)
        (expect test-late--warnings
                :to-contain "[Submissions] Adams, Alice: Canvas stored late status no status, not the none sent"))))
  (it "takes the status sent as stored when the reply names no submission"
    (with-test-api
      (with-grading-file (test-late--heading ":DAYS_LATE: 3\n:LATE_STATUS: missing\n")
        (test-late--push '((updateSubmissionGradeStatus . ((submission . :null) (errors . :null)))))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
        (expect (org-entry-get (point) "DAYS_LATE") :to-equal "3")
        (expect test-late--warnings :to-be nil))))
  (it "leaves a refused status a change, warns once, and sends the others"
    (with-test-api
      (with-grading-file (concat (test-late--heading ":LATE_STATUS: late\n")
                                 "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SUBMISSION_ID: 50002\n"
                                 ":LATE_STATUS: missing\n:END:\n")
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
            (test-late--push
             (lambda (variables)
               (if (equal (alist-get 'submissionId variables) "50001")
                   '((updateSubmissionGradeStatus
                      . ((submission . :null)
                         (errors . [((attribute . "late_policy_status")
                                     (message . "is not allowed"))]))))
                 (test-late--stored "missing")))))
          (expect (car messages)
                  :to-match "Pushed 0 grade(s), 1 late status(es) and 0 comment(s); late status not set for Adams, Alice (see the log)"))
        (expect (length test-late--warnings) :to-equal 1)
        (expect (car test-late--warnings) :to-match "Adams, Alice not set: .*is not allowed")
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-be nil)
        (org-canvas--submissions-goto-user 5002)
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
        (let ((left (org-canvas--submissions-collect-grade-changes)))
          (expect (length left) :to-equal 1)
          (expect (plist-get (car left) :user-id) :to-equal 5001)))))
  (it "records nothing under a dry run"
    (with-test-api
      (with-grading-file (test-late--heading ":LATE_STATUS: late\n")
        (test-late--push org-canvas--dry-run-response)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-be nil)
        (expect (length (org-canvas--submissions-collect-grade-changes)) :to-equal 1))))
  (it "sends nothing for a value Canvas does not take"
    (with-test-api
      (with-grading-file (test-late--heading ":LATE_STATUS: tardy\n")
        (expect (test-late--push (test-late--stored "late")) :to-throw 'user-error)
        (expect test-late--mutations :to-be nil)
        (expect (test-org-canvas-api-call-count) :to-equal 0))))
  (it "skips and marks a row whose status Canvas changed since the pull"
    (with-test-api
      (with-grading-file (test-late--heading ":SCORE: 92\n:CANVAS_SCORE: 92\n"
                                             ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
        (test-late--push (test-late--stored "extended") '("92" nil nil "missing"))
        (expect test-late--mutations :to-be nil)
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "CONFLICT") :to-equal "late status: Canvas has missing")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "late"))))
  (it "names a status Canvas cleared as no status"
    (expect (org-canvas--submissions-conflict-p
             '(:old-score "92" :late-status "missing" :old-late-status "late")
             '("92" nil nil nil))
            :to-equal "late status: Canvas has no status")
    (expect (org-canvas--submissions-conflict-p
             '(:old-score "92" :new-score "95")
             '("92" nil nil "late"))
            :to-be nil))
  (it "reads the live status alongside the score"
    (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
               (lambda (_id) (list (test-org-canvas-make-submission
                                    '((late_policy_status . "late")))))))
      (expect (nth 3 (alist-get 5001 (org-canvas--submissions-live-baselines "1001")))
              :to-equal "late"))))

(describe "a re-pull keeps a typed late status"
  (it "keeps it against an unmoved baseline, and asks nothing"
    (with-org-canvas-test-config
      (with-grading-file (test-late--heading ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
        (test-refresh--from-canvas '((late_policy_status . "late")))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "extended")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "late")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil))))
  (it "marks the heading when Canvas set another status since"
    (with-org-canvas-test-config
      (with-grading-file (test-late--heading ":LATE_STATUS: extended\n")
        (test-refresh--from-canvas '((late_policy_status . "missing")))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "extended")
        (expect (org-entry-get (point) "CANVAS_LATE_STATUS") :to-equal "missing")
        (expect (org-entry-get (point) "CONFLICT") :to-equal "late status: Canvas has missing"))))
  (it "writes nothing when Canvas now holds the typed status"
    (with-org-canvas-test-config
      (with-grading-file (test-late--heading ":LATE_STATUS: extended\n:CANVAS_LATE_STATUS: late\n")
        (test-refresh--from-canvas '((late_policy_status . "extended")))
        (org-canvas--submissions-goto-user 5001)
        (expect (org-entry-get (point) "LATE_STATUS") :to-equal "extended")
        (expect (org-entry-get (point) "CONFLICT") :to-be nil)
        (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)))))

(describe "org-canvas--submissions-record-days-late"
  (it "rewrites DAYS_LATE on a submitted row only"
    (with-grading-file (test-late--heading ":SUBMITTED_AT: <2026-02-15 Sun 23:45>\n:DAYS_LATE: 1\n")
      (org-canvas--submissions-goto-user 5001)
      (org-canvas--submissions-record-days-late 200000)
      (expect (org-entry-get (point) "DAYS_LATE") :to-equal "3")
      (org-canvas--submissions-record-days-late nil)
      (expect (org-entry-get (point) "DAYS_LATE") :to-equal "3"))
    (with-grading-file (test-late--heading ":DAYS_LATE: 1\n")
      (org-canvas--submissions-goto-user 5001)
      (org-canvas--submissions-record-days-late 0)
      (expect (org-entry-get (point) "DAYS_LATE") :to-equal "1"))))

;;;; Attempt history (issue #352)

(defconst test-progress--two-students
  (concat test-grading-file-header
          "#+PROPERTY: POST_POLICY manual\n"
          "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 95\n:CANVAS_SCORE: 92\n:END:\n\n"
          "* Beta, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:SCORE: 80\n:CANVAS_SCORE: 75\n:END:\n")
  "A grading file under a manual post policy with two score changes.")

(defun test-progress--scores ()
  "Return the CANVAS_SCORE of students 5001 and 5002 in this buffer."
  (mapcar (lambda (uid)
            (org-canvas--submissions-goto-user uid)
            (org-entry-get (point) "CANVAS_SCORE"))
          '(5001 5002)))

(defun test-progress--push (replies)
  "Push `test-progress--two-students' with the API answering REPLIES.
Return a plist: :calls, :waits, :asked (the post offer was made),
:said (the closing message) and :scores (each CANVAS_SCORE after)."
  (let (result)
    (with-org-canvas-test-config
      (with-grading-file test-progress--two-students
        (let ((org-canvas-submissions-check-conflicts nil)
              (org-canvas-submissions-progress-timeout 10)
              (org-canvas-submissions-progress-interval 2)
              (noninteractive nil)
              (asked nil) (said nil))
          (test-progress--with-replies replies
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt)
                         (if (string-match-p "post them" prompt) (progn (setq asked t) nil) t)))
                      ((symbol-function 'org-canvas--user-message)
                       (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
              (org-canvas-submissions-push-grades))
            (setq result (list :calls (reverse calls) :waits waits :asked asked
                               :said said :scores (test-progress--scores)))))))
    result))

(describe "org-canvas--submissions-await-progress (issue #382)"
  (it "reads the job at the configured interval until it completes"
    (with-org-canvas-test-config
      (let ((org-canvas-submissions-progress-interval 3)
            (org-canvas-submissions-progress-timeout 60))
        (test-progress--with-replies
            (list '((id . 9) (workflow_state . "running"))
                  '((id . 9) (workflow_state . "completed")))
          (let ((outcome (org-canvas--submissions-await-progress
                          '((id . 9) (workflow_state . "queued")
                            (url . "https://elsewhere.example/api/v1/progress/9"))
                          "2 grade(s)")))
            (expect (plist-get outcome :state) :to-be 'completed)
            (expect waits :to-equal '(3 3))
            (expect (length calls) :to-equal 2)
            (expect (car (car calls)) :to-be 'GET)
            ;; The address is built on the configured instance, never
            ;; taken from the reply's url.
            (expect (cadr (car calls)) :to-equal
                    (org-canvas--submissions-progress-url 9))
            (expect (cadr (car calls)) :not :to-match "elsewhere"))))))

  (it "asks nothing more of a job that has already finished"
    (test-progress--with-replies nil
      (let ((outcome (org-canvas--submissions-await-progress
                      '((id . 9) (workflow_state . "completed")) "x")))
        (expect (plist-get outcome :state) :to-be 'completed)
        (expect calls :to-be nil)
        (expect waits :to-be nil))))

  (it "reports a failed job with Canvas's message, or says Canvas gave none"
    (with-org-canvas-test-config
      (test-progress--with-replies
          (list '((id . 9) (workflow_state . "failed") (message . "grade too high")))
        (let ((outcome (org-canvas--submissions-await-progress
                        '((id . 9) (workflow_state . "queued")) "x")))
          (expect (plist-get outcome :state) :to-be 'failed)
          (expect (plist-get outcome :message) :to-equal "grade too high")))
      (test-progress--with-replies nil
        (let ((outcome (org-canvas--submissions-await-progress
                        '((id . 9) (workflow_state . "failed") (message)) "x")))
          (expect (plist-get outcome :message) :to-equal "Canvas gave no reason")))))

  (it "gives up after the timeout and calls the grades unconfirmed"
    (with-org-canvas-test-config
      (let ((org-canvas-submissions-progress-interval 2)
            (org-canvas-submissions-progress-timeout 5))
        (test-progress--with-replies
            (make-list 10 '((id . 9) (workflow_state . "running")))
          (let ((outcome (org-canvas--submissions-await-progress
                          '((id . 9) (workflow_state . "queued")) "x")))
            (expect (plist-get outcome :state) :to-be 'unconfirmed)
            (expect (plist-get outcome :message) :to-equal "still running after 6s")
            (expect (length waits) :to-equal 3))))))

  (it "waits a second at a time when the interval is not a positive number"
    (with-org-canvas-test-config
      (let ((org-canvas-submissions-progress-interval 0)
            (org-canvas-submissions-progress-timeout 2))
        (test-progress--with-replies
            (make-list 5 '((id . 9) (workflow_state . "running")))
          (org-canvas--submissions-await-progress '((id . 9) (workflow_state . "queued")) "x")
          (expect waits :to-equal '(1 1))))))

  (it "calls the grades unconfirmed when the progress cannot be read"
    (with-org-canvas-test-config
      (test-progress--with-replies (list 'error)
        (let ((outcome (org-canvas--submissions-await-progress
                        '((id . 9) (workflow_state . "running")) "x")))
          (expect (plist-get outcome :state) :to-be 'unconfirmed)
          (expect (plist-get outcome :message) :to-equal "the progress could not be read")))))

  (it "calls the grades unconfirmed when Canvas answered no Progress at all"
    (test-progress--with-replies nil
      (let ((outcome (org-canvas--submissions-await-progress '((name . "Mock")) "x")))
        (expect (plist-get outcome :state) :to-be 'unconfirmed)
        (expect (plist-get outcome :message) :to-match "no progress")
        (expect calls :to-be nil)))))

(describe "org-canvas--submissions-progress-url"
  (it "addresses the progress under the configured instance, trailing slash trimmed"
    (let ((org-canvas-base-url "https://canvas.example.edu/"))
      (expect (org-canvas--submissions-progress-url 42)
              :to-equal "https://canvas.example.edu/api/v1/progress/42"))))

(describe "a bulk grade push waits for Canvas's job (issue #382)"
  (it "records the baselines and offers to post once the job completes"
    (let ((r (test-progress--push
              (list '((id . 9) (workflow_state . "queued"))
                    '((id . 9) (workflow_state . "completed"))))))
      (expect (mapcar #'car (plist-get r :calls)) :to-equal '(POST GET))
      (expect (plist-get r :scores) :to-equal '("95" "80"))
      (expect (plist-get r :asked) :to-be t)
      (expect (plist-get r :said) :to-match "^Pushed 2 grade(s)")))

  (it "records nothing and offers no posting when the job fails"
    (let ((r (test-progress--push
              (list '((id . 9) (workflow_state . "queued"))
                    '((id . 9) (workflow_state . "failed") (message . "grade too high"))))))
      (expect (plist-get r :scores) :to-equal '("92" "75"))
      (expect (plist-get r :asked) :to-be nil)
      (expect (plist-get r :said)
              :to-match "Canvas did not apply 2 grade(s) (grade too high); nothing recorded")))

  (it "records nothing and offers no posting when the wait runs out"
    (let ((r (test-progress--push
              (cons '((id . 9) (workflow_state . "queued"))
                    (make-list 10 '((id . 9) (workflow_state . "running")))))))
      (expect (plist-get r :scores) :to-equal '("92" "75"))
      (expect (plist-get r :asked) :to-be nil)
      (expect (length (plist-get r :waits)) :to-equal 5)
      (expect (plist-get r :said)
              :to-match "2 grade(s) sent but not confirmed (still running after 10s)")))

  (it "sends no grade under a dry run and says the bulk push would be a background job"
    (let ((logged nil) (said nil) (scores nil) (sent nil))
      (with-org-canvas-test-config
        (with-grading-file test-progress--two-students
          (let ((org-canvas-submissions-check-conflicts nil)
                (org-canvas--dry-run t))
            (test-progress--with-replies nil
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                        ((symbol-function 'org-canvas--log-info)
                         (lambda (_logger fmt &rest args)
                           (push (apply #'format fmt args) logged)))
                        ((symbol-function 'org-canvas--user-message)
                         (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
                (org-canvas-submissions-push-grades))
              (setq sent calls
                    scores (test-progress--scores))))))
      (expect sent :to-be nil)
      (expect scores :to-equal '("92" "75"))
      (expect (car logged) :to-match
              "\\[DRY-RUN\\] Would send 2 grade(s) for assignment 1001 through update_grades, a Canvas background job")
      (expect said :to-match "^Dry run: would push 2 grade(s)")))

  (it "says a comment-only dry run would push, never that it pushed (issue #529)"
    (let ((org-canvas--dry-run t))
      (expect (org-canvas--submissions-grades-note 0 nil)
              :to-equal "Dry run: would push 0 grade(s)"))
    (let ((org-canvas--dry-run nil))
      (expect (org-canvas--submissions-grades-note 0 nil)
              :to-equal "Pushed 0 grade(s)")))

  (it "says nothing of a background job for a single dry-run grade"
    (let ((logged nil) (state nil))
      (with-org-canvas-test-config
        (let ((org-canvas--dry-run t))
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args) (push (apply #'format fmt args) logged))))
            (setq state (plist-get (org-canvas--submissions-send-grades
                                    "1001" (list (list :user-id 1 :old-score "1" :new-score "2")))
                                   :state)))))
      (expect state :to-be 'dry-run)
      (expect (car logged) :not :to-match "background")))

  (it "still records a late status Canvas stored when the grades did not land"
    (with-grading-file test-progress--two-students
      (let ((recorded nil))
        (cl-letf (((symbol-function 'org-canvas--submissions-record-late-status)
                   (lambda (change stored)
                     (push (list (plist-get change :user-id) stored) recorded))))
          (org-canvas--submissions-record-late-only
           (list (list :user-id 5001) (list :user-id 5002) (list :user-id 5003))
           '((5001 . (:status "late")) (5002 . dry-run) (5003 . (:status "none")))))
        ;; 5002 was a dry run and 5003 has no heading.
        (expect recorded :to-equal '((5001 (:status "late"))))))))

;;;; Pushing From a Script (issue #381)

(defun test-batch-push--run (assignment post &optional replies)
  "Push ASSIGNMENT from `test-progress--two-students' as a script would.
POST is passed through; REPLIES answer the API in turn (a completed
Progress by default).  No prompt may be asked.  Return a plist:
:result, :calls, :mutations and :saved (the file's text afterwards)."
  (let (out)
    (with-org-canvas-test-config
      (test-batch-push--with-dir test-progress--two-students
        (let ((org-canvas-submissions-check-conflicts nil)
              (noninteractive t)
              (mutations nil))
          (test-progress--with-replies
              (or replies (list '((id . 9) (workflow_state . "completed"))))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _) (error "Prompted")))
                      ((symbol-function 'org-canvas--confirm)
                       (lambda (&rest _) (error "Confirmed")))
                      ((symbol-function 'org-canvas--graphql-mutate)
                       (lambda (_what doc vars) (push (list doc vars) mutations) nil))
                      ((symbol-function 'org-canvas--user-message) #'ignore))
              (let ((result (org-canvas-push-submission-grades assignment post)))
                (setq out (list :result result :calls (reverse calls)
                                :mutations mutations
                                :saved (with-temp-buffer
                                         (insert-file-contents file)
                                         (buffer-string))))))))))
    out))

(describe "org-canvas-push-submission-grades (issue #381)"
  (it "pushes a grading file named by id, with no prompt, mode or view set by hand"
    (let* ((r (test-batch-push--run "1001" nil))
           (result (plist-get r :result)))
      (expect (mapcar #'car (plist-get r :calls)) :to-equal '(POST))
      (expect (cadr (car (plist-get r :calls))) :to-match "assignments/1001/submissions/update_grades")
      (expect (plist-get result :pushed) :to-equal 2)
      (expect (plist-get result :state) :to-be 'completed)
      (expect (plist-get result :posted) :to-be nil)
      (expect (plist-get result :conflicts) :to-equal 0)
      (expect (plist-get r :mutations) :to-be nil)
      ;; The baselines were recorded and the file saved.
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 95")
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 80")))

  (it "takes an integer id and the file's name as well"
    (expect (plist-get (plist-get (test-batch-push--run 1001 nil) :result) :pushed)
            :to-equal 2)
    (expect (plist-get (plist-get (test-batch-push--run "HW" nil) :result) :pushed)
            :to-equal 2))

  (it "posts only when asked, and only once Canvas stored the grades"
    (let* ((r (test-batch-push--run "1001" t))
           (mutation (car (plist-get r :mutations))))
      (expect (plist-get (plist-get r :result) :posted) :to-be t)
      (expect (car mutation) :to-be org-canvas--submissions-post-grades-mutation)
      (expect (alist-get 'assignmentId (cadr mutation)) :to-equal "1001")))

  (it "does not post grades whose job failed, and records none of them"
    (let* ((r (test-batch-push--run
               "1001" t
               (list '((id . 9) (workflow_state . "failed") (message . "no")))))
           (result (plist-get r :result)))
      (expect (plist-get result :pushed) :to-equal 0)
      (expect (plist-get result :state) :to-be 'failed)
      (expect (plist-get result :message) :to-equal "no")
      (expect (plist-get result :posted) :to-be nil)
      (expect (plist-get r :mutations) :to-be nil)
      (expect (plist-get r :saved) :to-match ":CANVAS_SCORE: 92")))

  (it "posts a column with nothing left to push when asked to"
    (with-org-canvas-test-config
      (test-batch-push--with-dir (concat test-grading-file-header
                                         "* A\n:PROPERTIES:\n:USER_ID: 1\n:SCORE: 5\n:CANVAS_SCORE: 5\n:END:\n")
        (let ((posted nil) (noninteractive t))
          (cl-letf (((symbol-function 'org-canvas--submissions-post-assignment)
                     (lambda (id) (setq posted id) t)))
            (let ((result (org-canvas-push-submission-grades "1001" t)))
              (expect (plist-get result :pushed) :to-equal 0)
              (expect (plist-get result :state) :to-be nil)
              (expect (plist-get result :posted) :to-be t)
              (expect posted :to-equal "1001")))))))

  (it "pushes a list of columns and returns one plist per column (issue #450)"
    (with-org-canvas-test-config
      (test-batch-push--with-dir (concat test-grading-file-header
                                         "* A\n:PROPERTIES:\n:USER_ID: 1\n:SCORE: 5\n:CANVAS_SCORE: 5\n:END:\n")
        (cl-letf (((symbol-function 'message) #'ignore))
          (let ((results (org-canvas-push-submission-grades '("1001" "HW"))))
            (expect (length results) :to-equal 2)
            (dolist (result results)
              (expect (plist-get result :pushed) :to-equal 0)
              (expect (plist-get result :posted) :to-be nil)))
          (expect (org-canvas-push-submission-grades '("1001") t '(1))
                  :to-throw 'user-error)))))

  (it "refuses an id no grading file names"
    (with-org-canvas-test-config
      (test-batch-push--with-dir test-progress--two-students
        (expect (org-canvas-push-submission-grades 9999) :to-throw 'user-error)
        (expect (org-canvas-push-submission-grades "9999") :to-throw 'user-error))))

  (it "refuses an id when there is no submissions directory"
    (let ((org-canvas-submissions-directory
           (expand-file-name "no-such-dir" temporary-file-directory)))
      (expect (org-canvas--submissions-files-for-id 1001) :to-be nil)
      (expect (org-canvas--submissions-push-target 1001) :to-throw 'user-error))))

(describe "org-canvas--submissions-post-assignment"
  (it "posts, or under a dry run sends nothing and answers nil"
    (let ((org-canvas--dry-run nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-mutate) (lambda (&rest _) nil))
                ((symbol-function 'message) #'ignore))
        (expect (org-canvas--submissions-post-assignment "1001") :to-be t)))
    (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
               (lambda (&rest _) org-canvas--dry-run-response)))
      (expect (org-canvas--submissions-post-assignment "1001") :to-be nil))))

(describe "org-canvas--submissions-ensure-context (issue #381)"
  (it "reads a saved grading file as the detail view before it holds a student"
    (with-grading-file test-grading-file-header
      (setq-local org-canvas-submissions--current-view nil)
      (org-canvas--submissions-ensure-context)
      (expect org-canvas-submissions--current-view :to-be 'detail))))

(describe "org-canvas-submissions-push-grades (issue #381)"
  (it "lets a user-error through rather than calling it a push failure"
    (with-grading-file test-progress--two-students
      (cl-letf (((symbol-function 'org-canvas--submissions-push-current)
                 (lambda (_) (user-error "SCORE disagrees with the rubric"))))
        (expect (org-canvas-submissions-push-grades) :to-throw 'user-error))))

  (it "asks nothing more when the push is declined"
    (with-grading-file test-progress--two-students
      (let ((offered nil) (org-canvas-submissions-check-conflicts nil))
        (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil))
                  ((symbol-function 'org-canvas--submissions-offer-to-post)
                   (lambda (_) (setq offered t))))
          (org-canvas-submissions-push-grades))
        (expect offered :to-be nil)))))

;;;; Sent Comments (issue #419)

(defun test-sent-only--push (&optional assignment)
  "Push only the sent comments of ASSIGNMENT, confirming; return (RESULT . PROMPT)."
  (let ((prompt nil))
    (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (p) (setq prompt p) t)))
      (cons (org-canvas-push-submission-comment-edits assignment) prompt))))

(describe "pushing only the sent comments (issue #425)"
  (it "sends the comment edit and nothing else the file holds"
    (with-test-api
      (with-grading-file (test-sent-only--everything-file)
        (org-canvas--submissions-ensure-context)
        (expect (org-canvas--submissions-collect-grade-changes) :not :to-be nil)
        (expect (org-canvas--submissions-collect-comment-drafts) :not :to-be nil)
        (expect (org-canvas--submissions-bank-pending) :not :to-be nil)
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
          (let* ((mutations nil)
                 (pushed (cl-letf (((symbol-function 'org-canvas--graphql-mutate)
                                    (lambda (&rest args) (push args mutations) nil)))
                           (test-sent-only--push))))
            (expect (cdr pushed) :to-equal "Push 1 comment edit(s)? ")
            (expect (car pushed) :to-equal
                    '(:edited 1 :deleted 0 :refused 0 :failed 0 :dry-run 0 :errored 0))
            (expect mutations :to-be nil))
          (expect (mapcar (lambda (c) (list (car c) (cadr c))) test-org-canvas-api-calls)
                  :to-equal
                  (list (list 'PUT (org-canvas-api-course-endpoint
                                    "assignments/%s/submissions/%s/comments/%s" 1001 5001 "22"))))
          (org-canvas--submissions-goto-user 5001)
          (expect (org-entry-get (point) "CANVAS_SCORE") :to-equal "3")
          (expect (org-entry-get (point) "SCORE") :to-equal "4")
          (expect (buffer-string) :to-match "^\\*\\* Comment to post\nSee me\\.$")
          (expect (buffer-string) :to-match "^- Show your units\\.$")
          (expect (org-canvas--submissions-collect-comment-edits) :to-be nil)
          (expect (org-canvas--submissions-collect-comment-drafts) :not :to-be nil)))))

  (it "refuses what the full push refuses and counts it"
    (with-test-api
      (with-grading-file (test-sent--file '(("21" . "Theirs.") ("22" . "Mine."))
                                          (concat "- *TA* [21] :: Theirs, rewritten.\n"
                                                  "- DELETE *Prof* [22] :: Mine.\n"))
        (test-sent--with-canvas (list (test-sent--comment 21 90 "Theirs.")
                                      (test-sent--comment 22 77 "Mine."))
          (let* ((pushed nil)
                 (warnings (test-sent--collecting-warnings
                            (setq pushed (test-sent-only--push)))))
            (expect warnings :to-equal
                    '("[Submissions] Comment 21 on Adams, Alice not changed: not yours; only its author may change it"))
            (expect (cdr pushed) :to-equal
                    "Push 1 comment deletion(s), leaving 1 comment change(s) unsent? ")
            (expect (car pushed) :to-equal
                    '(:edited 0 :deleted 1 :refused 1 :failed 0 :dry-run 0 :errored 0)))
          (expect (test-org-canvas-api-call-count) :to-equal 1)
          (expect (buffer-string) :to-match "\\[21\\] :: Theirs, rewritten\\.")))))

  (it "sends nothing and changes nothing under a dry run, even on a read-only course"
    (with-test-api
      (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                          "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine.")
                                      (test-sent--comment 23 77 "Drop."))
          (let ((org-canvas--dry-run t)
                (org-canvas-read-only t)
                (before (buffer-string)))
            (expect (car (test-sent-only--push)) :to-equal
                    '(:edited 0 :deleted 0 :refused 0 :failed 0 :dry-run 2 :errored 0))
            (expect (test-org-canvas-api-call-count) :to-equal 0)
            (expect (buffer-string) :to-equal before)
            (expect (length (org-canvas--submissions-collect-comment-edits)) :to-equal 2))))))

  (it "refuses a read-only course before anything is sent"
    (with-test-api
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
          (let ((org-canvas-read-only t))
            (expect (test-sent-only--push) :to-throw 'org-canvas-read-only-error))
          (expect (test-org-canvas-api-call-count) :to-equal 0)))))

  (it "sends nothing when the question is declined"
    (with-test-api
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
          (let ((noninteractive nil)
                (org-canvas-assume-yes nil)
                (asked nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq asked p) nil)))
              (expect (org-canvas-push-submission-comment-edits) :to-be nil))
            (expect asked :to-equal "Push 1 comment edit(s)? "))
          (expect (test-org-canvas-api-call-count) :to-equal 0)))))

  (it "says when there is nothing to send"
    (with-test-api
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine.\n")
        (let ((shown nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) shown)))
                    ((symbol-function 'org-canvas--confirm) (lambda (_) (error "must not ask"))))
            (expect (org-canvas-push-submission-comment-edits) :to-equal
                    '(:edited 0 :deleted 0 :refused 0 :failed 0 :dry-run 0 :errored 0)))
          (expect (car shown) :to-equal "No sent comment to change")
          (expect (test-org-canvas-api-call-count) :to-equal 0)))))

  (it "saves the new baseline through the package's save path"
    (with-test-api
      (setq test-org-canvas-api-responses
            `(("comments/22" . ,(test-sent--comment 22 77 "Mine, rewritten."))))
      (with-grading-file (test-sent--file '(("22" . "Mine.")) "- *Prof* [22] :: Mine, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine."))
          (let ((saves 0)
                (save (symbol-function 'org-canvas--save-buffer)))
            (cl-letf (((symbol-function 'org-canvas--save-buffer)
                       (lambda () (cl-incf saves) (funcall save))))
              (test-sent-only--push))
            (expect saves :to-equal 1))
          (expect (buffer-modified-p) :to-be nil)
          (expect (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))
                  :to-match (format ":CANVAS_COMMENTS: 22=%s"
                                    (org-canvas--submissions-comment-digest
                                     "Mine, rewritten.")))))))

  (it "pushes a grading file named from a script, without a prompt"
    (with-test-api
      (with-grading-file (test-sent--file '(("22" . "Mine.") ("23" . "Drop."))
                                          "- *Prof* [22] :: Mine, rewritten.\n- DELETE *Prof* [23] :: Drop.\n")
        (test-sent--with-canvas (list (test-sent--comment 22 77 "Mine.")
                                      (test-sent--comment 23 77 "Drop."))
          (let ((noninteractive t)
                (org-canvas-assume-yes nil)
                (grading (current-buffer)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "must not ask")))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) (error "must not prompt"))))
              (with-temp-buffer
                (expect (org-canvas-push-submission-comment-edits "HW") :to-equal
                        '(:edited 1 :deleted 1 :refused 0 :failed 0 :dry-run 0 :errored 0))))
            (expect (buffer-local-value 'org-canvas-submissions--current-view grading)
                    :to-equal 'detail)
            (expect (test-org-canvas-api-call-count) :to-equal 2)
            (expect (buffer-modified-p grading) :to-be nil))))))

  (it "finds a grading file by its assignment id"
    (with-test-api
      (with-grading-file (test-sent--file '(("23" . "Drop.")) "- DELETE *Prof* [23] :: Drop.\n")
        (test-sent--with-canvas (list (test-sent--comment 23 77 "Drop."))
          (with-temp-buffer
            (expect (plist-get (car (test-sent-only--push 1001)) :deleted) :to-equal 1))))))

  (it "reports the refused count from the full push as well"
    (with-test-api
      (with-grading-file (test-sent--file '(("21" . "Theirs.")) "- *TA* [21] :: Theirs, rewritten.\n")
        (test-sent--with-canvas (list (test-sent--comment 21 90 "Theirs."))
          (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
            (expect (plist-get (org-canvas-push-submission-grades 1001) :refused)
                    :to-equal 1))))))

  (it "refuses a buffer that names no assignment"
    (with-test-api
      (with-grading-file "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n"
        (expect (org-canvas-push-submission-comment-edits) :to-throw 'user-error)
        (expect (test-org-canvas-api-call-count) :to-equal 0))))

  (it "is bound to C in a grading file and in the dispatch menu"
    (expect (lookup-key org-canvas-submissions-mode-map (kbd "C"))
            :to-equal #'org-canvas-push-submission-comment-edits)
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-push-submission-comment-edits)
            :to-be-truthy)))

(describe "a push whose job was never confirmed (issue #496)"
  (it "leaves typed rows Canvas now holds unmarked on the next refresh"
    (let* ((dir (make-temp-file "org-canvas-subs-" t))
           (org-canvas-submissions-directory dir)
           (shown (list nil)))
      (unwind-protect
          (progn
            (test-rubric--display (list (test-org-canvas-make-submission '((score . nil)))) shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (org-canvas--submissions-rubric-set-row "_7104" "2" "Sharp")
              (org-canvas--submissions-rubric-set-row "_7106" "1" nil)
              (save-buffer))
            ;; The push stored the rows, but its job outlived the wait,
            ;; so CANVAS_RUBRIC was never advanced.
            (test-rubric--display
             (list (test-org-canvas-make-submission
                    '((score . 3)
                      (rubric_assessment . ((_7104 . ((points . 2) (comments . "Sharp")))
                                            (_7106 . ((points . 1))))))))
             shown)
            (with-current-buffer (car shown)
              (org-canvas--submissions-goto-user 5001)
              (expect (org-entry-get (point) "CONFLICT") :to-be nil)
              (expect (mapcar (lambda (r) (list (nth 3 r) (nth 4 r)))
                              (org-canvas--submissions-rubric-rows))
                      :to-equal '(("2" "Sharp") (nil nil) ("1" nil)))
              (expect (org-entry-get (point) "CANVAS_RUBRIC")
                      :to-equal (org-canvas--submissions-rubric-digest
                                 '(("_7104" "2" "Sharp") ("_7106" "1" nil))))
              (expect (org-canvas--submissions-collect-grade-changes) :to-be nil)))
        (when (buffer-live-p (car shown)) (kill-buffer (car shown)))
        (delete-directory dir t))))

  (it "sends again, unheld, a change whose values Canvas already holds"
    (let ((change (list :user-id 5001 :name "Adams, Alice" :old-score "5" :new-score "8"
                        :attempt 1 :triples '(("_1" "8" nil)) :old-rubric "aaa"
                        :new-rubric "bbb" :late-status "late" :old-late-status nil)))
      (expect (org-canvas--submissions-conflict-p change '("8" 1 "bbb" "late")) :to-be nil)
      (expect (org-canvas--submissions-conflict-p change '("8" nil "bbb" "late")) :to-be nil)
      (expect (org-canvas--submissions-conflict-p change '("7" 1 "bbb" "late"))
              :to-equal "score: Canvas has 7")
      (expect (org-canvas--submissions-conflict-p change '("8" 2 "bbb" "late"))
              :to-equal "score: Canvas has 8")
      (expect (org-canvas--submissions-conflict-p change '("8" 1 "ccc" "late"))
              :to-equal "score: Canvas has 8")
      (expect (org-canvas--submissions-conflict-p change '("8" 1 "bbb" nil))
              :to-equal "score: Canvas has 8")
      (expect (org-canvas--submissions-conflict-p change nil) :to-equal "score: Canvas has no grade")))

  (it "compares only what a change sends"
    (let ((score-only (list :old-score "5" :new-score "8")))
      (expect (org-canvas--submissions-live-holds-change-p score-only '("8" 3 "zzz" "missing"))
              :to-be-truthy)
      (expect (org-canvas--submissions-live-holds-change-p score-only '("5" 3 nil nil))
              :to-be nil))))

(provide 'org-canvas-submissions-push-test)
;;; org-canvas-submissions-push-test.el ends here
