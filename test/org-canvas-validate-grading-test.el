;;; org-canvas-validate-grading-test.el --- Buttercup tests for the validator's grading checks  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-validate-grading': grading periods
;; and overrides, scheduled posting, grading schemes, the
;; late window, and assignment weighting.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(describe "org-canvas--validate-assignment-structure"
  (it "returns nil with no override table"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:POINTS: 100
:END:

Some body text.
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-assignment-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Assignment 1"))
             :to-be nil)))

  (it "returns nil when override block exists but no table follows"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:POINTS: 100
:END:

#+NAME: overrides
Not a table row here.
"
     (org-back-to-heading t)
     (expect (org-canvas--validate-assignment-structure
              (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Assignment 1"))
             :to-be nil)))

  (it "returns warning for override row without file link"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:POINTS: 100
:END:

#+NAME: overrides
| Section | Due At | Unlock At | Lock At |
|---------+--------+-----------+---------|
| Plain   |        |           |         |
"
     (org-back-to-heading t)
     (let ((issues (org-canvas--validate-assignment-structure
                    (list :file (buffer-file-name) :line (line-number-at-pos) :heading "Assignment 1"))))
       (expect (length issues) :to-be-greater-than 0)
       (expect (plist-get (car issues) :message) :to-match "not a section link, Group: or Students:")))))

(defun test-validate--override-issues (table &optional props)
  "Return the assignment-structure issues of a heading holding TABLE.
PROPS is extra drawer text.  Each issue is reduced to a list of its
severity, line, property and message."
  (with-temp-org-buffer
      (format "* HW\n:PROPERTIES:\n%s:END:\n\n#+NAME: overrides\n%s"
              (or props "") table)
    (org-back-to-heading t)
    (mapcar (lambda (issue)
              (list (plist-get issue :severity) (plist-get issue :line)
                    (plist-get issue :property) (plist-get issue :message)))
            (org-canvas--validate-assignment-structure
             (list :file (buffer-file-name) :line 1 :heading "HW")))))

(describe "override row date order (issue #534)"
  (it "says nothing of rows whose dates run in order, each optional"
    (expect (test-validate--override-issues
             (concat "| Section | Due At | Unlock At | Lock At |\n"
                     "|---+---+---+---|\n"
                     "| Group: #1 | <2099-03-10 Tue 23:59> | <2099-03-01 Sun 00:00> | <2099-03-12 Thu 23:59> |\n"
                     "| Group: #2 | <2099-03-10 Tue 23:59> | | |\n"
                     "| Group: #3 | | <2099-03-01 Sun 00:00> | <2099-03-12 Thu 23:59> |\n"))
            :to-be nil))

  (it "warns on the row whose lock comes before its due date"
    (let* ((issues (test-validate--override-issues
                    (concat "| Section | Due At | Unlock At | Lock At |\n"
                            "|---+---+---+---|\n"
                            "| Group: #1 | <2099-03-10 Tue 23:59> | | <2099-03-12 Thu 23:59> |\n"
                            "| Group: #2 | <2099-03-10 Tue 23:59> | | <2099-03-09 Mon 23:59> |\n"
                            "| Group: #3 | <2099-03-10 Tue 23:59> | | |\n")))
           (issue (car issues)))
      (expect (length issues) :to-equal 1)
      (expect (nth 0 issue) :to-equal 'warning)
      (expect (nth 1 issue) :to-equal 9)
      (expect (nth 2 issue) :to-equal "Due At")
      (expect (nth 3 issue) :to-match
              "\\`Override row 'Group: #2': Due At is after Lock At")))

  (it "warns when the unlock comes after the due date"
    (let ((issues (test-validate--override-issues
                   (concat "| Section | Due At | Unlock At |\n"
                           "|---+---+---|\n"
                           "| [[file:sections.org::*Sec A][Sec A]] | <2099-03-10 Tue 23:59> | <2099-03-11 Wed 00:00> |\n"))))
      (expect (cl-some (lambda (i) (string-match-p
                                    "Override row 'Sec A': Unlock At is after Due At"
                                    (nth 3 i)))
                       issues)
              :to-be-truthy)))

  (it "compares unlock with lock when the row has no due date"
    (let ((issues (test-validate--override-issues
                   (concat "| Section | Due At | Unlock At | Lock At |\n"
                           "|---+---+---+---|\n"
                           "| Group: #1 | | <2099-03-15 Sun 00:00> | <2099-03-12 Thu 23:59> |\n"))))
      (expect (length issues) :to-equal 1)
      (expect (nth 3 (car issues)) :to-match "Unlock At is after Lock At")))

  (it "finds the date columns by their headers, as the push does"
    (let ((issues (test-validate--override-issues
                   (concat "| Section | Lock At | Due At |\n"
                           "|---+---+---|\n"
                           "| Group: #1 | <2099-03-12 Thu 23:59> | <2099-03-10 Tue 23:59> |\n"
                           "| Group: #2 | <2099-03-08 Sun 23:59> | <2099-03-10 Tue 23:59> |\n"))))
      (expect (length issues) :to-equal 1)
      (expect (nth 1 (car issues)) :to-equal 9)))

  (it "passes over a cell that is not a timestamp"
    (expect (test-validate--override-issues
             (concat "| Section | Due At | Unlock At | Lock At |\n"
                     "|---+---+---+---|\n"
                     "| Group: #1 | soon | <2099-03-15 Sun 00:00> | |\n"))
            :to-be nil)))

(defun test-validate--late-window-messages (props &optional table)
  "Return the messages of the late-window issues for a heading.
PROPS is its drawer text and TABLE an override table under it.
Other findings (date order, targets) are left out."
  (delq nil (mapcar (lambda (issue)
                      (let ((message (nth 3 issue)))
                        (and (string-match-p "Lock window\\|Unlock date" message)
                             message)))
                    (test-validate--override-issues (or table "") props))))

(describe "a course's late window (issue #536)"
  (it "is off by default"
    (expect (test-validate--late-window-messages
             ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n")
            :to-be nil))

  (it "passes an online heading locking at an allowed offset"
    (let ((org-canvas-validate-lock-after-due-hours 48))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n:LOCK_AT: <2099-03-12 Thu 23:59>\n")
              :to-be nil))
    (let ((org-canvas-validate-lock-after-due-hours '(0 48)))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: online_text_entry\n:DUE_AT: <2099-03-10 Tue 23:59>\n:LOCK_AT: <2099-03-10 Tue 23:59>\n")
              :to-be nil)))

  (it "counts hours by the clock across a daylight-saving change"
    (let ((org-canvas-validate-lock-after-due-hours 48))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-07 Sat 23:59>\n:LOCK_AT: <2099-03-09 Mon 23:59>\n")
              :to-be nil)))

  (it "warns when the lock is missing or at another offset"
    (let ((org-canvas-validate-lock-after-due-hours 48))
      (let ((missing (test-validate--late-window-messages
                      ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n"))
            (mismatch (test-validate--late-window-messages
                       ":SUBMISSION: online_url\n:DUE_AT: <2099-03-10 Tue 23:59>\n:LOCK_AT: <2099-03-11 Wed 23:59>\n")))
        (expect (length missing) :to-equal 1)
        (expect (car missing) :to-match
                "\\`Lock window missing: due <2099-03-10 Tue 23:59> with no lock date; the course locks online columns 48 hour(s) after")
        (expect (length mismatch) :to-equal 1)
        (expect (car mismatch) :to-match
                "\\`Lock window mismatch: locks 24 hour(s) after the due date .*the course allows 48 hour(s)"))))

  (it "leaves alone a column that is not online, or has no due date"
    (let ((org-canvas-validate-lock-after-due-hours 48)
          (org-canvas-validate-require-unlock t))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: on_paper\n:DUE_AT: <2099-03-10 Tue 23:59>\n")
              :to-be nil)
      (expect (test-validate--late-window-messages ":SUBMISSION: online_upload\n")
              :to-be nil)))

  (it "requires an unlock date when the course says so"
    (let ((org-canvas-validate-require-unlock t))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n")
              :to-equal '("Unlock date missing: the course requires one on every online column"))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: online_upload\n:UNLOCK_AT: <2099-03-01 Sun 00:00>\n:DUE_AT: <2099-03-10 Tue 23:59>\n")
              :to-be nil)))

  (it "marks the findings push-only once the column is past due, and not before"
    (let ((org-canvas-validate-lock-after-due-hours 48)
          (org-canvas-validate-require-unlock t))
      (let* ((past (car (org-canvas--validate-late-window
                         "<2020-03-10 Tue 23:59>" nil nil '(:file "/f" :line 1 :heading "HW"))))
             (future (car (org-canvas--validate-late-window
                           "<2099-03-10 Tue 23:59>" nil nil '(:file "/f" :line 1 :heading "HW"))))
             (past-severity (plist-get past :severity))
             (past-push-only (plist-get past :push-only))
             (future-push-only (plist-get future :push-only)))
        (expect past-severity :to-equal 'warning)
        (expect past-push-only :to-be t)
        (expect future :to-be-truthy)
        (expect future-push-only :to-be nil))))

  (it "asks a column for an unlock date only while it is not yet due"
    (let ((org-canvas-validate-require-unlock t))
      (expect (org-canvas--validate-late-window
               "<2020-03-10 Tue 23:59>" nil nil '(:file "/f" :line 1 :heading "HW"))
              :to-be nil)
      (expect (length (org-canvas--validate-late-window
                       "<2099-03-10 Tue 23:59>" nil nil '(:file "/f" :line 1 :heading "HW")))
              :to-equal 1)))

  (it "rounds the offset and says before when the lock comes first"
    (let ((org-canvas-validate-lock-after-due-hours 48))
      (let ((message (plist-get (car (org-canvas--validate-late-window
                                      "<2099-10-09 Fri 11:02>" "<2099-10-07 Wed 14:16>" nil
                                      '(:file "/f" :line 1 :heading "HW")))
                                :message)))
        (expect message :to-match "locks 44\\.8 hour(s) before the due date (due")
        (expect message :not :to-match "44\\.76"))))

  (it "checks each override row that sets a date, inheriting what it leaves empty"
    (let ((org-canvas-validate-lock-after-due-hours 48)
          (org-canvas-validate-require-unlock t))
      (let ((issues (test-validate--override-issues
                     (concat "| Section | Due At | Unlock At | Lock At |\n"
                             "|---+---+---+---|\n"
                             "| Group: #1 | <2099-03-17 Tue 23:59> | | <2099-03-19 Thu 23:59> |\n"
                             "| Group: #2 | <2099-03-17 Tue 23:59> | | |\n"
                             "| Group: #3 | | <2099-03-02 Mon 00:00> | |\n"
                             "| Group: #4 | <2099-03-17 Tue 23:59> | | <2099-03-17 Tue 23:59> |\n")
                     (concat ":SUBMISSION: online_upload\n:UNLOCK_AT: <2099-03-01 Sun 00:00>\n"
                             ":DUE_AT: <2099-03-10 Tue 23:59>\n:LOCK_AT: <2099-03-12 Thu 23:59>\n"))))
        (expect (mapcar (lambda (i) (list (nth 1 i) (nth 3 i))) issues)
                :to-equal
                ;; Group: #2's own due date also runs past the lock it
                ;; inherits, a date-order finding of its own (issue #538).
                '((13 "Override row 'Group: #2': Due At is after Lock At (<2099-03-17 Tue 23:59> > <2099-03-12 Thu 23:59> (the assignment's))")
                  (13 "Override row 'Group: #2': Lock window mismatch: locks 120 hour(s) before the due date (due <2099-03-17 Tue 23:59>, lock <2099-03-12 Thu 23:59>); the course allows 48 hour(s)")
                  (15 "Override row 'Group: #4': Lock window mismatch: locks 0 hour(s) after the due date (due <2099-03-17 Tue 23:59>, lock <2099-03-17 Tue 23:59>); the course allows 48 hour(s)"))))))

  (it "reads a row setting only a lock date against the assignment's due date"
    (let ((org-canvas-validate-lock-after-due-hours 48))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n:LOCK_AT: <2099-03-12 Thu 23:59>\n"
               (concat "| Section | Lock At |\n"
                       "|---+---|\n"
                       "| Group: #1 | <2099-03-12 Thu 23:59> |\n"
                       "| Group: #2 | <2099-03-13 Fri 23:59> |\n"
                       "| Group: #3 | |\n"))
              :to-equal
              '("Override row 'Group: #2': Lock window mismatch: locks 72 hour(s) after the due date (due <2099-03-10 Tue 23:59>, lock <2099-03-13 Fri 23:59>); the course allows 48 hour(s)"))))

  (it "leaves a lock date that does not parse to the timestamp check"
    (let ((org-canvas-validate-lock-after-due-hours 48))
      (expect (test-validate--late-window-messages
               ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n:LOCK_AT: Friday\n")
              :to-be nil)))

  (it "asks for an unlock date on a row only when neither it nor the heading has one"
    (let ((org-canvas-validate-require-unlock t))
      (let ((messages (test-validate--late-window-messages
                       (concat ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n"
                               ":UNLOCK_AT: <2099-03-01 Sun 00:00>\n")
                       (concat "| Section | Due At | Unlock At |\n"
                               "|---+---+---|\n"
                               "| Group: #1 | <2099-03-17 Tue 23:59> | |\n"
                               "| Group: #2 | <2099-03-17 Tue 23:59> | <2099-03-03 Tue 00:00> |\n"
                               "| Group: #3 | <2099-03-18 Wed 23:59> | |\n"))))
        (expect messages :to-be nil))))

  (it "warns on a row with neither an unlock nor a lock date to inherit"
    (let ((org-canvas-validate-require-unlock t)
          (org-canvas-validate-lock-after-due-hours 48))
      (let ((messages (test-validate--late-window-messages
                       ":SUBMISSION: online_upload\n:DUE_AT: <2099-03-10 Tue 23:59>\n"
                       (concat "| Section | Due At | Unlock At |\n"
                               "|---+---+---|\n"
                               "| Group: #1 | <2099-03-17 Tue 23:59> | |\n"
                               "| Group: #2 | <2099-03-17 Tue 23:59> | <2099-03-03 Tue 00:00> |\n"))))
        (expect messages :to-equal
                '("Lock window missing: due <2099-03-10 Tue 23:59> with no lock date; the course locks online columns 48 hour(s) after the due date"
                  "Unlock date missing: the course requires one on every online column"
                  "Override row 'Group: #1': Lock window missing: due <2099-03-17 Tue 23:59> with no lock date; the course locks online columns 48 hour(s) after the due date"
                  "Override row 'Group: #1': Unlock date missing: the course requires one on every online column"
                  "Override row 'Group: #2': Lock window missing: due <2099-03-17 Tue 23:59> with no lock date; the course locks online columns 48 hour(s) after the due date"))))))

(describe "org-canvas--validate-post-schedule (issue #352)"
  (cl-flet ((check (props)
              (with-temp-org-buffer (format "* HW\n:PROPERTIES:\n%s:END:\n" props)
                (org-back-to-heading t)
                (mapcar (lambda (issue)
                          (list (plist-get issue :severity)
                                (plist-get issue :property)
                                (plist-get issue :message)))
                        (org-canvas--validate-assignment-structure
                         (list :file (buffer-file-name) :line 1 :heading "HW"))))))
    (it "says nothing of a heading without a schedule, or with a sound one"
      (expect (check ":POINTS: 10\n") :to-be nil)
      (expect (check ":POST_GRADES_AT: [2026-12-01 Tue 09:00]\n:POST_COMMENTS_AT: [2026-12-01 Tue 09:00]\n:POST_POLICY: manual\n")
              :to-be nil))

    (it "calls one time without the other an error"
      (let ((issues (check ":POST_GRADES_AT: [2026-12-01 Tue 09:00]\n")))
        (expect (length issues) :to-equal 1)
        (expect (nth 0 (car issues)) :to-be 'error)
        (expect (nth 1 (car issues)) :to-equal "POST_GRADES_AT")
        (expect (nth 2 (car issues)) :to-match "without POST_COMMENTS_AT"))
      (let ((issues (check ":POST_COMMENTS_AT: [2026-12-01 Tue 09:00]\n")))
        (expect (nth 1 (car issues)) :to-equal "POST_COMMENTS_AT")
        (expect (nth 2 (car issues)) :to-match "POST_COMMENTS_AT without POST_GRADES_AT")))

    (it "calls grades posted before comments an error"
      (let ((issues (check ":POST_GRADES_AT: [2026-11-30 Mon 09:00]\n:POST_COMMENTS_AT: [2026-12-01 Tue 09:00]\n")))
        (expect (length issues) :to-equal 1)
        (expect (nth 2 (car issues)) :to-match "before POST_COMMENTS_AT")))

    (it "calls a schedule beside automatic posting an error"
      (let ((issues (check ":POST_GRADES_AT: [2026-12-01 Tue 09:00]\n:POST_COMMENTS_AT: [2026-11-30 Mon 09:00]\n:POST_POLICY: automatic\n")))
        (expect (length issues) :to-equal 1)
        (expect (nth 2 (car issues)) :to-match "needs manual posting")))

    (it "leaves a time that does not parse to the timestamp check"
      (expect (check ":POST_GRADES_AT: someday\n:POST_COMMENTS_AT: [2026-11-30 Mon 09:00]\n")
              :to-be nil))))

(describe "org-canvas--validate-external-tool"
  (it "warns when external_tool names no tool"
    (with-temp-org-buffer
     "* Problem Set
:PROPERTIES:
:SUBMISSION: external_tool
:END:
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-external-tool
                    (list :file "/f" :line 1 :heading "Problem Set"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :severity) :to-equal 'warning)
       (expect (plist-get (car issues) :message) :to-match "launches nothing"))))

  (it "accepts a launch URL"
    (with-temp-org-buffer
     "* Problem Set
:PROPERTIES:
:SUBMISSION: external_tool
:EXTERNAL_TOOL_URL: https://www.gradescope.com/auth/lti/callback
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-external-tool
              (list :file "/f" :line 1 :heading "Problem Set"))
             :to-be nil)))

  (it "accepts an installed tool id"
    (with-temp-org-buffer
     "* Problem Set
:PROPERTIES:
:SUBMISSION: external_tool
:EXTERNAL_TOOL_ID: 12787
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-external-tool
              (list :file "/f" :line 1 :heading "Problem Set"))
             :to-be nil)))

  (it "finds external_tool among several submission types"
    (with-temp-org-buffer
     "* Problem Set
:PROPERTIES:
:SUBMISSION: online_upload, external_tool
:END:
"
     (org-back-to-heading)
     (expect (length (org-canvas--validate-external-tool
                      (list :file "/f" :line 1 :heading "Problem Set")))
             :to-equal 1)))

  (it "warns when a tool is declared but never selected"
    (with-temp-org-buffer
     "* Problem Set
:PROPERTIES:
:SUBMISSION: online_upload
:EXTERNAL_TOOL_URL: https://www.gradescope.com/auth/lti/callback
:END:
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-external-tool
                    (list :file "/f" :line 1 :heading "Problem Set"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :message) :to-match "is ignored unless"))))

  (it "stays quiet on an ordinary assignment"
    (with-temp-org-buffer
     "* Essay
:PROPERTIES:
:SUBMISSION: online_upload
:END:
"
     (org-back-to-heading)
     (expect (org-canvas--validate-external-tool
              (list :file "/f" :line 1 :heading "Essay"))
             :to-be nil)))

  (it "reaches the report through the assignment structural check"
    (with-temp-org-buffer
     "* Problem Set
:PROPERTIES:
:SUBMISSION: external_tool
:END:
"
     (org-back-to-heading)
     (let ((issues (org-canvas--validate-assignment-structure
                    (list :file "/f" :line 1 :heading "Problem Set"))))
       (expect (length issues) :to-equal 1)
       (expect (plist-get (car issues) :property) :to-equal "EXTERNAL_TOOL_URL")))))

(describe "org-canvas--validate-override-section-ids"
  (it "reports warning when override section link target has no CANVAS_ID"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:END:\n"))
            (with-temp-org-buffer
             (format "| [[file:%s::*Section A][Section A]] | <2027-01-15 Fri 23:59> |\n" sections-file)
             (goto-char (point-min))
             (let ((issues (org-canvas--validate-override-section-ids
                            (buffer-file-name) "Assignment 1")))
               (expect issues :not :to-be nil)
               (expect (length issues) :to-equal 1)
               (expect (plist-get (car issues) :severity) :to-equal 'warning)
               (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID"))))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file))))

  (it "returns nil when section link target has CANVAS_ID"
    (let ((sections-file (make-temp-file "test-sections" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 99\n:END:\n"))
            (with-temp-org-buffer
             (format "| [[file:%s::*Section A][Section A]] | <2027-01-15 Fri 23:59> |\n" sections-file)
             (goto-char (point-min))
             (let ((issues (org-canvas--validate-override-section-ids
                            (buffer-file-name) "Assignment 1")))
               (expect issues :to-be nil))))
        (let ((buf (find-buffer-visiting sections-file)))
          (when buf (kill-buffer buf)))
        (delete-file sections-file)))))

(describe "org-canvas--validate-drop-rules"
  (it "warns when only DROP_LOWEST is set and exceeds assignment count"
    (let ((assignments-file (make-temp-file "test-assignments" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file assignments-file
              (insert "* HW 1\n:PROPERTIES:\n:GROUP: Homework\n:END:\n"))
            (let ((org-canvas-assignments-file assignments-file))
              (with-temp-org-buffer
               "* Homework
:PROPERTIES:
:DROP_LOWEST: 2
:END:
"
               (org-back-to-heading t)
               (let* ((loc (list :file (buffer-file-name)
                                 :line (line-number-at-pos)
                                 :heading "Homework"))
                      (issues (org-canvas--validate-drop-rules loc)))
                 (expect issues :not :to-be nil)
                 (expect (plist-get (car issues) :severity) :to-equal 'warning)
                 (expect (plist-get (car issues) :message) :to-match "Drop rules")))))
        (let ((buf (find-buffer-visiting assignments-file)))
          (when buf (kill-buffer buf)))
        (delete-file assignments-file)))))

(describe "org-canvas--count-assignments-in-group"
  (it "returns 0 (no crash) when the assignments file is nil"
    ;; Guards the `(and assignments-file (file-exists-p assignments-file))'
    ;; predicate: an `or' would call file-exists-p on nil and crash.
    (expect (org-canvas--count-assignments-in-group "Homework" nil)
            :to-equal 0))

  (it "counts assignments whose GROUP matches the group name"
    (let ((temp-dir (make-temp-file "count-test-" t)))
      (unwind-protect
          (let ((assign-file (expand-file-name "assignments.org" temp-dir)))
            (with-temp-file assign-file
              (insert "* A1\n:PROPERTIES:\n:GROUP: [[file:x::*Homework][Homework]]\n:END:\n"
                      "* A2\n:PROPERTIES:\n:GROUP: [[file:x::*Homework][Homework]]\n:END:\n"
                      "* A3\n:PROPERTIES:\n:GROUP: [[file:x::*Exams][Exams]]\n:END:\n"))
            (expect (org-canvas--count-assignments-in-group "Homework" assign-file)
                    :to-equal 2))
        (delete-directory temp-dir t))))

  (it "returns 0 when the file does not exist"
    (expect (org-canvas--count-assignments-in-group
             "Homework" "/nonexistent/assignments.org")
            :to-equal 0)))

(describe "org-canvas--validate-drop-rules"
  (it "returns nil when no drop rules"
    (with-temp-org-buffer
     "* Homework
:PROPERTIES:
:WEIGHT: 40
:END:
"
     (org-back-to-heading)
     (let ((loc (list :file (buffer-file-name) :line 1 :heading "Homework")))
       (expect (org-canvas--validate-drop-rules loc) :to-be nil))))

  (it "returns nil when a drop rule is present but zero"
    ;; Guards the `(> total-drops 0)' boundary: a >= would warn even when no
    ;; assignments are actually dropped (DROP_LOWEST: 0).  Uses an explicit 0
    ;; so the `(or drop-lowest drop-highest)' guard is passed and the boundary
    ;; line is actually reached.
    (let ((temp-dir (make-temp-file "drop-zero-" t)))
      (unwind-protect
          (let ((assign-file (expand-file-name "assignments.org" temp-dir)))
            (with-temp-file assign-file (insert ""))
            (with-temp-org-buffer
             "* Homework
:PROPERTIES:
:DROP_LOWEST: 0
:END:
"
             (org-back-to-heading)
             (let ((org-canvas-assignments-file assign-file)
                   (loc (list :file (buffer-file-name) :line 1 :heading "Homework")))
               (expect (org-canvas--validate-drop-rules loc) :to-be nil))))
        (delete-directory temp-dir t))))

  (it "returns warning when drops exceed assignment count"
    (let* ((temp-dir (make-temp-file "drop-test-" t))
           (assign-file (expand-file-name "assignments.org" temp-dir))
           (groups-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* Assignment 1\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n"))
            (with-temp-file groups-file
              (insert "* Homework\n:PROPERTIES:\n:DROP_LOWEST: 2\n:END:\n"))
            (let ((org-canvas-assignments-file assign-file))
              (with-current-buffer (find-file-noselect groups-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let* ((loc (list :file groups-file :line 1 :heading "Homework"))
                       (issues (org-canvas--validate-drop-rules loc)))
                  (expect issues :not :to-be nil)
                  (expect (plist-get (car issues) :severity) :to-equal 'warning)
                  (expect (plist-get (car issues) :message) :to-match "Drop rules")))))
        (delete-directory temp-dir t))))

  (it "warns when only DROP_HIGHEST exceeds assignment count"
    (let* ((temp-dir (make-temp-file "drop-test-" t))
           (assign-file (expand-file-name "assignments.org" temp-dir))
           (groups-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* HW 1\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n"))
            (with-temp-file groups-file
              (insert "* Homework\n:PROPERTIES:\n:DROP_HIGHEST: 2\n:END:\n"))
            (let ((org-canvas-assignments-file assign-file))
              (with-current-buffer (find-file-noselect groups-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let* ((loc (list :file groups-file :line 1 :heading "Homework"))
                       (issues (org-canvas--validate-drop-rules loc)))
                  (expect issues :not :to-be nil)
                  (expect (plist-get (car issues) :message) :to-match "Drop rules")))))
        (delete-directory temp-dir t))))

  (it "returns nil when drops are within count"
    (let* ((temp-dir (make-temp-file "drop-test-" t))
           (assign-file (expand-file-name "assignments.org" temp-dir))
           (groups-file (expand-file-name "assignment-groups.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* HW 1\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n* HW 2\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n* HW 3\n:PROPERTIES:\n:GROUP: [[file:assignment-groups.org::*Homework][Homework]]\n:END:\n"))
            (with-temp-file groups-file
              (insert "* Homework\n:PROPERTIES:\n:DROP_LOWEST: 1\n:END:\n"))
            (let ((org-canvas-assignments-file assign-file))
              (with-current-buffer (find-file-noselect groups-file)
                (goto-char (point-min))
                (org-back-to-heading)
                (let* ((loc (list :file groups-file :line 1 :heading "Homework"))
                       (issues (org-canvas--validate-drop-rules loc)))
                  (expect issues :to-be nil)))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--validate-override-section-ids"
  (it "returns warning for section link without CANVAS_ID"
    (let* ((temp-dir (make-temp-file "override-test-" t))
           (sections-file (expand-file-name "sections.org" temp-dir))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:END:\n"))
            (let ((link (format "[[file:%s::*Section A][Section A]]" sections-file)))
              (with-temp-file assign-file
                (insert (format "| %s | <2027-01-15> |\n" link)))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (let* ((issues (org-canvas--validate-override-section-ids
                                assign-file "Test Assignment")))
                  (expect issues :not :to-be nil)
                  (expect (plist-get (car issues) :severity) :to-equal 'warning)
                  (expect (plist-get (car issues) :message) :to-match "no CANVAS_ID")))))
        (delete-directory temp-dir t))))

  (it "returns nil for section link with CANVAS_ID"
    (let* ((temp-dir (make-temp-file "override-test-" t))
           (sections-file (expand-file-name "sections.org" temp-dir))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file sections-file
              (insert "* Section B\n:PROPERTIES:\n:CANVAS_ID: 200\n:END:\n"))
            (let ((link (format "[[file:%s::*Section B][Section B]]" sections-file)))
              (with-temp-file assign-file
                (insert (format "| %s | <2027-01-15> |\n" link)))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (let* ((issues (org-canvas--validate-override-section-ids
                                assign-file "Test Assignment")))
                  (expect issues :to-be nil)))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--validate-assignment-structure with section ID issues"
  (it "collects section ID issues from override table"
    (let* ((temp-dir (make-temp-file "val-assign-struct-" t))
           (sections-file (expand-file-name "sections.org" temp-dir))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            ;; Section without CANVAS_ID
            (with-temp-file sections-file
              (insert "* Section A\n:PROPERTIES:\n:END:\n"))
            (let ((link (format "[[file:%s::*Section A][Section A]]" sections-file)))
              (with-temp-file assign-file
                (insert (format "* Assignment 1\n:PROPERTIES:\n:POINTS: 100\n:END:\n\n#+NAME: overrides\n| Section | Due At |\n|---------+--------|\n| %s | <2027-01-15 Fri 23:59> |\n" link)))
              (with-current-buffer (find-file-noselect assign-file)
                (goto-char (point-min))
                (org-back-to-heading t)
                (let ((issues (org-canvas--validate-assignment-structure
                               (list :file assign-file :line 1 :heading "Assignment 1"))))
                  (expect issues :not :to-be nil)
                  (expect (cl-some (lambda (i) (string-match "no CANVAS_ID" (plist-get i :message)))
                                   issues)
                          :to-be-truthy)))))
        (delete-directory temp-dir t)))))

(describe "org-canvas--validate-weight-sum"
  (it "warns and names the direction when weights fall short of 100"
    (with-validate-test-dir dir
      (test-validate--weights-file dir "30.0" "30.0")
      (let ((issues (org-canvas--validate-weight-sum
                     (expand-file-name "assignment-groups.org" dir))))
        (expect (length issues) :to-equal 1)
        (expect (plist-get (car issues) :severity) :to-equal 'warning)
        (expect (plist-get (car issues) :message) :to-match "sum to 60")
        (expect (plist-get (car issues) :message) :to-match "inflated"))))

  (it "warns in the other direction when weights exceed 100"
    (with-validate-test-dir dir
      (test-validate--weights-file dir "60.0" "60.0")
      (let ((issues (org-canvas--validate-weight-sum
                     (expand-file-name "assignment-groups.org" dir))))
        (expect (plist-get (car issues) :message) :to-match "sum to 120")
        (expect (plist-get (car issues) :message) :to-match "deflated"))))

  (it "reports a fractional total without rounding it away"
    ;; Whole sums print as "60", not "60.0"; a fractional one has to keep
    ;; its decimals or the message would claim a total nobody wrote.
    (with-validate-test-dir dir
      (test-validate--weights-file dir "30.5" "30.0")
      (let ((issues (org-canvas--validate-weight-sum
                     (expand-file-name "assignment-groups.org" dir))))
        (expect (plist-get (car issues) :message) :to-match "sum to 60\\.5"))))

  (it "passes when the weights total exactly 100"
    (with-validate-test-dir dir
      (test-validate--weights-file dir "50.0" "30.0" "20.0")
      (expect (org-canvas--validate-weight-sum
               (expand-file-name "assignment-groups.org" dir))
              :to-be nil)))

  (it "tolerates repeating decimals that Canvas itself accepts"
    ;; 33.33 x2 + 33.34 is 100.00 exactly, but thirds generally are not;
    ;; the tolerance exists so a legitimate split is not flagged.
    (with-validate-test-dir dir
      (test-validate--weights-file dir "33.33" "33.33" "33.34")
      (expect (org-canvas--validate-weight-sum
               (expand-file-name "assignment-groups.org" dir))
              :to-be nil)))

  (it "says nothing when no group carries a weight"
    ;; An unweighted course is a normal configuration, not a mistake.
    (with-validate-test-dir dir
      (with-temp-file (expand-file-name "assignment-groups.org" dir)
        (insert "* Assignment Groups\n** Essays\n** Exams\n"))
      (expect (org-canvas--validate-weight-sum
               (expand-file-name "assignment-groups.org" dir))
              :to-be nil)))

  (it "reports the line of the first weighted group"
    (with-validate-test-dir dir
      (test-validate--weights-file dir "30.0" "30.0")
      (let ((issue (car (org-canvas--validate-weight-sum
                         (expand-file-name "assignment-groups.org" dir)))))
        ;; Line 2 is "** Group 1"; line 1 is the container heading.
        (expect (plist-get issue :line) :to-equal 2)))))

(describe "override rows for a group or students (issue #224)"
  (defmacro test-validate-override-target-with-files (&rest body)
    "Run BODY with groups.org (Team A = 55) and people.org (Adams, Alice = 1)."
    (declare (indent 0))
    `(let* ((dir (make-temp-file "ov-validate-" t))
            (org-canvas-groups-file (expand-file-name "groups.org" dir))
            (org-canvas-people-file (expand-file-name "people.org" dir))
            (loc (list :file "assignments.org" :line 3 :heading "HW")))
       (with-temp-file org-canvas-groups-file
         (insert "* Teams\n** Team A\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"))
       (with-temp-file org-canvas-people-file
         (insert "* Students\n** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"))
       (unwind-protect
           (progn ,@body)
         (dolist (f (list org-canvas-groups-file org-canvas-people-file))
           (let ((buf (find-buffer-visiting f)))
             (when buf (kill-buffer buf))))
         (delete-directory dir t))))

  (it "accepts a group that groups.org holds, or a literal #id"
    (test-validate-override-target-with-files
      (expect (org-canvas--validate-override-target "Group: Team A" loc) :to-be nil)
      (expect (org-canvas--validate-override-target "Group: #77" loc) :to-be nil)))

  (it "warns, push-only, about a group groups.org does not hold"
    (test-validate-override-target-with-files
      (let ((issue (org-canvas--validate-override-target "Group: Team Z" loc)))
        (expect (plist-get issue :severity) :to-equal 'warning)
        (expect (plist-get issue :message) :to-match "group 'Team Z' is not in groups.org")
        (expect (plist-get issue :push-only) :to-be t))))

  (it "names the students people.org does not hold and passes the rest"
    (test-validate-override-target-with-files
      (expect (org-canvas--validate-override-target "Students: Adams, Alice; #4" loc) :to-be nil)
      (let ((issue (org-canvas--validate-override-target
                    "Students: Adams, Alice; Nobody, Nell; Other, Otto" loc)))
        (expect (plist-get issue :message) :to-match "'Nobody, Nell', 'Other, Otto' not in people.org")
        (expect (plist-get issue :message) :not :to-match "Alice")
        (expect (plist-get issue :push-only) :to-be t))))

  (it "warns, push-only, about a student people.org marks as gone (issue #290)"
    (test-validate-override-target-with-files
      (with-temp-file org-canvas-people-file
        (insert "* Students\n** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n"
                ":ENROLLMENT_STATE: active\n:END:\n"
                "** Beta, Bob\n:PROPERTIES:\n:USER_ID: 2\n"
                ":ENROLLMENT_STATE: deleted\n:DEPARTED: <2026-09-14 Mon 19:19>\n:END:\n"))
      (expect (org-canvas--validate-override-target "Students: Adams, Alice" loc) :to-be nil)
      (let ((issue (org-canvas--validate-override-target "Students: Adams, Alice; Beta, Bob" loc)))
        (expect (plist-get issue :message)
                :to-equal "Override student(s) 'Beta, Bob' no longer enrolled according to people.org")
        (expect (plist-get issue :push-only) :to-be t))
      ;; A name people.org does not hold is the first thing to fix.
      (expect (plist-get (org-canvas--validate-override-target "Students: Beta, Bob; Nobody" loc)
                         :message)
              :to-match "'Nobody' not in people.org")))

  (it "warns when the lookup file is missing, unless the row carries an id"
    (let ((org-canvas-groups-file "/tmp/nonexistent-groups-xyzzy.org")
          (org-canvas-people-file "/tmp/nonexistent-people-xyzzy.org")
          (loc (list :file "assignments.org" :line 3 :heading "HW")))
      (expect (org-canvas--validate-override-target "Group: Team A" loc) :not :to-be nil)
      (expect (org-canvas--validate-override-target "Students: #1; #2" loc) :to-be nil)))

  (it "reports the three kinds through the assignment structure check"
    (test-validate-override-target-with-files
      (with-temp-org-buffer
          "* HW
:PROPERTIES:
:POINTS: 10
:END:

#+NAME: overrides
| Section                  | Due At           |
|--------------------------+------------------|
| Group: Team A            | <2027-02-15 Mon> |
| Students: Adams, Alice   | <2027-02-16 Tue> |
| Group: Team Z            | <2027-02-17 Wed> |
| Plain                    | <2027-02-18 Thu> |
"
        (org-back-to-heading t)
        (let ((messages (mapcar (lambda (i) (plist-get i :message))
                                (org-canvas--validate-assignment-structure
                                 (list :file (buffer-file-name) :line 1 :heading "HW")))))
          (expect (length messages) :to-equal 2)
          (expect (car messages) :to-match "Team Z")
          (expect (cadr messages) :to-match "'Plain' is not a section link"))))))

(describe "an Accept cell on an override row (issue #538)"
  (it "takes the row's findings of the checks it names, separated by ;"
    (let* ((org-canvas-validate-lock-after-due-hours 48)
           (brief (test-validate-538--brief
                   (test-validate-538--entry
                    (concat test-validate-538--online
                            ":LOCK_AT: <2099-03-12 Thu 23:59>\n")
                    (concat "| Section | Due At | Lock At | Accept |\n|---+---+---+---|\n"
                            "| Group: #1 | <2099-03-20 Fri 23:59> | <2099-03-19 Thu 23:59> | date-order Typo kept for the record; lock-window Debate week |\n"
                            "| Group: #2 | <2099-03-20 Fri 23:59> | <2099-03-19 Thu 23:59> | |\n"
                            "| Group: #3 | <2099-03-10 Tue 23:59> | <2099-03-12 Thu 23:59> | lock-window Gone |\n")))))
      (expect (mapcar (lambda (b) (list (nth 1 b) (nth 2 b) (nth 3 b))) brief)
              :to-equal '((12 date-order "Typo kept for the record")
                          (12 lock-window "Debate week")
                          (13 date-order nil)
                          (13 lock-window nil)
                          (14 nil nil)))
      (expect (nth 4 (car (last brief)))
              :to-equal "Override row 'Group: #3': Stale accept: Accept lock-window matched no finding here; remove it (Gone)")))

  (it "does not take the heading's findings"
    (let* ((org-canvas-validate-lock-after-due-hours 48)
           (brief (test-validate-538--brief
                   (test-validate-538--entry
                    test-validate-538--online
                    (concat "| Section | Due At | Lock At | Accept |\n|---+---+---+---|\n"
                            "| Group: #1 | <2099-03-10 Tue 23:59> | <2099-03-12 Thu 23:59> | lock-window In class |\n")))))
      (expect (mapcar (lambda (b) (list (nth 1 b) (nth 3 b) (nth 0 b))) brief)
              :to-equal '((1 nil warning) (11 nil note)))))

  (it "is left out by the override push, which reads its columns by title"
    (let ((plain (list '("Section" "Due At" "Lock At")
                       'hline
                       '("Group: #1" "<2099-03-10 Tue 23:59>" "<2099-03-12 Thu 23:59>")))
          (accept (list '("Section" "Accept" "Due At" "Lock At")
                        'hline
                        '("Group: #1" "lock-window In class" "<2099-03-10 Tue 23:59>" "<2099-03-12 Thu 23:59>"))))
      (expect (org-canvas--override-parse-table accept "/c/")
              :to-equal (org-canvas--override-parse-table plain "/c/")))))

(describe "override row date order on effective dates (issue #538)"
  (it "warns of a row's due date after the lock it inherits"
    (let ((brief (test-validate-538--brief
                  (test-validate-538--entry
                   ":DUE_AT: <2099-10-05 Mon 23:59>\n:LOCK_AT: <2099-10-07 Wed 23:59>\n"
                   (concat "| Section | Due At |\n|---+---|\n"
                           "| Students: #42 | <2099-10-09 Fri 23:59> |\n")))))
      (expect brief :to-equal
              '((warning 10 date-order nil "Override row 'Students: #42': Due At is after Lock At (<2099-10-09 Fri 23:59> > <2099-10-07 Wed 23:59> (the assignment's))")))))

  (it "leaves the heading's own order to the heading, once"
    (let ((brief (test-validate-538--brief
                  (test-validate-538--entry
                   ":DUE_AT: <2099-10-09 Fri 23:59>\n:LOCK_AT: <2099-10-07 Wed 23:59>\n"
                   (concat "| Section | Unlock At |\n|---+---|\n"
                           "| Group: #1 | <2099-10-01 Thu 00:00> |\n"
                           "| Group: #2 | <2099-10-02 Fri 00:00> |\n"
                           "| Group: #3 | <2099-10-03 Sat 00:00> |\n")))))
      (expect (mapcar #'cadr brief) :to-equal '(1))))

  (it "compares a row's own unlock with the due date it inherits"
    (let ((brief (test-validate-538--brief
                  (test-validate-538--entry
                   ":DUE_AT: <2099-10-05 Mon 23:59>\n"
                   (concat "| Section | Unlock At |\n|---+---|\n"
                           "| Group: #1 | <2099-10-06 Tue 00:00> |\n")))))
      (expect (nth 4 (car brief)) :to-match
              (regexp-quote "Unlock At is after Due At (<2099-10-06 Tue 00:00> > <2099-10-05 Mon 23:59> (the assignment's))")))))

(provide 'org-canvas-validate-grading-test)
;;; org-canvas-validate-grading-test.el ends here
