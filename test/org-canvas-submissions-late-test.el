;;; org-canvas-submissions-late-test.el --- Tests for the late-note drafts -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-draft-late-notes' (issue #484) and
;; the DUE_AT the pull writes for it.  Everything is local: no spec
;; reaches Canvas.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-late)
(require 'org-canvas-batch)

(defun test-slate--student (name uid props &rest sections)
  "Return a grading-file heading NAME for UID with PROPS and SECTIONS text."
  (concat (format "* %s\n:PROPERTIES:\n:USER_ID: %s\n" name uid)
          (mapconcat (lambda (p) (format ":%s: %s\n" (car p) (cdr p))) props "")
          ":END:\n" (apply #'concat sections)))

(defconst test-slate--draft "\n** Comment to post\n# Write a comment.\n\n"
  "An empty Comment to post section under its template.")

(defun test-slate--late (score &rest more)
  "Return the properties of a late row scored SCORE, two days late, and MORE."
  (append `(("STATUS" . "late") ("DAYS_LATE" . "2")
            ,@(and score (list (cons "SCORE" score)))
            ("SUBMITTED_AT" . "<2026-10-09 Fri 10:00>")
            ("DUE_AT" . "<2026-10-07 Wed 23:59>"))
          more))

(defun test-slate--file ()
  "Return the grading file the specs read."
  (concat
   "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
   "#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n#+PROPERTY: POINTS_POSSIBLE 10\n\n"
   (test-slate--student "Adams, Alice" 5001 (test-slate--late "8") test-slate--draft)
   (test-slate--student "Baker, Bob" 5002 (test-slate--late nil) test-slate--draft)
   (test-slate--student "Chen, Cara" 5003 (test-slate--late "9"))
   (test-slate--student "Davis, Dan" 5004 '(("STATUS" . "submitted") ("SCORE" . "10"))
                        test-slate--draft)
   (test-slate--student "Evans, Eve" 5005
                        '(("STATUS" . "late") ("DAYS_LATE" . "1") ("SCORE" . "7")
                          ("SUBMITTED_AT" . "<2026-10-08 Thu 09:00>"))
                        test-slate--draft)
   (test-slate--student "Fox, Finn" 5006 (test-slate--late "5")
                        "\n** Comment to post\n# Write a comment.\nGood argument.\n\n")))

(defun test-slate--settings (&optional props)
  "Return a settings.org text whose late policy has PROPS over the default."
  (concat "* Course Settings\n:PROPERTIES:\n"
          (mapconcat (lambda (p) (format ":%s: %s\n" (car p) (cdr p)))
                     (or props
                         '(("LATE_SUBMISSION_DEDUCTION_ENABLED" . "true")
                           ("LATE_SUBMISSION_DEDUCTION" . "10")
                           ("LATE_SUBMISSION_INTERVAL" . "day")
                           ("LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED" . "true")
                           ("LATE_SUBMISSION_MINIMUM_PERCENT" . "50")))
                     "")
          ":END:\n"))

(defmacro with-slate-file (&rest body)
  "Run BODY in a scratch grading file holding `test-slate--file'.
`dir' is the submissions directory, `file' the grading file and
`settings' the settings file, whose late policy `test-slate--settings'
writes."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "org-canvas-slate-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (settings (expand-file-name "settings.org" dir))
          (org-canvas-settings-file settings)
          (system-time-locale "C")
          (org-canvas--dry-run nil)
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert (test-slate--file)))
           (with-temp-file settings (insert (test-slate--settings)))
           (setq buf (org-canvas--submissions-visit-grading-file file))
           (with-current-buffer buf
             (cl-letf (((symbol-function 'org-canvas--log-info) #'ignore)
                       ((symbol-function 'org-canvas--log-warning) #'ignore))
               ,@body)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (when-let* ((sb (get-file-buffer settings)))
         (with-current-buffer sb (set-buffer-modified-p nil))
         (kill-buffer sb))
       (delete-directory dir t))))

(defmacro test-slate--quietly (&rest body)
  "Run BODY with `message' and `org-canvas--user-message' silenced."
  `(cl-letf (((symbol-function 'message) #'ignore)
             ((symbol-function 'org-canvas--user-message) #'ignore))
     ,@body))

(defun test-slate--draft-of (uid)
  "Return the drafted comment of student UID in the current grading buffer."
  (save-excursion
    (org-canvas--submissions-goto-user uid)
    (org-canvas--submissions-comment-draft)))

(defun test-slate--rewrite-settings (props)
  "Replace the settings file's late policy with PROPS, its buffer killed."
  (when-let* ((sb (get-file-buffer org-canvas-settings-file)))
    (kill-buffer sb))
  (with-temp-file org-canvas-settings-file (insert (test-slate--settings props))))

(defconst test-slate--alice-note
  (concat "This came in Fri Oct 9 at 10:00, after the due time of Wed Oct 7 at 23:59: "
          "2 days late.  The course's late policy takes 20% for that, so your 8 shows "
          "as 6 of 10.")
  "The note Alice's row gets under the default policy.")

;;;; The arithmetic

(describe "org-canvas--submissions-late-final (issue #484)"
  (it "takes the percent of the points possible"
    (expect (org-canvas--submissions-late-final 8 10 20 nil) :to-equal 6.0))
  (it "never takes a score below the floor, and never raises one under it"
    (expect (org-canvas--submissions-late-final 8 10 50 60) :to-equal 6.0)
    (expect (org-canvas--submissions-late-final 5 10 20 60) :to-equal 5.0))
  (it "stops at zero without a floor"
    (expect (org-canvas--submissions-late-final 3 10 120 nil) :to-equal 0.0))
  (it "rounds the deduction to two places"
    (expect (org-canvas--submissions-late-final 5 7 3 nil) :to-equal 4.79)))

(describe "org-canvas--submissions-late-number (issue #484)"
  (it "drops trailing zeros and keeps two places at most"
    (expect (org-canvas--submissions-late-number 6.0) :to-equal "6")
    (expect (org-canvas--submissions-late-number 6.5) :to-equal "6.5")
    (expect (org-canvas--submissions-late-number 6.256) :to-equal "6.26")
    (expect (org-canvas--submissions-late-number 10) :to-equal "10")))

(describe "org-canvas--submissions-late-pattern (issue #484)"
  (it "matches any note of the format's shape, its percent sign literal"
    (let ((org-canvas-submissions-late-note-format "Late %n day(s): %p%% off, %r to %f."))
      (let ((re (org-canvas--submissions-late-pattern)))
        (expect "Late 2 day(s): 20% off, 8 to 6." :to-match re)
        (expect "Late 2 day(s), 20 off" :not :to-match re)))))

;;;; The command

(describe "org-canvas-submissions-draft-late-notes (issue #484)"
  (it "drafts each scored late row's note and names what it skipped"
    (with-slate-file
      (let ((result (test-slate--quietly (org-canvas-submissions-draft-late-notes))))
        (expect (mapcar #'cadr (plist-get result :drafted))
                :to-equal '("5001" "5003" "5006"))
        (expect (nth 2 (car (plist-get result :drafted))) :to-equal test-slate--alice-note)
        (expect (plist-get result :skipped)
                :to-equal '(("Baker, Bob" "5002" "no score")
                            ("Evans, Eve" "5005" "no DUE_AT (re-pull the column)")))
        (expect (test-slate--draft-of "5001") :to-equal test-slate--alice-note)
        (expect (test-slate--draft-of "5004") :to-be nil)
        (expect (buffer-modified-p) :to-be nil))))

  (it "keeps the template, follows a draft already there, and adds a missing heading"
    (with-slate-file
      (test-slate--quietly (org-canvas-submissions-draft-late-notes file))
      (expect (buffer-string) :to-match
              "^\\*\\* Comment to post\n# Write a comment\\.\nThis came in Fri Oct 9")
      (expect (test-slate--draft-of "5006") :to-match "\\`Good argument\\.\n\nThis came in ")
      (expect (test-slate--draft-of "5006") :to-match "your 5 shows as 5 of 10\\.\\'")
      (expect (test-slate--draft-of "5003") :to-match "your 9 shows as 7 of 10\\.\\'")
      (expect (buffer-string) :to-match
              "your 9 shows as 7 of 10\\.\n\n\\* Davis, Dan")))

  (it "leaves a note already there alone, and names one a regrade changed"
    (with-slate-file
      (test-slate--quietly (org-canvas-submissions-draft-late-notes))
      (let ((before (buffer-string))
            (again (test-slate--quietly (org-canvas-submissions-draft-late-notes))))
        (expect (plist-get again :drafted) :to-be nil)
        (expect (length (plist-get again :same)) :to-equal 3)
        (expect (buffer-string) :to-equal before))
      (org-canvas--submissions-goto-user "5001")
      (org-entry-put (point) "SCORE" "9")
      (let* ((before (buffer-string))
             (third (test-slate--quietly (org-canvas-submissions-draft-late-notes))))
        (expect (plist-get third :changed) :to-equal '(("Adams, Alice" "5001")))
        (expect (buffer-string) :to-equal before))))

  (it "counts a note in a sent comment as there"
    (with-slate-file
      (org-canvas--submissions-goto-user "5001")
      (org-end-of-subtree t t)
      (insert "** Comments\n- *Prof* <2026-10-10 Sat 09:00> [11] :: "
              (replace-regexp-in-string "  " " " test-slate--alice-note) "\n\n")
      (let ((result (test-slate--quietly (org-canvas-submissions-draft-late-notes))))
        (expect (plist-get result :same) :to-equal '(("Adams, Alice" "5001"))))))

  (it "holds the students named by id or by name"
    (with-slate-file
      (let ((result (test-slate--quietly
                     (org-canvas-submissions-draft-late-notes nil '("5001" "Chen, Cara")))))
        (expect (plist-get result :held)
                :to-equal '(("Adams, Alice" "5001") ("Chen, Cara" "5003")))
        (expect (test-slate--draft-of "5001") :to-be nil))))

  (it "asks for the students to hold under a prefix argument"
    (with-slate-file
      (let ((noninteractive nil) (current-prefix-arg '(4)) result)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "5001, 5003")))
          (setq result (test-slate--quietly
                        (call-interactively #'org-canvas-submissions-draft-late-notes))))
        (expect (length (plist-get result :held)) :to-equal 2))))

  (it "writes nothing under a dry run and shows each note"
    (with-slate-file
      (let ((before (buffer-string)) (shown nil))
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'org-canvas--user-message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) shown))))
          (let ((result (org-canvas-submissions-draft-late-notes nil nil t)))
            (expect (plist-get result :dry-run) :to-be t)
            (expect (length (plist-get result :drafted)) :to-equal 3)))
        (expect (buffer-string) :to-equal before)
        (expect (car shown) :to-match
                "^\\[DRY-RUN\\] Late notes: 3 would be drafted, 0 already there, 0 changed, 0 held, 2 skipped$")
        (expect (seq-find (lambda (m) (string-prefix-p "[DRY-RUN] Late note for Adams" m))
                          shown)
                :to-be-truthy)
        (expect (seq-find (lambda (m) (string-match-p "skipped Baker, Bob (5002): no score" m))
                          shown)
                :to-be-truthy))))

  (it "names the held and changed students in its report"
    (let ((shown nil))
      (cl-letf (((symbol-function 'org-canvas--user-message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) shown))))
        (org-canvas--submissions-late-report
         '(:drafted (("A" "1" "n")) :same nil :changed (("B" "2")) :held (("C" "3"))
           :skipped nil :dry-run nil)))
      (expect (car shown) :to-equal
              "Late notes: 1 drafted, 0 already there, 1 changed, 1 held, 0 skipped")
      (expect shown :to-contain "Late notes: held C (3)")
      (expect (seq-find (lambda (m) (string-match-p "B (2) has a late note with other" m))
                        shown)
              :to-be-truthy)))

  (it "counts hours for an hourly policy"
    (with-slate-file
      (test-slate--rewrite-settings '(("LATE_SUBMISSION_DEDUCTION_ENABLED" . "true")
                                      ("LATE_SUBMISSION_DEDUCTION" . "1")
                                      ("LATE_SUBMISSION_INTERVAL" . "hour")))
      (let* ((result (test-slate--quietly (org-canvas-submissions-draft-late-notes)))
             (note (nth 2 (car (plist-get result :drafted)))))
        (expect note :to-match ": 35 hours late\\. ")
        (expect note :to-match "takes 35% for that, so your 8 shows as 4\\.5 of 10\\."))))

  (it "refuses a course whose late policy takes nothing, or is not pulled"
    (with-slate-file
      (test-slate--rewrite-settings '(("LATE_SUBMISSION_DEDUCTION_ENABLED" . "false")
                                      ("LATE_SUBMISSION_DEDUCTION" . "10")))
      (expect (org-canvas-submissions-draft-late-notes) :to-throw 'user-error)
      (with-temp-file org-canvas-settings-file (insert "No heading here.\n"))
      (when-let* ((sb (get-file-buffer org-canvas-settings-file))) (kill-buffer sb))
      (expect (org-canvas-submissions-draft-late-notes) :to-throw 'user-error)
      (delete-file org-canvas-settings-file)
      (expect (org-canvas-submissions-draft-late-notes) :to-throw 'user-error)))

  (it "refuses a grading file without POINTS_POSSIBLE"
    (with-slate-file
      (goto-char (point-min))
      (re-search-forward "^#\\+PROPERTY: POINTS_POSSIBLE.*\n")
      (replace-match "")
      (expect (org-canvas-submissions-draft-late-notes) :to-throw 'user-error))))

(describe "org-canvas--submissions-late-facts (issue #484)"
  (it "says what is missing"
    (let ((policy '(:deduction 10 :interval "day" :minimum nil)))
      (with-temp-buffer
        (org-mode)
        (insert "* A\n:PROPERTIES:\n:SCORE: 8\n:DUE_AT: <2026-10-07 Wed 23:59>\n:END:\n")
        (goto-char (point-min))
        (expect (org-canvas--submissions-late-facts policy 10) :to-equal "no SUBMITTED_AT")
        (org-entry-put (point) "SUBMITTED_AT" "<2026-10-09 Fri 10:00>")
        (expect (org-canvas--submissions-late-facts policy 10) :to-equal "no DAYS_LATE")))))

;;;; The pull's DUE_AT

(describe "DUE_AT (issue #484)"
  (it "is the student's cached due date, or submitted_at less seconds_late"
    (let ((org-canvas-time-zone "UTC"))
      (org-canvas--time-zone-reset)
      (unwind-protect
          (progn
            (expect (org-canvas--submissions-due-at
                     '((submitted_at . "2026-10-09T10:00:00Z") (seconds_late . 122460)
                       (cached_due_date . "2026-10-07T23:59:00Z")))
                    :to-equal "<2026-10-07 Wed 23:59>")
            (expect (org-canvas--submissions-due-at
                     '((submitted_at . "2026-10-09T10:00:00Z") (seconds_late . 122460)))
                    :to-equal "<2026-10-07 Wed 23:59>")
            (expect (org-canvas--submissions-due-at
                     '((submitted_at . "2026-10-09T10:00:00Z") (seconds_late . 0)
                       (cached_due_date . "2026-10-09T23:59:00Z")))
                    :to-be nil))
        (org-canvas--time-zone-reset))))

  (it "is written beside DAYS_LATE on a late row"
    (with-temp-buffer
      (org-mode)
      (org-canvas--submissions-render-detail-entry
       (test-org-canvas-make-submission '((late . t) (seconds_late . 400000))))
      (expect (buffer-string) :to-match ":DAYS_LATE: 5\n:DUE_AT: <[-0-9]+ [A-Za-z]+ [0-9:]+>\n"))))

;;;; Batch

(defun test-slate--batch (args)
  "Return the exit status of the batch command line ARGS."
  (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
            ((symbol-function 'message) #'ignore)
            ((symbol-function 'org-canvas--user-message) #'ignore))
    (let (status)
      (with-output-to-string (setq status (org-canvas-batch-main args)))
      status)))

(describe "the draft-late-notes subcommand (issue #484)"
  (it "drafts, a dry run on -n, exiting 1 on a skip"
    (with-slate-file
      (let ((before (buffer-string)))
        (expect (test-slate--batch '("-n" "draft-late-notes" "HW")) :to-equal 1)
        (expect (buffer-string) :to-equal before)
        (expect org-canvas--dry-run :to-be nil)
        (expect (test-slate--batch '("draft-late-notes" "--hold" "5002,5005" "HW"))
                :to-equal 0)
        (expect (test-slate--draft-of "5001") :to-equal test-slate--alice-note))))

  (it "refuses a --hold with nothing after it, and no file"
    (expect (test-slate--batch '("draft-late-notes" "HW" "--hold")) :to-equal 2)
    (expect (test-slate--batch '("draft-late-notes" "--hold" "5001")) :to-equal 2)))

(provide 'org-canvas-submissions-late-test)
;;; org-canvas-submissions-late-test.el ends here
