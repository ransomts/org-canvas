;;; org-canvas-submissions-verify-test.el --- Tests for reading a push back -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-submissions-verify' and the `verify' batch
;; command (issue #450).  Canvas is the column's submissions, mocked at
;; `org-canvas--submissions-fetch-for-assignment' (or at the all-pages
;; helper below it); no spec reaches Canvas.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-submissions-verify)
(require 'org-canvas-batch)

(cl-defun test-sver--file (&key (alice-score "8") delete draft (bob-status "unsubmitted"))
  "Return a grading file of two students.
ALICE-SCORE is Alice's SCORE (nil leaves it out), DELETE marks her
comment 12 for deletion, DRAFT is Bob's drafted comment and
BOB-STATUS his STATUS."
  (concat
   "#+TITLE: Submissions: HW\n#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001\n"
   "#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW\n\n"
   "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:STATUS: graded\n"
   (if alice-score (format ":SCORE: %s\n" alice-score) "")
   ":CANVAS_SCORE: 8\n:END:\n"
   "\n** Comments\n- *Prof* <2026-09-28 Mon 14:02> [11] :: One line.\n"
   (format "- %s*Prof* <2026-09-28 Mon 15:00> [12] ::\n  First.\n\n  Second.\n"
           (if delete "DELETE " ""))
   "\n** Rubric\n| Id | Criterion | Max | Score |\n|----+-----------+-----+-------|\n"
   "| _1 | Thesis    | 5   | 3     |\n| _2 | Structure | 5   | 5     |\n"
   "- _1 :: Thesis unclear.\n- _2 ::\n"
   "\n** Comment to post\n# Write a comment.\n\n"
   (format "* Baker, Bob\n:PROPERTIES:\n:USER_ID: 5002\n:STATUS: %s\n:END:\n" bob-status)
   "\n** Comment to post\n# Write a comment.\n" (if draft (concat draft "\n") "") "\n"))

(defun test-sver--comment (id text &optional author)
  "Return a Canvas submission comment ID with TEXT by AUTHOR (9 by default)."
  `((id . ,id) (author_id . ,(or author 9)) (comment . ,text)))

(cl-defun test-sver--alice (&key (score 8) excused posted
                                 (rubric '((_1 (points . 3) (comments . "Thesis unclear."))
                                           (_2 (points . 5) (comments . ""))))
                                 (comments (list (test-sver--comment 11 "One line.")
                                                 (test-sver--comment 12 "First.\n\nSecond."))))
  "Return Alice's submission as Canvas answers it, matching the file."
  `((user_id . 5001) (entered_score . ,score) (score . ,score)
    (excused . ,(if excused t :json-false))
    (posted_at . ,posted)
    (rubric_assessment . ,rubric)
    (submission_comments . ,(vconcat comments))))

(defun test-sver--bob ()
  "Return Bob's submission, ungraded and without comments."
  '((user_id . 5002) (entered_score) (score) (posted_at) (submission_comments . [])))

(defmacro with-sver-file (content submissions &rest body)
  "Run BODY on a scratch grading file CONTENT, Canvas answering SUBMISSIONS.
`dir' is the submissions directory, `file' the grading file and
`fetched' the assignment ids read."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "org-canvas-sver-" t))
          (org-canvas-submissions-directory dir)
          (file (expand-file-name "HW.org" dir))
          (fetched nil)
          (noninteractive t))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (with-html-to-org-identity
             (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment)
                        (lambda (id) (push id fetched) (vconcat ,submissions)))
                       ((symbol-function 'completing-read)
                        (lambda (&rest _) (error "Must not prompt")))
                       ((symbol-function 'message) #'ignore)
                       ((symbol-function 'princ) #'ignore))
               ,@body)))
       (let ((buf (find-buffer-visiting file)))
         (when (buffer-live-p buf)
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun test-sver--kinds (result)
  "Return the kinds of RESULT's findings, in order."
  (mapcar (lambda (f) (plist-get f :kind)) (plist-get result :findings)))

(describe "org-canvas-submissions-verify (issue #450)"
  (it "finds nothing when Canvas holds what the file says"
    (with-sver-file (test-sver--file) (list (test-sver--alice) (test-sver--bob))
      (let ((result (org-canvas-submissions-verify "1001")))
        (expect fetched :to-equal '("1001"))
        (expect (plist-get result :differences) :to-equal 0)
        (expect (plist-get result :findings) :to-be nil)
        (expect (plist-get result :students) :to-equal 2)
        (expect (plist-get result :unmatched) :to-equal 0)
        (expect (plist-get result :name) :to-equal "HW")
        (expect (plist-get result :file) :to-equal file)
        (dolist (key '(:posted :duplicates :scores :rubric-rows :comments
                       :comment-text :undeleted :drafts))
          (expect (plist-get result key) :to-equal 0)))))

  (it "reads the column through the all-pages helper with rubrics and comments"
    (with-org-canvas-test-config
     (let ((seen nil)
          (real (symbol-function 'org-canvas--submissions-fetch-for-assignment)))
      (with-sver-file (test-sver--file) nil
        (cl-letf (((symbol-function 'org-canvas--submissions-fetch-for-assignment) real)
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (method url params)
                     (setq seen (list method url params))
                     (vector (test-sver--alice) (test-sver--bob)))))
          (expect (plist-get (org-canvas-submissions-verify "HW") :differences)
                  :to-equal 0)))
      (expect (car seen) :to-be 'GET)
      (expect (nth 1 seen) :to-match "assignments/1001/submissions")
      (expect (nth 2 seen) :to-contain '("include[]" . "rubric_assessment"))
      (expect (nth 2 seen) :to-contain '("include[]" . "submission_comments")))))

  (it "counts a score Canvas does not hold"
    (with-sver-file (test-sver--file) (list (test-sver--alice :score 7) (test-sver--bob))
      (let* ((result (org-canvas-submissions-verify "1001"))
             (finding (car (plist-get result :findings))))
        (expect (plist-get result :scores) :to-equal 1)
        (expect (plist-get result :differences) :to-equal 1)
        (expect (plist-get finding :name) :to-equal "Adams, Alice")
        (expect (plist-get finding :user-id) :to-equal "5001")
        (expect (plist-get finding :file) :to-equal 8)
        (expect (plist-get finding :canvas) :to-equal 7))))

  (it "reads EX, a missing SCORE and a clear word as the push does"
    (with-sver-file (test-sver--file :alice-score "ex")
        (list (test-sver--alice :excused t) (test-sver--bob))
      (expect (plist-get (org-canvas-submissions-verify "1001") :scores) :to-equal 0))
    (with-sver-file (test-sver--file :alice-score nil)
        (list (test-sver--alice :score 9) (test-sver--bob))
      (expect (plist-get (org-canvas-submissions-verify "1001") :scores) :to-equal 1))
    (with-sver-file (test-sver--file :alice-score "8.0")
        (list (test-sver--alice) (test-sver--bob))
      (expect (plist-get (org-canvas-submissions-verify "1001") :scores) :to-equal 0))
    (with-sver-file (test-sver--file :alice-score "none")
        (list (test-sver--alice :score nil) (test-sver--bob))
      (expect (plist-get (org-canvas-submissions-verify "1001") :scores) :to-equal 0))
    (with-sver-file (test-sver--file :alice-score "eight")
        (list (test-sver--alice) (test-sver--bob))
      (let ((result (org-canvas-submissions-verify "1001")))
        (expect (plist-get result :scores) :to-equal 1)
        (expect (plist-get (car (plist-get result :findings)) :file) :to-equal "eight"))))

  (it "counts each Rubric row whose score or comment Canvas does not hold"
    (with-sver-file (test-sver--file)
        (list (test-sver--alice :rubric '((_1 (points . 2) (comments . "Thesis unclear."))))
              (test-sver--bob))
      (let* ((result (org-canvas-submissions-verify "1001"))
             (rows (seq-filter (lambda (f) (eq (plist-get f :kind) 'rubric-rows))
                               (plist-get result :findings))))
        (expect (plist-get result :rubric-rows) :to-equal 2)
        (expect (mapcar (lambda (f) (plist-get f :criterion-id)) rows) :to-equal '("_1" "_2"))
        (expect (plist-get (car rows) :file) :to-equal '("3" . "Thesis unclear."))
        (expect (plist-get (car rows) :canvas) :to-equal '("2" . "Thesis unclear."))
        (expect (plist-get (nth 1 rows) :canvas) :to-equal '(nil)))))

  (it "counts a comment the same author gave twice, but not the student's own"
    (with-sver-file (test-sver--file)
        (list (test-sver--alice
               :comments (list (test-sver--comment 11 "One line.")
                               (test-sver--comment 12 "First.\n\nSecond.")
                               (test-sver--comment 13 "One line.")
                               (test-sver--comment 14 "Thanks" 5001)
                               (test-sver--comment 15 "Thanks" 5001)
                               (test-sver--comment 16 "  ")))
              (test-sver--bob))
      (let ((result (org-canvas-submissions-verify "1001")))
        (expect (plist-get result :duplicates) :to-equal 1)
        (expect (plist-get (car (plist-get result :findings)) :id) :to-equal "13")
        (expect (plist-get result :comments) :to-equal 1)
        (expect (plist-get result :differences) :to-equal 2))))

  (it "counts a sent comment gone from Canvas or holding other text"
    (with-sver-file (test-sver--file)
        (list (test-sver--alice :comments (list (test-sver--comment 11 "Other.")))
              (test-sver--bob))
      (let ((result (org-canvas-submissions-verify "1001")))
        (expect (plist-get result :comment-text) :to-equal 2)
        (expect (plist-get result :comments) :to-equal 1)
        (expect (test-sver--kinds result) :to-equal '(comment-text comment-text comments)))))

  (it "counts an item marked DELETE whose comment Canvas still holds"
    (with-sver-file (test-sver--file :delete t) (list (test-sver--alice) (test-sver--bob))
      (let ((result (org-canvas-submissions-verify "1001")))
        (expect (plist-get result :undeleted) :to-equal 1)
        (expect (plist-get result :comments) :to-equal 0)
        (expect (plist-get result :differences) :to-equal 1)))
    (with-sver-file (test-sver--file :delete t)
        (list (test-sver--alice :comments (list (test-sver--comment 11 "One line.")))
              (test-sver--bob))
      (expect (plist-get (org-canvas-submissions-verify "1001") :differences) :to-equal 0)))

  (it "counts a drafted comment never posted, unless the student left"
    (with-sver-file (test-sver--file :draft "Well done.") (list (test-sver--alice) (test-sver--bob))
      (expect (plist-get (org-canvas-submissions-verify "1001") :drafts) :to-equal 1))
    (with-sver-file (test-sver--file :draft "Well done." :bob-status "left")
        (list (test-sver--alice))
      (let ((result (org-canvas-submissions-verify "1001")))
        (expect (plist-get result :drafts) :to-equal 0)
        (expect (plist-get result :unmatched) :to-equal 1))))

  (it "reports posted submissions, a difference only for a hidden push"
    (with-sver-file (test-sver--file)
        (list (test-sver--alice :posted "2026-10-01T10:00:00Z")
              '((user_id . 5009) (posted_at . "2026-10-01T10:00:00Z")))
      (let ((shown (org-canvas-submissions-verify "1001"))
            (hidden (org-canvas-submissions-verify "1001" t)))
        (expect (plist-get shown :posted) :to-equal 2)
        (expect (plist-get shown :differences) :to-equal 0)
        (expect (plist-get hidden :differences) :to-equal 2)
        (expect (mapcar (lambda (f) (plist-get f :name)) (plist-get hidden :findings))
                :to-equal '("Adams, Alice" "User 5009"))
        (expect (plist-get hidden :unmatched) :to-equal 1))))

  (it "verifies a list of columns, one plist each"
    (with-sver-file (test-sver--file) (list (test-sver--alice :score 7) (test-sver--bob))
      (let ((results (org-canvas-submissions-verify '("1001" "HW"))))
        (expect (length results) :to-equal 2)
        (expect (mapcar (lambda (r) (plist-get r :differences)) results) :to-equal '(1 1)))))

  (it "prints a report naming each difference"
    (with-sver-file (test-sver--file :delete t :draft "Hi.")
        (list (test-sver--alice
               :score 7 :posted "2026-10-01T10:00:00Z"
               :rubric '((_1 (points . 3) (comments . "Other words.")))
               :comments (list (test-sver--comment 11 "Changed.")
                               (test-sver--comment 12 "First.\n\nSecond.")
                               (test-sver--comment 13 "Changed.")))
              (test-sver--bob))
      (let ((text nil))
        (cl-letf (((symbol-function 'org-canvas--report-display)
                   (lambda (_name render &rest _)
                     (setq text (with-temp-buffer (funcall render) (buffer-string))))))
          (org-canvas-submissions-verify "1001" t))
        (expect text :to-match "^\\* HW (1001)")
        (expect text :to-match "posted 1, duplicate comments 1, mismatched scores 1")
        (expect text :to-match "posted 2026-10-01T10:00:00Z")
        (expect text :to-match "comment 13 repeats \"Changed.\"")
        (expect text :to-match "score: file 8, Canvas 7")
        (expect text :to-match "rubric row _1: file 3 \"Thesis unclear.\", Canvas 3 \"Other words.\"")
        (expect text :to-match "rubric row _2: file 5, Canvas -")
        (expect text :to-match "comment 11: Canvas holds other text")
        (expect text :to-match "comments: file lists 1, Canvas holds 2")
        (expect text :to-match "comment 12 marked DELETE is still on Canvas")
        (expect text :to-match "Baker, Bob (5002) :: drafted comment never posted"))))

  (it "words a missing comment, an unmatched heading and an absent score"
    (expect (org-canvas--submissions-verify-detail
             '(:kind comment-text :id "11" :canvas nil))
            :to-equal "comment 11: gone from Canvas")
    (expect (org-canvas--submissions-verify-detail '(:kind scores :file nil :canvas 5))
            :to-equal "score: file none, Canvas 5")
    (expect (org-canvas--submissions-verify-summary
             '(:name "HW" :differences 0 :posted 0 :duplicates 0 :scores 0 :rubric-rows 0
               :comments 0 :comment-text 0 :undeleted 0 :drafts 0 :students 3 :unmatched 1))
            :to-match "3 student(s), 1 with no submission on Canvas\\'"))

  (it "takes the grading buffer at hand interactively, a prefix arg for hidden"
    (with-sver-file (test-sver--file)
        (list (test-sver--alice :posted "2026-10-01T10:00:00Z") (test-sver--bob))
      (with-current-buffer (org-canvas--submissions-visit-grading-file file)
        (let* ((current-prefix-arg '(4))
               (result (call-interactively #'org-canvas-submissions-verify)))
          (expect (plist-get result :hidden) :to-be t)
          (expect (plist-get result :differences) :to-equal 1))))))

(describe "scripts/org-canvas verify (issue #450)"
  (it "exits 0 when every column matches and 1 when one differs"
    (with-sver-file (test-sver--file)
        (list (test-sver--alice :posted "2026-10-01T10:00:00Z") (test-sver--bob))
      (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore))
        (expect (org-canvas-batch-main '("verify" "1001")) :to-equal 0)
        (expect (org-canvas-batch-main '("verify" "1001" "HW")) :to-equal 0)
        (expect (org-canvas-batch-main '("verify" "--hidden" "1001")) :to-equal 1))))

  (it "is a usage error without a column"
    (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
              ((symbol-function 'message) #'ignore))
      (expect (org-canvas-batch-main '("verify" "--hidden")) :to-equal 2)
      (expect (org-canvas-batch-main '("verify")) :to-equal 2))))

(provide 'org-canvas-submissions-verify-test)
;;; org-canvas-submissions-verify-test.el ends here
