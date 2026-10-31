;;; org-canvas-gradebook-scores-test.el --- Buttercup tests for the group scores  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-gradebook-scores': the drop rules, the weighted
;; and unweighted total, the letter scheme, Canvas's counted scores, the
;; scores file and the Groups table.  Every request is answered by the
;; gradebook fake in test-fixtures.el; nothing here reaches the network
;; (Hard Rule 2), and the log is never read from the shared buffer
;; (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defun test-gradebook--ids (items)
  "Return the assignment ids of ITEMS, sorted."
  (sort (mapcar #'car items) #'<))

(defun test-gradebook--subsets (items k)
  "Return every K-element subset of ITEMS."
  (cond ((= k 0) (list nil))
        ((null items) nil)
        (t (append (mapcar (lambda (s) (cons (car items) s))
                           (test-gradebook--subsets (cdr items) (1- k)))
                   (test-gradebook--subsets (cdr items) k)))))

(defun test-gradebook--best-ratio (items k)
  "Return the highest ratio any K of ITEMS reach: the brute-force answer."
  (apply #'max (mapcar #'org-canvas--gradebook-ratio (test-gradebook--subsets items k))))

(describe "org-canvas--gradebook-drop"
  (it "returns the items untouched without drop rules"
    (let ((items '((1 5 10) (2 0 10))))
      (expect (org-canvas--gradebook-drop items '(:drop-lowest 0)) :to-equal items)
      (expect (org-canvas--gradebook-drop items nil) :to-equal items)))

  (it "drops the score whose removal maximises the percentage, not the lowest one"
    ;; Dropping 0/1 leaves 15/30 = 50%; dropping 5/20 leaves 10/11.
    (let ((items '((1 5 20) (2 0 1) (3 10 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 1)))
              :to-equal '(2 3))))

  (it "drops plain lowest scores when the points are equal"
    (let ((items '((1 7 10) (2 3 10) (3 9 10) (4 5 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 2)))
              :to-equal '(1 3))))

  (it "finds the best set that brute force finds"
    (let ((items '((1 3 4) (2 18 25) (3 0 2) (4 9 10) (5 40 60) (6 1 1) (7 12 30))))
      (dolist (drop '(1 2 3 4))
        (expect (org-canvas--gradebook-ratio
                 (org-canvas--gradebook-drop items (list :drop-lowest drop)))
                :to-be-close-to (test-gradebook--best-ratio items (- 7 drop)) 9))))

  (it "drops the score whose removal minimises the percentage for drop_highest"
    ;; Dropping 10/10 leaves 5/21; dropping 5/20 leaves 10/11.
    (let ((items '((1 5 20) (2 0 1) (3 10 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-highest 1)))
              :to-equal '(1 2))))

  (it "applies drop_lowest before drop_highest"
    (let ((items '((1 2 10) (2 4 10) (3 6 10) (4 8 10))))
      (expect (test-gradebook--ids
               (org-canvas--gradebook-drop items '(:drop-lowest 1 :drop-highest 1)))
              :to-equal '(2 3))))

  (it "always keeps one score, and lets drop_highest give way when the two cover all"
    (let ((items '((1 2 10) (2 4 10) (3 6 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 5)))
              :to-equal '(3))
      (expect (test-gradebook--ids
               (org-canvas--gradebook-drop items '(:drop-lowest 2 :drop-highest 1)))
              :to-equal '(3))))

  (it "never drops a never_drop score, which still counts toward the ratio"
    (let ((items '((1 0 10) (2 4 10) (3 6 10))))
      (expect (test-gradebook--ids
               (org-canvas--gradebook-drop items '(:drop-lowest 1 :never-drop (1))))
              :to-equal '(1 3))
      (expect (org-canvas--gradebook-drop '((1 0 10)) '(:drop-lowest 1 :never-drop (1)))
              :to-equal '((1 0 10)))))

  (it "drops by raw score when no droppable score has points"
    (let ((items '((1 3 0) (2 1 0) (3 2 0))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 1)))
              :to-equal '(1 3))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-highest 1)))
              :to-equal '(2 3))))

  (it "ranks from zero when the first guess has no points"
    (let ((items '((1 1 0) (2 1 0) (3 5 10))))
      (expect (test-gradebook--ids (org-canvas--gradebook-drop items '(:drop-lowest 1)))
              :to-equal '(1 3)))))

(describe "org-canvas--gradebook-total"
  (let ((quizzes '(:id 1 :weight 40))
        (essays '(:id 2 :weight 60)))
    (it "weights each group's percentage"
      (expect (org-canvas--gradebook-total
               (list (cons quizzes '(8 . 10)) (cons essays '(50 . 100))) t)
              :to-be-close-to 62.0 6))

    (it "scales up to the weights of the groups with points possible"
      (expect (org-canvas--gradebook-total
               (list (cons quizzes '(8 . 10)) (cons essays nil)) t)
              :to-be-close-to 80.0 6))

    (it "answers nil when no weighted group has points"
      (expect (org-canvas--gradebook-total (list (cons quizzes nil)) t) :to-be nil)
      (expect (org-canvas--gradebook-total (list (cons '(:id 3 :weight 0) '(5 . 10))) t)
              :to-be nil))

    (it "counts every kept point unweighted"
      (expect (org-canvas--gradebook-total
               (list (cons quizzes '(8 . 10)) (cons essays '(50 . 100))) nil)
              :to-be-close-to (/ 5800.0 110) 6)
      (expect (org-canvas--gradebook-total (list (cons quizzes nil)) nil) :to-be nil))))

(describe "org-canvas--gradebook-median"
  (it "takes the middle number, the mean of the middle two, and nothing of blanks"
    (expect (org-canvas--gradebook-median '(90 nil 70 80)) :to-equal 80.0)
    (expect (org-canvas--gradebook-median '(90 70 80.5 60)) :to-equal 75.25)
    (expect (org-canvas--gradebook-median '(nil :null)) :to-be nil)))

(describe "org-canvas--gradebook-letter and the statistics"
  (let ((scheme '(("B" . 80.0) ("A" . 90.0) ("F" . 0.0))))
    (it "rounds to two decimals before reading the cutoff"
      (expect (org-canvas--gradebook-letter 89.996 scheme) :to-equal "A")
      (expect (org-canvas--gradebook-letter 89.994 scheme) :to-equal "B")
      (expect (org-canvas--gradebook-letter -3 scheme) :to-equal "F")
      (expect (org-canvas--gradebook-letter nil scheme) :to-be nil)
      (expect (org-canvas--gradebook-letter 95 nil) :to-be nil))

    (it "counts letters best first and leaves out letters nobody gets"
      (expect (org-canvas--gradebook-letter-counts '(95 85 82 nil) scheme)
              :to-equal '(("A" . 1) ("B" . 2)))))

  (it "takes the median of odd, even and empty lists"
    (expect (org-canvas--gradebook-median '(3 1 2)) :to-equal 2.0)
    (expect (org-canvas--gradebook-median '(4 1 nil 2 3)) :to-equal 2.5)
    (expect (org-canvas--gradebook-median '(nil)) :to-be nil)))

(describe "org-canvas--gradebook-assignment-map and the counted scores"
  (it "keeps published assignments that count toward the grade"
    (let ((groups `(((id . 1) (assignments . [((id . 11) (published . t) (points_possible . 10))
                                              ((id . 12) (published . :json-false) (points_possible . 5))
                                              ((id . 13) (published . t) (points_possible . :null))
                                              ((id . 14) (published . t) (omit_from_final_grade . t)
                                               (points_possible . 5))])))))
      (expect (org-canvas--gradebook-assignment-map groups)
              :to-equal '((11 1 . 10) (13 1 . 0)))))

  (it "counts graded scores that are not excused, and only posted ones when told"
    (let ((map '((11 1 . 10))))
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 7) (posted_at . :null)) map t)
              :to-equal '(11 7 10))
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 7) (posted_at . :null)) map nil)
              :to-be nil)
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 7) (posted_at . "2026-09-01T00:00:00Z")) map nil)
              :to-equal '(11 7 10))
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . 0) (excused . t)) map t)
              :to-be nil)
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 11) (score . :null)) map t)
              :to-be nil)
      (expect (org-canvas--gradebook-submission-item
               '((assignment_id . 99) (score . 3)) map t)
              :to-be nil)))

  (it "reads Canvas's rules, null or nested, into a group plist"
    (expect (org-canvas--gradebook-canvas-group
             '((id . 1) (name . "Q") (group_weight . 15)
               (rules . ((drop_lowest . 1) (never_drop . [11 12])))))
            :to-equal '(:id 1 :name "Q" :weight 15 :drop-lowest 1 :drop-highest nil
                            :never-drop (11 12)))
    (expect (org-canvas--gradebook-canvas-group
             '((id . 2) (name . "E") (group_weight . :null) (rules . :null)))
            :to-equal '(:id 2 :name "E" :weight 0 :drop-lowest nil :drop-highest nil
                            :never-drop nil))))

(defun test-gradebook--groups-row (name)
  "Return NAME's row of the Groups table, cells trimmed, or nil."
  (let ((text (test-gradebook--file)))
    (when (string-match "^\\* Groups\n" text)
      (let ((groups (substring text (match-end 0))))
        (when (string-match (format "^| *%s *|\\(.*\\)$" (regexp-quote name)) groups)
          (mapcar #'string-trim (split-string (match-string 1 groups) "|" t)))))))

(describe "org-canvas-pull-gradebook with org-canvas-gradebook-groups"
  (it "reads the course once, for the role and the group weights (issue #488)"
    (test-gradebook--with-scores 40.0
      (org-canvas-pull-gradebook)
      (expect test-gradebook--course-reads :to-equal '((("include[]" . "total_scores"))))
      (expect (test-gradebook--groups-row "Adams, Alice") :to-be-truthy)))

  (it "keeps gradebook.org and the scores file out of git in a work tree (issue #549)"
    (test-gradebook--with-scores 50.0
      ;; rev-parse answers 0 (a work tree); ls-files and check-ignore
      ;; answer 1 (not tracked, not ignored), so the pull adds both
      ;; names, the file's before the scores file's.
      (cl-letf (((symbol-function 'process-file)
                 (lambda (_program _in _out _display &rest args)
                   (if (equal (car args) "rev-parse") 0 1))))
        (org-canvas-pull-gradebook))
      (expect (with-temp-buffer
                (insert-file-contents (expand-file-name ".gitignore" dir))
                (buffer-string))
              :to-equal "gradebook.org\ngradebook-scores.eld\n")))

  (it "reads the course itself when it was not handed one"
    (test-gradebook--with-scores 40.0
      (let ((scores (org-canvas--gradebook-fetch-scores
                     (list (list :user-id 1 :name "Adams, Alice" :current 86.67)) nil)))
        (expect (plist-get scores :weighted) :to-be t)
        (expect (length test-gradebook--course-reads) :to-equal 1))))

  (it "writes each group's percentage after the drop rules and checks Canvas's total"
    (test-gradebook--with-scores 40.0
      (spy-on 'org-canvas--log-warning)
      (org-canvas-pull-gradebook)
      (let ((text (test-gradebook--file)))
        (expect text :to-match "^| Student +| Quizzes +| Essays +| Computed +| Canvas +|$")
        (expect (test-gradebook--groups-row "Adams, Alice")
                :to-equal '("93.3" "80.0" "86.7" "86.7"))
        (expect (test-gradebook--groups-row "Beta, Bob")
                :to-equal '("50.0" "-" "50.0" "40.0"))
        (expect text :to-match "1 student(s) whose computed total differs from Canvas's by more than 0.1: Beta, Bob")
        (expect (spy-calls-count 'org-canvas--log-warning) :to-equal 1))
      (let ((scores (org-canvas--gradebook-read-scores)))
        (expect (plist-get scores :weighted) :to-be t)
        (expect (plist-get scores :scheme) :to-equal '(("A" . 85.0) ("B" . 75.0) ("F" . 0.0)))
        (expect (mapcar (lambda (g) (plist-get g :name)) (plist-get scores :groups))
                :to-equal '("Quizzes" "Essays")))))

  (it "says every total matches when they do, and counts only posted scores when told"
    (test-gradebook--with-scores 50.0
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :to-match "Every computed total is within 0.1 of Canvas's")
      (let ((org-canvas-gradebook-unposted nil)
            (test-gradebook--submissions
             (cons (test-gradebook--sub 2 21 100 '(posted_at . :null))
                   (test-gradebook--fixture-submissions))))
        (org-canvas-pull-gradebook)
        (expect (test-gradebook--groups-row "Beta, Bob") :to-equal '("50.0" "-" "50.0" "50.0")))))

  (it "removes the Groups heading when the option is turned off"
    (test-gradebook--with-scores 50.0
      (org-canvas-pull-gradebook)
      (expect (test-gradebook--file) :to-match "^\\* Groups")
      (let ((org-canvas-gradebook-groups nil))
        (org-canvas-pull-gradebook))
      (expect (test-gradebook--file) :not :to-match "Groups")
      (expect (test-gradebook--file) :to-match "^\\* Assignments")))

  (it "leaves a note and keeps no scores when Canvas refuses them"
    (test-gradebook--with-scores 50.0
      (let ((test-gradebook--submissions 'refuse))
        (spy-on 'org-canvas--log-warning)
        (org-canvas-pull-gradebook)
        (expect (test-gradebook--file) :to-match "Canvas refused the scores for this token")
        (expect (spy-calls-count 'org-canvas--log-warning) :to-equal 1)
        (expect (file-exists-p org-canvas-gradebook-scores-file) :to-be nil)))))

(describe "org-canvas--gradebook-fetch-scheme"
  (it "takes Canvas's default scheme when the course names none but shows letters"
    (expect (org-canvas--gradebook-fetch-scheme
             '((grading_standard_id . :null))
             '(((grades . ((current_grade . "B+"))))))
            :to-be org-canvas--gradebook-default-scheme)
    (expect (org-canvas--gradebook-fetch-scheme '((grading_standard_id . 0))
                                                '(((grades . ((current_score . 80))))))
            :to-be nil))

  (it "leaves the letters out with a warning when the standard is refused"
    (with-org-canvas-test-config
      (let ((test-gradebook--standard 'refuse))
        (cl-letf (((symbol-function 'org-canvas-api-request) #'test-gradebook--request))
          (spy-on 'org-canvas--log-warning)
          (expect (org-canvas--gradebook-fetch-scheme '((grading_standard_id . 5)) nil)
                  :to-be nil)
          (expect (spy-calls-count 'org-canvas--log-warning) :to-equal 1))))))

(provide 'org-canvas-gradebook-scores-test)
;;; org-canvas-gradebook-scores-test.el ends here
