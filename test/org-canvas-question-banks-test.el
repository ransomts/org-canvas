;;; org-canvas-question-banks-test.el --- Buttercup tests for the question banks pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-question-banks': the course's classic question
;; banks and their questions, read-only, in question-banks.org (issue
;; #492).  Every request is answered by a fake keyed on the URL; nothing
;; here reaches the network (Hard Rule 2), and the log is never read
;; from the shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: the tier list lives in org-canvas.el.
(require 'org-canvas)

(defvar test-question-banks--banks nil
  "Banks the fake API lists for the course, or `refuse'.")
(defvar test-question-banks--questions nil
  "Alist of bank id to the questions the fake API lists, or `refuse'.")
(defvar test-question-banks--single nil
  "Alist of bank id to the bank a read by id answers, or `refuse'.")
(defvar test-question-banks--requests nil
  "Each (URL . PARAMS) the fake API was asked, newest first.")

(defun test-question-banks--refuse ()
  "Signal the 403 Canvas answers a refused read with."
  (signal 'org-canvas-permission-error (list "403 Forbidden")))

(defun test-question-banks--list (_method url &optional params)
  "Answer the paginated URL with PARAMS from the fake tables, as vectors."
  (push (cons url params) test-question-banks--requests)
  (cond
   ((string-match "/api/v1/question_banks\\'" url)
    (if (eq test-question-banks--banks 'refuse)
        (test-question-banks--refuse)
      (vconcat test-question-banks--banks)))
   ((string-match "/api/v1/question_banks/\\([0-9]+\\)/questions\\'" url)
    (let ((qs (alist-get (string-to-number (match-string 1 url))
                         test-question-banks--questions)))
      (if (eq qs 'refuse) (test-question-banks--refuse) (vconcat qs))))
   (t (error "Unexpected paginated request: %s" url))))

(defun test-question-banks--one (_method url &rest args)
  "Answer the single-object URL, with ARGS, from the fake tables."
  (push (cons url (plist-get args :params)) test-question-banks--requests)
  (cond
   ((string-match "/api/v1/question_banks/\\([0-9]+\\)\\'" url)
    (let ((bank (alist-get (string-to-number (match-string 1 url))
                           test-question-banks--single)))
      (if (or (null bank) (eq bank 'refuse)) (test-question-banks--refuse) bank)))
   (t (error "Unexpected request: %s" url))))

(defun test-question-banks--bank (id title &rest extra)
  "A bank ID titled TITLE in the test course, plus EXTRA pairs."
  (append extra `((id . ,id) (title . ,title) (workflow_state . "active")
                  (context_type . "Course")
                  (context_code . ,(format "course_%s" test-org-canvas-course-id)))))

(defun test-question-banks--question (id name text &rest answers)
  "A multiple-choice bank question ID named NAME with TEXT and ANSWERS.
Each answer is (TEXT . WEIGHT)."
  `((id . ,id) (assessment_question_bank_id . 1) (question_name . ,name)
    (question_type . "multiple_choice_question") (question_text . ,text)
    (points_possible . 1)
    (answers . ,(vconcat (mapcar (lambda (a) `((text . ,(car a)) (weight . ,(cdr a))))
                                 answers)))))

(defmacro test-question-banks--with-course (banks questions &rest body)
  "Run BODY with the fake API serving BANKS and QUESTIONS.
The banks and quizzes files live in `with-test-course''s directory,
DIR, which BODY may use."
  (declare (indent 2))
  `(with-test-course (dir :files '(("question-banks.org" . nil)
                                   ("quizzes.org" . nil)))
     (let ((test-question-banks--banks ,banks)
           (test-question-banks--questions ,questions)
           (test-question-banks--single nil)
           (test-question-banks--requests nil))
       (with-org-canvas-test-config
         (with-html-to-org-identity
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                      #'test-question-banks--list)
                     ((symbol-function 'org-canvas-api-request) #'test-question-banks--one)
                     ((symbol-function 'org-canvas--log-warning) #'ignore)
                     ((symbol-function 'message) #'ignore))
             ,@body))))))

(defun test-question-banks--file ()
  "Return question-banks.org's text."
  (with-temp-buffer (insert-file-contents org-canvas-question-banks-file) (buffer-string)))

(defconst test-question-banks--quizzes-org
  (concat "* Module 7 Quiz\n:PROPERTIES:\n:CANVAS_ID: 682646\n:END:\n"
          "** Module 7 Quiz\n:PROPERTIES:\n:CANVAS_ID: 531944\n:TYPE: group\n"
          ":PICK_COUNT: 10\n:QUESTION_POINTS: 3.0\n:QUESTION_BANK_ID: 572313\n:END:\n"
          "* Final [Draft]\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n"
          "** Pool\n:PROPERTIES:\n:TYPE: group\n:QUESTION_BANK_ID: 900\n:END:\n"
          "** Q1\n:PROPERTIES:\n:TYPE: essay_question\n:END:\n")
  "A quizzes.org whose groups draw from bank 572313 and from bank 900.")

(describe "org-canvas--question-banks-list-params"
  (it "names the course as the context and asks for the question count"
    (with-org-canvas-test-config
      (expect (org-canvas--question-banks-list-params)
              :to-equal `(("context_type" . "Course")
                          ("context_id" . ,(format "%s" test-org-canvas-course-id))
                          ("include_question_count" . "true"))))))

(describe "org-canvas--question-banks-url"
  (it "is the global question banks endpoint, a trailing slash trimmed"
    (let ((org-canvas-base-url "https://canvas.example.edu/"))
      (expect (org-canvas--question-banks-url)
              :to-equal "https://canvas.example.edu/api/v1/question_banks")
      (expect (org-canvas--question-banks-url "/%s/questions" 42)
              :to-equal "https://canvas.example.edu/api/v1/question_banks/42/questions"))))

(describe "org-canvas--question-banks-uses"
  (it "is nil when quizzes.org does not exist"
    (let ((org-canvas-quizzes-file "/tmp/nonexistent/quizzes.org"))
      (expect (org-canvas--question-banks-uses) :to-be nil)))

  (it "reads each group's bank, quiz, name and pick count from quizzes.org"
    (test-question-banks--with-course nil nil
      (with-temp-file org-canvas-quizzes-file (insert test-question-banks--quizzes-org))
      (let ((uses (org-canvas--question-banks-uses)))
        (expect (mapcar #'car uses) :to-equal '("572313" "900"))
        (expect (cdr (car uses))
                :to-equal '(:quiz "Module 7 Quiz" :group "Module 7 Quiz" :pick "10"))
        (expect (plist-get (cdr (cadr uses)) :pick) :to-be nil)))))

(describe "org-canvas--question-banks-count"
  (it "prefers Canvas's count, then the questions read, else nothing"
    (expect (org-canvas--question-banks-count
             (list :bank '((assessment_question_count . 15)) :questions nil))
            :to-equal 15)
    (expect (org-canvas--question-banks-count
             (list :bank '((assessment_question_count . :null)) :questions '(a b)))
            :to-equal 2)
    (expect (org-canvas--question-banks-count (list :bank nil :questions 'unknown))
            :to-be nil)))

(describe "org-canvas-pull-question-banks"
  (it "writes one heading per bank with its properties and its questions at level 2"
    (test-question-banks--with-course
        (list (test-question-banks--bank 572313 "Module 7 Bank" '(assessment_question_count . 2))
              (test-question-banks--bank 12 "Unfiled Questions" '(assessment_question_count . 0)))
        (list (cons 572313 (list (test-question-banks--question
                                  51 "Primes" "<p>Which is not prime?</p>"
                                  '("9" . 100) '("7" . 0))
                                 (test-question-banks--question
                                  52 "Question" "<p>Pick one.</p>" '("A" . 100))))
              (cons 12 nil))
      (org-canvas-pull-question-banks)
      (let ((text (test-question-banks--file)))
        (expect text :to-match "^#\\+TITLE: Question Banks\n")
        (expect text :to-match "^#\\+LAST_SYNCED:")
        (expect text :to-match "^\\* Module 7 Bank\n:PROPERTIES:\n:BANK_ID: 572313\n:QUESTION_COUNT: 2\n:END:\n")
        (expect text :to-match "^\\* Unfiled Questions\n:PROPERTIES:\n:BANK_ID: 12\n:QUESTION_COUNT: 0\n:END:\n")
        (expect text :not :to-match "CONTEXT_CODE")
        (expect text :to-match "^\\*\\* Primes\n")
        (expect text :to-match ":TYPE: +multiple_choice_question")
        (expect text :to-match ":POINTS: +1\n")
        (expect text :to-match "Which is not prime\\?")
        (expect text :to-match "^- \\[X\\] 9$")
        (expect text :to-match "^- \\[ \\] 7$")
        (expect text :to-match "^\\*\\* Question\n")
        (expect (string-match "\\*\\* Primes" text)
                :to-be-less-than (string-match "\\* Unfiled Questions" text)))))

  (it "lists the banks with the course as context, asking for the count"
    (test-question-banks--with-course
        (list (test-question-banks--bank 1 "Bank")) nil
      (org-canvas-pull-question-banks)
      (let ((list-request (cl-find-if (lambda (r) (string-match-p "question_banks\\'" (car r)))
                                      test-question-banks--requests)))
        (expect (cdr list-request) :to-equal (org-canvas--question-banks-list-params)))))

  (it "names the quiz groups that draw from a bank, linked to their quiz"
    (test-question-banks--with-course
        (list (test-question-banks--bank 572313 "Module 7 Bank")) nil
      (with-temp-file org-canvas-quizzes-file (insert test-question-banks--quizzes-org))
      (setq test-question-banks--single
            `((900 . ,(test-question-banks--bank 900 "Shared Pool"
                                                 '(context_code . "account_1")))))
      (org-canvas-pull-question-banks)
      (let ((text (test-question-banks--file)))
        (expect text :to-match "^Drawn from by:\n- \\[\\[file:quizzes.org::\\*Module 7 Quiz\\]\\[Module 7 Quiz\\]\\]: group Module 7 Quiz, pick 10$")
        ;; Org escapes the brackets of a heading the link names.
        (expect text :to-match "^- \\[\\[file:quizzes.org::\\*Final \\\\\\[Draft\\\\\\]\\]\\[Final .*\\]\\]: group Pool$"))))

  (it "reads by id a bank a group names that the course does not list"
    (test-question-banks--with-course
        (list (test-question-banks--bank 572313 "Module 7 Bank"))
        (list (cons 900 (list (test-question-banks--question 90 "Shared" "<p>S</p>" '("Y" . 100)))))
      (with-temp-file org-canvas-quizzes-file (insert test-question-banks--quizzes-org))
      (setq test-question-banks--single
            `((900 . ,(test-question-banks--bank 900 "Shared Pool"
                                                 '(context_code . "account_1")
                                                 '(assessment_question_count . 1)))))
      (org-canvas-pull-question-banks)
      (let ((text (test-question-banks--file))
            (item (cl-find-if (lambda (r) (string-match-p "question_banks/900\\'" (car r)))
                              test-question-banks--requests)))
        (expect (cdr item) :to-equal org-canvas--question-banks-item-params)
        (expect text :to-match "^\\* Shared Pool\n:PROPERTIES:\n:BANK_ID: 900\n:QUESTION_COUNT: 1\n:CONTEXT_CODE: account_1\n:END:\n")
        (expect text :to-match "^\\*\\* Shared\n")
        (expect (string-match "\\* Module 7 Bank" text)
                :to-be-less-than (string-match "\\* Shared Pool" text)))))

  (it "counts a named bank Canvas refuses in the closing line and writes the rest"
    (test-question-banks--with-course
        (list (test-question-banks--bank 572313 "Module 7 Bank" '(assessment_question_count . 0)))
        nil
      (with-temp-file org-canvas-quizzes-file (insert test-question-banks--quizzes-org))
      (setq test-question-banks--single '((900 . refuse)))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (org-canvas-pull-question-banks))
        (expect (car messages)
                :to-equal "Question banks pull complete: 1 banks, 0 questions; 1 named by a quiz group could not be read."))
      (expect (test-question-banks--file) :not :to-match "BANK_ID: 900")))

  (it "keeps a bank whose questions are refused, saying so, never as an empty bank"
    (test-question-banks--with-course
        (list (test-question-banks--bank 5 "Locked"))
        (list (cons 5 'refuse))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (org-canvas-pull-question-banks))
        (expect (car messages)
                :to-equal "Question banks pull complete: 1 banks, 0 questions; 1 with questions Canvas did not list."))
      (let ((text (test-question-banks--file)))
        (expect text :to-match "^\\* Locked\n:PROPERTIES:\n:BANK_ID: 5\n:END:\n")
        (expect text :to-match "^Canvas did not list this bank's questions\\.$"))))

  (it "warns once for each refused read"
    (test-question-banks--with-course
        (list (test-question-banks--bank 5 "Locked"))
        (list (cons 5 'refuse))
      (with-temp-file org-canvas-quizzes-file (insert test-question-banks--quizzes-org))
      (setq test-question-banks--single '((572313 . refuse) (900 . refuse)))
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_logger fmt &rest args) (push (apply #'format fmt args) warned))))
          (org-canvas-pull-question-banks))
        (expect (length warned) :to-equal 3)
        (expect (cl-count-if (lambda (w) (string-match-p "Could not read question bank" w)) warned)
                :to-equal 2)
        (expect (cl-find-if (lambda (w) (string-match-p "questions of bank 5" w)) warned)
                :to-be-truthy))))

  (it "rewrites the whole file on a re-pull and keeps nothing written by hand"
    (test-question-banks--with-course
        (list (test-question-banks--bank 1 "Bank"))
        (list (cons 1 (list (test-question-banks--question 11 "Q" "<p>T</p>" '("A" . 100)))))
      (org-canvas-pull-question-banks)
      (let ((first (test-question-banks--file)))
        (with-current-buffer (org-canvas--find-file-noselect org-canvas-question-banks-file)
          (goto-char (point-max))
          (insert "* Hand written\n")
          (save-buffer))
        (org-canvas-pull-question-banks)
        (let ((second (test-question-banks--file)))
          (expect second :not :to-match "Hand written")
          (expect (replace-regexp-in-string "#\\+LAST_SYNCED:.*" "" second)
                  :to-equal (replace-regexp-in-string "#\\+LAST_SYNCED:.*" "" first))))))

  (it "writes the empty-file note when the course has no bank"
    (test-question-banks--with-course nil nil
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (org-canvas-pull-question-banks))
        (expect (car messages) :to-equal "Question banks pull complete: 0 banks, 0 questions."))
      (expect (file-exists-p org-canvas-question-banks-file) :to-be-truthy)))

  (it "signals when Canvas refuses the course's bank list"
    (test-question-banks--with-course 'refuse nil
      (expect (org-canvas-pull-question-banks) :to-throw 'org-canvas-permission-error)))

  (it "names a bank with no title by its id"
    (test-question-banks--with-course
        (list '((id . 3) (title . :null))) nil
      (org-canvas-pull-question-banks)
      (expect (test-question-banks--file) :to-match "^\\* Bank 3\n"))))

(describe "question banks wiring"
  (it "runs after the quizzes pull, is bound in the pull menu and opens the bank's page"
    (let ((names (mapcar #'car (apply #'append org-canvas--pull-tiers))))
      (expect (cl-position 'org-canvas-pull-quizzes names)
              :to-be-less-than (cl-position 'org-canvas-pull-question-banks names)))
    (let ((source (with-temp-buffer
                    (insert-file-contents (locate-library "org-canvas-transient.el"))
                    (buffer-string))))
      (expect source :to-match "(\"B\" \"Question banks\" org-canvas-pull-question-banks)"))
    (let* ((entry (cl-find "Question Banks" org-canvas--web-page-registry
                           :key (lambda (e) (plist-get e :name)) :test #'equal))
           (rule (car (plist-get entry :web-pages))))
      (expect (plist-get rule :path) :to-equal "question_banks/%s")
      (expect (plist-get rule :id-property) :to-equal "BANK_ID"))))

(provide 'org-canvas-question-banks-test)
;;; org-canvas-question-banks-test.el ends here
