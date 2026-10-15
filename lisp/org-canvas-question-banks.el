;;; org-canvas-question-banks.el --- Pull a course's classic question banks -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file pulls the classic question banks of the course into
;; question-banks.org, read-only: one level-1 heading per bank, its
;; questions beneath it at level 2 in the format quizzes.org uses, so
;; a bank's answer key reads as a quiz's does (issue #492).  A quiz
;; group that draws from a bank (QUESTION_BANK_ID in quizzes.org)
;; names no question of the quiz, so without this file the questions
;; students are drawn from were visible only in the web UI.
;;
;; It is pull-only and the file is derived: every pull rewrites it
;; whole.  Canvas has no API to create or edit a bank; a bank is made
;; in the web UI or by a QTI import, so nothing here is ever pushed.
;;
;; FILE STRUCTURE
;; ==============
;; In question-banks.org, one level-1 heading per bank, titled by the
;; bank's title:
;;   :BANK_ID:  :QUESTION_COUNT:  and :CONTEXT_CODE: for a bank that
;;   belongs to another course or the account
;; then a list of the quizzes.org groups that draw from the bank, each
;; linked to its quiz heading, and the bank's questions at level 2
;; (TYPE, POINTS, the text and the answers, as
;; `org-canvas--quiz-pull-insert-question' writes them).
;;
;; Which banks: every bank the course lists, then any bank a group in
;; quizzes.org names that the list lacks (an account bank, another
;; course's), read by id.  A bank whose questions Canvas will not list
;; keeps its heading with a line saying so, never an empty one (Hard
;; Rule 16).
;;
;; API NOTES
;; =========
;;   GET /api/v1/question_banks?context_type=Course&context_id=:id
;;       the course's banks; the course-scoped
;;       /courses/:id/question_banks is a 404.  Paginated.
;;   GET /api/v1/question_banks/:id
;;       one bank, for a bank another context owns.
;;   GET /api/v1/question_banks/:id/questions
;;       the bank's questions, the quiz question shape.  Paginated.
;; None of them is in the vendored OpenAPI spec; their parameters are
;; transcribed into the contract fixture from Canvas's published API
;; documentation (test/contract/README.md).

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-quizzes)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-question-banks-file (org-canvas--path "question-banks.org")
  "Path to the question-banks.org file.
It holds the course's classic question banks and their questions,
answer keys included, read-only."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-question-banks-file "question-banks.org")

(org-canvas-register-web-pages
 "Question Banks" 'org-canvas-question-banks-file
 '(:level 1 :id-property "BANK_ID" :path "question_banks/%s"))

(org-canvas-register-properties "question-banks"
  :label "Question Banks"
  :file-var 'org-canvas-question-banks-file
  :query "LEVEL=1"
  :properties
  '((:org-prop "BANK_ID" :data-key :bank_id :type number :pull-only t
     :doc "Canvas id of the question bank, as a group's QUESTION_BANK_ID")
    (:org-prop "QUESTION_COUNT" :data-key :question_count :type number
     :pull-only t
     :doc "How many questions the bank holds")
    (:org-prop "CONTEXT_CODE" :data-key :context_code :type string
     :pull-only t
     :doc "Owner of a bank outside this course, as account_1")))

(defconst org-canvas--question-banks-item-params
  '(("include_question_count" . "true"))
  "Query parameters of the read of one question bank.")

(defun org-canvas--question-banks-list-params ()
  "Return the query parameters that list the course's question banks.
A function, since they name `org-canvas-course-id', which is not known
when the module loads."
  `(("context_type" . "Course")
    ("context_id" . ,(format "%s" org-canvas-course-id))
    ("include_question_count" . "true")))

(defun org-canvas--question-banks-url (&optional suffix &rest args)
  "Return the question banks URL, with SUFFIX formatted with ARGS appended.
The banks live at the global /api/v1/question_banks, not under the
course."
  (concat (replace-regexp-in-string "/+\\'" "" org-canvas-base-url)
          "/api/v1/question_banks"
          (if suffix (apply #'format suffix args) "")))

;;;; Fetching

(defun org-canvas--question-banks-fetch-banks ()
  "Return the course's question banks as a list of alists.
A refusal signals: with no list there is nothing to write, and a full
pull counts the refusal as a skip."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas--question-banks-url)
           (org-canvas--question-banks-list-params))
          nil))

(defun org-canvas--question-banks-fetch-bank (bank-id)
  "Return question bank BANK-ID as an alist, or nil when Canvas refuses it."
  (condition-case err
      (org-canvas-api-request
       'GET (org-canvas--question-banks-url "/%s" bank-id)
       :params org-canvas--question-banks-item-params)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Question Banks] Could not read question bank %s (%s); skipped"
       bank-id (error-message-string err))
     nil)))

(defun org-canvas--question-banks-fetch-questions (bank-id)
  "Return the questions of bank BANK-ID as a list of alists, or `unknown'.
A refused list is `unknown', never empty: the bank keeps its heading
with a line saying its questions could not be read (Hard Rule 16)."
  (condition-case err
      (append (org-canvas-api-request-all-pages
               'GET (org-canvas--question-banks-url "/%s/questions" bank-id))
              nil)
    (org-canvas-api-error
     (org-canvas--log-warning org-canvas--logger
       "[Question Banks] Could not list the questions of bank %s (%s)"
       bank-id (error-message-string err))
     'unknown)))

;;;; Quiz Groups That Draw From a Bank

(defun org-canvas--question-banks-group-use ()
  "Return (BANK-ID . USE) for the quizzes.org group heading at point.
USE is a plist of :quiz, the parent quiz's heading, :group, the
group's, and :pick, its PICK_COUNT or nil."
  (let ((bank (org-entry-get (point) "QUESTION_BANK_ID"))
        (group (org-get-heading t t t t))
        (pick (org-entry-get (point) "PICK_COUNT"))
        (quiz (save-excursion
                (and (org-up-heading-safe) (org-get-heading t t t t)))))
    (cons (string-trim bank) (list :quiz quiz :group group :pick pick))))

(defun org-canvas--question-banks-uses ()
  "Return the quizzes.org groups that name a bank, as (BANK-ID . USE).
In file order; see `org-canvas--question-banks-group-use' for USE.
Nil when `org-canvas-quizzes-file' is unset or missing.  The file is
read as the quiz pull left it; nothing is asked of Canvas."
  (let ((file (and (boundp 'org-canvas-quizzes-file) org-canvas-quizzes-file)))
    (when (and file (file-exists-p file))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (org-map-entries #'org-canvas--question-banks-group-use
                           "LEVEL=2+QUESTION_BANK_ID={.}" 'file))))))

(defun org-canvas--question-banks-id (bank)
  "Return BANK's id as a string."
  (format "%s" (alist-get 'id bank)))

(defun org-canvas--question-banks-unlisted (banks group-uses)
  "Return the bank ids GROUP-USES name that BANKS lack, each once, in order."
  (let ((listed (mapcar #'org-canvas--question-banks-id banks))
        (missing nil))
    (dolist (use group-uses)
      (unless (or (member (car use) listed) (member (car use) missing))
        (push (car use) missing)))
    (nreverse missing)))

;;;; Collecting

(defun org-canvas--question-banks-entry (bank group-uses)
  "Return the entry plist for BANK, its questions fetched, from GROUP-USES.
Keys: :bank, the alist; :questions, a list or `unknown'; :uses, the
USE plists of the groups that name the bank."
  (let ((id (org-canvas--question-banks-id bank)))
    (list :bank bank
          :questions (org-canvas--question-banks-fetch-questions id)
          :uses (mapcar #'cdr (cl-remove-if-not
                               (lambda (use) (equal (car use) id)) group-uses)))))

(defun org-canvas--question-banks-collect ()
  "Fetch every bank to write and return (ENTRIES . REFUSED).
ENTRIES has one `org-canvas--question-banks-entry' per bank: the
course's listed banks, then those a quiz group names that the list
lacks.  REFUSED counts the named banks Canvas would not read."
  (let* ((uses (org-canvas--question-banks-uses))
         (banks (org-canvas--question-banks-fetch-banks))
         (refused 0))
    (dolist (id (org-canvas--question-banks-unlisted banks uses))
      (let ((bank (org-canvas--question-banks-fetch-bank id)))
        (if bank
            (setq banks (append banks (list bank)))
          (setq refused (1+ refused)))))
    (cons (mapcar (lambda (bank) (org-canvas--question-banks-entry bank uses))
                  banks)
          refused)))

;;;; Rendering

(defun org-canvas--question-banks-count (entry)
  "Return ENTRY's question count: Canvas's, else the questions read, else nil."
  (let ((count (org-canvas--alist-get-non-null
                'assessment_question_count (plist-get entry :bank)))
        (questions (plist-get entry :questions)))
    (cond (count count)
          ((listp questions) (length questions)))))

(defun org-canvas--question-banks-properties (entry)
  "Return ENTRY's drawer as (PROPERTY . VALUE) pairs, nil values dropped.
CONTEXT_CODE is written only for a bank this course does not own."
  (let* ((bank (plist-get entry :bank))
         (context (org-canvas--alist-get-non-null 'context_code bank))
         (own (format "course_%s" org-canvas-course-id)))
    (cl-remove-if-not
     #'cdr
     `(("BANK_ID" . ,(org-canvas--question-banks-id bank))
       ("QUESTION_COUNT" . ,(org-canvas--question-banks-count entry))
       ("CONTEXT_CODE" . ,(and context (not (equal context own)) context))))))

(defun org-canvas--question-banks-use-line (use)
  "Return the list item naming USE, a quiz group drawing from the bank.
The group is a level-2 heading, so its quiz is always there to link."
  (let* ((quiz (plist-get use :quiz))
         (link (org-link-make-string
                (format "file:%s::*%s"
                        (file-name-nondirectory org-canvas-quizzes-file)
                        (replace-regexp-in-string "\\\\\\([][]\\)" "\\1" quiz))
                quiz))
         (pick (plist-get use :pick)))
    (format "- %s: group %s%s\n" link (plist-get use :group)
            (if pick (format ", pick %s" pick) ""))))

(defun org-canvas--question-banks-insert-head (entry)
  "Insert ENTRY's heading, drawer and the groups drawing from it at point."
  (insert (format "* %s\n:PROPERTIES:\n"
                  (or (org-canvas--alist-get-non-null 'title (plist-get entry :bank))
                      (format "Bank %s" (org-canvas--question-banks-id
                                         (plist-get entry :bank))))))
  (dolist (prop (org-canvas--question-banks-properties entry))
    (insert (format ":%s: %s\n" (car prop) (cdr prop))))
  (insert ":END:\n")
  (when (plist-get entry :uses)
    (insert "\nDrawn from by:\n")
    (dolist (use (plist-get entry :uses))
      (insert (org-canvas--question-banks-use-line use))))
  (when (eq (plist-get entry :questions) 'unknown)
    (insert "\nCanvas did not list this bank's questions.\n")))

(defun org-canvas--question-banks-insert-entry (entry)
  "Insert ENTRY at the end of the buffer: its heading, then its questions.
Questions with no name of their own, or one another question of the
bank shares, are named by their place, as a quiz pull names them."
  (goto-char (point-max))
  (let ((pos (point))
        (questions (plist-get entry :questions)))
    (org-canvas--question-banks-insert-head entry)
    (when (listp questions)
      (dolist (q (org-canvas--quiz-pull-name-questions questions))
        (goto-char pos)
        (org-canvas--quiz-pull-insert-question q)))
    ;; The heading and every question end in a newline; one more
    ;; parts this bank from the next.
    (goto-char (point-max))
    (insert "\n")))

(defun org-canvas--question-banks-write (file entries)
  "Rewrite FILE whole with one heading per entry of ENTRIES."
  (org-canvas--create-file file)
  (with-current-buffer (org-canvas--find-file-noselect file)
    (erase-buffer)
    (insert (format "#+TITLE: %s\n\n"
                    (org-canvas--pull-label-for "question-banks")))
    (dolist (entry entries)
      (org-canvas--question-banks-insert-entry entry))
    (org-canvas--pull-write-file-header)
    (org-canvas--save-buffer)))

;;;; Pull

(defun org-canvas--question-banks-report (entries refused)
  "Log and show the closing line for ENTRIES written and REFUSED banks."
  (let* ((unread (cl-count-if (lambda (e) (eq (plist-get e :questions) 'unknown))
                              entries))
         (questions (apply #'+ (mapcar (lambda (e)
                                         (let ((qs (plist-get e :questions)))
                                           (if (listp qs) (length qs) 0)))
                                       entries)))
         (line (format "Question banks pull complete: %d banks, %d questions%s%s"
                       (length entries) questions
                       (if (> unread 0)
                           (format "; %d with questions Canvas did not list" unread)
                         "")
                       (if (> refused 0)
                           (format "; %d named by a quiz group could not be read"
                                   refused)
                         ""))))
    (org-canvas--log-info org-canvas--logger "%s" line)
    (message "%s." line)))

;;;###autoload
(defun org-canvas-pull-question-banks ()
  "Pull the course's classic question banks into question-banks.org.
One heading per bank with its id and question count, the quizzes.org
groups that draw from it, and its questions beneath it in the format
quizzes.org uses, answer keys included.  A bank a quiz group names
that the course does not own is read by id.  Read-only, and the file
is derived: every pull rewrites it whole.  Pull quizzes first, so the
groups that draw from each bank are current."
  (interactive)
  (org-canvas--start-operation "PULLING QUESTION BANKS")
  (let* ((file (expand-file-name org-canvas-question-banks-file))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "question-banks")
    (let* ((collected (org-canvas--question-banks-collect))
           (entries (car collected)))
      (if (null entries)
          (org-canvas--pull-emit-empty-file
           file (org-canvas--pull-label-for "question-banks"))
        (org-canvas--question-banks-write file entries)
        (org-canvas--pull-kill-fresh-buffer file was-fresh))
      (org-canvas--question-banks-report entries (cdr collected)))))

(provide 'org-canvas-question-banks)
;;; org-canvas-question-banks.el ends here
