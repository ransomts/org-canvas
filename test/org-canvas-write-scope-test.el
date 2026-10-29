;;; org-canvas-write-scope-test.el --- Specs for the write allowlist (issue #550)  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Covers `org-canvas-write-allowlist': the zero-entry case (nothing
;; is scoped, yesterday's behavior), the allow case, the refusal
;; message (it names the file and where to grant it), the sync layer's
;; gates, `org-canvas-sync' skipping a refused file with a warning
;; instead of aborting, and grading's separation from content writes
;; through the "submissions" word.  Every fixture is this file's own,
;; since CI runs each spec file alone.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-announcements)

;;;; Fixtures

(defmacro test-write-scope--with-course-file (filename content &rest body)
  "Visit a scratch file named FILENAME holding CONTENT, then run BODY.
The buffer's file is FILENAME, so a spec can name it in an allowlist."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "ws-course-" t))
          (file (expand-file-name ,filename dir))
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (setq buf (find-file-noselect file))
           (with-current-buffer buf
             (goto-char (point-min))
             ,@body))
       (when (buffer-live-p buf) (kill-buffer buf))
       (delete-directory dir t))))

(defmacro test-write-scope--with-grading-file (content &rest body)
  "Visit a scratch grading file holding CONTENT, in submissions mode, run BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "ws-grades-" t))
          (file (expand-file-name "HW.org" dir))
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (setq buf (find-file-noselect file))
           (with-current-buffer buf
             (org-mode)
             (org-canvas-submissions-mode 1)
             (cl-letf (((symbol-function 'org-canvas--graphql-query) #'ignore))
               ,@body)))
       (when (buffer-live-p buf) (kill-buffer buf))
       (delete-directory dir t))))

(defun test-write-scope--refusal (form)
  "Run FORM; return the message of the write-scope error it signals."
  (let ((err (should-error (funcall form) :type 'org-canvas-write-scope-error)))
    (cadr err)))

;; Dynamically bound around the orphan specs, whose fabricated registry
;; entries name it as their :file-var: a lexical `let' is invisible to
;; `boundp' and `symbol-value', which `org-canvas--feature-file' uses.
(defvar test-write-scope--pages-file nil
  "The file the orphan specs' fabricated Pages entry claims.")

(defconst test-write-scope--grading-header
  "#+TITLE: Submissions: HW
#+PROPERTY: CANVAS_ASSIGNMENT_ID 1001
#+PROPERTY: CANVAS_ASSIGNMENT_NAME HW

")

;;;; The predicate

(describe "org-canvas--write-scope-refused-p"
  (it "scopes nothing when the allowlist is empty"
    (let ((org-canvas-write-allowlist nil))
      (expect (org-canvas--write-scope-refused-p "pages.org") :to-be nil)))

  (it "refuses a file the allowlist does not name"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--write-scope-refused-p "pages.org") :to-be t)
      (expect (org-canvas--write-scope-refused-p "assignments.org") :to-be nil)))

  (it "reads a symbol entry as its name"
    (let ((org-canvas-write-allowlist '(submissions)))
      (expect (org-canvas--write-scope-refused-p "submissions") :to-be nil)
      (expect (org-canvas--write-scope-refused-p "assignments.org") :to-be t)))

  (it "refuses a nil name whenever a list is set"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--write-scope-refused-p nil) :to-be t))))

;;;; The two checks

(describe "org-canvas--check-write-scope"
  (it "names the file and where to grant it"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (let ((msg (test-write-scope--refusal
                  (lambda () (org-canvas--check-write-scope
                              "/course/pages.org" "a sync")))))
        (expect msg :to-match "a sync was refused")
        (expect msg :to-match "pages.org is not in it")
        (expect msg :to-match
                "add pages.org to org-canvas-write-allowlist in org-canvas-credentials.el"))))

  (it "allows a file the allowlist names, and checks nothing without one"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--check-write-scope "/course/assignments.org")
              :not :to-throw))
    (let ((org-canvas-write-allowlist nil))
      (expect (org-canvas--check-write-scope "/course/pages.org")
              :not :to-throw)))

  (it "refuses a buffer that visits no file, and only when scoped"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (let ((msg (test-write-scope--refusal
                  (lambda () (org-canvas--check-write-scope nil "a push at point")))))
        (expect msg :to-match "visits no file")))
    (let ((org-canvas-write-allowlist nil))
      (expect (org-canvas--check-write-scope nil) :not :to-throw))))

(describe "org-canvas--check-write-feature"
  (it "names the feature and where to grant it"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (let ((msg (test-write-scope--refusal
                  (lambda () (org-canvas--check-write-feature
                              'submissions "a grade push")))))
        (expect msg :to-match "a grade push was refused")
        (expect msg :to-match "submissions is not in it")
        (expect msg :to-match
                "add submissions to org-canvas-write-allowlist in org-canvas-credentials.el"))))

  (it "allows a feature the allowlist names, and checks nothing without one"
    (let ((org-canvas-write-allowlist '("submissions")))
      (expect (org-canvas--check-write-feature "submissions") :not :to-throw))
    (let ((org-canvas-write-allowlist nil))
      (expect (org-canvas--check-write-feature 'submissions) :not :to-throw))))

;;;; The sync layer

(describe "org-canvas--sync-run-pipeline write scope"
  (it "refuses a file outside the allowlist before anything is read"
    (let ((temp-dir (make-temp-file "ws-pipeline-" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (requests 0))
            (with-temp-file org-file
              (insert "* Entry One\n:PROPERTIES:\n:END:\n"))
            (let ((org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999")
                  (org-canvas-write-allowlist '("assignments.org")))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (&rest _)
                             (setq requests (1+ requests))
                             '((id . 1)))))
                  (expect (org-canvas--sync-run-pipeline
                           (list :feature "test" :file org-file :query "LEVEL=1"
                                 :parse #'org-canvas--announcement-parse-entry
                                 :build #'org-canvas--announcement-build-payload
                                 :push (lambda (data payload &optional ctx)
                                         (org-canvas--push-to-api
                                          data payload :endpoint "test" :ctx ctx))
                                 :finalize (lambda (data response &optional ctx)
                                             (org-canvas--finalize-item
                                              data response :ctx ctx))))
                          :to-throw 'org-canvas-write-scope-error)
                  (expect requests :to-equal 0)))))
        (delete-directory temp-dir t))))

  (it "runs the pipeline for a file the allowlist names, as it does with none"
    (dolist (allowlist (list nil '("pages.org")))
      (let ((temp-dir (make-temp-file "ws-pipeline-" t)))
        (unwind-protect
            (let* ((org-file (expand-file-name "pages.org" temp-dir))
                   (requests 0))
              (with-temp-file org-file
                (insert "* Entry One\n:PROPERTIES:\n:END:\n"))
              (let ((org-canvas-base-url "https://test.example.com")
                    (org-canvas-api-token "test-token")
                    (org-canvas-course-id "99999")
                    (org-canvas-write-allowlist allowlist))
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (&rest _)
                               (setq requests (1+ requests))
                               '((id . 1)))))
                    (org-canvas--sync-run-pipeline
                     (list :feature "test" :file org-file :query "LEVEL=1"
                           :parse #'org-canvas--announcement-parse-entry
                           :build #'org-canvas--announcement-build-payload
                           :push (lambda (data payload &optional ctx)
                                   (org-canvas--push-to-api
                                    data payload :endpoint "test" :ctx ctx))
                           :finalize (lambda (data response &optional ctx)
                                       (org-canvas--finalize-item
                                        data response :ctx ctx))))
                    (expect requests :to-be-truthy)))))
          (delete-directory temp-dir t))))))

(describe "org-canvas--push-at-point-runtime write scope"
  (it "refuses a push at point in a file the allowlist does not name"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
            "* Test Page
:PROPERTIES:
:CANVAS_ID: 99
:END:
"
          (org-back-to-heading t)
          (let ((org-canvas-write-allowlist '("assignments.org")))
            (expect (org-canvas--push-at-point-runtime
                     (list :feature "test"
                           :parse (lambda () (list :title "Test" :canvas-id "99"
                                                   :pom (point)))
                           :build (lambda (_data) '((title . "Test")))
                           :push (lambda (&rest _) '((id . 99)))
                           :finalize (lambda (&rest _) nil)))
                    :to-throw 'org-canvas-write-scope-error))))))

  (it "pushes at point in a file the allowlist names"
    (with-org-canvas-test-config
      (with-mock-api
        (test-write-scope--with-course-file
            "pages.org"
            "* Test Page
:PROPERTIES:
:CANVAS_ID: 99
:END:
"
          (org-back-to-heading t)
          (let ((org-canvas-write-allowlist '("pages.org"))
                (pushed nil))
            (with-sync-test-env
              (org-canvas--push-at-point-runtime
               (list :feature "test"
                     :parse (lambda () (list :title "Test" :canvas-id "99"
                                             :pom (point)))
                     :build (lambda (_data) '((title . "Test")))
                     :push (lambda (&rest _) (setq pushed t) '((id . 99)))
                     :finalize (lambda (&rest _) nil))))
            (expect pushed :to-be t)))))))

;;;; The custom-loop sync commands

(describe "the custom sync commands' write scope"
  (it "refuses the settings sync before the log header"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas-sync-settings)
              :to-throw 'org-canvas-write-scope-error)))

  (it "refuses the accommodations sync before the log header"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas-sync-quiz-accommodations)
              :to-throw 'org-canvas-write-scope-error)))

  (it "refuses the accommodations sync at point in an unnamed file"
    (test-write-scope--with-course-file
        "quizzes.org" "* A Quiz\n:PROPERTIES:\n:END:\n"
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (expect (org-canvas-sync-quiz-accommodations-at-point)
                :to-throw 'org-canvas-write-scope-error))))

  (it "refuses the override sync before the assignments file is read"
    (let ((org-canvas-write-allowlist '("pages.org")))
      (expect (org-canvas-sync-overrides)
              :to-throw 'org-canvas-write-scope-error))))

;;;; The delete layer

(describe "the delete layer's write scope"
  (it "refuses a delete-all for a file outside the allowlist"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--delete-all-runtime
               (list :feature "pages" :endpoint "pages" :file "pages.org"))
              :to-throw 'org-canvas-write-scope-error)))

  (it "refuses a prune for a file outside the allowlist"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--prune-runtime
               (list :feature "pages" :endpoint "pages" :file "pages.org"))
              :to-throw 'org-canvas-write-scope-error)))

  (it "refuses a delete at point in an unnamed file"
    (test-write-scope--with-course-file
        "pages.org" "* A Page\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n"
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (expect (org-canvas--delete-item-at-point "page" :endpoint "pages/%s")
                :to-throw 'org-canvas-write-scope-error))))

  (it "deletes at point when the allowlist names the file"
    (with-org-canvas-test-config
      (test-write-scope--with-course-file
          "pages.org" "* A Page\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n"
        (let ((org-canvas-write-allowlist '("pages.org"))
              (org-canvas-delete-confirmed t))
          (with-sync-test-env
            (with-mock-api
              (expect (org-canvas--delete-item-at-point "page" :endpoint "pages/%s")
                      :to-be 'deleted)
              (expect (test-org-canvas-api-called-p 'DELETE "pages/7")
                      :to-be-truthy))))))))

;;;; Publishing and the drift report

(describe "org-canvas-set-published-at-point write scope"
  (it "refuses publishing a heading whose file is not in the allowlist"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (cl-letf (((symbol-function 'org-canvas--publish-at-point-target)
                 (lambda ()
                   (list :feature (org-canvas--registry-find-feature "Pages")
                         :rules nil :title "T" :id "7" :spec nil))))
        (expect (org-canvas-set-published-at-point t)
                :to-throw 'org-canvas-write-scope-error)))))

(describe "org-canvas--diff-delete-execute write scope"
  (it "refuses a drift-report delete of an unnamed feature's file"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--diff-delete-execute
               (list :feature "Pages") '(:id "7") nil)
              :to-throw 'org-canvas-write-scope-error)))

  (it "refuses a module-item delete by modules.org, which claims it"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--diff-delete-execute
               (list :feature "Module Items")
               (list :id "5787193" :module-id "781715") nil)
              :to-throw 'org-canvas-write-scope-error)))

  (it "refuses an unregistered feature's row by its name"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--diff-delete-execute
               (list :feature "Bogus") '(:id "9") nil)
              :to-throw 'org-canvas-write-scope-error))))

;;;; Orphan cleanup

(describe "org-canvas--orphan-delete-all write scope"
  (it "leaves a refused feature's orphans alone, with a warning"
    (let ((test-write-scope--pages-file "/course/pages.org")
          (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (let ((org-canvas-write-allowlist '("assignments.org")))
          (org-canvas--orphan-delete-all
           (list (cons (list :name "Pages" :file-var 'test-write-scope--pages-file
                             :id-field 'id :title-field 'title
                             :endpoint "pages")
                       '(((id . 5) (title . "Orphan Page")))))))
        (expect (car warnings) :to-match "Left 1 Pages orphan")
        (expect (car warnings) :to-match "allowlist"))))

  (it "deletes a named feature's orphans"
    (with-org-canvas-test-config
      (let ((test-write-scope--pages-file "/course/pages.org"))
        (with-mock-api
          (let ((org-canvas-write-allowlist '("pages.org")))
            (org-canvas--orphan-delete-all
             (list (cons (list :name "Pages" :file-var 'test-write-scope--pages-file
                               :id-field 'id :title-field 'title
                               :endpoint "pages")
                         '(((id . 5) (title . "Orphan Page")))))))
          (expect (test-org-canvas-api-called-p 'DELETE "pages/5")
                  :to-be-truthy))))))

;;;; Sync-all and delete-all: skip, do not abort

(describe "org-canvas--safe-sync write scope"
  (it "skips a refused file with a warning and a skip, not a failure"
    (let* ((org-canvas--sync-global-counters
            (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
           (org-canvas--sync-global-feature-stats nil)
           (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (expect (org-canvas--safe-sync
                 (lambda ()
                   (org-canvas--signal 'org-canvas-write-scope-error "mocked refusal"))
                 "Pages")
                :to-be nil))
      (expect (plist-get org-canvas--sync-global-counters :skip) :to-equal 1)
      (expect (plist-get org-canvas--sync-global-counters :fail) :to-equal 0)
      (expect (car warnings) :to-match "\\[Skip\\] Pages:")
      (expect (car warnings) :to-match "mocked refusal")
      (expect (plist-get (car org-canvas--sync-global-feature-stats) :label)
              :to-equal "Pages"))))

(describe "org-canvas-sync write scope"
  (it "skips a refused file and carries the run on"
    (let ((call-order nil))
      (with-sync-test-env
        (with-mocked-sync-fns
            (:record call-order
             :overrides ((pages . (lambda ()
                                    (org-canvas--signal
                                     'org-canvas-write-scope-error
                                     "mocked refusal")))))
          (let ((counters (org-canvas-sync)))
            (setq call-order (nreverse call-order))
            (expect counters :not :to-throw)
            (expect (member 'pages call-order) :to-be nil)
            (expect (member 'settings call-order) :to-be-truthy)
            (expect (member 'modules call-order) :to-be-truthy)
            (expect (plist-get counters :skip) :to-equal 1))))))

  (it "deletes nothing but completes when every file is refused"
    (let ((warnings nil))
      (with-sync-test-env
        (let ((org-canvas--inhibit-log-clear t))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                     ((symbol-function 'org-canvas--log-warning)
                      (lambda (_logger fmt &rest args)
                        (push (apply #'format fmt args) warnings))))
            (let ((org-canvas-write-allowlist '("nowhere.org")))
              (expect (org-canvas-delete-all) :not :to-throw)))))
      (expect (cl-some (lambda (w) (string-match-p "\\[Skip\\] Pages" w))
                       warnings)
              :to-be-truthy)))

  (it "writes no snapshot when every file is refused (issue #555)"
    (with-sync-test-env
      (with-delete-snapshot-dir
        (let ((org-canvas--inhibit-log-clear t)
              (org-canvas-write-allowlist '("nowhere.org")))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (expect (org-canvas-delete-all) :not :to-throw)))
        ;; Refusal happens before each feature's listing, so no run ever
        ;; reached the snapshot write.
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil)))))

;;;; Grading: the submissions word

(describe "the grading commands' write scope"
  (it "refuses a grade push before the plan, naming submissions"
    (test-write-scope--with-grading-file
        (concat test-write-scope--grading-header
                "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 80\n:END:\n")
      (org-canvas--submissions-ensure-context)
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (let ((msg (test-write-scope--refusal
                    (lambda () (org-canvas--submissions-push-current nil)))))
          (expect msg :to-match "submissions is not in it")))))

  (it "pushes a grading file the allowlist names, with nothing to send"
    (test-write-scope--with-grading-file
        (concat test-write-scope--grading-header
                "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:SCORE: 92\n:CANVAS_SCORE: 92\n:END:\n")
      (org-canvas--submissions-ensure-context)
      (let ((org-canvas-write-allowlist '("submissions"))
            (org-canvas-submissions-check-conflicts nil))
        (expect (plist-get (org-canvas--submissions-push-current nil) :pushed)
                :to-equal 0))))

  (it "refuses a comment-edit push before the pending edits are read"
    (test-write-scope--with-grading-file
        (concat test-write-scope--grading-header
                "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (org-canvas--submissions-ensure-context)
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (expect (org-canvas--submissions-push-comment-edits-current)
                :to-throw 'org-canvas-write-scope-error))))

  (it "refuses posting a comment before the text is asked for"
    (test-write-scope--with-grading-file
        (concat test-write-scope--grading-header
                "* Adams, Alice\n:PROPERTIES:\n:USER_ID: 5001\n:END:\n")
      (org-canvas--submissions-ensure-context)
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (expect (org-canvas-submissions-add-comment)
                :to-throw 'org-canvas-write-scope-error))))

  (it "refuses deleting a saved comment before the question"
    (test-write-scope--with-grading-file
        (concat test-write-scope--grading-header
                "* Comment Bank\n- 12345 :: A saved comment\n")
      (goto-char (point-min))
      (re-search-forward "12345")
      (let ((org-canvas-write-allowlist '("assignments.org")))
        (expect (org-canvas-submissions-delete-comment-bank-item)
                :to-throw 'org-canvas-write-scope-error))))

  (it "refuses posting grades when the allowlist does not name submissions"
    (let ((org-canvas-write-allowlist '("assignments.org")))
      (expect (org-canvas--submissions-post-assignment "1001")
              :to-throw 'org-canvas-write-scope-error)))

  (it "posts grades when the allowlist names submissions"
    (let ((org-canvas-write-allowlist '("submissions"))
          (sent nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-send)
                 (lambda (_document _variables)
                   (setq sent t)
                   '((postAssignmentGrades . t)))))
        (expect (org-canvas--submissions-post-assignment "1001") :to-be t)
        (expect sent :to-be t)))))

;;;; Course activation

(describe "org-canvas-activate-course write allowlist"
  (it "clears the previous course's allowlist when the new credentials set none"
    (let* ((dir (make-temp-file "ws-activate-" t))
           (cred-file (expand-file-name "org-canvas-credentials.el" dir))
           (org-canvas-courses `(("Scoped" . ,dir))))
      (unwind-protect
          (with-org-canvas-course-globals
            (with-temp-file cred-file
              (insert "(setq org-canvas-course-id \"42\")\n"))
            (setq org-canvas-write-allowlist '("assignments.org"))
            (org-canvas-activate-course "Scoped")
            (expect org-canvas-write-allowlist :to-be nil))
        (delete-directory dir t))))

  (it "loads the allowlist a credentials file sets"
    (let* ((dir (make-temp-file "ws-activate-" t))
           (cred-file (expand-file-name "org-canvas-credentials.el" dir))
           (org-canvas-courses `(("Scoped" . ,dir))))
      (unwind-protect
          (with-org-canvas-course-globals
            (with-temp-file cred-file
              (insert "(setq org-canvas-course-id \"42\")\n"
                      "(setq org-canvas-write-allowlist '(\"submissions\"))\n"))
            (setq org-canvas-write-allowlist nil)
            (org-canvas-activate-course "Scoped")
            (expect org-canvas-write-allowlist :to-equal '("submissions")))
        (delete-directory dir t)))))

(provide 'org-canvas-write-scope-test)
;;; org-canvas-write-scope-test.el ends here
