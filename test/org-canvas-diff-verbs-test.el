;;; org-canvas-diff-verbs-test.el --- Drift report verb specs  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; lisp/org-canvas-diff-verbs.el: the verbs that act on a row -- delete,
;; pull, adopt a stamp, stamp a moved item.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(describe "org-canvas-diff-delete (issue #103)"
  (it "deletes the remote object through the feature's URL after confirming, and marks the row"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Files" :extra ((:kind extra :title "dup.png" :id "31505574")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-delete)
          (expect (thing-at-point 'line t) :to-match "DELETED   dup.png (id 31505574)")))
      (let ((deletes (test-org-canvas--diff-deletes)))
        (expect (length deletes) :to-equal 1)
        (expect (nth 1 (car deletes)) :to-match "files/31505574$"))))

  (it "sends the feature's delete body when it declares one"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Calendar Events" :extra ((:kind extra :title "Old" :id "5")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-delete)))
      (let ((delete (car (test-org-canvas--diff-deletes))))
        (expect (nth 1 delete) :to-match "calendar_events/5$")
        (expect (plist-get (nth 2 delete) :data)
                :to-equal '((cancel_reason . "Deleted by org-canvas"))))))

  (it "sends no DELETE and writes no snapshot when the confirmation is declined"
    (test-org-canvas--with-snapshot-dir
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'org-canvas-api-request)
                 #'test-org-canvas--diff-delete-stub))
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Files" :extra ((:kind extra :title "dup.png" :id "1")))))
          (test-org-canvas--diff-goto-row 'extra)
          (org-canvas-diff-delete)
          (expect (thing-at-point 'line t) :to-match "EXTRA     dup.png")))
      (expect (test-org-canvas--diff-deletes) :to-be nil)
      (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil)))

  (it "refuses a row with no remote object of its own to delete"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :divergences ((:kind missing :title "Gone" :id "61")))))
      (test-org-canvas--diff-goto-row 'missing)
      (expect (org-canvas-diff-delete) :to-throw 'user-error))))

(describe "org-canvas-diff-pull (issue #103)"
  (it "runs the single-heading pull on the CHANGED row's heading and marks the row"
    (let ((file (make-temp-file "diff-pull-" nil ".org"))
          (pulled-in nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:END:\n"
                      "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 61\n:END:\n"))
            (let ((org-canvas-assignments-file file))
              (cl-letf (((symbol-function 'org-canvas-pull-at-point)
                         (lambda () (setq pulled-in (cons (buffer-file-name)
                                                          (org-get-heading t t t t))))))
                (with-current-buffer (test-org-canvas--diff-report-buffer
                                      '((:name "Assignments"
                                         :divergences ((:kind modified :title "Lab 2" :id "61"
                                                        :remote-newer t
                                                        :updated "2026-08-25T00:00:00Z")))))
                  (test-org-canvas--diff-goto-row 'modified)
                  (org-canvas-diff-pull)
                  (expect (thing-at-point 'line t) :to-match "PULLED    Lab 2 (id 61)")))
              (expect (car pulled-in) :to-equal (file-truename file))
              (expect (cdr pulled-in) :to-equal "Lab 2")))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "refuses an EXTRA row of a feature that cannot pull into a new heading"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :extra ((:kind extra :title "Surprise" :id "99")))))
      (test-org-canvas--diff-goto-row 'extra)
      (expect (org-canvas-diff-pull) :to-throw 'user-error)))

  (it "refuses a row with no Canvas version to pull"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :divergences ((:kind missing :title "Gone" :id "61")))))
      (test-org-canvas--diff-goto-row 'missing)
      (expect (org-canvas-diff-pull) :to-throw 'user-error)))

  (it "pulls an EXTRA quiz into a new heading at the end of quizzes.org (issue #295)"
    (let ((file (make-temp-file "diff-pull-extra-" nil ".org"))
          (pulled nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Quiz 1\n:PROPERTIES:\n:CANVAS_ID: 5\n:END:\n"))
            (let ((org-canvas-quizzes-file file))
              (cl-letf (((symbol-function 'org-canvas--pull-at-point-1)
                         (lambda (feature id title)
                           (setq pulled (list (plist-get feature :name) id title
                                              (org-entry-get (point) "CANVAS_ID")
                                              (org-get-heading t t t t))))))
                (with-current-buffer (test-org-canvas--diff-report-buffer
                                      '((:name "Quizzes"
                                         :extra ((:kind extra :title "Survey" :id "77")))))
                  (test-org-canvas--diff-goto-row 'extra)
                  (org-canvas-diff-pull)
                  (expect (thing-at-point 'line t)
                          :to-match "PULLED    Survey (id 77, new heading)")))
              (expect pulled :to-equal '("Quizzes" "77" "Survey" "77" "Survey"))
              (with-current-buffer (find-buffer-visiting file)
                (expect (buffer-string) :to-match "^\\* Quiz 1\n\\(?:.*\n\\)*\\* Survey\n"))))
        (let ((buf (find-buffer-visiting file)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
        (delete-file file))))

  (it "sends nothing when the EXTRA pull is declined"
    (let ((file (make-temp-file "diff-pull-extra-no-" nil ".org")))
      (unwind-protect
          (let ((org-canvas-quizzes-file file))
            (cl-letf (((symbol-function 'org-canvas--confirm) (lambda (_) nil))
                      ((symbol-function 'org-canvas--pull-at-point-1)
                       (lambda (&rest _) (error "Declined pull must not fetch"))))
              (with-current-buffer (test-org-canvas--diff-report-buffer
                                    '((:name "Quizzes"
                                       :extra ((:kind extra :title "Survey" :id "77")))))
                (test-org-canvas--diff-goto-row 'extra)
                (org-canvas-diff-pull)
                (expect (thing-at-point 'line t) :to-match "EXTRA     Survey"))))
        (let ((buf (find-buffer-visiting file)))
          (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
        (delete-file file)))))

;;;; Stamp adoption (issue #257)

(defmacro test-org-canvas-257--with-assignments-file (content &rest body)
  "Run BODY with `org-canvas-assignments-file' bound to a file holding CONTENT.
The file is deleted afterwards, its buffer killed."
  (declare (indent 1))
  `(let ((file (make-temp-file "diff-adopt-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (let ((org-canvas-assignments-file file))
             ,@body))
       (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
       (delete-file file))))

(defun test-org-canvas-257--property (file title property)
  "Return PROPERTY of the heading TITLE in FILE, read from disk."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (concat "^\\* " (regexp-quote title) "$"))
    (org-entry-get (point) property)))

(defconst test-org-canvas-257--file
  (concat "* Lab 1\n:PROPERTIES:\n:CANVAS_ID: 60\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:PAYLOAD_HASH: aaa\n:END:\n"
          "* Lab 2\n:PROPERTIES:\n:CANVAS_ID: 61\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:PAYLOAD_HASH: bbb\n:END:\n"
          "* Lab 3\n:PROPERTIES:\n:CANVAS_ID: 62\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:PAYLOAD_HASH: ccc\n:END:\n")
  "Three stamped assignment headings for the adoption specs.")

(describe "org-canvas--diff-adoptable-p (issue #257)"
  (it "accepts a CHANGED row Canvas holds newer with no compared property differing"
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer t :fields nil
               :updated "2026-09-11T20:00:00Z"))
            :to-be-truthy))

  (it "refuses a row where a compared property differs, one Canvas is not newer on, and other kinds"
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer t
               :fields (("POINTS" "10" "12")) :updated "2026-09-11T20:00:00Z"))
            :to-be nil)
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer nil :fields nil
               :updated "2026-09-11T20:00:00Z"))
            :to-be nil)
    (expect (org-canvas--diff-adoptable-p
             '(:kind modified :title "Lab 2" :id "61" :remote-newer t :fields nil))
            :to-be nil)
    (expect (org-canvas--diff-adoptable-p '(:kind missing :title "Lab 2" :id "61"))
            :to-be nil)))

(describe "org-canvas-diff-adopt-stamp (issue #257)"
  (it "writes the Canvas timestamp into CANVAS_UPDATED_AT, keeps PAYLOAD_HASH, sends nothing, and marks the row"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-mock-api
        (with-current-buffer (test-org-canvas--diff-report-buffer
                              '((:name "Assignments"
                                 :divergences ((:kind modified :title "Lab 2" :id "61"
                                                :remote-newer t :fields nil
                                                :updated "2026-09-11T20:00:00Z")))))
          (test-org-canvas--diff-goto-row 'modified)
          (org-canvas-diff-adopt-stamp)
          (expect (thing-at-point 'line t)
                  :to-match "ADOPTED   Lab 2 (CANVAS_UPDATED_AT set to 2026-09-11T20:00:00Z, nothing sent)")
          (expect (get-text-property (point) 'org-canvas-diff-row) :to-be-truthy))
        (expect (test-org-canvas-api-call-count) :to-equal 0))
      (expect (test-org-canvas-257--property file "Lab 2" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T20:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 2" "PAYLOAD_HASH") :to-equal "bbb")
      ;; The neighbours are untouched.
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 3" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")))

  (it "is what a on a CHANGED row does"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-current-buffer (test-org-canvas--diff-report-buffer
                            '((:name "Assignments"
                               :divergences ((:kind modified :title "Lab 3" :id "62"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T21:00:00Z")))))
        (test-org-canvas--diff-goto-row 'modified)
        (org-canvas-diff-acknowledge)
        (expect (thing-at-point 'line t) :to-match "ADOPTED   Lab 3"))
      (expect (test-org-canvas-257--property file "Lab 3" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T21:00:00Z")))

  (it "refuses a CHANGED row where a compared property differs and leaves the file alone"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-current-buffer (test-org-canvas--diff-report-buffer
                            '((:name "Assignments"
                               :divergences ((:kind modified :title "Lab 2" :id "61"
                                              :remote-newer t
                                              :fields (("POINTS" "10" "12"))
                                              :updated "2026-09-11T20:00:00Z")))))
        (test-org-canvas--diff-goto-row 'modified)
        (expect (org-canvas-diff-adopt-stamp) :to-throw 'user-error)
        (expect (org-canvas-diff-acknowledge) :to-throw 'user-error)
        (expect (thing-at-point 'line t) :to-match "CHANGED   Lab 2"))
      (expect (test-org-canvas-257--property file "Lab 2" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")))

  (it "refuses a row that is not CHANGED"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :extra ((:kind extra :title "Surprise" :id "99")))))
      (test-org-canvas--diff-goto-row 'extra)
      (expect (org-canvas-diff-adopt-stamp) :to-throw 'user-error)))

  (it "signals when no heading carries the row's id"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (with-current-buffer (test-org-canvas--diff-report-buffer
                            '((:name "Assignments"
                               :divergences ((:kind modified :title "Ghost" :id "999"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z")))))
        (test-org-canvas--diff-goto-row 'modified)
        (expect (org-canvas-diff-adopt-stamp) :to-throw 'user-error)))))

(describe "org-canvas-diff-adopt-stamps (issue #257)"
  (defun test-org-canvas-257--run (results &optional answer)
    "Run the batch adoption over RESULTS as if every feature reported them.
ANSWER `no' declines the confirmation, which otherwise says yes; the
prompt text and the report text come back as (PROMPT . REPORT)."
    (let ((prompt nil))
      (with-org-canvas-test-config
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                    ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                    ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                    ((symbol-function 'y-or-n-p)
                     (lambda (q) (setq prompt q) (not (eq answer 'no))))
                    ((symbol-function 'org-canvas--diff-feature)
                     (lambda (feature)
                       (or (cl-find (plist-get feature :name) results
                                    :key (lambda (r) (plist-get r :name))
                                    :test #'string=)
                           (list :name (plist-get feature :name))))))
            (org-canvas-diff-adopt-stamps))))
      (cons prompt (with-current-buffer org-canvas--diff-adopt-buffer-name (buffer-string)))))

  (it "adopts every qualifying row after confirming, counts the held-back ones, and sends nothing"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (let ((out nil))
        (with-mock-api
          (setq out (test-org-canvas-257--run
                     '((:name "Assignments"
                        :divergences ((:kind modified :title "Lab 1" :id "60"
                                       :remote-newer t :fields nil
                                       :updated "2026-09-11T20:00:00Z")
                                      (:kind modified :title "Lab 2" :id "61"
                                       :remote-newer t
                                       :fields (("POINTS" "10" "12"))
                                       :updated "2026-09-11T20:00:00Z")
                                      (:kind modified :title "Lab 3" :id "62"
                                       :remote-newer t :fields nil
                                       :updated "2026-09-11T22:00:00Z")
                                      (:kind missing :title "Lab 4" :id "63"))
                        :extra ((:kind extra :title "Surprise" :id "99"))))))
          (expect (test-org-canvas-api-call-count) :to-equal 0))
        (expect (car out) :to-match "Adopt the Canvas timestamp of 2 CHANGED entries")
        (expect (cdr out) :to-match "ADOPTED   Lab 1 (Assignments; CANVAS_UPDATED_AT set to 2026-09-11T20:00:00Z)")
        (expect (cdr out) :to-match "ADOPTED   Lab 3 (Assignments; CANVAS_UPDATED_AT set to 2026-09-11T22:00:00Z)")
        (expect (cdr out) :to-match "2 stamp(s) adopted; PAYLOAD_HASH kept, nothing sent to Canvas")
        (expect (cdr out) :to-match "1 CHANGED row(s) left alone: a compared property differs")
        (expect (cdr out) :not :to-match "Lab 2 (Assignments"))
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T20:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 1" "PAYLOAD_HASH") :to-equal "aaa")
      (expect (test-org-canvas-257--property file "Lab 2" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 3" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T22:00:00Z")
      (expect (test-org-canvas-257--property file "Lab 3" "PAYLOAD_HASH") :to-equal "ccc")))

  (it "returns the count adopted and names a heading it could not find"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (let ((count nil))
        (cl-letf (((symbol-function 'org-canvas--report-display)
                   (lambda (_name render) (with-temp-buffer (funcall render) (buffer-string)))))
          (with-org-canvas-test-config
            (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                      ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                      ((symbol-function 'org-canvas--diff-feature)
                       (lambda (feature)
                         (if (string= (plist-get feature :name) "Assignments")
                             '(:name "Assignments"
                               :divergences ((:kind modified :title "Lab 1" :id "60"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z")
                                             (:kind modified :title "Ghost" :id "999"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z")))
                           (list :name (plist-get feature :name))))))
              ;; Batch: no prompt, `noninteractive' stays t under eldev.
              (setq count (org-canvas-diff-adopt-stamps)))))
        (expect count :to-equal 1))
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-11T20:00:00Z")))

  (it "renders the heading it could not find"
    (with-org-canvas-test-config
      (with-temp-buffer
        (org-canvas--diff-adopt-render
         '(("Assignments" "Lab 1" "2026-09-11T20:00:00Z"))
         '(("Assignments" . "Ghost")) 0)
        (expect (buffer-string) :to-match "NOT FOUND Ghost (Assignments; no heading carries its id)")
        (expect (buffer-string) :to-match "1 stamp(s) adopted")
        (expect (buffer-string) :not :to-match "left alone"))))

  (it "writes nothing and says so when the confirmation is declined"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (expect (test-org-canvas-257--run
               '((:name "Assignments"
                  :divergences ((:kind modified :title "Lab 1" :id "60"
                                 :remote-newer t :fields nil
                                 :updated "2026-09-11T20:00:00Z"))))
               'no)
              :to-throw 'user-error)
      (expect (test-org-canvas-257--property file "Lab 1" "CANVAS_UPDATED_AT")
              :to-equal "2026-09-01T00:00:00Z")))

  (it "asks nothing and reports so when no row qualifies"
    (let ((out (test-org-canvas-257--run
                '((:name "Assignments"
                   :divergences ((:kind modified :title "Lab 2" :id "61"
                                  :remote-newer t :fields (("POINTS" "10" "12"))
                                  :updated "2026-09-11T20:00:00Z")))
                  (:name "Pages" :error "boom")))))
      (expect (car out) :to-be nil)
      (expect (cdr out) :to-match "No stamp to adopt")
      (expect (cdr out) :to-match "1 CHANGED row(s) left alone")))

  (it "skips a row whose feature is not registered, without stopping the rest"
    (test-org-canvas-257--with-assignments-file test-org-canvas-257--file
      (let ((outcome (org-canvas--diff-adopt-all
                      '(("No Such Feature" . (:kind modified :title "X" :id "1"
                                              :remote-newer t :fields nil
                                              :updated "2026-09-11T20:00:00Z"))
                        ("Assignments" . (:kind modified :title "Lab 1" :id "60"
                                          :remote-newer t :fields nil
                                          :updated "2026-09-11T20:00:00Z"))))))
        (expect (car outcome) :to-equal '(("Assignments" "Lab 1" "2026-09-11T20:00:00Z")))
        (expect (cdr outcome) :to-equal '(("No Such Feature" . "X")))))))

(describe "org-canvas-diff-delete-rows (issue #345)"
  (it "deletes module items and a clean assignment, snapshotting each first"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions" . [((user_id . 1) (workflow_state . "unsubmitted"))])
              ("assignments/701" . ((id . 701) (name . "Session 87")))
              ("modules/781715/items/5787192" . ((id . 5787192) (title . "87-surveillance-capitalism")))))
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Module Items" :id "5787192")
                         (:feature "assignments" :id 701))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(deleted deleted))
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas-api-course-endpoint "modules/781715/items/5787192")
                                (org-canvas-api-course-endpoint "assignments/701")))
        ;; Each snapshot holds the object as Canvas returned it.
        (let* ((file (plist-get (car outcomes) :snapshot))
               (json (json-read-file file)))
          (expect (file-name-directory file) :to-equal (file-name-as-directory snapshot-dir))
          (expect (file-name-nondirectory file) :to-match "-module-items-5787192\\.json\\'")
          (expect (alist-get 'title json) :to-equal "87-surveillance-capitalism"))
        (expect (length (directory-files snapshot-dir nil "\\.json\\'")) :to-equal 2))))

  (it "reads an assignment with its own dates, the parameters its item read carries"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions" . [])
              ("assignments/701" . ((id . 701)))))
      (org-canvas-diff-delete-rows '((:feature "Assignments" :id "701"))
                                   :results test-org-canvas--diff-reshuffle-results)
      (let ((read (cl-find-if (lambda (c) (and (eq (car c) 'GET)
                                               (string-match-p "assignments/701\\'" (nth 1 c))))
                              test-org-canvas-api-calls)))
        (expect (plist-get (nth 3 read) :params)
                :to-equal (org-canvas--feature-item-params
                           (org-canvas--registry-find-feature "Assignments"))))))

  (it "refuses an assignment with a submission or a score, naming why"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions"
               . [((user_id . 1) (submitted_at . "2026-09-01T10:00:00Z") (workflow_state . "submitted"))
                  ((user_id . 2) (score . 8.0) (grade . "8") (workflow_state . "graded"))
                  ((user_id . 3) (workflow_state . "unsubmitted"))])
              ("assignments/701" . ((id . 701)))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Assignments" :id "701"))
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason)
                :to-equal "the assignment has 1 submission(s) and 1 score(s)")
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil)
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil))))

  (it "refuses an assignment Canvas flags as submitted to even when the list shows none"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/701/submissions" . [])
              ("assignments/701" . ((id . 701) (has_submitted_submissions . t)))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Assignments" :id "701"))
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason) :to-match "Canvas says"))))

  (it "checks a New Quiz's submissions as its assignment's, deleting at the quiz service (#313)"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("assignments/41/submissions"
               . [((user_id . 1) (submitted_at . "2026-09-01T10:00:00Z") (workflow_state . "submitted"))])
              ("assignments/42/submissions" . [])
              ("quizzes/4[12]" . ((id . "41") (assignment_id . "41") (title . "Midterm")))))
      (let* ((results '((:name "New Quizzes"
                         :extra ((:kind extra :title "Midterm" :id "41")
                                 (:kind extra :title "Final" :id "42")))))
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "New Quizzes" :all t)) :results results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(refused deleted))
        (expect (plist-get (car outcomes) :reason)
                :to-equal "the assignment has 1 submission(s) and 0 score(s)")
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas--new-quiz-api-endpoint "quizzes/42"))))))

  (it "refuses a rubric an assignment grades with, read with its associations"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("rubrics/801" . ((id . 801)
                                (associations . [((association_type . "Course") (association_id . 1))
                                                 ((association_type . "Assignment") (association_id . 701))])))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Rubrics" :id "801"))
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason) :to-equal "the rubric grades assignment(s) 701")
        (expect (test-org-canvas-call-arg
                 (test-org-canvas-find-api-call 'GET "rubrics/801") :params)
                :to-equal '(("include[]" . "associations")))
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil))))

  (it "deletes a rubric only a course association holds"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("rubrics/801" . ((id . 801)
                                (associations . [((association_type . "Course") (association_id . 1))])))))
      (expect (test-org-canvas--diff-outcome
               (org-canvas-diff-delete-rows '((:feature "Rubrics" :id "801"))
                                            :results test-org-canvas--diff-reshuffle-results)
               "801")
              :to-be 'deleted)))

  (it "deletes a refused object under :force, keeping the reason"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses
            '(("rubrics/801" . ((id . 801)
                                (associations . [((association_type . "Assignment") (association_id . 701))])))))
      (let ((outcome (car (org-canvas-diff-delete-rows
                           '((:feature "Rubrics" :id "801"))
                           :force t
                           :results test-org-canvas--diff-reshuffle-results))))
        (expect (plist-get outcome :outcome) :to-be 'deleted)
        (expect (plist-get outcome :reason) :to-match "701")
        (expect (file-exists-p (plist-get outcome :snapshot)) :to-be-truthy)
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas-api-course-endpoint "rubrics/801"))))))

  (it "deletes every EXTRA row of a feature under :all, and nothing UNCLAIMED or MOVED"
    (test-org-canvas--with-batch-delete
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Module Items" :all t))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :id)) outcomes)
                :to-equal '("5787192" "5787193"))
        (expect (length (test-org-canvas--diff-deleted-urls)) :to-equal 2))))

  (it "deletes a row two selectors name only once"
    (test-org-canvas--with-batch-delete
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Module Items" :id "5787192")
                         (:feature "module-items" :all t))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :id)) outcomes)
                :to-equal '("5787192" "5787193"))
        (expect (length (test-org-canvas--diff-deleted-urls)) :to-equal 2))))

  (it "names a selector that matches no EXTRA row, and sends nothing for it"
    (test-org-canvas--with-batch-delete
      (let ((outcomes (org-canvas-diff-delete-rows
                       '((:feature "Assignments" :id "702")
                         (:feature "Module Items" :id "5787194")
                         (:feature "Module Items" :id "999")
                         (:feature "Assignments" :id "42")
                         (:feature "Nonexistent" :id "1"))
                       :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(not-extra not-extra not-extra not-found not-found))
        (expect (plist-get (car outcomes) :reason) :to-match "UNCLAIMED row")
        (expect (plist-get (nth 3 outcomes) :reason) :to-match "no row with this id")
        (expect (plist-get (nth 4 outcomes) :reason) :to-match "no such feature")
        (expect test-org-canvas-api-calls :to-be nil))))

  (it "calls a RELOCATE row not EXTRA, and :all leaves it alone (issue #343)"
    (test-org-canvas--with-batch-delete
      (let* ((results '((:name "Module Items"
                         :relocate ((:kind relocate :title "Journal 05"
                                     :id "5707155" :where "Week 06"
                                     :to "Week 07")))))
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "Module Items" :id "5707155")
                          (:feature "Module Items" :all t))
                        :results results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(not-extra))
        (expect (plist-get (car outcomes) :reason) :to-match "RELOCATE row")
        (expect test-org-canvas-api-calls :to-be nil))))

  (it "refuses an object Canvas returned nothing for, with nothing to snapshot"
    (test-org-canvas--with-batch-delete
      (setq test-org-canvas-api-responses '(("modules/781715/items/5787192" . nil)))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method url &rest args)
                   (push (list method url nil args) test-org-canvas-api-calls)
                   nil)))
        (let ((outcome (car (org-canvas-diff-delete-rows
                             '((:feature "Module Items" :id "5787192"))
                             :results test-org-canvas--diff-reshuffle-results))))
          (expect (plist-get outcome :outcome) :to-be 'refused)
          (expect (plist-get outcome :reason) :to-match "nothing to snapshot")))))

  (it "reads and checks under a dry run, but writes no snapshot and sends no DELETE"
    (test-org-canvas--with-batch-delete
      (let* ((org-canvas--dry-run t)
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "Module Items" :all t))
                        :results test-org-canvas--diff-reshuffle-results)))
        (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                :to-equal '(dry-run dry-run))
        (expect (cl-remove-if (lambda (c) (eq (car c) 'GET)) test-org-canvas-api-calls)
                :to-be nil)
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil))))

  (it "fails every row on a read-only course, before a snapshot or a DELETE"
    (test-org-canvas--with-batch-delete
      (let* ((org-canvas-read-only t)
             (outcomes (org-canvas-diff-delete-rows
                        '((:feature "Module Items" :id "5787192"))
                        :results test-org-canvas--diff-reshuffle-results)))
        (expect (plist-get (car outcomes) :outcome) :to-be 'failed)
        (expect (plist-get (car outcomes) :reason) :to-match "read-only")
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil)
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil))))

  (it "records a failed read and goes on to the next row"
    (test-org-canvas--with-batch-delete
      (let ((mock (symbol-function 'org-canvas-api-request)))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (if (string-match-p "5787192\\'" url)
                         (signal 'org-canvas-api-error '("404 Not Found"))
                       (apply mock method url args))))
                  ((symbol-function 'org-canvas--log-error) #'ignore))
          (let ((outcomes (org-canvas-diff-delete-rows
                           '((:feature "Module Items" :all t))
                           :results test-org-canvas--diff-reshuffle-results)))
            (expect (mapcar (lambda (o) (plist-get o :outcome)) outcomes)
                    :to-equal '(failed deleted))
            (expect (plist-get (car outcomes) :reason) :to-match "404")
            (expect (length (test-org-canvas--diff-deleted-urls)) :to-equal 1))))))

  (it "runs the report's own comparison when no results are given"
    (test-org-canvas--with-batch-delete
      (let ((collected 0))
        (cl-letf (((symbol-function 'org-canvas--diff-collect-results)
                   (lambda () (setq collected (1+ collected))
                     test-org-canvas--diff-reshuffle-results)))
          (expect (test-org-canvas--diff-outcome
                   (org-canvas-diff-delete-rows '((:feature "Module Items" :id "5787193")))
                   "5787193")
                  :to-be 'deleted)
          (expect collected :to-equal 1))))))

(describe "org-canvas-diff-delete's safety check (issue #345)"
  (it "names an assignment's submissions in its question"
    (test-org-canvas--with-snapshot-dir
      (let ((asked nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (q) (setq asked q) nil))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _)
                     (push (list method url) test-org-canvas--diff-delete-requests)
                     (if (string-match-p "submissions" url)
                         [((submitted_at . "2026-09-01T10:00:00Z"))]
                       '((id . 701))))))
          (with-current-buffer (test-org-canvas--diff-report-buffer
                                '((:name "Assignments" :extra ((:kind extra :title "Session 87" :id "701")))))
            (test-org-canvas--diff-goto-row 'extra)
            (org-canvas-diff-delete)))
        (expect asked :to-equal
                "Delete Assignments 'Session 87' (id 701) from Canvas, although the assignment has 1 submission(s) and 0 score(s)? ")
        (expect (test-org-canvas--diff-deletes) :to-be nil)))))

;;;; Deleting an Owned Assignment (issue #366)

(defconst test-org-canvas-366--discussion
  '((id . 701) (name . "Week 3 forum")
    (submission_types . ["discussion_topic"])
    (discussion_topic . ((id . 55) (title . "Week 3 forum"))))
  "A graded discussion's assignment nobody has posted to yet.")

(defconst test-org-canvas-366--classic-quiz
  '((id . 701) (name . "Quiz 2") (quiz_id . 620)
    (submission_types . ["online_quiz"]))
  "A classic quiz's shadow assignment.")

(defconst test-org-canvas-366--new-quiz
  '((id . 701) (name . "Midterm") (is_quiz_lti_assignment . t)
    (submission_types . ["external_tool"]))
  "A New Quiz's assignment, read as an assignment.")

(defconst test-org-canvas-366--new-quiz-by-url
  '((id . 701) (name . "Midterm") (submission_types . ["external_tool"])
    (external_tool_tag_attributes
     . ((url . "https://school.quiz-lti-iad-prod.instructure.com/lti/launch"))))
  "A New Quiz's assignment from an instance that omits the flag.")

(defconst test-org-canvas-366--ordinary
  '((id . 701) (name . "Session 87") (submission_types . ["online_upload"]))
  "An assignment nothing else owns.")

(defconst test-org-canvas-366--results
  '((:name "Assignments"
     :extra ((:kind extra :title "Session 87" :id "701"))))
  "An Assignments EXTRA row, the New Quizzes section excluded.")

(defun test-org-canvas-366--respond (object)
  "Answer the assignment read with OBJECT and its submissions with none."
  (setq test-org-canvas-api-responses
        `(("assignments/701/submissions" . [])
          ("assignments/701" . ,object))))

(defun test-org-canvas-366--delete-rows (object &optional force)
  "Delete the Assignments row whose read returns OBJECT; return its outcome.
FORCE is passed on as :force."
  (test-org-canvas-366--respond object)
  (car (org-canvas-diff-delete-rows '((:feature "Assignments" :id "701"))
                                    :force force
                                    :results test-org-canvas-366--results)))

(defun test-org-canvas-366--k (object answer)
  "Press `k' on the Assignments row whose read returns OBJECT.
ANSWER is what the question gets; return the question asked."
  (test-org-canvas-366--respond object)
  (let ((asked nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (q) (setq asked q) answer)))
      (with-current-buffer (test-org-canvas--diff-report-buffer
                            test-org-canvas-366--results)
        (test-org-canvas--diff-goto-row 'extra)
        (org-canvas-diff-delete)))
    asked))

(describe "org-canvas-diff-delete-rows on an owned assignment (issue #366)"
  (it "refuses a graded discussion's assignment nobody posted to, naming it"
    (test-org-canvas--with-batch-delete
      (let ((outcome (test-org-canvas-366--delete-rows
                      test-org-canvas-366--discussion)))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason)
                :to-equal "the assignment belongs to discussion 'Week 3 forum' (a graded discussion, in discussions.org)")
        ;; Named from the object read; its submissions are not listed.
        (expect (test-org-canvas-api-called-p 'GET "submissions") :to-be nil)
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil)
        (expect (directory-files snapshot-dir nil "\\.json\\'") :to-be nil))))

  (it "refuses a classic quiz's assignment, naming the quiz"
    (test-org-canvas--with-batch-delete
      (let ((outcome (test-org-canvas-366--delete-rows
                      test-org-canvas-366--classic-quiz)))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason)
                :to-equal "the assignment belongs to quiz 620 (a classic quiz, in quizzes.org)")
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil))))

  (it "refuses a New Quiz's assignment with the New Quizzes section off"
    (test-org-canvas--with-batch-delete
      (let ((outcome (test-org-canvas-366--delete-rows
                      test-org-canvas-366--new-quiz)))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason)
                :to-equal "the assignment belongs to a New Quiz, in new-quizzes.org")
        (expect (test-org-canvas--diff-deleted-urls) :to-be nil))))

  (it "refuses a New Quiz's assignment known only by its launch URL"
    (test-org-canvas--with-batch-delete
      (let ((outcome (test-org-canvas-366--delete-rows
                      test-org-canvas-366--new-quiz-by-url)))
        (expect (plist-get outcome :outcome) :to-be 'refused)
        (expect (plist-get outcome :reason) :to-match "a New Quiz"))))

  (it "asks the predicate prune and the orphan scan use (#319)"
    (test-org-canvas--with-batch-delete
      (let ((seen nil))
        (cl-letf (((symbol-function 'org-canvas--assignment-owned-p)
                   (lambda (item) (push (alist-get 'id item) seen) nil)))
          (let ((outcome (test-org-canvas-366--delete-rows
                          test-org-canvas-366--discussion)))
            (expect seen :to-equal '(701))
            (expect (plist-get outcome :outcome) :to-be 'deleted))))))

  (it "falls back to the feature's delete-skip reason for an owner it cannot name"
    (test-org-canvas--with-batch-delete
      (cl-letf (((symbol-function 'org-canvas--assignment-owned-p)
                 (lambda (_item) t)))
        (let ((outcome (test-org-canvas-366--delete-rows
                        test-org-canvas-366--ordinary)))
          (expect (plist-get outcome :outcome) :to-be 'refused)
          (expect (plist-get outcome :reason)
                  :to-equal (concat "the assignment belongs to "
                                    org-canvas--assignment-owned-reason))))))

  (it "deletes an owned assignment under :force, keeping the reason"
    (test-org-canvas--with-batch-delete
      (let ((outcome (test-org-canvas-366--delete-rows
                      test-org-canvas-366--discussion t)))
        (expect (plist-get outcome :outcome) :to-be 'deleted)
        (expect (plist-get outcome :reason) :to-match "Week 3 forum")
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas-api-course-endpoint "assignments/701")))
        (expect (length (directory-files snapshot-dir nil "\\.json\\'"))
                :to-equal 1))))

  (it "still deletes an ordinary assignment with no submissions"
    (test-org-canvas--with-batch-delete
      (let ((outcome (test-org-canvas-366--delete-rows
                      test-org-canvas-366--ordinary)))
        (expect (plist-get outcome :outcome) :to-be 'deleted)
        (expect (plist-get outcome :reason) :to-be nil)
        (expect (test-org-canvas-api-called-p 'GET "assignments/701/submissions")
                :to-be-truthy)
        (expect (test-org-canvas--diff-deleted-urls)
                :to-equal (list (org-canvas-api-course-endpoint "assignments/701")))))))

(describe "k on an owned assignment (issue #366)"
  (it "names a graded discussion in its question and deletes nothing on no"
    (test-org-canvas--with-batch-delete
      (expect (test-org-canvas-366--k test-org-canvas-366--discussion nil)
              :to-equal "Delete Assignments 'Session 87' (id 701) from Canvas, although the assignment belongs to discussion 'Week 3 forum' (a graded discussion, in discussions.org)? ")
      (expect (test-org-canvas--diff-deleted-urls) :to-be nil)))

  (it "names a classic quiz in its question"
    (test-org-canvas--with-batch-delete
      (expect (test-org-canvas-366--k test-org-canvas-366--classic-quiz nil)
              :to-match "although the assignment belongs to quiz 620 (a classic quiz, in quizzes.org)\\? \\'")
      (expect (test-org-canvas--diff-deleted-urls) :to-be nil)))

  (it "names a New Quiz in its question with the New Quizzes section off"
    (test-org-canvas--with-batch-delete
      (expect (test-org-canvas-366--k test-org-canvas-366--new-quiz nil)
              :to-match "although the assignment belongs to a New Quiz, in new-quizzes.org\\? \\'")
      (expect (test-org-canvas--diff-deleted-urls) :to-be nil)))

  (it "deletes the owned assignment when the operator answers yes"
    (test-org-canvas--with-batch-delete
      (test-org-canvas-366--k test-org-canvas-366--discussion t)
      (expect (test-org-canvas--diff-deleted-urls)
              :to-equal (list (org-canvas-api-course-endpoint "assignments/701")))))

  (it "asks plainly about an ordinary assignment and deletes it"
    (test-org-canvas--with-batch-delete
      (expect (test-org-canvas-366--k test-org-canvas-366--ordinary t)
              :to-equal "Delete Assignments 'Session 87' (id 701) from Canvas? ")
      (expect (test-org-canvas--diff-deleted-urls)
              :to-equal (list (org-canvas-api-course-endpoint "assignments/701"))))))

(describe "org-canvas--diff-snapshot-dir (issue #345)"
  (it "defaults to canvas-snapshots/ under the course directory, resolved when used"
    (let* ((dir (make-temp-file "diff-course-" t))
           (org-canvas-directory dir)
           (org-canvas-diff-delete-snapshot-directory nil))
      (unwind-protect
          (expect (org-canvas--diff-snapshot-dir)
                  :to-equal (expand-file-name "canvas-snapshots/" (file-truename dir)))
        (delete-directory dir t)))))

(describe "org-canvas-diff-stamp-move (issue #342)"
  (it "stamps the item id on the paired heading, sends nothing, and marks the row"
    (test-org-canvas-342--with-modules-file file
      (with-mock-api
        (with-current-buffer (test-org-canvas-342--row-buffer
                              (test-org-canvas-342--moved file))
          (test-org-canvas--diff-goto-row 'moved)
          (org-canvas-diff-stamp-move)
          (expect (thing-at-point 'line t)
                  :to-match "STAMPED   R6: Arguing Both Sides (CANVAS_ID 5707155 on 'R7: Arguing Both Sides' in 'Week 6', nothing sent; the next sync moves it there from 'Week 5', keeping its id)")
          (expect (get-text-property (point) 'org-canvas-diff-row) :to-be-truthy))
        (expect (test-org-canvas-api-call-count) :to-equal 0))
      (expect (test-org-canvas-342--ids file)
              :to-equal '(("Readings" nil)
                          ("R7: Arguing Both Sides" "5707155")
                          ("Readings" nil)))))

  (it "is what s does, and S stamps them all"
    (expect (lookup-key org-canvas-diff-mode-map (kbd "s"))
            :to-be #'org-canvas-diff-stamp-move)
    (expect (lookup-key org-canvas-diff-mode-map (kbd "S"))
            :to-be #'org-canvas-diff-stamp-moves)
    (with-current-buffer (test-org-canvas--diff-report-buffer nil)
      (expect (buffer-string) :to-match "s/S stamp moved")))

  (it "finds the heading under its module when it is no longer on the recorded line"
    (test-org-canvas-342--with-modules-file file
      (with-current-buffer (test-org-canvas-342--row-buffer
                            (test-org-canvas-342--moved
                             file :title "Readings" :partner-heading "Readings"
                             :partner-title "Readings" :line 1))
        (test-org-canvas--diff-goto-row 'moved)
        (org-canvas-diff-stamp-move))
      ;; The Readings under Week 6, not the one under Week 5.
      (expect (test-org-canvas-342--ids file)
              :to-equal '(("Readings" nil)
                          ("R7: Arguing Both Sides" nil)
                          ("Readings" "5707155")))))

  (it "refuses when a heading already carries the id, and writes nothing"
    (test-org-canvas-342--with-modules-file file
      (let ((entry (test-org-canvas-342--moved file :id "781703")))
        (with-current-buffer (test-org-canvas-342--row-buffer entry)
          (test-org-canvas--diff-goto-row 'moved)
          (expect (condition-case e (org-canvas-diff-stamp-move)
                    (user-error (error-message-string e)))
                  :to-match "Item id 781703 is already on a heading")
          (expect (thing-at-point 'line t) :to-match "MOVED")))
      (expect (mapcar #'cadr (test-org-canvas-342--ids file)) :to-equal '(nil nil nil))))

  (it "refuses when the paired heading is gone or already stamped"
    (test-org-canvas-342--with-modules-file file
      (with-current-buffer (test-org-canvas-342--row-buffer
                            (test-org-canvas-342--moved
                             file :partner-heading "Nowhere" :partner-title "Nowhere"))
        (test-org-canvas--diff-goto-row 'moved)
        (expect (condition-case e (org-canvas-diff-stamp-move)
                  (user-error (error-message-string e)))
                :to-match "Cannot find the unstamped heading .Nowhere. under .Week 6."))
      (expect (org-canvas--diff-stamp-entry
               (test-org-canvas-342--moved (concat file ".missing")))
              :to-equal 'not-found)
      ;; A row with no partner title names the item instead.
      (expect (condition-case e
                  (org-canvas--diff-stamp-refusal
                   '(:title "Closer" :to "Week 6") 'not-found)
                (user-error (error-message-string e)))
              :to-match "unstamped heading .Closer. under .Week 6.")))

  (it "asks first on a row paired by title only, and writes nothing when declined"
    (test-org-canvas-342--with-modules-file file
      (let ((answer nil) (asked nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (q) (setq asked q) answer)))
          (with-current-buffer (test-org-canvas-342--row-buffer
                                (test-org-canvas-342--moved file :by-title t))
            (test-org-canvas--diff-goto-row 'moved)
            (expect (org-canvas-diff-stamp-move) :to-throw 'user-error)
            (expect asked :to-match "paired by title only")
            (expect (cadr (nth 1 (test-org-canvas-342--ids file))) :to-be nil)
            (setq answer t)
            (org-canvas-diff-stamp-move)))
        (expect (cadr (nth 1 (test-org-canvas-342--ids file))) :to-equal "5707155"))))

  (it "refuses a row that is not MOVED"
    (with-current-buffer (test-org-canvas--diff-report-buffer
                          '((:name "Assignments" :extra ((:kind extra :title "Surprise" :id "99")))))
      (test-org-canvas--diff-goto-row 'extra)
      (expect (org-canvas-diff-stamp-move) :to-throw 'user-error))))

(describe "org-canvas-diff-stamp-moves (issue #342)"
  (defun test-org-canvas-342--run (results &optional answer)
    "Run the bulk stamp over RESULTS as if every feature reported them.
ANSWER `no' declines the confirmation.  Returns (PROMPT REPORT COUNT)."
    (let ((prompt nil) (count nil))
      (with-org-canvas-test-config
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                    ((symbol-function 'display-buffer) (lambda (&rest _) nil))
                    ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                    ((symbol-function 'y-or-n-p)
                     (lambda (q) (setq prompt q) (not (eq answer 'no))))
                    ((symbol-function 'org-canvas--diff-feature)
                     (lambda (feature)
                       (or (cl-find (plist-get feature :name) results
                                    :key (lambda (r) (plist-get r :name))
                                    :test #'string=)
                           (list :name (plist-get feature :name))))))
            (setq count (org-canvas-diff-stamp-moves)))))
      (list prompt
            (with-current-buffer org-canvas--diff-stamp-buffer-name (buffer-string))
            count)))

  (it "stamps every MOVED row paired by content after confirming, lists the rest, and sends nothing"
    (test-org-canvas-342--with-modules-file file
      (let ((out nil))
        (with-mock-api
          (setq out (test-org-canvas-342--run
                     `((:name "Modules"
                        :children
                        (:name "Module Items"
                         :extra (,(test-org-canvas-342--moved file)
                                 ,(test-org-canvas-342--moved
                                   file :title "Readings" :id "502"
                                   :partner-heading "Readings" :partner-title "Readings"
                                   :line 11 :by-title t)
                                 ,(test-org-canvas-342--moved
                                   file :title "Gone" :id "503"
                                   :partner-heading "Gone" :partner-title "Gone")
                                 ,(test-org-canvas-342--moved
                                   file :title "Twice" :id "781704")
                                 (:kind extra :title "Stray" :id "504")))))))
          (expect (test-org-canvas-api-call-count) :to-equal 0))
        (expect (nth 0 out) :to-match "Stamp the item id of 3 MOVED rows")
        (expect (nth 1 out) :to-match "STAMPED   R6: Arguing Both Sides (CANVAS_ID 5707155 on 'R7: Arguing Both Sides' in 'Week 6')")
        (expect (nth 1 out) :to-match "NOT FOUND Gone (no unstamped heading 'Gone' under 'Week 6')")
        (expect (nth 1 out) :to-match "CLAIMED   Twice (item id 781704 is already on a heading)")
        (expect (nth 1 out) :to-match "HELD      Readings (item id 502; paired by title only")
        (expect (nth 1 out) :to-match "1 item id(s) stamped, nothing sent to Canvas")
        (expect (nth 1 out) :not :to-match "Stray")
        (expect (nth 2 out) :to-equal 1))
      (expect (test-org-canvas-342--ids file)
              :to-equal '(("Readings" nil)
                          ("R7: Arguing Both Sides" "5707155")
                          ("Readings" nil)))))

  (it "writes nothing when the confirmation is declined"
    (test-org-canvas-342--with-modules-file file
      (expect (test-org-canvas-342--run
               `((:name "Modules"
                  :children (:name "Module Items"
                             :extra (,(test-org-canvas-342--moved file)))))
               'no)
              :to-throw 'user-error)
      (expect (mapcar #'cadr (test-org-canvas-342--ids file)) :to-equal '(nil nil nil))))

  (it "asks nothing and says so when no row qualifies"
    (let ((out (test-org-canvas-342--run nil)))
      (expect (nth 0 out) :to-be nil)
      (expect (nth 1 out) :to-match "No item id stamped")
      (expect (nth 2 out) :to-equal 0)))

  (it "runs from a batch Emacs without asking"
    (test-org-canvas-342--with-modules-file file
      (let ((count nil))
        (cl-letf (((symbol-function 'org-canvas--report-display)
                   (lambda (_name render) (with-temp-buffer (funcall render))))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
          (with-org-canvas-test-config
            (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                      ((symbol-function 'org-canvas--diff-syllabus-references) (lambda () nil))
                      ((symbol-function 'org-canvas--diff-feature)
                       (lambda (feature)
                         (list :name (plist-get feature :name)
                               :extra (and (string= (plist-get feature :name) "Modules")
                                           (list (test-org-canvas-342--moved file)))))))
              (setq count (org-canvas-diff-stamp-moves)))))
        (expect count :to-equal 1))
      (expect (cadr (nth 1 (test-org-canvas-342--ids file))) :to-equal "5707155"))))

;;; org-canvas-diff-verbs-test.el ends here
