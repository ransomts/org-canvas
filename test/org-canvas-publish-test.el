;;; org-canvas-publish-test.el --- Tests for publishing one heading  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-set-published-at-point' and its by-heading and batch
;; twins send `published' alone and adopt the new state in the drawer
;; (issue #466).  Every spec mocks `org-canvas-api-request' with a
;; recorder that answers a GET with the item a spec gives and a PUT
;; with the same item, published as asked and stamped later.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-batch)
(require 'org-canvas-transient)

(defvar test-publish--calls nil
  "Requests recorded by `test-publish--api', newest first: (METHOD URL DATA).")

(defconst test-publish--put-time "2026-10-08T12:00:00Z"
  "The `updated_at' every mocked PUT answers with.")

(defun test-publish--api (remote)
  "Return a mock `org-canvas-api-request' serving REMOTE, an item alist.
A GET returns REMOTE; a PUT returns it with the `published' the body
carried and `updated_at' `test-publish--put-time'."
  (lambda (method url &rest args)
    (let ((data (plist-get args :data)))
      (push (list method url data) test-publish--calls)
      (if (eq method 'PUT)
          (let* ((inner (if (consp (cdar data)) (cdar data) data))
                 (flag (alist-get 'published inner)))
            (append `((published . ,flag) (updated_at . ,test-publish--put-time))
                    remote))
        remote))))

(defun test-publish--puts ()
  "Return the PUT requests recorded, oldest first."
  (reverse (seq-filter (lambda (c) (eq (car c) 'PUT)) test-publish--calls)))

(defmacro test-publish--with-file (var file-var content &rest body)
  "Write CONTENT to a temp file bound to VAR and FILE-VAR, then run BODY.
BODY runs in the file's buffer with point at its start, the course
writable and its API mocked by whatever BODY binds; the buffer and the
file are removed afterwards."
  (declare (indent 3))
  `(let ((,var (make-temp-file "publish-466-" nil ".org"))
         (test-publish--calls nil))
     (unwind-protect
         (with-org-canvas-test-config
           (with-temp-file ,var (insert ,content))
           (let ((,file-var ,var))
             (with-current-buffer (find-file-noselect ,var)
               (goto-char (point-min))
               (cl-letf (((symbol-function 'message) #'ignore))
                 ,@body))))
       (let ((buf (find-buffer-visiting ,var)))
         (when buf
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-file ,var))))

(defconst test-publish--assignment
  (concat "* Essay 1\n:PROPERTIES:\n:POINTS: 10\n:PUBLISHED: true\n"
          ":CANVAS_ID: 101\n:CANVAS_UPDATED_AT: 2026-09-01T00:00:00Z\n:END:\n\n"
          "The essay prompt.\n")
  "An assignment heading, published, last pushed 2026-09-01.")

(defun test-publish--assignment-remote (&rest overrides)
  "Return the assignment Canvas holds, with OVERRIDES (an alist) first."
  (append (car overrides)
          '((id . 101) (name . "Essay 1") (published . t) (unpublishable . t)
            (updated_at . "2026-09-01T00:00:00Z"))))

(defun test-publish--stamp-hash ()
  "Stamp the heading at point with the hash its push would store."
  (org-back-to-heading t)
  (org-entry-put (point) "PAYLOAD_HASH"
                 (org-canvas--sync-heading-hash
                  (cdr (assoc "assignment" org-canvas--sync-entry-specs))))
  (save-buffer))

(defun test-publish--prop (property)
  "Return PROPERTY of the first heading."
  (save-excursion (goto-char (point-min)) (org-entry-get (point) property)))

(describe "org-canvas-set-published-at-point (issue #466)"

  (it "sends only the flag, and restamps the hash of a heading still as pushed"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (test-publish--stamp-hash)
      (let ((old-hash (test-publish--prop "PAYLOAD_HASH")))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-publish--api (test-publish--assignment-remote))))
          (expect (org-canvas-set-published-at-point nil) :to-equal 'sent))
        (expect (mapcar #'caddr (test-publish--puts))
                :to-equal '(((assignment (published . :json-false)))))
        (expect (cadr (car (test-publish--puts))) :to-match "/assignments/101\\'")
        (expect (test-publish--prop "PUBLISHED") :to-equal "false")
        (expect (test-publish--prop "CANVAS_UPDATED_AT") :to-equal test-publish--put-time)
        ;; The hash moved with PUBLISHED, and a push would now skip.
        (expect (test-publish--prop "PAYLOAD_HASH") :not :to-equal old-hash)
        (expect (test-publish--prop "PAYLOAD_HASH")
                :to-equal (org-canvas--sync-heading-hash
                           (cdr (assoc "assignment" org-canvas--sync-entry-specs))))
        (expect (buffer-modified-p) :to-be nil))))

  (it "drops the hash of a heading edited since its last push"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (test-publish--stamp-hash)
      (goto-char (point-max))
      (insert "A draft paragraph not yet pushed.\n")
      (save-buffer)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api (test-publish--assignment-remote))))
        (org-canvas-set-published-at-point nil))
      (expect (length (test-publish--puts)) :to-equal 1)
      (expect (test-publish--prop "PAYLOAD_HASH") :to-be nil)
      (expect (test-publish--prop "CANVAS_UPDATED_AT") :to-equal test-publish--put-time)
      (expect (buffer-string) :to-match "A draft paragraph")))

  (it "refuses an unpublish Canvas would refuse, before sending anything"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api (test-publish--assignment-remote
                                     '((unpublishable . :json-false))))))
        (let ((err (condition-case e (org-canvas-set-published-at-point nil)
                     (user-error e))))
          (expect (cadr err) :to-match "will not unpublish 'Essay 1': it has student submissions")))
      (expect (test-publish--puts) :to-equal nil)
      (expect (test-publish--prop "PUBLISHED") :to-equal "true")))

  (it "publishes even when Canvas would not unpublish"
    (test-publish--with-file file org-canvas-assignments-file
        (replace-regexp-in-string "PUBLISHED: true" "PUBLISHED: false"
                                  test-publish--assignment)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api (test-publish--assignment-remote
                                     '((published . :json-false)
                                       (unpublishable . :json-false))))))
        (expect (org-canvas-set-published-at-point t) :to-equal 'sent))
      (expect (mapcar #'caddr (test-publish--puts))
              :to-equal '(((assignment (published . t)))))
      (expect (test-publish--prop "PUBLISHED") :to-equal "true")))

  (it "keeps the baseline when Canvas changed the item since it, so the next push asks"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (test-publish--stamp-hash)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api (test-publish--assignment-remote
                                     '((updated_at . "2026-09-20T00:00:00Z"))))))
        (org-canvas-set-published-at-point nil))
      (expect (length (test-publish--puts)) :to-equal 1)
      (expect (test-publish--prop "PUBLISHED") :to-equal "false")
      (expect (test-publish--prop "CANVAS_UPDATED_AT") :to-equal "2026-09-01T00:00:00Z")
      (expect (test-publish--prop "PAYLOAD_HASH") :to-be nil)))

  (it "takes a newer remote stamp for the change with conflict detection off"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (let ((org-canvas-detect-conflicts nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-publish--api (test-publish--assignment-remote
                                       '((updated_at . "2026-09-20T00:00:00Z"))))))
          (org-canvas-set-published-at-point nil)))
      (expect (test-publish--prop "CANVAS_UPDATED_AT") :to-equal test-publish--put-time)))

  (it "sends nothing when Canvas already holds the state, and records it"
    (test-publish--with-file file org-canvas-assignments-file
        (replace-regexp-in-string "PUBLISHED: true" "PUBLISHED: false"
                                  test-publish--assignment)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api (test-publish--assignment-remote))))
        (expect (org-canvas-set-published-at-point t) :to-equal 'unchanged))
      (expect (test-publish--puts) :to-equal nil)
      (expect (test-publish--prop "PUBLISHED") :to-equal "true")
      (expect (test-publish--prop "CANVAS_UPDATED_AT") :to-equal "2026-09-01T00:00:00Z")))

  (it "sends and writes nothing in a dry run"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (let ((before (buffer-string))
            (org-canvas--dry-run t))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-publish--api (test-publish--assignment-remote))))
          (expect (org-canvas-set-published-at-point nil) :to-equal 'dry-run))
        (expect (test-publish--puts) :to-equal nil)
        (expect (buffer-string) :to-equal before)
        (expect (buffer-modified-p) :to-be nil))))

  (it "refuses on a read-only course before any request"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (let ((org-canvas-read-only t))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (test-publish--api (test-publish--assignment-remote))))
          (expect (org-canvas-set-published-at-point nil)
                  :to-throw 'org-canvas-read-only-error)))
      (expect test-publish--calls :to-equal nil)))

  (it "refuses a heading Canvas no longer has"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api '((id . 101) (workflow_state . "deleted")))))
        (expect (org-canvas-set-published-at-point nil) :to-throw 'user-error))
      (expect (test-publish--puts) :to-equal nil)))

  (it "refuses a heading never pushed"
    (test-publish--with-file file org-canvas-assignments-file "* Draft\n"
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Must not reach Canvas"))))
        (let ((err (condition-case e (org-canvas-set-published-at-point t)
                     (user-error e))))
          (expect (cadr err) :to-match ".Draft. has no CANVAS_ID")))))

  (it "refuses a file whose headings do not publish one at a time"
    (test-publish--with-file file org-canvas-rubrics-file
        "* Rubric\n:PROPERTIES:\n:CANVAS_ID: 5\n:END:\n"
      (expect (org-canvas-set-published-at-point t) :to-throw 'user-error)))

  (it "publishes a page by its CANVAS_URL and refuses to unpublish the front page"
    (test-publish--with-file file org-canvas-pages-file
        "* Welcome\n:PROPERTIES:\n:CANVAS_URL: welcome\n:PUBLISHED: false\n:END:\n\nHi.\n"
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api '((url . "welcome") (title . "Welcome")
                                      (published . :json-false) (front_page . t)))))
        (org-canvas-set-published-at-point t)
        (expect (mapcar #'caddr (test-publish--puts))
                :to-equal '(((wiki_page (published . t)))))
        (expect (cadr (car (test-publish--puts))) :to-match "/pages/welcome\\'")
        (expect (test-publish--prop "CANVAS_URL") :to-equal "welcome")
        (expect (test-publish--prop "CANVAS_ID") :to-be nil)
        (let ((err (condition-case e (org-canvas-set-published-at-point nil)
                     (user-error e))))
          (expect (cadr err) :to-match "it is the course front page")))))

  (it "refuses a page scheduled by PUBLISH_AT, whose schedule the change would cancel"
    (test-publish--with-file file org-canvas-pages-file
        (concat "* Week 5\n:PROPERTIES:\n:CANVAS_URL: week-5\n"
                ":PUBLISH_AT: <2026-10-20 Tue 08:00>\n:END:\n")
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Must not reach Canvas"))))
        (let ((err (condition-case e (org-canvas-set-published-at-point t)
                     (user-error e))))
          (expect (cadr err) :to-match "scheduled by PUBLISH_AT")))))

  (it "sends a discussion's flag flat and refuses one with replies"
    (test-publish--with-file file org-canvas-discussions-file
        "* Week 1 forum\n:PROPERTIES:\n:CANVAS_ID: 7\n:PUBLISHED: true\n:END:\n\nTalk.\n"
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api '((id . 7) (title . "Week 1 forum")
                                      (published . t) (can_unpublish . :json-false)))))
        (let ((err (condition-case e (org-canvas-set-published-at-point nil)
                     (user-error e))))
          (expect (cadr err) :to-match "it has replies")))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api '((id . 7) (title . "Week 1 forum")
                                      (published . t) (can_unpublish . t)))))
        (org-canvas-set-published-at-point nil))
      (expect (mapcar #'caddr (test-publish--puts))
              :to-equal '(((published . :json-false))))))

  (it "climbs from a quiz question to its quiz"
    (test-publish--with-file file org-canvas-quizzes-file
        (concat "* Quiz 1\n:PROPERTIES:\n:CANVAS_ID: 30\n:PUBLISHED: true\n:END:\n"
                "** Question one\n:PROPERTIES:\n:CANVAS_ID: 900\n:END:\nWhat?\n")
      (search-forward "What?")
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api '((id . 30) (title . "Quiz 1")
                                      (published . t) (unpublishable . t)))))
        (org-canvas-set-published-at-point nil))
      (expect (mapcar #'caddr (test-publish--puts))
              :to-equal '(((quiz (published . :json-false)))))
      (expect (cadr (car (test-publish--puts))) :to-match "/quizzes/30\\'")
      (expect (test-publish--prop "PUBLISHED") :to-equal "false"))))

(describe "org-canvas-publish-at-point and org-canvas-unpublish-at-point (issue #466)"
  (it "are commands that publish and unpublish"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas-set-published-at-point)
                 (lambda (state) (push state seen) 'sent)))
        (call-interactively #'org-canvas-publish-at-point)
        (call-interactively #'org-canvas-unpublish-at-point))
      (expect seen :to-equal '(nil t)))))

(describe "org-canvas-set-published (issue #466)"
  (it "finds the heading by title in the feature's file"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (goto-char (point-max))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api (test-publish--assignment-remote))))
        (expect (org-canvas-set-published "assignments" "Essay 1" nil) :to-equal 'sent))
      (expect (test-publish--prop "PUBLISHED") :to-equal "false")))

  (it "finds the heading by its Canvas id"
    (test-publish--with-file file org-canvas-assignments-file test-publish--assignment
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (test-publish--api (test-publish--assignment-remote))))
        (org-canvas-set-published 'assignment 101 nil 'canvas-id))
      (expect (length (test-publish--puts)) :to-equal 1)))

  (it "names the features it knows when given another"
    (let ((err (condition-case e (org-canvas-set-published "rubric" "R" t)
                 (user-error e))))
      (expect (cadr err) :to-match "assignment, quiz, discussion, page"))))

(describe "org-canvas--sync-heading-hash (issue #466)"
  (it "answers nil, with a warning, when the heading cannot be built"
    (let ((warned nil))
      (with-temp-org-buffer "* Heading\n"
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (&rest _) (setq warned t))))
          (expect (org-canvas--sync-heading-hash
                   (list :parse (lambda () (error "Unparseable"))))
                  :to-be nil)))
      (expect warned :to-be t)))

  (it "registers the push at point of every feature that publishes one at a time"
    (dolist (name '("assignment" "quiz" "discussion" "page"))
      (expect (list name (functionp (plist-get (cdr (assoc name org-canvas--sync-entry-specs))
                                               :parse)))
              :to-equal (list name t)))))

(describe "batch publish and unpublish (issue #466)"
  (defun test-publish--batch (args)
    "Return the exit status of ARGS with the course setup mocked away."
    (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
              ((symbol-function 'message) #'ignore))
      (let (status)
        (with-output-to-string (setq status (org-canvas-batch-main args)))
        status)))

  (it "sets each named heading and exits 0, under the global dry run"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas-set-published)
                 (lambda (feature target published by)
                   (push (list feature target published by org-canvas--dry-run) seen)
                   'sent)))
        (expect (test-publish--batch '("-n" "unpublish" "assignment:Essay 1" "page#welcome"))
                :to-equal 0)
        (expect (test-publish--batch '("publish" "quiz:Quiz 1")) :to-equal 0))
      (expect (reverse seen)
              :to-equal '(("assignment" "Essay 1" nil title t)
                          ("page" "welcome" nil canvas-id t)
                          ("quiz" "Quiz 1" t title nil)))))

  (it "exits 1 when a heading is refused, after trying the rest"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas-set-published)
                 (lambda (_feature target &rest _)
                   (push target seen)
                   (when (equal target "Essay 1")
                     (user-error "Canvas will not unpublish 'Essay 1'"))
                   'sent)))
        (expect (test-publish--batch '("unpublish" "assignment:Essay 1" "assignment:Essay 2"))
                :to-equal 1))
      (expect seen :to-equal '("Essay 2" "Essay 1"))))

  (it "is listed in the help"
    (let ((text (org-canvas-batch--help-text)))
      (expect text :to-match "publish FEATURE:TITLE")
      (expect text :to-match "unpublish FEATURE:TITLE"))))

(describe "the dispatch menu (issue #466)"
  (it "offers publish and unpublish at point"
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-publish-at-point)
            :to-be-truthy)
    (expect (test-org-canvas-transient-has-command-p
             'org-canvas-dispatch 'org-canvas-unpublish-at-point)
            :to-be-truthy)))

(provide 'org-canvas-publish-test)
;;; org-canvas-publish-test.el ends here
