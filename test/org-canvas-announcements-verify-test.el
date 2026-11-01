;;; org-canvas-announcements-verify-test.el --- Tests for reading an announcement back -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-verify-announcements' and the announcement
;; arguments of the `verify' batch command (issue #502).  Canvas is
;; `org-canvas-api-request', mocked by `with-mock-api'; no spec reaches
;; the network, and none asserts on the log buffer.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-announcements-verify)
(require 'org-canvas-batch)

;;;; Fixtures

(defconst test-aver--file
  (concat "#+TITLE: Announcements\n"
          "* Reminder\n:PROPERTIES:\n:CANVAS_ID: 3001\n:END:\nAttention please.\n"
          "* Later\n:PROPERTIES:\n:CANVAS_ID: 3002\n"
          ":DELAYED_POST_AT: <2099-01-05 Mon 09:00>\n:ALLOW_COMMENTS: true\n:END:\n"
          "Coming soon.\n"
          "* Earlier\n:PROPERTIES:\n:CANVAS_ID: 3003\n"
          ":DELAYED_POST_AT: <2020-01-06 Mon 09:00>\n:END:\nAlready out.\n"
          "* Draft\n:PROPERTIES:\n:CANVAS_ID: 3004\n:PUBLISHED: false\n:END:\nHidden.\n"
          "* Unpushed\nNot yet.\n")
  "Four stamped announcement headings and one never pushed.")

(defun test-aver--topic (id title message &rest overrides)
  "Return a Canvas topic ID titled TITLE holding MESSAGE, OVERRIDES applied.
By default it is published, posted in 2026 and locked."
  ;; `copy-tree': a backquote shares its constant tail between calls.
  (let ((topic (copy-tree `((id . ,id) (title . ,title) (message . ,message)
                            (published . t) (posted_at . "2026-10-15T12:00:00Z")
                            (delayed_post_at) (locked . t)))))
    (while overrides
      (setf (alist-get (pop overrides) topic) (pop overrides)))
    topic))

(defun test-aver--topics ()
  "Return the mock responses for the four stamped headings, all agreeing."
  `(("discussion_topics/3001" . ,(test-aver--topic 3001 "Reminder"
                                                   "<p>Attention please.</p>"))
    ("discussion_topics/3002" . ,(test-aver--topic
                                  3002 "Later" "<p>Coming soon.</p>"
                                  'posted_at nil 'locked :json-false
                                  'delayed_post_at "2099-01-05T09:00:00Z"))
    ("discussion_topics/3003" . ,(test-aver--topic
                                  3003 "Earlier" "<p>Already out.</p>"
                                  'delayed_post_at "2020-01-06T09:00:00Z"))
    ("discussion_topics/3004" . ,(test-aver--topic
                                  3004 "Draft" "<p>Hidden.</p>"
                                  'published :json-false 'posted_at nil))))

(defmacro test-aver--with-course (&rest body)
  "Run BODY with the announcements file holding `test-aver--file'.
FILE is bound to its path, the API mocked with `test-aver--topics' as
its responses, the zone UTC and the preflight check and echo stubbed."
  (declare (indent 0))
  `(with-test-course (dir :files (list (cons "announcements.org" test-aver--file)))
     (let ((file org-canvas-announcements-file)
           (org-canvas-time-zone "UTC"))
       (with-test-api
         (setq test-org-canvas-api-responses (test-aver--topics))
         (cl-letf (((symbol-function 'org-canvas--preflight-check) #'ignore)
                   ((symbol-function 'org-canvas--user-message) #'ignore))
           ,@body)))))

(defun test-aver--verify (&rest titles)
  "Verify the headings TITLES name; return (RESULTS . PRINTED-TEXT)."
  (let (results)
    (let ((text (with-output-to-string
                  (setq results (org-canvas-verify-announcements
                                 (mapcar (lambda (title) (cons "announcements" title))
                                         titles))))))
      (cons results text))))

(defun test-aver--row (result field)
  "Return the row of RESULT named FIELD."
  (seq-find (lambda (r) (equal (plist-get r :field) field)) (plist-get result :rows)))

(defun test-aver--respond (id &rest overrides)
  "Make topic ID's mock response its default with OVERRIDES applied."
  (let ((cell (assoc (format "discussion_topics/%s" id) test-org-canvas-api-responses)))
    (setcdr cell (copy-tree (cdr cell)))
    (while overrides
      (setf (alist-get (pop overrides) (cdr cell)) (pop overrides)))))

;;;; Agreement

(describe "org-canvas-verify-announcements (issue #502)"
  (it "finds nothing to report when every heading landed as written"
    (test-aver--with-course
      (pcase-let ((`(,results . ,text)
                   (test-aver--verify "Reminder" "Later" "Earlier" "Draft")))
        (expect (mapcar (lambda (r) (plist-get r :differences)) results)
                :to-equal '(0 0 0 0))
        (expect text :to-match "^#\\+TITLE: Announcement verification")
        (expect text :to-match "^\\* Reminder (CANVAS_ID 3001): 0 difference(s)")
        (expect text :to-match "ok +posting +:: Org posts now; Canvas posted <2026-10-15")
        (expect text :to-match "ok +posting +:: Org scheduled for <2099-01-05 Mon 09:00>; Canvas scheduled for <2099-01-05 Mon 09:00>")
        (expect text :to-match "DELAYED_POST_AT <2020-01-06 Mon 09:00> passed")
        (expect text :to-match "ok +comments +:: Org allowed (ALLOW_COMMENTS true); Canvas open")
        (expect text :to-match "ok +body +:: Org matches; Canvas matches"))))

  (it "reads each topic on its own by id, and writes nothing"
    (test-aver--with-course
      (test-aver--verify "Reminder" "Later")
      (expect (mapcar #'car test-org-canvas-api-calls) :to-equal '(GET GET))
      (expect (test-org-canvas-api-called-p 'GET "/courses/[0-9]+/discussion_topics/3001\\'")
              :to-be-truthy)
      (expect (buffer-modified-p (find-buffer-visiting file)) :to-be nil)
      (expect (with-temp-buffer (insert-file-contents file) (buffer-string))
              :to-equal test-aver--file)))

  (it "names a heading by its Canvas id"
    (test-aver--with-course
      (let ((results (with-output-to-string
                       (org-canvas-verify-announcements
                        '(("announcement" "3001" canvas-id))))))
        (expect results :to-match "Reminder (CANVAS_ID 3001)")))))

;;;; Differences

(describe "org-canvas-verify-announcements differences"
  (it "calls a leftover posting time still ahead a difference"
    (test-aver--with-course
      (test-aver--respond 3001 'posted_at nil 'delayed_post_at "2099-02-01T10:00:00Z")
      (pcase-let ((`(,results . ,text) (test-aver--verify "Reminder")))
        (expect (plist-get (car results) :differences) :to-equal 1)
        (expect text :to-match "DIFFERS posting +:: Org posts now; Canvas scheduled for <2099-02-01"))))

  (it "calls a published heading Canvas has not posted a difference"
    (test-aver--with-course
      (test-aver--respond 3001 'posted_at nil)
      (let ((row (test-aver--row (car (car (test-aver--verify "Reminder"))) "posting")))
        (expect (plist-get row :ok) :to-be nil)
        (expect (plist-get row :canvas) :to-equal "not posted"))))

  (it "calls a schedule for another minute, or none, a difference"
    (test-aver--with-course
      (test-aver--respond 3002 'delayed_post_at "2099-01-05T10:00:00Z")
      (expect (plist-get (test-aver--row (car (car (test-aver--verify "Later"))) "posting") :ok)
              :to-be nil)
      (test-aver--respond 3002 'delayed_post_at nil)
      (expect (plist-get (test-aver--row (car (car (test-aver--verify "Later"))) "posting") :ok)
              :to-be nil)))

  (it "compares the title, PUBLISHED and ALLOW_COMMENTS"
    (test-aver--with-course
      (test-aver--respond 3001 'title "Reminder (edited)" 'published :json-false
                          'locked :json-false)
      (pcase-let* ((`(,results . ,text) (test-aver--verify "Reminder"))
                   (result (car results)))
        (expect (plist-get result :differences) :to-equal 3)
        (expect text :to-match "DIFFERS title +:: Org \"Reminder\"; Canvas \"Reminder (edited)\"")
        (expect text :to-match "DIFFERS published +:: Org true; Canvas false")
        (expect text :to-match "DIFFERS comments +:: Org off; Canvas open"))))

  (it "shows where a body stops agreeing"
    (test-aver--with-course
      (test-aver--respond 3001 'message "<p>Attention, everyone.</p>")
      (let ((row (test-aver--row (car (car (test-aver--verify "Reminder"))) "body")))
        (expect (plist-get row :ok) :to-be nil)
        (expect (plist-get row :org) :to-match "Attention please")
        (expect (plist-get row :canvas) :to-match "Attention, everyone"))))

  (it "calls a body that would not export unchecked, and a difference"
    (test-aver--with-course
      (cl-letf (((symbol-function 'org-canvas--diff-compare-body) #'ignore))
        (let ((row (test-aver--row (car (car (test-aver--verify "Reminder"))) "body")))
          (expect (plist-get row :ok) :to-be nil)
          (expect (plist-get row :canvas) :to-equal "unchecked")))))

  (it "reports a heading never pushed, gone, or missing, and goes on"
    (test-aver--with-course
      (cl-letf (((symbol-function 'org-canvas--pull-item-read)
                 (lambda (_feature id)
                   (if (equal id "3002") 'gone
                     (cdr (assoc (format "discussion_topics/%s" id)
                                 test-org-canvas-api-responses))))))
        (pcase-let ((`(,results . ,text)
                     (test-aver--verify "Unpushed" "Later" "Nope" "Reminder")))
          (expect (mapcar (lambda (r) (plist-get r :differences)) results)
                  :to-equal '(1 1 1 0))
          (expect text :to-match "^\\* Unpushed: no CANVAS_ID; the heading was never pushed")
          (expect text :to-match "^\\* Later (CANVAS_ID 3002): not on Canvas any more")
          (expect text :to-match "^\\* Nope: No Announcements heading titled \"Nope\"")))))

  (it "refuses a heading of another feature before reading anything"
    (test-aver--with-course
      (expect (org-canvas-verify-announcements '(("assignment" . "Essay")))
              :to-throw 'user-error)
      (expect test-org-canvas-api-calls :to-be nil)))

  (it "asks for a title when called interactively"
    (test-aver--with-course
      (let ((noninteractive nil) (shown nil))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Reminder"))
                  ((symbol-function 'display-buffer) (lambda (buf &rest _) (setq shown buf))))
          (let ((results (call-interactively #'org-canvas-verify-announcements)))
            (expect (plist-get (car results) :title) :to-equal "Reminder")
            (expect (buffer-name shown) :to-equal "*canvas-verify-announcements*")))))))

;;;; The command line

(describe "scripts/org-canvas verify announcements:TITLE (issue #502)"
  (it "tells an announcement argument from a column"
    (expect (org-canvas-batch--announcement-arg-p "announcements:Reminder") :to-be-truthy)
    (expect (org-canvas-batch--announcement-arg-p "announcement#3001") :to-be-truthy)
    (expect (org-canvas-batch--announcement-arg-p "HW 1") :to-be nil)
    (expect (org-canvas-batch--announcement-arg-p "1001") :to-be nil))

  (it "exits 0 when the announcements agree and 1 when one differs"
    (let ((differences 0) (seen nil))
      (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
                ((symbol-function 'org-canvas-verify-announcements)
                 (lambda (entries) (setq seen entries)
                   (list (list :differences differences))))
                ((symbol-function 'org-canvas-submissions-verify)
                 (lambda (&rest _) (error "No column was named"))))
        (expect (org-canvas-batch-main '("verify" "announcements:Reminder")) :to-equal 0)
        (expect seen :to-equal '(("announcements" "Reminder" title)))
        (setq differences 2)
        (expect (org-canvas-batch-main '("verify" "announcement#3001")) :to-equal 1)
        (expect seen :to-equal '(("announcement" "3001" canvas-id))))))

  (it "verifies columns and announcements named together, both"
    (let ((columns nil) (announcements nil))
      (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore)
                ((symbol-function 'org-canvas-verify-announcements)
                 (lambda (entries) (setq announcements entries)
                   (list (list :differences 0))))
                ((symbol-function 'org-canvas-submissions-verify)
                 (lambda (targets hidden) (setq columns (list targets hidden))
                   (list (list :differences 1)))))
        (expect (org-canvas-batch-main '("verify" "--hidden" "HW" "announcements:Reminder"))
                :to-equal 1)
        (expect columns :to-equal '(("HW") t))
        (expect announcements :to-equal '(("announcements" "Reminder" title))))))

  (it "still calls a bare --hidden a usage error"
    (cl-letf (((symbol-function 'org-canvas-batch-setup) #'ignore))
      (expect (org-canvas-batch-main '("verify" "--hidden")) :to-equal 2))))

(provide 'org-canvas-announcements-verify-test)
;;; org-canvas-announcements-verify-test.el ends here
