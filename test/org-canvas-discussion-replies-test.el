;;; org-canvas-discussion-replies-test.el --- Tests for the discussion replies pull -*- lexical-binding: t; -*-

;;; Commentary:

;; Specs for `org-canvas-discussion-replies', the pull-only archive of
;; discussion entries and replies.  Every request goes through a mocked
;; `org-canvas-api-request'; nothing reaches the network.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-discussion-replies)

(defvar test-replies--calls nil
  "The (METHOD . URL) pairs the mocked API saw during a spec.")

(defun test-replies--entry (id name at message &rest more)
  "Build an entry alist with ID, user NAME, posted AT and MESSAGE, plus MORE."
  (append `((id . ,id) (user_id . ,(* 10 id)) (user_name . ,name)
            (created_at . ,at) (message . ,message))
          more))

(defmacro test-replies--with-course (topics entries replies &rest body)
  "Run BODY with a temp course whose API answers TOPICS, ENTRIES and REPLIES.
TOPICS is the topic list; ENTRIES an alist of topic id to entry list;
REPLIES an alist of entry id to reply list.  The replies file is bound
to a fresh path under a temp directory, DIR, which BODY may use."
  (declare (indent 3))
  `(let* ((dir (make-temp-file "replies-" t))
          (org-canvas-directory dir)
          (org-canvas-discussion-replies-file (expand-file-name "discussion-replies.org" dir))
          (org-canvas-discussions-file (expand-file-name "discussions.org" dir))
          (test-replies--calls nil))
     (unwind-protect
         (with-org-canvas-test-config
           (with-html-to-org-identity
             (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil))
                       ((symbol-function 'org-canvas-api-request)
                        (lambda (method url &rest _)
                          (push (cons method url) test-replies--calls)
                          (cond
                           ((string-match "/entries/\\([0-9]+\\)/replies" url)
                            (vconcat (alist-get (string-to-number (match-string 1 url)) ,replies)))
                           ((string-match "/discussion_topics/\\([0-9]+\\)/entries" url)
                            (let ((topic-id (string-to-number (match-string 1 url))))
                              (when (eq (alist-get topic-id ,entries 'absent) 'forbidden)
                                (signal 'org-canvas-permission-error
                                        (list "Permission denied (HTTP 403)")))
                              (vconcat (alist-get topic-id ,entries))))
                           ((string-match "/discussion_topics" url) (vconcat ,topics))
                           (t nil)))))
               ,@body)))
       (dolist (f (list org-canvas-discussion-replies-file org-canvas-discussions-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-replies--file-text ()
  "Return the replies file's text as saved on disk."
  (with-temp-buffer
    (insert-file-contents org-canvas-discussion-replies-file)
    (buffer-string)))

(describe "org-canvas-pull-discussion-replies"
  (it "writes a topic with its entries and their replies at the right levels"
    (test-replies--with-course
        '(((id . 7) (title . "Week 1: Introductions")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "<p>Hello all</p>"
                                      '(has_more_replies . t))
                ,(test-replies--entry 101 "Bob" "2026-09-02T09:30:00Z" "<p>Hi</p>"))))
        `((100 . (,(test-replies--entry 200 "Carol" "2026-09-01T15:00:00Z" "<p>Welcome</p>"
                                        '(parent_id . 100)))))
      (org-canvas-pull-discussion-replies)
      (let ((text (test-replies--file-text)))
        (expect text :to-match "^\\* Week 1: Introductions\n")
        (expect text :to-match ":CANVAS_ID: +7\n")
        (expect text :to-match "^\\*\\* Reply by Alice (2026-09-01 [0-9:]+)\n")
        (expect text :to-match "^\\*\\* Reply by Bob (2026-09-02 [0-9:]+)\n")
        (expect text :to-match "^\\*\\*\\* Reply by Carol (2026-09-01 [0-9:]+)\n")
        (expect text :to-match ":CANVAS_ENTRY_ID: +100\n")
        (expect text :to-match ":AUTHOR: +Alice\n")
        (expect text :to-match ":AUTHOR_ID: +1000\n")
        (expect text :to-match ":POSTED_AT: +<2026-09-01")
        (expect text :to-match ":PARENT_ENTRY_ID: +100\n")
        (expect text :to-match "<p>Hello all</p>")
        (expect text :to-match "<p>Welcome</p>")
        (expect text :to-match "^#\\+LAST_SYNCED:")
        ;; Alice's reply sits under Alice, before Bob.
        (expect (string-match "Reply by Carol" text)
                :to-be-less-than (string-match "Reply by Bob" text)))))

  (it "fetches replies only for an entry that has some"
    (test-replies--with-course
        '(((id . 7) (title . "T")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a")
                ,(test-replies--entry 101 "Bob" "2026-09-02T09:30:00Z" "b"
                                      '(recent_replies . [((id . 300))])))))
        `((101 . (,(test-replies--entry 300 "Dan" "2026-09-02T10:00:00Z" "c"))))
      (org-canvas-pull-discussion-replies)
      (expect (cl-count-if (lambda (c) (string-match-p "/replies" (cdr c))) test-replies--calls)
              :to-equal 1)
      (expect (cl-some (lambda (c) (string-match-p "/entries/101/replies" (cdr c))) test-replies--calls)
              :to-be-truthy)
      (expect (test-replies--file-text) :to-match "^\\*\\*\\* Reply by Dan")))

  (it "writes nothing for a topic without entries"
    (test-replies--with-course
        '(((id . 7) (title . "Quiet")) ((id . 8) (title . "Busy")))
        `((8 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a"))))
        nil
      (org-canvas-pull-discussion-replies)
      (let ((text (test-replies--file-text)))
        (expect text :not :to-match "Quiet")
        (expect text :to-match "^\\* Busy\n"))))

  (it "leaves announcements out"
    (test-replies--with-course
        '(((id . 7) (title . "News") (is_announcement . t)))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a"))))
        nil
      (org-canvas-pull-discussion-replies)
      (expect (test-replies--file-text) :not :to-match "News")))

  (it "updates an entry in place on a re-pull instead of duplicating it"
    (test-replies--with-course
        '(((id . 7) (title . "T")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "first")
                ,(test-replies--entry 101 "Bob" "2026-09-02T09:30:00Z" "b"))))
        nil
      (org-canvas-pull-discussion-replies)
      (let ((first (test-replies--file-text)))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _)
                     (push (cons method url) test-replies--calls)
                     (cond
                      ((string-match "/entries$" url)
                       (vconcat (list (test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "edited")
                                      (test-replies--entry 101 "Bob" "2026-09-02T09:30:00Z" "b"))))
                      ((string-match "/discussion_topics" url) (vconcat '(((id . 7) (title . "T")))))
                      (t nil)))))
          (org-canvas-pull-discussion-replies))
        (let ((text (test-replies--file-text)))
          (expect (cl-count "Reply by Alice" (split-string text "\n") :test #'string-match-p) :to-equal 1)
          (expect (cl-count "^\\* T$" (split-string text "\n") :test #'string-match-p) :to-equal 1)
          (expect text :to-match "edited")
          (expect text :not :to-match "first")
          (expect (length (split-string text "\n"))
                  :to-equal (length (split-string first "\n")))))))

  (it "keeps a local entry Canvas no longer returns"
    (test-replies--with-course
        '(((id . 7) (title . "T")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a")
                ,(test-replies--entry 101 "Bob" "2026-09-02T09:30:00Z" "b"))))
        nil
      (org-canvas-pull-discussion-replies)
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (_method url &rest _)
                   (cond
                    ((string-match "/entries$" url)
                     (vconcat (list (test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a"))))
                    ((string-match "/discussion_topics" url) (vconcat '(((id . 7) (title . "T")))))
                    (t nil)))))
        (org-canvas-pull-discussion-replies))
      (expect (test-replies--file-text) :to-match "Reply by Bob")))

  (it "links the topic to its heading in discussions.org when that file holds its id"
    (test-replies--with-course
        '(((id . 7) (title . "Week 1")) ((id . 9) (title . "Unknown here")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a")))
          (9 . (,(test-replies--entry 102 "Eve" "2026-09-03T14:00:00Z" "e"))))
        nil
      (with-temp-file org-canvas-discussions-file
        (insert "* Week 1: Introductions\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n"))
      (org-canvas-pull-discussion-replies)
      (let ((text (test-replies--file-text)))
        (expect text :to-match "^\\* \\[\\[file:discussions.org::\\*Week 1: Introductions\\]\\[Week 1\\]\\]\n")
        (expect text :to-match "^\\* Unknown here\n"))))

  (it "uses the plain title when there is no discussions.org"
    (test-replies--with-course
        '(((id . 7) (title . "Week 1")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a"))))
        nil
      (org-canvas-pull-discussion-replies)
      (expect (test-replies--file-text) :to-match "^\\* Week 1\n")))

  (it "skips a topic whose entries it may not read, records it, and goes on"
    (test-replies--with-course
        '(((id . 7) (title . "Locked")) ((id . 8) (title . "Open")))
        `((7 . forbidden)
          (8 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a"))))
        nil
      (let ((warned nil) (said nil))
        (org-canvas--pull-summary-reset)
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) warned)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (expect (org-canvas-pull-discussion-replies) :not :to-throw))
        (expect (car (last warned)) :to-match "Skipping replies of 'Locked'")
        (expect (test-replies--file-text) :to-match "^\\* Open\n")
        (expect (test-replies--file-text) :not :to-match "Locked")
        (expect said :to-match "1 topics skipped")
        (expect (length (org-canvas--pull-summary-records)) :to-equal 1))))

  (it "skips a deleted entry stub"
    (test-replies--with-course
        '(((id . 7) (title . "T")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "a")
                ((id . 101) (deleted . t) (created_at . "2026-09-02T09:30:00Z")))))
        nil
      (org-canvas-pull-discussion-replies)
      (let ((text (test-replies--file-text)))
        (expect text :to-match "Reply by Alice")
        (expect text :not :to-match ":CANVAS_ENTRY_ID: +101"))))

  (it "converts the message through the HTML chokepoint"
    (test-replies--with-course
        '(((id . 7) (title . "T")))
        `((7 . (,(test-replies--entry 100 "Alice" "2026-09-01T14:00:00Z" "<p>Hello</p>"))))
        nil
      (cl-letf (((symbol-function 'org-canvas--html-to-org)
                 (lambda (html) (concat "converted: " html))))
        (org-canvas-pull-discussion-replies))
      (expect (test-replies--file-text) :to-match "converted: <p>Hello</p>"))))

(describe "org-canvas--reply-heading-text"
  (it "names the author and the posting time"
    (expect (org-canvas--reply-heading-text
             (test-replies--entry 1 "Alice" "2026-09-01T14:00:00Z" "x"))
            :to-match "\\`Reply by Alice (2026-09-01 [0-9]+:[0-9]+)\\'"))

  (it "falls back for a missing author and an unparseable time"
    (expect (org-canvas--reply-heading-text '((id . 1) (created_at . nil)))
            :to-equal "Reply by Unknown (undated)")))

(describe "org-canvas--reply-replace-body"
  (it "replaces only the body above the entry's own replies"
    (with-temp-org-buffer
        "* T\n** Reply by A (d)\n:PROPERTIES:\n:CANVAS_ENTRY_ID: 1\n:END:\nold body\n\n*** Reply by B (d)\n:PROPERTIES:\n:CANVAS_ENTRY_ID: 2\n:END:\nchild body\n"
      (with-html-to-org-identity
        (re-search-forward "^\\*\\* Reply by A")
        (org-back-to-heading t)
        (org-canvas--reply-replace-body "new body")
        (expect (buffer-string) :to-match "\n:END:\nnew body\n\\*\\*\\* Reply by B")
        (expect (buffer-string) :not :to-match "old body")
        (expect (buffer-string) :to-match "child body"))))

  (it "empties the body for a nil message"
    (with-temp-org-buffer "* T\n** Reply by A (d)\n:PROPERTIES:\n:CANVAS_ENTRY_ID: 1\n:END:\nold body\n"
      (re-search-forward "^\\*\\* Reply by A")
      (org-back-to-heading t)
      (org-canvas--reply-replace-body nil)
      (expect (buffer-string) :not :to-match "old body"))))

(describe "org-canvas--reply-insert-under"
  (it "starts the new heading on its own line when the parent ends without one"
    (with-temp-org-buffer "* T\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\nno newline at end"
      (let ((pos (org-canvas--reply-insert-under (point-min) 2 "Reply by A (d)")))
        (expect (buffer-substring pos (line-end-position)) :to-equal "** Reply by A (d)")
        (expect (buffer-string) :to-match "no newline at end\n[*][*] Reply by A")))))

(describe "discussion replies registration"
  (it "registers the file variable and the properties the manual documents"
    (expect (gethash "discussion-replies" org-canvas--property-registry) :not :to-be nil)
    (expect (mapcar (lambda (p) (plist-get p :org-prop))
                    (plist-get (gethash "discussion-replies" org-canvas--property-registry) :properties))
            :to-equal '("CANVAS_ENTRY_ID" "AUTHOR" "AUTHOR_ID" "POSTED_AT" "PARENT_ENTRY_ID")))

  (it "is a command, in the pull tiers after discussions"
    (expect (commandp 'org-canvas-pull-discussion-replies) :to-be t)
    (let ((tier (cl-find-if (lambda (tier) (assq 'org-canvas-pull-discussions tier))
                            org-canvas--pull-tiers)))
      (expect (assq 'org-canvas-pull-discussion-replies tier) :not :to-be nil)
      (expect (cl-position 'org-canvas-pull-discussion-replies (mapcar #'car tier))
              :to-be-greater-than
              (cl-position 'org-canvas-pull-discussions (mapcar #'car tier))))))

(provide 'org-canvas-discussion-replies-test)
;;; org-canvas-discussion-replies-test.el ends here
