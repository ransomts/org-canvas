;;; org-canvas-search-test.el --- Tests for the live course search  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-search-live' greps every text-bearing object of the live
;; course (issue #406).  Both request functions are stubbed with a
;; dispatcher keyed on the URL, so each spec says what Canvas holds and
;; the walk is exercised end to end without a network.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defconst test-search--tz "EST5EDT,M3.2.0,M11.1.0"
  "The course zone the specs render date fields in.
Bound as `org-canvas-time-zone', the user's pin, rather than the
resolved cache: the command's start-of-operation reset clears the
cache, and a CI runner's local zone is UTC.")

(defun test-search--course ()
  "Return the objects a small course holds, keyed by URL fragment.
Each key is matched against the request URL with `string-match-p';
the first match wins, so the more specific keys come first."
  `(("modules/1/items" . (((id . 11) (title . "Midterm review sheet") (type . "Page"))
                          ((id . 12) (title . "--- Week 06 ---") (type . "SubHeader"))))
    ("quizzes/5/questions" . (((id . 51) (question_name . "Q1")
                               (question_text . "<p>When is the <b>midterm</b>?</p>"))))
    ("api/quiz/v1/courses/[^/]+/quizzes/7/items"
     . (((id . 71) (entry . ((item_body . "<p>Midterm is Oct 7.</p>") (title . "Item 1"))))))
    ("api/quiz/v1/courses/[^/]+/quizzes"
     . (((id . 7) (assignment_id . 700) (title . "Practice midterm")
         (instructions . "<p>Practice.</p>"))))
    ("pages/syllabus" . ((url . "syllabus") (title . "Syllabus")
                         (body . "<p>The midterm is Sep&nbsp;30.</p>")))
    ("pages/other" . ((url . "other") (title . "Other") (body . "<p>Nothing.</p>")))
    ("pages" . (((url . "syllabus") (title . "Syllabus"))
                ((url . "other") (title . "Other"))))
    ("assignments" . (((id . 1) (name . "Midterm Exam")
                       (description . "<p>In class.</p>")
                       (due_at . "2026-10-01T03:59:00Z"))
                      ((id . 2) (name . "Shadow of a quiz") (quiz_id . 5)
                       (description . "<p>midterm midterm</p>"))))
    ("only_announcements\\|discussion_topics.*announcements"
     . (((id . 31) (title . "Midterm moved") (message . "<p>Now Oct 7.</p>")
         (posted_at . "2026-09-24T14:00:00Z"))))
    ("discussion_topics" . (((id . 21) (title . "Intro") (message . "<p>Hello.</p>"))
                            ((id . 31) (title . "Midterm moved") (is_announcement . t)
                             (message . "<p>Now Oct 7.</p>"))))
    ("modules" . (((id . 1) (name . "Week 06: Midterm") (unlock_at . :null))))
    ("calendar_events" . (((id . 41) (title . "Midterm")
                           (description . :null)
                           (start_at . "2026-09-30T12:00:00Z")
                           (end_at . "2026-09-30T13:15:00Z"))))
    ("quizzes" . (((id . 5) (title . "Midterm quiz") (description . "")
                   (due_at . "2026-10-01T03:59:00Z"))))
    ("courses/[^/?]+/?\\?" . ((id . 42) (name . "Ethics")
                                            (syllabus_body . "<p>Midterm: Sep 30 or 9/30.</p>")))))

(defun test-search--lookup (course url)
  "Return what COURSE holds at URL, or signal a 404."
  (let ((entry (cl-find-if (lambda (e) (string-match-p (car e) url)) course)))
    (unless entry
      (signal 'org-canvas-api-error (list (format "404 for %s" url))))
    (if (functionp (cdr entry)) (funcall (cdr entry)) (cdr entry))))

(defun test-search--course-without-new-quizzes ()
  "Return the course with the New Quizzes service refusing every read."
  (cons (cons "api/quiz/v1"
              (lambda () (signal 'org-canvas-permission-error '("403 Forbidden"))))
        (test-search--course)))

(defmacro test-search--with-course (course &rest body)
  "Run BODY with both request functions answering from COURSE.
The calendar's context code and the announcements' parameter reach the
URL through PARAMS, which the dispatcher appends before matching, so a
key can name them."
  (declare (indent 1))
  `(with-org-canvas-test-config
     (let ((org-canvas-time-zone test-search--tz)
           (course ,course))
       (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                  (lambda (_method url &optional params)
                    (test-search--lookup
                     course (concat url "?" (mapconcat (lambda (p) (format "%s=%s" (car p) (cdr p)))
                                                       params "&")))))
                 ((symbol-function 'org-canvas-api-request)
                  (lambda (_method url &rest args)
                    (let ((params (plist-get args :params)))
                      (test-search--lookup
                       course (concat url "?" (mapconcat (lambda (p) (format "%s=%s" (car p) (cdr p)))
                                                         params "&"))))))
                 ((symbol-function 'display-buffer) #'ignore)
                 ((symbol-function 'message) #'ignore))
         ,@body))))

(defun test-search--hit-lines (result)
  "Return RESULT's hits as \"type id field: context\" strings."
  (mapcar (lambda (hit)
            (let ((type (plist-get hit :type)))
              (format "%s %s %s: %s" type (plist-get hit :id)
                      (plist-get hit :field) (plist-get hit :context))))
          (plist-get result :hits)))

(describe "org-canvas--search-strip-html (issue #406)"
  (it "drops tags, folds entities and squeezes whitespace"
    (expect (org-canvas--search-strip-html
             "<p>The <b>midterm</b>&nbsp;is\n  Sep&#160;30 &amp; done&#39;</p>")
            :to-equal "The midterm is Sep 30 & done'"))

  (it "keeps words on either side of a tag apart"
    (expect (org-canvas--search-strip-html "<li>one</li><li>two</li>")
            :to-equal "one two"))

  (it "is the empty string for nil, :null or a number"
    (expect (org-canvas--search-strip-html nil) :to-equal "")
    (expect (org-canvas--search-strip-html :null) :to-equal "")
    (expect (org-canvas--search-strip-html 7) :to-equal "")))

(describe "org-canvas--search-matches (issue #406)"
  (it "returns each match in context, marking where the text continues"
    (let ((text (concat (make-string 70 ?a) " midterm " (make-string 70 ?b)
                        " MIDTERM end")))
      (let ((hits (org-canvas--search-matches "midterm" text)))
        (expect (length hits) :to-equal 2)
        (expect (car hits) :to-match "\\`\\.\\.\\.a\\{59\\} midterm b\\{59\\}\\.\\.\\.\\'")
        (expect (cadr hits) :to-match "MIDTERM end\\'")
        (expect (cadr hits) :not :to-match "\\.\\.\\.\\'"))))

  (it "returns the match alone with WHOLE"
    (expect (org-canvas--search-matches "[0-9]+" "on 30 or 7" t)
            :to-equal '("30" "7")))

  (it "does not loop on an empty match"
    (expect (org-canvas--search-matches "x*" "ab" t) :to-equal '("" ""))))

(describe "org-canvas--search-item-fields (issue #406)"
  (it "reads the title, the stripped fields and the dates as Org timestamps"
    (let ((org-canvas-time-zone test-search--tz))
      (expect (org-canvas--search-item-fields
               '(:title-field name :fields (description) :dates (due_at lock_at))
               '((name . "Midterm") (description . "<p>In class</p>")
                 (due_at . "2026-10-01T03:59:00Z") (lock_at . :null)))
              :to-equal '((name . "Midterm") (description . "In class")
                          (due_at . "<2026-09-30 Wed 23:59>")))))

  (it "leaves out an empty field and a missing title"
    (expect (org-canvas--search-item-fields
             '(:title-field name :fields (description))
             '((description . "<p></p>")))
            :to-equal nil)))

(describe "org-canvas--search-item-id (issue #406)"
  (it "takes the first of several id fields that is set"
    (expect (org-canvas--search-item-id '(:id-field (assignment_id id))
                                        '((id . 7) (assignment_id . :null)))
            :to-equal "7")
    (expect (org-canvas--search-item-id '(:id-field (assignment_id id))
                                        '((id . 7) (assignment_id . 700)))
            :to-equal "700")
    (expect (org-canvas--search-item-id '(:id-field id) '((url . "x"))) :to-be nil)))

(describe "org-canvas--search-live (issue #406)"
  (it "finds a regexp on every surface, children and date fields included"
    (test-search--with-course (test-search--course)
      (let* ((result (org-canvas--search-live "midterm\\|09-30"))
             (lines (test-search--hit-lines result)))
        (expect lines :to-contain "assignment 1 name: Midterm Exam")
        (expect lines :to-contain "announcement 31 title: Midterm moved")
        (expect lines :to-contain "page syllabus body: The midterm is Sep 30.")
        (expect lines :to-contain "module 1 name: Week 06: Midterm")
        (expect lines :to-contain "module item 11 title: Midterm review sheet")
        (expect lines :to-contain "calendar event 41 title: Midterm")
        (expect lines :to-contain "calendar event 41 start_at: <2026-09-30 Wed 08:00>")
        (expect lines :to-contain "calendar event 41 end_at: <2026-09-30 Wed 09:15>")
        (expect lines :to-contain "quiz 5 title: Midterm quiz")
        (expect lines :to-contain "quiz question 51 question_text: When is the midterm?")
        (expect lines :to-contain "new quiz 700 title: Practice midterm")
        (expect lines :to-contain "new quiz item 71 item_body: Midterm is Oct 7.")
        (expect lines :to-contain "syllabus 42 syllabus_body: Midterm: Sep 30 or 9/30.")
        (expect (plist-get result :skipped) :to-be nil))))

  (it "skips a quiz's shadow assignment and an announcement listed as a discussion"
    (test-search--with-course (test-search--course)
      (let ((lines (test-search--hit-lines (org-canvas--search-live "midterm"))))
        (expect (cl-remove-if-not (lambda (l) (string-prefix-p "assignment 2" l)) lines)
                :to-be nil)
        (expect (cl-remove-if-not (lambda (l) (string-prefix-p "discussion 31" l)) lines)
                :to-be nil)
        (expect (cl-count-if (lambda (l) (string-prefix-p "announcement 31 title" l)) lines)
                :to-equal 1))))

  (it "counts every object searched, the front page and children included"
    (test-search--with-course (test-search--course)
      ;; 1 assignment (the shadow dropped), 1 discussion, 1 announcement,
      ;; 2 pages, 1 module + 2 items, 1 event, 1 quiz + 1 question,
      ;; 1 New Quiz + 1 item, the syllabus.
      (expect (plist-get (org-canvas--search-live "zzz") :objects) :to-equal 14)
      (expect (plist-get (org-canvas--search-live "zzz") :hits) :to-be nil)))

  (it "skips a surface it cannot list, names it, and goes on"
    (test-search--with-course (test-search--course-without-new-quizzes)
      (let* ((warned nil)
             (result (cl-letf (((symbol-function 'org-canvas--log-warning)
                                (lambda (_l fmt &rest args)
                                  (push (apply #'format fmt args) warned))))
                       (org-canvas--search-live "midterm")))
             (skipped (plist-get result :skipped)))
        (expect (length skipped) :to-equal 1)
        (expect (caar skipped) :to-equal "new quizzes")
        (expect (cdar skipped) :to-match "403 Forbidden")
        (expect warned :to-contain (format "[Search] new quizzes: skipped, %s" (cdar skipped)))
        (expect (test-search--hit-lines result)
                :to-contain "syllabus 42 syllabus_body: Midterm: Sep 30 or 9/30."))))

  (it "skips a child list it cannot read, naming the parent"
    (test-search--with-course (cons (cons "quizzes/5/questions"
                                          (lambda () (signal 'org-canvas-api-error '("404"))))
                                    (test-search--course))
      (let ((result (org-canvas--search-live "midterm")))
        (expect (mapcar #'car (plist-get result :skipped))
                :to-equal '("quiz questions of quiz 5"))
        (expect (test-search--hit-lines result) :to-contain "quiz 5 title: Midterm quiz"))))

  (it "refuses without credentials, before any read"
    (with-org-canvas-test-config
      (let ((org-canvas-api-token nil) (read nil))
        (cl-letf (((symbol-function 'org-canvas--api-token) (lambda () nil))
                  ((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (&rest _) (setq read t) nil)))
          (expect (org-canvas--search-live "x") :to-throw 'org-canvas-credentials-error)
          (expect read :to-be nil))))))

(describe "org-canvas-search-live (issue #406)"
  (it "renders the hits by object into *canvas-search* and returns their count"
    (test-search--with-course (test-search--course)
      (let ((n (org-canvas-search-live "midterm")))
        (expect n :to-equal 11)
        (with-current-buffer "*canvas-search*"
          (expect major-mode :to-be 'special-mode)
          (expect (buffer-string) :to-match "\\`org-canvas live search: \"midterm\"\n")
          (expect (buffer-string)
                  :to-match "calendar event 41 \"Midterm\"\n  title: Midterm\n")
          (expect (buffer-string)
                  :to-match "page syllabus \"Syllabus\"\n  body: The midterm is Sep 30\\.\n")
          (expect (buffer-string) :to-match "11 hit(s) in 11 object(s); 14 object(s) searched\\.")
          (expect (buffer-string) :not :to-match "Skipped")))))

  (it "names what it skipped in the closing line and the echo area"
    (test-search--with-course (test-search--course-without-new-quizzes)
      (let ((said nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (org-canvas-search-live "midterm"))
        (expect said :to-match "\\`9 hit(s) in 12 object(s) on Canvas, 1 surface(s) skipped\\'")
        (with-current-buffer "*canvas-search*"
          (expect (buffer-string) :to-match "Skipped 1: new quizzes (.*403 Forbidden")))))

  (it "says so when nothing matches"
    (test-search--with-course (test-search--course)
      (expect (org-canvas-search-live "zzz") :to-equal 0)
      (with-current-buffer "*canvas-search*"
        (expect (buffer-string) :to-match "0 hit(s) in 0 object(s); 14 object(s) searched\\."))))

  (it "prints the report under noninteractive"
    (test-search--with-course (test-search--course)
      (let ((out (with-output-to-string
                   (let ((noninteractive t))
                     (org-canvas-search-live "syllabus_body")))))
        (expect out :to-match "0 hit(s)"))))

  (it "reads its regexp from the minibuffer"
    ;; A string spec, not a form: a form in `interactive' keeps the
    ;; coverage instrumentation out of the body.
    (expect (cadr (interactive-form 'org-canvas-search-live))
            :to-match "\\`s.*regexp"))

  (it "is bound in the transient's Tools group beside the date flavour"
    (let ((source (with-temp-buffer
                    (insert-file-contents (locate-library "org-canvas-transient.el"))
                    (buffer-string))))
      (expect source :to-match "\"/\" \".*\" org-canvas-search-live)")
      (expect source :to-match "\"%\" \".*\" org-canvas-search-live-dates)"))))

(describe "org-canvas-search-live-dates (issue #406)"
  (it "lists every ISO date, month-and-day, numeric date and date field per object"
    (test-search--with-course (test-search--course)
      (let ((n (org-canvas-search-live-dates)))
        (with-current-buffer "*canvas-search*"
          (let ((text (buffer-string)))
            (expect text :to-match "\\`org-canvas live search: every date\n")
            (expect text :to-match "syllabus 42\n  syllabus_body: Sep 30\n  syllabus_body: 9/30\n")
            (expect text :to-match "new quiz item 71 \"Item 1\"\n  item_body: Oct 7\n")
            (expect text :to-match "calendar event 41 \"Midterm\"\n  start_at: 2026-09-30\n  end_at: 2026-09-30\n")
            (expect text :to-match "assignment 1 \"Midterm Exam\"\n  due_at: 2026-09-30\n")
            (expect text :to-match "announcement 31 \"Midterm moved\"\n  message: Oct 7\n  posted_at: 2026-09-24\n")))
        (expect n :to-equal 10))))

  (it "matches an ordinal day and a full month name, but not a bare number"
    (let ((hits (org-canvas--search-matches
                 org-canvas--search-date-regexp
                 "October 7th, Sept. 30 and 12/25/2026; 30 alone; 2026-10-07T03" t)))
      (expect hits :to-equal '("October 7th" "Sept. 30" "12/25/2026" "2026-10-07")))))

(provide 'org-canvas-search-test)
;;; org-canvas-search-test.el ends here
