;;; org-canvas-search.el --- Search the live course's text for a regexp -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-search-live' reads every text-bearing object of the live
;; course and lists each place a regexp matches, with context, and
;; `org-canvas-search-live-dates' lists every date each object says.
;;
;; A date move is a text change in several places at once: assignment
;; descriptions, module item titles, page bodies, event titles,
;; announcements, quiz questions.  The drift report compares Org
;; against Canvas and cannot notice that Org itself carries a stale
;; value in a file the move never touched: after the 2026-09-24 slide
;; of a midterm from Sep 30 to Oct 7, calendar.org still held the
;; event on Sep 30 and had been pushed that way, so students' calendars
;; said the exam was tomorrow while every other surface said Oct 7.
;; A hand-written probe that GETs every object and greps it found the
;; event in one run; this file is that probe as a command (issue #406).
;;
;; The walk is data: `org-canvas--search-surfaces' names each object
;; type the search reads, the registry entry that lists it, the fields
;; whose text is searched and the date fields shown as Org timestamps
;; in `org-canvas--time-zone', so a date typed the way Org holds it
;; matches the field as well as the prose.  Children (module items,
;; quiz questions, New Quiz items) are read under their parent.  A
;; surface a role cannot read, or a service the course has off (New
;; Quizzes), is skipped and named in the closing line, never an abort.
;; Nothing is written, locally or to Canvas.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)

(declare-function org-canvas--new-quiz-api-endpoint "org-canvas-new-quiz-items")

;;;; Surfaces

(defun org-canvas--search-module-items (module)
  "Return the items of MODULE, a module alist."
  (org-canvas-api-request-all-pages
   'GET (org-canvas-api-course-endpoint "modules/%s/items" (alist-get 'id module))))

(defun org-canvas--search-quiz-questions (quiz)
  "Return the questions of QUIZ, a classic quiz alist."
  (org-canvas-api-request-all-pages
   'GET (org-canvas-api-course-endpoint "quizzes/%s/questions" (alist-get 'id quiz))))

(defun org-canvas--search-new-quiz-items (quiz)
  "Return the items of QUIZ, a New Quiz alist, with each entry's fields lifted.
The item's text sits under `entry' in the reply; lifting `item_body'
and `title' beside the id lets the item read like any other object."
  (mapcar (lambda (item)
            (let ((entry (alist-get 'entry item)))
              (append (list (cons 'item_body (and (listp entry)
                                                  (alist-get 'item_body entry)))
                            (cons 'title (and (listp entry)
                                              (alist-get 'title entry))))
                      item)))
          (org-canvas-api-request-all-pages
           'GET (org-canvas--new-quiz-api-endpoint "quizzes/%s/items"
                                                   (alist-get 'id quiz)))))

(defun org-canvas--search-syllabus ()
  "Return the course, with its syllabus body, as a one-item list."
  (list (org-canvas-api-request
         'GET (org-canvas-api-course-endpoint "")
         :params '(("include[]" . "syllabus_body")))))

(defun org-canvas--search-full-page (page)
  "Return PAGE read in full: the list carries no body."
  (org-canvas-api-request
   'GET (org-canvas-api-course-endpoint "pages/%s" (alist-get 'url page))))

(defconst org-canvas--search-surfaces
  '((:type "assignment" :feature "Assignments" :honor-skip t
     :fields (description) :dates (due_at unlock_at lock_at))
    (:type "discussion" :feature "Discussions" :honor-skip t
     :fields (message) :dates (delayed_post_at lock_at))
    (:type "announcement" :feature "Announcements"
     :fields (message) :dates (delayed_post_at posted_at))
    (:type "page" :feature "Pages" :each org-canvas--search-full-page
     :fields (body) :dates (publish_at))
    (:type "module" :feature "Modules" :dates (unlock_at)
     :child (:type "module item" :fetch org-canvas--search-module-items
             :id-field id :title-field title))
    (:type "calendar event" :feature "Calendar Events"
     :fields (description) :dates (start_at end_at))
    (:type "quiz" :feature "Quizzes" :fields (description)
     :dates (due_at unlock_at lock_at show_correct_answers_at)
     :child (:type "quiz question" :fetch org-canvas--search-quiz-questions
             :id-field id :title-field question_name :fields (question_text)))
    (:type "new quiz" :pull-feature "New Quizzes" :fields (instructions)
     :dates (due_at unlock_at lock_at)
     :child (:type "new quiz item" :fetch org-canvas--search-new-quiz-items
             :id-field id :title-field title :fields (item_body)))
    (:type "syllabus" :fetch org-canvas--search-syllabus
     :id-field id :fields (syllabus_body)))
  "The text-bearing objects a live search reads, one plist each.
`:type' names the object in a hit.  `:feature' (or `:pull-feature')
names the registry entry whose list URL, parameters, id and title
fields list it; `:fetch' is a function of no arguments returning the
list instead.  `:honor-skip' applies the feature's `:skip-fn': a
quiz's shadow assignment and an announcement listed as a discussion
would otherwise report every hit twice.  Pages are not skipped: the
front page is the one page every student reads.  `:each' reads an
item in full before searching it, since a page list carries no body.
`:fields' are the HTML or text fields searched after the title;
`:dates' the timestamp fields rendered as Org timestamps and searched
too.  `:child' is a surface read under each parent item through its
`:fetch', a function of the parent.")

;;;; Text

(defconst org-canvas--search-entities
  '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
    ("&quot;" . "\"") ("&#39;" . "'") ("&rsquo;" . "'") ("&lsquo;" . "'")
    ("&ldquo;" . "\"") ("&rdquo;" . "\"") ("&ndash;" . "-") ("&mdash;" . "-"))
  "HTML entities folded to text before a match, and their replacements.")

(defconst org-canvas--search-block-tag-regexp
  (concat "</?\\(?:p\\|div\\|br\\|li\\|ul\\|ol\\|h[1-6]\\|tr\\|td\\|th\\|table"
          "\\|blockquote\\|pre\\|hr\\|section\\|article\\|header\\|footer\\)\\b[^>]*>")
  "Match an HTML tag that starts or ends a block of text.")

(defun org-canvas--search-strip-html (html)
  "Return HTML as plain text: tags gone, common entities folded, spaces squeezed.
A block tag becomes a space so words on either side of it stay apart;
an inline tag vanishes, so \"<b>midterm</b>?\" keeps its question mark
beside the word.  Nil or a non-string is the empty string."
  (if (not (stringp html))
      ""
    (let* ((text (replace-regexp-in-string org-canvas--search-block-tag-regexp " " html))
           (text (replace-regexp-in-string "<[^>]*>" "" text)))
      (dolist (entity org-canvas--search-entities)
        (setq text (replace-regexp-in-string (regexp-quote (car entity))
                                             (cdr entity) text t t)))
      (setq text (replace-regexp-in-string
                  "&#\\([0-9]+\\);"
                  (lambda (m) (string (string-to-number (match-string 1 m))))
                  text t t))
      (string-trim (replace-regexp-in-string "[ \t\n\r\u00a0]+" " " text)))))

(defun org-canvas--search-matches (regexp text &optional whole)
  "Return each match of REGEXP in TEXT as a context string.
The context is the match with up to sixty characters either side,
marked with an ellipsis where TEXT continues; with WHOLE non-nil the
match alone.  Case is folded."
  (let ((case-fold-search t) (start 0) (hits nil))
    (while (and (< start (length text)) (string-match regexp text start))
      (let* ((b (match-beginning 0)) (e (match-end 0))
             (cb (max 0 (- b 60))) (ce (min (length text) (+ e 60))))
        (push (if whole
                  (substring text b e)
                (concat (if (> cb 0) "..." "") (substring text cb ce)
                        (if (< ce (length text)) "..." "")))
              hits)
        (setq start (if (= e b) (1+ e) e))))
    (nreverse hits)))

(defun org-canvas--search-item-id (surface item)
  "Return ITEM's id under SURFACE, as a string, or nil.
The id field may be a list of fields tried in order, as a New Quiz
declares `(assignment_id id)' (issue #309)."
  (let ((fields (plist-get surface :id-field)))
    (cl-some (lambda (field)
               (let ((v (org-canvas--alist-get-non-null field item)))
                 (and v (format "%s" v))))
             (if (listp fields) fields (list fields)))))

(defun org-canvas--search-item-fields (surface item)
  "Return the (FIELD . TEXT) pairs to search in ITEM, an object of SURFACE.
The title first, then each of `:fields' stripped of HTML, then each of
`:dates' as an Org timestamp; a field ITEM lacks is left out."
  (let ((title-field (plist-get surface :title-field))
        (pairs nil))
    (when title-field
      (let ((title (org-canvas--alist-get-non-null title-field item)))
        (when (stringp title)
          (push (cons title-field title) pairs))))
    (dolist (field (plist-get surface :fields))
      (let ((text (org-canvas--search-strip-html
                   (org-canvas--alist-get-non-null field item))))
        (unless (string-empty-p text)
          (push (cons field text) pairs))))
    (dolist (field (plist-get surface :dates))
      (let ((stamp (org-canvas--iso8601-to-org-timestamp
                    (org-canvas--alist-get-non-null field item))))
        (when stamp
          (push (cons field stamp) pairs))))
    (nreverse pairs)))

(defun org-canvas--search-item-hits (regexp surface item whole)
  "Return the hits of REGEXP in ITEM, an object of SURFACE.
Each hit is a plist: :type, :id and :title of the object, :field the
field the text came from, and :context, the match in context (the
match alone with WHOLE non-nil)."
  (let* ((title-field (plist-get surface :title-field))
         (title (and title-field
                     (org-canvas--alist-get-non-null title-field item)))
         (id (org-canvas--search-item-id surface item))
         (hits nil))
    (dolist (pair (org-canvas--search-item-fields surface item))
      (dolist (context (org-canvas--search-matches regexp (cdr pair) whole))
        (push (list :type (plist-get surface :type) :id id
                    :title (and (stringp title) title)
                    :field (symbol-name (car pair)) :context context)
              hits)))
    (nreverse hits)))

;;;; The walk

(defun org-canvas--search-surface-entry (surface)
  "Return the registry entry SURFACE lists through, or nil for a `:fetch'."
  (let ((feature (plist-get surface :feature))
        (pull (plist-get surface :pull-feature)))
    (cond
     (feature (org-canvas--registry-find-feature feature))
     (pull (cl-find pull org-canvas--pull-feature-registry
                    :key (lambda (f) (plist-get f :name)) :test #'string=)))))

(defun org-canvas--search-surface-items (surface)
  "Return the objects of SURFACE, each read in full where it asks.
A feature-listed surface takes its id and title fields from the
registry entry, and its `:honor-skip' items are dropped."
  (let* ((entry (org-canvas--search-surface-entry surface))
         (fetch (plist-get surface :fetch))
         (each (plist-get surface :each))
         (skip (and (plist-get surface :honor-skip) (plist-get entry :skip-fn)))
         (items (if fetch
                    (funcall fetch)
                  (org-canvas-api-request-all-pages
                   'GET (org-canvas--feature-list-url entry)
                   (org-canvas--feature-list-params entry)))))
    (when skip
      (setq items (cl-remove-if skip items)))
    (if each (mapcar each items) items)))

(defun org-canvas--search-surface-spec (surface)
  "Return SURFACE with the id and title fields of its registry entry filled in."
  (let ((entry (org-canvas--search-surface-entry surface)))
    (if entry
        (append (list :id-field (plist-get entry :id-field)
                      :title-field (plist-get entry :title-field))
                surface)
      surface)))

(defun org-canvas--search-plural (type)
  "Return TYPE, an object type, in the plural."
  (if (string-suffix-p "quiz" type)
      (concat type "zes")
    (concat type "s")))

(defun org-canvas--search-note-skip (state label err)
  "Record on STATE that LABEL could not be read, ERR being the error.
The reason is logged in full and kept, redacted, for the closing line."
  (let ((msg (org-canvas--log-redact (error-message-string err))))
    (org-canvas--log-warning org-canvas--logger
      "[Search] %s: skipped, %s" label msg)
    (plist-put state :skipped
               (append (plist-get state :skipped) (list (cons label msg))))))

(defun org-canvas--search-children (regexp surface item state whole)
  "Search the children of ITEM under SURFACE for REGEXP, adding to STATE.
WHOLE is passed on.  A child list that cannot be read is skipped and
named, with the parent's id, and never stops the walk."
  (let ((child (plist-get surface :child)))
    (when child
      (condition-case err
          (dolist (kid (funcall (plist-get child :fetch) item))
            (org-canvas--search-one regexp child kid state whole))
        (error
         (org-canvas--search-note-skip
          state (format "%s of %s %s"
                        (org-canvas--search-plural (plist-get child :type))
                        (plist-get surface :type)
                        (org-canvas--search-item-id surface item))
          err))))))

(defun org-canvas--search-one (regexp surface item state whole)
  "Search ITEM, an object of SURFACE, for REGEXP, adding hits to STATE.
Then its children, if SURFACE has any.  WHOLE is passed on."
  (plist-put state :objects (1+ (plist-get state :objects)))
  (plist-put state :hits
             (append (plist-get state :hits)
                     (org-canvas--search-item-hits regexp surface item whole)))
  (org-canvas--search-children regexp surface item state whole))

(defun org-canvas--search-surface (regexp surface state whole)
  "Search every object of SURFACE for REGEXP, adding to STATE.
WHOLE is passed on.  A surface that cannot be listed — a role
refusal, a service the course has off — is skipped and named, never
an abort (issue #155)."
  (let ((surface (org-canvas--search-surface-spec surface)))
    (message "Searching %s..."
             (org-canvas--search-plural (plist-get surface :type)))
    (condition-case err
        (dolist (item (org-canvas--search-surface-items surface))
          (org-canvas--search-one regexp surface item state whole))
      (error
       (org-canvas--search-note-skip
        state (org-canvas--search-plural (plist-get surface :type)) err)))))

(defun org-canvas--search-live (regexp &optional whole)
  "Search the live course's text for REGEXP; read-only.
Reads every surface of `org-canvas--search-surfaces' and returns a
plist: :hits, the list of hit plists (see `org-canvas--search-item-hits')
in the order found; :objects, how many objects were searched; and
:skipped, an alist of (LABEL . REASON) for the surfaces and child
lists that could not be read.  With WHOLE non-nil each hit's context
is the match alone.  Callable from a script: it prompts for nothing
and writes nothing."
  (org-canvas--ensure-credentials)
  (let ((state (list :hits nil :objects 0 :skipped nil)))
    (dolist (surface org-canvas--search-surfaces)
      (org-canvas--search-surface regexp surface state whole))
    state))

;;;; The date flavour

(defconst org-canvas--search-date-regexp
  (concat "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
          "\\|\\b\\(?:Jan\\(?:uary\\)?\\|Feb\\(?:ruary\\)?\\|Mar\\(?:ch\\)?\\|Apr\\(?:il\\)?"
          "\\|May\\|June?\\|July?\\|Aug\\(?:ust\\)?\\|Sept?\\(?:ember\\)?\\|Oct\\(?:ober\\)?"
          "\\|Nov\\(?:ember\\)?\\|Dec\\(?:ember\\)?\\)"
          "\\.? [0-9]\\{1,2\\}\\(?:st\\|nd\\|rd\\|th\\)?\\b"
          "\\|\\b[0-9]\\{1,2\\}/[0-9]\\{1,2\\}\\(?:/[0-9]\\{2,4\\}\\)?\\b")
  "Match a date the way prose and Canvas write one.
An ISO date (which an Org timestamp begins with), a month name or its
abbreviation followed by a day, or a numeric month/day.")

;;;; The report

(defun org-canvas--search-hit-heading (hit)
  "Return the line naming the object HIT is in."
  (concat (plist-get hit :type)
          (if (plist-get hit :id) (format " %s" (plist-get hit :id)) "")
          (if (plist-get hit :title) (format " \"%s\"" (plist-get hit :title)) "")))

(defun org-canvas--search-insert-hits (hits)
  "Insert HITS, one heading per object and one line per hit under it."
  (let ((last nil))
    (dolist (hit hits)
      (let ((heading (org-canvas--search-hit-heading hit)))
        (unless (equal heading last)
          (insert heading "\n")
          (setq last heading))
        (insert (format "  %s: %s\n" (plist-get hit :field)
                        (plist-get hit :context)))))))

(defun org-canvas--search-render (title result)
  "Insert the report of RESULT, a `org-canvas--search-live' plist, under TITLE."
  (let ((hits (plist-get result :hits))
        (skipped (plist-get result :skipped)))
    (insert title "\n")
    (insert (format "Course: %s | %s\n" org-canvas-course-id org-canvas-base-url))
    (insert (make-string 60 ?=) "\n\n")
    (org-canvas--search-insert-hits hits)
    (when hits (insert "\n"))
    (insert (format "%d hit(s) in %d object(s); %d object(s) searched.\n"
                    (length hits)
                    (length (cl-remove-duplicates
                             (mapcar #'org-canvas--search-hit-heading hits)
                             :test #'equal))
                    (plist-get result :objects)))
    (when skipped
      (insert (format "Skipped %d: %s\n" (length skipped)
                      (mapconcat (lambda (s) (format "%s (%s)" (car s) (cdr s)))
                                 skipped "; "))))))

(defun org-canvas--search-report (title result)
  "Show RESULT under TITLE in *canvas-search* and return the hit count.
Under `noninteractive' the report is printed instead (issue #169)."
  (org-canvas--report-display
   "*canvas-search*"
   (lambda () (org-canvas--search-render title result))
   #'special-mode)
  (let ((n (length (plist-get result :hits))))
    (message "%d hit(s) in %d object(s) on Canvas%s" n
             (plist-get result :objects)
             (if (plist-get result :skipped)
                 (format ", %d surface(s) skipped" (length (plist-get result :skipped)))
               ""))
    n))

;;;###autoload
(defun org-canvas-search-live (regexp)
  "Search the live course's text for REGEXP and report every match.
Reads every text-bearing object on Canvas — assignments, discussions,
announcements, pages, modules and their items, calendar events, quizzes
and their questions, New Quizzes and their items, the syllabus — and
lists each match with its object, its field and the text around it.
A date field is searched as the Org timestamp a pull would write, so
\"09-30\" finds an event dated September 30 as well as prose saying so.
Case is folded.

The audit after a date move: the drift report compares Org against
Canvas and cannot see a stale value both sides hold, which is how a
calendar event kept a midterm on its old date (issue #406).

Reads only; a surface the role cannot read or the course has off is
skipped and named.  Returns the number of hits, and prints the report
under `noninteractive' for a script."
  (interactive "sSearch the live course for regexp: ")
  (org-canvas--start-operation (format "SEARCHING CANVAS FOR %S" regexp))
  (org-canvas--search-report
   (format "org-canvas live search: %S" regexp)
   (org-canvas--search-live regexp)))

;;;###autoload
(defun org-canvas-search-live-dates ()
  "List every date the live course's text and date fields say.
An ISO date, a month name and day (\"Sep 30\", \"October 7th\") or a
numeric month/day (\"9/30\") in any field `org-canvas-search-live'
reads, and every date field as an Org timestamp, grouped by object.
One call to read every date a student can see after a date move
\(issue #406).  Reads only; returns the number of dates found."
  (interactive)
  (org-canvas--start-operation "LISTING EVERY DATE ON CANVAS")
  (org-canvas--search-report
   "org-canvas live search: every date"
   (org-canvas--search-live org-canvas--search-date-regexp t)))

(provide 'org-canvas-search)
;;; org-canvas-search.el ends here
