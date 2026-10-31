;;; org-canvas-diff-items.el --- Drift report's child item passes  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The drift report's children: the passes that compare what a
;; feature's own entries cannot, because Canvas holds it one level
;; down.  The items inside each module (#177), a New Quiz's items at the
;; quiz service (#322), and the overrides table of each assignment
;; (#411).
;;
;; `org-canvas--diff-children-fns' names each pass by feature, and
;; `org-canvas--diff-feature' runs the one its feature registers,
;; reporting the result right after the feature's own rows.  A pass
;; reads, pairs by content and never writes; the sync owns every
;; adoption it describes.

;;; Code:

(require 'org-canvas-core)
(require 'org-canvas-diff)
(require 'org-canvas-diff-fields)
(require 'cl-lib)

;; A module's items are read and paired with the sync's own parser and
;; content predicate, and an override with the sections module's table
;; readers; a New Quiz's items through New Quizzes' own sub-module.
;; Declared, never required: those are feature modules, and `org-canvas'
;; loads them.
(declare-function org-canvas--module-item-parse-entry "org-canvas-modules"
                  (modules-file-dir))
(declare-function org-canvas--module-item-same-content-p "org-canvas-modules"
                  (data item))
(declare-function org-canvas--new-quiz-api-endpoint "org-canvas-new-quiz-items"
                  (suffix &rest args))
(declare-function org-canvas--new-quiz-item-twin-p "org-canvas-new-quiz-items"
                  (data item))
(declare-function org-canvas--new-quiz-item-remote-title "org-canvas-new-quiz-items"
                  (item))
(declare-function org-canvas--new-quiz-item-kind "org-canvas-new-quiz-items"
                  (item))
(declare-function org-canvas--new-quiz-item-read-only-title
                  "org-canvas-new-quiz-items" (item kind))
(declare-function org-canvas--new-quiz-item-heading-kind
                  "org-canvas-new-quiz-items" (&optional pom))
(declare-function org-canvas--override-find-table "org-canvas-sections" (end))
(declare-function org-canvas--override-parse-table "org-canvas-sections"
                  (table source-dir))
(declare-function org-canvas--override-find-existing "org-canvas-sections"
                  (override existing))
(declare-function org-canvas--override-same-time-p "org-canvas-sections"
                  (ours theirs))
(declare-function org-canvas--override-target-field "org-canvas-sections"
                  (override))
(declare-function org-canvas--override-target-cell "org-canvas-sections" (ov))
(declare-function org-canvas--override-format-cell "org-canvas-sections"
                  (iso8601))
(declare-function org-canvas--override-row-redundant-p "org-canvas-sections"
                  (ov parent-due parent-unlock parent-lock))

;;;; Module Items (issue #177)
;;
;; Modules are the one feature with children the report skipped.  A
;; sync that had lost an item's stamp created a second copy of the same
;; assignment in the module (issue #179); students saw it twice, and
;; the report called Modules clean, since it compared module ids and
;; never looked inside one.  The child pass below lists the items of
;; every module the file claims — one request per module, the same
;; shape as the sync's own reconcile — and reports an item no level-2
;; heading anywhere in modules.org claims as EXTRA, in a section of its
;; own named "Module Items": an item id and a module id are different
;; things, so an acknowledgment in `org-canvas-diff-known-extras' is
;; filed under "module-items", and the row's delete goes to the item's
;; own URL.  An item another module's heading claims is a pending move
;; the next sync settles (issue #105), not an extra.
;;
;; An unclaimed item is then paired with an unstamped item heading,
;; one to one (issue #294), the way the sync would adopt it (issue
;; #299): the heading is read by the sync's own parser, which resolves
;; its link offline to a type and a content id — a lookup in the linked
;; Org file, no request, nothing stamped — and the sync's own predicate
;; says whether the Canvas item holds that content.  A same-titled item
;; linking different content is therefore left an EXTRA, and the
;; heading a PENDING create, which is what the sync does with them.
;; Only a heading whose link target has no Canvas id yet falls back to
;; the title — the sync's predicate does too — and its row says it was
;; paired by title only.  That title is the link's description, what
;; Canvas calls the item, never the markup.  A heading in the item's
;; own module makes
;; the pair UNCLAIMED (the next sync adopts the twin, #179); one under
;; another module makes it MOVED — the item was carried to a new week
;; in Org without its stamp, and the next sync creates it there and
;; leaves this copy.  A SubHeader is paired only within its module: its
;; title is all it has, and "Readings" recurs in every week by design.
;; The unstamped headings left over are PENDING creates.
;;
;; A MOVED row names the heading it paired with, and `s' stamps the
;; item's id there (issue #342).  The sync cannot move an item: once
;; stamped, the heading's id is not in its module, so the next sync
;; creates the item there with a new id and deletes the old copy (issue
;; #105).  That stamped heading is a RELOCATE row, counted apart like a
;; PENDING one, since the report is the only warning that the id
;; changes (issue #343).

(defconst org-canvas--diff-children-fns
  '(("modules" . org-canvas--diff-module-items)
    ("newquizzes" . org-canvas--diff-new-quiz-items)
    ("assignments" . org-canvas--diff-assignment-overrides))
  "Child checks, by normalized feature name.
Each a function of (ITEMS LOCAL FILE) — the feature's remote list,
the local entries `org-canvas--diff-feature' collected, and the file
they came from — returning a result plist of its own, reported right
after the feature's.")

(defun org-canvas--diff-children (name items local file)
  "Run feature NAME's child check, if any, over ITEMS, LOCAL and FILE."
  (when-let* ((fn (alist-get (org-canvas--diff-normalize-name name)
                             org-canvas--diff-children-fns
                             nil nil #'string=)))
    (funcall fn items local file)))

(defun org-canvas--diff-module-item-lists (modules)
  "Return (MODULE . ITEMS) for each of MODULES, (ID . NAME) pairs.
One list request per module."
  (mapcar (lambda (module)
            (cons module
                  (append (org-canvas-api-request-all-pages
                           'GET (org-canvas-api-course-endpoint
                                 "modules/%s/items" (car module)))
                          nil)))
          modules))

(defun org-canvas--diff-module-item-extras (lists claimed)
  "Return the items in LISTS no heading CLAIMED, as `extra' entries.
LISTS is what `org-canvas--diff-module-item-lists' returned; each
entry names the module it sits in and carries the module id, which is
what deleting it needs, and the item's type and content, which pairing
needs."
  (let ((extra nil))
    (dolist (pair lists)
      (dolist (item (cdr pair))
        (unless (member (format "%s" (alist-get 'id item)) claimed)
          (push (list :kind 'extra
                      :title (format "%s" (or (alist-get 'title item) "?"))
                      :id (format "%s" (alist-get 'id item))
                      :html-url (org-canvas--diff-normalize-remote
                                 (alist-get 'html_url item))
                      :module-id (car (car pair))
                      :where (cdr (car pair))
                      :type (alist-get 'type item)
                      :content-id (alist-get 'content_id item)
                      :page-url (alist-get 'page_url item))
                extra))))
    (nreverse extra)))

(defun org-canvas--diff-module-item-content (dir)
  "Return the sync's reading of the item heading at point, or nil.
DIR is the directory of modules.org, which the heading's link is
relative to.  The heading is read by the sync's own parser,
`org-canvas--module-item-parse-entry': a :type, and a :content-id or
:page-url when the link resolves — looked up in the linked Org file,
offline — or neither when its target has no Canvas id yet (issue
#299).  The parser logs as a sync stage would, so the logger is off
while it runs; a report is not a sync.  Nil when the parser is not
loaded or fails, and the pairing then goes by title alone."
  (let ((org-canvas--logger nil))
    (save-excursion
      (ignore-errors (org-canvas--module-item-parse-entry dir)))))

(defun org-canvas--diff-module-item-headings (file)
  "Return the level-2 headings of modules FILE as local entries.
Each is an `org-canvas--diff-local-entry' plist with the module it sits
under added: :module-id, that heading's CANVAS_ID, :where, its title,
and :content, what it links (`org-canvas--diff-module-item-content').
No markers: nothing here is read again."
  (when (and file (file-exists-p file))
    (let ((dir (file-name-directory (expand-file-name file))))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((parent (save-excursion
                           (when (org-up-heading-safe)
                             (cons (org-entry-get (point) "CANVAS_ID")
                                   (org-get-heading t t t t))))))
             (append (org-canvas--diff-local-entry "CANVAS_ID")
                     (list :module-id (car parent) :where (cdr parent)
                           :content (org-canvas--diff-module-item-content dir)))))
         "LEVEL=2" 'file)))))

(defun org-canvas--diff-module-item-remote (extra)
  "Return EXTRA, a remote module item entry, as the alist Canvas sent."
  (list (cons 'type (plist-get extra :type))
        (cons 'title (plist-get extra :title))
        (cons 'content_id (plist-get extra :content-id))
        (cons 'page_url (plist-get extra :page-url))))

(defun org-canvas--diff-module-item-match (extra entry)
  "Return how the unstamped item heading ENTRY matches EXTRA, or nil.
`content' when the sync would adopt EXTRA for ENTRY on what it links:
the same type and content id or page url, or for a SubHeader or an
external URL, whose title is its content, the same title
\(`org-canvas--module-item-same-content-p', issue #299).  `title' when
the two share only a title because ENTRY's link target has no Canvas
id yet, or ENTRY could not be parsed: the sync's predicate falls back
to the title there too, but what the target will be is not known."
  (let ((content (plist-get entry :content)))
    (cond
     ((null content)
      (and (string= (org-canvas--diff-match-title entry)
                    (string-trim (plist-get extra :title)))
           'title))
     ((not (org-canvas--module-item-same-content-p
            content (org-canvas--diff-module-item-remote extra)))
      nil)
     ((or (plist-get content :content-id) (plist-get content :page-url)
          (member (plist-get content :type) '("SubHeader" "ExternalUrl")))
      'content)
     (t 'title))))

(defun org-canvas--diff-module-item-partner (extra unstamped)
  "Return (ENTRY . MATCH), the entry of UNSTAMPED to pair with EXTRA.
EXTRA is a remote module item; MATCH is how they matched
\(`org-canvas--diff-module-item-match').  A heading in the item's own
module first; failing that, one under any other module, unless the
item is a SubHeader.  Nil when no heading matches."
  (let ((matches (delq nil (mapcar (lambda (e)
                                     (let ((m (org-canvas--diff-module-item-match
                                               extra e)))
                                       (and m (cons e m))))
                                   unstamped))))
    (or (cl-find-if (lambda (pair) (equal (plist-get (car pair) :module-id)
                                          (plist-get extra :module-id)))
                    matches)
        (and (not (equal (plist-get extra :type) "SubHeader"))
             (car matches)))))

(defun org-canvas--diff-module-item-paired (extra found file)
  "Return EXTRA re-kinded for FOUND, its (PARTNER . MATCH), from FILE.
`unclaimed' when PARTNER sits in the item's module, `moved' when it
does not.  :to names PARTNER's module and :by-title says only the
title paired them (issue #299).  :file, :line, :partner-heading (the
raw heading text) and :partner-title (what it shows) name the heading
itself, which is how a MOVED row says which heading it paired with,
a renamed one included, and how `s' finds it to stamp (issue #342)."
  (let ((partner (car found)))
    (append (list :kind (if (equal (plist-get partner :module-id)
                                   (plist-get extra :module-id))
                            'unclaimed
                          'moved)
                  :to (plist-get partner :where)
                  :by-title (eq (cdr found) 'title)
                  :file file :line (plist-get partner :line)
                  :partner-heading (plist-get partner :heading)
                  :partner-title (org-canvas--diff-match-title partner))
            (cddr extra))))

(defun org-canvas--diff-module-item-pair (extra headings &optional file)
  "Pair the EXTRA module items with the unstamped entries of HEADINGS.
Returns (ENTRIES . LEFT): ENTRIES is EXTRA with each paired item
re-kinded by `org-canvas--diff-module-item-paired', FILE being the
modules file HEADINGS came from, and LEFT the unstamped headings
nothing paired, the pending creates.  Each heading pairs at most once
\(issue #294)."
  (let ((unstamped (cl-remove-if (lambda (e) (plist-get e :id)) headings)))
    (cons (mapcar
           (lambda (e)
             (let ((found (org-canvas--diff-module-item-partner e unstamped)))
               (if (not found)
                   e
                 (setq unstamped (delq (car found) unstamped))
                 (org-canvas--diff-module-item-paired e found file))))
           extra)
          unstamped)))

(defun org-canvas--diff-module-item-synced (modules local)
  "Return (ID . TITLE) for each LOCAL module entry with an id in MODULES."
  (let ((index (org-canvas--diff-remote-index modules 'id)))
    (delq nil (mapcar (lambda (e)
                        (and (plist-get e :id)
                             (gethash (plist-get e :id) index)
                             (cons (plist-get e :id) (plist-get e :title))))
                      local))))

(defun org-canvas--diff-module-item-homes (lists)
  "Return a hash of the item ids in LISTS to (MODULE . ITEM).
MODULE is the (ID . NAME) pair of the module Canvas holds the item in."
  (let ((homes (make-hash-table :test 'equal)))
    (dolist (pair lists)
      (dolist (item (cdr pair))
        (puthash (format "%s" (alist-get 'id item))
                 (cons (car pair) item) homes)))
    homes))

(defun org-canvas--diff-module-item-relocation (entry home file)
  "Return the `relocate' entry of heading ENTRY, whose item is at HOME.
HOME is the (MODULE . ITEM) the item sits at, FILE the modules file."
  (list :kind 'relocate :title (org-canvas--diff-match-title entry)
        :id (plist-get entry :id) :where (cdr (car home))
        :to (plist-get entry :where)
        :file file :line (plist-get entry :line)
        :html-url (org-canvas--diff-normalize-remote
                   (alist-get 'html_url (cdr home)))))

(defun org-canvas--diff-module-item-relocations (lists headings file)
  "Return a `relocate' entry per stamped heading of HEADINGS in another module.
LISTS is what `org-canvas--diff-module-item-lists' returned and FILE
the modules file.  A heading stamped with an item id that Canvas holds
under a module other than the heading's own is not drift, and no row
of the report named it (issue #343): the next sync finds the id
missing from the heading's module and moves the item there, keeping
its id (`org-canvas--module-item-settle-foreign-id', issue #352), or,
when Canvas refuses the move, creates it there with a new id and
deletes the old copy."
  (let ((homes (org-canvas--diff-module-item-homes lists)))
    (delq nil
          (mapcar
           (lambda (e)
             (let ((home (and (plist-get e :id)
                              (gethash (plist-get e :id) homes))))
               (when (and home (not (equal (car (car home))
                                           (plist-get e :module-id))))
                 (org-canvas--diff-module-item-relocation e home file))))
           headings))))

(defun org-canvas--diff-module-items-result (modules local file)
  "Compare the items of the modules in LOCAL against Canvas; see the caller.
MODULES, LOCAL and FILE are `org-canvas--diff-module-items''s."
  (let* ((headings (org-canvas--diff-module-item-headings file))
         (lists (org-canvas--diff-module-item-lists
                 (org-canvas--diff-module-item-synced modules local)))
         (seen (org-canvas--diff-remote-index
                (apply #'append (mapcar #'cdr lists)) 'id))
         (split (org-canvas--diff-split-acknowledged
                 "Module Items"
                 (org-canvas--diff-module-item-extras
                  lists (delq nil (mapcar (lambda (e) (plist-get e :id)) headings)))
                 seen))
         (paired (org-canvas--diff-module-item-pair (plist-get split :extra)
                                                    headings file)))
    (list :name "Module Items"
          :divergences (plist-get split :stale)
          :extra (car paired)
          :acknowledged (plist-get split :acknowledged)
          :pending (org-canvas--diff-pending "Module Items" (cdr paired) nil
                                             file "CANVAS_ID")
          ;; Stamped headings whose item sits in another module (#343).
          :relocate (org-canvas--diff-module-item-relocations
                     lists headings file))))

(defun org-canvas--diff-module-items (modules local file)
  "Compare the items of the modules in LOCAL against Canvas.
MODULES is the course's module list; only a LOCAL entry whose id it
holds is looked into, one request each.  FILE is modules.org, whose
level-2 headings are what claims an item.  Returns a result plist named
\"Module Items\", shaped like `org-canvas--diff-feature''s: an item no
level-2 heading in modules.org claims is an extra (issue #177), minus
the ones `org-canvas-diff-known-extras' acknowledges under
\"module-items\", with a `stale-ack' divergence for an acknowledged id
no module holds any more.  An extra that shares its title with an
unstamped item heading is UNCLAIMED or MOVED, and the unstamped
headings left are :pending (issue #294).  A stamped heading whose item
Canvas holds in another module is :relocate (issue #343)."
  (if (org-canvas--diff-feature-excluded-p "Module Items")
      (list :name "Module Items" :excluded t)
    (condition-case err
        (org-canvas--diff-module-items-result modules local file)
      (error
       (list :name "Module Items" :error (error-message-string err))))))

;;;; New Quiz Items (issue #322)
;;
;; New Quizzes joined the report in #313 with their own settings and
;; instructions compared, and nothing below them: an item edited or added
;; in the web UI, or deleted there, left the quiz reading clean.  The
;; pass below is the module items' shape (#177) under a quiz.  For every
;; quiz heading whose CANVAS_ASSIGNMENT_ID the quiz service still lists,
;; one request lists its items (`quizzes/:id/items'), and the level-2
;; headings under that quiz are held against them: a heading whose
;; CANVAS_ITEM_ID the quiz no longer holds is MISSING; one whose
;; registered properties (POINTS, TYPE) or body differ is CHANGED; an
;; item no heading of its quiz claims is EXTRA.  Items belong to one
;; quiz, so a claim counts only within it, and an unstamped heading is
;; paired only with an item of its own quiz, by the predicate the sync
;; adopts a twin with (`org-canvas--new-quiz-item-twin-p', #179): the
;; item's first paragraph is the heading.  Such a pair is UNCLAIMED, and
;; the unstamped headings left are PENDING.  The body is the heading and
;; the prompt above the answer list, exported offline as a push would
;; send it, compared as text (#83); the answers themselves live in
;; `interaction_data', whose ids are minted afresh by every push, and
;; are not compared.  No item carries a conflict baseline of its own,
;; so an item row is never "modified remotely" and never adoptable.
;;
;; A list that cannot be read is not an empty quiz (Hard Rule 16): the
;; section reads "could not check" rather than reporting every heading
;; MISSING.  Every row names its quiz (:quiz-id, :where), which is what
;; its verbs need: `k' deletes the item at the quiz service, `b' opens
;; the quiz's page (an item has none), and `p' pulls the quiz, which
;; writes its items whole.  Acknowledgments are filed under
;; "new-quiz-items".  Read-only, as the rest of the report.
;;
;; A bank draw or a stimulus (issue #508) is mirrored by a heading
;; carrying ITEM_KIND.  Stamped, it is compared like any item, but only
;; its pull-only fields (ITEM_KIND, the bank's id and counts, POINTS)
;; take part: such an item has no `item_body' or question type to
;; compare.  Unstamped, it is never paired or pending, since the sync
;; never creates one; and an item of that kind is never a question
;; heading's twin.

(defconst org-canvas--diff-new-quiz-items-name "New Quiz Items"
  "The section New Quiz item rows report in (issue #322).")

(defun org-canvas--diff-new-quiz-item-list (quiz-id)
  "Return the items of the New Quiz QUIZ-ID as a list, or signal.
One request to the quiz service.  A reply that is not a list of item
objects is a misread, never an empty quiz, so it signals (Hard Rule
16)."
  (let ((reply (org-canvas-api-request-all-pages
                'GET (org-canvas--new-quiz-api-endpoint
                      "quizzes/%s/items" quiz-id))))
    (unless (and (or (vectorp reply) (listp reply))
                 (seq-every-p (lambda (i) (and (consp i) (consp (car i)))) reply))
      (error "Could not read the item list of quiz %s" quiz-id))
    (append reply nil)))

(defun org-canvas--diff-new-quiz-item-lists (quizzes local)
  "Return (QUIZ-ID TITLE . ITEMS) for each LOCAL quiz found in QUIZZES.
QUIZZES is the quiz service's reply, LOCAL the quiz headings
`org-canvas--diff-feature' collected; one request per stamped quiz."
  (let ((index (org-canvas--diff-remote-index quizzes '(assignment_id id))))
    (delq nil
          (mapcar (lambda (e)
                    (let ((id (plist-get e :id)))
                      (when (and id (gethash id index))
                        (cons id (cons (plist-get e :title)
                                       (org-canvas--diff-new-quiz-item-list id))))))
                  local))))

(defun org-canvas--diff-new-quiz-item-headings (file)
  "Return the item headings of New Quizzes FILE as local entries.
Each is an `org-canvas--diff-local-entry' plist keyed by CANVAS_ITEM_ID,
with the quiz it sits under added: :quiz-id, that heading's
CANVAS_ASSIGNMENT_ID, and :where, its title; and :pom, a marker the
caller releases."
  (when (and file (file-exists-p file))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (org-map-entries
       (lambda ()
         (let ((quiz (save-excursion
                       (when (org-up-heading-safe)
                         (cons (org-entry-get (point) "CANVAS_ASSIGNMENT_ID")
                               (org-get-heading t t t t))))))
           (append (org-canvas--diff-local-entry "CANVAS_ITEM_ID")
                   (list :quiz-id (car quiz) :where (cdr quiz)
                         :item-kind (org-canvas--new-quiz-item-heading-kind)
                         :container "quiz" :pom (point-marker)))))
       "LEVEL=2" 'file))))

(defun org-canvas--diff-new-quiz-item-base (heading file)
  "Return the fields common to every item row about HEADING in FILE.
The quiz it sits in, for the verbs, and where the heading is, for RET."
  (list :title (plist-get heading :title) :id (plist-get heading :id)
        :quiz-id (plist-get heading :quiz-id) :where (plist-get heading :where)
        :container "quiz" :file file :line (plist-get heading :line)
        :heading (plist-get heading :title) :property "CANVAS_ITEM_ID"))

(defun org-canvas--diff-new-quiz-item-entry (heading items props file)
  "Compare the stamped item HEADING with ITEMS, its quiz's list.
PROPS is the \"new-quiz-items\" property registration; FILE is where
HEADING lives.  Returns a `missing' entry when the quiz holds no item
of its id, a `modified' one when a property or the body differs, and
nil when they agree."
  (let* ((id (plist-get heading :id))
         (pom (plist-get heading :pom))
         (item (cl-find id items :test #'equal
                        :key (lambda (i) (format "%s" (alist-get 'id i)))))
         (base (org-canvas--diff-new-quiz-item-base heading file)))
    (if (null item)
        (cons :kind (cons 'missing base))
      (let* ((body (org-canvas--diff-compare-body props pom item))
             (fields (append (org-canvas--diff-compare-fields
                              (plist-get props :properties) pom item)
                             (and (cdr body) (list (cdr body))))))
        (when fields
          (append (list :kind 'modified :fields fields
                        :body-compared (and body t))
                  base))))))

(defun org-canvas--diff-new-quiz-item-title (item)
  "Return the report's name for remote ITEM: its first paragraph, or ?.
A bank draw or a stimulus is named as a pull would title its heading
\(issue #508)."
  (let ((kind (org-canvas--new-quiz-item-kind item))
        (title (org-canvas--new-quiz-item-remote-title item)))
    (cond (kind (org-canvas--new-quiz-item-read-only-title item kind))
          ((string-empty-p title) "?")
          (t title))))

(defun org-canvas--diff-new-quiz-item-extras (lists claimed)
  "Return the items in LISTS no heading of their quiz CLAIMED, as extras.
LISTS is what `org-canvas--diff-new-quiz-item-lists' returned; CLAIMED
holds a (QUIZ-ID . ITEM-ID) pair per stamped item heading.  Each entry
keeps the item, which pairing reads."
  (let (extra)
    (dolist (quiz lists)
      (dolist (item (cddr quiz))
        (let ((id (format "%s" (alist-get 'id item))))
          (unless (member (cons (car quiz) id) claimed)
            (push (list :kind 'extra
                        :title (org-canvas--diff-new-quiz-item-title item)
                        :id id :quiz-id (car quiz) :where (cadr quiz)
                        :container "quiz" :item item)
                  extra)))))
    (nreverse extra)))

(defun org-canvas--diff-new-quiz-item-twin (heading extra taken)
  "Return the entry of EXTRA the sync would adopt for unstamped HEADING.
One in HEADING's quiz, not in TAKEN, whose item opens with HEADING's
title (`org-canvas--new-quiz-item-twin-p'); nil when there is none."
  (let ((data (list :title (org-canvas--strip-statistics-cookie
                            (plist-get heading :title)))))
    (cl-find-if (lambda (e)
                  (and (equal (plist-get e :quiz-id) (plist-get heading :quiz-id))
                       (not (memq e taken))
                       (org-canvas--new-quiz-item-twin-p data (plist-get e :item))))
                extra)))

(defun org-canvas--diff-new-quiz-item-pair (extra headings file)
  "Pair the EXTRA items with the unstamped entries of HEADINGS in FILE.
Returns (ENTRIES . LEFT): ENTRIES is EXTRA with each paired item made
`unclaimed', naming its heading's place, and LEFT the unstamped
headings nothing paired, the pending creates.  Headings are taken in
file order, each pairing at most once, as the sync adopts them."
  (let (pairs left)
    (dolist (h headings)
      ;; A heading of a bank draw or a stimulus is never created, so an
      ;; unstamped one is neither paired nor pending (issue #508).
      (unless (or (plist-get h :id) (plist-get h :item-kind))
        (let ((twin (org-canvas--diff-new-quiz-item-twin
                     h extra (mapcar #'car pairs))))
          (if twin (push (cons twin h) pairs) (push h left)))))
    (cons (mapcar (lambda (e)
                    (let ((h (cdr (assq e pairs))))
                      (if h
                          (append (list :kind 'unclaimed :file file
                                        :line (plist-get h :line)
                                        :heading (plist-get h :title))
                                  (cddr e))
                        e)))
                  extra)
          (nreverse left))))

(defun org-canvas--diff-new-quiz-item-claims (headings)
  "Return a (QUIZ-ID . ITEM-ID) pair for each stamped entry of HEADINGS."
  (delq nil (mapcar (lambda (h)
                      (and (plist-get h :id)
                           (cons (plist-get h :quiz-id) (plist-get h :id))))
                    headings)))

(defun org-canvas--diff-new-quiz-item-divergences (headings lists file)
  "Return the MISSING and CHANGED entries of the stamped HEADINGS.
Only a heading under a quiz LISTS holds is compared; FILE is theirs."
  (let ((props (org-canvas--diff-find-properties "new-quiz-items")))
    (delq nil (mapcar (lambda (h)
                        (let ((quiz (assoc (plist-get h :quiz-id) lists)))
                          (and quiz (plist-get h :id)
                               (org-canvas--diff-new-quiz-item-entry
                                h (cddr quiz) props file))))
                      headings))))

(defun org-canvas--diff-new-quiz-items-result (quizzes local file)
  "Compare the items of the quizzes in LOCAL against Canvas; see the caller.
QUIZZES, LOCAL and FILE are `org-canvas--diff-new-quiz-items''s."
  (let ((name org-canvas--diff-new-quiz-items-name)
        (headings (org-canvas--diff-new-quiz-item-headings file)))
    (unwind-protect
        (let* ((lists (org-canvas--diff-new-quiz-item-lists quizzes local))
               (split (org-canvas--diff-split-acknowledged
                       name
                       (org-canvas--diff-new-quiz-item-extras
                        lists (org-canvas--diff-new-quiz-item-claims headings))
                       (org-canvas--diff-remote-index
                        (apply #'append (mapcar #'cddr lists)) 'id)))
               (paired (org-canvas--diff-new-quiz-item-pair
                        (plist-get split :extra) headings file)))
          (list :name name
                :divergences (append (org-canvas--diff-new-quiz-item-divergences
                                      headings lists file)
                                     (plist-get split :stale))
                :extra (car paired)
                :acknowledged (plist-get split :acknowledged)
                :pending (org-canvas--diff-pending name (cdr paired) nil
                                                   file "CANVAS_ITEM_ID")))
      (dolist (h headings) (set-marker (plist-get h :pom) nil)))))

(defun org-canvas--diff-new-quiz-items (quizzes local file)
  "Compare the items of the New Quizzes in LOCAL against Canvas.
QUIZZES is the quiz service's list; only a LOCAL quiz whose id it holds
is looked into, one request each.  FILE is new-quizzes.org, whose
level-2 headings are the items.  Returns a result plist named \"New
Quiz Items\", shaped like `org-canvas--diff-feature''s (issue #322);
a list that cannot be read makes it an :error, never an empty quiz."
  (let ((name org-canvas--diff-new-quiz-items-name))
    (if (org-canvas--diff-feature-excluded-p name)
        (list :name name :excluded t)
      (condition-case err
          (org-canvas--diff-new-quiz-items-result quizzes local file)
        (error
         (list :name name :error (error-message-string err)))))))

;;;; Assignment Overrides (issue #411)
;;
;; An extension is the commonest web-UI edit on a live course, and the
;; report could not show one: an override is not a field of the
;; assignment, so a per-student due date given in SpeedGrader moved the
;; column's `updated_at' and nothing else, and the row read CHANGED "in
;; a field this report does not compare".  Since a push by heading
;; reconciles the heading's table (#380), the same unseen extension was
;; deleted by the next push of that heading.  The pass below holds every
;; stamped heading's `#+NAME: overrides' table against the overrides
;; Canvas lists for the assignment — read with the list itself, through
;; the `include[]=overrides' the property registration adds, so the pass
;; costs no request — pairing a row with an override the way the
;; reconcile claims one (`org-canvas--override-find-existing': a section
;; row by its section, a group row by its group, a student row by the
;; same set of students), in a section of its own, "Assignment
;; Overrides".  A Canvas override no row claims is EXTRA, and the row
;; says the next push of that heading deletes it; a row Canvas lacks is
;; MISSING, which the next push creates; a claimed override whose dates
;; differ is CHANGED, with the dates.  A row that resolves to nobody is
;; not a row, as the push reads it.  Every entry is of kind `override'
;; and names its assignment (:where, :assignment-id) and heading (:file,
;; :line), so RET visits the heading, `b' opens the assignment's page,
;; and `p' pulls the heading, which writes Canvas's table whole; the
;; other verbs refuse — an override is deleted by dropping its row and
;; pushing the heading, and adopted by `p'.  A stamped assignment whose
;; list reply carries no `overrides' is a misread, never "none" (Hard
;; Rule 16): the section reads "could not check".  Read-only, as the
;; rest of the report.

(defconst org-canvas--diff-overrides-name "Assignment Overrides"
  "The section override rows report in (issue #411).")

(defconst org-canvas--diff-override-dates
  '((:due-at due_at "Due At" "due")
    (:unlock-at unlock_at "Unlock At" "unlock")
    (:lock-at lock_at "Lock At" "lock"))
  "The three override dates: row key, Canvas field, column title, word.")

(defun org-canvas--diff-override-headings (file query)
  "Return the stamped assignment headings of FILE with their override rows.
QUERY is the registry's heading query.  Each element is a plist: :id
the CANVAS_ID, :title and :heading the heading text, :line where it
sits, :rows the table's parsed rows (nil without a table).  A heading
a pull marked deleted is left out, as the sync leaves it (issue #402)."
  (when (and file (file-exists-p file))
    (let ((dir (file-name-directory file)))
      (with-current-buffer (org-canvas--find-file-noselect file)
        (delq nil
              (org-map-entries
               (lambda ()
                 (let ((id (org-entry-get (point) "CANVAS_ID")))
                   (when (and id (not (string-empty-p id))
                              (not (org-entry-get
                                    (point) org-canvas--prop-canvas-deleted)))
                     (let* ((end (save-excursion (org-end-of-subtree t) (point)))
                            (table (save-excursion
                                     (org-canvas--override-find-table end))))
                       (list :id id
                             :title (org-get-heading t t t t)
                             :heading (org-get-heading t t t t)
                             :line (line-number-at-pos)
                             :rows (and table
                                        (org-canvas--override-parse-table
                                         table dir)))))))
               (or query "LEVEL=1") 'file))))))

(defun org-canvas--diff-override-remote (heading index)
  "Return (ITEM . OVERRIDES) for HEADING's assignment in INDEX, or nil.
Nil when INDEX does not hold the assignment, which the Assignments
section reports MISSING.  Signals when the item carries no
`overrides' key: the list was read without them, and absence is not
an empty list (Hard Rule 16)."
  (let ((item (gethash (plist-get heading :id) index)))
    (when item
      (let ((cell (assq 'overrides item)))
        (unless cell
          (error "The assignment list carried no overrides for id %s (include[]=overrides not honored)"
                 (plist-get heading :id)))
        (cons item (append (cdr cell) nil))))))

(defun org-canvas--diff-override-who (target)
  "Return who override TARGET is for, as the report names it.
TARGET is a Canvas override alist, or a row's target as
`org-canvas--override-target-field' spells it, wrapped in a list: a
section reads by its sections.org heading, a group and students by
their names, `#id' when unresolved, as the table cell does."
  (org-link-display-format (org-canvas--override-target-cell target)))

(defun org-canvas--diff-override-dates-text (getter)
  "Return the dates GETTER answers, as \"due <…>, lock <…>\".
GETTER is called with each entry of `org-canvas--diff-override-dates'
and answers an ISO 8601 string or nil; the text spells each as the
table cell would."
  (mapconcat #'identity
             (delq nil
                   (mapcar (lambda (d)
                             (let ((v (funcall getter d)))
                               (and v (not (string-empty-p v))
                                    (format "%s %s" (nth 3 d)
                                            (org-canvas--override-format-cell v)))))
                           org-canvas--diff-override-dates))
             ", "))

(defun org-canvas--diff-override-fields (row existing)
  "Return (COLUMN ORG CANVAS) for each date of ROW that EXISTING carries otherwise.
ROW is a parsed table row, EXISTING the Canvas override it claims;
two dates agree as the reconcile says they do
\(`org-canvas--override-same-time-p'), so a row the push would not
send is not a row that differs (issue #380)."
  (delq nil
        (mapcar (lambda (d)
                  (let ((ours (plist-get row (nth 0 d)))
                        (theirs (alist-get (nth 1 d) existing)))
                    (unless (org-canvas--override-same-time-p ours theirs)
                      (list (nth 2 d)
                            (if ours (org-canvas--override-format-cell ours) "(unset)")
                            (if theirs (org-canvas--override-format-cell theirs)
                              "(unset)")))))
                org-canvas--diff-override-dates)))

(defun org-canvas--diff-override-entry (heading item file state &rest fields)
  "Return an override row of STATE for HEADING's assignment ITEM in FILE.
STATE is `extra', `missing' or `changed'; FIELDS are the row's own
keys.  Every row names its assignment and its heading's file and
line, which is what its verbs need."
  (append (list :kind 'override :state state
                :assignment-id (plist-get heading :id)
                :where (plist-get heading :title) :container "assignment"
                :file file :line (plist-get heading :line)
                :heading (plist-get heading :heading)
                :html-url (org-canvas--diff-normalize-remote
                           (alist-get 'html_url item)))
          fields))

(defun org-canvas--diff-override-row-entry (heading item file row overrides)
  "Compare table ROW of HEADING with the OVERRIDES of its ITEM on Canvas.
Returns (CLAIMED-ID . ENTRY): the id of the override ROW claims, or
nil, and a MISSING or CHANGED entry, or nil when they agree.  FILE
is the heading's."
  (let ((existing (org-canvas--override-find-existing row overrides))
        (who (org-canvas--diff-override-who
              (list (org-canvas--override-target-field row)))))
    (if (null existing)
        (cons nil (org-canvas--diff-override-entry
                   heading item file 'missing :title who
                   :dates (org-canvas--diff-override-dates-text
                           (lambda (d) (plist-get row (nth 0 d))))))
      (let ((fields (org-canvas--diff-override-fields row existing)))
        (cons (alist-get 'id existing)
              (and fields
                   (org-canvas--diff-override-entry
                    heading item file 'changed :title who
                    :id (format "%s" (alist-get 'id existing))
                    :fields fields
                    ;; What `A' rewrites the row's dates from (issue #528).
                    :override existing)))))))

(defun org-canvas--diff-override-extra-entry (heading item file ov)
  "Return the EXTRA entry for Canvas override OV of HEADING's ITEM in FILE.
A row whose dates are the assignment's own says so: a pull writes no
row for such an override (`org-canvas--override-row-redundant-p')."
  (org-canvas--diff-override-entry
   heading item file 'extra
   :title (org-canvas--diff-override-who ov)
   :id (format "%s" (alist-get 'id ov))
   :dates (org-canvas--diff-override-dates-text
           (lambda (d) (alist-get (nth 1 d) ov)))
   ;; What `A' appends to the heading's table (issue #498).
   :override ov
   :redundant (org-canvas--override-row-redundant-p
               ov (alist-get 'due_at item) (alist-get 'unlock_at item)
               (alist-get 'lock_at item))))

(defun org-canvas--diff-override-rows (heading item overrides file)
  "Return the override rows of HEADING against the OVERRIDES of its ITEM.
FILE is the heading's.  Each table row claims an override as the
reconcile would; what no row claims is EXTRA."
  (let ((claimed nil) (entries nil))
    (dolist (row (plist-get heading :rows))
      (let ((result (org-canvas--diff-override-row-entry
                     heading item file row overrides)))
        (when (car result) (push (car result) claimed))
        (when (cdr result) (push (cdr result) entries))))
    (dolist (ov overrides)
      (unless (memq (alist-get 'id ov) claimed)
        (push (org-canvas--diff-override-extra-entry heading item file ov)
              entries)))
    (nreverse entries)))

(defun org-canvas--diff-overrides-result (items file query)
  "Compare every stamped heading's table in FILE with ITEMS' overrides.
ITEMS is the Assignments list reply, QUERY the registry's heading
query.  Returns the result plist; see the caller."
  (let ((index (org-canvas--diff-remote-index items 'id))
        (entries nil))
    (dolist (heading (org-canvas--diff-override-headings file query))
      (let ((remote (org-canvas--diff-override-remote heading index)))
        (when remote
          (setq entries (nconc entries (org-canvas--diff-override-rows
                                        heading (car remote) (cdr remote)
                                        file))))))
    (list :name org-canvas--diff-overrides-name
          :divergences (cl-remove-if (lambda (e) (eq (plist-get e :state) 'extra))
                                     entries)
          :extra (cl-remove-if-not (lambda (e) (eq (plist-get e :state) 'extra))
                                   entries))))

(defun org-canvas--diff-assignment-overrides (items _local file)
  "Compare the overrides tables in FILE with the overrides ITEMS carry.
ITEMS is the Assignments list reply, read with `include[]=overrides'.
Returns a result plist named \"Assignment Overrides\", shaped like
`org-canvas--diff-feature''s (issue #411); a reply without overrides
makes it an :error, never a course without any."
  (let ((name org-canvas--diff-overrides-name))
    (if (org-canvas--diff-feature-excluded-p name)
        (list :name name :excluded t)
      (condition-case err
          (org-canvas--diff-overrides-result
           items file
           (plist-get (org-canvas--diff-find-properties "assignments") :query))
        (error
         (list :name name :error (error-message-string err)))))))


(provide 'org-canvas-diff-items)
;;; org-canvas-diff-items.el ends here
