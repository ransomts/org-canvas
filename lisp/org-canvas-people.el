;;; org-canvas-people.el --- Pull the course roster from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module pulls the people enrolled in the course into people.org:
;; who they are, in what role, in which sections, and whether their
;; enrollment is active.  It is pull-only.  Enrolling and removing
;; people is the registrar's and the web UI's business, so org-canvas
;; reads the roster for reference and never writes it back.  Nothing
;; local is deleted for being absent on Canvas: a heading for someone
;; who has dropped stays until you remove it.
;;
;; The module reads the enrollments API itself and does not depend on
;; `org-canvas-sections'; when sections.org exists and holds a section's
;; CANVAS_ID, the SECTIONS property links to it.
;;
;; FILE STRUCTURE
;; ==============
;; In people.org:
;;   - Level 1 headings = Roles: Students, Teachers, TAs, Observers,
;;                        Designers, in that order, only those present
;;   - Level 2 headings = People, titled by sortable name
;;
;; PROPERTIES (set by pull, read-only)
;; ====================================
;; USER_ID          - Canvas user id, the key a re-pull matches on
;; ROLE             - student, teacher, ta, observer or designer
;; SECTIONS         - the sections the person is enrolled in, comma
;;                    separated, each a link to sections.org when known
;; ENROLLMENT_STATE - active, invited, inactive or completed; several,
;;                    comma separated, when the person's enrollments differ
;; LAST_ACTIVITY    - when Canvas last saw the person in the course
;; SIS_USER_ID      - only with `org-canvas-people-include-identifiers'
;; LOGIN_ID         - only with `org-canvas-people-include-identifiers'
;;
;; PERSONAL DATA
;; =============
;; The file is a roster of names.  By default only the name, the role,
;; the sections, the state and the last activity are written; the SIS
;; id and login id are opt-in, and an email address or a grade is never
;; written (the enrollment object carries current scores; this module
;; does not read them).  A name is personal data all the same: treat
;; people.org as you treat the submissions directory and keep it out of
;; a course repository (add it to .gitignore) and out of anything
;; shared.  The module writes only `org-canvas-people-file' itself.
;;
;; API NOTES
;; =========
;;   GET /courses/:id/enrollments?state[]=...   - one row per enrollment;
;;       a person in two sections has two rows.  Paginated by bookmark,
;;       so the Link header is followed (`org-canvas-api-request-all-pages')
;;   GET /courses/:id/sections                  - section names for SECTIONS
;;   StudentViewEnrollment is Canvas's "Test Student" and is skipped.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)

;;;; Configuration

(defcustom org-canvas-people-file (org-canvas--path "people.org")
  "Path to the people.org file.
It holds the course roster once pulled; keep it out of a course repository."
  :type 'file
  :group 'org-canvas)
(org-canvas-register-file-var 'org-canvas-people-file "people.org")

(defcustom org-canvas-people-include-identifiers nil
  "When non-nil, write each person's SIS_USER_ID and LOGIN_ID as well.
Off by default: a name is enough for a roster, and the identifiers are
the ones an institution treats as sensitive."
  :type 'boolean
  :group 'org-canvas)

(org-canvas-register-properties "people"
  :label "People"
  :file-var 'org-canvas-people-file
  :query "LEVEL=2"
  :properties
  `((:org-prop "USER_ID" :data-key :user_id :type number :pull-only t
     :doc "Canvas user id; a re-pull matches the heading on it")
    (:org-prop "ROLE" :data-key :role :type enum :pull-only t
     :values ,org-canvas--valid-people-roles
     :doc "The person's role in the course")
    (:org-prop "SECTIONS" :data-key :sections :type string :pull-only t
     :doc "Sections the person is enrolled in, comma separated, linked to sections.org when known")
    (:org-prop "ENROLLMENT_STATE" :data-key :enrollment_state :type string :pull-only t
     :doc "active, invited, inactive or completed; several when the enrollments differ")
    (:org-prop "LAST_ACTIVITY" :data-key :last_activity_at :type timestamp :pull-only t
     :doc "When Canvas last saw the person in the course")
    (:org-prop "SIS_USER_ID" :data-key :sis_user_id :type string :pull-only t
     :doc "SIS id, only with org-canvas-people-include-identifiers")
    (:org-prop "LOGIN_ID" :data-key :login_id :type string :pull-only t
     :doc "Login id, only with org-canvas-people-include-identifiers")))

(defconst org-canvas--people-roles
  '(("TeacherEnrollment" "teacher" "Teachers")
    ("TaEnrollment" "ta" "TAs")
    ("DesignerEnrollment" "designer" "Designers")
    ("ObserverEnrollment" "observer" "Observers")
    ("StudentEnrollment" "student" "Students"))
  "Enrollment types the roster lists: (TYPE ROLE HEADING), in precedence order.
A person with two roles takes the first listed; the file's role
headings are written in the order Students, Teachers, TAs, Observers,
Designers regardless.  Anything else, StudentViewEnrollment above all,
is skipped.")

(defconst org-canvas--people-heading-order
  '("Students" "Teachers" "TAs" "Observers" "Designers")
  "Order of the role headings in people.org.")

;;;; Grouping Enrollments Into People

(defun org-canvas--people-role-rank (type)
  "Return TYPE's precedence in `org-canvas--people-roles', or nil to skip."
  (cl-position type org-canvas--people-roles :key #'car :test #'equal))

(defun org-canvas--people-new (uid user rank)
  "Return a fresh person plist for user UID from the USER object, at role RANK."
  (list :user-id uid
        :name (or (alist-get 'sortable_name user)
                  (alist-get 'name user)
                  (format "User %s" uid))
        :rank rank
        :section-ids nil :states nil :last-activity nil
        :sis-user-id (org-canvas--alist-get-non-null 'sis_user_id user)
        :login-id (org-canvas--alist-get-non-null 'login_id user)))

(defun org-canvas--people-merge-enrollment (person enrollment rank)
  "Fold one ENROLLMENT at role RANK into PERSON's plist, in place.
The higher role wins, a new section or state is appended, and the
latest activity is kept."
  (when (< rank (plist-get person :rank))
    (plist-put person :rank rank))
  (let ((sid (alist-get 'course_section_id enrollment)))
    (when (and sid (not (memq sid (plist-get person :section-ids))))
      (plist-put person :section-ids (append (plist-get person :section-ids) (list sid)))))
  (let ((state (alist-get 'enrollment_state enrollment)))
    (when (and (stringp state) (not (member state (plist-get person :states))))
      (plist-put person :states (append (plist-get person :states) (list state)))))
  (let ((seen (org-canvas--alist-get-non-null 'last_activity_at enrollment)))
    (when (and (stringp seen)
               (or (null (plist-get person :last-activity))
                   (string> seen (plist-get person :last-activity))))
      (plist-put person :last-activity seen)))
  person)

(defun org-canvas--people-group (enrollments)
  "Fold ENROLLMENTS, one per section, into one plist per person.
Each plist has :user-id, :name (the sortable name), :role, :heading,
:section-ids, :states, :last-activity (the latest), :sis-user-id and
:login-id.  Enrollment types the roster does not list are dropped.
Returns the people in role-heading order, then by name."
  (let ((people (make-hash-table :test 'eql)))
    (dolist (e (append enrollments nil))
      (let ((rank (org-canvas--people-role-rank (alist-get 'type e))))
        (when rank
          (let* ((uid (alist-get 'user_id e))
                 (p (or (gethash uid people)
                        (puthash uid (org-canvas--people-new uid (alist-get 'user e) rank)
                                 people))))
            (org-canvas--people-merge-enrollment p e rank)))))
    (let (result)
      (maphash (lambda (_uid p)
                 (let ((role (nth (plist-get p :rank) org-canvas--people-roles)))
                   (plist-put p :role (nth 1 role))
                   (plist-put p :heading (nth 2 role)))
                 (push p result))
               people)
      ;; Heading order first, so a fresh file reads Students, Teachers,
      ;; TAs, Observers, Designers, each created as its first person is
      ;; placed; then by name within a role.
      (sort result (lambda (a b)
                     (let ((ha (cl-position (plist-get a :heading) org-canvas--people-heading-order :test #'equal))
                           (hb (cl-position (plist-get b :heading) org-canvas--people-heading-order :test #'equal)))
                       (if (= ha hb)
                           (string< (plist-get a :name) (plist-get b :name))
                         (< ha hb))))))))

;;;; Section Names and Links

(defun org-canvas--people-section-heading (section-id)
  "Return the sections.org heading carrying SECTION-ID, or nil.
Nil as well when `org-canvas-sections-file' is unset or missing."
  (let ((file (and (boundp 'org-canvas-sections-file) org-canvas-sections-file)))
    (when (and file (file-exists-p file))
      (let ((target (format "%s" section-id)) (heading nil))
        (with-current-buffer (org-canvas--find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (when (and (not heading)
                          (equal (org-entry-get (point) "CANVAS_ID") target))
                 (setq heading (org-get-heading t t t t))))
             "LEVEL=1" 'file)))
        heading))))

(defun org-canvas--people-section-text (section-id names)
  "Return the SECTIONS text for SECTION-ID: a sections.org link, or its name.
NAMES maps section ids to the names the course reports."
  (let ((heading (org-canvas--people-section-heading section-id))
        (name (or (alist-get section-id names) (format "%s" section-id))))
    (if heading
        (let ((unescaped (replace-regexp-in-string "\\\\\\([][]\\)" "\\1" heading)))
          (org-link-make-string
           (format "file:%s::*%s"
                   (file-name-nondirectory org-canvas-sections-file) unescaped)
           name))
      name)))

(defun org-canvas--people-fetch-section-names ()
  "Return an alist of section id to name for the course."
  (mapcar (lambda (s) (cons (alist-get 'id s) (alist-get 'name s)))
          (append (org-canvas-api-request-all-pages
                   'GET (org-canvas-api-course-endpoint "sections"))
                  nil)))

;;;; Headings

(defun org-canvas--people-role-heading-pos (heading)
  "Return the position of the level-1 role HEADING, creating it if absent.
A new role heading is appended after the last existing one, so the
file keeps `org-canvas--people-heading-order' when pulled in that
order."
  (org-canvas--pull-heading-by-title heading))

(defun org-canvas--people-find-person (user-id)
  "Return the position of the heading carrying USER-ID anywhere in the file, or nil."
  (let ((target (format "%s" user-id)) (pos nil))
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when (and (not pos) (equal (org-entry-get (point) "USER_ID") target))
           (setq pos (point))))
       "USER_ID={.}" 'file))
    pos))

(defun org-canvas--people-append-person (role-pos name)
  "Append a level-2 heading NAME at the end of the role subtree at ROLE-POS.
Returns the new heading's position."
  (goto-char role-pos)
  (goto-char (save-excursion (org-end-of-subtree t) (point)))
  (skip-chars-backward " \t\n")
  (insert (format "\n** %s\n" name))
  (forward-line -1)
  (org-back-to-heading t)
  (point))

;;;; Properties

(defun org-canvas--people-set-properties (pos person names)
  "Write PERSON's pulled properties on the heading at POS.
NAMES maps section ids to names."
  (org-canvas-org-set-property pos "USER_ID" (format "%s" (plist-get person :user-id)))
  (org-canvas-org-set-property pos "ROLE" (plist-get person :role))
  (when (plist-get person :section-ids)
    (org-canvas-org-set-property
     pos "SECTIONS"
     (mapconcat (lambda (sid) (org-canvas--people-section-text sid names))
                (plist-get person :section-ids) ", ")))
  (when (plist-get person :states)
    (org-canvas-org-set-property
     pos "ENROLLMENT_STATE" (mapconcat #'identity (plist-get person :states) ", ")))
  (org-canvas--pull-set-timestamp-property
   pos "LAST_ACTIVITY" (plist-get person :last-activity))
  (when org-canvas-people-include-identifiers
    (when (plist-get person :sis-user-id)
      (org-canvas-org-set-property pos "SIS_USER_ID" (plist-get person :sis-user-id)))
    (when (plist-get person :login-id)
      (org-canvas-org-set-property pos "LOGIN_ID" (plist-get person :login-id)))))

(defun org-canvas--people-pull-one (person names)
  "Upsert PERSON into the current buffer under its role heading.
A heading already carrying the USER_ID is updated where it sits, its
ROLE included, so a person who changed roles is not duplicated.  NAMES
maps section ids to names."
  (let* ((existing (org-canvas--people-find-person (plist-get person :user-id)))
         (pos (or existing
                  (org-canvas--people-append-person
                   (org-canvas--people-role-heading-pos (plist-get person :heading))
                   (plist-get person :name)))))
    (goto-char pos)
    (org-edit-headline (plist-get person :name))
    (org-canvas--people-set-properties pos person names)
    pos))

;;;; Pull

(defun org-canvas--people-fetch-enrollments ()
  "Return every enrollment of the course, in every state, as a list."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint "enrollments")
           '(("state[]" . "active") ("state[]" . "invited")
             ("state[]" . "inactive") ("state[]" . "completed")))
          nil))

(defun org-canvas--people-summary (people)
  "Return a count-per-role string for PEOPLE, in heading order."
  (mapconcat
   (lambda (heading)
     (let ((n (cl-count heading people :key (lambda (p) (plist-get p :heading)) :test #'equal)))
       (and (> n 0) (format "%d %s" n (downcase heading)))))
   (cl-remove-if-not
    (lambda (h) (cl-some (lambda (p) (equal (plist-get p :heading) h)) people))
    org-canvas--people-heading-order)
   ", "))

;;;###autoload
(defun org-canvas-pull-people ()
  "Pull the course roster into people.org.
Roles become level-1 headings and each person a level-2 heading with
their role, sections, enrollment state and last activity.  Read-only:
nothing is pushed, and nothing local is deleted for being absent on
Canvas.  The file holds a roster of names afterwards; keep it out of a
course repository."
  (interactive)
  (org-canvas--start-operation "PULLING PEOPLE")
  (let* ((file (expand-file-name org-canvas-people-file))
         (people (org-canvas--people-group (org-canvas--people-fetch-enrollments)))
         (was-fresh (org-canvas--pull-was-fresh-p file)))
    (org-canvas--pull-confirm-unsaved file "people")
    (if (null people)
        (org-canvas--pull-emit-empty-file file (org-canvas--pull-label-for "people"))
      (let ((names (org-canvas--people-fetch-section-names))
            (count 0))
        (unless (file-exists-p file)
          (with-temp-file file (insert "")))
        (with-current-buffer (org-canvas--find-file-noselect file)
          ;; A role heading appears when its first person is placed, so
          ;; a person who kept an old heading leaves no empty new one.
          (dolist (person people)
            (cl-incf count)
            (when (zerop (% count 25))
              (message "People [%d/%d]..." count (length people)))
            (org-canvas--people-pull-one person names))
          (org-canvas--pull-write-file-header)
          (org-canvas--save-buffer))))
    (org-canvas--pull-kill-fresh-buffer file was-fresh)
    (let ((summary (org-canvas--people-summary people)))
      (org-canvas--log-info org-canvas--logger
        "People pull complete: %d people (%s)" (length people) summary)
      (message "People pull complete: %d people (%s)." (length people) summary))))

(provide 'org-canvas-people)
;;; org-canvas-people.el ends here
