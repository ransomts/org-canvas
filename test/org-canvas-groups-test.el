;;; org-canvas-groups-test.el --- Buttercup tests for the groups pull -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-groups': categories, groups and members pulled
;; into groups.org.  Every request is answered by a fake keyed on the
;; URL; nothing here reaches the network (Hard Rule 2), and the log is
;; captured into a list, never read from the shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
;; The whole package: `org-canvas-group-categories-file' must be special
;; for `let' to bind it, and the tier list lives in org-canvas.el.
(require 'org-canvas)

(defvar test-groups--categories nil
  "Categories the fake API lists for the course.")
(defvar test-groups--groups nil
  "Alist of category id to the groups the fake API lists for it.")
(defvar test-groups--users nil
  "Alist of group id to the users the fake API lists, or `forbidden'.")

(defun test-groups--api (_method url &optional _params)
  "Answer URL from the fake tables, as the paginated helper would."
  (cond
   ((string-match "/groups/\\([0-9]+\\)/users" url)
    (let ((answer (alist-get (string-to-number (match-string 1 url)) test-groups--users)))
      (if (eq answer 'forbidden)
          (signal 'org-canvas-permission-error
                  '("Permission denied (HTTP 403) reading the group's users"))
        answer)))
   ((string-match "/group_categories/\\([0-9]+\\)/groups" url)
    (alist-get (string-to-number (match-string 1 url)) test-groups--groups))
   ((string-match "/group_categories\\'" url)
    test-groups--categories)
   (t (error "Unexpected request: %s" url))))

(defmacro test-groups--with-course (&rest body)
  "Run BODY with a temp course directory, a groups file inside it, and the fake API.
FILE is bound to the groups file path and DIR to the directory."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "groups-test-" t))
          (file (expand-file-name "groups.org" dir))
          (org-canvas-groups-file file)
          (org-canvas-group-categories-file (expand-file-name "group-categories.org" dir)))
     (unwind-protect
         (with-org-canvas-test-config
           (cl-letf (((symbol-function 'org-canvas-api-request-all-pages) #'test-groups--api)
                     ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
             ,@body))
       (dolist (f (list file org-canvas-group-categories-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-groups--file-text (file)
  "Return FILE's text as saved on disk."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun test-groups--set-fixture ()
  "Two categories; one holds two groups with members, the other none."
  (setq test-groups--categories
        (list '((id . 10) (name . "Project Teams") (position . 1))
              '((id . 20) (name . "Empty Set") (position . 2)))
        test-groups--groups
        (list (cons 10 (list '((id . 101) (name . "Team A") (max_membership . 4)
                               (join_level . "invitation_only") (members_count . 2))
                             '((id . 102) (name . "Team B") (max_membership . nil)
                               (join_level . "parent_context_auto_join") (members_count . 0))))
              (cons 20 nil))
        test-groups--users
        (list (cons 101 (list '((id . 1) (name . "Ada Lovelace") (login_id . "ada@x"))
                              '((id . 2) (name . "Grace Hopper") (sis_user_id . "s2"))))
              (cons 102 nil))))

(describe "org-canvas-pull-groups"
  (before-each (test-groups--set-fixture))

  (it "writes categories, groups and members at their levels with the right properties"
    (test-groups--with-course
      (org-canvas-pull-groups)
      (let ((text (test-groups--file-text file)))
        (expect text :to-match "^\\* Project Teams\n")
        (expect text :to-match "^\\* Empty Set\n")
        (expect text :to-match "^\\*\\* Team A\n")
        (expect text :to-match "^\\*\\* Team B\n")
        (expect text :to-match ":CANVAS_ID: +10\n")
        (expect text :to-match ":CANVAS_ID: +101\n")
        (expect text :to-match ":MAX_MEMBERSHIP: +4\n")
        (expect text :to-match ":JOIN_LEVEL: +invitation_only\n")
        (expect text :to-match ":MEMBERS_COUNT: +2\n")
        (expect text :to-match "^- Ada Lovelace\n- Grace Hopper\n")
        (expect text :to-match "#\\+LAST_SYNCED:")
        ;; The display name only: no login, no SIS id.
        (expect text :not :to-match "ada@x")
        (expect text :not :to-match "s2"))))

  (it "omits a nil member limit and writes nothing under a group without members"
    (test-groups--with-course
      (org-canvas-pull-groups)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Team B")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "MAX_MEMBERSHIP") :to-be nil)
        (expect (org-entry-get (point) "MEMBERS_COUNT") :to-equal "0")
        (let ((body (buffer-substring-no-properties
                     (save-excursion (org-end-of-meta-data t) (point))
                     (save-excursion (org-end-of-subtree t) (point)))))
          (expect (string-trim body) :to-equal "")))))

  (it "puts the members under their own group, not the next one"
    (setq test-groups--users
          (list (cons 101 (list '((id . 1) (name . "Ada Lovelace"))))
                (cons 102 (list '((id . 3) (name . "Edsger Dijkstra"))))))
    (test-groups--with-course
      (org-canvas-pull-groups)
      (expect (test-groups--file-text file)
              :to-match "\\*\\* Team A\n:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n- Ada Lovelace\n\n\\*\\* Team B\n:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n- Edsger Dijkstra\n")))

  (it "upserts by id on a second pull and rewrites the member list"
    (test-groups--with-course
      (org-canvas-pull-groups)
      (setq test-groups--users
            (list (cons 101 (list '((id . 2) (name . "Grace Hopper"))))
                  (cons 102 nil)))
      (setcar (alist-get 10 test-groups--groups)
              '((id . 101) (name . "Team A (renamed)") (max_membership . 4)
                (join_level . "invitation_only") (members_count . 1)))
      (org-canvas-pull-groups)
      (let ((text (test-groups--file-text file)))
        (expect (cl-count-if (lambda (l) (string-prefix-p "* " l)) (split-string text "\n"))
                :to-equal 2)
        (expect (cl-count-if (lambda (l) (string-prefix-p "** " l)) (split-string text "\n"))
                :to-equal 2)
        (expect text :to-match "^\\*\\* Team A (renamed)\n")
        (expect text :not :to-match "Ada Lovelace")
        (expect text :to-match "^- Grace Hopper\n")
        (expect text :to-match ":MEMBERS_COUNT: +1\n"))))

  (it "links a category to its heading in group-categories.org, or names it plainly"
    (test-groups--with-course
      (with-temp-file org-canvas-group-categories-file
        (insert "* Project Teams\n:PROPERTIES:\n:CANVAS_ID: 10\n:END:\n"))
      (org-canvas-pull-groups)
      (let ((text (test-groups--file-text file)))
        (expect text :to-match
                "^\\* \\[\\[file:group-categories.org::\\*Project Teams\\]\\[Project Teams\\]\\]\n")
        (expect text :to-match "^\\* Empty Set\n"))))

  (it "names every category plainly when group-categories.org does not exist"
    (test-groups--with-course
      (org-canvas-pull-groups)
      (expect (test-groups--file-text file) :to-match "^\\* Project Teams\n")))

  (it "logs and skips a group whose members it may not list, keeping the rest"
    (setq test-groups--users
          (list (cons 101 'forbidden)
                (cons 102 (list '((id . 3) (name . "Edsger Dijkstra"))))))
    (test-groups--with-course
      (let ((warned nil))
        (org-canvas--pull-summary-reset)
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) warned))))
          (expect (org-canvas-pull-groups) :not :to-throw))
        (expect (car (last warned)) :to-match "members of 'Team A'")
        (let ((text (test-groups--file-text file)))
          (expect text :to-match "^\\*\\* Team A\n")
          (expect text :to-match "^- Edsger Dijkstra\n"))
        (let ((record (car (org-canvas--pull-summary-records))))
          (expect (plist-get record :kind) :to-be 'skip)
          (expect (plist-get record :item) :to-equal "Team A")))))

  (it "leaves an existing member list alone when listing it fails"
    (test-groups--with-course
      (org-canvas-pull-groups)
      (setq test-groups--users (list (cons 101 'forbidden) (cons 102 nil)))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas-pull-groups))
      (expect (test-groups--file-text file) :to-match "^- Ada Lovelace\n- Grace Hopper\n")))

  (it "writes the empty-file note for a course without categories"
    (setq test-groups--categories nil)
    (test-groups--with-course
      (org-canvas-pull-groups)
      (let ((text (test-groups--file-text file)))
        (expect text :to-match "#\\+TITLE: Groups")
        (expect text :to-match "Canvas returned 0 items"))))

  (it "is a command in the pull tier list"
    (expect (commandp 'org-canvas-pull-groups) :to-be t)
    (expect (cl-some (lambda (tier) (assq 'org-canvas-pull-groups tier))
                     org-canvas--pull-tiers)
            :to-be-truthy)))

(describe "org-canvas--group-find-or-create-l2"
  (it "finds an existing group by id under the category and creates a missing one at its end"
    (with-temp-org-buffer
        "* Cat\n:PROPERTIES:\n:CANVAS_ID: 10\n:END:\n** Team A\n:PROPERTIES:\n:CANVAS_ID: 101\n:END:\n- Someone\n\n* Other\n:PROPERTIES:\n:CANVAS_ID: 20\n:END:\n"
      (org-back-to-heading t)
      (let ((found (org-canvas--group-find-or-create-l2 101 "ignored")))
        (goto-char found)
        (expect (org-get-heading t t t t) :to-equal "Team A"))
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((made (org-canvas--group-find-or-create-l2 102 "Team B")))
        (goto-char made)
        (expect (org-get-heading t t t t) :to-equal "Team B")
        ;; Under Cat, before Other.
        (expect (save-excursion (re-search-forward "^\\* Other" nil t)) :to-be-truthy)))))

(describe "org-canvas--group-write-members"
  (it "replaces whatever body the group had with the names"
    (with-temp-org-buffer
        "* Cat\n** Team A\n:PROPERTIES:\n:CANVAS_ID: 101\n:END:\n- Old Name\nsome note\n\n** Team B\n"
      (re-search-forward "^\\*\\* Team A")
      (org-back-to-heading t)
      (org-canvas--group-write-members '(((name . "New One")) ((name . "New Two"))))
      (expect (buffer-string)
              :to-match "\\*\\* Team A\n:PROPERTIES:\n:CANVAS_ID: 101\n:END:\n- New One\n- New Two\n\n\\*\\* Team B")
      (expect (buffer-string) :not :to-match "Old Name")))

  (it "names a user Canvas returned without a name"
    (with-temp-org-buffer "* Cat\n** Team A\n"
      (re-search-forward "^\\*\\* Team A")
      (org-back-to-heading t)
      (org-canvas--group-write-members '(((id . 9))))
      (expect (buffer-string) :to-match "^- Unnamed\n"))))

(describe "org-canvas--group-category-title"
  (it "escapes nothing twice when the heading carries brackets"
    (let* ((dir (make-temp-file "groups-link-" t))
           (org-canvas-group-categories-file (expand-file-name "group-categories.org" dir)))
      (unwind-protect
          (progn
            (with-temp-file org-canvas-group-categories-file
              (insert "* Teams \\[2026\\]\n:PROPERTIES:\n:CANVAS_ID: 10\n:END:\n"))
            (expect (org-canvas--group-category-title 10 "Teams [2026]")
                    :to-match "\\`\\[\\[file:group-categories.org::\\*Teams "))
        (let ((buf (find-buffer-visiting org-canvas-group-categories-file)))
          (when buf (kill-buffer buf)))
        (delete-directory dir t))))

  (it "returns the name when the categories file is unset"
    (let ((org-canvas-group-categories-file nil))
      (expect (org-canvas--group-category-title 10 "Plain") :to-equal "Plain"))))

(provide 'org-canvas-groups-test)
;;; org-canvas-groups-test.el ends here
