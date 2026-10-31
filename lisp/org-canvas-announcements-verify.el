;;; org-canvas-announcements-verify.el --- Read a pushed announcement back from Canvas -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An announcement is the one push students see the moment it lands: a
;; leftover DELAYED_POST_AT, comments left open or a stale body cannot
;; be quietly fixed afterwards.  A push answers with what Canvas said to
;; the request, and the drift report is often told to leave
;; announcements out (`org-canvas-diff-excluded-features'), so
;; confirming that one went out now, with comments off and the text
;; intact, took a script that listed every announcement and compared
;; the new one by hand (issue #502).
;;
;; `org-canvas-verify-announcements' (`scripts/org-canvas verify
;; announcements:TITLE') is that read-back.  Each named heading's topic
;; is read on its own, by CANVAS_ID, through the feature registry's item
;; URL (Hard Rule 13), and five things are compared:
;;
;;   - title: the heading against Canvas's `title';
;;   - published: PUBLISHED (true when absent, as the push sends it)
;;     against `published';
;;   - posting: DELAYED_POST_AT against `delayed_post_at' and
;;     `posted_at'.  A future DELAYED_POST_AT wants the topic scheduled
;;     for that minute; none, or one already past, wants it posted (a
;;     `posted_at', unless the heading is unpublished) with no posting
;;     time still ahead;
;;   - comments: ALLOW_COMMENTS against `locked'.  The push locks an
;;     announcement unless ALLOW_COMMENTS is true;
;;   - body: the heading's exported body against `message', as text,
;;     the way the drift report compares bodies.
;;
;; Nothing is written, locally or on Canvas: the body is exported
;; offline (Hard Rule 19), so no image is uploaded and no link
;; resolved.

;;; Code:

(require 'cl-lib)
(require 'org-canvas-core)
;; A command file above the feature modules: it reads the announcement
;; registry entries and compares bodies the drift report's way.
(require 'org-canvas-diff)
(require 'org-canvas-diff-fields)
(require 'org-canvas-announcements)

(defconst org-canvas--announcement-verify-buffer "*canvas-verify-announcements*"
  "Buffer the announcement verification report is rendered into.")

(defconst org-canvas--announcement-verify-feature-name "Announcements"
  "The registry name of the feature this command verifies.")

;;;; Reading Both Sides

(defun org-canvas--announcement-verify-feature ()
  "Return the announcements feature registry entry."
  (org-canvas--diff-find-feature org-canvas--announcement-verify-feature-name))

(defun org-canvas--announcement-verify-feature-p (name)
  "Return non-nil when NAME, a feature name in any spelling, is announcements."
  (and (string-match-p "\\`announcements?\\'" (org-canvas--diff-normalize-name name)) t))

(defun org-canvas--announcement-verify-time (iso)
  "Return ISO, a Canvas timestamp or JSON null, as a time value, or nil."
  (org-canvas--parse-iso8601-time (org-canvas--diff-normalize-remote iso)))

(defun org-canvas--announcement-verify-org-time (pom)
  "Return the DELAYED_POST_AT of the heading at POM as a time value, or nil."
  (let ((ts (org-entry-get pom "DELAYED_POST_AT")))
    (and ts (not (string-empty-p (string-trim ts)))
         (org-canvas--parse-iso8601-time (org-canvas-org-parse-timestamp ts)))))

(defun org-canvas--announcement-verify-show-time (time)
  "Return TIME as an Org timestamp in the course zone."
  (format-time-string "<%Y-%m-%d %a %H:%M>" time (org-canvas--time-zone)))

(defun org-canvas--announcement-verify-row (field ok org canvas)
  "Return a report row for FIELD: OK non-nil when ORG and CANVAS agree."
  (list :field field :ok (and ok t) :org org :canvas canvas))

;;;; Checks

(defun org-canvas--announcement-verify-title (entry item)
  "Return the title row of heading ENTRY against Canvas ITEM."
  (let ((org (org-canvas--diff-squeeze (plist-get entry :match-title)))
        (canvas (org-canvas--diff-squeeze
                 (org-canvas--diff-normalize-remote (alist-get 'title item)))))
    (org-canvas--announcement-verify-row
     "title" (equal org canvas) (format "%S" org) (format "%S" (or canvas "")))))

(defun org-canvas--announcement-verify-published-p (pom)
  "Return non-nil when the heading at POM asks to be published.
PUBLISHED defaults to true, as the push sends it."
  (not (equal (org-entry-get pom "PUBLISHED") "false")))

(defun org-canvas--announcement-verify-published (pom item)
  "Return the published row of the heading at POM against Canvas ITEM."
  (let ((org (org-canvas--announcement-verify-published-p pom))
        (canvas (and (org-canvas--diff-normalize-remote (alist-get 'published item)) t)))
    (org-canvas--announcement-verify-row
     "published" (eq org canvas)
     (if org "true" "false") (if canvas "true" "false"))))

(defun org-canvas--announcement-verify-canvas-posting (item now)
  "Return how Canvas ITEM is posting, at NOW, as report text."
  (let ((delayed (org-canvas--announcement-verify-time (alist-get 'delayed_post_at item)))
        (posted (org-canvas--announcement-verify-time (alist-get 'posted_at item))))
    (cond ((and delayed (time-less-p now delayed))
           (format "scheduled for %s" (org-canvas--announcement-verify-show-time delayed)))
          (posted (format "posted %s" (org-canvas--announcement-verify-show-time posted)))
          (t "not posted"))))

(defun org-canvas--announcement-verify-posting-ok-p (org-time published item now)
  "Return non-nil when Canvas ITEM posts as ORG-TIME asks, at NOW.
ORG-TIME is the heading's DELAYED_POST_AT, nil without one; PUBLISHED
is whether the heading asks to be published.  A future ORG-TIME wants
Canvas scheduled for the same minute; otherwise no posting time may
lie ahead, and a published heading wants a `posted_at'."
  (let ((delayed (org-canvas--announcement-verify-time (alist-get 'delayed_post_at item)))
        (posted (org-canvas--announcement-verify-time (alist-get 'posted_at item))))
    (if (and org-time (time-less-p now org-time))
        (and delayed (= (org-canvas--diff-time-minute delayed)
                        (org-canvas--diff-time-minute org-time)))
      (and (not (and delayed (time-less-p now delayed)))
           (or posted (not published))))))

(defun org-canvas--announcement-verify-posting (pom item now)
  "Return the posting row of the heading at POM against Canvas ITEM, at NOW."
  (let ((org-time (org-canvas--announcement-verify-org-time pom))
        (published (org-canvas--announcement-verify-published-p pom)))
    (org-canvas--announcement-verify-row
     "posting"
     (org-canvas--announcement-verify-posting-ok-p org-time published item now)
     (cond ((and org-time (time-less-p now org-time))
            (format "scheduled for %s" (org-canvas--announcement-verify-show-time org-time)))
           (org-time (format "posts now (DELAYED_POST_AT %s passed)"
                             (org-canvas--announcement-verify-show-time org-time)))
           (t "posts now"))
     (org-canvas--announcement-verify-canvas-posting item now))))

(defun org-canvas--announcement-verify-comments (pom item)
  "Return the comments row of the heading at POM against Canvas ITEM.
The push locks the topic unless ALLOW_COMMENTS is true."
  (let ((org (equal (org-entry-get pom "ALLOW_COMMENTS") "true"))
        (canvas (org-canvas--announcement-remote-allow-comments item)))
    (org-canvas--announcement-verify-row
     "comments" (eq org (and canvas t))
     (if org "allowed (ALLOW_COMMENTS true)" "off")
     (if canvas "open" "locked"))))

(defun org-canvas--announcement-verify-body (pom item)
  "Return the body row of the heading at POM against Canvas ITEM.
The heading is exported offline, as the drift report exports it, so
nothing is uploaded; an export that fails is a difference, unchecked."
  (let ((cmp (org-canvas--diff-compare-body
              (org-canvas--diff-find-properties
               org-canvas--announcement-verify-feature-name)
              pom item)))
    (cond ((null cmp)
           (org-canvas--announcement-verify-row "body" nil "not exported" "unchecked"))
          ((null (cdr cmp))
           (org-canvas--announcement-verify-row "body" t "matches" "matches"))
          (t (org-canvas--announcement-verify-row
              "body" nil (format "%S" (nth 1 (cdr cmp))) (format "%S" (nth 2 (cdr cmp))))))))

(defun org-canvas--announcement-verify-rows (entry item now)
  "Return every row of heading ENTRY, its :pom set, against ITEM at NOW."
  (let ((pom (plist-get entry :pom)))
    (list (org-canvas--announcement-verify-title entry item)
          (org-canvas--announcement-verify-published pom item)
          (org-canvas--announcement-verify-posting pom item now)
          (org-canvas--announcement-verify-comments pom item)
          (org-canvas--announcement-verify-body pom item))))

;;;; One Heading

(defun org-canvas--announcement-verify-result (entry rows &optional error)
  "Return the result plist of heading ENTRY: its ROWS, or ERROR text."
  (list :title (or (plist-get entry :title) (plist-get entry :target))
        :id (plist-get entry :id) :rows rows :error error
        :differences (if error 1 (cl-count-if-not (lambda (r) (plist-get r :ok)) rows))))

(defun org-canvas--announcement-verify-here (feature now)
  "Verify the announcement heading at point against FEATURE's Canvas item.
NOW is the time a posting is judged at.  Return the result plist."
  (let* ((entry (append (org-canvas--diff-local-entry "CANVAS_ID")
                        (list :pom (point-marker))))
         (id (plist-get entry :id)))
    (unwind-protect
        (if (null id)
            (org-canvas--announcement-verify-result
             entry nil "no CANVAS_ID; the heading was never pushed")
          (let ((item (org-canvas--pull-item-read feature id)))
            (if (eq item 'gone)
                (org-canvas--announcement-verify-result
                 entry nil "not on Canvas any more")
              (org-canvas--announcement-verify-result
               entry (org-canvas--announcement-verify-rows entry item now)))))
      (set-marker (plist-get entry :pom) nil))))

(defun org-canvas--announcement-verify-one (feature entry now)
  "Verify heading ENTRY, (FEATURE-NAME TARGET BY), of FEATURE at NOW.
A heading that cannot be found or read is a result naming the
error, so the next heading is still verified."
  (condition-case err
      (let ((marker (org-canvas--sync-find-heading
                     :file org-canvas-announcements-file :query "LEVEL=1"
                     :target (nth 1 entry) :by (nth 2 entry)
                     :id-property "CANVAS_ID"
                     :feature (plist-get feature :name) :verb "verify")))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (set-marker marker nil)
            (org-canvas--announcement-verify-here feature now))))
    (error (org-canvas--announcement-verify-result
            (list :target (nth 1 entry)) nil
            (org-canvas--log-redact (error-message-string err))))))

(defun org-canvas--announcement-verify-entries (entries)
  "Return ENTRIES as (FEATURE TARGET BY) lists, each naming announcements.
An entry naming another feature is a `user-error', before anything
is read."
  (mapcar (lambda (entry)
            (let ((parsed (org-canvas--sync-headings-entry entry)))
              (unless (org-canvas--announcement-verify-feature-p (car parsed))
                (user-error "Verify reads announcements:TITLE, not %s:%s"
                            (car parsed) (nth 1 parsed)))
              parsed))
          entries))

;;;; Report

(defun org-canvas--announcement-verify-summary (result)
  "Return the one-line summary of RESULT, a heading's plist."
  (format "%s%s: %s" (plist-get result :title)
          (if (plist-get result :id) (format " (CANVAS_ID %s)" (plist-get result :id)) "")
          (or (plist-get result :error)
              (format "%d difference(s)" (plist-get result :differences)))))

(defun org-canvas--announcement-verify-render (results)
  "Insert the verification report on RESULTS, one section per heading."
  (insert "#+TITLE: Announcement verification\n")
  (dolist (result results)
    (insert (format "\n* %s\n\n" (org-canvas--announcement-verify-summary result)))
    (dolist (row (plist-get result :rows))
      (insert (format "- %-7s %-9s :: Org %s; Canvas %s\n"
                      (if (plist-get row :ok) "ok" "DIFFERS")
                      (plist-get row :field) (plist-get row :org)
                      (plist-get row :canvas))))))

(defun org-canvas--announcement-verify-ask ()
  "Read the title of an announcement heading to verify, as a heading entry."
  (list (list org-canvas--announcement-verify-feature-name
              (org-canvas--sync-heading-ask "announcements"
                                            org-canvas-announcements-file
                                            "LEVEL=1" "verify")
              'title)))

;;;###autoload
(defun org-canvas-verify-announcements (&optional entries)
  "Read pushed announcements back from Canvas; return what differs.
ENTRIES name the headings as `org-canvas-show-headings' takes them:
\(\"announcements\" . TITLE) or (\"announcements\" TARGET BY), BY
`title' or `canvas-id'.  Interactively, a title is asked for.

Each topic is read on its own by CANVAS_ID and compared on its title,
PUBLISHED, its posting (DELAYED_POST_AT against `delayed_post_at' and
`posted_at': scheduled for that minute when it is ahead, otherwise
posted with nothing still scheduled), ALLOW_COMMENTS against `locked',
and the body, exported offline, against `message' as text (issue
#502).  Nothing is written, locally or on Canvas.

Returns one plist per heading: :title, :id, :rows (each with :field,
:ok, :org and :canvas), :error (a heading not found, never pushed or
gone from Canvas) and :differences, the rows that disagree, or 1 on
an error.  So zero :differences is an announcement that landed as the
heading says.  The report is shown, or printed under `noninteractive',
and a line per heading is echoed."
  (interactive)
  (org-canvas--preflight-check)
  (let* ((parsed (org-canvas--announcement-verify-entries
                  (or entries (org-canvas--announcement-verify-ask))))
         (feature (org-canvas--announcement-verify-feature))
         (now (current-time))
         (results (mapcar (lambda (e) (org-canvas--announcement-verify-one feature e now))
                          parsed)))
    (org-canvas--report-display
     org-canvas--announcement-verify-buffer
     (lambda () (org-canvas--announcement-verify-render results))
     #'org-mode)
    (dolist (result results)
      (org-canvas--user-message "Verify %s" (org-canvas--announcement-verify-summary result)))
    results))

(provide 'org-canvas-announcements-verify)
;;; org-canvas-announcements-verify.el ends here
