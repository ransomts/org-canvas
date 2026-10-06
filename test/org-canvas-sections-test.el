;;; org-canvas-sections-test.el --- Buttercup tests for sections  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-sections)

;; Bound by the group and student override specs; the modules that
;; define them are not loaded here, so declare them special.
(defvar org-canvas-groups-file)
(defvar org-canvas-people-file)
(defvar org-canvas-assignments-file)

;;;; ================================================================
;;;; Section Pull Tests
;;;; ================================================================

;;;; Helper: find heading by CANVAS_ID

(describe "org-canvas--section-find-heading-by-id"
  (it "finds heading with matching CANVAS_ID"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:CANVAS_ID: 111
:END:

* Section B
:PROPERTIES:
:CANVAS_ID: 222
:END:
"
     (let ((marker (org-canvas--section-find-heading-by-id "222")))
       (expect marker :to-be-truthy)
       (goto-char (marker-position marker))
       (expect (org-get-heading t t t t) :to-equal "Section B"))))

  (it "returns nil when no matching CANVAS_ID"
    (with-temp-org-buffer
     "* Section A
:PROPERTIES:
:CANVAS_ID: 111
:END:
"
     (expect (org-canvas--section-find-heading-by-id "999") :to-be nil)))

  (it "returns nil in empty buffer"
    (with-temp-org-buffer
     ""
     (expect (org-canvas--section-find-heading-by-id "111") :to-be nil))))

;;;; Pull Sections

(describe "org-canvas-pull-sections"
  (it "creates new headings from Canvas sections"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file))
            ;; Create empty file
            (with-temp-file org-file (insert ""))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           [((id . 100) (name . "Section A")
                             (start_at . "2026-01-15T00:00:00Z")
                             (end_at . "2026-05-15T00:00:00Z")
                             (restrict_enrollments_to_section_dates . t))
                            ((id . 200) (name . "Section B")
                             (start_at . nil)
                             (end_at . nil)
                             (restrict_enrollments_to_section_dates . :json-false))])))
                (org-canvas-pull-sections)
                (with-current-buffer (find-file-noselect org-file)
                  ;; Two headings should exist
                  (goto-char (point-min))
                  (expect (org-map-entries (lambda () t) "LEVEL=1" 'file)
                          :to-have-same-items-as '(t t))
                  ;; First section
                  (goto-char (point-min))
                  (re-search-forward "^\\* " nil t)
                  (org-back-to-heading)
                  (expect (org-get-heading t t t t) :to-equal "Section A")
                  (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100")
                  (expect (org-entry-get (point) "START_AT") :to-match "^<2026-01-15")
                  (expect (org-entry-get (point) "END_AT") :to-match "^<2026-05-15")
                  (expect (org-entry-get (point) "RESTRICT_TO_DATES") :to-equal "true")
                  ;; Per-entry LAST_SYNCED no longer written; the file-level
                  ;; #+LAST_SYNCED header is written instead
                  (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
                  (expect (org-canvas--pull-read-file-header) :to-be-truthy)))))
        (delete-directory temp-dir t))))

  (it "updates properties on existing headings without changing heading name"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file))
            (with-temp-file org-file
              (insert "* My Custom Name for Section A
:PROPERTIES:
:CANVAS_ID: 100
:RESTRICT_TO_DATES: false
:END:
"))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           [((id . 100) (name . "Section A - MWF")
                             (start_at . "2026-01-15T00:00:00Z")
                             (end_at . "2026-05-15T00:00:00Z")
                             (restrict_enrollments_to_section_dates . t))]))
                        ((symbol-function 'y-or-n-p) (lambda (_) t)))
                (org-canvas-pull-sections)
                (with-current-buffer (find-file-noselect org-file)
                  (goto-char (point-min))
                  (re-search-forward "^\\* " nil t)
                  (org-back-to-heading)
                  ;; Heading name should be preserved
                  (expect (org-get-heading t t t t)
                          :to-equal "My Custom Name for Section A")
                  ;; Properties should be updated
                  (expect (org-entry-get (point) "RESTRICT_TO_DATES") :to-equal "true")
                  (expect (org-entry-get (point) "START_AT") :to-match "^<2026-01-15")
                  (expect (org-entry-get (point) "END_AT") :to-match "^<2026-05-15")
                  ;; File-level #+LAST_SYNCED header is written instead of
                  ;; per-entry property
                  (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
                  (expect (org-canvas--pull-read-file-header) :to-be-truthy)))))
        (delete-directory temp-dir t))))

  (it "creates new headings for sections not yet in file"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file))
            (with-temp-file org-file
              (insert "* Existing Section
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           [((id . 100) (name . "Existing Section")
                             (start_at . nil) (end_at . nil)
                             (restrict_enrollments_to_section_dates . :json-false))
                            ((id . 200) (name . "New Section")
                             (start_at . nil) (end_at . nil)
                             (restrict_enrollments_to_section_dates . :json-false))]))
                        ((symbol-function 'y-or-n-p) (lambda (_) t)))
                (org-canvas-pull-sections)
                (with-current-buffer (find-file-noselect org-file)
                  (let ((headings nil))
                    (org-map-entries
                     (lambda ()
                       (push (org-get-heading t t t t) headings))
                     "LEVEL=1" 'file)
                    (expect (length headings) :to-equal 2)
                    (expect (member "New Section" headings) :to-be-truthy))))))
        (delete-directory temp-dir t))))

  (it "warns about stale local headings"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file)
                 (warning-messages nil))
            (with-temp-file org-file
              (insert "* Stale Section
:PROPERTIES:
:CANVAS_ID: 999
:END:
"))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args) []))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) warning-messages)))
                        ((symbol-function 'y-or-n-p) (lambda (_) t)))
                (org-canvas-pull-sections)
                (expect (cl-some (lambda (msg)
                                   (string-match-p "Stale section" msg))
                                 warning-messages)
                        :to-be-truthy))))
        (delete-directory temp-dir t))))

  (it "handles empty API response gracefully"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file))
            (with-temp-file org-file (insert ""))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args) [])))
                (org-canvas-pull-sections)
                (with-current-buffer (find-file-noselect org-file)
                  ;; No headings should have been created
                  (expect (org-map-entries (lambda () t) "LEVEL=1" 'file)
                          :to-equal nil)))))
        (delete-directory temp-dir t))))

  (it "creates headings from sections that span multiple pages"
    ;; Regression: the previous implementation hard-coded ?per_page=100&page=1,
    ;; silently truncating courses with more than 100 sections.  Verify that a
    ;; section appearing only on page 2 still ends up in sections.org.
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file)
                 (calls 0))
            (with-temp-file org-file (insert ""))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           (cl-incf calls)
                           (pcase calls
                             (1 (vconcat
                                 (cl-loop for i from 1 to 100
                                          collect `((id . ,i)
                                                    (name . ,(format "Sec %d" i))
                                                    (start_at . nil) (end_at . nil)
                                                    (restrict_enrollments_to_section_dates
                                                     . :json-false)))))
                             (2 (vector '((id . 999)
                                          (name . "Page-2 Only Section")
                                          (start_at . nil) (end_at . nil)
                                          (restrict_enrollments_to_section_dates
                                           . :json-false))))
                             (_ (vector))))))
                (org-canvas-pull-sections)
                (expect calls :to-equal 2)
                (with-current-buffer (find-file-noselect org-file)
                  (goto-char (point-min))
                  (let ((found nil))
                    (org-map-entries
                     (lambda ()
                       (when (string= (org-entry-get (point) "CANVAS_ID") "999")
                         (setq found (org-get-heading t t t t))))
                     "LEVEL=1" 'file)
                    (expect found :to-equal "Page-2 Only Section"))))))
        (delete-directory temp-dir t))))

  (it "creates sections file when it does not exist on disk"
    ;; Regression: a fresh pull where sections.org is missing AND its path
    ;; is in `org-agenda-files' used to fail with `(user-error "Abort")'
    ;; from `org-check-agenda-file'.  The pull must pre-create the file
    ;; before `find-file-noselect' so org-mode never sees a non-existent
    ;; agenda file.
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file)
                 (org-agenda-files (list org-file)))
            ;; NOTE: do not pre-create the file.
            (expect (file-exists-p org-file) :to-be nil)
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           [((id . 100) (name . "Section A")
                             (start_at . nil) (end_at . nil)
                             (restrict_enrollments_to_section_dates . :json-false))]))
                        ;; Fail loudly if org tries to validate the agenda
                        ;; file before the pull pre-creates it.
                        ((symbol-function 'org-check-agenda-file)
                         (lambda (file)
                           (unless (file-exists-p file)
                             (error "org-check-agenda-file fired on missing file: %s"
                                    file)))))
                (org-canvas-pull-sections)
                (expect (file-exists-p org-file) :to-be-truthy)
                (with-current-buffer (find-file-noselect org-file)
                  (goto-char (point-min))
                  (re-search-forward "^\\* " nil t)
                  (org-back-to-heading)
                  (expect (org-entry-get (point) "CANVAS_ID")
                          :to-equal "100")))))
        (delete-directory temp-dir t))))

  (it "aborts when user declines overwrite of existing file"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file))
            (with-temp-file org-file
              (insert "* Existing Section\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            ;; `noninteractive' is t under the test runner and now skips
            ;; the prompt (issue #34); bind it off to test the decline.
            (with-sync-test-env
              (let ((noninteractive nil))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                  (expect (org-canvas-pull-sections) :to-throw 'user-error)))))
        (let ((buf (find-buffer-visiting
                    (expand-file-name "sections.org" temp-dir))))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "errors when sections file directory does not exist"
    (let ((org-canvas-sections-file "/nonexistent/dir/sections.org"))
      (with-sync-test-env
        (expect (org-canvas-pull-sections) :to-throw 'error))))

  (it "sets RESTRICT_TO_DATES to false for non-true values"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file))
            (with-temp-file org-file (insert ""))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           [((id . 100) (name . "Test")
                             (start_at . nil) (end_at . nil)
                             (restrict_enrollments_to_section_dates . :json-false))])))
                (org-canvas-pull-sections)
                (with-current-buffer (find-file-noselect org-file)
                  (goto-char (point-min))
                  (re-search-forward "^\\* " nil t)
                  (org-back-to-heading)
                  (expect (org-entry-get (point) "RESTRICT_TO_DATES")
                          :to-equal "false")))))
        (delete-directory temp-dir t))))

  (it "skips nil timestamps without setting properties"
    (let ((temp-dir (make-temp-file "sections-pull" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "sections.org" temp-dir))
                 (org-canvas-sections-file org-file))
            (with-temp-file org-file (insert ""))
            (with-sync-test-env
              (cl-letf (((symbol-function 'org-canvas-api-request)
                         (lambda (_method _url &rest _args)
                           [((id . 100) (name . "No Dates")
                             (start_at . nil) (end_at . nil)
                             (restrict_enrollments_to_section_dates . :json-false))])))
                (org-canvas-pull-sections)
                (with-current-buffer (find-file-noselect org-file)
                  (goto-char (point-min))
                  (re-search-forward "^\\* " nil t)
                  (org-back-to-heading)
                  (expect (org-entry-get (point) "START_AT") :to-be nil)
                  (expect (org-entry-get (point) "END_AT") :to-be nil)))))
        (delete-directory temp-dir t)))))

;;;; ================================================================
;;;; Override Tests
;;;; ================================================================

;;;; Override Table Finding

(describe "org-canvas--override-find-table"
  (it "finds overrides table within subtree"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:CANVAS_ID: 100
:END:

#+NAME: overrides
| Section   | Due At           | Unlock At | Lock At |
|-----------+------------------+-----------+---------|
| Section A | <2026-02-15 Sun> |           |         |
"
     (org-back-to-heading)
     (let ((end (save-excursion (org-end-of-subtree t) (point)))
           (table nil))
       (setq table (org-canvas--override-find-table end))
       (expect table :to-be-truthy)
       ;; Should have header + hline + 1 data row
       (expect (length table) :to-be-greater-than 2))))

  (it "returns nil when no overrides table"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:CANVAS_ID: 100
:END:

Just some body text.
"
     (org-back-to-heading)
     (let ((end (save-excursion (org-end-of-subtree t) (point))))
       (expect (org-canvas--override-find-table end) :to-be nil))))

  (it "does not find table outside subtree"
    (with-temp-org-buffer
     "* Assignment 1
:PROPERTIES:
:CANVAS_ID: 100
:END:

Body text.

* Assignment 2
:PROPERTIES:
:END:

#+NAME: overrides
| Section   | Due At           | Unlock At | Lock At |
|-----------+------------------+-----------+---------|
| Section A | <2026-02-15 Sun> |           |         |
"
     (org-back-to-heading)
     (let ((end (save-excursion (org-end-of-subtree t) (point))))
       (expect (org-canvas--override-find-table end) :to-be nil)))))

;;;; Override Section ID Resolution

(describe "org-canvas--override-resolve-section-id"
  (it "resolves section ID from link"
    (let ((temp-dir (make-temp-file "sections-test" t)))
      (unwind-protect
          (let ((sections-file (expand-file-name "sections.org" temp-dir)))
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 777
:END:
"))
            (let ((link (format "[[file:sections.org::*Section A][Section A]]")))
              (expect (org-canvas--override-resolve-section-id link temp-dir)
                      :to-equal "777")))
        (delete-directory temp-dir t))))

  (it "returns nil for missing section"
    (let ((temp-dir (make-temp-file "sections-test" t)))
      (unwind-protect
          (let ((sections-file (expand-file-name "sections.org" temp-dir)))
            (with-temp-file sections-file
              (insert "* Section B
:PROPERTIES:
:END:
"))
            (let ((link "[[file:sections.org::*Section A][Section A]]"))
              (expect (org-canvas--override-resolve-section-id link temp-dir)
                      :to-be nil)))
        (delete-directory temp-dir t))))

  (it "returns nil for non-existent file"
    (expect (org-canvas--override-resolve-section-id
             "[[file:missing.org::*Foo][Foo]]" "/tmp/nonexistent/")
            :to-be nil))

  (it "returns nil for non-link text"
    (expect (org-canvas--override-resolve-section-id
             "Just plain text" "/tmp/")
            :to-be nil)))

;;;; Override Timestamp Cell Parsing

(describe "org-canvas--override-parse-timestamp-cell"
  (it "parses Org timestamp"
    (let ((result (org-canvas--override-parse-timestamp-cell "<2026-02-15 Sun>")))
      (expect result :to-be-truthy)
      (expect result :to-match "^2026-02-15T")))

  (it "returns nil for empty string"
    (expect (org-canvas--override-parse-timestamp-cell "") :to-be nil))

  (it "returns nil for whitespace-only string"
    (expect (org-canvas--override-parse-timestamp-cell "   ") :to-be nil))

  (it "returns nil for non-timestamp text"
    (expect (org-canvas--override-parse-timestamp-cell "not a timestamp") :to-be nil)))

;;;; Override Table Parsing

(describe "org-canvas--override-parse-table"
  (it "parses complete override table"
    (let ((temp-dir (make-temp-file "sections-test" t)))
      (unwind-protect
          (let ((sections-file (expand-file-name "sections.org" temp-dir)))
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 100
:END:

* Section B
:PROPERTIES:
:CANVAS_ID: 200
:END:
"))
            (let ((table (list
                          '("Section" "Due At" "Unlock At" "Lock At")
                          'hline
                          (list "[[file:sections.org::*Section A][Section A]]"
                                "<2026-02-15 Sun>" "<2026-02-01 Sat>" "")
                          (list "[[file:sections.org::*Section B][Section B]]"
                                "<2026-02-12 Thu>" "" "<2026-02-20 Fri>"))))
              (let ((overrides (org-canvas--override-parse-table table temp-dir)))
                (expect (length overrides) :to-equal 2)
                ;; First override
                (expect (plist-get (nth 0 overrides) :section-id) :to-equal "100")
                (expect (plist-get (nth 0 overrides) :due-at) :to-match "^2026-02-15T")
                (expect (plist-get (nth 0 overrides) :unlock-at) :to-match "^2026-02-01T")
                (expect (plist-get (nth 0 overrides) :lock-at) :to-be nil)
                ;; Second override
                (expect (plist-get (nth 1 overrides) :section-id) :to-equal "200")
                (expect (plist-get (nth 1 overrides) :due-at) :to-match "^2026-02-12T")
                (expect (plist-get (nth 1 overrides) :unlock-at) :to-be nil)
                (expect (plist-get (nth 1 overrides) :lock-at) :to-match "^2026-02-20T"))))
        (delete-directory temp-dir t))))

  (it "skips rows with unresolvable section links"
    (let ((temp-dir (make-temp-file "sections-test" t)))
      (unwind-protect
          (let ((sections-file (expand-file-name "sections.org" temp-dir)))
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((table (list
                          '("Section" "Due At" "Unlock At" "Lock At")
                          'hline
                          (list "[[file:sections.org::*Section A][Section A]]"
                                "<2026-02-15 Sun>" "" "")
                          (list "[[file:sections.org::*Missing][Missing]]"
                                "<2026-02-15 Sun>" "" ""))))
              (let ((overrides (org-canvas--override-parse-table table temp-dir)))
                (expect (length overrides) :to-equal 1)
                (expect (plist-get (nth 0 overrides) :section-id) :to-equal "100"))))
        (delete-directory temp-dir t))))

  (it "skips hline rows"
    (let ((temp-dir (make-temp-file "sections-test" t)))
      (unwind-protect
          (let ((sections-file (expand-file-name "sections.org" temp-dir)))
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 100
:END:
"))
            (let ((table (list
                          '("Section" "Due At" "Unlock At" "Lock At")
                          'hline
                          'hline
                          (list "[[file:sections.org::*Section A][Section A]]"
                                "<2026-02-15 Sun>" "" ""))))
              (let ((overrides (org-canvas--override-parse-table table temp-dir)))
                (expect (length overrides) :to-equal 1))))
        (delete-directory temp-dir t)))))

;;;; Override Payload Building

(describe "org-canvas--override-build-payload"
  (it "includes course_section_id as number"
    (let* ((override '(:section-id "100" :due-at "2026-02-15T00:00:00Z"))
           (payload (org-canvas--override-build-payload override))
           (inner (alist-get 'assignment_override payload)))
      (expect (alist-get 'course_section_id inner) :to-equal 100)))

  (it "includes due_at when present"
    (let* ((override '(:section-id "100" :due-at "2026-02-15T00:00:00Z"))
           (payload (org-canvas--override-build-payload override))
           (inner (alist-get 'assignment_override payload)))
      (expect (alist-get 'due_at inner) :to-equal "2026-02-15T00:00:00Z")))

  (it "includes unlock_at when present"
    (let* ((override '(:section-id "100" :unlock-at "2026-02-01T00:00:00Z"))
           (payload (org-canvas--override-build-payload override))
           (inner (alist-get 'assignment_override payload)))
      (expect (alist-get 'unlock_at inner) :to-equal "2026-02-01T00:00:00Z")))

  (it "includes lock_at when present"
    (let* ((override '(:section-id "100" :lock-at "2026-02-20T00:00:00Z"))
           (payload (org-canvas--override-build-payload override))
           (inner (alist-get 'assignment_override payload)))
      (expect (alist-get 'lock_at inner) :to-equal "2026-02-20T00:00:00Z")))

  (it "omits optional dates when nil"
    (let* ((override '(:section-id "100"))
           (payload (org-canvas--override-build-payload override))
           (inner (alist-get 'assignment_override payload)))
      (expect (assq 'due_at inner) :to-be nil)
      (expect (assq 'unlock_at inner) :to-be nil)
      (expect (assq 'lock_at inner) :to-be nil))))

;;;; Override Sync for Assignment

(describe "org-canvas--override-sync-for-assignment"
  (it "creates new overrides"
    (with-org-canvas-test-config
      (let ((api-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (push method api-calls)
                     (cond
                      ((eq method 'GET) [])  ; no existing overrides
                      ((eq method 'POST) '((id . 1)))))))
          (let ((overrides (list (list :section-id "100"
                                      :due-at "2026-02-15T00:00:00Z"))))
            (let ((counts (org-canvas--override-sync-for-assignment "456" overrides)))
              (expect (nth 0 counts) :to-equal 1)  ; created
              (expect (nth 1 counts) :to-equal 0)  ; updated
              (expect (nth 2 counts) :to-equal 0))))))) ; deleted

  (it "updates existing overrides"
    (with-org-canvas-test-config
      (let ((api-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (push method api-calls)
                     (cond
                      ((eq method 'GET)
                       [((id . 10) (course_section_id . 100)
                         (due_at . "2026-02-10T00:00:00Z"))])
                      ((eq method 'PUT) '((id . 10)))))))
          (let ((overrides (list (list :section-id "100"
                                      :due-at "2026-02-15T00:00:00Z"))))
            (let ((counts (org-canvas--override-sync-for-assignment "456" overrides)))
              (expect (nth 0 counts) :to-equal 0)  ; created
              (expect (nth 1 counts) :to-equal 1)  ; updated
              (expect (nth 2 counts) :to-equal 0))))))) ; deleted

  (it "deletes removed overrides"
    (with-org-canvas-test-config
      (let ((api-calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (push method api-calls)
                     (cond
                      ((eq method 'GET)
                       [((id . 10) (course_section_id . 100)
                         (due_at . "2026-02-10T00:00:00Z"))
                        ((id . 20) (course_section_id . 200)
                         (due_at . "2026-02-12T00:00:00Z"))])
                      ((eq method 'PUT) '((id . 10)))
                      ((eq method 'DELETE) nil)))))
          ;; Only keep section 100, section 200 should be deleted
          (let ((overrides (list (list :section-id "100"
                                      :due-at "2026-02-15T00:00:00Z"))))
            (let ((counts (org-canvas--override-sync-for-assignment "456" overrides)))
              (expect (nth 0 counts) :to-equal 0)  ; created
              (expect (nth 1 counts) :to-equal 1)  ; updated
              (expect (nth 2 counts) :to-equal 1))))))) ; deleted

  (it "handles API errors gracefully during create"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _args)
                   (cond
                    ((eq method 'GET) [])
                    ((eq method 'POST)
                     (signal 'error '("API Error")))))))
        (let ((overrides (list (list :section-id "100"
                                    :due-at "2026-02-15T00:00:00Z"))))
          ;; Should not throw - errors are caught internally
          (let ((counts (org-canvas--override-sync-for-assignment "456" overrides)))
            (expect (nth 0 counts) :to-equal 0))))))

  (it "handles no existing overrides"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (method _url &rest _args)
                   (cond
                    ((eq method 'GET) [])
                    ((eq method 'POST) '((id . 1)))))))
        (let ((overrides (list (list :section-id "100"
                                    :due-at "2026-02-15T00:00:00Z")
                               (list :section-id "200"
                                     :due-at "2026-02-12T00:00:00Z"))))
          (let ((counts (org-canvas--override-sync-for-assignment "456" overrides)))
            (expect (nth 0 counts) :to-equal 2)
            (expect (nth 1 counts) :to-equal 0)
            (expect (nth 2 counts) :to-equal 0)))))))

;;;; Override Sync Integration

(describe "org-canvas-sync-overrides (mocked)"
  (it "processes assignments with override tables"
    (let ((temp-dir (make-temp-file "override-test" t)))
      (unwind-protect
          (let* ((assignments-file (expand-file-name "assignments.org" temp-dir))
                 (sections-file (expand-file-name "sections.org" temp-dir))
                 (post-count 0))
            (with-temp-file sections-file
              (insert "* Section A
:PROPERTIES:
:CANVAS_ID: 777
:END:
"))
            (with-temp-file assignments-file
              (insert "* Assignment 1
:PROPERTIES:
:CANVAS_ID: 456
:END:

#+NAME: overrides
| Section                                          | Due At           | Unlock At | Lock At |
|--------------------------------------------------+------------------+-----------+---------|
| [[file:sections.org::*Section A][Section A]]     | <2026-02-15 Sun> |           |         |
"))
            (let ((org-canvas-assignments-file assignments-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (cond
                              ((eq method 'GET) [])
                              ((eq method 'POST)
                               (setq post-count (1+ post-count))
                               '((id . 1)))))))
                  (org-canvas-sync-overrides)
                  (expect post-count :to-equal 1)))))
        (delete-directory temp-dir t))))

  (it "skips assignments without override tables"
    (let ((temp-dir (make-temp-file "override-test" t)))
      (unwind-protect
          (let* ((assignments-file (expand-file-name "assignments.org" temp-dir))
                 (api-call-count 0))
            (with-temp-file assignments-file
              (insert "* Assignment 1
:PROPERTIES:
:CANVAS_ID: 456
:END:

Just a description, no override table.
"))
            (let ((org-canvas-assignments-file assignments-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             (setq api-call-count (1+ api-call-count))
                             nil)))
                  (org-canvas-sync-overrides)
                  ;; No API calls should be made for assignments without tables
                  (expect api-call-count :to-equal 0)))))
        (delete-directory temp-dir t))))

  (it "skips assignments without CANVAS_ID"
    (let ((temp-dir (make-temp-file "override-test" t)))
      (unwind-protect
          (let* ((assignments-file (expand-file-name "assignments.org" temp-dir))
                 (api-call-count 0))
            (with-temp-file assignments-file
              (insert "* New Assignment
:PROPERTIES:
:END:

#+NAME: overrides
| Section   | Due At           | Unlock At | Lock At |
|-----------+------------------+-----------+---------|
| Section A | <2026-02-15 Sun> |           |         |
"))
            (let ((org-canvas-assignments-file assignments-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             (setq api-call-count (1+ api-call-count))
                             nil)))
                  (org-canvas-sync-overrides)
                  (expect api-call-count :to-equal 0)))))
        (delete-directory temp-dir t))))

  (it "errors when assignments file not found"
    (let ((org-canvas-assignments-file "/nonexistent/assignments.org"))
      (expect (org-canvas-sync-overrides) :to-throw 'error))))

;;;; Additional Coverage Tests

(describe "org-canvas--pull-sections-upsert newline guard"
  (it "inserts newline before heading when buffer lacks trailing newline"
    (with-temp-org-buffer
     "* Existing Section
:PROPERTIES:
:CANVAS_ID: 1
:END:"
     ;; No trailing newline
     (org-canvas--pull-sections-upsert
      '((id . 2) (name . "New Section") (start_at . nil)
        (end_at . nil) (restrict_enrollments_to_section_dates . nil)))
     (expect (buffer-string) :to-match "\\* New Section"))))

(describe "org-canvas--override-delete-removed error handling"
  (it "handles delete API error gracefully"
    (with-org-canvas-test-config
      (let ((deleted 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (when (eq method 'DELETE)
                       (signal 'error '("DELETE failed"))))))
          ;; Should not throw, just log and return (0 . 0)
          (setq deleted (org-canvas--override-delete-removed
                         "https://test.canvas.example.com/api/v1/courses/99999/assignments/1/overrides"
                         '(((id . 5) (course_section_id . 99)))
                         '(1 2 3)))
          (expect deleted :to-equal '(0 . 0)))))))

(describe "org-canvas-sync-overrides assignments fallback path"
  (it "falls back to org-canvas--path when assignments-file var unbound"
    (let* ((temp-dir (make-temp-file "override-test" t))
           (assign-file (expand-file-name "assignments.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file assign-file
              (insert "* Assignment
:PROPERTIES:
:CANVAS_ID: 1
:END:
"))
            (let ((org-canvas-directory temp-dir)
                  (org-canvas-assignments-file assign-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (_method _url &rest _args) nil)))
                    ;; Canvas answering nothing deletes nothing and throws
                    ;; nothing: the reconcile is a no-op, not a failure
                    (expect (org-canvas-sync-overrides) :not :to-throw))))))
        (let ((buf (find-buffer-visiting assign-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

;;;; Override Sync Preflight Tests

(describe "org-canvas--override-sync-preflight"
  (it "falls back to org-canvas--path when org-canvas-assignments-file unbound"
    (let ((old-val org-canvas-assignments-file)
          (org-canvas-directory "/tmp/nonexistent-dir-for-test/"))
      (makunbound 'org-canvas-assignments-file)
      (unwind-protect
          (expect (org-canvas--override-sync-preflight) :to-throw 'error)
        (setq org-canvas-assignments-file old-val)))))

(describe "org-canvas--override-sync-for-assignment error handling"
  (it "warns (treats remote as empty) when the override fetch fails"
    (with-org-canvas-test-config
      (spy-on 'org-canvas--log-warning)
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) (signal 'error '("HTTP 500")))))
        ;; Empty local overrides: after the failed fetch there is nothing to
        ;; create/delete, so the only observable effect is the warning.
        (org-canvas--override-sync-for-assignment "7" nil)
        (let ((warned nil))
          (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
            (when (and (>= (length call) 2) (stringp (nth 1 call))
                       (string-match-p "treating remote as empty" (nth 1 call)))
              (setq warned t)))
          (expect warned :to-be t))))))

;;;; Override dry run (issue #34)
;;
;; Overrides reconcile with PUT/POST/DELETE outside the shared push helper,
;; so they had the same hole the files module did: a preview rewrote and
;; removed real overrides on Canvas.

(describe "org-canvas--override-sync-for-assignment dry run"
  (it "reports updates and deletions without issuing them"
    (with-org-canvas-test-config
      (let ((calls nil)
            (logged nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     ;; Section 1 is still in the table (update); section 999
                     ;; has been removed from it (delete).
                     '(((id . 10) (course_section_id . 1))
                       ((id . 20) (course_section_id . 999)))))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (push (cons method url) calls)
                     nil))
                  ((symbol-function 'org-canvas--log-info)
                   (lambda (_logger fmt &rest args)
                     (push (apply #'format fmt args) logged))))
          (let* ((org-canvas--dry-run t)
                 (counts (org-canvas--override-sync-for-assignment
                          "42" '((:section-id "1" :due-at "2026-10-19T23:59:00Z")))))
            ;; Counts still describe the plan: 0 created, 1 updated, 1 deleted.
            (expect counts :to-equal '(0 1 1 0))
            (expect calls :to-equal nil)
            (expect (seq-filter (lambda (l) (string-match-p "Would UPDATE override" l))
                                logged)
                    :not :to-equal nil)
            (expect (seq-filter (lambda (l) (string-match-p "Would DELETE override" l))
                                logged)
                    :not :to-equal nil))))))

  (it "issues the requests when not a dry run"
    ;; The mirror of the above, so the guard cannot be left permanently on.
    (with-org-canvas-test-config
      (let ((calls nil))
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (lambda (_method _url &optional _params)
                     '(((id . 10) (course_section_id . 1))
                       ((id . 20) (course_section_id . 999)))))
                  ((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (push (cons method url) calls)
                     nil)))
          (let ((org-canvas--dry-run nil))
            (org-canvas--override-sync-for-assignment
             "42" '((:section-id "1" :due-at "2026-10-19T23:59:00Z")))
            (expect (seq-filter (lambda (c) (eq (car c) 'PUT)) calls)
                    :not :to-equal nil)
            (expect (seq-filter (lambda (c) (eq (car c) 'DELETE)) calls)
                    :not :to-equal nil)))))))

;;;; Mutation hardening (issue #38)

(describe "org-canvas--override-sync-for-assignment reported counts"
  ;; The returned (CREATED UPDATED DELETED) triple was never asserted with
  ;; more than one item of a kind, so flipping the `1+' counters survived.
  (it "counts creates, updates and deletes independently"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (_method _url &optional _params)
                   ;; Sections 1 and 2 exist remotely (so: updates);
                   ;; 998 and 999 are gone from the table (so: deletes).
                   '(((id . 11) (course_section_id . 1))
                     ((id . 12) (course_section_id . 2))
                     ((id . 98) (course_section_id . 998))
                     ((id . 99) (course_section_id . 999)))))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) nil)))
        (let ((counts (org-canvas--override-sync-for-assignment
                       "42"
                       '((:section-id "1" :due-at "2026-10-19T23:59:00Z")
                         (:section-id "2" :due-at "2026-10-20T23:59:00Z")
                         (:section-id "3" :due-at "2026-10-21T23:59:00Z")))))
          ;; One create (section 3), two updates (1 and 2), two deletes.
          (expect counts :to-equal '(1 2 2 0))))))

  (it "counts nothing when the table is empty and the remote is too"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) nil)))
        (expect (org-canvas--override-sync-for-assignment "42" nil)
                :to-equal '(0 0 0 0)))))

  (it "does not count a create whose request failed"
    (with-org-canvas-test-config
      (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _)
                   (org-canvas--signal 'org-canvas-api-error "rejected"))))
        (expect (org-canvas--override-sync-for-assignment
                 "42" '((:section-id "1" :due-at "2026-10-19T23:59:00Z")))
                :to-equal '(0 0 0 0))))))

(describe "org-canvas-pull-sections reported counts"
  ;; created/updated are only visible in the closing message, which no
  ;; test read — so both `1+' counters survived being flipped.
  (defun test-sections--pull-message (upsert-results)
    "Pull with `org-canvas--pull-sections-upsert' returning UPSERT-RESULTS.
Returns the final message string."
    (let ((dir (make-temp-file "pull-" t))
          (said nil)
          (remaining upsert-results))
      (unwind-protect
          (let ((sections-file (expand-file-name "sections.org" dir)))
            (with-temp-file sections-file (insert ""))
            (let ((org-canvas-sections-file sections-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                             (lambda (&rest _)
                               (let ((n 0))
                                 (mapcar (lambda (_r)
                                           (setq n (1+ n))
                                           `((id . ,n) (name . ,(format "S%d" n))))
                                         upsert-results))))
                            ((symbol-function 'org-canvas--pull-sections-upsert)
                             (lambda (_section) (pop remaining)))
                            ((symbol-function 'org-canvas--pull-sections-warn-stale) #'ignore)
                            ((symbol-function 'org-canvas--pull-write-file-header) #'ignore)
                            ((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (setq said (apply #'format fmt args)))))
                    (org-canvas-pull-sections))))))
        (let ((buf (find-buffer-visiting (expand-file-name "sections.org" dir))))
          (when buf
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (delete-directory dir t))
      said))

  (it "counts created and updated sections separately"
    (expect (test-sections--pull-message '(created updated updated))
            :to-equal "Section pull: 1 created, 2 updated."))

  (it "reports zeros for an empty remote"
    (expect (test-sections--pull-message '())
            :to-equal "Section pull: 0 created, 0 updated.")))

(describe "org-canvas-sync-overrides reported totals"
  ;; `assignments-processed' is only visible in the closing message, so
  ;; its `1+' survived.  The totals tell the user how much of their
  ;; overrides table actually reached Canvas.
  (it "counts each assignment that carried an overrides table"
    (let ((dir (make-temp-file "ovr-" t))
          (said nil))
      (unwind-protect
          (let ((assignments-file (expand-file-name "assignments.org" dir)))
            (with-temp-file assignments-file
              (insert "* First
:PROPERTIES:
:CANVAS_ID: 1
:END:

#+NAME: overrides
| Section | Due At |
|---------+--------|
| S1      | <2026-10-19 Mon 23:59> |

* Second
:PROPERTIES:
:CANVAS_ID: 2
:END:

#+NAME: overrides
| Section | Due At |
|---------+--------|
| S1      | <2026-10-20 Tue 23:59> |

* No Table
:PROPERTIES:
:CANVAS_ID: 3
:END:
"))
            (let ((org-canvas-assignments-file assignments-file))
              (with-org-canvas-test-config
                (with-sync-test-env
                  (cl-letf (((symbol-function 'org-canvas--override-parse-table)
                             (lambda (&rest _) '((:section-id "7"))))
                            ((symbol-function 'org-canvas--override-sync-for-assignment)
                             (lambda (&rest _) '(1 0 0 0)))
                            ((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (setq said (apply #'format fmt args)))))
                    (org-canvas-sync-overrides))))))
        (let ((buf (find-buffer-visiting (expand-file-name "assignments.org" dir))))
          (when buf
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (delete-directory dir t))
      ;; Two assignments have tables; the third must not be counted.
      ;; Full equality, not a substring match: "2 assignments" also
      ;; matches "-2 assignments", so a negated counter would slip past.
      (expect said :to-equal
              "Override sync: 2 assignments, 2 created, 0 updated, 0 deleted."))))

(describe "org-canvas--override-row-redundant-p"
  ;; No test touched this predicate directly, so both `or's and both
  ;; `equal's in it survived.  It decides whether an override row is
  ;; worth emitting at all: a row that merely repeats the parent's dates
  ;; is noise, but dropping one that differs would lose real information.
  (it "is redundant when every populated date matches the parent"
    (expect (org-canvas--override-row-redundant-p
             '((due_at . "2026-02-15T23:59:00Z"))
             "2026-02-15T23:59:00Z" nil nil)
            :to-be-truthy))

  (it "is redundant when the row carries no dates at all"
    (expect (org-canvas--override-row-redundant-p
             '((course_section_id . 1)) nil nil nil)
            :to-be-truthy))

  (it "is not redundant when the due date differs"
    (expect (org-canvas--override-row-redundant-p
             '((due_at . "2026-03-28T23:59:00Z"))
             "2026-02-15T23:59:00Z" nil nil)
            :to-be nil))

  (it "is not redundant when only the unlock date differs"
    ;; Each date is checked independently; a difference in any one of
    ;; them has to keep the row.
    (expect (org-canvas--override-row-redundant-p
             '((due_at . "2026-02-15T23:59:00Z")
               (unlock_at . "2026-02-01T00:00:00Z"))
             "2026-02-15T23:59:00Z" "2026-01-01T00:00:00Z" nil)
            :to-be nil))

  (it "is not redundant when only the lock date differs"
    (expect (org-canvas--override-row-redundant-p
             '((due_at . "2026-02-15T23:59:00Z")
               (lock_at . "2026-03-01T00:00:00Z"))
             "2026-02-15T23:59:00Z" nil "2026-04-01T00:00:00Z")
            :to-be nil))

  (it "is redundant when a populated unlock date equals the parent's"
    ;; The discriminating case for `(equal unlock parent-unlock)': every
    ;; other spec either leaves unlock nil (so `null' short-circuits) or
    ;; makes it differ.  These are ISO8601 *strings* from separate API
    ;; responses, so `eq' would compare identity and report a difference
    ;; that is not there — keeping rows that say nothing.
    (expect (org-canvas--override-row-redundant-p
             '((due_at . "2026-02-15T23:59:00Z")
               (unlock_at . "2026-01-01T00:00:00Z"))
             "2026-02-15T23:59:00Z" "2026-01-01T00:00:00Z" nil)
            :to-be-truthy))

  (it "is redundant when a populated lock date equals the parent's"
    (expect (org-canvas--override-row-redundant-p
             '((due_at . "2026-02-15T23:59:00Z")
               (lock_at . "2026-03-01T00:00:00Z"))
             "2026-02-15T23:59:00Z" nil "2026-03-01T00:00:00Z")
            :to-be-truthy))

  (it "treats a nil date on the row as no difference"
    ;; nil means "the override says nothing about this date", which is
    ;; not the same as "differs from the parent".
    (expect (org-canvas--override-row-redundant-p
             '((due_at . "2026-02-15T23:59:00Z") (unlock_at . nil))
             "2026-02-15T23:59:00Z" "2026-01-01T00:00:00Z" nil)
            :to-be-truthy)))

(describe "org-canvas--pull-sections-warn-stale"
  ;; `(and local-id (not (member local-id remote-ids)))' survived
  ;; `and'->`or': no test had a heading without a CANVAS_ID, so the
  ;; left-hand guard was never the deciding factor.
  (it "warns about a heading whose id is gone from Canvas"
    (with-temp-org-buffer
     "* Stale Section
:PROPERTIES:
:CANVAS_ID: 999
:END:
"
     (let ((warnings nil))
       (cl-letf (((symbol-function 'org-canvas--log-warning)
                  (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings)))
                 ((symbol-function 'message) (lambda (&rest _) nil)))
         (org-canvas--pull-sections-warn-stale '("100" "200"))
         (expect (length warnings) :to-equal 1)
         (expect (car warnings) :to-match "999")))))

  (it "stays silent for a heading that has no CANVAS_ID"
    ;; An unsynced heading is not stale — it was never on Canvas.
    (with-temp-org-buffer
     "* Locally Added Section
"
     (let ((warnings nil))
       (cl-letf (((symbol-function 'org-canvas--log-warning)
                  (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings)))
                 ((symbol-function 'message) (lambda (&rest _) nil)))
         (org-canvas--pull-sections-warn-stale '("100"))
         (expect warnings :to-be nil)))))

  (it "stays silent when the id is still on Canvas"
    (with-temp-org-buffer
     "* Live Section
:PROPERTIES:
:CANVAS_ID: 100
:END:
"
     (let ((warnings nil))
       (cl-letf (((symbol-function 'org-canvas--log-warning)
                  (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings)))
                 ((symbol-function 'message) (lambda (&rest _) nil)))
         (org-canvas--pull-sections-warn-stale '("100"))
         (expect warnings :to-be nil))))))

(describe "org-canvas--override-emit-table empty input"
  ;; `(> (length overrides) 0)' survived mutation to `>=': no test passed
  ;; an empty list, so the guard was never exercised at its boundary.
  (it "emits nothing for an empty override list"
    (with-temp-buffer
      (org-canvas--override-emit-table '() nil nil nil)
      (expect (buffer-string) :to-equal "")))

  (it "emits nothing for nil"
    (with-temp-buffer
      (org-canvas--override-emit-table nil nil nil nil)
      (expect (buffer-string) :to-equal "")))

  (it "emits a table for a single override"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . 1) (due_at . "2026-03-28T23:59:00Z")))
         nil nil nil))
      (expect (buffer-string) :to-match "#\\+NAME: overrides"))))

(describe "org-canvas--section-link-by-id with a string id"
  (it "coerces a non-numeric id to its string when unresolved"
    (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
      (expect (org-canvas--section-link-by-id "296338") :to-equal "296338"))))

(describe "org-canvas--override-emit-table lock column"
  (it "keeps the Lock At column when an override sets lock_at"
    (with-temp-buffer
      (let ((org-canvas-sections-file "/tmp/nonexistent-sections-xyzzy.org"))
        (org-canvas--override-emit-table
         '(((id . 1) (course_section_id . 1) (lock_at . "2026-03-28T23:59:00Z")))
         nil nil nil))
      (expect (buffer-string) :to-match "| Section | Lock At |")
      (expect (buffer-string) :not :to-match "Due At")
      (expect (buffer-string) :not :to-match "Unlock At")
      (expect (buffer-string) :to-match "| 1 | .*2026-03-2[89].* |"))))


;;;; Student and Group Overrides (issue #224)

(defmacro test-sections--with-lookup-files (&rest body)
  "Run BODY with a groups.org and a people.org in a temp dir.
Group `Team A' has CANVAS_ID 55; `Adams, Alice' has USER_ID 1 and
`Beta, Bob' USER_ID 2.  The sections file holds `Section A' as 100."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "override-kinds-" t))
          (org-canvas-groups-file (expand-file-name "groups.org" temp-dir))
          (org-canvas-people-file (expand-file-name "people.org" temp-dir))
          (org-canvas-sections-file (expand-file-name "sections.org" temp-dir)))
     (with-temp-file org-canvas-groups-file
       (insert "* Project Teams\n:PROPERTIES:\n:CANVAS_ID: 9\n:END:\n"
               "** Team A\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"))
     (with-temp-file org-canvas-people-file
       (insert "* Students\n** Adams, Alice\n:PROPERTIES:\n:USER_ID: 1\n:END:\n"
               "** Beta, Bob\n:PROPERTIES:\n:USER_ID: 2\n:END:\n"))
     (with-temp-file org-canvas-sections-file
       (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
     (unwind-protect
         (progn ,@body)
       (dolist (f (list org-canvas-groups-file org-canvas-people-file org-canvas-sections-file))
         (let ((buf (find-buffer-visiting f)))
           (when buf (kill-buffer buf))))
       (delete-directory temp-dir t))))

(describe "org-canvas--override-parse-target (issue #224)"
  (it "resolves a group by its groups.org title, or a literal #id"
    (test-sections--with-lookup-files
      (expect (org-canvas--override-parse-target "Group: Team A" temp-dir)
              :to-equal '(:group-id "55"))
      (expect (org-canvas--override-parse-target "Group: #77" temp-dir)
              :to-equal '(:group-id "77"))))

  (it "resolves students by their people.org titles, semicolon separated, or #id"
    (test-sections--with-lookup-files
      (expect (org-canvas--override-parse-target "Students: Adams, Alice; #9; Beta, Bob" temp-dir)
              :to-equal '(:student-ids ("1" "9" "2")))
      (expect (org-canvas--override-parse-target "Student: Beta, Bob" temp-dir)
              :to-equal '(:student-ids ("2")))))

  (it "still resolves a section link"
    (test-sections--with-lookup-files
      (expect (org-canvas--override-parse-target
               "[[file:sections.org::*Section A][Section A]]" temp-dir)
              :to-equal '(:section-id "100"))))

  (it "answers nil with one warning naming what did not resolve"
    (test-sections--with-lookup-files
      (let ((warned nil))
        (cl-letf (((symbol-function 'org-canvas--log-warning)
                   (lambda (_l fmt &rest args) (push (apply #'format fmt args) warned))))
          (expect (org-canvas--override-parse-target "Group: Team Z" temp-dir) :to-be nil)
          (expect (org-canvas--override-parse-target "Students: Adams, Alice; Nobody, Nell" temp-dir)
                  :to-be nil)
          (expect (org-canvas--override-parse-target "Plain" temp-dir) :to-be nil))
        (expect (length warned) :to-equal 3)
        (expect (nth 2 warned) :to-match "group 'Team Z'")
        (expect (nth 1 warned) :to-match "'Nobody, Nell'")
        (expect (nth 1 warned) :not :to-match "Alice")
        (expect (nth 0 warned) :to-match "section ID from: Plain"))))

  (it "resolves nothing but a literal #id when the lookup files are missing"
    (let ((org-canvas-groups-file "/tmp/nonexistent-groups-xyzzy.org")
          (org-canvas-people-file "/tmp/nonexistent-people-xyzzy.org"))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
        (expect (org-canvas--override-parse-target "Group: Team A" "/tmp") :to-be nil)
        (expect (org-canvas--override-parse-target "Group: #55" "/tmp")
                :to-equal '(:group-id "55"))))))

(describe "org-canvas--override-parse-table by header (issue #224)"
  (it "finds the date columns by their titles, so a pulled two-column table parses"
    (test-sections--with-lookup-files
      (let ((overrides (org-canvas--override-parse-table
                        (list '("Section" "Lock At") 'hline
                              (list "Group: Team A" "<2026-02-20 Fri>")
                              (list "Students: Adams, Alice" "<2026-02-21 Sat>"))
                        temp-dir)))
        (expect (length overrides) :to-equal 2)
        (expect (plist-get (nth 0 overrides) :group-id) :to-equal "55")
        (expect (plist-get (nth 0 overrides) :lock-at) :to-match "^2026-02-20T")
        (expect (plist-get (nth 0 overrides) :due-at) :to-be nil)
        (expect (plist-get (nth 0 overrides) :unlock-at) :to-be nil)
        (expect (plist-get (nth 1 overrides) :student-ids) :to-equal '("1")))))

  (it "falls back to positions when the header does not name the columns"
    (test-sections--with-lookup-files
      (let ((overrides (org-canvas--override-parse-table
                        (list '("Who" "When" "Opens" "Closes") 'hline
                              (list "Group: #5" "<2026-02-15 Sun>" "" "<2026-02-20 Fri>"))
                        temp-dir)))
        (expect (plist-get (car overrides) :due-at) :to-match "^2026-02-15T")
        (expect (plist-get (car overrides) :unlock-at) :to-be nil)
        (expect (plist-get (car overrides) :lock-at) :to-match "^2026-02-20T")))))

(describe "org-canvas--override-build-payload by kind (issue #224)"
  (it "sends group_id for a group row"
    (let ((inner (alist-get 'assignment_override
                            (org-canvas--override-build-payload
                             '(:group-id "55" :due-at "2026-02-15T00:00:00Z")))))
      (expect (alist-get 'group_id inner) :to-equal 55)
      (expect (assq 'course_section_id inner) :to-be nil)))

  (it "sends student_ids as a JSON array for a student row"
    (let* ((payload (org-canvas--override-build-payload '(:student-ids ("1" "2"))))
           (inner (alist-get 'assignment_override payload)))
      (expect (alist-get 'student_ids inner) :to-equal [1 2])
      (expect (json-encode payload) :to-match "\"student_ids\":\\[1,2\\]"))))

(describe "org-canvas--override-find-existing (issue #224)"
  (let ((existing '(((id . 10) (course_section_id . 100))
                    ((id . 20) (group_id . 55))
                    ((id . 30) (student_ids . [2 1])))))
    (it "matches a section row, a group row and a student row by kind"
      (expect (alist-get 'id (org-canvas--override-find-existing '(:section-id "100") existing))
              :to-equal 10)
      (expect (alist-get 'id (org-canvas--override-find-existing '(:group-id "55") existing))
              :to-equal 20)
      (expect (alist-get 'id (org-canvas--override-find-existing '(:student-ids ("1" "2")) existing))
              :to-equal 30))

    (it "treats a changed set of students as a different override"
      (expect (org-canvas--override-find-existing '(:student-ids ("1")) existing) :to-be nil)
      (expect (org-canvas--override-find-existing '(:student-ids ("1" "2" "3")) existing) :to-be nil))))

(describe "org-canvas--override-sync-for-assignment by kind (issue #224)"
  (defun test-sections--reconcile (existing overrides)
    "Reconcile OVERRIDES against EXISTING through a fake API.
Returns (COUNTS . CALLS), CALLS being (METHOD . URL-TAIL) in order."
    (let ((calls nil))
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest _args)
                     (push (cons method (car (last (split-string url "/")))) calls)
                     (cond ((eq method 'GET) (vconcat existing))
                           ((eq method 'POST) '((id . 99)))
                           (t nil)))))
          (cons (org-canvas--override-sync-for-assignment "456" overrides)
                (nreverse calls))))))

  (it "keeps a student's extension the table carries and deletes only what it dropped"
    (let* ((result (test-sections--reconcile
                    '(((id . 10) (course_section_id . 100) (due_at . "2026-02-10T00:00:00Z"))
                      ((id . 30) (student_ids . [7]) (due_at . "2026-02-20T00:00:00Z"))
                      ((id . 40) (group_id . 55)))
                    '((:section-id "100" :due-at "2026-02-15T00:00:00Z")
                      (:student-ids ("7") :due-at "2026-02-22T00:00:00Z")
                      (:student-ids ("8") :due-at "2026-02-22T00:00:00Z"))))
           (counts (car result)) (calls (cdr result)))
      (expect counts :to-equal '(1 2 1 0))
      (expect (member '(PUT . "10") calls) :to-be-truthy)
      (expect (member '(PUT . "30") calls) :to-be-truthy)
      (expect (member '(POST . "overrides") calls) :to-be-truthy)
      (expect (member '(DELETE . "40") calls) :to-be-truthy)
      (expect (member '(DELETE . "30") calls) :to-be nil)))

  (it "recreates a student override whose set of students changed"
    (let* ((result (test-sections--reconcile
                    '(((id . 30) (student_ids . [7 8])))
                    '((:student-ids ("7") :due-at "2026-02-22T00:00:00Z"))))
           (counts (car result)) (calls (cdr result)))
      (expect counts :to-equal '(1 0 1 0))
      (expect (member '(POST . "overrides") calls) :to-be-truthy)
      (expect (member '(DELETE . "30") calls) :to-be-truthy)))

  (it "writes nothing under a dry run and still reports the counts"
    (let* ((org-canvas--dry-run t)
           (result (test-sections--reconcile
                    '(((id . 40) (group_id . 55)))
                    '((:group-id "55" :due-at "2026-02-22T00:00:00Z")
                      (:student-ids ("7") :due-at "2026-02-22T00:00:00Z")))))
      (expect (car result) :to-equal '(1 1 0 0))
      (expect (mapcar #'car (cdr result)) :to-equal '(GET)))))

(describe "org-canvas--override-target-cell (issue #224)"
  (it "renders a group and students by title, #id when unresolved, and everyone otherwise"
    (test-sections--with-lookup-files
      (expect (org-canvas--override-target-cell '((id . 1) (group_id . 55)))
              :to-equal "Group: Team A")
      (expect (org-canvas--override-target-cell '((id . 1) (group_id . 56)))
              :to-equal "Group: #56")
      (expect (org-canvas--override-target-cell '((id . 1) (student_ids . [2 9])))
              :to-equal "Students: Beta, Bob; #9")
      (expect (org-canvas--override-target-cell '((id . 1) (course_section_id . 100)))
              :to-equal "[[file:sections.org::*Section A][Section A]]")
      (expect (org-canvas--override-target-cell '((id . 1) (student_ids . [])))
              :to-equal "All Sections")))

  (it "round-trips a pulled table with every kind back into the same overrides"
    (test-sections--with-lookup-files
      (with-temp-buffer
        (org-canvas--override-emit-table
         '(((id . 1) (group_id . 55) (due_at . "2026-02-20T23:59:00Z"))
           ((id . 2) (student_ids . [1 2]) (lock_at . "2026-02-21T23:59:00Z")))
         nil nil nil)
        (goto-char (point-min))
        (forward-line 1)
        (let ((overrides (org-canvas--override-parse-table (org-table-to-lisp) temp-dir)))
          (expect (plist-get (nth 0 overrides) :group-id) :to-equal "55")
          (expect (plist-get (nth 0 overrides) :due-at) :to-match "^2026-02-2")
          (expect (plist-get (nth 1 overrides) :student-ids) :to-equal '("1" "2"))
          (expect (plist-get (nth 1 overrides) :lock-at) :to-match "^2026-02-2")
          (expect (plist-get (nth 1 overrides) :unlock-at) :to-be nil))))))

(describe "org-canvas--override-existing-label"
  (it "names the section, group, students or everyone"
    (expect (org-canvas--override-existing-label '((course_section_id . 100))) :to-equal "section 100")
    (expect (org-canvas--override-existing-label '((group_id . 55))) :to-equal "group 55")
    (expect (org-canvas--override-existing-label '((student_ids . [1 2]))) :to-equal "students 1, 2")
    (expect (org-canvas--override-existing-label '((id . 3))) :to-equal "everyone")))

;;;; The Assignment's Baseline After Override Writes (issue #348)

(defconst test-ovr348-row
  "| [[file:sections.org::*Section A][Section A]] | <2026-02-15 Sun> | | |\n"
  "An overrides row for Section A, whose section id is 777.")

(defun test-ovr348-run (stamp before after rows &optional dry-run)
  "Run `org-canvas-sync-overrides' on one heading stamped STAMP.
The assignment's `updated_at' is BEFORE until an override write
lands and AFTER from then on.  ROWS is the table's body.  DRY-RUN
binds `org-canvas--dry-run'.  Return (FILE-TEXT . API-CALLS)."
  (let ((dir (make-temp-file "ovr348-" t)))
    (unwind-protect
        (let ((file (expand-file-name "assignments.org" dir)))
          (with-temp-file (expand-file-name "sections.org" dir)
            (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 777\n:END:\n"))
          (with-temp-file file
            (insert "* Assignment 1\n:PROPERTIES:\n:CANVAS_ID: 456\n"
                    (format ":CANVAS_UPDATED_AT: %s\n" stamp)
                    ":PAYLOAD_HASH: abc123\n:END:\n\n#+NAME: overrides\n"
                    "| Section | Due At | Unlock At | Lock At |\n"
                    "|---------+--------+-----------+---------|\n"
                    rows))
          (let ((org-canvas-assignments-file file)
                (org-canvas--dry-run dry-run))
            (with-org-canvas-test-config
              (with-sync-test-env
                (with-mock-api
                  (setq test-org-canvas-api-responses
                        `(("assignments/456/overrides" . [])
                          ("assignments/456\\'"
                           . ((id . 456) (updated_at . ,before)))))
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest args)
                               (prog1 (apply #'test-org-canvas-mock-api-request
                                             method url args)
                                 (unless (eq method 'GET)
                                   (push `("assignments/456\\'"
                                           . ((id . 456)
                                              (updated_at . ,after)))
                                         test-org-canvas-api-responses))))))
                    (org-canvas-sync-overrides))
                  (let ((buf (find-buffer-visiting file)))
                    (when buf
                      (with-current-buffer buf (set-buffer-modified-p nil))
                      (kill-buffer buf)))
                  (cons (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))
                        test-org-canvas-api-calls))))))
      (delete-directory dir t))))

(defun test-ovr348-assignment-reads (calls)
  "Return the GETs of the assignment itself among CALLS."
  (cl-remove-if-not (lambda (call)
                      (and (eq (car call) 'GET)
                           (string-match-p "assignments/456\\'" (cadr call))))
                    calls))

(describe "org-canvas-sync-overrides restamps the assignment (issue #348)"
  (it "restamps a clean heading after an override write and keeps the hash"
    (let* ((result (test-ovr348-run "2026-09-25T02:19:07Z"
                                    "2026-09-25T02:19:07Z"
                                    "2026-09-25T02:44:10Z"
                                    test-ovr348-row))
           (reads (test-ovr348-assignment-reads (cdr result))))
      (expect (car result)
              :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:44:10Z")
      (expect (car result) :to-match ":PAYLOAD_HASH: abc123")
      (expect (length reads) :to-equal 2)
      ;; Both reads carry the registry's item params (issue #273).
      (dolist (call reads)
        (expect (plist-get (nth 3 call) :params)
                :to-equal '(("override_assignment_dates" . "false"))))))

  (it "neither restamps nor re-reads when nothing was written"
    (let* ((result (test-ovr348-run "2026-09-25T02:19:07Z"
                                    "2026-09-25T02:19:07Z"
                                    "2026-09-25T02:44:10Z"
                                    ""))
           (reads (test-ovr348-assignment-reads (cdr result))))
      (expect (car result)
              :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:19:07Z")
      (expect (length reads) :to-equal 1)
      (expect (cl-remove-if (lambda (call) (eq (car call) 'GET)) (cdr result))
              :to-equal nil)))

  (it "keeps the stamp of a heading that had drifted before the writes"
    (let ((result (test-ovr348-run "2026-09-25T02:19:07Z"
                                   "2026-09-25T02:30:00Z"
                                   "2026-09-25T02:44:10Z"
                                   test-ovr348-row)))
      (expect (car result)
              :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:19:07Z")
      (expect (length (test-ovr348-assignment-reads (cdr result)))
              :to-equal 1)
      (expect (cl-find-if (lambda (call) (eq (car call) 'POST)) (cdr result))
              :not :to-be nil)))

  (it "reads and writes nothing about the assignment under a dry run"
    (let ((result (test-ovr348-run "2026-09-25T02:19:07Z"
                                   "2026-09-25T02:19:07Z"
                                   "2026-09-25T02:44:10Z"
                                   test-ovr348-row t)))
      (expect (car result)
              :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:19:07Z")
      (expect (test-ovr348-assignment-reads (cdr result)) :to-equal nil)
      (expect (cl-remove-if (lambda (call) (eq (car call) 'GET)) (cdr result))
              :to-equal nil)))

  (it "leaves a heading with no stamp of its own unread and unstamped"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer "* A\n:PROPERTIES:\n:CANVAS_ID: 456\n:END:\n"
          (expect (org-canvas--override-baseline-clean-p (point) "456")
                  :to-be nil)
          (expect test-org-canvas-api-calls :to-equal nil)))))

  (it "answers nil and warns when the assignment read fails"
    (let ((warned nil))
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (&rest _) (error "Boom")))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (&rest _) (setq warned t))))
          (expect (org-canvas--override-read-assignment "456") :to-be nil)
          (expect warned :to-be t)))))

  (it "reads the course endpoint when no assignments feature is registered"
    (with-org-canvas-test-config
      (with-mock-api
        (cl-letf (((symbol-function 'org-canvas--registry-find-feature)
                   (lambda (_) nil)))
          (setq test-org-canvas-api-responses
                '(("assignments/456" . ((updated_at . "2026-01-01T00:00:00Z")))))
          (expect (org-canvas--override-read-assignment "456")
                  :to-equal "2026-01-01T00:00:00Z")
          (expect (plist-get (nth 3 (car test-org-canvas-api-calls)) :params)
                  :to-be nil)))))

  (it "warns instead of signalling when the stamp cannot be written"
    (let ((warned nil))
      (with-org-canvas-test-config
        (cl-letf (((symbol-function 'org-canvas--override-read-assignment)
                   (lambda (_) "2026-01-01T00:00:00Z"))
                  ((symbol-function 'org-canvas-org-set-property)
                   (lambda (&rest _) (error "Buffer is stale")))
                  ((symbol-function 'org-canvas--log-warning)
                   (lambda (&rest _) (setq warned t))))
          (org-canvas--override-restamp (point-min) "456" "A")
          (expect warned :to-be t))))))

;;;; ================================================================
;;;; Meeting Times and Section Windows (issue #383)
;;;; ================================================================

(describe "org-canvas--section-meets-parse"
  (it "reads run-together day letters and a clock range"
    (expect (org-canvas--section-meets-parse "MWF 10:10-11:00")
            :to-equal '(((1 3 5) 610 660)))
    (expect (org-canvas--section-meets-parse "TTh 13:25 - 14:15")
            :to-equal '(((2 4) 805 855)))
    (expect (org-canvas--section-meets-parse "UMTWRFS 0:00-23:59")
            :to-equal '(((0 1 2 3 4 5 6) 0 1439))))

  (it "reads day names and several patterns separated by semicolons"
    (expect (org-canvas--section-meets-parse "Mon Wed 9:05-9:55; Friday 10:00-10:50")
            :to-equal '(((1 3) 545 595) ((5) 600 650)))
    (expect (org-canvas--section-meets-parse "tue/thu 12:20-13:10")
            :to-equal '(((2 4) 740 790))))

  (it "refuses the whole value when any pattern does not parse"
    (dolist (bad '("MWF 10-11" "Xyz 10:00-11:00" "M 11:00-10:00" "M 25:00-26:00"
                   "M 10:61-11:00" "Mo 10:00-11:00" "MWF 10:10-11:00; oops" "" nil))
      (expect (org-canvas--section-meets-parse bad) :to-be nil))))

(describe "org-canvas--section-keys"
  (it "names a linked section by its sections.org heading and a plain one by its text"
    (expect (org-canvas--section-keys
             "[[file:sections.org::*Section 101][CPSC 2921 101]], Lecture 100")
            :to-equal '("Section 101" "Lecture 100")))

  (it "unescapes brackets and falls back to a link's description"
    (expect (org-canvas--section-keys
             "[[file:sections.org::*Lab \\[A\\]][Lab]], [[https://x.test/s/1][Studio]]")
            :to-equal '("Lab [A]" "Studio"))
    (expect (org-canvas--section-keys "[[https://x.test/s/2]]")
            :to-equal '("https://x.test/s/2")))

  (it "returns nothing for an empty value"
    (expect (org-canvas--section-keys "") :to-be nil)))

(defmacro with-overrides-file (content &rest body)
  "Bind `org-canvas-assignments-file' to a scratch file holding CONTENT; run BODY."
  (declare (indent 1))
  `(let* ((file (make-temp-file "org-canvas-assign-" nil ".org"))
          (org-canvas-assignments-file file))
     (unwind-protect
         (progn (with-temp-file file (insert ,content)) ,@body)
       (when-let* ((buf (get-file-buffer file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-file file))))

(describe "org-canvas--override-section-windows"
  (it "reads each section row's Unlock At to Lock At, or to Due At without a lock"
    (with-overrides-file
        (concat "* Other\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
                "* Attendance 01\n:PROPERTIES:\n:CANVAS_ID: 2573836\n:END:\n\n"
                "#+NAME: overrides\n"
                "| Section | Due At | Unlock At | Lock At |\n|-\n"
                "| [[file:sections.org::*Section 101][Section 101]] | <2026-09-18 Fri 09:55> | <2026-09-18 Fri 09:05> | <2026-09-18 Fri 10:00> |\n"
                "| [[file:sections.org::*Section 102][Section 102]] | <2026-09-18 Fri 11:00> | <2026-09-18 Fri 10:10> |  |\n"
                "| [[file:sections.org::*Section 103][Section 103]] | <2026-09-18 Fri 11:00> |  |  |\n"
                "| Group: Team 1 | <2026-09-18 Fri 11:00> | <2026-09-18 Fri 10:10> |  |\n"
                "| Students: Doe, Jane | <2026-09-18 Fri 11:00> | <2026-09-18 Fri 10:10> |  |\n")
      (expect (org-canvas--override-section-windows "2573836")
              :to-equal '(("Section 101" "<2026-09-18 Fri 09:05>" "<2026-09-18 Fri 10:00>")
                          ("Section 102" "<2026-09-18 Fri 10:10>" "<2026-09-18 Fri 11:00>")))
      (expect (org-canvas--override-section-windows "1") :to-be nil)
      (expect (org-canvas--override-section-windows "999") :to-be nil)))

  (it "finds the columns by their titles when a pulled table dropped one"
    (with-overrides-file
        (concat "* Closer\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n"
                "#+NAME: overrides\n| Section | Unlock At | Lock At |\n|-\n"
                "| Lecture 100 | <2026-09-28 Mon 10:10> | <2026-09-28 Mon 11:00> |\n")
      (expect (org-canvas--override-section-windows 7)
              :to-equal '(("Lecture 100" "<2026-09-28 Mon 10:10>" "<2026-09-28 Mon 11:00>")))))

  (it "is nil when there is no assignments file"
    (let ((org-canvas-assignments-file "/nonexistent/assignments.org"))
      (expect (org-canvas--override-section-windows "1") :to-be nil))))

(describe "org-canvas--pull-sections-upsert and MEETS"
  (it "keeps a MEETS typed on the heading, since a pull sets only its own properties"
    (with-temp-org-buffer
     "* Section 101\n:PROPERTIES:\n:CANVAS_ID: 11\n:MEETS: F 09:05-09:55\n:END:\n"
     (org-canvas--pull-sections-upsert
      '((id . 11) (name . "CPSC 2921 101") (start_at . nil)
        (end_at . nil) (restrict_enrollments_to_section_dates . :json-false)))
     (goto-char (point-min))
     (expect (org-entry-get (point) "MEETS") :to-equal "F 09:05-09:55")
     (expect (org-get-heading t t t t) :to-equal "Section 101"))))

;;;; One Heading's Overrides, and No Unchanged PUT (issue #380)

(describe "org-canvas--override-same-time-p (issue #380)"
  (it "agrees on one instant however the zone is spelt"
    (expect (org-canvas--override-same-time-p
             "2026-09-28T14:20:00-04:00" "2026-09-28T18:20:00Z")
            :to-be-truthy))

  (it "disagrees on two instants"
    (expect (org-canvas--override-same-time-p
             "2026-09-28T18:15:00Z" "2026-09-28T18:20:00Z")
            :to-be nil))

  (it "agrees on two blanks and never on one blank"
    (expect (org-canvas--override-same-time-p nil nil) :to-be-truthy)
    (expect (org-canvas--override-same-time-p "" nil) :to-be-truthy)
    (expect (org-canvas--override-same-time-p nil "2026-09-28T18:20:00Z")
            :to-be nil)
    (expect (org-canvas--override-same-time-p "2026-09-28T18:20:00Z" nil)
            :to-be nil))

  (it "agrees with nothing when a string does not parse"
    (expect (org-canvas--override-same-time-p "not a date" "not a date")
            :to-be nil)))

(describe "org-canvas--override-dates-match-p (issue #380)"
  (it "matches when all three dates agree, blanks with absences"
    (expect (org-canvas--override-dates-match-p
             '(:section-id "1" :due-at "2026-09-28T18:20:00Z" :lock-at nil)
             '((id . 10) (course_section_id . 1) (due_at . "2026-09-28T18:20:00Z")))
            :to-be-truthy))

  (it "does not match when the override holds a date the row leaves blank"
    (expect (org-canvas--override-dates-match-p
             '(:section-id "1" :due-at "2026-09-28T18:20:00Z")
             '((id . 10) (due_at . "2026-09-28T18:20:00Z")
               (lock_at . "2026-09-28T19:00:00Z")))
            :to-be nil))

  (it "does not match when one date moved"
    (expect (org-canvas--override-dates-match-p
             '(:section-id "1" :due-at "2026-09-28T18:20:00Z")
             '((id . 10) (due_at . "2026-09-28T18:15:00Z")))
            :to-be nil)))

(describe "org-canvas--override-sync-for-assignment skips unchanged (issue #380)"
  (it "sends no PUT for a claimed override that already carries the row's dates"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments/456/overrides" .
                 [((id . 10) (course_section_id . 100)
                   (due_at . "2026-09-28T18:20:00Z"))
                  ((id . 20) (course_section_id . 200)
                   (due_at . "2026-09-28T18:15:00Z"))])))
        (let ((counts (org-canvas--override-sync-for-assignment
                       "456"
                       '((:section-id "100" :due-at "2026-09-28T14:20:00-04:00")
                         (:section-id "200" :due-at "2026-09-28T18:20:00Z")))))
          (expect counts :to-equal '(0 1 0 0))
          (expect (test-org-canvas-api-called-p 'PUT "overrides/10\\'") :to-be nil)
          (expect (test-org-canvas-api-called-p 'PUT "overrides/20\\'") :to-be-truthy)
          ;; The unchanged override is still claimed, so it is not deleted.
          (expect (test-org-canvas-api-called-p 'DELETE "overrides") :to-be nil))))))

(defconst test-ovr380-file
  (concat "* Attendance 02\n:PROPERTIES:\n:CANVAS_ID: 456\n"
          ":CANVAS_UPDATED_AT: 2026-09-28T10:00:00Z\n:END:\n\n"
          "#+NAME: overrides\n"
          "| Section | Due At | Unlock At | Lock At |\n"
          "|---------+--------+-----------+---------|\n"
          "| [[file:sections.org::*Section A][Section A]] | <2026-09-28 Mon 14:20> | | |\n"
          "* Attendance 03\n:PROPERTIES:\n:CANVAS_ID: 457\n:END:\n\n"
          "#+NAME: overrides\n"
          "| Section | Due At | Unlock At | Lock At |\n"
          "|---------+--------+-----------+---------|\n"
          "| [[file:sections.org::*Section A][Section A]] | <2026-09-30 Wed 14:20> | | |\n"
          "* No table\n:PROPERTIES:\n:CANVAS_ID: 458\n:END:\n"
          "* Not pushed yet\n\n#+NAME: overrides\n"
          "| Section | Due At | Unlock At | Lock At |\n"
          "| [[file:sections.org::*Section A][Section A]] | <2026-09-30 Wed 14:20> | | |\n")
  "Four assignment headings: two with tables, one without, one unstamped.")

(defun test-ovr380-call-in-file (heading fn)
  "Call FN at HEADING of a scratch assignments.org holding `test-ovr380-file'.
A sections.org beside it names Section A, id 777.  Return FN's value."
  (let ((dir (make-temp-file "ovr380-" t)))
    (unwind-protect
        (let ((file (expand-file-name "assignments.org" dir)))
          (with-temp-file (expand-file-name "sections.org" dir)
            (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 777\n:END:\n"))
          (with-temp-file file (insert test-ovr380-file))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (progn
                  (goto-char (point-min))
                  (re-search-forward (concat "^\\* " (regexp-quote heading)))
                  (funcall fn))
              (set-buffer-modified-p nil)
              (kill-buffer)
              (let ((sections (find-buffer-visiting
                               (expand-file-name "sections.org" dir))))
                (when sections (kill-buffer sections))))))
      (delete-directory dir t))))

(describe "org-canvas--override-sync-entry (issue #380)"
  (it "reconciles only the heading it is given"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments/456/overrides" .
                 [((id . 10) (course_section_id . 777)
                   (due_at . "2026-09-28T18:15:00Z"))])
                ("assignments/456\\'" .
                 ((id . 456) (updated_at . "2026-09-28T10:00:00Z")))))
        (expect (test-ovr380-call-in-file
                 "Attendance 02"
                 (lambda () (org-canvas--override-sync-entry (point-marker))))
                :to-equal '(0 1 0 0))
        (progn
          (expect (test-org-canvas-api-called-p 'PUT "assignments/456/overrides/10")
                  :to-be-truthy)
          (expect (test-org-canvas-api-called-p 'GET "assignments/457")
                  :to-be nil)))))

  (it "answers nil with no request for a heading without a table or a stamp"
    (with-org-canvas-test-config
      (with-mock-api
        (dolist (heading '("No table" "Not pushed yet"))
          (expect (test-ovr380-call-in-file
                   heading
                   (lambda () (org-canvas--override-sync-entry (point-marker))))
                  :to-be nil))
        (expect test-org-canvas-api-calls :to-equal nil)))))

;;;; An Override-Only Drift Restamps the Heading (issue #410)

(defun test-ovr410-run (mismatches &optional dry-run confirm)
  "Reconcile one drifted heading by heading; return (FILE-TEXT . API-CALLS).
The heading is stamped 02:19:07 and Canvas holds the assignment as
updated at 02:30:00 before the override write and 02:44:10 after, so
the baseline is dirty when the reconcile starts.  MISMATCHES is what
the `:mismatch-fn' answers, nil meaning the heading's own fields agree
with Canvas.  DRY-RUN binds `org-canvas--dry-run'; CONFIRM passes
`:confirm-deletes'."
  (let ((dir (make-temp-file "ovr410-" t)))
    (unwind-protect
        (let ((file (expand-file-name "assignments.org" dir)))
          (with-temp-file (expand-file-name "sections.org" dir)
            (insert "* Section A\n:PROPERTIES:\n:CANVAS_ID: 777\n:END:\n"))
          (with-temp-file file
            (insert "* Assignment 1\n:PROPERTIES:\n:CANVAS_ID: 456\n"
                    ":CANVAS_UPDATED_AT: 2026-09-25T02:19:07Z\n"
                    ":PAYLOAD_HASH: abc123\n:END:\n\n#+NAME: overrides\n"
                    "| Section | Due At | Unlock At | Lock At |\n"
                    "|---------+--------+-----------+---------|\n"
                    test-ovr348-row))
          (let ((org-canvas-assignments-file file)
                (org-canvas--dry-run dry-run)
                (seen-items nil))
            (with-org-canvas-test-config
              (with-sync-test-env
                (with-mock-api
                  (setq test-org-canvas-api-responses
                        '(("assignments/456/overrides" . [])
                          ("assignments/456\\'"
                           . ((id . 456) (points_possible . 1)
                              (updated_at . "2026-09-25T02:30:00Z")))))
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (method url &rest args)
                               (prog1 (apply #'test-org-canvas-mock-api-request
                                             method url args)
                                 (unless (eq method 'GET)
                                   (push '("assignments/456\\'"
                                           . ((id . 456) (points_possible . 1)
                                              (updated_at . "2026-09-25T02:44:10Z")))
                                         test-org-canvas-api-responses))))))
                    (with-current-buffer (org-canvas--find-file-noselect file)
                      (goto-char (point-min))
                      (org-canvas--override-sync-entry
                       (point-marker) nil
                       (list :mismatch-fn (lambda (item)
                                            (push item seen-items)
                                            mismatches)
                             :confirm-deletes confirm))
                      (org-canvas--save-buffer)))
                  (let ((buf (find-buffer-visiting file)))
                    (when buf
                      (with-current-buffer buf (set-buffer-modified-p nil))
                      (kill-buffer buf)))
                  (list (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))
                        test-org-canvas-api-calls
                        seen-items))))))
      (delete-directory dir t))))

(describe "org-canvas--override-sync-heading on a drifted heading (issue #410)"
  (it "restamps after the writes when the heading's own fields agree with Canvas"
    (let* ((result (test-ovr410-run nil))
           (reads (test-ovr348-assignment-reads (nth 1 result))))
      (expect (nth 0 result)
              :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:44:10Z")
      (expect (nth 0 result) :to-match ":PAYLOAD_HASH: abc123")
      ;; One read before the writes, one after: the comparison used
      ;; the second, made after the override landed.
      (expect (length reads) :to-equal 2)
      (expect (length (nth 2 result)) :to-equal 1)
      (expect (alist-get 'updated_at (car (nth 2 result)))
              :to-equal "2026-09-25T02:44:10Z")))

  (it "keeps the stamp when a field of the heading differs, naming it"
    (let ((logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged))))
        (let ((result (test-ovr410-run '(("POINTS" "1" "2")))))
          (expect (nth 0 result)
                  :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:19:07Z")
          (expect (length (test-ovr348-assignment-reads (nth 1 result)))
                  :to-equal 2)))
      (expect (cl-find-if (lambda (l) (string-match-p "POINTS (org 1, canvas 2)" l))
                          logged)
              :to-be-truthy)))

  (it "asks nothing of the heading under a dry run"
    (let ((result (test-ovr410-run nil t)))
      (expect (nth 0 result)
              :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:19:07Z")
      (expect (nth 2 result) :to-equal nil)
      (expect (test-ovr348-assignment-reads (nth 1 result)) :to-equal nil)))

  (it "leaves a drifted heading alone without a :mismatch-fn, as before"
    (let ((result (test-ovr348-run "2026-09-25T02:19:07Z"
                                   "2026-09-25T02:30:00Z"
                                   "2026-09-25T02:44:10Z"
                                   test-ovr348-row)))
      (expect (car result)
              :to-match ":CANVAS_UPDATED_AT: 2026-09-25T02:19:07Z")))

  (it "answers the item alist from the read, and nil when the read fails"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("assignments/456" . ((id . 456) (updated_at . "2026-01-01T00:00:00Z")))))
        (expect (alist-get 'id (org-canvas--override-read-assignment-item "456"))
                :to-equal 456))
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Boom")))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (expect (org-canvas--override-read-assignment-item "456") :to-be nil)))))

;;;; A Push by Heading Asks Before Deleting an Override (issue #411)

(defconst test-ovr411-existing
  '(((id . 10) (course_section_id . 777) (due_at . "2026-09-28T18:15:00Z"))
    ((id . 11) (student_ids . [279871]) (due_at . "2026-09-29T18:00:00Z")))
  "Two overrides Canvas holds; the table claims only the section's.")

(defun test-ovr411-delete (confirm answer &optional dry-run)
  "Run the delete step with CONFIRM over `test-ovr411-existing'.
ANSWER is what `y-or-n-p' answers, `batch' making the run
noninteractive instead.  DRY-RUN binds `org-canvas--dry-run'.  Return
\(RESULT ASKED CALLS): what the step returned, the prompt it asked or
nil, and the API calls made."
  (let ((asked nil))
    (with-org-canvas-test-config
      (with-mock-api
        (let ((org-canvas--dry-run dry-run)
              (noninteractive (eq answer 'batch)))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt) (setq asked prompt) answer))
                    ((symbol-function 'org-canvas--log-warning) #'ignore))
            (list (org-canvas--override-delete-removed
                   "https://test.canvas.example.com/api/v1/courses/1/assignments/456/overrides"
                   test-ovr411-existing '(10) confirm)
                  asked
                  test-org-canvas-api-calls)))))))

(describe "org-canvas--override-delete-removed asks before deleting (issue #411)"
  (it "deletes without asking when no one asked it to confirm"
    (let ((run (test-ovr411-delete nil 'batch)))
      (expect (nth 0 run) :to-equal '(1 . 0))
      (expect (nth 1 run) :to-be nil)
      (expect (cl-find-if (lambda (c) (eq (car c) 'DELETE)) (nth 2 run))
              :to-be-truthy)))

  (it "deletes on yes, naming who loses what"
    (let ((run (test-ovr411-delete t t)))
      (expect (nth 0 run) :to-equal '(1 . 0))
      (expect (nth 1 run) :to-match "1 override(s)")
      (expect (nth 1 run) :to-match "students 279871 due 2026-09-29T18:00:00Z")
      (expect (cl-find-if (lambda (c) (eq (car c) 'DELETE)) (nth 2 run))
              :to-be-truthy)))

  (it "keeps them on no, and counts them as kept"
    (let ((run (test-ovr411-delete t nil)))
      (expect (nth 0 run) :to-equal '(0 . 1))
      (expect (nth 1 run) :to-be-truthy)
      (expect (cl-find-if (lambda (c) (eq (car c) 'DELETE)) (nth 2 run))
              :to-be nil)))

  (it "keeps them in a batch Emacs, where no one can answer"
    (let ((run (test-ovr411-delete t 'batch)))
      (expect (nth 0 run) :to-equal '(0 . 1))
      (expect (nth 1 run) :to-be nil)
      (expect (cl-find-if (lambda (c) (eq (car c) 'DELETE)) (nth 2 run))
              :to-be nil)))

  (it "asks nothing under a dry run, which sends nothing anyway"
    (let ((run (test-ovr411-delete t nil t)))
      (expect (nth 0 run) :to-equal '(1 . 0))
      (expect (nth 1 run) :to-be nil)
      (expect (nth 2 run) :to-equal nil)))

  (it "asks nothing when every override is claimed"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((noninteractive nil) (asked nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt) (setq asked prompt) nil)))
            (expect (org-canvas--override-delete-removed
                     "https://test.canvas.example.com/api/v1/courses/1/assignments/456/overrides"
                     test-ovr411-existing '(10 11) t)
                    :to-equal '(0 . 0))
            (expect asked :to-be nil))))))

  (it "reports the kept count as the reconcile's fourth number"
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              `(("assignments/456/overrides" . ,(vconcat test-ovr411-existing))))
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
            (expect (org-canvas--override-sync-for-assignment
                     "456" '((:section-id "777" :due-at "2026-09-28T18:15:00Z")) t)
                    :to-equal '(0 0 0 1))))))))

;;; org-canvas-sections-test.el ends here
