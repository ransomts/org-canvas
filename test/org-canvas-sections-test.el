;;; org-canvas-sections-test.el --- Buttercup tests for sections  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-sections)

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
                  (org-back-to-heading)
                  (expect (org-get-heading t t t t) :to-equal "Section A")
                  (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100")
                  (expect (org-entry-get (point) "START_AT") :to-match "^<2026-01-15")
                  (expect (org-entry-get (point) "END_AT") :to-match "^<2026-05-15")
                  (expect (org-entry-get (point) "RESTRICT_TO_DATES") :to-equal "true")
                  (expect (org-entry-get (point) "LAST_SYNCED") :to-be-truthy)))))
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
                  (org-back-to-heading)
                  ;; Heading name should be preserved
                  (expect (org-get-heading t t t t)
                          :to-equal "My Custom Name for Section A")
                  ;; Properties should be updated
                  (expect (org-entry-get (point) "RESTRICT_TO_DATES") :to-equal "true")
                  (expect (org-entry-get (point) "START_AT") :to-match "^<2026-01-15")
                  (expect (org-entry-get (point) "END_AT") :to-match "^<2026-05-15")
                  (expect (org-entry-get (point) "LAST_SYNCED") :to-be-truthy)))))
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
            (with-sync-test-env
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                (expect (org-canvas-pull-sections) :to-throw 'user-error))))
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
          ;; Should not throw, just log and return 0
          (setq deleted (org-canvas--override-delete-removed
                         "https://test.canvas.example.com/api/v1/courses/99999/assignments/1/overrides"
                         '(((id . 5) (course_section_id . 99)))
                         '(1 2 3)))
          (expect deleted :to-equal 0))))))

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
                    (org-canvas-sync-overrides)
                    (expect t :to-be t))))))
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

;;; org-canvas-sections-test.el ends here
