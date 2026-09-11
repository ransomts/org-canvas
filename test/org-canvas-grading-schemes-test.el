;;; org-canvas-grading-schemes-test.el --- Buttercup tests for grading schemes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Specs for `org-canvas-grading-schemes': the scheme table, the push
;; that creates but never edits in place (Canvas has no update), the
;; pull that writes the table back, and the two validators.  Every
;; request is answered by a mock; nothing here reaches the network
;; (Hard Rule 2), and the log is captured into a list, never read from
;; the shared buffer (Hard Rule 3).

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-validate)
(require 'org-canvas)

(defconst test-gs--letters
  '((id . 4321) (title . "Letter Grades")
    (context_type . "Course") (context_id . 99999)
    (points_based . :json-false) (scaling_factor . 1.0)
    (grading_scheme . [((name . "A") (value . 0.94))
                       ((name . "B") (value . 0.84))
                       ((name . "C") (value . 0.74))
                       ((name . "F") (value . 0.0))]))
  "A course scheme, as Canvas answers it.")

(defconst test-gs--gpa
  '((id . 77) (title . "GPA Scale")
    (context_type . "Account") (context_id . 1)
    (points_based . t) (scaling_factor . 4.0)
    (grading_scheme . [((name . "A") (value . 0.9))
                       ((name . "B") (value . 0.725))
                       ((name . "F") (value . 0.0))]))
  "A points-based account scheme.")

(defconst test-gs--letters-org
  "* Letter Grades
:PROPERTIES:
:CANVAS_ID: 4321
:END:

#+NAME: scheme
| Grade | Cutoff |
|-------+--------|
| A     |     94 |
| B     |     84 |
| C     |     74 |
| F     |      0 |
"
  "The same scheme as `test-gs--letters', in Org.")

(defmacro test-gs--with-file (content &rest body)
  "Run BODY with `org-canvas-grading-schemes-file' bound to a temp file of CONTENT.
FILE is bound to the path; the visiting buffer is killed afterwards."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "gs-" t))
          (file (expand-file-name "grading-schemes.org" dir))
          (org-canvas-grading-schemes-file file))
     (when ,content
       (with-temp-file file (insert ,content)))
     (unwind-protect
         (with-org-canvas-test-config
           (with-sync-test-env
             (cl-letf (((symbol-function 'message) #'ignore))
               ,@body)))
       (dolist (buf (buffer-list))
         (let ((bf (buffer-file-name buf)))
           (when (and bf (string-prefix-p dir bf))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory dir t))))

(defun test-gs--file-text (file)
  "Return FILE's text."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun test-gs--first-entry (file)
  "Return the parsed first entry of FILE, with point left on its heading."
  (with-current-buffer (org-canvas--find-file-noselect file)
    (goto-char (point-min))
    (unless (org-at-heading-p) (outline-next-heading))
    (org-canvas--grading-scheme-parse-entry)))

(defmacro test-gs--with-entry (file &rest body)
  "Run BODY in FILE's buffer with DATA and PAYLOAD bound to its first entry.
A push runs in the file's own buffer, as the pipeline runs it."
  (declare (indent 1))
  `(with-current-buffer (org-canvas--find-file-noselect ,file)
     (let* ((data (test-gs--first-entry ,file))
            (payload (org-canvas--grading-scheme-build-payload data)))
       (ignore data payload)
       ,@body)))

(defmacro test-gs--capturing-log (var &rest body)
  "Run BODY with every log line pushed onto VAR as a formatted string."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-canvas--log-info)
              (lambda (_l fmt &rest args) (push (apply #'format fmt args) ,var)))
             ((symbol-function 'org-canvas--log-warning)
              (lambda (_l fmt &rest args) (push (apply #'format fmt args) ,var))))
     ,@body))

;;;; The Table

(describe "org-canvas--grading-scheme-parse-table"
  (it "reads grade rows below a header, trims cells, and allows a percent sign"
    (expect (org-canvas--grading-scheme-parse-table
             '(("Grade" "Cutoff") hline (" A " " 94 ") ("B" "84%") ("F" "0"))
             "x")
            :to-equal '(("A" . 94) ("B" . 84) ("F" . 0))))

  (it "takes a table without a header and a fractional cutoff"
    (expect (org-canvas--grading-scheme-parse-table '(("A" "89.5") ("F" "0")) "x")
            :to-equal '(("A" . 89.5) ("F" . 0))))

  (it "signals for a missing table, an empty one, and a cutoff that is not a number"
    (expect (org-canvas--grading-scheme-parse-table nil "Letters")
            :to-throw 'org-canvas-validation-error)
    (expect (org-canvas--grading-scheme-parse-table '(("Grade" "Cutoff") hline) "Letters")
            :to-throw 'org-canvas-validation-error)
    (expect (org-canvas--grading-scheme-parse-table
             '(("Grade" "Cutoff") ("A" "ninety") ("F" "0")) "Letters")
            :to-throw 'org-canvas-validation-error)))

(describe "org-canvas--grading-scheme-format-cutoff"
  (it "writes a whole percent without a decimal point and keeps a fraction"
    (expect (org-canvas--grading-scheme-format-cutoff 0.94) :to-equal "94")
    (expect (org-canvas--grading-scheme-format-cutoff 0.0) :to-equal "0")
    (expect (org-canvas--grading-scheme-format-cutoff 0.895) :to-equal "89.5")
    (expect (org-canvas--grading-scheme-format-cutoff 0.7250) :to-equal "72.5")))

;;;; Parse and Payload

(describe "org-canvas--grading-scheme-parse-entry"
  (it "reads the properties and the table into one plist"
    (test-gs--with-file "* GPA Scale
:PROPERTIES:
:CANVAS_ID: 77
:CONTEXT: account
:POINTS_BASED: true
:SCALING_FACTOR: 4.0
:END:

#+NAME: scheme
| Grade | Cutoff |
|-------+--------|
| A     |     90 |
| F     |      0 |
"
      (let ((data (test-gs--first-entry file)))
        (expect (plist-get data :title) :to-equal "GPA Scale")
        (expect (plist-get data :canvas-id) :to-equal "77")
        (expect (plist-get data :context) :to-equal "account")
        (expect (plist-get data :points_based) :to-be t)
        (expect (plist-get data :scaling_factor) :to-equal 4.0)
        (expect (plist-get data :scheme) :to-equal '(("A" . 90) ("F" . 0)))
        (expect (plist-get data :pom) :to-be-truthy))))

  (it "signals when the heading has no scheme table"
    (test-gs--with-file "* Bare\n"
      (expect (test-gs--first-entry file) :to-throw 'org-canvas-validation-error))))

(describe "org-canvas--grading-scheme-build-payload"
  (it "sends the title, the entries as percents and the points flag"
    (let ((payload (org-canvas--grading-scheme-build-payload
                    (list :title "Letters" :points_based nil :scaling_factor nil
                          :scheme '(("A" . 94) ("F" . 0))))))
      (expect (alist-get 'title payload) :to-equal "Letters")
      (expect (alist-get 'points_based payload) :to-equal :json-false)
      (expect (assq 'scaling_factor payload) :to-be nil)
      (expect (append (alist-get 'grading_scheme_entry payload) nil)
              :to-equal '(((name . "A") (value . 94)) ((name . "F") (value . 0))))))

  (it "sends the scaling factor only for a points-based scheme"
    (let ((points (org-canvas--grading-scheme-build-payload
                   (list :title "GPA" :points_based t :scaling_factor 4.0
                         :scheme '(("A" . 90) ("F" . 0)))))
          (percent (org-canvas--grading-scheme-build-payload
                    (list :title "Letters" :points_based nil :scaling_factor 4.0
                          :scheme '(("A" . 90) ("F" . 0))))))
      (expect (alist-get 'points_based points) :to-be t)
      (expect (alist-get 'scaling_factor points) :to-equal 4.0)
      (expect (assq 'scaling_factor percent) :to-be nil))))

(describe "org-canvas--grading-scheme-same-p"
  (it "matches on title, cutoffs and the points flag, and not otherwise"
    (let ((same (list :title "Letter Grades" :points_based nil
                      :scheme '(("A" . 94) ("B" . 84) ("C" . 74) ("F" . 0))))
          (cutoff (list :title "Letter Grades" :points_based nil
                        :scheme '(("A" . 93) ("B" . 84) ("C" . 74) ("F" . 0))))
          (fewer (list :title "Letter Grades" :points_based nil
                       :scheme '(("A" . 94) ("F" . 0))))
          (points (list :title "Letter Grades" :points_based t :scaling_factor 1.0
                        :scheme '(("A" . 94) ("B" . 84) ("C" . 74) ("F" . 0)))))
      (expect (org-canvas--grading-scheme-same-p same test-gs--letters) :to-be-truthy)
      (expect (org-canvas--grading-scheme-same-p cutoff test-gs--letters) :to-be nil)
      (expect (org-canvas--grading-scheme-same-p fewer test-gs--letters) :to-be nil)
      (expect (org-canvas--grading-scheme-same-p points test-gs--letters) :to-be nil)))

  (it "compares the scaling factor of a points-based scheme"
    (let ((four (list :title "GPA Scale" :points_based t :scaling_factor 4.0
                      :scheme '(("A" . 90) ("B" . 72.5) ("F" . 0))))
          (five (list :title "GPA Scale" :points_based t :scaling_factor 5
                      :scheme '(("A" . 90) ("B" . 72.5) ("F" . 0)))))
      (expect (org-canvas--grading-scheme-same-p four test-gs--gpa) :to-be-truthy)
      (expect (org-canvas--grading-scheme-same-p five test-gs--gpa) :to-be nil))))

;;;; Push

(describe "org-canvas--grading-scheme-push"
  (it "never pushes an account scheme"
    (with-mock-api
      (expect (org-canvas--grading-scheme-push
               (list :title "GPA Scale" :context "account" :scheme '(("A" . 90)))
               '((title . "GPA Scale")))
              :to-be 'skip)
      (expect (test-org-canvas-api-call-count) :to-equal 0)))

  (it "skips a stamped scheme that matches Canvas and records the hash"
    (test-gs--with-file test-gs--letters-org
      (with-mock-api
        (push (cons "grading_standards/4321" test-gs--letters) test-org-canvas-api-responses)
        (test-gs--with-entry file
          (let ((ctx (org-canvas--sync-make-ctx :counters (list :skip 0))))
          (expect (org-canvas--grading-scheme-push data payload ctx) :to-be 'skip)
          (expect (test-org-canvas-api-called-p 'GET "grading_standards/4321") :to-be-truthy)
          (expect (test-org-canvas-api-called-p 'POST "grading_standards") :to-be nil)
          (expect (org-entry-get (plist-get data :pom) org-canvas--prop-payload-hash)
                  :to-be-truthy))))))

  (it "skips a stamped scheme that differs, with a warning that says what to do"
    (test-gs--with-file (replace-regexp-in-string "94" "93" test-gs--letters-org)
      (with-mock-api
        (push (cons "grading_standards/4321" test-gs--letters) test-org-canvas-api-responses)
        (test-gs--with-entry file
          (let ((ctx (org-canvas--sync-make-ctx :counters (list :skip 0)))
                (log nil))
          (test-gs--capturing-log log
            (expect (org-canvas--grading-scheme-push data payload ctx) :to-be 'skip))
          (expect (cl-some (lambda (l) (string-match-p "cannot edit a grading scheme in place" l)) log)
                  :to-be-truthy)
          (expect (car (plist-get (plist-get ctx :counters) :skipped-titles))
                  :to-match "Letter Grades")
          (expect (org-entry-get (plist-get data :pom) org-canvas--prop-payload-hash)
                  :to-be nil)
          (expect (test-org-canvas-api-called-p 'POST "grading_standards") :to-be nil))))))

  (it "reads a stamped scheme from the run's snapshot instead of asking Canvas"
    (test-gs--with-file test-gs--letters-org
      (with-mock-api
        (test-gs--with-entry file
          (let* ((titles (make-hash-table :test 'equal))
                 (ctx (org-canvas--sync-make-ctx :counters (list :skip 0)
                                                 :remote-titles titles)))
            (puthash "Letter Grades" (list test-gs--letters) titles)
            (expect (org-canvas--grading-scheme-push data payload ctx) :to-be 'skip)
            (expect (test-org-canvas-api-call-count) :to-equal 0))))))

  (it "creates a stamped scheme afresh when Canvas no longer has its id"
    (test-gs--with-file test-gs--letters-org
      (let ((posted nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method url &rest args)
                     (pcase method
                       ('GET (signal 'org-canvas-api-error (list "404 Not Found")))
                       ('POST (setq posted (plist-get args :data)) '((id . 5000)))
                       (_ (error "Unexpected %s %s" method url))))))
          (let* ((data (test-gs--first-entry file))
                 (payload (org-canvas--grading-scheme-build-payload data)))
            (expect (alist-get 'id (org-canvas--grading-scheme-push data payload)) :to-equal 5000)
            (expect (alist-get 'title posted) :to-equal "Letter Grades"))))))

  (it "re-signals a GET failure that is not a 404"
    (test-gs--with-file test-gs--letters-org
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (signal 'org-canvas-api-error (list "500 Server Error")))))
        (let* ((data (test-gs--first-entry file))
               (payload (org-canvas--grading-scheme-build-payload data)))
          (expect (org-canvas--grading-scheme-push data payload)
                  :to-throw 'org-canvas-api-error)))))

  (it "creates a new scheme with a POST when Canvas holds no twin"
    (test-gs--with-file (replace-regexp-in-string ":CANVAS_ID: 4321\n" "" test-gs--letters-org)
      (with-mock-api
        (push (cons "grading_standards" []) test-org-canvas-api-responses)
        (let* ((data (test-gs--first-entry file))
               (payload (org-canvas--grading-scheme-build-payload data)))
          (org-canvas--grading-scheme-push data payload)
          (let ((body (test-org-canvas-api-call-data 'POST "grading_standards$")))
            (expect (alist-get 'title body) :to-equal "Letter Grades")
            (expect (length (alist-get 'grading_scheme_entry body)) :to-equal 4))))))

  (it "adopts a twin with the same cutoffs instead of creating a second"
    (test-gs--with-file (replace-regexp-in-string ":CANVAS_ID: 4321\n" "" test-gs--letters-org)
      (with-mock-api
        (let* ((data (test-gs--first-entry file))
               (payload (org-canvas--grading-scheme-build-payload data))
               (titles (make-hash-table :test 'equal))
               (ctx (org-canvas--sync-make-ctx :remote-titles titles)))
          (puthash "Letter Grades" (list test-gs--letters) titles)
          (expect (alist-get 'id (org-canvas--grading-scheme-push data payload ctx))
                  :to-equal 4321)
          (expect (test-org-canvas-api-called-p 'POST "grading_standards") :to-be nil)))))

  (it "is a duplicate when the twin has other cutoffs, unless the strategy is create"
    (test-gs--with-file (replace-regexp-in-string ":CANVAS_ID: 4321\n" "" test-gs--letters-org)
      (with-mock-api
        (let* ((data (test-gs--first-entry file))
               (payload (org-canvas--grading-scheme-build-payload data))
               (other (append '((id . 8) (title . "Letter Grades")
                                (grading_scheme . [((name . "A") (value . 0.9))
                                                   ((name . "F") (value . 0.0))]))
                              nil))
               (titles (make-hash-table :test 'equal))
               (ctx (org-canvas--sync-make-ctx :remote-titles titles)))
          (puthash "Letter Grades" (list other) titles)
          (expect (org-canvas--grading-scheme-push data payload ctx) :to-be 'duplicate)
          (expect (test-org-canvas-api-called-p 'POST "grading_standards") :to-be nil)
          (let ((org-canvas-duplicate-title-strategy 'create))
            (org-canvas--grading-scheme-push data payload ctx))
          (expect (test-org-canvas-api-called-p 'POST "grading_standards") :to-be-truthy)))))

  (it "asks the search function for a twin outside a run"
    (test-gs--with-file (replace-regexp-in-string ":CANVAS_ID: 4321\n" "" test-gs--letters-org)
      (with-mock-api
        (push (cons "grading_standards" (vector test-gs--letters)) test-org-canvas-api-responses)
        (let* ((data (test-gs--first-entry file))
               (payload (org-canvas--grading-scheme-build-payload data)))
          (expect (alist-get 'id (org-canvas--grading-scheme-push data payload)) :to-equal 4321)
          (expect (test-org-canvas-api-called-p 'GET "grading_standards") :to-be-truthy)
          (expect (test-org-canvas-api-called-p 'POST "grading_standards") :to-be nil)))))

  (it "previews a create under a dry run without a request or a stamp"
    (test-gs--with-file (replace-regexp-in-string ":CANVAS_ID: 4321\n" "" test-gs--letters-org)
      (with-mock-api
        (let* ((data (test-gs--first-entry file))
               (payload (org-canvas--grading-scheme-build-payload data))
               (titles (make-hash-table :test 'equal))
               (ctx (org-canvas--sync-make-ctx :remote-titles titles))
               (org-canvas--dry-run t))
          (expect (org-canvas--dry-run-response-p
                   (org-canvas--grading-scheme-push data payload ctx))
                  :to-be-truthy)
          (puthash "Letter Grades" (list test-gs--letters) titles)
          (expect (org-canvas--dry-run-response-p
                   (org-canvas--grading-scheme-push data payload ctx))
                  :to-be-truthy)
          (expect (test-org-canvas-api-call-count) :to-equal 0)
          (expect (test-gs--file-text file) :not :to-match "PAYLOAD_HASH"))))))

;;;; The Sync Command

(describe "org-canvas-sync-grading-schemes"
  (it "creates the scheme, stamps its id and hash, and skips it on the next run"
    (test-gs--with-file (replace-regexp-in-string ":CANVAS_ID: 4321\n" "" test-gs--letters-org)
      (let ((posts 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (pcase method
                       ('POST (cl-incf posts) '((id . 5000)))
                       (_ [])))))
          (org-canvas-sync-grading-schemes)
          (expect posts :to-equal 1)
          ;; No #+LAST_SYNCED header: a grading standard carries no
          ;; updated_at, and the header is stamped from Canvas's clock.
          (let ((text (test-gs--file-text file)))
            (expect text :to-match ":CANVAS_ID: +5000")
            (expect text :to-match ":PAYLOAD_HASH:"))
          (org-canvas-sync-grading-schemes)
          (expect posts :to-equal 1)))))

  (it "leaves a differing stamped scheme alone and names it in the summary"
    (test-gs--with-file (replace-regexp-in-string "94" "93" test-gs--letters-org)
      (let ((writes 0) (log nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (if (eq method 'GET) (vector test-gs--letters)
                       (cl-incf writes) '((id . 1))))))
          (test-gs--capturing-log log
            (org-canvas-sync-grading-schemes))
          (expect writes :to-equal 0)
          (expect (cl-some (lambda (l) (string-match-p "Success: 0 | Skipped: 1" l)) log)
                  :to-be-truthy)))))

  (it "has a push at point and a delete at point"
    (expect (fboundp 'org-canvas-sync-grading-scheme-at-point) :to-be t)
    (test-gs--with-file test-gs--letters-org
      (with-mock-api
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (with-current-buffer (org-canvas--find-file-noselect file)
            (goto-char (point-min))
            (unless (org-at-heading-p) (outline-next-heading))
            (org-canvas-delete-grading-scheme-at-point)))
        (expect (test-org-canvas-api-called-p 'DELETE "grading_standards/4321") :to-be-truthy)))))

;;;; Pull

(describe "org-canvas-pull-grading-schemes"
  (it "writes a heading, the properties and the table for each scheme"
    (test-gs--with-file nil
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (vector test-gs--letters test-gs--gpa))))
        (org-canvas-pull-grading-schemes))
      (let ((text (test-gs--file-text file)))
        (expect text :to-match "^\\* Letter Grades\n")
        (expect text :to-match ":CANVAS_ID: +4321\n")
        (expect text :to-match ":CONTEXT: +course\n")
        (expect text :to-match "^#\\+NAME: scheme\n| Grade +| Cutoff +|\n|-+\\+-+|\n| A +| +94 +|\n| B +| +84 +|\n| C +| +74 +|\n| F +| +0 +|\n")
        (expect text :to-match "^\\* GPA Scale\n")
        (expect text :to-match ":CONTEXT: +account\n")
        (expect text :to-match ":POINTS_BASED: +true\n")
        (expect text :to-match ":SCALING_FACTOR: +4.0\n")
        (expect text :to-match "| B +| +72.5 +|\n")
        (expect text :to-match "^#\\+LAST_SYNCED:"))
      ;; A percentage scheme carries no SCALING_FACTOR and no POINTS_BASED.
      (let ((letters (car (split-string (test-gs--file-text file) "^\\* GPA Scale"))))
        (expect letters :not :to-match "SCALING_FACTOR")
        (expect letters :not :to-match "POINTS_BASED"))))

  (it "rewrites the table on a re-pull without duplicating anything"
    (test-gs--with-file test-gs--letters-org
      (cl-letf (((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (vector (append '((grading_scheme . [((name . "A") (value . 0.9))
                                                                         ((name . "F") (value . 0.0))]))
                                                   (assq-delete-all 'grading_scheme (copy-alist test-gs--letters)))))))
        (org-canvas-pull-grading-schemes)
        (org-canvas-pull-grading-schemes))
      (let ((text (test-gs--file-text file)))
        (expect (length (split-string text "^\\* Letter Grades" t)) :to-equal 2)
        (expect (length (split-string text "^#\\+NAME: scheme" t)) :to-equal 2)
        (expect text :to-match "| A +| +90 +|\n| F +| +0 +|\n")
        (expect text :not :to-match "| B "))))

  (it "writes the empty-file note when the course has no scheme"
    (test-gs--with-file nil
      (cl-letf (((symbol-function 'org-canvas-api-request) (lambda (&rest _) [])))
        (org-canvas-pull-grading-schemes))
      (expect (test-gs--file-text file) :to-match "Canvas returned 0 items"))))

;;;; Validation

(defun test-gs--structure-issues (content)
  "Return the structural issues for the first heading of an Org buffer of CONTENT."
  (with-temp-org-buffer content
    (goto-char (point-min))
    (unless (org-at-heading-p) (outline-next-heading))
    (mapcar (lambda (i) (plist-get i :message))
            (org-canvas--validate-grading-scheme-structure
             (list :file "x.org" :line 1 :heading "x")))))

(describe "org-canvas--validate-grading-scheme-structure"
  (it "is quiet for a descending table ending in 0"
    (expect (test-gs--structure-issues test-gs--letters-org) :to-equal nil))

  (it "reports a missing table and an empty one"
    (expect (car (test-gs--structure-issues "* Bare\n")) :to-match "no #\\+NAME: scheme table")
    (expect (car (test-gs--structure-issues "* Empty\n#+NAME: scheme\n| Grade | Cutoff |\n"))
            :to-match "no grade rows"))

  (it "reports a cutoff that is not a number or is above 100"
    (let ((issues (test-gs--structure-issues
                   "* Odd\n#+NAME: scheme\n| Grade | Cutoff |\n| A | ninety |\n| B | 120 |\n| F | 0 |\n")))
      (expect (car issues) :to-match "'A' is not a number")
      (expect (cadr issues) :to-match "'B' is above 100")))

  (it "reports rows that do not descend, and a last row that is not 0"
    (expect (car (test-gs--structure-issues
                  "* Up\n#+NAME: scheme\n| Grade | Cutoff |\n| A | 80 |\n| B | 90 |\n| F | 0 |\n"))
            :to-match "must descend: 'A' (80) is not above 'B' (90)")
    (expect (car (test-gs--structure-issues
                  "* Gap\n#+NAME: scheme\n| Grade | Cutoff |\n| A | 90 |\n| B | 60 |\n"))
            :to-match "'B', should have a cutoff of 0"))

  (it "runs from the registry's structural function"
    (test-gs--with-file "* Up\n#+NAME: scheme\n| Grade | Cutoff |\n| A | 80 |\n| B | 90 |\n| F | 0 |\n"
      (with-nonexistent-canvas-files
        (let* ((org-canvas-grading-schemes-file file)
               (issues (plist-get (org-canvas--validate-run-all-specs) :issues)))
          (expect (cl-some (lambda (i) (string-match-p "must descend" (plist-get i :message)))
                           issues)
                  :to-be-truthy))))))

(describe "org-canvas--validate-grading-standard-ids"
  (it "warns where settings.org or assignments.org name a scheme the file lacks"
    (test-gs--with-file test-gs--letters-org
      (let ((org-canvas-settings-file (expand-file-name "settings.org" dir))
            (org-canvas-assignments-file (expand-file-name "assignments.org" dir)))
        (with-temp-file org-canvas-settings-file
          (insert "* Course\n:PROPERTIES:\n:GRADING_STANDARD_ID: 4321\n:END:\n"))
        (with-temp-file org-canvas-assignments-file
          (insert "* Essay\n:PROPERTIES:\n:GRADING_STANDARD_ID: 999\n:END:\n* Quiz\n:PROPERTIES:\n:GRADING_STANDARD_ID: 0\n:END:\n"))
        (let ((issues (org-canvas--validate-grading-standard-ids file)))
          (expect (length issues) :to-equal 1)
          (expect (plist-get (car issues) :severity) :to-be 'warning)
          (expect (plist-get (car issues) :heading) :to-equal "Essay")
          (expect (plist-get (car issues) :message)
                  :to-match "GRADING_STANDARD_ID 999 matches no CANVAS_ID in grading-schemes.org")))))

  (it "is quiet when the other files are absent"
    (test-gs--with-file test-gs--letters-org
      (let ((org-canvas-settings-file (expand-file-name "none.org" dir))
            (org-canvas-assignments-file (expand-file-name "none2.org" dir)))
        (expect (org-canvas--validate-grading-standard-ids file) :to-equal nil)))))

;;;; Wiring

(describe "grading schemes wiring"
  (it "registers the feature, the file variable and the properties"
    (expect (org-canvas--registry-find-feature "grading-schemes") :not :to-be nil)
    (expect (org-canvas--registry-find-property "SCALING_FACTOR") :not :to-be nil)
    (expect (assq 'org-canvas-grading-schemes-file org-canvas--file-var-registry)
            :not :to-be nil))

  (it "syncs before settings and pulls in the first content tier"
    (let ((first-tier (car org-canvas--sync-tiers)))
      (expect (cl-position 'org-canvas-sync-grading-schemes (mapcar #'car first-tier))
              :to-be-less-than
              (cl-position 'org-canvas-sync-settings (mapcar #'car first-tier))))
    (expect (assq 'org-canvas-pull-grading-schemes (nth 1 org-canvas--pull-tiers))
            :to-be-truthy)))

(provide 'org-canvas-grading-schemes-test)
;;; org-canvas-grading-schemes-test.el ends here
