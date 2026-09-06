;;; org-canvas-roundtrip-test.el --- Pull idempotence / round-trip guards  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Round-trip idempotence guards: applying the same Canvas response to a
;; heading twice must leave the buffer byte-identical.  This directly guards
;; against the scariest failure mode — a pull silently corrupting an .org
;; file on re-sync (duplicated bodies, churned properties, drifting
;; timestamps).  A non-idempotent pull means each sync mutates the source of
;; truth a little more.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-assignments)
(require 'org-canvas-assignment-groups)
(require 'org-canvas-rubrics)
(require 'org-canvas-pages)

(defun org-canvas-roundtrip--pull-twice (initial pull-fn response)
  "Apply PULL-FN/RESPONSE to INITIAL's first heading twice.
Returns a cons (FIRST . SECOND) of the buffer string after each pull.
Paged list sub-fetches (e.g. assignment overrides) are mocked to nil
so no pull-item reaches the network guard."
  (let ((tmp (make-temp-file "roundtrip-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert initial))
          (with-current-buffer (find-file-noselect tmp)
            (unwind-protect
                (with-html-to-org-identity
                  (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                             (lambda (&rest _) nil)))
                  (let ((org-canvas-course-id "99999")
                        (org-canvas-base-url "https://test.canvas.example.com")
                        (org-canvas-api-token "test-token")
                        first second)
                    (goto-char (point-min))
                    (re-search-forward "^\\* ")
                    (org-back-to-heading)
                    (funcall pull-fn response (point))
                    (setq first (buffer-string))
                    (goto-char (point-min))
                    (re-search-forward "^\\* ")
                    (org-back-to-heading)
                    (funcall pull-fn response (point))
                    (setq second (buffer-string))
                    (cons first second))))
              (kill-buffer))))
      (delete-file tmp))))

(describe "pull idempotence (round-trip guard)"
  (it "assignment pull is idempotent and populates the heading"
    (let* ((initial "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
           (response '((id . 1) (name . "Essay") (points_possible . 50)
                       (due_at . "2026-09-01T10:00:00Z") (published . t)))
           (r (org-canvas-roundtrip--pull-twice
               initial #'org-canvas--assignment-pull-item response)))
      (expect (car r) :to-match "POINTS")        ; pull actually did something
      (expect (cdr r) :to-equal (car r))))       ; second pull changed nothing

  (it "assignment-group pull is idempotent"
    (let* ((initial "* Homework\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
           (response '((id . 1) (name . "Homework") (group_weight . 30)
                       (position . 2)
                       (rules . ((drop_lowest . 1)))))
           (r (org-canvas-roundtrip--pull-twice
               initial #'org-canvas--assignment-group-pull-item response)))
      (expect (car r) :to-match "WEIGHT")
      (expect (cdr r) :to-equal (car r))))

  (it "rubric pull is idempotent"
    (let* ((initial "* Essay Rubric\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
           (response '((id . 1) (title . "Essay Rubric")
                       (data . (((description . "Thesis") (points . 20))
                                ((description . "Clarity") (points . 10))))))
           (r (org-canvas-roundtrip--pull-twice
               initial #'org-canvas--rubric-pull-item response)))
      (expect (cdr r) :to-equal (car r))))

  (it "page pull with body is idempotent (no body duplication on re-pull)"
    ;; Pages fetch detail and insert the body — the classic duplication risk.
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("pages/welcome" . ((page_id . 5)
                                     (body . "<p>Hello class.</p>")))))
        (let* ((initial (concat "* Welcome\n:PROPERTIES:\n"
                                ":CANVAS_URL: welcome\n:END:\n"))
               (response '((url . "welcome") (page_id . 5) (title . "Welcome")))
               (r (org-canvas-roundtrip--pull-twice
                   initial #'org-canvas--page-pull-item response)))
          (expect (car r) :to-match "Hello class\\.")
          (expect (cdr r) :to-equal (car r)))))))

;;;; Registry round trip (issue #137)
;;
;; For every property the registry lists for a feature with a single-item
;; pull, a Canvas item carrying that field is pulled onto a heading and
;; the heading is parsed back the way a push would read it.  The parsed
;; value must be the value pulled.  This is the invariant #133 and #134
;; broke through 99% line coverage: nothing asserted that what pull
;; wrote was what push reads.  Because the properties come from the
;; registry, a property added later is checked without anyone writing a
;; spec for it.  A property whose Canvas field is nested (a `:remote-fn'
;; spec) needs a `:fields' builder in the table below; one whose parse
;; key is not the registry's `:data-key' in either spelling needs a
;; `:parsed' accessor.  Links and `:local-only' bookkeeping are skipped,
;; and each feature must still check at least one property so a registry
;; entry cannot go quiet.

(defun org-canvas-roundtrip--synth (spec)
  "Return a non-default Canvas value for SPEC's type."
  (pcase (plist-get spec :type)
    ('boolean (if (plist-get spec :default) :json-false t))
    ('number 7)
    ('timestamp "2026-06-15T09:00:00Z")
    ('csv-enum (vconcat (or (seq-take (plist-get spec :values) 2)
                            '("alpha" "beta"))))
    ('enum (or (car (plist-get spec :values)) "sample"))
    (_ "sample text")))

(defun org-canvas-roundtrip--expected (spec value)
  "Return VALUE as the parse side should report it for SPEC."
  (pcase (plist-get spec :type)
    ('boolean (not (eq value :json-false)))
    ('csv-enum (sort (mapcar (lambda (x) (format "%s" x)) (append value nil))
                     #'string<))
    (_ value)))

(defun org-canvas-roundtrip--normalize (spec raw)
  "Return RAW, a parsed value, normalized like `org-canvas-roundtrip--expected'."
  (pcase (plist-get spec :type)
    ('boolean (and raw t))
    ('number (and raw (string-to-number (format "%s" raw))))
    ('csv-enum (sort (mapcar (lambda (x) (format "%s" x))
                             (if (listp raw)
                                 raw
                               (split-string (format "%s" raw) "," t "[ \t]+")))
                     #'string<))
    (_ (and raw (format "%s" raw)))))

(defun org-canvas-roundtrip--parsed (spec data)
  "Return DATA's value for SPEC under the registry key or its kebab spelling.
Hand-written parses key their plists in kebab-case where the registry
names the Canvas field, so both are tried."
  (let* ((key (plist-get spec :data-key))
         (kebab (intern (replace-regexp-in-string "_" "-" (symbol-name key)))))
    (if (plist-member data key)
        (plist-get data key)
      (plist-get data kebab))))

(defun org-canvas-roundtrip--checkable-p (spec override)
  "Return non-nil when SPEC can be round-tripped, given its table OVERRIDE."
  (and (not (plist-get spec :local-only))
       (memq (plist-get spec :type)
             '(boolean number timestamp enum csv-enum string))
       (or (null (plist-get spec :remote-fn))
           (plist-get override :fields))))

(defun org-canvas-roundtrip--check (registry-key pull-fn parse-fn template
                                                 base-item &optional overrides)
  "Pull each checkable property of REGISTRY-KEY, parse it back, and compare.
PULL-FN and PARSE-FN are the feature's functions, TEMPLATE the Org text
of a heading to pull onto, BASE-ITEM the fields every synthetic item
needs.  OVERRIDES is an alist of (ORG-PROP . PLIST) with optional
`:value' (the Canvas value to use), `:fields' (a function of the value
returning the item fields that carry it) and `:parsed' (a function of
the parsed plist returning the value).  Returns (CHECKED . MISMATCHES)
where each mismatch is (ORG-PROP EXPECTED PARSED)."
  (let ((entry (gethash registry-key org-canvas--property-registry))
        (mismatches nil)
        (checked 0))
    (dolist (spec (plist-get entry :properties))
      (let ((override (cdr (assoc (plist-get spec :org-prop) overrides))))
        (when (org-canvas-roundtrip--checkable-p spec override)
          (cl-incf checked)
          (let* ((value (if (plist-member override :value)
                            (plist-get override :value)
                          (org-canvas-roundtrip--synth spec)))
                 (fields (if (plist-get override :fields)
                             (funcall (plist-get override :fields) value)
                           (list (cons (org-canvas--registry-remote-key spec)
                                       value))))
                 (item (append fields base-item))
                 (expected (org-canvas-roundtrip--expected spec value))
                 (parsed nil))
            (with-temp-org-buffer template
              (goto-char (point-min))
              (re-search-forward "^:CANVAS_\\(ID\\|URL\\): ")
              (org-back-to-heading t)
              (with-html-to-org-identity
                (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                           (lambda (&rest _) nil)))
                  ;; A pull that emits children (rubric criteria) leaves
                  ;; point inside them; the heading itself does not move.
                  (let ((heading (point)))
                    (funcall pull-fn item heading)
                    (goto-char heading))
                  (let ((data (funcall parse-fn)))
                    (setq parsed (org-canvas-roundtrip--normalize
                                  spec
                                  (if (plist-get override :parsed)
                                      (funcall (plist-get override :parsed) data)
                                    (org-canvas-roundtrip--parsed spec data))))))))
            (unless (equal parsed expected)
              (push (list (plist-get spec :org-prop) expected parsed)
                    mismatches))))))
    (cons checked (nreverse mismatches))))

(defun org-canvas-roundtrip--inverse-locked (value)
  "Return the `locked' field that means the boolean VALUE."
  `((locked . ,(if (eq value :json-false) t :json-false))))

(defun org-canvas-roundtrip--features (local-file)
  "Return the round-trip table; LOCAL-FILE is an existing file for files.org."
  `(("announcements" org-canvas--announcement-pull-item
     org-canvas--announcement-parse-entry
     "* Hi\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (title . "Hi") (message . "<p>x</p>"))
     (("AUTHOR" :fields ,(lambda (v) `((user . ((display_name . ,v))))))
      ("ALLOW_COMMENTS" :fields org-canvas-roundtrip--inverse-locked)))
    ("discussions" org-canvas--discussion-pull-item
     org-canvas--discussion-parse-entry
     "* D\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (title . "D") (message . "<p>x</p>")))
    ("assignments" org-canvas--assignment-pull-item
     org-canvas--assignment-parse-entry
     "* A\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (name . "A") (description . ""))
     (("RUBRIC_USE_FOR_GRADING"
       :parsed ,(lambda (d) (plist-get d :rubric-use-for-grading)))
      ("RUBRIC_HIDE_SCORE_TOTAL"
       :fields ,(lambda (v) `((rubric_settings . ((hide_score_total . ,v))))))
      ("EXTERNAL_TOOL_URL"
       :fields ,(lambda (v) `((external_tool_tag_attributes . ((url . ,v))))))
      ("EXTERNAL_TOOL_ID"
       :fields ,(lambda (v) `((external_tool_tag_attributes . ((content_id . ,v))))))
      ("EXTERNAL_TOOL_NEW_TAB"
       :fields ,(lambda (v) `((external_tool_tag_attributes . ((new_tab . ,v))))))))
    ("assignment-groups" org-canvas--assignment-group-pull-item
     org-canvas--assignment-group-parse-entry
     "* Groups\n** HW\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (name . "HW") (group_weight . 40))
     (("DROP_LOWEST" :fields ,(lambda (v) `((rules . ((drop_lowest . ,v))))))
      ("DROP_HIGHEST" :fields ,(lambda (v) `((rules . ((drop_highest . ,v))))))
      ("NEVER_DROP" :value [11 12]
       :fields ,(lambda (v) `((rules . ((never_drop . ,v)))))
       :parsed ,(lambda (d) (alist-get 'never_drop (plist-get d :rules))))))
    ("pages" org-canvas--page-pull-item org-canvas--page-parse-entry
     "* P\n:PROPERTIES:\n:CANVAS_URL: p\n:END:\n"
     ((url . "p") (page_id . 5) (title . "P")))
    ("modules" org-canvas--module-pull-item org-canvas--module-parse-entry
     "* M\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (name . "M") (items . [])))
    ("rubrics" org-canvas--rubric-pull-item org-canvas--rubric-parse-entry
     "* R\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (title . "R")
      (data . [((id . "c1") (description . "Thesis") (points . 5)
                (ratings . [((description . "Full") (points . 5))]))]))
     (("FREE_FORM_CRITERION_COMMENTS"
       :fields ,(lambda (v) `((free_form_criterion_comments . ,v))))))
    ("files" org-canvas--file-pull-item org-canvas--file-parse-entry
     ,(format "* [[file:%s][%s]]\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
              local-file (file-name-nondirectory local-file))
     ((id . 1) (display_name . ,(file-name-nondirectory local-file)))
     (("PUBLISHED" :fields org-canvas-roundtrip--inverse-locked)
      ("USE_JUSTIFICATION"
       :fields ,(lambda (v) `((usage_rights . ((use_justification . ,v))))))
      ("USAGE_LICENSE"
       :fields ,(lambda (v) `((usage_rights . ((license . ,v))))))
      ("COPYRIGHT"
       :fields ,(lambda (v) `((usage_rights . ((legal_copyright . ,v))))))))
    ("group-categories" org-canvas--group-category-pull-item
     org-canvas--group-category-parse-entry
     "* G\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (name . "G")))
    ("calendar-events" org-canvas--calendar-event-pull-item
     org-canvas--calendar-event-parse-entry
     "* E\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
     ((id . 1) (title . "E") (start_at . "2026-06-15T09:00:00Z")))))

(describe "registry round trip: pull then parse returns the pulled value (issue #137)"
  (let ((local-file (make-temp-file "org-canvas-rt-" nil ".pdf")))
    (dolist (row (org-canvas-roundtrip--features local-file))
      (let ((row row))
        (it (car row)
          (with-org-canvas-test-config
            (with-mock-api
              (setq test-org-canvas-api-responses
                    '(("pages/p" . ((page_id . 5) (body . "<p>x</p>")))))
              (let ((result (apply #'org-canvas-roundtrip--check row)))
                (expect (car result) :to-be-greater-than 0)
                (expect (cdr result) :to-equal nil)))))))))

(provide 'org-canvas-roundtrip-test)
;;; org-canvas-roundtrip-test.el ends here
