;;; org-canvas-core-sync-test.el --- Buttercup tests for org-canvas-core's sync pipeline core  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The engine core: the define-sync DSL and the spec it builds, the
;; per-entry pipeline (skip, dry run, deleted-heading guards), the run
;; context, the counters and the global summary.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)
(require 'org-canvas-pages)
(require 'org-canvas-announcements)
(require 'org-canvas-discussions)
(require 'org-canvas-assignments)
(require 'org-canvas-rubrics)
(require 'org-canvas-files)

(describe "with-mock-api request recording"
  (it "records :params and :timeout, not just :data"
    (with-mock-api
      (org-canvas-api-request 'GET "https://x/api/v1/courses/1/items"
                              :data '((a . 1))
                              :params '(("per_page" . "100"))
                              :timeout 42)
      (let ((call (test-org-canvas-find-api-call 'GET "items")))
        (expect (nth 2 call) :to-equal '((a . 1)))
        (expect (test-org-canvas-call-arg call :params)
                :to-equal '(("per_page" . "100")))
        (expect (test-org-canvas-call-arg call :timeout) :to-equal 42))))

  (it "exposes payload via test-org-canvas-api-call-data"
    (with-mock-api
      (org-canvas-api-request 'POST "https://x/api/v1/courses/1/pages"
                              :data '((title . "T")))
      (expect (test-org-canvas-api-call-data 'POST "pages")
              :to-equal '((title . "T")))
      (expect (test-org-canvas-api-call-data 'POST "no-such") :to-be nil))))

(describe "org-canvas-define-sync generated function"
  (it "syncs entries from file successfully"
    ;; Test using org-canvas-sync-pages which is a real macro-generated function
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (post-count 0))
            (with-temp-file org-file
              (insert "* Page One
:PROPERTIES:
:END:

Content one.

* Page Two
:PROPERTIES:
:END:

Content two.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq post-count (1+ post-count)))
                             '((url . "test-url")))))
                  (org-canvas-sync-pages)
                  (expect post-count :to-equal 2)
                  ;; Verify CANVAS_URL was saved
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_URL") :to-equal "test-url"))))))
        (delete-directory temp-dir t))))

  (it "reports the number of successfully synced items"
    ;; Guards the `:success' counter increment: a broken increment would
    ;; mis-report the user-facing \"N success\" summary.
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "pages.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Page One\n:PROPERTIES:\n:END:\n\nc1.\n\n"
                      "* Page Two\n:PROPERTIES:\n:END:\n\nc2.\n"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_m _u &rest _a) '((url . "u")))))
                  (spy-on 'message :and-call-through)
                  (org-canvas-sync-pages)
                  (let ((reported nil))
                    (dolist (call (spy-calls-all-args 'message))
                      (let ((s (and (car call) (stringp (car call))
                                    (ignore-errors (apply #'format call)))))
                        ;; Leading space so a mutated "-2 success" can't match.
                        (when (and s (string-match-p " 2 success" s))
                          (setq reported t))))
                    (expect reported :to-be t))))))
        (delete-directory temp-dir t))))

  (it "continues after one entry fails"
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "pages.org" temp-dir))
                 (call-count 0))
            (with-temp-file org-file
              (insert "* Page One
:PROPERTIES:
:END:

Content one.

* Page Two
:PROPERTIES:
:END:

Content two.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (method _url &rest _args)
                             (when (eq method 'POST)
                               (setq call-count (1+ call-count))
                               (if (= call-count 1)
                                   (signal 'error '("First page failed"))
                                 '((url . "page-two-url")))))))
                  (org-canvas-sync-pages)
                  ;; Both should have been attempted
                  (expect call-count :to-equal 2)
                  ;; Failure isolation: page one's failure must not prevent
                  ;; page two from succeeding and being finalized.
                  (with-current-buffer (find-file-noselect org-file)
                    (goto-char (point-min))
                    (expect (org-entry-get (point) "CANVAS_URL") :to-be nil)
                    (re-search-forward "^\\* Page Two")
                    (org-back-to-heading)
                    (expect (org-entry-get (point) "CANVAS_URL")
                            :to-equal "page-two-url"))))))
        (delete-directory temp-dir t))))

  (it "sends a payload with the expected structure (not just any call)"
    (let ((temp-dir (make-temp-file "sync-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "pages.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Welcome
:PROPERTIES:
:END:

Hello world.
"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-pages)
                  (let* ((payload (test-org-canvas-api-call-data 'POST "pages"))
                         (page (gethash "wiki_page" payload)))
                    ;; Validate the actual body shape, so a renamed/dropped
                    ;; field is caught — not merely that a POST happened.
                    (expect (hash-table-p page) :to-be-truthy)
                    (expect (gethash "title" page) :to-equal "Welcome"))))))
        (delete-directory temp-dir t))))

  (it "errors when file not found"
    (let ((org-canvas-pages-file "/nonexistent/pages.org"))
      (with-sync-test-env
        (expect (org-canvas-sync-pages) :to-throw 'error)))))

(describe "org-canvas-define-sync macro validation"
  (it "errors when :file is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :parse #'identity
                            :build #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :file is required")))

  (it "errors when :parse is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :build #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :parse is required")))

  (it "errors when :build is missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :push #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :build is required")))

  (it "errors when :push and :endpoint are missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :build #'identity
                            :finalize #'identity))
            :to-throw 'error '("org-canvas-define-sync: :push or :endpoint is required")))

  (it "errors when :finalize and :endpoint are missing"
    (expect (macroexpand '(org-canvas-define-sync test-bad
                            :file some-file
                            :parse #'identity
                            :build #'identity
                            :push #'identity))
            :to-throw 'error '("org-canvas-define-sync: :finalize or :endpoint is required"))))

(describe "org-canvas-define-sync payload hashing"
  :var (temp-dir temp-file)

  (before-each
    (setq temp-dir (make-temp-file "sync-test-" t))
    (setq temp-file (expand-file-name "test.org" temp-dir))
    (with-temp-file temp-file
      (insert "* Item One\n:PROPERTIES:\n:END:\n\nContent.\n")))

  (after-each
    (let ((buf (find-buffer-visiting temp-file)))
      (when buf (kill-buffer buf)))
    (delete-directory temp-dir t))

  (it "saves PAYLOAD_HASH property after first sync"
    (with-org-canvas-test-config
      (with-mock-api
        (let* ((parse-called 0)
               (push-called 0)
               ;; Define a test sync using the internal macro structure
               ;; We call the pattern manually instead of using the macro
               (sync-file temp-file))
          ;; Simulate one iteration of the sync pipeline
          (with-current-buffer (find-file-noselect sync-file)
            (goto-char (point-min))
            (org-back-to-heading)
            (let* ((data (list :title "Item One" :canvas-id nil :pom (point)))
                   (payload '((title . "Item One")))
                   (payload-hash (md5 (json-encode payload))))
              ;; Before sync, no hash
              (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)
              ;; Simulate saving the hash
              (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
              (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal payload-hash)))))))

  (it "PAYLOAD_HASH is cleared by org-canvas-clear-sync-properties"
    (with-temp-org-buffer
     "* Test
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2025-01-01 Wed 00:00]
:PAYLOAD_HASH: abc123
:END:
"
     (org-back-to-heading)
     (org-canvas-clear-sync-properties (point))
     (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
     (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
     (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil))))

(describe "org-canvas--sync-payload-hash"
  (it "matches plain md5 of the encoded payload when no hash-extra fn is given"
    (let ((payload '((name . "X"))))
      (expect (org-canvas--sync-payload-hash payload nil nil)
              :to-equal (md5 (json-encode payload)))))

  (it "folds the hash-extra result into the hash"
    (let ((payload '((name . "X"))))
      (expect (org-canvas--sync-payload-hash payload nil (lambda (_) "extra"))
              :not :to-equal (md5 (json-encode payload)))))

  (it "passes the parsed data to the hash-extra fn"
    (let (seen)
      (org-canvas--sync-payload-hash '((a . 1)) '(:pom 42)
                                     (lambda (d) (setq seen d) ""))
      (expect seen :to-equal '(:pom 42)))))

(describe "metamorphic relations"
  (it "re-syncing unchanged content is a no-op (push idempotence)"
    ;; sync ∘ sync = sync: the second run must skip via PAYLOAD_HASH and make
    ;; no API write.  Runs the REAL pipeline twice (the existing payload-hash
    ;; test only simulates the save).
    (let ((temp-dir (make-temp-file "idem-" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "pages.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Welcome\n:PROPERTIES:\n:END:\n\nHello.\n"))
            (let ((org-canvas-pages-file org-file)
                  (org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (setq test-org-canvas-api-responses
                        '(("pages" . ((url . "welcome") (page_id . 5)))))
                  (org-canvas-sync-pages)             ; creates the page
                  (expect (test-org-canvas-api-called-p 'POST "pages")
                          :to-be-truthy)
                  (setq test-org-canvas-api-calls nil) ; observe only run 2
                  (org-canvas-sync-pages)             ; identical content
                  (expect (test-org-canvas-api-called-p 'POST "pages") :to-be nil)
                  (expect (test-org-canvas-api-called-p 'PUT "pages") :to-be nil)))))
        (delete-directory temp-dir t))))

  (it "heading parse is invariant to extra spaces after the stars"
    ;; `* Welcome' and `*  Welcome' must parse to the same title.
    (let ((d1 (with-temp-org-buffer "* Welcome\n:PROPERTIES:\n:END:\n"
                (org-back-to-heading) (org-canvas--page-parse-entry)))
          (d2 (with-temp-org-buffer "*  Welcome\n:PROPERTIES:\n:END:\n"
                (org-back-to-heading) (org-canvas--page-parse-entry))))
      (expect (plist-get d1 :title) :to-equal "Welcome")
      (expect (plist-get d1 :title) :to-equal (plist-get d2 :title)))))

(describe "org-canvas-define-sync at-point generation"
  (it "generates sync-page-at-point from sync macro"
    (expect (fboundp 'org-canvas-sync-page-at-point) :to-be-truthy))

  (it "generates sync-announcement-at-point"
    (expect (fboundp 'org-canvas-sync-announcement-at-point) :to-be-truthy))

  (it "generates sync-discussion-at-point"
    (expect (fboundp 'org-canvas-sync-discussion-at-point) :to-be-truthy))

  (it "generates sync-assignment-at-point"
    (expect (fboundp 'org-canvas-sync-assignment-at-point) :to-be-truthy))

  (it "generates sync-rubric-at-point"
    (expect (fboundp 'org-canvas-sync-rubric-at-point) :to-be-truthy))

  (it "generates sync-assignment-group-at-point"
    (expect (fboundp 'org-canvas-sync-assignment-group-at-point) :to-be-truthy))

  (it "singularizes group-categories correctly"
    (expect (fboundp 'org-canvas-sync-group-category-at-point) :to-be-truthy))

  (it "singularizes calendar-events correctly"
    (expect (fboundp 'org-canvas-sync-calendar-event-at-point) :to-be-truthy))

  (it "suppresses at-point with :no-at-point"
    ;; new-quizzes has :no-at-point t, its at-point is hand-written
    (expect (fboundp 'org-canvas-sync-new-quiz-at-point) :to-be-truthy)))

(describe "org-canvas--singularize"
  (it "uses the irregular-plural override when present"
    ;; Guards `(or (cdr (assoc ...)) (if ...))': an `and' there would fall
    ;; through to the naive trailing-s strip and mis-singularize.
    (expect (org-canvas--singularize "quizzes") :to-equal "quiz")
    (expect (org-canvas--singularize "new-quizzes") :to-equal "new-quiz")
    (expect (org-canvas--singularize "group-categories") :to-equal "group-category"))

  (it "strips a trailing s for regular plurals"
    (expect (org-canvas--singularize "pages") :to-equal "page")
    (expect (org-canvas--singularize "assignments") :to-equal "assignment")))

(describe "org-canvas--sync-process-entry conflict counter"
  (it "increments conflict counter when push returns conflict"
    (with-temp-org-buffer
     "* Conflict Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0 :conflict 0))
            (ctx (list :parse-fn (lambda () (list :title "Conflict Item"
                                                   :canvas-id "123"
                                                   :pom (point-marker)))
                       :build-fn (lambda (_data) '((title . "Conflict Item")))
                       :push-fn (lambda (_data _payload &optional _ctx) 'conflict)
                       :finalize-fn (lambda (_data _response &optional _ctx) nil)
                       :feature-name "items"
                       :feature-upper "ITEMS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :conflict) :to-equal 1)
       (expect (plist-get counters :success) :to-equal 0)))))

(describe "org-canvas--sync-log-summary with conflicts"
  (it "includes conflict and pulled counts in log when present"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 5 :skip 2 :fail 1 :conflict 3 :pulled 1))
            (let ((found-conflicts nil)
                  (found-pulled nil))
              (dolist (call (spy-calls-all-args 'org-canvas--log-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Conflicts" (nth 1 call)))
                  (setq found-conflicts t))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Pulled" (nth 1 call)))
                  (setq found-pulled t)))
              (expect found-conflicts :to-be-truthy)
              (expect found-pulled :to-be-truthy)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "omits conflict and pulled counts when zero"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 5 :skip 2 :fail 1 :conflict 0 :pulled 0))
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'org-canvas--log-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Conflicts" (nth 1 call)))
                  (setq found t)))
              (expect found :to-be nil)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--sync-execute-pipeline pulled counter"
  (it "increments pulled counter when push returns pulled"
    (with-temp-org-buffer
     "* Pulled Item
:PROPERTIES:
:CANVAS_ID: 333
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0 :conflict 0 :pulled 0))
            (ctx (list :parse-fn (lambda () (list :title "Pulled Item"
                                                   :canvas-id "333"
                                                   :pom (point-marker)))
                       :build-fn (lambda (_data) '((title . "Pulled Item")))
                       :push-fn (lambda (_data _payload &optional _ctx) 'pulled)
                       :finalize-fn (lambda (_data _response &optional _ctx) nil)
                       :feature-name "items"
                       :feature-upper "ITEMS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :pulled) :to-equal 1)
       (expect (plist-get counters :success) :to-equal 0)))))

(describe "org-canvas-define-sync conflict bindings"
  (it "binds conflict-apply-all to nil per sync"
    ;; Verify the macro delegates to sync-run-pipeline (which handles bindings)
    (let ((expanded (macroexpand
                     '(org-canvas-define-sync test-features
                        :file "/tmp/test.org"
                        :parse #'identity
                        :build #'identity
                        :push #'identity
                        :finalize #'identity
                        :pull-item-fn #'ignore))))
      ;; The expanded form should call sync-run-pipeline
      (expect (format "%S" expanded)
              :to-match "org-canvas--sync-run-pipeline")
      ;; Pull-item-fn should be passed through
      (expect (format "%S" expanded)
              :to-match "ignore"))))

(describe "org-canvas--sync-collect-entries duplicate warning"
  (it "warns about duplicate CANVAS_IDs"
    (let* ((temp-dir (make-temp-file "dup-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item 1
:PROPERTIES:
:CANVAS_ID: DUP-1
:END:
* Item 2
:PROPERTIES:
:CANVAS_ID: DUP-1
:END:
"))
            (spy-on 'org-canvas--log-warning)
            (with-org-canvas-test-config
              (org-canvas--sync-collect-entries test-file "LEVEL=1" "test")
              (let ((dup-warned nil))
                (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
                  (when (and (>= (length call) 2)
                             (stringp (nth 1 call))
                             (string-match-p "Duplicate" (nth 1 call)))
                    (setq dup-warned t)))
                (expect dup-warned :to-be-truthy))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t))))

  (it "does not warn when all CANVAS_IDs are unique"
    ;; Guards the `(> count 1)' boundary: a >= would warn for every id
    ;; (each appears once), producing false duplicate warnings.
    (let* ((temp-dir (make-temp-file "dup-test" t))
           (test-file (expand-file-name "test.org" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item One
:PROPERTIES:
:CANVAS_ID: UNIQ-1
:END:
* Item Two
:PROPERTIES:
:CANVAS_ID: UNIQ-2
:END:
"))
            (spy-on 'org-canvas--log-warning)
            (with-org-canvas-test-config
              (org-canvas--sync-collect-entries test-file "LEVEL=1" "test")
              (let ((dup-warned nil))
                (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
                  (when (and (>= (length call) 2)
                             (stringp (nth 1 call))
                             (string-match-p "Duplicate\\] CANVAS_ID" (nth 1 call)))
                    (setq dup-warned t)))
                (expect dup-warned :to-be nil))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--sync-execute-pipeline dry-run"
  (it "skips API call in dry-run mode"
    (let* ((temp-dir (make-temp-file "dry-test" t))
           (test-file (expand-file-name "test.org" temp-dir))
           (api-called nil))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "* Item
:PROPERTIES:
:END:
"))
            (with-org-canvas-test-config
              (with-sync-test-env
                (let ((org-canvas--dry-run t))
                  (cl-letf (((symbol-function 'org-canvas-api-request)
                             (lambda (&rest _args)
                               (setq api-called t)
                               '((id . 1)))))
                    (let* ((targets (org-canvas--sync-collect-entries
                                    test-file "LEVEL=1" "test"))
                           (counters (list :success 0 :skip 0 :fail 0 :conflict 0 :pulled 0))
                           (synced-ids (list nil))
                           (ctx (list :parse-fn (lambda ()
                                                  (list :title "Item" :canvas-id nil
                                                        :pom (point-marker)))
                                      :build-fn (lambda (_data) '((title . "Item")))
                                      :push-fn (lambda (_data _payload &optional _ctx)
                                                 (setq api-called t)
                                                 '((id . 1)))
                                      :finalize-fn (lambda (_data _response &optional _ctx) nil)
                                      :feature-name "test" :feature-upper "TEST"
                                      :total-count 1 :counters counters
                                      :synced-ids synced-ids
                                      :title-key :title)))
                      (dolist (marker (plist-get targets :targets))
                        (org-canvas--sync-process-entry marker ctx))
                      (expect api-called :to-be nil)))))))
        (let ((buf (find-buffer-visiting test-file)))
          (when buf (kill-buffer buf)))
        (delete-directory temp-dir t)))))

(describe "org-canvas--sync-warn-orphans"
  (it "warns about IDs not in synced set"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--sync-warn-orphans '("111" "222" "333") '("111" "333") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
        (when (and (>= (length call) 3)
                   (stringp (nth 1 call))
                   (string-match-p "Orphan" (nth 1 call)))
          (setq orphan-warned t)))
      (expect orphan-warned :to-be-truthy)))

  (it "does not warn when all IDs synced"
    (spy-on 'org-canvas--log-warning)
    (org-canvas--sync-warn-orphans '("111" "222") '("111" "222") "test")
    (let ((orphan-warned nil))
      (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
        (when (and (>= (length call) 2)
                   (stringp (nth 1 call))
                   (string-match-p "Orphan" (nth 1 call)))
          (setq orphan-warned t)))
      (expect orphan-warned :to-be nil))))

(describe "org-canvas--make-push-fn-form"
  (it "generates a lambda with endpoint only"
    (let ((form (org-canvas--make-push-fn-form "pages" nil nil nil)))
      (expect (car form) :to-equal 'lambda)
      (expect (format "%S" form) :to-match ":endpoint \"pages\"")))

  (it "includes id-key when provided"
    (let ((form (org-canvas--make-push-fn-form "pages" :canvas-url nil nil)))
      (expect (format "%S" form) :to-match ":id-key :canvas-url")))

  (it "includes title-key when provided"
    (let ((form (org-canvas--make-push-fn-form "pages" nil :name nil)))
      (expect (format "%S" form) :to-match ":title-key :name")))

  (it "includes find-fn when provided"
    (let ((form (org-canvas--make-push-fn-form "pages" nil nil '#'my-find)))
      (expect (format "%S" form) :to-match "my-find")))

  (it "omits optional keys when nil"
    (let ((form-str (format "%S" (org-canvas--make-push-fn-form "items" nil nil nil))))
      (expect form-str :not :to-match ":id-key")
      (expect form-str :not :to-match ":title-key")
      (expect form-str :not :to-match ":find-fn"))))

(describe "org-canvas--make-finalize-fn-form"
  (it "generates a lambda with no optional keys"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil nil)))
      (expect (car form) :to-equal 'lambda)
      (let ((form-str (format "%S" form)))
        (expect form-str :not :to-match ":id-field")
        (expect form-str :not :to-match ":id-property")
        (expect form-str :not :to-match ":post-fn"))))

  (it "includes id-field when provided"
    (let ((form (org-canvas--make-finalize-fn-form 'url nil nil nil)))
      (expect (format "%S" form) :to-match ":id-field")))

  (it "includes id-property when provided"
    (let ((form (org-canvas--make-finalize-fn-form nil "CANVAS_URL" nil nil)))
      (expect (format "%S" form) :to-match ":id-property \"CANVAS_URL\"")))

  (it "includes post-fn when provided"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil '#'my-post)))
      (expect (format "%S" form) :to-match "my-post")))

  (it "carries the endpoint alongside a post-fn, for the restamp (issue #124)"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil '#'my-post
                                                   "assignments")))
      (expect (format "%S" form) :to-match ":endpoint \"assignments\"")))

  (it "leaves the endpoint out when there is no post-fn to restamp after"
    (let ((form (org-canvas--make-finalize-fn-form nil nil nil nil "pages")))
      (expect (format "%S" form) :not :to-match ":endpoint"))))

(describe "org-canvas--sync-run-pipeline"
  (it "runs the full pipeline for entries in a file"
    (let ((temp-dir (make-temp-file "pipeline-test" t)))
      (unwind-protect
          (let* ((org-file (expand-file-name "test.org" temp-dir))
                 (parse-count 0))
            (with-temp-file org-file
              (insert "* Entry One\n:PROPERTIES:\n:END:\n\n* Entry Two\n:PROPERTIES:\n:END:\n"))
            (let ((org-canvas-base-url "https://test.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (cl-letf (((symbol-function 'org-canvas-api-request)
                           (lambda (_method _url &rest _args)
                             (setq parse-count (1+ parse-count))
                             '((id . 1)))))
			 (org-canvas--sync-run-pipeline (list
							 :feature "test" :file org-file :query "LEVEL=1"
							 :parse #'org-canvas--announcement-parse-entry
							 :build #'org-canvas--announcement-build-payload
							 :push (lambda (data payload &optional ctx)
								 (org-canvas--push-to-api data payload :endpoint "test" :ctx ctx))
							 :finalize (lambda (data response &optional ctx)
								     (org-canvas--finalize-item data response :ctx ctx))))
                  (expect parse-count :to-be-truthy)))))
        (delete-directory temp-dir t))))

  (it "starts every run with a context holding no apply-all answer"
    (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
              ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
              ((symbol-function 'org-canvas--sync-collect-entries)
               (lambda (&rest _) (list :targets nil :all-ids-before nil)))
              ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
              ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
	     (let ((ctx (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
							     :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore))))
        (expect (plist-get ctx :conflict-apply-all) :to-be nil)
        (expect (plist-get ctx :duplicate-apply-all) :to-be nil)
        (expect (plist-get ctx :feature-name) :to-equal "test"))))

  (it "carries the pull-item-fn argument in the returned context"
    (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
              ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
              ((symbol-function 'org-canvas--sync-collect-entries)
               (lambda (&rest _) (list :targets nil :all-ids-before nil)))
              ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
              ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
	     (let ((ctx (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
							     :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore
							     :pull-item-fn #'my-pull-fn))))
        (expect (plist-get ctx :pull-item-fn) :to-equal #'my-pull-fn)))))

(describe "org-canvas--parse-gen-transform-form"
  (it "generates enum form with default value"
    (let ((form (org-canvas--parse-gen-transform-form
                 "STATUS" :status 'enum "active"
                 '("active" "inactive"))))
      ;; Should produce a validate-property call with the default
      (expect form :to-contain 'org-canvas--validate-property)
      (expect (nth 4 form) :to-equal "active"))))

(describe "org-canvas-define-parse :after-read hook"
  (it "generates read-fn that calls after-read"
    (eval
     '(org-canvas-define-parse test--after-read-cov
        :after-read (lambda (raw _pom)
                      (plist-put raw :extra "injected")
                      raw)
        :properties
        (("TITLE_PROP" :title-prop :type string)))
     t)
    (with-temp-org-buffer
     "* Heading
:PROPERTIES:
:TITLE_PROP: hello
:END:
"
     (org-back-to-heading)
     (let ((raw (org-canvas--test--after-read-cov-read-props (point))))
       (expect (plist-get raw :extra) :to-equal "injected")))))

(describe "org-canvas--sync-log-summary global counters"
  (it "accumulates counts into org-canvas--sync-global-counters"
    (let* ((temp-file (make-temp-file "sum-test" nil ".org"))
           (org-canvas--sync-global-counters (list :success 0 :skip 0 :fail 0)))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Test\n"))
            (org-canvas--sync-log-summary "test" temp-file
                                          (list :success 3 :skip 1 :fail 2))
            (org-canvas--sync-log-summary "test2" temp-file
                                          (list :success 5 :skip 0 :fail 1))
            (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 8)
            (expect (plist-get org-canvas--sync-global-counters :skip) :to-equal 1)
            (expect (plist-get org-canvas--sync-global-counters :fail) :to-equal 3))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "does not accumulate when global counters are nil"
    (let* ((temp-file (make-temp-file "sum-test" nil ".org"))
           (org-canvas--sync-global-counters nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Test\n"))
            ;; Should not error when counters are nil
            (expect (org-canvas--sync-log-summary "test" temp-file
                                                  (list :success 1 :skip 0 :fail 0))
                    :not :to-throw))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--sync-collect-entries"
  (it "warns in minibuffer about duplicate CANVAS_IDs"
    (let* ((temp-file (make-temp-file "dup-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "* Item A\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n\n")
              (insert "* Item B\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\n"))
            (spy-on 'message)
            (spy-on 'org-canvas--log-warning)
            (org-canvas--sync-collect-entries temp-file "LEVEL=1" "test")
            (expect 'message :to-have-been-called)
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'message))
                (when (and (stringp (car call))
                           (string-match-p "CANVAS_ID 100 appears 2 times" (apply #'format call)))
                  (setq found t)))
              (expect found :to-be-truthy)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--sync-process-entry error includes heading title"
  (it "includes heading title when parse-fn errors"
    (with-temp-org-buffer
     "* My Assignment
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :parse-fn (lambda () (error "Parse failed"))
                       :build-fn #'ignore
                       :push-fn #'ignore
                       :finalize-fn #'ignore
                       :feature-name "assignments"
                       :feature-upper "ASSIGNMENTS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (spy-on 'org-canvas--log-error)
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :fail) :to-equal 1)
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-error))
           (when (>= (length call) 3)
             (let ((formatted (apply #'format (cdr call))))
               (when (and (string-match-p "ASSIGNMENTS" formatted)
                          (string-match-p "My Assignment" formatted))
                 (setq found t)))))
         (expect found :to-be-truthy)))))

  (it "includes heading title when build-fn errors"
    (with-temp-org-buffer
     "* Quiz One
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :parse-fn (lambda () (list :title "Quiz One"))
                       :build-fn (lambda (_) (error "Build failed"))
                       :push-fn #'ignore
                       :finalize-fn #'ignore
                       :feature-name "quizzes"
                       :feature-upper "QUIZZES"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (spy-on 'org-canvas--log-error)
       (org-canvas--sync-process-entry marker ctx)
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-error))
           (when (and (>= (length call) 3)
                      (stringp (nth 1 call)))
             (let ((formatted (apply #'format (cdr call))))
               (when (string-match-p "Quiz One" formatted)
                 (setq found t)))))
         (expect found :to-be-truthy))))))

(describe "org-canvas--sync-warn-duplicate-titles"
  (it "warns when duplicate titles exist"
    (with-temp-org-buffer
     "* Same Title
:PROPERTIES:
:END:

* Same Title
:PROPERTIES:
:END:

* Different Title
:PROPERTIES:
:END:
"
     (let ((markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))
       (spy-on 'org-canvas--log-warning)
       (org-canvas--sync-warn-duplicate-titles markers (buffer-file-name))
       (let ((found nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-warning))
           (when (and (>= (length call) 3)
                      (stringp (nth 1 call))
                      (string-match-p "Duplicate Title" (nth 1 call)))
             (let ((formatted (apply #'format (cdr call))))
               (when (string-match-p "Same Title" formatted)
                 (setq found t)))))
         (expect found :to-be-truthy)))))

  (it "does not warn when all titles are distinct"
    (with-temp-org-buffer
     "* Title A
:PROPERTIES:
:END:

* Title B
:PROPERTIES:
:END:
"
     (let ((markers (org-map-entries (lambda () (point-marker)) "LEVEL=1" 'file)))
       (spy-on 'org-canvas--log-warning)
       (org-canvas--sync-warn-duplicate-titles markers (buffer-file-name))
       (expect 'org-canvas--log-warning :not :to-have-been-called)))))

(describe "org-canvas--sync-execute-pipeline dry-run counter"
  (it "increments dry-run counter instead of success"
    (with-temp-org-buffer
     "* Dry Run Item
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (let* ((org-canvas--dry-run t)
            (counters (list :success 0 :skip 0 :fail 0 :dry-run 0))
            (ctx (list :push-fn #'ignore
                       :feature-name "pages"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil)))
            (data (list :title "Dry Run Item" :canvas-id nil))
            (payload '((title . "Dry Run Item"))))
       (org-canvas--sync-execute-pipeline data payload ctx)
       (expect (plist-get counters :dry-run) :to-equal 1)
       (expect (plist-get counters :success) :to-equal 0)))))

(describe "org-canvas--sync-log-summary dry-run format"
  (it "shows dry-run format when dry-run count > 0"
    (let ((temp-file (make-temp-file "summary-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (spy-on 'message)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 0 :skip 2 :fail 0 :dry-run 3))
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'org-canvas--log-info))
                (when (and (>= (length call) 2)
                           (stringp (nth 1 call))
                           (string-match-p "Would sync" (nth 1 call)))
                  (setq found t)))
              (expect found :to-be-truthy))
            (let ((found nil))
              (dolist (call (spy-calls-all-args 'message))
                (when (and (stringp (car call))
                           (string-match-p "dry-run" (car call)))
                  (setq found t)))
              (expect found :to-be-truthy)))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "accumulates dry-run count into global counters"
    (let ((temp-file (make-temp-file "summary-test" nil ".org"))
          (org-canvas--sync-global-counters (list :success 0 :skip 0 :fail 0 :dry-run 0)))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (spy-on 'org-canvas--log-info)
            (spy-on 'message)
            (org-canvas--sync-log-summary "test" temp-file
             '(:success 0 :skip 1 :fail 0 :dry-run 5))
            (expect (plist-get org-canvas--sync-global-counters :dry-run)
                    :to-equal 5))
        (let ((buf (find-buffer-visiting temp-file)))
          (when buf (kill-buffer buf)))
        (delete-file temp-file)))))

(describe "org-canvas--sync-warn-stale-headings"
  (it "warns and prompts when heading has LAST_SYNCED but no CANVAS_ID"
    (with-temp-org-buffer
     "* Stale Item
:PROPERTIES:
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (spy-on 'y-or-n-p :and-return-value t)
     (let ((noninteractive nil)
           (markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       (expect 'message :to-have-been-called)
       (expect 'y-or-n-p :to-have-been-called))))

  (it "aborts when user declines stale heading prompt"
    (with-temp-org-buffer
     "* Stale Item
:PROPERTIES:
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'y-or-n-p :and-return-value nil)
     (let ((noninteractive nil)
           (markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (expect (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
               :to-throw 'user-error))))

  (it "does not warn for normal heading with CANVAS_ID"
    (with-temp-org-buffer
     "* Normal Item
:PROPERTIES:
:CANVAS_ID: 123
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (let ((markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       (expect 'message :not :to-have-been-called))))

  (it "does not warn for new heading without LAST_SYNCED"
    (with-temp-org-buffer
     "* New Item
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (let ((markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       (expect 'message :not :to-have-been-called))))

  (it "lists all stale headings in single warning when multiple exist"
    (with-temp-org-buffer
     "* Stale A
:PROPERTIES:
:LAST_SYNCED: [2025-01-01 Wed 10:00]
:END:
* Stale B
:PROPERTIES:
:LAST_SYNCED: [2025-02-01 Wed 10:00]
:END:
"
     (spy-on 'message)
     (spy-on 'org-canvas--log-warning)
     (spy-on 'y-or-n-p :and-return-value t)
     (let ((noninteractive nil)
           (markers (org-map-entries (lambda () (point-marker)) nil 'file)))
       (org-canvas--sync-warn-stale-headings markers (buffer-file-name))
       ;; One consolidated message + one prompt
       (expect 'message :to-have-been-called-times 1)
       (expect 'y-or-n-p :to-have-been-called-times 1)
       ;; Both titles logged individually
       (expect 'org-canvas--log-warning :to-have-been-called-times 2)))))

(describe "org-canvas--sync-process-entry deferred counter"
  (it "counts deferred drop-rule rejections separately from failures"
    (with-temp-org-buffer
     "* Group A
:PROPERTIES:
:CANVAS_ID: 5
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :parse-fn
                       (lambda ()
                         (error "Drop rules cannot be higher than the number of assignments (HTTP 400)"))
                       :build-fn #'ignore
                       :push-fn #'ignore
                       :finalize-fn #'ignore
                       :feature-name "assignment-groups"
                       :feature-upper "ASSIGNMENT-GROUPS"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil))))
       (spy-on 'org-canvas--log-error)
       (org-canvas--sync-process-entry marker ctx)
       (expect (plist-get counters :deferred) :to-equal 1)
       (expect (plist-get counters :fail) :to-equal 0)
       (expect 'org-canvas--log-error :not :to-have-been-called)))))

(describe "org-canvas--sync-log-summary deferred count"
  (it "logs a Deferred line when counters include :deferred"
    (let ((temp-file (make-temp-file "sync-summary-" nil ".org"))
          (logged nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged))))
            (org-canvas--sync-log-summary "groups" temp-file
                                          (list :success 1 :skip 0 :fail 0
                                                :deferred 2))
            (expect (cl-find-if (lambda (l) (string-match-p "Deferred: 2" l))
                                logged)
                    :to-be-truthy))
        (delete-file temp-file))))

  (it "omits the Deferred line when nothing was deferred"
    (let ((temp-file (make-temp-file "sync-summary-" nil ".org"))
          (logged nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged))))
            (org-canvas--sync-log-summary "groups" temp-file
                                          (list :success 1 :skip 0 :fail 0))
            (expect (cl-find-if (lambda (l) (string-match-p "Deferred:" l))
                                logged)
                    :to-be nil))
        (delete-file temp-file)))))

(describe "org-canvas--sync-record-feature-stats"
  (it "is a no-op when no global sync is active"
    (let ((org-canvas--sync-global-counters nil)
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats "Pages" '(:success 3))
      (expect org-canvas--sync-global-feature-stats :to-be nil)))

  (it "accumulates aggregate counters including deferred"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats
       "Groups" '(:success 2 :skip 1 :fail 1 :deferred 1))
      (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 2)
      (expect (plist-get org-canvas--sync-global-counters :skip) :to-equal 1)
      (expect (plist-get org-canvas--sync-global-counters :fail) :to-equal 1)
      (expect (plist-get org-canvas--sync-global-counters :deferred) :to-equal 1)))

  (it "merges repeated records for the same label"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats
       "Module Items" '(:success 3 :skip 1 :skipped-titles ("A (no linked content)")))
      (org-canvas--sync-record-feature-stats
       "Module Items" '(:success 2 :fail 1 :failed-titles ("B")))
      (expect (length org-canvas--sync-global-feature-stats) :to-equal 1)
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :success) :to-equal 5)
        (expect (plist-get entry :skip) :to-equal 1)
        (expect (plist-get entry :fail) :to-equal 1)
        (expect (plist-get entry :skipped-titles)
                :to-equal '("A (no linked content)"))
        (expect (plist-get entry :failed-titles) :to-equal '("B")))))

  ;; Issue #66: a counter the run populated but the entry dropped never
  ;; reaches the table, and the table is what people read.
  (it "carries the dry-run, conflict and pulled counters into the entry"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats
       "Assignments" '(:dry-run 31 :skip 30 :conflict 2 :pulled 1))
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :dry-run) :to-equal 31)
        (expect (plist-get entry :conflict) :to-equal 2)
        (expect (plist-get entry :pulled) :to-equal 1))))

  (it "merges those counters across repeated records for one label"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats "Groups" '(:dry-run 2 :conflict 1))
      (org-canvas--sync-record-feature-stats "Groups" '(:dry-run 3 :pulled 4))
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :dry-run) :to-equal 5)
        (expect (plist-get entry :conflict) :to-equal 1)
        (expect (plist-get entry :pulled) :to-equal 4))))

  (it "accumulates conflicts and pulls into the aggregate counters"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-record-feature-stats "Pages" '(:conflict 3 :pulled 2))
      (expect (plist-get org-canvas--sync-global-counters :conflict) :to-equal 3)
      (expect (plist-get org-canvas--sync-global-counters :pulled) :to-equal 2))))

(describe "org-canvas--sync-summary-columns"
  (it "replaces Success with Would sync when the run was a dry run"
    ;; The reported symptom: a preview printing 0 success reads as clean.
    (expect (org-canvas--sync-summary-columns '((:dry-run 31 :skip 30)))
            :to-equal '(("Would sync" . :dry-run) ("Skipped" . :skip)
                        ("Failed" . :fail) ("Deferred" . :deferred))))

  (it "keeps the narrow table for an ordinary clean sync"
    (expect (org-canvas--sync-summary-columns '((:success 8 :skip 1)))
            :to-equal '(("Success" . :success) ("Skipped" . :skip)
                        ("Failed" . :fail) ("Deferred" . :deferred))))

  (it "adds a Conflicts column only when the run hit conflicts"
    (expect (mapcar #'car (org-canvas--sync-summary-columns
                           '((:success 8 :conflict 5))))
            :to-equal '("Success" "Skipped" "Failed" "Deferred" "Conflicts")))

  (it "adds a Pulled column only when something was pulled"
    (expect (mapcar #'car (org-canvas--sync-summary-columns
                           '((:success 8 :pulled 2))))
            :to-equal '("Success" "Skipped" "Failed" "Deferred" "Pulled")))

  (it "shows both when both happened, across different features"
    (expect (mapcar #'car (org-canvas--sync-summary-columns
                           '((:success 8 :conflict 1) (:success 2 :pulled 1))))
            :to-equal '("Success" "Skipped" "Failed" "Deferred"
                        "Conflicts" "Pulled"))))

(describe "org-canvas--sync-stat-total"
  (it "sums a counter across features, treating an absent one as zero"
    (expect (org-canvas--sync-stat-total
             '((:dry-run 31) (:skip 2) (:dry-run 16)) :dry-run)
            :to-equal 47)))

(describe "org-canvas--sync-log-summary feature stats recording"
  (it "records the feature's counters under its capitalized label"
    (let ((temp-file (make-temp-file "sync-summary-" nil ".org"))
          (org-canvas--sync-global-counters
           (list :success 0 :skip 0 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (unwind-protect
          (progn
            (org-canvas--sync-log-summary "pages" temp-file
                                          (list :success 2 :skip 1 :fail 1
                                                :failed-titles '("Course Home")))
            (let ((entry (car org-canvas--sync-global-feature-stats)))
              (expect (plist-get entry :label) :to-equal "Pages")
              (expect (plist-get entry :success) :to-equal 2)
              (expect (plist-get entry :failed-titles)
                      :to-equal '("Course Home")))
            (expect (plist-get org-canvas--sync-global-counters :success)
                    :to-equal 2))
        (delete-file temp-file)))))

(describe "org-canvas--sync-reclassify-skip-as-success"
  (it "moves one skip to success in aggregates and the labeled entry"
    (let ((org-canvas--sync-global-counters
           (list :success 5 :skip 2 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats
           (list (list :label "Module Items" :success 3 :skip 2 :fail 0
                       :deferred 0 :failed-titles nil
                       :skipped-titles '("A (no linked content)" "B (no linked content)")))))
      (org-canvas--sync-reclassify-skip-as-success "Module Items" "A")
      (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 6)
      (expect (plist-get org-canvas--sync-global-counters :skip) :to-equal 1)
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :success) :to-equal 4)
        (expect (plist-get entry :skip) :to-equal 1)
        (expect (plist-get entry :skipped-titles)
                :to-equal '("B (no linked content)")))))

  (it "is a no-op when no global sync is active"
    (let ((org-canvas--sync-global-counters nil)
          (org-canvas--sync-global-feature-stats
           (list (list :label "Module Items" :success 0 :skip 1 :fail 0
                       :deferred 0 :failed-titles nil :skipped-titles '("A")))))
      (org-canvas--sync-reclassify-skip-as-success "Module Items" "A")
      (expect (plist-get (car org-canvas--sync-global-feature-stats) :skip)
              :to-equal 1)))

  (it "credits the counter it is given, as a dry run does (issue #308)"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 1 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats
           (list (list :label "Module Items" :success 0 :skip 1 :fail 0
                       :deferred 0 :failed-titles nil :skipped-titles '("A")))))
      (org-canvas--sync-reclassify-skip-as-success "Module Items" "A" :dry-run)
      (expect (plist-get org-canvas--sync-global-counters :dry-run) :to-equal 1)
      (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 0)
      (let ((entry (car org-canvas--sync-global-feature-stats)))
        (expect (plist-get entry :dry-run) :to-equal 1)
        (expect (plist-get entry :success) :to-equal 0)
        (expect (plist-get entry :skip) :to-equal 0))))

  (it "tolerates a label with no recorded entry"
    (let ((org-canvas--sync-global-counters
           (list :success 0 :skip 1 :fail 0 :dry-run 0 :deferred 0))
          (org-canvas--sync-global-feature-stats nil))
      (org-canvas--sync-reclassify-skip-as-success "Module Items" "A")
      (expect (plist-get org-canvas--sync-global-counters :success) :to-equal 1))))

(describe "org-canvas--sync-log-global-summary"
  (it "renders the per-type table and named failed/skipped items"
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Module Items" :success 15 :skip 1 :fail 0
                       :deferred 0 :failed-titles nil
                       :skipped-titles '("Course Home (no linked content synced)"))
                 (list :label "Pages" :success 8 :skip 0 :fail 1 :deferred 0
                       :failed-titles '("Course Home") :skipped-titles nil)))
          (logged-info nil)
          (logged-warn nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged-info)))
                ((symbol-function 'org-canvas--log-warning)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged-warn))))
        (org-canvas--sync-log-global-summary))
      ;; Stats were pushed in reverse sync order; table renders Pages first
      (let ((lines (nreverse logged-info)))
        (expect (nth 0 lines) :to-match "Type.*Success.*Skipped.*Failed.*Deferred")
        (expect (nth 1 lines) :to-match "Pages +8 +0 +1 +0")
        (expect (nth 2 lines) :to-match "Module Items +15 +1 +0 +0"))
      (expect (cl-find-if (lambda (l) (string-match-p "Failed Pages: 'Course Home'" l))
                          logged-warn)
              :to-be-truthy)
      (expect (cl-find-if (lambda (l)
                            (string-match-p "Skipped Module Items: 'Course Home (no linked content synced)'" l))
                          logged-warn)
              :to-be-truthy)))

  (it "logs nothing when no stats were recorded"
    (let ((org-canvas--sync-global-feature-stats nil)
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged))))
        (org-canvas--sync-log-global-summary))
      (expect logged :to-be nil)))

  ;; Issue #66, the whole point: the table has to describe the run that
  ;; happened.  The reported case was 31 assignments and 16 modules pending
  ;; a push, printed as 0 success.
  (it "reports what a dry run would do, under a header that says so"
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Modules" :success 0 :skip 0 :fail 0 :deferred 0
                       :dry-run 16 :conflict 0 :pulled 0)
                 (list :label "Assignments" :success 0 :skip 30 :fail 0
                       :deferred 0 :dry-run 31 :conflict 0 :pulled 0)))
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (let ((lines (nreverse logged)))
        (expect (nth 0 lines) :to-match "DRY RUN")
        (expect (nth 1 lines) :to-match "Type.*Would sync.*Skipped")
        (expect (nth 1 lines) :not :to-match "Success")
        (expect (nth 2 lines) :to-match "Assignments +31 +30 +0 +0")
        (expect (nth 3 lines) :to-match "Modules +16 +0 +0 +0"))))

  (it "shows conflicts and pulls a real sync hit"
    ;; The worse half of #66: a run with five conflicts printed 0 failed
    ;; and nothing else, so the table said the course was clean.
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Pages" :success 8 :skip 0 :fail 0 :deferred 0
                       :dry-run 0 :conflict 5 :pulled 2)))
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (let ((lines (nreverse logged)))
        (expect (nth 0 lines) :to-match "Conflicts.*Pulled")
        (expect (nth 1 lines) :to-match "Pages +8 +0 +0 +0 +5 +2"))))

  (it "says nothing about a dry run when the sync was real"
    (let ((org-canvas--sync-global-feature-stats
           (list (list :label "Pages" :success 8 :skip 0 :fail 0 :deferred 0
                       :dry-run 0 :conflict 0 :pulled 0)))
          (logged nil))
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (expect (cl-find-if (lambda (l) (string-match-p "DRY RUN" l)) logged)
              :to-be nil))))

(describe "org-canvas--sync-run-pipeline :after-sync"
  (it "runs the hook after entries, before the summary"
    ;; Ordering matters: the hook reports on state the sync just produced,
    ;; and its output should read above the SYNC COMPLETE banner.
    (let ((order nil))
      (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
                ((symbol-function 'org-canvas--sync-collect-entries)
                 (lambda (&rest _) (list :targets nil :all-ids-before nil)))
                ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                ((symbol-function 'org-canvas--sync-log-summary)
                 (lambda (&rest _) (push 'summary order))))
               (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
						    :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore
						    :after-sync (lambda (ctx) (push (if (plist-get ctx :feature-name) 'after-sync 'no-ctx) order))))
        (expect (nreverse order) :to-equal '(after-sync summary)))))

  (it "is optional"
    (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
              ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
              ((symbol-function 'org-canvas--sync-collect-entries)
               (lambda (&rest _) (list :targets nil :all-ids-before nil)))
              ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
              ((symbol-function 'org-canvas--sync-log-summary) #'ignore))
	     (expect (org-canvas--sync-run-pipeline (list :feature "test" :file "/tmp/test.org" :query "LEVEL=1"
							  :parse #'ignore :build #'ignore :push #'ignore :finalize #'ignore))
              :not :to-throw)))

  (it "is wired through org-canvas-define-sync for assignment groups"
    ;; Guards the macro plumbing, not just the runtime argument: a
    ;; :after-sync that never reaches the pipeline would fail silently.
    (let ((hook-ran nil))
      (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                ((symbol-function 'org-canvas--sync-validate-file) #'ignore)
                ((symbol-function 'org-canvas--sync-collect-entries)
                 (lambda (&rest _) (list :targets nil :all-ids-before nil)))
                ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                ((symbol-function 'org-canvas--sync-log-summary) #'ignore)
                ((symbol-function 'org-canvas--assignment-group-reconcile-unmanaged)
                 (lambda (&rest _) (setq hook-ran t))))
        (org-canvas-sync-assignment-groups)
        (expect hook-ran :to-be t)))))

(describe "org-canvas--dry-run-decision-note (issue #84)"
  (it "names the standing answer when the strategy is set"
    (let ((org-canvas-conflict-strategy 'push))
      (expect (org-canvas--dry-run-decision-note 'org-canvas-conflict-strategy)
              :to-equal "; org-canvas-conflict-strategy is push")))

  (it "says a batch sync would skip"
    (let ((org-canvas-conflict-strategy nil) (noninteractive t))
      (expect (org-canvas--dry-run-decision-note 'org-canvas-conflict-strategy)
              :to-equal "; a batch sync would skip it")))

  (it "says a real sync would ask"
    (let ((org-canvas-duplicate-title-strategy nil) (noninteractive nil))
      (expect (org-canvas--dry-run-decision-note 'org-canvas-duplicate-title-strategy)
              :to-equal "; a real sync would ask"))))

(describe "org-canvas--sync-dry-run-entry (issue #84)"
  (defun test-dry-run-84--run (heading data remote-updated titles)
    "Run the dry-run branch for HEADING with DATA against a snapshot.
REMOTE-UPDATED and TITLES are the two halves of the snapshot.
Returns (COUNTERS . LOG-LINES)."
    (let ((logged nil) (counters nil))
      (with-temp-org-buffer heading
        (org-back-to-heading)
        (let* ((org-canvas--dry-run t)
               (ctx (list :push-fn (lambda (&rest _) (error "Must not push"))
                          :feature-name "assignments" :total-count 1
                          :counters (list :success 0 :skip 0 :fail 0
                                          :dry-run 0 :dry-run-conflict 0)
                          :synced-ids (list nil)
                          :baseline nil
                          :remote-updated remote-updated :remote-titles titles)))
          (cl-letf (((symbol-function 'org-canvas--log-info)
                     (lambda (_logger fmt &rest args)
                       (push (apply #'format fmt args) logged)))
                    ((symbol-function 'message) #'ignore))
            (org-canvas--sync-execute-pipeline data '((name . "x")) ctx))
          (setq counters (plist-get ctx :counters))))
      (cons counters (nreverse logged))))

  (defconst test-dry-run-84--stamped
    "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:CANVAS_UPDATED_AT: 2026-08-19T13:19:49Z
:END:
")

  (it "counts a remotely newer entry as a conflict, not a push"
    (let ((map (make-hash-table :test 'equal))
          (org-canvas-conflict-strategy nil) (noninteractive nil))
      (puthash "61" "2026-08-25T00:00:00Z" map)
      (let ((run (test-dry-run-84--run test-dry-run-84--stamped
                                        (list :title "Lab 1" :canvas-id "61") map nil)))
        (expect (plist-get (car run) :dry-run-conflict) :to-equal 1)
        (expect (plist-get (car run) :dry-run) :to-equal 0)
        (expect (car (cdr run))
                :to-equal "[DRY-RUN] Would CONFLICT 'Lab 1' (remote updated at 2026-08-25T00:00:00Z; a real sync would ask)"))))

  (it "names the standing conflict strategy"
    (let ((map (make-hash-table :test 'equal))
          (org-canvas-conflict-strategy 'push))
      (puthash "61" "2026-08-25T00:00:00Z" map)
      (let ((run (test-dry-run-84--run test-dry-run-84--stamped
                                        (list :title "Lab 1" :canvas-id "61") map nil)))
        (expect (car (cdr run)) :to-match "org-canvas-conflict-strategy is push"))))

  (it "still counts an entry Canvas has not touched as an update"
    (let ((map (make-hash-table :test 'equal)))
      (puthash "61" "2026-08-01T00:00:00Z" map)
      (let ((run (test-dry-run-84--run test-dry-run-84--stamped
                                        (list :title "Lab 1" :canvas-id "61") map nil)))
        (expect (plist-get (car run) :dry-run) :to-equal 1)
        (expect (plist-get (car run) :dry-run-conflict) :to-equal 0)
        (expect (car (cdr run)) :to-equal "[DRY-RUN] Would UPDATE 'Lab 1'"))))

  (it "counts a create whose title Canvas already holds as a conflict (issue #85)"
    (let ((titles (make-hash-table :test 'equal))
          (org-canvas-duplicate-title-strategy nil) (noninteractive nil))
      (puthash "R11" '(((id . 2563810) (name . "R11"))) titles)
      (let ((run (test-dry-run-84--run "* R11\n" (list :title "R11" :canvas-id nil)
                                        nil titles)))
        (expect (plist-get (car run) :dry-run-conflict) :to-equal 1)
        (expect (car (cdr run))
                :to-equal "[DRY-RUN] Would CONFLICT 'R11' (title already on Canvas as id 2563810; a real sync would ask)"))))

  (it "names a page holder by url and lists several holders"
    (let ((titles (make-hash-table :test 'equal))
          (org-canvas-duplicate-title-strategy 'adopt))
      (puthash "Welcome" '(((url . "welcome")) ((url . "welcome-2"))) titles)
      (let ((run (test-dry-run-84--run "* Welcome\n" (list :title "Welcome" :canvas-id nil)
                                        nil titles)))
        (expect (car (cdr run))
                :to-match "as id welcome, welcome-2; org-canvas-duplicate-title-strategy is adopt"))))

  (it "reports a plain create as a create"
    (let ((run (test-dry-run-84--run "* R11\n" (list :title "R11" :canvas-id nil)
                                      nil (make-hash-table :test 'equal))))
      (expect (plist-get (car run) :dry-run) :to-equal 1)
      (expect (car (cdr run)) :to-equal "[DRY-RUN] Would CREATE 'R11'"))))

(describe "org-canvas--sync-log-summary dry-run conflicts (issue #84)"
  (defun test-summary-84--run (counters)
    "Log a summary for COUNTERS.  Returns (LOG-LINES . MESSAGES)."
    (let ((temp-file (make-temp-file "summary-test" nil ".org"))
          (logged nil) (msgs nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file (insert "* Item\n"))
            (cl-letf (((symbol-function 'org-canvas--log-info)
                       (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
              (org-canvas--sync-log-summary "assignments" temp-file counters))
            (cons (nreverse logged) (nreverse msgs)))
        (let ((buf (find-buffer-visiting temp-file))) (when buf (kill-buffer buf)))
        (delete-file temp-file))))

  (it "reports would-conflict apart from would-sync"
    (let ((run (test-summary-84--run
                '(:success 0 :skip 39 :fail 0 :dry-run 27 :dry-run-conflict 10))))
      (expect (car run) :to-contain "Would sync: 27 | Would conflict: 10 | Skipped: 39")
      (expect (cdr run)
              :to-equal '("Assignments dry-run: 27 would sync, 10 would conflict, 39 skipped."))))

  (it "stays in dry-run form when every pending entry would conflict"
    (let ((run (test-summary-84--run
                '(:success 0 :skip 5 :fail 0 :dry-run 0 :dry-run-conflict 3))))
      (expect (car (cdr run)) :to-equal "Assignments dry-run: 0 would sync, 3 would conflict, 5 skipped."))))

(describe "org-canvas--sync-summary-columns dry-run conflicts (issue #84)"
  (it "adds a Conflicts column to the dry-run table when some entries would"
    (let ((org-canvas--dry-run nil))
      (expect (org-canvas--sync-summary-columns
               '((:dry-run 27 :dry-run-conflict 10 :skip 39)))
              :to-equal '(("Would sync" . :dry-run) ("Conflicts" . :dry-run-conflict)
                          ("Skipped" . :skip) ("Failed" . :fail)
                          ("Deferred" . :deferred)))))

  (it "treats stats with only would-conflict entries as a dry run"
    (let ((org-canvas--dry-run nil))
      (expect (org-canvas--sync-stats-dry-run-p '((:dry-run 0 :dry-run-conflict 2)))
              :to-be-truthy)
      (expect (org-canvas--sync-stats-dry-run-p '((:success 2))) :to-be nil)))

  (it "treats any stats as a dry run while one is running"
    (let ((org-canvas--dry-run t))
      (expect (org-canvas--sync-stats-dry-run-p '((:success 0))) :to-be-truthy)))

  (it "carries the count into the global table"
    (let ((org-canvas--dry-run nil)
          (org-canvas--sync-global-feature-stats nil)
          (org-canvas--sync-global-counters (list :success 0))
          (logged nil))
      (org-canvas--sync-record-feature-stats "Assignments"
                                             '(:dry-run 27 :dry-run-conflict 10 :skip 39))
      (expect (plist-get org-canvas--sync-global-counters :dry-run-conflict) :to-equal 10)
      (cl-letf (((symbol-function 'org-canvas--log-info)
                 (lambda (_logger fmt &rest args)
                   (push (apply #'format fmt args) logged)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--sync-log-global-summary))
      (let ((lines (nreverse logged)))
        (expect (nth 0 lines) :to-match "DRY RUN")
        (expect (nth 1 lines) :to-match "Would sync +Conflicts +Skipped")
        (expect (nth 2 lines) :to-match "Assignments +27 +10 +39")))))

(describe "org-canvas--sync-execute-pipeline duplicate outcome (issue #85)"
  (it "counts a duplicate-title skip among the skipped and names it"
    (with-temp-org-buffer "* R11\n"
      (org-back-to-heading)
      (let* ((counters (list :success 0 :skip 0 :fail 0 :conflict 0))
             (ctx (list :push-fn (lambda (_d _p &optional _ctx) 'duplicate)
                        :feature-name "assignments" :total-count 1
                        :counters counters :synced-ids (list nil)))
             (msgs nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
          (org-canvas--sync-execute-pipeline
           (list :title "R11" :canvas-id nil) '((name . "R11")) ctx))
        (expect (plist-get counters :skip) :to-equal 1)
        (expect (plist-get counters :success) :to-equal 0)
        (expect (car (plist-get counters :skipped-titles))
                :to-equal "R11 (already on Canvas; adopt it with org-canvas-adopt-at-point or rename)")
        (expect (car msgs) :to-equal "Assignments [1/1] SKIPPED: 'R11' (title already on Canvas)")))))

(describe "org-canvas--sync-run-pipeline title index (issue #85)"
  (defun test-titles-85--run (snapshot)
    "Push one unstamped heading through the pipeline with SNAPSHOT.
Returns the :remote-titles of the run context the push received."
    (let ((file (make-temp-file "titles-" nil ".org"))
          (seen 'unset))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* R11\n"))
            (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                      ((symbol-function 'org-canvas--sync-log-summary) #'ignore)
                      ((symbol-function 'org-canvas--sync-fetch-remote-snapshot)
                       (lambda (&rest _) snapshot))
                      ((symbol-function 'org-canvas--sync-warn-orphans) #'ignore)
                      ((symbol-function 'org-canvas--save-buffer) #'ignore))
		     (org-canvas--sync-run-pipeline (list
						     :feature "assignments" :file file :query "LEVEL=1"
						     :parse (lambda () (list :title "R11" :canvas-id nil :pom (point-marker)))
						     :build (lambda (_data) '((name . "R11")))
						     :push (lambda (_data _payload &optional ctx)
							     (setq seen (plist-get ctx :remote-titles))
							     '((id . 1)))
						     :finalize #'ignore)))
            seen)
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "binds the snapshot's titles while entries push"
    (let ((titles (make-hash-table :test 'equal)))
      (expect (test-titles-85--run (list :updated (make-hash-table :test 'equal)
                                         :titles titles))
              :to-be titles)))

  (it "marks the run as unchecked when there is no snapshot"
    (expect (test-titles-85--run nil) :to-equal 'none)))

(describe "org-canvas--sync-execute-pipeline skip path backfills the baseline (issue #104)"
  (it "records CANVAS_UPDATED_AT for an unchanged legacy entry"
    (with-temp-org-buffer
     "* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
     (org-back-to-heading t)
     (let* ((payload '((name . "Lab 1")))
            (map (make-hash-table :test 'equal))
            (counters (list :success 0 :skip 0 :fail 0))
            (ctx (list :push-fn (lambda (&rest _) (error "Must not push"))
                       :feature-name "assignments"
                       :total-count 1
                       :counters counters
                       :synced-ids (list nil)
                       :remote-updated map
                       :baseline (encode-time
                                  (org-parse-time-string "[2026-08-19 Wed 12:00]")))))
       (puthash "61" "2026-08-01T00:00:00Z" map)
       (org-entry-put (point) "PAYLOAD_HASH" (md5 (json-encode payload)))
       (org-canvas--sync-execute-pipeline
        (list :title "Lab 1" :canvas-id "61" :pom (point-marker)) payload ctx)
       (expect (plist-get counters :skip) :to-equal 1)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-08-01T00:00:00Z")))))

(describe "org-canvas--sync-check-spec"
  (it "accepts a spec made only of known keys holding the required ones"
    (expect (org-canvas--sync-check-spec
             (list :feature "pages" :parse #'ignore :hash-extra nil)
             '(:feature :parse))
            :not :to-throw))

  (it "names an unknown key, so a misspelt option cannot vanish"
    (expect (org-canvas--sync-check-spec (list :feature "pages" :parser #'ignore)
                                         '(:feature))
            :to-throw 'error))

  (it "names a missing required key"
    (expect (org-canvas--sync-check-spec (list :feature "pages") '(:feature :parse))
            :to-throw 'error)))

(describe "org-canvas-define-sync builds the spec its runners read"
  (it "runs the pipeline with the module's options under their own names"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas--sync-run-pipeline)
                 (lambda (spec) (setq seen spec) nil)))
        (org-canvas-sync-announcements))
      (expect (plist-get seen :feature) :to-equal "announcements")
      (expect (plist-get seen :query) :to-equal "LEVEL=1")
      (expect (plist-get seen :parse) :to-be #'org-canvas--announcement-parse-entry)
      (expect (plist-get seen :pull-item-fn) :to-be #'org-canvas--announcement-pull-item)
      (expect (functionp (plist-get seen :push)) :to-be t)
      (expect (functionp (plist-get seen :finalize)) :to-be t)
      (expect (org-canvas--sync-check-spec seen nil) :not :to-throw))))

(describe "sync spec :prepare and :first"
  (it "runs :prepare once before the first entry and keeps its result in the context"
    (let ((file (make-temp-file "prepare-" nil ".org"))
          (seen nil) (prepared 0))
      (unwind-protect
          (with-org-canvas-test-config
            (with-temp-file file
              (insert "* One\n:PROPERTIES:\n:END:\n* Two\n:PROPERTIES:\n:END:\n"))
            (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore))
              (org-canvas--sync-run-pipeline
               (list :feature "things" :file file :query "LEVEL=1"
                     :parse (lambda () (list :title (org-get-heading t t t t)
                                             :canvas-id "1" :pom (point-marker)))
                     :build (lambda (_data) '((name . "x")))
                     :push (lambda (_data _payload &optional ctx)
                             (push (plist-get ctx :prepared) seen)
                             '((id . 1)))
                     :finalize (lambda (&rest _) nil)
                     :prepare (lambda (ctx)
                                (cl-incf prepared)
                                (format "root-of-%s" (plist-get ctx :feature-name))))))
            (expect prepared :to-equal 1)
            (expect seen :to-equal '("root-of-things" "root-of-things")))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "stops the run before any push when :prepare signals"
    (let ((file (make-temp-file "prepare-" nil ".org"))
          (pushed nil))
      (unwind-protect
          (with-org-canvas-test-config
            (with-temp-file file (insert "* One\n"))
            (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore))
              (expect (org-canvas--sync-run-pipeline
                       (list :feature "things" :file file :query "LEVEL=1"
                             :parse (lambda () (list :title "One" :pom (point-marker)))
                             :build (lambda (_data) nil)
                             :push (lambda (&rest _) (setq pushed t))
                             :finalize #'ignore
                             :prepare (lambda (_ctx) (error "No root"))))
                      :to-throw 'error))
            (expect pushed :to-be nil))
        (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
        (delete-file file))))

  (it "runs :prepare before the parse of a push at point"
    (with-temp-org-buffer "* One\n"
      (org-back-to-heading)
      (let ((order nil))
        (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
          (org-canvas--push-at-point-runtime
           (list :feature "thing"
                 :parse (lambda () (push 'parse order) (list :title "One" :canvas-id "1" :pom (point)))
                 :build (lambda (_data) '((name . "x")))
                 :push (lambda (_data _payload &optional ctx)
                         (push (plist-get ctx :prepared) order) '((id . 1)))
                 :finalize (lambda (&rest _) nil)
                 :prepare (lambda (_ctx) (push 'prepare order) 'root))))
        (expect (nreverse order) :to-equal '(prepare parse root)))))

  (it "makes the generated sync run its :first command beforehand, keeping the log"
    (let ((order nil))
      (cl-letf (((symbol-function 'org-canvas--sync-run-pipeline)
                 (lambda (spec)
                   (push (list (plist-get spec :feature) org-canvas--inhibit-log-clear) order)
                   nil))
                ((symbol-function 'org-canvas-sync-outcome-groups)
                 (lambda () (push (list "outcome-groups" org-canvas--inhibit-log-clear) order))))
        (org-canvas-sync-outcomes))
      (expect (nreverse order)
              :to-equal '(("outcome-groups" t) ("outcomes" t))))))

(describe "sync spec :hash and :dry-run, and the push results they allow"
  (defmacro test-sync-hash--with-file (content &rest body)
    "Run BODY with FILE bound to a temp Org file holding CONTENT, in test config."
    (declare (indent 1))
    `(let ((file (make-temp-file "hash-" nil ".org")))
       (unwind-protect
           (with-org-canvas-test-config
             (with-temp-file file (insert ,content))
             (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore))
               ,@body))
         (let ((buf (find-buffer-visiting file)))
           (when buf (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
         (delete-file file))))

  (defun test-sync-hash--spec (file &rest keys)
    "A minimal sync spec over FILE whose push records a call, plus KEYS."
    (append (list :feature "things" :file file :query "LEVEL=1"
                  :parse (lambda () (list :title (org-get-heading t t t t)
                                          :canvas-id (org-entry-get (point) "CANVAS_ID")
                                          :pom (point-marker)))
                  :build (lambda (_data) '((name . "x")))
                  :finalize (lambda (&rest _) nil))
            keys))

  (it "skips on a stored hash the :hash function reproduces, and stamps its value"
    (test-sync-hash--with-file "* Same\n:PROPERTIES:\n:CANVAS_ID: 1\n:PAYLOAD_HASH: content-1\n:END:\n* Fresh\n:PROPERTIES:\n:CANVAS_ID: 2\n:END:\n"
      (let ((pushed nil))
        (org-canvas--sync-run-pipeline
         (test-sync-hash--spec file
                               :push (lambda (data _payload &optional _ctx)
                                       (push (plist-get data :title) pushed) '((id . 2)))
                               :hash (lambda (_payload data)
                                       (format "content-%s" (plist-get data :canvas-id)))))
        (expect pushed :to-equal '("Fresh"))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-max))
          (org-back-to-heading t)
          (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal "content-2")))))

  (it "never skips and never stamps when the push owns the hash"
    (test-sync-hash--with-file "* Same\n:PROPERTIES:\n:CANVAS_ID: 1\n:PAYLOAD_HASH: mine\n:END:\n"
      (let ((pushed 0))
        (org-canvas--sync-run-pipeline
         (test-sync-hash--spec file
                               :push (lambda (&rest _) (cl-incf pushed) t)
                               :hash 'push))
        (expect pushed :to-equal 1)
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (org-back-to-heading t)
          (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal "mine")))))

  (it "counts a push that answers skip as a skip, without finalizing"
    (test-sync-hash--with-file "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let ((ctx (org-canvas--sync-run-pipeline
                  (test-sync-hash--spec file
                                        :push (lambda (&rest _) 'skip)
                                        :finalize (lambda (&rest _) (error "must not finalize"))
                                        :hash 'push))))
        (expect (plist-get (plist-get ctx :counters) :skip) :to-equal 1)
        (expect (plist-get (plist-get ctx :counters) :success) :to-equal 0))))

  (it "lets a push preview a dry run itself when :dry-run is push, and counts the sentinel"
    (test-sync-hash--with-file "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let* ((org-canvas--dry-run t)
             (called nil)
             (ctx (org-canvas--sync-run-pipeline
                   (test-sync-hash--spec file
                                         :push (lambda (&rest _) (setq called t)
                                                 org-canvas--dry-run-response)
                                         :dry-run 'push))))
        (expect called :to-be t)
        (expect (plist-get (plist-get ctx :counters) :dry-run) :to-equal 1))))

  (it "reports from the snapshot under a dry run when the push does not preview"
    (test-sync-hash--with-file "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let* ((org-canvas--dry-run t)
             (called nil)
             (ctx (org-canvas--sync-run-pipeline
                   (test-sync-hash--spec file
                                         :push (lambda (&rest _) (setq called t) t)))))
        (expect called :to-be nil)
        (expect (plist-get (plist-get ctx :counters) :dry-run) :to-equal 1))))

  (it "counts a heading the parser declines as a skip, not a failure"
    (test-sync-hash--with-file "* Folder\n* File\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (let* ((pushed nil)
             (spec (test-sync-hash--spec file :push (lambda (data &rest _)
                                                      (push (plist-get data :title) pushed) t)
                                         :hash 'push))
             (ctx (progn
                    (plist-put spec :parse
                               (lambda () (unless (string= (org-get-heading t t t t) "Folder")
                                            (list :title (org-get-heading t t t t)
                                                  :canvas-id "1" :pom (point-marker)))))
                    (org-canvas--sync-run-pipeline spec))))
        (expect pushed :to-equal '("File"))
        (expect (plist-get (plist-get ctx :counters) :skip) :to-equal 1)
        (expect (plist-get (plist-get ctx :counters) :fail) :to-equal 0)
        (expect (plist-get (plist-get ctx :counters) :success) :to-equal 1))))

  (it "rejects a hash mode the spec keys do not know"
    (expect (org-canvas--sync-check-spec (list :feature "x" :hashing 'push) '(:feature))
            :to-throw 'error)))

(describe "org-canvas--sync-collect-entries scopes ids to the query (issue #196)"
  (defmacro test-196--with-file (content &rest body)
    "Run BODY with FILE bound to a temp Org file holding CONTENT."
    (declare (indent 1))
    `(let ((file (make-temp-file "orphan-" nil ".org")))
       (unwind-protect
           (progn (with-temp-file file (insert ,content))
                  (cl-letf (((symbol-function 'org-canvas--log-info) #'ignore))
                    ,@body))
         (let ((buf (find-buffer-visiting file))) (when buf (kill-buffer buf)))
         (delete-file file))))

  (it "collects only the ids of the headings the query selects"
    (test-196--with-file "* Module\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** Item\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n* Stale\n:PROPERTIES:\n:CANVAS_ID: 2\n:END:\n"
      (let ((entries (org-canvas--sync-collect-entries file "LEVEL=1" "modules")))
        (expect (plist-get entries :all-ids-before) :to-equal '("1" "2"))
        (expect (length (plist-get entries :targets)) :to-equal 2)
        (dolist (m (plist-get entries :targets)) (set-marker m nil)))))

  (it "reads the other level when the query names it"
    (test-196--with-file "* Group\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** Outcome\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
      (let ((entries (org-canvas--sync-collect-entries file "LEVEL=2" "outcomes")))
        (expect (plist-get entries :all-ids-before) :to-equal '("55"))
        (dolist (m (plist-get entries :targets)) (set-marker m nil)))))

  (it "keeps a heading without an id out of the list but among the targets"
    (test-196--with-file "* New\n* Stamped\n:PROPERTIES:\n:CANVAS_ID: 9\n:END:\n"
      (let ((entries (org-canvas--sync-collect-entries file "LEVEL=1" "pages")))
        (expect (plist-get entries :all-ids-before) :to-equal '("9"))
        (expect (length (plist-get entries :targets)) :to-equal 2)
        (dolist (m (plist-get entries :targets)) (set-marker m nil)))))

  (it "does not warn about a stamped child of a synced parent, but still about an unreached parent"
    (test-196--with-file "* Module\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n** Item\n:PROPERTIES:\n:CANVAS_ID: 55\n:END:\n"
      (let ((warned nil))
        (with-org-canvas-test-config
          (cl-letf (((symbol-function 'org-canvas--sync-fetch-remote-snapshot) #'ignore)
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) warned))))
            (org-canvas--sync-run-pipeline
             (list :feature "modules" :file file :query "LEVEL=1"
                   :parse (lambda () (list :title (org-get-heading t t t t)
                                           :canvas-id (org-entry-get (point) "CANVAS_ID")
                                           :pom (point-marker)))
                   :build (lambda (_data) '((name . "x")))
                   :push (lambda (&rest _) '((id . 1)))
                   :finalize (lambda (&rest _) nil)))))
        (expect (cl-some (lambda (w) (string-match-p "\\[Orphan\\]" w)) warned) :to-be nil)))))

(describe "org-canvas--graphql-mutate (issue #202)"
  (it "sends nothing under a dry run and answers the sentinel"
    (let ((org-canvas--dry-run t) (sent nil) (logged nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-send) (lambda (&rest _) (setq sent t)))
                ((symbol-function 'org-canvas--log-info)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
        (expect (org-canvas--dry-run-response-p
                 (org-canvas--graphql-mutate "set the course post policy to manual" "mutation { x }"))
                :to-be-truthy))
      (expect sent :to-be nil)
      (expect (car logged) :to-match "\\[DRY-RUN\\] Would set the course post policy to manual")))

  (it "sends the mutation with its variables otherwise"
    (let ((seen nil))
      (cl-letf (((symbol-function 'org-canvas--graphql-send)
                 (lambda (doc vars) (setq seen (list doc vars)) '((ok . t))))
                ((symbol-function 'org-canvas--log-info) #'ignore))
        (expect (org-canvas--graphql-mutate "do x" "mutation { x }" '((a . 1))) :to-equal '((ok . t)))
        (expect seen :to-equal '("mutation { x }" ((a . 1))))))))

;;;; Headings Canvas deleted (issue #392)

(describe "a push of a heading a pull marked deleted on Canvas (issue #392)"
  (it "is skipped by a full sync, named among the skipped, never parsed or sent"
    (with-temp-org-buffer
     "* Sprint 0
:PROPERTIES:
:CANVAS_ID: 100
:CANVAS_DELETED: [2026-09-28 Mon 10:00]
:END:
"
     (org-back-to-heading)
     (let* ((marker (point-marker))
            (parsed nil)
            (logged nil)
            (counters (list :success 0 :skip 0 :fail 0 :conflict 0))
            (ctx (list :parse-fn (lambda () (setq parsed t) nil)
                       :build-fn #'ignore :push-fn #'ignore :finalize-fn #'ignore
                       :feature-name "assignments" :feature-upper "ASSIGNMENTS"
                       :total-count 1 :counters counters :synced-ids (list nil))))
       (cl-letf (((symbol-function 'org-canvas--log-warning)
                  (lambda (_logger fmt &rest args)
                    (push (apply #'format fmt args) logged)))
                 ((symbol-function 'message) #'ignore))
         (org-canvas--sync-process-entry marker ctx))
       (expect parsed :to-be nil)
       (expect (plist-get counters :skip) :to-equal 1)
       (expect (plist-get counters :skipped-titles)
               :to-equal '("Sprint 0 (deleted on Canvas)"))
       (expect (car logged) :to-match "deleted on Canvas (CANVAS_DELETED \\[2026-09-28"))))

  (it "is refused at point with the way out"
    (with-temp-org-buffer
     "* Sprint 0
:PROPERTIES:
:CANVAS_ID: 100
:CANVAS_DELETED: [2026-09-28 Mon 10:00]
:END:
"
     (org-back-to-heading)
     (let ((err (condition-case e
                    (org-canvas--push-at-point-runtime
                     (list :feature "assignment" :parse #'ignore :build #'ignore
                           :push #'ignore :finalize #'ignore))
                  (user-error e))))
       (expect (car err) :to-be 'user-error)
       (expect (cadr err) :to-match "remove CANVAS_DELETED and its CANVAS_ID")))))

;;;; Headings under a heading Canvas deleted, and pull at point (issue #399)

(describe "a heading under one a pull marked deleted (issue #399)"
  (it "is skipped by a full sync with a note naming the heading above"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 100
:CANVAS_DELETED: [2026-09-28 Mon 10:00]
:END:
** Outcome
:PROPERTIES:
:CANVAS_ID: 400
:END:
"
     (goto-char (point-min))
     (re-search-forward "^\\*\\* Outcome")
     (org-back-to-heading t)
     (let* ((marker (point-marker))
            (parsed nil)
            (logged nil)
            (counters (list :success 0 :skip 0 :fail 0 :conflict 0))
            (ctx (list :parse-fn (lambda () (setq parsed t) nil)
                       :build-fn #'ignore :push-fn #'ignore :finalize-fn #'ignore
                       :feature-name "outcomes" :feature-upper "OUTCOMES"
                       :total-count 1 :counters counters :synced-ids (list nil))))
       (cl-letf (((symbol-function 'org-canvas--log-warning)
                  (lambda (_logger fmt &rest args)
                    (push (apply #'format fmt args) logged)))
                 ((symbol-function 'message) #'ignore))
         (org-canvas--sync-process-entry marker ctx))
       (expect parsed :to-be nil)
       (expect (plist-get counters :skip) :to-equal 1)
       (expect (car logged)
               :to-match "'Outcome' sits under a heading deleted on Canvas (CANVAS_DELETED \\[2026-09-28"))))

  (it "leaves a heading with no mark above it alone"
    (with-temp-org-buffer "* Group\n** Outcome\n"
      (goto-char (point-max))
      (org-back-to-heading t)
      (expect (org-canvas--sync-deleted-note "Outcome") :to-be nil))))

(provide 'org-canvas-core-sync-test)
;;; org-canvas-core-sync-test.el ends here
