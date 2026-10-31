;;; org-canvas-core-heading-test.el --- Buttercup tests for the at-point and by-heading commands  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The runtime every generated at-point command shares, and the batch
;; verbs that name one heading: sync, pull and delete by heading.

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

(describe "org-canvas--push-at-point-runtime"
  (it "hands the pull-item-fn to the push through the run context"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test Page
:PROPERTIES:
:CANVAS_ID: 99
:END:
"
         (org-back-to-heading)
         (let ((captured-pull-fn nil))
           (cl-letf (((symbol-function 'display-buffer) #'ignore))
		    (org-canvas--push-at-point-runtime (list
							:feature "test"
							:parse (lambda () (list :title "Test" :canvas-id "99" :pom (point)))
							:build (lambda (_data) '((title . "Test")))
							:push (lambda (_data _payload &optional ctx)
								(setq captured-pull-fn (plist-get ctx :pull-item-fn))
								'((id . 99)))
							:finalize (lambda (_data _response &optional _ctx) nil)
							:title-key :title
							:pull-item-fn #'my-pull-fn)))
           (expect captured-pull-fn :to-equal #'my-pull-fn)))))))

(describe "org-canvas--push-at-point-runtime"
  (it "skips sync when payload hash matches"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Test Page
:PROPERTIES:
:CANVAS_ID: 100
:END:

Body.
"
       (org-back-to-heading)
       (let* ((payload '((title . "Test Page") (body . "Body.")))
              (payload-hash (md5 (json-encode payload)))
              (api-called nil))
         ;; Set the stored hash to match
         (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
         (save-buffer)
         (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                   ((symbol-function 'display-buffer) #'ignore))
		  (org-canvas--push-at-point-runtime (list
						      :feature "page"
						      :parse (lambda () (list :title "Test Page" :canvas-id "100" :pom (point)))
						      :build (lambda (_data) payload)
						      :push (lambda (_data _payload &optional _ctx) (setq api-called t) '((id . 100)))
						      :finalize (lambda (_data _response &optional _ctx) nil)
						      :title-key :title)))
         ;; API should NOT have been called (skipped)
         (expect api-called :to-be nil))))))

(describe "org-canvas--push-at-point-runtime"
  (it "uses canvas-url for skip detection when canvas-id absent"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* Test Page
:PROPERTIES:
:CANVAS_URL: my-page-slug
:END:

Body.
"
       (org-back-to-heading)
       (let* ((payload '((wiki_page (title . "Test Page") (body . "Body."))))
              (payload-hash (md5 (json-encode payload)))
              (api-called nil))
         ;; Set stored hash to match — should skip
         (org-entry-put (point) "PAYLOAD_HASH" payload-hash)
         (save-buffer)
         (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                   ((symbol-function 'display-buffer) #'ignore))
		  (org-canvas--push-at-point-runtime (list
						      :feature "page"
						      :parse (lambda () (list :title "Test Page" :canvas-url "my-page-slug" :pom (point)))
						      :build (lambda (_data) payload)
						      :push (lambda (_data _payload &optional _ctx) (setq api-called t) '((url . "my-page-slug")))
						      :finalize (lambda (_data _response &optional _ctx) nil)
						      :title-key :title)))
         ;; Should be skipped because hash matches AND canvas-url is truthy
         (expect api-called :to-be nil))))))

(describe "org-canvas--push-at-point-runtime stage logging"
  (it "logs stage markers during sync"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:END:

Content here.
"
     (org-back-to-heading)
     (spy-on 'org-canvas--log-info)
     (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil))
               ((symbol-function 'save-buffer) (lambda () nil)))
	      (org-canvas--push-at-point-runtime (list
						  :feature "pages"
						  :parse (lambda () (list :title "Test Page" :canvas-id nil :pom (point-marker)))
						  :build (lambda (_) '((title . "Test Page")))
						  :push (lambda (_data _payload &optional _ctx) '((url . "test-page")))
						  :finalize (lambda (_data _response &optional _ctx) nil)
						  :title-key :title)))
     (let ((found-sync-at-point nil)
           (found-stage-2 nil)
           (found-stage-3 nil)
           (found-stage-4 nil))
       (dolist (call (spy-calls-all-args 'org-canvas--log-info))
         (when (>= (length call) 2)
           (let ((fmt (nth 1 call)))
             (when (stringp fmt)
               (when (string-match-p "SYNC-AT-POINT" fmt) (setq found-sync-at-point t))
               (when (string-match-p "Stage 2" fmt) (setq found-stage-2 t))
               (when (string-match-p "Stage 3" fmt) (setq found-stage-3 t))
               (when (string-match-p "Stage 4" fmt) (setq found-stage-4 t))))))
       (expect found-sync-at-point :to-be-truthy)
       (expect found-stage-2 :to-be-truthy)
       (expect found-stage-3 :to-be-truthy)
       (expect found-stage-4 :to-be-truthy))))

  (it "logs skip without push/finalize stages when unchanged"
    (with-temp-org-buffer
     "* Test Page
:PROPERTIES:
:CANVAS_URL: existing-page
:PAYLOAD_HASH: placeholder
:END:

Content here.
"
     (org-back-to-heading)
     (let* ((data (list :title "Test Page" :canvas-url "existing-page" :pom (point-marker)))
            (payload '((title . "Test Page")))
            (hash (md5 (json-encode payload))))
       ;; Set the stored hash to match
       (org-entry-put (point) "PAYLOAD_HASH" hash)
       (save-buffer)
       (spy-on 'org-canvas--log-info)
       (cl-letf (((symbol-function 'display-buffer) (lambda (_) nil)))
		(org-canvas--push-at-point-runtime (list
						    :feature "pages"
						    :parse (lambda () data)
						    :build (lambda (_) payload)
						    :push (lambda (_data _payload &optional _ctx) (error "Should not be called"))
						    :finalize (lambda (_data _response &optional _ctx) (error "Should not be called"))
						    :title-key :title)))
       (let ((found-skip nil)
             (found-stage-3 nil))
         (dolist (call (spy-calls-all-args 'org-canvas--log-info))
           (when (>= (length call) 2)
             (let ((fmt (nth 1 call)))
               (when (stringp fmt)
                 (when (string-match-p "Skip" fmt) (setq found-skip t))
                 (when (string-match-p "Stage 3" fmt) (setq found-stage-3 t))))))
         (expect found-skip :to-be-truthy)
         (expect found-stage-3 :to-be nil))))))

(describe "org-canvas--push-at-point-runtime stopped push (issues #85, #86)"
  (it "reports a duplicate instead of finalizing a symbol"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* R11\n"
        (org-back-to-heading)
        (let ((finalized nil) (msgs nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
		   (org-canvas--push-at-point-runtime (list
						       :feature "assignment"
						       :parse (lambda () (list :title "R11" :canvas-id nil :pom (point)))
						       :build (lambda (_data) '((name . "R11")))
						       :push (lambda (_data _payload &optional _ctx) 'duplicate)
						       :finalize (lambda (_data _response &optional _ctx) (setq finalized t))
						       :title-key :title)))
          (expect finalized :to-be nil)
          (expect (car msgs)
                  :to-equal "Assignment 'R11' not pushed — Canvas already holds this title; adopt it with M-x org-canvas-adopt-at-point or rename.")))))

  (it "words a conflict and a pull"
    (let ((msgs nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) msgs)))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (org-canvas--push-at-point-report-stop "page" "Welcome" 'conflict)
        (org-canvas--push-at-point-report-stop "page" "Welcome" 'pulled))
      (expect (nth 1 msgs) :to-match "remote item was modified")
      (expect (nth 0 msgs) :to-match "remote version was pulled"))))

(describe "org-canvas--push-at-point-runtime refreshes #+LAST_SYNCED (issue #104)"
  (it "writes the file header from the stamp finalize recorded"
    ;; assignments.org read [2026-08-19] after sixty at-point pushes:
    ;; only the full-sync path ever touched the header.
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (search-forward "* Lab 1")
       (org-back-to-heading t)
       (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                 ((symbol-function 'display-buffer) #'ignore))
		(org-canvas--push-at-point-runtime (list
						    :feature "assignment"
						    :parse (lambda () (list :title "Lab 1" :canvas-id "61" :pom (point-marker)))
						    :build (lambda (_data) '((name . "Lab 1")))
						    :push (lambda (_data _payload &optional _ctx)
							    '((id . 61) (updated_at . "2026-09-01T14:10:30Z")))
						    :finalize (lambda (data response &optional ctx) (org-canvas--finalize-item data response
															       (org-canvas--finalize-options :ctx ctx)))
						    :title-key :title)))
       (expect (org-canvas--pull-read-file-header)
               :to-equal (format-time-string
                          "[%Y-%m-%d %a %H:%M]"
                          (time-add (date-to-time "2026-09-01T14:10:30Z") 60))))))

  (it "leaves the header alone when the push stops at a conflict"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-08-19 Wed 09:59]
* Lab 1
:PROPERTIES:
:CANVAS_ID: 61
:END:
"
       (search-forward "* Lab 1")
       (org-back-to-heading t)
       (cl-letf (((symbol-function 'org-canvas-clear-log) #'ignore)
                 ((symbol-function 'display-buffer) #'ignore))
		(org-canvas--push-at-point-runtime (list
						    :feature "assignment"
						    :parse (lambda () (list :title "Lab 1" :canvas-id "61" :pom (point-marker)))
						    :build (lambda (_data) '((name . "Lab 1")))
						    :push (lambda (_data _payload &optional _ctx) 'conflict)
						    :finalize (lambda (data response &optional ctx) (org-canvas--finalize-item data response
															       (org-canvas--finalize-options :ctx ctx)))
						    :title-key :title)))
       (expect (org-canvas--pull-read-file-header)
               :to-equal "[2026-08-19 Wed 09:59]")))))

(describe "push at point with a push-owned hash"
  (it "calls the push despite a matching stored hash, stamps nothing, and returns the context"
    (with-temp-org-buffer "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:PAYLOAD_HASH: mine\n:END:\n"
      (org-back-to-heading)
      (let ((pushed nil) ctx)
        (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
          (setq ctx (org-canvas--push-at-point-runtime
                     (list :feature "thing"
                           :parse (lambda () (list :title "One" :canvas-id "1" :pom (point)))
                           :build (lambda (_data) '((name . "x")))
                           :push (lambda (_data _payload &optional ctx)
                                   (setq pushed t)
                                   (org-canvas--ctx-push ctx :file-changed-ids "One")
                                   t)
                           :finalize (lambda (&rest _) nil)
                           :hash 'push))))
        (expect pushed :to-be t)
        (expect (org-entry-get (point) "PAYLOAD_HASH") :to-equal "mine")
        (expect (plist-get ctx :file-changed-ids) :to-equal '("One")))))

  (it "reports a push that answers skip as unchanged"
    (with-temp-org-buffer "* One\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
      (org-back-to-heading)
      (let ((said nil))
        (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (org-canvas--push-at-point-runtime
           (list :feature "thing"
                 :parse (lambda () (list :title "One" :canvas-id "1" :pom (point)))
                 :build (lambda (_data) nil)
                 :push (lambda (&rest _) 'skip)
                 :finalize (lambda (&rest _) (error "must not finalize"))
                 :hash 'push)))
        (expect said :to-match "unchanged")))))

;;;; Sync by heading (issue #287)

(defconst test-org-canvas-heading-file-content
  "#+TITLE: Pages
* Week 05 objectives
:PROPERTIES:
:CANVAS_ID: 501
:END:
Body.
* Week 06 objectives
:PROPERTIES:
:CANVAS_ID: 601
:END:
* Twice
* Twice
* [[file:content/syllabus.pdf][Syllabus.pdf]]
** A child
"
  "A file with a stamped heading, an unstamped one, a repeated title and a link.")

(defun test-org-canvas-heading-spec (&rest overrides)
  "Return a sync spec whose stages record what they see, with OVERRIDES."
  (append overrides
          (list :feature "page" :query "LEVEL=1"
                :parse (lambda () (list :title (org-get-heading t t t t)
                                        :canvas-id (org-entry-get (point) "CANVAS_ID")
                                        :pom (point-marker)))
                :build (lambda (data) (list (cons 'title (plist-get data :title))))
                :push (lambda (_data _payload &optional _ctx) '((id . 777) (updated_at . "2026-09-17T10:00:00Z")))
                :finalize (lambda (data response &optional _ctx)
                            (org-canvas-org-save-sync-state (plist-get data :pom)
                                                            (alist-get 'id response)))
                :title-key :title)))

(describe "org-canvas--sync-find-heading"
  (it "finds a heading by its exact title"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (let ((m (org-canvas--sync-find-heading
                (org-canvas--heading-query
                 :file buffer-file-name :query "LEVEL=1"
                 :target "Week 06 objectives" :by nil
                 :id-property "CANVAS_ID" :feature "page"))))
        (expect (markerp m) :to-be-truthy)
        (expect (marker-buffer m) :to-be (current-buffer))
        (save-excursion (goto-char m)
                        (expect (org-entry-get (point) "CANVAS_ID") :to-equal "601")))))

  (it "finds a heading by its stamp, as a string or a number"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (dolist (target '("501" 501))
        (let ((m (org-canvas--sync-find-heading
                  (org-canvas--heading-query
                   :file buffer-file-name :query "LEVEL=1"
                   :target target :by 'canvas-id
                   :id-property "CANVAS_ID" :feature "page"))))
          (save-excursion (goto-char m)
                          (expect (org-get-heading t t t t) :to-equal "Week 05 objectives"))))))

  (it "does not let a prefix of a title match (R1 is not R10)"
    (with-temp-org-buffer "* R1\n* R10\n"
      (let ((m (org-canvas--sync-find-heading
                (org-canvas--heading-query
                 :file buffer-file-name :query "LEVEL=1" :target "R1" :by nil
                 :id-property "CANVAS_ID" :feature "x"))))
        (save-excursion (goto-char m) (expect (org-get-heading t t t t) :to-equal "R1")))))

  (it "matches a heading that is a link by its text as written"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (let ((m (org-canvas--sync-find-heading
                (org-canvas--heading-query
                 :file buffer-file-name :query "LEVEL=1"
                 :target "[[file:content/syllabus.pdf][Syllabus.pdf]]" :by nil
                 :id-property "CANVAS_ID" :feature "file"))))
        (expect (markerp m) :to-be-truthy))))

  (it "matches a files.org link heading by its description (issue #446)"
    (with-temp-org-buffer "* course\n** [[file:../syllabus.pdf][syllabus.pdf]]\n:PROPERTIES:\n:CANVAS_ID: 31861753\n:END:\n"
      (dolist (target '("syllabus.pdf" "[[file:../syllabus.pdf][syllabus.pdf]]"))
        (let ((m (org-canvas--sync-find-heading
                  (org-canvas--heading-query
                   :file buffer-file-name :query "LEVEL>0"
                   :target target :by nil
                   :id-property "CANVAS_ID" :feature "file"))))
          (save-excursion (goto-char m)
                          (expect (org-entry-get (point) "CANVAS_ID") :to-equal "31861753"))))))

  (it "matches a link heading by its description whether or not org-get-heading strips links"
    ;; Org 9.7 (Emacs 30) strips link markup in `org-get-heading'; Org 9.6
    ;; (Emacs 29) keeps it.  Both readings must find the heading.
    (with-temp-org-buffer "* [[file:../syllabus.pdf][syllabus.pdf]]\n"
      (dolist (reading '("[[file:../syllabus.pdf][syllabus.pdf]]" "syllabus.pdf"))
        (cl-letf (((symbol-function 'org-get-heading) (lambda (&rest _) reading)))
          (goto-char (point-min))
          (expect (org-canvas--sync-heading-matches-p "syllabus.pdf" nil "CANVAS_ID")
                  :to-be-truthy)
          (expect (org-canvas--sync-heading-matches-p
                   "[[file:../syllabus.pdf][syllabus.pdf]]" 'title "CANVAS_ID")
                  :to-be-truthy)
          (expect (org-canvas--sync-heading-matches-p "syllabus" nil "CANVAS_ID")
                  :not :to-be-truthy)))))

  (it "looks only at the headings the query selects"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (expect (org-canvas--sync-find-heading
               (org-canvas--heading-query
                :file buffer-file-name :query "LEVEL=1" :target "A child" :by nil
                :id-property "CANVAS_ID" :feature "page"))
              :to-throw 'user-error)
      (expect (markerp (org-canvas--sync-find-heading
                        (org-canvas--heading-query
                         :file buffer-file-name :query "LEVEL=2"
                         :target "A child" :by nil
                         :id-property "CANVAS_ID" :feature "page")))
              :to-be-truthy)))

  (it "is a user-error naming the file when nothing matches"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (condition-case err
          (progn (org-canvas--sync-find-heading
                  (org-canvas--heading-query
                   :file buffer-file-name :query "LEVEL=1"
                   :target "Week 07" :by nil
                   :id-property "CANVAS_ID" :feature "page"))
                 (expect nil :to-be-truthy))
        (user-error (expect (cadr err) :to-match "No page heading titled \"Week 07\" in org-test-")))))

  (it "is a user-error when two headings carry the title"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (condition-case err
          (progn (org-canvas--sync-find-heading
                  (org-canvas--heading-query
                   :file buffer-file-name :query "LEVEL=1"
                   :target "Twice" :by nil
                   :id-property "CANVAS_ID" :feature "page"))
                 (expect nil :to-be-truthy))
        (user-error (expect (cadr err) :to-match "2 page headings titled \"Twice\".*at point")))))

  (it "names the stamp in the error for a by-id lookup"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (condition-case err
          (progn (org-canvas--sync-find-heading
                  (org-canvas--heading-query
                   :file buffer-file-name :query "LEVEL=1"
                   :target "9" :by 'canvas-id
                   :id-property "CANVAS_URL" :feature "page"))
                 (expect nil :to-be-truthy))
        (user-error (expect (cadr err) :to-match "No page heading CANVAS_URL 9")))))

  (it "rejects a BY it does not know"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (expect (org-canvas--sync-find-heading
               (org-canvas--heading-query
                :file buffer-file-name :query "LEVEL=1" :target "x" :by 'name
                :id-property "CANVAS_ID" :feature "page"))
              :to-throw 'user-error))))

(describe "org-canvas--sync-heading-ask"
  (it "offers the titles under the query and returns the choice"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (let ((offered nil) (noninteractive nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _) (setq offered collection) (nth 1 collection))))
          (expect (org-canvas--sync-heading-ask "page" buffer-file-name "LEVEL=1")
                  :to-equal "Week 06 objectives"))
        (expect offered :to-equal '("Week 05 objectives" "Week 06 objectives" "Twice" "Twice"
                                    "[[file:content/syllabus.pdf][Syllabus.pdf]]")))))

  (it "never prompts in batch: a missing target is a user-error"
    (with-temp-org-buffer test-org-canvas-heading-file-content
      (let ((noninteractive t))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (error "prompted"))))
          (expect (org-canvas--sync-heading-ask "page" buffer-file-name "LEVEL=1")
                  :to-throw 'user-error))))))

(describe "org-canvas--sync-heading-runtime"
  (it "pushes the named heading at its position, stamps it and reports synced"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-org-canvas-heading-file-content
        (let ((saved nil) ctx)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'org-canvas--save-buffer) (lambda (&rest _) (setq saved t))))
            (setq ctx (org-canvas--sync-heading-runtime
                       (test-org-canvas-heading-spec :file buffer-file-name)
                       "Week 06 objectives")))
          (expect (plist-get ctx :outcome) :to-be 'synced)
          (expect saved :to-be-truthy)
          (goto-char (point-min))
          (re-search-forward "^\\* Week 06")
          (expect (org-entry-get (point) "CANVAS_ID") :to-equal "777")
          (re-search-backward "^\\* Week 05")
          (expect (org-entry-get (point) "CANVAS_ID") :to-equal "501")
          ;; Point in the file's buffer is where it was.
          (expect (point) :to-be-less-than (save-excursion (re-search-forward "Week 06")))))))

  (it "finds the heading by stamp with BY canvas-id and the spec's id property"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-org-canvas-heading-file-content
        (let ((pushed nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'org-canvas--save-buffer) #'ignore))
            (org-canvas--sync-heading-runtime
             (test-org-canvas-heading-spec
              :file buffer-file-name :id-property "CANVAS_ID"
              :push (lambda (data _payload &optional _ctx)
                      (setq pushed (plist-get data :title)) '((id . 501))))
             501 'canvas-id))
          (expect pushed :to-equal "Week 05 objectives")))))

  (it "asks for the title when none is given, outside batch"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-org-canvas-heading-file-content
        (let ((noninteractive nil) (pushed nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'org-canvas--save-buffer) #'ignore)
                    ((symbol-function 'completing-read) (lambda (&rest _) "Week 05 objectives")))
            (org-canvas--sync-heading-runtime
             (test-org-canvas-heading-spec
              :file buffer-file-name
              :push (lambda (data _payload &optional _ctx)
                      (setq pushed (plist-get data :title)) '((id . 501))))
             nil))
          (expect pushed :to-equal "Week 05 objectives")))))

  (it "leaves the file alone and reports the stop when the push says conflict"
    (with-org-canvas-test-config
      (with-temp-org-buffer test-org-canvas-heading-file-content
        (let ((messages nil) ctx)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
            (setq ctx (org-canvas--sync-heading-runtime
                       (test-org-canvas-heading-spec
                        :file buffer-file-name
                        :push (lambda (&rest _) 'conflict))
                       "Week 06 objectives")))
          (expect (plist-get ctx :outcome) :to-be 'conflict)
          (expect (car messages) :to-match "not pushed")
          (goto-char (point-min))
          (re-search-forward "^\\* Week 06")
          (expect (org-entry-get (point) "CANVAS_ID") :to-equal "601"))))))

(describe "org-canvas--push-at-point-runtime outcomes"
  (it "records unchanged when the stored hash matches"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* P\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
        (org-back-to-heading)
        (let* ((spec (test-org-canvas-heading-spec))
               (hash (org-canvas--sync-entry-hash '((title . "P")) nil (org-canvas--sync-make-ctx))))
          (org-entry-put (point) org-canvas--prop-payload-hash hash)
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (expect (plist-get (org-canvas--push-at-point-runtime spec) :outcome)
                    :to-be 'unchanged))))))

  (it "records unchanged when the push answers skip"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* P\n"
        (org-back-to-heading)
        (cl-letf (((symbol-function 'display-buffer) #'ignore))
          (expect (plist-get (org-canvas--push-at-point-runtime
                              (test-org-canvas-heading-spec :push (lambda (&rest _) 'skip)))
                             :outcome)
                  :to-be 'unchanged)))))

  (it "records a dry run and stamps nothing, instead of writing CANVAS_ID dry-run"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* P\n"
        (org-back-to-heading)
        (let ((finalized nil) ctx)
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (setq ctx (org-canvas--push-at-point-runtime
                       (test-org-canvas-heading-spec
                        :push (lambda (&rest _) org-canvas--dry-run-response)
                        :finalize (lambda (&rest _) (setq finalized t))))))
          (expect (plist-get ctx :outcome) :to-be 'dry-run)
          (expect finalized :to-be nil)
          (expect (org-entry-get (point) "CANVAS_ID") :to-be nil))))))

(describe "org-canvas--push-at-point-runtime :after-heading (issue #380)"
  (it "runs after a synced push, on the heading, and keeps its report"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* P\nBody.\n"
        (org-back-to-heading)
        (let ((seen nil) ctx)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (setq ctx (org-canvas--push-at-point-runtime
                       (test-org-canvas-heading-spec
                        :after-heading
                        (lambda (c)
                          (setq seen (list (org-get-heading t t t t)
                                           (org-entry-get (point) "CANVAS_ID")
                                           (plist-get c :outcome)))
                          '(:note "extra sent"))))))
          (expect seen :to-equal '("P" "777" synced))
          (expect (plist-get ctx :heading-report) :to-equal '(:note "extra sent"))))))

  (it "runs after an unchanged push and under a dry run"
    (with-org-canvas-test-config
      (dolist (push (list (lambda (&rest _) 'skip)
                          (lambda (&rest _) org-canvas--dry-run-response)))
        (with-temp-org-buffer "* P\n"
          (org-back-to-heading)
          (let ((ran 0))
            (cl-letf (((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'message) #'ignore))
              (org-canvas--push-at-point-runtime
               (test-org-canvas-heading-spec
                :push push :after-heading (lambda (_) (cl-incf ran) nil))))
            (expect ran :to-equal 1))))))

  (it "does not run when the push stopped at a conflict or a duplicate"
    (with-org-canvas-test-config
      (dolist (stop '(conflict duplicate pulled))
        (with-temp-org-buffer "* P\n"
          (org-back-to-heading)
          (let ((ran nil) ctx)
            (cl-letf (((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'message) #'ignore))
              (setq ctx (org-canvas--push-at-point-runtime
                         (test-org-canvas-heading-spec
                          :push (lambda (&rest _) stop)
                          :after-heading (lambda (_) (setq ran t) '(:note "x"))))))
            (expect ran :to-be nil)
            (expect (plist-get ctx :heading-report) :to-be nil)))))))

(describe "org-canvas-sync-headings heading reports (issue #380)"
  (after-each
    (setq org-canvas--sync-heading-fns
          (assoc-delete-all "widget" org-canvas--sync-heading-fns)))

  (it "merges the heading report into the result and prints its note"
    (let ((messages nil))
      (org-canvas--sync-register-heading-fn
       "widget" (lambda (_target _by)
                  (list :outcome 'synced
                        :heading-report '(:overrides (0 1 0)
                                          :note "overrides 0 created, 1 updated, 0 deleted"))))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (let ((result (car (org-canvas-sync-headings '((widget . "A"))))))
          (expect (plist-get result :overrides) :to-equal '(0 1 0))
          (expect (plist-get result :outcome) :to-be 'synced)))
      (expect (nth 1 messages)
              :to-equal "widget 'A': synced; overrides 0 created, 1 updated, 0 deleted"))))

(describe "org-canvas-define-sync sync-by-heading generation"
  (it "generates org-canvas-sync-page beside the at-point command"
    (expect (fboundp 'org-canvas-sync-page) :to-be-truthy)
    (expect (commandp 'org-canvas-sync-page) :to-be-truthy))

  (it "generates one per feature, singularized, and registers it"
    (dolist (name '("assignment" "announcement" "discussion" "rubric" "module" "file"
                    "assignment-group" "group-category" "calendar-event" "quiz"
                    "grading-scheme" "outcome-group" "outcome" "page" "new-quiz"))
      (let ((fn (intern (format "org-canvas-sync-%s" name))))
        (expect (list name (fboundp fn)) :to-equal (list name t))
        (expect (list name (cdr (assoc name org-canvas--sync-heading-fns)))
                :to-equal (list name fn)))))

  (it "tells the generated command which property the stamp lives in"
    (expect (documentation 'org-canvas-sync-page) :to-match "CANVAS_URL")
    (expect (documentation 'org-canvas-sync-assignment) :to-match "CANVAS_ID")
    (expect (documentation 'org-canvas-sync-assignment) :to-match "org-canvas-assignments-file"))

  (it "runs the page pipeline at the named heading of the pages file"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* Week 05 objectives\nBody.\n"
        (let ((org-canvas-pages-file buffer-file-name)
              (posted nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'org-canvas--save-buffer) #'ignore)
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (method _url &rest args)
                       (pcase method
                         ('POST (setq posted (plist-get args :data))
                                '((url . "week-05-objectives") (updated_at . "2026-09-17T10:00:00Z")))
                         (_ [])))))
            (let ((ctx (org-canvas-sync-page "Week 05 objectives")))
              (expect (plist-get ctx :outcome) :to-be 'synced)))
          (expect (gethash "title" (gethash "wiki_page" posted)) :to-equal "Week 05 objectives")
          (goto-char (point-min))
          (re-search-forward "^\\* Week 05")
          (expect (org-entry-get (point) "CANVAS_URL") :to-equal "week-05-objectives")))))

  (it "sends nothing and stamps nothing under a dry run"
    (with-org-canvas-test-config
      (with-temp-org-buffer "* Week 05 objectives\nBody.\n"
        (let ((org-canvas-pages-file buffer-file-name)
              (org-canvas--dry-run t)
              (writes nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'org-canvas--save-buffer) #'ignore)
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (method _url &rest _)
                       (unless (eq method 'GET) (push method writes))
                       [])))
            (expect (plist-get (org-canvas-sync-page "Week 05 objectives") :outcome)
                    :to-be 'dry-run))
          (expect writes :to-equal nil)
          (goto-char (point-min))
          (re-search-forward "^\\* Week 05")
          (expect (org-entry-get (point) "CANVAS_URL") :to-be nil))))))

(describe "org-canvas--sync-heading-fn-for"
  (it "resolves a plural or singular feature, as a symbol or a string"
    (dolist (name '(assignments assignment "assignments" "assignment" quizzes quiz new-quizzes))
      (expect (list name (org-canvas--sync-heading-fn-for name))
              :to-equal (list name (if (memq name '(quizzes quiz)) #'org-canvas-sync-quiz
                                     (if (eq name 'new-quizzes) #'org-canvas-sync-new-quiz
                                       #'org-canvas-sync-assignment))))))

  (it "is a user-error listing the features for a name nobody registered"
    (condition-case err
        (progn (org-canvas--sync-heading-fn-for 'settings) (expect nil :to-be-truthy))
      (user-error (expect (cadr err) :to-match "No sync by heading for settings; the features are: .*assignment.*page")))))

(describe "org-canvas--sync-headings-entry"
  (it "reads a dotted pair, a two-list and a three-list"
    (expect (org-canvas--sync-headings-entry '(assignment . "R5")) :to-equal '(assignment "R5" nil))
    (expect (org-canvas--sync-headings-entry '(page "W5")) :to-equal '(page "W5" nil))
    (expect (org-canvas--sync-headings-entry '(assignment "2563805" canvas-id))
            :to-equal '(assignment "2563805" canvas-id)))

  (it "rejects anything else"
    (expect (org-canvas--sync-headings-entry "R5") :to-throw 'user-error)
    (expect (org-canvas--sync-headings-entry '(assignment)) :to-throw 'user-error)))

(describe "org-canvas-sync-headings"
  (before-each
    (org-canvas--sync-register-heading-fn "widget" #'ignore))
  (after-each
    (setq org-canvas--sync-heading-fns
          (assoc-delete-all "widget" org-canvas--sync-heading-fns)))

  (it "syncs each entry in order with its target and BY, and returns the outcomes"
    (let ((calls nil) (messages nil))
      (org-canvas--sync-register-heading-fn
       "widget" (lambda (target by) (push (list target by) calls)
                  (list :outcome (if by 'unchanged 'synced))))
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (let ((results (org-canvas-sync-headings '((widget . "A") (widgets "7" canvas-id)))))
          (expect (nreverse calls) :to-equal '(("A" nil) ("7" canvas-id)))
          (expect (mapcar (lambda (r) (plist-get r :outcome)) results) :to-equal '(synced unchanged))
          (expect (plist-get (car results) :target) :to-equal "A")))
      (expect (car messages) :to-equal "Synced 2 heading(s): 1 synced, 1 unchanged, 0 stopped, 0 failed")
      (expect (nth 1 messages) :to-match "widgets '7': unchanged")))

  (it "reports a failed heading with its error and pushes the next one"
    (let ((calls nil) (messages nil) (logged nil))
      (org-canvas--sync-register-heading-fn
       "widget" (lambda (target _by)
                  (push target calls)
                  (if (equal target "bad") (user-error "No widget heading titled \"bad\"")
                    (list :outcome 'synced))))
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                ((symbol-function 'org-canvas--log-error) (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
        (let ((results (org-canvas-sync-headings '((widget . "bad") (widget . "good")))))
          (expect (nreverse calls) :to-equal '("bad" "good"))
          (expect (plist-get (car results) :outcome) :to-be 'failed)
          (expect (plist-get (car results) :error) :to-match "No widget heading")
          (expect (plist-get (cadr results) :outcome) :to-be 'synced)))
      (expect (car messages) :to-match "1 synced, 0 unchanged, 0 stopped, 1 failed")
      (expect (car logged) :to-match "widget 'bad' failed")))

  (it "counts a conflict, a pull, a duplicate and a dry run as stopped"
    (let ((messages nil) (answers (list 'conflict 'pulled 'duplicate 'dry-run)))
      (org-canvas--sync-register-heading-fn
       "widget" (lambda (_target _by) (list :outcome (pop answers))))
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (org-canvas-sync-headings '((widget . "a") (widget . "b") (widget . "c") (widget . "d"))))
      (expect (car messages) :to-match "0 synced, 0 unchanged, 4 stopped, 0 failed")))

  (it "stops before anything is sent when a feature is not known"
    (let ((calls 0))
      (org-canvas--sync-register-heading-fn "widget" (lambda (&rest _) (cl-incf calls) (list :outcome 'synced)))
      (expect (org-canvas-sync-headings '((widget . "a") (gizmo . "b"))) :to-throw 'user-error)
      (expect calls :to-equal 0))))

(defmacro test-sync-399--with-heading (reply &rest body)
  "Run BODY at a stamped announcement whose single read answers REPLY.
REPLY is a function of the URL.  BODY sees `saved' and `messages'."
  (declare (indent 1))
  `(with-org-canvas-test-config
     (with-temp-org-buffer
      "* Sprint 0\n:PROPERTIES:\n:CANVAS_ID: 100\n:END:\nMy notes.\n"
      (let ((org-canvas-announcements-file buffer-file-name)
            (saved nil)
            (messages nil))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (_m url &rest _) (funcall ,reply url)))
                  ((symbol-function 'org-canvas--save-buffer)
                   (lambda (&rest _) (setq saved t)))
                  ((symbol-function 'org-canvas--log-warning) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (goto-char (point-min))
          (org-back-to-heading t)
          (ignore saved messages)
          ,@body)))))

(describe "a pull at point of an item Canvas deleted (issue #399)"
  (it "marks the heading, keeps its text and saves, on a 404"
    (test-sync-399--with-heading
        (lambda (_url) (signal 'org-canvas-api-error (list "Not found (HTTP 404)")))
      (let ((feature (org-canvas--registry-find-feature "announcements")))
        (expect (org-canvas--pull-at-point-1 feature "100" "Sprint 0") :to-be 'gone))
      (expect (org-entry-get (point) "CANVAS_DELETED") :to-match "\\`\\[[0-9]\\{4\\}-")
      (expect (org-entry-get (point) "CANVAS_ID") :to-equal "100")
      (expect (buffer-string) :to-match "My notes\\.")
      (expect saved :to-be-truthy)
      (expect (car messages)
              :to-match "'Sprint 0' is no longer on Canvas (Announcements 100); heading kept, marked CANVAS_DELETED")))

  (it "marks it on a reply whose workflow_state is deleted, and names it by heading"
    (test-sync-399--with-heading
        (lambda (_url) '((id . 100) (workflow_state . "deleted")))
      (let ((result (org-canvas--pull-heading-here "announcement")))
        (expect (plist-get result :outcome) :to-be 'deleted))
      (expect (org-entry-get (point) "CANVAS_DELETED") :to-be-truthy)))

  (it "still fails on any other error, marking nothing"
    (test-sync-399--with-heading
        (lambda (_url) (signal 'org-canvas-api-error (list "Boom (HTTP 500)")))
      (let ((feature (org-canvas--registry-find-feature "announcements")))
        (expect (org-canvas--pull-at-point-1 feature "100" "Sprint 0")
                :to-throw 'org-canvas-api-error))
      (expect (org-entry-get (point) "CANVAS_DELETED") :to-be nil)))

  (it "counts a deleted heading in the pull by name's report"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (org-canvas--pull-headings-report
         (list (list :feature "page" :target "A" :outcome 'deleted)
               (list :feature "page" :target "B" :outcome 'pulled))))
      (expect (car messages)
              :to-equal "Pulled 2 heading(s): 1 pulled, 0 dry run, 0 failed, 1 deleted on Canvas"))))

(provide 'org-canvas-core-heading-test)
;;; org-canvas-core-heading-test.el ends here
