;;; org-canvas-browse-test.el --- Tests for org-canvas-browse-at-point  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `org-canvas-browse-at-point' opens a heading's Canvas web page (issue
;; #292).  `browse-url' is mocked throughout, and `org-canvas-api-request'
;; is made to fail, since opening a page must never send anything.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)

(defun test-org-canvas--browse (file-var content search &optional edit)
  "Browse the heading SEARCH finds in CONTENT, a file bound to FILE-VAR.
EDIT is passed on.  Returns a plist: :url, the address `browse-url'
was handed; :return, what the command returned; :error, the message
of a `user-error' it signalled; :message, the echo-area line."
  (let ((opened nil) (said nil) (result nil) (err nil))
    (with-temp-org-buffer content
      (cl-letf (((symbol-value file-var) (buffer-file-name))
                ((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq opened url)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args))))
                ((symbol-function 'org-canvas-api-request)
                 (lambda (&rest _) (error "Browsing sent a request"))))
        (let ((org-canvas-base-url "https://canvas.test")
              (org-canvas-course-id "42"))
          (when search (re-search-forward search))
          (condition-case e
              (setq result (org-canvas-browse-at-point edit))
            (user-error (setq err (error-message-string e)))))))
    (list :url opened :return result :error err :message said)))

(describe "org-canvas-browse-at-point (issue #292)"
  (it "opens an assignment's page, and its edit page with a prefix"
    (let ((content "* Essay\n:PROPERTIES:\n:CANVAS_ID: 2497349\n:END:\nBody.\n"))
      (let ((view (test-org-canvas--browse 'org-canvas-assignments-file content "Body"))
            (edit (test-org-canvas--browse 'org-canvas-assignments-file content "Body" t)))
        (expect (plist-get view :url)
                :to-equal "https://canvas.test/courses/42/assignments/2497349")
        (expect (plist-get view :return) :to-equal (plist-get view :url))
        (expect (plist-get view :message) :to-equal "Opened 'Essay' on Canvas")
        (expect (plist-get edit :url)
                :to-equal "https://canvas.test/courses/42/assignments/2497349/edit"))))

  (it "opens the edit page through its own command"
    (let (opened)
      (cl-letf (((symbol-function 'org-canvas-browse-at-point)
                 (lambda (&optional edit) (setq opened edit))))
        (org-canvas-browse-edit-at-point))
      (expect opened :to-be t)))

  (it "names a page by its CANVAS_URL slug"
    (expect (plist-get (test-org-canvas--browse
                        'org-canvas-pages-file
                        "* Syllabus\n:PROPERTIES:\n:CANVAS_URL: syllabus-2\n:END:\n" nil t)
                       :url)
            :to-equal "https://canvas.test/courses/42/pages/syllabus-2/edit"))

  (it "opens discussions and announcements at their discussion topic"
    (expect (plist-get (test-org-canvas--browse
                        'org-canvas-discussions-file
                        "* Intro\n:PROPERTIES:\n:CANVAS_ID: 7\n:END:\n" nil)
                       :url)
            :to-equal "https://canvas.test/courses/42/discussion_topics/7")
    (expect (plist-get (test-org-canvas--browse
                        'org-canvas-announcements-file
                        "* Welcome\n:PROPERTIES:\n:CANVAS_ID: 8\n:END:\n" nil t)
                       :url)
            :to-equal "https://canvas.test/courses/42/discussion_topics/8/edit"))

  (it "opens a New Quiz by its assignment id"
    (expect (plist-get (test-org-canvas--browse
                        'org-canvas-new-quizzes-file
                        (concat "* Quiz 01\n:PROPERTIES:\n:CANVAS_ASSIGNMENT_ID: 2391125\n:END:\n"
                                "** An item\n:PROPERTIES:\n:CANVAS_ITEM_ID: 70023\n:END:\n")
                        "An item")
                       :url)
            :to-equal "https://canvas.test/courses/42/assignments/2391125"))

  (it "opens a classic quiz question's quiz, and says why"
    (let ((r (test-org-canvas--browse
              'org-canvas-quizzes-file
              (concat "* Quiz 01\n:PROPERTIES:\n:CANVAS_ID: 639491\n:END:\n"
                      "** Question: Types\n:PROPERTIES:\n:CANVAS_ID: 9742963\n:END:\n")
              "Types" t)))
      (expect (plist-get r :url)
              :to-equal "https://canvas.test/courses/42/quizzes/639491/edit")
      (expect (plist-get r :message)
              :to-equal "Opened 'Quiz 01' on Canvas ('Question: Types' has no page of its own)")))

  (it "opens a module, and a module item by its own id"
    (let ((content (concat "* Week 01\n:PROPERTIES:\n:CANVAS_ID: 751444\n:END:\n"
                           "** Course Home\n:PROPERTIES:\n:CANVAS_ID: 5440928\n:END:\n")))
      (expect (plist-get (test-org-canvas--browse 'org-canvas-modules-file content nil) :url)
              :to-equal "https://canvas.test/courses/42/modules/751444")
      (expect (plist-get (test-org-canvas--browse 'org-canvas-modules-file content "Course Home")
                         :url)
              :to-equal "https://canvas.test/courses/42/modules/items/5440928")))

  (it "says when Canvas has no separate edit page and opens the page itself"
    (let ((r (test-org-canvas--browse
              'org-canvas-rubrics-file
              "* Lab Rubric\n:PROPERTIES:\n:CANVAS_ID: 123587\n:END:\n** Code Quality :10pt:\n"
              "Code Quality" t)))
      (expect (plist-get r :url) :to-equal "https://canvas.test/courses/42/rubrics/123587")
      (expect (plist-get r :message) :to-match "has no page of its own")
      (expect (plist-get r :message) :to-match "no separate edit page")))

  (it "opens a file by id and a folder by its path of names"
    (let ((content (concat "* Lecture Notes\n"
                           "** Week 1\n"
                           "*** [[file:content/a b.pdf][a b.pdf]]\n"
                           ":PROPERTIES:\n:CANVAS_ID: 29672559\n:END:\n"
                           "*** [[file:content/new.pdf][new.pdf]]\n")))
      (expect (plist-get (test-org-canvas--browse 'org-canvas-files-file content "a b.pdf\\]") :url)
              :to-equal "https://canvas.test/courses/42/files/29672559")
      (expect (plist-get (test-org-canvas--browse 'org-canvas-files-file content "Week 1") :url)
              :to-equal "https://canvas.test/courses/42/files/folder/Lecture%20Notes/Week%201")
      (expect (plist-get (test-org-canvas--browse 'org-canvas-files-file content "new.pdf\\]")
                         :error)
              :to-match "has no CANVAS_ID yet")))

  (it "opens outcomes, and a group at the course's outcomes page"
    (let ((content (concat "* Programming Skills\n:PROPERTIES:\n:CANVAS_ID: 124176\n:END:\n"
                           "** Python Proficiency\n:PROPERTIES:\n:CANVAS_ID: 51479\n:END:\n")))
      (expect (plist-get (test-org-canvas--browse 'org-canvas-outcomes-file content nil) :url)
              :to-equal "https://canvas.test/courses/42/outcomes")
      (expect (plist-get (test-org-canvas--browse 'org-canvas-outcomes-file content "Python")
                         :url)
              :to-equal "https://canvas.test/courses/42/outcomes/51479")))

  (it "covers the remaining registered files"
    (dolist (case '((org-canvas-calendar-events-file t "calendar_events/5/edit")
                    (org-canvas-group-categories-file nil "groups#tab-5")
                    (org-canvas-grading-schemes-file nil "grading_standards")
                    (org-canvas-assignment-groups-file nil "assignments")
                    (org-canvas-sections-file nil "sections/5")
                    (org-canvas-settings-file nil "settings")))
      (expect (plist-get (test-org-canvas--browse
                          (nth 0 case) "* Thing\n:PROPERTIES:\n:CANVAS_ID: 5\n:END:\n"
                          nil (nth 1 case))
                         :url)
              :to-equal (concat "https://canvas.test/courses/42/" (nth 2 case)))))

  (it "refuses an unstamped heading, naming the property to sync"
    (let ((r (test-org-canvas--browse 'org-canvas-assignments-file "* Draft\nText\n" "Text")))
      (expect (plist-get r :url) :to-be nil)
      (expect (plist-get r :error)
              :to-match "Draft. has no CANVAS_ID yet; sync it to Canvas first")))

  (it "refuses a buffer that is no course file"
    (let (err)
      (with-temp-org-buffer "* Anything\n"
        (condition-case e (org-canvas-browse-at-point)
          (user-error (setq err (error-message-string e)))))
      (expect err :to-match "is not a course file with pages on Canvas")))

  (it "refuses point above the first heading"
    (expect (plist-get (test-org-canvas--browse
                        'org-canvas-assignments-file
                        "#+TITLE: Assignments\n* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n" nil)
                       :error)
            :to-equal "Move point to a heading first"))

  (it "refuses when no course is configured"
    (let (err)
      (let ((org-canvas-course-id ""))
        (condition-case e (org-canvas--web-url "assignments/1")
          (user-error (setq err (error-message-string e)))))
      (expect err :to-match "No course configured"))))

(describe "org-canvas--browse-heading-target"
  (it "signals when no rule names any level up the outline"
    (with-temp-org-buffer "* Top\n** Child\n"
      (re-search-forward "Child")
      (expect (org-canvas--browse-heading-target
               '(:name "Odd" :web-pages ((:level 3 :path "x")))
               nil)
              :to-throw 'user-error)))

  (it "names a generic id when the level's rules carry no id property"
    (with-temp-org-buffer "* Top\n"
      (let ((err (condition-case e
                     (org-canvas--browse-heading-target
                      (list :name "Odd" :web-pages
                            (list (list :level 1 :path-fn #'ignore)))
                      nil)
                   (user-error (error-message-string e)))))
        (expect err :to-match "Top. has no Canvas id yet")))))

(describe "org-canvas--feature-web-url"
  (it "uses the feature's first top-level rule that names a path"
    (let ((org-canvas-base-url "https://canvas.test/")
          (org-canvas-course-id "42"))
      (expect (org-canvas--feature-web-url
               (org-canvas--registry-find-feature "Files") "9")
              :to-equal "https://canvas.test/courses/42/files/9")
      (expect (org-canvas--feature-web-url
               (org-canvas--registry-find-feature "Assignments") "9" t)
              :to-equal "https://canvas.test/courses/42/assignments/9/edit")
      (expect (org-canvas--feature-web-url '(:name "None") "9") :to-be nil))))

(describe "org-canvas-register-web-pages"
  (it "replaces an entry of the same name"
    (let ((org-canvas--web-page-registry nil))
      (org-canvas-register-web-pages "X" 'x-file '(:path "a"))
      (org-canvas-register-web-pages "X" 'x-file '(:path "b"))
      (expect (length org-canvas--web-page-registry) :to-equal 1)
      (expect (plist-get (car (plist-get (car org-canvas--web-page-registry) :web-pages))
                         :path)
              :to-equal "b"))))

(provide 'org-canvas-browse-test)
;;; org-canvas-browse-test.el ends here
