;;; org-canvas-setup.el --- Setup wizard for new courses -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Interactive setup wizard for initializing org-canvas with a new course.
;; Creates credentials file, skeleton .org files, and tests the connection.

;;; Code:

(require 'org-canvas-core)

(defconst org-canvas--skeleton-files
  '("assignments.org" "pages.org" "quizzes.org" "modules.org"
    "files.org" "outcomes.org" "rubrics.org" "discussions.org"
    "announcements.org" "assignment-groups.org" "sections.org"
    "settings.org")
  "List of org files to create in a new course skeleton.")

(defun org-canvas--write-credentials-file (dir url token course-id)
  "Write org-canvas-credentials.el in DIR with URL, TOKEN, COURSE-ID."
  (let ((file (expand-file-name "org-canvas-credentials.el" dir)))
    (with-temp-file file
      (insert ";;; org-canvas-credentials.el --- Course credentials  -*- lexical-binding: t; -*-\n\n")
      (insert ";; This file is NOT checked into version control.\n")
      (insert ";; It contains sensitive API credentials.\n\n")
      (insert (format "(setq org-canvas-directory %S)\n" dir))
      (insert (format "(setq org-canvas-base-url %S)\n" url))
      (insert (format "(setq org-canvas-api-token %S)\n" token))
      (insert (format "(setq org-canvas-course-id %S)\n" course-id))
      (insert "\n(provide 'org-canvas-credentials)\n")
      (insert ";;; org-canvas-credentials.el ends here\n"))
    file))

(defun org-canvas--suggest-gitignore (dir)
  "Suggest adding credentials to .gitignore in DIR."
  (let ((gitignore (expand-file-name ".gitignore" dir)))
    (if (not (file-exists-p gitignore))
        (when (y-or-n-p
               "No .gitignore found.  Create one to protect credentials? ")
          (with-temp-file gitignore
            (insert "org-canvas-credentials.el\n")))
      (with-temp-buffer
        (insert-file-contents gitignore)
        (unless (string-match-p "org-canvas-credentials" (buffer-string))
          (when (y-or-n-p "Add org-canvas-credentials.el to .gitignore? ")
            (append-to-file
             "org-canvas-credentials.el\n" nil gitignore)))))))

(defun org-canvas--create-skeleton-files (dir)
  "Create minimal skeleton .org files in DIR."
  (dolist (filename org-canvas--skeleton-files)
    (let ((file (expand-file-name filename dir)))
      (unless (file-exists-p file)
        (with-temp-file file
          (insert (format "#+TITLE: %s\n"
                          (capitalize (file-name-sans-extension filename))))
          (insert "# See documentation/manual.org for property reference\n"))))))

;;;###autoload
(defun org-canvas-init ()
  "Set up org-canvas for a new course.
Prompts for required configuration, tests the connection,
and writes org-canvas-credentials.el."
  (interactive)
  (let* ((dir (read-directory-name "Course directory: " nil nil t))
         (url (read-string "Canvas base URL: " "https://canvas.instructure.com"))
         (token (read-passwd "API token (Canvas > Account > Settings > + New Access Token): "))
         (course-id (read-string "Course ID (number from your Canvas course URL): ")))
    ;; Validate inputs
    (when (string-empty-p token)
      (user-error "API token cannot be empty"))
    (when (string-empty-p course-id)
      (user-error "Course ID cannot be empty"))
    (unless (string-prefix-p "https://" url)
      (unless (y-or-n-p "URL does not start with https://.  Continue anyway? ")
        (user-error "Aborted")))
    ;; Check for existing credentials file
    (let ((cred-file (expand-file-name "org-canvas-credentials.el" dir)))
      (when (file-exists-p cred-file)
        (unless (y-or-n-p (format "%s already exists.  Overwrite? "
                                  (file-name-nondirectory cred-file)))
          (user-error "Aborted"))))
    ;; Test connection before writing config
    (message "Testing connection...")
    (let ((org-canvas-base-url url)
          (org-canvas-api-token token)
          (org-canvas-course-id course-id))
      (condition-case err
          (let ((course (org-canvas-api-request 'GET
                          (org-canvas-api-course-endpoint ""))))
            (message "Connected to: %s" (alist-get 'name course)))
        (error
         (if (y-or-n-p (format "Connection failed: %s\nSave credentials anyway? "
                               (error-message-string err)))
             (message "Saving credentials without connection verification...")
           (user-error "Aborted")))))
    ;; Write credentials
    (let ((cred-file (org-canvas--write-credentials-file dir url token course-id)))
      (message "Credentials saved to %s" cred-file))
    ;; Suggest .gitignore protection
    (org-canvas--suggest-gitignore dir)
    ;; Optionally create skeleton files
    (when (y-or-n-p "Create skeleton .org files for all content types? ")
      (org-canvas--create-skeleton-files dir)
      (message "Created skeleton files in %s" dir))
    ;; Load the new credentials
    (setq org-canvas-directory dir)
    (setq org-canvas-base-url url)
    (setq org-canvas-api-token token)
    (setq org-canvas-course-id course-id)
    (org-canvas--recompute-file-paths)
    ;; Offer to register in course registry
    (let ((course-name (read-string "Register as course (empty to skip): ")))
        (unless (string-empty-p course-name)
          (customize-save-variable
           'org-canvas-courses
           (cons (cons course-name dir)
                 (assoc-delete-all course-name org-canvas-courses)))
          (setq org-canvas--active-course-name course-name)))
    (message "org-canvas initialized!  Use M-x org-canvas-status to see sync state.")))

(defun org-canvas--read-course-name ()
  "Prompt for a course name from `org-canvas-courses'."
  (completing-read "Course: "
                   (mapcar #'car org-canvas-courses) nil t))

;;;###autoload
(defun org-canvas-activate-course (&optional name)
  "Activate the course named NAME from `org-canvas-courses'.
If NAME is nil and called interactively, prompt with completion."
  ;; Bare `(interactive)' rather than `(interactive (list ...))'.
  ;; A sexp argument to `interactive' confuses edebug enough that
  ;; undercover stops counting hits inside `unless'/`when' bodies in
  ;; the function — see CLAUDE.md for the bisection.
  (interactive)
  (unless name
    (setq name (org-canvas--read-course-name)))
  (let ((dir (cdr (assoc name org-canvas-courses))))
    (unless dir
      (user-error "Course '%s' not found in org-canvas-courses" name))
    (unless (file-directory-p dir)
      (user-error "Course directory does not exist: %s" dir))
    (let ((cred-file (expand-file-name "org-canvas-credentials.el" dir)))
      (unless (file-exists-p cred-file)
        (user-error "No org-canvas-credentials.el in %s" dir))
      (load cred-file nil t)
      ;; The credentials file's `(setq org-canvas-directory ...)' triggers
      ;; the directory watcher in core-config.el, which recomputes all
      ;; registered file vars — no explicit `recompute-file-paths' needed.
      ;; The course zone belongs to the old course; forget it.
      (org-canvas--time-zone-reset)
      (org-canvas-clear-log)
      (setq org-canvas--active-course-name name)
      (message "Activated course: %s (ID: %s at %s)"
               name org-canvas-course-id org-canvas-base-url))))

(provide 'org-canvas-setup)
;;; org-canvas-setup.el ends here
