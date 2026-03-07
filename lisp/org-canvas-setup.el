;;; org-canvas-setup.el --- Setup wizard for new courses -*- lexical-binding: t; -*-

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
         (token (read-string "API token: "))
         (course-id (read-string "Course ID: ")))
    ;; Validate inputs
    (when (string-empty-p token)
      (user-error "API token cannot be empty"))
    (when (string-empty-p course-id)
      (user-error "Course ID cannot be empty"))
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
    ;; Optionally create skeleton files
    (when (y-or-n-p "Create skeleton .org files for all content types? ")
      (org-canvas--create-skeleton-files dir)
      (message "Created skeleton files in %s" dir))
    ;; Load the new credentials
    (setq org-canvas-directory dir)
    (setq org-canvas-base-url url)
    (setq org-canvas-api-token token)
    (setq org-canvas-course-id course-id)
    (message "org-canvas initialized!  Use M-x org-canvas-status to see sync state.")))

(provide 'org-canvas-setup)
;;; org-canvas-setup.el ends here
