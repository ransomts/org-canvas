;;; org-canvas-peer-reviews.el --- Pull who reviews whom on an assignment -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An assignment's peer-review settings (PEER_REVIEWS, PEER_REVIEW_COUNT,
;; PEER_REVIEW_DUE_AT, AUTOMATIC_PEER_REVIEWS) are pushed from
;; assignments.org.  Who was then assigned to review whom, and who never
;; did, is visible only in SpeedGrader, and an unassigned or undone
;; review silently costs a student points.
;;
;; `org-canvas-pull-peer-reviews' picks an assignment with peer reviews
;; enabled and writes what Canvas holds as two read-only tables under
;; the submissions directory, as <assignment> (peer reviews).org:
;;
;;   Reviewer | Reviews | State | Completed at    one row per review,
;;                                               sorted by reviewer
;;   Student | Reviewers assigned | Reviews received
;;                                               one row per active
;;                                               student, so a zero
;;                                               is visible
;;
;; and a summary line above them.  Every pull rewrites the file.
;; Nothing is pushed: assigning a reviewer stays the web UI's business
;; (the API has it; see coverage.org).
;;
;; API NOTES
;; =========
;;   GET /courses/:id/assignments            filtered to `peer_reviews'
;;   GET /courses/:id/assignments/:id/peer_reviews
;;         ?include[]=user&include[]=submission_comments
;;       one row per review: `user_id' (the student reviewed),
;;       `assessor_id' (the reviewer), `workflow_state' (assigned or
;;       completed), `user' and `assessor' as display objects, and the
;;       submission's comments, from which the reviewer's newest one
;;       dates the completion.
;;   GET /courses/:id/enrollments?type[]=StudentEnrollment&state[]=active
;;       the roster, for the names and for the students nobody reviews.
;;
;; PRIVACY
;; =======
;; The file names students.  It lives in the submissions directory,
;; which gets a .gitignore when created
;; (`org-canvas-submissions-write-gitignore'), so it never travels with
;; a course repository.

;;; Code:

(require 'org-canvas-core)
(require 'cl-lib)
;; A command file above the feature modules, extending the grading
;; workflow: it reuses the submissions directory and its .gitignore
;; (as quiz-submissions does).
(require 'org-canvas-submissions)

;;;; Fetching

(defun org-canvas--peer-reviews-fetch-assignments ()
  "Return the course's assignments with peer reviews enabled, by name."
  (sort (cl-remove-if-not
         (lambda (a) (eq (alist-get 'peer_reviews a) t))
         (append (org-canvas-api-request-all-pages
                  'GET (org-canvas-api-course-endpoint "assignments"))
                 nil))
        (lambda (a b) (string< (or (alist-get 'name a) "")
                               (or (alist-get 'name b) "")))))

(defun org-canvas--peer-reviews-fetch (assignment-id)
  "Return ASSIGNMENT-ID's peer reviews as a list, with users and comments."
  (append (org-canvas-api-request-all-pages
           'GET (org-canvas-api-course-endpoint "assignments/%s/peer_reviews"
                                                assignment-id)
           '(("include[]" . "user") ("include[]" . "submission_comments")))
          nil))

(defun org-canvas--peer-reviews-fetch-students ()
  "Return the active students as an alist of user id to sortable name."
  (let ((students nil))
    (dolist (e (append (org-canvas-api-request-all-pages
                        'GET (org-canvas-api-course-endpoint "enrollments")
                        '(("type[]" . "StudentEnrollment") ("state[]" . "active")))
                       nil))
      (let ((uid (alist-get 'user_id e))
            (user (alist-get 'user e)))
        (unless (assoc uid students)
          (push (cons uid (or (alist-get 'sortable_name user)
                              (alist-get 'name user)
                              (format "User %s" uid)))
                students))))
    (nreverse students)))

;;;; Rows

(defun org-canvas--peer-reviews-name (user-id display students)
  "Return the name of USER-ID: the roster's, else DISPLAY's, else the id.
DISPLAY is the user object the include supplies, or nil; STUDENTS is
the roster alist."
  (or (cdr (assoc user-id students))
      (org-canvas--alist-get-non-null 'display_name display)
      (org-canvas--alist-get-non-null 'sortable_name display)
      (org-canvas--alist-get-non-null 'name display)
      (format "User %s" user-id)))

(defun org-canvas--peer-reviews-completed-at (review)
  "Return when the reviewer of REVIEW last commented, or nil.
A peer review is complete once the reviewer has assessed or commented,
and the review object carries no date of its own, so the reviewer's
newest submission comment stands in."
  (let ((assessor (alist-get 'assessor_id review))
        (latest nil))
    (dolist (c (append (alist-get 'submission_comments review) nil))
      (let ((at (org-canvas--alist-get-non-null 'created_at c)))
        (when (and (equal (alist-get 'author_id c) assessor)
                   (stringp at)
                   (or (null latest) (string> at latest)))
          (setq latest at))))
    latest))

(defun org-canvas--peer-reviews-rows (reviews students)
  "Fold REVIEWS into one plist per review, sorted by reviewer then student.
STUDENTS is the roster alist.  Each plist has :reviewer, :student,
:reviewer-id, :student-id, :completed (non-nil when the review is
done) and :completed-at (an ISO timestamp or nil)."
  (let ((rows
         (mapcar
          (lambda (r)
            (let ((reviewer (alist-get 'assessor_id r))
                  (student (alist-get 'user_id r)))
              (list :reviewer (org-canvas--peer-reviews-name
                               reviewer (alist-get 'assessor r) students)
                    :student (org-canvas--peer-reviews-name
                              student (alist-get 'user r) students)
                    :reviewer-id reviewer
                    :student-id student
                    :completed (equal (alist-get 'workflow_state r) "completed")
                    :completed-at (org-canvas--peer-reviews-completed-at r))))
          reviews)))
    (sort rows (lambda (a b)
                 (let ((ra (plist-get a :reviewer)) (rb (plist-get b :reviewer)))
                   (if (string= ra rb)
                       (string< (plist-get a :student) (plist-get b :student))
                     (string< ra rb)))))))

(defun org-canvas--peer-reviews-student-rows (rows students)
  "Return one plist per student in STUDENTS or ROWS, sorted by name.
Each has :name, :id, :assigned (reviewers assigned to the student) and
:received (reviews of the student that are complete).  A student on
the roster with no review at all is a row of zeros, which is the
point."
  (let ((table nil))
    (dolist (s students)
      (push (list :name (cdr s) :id (car s) :assigned 0 :received 0) table))
    (dolist (row rows)
      (let* ((id (plist-get row :student-id))
             (entry (cl-find-if (lambda (e) (equal (plist-get e :id) id)) table)))
        (unless entry
          (setq entry (list :name (plist-get row :student) :id id :assigned 0 :received 0))
          (push entry table))
        (plist-put entry :assigned (1+ (plist-get entry :assigned)))
        (when (plist-get row :completed)
          (plist-put entry :received (1+ (plist-get entry :received))))))
    (sort table (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

;;;; Rendering

(defun org-canvas--peer-reviews-summary (rows student-rows)
  "Return the summary line for ROWS and STUDENT-ROWS."
  (let ((reviewers-done
         (cl-remove-duplicates
          (mapcar (lambda (r) (plist-get r :reviewer-id))
                  (cl-remove-if-not (lambda (r) (plist-get r :completed)) rows)))))
    (format "%d reviews assigned | %d completed | %d students with no reviewer | %d students who reviewed nobody"
            (length rows)
            (cl-count-if (lambda (r) (plist-get r :completed)) rows)
            (cl-count-if (lambda (s) (= 0 (plist-get s :assigned))) student-rows)
            (cl-count-if (lambda (s) (not (memq (plist-get s :id) reviewers-done)))
                         student-rows))))

(defun org-canvas--peer-reviews-insert-table (header rows-fn)
  "Insert a table with HEADER and one row per cell list ROWS-FN yields.
HEADER is a list of column titles; ROWS-FN is a function returning a
list of cell lists.  The table is aligned once written."
  (let ((start (point)))
    (insert "| " (mapconcat #'identity header " | ") " |\n")
    (insert "|" (mapconcat (lambda (_) "---") header "+") "|\n")
    (dolist (cells (funcall rows-fn))
      (insert "| " (mapconcat #'identity cells " | ") " |\n"))
    (save-excursion
      (goto-char start)
      (org-table-align))))

(defun org-canvas--peer-reviews-insert (assignment rows student-rows)
  "Insert the file for ASSIGNMENT from ROWS and STUDENT-ROWS at point."
  (insert (format "#+TITLE: Peer reviews: %s\n" (alist-get 'name assignment)))
  (insert (format "#+PROPERTY: ASSIGNMENT_ID %s\n" (alist-get 'id assignment)))
  (insert (format "#+PROPERTY: ASSIGNMENT_NAME %s\n" (alist-get 'name assignment)))
  (insert (format "#+PROPERTY: PULLED_AT %s\n" (format-time-string "<%Y-%m-%d %a %H:%M>")))
  (when (stringp (alist-get 'html_url assignment))
    (insert (format "\nAssignment: [[%s][Open in Canvas]]\n" (alist-get 'html_url assignment))))
  (insert "\nRead-only: every pull rewrites this file.  Reviewers are assigned in\n"
          "the Canvas web UI (SpeedGrader or the assignment's peer review page).\n\n")
  (insert (org-canvas--peer-reviews-summary rows student-rows) "\n\n")
  (org-canvas--peer-reviews-insert-table
   '("Reviewer" "Reviews" "State" "Completed at")
   (lambda ()
     (mapcar (lambda (row)
               (list (plist-get row :reviewer)
                     (plist-get row :student)
                     (if (plist-get row :completed) "completed" "assigned")
                     (or (org-canvas--iso8601-to-org-timestamp (plist-get row :completed-at))
                         "-")))
             rows)))
  (insert "\n")
  (org-canvas--peer-reviews-insert-table
   '("Student" "Reviewers assigned" "Reviews received")
   (lambda ()
     (mapcar (lambda (s)
               (list (plist-get s :name)
                     (number-to-string (plist-get s :assigned))
                     (number-to-string (plist-get s :received))))
             student-rows))))

(defun org-canvas--peer-reviews-file-path (assignment)
  "Return the file path for ASSIGNMENT under the submissions directory.
The name is the assignment's sanitised as grading files are, with
\" (peer reviews)\" appended so it never collides with the grading
file."
  (expand-file-name
   (format "%s (peer reviews).org"
           (org-canvas--submissions-sanitize-filename
            (or (alist-get 'name assignment) "assignment")))
   (org-canvas--submissions-ensure-directory)))

(defun org-canvas--peer-reviews-write (assignment rows student-rows)
  "Write ROWS and STUDENT-ROWS for ASSIGNMENT to its file; return the buffer.
The buffer is left read-only: the tables are derived from Canvas and a
pull rewrites them."
  (let ((file (org-canvas--peer-reviews-file-path assignment)))
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (with-current-buffer (org-canvas--find-file-noselect file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-canvas--peer-reviews-insert assignment rows student-rows)
        (org-canvas--save-buffer))
      (setq buffer-read-only t)
      (current-buffer))))

;;;; Entry Point

(defun org-canvas--peer-reviews-choose (assignments)
  "Ask which of ASSIGNMENTS to pull and return it."
  (unless assignments
    (user-error "No assignment in this course has peer reviews enabled"))
  (let* ((names (mapcar (lambda (a) (alist-get 'name a)) assignments))
         (chosen (completing-read "Assignment: " names nil t)))
    (cl-find-if (lambda (a) (equal (alist-get 'name a) chosen)) assignments)))

(defun org-canvas--peer-reviews-pull (assignment)
  "Pull ASSIGNMENT's peer reviews into its file and return the buffer."
  (let* ((students (org-canvas--peer-reviews-fetch-students))
         (rows (org-canvas--peer-reviews-rows
                (org-canvas--peer-reviews-fetch (alist-get 'id assignment))
                students))
         (student-rows (org-canvas--peer-reviews-student-rows rows students))
         (summary (org-canvas--peer-reviews-summary rows student-rows))
         (buffer (org-canvas--peer-reviews-write assignment rows student-rows)))
    (org-canvas--log-info org-canvas--logger
      "Peer reviews pulled for '%s': %s" (alist-get 'name assignment) summary)
    (message "Peer reviews for %s: %s" (alist-get 'name assignment) summary)
    buffer))

;;;###autoload
(defun org-canvas-pull-peer-reviews ()
  "Select an assignment with peer reviews and pull who reviews whom.
The file is written under the submissions directory as
<assignment> (peer reviews).org: one row per review with the reviewer,
the student reviewed, the state and when it was completed, and one
row per active student with the reviewers assigned and the reviews
received.  Nothing is pushed from it; reviewers are assigned in the
Canvas web UI."
  (interactive)
  (let* ((assignment (org-canvas--peer-reviews-choose
                      (org-canvas--peer-reviews-fetch-assignments)))
         (buffer (org-canvas--peer-reviews-pull assignment)))
    (unless noninteractive
      (pop-to-buffer buffer))))

(provide 'org-canvas-peer-reviews)
;;; org-canvas-peer-reviews.el ends here
