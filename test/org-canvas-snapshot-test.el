;;; org-canvas-snapshot-test.el --- Snapshot tests for payload functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Snapshot (approval) tests for all build-payload functions.
;; Serializes payloads to normalized JSON and compares against
;; approved baseline files in test/snapshots/.
;;
;; To update snapshots after intentional payload changes:
;;   UPDATE_SNAPSHOTS=1 eldev test "snapshot"
;;
;; Snapshot files are checked into version control so changes
;; show up in diffs.

;;; Code:

(require 'test-helper)
(require 'json)
(require 'org-canvas-announcements)
(require 'org-canvas-pages)
(require 'org-canvas-discussions)
(require 'org-canvas-assignments)
(require 'org-canvas-assignment-groups)
(require 'org-canvas-rubrics)
(require 'org-canvas-calendar)
(require 'org-canvas-group-categories)
(require 'org-canvas-settings)
(require 'org-canvas-quizzes)
(require 'org-canvas-new-quizzes)
(require 'org-canvas-modules)
(require 'org-canvas-outcomes)
(require 'org-canvas-sections)

;;;; Snapshot Infrastructure

(defvar test-snapshot-dir
  (expand-file-name "snapshots" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory for snapshot JSON files.")

(defun test-snapshot--normalize (obj)
  "Recursively normalize OBJ for deterministic JSON output.
Converts hash-tables to sorted alists and strips markers."
  (cond
   ((hash-table-p obj)
    (let ((pairs nil))
      (maphash (lambda (k v)
                 (push (cons (if (symbolp k) (symbol-name k) k)
                             (test-snapshot--normalize v))
                       pairs))
               obj)
      (sort pairs (lambda (a b) (string< (car a) (car b))))))
   ((markerp obj) "<marker>")
   ((vectorp obj)
    (vconcat (mapcar #'test-snapshot--normalize obj)))
   ((consp obj)
    (if (and (car obj) (not (consp (car obj))) (not (consp (cdr obj))))
        ;; Dotted pair
        (cons (test-snapshot--normalize (car obj))
              (test-snapshot--normalize (cdr obj)))
      ;; Regular list or alist
      (mapcar #'test-snapshot--normalize obj)))
   (t obj)))

(defun test-snapshot--to-json (payload)
  "Convert PAYLOAD to pretty-printed, deterministic JSON string."
  (let ((json-encoding-pretty-print t)
        (json-encoding-default-indentation "  ")
        (normalized (test-snapshot--normalize payload)))
    (json-encode normalized)))

(defun test-snapshot-file (name)
  "Return the snapshot file path for NAME."
  (expand-file-name (concat name ".json") test-snapshot-dir))

(defun test-snapshot-compare (name payload)
  "Compare PAYLOAD against snapshot NAME.
If UPDATE_SNAPSHOTS is set, write the snapshot instead of comparing."
  (let ((json-str (test-snapshot--to-json payload))
        (snap-file (test-snapshot-file name)))
    (if (getenv "UPDATE_SNAPSHOTS")
        ;; Update mode: write the snapshot
        (progn
          (unless (file-directory-p test-snapshot-dir)
            (make-directory test-snapshot-dir t))
          (with-temp-file snap-file
            (insert json-str)
            (insert "\n")))
      ;; Compare mode: read and diff
      (unless (file-exists-p snap-file)
        (error "Snapshot '%s' not found. Run with UPDATE_SNAPSHOTS=1 to create" name))
      (let ((expected (with-temp-buffer
                        (insert-file-contents snap-file)
                        (string-trim (buffer-string)))))
        (expect json-str :to-equal expected)))))

;;;; Test Data Builders
;; Pure data plists that mirror what parse-entry would produce.

(defun test-snapshot--announcement-data ()
  "Build test data for announcement payload."
  (list :title "Week 3 Update"
        :message "<p>Remember to submit by Friday.</p>"
        :published t
        :delayed_post_at "2026-04-01T09:00:00Z"
        :allow_discussion_comments nil
        :specific_sections "Section A"))

(defun test-snapshot--page-data ()
  "Build test data for page payload."
  (list :title "Course Syllabus"
        :body "<h1>Syllabus</h1><p>Welcome to the course.</p>"
        :published t
        :front_page nil
        :editing_roles "teachers"
        :student_todo_at nil
        :notify_of_update nil
        :canvas-url "course-syllabus"))

(defun test-snapshot--discussion-data ()
  "Build test data for discussion payload."
  (list :title "Week 1 Discussion"
        :message "<p>Introduce yourself.</p>"
        :published t
        :discussion_type "threaded"
        :require_initial_post t
        :pinned nil
        :delayed_post_at nil
        :allow_rating nil
        :only_graders_can_rate nil
        :sort_by_rating nil
        :group_category_id nil
        :specific_sections nil
        :lock_at nil
        :points_possible nil
        :grading_type nil
        :due_at nil
        :assignment_group_id nil))

(defun test-snapshot--assignment-data ()
  "Build test data for assignment payload."
  (list :title "Essay 1"
        :description "<p>Write a 5-page essay.</p>"
        :published t
        :submission_types "online_upload"
        :points_possible 100
        :grading_type "points"
        :due_at "2026-04-15T23:59:00Z"
        :unlock_at "2026-04-01T00:00:00Z"
        :lock_at "2026-04-16T23:59:00Z"
        :allowed_extensions "pdf,docx"
        :allowed_attempts nil
        :assignment_group_id 42
        :peer_reviews nil
        :automatic_peer_reviews nil
        :peer_review_count nil
        :peer_reviews_due_at nil
        :omit_from_final_grade nil
        :anonymous_grading nil
        :notify_of_update nil
        :group_category_id nil
        :grade_group_students_individually nil
        :only_visible_to_overrides nil
        :moderated_grading nil
        :grader_count nil
        :muted nil
        :turnitin_enabled nil
        :grading_standard_id nil
        :position nil
        :canvas-id "555"))

(defun test-snapshot--assignment-group-data ()
  "Build test data for assignment group payload."
  (list :name "Homework"
        :canvas-id "10"
        :group_weight 30
        :rules '((drop_lowest . 1))
        :position 1))

(defun test-snapshot--rubric-data ()
  "Build test data for rubric payload."
  (list :title "Essay Rubric"
        :free-form nil
        :criteria (list (list :description "Thesis"
                              :long_description "Clear thesis statement"
                              :points 25
                              :outcome-id nil
                              :ratings (list (list :description "Excellent"
                                                   :points 25)
                                             (list :description "Good"
                                                   :points 20)
                                             (list :description "Poor"
                                                   :points 5))))))

(defun test-snapshot--calendar-data ()
  "Build test data for calendar event payload."
  (list :title "Office Hours"
        :start_at "2026-04-01T14:00:00Z"
        :end_at "2026-04-01T15:00:00Z"
        :all_day nil
        :description "<p>Drop by my office.</p>"
        :location_name "Room 301"
        :location_address "Science Building"))

(defun test-snapshot--group-category-data ()
  "Build test data for group category payload."
  (list :title "Lab Groups"
        :self_signup "enabled"
        :group_limit 4
        :auto_leader "first"
        :create_group_count 8))

(defun test-snapshot--override-data ()
  "Build test data for override payload."
  (list :section-id "101"
        :due-at "2026-04-20T23:59:00Z"
        :unlock-at "2026-04-01T00:00:00Z"
        :lock-at "2026-04-21T23:59:00Z"))

(defun test-snapshot--question-data ()
  "Build test data for quiz question payload."
  (list :name "What is 2+2?"
        :text-html "<p>What is 2+2?</p>"
        :question_type "multiple_choice_question"
        :points_possible 5
        :answers '(((answer_text . "4") (answer_weight . 100))
                   ((answer_text . "5") (answer_weight . 0))
                   ((answer_text . "3") (answer_weight . 0)))))

(defun test-snapshot--quiz-data ()
  "Build test data for quiz payload."
  (list :title "Midterm Quiz"
        :published nil
        :quiz_type "assignment"
        :time_limit 60
        :shuffle_answers t
        :show_correct_answers t
        :show_correct_answers_at nil
        :hide_correct_answers_at nil
        :allowed_attempts 2
        :scoring_policy "keep_highest"
        :one_question_at_a_time nil
        :cant_go_back nil
        :access_code nil
        :ip_filter nil
        :one_time_results nil
        :lock_at nil
        :unlock_at nil
        :due_at "2026-04-15T23:59:00Z"
        :description "<p>Good luck!</p>"
        :assignment_group_id nil))

(defun test-snapshot--new-quiz-item-data ()
  "Build test data for new quiz item payload.
Must be called inside cl-letf that mocks UUID generation."
  (let ((interaction-data
         (org-canvas--new-quiz-item-build-choice-data
          '(("Paris" . t) ("London" . nil) ("Berlin" . nil)))))
    (list :title "Capital of France?"
          :text ""
          :type "choice"
          :points 5
          :text-html "<p>Capital of France?</p>"
          :interaction-data interaction-data)))

(defun test-snapshot--module-data ()
  "Build test data for module payload."
  (list :title "Week 1: Introduction"
        :published t
        :position 1
        :prerequisite-ids nil
        :require-sequential nil
        :requirement-count nil))

(defun test-snapshot--outcome-data ()
  "Build test data for outcome payload."
  (list :title "Critical Thinking"
        :description "Students will demonstrate critical analysis skills."
        :calculation_method "decaying_average"
        :calculation_int 65
        :mastery_points 3
        :ratings '(((description . "Exceeds") (points . 4))
                   ((description . "Meets") (points . 3))
                   ((description . "Developing") (points . 2))
                   ((description . "Below") (points . 1)))))

(defun test-snapshot--outcome-group-data ()
  "Build test data for outcome group payload."
  (list :title "Program Outcomes"))

;;;; Deterministic Mocks

(defvar test-snapshot--uuid-counter 0
  "Counter for deterministic UUID generation in snapshot tests.")

(defun test-snapshot--deterministic-uuid ()
  "Generate a deterministic UUID for snapshot reproducibility."
  (cl-incf test-snapshot--uuid-counter)
  (format "%032x" test-snapshot--uuid-counter))

(defun test-snapshot--deterministic-numeric-id ()
  "Generate a deterministic numeric ID for snapshot reproducibility."
  (cl-incf test-snapshot--uuid-counter)
  test-snapshot--uuid-counter)

;;;; Snapshot Tests

(describe "Snapshot Tests"
  (before-each
    (setq test-snapshot--uuid-counter 0))

  (describe "announcement payload"
    (it "matches snapshot"
      (cl-letf (((symbol-function 'org-canvas-current-iso8601-timestamp)
                 (lambda () "2026-01-01T00:00:00Z")))
        (let* ((data (test-snapshot--announcement-data))
               (payload (org-canvas--announcement-build-payload data)))
          (test-snapshot-compare "announcement-payload" payload)))))

  (describe "page payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--page-data))
             (payload (org-canvas--page-build-payload data)))
        (test-snapshot-compare "page-payload" payload))))

  (describe "discussion payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--discussion-data))
             (payload (org-canvas--discussion-build-payload data)))
        (test-snapshot-compare "discussion-payload" payload))))

  (describe "assignment payload"
    (it "matches snapshot"
      (with-org-canvas-test-config
        (let* ((data (test-snapshot--assignment-data))
               (payload (org-canvas--assignment-build-payload data)))
          (test-snapshot-compare "assignment-payload" payload)))))

  (describe "assignment-group payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--assignment-group-data))
             (payload (org-canvas--assignment-group-build-payload data)))
        (test-snapshot-compare "assignment-group-payload" payload))))

  (describe "rubric payload"
    (it "matches snapshot"
      (with-org-canvas-test-config
        (let* ((data (test-snapshot--rubric-data))
               (payload (org-canvas--rubric-build-payload data)))
          (test-snapshot-compare "rubric-payload" payload)))))

  (describe "calendar-event payload"
    (it "matches snapshot"
      (with-org-canvas-test-config
        (let* ((data (test-snapshot--calendar-data))
               (payload (org-canvas--calendar-event-build-payload data)))
          (test-snapshot-compare "calendar-event-payload" payload)))))

  (describe "group-category payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--group-category-data))
             (payload (org-canvas--group-category-build-payload data)))
        (test-snapshot-compare "group-category-payload" payload))))

  (describe "override payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--override-data))
             (payload (org-canvas--override-build-payload data)))
        (test-snapshot-compare "override-payload" payload))))

  (describe "question payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--question-data))
             (payload (org-canvas--question-build-payload data)))
        (test-snapshot-compare "question-payload" payload))))

  (describe "quiz payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--quiz-data))
             (payload (org-canvas--quiz-build-payload data)))
        (test-snapshot-compare "quiz-payload" payload))))

  (describe "new-quiz-item payload"
    (it "matches snapshot"
      (cl-letf (((symbol-function 'org-canvas--new-quiz-uuid)
                 #'test-snapshot--deterministic-uuid)
                ((symbol-function 'org-canvas--new-quiz-numeric-id)
                 #'test-snapshot--deterministic-numeric-id))
        ;; Data builder must be inside cl-letf since it calls build-choice-data
        (let* ((data (test-snapshot--new-quiz-item-data))
               (payload (org-canvas--new-quiz-item-build-payload data)))
          (test-snapshot-compare "new-quiz-item-payload" payload)))))

  (describe "module payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--module-data))
             (payload (org-canvas--module-build-payload data)))
        (test-snapshot-compare "module-payload" payload))))

  (describe "outcome payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--outcome-data))
             (payload (org-canvas--outcome-build-payload data)))
        (test-snapshot-compare "outcome-payload" payload))))

  (describe "outcome-group payload"
    (it "matches snapshot"
      (let* ((data (test-snapshot--outcome-group-data))
             (payload (org-canvas--outcome-group-build-payload data)))
        (test-snapshot-compare "outcome-group-payload" payload)))))

(provide 'org-canvas-snapshot-test)

;;; org-canvas-snapshot-test.el ends here
