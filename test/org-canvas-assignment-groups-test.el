;;; org-canvas-assignment-groups-test.el --- Buttercup tests for assignment groups  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-assignment-groups)

;;;; Transform (pure, no buffer)

(describe "org-canvas--assignment-group-transform-props"
  (it "strips statistics cookie from name"
    (let ((result (org-canvas--assignment-group-transform-props
                   '(:title-raw "Homework [1/3]" :canvas-id nil
                     :weight-raw "30" :drop-lowest-raw nil :drop-highest-raw nil
                     :never-drop-raw nil :position-raw nil))))
      (expect (plist-get result :name) :to-equal "Homework")))

  (it "converts weight to number"
    (let ((result (org-canvas--assignment-group-transform-props
                   '(:title-raw "Labs" :canvas-id nil
                     :weight-raw "25" :drop-lowest-raw nil :drop-highest-raw nil
                     :never-drop-raw nil :position-raw nil))))
      (expect (plist-get result :group_weight) :to-equal 25)))

  (it "defaults weight to 0.0 when absent"
    (let ((result (org-canvas--assignment-group-transform-props
                   '(:title-raw "Labs" :canvas-id nil
                     :weight-raw nil :drop-lowest-raw nil :drop-highest-raw nil
                     :never-drop-raw nil :position-raw nil))))
      (expect (plist-get result :group_weight) :to-equal 0.0)))

  (it "builds drop rules"
    (let ((result (org-canvas--assignment-group-transform-props
                   '(:title-raw "Quizzes" :canvas-id nil
                     :weight-raw "20" :drop-lowest-raw "2" :drop-highest-raw nil
                     :never-drop-raw nil :position-raw nil))))
      (expect (alist-get 'drop_lowest (plist-get result :rules)) :to-equal 2)))

  (it "parses never-drop comma list"
    (let ((result (org-canvas--assignment-group-transform-props
                   '(:title-raw "Quizzes" :canvas-id nil
                     :weight-raw "20" :drop-lowest-raw nil :drop-highest-raw nil
                     :never-drop-raw "101,102,103" :position-raw nil))))
      (expect (alist-get 'never_drop (plist-get result :rules))
              :to-equal '(101 102 103))))

  (it "converts position to number"
    (let ((result (org-canvas--assignment-group-transform-props
                   '(:title-raw "Labs" :canvas-id nil
                     :weight-raw "30" :drop-lowest-raw nil :drop-highest-raw nil
                     :never-drop-raw nil :position-raw "3"))))
      (expect (plist-get result :position) :to-equal 3))))

;;;; Stage 1: Parse Entry

(describe "org-canvas--assignment-group-parse-entry"
  (it "extracts name from heading"
    (with-temp-org-buffer
     "* Config
** Homework
:PROPERTIES:
:WEIGHT: 30
:END:
"
     (search-forward "Homework")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :name) :to-equal "Homework"))))

  (test-org-canvas-define-common-parse-tests
   #'org-canvas--assignment-group-parse-entry)

  (it "parses group_weight"
    (with-temp-org-buffer
     "* Config
** Projects
:PROPERTIES:
:WEIGHT: 25
:END:
"
     (search-forward "Projects")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       ;; string-to-number returns integer for "25"
       (expect (plist-get data :group_weight) :to-equal 25))))

  (it "defaults group_weight to 0"
    (with-temp-org-buffer
     "* Config
** Group
:PROPERTIES:
:END:
"
     (search-forward "Group")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :group_weight) :to-equal 0.0))))

  (it "parses drop_lowest rule"
    (with-temp-org-buffer
     "* Config
** Quizzes
:PROPERTIES:
:WEIGHT: 20
:DROP_LOWEST: 2
:END:
"
     (search-forward "Quizzes")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :rules) :to-be-truthy)
       (expect (alist-get 'drop_lowest (plist-get data :rules)) :to-equal 2))))

  (it "parses drop_highest rule"
    (with-temp-org-buffer
     "* Config
** Labs
:PROPERTIES:
:WEIGHT: 15
:DROP_HIGHEST: 1
:END:
"
     (search-forward "Labs")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (alist-get 'drop_highest (plist-get data :rules)) :to-equal 1))))

  (it "parses POSITION as number"
    (with-temp-org-buffer
     "* Config
** Homework
:PROPERTIES:
:WEIGHT: 30
:POSITION: 2
:END:
"
     (search-forward "Homework")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :position) :to-equal 2))))

  (it "returns nil for position when not set"
    (with-temp-org-buffer
     "* Config
** Group
:PROPERTIES:
:WEIGHT: 10
:END:
"
     (search-forward "Group")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (plist-get data :position) :to-be nil))))

)

;;;; Stage 2: Build Payload

(describe "org-canvas--assignment-group-build-payload"
  (it "includes name in payload"
    (let* ((data '(:name "Homework" :group_weight 30.0 :rules nil :canvas-id nil))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'name payload) :to-equal "Homework")))

  (it "includes group_weight"
    (let* ((data '(:name "Test" :group_weight 25.0 :rules nil :canvas-id nil))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'group_weight payload) :to-equal 25.0)))

  (it "excludes rules for new groups (POST)"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules ((drop_lowest . 1))))
           (payload (org-canvas--assignment-group-build-payload data)))
      ;; Rules should NOT be included for new groups (canvas-id is nil)
      (expect (alist-get 'rules payload) :to-be nil)))

  (it "includes rules for existing groups (PUT)"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id "123"
                   :rules ((drop_lowest . 2))))
           (payload (org-canvas--assignment-group-build-payload data)))
      ;; Rules should be included for updates (canvas-id is present)
      (expect (alist-get 'rules payload) :to-be-truthy)
      (expect (alist-get 'drop_lowest (alist-get 'rules payload)) :to-equal 2)))

  (it "includes position in payload when set"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules nil :position 3))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'position payload) :to-equal 3)))

  (it "excludes position from payload when nil"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules nil :position nil))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'position payload) :to-be nil))))

;;;; Stage 3: Push to API (mocked)

(describe "assignment-group push-to-api (mocked)"
  (it "uses POST for new groups"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "New" :canvas-id nil))
              (payload '((name . "New"))))
          (org-canvas--push-to-api data payload
            :endpoint "assignment_groups" :title-key :name)
          (expect-api-called 'POST "assignment_groups$")))))

  (it "uses PUT for existing groups"
    (with-org-canvas-test-config
      (with-mock-api
        (let ((data '(:name "Existing" :canvas-id "321"))
              (payload '((name . "Existing"))))
          (org-canvas--push-to-api data payload
            :endpoint "assignment_groups" :title-key :name)
          (expect-api-called 'PUT "assignment_groups/321")))))

  (it "sends a POST body reflecting the org input (real pipeline)"
    (let ((temp-dir (make-temp-file "ag-test" t)))
      (unwind-protect
          (let ((org-file (expand-file-name "assignment-groups.org" temp-dir)))
            (with-temp-file org-file
              (insert "* Config\n** Homework\n:PROPERTIES:\n:WEIGHT: 30\n:END:\n"))
            (let ((org-canvas-assignment-groups-file org-file)
                  (org-canvas-base-url "https://test.canvas.example.com")
                  (org-canvas-api-token "test-token")
                  (org-canvas-course-id "99999"))
              (with-sync-test-env
                (with-mock-api
                  (org-canvas-sync-assignment-groups)
                  (let ((body (test-org-canvas-api-call-data 'POST "assignment_groups")))
                    (expect (alist-get 'name body) :to-equal "Homework")
                    (expect (alist-get 'group_weight body) :to-equal 30)
                    ;; Drop rules are excluded on POST (two-phase sync)
                    (expect (alist-get 'rules body) :to-be nil))))))
        (delete-directory temp-dir t)))))

;;;; Stage 4: Finalize

(describe "assignment-group finalize"
  (it "saves CANVAS_ID from response"
    (with-temp-org-buffer
     "* Config
** Test Group
:PROPERTIES:
:WEIGHT: 20
:END:
"
     (search-forward "Test Group")
     (org-back-to-heading)
     (let ((data (list :name "Test Group" :pom (point-marker)))
           (response '((id . 55555) (name . "Test Group"))))
       (org-canvas--finalize-item data response :title-key :name)
       (expect (org-entry-get (point) "CANVAS_ID") :to-equal "55555"))))

  (it "does not write per-entry LAST_SYNCED (file-level header instead)"
    (with-temp-org-buffer
     "* Config
** Test
:PROPERTIES:
:WEIGHT: 15
:END:
"
     (search-forward "Test")
     (org-back-to-heading)
     (let ((data (list :name "Test" :pom (point-marker)))
           (response '((id . 44444))))
       (org-canvas--finalize-item data response :title-key :name)
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)))))

;;;; Pull Function Tests

(describe "org-canvas--assignment-group-pull-item"
  (it "sets WEIGHT property"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 25))
      (point))
     (expect (org-entry-get (point) "WEIGHT") :to-equal "25")))

  (it "sets DROP_LOWEST and DROP_HIGHEST from rules"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 10)
        (rules . ((drop_lowest . 2) (drop_highest . 1))))
      (point))
     (expect (org-entry-get (point) "DROP_LOWEST") :to-equal "2")
     (expect (org-entry-get (point) "DROP_HIGHEST") :to-equal "1")))

  (it "skips zero drop rules"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 50)
        (rules . ((drop_lowest . 0) (drop_highest . 0))))
      (point))
     (expect (org-entry-get (point) "DROP_LOWEST") :to-be nil)
     (expect (org-entry-get (point) "DROP_HIGHEST") :to-be nil)))

  (it "sets POSITION property on pull"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30) (position . 2))
      (point))
     (expect (org-entry-get (point) "POSITION") :to-equal "2")))

  (it "does not set POSITION when nil in response"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30))
      (point))
     (expect (org-entry-get (point) "POSITION") :to-be nil))))

;;;; New Property Tests: NEVER_DROP

(describe "org-canvas--assignment-group-parse-entry (never_drop)"
  (it "parses NEVER_DROP into rules"
    (with-temp-org-buffer
     "* Config
** Protected
:PROPERTIES:
:WEIGHT: 30
:NEVER_DROP: 123,456,789
:END:
"
     (search-forward "Protected")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (alist-get 'never_drop (plist-get data :rules))
               :to-equal '(123 456 789)))))

  (it "does not include never_drop when absent"
    (with-temp-org-buffer
     "* Config
** Simple
:PROPERTIES:
:WEIGHT: 20
:END:
"
     (search-forward "Simple")
     (org-back-to-heading)
     (let ((data (org-canvas--assignment-group-parse-entry)))
       (expect (alist-get 'never_drop (plist-get data :rules)) :to-be nil)))))

(describe "org-canvas--assignment-group-build-payload (never_drop)"
  (it "includes never_drop in rules for updates"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id "123"
                   :rules ((drop_lowest . 1) (never_drop . (100 200)))))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'never_drop (alist-get 'rules payload))
              :to-equal '(100 200))))

  (it "excludes never_drop rules for new groups"
    (let* ((data '(:name "Test" :group_weight 20.0 :canvas-id nil
                   :rules ((never_drop . (100 200)))))
           (payload (org-canvas--assignment-group-build-payload data)))
      (expect (alist-get 'rules payload) :to-be nil))))

(describe "org-canvas--assignment-group-pull-item (never_drop)"
  (it "sets NEVER_DROP property from rules"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30)
        (rules . ((never_drop . [100 200 300]))))
      (point))
     (expect (org-entry-get (point) "NEVER_DROP") :to-equal "100,200,300")))

  (it "does not set NEVER_DROP when empty"
    (with-temp-org-buffer
     "* Group
:PROPERTIES:
:CANVAS_ID: 1
:END:
"
     (org-back-to-heading)
     (org-canvas--assignment-group-pull-item
      '((id . 1) (name . "Group") (group_weight . 30)
        (rules . ((never_drop . []))))
      (point))
     (expect (org-entry-get (point) "NEVER_DROP") :to-be nil))))

;;;; Delete at Point

(describe "org-canvas-delete-assignment-group-at-point"
  (it "deletes assignment group and clears properties"
    (with-org-canvas-test-config
      (with-mock-api
        (with-temp-org-buffer
         "* Test
:PROPERTIES:
:CANVAS_ID: 42
:LAST_SYNCED: [2024-01-01 Mon]
:END:
"
         (org-back-to-heading)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (org-canvas-delete-assignment-group-at-point)
           (expect-api-called 'DELETE "assignment_groups/42")
           (expect (org-entry-get (point) "CANVAS_ID") :to-be nil)
           (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil))))))

  (it "errors when no CANVAS_ID"
    (with-temp-org-buffer
     "* New
:PROPERTIES:
:END:
"
     (org-back-to-heading)
     (expect (org-canvas-delete-assignment-group-at-point) :to-throw 'user-error))))

;;;; Unmanaged group reconciliation (issue #37)
;;
;; A Canvas group absent from assignment-groups.org is invisible to
;; org-canvas.  With weighted grading on and the managed groups already
;; summing to 100, an assignment landing in it is worth exactly nothing
;; and nothing says so.  Detecting it needs remote state, so this runs as
;; the sync's :after-sync hook rather than in the offline validator.

(defmacro with-assignment-groups-file (content &rest body)
  "Bind `org-canvas-assignment-groups-file' to a temp file with CONTENT."
  (declare (indent 1))
  `(let* ((temp-file (make-temp-file "ag-" nil ".org"))
          (org-canvas-assignment-groups-file temp-file))
     (unwind-protect
         (progn
           (with-temp-file temp-file (insert ,content))
           ,@body)
       (let ((buf (find-buffer-visiting temp-file)))
         (when buf
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-file temp-file))))

(defun test-ag--remote (&rest groups)
  "Return a stub `org-canvas-api-request-all-pages' yielding GROUPS."
  (lambda (&rest _) groups))

(describe "org-canvas--assignment-group-local-ids"
  (it "collects the CANVAS_IDs recorded in the file"
    (with-assignment-groups-file
        "* Assignment Groups
** Essays
:PROPERTIES:
:CANVAS_ID: 111
:WEIGHT:   60.0
:END:
** Exams
:PROPERTIES:
:CANVAS_ID: 222
:WEIGHT:   40.0
:END:
"
      (expect (org-canvas--assignment-group-local-ids) :to-equal '("111" "222"))))

  (it "returns nil for a file with no synced groups"
    (with-assignment-groups-file "* Assignment Groups\n** Essays\n"
      (expect (org-canvas--assignment-group-local-ids) :to-be nil))))

(describe "org-canvas--assignment-group-reconcile-unmanaged"
  (it "reports an unmanaged group holding assignments as an error"
    ;; The live case: those assignments are silent zeros.
    (with-org-canvas-test-config
      (with-assignment-groups-file
          "* Assignment Groups
** Essays
:PROPERTIES:
:CANVAS_ID: 111
:END:
"
        (let ((errors nil))
          (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                     (test-ag--remote
                      '((id . 111) (name . "Essays") (assignments . []))
                      '((id . 677142) (name . "Assignments")
                        (assignments . [((id . 1)) ((id . 2))]))))
                    ((symbol-function 'org-canvas--log-error)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) errors)))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (let ((unmanaged (org-canvas--assignment-group-reconcile-unmanaged)))
              (expect (length unmanaged) :to-equal 1)
              (expect (alist-get 'id (car unmanaged)) :to-equal 677142)
              (expect (length errors) :to-equal 1)
              (expect (car errors) :to-match "Assignments")
              (expect (car errors) :to-match "2 assignment")))))))

  (it "reports an empty unmanaged group as a warning, not an error"
    ;; The trap is armed but has not sprung: nothing is mis-weighted yet.
    (with-org-canvas-test-config
      (with-assignment-groups-file
          "* Assignment Groups
** Essays
:PROPERTIES:
:CANVAS_ID: 111
:END:
"
        (let ((errors nil) (warnings nil))
          (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                     (test-ag--remote
                      '((id . 111) (name . "Essays") (assignments . []))
                      '((id . 677142) (name . "Assignments") (assignments . []))))
                    ((symbol-function 'org-canvas--log-error)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) errors)))
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings)))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (org-canvas--assignment-group-reconcile-unmanaged)
            (expect errors :to-be nil)
            (expect (length warnings) :to-equal 1)
            (expect (car warnings) :to-match "empty now"))))))

  (it "says nothing when every remote group is managed"
    (with-org-canvas-test-config
      (with-assignment-groups-file
          "* Assignment Groups
** Essays
:PROPERTIES:
:CANVAS_ID: 111
:END:
"
        (let ((logged nil))
          (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                     (test-ag--remote
                      '((id . 111) (name . "Essays") (assignments . []))))
                    ((symbol-function 'org-canvas--log-error)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged)))
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) logged))))
            (expect (org-canvas--assignment-group-reconcile-unmanaged) :to-be nil)
            (expect logged :to-be nil))))))

  (it "asks Canvas for assignments so it can grade the severity"
    (with-org-canvas-test-config
      (with-assignment-groups-file "* Assignment Groups\n"
        (let ((params nil))
          (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                     (lambda (_method _url &optional p) (setq params p) nil)))
            (org-canvas--assignment-group-reconcile-unmanaged)
            (expect params :to-equal '(("include[]" . "assignments"))))))))

  (it "never deletes an unmanaged group"
    ;; Explicitly requested in #37: Canvas requires at least one group,
    ;; and silently removing something made in the web UI is worse than
    ;; the trap it warns about.
    (with-org-canvas-test-config
      (with-assignment-groups-file "* Assignment Groups\n"
        (let ((calls nil))
          (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                     (test-ag--remote
                      '((id . 677142) (name . "Assignments") (assignments . []))))
                    ((symbol-function 'org-canvas-api-request)
                     (lambda (method url &rest _) (push (cons method url) calls) nil))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (org-canvas--assignment-group-reconcile-unmanaged)
            (expect calls :to-equal nil))))))

  (it "degrades to a warning when the fetch fails, without signaling"
    ;; A reconciliation problem must not fail a sync that otherwise worked.
    (with-org-canvas-test-config
      (with-assignment-groups-file "* Assignment Groups\n"
        (let ((warnings nil))
          (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                     (lambda (&rest _)
                       (org-canvas--signal 'org-canvas-api-error "network down")))
                    ((symbol-function 'org-canvas--log-warning)
                     (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
            (expect (org-canvas--assignment-group-reconcile-unmanaged) :not :to-throw)
            (expect (car warnings) :to-match "Could not check"))))))

  (it "treats a group as managed by CANVAS_ID, not by name"
    ;; A local heading renamed by the user still manages its remote group.
    (with-org-canvas-test-config
      (with-assignment-groups-file
          "* Assignment Groups
** Renamed Locally
:PROPERTIES:
:CANVAS_ID: 111
:END:
"
        (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                   (test-ag--remote
                    '((id . 111) (name . "Original Name") (assignments . [])))))
          (expect (org-canvas--assignment-group-reconcile-unmanaged) :to-be nil))))))

;;; org-canvas-assignment-groups-test.el ends here
