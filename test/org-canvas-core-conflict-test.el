;;; org-canvas-core-conflict-test.el --- Tests for org-canvas-core-conflict -*- lexical-binding: t; -*-

;;; Commentary:

;; Specs for `org-canvas-core-conflict'.

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

(describe "org-canvas--conflict-format-diff"
  (it "creates a buffer with conflict details"
    (let ((data (list :title "My Page" :description "local body text"
                      :pom nil))
          (remote '((title . "My Page Remote")
                    (updated_at . "2026-02-01T10:00:00Z")
                    (body . "remote body text")))
)
      (let ((buf (org-canvas--conflict-format-diff data remote t)))
        (unwind-protect
            (with-current-buffer buf
              (expect (buffer-string) :to-match "Conflict: My Page")
              (expect (buffer-string) :to-match "Remote updated_at:")
              (expect (buffer-string) :to-match "Title")
              (expect (buffer-string) :to-match "My Page Remote"))
          (when (buffer-live-p buf) (kill-buffer buf))))))

  (it "handles nil body gracefully"
    (let ((data (list :title "No Body" :pom nil))
          (remote '((title . "No Body") (updated_at . "2026-02-01T10:00:00Z")))
)
      (let ((buf (org-canvas--conflict-format-diff data remote)))
        (unwind-protect
            (with-current-buffer buf
              (expect (buffer-string) :to-match "Conflict: No Body"))
          (when (buffer-live-p buf) (kill-buffer buf))))))

  (it "shows pull option only when pull-item-fn is set"
    (let ((data (list :title "Item" :pom nil))
          (remote '((title . "Item") (updated_at . "2026-02-01T10:00:00Z"))))
      ;; With pull-item-fn
      (progn
        (let ((buf (org-canvas--conflict-format-diff data remote t)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :to-match "l = Pull"))
            (when (buffer-live-p buf) (kill-buffer buf)))))
      ;; Without pull-item-fn
      (progn
        (let ((buf (org-canvas--conflict-format-diff data remote)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :not :to-match "l = Pull"))
            (when (buffer-live-p buf) (kill-buffer buf)))))))

  (it "shows P/L/S when pull-item-fn is set, P/S when nil"
    (let ((data (list :title "Item" :pom nil))
          (remote '((title . "Item") (updated_at . "2026-02-01T10:00:00Z"))))
      ;; With pull-item-fn: should show P/L/S
      (progn
        (let ((buf (org-canvas--conflict-format-diff data remote t)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :to-match "P/L/S"))
            (when (buffer-live-p buf) (kill-buffer buf)))))
      ;; Without pull-item-fn: should show P/S
      (progn
        (let ((buf (org-canvas--conflict-format-diff data remote)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :to-match "P/S")
                (expect (buffer-string) :not :to-match "P/L/S"))
            (when (buffer-live-p buf) (kill-buffer buf))))))))

(describe "org-canvas--conflict-prompt"
  (it "returns push for p"
    ;; These simulate a human at the keyboard; under batch the prompt
    ;; short-circuits to skip rather than reading a key (issue #72).
    (let ((noninteractive nil))
     (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?p)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'push))))

  (it "returns pull for l"
    ;; These simulate a human at the keyboard; under batch the prompt
    ;; short-circuits to skip rather than reading a key (issue #72).
    (let ((noninteractive nil))
     (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?l)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'pull))))

  (it "returns skip for s"
    ;; These simulate a human at the keyboard; under batch the prompt
    ;; short-circuits to skip rather than reading a key (issue #72).
    (let ((noninteractive nil))
     (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?s)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'skip))))

  (it "returns push-all for P"
    ;; These simulate a human at the keyboard; under batch the prompt
    ;; short-circuits to skip rather than reading a key (issue #72).
    (let ((noninteractive nil))
     (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?P)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'push-all))))

  (it "returns pull-all for L"
    ;; These simulate a human at the keyboard; under batch the prompt
    ;; short-circuits to skip rather than reading a key (issue #72).
    (let ((noninteractive nil))
     (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?L)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'pull-all))))

  (it "returns skip-all for S"
    ;; These simulate a human at the keyboard; under batch the prompt
    ;; short-circuits to skip rather than reading a key (issue #72).
    (let ((noninteractive nil))
     (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?S)))
      (expect (org-canvas--conflict-prompt t) :to-equal 'skip-all)))))

(describe "org-canvas--resolve-conflict"
  (it "returns the run's apply-all answer immediately when set"
    (let ((ctx (org-canvas--sync-make-ctx :conflict-apply-all 'push)))
      (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")) ctx)
              :to-equal 'push)))

  (it "returns skip when apply-all is skip"
    (let ((ctx (org-canvas--sync-make-ctx :conflict-apply-all 'skip)))
      (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")) ctx)
              :to-equal 'skip)))

  (it "remembers push-all in the run context"
    (let ((ctx (org-canvas--sync-make-ctx))
          (noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'push-all)))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")) ctx)
                :to-equal 'push)
        (expect (plist-get ctx :conflict-apply-all) :to-equal 'push))))

  (it "remembers skip-all in the run context"
    (let ((ctx (org-canvas--sync-make-ctx))
          (noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'skip-all)))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")) ctx)
                :to-equal 'skip)
        (expect (plist-get ctx :conflict-apply-all) :to-equal 'skip))))

  (it "remembers pull-all in the run context"
    (let ((ctx (org-canvas--sync-make-ctx :pull-item-fn #'ignore))
          (noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'pull-all)))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")) ctx)
                :to-equal 'pull)
        (expect (plist-get ctx :conflict-apply-all) :to-equal 'pull))))

  (it "offers pull only when the run context names a pull function (issue #141)"
    (let ((seen nil)
          (noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (has-pull) (push has-pull seen) 'skip)))
        (org-canvas--resolve-conflict '(:title "X") '((title . "X"))
                                      (org-canvas--sync-make-ctx :pull-item-fn #'ignore))
        (org-canvas--resolve-conflict '(:title "X") '((title . "X"))
                                      (org-canvas--sync-make-ctx))
        (org-canvas--resolve-conflict '(:title "X") '((title . "X")) nil))
      (expect (nreverse seen) :to-equal '(t nil nil))))

  (it "kills the diff buffer after prompting"
    (let ((ctx (org-canvas--sync-make-ctx))
          (noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'push)))
        (org-canvas--resolve-conflict '(:title "X") '((title . "X")) ctx)
        (expect (get-buffer org-canvas--conflict-buffer-name) :to-be nil)))))

(describe "org-canvas--conflict-unattended-action"
  ;; Issue #72: a batch sync died reading a keystroke that cannot arrive.
  (it "takes skip under batch when nothing is configured"
    (let ((org-canvas-conflict-strategy nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
        (expect (org-canvas--conflict-unattended-action '(:title "X"))
                :to-equal 'skip))))

  (it "honours a configured strategy, batch or not"
    (let ((org-canvas-conflict-strategy 'push))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
        (expect (org-canvas--conflict-unattended-action '(:title "X"))
                :to-equal 'push)
        (let ((noninteractive nil))
          (expect (org-canvas--conflict-unattended-action '(:title "X"))
                  :to-equal 'push)))))

  (it "defers to the prompt when interactive and unconfigured"
    (let ((org-canvas-conflict-strategy nil)
          (noninteractive nil))
      (expect (org-canvas--conflict-unattended-action '(:title "X")) :to-be nil)))

  (it "names the entry and says why it was not asked about"
    (let ((org-canvas-conflict-strategy nil)
          (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--conflict-unattended-action '(:title "Lab 1"))
        (expect (car warnings) :to-match "'Lab 1'")
        (expect (car warnings) :to-match "batch mode")
        (expect (car warnings) :to-match "org-canvas-conflict-strategy"))))

  (it "names a file entry by its display name, and an anonymous one plainly"
    ;; Files carry :display-name rather than :title.
    (let ((org-canvas-conflict-strategy 'skip)
          (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--conflict-unattended-action '(:display-name "syllabus.pdf"))
        (expect (car warnings) :to-match "'syllabus.pdf'")
        (org-canvas--conflict-unattended-action nil)
        (expect (car warnings) :to-match "'entry'"))))

  (it "credits the setting when one is configured"
    (let ((org-canvas-conflict-strategy 'skip)
          (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (org-canvas--conflict-unattended-action '(:title "Lab 1"))
        (expect (car warnings) :to-match "(org-canvas-conflict-strategy)")))))

(describe "org-canvas--conflict-prompt under batch"
  (it "returns skip instead of reading a key that cannot arrive"
    ;; read-char-choice signals end-of-file in batch, taking the sync with it.
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (&rest _) (error "should not be called"))))
      (expect (org-canvas--conflict-prompt t) :to-equal 'skip))))

(describe "org-canvas--resolve-conflict unattended"
  (it "resolves without prompting under batch"
    (let ((org-canvas-conflict-strategy nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore)
                ((symbol-function 'org-canvas--conflict-format-diff)
                 (lambda (&rest _) (error "should not build a diff"))))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X"))
                                              (org-canvas--sync-make-ctx))
                :to-equal 'skip))))

  (it "follows the configured strategy ahead of the prompt"
    (let ((org-canvas-conflict-strategy 'push)
          (noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore)
                ((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (&rest _) (error "should not prompt"))))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X"))
                                              (org-canvas--sync-make-ctx))
                :to-equal 'push))))

  (it "still lets a run's apply-all answer win"
    ;; The per-run choice is more specific than the standing setting.
    (let ((org-canvas-conflict-strategy 'push))
      (expect (org-canvas--resolve-conflict
               '(:title "X") '((title . "X"))
               (org-canvas--sync-make-ctx :conflict-apply-all 'pull
                                          :pull-item-fn #'ignore))
              :to-equal 'pull)))

  (it "is the seam a caller has, since a run's answer dies with its context"
    ;; The reported dead end (issue #72) was a let of the old apply-all
    ;; variable losing to the pipeline's rebinding.  The defcustom is
    ;; read whenever the run context holds no answer of its own.
    (let ((org-canvas-conflict-strategy 'skip))
      (cl-letf (((symbol-function 'org-canvas--log-warning) #'ignore))
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X"))
                                              (org-canvas--sync-make-ctx))
                :to-equal 'skip)
        (expect (org-canvas--resolve-conflict '(:title "X") '((title . "X")) nil)
                :to-equal 'skip)))))

(describe "org-canvas--conflict-pull-local"
  (it "calls pull-item-fn and refreshes file-level LAST_SYNCED header"
    (with-temp-org-buffer
     "#+LAST_SYNCED: [2026-01-01 Thu 10:00]
* Old Title
:PROPERTIES:
:CANVAS_ID: 100
:PAYLOAD_HASH: abc123
:END:
"
     (re-search-forward "^\\* ")
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (data (list :title "Old Title" :pom pom))
            (remote '((title . "New Title")
                      (updated_at . "2026-02-01T12:00:00Z")
                      (message . "new body")))
            (pull-called nil))
       (org-canvas--conflict-pull-local data remote
         (lambda (_item _pos) (setq pull-called t)))
       (expect pull-called :to-be-truthy)
       ;; PAYLOAD_HASH should be deleted
       (expect (org-entry-get pom "PAYLOAD_HASH") :to-be nil)
       ;; CANVAS_UPDATED_AT should be set on the heading
       (expect (org-entry-get pom "CANVAS_UPDATED_AT")
               :to-equal "2026-02-01T12:00:00Z")
       ;; Per-entry LAST_SYNCED should not be written
       (expect (org-entry-get pom "LAST_SYNCED") :to-be nil)
       ;; File-level header should have been refreshed
       (let ((new-synced (org-canvas--pull-read-file-header)))
         (expect new-synced :to-be-truthy)
         (expect new-synced :not :to-equal "[2026-01-01 Thu 10:00]")))))

  (it "updates heading title when different"
    (with-temp-org-buffer
     "* Original Name
:PROPERTIES:
:CANVAS_ID: 200
:END:
"
     (org-back-to-heading)
     (let* ((pom (let ((m (point-marker)))
                   (set-marker-insertion-type m t)
                   m))
            (data (list :title "Original Name" :pom pom))
            (remote '((title . "Updated Name")
                      (updated_at . "2026-02-01T12:00:00Z"))))
       (org-canvas--conflict-pull-local data remote
         (lambda (_item _pos) nil))
       ;; pull-write-file-header may have inserted text at the top —
       ;; navigate by structure rather than by stale position
       (goto-char (point-min))
       (re-search-forward "^\\* " nil t)
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "Updated Name"))))

  (it "handles integer pom (non-marker)"
    (with-temp-org-buffer
     "* Integer POM Test
:PROPERTIES:
:CANVAS_ID: 300
:PAYLOAD_HASH: oldhash
:END:
"
     (org-back-to-heading)
     (let* ((pom (point))
            (data (list :title "Integer POM Test" :pom pom))
            (remote '((title . "Renamed via Integer")
                      (updated_at . "2026-03-01T09:00:00Z")))
            (pull-called nil))
       (org-canvas--conflict-pull-local data remote
         (lambda (_item _pos) (setq pull-called t)))
       (expect pull-called :to-be-truthy)
       ;; Re-locate the heading because pull-write-file-header may have
       ;; shifted positions when inserting the file-level header
       (goto-char (point-min))
       (re-search-forward "^\\* " nil t)
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "Renamed via Integer")
       (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-03-01T09:00:00Z")
       ;; File-level header gets refreshed
       (expect (org-canvas--pull-read-file-header) :to-be-truthy)))))

(describe "org-canvas--conflict-format-diff"
  (it "reads LAST_SYNCED from file-level header and uses name field for title"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-15 Thu 14:30]
* My Item
:PROPERTIES:
:CANVAS_ID: 123
:END:

Local body content.
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let ((data (list :title "My Item"
                         :description "Local body content."
                         :pom (point-marker)))
             (remote '((name . "Remote Item Name")
                       (updated_at . "2026-02-01T10:00:00Z")
                       (description . "Remote body."))))
         (let ((buf (org-canvas--conflict-format-diff data remote)))
           (unwind-protect
               (with-current-buffer buf
                 (let ((content (buffer-string)))
                   ;; Should contain the remote title from 'name field
                   (expect content :to-match "Remote Item Name")
                   ;; Should contain timestamps
                   (expect content :to-match "2026-01-15")
                   (expect content :to-match "2026-02-01")))
             (when (buffer-live-p buf)
               (kill-buffer buf)))))))))

(describe "org-canvas--conflict-pull-local"
  (it "renames heading and invokes pull-item-fn"
    (with-temp-org-buffer
     "* Old Title
:PROPERTIES:
:CANVAS_ID: 456
:END:

Old body.
"
     (org-back-to-heading)
     (let* ((pom (point-marker))
            (pull-called nil)
            (data (list :title "Old Title" :pom pom))
            (remote '((title . "New Remote Title")
                      (updated_at . "2026-02-10T08:00:00Z")
                      (body . "New body."))))
       (org-canvas--conflict-pull-local
        data remote
        (lambda (_response _pos)
          (setq pull-called t)))
       ;; Re-navigate by structure: pull-write-file-header may have
       ;; inserted text at the top, shifting positions
       (goto-char (point-min))
       (re-search-forward "^\\* " nil t)
       (org-back-to-heading)
       (expect (org-get-heading t t t t) :to-equal "New Remote Title")
       ;; pull-item-fn should have been called
       (expect pull-called :to-be-truthy)
       ;; Per-entry LAST_SYNCED should not be written
       (expect (org-entry-get (point) "LAST_SYNCED") :to-be nil)
       ;; File-level header should have been written
       (expect (org-canvas--pull-read-file-header) :to-match "^\\[20")
       (expect (org-entry-get (point) "CANVAS_UPDATED_AT")
               :to-equal "2026-02-10T08:00:00Z")
       ;; PAYLOAD_HASH should be deleted
       (expect (org-entry-get (point) "PAYLOAD_HASH") :to-be nil)))))

(describe "org-canvas-demo-conflict"
  (it "runs the demo and returns a resolution choice"
    (let ((demo-message nil))
      (cl-letf (((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'push))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq demo-message (apply #'format fmt args)))))
        (org-canvas-demo-conflict)
        (expect demo-message :to-match "push")))))

(describe "conflict batch flow"
  (it "auto-pushes second conflict after user chooses Push All on first"
    (with-org-canvas-test-config
      (let ((org-canvas-detect-conflicts t)
            ;; One run context shared by both pushes, as a sync would.
            (ctx (org-canvas--sync-make-ctx))
            ;; The flow under test is a human answering the prompt once;
            ;; batch mode resolves without one (issue #72).
            (noninteractive nil)
            (prompt-count 0)
            (put-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (pcase method
                       ;; GET always returns a "newer" item (conflict)
                       ('GET '((id . 1) (updated_at . "2026-03-01T10:00:00Z")))
                       ('PUT (setq put-count (1+ put-count))
                             '((id . 1))))))
                  ((symbol-function 'org-canvas--conflict-prompt)
                   (lambda (_has-pull)
                     (setq prompt-count (1+ prompt-count))
                     'push-all)))
          ;; First push: conflict detected, user prompted, chooses "Push All"
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item 1\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data1 (list :title "Item 1" :canvas-id "1" :pom (point-marker)))
                 (payload1 '((title . "Item 1"))))
             (org-canvas--push-to-api data1 payload1 :endpoint "items" :ctx ctx)
             ;; User was prompted once
             (expect prompt-count :to-equal 1)
             ;; PUT was called (force push)
             (expect put-count :to-equal 1)))
          ;; Second push: conflict detected, but apply-all is already 'push
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item 2\n:PROPERTIES:\n:CANVAS_ID: 2\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data2 (list :title "Item 2" :canvas-id "2" :pom (point-marker)))
                 (payload2 '((title . "Item 2"))))
             (org-canvas--push-to-api data2 payload2 :endpoint "items" :ctx ctx)
             ;; No additional prompt (apply-all active)
             (expect prompt-count :to-equal 1)
             ;; PUT was called again
             (expect put-count :to-equal 2)))))))

  (it "auto-skips second conflict after user chooses Skip All on first"
    (with-org-canvas-test-config
      (let ((org-canvas-detect-conflicts t)
            ;; One run context shared by both pushes, as a sync would.
            (ctx (org-canvas--sync-make-ctx))
            ;; The flow under test is a human answering the prompt once;
            ;; batch mode resolves without one (issue #72).
            (noninteractive nil)
            (prompt-count 0)
            (put-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (pcase method
                       ('GET '((id . 1) (updated_at . "2026-03-01T10:00:00Z")))
                       ('PUT (setq put-count (1+ put-count))
                             '((id . 1))))))
                  ((symbol-function 'org-canvas--conflict-prompt)
                   (lambda (_has-pull)
                     (setq prompt-count (1+ prompt-count))
                     'skip-all)))
          ;; First push: conflict, user chooses "Skip All"
          ;; push-to-api returns 'conflict for both skip and skip-all
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item A\n:PROPERTIES:\n:CANVAS_ID: 10\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data (list :title "Item A" :canvas-id "10" :pom (point-marker)))
                 (payload '((title . "Item A"))))
             (let ((result (org-canvas--push-to-api data payload :endpoint "items" :ctx ctx)))
               (expect result :to-equal 'conflict))))
          ;; Second push: auto-skipped without prompt
          (with-temp-org-buffer
           "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item B\n:PROPERTIES:\n:CANVAS_ID: 20\n:END:\n"
           (re-search-forward "^\\* ")
           (org-back-to-heading)
           (let ((data (list :title "Item B" :canvas-id "20" :pom (point-marker)))
                 (payload '((title . "Item B"))))
             (let ((result (org-canvas--push-to-api data payload :endpoint "items" :ctx ctx)))
               (expect result :to-equal 'conflict))))
          ;; Prompt was only shown once
          (expect prompt-count :to-equal 1)
          ;; No PUTs were sent
          (expect put-count :to-equal 0)))))

  (it "forgets a capital answer once its run context is gone (issue #141)"
    ;; Two pushes at point are two runs.  The old global kept the first
    ;; run's Push All alive, so every later push at point overwrote
    ;; conflicts without asking.
    (with-org-canvas-test-config
      (let ((org-canvas-detect-conflicts t)
            (noninteractive nil)
            (prompt-count 0))
        (cl-letf (((symbol-function 'org-canvas-api-request)
                   (lambda (method _url &rest _args)
                     (pcase method
                       ('GET '((id . 1) (updated_at . "2026-03-01T10:00:00Z")))
                       ('PUT '((id . 1))))))
                  ((symbol-function 'org-canvas--conflict-prompt)
                   (lambda (_has-pull)
                     (setq prompt-count (1+ prompt-count))
                     'push-all)))
          (dolist (id '("1" "2"))
            (with-temp-org-buffer
             (format "#+LAST_SYNCED: [2026-01-01 Thu 10:00]\n* Item\n:PROPERTIES:\n:CANVAS_ID: %s\n:END:\n" id)
             (re-search-forward "^\\* ")
             (org-back-to-heading)
             (org-canvas--push-to-api
              (list :title "Item" :canvas-id id :pom (point-marker))
              '((title . "Item")) :endpoint "items"
              :ctx (org-canvas--sync-make-ctx))))
          (expect prompt-count :to-equal 2))))))

(describe "org-canvas--resolve-conflict unexpected choice"
  (it "returns skip for unexpected choice symbol"
    (spy-on 'org-canvas--log-warning)
    (let ((ctx (org-canvas--sync-make-ctx)))
      (cl-letf (((symbol-function 'org-canvas--conflict-format-diff)
                 (lambda (_data _remote &optional _has-pull) (get-buffer-create "*test-diff*")))
                ((symbol-function 'org-canvas--conflict-prompt)
                 (lambda (_has-pull) 'unexpected-value)))
        (let ((result (org-canvas--resolve-conflict '(:title "Test") '((title . "Test")) ctx)))
          (expect result :to-equal 'skip)
          (expect 'org-canvas--log-warning :to-have-been-called))))))

(describe "org-canvas--conflict-format-diff baseline line (issue #86)"
  (it "shows the CANVAS_UPDATED_AT the check compared"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "#+LAST_SYNCED: [2026-01-15 Thu 14:30]
* My Item
:PROPERTIES:
:CANVAS_ID: 123
:CANVAS_UPDATED_AT: 2026-01-20T10:00:00Z
:END:
"
       (re-search-forward "^\\* ")
       (org-back-to-heading)
       (let* ((data (list :title "My Item" :description "x" :pom (point-marker)))
              (remote '((title . "My Item") (updated_at . "2026-02-01T10:00:00Z")
                        (body . "y")))
              (buf (org-canvas--conflict-format-diff data remote)))
         (unwind-protect
             (with-current-buffer buf
               (expect (buffer-string)
                       :to-match "Local baseline: +CANVAS_UPDATED_AT 2026-01-20T10:00:00Z")
               (expect (buffer-string) :not :to-match "Local LAST_SYNCED"))
           (when (buffer-live-p buf) (kill-buffer buf)))))))

  (it "says there is no baseline when the entry has none"
    (with-org-canvas-test-config
      (with-temp-org-buffer
       "* My Item
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
       (org-back-to-heading)
       (let* ((data (list :title "My Item" :description "x" :pom (point-marker)))
              (remote '((title . "My Item") (updated_at . "2026-02-01T10:00:00Z")))
              (buf (org-canvas--conflict-format-diff data remote)))
         (unwind-protect
             (with-current-buffer buf
               (expect (buffer-string) :to-match "Local baseline: +none (first sync)"))
           (when (buffer-live-p buf) (kill-buffer buf)))))))

  (it "survives a pom that is not in an Org buffer"
    (with-org-canvas-test-config
      (with-temp-buffer
        (insert "plain text")
        (let* ((data (list :title "X" :description "x" :pom (point)))
               (remote '((title . "X") (updated_at . "2026-02-01T10:00:00Z")))
               (buf (org-canvas--conflict-format-diff data remote)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-string) :to-match "Local baseline: +none"))
            (when (buffer-live-p buf) (kill-buffer buf))))))))

(describe "org-canvas--duplicate-prompt (issue #85)"
  (it "skips without asking in batch"
    (let ((noninteractive t))
      (expect (org-canvas--duplicate-prompt "R11" '("1")) :to-equal 'skip)))

  (it "offers adopt only for a single holder"
    (let ((noninteractive nil) (seen-keys nil) (seen-prompt nil))
      (cl-letf (((symbol-function 'read-char-choice)
                 (lambda (prompt keys)
                   (setq seen-prompt prompt seen-keys keys)
                   ?a)))
        (expect (org-canvas--duplicate-prompt "R11" '("1")) :to-equal 'adopt)
        (expect seen-keys :to-equal '(?a ?A ?s ?S ?c ?C))
        (expect seen-prompt :to-match "'R11' already exists on Canvas (id 1)\\. \\[a\\]dopt "))
      (cl-letf (((symbol-function 'read-char-choice)
                 (lambda (prompt keys)
                   (setq seen-prompt prompt seen-keys keys)
                   ?s)))
        (expect (org-canvas--duplicate-prompt "R11" '("1" "2")) :to-equal 'skip)
        (expect seen-keys :to-equal '(?s ?S ?c ?C))
        (expect seen-prompt :to-match "(id 1, 2)\\. \\[s\\]kip")
        (expect seen-prompt :not :to-match "adopt"))))

  (it "maps every key"
    (let ((noninteractive nil))
      (dolist (pair '((?a . adopt) (?A . adopt-all) (?s . skip)
                      (?S . skip-all) (?c . create) (?C . create-all)))
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) (car pair))))
          (expect (org-canvas--duplicate-prompt "R11" '("1")) :to-equal (cdr pair)))))))

(describe "org-canvas--duplicate-unattended-action (issue #85)"
  (it "follows the strategy and says so"
    (let ((org-canvas-duplicate-title-strategy 'adopt) (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
        (expect (org-canvas--duplicate-unattended-action "R11") :to-equal 'adopt))
      (expect (car warnings)
              :to-equal "[Duplicate] 'R11' resolved as adopt without prompting (org-canvas-duplicate-title-strategy)")))

  (it "skips in batch and says how to choose"
    (let ((org-canvas-duplicate-title-strategy nil) (noninteractive t) (warnings nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (_l fmt &rest args) (push (apply #'format fmt args) warnings))))
        (expect (org-canvas--duplicate-unattended-action "R11") :to-equal 'skip))
      (expect (car warnings) :to-match "batch mode; set org-canvas-duplicate-title-strategy")))

  (it "is nil when someone can be asked"
    (let ((org-canvas-duplicate-title-strategy nil) (noninteractive nil))
      (cl-letf (((symbol-function 'org-canvas--log-warning)
                 (lambda (&rest _) (error "Nothing to say"))))
        (expect (org-canvas--duplicate-unattended-action "R11") :to-be nil)))))

(describe "org-canvas--resolve-duplicate (issue #85)"
  (it "honours the run's standing apply-all answer first"
    (let ((ctx (org-canvas--sync-make-ctx :duplicate-apply-all 'create))
          (org-canvas-duplicate-title-strategy 'skip))
      (expect (org-canvas--resolve-duplicate "R11" '("1") ctx) :to-equal 'create)))

  (it "takes the unattended answer before prompting"
    (let ((org-canvas-duplicate-title-strategy 'adopt))
      (cl-letf (((symbol-function 'read-char-choice)
                 (lambda (&rest _) (error "Must not prompt")))
                ((symbol-function 'org-canvas--log-warning) #'ignore))
        (expect (org-canvas--resolve-duplicate "R11" '("1") (org-canvas--sync-make-ctx))
                :to-equal 'adopt)
        (expect (org-canvas--resolve-duplicate "R11" '("1")) :to-equal 'adopt))))

  (it "remembers a capital answer in the run context for the rest of the run"
    (let ((org-canvas-duplicate-title-strategy nil)
          (noninteractive nil))
      (dolist (pair '((?S . skip) (?A . adopt) (?C . create)))
        (let ((ctx (org-canvas--sync-make-ctx)))
          (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) (car pair))))
            (expect (org-canvas--resolve-duplicate "R11" '("1") ctx) :to-equal (cdr pair))
            (expect (plist-get ctx :duplicate-apply-all) :to-equal (cdr pair)))))))

  (it "passes a lowercase answer through without remembering it"
    (let ((ctx (org-canvas--sync-make-ctx))
          (org-canvas-duplicate-title-strategy nil)
          (noninteractive nil))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?c)))
        (expect (org-canvas--resolve-duplicate "R11" '("1") ctx) :to-equal 'create)
        (expect (plist-get ctx :duplicate-apply-all) :to-be nil)))))

(provide 'org-canvas-core-conflict-test)
;;; org-canvas-core-conflict-test.el ends here
